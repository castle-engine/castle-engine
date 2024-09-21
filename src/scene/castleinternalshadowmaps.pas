{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow maps internal utilities. }
unit CastleInternalShadowMaps;

{$I castleconf.inc}

interface

uses X3DNodes, CastleShapes;

{ Automatically handle X3D "receiveShadows" field
  by inserting appropriate lower-level nodes.

  If Enable is @true, the appropriate lower-level nodes are added,
  or replaced (if they already existed, because you call
  ProcessShadowMapsReceivers again).
  If Enable is @false, the appropriate nodes (added by previous calls to
  ProcessShadowMapsReceivers) will be removed instead.

  This adds/removes by changing the TShape.InternalShadowMaps,
  nothing more (it does not modify textures in Appearance.textures,
  or any texture coordinates). }
procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);

implementation

uses SysUtils, Generics.Collections,
  CastleUtils, CastleStringUtils, CastleImages,
  CastleBoxes, CastleLog, CastleVectors, CastleRectangles;

type
  { Information about light source relevant for shadow maps. }
  TLight = record
    Light: TAbstractPunctualLightNode;
    ShadowMap: TGeneratedShadowMapNode;
    ShadowReceiversBox: TBox3D;
  end;
  PLight = ^TLight;

  TLightList = class({$ifdef FPC}specialize{$endif} TStructList<TLight>)
  public
    DefaultShadowMapSize: Cardinal;
    ShadowCastersBox: TBox3D;
    LightsCastingOnEverything: TX3DNodeList;

    { Find existing or add new TLight record for this light node.
      This also creates shadow map for this light. }
    function FindLight(Light: TAbstractPunctualLightNode): PLight;

    procedure ShapeRemove(const Shape: TShape);
    procedure ShapeAdd(const Shape: TShape);

    { Finish calculating light's projectionXxx parameters,
      and assign them to the light node. }
    procedure HandleLightAutomaticProjection(const Light: TLight);

    { Add light node to LightsCastingOnEverything, if shadows=TRUE. }
    procedure HandleLightCastingOnEverything(Node: TX3DNode);
  end;

function TLightList.FindLight(Light: TAbstractPunctualLightNode): PLight;
var
  I: Integer;
  LightUniqueName: string;
begin
  for I := 0 to Count - 1 do
    if L[I].Light = Light then
      Exit(PLight(Ptr(I)));

  { add a new TLight record }
  Result := PLight(Add);
  Result^.Light := Light;
  Result^.ShadowReceiversBox := TBox3D.Empty;

  { Assign unique nodenames to the created ShadowMap and TexGen nodes,
    this way when saving they will be shared by DEF/USE.
    Based on LightUniqueName. }
  LightUniqueName := Light.X3DName;
  if LightUniqueName = '' then
    LightUniqueName := 'Light' + IntToStr(Random(1000000));

  { create new (or use existing) GeneratedShadowMap node }

  if (Light.FdDefaultShadowMap.Value <> nil) and
     (Light.FdDefaultShadowMap.Value is TGeneratedShadowMapNode) then
  begin
    Result^.ShadowMap := TGeneratedShadowMapNode(Light.FdDefaultShadowMap.Value);
  end else
  begin
    Result^.ShadowMap := TGeneratedShadowMapNode.Create;
    { Allows castle-model-viewer lights editor to easily configure this node. }
    Light.FdDefaultShadowMap.Value := Result^.ShadowMap;
    Result^.ShadowMap.Update := upAlways;
    Result^.ShadowMap.Size := DefaultShadowMapSize;
  end;

  { Regardless if this is taken from defaultShadowMap or created,
    always set "light" to our light. This way user doesn't have to
    specify defaultShadowMap.light is the same light. }
  Result^.ShadowMap.FdLight.Value := Light;
end;

{ If this shape was processed by some ShapeAdd previously,
  make Shape.InternalShadowMaps empty. }
procedure TLightList.ShapeRemove(const Shape: TShape);
begin
  FreeAndNil(Shape.InternalShadowMaps);
end;

procedure TLightList.ShapeAdd(const Shape: TShape);

  { Add given light to Shape. }
  procedure HandleLight(LightNode: TAbstractPunctualLightNode);
  var
    Light: PLight;
  begin
    // create Shape.InternalShadowMaps if necessary
    if Shape.InternalShadowMaps = nil then
      Shape.InternalShadowMaps := TX3DNodeList.Create(false);

    // add Light^.ShadowMap to Shape.InternalShadowMaps
    Light := FindLight(LightNode);
    if Shape.InternalShadowMaps.IndexOf(Light^.ShadowMap) = -1 then
      Shape.InternalShadowMaps.Add(Light^.ShadowMap);

    Light^.ShadowReceiversBox.Include(Shape.BoundingBox);
  end;

  { Account for the fact Shape casts shadows. }
  procedure HandleShadowCaster;
  begin
    ShadowCastersBox.Include(Shape.BoundingBox);
  end;

var
  I: Integer;
  App: TAppearanceNode;
begin
  { Handle VRML 1.0 nodes, without TShapeNode, early.
    They cannot be shadow maps receivers, but they can be shadow casters. }
  if Shape.Node = nil then
  begin
    HandleShadowCaster;
    Exit; { VRML <= 1.0 shapes  }
  end;

  App := Shape.Node.Appearance;
  if (App = nil) or App.ShadowCaster then
    HandleShadowCaster;

  // handle Appearance.receiveShadows
  if App <> nil then
  begin
    for I := 0 to App.FdReceiveShadows.Count - 1 do
      if App.FdReceiveShadows[I] is TAbstractPunctualLightNode then
        HandleLight(TAbstractPunctualLightNode(App.FdReceiveShadows[I]));
  end;

  // add receiving shadows implied by shadows=TRUE on light sources
  for I := 0 to LightsCastingOnEverything.Count - 1 do
    HandleLight(TAbstractPunctualLightNode(LightsCastingOnEverything[I]));
end;

procedure TLightList.HandleLightAutomaticProjection(const Light: TLight);

  { Auto-calculate projection near and far, knowing that light's
    projection location and direction are set now. }
  procedure AutoCalculateProjectionNearFar;
  var
    ProjectionNear, ProjectionFar: Single;
    MinReceiverDistance, MaxReceiverDistance: Single;
  begin
    if ShadowCastersBox.IsEmpty then
    begin
      { No shadow casters? So any sensible values are fine. }
      ProjectionNear := 0.1;
      ProjectionFar := 1;
    end else
    begin
      { Projection near/far must include all shadow casters between
        light source and the shadow receivers. }
      Light.Light.Box3DDistances(ShadowCastersBox, ProjectionNear, ProjectionFar);
      Light.Light.Box3DDistances(Light.ShadowReceiversBox, MinReceiverDistance, MaxReceiverDistance);
      // MinReceiverDistance is ignored

      MaxVar(ProjectionNear, 0);
      MinVar(ProjectionFar, MaxReceiverDistance);

      if ProjectionNear > ProjectionFar then
      begin
        { No shadow casters that can cast shadows on our shadow receivers?
          So any sensible values are fine. }
        ProjectionNear := 0.1;
        ProjectionFar := 1;
      end else
      begin
        { So we know now that ProjectionNear >= 0 and
          ProjectionFar >= ProjectionNear. }

        { final correction of auto-calculated projectionFar: must be > 0 }
        if ProjectionFar <= 0 then
          ProjectionFar := 1;

        { Make ProjectionFar larger and ProjectionNear smaller, since
          1. At the beginning of the projection range
             the depth texture has the best precision.
          2. The range should be slightly larger than ShadowCastersBox anyway,
             to be sure to capture shadow casters exactly at the begin/end
             of projection range (like a box side exactly at the beginning
             of ShadowCastersBox range in demo_models/shadow_spot_simple.wrl). }
        ProjectionFar := ProjectionFar * 2.0;
        ProjectionNear := ProjectionNear / 2.0;

        { final correction of auto-calculated projectionNear: must be > 0,
          and preferably > some epsilon of projectionFar (to avoid depth
          precision problems). }
        MaxVar(ProjectionNear, ProjectionFar * 0.001);
      end;
    end;

    WritelnLog('Shadow Maps', Format('Auto-calculated light source "%s" projectionNear is %f, projectionFar is %f',
      [Light.Light.NiceName, ProjectionNear, ProjectionFar]));

    { Set light node's projectionXxx values, if they are needed. }
    if Light.Light.FdProjectionNear.Value = 0 then
      Light.Light.FdProjectionNear.Value := ProjectionNear;
    if Light.Light.FdProjectionFar.Value = 0 then
      Light.Light.FdProjectionFar.Value := ProjectionFar;
  end;

  procedure AutoCalculateProjectionForDirectionalLight(
    const LightNode: TDirectionalLightNode);
  var
    Pos, Dir, Side, Up, MinCorner, MaxCorner: TVector3;
    ProjectionLocation: TVector3;
    ProjectionRectangle: TFloatRectangle;
  begin
    if LightNode.FdProjectionRectangle.Value.IsPerfectlyZero and
      (not ShadowCastersBox.IsEmpty) then
    begin
      LightNode.GetView(Pos, Dir, Side, Up);
      MinCorner := ShadowCastersBox.MinimumCorner(LightNode.ProjectionWorldDirection);
      MaxCorner := ShadowCastersBox.MaximumCorner(LightNode.ProjectionWorldDirection);
      { do not place ProjectionLocation exactly at MinCorner, it would be too close
        to ShadowCastersBox, forcing projectionNear always almost zero. }
      ProjectionLocation :=
        //MinCorner - (MaxCorner - MinCorner)
        MinCorner * 2 - MaxCorner; // same thing
      ProjectionRectangle := ShadowCastersBox.OrthoProject(
        ProjectionLocation, Dir, Side, Up);
      LightNode.FdProjectionRectangle.Value := ProjectionRectangle.ToX3DVector;
      LightNode.FdProjectionLocation.Value :=
        LightNode.InverseTransform.MultPoint(ProjectionLocation);

      WritelnLog('Shadow Maps', Format('Auto-calculated directional light source "%s" projectionLocation as %s, projectionRectangle as %s',
        [Light.Light.NiceName,
         ProjectionLocation.ToString,
         ProjectionRectangle.ToString]));
    end;
  end;

begin
  { calculate projectionLocation/Rectangle first,
    since projectionLocation determines the right range for projectionNear/Far }
  if Light.Light is TDirectionalLightNode then
    AutoCalculateProjectionForDirectionalLight(
      TDirectionalLightNode(Light.Light));
  AutoCalculateProjectionNearFar;
end;

procedure TLightList.HandleLightCastingOnEverything(Node: TX3DNode);
begin
  if TAbstractPunctualLightNode(Node).Shadows then
    LightsCastingOnEverything.Add(Node);
end;

procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);
var
  Lights: TLightList;
  L: PLight;
  I: Integer;
begin
  { This is valid situation (TCastleSceneCore.RootNode may be nil).
    Nothing to do then. }
  if Model = nil then Exit;

  Lights := TLightList.Create;
  try
    { Shapes.Traverse here enumerate all (active and not) shapes.
      In case a shape is not active, it may become active later
      (e.g. by Switch.whichChoice change), and ProcessShadowMapsReceivers
      will not necessarily be run again. So we better account for this
      shape already. }

    { We first remove all old GeneratedShadowMap
      nodes, in one Shapes.Traverse run. Then, if Enable,
      we make another Shapes.Traverse run and only add necessary nodes.

      Previously I tried to do both (removal and addition) at the same time,
      but this was just too error-prone. Notice that multiple shapes may refer
      to the same light node. And shapes may have multiple GeneratedShadowMap,
      if they receive shadow from more then one light. So it was too easy
      to remove a shadow map (or projector) that we have just added... }
    Shapes.Traverse({$ifdef FPC}@{$endif} Lights.ShapeRemove, false);

    if Enable then
    begin
      Lights.DefaultShadowMapSize := DefaultShadowMapSize;
      Lights.ShadowCastersBox := TBox3D.Empty;

      { calculate Lights.LightsCastingOnEverything first }
      Lights.LightsCastingOnEverything := TX3DNodeList.Create(false);
      Model.EnumerateNodes(TAbstractPunctualLightNode,
        {$ifdef FPC}@{$endif}Lights.HandleLightCastingOnEverything,
        false);

      Shapes.Traverse({$ifdef FPC}@{$endif} Lights.ShapeAdd, false);

      for I := 0 to Lights.Count - 1 do
      begin
        L := PLight(Lights.Ptr(I));

        Lights.HandleLightAutomaticProjection(L^);

        { Although we try to construct things only when they will be actually
          used (so no unused nodes should remain now for free), actually
          there is a chance something remained unused if HandleLight failed
          with WritelnWarning after FindLight. }
        L^.ShadowMap.FreeIfUnused;
        L^.ShadowMap := nil;
      end;

      FreeAndNil(Lights.LightsCastingOnEverything);
    end;
  finally FreeAndNil(Lights) end;
end;

end.
