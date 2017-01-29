{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow maps internal utilities. }
unit X3DShadowMaps;

{$I castleconf.inc}
{$modeswitch nestedprocvars}{$H+}

interface

uses X3DNodes, CastleShapes;

type
  TShadowSampling = (ssSimple,

    { Percentage Closer Filtering improve shadow maps look, by sampling
      the depth map a couple of times. They also make shadow more blurry
      (increase shadow map size to counteract this), and a little slower.
      They may also introduce new artifacts (due to bad interaction
      with the "polygon offset" of shadow map). }
    ssPCF4, ssPCF4Bilinear, ssPCF16,

    { Variance Shadow Maps, see http://www.punkuser.net/vsm/ .
      This may generally produce superior
      results, as shadow maps can be then filtered like normal textures
      (bilinear, mipmaps, anisotropic filtering). So shadows look much nicer
      from very close and very far distances.
      However, this requires new GPU, and may cause artifacts on some scenes. }
    ssVarianceShadowMaps);

const
  ShadowSamplingNames: array [TShadowSampling] of string =
  ( 'Simple', 'PCF 4', 'PCF 4 Bilinear', 'PCF 16', 'Variance Shadow Maps (Experimental)' );

  DefaultShadowSampling = ssPCF16;

{ Automatically handle VRML/X3D "receiveShadows" field
  by inserting appropriate lower-level nodes.

  If Enable is @true, the appropriate lower-level nodes are added,
  or replaced (if they already existed, because you call
  ProcessShadowMapsReceivers again).
  If Enable is @false, the appropriate nodes (added by previous calls to
  ProcessShadowMapsReceivers) will be removed instead.

  For each shape with "receiveShadows", we:
  @orderedList(
    @item(extend it's "texture" field with appropriate GeneratedShadowMap,)
    @item(extend it's "texCoord" field with appropriate
      ProjectedTextureCoordinate,)
  ) }
procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);

implementation

uses SysUtils, CastleUtils, CastleStringUtils,
  CastleBoxes, CastleLog, CastleVectors, CastleGenericLists,
  CastleRectangles;

const
  { Suffix of VRML node names created by ProcessShadowMapsReceivers
    transformation. }
  X3DNameSuffix = '_generated_by_ProcessShadowMapsReceivers';

type
  { Information about light source relevant for shadow maps. }
  TLight = record
    Light: TAbstractLightNode;
    ShadowMap: TGeneratedShadowMapNode;
    TexGen: TProjectedTextureCoordinateNode;
    ShadowReceiversBox: TBox3D;
  end;
  PLight = ^TLight;

  TLightList = class(specialize TGenericStructList<TLight>)
  public
    DefaultShadowMapSize: Cardinal;
    ShadowMapShaders: array [boolean, 0..1] of TComposedShaderNode;
    ShadowCastersBox: TBox3D;
    LightsCastingOnEverything: TX3DNodeList;

    { Find existing or add new TLight record for this light node.
      This also creates shadow map and texture generator nodes for this light. }
    function FindLight(Light: TAbstractLightNode): PLight;

    procedure ShapeRemove(Shape: TShape);
    procedure ShapeAdd(Shape: TShape);

    { Finish calculating light's projectionXxx parameters,
      and assign them to the light node. }
    procedure HandleLightAutomaticProjection(const Light: TLight);

    { Add light node to LightsCastingOnEverything, if shadows=TRUE. }
    procedure HandleLightCastingOnEverything(Node: TX3DNode);
  end;

function TLightList.FindLight(Light: TAbstractLightNode): PLight;
var
  I: Integer;
  LightUniqueName: string;
begin
  for I := 0 to Count - 1 do
    if L[I].Light = Light then Exit(Ptr(I));

  { add a new TLight record }
  Result := Add;
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
    { Allows view3dscene lights editor to easily configure this node. }
    Light.FdDefaultShadowMap.Value := Result^.ShadowMap;
    Result^.ShadowMap.FdUpdate.Value := upAlways;
    Result^.ShadowMap.FdSize.Value := DefaultShadowMapSize;
  end;

  { Regardless if this is taken from defaultShadowMap or created,
    always set "light" to our light. This way user doesn't have to
    specify defaultShadowMap.light is the same light. }
  Result^.ShadowMap.FdLight.Value := Light;

  { Regardless if this is taken from defaultShadowMap or created,
    set X3DName, such that it has X3DNameSuffix. This is needed for
    HandleShadowMap, so that it can be removed later. }
  Result^.ShadowMap.X3DName := LightUniqueName + '_ShadowMap' + X3DNameSuffix;

  { create new ProjectedTextureCoordinate node }

  Result^.TexGen := TProjectedTextureCoordinateNode.Create;
  Result^.TexGen.X3DName := LightUniqueName + '_TexGen' + X3DNameSuffix;
  Result^.TexGen.FdProjector.Value := Light;
end;

{ If this shape was processed by some ShapeAdd previously,
  removed the ProjectedTextureCoordinate and GeneratedShadowMap nodes we added. }
procedure TLightList.ShapeRemove(Shape: TShape);

  { Remove old GeneratedShadowMap nodes that we added. }
  procedure RemoveOldShadowMap(Texture: TMFNode);
  var
    I: Integer;
  begin
    I := 0;
    while I < Texture.Count do
      if IsSuffix(X3DNameSuffix, Texture[I].X3DName) and
         (Texture[I] is TGeneratedShadowMapNode) then
        Texture.Delete(I) else
        Inc(I);
  end;

  { Remove old ProjectedTextureCoordinate nodes that we added. }
  procedure RemoveOldTexGen(TexCoord: TMFNode);
  var
    I: Integer;
  begin
    I := 0;
    while I < TexCoord.Count do
      if IsSuffix(X3DNameSuffix, TexCoord[I].X3DName) and
         (TexCoord[I] is TProjectedTextureCoordinateNode) then
        TexCoord.Delete(I) else
        Inc(I);
  end;

begin
  if Shape.Node <> nil then
  begin
    if Shape.Node.Texture is TMultiTextureNode then
      RemoveOldShadowMap(TMultiTextureNode(Shape.Node.Texture).FdTexture);
    if (Shape.Geometry.TexCoordField <> nil) and
       (Shape.Geometry.TexCoordField.Value <> nil) and
       (Shape.Geometry.TexCoordField.Value is TMultiTextureCoordinateNode) then
      RemoveOldTexGen(TMultiTextureCoordinateNode(Shape.Geometry.TexCoordField.Value).FdTexCoord);
  end;
end;

procedure TLightList.ShapeAdd(Shape: TShape);

  { Add ShadowMap to the textures used by the shape.
    Always converts Texture to TMultiTextureNode, to add the shadow map
    preserving old texture.

    Returns the count of textures in TexturesCount, not counting the last
    ShadowMap texture. But it *does* count the texture in
    OriginalGeometry.FontTextureNode. IOW, TexturesCount is
    "how many texCoords are actually used, not counting new stuff for shadow maps". }
  procedure HandleShadowMap(var Texture: TAbstractTextureNode;
    const ShadowMap: TGeneratedShadowMapNode; out TexturesCount: Cardinal);
  var
    MTexture: TMultiTextureNode;
  begin
    { calculate MTexture }
    if (Texture <> nil) and
       (Texture is TMultiTextureNode) then
    begin
      { if Texture already is MultiTexture, then we're already Ok }
      MTexture := TMultiTextureNode(Texture);
    end else
    begin
      MTexture := TMultiTextureNode.Create;
      if Texture <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texture" field on the same position) }
        MTexture.PositionInParent := Texture.PositionInParent;
        MTexture.FdTexture.Add(Texture);
      end;
      Texture := MTexture;
    end;
    Assert(Texture = MTexture);

    TexturesCount := MTexture.FdTexture.Count;
    if Shape.OriginalGeometry.FontTextureNode <> nil then
      Inc(TexturesCount);

    { If the texture that we want to add is already present, abort.
      This may happen, as HandleLight may iterate many times over
      the same light. }
    if MTexture.FdTexture.Items.IndexOf(ShadowMap) <> -1 then
    begin
      Dec(TexturesCount);
      Exit;
    end;

    MTexture.FdTexture.Add(ShadowMap);
  end;

  { Add to the texCoord field.
    Converts texCoord to TMultiTextureCoordinateNode,
    to preserve previous tex coord.

    May remove some texCoord nodes, knowing that only the 1st
    RelevantTexCoordsCount nodes are used.
    May add some texCoord nodes (with TextureCoordinateGenerator = BOUNDS),
    to make sure that we have at least RelevantTexCoordsCount nodes.

    Makes sure that the count of texCoords is exactly RelevantTexCoordsCount,
    not counting the last (newly added) TexGen node. }
  procedure HandleTexGen(var TexCoord: TX3DNode;
    const TexGen: TProjectedTextureCoordinateNode;
    const RelevantTexCoordsCount: Cardinal);

    { Resize Coords. If you increase Coords, then new ones
      are TextureCoordinateGenerator nodes (with mode=BOUNDS). }
    procedure ResizeTexCoord(const Coords: TMFNode; const NewCount: Cardinal);
    var
      OldCount: Cardinal;
      NewTexCoordGen: TTextureCoordinateGeneratorNode;
      I: Integer;
    begin
      OldCount := Coords.Count;
      Coords.Count := NewCount;

      for I := OldCount to NewCount - 1 do
      begin
        NewTexCoordGen := TTextureCoordinateGeneratorNode.Create;
        NewTexCoordGen.FdMode.Value := 'BOUNDS';
        Coords.Replace(I, NewTexCoordGen);
      end;
    end;

  var
    MTexCoord: TMultiTextureCoordinateNode;
  begin
    { calculate MTexCoord }
    if (TexCoord <> nil) and
       (TexCoord is TMultiTextureCoordinateNode) then
    begin
      { if TexCoord already is MultiTextureCoordinate, then we're already Ok }
      MTexCoord := TMultiTextureCoordinateNode(TexCoord);
    end else
    begin
      MTexCoord := TMultiTextureCoordinateNode.Create;
      if TexCoord <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texCoord" field on the same position) }
        MTexCoord.PositionInParent := TexCoord.PositionInParent;
        MTexCoord.FdTexCoord.Add(TexCoord);
      end;
      TexCoord := MTexCoord;
    end;
    Assert(TexCoord = MTexCoord);

    { If the texcoord that we want to add is already present, abort.
      This may happen, as HandleLight may iterate many times over
      the same light. }
    if MTexCoord.FdTexCoord.Items.IndexOf(TexGen) = -1 then
    begin
      { Add new necessary TextureCoordinateGenerator nodes,
        or remove unused nodes, to make texCoord size right }
      ResizeTexCoord(MTexCoord.FdTexCoord, RelevantTexCoordsCount);

      MTexCoord.FdTexCoord.Add(TexGen);
    end;
  end;

  { Change textureTransform into MultiTextureTransform if necessary.
    Otherwise, user's TextureTransform could get ignored, because X3D spec
    says that:

      "If using a MultiTexture node with a geometry node without
      a MultiTextureTransform node, identity matrices are assumed
      for all channels."

    IOW, direct TextureTransform is ignored when using MultiTexture.
    Only MultiTextureTransform is taken into account.
    And our HandleShadowMap just changed your texture into MultiTexture.

    We do not add/remove from there anything (we do not need any
    texture transforms there, X3D will assume identity for texture units
    without corresponding TextureTransform node, this is Ok). }
  procedure HandleTextureTransform(var TextureTransform: TAbstractTextureTransformNode);
  var
    MultiTT: TMultiTextureTransformNode;
  begin
    if (TextureTransform <> nil) and
       (TextureTransform is TTextureTransformNode) then
    begin
      MultiTT := TMultiTextureTransformNode.Create;
      { set position in parent only for more deterministic output
	(new "texture" field on the same position) }
      MultiTT.PositionInParent := TextureTransform.PositionInParent;
      MultiTT.FdTextureTransform.Add(TextureTransform);
      TextureTransform := MultiTT;
    end;
  end;

  { 1. Add necessary ShadowMap
    2. Add necessary TexGen
    3. Convert texture, texCoord, textureTransform to multi-texture if needed }
  procedure HandleLight(LightNode: TAbstractLightNode);
  var
    Light: PLight;
    Texture: TAbstractTextureNode;
    TextureTransform: TAbstractTextureTransformNode;
    TexCoord: TX3DNode;
    TexturesCount: Cardinal;
  begin
    Light := FindLight(LightNode);

    Texture := Shape.Node.Texture;
    HandleShadowMap(Texture, Light^.ShadowMap, TexturesCount);
    { set Texture.
      Note: don't use "Shape.Node.Texture := ", we have to avoid calling
      "Send(xxx)" underneath here, as it would cause CastleSceneCore processing
      that could recursively call ProcessShadowMapsReceivers, creating a loop. }
    if Shape.Node.Appearance = nil then
      Shape.Node.FdAppearance.Value := TAppearanceNode.Create;
    Shape.Node.Appearance.FdTexture.Value := Texture;

    TexCoord := Shape.Geometry.TexCoordField.Value;
    HandleTexGen(TexCoord, Light^.TexGen, TexturesCount);
    Shape.Geometry.TexCoordField.Value := TexCoord;

    if (Shape.Geometry <> Shape.OriginalGeometry) and
       (Shape.OriginalGeometry.TexCoordField <> nil) and
       (not (Shape.OriginalGeometry is TTextNode)) and
       (not (Shape.OriginalGeometry is TAsciiTextNode_1)) then
    begin
      { If this shape uses proxy, the proxy may be freed and regenerated
        on some VRML/X3D graph changes. We want this regeneration to
        preserve our modifications to the TexCoord, so that shadow maps
        still work. So set here original geometry texCoord too,
        if possible.

        This is dirty for a couple of reasons:

        - We use here the internal knowledge that within nodes like Sphere,
          we allow MultiTexture node with explicit TextureCoordinate
          children inside (even though our specification says that we only allow
          generated texture coordinate nodes (like TextureCoordinateGenerator)
          on Sphere.texCoord).

        - We use here the internal knowledge that Sphere.Proxy method,
          when OriginalGeometry.FdTexCoord.Value <> nil,
          just uses OriginalGeometry.FdTexCoord.Value for returned
          Proxy geometry FdTexCoord, without any processing.

          TODO: This idea fails e.g. for Text node, where TextProxy does something
          more involved with FdTexCoord field, and this scheme would just fail.

        - Since we add explicit texture coords for Sphere and such
          nodes here, we assume that Sphere.Proxy will always generate
          the same coordinates for this node. This is true now,
          but it will stop being true if triangulation detail will be dynamic
          (e.g. based on distance to camera). }
      Shape.OriginalGeometry.TexCoordField.Value := TexCoord;
    end;

    TextureTransform := Shape.Node.TextureTransform;
    HandleTextureTransform(TextureTransform);
    { set TextureTransform.
      Note: don't use "Shape.Node.TextureTransform := ", we have to avoid calling
      "Send(xxx)" underneath here, as it would cause CastleSceneCore processing
      that could recursively call ProcessShadowMapsReceivers, creating a loop. }
    if Shape.Node.Appearance = nil then
      Shape.Node.FdAppearance.Value := TAppearanceNode.Create;
    Shape.Node.Appearance.FdTextureTransform.Value := TextureTransform;

    Light^.ShadowReceiversBox.Add(Shape.BoundingBox);
  end;

  procedure HandleShadowCaster;
  begin
    ShadowCastersBox.Add(Shape.BoundingBox);
  end;

var
  I: Integer;
  App: TAppearanceNode;
begin
  if Shape.Node = nil then
  begin
    HandleShadowCaster;
    Exit; { VRML <= 1.0 shapes cannot be shadow maps receivers,
      but they can be shadow casters }
  end;

  App := Shape.Node.Appearance;

  { If Appearance is NULL, but we should create it --- do it.
    Testcase: shadow_maps/primitives.x3dv with appearance commented out. }
  if (App = nil) and
     (LightsCastingOnEverything.Count <> 0) then
  begin
    App := TAppearanceNode.Create('', Shape.Node.BaseUrl); { recalculate App }
    { assign it using "FdAppearance.Value := ", not "Appearance := ",
      to avoid doing "Send(xxx)" inside that could recursively cause ProcessShadowMapsReceivers }
    Shape.Node.FdAppearance.Value := App;
  end;

  { If the previous check left App = nil, then we know this shape
    doesn't receiveShadows (LightsCastingOnEverything empty,
    and no Appearance -> no receiveShadows field). }
  if App = nil then
  begin
    HandleShadowCaster;
    Exit;
  end;

  if App.FdShadowCaster.Value then
    HandleShadowCaster;

  { Check are receiveShadows empty, so we don't check TexCoord existence
    when there's no need. }
  if (App.FdReceiveShadows.Count = 0) and
     (LightsCastingOnEverything.Count = 0) then Exit;

  { HandleLight needs here a shape with geometry with texCoord.
    Better check it here, before we start changing anything. }
  if Shape.Geometry.TexCoordField = nil then
  begin
    WritelnWarning('VRML/X3D', 'Geometry node ' + Shape.Geometry.X3DType + ' does not have a texCoord, cannot be shadow maps receiver.');
    Exit;
  end;

  { Treat lights on "receiveShadows" field and
    lights on LightsCastingOnEverything list the same:
    call HandleLight on them.

    TODO: secure against light both on LightsCastingOnEverything
    and "receiveShadows". In fact, remove duplicates from the sum
    of both lists. }

  for I := 0 to App.FdReceiveShadows.Count - 1 do
    if App.FdReceiveShadows[I] is TAbstractLightNode then
      HandleLight(TAbstractLightNode(App.FdReceiveShadows[I]));

  for I := 0 to LightsCastingOnEverything.Count - 1 do
    HandleLight(TAbstractLightNode(LightsCastingOnEverything[I]));
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
        ProjectionFar *= 2.0;
        ProjectionNear /= 2.0;

        { final correction of auto-calculated projectionNear: must be > 0,
          and preferably > some epsilon of projectionFar (to avoid depth
          precision problems). }
        MaxVar(ProjectionNear, ProjectionFar * 0.001);
      end;
    end;

    if Log then
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
    Pos, Dir, Side, Up, MinCorner, MaxCorner: TVector3Single;
    ProjectionLocation: TVector3Single;
    ProjectionRectangle: TFloatRectangle;
  begin
    if VectorsPerfectlyEqual(
         LightNode.FdProjectionRectangle.Value, ZeroVector4Single) and
      (not ShadowCastersBox.IsEmpty) then
    begin
      LightNode.GetView(Pos, Dir, Side, Up);
      MinCorner := ShadowCastersBox.MinimumCorner(LightNode.ProjectionSceneDirection);
      MaxCorner := ShadowCastersBox.MaximumCorner(LightNode.ProjectionSceneDirection);
      { do not place ProjectionLocation exactly at MinCorner, it would be too close
        to ShadowCastersBox, forcing projectionNear always almost zero. }
      ProjectionLocation :=
        //MinCorner - (MaxCorner - MinCorner)
        MinCorner * 2 - MaxCorner; // same thing
      ProjectionRectangle := ShadowCastersBox.OrthoProject(
        ProjectionLocation, Dir, Side, Up);
      LightNode.FdProjectionRectangle.Value := ProjectionRectangle.ToX3DVector;
      LightNode.FdProjectionLocation.Value :=
        MatrixMultPoint(LightNode.InvertedTransform, ProjectionLocation);

      if Log then
        WritelnLog('Shadow Maps', Format('Auto-calculated directional light source "%s" projectionLocation as %s, projectionRectangle as %s',
          [Light.Light.NiceName,
           VectorToNiceStr(ProjectionLocation),
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
  if TAbstractLightNode(Node).FdShadows.Value then
    LightsCastingOnEverything.Add(Node);
end;

procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);
var
  Lights: TLightList;

  procedure HereShapeRemove(Shape: TShape);
  begin
    Lights.ShapeRemove(Shape);
  end;

  procedure HereShapeAdd(Shape: TShape);
  begin
    Lights.ShapeAdd(Shape);
  end;

var
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

    { We first remove all old GeneratedShadowMap / ProjectedTextureCoordinate
      nodes, in one Shapes.Traverse run. Then, if Enable,
      we make another Shapes.Traverse run and only add necessary nodes.

      Previously I tried to do both (removal and addition) at the same time,
      but this was just too error-prone. Notice that multiple shapes may refer
      to the same light node. And shapes may have multiple GeneratedShadowMap,
      if they receive shadow from more then one light. So it was too easy
      to remove a shadow map (or projector) that we have just added... }
    Shapes.Traverse(@HereShapeRemove, false);

    if Enable then
    begin
      Lights.DefaultShadowMapSize := DefaultShadowMapSize;
      Lights.ShadowCastersBox := EmptyBox3D;

      { calculate Lights.LightsCastingOnEverything first }
      Lights.LightsCastingOnEverything := TX3DNodeList.Create(false);
      Model.EnumerateNodes(TAbstractLightNode, @Lights.HandleLightCastingOnEverything, false);

      Shapes.Traverse(@HereShapeAdd, false);

      for I := 0 to Lights.Count - 1 do
      begin
        L := Lights.Ptr(I);

        Lights.HandleLightAutomaticProjection(L^);

        { Although we try to construct things only when they will be actually
          used (so no unused nodes should remain now for free), actually
          there is a chance something remained unused if HandleLight failed
          with WritelnWarning after FindLight. }
        L^.ShadowMap.FreeIfUnused;
        L^.ShadowMap := nil;
        L^.TexGen.FreeIfUnused;
        L^.TexGen := nil;
      end;

      FreeAndNil(Lights.LightsCastingOnEverything);
    end;
  finally FreeAndNil(Lights) end;
end;

end.
