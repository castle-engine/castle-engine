{
  Copyright 2010-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow maps internal utilities. }
unit VRMLShadowMaps;

interface

uses VRMLNodes, VRMLShape;

type
  TPercentageCloserFiltering = (pcfNone, pcf4, pcf4Bilinear, pcf16);

const
  PCFNames: array [TPercentageCloserFiltering] of string =
  ( 'Simple', 'PCF 4', 'PCF 4 Bilinear', 'PCF 16' );

  DefaultPercentageCloserFiltering = pcf16;

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
procedure ProcessShadowMapsReceivers(Model: TVRMLNode; Shapes: TVRMLShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);

implementation

uses SysUtils, KambiUtils, VRMLFields, VRMLErrors, KambiStringUtils, Math,
  Boxes3D, KambiLog, VectorMath;

{$define read_interface}
{$define read_implementation}

const
  { Suffix of VRML node names created by ProcessShadowMapsReceivers
    transformation. }
  NodeNameSuffix = '_generated_by_ProcessShadowMapsReceivers';

type
  { Information about light source relevant for shadow maps. }
  TLight = record
    Light: TNodeX3DLightNode;
    ShadowMap: TNodeGeneratedShadowMap;
    TexGen: TNodeProjectedTextureCoordinate;
    MaxShadowReceiverDistance: Single;
  end;
  PLight = ^TLight;

  TDynArrayItem_1 = TLight;
  PDynArrayItem_1 = PLight;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
type
  TDynLightArray = class(TDynArray_1)
  public
    DefaultShadowMapSize: Cardinal;
    ShadowMapShaders: array [boolean, 0..1] of TNodeComposedShader;
    ShadowCastersBox: TBox3D;
    LightsCastingOnEverything: TVRMLNodesList;

    { Find existing or add new TLight record for this light node.
      This also creates shadow map and texture generator nodes for this light. }
    function FindLight(Light: TNodeX3DLightNode): PLight;

    procedure ShapeRemove(Shape: TVRMLShape);
    procedure ShapeAdd(Shape: TVRMLShape);

    { Finish calculating light's projectionXxx parameters,
      and assing them to the light node. }
    procedure HandleLightAutomaticProjection(const L: TLight);

    { Add light node to LightsCastingOnEverything, if shadows=TRUE. }
    procedure HandleLightCastingOnEverything(Node: TVRMLNode);
  end;

function TDynLightArray.FindLight(Light: TNodeX3DLightNode): PLight;
var
  I: Integer;
  LightUniqueName: string;
begin
  for I := 0 to Count - 1 do
    if Items[I].Light = Light then Exit(Pointers[I]);

  { add a new TLight record }
  Result := Add;
  Result^.Light := Light;
  Result^.MaxShadowReceiverDistance := 0;

  { Assign unique nodenames to the created ShadowMap and TexGen nodes,
    this way when saving they will be shared by DEF/USE.
    Based on LightUniqueName. }
  LightUniqueName := Light.NodeName;
  if LightUniqueName = '' then
    LightUniqueName := 'Light' + IntToStr(Random(1000000));

  { create new (or use existing) GeneratedShadowMap node }

  if (Light.FdDefaultShadowMap.Value <> nil) and
     (Light.FdDefaultShadowMap.Value is TNodeGeneratedShadowMap) then
  begin
    Result^.ShadowMap := TNodeGeneratedShadowMap(Light.FdDefaultShadowMap.Value);

    { TODO: for now, we remove the shadow map from defaultShadowMap,
      otherwise we would have a loop in our VRML nodes graph
      (light contains defaultShadowMap that contains GeneratedShadowMap
      with light field pointing again to the parent light).
      And we currently cannot handle nicely such loops (our parser
      never creates them, our enumeration routines assume they don't exist
      etc.) This will be eventually fixed (something like PTraversingInfo
      will be used all around TVRMLNode.DirectEnumerate*, and checked to avoid
      visiting nodes we're already inside), VRML/X3D actually require
      us to handle it in some Script cases anyway. }
    Result^.ShadowMap.KeepExistingBegin;
    Light.FdDefaultShadowMap.Value := nil;
    Result^.ShadowMap.KeepExistingEnd;

    { To avoid losing the information about default shadow map size etc.
      (which may be useful later, if we call ProcessShadowMapsReceivers again on the same model,
      for example if user turns off/on shadow maps), we save important fields
      inside Light properties. }
    Light.DefaultShadowMapSave(Result^.ShadowMap);
  end else
  begin
    Result^.ShadowMap := TNodeGeneratedShadowMap.Create('', '');
    if not Light.DefaultShadowMapLoad(Result^.ShadowMap) then
    begin
      Result^.ShadowMap.FdUpdate.Value := 'ALWAYS';
      Result^.ShadowMap.FdSize.Value := DefaultShadowMapSize;
    end;
  end;

  { Regardless if this is taken from defaultShadowMap or created,
    always set "light" to our light. This way user doesn't have to
    specify defaultShadowMap.light is the same light. }
  Result^.ShadowMap.FdLight.Value := Light;

  { Regardless if this is taken from defaultShadowMap or created,
    set NodeName, such that it has NodeNameSuffix. This is needed for
    HandleShadowMap, so that it can be removed later. }
  Result^.ShadowMap.NodeName := LightUniqueName + '_ShadowMap' + NodeNameSuffix;

  { create new ProjectedTextureCoordinate node }

  Result^.TexGen := TNodeProjectedTextureCoordinate.Create('', '');
  Result^.TexGen.NodeName := LightUniqueName + '_TexGen' + NodeNameSuffix;
  Result^.TexGen.FdProjector.Value := Light;
end;

{ If this shape was processed by some ShapeAdd previously,
  removed the ProjectedTextureCoordinate and GeneratedShadowMap nodes we added. }
procedure TDynLightArray.ShapeRemove(Shape: TVRMLShape);

  { Remove old GeneratedShadowMap nodes that we added. }
  procedure RemoveOldShadowMap(Texture: TMFNode);
  var
    I: Integer;
  begin
    I := 0;
    while I < Texture.Count do
      if IsSuffix(NodeNameSuffix, Texture[I].NodeName) and
         (Texture[I] is TNodeGeneratedShadowMap) then
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
      if IsSuffix(NodeNameSuffix, TexCoord[I].NodeName) and
         (TexCoord[I] is TNodeProjectedTextureCoordinate) then
        TexCoord.Delete(I) else
        Inc(I);
  end;

begin
  if Shape.Node <> nil then
  begin
    if Shape.Node.Texture is TNodeMultiTexture then
      RemoveOldShadowMap(TNodeMultiTexture(Shape.Node.Texture).FdTexture);
    if (Shape.Geometry.TexCoordField <> nil) and
       (Shape.Geometry.TexCoordField.Value <> nil) and
       (Shape.Geometry.TexCoordField.Value is TNodeMultiTextureCoordinate) then
      RemoveOldTexGen(TNodeMultiTextureCoordinate(Shape.Geometry.TexCoordField.Value).FdTexCoord);
  end;
end;

procedure TDynLightArray.ShapeAdd(Shape: TVRMLShape);

  { Add ShadowMap to the textures used by the shape.
    Always converts Texture to TNodeMultiTexture, to add the shadow map
    preserving old texture.

    Returns the count of textures in TexturesCount, not counting the last
    ShadowMap texture. }
  procedure HandleShadowMap(var Texture: TNodeX3DTextureNode;
    const ShadowMap: TNodeGeneratedShadowMap; out TexturesCount: Cardinal);
  var
    MTexture: TNodeMultiTexture;
  begin
    { calculate MTexture }
    if (Texture <> nil) and
       (Texture is TNodeMultiTexture) then
    begin
      { if Texture already is MultiTexture, then we're already Ok }
      MTexture := TNodeMultiTexture(Texture);
    end else
    begin
      MTexture := TNodeMultiTexture.Create('', '');
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
    Converts texCoord to TNodeMultiTextureCoordinate,
    to preserve previous tex coord.

    May remove some texCoord nodes, knowing that only the 1st
    RelevantTexCoordsCount nodes are used.
    May add some texCoord nodes (with TextureCoordinateGenerator = BOUNDS),
    to make sure that we have at least RelevantTexCoordsCount nodes.

    Makes sure that the count of texCoords is exactly RelevantTexCoordsCount,
    not counting the last (newly added) TexGen node. }
  procedure HandleTexGen(var TexCoord: TVRMLNode;
    const TexGen: TNodeProjectedTextureCoordinate;
    const RelevantTexCoordsCount: Cardinal);

    { Resize Coords. If you increase Coords, then new ones
      are TextureCoordinateGenerator nodes (with mode=BOUNDS). }
    procedure ResizeTexCoord(const Coords: TMFNode; const NewCount: Cardinal);
    var
      OldCount: Cardinal;
      NewTexCoordGen: TNodeTextureCoordinateGenerator;
      I: Integer;
    begin
      OldCount := Coords.Count;
      Coords.Count := NewCount;

      for I := OldCount to NewCount - 1 do
      begin
        NewTexCoordGen := TNodeTextureCoordinateGenerator.Create('', '');
        NewTexCoordGen.FdMode.Value := 'BOUNDS';
        Coords.Replace(I, NewTexCoordGen);
      end;
    end;

  var
    MTexCoord: TNodeMultiTextureCoordinate;
  begin
    { calculate MTexCoord }
    if (TexCoord <> nil) and
       (TexCoord is TNodeMultiTextureCoordinate) then
    begin
      { if TexCoord already is MultiTextureCoordinate, then we're already Ok }
      MTexCoord := TNodeMultiTextureCoordinate(TexCoord);
    end else
    begin
      MTexCoord := TNodeMultiTextureCoordinate.Create('', '');
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
  procedure HandleTextureTransform(var TextureTransform: TNodeX3DTextureTransformNode);
  var
    MultiTT: TNodeMultiTextureTransform;
  begin
    if (TextureTransform <> nil) and
       (TextureTransform is TNodeTextureTransform) then
    begin
      MultiTT := TNodeMultiTextureTransform.Create('', '');
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
  procedure HandleLight(LightNode: TNodeX3DLightNode);
  var
    Light: PLight;
    Texture: TNodeX3DTextureNode;
    TextureTransform: TNodeX3DTextureTransformNode;
    TexCoord: TVRMLNode;
    TexturesCount: Cardinal;
    Box: TBox3D;
    MinReceiverDistance, MaxReceiverDistance: Single;
  begin
    Light := FindLight(LightNode);

    Texture := Shape.Node.Texture;
    HandleShadowMap(Texture, Light^.ShadowMap, TexturesCount);
    Shape.Node.Texture := Texture;

    TexCoord := Shape.Geometry.TexCoordField.Value;
    HandleTexGen(TexCoord, Light^.TexGen, TexturesCount);
    Shape.Geometry.TexCoordField.Value := TexCoord;

    if (Shape.Geometry <> Shape.OriginalGeometry) and
       (Shape.OriginalGeometry.TexCoordField <> nil) then
    begin
      { If this shape uses proxy, the proxy may be freed and regenerated
        on some VRML/X3D graph changes. We want this regeneration to
        preserve our modifications to the TexCoord, so that shadow maps
        still work. So set here original geometry texCoord too,
        if possible.

        We actually overuse here the fact that within nodes Sphere, Teapot
        etc. we allow MultiTexture node with explicit TextureCoordinate
        inside.

        TODO: this is very far from perfect, makes some assumptions that
        are not necessarily true. That's because in case of proxy geometry,
        we add back to the original geometry a texCoord designed for proxy.
        So e.g. if the vertexes of original shape changed (although they can't,
        for now), then generating proxy will use the old texCoord. }
      Shape.OriginalGeometry.TexCoordField.Value := TexCoord;
    end;

    TextureTransform := Shape.Node.TextureTransform;
    HandleTextureTransform(TextureTransform);
    Shape.Node.TextureTransform := TextureTransform;

    Box := Shape.BoundingBox;
    if not IsEmptyBox3D(Box) then
    begin
      LightNode.Box3DDistances(Box, MinReceiverDistance, MaxReceiverDistance);
      MaxTo1st(Light^.MaxShadowReceiverDistance, MaxReceiverDistance);
      { We do not use MinReceiverDistance for anything }
    end;
  end;

  procedure HandleShadowCaster;
  begin
    Box3DSumTo1st(ShadowCastersBox, Shape.BoundingBox);
  end;

var
  I: Integer;
  App: TNodeAppearance;
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
    App := TNodeAppearance.Create('', Shape.Node.WWWBasePath); { recalculate App }
    Shape.Node.Appearance := App;
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
    VRMLWarning(vwIgnorable, 'Geometry node "' + Shape.Geometry.NodeTypeName + '" does not have a texCoord, cannot be shadow maps receiver.');
    Exit;
  end;

  { Treat lights on "receiveShadows" field and
    lights on LightsCastingOnEverything list the same:
    call HandleLight on them.

    TODO: secure against light both on LightsCastingOnEverything
    and "receiveShadows". In fact, remove duplicates from the sum
    of both lists. }

  for I := 0 to App.FdReceiveShadows.Count - 1 do
    if App.FdReceiveShadows.Items[I] is TNodeX3DLightNode then
      HandleLight(TNodeX3DLightNode(App.FdReceiveShadows.Items[I]));

  for I := 0 to LightsCastingOnEverything.Count - 1 do
    HandleLight(TNodeX3DLightNode(LightsCastingOnEverything.Items[I]));
end;

procedure TDynLightArray.HandleLightAutomaticProjection(const L: TLight);
var
  ProjectionNear, ProjectionFar: Single;
begin
  if IsEmptyBox3D(ShadowCastersBox) then
  begin
    { No shadow casters? So any sensible values are fine. }
    ProjectionNear := 0.1;
    ProjectionFar := 1;
  end else
  begin
    { Projection near/far must include all shadow casters between
      light source and the shadow receivers. }
    L.Light.Box3DDistances(ShadowCastersBox, ProjectionNear, ProjectionFar);
    MaxTo1st(ProjectionNear, 0);
    MinTo1st(ProjectionFar, L.MaxShadowReceiverDistance);

    if ProjectionNear > ProjectionFar then
    begin
      { No *important* shadow casters? So any sensible values are fine. }
      ProjectionNear := 0.1;
      ProjectionFar := 1;
    end else
    begin
      { So we know now that ProjectionNear >= 0 and
        ProjectionFar >= ProjectionNear. }

      { final correction of auto-calculated projectionFar: must be > 0 }
      if ProjectionFar <= 0 then
        ProjectionFar := 1;

      { final correction of auto-calculated projectionNear: must be > 0,
        and preferably > some epsilon of projectionFar (to avoid depth
        precision problems). }
      MaxTo1st(ProjectionNear, ProjectionFar / 1000);
    end;
  end;

  if Log then
    WritelnLog('Shadow Maps', Format('Auto-calculated light source %s projectionNear is %f, projectionFar is %f',
      [L.Light.NodeTypeName, ProjectionNear, ProjectionFar]));

  { Set light node's projectionXxx values, if they are needed. }
  if L.Light.FdProjectionNear.Value = 0 then
    L.Light.FdProjectionNear.Value := ProjectionNear;
  if L.Light.FdProjectionFar.Value = 0 then
    L.Light.FdProjectionFar.Value := ProjectionFar;
end;

procedure TDynLightArray.HandleLightCastingOnEverything(Node: TVRMLNode);
begin
  if TNodeX3DLightNode(Node).FdShadows.Value then
    LightsCastingOnEverything.Add(Node);
end;

procedure ProcessShadowMapsReceivers(Model: TVRMLNode; Shapes: TVRMLShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);
var
  Lights: TDynLightArray;
  L: PLight;
  I: Integer;
begin
  { This is valid situation (TVRMLScene.RootNode may be nil).
    Nothing to do then. }
  if Model = nil then Exit;

  Lights := TDynLightArray.Create;
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
    Shapes.Traverse(@Lights.ShapeRemove, false);

    if Enable then
    begin
      Lights.DefaultShadowMapSize := DefaultShadowMapSize;
      Lights.ShadowCastersBox := EmptyBox3D;

      { calculate Lights.LightsCastingOnEverything first }
      Lights.LightsCastingOnEverything := TVRMLNodesList.Create;
      Model.EnumerateNodes(TNodeX3DLightNode, @Lights.HandleLightCastingOnEverything, false);

      Shapes.Traverse(@Lights.ShapeAdd, false);

      for I := 0 to Lights.Count - 1 do
      begin
        L := Lights.Pointers[I];

        Lights.HandleLightAutomaticProjection(L^);

        { Although we try to construct things only when they will be actually
          used (so no unused nodes should remain now for free), actually
          there is a chance something remained unused if HandleLight failed
          with VRMLWarning after FindLight. }
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
