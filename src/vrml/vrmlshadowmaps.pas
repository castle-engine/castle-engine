{
  Copyright 2010-2010 Michalis Kamburelis.

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
    @item(override appearance's "shaders", to use appropriate shadow map
      shader.)
  ) }
procedure ProcessShadowMapsReceivers(Model: TVRMLNode; Shapes: TVRMLShapeTree;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal;
  const DefaultVisualizeShadowMap: boolean;
  const PCF: TPercentageCloserFiltering);

type
  { Shadow map shader, either a normal one or a special designed for
    Variance Shadow Maps. This is used to choose from VRMLOpenGLRenderer
    whether we want VSM or not.

    This is dirty internal stuff.
    @exclude }
  TNodeShaderPartShadowMap = class(TNodeShaderPart)
  private
    VSMContents: string;
  public
    { Use a different, special shader code designed for Variance Shadow Maps.
      If you set this to @true, LoadContents will return a special hardcoded
      VSM shader. }
    VarianceShadowMapsEnabled: boolean;
    function LoadContents: string; override;
  end;

implementation

uses SysUtils, KambiUtils, VRMLFields, VRMLErrors, KambiStringUtils, Math,
  Boxes3D, KambiLog, VectorMath;

{$define read_interface}
{$define read_implementation}

function TNodeShaderPartShadowMap.LoadContents: string;
begin
  if VarianceShadowMapsEnabled then
  begin
    Result := VSMContents;
    FUsedFullUrl := 'INTERNAL-VARIANCE-SHADOW-MAP-SHADER';
  end else
    Result := inherited;
end;

const
  MaxBaseTextures = 1;
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
    Enable: boolean;
    DefaultShadowMapSize: Cardinal;
    DefaultVisualizeShadowMap: boolean;
    PCF: TPercentageCloserFiltering;
    ShadowMapShaders: array [boolean, 0..1] of TNodeComposedShader;
    ShadowCastersBox: TBox3D;
    LightsCastingOnEverything: TVRMLNodesList;

    { Find existing or add new TLight record for this light node.
      If Enable, this also creates shadow map and texture generator nodes
      for this light. }
    function FindLight(Light: TNodeX3DLightNode): PLight;

    function CreateShadowMapShader(const VisualizeShadowMap: boolean;
      const BaseTexCount: Cardinal): TNodeComposedShader;

    { Handle Appearance.shaders field for shadow maps.
      Adds, replaces or removes shaders, based on Enabled state and whether
      our shader is already there. }
    procedure HandleShaders(Shaders: TMFNode;
      const VisualizeShadowMap: boolean; const BaseTexCount: Cardinal);

    procedure HandleShape(Shape: TVRMLShape);

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

  if not Enable then
  begin
    Result^.ShadowMap := nil;
    Result^.TexGen := nil;
    Exit;
  end;

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
  end else
  begin
    Result^.ShadowMap := TNodeGeneratedShadowMap.Create('', '');
    Result^.ShadowMap.FdUpdate.Value := 'ALWAYS';
    Result^.ShadowMap.FdSize.Value := DefaultShadowMapSize;
  end;

  { Regardless if this is taken from defaultShadowMap or created,
    always set "light" to our light. This way user doesn't have to
    specify defaultShadowMap.light is the same light. }
  Result^.ShadowMap.FdLight.Value := Light;

  { Regardless if this is taken from defaultShadowMap or created,
    set NodeName, such that it has NodeNameSuffix. This is needed for
    HandleShadowMap, so that it can be removed later. }
  Result^.ShadowMap.NodeName := LightUniqueName + '_ShadowMap' + NodeNameSuffix;

  if DefaultVisualizeShadowMap then
    Result^.ShadowMap.FdCompareMode.Value := 'NONE';

  { create new ProjectedTextureCoordinate node }

  Result^.TexGen := TNodeProjectedTextureCoordinate.Create('', '');
  Result^.TexGen.NodeName := LightUniqueName + '_TexGen' + NodeNameSuffix;
  Result^.TexGen.FdProjector.Value := Light;
end;

function TDynLightArray.CreateShadowMapShader(const VisualizeShadowMap: boolean;
  const BaseTexCount: Cardinal): TNodeComposedShader;
const
  FragmentShader: array [boolean, 0..MaxBaseTextures] of string =
    ( ( {$I shadow_map_0.fs.inc},
        {$I shadow_map_1.fs.inc} ),
      ( {$I shadow_map_0_show_depth.fs.inc},
        {$I shadow_map_1_show_depth.fs.inc} )
    );
  VSMFragmentShader: array [boolean, 0..MaxBaseTextures] of string =
    ( ( {$I variance_shadow_map_0.fs.inc},
        {$I variance_shadow_map_1.fs.inc} ),
      ( {$I variance_shadow_map_0_show_depth.fs.inc},
        {$I variance_shadow_map_1_show_depth.fs.inc} )
    );
  FragmentShaderCommon = {$I shadow_map_common.fs.inc};
  VSMFragmentShaderCommon = {$I variance_shadow_map_common.fs.inc};
  PCFDefine: array [TPercentageCloserFiltering] of string =
  ( '', '#define PCF4', '#define PCF4_BILINEAR', '#define PCF16' );
var
  I: Integer;
  Part: TNodeShaderPartShadowMap;
begin
  Result := TNodeComposedShader.Create('', '');
  Result.NodeName := 'Shader_ShadowMap_' + IntToStr(BaseTexCount) + 'Textures' + NodeNameSuffix;
  Result.FdLanguage.Value := 'GLSL';
  for I := 0 to BaseTexCount - 1 do
    Result.AddCustomField(TSFInt32.Create(Result, 'texture' + IntToStr(I), I));
  Result.AddCustomField(TSFInt32.Create(Result, 'shadowMap', BaseTexCount));

  Part := TNodeShaderPartShadowMap.Create('', '');
  Part.FdType.Value := 'FRAGMENT';
  Part.FdUrl.Items.Count := 1;
  Part.FdUrl.Items[0] := NL + FragmentShader[VisualizeShadowMap, BaseTexCount];
  Part.VSMContents := VSMFragmentShader[VisualizeShadowMap, BaseTexCount];
  Result.FdParts.Add(Part);

  Part := TNodeShaderPartShadowMap.Create('', '');
  Part.FdType.Value := 'FRAGMENT';
  Part.FdUrl.Items.Count := 1;
  Part.FdUrl.Items[0] := NL + PCFDefine[PCF] + NL + FragmentShaderCommon;
  Part.VSMContents := VSMFragmentShaderCommon;
  Result.FdParts.Add(Part);
end;

procedure TDynLightArray.HandleShaders(Shaders: TMFNode;
  const VisualizeShadowMap: boolean; const BaseTexCount: Cardinal);
begin
  if Enable then
  begin
    if ShadowMapShaders[VisualizeShadowMap, BaseTexCount] = nil then
      ShadowMapShaders[VisualizeShadowMap, BaseTexCount] :=
        CreateShadowMapShader(VisualizeShadowMap, BaseTexCount);

    { If this exact shader node is already present (maybe we process
      the same shape more than once?), then do nothing.
      Note: we should, in such case, have Shaders.Count = 1...
      unless user took the processed VRML and messed with it.
      So better not check/depend on it. }
    if Shaders.Items.IndexOf(ShadowMapShaders[VisualizeShadowMap, BaseTexCount]) <> -1 then
      Exit;

    { We have to remove previous shaders, regardless if they were our own
      shaders or custom user shaders. This will be done only for shapes
      with receiveShadows, so no harm happens to other stuff. }
    Shaders.Clear;
    Shaders.Add(ShadowMapShaders[VisualizeShadowMap, BaseTexCount]);
  end else
  begin
    { Detect if it's our own shader (by checking NodeNameSuffix).
      Only then, remove it. }
    if (Shaders.Count = 1) and
       IsSuffix(NodeNameSuffix, Shaders.Items[0].NodeName) then
      Shaders.Clear;
  end;
end;

procedure TDynLightArray.HandleShape(Shape: TVRMLShape);

  { Add/Remove/Replace ShadowMap to the textures used by the material
    (TODO: and by shaders, in the future; for now, we overuse the fact
    that our shaders will get normal textures anyway).

    Converts Texture to MultiTexture, to add the shadow map
    preserving old texture.

    Returns the count of textures in TexturesCount, not counting the last
    ShadowMap texture. }
  procedure HandleShadowMap(var Texture: TVRMLNode;
    const ShadowMap: TNodeGeneratedShadowMap; out TexturesCount: Cardinal);
  var
    MTexture: TNodeMultiTexture;
  begin
    { calculate MTexture and TexturesCount }
    if (Texture <> nil) and
       (Texture is TNodeMultiTexture) then
    begin
      { if Texture already is MultiTexture, then we're already Ok }
      MTexture := TNodeMultiTexture(Texture);
      TexturesCount := MTexture.FdTexture.Count;

      { If the last texture is the one we want to add
        (this also implies that ShadowMap is non-nil and so Enable is true)
        then we're all set, nothing more to do.
        This may happen, as HandleLight may iterate many times over
        the same light. }
      if (TexturesCount <> 0) and
         (MTexture.FdTexture.Items.Last = ShadowMap) then
      begin
        Dec(TexturesCount);
        Exit;
      end;

      { Remove old GeneratedShadowMap that we added there. }
      if (TexturesCount <> 0) and
         IsSuffix(NodeNameSuffix, MTexture.FdTexture.Items.Last.NodeName) then
      begin
        MTexture.FdTexture.Delete(TexturesCount - 1);
        Dec(TexturesCount);
      end;
    end else
    begin
      MTexture := TNodeMultiTexture.Create('', '');
      if Texture <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texture" field on the same position) }
        MTexture.PositionInParent := Texture.PositionInParent;
        MTexture.FdTexture.Add(Texture);
        TexturesCount := 1;
      end else
        TexturesCount := 0;
      Texture := MTexture;
    end;

    Assert(Texture = MTexture);
    Assert(TexturesCount = MTexture.FdTexture.Count);

    if Enable then
      MTexture.FdTexture.Add(ShadowMap);
  end;

  { Add/Remove/Replace to the texCoord field.

    Converts texCoord to multi tex coord, to preserve previous tex coord.

    May remove some texCoord nodes, knowing that only the 1st
    RelevantTexCoordsCount nodes are used.

    Returns the count of texCoords in TexCoordsCount, not counting the last
    TexGen node. }
  procedure HandleTexGen(var TexCoord: TVRMLNode;
    const TexGen: TNodeProjectedTextureCoordinate; out TexCoordsCount: Cardinal;
    const RelevantTexCoordsCount: Cardinal);
  var
    MTexCoord: TNodeMultiTextureCoordinate;
  begin
    { calculate MTexCoord and TexCoordsCount }
    if (TexCoord <> nil) and
       (TexCoord is TNodeMultiTextureCoordinate) then
    begin
      { if TexCoord already is MultiTextureCoordinate, then we're already Ok }
      MTexCoord := TNodeMultiTextureCoordinate(TexCoord);
      TexCoordsCount := MTexCoord.FdTexCoord.Count;

      { If the last texcoord is the one we want to add
        (this also implies that TexGen is non-nil and so Enable is true)
        then we're all set, nothing more to do.
        This may happen, as HandleLight may iterate many times over
        the same light. }
      if (TexCoordsCount <> 0) and
         (MTexCoord.FdTexCoord.Items.Last = TexGen) then
      begin
        Dec(TexCoordsCount);
        Exit;
      end;

      { Remove old TextureCoordinate that we added there. }
      if (TexCoordsCount <> 0) and
         IsSuffix(NodeNameSuffix, MTexCoord.FdTexCoord.Items.Last.NodeName) then
      begin
        MTexCoord.FdTexCoord.Delete(TexCoordsCount - 1);
        Dec(TexCoordsCount);
      end;
    end else
    begin
      MTexCoord := TNodeMultiTextureCoordinate.Create('', '');
      if TexCoord <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texCoord" field on the same position) }
        MTexCoord.PositionInParent := TexCoord.PositionInParent;
        MTexCoord.FdTexCoord.Add(TexCoord);
        TexCoordsCount := 1;
      end else
        TexCoordsCount := 0;
      TexCoord := MTexCoord;
    end;

    Assert(TexCoord = MTexCoord);
    Assert(TexCoordsCount = MTexCoord.FdTexCoord.Count);

    if Enable then
    begin
      { At this point, remove not relevant texCoords. }
      if TexCoordsCount > RelevantTexCoordsCount then
      begin
        MTexCoord.FdTexCoord.TheCount := RelevantTexCoordsCount;
        TexCoordsCount := MTexCoord.FdTexCoord.Count;
      end;

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
  procedure HandleTextureTransform(var TextureTransform: TVRMLNode);
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

var
  ShapeNode: TNodeX3DShapeNode;
  App: TNodeAppearance;

  { 1. Add/Remove/Replace ShadowMap
    2. Add/Remove/Replace TexGen
    3. Add/Remove/Replace shader to the shaders field.

    TODO: finish handling multiple shadow maps on the same mesh.
    This should basically already work (we check is old texture / texCoord
    already a multi-texture, and behave Ok), what is missing is actually
    implementing and using then a shader taking into account many shadow maps.
  }
  procedure HandleLight(LightNode: TNodeX3DLightNode);
  var
    Light: PLight;
    Texture, TextureTransform: TVRMLNode;
    TexCoord: TVRMLNode;
    TexturesCount, TexCoordsCount: Cardinal;
    VisualizeShadowMap: boolean;
    Box: TBox3D;
    MinReceiverDistance, MaxReceiverDistance: Single;
  begin
    Light := FindLight(LightNode);

    Texture := ShapeNode.Texture;
    HandleShadowMap(Texture, Light^.ShadowMap, TexturesCount);
    App.FdTexture.Value := Texture;

    TexCoord := Shape.Geometry.TexCoordField.Value;
    HandleTexGen(TexCoord, Light^.TexGen, TexCoordsCount, TexturesCount);
    Shape.Geometry.TexCoordField.Value := TexCoord;

    TextureTransform := App.FdTextureTransform.Value;
    HandleTextureTransform(TextureTransform);
    App.FdTextureTransform.Value := TextureTransform;

    if TexCoordsCount <> TexturesCount then
    begin
      VRMLWarning(vwIgnorable, Format('Texture units (%d) used by the Appearance.texture non-equal with used by texCoord (%d). This is too complicated setup to easily use shadow maps by the "receiveShadows".',
        [TexturesCount, TexCoordsCount]));
      Exit;
    end;

    if TexCoordsCount > MaxBaseTextures then
    begin
      VRMLWarning(vwIgnorable, Format('Shadow map shader for %d base textures not implemented yet.', [TexCoordsCount]));
      Exit;
    end;

    VisualizeShadowMap := (Light^.ShadowMap <> nil) and
      (Light^.ShadowMap.FdCompareMode.Value = 'NONE');

    HandleShaders(App.FdShaders, VisualizeShadowMap, TexCoordsCount);

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
begin
  ShapeNode := Shape.State.ShapeNode;
  if ShapeNode = nil then
  begin
    HandleShadowCaster;
    Exit; { VRML <= 1.0 shapes cannot be shadow maps receivers }
  end;

  { TODO: it's possible that App = nil, but still
    LightsCastingOnEverything.Count <> 0. We should create Appearance
    node in this case. }
  App := ShapeNode.Appearance;
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
  const DefaultShadowMapSize: Cardinal;
  const DefaultVisualizeShadowMap: boolean;
  const PCF: TPercentageCloserFiltering);
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
    Lights.Enable := Enable;
    Lights.DefaultShadowMapSize := DefaultShadowMapSize;
    Lights.DefaultVisualizeShadowMap := DefaultVisualizeShadowMap;
    Lights.PCF := PCF;
    Lights.ShadowCastersBox := EmptyBox3D;

    { calculate Lights.LightsCastingOnEverything first }
    Lights.LightsCastingOnEverything := TVRMLNodesList.Create;
    Model.EnumerateNodes(TNodeX3DLightNode, @Lights.HandleLightCastingOnEverything, false);

    { Enumerate all (active and not) shapes for the receiveShadows
      calculations. In case a shape is not active, it may become active later
      (e.g. by Switch.whichChoice change), and ProcessShadowMapsReceivers
      will not necessarily be run again. So we better account for this
      shape already. }
    Shapes.Traverse(@Lights.HandleShape, false);

    if Enable then
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
  finally FreeAndNil(Lights) end;
end;

end.
