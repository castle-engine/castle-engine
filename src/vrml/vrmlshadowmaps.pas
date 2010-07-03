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

uses VRMLNodes;

type
  TPercentageCloserFiltering = (pcfNone, pcf4, pcf4Bilinear, pcf16);

{ Automatically handle VRML/X3D "receiveShadows" field
  by inserting appropriate lower-level nodes.

  For each shape with "receiveShadows", we:
  @orderedList(
    @item(extend it's "texture" field with appropriate GeneratedShadowMap,)
    @item(extend it's "texCoord" field with appropriate
      TextureCoordinateGenerator (with mode="PROJECTION"),)
    @item(override appearance's "shaders", to use appropriate shadow map
      shader.)
  ) }
procedure ProcessShadowMapsReceivers(Model: TVRMLNode;
  const DefaultShadowMapSize: Cardinal;
  const DefaultVisualizeShadowMap: boolean;
  const PCF: TPercentageCloserFiltering);

implementation

uses SysUtils, KambiUtils, VRMLFields, VRMLErrors;

{$define read_interface}
{$define read_implementation}

const
  MaxBaseTextures = 1;

type
  { Information about light source relevant for shadow maps. }
  TLight = record
    Light: TNodeX3DLightNode;
    ShadowMap: TNodeGeneratedShadowMap;
    TexGen: TNodeTextureCoordinateGenerator;
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
    DefaultVisualizeShadowMap: boolean;
    PCF: TPercentageCloserFiltering;
    ShadowMapShaders: array [boolean, 0..1] of TNodeComposedShader;
    function FindLight(Light: TNodeX3DLightNode): PLight;
    function CreateShadowMapShader(const VisualizeShadowMap: boolean;
      const BaseTexCount: Cardinal): TNodeComposedShader;
    procedure HandleShape(Node: TVRMLNode);
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
    Result^.ShadowMap.NodeName := LightUniqueName + '_Automatic_ShadowMap';
    Result^.ShadowMap.FdUpdate.Value := 'ALWAYS';
    Result^.ShadowMap.FdSize.Value := DefaultShadowMapSize;
  end;
  { Regardless if this is taken from defaultShadowMap or created,
    always set "light" to our light. This way user doesn't have to
    specify defaultShadowMap.light is the same light. }
  Result^.ShadowMap.FdLight.Value := Light;

  if DefaultVisualizeShadowMap then
    Result^.ShadowMap.FdCompareMode.Value := 'NONE';

  { create new TextureCoordinateGenerator node }

  Result^.TexGen := TNodeTextureCoordinateGenerator.Create('', '');
  Result^.TexGen.NodeName := LightUniqueName + '_Automatic_TexGen';
  Result^.TexGen.FdProjectedLight.Value := Light;
  Result^.TexGen.FdMode.Value := 'PROJECTION';
end;

function TDynLightArray.CreateShadowMapShader(const VisualizeShadowMap: boolean;
  const BaseTexCount: Cardinal): TNodeComposedShader;
const
  VarianceShadowMaps = false; { TODO: hardcoded }
  ShadowMapFragmentShader: array [boolean, boolean, 0..MaxBaseTextures] of string =
  ( { Variance Shadow Maps = false }
    ( ( {$I shadow_map_0.fs.inc}, {$I shadow_map_1.fs.inc} ),
      { TODO: no shadow_map_0_depth.fs.inc version }
      ( {$I shadow_map_0.fs.inc}, {$I shadow_map_1_show_depth.fs.inc} )
    ),
    { Variance Shadow Maps = true }
    ( ( {$I variance_shadow_map_0.fs.inc}, {$I variance_shadow_map_1.fs.inc} ),
      { TODO: no shadow_map_0_depth.fs.inc version }
      ( {$I variance_shadow_map_0.fs.inc}, {$I variance_shadow_map_1_show_depth.fs.inc} )
    )
  );
  ShadowMapFragmentShaderCommon: array [boolean] of string =
  ( {$I shadow_map_common.fs.inc}, {$I variance_shadow_map_common.fs.inc} );
  PCFDefine: array [TPercentageCloserFiltering] of string =
  ( '', '#define PCF4', '#define PCF4_BILINEAR', '#define PCF16' );
var
  I: Integer;
  Part: TNodeShaderPart;
begin
  Result := TNodeComposedShader.Create('', '');
  Result.NodeName := 'Shader_ShadowMap_' + IntToStr(BaseTexCount) + 'Textures';
  Result.FdLanguage.Value := 'GLSL';
  for I := 0 to BaseTexCount - 1 do
    Result.AddCustomField(TSFInt32.Create(Result, 'texture' + IntToStr(I), I));
  Result.AddCustomField(TSFInt32.Create(Result, 'shadowMap', BaseTexCount));

  Part := TNodeShaderPart.Create('', '');
  Part.FdType.Value := 'FRAGMENT';
  Part.FdUrl.Items.Count := 1;
  Part.FdUrl.Items[0] := NL + ShadowMapFragmentShader[
    VarianceShadowMaps, VisualizeShadowMap, BaseTexCount];
  Result.FdParts.AddItem(Part);

  Part := TNodeShaderPart.Create('', '');
  Part.FdType.Value := 'FRAGMENT';
  Part.FdUrl.Items.Count := 1;
  { PCFDefine actually is useless for VSM, but no harm in including it. }
  Part.FdUrl.Items[0] := NL + PCFDefine[PCF] + NL +
    ShadowMapFragmentShaderCommon[VarianceShadowMaps];
  Result.FdParts.AddItem(Part);
end;

procedure TDynLightArray.HandleShape(Node: TVRMLNode);
var
  Shape: TNodeShape;
  App: TNodeAppearance;

  { This:
    1. adds ShadowMap to the textures used by the material and by shaders
       (converting texture to MultiTexture, to add the shadow map
       preserving old texture.)
    2. adds TexGen to the texCoord field,
       (converting texCoord to multi tex coord, to preserve
       previous tex coord.)
    3. adds appropriate shader to the shaders field.

    TODO: finish handling multiple shadow maps on the same mesh.
    This should basically already work (we check is old texture / texCoord
    already a multi-texture, and behave Ok), what is missing is actually
    implementing and using then a shader taking into account many shadow maps.
  }
  procedure HandleLight(LightNode: TNodeX3DLightNode);
  var
    Light: PLight;
    OldTexture: TVRMLNode;
    NewTexture: TNodeMultiTexture;
    OldTexCoord: TVRMLNode;
    NewTexCoord: TNodeMultiTextureCoordinate;
    OriginalTextures, OriginalTexCoords: Cardinal;
    VisualizeShadowMap: boolean;
  begin
    Light := FindLight(LightNode);

    OldTexture := Shape.Texture;
    OldTexCoord := TVRMLGeometryNode(Shape.FdGeometry.Value).TexCoordField.Value;

    OriginalTextures := 0;
    OriginalTexCoords := 0;

    { calculate NewTexture and OriginalTextures }
    if (OldTexture <> nil) and
       (OldTexture is TNodeMultiTexture) then
    begin
      { if texture already is MultiTexture, then we're already Ok }
      NewTexture := TNodeMultiTexture(OldTexture);
      OriginalTextures := NewTexture.FdTexture.Count;
    end else
    begin
      NewTexture := TNodeMultiTexture.Create('', '');
      if OldTexture <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texture" field on the same position) }
        NewTexture.PositionInParent := OldTexture.PositionInParent;
        NewTexture.FdTexture.AddItem(OldTexture);
        OriginalTextures := 1;
      end;
    end;

    { calculate NewTexCoord and OriginalTexCoords }
    if (OldTexCoord <> nil) and
       (OldTexCoord is TNodeMultiTextureCoordinate) then
    begin
      { if texCoord already is MultiTextureCoordinate, then we're already Ok }
      NewTexCoord := TNodeMultiTextureCoordinate(OldTexCoord);
      OriginalTexCoords := NewTexCoord.FdTexCoord.Count;
    end else
    begin
      NewTexCoord := TNodeMultiTextureCoordinate.Create('', '');
      if OldTexCoord <> nil then
      begin
        { set position in parent only for more deterministic output
          (new "texCoord" field on the same position) }
        NewTexCoord.PositionInParent := OldTexCoord.PositionInParent;
        NewTexCoord.FdTexCoord.AddItem(OldTexCoord);
        OriginalTexCoords := 1;
      end;
    end;

    { At this point we didn't do much yet (we only eventually
      converted texture and texCoord to be in multi-texture versions).
      So check and just exit if we decide this node is already processed,
      or some trouble may occur. }

    if (NewTexture.FdTexture.Items.IndexOf(Light^.ShadowMap) <> -1) and
       (NewTexCoord.FdTexCoord.Items.IndexOf(Light^.TexGen) <> -1) then
      Exit;

    if OriginalTexCoords <> OriginalTextures then
    begin
      VRMLWarning(vwIgnorable, 'Texture units used by the Appearance.texture non-equal with used by texCoord. This is too complicated setup to easily use shadow maps by the "receiveShadows".');
      Exit;
    end;

    if OriginalTexCoords > MaxBaseTextures then
    begin
      VRMLWarning(vwIgnorable, Format('Shadow map shader for %d base textures not implemented yet.', [OriginalTexCoords]));
      Exit;
    end;

    NewTexture.FdTexture.AddItem(Light^.ShadowMap);
    NewTexCoord.FdTexCoord.AddItem(Light^.TexGen);

    App.FdTexture.Value := NewTexture;
    TVRMLGeometryNode(Shape.FdGeometry.Value).TexCoordField.Value := NewTexCoord;

    VisualizeShadowMap := Light^.ShadowMap.FdCompareMode.Value = 'NONE';

    if ShadowMapShaders[VisualizeShadowMap, OriginalTexCoords] = nil then
      ShadowMapShaders[VisualizeShadowMap, OriginalTexCoords] :=
        CreateShadowMapShader(VisualizeShadowMap, OriginalTexCoords);
    App.FdShaders.AddItem(ShadowMapShaders[VisualizeShadowMap, OriginalTexCoords]);
  end;

var
  I: Integer;
begin
  Shape := Node as TNodeShape;

  { HandleLight needs here a shape with geometry with texCoord.
    Better check it here, before we start changing anything. }
  if Shape.FdGeometry.Value = nil then Exit;

  if (not (Shape.FdGeometry.Value is TVRMLGeometryNode)) or
     (TVRMLGeometryNode(Shape.FdGeometry.Value).TexCoordField = nil) then
  begin
    VRMLWarning(vwIgnorable, 'Geometry node "' + Shape.FdGeometry.Value.NodeTypeName + '" does not have a texCoord, cannot be shadow maps receiver.');
    Exit;
  end;

  App := Shape.Appearance;
  if App = nil then Exit;

  for I := 0 to App.FdReceiveShadows.Count - 1 do
    if App.FdReceiveShadows.Items[I] is TNodeX3DLightNode then
      HandleLight(TNodeX3DLightNode(App.FdReceiveShadows.Items[I]));
end;

procedure ProcessShadowMapsReceivers(Model: TVRMLNode;
  const DefaultShadowMapSize: Cardinal;
  const DefaultVisualizeShadowMap: boolean;
  const PCF: TPercentageCloserFiltering);
var
  Lights: TDynLightArray;
begin
  Lights := TDynLightArray.Create;
  try
    Lights.DefaultShadowMapSize := DefaultShadowMapSize;
    Lights.DefaultVisualizeShadowMap := DefaultVisualizeShadowMap;
    Lights.PCF := PCF;

    { Enumerate all (active and not) shapes for the receiveShadows
      calculations. In case a shape is not active, it may become active later
      (e.g. by Switch.whichChoice change), and ProcessShadowMapsReceivers
      will not necessarily be run again. So we better account for this
      shape already. }
    Model.EnumerateNodes(TNodeShape, @Lights.HandleShape, false);

    { no need to call FreeIfUnused on eventually created ShadowMapShaders,
      or ShadowMap or TexGen instances: they are all constructed smartly,
      such that we know that they are constructed only when are actually
      used (referenced by Model). }
  finally FreeAndNil(Lights) end;
end;

end.
