{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Express terrain as TCastleScene. }
unit GameScene;

interface

uses Classes,
  CastleVectors, CastleTerrain, CastleScene, X3DNodes, X3DFields,
  CastleRenderOptions;

type
  { Scene showing a terrain.
    Uses CastleTerrain to build terrain shape.
    Uses custom shader effect to have nice and configurable appearance. }
  TTerrainScene = class(TCastleScene)
  private
    TerrainNode: TAbstractChildNode;
    TerrainNodeTriangulated: boolean;
    Appearance: TAppearanceNode;
    Effect: TEffectNode;

    function GetLighting: boolean;
    procedure SetLighting(const Value: boolean);
    function GetTextured: boolean;
    procedure SetTextured(const Value: boolean);
  public
    UseTriangulatedNode: boolean;

    { Shader parameters.
      @groupBegin }
    TextureHeights: array [0..3] of Single;
    TextureHeightsFields: array [0..3] of TSFFloat;
    UVScale: array [1..3] of Single;
    UVScaleFields: array [1..3] of TSFFloat;
    TextureMix, NormalDark, NormalDarkening: Single;
    { @groupEnd }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Regenerate geometry (vertexes, normals etc.) to show the current
      Terrain instance. This only calls @link(TTerrain.Height) method.

      Size is the size of the square layer around the (0, 0). }
    procedure Regenerate(Terrain: TTerrain;
      const Divisions: Cardinal; const Size: Single);

    property Lighting: boolean read GetLighting write SetLighting;
    property Textured: boolean read GetTextured write SetTextured;

    { Apply new shader values. }
    procedure UpdateShader(Sender: TObject);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleFilesUtils, CastleRectangles;

constructor TTerrainScene.Create(AOwner: TComponent);

  procedure SetDefaultShaderUniforms;
  begin
    TextureHeights[0] := 5;
    TextureHeights[1] := 6;
    TextureHeights[2] := 7;
    TextureHeights[3] := 10;
    UVScale[1] := 0.11;
    UVScale[2] := 0.26;
    UVScale[3] := 0.36;
    TextureMix := 1;
    NormalDark := 0.94;
    NormalDarkening := 0.3;
  end;

  procedure AdjustAppearance;
  var
    VertexPart, FragmentPart: TEffectPartNode;
    Tex1, Tex2, Tex3: TImageTextureNode;
    I: Integer;
  begin
    { initialize Effect node, for a shader effect }
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Appearance.SetEffects([Effect]);

    { pass textures to shader effect }
    Tex1 := TImageTextureNode.Create;
    Tex1.SetUrl(['castle-data:/textures/island_sand2_d.jpg']);
    Tex2 := TImageTextureNode.Create;
    Tex2.SetUrl(['castle-data:/textures/ground_mud2_d.jpg']);
    Tex3 := TImageTextureNode.Create;
    Tex3.SetUrl(['castle-data:/textures/mntn_white_d.jpg']);

    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_1', [], Tex1));
    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_2', [], Tex2));
    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_3', [], Tex3));

    { pass uniforms to shader effect }
    for I := Low(TextureHeights) to High(TextureHeights) do
    begin
      TextureHeightsFields[I] := TSFFloat.Create(
        Effect, true, 'h' + IntToStr(I), TextureHeights[I]);
      Effect.AddCustomField(TextureHeightsFields[I]);
    end;
    for I := Low(UVScale) to High(UVScale) do
    begin
      UVScaleFields[I] := TSFFloat.Create(
        Effect, true, 'uv_scale_' + IntToStr(I), UVScale[I]);
      Effect.AddCustomField(UVScaleFields[I]);
    end;
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'texture_mix', TextureMix));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_dark', NormalDark));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_darkening', NormalDarkening));

    { initialize 2 EffectPart nodes (one for vertex shader, one for fragment shader) }
    FragmentPart := TEffectPartNode.Create;
    FragmentPart.ShaderType := stFragment;
    FragmentPart.SetUrl(['castle-data:/shaders/terrain.fs']);

    VertexPart := TEffectPartNode.Create;
    VertexPart.ShaderType := stVertex;
    VertexPart.SetUrl(['castle-data:/shaders/terrain.vs']);

    Effect.SetParts([FragmentPart, VertexPart]);

    { make the material lit }
    Appearance.Material := TMaterialNode.Create;
  end;

begin
  inherited;
  SetDefaultShaderUniforms;
  Appearance := TAppearanceNode.Create;
  Appearance.KeepExistingBegin; // it's easiest to manage release of Appearance
  AdjustAppearance;
  UseTriangulatedNode := true;
end;

destructor TTerrainScene.Destroy;
begin
  inherited;
  // remove after RootNode containing this is removed too
  FreeAndNil(Appearance);
end;

procedure TTerrainScene.Regenerate(Terrain: TTerrain;
  const Divisions: Cardinal; const Size: Single);
var
  Root: TX3DRootNode;
  Range: TFloatRectangle;
begin
  if (TerrainNode <> nil) and (TerrainNodeTriangulated <> UseTriangulatedNode) then
  begin
    RootNode := nil; // will free TerrainNode and RootNode
    TerrainNode := nil;
  end;

  Range := FloatRectangle(-Size/2, -Size/2, Size, Size);

  if TerrainNode = nil then
  begin
    if UseTriangulatedNode then
      TerrainNode := Terrain.CreateTriangulatedNode(Divisions, Range, Range, Appearance)
    else
      TerrainNode := Terrain.CreateNode(Divisions, Range, Range, Appearance);
    TerrainNodeTriangulated := UseTriangulatedNode;
    Root := TX3DRootNode.Create;
    Root.AddChildren(TerrainNode);
    Load(Root, true);
  end else
  begin
    if UseTriangulatedNode then
      Terrain.UpdateTriangulatedNode(TerrainNode, Divisions, Range, Range)
    else
      Terrain.UpdateNode(TerrainNode, Divisions, Range, Range);
  end;
end;

function TTerrainScene.GetLighting: boolean;
begin
  Result := Appearance.Material <> nil;
end;

procedure TTerrainScene.SetLighting(const Value: boolean);
begin
  if Value then
  begin
    if Appearance.Material = nil then
      Appearance.Material := TMaterialNode.Create;
  end else
    Appearance.Material := nil;
end;

function TTerrainScene.GetTextured: boolean;
begin
  Result := Effect.Enabled;
end;

procedure TTerrainScene.SetTextured(const Value: boolean);
begin
  Effect.Enabled := Value;
end;

procedure TTerrainScene.UpdateShader(Sender: TObject);
var
  I: Integer;
begin
  { Pass new value to the existing shader. }
  for I := Low(TextureHeights) to High(TextureHeights) do
    TextureHeightsFields[I].Send(TextureHeights[I]);
  for I := Low(UVScale) to High(UVScale) do
    UVScaleFields[I].Send(UVScale[I]);
  (Effect.Field('texture_mix') as TSFFloat).Send(TextureMix);
  (Effect.Field('normal_dark') as TSFFloat).Send(NormalDark);
  (Effect.Field('normal_darkening') as TSFFloat).Send(NormalDarkening);
end;

end.
