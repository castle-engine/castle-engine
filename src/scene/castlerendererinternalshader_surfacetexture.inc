{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2010-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating shader code for "surface textures" that modify the corresponding
  material parameter. }

{$ifdef read_interface}

//type
  TSurfaceTexture = (
    stEmissive,
    stAmbient,
    stSpecular,
    stShininess,
    stMetallicRoughness,
    stOcclusion
  );

  { Texture that has a regular sampler (storage) on the GPU side.
    IOW, not ttShader. }
  TTextureTypeSample = tt2D..tt3D;

  TSurfaceTextureShader = record
    Enable: boolean;
    TextureUnit, TextureCoordinatesId: Cardinal;
    UniformTextureName: String;
    SurfaceTexture: TSurfaceTexture;
    TextureType: TTextureTypeSample;
    procedure PrepareHash(var Hash: TShaderCodeHash);
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
  end;

  { Manage a number of surface textures shaders.
    Follows castlerendererinternalshader_feature_api.md API conventions. }
  TSurfaceTextureShaderList = record
    Items: array [TSurfaceTexture] of TSurfaceTextureShader;
    procedure Clear;
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSurfaceTextureShader ------------------------------------------------------ }

procedure TSurfaceTextureShader.PrepareHash(var Hash: TShaderCodeHash);
var
  HashMultiplier: UInt32;
begin
  if Enable then
  begin
    HashMultiplier := 2063 * (1 + Ord(SurfaceTexture));
    Hash.AddInteger(HashMultiplier * (
      2069 * (1 + TextureUnit) +
      2081 * (1 + TextureCoordinatesId) +
      2087 * (1 + Ord(TextureType))
    ));
  end;
end;

procedure TSurfaceTextureShader.GenerateCode(const Shader: TShader);
const
  { Simplified version of TTextureShader.Enable .
    Note that we use texture2DProj, texture3DProj to pass full 4D
    texture coordinates, just in case someone uses TextureCoordinate4D
    (or projected texture mapping) with them. }
  TextureQuery: array [TTextureTypeSample] of String = (
    'texture2DProj({uniform_texture_name}, {coord_name})', // tt2D
    'textureCube  ({uniform_texture_name}, {coord_name}.xyz)', // ttCubeMap
    'texture3DProj({uniform_texture_name}, {coord_name})' // tt3D
  );

  { Note: The PlugCode is only determined by SurfaceTexture.
    So it *camnot* affect the hash, so we *do not* have to care about it
    during PrepareHash. }
  PlugCode: array [TSurfaceTexture] of String = (
    // stEmissive
    'void PLUG_material_emissive(inout vec3 parameter)' + NL +
    '{' + NL +
    '  parameter *= castle_texture_color_to_linear({texture_query}.rgb);' + NL +
    '}',

    // stAmbient
    'void PLUG_material_ambient(inout vec3 parameter)' + NL +
    '{' + NL +
    '  parameter *= castle_texture_color_to_linear({texture_query}.rgb);' + NL +
    '}',

    // stSpecular
    'void PLUG_material_specular(inout vec3 parameter)' + NL +
    '{' + NL +
    '  parameter *= castle_texture_color_to_linear({texture_query}.rgb);' + NL +
    '}',

    // stShininess
    'void PLUG_material_shininess(inout float parameter)' + NL +
    '{' + NL +
    '  parameter *= {texture_query}.a;' + NL +
    '}',

    // stMetallicRoughness
    'void PLUG_material_metallic_roughness(inout float metallic, inout float roughness)' + NL +
    '{' + NL +
    { Roughness is stored in the 'g' channel, metallic is stored in the 'b' channel.
      Read it to place metallic is x, roughness in y. }
    '  vec2 mr = {texture_query}.bg;' + NL +
    '  metallic *= mr.x;' + NL +
    '  roughness *= mr.y;' + NL +
    '}',

    // stOcclusion
    'uniform float castle_MaterialOcclusionStrength;' + NL +
    'void PLUG_material_occlusion(inout vec4 color)' + NL +
    '{' + NL +
    '  float occlusion = {texture_query}.r;' + NL +
    '  color.rgb = mix(color.rgb, color.rgb * occlusion, castle_MaterialOcclusionStrength);' + NL +
    '}'
  );
var
  FinalTextureQuery: String;
begin
  if Enable then
  begin
    FinalTextureQuery := SReplacePatterns(TextureQuery[TextureType],
      [
        '{uniform_texture_name}',
        '{coord_name}'
      ],
      [
        UniformTextureName,
        TTextureCoordinateShader.CoordName(TextureCoordinatesId)
      ], false);

    { This is only used with TextureType that has non-empty sampler name,
      so it is not ttShader. }
    Assert(SamplerFromTextureType[TextureType] <> '');

    Shader.Plug(stFragment, SReplacePatterns(
      'uniform {texture_sampler_type} {uniform_texture_name};' + NL +
      {$ifndef OpenGLES} // avoid redeclaring stuff when no "separate compilation units" (OpenGLES)
      'varying vec4 {coord_name};' + NL +
      '// Declare functions defined in tone_mapping.fs' + NL +
      'vec4 castle_texture_color_to_linear(const in vec4 srgbIn);' + NL +
      'vec3 castle_texture_color_to_linear(const in vec3 srgbIn);' + NL +
      {$endif}
      PlugCode[SurfaceTexture],
      [
        '{uniform_texture_name}',
        '{coord_name}',
        '{texture_query}',
        '{texture_sampler_type}'
      ],
      [
        UniformTextureName,
        TTextureCoordinateShader.CoordName(TextureCoordinatesId),
        FinalTextureQuery,
        SamplerFromTextureType[TextureType]
      ], false));
  end;
end;

procedure TSurfaceTextureShader.SetUniformsOnce(const AProgram: TX3DShaderProgram);
begin
  if Enable then
    AProgram.SetUniform(UniformTextureName, Integer(TextureUnit));
end;

{ TSurfaceTextureShaderList -------------------------------------------------- }

procedure TSurfaceTextureShaderList.Clear;
var
  SurfaceTexture: TSurfaceTexture;
begin
  for SurfaceTexture := Low(TSurfaceTexture) to High(TSurfaceTexture) do
    { No need to reset other Items[SurfaceTexture] fields. }
    Items[SurfaceTexture].Enable := false;
end;

procedure TSurfaceTextureShaderList.GenerateCode(const Shader: TShader);
var
  SurfaceTexture: TSurfaceTexture;
begin
  for SurfaceTexture := Low(TSurfaceTexture) to High(TSurfaceTexture) do
    Items[SurfaceTexture].GenerateCode(Shader);
end;

procedure TSurfaceTextureShaderList.SetUniformsOnce(const AProgram: TX3DShaderProgram);
var
  SurfaceTexture: TSurfaceTexture;
begin
  for SurfaceTexture := Low(TSurfaceTexture) to High(TSurfaceTexture) do
    Items[SurfaceTexture].SetUniformsOnce(AProgram);
end;

{$endif read_implementation}
