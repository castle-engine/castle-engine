{%MainUnit castlerendererinternalshader.pas}
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

{ Generating shader code for "plane mirror" feature,
  https://castle-engine.io/x3d_extensions_mirror_plane.php . }

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

  TSurfaceTextureShader = record
    Enable: boolean;
    TextureUnit, TextureCoordinatesId: Cardinal;
    UniformTextureName: String;
    PlugCode: String;
    procedure PrepareHash(var Hash: TShaderCodeHash;
      const SurfaceTexture: TSurfaceTexture);
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
  end;

  TSurfaceTextureShaderList = record
    Items: array [TSurfaceTexture] of TSurfaceTextureShader;
    procedure Clear;
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSurfaceTextureShader ------------------------------------------------------ }

procedure TSurfaceTextureShader.PrepareHash(var Hash: TShaderCodeHash;
  const SurfaceTexture: TSurfaceTexture);
var
  HashMultiplier: UInt32;
begin
  if Enable then
  begin
    HashMultiplier := 2063 * (1 + Ord(SurfaceTexture));
    Hash.AddInteger(HashMultiplier * (
      2069 * (1 + TextureUnit) +
      2081 * (1 + TextureCoordinatesId)
    ));
    { TODO: add Hash.AddString(PlugCode, 2083 * HashMultiplier);
      to account that PlugCode may change?
      But in reality it never changes, only in case of CommonSurfaceShader
      the ChannelMask is configurable. }
  end;
end;

procedure TSurfaceTextureShader.GenerateCode(const Shader: TShader);
begin
  if Enable then
  begin
    Shader.Plug(stFragment, SReplacePatterns(
      'uniform sampler2D {uniform_texture_name};' + NL +
      {$ifndef OpenGLES} // avoid redeclaring stuff when no "separate compilation units" (OpenGLES)
      'varying vec4 {coord_name};' + NL +
      '// Declare functions defined in tone_mapping.fs' + NL +
      'vec4 castle_texture_color_to_linear(const in vec4 srgbIn);' + NL +
      'vec3 castle_texture_color_to_linear(const in vec3 srgbIn);' + NL +
      {$endif}
      PlugCode,
      [
        '{uniform_texture_name}',
        '{coord_name}'
      ],
      [
        UniformTextureName,
        TTextureCoordinateShader.CoordName(TextureCoordinatesId)
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
