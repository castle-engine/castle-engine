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

{ Generating shader code for shadow maps. }

{$ifdef read_interface}

//type
  { Generating shadow map shader code.

    Initially, TShader.EnableShadowMap only initialized this and calls PrepareHash.

    Later, once we know we need to generate new code,
    and once light shader code is fully known
    (e.g. we know lighting model, physical or Phong) -> then GenerateCode is called.

    Uniforms are set by SetUniforms (once, after linking)
    and SetDynamicUniforms (each frame). }
  TShadowMapShader = record
  strict private
    function ShadowMapName: String;
    function ProjectorMatrixName: String;
  public
    TextureUnit: Integer;
    ShadowMap: TGeneratedShadowMapNode;
    procedure PrepareHash(var Hash: TShaderCodeHash);
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
    procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram;
      const RenderingCamera: TRenderingCamera);
  end;

  { List of TShadowMapShader, containing all shadow maps used
    when rendering one shape.
    The methods here just call corresponding TShadowMapShader methods
    on all children. }
  TShadowMapShaderList = class({$ifdef FPC}specialize{$endif} TStructList<TShadowMapShader>)
  public
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
    procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram;
      const RenderingCamera: TRenderingCamera);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TShadowMapShader ----------------------------------------------------------- }

procedure TShadowMapShader.PrepareHash(var Hash: TShaderCodeHash);
begin
  Hash.AddInteger(6353 * TextureUnit);
  Hash.AddPointer(ShadowMap);
end;

function TShadowMapShader.ShadowMapName: String;
begin
  Result := 'castle_shadow_map_' + IntToStr(TextureUnit);
end;

function TShadowMapShader.ProjectorMatrixName: String;
begin
  Result := 'castle_projector_matrix_' + IntToStr(TextureUnit);
end;

procedure TShadowMapShader.GenerateCode(const Shader: TShader);
var
  ProjectedTexCoordName: String;
  SamplerType: String;
  ShadowLightShader: TLightShader;
begin
  // we pass here only TGeneratedShadowMapNode with light node set
  Assert(ShadowMap.Light <> nil);

  if not Shader.LightShaders.Find(ShadowMap.Light, ShadowLightShader) then
    Exit;

  { For variance shadow maps, use normal sampler2D, not sampler2DShadow }
  if Shader.ShadowSampling = ssVarianceShadowMaps then
    SamplerType := 'sampler2D'
  else
    SamplerType := 'sampler2DShadow';

  ProjectedTexCoordName := 'castle_projected_tex_coord_' + IntToStr(TextureUnit);

  { Vertex shader calculates varying vec4:
    current vertex projected into the light node coordinate system.  }
  Shader.Plug(stVertex,
    Format('uniform mat4 %s;', [ProjectorMatrixName]) + NL +
    Format('varying vec4 %s;', [ProjectedTexCoordName]) + NL +
    'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' + NL +
    '{' + NL +
    Format('  %s = %s * vertex_eye;', [ProjectedTexCoordName, ProjectorMatrixName]) + NL +
    '}' + NL);

  { Fragment shader uses this varying vec4 to calculate shadow,
    when given light is applied. }
  Shader.Plug(stFragment,
    Format('uniform %s %s;', [SamplerType, ShadowMapName]) + NL +
    Format('varying vec4 %s;', [ProjectedTexCoordName]) + NL +

    // Do not redeclare shadow() and friends on OpenGLES
    {$ifndef OpenGLES}
    Shader.DeclareShadowFunctions + NL +
    {$endif}

    'void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)' + NL +
    '{' +NL+
    Format(
      '  scale *= shadow(%s, %s, %d.0);', [
        ShadowMapName,
        ProjectedTexCoordName,
        ShadowMap.Size
      ]
    ) + NL +
    '}',
    ShadowLightShader.Code);
end;

procedure TShadowMapShader.SetUniformsOnce(const AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(ShadowMapName, TextureUnit);
end;

procedure TShadowMapShader.SetDynamicUniforms(const AProgram: TX3DShaderProgram;
  const RenderingCamera: TRenderingCamera);
const
  ProjectorScalingMatrix: TMatrix4 = (Data:
  ( (0.5,   0,   0, 0),
    (  0, 0.5,   0, 0),
    (  0,   0, 0.5, 0),
    (0.5, 0.5, 0.5, 1) ));
var
  ProjectorMatrix: TMatrix4;
begin
  RenderingCamera.InverseMatrixNeeded;
  ProjectorMatrix := (ProjectorScalingMatrix *
    ShadowMap.Light.GetProjectorMatrix) *
    RenderingCamera.InverseMatrix;
  AProgram.SetUniform(ProjectorMatrixName, ProjectorMatrix);
end;

{ TShadowMapShaderList ----------------------------------------------------------- }

procedure TShadowMapShaderList.GenerateCode(const Shader: TShader);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I].GenerateCode(Shader);
end;

procedure TShadowMapShaderList.SetUniformsOnce(const AProgram: TX3DShaderProgram);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I].SetUniformsOnce(AProgram);
end;

procedure TShadowMapShaderList.SetDynamicUniforms(const AProgram: TX3DShaderProgram;
  const RenderingCamera: TRenderingCamera);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I].SetDynamicUniforms(AProgram, RenderingCamera);
end;

{$endif read_implementation}
