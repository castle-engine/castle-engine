{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating shader code for skin animation on GPU. }

{$ifdef read_interface}

//type
  { Generating shader code for skin animation on GPU.
    Follows castlerendererinternalshader_feature_api.md API conventions. }
  TSkinShader = record
  public
    Enabled: Boolean;
    { Texture unit containing skin joint matrices.
      -1 if not used, then pass joint matrices in uniforms.
      Set this before EnableAndPrepareHash. }
    JointTextureUnit: Integer;
    JointMatrix: TMatrix4List;
    procedure Clear;
    procedure EnableAndPrepareHash(var Hash: TShaderCodeHash);
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
    procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram);
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TSkinShader.Clear;
begin
  Enabled := false;
end;

procedure TSkinShader.EnableAndPrepareHash(var Hash: TShaderCodeHash);
begin
  if not Enabled then
  begin
    Enabled := true;
    Hash.AddInteger(5683);
    // make every possible value, even -1, change hash
    Hash.AddInteger(5689 * (JointTextureUnit + 2));
  end;
end;

procedure TSkinShader.GenerateCode(const Shader: TShader);
const
  ShaderSkinAnimation = {$I skin_animation.vs.inc};
var
  SavedWarnMissingPlugs: Boolean;
  Defines: String;
begin
  if Enabled then
  begin
    SavedWarnMissingPlugs := Shader.WarnMissingPlugs;
    { Do not warn that PLUG_tangent_object_space is missing, which is normal
      when we do skinned animation on a shape without normal map.
      Testcase: woman from animate_bones_by_code_standalone.dpr . }
    Shader.WarnMissingPlugs := false;

    if JointTextureUnit >= 0 then
      Defines := '#define CASTLE_JOINT_TEXTURE' + NL
    else
      Defines := Format('#define CASTLE_MAX_SKIN_JOINTS %d', [
        GLFeatures.MaxSkinJointsInUniforms
      ]) + NL;

    Shader.Plug(stVertex, Defines + ShaderSkinAnimation);

    Shader.WarnMissingPlugs := SavedWarnMissingPlugs;
  end;
end;

procedure TSkinShader.SetUniformsOnce(const AProgram: TX3DShaderProgram);
begin
  if Enabled and (JointTextureUnit >= 0) then
    AProgram.SetUniform('castle_JointTexture', JointTextureUnit);
end;

procedure TSkinShader.SetDynamicUniforms(const AProgram: TX3DShaderProgram);
begin
  if Enabled and (JointTextureUnit = -1) then
  begin
    // WritelnLog('Passing to shader castle_JointMatrix with count %d', [JointMatrix.Count]);
    AProgram.SetUniform('castle_JointMatrix', JointMatrix);
  end;
end;

{$endif read_implementation}
