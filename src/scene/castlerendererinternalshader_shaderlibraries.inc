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

{ Manage shader code libraries, for TEffectNode.SetShaderLibraries. }

{$ifdef read_interface}

//type
  TShaderLibrary = (slEyeWorldSpace);
  TShaderLibrarySet = set of TShaderLibrary;

  { Generating shader code for bump mapping.
    Follows castlerendererinternalshader_feature_api.md API conventions. }
  TShaderLibraries = record
  public
    Enabled: TShaderLibrarySet;
    procedure Clear;
    procedure EnableAndPrepareHash(const L: TShaderLibrary; var Hash: TShaderCodeHash);
    procedure GenerateCode(const Shader: TShader);
    procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram;
      const RenderingCamera: TRenderingCamera);
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TShaderLibraries.Clear;
begin
  Enabled := [];
end;

procedure TShaderLibraries.EnableAndPrepareHash(const L: TShaderLibrary; var Hash: TShaderCodeHash);
const
  LibraryHash: array [TShaderLibrary] of Integer = (
    1427 // slEyeWorldSpace
  );
begin
  if not (L in Enabled) then // if not already added by another Effect
  begin
    Include(Enabled, L);
    Hash.AddInteger(LibraryHash[L]);
  end;
end;

procedure TShaderLibraries.GenerateCode(const Shader: TShader);
const
  ShaderLibraryEyeWorldSpace = {$I shader_libraries_EyeWorldSpace.glsl.inc};
begin
  if slEyeWorldSpace in Enabled then
  begin
    { This shader library is linked with both vertex and fragment shaders,
      so we need to plug it into both. Other shader libraries may be different,
      may only augment fragment shader, or only vertex shader. }
    Shader.Plug(stFragment, ShaderLibraryEyeWorldSpace);
    Shader.Plug(stVertex, ShaderLibraryEyeWorldSpace);
  end;
end;

procedure TShaderLibraries.SetDynamicUniforms(const AProgram: TX3DShaderProgram;
  const RenderingCamera: TRenderingCamera);
begin
  if slEyeWorldSpace in Enabled then
  begin
    { castle_CameraInversMatrix was already handled in TShader.SetDynamicUniforms.
      Pass remaining 3 matrices needed by GLSL in EyeWorldSpace.glsl . }
    AProgram.SetUniform('castle_CameraMatrix', RenderingCamera.Matrix);
    AProgram.SetUniform('castle_CameraRotationMatrix', RenderingCamera.RotationMatrix3);
    RenderingCamera.RotationInverseMatrixNeeded;
    AProgram.SetUniform('castle_CameraRotationInverseMatrix', RenderingCamera.RotationInverseMatrix3);
  end;
end;

{$endif read_implementation}
