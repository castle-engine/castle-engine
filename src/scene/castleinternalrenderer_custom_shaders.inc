{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
//type
  { Custom shaders, can be set on TMeshRenderer.CustomShaders to override
    the default shaders. }
  TCustomShaders = class
    Shader: array [{ alpha test} Boolean] of TX3DShaderProgramBase;
    constructor Create(const VertexCode, FragmentCode: String);
    destructor Destroy; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCustomShaders.Create(const VertexCode, FragmentCode: String);

  procedure DoInitialize(const VertexCode, FragmentCode: string);
  begin
    Shader[false] := TX3DShaderProgramBase.Create;
    Shader[false].AttachVertexShader(VertexCode);
    Shader[false].AttachFragmentShader(FragmentCode);
    Shader[false].Link;

    Shader[true] := TX3DShaderProgramBase.Create;
    Shader[true].AttachVertexShader('#define ALPHA_TEST' + NL + VertexCode);
    Shader[true].AttachFragmentShader('#define ALPHA_TEST' + NL + FragmentCode);
    Shader[true].Link;
  end;

begin
  try
    DoInitialize(VertexCode, FragmentCode);
  except
    on E: EGLSLError do
    begin
      FreeAndNil(Shader[false]);
      FreeAndNil(Shader[true]);

      WritelnWarning('Scene', 'Error compiling/linking GLSL shaders for shadow maps: %s',
        [E.Message]);

      DoInitialize({$I fallback.vs.inc}, {$I fallback.fs.inc});
    end;
  end;
end;

destructor TCustomShaders.Destroy;
begin
  FreeAndNil(Shader[false]);
  FreeAndNil(Shader[true]);
end;

{$endif read_implementation}