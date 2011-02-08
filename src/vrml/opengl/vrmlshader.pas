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

{ Setting up OpenGL shading (TVRMLShader).

  Internal for VRMLGLRenderer. @exclude }
unit VRMLShader;

interface

uses GLShaders, FGL;

type
  { Uniform value type, for TUniform. }
  TUniformType = (utLongInt, utSingle);

  { Uniform value to set after binding shader. }
  TUniform = class
    Name: string;
    AType: TUniformType;
    Value: record
      case Integer of
        utLongInt: (LongInt: LongInt);
        utSingle: (Single: Single);
    end;
  end;

  TTextureType = (tt2D, tt2DShadow, ttCubeMap, tt3D);

  TUniformsList = specialize TFPGObjectList<TUniform>;

  { Create appropriate shader and at the same time set OpenGL parameters
    for fixed-function rendering. Once everything is set up,
    you can use the @link(CreateProgram) to create and link a program
    (that you should then enable), or simply allow the fixed-function
    pipeline to work.

    This is used internally by TVRMLGLRenderer. It isn't supposed to be used
    directly by other code. }
  TVRMLShader = class
  private
    Uniforms: TUniformsList;
    TextureApply, TextureCoordPass, FragmentShaderDeclare: string;
  public
    destructor Destroy; override;
    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType);
    function CreateProgram: TGLSLProgram;
    procedure SetupUniforms(AProgram: TGLSLProgram);
  end;

implementation

uses SysUtils, GL, GLExt, KambiUtils, KambiStringUtils, KambiGLUtils;

{ TODO: a way to turn off using fixed-function pipeline completely
  will be needed some day.

  TODO: caching shader programs, using the same program if all settings
  are the same, will be needed some day. TShapeCache is not a good place
  for this, as the conditions for two shapes to share arrays/vbos
  are smaller/different (for example, two different geometry nodes
  can definitely share the same shader).

  Maybe caching should be done in this unit, or maybe in TVRMLGLRenderer
  in some TShapeShaderCache or such.

  TODO: a way to turn on/off per-pixel shading should be available.

  TODO: some day, avoid using predefined OpenGL state variables.
  Use only shader uniforms. Right now, we allow some state to be assigned
  using direct normal OpenGL fixed-function functions in VRMLGLRenderer,
  and our shaders just use it.
}

destructor TVRMLShader.Destroy;
begin
  FreeAndNil(Uniforms);
  inherited;
end;

procedure TVRMLShader.EnableTexture(const TextureUnit: Cardinal;
  const TextureType: TTextureType);
const
  OpenGLTextureType: array [TTextureType] of string =
  ('sampler2D', 'sampler2DShadow', 'samplerCube', 'sampler3D');
var
  Uniform: TUniform;
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  case TextureType of
    tt2D, tt2DShadow:
      begin
        glEnable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    ttCubeMap:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glEnable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    tt3D:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glEnable(GL_TEXTURE_3D_EXT);
      end;
    else raise EInternalError.Create('TextureEnableDisable?');
  end;

  { Enable for shader pipeline }

  Uniform := TUniform.Create;
  Uniform.Name := Format('texture_%d', [TextureUnit]);
  Uniform.AType := utLongInt;
  Uniform.Value.LongInt := TextureUnit;

  if Uniforms = nil then
    Uniforms := TUniformsList.Create;
  Uniforms.Add(Uniform);

  TextureCoordPass += Format('gl_TexCoord[%d] = gl_TextureMatrix[%0:d] * gl_MultiTexCoord%0:d;' + NL,
    [TextureUnit]);
  { TODO: always modulate mode for now
    TODO: other TextureType may require different gets than texture2D }
  TextureApply += Format('gl_FragColor *= texture2D(%s, gl_TexCoord[%d].st);' + NL,
    [Uniform.Name, TextureUnit]);
  FragmentShaderDeclare += Format('uniform %s %s;' + NL,
    [OpenGLTextureType[TextureType], Uniform.Name]);
end;

function TVRMLShader.CreateProgram: TGLSLProgram;

  procedure Replace(var S: string; const ParameterName, ParameterValue: string);
  begin
    StringReplaceAllTo1st(S, '/* *** ' + ParameterName + ' *** */', ParameterValue, false);
  end;

var
  FS, VS: string;
begin
  VS := {$I template.vs.inc};
  Replace(VS, 'TEXTURE-COORD-PASS', TextureCoordPass);

  FS := {$I template.fs.inc};
  Replace(FS, 'TEXTURE-APPLY', TextureApply);
  Replace(FS, 'FRAGMENT-SHADER-DECLARE', FragmentShaderDeclare);

  Result := TGLSLProgram.Create;
  try
    Result.AttachVertexShader(VS);
    Result.AttachFragmentShader(FS);
    Result.Link(true);
  except Result.Free; raise end;
end;

procedure TVRMLShader.SetupUniforms(AProgram: TGLSLProgram);
var
  I: Integer;
begin
  if Uniforms <> nil then
    for I := 0 to Uniforms.Count - 1 do
      case Uniforms[I].AType of
        utLongInt: AProgram.SetUniform(Uniforms[I].Name, Uniforms[I].Value.LongInt);
        utSingle : AProgram.SetUniform(Uniforms[I].Name, Uniforms[I].Value.Single );
        else raise EInternalError.Create('TVRMLShader.SetupUniforms:Uniforms[I].Type?');
      end;
end;

end.
