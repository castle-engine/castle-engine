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
  TUniformsList = specialize TFPGObjectList<TUniform>;

  TTextureType = (tt2D, tt2DShadow, ttCubeMap, tt3D);

  TTexGenPlane = (tgpEye, tgpObject, tgpSphere, tgpNormal, tgpReflection);
  TTexGenComponent = 0..3;

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
    TextureApply, TextureCoordInitialize,
      TextureCoordGen, TextureCoordMatrix, FragmentShaderDeclare: string;
  public
    destructor Destroy; override;

    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Component: TTexGenComponent; const Plane: TTexGenPlane);
    procedure DisableTexGen(const TextureUnit: Cardinal);

    function CreateProgram: TGLSLProgram;
    procedure SetupUniforms(AProgram: TGLSLProgram);
  end;

implementation

uses SysUtils, GL, GLExt, KambiUtils, KambiStringUtils, KambiGLUtils,
  VRMLErrors, KambiLog;

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
  TextureSampleCall: string;
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

  TextureCoordInitialize += Format('gl_TexCoord[%d] = gl_MultiTexCoord%0:d;' + NL,
    [TextureUnit]);
  TextureCoordMatrix += Format('gl_TexCoord[%d] = gl_TextureMatrix[%0:d] * gl_TexCoord[%0:d];' + NL,
    [TextureUnit]);
  { TODO: always modulate mode for now }
  case TextureType of
    tt2D      : TextureSampleCall := 'texture2D(%s, %s.st)';
    tt2DShadow: TextureSampleCall := 'shadow2DProj(%s, %s)';
    ttCubeMap : TextureSampleCall := 'textureCube(%s, %s.xyz)';
    { For 3D textures, remember we may get 4D tex coords
      through TextureCoordinate4D, so we have to use texture3DProj }
    tt3D      : TextureSampleCall := 'texture3DProj(%s, %s)';
    else raise EInternalError.Create('TVRMLShader.EnableTexture:TextureType?');
  end;
  TextureApply += Format('gl_FragColor *= ' + TextureSampleCall + ';' + NL,
    [Uniform.Name, 'gl_TexCoord[' + IntToStr(TextureUnit) + ']']);
  FragmentShaderDeclare += Format('uniform %s %s;' + NL,
    [OpenGLTextureType[TextureType], Uniform.Name]);
end;

procedure TVRMLShader.EnableTexGen(const TextureUnit: Cardinal;
  const Component: TTexGenComponent; const Plane: TTexGenPlane);
const
  PlaneComponentNames: array [TTexGenComponent] of char = ('S', 'T', 'R', 'Q');
  { Note: R changes to p ! }
  VectorComponentNames: array [TTexGenComponent] of char = ('s', 't', 'p', 'q');
var
  PlaneName, Source: string;
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  case Component of
    0: glEnable(GL_TEXTURE_GEN_S);
    1: glEnable(GL_TEXTURE_GEN_T);
    2: glEnable(GL_TEXTURE_GEN_R);
    3: glEnable(GL_TEXTURE_GEN_Q);
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Component?');
  end;

  { Enable for shader pipeline.
    See helpful info about simulating glTexGen in GLSL in:
    http://www.mail-archive.com/osg-users@lists.openscenegraph.org/msg14238.html }

  case Plane of
    tgpEye   : begin PlaneName := 'gl_EyePlane'   ; Source := 'vertex_eye'; end;
    tgpObject: begin PlaneName := 'gl_ObjectPlane'; Source := 'gl_Vertex' ; end;
    tgpSphere: begin VRMLWarning(vwIgnorable, '"Sphere" texture generation for shader pipeline not implemented yet'); Exit; end;
    tgpNormal: begin VRMLWarning(vwIgnorable, '"Normal" texture generation for shader pipeline not implemented yet'); Exit; end;
    tgpReflection: begin VRMLWarning(vwIgnorable, '"Reflection" texture generation for shader pipeline not implemented yet'); Exit; end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Plane?');
  end;

  TextureCoordGen += Format('gl_TexCoord[%d].%s = dot(%s, %s%s[%0:d]);' + NL,
    [TextureUnit, VectorComponentNames[Component],
     Source, PlaneName, PlaneComponentNames[Component]]);
end;

procedure TVRMLShader.DisableTexGen(const TextureUnit: Cardinal);
begin
  { Disable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glDisable(GL_TEXTURE_GEN_R);
  glDisable(GL_TEXTURE_GEN_Q);
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
  Replace(VS, 'TEXTURE-COORD-PASS', 
    TextureCoordInitialize + TextureCoordGen + TextureCoordMatrix);

  FS := {$I template.fs.inc};
  Replace(FS, 'TEXTURE-APPLY', TextureApply);
  Replace(FS, 'FRAGMENT-SHADER-DECLARE', FragmentShaderDeclare);

  if Log then
  begin
    WritelnLogMultiline('Generated GLSL vertex shader', VS);
    WritelnLogMultiline('Generated GLSL fragment shader', FS);
  end;

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
