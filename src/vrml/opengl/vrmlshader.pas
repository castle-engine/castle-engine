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

  TTexGenerationComponent = (tgEye, tgObject);
  TTexGenerationComplete = (tgSphere, tgNormal, tgReflection);
  TTexComponent = 0..3;

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
      TextureCoordGen, TextureCoordMatrix, FragmentShaderDeclare,
      ClipPlane: string;
  public
    destructor Destroy; override;

    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType; const ShadowMapSize: Cardinal = 0);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComponent; const Component: TTexComponent);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComplete);
    procedure DisableTexGen(const TextureUnit: Cardinal);
    procedure EnableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure DisableClipPlane(const ClipPlaneIndex: Cardinal);

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
  const TextureType: TTextureType; const ShadowMapSize: Cardinal);
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
    tt2DShadow: TextureSampleCall := 'shadow(%s, %s)';
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

  if TextureType = tt2DShadow then
  begin
    FragmentShaderDeclare += Format('#define SHADOW_MAP_SIZE %d' + NL,
      [ShadowMapSize]);
  end;
end;

procedure TVRMLShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComplete);
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  { glEnable(GL_TEXTURE_GEN_*) below }

  { Enable for shader pipeline }
  case Generation of
    tgSphere:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        TextureCoordGen += Format(
          { Sphere mapping in GLSL adapted from
            http://www.ozone3d.net/tutorials/glsl_texturing_p04.php#part_41
            by Jerome Guinot aka 'JeGX', many thanks! }
          'vec3 r = reflect( normalize(vec3(vertex_eye)), normal_eye );' + NL +
	  'float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );' + NL +
          '/* Using 1.0 / 2.0 instead of 0.5 to workaround fglrx bugs */' + NL +
	  'gl_TexCoord[%d].st = r.xy / m + vec2(1.0, 1.0) / 2.0;',
          [TextureUnit]);
      end;
    tgNormal:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = normal_eye;' + NL,
          [TextureUnit]);
      end;
    tgReflection:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        { Negate reflect result --- just like for kambi_vrml_test_suite/x3d/water_reflections/water_reflections_normalmap.fs }
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = -reflect(-vec3(vertex_eye), normal_eye);' + NL,
          [TextureUnit]);
      end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Generation?');
  end;
end;

procedure TVRMLShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComponent; const Component: TTexComponent);
const
  PlaneComponentNames: array [TTexComponent] of char = ('S', 'T', 'R', 'Q');
  { Note: R changes to p ! }
  VectorComponentNames: array [TTexComponent] of char = ('s', 't', 'p', 'q');
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

  case Generation of
    tgEye   : begin PlaneName := 'gl_EyePlane'   ; Source := 'vertex_eye'; end;
    tgObject: begin PlaneName := 'gl_ObjectPlane'; Source := 'gl_Vertex' ; end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Generation?');
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

procedure TVRMLShader.EnableClipPlane(const ClipPlaneIndex: Cardinal);
begin
  glEnable(GL_CLIP_PLANE0 + ClipPlaneIndex);
  if ClipPlane = '' then
    ClipPlane := 'gl_ClipVertex = vertex_eye;';
end;

procedure TVRMLShader.DisableClipPlane(const ClipPlaneIndex: Cardinal);
begin
  glDisable(GL_CLIP_PLANE0 + ClipPlaneIndex);
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
  Replace(VS, 'VERTEX-PROCESSING', TextureCoordInitialize + TextureCoordGen
    + TextureCoordMatrix + ClipPlane);

  FS := {$I template.fs.inc};
  Replace(FS, 'TEXTURE-APPLY', TextureApply);
  Replace(FS, 'FRAGMENT-SHADER-DECLARE',
    FragmentShaderDeclare + {$I shadow_map_common.fs.inc});

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
