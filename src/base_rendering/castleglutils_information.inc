{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: GLInformationString. }

{$ifdef read_interface}

{ Multiline string describing attributes of current OpenGL(ES)
  library. This simply queries OpenGL(ES) using glGet* functions
  about many things. Does not change OpenGL(ES) state in any way.

  Note that the last line of returned string does not terminate
  with a newline character (so e.g. you may want to do
  Writeln(GLInformationString) instead of just Write(GLInformationString)). }
function GLInformationString: String;

{ One-line string summarizing current OpenGL(ES). }
function GLInformationStringSummary: String;

{$endif read_interface}

{$ifdef read_implementation}

function GLInformationString: string;
const
  GLSupportNamesFBO: array [TGLSupport] of String =
  ( 'None', 'Extension', 'Standard (or ARB "core extension")' );

  function GetInteger(const Param: TGLenum): String;
  begin
    Result := IntToStr(glGetInteger(Param));
  end;

  { Unused for now:
  function GetInteger2(const Param: TGLenum; const Available: Boolean): String;
  var
    V: packed array [0..1] of TGLInt;
  begin
    if not Available then
      Exit('Not Available');
    glGetIntegerv(Param, @V);
    Result := Format('%d %d', [V[0], V[1]]);
  end;
  }

  function GetFloat2(const Param: TGLenum; const Available: Boolean): String;
  var
    V: packed array [0..1] of TGLFloat;
  begin
    if not Available then
      Exit('Not Available');
    glGetFloatv(Param, @V);
    Result := Format('%f %f', [V[0], V[1]]);
  end;

  function GetBoolean(const Param: TGLenum): String;
  begin
    Result := BoolToStr(glGetInteger(Param) = GL_TRUE, true);
  end;

  function VersionReport(Version: TGenericGLVersion): string;
  begin
    Result := Format(
      '  Version parsed: major: %d, minor: %d, release exists: %s, ' +
      'release: %d, vendor-specific information: "%s"', [
        Version.Major,
        Version.Minor,
        BoolToStr(Version.ReleaseExists, true),
        Version.Release,
        Version.VendorVersion
      ]);
  end;

  function VendorReport(Version: TGLVersion): string;
  begin
    Result :=
      Format(
        '  Vendor-specific version parsed: major: %d, minor: %d, release: %d' + NL +
        '  Vendor: %s' + NL +
        '  Vendor type: %s' + NL +
        NL +
        '  Renderer: %s' + NL +
        '  Fglrx (ATI on Linux): %s' + NL +
        '  Mesa: %s' + NL +
        NL +
        '  Buggy glGenerateMipmap(EXT): %s' + NL +
        '  Buggy GL_LIGHT_MODEL_TWO_SIDE: %s' + NL +
        '  Buggy VBO: %s' + NL +
        '  Buggy shader shadow map: %s' + NL +
        '  Buggy FBO rendering to multi-sampling texture: %s' + NL +
        '  Buggy FBO rendering to cube map texture: %s' + NL +
        '  Buggy swap buffers with non-standard glViewport: %s' + NL +
        '  Buggy 32-bit depth buffer: %s' + NL +
        '  Buggy GLSL gl_FrontFacing: %s' + NL +
        '  Buggy GLSL read varying: %s' + NL +
        '  Buggy Pure Shader Pipeline: %s' + NL +
        '  Buggy Texture Size Above 2048: %s' + NL +
        '  Buggy Shader Bump Mapping num_steps: %s',
        [ Version.VendorMajor, Version.VendorMinor, Version.VendorRelease,
          PAnsiChar(glGetString(GL_VENDOR)),
          VendorTypeToStr(Version.VendorType),
          PAnsiChar(glGetString(GL_RENDERER)),
          BoolToStr(Version.Fglrx, true),
          BoolToStr(Version.Mesa, true),

          BoolToStr(Version.BuggyGenerateMipmap, true),
          BoolToStr(Version.BuggyLightModelTwoSide, true),
          BoolToStr(Version.BuggyVBO, true),
          BoolToStr(Version.BuggyShaderShadowMap, true),
          BoolToStr(Version.BuggyFBOMultiSampling, true),
          BoolToStr(Version.BuggyFBOCubeMap, true),
          BoolToStr(Version.BuggySwapNonStandardViewport, true),
          BoolToStr(Version.BuggyDepth32, true),
          BoolToStr(Version.BuggyGLSLFrontFacing, true),
          BoolToStr(Version.BuggyGLSLReadVarying, true),
          BoolToStr(Version.BuggyPureShaderPipeline, true),
          BoolToStr(Version.BuggyTextureSizeAbove2048, true),
          BoolToStr(Version.BuggyGLSLBumpMappingNumSteps, true)
        ]);
  end;

  function GetMaxCubeMapTextureSize: string;
  begin
    if GLFeatures.TextureCubeMap <> gsNone then
      Result := IntToStr(GLFeatures.MaxCubeMapTextureSize) else
      Result := 'Cube maps not available';
  end;

  function GetMaxTexture3DSize: string;
  begin
    if GLFeatures.Texture3D <> gsNone then
      Result := IntToStr(GLFeatures.MaxTexture3DSize) else
      Result := '3D textures not available';
  end;

  function GetMaxTextureMaxAnisotropy: string;
  begin
    if GLFeatures.EXT_texture_filter_anisotropic then
      Result := FloatToStrDot(GLFeatures.MaxTextureMaxAnisotropyEXT)
    else
      Result := 'EXT_texture_filter_anisotropic not available';
  end;

  function GetQueryCounterBits: string;
  begin
    if GLFeatures.ARB_occlusion_query then
      Result := IntToStr(GLFeatures.QueryCounterBits) else
      Result := 'ARB_occlusion_query not available';
  end;

  function GetMaxRenderbufferSize: string;
  begin
    if GLFeatures.Framebuffer <> gsNone then
      Result := IntToStr(GLFeatures.MaxRenderbufferSize) else
      Result := 'Framebuffer not available';
  end;

  function TextureCompressionsToString(const Compressions: TTextureCompressions): string;
  var
    C: TTextureCompression;
  begin
    Result := '';
    for C := Low(C) to High(C) do
      if C in Compressions then
      begin
        if Result <> '' then Result := Result + ', ';
        Result := Result + TextureCompressionInfo[C].Name;
      end;
    Result := '[' + Result + ']';
  end;

  function IntToStrUnknown(const I: Integer): string;
  begin
    Result := IntToStr(I);
    if I = 0 then
      Result := Result + ' (unknown)';
  end;

begin
  Result:=
    {$ifdef OpenGLES} 'OpenGLES' {$else} 'OpenGL' {$endif} +
    ' information (detected by ' + ApplicationName +'):' + NL +
    NL +

    '--------' + NL +
    'Version:' + NL +
    '  Version string: ' + PAnsiChar(glGetString(GL_VERSION)) + NL +
    VersionReport(GLVersion) + NL +
    VendorReport(GLVersion) + NL +
    NL +

    {$ifdef OpenGLES}
    '------------------------' + NL +
    'Versions available:' + NL +
    NL +
    '  2_0: ' + BoolToStr(GLFeatures.VersionES_2_0, true) + NL +
    '  3.0: ' + BoolToStr(GLFeatures.VersionES_3_0, true) + NL +
    '  3.1: ' + BoolToStr(GLFeatures.VersionES_3_1, true) + NL +
    '  3.2: ' + BoolToStr(GLFeatures.VersionES_3_2, true) + NL +
    NL +

    {$else}
    '------------------------' + NL +
    'Real versions available:' + NL +
    '(checks both version string and actual functions availability in GL library, to secure from buggy implementations)' + NL +
    NL +
    '  1.2: ' + BoolToStr(GLFeatures.Version_1_2, true) + NL +
    '  1.3: ' + BoolToStr(GLFeatures.Version_1_3, true) + NL +
    '  1.4: ' + BoolToStr(GLFeatures.Version_1_4, true) + NL +
    '  1.5: ' + BoolToStr(GLFeatures.Version_1_5, true) + NL +
    '  2.0: ' + BoolToStr(GLFeatures.Version_2_0, true) + NL +
    '  2.1: ' + BoolToStr(GLFeatures.Version_2_1, true) + NL +
    '  3.0: ' + BoolToStr(GLFeatures.Version_3_0, true) + NL +
    '  3.1: ' + BoolToStr(GLFeatures.Version_3_1, true) + NL +
    '  3.2: ' + BoolToStr(GLFeatures.Version_3_2, true) + NL +
    '  3.3: ' + BoolToStr(GLFeatures.Version_3_3, true) + NL +
    '  4.0: ' + BoolToStr(GLFeatures.Version_4_0, true) + NL +
    NL +
    {$endif}

    '---------' + NL +
    'Features:' + NL +
    '  Shaders (GLSL) support: ' + GLSupportNames[GLFeatures.Shaders] + NL +
    '  => Enable deprecated (fixed-function) support: ' + BoolToStr(GLFeatures.EnableFixedFunction, true) + NL +
    '  Multi-texturing: ' + BoolToStr(GLFeatures.UseMultiTexturing, true) + NL +
    '  Framebuffer Object: ' + GLSupportNamesFBO[GLFeatures.Framebuffer] + NL +
    '  Multi-sampling for FBO buffers and textures: ' + BoolToStr(GLFeatures.FBOMultiSampling, true) + NL +
    '  Vertex Buffer Object: ' + BoolToStr(GLFeatures.VertexBufferObject, true) + NL +
    '  GenerateMipmap available (and reliable): ' + BoolToStr(HasGenerateMipmap, true) + NL +
    '  Cube map textures: ' + GLSupportNames[GLFeatures.TextureCubeMap] + NL +
    '  Compressed textures supported: ' + TextureCompressionsToString(GLFeatures.TextureCompression) + NL +
    '  3D textures: ' + GLSupportNames[GLFeatures.Texture3D] + NL +
    '  Textures non-power-of-2: ' + BoolToStr(GLFeatures.TextureNonPowerOfTwo, true) + NL +
    '  Blend constant parameter: ' + BoolToStr(GLFeatures.BlendConstant, true) + NL +
    '  Float textures: ' + BoolToStr(GLFeatures.TextureFloat, true) + NL +
    '  Depth textures: ' + BoolToStr(GLFeatures.TextureDepth, true) + NL +
    '  Packed depth + stencil: ' + BoolToStr(GLFeatures.PackedDepthStencil, true) + NL +
    NL +
    '  All extensions: ' +PAnsiChar(glGetString(GL_EXTENSIONS)) + NL +
    NL +

    '---------------------------' + NL +
    'Current buffers bit depths:' + NL +
    '  Color (red / green / blue / alpha): '
      +GetInteger(GL_RED_BITS) +' / '
      +GetInteger(GL_GREEN_BITS) +' / '
      +GetInteger(GL_BLUE_BITS) +' / '
      +GetInteger(GL_ALPHA_BITS) + NL +
    '  Depth: ' +GetInteger(GL_DEPTH_BITS) + NL +
    {$ifndef OpenGLES}
    '  Index: ' +GetInteger(GL_INDEX_BITS) + NL +
    {$endif}
    '  Stencil: ' +GetInteger(GL_STENCIL_BITS) + NL +
    {$ifndef OpenGLES}
    '  Accumulation (red / green / blue / alpha): '
      +GetInteger(GL_ACCUM_RED_BITS) +' / '
      +GetInteger(GL_ACCUM_GREEN_BITS) +' / '
      +GetInteger(GL_ACCUM_BLUE_BITS) +' / '
      +GetInteger(GL_ACCUM_ALPHA_BITS) + NL +
    '  Double buffer: ' + GetBoolean(GL_DOUBLEBUFFER) + NL +
    {$endif}
    '  Multi-sampling (full-screen anti-aliasing) possible: ' +
    BoolToStr(GLFeatures.Multisample, true) + NL +
    '    Current: ' + IntToStr(GLFeatures.CurrentMultiSampling) + ' samples per pixel' + NL +
    NL +

    {$ifndef OpenGLES}
    '-------------' + NL +
    'Stack depths:' + NL +
    '  Attributes: ' +GetInteger(GL_MAX_ATTRIB_STACK_DEPTH) + NL +
    '  Client attributes: ' +GetInteger(GL_MAX_CLIENT_ATTRIB_STACK_DEPTH) + NL +
    '  Modelview: ' +GetInteger(GL_MAX_MODELVIEW_STACK_DEPTH) + NL +
    '  Projection: ' +GetInteger(GL_MAX_PROJECTION_STACK_DEPTH) + NL +
    '  Texture: ' +GetInteger(GL_MAX_TEXTURE_STACK_DEPTH) + NL +
    '  Name: ' +GetInteger(GL_MAX_NAME_STACK_DEPTH) + NL +
    NL +
    {$endif}

    '-------' + NL +
    'Limits:' + NL +
    '  Max clip planes: ' + IntToStr(GLFeatures.MaxClipPlanes) + NL +
    '  Max lights (fixed-function limit): ' + IntToStr(GLFeatures.MaxLights) + NL +
    {$ifndef OpenGLES}
    '  Max eval order: ' +GetInteger(GL_MAX_EVAL_ORDER) + NL +
    '  Max list nesting: ' +GetInteger(GL_MAX_LIST_NESTING) + NL +
    '  Max pixel map table: ' +GetInteger(GL_MAX_PIXEL_MAP_TABLE) + NL +
    {$endif}
    '  Max texture size: ' + IntToStr(GLFeatures.MaxTextureSize) + NL +
    '  Max viewport dims (width, height): ' +GLFeatures.MaxViewportDimensions.ToString + NL +
    '  Max texture units (shader pipeline): ' + IntToStr(GLFeatures.MaxTextureUnitsShaders) + NL +
    '  Max texture units (fixed-function): ' + IntToStr(GLFeatures.MaxTextureUnitsFixedFunction) + NL +
    '  Max cube map texture size: ' + GetMaxCubeMapTextureSize + NL +
    '  Max 3d texture size: ' + GetMaxTexture3DSize + NL +
    '  Max texture max anisotropy: ' + GetMaxTextureMaxAnisotropy + NL +
    '  Query counter bits (for occlusion query): ' + { for occlusion query  GL_SAMPLES_PASSED_ARB }
      GetQueryCounterBits + NL +
    '  Max renderbuffer size: ' + GetMaxRenderbufferSize + NL +
    '  Max line width: ' + GetFloat2(GL_ALIASED_LINE_WIDTH_RANGE, {$ifdef OpenGLES} true {$else} GLFeatures.Version_2_0 {$endif}) + NL +
    NL +
    '-------' + NL +
    'Memory (in Kb):' + NL +
    '  Total: ' + IntToStrUnknown(GLFeatures.Memory.Total) + NL +
    '  Current: ' + IntToStrUnknown(GLFeatures.Memory.Current) + NL +
    '  Current for VBO: ' + IntToStrUnknown(GLFeatures.Memory.CurrentVbo) + NL +
    '  Current for Textures: ' + IntToStrUnknown(GLFeatures.Memory.CurrentTexture) + NL +
    '  Current for Renderbuffers: ' + IntToStrUnknown(GLFeatures.Memory.CurrentRenderbuffer);

  CheckGLErrors;
end;

function GLInformationStringSummary: String;
begin
  Result := Format('%s %d.%d', [
    {$ifdef OpenGLES} 'OpenGLES' {$else} 'OpenGL' {$endif},
    GLVersion.Major,
    GLVersion.Minor
  ]);
end;

{$endif read_implementation}
