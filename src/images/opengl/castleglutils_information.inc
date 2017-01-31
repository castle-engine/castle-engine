{
  Copyright 2001-2017 Michalis Kamburelis.

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

{ Multiline string describing attributes of current OpenGL
  library. This simply queries OpenGL using glGet* functions
  about many things. Does not change OpenGL state in any way.

  Note that the last line of returned string does not terminate
  with a newline character (so e.g. you may want to do
  Writeln(GLInformationString) instead of just Write(GLInformationString)). }
function GLInformationString: string;

{$endif read_interface}

{$ifdef read_implementation}

function GLInformationString: string;
const
  GLSupportNamesFBO: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard (or ARB "core extension")' );

  function GetInteger(param: TGLenum): string;
  begin
    Result := IntToStr(glGetInteger(param));
  end;

  function GetInteger2(param: TGLenum; const form: string): string;
  var
    v: packed array [0..1] of TGLint;
  begin
    glGetIntegerv(param, @v);
    result := Format(form, [v[0], v[1]]);
  end;

  function GetBoolean(param: TGLenum): string;
  begin
    Result := BoolToStr(glGetInteger(Param) = GL_TRUE, true);
  end;

  function VersionReport(Version: TGenericGLVersion): string;
  begin
    Result := Format('  Version parsed: major: %d, minor: %d, release exists: %s, ' +
      'release: %d, vendor-specific information: "%s"',
      [ Version.Major, Version.Minor, BoolToStr(Version.ReleaseExists, true),
        Version.Release, Version.VendorInfo ]);
  end;

  function VendorReport(Version: TGLVersion): string;
  begin
    Result :=
      Format(
        '  Vendor-specific version parsed: major: %d, minor: %d, release: %d' +nl+
        '  Vendor: %s' +nl+
        '  Vendor type: %s' +nl+
        nl+
        '  Renderer: %s' +nl+
        '  Fglrx (ATI on Linux): %s' +nl+
        '  Mesa: %s' +nl+
        nl+
        '  Buggy glGenerateMipmap(EXT): %s' +nl+
        '  Buggy GL_LIGHT_MODEL_TWO_SIDE: %s' +nl+
        '  Buggy VBO: %s' +nl+
        '  Buggy shader shadow map: %s' +nl+
        '  Buggy GLSL "const in gl_XxxParameters" declaration: %s' +nl+
        '  Buggy FBO rendering to multi-sampling texture: %s' +nl+
        '  Buggy FBO rendering to cube map texture: %s' +nl+
        '  Buggy swap buffers with non-standard glViewport: %s' +nl+
        '  Buggy 32-bit depth buffer: %s' +nl+
        '  Buggy GLSL gl_FrontFacing: %s' +nl+
        '  Buggy GLSL read varying: %s',
        [ Version.VendorMajor, Version.VendorMinor, Version.VendorRelease,
          PChar(glGetString(GL_VENDOR)),
          VendorTypeToStr(Version.VendorType),
          PChar(glGetString(GL_RENDERER)),
          BoolToStr(Version.Fglrx, true),
          BoolToStr(Version.Mesa, true),

          BoolToStr(Version.BuggyGenerateMipmap, true),
          BoolToStr(Version.BuggyLightModelTwoSide, true),
          BoolToStr(Version.BuggyVBO, true),
          BoolToStr(Version.BuggyShaderShadowMap, true),
          BoolToStr(Version.BuggyGLSLConstStruct, true),
          BoolToStr(Version.BuggyFBOMultiSampling, true),
          BoolToStr(Version.BuggyFBOCubeMap, true),
          BoolToStr(Version.BuggySwapNonStandardViewport, true),
          BoolToStr(Version.BuggyDepth32, true),
          BoolToStr(Version.BuggyGLSLFrontFacing, true),
          BoolToStr(Version.BuggyGLSLReadVarying, true)
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
      Result := FloatToStr(GLFeatures.MaxTextureMaxAnisotropyEXT) else
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
        if Result <> '' then Result += ', ';
        Result += TextureCompressionInfo[C].Name;
      end;
    Result := '[' + Result + ']';
  end;

begin
  Result:=
    'OpenGL information (detected by ' + ApplicationName +'):' +nl+
    nl+

    '--------' +nl+
    'Version:' +nl+
    '  Version string: ' +PChar(glGetString(GL_VERSION)) +nl+
    VersionReport(GLVersion) +nl+
    VendorReport(GLVersion) +nl+
    nl+

    {$ifndef OpenGLES}
    '------------------------' +nl+
    'Real versions available:' +nl+
    '(checks both version string and actual functions availability in GL library, to secure from buggy OpenGL implementations)' +nl+
    nl+
    '  1.2: ' + BoolToStr(GLFeatures.Version_1_2, true) +nl+
    '  1.3: ' + BoolToStr(GLFeatures.Version_1_3, true) +nl+
    '  1.4: ' + BoolToStr(GLFeatures.Version_1_4, true) +nl+
    '  1.5: ' + BoolToStr(GLFeatures.Version_1_5, true) +nl+
    '  2.0: ' + BoolToStr(GLFeatures.Version_2_0, true) +nl+
    '  2.1: ' + BoolToStr(GLFeatures.Version_2_1, true) +nl+
    '  3.0: ' + BoolToStr(GLFeatures.Version_3_0, true) +nl+
    '  3.1: ' + BoolToStr(GLFeatures.Version_3_1, true) +nl+
    '  3.2: ' + BoolToStr(GLFeatures.Version_3_2, true) +nl+
    '  3.3: ' + BoolToStr(GLFeatures.Version_3_3, true) +nl+
    '  4.0: ' + BoolToStr(GLFeatures.Version_4_0, true) +nl+
    nl+
    {$endif}

    '---------' +nl+
    'Features:' +nl+
    '  GLSL shaders support: ' + GLSupportNames[TGLSLProgram.ClassSupport] +nl+
    '  Multi-texturing: ' + BoolToStr(GLFeatures.UseMultiTexturing, true) +nl+
    '  Framebuffer Object: ' + GLSupportNamesFBO[GLFeatures.Framebuffer] +nl+
    '  Multi-sampling for FBO buffers and textures: ' + BoolToStr(GLFeatures.FBOMultiSampling, true) +nl+
    '  Vertex Buffer Object: ' + BoolToStr(GLFeatures.VertexBufferObject, true) +nl+
    '  GenerateMipmap available (and reliable): ' + BoolToStr(HasGenerateMipmap, true) +nl+
    '  Cube map textures: ' + GLSupportNames[GLFeatures.TextureCubeMap] +nl+
    '  Compressed textures supported: ' + TextureCompressionsToString(GLFeatures.TextureCompression) +nl+
    '  3D textures: ' + GLSupportNames[GLFeatures.Texture3D] +nl+
    '  Textures non-power-of-2: ' + BoolToStr(GLFeatures.TextureNonPowerOfTwo, true) +nl+
    '  Blend constant parameter: ' + BoolToStr(GLFeatures.BlendConstant, true) +nl+
    '  Float textures: ' + BoolToStr(GLFeatures.TextureFloat, true) +nl+
    '  Depth textures: ' + BoolToStr(GLFeatures.TextureDepth, true) +nl+
    '  Packed depth + stencil: ' + BoolToStr(GLFeatures.PackedDepthStencil, true) +nl+
    nl+
    '  All extensions: ' +PChar(glGetString(GL_EXTENSIONS)) +nl+
    nl+

    {$ifndef OpenGLES}
    '-----------------------------' +nl+
    'OpenGL utility (GLU) version:' +nl+
    '  Version string: ' +gluGetString(GLU_VERSION) +nl+
    VersionReport(GLUVersion) +nl+
    '  Extensions: '+gluGetString(GLU_EXTENSIONS) +nl+
    nl+
    {$endif}

    '---------------------------' +nl+
    'Current buffers bit depths:' +nl+
    '  Color (red / greeen / blue / alpha): '
      +GetInteger(GL_RED_BITS) +' / '
      +GetInteger(GL_GREEN_BITS) +' / '
      +GetInteger(GL_BLUE_BITS) +' / '
      +GetInteger(GL_ALPHA_BITS) +nl+
    '  Depth: ' +GetInteger(GL_DEPTH_BITS) +nl+
    {$ifndef OpenGLES}
    '  Index: ' +GetInteger(GL_INDEX_BITS) +nl+
    {$endif}
    '  Stencil: ' +GetInteger(GL_STENCIL_BITS) +nl+
    {$ifndef OpenGLES}
    '  Accumulation (red / greeen / blue / alpha): '
      +GetInteger(GL_ACCUM_RED_BITS) +' / '
      +GetInteger(GL_ACCUM_GREEN_BITS) +' / '
      +GetInteger(GL_ACCUM_BLUE_BITS) +' / '
      +GetInteger(GL_ACCUM_ALPHA_BITS) +nl+
    '  Double buffer: ' + GetBoolean(GL_DOUBLEBUFFER) +nl+
    {$endif}
    '  Multisampling (full-screen antialiasing): ' + BoolToStr(GLFeatures.Multisample, true) +nl+
    '    Current: ' + IntToStr(GLFeatures.CurrentMultiSampling) + ' samples per pixel' +nl+
    nl+

    {$ifndef OpenGLES}
    '-------------' +nl+
    'Stack depths:' +nl+
    '  Attributes: ' +GetInteger(GL_MAX_ATTRIB_STACK_DEPTH) +nl+
    '  Client attributes: ' +GetInteger(GL_MAX_CLIENT_ATTRIB_STACK_DEPTH) +nl+
    '  Modelview: ' +GetInteger(GL_MAX_MODELVIEW_STACK_DEPTH) +nl+
    '  Projection: ' +GetInteger(GL_MAX_PROJECTION_STACK_DEPTH) +nl+
    '  Texture: ' +GetInteger(GL_MAX_TEXTURE_STACK_DEPTH) +nl+
    '  Name: ' +GetInteger(GL_MAX_NAME_STACK_DEPTH) +nl+
    nl+
    {$endif}

    '-------' +nl+
    'Limits:' +nl+
    '  Max clip planes: ' + IntToStr(GLFeatures.MaxClipPlanes) +nl+
    '  Max lights: ' + IntToStr(GLFeatures.MaxLights) +nl+
    {$ifndef OpenGLES}
    '  Max eval order: ' +GetInteger(GL_MAX_EVAL_ORDER) +nl+
    '  Max list nesting: ' +GetInteger(GL_MAX_LIST_NESTING) +nl+
    '  Max pixel map table: ' +GetInteger(GL_MAX_PIXEL_MAP_TABLE) +nl+
    {$endif}
    '  Max texture size: ' + IntToStr(GLFeatures.MaxTextureSize) +nl+
    '  Max viewport dims: ' +GetInteger2(GL_MAX_VIEWPORT_DIMS, 'width %d / height %d') +nl+
    '  Max texture units: ' + IntToStr(GLFeatures.MaxTextureUnits) +nl+
    '  Max cube map texture size: ' + GetMaxCubeMapTextureSize +nl+
    '  Max 3d texture size: ' + GetMaxTexture3DSize +nl+
    '  Max texture max anisotropy: ' + GetMaxTextureMaxAnisotropy +nl+
    '  Query counter bits (for occlusion query): ' + { for occlusion query  GL_SAMPLES_PASSED_ARB }
      GetQueryCounterBits +nl+
    '  Max renderbuffer size: ' + GetMaxRenderbufferSize
    {$ifdef OpenGLES} +NL+
    '  Max line width: ' + GetInteger(GL_ALIASED_LINE_WIDTH_RANGE)
    {$endif};

   CheckGLErrors;
end;

{$endif read_implementation}
