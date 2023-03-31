{%MainUnit castleglutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

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
  Section = '----------------------------------------';

  function GetFloat2(const Param: TGLenum; const Available: Boolean): String;
  var
    V: packed array [0..1] of TGLFloat;
  begin
    if not Available then
      Exit('Not Available');
    glGetFloatv(Param, @V);
    Result := Format('%f %f', [V[0], V[1]]);
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
        '  Fglrx (ancient ATI drivers on Linux): %s' + NL +
        '  Mesa: %s', [
        Version.VendorMajor,
        Version.VendorMinor,
        Version.VendorRelease,
        PAnsiChar(glGetString(GL_VENDOR)),
        VendorTypeToStr(Version.VendorType),
        PAnsiChar(glGetString(GL_RENDERER)),
        BoolToStr(Version.Fglrx, true),
        BoolToStr(Version.Mesa, true)
      ]);
  end;

  function BugsReport(Version: TGLVersion): string;
  begin
    Result :=
      Format(
        '  Buggy VBO: %s' + NL +
        '  Buggy FBO rendering to multi-sampling texture: %s' + NL +
        '  Buggy FBO rendering to cube map texture: %s' + NL +
        '  Buggy swap buffers with non-standard glViewport: %s' + NL +
        '  Buggy 32-bit depth buffer: %s' + NL +
        '  Buggy GLSL gl_FrontFacing: %s' + NL +
        '  Buggy GLSL read varying: %s' + NL +
        '  Buggy Pure Shader Pipeline: %s' + NL +
        '  Buggy Texture Size Above 2048: %s' + NL +
        '  Buggy Shader Bump Mapping num_steps: %s', [
        BoolToStr(Version.BuggyVBO, true),
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

  function GetMaxTextureMaxAnisotropy: string;
  begin
    if GLFeatures.TextureFilterAnisotropic then
      Result := FloatToStrDot(GLFeatures.MaxTextureMaxAnisotropy)
    else
      Result := 'EXT_texture_filter_anisotropic not available';
  end;

  function GetOcclusionQueryCounterBits: string;
  begin
    if GLFeatures.OcclusionQuery then
      Result := IntToStr(GLFeatures.OcclusionQueryCounterBits)
    else
      Result := 'Occlusion query not available';
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
  Result :=
    {$ifdef OpenGLES} 'OpenGLES' {$else} 'OpenGL' {$endif} +
    ' information (detected by ' + ApplicationName + '):' + NL +
    NL +

    Section + NL +
    'Basic information:' + NL +
    NL +
    '  Context requested: ' + CapabilitiesStr[TGLFeatures.RequestCapabilities] + NL +
    NL +
    '  Version string: ' + PAnsiChar(glGetString(GL_VERSION)) + NL +
    VersionReport(GLVersion) + NL +
    VendorReport(GLVersion) + NL +
    NL +

    Section + NL +
    'Confirmed versions:' + NL +
    '(checks both version string and actual functions availability in the library, to secure from buggy implementations)' + NL +
    NL +
    {$ifdef OpenGLES}
    '  2_0: ' + BoolToStr(GLFeatures.VersionES_2_0, true) + NL +
    '  3.0: ' + BoolToStr(GLFeatures.VersionES_3_0, true) + NL +
    '  3.1: ' + BoolToStr(GLFeatures.VersionES_3_1, true) + NL +
    '  3.2: ' + BoolToStr(GLFeatures.VersionES_3_2, true) + NL +
    NL +

    {$else}
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

    Section + NL +
    'Features:' + NL +
    NL +
    '  Shaders (GLSL): ' + BoolToStr(GLFeatures.Shaders, true) + NL +
    '  Multi-texturing: ' + BoolToStr(GLFeatures.UseMultiTexturing, true) + NL +
    '  Framebuffer Object: ' + BoolToStr(GLFeatures.Framebuffer, true) + NL +
    '  Multi-sampling for FBO buffers and textures: ' + BoolToStr(GLFeatures.FBOMultiSampling, true) + NL +
    '  Vertex Buffer Object: ' + BoolToStr(GLFeatures.VertexBufferObject, true) + NL +
    '  Vertex Array Object: ' + BoolToStr(GLFeatures.VertexArrayObject, true) + NL +
    '  GenerateMipmap available (and reliable): ' + BoolToStr(HasGenerateMipmap, true) + NL +
    '  Cube map textures: ' + BoolToStr(GLFeatures.TextureCubeMap, true) + NL +
    '  Compressed textures supported: ' + TextureCompressionsToString(GLFeatures.TextureCompression) + NL +
    '  3D textures: ' + BoolToStr(GLFeatures.Texture3D, true) + NL +
    '  Textures non-power-of-2: ' + BoolToStr(GLFeatures.TextureNonPowerOfTwo, true) + NL +
    '  Blend constant parameter: ' + BoolToStr(GLFeatures.BlendConstant, true) + NL +
    '  Float textures: ' + BoolToStr(GLFeatures.TextureFloat, true) + NL +
    '  Depth textures: ' + BoolToStr(GLFeatures.TextureDepth, true) + NL +
    '  Shadow samplers (necessary for shadow maps): ' + BoolToStr(GLFeatures.TextureDepthCompare, true) + NL +
    '  Packed depth + stencil: ' + BoolToStr(GLFeatures.PackedDepthStencil, true) + NL +
    '  Texture Modern Formats + Swizzle: ' + BoolToStr(GLFeatures.TextureModernFormatsAndSwizzle, true) + NL +
    '  Shadow Volumes Possible (stencil buffer, two-sided stencil operations): ' + BoolToStr(GLFeatures.ShadowVolumesPossible, true) + NL +
    '  Occlusion Query: ' + BoolToStr(GLFeatures.OcclusionQuery, true) + NL +
    '  Texture Max Level: ' + BoolToStr(GLFeatures.TextureMaxLevel, true) + NL +
    NL +
    '  Necessary to use deprecated fixed-function pipeline: ' + BoolToStr(GLFeatures.EnableFixedFunction, true) + NL +
    NL +
    '  All extensions: ' + GLFeatures.AllExtensions + NL +
    NL +

    Section + NL +
    'Bugs:' + NL +
    NL +
    BugsReport(GLVersion) + NL +
    NL +

    Section + NL +
    'Current buffers bit depths:' + NL +
    NL +
    '  Color buffer (red, green, blue, alpha): '
      + IntToStr(GLFeatures.RedBits) +' / '
      + IntToStr(GLFeatures.GreenBits) +' / '
      + IntToStr(GLFeatures.BlueBits) +' / '
      + IntToStr(GLFeatures.AlphaBits) + NL +
    '  Depth: ' + IntToStr(GLFeatures.DepthBits) + NL +
    '  Stencil: ' + IntToStr(GLFeatures.StencilBits) + NL +
    '  Double buffer: ' + BoolToStr(GLFeatures.DoubleBuffer, true) + NL +
    '  Multi-sampling (full-screen anti-aliasing) possible: ' +
    BoolToStr(GLFeatures.Multisample, true) + NL +
    '    Current: ' + IntToStr(GLFeatures.CurrentMultiSampling) + ' samples per pixel' + NL +
    NL +

    Section + NL +
    'Limits:' + NL +
    NL +
    '  Max clip planes: ' + IntToStr(GLFeatures.MaxClipPlanes) + NL +
    '  Max lights (fixed-function limit): ' + IntToStr(GLFeatures.MaxLights) + NL +
    '  Max texture size: ' + IntToStr(GLFeatures.MaxTextureSize) + NL +
    '  Max viewport dims (width, height): ' +GLFeatures.MaxViewportDimensions.ToString + NL +
    '  Max texture units (shader pipeline): ' + IntToStr(GLFeatures.MaxTextureUnitsShaders) + NL +
    '  Max texture units (fixed-function): ' + IntToStr(GLFeatures.MaxTextureUnitsFixedFunction) + NL +
    '  Max cube map texture size: ' + IntToStr(GLFeatures.MaxCubeMapTextureSize) + NL +
    '  Max 3d texture size: ' + IntToStr(GLFeatures.MaxTexture3DSize) + NL +
    '  Max texture max anisotropy: ' + GetMaxTextureMaxAnisotropy + NL +
    '  Occlusion query counter bits: ' +  GetOcclusionQueryCounterBits + NL +
    '  Max renderbuffer size: ' + IntToStr(GLFeatures.MaxRenderbufferSize) + NL +
    '  Max line width: ' + GLFeatures.LineWidth.ToString + NL +
    NL +

    Section + NL +
    'Memory (in Kb):' + NL +
      NL +
    '  Total: ' + IntToStrUnknown(GLFeatures.Memory.Total) + NL +
    '  Current: ' + IntToStrUnknown(GLFeatures.Memory.Current) + NL +
    '  Current for VBO: ' + IntToStrUnknown(GLFeatures.Memory.CurrentVbo) + NL +
    '  Current for Textures: ' + IntToStrUnknown(GLFeatures.Memory.CurrentTexture) + NL +
    '  Current for Renderbuffers: ' + IntToStrUnknown(GLFeatures.Memory.CurrentRenderbuffer);

  CheckGLErrors('End of GLInformationString');
end;

function GLInformationStringSummary: String;
begin
  Result := Format('%s %d.%d (%s) (modern rendering: %s)', [
    {$ifdef OpenGLES} 'OpenGLES' {$else} 'OpenGL' {$endif},
    GLVersion.Major,
    GLVersion.Minor,
    VendorTypeToStr(GLVersion.VendorType),
    BoolToStr(not GLFeatures.EnableFixedFunction, true)
  ]);
end;

{$endif read_implementation}
