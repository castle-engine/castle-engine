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

const
  RenderingApiName =
    {$if defined(CASTLE_WEBGL)} 'WebGL'
    {$elseif defined(OpenGLES)} 'OpenGLES'
    {$else} 'OpenGL'
    {$endif};

function GLInformationString: string;
const
  Section = '----------------------------------------';

  function GetFloat2(const Param: TGLenum; const Available: Boolean): String;
  var
    V: TVector2;
  begin
    if not Available then
      Exit('Not Available');
    V := glGetFloat2(Param);
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
        '  Version in suffix, specific to vendor: major: %d, minor: %d, release: %d' + NL +
        NL +
        '  Vendor string: %s' + NL +
        '  Vendor type: %s' + NL +
        '  Special vendors:' + NL +
        '    Fglrx: %s' + NL +
        '    Mesa: %s' + NL +
        NL +
        '  Renderer: %s', [
        Version.VendorMajor,
        Version.VendorMinor,
        Version.VendorRelease,
        Version.Vendor,
        VendorTypeToStr(Version.VendorType),
        BoolToStr(Version.Fglrx, true),
        BoolToStr(Version.Mesa, true),
        Version.Renderer
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
        '  Buggy Texture Size Above 2048: %s' + NL +
        '  Buggy Shader Bump Mapping num_steps: %s', [
        BoolToStr(Version.BuggyVBO, true),
        BoolToStr(Version.BuggyFBOMultiSampling, true),
        BoolToStr(Version.BuggyFBOCubeMap, true),
        BoolToStr(Version.BuggySwapNonStandardViewport, true),
        BoolToStr(Version.BuggyDepth32, true),
        BoolToStr(Version.BuggyGLSLFrontFacing, true),
        BoolToStr(Version.BuggyGLSLReadVarying, true),
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

  function MemoryInfo(const Name: String; const ValueInKb: Integer): String;
  begin
    if ValueInKb = 0 then
      Result := ''
    else
      Result := Format('  %s: %d mb (%d kb)' + NL, [
        Name,
        ValueInKb div 1024,
        ValueInKb
      ]);
  end;

begin
  Result :=
    RenderingApiName +
    ' information (detected by ' + ApplicationName + '):' + NL +
    NL +

    Section + NL +
    'Basic information:' + NL +
    NL +
    '  Context requested: ' + CapabilitiesStr[TGLFeatures.RequestCapabilities] + NL +
    NL +
    '  Version string: ' + GLVersion.VersionString + NL +
    VersionReport(GLVersion) + NL +
    VendorReport(GLVersion) + NL +
    NL +

    Section + NL +
    'Determined versions:' + NL +
    NL +
    {$if defined(CASTLE_WEBGL)}
    '  2.0: ' + BoolToStr(GLFeatures.VersionWebGL_2, true) + NL +
    NL +

    {$elseif defined(OpenGLES)}
    '  2.0: ' + BoolToStr(GLFeatures.VersionES_2_0, true) + NL +
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
    '  Transform Feedback Varyings: ' + BoolToStr(GLFeatures.TransformFeedbackVaryings, true) + NL +
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

    {$ifdef CASTLE_WEBGL}
    Section + NL +
    'WebGL context attributes:' + NL +
    NL +
    '  Alpha: ' + BoolToStr(GL.getContextAttributes.alpha, true) + NL +
    '  Depth: ' + BoolToStr(GL.getContextAttributes.depth, true) + NL +
    '  Stencil: ' + BoolToStr(GL.getContextAttributes.stencil, true) + NL +
    '  Antialias: ' + BoolToStr(GL.getContextAttributes.antialias, true) + NL +
    '  PremultipliedAlpha: ' + BoolToStr(GL.getContextAttributes.premultipliedAlpha, true) + NL +
    '  PreserveDrawingBuffer: ' + BoolToStr(GL.getContextAttributes.preserveDrawingBuffer, true) + NL +
    '  PowerPreference: ' + String(GL.getContextAttributes.powerPreference) + NL +
    '  FailIfMajorPerformanceCaveat: ' + BoolToStr(GL.getContextAttributes.failIfMajorPerformanceCaveat, true) + NL +
    NL +
    {$endif}

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
    '  Max fragment uniform components: ' + IntToStr(GLFeatures.MaxFragmentUniformComponents) + NL +
    '  Max vertex uniform components: ' + IntToStr(GLFeatures.MaxVertexUniformComponents) + NL +
    '  Estimated maximum number of joints for skinning on GPU: ' + IntToStr(GLFeatures.MaxSkinnedJointsShaders) + NL +
    NL +

    Section + NL +
    'Memory:' + NL +
      NL +
    { Using wording from
      https://registry.khronos.org/OpenGL/extensions/NVX/NVX_gpu_memory_info.txt
      to be precise.
      See also how glxinfo displays it,
      https://stackoverflow.com/questions/75756085/linuxmesaopengl-measuring-amount-of-allocated-video-memory-of-an-application . }
    MemoryInfo('Dedicated video memory', GLFeatures.Memory.DedicatedVideoMemory) +
    MemoryInfo('Total available memory', GLFeatures.Memory.TotalAvailableMemory) +
    MemoryInfo('Currently available dedicated video memory', GLFeatures.Memory.CurrentAvailableVideoMemory) +
    MemoryInfo('VBO free memory', GLFeatures.Memory.VboFreeMemory) +
    MemoryInfo('Texture free memory', GLFeatures.Memory.TextureFreeMemory) +
    MemoryInfo('Renderbuffer free memory', GLFeatures.Memory.RenderbufferFreeMemory) +
    '';

  CheckGLErrors('End of GLInformationString');
end;

function GLInformationStringSummary: String;
var
  VendorStrOrNone: String;
begin
  { Show detected vendor type or nothing if we don't know vendor type.
    We show nothing when vendor type is gvOther, to not confuse the user
    ("other" just means we didn't have code to recognize this vendor). }
  if GLVersion.VendorType = gvOther then
    VendorStrOrNone := ''
  else
    VendorStrOrNone := ' (' + VendorTypeToStr(GLVersion.VendorType) + ')';

  Result := Format('%s %d.%d%s (modern rendering: %s)', [
    RenderingApiName,
    GLVersion.Major,
    GLVersion.Minor,
    VendorStrOrNone,
    BoolToStr(not GLFeatures.EnableFixedFunction, true)
  ]);
end;

{$endif read_implementation}
