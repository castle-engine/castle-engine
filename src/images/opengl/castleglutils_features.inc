{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: TGLFeatures and friends. }

{$ifdef read_interface}

type
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

type
  { OpenGL memory information. }
  TGLMemoryInfo = class
  strict private
    ExtensionNVidia, ExtensionATI: boolean;
  public
    { Total and current memory available, in Kb.
      May be zero if unknown (only some NVidia GPUs support this). }
    Total, Current: TGLint;

    { Current memory free for a particular feature, in Kb.
      May be zero if unknown (only some AMD GPUs support this). }
    CurrentVbo, CurrentTexture, CurrentRenderbuffer: TGLint;

    constructor Create;
    procedure Refresh;

    { Detect GPUs with low texture memory.
      Looks at @link(Current) or @link(CurrentTexture), whichever is available.

      This is useful to detect at runtime when to use e.g. compressed textures
      (S3TC and such) or downscaled textures (using @link(GLTextureScale) or
      @link(TextureLoadingScale)). }
    function LessTextureMemoryThan(const RequiredMemoryKb: Integer): boolean;
  end;

  { OpenGL features, analyzed based on OpenGL extensions and version.
    A single instance of this class is assigned to @link(GLFeatures)
    after the first OpenGL context was created.

    If you have multiple OpenGL contexts, our engine assumes they share resources
    and have equal features. }
  TGLFeatures = class
  public
    {$ifndef OpenGLES}
    { OpenGL versions supported. Checked by looking at GL version string
      @italic(and) by checking whether actual entry points are available.

      This is important because bad OpenGL implementations
      (like the horrible ATI Linux closed drivers) sometimes lie,
      claiming support in the GL version string but actually lacking proper
      function entry points.
      We check the actual presence of entry points (GLExt.Load_GL_version_x_x
      do that).

      We *also* check version string (TGLVersion.AtLeast),
      since OpenGL may have some buggy entry point in case of work-in-progress
      features (not yet advertised in GL version string),
      e.g. Mesa 6.x had such buggy glStencilOpSeparate. This is correct OpenGL
      behavior AFAIK, and we handle it. }
    Version_1_2: boolean;
    Version_1_3: boolean;
    Version_1_4: boolean;
    Version_1_5: boolean;
    Version_2_0: boolean;
    Version_2_1: boolean;
    Version_3_0: boolean;
    Version_3_1: boolean;
    Version_3_2: boolean;
    Version_3_3: boolean;
    Version_4_0: boolean;
    {$endif}

    { Is the extension below loaded.
      Note: we prefer to avoid exposing directly each extension presence
      like below.
      Instead most interesting extensions are wrapped in "features"
      like @link(UseMultiTexturing), see lower. }
    ARB_shadow: boolean;
    EXT_texture_filter_anisotropic: boolean;
    NV_multisample_filter_hint: boolean;
    ARB_occlusion_query: boolean;
    EXT_framebuffer_blit: Boolean;
    {$ifndef OpenGLES}
    EXT_fog_coord: boolean;
    ARB_window_pos: boolean;
    MESA_window_pos: boolean;
    {$endif}

    { GL_CLAMP_TO_EDGE, if available in current OpenGL version.
      Otherwise GL_CLAMP.

      Use this (insteaf of direct GL_CLAMP_TO_EDGE)
      to work with @italic(really ancient) OpenGL versions before 1.2.
      Note that our engine officially supports only OpenGL >= 1.2,
      so don't expect everything to work smootly with such ancient OpenGL anyway! }
    CLAMP_TO_EDGE: TGLenum;

    { Constant (for given context) OpenGL limits.
      Initialized to 0 if appropriate OpenGL functionality is not available.
      @groupBegin }
    MaxTextureSize: Cardinal;
    MaxLights: Cardinal;
    MaxCubeMapTextureSize: Cardinal;
    MaxTexture3DSize: Cardinal;
    MaxTextureMaxAnisotropyEXT: Single;
    QueryCounterBits: TGLint;
    MaxRenderbufferSize: TGLuint;
    MaxClipPlanes: Cardinal;
    { @groupEnd }

    Multisample: boolean;

    { Number of texture units available for fixed-function pipeline.
      Equal to 0 on OpenGLES (that doesn't have fixed-function pipeline).
      Equal to glGetInteger(GL_MAX_TEXTURE_UNITS), if multi-texturing
      available. Equal to 1 (OpenGL supports always 1 texture) otherwise. }
    MaxTextureUnitsFixedFunction: Cardinal;

    { Number of texture units available for shader pipeline. }
    MaxTextureUnitsShaders: Cardinal;

    { Are all OpenGL multi-texturing extensions for
      VRML/X3D MultiTexture support available.

      This used to check a couple of multitexturing extensions,
      like ARB_multitexture. Right now, it simply checks for OpenGL 1.3 version.
      It is supported by virtually all existing GPUs.
      So it's acceptable to just check it, and write your code for 1.3,
      and eventual fallback code (when this is false) write only for really
      ancient GPUs. }
    UseMultiTexturing: boolean;

    { Are 3D textures supported by OpenGL.
      If they are, note that GL_TEXTURE_3D and GL_TEXTURE_3D_EXT are equal,
      so often both Texture3D = gsStandard and Texture3D = gsExtension
      cases may be handled by the same code. }
    Texture3D: TGLSupport;

    { Is Framebuffer supported. Value gsExtension means that EXT_framebuffer_object
      is used, gsStandard means that ARB_framebuffer_object (which is
      a "core extesion", present the same way in OpenGL 3 core,
      also in OpenGL ES >= 2.0 core) is available. }
    Framebuffer: TGLSupport;

    { Is multisampling possible for FBO buffers and textures.
      Although these are two orthogonal features of OpenGL,
      in practice you want to use multisample for both FBO buffers and textures,
      or for none --- otherwise, FBO can not be initialized correctly
      when you mix various multisample settings. }
    FBOMultiSampling: boolean;

    { How multi-sampling was initialized for this OpenGL context.
      Value = 1 means that no multi-sampling is initialized.
      Values > 1 mean that you have multi-sampling, with given number of samples
      per pixel.
      Contrast this with TCastleWindowBase.MultiSampling or TOpenGLControl.MultiSampling,
      that say @italic(how many samples you wanted to get). }
    CurrentMultiSampling: Cardinal;

    { Does OpenGL context have depth buffer packed with stencil buffer.
      See OpenGL extensions EXT_packed_depth_stencil and OES_packed_depth_stencil
      (http://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt,
      https://www.khronos.org/registry/OpenGL/extensions/OES/OES_packed_depth_stencil.txt)
      for explanation what does it mean.

      This is important for FBOs, as the depth/stencil have to be set up differently
      depending on PackedDepthStencil value.
      This is also important for all code using TGLRenderToTexture
      with TGLRenderToTexture.Buffer equal tbDepth or tbColorAndDepth:
      your depth texture must be prepared differently, to include both depth+stencil
      data, to work.

      For now, this is simply equal to GL_EXT_packed_depth_stencil.
      (TODO: for core OpenGL 3, how to detect should we use packed version?
      http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt
      incorporates EXT_packed_depth_stencil, so forward-compatible contexts
      do not need to declare it.
      Should we assume that forward-compatible gl 3 contexts always have
      depth/stencil packed?) }
    PackedDepthStencil: boolean;

    { Does OpenGL context support shadow volumes.
      This simply checks do we have stencil buffer with at least 4 bits for now. }
    ShadowVolumesPossible: boolean;

    { Are non-power-of-2 textures supported. }
    TextureNonPowerOfTwo: boolean;

    { Are cubemaps supported.

      gsExtension means GL_ARB_texture_cube_map on core OpenGL.
      gsStandard means standard feature of OpenGL or OpenGL ES.
      Since the constants defined by ARB_texture_cube_map were promoted
      to core with the same values, the distinction between gsExtension
      and gsStandard in practice doesn't exist. }
    TextureCubeMap: TGLSupport;

    { Which texture compression formats are supported. }
    TextureCompression: TTextureCompressions;

    { VBO support (in OpenGL (ES) core). }
    VertexBufferObject: boolean;

    { glBlendColor and GL_CONSTANT_ALPHA support. }
    BlendConstant: boolean;

    { Support for float texture formats for glTexImage2d. }
    TextureFloat: boolean;

    { Support for depth texture formats for glTexImage2d. }
    TextureDepth: boolean;

    { GLSL shaders support. }
    Shaders: TGLSupport;

    { Enable OpenGL fixed-function rendering on desktops.

      @unorderedList(
        @item(This makes Gouraud-shaded shapes rendered using OpenGL
          fixed-function pipeline on desktops.)
        @item(It also makes TDrawableImage.Draw, DrawPrimitive2D (and consequently
          most of the 2D user-interface) rendered using OpenGL fixed-function pipeline
          on desktops.)
        @item(It also allows you to use fixed-function pipeline calls in the
          @link(TCastleWindowBase.OnRender) callback, @link(TCastleUserInterface.Render)
          override and related places.)
        @item(It also allows some deprecated rendering features like WindowPos
          to continue to work.)
      )

      This is @false by default on modern GPUs (that have good GLSL support).

      On OpenGLES, it must be @false always, since OpenGLES >= 2
      has no fixed-function pipeline.
    }
    EnableFixedFunction: boolean;

    { Total and current memory available.
      Call Memory.Refresh whenever needed. }
    Memory: TGLMemoryInfo;

    MaxViewportDimensions: TVector2Integer;

    { Maximum number of texture units you can safely always use.
      If EnableFixedFunction, this is MaxTextureUnitsFixedFunction,
      otherwise MaxTextureUnitsShaders. }
    function MaxTextureUnits: Cardinal;

    constructor Create;
    destructor Destroy; override;
  end;

{ Initialize GLVersion and GLUVersion and GLFeatures. }
procedure GLInformationInitialize;

var
  GLFeatures: TGLFeatures;

{$endif read_interface}

{$ifdef read_implementation}

procedure GLInformationInitialize;
begin
  FreeAndNil(GLVersion);
  GLVersion := TGLVersion.Create(PChar(glGetString(GL_VERSION)),
    PChar(glGetString(GL_VENDOR)), PChar(glGetString(GL_RENDERER)));

  {$ifndef OpenGLES}
  {$ifdef CASTLE_OBJFPC}
  {$warnings off} // consciously initializing deprecated stuff, to keep it working
  FreeAndNil(GLUVersion);
  { gluGetString is valid for version 1.1 or later }
  if Assigned(gluGetString) then
    GLUVersion := TGenericGLVersion.Create(gluGetString(GLU_VERSION))
  else
    GLUVersion := TGenericGLVersion.Create('1.0');
  {$warnings on}
  {$endif CASTLE_OBJFPC}
  {$endif}

  FreeAndNil(GLFeatures);
  GLFeatures := TGLFeatures.Create;
end;

{$ifdef OpenGLES}
{ Based on GLExt unit in FPC. This function is missing from GLES header,
  which does not check for extensions presence at all. }
function glext_ExtensionSupported(const extension: String;
  const searchIn: String): Boolean;
var
  extensions: PChar;
  start: PChar;
  where, terminator: PChar;
begin
  if (Pos(' ', extension) <> 0) or (extension = '') then
  begin
    Result := FALSE;
    Exit;
  end;

  if searchIn = '' then extensions := PChar(glGetString(GL_EXTENSIONS))
  else extensions := PChar(searchIn);
  start := extensions;
  while TRUE do
  begin
    where := StrPos(start, PChar(extension));
    if where = nil then Break;
    terminator := Pointer(PtrUInt(where) + PtrUInt(Length(extension)));
    {$warnings off} { Stop warning about unportable PtrUInt conversions }
    if (where = start) or (PChar(Pointer(PtrUInt(where) - PtrUInt(1)))^ = ' ') then
    {$warnings on}
    begin
      if (terminator^ = ' ') or (terminator^ = #0) then
      begin
        Result := TRUE;
        Exit;
      end;
    end;
    start := terminator;
  end;
  Result := FALSE;
end;
{$endif}

constructor TGLFeatures.Create;

  function GetVector2Integer(const Param: TGLenum): TVector2Integer;
  var
    V: packed array [0..1] of TGLInt;
  begin
    glGetIntegerv(Param, @V);
    Result.Data[0] := V[0];
    Result.Data[1] := V[1];
  end;

var
  SupportedExtensions: string;
begin
  inherited;

  {$ifndef OpenGLES}
  Version_1_2 := GLVersion.AtLeast(1, 2) and Load_GL_version_1_2;
  Version_1_3 := GLVersion.AtLeast(1, 3) and Load_GL_version_1_3;
  Version_1_4 := GLVersion.AtLeast(1, 4) and Load_GL_version_1_4;
  Version_1_5 := GLVersion.AtLeast(1, 5) and Load_GL_version_1_5;
  Version_2_0 := GLVersion.AtLeast(2, 0) and Load_GL_version_2_0;
  Version_2_1 := GLVersion.AtLeast(2, 1) and Load_GL_version_2_1;
  Version_3_0 := GLVersion.AtLeast(3, 0) and Load_GL_version_3_0;
  Version_3_1 := GLVersion.AtLeast(3, 1) and Load_GL_version_3_1;
  Version_3_2 := GLVersion.AtLeast(3, 2) and Load_GL_version_3_2;
  Version_3_3 := GLVersion.AtLeast(3, 3) and Load_GL_version_3_3;
  Version_4_0 := GLVersion.AtLeast(4, 0) and Load_GL_version_4_0;

  ARB_window_pos := Load_GL_ARB_window_pos;
  MESA_window_pos := Load_GL_MESA_window_pos;

  ARB_shadow := Load_GL_ARB_shadow;
  EXT_fog_coord := Load_GL_EXT_fog_coord;
  NV_multisample_filter_hint := Load_GL_NV_multisample_filter_hint;
  {$endif}

  {$ifdef OpenGLES}
  CLAMP_TO_EDGE := GL_CLAMP_TO_EDGE;
  {$else}
  if Version_1_2 then
    CLAMP_TO_EDGE := GL_CLAMP_TO_EDGE else
    CLAMP_TO_EDGE := GL_CLAMP;
  {$endif}

  MaxTextureSize := glGetInteger(GL_MAX_TEXTURE_SIZE);

  { TODO: for OpenGLES, as well as non-shader pipeline, this can be actually
    infinite (in theory, of course even 8 lights at the same shape is slow).
    Make it configurable somewhere, e.g. at Attribtes.MaxLightsPerShape ? }
  MaxLights := {$ifdef OpenGLES} 8 {$else} glGetInteger(GL_MAX_LIGHTS) {$endif};

  { We do not look at GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS,
    which is usually much smaller (may even be 0).
    But most of our texture uses do not access the texture from the vertex shader,
    only fragment shader. }
  MaxTextureUnitsShaders := glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS);

  {$ifdef OpenGLES}
  MaxTextureUnitsFixedFunction := 0;
  {$else}
  if Version_1_3 then
    MaxTextureUnitsFixedFunction := glGetInteger(GL_MAX_TEXTURE_UNITS)
  else
    MaxTextureUnitsFixedFunction := 1;
  {$endif}

  MaxCubeMapTextureSize := 0;
  {$ifdef OpenGLES}
  TextureCubeMap := gsStandard;
  {$else}
  if Version_1_3 then
    TextureCubeMap := gsStandard else
  if Load_GL_ARB_texture_cube_map then
    TextureCubeMap := gsExtension else
    TextureCubeMap := gsNone;
  {$endif}
  if TextureCubeMap <> gsNone then
    MaxCubeMapTextureSize := glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE);

  {$ifndef OpenGLES}
  if Version_1_2 then
    Texture3D := gsStandard else
  if Load_GL_EXT_texture3D then
    Texture3D := gsExtension else
  {$endif}
    Texture3D := gsNone;

  { calculate MaxTexture3DSize, eventually correct Texture3D if buggy }
  case Texture3D of
    {$ifndef OpenGLES}
    gsExtension: MaxTexture3DSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE_EXT);
    gsStandard : MaxTexture3DSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE);
    {$endif}
    gsNone     : MaxTexture3DSize := 0;
  end;
  if (MaxTexture3DSize = 0) and (Texture3D <> gsNone) then
  begin
    Texture3D := gsNone;
    WritelnLog('OpenGL', 'Buggy OpenGL 3D texture support: reported as supported, but GL_MAX_3D_TEXTURE_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
  end;

  // TODO: there is also such extension for OpenGL ES
  EXT_texture_filter_anisotropic := {$ifdef OpenGLES} false {$else}
    Load_GL_EXT_texture_filter_anisotropic {$endif};
  if EXT_texture_filter_anisotropic then
    MaxTextureMaxAnisotropyEXT := glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT) else
    MaxTextureMaxAnisotropyEXT := 0.0;

  ARB_occlusion_query := false;
  QueryCounterBits := 0;
  {$ifndef OpenGLES}
  ARB_occlusion_query := Load_GL_ARB_occlusion_query;
  if ARB_occlusion_query then
    glGetQueryivARB(GL_SAMPLES_PASSED_ARB, GL_QUERY_COUNTER_BITS_ARB, @QueryCounterBits);
  {$endif}

  { calculate GLFramebuffer }
  {$ifdef OpenGLES}
  Framebuffer := gsStandard;
  {$else}
  if Version_3_0 or Load_GL_ARB_framebuffer_object then
    Framebuffer := gsStandard else
  if Load_GL_EXT_framebuffer_object then
    Framebuffer := gsExtension else
    Framebuffer := gsNone;
  {$endif}

  if Framebuffer <> gsNone then
  begin
    MaxRenderbufferSize := glGetInteger(GL_MAX_RENDERBUFFER_SIZE { equal to GL_MAX_RENDERBUFFER_SIZE_EXT });
    if MaxRenderbufferSize = 0 then
    begin
      Framebuffer := gsNone;
      WritelnLog('OpenGL', 'Buggy OpenGL Framebuffer: reported as supported, but GL_MAX_RENDERBUFFER_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    MaxRenderbufferSize := 0;

  EXT_framebuffer_blit := Load_GL_EXT_framebuffer_blit;

  { calculate UseMultiTexturing: check extensions required for multitexturing.

    We used to require a couple of extensions for this:
    - EXT_texture_env_combine
    - ARB_multitexture
    - ARB_texture_env_dot3
    But GL version >= 1.3 is actually required for GL_subtract,
    and includes all above extensions in core. }
  UseMultiTexturing := {$ifdef OpenGLES} true {$else} Version_1_3 {$endif};

  FBOMultiSampling := {$ifdef OpenGLES} false {$else}
    { Is GL_ARB_framebuffer_object available? }
    (Framebuffer = gsStandard) and
    Load_GL_ARB_texture_multisample and
    (not GLVersion.BuggyFBOMultiSampling) {$endif};

  Multisample := {$ifdef OpenGLES} true {$else} Load_GL_ARB_multisample {$endif};
  if Multisample and (glGetInteger({$ifdef OpenGLES} GL_SAMPLE_BUFFERS {$else} GL_SAMPLE_BUFFERS_ARB {$endif}) <> 0) then
  begin
    CurrentMultiSampling := glGetInteger({$ifdef OpenGLES} GL_SAMPLES {$else} GL_SAMPLES_ARB {$endif});
    if CurrentMultiSampling <= 1 then
    begin
      WritelnWarning('MultiSampling', Format('We successfully got multi-sampling buffer, but only %d samples per pixel. This doesn''t make much sense, assuming buggy OpenGL implementation, and anti-aliasing may not work.',
        [CurrentMultiSampling]));
      CurrentMultiSampling := 1;
    end;
  end else
    CurrentMultiSampling := 1;

  SupportedExtensions := PChar(glGetString(GL_EXTENSIONS));

  PackedDepthStencil :=
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_packed_depth_stencil', SupportedExtensions)
    {$else} Load_GL_EXT_packed_depth_stencil
    {$endif};

  ShadowVolumesPossible := glGetInteger(GL_STENCIL_BITS) >= 4;

  TextureNonPowerOfTwo := {$ifdef OpenGLES} true {$else}
    Load_GL_ARB_texture_non_power_of_two or Version_2_0 {$endif};

  TextureCompression := [];

  {$ifndef OpenGLES}
  { on non-OpenGLES, we require ARB_texture_compression for *any* compression
    format, to have the necessary glCompressedTexImage2DARB call available }
  if Load_GL_ARB_texture_compression then
  {$endif}
  begin
    { See http://stackoverflow.com/questions/9148795/android-opengl-texture-compression
      and http://developer.android.com/guide/topics/graphics/opengl.html
      for possible GPU extensions for compression formats. }

    if glext_ExtensionSupported('GL_OES_texture_compression_S3TC', SupportedExtensions) or
       glext_ExtensionSupported('GL_EXT_texture_compression_s3tc', SupportedExtensions) or
       glext_ExtensionSupported('GL_NV_texture_compression_s3tc', SupportedExtensions) then
      TextureCompression := TextureCompression + [
        tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5];

    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt1', SupportedExtensions) then
      TextureCompression := TextureCompression + [tcDxt1_RGB, tcDxt1_RGBA];
    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt3', SupportedExtensions) then
      TextureCompression := TextureCompression + [tcDxt3];
    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt5', SupportedExtensions) then
      TextureCompression := TextureCompression + [tcDxt5];

    if glext_ExtensionSupported('GL_IMG_texture_compression_pvrtc', SupportedExtensions) then
      TextureCompression := TextureCompression + [
        tcPvrtc1_4bpp_RGB,
        tcPvrtc1_2bpp_RGB,
        tcPvrtc1_4bpp_RGBA,
        tcPvrtc1_2bpp_RGBA];

    if glext_ExtensionSupported('GL_IMG_texture_compression_pvrtc2', SupportedExtensions) then
      TextureCompression := TextureCompression + [
        tcPvrtc2_4bpp,
        tcPvrtc2_2bpp];

    if glext_ExtensionSupported('GL_AMD_compressed_ATC_texture', SupportedExtensions) or
       glext_ExtensionSupported('GL_ATI_texture_compression_atitc', SupportedExtensions) then
      TextureCompression := TextureCompression + [
        tcATITC_RGB,
        tcATITC_RGBA_ExplicitAlpha,
        tcATITC_RGBA_InterpolatedAlpha];

    if glext_ExtensionSupported('GL_OES_compressed_ETC1_RGB8_texture', SupportedExtensions) then
      TextureCompression := TextureCompression + [tcETC1];

    if glext_ExtensionSupported('GL_KHR_texture_compression_astc_ldr', SupportedExtensions) or
       glext_ExtensionSupported('GL_KHR_texture_compression_astc_hdr', SupportedExtensions) then
      TextureCompression := TextureCompression + [
        tcASTC_4x4_RGBA,
        tcASTC_5x4_RGBA,
        tcASTC_5x5_RGBA,
        tcASTC_6x5_RGBA,
        tcASTC_6x6_RGBA,
        tcASTC_8x5_RGBA,
        tcASTC_8x6_RGBA,
        tcASTC_8x8_RGBA,
        tcASTC_10x5_RGBA,
        tcASTC_10x6_RGBA,
        tcASTC_10x8_RGBA,
        tcASTC_10x10_RGBA,
        tcASTC_12x10_RGBA,
        tcASTC_12x12_RGBA,
        tcASTC_4x4_SRGB8_ALPHA8,
        tcASTC_5x4_SRGB8_ALPHA8,
        tcASTC_5x5_SRGB8_ALPHA8,
        tcASTC_6x5_SRGB8_ALPHA8,
        tcASTC_6x6_SRGB8_ALPHA8,
        tcASTC_8x5_SRGB8_ALPHA8,
        tcASTC_8x6_SRGB8_ALPHA8,
        tcASTC_8x8_SRGB8_ALPHA8,
        tcASTC_10x5_SRGB8_ALPHA8,
        tcASTC_10x6_SRGB8_ALPHA8,
        tcASTC_10x8_SRGB8_ALPHA8,
        tcASTC_10x10_SRGB8_ALPHA8,
        tcASTC_12x10_SRGB8_ALPHA8,
        tcASTC_12x12_SRGB8_ALPHA8 ];
  end;

  SupportedTextureCompressionKnown := true;
  SupportedTextureCompression := TextureCompression;

  VertexBufferObject := {$ifdef OpenGLES} true {$else}
    Version_1_5 and not GLVersion.BuggyVBO {$endif};

  BlendConstant := {$ifdef OpenGLES} true {$else}
    { GL_CONSTANT_ALPHA is available as part of ARB_imaging, since GL 1.4
      as standard. glBlendColor is available since 1.2 as standard. }
    ((Version_1_2 and Load_GL_ARB_imaging) or Version_1_4) and not GLVersion.Fglrx {$endif};

  TextureFloat :=
    {$ifdef OpenGLES} false
    {$else} Load_GL_ATI_texture_float or Load_GL_ARB_texture_float
    {$endif};

  TextureDepth :=
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_depth_texture', SupportedExtensions)
    {$else} Load_GL_ARB_depth_texture
    {$endif};

  {$ifdef OpenGLES}
  Shaders := gsStandard;
  {$else}
  if Version_2_0 then
    Shaders := gsStandard else
  {$ifndef ForceStandardGLSLApi}
  if Load_GL_ARB_shader_objects and
     Load_GL_ARB_vertex_shader and
     Load_GL_ARB_fragment_shader and
     Load_GL_ARB_shading_language_100 then
    Shaders := gsExtension else
  {$endif not ForceStandardGLSLApi}
    Shaders := gsNone;
  {$endif}

  EnableFixedFunction :=
    {$ifdef OpenGLES} false
    {$else}
    (Shaders <> gsStandard) or
    // checking for VertexBufferObject also makes sure that BuggyVBO = false
    (not VertexBufferObject) or
    GLVersion.BuggyPureShaderPipeline
    {$endif};

  { Calculate MaxClipPlanes.
    The logic follows various approaches to clip planes in TShader.EnableClipPlane.
    TODO: If you later switch EnableFixedFunction to true,
    our code doesn't recalculate this limit now. }
  {$ifndef OpenGLES}
  if EnableFixedFunction or (not Version_3_0) then
    MaxClipPlanes := glGetInteger(GL_MAX_CLIP_PLANES)
  else
    MaxClipPlanes := glGetInteger(GL_MAX_CLIP_DISTANCES);
  {$else}
  { Any value is in theory correct for OpenGLES.
    The only limit is number of varying variables we can pass from vertex
    to fragment shader, and fragment code speed.
    With OpenGLES 2, the clipping requires
    passing data to the fragment shader, and discarding fragments there.

    We set 8, as it seems reasonable, and is the minimal guaranteed value
    of GL_MAX_CLIP_DISTANCES. }
  MaxClipPlanes := 8;
  {$endif}

  MaxViewportDimensions := GetVector2Integer(GL_MAX_VIEWPORT_DIMS);

  // set GLFeatures singleton, TGLMemoryInfo.Refresh depends on it
  GLFeatures := Self;

  Memory := TGLMemoryInfo.Create;
end;

destructor TGLFeatures.Destroy;
begin
  FreeAndNil(Memory);
end;

function TGLFeatures.MaxTextureUnits: Cardinal;
begin
  if EnableFixedFunction then
    { Note doing
        Result := Min(MaxTextureUnitsFixedFunction, MaxTextureUnitsShaders)
      would not be OK, since then a GPU that cannot do shaders would not
      be able to use textures at all. }
    Result := MaxTextureUnitsFixedFunction
  else
    Result := MaxTextureUnitsShaders;
end;

{ TGLMemoryInfo -------------------------------------------------------------- }

constructor TGLMemoryInfo.Create;
var
  SupportedExtensions: string;
begin
  inherited;
  SupportedExtensions := PChar(glGetString(GL_EXTENSIONS));
  ExtensionNVidia := glext_ExtensionSupported('GL_NVX_gpu_memory_info', SupportedExtensions);
  ExtensionATI := glext_ExtensionSupported('GL_ATI_meminfo', SupportedExtensions);
  Refresh;
end;

procedure TGLMemoryInfo.Refresh;
const
  GL_GPU_MEM_INFO_TOTAL_AVAILABLE_MEM_NVX   = $9048;
  GL_GPU_MEM_INFO_CURRENT_AVAILABLE_MEM_NVX = $9049;
  GL_VBO_FREE_MEMORY_ATI          = $87FB;
  GL_TEXTURE_FREE_MEMORY_ATI      = $87FC;
  GL_RENDERBUFFER_FREE_MEMORY_ATI = $87FD;
var
  FreeMemory: TVector4Integer;
begin
  // see http://nasutechtips.blogspot.com/2011/02/how-to-get-gpu-memory-size-and-usage-in.html

  // http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
  if ExtensionNVidia then
  begin
    glGetIntegerv(GL_GPU_MEM_INFO_TOTAL_AVAILABLE_MEM_NVX, @Total);
    glGetIntegerv(GL_GPU_MEM_INFO_CURRENT_AVAILABLE_MEM_NVX, @Current);
  end;

  // https://www.khronos.org/registry/OpenGL/extensions/ATI/ATI_meminfo.txt
  if ExtensionATI then
  begin
    if GLFeatures.VertexBufferObject then
    begin
      glGetIntegerv(GL_VBO_FREE_MEMORY_ATI, @FreeMemory);
      CurrentVbo := FreeMemory[0];
    end;

    glGetIntegerv(GL_TEXTURE_FREE_MEMORY_ATI, @FreeMemory);
    CurrentTexture := FreeMemory[0];

    if GLFeatures.Framebuffer <> gsNone then
    begin
      glGetIntegerv(GL_RENDERBUFFER_FREE_MEMORY_ATI, @FreeMemory);
      CurrentRenderbuffer := FreeMemory[0];
    end;
  end;
end;

function TGLMemoryInfo.LessTextureMemoryThan(const RequiredMemoryKb: Integer): boolean;
begin
  if Current <> 0 then
    Result := Current < RequiredMemoryKb
  else
  if CurrentTexture <> 0 then
    Result := CurrentTexture < RequiredMemoryKb
  else
    // unknown memory, assume old GPU with low memory
    Result := true;
end;

{$endif read_implementation}
