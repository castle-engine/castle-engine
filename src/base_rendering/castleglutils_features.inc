{%MainUnit castleglutils.pas}
{
  Copyright 2016-2023 Michalis Kamburelis.

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
  { Unused for now.
    Was used to differentiate in TGLFeatures whether something is supported as core or extension,
    but in the end -- this it not needed for anything now.
    Maybe will be useful again in the future.
    @exclude }
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  { Unused for now.
    Was used to differentiate in TGLFeatures whether something is supported as core or extension,
    but in the end -- this it not needed for anything now.
    Maybe will be useful again in the future.
    @exclude }
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

type
  { OpenGL memory information. }
  TGLMemoryInfo = class
  strict private
    ExtensionNVidia, ExtensionATI: Boolean;
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
    function LessTextureMemoryThan(const RequiredMemoryKb: Integer): Boolean;
  end;

  { OpenGL(ES) capabilities to request at context creation.
    Use with @link(TGLFeatures.RequestCapabilities). }
  TGLRequestCapabilities = (
    { Use modern GPU code (shaders, VBO etc.) unless it is not possible
      (not supported by GPU).
      This is the default mode.

      GLFeatures.EnableFixedFunction may be @false (preferred) or @true (fallback).

      TODO: At some point, rcAutomatic will automatically require modern context
      with deprecated functionality removed (just like rcForceModern)
      and only fallback on non-modern context if that is not possible.
      For now, rcAutomatic still initializes context with compatibility entry points
      too, although it will not use them.
      We use flag rcForceModern to test whether the "modern (core) context with
      deprecated stuff removed" is worth the hassle, i.e. is there a practical benefit
      (measurable speedup / functionality boost) of not having compatibility entry points. }
    rcAutomatic,

    { Force using deprecated fixed-function even on modern GPUs.
      Should only be used to test our fixed-function code,
      never use this in production code -- you should always strive to use
      modern GPU code.

      GLFeatures.EnableFixedFunction will be always @true.

      We will keep OpenGL API usage at 1.x, for large things (no shaders,
      no VBO) and small things (like querying OpenGL extensions).
      The goal is to really simulate ancient GPU, before shaders were part
      of the OpenGL standard. }
    rcForceFixedFunction,

    { Force initializing modern OpenGL context, with deprecated functionality
      not available.

      Modern context requires support for modern functionality (like shaders)
      and may actually remove support for deprecated functionality (like fixed-function).
      This may provide some additional speedup / functionality at some point
      (but nothing you can really observe in practice, for now, in my experience).

      Using this guarantees you always have GLFeatures.EnableFixedFunction = @false.

      On platforms where we honor this flag at context creation (using glX etc.):
      This even guarantees more: that at least OpenGL version 3.3
      (TGLFeatures.ModernVersionMajor, TGLFeatures.ModernVersionMinor) is available.
      In exchange, it really prohibits using any older deprecated OpenGL API.
      So the code must, when Version_3_3 (or maybe from earlier versions for each
      functionality), use new OpenGL API (and not any deprecated API).

      The specifics what it does:
      We try to initialize "core" profile of OpenGL version 3.3
      (TGLFeatures.ModernVersionMajor, TGLFeatures.ModernVersionMinor).

      TODO: On macOS, we will also request forward-compatible context.
      See https://www.khronos.org/opengl/wiki/OpenGL_Context#Context_types ,
      https://www.khronos.org/opengl/wiki/Creating_an_OpenGL_Context_(WGL) .

      TODO: For now, this only really affects context creation on glX,
      on other platforms we just check that we got context with
      EnableFixedFunction = false. }
    rcForceModern
  );

const
  CapabilitiesStr: array [TGLRequestCapabilities] of String = (
    'automatic',
    'force-fixed-function',
    'force-modern'
  );

type
  { OpenGL(ES) features, analyzed based on extensions and version.
    A single instance of this class is assigned to @link(GLFeatures)
    after the first rendering context was created.
    If you have multiple rendering contexts, our engine assumes they share resources
    and have equal features. }
  TGLFeatures = class
  strict private
    {$ifndef OpenGLES}
    FVersion_1_2: Boolean;
    FVersion_1_3: Boolean;
    FVersion_1_4: Boolean;
    FVersion_1_5: Boolean;
    FVersion_2_0: Boolean;
    FVersion_2_1: Boolean;
    FVersion_3_0: Boolean;
    FVersion_3_1: Boolean;
    FVersion_3_2: Boolean;
    FVersion_3_3: Boolean;
    FVersion_4_0: Boolean;
    {$else}
    FVersionES_2_0: Boolean;
    FVersionES_3_0: Boolean;
    FVersionES_3_1: Boolean;
    FVersionES_3_2: Boolean;
    {$endif}
    FTextureFilterAnisotropic: Boolean;
    FNV_multisample_filter_hint: Boolean;
    FOcclusionQuery: Boolean;
    FCLAMP_TO_EDGE: TGLenum;
    FMaxTextureSize: Cardinal;
    FMaxLights: Cardinal;
    FMaxCubeMapTextureSize: Cardinal;
    FMaxTexture3DSize: Cardinal;
    FMaxTextureMaxAnisotropy: Single;
    FOcclusionQueryCounterBits: TGLint;
    FMaxRenderbufferSize: TGLuint;
    FMaxClipPlanes: Cardinal;
    FMultisample: Boolean;
    FMaxTextureUnitsFixedFunction: Cardinal;
    FMaxTextureUnitsShaders: Cardinal;
    FUseMultiTexturing: Boolean;
    FTexture3D: Boolean;
    FFramebuffer: Boolean;
    FFramebufferBlit: Boolean;
    FFBOMultiSampling: Boolean;
    FCurrentMultiSampling: Cardinal;
    FPackedDepthStencil: Boolean;
    FShadowVolumesPossible: Boolean;
    FTextureNonPowerOfTwo: Boolean;
    FTextureCubeMap: Boolean;
    FTextureCompression: TTextureCompressions;
    FVertexBufferObject: Boolean;
    FVertexArrayObject: Boolean;
    FBlendConstant: Boolean;
    FTextureFloat: Boolean;
    FTextureDepth, FTextureDepthCompare: Boolean;
    FShaders: Boolean;
    FEnableFixedFunction: Boolean;
    FMemory: TGLMemoryInfo;
    FMaxViewportDimensions: TVector2Integer;
    FTextureModernFormatsAndSwizzle: Boolean;
    SupportedExtensions: AnsiString;
    FRedBits, FGreenBits, FBlueBits, FAlphaBits, FDepthBits, FStencilBits: TGLint;
    FDoubleBuffer: Boolean;
    FLineWidth: TVector2;
    FTextureMaxLevel: Boolean;
    class var
      FRequestCapabilities: TGLRequestCapabilities;
      FDebug: Boolean;
    class procedure SetRequestCapabilities(const Value: TGLRequestCapabilities); static;
    class procedure SetDebug(const Value: Boolean); static;
  private
    function HasExtension(const Extension: String): Boolean;
    function AllExtensions: String;
  public
    constructor Create;
    destructor Destroy; override;

    {$ifndef OpenGLES}
    const
      { OpenGL version considered "modern" for us,
        that we can use with its "core" profile.

        We need >= 3.2 as before it, there were no profiles (only "forward compatibility").

        We need >= 3.3 as it has texture swizzle.
        When we don't have texture swizzle,
        then we cannot use modern OpenGL texture format constants,
        and so we need compatibility.
        See TextureModernFormatsAndSwizzle . }
      ModernVersionMajor = 3;
      ModernVersionMinor = 3;

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
    property Version_1_2: Boolean read FVersion_1_2;
    property Version_1_3: Boolean read FVersion_1_3;
    property Version_1_4: Boolean read FVersion_1_4;
    property Version_1_5: Boolean read FVersion_1_5;
    property Version_2_0: Boolean read FVersion_2_0;
    property Version_2_1: Boolean read FVersion_2_1;
    property Version_3_0: Boolean read FVersion_3_0;
    property Version_3_1: Boolean read FVersion_3_1;
    property Version_3_2: Boolean read FVersion_3_2;
    property Version_3_3: Boolean read FVersion_3_3;
    property Version_4_0: Boolean read FVersion_4_0;
    {$else}
    property VersionES_2_0: Boolean read FVersionES_2_0;
    property VersionES_3_0: Boolean read FVersionES_3_0;
    property VersionES_3_1: Boolean read FVersionES_3_1;
    property VersionES_3_2: Boolean read FVersionES_3_2;
    {$endif}

    { Supports anisotropic texture filtering, as EXT_texture_filter_anisotropic extension
      (possible for both OpenGL and OpenGLES).
      This was introduced into OpenGL core late, but is widely available as extension.
      https://www.khronos.org/opengl/wiki/Sampler_Object#Anisotropic_filtering
      https://www.khronos.org/opengl/wiki/Ubiquitous_Extension }
    property TextureFilterAnisotropic: Boolean read FTextureFilterAnisotropic;

    property NV_multisample_filter_hint: Boolean read FNV_multisample_filter_hint;

    { Occlusion query (glGenQueries, glGetQueryObject, glBeginQuery, glEndQuery)
      in core OpenGL or OpenGLES. }
    property OcclusionQuery: Boolean read FOcclusionQuery;

    { GL_CLAMP_TO_EDGE, if available in current OpenGL version.
      Otherwise GL_CLAMP.
      Use this (instead of just GL_CLAMP_TO_EDGE)
      to work with @italic(really ancient) OpenGL versions before 1.2. }
    property CLAMP_TO_EDGE: TGLenum read FCLAMP_TO_EDGE;

    { Constant (for given context) OpenGL limits.
      Initialized to 0 if appropriate OpenGL functionality is not available.

      Note that MaxLights is only relevant if we use OpenGL fixed-function pipeline.
      Otherwise the actual limit is defined purely by
      @link(TCastleRenderOptions.MaxLightsPerShape Scene.RenderOptions.MaxLightsPerShape),
      specific to each TCastleScene,
      and is completely configurable (can be higher or lower than MaxLights).

      @groupBegin }
    property MaxTextureSize: Cardinal read FMaxTextureSize;
    property MaxLights: Cardinal read FMaxLights;
    property MaxCubeMapTextureSize: Cardinal read FMaxCubeMapTextureSize;
    property MaxTexture3DSize: Cardinal read FMaxTexture3DSize;
    property MaxTextureMaxAnisotropy: Single read FMaxTextureMaxAnisotropy;
    property OcclusionQueryCounterBits: TGLint read FOcclusionQueryCounterBits;
    property MaxRenderbufferSize: TGLuint read FMaxRenderbufferSize;
    property MaxClipPlanes: Cardinal read FMaxClipPlanes;
    { @groupEnd }

    { Multisampling is available and can be disabled / enabled by GL_MULTISAMPLE flag
      (it is enabled by default, if buffers have multiple samples available). }
    property Multisample: Boolean read FMultisample;

    { Number of texture units available for fixed-function pipeline.

      Since querying glGetInteger(GL_MAX_TEXTURE_UNITS) is deprecated
      (or not even possible in case of OpenGLES or "core" profile)
      and we want to keep fixed-function support as simple as possible,
      we just assume 2 (the only number guaranteed) if only multi-texturing
      is available (UseMultiTexturing). }
    property MaxTextureUnitsFixedFunction: Cardinal read FMaxTextureUnitsFixedFunction;

    { Number of texture units available for shader pipeline. }
    property MaxTextureUnitsShaders: Cardinal read FMaxTextureUnitsShaders;

    { Are all OpenGL multi-texturing extensions for
      VRML/X3D MultiTexture support available.

      This used to check a couple of multitexturing extensions,
      like ARB_multitexture. Right now, it simply checks for OpenGL 1.3 version.
      It is supported by virtually all existing GPUs.
      So it's acceptable to just check it, and write your code for 1.3,
      and eventual fallback code (when this is false) write only for really
      ancient GPUs. }
    property UseMultiTexturing: Boolean read FUseMultiTexturing;

    { Are 3D textures (GL_TEXTURE_3D, glTexImage3D) supported by core OpenGL or OpenGLES. }
    property Texture3D: Boolean read FTexture3D;

    { Framebuffer supported, by core OpenGL or OpenGLES. }
    property Framebuffer: Boolean read FFramebuffer;

    { glFramebufferBlit supported, by core OpenGL or OpenGLES. }
    property FramebufferBlit: Boolean read FFramebufferBlit;

    { Is multisampling possible for FBO buffers and textures.
      Although these are two orthogonal features of OpenGL,
      in practice you want to use multisample for both FBO buffers and textures,
      or for none --- otherwise, FBO can not be initialized correctly
      when you mix various multisample settings. }
    property FBOMultiSampling: Boolean read FFBOMultiSampling;

    { How multi-sampling was initialized for this OpenGL context.
      Value = 1 means that no multi-sampling is initialized.
      Values > 1 mean that you have multi-sampling, with given number of samples
      per pixel.
      Contrast this with TCastleWindow.MultiSampling or TOpenGLControl.MultiSampling,
      that say @italic(how many samples you wanted to get). }
    property CurrentMultiSampling: Cardinal read FCurrentMultiSampling;

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
    property PackedDepthStencil: Boolean read FPackedDepthStencil;

    { Does OpenGL context support shadow volumes.
      This checks do we have:
      - stencil buffer with at least 4 bits
      - stencil operations that wrap (GL_INCR/DECR_WRAP in OpenGL >= 2.0)
      - separate stencil operation (glStencilOpSeparate, without this shadow volumes
        are more complicated, and now we just require it) }
    property ShadowVolumesPossible: Boolean read FShadowVolumesPossible;

    { Are non-power-of-2 textures supported. }
    property TextureNonPowerOfTwo: Boolean read FTextureNonPowerOfTwo;

    { Are cubemaps supported (so you can load to texture targets like
      GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z) by core OpenGL or OpenGLES. }
    property TextureCubeMap: Boolean read FTextureCubeMap;

    { Which texture compression formats are supported.
      If this is not [], then we guarantee glCompressedTexImage2D is also supported. }
    property TextureCompression: TTextureCompressions read FTextureCompression;

    { VBO support (in OpenGL (ES) core). }
    property VertexBufferObject: Boolean read FVertexBufferObject;

    { VAO support (in OpenGL (ES) core). }
    property VertexArrayObject: Boolean read FVertexArrayObject;

    { glBlendColor and GL_CONSTANT_ALPHA support. }
    property BlendConstant: Boolean read FBlendConstant;

    { Support for float texture formats for glTexImage2d. }
    property TextureFloat: Boolean read FTextureFloat;

    { Support for depth texture formats for glTexImage2d. }
    property TextureDepth: Boolean read FTextureDepth;

    { Ability to create depth textures (TextureDepth), to define comparison on them:
      GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_COMPARE_REF_TO_TEXTURE,
      finally to use then in shaders (sampler2DShadow).
      On OpenGLES the GLSL constructs require that we can use "#version 300 es",
      to access sampler2DShadow using texture / textureProj calls. }
    property TextureDepthCompare: Boolean read FTextureDepthCompare;

    { Are GLSL shaders supported. }
    property Shaders: Boolean read FShaders;

    { Use OpenGL fixed-function rendering for ancient OpenGL versions on desktops.
      This rules whole rendering approach.

      When this is @false, you can be sure that these features are supported
      (OpenGL API exposes them, they are not detected as "buggy"):

      @unorderedList(
        @item(Multi-texturing (@link(UseMultiTexturing) is guaranteed to be @true))
        @item(VBO (@link(VertexBufferObject) is guaranteed to be @true))
        @item(Shaders (@link(Shaders) is guaranteed to be @true))
        @item(OpenGL version >= 2.0 is available on desktops.

          Note that when using rcForceModern, we want to have a "core" profile with OpenGL >= 3.3
          (TGLFeatures.ModernVersionMajor, TGLFeatures.ModernVersionMinor).
          And as such when Version_3_3 is @true, you should use OpenGL 3.3 API,
          not API from older OpenGL like 2.x.

          This means that, aside from EnabledFixedFunction boolean,
          you should still check Version_X_Y to avoid using functionality that is deprecated
          and replaced with something else
          (in OpenGL 3.3, TGLFeatures.ModernVersionMajor, TGLFeatures.ModernVersionMinor).
        )
      )

      When this is @true, you should use ancient fixed-function pipeline for all rendering,
      without shaders or VBO.
      In fact, to make it easier, we set Shaders and VertexBufferObject to @false
      in this case (even if in theory one of them could be used, at least in some cases...
      we don't want to maintain too many alternatives for ancient systems,
      we want only one path for ancient systems: all modern stuff disabled).
      This makes Shaders and VertexBufferObject practically useless now:
      they are always negation of EnabledFixedFunction. And this is OK, it means we have
      2 clear approaches to rendering, not a lot of possible combinations to test.

      Note that @link(UseMultiTexturing) may be @true or @false when
      EnableFixedFunction is @true. So we still allow the option to support fixed-function
      with or without multi-texturing.

      When this is @true:

      @unorderedList(
        @item(This forces Gouraud shading.)
        @item(It makes TDrawableImage, DrawPrimitive2D, TCastleRenderUnlitMesh
          use OpenGL fixed-function pipeline.)
        @item(It allows you to use fixed-function pipeline calls in the
          @link(TCastleWindow.OnRender) callback, @link(TCastleUserInterface.Render)
          override and related places.)
        @item(It also allows some deprecated rendering features like WindowPos
          to continue to work.)
      )

      Note that on OpenGLES, it must be always @false, since OpenGLES >= 2
      has no fixed-function pipeline, and it guarantees having working shaders and VBO.
    }
    property EnableFixedFunction: Boolean read FEnableFixedFunction;

    { Request specific OpenGL(ES) capabilities at context creation.

      This is a class property and it can only change before OpenGL(ES)
      context is initialized, which means before TCastleWindow.Open
      occurs, which means before @link(GLFeatures) instance is initialized.

      The routines that initialize OpenGL(ES) context, like using wgl (on Windows) or glX (on Unix)
      should look at this setting and honour it.

      Normal code using OpenGL(ES) should not look at this property:
      instead, detect the current OpenGL(ES) capabilities looking at properties of current
      @link(TGLFeatures) instance, like @link(TGLFeatures.Framebuffer), @link(TGLFeatures.Version_4_0) etc.
      This way, we should use modern features both when rcForceFixedFunction and rcAutomatic is set. }
    class property RequestCapabilities: TGLRequestCapabilities
      read FRequestCapabilities write SetRequestCapabilities;

    { Request debug OpenGL(ES) context.
      OpenGL will report a lot of information and errors to log,
      and OpenGL API usage errors will result in immediate exception (with useful stack-trace
      pointing to their usage in CGE).
      This should never be used at production, but it is very useful when developing
      and using OpenGL(ES) API directly.

      See https://www.khronos.org/opengl/wiki/Debug_Output . }
    class property Debug: Boolean read FDebug write SetDebug;

    { Total and current memory available.
      Call Memory.Refresh whenever needed. }
    property Memory: TGLMemoryInfo read FMemory;

    property MaxViewportDimensions: TVector2Integer read FMaxViewportDimensions;

    { Maximum number of texture units you can safely always use.
      If EnableFixedFunction, this is MaxTextureUnitsFixedFunction,
      otherwise MaxTextureUnitsShaders. }
    function MaxTextureUnits: Cardinal;

    { We should use modern OpenGL enums for format, internal format
      and we have texture swizzle available.

      This means we should use e.g. GL_RG8 and not GL_LUMINANCE_ALPHA.
      See https://www.khronos.org/opengl/wiki/Image_Format .

      To make it always work (e.g. to use grayscale or grayscale+alpha images),
      we really need
      https://www.khronos.org/opengl/wiki/Texture#Swizzle_mask
      https://registry.khronos.org/OpenGL/extensions/ARB/ARB_texture_swizzle.txt
      https://registry.khronos.org/OpenGL/extensions/EXT/EXT_texture_swizzle.txt
      (core extension since 3.3).
    }
    property TextureModernFormatsAndSwizzle: Boolean read FTextureModernFormatsAndSwizzle;

    { Size, in bits, of the red channel of the color buffer. }
    property RedBits: TGLint read FRedBits;
    { Size, in bits, of the green channel of the color buffer. }
    property GreenBits: TGLint read FGreenBits;
    { Size, in bits, of the blue channel of the color buffer. }
    property BlueBits: TGLint read FBlueBits;
    { Size, in bits, of the alpha channel of the color buffer. }
    property AlphaBits: TGLint read FAlphaBits;
    { Size, in bits, of the depth buffer. }
    property DepthBits: TGLint read FDepthBits;
    { Size, in bits, of the stencil buffer. }
    property StencilBits: TGLint read FStencilBits;
    { Do we have double-buffer. }
    property DoubleBuffer: Boolean read FDoubleBuffer;

    { Line width granularity and maximum width. }
    property LineWidth: TVector2 read FLineWidth;

    { Supports glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAX_LEVEL, ...); }
    property TextureMaxLevel: Boolean read FTextureMaxLevel;
  end;

{ Initialize GLVersion and GLFeatures. }
procedure GLInformationInitialize;

var
  GLFeatures: TGLFeatures;

  { Much more verbose logging of current context capabilities in GLInformationInitialize. }
  LogGLInformationVerbose: Boolean = false;

{ Convert a string (like command-line argument) to TGLRequestCapabilities. }
function StrToCapabilities(const S: String): TGLRequestCapabilities;

{$endif read_interface}

{$ifdef read_implementation}

procedure GLInformationInitialize;
begin
  FreeAndNil(GLVersion);
  GLVersion := TGLVersion.Create( PGLChar(glGetString(GL_VERSION)),
    PGLChar(glGetString(GL_VENDOR)), PGLChar(glGetString(GL_RENDERER)));

  FreeAndNil(GLFeatures);
  GLFeatures := TGLFeatures.Create;

  if LogGLInformationVerbose then
    WritelnLogMultiline('Rendering Initialized', GLInformationString)
  else
    WritelnLog('Rendering Initialized', GLInformationStringSummary +
      ' (for more info: LogGLInformationVerbose:=true)');
end;

function TGLFeatures.HasExtension(const Extension: String): Boolean;

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

    if searchIn = '' then extensions := PGLChar(glGetString(GL_EXTENSIONS))
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

var
  SupportedExt: AnsiString;
  I: Integer;
begin
  { Catch an often mistake in Extension name. }
  if not IsPrefix('GL_', Extension, false) then
    raise Exception.CreateFmt('Using TGLFeatures.HasExtension with extension name without GL_ prefix: %s', [
      Extension
    ]);

  if {$ifdef OpenGLES} VersionES_3_0 {$else} Version_3_0 {$endif} then
  begin
    { With new OpenGL, one should use glGetStringi to enumerate over extensions,
      see https://www.khronos.org/opengl/wiki/GLAPI/glGetString }
    for I := 0 to glGetInteger(GL_NUM_EXTENSIONS) - 1 do
    begin
      SupportedExt := PGLChar(glGetStringi(GL_EXTENSIONS, I));
      if SupportedExt = Extension then
      begin
        // WritelnLog('Detected extension as supported: ' + Extension);
        Exit(true);
      end;
    end;
    Result := false;
  end else
  begin
    if SupportedExtensions = '' then
      SupportedExtensions := PAnsiChar(glGetString(GL_EXTENSIONS));
    Result := glext_ExtensionSupported(Extension, SupportedExtensions);
  end;
end;

function TGLFeatures.AllExtensions: String;
var
  NumExtensions: TGLint;
  SupportedExt: AnsiString;
  I: Integer;
begin
  if {$ifdef OpenGLES} VersionES_3_0 {$else} Version_3_0 {$endif} then
  begin
    { Output in this case is a bit richer, since it was natural to implement using
      new OpenGL way to query for extensions. }
    NumExtensions := glGetInteger(GL_NUM_EXTENSIONS);
    Result := Format('%d extensions: ', [NumExtensions]);
    for I := 0 to glGetInteger(GL_NUM_EXTENSIONS) - 1 do
    begin
      SupportedExt := PGLChar(glGetStringi(GL_EXTENSIONS, I));
      Result := Result + Format('(%d) %s ', [I, SupportedExt]);
    end;
  end else
  begin
    if SupportedExtensions = '' then
      SupportedExtensions := PAnsiChar(glGetString(GL_EXTENSIONS));
    Result := SupportedExtensions;
  end;
end;

procedure CallbackGLDebug(Source: TGLenum; AType: TGLenum; Id: TGLuint; Severity: TGLenum;
  Length: TGLsizei; Message: PAnsiChar; UserParam: Pointer);
  {$if     defined(OPENGL_CALLBACK_STDCALL)} stdcall;
  {$elseif defined(OPENGL_CALLBACK_CDECL)} cdecl;
  {$else} {$fatal Include openglmac.inc that should define OPENGL_CALLBACK_xxx}
  {$endif}
var
  SourceStr, TypeStr, SeverityStr, MessageStr, MessageWhole: String;
begin
  case Source of
    GL_DEBUG_SOURCE_API            : SourceStr := 'API';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM  : SourceStr := 'Window System';
    GL_DEBUG_SOURCE_SHADER_COMPILER: SourceStr := 'Shader Compiler';
    GL_DEBUG_SOURCE_THIRD_PARTY    : SourceStr := '3rd Party';
    GL_DEBUG_SOURCE_APPLICATION    : SourceStr := 'Application';
    GL_DEBUG_SOURCE_OTHER          : SourceStr := 'Other';
    else                             SourceStr := 'Unknown';
  end;

  case AType of
    GL_DEBUG_TYPE_ERROR              : TypeStr := 'GL error';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: TypeStr := 'Attempted use of deprecated features';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR : TypeStr := 'Triggering of undefined behavior';
    GL_DEBUG_TYPE_PORTABILITY        : TypeStr := 'Portability issue';
    GL_DEBUG_TYPE_PERFORMANCE        : TypeStr := 'Performance notification';
    GL_DEBUG_TYPE_MARKER             : TypeStr := 'Marker';
    GL_DEBUG_TYPE_PUSH_GROUP         : TypeStr := 'Group push';
    GL_DEBUG_TYPE_POP_GROUP          : TypeStr := 'Group pop';
    GL_DEBUG_TYPE_OTHER              : TypeStr := 'Other';
    else                               TypeStr := 'Unknown';
  end;

  case Severity of
    GL_DEBUG_SEVERITY_LOW         : SeverityStr := 'Low';
    GL_DEBUG_SEVERITY_MEDIUM      : SeverityStr := 'Medium';
    GL_DEBUG_SEVERITY_HIGH        : SeverityStr := 'High';
    GL_DEBUG_SEVERITY_NOTIFICATION: SeverityStr := 'Notification';
    else                            SeverityStr := 'Unknown';
  end;

  MessageStr := Message;

  MessageWhole := Format('OpenGL debug: %s, %s, %d, %s: %s', [
    SourceStr,
    TypeStr,
    Id,
    SeverityStr,
    MessageStr
  ]);

  if AType = GL_DEBUG_TYPE_ERROR then
    raise EOpenGLError.Create(MessageWhole)
  else
  // Don't output notifcations, too much log every frame
  if Severity <> GL_DEBUG_SEVERITY_NOTIFICATION then
    WritelnLog(MessageWhole);
end;

{$ifndef OpenGLES}
{ Workaround GLExt in FPC missing "extdecl" for these routines,
  see https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/381 }
var
  glDebugMessageCallback:procedure (callback:GLDEBUGPROC; userParam: Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDebugMessageControl :procedure (source:GLenum; _type:GLenum; severity:GLenum; count:GLsizei; ids:PGLuint; enabled:GLboolean); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

constructor TGLFeatures.Create;

  function GetVector2Integer(const Param: TGLenum): TVector2Integer;
  var
    V: packed array [0..1] of TGLInt;
  begin
    glGetIntegerv(Param, @V);
    Result.X := V[0];
    Result.Y := V[1];
  end;

  procedure InitializeDebug;
  begin
    if HasExtension('GL_KHR_debug') then
    begin
      {$ifndef OpenGLES}
      { Initialize manually to:
        - Workaround that we cannot use Load_GL_Debug_output, as it uses deprecated
          glGetString(GL_EXTENSIONS). So we initialize 2 entry points manually.
        - Workaround that we define them in this unit, to have proper stdcall/cdecl,
          see https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/381 .
        Note that we don't really use glDebugMessageControl now, but we load+check it too,
        for consistency.
        This is not necessary on OpenGLES, our CastleGLES just always loads all entry points now,
        without even checking GL_KHR_debug. }
      Pointer({$ifndef FPC}@{$endif} glDebugMessageCallback) := wglGetProcAddress('glDebugMessageCallback');
      Pointer({$ifndef FPC}@{$endif} glDebugMessageControl) := wglGetProcAddress('glDebugMessageControl');
      {$endif}

      if not Assigned(glDebugMessageCallback) then raise Exception.Create('glDebugMessageCallback missing');
      if not Assigned(glDebugMessageControl) then raise Exception.Create('glDebugMessageControl missing');

      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
      glDebugMessageCallback(
        {$ifndef FPC}GLDEBUGPROC(@CallbackGLDebug){$endif}
        {$ifdef FPC}@CallbackGLDebug{$endif}, nil);
      WritelnLog('OpenGL(ES) debugging enabled, expect extra log messages');
    end;
  end;

{$ifndef OpenGLES}
var
  LoadExtensions: Boolean;
{$endif}
begin
  inherited;

  {$ifndef OpenGLES}
  { TODO: We cannot load extensions with rcForceModern, as FPC GLExt then uses
    glGetString(GL_EXTENSIONS) which is not available.
    With new OpenGL, one should use glGetStringi to enumerate over extensions,
    see our HasExtension. }
  LoadExtensions := RequestCapabilities <> rcForceModern;
  {$endif}

  {$ifndef OpenGLES}
  {$ifdef FPC}
  if RequestCapabilities = rcForceModern then
  begin
    { Do not load non-core OpenGL entry points in this case.
      We define still all Version_X_Y booleans, because this is easier for
      backward compatibility, as most code used them like "at least X.Y".

      In the long run, we should probably just remove these Version_X_Y
      with unclear meaning now -- since having core/forward-compatible context
      means we may have version > X.Y, but still not have entry points for X.Y.
    }
    FVersion_1_2 := GLVersion.AtLeast(1, 2);
    FVersion_1_3 := GLVersion.AtLeast(1, 3);
    FVersion_1_4 := GLVersion.AtLeast(1, 4);
    FVersion_1_5 := GLVersion.AtLeast(1, 5);
    FVersion_2_0 := GLVersion.AtLeast(2, 0);
    FVersion_2_1 := GLVersion.AtLeast(2, 1);
    FVersion_3_0 := GLVersion.AtLeast(3, 0) and Load_GL_version_3_0_CORE();
    FVersion_3_1 := GLVersion.AtLeast(3, 1) and Load_GL_version_3_1_CORE();
    FVersion_3_2 := GLVersion.AtLeast(3, 2) and Load_GL_version_3_2_CORE();
    FVersion_3_3 := GLVersion.AtLeast(3, 3) and Load_GL_version_3_3_CORE();
    FVersion_4_0 := GLVersion.AtLeast(4, 0) and Load_GL_version_4_0_CORE();
  end else
  begin
    FVersion_1_2 := GLVersion.AtLeast(1, 2) and Load_GL_version_1_2;
    FVersion_1_3 := GLVersion.AtLeast(1, 3) and Load_GL_version_1_3;
    FVersion_1_4 := GLVersion.AtLeast(1, 4) and Load_GL_version_1_4;
    FVersion_1_5 := GLVersion.AtLeast(1, 5) and Load_GL_version_1_5;
    if RequestCapabilities <> rcForceFixedFunction then
    begin
      FVersion_2_0 := GLVersion.AtLeast(2, 0) and Load_GL_version_2_0;
      FVersion_2_1 := GLVersion.AtLeast(2, 1) and Load_GL_version_2_1;
      FVersion_3_0 := GLVersion.AtLeast(3, 0) and Load_GL_version_3_0;
      FVersion_3_1 := GLVersion.AtLeast(3, 1) and Load_GL_version_3_1;
      FVersion_3_2 := GLVersion.AtLeast(3, 2) and Load_GL_version_3_2;
      FVersion_3_3 := GLVersion.AtLeast(3, 3) and Load_GL_version_3_3;
      FVersion_4_0 := GLVersion.AtLeast(4, 0) and Load_GL_version_4_0;
    end;
  end;
  {$else}
  // TODO: Delphi OpenGLext has only InitOpenGLext
  FVersion_1_2 := GLVersion.AtLeast(1, 2);
  FVersion_1_3 := GLVersion.AtLeast(1, 3);
  FVersion_1_4 := GLVersion.AtLeast(1, 4);
  FVersion_1_5 := GLVersion.AtLeast(1, 5);
  if RequestCapabilities <> rcForceFixedFunction then
  begin
    FVersion_2_0 := GLVersion.AtLeast(2, 0);
    FVersion_2_1 := GLVersion.AtLeast(2, 1);
    FVersion_3_0 := GLVersion.AtLeast(3, 0);
    FVersion_3_1 := GLVersion.AtLeast(3, 1);
    FVersion_3_2 := GLVersion.AtLeast(3, 2);
    FVersion_3_3 := GLVersion.AtLeast(3, 3);
    FVersion_4_0 := GLVersion.AtLeast(4, 0);
  end;
  InitOpenGLext; // TODO: check that later
  {$endif}
  {$else}
  FVersionES_2_0 := GLVersion.AtLeast(2, 0);
  FVersionES_3_0 := GLVersion.AtLeast(3, 0);
  FVersionES_3_1 := GLVersion.AtLeast(3, 1);
  FVersionES_3_2 := GLVersion.AtLeast(3, 2);

  if not VersionES_2_0 then
    raise Exception.Create('We only support OpenGL ES versions >= 2.0');
  {$endif}

  { Do this as early as possible, which means: after Version_3_2 is defined,
    to debug even errors below in TGLFeatures initialization. }
  if Debug then
    InitializeDebug;

  FNV_multisample_filter_hint := HasExtension('GL_NV_multisample_filter_hint');

  {$ifdef OpenGLES}
  FCLAMP_TO_EDGE := GL_CLAMP_TO_EDGE;
  {$else}
  if Version_1_2 then
    FCLAMP_TO_EDGE := GL_CLAMP_TO_EDGE
  else
    FCLAMP_TO_EDGE := GL_CLAMP;
  {$endif}

  FMaxTextureSize := glGetInteger(GL_MAX_TEXTURE_SIZE);
  if GLVersion.BuggyTextureSizeAbove2048 then
    FMaxTextureSize := Min(FMaxTextureSize, 2048);

  { Querying glGetInteger(GL_MAX_LIGHTS) is deprecated in core profile,
    as it makes sense only for fixed-function.
    Just assume 8 now. }
  FMaxLights := 8;

  FTextureCubeMap := {$ifdef OpenGLES} true
    {$else} Version_1_3
    {$endif};

  if TextureCubeMap then
    FMaxCubeMapTextureSize := glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE)
  else
    FMaxCubeMapTextureSize := 0;

  { OpenGLES: Since 3.0, https://registry.khronos.org/OpenGL-Refpages/es3.0/html/glTexImage3D.xhtml .
    OpenGL: Since 1.2, https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage3D.xhtml
    (actually they don't show there OpenGL 1.x anymore) }
  FTexture3D := {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_1_2
    {$endif};

  { calculate MaxTexture3DSize, eventually correct Texture3D if buggy }
  if Texture3D then
  begin
    FMaxTexture3DSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE);
    if MaxTexture3DSize = 0 then
    begin
      FTexture3D := false;
      WritelnLog('OpenGL', 'Buggy OpenGL 3D texture support: reported as supported, but GL_MAX_3D_TEXTURE_SIZE is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    FMaxTexture3DSize := 0;

  { Both OpenGL and OpenGLES commonly define this extension.
    See https://registry.khronos.org/OpenGL/index_es.php#otherextspecs (OpenGLES list),
    https://www.khronos.org/opengl/wiki/Sampler_Object#Anisotropic_filtering
    https://www.khronos.org/opengl/wiki/Ubiquitous_Extension }
  FTextureFilterAnisotropic := HasExtension('GL_EXT_texture_filter_anisotropic');
  if TextureFilterAnisotropic then
    FMaxTextureMaxAnisotropy := glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT)
  else
    FMaxTextureMaxAnisotropy := 0.0;

  FOcclusionQuery :=
    {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_1_5
    {$endif};

  if OcclusionQuery then
  begin
    {$ifdef OpenGLES}
    FOcclusionQueryCounterBits := 32; // just use some sensible default
    {$else}
    glGetQueryiv(GL_SAMPLES_PASSED, GL_QUERY_COUNTER_BITS, @FOcclusionQueryCounterBits);
    {$endif}
  end else
    FOcclusionQueryCounterBits := 0;

  { calculate Framebuffer }
  FFramebuffer := {$ifdef OpenGLES} true // OpenGLES >= 2 includes FBO
    { Note: Do not change Load_GL_ARB_framebuffer_object to HasExtension('GL_ARB_framebuffer_object')
      here, as we need to load entry points (like glGenerateMipmap) in this case. }
    {$else} Version_3_0 {$ifdef FPC} or (LoadExtensions and Load_GL_ARB_framebuffer_object) {$endif}
    {$endif};

  if Framebuffer then
  begin
    FMaxRenderbufferSize := glGetInteger(GL_MAX_RENDERBUFFER_SIZE);
    if MaxRenderbufferSize = 0 then
    begin
      FFramebuffer := false;
      WritelnLog('OpenGL', 'Buggy OpenGL Framebuffer: reported as supported, but GL_MAX_RENDERBUFFER_SIZE is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    FMaxRenderbufferSize := 0;

  { - In case of OpenGLES: while OpenGLES 2 includes base FBO,
      only OpenGLES 3 has glBlitFramebuffer.
    - In case of OpenGL: OpenGL 3 core includes both base FBO and glBlitFramebuffer. }
  FFramebufferBlit := {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_3_0
    {$endif};

  { Calculate UseMultiTexturing.
    We used to require a couple of extensions for multitexturing:
    - EXT_texture_env_combine
    - ARB_multitexture
    - ARB_texture_env_dot3
    But GL version >= 1.3 is actually required for GL_subtract,
    and includes all above extensions in core.
    It is simpler to just depend on presense in core, and all modern OpenGLs will satisfy this. }
  FUseMultiTexturing := {$ifdef OpenGLES} true
    {$else} Version_1_3
    {$endif};

  if FUseMultiTexturing then
    FMaxTextureUnitsFixedFunction := 2
  else
    FMaxTextureUnitsFixedFunction := 1;

  FFBOMultiSampling := {$ifdef OpenGLES} false
    {$else}
    Framebuffer and
    LoadExtensions and
    Load_GL_ARB_texture_multisample and
    (not GLVersion.BuggyFBOMultiSampling)
    {$endif};

  FMultisample :=
    {$ifdef OpenGLES} true
    {$else} Version_1_3 or HasExtension('GL_ARB_multisample')
    {$endif};
  if FMultisample and (glGetInteger(GL_SAMPLE_BUFFERS) <> 0) then
  begin
    FCurrentMultiSampling := glGetInteger(GL_SAMPLES);
    if FCurrentMultiSampling <= 1 then
    begin
      WritelnWarning('MultiSampling', Format('We successfully got multi-sampling buffer, but only %d samples per pixel. This doesn''t make much sense, assuming buggy OpenGL implementation, and anti-aliasing may not work.',
        [CurrentMultiSampling]));
      FCurrentMultiSampling := 1;
    end;
  end else
    FCurrentMultiSampling := 1;

  FPackedDepthStencil :=
    {$ifdef OpenGLES} HasExtension('GL_OES_packed_depth_stencil')
    // macOS doesn't have GL_EXT_packed_depth_stencil, but FBO creation only works when using packed depth+stencil
    {$else} HasExtension('GL_EXT_packed_depth_stencil') {$ifdef DARWIN} or true {$endif}
    {$endif};

  {$ifndef OpenGLES}
  if Version_3_2 then
  begin
    { Read buffer sizes with new OpenGL core
      https://registry.khronos.org/OpenGL-Refpages/gl4/html/glGetFramebufferAttachmentParameter.xhtml
      https://stackoverflow.com/questions/27371067/how-to-query-the-depth-of-the-opengl-stencil-buffer }
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_FRONT_LEFT, GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE, @FRedBits);
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_FRONT_LEFT, GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE, @FGreenBits);
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_FRONT_LEFT, GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE, @FBlueBits);
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_FRONT_LEFT, GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE, @FAlphaBits);
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_STENCIL, GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE, @FStencilBits);
    glGetFramebufferAttachmentParameteriv(GL_DRAW_FRAMEBUFFER,
      GL_DEPTH, GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE, @FDepthBits);
    FDoubleBuffer := true; // TODO: any way to query it?
  end else
  {$endif}
  begin
    FRedBits := glGetInteger(GL_RED_BITS);
    FGreenBits := glGetInteger(GL_GREEN_BITS);
    FBlueBits := glGetInteger(GL_BLUE_BITS);
    FAlphaBits := glGetInteger(GL_ALPHA_BITS);
    FStencilBits := glGetInteger(GL_STENCIL_BITS);
    FDepthBits := glGetInteger(GL_DEPTH_BITS);
    {$ifndef OpenGLES}
    FDoubleBuffer := glGetBoolean(GL_DOUBLEBUFFER) <> GL_FALSE;
    {$else}
    FDoubleBuffer := true;
    {$endif}
  end;

  FShadowVolumesPossible :=
    (StencilBits >= 4) and
    { OpenGLES 2 always has GL_INCR/DECR_WRAP, glStencilOpSeparate:
      see bottom table at https://registry.khronos.org/OpenGL-Refpages/es3.0/html/glStencilOpSeparate.xhtml }
    {$ifdef OpenGLES} true
    {$else} Version_2_0
    {$endif};

  FTextureNonPowerOfTwo :=
    {$ifdef OpenGLES} true
    {$else} Version_2_0 or HasExtension('GL_ARB_texture_non_power_of_two')
    {$endif};

  FTextureCompression := [];

  {$ifndef OpenGLES}
  { OpenGL 1.3 introduced glCompressedTexImage2D in core.
    To keep things simple, we don't support compressed textures with older OpenGLs
    by looking at GL_ARB_texture_compression. }
  if Version_1_3 then
  {$endif}
  begin
    { See http://stackoverflow.com/questions/9148795/android-opengl-texture-compression
      and http://developer.android.com/guide/topics/graphics/opengl.html
      for possible GPU extensions for compression formats. }

    if HasExtension('GL_OES_texture_compression_S3TC') or
       HasExtension('GL_EXT_texture_compression_s3tc') or
       HasExtension('GL_NV_texture_compression_s3tc') then
      FTextureCompression := TextureCompression + [
        tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5];

    if HasExtension('GL_EXT_texture_compression_dxt1') then
      FTextureCompression := TextureCompression + [tcDxt1_RGB, tcDxt1_RGBA];
    if HasExtension('GL_EXT_texture_compression_dxt3') then
      FTextureCompression := TextureCompression + [tcDxt3];
    if HasExtension('GL_EXT_texture_compression_dxt5') then
      FTextureCompression := TextureCompression + [tcDxt5];

    if HasExtension('GL_IMG_texture_compression_pvrtc') then
      FTextureCompression := TextureCompression + [
        tcPvrtc1_4bpp_RGB,
        tcPvrtc1_2bpp_RGB,
        tcPvrtc1_4bpp_RGBA,
        tcPvrtc1_2bpp_RGBA];

    if HasExtension('GL_IMG_texture_compression_pvrtc2') then
      FTextureCompression := TextureCompression + [
        tcPvrtc2_4bpp,
        tcPvrtc2_2bpp];

    if HasExtension('GL_AMD_compressed_ATC_texture') or
       HasExtension('GL_ATI_texture_compression_atitc') then
      FTextureCompression := TextureCompression + [
        tcATITC_RGB,
        tcATITC_RGBA_ExplicitAlpha,
        tcATITC_RGBA_InterpolatedAlpha];

    if HasExtension('GL_OES_compressed_ETC1_RGB8_texture') then
      FTextureCompression := TextureCompression + [tcETC1];

    if HasExtension('GL_KHR_texture_compression_astc_ldr') or
       HasExtension('GL_KHR_texture_compression_astc_hdr') then
      FTextureCompression := TextureCompression + [
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

  FVertexBufferObject := {$ifdef OpenGLES} true
    {$else} Version_1_5 and not GLVersion.BuggyVBO
    {$endif};

  FBlendConstant := {$ifdef OpenGLES} true
    {$else}
    { OpenGL 1.4 has GL_CONSTANT_ALPHA, glBlendColor is available since 1.2. }
    Version_1_4
    {$endif};

  FTextureFloat :=
    {$ifdef OpenGLES} false
    {$else} HasExtension('GL_ATI_texture_float') or HasExtension('GL_ARB_texture_float')
    {$endif};

  FTextureDepth :=
    {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_1_4 or HasExtension('GL_ARB_depth_texture')
    {$endif};

  { ARB_shadow defines no new functions, so we don't need to load using this:
      LoadExtensions and Load_GL_ARB_shadow;
    and therefore it will work also with modern OpenGL where Load_GL_ARB_shadow is broken in GLExt. }
  FTextureDepthCompare :=
    { Note that we don't look at GL_EXT_shadow_samplers, we need GLSL "300 es" anyway
      to access shadow samplers from GLSL. }
    {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_1_4 or HasExtension('GL_ARB_shadow')
    {$endif};

  if FTextureDepthCompare and (not FTextureDepth) then
  begin
    WritelnWarning('OpenGL(ES) feature TextureDepthCompare is not possible without TextureDepth, fixing');
    FTextureDepthCompare := false;
  end;

  FShaders := {$ifdef OpenGLES} true
    {$else} Version_2_0
    {$endif};

  FEnableFixedFunction :=
    {$ifdef OpenGLES} false
    {$else}
    (not UseMultiTexturing) or
    (not Shaders) or
    // checking for VertexBufferObject also makes sure that BuggyVBO = false
    (not VertexBufferObject) or
    GLVersion.BuggyPureShaderPipeline
    {$endif};

  { Right now, EnableFixedFunction is actually synchronized with Shaders and VertexBufferObject.
    When not EnableFixedFunction, everything should fall to rendering using ancient way. }
  if EnableFixedFunction then
  begin
    FShaders := false;
    FVertexBufferObject := false;
  end;

  // RequestCapabilities = rcForceFixedFunction overrides above detection
  if RequestCapabilities = rcForceFixedFunction then
  begin
    {$ifndef OpenGLES}
    if Version_3_1 and not HasExtension('GL_ARB_compatibility') then
      raise Exception.Create('Requested deprecated fixed-function approach, but this is a modern context without compatibility (ARB_compatibility). Cannot use fixed-function.');
    {$endif}

    FEnableFixedFunction := true;
    FShaders := false;
    FVertexBufferObject := false;
  end;

  Assert(EnableFixedFunction = (not Shaders));
  Assert(EnableFixedFunction = (not VertexBufferObject));

  { We do not look at GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS,
    which is usually much smaller (may even be 0).
    But most of our texture uses do not access the texture from the vertex shader,
    only fragment shader.

    Note: we read this only if shaders are supported (OpenGL 2.0),
    otherwise GL_MAX_TEXTURE_IMAGE_UNITS is invalid enum for older OpenGL. }
  if Shaders then
    FMaxTextureUnitsShaders := glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS);

  FVertexArrayObject := {$ifdef OpenGLES} false {$else} (not EnableFixedFunction) and Version_3_0 {$endif};

  { Calculate MaxClipPlanes.
    The logic follows various approaches to clip planes in TShader.EnableClipPlane. }
  {$ifndef OpenGLES}
  if EnableFixedFunction or (not Version_3_0) then
    FMaxClipPlanes := glGetInteger(GL_MAX_CLIP_PLANES)
  else
    FMaxClipPlanes := glGetInteger(GL_MAX_CLIP_DISTANCES);
  {$else}
  { Any value is in theory correct for OpenGLES.
    The only limit is number of varying variables we can pass from vertex
    to fragment shader, and fragment code speed.
    With OpenGLES 2, the clipping requires
    passing data to the fragment shader, and discarding fragments there.

    We set 8, as it seems reasonable, and is the minimal guaranteed value
    of GL_MAX_CLIP_DISTANCES. }
  FMaxClipPlanes := 8;
  {$endif}

  FMaxViewportDimensions := GetVector2Integer(GL_MAX_VIEWPORT_DIMS);

  FTextureModernFormatsAndSwizzle :=
    (RequestCapabilities <> rcForceFixedFunction) and
    (
      {$ifndef OpenGLES} Version_3_3 or {$endif}
      HasExtension('GL_ARB_texture_swizzle') or
      HasExtension('GL_EXT_texture_swizzle')
    );

  {$ifndef OpenGLES}
  // set LineWidth to some reasonable default when it cannot be queried on old OpenGL
  if not Version_2_0 then
    FLineWidth := Vector2(1, 8)
  else
  {$endif}
  begin
    Assert(SizeOf(TGLFloat) = SizeOf(Single));
    glGetFloatv(GL_ALIASED_LINE_WIDTH_RANGE, @FLineWidth);
  end;

  FTextureMaxLevel :=
    {$ifdef OpenGLES} VersionES_3_0
    {$else} Version_1_2
    {$endif};

  // set GLFeatures singleton, TGLMemoryInfo.Create and TGLMemoryInfo.Refresh depend on it
  GLFeatures := Self;

  FMemory := TGLMemoryInfo.Create;

  if (RequestCapabilities = rcForceModern) and EnableFixedFunction then
    raise Exception.Create('Requested modern context, but it was detected that we have to use fixed-function. We failed to really get modern context.');

  CheckGLErrors('End of TGLFeatures.Create');
end;

destructor TGLFeatures.Destroy;
begin
  FreeAndNil(FMemory);
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

const
  SCannotChangeAfterContextInitialized = 'You cannot change "%s" once rendering context is already initialized.' + NL +
    NL +
    'You can only change it before TCastleWindow.Open and before TCastleApplication.OnInitialize. In usual applications, put it in "initialization" of unit like GameInitialize that creates TCastleWindow.';

class procedure TGLFeatures.SetRequestCapabilities(const Value: TGLRequestCapabilities);
begin
  if FRequestCapabilities <> Value then
  begin
    { Reason why you cannot change it after context is initialized:

      1. Cannot change to rcForceFixedFunction:

         Otherwise creating OpenGL context could
         initialize early some resources for modern pipeline,
         e.g. RenderLoadingBackground would initialize TDrawableImage shaders.
         Setting EnableFixedFunction to true later, and disabling shaders,
         would put us in a weird state -- some TDrawableImage
         shader could be enabled,
         and it couldn't be disabled (because InternalSetCurrentProgram
         now thinks that shaders are not available).

         It is more reliable to guarantee that EnableFixedFunction
         is constant once OpenGL(ES) context is open.
         So e.g. if EnableFixedFunction=true, you can be 100% certain
         that no shaders are enabled, because they are disabled from the start.

      2. Cannot change to rcForceModern:

         Naturally, rcForceModern is applied at context creation,
         so it would be useless to set it later.
    }
    if GLFeatures <> nil then
      raise Exception.CreateFmt(SCannotChangeAfterContextInitialized, ['TGLFeatures.SetRequestCapabilities']);
    FRequestCapabilities := Value;
  end;
end;

class procedure TGLFeatures.SetDebug(const Value: Boolean);
begin
  if FDebug <> Value then
  begin
    if GLFeatures <> nil then
      raise Exception.CreateFmt(SCannotChangeAfterContextInitialized, ['TGLFeatures.Debug']);
    FDebug := Value;
  end;
end;

{ TGLMemoryInfo -------------------------------------------------------------- }

constructor TGLMemoryInfo.Create;
begin
  inherited;
  ExtensionNVidia := GLFeatures.HasExtension('GL_NVX_gpu_memory_info');
  ExtensionATI := GLFeatures.HasExtension('GL_ATI_meminfo');
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

    if GLFeatures.Framebuffer then
    begin
      glGetIntegerv(GL_RENDERBUFFER_FREE_MEMORY_ATI, @FreeMemory);
      CurrentRenderbuffer := FreeMemory[0];
    end;
  end;
end;

function TGLMemoryInfo.LessTextureMemoryThan(const RequiredMemoryKb: Integer): Boolean;
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

{ global routines --------------------------------------------------------------------- }

function StrToCapabilities(const S: String): TGLRequestCapabilities;
begin
  for Result := Low(Result) to High(Result) do
    if SameText(CapabilitiesStr[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid capabilities name "%s"', [S]);
end;

{$endif read_implementation}
