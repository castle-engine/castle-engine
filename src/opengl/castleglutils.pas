{
  Copyright 2001-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various low-level utilities for working with OpenGL. }
unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses Math, CastleGL, SysUtils, CastleUtils, CastleVectors, CastleBoxes,
  CastleImages, Matrix, CastleRectangles, CastleColors;

{$define read_interface}

type
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

type
  { Types with leading "T" } { }
  TGLenum     = GLenum;
  TGLboolean  = GLboolean;
  TGLbitfield = GLbitfield;
  TGLbyte     = GLbyte;
  TGLshort    = GLshort;
  TGLint      = GLint;
  TGLsizei    = GLsizei;
  TGLubyte    = GLubyte;
  TGLushort   = GLushort;
  TGLuint     = GLuint;
  TGLfloat    = GLfloat;
  TGLclampf   = GLclampf;
  {$ifndef OpenGLES}
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;
  {$endif not OpenGLES}

type
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

    { Number of texture units available.
      Equal to glGetInteger(GL_MAX_TEXTURE_UNITS), if multi-texturing
      available. Equal to 1 (OpenGL supports always 1 texture) otherwise. }
    MaxTextureUnits: Cardinal;

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
      Contrast this with TCastleWindowCustom.MultiSampling or TOpenGLControl.MultiSampling,
      that say @italic(how many samples you wanted to get). }
    CurrentMultiSampling: Cardinal;

    { Does OpenGL context have depth buffer packed with stencil buffer.
      See EXT_packed_depth_stencil extension for explanation.

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

    { Texture S3TC compression support. This means you can load textures by
      glCompressedTexImage2DARB and use GL_COMPRESSED_*_S3TC_*_EXT enums. }
    TextureCompressionS3TC: boolean;

    { VBO support (in OpenGL (ES) core). }
    VertexBufferObject: boolean;

    { glBlendColor and GL_CONSTANT_ALPHA support. }
    BlendConstant: boolean;

    { Support for float texture formats for glTexImage2d. }
    TextureFloat: boolean;

    { Support for depth texture formats for glTexImage2d. }
    TextureDepth: boolean;

    constructor Create;
  end;

{ Initialize GLVersion and GLUVersion and GLFeatures. }
procedure GLInformationInitialize;

var
  GLFeatures: TGLFeatures;

{ OpenGL vector/matrix types ------------------------------------------------- }

type
  { }
  TVector2f = TVector2Single;   PVector2f = PVector2Single;
  TVector2d = TVector2Double;   PVector2d = PVector2Double;
  TVector2ub = TVector2Byte;    PVector2ub = PVector2Byte;
  TVector2i = TVector2LongInt;  PVector2i = PVector2LongInt;

  TVector3f = TVector3Single;   PVector3f = PVector3Single;
  TVector3d = TVector3Double;   PVector3d = PVector3Double;
  TVector3ub = TVector3Byte;    PVector3ub = PVector3Byte;
  TVector3i = TVector3LongInt;  PVector3i = PVector3LongInt;

  TVector4f = TVector4Single;   PVector4f = PVector4Single;
  TVector4d = TVector4Double;   PVector4d = PVector4Double;
  TVector4ub = TVector4Byte;    PVector4ub = PVector4Byte;
  TVector4i = TVector4LongInt;  PVector4i = PVector4LongInt;

  TMatrix2f = TMatrix2Single;  PMatrix2f = PMatrix2Single;
  TMatrix2d = TMatrix2Double;  PMatrix2d = PMatrix2Double;

  TMatrix3f = TMatrix3Single;  PMatrix3f = PMatrix3Single;
  TMatrix3d = TMatrix3Double;  PMatrix3d = PMatrix3Double;

  TMatrix4f = TMatrix4Single;  PMatrix4f = PMatrix4Single;
  TMatrix4d = TMatrix4Double;  PMatrix4d = PMatrix4Double;

{ OpenGL error checking ------------------------------------------------------ }

type
  { OpenGL error. Usually indicates a bug in your code (or shader code,
    depending on TUniformNotFoundAction and TUniformTypeMismatchAction;
    by default, they do not cause errors).

    When programming for platforms with limited GPU memory (Android, iOS...)
    you should prepare to handle EOpenGLOutOfMemoryError (corresponding to
    GL_OUT_OF_MEMORY). This can always happen for large GPU data,
    and you should be prepared to capture it (at least around TGameSceneManager.LoadLevel)
    and display some nice information for user.
    Alternatively, you can leave GLOutOfMemoryError = @false,
    and then EOpenGLOutOfMemoryError will not happen, but you risk all kinds
    of rendering artifacts. }
  EOpenGLError = class(Exception)
  public
    ErrorCode: TGLenum;
    constructor Create(const AErrorCode: TGLenum; const AdditionalComment: string = '');
  end;

  EOpenGLOutOfMemoryError = class(EOpenGLError)
  end;

var
  { When GPU runs out of memory, raise exception (EOpenGLOutOfMemoryError)
    or merely make a warning. Merely making a warning is very risky (you risk all kinds
    of rendering artifacts), but sometimes the rendering is actually smooth
    even though GPU complains. }
  GLOutOfMemoryError: boolean = false;

procedure GLOutOfMemory(const AdditionalComment: string = '');

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string = ''): string;

{ Check are any OpenGL errors recorded (in glGetError).
  If there are errors, our behavior depends on whether we were compiled
  with -dRELEASE. With -dRELEASE, we make OnWarning. This way eventual
  errors in release builds don't completely abort your program.

  Note that the behavior on GL_OUT_OF_MEMORY is different.
  -dRELEASE does not matter here. Only GLOutOfMemoryError boolean dictates
  if we should raise an exception or merely make warning, regardless of -dRELEASE
  state.

  Without -dRELEASE, we raise EOpenGLError. So a developer is strongly
  suggested to fix the code to not produce OpenGL errors, never ever.

  @raises EOpenGLError }
procedure CheckGLErrors(const AdditionalComment: string = '');

{ Raise EOpenGLError for given OpenGL error code.
  This has calling convention suitable for registering this procedure
  as GLU_TESS_ERROR for gluTessCallback, or GLU_ERROR for gluQuadricCallback. }
procedure GLErrorRaise(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}

{ ---------------------------------------------------------------------------- }

{ Comfortable wrappers for OpenGL glGet* that return a single value.

  Guarantee that result is zero in case of OpenGL error.
  (Otherwise, OpenGL could leave them undefined and only set glGetError.)
  @groupBegin }
function glGetFloat(pname: TGLEnum): TGLfloat;
function glGetInteger(pname: TGLEnum): TGLint;
function glGetBoolean(pname: TGLEnum): TGLboolean;
{ @groupEnd }

{ ------------------------------------------------------------------------------
  Comfortable wrappers around many OpenGL functions.
  Overloaded for our vector types.

  Note that functions here simply call appropriate OpenGL functions.
  Long time ago we tried using tricks to speed this up (eliminate
  function call overhead), by importing these functions from so/dll
  under different names, like

    procedure glVertexv(const V: TVector3Single); OPENGL_CALL overload; external OpenGLDLL name 'glVertex3fv';

  But this is problematic: it assumes that TVector3Single will be passed
  by reference. Which actually is not guaranteed by a compiler (FPC sometimes
  doesn't). In newer FPC versions, we could use "constref" for this.
  Or we could just declare these functions as "inline".
  However, speeding these functions is just not needed at all anymore
  (especially with new VBO renderer that passes everything through arrays).

  TODO: almost all these functions belong to deprecated fixed-function
  and do immediate operations. They should be just removed, and everything
  using them fixed to use VBO.
}

{$ifndef OpenGLES}
{ }
procedure glColorv(const v: TVector3ub); overload;
procedure glColorv(const v: TVector4ub); overload;
procedure glColorv(const v: TVector3f); overload;
procedure glColorv(const v: TVector4f); overload;

procedure glTranslatev(const V: TVector3f); overload;
procedure glTranslatev(const V: TVector3_Single); overload;

procedure glScalev(const V: Single); overload;
procedure glScalev(const V: TVector3f); overload;
procedure glScalev(const V: TVector3_Single); overload;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); overload;

procedure glClipPlane(plane: GLenum; const V: TVector4d); overload;

procedure glNormalv(const v: TVector3f); overload;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f); overload;

procedure glVertexv(const v: TVector2f); overload;
procedure glVertexv(const v: TVector2i); overload;
procedure glVertexv(const v: TVector3f); overload;
procedure glVertexv(const v: TVector3i); overload;
procedure glVertexv(const v: TVector4f); overload;
procedure glVertexv(const v: TVector4i); overload;

procedure glVertexv(const v: TVector2_Single); overload;
procedure glVertexv(const v: TVector3_Single); overload;
procedure glVertexv(const v: TVector4_Single); overload;

procedure glTexCoordv(const v: TVector2f); overload;
procedure glTexCoordv(const v: TVector3f); overload;
procedure glTexCoordv(const v: TVector4f); overload;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f); overload;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f); overload;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); overload;

procedure glFogv(pname: TGLEnum; const params: TVector4f); overload;

procedure glMultMatrix(const m: TMatrix4f); overload;
procedure glLoadMatrix(const m: TMatrix4f); overload;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); overload;

{$endif}

procedure GLViewport(const Rect: TRectangle);

function GetCurrentColor: TCastleColor;
procedure SetCurrentColor(const Value: TCastleColor);

{ Current color, set by glColorv and used for TCastleFont font printing
  (in case you use deprecated TCastleFont.Print overloads without
  explicit colors).
  You should not depend on this in new programs, rather use TCastleFont.Print
  with explicit Color parameter. }
property CurrentColor: TCastleColor read GetCurrentColor write SetCurrentColor;

{ Simple save/restore of OpenGL pixel store ---------------------------------- }

type
  TUnpackNotAlignedData = record
    Alignment: Cardinal;
  end;
  TPackNotAlignedData = TUnpackNotAlignedData;

{ Save/restore OpenGL pixel store for unpacking TRGBImage.
  Before you pass an TRGBImage to glDrawPixels, glTexImage1D, glTexImage2D,
  glBitmap, glPolygonStipple and such, call
  BeforeUnpackNotAlignedRGBImage, and later call
  AfterUnpackNotAlignedRGBImage to restore original state.
  @groupBegin }
procedure BeforeUnpackNotAlignedRGBImage(out unpackdata: TUnpackNotAlignedData; imageWidth: cardinal);
procedure AfterUnpackNotAlignedRGBImage(const unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
{ @groupEnd }

{ Save/restore OpenGL pixel store for unpacking / packing given TCastleImage.
  Before you pass this image to some OpenGL procedures
  (like glDrawPixels for unpacking, glReadPixels for packing),
  call BeforeXxx, and later call AfterXxx to restore original state.
  These will take care of setting/restoring pixel alignment.
  @groupBegin }
procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
{ @groupEnd }

{ Projection matrix -------------------------------------------------------- }

function GetProjectionMatrix: TMatrix4Single;
procedure SetProjectionMatrix(const Value: TMatrix4Single);

{ Current projection matrix.

  For OpenGLES, this is merely a global ProjectionMatrix variable.
  It must be passed to various shaders to honour the projection.

  For desktop OpenGL, setting this also sets fixed-function projection matrix.
  The OpenGL matrix mode is temporarily changed to GL_PROJECTION,
  then changed back to GL_MODELVIEW. }
property ProjectionMatrix: TMatrix4Single
  read GetProjectionMatrix write SetProjectionMatrix;

{ Set ProjectionMatrix to perspective or orthogonal.

  For PerspectiveProjection, ZFar may have special ZFarInfinity value
  to create a perspective projection with far plane set at infinity.
  Useful e.g. for z-fail shadow volumes.

  @groupBegin }
function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
function OrthoProjection(const left, right, bottom, top: Single;
  const zNear: Single = -1; const zFar: Single = 1): TMatrix4Single;
{ @groupEnd }

var
  { Viewport size for 2D rendering functions: DrawRectangle and TGLImage.Draw.
    UI container (like TCastleWindowCustom or TCastleControlCustom)
    must take care to set this before rendering. }
  Viewport2DSize: TVector2Single;

{ ---------------------------------------------------------------------------- }

{ }
procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);

{$ifndef OpenGLES}

{ Draw vertical line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use UI interface drawing like Theme.Draw and TGLImage.Draw. }
procedure GLVerticalLine(x, y1, y2: TGLfloat); deprecated;

{ Draw horizontal line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use UI interface drawing like Theme.Draw and TGLImage.Draw. }
procedure GLHorizontalLine(x1, x2, y: TGLfloat); deprecated;

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).

  Everything is drawn CCW when seen from standard view (x grows right, y up).
  Uses current OpenGL color. }
procedure GLDrawArrow(HeadThickness: TGLfloat = 0.4;
  HeadLength: TGLfloat = 0.5);

{ Comfortable wrapper for gluNewQuadric. Sets all quadric parameters.
  Sets also the GLU_ERROR callback to ReportGLerror.
  @raises Exception If gluNewQuadric fails (returns nil). }
function NewGLUQuadric(
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL): PGLUQuadric; overload;

{ Render sphere in OpenGL. Radius, Slices, Stacks have the same meaning
  as for gluSphere (in case they are not self-explanatory...).
  Other parameters set glu quadric parameters, see glu quadric documentation. }
procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL);

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3Single; Size: Single);

{ Draw corner markers (3 lines) at the 8 corners of the box.
  Proportion is the fraction of the box length, the marker extends too. }
procedure glDrawCornerMarkers(const Box: TBox3D; const Proportion: Single = 0.1);

{ Draw the wireframe box.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawBox3DWire(const Box: TBox3D);

{ Draw simple box. Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. Order is CCW outside
  (so if you want, you can turn on backface culling yourself).

  You @bold(must enable GL_VERTEX_ARRAY before using this).
  (It's not done automatically, as it's much faster to do it once
  for many glDrawBox3DSimple calls. Example --- bzwgen city view behind
  building 1, with occlusion query used: FPC 150 vs 110 when
  GL_VERTEX_ARRAY is activated once in OcclusionBoxStateBegin, not here.
  Tested on fglrx on Radeon X1600 (chantal).)

  It can be safely placed in a display list. }
procedure glDrawBox3DSimple(const Box: TBox3D);

{ Call glColor, taking Opacity as separate Single argument.
  Deprecated, do not use colors like that, instead pass TCastleColor
  to appropriate routines like TCastleFont.Print.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single); deprecated;
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single); deprecated;
{ @groupEnd }
{$endif}

{ Draw a rectangle that modulates colors underneath,
  suddenly changing it to FadeColor and then fading to blackness and
  then fading back to normal, as FadeIntensity goes down from 1.0 to 0.0.
  This is nice to use for a screen effect when player is hurt.

  Only RGB portion of FadeColor is used. }
procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single;
  const FadeIntensity: Single); deprecated;
procedure GLFadeRectangle(const Rect: TRectangle;
  const FadeColor: TCastleColor;
  const FadeIntensity: Single);

{ Draw a rectangle with blending.
  @deprecated Deprecated, use DrawRectangle instead. }
procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor, DestinationFactor: TGLenum;
  const Color: TVector4Single); deprecated;
procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4Single); deprecated;

{ Draw a simple rectangle filled with a color.
  Blending is automatically used if Color alpha < 1.

  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TGLEnum = GL_SRC_ALPHA;
  const BlendingDestinationFactor: TGLEnum = GL_ONE_MINUS_SRC_ALPHA;
  const ForceBlending: boolean = false);

{ Multiline string describing attributes of current OpenGL
  library. This simply queries OpenGL using glGet* functions
  about many things. Does not change OpenGL state in any way.

  Note that the last line of returned string does not terminate
  with a newline character (so e.g. you may want to do
  Writeln(GLInformationString) instead of just Write(GLInformationString)). }
function GLInformationString: string;

{ Utilities for display lists ---------------------------------------- }
{ Deprecated: all display list usage will be removed, since it doesn't
  exist in modern OpenGL and OpenGL ES. }

{$ifndef OpenGLES}

type
  { }
  EOpenGLNoMoreDisplayLists = class(Exception)
  end;

{ Call glGenLists(range) and checks the result.

  @raises(EOpenGLNoMoreDisplayLists
    When glGenLists(Range) returned zero for non-zero Range.
    The exception's Message shows Place, which may describe
    where this is called --- makes it easier to debug.) }
function glGenListsCheck(range: TGLsizei; const Place: string): TGLuint;

{ If List <> 0 then it does glDeleteList on List and sets List to 0.
  In other words this is simply glDeleteList but
  @orderedList(
    @item only if List really should be deleted
    @item sets List to 0 after deletion
  ) }
procedure glFreeDisplayList(var list: TGLuint);

{$endif}

{ If Buffer <> 0 then it does glDeleteBuffers and sets Buffer to 0. }
procedure glFreeBuffer(var Buffer: TGLuint);

{ Set color and depth buffers writeable or not.
  This is just a shortcut for
  @longcode(#
    glDepthMask(Writeable);
    glColorMask(Writeable, Writeable, Writeable, Writeable);
  #) }
procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);

{ Draw the 2D GUI stuff (like following GUI images and TCastleFont)
  with lower-left corner in the X,Y pixel.
  It's not adviced to use this, better use TGLImage.Draw(X,Y)
  or TCastleFont.Print(X,Y,string) methods.
  @groupBegin }
procedure SetWindowPos(const X, Y: TGLint);
procedure SetWindowPos(const Value: TVector2i);
procedure SetWindowPosF(const X, Y: TGLfloat); deprecated;
procedure SetWindowPosZero; deprecated;
{ @groupEnd }

function GetWindowPos: TVector2i;
property WindowPos: TVector2i read GetWindowPos write SetWindowPos;

type
  TDepthRange = (drFull, drNear, drFar);

function GetDepthRange: TDepthRange;
procedure SetDepthRange(const Value: TDepthRange);

{ Use this to operate on OpenGL glDepthRange. For now, our engine has
  very simple use for this, for TPlayer.RenderOnTop. }
property DepthRange: TDepthRange read GetDepthRange write SetDepthRange;

type
  TEnableTextureTarget = (etNone, et2D, etCubeMap, et3D);

{ Enable exactly one (or none, for Target=etNone) OpenGL texture target.
  Always use this instead of manually calling @code(glDisable(GL_TEXTURE_2D)),
  @code(glEnable(GL_TEXTURE_2D)) and such. This makes sure to have at most
  one texture target enabled, and disable others.

  Remember that this state is different for every texture unit in OpenGL,
  in case you use multi-texturing.

  Remember that not all texture targets are guaranteed to be supported by OpenGL.
  Target=etNone and Target=et2D are always supported.
  For the rest, check appropriate GLFeatures property (before even
  creating a texture with such type). If you pass an unsupported target type
  to this procedure, it will be ignored (all targets will be disabled,
  like for Target=etNone).

  Note that this is only for fixed-function OpenGL pipeline.
  Shader pipeline completely ignores the enabled state of texture units. }
function GLEnableTexture(const Target: TEnableTextureTarget): boolean;

type
  TClearBuffer = (cbColor, cbDepth, cbStencil);
  TClearBuffers = set of TClearBuffer;

{ Clear OpenGL buffer contents.
  Never call OpenGL glClear or glClearColor, always use this procedure. }
procedure GLClear(const Buffers: TClearBuffers;
  const ClearColor: TCastleColor);

{ Enable or disable scissor.
  Always do it using these procedures, do not call glScissor or
  glEnable(GL_SCISSOR_TEST) / glDisable(GL_SCISSOR_TEST) yourself,
  or push/pop attrib. }
procedure ScissorEnable(const Rect: TRectangle);
procedure ScissorDisable;

function GetGlobalAmbient: TVector3Single;
procedure SetGlobalAmbient(const Value: TVector3Single);

{ Global ambient lighting. This is added to every 3D object color,
  multiplied by material ambient.

  The default value is (0.2, 0.2, 0.2). It matches default
  GL_LIGHT_MODEL_AMBIENT in fixed-function OpenGL.
  It also matches the required value of VRML 1.0 specification.
  For VRML 2.0 / X3D, lighting equations suggest that it should be zero. }
property GlobalAmbient: TVector3Single
  read GetGlobalAmbient write SetGlobalAmbient;

{$undef read_interface}

implementation

{$define read_implementation}

uses CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders,
  CastleGLImages, CastleLog, CastleWarnings, CastleUIControls;

procedure GLInformationInitialize;
begin
  FreeAndNil(GLVersion);
  GLVersion := TGLVersion.Create(PChar(glGetString(GL_VERSION)),
    PChar(glGetString(GL_VENDOR)), PChar(glGetString(GL_RENDERER)));

  {$ifndef OpenGLES}
  FreeAndNil(GLUVersion);
  { gluGetString is valid for version 1.1 or later }
  if Assigned(gluGetString) then
    GLUVersion := TGenericGLVersion.Create(gluGetString(GLU_VERSION)) else
    GLUVersion := TGenericGLVersion.Create('1.0');
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
    {$warnings off} { Stop warning about unportable PtrUInt convertions }
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
{$ifdef OpenGLES}
var
  GLESExtensions: string;
{$endif}
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

  { We want to be able to render any scene --- so we have to be prepared
    that fog interpolation has to be corrected for perspective.
    TODO: this should be moved elsewhere, but where? }
  glHint(GL_FOG_HINT, GL_NICEST);
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

  {$ifdef OpenGLES}
  MaxTextureUnits := Max(
    glGetInteger(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS),
    glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS));
  {$else}
  if Version_1_3 then
    MaxTextureUnits := glGetInteger(GL_MAX_TEXTURE_UNITS) else
    MaxTextureUnits := 1;
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
    if Log then WritelnLog('OpenGL', 'Buggy OpenGL 3D texture support: reported as supported, but GL_MAX_3D_TEXTURE_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
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
      if Log then WritelnLog('OpenGL', 'Buggy OpenGL Framebuffer: reported as supported, but GL_MAX_RENDERBUFFER_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    MaxRenderbufferSize := 0;

  { TODO: this should be completely configurable in shader pipeline.
    Make it configurable at Attributes.MaxClipPlanes? }
  MaxClipPlanes := {$ifdef OpenGLES} 8 {$else} glGetInteger(GL_MAX_CLIP_PLANES) {$endif};

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
      OnWarning(wtMinor, 'MultiSampling', Format('We successfully got multi-sampling buffer, but only %d samples per pixel. This doesn''t make much sense, assuming buggy OpenGL implementation, and anti-aliasing may not work.',
        [CurrentMultiSampling]));
      CurrentMultiSampling := 1;
    end;
  end else
    CurrentMultiSampling := 1;

  {$ifdef OpenGLES}
  GLESExtensions := Pchar(glGetString(GL_EXTENSIONS));
  {$endif}

  PackedDepthStencil :=
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_packed_depth_stencil', GLESExtensions)
    {$else} Load_GL_EXT_packed_depth_stencil
    {$endif};

  ShadowVolumesPossible := glGetInteger(GL_STENCIL_BITS) >= 4;

  TextureNonPowerOfTwo := {$ifdef OpenGLES} true {$else}
    Load_GL_ARB_texture_non_power_of_two or Version_2_0 {$endif};

  TextureCompressionS3TC := {$ifdef OpenGLES} false {$else}
    Load_GL_ARB_texture_compression and Load_GL_EXT_texture_compression_s3tc {$endif};

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
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_depth_texture', GLESExtensions)
    {$else} Load_GL_ARB_depth_texture
    {$endif};
end;

{ EOpenGLError, CheckGLErrors ------------------------------------------------ }

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string): string;
var
  S: string;
begin
  { Do not use gluErrorString, not available in OpenGL ES.
    Error decriptions below from
    http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetError.xml }
  case ErrorCode of
    GL_NO_ERROR: S := 'No error has been recorded.';
    GL_INVALID_ENUM: S := 'An unacceptable value is specified for an enumerated argument.';
    GL_INVALID_VALUE: S := 'A numeric argument is out of range.';
    GL_INVALID_OPERATION: S := 'The specified operation is not allowed in the current state.';
    GL_INVALID_FRAMEBUFFER_OPERATION: S := 'The command is trying to render to or read from the framebuffer while the currently bound framebuffer is not framebuffer complete (i.e. the return value from glCheckFramebufferStatus is not GL_FRAMEBUFFER_COMPLETE).';
    GL_OUT_OF_MEMORY: S := 'There is not enough memory left to execute the command.';
    else S := 'Unknown error.';
  end;

  if AdditionalComment <> '' then
    Result := AdditionalComment + nl else
    Result := '';
  Result += Format('OpenGL error (%d): %s', [ErrorCode, S]);
end;

constructor EOpenGLError.Create(const AErrorCode: TGLenum; const AdditionalComment: string);
begin
  ErrorCode := AErrorCode;
  inherited Create(GLErrorString(ErrorCode, AdditionalComment));
end;

procedure GLOutOfMemory(const AdditionalComment: string);
const
  ErrorCode = GL_OUT_OF_MEMORY;
begin
  if GLOutOfMemoryError then
    raise EOpenGLOutOfMemoryError.Create(ErrorCode, AdditionalComment) else
    OnWarning(wtMajor, 'OpenGL', GLErrorString(ErrorCode, AdditionalComment));
end;

procedure CheckGLErrors(const AdditionalComment: string);
var
  ErrorCode: TGLenum;
begin
  ErrorCode := glGetError();
  if ErrorCode <> GL_NO_ERROR then
  begin
    if ErrorCode = GL_OUT_OF_MEMORY then
      GLOutOfMemory(AdditionalComment) else
      {$ifdef RELEASE}
      OnWarning(wtMajor, 'OpenGL', GLErrorString(ErrorCode, AdditionalComment));
      {$else}
      raise EOpenGLError.Create(ErrorCode, AdditionalComment);
      {$endif}
  end;
end;

procedure GLErrorRaise(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}
begin
  if ErrorCode = GL_OUT_OF_MEMORY then
    GLOutOfMemory else
    raise EOpenGLError.Create(ErrorCode);
end;

{ usprawnienia glGet ---------------------------------------------------------------------}

function glGetFloat(pname: TGLEnum): TGLfloat;
begin
  FillChar(result, SizeOf(result), 0);
  glGetFloatv(pname, @result)
end;

function glGetInteger(pname: TGLEnum): TGLint;
begin
  { Just for test, to somewhat simulate hardware with lower
    GL_MAX_TEXTURE_UNITS_ARB,
  if pname = GL_MAX_TEXTURE_UNITS_ARB then
  begin
    Result := 2;
    Exit;
  end;}

  FillChar(result, SizeOf(result), 0);
  glGetIntegerv(pname, @result);
end;

function glGetBoolean(pname: TGLEnum): TGLboolean;
begin
  FillChar(result, SizeOf(result), 0);
  glGetBooleanv(pname, @result)
end;

{ ---------------------------------------------------- }

var
  FCurrentColor: TCastleColor;

function GetCurrentColor: TCastleColor;
begin
  Result := FCurrentColor;
end;

procedure SetCurrentColor(const Value: TCastleColor);
begin
  FCurrentColor := Value;
end;

{$ifndef OpenGLES}

procedure glColorv(const v: TVector3ub);
begin
  glColor3ubv(@v);
  FCurrentColor := Vector4Single(Vector4Byte(V, 255));
end;

procedure glColorv(const v: TVector4ub);
begin
  glColor4ubv(@v);
  FCurrentColor := Vector4Single(V);
end;

procedure glColorv(const v: TVector3f);
begin
  glColor3fv(@v);
  FCurrentColor := Vector4Single(V);
end;

procedure glColorv(const v: TVector4f);
begin
  glColor4fv(@v);
  FCurrentColor := V;
end;

procedure glTranslatev(const V: TVector3f); begin glTranslatef(V[0], V[1], V[2]); end;

procedure glTranslatev(const V: TVector3_Single); begin glTranslatef(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glScalev(const V: Single); begin glScalef(V, V, V); end;

procedure glScalev(const V: TVector3f); begin glScalef(V[0], V[1], V[2]); end;

procedure glScalev(const V: TVector3_Single); begin glScalef(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); begin glRotatef(Angle, V[0], V[1], V[2]); end;

procedure glVertexv(const v: TVector2_Single);  begin glVertex2fv(@v.Data); end;

procedure glVertexv(const v: TVector3_Single);  begin glVertex3fv(@v.Data); end;

procedure glVertexv(const v: TVector4_Single);  begin glVertex4fv(@v.Data); end;

procedure glClipPlane(plane: GLenum; const V: TVector4d);
begin
  GL.glClipPlane(plane, @V);
end;

procedure glNormalv(const v: TVector3d); begin glNormal3dv(@v); end;
procedure glNormalv(const v: TVector3f); begin glNormal3fv(@v); end;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f);  begin glMaterialfv(face, pname, @params); end;

procedure glVertexv(const v: TVector2f);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector2i);  begin glVertex2iv(@v); end;
procedure glVertexv(const v: TVector3f);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector3i);  begin glVertex3iv(@v); end;
procedure glVertexv(const v: TVector4f);  begin glVertex4fv(@v); end;
procedure glVertexv(const v: TVector4i);  begin glVertex4iv(@v); end;

procedure glTexCoordv(const v: TVector2f);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector3f);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector4f);  begin glTexCoord4fv(@v); end;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f);  begin glTexGenfv(coord, pname, @params); end;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f);  begin glLightfv(light, pname, @params); end;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); begin glLightModelfv(pname, @params); end;
procedure glLightModelv(pname: TGLenum; const params: TVector4i); begin glLightModeliv(pname, @params); end;

procedure glFogv(pname: TGLEnum; const params: TVector4f);  begin glFogfv(pname, @params); end;

procedure glMultMatrix(const m: TMatrix4f); begin glMultMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4f); begin glLoadMatrixf(@m) end;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); begin glTexEnvfv(target, pname, @params); end;

{$endif}

procedure GLViewport(const Rect: TRectangle);
begin
  {$ifndef OpenGLES} GL {$else} CastleGLES20 {$endif}
    .glViewport(Rect.Left, Rect.Bottom, Rect.Width, Rect.Height);
end;

{ uproszczenia dla sejwowania / ladowania gl state : ---------------------------------- }

procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if unpackData.Alignment = 0 then
    raise Exception.Create('OpenGL context is probably not initialized yet: glGetInteger(GL_UNPACK_ALIGNMENT) returned 0');
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, unpackData.Alignment);
end;

procedure BeforeUnpackNotAlignedRGBImage(out unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if (imageWidth*3 mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackNotAlignedRGBImage(const unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
begin
  if (imageWidth*3 mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, unpackData.Alignment);
end;

procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, packData.Alignment);
end;

procedure BeforePackNotAlignedRGBImage(out packdata: TPackNotAlignedData; imageWidth: cardinal);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (imageWidth*3 mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackNotAlignedRGBImage(const packData: TPackNotAlignedData; imageWidth: cardinal);
begin
  if (imageWidth*3 mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, packData.Alignment);
end;

{ projection matrix ---------------------------------------------------------- }

var
  FProjectionMatrix: TMatrix4Single;

function GetProjectionMatrix: TMatrix4Single;
begin
  Result := FProjectionMatrix;
end;

procedure SetProjectionMatrix(const Value: TMatrix4Single);
begin
  FProjectionMatrix := Value;

  {$ifndef OpenGLES}
  glMatrixMode(GL_PROJECTION);
  glLoadMatrix(Value);
  glMatrixMode(GL_MODELVIEW);
  {$endif}
end;

function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
begin
  Result := PerspectiveProjMatrixDeg(fovy, aspect, zNear, zFar);
  ProjectionMatrix := Result;
end;

function OrthoProjection(const left, right, bottom, top, zNear, zFar: Single): TMatrix4Single;
begin
  Result := OrthoProjMatrix(left, right, bottom, top, zNear, zFar);
  ProjectionMatrix := Result;
end;

{ Various helpers ------------------------------------------------------------ }

procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);
begin
  if isEnabled then glEnable(value) else glDisable(value);
end;

{$ifndef OpenGLES}
procedure GLVerticalLine(x, y1, y2: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x, y1); glVertex2f(x, y2); glEnd;
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x1, y); glVertex2f(x2, y); glEnd;
end;

procedure GLDrawArrow(HeadThickness, HeadLength: TGLfloat);
begin
  HeadLength := 2*HeadLength; { mapuj HeadLength na zakres 0..2 }

  { TODO: tutaj powinienes przelaczac glEdgeFlag }

  glBegin(GL_TRIANGLES);
    glVertex2f(0, 2);
    glVertex2f(-1, HeadLength);
    glVertex2f(-HeadThickness, HeadLength);

    glVertex2f(0, 2);
    glVertex2f(-HeadThickness, HeadLength);
    glVertex2f(HeadThickness, HeadLength);

    glVertex2f(0, 2);
    glVertex2f(HeadThickness, HeadLength);
    glVertex2f(1, HeadLength);
  glEnd;

  glBegin(GL_QUADS);
    glVertex2f(-HeadThickness, HeadLength);
    glVertex2f(-HeadThickness, 0);
    glVertex2f(HeadThickness, 0);
    glVertex2f(HeadThickness, HeadLength);
  glEnd;
end;

function NewGLUQuadric(texture: boolean; normals: TGLenum;
  orientation: TGLenum; drawStyle: TGLenum): PGLUQuadric;
begin
  result := gluNewQuadric();
  if result = nil then
    raise Exception.Create('gluNewQuadric cannot be created');
  gluQuadricCallback(result, GLU_ERROR, TCallBack(@GLErrorRaise));
  gluQuadricTexture(result, Ord(texture));
  gluQuadricNormals(result, normals);
  gluQuadricOrientation(result, orientation);
  gluQuadricDrawStyle(result, drawStyle);
end;

procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean; Normals: TGLenum;
  Orientation: TGLenum; DrawStyle: TGLenum);
var
  Q: PGLUQuadric;
begin
  Q := NewGLUQuadric(Texture, Normals, Orientation, DrawStyle);
  try
    gluSphere(Q, Radius, Slices, Stacks);
  finally gluDeleteQuadric(Q); end;
end;

procedure glDrawAxisWire(const Position: TVector3Single; Size: Single);
begin
  Size /= 2;
  glBegin(GL_LINES);
    glVertexv(Position - Vector3Single(Size, 0, 0));
    glVertexv(Position + Vector3Single(Size, 0, 0));
    glVertexv(Position - Vector3Single(0, Size, 0));
    glVertexv(Position + Vector3Single(0, Size, 0));
    glVertexv(Position - Vector3Single(0, 0, Size));
    glVertexv(Position + Vector3Single(0, 0, Size));
  glEnd;
end;

procedure glDrawCornerMarkers(const Box: TBox3D; const Proportion: Single);

  procedure glDrawCorners(const minx, miny, minz, maxx, maxy, maxz: Single);

    procedure glDrawCornerLines(const x, y, z, dx, dy, dz: Single);
    begin
      glVertex3f(x, y, z);
      glVertex3f(x+dx, y, z);
      glVertex3f(x, y, z);
      glVertex3f(x, y+dy, z);
      glVertex3f(x, y, z);
      glVertex3f(x, y, z+dz);
    end;

  var
    Xlength, Ylength, Zlength: Single;
  begin
    Xlength := (maxx - minx) * Proportion;
    Ylength := (maxy - miny) * Proportion;
    Zlength := (maxz - minz) * Proportion;
    glBegin(GL_LINES);
      glDrawCornerLines(minx,miny,minz,Xlength,Ylength,Zlength);
      glDrawCornerLines(minx,miny,maxz,Xlength,Ylength,-Zlength);
      glDrawCornerLines(minx,maxy,minz,Xlength,-Ylength,Zlength);
      glDrawCornerLines(minx,maxy,maxz,Xlength,-Ylength,-Zlength);
      glDrawCornerLines(maxx,miny,minz,-Xlength,Ylength,Zlength);
      glDrawCornerLines(maxx,miny,maxz,-Xlength,Ylength,-Zlength);
      glDrawCornerLines(maxx,maxy,minz,-Xlength,-Ylength,Zlength);
      glDrawCornerLines(maxx,maxy,maxz,-Xlength,-Ylength,-Zlength);
    glEnd;
  end;

begin
  glDrawCorners(Box.Data[0,0], Box.Data[0,1], Box.Data[0,2],
                Box.Data[1,0], Box.Data[1,1], Box.Data[1,2]);
end;

procedure glDrawBox3DWire(const Box: TBox3D);

  procedure glDrawRaw(const minx, miny, minz, maxx, maxy, maxz: Single);
  begin
    glBegin(GL_LINE_LOOP);
      glVertex3f(minx, miny, minz);
      glVertex3f(maxx, miny, minz);
      glVertex3f(maxx, maxy, minz);
      glVertex3f(minx, maxy, minz);
      glVertex3f(minx, maxy, maxz);
      glVertex3f(maxx, maxy, maxz);
      glVertex3f(maxx, miny, maxz);
      glVertex3f(minx, miny, maxz);
    glEnd;

    glBegin(GL_LINES);
      glVertex3f(minx, miny, minz);
      glVertex3f(minx, maxy, minz);
      glVertex3f(minx, miny, maxz);
      glVertex3f(minx, maxy, maxz);
      glVertex3f(maxx, miny, minz);
      glVertex3f(maxx, miny, maxz);
      glVertex3f(maxx, maxy, minz);
      glVertex3f(maxx, maxy, maxz);
    glEnd;
  end;

begin
  glDrawRaw(Box.Data[0,0], Box.Data[0,1], Box.Data[0,2],
            Box.Data[1,0], Box.Data[1,1], Box.Data[1,2])
end;

procedure glDrawBox3DSimple(const Box: TBox3D);
var
  Verts: array [0..7] of TVector3Single;
const
  VertsIndices: array [0..23] of TGLuint =
  (
    0, 1, 3, 2,
    1, 5, 7, 3,
    5, 4, 6, 7,
    4, 0, 2, 6,
    2, 3, 7, 6,
    0, 4, 5, 1
  );
begin
  if Box.IsEmpty then Exit;

  { Verts index in octal notation indicates which of 8 vertexes it is. }
  Verts[0] := Box.Data[0];
  Verts[1] := Box.Data[0]; Verts[1][0] := Box.Data[1][0];
  Verts[2] := Box.Data[0]; Verts[2][1] := Box.Data[1][1];
  Verts[4] := Box.Data[0]; Verts[4][2] := Box.Data[1][2];

  Verts[3] := Box.Data[1]; Verts[3][2] := Box.Data[0][2];
  Verts[5] := Box.Data[1]; Verts[5][1] := Box.Data[0][1];
  Verts[6] := Box.Data[1]; Verts[6][0] := Box.Data[0][0];
  Verts[7] := Box.Data[1];

  glVertexPointer(3, GL_FLOAT, 0, @Verts);

  { TODO: use vbo. Speed of this is important for occlusion query. }

  glDrawElements(GL_QUADS, 6 * 4, GL_UNSIGNED_INT, @VertsIndices);
end;

procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
begin
  glColor4f(Color[0], Color[1], Color[2], Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color[0] / 255, Color[1] / 255, Color[2] / 255, Opacity);
end;
{$endif}

procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor, DestinationFactor: TGLenum;
  const Color: TVector4Single);
begin
  DrawRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1), Color,
    SourceFactor, DestinationFactor, true);
end;

procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4Single);
begin
  DrawRectangle(Rect, Color, GL_ONE, GL_SRC_ALPHA, true);
end;

procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single; const FadeIntensity: Single);
begin
  GLFadeRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1),
    Vector4Single(FadeColor, 1.0), FadeIntensity);
end;

procedure GLFadeRectangle(const Rect: TRectangle;
  const FadeColor: TCastleColor; const FadeIntensity: Single);
const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  { We assume that MinScale is small enough that difference between
    "FadeColor * MinScale * screen color" and
    "MinScale * screen color" is not noticeable. }
  MinScale = 0.5;
  { Constants below make resulting screen color = glColor * previous screen color.
    Note that as long as all components of FadeColor are <= 1,
    then all components of our glColor are also always <= 1,
    and this means that we will always make the screen darker (or equal,
    but never brighter). }
  SourceFactor = GL_ZERO;
  DestinationFactor = GL_SRC_COLOR;
var
  Color: TCastleColor;
begin
  if FadeIntensity > 0 then
  begin
    { for FadeIntensity in 1...FullWhiteEnd (going down):
      screen color := FadeColor * screen color }
    if FadeIntensity > FullWhiteEnd then
      Color := FadeColor else
    { for FadeIntensity in FullWhiteEnd...FullBlack (going down):
      screen color := FadeColor * screen color ...
        FadeColor * MinScale * screen color }
    if FadeIntensity > FullBlack then
      Color := FadeColor * MapRange(FadeIntensity, FullWhiteEnd, FullBlack, 1, MinScale) else
    { for FadeIntensity in FullBlack...0 (going down):
      screen color := MinScale * screen color ...
        unchanged screen color }
      Color := White * MapRange(FadeIntensity, FullBlack, 0, MinScale, 1);

    Color[3] := 1.0; { alpha always 1.0 in this case }
    DrawRectangle(Rect, Color, SourceFactor, DestinationFactor, true);
  end;
end;

{ DrawRectangle ---------------------------------------------------------------- }

var
  {$ifdef GLImageUseShaders}
  GLRectangleProgram: TGLSLProgram;
  {$endif}
  RectanglePointVbo: TGLuint;
  RectanglePoint: packed array [0..3] of TVector2SmallInt;

procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor, BlendingDestinationFactor: TGLEnum;
  const ForceBlending: boolean);
var
  Blending: boolean;
{$ifdef GLImageUseShaders}
  AttribEnabled: array [0..0] of TGLuint;
  AttribLocation: TGLuint;
{$endif}
begin
  {$ifdef GLImageUseShaders}
  if GLRectangleProgram = nil then
  begin
    GLRectangleProgram := TGLSLProgram.Create;
    GLRectangleProgram.AttachVertexShader({$I rectangle.vs.inc});
    GLRectangleProgram.AttachFragmentShader({$I rectangle.fs.inc});
    GLRectangleProgram.Link(true);
  end;
  {$endif}

  Blending := ForceBlending or (Color[3] < 1);
  if Blending then
  begin
    glBlendFunc(BlendingSourceFactor, BlendingDestinationFactor); // saved by GL_COLOR_BUFFER_BIT
    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
  end;

  if (RectanglePointVbo = 0) and GLFeatures.VertexBufferObject then
    glGenBuffers(1, @RectanglePointVbo);

  RectanglePoint[0] := Vector2SmallInt(R.Left          , R.Bottom);
  RectanglePoint[1] := Vector2SmallInt(R.Left + R.Width, R.Bottom);
  RectanglePoint[2] := Vector2SmallInt(R.Left + R.Width, R.Bottom + R.Height);
  RectanglePoint[3] := Vector2SmallInt(R.Left          , R.Bottom + R.Height);

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, RectanglePointVbo);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(RectanglePoint),
      @(RectanglePoint[0]), GL_STREAM_DRAW);
  end;

  {$ifdef GLImageUseShaders}
  GLRectangleProgram.Enable;
  AttribEnabled[0] := GLRectangleProgram.VertexAttribPointer(
    'vertex', 0, 2, GL_SHORT, GL_FALSE, SizeOf(TVector2SmallInt), nil);
  GLRectangleProgram.SetUniform('viewport_size', Viewport2DSize);
  GLRectangleProgram.SetUniform('color', Color);

  {$else}
  glLoadIdentity();
  glColorv(Color);

  glEnableClientState(GL_VERTEX_ARRAY);
  if GLFeatures.VertexBufferObject then
    glVertexPointer(2, GL_SHORT, SizeOf(TVector2SmallInt), nil) else
    glVertexPointer(2, GL_SHORT, SizeOf(TVector2SmallInt), @(RectanglePoint[0]));
  {$endif}

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  {$ifdef GLImageUseShaders}
  GLRectangleProgram.Disable;
  { attribute arrays are enabled independent from GLSL program, so we need
    to disable them separately }
  for AttribLocation in AttribEnabled do
    TGLSLProgram.DisableVertexAttribArray(AttribLocation);
  {$else}
  glDisableClientState(GL_VERTEX_ARRAY);
  {$endif}

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;

  if Blending then
    glDisable(GL_BLEND);
end;

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
    Result := BoolToStr[glGetInteger(Param) = GL_TRUE];
  end;

  function VersionReport(Version: TGenericGLVersion): string;
  begin
    Result := Format('  Version parsed: major: %d, minor: %d, release exists: %s, ' +
      'release: %d, vendor-specific information: "%s"',
      [ Version.Major, Version.Minor, BoolToStr[Version.ReleaseExists],
        Version.Release, Version.VendorInfo ]);
  end;

  function VendorReport(Version: TGLVersion): string;
  begin
    Result :=
      Format(
        '  Vendor-specific version parsed: major: %d, minor: %d, release: %d' +nl+
        '  Vendor: ' +PChar(glGetString(GL_VENDOR)) +nl+
        '  Renderer: ' +PChar(glGetString(GL_RENDERER)) +nl+
        nl+
        '  NVidia: %s' +nl+
        '  ATI: %s (fglrx: %s)' +nl+
        '  Intel: %s' +nl+
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
        '  Buggy GLSL gl_FrontFacing: %s',
        [ Version.VendorMajor, Version.VendorMinor, Version.VendorRelease,
          BoolToStr[Version.VendorNVidia],
          BoolToStr[Version.VendorATI],
          BoolToStr[Version.Fglrx],
          BoolToStr[Version.VendorIntel],
          BoolToStr[Version.Mesa],

          BoolToStr[Version.BuggyGenerateMipmap],
          BoolToStr[Version.BuggyLightModelTwoSide],
          BoolToStr[Version.BuggyVBO],
          BoolToStr[Version.BuggyShaderShadowMap],
          BoolToStr[Version.BuggyGLSLConstStruct],
          BoolToStr[Version.BuggyFBOMultiSampling],
          BoolToStr[Version.BuggyFBOCubeMap],
          BoolToStr[Version.BuggySwapNonStandardViewport],
          BoolToStr[Version.BuggyDepth32],
          BoolToStr[Version.BuggyGLSLFrontFacing]
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
    '  1.2: ' + BoolToStr[GLFeatures.Version_1_2] +nl+
    '  1.3: ' + BoolToStr[GLFeatures.Version_1_3] +nl+
    '  1.4: ' + BoolToStr[GLFeatures.Version_1_4] +nl+
    '  1.5: ' + BoolToStr[GLFeatures.Version_1_5] +nl+
    '  2.0: ' + BoolToStr[GLFeatures.Version_2_0] +nl+
    '  2.1: ' + BoolToStr[GLFeatures.Version_2_1] +nl+
    '  3.0: ' + BoolToStr[GLFeatures.Version_3_0] +nl+
    '  3.1: ' + BoolToStr[GLFeatures.Version_3_1] +nl+
    '  3.2: ' + BoolToStr[GLFeatures.Version_3_2] +nl+
    '  3.3: ' + BoolToStr[GLFeatures.Version_3_3] +nl+
    '  4.0: ' + BoolToStr[GLFeatures.Version_4_0] +nl+
    nl+
    {$endif}

    '---------' +nl+
    'Features:' +nl+
    '  GLSL shaders support: ' + GLSupportNames[TGLSLProgram.ClassSupport] +nl+
    '  Multi-texturing: ' + BoolToStr[GLFeatures.UseMultiTexturing] +nl+
    '  Framebuffer Object: ' + GLSupportNamesFBO[GLFeatures.Framebuffer] +nl+
    '  Multi-sampling for FBO buffers and textures: ' + BoolToStr[GLFeatures.FBOMultiSampling] +nl+
    '  Vertex Buffer Object: ' + BoolToStr[GLFeatures.VertexBufferObject] +nl+
    '  GenerateMipmap available (and reliable): ' + BoolToStr[HasGenerateMipmap] +nl+
    '  Cube map textures: ' + GLSupportNames[GLFeatures.TextureCubeMap] +nl+
    '  S3TC compressed textures: ' + BoolToStr[GLFeatures.TextureCompressionS3TC] +nl+
    '  3D textures: ' + GLSupportNames[GLFeatures.Texture3D] +nl+
    '  Textures non-power-of-2: ' + BoolToStr[GLFeatures.TextureNonPowerOfTwo] +nl+
    '  Blend constant parameter: ' + BoolToStr[GLFeatures.BlendConstant] +nl+
    '  Float textures: ' + BoolToStr[GLFeatures.TextureFloat] +nl+
    '  Depth textures: ' + BoolToStr[GLFeatures.TextureDepth] +nl+
    '  Packed depth + stencil: ' + BoolToStr[GLFeatures.PackedDepthStencil] +nl+
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
    '  Multisampling (full-screen antialiasing): ' + BoolToStr[GLFeatures.Multisample] +nl+
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
    '  Max renderbuffer size: ' + GetMaxRenderbufferSize;

   CheckGLErrors;
end;

{$ifndef OpenGLES}

function glGenListsCheck(range: TGLsizei; const Place: string): TGLuint;
begin
  Result := glGenLists(range);
  if (Result = 0) and (range <> 0) then
    raise EOpenGLNoMoreDisplayLists.CreateFmt(
      'No more OpenGL display lists available when trying to allocate new %d display lists from "%s". This may mean that GPU memory is full (low possibility, unless you used some ridiculous number of display lists), or that OpenGL context is not initialized yet', [range, Place]);
end;

procedure glFreeDisplayList(var list: TGLuint);
begin
 if list <> 0 then
 begin
  glDeleteLists(list, 1);
  list := 0;
 end;
end;

{$endif}

procedure glFreeBuffer(var Buffer: TGLuint);
begin
  if Buffer <> 0 then
  begin
    glDeleteBuffers(1, @Buffer);
    Buffer := 0;
  end;
end;

procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);
begin
  glDepthMask(Writeable);
  glColorMask(Writeable, Writeable, Writeable, Writeable);
end;

var
  FWindowPos: TVector2Integer;

procedure SetWindowPosF(const X, Y: TGLfloat);
begin
  { Hack, only to somewhat support SetWindowPosF for old programs.
    SetWindowPosF should not be used in new code. }
  FWindowPos[0] := Floor(X);
  FWindowPos[1] := Floor(Y);
end;

procedure SetWindowPos(const X, Y: TGLint);
begin
  FWindowPos[0] := X;
  FWindowPos[1] := Y;
end;

procedure SetWindowPos(const Value: TVector2i);
begin
  SetWindowPos(Value[0], Value[1]);
end;

procedure SetWindowPosZero;
begin
  SetWindowPos(0, 0);
end;

function GetWindowPos: TVector2i;
begin
  Result := FWindowPos;
end;

var
  FDepthRange: TDepthRange = drFull;

function GetDepthRange: TDepthRange;
begin
  Result := FDepthRange;
end;

procedure SetDepthRange(const Value: TDepthRange);
begin
  if FDepthRange <> Value then
  begin
    {$ifdef OpenGLES} {$define glDepthRange := glDepthRangef} {$endif}
    FDepthRange := Value;
    case Value of
      drFull: glDepthRange(0  , 1);
      drNear: glDepthRange(0  , 0.1);
      drFar : glDepthRange(0.1, 1);
    end;
  end;
end;

function GLEnableTexture(const Target: TEnableTextureTarget): boolean;
begin
  Result := true;

  {$ifndef OpenGLES}

  case Target of
    etNone: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    et2D: begin
        glEnable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    etCubeMap: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glEnable(GL_TEXTURE_CUBE_MAP) else Result := false;
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    et3D: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glEnable(GL_TEXTURE_3D) else Result := false;
      end;
    else raise EInternalError.Create('GLEnableTexture:Target?');
  end;

  {$endif}
end;

var
  FClearColor: TCastleColor;

procedure GLClear(const Buffers: TClearBuffers;
  const ClearColor: TCastleColor);
const
  ClearBufferMask: array [TClearBuffer] of TGLbitfield =
  ( GL_COLOR_BUFFER_BIT,
    GL_DEPTH_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT );
var
  Mask: TGLbitfield;
  B: TClearBuffer;
begin
  if not VectorsPerfectlyEqual(FClearColor, ClearColor) then
  begin
    FClearColor := ClearColor;
    glClearColor(FClearColor[0], FClearColor[1], FClearColor[2], FClearColor[3]);
  end;
  Mask := 0;
  for B in Buffers do
    Mask := Mask or ClearBufferMask[B];
  if Mask <> 0 then
    {$ifndef OpenGLES} GL {$else} CastleGLES20 {$endif}.GLClear(Mask);
end;

var
//  FScissor: TRectangle; // not needed now
  FScissorEnabled: boolean;

procedure ScissorEnable(const Rect: TRectangle);
begin
//  FScissor := Rect;
  FScissorEnabled := true;
  glScissor(Rect.Left, Rect.Bottom, Rect.Width, Rect.Height);
  glEnable(GL_SCISSOR_TEST);
end;

procedure ScissorDisable;
begin
  if FScissorEnabled then
  begin
    glDisable(GL_SCISSOR_TEST);
    FScissorEnabled := false;
  end;
end;

var
  FGlobalAmbient: TVector3Single = (0.2, 0.2, 0.2);

function GetGlobalAmbient: TVector3Single;
begin
  Result := FGlobalAmbient;
end;

procedure SetGlobalAmbient(const Value: TVector3Single);
begin
  FGlobalAmbient := Value;

  {$ifndef OpenGLES}
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(FGlobalAmbient, 1.0));
  {$endif}
end;

procedure ContextClose;
begin
  glFreeBuffer(RectanglePointVbo);
  {$ifdef GLImageUseShaders}
  FreeAndNil(GLRectangleProgram);
  {$endif}

  { free things created by GLInformationInitialize }
  FreeAndNil(GLVersion);
  {$ifndef OpenGLES}
  FreeAndNil(GLUVersion);
  {$endif}
  FreeAndNil(GLFeatures);
end;

initialization
  { Our GLVersion, GLFeatures should be freed at the every end,
    as a lot of code uses them. So place ContextClose to be called last,
    OnGLContextClose[0].
    Every other unit initializion does OnGLContextClose.Add,
    so our initialization will stay as OnGLContextClose[0]. }
  OnGLContextClose.Insert(0, @ContextClose);
end.
