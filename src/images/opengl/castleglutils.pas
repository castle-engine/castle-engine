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

{ Various low-level utilities for working with OpenGL. }
unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses SysUtils, Math, Matrix,
  { Because of FPC 2.6.4 bugs (not present in FPC >= 3.0.0) we cannot use here
    the FGL unit. It breaks compilation of Lazarus packages, as compiling
    castle_window.lpk then accidentally wants to recompile CastleGLShaders too.
    In consequence, we use TFPObjectList instead of generic TFPGObjectList for scissor stuff.
    This will be remedied once we drop FPC 2.6.4 compatibility. }
  Contnrs,
  CastleImages, CastleGL, CastleUtils, CastleVectors, CastleRectangles, CastleColors;

{$define read_interface}

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
  with -dRELEASE. With -dRELEASE, we make WritelnWarning. This way eventual
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

  TODO: Some of these functions should be treated as internal.
  A subset of these internal functions are only available in fixed-function pipeline.
  The rest of these functions are just deprecated -- because they
  are only for fixed-function pipeline, in immediate mode (and all modern code
  should use VBO).
}

{$ifndef OpenGLES}

// Do not mark glColorv as "deprecated" for now. These have valid (although internal) usage.
{ }
procedure glColorv(const v: TVector3ub); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
{ }
procedure glColorv(const v: TVector4ub); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
{ }
procedure glColorv(const v: TVector3f); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
{ }
procedure glColorv(const v: TVector4f); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';

{ }
procedure glTranslatev(const V: TVector3f); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';
procedure glTranslatev(const V: TVector3_Single); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';

procedure glScalev(const V: Single); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';
procedure glScalev(const V: TVector3f); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';
procedure glScalev(const V: TVector3_Single); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); overload; deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';

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

// Do not mark as "deprecated" for now. These have valid (although internal) usage.
{ }
procedure glMultMatrix(const m: TMatrix4f); overload; //deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';
{ }
procedure glLoadMatrix(const m: TMatrix4f); overload; //deprecated 'use other methods to transform things, e.g. TUIControl position / anchors, or T3DTransform for TCastleScene, or TTransformNode in X3D';

{ }
procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); overload;

{$endif}

procedure GLViewport(const Rect: TRectangle);

function GetCurrentColor: TCastleColor; deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
procedure SetCurrentColor(const Value: TCastleColor); deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';

{ Current color, set by glColorv and used for TCastleFont font printing
  (in case you use deprecated TCastleFont.Print overloads without
  explicit colors).

  @deprecated Instead of this, use drawing routines that take
  Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...)
  or TGLImage.Color. }
property CurrentColor: TCastleColor read GetCurrentColor write SetCurrentColor;
  // deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';

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
  { Viewport size for 2D rendering functions: DrawRectangle and TGLImageCore.Draw.
    UI container (like TCastleWindowCustom or TCastleControlCustom)
    must take care to set this before rendering. }
  Viewport2DSize: TVector2Single;

{ ---------------------------------------------------------------------------- }

{ }
procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);

{$ifndef OpenGLES}

{ Draw vertical line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TGLImageCore.Draw. }
procedure GLVerticalLine(x, y1, y2: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw horizontal line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TGLImageCore.Draw. }
procedure GLHorizontalLine(x1, x2, y: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).

  Everything is drawn CCW when seen from standard view (x grows right, y up).
  Uses current OpenGL color. }
procedure GLDrawArrow(HeadThickness: TGLfloat = 0.4;
  HeadLength: TGLfloat = 0.5); deprecated 'use DrawPrimitive2D to draw shapes';

{ Comfortable wrapper for gluNewQuadric. Sets all quadric parameters.
  Sets also the GLU_ERROR callback to ReportGLerror.
  @raises Exception If gluNewQuadric fails (returns nil). }
function NewGLUQuadric(
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL): PGLUQuadric; deprecated 'use TCastleScene to draw 3D stuff';

{ Render sphere in OpenGL. Radius, Slices, Stacks have the same meaning
  as for gluSphere (in case they are not self-explanatory...).
  Other parameters set glu quadric parameters, see glu quadric documentation. }
procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL); deprecated 'use TCastleScene to draw 3D stuff';

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3Single; Size: Single); deprecated 'use TCastleScene to draw 3D stuff';

{ Call glColor, taking Opacity as separate Single argument.
  Deprecated, do not use colors like that, instead pass TCastleColor
  to appropriate routines like TCastleFont.Print.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single); deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single); deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
{ @groupEnd }
{$endif}

{ Utilities for display lists ---------------------------------------- }

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
function glGenListsCheck(range: TGLsizei; const Place: string): TGLuint; deprecated 'do not use display lists; they are not available on OpenGLES and modern OpenGL';

{ If List <> 0 then it does glDeleteList on List and sets List to 0.
  In other words this is simply glDeleteList but
  @orderedList(
    @item only if List really should be deleted
    @item sets List to 0 after deletion
  ) }
procedure glFreeDisplayList(var list: TGLuint); deprecated 'do not use display lists; they are not available on OpenGLES and modern OpenGL';

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
  @groupBegin }
procedure SetWindowPos(const X, Y: TGLint); deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';
procedure SetWindowPos(const Value: TVector2i); deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';
procedure SetWindowPosF(const X, Y: TGLfloat); deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';
procedure SetWindowPosZero; deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';
{ @groupEnd }

function GetWindowPos: TVector2i; deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';

{ Global position for drawing 2D stuff.
  @deprecated Do this use this.
  Instead of this, use drawing routines that take position as parameters,
  like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...). }
property WindowPos: TVector2i read GetWindowPos write SetWindowPos;
  // deprecated 'instead of this, use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';

type
  { Possible values of @link(DepthRange). }
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

{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_context.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders,
  CastleLog, CastleApplicationProperties;

{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_context.inc}

{ EOpenGLError, CheckGLErrors ------------------------------------------------ }

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string): string;
var
  S: string;
begin
  { Do not use gluErrorString, not available in OpenGL ES.
    Error descriptions below from
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
    WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
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
      WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
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

{ ---------------------------------------------------------------------------- }

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
  {$warnings off}
  glLoadMatrix(Value); // consciously using deprecated stuff; this should be internal in this unit
  {$warnings on}
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

(* // These versions would work with OpenGLES. But they ignore OpenGL matrix state like glTransform etc.

procedure GLVerticalLine(x, y1, y2: TGLfloat);
begin
  DrawPrimitive2D(pmLines,
    [Vector2SmallInt(Round(x), Round(y1)),
     Vector2SmallInt(Round(x), Round(y2))],
    CurrentColor);
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  DrawPrimitive2D(pmLines,
    [Vector2SmallInt(Round(x1), Round(y)),
     Vector2SmallInt(Round(x2), Round(y))],
    CurrentColor);
end;

procedure GLDrawArrow(HeadThickness, HeadLength: TGLfloat);
begin
  HeadLength := 2*HeadLength; { mapuj HeadLength na zakres 0..2 }

  DrawPrimitive2D(pmTriangles,
    [Vector2SmallInt(0, 2),
     Vector2SmallInt(-1, Round(HeadLength)),
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),

     Vector2SmallInt(0, 2),
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength)),

     Vector2SmallInt(0, 2),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(1, Round(HeadLength)),

     // quad
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(-Round(HeadThickness), 0),
     Vector2SmallInt(Round(HeadThickness), 0),

     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(Round(HeadThickness), 0),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength))
    ],
    CurrentColor);
end;
*)

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
  {$warnings off} { deliberately using deprecated stuff inside another deprecated }
  Q := NewGLUQuadric(Texture, Normals, Orientation, DrawStyle);
  {$warnings on}
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

procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
begin
  glColor4f(Color[0], Color[1], Color[2], Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color[0] / 255, Color[1] / 255, Color[2] / 255, Opacity);
end;
{$endif}

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
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  SetWindowPos(Value[0], Value[1]);
  {$warnings on}
end;

procedure SetWindowPosZero;
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  SetWindowPos(0, 0);
  {$warnings on}
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

{ ---------------------------------------------------------------------------- }

procedure ContextClose;
begin
  FreeAndNil(Primitive2DRes);

  { free things created by GLInformationInitialize }
  FreeAndNil(GLVersion);
  {$ifndef OpenGLES}
  FreeAndNil(GLUVersion);
  {$endif}
  FreeAndNil(GLFeatures);
end;

initialization
  { Our GLVersion, GLFeatures should be freed at the very end,
    as a lot of code uses them. So place ContextClose to be called last,
    OnGLContextClose[0].
    Every other unit initializion does OnGLContextClose.Add,
    so our initialization will stay as OnGLContextClose[0]. }
  ApplicationProperties.OnGLContextClose.Insert(0, @ContextClose);
end.
