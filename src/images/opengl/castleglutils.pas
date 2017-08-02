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

uses SysUtils, Math, Matrix, Generics.Collections,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleImages, CastleUtils, CastleVectors, CastleRectangles,
  CastleColors, CastleProjection;

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
  TVector2f = TVector2 deprecated 'use TVector2';
  PVector2f = PVector2 deprecated 'use PVector2';

  TVector2ub = TVector2Byte deprecated 'use TVector2Byte';
  PVector2ub = PVector2Byte deprecated 'use PVector2Byte';

  TVector2i = TVector2Integer deprecated 'use TVector2Integer';
  PVector2i = PVector2Integer deprecated 'use PVector2Integer';

  TVector3f = TVector3 deprecated 'use TVector3';
  PVector3f = PVector3 deprecated 'use PVector3';

  TVector3ub = TVector3Byte deprecated 'use TVector3Byte';
  PVector3ub = PVector3Byte deprecated 'use PVector3Byte';

  TVector3i = TVector3Integer deprecated 'use TVector3Integer';
  PVector3i = PVector3Integer deprecated 'use PVector3Integer';

  TVector4f = TVector4 deprecated 'use TVector4';
  PVector4f = PVector4 deprecated 'use PVector4';

  TVector4ub = TVector4Byte deprecated 'use TVector4Byte';
  PVector4ub = PVector4Byte deprecated 'use PVector4Byte';

  TVector4i = TVector4Integer deprecated 'use TVector4Integer';
  PVector4i = PVector4Integer deprecated 'use PVector4Integer';

  TMatrix2f = TMatrix2 deprecated 'use TMatrix2';
  PMatrix2f = PMatrix2 deprecated 'use PMatrix2';

  TMatrix3f = TMatrix3 deprecated 'use TMatrix3';
  PMatrix3f = PMatrix3 deprecated 'use PMatrix3';

  TMatrix4f = TMatrix4 deprecated 'use TMatrix4';
  PMatrix4f = PMatrix4 deprecated 'use PMatrix4';

{ OpenGL error checking ------------------------------------------------------ }

type
  { OpenGL error.
    Usually OpenGL error indicates a bug in our code (or shader code,
    depending on TUniformNotFoundAction and TUniformTypeMismatchAction;
    by default, they do not cause errors).
    Except the @link(EOpenGLOutOfMemoryError), which may happen regardless
    of what we do, and depends on the device. }
  EOpenGLError = class(Exception)
  public
    ErrorCode: TGLenum;
    constructor Create(const AErrorCode: TGLenum; const AdditionalComment: string = '');
  end;

  { GPU memory is not sufficient.

    When programming for platforms with limited GPU memory (Android, iOS...)
    you should prepare to handle EOpenGLOutOfMemoryError (corresponding to
    GL_OUT_OF_MEMORY). This can always happen for large GPU data,
    and you should be prepared to capture it (at least around TGameSceneManager.LoadLevel)
    and display some nice information for user.
    Alternatively, you can leave GLOutOfMemoryError = @false,
    and then EOpenGLOutOfMemoryError will not happen, but you risk all kinds
    of rendering artifacts. }
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
  deprecated 'this function is no longer useful, as you should not use GLU in cross-platform programs';

{ ---------------------------------------------------------------------------- }

{ Comfortable wrappers for OpenGL glGet* that return a single value.

  Guarantee that result is zero in case of OpenGL error.
  (Otherwise, OpenGL could leave them undefined and only set glGetError.)
  @groupBegin }
function glGetFloat(pname: TGLEnum): TGLfloat;
function glGetInteger(pname: TGLEnum): TGLint;
function glGetBoolean(pname: TGLEnum): TGLboolean;
{ @groupEnd }

(*------------------------------------------------------------------------------
  Comfortable wrappers around many OpenGL functions.
  Overloaded for our vector types.

  Note that functions here simply call appropriate OpenGL functions.
  Long time ago we tried using tricks to speed this up (eliminate
  function call overhead), by importing these functions from so/dll
  under different names, like

    procedure glVertexv(const V: TVector3);
      {$ifdef OPENGL_CDECL} cdecl; {$endif}
      {$ifdef OPENGL_STDCALL} stdcall; {$endif}
      overload; external OpenGLDLL name 'glVertex3fv';

  But this is problematic: it assumes that TVector3 will be passed
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
*)

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

procedure glClipPlane(plane: GLenum; const V: TVector4Double); overload;

procedure glNormalv(const v: TVector3); overload;

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

var
  { Current color, set by glColorv and used for TCastleFont font printing
    (in case you use deprecated TCastleFont.Print overloads without
    explicit colors).

    @deprecated Instead of this, use drawing routines that take
    Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...)
    or TGLImage.Color. }
  CurrentColor: TCastleColor
    deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';

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

{$ifdef CASTLE_OBJFPC}
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
{$endif}

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3; Size: Single); deprecated 'use TCastleScene to draw 3D stuff';

{ Call glColor, taking Opacity as separate Single argument.
  Deprecated, do not use colors like that, instead pass TCastleColor
  to appropriate routines like TCastleFont.Print.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3; const Opacity: Single); overload; deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single); overload; deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleFont.Print(X,Y,Color,...) or TGLImage.Color';
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

{ Global position for drawing 2D stuff.
  @deprecated Do this use this.
  Instead of this, use drawing routines that take position as parameters,
  like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...). }
var
  WindowPos: TVector2Integer
    deprecated 'use drawing routines that take position as parameters, like TGLImageCore.Draw(X,Y) or TCastleFont.Print(X,Y,...)';

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
  Result := Result + Format('OpenGL error (%d): %s', [ErrorCode, S]);
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

{$ifndef OpenGLES}

procedure glColorv(const v: TVector3ub);
begin
  glColor3ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(Vector3(V), 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4ub);
begin
  glColor4ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V);
  {$warnings on}
end;

procedure glColorv(const v: TVector3f);
begin
  glColor3fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V, 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4f);
begin
  glColor4fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := V;
  {$warnings on}
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

procedure glClipPlane(plane: GLenum; const V: TVector4Double);
begin
  GL.glClipPlane(plane, @V);
end;

procedure glNormalv(const v: TVector3); begin glNormal3fv(@v); end;

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

{$ifdef CASTLE_OBJFPC}
function NewGLUQuadric(texture: boolean; normals: TGLenum;
  orientation: TGLenum; drawStyle: TGLenum): PGLUQuadric;
begin
  result := gluNewQuadric();
  if result = nil then
    raise Exception.Create('gluNewQuadric cannot be created');
  {$push}
  {$warnings off}
  // knowingly using deprecated in another deprecated
  gluQuadricCallback(result, GLU_ERROR, TCallBack(@GLErrorRaise));
  {$pop}
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
{$endif CASTLE_OBJFPC}

procedure glDrawAxisWire(const Position: TVector3; Size: Single);
begin
  Size := Size / 2;
  glBegin(GL_LINES);
    glVertexv(Position - Vector3(Size, 0, 0));
    glVertexv(Position + Vector3(Size, 0, 0));
    glVertexv(Position - Vector3(0, Size, 0));
    glVertexv(Position + Vector3(0, Size, 0));
    glVertexv(Position - Vector3(0, 0, Size));
    glVertexv(Position + Vector3(0, 0, Size));
  glEnd;
end;

procedure glColorOpacity(const Color: TVector3; const Opacity: Single);
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
  {$ifdef CASTLE_OBJFPC}
  {$warnings off} // consciously initializing deprecated stuff, to keep it working
  FreeAndNil(GLUVersion);
  {$warnings on}
  {$endif CASTLE_OBJFPC}
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
