{
  Copyright 2016-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: simple helpers for OpenGL commands. }

{$ifdef read_interface}

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
procedure glColorv(const v: TVector3Byte); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector4Byte); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector3); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector4); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';

{ }
procedure glTranslatev(const V: TVector3); overload; deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';

procedure glScalev(const V: Single); overload; deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';
procedure glScalev(const V: TVector3); overload; deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3); overload; deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';

procedure glClipPlane(plane: TGLenum; const V: TVector4Double); overload;

procedure glNormalv(const v: TVector3); overload;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4); overload;

procedure glVertexv(const v: TVector2); overload;
procedure glVertexv(const v: TVector3); overload;
procedure glVertexv(const v: TVector4); overload;

procedure glTexCoordv(const v: TVector2); overload;
procedure glTexCoordv(const v: TVector3); overload;
procedure glTexCoordv(const v: TVector4); overload;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4); overload;

procedure glLightv(light, pname: TGLEnum; const params: TVector4); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3); overload;

procedure glLightModelv(pname: TGLenum; const params: TVector4); overload;

procedure glFogv(pname: TGLEnum; const params: TVector4); overload;

// Do not mark as "deprecated" for now. These have valid (although internal) usage.
{ }
procedure glMultMatrix(const m: TMatrix4); overload; //deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';
{ }
procedure glLoadMatrix(const m: TMatrix4); overload; //deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';

{ }
procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4); overload;

{$endif}

procedure GLViewport(const Rect: TRectangle);
  deprecated 'use RenderContext.Viewport';

var
  { Current color, set by glColorv and used for TCastleAbstractFont font printing
    (in case you use deprecated TCastleAbstractFont.Print overloads without
    explicit colors).

    @deprecated Instead of this, use drawing routines that take
    Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...)
    or TDrawableImage.Color. }
  CurrentColor: TCastleColor
    deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';

{ ---------------------------------------------------------------------------- }

{ Enable or disable given OpenGL flag. }
procedure GLSetEnabled(Value: TGLenum; IsEnabled: boolean);

{$ifndef OpenGLES}

{ Draw vertical line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TDrawableImage.Draw. }
procedure GLVerticalLine(x, y1, y2: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw horizontal line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TDrawableImage.Draw. }
procedure GLHorizontalLine(x1, x2, y: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).

  Everything is drawn CCW when seen from standard view (x grows right, y up).
  Uses current OpenGL color. }
procedure GLDrawArrow(HeadThickness: TGLfloat = 0.4;
  HeadLength: TGLfloat = 0.5); deprecated 'use DrawPrimitive2D to draw shapes';

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3; Size: Single); deprecated 'use TCastleScene to draw 3D stuff';

{ Call glColor, taking Opacity as separate Single argument.
  Deprecated, do not use colors like that, instead pass TCastleColor
  to appropriate routines like TCastleAbstractFont.Print.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3; const Opacity: Single); overload; deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single); overload; deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
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

  This is just a shortcut for setting
  RenderContext.DepthBufferUpdate,
  RenderContext.ColorChannels.

  Which are in turn shortcuts to doing this in OpenGL(ES):
  @longcode(#
    glDepthMask(Writeable);
    glColorMask(Writeable, Writeable, Writeable, Writeable);
  #) }
procedure glSetDepthAndColorWriteable(const Writeable: boolean);
  deprecated 'set RenderContext.DepthBufferUpdate, RenderContext.ColorChannels explicitly, this allows to save/restore them reiably';

{ Global position for drawing 2D stuff.
  @deprecated Do this use this.
  Instead of this, use drawing routines that take position as parameters,
  like TDrawableImage.Draw(X,Y) or TCastleAbstractFont.Print(X,Y,...). }
var
  WindowPos: TVector2Integer
    deprecated 'use drawing routines that take position as parameters, like TDrawableImage.Draw(X,Y) or TCastleAbstractFont.Print(X,Y,...)';

type
  TEnableTextureTarget = (etNone, et2D, etCubeMap, et3D);

{ Enable exactly one (or none, for Target=etNone) OpenGL texture target
  for fixed-function pipeline.
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

function BoolToGL(const Value: boolean): TGLboolean;

{$ifndef ForceStandardGLSLApi}
{ PGLcharARBOrNil simply returns a Pointer(S), you can think of it as a NO-OP.
  If string is empty, this returns @nil, otherwise it works just like
  PGLcharARBOrNil(S): returns a Pointer(S) with appropriate type cast. }
function PGLcharARBOrNil(const s: AnsiString): PGLcharARB;
{$endif}

{$endif read_interface}

{$ifdef read_implementation}

{ glGetXxx ---------------------------------------------------------------------}

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

procedure glColorv(const v: TVector3Byte);
begin
  glColor3ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(Vector3(V), 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4Byte);
begin
  glColor4ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V);
  {$warnings on}
end;

procedure glColorv(const v: TVector3);
begin
  glColor3fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V, 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4);
begin
  glColor4fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := V;
  {$warnings on}
end;

procedure glTranslatev(const V: TVector3); begin glTranslatef(V.X, V.Y, V.Z); end;

procedure glScalev(const V: Single); begin glScalef(V, V, V); end;

procedure glScalev(const V: TVector3); begin glScalef(V.X, V.Y, V.Z); end;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3); begin glRotatef(Angle, V.X, V.Y, V.Z); end;

procedure glClipPlane(plane: TGLenum; const V: TVector4Double);
begin
  {$ifdef FPC}GL.{$else}OpenGL.{$endif}glClipPlane(plane, @V);
end;

procedure glNormalv(const v: TVector3); begin glNormal3fv(@v); end;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4);  begin glMaterialfv(face, pname, @params); end;

procedure glVertexv(const v: TVector2);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector3);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector4);  begin glVertex4fv(@v); end;

procedure glTexCoordv(const v: TVector2);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector3);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector4);  begin glTexCoord4fv(@v); end;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4);  begin glTexGenfv(coord, pname, @params); end;

procedure glLightv(light, pname: TGLEnum; const params: TVector4);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3);  begin glLightfv(light, pname, @params); end;

procedure glLightModelv(pname: TGLenum; const params: TVector4); overload; begin glLightModelfv(pname, @params); end;

procedure glFogv(pname: TGLEnum; const params: TVector4);  begin glFogfv(pname, @params); end;

procedure glMultMatrix(const m: TMatrix4); begin glMultMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4); begin glLoadMatrixf(@m) end;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4); begin glTexEnvfv(target, pname, @params); end;

{$endif}

procedure GLViewport(const Rect: TRectangle);
begin
  RenderContext.Viewport := Rect;
end;

{ Various helpers ------------------------------------------------------------ }

procedure GLSetEnabled(Value: TGLenum; IsEnabled: boolean);
begin
  if IsEnabled then glEnable(Value) else glDisable(Value);
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
  HeadLength := 2*HeadLength; { map HeadLength to 0..2 }

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
    [Vector2(x, y1),
     Vector2(x, y2)],
    CurrentColor);
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  DrawPrimitive2D(pmLines,
    [Vector2(x1, y),
     Vector2(x2, y)],
    CurrentColor);
end;

procedure GLDrawArrow(HeadThickness, HeadLength: TGLfloat);
begin
  HeadLength := 2*HeadLength; { map HeadLength to 0..2 }

  DrawPrimitive2D(pmTriangles,
    [Vector2(0, 2),
     Vector2(-1, HeadLength),
     Vector2(-HeadThickness, HeadLength),

     Vector2(0, 2),
     Vector2(-HeadThickness, HeadLength),
     Vector2(HeadThickness, HeadLength),

     Vector2(0, 2),
     Vector2(HeadThickness, HeadLength),
     Vector2(1, HeadLength),

     // quad
     Vector2(-HeadThickness, HeadLength),
     Vector2(-HeadThickness, 0),
     Vector2(HeadThickness, 0),

     Vector2(-HeadThickness, HeadLength),
     Vector2(HeadThickness, 0),
     Vector2(HeadThickness, HeadLength)
    ],
    CurrentColor);
end;
*)

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
  glColor4f(Color.X, Color.Y, Color.Z, Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color.X / 255, Color.Y / 255, Color.Z / 255, Opacity);
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

function BoolToGL(const Value: boolean): TGLboolean;
begin
  if Value then
    Result := GL_TRUE
  else
    Result := GL_FALSE;
end;

procedure glSetDepthAndColorWriteable(const Writeable: boolean);
begin
  RenderContext.DepthBufferUpdate := Writeable;
  if Writeable then
    RenderContext.ColorChannels := [0..3]
  else
    RenderContext.ColorChannels := [];
end;

function GLEnableTexture(const Target: TEnableTextureTarget): boolean;
begin
  Result := true;

  if GLFeatures.EnableFixedFunction then
  begin
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
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('GLEnableTexture:Target?');
      {$endif}
    end;

    {$endif}
  end;
end;

{$ifndef ForceStandardGLSLApi}
function PGLcharARBOrNil(const s: AnsiString): PGLcharARB;
begin if s = '' then result := nil else result := PGLcharARB(s); end;
{$endif}

{$endif read_implementation}
