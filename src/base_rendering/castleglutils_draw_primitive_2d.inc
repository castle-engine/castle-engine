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

{ Part of CastleGLUtils unit: DrawPrimitive2D and friends. }

{$ifdef read_interface}

type
  { Primitive to draw using DrawPrimitive2D.
    The modes correspond to OpenGL drawing modes, see
    https://www.opengl.org/wiki/Primitive
    https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml }
  TPrimitiveMode = (
    pmPoints,
    pmLineStrip,
    pmLineLoop,
    pmLines,
    pmTriangleStrip,
    pmTriangleFan,
    pmTriangles
  );

  TLineStipple = record
    { Makes pattern scaled to be longer.
      See glLineStipple parameter. }
    Factor: Integer;

    { The line pattern, each bit in 16-bit integer says if the line is visible.
      See glLineStipple parameter. }
    Pattern: Word;
  end;

const
  DefaultCircleSlices = 32;

{ Draw a rectangle filled with a color.

  Like with DrawPrimitive2D:
  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;
procedure DrawRectangle(const R: TFloatRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;

{ Draw a rectangle frame.

  Like with DrawPrimitive2D:
  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawRectangleOutline(const R: TRectangle; const Color: TCastleColor;
  const LineWidth: Single = 1;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;
procedure DrawRectangleOutline(const R: TFloatRectangle; const Color: TCastleColor;
  const LineWidth: Single = 1;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;

{ Draw a circle. It may be scaled differently in X than Y, thus resembling an ellipse.

  Like with DrawPrimitive2D:
  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawCircle(const Middle: TVector2Integer;
  const HorizontalRadius, VerticalRadius: Integer;
  const Color: TCastleColor;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;
procedure DrawCircle(const Middle: TVector2;
  const HorizontalRadius, VerticalRadius: Single;
  const Color: TCastleColor;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;

{ Draw a circle outline. It may be scaled differently in X than Y, thus resembling an ellipse.

  Like with DrawPrimitive2D:
  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawCircleOutline(const Middle: TVector2Integer;
  const HorizontalRadius, VerticalRadius: Integer;
  const Color: TCastleColor;
  const LineWidth: Single = 1;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;
procedure DrawCircleOutline(const Middle: TVector2;
  const HorizontalRadius, VerticalRadius: Single;
  const Color: TCastleColor;
  const LineWidth: Single = 1;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false); overload;

{ Draw a simple 2D primitive with a given color.
  This can be used to draw a series of points, lines or triangles,
  depending on the @code(Mode) parameter.

  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.

  @param(LineWidth Line width, only used when Mode indicates lines.

    Note that @bold(values other than 1 are not guaranteed to be supported.)
    While it's a fast method to have thick lines, it is implementation-limited in case of
    @orderedList(
      @item(desktop OpenGL with anti-aliasing, see
        https://www.opengl.org/sdk/docs/man/html/glLineWidth.xhtml )
      @item(mobile OpenGLES, with or without anti-aliasing, see
        https://www.khronos.org/opengles/sdk/docs/man/xhtml/glLineWidth.xml )
    ))

  @param(PointSize Point size, only used when Mode indicates points.

    Note that @bold(values other than 1 are not guaranteed to be supported.)
    Mobile OpenGLES does not support setting point size like this.
    And desktop OpenGL limits point size in case anti-aliasing is used,
    see https://www.opengl.org/sdk/docs/man2/xhtml/glPointSize.xml .)
}
procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2SmallInt;
  const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false;
  const LineWidth: Single = 1;
  const PointSize: Cardinal = 1); overload;
procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2;
  const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false;
  const LineWidth: Single = 1;
  const PointSize: Cardinal = 1); overload;

{ Draw primitive, using a stipple (pattern) for line rendering.
  Note that the stipple is ignored on mobile (OpenGLES). }
procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const LineStipple: TLineStipple;
  const Points: array of TVector2;
  const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false;
  const LineWidth: Single = 1;
  const PointSize: Cardinal = 1); overload;

{ Draw a rectangle that modulates colors underneath,
  making nice animation to AFadeColor while FadeIntensity changes from 1.0
  down to 0.0.

  The GLFadeRectangleLight version makes a flash to AFadeColor,
  then goes back to normal. It works reasonably with any color (light or dark).

  The GLFadeRectangleDark version makes additional flash to blackness
  in the middle (so it goes from no modulation, to AFadeColor,
  to pure black, and then back to normal).
  So the "Dark" version is a little more impressive when you're
  flashing with a dark color.

  These are nice as a screen effect, to flash some color (e.g. flash
  red color when the player is hurt).

  Only RGB portion of AFadeColor is used. }
procedure GLFadeRectangleDark(const X1, Y1, X2, Y2: Integer;
  const AFadeColor: TVector3;
  const FadeIntensity: Single); overload;
procedure GLFadeRectangleDark(const Rect: TRectangle;
  const AFadeColor: TCastleColor;
  const FadeIntensity: Single); overload;
procedure GLFadeRectangleDark(const Rect: TFloatRectangle;
  const AFadeColor: TCastleColor;
  const FadeIntensity: Single); overload;
procedure GLFadeRectangleLight(const Rect: TRectangle;
  const AFadeColor: TCastleColor;
  const FadeIntensity: Single); overload;
procedure GLFadeRectangleLight(const Rect: TFloatRectangle;
  const AFadeColor: TCastleColor;
  const FadeIntensity: Single); overload;
procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const AFadeColor: TVector3;
  const FadeIntensity: Single); overload; deprecated 'use GLFadeRectangleDark, or TCastleFlashEffect with Dark = true';
procedure GLFadeRectangle(const Rect: TRectangle;
  const AFadeColor: TCastleColor;
  const FadeIntensity: Single); overload; deprecated 'use GLFadeRectangleDark, or TCastleFlashEffect with Dark = true';

{ Draw a rectangle with blending.
  @deprecated Deprecated, use DrawRectangle instead. }
procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor;
  const Color: TVector4); overload; deprecated 'use DrawRectangle';
procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4); overload; deprecated 'use DrawRectangle';

{ Call OpenGL @url(https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml glBlendFunc).
  Accepts parameters as enumerated types TBlendingSourceFactor, TBlendingDestinationFactor.
  Secures from using source or destination factor unsupported on current OpenGL version:
  makes a warnings and falls back on a sensible default (so it will always call
  glBlendFunc anyway). }
procedure GLBlendFunction(
  SourceFactor: TBlendingSourceFactor;
  DestinationFactor: TBlendingDestinationFactor); deprecated 'use RenderContext.BlendingEnable';

{$endif read_interface}

{$ifdef read_implementation}

{ DrawPrimitive2D ---------------------------------------------------------------- }

const
  {$ifdef USE_DGL}
  GL_CONSTANT_COLOR = GL_CONSTANT_COLOR_ARB;
  {$endif}

  BlendingSourceFactorToGL: array [TBlendingSourceFactor] of TGLEnum = (
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_ZERO,
    GL_ONE,

    GL_DST_COLOR,
    GL_SRC_COLOR,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_ONE_MINUS_DST_ALPHA,

    GL_SRC_ALPHA_SATURATE,

    GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR,
    GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA
  );
  BlendingDestinationFactorToGL: array [TBlendingDestinationFactor] of TGLEnum = (
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_ZERO,
    GL_ONE,

    GL_DST_COLOR,
    GL_SRC_COLOR,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_ONE_MINUS_DST_ALPHA,

    // GL_SRC_ALPHA_SATURATE, // not supported as destination factor

    GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR,
    GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA
  );
  PrimitiveModeToGL: array [TPrimitiveMode] of TGLEnum = (
    GL_POINTS,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_TRIANGLES
  );

type
  { OpenGL resources for DrawPrimitive2D.
    Shared across OpenGL contexts, must be freed before last OpenGL context is freed. }
  TPrimitive2DResources = class
    { Used only in shader pipeline }
    Prog: TGLSLProgram;
    UniformViewportSize, UniformColor: TGLSLUniform;
    AttribVertex: TGLSLAttribute;

    Vbo: TGLBuffer;
    Vao: TVertexArrayObject;
    PointPtr: Pointer;
    PointPtrAllocated: Integer;
    destructor Destroy; override;
  end;

destructor TPrimitive2DResources.Destroy;
begin
  FreeBuffer(Vbo);
  FreeAndNil(Vao);
  FreeAndNil(Prog);
  FreeMem(PointPtr); // not tied to OpenGL, but freed together anyway
  PointPtr := nil;
  PointPtrAllocated := 0;
  inherited;
end;

var
  Primitive2DRes: TPrimitive2DResources;

procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2SmallInt; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean;
  const LineWidth: Single;
  const PointSize: Cardinal);
var
  PointsSingle: array of TVector2;
  I: Integer;
begin
  SetLength(PointsSingle, Length(Points));
  for I := 0 to Length(Points) - 1 do
    PointsSingle[I] := Vector2(
      Points[I].X,
      Points[I].Y);
  DrawPrimitive2D(Mode, PointsSingle, Color,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending,
    LineWidth, PointSize);
end;

procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean;
  const LineWidth: Single;
  const PointSize: Cardinal);
var
  Blending: boolean;
  I, RequiredPointPtrSize: Integer;
  TargetPointPtr: PVector2;
begin
  TDrawableImage.BatchingFlush;

  FrameProfiler.Start(fmDrawPrimitive2D);

  if Primitive2DRes = nil then
    Primitive2DRes := TPrimitive2DResources.Create;

  if not GLFeatures.EnableFixedFunction then
  begin
    if Primitive2DRes.Prog = nil then
    begin
      Primitive2DRes.Prog := TGLSLProgram.Create;
      Primitive2DRes.Prog.AttachVertexShader({$I primitive_2.vs.inc});
      Primitive2DRes.Prog.AttachFragmentShader({$I primitive_2.fs.inc});
      Primitive2DRes.Prog.Link;

      Primitive2DRes.UniformViewportSize := Primitive2DRes.Prog.Uniform('viewport_size');
      Primitive2DRes.UniformColor := Primitive2DRes.Prog.Uniform('color');
      Primitive2DRes.AttribVertex := Primitive2DRes.Prog.Attribute('vertex');
    end;

    if Primitive2DRes.Vbo = GLObjectNone then
    begin
      Assert(GLFeatures.VertexBufferObject);
      Primitive2DRes.Vbo := glCreateBuffer();
    end;

    if Primitive2DRes.Vao = nil then
      Primitive2DRes.Vao := TVertexArrayObject.Create(nil);
  end;

  { apply LineWidth, PointSize.
    Their setters avoid doing anything when they already have the requested
    values, so we can just assign them here not worrying about performance. }
  RenderContext.LineWidth := LineWidth;
  RenderContext.PointSize := PointSize;

  Blending := ForceBlending or (Color.W < 1);
  if Blending then
    RenderContext.BlendingEnable(BlendingSourceFactor, BlendingDestinationFactor);

  { make Primitive2DRes.PointPtr have necessary size }
  RequiredPointPtrSize := SizeOf(TVector2) * (High(Points) + 1);
  if Primitive2DRes.PointPtrAllocated < RequiredPointPtrSize then
  begin
    { FreeMem + GetMem, not ReallocMem,
      because we don't care about preserving previous memory contents }
    if Primitive2DRes.PointPtr <> nil then
      FreeMem(Primitive2DRes.PointPtr);
    Primitive2DRes.PointPtr := GetMem(RequiredPointPtrSize);
    Primitive2DRes.PointPtrAllocated := RequiredPointPtrSize;
  end;

  { copy Points to Primitive2DRes.PointPtr }
  TargetPointPtr := Primitive2DRes.PointPtr;
  for I := 0 to High(Points) do
  begin
    Move(Points[I], TargetPointPtr^, SizeOf(TVector2));
    Inc(TargetPointPtr);
  end;

  if not GLFeatures.EnableFixedFunction then
  begin
    RenderContext.CurrentProgram := Primitive2DRes.Prog;
    RenderContext.CurrentVao := Primitive2DRes.Vao;

    RenderContext.BindBuffer[btArray] := Primitive2DRes.Vbo;
    { load only RequiredPointPtrSize, not Primitive2DRes.PointPtrAllocated which may be larger }
    Assert(Primitive2DRes.PointPtrAllocated >= RequiredPointPtrSize);
    glBufferData(GL_ARRAY_BUFFER, RequiredPointPtrSize, Primitive2DRes.PointPtr, GL_DYNAMIC_DRAW);

    Primitive2DRes.AttribVertex.EnableArrayVector2(Primitive2DRes.Vao, SizeOf(TVector2), 0);

    { We do not warn when InternalClearColorsByDraw, because then indeed we'll have viewport
      with non-zero left/bottom, and we DrawRectangle is used with Viewport as argument.
      It works OK in this case. }
    if (not RenderContext.InternalClearColorsByDraw) and
       ( (RenderContext.Viewport.Left <> 0) or
         (RenderContext.Viewport.Bottom <> 0)
       ) then
      WritelnWarning('Drawing DrawPrimitive2D (DrawRectangle etc.) when viewport (left,bottom) is not (0,0) is undefined now');

    Primitive2DRes.UniformViewportSize.SetValue(Vector2(
      RenderContext.Viewport.Width,
      RenderContext.Viewport.Height
    ));

    Primitive2DRes.UniformColor.SetValue(Color);
  end else
  begin
    {$ifndef OpenGLES}
    Assert(RenderContext.CurrentProgram = nil); // when EnableFixedFunction, this should always stay nil
    glLoadIdentity();
    glColorv(Color);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, SizeOf(TVector2), Primitive2DRes.PointPtr);
    {$endif}
  end;

  glDrawArrays(PrimitiveModeToGL[Mode], 0, High(Points) + 1);

  if not GLFeatures.EnableFixedFunction then
  begin
    // Primitive2DRes.Prog.Disable; // no need to disable, keep it enabled to save speed
    { attribute arrays are enabled independent from GLSL program, so we need
      to disable them separately }
    Primitive2DRes.AttribVertex.DisableArray;

    RenderContext.BindBuffer[btArray] := GLObjectNone;
  end else
  begin
    {$ifndef OpenGLES}
    glDisableClientState(GL_VERTEX_ARRAY);
    {$endif}
  end;

  if Blending then
    RenderContext.BlendingDisable;

  FrameProfiler.Stop(fmDrawPrimitive2D);
end;

procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const LineStipple: TLineStipple;
  const Points: array of TVector2;
  const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false;
  const LineWidth: Single = 1;
  const PointSize: Cardinal = 1);
begin
  {$ifndef OpenGLES}
  glLineStipple(1, LineStipple.Pattern);
  glEnable(GL_LINE_STIPPLE);
  {$endif}

  DrawPrimitive2D(Mode, Points, Color,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending,
    LineWidth, PointSize);

  {$ifndef OpenGLES}
  glDisable(GL_LINE_STIPPLE);
  {$endif}
end;

{ DrawRectangle ---------------------------------------------------------------- }

procedure DrawRectangle(const R: TFloatRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean);
var
  RectanglePoint: array [0..3] of TVector2;
begin
  RectanglePoint[0] := Vector2(R.Left          , R.Bottom);
  RectanglePoint[1] := Vector2(R.Left + R.Width, R.Bottom);
  RectanglePoint[2] := Vector2(R.Left + R.Width, R.Bottom + R.Height);
  RectanglePoint[3] := Vector2(R.Left          , R.Bottom + R.Height);

  DrawPrimitive2D(pmTriangleFan, RectanglePoint,
    Color, BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean);
begin
  DrawRectangle(FloatRectangle(R), Color,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

procedure DrawRectangleOutline(const R: TFloatRectangle; const Color: TCastleColor;
  const LineWidth: Single;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean);
var
  RectanglePoint: array [0..3] of TVector2;
begin
  RectanglePoint[0] := Vector2(R.Left          , R.Bottom);
  RectanglePoint[1] := Vector2(R.Left + R.Width, R.Bottom);
  RectanglePoint[2] := Vector2(R.Left + R.Width, R.Bottom + R.Height);
  RectanglePoint[3] := Vector2(R.Left          , R.Bottom + R.Height);

  DrawPrimitive2D(pmLineLoop, RectanglePoint,
    Color, BlendingSourceFactor, BlendingDestinationFactor, ForceBlending, LineWidth);
end;

procedure DrawRectangleOutline(const R: TRectangle; const Color: TCastleColor;
  const LineWidth: Single;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean);
begin
  DrawRectangleOutline(FloatRectangle(R), Color, LineWidth,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

{ DrawCircle ----------------------------------------------------------------- }

procedure DrawCircle(const Middle: TVector2;
  const HorizontalRadius, VerticalRadius: Single;
  const Color: TCastleColor;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false);
var
  CirclePoint: array of TVector2;
  PreviousSin, PreviousCos, NextSin, NextCos: Single;
  I: Integer;
begin
  SetLength(CirclePoint, CircleSlices * 3);
  SinCos(0, PreviousSin, PreviousCos);
  for I := 1 to CircleSlices do
  begin
    if I = CircleSlices then
      { do not depend that SinCos(2 * Pi, ...) is precisely equal to SinCos(0, ...) }
      SinCos(0, NextSin, NextCos)
    else
      SinCos(2 * Pi * I / CircleSlices, NextSin, NextCos);
    CirclePoint[(I - 1) * 3    ] := Vector2(
      Middle.X,
      Middle.Y);
    CirclePoint[(I - 1) * 3 + 1] := Vector2(
      Middle.X + PreviousSin * HorizontalRadius,
      Middle.Y + PreviousCos * VerticalRadius);
    CirclePoint[(I - 1) * 3 + 2] := Vector2(
      Middle.X + NextSin     * HorizontalRadius,
      Middle.Y + NextCos     * VerticalRadius);
    PreviousSin := NextSin;
    PreviousCos := NextCos;
  end;

  DrawPrimitive2D(pmTriangleFan, CirclePoint,
    Color, BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

procedure DrawCircle(const Middle: TVector2Integer;
  const HorizontalRadius, VerticalRadius: Integer;
  const Color: TCastleColor;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false);
begin
  DrawCircle(Vector2(Middle), HorizontalRadius, VerticalRadius,
    Color, CircleSlices,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

procedure DrawCircleOutline(const Middle: TVector2;
  const HorizontalRadius, VerticalRadius: Single;
  const Color: TCastleColor;
  const LineWidth: Single = 1;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false);
var
  CirclePoint: array of TVector2;
  S, C: Single;
  I: Integer;
begin
  SetLength(CirclePoint, CircleSlices);
  for I := 0 to CircleSlices - 1 do
  begin
    SinCos(2 * Pi * I / (CircleSlices - 1), S, C);
    CirclePoint[I] := Vector2(
      Middle.X + S * HorizontalRadius,
      Middle.Y + C * VerticalRadius);
  end;

  DrawPrimitive2D(pmLineLoop, CirclePoint,
    Color, BlendingSourceFactor, BlendingDestinationFactor, ForceBlending, LineWidth);
end;

procedure DrawCircleOutline(const Middle: TVector2Integer;
  const HorizontalRadius, VerticalRadius: Integer;
  const Color: TCastleColor;
  const LineWidth: Single = 1;
  const CircleSlices: Cardinal = DefaultCircleSlices;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false);
begin
  DrawCircleOutline(Vector2(Middle), HorizontalRadius, VerticalRadius,
    Color, LineWidth, CircleSlices,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

{ GLFadeRectangle ------------------------------------------------------------ }

procedure GLFadeRectangleDark(const X1, Y1, X2, Y2: Integer;
  const AFadeColor: TVector3; const FadeIntensity: Single);
begin
  GLFadeRectangleDark(Rectangle(X1, Y1, X2 - X1, Y2 - Y1),
    Vector4(AFadeColor, 1.0), FadeIntensity);
end;

procedure GLFadeRectangleDark(const Rect: TRectangle;
  const AFadeColor: TCastleColor; const FadeIntensity: Single);
var
  Color: TCastleColor;
begin
  if FadeIntensity > 0 then
  begin
    Color := FadeDarkColor(AFadeColor, FadeIntensity);
    DrawRectangle(Rect, Color, bsZero, bdSrcColor, true);
  end;
end;

procedure GLFadeRectangleDark(const Rect: TFloatRectangle;
  const AFadeColor: TCastleColor; const FadeIntensity: Single);
begin
  GLFadeRectangleDark(Rect.Round, AFadeColor, FadeIntensity);
end;

procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const AFadeColor: TVector3; const FadeIntensity: Single);
begin
  {$warnings off} // using deprecated in deprecated
  GLFadeRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1),
    Vector4(AFadeColor, 1.0), FadeIntensity);
  {$warnings on}
end;

procedure GLFadeRectangle(const Rect: TRectangle;
  const AFadeColor: TCastleColor; const FadeIntensity: Single);
var
  Color: TCastleColor;
begin
  if FadeIntensity > 0 then
  begin
    Color := FadeDarkColor(AFadeColor, FadeIntensity);
    DrawRectangle(Rect, Color, bsZero, bdSrcColor, true);
  end;
end;

procedure GLFadeRectangleLight(const Rect: TRectangle;
  const AFadeColor: TCastleColor; const FadeIntensity: Single);
var
  Color: TCastleColor;
begin
  if FadeIntensity > 0 then
  begin
    Color := FadeColor(AFadeColor, FadeIntensity);
    DrawRectangle(Rect, Color, bsSrcAlpha, bdOneMinusSrcAlpha, true);
  end;
end;

procedure GLFadeRectangleLight(const Rect: TFloatRectangle;
  const AFadeColor: TCastleColor; const FadeIntensity: Single);
begin
  GLFadeRectangleLight(Rect.Round, AFadeColor, FadeIntensity);
end;

{ GLBlendRectangle ----------------------------------------------------------- }

procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor;
  const Color: TVector4);
begin
  DrawRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1), Color,
    SourceFactor, DestinationFactor, true);
end;

procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4);
begin
  DrawRectangle(Rect, Color, bsOne, bdSrcAlpha, true);
end;

{ GLBlendFunction ------------------------------------------------------------ }

procedure GLBlendFunction(
  SourceFactor: TBlendingSourceFactor;
  DestinationFactor: TBlendingDestinationFactor);
begin
  {$ifndef OpenGLES}
  if not GLFeatures.Version_1_4 then
  begin
    if SourceFactor in [bsSrcColor, bsOneMinusSrcColor] then
    begin
      WritelnWarning('Blending', 'Blending factor "%s" as "source" requires OpenGL 1.4', [
        BlendingSourceFactorNames[SourceFactor]
      ]);
      SourceFactor := bsSrcAlpha;
    end;

    if DestinationFactor in [bdDstColor, bdOneMinusDstColor] then
    begin
      WritelnWarning('Blending', 'Blending factor "%s" as "destination" requires OpenGL 1.4', [
        BlendingDestinationFactorNames[DestinationFactor]
      ]);
      DestinationFactor := bdOneMinusSrcAlpha;
    end;
  end;
  {$endif}

  glBlendFunc(
    BlendingSourceFactorToGL[SourceFactor],
    BlendingDestinationFactorToGL[DestinationFactor]);
end;

{$endif read_implementation}
