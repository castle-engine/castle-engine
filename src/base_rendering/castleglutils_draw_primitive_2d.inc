{%MainUnit castleglutils.pas}
{
  Copyright 2016-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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

{ RHI 2D primitive drawing statistics and state for testing and diagnostics.
  @exclude }
function RHIPrimitive2DDrawCallCount: Cardinal;
function RHIPrimitive2DLastVertexCount: Cardinal;
function RHIPrimitive2DLastSubmittedVertexCount: Cardinal;
function RHIPrimitive2DLastMode: TPrimitiveMode;
function RHIPrimitive2DLastColor: TCastleColor;
function RHIPrimitive2DLastLineWidth: Single;
function RHIPrimitive2DLastPointSize: Cardinal;
function RHIPrimitive2DBuffer: TObject;
function RHIPrimitive2DPipeline: TObject;
function RHIPrimitive2DLastStencilTestEnable: Boolean;
procedure RHIPrimitive2DResetStats;

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
  { OpenGL and RHI resources for DrawPrimitive2D.
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

    { RHI resources and diagnostic counters }
    RHIOwnerDevice: TRHIDevice;
    RHIVbo: TRHIBuffer;
    RHIVboWriteOffset: PtrUInt;
    RHILastVertexByteOffset: PtrUInt;
    RHILastFrameIndex: Cardinal;
    RHILastFramesPresented: Cardinal;
    RHILastCmdList: TRHICommandList;
    RHIVertShader: TRHIShaderModule;
    RHIFragShader: TRHIShaderModule;
    RHIPipeline: TRHIPipeline;
    RHIPipelineTopology: TRHIPrimitiveTopology;
    RHIDrawCallCount: Cardinal;
    RHILastVertexCount: Cardinal;
    RHILastSubmittedVertexCount: Cardinal;
    RHILastMode: TPrimitiveMode;
    RHILastColor: TCastleColor;
    RHILastLineWidth: Single;
    RHILastPointSize: Cardinal;
    RHILastStencilTestEnable: Boolean;
    destructor Destroy; override;
  end;

destructor TPrimitive2DResources.Destroy;
begin
  FreeBuffer(Vbo);
  FreeAndNil(Vao);
  FreeAndNil(Prog);
  FreeAndNil(RHIVbo);
  FreeAndNil(RHIVertShader);
  FreeAndNil(RHIFragShader);
  RHIPipeline := nil; // owned by RHIDevice pipeline cache
  FreeMem(PointPtr); // not tied to OpenGL, but freed together anyway
  PointPtr := nil;
  PointPtrAllocated := 0;
  inherited;
end;

var
  Primitive2DRes: TPrimitive2DResources;

function RHIPrimitive2DDrawCallCount: Cardinal;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHIDrawCallCount
  else
    Result := 0;
end;

function RHIPrimitive2DLastVertexCount: Cardinal;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastVertexCount
  else
    Result := 0;
end;

function RHIPrimitive2DLastSubmittedVertexCount: Cardinal;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastSubmittedVertexCount
  else
    Result := 0;
end;

function RHIPrimitive2DLastMode: TPrimitiveMode;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastMode
  else
    Result := pmPoints;
end;

function RHIPrimitive2DLastColor: TCastleColor;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastColor
  else
    Result := TVector4.Zero;
end;

function RHIPrimitive2DLastLineWidth: Single;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastLineWidth
  else
    Result := 1.0;
end;

function RHIPrimitive2DLastPointSize: Cardinal;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastPointSize
  else
    Result := 1;
end;

function RHIPrimitive2DBuffer: TObject;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHIVbo
  else
    Result := nil;
end;

function RHIPrimitive2DPipeline: TObject;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHIPipeline
  else
    Result := nil;
end;

function RHIPrimitive2DLastStencilTestEnable: Boolean;
begin
  if Primitive2DRes <> nil then
    Result := Primitive2DRes.RHILastStencilTestEnable
  else
    Result := False;
end;

procedure RHIPrimitive2DResetStats;
begin
  if Primitive2DRes <> nil then
  begin
    FreeAndNil(Primitive2DRes.RHIVbo);
    Primitive2DRes.RHIVboWriteOffset := 0;
    Primitive2DRes.RHILastVertexByteOffset := 0;
    Primitive2DRes.RHILastFrameIndex := 0;
    Primitive2DRes.RHILastFramesPresented := 0;
    Primitive2DRes.RHILastCmdList := nil;
    FreeAndNil(Primitive2DRes.RHIVertShader);
    FreeAndNil(Primitive2DRes.RHIFragShader);
    Primitive2DRes.RHIPipeline := nil;
    Primitive2DRes.RHIOwnerDevice := nil;
    Primitive2DRes.RHIDrawCallCount := 0;
    Primitive2DRes.RHILastVertexCount := 0;
    Primitive2DRes.RHILastSubmittedVertexCount := 0;
    Primitive2DRes.RHILastMode := pmPoints;
    Primitive2DRes.RHILastColor := TVector4.Zero;
    Primitive2DRes.RHILastLineWidth := 1.0;
    Primitive2DRes.RHILastPointSize := 1;
    Primitive2DRes.RHILastStencilTestEnable := False;
  end;
end;

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
type
  TPrimitive2DPushConstants = packed record
    ViewportSize: TVector2;
    PointSize: Single;
    Padding: Single;
    Color: TVector4;
  end;
const
  PrimitiveModeToRHI: array [TPrimitiveMode] of TRHIPrimitiveTopology = (
    ptPointList,
    ptLineStrip,
    ptLineStrip,
    ptLineList,
    ptTriangleStrip,
    ptTriangleList,
    ptTriangleList
  );
  Primitive2DVertGLSL =
    '#version 450' + LineEnding +
    'layout(location = 0) in vec2 vertex;' + LineEnding +
    'layout(push_constant) uniform PushConstants {' + LineEnding +
    '  vec2 viewport_size;' + LineEnding +
    '  float point_size;' + LineEnding +
    '  float padding;' + LineEnding +
    '  vec4 color;' + LineEnding +
    '} pc;' + LineEnding +
    'void main() {' + LineEnding +
    '  gl_Position = vec4(vertex * 2.0 / pc.viewport_size - vec2(1.0), 0.0, 1.0);' + LineEnding +
    '  gl_PointSize = pc.point_size;' + LineEnding +
    '}';
  Primitive2DFragGLSL =
    '#version 450' + LineEnding +
    'layout(push_constant) uniform PushConstants {' + LineEnding +
    '  vec2 viewport_size;' + LineEnding +
    '  float point_size;' + LineEnding +
    '  float padding;' + LineEnding +
    '  vec4 color;' + LineEnding +
    '} pc;' + LineEnding +
    'layout(location = 0) out vec4 out_Color;' + LineEnding +
    'void main() {' + LineEnding +
    '  out_Color = pc.color;' + LineEnding +
    '}';

  function MapBlendSourceFactor(const F: TBlendingSourceFactor): TRHIBlendFactor;
  begin
    case F of
      bsSrcAlpha, bsSrcAlphaSaturate, bsConstantAlpha: Result := bfSrcAlpha;
      bsOneMinusSrcAlpha, bsOneMinusConstantAlpha: Result := bfOneMinusSrcAlpha;
      bsZero: Result := bfZero;
      bsOne: Result := bfOne;
      bsDstColor: Result := bfDstColor;
      bsSrcColor, bsConstantColor: Result := bfSrcColor;
      bsDstAlpha: Result := bfDstAlpha;
      bsOneMinusDstColor: Result := bfOneMinusDstColor;
      bsOneMinusSrcColor, bsOneMinusConstantColor: Result := bfOneMinusSrcColor;
      bsOneMinusDstAlpha: Result := bfOneMinusDstAlpha;
      else Result := bfSrcAlpha;
    end;
  end;

  function MapBlendDestFactor(const F: TBlendingDestinationFactor): TRHIBlendFactor;
  begin
    case F of
      bdSrcAlpha, bdConstantAlpha: Result := bfSrcAlpha;
      bdOneMinusSrcAlpha, bdOneMinusConstantAlpha: Result := bfOneMinusSrcAlpha;
      bdZero: Result := bfZero;
      bdOne: Result := bfOne;
      bdDstColor: Result := bfDstColor;
      bdSrcColor, bdConstantColor: Result := bfSrcColor;
      bdDstAlpha: Result := bfDstAlpha;
      bdOneMinusDstColor: Result := bfOneMinusDstColor;
      bdOneMinusSrcColor, bdOneMinusConstantColor: Result := bfOneMinusSrcColor;
      bdOneMinusDstAlpha: Result := bfOneMinusDstAlpha;
      else Result := bfOneMinusSrcAlpha;
    end;
  end;

var
  Blending: boolean;
  I, RequiredPointPtrSize: Integer;
  SubmittedVertexCount: Cardinal;
  ExpandTriangleFan, ExpandLineLoop: Boolean;
  TargetPointPtr: PVector2;
  RHIBufDesc: TRHIBufferDesc;
  RHIPipeDesc: TRHIGraphicsPipelineDesc;
  RHITopology: TRHIPrimitiveTopology;
  PushConsts: TPrimitive2DPushConstants;
  ScissorR: TRectangle;
  CurrentVboOffset, NewVboCapacity: PtrUInt;
begin
  if High(Points) = -1 then Exit;

  TDrawableImage.BatchingFlush;

  FrameProfiler.Start(fmDrawPrimitive2D);

  if Primitive2DRes = nil then
    Primitive2DRes := TPrimitive2DResources.Create;

  ExpandTriangleFan := (RHIDevice <> nil) and (Mode = pmTriangleFan) and
    (Length(Points) >= 3) and
    (not ((Length(Points) >= 6) and ((Length(Points) mod 3) = 0) and
      TVector2.PerfectlyEquals(Points[0], Points[3])));
  ExpandLineLoop := (RHIDevice <> nil) and (Mode = pmLineLoop) and
    (Length(Points) >= 2);

  if ExpandTriangleFan then
    SubmittedVertexCount := Cardinal(Length(Points) - 2) * 3
  else
  if ExpandLineLoop then
    SubmittedVertexCount := Cardinal(Length(Points) + 1)
  else
    SubmittedVertexCount := Cardinal(High(Points) + 1);

  { make Primitive2DRes.PointPtr have necessary size }
  RequiredPointPtrSize := SizeOf(TVector2) * Integer(SubmittedVertexCount);
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
  if ExpandTriangleFan then
  begin
    for I := 1 to High(Points) - 1 do
    begin
      TargetPointPtr^ := Points[0];
      Inc(TargetPointPtr);
      TargetPointPtr^ := Points[I];
      Inc(TargetPointPtr);
      TargetPointPtr^ := Points[I + 1];
      Inc(TargetPointPtr);
    end;
  end else
  begin
    for I := 0 to High(Points) do
    begin
      Move(Points[I], TargetPointPtr^, SizeOf(TVector2));
      Inc(TargetPointPtr);
    end;
    if ExpandLineLoop then
    begin
      TargetPointPtr^ := Points[0];
      Inc(TargetPointPtr);
    end;
  end;

  if RHIDevice <> nil then
  begin
    if (Primitive2DRes.RHIOwnerDevice <> RHIDevice) or
       ((Primitive2DRes.RHIVbo <> nil) and (Primitive2DRes.RHIVbo.Device <> RHIDevice)) or
       ((Primitive2DRes.RHIVertShader <> nil) and (Primitive2DRes.RHIVertShader.Device <> RHIDevice)) then
    begin
      FreeAndNil(Primitive2DRes.RHIVbo);
      Primitive2DRes.RHIVboWriteOffset := 0;
      Primitive2DRes.RHILastVertexByteOffset := 0;
      Primitive2DRes.RHILastFrameIndex := 0;
      Primitive2DRes.RHILastFramesPresented := 0;
      Primitive2DRes.RHILastCmdList := nil;
      FreeAndNil(Primitive2DRes.RHIVertShader);
      FreeAndNil(Primitive2DRes.RHIFragShader);
      Primitive2DRes.RHIPipeline := nil;
      Primitive2DRes.RHIOwnerDevice := RHIDevice;
    end;
    if Primitive2DRes.RHIVertShader = nil then
      Primitive2DRes.RHIVertShader := RHIDevice.CreateShaderModuleFromGLSL(
        ssVertex, Primitive2DVertGLSL, 'Primitive2D_VS');
    if Primitive2DRes.RHIFragShader = nil then
      Primitive2DRes.RHIFragShader := RHIDevice.CreateShaderModuleFromGLSL(
        ssFragment, Primitive2DFragGLSL, 'Primitive2D_FS');

    if (Primitive2DRes.RHILastFrameIndex <> RHIDevice.FrameIndex) or
       (Primitive2DRes.RHILastFramesPresented <> RHIDevice.TotalFramesStarted) or
       (Primitive2DRes.RHILastCmdList <> ActiveCommandList) then
    begin
      Primitive2DRes.RHIVboWriteOffset := 0;
      Primitive2DRes.RHILastFrameIndex := RHIDevice.FrameIndex;
      Primitive2DRes.RHILastFramesPresented := RHIDevice.TotalFramesStarted;
      Primitive2DRes.RHILastCmdList := ActiveCommandList;
    end;

    CurrentVboOffset := Primitive2DRes.RHIVboWriteOffset;
    if (Primitive2DRes.RHIVbo = nil) or
       (Primitive2DRes.RHIVbo.Desc.Size < CurrentVboOffset + PtrUInt(RequiredPointPtrSize)) then
    begin
      FreeAndNil(Primitive2DRes.RHIVbo);
      NewVboCapacity := Max(PtrUInt(65536), (CurrentVboOffset + PtrUInt(RequiredPointPtrSize)) * 2);
      RHIBufDesc := TRHIBufferDesc.Create(NewVboCapacity, [buVertex, buTransferDst], muCpuToGpu);
      Primitive2DRes.RHIVbo := RHIDevice.CreateBuffer(RHIBufDesc, nil);
      CurrentVboOffset := 0;
      Primitive2DRes.RHIVbo.UploadData(Primitive2DRes.PointPtr^, RequiredPointPtrSize, 0);
    end else
      Primitive2DRes.RHIVbo.UploadData(Primitive2DRes.PointPtr^, RequiredPointPtrSize, CurrentVboOffset);

    Primitive2DRes.RHILastVertexByteOffset := CurrentVboOffset;
    Primitive2DRes.RHIVboWriteOffset := (CurrentVboOffset + PtrUInt(RequiredPointPtrSize) + 15) and not PtrUInt(15);

    RHITopology := PrimitiveModeToRHI[Mode];
    FillChar(RHIPipeDesc, SizeOf(RHIPipeDesc), 0);
    RHIPipeDesc.VertexShader := Primitive2DRes.RHIVertShader;
    RHIPipeDesc.FragmentShader := Primitive2DRes.RHIFragShader;
    RHIPipeDesc.Topology := RHITopology;
    SetLength(RHIPipeDesc.VertexInput.Bindings, 1);
    RHIPipeDesc.VertexInput.Bindings[0].Binding := 0;
    RHIPipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TVector2);
    RHIPipeDesc.VertexInput.Bindings[0].PerInstance := False;
    SetLength(RHIPipeDesc.VertexInput.Attributes, 1);
    RHIPipeDesc.VertexInput.Attributes[0].Location := 0;
    RHIPipeDesc.VertexInput.Attributes[0].Binding := 0;
    RHIPipeDesc.VertexInput.Attributes[0].Format := vafVector2;
    RHIPipeDesc.VertexInput.Attributes[0].Offset := 0;
    RHIPipeDesc.Rasterizer.CullMode := cmNone;
    RHIPipeDesc.Rasterizer.FrontFace := ffCounterClockwise;
    RHIPipeDesc.Rasterizer.PolygonMode := pmFill;
    RHIPipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
    if ForceBlending or (Color.W < 1) then
    begin
      RHIPipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
      RHIPipeDesc.BlendState.SrcColorBlendFactor := MapBlendSourceFactor(BlendingSourceFactor);
      RHIPipeDesc.BlendState.DstColorBlendFactor := MapBlendDestFactor(BlendingDestinationFactor);
    end else
      RHIPipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
    if RenderContext <> nil then
    begin
      RenderContext.LineWidth := LineWidth;
      RenderContext.PointSize := PointSize;
      RenderContext.ApplyRHIStencilAndColorMask(RHIPipeDesc.DepthStencil, RHIPipeDesc.BlendState);
    end;
    RHIPipeDesc.PushConstantSize := SizeOf(TPrimitive2DPushConstants);
    RHIPipeDesc.PushConstantStages := [ssVertex, ssFragment];
    if (ActiveCommandList <> nil) and ActiveCommandList.IsRendering then
    begin
      if ActiveCommandList.ActiveRenderingHasColor then
      begin
        SetLength(RHIPipeDesc.ColorAttachmentFormats, 1);
        RHIPipeDesc.ColorAttachmentFormats[0] := ActiveCommandList.ActiveRenderingColorFormat;
      end else
        SetLength(RHIPipeDesc.ColorAttachmentFormats, 0);
      if ActiveCommandList.ActiveRenderingHasDepth then
        RHIPipeDesc.DepthAttachmentFormat := ActiveCommandList.ActiveRenderingDepthFormat
      else
        RHIPipeDesc.DepthAttachmentFormat := pfUndefined;
      RHIPipeDesc.SampleCount := Max(Cardinal(1), ActiveCommandList.ActiveRenderingSampleCount);
    end else
    begin
      SetLength(RHIPipeDesc.ColorAttachmentFormats, 1);
      if RHIDevice.GetSwapchainColorTexture <> nil then
        RHIPipeDesc.ColorAttachmentFormats[0] := RHIDevice.GetSwapchainColorTexture.Desc.Format
      else
        RHIPipeDesc.ColorAttachmentFormats[0] := pfBGRA8_UNorm;
      if RHIDevice.GetSwapchainDepthTexture <> nil then
        RHIPipeDesc.DepthAttachmentFormat := RHIDevice.GetSwapchainDepthTexture.Desc.Format
      else
        RHIPipeDesc.DepthAttachmentFormat := pfD24_UNorm_S8_UInt;
      RHIPipeDesc.SampleCount := 1;
    end;
    if RHIPipeDesc.DepthStencil.StencilTestEnable and
       not (RHIPipeDesc.DepthAttachmentFormat in [pfD24_UNorm_S8_UInt, pfD32_Float_S8_UInt]) then
      RHIPipeDesc.DepthAttachmentFormat := pfD24_UNorm_S8_UInt;
    Primitive2DRes.RHIPipeline := RHIDevice.GetOrCreatePipeline(RHIPipeDesc);
    Primitive2DRes.RHIPipelineTopology := RHITopology;

    Inc(Primitive2DRes.RHIDrawCallCount);
    Primitive2DRes.RHILastVertexCount := High(Points) + 1;
    Primitive2DRes.RHILastSubmittedVertexCount := SubmittedVertexCount;
    Primitive2DRes.RHILastMode := Mode;
    Primitive2DRes.RHILastColor := Color;
    Primitive2DRes.RHILastLineWidth := LineWidth;
    Primitive2DRes.RHILastPointSize := PointSize;
    Primitive2DRes.RHILastStencilTestEnable := RHIPipeDesc.DepthStencil.StencilTestEnable;

    if ActiveCommandList <> nil then
    begin
      ActiveCommandList.BindPipeline(Primitive2DRes.RHIPipeline);
      ActiveCommandList.SetLineWidth(LineWidth);
      if (RenderContext <> nil) and RenderContext.FinalScissor(ScissorR) then
        ActiveCommandList.SetScissor(ScissorR);
      if (RenderContext <> nil) and (RenderContext.Viewport.Width > 0) and (RenderContext.Viewport.Height > 0) then
        PushConsts.ViewportSize := Vector2(RenderContext.Viewport.Width, RenderContext.Viewport.Height)
      else
      if ActiveCommandList.IsRendering and
         (ActiveCommandList.ActiveRenderingInfo.RenderArea.Width > 0) and
         (ActiveCommandList.ActiveRenderingInfo.RenderArea.Height > 0) then
        PushConsts.ViewportSize := Vector2(
          ActiveCommandList.ActiveRenderingInfo.RenderArea.Width,
          ActiveCommandList.ActiveRenderingInfo.RenderArea.Height)
      else
        PushConsts.ViewportSize := Vector2(1920, 1080);
      PushConsts.PointSize := Max(1.0, Single(PointSize));
      PushConsts.Padding := 0.0;
      PushConsts.Color := Color;
      ActiveCommandList.PushConstants([ssVertex, ssFragment], 0, SizeOf(PushConsts), PushConsts);
      ActiveCommandList.BindVertexBuffers(0, [Primitive2DRes.RHIVbo], [CurrentVboOffset]);
      ActiveCommandList.Draw(SubmittedVertexCount, 1, 0, 0);
    end;

    if (ActiveCommandList <> nil) or (GLFeatures = nil) or (ApplicationProperties = nil) or (not ApplicationProperties.IsGLContextOpen) then
    begin
      FrameProfiler.Stop(fmDrawPrimitive2D);
      Exit;
    end;
  end;

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
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    glLineStipple(1, LineStipple.Pattern);
    glEnable(GL_LINE_STIPPLE);
  end;
  {$endif}

  DrawPrimitive2D(Mode, Points, Color,
    BlendingSourceFactor, BlendingDestinationFactor, ForceBlending,
    LineWidth, PointSize);

  {$ifndef OpenGLES}
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
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
