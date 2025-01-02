{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Access WebGL API from Castle Game Engine compiled in WebAssembly.

  The API is deliberately made to be compatible CastleGLES unit,
  to use this unit as a "drop-in replacement" as much as possible. }
unit CastleInternalWebGL;

interface

uses JOB.JS,
  CastleInternalJobWeb, CastleVectors;

type
  { Alias simple types to CastleInternalJobWeb types. }
  TGLenum = CastleInternalJobWeb.TGLenum;
  TGLboolean = CastleInternalJobWeb.TGLboolean;
  TGLbitfield = CastleInternalJobWeb.TGLbitfield;
  TGLbyte = CastleInternalJobWeb.TGLbyte;
  TGLshort = CastleInternalJobWeb.TGLshort;
  TGLint = CastleInternalJobWeb.TGLint;
  TGLsizei = CastleInternalJobWeb.TGLsizei;
  TGLintptr = CastleInternalJobWeb.TGLintptr;
  TGLsizeiptr = CastleInternalJobWeb.TGLsizeiptr;
  TGLubyte = CastleInternalJobWeb.TGLubyte;
  TGLushort = CastleInternalJobWeb.TGLushort;
  TGLuint = CastleInternalJobWeb.TGLuint;
  TGLfloat = CastleInternalJobWeb.TGLfloat;
  TGLclampf = CastleInternalJobWeb.TGLclampf;

const
  { Define GL_TRUE/FALSE for compatibility with CastleGLES.
    In WebGL, as defined in CastleInternalJobWeb, these are just Pascal booleans. }
  GL_TRUE = true;
  GL_FALSE = false;

{$define read_interface}
{$I castleinternalwebgl_flat_api.inc}
{$undef read_interface}

{ Routines providing WebGL "flat" API that looks deliberately similar to
  CastleGLES unit. }

function glGetString(const Param: TGLenum): String;

{ Helper routines specific to WebGL }

procedure RunWebGLAnimation;

var
  GL: IJSWebGLRenderingContext;
  GL2: IJSWebGL2RenderingContext;

function MatrixToWebGL(const M: TMatrix2): IJSFloat32Array;
function MatrixToWebGL(const M: TMatrix3): IJSFloat32Array;
function MatrixToWebGL(const M: TMatrix4): IJSFloat32Array;

implementation

uses SysUtils, JOB.Shared,
  CastleLog, CastleUtils,
  // TODO: not necessary in the future? only for some test queries below
  CastleInternalGLUtils;

{$push}
  // Hide: Warning: Implicit string type conversion from "AnsiString" to "UnicodeString"
  {$warn 4105 off}
  // Hide: Warning: Implicit string type conversion with potential data loss from "UnicodeString" to "AnsiString"
  {$warn 4104 off}

  {$define read_implementation}
  {$I castleinternalwebgl_flat_api.inc}
  {$undef read_implementation}
{$pop}

function glGetString(const Param: TGLenum): String;
begin
  Result := GL.GetParameter(Param);
end;

{ Rendering and animation using WebGL ---------------------------------------- }

var
  { Variables set by GLContextOpen, read-only (for now) by other code }
  Canvas: IJSHTMLCanvasElement;
  ContextWidth, ContextHeight: TGLSizeI;

procedure GLContextOpen;

	function StrArrayJoin(const A: CastleInternalJobWeb.TUnicodeStringDynArray): String;
	var
		I: Integer;
	begin
		Result := '';
		for I := 0 to A.Length - 1 do
      // TODO: TJSArray._GetStrings is not OK, see https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/893
			// Result += A.Strings[I] + ', ';
      Result += A.Elements[I].AsString + ', ';
	end;

begin
  Canvas := TJSHTMLCanvasElement.Cast(JSDocument.getElementById('castle-canvas'));

  GL2 := TJSWebGL2RenderingContext.Cast(canvas.getContext('webgl2'));
  if GL2 <> nil then
  begin
    // WebGL 2.0 is a superset of WebGL 1.0
    GL := TJSWebGLRenderingContext.Cast(GL2);
    WritelnLog('WebGL 2.0 context initialized from WebAssembly');
  end else
  begin
    GL := TJSWebGLRenderingContext.Cast(canvas.getContext('webgl'));
    if GL = nil then
      raise Exception.Create('Failed to load WebGL context (2.0 or 1.0) from WebAssembly');
    WritelnLog('WebGL 1.0 context initialized from WebAssembly');
  end;

  ContextWidth := GL.drawingBufferWidth;
  ContextHeight := GL.drawingBufferHeight;

  WritelnLog('WebGL context initialized from WebAssembly');
  WritelnLog(Format('Context Width x Height: %d x %d', [
    ContextWidth,
    ContextHeight
  ]));
  WritelnLog(Format('Context basic information:' + NL +
    '  Version: %s' + NL +
    '  Shading Language Version: %s' + NL +
    '  Renderer: %s' + NL +
    '  Vendor: %s', [
    glGetString(TJSWebGLRenderingContext.SHADING_LANGUAGE_VERSION),
    glGetString(TJSWebGLRenderingContext.RENDERER),
    glGetString(TJSWebGLRenderingContext.VENDOR),
    glGetString(TJSWebGLRenderingContext.VERSION)
  ]));
  WritelnLog('Context limits:' + NL +
    '  Max Viewport Dimensions: %s' + NL +
    '  Max Texture Size: %d' + NL +
    '  Max Texture Image Units: %d' + NL +
    '  Max Vertex Texture Image Units: %d' + NL +
    '  Max Combined Texture Image Units: %d' + NL +
    '  Max Vertex Uniform Vectors: %d' + NL +
    '  Max Fragment Uniform Vectors: %d' + NL +
    '  Max Varying Vectors: %d' + NL +
    '  Max Vertex Attributes: %d', [
    glGetInteger2(TJSWebGLRenderingContext.MAX_VIEWPORT_DIMS).ToString,
    glGetInteger(TJSWebGLRenderingContext.MAX_TEXTURE_SIZE),
    glGetInteger(TJSWebGLRenderingContext.MAX_TEXTURE_IMAGE_UNITS),
    glGetInteger(TJSWebGLRenderingContext.MAX_VERTEX_TEXTURE_IMAGE_UNITS),
    glGetInteger(TJSWebGLRenderingContext.MAX_COMBINED_TEXTURE_IMAGE_UNITS),
    glGetInteger(TJSWebGLRenderingContext.MAX_VERTEX_UNIFORM_VECTORS),
    glGetInteger(TJSWebGLRenderingContext.MAX_FRAGMENT_UNIFORM_VECTORS),
    glGetInteger(TJSWebGLRenderingContext.MAX_VARYING_VECTORS),
    glGetInteger(TJSWebGLRenderingContext.MAX_VERTEX_ATTRIBS)
  ]);
  WritelnLog('Context attributes:' + NL +
    '  Alpha: %s' + NL +
    '  Depth: %s' + NL +
    '  Stencil: %s' + NL +
    '  Antialias: %s' + NL +
    '  PremultipliedAlpha: %s' + NL +
    '  PreserveDrawingBuffer: %s' + NL +
    '  PowerPreference: %s' + NL +
    '  FailIfMajorPerformanceCaveat: %s', [
    BoolToStr(GL.getContextAttributes.alpha, true),
    BoolToStr(GL.getContextAttributes.depth, true),
    BoolToStr(GL.getContextAttributes.stencil, true),
    BoolToStr(GL.getContextAttributes.antialias, true),
    BoolToStr(GL.getContextAttributes.premultipliedAlpha, true),
    BoolToStr(GL.getContextAttributes.preserveDrawingBuffer, true),
    GL.getContextAttributes.powerPreference,
    BoolToStr(GL.getContextAttributes.failIfMajorPerformanceCaveat, true)
  ]);
  WritelnLog('Supported Extensions: ' + NL +
    '  Count: %d' + NL +
    '  List: %s', [
    GL.getSupportedExtensions.Length,
    StrArrayJoin(GL.getSupportedExtensions)
  ]);
end;

var
  LifeTime: Double = 0;

procedure Update(const DeltaTime: Float);
begin
  LifeTime += DeltaTime;
end;

procedure Render;
var
  Col: Float;
begin
  //Col := Frac(LifeTime); // TODO: animate
  Col := 1.0;

  GL.clearColor(Col, Col, 0.0, 1);
  GL.viewport(0, 0, ContextWidth, ContextHeight);
  GL.clear(TJSWebGLRenderingContext.COLOR_BUFFER_BIT);
  GL.flush; // TODO: remove later, pointless when using requestAnimationFrame, according to MDN
end;

// TODO

(*

var
  CanvasAnimationHandler: Integer = 0;
  HasPreviousTime: Boolean = False;
  PreviousTime: TJSDOMHighResTimeStamp;

{ Callback for requestAnimationFrame
  ( https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame ) }
procedure AnimationFrame(Time: TJSDOMHighResTimeStamp);
var
  DeltaTime: Float;
begin
  { TODO: recreate context.
    See https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/isContextLost }
	if GL.isContextLost then
    raise Exception.Create('Context has been lost');

  // calculate DeltaTime, update HasPreviousTime and PreviousTime
  if HasPreviousTime then
    DeltaTime := (Time - PreviousTime) / 1000
  else
  begin
    DeltaTime := 1 / 60; // arbitrary initial value
    HasPreviousTime := True;
  end;
  PreviousTime := Time;

  // update and render
	Update(DeltaTime);
	Render;

  // schedule next frame
	if CanvasAnimationHandler <> 0 then
		CanvasAnimationHandler := window.requestAnimationFrame(@AnimationFrame);
end;
*)
procedure RunWebGLAnimation;
begin
  GLContextOpen;

  // TODO: just run once Render, for now
  Render;

  // TODO
	//CanvasAnimationHandler := window.requestAnimationFrame(@AnimationFrame);
end;

function MatrixToWebGL(const M: TMatrix2): IJSFloat32Array;
var
  Cols, Rows, Col, Row: Integer;
begin
  Cols := High(TMatrix2.TIndex) + 1;
  Rows := Cols; // square matrix
  Result := TJSFloat32Array.Create(Cols * Rows);

  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := 0 to Cols -1 do
    for Row := 0 to Rows - 1 do
      Result[Col * Cols + Row] := M[Col, Row];
end;

function MatrixToWebGL(const M: TMatrix3): IJSFloat32Array;
var
  Cols, Rows, Col, Row: Integer;
begin
  Cols := High(TMatrix3.TIndex) + 1;
  Rows := Cols; // square matrix
  Result := TJSFloat32Array.Create(Cols * Rows);

  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := 0 to Cols -1 do
    for Row := 0 to Rows - 1 do
      Result[Col * Cols + Row] := M[Col, Row];
end;

function MatrixToWebGL(const M: TMatrix4): IJSFloat32Array;
var
  Cols, Rows, Col, Row: Integer;
begin
  Cols := High(TMatrix4.TIndex) + 1;
  Rows := Cols; // square matrix
  Result := TJSFloat32Array.Create(Cols * Rows);

  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := 0 to Cols -1 do
    for Row := 0 to Rows - 1 do
      Result[Col * Cols + Row] := M[Col, Row];
end;

end.
