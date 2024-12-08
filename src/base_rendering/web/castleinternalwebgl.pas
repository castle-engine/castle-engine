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
{ Access WebGL API from Castle Game Engine compiled in WebAssembly. }
unit CastleInternalWebGL;

interface

procedure RunWebGLAnimation;

implementation

uses SysUtils, JOB.Shared, JOB.JS,
  CastleInternalJobWeb, CastleLog, CastleUtils;

{ Rendering and animation using WebGL ---------------------------------------- }

var
  { Variables set by GLContextOpen, read-only (for now) by other code }
  Canvas: IJSHTMLCanvasElement;
  GL: IJSWebGLRenderingContext;
  ContextWidth, ContextHeight: TGLSizeI;

procedure GLContextOpen;

	function StrArrayJoin(const A: TUnicodeStringDynArray): String;
	var
		I: Integer;
	begin
		Result := '';
		for I := 0 to A.Length - 1 do
      // TODO: TJSArray._GetStrings error, should Exit when result is string, submit
			// Result += A.Strings[I] + ', ';
      Result += A.Elements[I].AsString + ', ';
	end;

begin
  Canvas := TJSHTMLCanvasElement.Cast(JSDocument.getElementById('castle-canvas'));

  GL := TJSWebGLRenderingContext.Cast(canvas.getContext('webgl'));
  if GL = nil then
    raise Exception.Create('Failed to load WebGL context from WASM');

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
    GL.GetParameter(TJSWebGLRenderingContext.SHADING_LANGUAGE_VERSION),
    GL.GetParameter(TJSWebGLRenderingContext.RENDERER),
    GL.GetParameter(TJSWebGLRenderingContext.VENDOR),
    GL.GetParameter(TJSWebGLRenderingContext.VERSION)
  ]));
  WritelnLog('Context limits:' + NL +
    //'  Max Viewport Dimensions: %d' + NL +
    '  Max Texture Size: %d' + NL +
    '  Max Texture Image Units: %d' + NL +
    '  Max Vertex Texture Image Units: %d' + NL +
    '  Max Combined Texture Image Units: %d' + NL +
    '  Max Vertex Uniform Vectors: %d' + NL +
    '  Max Fragment Uniform Vectors: %d' + NL +
    '  Max Varying Vectors: %d' + NL +
    '  Max Vertex Attributes: %d', [
    //Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_VIEWPORT_DIMS)), // how to get 2D int vector?
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_TEXTURE_SIZE)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_TEXTURE_IMAGE_UNITS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_VERTEX_TEXTURE_IMAGE_UNITS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_COMBINED_TEXTURE_IMAGE_UNITS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_VERTEX_UNIFORM_VECTORS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_FRAGMENT_UNIFORM_VECTORS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_VARYING_VECTORS)),
    Integer(GL.GetParameter(TJSWebGLRenderingContext.MAX_VERTEX_ATTRIBS))
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

end.
