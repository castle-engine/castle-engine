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

{ Use Pas2js to run a WebAssembly program.
  This allows Pas2js to define WebAssembly environment,
  e.g. where does Pascal Writeln go, expose to WebAssembly API available
  from Pas2js (like WebGL).

  Follows
  https://www.freepascal.org/~michael/articles/fpcwasm1/fpcwasm1.pdf

  This version uses TWASIHostApplication , which provides a few convenience
  things to use WebAssembly (compared to more basic TBrowserApplication).

  See https://castle-engine.io/web for usage docs of engine developers.
  This gets automatically generated and build by
  "castle-engine compile --target=web" tool.

  This also initializes WebGL context and for now
  (TODO - this should rely on WebAssembly code)
  does sample rendering, update and animation loop.
}

uses Classes, SysUtils, Math,
  // pas2js-specific units
  WasiEnv, Web, BrowserApp, WebWidget, HtmlWidgets, JS,
  WebAssembly, WasiHostApp, BrowserConsole, WebGL, JOB_Browser;

{ Rendering and animation using WebGL from Pas2js -------------------------- }

var
  { Variables set by GLContextOpen, read-only (for now) by other code }
	GL: TJSWebGLRenderingContext;
  Canvas: TJSHTMLCanvasElement;
	ContextWidth, ContextHeight: GLSizeI;

procedure GLContextOpen;

	function StrArrayJoin(const A: TStringDynArray): String;
	var
		I: Integer;
	begin
		Result := '';
		for I := 0 to High(A) do
			Result += A[I] + ', ';
	end;

begin
  Canvas := TJSHTMLCanvasElement(document.getElementById('castle-canvas'));

	GL := TJSWebGLRenderingContext(canvas.getContext('webgl'));
	if GL = nil then
	  raise Exception.Create('Failed to load WebGL');

	ContextWidth := GL.drawingBufferWidth;
	ContextHeight := GL.drawingBufferHeight;

	Writeln('Context Width x Height: ', ContextWidth, ' x ', ContextHeight);
	Writeln('Context basic information:', LineEnding,
	  '  Version: ', GL.GetParameter(GL.VERSION), LineEnding,
		'  Shading Language Version: ', GL.GetParameter(GL.SHADING_LANGUAGE_VERSION), LineEnding,
		'  Renderer: ', GL.GetParameter(GL.RENDERER), LineEnding,
		'  Vendor: ', GL.GetParameter(GL.VENDOR)
	);
	Writeln('Context limits:', LineEnding,
		'  Max Viewport Dimensions: ', GL.GetParameter(GL.MAX_VIEWPORT_DIMS), LineEnding,
		'  Max Texture Size: ', GL.GetParameter(GL.MAX_TEXTURE_SIZE), LineEnding,
		'  Max Texture Image Units: ', GL.GetParameter(GL.MAX_TEXTURE_IMAGE_UNITS), LineEnding,
		'  Max Vertex Texture Image Units: ', GL.GetParameter(GL.MAX_VERTEX_TEXTURE_IMAGE_UNITS), LineEnding,
		'  Max Combined Texture Image Units: ', GL.GetParameter(GL.MAX_COMBINED_TEXTURE_IMAGE_UNITS), LineEnding,
		'  Max Vertex Uniform Vectors: ', GL.GetParameter(GL.MAX_VERTEX_UNIFORM_VECTORS), LineEnding,
		'  Max Fragment Uniform Vectors: ', GL.GetParameter(GL.MAX_FRAGMENT_UNIFORM_VECTORS), LineEnding,
		'  Max Varying Vectors: ', GL.GetParameter(GL.MAX_VARYING_VECTORS), LineEnding,
		'  Max Vertex Attributes: ', GL.GetParameter(GL.MAX_VERTEX_ATTRIBS)
		// ...
	);
	Writeln('Context attributes: ', LineEnding,
		'  Alpha: ', GL.getContextAttributes.alpha, LineEnding,
		'  Depth: ', GL.getContextAttributes.depth, LineEnding,
		'  Stencil: ', GL.getContextAttributes.stencil, LineEnding,
		'  Antialias: ', GL.getContextAttributes.antialias, LineEnding,
		'  PremultipliedAlpha: ', GL.getContextAttributes.premultipliedAlpha, LineEnding,
		'  PreserveDrawingBuffer: ', GL.getContextAttributes.preserveDrawingBuffer, LineEnding,
		'  PowerPreference: ', GL.getContextAttributes.powerPreference, LineEnding,
		'  FailIfMajorPerformanceCaveat: ', GL.getContextAttributes.failIfMajorPerformanceCaveat
	);
	Writeln('Supported Extensions: ', LineEnding,
		'  Count: ', Length(GL.getSupportedExtensions), LineEnding,
		'  List: ', StrArrayJoin(GL.getSupportedExtensions)
	);
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
  Col := Frac(LifeTime);

  GL.clearColor(Col, Col, 0.0, 1);
  GL.viewport(0, 0, ContextWidth, ContextHeight);
  GL.clear(GL.COLOR_BUFFER_BIT);
  // GL.flush; // pointless when using requestAnimationFrame, according to MDN
end;

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

procedure RunWebGLAnimation;
begin
  GLContextOpen;
	CanvasAnimationHandler := window.requestAnimationFrame(@AnimationFrame);
end;

{ Main program -------------------------------------------------------------- }

type
  TMyApplication = class(TWASIHostApplication)
  private
    FJsBridge : TJSObjectBridge;
    function DoBeforeStart(Sender: TObject;
      ADescriptor: TWebAssemblyStartDescriptor): Boolean;
    procedure DoWrite(Sender: TObject; aOutput: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FJsBridge := TJSObjectBridge.Create(WasiEnvironment);
  OnConsoleWrite := @DoWrite;
end;

procedure TMyApplication.DoWrite(Sender: TObject; aOutput: String);
begin
  Writeln(aOutput);
end;

destructor TMyApplication.Destroy;
begin
  inherited;
end;

function TMyApplication.DoBeforeStart(Sender: TObject;
  ADescriptor: TWebAssemblyStartDescriptor): Boolean;
begin
  { Initialize FJsBridge.InstanceExports, just like
    pas2js/demo/wasienv/button/BrowserButton1.lpr does.
    Initializing FJsBridge makes the CastleJobWeb (on top of Job.JS)
    functional in WebAssembly, which allows WebAssembly to call various
    JS APIs like DOM or WebGL. }
  FJsBridge.InstanceExports := ADescriptor.Exported;
  Result := true;
end;

procedure TMyApplication.DoRun;
begin
  WriteLn('Starting WebAssembly program from ${EXECUTABLE_NAME}.wasm');
  StartWebAssembly('${EXECUTABLE_NAME}.wasm${RANDOM_URL_SUFFIX}',
    true, @DoBeforeStart);
  RunWebGLAnimation;

  { Terminate doesn't really do anything we critically need
    (Application.Run ends anyway), but makes it clear that we're done by setting
    Terminated = true. }
  Terminate;
end;

var
  Application: TMyApplication;
begin
  // customize where BrowserConsole output goes
  ConsoleElementID := 'pas2js-console-output';
  // changing ConsoleStyle doesn't work reliably, old style remains?
  // ConsoleStyle := ''; // we will style it using CSS, not by this
  HookConsole;

  Application := TMyApplication.Create(nil);
  Application.Initialize; // doesn't do anything by default
  Application.Run;

  WriteLn('Normal flow of Pas2js program finished (though registered callbacks, like requestAnimationFrame and when WASM accesses environment exposed by JS, may still run).');

  { Important: *Do not* free Application instance here!
    It would make WebAssembly program fail, as it tries to acccess
    memory and the JS application no longer exists.
    This Pas2js application ends very quickly, usually before xxx.wasm
    even starts. }
  //FreeAndNil(Application);
end.
