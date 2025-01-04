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

  This version uses TWASIHostApplication, which provides a few convenience
  things to use WebAssembly (compared to more basic TBrowserApplication).

  See https://castle-engine.io/web for usage docs of engine developers.
  This gets automatically generated and build by
  "castle-engine compile --target=web" tool.
}

uses Classes, SysUtils, Math,
  // pas2js-specific units
  WasiEnv, Web, BrowserApp, WebWidget, HtmlWidgets, JS,
  WebAssembly, WasiHostApp, BrowserConsole, JOB_Browser;

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
  // Necessary when the WASM we run is "library", not "program"
  RunEntryFunction := '_initialize';
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
    Initializing FJsBridge makes the CastleInternalJobWeb (on top of Job.JS)
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

  WriteLn('Pas2js program finished. (But registered callbacks, like when WebAssembly accesses environment exposed by JS, may still execute our code.)');

  { Important: *Do not* free Application instance here!
    It would make WebAssembly program fail, as it tries to acccess
    memory and the JS application no longer exists.
    This Pas2js application ends very quickly, usually before xxx.wasm
    even starts. }
  //FreeAndNil(Application);
end.
