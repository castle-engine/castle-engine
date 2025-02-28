{
  Copyright 2024-2025 Michalis Kamburelis.

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

    { Update HTML progress bar to show given progress. }
    procedure UpdateProgress(const ALoaded, ATotal: Integer);
    { Update HTML castle-loading-into to show the error to user. }
    procedure ShowFatalError(const Message: String);

    { Handlers for TXMLHttpRequest events. }
    procedure DataLoaded(Event: TJSProgressEvent);
    procedure DataError(Event: TJSProgressEvent);
    procedure DataAbort(Event: TJSProgressEvent);
    procedure DataProgress(Event: TJSProgressEvent);
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

procedure TMyApplication.DoRun;
var
  Xhr: TJSXMLHttpRequest;
  DataUrl: String;
begin
  DataUrl := '${NAME}_data.zip${RANDOM_URL_SUFFIX}';
  WriteLn('Downloading application data from ' + DataUrl);

  { We download the application data from pas2js,
    and pass it to the WebAssembly program by setting a global
    variable.
    This is easy, and it allows the WASM program to execute similar
    to desktop: it can just assume that data has been read,edt
    it doesn't need to wait before Application.OnInitialize etc. }

  Xhr := TJSXMLHttpRequest.New;
  Xhr.Open('GET', DataUrl);
  Xhr.ResponseType := 'arraybuffer';
  Xhr.AddEventListener('load', @DataLoaded);
  Xhr.AddEventListener('error', @DataError);
  Xhr.AddEventListener('abort', @DataAbort);
  Xhr.AddEventListener('progress', @DataProgress);
  Xhr.Send;

  { Terminate doesn't really do anything we critically need
    (Application.Run ends anyway), but makes it clear that we're done by setting
    Terminated = true. }
  Terminate;
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

const
  HtmlNL = '<br>';
  DataErrorSuffix = HtmlNL +
    'Make sure the data zip is available at the correct URL and served by the HTTP server.' + HtmlNL +
    'See the Developer Tools (F12) console for more information.';

procedure TMyApplication.DataLoaded(Event: TJSProgressEvent);

  procedure DoStartWebAssembly;
  begin
    WriteLn('Starting WebAssembly program from ${EXECUTABLE_NAME}.wasm');
    StartWebAssembly('${EXECUTABLE_NAME}.wasm${RANDOM_URL_SUFFIX}',
      true, @DoBeforeStart);
  end;

var
  Xhr: TJSXMLHttpRequest;
  ArrayResponse: TJSArrayBuffer;
begin
  Xhr := TJSXMLHttpRequest(Event.Target);
  if Xhr.Status = 200 then
  begin
    UpdateProgress(Event.Loaded, Event.Total);
    ArrayResponse := TJSArrayBuffer(Xhr.Response);
    WriteLn('Downloaded data successfully, size: ', ArrayResponse.byteLength);
    { Pass the TJSArrayBuffer to WebAssembly through a global variable that
      is read by the WebAssembly program.
      This seems simplest and it means we utilize JOB to pass
      pas2js TJSArrayBuffer -> web assembly IJSArrayBuffer.
      This seems simpler than own TImportExtension, where it's easy to expose
      functions but only with simple parameters, not so easy to pass blob
      of binary data. }
    Document.Properties['CastleApplicationData'] := ArrayResponse;
    DoStartWebAssembly;
  end else
  begin
    ShowFatalError(Format('Failed to download data: received HTTP status %d.' + DataErrorSuffix, [
      Xhr.Status
    ]));
  end;
end;

procedure TMyApplication.DataError(Event: TJSProgressEvent);
begin
  ShowFatalError('Failed to download data.' + DataErrorSuffix);
end;

procedure TMyApplication.DataAbort(Event: TJSProgressEvent);
begin
  ShowFatalError('Failed to download data: aborted.' + DataErrorSuffix);
end;

procedure TMyApplication.DataProgress(Event: TJSProgressEvent);
begin
  UpdateProgress(Event.Loaded, Event.Total);
end;

procedure TMyApplication.UpdateProgress(const ALoaded, ATotal: Integer);
var
  {LoadingProgressContainer,} LoadingProgressBar, LoadingInfo: TJSHTMLElement;
  PercentProgress: Integer;
begin
  // WriteLn(Format('Downloaded %d of %d bytes', [ALoaded, ATotal]));

  PercentProgress := Ceil(100 * ALoaded / ATotal);

  // TODO, how to set aria-valuenow?
  // LoadingProgressContainer := Document.getElementById('castle-loading-progress-container') as TJSHTMLElement;
  // if LoadingProgressContainer <> nil then
  //   LoadingProgressContainer.AriaValueNow := PercentProgress;

  LoadingProgressBar := Document.getElementById('castle-loading-progress-bar') as TJSHTMLElement;
  if LoadingProgressBar <> nil then
  begin
    LoadingProgressBar.Style['width'] := IntToStr(PercentProgress) + '%';
  end;

  LoadingInfo := Document.getElementById('castle-loading-info') as TJSHTMLElement;
  if LoadingInfo <> nil then
  begin
    LoadingInfo.InnerHTML := Format('Loading application... (downloading data: %d%%, %d / %d bytes)', [
      PercentProgress,
      ALoaded,
      ATotal
    ]);
  end;
end;

procedure TMyApplication.ShowFatalError(const Message: String);
var
  LoadingInfo: TJSHTMLElement;
begin
  WriteLn('Fatal error: ' + Message);

  LoadingInfo := Document.getElementById('castle-loading-info') as TJSHTMLElement;
  if LoadingInfo = nil then
    Exit;
  LoadingInfo.InnerHTML := 'Fatal error: ' + HtmlNL + Message;
  { Nice error message style using Bootstrap,
    see https://getbootstrap.com/docs/5.3/customize/color/#colors }
  LoadingInfo.ClassName := LoadingInfo.ClassName +
    ' p-3 text-bg-danger rounded-3';
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
