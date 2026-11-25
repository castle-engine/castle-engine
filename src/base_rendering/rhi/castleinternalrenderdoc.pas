{
  Copyright 2026 Michalis Kamburelis, Antigravity.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ In-application RenderDoc API integration for Castle Game Engine.
  Allows programmatic frame capturing and launching the RenderDoc Replay UI
  directly from CGE applications, editor, or test suites. }
unit CastleInternalRenderDoc;

{$I castleconf.inc}

interface

uses
  SysUtils, CastleDynLib, CastleLog;

const
  RENDERDOC_API_Version_1_0_0 = 10000;
  RENDERDOC_API_Version_1_1_2 = 10102;
  RENDERDOC_API_Version_1_4_0 = 10400;

type
  { RenderDoc C API Function Prototypes }
  pRENDERDOC_GetAPIVersion = procedure(out Major, Minor, Patch: Integer); cdecl;
  pRENDERDOC_SetCaptureOptionU32 = function(Opt: Cardinal; Val: Cardinal): Integer; cdecl;
  pRENDERDOC_SetCaptureOptionF32 = function(Opt: Cardinal; Val: Single): Integer; cdecl;
  pRENDERDOC_GetCaptureOptionU32 = function(Opt: Cardinal): Cardinal; cdecl;
  pRENDERDOC_GetCaptureOptionF32 = function(Opt: Cardinal): Single; cdecl;
  pRENDERDOC_SetFocusToggleKeys = procedure(Keys: Pointer; Num: Integer); cdecl;
  pRENDERDOC_SetCaptureKeys = procedure(Keys: Pointer; Num: Integer); cdecl;
  pRENDERDOC_GetOverlayBits = function: Cardinal; cdecl;
  pRENDERDOC_MaskOverlayBits = procedure(AndFlags, OrFlags: Cardinal); cdecl;
  pRENDERDOC_Shutdown = procedure; cdecl;
  pRENDERDOC_UnloadCrashHandler = procedure; cdecl;
  pRENDERDOC_SetCaptureFilePathTemplate = procedure(PathTemplate: PAnsiChar); cdecl;
  pRENDERDOC_GetCaptureFilePathTemplate = function: PAnsiChar; cdecl;
  pRENDERDOC_GetNumCaptures = function: Cardinal; cdecl;
  pRENDERDOC_GetCapture = function(Index: Cardinal; FileName: PAnsiChar; PathLength: PCardinal; Timestamp: PUInt64): Cardinal; cdecl;
  pRENDERDOC_TriggerCapture = procedure; cdecl;
  pRENDERDOC_IsRemoteAccessConnected = function: Cardinal; cdecl;
  pRENDERDOC_LaunchReplayUI = function(ConnectTargetControl: Cardinal; CmdLine: PAnsiChar): Cardinal; cdecl;
  pRENDERDOC_SetActiveWindow = procedure(Device: Pointer; WndHandle: Pointer); cdecl;
  pRENDERDOC_StartFrameCapture = procedure(Device: Pointer; WndHandle: Pointer); cdecl;
  pRENDERDOC_IsFrameCapturing = function: Cardinal; cdecl;
  pRENDERDOC_EndFrameCapture = function(Device: Pointer; WndHandle: Pointer): Cardinal; cdecl;
  pRENDERDOC_TriggerMultiFrameCapture = procedure(NumFrames: Cardinal); cdecl;

  PRENDERDOC_API_1_1_2 = ^RENDERDOC_API_1_1_2;
  RENDERDOC_API_1_1_2 = record
    GetAPIVersion: pRENDERDOC_GetAPIVersion;
    SetCaptureOptionU32: pRENDERDOC_SetCaptureOptionU32;
    SetCaptureOptionF32: pRENDERDOC_SetCaptureOptionF32;
    GetCaptureOptionU32: pRENDERDOC_GetCaptureOptionU32;
    GetCaptureOptionF32: pRENDERDOC_GetCaptureOptionF32;
    SetFocusToggleKeys: pRENDERDOC_SetFocusToggleKeys;
    SetCaptureKeys: pRENDERDOC_SetCaptureKeys;
    GetOverlayBits: pRENDERDOC_GetOverlayBits;
    MaskOverlayBits: pRENDERDOC_MaskOverlayBits;
    Shutdown: pRENDERDOC_Shutdown;
    UnloadCrashHandler: pRENDERDOC_UnloadCrashHandler;
    SetCaptureFilePathTemplate: pRENDERDOC_SetCaptureFilePathTemplate;
    GetCaptureFilePathTemplate: pRENDERDOC_GetCaptureFilePathTemplate;
    GetNumCaptures: pRENDERDOC_GetNumCaptures;
    GetCapture: pRENDERDOC_GetCapture;
    TriggerCapture: pRENDERDOC_TriggerCapture;
    IsRemoteAccessConnected: pRENDERDOC_IsRemoteAccessConnected;
    LaunchReplayUI: pRENDERDOC_LaunchReplayUI;
    SetActiveWindow: pRENDERDOC_SetActiveWindow;
    StartFrameCapture: pRENDERDOC_StartFrameCapture;
    IsFrameCapturing: pRENDERDOC_IsFrameCapturing;
    EndFrameCapture: pRENDERDOC_EndFrameCapture;
    TriggerMultiFrameCapture: pRENDERDOC_TriggerMultiFrameCapture;
  end;

  pRENDERDOC_GetAPI = function(Version: Integer; OutAPIPointers: Pointer): Integer; cdecl;

  { RenderDoc in-application controller }
  TRenderDocAPI = class
  private
    FLibrary: TDynLib;
    FAPI: PRENDERDOC_API_1_1_2;
    FInitialized: Boolean;
    function EnsureLoaded: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns True if RenderDoc dynamic library is loaded and API initialized. }
    function Available: Boolean;

    { Triggers a capture of the next frame. }
    procedure TriggerCapture;

    { Explicit frame capture delimitation. }
    procedure StartFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil);
    function EndFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil): Boolean;
    function IsFrameCapturing: Boolean;

    { Sets the file path template for captures (e.g. 'captures/cge_frame'). }
    procedure SetCaptureFilePathTemplate(const PathTemplate: String);
    function GetCaptureFilePathTemplate: String;

    { Returns total number of captured frames recorded in this session. }
    function GetNumCaptures: Cardinal;

    { Launches the RenderDoc Replay UI and connects to the running application. }
    function LaunchReplayUI(const ConnectTargetControl: Boolean = True; const CommandLine: String = ''): Boolean;
  end;

function RenderDoc: TRenderDocAPI;

{ Procedural helpers }
function RenderDocAvailable: Boolean;
procedure RenderDocTriggerCapture;
procedure RenderDocStartFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil);
function RenderDocEndFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil): Boolean;
function RenderDocIsFrameCapturing: Boolean;
function RenderDocLaunchReplayUI(const ConnectTargetControl: Boolean = True; const CommandLine: String = ''): Boolean;

implementation

var
  FRenderDocInstance: TRenderDocAPI = nil;

function RenderDoc: TRenderDocAPI;
begin
  if FRenderDocInstance = nil then
    FRenderDocInstance := TRenderDocAPI.Create;
  Result := FRenderDocInstance;
end;

function RenderDocAvailable: Boolean;
begin
  Result := RenderDoc.Available;
end;

procedure RenderDocTriggerCapture;
begin
  RenderDoc.TriggerCapture;
end;

procedure RenderDocStartFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil);
begin
  RenderDoc.StartFrameCapture(Device, WindowHandle);
end;

function RenderDocEndFrameCapture(const Device: Pointer = nil; const WindowHandle: Pointer = nil): Boolean;
begin
  Result := RenderDoc.EndFrameCapture(Device, WindowHandle);
end;

function RenderDocIsFrameCapturing: Boolean;
begin
  Result := RenderDoc.IsFrameCapturing;
end;

function RenderDocLaunchReplayUI(const ConnectTargetControl: Boolean = True; const CommandLine: String = ''): Boolean;
begin
  Result := RenderDoc.LaunchReplayUI(ConnectTargetControl, CommandLine);
end;

{ TRenderDocAPI }

constructor TRenderDocAPI.Create;
begin
  inherited Create;
  FLibrary := nil;
  FAPI := nil;
  FInitialized := False;
end;

destructor TRenderDocAPI.Destroy;
begin
  FreeAndNil(FLibrary);
  FAPI := nil;
  inherited Destroy;
end;

function TRenderDocAPI.EnsureLoaded: Boolean;
const
  {$if defined(MSWINDOWS)}
  RenderDocLibNames: array[0..1] of String = ('renderdoc.dll', 'librenderdoc.dll');
  {$elseif defined(DARWIN)}
  RenderDocLibNames: array[0..0] of String = ('librenderdoc.dylib');
  {$else}
  RenderDocLibNames: array[0..1] of String = ('librenderdoc.so', 'librenderdoc.so.1');
  {$endif}
var
  I: Integer;
  GetAPI: pRENDERDOC_GetAPI;
  EnvPath: String;
begin
  if FInitialized then
    Exit(FAPI <> nil);
  FInitialized := True;

  EnvPath := GetEnvironmentVariable('RENDERDOC_PATH');
  if EnvPath <> '' then
    FLibrary := TDynLib.Load(EnvPath, False);

  for I := Low(RenderDocLibNames) to High(RenderDocLibNames) do
  begin
    if FLibrary <> nil then Break;
    FLibrary := TDynLib.Load(RenderDocLibNames[I], False);
  end;

  if FLibrary = nil then Exit(False);

  Pointer(GetAPI) := FLibrary.Symbol('RENDERDOC_GetAPI');
  if not Assigned(GetAPI) then
  begin
    FreeAndNil(FLibrary);
    Exit(False);
  end;

  if (GetAPI(RENDERDOC_API_Version_1_1_2, @FAPI) <> 1) and
     (GetAPI(RENDERDOC_API_Version_1_0_0, @FAPI) <> 1) then
  begin
    FreeAndNil(FLibrary);
    FAPI := nil;
    Exit(False);
  end;

  WritelnLog('RenderDoc', 'RenderDoc In-Application API successfully initialized');
  Result := FAPI <> nil;
end;

function TRenderDocAPI.Available: Boolean;
begin
  Result := EnsureLoaded;
end;

procedure TRenderDocAPI.TriggerCapture;
begin
  if Available and Assigned(FAPI^.TriggerCapture) then
    FAPI^.TriggerCapture();
end;

procedure TRenderDocAPI.StartFrameCapture(const Device: Pointer; const WindowHandle: Pointer);
begin
  if Available and Assigned(FAPI^.StartFrameCapture) then
    FAPI^.StartFrameCapture(Device, WindowHandle);
end;

function TRenderDocAPI.EndFrameCapture(const Device: Pointer; const WindowHandle: Pointer): Boolean;
begin
  Result := Available and Assigned(FAPI^.EndFrameCapture) and
    (FAPI^.EndFrameCapture(Device, WindowHandle) = 1);
end;

function TRenderDocAPI.IsFrameCapturing: Boolean;
begin
  Result := Available and Assigned(FAPI^.IsFrameCapturing) and
    (FAPI^.IsFrameCapturing() = 1);
end;

procedure TRenderDocAPI.SetCaptureFilePathTemplate(const PathTemplate: String);
var
  A: AnsiString;
begin
  if Available and Assigned(FAPI^.SetCaptureFilePathTemplate) then
  begin
    A := AnsiString(PathTemplate);
    FAPI^.SetCaptureFilePathTemplate(PAnsiChar(A));
  end;
end;

function TRenderDocAPI.GetCaptureFilePathTemplate: String;
var
  P: PAnsiChar;
begin
  Result := '';
  if Available and Assigned(FAPI^.GetCaptureFilePathTemplate) then
  begin
    P := FAPI^.GetCaptureFilePathTemplate();
    if P <> nil then Result := String(P);
  end;
end;

function TRenderDocAPI.GetNumCaptures: Cardinal;
begin
  if Available and Assigned(FAPI^.GetNumCaptures) then
    Result := FAPI^.GetNumCaptures()
  else
    Result := 0;
end;

function TRenderDocAPI.LaunchReplayUI(const ConnectTargetControl: Boolean; const CommandLine: String): Boolean;
var
  CmdAnsi: AnsiString;
begin
  CmdAnsi := AnsiString(CommandLine);
  Result := Available and Assigned(FAPI^.LaunchReplayUI) and
    (FAPI^.LaunchReplayUI(Ord(ConnectTargetControl), PAnsiChar(CmdAnsi)) = 1);
end;

finalization
  FreeAndNil(FRenderDocInstance);
end.
