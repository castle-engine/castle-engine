{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Alien Outpost".

  "Alien Outpost" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Alien Outpost" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Library to run the game as NPAPI plugin. }
library alienoutpost;

{$I castle_npapi.inc}

{ Do not use windowless for now, I can't seem to be able to get good X11 window XID. }
{ $define PLUGIN_WINDOWLESS}

uses CMem, //< TODO, cmem is a desperate try. TODO: is this needed? not sure.
  {$ifdef DEBUG_PLUGIN_THREAD_ID}
    {$ifdef LINUX} Libc, {$endif}
    {$ifdef MSWINDOWS} Windows, {$endif}
  {$endif}
  SysUtils, Classes, CTypes, {$ifdef MOZ_X11} Xlib, X, {$endif}
  CastleNPAPI, CastleLog, CastleWarnings, CastleUtils, CastleWindow,
  CastleFilesUtils, CastleStringUtils,
  Game;

var
  sBrowserFuncs: TNPNetscapeFuncs;

type
  TInstanceData = record
    Npp: PNPP;
    Window: TNPWindow;
    CastleWindow: TCastleWindowCustom;
    FreeCastleWindow: boolean;
    NamedParameters: TCastleStringList;
    {$ifndef PLUGIN_WINDOWLESS}
    HasEventCheckTimer: boolean;
    EventCheckTimerId: CUInt32;
    {$endif}
  end;
  PInstanceData = ^TInstanceData;

var
  { String values returned by NP_ functions.
    For safety (in case C code copies only pointers)
    they are read-only after initialization. }
  PluginName, PluginDescription, PluginMimeDescription, PluginVersion: string;
  StaticResultsInitialized: boolean = false;

procedure InitializeStaticResults;
begin
  if not StaticResultsInitialized then
  begin
    StaticResultsInitialized := true;

    PluginName := ApplicationName;
    PluginDescription := 'Plugin created with Castle Game Engine to run ' + ApplicationName;
    { Note: castle-engine build tool places the same thing inside Windows resource file
      for compiled plugin, because on Windows this info is taken from registry + resources. }
    PluginMimeDescription := 'application/x-' + ApplicationName + ':cge:Game Data for ' + ApplicationName + ' (Castle Game Engine)';
    { TODO: from CastleEngineManifest.xml }
    PluginVersion := '1.0.0.0';

    WritelnLogMultiline('Plugin', 'Initialized NPAPI plugin:' + NL +
      'Name: ' + PluginName + NL +
      'Description: ' + PluginDescription + NL +
      'MIME description: ' + PluginMimeDescription + NL +
      'Version: ' + PluginVersion);
  end;
end;

function NP_GetPluginVersion: PChar; extdecl_osdecl;
begin
  try
    InitializeStaticResults;
    Result := PChar(PluginVersion);
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NP_GetPluginVersion: ' + ExceptMessage(E));
      Result := nil;
    end;
  end;
end;

function NP_GetMIMEDescription: PChar; extdecl_osdecl;
begin
  try
    InitializeStaticResults;
    Result := PChar(PluginMimeDescription);
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NP_GetMIMEDescription: ' + ExceptMessage(E));
      Result := nil;
    end;
  end;
end;

function NP_GetValue(Future: Pointer; aVariable: TNPPVariable; aValue: Pointer): TNPError; extdecl_osdecl;
begin
  try
    InitializeStaticResults;
    case aVariable of
      NPPVpluginNameString:
        PPChar(aValue)^ := PChar(PluginName);
      NPPVpluginDescriptionString:
        PPChar(aValue)^ := PChar(PluginDescription);
      else
        Exit(NPERR_INVALID_PARAM);
    end;
    Result := NPERR_NO_ERROR;
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NP_GetValue: ' + ExceptMessage(E));
      Result := NPERR_GENERIC_ERROR;
    end;
  end;
end;

function NP_Shutdown: TNPError; extdecl_osdecl;
begin
  try
    WritelnLog('Plugin', Format('NP_Shutdown', []));
    Result := NPERR_NO_ERROR;
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NP_Shutdown: ' + ExceptMessage(E));
      Result := NPERR_GENERIC_ERROR;
    end;
  end;
end;

{$ifdef MOZ_X11}
function CheckXEmbedSupport(instance: PNPP): boolean;
var
  Err: TNPError;
  SupportsXEmbed: TNPBool;
  //Toolkit: TNPNToolkitType;
begin
  Err := sBrowserFuncs.getvalue(instance, NPNVSupportsXEmbedBool, @supportsXEmbed);
  if (err <> NPERR_NO_ERROR) or (supportsXEmbed = 0) then
  begin
    OnWarning(wtMajor, 'Plugin', 'Browser does not support XEmbed');
    Exit(false);
  end;
  WritelnLog('Plugin', 'XEmbed supported');

  // Asking for toolkit ends with error when using XEmbed?
  // Err := sBrowserFuncs.getvalue(instance, NPNVToolkit, @toolkit);
  // if (err <> NPERR_NO_ERROR) or (toolkit <> NPNVGtk2) then
  // begin
  //   OnWarning(wtMajor, 'Plugin', Format('Browser toolkit is not GTK2 (%d) or error when getting toolkit (%d)',
  //     [Toolkit, Err]));
  //   Exit(false);
  // end;

  Result := true;
end;
{$endif}

function InstanceDataCheck(instance: PNPP): PInstanceData;
begin
  if Instance  = nil then
  begin
    OnWarning(wtMajor, 'Plugin', 'NPAPI plugin instance not assigned');
    Exit(nil);
  end;

  Result := PInstanceData(instance^.pdata);
  if Result  = nil then
    OnWarning(wtMajor, 'Plugin', 'NPAPI plugin instance data not assigned');
end;

{$ifndef PLUGIN_WINDOWLESS}
var
  InsideTimer: boolean;

procedure EventCheckTimer(instance: PNPP; timerID: CUInt32); extdecl;
const
  { This is brutal, but it seems the only way to get smooth mouse movement
    after mouse click on Firefox. Otherwise (with = 1) we get hangs,
    like the timer was not updated as often as it should.
    Works fine with = 1 on (old) Google Chrome.
    TODO: see what FireBreath is doing here? }
  ProcessCount = 4;
var
  InstanceData: PInstanceData;
  I, J: Integer;
begin
  // We could try calling the processing here like this:
  //sBrowserFuncs.pluginthreadasynccall(instance, @EventCheckTimerCallback, instance);
  // but it's not necessary anywhere.

  {$ifdef DEBUG_PLUGIN_THREAD_ID} // weird FPC 2.6.4 bug prevents uncommenting this
  {$ifdef LINUX} Writeln('Thread id in EventCheckTimerCallback: ', pthread_self); {$endif}
  {$ifdef MSWINDOWS} WritelnLog('Plugin', Format('Thread id in EventCheckTimerCallback: %d', [PtrUint(GetCurrentThreadId)])); {$endif}
  {$endif}

  InsideTimer := true;
  try
    try
      InstanceData := InstanceDataCheck(Instance);
      if InstanceData = nil then Exit; // warning already done by InstanceDataCheck

      // TODO: avoid multiple timers for multiple window, we want only one.

      for J := 1 to ProcessCount do
      begin
        // TODO: all too slow
        //Application.ProcessMessage(false, true);
        //Application.ProcessMessage(false, false);
        Application.ProcessAllMessages;

        // TODO: this should not be necessary
        // TODO: even if, only for InstanceData^.CastleWindow should be enough!
        // if InstanceData^.CastleWindow <> nil then
        //   InstanceData^.CastleWindow.Invalidate
        for I := 0 to Application.OpenWindowsCount - 1 do
          Application.OpenWindows[I].Invalidate;
      end;
    except
      on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in EventCheckTimer: ' + ExceptMessage(E));
    end;
  finally InsideTimer := false end;
end;
{$endif}

function NPP_New(pluginType: TNPMIMEType; instance: PNPP; mode: CUInt16; argc: CInt16; argn: PPChar; argv: PPChar; saved: PNPSavedData): TNPError; extdecl;
var
  {$ifdef PLUGIN_WINDOWLESS}
  browserSupportsWindowless: TNPBool;
  {$endif}
  InstanceData: PInstanceData;
  I: Integer;
begin
  try
    WritelnLog('Plugin', Format('NPP_New', []));

    {$ifdef PLUGIN_WINDOWLESS}
    // Make sure we can render this plugin
    browserSupportsWindowless := 0;
    sBrowserFuncs.getvalue(instance, NPNVSupportsWindowless, @browserSupportsWindowless);
    if browserSupportsWindowless = 0 then
    begin
      OnWarning(wtMajor, 'Plugin', 'Windowless mode not supported by the browser');
      Exit(NPERR_GENERIC_ERROR);
    end;

    sBrowserFuncs.setvalue(instance, NPPVpluginWindowBool, nil);
    {$endif}

    // set up our our instance data
    InstanceData := GetMem(SizeOf(TInstanceData));
    FillChar(InstanceData^, SizeOf(TInstanceData), 0);
    InstanceData^.npp := instance;
    instance^.pdata := InstanceData;

    {$ifdef MOZ_X11}
    if not CheckXEmbedSupport(instance) then
      Exit(NPERR_GENERIC_ERROR);
    {$endif}

    InstanceData^.NamedParameters := TCastleStringList.Create;
    for I := 0 to ArgC - 1 do
    begin
      if argn[I] = 'cge_data' then
      begin
        ApplicationDataOverride := argv[I];
        WritelnLog('Plugin', Format('Found plugin parameter cge_data, using as application data path: %s', [ApplicationDataOverride]));
      end;
      InstanceData^.NamedParameters.Values[argn[I]] := argv[I];
      WritelnLog('Plugin', Format('Params %s=%s', [argn[I], argv[I]]));
    end;

    Application.UserAgent := sBrowserFuncs.uagent(instance);

    {$ifndef PLUGIN_WINDOWLESS}
    InstanceData^.EventCheckTimerId := sBrowserFuncs.scheduletimer(instance, 0, 1, @EventCheckTimer);
    InstanceData^.HasEventCheckTimer := true;
    {$endif}

    {$ifdef DEBUG_PLUGIN_THREAD_ID}
    {$ifdef LINUX} Writeln('Thread id of current thread: ', pthread_self); {$endif}
    {$ifdef MSWINDOWS} WritelnLog('Plugin', Format('Thread id of current thread in NPP_New: %d', [PtrUint(GetCurrentThreadId)])); {$endif}
    {$endif}

    WritelnLog('Plugin', Format('Finish NPP_New, user agent is %s', [Application.UserAgent]));
    Result := NPERR_NO_ERROR;
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NPP_New: ' + ExceptMessage(E));
      Result := NPERR_GENERIC_ERROR;
    end;
  end;
end;

function NPP_Destroy(instance: PNPP; save: PPNPSavedData): TNPError; extdecl;
var
  InstanceData: PInstanceData;
begin
  try
    WritelnLog('Plugin', Format('NPP_Destroy', []));

    InstanceData := InstanceDataCheck(instance);
    if InstanceData = nil then Exit(NPERR_GENERIC_ERROR); // warning already done by InstanceDataCheck

    {$ifndef PLUGIN_WINDOWLESS}
    if InstanceData^.HasEventCheckTimer then
    begin
      sBrowserFuncs.unscheduletimer(instance, InstanceData^.EventCheckTimerId);
      InstanceData^.HasEventCheckTimer := false;
    end;

    { wait for InsideTimer = false }
    while InsideTimer do Sleep(10);
    {$endif}

    Instance^.pdata := nil;

    InstanceData^.CastleWindow.Close(false);
    if InstanceData^.FreeCastleWindow then
      FreeAndNil(InstanceData^.CastleWindow) else
      InstanceData^.CastleWindow := nil;

    FreeAndNil(InstanceData^.NamedParameters);

    FreeMem(InstanceData);
    Result := NPERR_NO_ERROR;
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NPP_Destroy: ' + ExceptMessage(E));
      Result := NPERR_GENERIC_ERROR;
    end;
  end;
end;

function NPP_SetWindow(instance: PNPP; window: PNPWindow): TNPError; extdecl;
var
  InstanceData: PInstanceData;
  WindowInitialize: boolean;
begin
  try
    WritelnLog('Plugin', Format('NPP_SetWindow', []));

    InstanceData := InstanceDataCheck(instance);
    if InstanceData = nil then Exit(NPERR_GENERIC_ERROR); // warning already done by InstanceDataCheck

    {$ifdef MOZ_X11}
    if (window = nil) or (window^.ws_info = nil) then
    begin
      OnWarning(wtMajor, 'Plugin', 'window or window^.ws_info is NULL, ignoring NPP_SetWindow');
      Exit(NPERR_GENERIC_ERROR);
    end;
    {$endif}

    if Window^.aType <> {$ifdef PLUGIN_WINDOWLESS} NPWindowTypeDrawable {$else} NPWindowTypeWindow {$endif} then
    begin
      OnWarning(wtMajor, 'Plugin', Format('Window type is %d, unsupported', [Window^.aType]));
      Exit(NPERR_GENERIC_ERROR);
    end;

    { the 2nd comparison is necessary to detect buggy Google Chrome on Windows
      that passes window=nil later }
    WindowInitialize := (InstanceData^.window.window <> window^.window) and (window^.window <> nil);
    InstanceData^.window := window^;

    if WindowInitialize then
    begin
      if (Application.MainWindow <> nil) and
         Application.MainWindow.Closed then
      begin
        InstanceData^.CastleWindow := Application.MainWindow;
        InstanceData^.FreeCastleWindow := false;
      end else
      begin
        InstanceData^.CastleWindow := Application.DefaultWindowClass.Create(Application);
        InstanceData^.FreeCastleWindow := true;
      end;
      InstanceData^.CastleWindow.NamedParameters.Assign(InstanceData^.NamedParameters);
      FreeAndNil(InstanceData^.NamedParameters); // will not be useful anymore
    end;

    if InstanceData^.CastleWindow <> nil then
    begin
      { copy data to InstanceData^.CastleWindow, such that we don't keep
        pointers from NPP_SetWindow callers, instead we have our own data copy. }
      InstanceData^.CastleWindow.PluginWindow := Window^;
      {$ifdef MOZ_X11}
      InstanceData^.CastleWindow.PluginStruct := PNPSetWindowCallbackStruct(Window^.Ws_Info)^;
      {$endif}
      if WindowInitialize then
      begin
        { open new window }
        {$ifdef MOZ_X11}
        Application.FXDisplay := PNPSetWindowCallbackStruct(Window^.Ws_Info)^.Display; // set this early
        {$endif}
        { For windowless plugins, "Unix/X11: The drawable is provided in the paint message.",
          so defer Open call to later. }
        {$ifndef PLUGIN_WINDOWLESS}
        InstanceData^.CastleWindow.Open;
        {$else}
          {$ifndef MOZ_X11}
          InstanceData^.CastleWindow.Open;
          {$endif}
        {$endif}
      end else
      begin
        InstanceData^.CastleWindow.Left := Window^.X;
        InstanceData^.CastleWindow.Top := Window^.Y;
        if (Window^.Width  <> InstanceData^.CastleWindow.Width) or
           (Window^.Height <> InstanceData^.CastleWindow.Height) then
          InstanceData^.CastleWindow.DoResize(Window^.Width, Window^.Height, false);
      end;
    end;
    Result := NPERR_NO_ERROR;
  except
    on E: TObject do
    begin
      OnWarning(wtMajor, 'Plugin', 'Exception in NPP_SetWindow: ' + ExceptMessage(E));
      Result := NPERR_GENERIC_ERROR;
    end;
  end;
end;

function NPP_NewStream(instance: PNPP; aType: TNPMIMEType; stream: PNPStream; seekable: TNPBool; stype: PCUInt16): TNPError; extdecl;
begin
  try
    WritelnLog('Plugin', Format('NPP_NewStream', []));
    Result := NPERR_GENERIC_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_NewStream: ' + ExceptMessage(E));
  end;
end;

function NPP_DestroyStream(instance: PNPP; stream: PNPStream; reason: TNPReason): TNPError; extdecl;
begin
  try
    WritelnLog('Plugin', Format('NPP_DestroyStream', []));
    Result := NPERR_GENERIC_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_DestroyStream: ' + ExceptMessage(E));
  end;
end;

function NPP_WriteReady(instance: PNPP; stream: PNPStream): CInt32; extdecl;
begin
  try
    Result := 0;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_WriteReady: ' + ExceptMessage(E));
  end;
end;

function NPP_Write(instance: PNPP; stream: PNPStream; offset: CInt32; len: CInt32; buffer: Pointer): CInt32; extdecl;
begin
  try
    Result := 0;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_Write: ' + ExceptMessage(E));
  end;
end;

procedure NPP_StreamAsFile(instance: PNPP; stream: PNPStream; fname: PChar); extdecl;
begin
  try
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_StreamAsFile: ' + ExceptMessage(E));
  end;
end;

procedure NPP_Print(instance: PNPP; platformPrint: PNPPrint); extdecl;
begin
  try
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_Print: ' + ExceptMessage(E));
  end;
end;

function NPP_HandleEvent(instance: PNPP; event: Pointer): cint16; extdecl;
{$ifdef MOZ_X11}
var
  nativeEvent: PXEvent;
  {$ifdef PLUGIN_WINDOWLESS}
  InstanceData: PInstanceData;
  {$endif}
begin
  try
    {$ifdef PLUGIN_WINDOWLESS}
    InstanceData := InstanceDataCheck((instance);
    if InstanceData = nil then Exit(0); // warning already done by InstanceDataCheck
    {$endif}
    nativeEvent := PXEvent(event);
    WritelnLog('Plugin', Format('Got event %d', [nativeEvent^._type]));

    {$ifdef PLUGIN_WINDOWLESS}
    { For windowless plugins, "Unix/X11: The drawable is provided in the paint message.",
      so defer Open call to later. }
    if (nativeEvent^._type = GraphicsExpose) and InstanceData^.CastleWindow.Closed then
    begin
      WritelnLog('Plugin', Format('Opening windowless Unix plugin on 1st expose event, drawable is %d', [nativeEvent^.xgraphicsexpose.drawable]));
      InstanceData^.window.window := Pointer(nativeEvent^.xany.window);
      InstanceData^.CastleWindow.PluginWindow.window := Pointer(nativeEvent^.xany.window);
      InstanceData^.CastleWindow.Open;
    end;
    {$endif}

    if Application.HandleXEvent(nativeEvent^) then
      Result := 1 else
      Result := 0;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_HandleEvent: ' + ExceptMessage(E));
  end;
{$else}
begin
  try
    WritelnLog('Plugin', Format('Event', []));
    // TODO: not even trying to make windowless plugin work for non-Unix now
    Result := 0;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_HandleEvent: ' + ExceptMessage(E));
  end;
{$endif}
end;

procedure NPP_URLNotify(instance: PNPP; URL: PChar; reason: TNPReason; notifyData: Pointer); extdecl;
begin
  try
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_URLNotify: ' + ExceptMessage(E));
  end;
end;

function NPP_GetValue(instance: PNPP; variable: TNPPVariable; value: Pointer): TNPError; extdecl;
begin
  try
    WritelnLog('Plugin', Format('NPP_GetValue %d', [Ord(variable)]));

    // https://developer.mozilla.org/en-US/Add-ons/Plugins/XEmbed_Extension_for_Mozilla_Plugins
    // For windowless plugins it seems ignored, for windowed plugins it's required by Chrome
    // (seems ignored by Firefox?)

    {$ifdef MOZ_X11}
    if variable = NPPVpluginNeedsXEmbed then
    begin
      PNPBool(value)^ := 1;
      Exit(NPERR_NO_ERROR);
    end;
    {$endif}

    Result := NPERR_GENERIC_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_GetValue: ' + ExceptMessage(E));
  end;
end;

function NPP_SetValue(instance: PNPP; variable: TNPNVariable; value: Pointer): TNPError; extdecl;
begin
  try
    WritelnLog('Plugin', Format('NPP_SetValue %d', [Ord(variable)]));
    Result := NPERR_GENERIC_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NPP_SetValue: ' + ExceptMessage(E));
  end;
end;

{ Called only on Windows, it seems. We call it manually on Unix. }
function NP_GetEntryPoints(pFuncs: PNPPluginFuncs): TNPError; extdecl_osdecl;
var
  MinSize: Integer;
begin
  try
    WritelnLog('Plugin', 'NP_GetEntryPoints called');

    // note: do NOT clear the pFuncs^, like
    //FillChar(pFuncs^, SizeOf(TNPPluginFuncs), 0);
    // as this would clear also pFuncs^.size.

    // Check the size of the provided structure based on the offset of the
    // last member we need.
    MinSize := PtrUInt(@(pFuncs^.setvalue)) - PtrUInt(pFuncs) + SizeOf(Pointer);
    WritelnLog('Plugin', Format('Min size of NPPluginFuncs struct %d', [MinSize]));
    if pFuncs^.size < MinSize then
    begin
      OnWarning(wtMajor, 'Plugin', Format('Too old implementation of NPAPI in your browser (because NPPluginFuncs struct is too small: %d). Upgrade your browser.', [pFuncs^.size]));
      Exit(NPERR_INVALID_FUNCTABLE_ERROR);
    end;

    pFuncs^.newp := @NPP_New;
    pFuncs^.destroy := @NPP_Destroy;
    pFuncs^.setwindow := @NPP_SetWindow;
    pFuncs^.newstream := @NPP_NewStream;
    pFuncs^.destroystream := @NPP_DestroyStream;
    pFuncs^.asfile := @NPP_StreamAsFile;
    pFuncs^.writeready := @NPP_WriteReady;
    pFuncs^.aWrite := @NPP_Write;
    pFuncs^.print := @NPP_Print;
    pFuncs^.event := @NPP_HandleEvent;
    pFuncs^.urlnotify := @NPP_URLNotify;
    pFuncs^.getvalue := @NPP_GetValue;
    pFuncs^.setvalue := @NPP_SetValue;

    Result := NPERR_NO_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NP_GetEntryPoints: ' + ExceptMessage(E));
  end;
end;

type
   tc = class(tthread)
     procedure execute;override;
   end;

   procedure tc.execute;
   begin
   end;

{ Prepare in case current process will use various threads when calling our code.
  See http://wiki.freepascal.org/Multithreaded_Application_Tutorial#External_threads .
  Not necessary in practice --- tested on various browsers and OSes, we're always in a single thread. }
procedure PrepareForExternalThreads;
begin
  { initialise threading system }
  with tc.create(false) do
  begin
    waitfor;
    free;
  end;
  if IsMultiThread then
    WritelnLog('Plugin', 'OK: prepared for multi-threading') else
    OnWarning(wtMajor, 'Plugin', 'NOT prepared for multi-threading');
end;

function NP_Initialize(bFuncs: PNPNetscapeFuncs {$ifdef UNIX}; pFuncs: PNPPluginFuncs{$endif}): TNPError; extdecl_osdecl;
var
  MinSize: Integer;
begin
  try
    PrepareForExternalThreads;

    WritelnLog('Plugin', 'NP_Initialize.'
      {$ifdef WIN32} + ' (win32 plugin)' {$endif}
      {$ifdef WIN64} + ' (win64 plugin)' {$endif});

    // Check the size of the provided structure based on the offset of the
    // last member we need.
    MinSize := PtrUInt(@(bFuncs^.unscheduletimer)) - PtrUInt(bFuncs) + SizeOf(Pointer);
    WritelnLog('Plugin', Format('Min size of NPNetscapeFuncs struct %d', [MinSize]));
    if bFuncs^.size < MinSize then
    begin
      OnWarning(wtMajor, 'Plugin', Format('Too old implementation of NPAPI in your browser (because PNPNetscapeFuncs struct is too small: %d). Upgrade your browser.', [bFuncs^.size]));
      Exit(NPERR_INVALID_FUNCTABLE_ERROR);
    end;

    FillChar(sBrowserFuncs, SizeOf(sBrowserFuncs), 0);
    Move(bFuncs^, sBrowserFuncs, MinSize); // copy only MinSize, not more --- this is safe

    {$ifdef UNIX}
    { Trick to reuse NP_GetEntryPoints implementation for Unix. }
    NP_GetEntryPoints(pFuncs);
    {$endif}

    Result := NPERR_NO_ERROR;
  except
    on E: TObject do OnWarning(wtMajor, 'Plugin', 'Exception in NP_Initialize: ' + ExceptMessage(E));
  end;

  WritelnLog('Plugin', 'NP_Initialize finished');
end;

{$ifdef MSWINDOWS}
  {$R plugin-automatic-windows-resources.res}
{$endif MSWINDOWS}

exports
  NP_GetPluginVersion,
  NP_GetMIMEDescription,
  NP_GetValue,
  NP_Initialize,
  NP_Shutdown,
  NP_GetEntryPoints,

  { TODO: probably exporting these is not necessary, NP_Initialize passes them? }
  NPP_New,
  NPP_Destroy,
  NPP_SetWindow,
  NPP_NewStream,
  NPP_DestroyStream,
  NPP_WriteReady,
  NPP_Write,
  NPP_HandleEvent,
  NPP_GetValue,
  NPP_SetValue,
  NPP_StreamAsFile,
  NPP_Print,
  NPP_URLNotify;

end.
