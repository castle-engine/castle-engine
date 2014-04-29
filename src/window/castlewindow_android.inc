{
  Copyright 2004-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage OpenGL ES context on Android using EGL,
  and manage message loop using Android NDK. }

{$I castlewindow_egl.inc}
{$I castlewindow_dialogs_by_messages.inc}

{$ifdef read_interface_uses}
CastleAndroidNativeWindow, CastleAndroidInput,
CastleAndroidNativeAppGlue, CastleGenericLists,
{$endif}

{$ifdef read_implementation_uses}
CastleAndroidLooper,
CastleAndroidLog, CastleAndroidAssetStream, CastleConfig,
CastleOpenAL, CastleVorbisFile, CastleFreeTypeH, CastleZLib,
{$endif}

{$ifdef read_window_interface}
private
  NativeWindow: EGLNativeWindowType; //< must be set before Window.Open
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  type
    { On Android, we have to track previous touch positions using our
      own structure, instead of relying on
      MainWindow.FTouches.FingerIndexPosition[FingerIndex].

      Reason: DoMotion is not immediately called when
      receiving AMOTION_EVENT_ACTION_MOVE. That is bacause we have to do
      our own event loop, that is emptied after native_app_glue is emptied,
      so we may process many AMOTION_EVENT_ACTION_MOVE before calling
      DoMotion for them.
      This is a consequence of native_app_glue weirdness, that forces us to process
      AMOTION_EVENT_ACTION_MOVE immediately, while calling DoMotion may take a while
      (esp. if it calls Application.ProcessMessage). }
    TTouchChange = object
      OldPosition, Position: TVector2Single;
    end;

    TQueuedEventType = (aeOpen, aeMotionDown, aeMotionUp, aeMotionMove);
    TQueuedEvent = object
      EventType: TQueuedEventType;
      Touch: TTouchChange;
      FingerIndex: TFingerIndex;
    end;
    TQueuedEventList = specialize TGenericStructList<TQueuedEvent>;

  const
    FingerCount = 20;

  var
    FScreenWidth, FScreenHeight: Integer;
    LastDoTimerTime: TMilisecTime;
    AndroidApp: PAndroid_App;
    QEvents: TQueuedEventList;
    TouchChanges: array [0..FingerCount - 1] of TTouchChange;

  function HandleInput(App: PAndroid_app; Event: PAInputEvent): boolean;
  procedure HandleCommand(App: PAndroid_app; Command: Integer);
{$endif read_application_interface}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_implementation}

{ Android NDK integration ---------------------------------------------------- }

procedure OpenContext;
var
  Width, Height: Integer;
  Window: TCastleWindowCustom;
  NativeWindow: PANativeWindow;
begin
  Window := Application.MainWindow;
  NativeWindow := Window.NativeWindow; // NativeWindow was saved here by HandleCommand

  { get window size, use it }
  Width := ANativeWindow_getWidth(NativeWindow);
  Height := ANativeWindow_getHeight(NativeWindow);
  AndroidLog(alInfo, 'OpenContext (size: %d %d)', [Width, Height]);
  Application.FScreenWidth := Width;
  Application.FScreenHeight := Height;
  //Window.FullScreen := true; // TODO: setting fullscreen should work like that 4 lines below. Also, should be default?
  Window.Left := 0;
  Window.Top := 0;
  Window.Width := Width;
  Window.Height := Height;

  { create OpenGL context, run OnOpen events and so on }
  Window.Open;
end;

{ Whenever the context is lost, this is called.
  It's important that we release all OpenGL resources, to recreate them later. }
procedure CloseContext;
var
  Window: TCastleWindowCustom;
begin
  AndroidLog(alInfo, 'CloseContext');

  Window := Application.MainWindow;
  if Window <> nil then
  begin
    Window.Close;
    Window.NativeWindow := nil; // make sure to not access the NativeWindow anymore
  end;
end;

procedure TCastleApplication.HandleCommand(App: PAndroid_app; Command: Integer);
var
  QEvent: TQueuedEvent;
  ConfigState: TStringStream;
begin
  case Command of
    APP_CMD_INIT_WINDOW:
      begin
        { We will actually process this event (calling TCastleWindow.Open)
          later, to avoid ANRs when OnOpen does something time-consuming
          (like SceneManager.LoadLevel). }
        MainWindow.NativeWindow := App^.Window;
        QEvent.EventType := aeOpen;
        QEvents.Add(QEvent);
      end;
    APP_CMD_TERM_WINDOW:
      begin
        { Note that we cannot delay processing this event using
          QEvents. After CloseContext, window is really destroyed,
          see android_app_post_exec_cmd in native app glue. }
        QEvents.Clear;
        CloseContext;
      end;
    APP_CMD_SAVE_STATE:
      begin
        { We cannot delay processing this, onSaveInstanceState waits for us
          to fill App^.SavedState. }
        ConfigState := TStringStream.Create('');
        try
          { read Config to ConfigState }
          Config.Save(ConfigState);
          ConfigState.Position := 0;
          { read ConfigState to App^.SavedState }
          App^.SavedState := AllocateSavedState(ConfigState.Size);
          ConfigState.ReadBuffer(App^.SavedState^, ConfigState.Size);
          App^.SavedStateSize := ConfigState.Size;
          { log }
          WritelnLogMultiline('Config', 'Saved state:' + NL + ConfigState.DataString);
        finally FreeAndNil(ConfigState) end;
      end;
  end;
end;

function TCastleApplication.HandleInput(App: PAndroid_app; Event: PAInputEvent): boolean;

  function CurrentPosition(const PointerIndex: Integer): TVector2Single;
  begin
    Result := Vector2Single(
      AMotionEvent_getX(Event, PointerIndex),
      ScreenHeight - AMotionEvent_getY(Event, PointerIndex));
  end;

  { Note that MotionDown etc. do *not* call directly DoMouseDown and similar,
    as we *cannot* block HandleInput for indefinite amount of time
    (and DoMouseDown may call OnPress which may even do MessageOK
    that makes a modal dialog calling Application.ProcessMessage inside).
    The API of native_app_glue is somewhat stupid, it allows us to process
    messages in a loop, but still the HandleInput and HandleCommand
    cannot block making a modal dialog ---  the process_input and process_cmd
    implementations in native_app_glue depend on that. }

  procedure MotionDown(const FingerIndex: TFingerIndex; const PointerIndex: Integer);
  var
    QEvent: TQueuedEvent;
  begin
    if FingerIndex < FingerCount then
    begin
      { update TouchChanges }
      TouchChanges[FingerIndex].OldPosition := TouchChanges[FingerIndex].Position;
      TouchChanges[FingerIndex].Position := CurrentPosition(PointerIndex);

      QEvent.EventType := aeMotionDown;
      QEvent.FingerIndex := FingerIndex;
      QEvent.Touch := TouchChanges[FingerIndex];
      QEvents.Add(QEvent);
    end;
  end;

  procedure MotionUp(const FingerIndex: TFingerIndex; const PointerIndex: Integer);
  var
    QEvent: TQueuedEvent;
  begin
    if FingerIndex < FingerCount then
    begin
      { update TouchChanges }
      TouchChanges[FingerIndex].OldPosition := TouchChanges[FingerIndex].Position;
      TouchChanges[FingerIndex].Position := CurrentPosition(PointerIndex);

      QEvent.EventType := aeMotionUp;
      QEvent.FingerIndex := FingerIndex;
      QEvent.Touch := TouchChanges[FingerIndex];
      QEvents.Add(QEvent);
    end;
  end;

  procedure MotionMove(const FingerIndex: TFingerIndex; const PointerIndex: Integer);
  var
    QEvent: TQueuedEvent;
    NewPosition: TVector2Single;
  const
    MinDistanceToReportMove = 0.5;
  begin
    if FingerIndex < FingerCount then
    begin
      NewPosition := CurrentPosition(PointerIndex);
      if PointsDistanceSqr(NewPosition, TouchChanges[FingerIndex].Position) >
         Sqr(MinDistanceToReportMove) then
      begin
        { update TouchChanges }
        TouchChanges[FingerIndex].OldPosition := TouchChanges[FingerIndex].Position;
        TouchChanges[FingerIndex].Position := NewPosition;

        QEvent.EventType := aeMotionMove;
        QEvent.FingerIndex := FingerIndex;
        QEvent.Touch := TouchChanges[FingerIndex];
        QEvents.Add(QEvent);
      end;
    end;
  end;

var
  MotionAction, PointerIndex, ActionPointerIndex: Integer;
begin
  Result := false;
  if AInputEvent_getType(Event) = AINPUT_EVENT_TYPE_MOTION then
  begin
    MotionAction := AMotionEvent_getAction(Event) and AMOTION_EVENT_ACTION_MASK;
    ActionPointerIndex := (AMotionEvent_getAction(Event) and
      AMOTION_EVENT_ACTION_POINTER_INDEX_MASK) shr
      AMOTION_EVENT_ACTION_POINTER_INDEX_SHIFT;

    case MotionAction of
      AMOTION_EVENT_ACTION_DOWN:
        begin
          for PointerIndex := 0 to AMotionEvent_getPointerCount(Event) - 1 do
            MotionDown(AMotionEvent_getPointerId(Event, PointerIndex), PointerIndex);
          Result := true;
        end;
      AMOTION_EVENT_ACTION_UP:
        begin
          for PointerIndex := 0 to AMotionEvent_getPointerCount(Event) - 1 do
            MotionUp(AMotionEvent_getPointerId(Event, PointerIndex), PointerIndex);
          Result := true;
        end;
      AMOTION_EVENT_ACTION_CANCEL:
        WritelnLog('Android', 'Motion cancel event');
        { TODO: Handle this like AMOTION_EVENT_ACTION_UP? }
      AMOTION_EVENT_ACTION_POINTER_DOWN:
        MotionDown(AMotionEvent_getPointerId(Event, ActionPointerIndex), ActionPointerIndex);
      AMOTION_EVENT_ACTION_POINTER_UP:
        MotionUp(AMotionEvent_getPointerId(Event, ActionPointerIndex), ActionPointerIndex);
      AMOTION_EVENT_ACTION_MOVE:
        begin
          for PointerIndex := 0 to AMotionEvent_getPointerCount(Event) - 1 do
            MotionMove(AMotionEvent_getPointerId(Event, PointerIndex), PointerIndex);
          Result := true;
        end;
    end;
  end;
end;

procedure AndroidMainImplementation(App: PAndroid_App);
var
  ConfigState: TStringStream;
begin
  Application.AndroidApp := App;

  { Use SavedState as XML contents that should be loaded to global Config.
    Note that we directly use AndroidLog below, not WritelnLog or OnWarning,
    because user code had no chance to call InitializeLog yet. }
  if App^.SavedState <> nil then
  begin
    ConfigState := TStringStream.Create('');
    try
      { load ConfigState from App^.SavedState }
      ConfigState.WriteBuffer(App^.SavedState^, App^.SavedStateSize);
      ConfigState.Position := 0;
      { load Config from ConfigState }
      try
        Config.Load(ConfigState);
        { log }
        AndroidLog(alInfo, 'Config: Loaded state:' + NL + ConfigState.DataString);
      except
        on E: TObject do
          AndroidLog(alWarn, 'Config: Cannot read XML config from Android state: ' + ExceptMessage(E));
      end;
    finally FreeAndNil(ConfigState) end;
  end else
    AndroidLog(alInfo, 'Config: no saved state');

  Application.Run;
end;

{ TCastleWindowCustom ---------------------------------------------------------- }

procedure TCastleWindowCustom.CreateBackend;
begin
end;

{ It would be nice to update Android's app menu based on MainMenu contents.
  But we cannot do this from NDK, it would require implementing Java code
  connected to us. }

procedure TCastleWindowCustom.BackendMenuInitialize;
begin
end;

procedure TCastleWindowCustom.BackendMenuFinalize;
begin
end;

procedure TCastleWindowCustom.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowCustom.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowCustom.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.OpenBackend;
begin
  ContextCreateEnd(NativeWindow);
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowCustom.CloseBackend;
begin
  ContextDestroy;
  { Note that we do not clear NativeWindow now, because we may need it
    if window is opened again. Although when Android system opens the window,
    NativeWindow is set by OpenContext,
    but when we manually reopen the window from
    code (like "reopen context" button in android_demo), we need to keep
    previous NativeWindow value. }
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
end;

procedure TCastleWindowCustom.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

procedure TCastleWindowCustom.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TCastleWindowCustom.RedirectKeyDownToMenuClick: boolean;
begin
  { Call menu shortcuts on key presses.
    Since we don't show MainMenu, this is the only way how we can at least
    support it's shortcuts. }
  Result := true;
end;

procedure TCastleWindowCustom.SetMousePosition(const Value: TVector2Single);
begin
  { There is no cursor position on Android devices.
    Just update MousePosition (although we're not really required to
    do this, code using SetMousePosition should not depend that
    it was always successfull). }
  if not Closed then
    FMousePosition := Value;
end;

procedure TCastleWindowCustom.SetFullScreen(const Value: boolean);
begin
  FFullScreen := Value; //< does nothing on Android
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
  AndroidMain := @AndroidMainImplementation;
end;

procedure TCastleApplication.DestroyBackend;
begin
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  Result := ProcessMessage(false, false);
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;

  { Call Update on Application and all open Windows, call OnTimer on Application }
  procedure WindowsUpdateAndTimer;
  begin
    DoSelfUpdate;
    FOpenWindows.DoUpdate;
    MaybeDoTimer(LastDoTimerTime);
  end;

  procedure Resize(Width, Height: Integer);
  var
    Window: TCastleWindowCustom;
  begin
    AndroidLog(alInfo, 'Resize (size: %d %d)', [Width, Height]);

    FScreenWidth := Width;
    FScreenHeight := Height;

    if not MainWindow.Closed then
      MainWindow.DoResize(Width, Height, false);
  end;

var
  Ident, Events, NewWidth, NewHeight: Integer;
  Source: Pandroid_poll_source;
  QEvent: TQueuedEvent;
  MessageHandlingBegin: TTimerResult;
begin
  MessageHandlingBegin := Timer;
  repeat
    if WaitForMessage and AllowSuspendForInput and
      { Unfortunately, we have to be more conservative than
        AllowSuspendForInput to decide when we can suspend.

        Right now we cannot suspend on non-closed window.
        Otherwise we do not react to resize events soon enough,
        since we have to actually do a couple of loop passes until resize
        reaches us. Reproducible on drawing_toy: without this,
        we will not receive resize before drawing. }
      ((MainWindow = nil) or MainWindow.Closed) then
    begin
      WritelnLog('Android', 'Waiting for next event without consuming CPU ticks.');
      Ident := ALooper_pollAll(-1 { wait }, nil, @Events, @Source);
    end else
      Ident := ALooper_pollAll(0, nil, @Events, @Source);
    if Ident < 0 then Break;

    if Source <> nil then
      Source^.Process(AndroidApp, Source);

    { poll our own events quueue.
      Yes, we need our own events queue to shield from the native_app_glue
      weirdness... }
    if MainWindow <> nil then
      while QEvents.Count > 0 do
      begin
        QEvent := QEvents.First;
        QEvents.Delete(0);
        case QEvent.EventType of
          aeOpen: OpenContext;
          aeMotionDown:
            if not MainWindow.Closed then
              MainWindow.DoMouseDown(QEvent.Touch.Position, mbLeft, QEvent.FingerIndex);
          aeMotionUp  :
            if not MainWindow.Closed then
              MainWindow.DoMouseUp  (QEvent.Touch.Position, mbLeft, QEvent.FingerIndex, false);
          aeMotionMove:
            if not MainWindow.Closed then
              MainWindow.DoMotion   (InputMotion(QEvent.Touch.OldPosition, QEvent.Touch.Position, [mbLeft], QEvent.FingerIndex));
        end;
      end;

    // Check if we are exiting.
    if AndroidApp^.DestroyRequested = 1 then
    begin
      CloseContext;
      Exit(false);
    end;

  // loop condition avoids being clogged with motion events
  until (Timer - MessageHandlingBegin) / TimerFrequency > 1 / LimitFPS;

  if (MainWindow <> nil) and not MainWindow.Closed then
  begin
    { check for Resize. As there is no reliable event to capture it
      (ANativeWindow_getWidth and ANativeWindow_getheight are immediately
      updated, but for some time EGL sizes stay old) so we just watch
      for changes, and only fire our "Resize" when really EGL size changed. }
    MainWindow.QuerySize(NewWidth, NewHeight);
    if (NewWidth <> MainWindow.Width) or
       (NewHeight <> MainWindow.Height) then
      Resize(NewWidth, NewHeight);

    { we do not check Invalidated here, just redraw every frame when window is open }
    MainWindow.DoRender;

    WindowsUpdateAndTimer;
  end;

  { Note that we ignore WaitToLimitFPS here, right now.
    When redrawing (not MainWindow.Closed), it is always "on" in the main loop,
    we don't control it, Android throttles render speed anyway.
    When not redrawing, then there's not much point in WaitToLimitFPS,
    we should rather let WaitForMessage mechanism work (when all windows are
    closed, we should usually have WaitForMessage = true). }

  Result := true;
end;

procedure TCastleApplication.Run;
begin
  if MainWindow = nil then
    raise Exception.Create('For Android, you have to assign Application.MainWindow');

  if QEvents = nil then
    QEvents := TQueuedEventList.Create else
    QEvents.Clear;

  FillByte(TouchChanges, SizeOf(TouchChanges), 0);

  { since we have AndroidApp now, prepare it }
  AndroidApp^.OnAppCmd := @HandleCommand;
  AndroidApp^.OnInputEvent := @HandleInput;
  AssetManager := AndroidApp^.Activity^.AssetManager;

  { We could not do dlopen on Android before AndroidMain is called.
    Load necessary libraries now. Some of the below libraries are not really
    available on Android anyway, but we keep the list below for completeness
    (engine gracefully handles lack of most libraries, only OpenGLES
    is really required). }
  GLES20Initialization;
  OpenALInitialization;
  VorbisFileInitialization;
  LoadFreeTypeLibrary;
  {$ifndef CASTLE_PNG_USING_FCL_IMAGE}
  {$ifndef CASTLE_PNG_STATIC}
  PngInitialization;
  {$endif}
  {$endif}
  {$ifndef CASTLE_ZLIB_USING_PASZLIB}
  ZLibInitialization;
  {$endif}

  { Note that this may be called many times, because AndroidMainImplementation
    may be called many times, because ANativeActivity_onCreate may be called
    multple times. So it is useful that Initialize has
    mechanism to not call OnInitialize more than once. }
  Initialize;

  while ProcessMessage(true, true) do ;

  FreeAndNil(QEvents);
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  { TODO }
end;

function TCastleApplication.ScreenWidth: integer;
begin
  Result := FScreenWidth;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  Result := FScreenHeight;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Android';
end;

{ TCastleClipboard ----------------------------------------------------------- }

function TCastleClipboard.GetAsText: string;
begin
  { TODO }
  Result := '';
end;

procedure TCastleClipboard.SetAsText(const Value: string);
begin
  { TODO }
end;

{ TCastleWindow -------------------------------------------------------------- }

procedure TCastleWindow.NavigationInfoChanged;
begin
end;

{$endif read_implementation}
