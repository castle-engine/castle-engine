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
CastleAndroidNativeAppGlue, CastleGenericLists,
{$endif}

{$ifdef read_implementation_uses}
CastleAndroidNativeWindow, CastleAndroidInput, CastleAndroidLooper,
CastleAndroidLog, CastleAndroidAssetStream,
{$endif}

{$ifdef read_window_interface}
private
  NativeWindow: EGLNativeWindowType; //< must be set before Window.Open
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  type
    TQueuedEventType = (aeOpen, aeMotionDown, aeMotionUp, aeMotionMove);
    TQueuedEvent = object
      EventType: TQueuedEventType;
      MotionX, MotionY: Integer;
    end;
    TQueuedEventList = specialize TGenericStructList<TQueuedEvent>;
  var
    FScreenWidth, FScreenHeight: Integer;
    LastDoTimerTime: TMilisecTime;
    AndroidApp: PAndroid_App;
    QEvents: TQueuedEventList;
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
  //Window.FullScreen := true; // TODO: setting fullscreen should work like that 2 lines below. Also, should be default?
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
    Window.Close;
end;

procedure HandleCommand(App: PAndroid_app; Command: Integer);
var
  QEvent: TCastleApplication.TQueuedEvent;
begin
  case Command of
    APP_CMD_INIT_WINDOW:
      begin
        { We will actually process this event (calling TCastleWindow.Open)
          later, to avoid ANRs when OnOpen does something time-consuming
          (like SceneManager.LoadLevel). }
        Application.MainWindow.NativeWindow := App^.Window;
        QEvent.EventType := aeOpen;
        Application.QEvents.Add(QEvent);
      end;
    APP_CMD_TERM_WINDOW:
      begin
        { Note that we cannot delay processing this event using
          Application.QEvents. After CloseContext, window is really destroyed,
          see android_app_post_exec_cmd in native app glue. }
        Application.QEvents.Clear;
        CloseContext;
      end;
  end;
end;

function HandleInput(App: PAndroid_app; Event: PAInputEvent): boolean;
var
  MotionAction {, MotionIndex}: Integer;
  QEvent: TCastleApplication.TQueuedEvent;
begin
  Result := false;
  if AInputEvent_getType(Event) = AINPUT_EVENT_TYPE_MOTION then
  begin
    MotionAction := AMotionEvent_getAction(Event) and AMOTION_EVENT_ACTION_MASK;
    // MotionIndex := AMotionEvent_getAction(Event) shr AMOTION_EVENT_ACTION_POINTER_INDEX_SHIFT;
    QEvent.MotionX := Round(AMotionEvent_getX(Event, 0));
    QEvent.MotionY := Round(AMotionEvent_getY(Event, 0));

    { We do *not* call here directly DoMouseDown and such,
      as we *cannot* block HandleInput for indefinite amount of time
      (and DoMouseDown may call OnPress which may even do MessageOK
      that makes a modal dialog calling Application.ProcessMessage inside).

      The API of native_app_glue is somewhat stupid, it allows us to process
      messages in a loop, but still the HandleInput and HandleCommand
      cannot block making a modal dialog ---  the process_input and process_cmd
      implementations in native_app_glue depend on that. }

    case MotionAction of
      AMOTION_EVENT_ACTION_DOWN:
        begin
          QEvent.EventType := aeMotionDown;
          Application.QEvents.Add(QEvent);
          Result := true;
        end;
      AMOTION_EVENT_ACTION_UP:
        begin
          QEvent.EventType := aeMotionUp;
          Application.QEvents.Add(QEvent);
          Result := true;
        end;
      AMOTION_EVENT_ACTION_MOVE:
        begin
          QEvent.EventType := aeMotionMove;
          Application.QEvents.Add(QEvent);
          Result := true;
        end;
    end;
  end;
end;

procedure AndroidMainImplementation(App: PAndroid_App);
begin
  try
    Application.AndroidApp := App;
    Application.Run;
  except
    on E: TObject do AndroidLog(E);
  end;
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
  NativeWindow := nil; // make sure to not access the NativeWindow anymore
end;

procedure TCastleWindowCustom.Invalidate;
begin
  { no point in doing anything here, we redraw every frame }
  // if not Closed then { };
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
end;

procedure TCastleWindowCustom.FlushRedisplay;
begin
  DoRender;
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

procedure TCastleWindowCustom.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  { There is no cursor position on Android devices.
    Just update MouseX,Y properties (although we're not really required to
    do this, code using SetMousePosition should not depend that
    it was always successfull). }
  if not Closed then
  begin
    FMouseX := NewMouseX;
    FMouseY := NewMouseY;
  end;
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

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  { We have no way to honour WaitForMessage or WaitToLimitFPS values
    on Android.
    - WaitToLimitFPS is always "on" in the main loop,
      we don't control it, Android throttles render speed anyway.
    - WaitForMessage could be possibly implemented, but it's not useful much. }
  Result := ProcessAllMessages;
end;

function TCastleApplication.ProcessAllMessages: boolean;

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

    Application.FScreenWidth := Width;
    Application.FScreenHeight := Height;

    Window := Application.MainWindow;
    if not Window.Closed then
      Window.DoResize(Width, Height, false);
  end;

var
  Ident, Events, NewWidth, NewHeight: Integer;
  Source: Pandroid_poll_source;
  QEvent: TQueuedEvent;
begin
  repeat
    Ident := ALooper_pollAll(0, nil, @Events, @Source);
    if Ident < 0 then Break;

    if Source <> nil then
      Source^.Process(AndroidApp, Source);

    { poll our own events quueue.
      Yes, we need our own events queue to shield from the native_app_glue
      weirdness... }
    while QEvents.Count > 0 do
    begin
      QEvent := QEvents.First;
      QEvents.Delete(0);
      case QEvent.EventType of
        aeOpen      : OpenContext;
        aeMotionDown: MainWindow.DoMouseDown(QEvent.MotionX, QEvent.MotionY, mbLeft);
        aeMotionUp  : MainWindow.DoMouseUp  (QEvent.MotionX, QEvent.MotionY, mbLeft);
        aeMotionMove: MainWindow.DoMouseMove(QEvent.MotionX, QEvent.MotionY);
      end;
    end;

    // Check if we are exiting.
    if AndroidApp^.DestroyRequested = 1 then
    begin
      CloseContext;
      Exit(false);
    end;
  until false;

  if not MainWindow.Closed then
  begin
    { check for Resize. As there is no reliable event to capture it
      (ANativeWindow_getWidth and ANativeWindow_getheight are immediately
      updated, but for some time EGL sizes stay old) so we just watch
      for changes, and only fire our "Resize" when really EGL size changed. }
    MainWindow.QuerySize(NewWidth, NewHeight);
    if (NewWidth <> MainWindow.Width) or
       (NewHeight <> MainWindow.Height) then
      Resize(NewWidth, NewHeight);

    MainWindow.DoRender;

    WindowsUpdateAndTimer;
  end;

  Result := true;
end;

procedure TCastleApplication.Run;
begin
  if MainWindow = nil then
    raise Exception.Create('For Android, you have to assign Application.MainWindow');

  if QEvents = nil then
    QEvents := TQueuedEventList.Create else
    QEvents.Clear;

  { since we have AndroidApp now, prepare it }
  AndroidApp^.OnAppCmd := @HandleCommand;
  AndroidApp^.OnInputEvent := @HandleInput;
  AssetManager := AndroidApp^.Activity^.AssetManager;

  { Note that this may be called many times, because AndroidMainImplementation
    may be called many times, because ANativeActivity_onCreate may be called
    multple times. So it is useful that Initialize has
    mechanism to not call OnInitialize more than once. }
  Initialize;

  while ProcessAllMessages do ;

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

procedure TCastleWindow.NavigationInfoChanged(Sender: TObject);
begin
end;

{$endif read_implementation}
