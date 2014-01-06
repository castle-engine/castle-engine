{
  Copyright 2004-2013 Michalis Kamburelis.

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
CastleAndroidNativeAppGlue,
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
  FScreenWidth, FScreenHeight: Integer;
  LastDoTimerTime: TMilisecTime;
  AndroidApp: PAndroid_App;
{$endif read_application_interface}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_implementation}

{ Android NDK integration ---------------------------------------------------- }

procedure OpenContext(NativeWindow: PANativeWindow);
var
  Width, Height: Integer;
begin
  Application.MainWindow.NativeWindow := NativeWindow;
  Width := ANativeWindow_getWidth(NativeWindow);
  Height := ANativeWindow_getHeight(NativeWindow);

  AndroidLog(alInfo, 'OpenContext (size: %d %d)', [Width, Height]);

  Application.FScreenWidth := Width;
  Application.FScreenHeight := Height;

  //Application.MainWindow.FullScreen := true; // TODO: setting fullscreen should work like that 2 lines below. Also, should be default?
  Application.MainWindow.Width := Width;
  Application.MainWindow.Height := Height;
  Application.MainWindow.Open;
end;

procedure CloseContext;
begin
  AndroidLog(alInfo, 'CloseContext');

  { Whenever the context is lost, this is called.
    It's important that we release all OpenGL resources, to recreate them later
    (we wil call Window.Open only from onNativeWindowResized, since we don't know
    the size yet). }
  if Application.MainWindow <> nil then
  begin
    Application.MainWindow.Close;
    Application.MainWindow.NativeWindow := nil; // make sure to not access the NativeWindow anymore
  end;
end;

procedure Resize(Width, Height: Integer);
begin
  AndroidLog(alInfo, 'Resize (size: %d %d)', [Width, Height]);

  Application.FScreenWidth := Width;
  Application.FScreenHeight := Height;

  if not Application.MainWindow.Closed then
    Application.MainWindow.DoResize(Width, Height, false);
end;

procedure HandleCommand(App: PAndroid_app; Command: Integer);
begin
  case Command of
    APP_CMD_INIT_WINDOW: OpenContext(App^.Window);
    APP_CMD_TERM_WINDOW: CloseContext;
  end;
end;

function HandleInput(App: PAndroid_app; Event: PAInputEvent): boolean;
var
  MotionAction, {MotionIndex, }MotionX, MotionY: Integer;
begin
  Result := false;
  if AInputEvent_getType(Event) = AINPUT_EVENT_TYPE_MOTION then
  begin
    MotionAction := AMotionEvent_getAction(Event) and AMOTION_EVENT_ACTION_MASK;
    // MotionIndex := AMotionEvent_getAction(Event) shr AMOTION_EVENT_ACTION_POINTER_INDEX_SHIFT;
    MotionX := Round(AMotionEvent_getX(event, 0));
    MotionY := Round(AMotionEvent_getY(event, 0));
    case MotionAction of
      AMOTION_EVENT_ACTION_DOWN: begin Application.MainWindow.DoMouseDown(MotionX, MotionY, mbLeft); Result := true; end;
      AMOTION_EVENT_ACTION_UP  : begin Application.MainWindow.DoMouseUp(MotionX, MotionY, mbLeft);   Result := true; end;
      AMOTION_EVENT_ACTION_MOVE: begin Application.MainWindow.DoMouseMove(MotionX, MotionY);         Result := true; end;
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

{ TCastleWindowBase ---------------------------------------------------------- }

procedure TCastleWindowBase.CreateBackend;
begin
end;

// TODO: update Android's app menu based on MainMenu contents?

procedure TCastleWindowBase.BackendMenuInitialize;
begin
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowBase.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.OpenBackend;
begin
  ContextCreateEnd(NativeWindow);
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowBase.CloseBackend;
begin
  ContextDestroy;
end;

procedure TCastleWindowBase.PostRedisplay;
begin
  { no point in doing anything here, we redraw every frame }
  // if not Closed then { };
end;

procedure TCastleWindowBase.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  DoDraw;
end;

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TCastleWindowBase.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO } true;
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    { TODO };
end;

procedure TCastleWindowBase.SetFullScreen(const Value: boolean);
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
  // TODO: honor WaitForMessage, WaitToLimitFPS
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

var
  Ident, Events, NewWidth, NewHeight: Integer;
  Source: Pandroid_poll_source;
begin
  repeat
    Ident := ALooper_pollAll(0, nil, @Events, @Source);
    if Ident < 0 then Break;

    if Source <> nil then
      Source^.Process(AndroidApp, Source);

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

    MainWindow.DoDraw;

    WindowsUpdateAndTimer;
  end;

  Result := true;
end;

procedure TCastleApplication.Run;
begin
  if MainWindow = nil then
    raise Exception.Create('For Android, you have to assign Application.MainWindow');

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
