{ -*- compile-command: "sh compile.sh" -*- }
library cge_android_lib;

uses CMem, // TODO: CMem is possibly *not* needed here, only used by CastleAndroidNativeAppGlue
  Math, JNI, CTypes, SysUtils, CastleStringUtils, CastleGLUtils, CastleWindow,
  CastleUIControls, CastleVectors, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleRectangles, CastleAndroidLog, CastleUtils, CastleAndroidNativeActivity,
  CastleAndroidNativeWindow, CastleAndroidRect, CastleAndroidNativeAppGlue,
  CastleAndroidInput, CastleAndroidLooper, CastleKeysMouse;

{ We will only compile this for Android, so all exports are always cdecl. }
{$define jniexport := cdecl}

var
  Window: TCastleWindow;

type
  T2DControls = class(TUIControl)
  public
    procedure Draw; override;
    function DrawStyle: TUIControlDrawStyle; override;
  end;

function T2DControls.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure T2DControls.Draw;
begin
  DrawRectangle(Rectangle(
    Application.ScreenWidth - 100,
    Application.ScreenHeight - 100, 80, 80), Blue);
end;

var
  Background: TCastleSimpleBackground;
  MyControl: T2DControls;
  Image: TCastleImageControl;

{ One-time initialization. }
procedure Initialize;
begin
  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Transparent := true;

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertBack(Background);

  MyControl := T2DControls.Create(Window);
  Window.Controls.InsertFront(MyControl);

  Image := TCastleImageControl.Create(Window);
  // TODO: png support
  // TODO: read files using Anroid assets:
  // http://stackoverflow.com/questions/13317387/how-to-get-file-in-assets-from-android-ndk
//    Image.Image := TouchCtlOuter.MakeCopy;
  Image.URL := 'file:///sdcard/kambitest/sample_texture.ppm';
  Window.Controls.InsertFront(Image);

  Window.Load('file:///sdcard/kambitest/castle_with_lights_and_camera.wrl');
end;

procedure OpenContext(NativeWindow: PANativeWindow);
var
  Width, Height: Integer;
begin
  Window.NativeWindow := NativeWindow;
  Width := ANativeWindow_getWidth(NativeWindow);
  Height := ANativeWindow_getHeight(NativeWindow);

  AndroidLog(alInfo, 'OpenContext (%d %d)', [Width, Height]);

  Application.AndroidInit(Width, Height);

  Image.Left := 10;
  Image.Bottom := Application.ScreenHeight - 300;

  //Window.FullScreen := true; // TODO: setting fullscreen should work like that 2 lines below. Also, should be default?
  Window.Width := Width;
  Window.Height := Height;
  Window.Open;
end;

procedure CloseContext;
begin
  AndroidLog(alInfo, 'CloseContext');

  { Whenever the context is lost, this is called.
    It's important that we release all OpenGL resources, to recreate them later
    (we wil call Window.Open only from onNativeWindowResized, since we don't know
    the size yet). }
  if Window <> nil then
    Window.Close;

  Window.NativeWindow := nil; // make sure to not access the NativeWindow anymore
end;

procedure Resize(Width, Height: Integer);
begin
  AndroidLog(alInfo, 'Resize %d %d', [Width, Height]);

  Application.AndroidInit(Width, Height);
  if not Window.Closed then
    Window.AndroidResize(Width, Height);

  Image.Left := 10;
  Image.Bottom := Application.ScreenHeight - 300;
end;

procedure HandleCommand(App: PAndroid_app; Command: CInt32); cdecl;
begin
  case Command of
    APP_CMD_INIT_WINDOW: OpenContext(App^.Window);
    APP_CMD_TERM_WINDOW: CloseContext;
  end;
end;

function HandleInput(App: PAndroid_app; Event: PAInputEvent): CInt; cdecl;
var
  MotionAction, MotionIndex, MotionX, MotionY: CInt;
begin
  Result := 0;
  if AInputEvent_getType(Event) = AINPUT_EVENT_TYPE_MOTION then
  begin
    MotionAction := AMotionEvent_getAction(Event) and AMOTION_EVENT_ACTION_MASK;
    // MotionIndex := AMotionEvent_getAction(Event) shr AMOTION_EVENT_ACTION_POINTER_INDEX_SHIFT;
    MotionX := Round(AMotionEvent_getX(event, 0));
    MotionY := Round(AMotionEvent_getY(event, 0));
    case MotionAction of
      AMOTION_EVENT_ACTION_DOWN: begin Window.AndroidMouseDown(MotionX, MotionY, mbLeft); Result := 1; end;
      AMOTION_EVENT_ACTION_UP  : begin Window.AndroidMouseUp(MotionX, MotionY, mbLeft);   Result := 1; end;
      AMOTION_EVENT_ACTION_MOVE: begin Window.AndroidMouseMove(MotionX, MotionY);         Result := 1; end;
    end;
  end;
end;

procedure android_main(App: Pandroid_app); jniexport;
var
  Ident, Events, NewWidth, NewHeight: Integer;
  Source: Pandroid_poll_source;
begin
  try
    Initialize;

    App^.OnAppCmd := @HandleCommand;
    App^.OnInputEvent := @HandleInput;

    while true do
    begin
      repeat
        Ident := ALooper_pollAll(0, nil, @Events, @Source);
        if Ident < 0 then Break;

        if Source <> nil then
          Source^.Process(App, Source);

        // Check if we are exiting.
        if App^.DestroyRequested = 1 then
        begin
          CloseContext;
          Exit;
        end;

      until false;

      if not Window.Closed then
      begin
        { check for Resize. As there is no reliable event to capture it
          (ANativeWindow_getWidth and ANativeWindow_getheight are immediately
          updated, but for some time EGL sizes stay old) so we just watch
          for changes, and only fire our "Resize" when really EGL size changed. }
        Window.QuerySize(NewWidth, NewHeight);
        if (NewWidth <> Window.Width) or (NewHeight <> Window.Height) then
          Resize(NewWidth, NewHeight);

        GLClear([cbColor], Green); // first line on Android that worked :)
        Window.AndroidDraw;
      end;
    end;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

exports ANativeActivity_onCreate,
  { Export this only for our own CastleAndroidNativeAppGlue unit to load it. }
  android_main;

function MyGetApplicationName: string;
begin
  Result := 'cge_android_lib';
end;

begin
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;
end.
