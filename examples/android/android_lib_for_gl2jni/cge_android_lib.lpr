{ -*- compile-command: "sh compile.sh" -*- }
library castleengine;

uses Math, JNI, CastleStringUtils, CastleGLUtils, CastleWindow,
  CastleUIControls, CastleVectors, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleRectangles;

{$ifdef mswindows}
  {$define jniexport := stdcall}
{$else}
  {$define jniexport := cdecl}
{$endif}

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

procedure Java_com_android_gl2jni_GL2JNILib_init(Env: PJNIEnv; obj: JObject;
  Width: JInt; Height: JInt); jniexport;
var
  Background: TCastleSimpleBackground;
  MyControl: T2DControls;
  Image: TCastleImageControl;
begin
  try
    Application.AndroidInit(Width, Height);

    Window := TCastleWindow.Create(Application);
    //Window.FullScreen := true; // sTODO: setting fullscreen should work like that 2 lines below. Also, should be default?
    Window.Width := Width;
    Window.Height := Height;
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
    Image.Left := 10;
    Image.Bottom := Application.ScreenHeight - 300;
    Window.Controls.InsertFront(Image);

    Window.Load('file:///sdcard/kambitest/castle_with_lights_and_camera.wrl');

    Window.Open;
  except
    // TODO: logging exceptions to Android logcat
  end;
end;

procedure Java_com_android_gl2jni_GL2JNILib_step(Env: PJNIEnv; obj: jobject); jniexport;
begin
  try
    GLClear([cbColor], Green); // first line on Android that worked :)
    Window.AndroidDraw;
  except
    // TODO: logging exceptions to Android logcat
  end;
end;

exports
  Java_com_android_gl2jni_GL2JNILib_init,
  Java_com_android_gl2jni_GL2JNILib_step;

  // TODO: switching out/in to the app makes wrong rendering, like a black
  // lighting without 2d?

begin
end.
