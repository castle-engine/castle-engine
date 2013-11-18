{ -*- compile-command: "sh compile.sh" -*- }
library castleengine;

uses Math, JNI, SysUtils, CastleStringUtils, CastleGLUtils, CastleWindow,
  CastleUIControls, CastleVectors, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleRectangles, CastleAndroidLog, CastleUtils;

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

{ See http://developer.android.com/reference/android/opengl/GLSurfaceView.Renderer.html
  for documentation when these methods are called. }

var
  Initialized: boolean;

  Background: TCastleSimpleBackground;
  MyControl: T2DControls;
  Image: TCastleImageControl;

{ One-time initialization. }
procedure Initialize;
begin
  if Initialized then Exit;

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

  Initialized := true;
end;

procedure Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_SurfaceCreated(
  Env: PJNIEnv; Obj: JObject); jniexport;
begin
  try
    AndroidLog(alInfo, 'SurfaceCreated');

    { Whenever the context is lost, this is called.
      It's important that we release all OpenGL resources, to recreate them later
      (we wil call Window.Open only from SurfaceChanged, since we don't know
      the size yet). }
    if Window <> nil then
      Window.Close;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

procedure Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_SurfaceChanged(
  Env: PJNIEnv; Obj: JObject; Width: JInt; Height: JInt); jniexport;
begin
  try
    Initialize;

    Application.AndroidInit(Width, Height);

    if Window.Closed then
    begin
      AndroidLog(alInfo, 'SurfaceChanged %d %d - creating GL context resources', [Width, Height]);
      //Window.FullScreen := true; // sTODO: setting fullscreen should work like that 2 lines below. Also, should be default?
      Window.Width := Width;
      Window.Height := Height;
      Window.Open;
    end else
    begin
      AndroidLog(alInfo, 'SurfaceChanged %d %d - resizing, keeping GL context resources', [Width, Height]);
      Window.AndroidResize(Width, Height);
    end;

    Image.Left := 10;
    Image.Bottom := Application.ScreenHeight - 300;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

procedure Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_DrawFrame(
  Env: PJNIEnv; Obj: JObject); jniexport;
begin
  try
    GLClear([cbColor], Green); // first line on Android that worked :)
    Window.AndroidDraw;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

{ Following comments from JNI.pas from FPC,
  export JNI_OnLoad and JNI_OnUnload routines.
  They are optional (if not exported, VM will not call them).
  Maybe at some point the information in CurrentJavaVirtualMachine will be useful. }

var
  CurrentJavaVirtualMachine: PJavaVM = nil;

function JNI_OnLoad(VM: PJavaVM; Reserved: Pointer): jint; jniexport;
begin
  try
    AndroidLog(alInfo, 'JNI OnLoad');
    CurrentJavaVirtualMachine := VM;
    Result := JNI_VERSION_1_6;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

procedure JNI_OnUnload(VM: PJavaVM; Reserved: Pointer); jniexport;
begin
  try
    AndroidLog(alInfo, 'JNI OnUnload');
    CurrentJavaVirtualMachine := nil;
  except
    on E: TObject do AndroidLog(E);
  end;
end;

exports
  Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_SurfaceCreated,
  Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_SurfaceChanged,
  Java_net_sourceforge_castleengine_cgeandroidtest_GL2JNILib_DrawFrame,
  JNI_OnLoad,
  JNI_OnUnload;

function MyGetApplicationName: string;
begin
  Result := 'cge_android_lib';
end;

begin
  OnGetApplicationName := @MyGetApplicationName;
end.
