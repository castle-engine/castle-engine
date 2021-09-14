program DelphiWindow;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, CastleLog, CastleWindow, CastleGLShaders, CastleGLUtils,
  CastleRectangles, CastleColors, CastleGLImages, CastleControls;

var
  Window: TCastleWindowBase;
  DrawableImage: TDrawableImage;


  procedure Init(Sender: TCastleContainer);
  begin
    DrawableImage := TDrawableImage.Create('castle-data:/patreonlogoorange_45px.png');
  end;

  procedure ReleaseImg(Sender: TCastleContainer);
  begin
    FreeAndNil(DrawableImage);
  end;

  procedure Render(Sender: TCastleContainer);
  var
    Rect: TRectangle;
  begin
    // ... e.g. DrawRectangle or TDrawableImage.Draw calls inside
    Rect.Left := 10;
    Rect.Bottom := 10;
    Rect.Width := 1000;
    Rect.Height := 500;
    DrawRectangle(Rect, Red);
    GetUIFont.Print(400, 800, Yellow, 'Castle Game Engine in Delphi');
    GetUIFont.Print(800, 650, Gray, 'Support the engine on');
    DrawableImage.Draw(815,600);
  end;

  begin
    InitializeLog;
    //LogShaders := true;
    WritelnLog('Simplest CastleWindow in Delphi');
    Window := TCastleWindowBase.Create(Application);
    Window.Caption := 'Simplest CastleWindow in Delphi';
    Window.OnRender := Render;
    Window.OnOpen := Init;
    Window.OnClose := ReleaseImg;
    Window.OpenAndRun;
  end.
