program DelphiWindow;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, CastleLog, CastleWindow, CastleGLShaders, CastleGLUtils,
  CastleRectangles, CastleColors, CastleGLImages, CastleControls, castlevectors;

var
  Window: TCastleWindowBase;
  DrawableImage: TDrawableImage;
  DrawableImageCGE: TDrawableImage;
  DrawableImageDelphi: TDrawableImage;


  procedure Init(Sender: TCastleContainer);
  begin
    DrawableImage := TDrawableImage.Create('castle-data:/patreonlogoorange_45px.png');
    DrawableImageCGE := TDrawableImage.Create('castle-data:/cge_384px.png');
    DrawableImageDelphi := TDrawableImage.Create('castle-data:/daaelphi_11_384px.png');
  end;

  procedure ReleaseImg(Sender: TCastleContainer);
  begin
    FreeAndNil(DrawableImage);
    FreeAndNil(DrawableImageCGE);
    FreeAndNil(DrawableImageDelphi);
  end;

  procedure Render(Sender: TCastleContainer);
  var
    Rect: TRectangle;
  begin
    Rect.Left := 10;
    Rect.Bottom := 100;
    Rect.Width := 1000;
    Rect.Height := 500;
    DrawRectangle(Rect, Vector4(0.15, 0.15, 0.15, 1.0));
    GetUIFont.Size := 30;
    GetUIFont.Print(283, 650, Orange, 'Castle Game Engine in Delphi');
    DrawableImageDelphi.Draw(100, 150);
    DrawableImageCGE.Draw(520,150);
    GetUIFont.Print(215, 40, Silver, 'Support us on Patreon:');
    DrawableImage.Draw(600,30);
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
