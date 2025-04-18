{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ImageTransform1: TCastleImageTransform;
    ButtonLoadGenerated, ButtonLoadPng: TCastleButton;
  private
    procedure ClickLoadGenerated(Sender: TObject);
    procedure ClickLoadPng(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleImages, CastleGLImages, CastleGLUtils, CastleRenderContext,
  CastleRectangles, CastleColors, CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonLoadGenerated.OnClick := {$ifdef FPC}@{$endif} ClickLoadGenerated;
  ButtonLoadPng.OnClick := {$ifdef FPC}@{$endif} ClickLoadPng;
  ClickLoadGenerated(nil);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickLoadGenerated(Sender: TObject);

  function CreateDrawableImage: TDrawableImage;
  var
    TestImage: TDrawableImage;
    I: Integer;
  begin
    Result := TDrawableImage.Create(1024, 1024, TRGBImage, true);

    // draw some primitives on Result
    Result.RenderToImageBegin;
    RenderContext.Clear([cbColor], Yellow);
    DrawRectangle(FloatRectangle(100, 100, 824, 824),
      // very light blue
      Vector4(0.7, 0.7, 1, 1));
    DrawRectangleOutline(FloatRectangle(200, 200, 624, 624), Red, 10);
    DrawPrimitive2D(pmLines, [
        Vector2(200, 200),
        Vector2(1024 - 200, 1024 - 200)
      ], Green,
      // default blending parameters, just to specify LineWidth later
      bsSrcAlpha, bdOneMinusSrcAlpha, false,
      10);
    Result.RenderToImageEnd;

    // draw TestImage a few times on Result
    TestImage := TDrawableImage.Create('castle-data:/test_texture.png');
    try
      Result.DrawFrom(TestImage,
        FloatRectangle(0, 0, TestImage.Width, TestImage.Height),
        TestImage.FloatRect);

      // green tint, very transparent
      TestImage.Color := Vector4(0, 0.25, 0, 0.3);
      Result.DrawFrom(TestImage,
        Result.FloatRect,
        TestImage.FloatRect);

      // tint image with red, and make it somewhat transparent
      TestImage.Color := Vector4(1, 0.25, 0.25, 0.9);
      Result.DrawFrom(TestImage,
        FloatRectangle(512, 512, 512, 512),
        TestImage.FloatRect);

      // tint image with blue, and make it somewhat transparent,
      // and draw it 4x, stretched vertically
      TestImage.Color := Vector4(0.25, 0.25, 1, 0.9);
      for I := 0 to 3 do
        Result.DrawFrom(TestImage,
          FloatRectangle(I * 128, 512, 128, 256),
          TestImage.FloatRect);
    finally
      FreeAndNil(TestImage);
    end;
  end;

var
  MyDrawableImage: TDrawableImage;
  MyImage: TCastleImage;
begin
  MyDrawableImage := CreateDrawableImage;
  try
    MyImage := MyDrawableImage.GetContents(TRGBImage);
    ImageTransform1.LoadFromImage(MyImage, true);
    { MyImage is now owned by ImageTransform1, because we passed
      TakeImageOwnership = true to ImageTransform1.LoadFromImage.
      We still should free MyDrawableImage. }
  finally FreeAndNil(MyDrawableImage); end;
end;

procedure TViewMain.ClickLoadPng(Sender: TObject);
begin
  ImageTransform1.Url := 'castle-data:/test_texture.png';
end;

end.
