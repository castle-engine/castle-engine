{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to draw a tiling background.
  Defines a reusable class TTilingBackground. }

uses SysUtils, Classes,
  CastleVectors, CastleWindow, CastleRectangles,
  CastleUtils, CastleGLImages, CastleUIControls, CastleStringUtils,
  CastleKeysMouse;

var
  Window: TCastleWindowBase;

type
  { User interface component that draws a tiling texture. }
  TTilingBackground = class(TCastleUserInterface)
  strict private
    Image: TDrawableImage;
  public
    { Image position visible at the control's left-bottom corner,
      in the range [0..1, 0..1]. }
    ImageOrigin: TVector2;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TTilingBackground.Create(AOwner: TComponent);
begin
  inherited;
  Image := TDrawableImage.Create('castle-data:/test_texture.png');
end;

destructor TTilingBackground.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TTilingBackground.Render;
var
  ScreenRects, ImageRects: TFloatRectangleList;
  R: TFloatRectangle;
  X, Y, ImageW, ImageH: Single;
begin
  inherited;
  R := RenderRect;
  ImageW := Image.Width * UIScale;
  ImageH := Image.Height * UIScale;

  { Calculate all ScreenRects (position/size on the screen where to draw image)
    and ImageRects (position/size of the image to draw, always the same
    in this example ("Image.Rect"), we draw whole images).
    Later we can render them with one Image.Draw call.
    This is much faster (for many images) than calling Image.Draw multiple times.
  }

  ScreenRects := TFloatRectangleList.Create;
  try
    ImageRects := TFloatRectangleList.Create;
    try
      X := R.Left + ImageW * (ImageOrigin.X - 1);
      while X < R.Right do
      begin
        Y := R.Bottom + ImageH * (ImageOrigin.Y - 1);
        while Y < R.Top do
        begin
          ScreenRects.Add(FloatRectangle(X, Y, ImageW, ImageH));
          ImageRects.Add(FloatRectangle(Image.Rect));
          Y += ImageH;
        end;
        X += ImageW;
      end;

      Assert(ScreenRects.Count = ImageRects.Count);
      Image.Draw(ScreenRects.List, ImageRects.List, ScreenRects.Count);
    finally FreeAndNil(ImageRects) end;
  finally FreeAndNil(ScreenRects) end;
end;

var
  { Single instance of TTilingBackground in this application. }
  TilingBackground: TTilingBackground;

procedure Update(Container: TUIContainer);

  procedure Move(const X, Y: Single);
  const
    MoveSpeed = 1;
  var
    T: TVector2;
  begin
    T := TilingBackground.ImageOrigin;
    T := T + MoveSpeed * Container.Fps.SecondsPassed * Vector2(X, Y);
    { Use Frac to put T.X, T.Y in [0..1] range.
      TTilingBackground.Render assumes this is true. }
    T.X := Frac(T.X);
    T.Y := Frac(T.Y);
    TilingBackground.ImageOrigin := T;
  end;

begin
  if Container.Pressed[keyUp]    then Move(0,  1);
  if Container.Pressed[keyDown]  then Move(0, -1);
  if Container.Pressed[keyRight] then Move( 1, 0);
  if Container.Pressed[keyLeft]  then Move(-1, 0);
end;

begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  TilingBackground := TTilingBackground.Create(Application);
  TilingBackground.FullSize := true;

  // { Alternative size and position that are not FullSize,
  //   just to demonstrate that TTilingBackground works in this case too. }
  // TilingBackground.FullSize := false;
  // TilingBackground.Anchor(vpMiddle);
  // TilingBackground.Anchor(hpMiddle);
  // TilingBackground.Width := 300;
  // TilingBackground.Height := 300;

  Window.Controls.InsertFront(TilingBackground);

  Window.OnUpdate := @Update;
  Application.Run;
end.
