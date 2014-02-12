{
  Copyright 2013-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple drawing toy. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils, CastleControls, CastleImages, CastleLog, CastleColors,
  CastleFilesUtils, CastleKeysMouse;

var
  ImageControl: TCastleImageControl;
  BrushWhite: TRGBAlphaImage;
  BrushYellow: TRGBAlphaImage;

procedure ApplicationInitialize;
begin
  InitializeLog('1.0.0');

  BrushWhite := LoadImage(ApplicationData('brush_white.png'), [TRGBAlphaImage]) as TRGBAlphaImage;
  BrushWhite.PremultiplyAlpha; // makes drawing this image much faster
  BrushYellow := LoadImage(ApplicationData('brush_yellow.png'), [TRGBAlphaImage]) as TRGBAlphaImage;
  BrushYellow.PremultiplyAlpha;

  ImageControl := TCastleImageControl.Create(Window);
  Window.Controls.InsertFront(ImageControl);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, Format('Image: %d x %d. FPS : %f (real : %f)',
    [ImageControl.Image.Width,
     ImageControl.Image.Height,
     Window.Fps.FrameTime,
     Window.Fps.RealTime]));
end;

procedure WindowResize(Container: TUIContainer);
var
  NewImage: TRGBImage;
begin
  if ImageControl.Image = nil then
  begin
    NewImage := TRGBImage.Create(Container.Width, Container.Height);
    NewImage.Clear(Black);
    ImageControl.Image := NewImage;
  end else
  if (ImageControl.Image.Width <> Container.Width) or
     (ImageControl.Image.Height <> Container.Height) then
  begin
    NewImage := TRGBImage.Create(Container.Width, Container.Height);
    NewImage.Clear(Black);
    { CopyFrom will take care to only copy the relevant part.
      Where previous ImageControl.Image was larger, it will be cut off.
      Where previous ImageControl.Image was smaller, the black color will remain. }
    NewImage.DrawFrom(ImageControl.Image, 0, 0);
    ImageControl.Image := NewImage;
  end;
end;

procedure Draw(X, Y: Integer);
var
  Brush: TCastleImage;
begin
  if mbLeft in Window.MousePressed then
    Brush := BrushWhite else
    Brush := BrushYellow;
  X := X - Brush.Width div 2;
  Y := Window.Height - Y - Brush.Height div 2;
  Brush.DrawTo(ImageControl.Image, X, Y);
  ImageControl.ImageChanged;
end;

procedure WindowPress(Container: TUIContainer; const Input: TInputPressRelease);
begin
  Draw(Container.MouseX, Container.MouseY);
end;

procedure WindowMouseMove(Container: TUIContainer; NewX, NewY: Integer);
begin
  if Window.MousePressed <> [] then
    Draw(NewX, NewY);
end;

function MyGetApplicationName: string;
begin
  Result := 'drawing_toy';
end;

initialization
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnResize := @WindowResize;
  Window.OnPress := @WindowPress;
  Window.OnMouseMove := @WindowMouseMove;
finalization
  FreeAndNil(BrushWhite);
  FreeAndNil(BrushYellow);
end.
