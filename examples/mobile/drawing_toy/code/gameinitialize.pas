{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple drawing toy. }
unit GameInitialize;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils, Classes, CastleControls, CastleImages, CastleLog, CastleColors,
  CastleFilesUtils, CastleKeysMouse, CastleVectors, CastleApplicationProperties,
  CastleGLImages, CastleRectangles;

var
  ImageControl: TCastleImageControl;
  Brushes: array [0..6] of TDrawableImage;

procedure ApplicationInitialize;

  function LoadBrush(const Name: string): TDrawableImage;
  begin
    Result := TDrawableImage.Create('castle-data:/brush_' + Name + '.png');
  end;

begin
  InitializeLog;

  Brushes[0] := LoadBrush('red');
  Brushes[1] := LoadBrush('green');
  Brushes[2] := LoadBrush('blue');
  Brushes[3] := LoadBrush('cross_red');
  Brushes[4] := LoadBrush('cross_green');
  Brushes[5] := LoadBrush('cross_blue');

  ImageControl := TCastleImageControl.Create(Window);
  Window.Controls.InsertFront(ImageControl);
end;

procedure WindowRender(Container: TCastleContainer);
var
  S: TStringList;
  I: Integer;
begin
  { Print text with FPS, Touches and other info useful for debugging }
  S := TStringList.Create;
  try
    S.Append(Format('Image: %d x %d. FPS: %s',
      [ImageControl.Image.Width,
       ImageControl.Image.Height,
       Window.Fps.ToString]));
    S.Append(Format('Touches (%d)', [Container.TouchesCount]));
    for I := 0 to Container.TouchesCount - 1 do
      S.Append(Format('Touch %d: FingerIndex %d, Position %s',
        [I,
         Container.Touches[I].FingerIndex,
         Container.Touches[I].Position.ToString ]));
    FallbackFont.PrintStrings(10, 10, Yellow, S, false, 2);
  finally FreeAndNil(S) end;
end;

procedure WindowResize(Container: TCastleContainer);
var
  NewImage: TRGBImage;
begin
  if (ImageControl.Image = nil) or
     (ImageControl.Image.Width <> Container.Width) or
     (ImageControl.Image.Height <> Container.Height) then
  begin
    NewImage := TRGBImage.Create(Container.Width, Container.Height);
    NewImage.Clear(Black);
    ImageControl.Image := NewImage;
  end;
end;

procedure Draw(const Position: TVector2; const BrushIndex: TFingerIndex);
var
  Brush: TDrawableImage;
  X, Y: Integer;
begin
  { not enough brushes, this means your touch device supports > High(Brushes) + 1
    simultaneous touches. }
  if BrushIndex > High(Brushes) then Exit;

  Brush := Brushes[BrushIndex];
  X := Round(Position[0]) - Brush.Width div 2;
  Y := Round(Position[1]) - Brush.Height div 2;

  { Old approach: drawing on CPU
    (assumes is a TRGBAlphaImage with PremultiplyAlpha done). }
  // Brush.DrawTo(ImageControl.Image, X, Y, dmAdd);
  // ImageControl.ImageChanged;

  { New approach: fast image-to-image drawing on GPU. }
  ImageControl.DrawableImage.DrawFrom(Brush,
    FloatRectangle(X, Y, Brush.Width, Brush.Height),
    FloatRectangle(Brush.Rect));
end;

procedure WindowPress(Container: TCastleContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    Draw(Event.Position, Event.FingerIndex);
end;

procedure WindowMotion(Container: TCastleContainer; const Event: TInputMotion);
begin
  if Event.Pressed <> [] then
    Draw(Event.Position, Event.FingerIndex);
end;

procedure ApplicationFinalize;
var
  I: Integer;
begin
  for I := Low(Brushes) to High(Brushes) do
    FreeAndNil(Brushes[I]);
end;

initialization
  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  Window.OnRender := @WindowRender;
  Window.OnResize := @WindowResize;
  Window.OnPress := @WindowPress;
  Window.OnMotion := @WindowMotion;
finalization
  ApplicationFinalize;
end.
