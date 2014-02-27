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
  CastleFilesUtils, CastleKeysMouse, CastleVectors;

var
  ImageControl: TCastleImageControl;
  Brushes: array [0..6] of TRGBAlphaImage;

procedure ApplicationInitialize;

  function LoadBrush(const Name: string): TRGBAlphaImage;
  begin
    Result := LoadImage(ApplicationData('brush_' + Name + '.png'), [TRGBAlphaImage]) as TRGBAlphaImage;
    Result.PremultiplyAlpha; // makes drawing this image much faster
  end;

begin
  InitializeLog('1.0.0');

  Brushes[0] := LoadBrush('red');
  Brushes[1] := LoadBrush('green');
  Brushes[2] := LoadBrush('blue');
  Brushes[3] := LoadBrush('cross_red');
  Brushes[4] := LoadBrush('cross_green');
  Brushes[5] := LoadBrush('cross_blue');

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

procedure Draw(const Position: TVector2Single; const BrushIndex: TFingerIndex);
var
  Brush: TCastleImage;
  X, Y: Integer;
begin
  { not enough brushes, this means your touch device supports > High(Brushes) + 1
    simultaneous touches. }
  if BrushIndex > High(Brushes) then Exit;

  Brush := Brushes[BrushIndex];
  X := Round(Position[0]) - Brush.Width div 2;
  Y := Round(Position[1]) - Brush.Height div 2;
  Brush.DrawTo(ImageControl.Image, X, Y);
  ImageControl.ImageChanged;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    Draw(Event.Position, Event.FingerIndex);
end;

procedure WindowMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  if Event.Pressed <> [] then
    Draw(Event.Position, Event.FingerIndex);
end;

function MyGetApplicationName: string;
begin
  Result := 'drawing_toy';
end;

procedure ApplicationFinalize;
var
  I: Integer;
begin
  for I := Low(Brushes) to High(Brushes) do
    FreeAndNil(Brushes[I]);
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
  Window.OnMotion := @WindowMotion;
finalization
  ApplicationFinalize;
end.
