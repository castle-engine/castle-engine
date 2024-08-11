{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleGLImages, CastleRectangles;

type
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainImageControl: TCastleImageControl;
    LabelStatus: TCastleLabel;
  private
    Brushes: array [0..6] of TDrawableImage;
    procedure Draw(const Position: TVector2; const BrushIndex: TFingerIndex);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleImages, CastleColors;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  function LoadBrush(const Name: string): TDrawableImage;
  begin
    Result := TDrawableImage.Create('castle-data:/brush_' + Name + '.png');
  end;

var
  NewImage: TRGBImage;
  NewDrawableImage: TDrawableImage;
begin
  inherited;
  Brushes[0] := LoadBrush('red');
  Brushes[1] := LoadBrush('green');
  Brushes[2] := LoadBrush('blue');
  Brushes[3] := LoadBrush('cross_red');
  Brushes[4] := LoadBrush('cross_green');
  Brushes[5] := LoadBrush('cross_blue');

  { Create new image to draw on }
  NewImage := TRGBImage.Create(Container.PixelsWidth, Container.PixelsHeight);
  NewImage.Clear(Black);
  NewDrawableImage := TDrawableImage.Create(
    NewImage,
    { SmoothScaling } true,
    { OwnsImage } true);
  MainImageControl.DrawableImage := NewDrawableImage;
end;

procedure TViewMain.Stop;
var
  I: Integer;
begin
  for I := Low(Brushes) to High(Brushes) do
    FreeAndNil(Brushes[I]);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  S: String;
  I: Integer;
begin
  inherited;

  { Print text with FPS, Touches and other info useful for debugging }
  S := Format('Image: %d x %d. FPS: %s' + NL + 'Touches (%d)', [
    MainImageControl.Image.Width,
    MainImageControl.Image.Height,
    Container.Fps.ToString,
    Container.TouchesCount
  ]) + NL;

  for I := 0 to Container.TouchesCount - 1 do
    S := S + Format('Touch %d: FingerIndex %d, Position %s', [
      I,
      Container.Touches[I].FingerIndex,
      Container.Touches[I].Position.ToString
    ]);

  LabelStatus.Caption := S;
end;

procedure TViewMain.Resize;
var
  NewImage: TRGBImage;
  NewDrawableImage: TDrawableImage;
  CommonRect: TFloatRectangle;
begin
  inherited;

  { When window is resized, create new TDrawableImage with new size
    and copy contens from previous image (as much as possible). }

  if (MainImageControl.DrawableImage = nil) or
     (MainImageControl.DrawableImage.Width <> Container.PixelsWidth) or
     (MainImageControl.DrawableImage.Height <> Container.PixelsHeight) then
  begin
    NewImage := TRGBImage.Create(Container.PixelsWidth, Container.PixelsHeight);
    NewImage.Clear(Black);

    NewDrawableImage := TDrawableImage.Create(
      NewImage,
      { SmoothScaling } true,
      { OwnsImage } true);
    { Common part in both old and new areas }
    CommonRect := FloatRectangle(MainImageControl.DrawableImage.Rect * NewDrawableImage.Rect);
    NewDrawableImage.PrepareResources; // create OpenGL resources now, to enable DrawFrom
    NewDrawableImage.DrawFrom(MainImageControl.DrawableImage, CommonRect, CommonRect);
    MainImageControl.DrawableImage := NewDrawableImage;
  end;
end;

procedure TViewMain.Draw(const Position: TVector2; const BrushIndex: TFingerIndex);
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
  MainImageControl.DrawableImage.DrawFrom(Brush,
    FloatRectangle(X, Y, Brush.Width, Brush.Height),
    FloatRectangle(Brush.Rect));
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.EventType = itMouseButton then
  begin
    Draw(Event.Position, Event.FingerIndex);
    Exit(true);
  end;
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.Pressed <> [] then
  begin
    Draw(Event.Position, Event.FingerIndex);
    Exit(true);
  end;
end;

end.
