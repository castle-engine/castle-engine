{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Screen effects done by blending screen with given color (TCastleFlashEffect). }
unit CastleFlashEffect;

{$I castleconf.inc}

interface

uses Classes,
  CastleUIControls, CastleColors, CastleRectangles, CastleGLImages,
  CastleImages;

type
  { Fade out, flash, and similar screen effects
    done by blending screen with given color. }
  TCastleFlashEffect = class(TUIControl)
  strict private
    FIntensity: Single;
    FColor: TCastleColor;
    FDark: boolean;
    FDuration: Single;
    FWidth, FHeight: Cardinal;
    FFullSize: boolean;
    FImage, FImageAsGrayscale: TCastleImage;
    FGLImage, FGLImageAsGrayscale: TGLImageCore;
    FOwnsImage: boolean;
    procedure SetImage(const Value: TCastleImage);
    procedure ImageChanged;
  public
    const
      DefaultDuration = 0.5;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function Rect: TRectangle; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;

    procedure Flash(const AColor: TCastleColor; const ADark: boolean);
    procedure Reset;
  published
    { Rectangle where the effect will be drawn.

      When FullSize is @true (the default), the effect always fills
      the whole parent (like TCastleWindow or TCastleControl,
      if you just placed the TCastleFlashEffect on TCastleWindowCustom.Controls
      or TCastleControlCustom.Controls),
      and the values of Left, Bottom, Width, Height are ignored.

      When FullSize is @false,
      the values of Left, Bottom, Width, Height
      define the size and position of the rectangle effect.

      @seealso Rect

      @groupBegin }
    property FullSize: boolean read FFullSize write FFullSize default true;
    property Width: Cardinal read FWidth write FWidth default 0;
    property Height: Cardinal read FHeight write FHeight default 0;
    { @groupEnd }

    property Duration: Single read FDuration write FDuration
      default DefaultDuration;

    { Set this to non-nil to modulate the color with an image.
      The image is always stretched to cover our whole size. }
    property Image: TCastleImage read FImage write SetImage;

    { Free the @link(Image) instance automatically. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default false;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleGLUtils;

constructor TCastleFlashEffect.Create(AOwner: TComponent);
begin
  inherited;
  FDuration := DefaultDuration;
  FFullSize := true;
end;

destructor TCastleFlashEffect.Destroy;
begin
  if OwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  FreeAndNil(FImageAsGrayscale);
  FreeAndNil(FGLImage);
  FreeAndNil(FGLImageAsGrayscale);
  inherited;
end;

procedure TCastleFlashEffect.SetImage(const Value: TCastleImage);

  { For FadeColor, the image should be
    - white opaque where the effect IS applied
    - white transparent where the effect IS NOT applied.
    For FadeDarkColor, the image should be
    - black opaque where the effect IS applied
      (This is only an approximation.
       Only really correct if Color is exactly black.
       There's no correct value here otherwise, that would work equally
       to what DrawRectangle(FinalColor, ...) does when Image = nil.
       We would need to render image with different shader, e.g. doing
       lerp inside shader.
       This is the drawback of "Dark" flash mode.)
    - white opaque where the effect IS NOT applied. }
  function CreateGrayscaleFromAlpha(const Img: TCastleImage): TGrayscaleImage;
  var
    X, Y, Z: Integer;
  begin
    Result := TGrayscaleImage.Create(Img.Width, Img.Height, Img.Depth);
    for X := 0 to Img.Width - 1 do
      for Y := 0 to Img.Height - 1 do
        for Z := 0 to Img.Depth - 1 do
          Result.PixelPtr(X, Y, Z)^ := High(Byte) -
            Clamped(Round(Img.Colors[X, Y, Z][3] * 255), Low(Byte), High(Byte));
  end;

begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FreeAndNil(FImageAsGrayscale);

    FImage := Value;
    if FImage <> nil then
      FImageAsGrayscale := CreateGrayscaleFromAlpha(FImage);

    ImageChanged;
  end;
end;

procedure TCastleFlashEffect.GLContextOpen;
begin
  inherited;
  ImageChanged;
end;

procedure TCastleFlashEffect.GLContextClose;
begin
  FreeAndNil(FGLImage);
  FreeAndNil(FGLImageAsGrayscale);
  inherited;
end;

procedure TCastleFlashEffect.ImageChanged;
begin
  if GLInitialized then
  begin
    if FImage <> nil then
    begin
      if FGLImage <> nil then
        FGLImage.Load(FImage)
      else
        FGLImage := TGLImageCore.Create(FImage, true);
    end else
      FreeAndNil(FGLImage); // make sure to free FGLImage when FImage is nil

    if FImageAsGrayscale <> nil then
    begin
      if FGLImageAsGrayscale <> nil then
        FGLImageAsGrayscale.Load(FImageAsGrayscale)
      else
        FGLImageAsGrayscale := TGLImageCore.Create(FImageAsGrayscale, true);
    end else
      FreeAndNil(FGLImageAsGrayscale); // make sure to free FGLImageAsGrayscale when FImageAsGrayscale is nil

    VisibleChange;
  end;
end;

function TCastleFlashEffect.Rect: TRectangle;
begin
  if FullSize then
    Result := ParentRect else
  begin
    Result := Rectangle(Left, Bottom, Width, Height);
    // applying UIScale on this is easy...
    Result := Result.ScaleAround0(UIScale);
  end;
end;

procedure TCastleFlashEffect.Flash(const AColor: TCastleColor; const ADark: boolean);
begin
  FColor := AColor;
  FIntensity := 1;
  FDark := ADark;
end;

procedure TCastleFlashEffect.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  if FIntensity > 0 then
    FIntensity -= (1 / Duration) * SecondsPassed;
end;

procedure TCastleFlashEffect.Render;
var
  FinalImage: TGLImageCore;
  FinalColor: TCastleColor;
  SourceFactor: TBlendingSourceFactor;
  DestinationFactor: TBlendingDestinationFactor;
begin
  inherited;
  if FIntensity > 0 then
  begin
    if FDark then
    begin
      FinalImage := FGLImageAsGrayscale;
      FinalColor := FadeDarkColor(FColor, FIntensity);
      { Constants below make resulting screen color = FinalColor * previous screen color.
        Note that as long as all components of Color are <= 1,
        then all components of our FinalColor are also always <= 1,
        and this means that we will always make the screen darker (or equal,
        but never brighter). }
      SourceFactor := bsZero;
      DestinationFactor := bdSrcColor;
    end else
    begin
      FinalImage := FGLImage;
      FinalColor := FadeColor(FColor, FIntensity);
      SourceFactor := bsSrcAlpha;
      DestinationFactor := bdOneMinusSrcAlpha;
    end;

    if FinalImage <> nil then
    begin
      FinalImage.Color := FinalColor;
      FinalImage.Alpha := acBlending;
      FinalImage.BlendingSourceFactor := SourceFactor;
      FinalImage.BlendingDestinationFactor := DestinationFactor;
      FinalImage.Draw(ScreenRect);
    end else
      DrawRectangle(ScreenRect, FinalColor, SourceFactor, DestinationFactor, true);
  end;
end;

procedure TCastleFlashEffect.Reset;
begin
  FIntensity := 0;
end;

end.
