{
  Copyright 2016-2023 Michalis Kamburelis.

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
  CastleImages, CastleComponentSerialize;

type
  { Fade out, flash, and similar screen effects
    done by blending screen with given color. }
  TCastleFlashEffect = class(TCastleUserInterface)
  strict private
    FIntensity: Single;
    FColor: TCastleColor;
    FDark: boolean;
    FDuration: Single;
    FImage: TCastleImagePersistent;
    FDrawableImageAsGrayscale: TDrawableImage;
    procedure ImageChanged(Sender: TObject);
  public
    const
      DefaultDuration = 0.5;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;

    procedure Flash(const AColor: TCastleColor; const ADark: boolean);
    procedure Reset;

    { Is the effect in-progress.
      This is temporarily @true after calling @link(Flash).
      It automatically ends (switches to @false) after the @link(Duration)
      has passed, and when you call @link(Reset) explicitly. }
    function Active: boolean;
  published
    property Duration: Single read FDuration write FDuration
      {$ifdef FPC}default DefaultDuration{$endif};

    { Set this image to modulate the color with an image.
      The image is always stretched to cover our whole size. }
    property Image: TCastleImagePersistent read FImage;

    property FullSize default true;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleGLUtils, CastleLog, CastleRenderOptions;

constructor TCastleFlashEffect.Create(AOwner: TComponent);
begin
  inherited;
  FDuration := DefaultDuration;
  FullSize := true;
  FImage := TCastleImagePersistent.Create;
  FImage.OnChange := {$ifdef FPC}@{$endif}ImageChanged;
end;

destructor TCastleFlashEffect.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FDrawableImageAsGrayscale);
  inherited;
end;

procedure TCastleFlashEffect.ImageChanged(Sender: TObject);

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

var
  ImageAsGrayscale: TCastleImage;
begin
  VisibleChange([chRender]);

  FreeAndNil(FDrawableImageAsGrayscale);

  { TODO: It would be better to get rid of generating FDrawableImageAsGrayscale,
    and also of limitation that FImage.Image must be TCastleImage
    (cannot TEncodedImage). }

  if FImage.Image <> nil then
  begin
    if FImage.Image is TCastleImage then
    begin
      ImageAsGrayscale := CreateGrayscaleFromAlpha(FImage.Image as TCastleImage);
      FDrawableImageAsGrayscale := TDrawableImage.Create(ImageAsGrayscale, true, true);
    end else
      WritelnWarning('TODO: TCastleFlashEffect.Image must not be GPU compressed for now');
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
    FIntensity := FIntensity - ((1 / Duration) * SecondsPassed);
end;

procedure TCastleFlashEffect.Render;
var
  FinalImage: TDrawableImage;
  FinalColor: TCastleColor;
  SourceFactor: TBlendingSourceFactor;
  DestinationFactor: TBlendingDestinationFactor;
begin
  inherited;
  if Active then
  begin
    if FDark then
    begin
      if FImage.Empty then
        FinalImage := nil
      else
        FinalImage := FDrawableImageAsGrayscale;

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
      if FImage.Empty then
        FinalImage := nil
      else
        FinalImage := FImage.DrawableImage;

      FinalColor := FadeColor(FColor, FIntensity);
      SourceFactor := bsSrcAlpha;
      DestinationFactor := bdOneMinusSrcAlpha;
    end;

    if FinalImage <> nil then
    begin
      FinalImage.ScaleCorners := UIScale;
      FinalImage.Color := FinalColor;
      FinalImage.Alpha := acBlending;
      FinalImage.BlendingSourceFactor := SourceFactor;
      FinalImage.BlendingDestinationFactor := DestinationFactor;
      FinalImage.Draw(RenderRect);
    end else
      DrawRectangle(RenderRect, FinalColor, SourceFactor, DestinationFactor, true);
  end;
end;

procedure TCastleFlashEffect.Reset;
begin
  FIntensity := 0;
end;

function TCastleFlashEffect.Active: boolean;
begin
  Result := FIntensity > 0;
end;

initialization
  RegisterSerializableComponent(TCastleFlashEffect, 'Flash Effect');
end.
