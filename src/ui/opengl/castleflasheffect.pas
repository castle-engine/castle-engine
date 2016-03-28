{
  Copyright 2016-2016 Michalis Kamburelis.

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

interface

uses Classes,
  CastleUIControls, CastleColors, CastleRectangles;

type
  { Fade out, flash, and similar screen effects
    done by blending screen with given color. }
  TCastleFlashEffect = class(TUIControl)
  private
    FIntensity: Single;
    FColor: TCastleColor;
    FDark: boolean;
    FDuration: Single;
    FWidth, FHeight: Cardinal;
    FFullSize: boolean;
  public
    const
      DefaultDuration = 0.5;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function Rect: TRectangle; override;

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
  end;

implementation

uses CastleGLUtils;

constructor TCastleFlashEffect.Create(AOwner: TComponent);
begin
  inherited;
  FDuration := DefaultDuration;
  FFullSize := true;
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
begin
  inherited;
  if FIntensity > 0 then
    if FDark then
      {$warnings off}
      GLFadeRectangle(ScreenRect, FColor, FIntensity) else
      GLFadeRectangleLight(ScreenRect, FColor, FIntensity);
      {$warnings on}
end;

procedure TCastleFlashEffect.Reset;
begin
  FIntensity := 0;
end;

end.
