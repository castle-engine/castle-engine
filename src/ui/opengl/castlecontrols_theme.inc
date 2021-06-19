{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Theme for 2D GUI controls.
    Should only be used through the single global instance @link(Theme). }
  TCastleTheme = class
  strict private
    FImagesPersistent: array [TThemeImage] of TCastleImagePersistent;
    FMessageFont: TCastleAbstractFont;
    FOwnsMessageFont: boolean;
    function GetImages(const ImageType: TThemeImage): TCastleImage;
    procedure SetImages(const ImageType: TThemeImage; const Value: TCastleImage);
    function GetOwnsImages(const ImageType: TThemeImage): boolean;
    procedure SetOwnsImages(const ImageType: TThemeImage; const Value: boolean);
    function GetImagePersistent(const ImageType: TThemeImage): TCastleImagePersistent;
    function GetCorners(const ImageType: TThemeImage): TVector4;
    procedure SetCorners(const ImageType: TThemeImage; const Value: TVector4);
    procedure SetMessageFont(const Value: TCastleAbstractFont);
  public
    TooltipTextColor: TCastleColor;
    TextColor, DisabledTextColor: TCastleColor;
    MessageTextColor: TCastleColor;
    MessageInputTextColor: TCastleColor;

    BarEmptyColor: TVector3Byte;
    BarFilledColor: TVector3Byte;

    { Color layer displayed between modal dialogs (CastleMessages, CastleDialogStates, TGLModeFrozenScreen)
      and their background (state underneath or a screenshot).
      Default is (0, 0, 0, 0.25), making things underneath darker,
      which helps dialog to stand out. }
    BackgroundColor: TCastleColor;

    { Color used instead of BackgroundColor when InternalForceOpaqueBackground is used.
      You can only customize the RGB values, alpha is always 1, so the resulting
      color is always opaque.
      It's a dark gray color (0.33, 0.33, 0.33) by default. }
    BackgroundOpaqueColor: TCastleColorRGB;

    { Colors used when displaying the "Loading..." text when Android application
      is resuming. Note that you can also customize the tiLoading image.
      By default, LoadingBackgroundColor is black, and LoadingTextColor is white. }
    LoadingBackgroundColor, LoadingTextColor: TCastleColor;

    { For what window height was the Images[tiLoading] prepared.
      If you set Images[tiLoading], then most likely you will want to set it too.
      The rendered size of Images[tiLoading] is adjusted, looking at actual
      window height and this property.

      Note that when rendering Images[tiLoading], TCastleApplication.OnInitialize
      is not run, so Container.UIScaling is usually not initialized yet.
      That's why we instead do this special scaling approach for Images[tiLoading].

      By default this is 1000. }
    LoadingImageForWindowHeight: Single;

    { Width of scrollbar for TCastleScrollView. }
    ScrollBarWidth: Single;
    { Minumum scrollbar height, to be comfortably visible and draggable. }
    MinimumScrollbarSliderHeight: Single;

    { Request modal boxes to use opaque color from @link(BackgroundOpaqueColor).

      This is internally set and unset by the CastleWindow exception handler
      (you should not modify this yourself).
      It allows to request showing opaque color under
      @link(TCastleWindowBase.MessageOK) and friends,
      or modal states by @link(TGLModeFrozenScreen).

      This allows to avoid making a screenshot (TStateDialog.BackgroundScreenshot may be false),
      which is safer when we want to show an exception
      (since the application may be in some dirty state, so it's better to avoid
      rendering user stuff).

      @exclude }
    InternalForceOpaqueBackground: boolean;

    DefaultLabelWhite: Boolean deprecated 'if you want the labels to be white, adjust TCastleLabel.Color to White after creating every TCastleLabel';

    constructor Create;
    destructor Destroy; override;

    { Theme images, represented as TCastleImage.
      Although they all have sensible defaults, you can also change them
      at any time. Simply create TCastleImage instance (e.g. by LoadImage
      function) and assign it here. Be sure to adjust also @link(OwnsImages)
      if you want the theme to automatically free the image when it's no longer
      used.

      The alpha channel of the image, if any, is automatically correctly used
      (for alpha test or alpha blending, see TDrawableImage). }
    property Images[const ImageType: TThemeImage]: TCastleImage read GetImages write SetImages; deprecated 'use ImagesPersistent[...].Image';

    { Images used to draw UI controls by default.

      You can adjust them e.g. to adjust how a default TCastleButton looks.
      Note that some controls have properties to override the "theme" look
      for a particular control, e.g. on a particular TCastleButton
      you can set @link(TCastleButton.CustomBackground) and
      @link(TCastleButton.CustomBackgroundNormal).

      When adjusting these images, it is simplest to assign
      the image URL by setting @link(TCastleImagePersistent.URL).
      See https://castle-engine.io/manual_2d_user_interface.php#section_theme .
      Adjust also @link(TCastleImagePersistent.ProtectedSides) to use
      @url(https://en.wikipedia.org/wiki/9-slice_scaling 9-slice scaling). }
    property ImagesPersistent[const ImageType: TThemeImage]: TCastleImagePersistent read GetImagePersistent;

    property OwnsImages[const ImageType: TThemeImage]: boolean read GetOwnsImages write SetOwnsImages; deprecated 'use ImagesPersistent[...].OwnsImage';

    { Corners that determine how image on @link(Images) is stretched when
      drawing by @link(TCastleTheme.Draw) method.
      Together with assigning @link(Images), adjust also this property.
      It is used for images rendered using TDrawableImage.Draw3x3,
      it determines how the image is stretched.
      The corners are specified as 4D vector, order like in CSS: top, right, down,
      left. }
    property Corners[const ImageType: TThemeImage]: TVector4 read GetCorners write SetCorners; deprecated 'use ImagesPersistent[...].ProtectedSides';

    { Draw the selected theme image on screen.

      @param(Color Color tint of the image.
        If you do not specify a color, white will be used, so image will be displayed
        as-is. Specifying a color means that image will be multiplied by it,
        just like for @link(TDrawableImage.Color).)

      @param(UIScale Used to properly scale corners, passed to @link(TDrawableImage.ScaleCorners).
        This parameter does @italic(not) scale the place where image is displayed,
        to do this just scale the given Rect parameter yourself.)
    }
    procedure Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
      const UIScale: Single = 1.0);
    procedure Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
      const UIScale: Single; const Color: TCastleColor);
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single = 1.0);
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single; const Color: TCastleColor);

    { Font used by dialogs.
      Leave @nil to use @link(TUIContainer.DefaultFont). }
    property MessageFont: TCastleAbstractFont read FMessageFont write SetMessageFont;

    property OwnsMessageFont: boolean
      read FOwnsMessageFont write FOwnsMessageFont default true;

    { Set dialogs theme to light. }
    procedure DialogsLight;
  end;

function Theme: TCastleTheme;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTheme --------------------------------------------------------------- }

constructor TCastleTheme.Create;
var
  T: TThemeImage;
begin
  inherited;
  TooltipTextColor       := Vector4(0   , 0, 0, 1);
  TextColor              := Vector4(0   , 0, 0, 1);
  DisabledTextColor      := Vector4(0.33, 0.33, 0.33, 1);
  MessageInputTextColor  := Vector4(0.33, 1, 1, 1);
  MessageTextColor       := Vector4(1   , 1, 1, 1);
  BackgroundColor        := Vector4(0, 0, 0, 0.25);
  BackgroundOpaqueColor  := Vector3(0.33, 0.33, 0.33);
  LoadingBackgroundColor := Black;
  LoadingTextColor       := White;
  LoadingImageForWindowHeight := 1000;
  ScrollBarWidth := 20;
  MinimumScrollbarSliderHeight := 20;

  FOwnsMessageFont := true;

  for T in TThemeImage do
  begin
    FImagesPersistent[T] := TCastleImagePersistent.Create;
    FImagesPersistent[T].OwnsImage := false;
  end;

  FImagesPersistent[tiPanel].Image := Panel;
  FImagesPersistent[tiPanelSeparator].Image := PanelSeparator;
  FImagesPersistent[tiProgressBar].Image := ProgressBar;
  FImagesPersistent[tiProgressBar].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiProgressFill].Image := ProgressFill;
  FImagesPersistent[tiProgressFill].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiSwitchControl].Image := ProgressBar;      // reuse same images as ProgressBar
  FImagesPersistent[tiSwitchControl].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiSwitchControlFill].Image := ProgressFill;
  FImagesPersistent[tiSwitchControlFill].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiButtonNormal].Image := ButtonNormal;
  FImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiButtonDisabled].Image := ButtonDisabled;
  FImagesPersistent[tiButtonDisabled].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiButtonPressed].Image := ButtonPressed;
  FImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiButtonFocused].Image := ButtonFocused;
  FImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiWindow].Image := WindowDark;
  FImagesPersistent[tiWindow].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiScrollbarFrame].Image := ScrollbarFrame;
  FImagesPersistent[tiScrollbarFrame].ProtectedSides.AllSides := 1;
  FImagesPersistent[tiScrollbarSlider].Image := ScrollbarSlider;
  FImagesPersistent[tiScrollbarSlider].ProtectedSides.AllSides := 3;
  FImagesPersistent[tiSlider].Image := Slider;
  FImagesPersistent[tiSlider].ProtectedSides.Top := 4;
  FImagesPersistent[tiSlider].ProtectedSides.Bottom := 4;
  FImagesPersistent[tiSlider].ProtectedSides.Left := 7;
  FImagesPersistent[tiSlider].ProtectedSides.Right := 7;
  FImagesPersistent[tiSliderPosition].Image := SliderPosition;
  FImagesPersistent[tiSliderPosition].ProtectedSides.AllSides := 1;
  FImagesPersistent[tiLabel].Image := FrameWhiteBlack;
  FImagesPersistent[tiLabel].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiGroup].Image := FrameWhiteBlack;
  FImagesPersistent[tiGroup].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiActiveFrame].Image := FrameWhite;
  FImagesPersistent[tiActiveFrame].ProtectedSides.AllSides := 2;
  FImagesPersistent[tiTooltip].Image := Tooltip;
  FImagesPersistent[tiTooltip].ProtectedSides.AllSides := 1;
  FImagesPersistent[tiTouchCtlInner].Image := TouchCtlInner;
  FImagesPersistent[tiTouchCtlOuter].Image := TouchCtlOuter;
  FImagesPersistent[tiTouchCtlFlyInner].Image := TouchCtlFlyInner;
  FImagesPersistent[tiTouchCtlFlyOuter].Image := TouchCtlFlyOuter;
  FImagesPersistent[tiCrosshair1].Image := Crosshair1;
  FImagesPersistent[tiCrosshair2].Image := Crosshair2;
  FImagesPersistent[tiCheckmark].Image := Checkmark;

  { Note that tiSquareEmpty and tiSquarePressedBackground could have
    corners = (4, 4, 4, 4), but tiSquareChecked cannot.
    And these 3 images must be consistent.
    So, do not declare any corners for them. }
  FImagesPersistent[tiSquareEmpty].Image := SquareEmpty;
  FImagesPersistent[tiSquareChecked].Image := SquareChecked;
  FImagesPersistent[tiSquarePressedBackground].Image := SquarePressedBackground;

  FImagesPersistent[tiDisclosure].Image := Disclosure;
  FImagesPersistent[tiLoading].Image := Loading;
  FImagesPersistent[tiEdit].Image := Edit;
  FImagesPersistent[tiEdit].ProtectedSides.AllSides := 2;
end;

destructor TCastleTheme.Destroy;
var
  T: TThemeImage;
begin
  for T in TThemeImage do
    FreeAndNil(FImagesPersistent[T]);

  if OwnsMessageFont then
    FreeAndNil(FMessageFont)
  else
    FMessageFont := nil;
  inherited;
end;

function TCastleTheme.GetImages(const ImageType: TThemeImage): TCastleImage;
begin
  Result := FImagesPersistent[ImageType].Image as TCastleImage;
end;

procedure TCastleTheme.SetImages(const ImageType: TThemeImage;
  const Value: TCastleImage);
begin
  FImagesPersistent[ImageType].Image := Value;
end;

function TCastleTheme.GetOwnsImages(const ImageType: TThemeImage): boolean;
begin
  Result := FImagesPersistent[ImageType].OwnsImage;
end;

procedure TCastleTheme.SetOwnsImages(const ImageType: TThemeImage;
  const Value: boolean);
begin
  FImagesPersistent[ImageType].OwnsImage := Value;
end;

function TCastleTheme.GetImagePersistent(const ImageType: TThemeImage): TCastleImagePersistent;
begin
  Result := FImagesPersistent[ImageType];
end;

function TCastleTheme.GetCorners(const ImageType: TThemeImage): TVector4;
var
  B: TBorder;
begin
  B := FImagesPersistent[ImageType].ProtectedSides;
  Result := Vector4(
    B.TotalTop,
    B.TotalRight,
    B.TotalBottom,
    B.TotalLeft
  );
end;

procedure TCastleTheme.SetCorners(const ImageType: TThemeImage; const Value: TVector4);
var
  B: TBorder;
begin
  B := FImagesPersistent[ImageType].ProtectedSides;
  B.AllSides := 0;
  B.Top := Value[0];
  B.Right := Value[1];
  B.Bottom := Value[2];
  B.Left := Value[3];
end;

procedure TCastleTheme.Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
  const UIScale: Single);
begin
  Draw(Rect, ImageType, UIScale, White);
end;

procedure TCastleTheme.Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
  const UIScale: Single; const Color: TCastleColor);
begin
  FImagesPersistent[ImageType].Color := Color;
  FImagesPersistent[ImageType].ScaleCorners := UIScale;
  FImagesPersistent[ImageType].Draw(Rect);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage;
  const UIScale: Single);
begin
  Draw(FloatRectangle(Rect), ImageType, UIScale);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage;
  const UIScale: Single; const Color: TCastleColor);
begin
  Draw(FloatRectangle(Rect), ImageType, UIScale, Color);
end;

procedure TCastleTheme.SetMessageFont(const Value: TCastleAbstractFont);
begin
  if FMessageFont <> Value then
  begin
    if OwnsMessageFont then
      FreeAndNil(FMessageFont);
    FMessageFont := Value;
  end;
end;

procedure TCastleTheme.DialogsLight;
begin
  MessageInputTextColor := Vector4(0, 0.4, 0, 1.0);
  MessageTextColor := Black;
  FImagesPersistent[tiWindow].Image := WindowGray;
  FImagesPersistent[tiWindow].OwnsImage := false;
  FImagesPersistent[tiLabel].Image := FrameYellowBlack;
  FImagesPersistent[tiLabel].OwnsImage := false;
end;

var
  FTheme: TCastleTheme;

function Theme: TCastleTheme;
begin
  Result := FTheme;
end;

{$endif read_implementation}
