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
    FImages: array [TThemeImage] of TCastleImage;
    FCorners: array [TThemeImage] of TVector4;
    FDrawableImages: array [TThemeImage] of TDrawableImage;
    FOwnsImages: array [TThemeImage] of boolean;
    FMessageFont: TCastleFont;
    FOwnsMessageFont: boolean;
    function GetImages(const ImageType: TThemeImage): TCastleImage;
    procedure SetImages(const ImageType: TThemeImage; const Value: TCastleImage);
    function GetOwnsImages(const ImageType: TThemeImage): boolean;
    procedure SetOwnsImages(const ImageType: TThemeImage; const Value: boolean);
    function GetCorners(const ImageType: TThemeImage): TVector4;
    procedure SetCorners(const ImageType: TThemeImage; const Value: TVector4);
    function GetDrawableImages(const ImageType: TThemeImage): TDrawableImage;
    procedure GLContextClose(Sender: TObject);
    procedure SetMessageFont(const Value: TCastleFont);
  private
    { TDrawableImage instances for fast and easy drawing of images on 2D screen.
      Reading them for the 1st time means that the TDrawableImage instance is created,
      so use them only when OpenGL context is already active (window is open etc.).
      Changing the TCastleImage instance will automatically free (and recreate
      at next access) the corresponding TDrawableImage instance. }
    property DrawableImages[const ImageType: TThemeImage]: TDrawableImage read GetDrawableImages;
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

    { 2D GUI images, represented as TCastleImage.
      Although they all have sensible defaults, you can also change them
      at any time. Simply create TCastleImage instance (e.g. by LoadImage
      function) and assign it here. Be sure to adjust also @link(OwnsImages)
      if you want the theme to automatically free the image when it's no longer
      used.

      The alpha channel of the image, if any, is automatically correctly used
      (for alpha test or alpha blending, see TDrawableImage). }
    property Images[const ImageType: TThemeImage]: TCastleImage read GetImages write SetImages;

    property OwnsImages[const ImageType: TThemeImage]: boolean read GetOwnsImages write SetOwnsImages;

    { Corners that determine how image on @link(Images) is stretched when
      drawing by @link(TCastleTheme.Draw) method.
      Together with assigning @link(Images), adjust also this property.
      It is used for images rendered using TDrawableImage.Draw3x3,
      it determines how the image is stretched.
      The corners are specified as 4D vector, order like in CSS: top, right, down,
      left. }
    property Corners[const ImageType: TThemeImage]: TVector4 read GetCorners write SetCorners;

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
    property MessageFont: TCastleFont read FMessageFont write SetMessageFont;

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

  FImages[tiPanel] := Panel;
  FCorners[tiPanel] := Vector4(0, 0, 0, 0);
  FImages[tiPanelSeparator] := PanelSeparator;
  FCorners[tiPanelSeparator] := Vector4(0, 0, 0, 0);
  FImages[tiProgressBar] := ProgressBar;
  FCorners[tiProgressBar] := Vector4(2, 2, 2, 2);
  FImages[tiProgressFill] := ProgressFill;
  FCorners[tiProgressFill] := Vector4(2, 2, 2, 2);
  FImages[tiSwitchControl] := ProgressBar;      // reuse same images as ProgressBar
  FCorners[tiSwitchControl] := Vector4(2, 2, 2, 2);
  FImages[tiSwitchControlFill] := ProgressFill;
  FCorners[tiSwitchControlFill] := Vector4(2, 2, 2, 2);
  FImages[tiButtonNormal] := ButtonNormal;
  FCorners[tiButtonNormal] := Vector4(2, 2, 2, 2);
  FImages[tiButtonDisabled] := ButtonDisabled;
  FCorners[tiButtonDisabled] := Vector4(2, 2, 2, 2);
  FImages[tiButtonPressed] := ButtonPressed;
  FCorners[tiButtonPressed] := Vector4(2, 2, 2, 2);
  FImages[tiButtonFocused] := ButtonFocused;
  FCorners[tiButtonFocused] := Vector4(2, 2, 2, 2);
  FImages[tiWindow] := WindowDark;
  FCorners[tiWindow] := Vector4(2, 2, 2, 2);
  FImages[tiScrollbarFrame] := ScrollbarFrame;
  FCorners[tiScrollbarFrame] := Vector4(1, 1, 1, 1);
  FImages[tiScrollbarSlider] := ScrollbarSlider;
  FCorners[tiScrollbarSlider] := Vector4(3, 3, 3, 3);
  FImages[tiSlider] := Slider;
  FCorners[tiSlider] := Vector4(4, 7, 4, 7);
  FImages[tiSliderPosition] := SliderPosition;
  FCorners[tiSliderPosition] := Vector4(1, 1, 1, 1);
  FImages[tiLabel] := FrameWhiteBlack;
  FCorners[tiLabel] := Vector4(2, 2, 2, 2);
  FImages[tiGroup] := FrameWhiteBlack;
  FCorners[tiGroup] := Vector4(2, 2, 2, 2);
  FImages[tiActiveFrame] := FrameWhite;
  FCorners[tiActiveFrame] := Vector4(2, 2, 2, 2);
  FImages[tiTooltip] := Tooltip;
  FCorners[tiTooltip] := Vector4(1, 1, 1, 1);
  FImages[tiTouchCtlInner] := TouchCtlInner;
  FCorners[tiTouchCtlInner] := Vector4(0, 0, 0, 0);
  FImages[tiTouchCtlOuter] := TouchCtlOuter;
  FCorners[tiTouchCtlOuter] := Vector4(0, 0, 0, 0);
  FImages[tiTouchCtlFlyInner] := TouchCtlFlyInner;
  FCorners[tiTouchCtlFlyInner] := Vector4(0, 0, 0, 0);
  FImages[tiTouchCtlFlyOuter] := TouchCtlFlyOuter;
  FCorners[tiTouchCtlFlyOuter] := Vector4(0, 0, 0, 0);
  FImages[tiCrosshair1] := Crosshair1;
  FCorners[tiCrosshair1] := Vector4(0, 0, 0, 0);
  FImages[tiCrosshair2] := Crosshair2;
  FCorners[tiCrosshair2] := Vector4(0, 0, 0, 0);
  FImages[tiCheckmark] := Checkmark;
  FCorners[tiCheckmark] := Vector4(0, 0, 0, 0);

  { Note that tiSquareEmpty and tiSquarePressedBackground could have
    corners = (4, 4, 4, 4), but tiSquareChecked cannot.
    And these 3 images must be consistent.
    So, do not declare any corners for them. }
  FImages[tiSquareEmpty] := SquareEmpty;
  FCorners[tiSquareEmpty] := Vector4(0, 0, 0, 0);
  FImages[tiSquareChecked] := SquareChecked;
  FCorners[tiSquareChecked] := Vector4(0, 0, 0, 0);
  FImages[tiSquarePressedBackground] := SquarePressedBackground;
  FCorners[tiSquarePressedBackground] := Vector4(0, 0, 0, 0);

  FImages[tiDisclosure] := Disclosure;
  FCorners[tiDisclosure] := Vector4(0, 0, 0, 0);
  FImages[tiLoading] := Loading;
  FCorners[tiLoading] := Vector4(0, 0, 0, 0);
  FImages[tiEdit] := Edit;
  FCorners[tiEdit] := Vector4(2, 2, 2, 2);

  ApplicationProperties.OnGLContextCloseObject.Add(@GLContextClose);
end;

destructor TCastleTheme.Destroy;
var
  I: TThemeImage;
begin
  ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextClose);
  for I in TThemeImage do
    Images[I] := nil; // will free Images[I] if necessary
  if OwnsMessageFont then
    FreeAndNil(FMessageFont) else
    FMessageFont := nil;
  inherited;
end;

function TCastleTheme.GetImages(const ImageType: TThemeImage): TCastleImage;
begin
  Result := FImages[ImageType];
end;

procedure TCastleTheme.SetImages(const ImageType: TThemeImage;
  const Value: TCastleImage);
begin
  if FImages[ImageType] <> Value then
  begin
    { free previous image }
    if FOwnsImages[ImageType] then
      FreeAndNil(FImages[ImageType]);
    FImages[ImageType] := Value;
    FreeAndNil(FDrawableImages[ImageType]);
  end;
end;

function TCastleTheme.GetOwnsImages(const ImageType: TThemeImage): boolean;
begin
  Result := FOwnsImages[ImageType];
end;

procedure TCastleTheme.SetOwnsImages(const ImageType: TThemeImage;
  const Value: boolean);
begin
  FOwnsImages[ImageType] := Value;
end;

function TCastleTheme.GetCorners(const ImageType: TThemeImage): TVector4;
begin
  Result := FCorners[ImageType];
end;

procedure TCastleTheme.SetCorners(const ImageType: TThemeImage; const Value: TVector4);
begin
  FCorners[ImageType] := Value;
end;

function TCastleTheme.GetDrawableImages(const ImageType: TThemeImage): TDrawableImage;
begin
  if FDrawableImages[ImageType] = nil then
    FDrawableImages[ImageType] := TDrawableImage.Create(FImages[ImageType], true, false);
  Result := FDrawableImages[ImageType];
end;

procedure TCastleTheme.GLContextClose(Sender: TObject);
var
  ImageType: TThemeImage;
begin
  for ImageType in TThemeImage do
    FreeAndNil(FDrawableImages[ImageType]);
end;

procedure TCastleTheme.Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
  const UIScale: Single);
begin
  Draw(Rect, ImageType, UIScale, White);
end;

procedure TCastleTheme.Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
  const UIScale: Single; const Color: TCastleColor);
begin
  DrawableImages[ImageType].Color := Color;
  DrawableImages[ImageType].ScaleCorners := UIScale;
  DrawableImages[ImageType].Draw3x3(Rect, Corners[ImageType]);
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

procedure TCastleTheme.SetMessageFont(const Value: TCastleFont);
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
  Images[tiWindow] := WindowGray;
  Images[tiLabel] := FrameYellowBlack;
end;

var
  FTheme: TCastleTheme;

function Theme: TCastleTheme;
begin
  Result := FTheme;
end;

{$endif read_implementation}
