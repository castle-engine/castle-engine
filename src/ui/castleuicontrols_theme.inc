{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Type of theme image, use of @link(TCastleTheme.ImagesPersistent). }
  TThemeImage = (
    tiPanel, tiPanelSeparator, tiProgressBar, tiProgressFill,
    tiButtonPressed, tiButtonDisabled, tiButtonFocused, tiButtonNormal,
    tiWindow, tiScrollbarFrame, tiScrollbarSlider,
    tiSliderBackground, tiSliderThumb, tiLabel, tiGroup, tiActiveFrame, tiTooltip,
    tiTouchCtlInner, tiTouchCtlOuter, tiTouchCtlFlyInner, tiTouchCtlFlyOuter,
    tiCrosshair1, tiCrosshair2, tiCheckmark,
    tiSquareEmpty, tiSquareChecked, tiSquarePressedBackground,
    tiDisclosure,
    tiSwitchControl, tiSwitchControlFill,

    { Image displayed when the application is initializing,
      during @link(TCastleApplication.OnInitialize Application.OnInitialize)
      and @link(TCastleWindow.OnOpen Window.OnOpen).
      And @link(TCastleUserInterface.GLContextOpen) for all initially present UI controls.
      This "loading image" is loaded and displayed first,
      so that user does not see a black screen while the resources are prepared.

      It is especially useful on Android, where we can lose the OpenGL context
      at any moment, as user may switch applications in the middle of the game.
      When getting back to the application, we need to initiailize some
      resources, and during this process we also show this image.
      So this serves as a universal "please wait, we're loading" screen.

      You can customize this image, by setting
      @link(TCastleTheme.ImagesPersistent Theme.ImagesPersistent[tiLoading].Image),
      @link(TCastleTheme.LoadingBackgroundColor LoadingBackgroundColor),
      @link(TCastleTheme.LoadingTextColor LoadingTextColor).
      See https://castle-engine.io/manual_2d_user_interface.php#section_theme
      for a sample code that sets a theme image.

      Note that the customization of this image should be done before
      @link(TCastleApplication.OnInitialize Application.OnInitialize) has
      started, so it has to be usually done from the "initialization" section
      of some unit. And in the "initialization" section of a unit,
      you cannot load files (doing @link(LoadImage) at this point may fail on
      some Android devices, as we cannot load assets before activity is started).
      So you can only assign images already available in code ---
      use image-to-pascal tool to convert any image to a Pascal code for this purpose. }
    tiLoading,

    { TCastleEdit frame. }
    tiEdit
  );

  { Theme for user interface controls.

    You can customize @link(Theme) singleton to change the default look.

    You can also create new instances of TCastleTheme and use them for multiple controls,
    assigning to @link(TCastleUserInterface.CustomTheme).

    You can also use @link(FallbackTheme) to effectively disable any customizations
    and always use default hardcoded look. }
  TCastleTheme = class(TCastleComponent)
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

    { Color layer displayed between modal dialogs (CastleMessages, CastleDialogViews, TGLModeFrozenScreen)
      and their background (view underneath or a screenshot).
      Default is (0, 0, 0, 0.25), making things underneath darker,
      which helps dialog to stand out. }
    BackgroundColor: TCastleColor;

    { Color used instead of BackgroundColor when InternalMessageFallbackLook is used.
      You can only customize the RGB values, alpha is always 1, so the resulting
      color is always opaque.
      It's a dark gray color (0.33, 0.33, 0.33) by default. }
    BackgroundOpaqueColor: TCastleColorRGB;

    { Colors used when displaying the loading image (by default just with the "Loading..." text)
      when application is loading (before proper CGE UI can be displayed).
      By default, LoadingBackgroundColor is black, and LoadingColor is white.

      See https://castle-engine.io/loading_image for
      an example how to set loading image. }
    LoadingBackgroundColor, LoadingColor: TCastleColor;

    { Adjust loading image following the UI scaling determined by these properties.
      They determine for what window reference size was the loading image prepared.
      The loading image is whatever you set at @code(ImagesPersistent[tiLoading]).
      See https://castle-engine.io/loading_image for
      an example how to set loading image.

      These properties deliberately match the behavior of
      @link(TCastleContainer.UIScaling),
      @link(TCastleContainer.UIReferenceWidth),
      @link(TCastleContainer.UIReferenceHeight),
      @link(TCastleContainer.UIExplicitScale).
      If these LoadingUIXxx properties are equal to the
      @link(TCastleContainer).UIXxx properties, then the rendered image size will
      be precisely equal to how TCastleImageControl rendering of this image
      (with anchors set to middle) would look.

      The reason why these properties exist is that when rendering
      @code(ImagesPersistent[tiLoading]), TCastleApplication.OnInitialize
      is not yet run, and so @link(TCastleContainer.UIScaling) may be not yet be initialized
      (because you cannot read CastleSettings.xml earlier).

      By default, we set LoadingUIScaling = usEncloseReferenceSize,
      LoadingUIReferenceWidth = 1600,
      LoadingUIReferenceHeight = 900. Note that this is in contrast to
      @link(TCastleContainer).UIXxx properties, that by default set no UI scaling.

      @groupBegin }
    LoadingUIScaling: TUIScaling;
    LoadingUIReferenceWidth: Single;
    LoadingUIReferenceHeight: Single;
    LoadingUIExplicitScale: Single;
    { @groupEnd }

    { Width of scrollbar for TCastleScrollView. }
    ScrollBarWidth: Single;
    { Minumum scrollbar height, to be comfortably visible and draggable. }
    MinimumScrollbarSliderHeight: Single;

    { Request modal boxes to use safe "fallback" rendering options,
      to work even when application is in buggy/dirty state
      because something unexpected happened.

      This is internally set and unset by the CastleWindow exception handler
      (you should not modify this yourself).
      It allows to request "fallback" drawing for
      @link(TCastleWindow.MessageOK) and friends,
      or modal states by @link(TGLModeFrozenScreen).

      This means:

      @unorderedList(
        @item(Using opaque color from @link(BackgroundOpaqueColor) such that
          we don't need to render anything underneath.)

        @item(Use FallbackFont and FallbackTheme, so ignore user customizations
          to font and theme.)

        @item(Avoid making a screenshot (TStateDialog.BackgroundScreenshot may be false),
          which is safer since the application may be in some dirty state, so it's better to avoid
          rendering user stuff.)
      )

      @exclude }
    InternalMessageFallbackLook: boolean;

    DefaultLabelWhite: Boolean deprecated 'if you want the labels to be white, adjust TCastleLabel.Color to White after creating every TCastleLabel';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$ifdef FPC}
    { Theme images, represented as TCastleImage.
      Although they all have sensible defaults, you can also change them
      at any time. Simply create TCastleImage instance (e.g. by LoadImage
      function) and assign it here. Be sure to adjust also @link(OwnsImages)
      if you want the theme to automatically free the image when it's no longer
      used.

      The alpha channel of the image, if any, is automatically correctly used
      (for alpha test or alpha blending, see TDrawableImage). }
    property Images[const ImageType: TThemeImage]: TCastleImage read GetImages write SetImages; deprecated 'use ImagesPersistent[...].Image';
    {$endif}

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

    {$ifdef FPC}
    property OwnsImages[const ImageType: TThemeImage]: boolean read GetOwnsImages write SetOwnsImages; deprecated 'use ImagesPersistent[...].OwnsImage';

    { Corners that determine how image on @link(Images) is stretched when
      drawing by @link(TCastleTheme.Draw) method.
      Together with assigning @link(Images), adjust also this property.
      It is used for images rendered using TDrawableImage.Draw3x3,
      it determines how the image is stretched.
      The corners are specified as 4D vector, order like in CSS: top, right, down,
      left. }
    property Corners[const ImageType: TThemeImage]: TVector4 read GetCorners write SetCorners;
      deprecated 'use ImagesPersistent[...].ProtectedSides';
    {$endif FPC}

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
      const UIScale: Single = 1.0); overload;
    procedure Draw(const Rect: TFloatRectangle; const ImageType: TThemeImage;
      const UIScale: Single; const Color: TCastleColor); overload;
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single = 1.0); overload;
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single; const Color: TCastleColor); overload;

    { Font used by dialogs.
      Leave @nil to use @link(TCastleContainer.DefaultFont). }
    property MessageFont: TCastleAbstractFont read FMessageFont write SetMessageFont;

    property OwnsMessageFont: boolean
      read FOwnsMessageFont write FOwnsMessageFont default true;

    { Set dialogs theme to light. }
    procedure DialogsLight;

    {$ifdef FPC}
    property LoadingTextColor: TCastleColor
      read LoadingColor write LoadingColor;
      deprecated 'use LoadingColor';
    property LoadingImageForWindowHeight: Single
      read LoadingUIReferenceHeight write LoadingUIReferenceHeight;
      deprecated 'use LoadingUIReferenceHeight';
    {$endif}
  end;

{ Theme that you can customize, used by default by all UI controls. }
function Theme: TCastleTheme;

{ Fallback (do not change this, ever) theme. Default state of @link(Theme). }
function FallbackTheme: TCastleTheme;

const
  tiSlider = tiSliderBackground deprecated 'use tiSliderBackground';
  tiSliderPosition = tiSliderThumb deprecated 'use tiSliderThumb';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTheme --------------------------------------------------------------- }

constructor TCastleTheme.Create(AOwner: TComponent);
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
  LoadingColor           := White;
  ScrollBarWidth := 20;
  MinimumScrollbarSliderHeight := 20;
  LoadingUIScaling := usEncloseReferenceSize;
  LoadingUIReferenceWidth := 1600;
  LoadingUIReferenceHeight := 900;

  FOwnsMessageFont := true;

  for T := Low(TThemeImage) to High(TThemeImage) do
  begin
    FImagesPersistent[T] := TCastleImagePersistent.Create;
    { Hide 9-slices visualization in editor, as user likely didn't set the corners of theme,
      and could be surprised by all extra drawing (e.g. on every default button). }
    FImagesPersistent[T].InternalVisualizeProtectedSides := false;
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
  FImagesPersistent[tiSliderBackground].Image := SliderBackground;
  FImagesPersistent[tiSliderBackground].ProtectedSides.AllSides := 31;
  FImagesPersistent[tiSliderBackground].ProtectedSidesScaling := pssMatchHeight;
  FImagesPersistent[tiSliderThumb].Image := SliderThumb;
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
  FImagesPersistent[tiLoading].Image := CastleInternalControlsImages.Loading;
  FImagesPersistent[tiEdit].Image := Edit;
  FImagesPersistent[tiEdit].ProtectedSides.AllSides := 2;
end;

destructor TCastleTheme.Destroy;
var
  T: TThemeImage;
begin
  for T := Low(TThemeImage) to High(TThemeImage) do
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
  FTheme, FFallbackTheme: TCastleTheme;

function Theme: TCastleTheme;
begin
  Result := FTheme;
end;

function FallbackTheme: TCastleTheme;
begin
  Result := FFallbackTheme;
end;

procedure InitializationTheme;
begin
  FTheme := TCastleTheme.Create(nil);
  FFallbackTheme := TCastleTheme.Create(nil);
end;

procedure FinalizationTheme;
begin
  FreeAndNil(FTheme);
  FreeAndNil(FFallbackTheme);
end;

{$endif read_implementation}
