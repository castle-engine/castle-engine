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

  { Possible image placement for a button, see @link(TCastleButton.ImageLayout). }
  TCastleButtonImageLayout = (ilTop, ilBottom, ilLeft, ilRight);

  { Clickable button.

    This is TCastleUserInterface descendant, so to use it just add it to
    the TCastleWindowCustom.Controls or TCastleControlCustom.Controls list.
    You will also usually want to adjust position (TCastleButton.Left,
    TCastleButton.Bottom), TCastleButton.Caption,
    and assign TCastleButton.OnClick (or ovevrride TCastleButton.DoClick). }
  TCastleButton = class(TCastleUserInterfaceFont)
  strict private
    FCalculatedPreferred: Boolean;
    { The only method that can access these is PreferredSize.
      Everything else should use
      RenderRect, EffectiveWidth, EffectiveHeight or other methods.
      This makes sure that FCalculatedPreferred is honored. }
    FCalculatedPreferredWidth, FCalculatedPreferredHeight: Single;
    FOnClick: TNotifyEvent;
    FCaption: string;
    FAutoSize, FAutoSizeWidth, FAutoSizeHeight: boolean;
    TextWidth, TextHeight: Single;
    FPressed: boolean;
    FImage,
      FCustomBackgroundPressed,
      FCustomBackgroundDisabled,
      FCustomBackgroundFocused,
      FCustomBackgroundNormal: TCastleImage;
    FGLImage,
      FGLCustomBackgroundPressed,
      FGLCustomBackgroundDisabled,
      FGLCustomBackgroundFocused,
      FGLCustomBackgroundNormal: TGLImage;
    FOwnsImage,
      FOwnsCustomBackgroundPressed,
      FOwnsCustomBackgroundDisabled,
      FOwnsCustomBackgroundFocused,
      FOwnsCustomBackgroundNormal: boolean;
    FCustomColorPressed,
      FCustomColorDisabled,
      FCustomColorFocused,
      FCustomColorNormal: TCastleColor;
    FCustomBackground: boolean;
    FCustomBackgroundCorners: TVector4Integer;
    FCustomTextColor: TCastleColor;
    FCustomTextColorUse: boolean;
    FToggle: boolean;
    ClickStarted: boolean;
    ClickStartedPosition: TVector2;
    FMinImageWidth: Cardinal;
    FMinImageHeight: Cardinal;
    FImageLayout: TCastleButtonImageLayout;
    FImageAlphaTest: boolean;
    FMinWidth, FMinHeight: Cardinal;
    FImageMargin: Cardinal;
    FPaddingHorizontal, FPaddingVertical: Cardinal;
    FTintPressed, FTintDisabled, FTintFocused, FTintNormal: TCastleColor;
    FEnabled: boolean;
    FEnableParentDragging: boolean;
    FTextAlignment: THorizontalPosition;
    FLineSpacing: Single;
    FHtml: boolean;
    procedure SetCaption(const Value: string);
    procedure SetAutoSize(const Value: boolean);
    procedure SetAutoSizeWidth(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: boolean);
    procedure SetCustomColorDisabled(const AValue: TCastleColor);
    procedure SetCustomColorFocused(const AValue: TCastleColor);
    procedure SetCustomColorNormal(const AValue: TCastleColor);
    procedure SetCustomColorPressed(const AValue: TCastleColor);
    { Recalculate TextWidth, TextHeight, and set FCalculatedPreferred to false. }
    procedure UpdateTextSize;
    procedure SetImage(const Value: TCastleImage);
    procedure SetCustomBackgroundPressed(const Value: TCastleImage);
    procedure SetCustomBackgroundDisabled(const Value: TCastleImage);
    procedure SetCustomBackgroundFocused(const Value: TCastleImage);
    procedure SetCustomBackgroundNormal(const Value: TCastleImage);
    procedure SetImageLayout(const Value: TCastleButtonImageLayout);
    procedure SetMinWidth(const Value: Cardinal);
    procedure SetMinHeight(const Value: Cardinal);
    procedure SetImageMargin(const Value: Cardinal);
    procedure SetEnabled(const Value: boolean);
    procedure SetTextAlignment(const Value: THorizontalPosition);
    procedure SetLineSpacing(const Value: Single);
    procedure SetHtml(const Value: boolean);
    function GetTextToRender: TRichText;
  protected
    procedure SetPressed(const Value: boolean); virtual;
    procedure UIScaleChanged; override;
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    const
      DefaultImageMargin = 10;
      DefaultPaddingHorizontal = 10;
      DefaultPaddingVertical = 10;
      DefaultLineSpacing = 2;
      DefaultTextAlignment = hpMiddle;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure FontChanged; override;
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); override;
    procedure EditorAllowResize(out ResizeWidth, ResizeHeight: Boolean;
      out Reason: String); override;

    { Called when user clicks the button. In this class, simply calls
      OnClick callback. }
    procedure DoClick; virtual;
    procedure SetFocused(const Value: boolean); override;
    { Set this to non-nil to display an image on the button. }
    property Image: TCastleImage read FImage write SetImage;
    { Should we free the @link(Image) when you set another one or at destructor. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default false;

    { Color tint when button is pressed (regardless if enabled or disabled). Opaque white by default. }
    property TintPressed : TCastleColor read FTintPressed  write FTintPressed;
    { Color tint when button is disabled (and not pressed). Opaque white by default. }
    property TintDisabled: TCastleColor read FTintDisabled write FTintDisabled;
    { Color tint when button is focused. Opaque white by default. }
    property TintFocused : TCastleColor read FTintFocused  write FTintFocused;
    { Color tint when button is enabled, but neither pressed nor focused. Opaque white by default. }
    property TintNormal  : TCastleColor read FTintNormal   write FTintNormal;

    { Background image on the pressed button. See @link(CustomBackground) for details. }
    property CustomBackgroundPressed: TCastleImage read FCustomBackgroundPressed write SetCustomBackgroundPressed;
    { Should we free @link(CustomBackgroundPressed) image when you set another one or at destructor. }
    property OwnsCustomBackgroundPressed: boolean read FOwnsCustomBackgroundPressed write FOwnsCustomBackgroundPressed default false;

    { Background image on the disabled button. See @link(CustomBackground) for details. }
    property CustomBackgroundDisabled: TCastleImage read FCustomBackgroundDisabled write SetCustomBackgroundDisabled;
    { Should we free @link(CustomBackgroundDisabled) image when you set another one or at destructor. }
    property OwnsCustomBackgroundDisabled: boolean read FOwnsCustomBackgroundDisabled write FOwnsCustomBackgroundDisabled default false;

    { Background image on the focused button. See @link(CustomBackground) for details. }
    property CustomBackgroundFocused: TCastleImage read FCustomBackgroundFocused write SetCustomBackgroundFocused;
    { Should we free @link(CustomBackgroundFocused) image when you set another one or at destructor. }
    property OwnsCustomBackgroundFocused: boolean read FOwnsCustomBackgroundFocused write FOwnsCustomBackgroundFocused default false;

    { Background image on the normal button. See @link(CustomBackground) for details. }
    property CustomBackgroundNormal: TCastleImage read FCustomBackgroundNormal write SetCustomBackgroundNormal;
    { Should we free @link(CustomBackgroundNormal) image when you set another one or at destructor. }
    property OwnsCustomBackgroundNormal: boolean read FOwnsCustomBackgroundNormal write FOwnsCustomBackgroundNormal default false;

    { Corners used when rendering custom background images.
      See @link(CustomBackground) for details. Zero by default. }
    property CustomBackgroundCorners: TVector4Integer read FCustomBackgroundCorners write FCustomBackgroundCorners;

    { Text color to use if @link(CustomTextColorUse) is @true.
      Black by default, just like @code(Theme.TextColor). }
    property CustomTextColor: TCastleColor read FCustomTextColor write FCustomTextColor;

    { Button colors used when @link(CustomBackground) is @true,
      but CustomBackgroundXxx images (like @link(CustomBackgroundNormal))
      are left unassigned.

      By default they are all transparent black, so actually the button
      is completely transparent in this case.

      They are affected by tint (see @link(TintPressed), @link(TintDisabled),
      @link(TintFocused), @link(TintNormal)). The tint color is simply multiplied
      component-wise with these colors. By default all tint colors are
      opaque white, so the multiplication by tint actually doesn't change anything.
    }
    property CustomColorPressed: TCastleColor read FCustomColorPressed write SetCustomColorPressed;
    property CustomColorDisabled: TCastleColor read FCustomColorDisabled write SetCustomColorDisabled;
    property CustomColorFocused: TCastleColor read FCustomColorFocused write SetCustomColorFocused;
    property CustomColorNormal: TCastleColor read FCustomColorNormal write SetCustomColorNormal;
  published
    { Auto-size routines (see @link(AutoSize)) may treat the image
      like always having at least these minimal sizes.
      Even if the @link(Image) is empty (@nil).
      This is useful when you have a row of buttons (typical for toolbar),
      and you want them to have the same height, and their captions
      to be displayed at the same level, regardless of their images sizes. }
    property MinImageWidth: Cardinal read FMinImageWidth write FMinImageWidth default 0;
    property MinImageHeight: Cardinal read FMinImageHeight write FMinImageHeight default 0;

    { For multi-line @link(Caption), the horizontal alignment of the lines. }
    property TextAlignment: THorizontalPosition
      read FTextAlignment write SetTextAlignment default DefaultTextAlignment;

    { For multi-line @link(Caption), the extra spacing between lines.
      May also be negative to squeeze lines tighter. }
    property LineSpacing: Single read FLineSpacing write SetLineSpacing
      default DefaultLineSpacing;

    { Enable HTML tags in the @link(Caption).
      This allows to easily change colors or use bold, italic text.

      See the example examples/fonts/html_text.lpr and
      examples/fonts/html_text_demo.html for a demo of what HTML tags can do.
      See @link(TCastleFont.PrintStrings) documentation for a list of support HTML markup.

      Note that to see the bold/italic font variants in the HTML markup,
      you need to set the font to be TFontFamily with bold/italic variants.
      See the example mentioned above, examples/fonts/html_text.lpr,
      for a code how to do it. }
    property Html: boolean read FHtml write SetHtml default false;

    { Use custom background images. If @true, we use properties
      @unorderedList(
        @item @link(CustomBackgroundPressed) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item @link(CustomBackgroundDisabled) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item @link(CustomBackgroundFocused) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item(@link(CustomBackgroundNormal) (or fallback on solid color if @nil,
          see @link(CustomColorPressed),
          see @link(CustomColorNormal) and so on).)
      )
      They are rendered as 3x3 images (see TGLImageCore.Draw3x3) with corners
      specified by @link(CustomBackgroundCorners).

      They are affected by tint (see @link(TintPressed), @link(TintDisabled),
      @link(TintFocused), @link(TintNormal)).
    }
    property CustomBackground: boolean read FCustomBackground write FCustomBackground default false;

    { Should we use custom text color in @link(CustomTextColor)
      instead of @code(Theme.TextColor) or @code(Theme.DisabledTextColor). }
    property CustomTextColorUse: boolean read FCustomTextColorUse write FCustomTextColorUse;

    { Horizontal distance between text or @link(Image) and the button border. }
    property PaddingHorizontal: Cardinal
      read FPaddingHorizontal write FPaddingHorizontal default DefaultPaddingHorizontal;

    { Vertical distance between text or @link(Image) and the button border. }
    property PaddingVertical: Cardinal
      read FPaddingVertical write FPaddingVertical default DefaultPaddingVertical;

    { When AutoSize is @true (the default) then button sizes are automatically
      calculated when you change the @link(Caption) and @link(Image).
      The calculated size takes into account the @link(Caption) text size (with current font),
      and @link(Image) size, plus some margin to make it look nice.

      Width is auto-calculated only when AutoSize and AutoSizeWidth
      (otherwise we use @link(Width), @link(WidthFraction) and similar properties).
      Likewise, Height is calculated only when AutoSize and AutoSizeHeight
      (otherwise we use @link(Height), @link(HeightFraction) and similar properties).
      This way you can turn off auto-sizing in only one dimension if you
      want (and when you don't need such flexibility, leave
      AutoSizeWidth = AutoSizeHeight = @true and control both by simple
      AutoSize).

      If needed, you can query the resulting button size with the standard
      TCastleUserInterface methods like @link(TCastleUserInterface.EffectiveWidth) and
      @link(TCastleUserInterface.EffectiveHeight). Note that they may not be available
      before the button is actually added to the container,
      and the container size is initialized (we need to know the size of container,
      for UI scaling to determine the font size). }
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;
    property AutoSizeWidth: boolean read FAutoSizeWidth write SetAutoSizeWidth default true;
    property AutoSizeHeight: boolean read FAutoSizeHeight write SetAutoSizeHeight default true;

    { When auto-size is in effect, these properties may force
      a minimal width/height of the button. This is useful if you want
      to use auto-size (to make sure that the content fits inside),
      but you want to force filling some space. }
    property MinWidth: Cardinal read FMinWidth write SetMinWidth default 0;
    property MinHeight: Cardinal read FMinHeight write SetMinHeight default 0;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    { Caption to display on the button.
      The text may be multiline (use the LineEnding or NL constants to mark newlines). }
    property Caption: string read FCaption write SetCaption;

    { Can the button be permanently pressed. Good for making a button
      behave like a checkbox, that is indicate a boolean state.
      When @link(Toggle) is @true, you can set the @link(Pressed) property,
      and the clicks are visualized a little differently. }
    property Toggle: boolean read FToggle write FToggle default false;

    { Is the button pressed down. If @link(Toggle) is @true,
      you can read and write this property to set the pressed state.

      When not @link(Toggle), this property isn't really useful to you.
      The pressed state is automatically managed then to visualize
      user clicks. In this case, you can read this property,
      but you cannot reliably set it. }
    property Pressed: boolean read FPressed write SetPressed default false;

    { Where the @link(Image) is drawn on a button. }
    property ImageLayout: TCastleButtonImageLayout
      read FImageLayout write SetImageLayout default ilLeft;

    { If the image has alpha channel, should we render with alpha test
      (simple yes/no transparency) or alpha blending (smootly mix
      with background using full transparency). }
    property ImageAlphaTest: boolean
      read FImageAlphaTest write FImageAlphaTest default false;

    { Distance between text and @link(Image). Unused if @link(Image) not set. }
    property ImageMargin: Cardinal read FImageMargin write SetImageMargin
      default DefaultImageMargin;

    property Enabled: boolean read FEnabled write SetEnabled default true;

    { Enable to drag a parent control, for example to drag a TCastleScrollView
      that contains this button. To do this, you need to turn on
      TCastleScrollView.EnableDragging, and set EnableParentDragging=@true
      on all buttons inside. In effect, buttons will cancel the click operation
      once you start dragging, which allows the parent to handle
      all the motion events for dragging. }
    property EnableParentDragging: boolean
      read FEnableParentDragging write FEnableParentDragging default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlebutton_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleButton --------------------------------------------------------------- }

constructor TCastleButton.Create(AOwner: TComponent);
begin
  inherited;

  FAutoSize := true;
  FAutoSizeWidth := true;
  FAutoSizeHeight := true;
  FImageLayout := ilLeft;
  FImageMargin := DefaultImageMargin;
  FPaddingHorizontal := DefaultPaddingHorizontal;
  FPaddingVertical := DefaultPaddingVertical;
  FTintPressed := White;
  FTintDisabled := White;
  FTintFocused := White;
  FTintNormal := White;
  FEnabled := true;
  FLineSpacing := DefaultLineSpacing;
  FTextAlignment := DefaultTextAlignment;
  FCustomTextColor := Black;
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlebutton_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleButton.Destroy;
begin
  if OwnsImage then FreeAndNil(FImage);
  if OwnsCustomBackgroundPressed  then FreeAndNil(FCustomBackgroundPressed);
  if OwnsCustomBackgroundDisabled then FreeAndNil(FCustomBackgroundDisabled);
  if OwnsCustomBackgroundFocused  then FreeAndNil(FCustomBackgroundFocused);
  if OwnsCustomBackgroundNormal   then FreeAndNil(FCustomBackgroundNormal);

  FreeAndNil(FGLImage);
  FreeAndNil(FGLCustomBackgroundPressed);
  FreeAndNil(FGLCustomBackgroundDisabled);
  FreeAndNil(FGLCustomBackgroundFocused);
  FreeAndNil(FGLCustomBackgroundNormal);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlebutton_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleButton.Render;
var
  TextLeft, TextBottom: Single;

  procedure RenderText;
  var
    TextColor: TCastleColor;
    TextX, LineSpacingScaled: Single;
    TextToRender: TRichText;
  begin
    if Enabled then
      TextColor := Theme.TextColor else
      TextColor := Theme.DisabledTextColor;
    if CustomTextColorUse then
      TextColor := CustomTextColor;

    if (not Html) and (CharsPos([#10, #13], Caption) = 0) then
    begin
      { fast case: single line, and no need to use TRichText in this case }
      Font.Print(TextLeft, TextBottom, TextColor, Caption);
    end else
    begin
      { calculate TextX }
      case TextAlignment of
        hpLeft  : TextX := TextLeft;
        hpMiddle: TextX := TextLeft + TextWidth / 2;
        hpRight : TextX := TextLeft + TextWidth;
        else raise EInternalError.Create('TCastleButton.Render: Alignment?');
      end;

      LineSpacingScaled := UIScale * LineSpacing;
      TextToRender := GetTextToRender;
      try
        TextToRender.Print(TextX, TextBottom,
          TextColor, LineSpacingScaled, TextAlignment);
      finally FreeAndNil(TextToRender) end;
    end;
  end;

var
  ImgLeft, ImgBottom, ImgScreenWidth, ImgScreenHeight: Single;
  Background: TThemeImage;
  CustomBackgroundImage: TGLImage;
  SR: TFloatRectangle;
  ImageMarginScaled: Single;
  UseImage, UseText: boolean;
  Tint, SolidColor: TCastleColor;
begin
  inherited;

  ImageMarginScaled := ImageMargin * UIScale;

  SR := RenderRect;

  { calculate Tint }
  if Pressed then
    Tint := TintPressed else
  if not Enabled then
    Tint := TintDisabled else
  if Focused then
    Tint := TintFocused else
    Tint := TintNormal;

  { calculate CustomBackgroundImage }
  CustomBackgroundImage := nil;
  if CustomBackground then
  begin
    if Pressed then
      CustomBackgroundImage := FGLCustomBackgroundPressed
    else
    if not Enabled then
      CustomBackgroundImage := FGLCustomBackgroundDisabled
    else
    if Focused then
      CustomBackgroundImage := FGLCustomBackgroundFocused
    else
      CustomBackgroundImage := FGLCustomBackgroundNormal;
    { instead of CustomBackgroundDisabled/Pressed/Focused, use Normal, if available }
    if CustomBackgroundImage = nil then
      CustomBackgroundImage := FGLCustomBackgroundNormal;
    { render using CustomBackgroundImage, if any }
    if CustomBackgroundImage <> nil then
    begin
      CustomBackgroundImage.Color := Tint;
      CustomBackgroundImage.ScaleCorners := UIScale;
      CustomBackgroundImage.Draw3x3(SR, CustomBackgroundCorners);
    end else
    begin
      if Pressed then
        SolidColor := FCustomColorPressed
      else
      if not Enabled then
        SolidColor := FCustomColorDisabled
     else
      if Focused then
        SolidColor := FCustomColorFocused
      else
        SolidColor := FCustomColorNormal;
      if SolidColor[3] <> 0 then
        DrawRectangle(SR, SolidColor * Tint);
    end;
  end else
  begin
    if Pressed then
      Background := tiButtonPressed
    else
    if not Enabled then
      Background := tiButtonDisabled
    else
    if Focused then
      Background := tiButtonFocused
    else
      Background := tiButtonNormal;
    Theme.Draw(SR, Background, UIScale, Tint);
  end;

  UseImage := (FImage <> nil) and (FGLImage <> nil);
  if UseImage then
  begin
    ImgScreenWidth  := UIScale * FImage.Width;
    ImgScreenHeight := UIScale * FImage.Height;
  end else
  begin
    { Not really necessary,
      but silence FPC 3.3.1 warnings that these are not initialized later. }
    ImgScreenWidth  := 0;
    ImgScreenHeight := 0;
  end;

  UseText := (Length(Caption) > 0);

  if UseText then
  begin
    TextLeft := SR.Left + (SR.Width - TextWidth) / 2;
    if UseImage and (ImageLayout = ilLeft) then
      TextLeft := TextLeft + ((ImgScreenWidth + ImageMarginScaled) / 2) else
    if UseImage and (ImageLayout = ilRight) then
      TextLeft := TextLeft - ((ImgScreenWidth + ImageMarginScaled) / 2);

    TextBottom := SR.Bottom + (SR.Height - TextHeight) / 2;
    if UseImage and (ImageLayout = ilBottom) then
      TextBottom := TextBottom + ((ImgScreenHeight + ImageMarginScaled) / 2) else
    if UseImage and (ImageLayout = ilTop) then
      TextBottom := TextBottom - ((ImgScreenHeight + ImageMarginScaled) / 2);
    TextBottom := TextBottom + Font.Descend;

    RenderText;
  end else
  begin
    { Not really necessary,
      but silence FPC 3.3.1 warnings that these are not initialized later. }
    TextLeft := 0;
    TextBottom := 0;
  end;

  if UseImage then
  begin
    { update FGLImage.Alpha based on ImageAlphaTest }
    if FImage.HasAlpha then
    begin
      if ImageAlphaTest then
        FGLImage.Alpha := acTest else
        FGLImage.Alpha := acBlending;
    end;

    ImgLeft := SR.Left + (SR.Width - ImgScreenWidth) / 2;
    ImgBottom := SR.Bottom + (SR.Height - ImgScreenHeight) / 2;
    if UseText then
    begin
      case ImageLayout of
        ilLeft         : ImgLeft := TextLeft - ImgScreenWidth - ImageMarginScaled;
        ilRight        : ImgLeft := TextLeft + TextWidth + ImageMarginScaled;
      end;
      case ImageLayout of
        ilBottom       : ImgBottom := TextBottom - ImgScreenHeight - ImageMarginScaled;
        ilTop          : ImgBottom := TextBottom + TextHeight + ImageMarginScaled;
      end;
    end;
    FGLImage.Draw(FloatRectangle(ImgLeft, ImgBottom, ImgScreenWidth, ImgScreenHeight));
  end;
end;

procedure TCastleButton.GLContextOpen;
begin
  { TODO: After migrating TGLImageCore -> TGLImage,
    there is no longer a need to create/destroy TGLImage instances
    in GLContextOpen/GLContextClose.
    We can instead create/destroy them in constructor/destructor,
    and simplify this implementation. }

  inherited;
  if (FGLImage = nil) and (FImage <> nil) then
    FGLImage := TGLImage.Create(FImage, true, false);
  if (FGLCustomBackgroundPressed = nil) and (FCustomBackgroundPressed <> nil) then
    FGLCustomBackgroundPressed := TGLImage.Create(FCustomBackgroundPressed, true, false);
  if (FGLCustomBackgroundDisabled = nil) and (FCustomBackgroundDisabled <> nil) then
    FGLCustomBackgroundDisabled := TGLImage.Create(FCustomBackgroundDisabled, true, false);
  if (FGLCustomBackgroundFocused = nil) and (FCustomBackgroundFocused <> nil) then
    FGLCustomBackgroundFocused := TGLImage.Create(FCustomBackgroundFocused, true, false);
  if (FGLCustomBackgroundNormal = nil) and (FCustomBackgroundNormal <> nil) then
    FGLCustomBackgroundNormal := TGLImage.Create(FCustomBackgroundNormal, true, false);
  UpdateTextSize;
end;

procedure TCastleButton.GLContextClose;
begin
  FreeAndNil(FGLImage);
  FreeAndNil(FGLCustomBackgroundPressed);
  FreeAndNil(FGLCustomBackgroundDisabled);
  FreeAndNil(FGLCustomBackgroundFocused);
  FreeAndNil(FGLCustomBackgroundNormal);
  inherited;
end;

function TCastleButton.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  Result := ExclusiveEvents;
  if Enabled then
  begin
    if not Toggle then
    begin
      FPressed := true;
      { We base our Render on Pressed value. }
      VisibleChange([chRender]);
    end;
    // regardless of Toggle value, set ClickStarted, to be able to reach OnClick.
    ClickStarted := true;
    ClickStartedPosition := Event.Position;
  end;
end;

function TCastleButton.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  if ClickStarted then
  begin
    Result := ExclusiveEvents;

    if not Toggle then FPressed := false;
    ClickStarted := false;
    { We base our Render on Pressed value. }
    VisibleChange([chRender]);

    { This is normal behavior of buttons: to click them, you have to make
      mouse down on the button, and then release mouse while still over
      the button.

      We have to check CapturesEventsAtPosition, since (because we keep "focus"
      on this button, if mouse down was on this) we *always* get release event
      (even if mouse position is no longer over this button).

      This is consistent with behaviours of other toolkits.
      It means that if the user does mouse down over the button,
      moves mouse out from the control, then moves it back inside,
      then does mouse up -> it counts as a "click". }
    if Enabled and CapturesEventsAtPosition(Event.Position) then
      DoClick;
  end;
end;

function TCastleButton.Motion(const Event: TInputMotion): boolean;

  { Similar to Release implementation, but never calls DoClick. }
  procedure CancelDragging;
  begin
    if not Toggle then FPressed := false;
    ClickStarted := false;
    { We base our Render on Pressed value. }
    VisibleChange([chRender]);
    { Without ReleaseCapture, the parent (like TCastleScrollView) would still
      not receive the following motion events. }
    Container.ReleaseCapture(Self);
  end;

const
  DistanceToHijackDragging = 20;
begin
  Result := inherited;
  if Result then Exit;

  if ClickStarted and EnableParentDragging and
    (PointsDistanceSqr(ClickStartedPosition, Event.Position) >
     { scaling with UIScale is helpful. Scaling with physical size
       would probably be even better, for mobiles. }
     Sqr(DistanceToHijackDragging * UIScale)) then
    CancelDragging;
end;

procedure TCastleButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TCastleButton.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetAutoSize(const Value: boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetAutoSizeWidth(const Value: boolean);
begin
  if Value <> FAutoSizeWidth then
  begin
    FAutoSizeWidth := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetAutoSizeHeight(const Value: boolean);
begin
  if Value <> FAutoSizeHeight then
  begin
    FAutoSizeHeight := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetCustomColorDisabled(
  const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FCustomColorDisabled, AValue) then
  begin
    FCustomColorDisabled := AValue;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.SetCustomColorFocused(
  const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FCustomColorFocused, AValue) then
  begin
    FCustomColorFocused := AValue;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.SetCustomColorNormal(
  const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FCustomColorNormal, AValue) then
  begin
    FCustomColorNormal := AValue;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.SetCustomColorPressed(
  const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FCustomColorPressed, AValue) then
  begin
    FCustomColorPressed := AValue;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.FontChanged;
begin
  inherited;
  UpdateTextSize;
end;

function TCastleButton.GetTextToRender: TRichText;
begin
  Result := TRichText.Create(Font, Caption, Html);
end;

procedure TCastleButton.UpdateTextSize;
var
  LineSpacingScaled: Single;
  TextToRender: TRichText;
begin
  if Font <> nil then
  begin
    if (not Html) and (CharsPos([#10, #13], Caption) = 0) then
    begin
      { fast case: single line, and no need to use TRichText in this case }
      TextWidth := Font.TextWidth(Caption);
      TextHeight := Font.RowHeight;
    end else
    begin
      LineSpacingScaled := UIScale * LineSpacing;
      TextToRender := GetTextToRender;
      try
        TextWidth := TextToRender.Width;
        TextHeight := TextToRender.Count * (Font.RowHeight + LineSpacingScaled);
      finally FreeAndNil(TextToRender) end;
    end;

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.UIScaleChanged;
begin
  inherited;
  VisibleChange([chRectangle]);
end;

procedure TCastleButton.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FreeAndNil(FGLImage);

    FImage := Value;

    { TODO: After migrating TGLImageCore -> TGLImage,
      this can be simplified. We can create TGLImage regardless if GLInitialized. }
    if GLInitialized and (FImage <> nil) then
      FGLImage := TGLImage.Create(FImage, true, false);

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetCustomBackgroundPressed(const Value: TCastleImage);
begin
  if FCustomBackgroundPressed <> Value then
  begin
    if OwnsCustomBackgroundPressed then FreeAndNil(FCustomBackgroundPressed);
    FreeAndNil(FGLCustomBackgroundPressed);

    FCustomBackgroundPressed := Value;

    { TODO: After migrating TGLImageCore -> TGLImage,
      this can be simplified. We can create TGLImage regardless if GLInitialized. }
    if GLInitialized and (FCustomBackgroundPressed <> nil) then
      FGLCustomBackgroundPressed := TGLImage.Create(FCustomBackgroundPressed, true, false);

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetCustomBackgroundDisabled(const Value: TCastleImage);
begin
  if FCustomBackgroundDisabled <> Value then
  begin
    if OwnsCustomBackgroundDisabled then FreeAndNil(FCustomBackgroundDisabled);
    FreeAndNil(FGLCustomBackgroundDisabled);

    FCustomBackgroundDisabled := Value;

    { TODO: After migrating TGLImageCore -> TGLImage,
      this can be simplified. We can create TGLImage regardless if GLInitialized. }
    if GLInitialized and (FCustomBackgroundDisabled <> nil) then
      FGLCustomBackgroundDisabled := TGLImage.Create(FCustomBackgroundDisabled, true, false);

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetCustomBackgroundFocused(const Value: TCastleImage);
begin
  if FCustomBackgroundFocused <> Value then
  begin
    if OwnsCustomBackgroundFocused then FreeAndNil(FCustomBackgroundFocused);
    FreeAndNil(FGLCustomBackgroundFocused);

    FCustomBackgroundFocused := Value;

    { TODO: After migrating TGLImageCore -> TGLImage,
      this can be simplified. We can create TGLImage regardless if GLInitialized. }
    if GLInitialized and (FCustomBackgroundFocused <> nil) then
      FGLCustomBackgroundFocused := TGLImage.Create(FCustomBackgroundFocused, true, false);

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetCustomBackgroundNormal(const Value: TCastleImage);
begin
  if FCustomBackgroundNormal <> Value then
  begin
    if OwnsCustomBackgroundNormal then FreeAndNil(FCustomBackgroundNormal);
    FreeAndNil(FGLCustomBackgroundNormal);

    FCustomBackgroundNormal := Value;

    { TODO: After migrating TGLImageCore -> TGLImage,
      this can be simplified. We can create TGLImage regardless if GLInitialized. }
    if GLInitialized and (FCustomBackgroundNormal <> nil) then
      FGLCustomBackgroundNormal := TGLImage.Create(FCustomBackgroundNormal, true, false);

    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetFocused(const Value: boolean);
begin
  if Value <> Focused then
  begin
    if not Value then
    begin
      if not Toggle then FPressed := false;
      ClickStarted := false;
    end;

    { We base our Render on Pressed and Focused value. }
    VisibleChange([chRender]);
  end;

  inherited;
end;

procedure TCastleButton.SetTextAlignment(const Value: THorizontalPosition);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.SetLineSpacing(const Value: Single);
begin
  if FLineSpacing <> Value then
  begin
    FLineSpacing := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetHtml(const Value: boolean);
begin
  if FHtml <> Value then
  begin
    FHtml := Value;
    UpdateTextSize;
  end;
end;

procedure TCastleButton.SetPressed(const Value: boolean);
begin
  if FPressed <> Value then
  begin
    { Allow to change Pressed always.
      This is necessary for correct deserialization,
      where Pressed may be deserialized
      befoire Toggle is deserialized. }
    // if not Toggle then
    //   raise Exception.Create('You cannot modify TCastleButton.Pressed value when Toggle is false');
    FPressed := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.SetImageLayout(const Value: TCastleButtonImageLayout);
begin
  if FImageLayout <> Value then
  begin
    FImageLayout := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetMinWidth(const Value: Cardinal);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetMinHeight(const Value: Cardinal);
begin
  if FMinHeight <> Value then
  begin
    FMinHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetImageMargin(const Value: Cardinal);
begin
  if FImageMargin <> Value then
  begin
    FImageMargin := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleButton.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleButton.PreferredSize(var PreferredWidth, PreferredHeight: Single);

  procedure CalculatePreferredSize;
  var
    ImgSize: Single;
    MinWidthScaled, MinHeightScaled,
      ImageMarginScaled,
      PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  begin
    PaddingHorizontalScaled := PaddingHorizontal * UIScale;
    PaddingVerticalScaled   := PaddingVertical   * UIScale;
    ImageMarginScaled       := ImageMargin       * UIScale;
    MinWidthScaled          := MinWidth          * UIScale;
    MinHeightScaled         := MinHeight         * UIScale;

    FCalculatedPreferredWidth  := TextWidth  + PaddingHorizontalScaled * 2;
    FCalculatedPreferredHeight := TextHeight + PaddingVerticalScaled * 2;

    if (FImage <> nil) or
       (MinImageWidth <> 0) or
       (MinImageHeight <> 0) then
    begin
      if FImage <> nil then
        ImgSize := Max(FImage.Width, MinImageWidth) else
        ImgSize := MinImageWidth;
      ImgSize := ImgSize * UIScale;
      case ImageLayout of
        ilLeft, ilRight: FCalculatedPreferredWidth := FCalculatedPreferredWidth + ImgSize + ImageMarginScaled;
        ilTop, ilBottom: FCalculatedPreferredWidth := Max(FCalculatedPreferredWidth, ImgSize + PaddingHorizontalScaled * 2);
      end;

      if FImage <> nil then
        ImgSize := Max(FImage.Height, MinImageHeight) else
        ImgSize := MinImageHeight;
      ImgSize := ImgSize * UIScale;
      case ImageLayout of
        ilLeft, ilRight: FCalculatedPreferredHeight := Max(FCalculatedPreferredHeight, ImgSize + PaddingVerticalScaled * 2);
        ilTop, ilBottom: FCalculatedPreferredHeight := FCalculatedPreferredHeight + ImgSize + ImageMarginScaled;
      end;
    end;

    { at the end apply MinXxx properties }
    MaxVar(FCalculatedPreferredWidth, MinWidthScaled);
    MaxVar(FCalculatedPreferredHeight, MinHeightScaled);
  end;

begin
  inherited;

  if not FCalculatedPreferred then
  begin
    FCalculatedPreferred := true;
    CalculatePreferredSize;
  end;

  if AutoSize and AutoSizeWidth then
    PreferredWidth := FCalculatedPreferredWidth;
  if AutoSize and AutoSizeHeight then
    PreferredHeight := FCalculatedPreferredHeight;
end;

procedure TCastleButton.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSize and AutoSizeWidth then
  begin
    ResizeWidth := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleButton.AutoSize" or "TCastleButton.AutoSizeWidth" to change width.');
  end;
  if AutoSize and AutoSizeHeight then
  begin
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleButton.AutoSize" or "TCastleButton.AutoSizeHeight" to change height.');
  end;
end;

procedure TCastleButton.VisibleChange(const Changes: TCastleUserInterfaceChanges;
  const ChangeInitiatedByChildren: boolean = false);
begin
  inherited;
  // react to SetWidth, SetHeight etc.
  if [chRectangle, chChildren] * Changes <> [] then
    FCalculatedPreferred := false;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlebutton_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
