{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Standard 2D controls: buttons, labels, sliders etc. }
unit CastleControls;

{$I castleconf.inc}

interface

uses Classes, FGL, CastleVectors, CastleUIControls, CastleFonts, CastleTextureFontData,
  CastleKeysMouse, CastleImages, CastleUtils, CastleGLImages, CastleRectangles,
  CastleColors, CastleProgress, CastleTimeUtils, CastleFontFamily, CastleGLUtils,
  CastleURIUtils, CastleLog;

type
  TCastleLabel = class;

  { Base class for all user interface controls using a font.
    Allows to customize font and font size per-control, or use defaults. }
  TUIControlFont = class(TUIControl)
  strict private
    FTooltip: string;
    TooltipLabel: TCastleLabel;
    FCustomFont: TCastleFont;
    FOwnsCustomFont: boolean;
    FLastSeenUIFontXxx: TCastleFont; //< remembered only to call FontChanged
    FFontSize: Single;
    FCustomizedFont: TFontFamily; //< used only when some properties make it necessary
    FSmallFont: boolean;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    procedure SetCustomFont(const Value: TCastleFont);
    procedure SetFontSize(const Value: Single);
    procedure SetSmallFont(const Value: boolean);
    procedure SetOutline(const Value: Cardinal);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineHighQuality(const Value: boolean);
  protected
    { Called when Font or it's size changed. }
    procedure FontChanged; virtual;
    procedure UIScaleChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure GLContextClose; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender; override;
    procedure Render; override;

    { Font used for rendering and measuring this control.

      Depending on various properties
      (@(SmallFont), @link(CustomFont), @link(FontSize), @link(TUIControl.UIScale))
      it may return @link(UIFont), @link(UIFontSmall), or @link(CustomFont),
      or a TCustomizedFont instances that wraps (and scales) one of the above. }
    function Font: TCastleFont;

    { Check does currently used font (see @link(Font)) changed,
      and eventually call FontChanged method @italic(now).

      You only need to explicitly call this in very specific circumstances,
      when you just changed @link(UIFont) or @link(UIFontSmall) (as changing
      @link(CustomFont) automatically
      immediately calls this) and you want control size to be updated
      immediately (for example, you need @link(TCastleButton.Height)
      to be immediately valid). Without calling this,
      it could be updated only at next Render call. }
    procedure CheckFontChanged;

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TUIControl.TooltipExists and
      TUIControl.TooltipStyle and TUIControl.TooltipRender
      to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;

    { When non-nil, this font will be used to draw this control.
      Otherwise the global @link(UIFont) or @link(UIFontSmall)
      (depending on @link(SmallFont)) will be used. }
    property CustomFont: TCastleFont
      read FCustomFont write SetCustomFont;
    property OwnsCustomFont: boolean
      read FOwnsCustomFont write FOwnsCustomFont default false;
      deprecated 'use TCastleFont (inherited from TComponent) owner mechanism';

    { Use given font size when drawing. Leave at default zero to use
      the default size of the font (may still be scaled by @link(TUIControl.UIScale),
      but this happens only if @link(TUIContainer.UIScaling) is set to
      something else than default usNone).
      Font itself is taken from
      @link(CustomFont) or, if not set, from @link(UIFont) or @link(UIFontSmall). }
    property FontSize: Single read FFontSize write SetFontSize default 0.0;

    { Use smaller font.
      If no @link(CustomFont) is assigned, this says to use
      @link(UIFontSmall) instead of @link(UIFont).
      If @link(CustomFont) is assigned, this says to scale it's size by 2. }
    property SmallFont: boolean read FSmallFont write SetSmallFont default false;

    { Outline size around the font.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      See TCastleFont.Outline for more details.

      Specifying a non-zero outline for a label overrides the default outline
      settings for the current font. By default, fonts do not have an outline.

      @seealso OutlineHighQuality }
    property Outline: Cardinal read FOutline write SetOutline default 0;

    { Optionally force better outline quality. Used only if Outline <> 0.
      High quality outline looks better, but is about 2x more expensive to draw.
      @seealso Outline }
    property OutlineHighQuality: boolean
      read FOutlineHighQuality write SetOutlineHighQuality default false;
  end;

  TCastleButtonImageLayout = (ilTop, ilBottom, ilLeft, ilRight);

  { Clickable button.

    This is TUIControl descendant, so to use it just add it to
    the TCastleWindowCustom.Controls or TCastleControlCustom.Controls list.
    You will also usually want to adjust position (TCastleButton.Left,
    TCastleButton.Bottom), TCastleButton.Caption,
    and assign TCastleButton.OnClick (or ovevrride TCastleButton.DoClick). }
  TCastleButton = class(TUIControlFont)
  strict private
    FWidth, FHeight: Cardinal;
    FFinalScaledValid: boolean;
    { The only method that can access these is Rect.
      Everything else should use Rect, ScreenRect, CalculatedWidth, CalculatedHeight
      or other methods that wrap Rect.
      This makes sure that FFinalScaledValid is honored. }
    FFinalScaledWidth, FFinalScaledHeight: Cardinal;
    FOnClick: TNotifyEvent;
    FCaption: string;
    FAutoSize, FAutoSizeWidth, FAutoSizeHeight: boolean;
    TextWidth, TextHeight: Cardinal;
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
      FGLCustomBackgroundNormal: TGLImageCore;
    FOwnsImage,
      FOwnsCustomBackgroundPressed,
      FOwnsCustomBackgroundDisabled,
      FOwnsCustomBackgroundFocused,
      FOwnsCustomBackgroundNormal: boolean;
    FCustomBackground: boolean;
    FCustomBackgroundCorners: TVector4Integer;
    FCustomTextColor: TCastleColor;
    FCustomTextColorUse: boolean;
    FToggle: boolean;
    ClickStarted: boolean;
    ClickStartedPosition: TVector2Single;
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
    FLineSpacing: Integer;
    FHtml: boolean;
    procedure SetCaption(const Value: string);
    procedure SetAutoSize(const Value: boolean);
    procedure SetAutoSizeWidth(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: boolean);
    { Recalculate TextWidth, TextHeight, and set FFinalScaledValid to false. }
    procedure UpdateTextSize;
    procedure SetImage(const Value: TCastleImage);
    procedure SetCustomBackgroundPressed(const Value: TCastleImage);
    procedure SetCustomBackgroundDisabled(const Value: TCastleImage);
    procedure SetCustomBackgroundFocused(const Value: TCastleImage);
    procedure SetCustomBackgroundNormal(const Value: TCastleImage);
    procedure SetImageLayout(const Value: TCastleButtonImageLayout);
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetMinWidth(const Value: Cardinal);
    procedure SetMinHeight(const Value: Cardinal);
    procedure SetImageMargin(const Value: Cardinal);
    procedure SetEnabled(const Value: boolean);
    procedure SetTextAlignment(const Value: THorizontalPosition);
    procedure SetLineSpacing(const Value: Integer);
    procedure SetHtml(const Value: boolean);
    function GetTextToRender: TRichText;
  protected
    procedure FontChanged; override;
    procedure SetPressed(const Value: boolean); virtual;
    procedure UIScaleChanged; override;
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
    function Rect: TRectangle; override;

    { Called when user clicks the button. In this class, simply calls
      OnClick callback. }
    procedure DoClick; virtual;
    procedure SetFocused(const Value: boolean); override;
    { Set this to non-nil to display an image on the button. }
    property Image: TCastleImage read FImage write SetImage;
    { Should we free the @link(Image) when you set another one or at destructor. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default false;

    { Auto-size routines (see @link(AutoSize)) may treat the image
      like always having at least these minimal sizes.
      Even if the @link(Image) is empty (@nil).
      This is useful when you have a row of buttons (typical for toolbar),
      and you want them to have the same height, and their captions
      to be displayed at the same level, regardless of their images sizes. }
    property MinImageWidth: Cardinal read FMinImageWidth write FMinImageWidth default 0;
    property MinImageHeight: Cardinal read FMinImageHeight write FMinImageHeight default 0;

    { Color tint when button is pressed (regardless if enabled or disabled). Opaque white by default. }
    property TintPressed : TCastleColor read FTintPressed  write FTintPressed;
    { Color tint when button is disabled (and not pressed). Opaque white by default. }
    property TintDisabled: TCastleColor read FTintDisabled write FTintDisabled;
    { Color tint when button is focused. Opaque white by default. }
    property TintFocused : TCastleColor read FTintFocused  write FTintFocused;
    { Color tint when button is enabled, but neither pressed nor focused. Opaque white by default. }
    property TintNormal  : TCastleColor read FTintNormal   write FTintNormal;

    { Use custom background images. If @true, we use properties
      @unorderedList(
        @item @link(CustomBackgroundPressed) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item @link(CustomBackgroundDisabled) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item @link(CustomBackgroundFocused) (or fallback on @link(CustomBackgroundNormal) if @nil),
        @item @link(CustomBackgroundNormal) (or fallback on transparent background if @nil).
      ).
      They are rendered as 3x3 images (see TGLImageCore.Draw3x3) with corners
      specified by @link(CustomBackgroundCorners). }
    property CustomBackground: boolean read FCustomBackground write FCustomBackground default false;

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

    { Should we use custom text color in @link(CustomTextColor)
      instead of @code(Theme.TextColor) or @code(Theme.DisabledTextColor). }
    property CustomTextColorUse: boolean read FCustomTextColorUse write FCustomTextColorUse;
    { Text color to use if @link(CustomTextColorUse) is @true.
      Black by default, just like @code(Theme.TextColor). }
    property CustomTextColor: TCastleColor read FCustomTextColor write FCustomTextColor;

    { For multi-line @link(Caption), the horizontal alignment of the lines. }
    property TextAlignment: THorizontalPosition
      read FTextAlignment write SetTextAlignment default DefaultTextAlignment;

    { For multi-line @link(Caption), the extra spacing between lines.
      May also be negative to squeeze lines tighter. }
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing
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
  published
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;

    { Horizontal distance between text or @link(Image) and the button border. }
    property PaddingHorizontal: Cardinal
      read FPaddingHorizontal write FPaddingHorizontal default DefaultPaddingHorizontal;

    { Vertical distance between text or @link(Image) and the button border. }
    property PaddingVertical: Cardinal
      read FPaddingVertical write FPaddingVertical default DefaultPaddingVertical;

    { When AutoSize is @true (the default) then button sizes are automatically
      adjusted when you change the Caption and @link(Image).
      The calculated size takes into account the Caption text size (with current font),
      and @link(Image) size, plus some margin to make it look nice.

      @link(Width) is adjusted only when AutoSize and AutoSizeWidth.
      And @link(Height) is adjusted only when AutoSize and AutoSizeHeight.
      This way you can turn off auto-sizing in only one dimension if you
      want (and when you don't need such flexibility, leave
      AutoSizeWidth = AutoSizeHeight = @true and control both by simple
      AutoSize).

      If needed, you can query the resulting button size with the standard
      TUIControl methods like @link(TUIControl.CalculatedWidth) and
      @link(TUIControl.CalculatedHeight). Note that they may not be available
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
  end;

  { Panel or a toolbar control.
    Use as a background for other controls like buttons.
    You can add vertical separators to separate groups of controls on the panel. }
  TCastlePanel = class(TUIControlSizeable)
  strict private
    FVerticalSeparators: TCardinalList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    { Separator lines drawn on panel. Useful if you want to visually separate
      groups of contols (like a groups of buttons when you use
      this panel as a toolbar).

      Values are the horizontal positions of the separators (with respect
      to this panel @link(Left)). Width of the separator is in SeparatorSize. }
    property VerticalSeparators: TCardinalList read FVerticalSeparators;
    class function SeparatorSize: Cardinal;
  end;

  { Image control.
    Size is automatically adjusted to the image size, if Stretch is @false (default).
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.URL property
    or straight TCastleImageControl.Image.

    We automatically use alpha test or alpha blending based
    on loaded image alpha channel (see TGLImageCore.Alpha).
    You can influence this by @link(AlphaChannel) property. }
  TCastleImageControl = class(TUIControl)
  strict private
    FURL: string;
    FImage: TCastleImage;
    FGLImage: TGLImageCore;
    FAlphaChannel: TAutoAlphaChannel;
    FStretch: boolean;
    FProportional: boolean;
    FFullSize: boolean;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FColor: TCastleColor;
    FCorners: TVector4Integer;
    FOwnsImage: boolean;
    FSmoothScaling: boolean;
    FCenterX: Single;
    FCenterY: Single;
    FRotation: Single;
    FClip: boolean;
    FClipLine: TVector3Single;
    procedure SetCenterX(const AValue: Single);
    procedure SetCenterY(const AValue: Single);
    procedure SetRotation(const AValue: Single);
    procedure SetURL(const Value: string);
    procedure SetImage(const Value: TCastleImage);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
    procedure SetStretch(const Value: boolean);
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetFullSize(const Value: boolean);
    procedure SetProportional(const Value: boolean);
    procedure SetColor(const Value: TCastleColor);
    procedure SetSmoothScaling(const Value: boolean);
    procedure SetClip(const Value: boolean);
    procedure SetClipLine(const Value: TVector3Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Rect: TRectangle; override;
    procedure ImageChanged;

    { Image displayed, or @nil if none. You can set it by setting @link(URL),
      or you can set this property directly if you loaded/created the image contents
      yourself.

      Note that by default the TCastleImage instance assigned here is owned
      by this component (see @link(OwnsImage)).
      So if you set this property to your custom TCastleImage instance you should
      leave memory management of this instance to this component.
      You can either create a copy by TCastleImage.MakeCopy
      if you want to give here only a copy, or you can change @link(OwnsImage)
      to @false.

      It is allowed to modify the contents or even size of this image.
      Just make sure to call ImageChanged after the modifications are done
      to update the actual rendered image.
      The control size will be updated immediately (taking into account current
      @link(Stretch) and related properties values). }
    property Image: TCastleImage read FImage write SetImage;

    { Whether the memory management of assigned @link(Image) is automatic.
      See @link(Image) documentation for details. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default true;

    { Color tint of the image. This simply multiplies the image RGBA components,
      just like @link(TGLImageCore.Color). By default this is opaque white,
      which means that image colors are unchanged. }
    property Color: TCastleColor read FColor write SetColor;

    { Corners of the image that are not stretched even
      in case @link(Stretch) is used.
      See @link(TGLImageCore.Draw3x3) for the details how drawing image
      with borders work. }
    property Corners: TVector4Integer read FCorners write FCorners;

    { X coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterX: Single read FCenterX write SetCenterX default 0.5;

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read FCenterY write SetCenterY default 0.5;

    { Rotation in radians. Default value 0. }
    property Rotation: Single read FRotation write SetRotation default 0;

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine). }
    property Clip: boolean read FClip write SetClip;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The equation is calculated in the final scaled screen coordinates
      (not in the local, unscaled pixels). Adjust it to match the ScreenRect
      if necessary, to make it work with UI scaling. }
    property ClipLine: TVector3Single read FClipLine write SetClipLine;
  published
    { URL of the image. Setting this also sets @link(Image).
      Set this to '' to clear the image. }
    property URL: string read FURL write SetURL;
    { Deprecated name for @link(URL). }
    property FileName: string read FURL write SetURL; deprecated;
    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this to force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel
      read FAlphaChannel write SetAlphaChannel default acAuto;
    { Deprecated, use more flexible AlphaChannel instead. }
    property Blending: boolean read GetBlending write SetBlending stored false; deprecated 'use AlphaChannel';

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      See @link(TGLImageCore.SmoothScaling). }
    property SmoothScaling: boolean
      read FSmoothScaling write SetSmoothScaling default true;

    { Size of the image control.

      If Stretch = @false, then values you set for Width, Height, FullSize,
      Proportional properties do not matter (they are still remembered though,
      so you can set properties in any order).
      The displayed size (you can check it through @link(Rect) function)
      always corresponds to the underlying image size.
      The Left and Bottom properties work as usual, they allow you to move the control.

      If Stretch = @true, then the image will be stretched to fill the requested area.
      @unorderedList(
        @item(If Stretch = @true and FullSize = @true then values of Width,
          Height, Left, Bottom do not matter:
          image always fills the whole parent
          (@link(Rect) corresponds to the parent area).)

        @item(Otherwise, if Stretch = @true and Proportional = @true,
          then the image will be proportionally scaled to fit within
          the requested Width and Height. If the aspect ratio of image
          will be different than aspect ratio of Width/Height, the scaled image
          will be centered inside the Width/Height.)

        @item(Otherwise, if Stretch = @true but no other condition
          (so FullSize = @false and Proportional = @false)
          then the image will be scaled to exactly fill
          the requested Width and Height
          (without paying attention to the aspect ratio of the image).

          This is the case when you fully force the displayed size
          and position, regardless of image size. Displayed image will
          always exactly fill the requested area.
        )
      )

      Note that you can always look at @link(Rect) value to know
      the current calculated size and position of the image control on screen.

      @groupBegin }
    property Stretch: boolean read FStretch write SetStretch default false;
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;
    property FullSize: boolean read FFullSize write SetFullSize default false;
    property Proportional: boolean read FProportional write SetProportional default false;
    { @groupEnd }
  end;

  TCastleTouchCtlMode = (ctcmWalking, ctcmWalkWithSideRot, ctcmHeadRotation,
                         ctcmFlyUpdown, ctcmPanXY);
  TCastleTouchPosition = (tpManual, tpLeft, tpRight);

  { Touch user interface to navigate in a 3D world.
    Shows one "lever", that can be moved up/down/left/right,
    and controls the movement while Walking or Flying.

    Usually, this control is created and used through the @link(TCastleWindowTouch)
    properties. There is no need to directly create TCastleTouchControl instance
    in this case. }
  TCastleTouchControl = class(TUIControl)
  strict private
    FTouchMode: TCastleTouchCtlMode;
    FLeverOffset: TVector2Single;
    FDragging: Integer; //< finger index that started drag, -1 if none
    FPosition: TCastleTouchPosition;
    FScale: Single;
    function TotalScale: Single;
    procedure SetPosition(const Value: TCastleTouchPosition);
    procedure SetScale(const Value: Single);
    function MaxOffsetDist: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;

    { Size of this control, ignoring GetExists. }
    function Rect: TRectangle; override;

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure SetTouchMode(const Value: TCastleTouchCtlMode);
    procedure GetSensorRotation(var X, Y, Z, Angle: Double);
    procedure GetSensorTranslation(var X, Y, Z, Length: Double);
  published
    property TouchMode: TCastleTouchCtlMode
      read FTouchMode write SetTouchMode default ctcmWalking;

    { Set position of touch control. Right now this simply sets
      the anchor using @link(TUIControl.HasHorizontalAnchor) and friends.
      Tip: Use @link(TUIContainer.UIScaling) to have the anchors automatically
      scale with screen size.

      The size of the control is set to be constant physical size,
      so it's not affected by @link(TUIContainer.UIScaling), only by
      @link(TUIContainer.Dpi). }
    property Position: TCastleTouchPosition
      read FPosition write SetPosition default tpManual;

    property Scale: Single read FScale write SetScale default 1;
  end;

  { Fill a rectangle on screen with given color. }
  TCastleRectangleControl = class(TUIControlSizeable)
  strict private
    FColor: TCastleColor;
    FInterceptInput: boolean;
    procedure SetColor(const Value: TCastleColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    { Rectangle color. By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    { Prevents passing mouse/keyboard events to the controls underneath.
      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" by this control.  }
    property InterceptInput: boolean read FInterceptInput write FInterceptInput
      default false;
  end;

  TShapeType = (stRectangle, stCircle);

  { Draw a simple shape (rectangle, circle) with given color and optional outline. }
  TCastleShape = class(TUIControlSizeable)
  strict private
    FFilled, FOutline, FOutlineThick: boolean;
    FColor, FOutlineColor: TCastleColor;
    FOutlineWidth: Single;
    FShapeType: TShapeType;
    procedure SetShapeType(const Value: TShapeType);
    procedure SetFilled(const Value: boolean);
    procedure SetColor(const Value: TCastleColor);
    procedure SetOutline(const Value: boolean);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineWidth(const Value: Single);
    procedure SetOutlineThick(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;

    property ShapeType: TShapeType read FShapeType write SetShapeType default stRectangle;

    property Filled: boolean read FFilled write SetFilled default true;

    { The fill color, used if @link(Filled). By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    property Outline: boolean read FOutline write SetOutline default false;

    { The outline color, used if @link(Outline). By default, opaque black. }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;

    { Determines the drawing method of the outline, used if @link(Outline).

      @definitionList(
        @itemLabel(@false (default))
        @item(Draw the outline using lines, and apply OutlineWidth by changing
          line width.

          Disadvantage:
          @bold(outline widths thicker than 1 pixel are not guaranteed
          to be supported. In particular they will almost never work on mobile
          (OpenGLES).)
          See the LineWidth comments at DrawPrimitive2D procedure.

          Consider using other methods if you need to draw a thick shape outline
          in a reliable manner. To draw a rectangle with frame, it's usually better
          to use Draw3x3 call, with a special texture with a frame and insides.)

        @itemLabel(@true)
        @item(Draw the outline by first drawing a larger shape with OutlineColor
          underneath the smaller shape with Color.

          Disadvantages:
          @unorderedList(
            @item(Cannot work sensibly if @link(Filled) is @false,
              so it's disabled then. When @link(Filled) is @false,
              it's like OutlineThick was always also @false.)
            @item(The alpha blending may not be exactly what you want,
              since the pixels inside are overdrawn with both OutlineColor
              and then with Color.)
            @item(May look a little worse in case of small OutlineWidth
              and non-rectangular shapes.)
          )

          Advantage: thick OutlineWidth works reliably.)
      )
    }
    property OutlineThick: boolean read FOutlineThick write SetOutlineThick;

    { The outline width, used if @link(Outline).
      It is affected by UI scaling.

      If OutlineThick is @false, then
      @bold(outline widths thicker than 1 pixel are not guaranteed
      to be supported. In particular they will almost never work on mobile (OpenGLES).)
      Change OutlineThick to @true to have reliable thick outlines. }
    property OutlineWidth: Single read FOutlineWidth write SetOutlineWidth default 1.0;
  end;

  { Fill the whole window with a simple color.
    This is very fast, but it unconditionally clears the whole window,
    and there is no blending (if your @link(Color) has some alpha, it is
    just copied to the color buffer). To clear the rectangle with a color,
    with optional blending, use @link(TCastleRectangleControl) instead.

    Note that @link(TCastleSceneManager) clears the background under itself
    by default. See TCastleSceneManager.Transparent,
    TCastleSceneManager.BackgroundColor .
    So if you use @link(TCastleSceneManager) that fills the whole screen,
    then there's no need to use this control. }
  TCastleSimpleBackground = class(TUIControl)
  strict private
    FColor: TCastleColor;
    procedure SetColor(const Value: TCastleColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    { Background color. By default, this is black color with opaque alpha. }
    property Color: TCastleColor read FColor write SetColor;
  end;

  TCastleScrollView = class;

  { Dialog box that can display a long text, with automatic vertical scrollbar.
    You can also add buttons at the bottom.
    You can also have an input text area.
    This can be used to make either a modal or non-modal dialog boxes.

    See CastleMessages for routines that intensively use this dialog underneath,
    giving you easy MessageXxx routines that ask user for confirmation and such. }
  TCastleDialog = class abstract(TUIControlFont)
  strict private
    const
      BoxMargin = 10;
      WindowMargin = 10;
      ButtonHorizontalMargin = 10;
    type
      TDialogScrollArea = class(TUIControlSizeable)
      strict private
        Dialog: TCastleDialog;
      public
        constructor Create(AOwner: TComponent); override;
        procedure Render; override;
      end;
    var
    FInputText: string;

    { Broken Text. }
    Broken_Text: TRichText;
    { Ignored (not visible) if not DrawInputText.
      Else broken InputText. }
    Broken_InputText: TStringList;

    MaxLineWidth: integer;
    { Sum of all Broken_Text.Count + Broken_InputText.Count.
      In other words, all lines that are scrolled by the scrollbar. }
    AllScrolledLinesCount: integer;

    { Things below set in MessageCore, readonly afterwards. }
    { Main text to display. Read-only contents. }
    Text: TStringList;
    { Drawn as window background. @nil means there is no background
      (use only if there is always some other 2D control underneath TCastleDialog).
      When assigned, stretched to cover whole screen. }
    GLBackground: TGLImageCore;
    Background: TCastleImage;
    TextAlign: THorizontalPosition;
    { Should we display InputText }
    DrawInputText: boolean;
    Buttons: array of TCastleButton;
    LifeTime: TFloatTime;
    FHtml: boolean;
    ScrollView: TCastleScrollView;

    function BoxMarginScaled: Integer;
    function WindowMarginScaled: Integer;
    function ButtonHorizontalMarginScaled: Integer;

    procedure SetInputText(const value: string);

    { Calculate height in pixels needed to draw Buttons.
      Returns 0 if there are no Buttons. }
    function ButtonsHeightScaled: Integer;
    function ButtonsHeight: Integer;
    procedure UpdateSizes;
  public
    { Set this to @true to signal that modal dialog window should be closed.
      This is not magically handled --- if you implement a modal dialog box,
      you should check in your loop whether something set Answered to @true. }
    Answered: boolean;

    { The whole rectangle where we draw dialog box. }
    function Rect: TRectangle; override;

    { Input text. Displayed only if DrawInputText. }
    property InputText: string read FInputText write SetInputText;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign display stuff. Call this before adding control to Controls list.
      ABackground instance becomes owned by this component. }
    procedure Initialize(
      const TextList: TStringList; const ATextAlign: THorizontalPosition;
      const AHtml: boolean;
      const AButtons: array of TCastleButton;
      const ADrawInputText: boolean; const AInputText: string;
      const ABackground: TCastleImage);
    procedure Resize; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; override;
  end;

  TThemeImage = (
    tiPanel, tiPanelSeparator, tiProgressBar, tiProgressFill,
    tiButtonPressed, tiButtonDisabled, tiButtonFocused, tiButtonNormal,
    tiWindow, tiScrollbarFrame, tiScrollbarSlider,
    tiSlider, tiSliderPosition, tiLabel, tiActiveFrame, tiTooltip,
    tiTouchCtlInner, tiTouchCtlOuter, tiTouchCtlFlyInner, tiTouchCtlFlyOuter,
    tiCrosshair1, tiCrosshair2, tiErrorBackground,

    { Image displayed when the application is initializing,
      during @link(TCastleApplication.OnInitialize Application.OnInitialize)
      and @link(TCastleWindowCustom.OnOpen Window.OnOpen).
      And @link(TUIControl.GLContextOpen) for all initially present UI controls.
      This "loading image" is loaded and displayed first,
      so that user does not see a black screen while the resources are prepared.

      It is especially useful on Android, where we can lose the OpenGL context
      at any moment, as user may switch applications in the middle of the game.
      When getting back to the application, we need to initiailize some
      resources, and during this process we also show this image.
      So this serves as a universal "please wait, we're loading" screen.

      You can customize this image, by setting
      @link(TCastleTheme.Images Theme.Images[tiLoading]),
      @link(TCastleTheme.LoadingBackgroundColor LoadingBackgroundColor),
      @link(TCastleTheme.LoadingTextColor LoadingTextColor).
      See http://castle-engine.sourceforge.net/tutorial_player_2d_controls.php
      for a sample code that sets a theme image.

      Note that the customization of this image should be done before
      @link(TCastleApplication.OnInitialize Application.OnInitialize) has
      started, so it has to be usually done from the "initialization" section
      of some unit. And in the "initialization" section of a unit,
      you cannot load files (doing @link(LoadImage) at this point may fail on
      some Android devices, as we cannot load assets before activity is started).
      So you can only assign images already available in code ---
      use image-to-pascal tool to convert any image to a Pascal code for this purpose. }
    tiLoading);

  { Label with possibly multiline text, in an optional box. }
  TCastleLabel = class(TUIControlFont)
  strict private
    FText: TStrings;
    FPaddingHorizontal, FPaddingVertical, FPadding: Integer;
    FLineSpacing: Integer;
    FColor: TCastleColor;
    FHtml: boolean;
    FFrame: boolean;
    FFrameColor: TCastleColor;
    FMaxWidth: Integer;
    FAlignment: THorizontalPosition;
    FVerticalAlignment: TVerticalPosition;
    FAutoSize: boolean;
    FWidth, FHeight: Cardinal;
    FFullSize: boolean;
    FMaxDisplayChars: Integer;
    function GetTextToRender: TRichText;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetFullSize(const Value: boolean);
    procedure SetAutoSize(const Value: boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: THorizontalPosition);
    procedure SetVerticalAlignment(const Value: TVerticalPosition);
    procedure SetMaxDisplayChars(const Value: Integer);
  private
    { For internal use by tooltip rendering. In normal circumstances,
      leave this at tiLabel. }
    ImageType: TThemeImage;
  public
    const
      DefaultLineSpacing = 2;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Rect: TRectangle; override;

    { Text color. By default it's white. }
    property Color: TCastleColor read FColor write FColor;

    { Color tint of the background image, see @link(Frame). By default white. }
    property FrameColor: TCastleColor read FFrameColor write FFrameColor;

    function DisplayChars: Cardinal;
  published
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;
    property FullSize: boolean read FFullSize write SetFullSize default false;

    { Should we automatically adjust size to the text size.
      The size of the label determines where does it display the @link(Frame),
      where does it catch events, to what width is it aligned (see @link(Alignment))
      and so on.

      When this is @true (the default) then
      @link(Width), @link(Height), @link(FullSize) values are ignored. }
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;

    { Caption displayed on the label, each line as a string. }
    property Text: TStrings read FText;

    { Caption displayed on the label.
      This is just a shortcut to get/set @link(Text) as a single string.

      Use LineEnding or NL constant when setting this to indicate a newline.
      These two examples equivalent. First example:

      @longCode(#
        Label1.Text.Clear;
        Label1.Text.Add('First line');
        Label1.Text.Add('Second line');
      #)

      Second (equivalent) example:

      @longCode(#
        Label1.Caption := 'First line' + LineEnding + 'Second line';
      #)
    }
    property Caption: string read GetCaption write SetCaption stored false;

    { Inside the label rectangle, padding between rect borders and text.
      Total horizontal padding is the sum @code(PaddingHorizontal + Padding),
      total verical padding is the sum @code(PaddingVertical + Padding).
      @groupBegin }
    property PaddingHorizontal: Integer
      read FPaddingHorizontal write FPaddingHorizontal default 0;
    property PaddingVertical: Integer
      read FPaddingVertical write FPaddingVertical default 0;
    property Padding: Integer
      read FPadding write FPadding default 0;
    { @groupEnd }

    { Extra spacing between lines.
      May be negative to squeeze lines tighter together. }
    property LineSpacing: Integer read FLineSpacing write FLineSpacing default DefaultLineSpacing;

    { Does the text use HTML markup.
      This allows to easily change colors or use bold, italic text.

      See the example examples/fonts/html_text.lpr and
      examples/fonts/html_text_demo.html for a demo of what HTML tags can do.
      See @link(TCastleFont.PrintStrings) documentation for a list of support HTML markup.

      Note that to see the bold/italic font variants in the HTML markup,
      you need to set the font to be TFontFamily with bold/italic variants.
      See the example mentioned above, examples/fonts/html_text.lpr,
      for a code how to do it. }
    property Html: boolean read FHtml write FHtml default false;

    property Tags: boolean read FHtml write FHtml stored false default false;
      deprecated 'use Html instead';

    { Draw frame around the text. Frame uses theme image tiLabel,
      see TCastleTheme.Images if you want to customize it. }
    property Frame: boolean read FFrame write FFrame default false;

    { If non-zero, limit the width of resulting label.
      The text will be broken in the middle of lines, to make it fit
      (together with @link(PaddingHorizontal)) inside MaxWidth. }
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write SetAlignment default hpLeft;

    { Vertical alignment of the text. Usually you don't want to use this,
      instead leave @link(AutoSize) = @true and align the label to the parent
      using anchors, like @code(MyLabel.Anchor(vpMiddle);)
      or @code(MyLabel.Anchor(vpTop);).

      This property is useful if you really need to manually control the size.
      It only matters when @link(AutoSize) is @false.
      Then it controls where the text is, with respect to it's rectangle defined
      by properties like @link(Height) or @link(FullSize). }
    property VerticalAlignment: TVerticalPosition
      read FVerticalAlignment write SetVerticalAlignment default vpBottom;

    { Limit the displayed label text, if not -1.
      This doesn't affect the label size, only the rendered text.
      It's nice to show the animation of text "expanding", filling some area.
      Use DisplayChars as the maximum sensible value for this. }
    property MaxDisplayChars: Integer
      read FMaxDisplayChars write SetMaxDisplayChars default -1;
  end;

  TCastleCrosshairShape = (csCross, csCrossRect);

  { Display a simple crosshair in the middle of the parent control. }
  TCastleCrosshair = class(TUIControl)
  strict private
    FShape: TCastleCrosshairShape;
    procedure SetShape(const Value: TCastleCrosshairShape);
    function ImageType: TThemeImage;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;

    { Size of this control, ignoring GetExists. }
    function Rect: TRectangle; override;
  published
    property Shape: TCastleCrosshairShape read FShape write SetShape default csCross;

    { By default, crosshair is centered. }
    property HasHorizontalAnchor default true;
    property HasVerticalAnchor default true;
    property HorizontalAnchorSelf default hpMiddle;
    property HorizontalAnchorParent default hpMiddle;
    property VerticalAnchorSelf default vpMiddle;
    property VerticalAnchorParent default vpMiddle;
  end;

  { Progress bar user interface.
    This is usually used through the CastleWindowProgress and CastleProgress
    features. There is no need to directly create and access the TCastleProgressBar
    instance in this case. }
  TCastleProgressBar = class(TUIControlFont)
  strict private
    { Background image. }
    FBackground: TCastleImage;
    FGLBackground: TGLImageCore;
    FYPosition: Single;
    FProgress: TProgress;
    procedure SetBackground(const Value: TCastleImage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Rect: TRectangle; override;

    { Progress that rules the position and title displayed. }
    property Progress: TProgress read FProgress write FProgress;

    { Background drawn under the progress bar.
      May be left unassigned (@nil), in which case you're responsible for making
      sure some other control (like TCastleSimpleBackground or maybe 3D viewport)
      always covers the screen underneath.

      When it's assigned, it's always drawn scaled to cover the whole
      screen (container). It's owner by this component (it will be automatically
      freed when necessary). }
    property Background: TCastleImage read FBackground write SetBackground;

    { Vertical positon of the progress bar.
      0 means the middle of progress bar is at the bottom of the container,
      1 means at the top. 0.5 indicates the middle, and it's the default.

      Note that extreme values (0 or 1) mean that bottom or top half
      of the progress bar doesn't fit on the screen, as this property
      positions the middle of the progress bar. }
    property YPosition: Single read FYPosition write FYPosition
      default TProgressUserInterface.DefaultBarYPosition;
  end;

  { Error background, for various error handlers.
    This is notably used by the CastleWindow exception handler. }
  TErrorBackground = class(TUIControl)
    procedure Render; override;
  end;

  { An abstract slider user interface. Usually you want to use of its descendants,
    like @link(TCastleFloatSlider) or @link(TCastleIntegerSlider). }
  TCastleAbstractSlider = class(TUIControlFont)
  strict private
    FDisplayValue: boolean;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOnChange: TNotifyEvent;
    FCaption: string;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    function IndicatorWidth(const R: TRectangle): Integer;
    procedure SetCaption(const Value: string);
  private
    { Draw a slider at given Position. If Position is outside 0..1, it is clamped
      to 0..1 (this way we do not show slider at some wild position if it's
      outside the expected range; but DrawSliderText will still show the true,
      unclamped, value). }
    procedure DrawSliderPosition(const R: TRectangle; const Position: Single);

    { Returns a value of Position, always in 0..1 range,
      that would result in slider being drawn at XCoord screen position
      by DrawSliderPosition.
      Takes Rectangle as the rectangle currently occupied by the whole slider. }
    function XCoordToSliderPosition(const XCoord: Single;
      const R: TRectangle): Single;

    procedure DrawSliderText(const R: TRectangle; Text: string);
  strict protected
    { React to value change. The default implementation simply calls the OnChange
      event, if assigned. This is called when value is changed by user actions
      (key, mouse), not when it's explicitly assigned by code. }
    procedure DoChange; virtual;
  public
    const
      DefaultWidth = 200;
      DefaultHeight = 20;
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Rect: TRectangle; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  published
    property Width: Cardinal read FWidth write SetWidth default DefaultWidth;
    property Height: Cardinal read FHeight write SetHeight default DefaultHeight;

    property SmallFont default true;

    { Display the current value as text on the slider,
      right next to the @link(Caption).

      The exact method to display is defined by method
      @link(TCastleFloatSlider.ValueToStr) or
      @link(TCastleIntegerSlider.ValueToStr) (depending on descendant),
      so you can further customize it. }
    property DisplayValue: boolean
      read FDisplayValue write FDisplayValue default true;

    { Displayed on the slider. Right before value, if @link(DisplayValue). }
    property Caption: string read FCaption write SetCaption;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { Slider to change a float value within a given range. }
  TCastleFloatSlider = class(TCastleAbstractSlider)
  strict private
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FMultipleOf: Single;
    procedure SetMin(const AMin: Single);
    procedure SetMax(const AMax: Single);
    procedure SetValue(const AValue: Single);
  public
    const
      DefaultMin = 0.0;
      DefaultMax = 1.0;
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function ValueToStr(const AValue: Single): string; virtual;
    { Round to multiple of @link(MultipleOf), if non-zero, and clamp
      to @link(Min) and @link(Max) range. }
    function RoundAndClamp(const AValue: Single): Single;
  published
    property Min: Single read FMin write SetMin default DefaultMin;
    property Max: Single read FMax write SetMax default DefaultMax;

    { Current value. Usually within @link(Min) and @link(Max) range,
      although the general code should be ready for handle any value here
      (to work even during changes to @link(Min) and @link(Max) properties). }
    property Value: Single read FValue write SetValue default DefaultMin;

    { If non-zero, we force the value selected by user to be a multiple
      of this value (clamped to @link(Min), @link(Max) range).
      For example, if you set this to 0.25, and slider is between 0..1,
      then when user clicks around 0.3 --- we will pick 0.25. It user clicks
      around 0.4 --- we will pick 0.5.

      This only affects values selected by user interactions (clicking,
      dragging). This does not process the values you set by code to @link(Value)
      property, though you can use RoundAndClamp method on your values
      yourself. }
    property MultipleOf: Single read FMultipleOf write FMultipleOf;
  end;

  { Slider to change an integer value within a given range. }
  TCastleIntegerSlider = class(TCastleAbstractSlider)
  strict private
    FMin: Integer;
    FMax: Integer;
    FValue: Integer;
    function XCoordToValue(
      const XCoord: Single; const R: TRectangle): Integer;
    procedure SetMin(const AMin: Integer);
    procedure SetMax(const AMax: Integer);
    procedure SetValue(const AValue: Integer);
  public
    const
      DefaultMin = 0;
      DefaultMax = 10;
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function ValueToStr(const AValue: Integer): string; virtual;
  published
    property Min: Integer read FMin write SetMin default DefaultMin;
    property Max: Integer read FMax write SetMax default DefaultMax;

    { Current value. Usually within @link(Min) and @link(Max) range,
      although the general code should be ready for handle any value here
      (to work even during changes to @link(Min) and @link(Max) properties). }
    property Value: Integer read FValue write SetValue default DefaultMin;
  end;

  { Children added to @link(ScrollArea) can be scrolled vertically.
    We automatically show a scrollbar, and handle various scrolling inputs
    to be functional on both desktops and mobile
    (we handle scrolling by keys, mouse wheel, dragging by scrollbar,
    dragging the whole area - see @link(EnableDragging)). }
  TCastleScrollView = class(TUIControlSizeable)
  strict private
    FScrollArea: TUIControlSizeable;
    Scissor: TScissor;

    FScroll: Single;
    ScrollbarVisible: boolean;
    { Rects in screen coordinates (like ScreenRect, after UI scaling and anchors. }
    ScrollbarFrame, ScrollbarSlider: TRectangle;
    ScrollBarDragging: boolean;
    FKeyScrollSpeed, FWheelScrollSpeed: Single;
    FScrollBarWidth: Cardinal;
    FEnableDragging: boolean;
    DragSinceLastUpdate, DragSpeed, TimeSinceDraggingStopped, TimeSinceDraggingStarted: Double;
    ScrollbarActive: Single;
    FTintScrollBarInactive: TCastleColor;

    { Min and max sensible values for @link(Scroll). }
    function ScrollMin: Single;
    function ScrollMax: Single;

    procedure SetScroll(Value: Single);

    { How many pixels do we scroll. This corresponds to ScrollArea vertical anchor,
      so it's in unscaled pixels. Kept as a float, to allow smooth time-based changes.
      Note that setting it always clamps the value to a sensible range. }
    property Scroll: Single read FScroll write SetScroll;
  private
    function ScrollBarWidthScaled: Integer;
  public
    const
      DefaultKeyScrollSpeed = 200.0;
      DefaultWheelScrollSpeed = 20.0;
      DefaultScrollBarWidth = 20;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure RenderOverChildren; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    { Color and alpha tint to use when scrollbar is not used.
      May have some alpha, which makes scrollbar "make itself more opaque",
      and thus noticeable, when you start dragging.
      By default it's opaque white, which means that no tint is shown. }
    property TintScrollBarInactive: TCastleColor read FTintScrollBarInactive write FTintScrollBarInactive;
  published
    { Children you add here will be scrolled. }
    property ScrollArea: TUIControlSizeable read FScrollArea;
    { Speed of scrolling by keys, in pixels (before UI scaling) per second. }
    property KeyScrollSpeed: Single read FKeyScrollSpeed write FKeyScrollSpeed default DefaultKeyScrollSpeed;
    { Speed of scrolling by mouse wheel, in pixels (before UI scaling) per event. }
    property WheelScrollSpeed: Single read FWheelScrollSpeed write FWheelScrollSpeed default DefaultWheelScrollSpeed;
    { Width of the scroll bar. }
    property ScrollBarWidth: Cardinal read FScrollBarWidth write FScrollBarWidth default DefaultScrollBarWidth;
    { Enable scrolling by dragging @italic(anywhere) in the scroll area.
      This is usually suitable for mobile devices.
      Note that this doesn't affect the dragging directly by the scrollbar,
      which is always enabled. }
    property EnableDragging: boolean read FEnableDragging write FEnableDragging default false;
  end;

  { Timer, running the @link(OnTimer) event periodically. }
  TCastleTimer = class(TUIControl)
  strict private
    FCounteractDelays: boolean;
    FIntervalSeconds, IntervalRemaining: TFloatTime;
    FOnTimer: TNotifyEvent;
    procedure SetIntervalSeconds(AValue: TFloatTime);
  strict protected
    procedure DoTimer; virtual;
  public
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
  published
    { How often should we call OnTimer. Value of 0 means to call OnTimer
      in every Update event. }
    property IntervalSeconds: TFloatTime read FIntervalSeconds write SetIntervalSeconds;
    { The event called periodically. }
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    { Should we counteract the delays in timer by firing next event sooner.
      This helps to keep a constant frequency of timer events over a long time,
      and to keep multiple things (like multiple timer instances)
      perfectly synchronized with each other.
      But it may cause to execute a @italic(lot) of timer events at once,
      in case the application hung for some time. }
    property CounteractDelays: boolean
      read FCounteractDelays write FCounteractDelays default false;
  end;

  { Theme for 2D GUI controls.
    Should only be used through the single global instance @link(Theme). }
  TCastleTheme = class
  strict private
    FImages: array [TThemeImage] of TCastleImage;
    FCorners: array [TThemeImage] of TVector4Integer;
    FGLImages: array [TThemeImage] of TGLImageCore;
    FOwnsImages: array [TThemeImage] of boolean;
    FMessageFont: TCastleFont;
    FOwnsMessageFont: boolean;
    FMessageErrorBackground: boolean;
    function GetImages(const ImageType: TThemeImage): TCastleImage;
    procedure SetImages(const ImageType: TThemeImage; const Value: TCastleImage);
    function GetOwnsImages(const ImageType: TThemeImage): boolean;
    procedure SetOwnsImages(const ImageType: TThemeImage; const Value: boolean);
    function GetCorners(const ImageType: TThemeImage): TVector4Integer;
    procedure SetCorners(const ImageType: TThemeImage; const Value: TVector4Integer);
    function GetGLImages(const ImageType: TThemeImage): TGLImageCore;
    procedure GLContextClose(Sender: TObject);
    procedure SetMessageFont(const Value: TCastleFont);
  private
    { TGLImageCore instances for fast and easy drawing of images on 2D screen.
      Reading them for the 1st time means that the TGLImageCore instance is created,
      so use them only when OpenGL context is already active (window is open etc.).
      Changing the TCastleImage instance will automatically free (and recreate
      at next access) the corresponding TGLImageCore instance. }
    property GLImages[const ImageType: TThemeImage]: TGLImageCore read GetGLImages;
  public
    TooltipTextColor: TCastleColor;
    TextColor, DisabledTextColor: TCastleColor;
    MessageTextColor: TCastleColor;
    MessageInputTextColor: TCastleColor;

    BarEmptyColor: TVector3Byte;
    BarFilledColor: TVector3Byte;

    { Tint of background image under TCastleDialog and TGLModeFrozenScreen.
      Default is (0.25, 0.25, 0.25, 1), which makes background darker,
      which helps dialog to stand out. }
    BackgroundTint: TCastleColor;

    { Colors used when displaying the "Loading..." text when Android application
      is resuming. Note that you can also customize the tiLoading image.
      By default, LoadingBackgroundColor is black, and LoadingTextColor is white. }
    LoadingBackgroundColor, LoadingTextColor: TCastleColor;

    { Undernath various message dialogs show "error background"
      (@link(tiErrorBackground)) image instead of the default behaviour.
      @italic(This is automatically used by CastleWindow exception handler,
      you should not need to modify this property yourself.)

      This affects modal messages made by @link(CastleMessages) unit,
      and modal messages made by @link(TCastleWindowCustom.MessageOK) and friends,
      or modal states by @link(TGLModeFrozenScreen).

      The default behaviour of them is to show the TCastleDialog provided background
      (coming from screenshot usually), in case of
      @link(CastleMessages) it is mixed with @link(BackgroundTint).
      But saving the screen is potentially a bad idea when an exception occured
      (since the application may be already in some dirty state, for which
      the developer is not necessarily prepared). }
    property MessageErrorBackground: boolean
      read FMessageErrorBackground write FMessageErrorBackground default false;

    constructor Create;
    destructor Destroy; override;

    { 2D GUI images, represented as TCastleImage.
      Although they all have sensible defaults, you can also change them
      at any time. Simply create TCastleImage instance (e.g. by LoadImage
      function) and assign it here. Be sure to adjust also @link(OwnsImages)
      if you want the theme to automatically free the image when it's no longer
      used.

      The alpha channel of the image, if any, is automatically correctly used
      (for alpha test or alpha blending, see TGLImageCore). }
    property Images[const ImageType: TThemeImage]: TCastleImage read GetImages write SetImages;

    property OwnsImages[const ImageType: TThemeImage]: boolean read GetOwnsImages write SetOwnsImages;

    { Corners that determine how image on @link(Images) is stretched when
      drawing by @link(TCastleTheme.Draw) method.
      Together with assigning @link(Images), adjust also this property.
      It is used for images rendered using TGLImageCore.Draw3x3,
      it determines how the image is stretched.
      The corners are specified as 4D vector, order like in CSS: top, right, down,
      left. }
    property Corners[const ImageType: TThemeImage]: TVector4Integer read GetCorners write SetCorners;

    { Draw the selected theme image on screen.

      @param(Color Color tint of the image.
        If you do not specify a color, white will be used, so image will be displayed
        as-is. Specifying a color means that image will be multiplied by it,
        just like for @link(TGLImageCore.Color).)

      @param(UIScale Used to properly scale corners, passed to @link(TGLImageCore.ScaleCorners).
        This parameter does @italic(not) scale the place where image is displayed,
        to do this just scale the given Rect parameter yourself.)
    }
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single = 1.0);
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const UIScale: Single; const Color: TCastleColor);

    { Font used by dialogs. Leave @nil to use UIFont. }
    property MessageFont: TCastleFont read FMessageFont write SetMessageFont;
    property OwnsMessageFont: boolean
      read FOwnsMessageFont write FOwnsMessageFont default true;

    { Set dialogs theme to light. }
    procedure DialogsLight;
  end;

{ The 2D fonts used throughout UI interface.

  By default, this is a modern sans-serif font hardcoded into the engine.
  It will be automatically created and freed if needed. This is comfortable
  for simple applications, you can just start "drawing text"
  without initializing anything.

  You can assign your own font here, to make this the default
  font used by all 2D controls.

  Note that assigning font here @italic(does not)
  make it automatically freed (this would cause more trouble than comfort).
  To make sure your own fonts are always freed, set the font "owner" at creation,
  e.g. the example below sets the @code(Application) as owner:

  @longCode(#
    MyFont := TTextureFont.Create(Application);
    MyFont.Load(TextureFont_Xxxx);
    UIFont := MyFont;
  #)

  @groupBegin }
function GetUIFont: TCastleFont;
procedure SetUIFont(const Value: TCastleFont);

function GetUIFontSmall: TCastleFont;
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TUIControlFont.SmallFont';
procedure SetUIFontSmall(const Value: TCastleFont);
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TUIControlFont.SmallFont';

property UIFont: TCastleFont read GetUIFont write SetUIFont;
property UIFontSmall: TCastleFont read GetUIFontSmall write SetUIFontSmall;
{ @groupEnd }

function Theme: TCastleTheme;

procedure Register;

implementation

uses SysUtils, Math, CastleControlsImages, CastleTextureFont_DjvSans_20,
  CastleTextureFont_DejaVuSans_10,
  CastleApplicationProperties, CastleStringUtils;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleButton, TCastleImageControl, TCastleRectangleControl,
    TCastleLabel, TCastleCrosshair, TCastleIntegerSlider, TCastleFloatSlider,
    TCastleScrollView]);
end;

{ UIFont --------------------------------------------------------------------- }

type
  TDefaultUIFont = class(TComponent)
  public
    Normal: TCastleFont;
    Small: TCastleFont;
  end;

var
  DefaultUIFont: TDefaultUIFont;
  FUIFont, FUIFontSmall: TCastleFont;

function GetUIFont: TCastleFont;
begin
  if FUIFont = nil then
  begin
    if DefaultUIFont = nil then
      DefaultUIFont := TDefaultUIFont.Create(nil);
    if DefaultUIFont.Normal = nil then
    begin
      DefaultUIFont.Normal := TTextureFont.Create(DefaultUIFont);
      (DefaultUIFont.Normal as TTextureFont).Load(TextureFont_DejaVuSans_20);
    end;
    FUIFont := DefaultUIFont.Normal;
  end;

  Result := FUIFont;
end;

procedure SetUIFont(const Value: TCastleFont);
begin
  FUIFont := Value;
end;

function GetUIFontSmall: TCastleFont;
begin
  if FUIFontSmall = nil then
  begin
    if DefaultUIFont = nil then
      DefaultUIFont := TDefaultUIFont.Create(nil);
    if DefaultUIFont.Small = nil then
    begin
      DefaultUIFont.Small := TTextureFont.Create(DefaultUIFont);
      (DefaultUIFont.Small as TTextureFont).Load(TextureFont_DejaVuSans_10);
    end;
    FUIFontSmall := DefaultUIFont.Small;
  end;

  Result := FUIFontSmall;
end;

procedure SetUIFontSmall(const Value: TCastleFont);
begin
  FUIFontSmall := Value;
end;

{ TCastleTimer --------------------------------------------------------------- }

procedure TCastleTimer.SetIntervalSeconds(AValue: TFloatTime);
begin
  if FIntervalSeconds <> AValue then
  begin
    FIntervalSeconds := AValue;
    IntervalRemaining := FIntervalSeconds;
  end;
end;

procedure TCastleTimer.DoTimer;
begin
  if Assigned(OnTimer) then
    OnTimer(Self);
end;

procedure TCastleTimer.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if IntervalSeconds <> 0 then
  begin
    IntervalRemaining -= SecondsPassed;
    if CounteractDelays then
    begin
      while IntervalRemaining < 0 do
      begin
        IntervalRemaining += IntervalSeconds;
        DoTimer;
      end;
    end else
    begin
      if IntervalRemaining < 0 then
      begin
        IntervalRemaining := IntervalSeconds;
        DoTimer;
      end;
    end;
  end else
  begin
    { for IntervalSeconds = 0, just call timer at every Update. }
    DoTimer;
  end;
end;

{ TUIControlFont ---------------------------------------------------------- }

destructor TUIControlFont.Destroy;
begin
  CustomFont := nil; // make sure to free FCustomFont, if necessary
  FreeAndNil(FCustomizedFont);
  inherited;
end;

function TUIControlFont.TooltipExists: boolean;
begin
  Result := Tooltip <> '';
end;

procedure TUIControlFont.TooltipRender;
var
  X, Y: Integer;
  TooltipRect: TRectangle;
begin
  if TooltipLabel = nil then
  begin
    TooltipLabel := TCastleLabel.Create(nil);
    TooltipLabel.ImageType := tiTooltip;
    TooltipLabel.Frame := true;
    TooltipLabel.Container := Container;
    { we know that GL context now exists, so just directly call GLContextOpen }
    TooltipLabel.GLContextOpen;
  end;

  { assign TooltipLabel.Text first, to get TooltipRect.Width/Height }
  TooltipLabel.Padding := 5;
  TooltipLabel.Color := Theme.TooltipTextColor;
  TooltipLabel.Text.Clear;
  TooltipLabel.Text.Append(Tooltip);
  TooltipRect := TooltipLabel.Rect;

  { divide by TooltipLabel.UIScale because TooltipLabel.Rect will multiply by it }
  X := Round(Container.TooltipPosition[0] / TooltipLabel.UIScale);
  Y := Round(Container.TooltipPosition[1] / TooltipLabel.UIScale);

  { now try to fix X, Y to make tooltip fit inside a window }
  MinVar(X, ContainerWidth - TooltipRect.Width);
  MinVar(Y, ContainerHeight - TooltipRect.Height);
  MaxVar(X, 0);
  MaxVar(Y, 0);

  TooltipLabel.Left := X;
  TooltipLabel.Bottom := Y;

  { just explicitly call Render method of TooltipLabel }
  TooltipLabel.Render;
end;

procedure TUIControlFont.GLContextClose;
begin
  { make sure to call GLContextClose on TooltipLabel,
    actually we can just free it now }
  FreeAndNil(TooltipLabel);
  inherited;
end;

function TUIControlFont.Font: TCastleFont;
begin
  if CustomFont <> nil then
    Result := CustomFont else
    Result := UIFont;

  { if FontSize or UIScale are non-trivial,
    wrap Result in TCustomizedFont instance }
  if (FFontSize <> 0) or (UIScale <> 1) or SmallFont or (Outline <> 0) then
  begin
    if FCustomizedFont = nil then
      FCustomizedFont := TFontFamily.Create(nil);
    if FFontSize <> 0 then
      FCustomizedFont.Size := FFontSize else
      FCustomizedFont.Size := Result.RealSize; // to have something to multiply in line below
    FCustomizedFont.Size := FCustomizedFont.Size * UIScale;
    if SmallFont then
      FCustomizedFont.Size := FCustomizedFont.Size * 0.5;
    if Result is TFontFamily then
    begin
      { copying Result to FCustomizedFont this way allows to render with HTML
        when CustomFont and FontSize are used together }
      FCustomizedFont.RegularFont := TFontFamily(Result).RegularFont;
      FCustomizedFont.BoldFont := TFontFamily(Result).BoldFont;
      FCustomizedFont.ItalicFont := TFontFamily(Result).ItalicFont;
      FCustomizedFont.BoldItalicFont := TFontFamily(Result).BoldItalicFont;
    end else
    begin
      FCustomizedFont.RegularFont := Result;
      FCustomizedFont.BoldFont := nil;
      FCustomizedFont.ItalicFont := nil;
      FCustomizedFont.BoldItalicFont := nil;
    end;
    if Outline <> 0 then
    begin
      FCustomizedFont.CustomizeOutline := true;
      FCustomizedFont.Outline := Outline;
      FCustomizedFont.OutlineHighQuality := OutlineHighQuality;
      FCustomizedFont.OutlineColor := OutlineColor;
    end;
    Result := FCustomizedFont;
  end;
end;

procedure TUIControlFont.SetCustomFont(const Value: TCastleFont);
begin
  if FCustomFont <> Value then
  begin
    if FCustomFont <> nil then
      FCustomFont.RemoveFreeNotification(Self);
    {$warnings off}
    if OwnsCustomFont then
      FreeAndNil(FCustomFont) else
      FCustomFont := nil;
    {$warnings on}
    FCustomFont := Value;
    if FCustomFont <> nil then
      FCustomFont.FreeNotification(Self);
    { don't call virtual function, that may try to measure the text by accessing Font
      (needlessly using UIFont, possibly even recreating UIFont if "finalization"
      of this unit already run, possibly even accessing invalid freed
      TextureFontData). }
    if not (csDestroying in ComponentState) then
      FontChanged;
  end;
end;

procedure TUIControlFont.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FCustomFont) then
  begin
    { set to nil by SetCustomFont to clean nicely }
    { since it's already being freed, do not apply OwnsCustomFont effect
      inside SetCustomFont. }
    {$warnings off}
    OwnsCustomFont := false;
    {$warnings on}
    CustomFont := nil;
  end;
end;

procedure TUIControlFont.SetFontSize(const Value: Single);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetSmallFont(const Value: boolean);
begin
  if FSmallFont <> Value then
  begin
    FSmallFont := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutline(const Value: Cardinal);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutlineColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutlineHighQuality(const Value: boolean);
begin
  if FOutlineHighQuality <> Value then
  begin
    FOutlineHighQuality := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.FontChanged;
begin
end;

procedure TUIControlFont.UIScaleChanged;
begin
  inherited;
  FontChanged;
end;

procedure TUIControlFont.Render;
begin
  inherited;
  CheckFontChanged;
end;

procedure TUIControlFont.CheckFontChanged;
var
  DefaultFont: TCastleFont;
begin
  { we do not use here UIFont[Small] but FUIFont[Small],
    to not create font without need. If the corresponding global font is nil now,
    it can remain nil, no need to initialize it yet. }
  if SmallFont then
    DefaultFont := FUIFontSmall else
    DefaultFont := FUIFont;

  if (CustomFont = nil) and (FLastSeenUIFontXxx <> DefaultFont) then
    FontChanged;
  FLastSeenUIFontXxx := DefaultFont;
end;

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
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }
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

  inherited;
end;

procedure TCastleButton.Render;
var
  TextLeft, TextBottom: Integer;

  procedure RenderText;
  var
    TextColor: TCastleColor;
    TextX, LineSpacingScaled: Integer;
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
        hpMiddle: TextX := TextLeft + TextWidth div 2;
        hpRight : TextX := TextLeft + TextWidth;
        else raise EInternalError.Create('TCastleButton.Render: Alignment?');
      end;

      LineSpacingScaled := Round(UIScale * LineSpacing);
      TextToRender := GetTextToRender;
      try
        TextToRender.Print(TextX, TextBottom, TextColor, LineSpacingScaled, TextAlignment);
      finally FreeAndNil(TextToRender) end;
    end;
  end;

var
  ImgLeft, ImgBottom, ImgScreenWidth, ImgScreenHeight: Integer;
  Background: TThemeImage;
  CustomBackgroundImage: TGLImageCore;
  SR: TRectangle;
  ImageMarginScaled: Cardinal;
  UseImage: boolean;
  Tint: TCastleColor;
begin
  inherited;

  ImageMarginScaled := Round(ImageMargin * UIScale);

  SR := ScreenRect;

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
      CustomBackgroundImage := FGLCustomBackgroundPressed else
    if not Enabled then
      CustomBackgroundImage := FGLCustomBackgroundDisabled else
    if Focused then
      CustomBackgroundImage := FGLCustomBackgroundFocused else
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
    end;
  end else
  begin
    if Pressed then
      Background := tiButtonPressed else
    if not Enabled then
      Background := tiButtonDisabled else
    if Focused then
      Background := tiButtonFocused else
      Background := tiButtonNormal;
    Theme.Draw(SR, Background, UIScale, Tint);
  end;

  UseImage := (FImage <> nil) and (FGLImage <> nil);
  if UseImage then
  begin
    ImgScreenWidth  := Round(UIScale * FImage.Width);
    ImgScreenHeight := Round(UIScale * FImage.Height);
  end;

  TextLeft := SR.Left + (SR.Width - TextWidth) div 2;
  if UseImage and (ImageLayout = ilLeft) then
    TextLeft += (ImgScreenWidth + ImageMarginScaled) div 2 else
  if UseImage and (ImageLayout = ilRight) then
    TextLeft -= (ImgScreenWidth + ImageMarginScaled) div 2;

  TextBottom := SR.Bottom + (SR.Height - TextHeight) div 2;
  if UseImage and (ImageLayout = ilBottom) then
    TextBottom += (ImgScreenHeight + ImageMarginScaled) div 2 else
  if UseImage and (ImageLayout = ilTop) then
    TextBottom -= (ImgScreenHeight + ImageMarginScaled) div 2;
  TextBottom += Font.Descend;

  RenderText;

  if UseImage then
  begin
    { update FGLImage.Alpha based on ImageAlphaTest }
    if FImage.HasAlpha then
    begin
      if ImageAlphaTest then
        FGLImage.Alpha := acTest else
        FGLImage.Alpha := acBlending;
    end;
    case ImageLayout of
      ilLeft         : ImgLeft := TextLeft - ImgScreenWidth - ImageMarginScaled;
      ilRight        : ImgLeft := TextLeft + TextWidth + ImageMarginScaled;
      ilBottom, ilTop: ImgLeft := SR.Left + (SR.Width - ImgScreenWidth) div 2;
    end;
    case ImageLayout of
      ilBottom       : ImgBottom := TextBottom - ImgScreenHeight - ImageMarginScaled;
      ilTop          : ImgBottom := TextBottom + TextHeight + ImageMarginScaled;
      ilLeft, ilRight: ImgBottom := SR.Bottom + (SR.Height - ImgScreenHeight) div 2;
    end;
    FGLImage.Draw(Rectangle(ImgLeft, ImgBottom, ImgScreenWidth, ImgScreenHeight));
  end;
end;

procedure TCastleButton.GLContextOpen;
begin
  inherited;
  if (FGLImage = nil) and (FImage <> nil) then
    FGLImage := TGLImageCore.Create(FImage, true);
  if (FGLCustomBackgroundPressed = nil) and (FCustomBackgroundPressed <> nil) then
    FGLCustomBackgroundPressed := TGLImageCore.Create(FCustomBackgroundPressed, true);
  if (FGLCustomBackgroundDisabled = nil) and (FCustomBackgroundDisabled <> nil) then
    FGLCustomBackgroundDisabled := TGLImageCore.Create(FCustomBackgroundDisabled, true);
  if (FGLCustomBackgroundFocused = nil) and (FCustomBackgroundFocused <> nil) then
    FGLCustomBackgroundFocused := TGLImageCore.Create(FCustomBackgroundFocused, true);
  if (FGLCustomBackgroundNormal = nil) and (FCustomBackgroundNormal <> nil) then
    FGLCustomBackgroundNormal := TGLImageCore.Create(FCustomBackgroundNormal, true);
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
      VisibleChange;
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
    VisibleChange;

    { This is normal behavior of buttons: to click them, you have to make
      mouse down on the button, and then release mouse while still over
      the button.

      We have to check ScreenRect.Contains, since (because we keep "focus"
      on this button, if mouse down was on this) we *always* get release event
      (even if mouse position is no longer over this button).

      This is consistent with behaviours of other toolkits.
      It means that if the user does mouse down over the button,
      moves mouse out from the control, then moves it back inside,
      then does mouse up -> it counts as a "click". }
    if Enabled and ScreenRect.Contains(Event.Position) then
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
    VisibleChange;
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
  LineSpacingScaled: Integer;
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
      LineSpacingScaled := Round(UIScale * LineSpacing);
      TextToRender := GetTextToRender;
      try
        TextWidth := TextToRender.Width;
        TextHeight := TextToRender.Count * (Font.RowHeight + LineSpacingScaled);
      finally FreeAndNil(TextToRender) end;
    end;

    FFinalScaledValid := false;
  end;
end;

procedure TCastleButton.UIScaleChanged;
begin
  inherited;
  FFinalScaledValid := false;
end;

procedure TCastleButton.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FreeAndNil(FGLImage);

    FImage := Value;

    if GLInitialized and (FImage <> nil) then
      FGLImage := TGLImageCore.Create(FImage, true);

    FFinalScaledValid := false;
  end;
end;

procedure TCastleButton.SetCustomBackgroundPressed(const Value: TCastleImage);
begin
  if FCustomBackgroundPressed <> Value then
  begin
    if OwnsCustomBackgroundPressed then FreeAndNil(FCustomBackgroundPressed);
    FreeAndNil(FGLCustomBackgroundPressed);

    FCustomBackgroundPressed := Value;

    if GLInitialized and (FCustomBackgroundPressed <> nil) then
      FGLCustomBackgroundPressed := TGLImageCore.Create(FCustomBackgroundPressed, true);

    FFinalScaledValid := false;
  end;
end;

procedure TCastleButton.SetCustomBackgroundDisabled(const Value: TCastleImage);
begin
  if FCustomBackgroundDisabled <> Value then
  begin
    if OwnsCustomBackgroundDisabled then FreeAndNil(FCustomBackgroundDisabled);
    FreeAndNil(FGLCustomBackgroundDisabled);

    FCustomBackgroundDisabled := Value;

    if GLInitialized and (FCustomBackgroundDisabled <> nil) then
      FGLCustomBackgroundDisabled := TGLImageCore.Create(FCustomBackgroundDisabled, true);

    FFinalScaledValid := false;
  end;
end;

procedure TCastleButton.SetCustomBackgroundFocused(const Value: TCastleImage);
begin
  if FCustomBackgroundFocused <> Value then
  begin
    if OwnsCustomBackgroundFocused then FreeAndNil(FCustomBackgroundFocused);
    FreeAndNil(FGLCustomBackgroundFocused);

    FCustomBackgroundFocused := Value;

    if GLInitialized and (FCustomBackgroundFocused <> nil) then
      FGLCustomBackgroundFocused := TGLImageCore.Create(FCustomBackgroundFocused, true);

    FFinalScaledValid := false;
  end;
end;

procedure TCastleButton.SetCustomBackgroundNormal(const Value: TCastleImage);
begin
  if FCustomBackgroundNormal <> Value then
  begin
    if OwnsCustomBackgroundNormal then FreeAndNil(FCustomBackgroundNormal);
    FreeAndNil(FGLCustomBackgroundNormal);

    FCustomBackgroundNormal := Value;

    if GLInitialized and (FCustomBackgroundNormal <> nil) then
      FGLCustomBackgroundNormal := TGLImageCore.Create(FCustomBackgroundNormal, true);

    FFinalScaledValid := false;
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
    VisibleChange;
  end;

  inherited;
end;

procedure TCastleButton.SetTextAlignment(const Value: THorizontalPosition);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetLineSpacing(const Value: Integer);
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
    if not Toggle then
      raise Exception.Create('You cannot modify TCastleButton.Pressed value when Toggle is false');
    FPressed := Value;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetImageLayout(const Value: TCastleButtonImageLayout);
begin
  if FImageLayout <> Value then
  begin
    FImageLayout := Value;
    FFinalScaledValid := false;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    FFinalScaledValid := false;
    VisibleChange(true);
  end;
end;

procedure TCastleButton.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FFinalScaledValid := false;
    VisibleChange(true);
  end;
end;

procedure TCastleButton.SetMinWidth(const Value: Cardinal);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    FFinalScaledValid := false;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetMinHeight(const Value: Cardinal);
begin
  if FMinHeight <> Value then
  begin
    FMinHeight := Value;
    FFinalScaledValid := false;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetImageMargin(const Value: Cardinal);
begin
  if FImageMargin <> Value then
  begin
    FImageMargin := Value;
    FFinalScaledValid := false;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange;
  end;
end;

function TCastleButton.Rect: TRectangle;

  procedure CalculateFinalScaledSize;
  var
    ImgSize: Cardinal;
    MinWidthScaled, MinHeightScaled,
      ImageMarginScaled,
      PaddingHorizontalScaled, PaddingVerticalScaled: Cardinal;
  begin
    FFinalScaledWidth  := Round(FWidth  * UIScale);
    FFinalScaledHeight := Round(FHeight * UIScale);

    if AutoSize then
    begin
      PaddingHorizontalScaled := Round(PaddingHorizontal * UIScale);
      PaddingVerticalScaled   := Round(PaddingVertical   * UIScale);
      ImageMarginScaled       := Round(ImageMargin       * UIScale);
      MinWidthScaled          := Round(MinWidth          * UIScale);
      MinHeightScaled         := Round(MinHeight         * UIScale);

      { We modify FFinalScaledWidth, FFinalScaledHeight,
        and avoid causing UpdateFocusAndMouseCursor too many times.
        We'll call it at the end explicitly, with VisibleChange(true). }
      if AutoSizeWidth  then FFinalScaledWidth  := TextWidth  + PaddingHorizontalScaled * 2;
      if AutoSizeHeight then FFinalScaledHeight := TextHeight + PaddingVerticalScaled * 2;
      if (FImage <> nil) or
         (MinImageWidth <> 0) or
         (MinImageHeight <> 0) then
      begin
        if AutoSizeWidth then
        begin
          if FImage <> nil then
            ImgSize := Max(FImage.Width, MinImageWidth) else
            ImgSize := MinImageWidth;
          ImgSize := Round(ImgSize * UIScale);
          case ImageLayout of
            ilLeft, ilRight: FFinalScaledWidth := FFinalScaledWidth + ImgSize + ImageMarginScaled;
            ilTop, ilBottom: FFinalScaledWidth := Max(FFinalScaledWidth, ImgSize + PaddingHorizontalScaled * 2);
          end;
        end;
        if AutoSizeHeight then
        begin
          if FImage <> nil then
            ImgSize := Max(FImage.Height, MinImageHeight) else
            ImgSize := MinImageHeight;
          ImgSize := Round(ImgSize * UIScale);
          case ImageLayout of
            ilLeft, ilRight: FFinalScaledHeight := Max(FFinalScaledHeight, ImgSize + PaddingVerticalScaled * 2);
            ilTop, ilBottom: FFinalScaledHeight := FFinalScaledHeight + ImgSize + ImageMarginScaled;
          end;
        end;
      end;

      { at the end apply MinXxx properties }
      if AutoSizeWidth then
        MaxVar(FFinalScaledWidth, MinWidthScaled);
      if AutoSizeHeight then
        MaxVar(FFinalScaledHeight, MinHeightScaled);

      if AutoSizeWidth or AutoSizeHeight then
        VisibleChange(true);
    end;
  end;

begin
  if not FFinalScaledValid then
  begin
    FFinalScaledValid := true;
    CalculateFinalScaledSize;
  end;
  Result := Rectangle(LeftBottomScaled, FFinalScaledWidth, FFinalScaledHeight);
end;

{ TCastlePanel ------------------------------------------------------------------ }

constructor TCastlePanel.Create(AOwner: TComponent);
begin
  inherited;
  FVerticalSeparators := TCardinalList.Create;
end;

destructor TCastlePanel.Destroy;
begin
  FreeAndNil(FVerticalSeparators);
  inherited;
end;

procedure TCastlePanel.Render;
const
  SeparatorMargin = 8;
var
  I, SeparatorMarginScaled: Integer;
  SR: TRectangle;
begin
  inherited;

  SR := ScreenRect;
  Theme.Draw(SR, tiPanel, UIScale);

  SeparatorMarginScaled := Round(SeparatorMargin * UIScale);
  if Height <= 2 * SeparatorMarginScaled then
    Exit; // no space
  for I := 0 to VerticalSeparators.Count - 1 do
    Theme.Draw(Rectangle(
      SR.Left + Round(VerticalSeparators[I] * UIScale),
      SR.Bottom + SeparatorMarginScaled,
      Theme.Images[tiPanelSeparator].Width, Height - 2 * SeparatorMarginScaled),
      tiPanelSeparator, UIScale);
end;

class function TCastlePanel.SeparatorSize: Cardinal;
begin
  Result := 2;
end;

{ TCastleImageControl ---------------------------------------------------------------- }

constructor TCastleImageControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
  FOwnsImage := true;
  FSmoothScaling := true;
  FCenterX := 0.5;
  FCenterY := 0.5;
  FRotation := 0;
end;

destructor TCastleImageControl.Destroy;
begin
  if OwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.SetSmoothScaling(const Value: boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    if FGLImage <> nil then
    begin
      FGLImage.SmoothScaling := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetClip(const Value: boolean);
begin
  if FClip <> Value then
  begin
    FClip := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Clip := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetClipLine(const Value: TVector3Single);
begin
  if not VectorsPerfectlyEqual(FClipLine, Value) then
  begin
    FClipLine := Value;
    if FGLImage <> nil then
    begin
      FGLImage.ClipLine := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetURL(const Value: string);
begin
  if Value <> '' then
    Image := LoadImage(Value) else
    Image := nil;

  { only once new Image is successfully loaded, change property value.
    If LoadImage raised exception, URL will remain unchanged. }
  FURL := Value;
end;

procedure TCastleImageControl.SetCenterX(const AValue: Single);
begin
  if FCenterX <> AValue then
  begin
    FCenterX := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.CenterX := FCenterX;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetCenterY(const AValue: Single);
begin
  if FCenterY <> AValue then
  begin
    FCenterY := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.CenterY := FCenterY;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetRotation(const AValue: Single);
begin
  if FRotation <> AValue then
  begin
    FRotation := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.Rotation := FRotation;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FImage := Value;
    ImageChanged;
  end;
end;

procedure TCastleImageControl.Render;
var
  SR: TRectangle;
begin
  inherited;
  if FGLImage = nil then Exit;
  SR := ScreenRect;
  if ZeroVector(FCorners) then
    FGLImage.Draw(SR) else
  begin
    FGLImage.ScaleCorners := UIScale;
    FGLImage.Draw3x3(SR, FCorners);
  end;
  { Useful to debug that Proportional works.
  if Stretch and not FullSize then
    Theme.Draw(Rectangle(Left, Bottom, Width, Height), tiActiveFrame, UIScale); }
end;

function TCastleImageControl.Rect: TRectangle;
var
  NewWidth, NewHeight, NewLeft, NewBottom: Integer;
  ApplyScaling: boolean;
begin
  ApplyScaling := true;
  if not Stretch then
  begin
    if FImage <> nil then
      Result := Rectangle(Left, Bottom, FImage.Width, FImage.Height) else
      Result := Rectangle(Left, Bottom, 0, 0);
  end else
  begin
    if FullSize then
    begin
      Result := ParentRect;
      ApplyScaling := false;
    end else
    if Proportional and (FImage <> nil) then
    begin
      if Width / Height > FImage.Width / FImage.Height then
      begin
        NewWidth := FImage.Width * Height div FImage.Height;
        NewLeft := Left + (Width - NewWidth) div 2;
        Result := Rectangle(NewLeft, Bottom, NewWidth, Height);
      end else
      begin
        NewHeight := FImage.Height * Width div FImage.Width;
        NewBottom := Bottom + (Height - NewHeight) div 2;
        Result := Rectangle(Left, NewBottom, Width, NewHeight);
      end;
    end else
      Result := Rectangle(Left, Bottom, Width, Height);
  end;

  if ApplyScaling then
    // applying UIScale on this is easy...
    Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleImageControl.GLContextOpen;
begin
  inherited;
  if FGLImage = nil then
    ImageChanged;
end;

procedure TCastleImageControl.GLContextClose;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.ImageChanged;
begin
  if GLInitialized then
  begin
    if FImage <> nil then
    begin
      if FGLImage <> nil then
        FGLImage.Load(FImage) else
      begin
        FGLImage := TGLImageCore.Create(FImage, FSmoothScaling);
        FGLImage.Color := Color;
        FGLImage.CenterX := FCenterX;
        FGLImage.CenterY := FCenterY;
        FGLImage.Rotation := FRotation;
        FGLImage.Clip := FClip;
        FGLImage.ClipLine := FClipLine;
        FGLImage.Alpha := AlphaChannel;
      end;
    end else
      FreeAndNil(FGLImage); // make sure to free FGLImage when FImage is nil
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  if FAlphaChannel <> Value then
  Begin
    FAlphaChannel := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Alpha := Value;
      VisibleChange;
    end;
  end;
end;

function TCastleImageControl.GetBlending: boolean;
begin
  Result := AlphaChannel <> acBlending;
end;

procedure TCastleImageControl.SetBlending(const Value: boolean);
begin
  if Value then
    AlphaChannel := acBlending else
    AlphaChannel := acTest;
end;

procedure TCastleImageControl.SetStretch(const Value: boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetProportional(const Value: boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FColor, Value) then
  begin
    FColor := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Color := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    VisibleChange;
  end;
end;

{ TCastleCrosshair ---------------------------------------------------- }

constructor TCastleCrosshair.Create(AOwner: TComponent);
begin
  inherited;
  FShape := csCross;
  Anchor(hpMiddle);
  Anchor(vpMiddle);
end;

procedure TCastleCrosshair.SetShape(const Value: TCastleCrosshairShape);
begin
  FShape := Value;
  VisibleChange;
end;

function TCastleCrosshair.ImageType: TThemeImage;
begin
  if FShape = csCrossRect then
    Result := tiCrosshair2 else
    Result := tiCrosshair1;
end;

function TCastleCrosshair.Rect: TRectangle;
begin
  Result := Theme.Images[ImageType].Rect;

  // applying UIScale on this is easy...
  Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleCrosshair.Render;
begin
  inherited;
  Theme.Draw(ScreenRect, ImageType, UIScale);
end;

{ TCastleTouchControl ---------------------------------------------------------------- }

constructor TCastleTouchControl.Create(AOwner: TComponent);
begin
  inherited;
  FDragging := -1;
  FScale := 1;
end;

procedure TCastleTouchControl.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    VisibleChange;
  end;
end;

function TCastleTouchControl.TotalScale: Single;
begin
  if ContainerSizeKnown then
    Result := Scale * Container.Dpi / 96 else
    Result := 1;
end;

function TCastleTouchControl.Rect: TRectangle;
begin
  // do not apply UIScale here to Width / Height,
  // it's already adjusted to physical size
  Result := Rectangle(LeftBottomScaled,
    Round(Theme.Images[tiTouchCtlOuter].Width  * TotalScale),
    Round(Theme.Images[tiTouchCtlOuter].Height * TotalScale));
end;

procedure TCastleTouchControl.SetPosition(const Value: TCastleTouchPosition);
const
  CtlBorder = 24;
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    case Position of
      tpLeft:
        begin
          Anchor(hpLeft, CtlBorder);
          Anchor(vpBottom, CtlBorder);
        end;
      tpRight:
        begin
          Anchor(hpRight, -CtlBorder);
          Anchor(vpBottom, CtlBorder);
        end;
    end;
    VisibleChange;
  end;
end;

function TCastleTouchControl.MaxOffsetDist: Integer;
begin
  Result := Round(TotalScale *
    (Theme.Images[tiTouchCtlOuter].Width -
     Theme.Images[tiTouchCtlInner].Width) / 2);
end;

procedure TCastleTouchControl.Render;
var
  LevOffsetTrimmedX, LevOffsetTrimmedY, MaxDist: Integer;
  LeverDist: Double;
  InnerRect: TRectangle;
  ImageInner, ImageOuter: TThemeImage;
  SR: TRectangle;
begin
  inherited;
  SR := ScreenRect;

  if FTouchMode = ctcmFlyUpdown then
  begin
    ImageInner := tiTouchCtlFlyInner;
    ImageOuter := tiTouchCtlFlyOuter;
  end else
  begin
    ImageInner := tiTouchCtlInner;
    ImageOuter := tiTouchCtlOuter;
  end;
  Theme.Draw(SR, ImageOuter, UIScale);

  // compute lever offset (must not move outside outer ring)
  LeverDist := VectorLen(FLeverOffset);
  MaxDist := MaxOffsetDist;
  if LeverDist <= MaxDist then
  begin
    LevOffsetTrimmedX := Round(FLeverOffset[0]);
    LevOffsetTrimmedY := Round(FLeverOffset[1]);
  end else
  begin
    LevOffsetTrimmedX := Floor((FLeverOffset[0]*MaxDist)/LeverDist);
    LevOffsetTrimmedY := Floor((FLeverOffset[1]*MaxDist)/LeverDist);
  end;
  if FTouchMode = ctcmFlyUpdown then LevOffsetTrimmedX := 0;

  // draw lever
  InnerRect := Theme.Images[ImageInner].Rect; // rectangle at (0,0)
  InnerRect.Width  := Round(InnerRect.Width  * TotalScale);
  InnerRect.Height := Round(InnerRect.Height * TotalScale);
  InnerRect.Left   := SR.Left   + (SR.Width  - InnerRect.Width ) div 2 + LevOffsetTrimmedX;
  InnerRect.Bottom := SR.Bottom + (SR.Height - InnerRect.Height) div 2 + LevOffsetTrimmedY;

  Theme.Draw(InnerRect, ImageInner, UIScale);
end;

procedure TCastleTouchControl.SetTouchMode(const Value: TCastleTouchCtlMode);
begin
  if FTouchMode <> Value then
  begin
    FTouchMode := Value;
    { we may swap outer image depending on the TouchMode in some later version }
    VisibleChange;
  end;
end;

function TCastleTouchControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  Result := ExclusiveEvents;
  FDragging := Event.FingerIndex;
  FLeverOffset := ZeroVector2Single;
end;

function TCastleTouchControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  if FDragging = Event.FingerIndex then
  begin
    Result := ExclusiveEvents;

    FDragging := -1;
    FLeverOffset := ZeroVector2Single;
    VisibleChange; { repaint with lever back in the center }
  end;
end;

function TCastleTouchControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  if (not Result) and (FDragging = Event.FingerIndex) then
  begin
    FLeverOffset := FLeverOffset + Event.Position - Event.OldPosition;
    VisibleChange;
    Result := ExclusiveEvents;
  end;
end;

procedure TCastleTouchControl.GetSensorRotation(var X, Y, Z, Angle: Double);
var
  FxConst: Double;
begin
  FxConst := 10/MaxOffsetDist;
  if FTouchMode = ctcmHeadRotation then
  begin
    X :=  FLeverOffset[1] * FxConst;
    Y := -FLeverOffset[0] * FxConst;
    Angle := 1;
  end else
  if FTouchMode = ctcmWalkWithSideRot then
  begin
    Y := -FLeverOffset[0] * FxConst;
    Angle := 1;
  end;
end;

procedure TCastleTouchControl.GetSensorTranslation(var X, Y, Z, Length: Double);
var
  FxConst: Double;
begin
  FxConst := 200/MaxOffsetDist;
  case FTouchMode of
    ctcmWalking:
      begin
        X :=  FLeverOffset[0] * FxConst / 1.5;  { walking to the sides should be slower }
        Z := -FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmWalkWithSideRot:
      begin
        Z := -FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmFlyUpdown:
      begin
        Y := FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmPanXY:
      begin
        X := -FLeverOffset[0] * FxConst;
        Y := -FLeverOffset[1] * FxConst;
        Length := 5;
      end;
  end;
end;

{ TCastleRectangleControl ---------------------------------------------------- }

constructor TCastleRectangleControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
end;

procedure TCastleRectangleControl.SetColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleRectangleControl.Render;
begin
  inherited;
  DrawRectangle(ScreenRect, Color);
end;

function TCastleRectangleControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleRectangleControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleRectangleControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

{ TCastleShape --------------------------------------------------------------- }

constructor TCastleShape.Create(AOwner: TComponent);
begin
  inherited;
  FShapeType := stRectangle;
  FFilled := true;
  FColor := White;
  FOutline := false;
  FOutlineWidth := 1.0;
  FOutlineColor := Black;
end;

procedure TCastleShape.SetShapeType(const Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetFilled(const Value: boolean);
begin
  if FFilled <> Value then
  begin
    FFilled := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutline(const Value: boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineWidth(const Value: Single);
begin
  if FOutlineWidth <> Value then
  begin
    FOutlineWidth := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineThick(const Value: boolean);
begin
  if FOutlineThick <> Value then
  begin
    FOutlineThick := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.Render;
var
  SR: TRectangle;
  OutlineW, OutlineIn, OutlineOut: Integer;
begin
  inherited;
  SR := ScreenRect;

  if Filled and Outline and OutlineThick then
  begin
    { special case when we use OutlineThick drawing mode }

    OutlineW := Ceil(UIScale * OutlineWidth);
    OutlineIn := - OutlineW div 2;
    OutlineOut := OutlineW + OutlineIn;

    case ShapeType of
      stRectangle:
        begin
          DrawRectangle(SR.Grow(OutlineOut), OutlineColor);
          DrawRectangle(SR.Grow(OutlineIn ), Color);
        end;
      stCircle   :
        begin
          DrawCircle(SR.Center, SR.Width div 2 + OutlineOut, SR.Height div 2 + OutlineOut, OutlineColor);
          DrawCircle(SR.Center, SR.Width div 2 + OutlineIn , SR.Height div 2 + OutlineIn , Color);
        end;
      else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
    end;
  end else
  begin
    if Filled then
      case ShapeType of
        stRectangle: DrawRectangle(SR, Color);
        stCircle   : DrawCircle(SR.Center, SR.Width div 2, SR.Height div 2, Color);
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
      end;

    if Outline then
      case ShapeType of
        stRectangle: DrawRectangleOutline(SR, OutlineColor, UIScale * OutlineWidth);
        stCircle   : DrawCircleOutline(SR.Center, SR.Width div 2, SR.Height div 2, OutlineColor, UIScale * OutlineWidth);
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
      end;
  end;
end;

{ TCastleSimpleBackground ---------------------------------------------------- }

constructor TCastleSimpleBackground.Create(AOwner: TComponent);
begin
  inherited;
  FColor := Black;
end;

procedure TCastleSimpleBackground.SetColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleSimpleBackground.Render;
begin
  inherited;
  RenderContext.Clear([cbColor], Color);
end;

{ TDialogScrollArea -------------------------------------------------------------- }

const
  CaretChar = '|';

constructor TCastleDialog.TDialogScrollArea.Create(AOwner: TComponent);
begin
  inherited;
  Dialog := AOwner as TCastleDialog;
  FullSize := true; // we want our ScreenRect to be equal to parent
end;

procedure TCastleDialog.TDialogScrollArea.Render;
type
  TCaretMode = (cmNone, cmVisible, cmInvisible);

  { Render a Text line, and move Y up to the line above. }
  procedure DrawString(X: Integer; var Y: Integer; const Color: TCastleColor;
    Text: string; const TextAlign: THorizontalPosition;
    const Caret: TCaretMode);
  var
    CaretWidth: Integer;
  begin
    if Caret <> cmNone then
      CaretWidth := Dialog.Font.TextWidth(CaretChar) else
      CaretWidth := 0;
    { change X only locally, to take TextAlign into account }
    case TextAlign of
      hpMiddle: X += (Dialog.MaxLineWidth - (Dialog.Font.TextWidth(Text) + CaretWidth)) div 2;
      hpRight : X +=  Dialog.MaxLineWidth - (Dialog.Font.TextWidth(Text) + CaretWidth);
    end;
    if Caret = cmVisible then
      Text := Text + CaretChar;
    Dialog.Font.Print(X, Y, Color, Text);
    { change Y for caller, to print next line higher }
    Y += Dialog.Font.RowHeight;
  end;

  { Render all lines in S, and move Y up to the line above. }
  procedure DrawStrings(const X: Integer; var Y: Integer;
    const Color: TCastleColor; const s: TStrings; TextAlign: THorizontalPosition;
    const AddCaret: boolean);
  const
    CaretSpeed = 1; //< how many blinks per second
  var
    I: Integer;
    Caret: TCaretMode;
  begin
    for i := S.Count - 1 downto 0 do
    begin
      if AddCaret and (I = S.Count - 1) then
      begin
        if FloatModulo(Dialog.LifeTime * CaretSpeed, 1.0) < 0.5 then
          Caret := cmVisible else
          Caret := cmInvisible;
      end else
        Caret := cmNone;
      { each DrawString call will move Y up }
      DrawString(X, Y, Color, s[i], TextAlign, Caret);
    end;
  end;

var
  TextX, TextY: Integer;
  SR: TRectangle;
begin
  inherited;

  SR := ScreenRect; // screen rectangle of ScrollView.ScrollArea
  TextX := SR.Left   + Dialog.BoxMarginScaled;
  TextY := SR.Bottom + Dialog.BoxMarginScaled;

  { draw Broken_InputText and Broken_Text.
    Order matters, as it's drawn from bottom to top. }
  if Dialog.DrawInputText then
    DrawStrings(TextX, TextY, Theme.MessageInputTextColor,
      Dialog.Broken_InputText, Dialog.TextAlign, true);

  { adjust TextX for TRichText.Print call }
  case Dialog.TextAlign of
    hpMiddle: TextX := (SR.Left + SR.Right) div 2;
    hpRight : TextX := SR.Right - Dialog.BoxMarginScaled;
  end;
  Dialog.Broken_Text.Print(TextX, TextY, Theme.MessageTextColor, 0, Dialog.TextAlign);
end;

{ TCastleDialog -------------------------------------------------------------- }

constructor TCastleDialog.Create(AOwner: TComponent);
begin
  inherited;
  { use Theme.MessageFont this way }
  CustomFont := Theme.MessageFont;
  { Contents of Broken_InputText will be initialized in TCastleDialog.UpdateSizes. }
  Broken_InputText := TStringList.Create;
  Anchor(hpMiddle);
  Anchor(vpMiddle);

  ScrollView := TCastleScrollView.Create(Self);
  ScrollView.ScrollArea.InsertFront(TDialogScrollArea.Create(Self));
  ScrollView.EnableDragging := true;
  InsertFront(ScrollView);
end;

procedure TCastleDialog.Initialize(const TextList: TStringList;
  const ATextAlign: THorizontalPosition; const AHtml: boolean;
  const AButtons: array of TCastleButton;
  const ADrawInputText: boolean; const AInputText: string;
  const ABackground: TCastleImage);
var
  I: Integer;
begin
  Text := TextList;
  Background := ABackground;
  if GLInitialized then
    GLBackground := TGLImageCore.Create(Background, true);
  TextAlign := ATextAlign;
  FHtml := AHtml;
  DrawInputText := ADrawInputText;
  FInputText := AInputText;
  SetLength(Buttons, Length(AButtons));
  for I := 0 to High(AButtons) do
  begin
    Buttons[I] := AButtons[I];
    InsertFront(Buttons[I]);
  end;
end;

destructor TCastleDialog.Destroy;
begin
  FreeAndNil(Broken_Text);
  FreeAndNil(Broken_InputText);
  FreeAndNil(Background);
  inherited;
end;

procedure TCastleDialog.GLContextOpen;
begin
  inherited;
  if (GLBackground = nil) and (Background <> nil) then
    GLBackground := TGLImageCore.Create(Background, true);
end;

procedure TCastleDialog.GLContextClose;
begin
  FreeAndNil(GLBackground);
  inherited;
end;

procedure TCastleDialog.SetInputText(const value: string);
begin
  FInputText := value;
  VisibleChange;
  UpdateSizes;
end;

function TCastleDialog.ButtonsHeightScaled: Integer;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxVar(Result, Round(Button.CalculatedHeight * UIScale) + 2 * BoxMarginScaled);
end;

function TCastleDialog.ButtonsHeight: Integer;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxVar(Result, Button.CalculatedHeight + 2 * BoxMargin);
end;

procedure TCastleDialog.Resize;
begin
  inherited;
  UpdateSizes;
end;

procedure TCastleDialog.UpdateSizes;

  { Reposition Buttons. }
  procedure UpdateButtons;
  var
    X, Y, I: Integer;
    Button: TCastleButton;
  begin
    if Length(Buttons) <> 0 then
    begin
      X := -BoxMargin;
      Y :=  BoxMargin;
      for I := Length(Buttons) - 1 downto 0 do
      begin
        Button := Buttons[I];
        Button.Anchor(vpBottom, Y);
        Button.Anchor(hpRight, X);
        X -= Button.CalculatedWidth + ButtonHorizontalMargin;
      end;
    end;
  end;

var
  BreakWidth, ButtonsWidth: integer;
  Button: TCastleButton;
begin
  { calculate BreakWidth, which is the width at which we should break
    our string lists Broken_Xxx. We must here always subtract
    ScrollBarWidthScaled to be on the safe side, because we don't know
    yet is ScrollBarVisible. }
  BreakWidth := Max(0, ParentRect.Width - BoxMarginScaled * 2
    - WindowMarginScaled * 2 - ScrollView.ScrollBarWidthScaled);

  { calculate MaxLineWidth and AllScrolledLinesCount }

  { calculate Broken_Text }
  FreeAndNil(Broken_Text);
  Broken_Text := TRichText.Create(Font, Text, FHtml);
  Broken_Text.Wrap(BreakWidth);
  MaxLineWidth := Broken_Text.Width;
  AllScrolledLinesCount := Broken_Text.Count;

  ButtonsWidth := 0;
  for Button in Buttons do
    ButtonsWidth += Round(Button.CalculatedWidth * UIScale) + ButtonHorizontalMarginScaled;
  if ButtonsWidth > 0 then
    ButtonsWidth -= ButtonHorizontalMarginScaled; // extract margin from last button
  MaxVar(MaxLineWidth, ButtonsWidth);

  if DrawInputText then
  begin
    { calculate Broken_InputText }
    Broken_InputText.Clear;
    Font.BreakLines(InputText, Broken_InputText, BreakWidth);
    { It's our intention that if DrawInputText then *always*
      at least 1 line of InputText (even if it's empty) will be shown.
      That's because InputText is the editable text for the user,
      so there should be indication of "empty line". }
    if Broken_InputText.count = 0 then Broken_InputText.Add('');
    MaxVar(MaxLineWidth, Font.MaxTextWidth(Broken_InputText) + Font.TextWidth(CaretChar));
    AllScrolledLinesCount += Broken_InputText.count;
  end;

  { Now we have MaxLineWidth and AllScrolledLinesCount calculated,
    so our Rect and ScreenRect return valid values. }

  ScrollView.Left := 0;
  ScrollView.Bottom := ButtonsHeight;
  ScrollView.Width := CalculatedWidth;
  ScrollView.Height := CalculatedHeight - ButtonsHeight;

  { add Font.Descend, to be able to see the descend of the bottom line when Scroll is ScrollMax. }
  ScrollView.ScrollArea.Height :=
    Round((Font.RowHeight * AllScrolledLinesCount + Font.Descend) / UIScale) + 2 * BoxMargin;
  ScrollView.ScrollArea.Width := CalculatedWidth;

  UpdateButtons;
end;

procedure TCastleDialog.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  LifeTime += SecondsPassed;
  { when we have input text, we display blinking caret, so keep redrawing }
  if DrawInputText then
    VisibleChange;
end;

procedure TCastleDialog.Render;
begin
  inherited;

  if GLBackground <> nil then
  begin
    GLBackground.Color := Theme.BackgroundTint;
    GLBackground.Draw(ParentRect);
  end;

  Theme.Draw(ScreenRect, tiWindow, UIScale);
end;

function TCastleDialog.CapturesEventsAtPosition(const Position: TVector2Single): boolean;
begin
  Result := true; // always capture
end;

function TCastleDialog.Rect: TRectangle;
var
  PR: TRectangle;
begin
  PR := ParentRect;
  Result := Rectangle(0, 0,
    Min(MaxLineWidth + BoxMarginScaled * 2 + ScrollView.ScrollBarWidthScaled,
      PR.Width  - WindowMarginScaled * 2),
    Min(Font.RowHeight * AllScrolledLinesCount + Font.Descend + BoxMarginScaled * 2 + ButtonsHeightScaled
      { adding here + 2 is a hack to make sure that TCastleScrollView will
        not show scrollbars when not necessary. That's because we set
        ScrollView.ScrollArea.Height using a similar equation as above
        "Font.RowHeight * AllScrolledLinesCount + Font.Descend...",
        but it's in unscaled size (/ UIScale), and sometimes (with wild UIScale
        values) it seems like scrollbars are needed (CalculatedHeight < ScrollArea.CalculatedHeight)
        even though they actually are not.

        Reproducible if you try to resize to small sizes the demo on
        /home/michalis/sources/castle-engine/castle-engine/examples/android/android_demo/game.pas . }
      + 2,
      PR.Height - WindowMarginScaled * 2));
end;

function TCastleDialog.BoxMarginScaled: Integer;
begin
  Result := Round(BoxMargin * UIScale);
end;

function TCastleDialog.WindowMarginScaled: Integer;
begin
  Result := Round(WindowMargin * UIScale);
end;

function TCastleDialog.ButtonHorizontalMarginScaled: Integer;
begin
  Result := Round(ButtonHorizontalMargin * UIScale);
end;

{ TCastleLabel --------------------------------------------------------------- }

constructor TCastleLabel.Create(AOwner: TComponent);
begin
  inherited;
  FText := TStringList.Create;
  FColor := White;
  FFrame := false;
  FFrameColor := White;
  FLineSpacing := DefaultLineSpacing;
  FAutoSize := true;
  ImageType := tiLabel;
  FMaxDisplayChars := -1;
end;

destructor TCastleLabel.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

function TCastleLabel.GetTextToRender: TRichText;
var
  PaddingHorizontalScaled, MaxWidthScaled, WrapWidth: Integer;
  US: Single;
begin
  Result := TRichText.Create(Font, Text, Html);
  if MaxWidth <> 0 then
  begin
    US := UIScale;
    PaddingHorizontalScaled := Round(US * PaddingHorizontal);
    MaxWidthScaled := Round(US * MaxWidth);
    WrapWidth := MaxWidthScaled - 2 * PaddingHorizontalScaled;
    if WrapWidth > 0 then
      Result.Wrap(WrapWidth);
  end;
end;

function TCastleLabel.Rect: TRectangle;
var
  TextToRender: TRichText;
  TextToRenderWidth, TextToRenderCount: Cardinal;
  PaddingHorizontalScaled, PaddingVerticalScaled, LineSpacingScaled: Integer;
  US: Single;
begin
  if AutoSize then
  begin
    if (not Html) and (MaxWidth = 0) then
    begin
      { fast case: no need to use TRichText in this case }
      TextToRenderWidth := Font.MaxTextWidth(Text);
      TextToRenderCount := Text.Count;
    end else
    begin
      TextToRender := GetTextToRender;
      try
        TextToRenderWidth := TextToRender.Width;
        TextToRenderCount := TextToRender.Count;
      finally FreeAndNil(TextToRender) end;
    end;

    US := UIScale;
    PaddingHorizontalScaled := Round(US * (PaddingHorizontal + Padding));
    PaddingVerticalScaled := Round(US * (PaddingVertical + Padding));
    LineSpacingScaled := Round(US * LineSpacing);
    Result := Rectangle(
      LeftBottomScaled,
      TextToRenderWidth + 2 * PaddingHorizontalScaled,
      (Font.RowHeight + LineSpacingScaled) * TextToRenderCount +
        2 * PaddingVerticalScaled + Font.Descend);
  end else
  if FullSize then
    Result := ParentRect else
  begin
    Result := Rectangle(Left, Bottom, Width, Height);
    Result := Result.ScaleAround0(UIScale);
  end;
end;

procedure TCastleLabel.Render;
var
  TextToRender: TRichText;
  LineSpacingScaled: Integer;

  function TextHeight: Integer;
  begin
    Result := TextToRender.Count * (Font.RowHeight + LineSpacingScaled);
  end;

var
  SR: TRectangle;
  TextX, TextBottom, PaddingHorizontalScaled,
    PaddingVerticalScaled: Integer;
  US: Single;
begin
  inherited;
  if Text.Count = 0 then Exit; // early exit in case of easy, fast case

  SR := ScreenRect;
  US := UIScale;
  PaddingHorizontalScaled := Round(US * (PaddingHorizontal + Padding));
  PaddingVerticalScaled := Round(US * (PaddingVertical + Padding));
  LineSpacingScaled := Round(US * LineSpacing);
  if Frame then
    Theme.Draw(SR, ImageType, UIScale, FrameColor);

  { calculate TextX }
  case Alignment of
    hpLeft  : TextX := SR.Left + PaddingHorizontalScaled;
    hpMiddle: TextX := (SR.Left + SR.Right) div 2;
    hpRight : TextX := SR.Right - PaddingHorizontalScaled;
    else raise EInternalError.Create('TCastleLabel.Render: Alignment?');
  end;

  { calculate TextBottom }
  TextBottom := SR.Bottom + PaddingVerticalScaled + Font.Descend;

  if (not Html) and (MaxWidth = 0) and
     (AutoSize or (VerticalAlignment = vpBottom)) and
     (MaxDisplayChars = -1) then
  begin
    { fast case: no need to use TRichText in this case }
    Font.PrintStrings(TextX, TextBottom, Color, Text, false, LineSpacingScaled, Alignment);
  end else
  begin
    TextToRender := GetTextToRender;
    try
      { fix TextBottom, in case of non-trivial VerticalAlignment }
      if not AutoSize then
        case VerticalAlignment of
          vpMiddle: TextBottom := SR.Bottom + (SR.Height - TextHeight) div 2;
          vpTop   : TextBottom := SR.Top - PaddingVerticalScaled - Font.Descend - TextHeight;
        end;

      TextToRender.Print(TextX, TextBottom, Color, LineSpacingScaled, Alignment,
        MaxDisplayChars);
    finally FreeAndNil(TextToRender) end;
  end;
end;

procedure TCastleLabel.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    VisibleChange(true);
  end;
end;

procedure TCastleLabel.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    VisibleChange(true);
  end;
end;

procedure TCastleLabel.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    VisibleChange(true);
  end;
end;

procedure TCastleLabel.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    VisibleChange(true);
  end;
end;

function TCastleLabel.GetCaption: string;
begin
  Result := Text.Text;
end;

procedure TCastleLabel.SetCaption(const Value: string);
begin
  Text.Text := Value;
end;

procedure TCastleLabel.SetAlignment(const Value: THorizontalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    VisibleChange;
  end;
end;

procedure TCastleLabel.SetVerticalAlignment(const Value: TVerticalPosition);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    VisibleChange;
  end;
end;

procedure TCastleLabel.SetMaxDisplayChars(const Value: Integer);
begin
  if FMaxDisplayChars <> Value then
  begin
    FMaxDisplayChars := Value;
    VisibleChange;
  end;
end;

function TCastleLabel.DisplayChars: Cardinal;
var
  TextToRender: TRichText;
begin
  if Text.Count = 0 then Exit(0); // early exit in case of easy, fast case

  TextToRender := GetTextToRender;
  try
    Result := TextToRender.DisplayChars;
  finally FreeAndNil(TextToRender) end;
end;

{ TCastleProgressBar --------------------------------------------------------- }

const
  Dots = '...';

{ Make Text shorter to fit the text width (as rendered using Font)
  inside MaxWidth (in pixels). }
procedure MakeTextFit(var Text: string; const Font: TCastleFont;
  const MaxWidth: Integer);
var
  DotsWidth: Integer;

  { Make Text shorter by preserving first and last words, inserting
    dots inside, and preserving as much as possible text between first and last
    words. }
  function TrimIntelligent: boolean;
  begin
    Result := false;

    { Not implemented for now, not needed. The idea of algorithm below.
      Separator characters are whitespace or / or \. They include slash
      and backslash, to work nicely with URLs and filenames, to show
      the last (usually most relevant) part of URL / filename.

      Find first separator in Text
      if not found, exit false

      Find last separator in Text
      if not found (should not happen) or <= first separator, exit false

      Prefix := Text up to and including first separator
      Suffix := Text suffix, including last separator

      NewWidth := Font.TextWidth(Prefix) + Font.TextWidth(Suffix) + DotsWidth;
      if NewWidth > MaxWidth then exit false

      // We know that we're OK now, using Prefix + ... + Suffix is good.
      // See how many additional characters we can add and still fit in MaxWidth.
      Result := true;
      NextIndex := Length(Prefix) + 1;
      while NextIndex < LastSeparator then
        PotentialPrefix := Prefix + Text[NextIndex]
        PotentialNewWidth := NewWidth + Font.TextWidth(Text[NextIndex])
        if PotentialNewWidth > MaxWidth then Break;
        NewWidth := PotentialNewWidth;
        Prefix := PotentialPrefix;
      end;
      Text := Prefix + Dots + Suffix;
    }
  end;

  { Make Text shorter by taking as long prefix as possible to fit
    the prefix + Dots. }
  procedure TrimSimple;
  var
    NewTextDotsWidth, PotentialNewTextDotsWidth: Integer;
    NewText, PotentialNewText: string;
    C: char;
  begin
    NewText := '';
    NewTextDotsWidth := DotsWidth;
    while Length(NewText) < Length(Text) do
    begin
      C := Text[Length(NewText) + 1];
      PotentialNewText := NewText + C;
      PotentialNewTextDotsWidth := NewTextDotsWidth + Font.TextWidth(C);
      if PotentialNewTextDotsWidth > MaxWidth then Break;
      NewText := PotentialNewText;
      NewTextDotsWidth := PotentialNewTextDotsWidth;
    end;
    Text := NewText + Dots;
  end;

var
  TextWidth: Integer;
begin
  TextWidth := Font.TextWidth(Text);
  if TextWidth <= MaxWidth then
  begin
    { No trimming needs to be done. Add dots at the end, if we have space. }
    if Font.TextWidth(Text + Dots) < MaxWidth then
      Text += Dots;
    Exit;
  end;

  DotsWidth := Font.TextWidth(Dots);

  if not TrimIntelligent then
    TrimSimple;
end;

function TCastleProgressBar.Rect: TRectangle;
var
  XMargin, Height, YMiddle, Bot: Integer;
  PR: TRectangle;
begin
  PR := ParentRect;
  XMargin := PR.Width div 8;
  Height := PR.Height div 12;
  YMiddle := Round(PR.Height * YPosition);
  Bot := YMiddle - Height div 2;
  Result := Rectangle(XMargin, Bot, PR.Width - 2 * XMargin, Height);
end;

procedure TCastleProgressBar.Render;
const
  PaddingHorizontal = 20;
  MinPaddingVertical = 4;
var
  MaxTextWidth: Integer;
  Caption: string;
  BarRect, FillRect: TRectangle;
  HeightForText: Single;
begin
  inherited;

  if Progress = nil then Exit;

  if FGLBackground <> nil then
    FGLBackground.Draw(ParentRect);

  BarRect := Rect;
  Theme.Draw(BarRect, tiProgressBar, UIScale);

  FillRect := BarRect.LeftPart(Round(BarRect.Width * Progress.Position / Progress.Max));
  { it's normal that at the beginning FillRect is too small to be drawn }
  Theme.GLImages[tiProgressFill].IgnoreTooLargeCorners := true;
  Theme.Draw(FillRect, tiProgressFill, UIScale);

  MaxTextWidth := BarRect.Width - PaddingHorizontal;
  Caption := Progress.Title;

  Font.PushProperties;
  HeightForText := BarRect.Height - 2 * MinPaddingVertical;
  if Font.RowHeight > HeightForText then
    Font.Scale := Font.Scale / (Font.RowHeight / HeightForText);
  MakeTextFit(Caption, Font, MaxTextWidth);
  Font.Print(BarRect.Left + PaddingHorizontal,
    BarRect.Bottom + (BarRect.Height - Font.RowHeight) div 2,
    Theme.TextColor, Caption);
  Font.PopProperties;
end;

constructor TCastleProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FYPosition := TProgressUserInterface.DefaultBarYPosition;
end;

destructor TCastleProgressBar.Destroy;
begin
  FreeAndNil(FBackground);
  inherited;
end;

procedure TCastleProgressBar.SetBackground(const Value: TCastleImage);
begin
  if FBackground <> Value then
  begin
    FreeAndNil(FBackground);
    FBackground := Value;

    { Free and optionally recreate FGLBackground.
      We keep assertion that when FBackground is assigned and OpenGL
      context is active => FGLBackground is assigned too. }
    FreeAndNil(FGLBackground);
    if GLInitialized and (FBackground <> nil) then
      FGLBackground := TGLImageCore.Create(FBackground, true);
  end;
end;

procedure TCastleProgressBar.GLContextOpen;
begin
  inherited;
  if (FGLBackground = nil) and (FBackground <> nil) then
    FGLBackground := TGLImageCore.Create(FBackground, true);
end;

procedure TCastleProgressBar.GLContextClose;
begin
  FreeAndNil(FGLBackground);
  inherited;
end;

{ TErrorBackground ------------------------------------------------------ }

procedure TErrorBackground.Render;
begin
  inherited;
  Theme.Draw(ParentRect, tiErrorBackground, UIScale);
end;

{ TCastleAbstractSlider ------------------------------------------------------ }

constructor TCastleAbstractSlider.Create(AOwner: TComponent);
begin
  inherited;
  FDisplayValue := true;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  SmallFont := true;
end;

function TCastleAbstractSlider.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
  // applying UIScale on this is easy...
  Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleAbstractSlider.Render;
begin
  inherited;
  Theme.Draw(ScreenRect, tiSlider, UIScale);
end;

function TCastleAbstractSlider.IndicatorWidth(const R: TRectangle): Integer;
begin
  Result := R.Height div 2 { guess suitable tiSliderPosition width from height };
end;

procedure TCastleAbstractSlider.DrawSliderPosition(const R: TRectangle;
  const Position: Single);
var
  IndicatorW: Integer;
begin
  IndicatorW := IndicatorWidth(R);
  Theme.Draw(Rectangle(
    R.Left +
      Round(MapRangeClamped(Position, 0, 1, IndicatorW div 2, R.Width - IndicatorW div 2))
      - IndicatorW div 2,
    R.Bottom,
    IndicatorW,
    R.Height), tiSliderPosition, UIScale);
end;

function TCastleAbstractSlider.XCoordToSliderPosition(
  const XCoord: Single; const R: TRectangle): Single;
var
  IndicatorW: Integer;
begin
  IndicatorW := IndicatorWidth(R);
  Result := Clamped(MapRange(XCoord,
    R.Left + IndicatorW div 2,
    R.Left + R.Width - IndicatorW div 2, 0, 1), 0, 1);
end;

procedure TCastleAbstractSlider.DrawSliderText(
  const R: TRectangle; Text: string);
begin
  if (Caption <> '') and (Text <> '') then
    Text := Caption + ': ' + Text else
    Text := Caption + Text;
  Font.Print(
    R.Left + (R.Width - Font.TextWidth(Text)) div 2,
    R.Bottom + (R.Height - Font.RowHeight) div 2,
    Black, Text);
end;

procedure TCastleAbstractSlider.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCastleAbstractSlider.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    VisibleChange;
  end;
end;

procedure TCastleAbstractSlider.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    VisibleChange;
  end;
end;

procedure TCastleAbstractSlider.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    VisibleChange;
  end;
end;

procedure TCastleAbstractSlider.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  { left / right arrow keys pressed are already handled by slider }
  if HandleInput and ExclusiveEvents and
     (Container.Pressed[K_Right] or Container.Pressed[K_Left]) then
    HandleInput := false;
end;

{ TCastleFloatSlider --------------------------------------------------------- }

constructor TCastleFloatSlider.Create(AOwner: TComponent);
begin
  inherited;
  FMin := DefaultMin;
  FMax := DefaultMax;
  FValue := FMin;
end;

procedure TCastleFloatSlider.Render;
var
  R: TRectangle;
begin
  inherited;
  R := ScreenRect;
  DrawSliderPosition(R, MapRange(Value, Min, Max, 0, 1));
  if DisplayValue then
    DrawSliderText(R, ValueToStr(Value));
end;

function TCastleFloatSlider.Press(const Event: TInputPressRelease): boolean;

  function ValueChange: Single;
  begin
    Result := (Max - Min) / 100;
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_Right) then
  begin
    if MultipleOf <> 0 then
      Value := CastleUtils.Min(Max, Value + MultipleOf) else
      Value := CastleUtils.Min(Max, Value + ValueChange);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(K_Left) then
  begin
    if MultipleOf <> 0 then
      Value := CastleUtils.Max(Min, Value - MultipleOf) else
      Value := CastleUtils.Max(Min, Value - ValueChange);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsMouseButton(mbLeft) then
  begin
    Value := RoundAndClamp(MapRange(
      XCoordToSliderPosition(Event.Position[0], ScreenRect), 0, 1, Min, Max));
    DoChange;
    Result := ExclusiveEvents;
  end;
end;

function TCastleFloatSlider.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if mbLeft in Event.Pressed then
  begin
    Value := RoundAndClamp(MapRange(
      XCoordToSliderPosition(Event.Position[0], ScreenRect), 0, 1, Min, Max));
    DoChange;
    Result := ExclusiveEvents;
  end;
end;

function TCastleFloatSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('%f', [AValue]);
end;

procedure TCastleFloatSlider.SetMin(const AMin: Single);
begin
  if FMin <> AMin then
  begin
    FMin := AMin;
    VisibleChange;
  end;
end;

procedure TCastleFloatSlider.SetMax(const AMax: Single);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    VisibleChange;
  end;
end;

procedure TCastleFloatSlider.SetValue(const AValue: Single);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    VisibleChange;
  end;
end;

function TCastleFloatSlider.RoundAndClamp(const AValue: Single): Single;
var
  DivResult: Int64;
  M, Remainder: Double;
begin
  if MultipleOf <> 0 then
  begin
    { we use FloatDivMod.
      We have to secure in case AValue or MultipleOf are < 0.
      For MultipleOf it's easy, just always use Abs(MultipleOf). }
    M := Abs(MultipleOf);
    if AValue >= 0 then
    begin
      FloatDivMod(AValue, M, DivResult, Remainder);
      if Remainder < M / 2 then
        Result := M * DivResult else
        Result := M * (DivResult + 1);
    end else
    begin
      FloatDivMod(-AValue, M, DivResult, Remainder);
      if Remainder < M / 2 then
        Result := M * DivResult else
        Result := M * (DivResult + 1);
      Result := -Result;
    end;
  end else
    Result := AValue;

  { Clamp at the end. If Min, Max are not a multiple of MultipleOf - so be it. }
  Result := Clamped(Result, Min, Max);
end;

{ TCastleIntegerSlider ------------------------------------------------------- }

constructor TCastleIntegerSlider.Create(AOwner: TComponent);
begin
  inherited;
  FMin := DefaultMin;
  FMax := DefaultMax;
  FValue := FMin;
end;

procedure TCastleIntegerSlider.Render;
var
  R: TRectangle;
begin
  inherited;
  R := ScreenRect;
  DrawSliderPosition(R, MapRange(Value, Min, Max, 0, 1));
  if DisplayValue then
    DrawSliderText(R, ValueToStr(Value));
end;

function TCastleIntegerSlider.Press(const Event: TInputPressRelease): boolean;
const
  ValueChange = 1;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_Right) then
  begin
    Value := CastleUtils.Min(Max, Value + ValueChange);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(K_Left) then
  begin
    Value := CastleUtils.Max(Min, Value - ValueChange);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsMouseButton(mbLeft) then
  begin
    Value := XCoordToValue(Event.Position[0], ScreenRect);
    DoChange;
    Result := ExclusiveEvents;
  end;
end;

function TCastleIntegerSlider.XCoordToValue(
  const XCoord: Single; const R: TRectangle): Integer;
begin
  { We do additional Clamped over Round result to avoid any
    chance of floating-point errors due to lack of precision. }
  Result := Clamped(Round(
    MapRange(XCoordToSliderPosition(XCoord, R), 0, 1,
      Min, Max)), Min, Max);
end;

function TCastleIntegerSlider.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if mbLeft in Event.Pressed then
  begin
    Value := XCoordToValue(Event.Position[0], ScreenRect);
    DoChange;
    Result := ExclusiveEvents;
  end;
end;

function TCastleIntegerSlider.ValueToStr(const AValue: Integer): string;
begin
  Result := IntToStr(AValue);
end;

procedure TCastleIntegerSlider.SetMin(const AMin: Integer);
begin
  if FMin <> AMin then
  begin
    FMin := AMin;
    VisibleChange;
  end;
end;

procedure TCastleIntegerSlider.SetMax(const AMax: Integer);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    VisibleChange;
  end;
end;

procedure TCastleIntegerSlider.SetValue(const AValue: Integer);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    VisibleChange;
  end;
end;

{ TCastleScrollView ---------------------------------------------------------- }

constructor TCastleScrollView.Create(AOwner: TComponent);
begin
  inherited;

  FKeyScrollSpeed := DefaultKeyScrollSpeed;
  FWheelScrollSpeed := DefaultWheelScrollSpeed;
  FScrollBarWidth := DefaultScrollBarWidth;
  FTintScrollBarInactive := White;

  FScrollArea := TUIControlSizeable.Create(Self);
  FScrollArea.SetSubComponent(true);
  FScrollArea.Name := 'ScrollArea';
  FScrollArea.Anchor(vpTop);
  InsertFront(FScrollArea);

  Scissor := TScissor.Create;
end;

destructor TCastleScrollView.Destroy;
begin
  FreeAndNil(Scissor);
  inherited;
end;

procedure TCastleScrollView.Render;
begin
  inherited;
  Scissor.Rect := ScreenRect;
  Scissor.Enabled := true;
end;

procedure TCastleScrollView.RenderOverChildren;
var
  Color: TCastleColor;
begin
  Scissor.Enabled := false;

  if ScrollBarVisible then
  begin
    Color := Lerp(ScrollbarActive, TintScrollBarInactive, White);
    Theme.Draw(ScrollbarFrame, tiScrollbarFrame, UIScale, Color);
    Theme.Draw(ScrollbarSlider, tiScrollbarSlider, UIScale, Color);
  end;
  inherited;
end;

procedure TCastleScrollView.SetScroll(Value: Single);
begin
  ClampVar(Value, ScrollMin, ScrollMax);
  if FScroll <> Value then
  begin
    FScroll := Value;
    FScrollArea.Anchor(vpTop, Round(FScroll));
  end;
end;

procedure TCastleScrollView.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Calculate ScrollBarVisible, ScrollbarFrame, ScrollbarFrame }
  procedure UpdateScrollBarVisibleAndRects;
  var
    SR: TRectangle;
  begin
    SR := ScreenRect;

    ScrollbarFrame := SR.RightPart(ScrollBarWidthScaled);

    ScrollbarSlider := ScrollbarFrame;
    ScrollbarSlider.Height := CalculatedHeight * SR.Height div ScrollArea.CalculatedHeight;
    ScrollBarVisible := ScrollbarFrame.Height > ScrollbarSlider.Height;
    { equivalent would be to set
        ScrollBarVisible := CalculatedHeight < ScrollArea.CalculatedHeight
      But this way makes it clear that MapRange below is valid, will not divide by zero. }
    if ScrollBarVisible then
      ScrollbarSlider.Bottom += Round(MapRange(Scroll, ScrollMin, ScrollMax,
        ScrollbarFrame.Height - ScrollbarSlider.Height, 0)) else
    begin
      ScrollBarDragging := false;
      Scroll := ScrollMin; // make sure to shift to ScrollMin if scroll suddenly disappears
      DragSpeed := 0;
      TimeSinceDraggingStopped := 0;
      ScrollbarActive := 0;
    end;
  end;

  procedure HandleKeys;
  begin
    if ScrollBarVisible and HandleInput then
    begin
      if Container.Pressed[K_Up  ] then
      begin
        Scroll := Scroll - KeyScrollSpeed * SecondsPassed;
        TimeSinceDraggingStopped := 0;
      end;
      if Container.Pressed[K_Down] then
      begin
        Scroll := Scroll + KeyScrollSpeed * SecondsPassed;
        TimeSinceDraggingStopped := 0;
      end;
      HandleInput := not ExclusiveEvents;
    end;
  end;

  { Make the illusion of "inertial force" when dragging, by gradually
    decelerating dragging speed once user stops dragging.
    Also updates TimeSinceDraggingStopped. }
  procedure DraggingInertialForce;
  const
    DragDecelerationDuration = 0.5;
  var
    CurrentDragSpeed: Single;
  begin
    if ScrollbarVisible then
    begin
      if mbLeft in Container.MousePressed then
      begin
        { note that we update DragSpeed even when DragSinceLastUpdate = 0,
          which means user keeps pressing but doesn't drag }
        if not Zero(SecondsPassed) then
          DragSpeed := DragSinceLastUpdate / SecondsPassed else
          DragSpeed := 0; // whatever sensible value
        TimeSinceDraggingStopped := 0;
      end else
      begin
        TimeSinceDraggingStopped += SecondsPassed;
        if (DragSpeed <> 0) and
           (TimeSinceDraggingStopped < DragDecelerationDuration) then
        begin
          CurrentDragSpeed := MapRange(
            TimeSinceDraggingStopped, 0, DragDecelerationDuration,
            DragSpeed, 0);
          Scroll := Scroll + CurrentDragSpeed * SecondsPassed;
          { stop inertial force if you reached the border of scroll }
          if CurrentDragSpeed > 0 then
          begin
            if Scroll = ScrollMax then TimeSinceDraggingStopped := DragDecelerationDuration;
          end else
          begin
            if Scroll = ScrollMin then TimeSinceDraggingStopped := DragDecelerationDuration;
          end;
        end;
      end;
      DragSinceLastUpdate := 0;
    end;
  end;

  { Update ScrollbarActive, TimeSinceDraggingStarted }
  procedure UpdateScrollBarActive;
  const
    AppearTime = 0.5;
    DisappearTime = 0.5;
  var
    NewScrollbarActive: Single;
  begin
    { update TimeSinceDragginStarted }
    if TimeSinceDraggingStopped = 0 then
    begin
      { dragging now }
      TimeSinceDraggingStarted += SecondsPassed;
      if TimeSinceDraggingStarted > AppearTime then
        NewScrollbarActive := 1 else
        NewScrollbarActive := TimeSinceDraggingStarted / AppearTime;
    end else
    begin
      { not dragging now }
      TimeSinceDraggingStarted := 0;
      if TimeSinceDraggingStopped > DisappearTime then
        NewScrollbarActive := 0 else
        NewScrollbarActive := 1 - TimeSinceDraggingStopped / DisappearTime;
    end;
    if ScrollbarActive <> NewScrollbarActive then
    begin
      ScrollbarActive := NewScrollbarActive;
      VisibleChange;
    end;
  end;

begin
  inherited;
  UpdateScrollBarVisibleAndRects;
  HandleKeys;
  DraggingInertialForce;
  UpdateScrollBarActive;
end;

function TCastleScrollView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  { if not ScrollBarVisible then there is no point in changing Scroll.

    This way we allow TCastleDialog (that uses TCastleScrollView) descendants
    like TCastleKeyMouseDialog to handle K_PageDown, K_PageUp, K_Home and K_End keys
    and mouse wheel. And this is very good for MessageKey,
    when it's used e.g. to allow user to choose any TKey.
    Otherwise MessageKey would not be able to return
    K_PageDown, K_PageUp, etc. keys. }

  if ScrollBarVisible then
    case Event.EventType of
      itKey:
        case Event.Key of
          K_PageUp:   begin Scroll := Scroll - Height; Result := ExclusiveEvents; end;
          K_PageDown: begin Scroll := Scroll + Height; Result := ExclusiveEvents; end;
          K_Home:     begin Scroll := ScrollMin; Result := ExclusiveEvents; end;
          K_End:      begin Scroll := ScrollMax; Result := ExclusiveEvents; end;
        end;
      itMouseButton:
        begin
          if (Event.MouseButton = mbLeft) and ScrollBarVisible and
            ScrollbarFrame.Contains(Container.MousePosition) then
          begin
            if Container.MousePosition[1] < ScrollbarSlider.Bottom then
            begin
              Scroll := Scroll + Height;
              TimeSinceDraggingStopped := 0;
            end else
            if Container.MousePosition[1] >= ScrollbarSlider.Top then
            begin
              Scroll := Scroll - Height;
              TimeSinceDraggingStopped := 0;
            end else
              ScrollBarDragging := true;
            Result := ExclusiveEvents;
          end;
        end;
      itMouseWheel:
        if Event.MouseWheelVertical then
        begin
          Scroll := Scroll - Event.MouseWheelScroll * WheelScrollSpeed;
          TimeSinceDraggingStopped := 0;
          Result := ExclusiveEvents;
        end;
    end;
end;

function TCastleScrollView.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    ScrollBarDragging := false;
    Result := ExclusiveEvents;
  end;
end;

function TCastleScrollView.Motion(const Event: TInputMotion): boolean;
var
  Drag: Single;
begin
  Result := inherited;
  if Result then Exit;

  if ScrollBarDragging then
  begin
    Scroll := Scroll + (Event.OldPosition[1] - Event.Position[1]) /
      ScrollbarFrame.Height * ScrollArea.ScreenRect.Height;
    TimeSinceDraggingStopped := 0;
    Result := ExclusiveEvents;
  end else
  if EnableDragging and (mbLeft in Event.Pressed) then
  begin
    Drag := ((Event.Position[1] - Event.OldPosition[1]) / UIScale);
    Scroll := Scroll + Drag;
    DragSinceLastUpdate += Drag;
    Result := ExclusiveEvents;
  end;
end;

function TCastleScrollView.ScrollMin: Single;
begin
  Result := 0;
end;

function TCastleScrollView.ScrollMax: Single;
begin
  Result := Max(0, ScrollArea.CalculatedHeight - CalculatedHeight);
end;

function TCastleScrollView.ScrollBarWidthScaled: Integer;
begin
  Result := Round(ScrollBarWidth * UIScale);
end;

{ TCastleTheme --------------------------------------------------------------- }

constructor TCastleTheme.Create;
begin
  inherited;
  TooltipTextColor       := Vector4Single(0   , 0, 0, 1);
  TextColor              := Vector4Single(0   , 0, 0, 1);
  DisabledTextColor      := Vector4Single(0.33, 0.33, 0.33, 1);
  MessageInputTextColor  := Vector4Single(0.33, 1, 1, 1);
  MessageTextColor       := Vector4Single(1   , 1, 1, 1);
  BackgroundTint         := Vector4Single(0.25, 0.25, 0.25, 1);
  LoadingBackgroundColor := Black;
  LoadingTextColor       := White;

  FOwnsMessageFont := true;
  FMessageErrorBackground := false;

  FImages[tiPanel] := Panel;
  FCorners[tiPanel] := Vector4Integer(0, 0, 0, 0);
  FImages[tiPanelSeparator] := PanelSeparator;
  FCorners[tiPanelSeparator] := Vector4Integer(0, 0, 0, 0);
  FImages[tiProgressBar] := ProgressBar;
  FCorners[tiProgressBar] := Vector4Integer(7, 7, 7, 7);
  FImages[tiProgressFill] := ProgressFill;
  FCorners[tiProgressFill] := Vector4Integer(1, 1, 1, 1);
  FImages[tiButtonNormal] := ButtonNormal;
  FCorners[tiButtonNormal] := Vector4Integer(2, 2, 2, 2);
  FImages[tiButtonDisabled] := ButtonDisabled;
  FCorners[tiButtonDisabled] := Vector4Integer(2, 2, 2, 2);
  FImages[tiButtonPressed] := ButtonPressed;
  FCorners[tiButtonPressed] := Vector4Integer(2, 2, 2, 2);
  FImages[tiButtonFocused] := ButtonFocused;
  FCorners[tiButtonFocused] := Vector4Integer(2, 2, 2, 2);
  FImages[tiWindow] := WindowDark;
  FCorners[tiWindow] := Vector4Integer(2, 2, 2, 2);
  FImages[tiScrollbarFrame] := ScrollbarFrame;
  FCorners[tiScrollbarFrame] := Vector4Integer(1, 1, 1, 1);
  FImages[tiScrollbarSlider] := ScrollbarSlider;
  FCorners[tiScrollbarSlider] := Vector4Integer(3, 3, 3, 3);
  FImages[tiSlider] := Slider;
  FCorners[tiSlider] := Vector4Integer(4, 7, 4, 7);
  FImages[tiSliderPosition] := SliderPosition;
  FCorners[tiSliderPosition] := Vector4Integer(1, 1, 1, 1);
  FImages[tiLabel] := FrameWhiteBlack;
  FCorners[tiLabel] := Vector4Integer(2, 2, 2, 2);
  FImages[tiActiveFrame] := FrameWhite;
  FCorners[tiActiveFrame] := Vector4Integer(2, 2, 2, 2);
  FImages[tiTooltip] := Tooltip;
  FCorners[tiTooltip] := Vector4Integer(1, 1, 1, 1);
  FImages[tiTouchCtlInner] := TouchCtlInner;
  FCorners[tiTouchCtlInner] := Vector4Integer(0, 0, 0, 0);
  FImages[tiTouchCtlOuter] := TouchCtlOuter;
  FCorners[tiTouchCtlOuter] := Vector4Integer(0, 0, 0, 0);
  FImages[tiTouchCtlFlyInner] := TouchCtlFlyInner;
  FCorners[tiTouchCtlFlyInner] := Vector4Integer(0, 0, 0, 0);
  FImages[tiTouchCtlFlyOuter] := TouchCtlFlyOuter;
  FCorners[tiTouchCtlFlyOuter] := Vector4Integer(0, 0, 0, 0);
  FImages[tiCrosshair1] := Crosshair1;
  FCorners[tiCrosshair1] := Vector4Integer(0, 0, 0, 0);
  FImages[tiCrosshair2] := Crosshair2;
  FCorners[tiCrosshair2] := Vector4Integer(0, 0, 0, 0);
  FImages[tiErrorBackground] := ErrorBackground;
  FCorners[tiErrorBackground] := Vector4Integer(0, 0, 0, 0);
  FImages[tiLoading] := Loading;
  FCorners[tiLoading] := Vector4Integer(0, 0, 0, 0);

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
    FreeAndNil(FGLImages[ImageType]);
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

function TCastleTheme.GetCorners(const ImageType: TThemeImage): TVector4Integer;
begin
  Result := FCorners[ImageType];
end;

procedure TCastleTheme.SetCorners(const ImageType: TThemeImage; const Value: TVector4Integer);
begin
  FCorners[ImageType] := Value;
end;

function TCastleTheme.GetGLImages(const ImageType: TThemeImage): TGLImageCore;
begin
  if FGLImages[ImageType] = nil then
    FGLImages[ImageType] := TGLImageCore.Create(FImages[ImageType], true);
  Result := FGLImages[ImageType];
end;

procedure TCastleTheme.GLContextClose(Sender: TObject);
var
  ImageType: TThemeImage;
begin
  for ImageType in TThemeImage do
    FreeAndNil(FGLImages[ImageType]);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage;
  const UIScale: Single);
begin
  Draw(Rect, ImageType, UIScale, White);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage;
  const UIScale: Single; const Color: TCastleColor);
begin
  GLImages[ImageType].Color := Color;
  GLImages[ImageType].ScaleCorners := UIScale;
  GLImages[ImageType].Draw3x3(Rect, Corners[ImageType]);
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
  MessageInputTextColor := Vector4Single(0, 0.4, 0, 1.0);
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

initialization
  FTheme := TCastleTheme.Create;
finalization
  FreeAndNil(FTheme);
  if DefaultUIFont <> nil then
  begin
    if (FUIFont <> nil) and
       (FUIFont = DefaultUIFont.Normal) then
      FUIFont := nil;
    if (FUIFontSmall <> nil) and
       (FUIFontSmall = DefaultUIFont.Small) then
      FUIFontSmall := nil;
    FreeAndNil(DefaultUIFont);
  end;
end.
