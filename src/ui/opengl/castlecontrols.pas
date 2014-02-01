{
  Copyright 2010-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Controls drawn inside OpenGL context. }
unit CastleControls;

interface

uses Classes, CastleVectors, CastleUIControls, CastleFonts,
  CastleTextureFontData,
  CastleKeysMouse, CastleImages, CastleUtils, CastleGLImages,
  CastleRectangles, CastleColors, CastleProgress;

type
  TCastleLabel = class;

  { Base class for all controls inside an OpenGL context using a font. }
  TUIControlFont = class(TUIRectangularControl)
  private
    FTooltip: string;
    TooltipLabel: TCastleLabel;
    FCustomFont: TCastleFont;
    FOwnsCustomFont: boolean;
    procedure SetCustomFont(const Value: TCastleFont);
  protected
    { Font custom to this control. By default this returns UIFont,
      you can override this to return your font.
      It's OK to return here @nil if font is not ready yet,
      but during Render (when OpenGL context is available) font must be ready. }
    function Font: TCastleFont; virtual;
  public
    destructor Destroy; override;
    procedure GLContextClose; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender; override;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TUIControl.TooltipExists and
      TUIControl.TooltipStyle and TUIControl.TooltipRender
      to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;

    { When non-nil, this font will be used to draw this control.
      Otherwise the default UIFont will be used. }
    property CustomFont: TCastleFont
      read FCustomFont write SetCustomFont;
    property OwnsCustomFont: boolean
      read FOwnsCustomFont write FOwnsCustomFont default false;
  end;

  TCastleButtonImageLayout = (ilTop, ilBottom, ilLeft, ilRight);

  { Button inside OpenGL context.

    This is TUIControl descendant, so to use it just add it to
    the TCastleWindowCustom.Controls or TCastleControlCustom.Controls list.
    You will also usually want to adjust position (TCastleButton.Left,
    TCastleButton.Bottom), TCastleButton.Caption,
    and assign TCastleButton.OnClick (or ovevrride TCastleButton.DoClick). }
  TCastleButton = class(TUIControlFont)
  private
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOnClick: TNotifyEvent;
    FCaption: string;
    FAutoSize, FAutoSizeWidth, FAutoSizeHeight: boolean;
    TextWidth, TextHeight: Cardinal;
    FPressed: boolean;
    FOwnsImage: boolean;
    FImage: TCastleImage;
    FGLImage: TGLImage;
    FToggle: boolean;
    ClickStarted: boolean;
    FMinImageWidth: Cardinal;
    FMinImageHeight: Cardinal;
    FImageLayout: TCastleButtonImageLayout;
    FImageAlphaTest: boolean;
    FMinWidth, FMinHeight: Cardinal;
    FImageMargin: Cardinal;
    procedure SetCaption(const Value: string);
    procedure SetAutoSize(const Value: boolean);
    procedure SetAutoSizeWidth(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: boolean);
    { Calculate TextWidth, TextHeight and call UpdateSize. }
    procedure UpdateTextSize;
    { If AutoSize, update Width, Height.
      This depends on Caption, AutoSize*, Font availability. }
    procedure UpdateSize;
    procedure SetImage(const Value: TCastleImage);
    procedure SetPressed(const Value: boolean);
    procedure SetImageLayout(const Value: TCastleButtonImageLayout);
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetMinWidth(const Value: Cardinal);
    procedure SetMinHeight(const Value: Cardinal);
    procedure SetImageMargin(const Value: Cardinal);
  public
    const
      DefaultImageMargin = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
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
  published
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;

    { When AutoSize is @true (the default) then Width/Height are automatically
      adjusted when you change the Caption and @link(Image).
      They take into account
      Caption width/height with current font, @link(Image) width/height,
      and add some margin to make it look good.

      To be more precise, Width is adjusted only when AutoSize and AutoSizeWidth.
      And Height is adjusted only when AutoSize and AutoSizeHeight.
      This way you can turn off auto-sizing in only one dimension if you
      want (and when you don't need such flexibility, leave
      AutoSizeWidth = AutoSizeHeight = @true and control both by simple
      AutoSize).

      Note that this adjustment happens only when OpenGL context is initialized
      (because only then we actually know the font used).
      So don't depend on Width/Height values calculated correctly before
      OpenGL context is ready. }
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

    property ImageMargin: Cardinal read FImageMargin write SetImageMargin
      default DefaultImageMargin;
  end;

  { Panel inside OpenGL context.
    Use as a comfortable (and with matching colors) background
    for other controls like buttons and such.
    May be used as a toolbar, together with appropriately placed
    TCastleButton over it. }
  TCastlePanel = class(TUIRectangularControl)
  private
    FWidth: Cardinal;
    FHeight: Cardinal;
    FVerticalSeparators: TCardinalList;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    function Rect: TRectangle; override;

    { Separator lines drawn on panel. Useful if you want to visually separate
      groups of contols (like a groups of buttons when you use
      this panel as a toolbar).

      Values are the horizontal positions of the separators (with respect
      to this panel @link(Left)). Width of the separator is in SeparatorSize. }
    property VerticalSeparators: TCardinalList read FVerticalSeparators;
    class function SeparatorSize: Cardinal;
  published
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;
  end;

  { Image control inside OpenGL context.
    Size is automatically adjusted to the image size, if Stretch is @false (default).
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.URL property
    or straight TCastleImageControl.Image.

    We automatically use alpha test or alpha blending based
    on loaded image alpha channel (see TGLImage.Alpha).
    You can influence this by @link(AlphaChannel) property. }
  TCastleImageControl = class(TUIRectangularControl)
  private
    FURL: string;
    FImage: TCastleImage;
    FGLImage: TGLImage;
    FAlphaChannel: TAutoAlphaChannel;
    FStretch: boolean;
    FFullSize: boolean;
    FWidth: Cardinal;
    FHeight: Cardinal;
    procedure SetURL(const Value: string);
    procedure SetImage(const Value: TCastleImage);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
    procedure SetStretch(const Value: boolean);
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetFullSize(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Rect: TRectangle; override;

    { Image displayed, or @nil if none.
      This image is owned by this component. If you set this property
      to your custom TCastleImage instance you should
      leave memory management of this instance to this component.
      If necessary, you can always create a copy by TCastleImage.MakeCopy
      if you want to give here only a copy. }
    property Image: TCastleImage read FImage write SetImage;
  published
    { URL of the image. Setting this also sets @link(Image).
      Set this to '' to clear the image. }
    property URL: string read FURL write SetURL;
    { Deprecated name for @link(URL). }
    property FileName: string read FURL write SetURL; deprecated;
    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      determine how the alpha of image is treated (opaque, alpha test,
      alpha blending). Set this to force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel
      read FAlphaChannel write SetAlphaChannel default acAuto;
    { Deprecated, use more flexible AlphaChannel instead. }
    property Blending: boolean read GetBlending write SetBlending stored false; deprecated;

    { Size of the image control.

      If Stretch = @false, then values you manually set for Width, Height, FullSize
      properties do not matter (they are still remembered though,
      so you can set properties in any order).
      The displayed size (you can check it through @link(Rect) function)
      always corresponds to the underlying image size.
      The Left and Bottom properties work as usual, they allow you to move the control.

      If Stretch = @true, then the image will be stretched
      (without paying attention to the aspect ratio) to fill the requested area.
      If Stretch = @true and FullSize = @true then values of Width,
      Height, Left, Bottom do not matter:
      image always fills the whole container
      (@link(Rect) corresponds to the container area).
      If Stretch = @true and FullSize = @false,
      you have full manual control over the image size and position.

      Note that you can always look at @link(Rect) value to know
      the current calculated size and position of the image control on screen.

      @groupBegin }
    property Stretch: boolean read FStretch write SetStretch default false;
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;
    property FullSize: boolean read FFullSize write SetFullSize default false;
    { @groupEnd }
  end;

  TCastleTouchCtlMode = (ctcmWalking, ctcmWalkWithSideRot, ctcmHeadRotation,
                         ctcmFlyUpdown, ctcmPanXY);
  TCastleTouchPosition = (tpManual, tpLeft, tpRight);

  { Control for touch interfaces. Shows one "lever", that can be moved
    up/down/left/right, and controls the movement while Walking or Flying. }
  TCastleTouchControl = class(TUIRectangularControl)
  private
    FTouchMode: TCastleTouchCtlMode;
    FLeverOffsetX: Integer;
    FLeverOffsetY: Integer;
    FDragStarted: boolean;
    FPosition: TCastleTouchPosition;
    function SizeScale: Single;
    procedure SetPosition(const Value: TCastleTouchPosition);
    { Update Left and Bottom based on Position and current Container size. }
    procedure UpdateLeftBottom;
  public
    procedure Render; override;
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;

    { Size of this control, ignoring GetExists. }
    function Width: Cardinal;
    function Height: Cardinal;
    function Rect: TRectangle; override;

    function PositionInside(const X, Y: Integer): boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    procedure SetTouchMode(const Value: TCastleTouchCtlMode);
    procedure GetSensorRotation(var X, Y, Z, Angle: Double);
    procedure GetSensorTranslation(var X, Y, Z, Length: Double);
  private
    function MaxOffsetDist: Integer;
  published
    property TouchMode: TCastleTouchCtlMode
      read FTouchMode write SetTouchMode default ctcmWalking;
    property Position: TCastleTouchPosition
      read FPosition write SetPosition default tpManual;
  end;

  { Simple background fill. Using OpenGL GLClear, so unconditionally
    clears things underneath. In simple cases, you don't want to use this:
    instead you usually have TCastleSceneManager that fills the whole screen,
    and it provides a background already. }
  TCastleSimpleBackground = class(TUIControl)
  private
    FColor: TCastleColor;
  public
    constructor Create(AOwner: TComponent); override;
    function RenderStyle: TRenderStyle; override;
    procedure Render; override;
    { Background color. By default, this is black color with opaque alpha. }
    property Color: TCastleColor read FColor write FColor;
  end;

  { Text alignment for TCastleDialog. }
  TTextAlign = (taLeft, taMiddle, taRight);

  { Dialog box that can display a long text, with automatic vertical scrollbar.
    You can also add buttons at the bottom.
    You can also have an input text area.
    This can be used to make either a modal or non-modal dialog boxes.

    See CastleMessages for routines that intensively use this dialog underneath,
    giving you easy MessageXxx routines that ask user for confirmation and such. }
  TCastleDialog = class abstract(TUIControl)
  strict private
    const
      BoxMargin = 10;
      WindowMargin = 10;
      ScrollBarWholeWidth = 20;
      ButtonHorizontalMargin = 10;
    var
    FScroll: Single;
    FInputText: string;

    { Broken Text. }
    Broken_Text: TStringList;
    { Ignored (not visible) if not DrawInputText.
      Else broken InputText. }
    Broken_InputText: TStringList;

    MaxLineWidth: integer;
    { Sum of all Broken_Text.Count + Broken_InputText.Count.
      In other words, all lines that are scrolled by the scrollbar. }
    AllScrolledLinesCount: integer;
    VisibleScrolledLinesCount: integer;

    { Min and max sensible values for @link(Scroll). }
    ScrollMin, ScrollMax: integer;
    ScrollInitialized: boolean;

    ScrollMaxForScrollbar: integer;

    ScrollbarVisible: boolean;
    ScrollbarFrame: TRectangle;
    ScrollbarSlider: TRectangle;
    ScrollBarDragging: boolean;

    { Things below set in MessageCore, readonly afterwards. }
    { Main text to display. Read-only contents. }
    Text: TStringList;
    { Drawn as window background. @nil means there is no background
      (use only if there is always some other 2D control underneath TCastleDialog).
      When assigned, stretched to cover whole screen. }
    Background: TGLImage;
    Align: TTextAlign;
    { Should we display InputText }
    DrawInputText: boolean;
    Buttons: array of TCastleButton;

    procedure SetScroll(Value: Single);
    { How many pixels up should be move the text.
      Kept as a float, to allow smooth time-based changes.
      Note that setting Scroll always clamps the value to sensible range. }
    property Scroll: Single read FScroll write SetScroll;
    procedure ScrollPageDown;
    procedure ScrollPageUp;

    procedure SetInputText(const value: string);

    { Calculate height in pixels needed to draw Buttons.
      Returns 0 if there are no Buttons = ''. }
    function ButtonsHeight: Integer;
    procedure UpdateSizes;
    { The whole rectangle where we draw dialog box. }
    function WholeMessageRect: TRectangle;
    { If ScrollBarVisible, ScrollBarWholeWidth. Else 0. }
    function RealScrollBarWholeWidth: Integer;
    function Font: TTextureFont;
  public
    { Set this to @true to signal that modal dialog window should be closed.
      This is not magically handled --- if you implement a modal dialog box,
      you should check in your loop whether something set Answered to @true. }
    Answered: boolean;

    { Input text. Displayed only if DrawInputText. }
    property InputText: string read FInputText write SetInputText;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign display stuff. Call this before adding control to Controls list. }
    procedure Initialize(
      const TextList: TStringList; const ATextAlign: TTextAlign;
      const AButtons: array of TCastleButton;
      const ADrawInputText: boolean; const AInputText: string;
      const ABackground: TCastleImage);
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;
    procedure GLContextClose; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function PositionInside(const X, Y: Integer): boolean; override;
  end;

  TThemeImage = (
    tiPanel, tiPanelSeparator, tiProgressBar, tiProgressFill,
    tiButtonPressed, tiButtonFocused, tiButtonNormal,
    tiWindow, tiScrollbarFrame, tiScrollbarSlider,
    tiSlider, tiSliderPosition, tiLabel, tiActiveFrame, tiTooltip,
    tiTouchCtlInner, tiTouchCtlOuter, tiTouchCtlFlyInner, tiTouchCtlFlyOuter);

  { Label with possibly multiline text, in a box. }
  TCastleLabel = class(TUIControlFont)
  private
    FText: TStrings;
    FPadding: Integer;
    FLineSpacing: Integer;
    FColor: TCastleColor;
    FTags: boolean;
  protected
    ImageType: TThemeImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Rect: TRectangle; override;

    { Text color. By default it's white. }
    property Color: TCastleColor read FColor write FColor;
  published
    property Text: TStrings read FText;

    { Inside the label box, padding between rect borders and text. }
    property Padding: Integer read FPadding write FPadding default 0;

    { Extra spacing between lines (may also be negative to squeeze lines
      tighter). }
    property LineSpacing: Integer read FLineSpacing write FLineSpacing default 0;

    { Does the text use HTML-like tags. This is very limited for now,
      see TCastleFont.PrintStrings documentation. }
    property Tags: boolean read FTags write FTags default false;
  end;

  TCastleProgressBar = class(TUIControl)
  private
    { Background image. }
    FBackground: TCastleImage;
    FGLBackground: TGLImage;
    FYPosition: Single;
    FProgress: TProgress;
    procedure SetBackground(const Value: TCastleImage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Rect: TRectangle;

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

  { Theme for 2D GUI controls.
    Should only be used through the single global instance @link(Theme). }
  TCastleTheme = class
  private
    FImages: array [TThemeImage] of TCastleImage;
    FCorners: array [TThemeImage] of TVector4Integer;
    FGLImages: array [TThemeImage] of TGLImage;
    FOwnsImages: array [TThemeImage] of boolean;
    FMessageFont: TTextureFontData;
    FGLMessageFont: TTextureFont;
    function GetImages(const ImageType: TThemeImage): TCastleImage;
    procedure SetImages(const ImageType: TThemeImage; const Value: TCastleImage);
    function GetOwnsImages(const ImageType: TThemeImage): boolean;
    procedure SetOwnsImages(const ImageType: TThemeImage; const Value: boolean);
    function GetCorners(const ImageType: TThemeImage): TVector4Integer;
    procedure SetCorners(const ImageType: TThemeImage; const Value: TVector4Integer);
    function GetGLImages(const ImageType: TThemeImage): TGLImage;
    procedure GLContextClose;
    { TGLImage instances for fast and easy drawing of images on 2D screen.
      Reading them for the 1st time means that the TGLImage instance is created,
      so use them only when OpenGL context is already active (window is open etc.).
      Changing the TCastleImage instance will automatically free (and recreate
      at next access) the corresponding TGLImage instance. }
    property GLImages[const ImageType: TThemeImage]: TGLImage read GetGLImages;
    procedure SetMessageFont(const Value: TTextureFontData);
  public
    TooltipTextColor: TCastleColor;
    TextColor: TCastleColor;
    MessageTextColor: TCastleColor;
    MessageInputTextColor: TCastleColor;

    BarEmptyColor: TVector3Byte;
    BarFilledColor: TVector3Byte;

    constructor Create;
    destructor Destroy; override;

    { 2D GUI images, represented as TCastleImage.
      Although they all have sensible defaults, you can also change them
      at any time. Simply create TCastleImage instance (e.g. by LoadImage
      function) and assign it here. Be sure to adjust also @link(OwnsImages)
      if you want the theme to automatically free the image when it's no longer
      used.

      The alpha channel of the image, if any, is automatically correctly used
      (for alpha test or alpha blending, see TGLImage). }
    property Images[const ImageType: TThemeImage]: TCastleImage read GetImages write SetImages;

    property OwnsImages[const ImageType: TThemeImage]: boolean read GetOwnsImages write SetOwnsImages;

    { Corners that determine how image on @link(Images) is stretched when
      drawing by @link(TCastleTheme.Draw) method.
      Together with assigning @link(Images), adjust also this property.
      It is used for images rendered using TGLImage.Draw3x3,
      it determines how the image is stretched.
      The corners are specified as 4D vector, order like in CSS: top, right, down,
      left. }
    property Corners[const ImageType: TThemeImage]: TVector4Integer read GetCorners write SetCorners;

    { Draw the selected theme image on screen.
      If you do not specify a color, white will be used, so image will be displayed
      as-is. Specifying a color means that image will be multiplied by it,
      just like for @link(TGLImage.Color). }
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage);
    procedure Draw(const Rect: TRectangle; const ImageType: TThemeImage;
      const Color: TCastleColor);

    { Font used by dialogs.
      Note that it doesn't have to be mono-spaced. }
    property MessageFont: TTextureFontData read FMessageFont write SetMessageFont;
    function GLMessageFont: TTextureFont;
  end;

{ The 2D fonts used throughout UI interface.

  They work fast. Actually, only the first "create" call does actual work.
  The font is kept until the GL context is destroyed.
  (We used to have reference-counting for this, but actually just keeping
  the resource for the rest of GL context life is 1. easier and 2. better,
  because we want to keep the resource even if you destroy and then recreate
  all your controls.)

  @groupBegin }
function GetUIFont: TCastleFont;
procedure SetUIFont(const Value: TCastleFont);

function GetUIFontSmall: TCastleFont;
procedure SetUIFontSmall(const Value: TCastleFont);

property UIFont: TCastleFont read GetUIFont write SetUIFont;
property UIFontSmall: TCastleFont read GetUIFontSmall write SetUIFontSmall;
{ @groupEnd }

function Theme: TCastleTheme;

procedure Register;

implementation

uses SysUtils, Math, CastleControlsImages, CastleTextureFont_DejaVuSans_20,
  CastleTextureFont_DejaVuSans_10, CastleTextureFont_DejaVuSansMono_18,
  CastleGLUtils;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleButton, TCastleImageControl]);
end;

{ TUIControlFont ---------------------------------------------------------- }

destructor TUIControlFont.Destroy;
begin
  CustomFont := nil; // make sure to free FCustomFont, if necessary
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
    { we know that GL context now exists, so just directly call GLContextOpen }
    TooltipLabel.GLContextOpen;
  end;

  { assign TooltipLabel.Text first, to get TooltipRect.Width/Height }
  TooltipLabel.Padding := 5;
  TooltipLabel.Color := Theme.TooltipTextColor;
  TooltipLabel.Text.Clear;
  TooltipLabel.Text.Append(Tooltip);
  TooltipRect := TooltipLabel.Rect;

  X := Container.TooltipX;
  Y := ContainerHeight - Container.TooltipY;

  { now try to fix X, Y to make tooltip fit inside a window }
  MinTo1st(X, ContainerWidth - TooltipRect.Width);
  MinTo1st(Y, ContainerHeight - TooltipRect.Height);
  MaxTo1st(X, 0);
  MaxTo1st(Y, 0);

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
  if CustomFont <> nil then
    CustomFont.GLContextClose;
  inherited;
end;

function TUIControlFont.Font: TCastleFont;
begin
  if GLInitialized then
  begin
    if CustomFont <> nil then
      Result := CustomFont else
      Result := UIFont;
  end else
    Result := nil;
end;

procedure TUIControlFont.SetCustomFont(const Value: TCastleFont);
begin
  if FCustomFont <> Value then
  begin
    if OwnsCustomFont then
      FreeAndNil(FCustomFont) else
      FCustomFont := nil;
    FCustomFont := Value;
  end;
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
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }
end;

destructor TCastleButton.Destroy;
begin
  if OwnsImage then FreeAndNil(FImage);
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleButton.Render;
var
  TextLeft, TextBottom, ImgLeft, ImgBottom: Integer;
  Background: TThemeImage;
begin
  if Pressed then
    Background := tiButtonPressed else
  if Focused then
    Background := tiButtonFocused else
    Background := tiButtonNormal;
  Theme.Draw(Rect, Background);

  TextLeft := Left + (Width - TextWidth) div 2;
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilLeft) then
    TextLeft += (FImage.Width + ImageMargin) div 2 else
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilRight) then
    TextLeft -= (FImage.Width + ImageMargin) div 2;

  TextBottom := Bottom + (Height - TextHeight) div 2;
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilBottom) then
    TextBottom += (FImage.Height + ImageMargin) div 2 else
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilTop) then
    TextBottom -= (FImage.Height + ImageMargin) div 2;

  Font.Print(TextLeft, TextBottom, Theme.TextColor, Caption);

  if (FImage <> nil) and (FGLImage <> nil) then
  begin
    { update FGLImage.Alpha based on ImageAlphaTest }
    if FImage.HasAlpha then
    begin
      if ImageAlphaTest then
        FGLImage.Alpha := acSimpleYesNo else
        FGLImage.Alpha := acFullRange;
    end;
    case ImageLayout of
      ilLeft         : ImgLeft := TextLeft - FImage.Width - ImageMargin;
      ilRight        : ImgLeft := TextLeft + TextWidth + ImageMargin;
      ilBottom, ilTop: ImgLeft := Left + (Width - FImage.Width) div 2;
    end;
    case ImageLayout of
      ilBottom       : ImgBottom := TextBottom - FImage.Height - ImageMargin;
      ilTop          : ImgBottom := TextBottom + TextHeight + ImageMargin;
      ilLeft, ilRight: ImgBottom := Bottom + (Height - FImage.Height) div 2;
    end;
    FGLImage.Draw(ImgLeft, ImgBottom);
  end;
end;

function TCastleButton.PositionInside(const X, Y: Integer): boolean;
begin
  Result := GetExists and
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

procedure TCastleButton.GLContextOpen;
begin
  inherited;
  if (FGLImage = nil) and (FImage <> nil) then
    FGLImage := TGLImage.Create(FImage);
  UpdateTextSize;
end;

procedure TCastleButton.GLContextClose;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

function TCastleButton.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

  Result := ExclusiveEvents;
  if not Toggle then FPressed := true;
  ClickStarted := true;
  { We base our Render on Pressed value. }
  VisibleChange;
end;

function TCastleButton.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

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

      (Larger UI toolkits have also the concept of "capturing",
      that a Focused control with Pressed captures remaining
      mouse/key events even when mouse goes out. This allows the user
      to move mouse out from the control, and still go back to make mouse up
      and make "click". }
    DoClick;
  end;
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

procedure TCastleButton.UpdateTextSize;
begin
  if Font <> nil then
  begin
    TextWidth := Font.TextWidth(Caption);
    TextHeight := Font.RowHeightBase;
    UpdateSize;
  end;
end;

procedure TCastleButton.UpdateSize;
const
  HorizontalMargin = 10;
  VerticalMargin = 10;
var
  ImgSize: Cardinal;
begin
  if AutoSize then
  begin
    { We modify FWidth, FHeight directly,
      to avoid causing UpdateFocusAndMouseCursor too many times.
      We'll call it at the end explicitly. }
    if AutoSizeWidth then FWidth := TextWidth + HorizontalMargin * 2;
    if AutoSizeHeight then FHeight := TextHeight + VerticalMargin * 2;
    if (FImage <> nil) or
       (MinImageWidth <> 0) or
       (MinImageHeight <> 0) then
    begin
      if AutoSizeWidth then
      begin
        if FImage <> nil then
          ImgSize := Max(FImage.Width, MinImageWidth) else
          ImgSize := MinImageWidth;
        case ImageLayout of
          ilLeft, ilRight: FWidth := Width + ImgSize + ImageMargin;
          ilTop, ilBottom: FWidth := Max(Width, ImgSize + HorizontalMargin * 2);
        end;
      end;
      if AutoSizeHeight then
      begin
        if FImage <> nil then
          ImgSize := Max(FImage.Height, MinImageHeight) else
          ImgSize := MinImageHeight;
        case ImageLayout of
          ilLeft, ilRight: FHeight := Max(Height, ImgSize + VerticalMargin * 2);
          ilTop, ilBottom: FHeight := Height + ImgSize + ImageMargin;
        end;
      end;
    end;

    { at the end apply MinXxx properties }
    if AutoSizeWidth then
      MaxTo1st(FWidth, MinWidth);
    if AutoSizeHeight then
      MaxTo1st(FHeight, MinHeight);

    if (AutoSizeWidth or AutoSizeHeight) and (Container <> nil) then
      Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleButton.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FreeAndNil(FGLImage);

    FImage := Value;

    if GLInitialized and (FImage <> nil) then
      FGLImage := TGLImage.Create(FImage);

    UpdateSize;
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
    UpdateSize;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleButton.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleButton.SetMinWidth(const Value: Cardinal);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    UpdateSize;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetMinHeight(const Value: Cardinal);
begin
  if FMinHeight <> Value then
  begin
    FMinHeight := Value;
    UpdateSize;
    VisibleChange;
  end;
end;

procedure TCastleButton.SetImageMargin(const Value: Cardinal);
begin
  if FImageMargin <> Value then
  begin
    FImageMargin := Value;
    UpdateSize;
    VisibleChange;
  end;
end;

function TCastleButton.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
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
  I: Integer;
begin
  Theme.Draw(Rect, tiPanel);

  for I := 0 to VerticalSeparators.Count - 1 do
    Theme.Draw(Rectangle(
      Left + VerticalSeparators[I], Bottom + SeparatorMargin,
      Theme.Images[tiPanelSeparator].Width, Height - 2 * SeparatorMargin),
      tiPanelSeparator);
end;

function TCastlePanel.PositionInside(const X, Y: Integer): boolean;
begin
  Result := GetExists and
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

class function TCastlePanel.SeparatorSize: Cardinal;
begin
  Result := 2;
end;

procedure TCastlePanel.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastlePanel.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

function TCastlePanel.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
end;

{ TCastleImageControl ---------------------------------------------------------------- }

constructor TCastleImageControl.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCastleImageControl.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FGLImage);
  inherited;
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

procedure TCastleImageControl.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    FreeAndNil(FImage);
    FreeAndNil(FGLImage);

    FImage := Value;
    if GLInitialized and (FImage <> nil) then
    begin
      FGLImage := TGLImage.Create(FImage, true);
      if AlphaChannel <> acAuto then
        FGLImage.Alpha := AlphaChannel;
    end;
  end;
end;

procedure TCastleImageControl.Render;
begin
  if FGLImage = nil then Exit;
  FGLImage.Draw(Rect);
end;

function TCastleImageControl.PositionInside(const X, Y: Integer): boolean;
var
  R: TRectangle;
begin
  R := Rect;
  Result := GetExists and
    ( FullSize or
      ( (X >= R.Left) and
        (X  < R.Left + R.Width) and
        (R.Height - Y >= R.Bottom) and
        (R.Height - Y  < R.Bottom + R.Height)
      )
    );
end;

function TCastleImageControl.Rect: TRectangle;
begin
  if not Stretch then
  begin
    if FImage <> nil then
      Result := Rectangle(Left, Bottom, FImage.Width, FImage.Height) else
      Result := Rectangle(Left, Bottom, 0, 0);
  end else
  if FullSize then
    Result := ContainerRect else
    Result := Rectangle(Left, Bottom, Width, Height);
end;

procedure TCastleImageControl.GLContextOpen;
begin
  inherited;
  if (FGLImage = nil) and (FImage <> nil) then
  begin
    FGLImage := TGLImage.Create(FImage, true);
    if AlphaChannel <> acAuto then
      FGLImage.Alpha := AlphaChannel;
  end;
end;

procedure TCastleImageControl.GLContextClose;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  if FAlphaChannel <> Value then
  Begin
    FAlphaChannel := Value;
    if FGLImage <> nil then
    begin
      { update FGLImage.Alpha }
      if AlphaChannel <> acAuto then
        FGLImage.Alpha := AlphaChannel else
        FGLImage.Alpha := FImage.AlphaChannel;
    end;
  end;
end;

function TCastleImageControl.GetBlending: boolean;
begin
  Result := AlphaChannel <> acFullRange;
end;

procedure TCastleImageControl.SetBlending(const Value: boolean);
begin
  if Value then
    AlphaChannel := acFullRange else
    AlphaChannel := acSimpleYesNo;
end;

procedure TCastleImageControl.SetStretch(const Value: boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    VisibleChange;
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

{ TCastleTouchControl ---------------------------------------------------------------- }

function TCastleTouchControl.SizeScale: Single;
begin
  if Container <> nil then
    Result := Container.Dpi / 96 else
    Result := 1;
end;

function TCastleTouchControl.Width: Cardinal;
begin
  Result := Round(Theme.Images[tiTouchCtlOuter].Width  * SizeScale);
end;

function TCastleTouchControl.Height: Cardinal;
begin
  Result := Round(Theme.Images[tiTouchCtlOuter].Height * SizeScale);
end;

function TCastleTouchControl.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
end;

procedure TCastleTouchControl.SetPosition(const Value: TCastleTouchPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if GLInitialized then UpdateLeftBottom;
  end;
end;

procedure TCastleTouchControl.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  UpdateLeftBottom;
end;

procedure TCastleTouchControl.UpdateLeftBottom;
var
  CtlBorder: Integer;
begin
  CtlBorder := Round(24 * Container.Dpi / 96);
  case Position of
    tpLeft:
      begin
        Left := CtlBorder;
        Bottom := CtlBorder;
        VisibleChange;
      end;
    tpRight:
      begin
        Left := ContainerWidth - Width - CtlBorder;
        Bottom := CtlBorder;
        VisibleChange;
      end;
  end;
end;

function TCastleTouchControl.PositionInside(const X, Y: Integer): boolean;
begin
  Result := GetExists and Rect.Contains(X, ContainerHeight - Y);
end;

function TCastleTouchControl.MaxOffsetDist: Integer;
begin
  Result := Round(SizeScale *
    (Theme.Images[tiTouchCtlOuter].Width -
     Theme.Images[tiTouchCtlInner].Width) / 2);
end;

procedure TCastleTouchControl.Render;
var
  LevOffsetTrimmedX, LevOffsetTrimmedY, MaxDist: Integer;
  LeverDist: Double;
  InnerRect: TRectangle;
  ImageInner, ImageOuter: TThemeImage;
begin
  if FTouchMode = ctcmFlyUpdown then
  begin
    ImageInner := tiTouchCtlFlyInner;
    ImageOuter := tiTouchCtlFlyOuter;
  end
  else
  begin
    ImageInner := tiTouchCtlInner;
    ImageOuter := tiTouchCtlOuter;
  end;
  Theme.Draw(Rect, ImageOuter);

  // compute lever offset (must not move outside outer ring)
  LeverDist := sqrt(FLeverOffsetX*FLeverOffsetX + FLeverOffsetY*FLeverOffsetY);
  MaxDist := MaxOffsetDist();
  if LeverDist <= MaxDist then
  begin
    LevOffsetTrimmedX := FLeverOffsetX;
    LevOffsetTrimmedY := FLeverOffsetY;
  end
  else begin
    LevOffsetTrimmedX := Floor((FLeverOffsetX*MaxDist)/LeverDist);
    LevOffsetTrimmedY := Floor((FLeverOffsetY*MaxDist)/LeverDist);
  end;
  if FTouchMode = ctcmFlyUpdown then LevOffsetTrimmedX := 0;

  // draw lever
  InnerRect := Theme.Images[ImageInner].Rect; // rectangle at (0,0)
  InnerRect.Width  := Round(InnerRect.Width  * SizeScale);
  InnerRect.Height := Round(InnerRect.Height * SizeScale);
  InnerRect.Left   := Left   + (Width  - InnerRect.Width ) div 2 + LevOffsetTrimmedX;
  InnerRect.Bottom := Bottom + (Height - InnerRect.Height) div 2 - LevOffsetTrimmedY;
  Theme.Draw(InnerRect, ImageInner);
end;

procedure TCastleTouchControl.SetTouchMode(const Value: TCastleTouchCtlMode);
begin
  FTouchMode := Value;
  { we may swap outer image depending on the TouchMode in some later version }
end;

function TCastleTouchControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

  Result := ExclusiveEvents;
  FDragStarted := true;
  FLeverOffsetX := 0;
  FLeverOffsetY := 0;
end;

function TCastleTouchControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

  if FDragStarted then
  begin
    Result := ExclusiveEvents;

    FDragStarted := false;
    FLeverOffsetX := 0;
    FLeverOffsetY := 0;
    VisibleChange;   { repaint with lever back in the center }
  end;
end;

function TCastleTouchControl.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  if FDragStarted then
  begin
    FLeverOffsetX := FLeverOffsetX + NewX - OldX;
    FLeverOffsetY := FLeverOffsetY + NewY - OldY;
    VisibleChange;
  end;
  Result := FDragStarted;
end;

procedure TCastleTouchControl.GetSensorRotation(var X, Y, Z, Angle: Double);
var
  FxConst: Double;
begin
  FxConst := 10/MaxOffsetDist();
  if FTouchMode = ctcmHeadRotation then
  begin
    X := -FLeverOffsetY*FxConst;
    Y := -FLeverOffsetX*FxConst;
    Angle := 1;
  end
  else if FTouchMode = ctcmWalkWithSideRot then
  begin
    Y := -FLeverOffsetX*FxConst;
    Angle := 1;
  end;
end;

procedure TCastleTouchControl.GetSensorTranslation(var X, Y, Z, Length: Double);
var
  FxConst: Double;
begin
  FxConst := 200/MaxOffsetDist();
  if FTouchMode = ctcmWalking then
  begin
    X := FLeverOffsetX*FxConst/1.5;  { walking to the sides should be slower }
    Z := FLeverOffsetY*FxConst;
    Length := 20;
  end
  else if FTouchMode = ctcmWalkWithSideRot then
  begin
    Z := FLeverOffsetY*FxConst;
    Length := 20;
  end
  else if FTouchMode = ctcmFlyUpdown then
  begin
    Y := -FLeverOffsetY*FxConst;
    Length := 20;
  end
  else if FTouchMode = ctcmPanXY then
  begin
    X := -FLeverOffsetX*FxConst;
    Y := FLeverOffsetY*FxConst;
    Length := 20;
  end;
end;

{ TCastleSimpleBackground ---------------------------------------------------- }

constructor TCastleSimpleBackground.Create(AOwner: TComponent);
begin
  inherited;
  FColor := Black;
end;

function TCastleSimpleBackground.RenderStyle: TRenderStyle;
begin
  { 3D, because we want to be drawn before other 3D objects }
  Result := rs3D;
end;

procedure TCastleSimpleBackground.Render;
begin
  GLClear([cbColor], Color);
end;

{ TCastleDialog -------------------------------------------------------------- }

constructor TCastleDialog.Create(AOwner: TComponent);
begin
  inherited;
  { Contents of Broken_xxx will be initialized in TCastleDialog.UpdateSizes. }
  Broken_Text := TStringList.Create;
  Broken_InputText := TStringList.Create;
end;

procedure TCastleDialog.Initialize(const TextList: TStringList;
  const ATextAlign: TTextAlign; const AButtons: array of TCastleButton;
  const ADrawInputText: boolean; const AInputText: string;
  const ABackground: TCastleImage);
var
  I: Integer;
begin
  Text := TextList;
  Background := TGLImage.Create(ABackground, true);
  Align := ATextAlign;
  DrawInputText := ADrawInputText;
  FInputText := AInputText;
  SetLength(Buttons, Length(AButtons));
  for I := 0 to High(AButtons) do
    Buttons[I] := AButtons[I];
end;

destructor TCastleDialog.Destroy;
begin
  FreeAndNil(Broken_Text);
  FreeAndNil(Broken_InputText);
  inherited;
end;

procedure TCastleDialog.GLContextClose;
begin
  FreeAndNil(Background);
  inherited;
end;

procedure TCastleDialog.SetScroll(Value: Single);
begin
  Clamp(Value, ScrollMin, ScrollMax);
  if Value <> Scroll then
  begin
    FScroll := Value;
    VisibleChange;
  end;
end;

procedure TCastleDialog.ScrollPageDown;
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  Scroll := Scroll + PageHeight;
end;

procedure TCastleDialog.ScrollPageUp;
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  Scroll := Scroll - PageHeight;
end;

procedure TCastleDialog.SetInputText(const value: string);
begin
  FInputText := value;
  VisibleChange;
  UpdateSizes;
end;

function TCastleDialog.ButtonsHeight: Integer;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxTo1st(Result, Button.Height + 2 * BoxMargin);
end;

procedure TCastleDialog.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
var
  MessageRect: TRectangle;
  X, Y, I: Integer;
  Button: TCastleButton;
begin
  inherited;
  UpdateSizes;

  { Reposition Buttons. }
  if Length(Buttons) <> 0 then
  begin
    MessageRect := WholeMessageRect;
    X := MessageRect.Right  - BoxMargin;
    Y := MessageRect.Bottom + BoxMargin;
    for I := Length(Buttons) - 1 downto 0 do
    begin
      Button := Buttons[I];
      X -= Button.Width;
      Button.Left := X;
      Button.Bottom := Y;
      X -= ButtonHorizontalMargin;
    end;
  end;
end;

procedure TCastleDialog.UpdateSizes;
var
  BreakWidth, ButtonsWidth: integer;
  WindowScrolledHeight: Integer;
  Button: TCastleButton;
begin
  { calculate BreakWidth, which is the width at which we should break
    our string lists Broken_Xxx. We must here always subtract
    ScrollBarWholeWidth to be on the safe side, because we don't know
    yet is ScrollBarVisible. }
  BreakWidth := Max(0, ContainerWidth - BoxMargin * 2
    - WindowMargin * 2 - ScrollBarWholeWidth);

  { calculate MaxLineWidth and AllScrolledLinesCount }

  { calculate Broken_Text }
  Broken_Text.Clear;
  font.BreakLines(Text, Broken_Text,  BreakWidth);
  MaxLineWidth := font.MaxTextWidth(Broken_Text);
  AllScrolledLinesCount := Broken_Text.count;

  ButtonsWidth := 0;
  for Button in Buttons do
    ButtonsWidth += Button.Width + ButtonHorizontalMargin;
  if ButtonsWidth > 0 then
    ButtonsWidth -= ButtonHorizontalMargin; // extract margin from last button
  MaxTo1st(MaxLineWidth, ButtonsWidth);

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
    MaxLineWidth := max(MaxLineWidth, font.MaxTextWidth(Broken_InputText));
    AllScrolledLinesCount += Broken_InputText.count;
  end;

  { Now we have MaxLineWidth and AllScrolledLinesCount calculated }

  { Calculate WindowScrolledHeight --- number of pixels that are controlled
    by the scrollbar. }
  WindowScrolledHeight := ContainerHeight - BoxMargin * 2
    - WindowMargin * 2 - ButtonsHeight;

  { calculate VisibleScrolledLinesCount, ScrollBarVisible }

  VisibleScrolledLinesCount := Clamped(WindowScrolledHeight div Font.RowHeight,
    0, AllScrolledLinesCount);
  ScrollBarVisible := VisibleScrolledLinesCount < AllScrolledLinesCount;
  { if ScrollBarVisible changed from true to false then we must make
    sure that ScrollBarDragging is false. }
  if not ScrollBarVisible then
    ScrollBarDragging := false;

  { Note that when not ScrollBarVisible,
    then VisibleScrolledLinesCount = AllScrolledLinesCount,
    then ScrollMin = 0
    so ScrollMin = ScrollMax,
    so Scroll will always be 0. }
  ScrollMin := -Font.RowHeight *
    (AllScrolledLinesCount - VisibleScrolledLinesCount);
  { ScrollMax jest stale ale to nic; wszystko bedziemy pisac
    tak jakby ScrollMax tez moglo sie zmieniac - byc moze kiedys zrobimy
    z tej mozliwosci uzytek. }
  ScrollMax := 0;
  ScrollMaxForScrollbar := ScrollMin + Font.RowHeight * AllScrolledLinesCount;

  if ScrollInitialized then
    { This clamps Scroll to proper range }
    Scroll := Scroll else
  begin
    { Need to initalize Scroll, otherwise default Scroll = 0 means were
      at the bottom of the text. }
    Scroll := ScrollMin;
    ScrollInitialized := true;
  end;
end;

function TCastleDialog.Press(const Event: TInputPressRelease): boolean;
var
  MY: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  { if not ScrollBarVisible then there is no point in changing Scroll
    (because always ScrollMin = ScrollMax = Scroll = 0).

    This way we allow descendants like TCastleKeyMouseDialog
    to handle K_PageDown, K_PageUp, K_Home and K_End keys
    and mouse wheel. And this is very good for MessageKey,
    when it's used e.g. to allow user to choose any TKey.
    Otherwise MessageKey would not be able to return
    K_PageDown, K_PageUp, etc. keys. }

  if ScrollBarVisible then
    case Event.EventType of
      itKey:
        case Event.Key of
          K_PageUp:   begin ScrollPageUp;        Result := true; end;
          K_PageDown: begin ScrollPageDown;      Result := true; end;
          K_Home:     begin Scroll := ScrollMin; Result := true; end;
          K_End:      begin Scroll := ScrollMax; Result := true; end;
        end;
      itMouseButton:
        begin
          MY := ContainerHeight - Container.MouseY;
          if (Event.MouseButton = mbLeft) and ScrollBarVisible and
            ScrollbarFrame.Contains(Container.MouseX, MY) then
          begin
            if MY < ScrollbarSlider.Bottom then
              ScrollPageDown else
            if MY >= ScrollbarSlider.Top then
              ScrollPageUp else
              ScrollBarDragging := true;
            Result := true;
          end;
        end;
      itMouseWheel:
        if Event.MouseWheelVertical then
        begin
          Scroll := Scroll - Event.MouseWheelScroll * Font.RowHeight;
          Result := true;
        end;
    end;
end;

function TCastleDialog.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    ScrollBarDragging := false;
    Result := true;
  end;
end;

function TCastleDialog.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  Result := ScrollBarDragging;
  if Result then
    Scroll := Scroll + (NewY- OldY) / ScrollbarFrame.Height *
      (ScrollMaxForScrollbar - ScrollMin);
end;

procedure TCastleDialog.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  function Factor: Single;
  begin
    result := 200.0 * SecondsPassed;
    if mkCtrl in Container.Pressed.Modifiers then result *= 6;
  end;

begin
  inherited;

  if HandleInput then
  begin
    if Container.Pressed[K_Up  ] then Scroll := Scroll - Factor;
    if Container.Pressed[K_Down] then Scroll := Scroll + Factor;
    HandleInput := not ExclusiveEvents;
  end;
end;

procedure TCastleDialog.Render;

  { Render a Text line, and move Y up to the line above. }
  procedure DrawString(X: Integer; var Y: Integer; const Color: TCastleColor;
    const text: string; TextAlign: TTextAlign);
  begin
    { change X only locally, to take TextAlign into account }
    case TextAlign of
      taMiddle: X += (MaxLineWidth - font.TextWidth(text)) div 2;
      taRight : X +=  MaxLineWidth - font.TextWidth(text);
    end;
    Font.Print(X, Y, Color, text);
    { change Y for caller, to print next line higher }
    Y += font.RowHeight;
  end;

  { Render all lines in S, and move Y up to the line above. }
  procedure DrawStrings(const X: Integer; var Y: Integer;
    const Color: TCastleColor; const s: TStrings; TextAlign: TTextAlign);
  var
    i: integer;
  begin
    for i := s.count-1 downto 0 do
      { each DrawString call will move Y up }
      DrawString(X, Y, Color, s[i], TextAlign);
  end;

var
  MessageRect: TRectangle;
  { InnerRect to okienko w ktorym mieszcza sie napisy,
    a wiec WholeMessageRect zmniejszony o BoxMargin we wszystkich kierunkach
    i z ew. obcieta prawa czescia przeznaczona na ScrollbarFrame. }
  InnerRect: TRectangle;
  ScrollBarLength: integer;
  TextX, TextY: Integer;
const
  { odleglosc paska ScrollBara od krawedzi swojego waskiego recta
    (prawa krawedz jest zarazem krawedzia duzego recta !) }
  ScrollBarMargin = 2;
  { szerokosc paska ScrollBara }
  ScrollBarInternalWidth = ScrollBarWholeWidth - ScrollBarMargin * 2;
begin
  inherited;

  if Background <> nil then
    Background.Draw(ContainerRect);

  MessageRect := WholeMessageRect;
  Theme.Draw(MessageRect, tiWindow);

  MessageRect := MessageRect.RemoveBottom(ButtonsHeight);

  { calculate InnerRect }
  InnerRect := MessageRect.Grow(-BoxMargin);
  InnerRect.Width -= RealScrollBarWholeWidth;

  { draw scrollbar, and calculate it's rectangles }
  if ScrollBarVisible then
  begin
    ScrollbarFrame := MessageRect.RightPart(ScrollBarWholeWidth).
      RemoveRight(Theme.Corners[tiWindow][1]).
      RemoveTop(Theme.Corners[tiWindow][0]);
    Theme.Draw(ScrollbarFrame, tiScrollbarFrame);

    ScrollBarLength := MessageRect.Height - ScrollBarMargin*2;
    ScrollbarSlider := ScrollbarFrame;
    ScrollbarSlider.Height := VisibleScrolledLinesCount * ScrollBarLength
      div AllScrolledLinesCount;
    ScrollbarSlider.Bottom += Round(MapRange(Scroll,
      ScrollMin, ScrollMax, ScrollbarFrame.Height - ScrollbarSlider.Height, 0));
    Theme.Draw(ScrollbarSlider, tiScrollbarSlider);
  end else
  begin
    ScrollbarFrame := TRectangle.Empty;
    ScrollbarSlider := TRectangle.Empty;
  end;

  { Make scissor to cut off text that is too far up/down.
    We add bottom height Font.Descend, to see the descend of
    the bottom line (which is below InnerRect.Bottom, and would not be
    ever visible otherwise). }
  ScissorEnable(InnerRect.GrowBottom(Font.Descend));

  TextX := InnerRect.Left;
  TextY := InnerRect.Bottom + Round(Scroll);

  { draw Broken_InputText and Broken_Text.
    Order matters, as it's drawn from bottom to top. }
  if DrawInputText then
    DrawStrings(TextX, TextY, Theme.MessageInputTextColor, Broken_InputText, Align);
  DrawStrings(TextX, TextY, Theme.MessageTextColor, Broken_Text, Align);

  ScissorDisable;
end;

function TCastleDialog.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

function TCastleDialog.RealScrollBarWholeWidth: Integer;
begin
  Result := Iff(ScrollBarVisible, ScrollBarWholeWidth, 0);
end;

function TCastleDialog.WholeMessageRect: TRectangle;
begin
  Result := Rectangle(0, 0, ContainerWidth, ContainerHeight).Center(
    Min(MaxLineWidth + BoxMargin * 2 + RealScrollBarWholeWidth,
      ContainerWidth  - WindowMargin * 2),
    Min(AllScrolledLinesCount * Font.RowHeight + BoxMargin * 2 + ButtonsHeight,
      ContainerHeight - WindowMargin * 2));
end;

function TCastleDialog.Font: TTextureFont;
begin
  Result := Theme.GLMessageFont;
end;

{ TCastleLabel --------------------------------------------------------------- }

constructor TCastleLabel.Create(AOwner: TComponent);
begin
  inherited;
  FText := TStringList.Create;
  FColor := White;
  ImageType := tiLabel;
end;

destructor TCastleLabel.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

function TCastleLabel.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom,
    Font.MaxTextWidth(Text, Tags) + 2 * Padding,
    (Font.RowHeight + LineSpacing) * Text.Count + 2 * Padding + Font.Descend);
end;

procedure TCastleLabel.Render;
var
  R: TRectangle;
begin
  if Text.Count = 0 then Exit;
  R := Rect;
  Theme.Draw(Rect, ImageType);
  Font.PrintStrings(R.Left + Padding,
    R.Bottom + Padding + Font.Descend, Color, Text, Tags, LineSpacing);
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
  XMargin, Height, YMiddle, Bottom: Integer;
begin
  XMargin := ContainerWidth div 8;
  Height := ContainerHeight div 12;
  YMiddle := Round(ContainerHeight * YPosition);
  Bottom := YMiddle - Height div 2;
  Result := Rectangle(XMargin, Bottom, ContainerWidth - 2 * XMargin, Height);
end;

procedure TCastleProgressBar.Render;
const
  Padding = 20;
var
  MaxTextWidth: Integer;
  Font: TCastleFont;
  Caption: string;
  BarRect, FillRect: TRectangle;
begin
  if Progress = nil then Exit;

  if FGLBackground <> nil then
    FGLBackground.Draw(ContainerRect);

  BarRect := Rect;
  Theme.Draw(BarRect, tiProgressBar);

  FillRect := BarRect.LeftPart(Round(BarRect.Width * Progress.Position / Progress.Max));
  { it's normal that at the beginning FillRect is too small to be drawn }
  Theme.GLImages[tiProgressFill].IgnoreTooLargeCorners := true;
  Theme.Draw(FillRect, tiProgressFill);

  MaxTextWidth := BarRect.Width - Padding;
  Caption := Progress.Title;
  if (UIFont.RowHeight < BarRect.Height) and
     (UIFont.TextWidth(Caption) < MaxTextWidth) then
  begin
    Font := UIFont;
    if UIFont.TextWidth(Caption + Dots) < MaxTextWidth then
      Caption += Dots;
  end else
  begin
    Font := UIFontSmall;
    MakeTextFit(Caption, Font, MaxTextWidth);
  end;
  Font.Print(BarRect.Left + Padding,
    BarRect.Bottom + (BarRect.Height - Font.RowHeight) div 2,
    Theme.TextColor, Caption);
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
      FGLBackground := TGLImage.Create(FBackground, true);
  end;
end;

procedure TCastleProgressBar.GLContextOpen;
begin
  inherited;
  if (FGLBackground = nil) and (FBackground <> nil) then
    FGLBackground := TGLImage.Create(FBackground, true);
end;

procedure TCastleProgressBar.GLContextClose;
begin
  FreeAndNil(FGLBackground);
  inherited;
end;

{ TCastleTheme --------------------------------------------------------------- }

constructor TCastleTheme.Create;
begin
  inherited;
  TooltipTextColor      := Vector4Single(0   , 0, 0, 1);
  TextColor             := Vector4Single(0   , 0, 0, 1);
  MessageInputTextColor := Vector4Single(0.33, 1, 1, 1);
  MessageTextColor      := Vector4Single(1   , 1, 1, 1);

  MessageFont := TextureFont_DejaVuSansMono_18;

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
  FImages[tiButtonPressed] := ButtonPressed;
  FCorners[tiButtonPressed] := Vector4Integer(2, 2, 2, 2);
  FImages[tiButtonFocused] := ButtonFocused;
  FCorners[tiButtonFocused] := Vector4Integer(2, 2, 2, 2);
  FImages[tiWindow] := WindowDark;
  FCorners[tiWindow] := Vector4Integer(2, 2, 2, 2);
  FImages[tiScrollbarFrame] := ScrollbarFrame;
  FCorners[tiScrollbarFrame] := Vector4Integer(1, 1, 1, 1);
  FImages[tiScrollbarSlider] := ScrollbarSlider;
  FCorners[tiScrollbarSlider] := Vector4Integer(2, 2, 2, 2);
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
end;

destructor TCastleTheme.Destroy;
var
  I: TThemeImage;
begin
  for I in TThemeImage do
    if FOwnsImages[I] then
      FreeAndNil(FImages[I]) else
      FImages[I] := nil;
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

function TCastleTheme.GetGLImages(const ImageType: TThemeImage): TGLImage;
begin
  if FGLImages[ImageType] = nil then
    FGLImages[ImageType] := TGLImage.Create(FImages[ImageType], true);
  Result := FGLImages[ImageType];
end;

procedure TCastleTheme.GLContextClose;
var
  ImageType: TThemeImage;
begin
  for ImageType in TThemeImage do
    FreeAndNil(FGLImages[ImageType]);
  FreeAndNil(FGLMessageFont);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage);
begin
  Draw(Rect, ImageType, White);
end;

procedure TCastleTheme.Draw(const Rect: TRectangle; const ImageType: TThemeImage;
  const Color: TCastleColor);
begin
  GLImages[ImageType].Color := Color;
  GLImages[ImageType].Draw3x3(Rect, Corners[ImageType]);
end;

procedure TCastleTheme.SetMessageFont(const Value: TTextureFontData);
begin
  if FMessageFont <> Value then
  begin
    FMessageFont := Value;
    FreeAndNil(FGLMessageFont);
  end;
end;

function TCastleTheme.GLMessageFont: TTextureFont;
begin
  if FGLMessageFont = nil then
    FGLMessageFont := TTextureFont.Create(FMessageFont);
  Result := FGLMessageFont;
end;

var
  FTheme: TCastleTheme;

function Theme: TCastleTheme;
begin
  Result := FTheme;
end;

{ UIFont --------------------------------------------------------------------- }

var
  FUIFont: TCastleFont;
  FUIFontSmall: TCastleFont;

function GetUIFont: TCastleFont;
begin
  if FUIFont = nil then
    FUIFont := TTextureFont.Create(TextureFont_DejaVuSans_20);
  Result := FUIFont;
end;

procedure SetUIFont(const Value: TCastleFont);
begin
  if FUIFont <> Value then
  begin
    FreeAndNil(FUIFont);
    FUIFont := Value;
  end;
end;

function GetUIFontSmall: TCastleFont;
begin
  if FUIFontSmall = nil then
    FUIFontSmall := TTextureFont.Create(TextureFont_DejaVuSans_10);
  Result := FUIFontSmall;
end;

procedure SetUIFontSmall(const Value: TCastleFont);
begin
  if FUIFontSmall <> Value then
  begin
    FreeAndNil(FUIFontSmall);
    FUIFontSmall := Value;
  end;
end;

procedure ContextClose;
begin
  if FUIFont <> nil then
    FUIFont.GLContextClose;
  if FUIFontSmall <> nil then
    FUIFontSmall.GLContextClose;
  if FTheme <> nil then
    FTheme.GLContextClose;
end;

initialization
  OnGLContextClose.Add(@ContextClose);
  FTheme := TCastleTheme.Create;
finalization
  FreeAndNil(FTheme);
  FreeAndNil(FUIFont);
  FreeAndNil(FUIFontSmall);
end.
