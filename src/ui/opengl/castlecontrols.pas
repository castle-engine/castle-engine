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

uses Classes, GL, CastleVectors, CastleUIControls, CastleGLBitmapFonts,
  CastleKeysMouse, CastleImages, CastleUtils, CastleGLImages;

type
  { Base class for all controls inside an OpenGL context using a font. }
  TUIControlFont = class(TUIControlPos)
  private
    FFont: TGLBitmapFontAbstract;
    FTooltip: string;
  protected
    property Font: TGLBitmapFontAbstract read FFont;
  public
    function TooltipStyle: TUIControlDrawStyle; override;
    procedure DrawTooltip; override;
    procedure GLContextOpen; override;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TUIControl.TooltipStyle and
      TUIControl.DrawTooltip to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;
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
    GLButtonPressed, GLButtonFocused, GLButtonNormal: TGLImage;
    FMinWidth, FMinHeight: Cardinal;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

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
  end;

  { Panel inside OpenGL context.
    Use as a comfortable (and with matching colors) background
    for other controls like buttons and such.
    May be used as a toolbar, together with appropriately placed
    TCastleButton over it. }
  TCastlePanel = class(TUIControlPos)
  private
    FWidth: Cardinal;
    FHeight: Cardinal;
    FVerticalSeparators: TCardinalList;
    GLPanel, GLPanelSeparator: TGLImage;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;

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
    Size is automatically adjusted to the image size.
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.URL property
    or straight TCastleImageControl.Image.

    We automatically use alpha test or alpha blending based
    on loaded image alpha channel (see TGLImage.Alpha). }
  TCastleImageControl = class(TUIControlPos)
  private
    FURL: string;
    FImage: TCastleImage;
    FGLImage: TGLImage;
    procedure SetURL(const Value: string);
    procedure SetImage(const Value: TCastleImage);
  public
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Width: Cardinal;
    function Height: Cardinal;

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
  end;

type
  { Simple background fill. Using OpenGL glClear, so unconditionally
    clears things underneath. In simple cases, you don't want to use this:
    instead you usually have TCastleSceneManager that fill the whole screen,
    and it provides a background already. }
  TCastleSimpleBackground = class(TUIControl)
  private
    FColor: TVector4Single;
  public
    constructor Create(AOwner: TComponent); override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    { Background color. By default, this is black color with opaque (1.0) alpha. }
    property Color: TVector4Single read FColor write FColor;
  end;

  { Theme for controls derived from TUIControl.
    For now it's only useful through the single global instance @link(Theme).

    Many of the 2D GUI is defined through images, represented as TCastleImage.
    Although they all have sensible defaults, you can also change them
    at any time. Simply create TCastleImage instance (e.g. by LoadImage
    function) and assign it here.
    Note that TCastleImage instance is not owned by this class,
    you're responsible for freeing it.
    The alpha channel of the image, if any, is automatically correctly used
    (for alpha test or alpha blending, see TGLImage).

    Together with assigning image, adjust also the XxxCorner property.
    It is used for images rendered using TGLImage.Draw3x3,
    it determines how the image is stretched.
    The corners are specified as 4D vector, order like in CSS: top, right, down,
    left.

    The GLXxx functions expose the TGLImage instances used
    for fast and easy drawing of these images on 2D screen.
    Reading them for the 1st time means that the TGLImage instance is created,
    so use them only when OpenGL context is already active (window is open etc.).
    Changing the TCastleImage instance will automatically free (and recreate
    at next access) the corresponding TGLImage instance. }
  TCastleTheme = class
  private
    FWindow: TCastleImage;
    FWindowCorner: TVector4Integer;
    FGLWindow: TGLImage;
    procedure SetWindow(const Value: TCastleImage);
    procedure GLContextClose;
  public
    TooltipInsideColor: TVector3Byte;
    TooltipBorderColor: TVector3Byte;
    TooltipTextColor  : TVector3Byte;

    TextColor      : TVector3Byte;

    BarEmptyColor : TVector3Byte;
    BarFilledColor: TVector3Byte;

    property Window: TCastleImage read FWindow write SetWindow;
    property WindowCorner: TVector4Integer read FWindowCorner write FWindowCorner;
    function GLWindow: TGLImage;

    constructor Create;
  end;

{ The bitmap fonts used throughout UI interface.

  They work fast. Actually, only the first "create" call does actual work.
  The font is kept until the GL context is destroyed.
  (We used to have reference-counting for this, but actually just keeping
  the resource for the rest of GL context life is 1. easier and 2. better,
  because we want to keep the resource even if you destroy and then recreate
  all your controls.)

  @groupBegin }
function GetUIFont: TGLBitmapFontAbstract;
procedure SetUIFont(const Value: TGLBitmapFontAbstract);

function GetUIFontSmall: TGLBitmapFontAbstract;
procedure SetUIFontSmall(const Value: TGLBitmapFontAbstract);

property UIFont: TGLBitmapFontAbstract read GetUIFont write SetUIFont;
property UIFontSmall: TGLBitmapFontAbstract read GetUIFontSmall write SetUIFontSmall;
{ @groupEnd }

function Theme: TCastleTheme;

procedure Register;

implementation

uses SysUtils, CastleControlsImages, CastleBitmapFont_BVSans_m10,
  CastleBitmapFont_BVSans, CastleGLUtils, Math, CastleColors;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleButton, TCastleImageControl]);
end;

{ Specify vertex at given pixel.

  With standard OpenGL ortho projection,
  glVertex takes coordinates in float [0..Width, 0..Height] range.
  To reliably draw on (0,0) pixel you should pass (0+epsilon, 0+epsilon)
  to glVertex. This procedure takes care of adding this epsilon. }
procedure glVertexPixel(const X, Y: Integer);
begin
  glVertex2f(X + 0.1, Y + 0.1);
end;

{ TUIControlFont ---------------------------------------------------------- }

function TUIControlFont.TooltipStyle: TUIControlDrawStyle;
begin
  if Tooltip <> '' then
    Result := ds2D else
    Result := dsNone;
end;

procedure TUIControlFont.DrawTooltip;
var
  X, Y, W, H: Integer;
begin
  X := Container.TooltipX;
  Y := ContainerHeight - Container.TooltipY;
  W := Font.TextWidth(Tooltip) + 8;
  H := Font.RowHeight + 8;

  { now try to fix X, Y to make tooltip fit inside a window }
  MinTo1st(X, ContainerWidth - W);
  MinTo1st(Y, ContainerHeight - H);
  MaxTo1st(X, 0);
  MaxTo1st(Y, 0);

  Font.PrintStringsBox([Tooltip], false, X, Y, 0,
    Vector4Single(Theme.TooltipInsideColor, 1),
    Vector4Single(Theme.TooltipBorderColor, 1),
    Vector4Single(Theme.TooltipTextColor, 1), 5);
end;

procedure TUIControlFont.GLContextOpen;
begin
  inherited;
  FFont := UIFont;
end;

{ TCastleButton --------------------------------------------------------------- }

const
  ButtonCaptionImageMargin = 10;

constructor TCastleButton.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := true;
  FAutoSizeWidth := true;
  FAutoSizeHeight := true;
  FImageLayout := ilLeft;
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }
end;

destructor TCastleButton.Destroy;
begin
  if OwnsImage then FreeAndNil(FImage);
  FreeAndNil(FGLImage);
  inherited;
end;

function TCastleButton.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleButton.Draw;
var
  TextLeft, TextBottom, ImgLeft, ImgBottom: Integer;
  GLBackground: TGLImage;
begin
  if not GetExists then Exit;

  if Pressed then
    GLBackground := GLButtonPressed else
  if Focused then
    GLBackground := GLButtonFocused else
    GLBackground := GLButtonNormal;

  GLBackground.Draw3x3(Left, Bottom, Width, Height, 2, 2, 2, 2);

  glColorOpacity(Theme.TextColor, 1);

  TextLeft := Left + (Width - TextWidth) div 2;
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilLeft) then
    TextLeft += (FImage.Width + ButtonCaptionImageMargin) div 2 else
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilRight) then
    TextLeft -= (FImage.Width + ButtonCaptionImageMargin) div 2;

  TextBottom := Bottom + (Height - TextHeight) div 2;
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilBottom) then
    TextBottom += (FImage.Height + ButtonCaptionImageMargin) div 2 else
  if (FImage <> nil) and (FGLImage <> nil) and (ImageLayout = ilTop) then
    TextBottom -= (FImage.Height + ButtonCaptionImageMargin) div 2;

  Font.Print(TextLeft, TextBottom, Caption);

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
      ilLeft         : ImgLeft := TextLeft - FImage.Width - ButtonCaptionImageMargin;
      ilRight        : ImgLeft := TextLeft + TextWidth + ButtonCaptionImageMargin;
      ilBottom, ilTop: ImgLeft := Left + (Width - FImage.Width) div 2;
    end;
    case ImageLayout of
      ilBottom       : ImgBottom := TextBottom - FImage.Height - ButtonCaptionImageMargin;
      ilTop          : ImgBottom := TextBottom + TextHeight + ButtonCaptionImageMargin;
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

  if GLButtonPressed = nil then
    GLButtonPressed := TGLImage.Create(Button_pressed, true);
  if GLButtonFocused = nil then
    GLButtonFocused := TGLImage.Create(Button_focused, true);
  if GLButtonNormal = nil then
    GLButtonNormal := TGLImage.Create(Button_normal, true);

  UpdateTextSize;
end;

procedure TCastleButton.GLContextClose;
begin
  FreeAndNil(FGLImage);
  FreeAndNil(GLButtonPressed);
  FreeAndNil(GLButtonFocused);
  FreeAndNil(GLButtonNormal);
  inherited;
end;

function TCastleButton.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

  Result := ExclusiveEvents;
  if not Toggle then FPressed := true;
  ClickStarted := true;
  { We base our Draw on Pressed value. }
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
    { We base our Draw on Pressed value. }
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
          ilLeft, ilRight: FWidth := Width + ImgSize + ButtonCaptionImageMargin;
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
          ilTop, ilBottom: FHeight := Height + ImgSize + ButtonCaptionImageMargin;
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

    { We base our Draw on Pressed and Focused value. }
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

function TCastlePanel.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastlePanel.Draw;
const
  SeparatorMargin = 8;
var
  I: Integer;
begin
  if not GetExists then Exit;

  GLPanel.Draw(Left, Bottom, Width, Height,
    0, 0, GLPanel.Width, GLPanel.Height);

  for I := 0 to VerticalSeparators.Count - 1 do
    GLPanelSeparator.Draw(Left + VerticalSeparators[I], Bottom + SeparatorMargin,
      GLPanelSeparator.Width, Height - 2 * SeparatorMargin,
      0, 0, GLPanelSeparator.Width, GLPanelSeparator.Height);
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

procedure TCastlePanel.GLContextOpen;
begin
  inherited;
  if GLPanel = nil then
    GLPanel := TGLImage.Create(Panel, true);
  if GLPanelSeparator = nil then
    GLPanelSeparator := TGLImage.Create(Panel_separator, true);
end;

procedure TCastlePanel.GLContextClose;
begin
  FreeAndNil(GLPanel);
  FreeAndNil(GLPanelSeparator);
  inherited;
end;

{ TCastleImageControl ---------------------------------------------------------------- }

destructor TCastleImageControl.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.SetURL(const Value: string);
begin
  if Value <> '' then
    Image := LoadImage(Value, []) else
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
      FGLImage := TGLImage.Create(FImage);
  end;
end;

function TCastleImageControl.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists and (FGLImage <> nil) then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleImageControl.Draw;
begin
  if not (GetExists and (FGLImage <> nil)) then Exit;
  FGLImage.Draw(Left, Bottom);
end;

function TCastleImageControl.PositionInside(const X, Y: Integer): boolean;
begin
  Result := GetExists and
    (FImage <> nil) and
    (X >= Left) and
    (X  < Left + FImage.Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + FImage.Height);
end;

procedure TCastleImageControl.GLContextOpen;
begin
  inherited;
  if (FGLImage = nil) and (FImage <> nil) then
    FGLImage := TGLImage.Create(FImage);
end;

procedure TCastleImageControl.GLContextClose;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

function TCastleImageControl.Width: Cardinal;
begin
  if FImage <> nil then
    Result := FImage.Width else
    Result := 0;
end;

function TCastleImageControl.Height: Cardinal;
begin
  if FImage <> nil then
    Result := FImage.Height else
    Result := 0;
end;

{ TCastleSimpleBackground ---------------------------------------------------- }

constructor TCastleSimpleBackground.Create(AOwner: TComponent);
begin
  inherited;
  FColor := Black4Single;
end;

function TCastleSimpleBackground.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    { 3D, because we want to be drawn before other 3D objects }
    Result := ds3D else
    Result := dsNone;
end;

procedure TCastleSimpleBackground.Draw;
begin
  if not GetExists then Exit;
  glPushAttrib(GL_COLOR_BUFFER_BIT);
    glClearColor(Color[0], Color[1], Color[2], Color[3]); // saved by GL_COLOR_BUFFER_BIT
    glClear(GL_COLOR_BUFFER_BIT);
  glPopAttrib;
end;

{ TCastleTheme --------------------------------------------------------------- }

constructor TCastleTheme.Create;
begin
  inherited;
  TooltipInsideColor := Vector3Byte(255, 234, 169);
  TooltipBorderColor := Vector3Byte(157, 133, 105);
  TooltipTextColor   := Vector3Byte(  0,   0,   0);
  TextColor       := Vector3Byte(  0,   0,   0);
  BarEmptyColor  := Vector3Byte(192, 192, 192);
  BarFilledColor := Vector3Byte(Round(0.2 * 255), Round(0.5 * 255), 0);

  Window := WindowDark;
  WindowCorner := Vector4Integer(2, 2, 2, 2);
end;

procedure TCastleTheme.SetWindow(const Value: TCastleImage);
begin
  if FWindow <> Value then
  begin
    FWindow := Value;
    FreeAndNil(FGLWindow);
  end;
end;

function TCastleTheme.GLWindow: TGLImage;
begin
  if FGLWindow = nil then
    FGLWindow := TGLImage.Create(FWindow, true);
  Result := FGLWindow;
end;

procedure TCastleTheme.GLContextClose;
begin
  FreeAndNil(FGLWindow);
end;

var
  FTheme: TCastleTheme;

function Theme: TCastleTheme;
begin
  Result := FTheme;
end;

{ UIFont --------------------------------------------------------------------- }

var
  FUIFont: TGLBitmapFontAbstract;
  FUIFontSmall: TGLBitmapFontAbstract;

function GetUIFont: TGLBitmapFontAbstract;
begin
  if FUIFont = nil then
    FUIFont := TGLBitmapFont.Create(BitmapFont_BVSans);
  Result := FUIFont;
end;

procedure SetUIFont(const Value: TGLBitmapFontAbstract);
begin
  if FUIFont <> Value then
  begin
    FreeAndNil(FUIFont);
    FUIFont := Value;
  end;
end;

function GetUIFontSmall: TGLBitmapFontAbstract;
begin
  if FUIFontSmall = nil then
    FUIFontSmall := TGLBitmapFont.Create(BitmapFont_BVSans_m10);
  Result := FUIFontSmall;
end;

procedure SetUIFontSmall(const Value: TGLBitmapFontAbstract);
begin
  if FUIFontSmall <> Value then
  begin
    FreeAndNil(FUIFontSmall);
    FUIFontSmall := Value;
  end;
end;

procedure WindowClose(const Container: IUIContainer);
begin
  FreeAndNil(FUIFont);
  FreeAndNil(FUIFontSmall);
  if FTheme <> nil then
    FTheme.GLContextClose;
end;

initialization
  OnGLContextClose.Add(@WindowClose);
  FTheme := TCastleTheme.Create;
finalization
  FreeAndNil(FTheme);
end.
