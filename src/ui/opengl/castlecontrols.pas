{
  Copyright 2010-2012 Michalis Kamburelis.

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

uses Classes, GL, VectorMath, UIControls, OpenGLFonts,
  KeysMouse, Images, CastleUtils;

type
  { Base class for all controls inside an OpenGL context using a font. }
  TUIControlFont = class(TUIControlPos)
  private
    FFont: TGLBitmapFont_Abstract;
    FTooltip: string;
  protected
    property Font: TGLBitmapFont_Abstract read FFont;
  public
    function TooltipStyle: TUIControlDrawStyle; override;
    procedure DrawTooltip; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
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
    FImage: TImage;
    FGLImage: TGLuint;
    FToggle: boolean;
    ClickStarted: boolean;
    FOpacity: Single;
    FMinImageWidth: Cardinal;
    FMinImageHeight: Cardinal;
    FImageLayout: TCastleButtonImageLayout;
    FImageAlphaTest: boolean;
    procedure SetCaption(const Value: string);
    procedure SetAutoSize(const Value: boolean);
    procedure SetAutoSizeWidth(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: boolean);
    { Calculate TextWidth, TextHeight and call UpdateSize. }
    procedure UpdateTextSize;
    { If AutoSize, update Width, Height.
      This depends on Caption, AutoSize*, Font availability. }
    procedure UpdateSize;
    procedure SetImage(const Value: TImage);
    procedure SetPressed(const Value: boolean);
    procedure SetImageLayout(const Value: TCastleButtonImageLayout);
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
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { Called when user clicks the button. In this class, simply calls
      OnClick callback. }
    procedure DoClick; virtual;
    procedure SetFocused(const Value: boolean); override;
    { Set this to non-nil to display an image on the button. }
    property Image: TImage read FImage write SetImage;
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

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read FCaption write SetCaption;

    { Can the button be permanently pressed. Good for making a button
      behave like a checkbox, that is indicate a boolean state.
      When @link(Toggle) is @true, you can set the @link(Pressed) property,
      and the clicks are visualized a little different. }
    property Toggle: boolean read FToggle write FToggle default false;

    { Is the button pressed down. If @link(Toggle) is @true,
      you can read and write this property to set the pressed state.

      When not @link(Toggle), this property isn't really useful to you.
      The pressed state is automatically managed then to visualize
      user clicks. In this case, you can read this property,
      but you cannot reliably set it. }
    property Pressed: boolean read FPressed write SetPressed default false;

    { Opacity (1 - transparency) with which control is drawn.
      When this is < 1, we draw control with nice blending.
      Make sure you have some background control underneath in this case. }
    property Opacity: Single read FOpacity write FOpacity default 1.0;

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
    FOpacity: Single;
    FVerticalSeparators: TCardinalList;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

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

    { Opacity (1 - transparency) with which control is drawn.
      When this is < 1, we draw control with nice blending.
      Make sure you have some background control underneath in this case. }
    property Opacity: Single read FOpacity write FOpacity default 1.0;
  end;

  { Image control inside OpenGL context.
    Size is automatically adjusted to the image size.
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.FileName property. }
  TCastleImageControl = class(TUIControlPos)
  private
    FFileName: string;
    FImage: TImage;
    FGLImage: TGLuint;
    FBlending: boolean;
    procedure SetFileName(const Value: string);
  public
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  published
    property FileName: string read FFileName write SetFileName;
    { Set to @true to draw image with blending. This is suitable for images
      that (may) have nice alpha channel. }
    property Blending: boolean read FBlending write FBlending default false;
  end;

{ Create and destroy the default bitmap font used throughout UI interface.

  They work fast. Actually, only the first "create" call does actual work,
  following calls only increment an internal counter.
  Destroy decreases the counter and only frees the resources when the counter
  is zero.

  Destroying @nil is allowed NO-OP, for comfort.

  @groupBegin }
function CreateUIFont: TGLBitmapFont_Abstract;
procedure DestroyUIFont(var Font: TGLBitmapFont_Abstract);
{ @groupEnd }

const
  TooltipInsideColor: TVector3Byte = (255, 234, 169);
  TooltipBorderColor: TVector3Byte = (157, 133, 105);
  TooltipTextColor  : TVector3Byte = (  0,   0,   0);

procedure Register;

implementation

uses SysUtils, BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts,
  CastleGLUtils, GLImages, Math;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleButton, TCastleImageControl]);
end;

const
  { Our controls theme.
    These colors match somewhat our TCastleOnScreenMenu slider images. }
  ColInsideUp  : array[boolean] of TVector3Byte = ( (165, 245, 210), (169, 251, 216) );
  ColInsideDown: array[boolean] of TVector3Byte = ( (126, 188, 161), (139, 207, 177) );
  ColDarkFrame : TVector3Byte = ( 99,  99,  99);
  ColLightFrame: TVector3Byte = (221, 221, 221);
  ColText      : TVector3Byte = (  0,   0,   0);

{ Call glColor, taking Opacity as separate Single argument }
procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
begin
  glColor4f(Color[0], Color[1], Color[2], Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color[0] / 255, Color[1] / 255, Color[2] / 255, Opacity);
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

  glTranslatef(X, Y, 0);
  Font.PrintStringsBox([Tooltip], false, 0,
    Vector4Single(TooltipInsideColor, 255),
    Vector4Single(TooltipBorderColor, 255),
    Vector4Single(TooltipTextColor, 255), 5);
end;

procedure TUIControlFont.GLContextOpen;
begin
  inherited;
  FFont := CreateUIFont;
end;

procedure TUIControlFont.GLContextClose;
begin
  DestroyUIFont(FFont);
  inherited;
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
  FOpacity := 1;
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }
end;

destructor TCastleButton.Destroy;
begin
  if OwnsImage then FreeAndNil(FImage);
  glFreeDisplayList(FGLImage);
  inherited;
end;

function TCastleButton.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleButton.Draw;

  procedure DrawFrame(const Level: Cardinal; const Inset: boolean);
  begin
    if Inset then
      glColorOpacity(ColLightFrame, Opacity) else
      glColorOpacity(ColDarkFrame, Opacity);
    glVertexPixel( Level + Left            ,  Level + Bottom);
    glVertexPixel(-Level + Left + Width - 1,  Level + Bottom);
    glVertexPixel(-Level + Left + Width - 1,  Level + Bottom);
    glVertexPixel(-Level + Left + Width - 1, -Level + Bottom + Height - 1);
    if Inset then
      glColorOpacity(ColDarkFrame, Opacity) else
      glColorOpacity(ColLightFrame, Opacity);
    glVertexPixel( Level + Left            ,  Level + Bottom + 1);
    glVertexPixel( Level + Left            , -Level + Bottom + Height - 1);
    glVertexPixel( Level + Left            , -Level + Bottom + Height - 1);
    glVertexPixel(-Level + Left + Width    , -Level + Bottom + Height - 1);
  end;

var
  TextLeft, TextBottom, ImgLeft, ImgBottom: Integer;
begin
  if not GetExists then Exit;

  if Opacity < 1 then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
  end;

  glPushAttrib(GL_LIGHTING_BIT);
    glShadeModel(GL_SMOOTH); // saved by GL_LIGHTING_BIT
    glBegin(GL_QUADS);
      glColorOpacity(ColInsideDown[Focused and not Pressed], Opacity);
      glVertexPixel(Left        , Bottom);
      glVertexPixel(Left + Width, Bottom);
      glColorOpacity(ColInsideUp[Focused and not Pressed], Opacity);
      glVertexPixel(Left + Width, Bottom + Height);
      glVertexPixel(Left        , Bottom + Height);
    glEnd;

    glLineWidth(1.0);
    glBegin(GL_LINES);
      DrawFrame(0, Pressed);
      DrawFrame(1, Pressed);
    glEnd;

    glColorOpacity(ColText, Opacity);

    TextLeft := Left + (Width - TextWidth) div 2;
    if (FImage <> nil) and (FGLImage <> 0) and (ImageLayout = ilLeft) then
      TextLeft += (FImage.Width + ButtonCaptionImageMargin) div 2 else
    if (FImage <> nil) and (FGLImage <> 0) and (ImageLayout = ilRight) then
      TextLeft -= (FImage.Width + ButtonCaptionImageMargin) div 2;

    TextBottom := Bottom + (Height - TextHeight) div 2;
    if (FImage <> nil) and (FGLImage <> 0) and (ImageLayout = ilBottom) then
      TextBottom += (FImage.Height + ButtonCaptionImageMargin) div 2 else
    if (FImage <> nil) and (FGLImage <> 0) and (ImageLayout = ilTop) then
      TextBottom -= (FImage.Height + ButtonCaptionImageMargin) div 2;

    glRasterPos2i(TextLeft, TextBottom);
    Font.Print(Caption);
  glPopAttrib;

  if Opacity < 1 then
    glPopAttrib;

  if (FImage <> nil) and (FGLImage <> 0) then
  begin
    if FImage.HasAlpha then
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT);

      if ImageAlphaTest then
      begin
        glAlphaFunc(GL_GEQUAL, 0.5); // saved by GL_COLOR_BUFFER_BIT
        glEnable(GL_ALPHA_TEST); // saved by GL_COLOR_BUFFER_BIT
      end else
      begin
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
        glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
      end;
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
    glRasterPos2i(ImgLeft, ImgBottom);
    glCallList(FGLImage);
    if FImage.HasAlpha then
      glPopAttrib;
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
  if (FGLImage = 0) and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
  UpdateTextSize;
end;

procedure TCastleButton.GLContextClose;
begin
  glFreeDisplayList(FGLImage);
  inherited;
end;

function TCastleButton.MouseDown(const Button: KeysMouse.TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  Result := ExclusiveEvents;
  if not Toggle then FPressed := true;
  ClickStarted := true;
  { We base our Draw on Pressed value. }
  VisibleChange;
end;

function TCastleButton.MouseUp(const Button: KeysMouse.TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

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

    if (AutoSizeWidth or AutoSizeHeight) and (Container <> nil) then
      Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleButton.SetImage(const Value: TImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    glFreeDisplayList(FGLImage);

    FImage := Value;

    if GLInitialized and (FImage <> nil) then
      FGLImage := ImageDrawToDisplayList(FImage);

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
  end;
end;

procedure TCastleButton.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  { let controls under the TCastleButton handle keys/mouse,
    because TCastleButton doesn't do anything with them by default. }
  LetOthersHandleMouseAndKeys := true;
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

{ TCastlePanel ------------------------------------------------------------------ }

constructor TCastlePanel.Create(AOwner: TComponent);
begin
  inherited;
  FOpacity := 1;
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

  function PanelCol(const V: TVector3Byte): TVector3Single;
  const
    Exp = 1.3;
  begin
    Result[0] := Power(V[0] / 255, Exp);
    Result[1] := Power(V[1] / 255, Exp);
    Result[2] := Power(V[2] / 255, Exp);
  end;

const
  SeparatorMargin = 8;
var
  I: Integer;
begin
  if not GetExists then Exit;

  if Opacity < 1 then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
  end;

  glPushAttrib(GL_LIGHTING_BIT);
    glShadeModel(GL_SMOOTH); // saved by GL_LIGHTING_BIT
    glBegin(GL_QUADS);
      glColorOpacity(PanelCol(ColInsideDown[false]), Opacity);
      glVertexPixel(Left        , Bottom);
      glVertexPixel(Left + Width, Bottom);
      glColorOpacity(PanelCol(ColInsideUp[false]), Opacity);
      glVertexPixel(Left + Width, Bottom + Height);
      glVertexPixel(Left        , Bottom + Height);
    glEnd;
  glPopAttrib;

  if VerticalSeparators.Count <> 0 then
  begin
    glLineWidth(1.0);
    glBegin(GL_LINES);
      glColorOpacity(ColDarkFrame, Opacity);
      for I := 0 to VerticalSeparators.Count - 1 do
      begin
        glVertexPixel(Left + VerticalSeparators[I], Bottom + SeparatorMargin);
        glVertexPixel(Left + VerticalSeparators[I], Bottom + Height - SeparatorMargin);
      end;
      glColorOpacity(ColLightFrame, Opacity);
      for I := 0 to VerticalSeparators.Count - 1 do
      begin
        glVertexPixel(Left + VerticalSeparators[I] + 1, Bottom + SeparatorMargin);
        glVertexPixel(Left + VerticalSeparators[I] + 1, Bottom + Height - SeparatorMargin);
      end;
    glEnd;
  end;

  if Opacity < 1 then
    glPopAttrib;
end;

function TCastlePanel.PositionInside(const X, Y: Integer): boolean;
begin
  Result := GetExists and
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

procedure TCastlePanel.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  { let controls under the TCastlePanel handle keys/mouse,
    because TCastlePanel doesn't do anything with them by default. }
  LetOthersHandleMouseAndKeys := true;
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

{ TCastleImageControl ---------------------------------------------------------------- }

destructor TCastleImageControl.Destroy;
begin
  FreeAndNil(FImage);
  glFreeDisplayList(FGLImage);
  inherited;
end;

procedure TCastleImageControl.SetFileName(const Value: string);
var
  NewImage: TImage;
begin
  if Value <> '' then
    NewImage := LoadImage(Value, [], [], 0, 0) else
    NewImage := nil;

  { only once NewImage is successfully loaded, do the rest }
  FreeAndNil(FImage);
  glFreeDisplayList(FGLImage);

  FImage := NewImage;
  FFileName := Value;
  if GLInitialized and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
end;

function TCastleImageControl.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists and (FGLImage <> 0) then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleImageControl.Draw;
begin
  if not (GetExists and (FGLImage <> 0)) then Exit;

  if Blending then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
  end;

  glRasterPos2i(Left, Bottom);
  glCallList(FGLImage);

  if Blending then
    glPopAttrib;
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
  if (FGLImage = 0) and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
end;

procedure TCastleImageControl.GLContextClose;
begin
  glFreeDisplayList(FGLImage);
  inherited;
end;

procedure TCastleImageControl.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  { let controls under the TCastleImageControl handle keys/mouse,
    because TCastleImageControl doesn't do anything with them by default. }
  LetOthersHandleMouseAndKeys := true;
end;

{ UIFont --------------------------------------------------------------------- }

var
  UIFont: TGLBitmapFont_Abstract;
  UIFontUsed: Cardinal;

function CreateUIFont: TGLBitmapFont_Abstract;
begin
  if UIFont = nil then
  begin
    UIFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
    UIFontUsed := 0;
  end;

  Inc(UIFontUsed);
  Result := UIFont;
end;

procedure DestroyUIFont(var Font: TGLBitmapFont_Abstract);
begin
  if Font <> nil then
  begin
    Assert(Font = UIFont, 'You can pass to DestroyUIFont only fonts created with CreateUIFont');
    Dec(UIFontUsed);
    if UIFontUsed = 0 then
      FreeAndNil(UIFont);
    Font := nil;
  end;
end;

end.
