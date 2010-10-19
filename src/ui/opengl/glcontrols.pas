{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Controls drawn inside OpenGL context. }
unit GLControls;

interface

uses UIControls, OpenGLFonts, KeysMouse, Classes, Images, GL;

type
  { Button inside OpenGL context.

    This is TUIControl descendant, so to use it just add it to
    the TGLUIWindow.Controls or TKamOpenGLControl.Controls list.
    You will also usually want to adjust position (TKamGLButton.Left,
    TKamGLButton.Bottom), TKamGLButton.Caption,
    and assign TKamGLButton.OnClick (or ovevrride TKamGLButton.DoClick). }
  TKamGLButton = class(TUIControlPos)
  private
    Font: TGLBitmapFont_Abstract;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOnClick: TNotifyEvent;
    FCaption: string;
    FAutoSize: boolean;
    TextWidth, TextHeightBase: Cardinal;
    FPressed: boolean;
    FOwnsImage: boolean;
    FImage: TImage;
    FGLImage: TGLuint;
    FToggle: boolean;
    ClickStarted: boolean;
    FOpacity: Single;
    procedure SetCaption(const Value: string);
    procedure SetAutoSize(const Value: boolean);
    { Calculate TextWidth, TextHeightBase and call UpdateSize. }
    procedure UpdateTextSize;
    { If AutoSize, update Width, Height.
      This depends on Caption, AutoSize, Font availability. }
    procedure UpdateSize;
    procedure SetImage(const Value: TImage);
    procedure SetPressed(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextInit; override;
    procedure GLContextClose; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    { Called when user clicks the button. In this class, simply calls
      OnClick callback. }
    procedure DoClick; virtual;
    procedure SetFocused(const Value: boolean); override;
    { Set this to non-nil to display an image on the button. }
    property Image: TImage read FImage write SetImage;
    { Should we free the @link(Image) when you set another one or at destructor. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default false;
  published
    property Width: Cardinal read FWidth write FWidth default 0;
    property Height: Cardinal read FHeight write FHeight default 0;

    { When AutoSize is @true (the default) then Width/Height are automatically
      adjusted when you change the Caption and @link(Image). They take into account
      Caption width/height with current font, @link(Image) width/height,
      and add some margin to make it look good.

      Note that this adjustment happens only when OpenGL context is initialized
      (because only then we actually know the font used).
      So don't depend on Width/Height values calculated correctly before
      OpenGL context is ready. }
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read FCaption write SetCaption;

    { Can the button be permanently pressed. Good for making a button
      behave like a checkbox, that is indicate a boolean state.
      When @link(Toggle) is @true, you can set the @link(Pressed) property,
      and the clicks are visualized a little different. }
    property Toggle: boolean read FToggle write FToggle;

    { Is the button pressed down. If @link(Toggle) is @true,
      you can read and write this property to set the pressed state.

      When not @link(Toggle), this property isn't really useful to you.
      The pressed state is automatically managed then to visualize
      user clicks. You can read this property, but you cannot set it. }
    property Pressed: boolean read FPressed write SetPressed;

    { Opacity (1 - transparency) with which control is drawn.
      When this is < 1, we draw control with nice blending.
      Make sure you have some background control underneath in this case. }
    property Opacity: Single read FOpacity write FOpacity default 1.0;
  end;

  { Panel inside OpenGL context.
    Use as a comfortable (and with matching colors) background
    for other controls like buttons and such. }
  TKamPanel = class(TUIControlPos)
  private
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOpacity: Single;
  public
    constructor Create(AOwner: TComponent); override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
  published
    property Width: Cardinal read FWidth write FWidth default 0;
    property Height: Cardinal read FHeight write FHeight default 0;

    { Opacity (1 - transparency) with which control is drawn.
      When this is < 1, we draw control with nice blending.
      Make sure you have some background control underneath in this case. }
    property Opacity: Single read FOpacity write FOpacity default 1.0;
  end;

  { Image control inside OpenGL context.
    Size is automatically adjusted to the image size.
    You should set TKamGLImage.Left, TKamGLImage.Bottom properties,
    and load your image by setting TKamGLImage.FileName property. }
  TKamGLImage = class(TUIControlPos)
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
    procedure GLContextInit; override;
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

{ Create and destroy the default UI interface bitmap font.

  They don't actually create new font each time --- first create
  creates the font, next ones only increase the internal counter.
  Destroy decreases the counter and only really frees when it goes to zero.

  The bottom line: you should use them just like normal create / destroy
  (always pair a destroy with a create; destroying @nil is allowed NOOP
  for comfort). But they work fast.

  @groupBegin }
function CreateUIFont: TGLBitmapFont_Abstract;
procedure DestroyUIFont(var Font: TGLBitmapFont_Abstract);
{ @groupEnd }

procedure Register;

implementation

uses SysUtils, BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts, VectorMath,
  KambiGLUtils, GLImages, KambiUtils, Math;

procedure Register;
begin
  RegisterComponents('Kambi', [TKamGLButton, TKamGLImage]);
end;

const
  { Our controls theme.
    These colors match somewhat our TGLMenu slider images. }
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

{ TKamGLButton ------------------------------------------------------------------ }

const
  ButtonCaptionImageMargin = 10;

constructor TKamGLButton.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := true;
  FOpacity := 1;
  { no need to UpdateTextSize here yet, since Font is for sure not ready yet. }
end;

destructor TKamGLButton.Destroy;
begin
  if OwnsImage then FreeAndNil(FImage);
  glFreeDisplayList(FGLImage);
  inherited;
end;

function TKamGLButton.DrawStyle: TUIControlDrawStyle;
begin
  if Exists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TKamGLButton.Draw;

  procedure DrawFrame(const Level: Cardinal; const Inset: boolean);
  begin
    if Inset then
      glColorOpacity(ColLightFrame, Opacity) else
      glColorOpacity(ColDarkFrame, Opacity);
    glVertex2i( Level + Left            ,  Level + Bottom);
    glVertex2i(-Level + Left + Width - 1,  Level + Bottom);
    glVertex2i(-Level + Left + Width - 1,  Level + Bottom);
    glVertex2i(-Level + Left + Width - 1, -Level + Bottom + Height - 1);
    if Inset then
      glColorOpacity(ColDarkFrame, Opacity) else
      glColorOpacity(ColLightFrame, Opacity);
    glVertex2i( Level + Left            ,  Level + Bottom + 1);
    glVertex2i( Level + Left            , -Level + Bottom + Height - 1);
    glVertex2i( Level + Left            , -Level + Bottom + Height - 1);
    glVertex2i(-Level + Left + Width    , -Level + Bottom + Height - 1);
  end;

var
  TextLeft: Integer;
begin
  if not Exists then Exit;

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
      glVertex2i(Left        , Bottom);
      glVertex2i(Left + Width, Bottom);
      glColorOpacity(ColInsideUp[Focused and not Pressed], Opacity);
      glVertex2i(Left + Width, Bottom + Height);
      glVertex2i(Left        , Bottom + Height);
    glEnd;

    glBegin(GL_LINES);
      DrawFrame(0, Pressed);
      DrawFrame(1, Pressed);
    glEnd;

    glColorOpacity(ColText, Opacity);
    if (FImage <> nil) and (FGLImage <> 0) then
      TextLeft := Left +
        (Width + FImage.Width + ButtonCaptionImageMargin - TextWidth) div 2 else
      TextLeft := Left + (Width - TextWidth) div 2;
    glRasterPos2i(TextLeft, Bottom + (Height - TextHeightBase) div 2);
    Font.Print(Caption);
  glPopAttrib;

  if (FImage <> nil) and (FGLImage <> 0) then
  begin
    if FImage.HasAlpha then
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT);
        glAlphaFunc(GL_GEQUAL, 0.5); // saved by GL_COLOR_BUFFER_BIT
        glEnable(GL_ALPHA_TEST); // saved by GL_COLOR_BUFFER_BIT
    end;
    glRasterPos2i(TextLeft - FImage.Width - ButtonCaptionImageMargin,
      Bottom + (Height - FImage.Height) div 2);
    glCallList(FGLImage);
    if FImage.HasAlpha then
      glPopAttrib;
  end;

  if Opacity < 1 then
    glPopAttrib;
end;

function TKamGLButton.PositionInside(const X, Y: Integer): boolean;
begin
  Result := Exists and
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

procedure TKamGLButton.GLContextInit;
begin
  inherited;
  Font := CreateUIFont;
  if (FGLImage = 0) and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
  UpdateTextSize;
end;

procedure TKamGLButton.GLContextClose;
begin
  DestroyUIFont(Font);
  glFreeDisplayList(FGLImage);
  inherited;
end;

function TKamGLButton.MouseDown(const Button: KeysMouse.TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := ExclusiveEvents;
  if not Toggle then FPressed := true;
  ClickStarted := true;
  { We base our Draw on Pressed value. }
  VisibleChange;
end;

function TKamGLButton.MouseUp(const Button: KeysMouse.TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

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

procedure TKamGLButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TKamGLButton.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    UpdateTextSize;
  end;
end;

procedure TKamGLButton.SetAutoSize(const Value: boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    UpdateTextSize;
  end;
end;

procedure TKamGLButton.UpdateTextSize;
begin
  if Font <> nil then
  begin
    TextWidth := Font.TextWidth(Caption);
    TextHeightBase := Font.RowHeightBase;
    UpdateSize;
  end;
end;

procedure TKamGLButton.UpdateSize;
const
  HorizontalMargin = 10;
  VerticalMargin = 10;
begin
  if AutoSize then
  begin
    Width := TextWidth + HorizontalMargin * 2;
    Height := TextHeightBase + VerticalMargin * 2;
    if FImage <> nil then
    begin
      Width := Width + FImage.Width + ButtonCaptionImageMargin;
      Height := Max(Height, FImage.Height + VerticalMargin * 2);
    end;
  end;
end;

procedure TKamGLButton.SetImage(const Value: TImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    glFreeDisplayList(FGLImage);

    FImage := Value;

    if GLContextInitialized and (FImage <> nil) then
      FGLImage := ImageDrawToDisplayList(FImage);

    UpdateSize;
  end;
end;

procedure TKamGLButton.SetFocused(const Value: boolean);
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

procedure TKamGLButton.SetPressed(const Value: boolean);
begin
  if FPressed <> Value then
  begin
    if not Toggle then
      raise Exception.Create('You cannot modify TKamGLButton.Pressed value when Toggle is false');
    FPressed := Value;
  end;
end;

{ TKamPanel ------------------------------------------------------------------ }

constructor TKamPanel.Create(AOwner: TComponent);
begin
  inherited;
  FOpacity := 1;
end;

function TKamPanel.DrawStyle: TUIControlDrawStyle;
begin
  if Exists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TKamPanel.Draw;

  function PanelCol(const V: TVector3Byte): TVector3Single;
  const
    Exp = 1.3;
  begin
    Result[0] := Power(V[0] / 255, Exp);
    Result[1] := Power(V[1] / 255, Exp);
    Result[2] := Power(V[2] / 255, Exp);
  end;

begin
  if not Exists then Exit;

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
      glVertex2i(Left        , Bottom);
      glVertex2i(Left + Width, Bottom);
      glColorOpacity(PanelCol(ColInsideUp[false]), Opacity);
      glVertex2i(Left + Width, Bottom + Height);
      glVertex2i(Left        , Bottom + Height);
    glEnd;
  glPopAttrib;

  if Opacity < 1 then
    glPopAttrib;
end;

function TKamPanel.PositionInside(const X, Y: Integer): boolean;
begin
  Result := Exists and
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

{ TKamGLImage ---------------------------------------------------------------- }

destructor TKamGLImage.Destroy;
begin
  FreeAndNil(FImage);
  glFreeDisplayList(FGLImage);
  inherited;
end;

procedure TKamGLImage.SetFileName(const Value: string);
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
  if GLContextInitialized and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
end;

function TKamGLImage.DrawStyle: TUIControlDrawStyle;
begin
  if Exists and (FGLImage <> 0) then
    Result := ds2D else
    Result := dsNone;
end;

procedure TKamGLImage.Draw;
begin
  if not (Exists and (FGLImage <> 0)) then Exit;

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

function TKamGLImage.PositionInside(const X, Y: Integer): boolean;
begin
  Result := Exists and
    (FImage <> nil) and
    (X >= Left) and
    (X  < Left + FImage.Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + FImage.Height);
end;

procedure TKamGLImage.GLContextInit;
begin
  inherited;
  if (FGLImage = 0) and (FImage <> nil) then
    FGLImage := ImageDrawToDisplayList(FImage);
end;

procedure TKamGLImage.GLContextClose;
begin
  glFreeDisplayList(FGLImage);
  inherited;
end;

procedure TKamGLImage.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  { let controls under the TKamGLImage handle keys/mouse,
    because TKamGLImage doesn't do anything with them by default. }
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
