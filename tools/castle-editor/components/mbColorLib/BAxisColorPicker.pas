unit BAxisColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics,
  HTMLColors, mbColorPickerControl;

type
  TBAxisColorPicker = class(TmbColorPickerControl)
  private
    FR, FG, FB: integer;
    procedure SetRValue(r: integer);
    procedure SetGValue(g: integer);
    procedure SetBValue(b: integer);
  protected
    procedure CorrectCoords(var x, y: integer);
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(x, y: Integer): TColor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(x, y: Integer);
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clBlue;
    property Red: integer read FR write SetRValue default 0;
    property Green: integer read FG write SetGValue default 0;
    property Blue: integer read FB write SetBValue default 255;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils;


{TBAxisColorPicker}

constructor TBAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 255, 255);
  HintFormat := 'R: %r G: %g'#13'Hex: %hex';
  FG := 0;
  FB := 255;
  FR := 0;
  FSelected := clBlue;
  MarkerStyle := msCircle;
end;

procedure TBAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TBAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TBAxisColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
  CorrectCoords(x, y);
  FR := GetRValue(FSelected);
  FG := GetGValue(FSelected);
  FB := GetBValue(FSelected);
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

function TBAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  r, g: Integer;
begin
  r := round(x / (Width - 1) * 255);
  g := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(r, g, FB);
end;

{ x is RED, y is GREEN }
function TBAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(x, FBufferBmp.Height - 1 - y, FB);
end;

procedure TBAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  delta := IfThen(ssCtrl in Shift, 10, 1);

  case Key of
    VK_LEFT  : SelectColor(mx - delta, my);
    VK_RIGHT : SelectColor(mx + delta, my);
    VK_UP    : SelectColor(mx, my - delta);
    VK_DOWN  : SelectColor(mx, my + delta);
    else       eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TBAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure TBAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(x, y);
end;

procedure TBAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
end;

procedure TBAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

procedure TBAxisColorPicker.Resize;
begin
  mx := round(FR * Width / 255);
  my := round((255 - FG) * Height / 255);
  inherited;
end;

procedure TBAxisColorPicker.SelectColor(x, y: Integer);
var
  c: TColor;
  r, g, b: Integer;
  needNewGradient: Boolean;
begin
  CorrectCoords(x, y);
  mx := x;
  my := y;
  c := GetColorAtPoint(x, y);
  if c = FSelected then
    exit;
  FSelected := c;
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  needNewGradient := b <> FB;
  FR := r;
  FG := g;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TBAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  if b <> FB then
    SetSelectedColor(RGBToColor(FR, FG, b));
end;

procedure TBAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  SetSelectedColor(RGBtoColor(FR, g, FB));
end;

procedure TBAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  SetSelectedColor(RGBtoColor(r, FG, FB));
end;

procedure TBAxisColorPicker.SetSelectedColor(c: TColor);
var
  r, g, b: Integer;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelected then
    exit;

  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  needNewGradient := (b <> FB);
  FR := r;
  FG := g;
  FB := b;
  FSelected := c;
  mx := Round(FR * Width / 255);                  // RED is on x
  my := Round((255 - FG) * Height / 255);         // GREEN is on y
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
