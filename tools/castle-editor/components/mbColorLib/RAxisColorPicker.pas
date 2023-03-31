unit RAxisColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorPickerControl;

type
  TRAxisColorPicker = class(TmbColorPickerControl)
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
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(x, y: Integer);
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clRed;
    property Red: integer read FR write SetRValue default 255;
    property Green: integer read FG write SetGValue default 0;
    property Blue: integer read FB write SetBValue default 0;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils;

{TRAxisColorPicker}

constructor TRAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'G: %g B: %b'#13'Hex: %hex';
  FG := 0;
  FB := 0;
  FR := 255;
  FSelected := clRed;
  MarkerStyle := msCircle;
end;

procedure TRAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TRAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TRAxisColorPicker.DrawMarker(x, y: integer);
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

function TRAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  g, b: Integer;
begin
  b := round(x / (Width - 1) * 255);
  g := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(FR, g, b);
end;

{ x is BLUE, y is GREEN }
function TRAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(FR, FBufferBmp.Height - 1 - y, x);
end;

procedure TRAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  delta: Integer;
  eraseKey: Boolean;
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

procedure TRAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure TRAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(x, y);
end;

procedure TRAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
end;

procedure TRAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

procedure TRAxisColorPicker.Resize;
begin
  mx := Round(FB * Width / 255);
  my := Round((255 - FG) * Height / 255);
  inherited;
end;

procedure TRAxisColorPicker.SelectColor(x, y: Integer);
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
  needNewGradient := r <> FR;
  FR := r;
  FG := g;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TRAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  SetSelectedColor(RGBtoColor(FR, FG, b));
end;

procedure TRAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  SetSelectedColor(RGBtoColor(FR, g, FB));
end;

procedure TRAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  if FR <> r then
    SetSelectedColor(RGBtoColor(r, FG, FB));
end;

procedure TRAxisColorPicker.SetSelectedColor(c: TColor);
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
  needNewGradient := r <> FR;
  FR := r;
  FG := g;
  FB := b;
  FSelected := c;
  mx := Round(FB * Width / 255);            // BLUE on x
  my := Round((255 - FG) * Height / 255);   // GREEN on y
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
