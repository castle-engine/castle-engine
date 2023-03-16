unit CIEAColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, RGBCIEUtils, mbColorPickerControl;

type
  TCIEAColorPicker = class(TmbColorPickerControl)
  private
    FL, FA, FB: integer;
    procedure SetLValue(l: integer);
    procedure SetAValue(a: integer);
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
    property SelectedColor default clFuchsia;
    property LValue: integer read FL write SetLValue default 100;
    property AValue: integer read FA write SetAValue default 127;
    property BValue: integer read FB write SetBValue default -128;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils;

{TCIEAColorPicker}

constructor TCIEAColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'L: %cieL B: %cieB'#13'Hex: %hex';
  FSelected := clFuchsia;
  FL := 100;
  FA := 127;
  FB := -128;
  MarkerStyle := msCircle;
end;

procedure TCIEAColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TCIEAColorPicker.CreateWnd;
begin
 inherited;
 CreateGradient;
end;

procedure TCIEAColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  FL := Round(GetCIELValue(FSelected));
  FA := Round(GetCIEAValue(FSelected));
  FB := Round(GetCIEBValue(FSelected));
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

{
function TCIEAColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  l, a, b: Integer;
begin
  l := round(100 * (1 - y / (Height-1)));
  a := FA;
  b := round(255 * (x / (Width - 1))) - 128;
  Result := LabToRGB(l, a, b);
end;
}
function TCIEAColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  l, b: Integer;
begin
  l := round((1 - y / (Height - 1)) * 100);
  b := round((x / (Width - 1) - 0.5) * 255);
  Result := LabToRGB(l, FA, b);
end;

// In the original code: for L ... for B ... LabToRGB(Round(100-L*100/255), FA, B-128);
// --> x is B, y is L
function TCIEAColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := LabToRGB(Round(100 - y*100/255), FA, x - 128);
end;

procedure TCIEAColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TCIEAColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure TCIEAColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(X, Y);
end;

procedure TCIEAColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(X, Y);
end;

procedure TCIEAColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

procedure TCIEAColorPicker.Resize;
begin
  mx := Round((FB + 128) / 255 * Width);
//  myy := Round(((100 - FL) * 255 / 100) * Height / 255);
  my := Round((100 - FL) / 100 * Height);
  inherited;
end;

procedure TCIEAColorPicker.SelectColor(x, y: Integer);
var
  c: TColor;
  L, a, b: Integer;
  needNewGradient: Boolean;
begin
  CorrectCoords(x, y);
  c := GetColorAtPoint(x, y);
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelected then
    exit;

  mx := x;
  my := y;
  L := Round(GetCIELValue(c));
  a := Round(GetCIEAValue(c));
  b := Round(GetCIEBValue(c));
  needNewGradient := a <> FA;
  FSelected := c;
  FL := L;
  FA := a;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TCIEAColorPicker.SetAValue(a: integer);
begin
  Clamp(a, -128, 127);
  FA := a;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEAColorPicker.SetBValue(b: integer);
begin
  Clamp(b, -128, 127);
  FB := b;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEAColorPicker.SetLValue(l: integer);
begin
  Clamp(L, 0, 100);
  FL := L;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEAColorPicker.SetSelectedColor(c: TColor);
var
  l, a, b: Integer;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelected then
    exit;

  l := Round(GetCIELValue(c));
  a := Round(GetCIEAValue(c));
  b := Round(GetCIEBValue(c));
  needNewGradient := a <> FA;
  FL := l;
  FA := a;
  FB := b;
  FSelected := c;
  mx := Round((FB + 128) * Width / 255);
//  my := Round((100 - FL) * 255 / 100 * Height / 255);
  my := Round((100 - FL) / 100 * Height);
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
