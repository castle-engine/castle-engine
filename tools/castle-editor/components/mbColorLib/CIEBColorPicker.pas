unit CIEBColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, RGBCIEUtils, mbColorPickerControl;

type

  { TCIEBColorPicker }

  TCIEBColorPicker = class(TmbColorPickerControl)
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
    property LValue: integer read FL write SetLValue default 100;
    property AValue: integer read FA write SetAValue default -128;
    property BValue: integer read FB write SetBValue default 127;
    property MarkerStyle default msCircle;
    property SelectedColor default clLime;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils;

{TCIEBColorPicker}

constructor TCIEBColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'L: %cieL A: %cieA'#13'Hex: %hex';
  FSelected := clLime;
  FL := 100;
  FA := -128;
  FB := 127;
  MarkerStyle := msCircle;
end;

procedure TCIEBColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TCIEBColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TCIEBColorPicker.DrawMarker(x, y: integer);
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
function TCIEBColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  l, a, b: Integer;
begin
  l := round(100 * (1 - y / (Height-1)));
  a := round(255 * (x / (Width - 1))) - 128;
  b := FB;
  Result := LabToRGB(l, a, b);
end;
}
function TCIEBColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  l, a: Integer;
begin
  l := Round((1 - y / (Height - 1)) * 100);
  a := Round((x / (Width - 1) - 0.5) * 255);
  Result := LabToRGB(l, a, FB);
end;

{ In the original code: for L ... for A ... LabToRGB(Round(100-L*100/244), A-128, FB)
  --> x is A, y is L}
function TCIEBColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := LabToRGB(Round(100 - y*100/255), x - 128, FB);
end;

procedure TCIEBColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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
     else      eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TCIEBColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(X, Y);
  SetFocus;
end;

procedure TCIEBColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(X, Y);
end;

procedure TCIEBColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(X, Y);
end;

procedure TCIEBColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

procedure TCIEBColorPicker.Resize;
begin
  mx := Round((FA + 128) * (Width / 255));
//  myy := Round(((100 - FL) * 255 / 100) * (Height / 255));
  my := Round(( 100 - FL) / 100 * Height);
  inherited;
end;

procedure TCIEBColorPicker.SelectColor(x, y: Integer);
var
  c: TColor;
  l, a, b: Integer;
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
  l := Round(GetCIELValue(c));
  a := Round(GetCIEAValue(c));
  b := Round(GetCIEBValue(c));
  needNewGradient := b <> FB;
  FSelected := c;
  FL := l;
  FA := a;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TCIEBColorPicker.SetAValue(a: integer);
begin
  Clamp(a, -128, 127);
  FA := a;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetBValue(b: integer);
begin
  Clamp(b, -128, 127);
  FB := b;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetLValue(L: integer);
begin
  Clamp(L, 0, 100);
  FL := L;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetSelectedColor(c: TColor);
var
  L, a, b: Integer;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelected then
    exit;

  L := Round(GetCIELValue(c));
  a := Round(GetCIEAValue(c));
  b := Round(GetCIEBValue(c));
  needNewGradient := (b <> FB);
  FL := L;
  FA := a;
  FB := b;
  FSelected := c;
  mx := Round((FA + 128) * Width / 255);
//  my := Round((100 - FL) * 255 / 100* Height / 255);
  my := Round((100 - FL) / 100 * Height);
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
