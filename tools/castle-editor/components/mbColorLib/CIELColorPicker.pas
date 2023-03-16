unit CIELColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms,
  HTMLColors, RGBCIEUtils, mbColorPickerControl;

type
  TCIELColorPicker = class(TmbColorPickerControl)
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
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(X, Y: Integer);
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(X, Y: Integer): TColor; override;
  published
    property AValue: integer read FA write SetAValue default -128;
    property BValue: integer read FB write SetBValue default 127;
    property LValue: integer read FL write SetLValue default 100;
    property MarkerStyle default msCircle;
    property SelectedColor default clAqua;
    property OnChange;
 end;


implementation

uses
  mbUtils;

{TCIELColorPicker}

constructor TCIELColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'A: %cieA B: %cieB'#13'Hex: %hex';
  FSelected := clAqua;
  FL := 100;
  FA := -128;
  FB := 127;
  MarkerStyle := msCircle;
end;

procedure TCIELColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  clamp(y, 0, Height - 1);
end;

procedure TCIELColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TCIELColorPicker.DrawMarker(x, y: integer);
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

function TCIELColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  a, b: Double;
begin
  a := (y / (Height - 1) - 0.5) * 255;
  b := (x / (Width  - 1) - 0.5) * 255;
  Result := LabToRGB(FL, a, b);
end;
{
var
  a, b: Integer;
begin
  a := round(255 * (y / (Height - 1))) - 128;
  b := round(255 * (x / (Width - 1))) - 128;
  Result := LabToRGB(FL, a, b);
end;
}

{ Original code: for A ... for B ---> LabToRGB(FL, A - 128, B - 128) }
function TCIELColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := LabToRGB(FL, y - 128, x - 128);
end;

procedure TCIELColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  erasekey := true;
  delta := IfThen(ssCtrl in Shift, 10, 1);

  case Key of
    VK_LEFT  : SelectColor(mx - delta, my);
    VK_Right : SelectColor(mx + delta, my);
    VK_UP    : SelectColor(mx, my - delta);
    VK_DOWN  : SelectColor(mx, my + delta);
    else       eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TCIELColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(X, Y);
  SetFocus;
end;

procedure TCIELColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(X, Y);
end;

procedure TCIELColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(X, Y);
end;

procedure TCIELColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  CorrectCoords(mx, my);
  DrawMarker(mx, my);
end;

procedure TCIELColorPicker.Resize;
begin
  mx := Round((FA + 128) * Width / 255);
  my := Round((255 - (FB + 128)) * Height / 255);
  inherited;
end;

procedure TCIELColorPicker.SelectColor(x, y: Integer);
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
  needNewGradient := l <> FL;
  FSelected := c;
  FL := l;
  FA := a;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TCIELColorPicker.SetAValue(a: integer);
begin
  Clamp(A, -128, 127);
  FA := a;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetBValue(b: integer);
begin
  Clamp(b, -128, 127);
  FB := b;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetLValue(l: integer);
begin
  Clamp(L, 0, 100);
  FL := L;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetSelectedColor(c: TColor);
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
  needNewGradient := L <> FL;
  FL := L;
  FA := a;
  FB := b;
  FSelected := c;
  mx := Round((FA + 128) * Width / 255);
  my := Round((255 - (FB + 128)) * Height / 255);
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
