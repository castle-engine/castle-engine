unit GAxisColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLType, LCLIntf, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorPickerControl;

type
  TGAxisColorPicker = class(TmbColorPickerControl)
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
    property SelectedColor default clLime;
    property Red: integer read FR write SetRValue default 0;
    property Green: integer read FG write SetGValue default 255;
    property Blue: integer read FB write SetBValue default 0;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils;

{TGAxisColorPicker}

constructor TGAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'R: %r B: %b'#13'Hex: %hex';
  FG := 255;
  FB := 0;
  FR := 0;
  FSelected := clLime;
  MarkerStyle := msCircle;
end;

procedure TGAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width-1);
  Clamp(y, 0, Height-1);
end;

procedure TGAxisColorPicker.CreateWnd;
begin
 inherited;
 CreateGradient;
end;

procedure TGAxisColorPicker.DrawMarker(x, y: integer);
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

function TGAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  r, b: Integer;
begin
  b := round(x / (Width - 1) * 255);
  r := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(r, FG, b);
end;

// x is BLUE, y is RED
function TGAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(FBufferBmp.Height - 1 - y, FG, x);
end;

procedure TGAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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

  if eraseKey then Key := 0;
  inherited;
end;

procedure TGAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure TGAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(x, y);
end;

procedure TGAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
end;

procedure TGAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

procedure TGAxisColorPicker.Resize;
begin
  mx := Round(FB * Width / 255);
  my := Round((255 - FR) * Height / 255);
  inherited;
end;

procedure TGAxisColorPicker.SelectColor(x, y: Integer);
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
  needNewGradient := g <> FG;
  FR := r;
  FG := g;
  FB := b;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TGAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  SetSelectedColor(RGBToColor(FR, FG, b));
end;

procedure TGAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  if FG <> g then
    SetSelectedColor(RGBToColor(FR, g, FB));
end;

procedure TGAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  SetSelectedColor(RGBToColor(r, FG, FB));
end;

procedure TGAxisColorPicker.SetSelectedColor(c: TColor);
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
  needNewGradient := g <> FG;
  FR := r;
  FG := g;
  FB := b;
  FSelected := c;
  mx := Round(FB * Width / 255);            // BLUE is x
  my := Round((255 - FR) * Height / 255);   // RED is y
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
