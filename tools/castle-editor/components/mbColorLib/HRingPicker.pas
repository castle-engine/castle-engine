unit HRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms,
  HTMLColors, mbColorConv, mbColorPickerControl;

type
  THRingPicker = class(TmbHSLVColorPickerControl)
  private
    FSelectedColor: TColor;
    FHueLineColor: TColor;
    FRadius: integer;
    procedure SetRadius(r: integer);
    procedure SetHueLineColor(c: TColor);
  protected
    procedure CreateGradient; override;
    procedure DrawHueLine;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
//    function MouseOnPicker(X, Y: Integer): Boolean;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(x, y: integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue default 0;
    property Luminance default 127;
    property Saturation default 255;
    property Value default 255;
    property MaxHue default 360;
    property MaxLuminance default 255;
    property MaxSaturation default 255;
    property MaxValue default 255;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property Radius: integer read FRadius write SetRadius default 40;
    property SelectedColor default clRed; //clNone;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{ THRingPicker }

constructor THRingPicker.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialBounds(0, 0, 204, 204);
  FHue := 0.0;
  FVal := 1.0;
  FLum := 0.5;
  FSat := 1.0;
  SetSelectedColor(clRed);
  FRadius := 40;
  FHueLineColor := clGray;
  HintFormat := 'Hue: %h (selected)';
  TabStop := true;
end;

procedure THRingPicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;

procedure THRingPicker.DrawHueLine;
var
  angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  radius := Min(Width, Height) div 2;
  if (FHue >= 0) and (FHue <= 1.0) then
  begin
    angle := -FHue * TWO_PI;
    SinCos(angle, sinAngle, cosAngle);
    Canvas.Pen.Color := FHueLineColor;
    Canvas.MoveTo(radius, radius);
    Canvas.LineTo(radius + round(radius*cosAngle), radius + round(radius*sinAngle));
  end;
end;

function THRingPicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle: Double;
  dx, dy, radius: integer;
  h: Double;
begin
  radius := Min(Width, Height) div 2;

  if PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    dx := x - Radius;
    dy := y - Radius;
    angle := 360 + 180 * arctan2(-dy, dx) / pi;
    if angle < 0 then
      angle := angle + 360
    else if angle > 360 then
      angle := angle - 360;
    h := angle / 360;
    Result := HSLVtoColor(h, FSat, FLum, FVal);
  end
  else
    Result := clNone;
end;

{ Outer loop: Y, Inner loop: X }
function THRingPicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  dx, dy: Integer;
  dSq, rSq: Integer;
  radius, size: Integer;
  H: Double;
begin
  size := FGradientWidth;  // or Height, they are the same...
  radius := size div 2;
  rSq := sqr(radius);
  dx := X - radius;
  dy := Y - radius;
  dSq := sqr(dx) + sqr(dy);
  if dSq <= rSq then
  begin
    H := 180 * (1 + arctan2(dx, dy) / pi);  // wp: order (x,y) is correct!
    H := H + 90;
    if H > 360 then H := H - 360;
    Result := HSLVtoColor(H/360, FSat, FLum, FVal);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := GetDefaultColor(dctBrush);
end;

function THRingPicker.GetSelectedColor: TColor;
begin
  if FSelectedColor <> clNone then
    Result := HSLVtoColor(FHue, FSat, FLum, FVal)
  else
    Result := clNone;
end;
                                 {
function THRingPicker.MouseOnPicker(X, Y: Integer): Boolean;
var
  diameter, r: Integer;
  P, ctr: TPoint;
begin
  diameter := Min(Width, Height);
  r := diameter div 2;      // outer radius
  P := Point(x, y);
  ctr := Point(r, r);
  Result := PtInCircle(P, ctr, r) and not PtInCircle(P, ctr, Radius);
end;                              }

procedure THRingPicker.Paint;
var
  rgn, r1, r2: HRGN;
  r: TRect;
  size: Integer;
  ringwidth: Integer;
begin
  PaintParentBack(Canvas);
  size := Min(Width, Height);         // diameter of circle
  ringwidth := size div 2 - FRadius;  // FRadius is inner radius
  r := ClientRect;
  r.Right := R.Left + size;
  R.Bottom := R.Top + size;
  InflateRect(R, -1, -1);      // Remove spurious black pixels at the border
  r1 := CreateEllipticRgnIndirect(R);
  if ringwidth > 0 then
  begin
    rgn := r1;
    InflateRect(R, -ringwidth, - ringwidth);
    r2 := CreateEllipticRgnIndirect(R);
    CombineRgn(rgn, r1, r2, RGN_DIFF);
  end;
  SelectClipRgn(Canvas.Handle, rgn);
  Canvas.Draw(0, 0, FBufferBmp);
  DeleteObject(rgn);
  DrawHueLine;
  DoChange;
end;

procedure THRingPicker.Resize;
begin
  inherited;
  if Min(Width, Height) <> FGradientWidth then
    CreateGradient;
  UpdateCoords;
end;

procedure THRingPicker.SelectColor(x, y: integer);
var
  angle, dx, dy, Radius: integer;
begin
  mx := y;
  my := y;
  FSelectedColor := clWhite;
  radius := Min(Width, Height) div 2;
  dx := x - radius;
  dy := y - radius;
  angle := round(360 + 180*arctan2(-dy, dx) / pi);
  SetRelHue(angle/360);
end;

procedure THRingPicker.SetHueLineColor(c: TColor);
begin
  if FHueLineColor <> c then
  begin
    FHueLineColor := c;
    Invalidate;
  end;
end;

procedure THRingPicker.SetRadius(r: integer);
begin
  if FRadius <> r then
  begin
    FRadius := r;
    Invalidate;
  end;
end;

procedure THRingPicker.SetRelHue(H: Double);
begin
  if H > 1 then H := H - 1;
  if H < 0 then H := H + 1;
  if FHue <> h then
  begin
    FHue := h;
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

(*
procedure THRingPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if Saturation <> s then
  begin
    FSat := s / FMaxSat;
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;
*)
procedure THRingPicker.SetSelectedColor(c: TColor);
var
  H: Double = 0;
  S: Double = 0;
  L: Double = 0;
  V: Double = 0;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = GetSelectedColor then
    Exit;

  ColorToHSLV(c, H, S, L, V);
  case BrightnessMode of
    bmLuminance:
      begin
        needNewGradient := (S <> FSat) or (L <> FLum);
        FLum := L;
      end;
    bmValue:
      begin
        needNewGradient := (S <> FSat) or (V <> FVal);
        FVal := V;
      end;
  end;
  FHue := h;
  FSat := s;
  UpdateCoords;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

(*
procedure THRingPicker.SetVal(v: integer);
begin
  Clamp(v, 0, FMaxVal);
  if Value <> V then
  begin
    FVal := V / FMaxVal;
    if BrightnessMode = bmValue then
    begin
      CreateGradient;
      Invalidate;
    end;
    DoChange;
  end;
end;
*)
procedure THRingPicker.UpdateCoords;
var
  r, angle: double;
  radius: integer;
  sinAngle, cosAngle: Double;
begin
  radius := Min(Width, Height) div 2;
  r := -radius * FSat;
  angle := -(FHue * 2 + 1) * pi;
  SinCos(angle, sinAngle, cosAngle);
  mx := round(cosAngle * r) + radius;
  my := round(sinAngle * r) + radius;
end;

end.
