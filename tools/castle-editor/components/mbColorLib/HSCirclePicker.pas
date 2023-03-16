unit HSCirclePicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms, Themes,
  HTMLColors, mbColorConv, mbColorPickerControl;

type
  THSCirclePicker = class(TmbHSLVColorPickerControl)
  private
    FSatCircColor, FHueLineColor: TColor;
    FShowSatCirc: boolean;
    FShowHueLine: boolean;
    FShowSelCirc: boolean;
    procedure SetRelHue(H: Double);
    procedure SetRelSat(S: Double);
    procedure SetSatCircColor(c: TColor);
    procedure SetHueLineColor(c: TColor);
    procedure DrawSatCirc;
    procedure DrawHueLine;
    procedure DrawMarker(x, y: integer);
    procedure SetShowSatCirc(s: boolean);
    procedure SetShowSelCirc(s: boolean);
    procedure SetShowHueLine(s: boolean);
    procedure UpdateCoords;
  protected
    procedure CreateGradient; override;
//    procedure CreateWnd; override;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(x, y: integer); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
  published
    property BrightnessMode default bmValue;
    property SelectedColor default clRed;
    property Hue default 0;
    property Luminance default 127;
    property Saturation default 255;
    property Value default 255;
    property MaxHue default 360;
    property MaxLuminance default 255;
    property MaxSaturation default 255;
    property MaxValue default 255;
    property SaturationCircleColor: TColor read FSatCircColor write SetSatCircColor default clSilver;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property ShowSaturationCircle: boolean read FShowSatCirc write SetShowSatCirc default true;
    property ShowHueLine: boolean read FShowHueLine write SetShowHueLine default true;
    property ShowSelectionCircle: boolean read FShowSelCirc write SetShowSelCirc default true;
    property MarkerStyle default msCrossCirc;
    property OnChange;
  end;

implementation

uses
  mbUtils;

{ THSCirclePicker }

constructor THSCirclePicker.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialBounds(0, 0, 204, 204);
  FHue := 0;
  FSat := 1.0;
  FLum := 0.5;
  FVal := 1.0;
  SetSelectedColor(clRed);
  BrightnessMode := bmValue;
  FSatCircColor := clSilver;
  FHueLineColor := clGray;
  FShowSatCirc := true;
  FShowHueLine := true;
  FShowSelCirc := true;
  MarkerStyle := msCrossCirc;
end;

procedure THSCirclePicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;
                  (*
procedure THSCirclePicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;                *)

procedure THSCirclePicker.DrawSatCirc;
var
  delta: integer;
  radius: integer;
begin
  if not FShowSatCirc then
    exit;
  if (FSat > 0) and (FSat < 1.0) then
  begin
    radius := Min(Width, Height) div 2;
    Canvas.Pen.Color := FSatCircColor;
    Canvas.Brush.Style := bsClear;
    delta := round(radius * FSat);
    Canvas.Ellipse(radius - delta, radius - delta, radius + delta, radius + delta);
  end;
end;

procedure THSCirclePicker.DrawHueLine;
var
  angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  if not FShowHueLine then
    exit;
  radius := Min(Width, Height) div 2;
  if (FHue >= 0) and (FHue <= 1.0) then
  begin
    angle := -FHue * 2 * pi;
    SinCos(angle, sinAngle, cosAngle);
    Canvas.Pen.Color := FHueLineColor;
    Canvas.MoveTo(radius, radius);
    Canvas.LineTo(radius + round(radius*cosAngle), radius + round(radius*sinAngle));
  end;
end;

procedure THSCirclePicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  if not FShowSelCirc then
    exit;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clGray;
  InternalDrawMarker(x, y, c);
end;

function THSCirclePicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle: Double;
  dx, dy, r, radius: integer;
  h, s: double;
begin
  radius := Min(Width, Height) div 2;
  dx := x - Radius;
  dy := y - Radius;

  r := round(sqrt(sqr(dx) + sqr(dy)));
  if r <= radius then
  begin
    angle := 360 + 180 * arctan2(-dy, dx) / pi;
    if angle < 0 then
      angle := angle + 360
    else if angle > 360 then
      angle := angle - 360;
    h := angle / 360;
    s := r / radius;
    Result := HSLVtoColor(h, s, FLum, FVal);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := clNone;
end;

{ Outer loop: Y, Inner loop: X }
function THSCirclePicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  dx, dy: Integer;
  dSq, radiusSq: Integer;
  radius, size: Integer;
  S, H: Double;
begin
  size := FGradientWidth;  // or Height, they are the same...
  radius := size div 2;
  radiusSq := sqr(radius);
  dx := X - radius;
  dy := Y - radius;
  dSq := sqr(dx) + sqr(dy);
  if dSq <= radiusSq then
  begin
    if radius <> 0 then
      S := sqrt(dSq) / radius
    else
      S := 0;
    H := 180 * (1 + arctan2(dx, dy) / pi);  // wp: order (x,y) is correct!
    H := H + 90;
    if H > 360 then H := H - 360;
    Result := HSLVtoColor(H/360, S, FLum, FVal);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := GetDefaultColor(dctBrush);
end;

procedure THSCirclePicker.Paint;
var
  rgn: HRGN;
  R: TRect;
begin
  PaintParentBack(Canvas);
  R := ClientRect;
  R.Right := R.Left + Min(Width, Height);
  R.Bottom := R.Top + Min(Width, Height);
  InflateRect(R, -1, -1);  // Avoid spurious black pixels at the border
  rgn := CreateEllipticRgnIndirect(R);
  SelectClipRgn(Canvas.Handle, rgn);
  Canvas.Draw(0, 0, FBufferBmp);
  DeleteObject(rgn);
  DrawSatCirc;
  DrawHueLine;
  DrawMarker(mx, my);
end;

procedure THSCirclePicker.Resize;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THSCirclePicker.SelectColor(x, y: integer);
var
  angle: Double;
  dx, dy, r, radius: integer;
  H, S: Double;
begin
  mx := x;
  my := y;

  radius := Min(Width, Height) div 2;
  dx := x - radius;
  dy := y - radius;
  r := round(sqrt(sqr(dx) + sqr(dy)));

  if r > radius then  // point outside circle
  begin
    SetSelectedColor(clNone);
    exit;
  end;

  //FSelectedColor := clWhite;         // ????
  angle := 360 + 180*arctan2(-dy, dx) / pi;   // wp: "-y, x" correct? The others have "x, y"
  if angle < 0 then
    angle := angle + 360
  else if angle > 360 then
    angle := angle - 360;
  H := angle / 360;
  if r > radius then
    S := 1.0
  else
    S := r / radius;

  if (H = FHue) and (S = FSat) then
    exit;

  FHue := H;
  FSat := S;
  FSelected := HSLVToColor(FHue, FSat, FLum, FVal);
  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSCirclePicker.SetHueLineColor(c: TColor);
begin
  if FHueLineColor <> c then
  begin
    FHueLineColor := c;
    Invalidate;
  end;
end;

procedure THSCirclePicker.SetRelHue(H: Double);
begin
  if H > 1 then H := H - 1;
  if H < 0 then H := H + 1;
  if FHue <> h then
  begin
    FHue := h;
    FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure THSCirclePicker.SetRelSat(S: Double);
begin
  Clamp(S, 0.0, 1.0);
  if FSat <> S then
  begin
    FSat := s;
    FSelected := HSLVToColor(FHue, FSat, FLum, FVal);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure THSCirclePicker.SetSatCircColor(c: TColor);
begin
  if FSatCircColor <> c then
  begin
    FSatCircColor := c;
    Invalidate;
  end;
end;

procedure THSCirclePicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelected then
    exit;
  ColorToHSLV(c, FHue, FSat, FLum, FVal);
  FSelected := c;
  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSCirclePicker.SetShowHueLine(s: boolean);
begin
  if FShowHueLine <> s then
  begin
    FShowHueLine := s;
    Invalidate;
  end;
end;

procedure THSCirclePicker.SetShowSatCirc(s: boolean);
begin
  if FShowSatCirc <> s then
  begin
    FShowSatCirc := s;
    Invalidate;
  end;
end;

procedure THSCirclePicker.SetShowSelCirc(s: boolean);
begin
  if FShowSelCirc <> s then
  begin
    FShowSelCirc := s;
    Invalidate;
  end;
end;

procedure THSCirclePicker.UpdateCoords;
var
  r, angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  radius := Min(Width, Height) div 2;
  r := -FSat * radius;
  angle := -(FHue * 2 + 1) * pi;
  SinCos(angle, sinAngle, cosAngle);
  mx := round(cosAngle * r) + radius;
  my := round(sinAngle * r) + radius;
end;

end.
