unit mbColorConv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TBrightnessMode = (bmLuminance, bmValue);

{ HSL color model }

function HSLtoColor(H, S, L: double): TColor;
procedure HSLtoRGB(H, S, L: Double; out R, G, B: Integer);

procedure ColortoHSL(c: TColor; out H, S, L: Double);
procedure RGBtoHSL(R, G, B: Integer; out H, S, L: Double);

{ HSV color model }

procedure ColorToHSV(c: TColor; out H, S, V: Double);
procedure RGBtoHSV(R, G, B: Integer; out H, S, V: Double);

function HSVtoColor(H, S, V: Double): TColor;
procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);

{ H, S, L, V extraction }

function GetRelHValue(c: TColor): Double;
function GetRelSValueHSL(c: TColor): Double;
function GetRelSValueHSV(c: TColor): Double;
function GetRelLValue(c: TColor): Double;
function GetRelVValue(c: TColor): Double;


implementation

uses
  Math, LclIntf;

function modulo(x, y: Double): Double;
begin
  Result := x - floor(x / y) * y;
end;

//==============================================================================
//                        HSL color model
//==============================================================================

function HSLToColor(H, S, L: Double): TColor;
var
  R, G, B: Integer;
begin
  HSLtoRGB(H, S, L, R, G, B);
  Result := RGBtoColor(R, G, B);
end;
                                 (*
procedure HSLtoRGB(H, S, L: double; out R, G, B: Integer);
var
  C, X, m: Double;
  rr, gg, bb: Double;
begin
  H := H * 360;
  C := (1 - abs(2*L - 1)) * S;
  X := C * (1 - abs(modulo(H / 60, 2) - 1));
  m := L - C/2;
  if H < 60 then
  begin
    R := round((C + m) * 255);
    G := round((X + m) * 255);
    B := round(m * 255);
  end else
  if H < 120 then
  begin
    R := round((X + m) * 255);
    G := round((C + m) * 255);
    B := round(m * 255);
  end else
  if H < 180 then
  begin
    R := round(m * 255);
    G := round((C + m) * 255);
    B := round((X + m) * 255);
  end else
  if H < 240 then
  begin
    R := round(m * 255);
    G := round((X + m) * 255);
    B := round((C + m) * 255);
  end else
  if H < 300 then
  begin
    R := round((X + m) * 255);
    G := round(m * 255);
    B := round((C + m) * 255);
  end else
  begin
    R := round((C + m) * 255);
    G := round(m * 255);
    B := round((X + m) * 255);
  end;
end;                               *)


procedure HSLtoRGB(H, S, L: double; out R, G, B: Integer);
var
  M1, M2: double;

  function HueToColorValue(Hue: double): byte;
  var
    V : double;
  begin
    if Hue > 10 then
      Hue := Hue + 1;
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2/3 - Hue) * 6
    else
      V := M1;
    Result := round(255 * V)
  end;

begin
  if S = 0 then
  begin
    R := round(255 * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1/3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1/3)
  end;
end;

procedure ColorToHSL(c: TColor; out H, S, L: Double);
begin
  RGBtoHSL(GetRValue(c), GetGValue(c), GetBValue(c), H, S, L);
end;

// From: http://www.rapidtables.com/convert/color/rgb-to-hsl.htm
procedure RGBtoHSL(R, G, B: Integer; out H, S, L: Double);
var
  rr, gg, bb, Cmax, Cmin, delta: double;
begin
  rr := R / 255;
  gg := G / 255;
  bb := B / 255;
  Cmax := MaxValue([rr, gg, bb]);
  Cmin := MinValue([rr, gg, bb]);
  delta := (Cmax - Cmin);
  if delta = 0 then
  begin
    H := 0;
    S := 0;
  end else
  begin
    // Calculate L
    L := (Cmax + Cmin) / 2;

    // Calculate H
    if Cmax = rr then
    begin
      H := modulo((gg - bb) / delta, 6);
      {
      H := ((gg - bb) / delta);
      H := H - floor(H / 6);
      }
      H := H * 60;
    end else
    if Cmax = gg then
      H := 60 * ((bb - rr) / delta + 2)
    else
    if Cmax = bb then
      H := 60 * ((rr - gg) / delta + 4)
    else
      H := 0;
    H := H / 360;

    // Calculate S
    S := delta / (1 - abs(2 * L - 1));
  end;
end;


  (*
procedure RGBtoHSL(R, G, B: Integer; out H, S, L: Double);
var
  rr, gg, bb, D, Cmax, Cmin: double;
begin
  rr := R / 255;
  gg := G / 255;
  bb := B / 255;
  Cmax := MaxValue([rr, gg, bb]);
  Cmin := MinValue([rr, gg, bb]);
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    D := Cmax - Cmin;
    //calc S
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    //calc H
    if R = Cmax then
      H := (gg - bb) / D
    else if G = Cmax then
      H  := 2 + (bb - rr) /D
    else
      H := 4 + (rr - gg) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;
*)

//==============================================================================
//                        HSV color model
//==============================================================================

{ Assumes H, S, V in the range 0..1 and calculates the R, G, B values which are
  returned to be in the range 0..255.
  From: http://axonflux.com/handy-rgb-to-hsl-and-rgb-to-hsv-color-model-c
}
procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);
var
  i: Integer;
  f: Double;
  p, q, t: Double;

  procedure MakeRgb(rr, gg, bb: Double);
  begin
    R := Round(rr * 255);
    G := Round(gg * 255);
    B := Round(bb * 255);
  end;

begin
  i := floor(H * 6);
  f := H * 6 - i;
  p := V * (1 - S);
  q := V * (1 - f*S);
  t := V * (1 - (1 - f) * S);
  case i mod 6 of
    0: MakeRGB(V, t, p);
    1: MakeRGB(q, V, p);
    2: MakeRGB(p, V, t);
    3: MakeRGB(p, q, V);
    4: MakeRGB(t, p, V);
    5: MakeRGB(V, p, q);
    else MakeRGB(0, 0, 0);
  end;
end;

function HSVToColor(H, S, V: Double): TColor;
var
  r, g, b: Integer;
begin
  HSVtoRGB(H, S, V, r, g, b);
  Result := RgbToColor(r, g, b);
end;

{ Assumes R, G, B to be in range 0..255. Calculates H, S, V in range 0..1
  From: http://axonflux.com/handy-rgb-to-hsl-and-rgb-to-hsv-color-model-c }

procedure ColorToHSV(c: TColor; out H, S, V: Double);
begin
  RGBToHSV(GetRValue(c), GetGValue(c), GetBValue(c), H, S, V);
end;

procedure RGBToHSV(R, G, B: Integer; out H, S, V: Double);
var
  rr, gg, bb: Double;
  cmax, cmin, delta: Double;
begin
  rr := R / 255;
  gg := G / 255;
  bb := B / 255;
  cmax := MaxValue([rr, gg, bb]);
  cmin := MinValue([rr, gg, bb]);
  delta := cmax - cmin;
  if delta = 0 then
  begin
    H := 0;
    S := 0;
  end else
  begin
    if cmax = rr then
      H := (gg - bb) / delta + IfThen(gg < bb, 6, 0)
    else if cmax = gg then
      H := (bb - rr) / delta + 2
    else if (cmax = bb) then
      H := (rr -gg) / delta + 4;
    H := H / 6;
    S := delta / cmax;
  end;
  V := cmax;
end;


//==============================================================================
//                      H, S, L, V extraction
//==============================================================================

function GetRelHValue(c: TColor): Double;
var
  H, S, L: Double;
begin
  ColorToHSL(c, H, S, L);  // Could also use HSV - H is the same in both models
  Result := H;
end;

function GetRelSValueHSL(c: TColor): Double;
var
  H, S, L: Double;
begin
  ColorToHSL(c, H, S, L);
  Result := S;
end;

function GetRelSValueHSV(c: TColor): Double;
var
  H, S, V: Double;
begin
  ColorToHSV(c, H, S, V);
  Result := S;
end;

function GetRelLValue(c: TColor): Double;
var
  H, S, L: Double;
begin
  ColorToHSL(c, H, S, L);
  result := L;
end;

function GetRelVValue(c: TColor): Double;
var
  H, S, V: Double;
begin
  ColorToHSV(c, H, S, V);
  Result := V;
end;

end.

