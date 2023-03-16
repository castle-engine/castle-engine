unit RGBHSVUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Math,
  Scanlines;

{ The next four procedures assume H, S, V to be in the range 0..1 }
//procedure ColorToHSV(c: TColor; out H, S, V: Double);
//procedure RGBtoHSV(R, G, B: Integer; out H, S, V: Double);

//function HSVtoColor(H, S, V: Double): TColor;
//procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);

{ These next procedure assume H to be in the range 0..360
  and S, V in the range 0..255 }
procedure RGBtoHSVRange(R, G, B: integer; out H, S, V: integer);
procedure HSVtoRGBRange(H, S, V: Integer; out R, G, B: Integer);
function HSVRangeToColor(H, S, V: Integer): TColor;
function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;

function GetHValue(Color: TColor): integer;
function GetVValue(Color: TColor): integer;
function GetSValue(Color: TColor): integer;


implementation

{ Assumes R, G, B to be in range 0..255. Calculates H, S, V in range 0..1
  From: http://axonflux.com/handy-rgb-to-hsl-and-rgb-to-hsv-color-model-c }
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

procedure ColorToHSV(c: TColor; out H, S, V: Double);
begin
  RGBToHSV(GetRValue(c), GetGValue(c), GetBValue(c), H, S, V);
end;

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


//------------------------------------------------------------------------------

procedure RGBToHSVRange(R, G, B: integer; out H, S, V: integer);
var
  Delta, Min, H1, S1: double;
begin
  Min := MinIntValue([R, G, B]);
  V := MaxIntValue([R, G, B]);
  Delta := V - Min;
  if V =  0.0 then S1 := 0 else S1 := Delta / V;
  if S1  = 0.0 then
    H1 := 0
  else
  begin
    if R = V then
      H1 := 60.0 * (G - B) / Delta
    else if G = V then
      H1 := 120.0 + 60.0 * (B - R) / Delta
    else if B = V then
      H1 := 240.0 + 60.0 * (R - G) / Delta;
    if H1 < 0.0 then H1 := H1 + 360.0;
  end;
  h := round(h1);
  s := round(s1*255);
end;

procedure HSVtoRGBRange(H, S, V: Integer; out R, G, B: Integer);
var
  t: TRGBTriple;
begin
  t := HSVtoRGBTriple(H, S, V);
  R := t.rgbtRed;
  G := t.rgbtGreen;
  B := t.rgbtBlue;
end;

function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
const
  divisor: integer = 255*60;
var
  f, hTemp, p, q, t, VS: integer;
begin
  if H > 360 then H := H - 360;
  if H < 0 then H := H + 360;
  if s = 0 then
    Result := RGBtoRGBTriple(V, V, V)
  else
  begin
    if H = 360 then hTemp := 0 else hTemp := H;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    VS := V*S;
    p := V - VS div 255;
    q := V - (VS*f) div divisor;
    t := V - (VS*(60 - f)) div divisor;
    case hTemp of
      0: Result := RGBtoRGBTriple(V, t, p);
      1: Result := RGBtoRGBTriple(q, V, p);
      2: Result := RGBtoRGBTriple(p, V, t);
      3: Result := RGBtoRGBTriple(p, q, V);
      4: Result := RGBtoRGBTriple(t, p, V);
      5: Result := RGBtoRGBTriple(V, p, q);
    else Result := RGBtoRGBTriple(0,0,0)
    end;
  end;
end;

function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;
const
  divisor: integer = 255*60;
var
  f, hTemp, p, q, t, VS: integer;
begin
  if H > 360 then H := H - 360;
  if H < 0 then H := H + 360;
  if s = 0 then
    Result := RGBtoRGBQuad(V, V, V)
  else
  begin
    if H = 360 then hTemp := 0 else hTemp := H;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    VS := V*S;
    p := V - VS div 255;
    q := V - (VS*f) div divisor;
    t := V - (VS*(60 - f)) div divisor;
    case hTemp of
      0: Result := RGBtoRGBQuad(V, t, p);
      1: Result := RGBtoRGBQuad(q, V, p);
      2: Result := RGBtoRGBQuad(p, V, t);
      3: Result := RGBtoRGBQuad(p, q, V);
      4: Result := RGBtoRGBQuad(t, p, V);
      5: Result := RGBtoRGBQuad(V, p, q);
    else Result := RGBtoRGBQuad(0,0,0)
    end;
  end;
end;

function HSVRangetoColor(H, S, V: integer): TColor;
begin
  Result := RGBTripleToColor(HSVtoRGBTriple(H, S, V));
end;

//------------------------------------------------------------------------------

function GetHValue(Color: TColor): integer;
var
  s, v: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), Result, s, v);
end;

function GetSValue(Color: TColor): integer;
var
  h, v: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, Result, v);
end;

function GetVValue(Color: TColor): integer;
var
  h, s: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, s, Result);
end;

end.
