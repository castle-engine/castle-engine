unit RGBCMYKUtils;

interface

// Activate only one of these defines  - see comments below

{.$DEFINE CMYK_FORMULA_1}       // Original formula used by mbColorLib
{$DEFINE CMYK_FORMULA_2}        // Result agrees with OpenOffice

uses
  LCLIntf, Graphics, Math;

function CMYtoColor(C, M, Y: integer): TColor;
procedure RGBtoCMY(clr: TColor; var C, M, Y: integer);

function CMYKToColor (C, M, Y, K: Integer): TColor;
procedure ColorToCMYK(clr: TColor; out C, M, Y, K: integer);

function GetCValue(c: TColor): integer;
function GetMValue(c: TColor): integer;
function GetYValue(c: TColor): integer;
function GetKValue(c: TColor): integer;


implementation

function CMYtoColor(C, M, Y: integer): TColor;
begin
  Result := RGB(255 - C, 255 - M, 255 - Y);
end;

procedure RGBtoCMY(clr: TColor; var C, M, Y: integer);
begin
  C := 255 - GetRValue(clr);
  M := 255 - GetGValue(clr);
  Y := 255 - GetBValue(clr);
end;

{$IFDEF CMYK_FORMULA_1}
//==============================================================================
// Original formulas of mbColorLib
//==============================================================================
function CMYKtoColor(C, M, Y, K: Integer): TColor;
begin
  Result := RGBtoColor(
    (255 - (C + K)) mod 255,  // wp: added mod 255, otherwise the result is nonsense
    (255 - (M + K)) mod 255,
    (255 - (Y + K)) mod 255
  );
end;

procedure ColorToCMYK(clr: TColor; out C, M, Y, K: integer);
begin
  C := 255 - GetRValue(clr);
  M := 255 - GetGValue(clr);
  Y := 255 - GetBValue(clr);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;
{$ENDIF}

{$IFDEF CMYK_FORMULA_2}
//==============================================================================
// Other formulas
// http://www.rapidtables.com/convert/color/cmyk-to-rgb.htm
// or https://stackoverflow.com/questions/2426432/convert-rgb-color-to-cmyk
//
// Result agrees with OpenOffice.
//==============================================================================
function CMYKtoColor(C, M, Y, K: Integer): TColor;
begin
  Result := RGBtoColor(
    (255-C) * (255-K) div 255,
    (255-M) * (255-K) div 255,
    (255-Y) * (255-K) div 255
  );
end;

procedure ColorToCMYK(clr: TColor; out C, M, Y, K: Integer);
var
  r, g, b: Integer;
  r1, g1, b1, c1, m1, y1, k1: Double;
begin
  r := GetRValue(clr);
  g := GetGValue(clr);
  b := GetBValue(clr);
  if (r = 0) and (g = 0) and (b = 0) then
  begin
    C := 0;
    M := 0;
    Y := 0;
    K := 1;
    exit;
  end;
  r1 := r / 255;
  g1 := g / 255;
  b1 := b / 255;
  k1 := MinValue([1-r1, 1-g1, 1-b1]);
  c1 := (1 - r1 - k1) / (1 - k1);
  m1 := (1 - g1 - k1) / (1 - k1);
  y1 := (1 - b1 - k1) / (1 - k1);
  C := round(255 * c1);
  M := round(255 * m1);
  Y := round(255 * y1);
  K := round(255 * k1);
end;
{$ENDIF}

//==============================================================================

function GetCValue(c: TColor): integer;
var
  d: integer;
begin
  ColorToCMYK(c, Result, d, d, d);
end;

function GetMValue(c: TColor): integer;
var
  d: integer;
begin
  ColorToCMYK(c, d, Result, d, d);
end;

function GetYValue(c: TColor): integer;
var
  d: integer;
begin
  ColorToCMYK(c, d, d, Result, d);
end;

function GetKValue(c: TColor): integer;
var
  d: integer;
begin
  ColorToCMYK(c, d, d, d, Result);
end;

end.
