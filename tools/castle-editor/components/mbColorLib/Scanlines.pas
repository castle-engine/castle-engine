unit Scanlines;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Graphics;

type
  TRGBTripleArray = array [0..65535] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;

  TRGBQuadArray = array [0..65535] of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;

function RGBtoRGBTriple(R, G, B: byte): TRGBTriple;
function RGBtoRGBQuad(R, G, B: byte): TRGBQuad; overload;
function RGBToRGBQuad(c: TColor): TRGBQuad; overload;
function RGBQuadToRGB(q: TRGBQuad): TColor;
function RGBTripleToColor(RGBTriple : TRGBTriple) : TColor;


implementation

function RGBtoRGBTriple(R, G, B: byte): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := R;
    rgbtGreen := G;
    rgbtBlue := B;
  end
end;

function RGBtoRGBQuad(R, G, B: byte): TRGBQuad; overload;
begin
  with Result do
  begin
    rgbRed := R;
    rgbGreen := G;
    rgbBlue := B;
    rgbReserved := 0;
  end
end;

function RGBToRGBQuad(c: TColor): TRGBQuad; overload;
begin
  with Result do
  begin
    rgbRed := GetRValue(c);
    rgbGreen := GetGValue(c);
    rgbBlue := GetBValue(c);
    rgbReserved := 0
  end;
end;

function RGBQuadToRGB(q: TRGBQuad): TColor;
begin
  Result := RGB(q.rgbRed, q.rgbGreen, q.rgbBlue);
end;

function RGBTripleToColor(RGBTriple: TRGBTriple): TColor;
begin
  Result := RGBTriple.rgbtBlue shl 16 + RGBTriple.rgbtGreen shl 8 + RGBTriple.rgbtRed;
end;

end.
 
