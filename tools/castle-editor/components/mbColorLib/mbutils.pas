unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf;

procedure Clamp(var AValue: Integer; AMin, AMax: Integer); overload;
procedure Clamp(var AValue: Double; AMin, AMax: Double); overload;
procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
function PointInCircle(p: TPoint; Size: integer): boolean;
function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;

function HighContrastColor(AColor: TColor): TColor;

function HeightOfRect(R: TRect): Integer;
function WidthOfRect(R: TRect): Integer;
function IsEmptyRect(R: TRect): Boolean;

const
  EMPTY_RECT: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
  TWO_PI = 2.0 * pi;

implementation

procedure Clamp(var AValue: integer; AMin, AMax: integer);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

procedure Clamp(var AValue: Double; AMin, AMax: Double);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
begin
  while X1 <= X2 do begin
    ACanvas.Pixels[X1, Y] := AColor;
    inc(X1, 2);
  end;
end;

function PointInCircle(p: TPoint; Size: integer): boolean;
var
  r: integer;
begin
  r := size div 2;
  Result := (sqr(p.x - r) + sqr(p.y - r) <= sqr(r));
end;

function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;
begin
  Result := sqr(p.x - ctr.x) + sqr(p.y - ctr.y) <= sqr(Radius);
end;

function HeightOfRect(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function WidthOfRect(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function IsEmptyRect(R: TRect): Boolean;
begin
  Result := (R.Left = -1) and (R.Top = -1) and (R.Right = -1) and (R.Bottom = -1);
end;

function HighContrastColor(AColor: TColor): TColor;
begin
  if GetRValue(AColor) + GetGValue(AColor) + GetBValue(AColor) > 3*128 then
    Result := clBlack
  else
    Result := clWhite;
end;

end.

