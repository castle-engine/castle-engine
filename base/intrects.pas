{
  Copyright 2004 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Extremely simple unit with basic operations on integer rectangles.)

  Does not use Types unit with TRect --- I do not like all
  trash from Types unit, and I want to keep some compatibility with
  my types in VectorMath. }

unit IntRects;

interface

uses VectorMath;

type
  { Must be

@preformatted(
  Rect[0,0] <= Rect [1,0],
  Rect[0,1] <= Rect [1,1]
)

    If the exact pixel interpretation is required, TIntRect should
    be treated as a set of points (x,y) where

@preformatted(
  Rect[0,0] <= x < Rect [1,0]
  Rect[0,1] <= y < Rect [1,1]
)

    In such situations Rect may be considered empty when

@preformatted(
  (Rect[0,0] = Rect [1,0]) or (Rect[0,1] = Rect [1,1])
) }
  TIntRect = array[0..1]of TVector2Integer;

const
  IntRectEmpty: TIntRect = ((0, 0), (0, 0));

function IntRect(X1, Y1, X2, Y2:Integer):TIntRect;

function RectsEqual(const R1, R2:TIntRect):boolean;

function RectWidth(const r:TIntRect):Cardinal;
function RectHeight(const r:TIntRect):Cardinal;

{ It's grow, or shrink (when GrowValue < 0).
  Just remember to preserve basic TIntRect type assumptions
  (Rect[0,0] <= Rect [1,0], Rect[0,1] <= Rect [1,1]),
  so don't shrink it too much. }
function GrowRect(const r:TIntRect; GrowValue:Integer):TIntRect;

{ Returns TIntRect with Width = W, Height = H and centered on R. }
function CenteredRect(const R:TIntRect; w,h:Cardinal):TIntRect;

function PointInRect(const v:TVector2Integer; const r:TIntRect):boolean; overload;
function PointInRect(const x,y:Integer; const r:TIntRect):boolean; overload;

function IntRectToNiceStr(const R:TIntRect):string;

implementation

uses SysUtils;

{ Some simple functions here are not written elegantly,
  some of them could use each other.
  Instead, they are sligtly optimized and their code is expanded
  (aka hand-made inline functions). }

function IntRect(X1, Y1, X2, Y2:Integer):TIntRect;
begin
 Result[0,0]:=X1;
 Result[0,1]:=Y1;
 Result[1,0]:=X2;
 Result[1,1]:=Y2;
end;

function RectsEqual(const R1, R2:TIntRect):boolean;
begin
 { instead of checking (R1[0,0] = R2[0,0]) and (R1[0,1] = R2[0,1]) ...
   we optimize a little }
 Result:=CompareMem(@R1, @R2, SizeOf(TIntRect));
end;

function RectWidth(const r:TIntRect):Cardinal;
begin
 Result:=r[1,0] - r[0,0];
end;

function RectHeight(const r:TIntRect):Cardinal;
begin
 Result:=r[1,1] - r[0,1];
end;

function GrowRect(const r:TIntRect; GrowValue:Integer):TIntRect;
begin
 Result[0,0] := R[0,0] - GrowValue;
 Result[0,1] := R[0,1] - GrowValue;
 Result[1,0] := R[1,0] + GrowValue;
 Result[1,1] := R[1,1] + GrowValue;
end;

function CenteredRect(const R:TIntRect; w,h:Cardinal):TIntRect;
begin
 { We're casting W,H to integer. They are declared as Cardinal only
   to produce some compiler RunTime checks in debug mode. }
 Result[0,0]:=R[0,0] + (R[1,0] - R[0,0] - Integer(W)) div 2;
 Result[0,1]:=R[0,1] + (R[1,1] - R[0,1] - Integer(H)) div 2;
 Result[1,0]:=Result[0,0] + Integer(W);
 Result[1,1]:=Result[0,1] + Integer(H);
end;

function PointInRect(const v:TVector2Integer; const r:TIntRect):boolean;
begin
 Result:=(r[0,0] <= v[0]) and (v[0] < r[1,0]) and
         (r[0,1] <= v[1]) and (v[1] < r[1,1]);
end;

function PointInRect(const x,y:Integer; const r:TIntRect):boolean;
begin
 Result:=(r[0,0] <= x) and (x < r[1,0]) and
         (r[0,1] <= y) and (y < r[1,1]);
end;

function IntRectToNiceStr(const R:TIntRect):string;
begin
 Result:=Format('(%d,%d)-(%d,%d)', [R[0,0], R[0,1], R[1,0], R[1,1]]);
end;

end.