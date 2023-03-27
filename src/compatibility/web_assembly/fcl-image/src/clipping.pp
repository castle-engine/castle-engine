{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Clipping support.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit Clipping;

interface

uses classes;

procedure SortRect (var rect : TRect);
procedure SortRect (var left,top, right,bottom : integer);
function PointInside (const x,y:integer; bounds:TRect) : boolean;

Function CheckRectClipping (ClipRect:TRect; var Rect:Trect) : Boolean;
Function CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer) : Boolean;
procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);

implementation

procedure SortRect (var rect : TRect);
begin
  with rect do
    SortRect (left,top, right,bottom);
end;

procedure SortRect (var left,top, right,bottom : integer);
var r : integer;
begin
  if left > right then
    begin
    r := left;
    left := right;
    right := r;
    end;
  if top > bottom then
    begin
    r := top;
    top := bottom;
    bottom := r;
    end;
end;

function PointInside (const x,y:integer; bounds:TRect) : boolean;
begin
  SortRect (bounds);
  with Bounds do
    result := (x >= left) and (x <= right) and
              (y >= top) and (y <= bottom);
end;

Function CheckRectClipping (ClipRect:TRect; var Rect:Trect) : Boolean;
begin
  with ClipRect do
    Result:=CheckRectClipping (ClipRect, left,top,right,bottom);
end;

Function CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer) : boolean;

  procedure ClearRect;
  begin
    x1 := -1;
    x2 := -1;
    y1 := -1;
    y2 := -1;
  end;
begin
  Result:=true;
  SortRect (ClipRect);
  SortRect (x1,y1, x2,y2);

  with ClipRect do
    begin
    if ( x1 < Left ) then // left side needs to be clipped
      x1 := left;
    if ( x2 > right ) then // right side needs to be clipped
      x2 := right;
    if ( y1 < top ) then // top side needs to be clipped
      y1 := top;
    if ( y2 > bottom ) then // bottom side needs to be clipped
      y2 := bottom;
    if (x1 > x2) or (y1 > y2) then
      begin
      ClearRect;
      Result:=False;
      end;
    end;
end;

procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
var a,b : single;
    Calculated : boolean;
    xdiff,n : integer;
  procedure CalcLine;
    begin
    if not Calculated then
      begin
      xdiff := (x1-x2);
      a := (y1-y2) / xdiff;
      b := (x1*y2 - x2*y1) / xdiff;
      Calculated := true;
      end;
    end;
  procedure ClearLine;
    begin
    x1 := -1;
    y1 := -1;
    x2 := -1;
    y2 := -1;
    end;
begin
  Calculated := false;
  SortRect (ClipRect);
  xdiff := (x1-x2);
  with ClipRect do
    if xdiff = 0 then
      begin  // vertical line
      if y1 > bottom then
        y1 := bottom
      else if y1 < top then
        y1 := top;
      if y2 > bottom then
        y2 := bottom
      else if y2 < top then
        y2 := top;
      end
    else if (y1-y2) = 0 then
      begin  // horizontal line
      if x1 < left then
        x1 := left
      else if x1 > right then
        x1 := right;
      if x2 < left then
        x2 := left
      else if x2 > right then
        x2 := right;
      end
    else
      if ( (y1 < top) and (y2 < top) ) or
         ( (y1 > bottom) and (y2 > bottom) ) or
         ( (x1 > right) and (x2 > right) ) or
         ( (x1 < left) and (x2 < left) ) then
        ClearLine // completely outside ClipRect
      else
        begin
        if (y1 < top) or (y2 < top) then
          begin
          CalcLine;
          n := round ((top - b) / a);
          if (n >= left) and (n <= right) then
            if (y1 < top) then
              begin
              x1 := n;
              y1 := top;
              end
            else
              begin
              x2 := n;
              y2 := top;
              end;
          end;
        if (y1 > bottom) or (y2 > bottom) then
          begin
          CalcLine;
          n := round ((bottom - b) / a);
          if (n >= left) and (n <= right) then
            if (y1 > bottom) then
              begin
              x1 := n;
              y1 := bottom;
              end
            else
              begin
              x2 := n;
              y2 := bottom;
              end;
          end;
        if (x1 < left) or (x2 < left) then
          begin
          CalcLine;
          n := round ((left * a) + b);
          if (n <= bottom) and (n >= top) then
            if (x1 < left) then
              begin
              x1 := left;
              y1 := n;
              end
            else
              begin
              x2 := left;
              y2 := n;
              end;
          end;
        if (x1 > right) or (x2 > right) then
          begin
          CalcLine;
          n := round ((right * a) + b);
          if (n <= bottom) and (n >= top) then
            if (x1 > right) then
              begin
              x1 := right;
              y1 := n;
              end
            else
              begin
              x2 := right;
              y2 := n;
              end;
          end;
        end;
end;

end.
