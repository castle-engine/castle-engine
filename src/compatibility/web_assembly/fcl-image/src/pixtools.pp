{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Pixel drawing routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit PixTools;

interface

uses classes, FPCanvas, FPimage;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);

implementation

uses clipping;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  FillRectangleColor (Canv, x1,y1, x2,y2, Canv.Brush.FPColor);
end;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var x,y : integer;
begin
  SortRect (x1,y1, x2,y2);
  with Canv do
    begin
    for x := x1 to x2 do
      for y := y1 to y2 do
        DrawPixel(x,y,color);
    end;
end;

{procedure DrawSolidPolyLine (Canv : TFPCustomCanvas; points:array of TPoint; close:boolean);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  with Canv do
    begin
    for r := i+1 to a do
      begin
      Line (p.x, p.y, points[r].x, points[r].y);
      p := points[r];
      end;
    if close then
      Line (p.x,p.y, points[i].x,points[i].y);
    end;
end;
}
type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);

procedure PutPixelCopy(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    DrawPixel(x,y,color);
end;

procedure PutPixelXor(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] xor color;
end;

procedure PutPixelOr(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] or color;
end;

procedure PutPixelAnd(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] and color;
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  DrawSolidLine (Canv, x1,y1, x2,y2, Canv.Pen.FPColor);
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  with canv.pen do
    case mode of
      pmMerge : PutPixelProc := @PutPixelAnd;
      pmMask : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

type
  TLinePoints = array[0..PatternBitCount-1] of boolean;
  PLinePoints = ^TLinePoints;

procedure PatternToPoints (const APattern:TPenPattern; LinePoints:PLinePoints);
var r : integer;
    i : longword;
begin
  i := 1;
  for r := PatternBitCount-1 downto 1 do
    begin
    LinePoints^[r] := (APattern and i) <> 0;
    i := i shl 1;
    end;
  LinePoints^[0] := (APattern and i) <> 0;
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
begin
  DrawPatternLine (Canv, x1,y1, x2,y2, pattern, Canv.Pen.FPColor);
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
// Is copy of DrawSolidLine with paterns added. Not the same procedure for faster solid lines
var LinePoints : TLinePoints;
    PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        if LinePoints[x mod PatternBitCount] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        if LinePoints[y mod PatternBitCount] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      if LinePoints[r mod PatternBitCount] then
        PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  PatternToPoints (pattern, @LinePoints);
  with canv.pen do
    case mode of
      pmMask : PutPixelProc := @PutPixelAnd;
      pmMerge : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashHorizontal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var y : integer;
begin
  with rect do
    begin
    y := Width + top;
    while y <= bottom do
      begin
      DrawSolidLine (Canv, left,y, right,y, c);
      inc (y,Width);
      end
    end;
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashVertical (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var x : integer;
begin
  with rect do
    begin
    x := Width + left;
    while x <= right do
      begin
      DrawSolidLine (Canv, x,top, x,bottom, c);
      inc (x, Width);
      end;
    end;
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashDiagonal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start + current - max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := top + Width;
    rx := left + Width;
    while (rx < right) and (ry < bottom) do
      begin
      DrawSolidLine (Canv, left,ry, rx,top, c);
      inc (rx, Width);
      inc (ry, Width);
      end;
    // check which turn need to be taken: left-bottom, right-top, or both
    if (rx >= right) then
      begin
      if (ry >= bottom) then
        begin // Both corners reached
        r := CheckCorner (rx, right, top);
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, top);
        while (ry < bottom) do
          begin
          DrawSolidLine (Canv, left,ry, right,r, c);
          inc (r, Width);
          inc (ry, Width);
          end;
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      end
    else
      if (ry >= bottom) then
        begin  // fill horizontal
        r := checkCorner (ry, bottom, left);
        while (rx <= right) do
          begin
          DrawSolidLine (Canv, r,bottom, rx,top, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, top);
        rx := r;
        end;
    while (rx < right) do  // fill lower right corner
      begin
      DrawSolidLine (Canv, rx,bottom, right,ry, c);
      inc (rx, Width);
      inc (ry, Width);
      end;
    end;
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashBackDiagonal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
  function CheckInversCorner (Current, min, start : integer) : integer;
  begin
    if Current < min then
      result := Start - current + min
    else
      result := Start;
  end;
  function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start - current + max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := bottom - Width;
    rx := left + Width;
    while (rx < right) and (ry > top) do
      begin
      DrawSolidLine (Canv, left,ry, rx,bottom, c);
      inc (rx, Width);
      dec (ry, Width);
      end;
    // check which turn need to be taken: left-top, right-bottom, or both
    if (rx >= right) then
      begin
      if (ry <= top) then
        begin // Both corners reached
        r := CheckCorner (rx, right, bottom);
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, bottom);
        while (ry > top) do
          begin
          DrawSolidLine (Canv, left,ry, right,r, c);
          dec (r, Width);
          dec (ry, Width);
          end;
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      end
    else
      if (ry <= top) then
        begin  // fill horizontal
        r := checkInversCorner (ry, top, left);
        while (rx < right) do
          begin
          DrawSolidLine (Canv, r,top, rx,bottom, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, bottom);
        rx := r;
        end;
    while (rx < right) do  // fill upper right corner
      begin
      DrawSolidLine (Canv, rx,top, right,ry, c);
      inc (rx, Width);
      dec (ry, Width);
      end;
    end;
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
begin
  FillRectanglePattern (Canv, x1,y1, x2,y2, pattern, Canv.Brush.FPColor);
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
var r : integer;
begin
  for r := y1 to y2 do
    DrawPatternLine (Canv, x1,r, x2,r, pattern[r mod PatternBitCount], color);
end;

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.DrawPixel(x,y, colors[x mod width, y mod height]);
end;

procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.DrawPixel(x,y, colors[(x-x1) mod width, (y-y1) mod height]);
end;

type
  TFuncSetColor = procedure (Canv:TFPCustomCanvas; x,y:integer; data:pointer);

  PDoneRec = ^TDoneRec;
  TDoneRec = record
    x, min, max : integer;
    next : PDoneRec;
  end;

  PFloodFillData = ^TFloodFillData;
  TFloodFillData = record
    Canv : TFPCustomCanvas;
    ReplColor : TFPColor;
    SetColor : TFuncSetColor;
    ExtraData : pointer;
    DoneList : TList;
  end;

function FindDoneIndex (const data:PFloodFillData; x:integer; out index:integer):boolean;
begin
  with data^.DoneList do
    begin
    index := 0;
    while (index < count) and (PDoneRec(items[index])^.x <> x) do
      inc (index);
    result := (index < count) and (PDoneRec(items[index])^.x = x);
    end;
end;

procedure FreeDoneList (const data:TFloodFillData);
  procedure FreeList (p:PDoneRec);
  var n : PDoneRec;
  begin
    while assigned(p) do
      begin
      n := p^.Next;
      dispose (p);
      p := n;
      end;
  end;
var r : integer;
begin
  with data do
  for r := 0 to DoneList.Count-1 do
    FreeList (PDoneRec(DoneList[r]));
end;

procedure CheckFloodFillColor (x,top,bottom,Direction:integer; data:PFloodFillData);

  procedure CheckRange;
  var r,t,b : integer;
  begin
    t := top;
    b := top -1;
    for r := top to bottom do
      with data^ do
        begin
        if canv.colors[x,r] = ReplColor then
          begin
          b := r;
          SetColor(Canv,x,r,ExtraData);
          end
        else
          begin
          if t < r then
            CheckFloodFillColor (x+Direction, t, r-1, Direction, data);
          t := r + 1;
          end;
        end;
    if t <= b then
      CheckFloodFillColor (x+Direction, t, b, Direction, data);
  end;

  procedure CheckAboveRange;
  var t,b : integer;
  begin
    with data^ do
      begin
      t := top - 1;
      while (t >= 0) and (Canv.colors[x,t]=ReplColor) do
        begin
        SetColor(Canv, x,t, ExtraData);
        dec (t);
        end;
      t := t + 1;
      b := top - 1;
      if t <= b then
        begin
        CheckFloodFillColor (x-1, t, b, -1, data);
        CheckFloodFillColor (x+1, t, b, 1, data);
        end;
      end;
  end;

  procedure CheckBelowRange;
  var r,t,b : integer;
  begin
    with data^ do
      begin
      r := Canv.Height;
      b := bottom + 1;
      t := b;
      while (b < r) and (Canv.colors[x,b]=ReplColor) do
        begin
        SetColor (Canv,x,b,ExtraData);
        inc (b);
        end;
      b := b - 1;
      if t <= b then
        begin
        CheckFloodFillColor (x-1, t, b, -1, data);
        CheckFloodFillColor (x+1, t, b, 1, data);
        end;
      end;
  end;

var DoAbove, DoBelow : boolean;

begin
  with data^ do
    begin
    if (x >= Canv.width) or (x < 0) then
      Exit;
    if top < 0 then
      top := 0;
    if bottom >= Canv.Height then
      bottom := Canv.Height-1;
    DoAbove := (Canv.colors[x,top] = ReplColor);
    DoBelow := (Canv.colors[x,bottom] = ReplColor);
    end;
  CheckRange;
  if DoAbove then
    CheckAboveRange;
  if DoBelow then
    CheckBelowRange;
end;

procedure CheckFloodFill (x,top,bottom,Direction:integer; data:PFloodFillData);
var beforetop, ontop, chain, myrec : PDoneRec;
    doneindex : integer;

  procedure CheckRange;
  var r,t,b : integer;
      n : PDoneRec;
  begin
    ontop := nil;
    beforetop := nil;
    n := chain;
    while (n <> nil) and (n^.min <= top) do
      begin
      beforetop := ontop;
      ontop := n;
      n := n^.next;
      end;
    if assigned(ontop) and (ontop^.max < top) then
      begin
      beforetop := ontop;
      ontop := nil;
      end;
    // ontop is: nil OR rec before top OR rec containing top
    if assigned(ontop) then
      begin
      t := ontop^.max + 1;
      myrec := ontop;
      end
    else
      begin
      t := top;
      new(myrec);
      myrec^.x := x;
      myrec^.min := top;
      myrec^.max := top;
      myrec^.Next := n;
      if assigned(beforetop) then
        beforetop^.next := myrec
      else
        begin
        with data^.DoneList do
          if DoneIndex < Count then
            Items[DoneIndex] := myrec
          else
            Add (myrec);
        chain := myrec;
        end;
      end;
    ontop := myrec;
    // ontop is rec containing the top
    b := t-1;
    r := t;
    while (r <= bottom) do
      begin
      with data^ do
        begin
        if canv.colors[x,r] = ReplColor then
          begin
          b := r;
          SetColor(Canv,x,r,ExtraData);
          end
        else
          begin
          if t < r then
            begin
            myrec^.max := r;
            CheckFloodFill (x+Direction, t, r-1, Direction, data);
            end;
          t := r + 1;
          end;
        inc (r);
        end;
      if assigned(n) and (r >= n^.min) then
        begin
        if t < r then
          begin
          myrec^.max := n^.min-1;
          CheckFloodFill (x+Direction, t, r-1, Direction, data);
          end;
        while assigned(n) and (r >= n^.min) do
          begin
          myrec := n;
          r := myrec^.max + 1;
          n := n^.next;
          end;
        t := r;
        end;
      end;
    myrec^.max := r - 1;
    if t <= b then
      CheckFloodFill (x+Direction, t, b, Direction, data);
  end;

  procedure CheckAboveRange (highest:integer);
  var t,b : integer;
  begin
    with data^ do
      begin
      t := top - 1;
      while (t >= highest) and (Canv.colors[x,t]=ReplColor) do
        begin
        SetColor(Canv, x,t, ExtraData);
        dec (t);
        end;
      t := t + 1;
      b := top - 1;
      if t <= b then
        begin
        ontop^.min := t - 1;
        CheckFloodFill (x-1, t, b, -1, data);
        CheckFloodFill (x+1, t, b, 1, data);
        end;
      end;
  end;

  procedure CheckBelowRange (lowest : integer);
  var t,b : integer;
  begin
    with data^ do
      begin
      b := bottom + 1;
      t := b;
      while (b <= lowest) and (Canv.colors[x,b]=ReplColor) do
        begin
        SetColor (Canv,x,b,ExtraData);
        inc (b);
        end;
      b := b - 1;
      if t <= b then
        begin
        myrec^.max := b+1;
        CheckFloodFill (x-1, t, b, -1, data);
        CheckFloodFill (x+1, t, b, 1, data);
        end;
      end;
  end;

var DoAbove, DoBelow : boolean;
    m : integer;
begin
  with data^ do
    begin
    if (x >= Canv.width) or (x < 0) then
      Exit;
    if top < 0 then
      top := 0;
    if bottom >= Canv.Height then
      bottom := Canv.Height-1;
    DoAbove := (Canv.colors[x,top] = ReplColor);
    DoBelow := (Canv.colors[x,bottom] = ReplColor);
    end;
  if FindDoneIndex (data, x, DoneIndex) then
    begin
    chain := PDoneRec(data^.DoneList[DoneIndex]);
    myrec := chain;
    while assigned(myrec) do
      with myrec^ do
        myrec := next;
    end
  else
    chain := nil;
  CheckRange;
  // ontop: rec containing top
  // myrec: rec containing bottom
  if DoAbove and (ontop^.min = top) then
    begin
    if assigned (beforetop) then
      m := beforetop^.max + 1
    else
      m := 0;
    CheckAboveRange (m);
    end;
  if DoBelow and (myrec^.max = bottom) then
    begin
    if assigned (myrec^.next) then
      m := myrec^.next^.min - 1
    else
      m := data^.Canv.Height - 1;
    CheckBelowRange (m);
    end;
end;

procedure SetFloodColor (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
begin
  Canv.DrawPixel(x,y, PFPColor(data)^);
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
var d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodColor;
  d.ExtraData := @color;
  CheckFloodFillColor (x, y, y, 1, @d);
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
begin
  FillFloodColor (Canv, x, y, Canv.Brush.FPColor);
end;

type
  TBoolPlane = array[0..PatternBitCount-1] of TLinePoints;
  TFloodPatternRec = record
    plane : TBoolPlane;
    color : TFPColor;
  end;
  PFloodPatternRec = ^TFloodPatternRec;

procedure SetFloodPattern (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var p : PFloodPatternRec;
begin
  p := PFloodPatternRec(data);
  if p^.plane[x mod PatternBitCount, y mod PatternBitCount] then
    Canv.colors[x,y] := p^.color;
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
var rec : TFloodPatternRec;
    d : TFloodFillData;

  procedure FillPattern;
  var r : integer;
  begin
    for r := 0 to PatternBitCount-1 do
      PatternToPoints (pattern[r], @rec.plane[r]);
  end;

begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodPattern;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  try
    FillPattern;
    rec.color := Color;
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
begin
  FillFloodPattern (Canv, x, y, pattern, Canv.Brush.FPColor);
end;

type
  TFloodHashRec = record
    color : TFPColor;
    width : integer;
  end;
  PFloodHashRec = ^TFloodHashRec;

procedure SetFloodHashHor(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (y mod r^.width) = 0 then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashVer(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (x mod r^.width) = 0 then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) + (y mod w)) = (w - 1) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashBDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if (x mod w) = (y mod w) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) = 0) or ((y mod w) = 0) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashDiagCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ( (x mod w) = (y mod w) ) or
     ( ((x mod w) + (y mod w)) = (w - 1) ) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure FillFloodHash (Canv:TFPCustomCanvas; x,y:integer; width:integer; SetHashColor:TFuncSetColor; const c:TFPColor);
var rec : TFloodHashRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := SetHashColor;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  rec.color := c;
  rec.width := Width;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashHor, c);
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashHorizontal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashVer, c);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashVertical (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiag, c);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashBDiag, c);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashBackDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiagCross, c);
end;

procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashCross, c);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

type
  TFloodImageRec = record
    xo,yo : integer;
    image : TFPCustomImage;
  end;
  PFloodImageRec = ^TFloodImageRec;

procedure SetFloodImage (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
begin
  r := PFloodImageRec(data);
  with r^.image do
    Canv.DrawPixel(x,y,colors[x mod width, y mod height]);
end;

procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodImage;
  d.ExtraData := @rec;
  d.DoneList := Tlist.Create;
  rec.image := image;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure SetFloodImageRel (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
    xi, yi : integer;
begin
  r := PFloodImageRec(data);
  with r^, image do
    begin
    xi := (x - xo) mod width;
    if xi < 0 then
      xi := width - xi;
    yi := (y - yo) mod height;
    if yi < 0 then
      yi := height - yi;
    Canv.DrawPixel(x,y,colors[xi,yi]);
    end;
end;

procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodImageRel;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  rec.image := image;
  rec.xo := x;
  rec.yo := y;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

end.
