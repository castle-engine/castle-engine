{
  Copyright 2011-2017 Lazarus developers and Michalis Kamburelis.

  Parts of this unit are based on LCL GraphUtil unit.
  Luckily, both LCL and Castle Game Engine use the same license,
  LGPL with static linking exception, see the file COPYING.txt.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Canvas utils. }
unit CastleGraphUtil;

interface

uses FpImage, FpCanvas;

type
  TFillStyle =
  (
    fsSurface, // fill till the color (it fills all execpt this color)
    fsBorder   // fill this color (it fills only conneted pixels of this color)
  );

{ Simple FloodFill on any canvas.
  Adjusted from LCL GraphUtil, to work with TFPCustomCanvas. }
procedure FloodFill(Canvas: TFPCustomCanvas; X, Y: Integer; lColor: TFPColor;
  FillStyle: TFillStyle);

{ Random TFPColor value that isn't too dark. }
function RandomLightFPColor: TFPColor;

implementation

uses Types;

{$I norqcheckbegin.inc}

procedure FloodFill(Canvas: TFPCustomCanvas; X, Y: Integer; lColor: TFPColor;
  FillStyle: TFillStyle);

//Written by Chris Rorden
// Very slow, because uses Canvas.Pixels.
//A simple first-in-first-out circular buffer (the queue) for flood-filling contiguous voxels.
//This algorithm avoids stack problems associated simple recursive algorithms
//http://steve.hollasch.net/cgindex/polygons/floodfill.html [^]

type
  ByteRA = array [1..1] of byte;
  Bytep = ^ByteRA;
  LongIntRA = array [1..1] of LongInt;
  LongIntp = ^LongIntRA;

const
  kFill = 0; //pixels we will want to flood fill
  kFillable = 128; //voxels we might flood fill
  kUnfillable = 255; //voxels we can not flood fill
var
  lWid,lHt,lQSz,lQHead,lQTail: integer;
  lQRA: LongIntP;
  lMaskRA: ByteP;

  procedure IncQra(var lVal, lQSz: integer);//nested inside FloodFill
  begin
      inc(lVal);
      if lVal >= lQSz then
         lVal := 1;
  end; //nested Proc IncQra

  function Pos2XY (lPos: integer): TPoint;
  begin
      result.X := ((lPos-1) mod lWid)+1; //horizontal position
      result.Y := ((lPos-1) div lWid)+1; //vertical position
  end; //nested Proc Pos2XY

  procedure TestPixel(lPos: integer);
  begin
       if (lMaskRA^[lPos]=kFillable) then begin
          lMaskRA^[lPos] := kFill;
          lQra^[lQHead] := lPos;
          incQra(lQHead,lQSz);
       end;
  end; //nested Proc TestPixel

  procedure RetirePixel; //nested inside FloodFill
  var
     lVal: integer;
     lXY : TPoint;
  begin
     lVal := lQra^[lQTail];
     lXY := Pos2XY(lVal);
     if lXY.Y > 1 then
          TestPixel (lVal-lWid);//pixel above
     if lXY.Y < lHt then
        TestPixel (lVal+lWid);//pixel below
     if lXY.X > 1 then
          TestPixel (lVal-1); //pixel to left
     if lXY.X < lWid then
        TestPixel (lVal+1); //pixel to right
     incQra(lQTail,lQSz); //done with this pixel
  end; //nested proc RetirePixel

var
   lTargetColorVal,lDefaultVal: byte;
   lX,lY,lPos: integer;
   lBrushColor: TFPColor;
begin //FloodFill
  if FillStyle = fsSurface then begin
     //fill only target color with brush - bounded by nontarget color.
     if Canvas.Colors[X,Y] <> lColor then exit;
     lTargetColorVal := kFillable;
     lDefaultVal := kUnfillable;
  end else begin //fsBorder
      //fill non-target color with brush - bounded by target-color
     if Canvas.Colors[X,Y] = lColor then exit;
     lTargetColorVal := kUnfillable;
     lDefaultVal := kFillable;
  end;
  //if (lPt < 1) or (lPt > lMaskSz) or (lMaskP[lPt] <> 128) then exit;
  lHt := Canvas.Height;
  lWid := Canvas.Width;
  lQSz := lHt * lWid;
  //Qsz should be more than the most possible simultaneously active pixels
  //Worst case scenario is a click at the center of a 3x3 image: all 9 pixels will be active simultaneously
  //for larger images, only a tiny fraction of pixels will be active at one instance.
  //perhaps lQSz = ((lHt*lWid) div 4) + 32; would be safe and more memory efficient
  if (lHt < 1) or (lWid < 1) then exit;
  getmem(lQra,lQSz*sizeof(longint)); //very wasteful -
  getmem(lMaskRA,lHt*lWid*sizeof(byte));
  for lPos := 1 to (lHt*lWid) do
      lMaskRA^[lPos] := lDefaultVal; //assume all voxels are non targets
  lPos := 0;
  // MG: it is very slow to access the whole (!) canvas with pixels
  for lY := 0 to (lHt-1) do
      for lX := 0 to (lWid-1) do begin
          lPos := lPos + 1;
          if Canvas.Colors[lX,lY] = lColor then
             lMaskRA^[lPos] := lTargetColorVal;
      end;
  lQHead := 2;
  lQTail := 1;
  lQra^[lQTail] := ((Y * lWid)+X+1); //NOTE: both X and Y start from 0 not 1
  lMaskRA^[lQra^[lQTail]] := kFill;
  RetirePixel;
  while lQHead <> lQTail do
        RetirePixel;
  lBrushColor := Canvas.Brush.FPColor;
  lPos := 0;
  for lY := 0 to (lHt-1) do
      for lX := 0 to (lWid-1) do begin
          lPos := lPos + 1;
          if lMaskRA^[lPos] = kFill then
             Canvas.Colors[lX,lY] := lBrushColor;
      end;
  freemem(lMaskRA);
  freemem(lQra);
end;

{$I norqcheckend.inc}

function RandomLightFPColor: TFPColor;
begin
  repeat
    Result := FPColor(Random($FFFF), Random($FFFF), Random($FFFF));
  until ( (Result.Red / High(Word)) +
          (Result.Green / High(Word)) +
          (Result.Blue / High(Word)) ) / 3.0 > 0.2;
end;

end.
