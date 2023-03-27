{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Drawing of ellipses and arcs, and filling ellipses and pies.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit Ellipses;

interface

uses classes, FPImage, FPCanvas;

procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; const c:TFPColor);
procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Width:integer; const c:TFPColor);
procedure DrawPatternEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TPenPattern; const c:TFPColor);
procedure FillEllipseColor (Canv:TFPCustomCanvas; const Bounds:TRect; const c:TFPColor);
procedure FillEllipsePattern (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TBrushPattern; const c:TFPColor);
procedure FillEllipseHashHorizontal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashVertical (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashBackDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashDiagCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseImage (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
procedure FillEllipseImageRel (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);

type

  PEllipseInfoData = ^TEllipseInfoData;
  TEllipseInfoData = record
    x, ytopmax, ytopmin, ybotmax, ybotmin : integer;
    OnlyTop : boolean;
  end;

  TEllipseInfo = class
  private
    fcx, fcy, frx,fry : double;
    InfoList : TList;
    procedure FreeList;
    procedure ClearList;
    function FindXIndex (x:integer) : integer;
    procedure PrepareCalculation (out np:integer; out delta:real);
    function NewInfoRec (anX:integer) : PEllipseInfoData;
    procedure CalculateCircular (const b:TRect; out x,y,rx,ry:real);
  public
    constructor create;
    destructor destroy; override;
    function GetInfoForX (x:integer; var ytopmax,ytopmin,ybotmax,ybotmin:integer):boolean;
    function GetInfoForX (x:integer; var Info:PEllipseInfoData):boolean;
    procedure GatherEllipseInfo (const bounds:TRect);
    property cx : double read fcx; // center point
    property cy : double read fcy;
    property rhor : double read frx; // radius
    property rver : double read fry;
    { only usable when created with GatherArcInfo }
  end;

implementation

constructor TEllipseInfo.Create;
begin
  inherited;
  InfoList := TList.Create;
end;

destructor TEllipseInfo.Destroy;
begin
  FreeList;
  inherited;
end;

procedure TEllipseInfo.ClearList;
var r : integer;
    d : PEllipseInfoData;
begin
  if assigned (InfoList) then
    begin
    for r := 0 to infolist.count-1 do
      begin
      d := PEllipseInfoData(InfoList[r]);
      dispose (d);
      end;
    InfoList.clear;
    end;
end;

procedure TEllipseInfo.FreeList;
begin
  if assigned (InfoList) then
    begin
    ClearList;
    InfoList.Free;
    InfoList := nil;
    end;
end;

function TEllipseInfo.GetInfoForX (x:integer; var ytopmax,ytopmin,ybotmax,ybotmin:integer):boolean;
var r : PEllipseInfoData;
begin
  R:=Nil;
  result := GetInfoForX (x, r);
  if assigned(r) then
    begin
    ytopmax := ytopmax;
    ytopmin := ytopmin;
    ybotmax := ybotmax;
    ybotmin := ybotmin;
    end;
end;

function TEllipseInfo.FindXIndex (x : integer) : integer;
begin
  result := InfoList.Count;
  repeat
    dec (result);
  until (result < 0) or (x = PEllipseInfoData(InfoList[result])^.x);
end;

function TEllipseInfo.GetInfoForX (x:integer; var Info:PEllipseInfoData):boolean;
var r : integer;
begin
  r := FindXIndex (x);
  result := (r >= 0);
  if result then
    Info := PEllipseInfoData(InfoList[r])
end;

procedure TEllipseInfo.PrepareCalculation (out np:integer; out delta:real);
begin
  np := round(1.5708 * sqrt(sqr(frx)+sqr(fry)) );
  // number of pixel in quarter circel to calculate without gaps in drawing
  delta := pi / (2 * np);
end;

function TEllipseInfo.NewInfoRec (anX:integer) : PEllipseInfoData;
begin
  new (result);
  result^.x := anX;
  infolist.Add (result);
  with result^ do
    begin
    ytopmax := -1;
    ytopmin := maxint;
    ybotmax := -1;
    ybotmin := maxint;
    end;
end;

procedure TEllipseInfo.CalculateCircular (const b:TRect; out x,y,rx,ry:real);
begin
  with b do
    begin
    x := (right+left) / 2;
    y := (top+bottom) / 2;
    rx := abs(right-left) / 2;
    ry := abs(bottom-top) / 2;
    end;
end;

procedure TEllipseInfo.GatherEllipseInfo (const bounds:TRect);
var infoP, infoM : PEllipseInfoData;
    halfnumber,
    r, NumberPixels, xtemp,yt,yb : integer;
    pPy, pMy, x,y, rx,ry, xd,yd,ra, rdelta : real;
begin
  ClearList;
  CalculateCircular (bounds, x,y,rx,ry);
  with bounds do
  fcx := x;
  fcy := y;
  frx := rx;
  fry := ry;
  if (rx < 0.5) and (ry < 0.5) then
    with NewInfoRec (round(x))^ do
      begin
      ytopmax := round(y);
      ytopmin := ytopmax;
      ybotmax := ytopmax;
      ybotmin := ytopmax;
      end
  else
    begin
    PrepareCalculation (NumberPixels, rdelta);
    halfnumber := NumberPixels div 2;
    pPy := maxint;
    pMy := maxint;
    ra := 0;
    infoP := NewInfoRec (round(x + rx));
    infoM := NewInfoRec (round(x - rx));
    for r := 0 to NumberPixels do
      begin
      xd := rx * cos(ra);
      yd := ry * sin(ra);
      // take all 4 quarters
      yt := round(y - yd);
      yb := round(y + yd);
      xtemp := round (x + xd);
      // quarter 1 and 4 at the same x line
      if infoP^.x <> xtemp then                  // has correct record ?
        begin
        with infoP^ do                           // ensure single width
          begin
          if r < halfnumber then
            begin
            if ytopmin = yt then
              begin
              inc (ytopmin);
              dec (ybotmax);
              end;
            end
          else
            begin
            if (ytopmax = pPy) and (ytopmax <> ytopmin) then
              begin
              dec (ytopmax);
              inc (ybotmin);
              end;
            end;
          pPy := ytopmin;
          end;
        if not GetInfoForX (xtemp, infoP) then  // record exists already ?
          infoP := NewInfoRec (xtemp);          // create a new recod
        end;
      // lower y is top, min is lowest
      with InfoP^ do
        begin
        if yt < ytopmin then
          ytopmin := yt;
        if yb < ybotmin then
          ybotmin := yb;
        if yt > ytopmax then
          ytopmax := yt;
        if yb > ybotmax then
          ybotmax := yb;
        end;
      // quarter 2 and 3 on the same x line
      xtemp := round(x - xd);
      if infoM^.x <> xtemp then                  // has correct record ?
        begin
        with infoM^ do             // ensure single width
          begin
          if r < halfnumber then
            begin
            if ytopmin = yt then
              begin
              inc (ytopmin);
              dec (ybotmax);
              end;
            end
          else
            begin
            if (ytopmax = pMy) and (ytopmax <> ytopmin) then
              begin
              dec (ytopmax);
              inc (ybotmin);
              end;
            end;
          pMy := ytopmin;
          end;
        if not GetInfoForX (xtemp, infoM) then  // record exists already ?
          infoM := NewInfoRec (xtemp);          // create a new recod
        end;
      // lower y is top, min is lowest
      with InfoM^ do
        begin
        if yt < ytopmin then
          ytopmin := yt;
        if yb < ybotmin then
          ybotmin := yb;
        if yt > ytopmax then
          ytopmax := yt;
        if yb > ybotmax then
          ybotmax := yb;
        end;
      ra := ra + rdelta;
      end;
    end;
end;


{ The drawing routines }

type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
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

procedure PutPixelCopy(Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
begin
  with Canv do
    DrawPixel(x,y,color);
end;

procedure PutPixelXor(Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] xor color;
end;

procedure PutPixelOr(Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] or color;
end;

procedure PutPixelAnd(Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] and color;
end;

procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    MyPutPix : TPutPixelProc;
begin
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  info := TEllipseInfo.Create;
  with Canv, info do
    try
      GatherEllipseInfo (bounds);
      for r := 0 to InfoList.count-1 do
        with PEllipseInfoData(InfoList[r])^ do
          begin
          for y := ytopmin to ytopmax do
            MyPutPix (Canv, x,y, c);
          for y := ybotmin to ybotmax do
            MyPutPix (Canv, x,y, c);
          end;
    finally
      info.Free;
    end;
end;

procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Width:integer; const c:TFPColor);
var infoOut, infoIn : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    MyPutPix : TPutPixelProc;
begin
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  infoIn := TEllipseInfo.Create;
  infoOut := TEllipseInfo.Create;
  dec (width);
  id:=Nil;
  try
    infoOut.GatherEllipseInfo(bounds);
    with bounds do
      infoIn.GatherEllipseInfo (Rect(left+width,top+width,right-width,bottom-width));
    with Canv do
      for r := 0 to infoOut.infolist.count-1 do
        with PEllipseInfoData (infoOut.infolist[r])^ do
          begin
          if infoIn.GetInfoForX (x, id) then
            begin
            for y := ytopmin to id^.ytopmax do
              MyPutPix (canv, x,y, c);
            for y := id^.ybotmin to ybotmax do
              MyPutPix (canv, x,y, c);
            end
          else
            begin // no inner circle found: draw all points between top and bottom
            for y := ytopmin to ybotmax do
              MyPutPix (canv, x,y, c);
            end;
          end;
    finally
      infoOut.Free;
      infoIn.Free;
    end;
end;

procedure DrawPatternEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TPenPattern; const c:TFPColor);
var info : TEllipseInfo;
    xx, y : integer;
    LinePoints : TLinePoints;
    MyPutPix : TPutPixelProc;
    id : PEllipseInfoData;
    CountDown, CountUp, half : integer;
begin
  id:=Nil;
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  PatternToPoints (pattern, @LinePoints);
  info := TEllipseInfo.Create;
  with Canv, info do
    try
      GatherEllipseInfo (bounds);
      CountUp := 0;
      CountDown := PatternBitCount - 1;
      half := round (cx);
      for xx := bounds.left to half do
        if GetInfoForX (xx, id) then
          begin
          with id^ do
            begin
            for y := ytopmax downto ytopmin do
              begin
              if LinePoints[CountUp mod PatternBitCount] then
                MyPutPix (Canv, xx,y, c);
              inc (CountUp);
              end;
            for y := ybotmin to ybotmax do
              begin
              if LinePoints[PatternBitCount - (CountDown mod PatternBitCount) - 1] then
                MyPutPix (Canv, xx,y, c);
              inc (CountDown);
              end;
            end;
          end;
      for xx := half+1 to bounds.right do
        if GetInfoForX (xx, id) then
          begin
          with id^ do
            begin
            for y := ytopmin to ytopmax do
              begin
              if LinePoints[CountUp mod PatternBitCount] then
                MyPutPix (Canv, xx,y, c);
              inc (CountUp);
              end;
            for y := ybotmax downto ybotmin do
              begin
              if LinePoints[Patternbitcount - (CountDown mod PatternBitCount) - 1] then
                MyPutPix (Canv, xx,y, c);
              inc (CountDown);
              end;
            end;
          end;
    finally
      info.Free;
    end;
end;

procedure FillEllipseColor (Canv:TFPCustomCanvas; const Bounds:TRect; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    with Canv do
      for r := 0 to info.infolist.count-1 do
        with PEllipseInfoData (info.infolist[r])^ do
          for y := ytopmin to ybotmax do
            DrawPixel(x,y,c);
  finally
    info.Free;
  end;
end;

procedure FillEllipsePattern (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TBrushPattern; const c:TFPColor);
begin
end;

procedure FillEllipseHashHorizontal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        for y := ytopmin to ybotmax do
          if (y mod width) = 0 then
            canv.DrawPixel(x,y,c);
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashVertical (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        if (x mod width) = 0 then
          for y := ytopmin to ybotmax do
            canv.DrawPixel(x,y,c);
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := width - 1 - (x mod width);
        for y := ytopmin to ybotmax do
          if (y mod width) = w then
            canv.DrawPixel(x,y,c);
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashBackDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := (x mod width);
        for y := ytopmin to ybotmax do
          if (y mod width) = w then
            canv.DrawPixel(x,y,c);
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashDiagCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    wy,w1,w2 : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w1 := (x mod width);
        w2 := width - 1 - (x mod width);
        for y := ytopmin to ybotmax do
          begin
          wy := y mod width;
          if (wy = w1) or (wy = w2) then
            canv.DrawPixel(x,y,c);
          end;
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        if (x mod width) = 0 then
          for y := ytopmin to ybotmax do
            canv.DrawPixel(x,y,c)
        else
          for y := ytopmin to ybotmax do
            if (y mod width) = 0 then
              canv.DrawPixel(x,y,c);
  finally
    info.Free;
  end;
end;

procedure FillEllipseImage (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
var info : TEllipseInfo;
    r, y : integer;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := (x mod image.width);
        for y := ytopmin to ybotmax do
          canv.DrawPixel(x,y,Image.colors[w, (y mod image.height)]);
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseImageRel (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
var info : TEllipseInfo;
    r, y : integer;
    xo,yo, xi,yi : integer;
begin
  info := TEllipseInfo.Create;
  try
    with info do
      begin
      GatherEllipseInfo(bounds);
      xo := round(cx) - (image.width div 2);
      yo := round(cy) - (image.height div 2);
      end;
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        xi := (x - xo) mod image.width;
        if xi < 0 then
          inc (xi, image.width);
        for y := ytopmin to ybotmax do
          begin
          yi := (y - yo) mod image.height;
          if yi < 0 then
            inc (yi, image.height);
          canv.DrawPixel(x,y,Image.colors[xi, yi]);
          end;
        end;
  finally
    info.Free;
  end;
end;

end.
