{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TPixelCanvas class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPPixlCanv;

interface

uses Sysutils, classes, FPImage, FPCanvas, PixTools, ellipses;

type

  { need still to be implemented in descendants :
    GetColor / SetColor
    Get/Set Width/Height
  }

  PixelCanvasException = class (TFPCanvasException);

  { TFPPixelCanvas }

  TFPPixelCanvas = class (TFPCustomCanvas)
  private
    FHashWidth : word;
    FRelativeBI : boolean;
  protected
    procedure DoCopyRect(x, y: integer; canvas: TFPCustomCanvas; const SourceRect: TRect); override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoDraw(x, y: integer; const image: TFPCustomImage); override;
    procedure DoTextOut (x,y:integer;text:string); override;
    procedure DoGetTextSize (text:string; var w,h:integer); override;
    function  DoGetTextHeight (text:string) : integer; override;
    function  DoGetTextWidth (text:string) : integer; override;
    procedure DoRectangle (const Bounds:TRect); override;
    procedure DoRectangleFill (const Bounds:TRect); override;
    procedure DoEllipseFill (const Bounds:TRect); override;
    procedure DoEllipse (const Bounds:TRect); override;
    procedure DoPolygonFill (const points:array of TPoint); override;
    procedure DoPolygon (const points:array of TPoint); override;
    procedure DoPolyline (const points:array of TPoint); override;
    procedure DoFloodFill (x,y:integer); override;
    procedure DoLine (x1,y1,x2,y2:integer); override;
  public
    constructor create;
    property HashWidth : word read FHashWidth write FHashWidth;
    property RelativeBrushImage : boolean read FRelativeBI write FRelativeBI;
  end;

const
  PenPatterns : array[psDash..psDashDotDot] of TPenPattern =
    ($EEEEEEEE, $AAAAAAAA, $E4E4E4E4, $EAEAEAEA);
  sErrNoImage:string = 'No brush image specified';
  sErrNotAvailable:string = 'Not available';

implementation

uses Clipping;

const
  DefaultHashWidth = 15;

procedure NotImplemented;
begin
  raise ENotImplemented.Create(sErrNotAvailable);
end;

constructor TFPPixelCanvas.create;
begin
  inherited;
  FHashWidth := DefaultHashWidth;
end;

procedure TFPPixelCanvas.DoCopyRect(x, y: integer; canvas: TFPCustomCanvas; const SourceRect: TRect);
Var
  W,H,XS1,XS2,YS1,YS2 : Integer;

begin
  XS1:=SourceRect.Left;
  XS2:=SourceRect.Right;
  YS1:=SourceRect.Top;
  YS2:=SourceRect.Bottom;
  For H:=0 to YS2-YS1 do
    For W:=0 to XS2-XS1 do
      Colors[x+h,y+h]:=Canvas.Colors[XS1+W,YS1+H];
end;

function TFPPixelCanvas.DoCreateDefaultFont : TFPCustomFont;
begin
  result := TFPEmptyFont.Create;
  with result do
    begin
    Size := 10;
    FPColor := colBlack;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultPen : TFPCustomPen;
begin
  result := TFPEmptyPen.Create;
  with result do
    begin
    FPColor := colBlack;
    width := 1;
    pattern := 0;
    Style := psSolid;
    Mode := pmCopy;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultBrush : TFPCustomBrush;
begin
  result := TFPEmptyBrush.Create;
  result.Style := bsSolid;
end;

procedure TFPPixelCanvas.DoDraw(x, y: integer; const image: TFPCustomImage);

Var
  W,h : Integer;

begin
  For H:=0 to Image.Height-1 do
    For W:=0 to Image.Width-1 do
      Colors[x+w,y+h]:=Image.Colors[W,H];
end;

procedure TFPPixelCanvas.DoTextOut (x,y:integer;text:string);
begin
  NotImplemented;
end;

procedure TFPPixelCanvas.DoGetTextSize (text:string; var w,h:integer);
begin
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextHeight (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextWidth (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

procedure TFPPixelCanvas.DoRectangle (const Bounds:TRect);
var pattern : longword;

  procedure CheckLine (x1,y1, x2,y2 : integer);
  begin
    if clipping then
      CheckLineClipping (ClipRect, x1,y1, x2,y2);
    if x1 >= 0 then
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor)
  end;

  procedure CheckPLine (x1,y1, x2,y2 : integer);
  begin
    if clipping then
      CheckLineClipping (ClipRect, x1,y1, x2,y2);
    if x1 >= 0 then
      DrawPatternLine (self, x1,y1, x2,y2, pattern, Pen.FPColor)
  end;

var b : TRect;
    r : integer;

begin
  b := bounds;
  if pen.style = psSolid then
    for r := 1 to pen.width do
      begin
      with b do
        begin
        CheckLine (left,top,left,bottom);
        CheckLine (left,bottom,right,bottom);
        CheckLine (right,bottom,right,top);
        CheckLine (right,top,left,top);
        end;
      DecRect (b);
      end
  else if pen.style <> psClear then
    begin
    if pen.style = psPattern then
      pattern := Pen.pattern
    else
      pattern := PenPatterns[pen.style];
    with b do
      begin
      CheckPLine (left,top,left,bottom);
      CheckPLine (left,bottom,right,bottom);
      CheckPLine (right,bottom,right,top);
      CheckPLine (right,top,left,top);
      end;
    end;
end;

procedure TFPPixelCanvas.DoRectangleFill (const Bounds:TRect);
var b : TRect;
begin
  b := Bounds;
  SortRect (b);
  if clipping then
    CheckRectClipping (ClipRect, B);
  with b do
    case Brush.style of
      bsSolid : FillRectangleColor (self, left,top, right,bottom);
      bsPattern : FillRectanglePattern (self, left,top, right,bottom, brush.pattern);
      bsImage :
        if assigned (brush.image) then
          if FRelativeBI then
            FillRectangleImageRel (self, left,top, right,bottom, brush.image)
          else
            FillRectangleImage (self, left,top, right,bottom, brush.image)
        else
          raise PixelCanvasException.Create (sErrNoImage);
      bsBDiagonal : FillRectangleHashDiagonal (self, b, FHashWidth);
      bsFDiagonal : FillRectangleHashBackDiagonal (self, b, FHashWidth);
      bsCross :
        begin
        FillRectangleHashHorizontal (self, b, FHashWidth);
        FillRectangleHashVertical (self, b, FHashWidth);
        end;
      bsDiagCross :
        begin
        FillRectangleHashDiagonal (self, b, FHashWidth);
        FillRectangleHashBackDiagonal (self, b, FHashWidth);
        end;
      bsHorizontal : FillRectangleHashHorizontal (self, b, FHashWidth);
      bsVertical : FillRectangleHashVertical (self, b, FHashWidth);
    end;
end;

procedure TFPPixelCanvas.DoEllipseFill (const Bounds:TRect);
begin
  case Brush.style of
    bsSolid : FillEllipseColor (self, Bounds, Brush.FPColor);
    bsPattern : FillEllipsePattern (self, Bounds, brush.pattern, Brush.FPColor);
    bsImage :
      if assigned (brush.image) then
        if FRelativeBI then
          FillEllipseImageRel (self, Bounds, brush.image)
        else
          FillEllipseImage (self, Bounds, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillEllipseHashDiagonal (self, Bounds, FHashWidth, Brush.FPColor);
    bsFDiagonal : FillEllipseHashBackDiagonal (self, Bounds, FHashWidth, Brush.FPColor);
    bsCross : FillEllipseHashCross (self, Bounds, FHashWidth, Brush.FPColor);
    bsDiagCross : FillEllipseHashDiagCross (self, Bounds, FHashWidth, Brush.FPColor);
    bsHorizontal : FillEllipseHashHorizontal (self, Bounds, FHashWidth, Brush.FPColor);
    bsVertical : FillEllipseHashVertical (self, Bounds, FHashWidth, Brush.FPColor);
  end;
end;

procedure TFPPixelCanvas.DoEllipse (const Bounds:TRect);
begin
  with pen do
    case style of
      psSolid :
        if pen.width > 1 then
          DrawSolidEllipse (self, Bounds, width, FPColor)
        else
          DrawSolidEllipse (self, Bounds, FPColor);
      psPattern:
        DrawPatternEllipse (self, Bounds, pattern, FPColor);
      psDash, psDot, psDashDot, psDashDotDot :
        DrawPatternEllipse (self, Bounds, PenPatterns[Style], FPColor);
    end;
end;

procedure TFPPixelCanvas.DoPolygonFill (const points:array of TPoint);
begin  //TODO: how to find a point inside the polygon ?
end;

procedure TFPPixelCanvas.DoFloodFill (x,y:integer);
begin
  case Brush.style of
    bsSolid : FillFloodColor (self, x,y);
    bsPattern : FillFloodPattern (self, x,y, brush.pattern);
    bsImage :
      if assigned (brush.image) then
        if FRelativeBI then
          FillFloodImageRel (self, x,y, brush.image)
        else
          FillFloodImage (self, x,y, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillFloodHashDiagonal (self, x,y, FHashWidth);
    bsFDiagonal : FillFloodHashBackDiagonal (self, x,y, FHashWidth);
    bsCross : FillFloodHashCross (self, x,y, FHashWidth);
    bsDiagCross : FillFloodHashDiagCross (self, x,y, FHashWidth);
    bsHorizontal : FillFloodHashHorizontal (self, x,y, FHashWidth);
    bsVertical : FillFloodHashVertical (self, x,y, FHashWidth);
  end;
end;

procedure TFPPixelCanvas.DoPolygon (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
  DoLine (p.x,p.y, points[i].x,points[i].y);
end;

procedure TFPPixelCanvas.DoPolyline (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
end;

procedure TFPPixelCanvas.DoLine (x1,y1,x2,y2:integer);

  procedure DrawOneLine (xx1,yy1, xx2,yy2:integer);
  begin
    if Clipping then
      CheckLineClipping (ClipRect, xx1,yy1, xx2,yy2);
    DrawSolidLine (self, xx1,yy1, xx2,yy2, Pen.FPColor);
  end;

  procedure SolidThickLine;
  var w1, w2, r : integer;
      MoreHor : boolean;
  begin
    // determine lines above and under
    w1 := pen.width div 2;
    w2 := w1;
    if w1+w2 = pen.width then
      dec (w1);
    // determine slanting
    MoreHor := (abs(x2-x1) < abs(y2-y1));
    if MoreHor then
      begin  // add lines left/right
      for r := 1 to w1 do
        DrawOneLine (x1-r,y1, x2-r,y2);
      for r := 1 to w2 do
        DrawOneLine (x1+r,y1, x2+r,y2);
      end
    else
      begin  // add lines above/under
      for r := 1 to w1 do
        DrawOneLine (x1,y1-r, x2,y2-r);
      for r := 1 to w2 do
        DrawOneLine (x1,y1+r, x2,y2+r);
      end;
  end;

begin
  if Clipping then
    CheckLineClipping (ClipRect, x1,y1, x2,y2);
  case Pen.style of
    psSolid :
      begin
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor);
      if pen.width > 1 then
        SolidThickLine;
      end;
    psPattern:
      DrawPatternLine (self, x1,y1, x2,y2, pen.pattern);
      // Patterned lines have width always at 1
    psDash, psDot, psDashDot, psDashDotDot :
      DrawPatternLine (self, x1,y1, x2,y2, PenPatterns[Pen.Style]);
  end;
end;


end.
