{%MainUnit fpcanvas.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCustomCanvas implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomCanvas }

constructor TFPCustomCanvas.Create;
begin
  inherited create;
  FClipping := false;
  FRemovingHelpers := false;
  FHelpers := TList.Create;
  FDefaultFont := CreateDefaultFont;
  FDefaultPen := CreateDefaultPen;
  FDefaultBrush := CreateDefaultBrush;
end;

destructor TFPCustomCanvas.Destroy;
begin
  FreeAndNil(FClipRegion);
  FRemovingHelpers := True;
  // first remove all helper references
  RemoveHelpers;
  // then free helpers
  FDefaultFont.Free;
  FDefaultBrush.Free;
  FDefaultPen.Free;
  FHelpers.Free;
  FRemovingHelpers := False;
  inherited;
end;

procedure TFPCustomCanvas.CheckHelper (AHelper:TFPCanvasHelper);
// remove references to AHelper
begin
  if AHelper = FPen then
    FPen := nil
  else if AHelper = FFont then
    FFont := nil
  else if AHelper = FBrush then
    FBrush := nil;
  if not FRemovingHelpers then
    begin
    if AHelper = FDefaultFont then
      FDefaultFont := CreateDefaultFont
    else if AHelper = FDefaultPen then
      FDefaultPen := CreateDefaultPen
    else if AHelper = FDefaultBrush then
      FDefaultBrush := CreateDefaultBrush;
    end;
  FHelpers.Remove (AHelper);
end;

procedure TFPCustomCanvas.RemoveHelpers;
var r : integer;
begin
  for r := FHelpers.count-1 downto 0 do
    with TFPCanvasHelper(FHelpers[r]) do
      if FCanvas = self then
        if FFixedCanvas then
          DeallocateResources
        else
          FCanvas := nil;
  FHelpers.Clear;
end;

procedure TFPCustomCanvas.AddHelper (AHelper : TFPCanvasHelper);
var r : integer;
begin
  r := FHelpers.IndexOf (AHelper);
  if r < 0 then
    FHelpers.Add (AHelper);
end;

function TFPCustomCanvas.CreateDefaultFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EFont])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultPen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EPen])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EBrush])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.GetClipping: boolean;
begin
  Result:=FClipping;
end;

function TFPCustomCanvas.GetClipRect: TRect;
begin
  if FClipRegion = nil then Result := Bounds(0, 0, 0, 0)
  else Result:=FClipRegion.GetBoundingRect();
end;

function TFPCustomCanvas.CreateFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
end;

function TFPCustomCanvas.CreatePen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
end;

function TFPCustomCanvas.CreateBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
end;

function TFPCustomCanvas.AllowFont (AFont : TFPCustomFont) : boolean;
begin
  if AFont is TFPCustomDrawFont then
    result := true
  else
    result := DoAllowFont (AFont);
end;

procedure TFPCustomCanvas.SetFont (AValue:TFPCustomFont);
begin
  if (AValue <> FFont) and AllowFont(AValue) then
    begin
      if FManageResources then
        FFont.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FFont := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetFont : TFPCustomFont;
begin
  if assigned (FFont) then
    result := FFont
  else
    result := FDefaultFont;
end;

function TFPCustomCanvas.DoAllowFont (AFont : TFPCustomFont) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  if ABrush is TFPCustomDrawBrush then
    result := true
  else
    result := DoAllowBrush (ABrush);
end;

procedure TFPCustomCanvas.SetBrush (AValue:TFPCustomBrush);
begin
  if (AValue <> FBrush) and AllowBrush(AValue) then
    begin
      if FManageResources then
        FBrush.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FBrush := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetBrush : TFPCustomBrush;
begin
  if assigned (FBrush) then
    result := FBrush
  else
    result := FDefaultBrush
end;

function TFPCustomCanvas.DoAllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowPen (APen : TFPCustomPen) : boolean;
begin
  if APen is TFPCustomDrawPen then
    result := true
  else
    result := DoAllowPen (APen);
end;

procedure TFPCustomCanvas.SetPen (AValue:TFPCustomPen);
begin
  if (AValue <> FPen) and AllowPen (AValue) then
    begin
      if FManageResources then
        FPen.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FPen := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetPen : TFPCustomPen;
begin
  if assigned (FPen) then
    result := FPen
  else
    result := FDefaultPen;
end;

procedure TFPCustomCanvas.SetClipping(const AValue: boolean);
begin
  FClipping:=AValue;
end;

procedure TFPCustomCanvas.SetClipRect(const AValue: TRect);
var
  lNewRegion: TFPRectRegion;
begin
  lNewRegion := TFPRectRegion.Create;
  lNewRegion.Rect := AValue;
  if FClipRegion <> nil then FClipRegion.Free;
  FClipRegion := lNewRegion;
end;

procedure TFPCustomCanvas.SetPenPos(const AValue: TPoint);
begin
  FPenPos:=AValue;
end;

function TFPCustomCanvas.DoAllowPen (APen : TFPCustomPen) : boolean;
begin
  result := false;
end;

procedure TFPCustomCanvas.DoLockCanvas;
begin
end;

procedure TFPCustomCanvas.DoUnlockCanvas;
begin
end;

procedure TFPCustomCanvas.LockCanvas;
begin
  if FLocks = 0 then
    DoLockCanvas;
  inc (FLocks);
end;

procedure TFPCustomCanvas.UnlockCanvas;
begin
  if FLocks > 0 then
    begin
    dec (FLocks);
    if FLocks = 0 then
      DoUnlockCanvas;
    end
  else
    raise TFPCanvasException.Create (ErrNoLock);
end;

function TFPCustomCanvas.Locked: boolean;
begin
  Result:=FLocks>0;
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:string);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:string; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:string) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:string) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: string): TSize;
begin
  Result:=Default(TSize);
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: string): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: string): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:unicodestring);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:unicodestring; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:unicodestring) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:unicodestring) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: unicodestring): TSize;
begin
  Result:=Default(TSize);
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: unicodestring): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: unicodestring): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.DoTextOut (x,y:integer;text:unicodestring); 

begin
  DoTextOut(x,y,string(text));
end;

procedure TFPCustomCanvas.DoGetTextSize (text:unicodestring; var w,h:integer); 

begin
  DoGetTextSize(String(Text),w,h);
end;

function  TFPCustomCanvas.DoGetTextHeight (text:unicodestring) : integer; 

begin
  Result:=DoGetTextHeight(String(text));
end;

function  TFPCustomCanvas.DoGetTextWidth (text:unicodestring) : integer; 

begin
  Result:=DoGetTextWidth(String(text));
end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg,
  Angle16DegLength: Integer);
begin

end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
begin

end;

procedure TFPCustomCanvas.DoMoveTo (x,y:integer);
begin
end;

procedure TFPCustomCanvas.DoLineTo (x,y:integer);
begin
  DoLine (FPenPos.X,FPenPos.y, x,y);
end;

procedure TFPCustomCanvas.MoveTo (x,y:integer);
begin
  FPenPos.x := x;
  FPenPos.y := y;
  DoMoveTo (x,y);
end;

procedure TFPCustomCanvas.MoveTo (p:TPoint);
begin
  FPenPos := p;
  DoMoveTo (p.x,p.y);
end;

procedure TFPCustomCanvas.LineTo (x,y:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (FPenPos.x, FPenPos.y, x, y)
    else
      DoLineTo (x,y);
  FPenPos.x := x;
  FPenPos.y := y;
end;

procedure TFPCustomCanvas.LineTo (p:TPoint);
begin
  LineTo (p.x, p.y);
end;

procedure TFPCustomCanvas.Line (x1,y1,x2,y2:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (x1,y1, x2,y2)
    else
      DoLine (x1,y1, x2,y2);
  FPenPos.x := x2;
  FPenPos.y := y2;
end;

procedure TFPCustomCanvas.Line (const p1,p2:TPoint);
begin
  Line (p1.x,p1.y,p2.x,p2.y);
end;

procedure TFPCustomCanvas.Line (const points:TRect);
begin
  with points do
    Line (left,top, right,bottom);
end;

procedure TFPCustomCanvas.Polyline (Const points:array of TPoint);
begin
  if Pen.Style <> psClear then
   if Pen is TFPCustomDrawPen then
     TFPCustomDrawPen(Pen).Polyline (points,false)
   else
     DoPolyline (points);
  FPenPos := points[high(points)];
end;

procedure TFPCustomCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

begin
  DoRadialPie(X1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
end;

procedure TFPCustomCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

begin
  // To be implemented
end;

procedure TFPCustomCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False);

begin
 // To be implemented
end;

procedure TFPCustomCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
                     Filled: boolean = False;
                     Continuous: boolean = False);
begin
  DoPolyBezier(Points,NumPts,Filled,Continuous);
end;
                     
procedure TFPCustomCanvas.PolyBezier(const Points: array of TPoint;  
                     Filled: boolean = False;
                     Continuous: boolean = False);
var 
  NPoints{, i}: integer;
//  PointArray: ^TPoint;
begin
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints>0 then
    DoPolyBezier(@Points[Low(Points)],NPoints,Filled,Continuous);
{
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints<=0 then exit;
    GetMem(PointArray,SizeOf(TPoint)*NPoints);
  try  
    for i:=0 to NPoints-1 do
      PointArray[i]:=Points[i+Low(Points)];
    DoPolyBezier(PointArray, NPoints, Filled, Continuous);
  finally
    FreeMem(PointArray);
  end;}
end;

procedure TFPCustomCanvas.Clear;
var r : TRect;
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush(Brush).Rectangle(0,0, width, height)
    else
      begin
      r := Rect(0,0, width, height);
      DoRectangleFill (r);
      end;
    end;
end;

procedure TFPCustomCanvas.DrawPixel(const x, y: integer;
  const newcolor: TFPColor);
begin
  case FDrawingMode of
    dmOpaque: Colors[x,y] := newcolor;
    dmAlphaBlend: Colors[x,y] := AlphaBlend(Colors[x,y], newcolor);
    dmCustom: Colors[x,y] := FOnCombineColors(Colors[x,y], newcolor);
  end;
end;

procedure TFPCustomCanvas.Erase;
var
  x,y:Integer;
begin
  for x:=0 to Width-1 do
    for y:=0 to Height-1 do
      Colors[x,y]:=colTransparent;
end;

procedure TFPCustomCanvas.DoRectangleAndFill (const Bounds:TRect);
begin
  DoRectangleFill (Bounds);
  DoRectangle (Bounds);
end;

procedure TFPCustomCanvas.DoEllipseAndFill (const Bounds:TRect);
begin
  DoEllipseFill (Bounds);
  DoEllipse (Bounds);
end;

procedure TFPCustomCanvas.DoPolygonAndFill (const points:array of TPoint);
begin
  DoPolygonFill (points);
  DoPolygon (points);
end;

procedure TFPCustomCanvas.Ellipse (const Bounds:TRect);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  pb := false;
  dp:=False;
  db:=False;
  if p and (Pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (Brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoEllipseAndFill (bounds)
  else
    begin
    if p then
      DoEllipse (bounds)
    else if dp then
      with bounds do
        TFPCustomDrawPen(Pen).Ellipse (left,top,right,bottom);
    if b then
      DoEllipseFill (bounds)
    else if db then 
      with bounds do
        TFPCustomDrawBrush(Brush).Ellipse (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.Ellipse (left,top,right,bottom:integer);
begin
  Ellipse (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.EllipseC (x,y:integer; rx,ry:longword);
begin
  Ellipse (Rect(x-rx,y-ry,x+rx,y+ry));
end;

procedure TFPCustomCanvas.Rectangle (left,top,right,bottom:integer);
begin
  Rectangle (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.FillRect(const ARect: TRect);

begin
  if (Brush.style <> bsClear) then
    begin
    if not (brush is TFPCustomDrawBrush) then
      DoRectangleFill (ARect)
    else 
      with ARect do
        TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.FillRect(X1,Y1,X2,Y2: Integer);

begin
  FillRect (Rect(X1,Y1,X2,Y2));
end;
        
procedure TFPCustomCanvas.Rectangle (const Bounds:TRect);
var np,nb,dp,db: boolean;
begin
  np:= Pen.style <> psClear; // Need pen ?
  nb:= Brush.style <> bsClear;  // Need brush ?
  dp:=(pen is TFPCustomDrawPen); // Pen draws ?
  db:=(brush is TFPCustomDrawBrush); // Brush draws ?
  if (np and nb) and not (db or dp) then
    DoRectangleAndFill (bounds)
  else
    begin
    if np then
      begin
      If not dp then
        DoRectangle (bounds)
      else
        with bounds do
          TFPCustomDrawPen(Pen).Rectangle (left,top,right,bottom);
      end;
    if Nb then
      begin
      if not db then
        DoRectangleFill (bounds)
      else 
        with bounds do
          TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
      end;
    end;
end;

procedure TFPCustomCanvas.FloodFill (x,y:integer);
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush (Brush).FloodFill (x,y)
    else
      DoFloodFill (x,y);
    end;
end;

procedure TFPCustomCanvas.Polygon (const points:array of TPoint);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  dp:=false;
  db:=false;
  pb:=False;
  if p and (pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoPolygonAndFill (points)
  else
    begin
    if p then
      DoPolygon (points)
    else if dp then
      TFPCustomDrawPen(Pen).Polyline (points, true);
    if b then
      DoPolygonFill (points)
    else if db then
      TFPCustomDrawBrush(Brush).Polygon (points);
    end;
end;

procedure TFPCustomCanvas.CopyRect (x,y:integer; canvas:TFPCustomCanvas;
  SourceRect:TRect);
var yy,r,t : integer;
begin
  SortRect (SourceRect);
  with SourceRect do begin
    for t := top to bottom do begin
      yy := t - top + y;
      for r := left to right do
        colors[r - left + x,yy] := canvas.colors[r,t];
    end;
  end;
end;

procedure TFPCustomCanvas.Draw (x,y:integer; image:TFPCustomImage);
var xx,xi,yi,xm,ym,r,t : integer;
begin
  xm := x + image.width-1;
  if (xm<0) or (x>width) then exit;
  if xm >= width then
    xm := width - 1;
  ym := y + image.height-1;
  if (ym<0) or (y>height) then exit;
  if ym >= height then
    ym := height - 1;
  xi := x;
  yi := y;
  if clipping then
    if not CheckRectClipping (ClipRect, xi,yi, xm,ym) then
      exit;
  for r := xi to xm do
    begin
    xx := r - x;
    for t := yi to ym do
      DrawPixel(r,t, image.colors[xx,t-y]);
    end;
end;

procedure TFPCustomCanvas.StretchDraw(x, y, w, h: integer; source: TFPCustomImage);
var
    FreeInterpolation : boolean;
    IP : TFPCustomInterpolation;
begin
  FreeInterpolation := not assigned (FInterpolation);
  if FreeInterpolation then
    IP := TMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    with IP do
      begin
      Initialize (source, self);
      Execute (x,y,w,h);
      end;
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;


