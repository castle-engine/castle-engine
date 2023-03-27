{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    fpImage Gaussian blur routines by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

}

unit FPImgGauss;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, FPimage;

{ Fast Gaussian blur to Area (excluding Area.Right and Area.Bottom)
  Pixels outside the image are treated as having the same color as the edge.
  This is a binominal approximation of fourth degree, so it is pretty near the
  real gaussian blur in most cases but much faster for big radius.
  Runtime: O((Area.Width+Radius) * (Area.Height+Radius))  }
procedure GaussianBlurBinominal4(AImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect);
procedure GaussianBlurBinominal4(SrcImg, DestImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect; DestXY: TPoint);

{ Gaussian blur to Area (excluding Area.Right and Area.Bottom)
  Pixels outside the image are treated as having the same color as the edge.
  Runtime: O(Area.Width * Area.Height * Radius)  }
procedure GaussianBlur(Img: TFPCustomImage; Radius: integer; Area: TRect);

{ MatrixBlur1D
  The Matrix1D has a width of Radius*2+1.
  The sum of all entries in the Matrix1D must be <= 65536.
  Create Matrix1D with ComputeGaussianBlurMatrix1D.
  Each pixel x,y in Area is replaced by a pixel computed from all pixels in
  x-Radius..x+Radius, y-Radius..y+Radius
  The new value is the sum of all pixels multiplied by Matrix1D, once
  horizontally and once vertically.

  Pixels outside the image are treated as having the same color as the edge.

  Runtime is O(Area width * Area height * Radius) }
procedure MatrixBlur1D(Img: TFPCustomImage; Radius: integer; Area: TRect; Matrix1D: PWord);

{ MatrixBlur2D
  The Matrix2D is quadratic and has a width of Radius*2+1.
  The sum of all entries in the Matrix2D must be <= 65536.
  Create Matrix2D with ComputeGaussianBlurMatrix2D.
  Each pixel x,y in Area (Left..Right-1,Top..Bottom-1) is replaced by a pixel
  computed from all pixels in x-Radius..x+Radius, y-Radius..y+Radius.
  The new value is the sum of all pixels multiplied by Matrix2D.

  Pixels outside the image are treated as having the same color as the edge.

  Runtime is O(Area width * Area height * Radius * Radius) }
procedure MatrixBlur2D(Img: TFPCustomImage; Radius: integer; Area: TRect; Matrix2D: PWord);

{ ComputeGaussianBlurMatrix1D creates a one dimensional matrix of size
  Width = Radius*2+1

  Deviation := Radius / 3
  G(x) := (1 / SQRT( 2 * pi * Deviation^2)) * e^( - (x^2) / (2 * Deviation^2) )

  Each word is a factor [0,1) multiplied by 65536.
  The total sum of the matrix is 65536. }
function ComputeGaussianBlurMatrix1D(Radius: integer): PWord;

{ ComputeGaussianBlurMatrix2D creates a two dimensional matrix of quadratic size
  Width = Radius*2+1

  Deviation := Radius / 3
  G(x,y) := (1 / (2 * pi * Deviation^2)) * e^( - (x^2 + y^2) / (2 * Deviation^2) )

  Each word is a factor [0,1) multiplied by 65536.
  The total sum of the matrix is 65536. }
function ComputeGaussianBlurMatrix2D(Radius: integer): PWord;

implementation

type

  { TIntRingBuffer }

  TIntRingBuffer = object
  private
    FSize: integer;
    procedure SetSize(AValue: integer);
  public
    RingBuffer: PFPColor;
    procedure Put(Index: integer; const Col: TFPColor);
    procedure Get(Index: integer; out Col: TFPColor);
    property Size: integer read FSize write SetSize;
    procedure Init(len: integer);
    procedure Clear;
  end;

{ TIntRingBuffer }

procedure TIntRingBuffer.SetSize(AValue: integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
  ReAllocMem(RingBuffer,AValue*SizeOf(TFPColor));
end;

procedure TIntRingBuffer.Put(Index: integer; const Col: TFPColor);
begin
  Index:=Index mod FSize;
  if Index<0 then inc(Index,FSize);
  RingBuffer[Index]:=Col;
end;

procedure TIntRingBuffer.Get(Index: integer; out Col: TFPColor);
begin
  Index:=Index mod FSize;
  if Index<0 then inc(Index,FSize);
  Col:=RingBuffer[Index];
end;

procedure TIntRingBuffer.Init(len: integer);
begin
  FSize:=0;
  RingBuffer:=nil;
  Size:=len;
end;

procedure TIntRingBuffer.Clear;
begin
  Size:=0;
end;

procedure GaussianBlurBinominal4(AImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect);
begin
  GaussianBlurBinominal4(AImg,AImg,Radius,SrcArea,SrcArea.TopLeft);
end;

procedure GaussianBlurBinominal4(SrcImg, DestImg: TFPCustomImage;
  Radius: integer; SrcArea: TRect; DestXY: TPoint);
type
  TIntegerColor = record
    red, green, blue, alpha: integer;
  end;
const
  clearIntegerColor: TIntegerColor = (red:0;green:0;blue:0;alpha:0);
var
  x,y,i: LongInt;
  Pixel: TFPColor;
  difference: TIntegerColor;
  derivative1: TIntegerColor;
  derivative2: TIntegerColor;
  sum: TIntegerColor;
  Weight: Single;
  col: TFPColor;
  buffer: TIntRingBuffer;
  Col1, Col2, Col3, Col4: TFPColor;
begin
  // clip
  if SrcArea.Left<0 then begin
    dec(DestXY.X,SrcArea.Left);
    SrcArea.Left:=0;
  end;
  if SrcArea.Top<0 then begin
    dec(DestXY.Y,SrcArea.Top);
    SrcArea.Top:=0;
  end;
  SrcArea.Right:=Min(SrcImg.Width,SrcArea.Right);
  SrcArea.Top:=Min(SrcImg.Height,SrcArea.Top);
  SrcArea.Right:=Min(SrcArea.Right,DestImg.Width-DestXY.X+SrcArea.Left);
  SrcArea.Bottom:=Min(SrcArea.Bottom,DestImg.Height-DestXY.Y+SrcArea.Top);
  if SrcArea.Left>=SrcArea.Right then exit;
  if SrcArea.Top>=SrcArea.Bottom then exit;

  // blur  -- RingBuffer of Size 147 is needed. range=(0,(int)(N_CELLS/4/1.73))
  //                     N_CELLS=1024 don't ask! see paper: gauss.pdf, 3.sourcecdoe
  //                     Or 4*Radius+1, sounds better. see Comment underneath
  //Radius:=round(sqrt(3*Radius*Radius));
  buffer.Init(4*Radius);
  Weight := 1.0/(single(Radius*Radius*Radius*Radius));
  // vertical blur
  for x:=SrcArea.Left to SrcArea.Right-1 do begin
    // set up init values for the first blur
    difference:=clearIntegerColor;
    derivative1:=clearIntegerColor;
    derivative2:=clearIntegerColor;
    sum:=clearIntegerColor;
    for y:=SrcArea.Top-4*Radius to SrcArea.Bottom-1 do begin
      if y >= SrcArea.Top then begin //{+1,-4,+6,-4,+1}
        buffer.Get(y-2*Radius,Col1);
        buffer.Get(y-Radius,Col2);
        buffer.Get(y,Col3);
        buffer.Get(y+Radius,Col4);
        difference.alpha :=difference.alpha+Col1.alpha-4*(Col2.alpha+Col4.alpha)+6*Col3.alpha;
        difference.red   :=difference.red  +Col1.red  -4*(Col2.red  +Col4.red)  +6*Col3.red;
        difference.green :=difference.green+Col1.green-4*(Col2.green+Col4.green)+6*Col3.green;
        difference.blue  :=difference.blue +Col1.blue -4*(Col2.blue +Col4.blue) +6*Col3.blue;
        col:=SrcImg.Colors[x,y];
        col.alpha:=min($FFFF,max(0,round(sum.alpha*Weight)));
        col.red  :=min($FFFF,max(0,round(sum.red  *Weight)));
        col.green:=min($FFFF,max(0,round(sum.green*Weight)));
        col.blue :=min($FFFF,max(0,round(sum.blue *Weight)));
        DestImg.Colors[x,y]:=col; // set blurred pixel
      end else begin
        if (y+3*Radius) >= SrcArea.Top then begin
          // -4*buffer(y+Radius)
          buffer.Get(y+Radius,Col4);
          difference.alpha:=difference.alpha-4*Col4.alpha;
          difference.red  :=difference.red  -4*Col4.red;
          difference.green:=difference.green-4*Col4.green;
          difference.blue :=difference.blue -4*Col4.blue;
        end;
        if (y+2*Radius) >= SrcArea.Top then begin
          // +6*buffer(y)
          buffer.Get(y,Col3);
          difference.alpha:=difference.alpha+6*Col4.alpha;
          difference.red  :=difference.red  +6*Col4.red;
          difference.green:=difference.green+6*Col4.green;
          difference.blue :=difference.blue +6*Col4.blue;
        end;
        if (y+  Radius) >= SrcArea.Top then begin
          // -4*buffer(y-Radius)
          buffer.Get(y-Radius,Col2);
          difference.alpha:=difference.alpha-4*Col2.alpha;
          difference.red  :=difference.red  -4*Col2.red;
          difference.green:=difference.green-4*Col2.green;
          difference.blue :=difference.blue -4*Col2.blue;
        end;
      end;
      i:=Min(DestImg.Height-1,Max(0,y+2*Radius-1));
      // accumulate pixel blur
      pixel := SrcImg.Colors[x,i];
      difference.alpha := difference.alpha+pixel.alpha;
      difference.red   := difference.red  +pixel.red;
      difference.green := difference.green+pixel.green;
      difference.blue  := difference.blue +pixel.blue;
      derivative2.alpha := derivative2.alpha+difference.alpha;
      derivative2.red   := derivative2.red  +difference.red;
      derivative2.green := derivative2.green+difference.green;
      derivative2.blue  := derivative2.blue +difference.blue;
      derivative1.alpha := derivative1.alpha+derivative2.alpha;
      derivative1.red   := derivative1.red  +derivative2.red;
      derivative1.green := derivative1.green+derivative2.green;
      derivative1.blue  := derivative1.blue +derivative2.blue;
      sum.alpha := sum.alpha+derivative1.alpha;
      sum.red   := sum.red  +derivative1.red;
      sum.green := sum.green+derivative1.green;
      sum.blue  := sum.blue +derivative1.blue;
      buffer.Put(y+2*Radius,pixel);  // buffer pixel, min buffer size: 4*Radius
    end;
  end;

  //horizontal blur
  for y:=SrcArea.Top to SrcArea.Bottom-1 do begin
    // set up init values for the first blur
    difference:=clearIntegerColor;
    derivative1:=clearIntegerColor;
    derivative2:=clearIntegerColor;
    sum:=clearIntegerColor;
    for x:=SrcArea.Left-4*Radius to SrcArea.Right-1 do begin
      if x >= SrcArea.Left then begin //{+1,-4,+6,-4,+1}
        buffer.Get(x-2*Radius,Col1);
        buffer.Get(x-Radius,Col2);
        buffer.Get(x,Col3);
        buffer.Get(x+Radius,Col4);
        difference.alpha :=difference.alpha+Col1.alpha-4*(Col2.alpha+Col4.alpha)+6*Col3.alpha;
        difference.red   :=difference.red  +Col1.red  -4*(Col2.red  +Col4.red)  +6*Col3.red;
        difference.green :=difference.green+Col1.green-4*(Col2.green+Col4.green)+6*Col3.green;
        difference.blue  :=difference.blue +Col1.blue -4*(Col2.blue +Col4.blue) +6*Col3.blue;
        col:=DestImg.Colors[x,y];
        col.alpha:=min($FFFF,max(0,round(sum.alpha*Weight)));
        col.red  :=min($FFFF,max(0,round(sum.red  *Weight)));
        col.green:=min($FFFF,max(0,round(sum.green*Weight)));
        col.blue :=min($FFFF,max(0,round(sum.blue *Weight)));
        DestImg.Colors[x,y]:=col; // set blurred pixel
      end else begin
        if (x+3*Radius) >= SrcArea.Left then begin
          // -4*buffer(x+Radius)
          buffer.Get(x+Radius,Col4);
          difference.alpha:=difference.alpha-4*Col4.alpha;
          difference.red  :=difference.red  -4*Col4.red;
          difference.green:=difference.green-4*Col4.green;
          difference.blue :=difference.blue -4*Col4.blue;
        end;
        if (x+2*Radius) >= SrcArea.Left then begin
          // +6*buffer(x)
          buffer.Get(x,Col3);
          difference.alpha:=difference.alpha+6*Col3.alpha;
          difference.red  :=difference.red  +6*Col3.red;
          difference.green:=difference.green+6*Col3.green;
          difference.blue :=difference.blue +6*Col3.blue;
        end;
        if (x+  Radius) >= SrcArea.Left then begin
          // -4*buffer(x-Radius)
          buffer.Get(x-Radius,Col2);
          difference.alpha:=difference.alpha-4*Col2.alpha;
          difference.red  :=difference.red  -4*Col2.red;
          difference.green:=difference.green-4*Col2.green;
          difference.blue :=difference.blue -4*Col2.blue;
        end;
      end;
      i:=Min(DestImg.Width-1,Max(0,x+2*Radius-1));
      // accumulate pixel blur
      pixel := DestImg.Colors[i,y];
      difference.alpha := difference.alpha+pixel.alpha;
      difference.red   := difference.red  +pixel.red;
      difference.green := difference.green+pixel.green;
      difference.blue  := difference.blue +pixel.blue;
      derivative2.alpha := derivative2.alpha+difference.alpha;
      derivative2.red   := derivative2.red  +difference.red;
      derivative2.green := derivative2.green+difference.green;
      derivative2.blue  := derivative2.blue +difference.blue;
      derivative1.alpha := derivative1.alpha+derivative2.alpha;
      derivative1.red   := derivative1.red  +derivative2.red;
      derivative1.green := derivative1.green+derivative2.green;
      derivative1.blue  := derivative1.blue +derivative2.blue;
      sum.alpha := sum.alpha+derivative1.alpha;
      sum.red   := sum.red  +derivative1.red;
      sum.green := sum.green+derivative1.green;
      sum.blue  := sum.blue +derivative1.blue;

      buffer.Put(x+2*Radius,pixel);  // buffer pixel, min buffer size: 4*Radius
    end;
  end;
  buffer.Clear;
end;

procedure GaussianBlur(Img: TFPCustomImage; Radius: integer; Area: TRect);
var
  Matrix: PWord;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  // compute gaussian matrix
  Matrix:=ComputeGaussianBlurMatrix1D(Radius);
  try
    MatrixBlur1D(Img,Radius,Area,Matrix);
  finally
    FreeMem(Matrix);
  end;
end;

procedure MatrixBlur1D(Img: TFPCustomImage; Radius: integer; Area: TRect;
  Matrix1D: PWord);
{ Implementation:
    It runs line by line from Area.Left to Area.Bottom-1.
    It allocates some temporary memory to store the original pixel values
    above the current line.
}
var
  y: Integer;
  x: Integer;
  OrigWidth: Integer;
  OrigHeight: LongInt;
  OrigPixels: PFPColor;
  VertSums: PFPColor;
  NewRed, NewGreen, NewBlue, NewAlpha: cardinal;
  yd: LongInt;
  xd: LongInt;
  xr: Integer;
  yr: Integer;
  Col: TFPColor;
  NewCol: TFPColor;
  Multiplier: Word;
  StartX: Integer;
  EndX: Integer;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  //for x:=0 to MatrixWidth-1 do WriteLn('GaussianBlurNew ',x,' ',Matrix[x]);
  OrigPixels:=nil;
  VertSums:=nil;
  try
    // allocate space for original pixels
    OrigWidth:=Area.Right-Area.Left;
    OrigHeight:=Radius+1;
    //writeln('GaussianBlur ',OrigWidth,'*',OrigHeight,'*',SizeOf(TFPColor));
    GetMem(OrigPixels,OrigWidth*OrigHeight*SizeOf(TFPColor));
    // get original pixels (the bottom line of OrigPixels will be Area.Top)
    y:=Area.Top;
    for yd:=-Radius to 0 do begin
      yr:=Max(0,y+yd);
      for x:=Area.Left to Area.Right-1 do begin
        OrigPixels[x-Area.Left+(yd+Radius)*OrigWidth]:=Img.Colors[x,yr];
      end;
    end;

    GetMem(VertSums,(OrigWidth+2*Radius)*SizeOf(TFPColor));

    // compute new pixels
    for y:=Area.Top to Area.Bottom-1 do begin
      // move OrigPixels one line up
      System.Move(OrigPixels[OrigWidth],OrigPixels[0],
        OrigWidth*(OrigHeight-1)*SizeOf(TFPColor));
      // and copy current line to OrigPixels
      for x:=Area.Left to Area.Right-1 do begin
        OrigPixels[x-Area.Left+Radius*OrigWidth]:=Img.Colors[x,y];
      end;

      // compute vertical sums
      // (for each x compute the sum of y-Radius..y+Radius colors
      //  multiplied with the gaussian matrix)
      StartX:=Area.Left-Radius;
      EndX:=Area.Right-1+Radius;
      for x:=StartX to EndX do begin
        // xr: x coordinate on img (coords out of bounds are mapped to the edges)
        xr:=Min(Max(0,x),Img.Width-1);
        // compute new color for this pixel
        NewRed:=0;
        NewGreen:=0;
        NewBlue:=0;
        NewAlpha:=0;
        for yd:=-Radius to Radius do begin
          // yr: y coordinate on img (coords out of bounds are mapped to the edges)
          yr:=Min(Max(0,y+yd),Img.Height-1);
          // get color
          if (yd<=0) and (xr>=Area.Left) and (xr<Area.Right) then begin
            // this pixel was replaced => use the OrigPixels
            Col:=OrigPixels[xr-Area.Left+(yd+Radius)*OrigWidth];
          end else begin
            Col:=Img.Colors[xr,yr];
          end;
          // multiply with gaussian matrix
          Multiplier:=Matrix1D[yd+Radius];
          inc(NewRed,Col.red*Multiplier);
          inc(NewGreen,Col.green*Multiplier);
          inc(NewBlue,Col.blue*Multiplier);
          inc(NewAlpha,Col.alpha*Multiplier);
          //writeln('GaussianBlur x=',x,' y=',y,' xd=',xd,' yd=',yd,' xr=',xr,' yr=',yr,' Col=',dbgs(Col),' NewCol=r=',hexstr(NewRed,8),'g=',hexstr(NewGreen,8),'b=',hexstr(NewBlue,8),'a=',hexstr(NewAlpha,8));
        end;
        NewCol.red:=NewRed shr 16;
        NewCol.green:=NewGreen shr 16;
        NewCol.blue:=NewBlue shr 16;
        NewCol.alpha:=NewAlpha shr 16;
        VertSums[x-StartX]:=NewCol;
      end;

      // compute horizontal sums
      // (for each x compute the sum of x-Radius..x+Radius vertical sums
      //  multiplied with the gaussian matrix)
      for x:=Area.Left to Area.Right-1 do begin
        // compute new color for this pixel
        NewRed:=0;
        NewGreen:=0;
        NewBlue:=0;
        NewAlpha:=0;
        for xd:=-Radius to Radius do begin
          xr:=x+xd;
          Col:=VertSums[xr-StartX];
          // multiply with gaussian matrix
          Multiplier:=Matrix1D[xd+Radius];
          inc(NewRed,Col.red*Multiplier);
          inc(NewGreen,Col.green*Multiplier);
          inc(NewBlue,Col.blue*Multiplier);
          inc(NewAlpha,Col.alpha*Multiplier);
          //writeln('GaussianBlur x=',x,' y=',y,' xd=',xd,' yd=',yd,' xr=',xr,' yr=',yr,' Col=',dbgs(Col),' NewCol=r=',hexstr(NewRed,8),'g=',hexstr(NewGreen,8),'b=',hexstr(NewBlue,8),'a=',hexstr(NewAlpha,8));
        end;
        NewCol.red:=NewRed shr 16;
        NewCol.green:=NewGreen shr 16;
        NewCol.blue:=NewBlue shr 16;
        NewCol.alpha:=NewAlpha shr 16;
        // set new pixel
        //writeln('GaussianBlur x=',x,' y=',y,' OldCol=',dbgs(img.Colors[x,y]),' NewCol=',dbgs(NewCol));
        Img.Colors[x,y]:=NewCol;
      end;
    end;
  finally
    if OrigPixels<>nil then FreeMem(OrigPixels);
    if VertSums<>nil then FreeMem(VertSums);
  end;
end;

procedure MatrixBlur2D(Img: TFPCustomImage; Radius: integer; Area: TRect;
  Matrix2D: PWord);
{ Implementation:
    It runs line by line from Area.Left to Area.Bottom-1.
    It allocates some temporary memory to store the original pixel values
    above the current line.
}
var
  y: Integer;
  x: Integer;
  OrigWidth: Integer;
  OrigHeight: LongInt;
  OrigPixels: PFPColor;
  NewRed, NewGreen, NewBlue, NewAlpha: cardinal;
  yd: LongInt;
  xd: LongInt;
  xr: Integer;
  yr: Integer;
  Col: TFPColor;
  NewCol: TFPColor;
  Multiplier: Word;
  MatrixWidth: Integer;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  MatrixWidth:=Radius*2+1;
  //WriteM('matrix ',Matrix2D,MatrixWidth);
  OrigPixels:=nil;
  try
    // allocate space for original pixels
    OrigWidth:=Area.Right-Area.Left;
    OrigHeight:=Radius+1;
    //DebugLn(['GaussianBlur ',OrigWidth,'*',OrigHeight,'*',SizeOf(TFPColor)]);
    GetMem(OrigPixels,OrigWidth*OrigHeight*SizeOf(TFPColor));
    // get original pixels (the bottom line of OrigPixels will be Area.Top)
    y:=Area.Top;
    for yd:=-Radius to 0 do begin
      yr:=Max(0,y+yd);
      for x:=Area.Left to Area.Right-1 do begin
        OrigPixels[x-Area.Left+(yd+Radius)*OrigWidth]:=Img.Colors[x,yr];
      end;
    end;

    // compute new pixels
    for y:=Area.Top to Area.Bottom-1 do begin
      // move OrigPixels one line up
      System.Move(OrigPixels[OrigWidth],OrigPixels[0],
        OrigWidth*(OrigHeight-1)*SizeOf(TFPColor));
      // and copy current line to OrigPixels
      for x:=Area.Left to Area.Right-1 do begin
        OrigPixels[x-Area.Left+Radius*OrigWidth]:=Img.Colors[x,y];
      end;
      // compute line
      for x:=Area.Left to Area.Right-1 do begin
        // compute new color for this pixel
        NewRed:=0;
        NewGreen:=0;
        NewBlue:=0;
        NewAlpha:=0;
        for yd:=-Radius to Radius do begin
          // yr: y coordinate on img (coords out of bounds are mapped to the edges)
          yr:=Min(Max(0,y+yd),Img.Height-1);
          for xd:=-Radius to Radius do begin
            // xr: x coordinate on img (coords out of bounds are mapped to the edges)
            xr:=Min(Max(0,x+xd),Img.Width-1);
            // get color
            if (yd<=0) and (xr>=Area.Left) and (xr<Area.Right) then begin
              // this pixel was replaced => use the OrigPixels
              Col:=OrigPixels[xr-Area.Left+(yd+Radius)*OrigWidth];
            end else begin
              Col:=Img.Colors[xr,yr];
            end;
            // multiply with gauss Matrix2D
            Multiplier:=Matrix2D[xd+Radius+(yd+Radius)*MatrixWidth];
            inc(NewRed,Col.red*Multiplier);
            inc(NewGreen,Col.green*Multiplier);
            inc(NewBlue,Col.blue*Multiplier);
            inc(NewAlpha,Col.alpha*Multiplier);
            //DebugLn(['GaussianBlur x=',x,' y=',y,' xd=',xd,' yd=',yd,' xr=',xr,' yr=',yr,' Col=',dbgs(Col),' NewCol=r=',hexstr(NewRed,8),'g=',hexstr(NewGreen,8),'b=',hexstr(NewBlue,8),'a=',hexstr(NewAlpha,8)]);
          end;
        end;
        NewCol.red:=NewRed shr 16;
        NewCol.green:=NewGreen shr 16;
        NewCol.blue:=NewBlue shr 16;
        NewCol.alpha:=NewAlpha shr 16;
        // set new pixel
        //DebugLn(['GaussianBlur x=',x,' y=',y,' OldCol=',dbgs(img.Colors[x,y]),' NewCol=',dbgs(NewCol)]);
        Img.Colors[x,y]:=NewCol;
      end;
    end;
  finally
    if OrigPixels<>nil then FreeMem(OrigPixels);
  end;
end;

function ComputeGaussianBlurMatrix1D(Radius: integer): PWord;
// returns a 1dim matrix of Words for the gaussian blur.
// Each word is a factor [0,1) multiplied by 65536.
// The total sum of the matrix is 65536.
const
  StandardDeviationToRadius = 3; // Pixels more far away as 3*Deviation are too small
var
  Width: Integer;
  Size: Integer;
  Matrix: PWord;
  Deviation: Single;
  m,p: Single;
  x: Integer;
  Value: Integer;
  MatrixSum: Integer;
  g: Single;
begin
  Width:=Radius*2+1;
  Size:=SizeOf(Word)*Width*Width;
  Matrix:=nil;
  GetMem(Matrix,Size);
  Result:=Matrix;
  FillByte(Matrix^,Size,0);
  // Deviation := Radius / 3
  // G(x) := (1 / SQRT( 2 * pi * Deviation^2)) * e^( - (x^2) / (2 * Deviation^2) )
  //                     m                     * e^(    x^2  *  p )
  // m := 1 / SQRT( 2 * pi * Deviation^2)
  // p := -1 / (2 * Deviation^2)
  Deviation:=single(Radius)/StandardDeviationToRadius;
  m:=1/Sqrt(2*pi*Deviation*Deviation);
  p:=-1/(2*Deviation*Deviation);
  for x:=0 to Radius do begin
    g:=m*exp(single(x*x)*p);
    Value:=floor(g*65536);
    Matrix[Radius+x]:=Value;
    Matrix[Radius-x]:=Value;
  end;
  // fix sum to 65536
  MatrixSum:=0;
  for x:=0 to Width-1 do
    inc(MatrixSum,Matrix[x]);
  Matrix[Radius]:=Min(High(Word),65536-MatrixSum+Matrix[Radius]);
end;

function ComputeGaussianBlurMatrix2D(Radius: integer): PWord;
// returns a 2dim matrix of Words for the gaussian blur.
// Each word is a factor [0,1) multiplied by 65536.
// The total sum of the matrix is 65536.
const
  StandardDeviationToRadius = 3; // Pixels more far away as 3*Deviation are too small
var
  Matrix: PWord;
  Size: Integer;
  Deviation: single;
  m,p: single;
  g: single;
  y: Integer;
  x: Integer;
  yd: Integer;
  xd: Integer;
  MatrixSum: Integer;
  Value: Word;
  Width: Integer;
begin
  Width:=Radius*2+1;
  Size:=SizeOf(Word)*Width*Width;
  Matrix:=nil;
  GetMem(Matrix,Size);
  Result:=Matrix;
  FillByte(Matrix^,Size,0);
  // Deviation = Radius / StandardDeviationToRadius
  // G(x,y) := (1 / (2 * pi * Deviation^2)) * e^( - (x^2 + y^2) / (2 * Deviation^2) )
  //         =              m               * e^(   (x^2 + y^2) *   p       )
  // m := 1 / (2 * pi * Deviation^2)
  // p := -1 / (2 * Deviation^2)
  Deviation:=single(Radius)/StandardDeviationToRadius;
  m:=1/(2*pi*Deviation*Deviation);
  p:=-1/(2*Deviation*Deviation);
  for y:=0 to Radius do begin
    yd:=Radius-y;
    yd:=yd*yd;
    for x:=y to Radius do begin
      xd:=Radius-x;
      xd:=xd*xd;
      g:=m*exp((single(xd)+single(yd))*p);
      Value:=floor(g*65536);
      Matrix[x+y*Width]:=Value;
      // mirror diagonally
      Matrix[y+x*Width]:=Value;
    end;
    // mirror horizontally
    for x:=Radius+1 to Width-1 do
      Matrix[x+y*Width]:=Matrix[(Width-x-1)+y*Width];
    // mirror vertically
    System.Move(Matrix[y*Width],Matrix[(Width-y-1)*Width],SizeOf(Word)*Width);
  end;
  // fix sum to 65536
  MatrixSum:=0;
  for y:=0 to Width-1 do
    for x:=0 to Width-1 do
      inc(MatrixSum,Matrix[x+y*Width]);
  Matrix[Radius+Radius*Width]:=Min(High(Word),65536-MatrixSum+Matrix[Radius+Radius*Width]);
end;

end.

