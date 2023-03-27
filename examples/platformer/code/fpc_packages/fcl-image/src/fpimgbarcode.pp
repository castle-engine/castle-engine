{
    This file is part of the Free Pascal FCL library.
    Copyright (c) 2017 by Michael Van Canneyt
    member of the Free Pascal development team

    Barcode drawing routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPImgBarCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcanvas, fpimage, types, fpbarcode;

Type
  // So people don't need to include fpBarcode
  TBarcodeEncoding = fpbarcode.TBarcodeEncoding;

  { TFPDrawBarCode }

  TFPDrawBarCode = Class
  private
    FCanvas: TFPCustomCanvas;
    FClipping: Boolean;
    FEncoding: TBarcodeEncoding;
    FImage: TFPCustomImage;
    FRect: TRect;
    FText: String;
    FUnitWidth: Integer;
    FWeight: Double;
    FFreeCanvas : Boolean;
    FWidths : TBarWidthArray;
    procedure SetCanvas(AValue: TFPCustomCanvas);
    procedure SetEncoding(AValue: TBarcodeEncoding);
    procedure SetImage(AValue: TFPCustomImage);
    procedure SetUnitWidth(AValue: Integer);
    procedure SetWeight(AValue: Double);
  Protected
    procedure CheckFreeCanvas;
    Procedure CalcWidths; virtual;
    Property FreeCanvas : Boolean Read FFreeCanvas Write FFreeCanvas;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    procedure CheckCanvas; virtual;
    // Returns true if the text was drawn, false if not.
    Function Draw : Boolean; virtual;
    // Returns true if the text can be drawn using current encoding, false if not
    Function AllowDraw : Boolean;
    // Informational: calc width of text using current parameters. -1 if the text cannot be drawn.
    Function CalcWidth : Integer;
    // One of Image or Canvas must be set.
    Property Image : TFPCustomImage Read FImage Write SetImage;
    Property Canvas : TFPCustomCanvas Read FCanvas Write SetCanvas;
    // Rectangle in which to draw
    Property Rect : TRect Read FRect Write FRect;
    // Unit width of a bar
    Property UnitWidth : Integer Read FUnitWidth Write SetUnitWidth;
    // Weight to use when calculating bar widths.
    Property Weight : Double Read FWeight Write SetWeight;
    // Encoding to use
    Property Encoding : TBarcodeEncoding Read FEncoding Write SetEncoding;
    // Text to draw.
    Property Text : String Read FText Write FText;
    // If true, the barcode will be clipped if it falls outside rect.
    Property Clipping : Boolean Read FClipping Write FClipping;
  end;

Function DrawBarCode(Img : TFPCustomImage; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;
Function DrawBarCode(Img : TFPCustomImage; Rect : TRect; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

implementation

uses
  FPImgCanv;

Function DrawBarCode(Img : TFPCustomImage; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

Var
  T : TRect;

begin
  T.Left:=0;
  T.Top:=0;
  T.Right:=Img.Width-1;
  T.Bottom:=Img.Height-1;
  Result:=DrawBarCode(Img,T,S,E,aWidth,aWeight);
end;

Function DrawBarCode(Img : TFPCustomImage; Rect : TRect; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

Var
  DBC : TFPDrawBarCode;

begin
  DBC:=TFPDrawBarCode.Create;
  try
    DBC.Rect:=Rect;
    DBC.UnitWidth:=aWidth;
    DBC.Weight:=aWeight;
    DBC.Encoding:=E;
    DBC.Text:=S;
    DBC.Image:=Img;
    Result:=DBC.Draw;
  finally
    DBC.Free;
  end;
end;
{ TFPDrawBarCode }
procedure TFPDrawBarCode.CheckFreeCanvas;

begin
  if FFreeCanvas then
    FreeAndNil(FCanvas)
  else
    FCanvas:=Nil;
end;

procedure TFPDrawBarCode.SetImage(AValue: TFPCustomImage);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
  CheckFreeCanvas;
end;

procedure TFPDrawBarCode.SetUnitWidth(AValue: Integer);
begin
  if FUnitWidth=AValue then Exit;
  FUnitWidth:=AValue;
  CalcWidths;
end;

procedure TFPDrawBarCode.SetWeight(AValue: Double);
begin
  if FWeight=AValue then Exit;
  FWeight:=AValue;
  CalcWidths;
end;

procedure TFPDrawBarCode.CalcWidths;
begin
  FWidths:=CalcBarWidths(FEncoding,UnitWidth,Weight);
end;

procedure TFPDrawBarCode.SetCanvas(AValue: TFPCustomCanvas);
begin
  if FCanvas=AValue then Exit;
  CheckFreeCanvas;
  FCanvas:=AValue;
end;

procedure TFPDrawBarCode.SetEncoding(AValue: TBarcodeEncoding);
begin
  if FEncoding=AValue then Exit;
  FEncoding:=AValue;
  CalcWidths;
end;

constructor TFPDrawBarCode.Create;
begin
  FUnitWidth:=1;
  FWeight:=2.0;
  FEncoding:=beEAN8;
  CalcWidths;
end;

Destructor TFPDrawBarCode.Destroy;

begin
  CheckFreeCanvas;
end;

procedure TFPDrawBarCode.CheckCanvas;

begin
  if (FCanvas=Nil) then
    begin
    FCanvas:=TFPImageCanvas.create(FImage);
    FFreeCanvas:=True;
    end;
end;

Function TFPDrawBarCode.Draw : Boolean;

Var
  Cnv : TFPCustomCanvas;
  I,L,MaxWidth, W, H : integer;
  xOffset: integer;
  BarRect : TRect;
  BP : TBarParams;
  Data : TBarTypeArray;

begin
  Result:=AllowDraw;
  if not Result then
    exit;
  CheckCanvas;
  Cnv:=FCanvas;
  Data:=StringToBarTypeArray(Text,FEncoding);
  xOffset := 0;
  Cnv.Brush.FPColor := colWhite;
  Cnv.Brush.Style:=bsSolid;
  Cnv.FillRect(Rect);
  Cnv.Pen.Width := 1;
  I:=0;
  L:=Length(Data);
  MaxWidth:=Rect.Right-Rect.Left;
  While (I<L) and (Not Clipping or (XOffset<MaxWidth))  do
    begin
    BP:=BarTypeToBarParams(Data[i]);
    case BP.c of
      bcBlack : Cnv.Pen.FPColor := colBlack;
      bcWhite : Cnv.Pen.FPColor := colWhite;
    end;
    W:=FWidths[BP.w];
    Cnv.Brush.FPColor:=Cnv.Pen.FPColor;
    H:=Rect.Bottom-Rect.Top;
    if BP.h=bhTwoFifth then
      H:=H*2 div 5;
    BarRect.Left:=Rect.Left+xOffset;
    BarRect.Top:=Rect.Top;
    BarRect.Bottom:=Rect.Top+H;
    BarRect.Right:=BarRect.Left + W-1;
    if (Not Clipping or (BarRect.Right<=MaxWidth)) then
      Cnv.FillRect(BarRect);
    xOffset:=xOffset + W;
    Inc(I);
    end;
end;

function TFPDrawBarCode.AllowDraw: Boolean;

begin
  Result:=StringAllowsBarEncoding(FText,FEncoding);
end;

function TFPDrawBarCode.CalcWidth: Integer;

begin
  if AllowDraw then
    Result:=CalcStringWidthInBarCodeEncoding(FText,FEncoding,UnitWidth,Weight)
  else
    Result:=-1;
end;


end.

