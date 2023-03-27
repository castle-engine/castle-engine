{ Copyright (C) 2007 Laurent Jacques

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  Load all format compressed or not
}

unit FPReadPCX;

{$mode objfpc}{$H+}

interface

uses FPImage, Classes, SysUtils, pcxcomn;

type

  { TFPReaderPCX }

  TFPReaderPCX = class(TFPCustomImageReader)
  private
    FCompressed: boolean;
  protected
    Header:     TPCXHeader;
    BytesPerPixel: byte;
    FScanLine:  PByte;
    FLineSize:  integer;
    TotalWrite: longint;
    procedure CreateGrayPalette(Img: TFPCustomImage);
    procedure CreateBWPalette(Img: TFPCustomImage);
    procedure CreatePalette16(Img: TFPCustomImage);
    procedure ReadPalette(Stream: TStream; Img: TFPCustomImage);
    procedure AnalyzeHeader(Img: TFPCustomImage);
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure ReadScanLine(Row: integer; Stream: TStream); virtual;
    procedure UpdateProgress(percent: longint);
    procedure WriteScanLine(Row: integer; Img: TFPCustomImage); virtual;
  public
    property Compressed: boolean Read FCompressed;
  end;

implementation


procedure TFPReaderPCX.CreatePalette16(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 15 do
  begin
    with c, header do
    begin
      Red   := ColorMap[I].red shl 8;
      Green := ColorMap[I].Green shl 8;
      Blue  := ColorMap[I].Blue shl 8;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateGrayPalette(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 255 do
  begin
    with c do
    begin
      Red   := I * 255;
      Green := I * 255;
      Blue  := I * 255;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateBWPalette(Img: TFPCustomImage);
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  Img.Palette.Add(colBlack);
  Img.Palette.Add(colWhite);
end;

procedure TFPReaderPCX.ReadPalette(Stream: TStream; Img: TFPCustomImage);
var
  RGBEntry: TRGB;
  I:      integer;
  c:      TFPColor;
  OldPos: integer;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  OldPos := Stream.Position;
  Stream.Position := Stream.Size - 768;
  for I := 0 to 255 do
  begin
    Stream.Read(RGBEntry, SizeOf(RGBEntry));
    with c do
    begin
      Red   := RGBEntry.Red shl 8;
      Green := RGBEntry.Green shl 8;
      Blue  := RGBEntry.Blue shl 8;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(C);
  end;
  Stream.Position := OldPos;
end;

procedure TFPReaderPCX.AnalyzeHeader(Img: TFPCustomImage);
begin
  with Header do
  begin
    if not ((FileID in [$0A, $0C]) and (ColorPlanes in [1, 3, 4]) and
      (Version in [0, 2, 3, 5]) and (PaletteType in [1, 2])) then
      raise Exception.Create('Unknown/Unsupported PCX image type');
    BytesPerPixel := BitsPerPixel * ColorPlanes;
    FCompressed   := Encoding = 1;
    Img.Width     := XMax - XMin + 1;
    Img.Height    := YMax - YMin + 1;
    FLineSize     := (BytesPerLine * ColorPlanes);
    GetMem(FScanLine, FLineSize);
  end;
end;

procedure TFPReaderPCX.ReadScanLine(Row: integer; Stream: TStream);
var
  P: PByte;
  B: byte;
  bytes, Count: integer;
begin
  P     := FScanLine;
  bytes := FLineSize;
  Count := 0;
  if Compressed then
  begin
    while bytes > 0 do
    begin
      if (Count = 0) then
      begin
        Stream.ReadBuffer(B, 1);
        if (B < $c0) then
          Count := 1
        else
        begin
          Count := B - $c0;
          Stream.ReadBuffer(B, 1);
        end;
      end;
      Dec(Count);
      P[0] := B;
      Inc(P);
      Dec(bytes);
    end;
  end
  else
    Stream.ReadBuffer(FScanLine^, FLineSize);
end;

procedure TFPReaderPCX.UpdateProgress(percent: longint);
var
  continue: boolean;
  Rect:     TRect;
begin
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psRunning, 0, False, Rect, '', continue);
end;

procedure TFPReaderPCX.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H, Row:   integer;
  continue: boolean;
  Rect:     TRect;
begin
  TotalWrite  := 0;
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psStarting, 0, False, Rect, '', continue);
  Stream.Read(Header, SizeOf(Header));
  AnalyzeHeader(Img);
  case BytesPerPixel of
    1: CreateBWPalette(Img);
    4: CreatePalette16(Img);
    8: ReadPalette(stream, Img);
    else
      if (Header.PaletteType = 2) then
        CreateGrayPalette(Img);
  end;
  H := Img.Height;
  TotalWrite := Img.Height * Img.Width;
  for Row := 0 to H - 1 do
  begin
    ReadScanLine(Row, Stream);
    WriteScanLine(Row, Img);
  end;
  Progress(psEnding, 100, False, Rect, '', continue);
  freemem(FScanLine);
end;

procedure TFPReaderPCX.WriteScanLine(Row: integer; Img: TFPCustomImage);
var
  Col:   integer;
  C:     TFPColor;
  P, P1, P2, P3: PByte;
  Z2:    word;
  color: byte;
begin
  C.Alpha := AlphaOpaque;
  P  := FScanLine;
  Z2 := Header.BytesPerLine;
  begin
    case BytesPerPixel of
      1:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Img.Colors[Col, Row] := Img.Palette[1]
          else
            Img.Colors[Col, Row] := Img.Palette[0];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      4:
      begin
        P1 := P;
        Inc(P1, Z2);
        P2 := P;
        Inc(P2, Z2 * 2);
        P3 := P;
        Inc(P3, Z2 * 3);
        for Col := 0 to Img.Width - 1 do
        begin
          color := 0;
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1);
          if (P1[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 1);
          if (P2[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 2);
          if (P3[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 3);
          Img.Colors[Col, Row] := Img.Palette[color];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      8:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          Img.Colors[Col, Row] := Img.Palette[P[Col]];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      24:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          with C do
          begin
            Red   := P[col] or (P[col] shl 8);
            Blue  := P[col + Z2 * 2] or (P[col + Z2 * 2] shl 8);
            Green := P[col + Z2] or (P[col + Z2] shl 8);
            Alpha := alphaOpaque;
          end;
          Img[col, row] := C;
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
    end;
  end;
end;

function TFPReaderPCX.InternalCheck(Stream: TStream): boolean;
var
  hdr: TPcxHeader;
  n: Integer;
  oldPos: Int64;
begin
  Result:=False;
  if Stream = nil then
    exit;
  oldPos := Stream.Position;
  try
    n:=SizeOf(hdr);
    Result:=(Stream.Read(hdr, n)=n)
            and (hdr.FileID in [$0A, $0C]) 
            and (hdr.ColorPlanes in [1, 3, 4]) 
            and (hdr.Version in [0, 2, 3, 5])
            and (hdr.PaletteType in [1, 2]);
  finally
    Stream.Position := oldPos;
  end;
end;


initialization
  ImageHandlers.RegisterImageReader('PCX Format', 'pcx', TFPReaderPCX);
end.
