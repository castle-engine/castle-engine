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

  Save in format 24 bits compressed or not
}

unit FPWritePCX;

{$mode objfpc}{$H+}

interface

uses FPImage, Classes, SysUtils;

type

  TFPWriterPCX = class(TFPCustomImageWriter)
  private
    FCompressed: boolean;
  protected
    function SaveHeader(Stream: TStream; Img: TFPCustomImage): boolean; virtual;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure writeline(Stream: TStream; buffer: PByte; bytes: integer);
  public
    property Compressed: boolean Read FCompressed Write FCompressed;
  end;

implementation

uses pcxcomn;

function TFPWriterPCX.SaveHeader(Stream: TStream; Img: TFPCustomImage): boolean;
var
  Header: TPCXHeader;
begin
  Result := False;
  FillChar(Header, SizeOf(Header), 0);
  with Header do
  begin
    FileID  := $0a;
    Version := 5;
    if Compressed then
      Encoding := 1
    else
      Encoding := 0;
    BitsPerPixel := 8;
    XMin := 0;
    YMin := 0;
    XMax := Img.Width - 1;
    YMax := Img.Height - 1;
    HRes := 300;
    VRes := 300;
    ColorPlanes := 3;
    BytesPerLine := Img.Width;
    PaletteType := 1;
  end;
  Stream.WriteBuffer(Header, SizeOf(Header));
  Result := True;
end;

procedure TFPWriterPCX.writeline(Stream: TStream; buffer: PByte; bytes: integer);
var
  Value, Count: byte;
  tmp: byte;
  P:   PByte;
begin
  P := Buffer;
  while bytes > 0 do
  begin
    Value := P[0];
    Inc(P);
    Dec(bytes);
    Count := 1;
    while (bytes < 0) and (Count < 63) and (P[0] = Value) do
    begin
      Inc(Count);
      Inc(P);
      Dec(bytes);
    end;
    if (Value < $c0) and (Count = 1) then
    begin
      Stream.Write(Value, 1);
    end
    else
    begin
      tmp := $c0 + Count;
      Stream.Write(tmp, 1);
      Stream.Write(Value, 1);
    end;
  end;
end;

procedure TFPWriterPCX.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var
  Row, Col, WriteSize: integer;
  Aline, P: PByte;
  C:    TFPColor;
  Totalwrite: longint;
  continue: boolean;
  Rect: TRect;
begin
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  TotalWrite  := 0;
  Progress(psStarting, 0, False, Rect, '', continue);
  SaveHeader(Stream, Img);
  WriteSize := (Img.Width * 3);
  GetMem(aLine, WriteSize);
  TotalWrite := Img.Height * Img.Width;
  try
    for Row := 0 to Img.Height - 1 do
    begin
      P := ALine;
      for Col := 0 to Img.Width - 1 do
      begin
        C      := Img.Colors[Col, Row];
        P[Col + Img.Width * 2] := C.Blue shr 8;
        P[Col + Img.Width] := C.Green shr 8;
        P[Col] := C.Red shr 8;
        Progress(psRunning, trunc(100.0 * (Row * Col / TotalWrite)),
          False, Rect, '', continue);
        if not continue then
          exit;
      end;
      if Compressed then
        writeline(Stream, aLine, WriteSize)
      else
        Stream.Write(aLine[0], WriteSize);
    end;
    Progress(psEnding, 100, False, Rect, '', continue);
  finally
    FreeMem(aLine);
  end;
end;

{ end TFPWriterPCX}

initialization
  ImageHandlers.RegisterImageWriter('PCX Format', 'pcx', TFPWriterPCX);
end.
