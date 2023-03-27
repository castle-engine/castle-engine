{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Michael Van Canneyt of the Free Pascal development team

    TARGA writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{$mode objfpc}{$h+}
unit FPWriteTGA;

interface

uses FPImage, classes, sysutils;

type

  TFPWriterTarga = class (TFPCustomImageWriter)
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  end;


implementation

uses targacmn;

function TFPWriterTarga.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;

var
  Header : TTargaHeader;
  ID : ShortString;

begin
  Result:=False;
  ID:=Img.Extra[KeyIdentification];
  FillChar(Header,SizeOf(Header),0);
  With Header do
    begin
    IDLen:=Length(ID);
    MapType:=0; // No colormap. Uncompressed RGB Only.
    ImgType:=2; // Uncompressed RGB
    MapStart:=FromWord(0); // No data
    MapLength:=FromWord(0); // No colormap yet.
    MapEntrySize:=0; // No colormap yet.
    OriginX:= FromWord(0);
    OriginY:=FromWord(0);
    Width:=FromWord(Img.Width);
    Height:=FromWord(Img.Height);
    PixelSize:=24; // BGR data.
    Flags:=$20; // Top-town, non interlaced.
  end;
  Stream.WriteBuffer(Header,SizeOf(Header));
  If Header.IDlen>0 then
    Stream.WriteBuffer(Id[1],Header.IDLen);
  Result:=true;
end;

procedure TFPWriterTarga.InternalWrite (Stream:TStream; Img:TFPCustomImage);

var
  Row,Col,WriteSize:Integer;
  Aline,P: PByte;
  C : TFPColor;

begin
  SaveHeader(Stream,Img);
  WriteSize:=Img.Width*3;
  GetMem(aLine,WriteSize);
  Try
    for Row:=0 to Img.Height-1 do
      begin
      P:=ALine;
      For Col:=0 to Img.width-1 do
        begin
        C:=Img.Colors[Col,Row];
        P^:=C.Blue shr 8;
        Inc(P);
        P^:=C.Green shr 8;
        Inc(P);
        P^:=C.Red shr 8;
        Inc(P);
        end;
      Stream.Write(aLine[0],WriteSize);
      end;
  Finally
    FreeMem(aLine);
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('TARGA Format', 'tga', TFPWriterTarga);
end.
