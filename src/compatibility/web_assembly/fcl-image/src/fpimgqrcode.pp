{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Michael Van Canneyt, member of the Free Pascal development team

    fpImage QR code drawing algorithm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPImgQRCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpImage, fpqrcodegen;

type

  { TImageQRCodeGenerator }

  TImageQRCodeGenerator = Class(TQRCodeGenerator)
  private
    FPixelSize: Integer;
    FBorder: Integer;
  Public
    Constructor Create; override;
    Procedure Draw(Img : TFPCustomImage);
    Procedure Draw(Img : TFPCustomImage; DestX, DestY: Integer);
    Function SaveToStream(const AStream : TStream; AWriter: TFPCustomImageWriter): Boolean;
    Function SaveToFile(const AFileName : String): Boolean;
    Property PixelSize : Integer Read FPixelSize Write FPixelSize default 2;
    Property Border : Integer Read FBorder Write FBorder default 0;
  end;

Procedure DrawQRCode(Img : TFPCustomImage; QRCode : TQRBuffer; aOrigin: TPoint; PixelSize : Byte = 1);

implementation

Procedure DrawQRCode(Img : TFPCustomImage; QRCode : TQRBuffer; aOrigin: TPoint; PixelSize : Byte = 1);

Var
  X,Y,PH,PV,PX,PY,S : Word;
  col : TFPColor;

begin
  PY:=aOrigin.Y;
  S:=QRGetSize(QRCode);
//  Writeln('Size ',S);
  if S=0 then
      exit;
  For Y:=0 to S-1 do
    begin
    PX:=aOrigin.X;
    For X:=0 to S-1 do
      begin
      if QRgetModule(QRCode,X,Y) then
        begin
        Col:=colBlack;
//        Write('##');
        end
      else
        begin
        Col:=colWhite;
//        Write('  ');
        end;
      For pV:=0 to PixelSize-1 do
        For pH:=0 to PixelSize-1 do
          Img.Colors[PX+PH,PY+PV]:=col;
      Inc(PX,PixelSize);
      end;
//    Writeln;
    Inc(PY,PixelSize);
    end;
end;

{ TImageQRCodeGenerator }

constructor TImageQRCodeGenerator.Create;
begin
  inherited Create;
  FPixelSize:=2;
end;

procedure TImageQRCodeGenerator.Draw(Img: TFPCustomImage);
begin
  Draw(Img, 0, 0);
end;

procedure TImageQRCodeGenerator.Draw(Img: TFPCustomImage; DestX,
  DestY: Integer);
var
  X,Y : Integer;
  S,D : Integer;
begin
  S:=Size;
  D:=PixelSize*S;
  if Border>0 then
    begin
    For X:=0 to D+(Border*2)-1 do
      For Y:=1 to Border do
        begin
        Img[DestX+X,DestY+Y-1]:=colWhite;
        Img[DestX+X,DestY+D+(Border*2)-Y]:=colWhite;
        end;
    For Y:=Border to D+Border-1 do
      For X:=1 to Border do
        begin
        Img[DestX+X-1,DestY+Y]:=colWhite;
        Img[DestX+D+(Border*2)-X,DestY+Y]:=colWhite;
        end;
    end;
  DrawQRCode(Img,Bytes,Point(DestX+Border,DestY+Border),PixelSize);
end;

function TImageQRCodeGenerator.SaveToFile(const AFileName: String): Boolean;

  {$IF NOT (FPC_FULLVERSION >= 30101)}
  function FindWriterFromExtension(extension: String): TFPCustomImageWriterClass;
  var
    s: string;
    r: integer;
  begin
    extension := lowercase (extension);
    if (extension <> '') and (extension[1] = '.') then
      system.delete (extension,1,1);
    with ImageHandlers do
      begin
        r := count-1;
        s := extension + ';';
        while (r >= 0) do
          begin
          Result := ImageWriter[TypeNames[r]];
          if (pos(s,{$if (FPC_FULLVERSION = 20604)}Extentions{$else}Extensions{$endif}[TypeNames[r]]+';') <> 0) then
            Exit;
          dec (r);
          end;
      end;
    Result := nil;
  end;

  function FindWriterFromFileName(const filename: String): TFPCustomImageWriterClass;
  begin
    Result := FindWriterFromExtension(ExtractFileExt(filename));
  end;
  {$ENDIF}
  
Var
  WriterClass : TFPCustomImageWriterClass;
  Writer : TFPCustomImageWriter;
  Stream : TFileStream;


begin
  Result := Size>0;
  if not Result then exit;
  WriterClass := {$IF (FPC_FULLVERSION >= 30101)}TFPCustomImage.{$ENDIF}FindWriterFromFileName(AFileName);
  if Assigned(WriterClass) then
  begin
    Writer := nil;
    Stream := nil;
    try
      Writer := WriterClass.Create;
      Stream := TFileStream.Create(AFileName, fmCreate);
      SaveToStream(Stream, Writer);
    finally
      Stream.Free;
      Writer.Free;
    end;
  end else
    FPImageException.CreateFmt(ErrorText[StrCantDetermineType], [AFileName]);
end;

function TImageQRCodeGenerator.SaveToStream(const AStream: TStream;
  AWriter: TFPCustomImageWriter): Boolean;
Var
  Img : TFPCustomImage;
  D,S : Word;


begin
  S:=Size;
  Result := S>0;
  if not Result then exit;
  D:=PixelSize*S;
  Img:=TFPCompactImgGray8Bit.Create(D+Border*2,D+Border*2);
  try
    Draw(Img);
    Img.SaveToStream(AStream, AWriter);
  finally
    Img.Free;
  end;
end;

end.

