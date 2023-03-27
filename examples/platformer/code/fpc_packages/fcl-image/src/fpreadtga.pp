{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{ - 22/11/2007 Modified by Laurent Jacques for support all format }

{$mode objfpc}
{$h+}

unit FPReadTGA;

interface

uses FPImage, classes, sysutils, targacmn;

const
  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_GRAY_IMAGE = 3;

type

  { TFPReaderTarga }

  TFPReaderTarga = class (TFPCustomImageReader)
  Private
    Procedure FreeBuffers;       // Free (and nil) buffers.
  protected
    Header         : TTargaHeader;
    AlphaBits      : Byte;
    Identification : ShortString;
    Compressed,
    BottomUp       : Boolean;
    BytesPerPixel  : Byte;
    FPalette       : PFPColor;
    FScanLine      : PByte;
    FLineSize      : Integer;
    FPaletteSize   : Integer;
    FBlockCount    : Integer;
    FPixelCount    : Integer;
    FLastPixel     : Packed Array[0..3] of byte;
    // AnalyzeHeader will allocate the needed buffers.
    Procedure AnalyzeHeader(Img : TFPCustomImage);
    procedure CreateGrayPalette;
    Procedure ReadPalette(Stream : TStream);
    procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
    procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
    // required by TFPCustomImageReader
    procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Stream:TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

Implementation

Constructor TFPReaderTarga.Create;

begin
end;

Destructor TFPReaderTarga.Destroy;

begin
  FreeBuffers;
  Inherited;
end;

Procedure TFPReaderTarga.FreeBuffers;

begin
  If (FScanLine<>Nil) then
    begin
    FreeMem(FScanLine);
    FScanLine:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FScanLine:=Nil;
    end;
end;

Procedure TFPReaderTarga.AnalyzeHeader(Img : TFPCustomImage);

begin
  With Header do
    begin
    if not (ImgType in [1, 2, 3, 9, 10, 11]) and
       not (PixelSize in [8, 16, 24, 32]) then
      Raise Exception.Create('Unknown/Unsupported Targa image type');
    BottomUp:=(Flags and $20) <>0;
    AlphaBits := Flags and $0F;
    BytesPerPixel:=PixelSize;
    Compressed:=ImgType>8;
    If Compressed then
      ImgType:=ImgType-8;
    FLineSize:=(BytesPerPixel div 8)*ToWord(Width);
    GetMem(FScanLine,FLineSize);

    if ImgType = TARGA_GRAY_IMAGE then
      FPaletteSize:=SizeOf(TFPColor)*255
    else
      FPaletteSize:=SizeOf(TFPColor)*ToWord(MapLength);
    GetMem(FPalette,FPaletteSize);
    Img.Width:=ToWord(Width);
    Img.Height:=ToWord(Height);
    end;
end;

Procedure TFPReaderTarga.CreateGrayPalette;

Var
  I : Integer;

Begin
  For I:=0 To 255 Do
  Begin
    With FPalette[I] do
      begin
      Red:=I*255;
      Green:=I*255;
      Blue:=I*255;
      Alpha:=AlphaOpaque;
      end;
  end;
End;

Procedure TFPReaderTarga.ReadPalette(Stream : TStream);

Var
  BGREntry : TBGREntry;
  BGRAEntry : TBGRAEntry;
  I : Integer;

begin
  Case Header.MapEntrySize Of
     16, 24:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGREntry, SizeOf(BGREntry));
          With FPalette[I] do
            begin
            Red:=BGREntry.Red shl 8;
            Green:=BGREntry.Green shl 8;
            Blue:=BGREntry.Blue shl 8;
            Alpha:=alphaOpaque;
            end;
        end;
     32:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGRAEntry,SizeOf(BGRAEntry));
          With FPalette[I] do
            begin
            Red:=BGRAEntry.Red shl 8;
            Green:=BGRAEntry.Green shl 8;
            Blue:=BGRAEntry.Blue shl 8;
            if alphaBits = 8 then
               if (BGRAEntry.Alpha and $80) <> 0 then
                 Alpha:=alphaTransparent
               else
                 Alpha:=AlphaOpaque;
            end;
        end;
    end;
end;


Procedure TFPReaderTarga.InternalRead  (Stream:TStream; Img:TFPCustomImage);

var
  H,Row : Integer;

begin
  Stream.Read(Header,SizeOf(Header));
  AnalyzeHeader(Img);
  If Header.IdLen>0 then
    begin
    SetLength(Identification,Header.IDLen);
    Stream.Read(Identification[1],Header.Idlen);
    If Length(Identification)<>0 then
      Img.Extra[KeyIdentification]:=Identification;
    end;

  If Header.MapType<>0 then
    ReadPalette(Stream);
  if Header.ImgType = TARGA_GRAY_IMAGE then
    CreateGrayPalette;

  H:=Img.height;
  If BottomUp then
    For Row:=0 to H-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end
  else
    For Row:=H-1 downto 0 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
end;

Procedure TFPReaderTarga.ReadScanLine(Row : Integer; Stream : TStream);

Var
  P : PByte;
  B : Byte;
  I,J : Integer;

begin
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
    P:=FScanLine;
    For I:=0 to ToWord(Header.Width)-1 do
      begin
      If (FPixelCount>0) then
        Dec(FPixelCount)
      else
        begin
        Dec(FBlockCount);
        If (FBlockCount<0) then
          begin
          Stream.ReadBuffer(B,1);
          If (B and $80)<>0 then
            begin
            FPixelCount:=B and $7F;
            FblockCount:=0;
            end
          else
            FBlockCount:=B and $7F
          end;
        Stream.ReadBuffer(FlastPixel,BytesPerPixel shr 3);
        end;
      For J:=0 to (BytesPerPixel shr 3)-1 do
        begin
        P[0]:=FLastPixel[j];
        Inc(P);
        end;
      end;
    end;
end;

Procedure TFPReaderTarga.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Col : Integer;
  C   : TFPColor;
  W   : Word;
  P   : PByte;

begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Case Header.ImgType of
    TARGA_INDEXED_IMAGE
      : for Col:=0 to Img.width-1 do
         Img.Colors[Col,Row]:=FPalette[P[Col]];
    TARGA_TRUECOLOR_IMAGE
      : for Col:=0 to Img.Width-1 do
          begin
          // Fill C depending on number of pixels.
          case BytesPerPixel of
          8,16 : begin
                 W:=P[0];
                 inc(P);
                 W:=W or (P[0] shl 8);
                 With C do
                   begin
                   Red:=((W)shr 10) shl 11;
                   Green:=((w)shr 5) shl 11;
                   Blue:=((w)) shl 11;
                   end;
                end;
          24,32 : With C do
                  begin
                  Blue:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Green:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Red:=P[0] or (P[0] shl 8);
                  If bytesPerPixel=32 then
                    begin
                    Inc(P);
                    Alpha:=AlphaOpaque;
                    if alphaBits = 8 then
                      if (P[0] and $80) = 0 then
                        Alpha:=alphaTransparent;
                    end;
                  end;
          end; // Case BytesPerPixel;
          Img[Col,Row]:=C;
          Inc(P);
          end;
    TARGA_GRAY_IMAGE
      :  case BytesPerPixel of
           8 : for Col:=0 to Img.width-1 do
                 Img.Colors[Col,Row]:=FPalette[P[Col]];
          16 : for Col:=0 to Img.width-1 do
               begin
                 With C do
                 begin
                   Blue:=FPalette[P^].blue;
                   Green:=FPalette[P^].green;
                   Red:=FPalette[P^].red;
                   Inc(P);
                   Alpha:=AlphaOpaque;
                   if alphaBits = 8 then
                    if (P[0] and $80) = 0 then
                        Alpha:=alphaTransparent;
                   Inc(P);
                 end;
               Img[Col,Row]:=C;
               end;
         end;
  end;
end;

function  TFPReaderTarga.InternalCheck (Stream:TStream) : boolean;

var
  hdr: TTargaHeader;
  oldPos: Int64;
  n: Integer;
  
begin
  Result:=False;
  if Stream = nil then
    exit;
  oldPos := Stream.Position;
  try
    n := SizeOf(hdr);
    Result:=(Stream.Read(hdr, n)=n)
            and (hdr.ImgType in [1, 2, 3, 9, 10, 11]) 
            and (hdr.PixelSize in [8, 16, 24, 32]);
  finally
    Stream.Position := oldPos;
  end;
end;


initialization
  ImageHandlers.RegisterImageReader ('TARGA Format', 'tga', TFPReaderTarga);
end.
