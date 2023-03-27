{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    PNM writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{Support for writing PNM (Portable aNyMap) formats added :
    * PBM (P1,P4) : Portable BitMap format : 1 bit per pixel
    * PGM (P2,P5) : Portable GrayMap format : 8 bits per pixel for P2 (ASCII), 8 or 16 bit for P5 (binary)
    * PPM (P3,P6) : Portable PixelMap format : 24 bits per pixel for P3 (ASCII), 24 or 48 bit for P6 (binary)}
{$mode objfpc}{$h+}
unit FPWritePNM;

interface

uses FPImage, classes, sysutils;

type
  TPNMColorDepth = (pcdAuto,pcdBlackWhite, pcdGrayscale, pcdRGB);

  { TFPWriterPNM }

  TFPWriterPNM = class(TFPCustomImageWriter)
  private
    FFullWidth: Boolean;
    FColorDepth: TPNMColorDepth;
    FBinaryFormat: boolean;
    procedure SetFullWidth(AValue: Boolean);
  protected
    procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
  public
    Property FullWidth: Boolean Read FFullWidth Write SetFullWidth; {if true write 16 bits per colour for P5, P6 formats}
    function GuessColorDepthOfImage(Img: TFPCustomImage): TPNMColorDepth;
    function GetColorDepthOfExtension(AExtension: string): TPNMColorDepth;
    function GetFileExtension(AColorDepth: TPNMColorDepth): string;
    constructor Create; override;
    Property BinaryFormat : Boolean Read FBinaryFormat Write FBinaryFormat;
    Property ColorDepth: TPNMColorDepth Read FColorDepth Write FColorDepth;
  end;

  { TFPWriterPBM }

  TFPWriterPBM = class(TFPWriterPNM)
      constructor Create; override;
  end;

  { TFPWriterPGM }

  TFPWriterPGM = class(TFPWriterPNM)
      constructor Create; override;
  end;

  { TFPWriterPPM }

  TFPWriterPPM = class(TFPWriterPNM)
      constructor Create; override;
  end;

procedure SaveImageToPNMFile(Img: TFPCustomImage; filename: string; UseBinaryFormat: boolean = true);

implementation

procedure SaveImageToPNMFile(Img: TFPCustomImage; filename: string; UseBinaryFormat: boolean = true);
var writer: TFPWriterPNM;
    curExt: string;
begin
  writer := TFPWriterPNM.Create;
  writer.BinaryFormat := UseBinaryFormat;
  curExt := Lowercase(ExtractFileExt(filename));
  if (curExt='.pnm') or (curExt='') then
  begin
    writer.ColorDepth := writer.GuessColorDepthOfImage(Img);
    filename := ChangeFileExt(filename,'.'+writer.GetFileExtension(writer.ColorDepth));
  end else
    writer.ColorDepth := writer.GetColorDepthOfExtension(curExt);
  Img.SaveToFile(filename,writer);
  writer.Free;
end;

{ TFPWriterPPM }

constructor TFPWriterPPM.Create;
begin
  inherited Create;
  ColorDepth := pcdRGB;
end;

{ TFPWriterPGM }

constructor TFPWriterPGM.Create;
begin
  inherited Create;
  ColorDepth := pcdGrayscale;
end;

{ TFPWriterPBM }

constructor TFPWriterPBM.Create;
begin
  inherited Create;
  ColorDepth:= pcdBlackWhite;
end;

{ TFPWriterPNM }

constructor TFPWriterPNM.Create;
begin
  inherited Create;
  ColorDepth := pcdAuto;
  BinaryFormat := True;
end;

procedure TFPWriterPNM.SetFullWidth(AValue: Boolean);
begin
  if FFullWidth=AValue then Exit;
  FFullWidth:=AValue;
  if FFullWidth then
    BinaryFormat:=True;
end;

procedure TFPWriterPNM.InternalWrite(Stream:TStream;Img:TFPCustomImage);
var useBitMapType: integer;

  function SaveHeader(stream:TStream):boolean;
    const
      MagicWords:Array[1..6]OF String[2]=('P1','P2','P3','P4','P5','P6');
    var
      PNMInfo:String;
      strWidth,StrHeight:String[15];
    begin
      SaveHeader:=false;
      with Img do
        begin
          Str(Img.Width,StrWidth);
          Str(Img.Height,StrHeight);
        end;
      PNMInfo:=Concat(MagicWords[useBitMapType],#10,StrWidth,#32,StrHeight,#10);
      if (useBitMapType in [5,6]) and FullWidth then
        PNMInfo:=Concat(PNMInfo,'65535'#10)
      else if (useBitMapType in [2,3,5,6]) then
        PNMInfo:=Concat(PNMInfo,'255'#10);
      stream.seek(0,soFromBeginning);
      stream.Write(PNMInfo[1],Length(PNMInfo));
      SaveHeader := true;
    end;
  var
    Row,Coulumn,nBpLine,i:Integer;
    aColor:TFPColor;
    aLine:PByte;
    dLine : PWord;
    strCol:String[3];
    LinuxEndOfLine: char;
    UseColorDepth: TPNMColorDepth;

  begin
    LinuxEndOfLine := #10;

    //determine color depth
    if ColorDepth = pcdAuto then
      UseColorDepth := GuessColorDepthOfImage(Img) else
      UseColorDepth := ColorDepth;

    //determine file format number (1-6)
    case UseColorDepth of
      pcdBlackWhite: useBitMapType := 1;
      pcdGrayscale: useBitMapType := 2;
      pcdRGB: useBitMapType := 3;
    end;
    if BinaryFormat then inc(useBitMapType,3);
    if FullWidth and Not BinaryFormat then
      Raise FPImageException.Create('Fullwidth can only be used with binary format');
    SaveHeader(Stream);
    case useBitMapType of
      1:nBpLine:=Img.Width*2;{p p p}
      2:nBpLine:=Img.Width*4;{lll lll lll}
      3:nBpLine:=Img.Width*3*4;{rrr ggg bbb rrr ggg bbb}
      4:nBpLine:=(Img.Width+7) SHR 3;
      5:nBpLine:=Img.Width*(1+Ord(FullWidth));
      6:nBpLine:=Img.Width*3*(1+Ord(FullWidth));
    end;

    GetMem(aLine,nBpLine);//3 extra byte for BMP 4Bytes alignement.
    dLine:=PWord(aLine);
    for Row:=0 to img.Height-1 do
      begin
        FillChar(aLine^,nBpLine,0);
        for Coulumn:=0 to img.Width-1 do
          begin
            aColor:=img.Colors[Coulumn,Row];
            with aColor do
              case useBitMapType of
                1:begin
                    if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                    then
                      aLine[2*Coulumn]:=Ord('1')
                    else
                      aLine[2*Coulumn]:=Ord('0');
                    aLine[2*Coulumn+1]:=32;
                  end;
                2:begin
                    Str(Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114))),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*Coulumn+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*Coulumn+i]:=32;
                  end;
                3:begin
                    Str(Hi(Red),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn)+i]:=32;
                    Str(Hi(Green),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+1)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+1)+i]:=32;
                    Str(Hi(Blue),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+2)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+2)+i]:=32;
                  end;
                4:if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                  then
                    aLine[Coulumn shr 3]:=aLine[Coulumn shr 3] or ($80 shr (Coulumn and $07));
                5: if FullWidth then {16 bit per colour}
                     dLine[Coulumn]:=NToBe(Word(Round(Red*0.299+Green*0.587+Blue*0.114))) {write in big-endian format}
                   else {8 bit per colour}
                     aLine[Coulumn]:=Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114)));
                6:if FullWidth then
                  begin {16 bit per colour}
                    dLine[3*Coulumn]:=NToBE(Red); {write in big-endian format}
                    dLine[3*Coulumn+1]:=NToBE(Green);
                    dLine[3*Coulumn+2]:=NToBE(Blue);
                  end
                  else
                  begin {8 bit per colour}
                    aLine[3*Coulumn]:=Hi(Red);
                    aLine[3*Coulumn+1]:=Hi(Green);
                    aLine[3*Coulumn+2]:=Hi(Blue);
                  end;
            end;
          end;
        Stream.Write(aLine^,nBpLine);
        if useBitMapType in[1..3] then Stream.Write(LinuxEndOfLine,1);
      end;
    FreeMem(aLine,nBpLine);
  end;

function TFPWriterPNM.GetColorDepthOfExtension(AExtension: string
  ): TPNMColorDepth;
begin
  if (length(AExtension) > 0) and (AExtension[1]='.') then
    delete(AExtension,1,1);
  AExtension := LowerCase(AExtension);
  if AExtension='pbm' then result := pcdBlackWhite else
  if AExtension='pgm' then result := pcdGrayscale else
  if AExtension='ppm' then result := pcdRGB else
    result := pcdAuto;
end;

function TFPWriterPNM.GuessColorDepthOfImage(Img: TFPCustomImage): TPNMColorDepth;
var Row, Col: integer;
    aColor: TFPColor;
begin
   result := pcdBlackWhite;
   for Row:=0 to img.Height-1 do
     for Col:=0 to img.Width-1 do
     begin
       aColor:=img.Colors[Col,Row];
       if (AColor.red >= 256) and (AColor.green >= 256) and (AColor.blue >= 256) and
          (AColor.red < $FF00) and (AColor.green < $FF00) and (AColor.blue < $FF00) then
       begin
          if (AColor.red shr 8 <> AColor.Green shr 8) or
             (AColor.blue shr 8 <> AColor.Green shr 8) or
             (AColor.red shr 8 <> AColor.blue shr 8) then
          begin
             result := pcdRGB;
             exit;
          end else
            result := pcdGrayscale;
       end;
     end;
end;

function TFPWriterPNM.GetFileExtension(AColorDepth: TPNMColorDepth): string;
begin
  case AColorDepth of
    pcdBlackWhite: result := 'pbm';
    pcdGrayscale: result := 'pgm';
    pcdRGB: result := 'ppm';
  else
    result := 'pnm';
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('Netpbm Portable aNyMap', 'pnm', TFPWriterPNM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable BitMap', 'pbm', TFPWriterPBM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable GrayMap', 'pgm', TFPWriterPGM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable PixelMap', 'ppm', TFPWriterPPM);
end.
