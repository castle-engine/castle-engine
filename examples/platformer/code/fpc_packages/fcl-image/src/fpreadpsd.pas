{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Tiff reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  ToDo: read further images
}
unit FPReadPSD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage;

type
  TRGB = packed record
    Red, Green, Blue : Byte;
  end;

  TLab = record
    L, a, b: byte;
  end;


  TPSDHeader = packed record
    Signature : array[0..3] of Char;   // File IDs '8BPS'
    Version : word;                    // Version number, always 1
    Reserved : array[0..5] of Byte;    // Reserved, must be zeroed
    Channels : Word;                   // Number of color channels (1-24) including alpha channels
    Rows : Cardinal;                   // Height of image in pixels (1-30000)
    Columns : Cardinal;                // Width of image in pixels (1-30000)
    Depth : Word;                      // Number of bits per channel (1, 8, and 16)
    Mode: Word;                        // Color mode
  end;
  {
  Mode Description
  0 Bitmap (monochrome)
  1 Gray-scale
  2 Indexed color (palette color)
  3 RGB color
  4 CMYK color
  7 Multichannel color
  8 Duotone (halftone)
  9 Lab color
  }

  TColorModeDataBlock = packed record
    Types : array[0..3] of Char;   // Always "8BIM"
    ID:word;                       // (See table below)
    Name:byte;                     // Even-length Pascal-format string, 2 bytes or longer
    Size : Cardinal;               // Length of resource data following, in bytes
    Data:byte;                     // Resource data, padded to even length
  end;
  {
  ID Data Format Description
  03e8 WORD[5] Channels, rows, columns, depth, and mode
  03e9 Optional Macintosh print manager information
  03eb Indexed color table
  03ed (See below) Resolution information
       "TResolutionInfo"
  03ee BYTE[] Alpha channel names (Pascal-format strings)
  03ef (See below) Display information for each channel
       "TDisplayInfo"
  03f0 BYTE[] Optional Pascal-format caption string
  03f1 LONG, WORD Fixed-point border width, border units (see below)
  03f2 Background color
  03f3 BYTE[8] Print flags (see below)
  03f4 Gray-scale and halftoning information
  03f5 Color halftoning information
  03f6 Duotone halftoning information
  03f7 Gray-scale and multichannel transfer function
  03f8 Color transfer functions
  03f9 Duotone transfer functions
  03fa Duotone image information
  03fb BYTE[2] Effective black and white value for dot range
  03fc
  03fd EPS options
  03fe WORD, BYTE Quick Mask channel ID, flag for mask initially empty
  03ff
  0400 WORD Index of target layer (0=bottom)
  0401 Working path
  0402 WORD[] Layers group info, group ID for dragging groups
  0403
  0404 IPTC-NAA record
  0405 Image mode for raw-format files
  0406 JPEG quality (Adobe internal)
  07d0
  0bb6 Saved path information
  0bb7 Clipping pathname
  2710 (See below) Print flags information
  }
  TResolutionInfo = record
    hRes:Cardinal;     // Fixed-point number: pixels per inch
    hResUnit:word;     // 1=pixels per inch, 2=pixels per centimeter
    WidthUnit:word;    // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
    vRes:Cardinal;     // Fixed-point number: pixels per inch
    vResUnit:word;     // 1=pixels per inch, 2=pixels per centimeter
    HeightUnit:word;   // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
  end;

  TDisplayInfo = record
    ColorSpace:word;
    Color:array[0..3] of word;
    Opacity:word;          // 0-100
    Kind:byte;             // 0=selected, 1=protected
    Padding:byte;          // Always zero
  end;

  TFPReaderPSD = class;

  TPSDCreateCompatibleImgEvent = procedure(Sender: TFPReaderPSD;
                                        var NewImage: TFPCustomImage) of object;

  { TFPReaderPSD }

  TFPReaderPSD = class(TFPCustomImageReader)
  private
    FCompressed: boolean;
    FOnCreateImage: TPSDCreateCompatibleImgEvent;
  protected
    FHeader        : TPSDHeader;
    FColorDataBlock: TColorModeDataBlock;
    FBytesPerPixel : Byte;
    FScanLine      : PByte;
    FLineSize      : PtrInt;
    FPalette       : TFPPalette;
    FWidth         : integer;
    FHeight        : Integer;
    FBlockCount    : word;
    FChannelCount  : word;
    FLengthOfLine  : array of Word;
    FByteRead      : PtrInt;
    procedure CreateGrayPalette;
    procedure CreateBWPalette;
    function ReadPalette(Stream: TStream): boolean;
    procedure AnalyzeHeader;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream): boolean; virtual;
    procedure WriteScanLine(Img: TFPCustomImage); virtual;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    constructor Create; override;
    property Compressed: Boolean read FCompressed;
    property ThePalette: TFPPalette read FPalette;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property BytesPerPixel: Byte read FBytesPerPixel;
    property BlockCount: word read FBlockCount;
    property ChannelCount: word read FChannelCount;
    property Header: TPSDHeader read FHeader;
    property OnCreateImage: TPSDCreateCompatibleImgEvent read FOnCreateImage write FOnCreateImage;
  end;

implementation

function CorrectCMYK(const C : TFPColor): TFPColor;
var
  MinColor: word;
begin
  if C.red<C.green then MinColor:=C.red
  else MinColor:= C.green;
  if C.blue<MinColor then MinColor:= C.blue;
  if MinColor+ C.alpha>$FFFF then MinColor:=$FFFF-C.alpha;
  Result.red:=C.red-MinColor;
  Result.green:=C.green-MinColor;
  Result.blue:=C.blue-MinColor;
  Result.alpha:=C.alpha+MinColor;
end;

function CMYKtoRGB ( C : TFPColor):  TFPColor;
begin
  C:=CorrectCMYK(C);
  if (C.red + C.Alpha)<$FFFF then
    Result.Red:=$FFFF-(C.red+C.Alpha) else Result.Red:=0;
  if (C.Green+C.Alpha)<$FFFF then
    Result.Green:=$FFFF-(C.Green+C.Alpha) else Result.Green:=0;
  if (C.blue+C.Alpha)<$FFFF then
    Result.blue:=$FFFF-(C.blue+C.Alpha) else Result.blue:=0;
  Result.alpha:=alphaOpaque;
end;

function XYZToRGB(const X, Y, Z :double):TFPColor;
begin
  // ToDo
  Result:=colBlack;
end;

function LabToRGB(const L:TLab):TFPColor;
begin
  // ToDo
  Result:=colBlack;
end;

{ TFPReaderPSD }

procedure TFPReaderPSD.CreateGrayPalette;
Var
  I : Integer;
  c : TFPColor;
Begin
  ThePalette.count := 0;
  For I:=0 To 255 Do
  Begin
    With c do
    begin
      Red:=I*255;
      Green:=I*255;
      Blue:=I*255;
      Alpha:=alphaOpaque;
    end;
    ThePalette.Add (c);
  End;
end;

procedure TFPReaderPSD.CreateBWPalette;
begin
  ThePalette.count := 0;
  ThePalette.Add (colBlack);
  ThePalette.Add (colWhite);
end;

function TFPReaderPSD.ReadPalette(Stream: TStream): boolean;
Var
  I : Integer;
  c : TFPColor;
  OldPos: Integer;
  BufSize:Longint;
  PalBuf: array[0..767] of Byte;
  ContProgress: Boolean;
begin
  Result:=false;
  ThePalette.count := 0;
  OldPos := Stream.Position;
  BufSize:=0;
  Stream.Read(BufSize, SizeOf(BufSize));
  BufSize:=BEtoN(BufSize);
  Stream.Read(PalBuf, BufSize);
  ContProgress:=true;
  Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)),
           False, Rect(0,0,0,0), '', ContProgress);
  if not ContProgress then exit;
  For I:=0 To BufSize div 3 Do
  Begin
    With c do
    begin
      Red:=PalBuf[I] shl 8;
      Green:=PalBuf[I+(BufSize div 3)] shl 8;
      Blue:=PalBuf[I+(BufSize div 3)* 2] shl 8;
      Alpha:=alphaOpaque;
    end;
    ThePalette.Add(C);
  End;
  Stream.Position := OldPos;
  Result:=true;
end;

procedure TFPReaderPSD.AnalyzeHeader;
begin
  With FHeader do
  begin
    Depth:=BEtoN(Depth);
    if (Signature <> '8BPS') then
      Raise Exception.Create('Unknown/Unsupported PSD image type');
    Channels:=BEtoN(Channels);
    if Channels > 4 then
      FBytesPerPixel:=Depth*4
    else
      FBytesPerPixel:=Depth*Channels;
    Mode:=BEtoN(Mode);
    FWidth:=BEtoN(Columns);
    FHeight:=BEtoN(Rows);
    FChannelCount:=Channels;
    FLineSize:=PtrInt(FHeight)*FWidth*Depth div 8;
    FLineSize:=FLineSize*Channels;
    GetMem(FScanLine,FLineSize);
  end;
end;

procedure TFPReaderPSD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H: Integer;
  BufSize:Cardinal;
  Encoding:word;
  ContProgress: Boolean;
begin
  FScanLine:=nil;
  FPalette:=nil;
  try
    Stream.Position:=0;
    ContProgress:=true;
    Progress(FPimage.psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    // read header
    Stream.Read(FHeader, SizeOf(FHeader));
    Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    AnalyzeHeader;
    Case FHeader.Mode of
        0:begin  // Bitmap (monochrome)
            FPalette := TFPPalette.Create(0);
            CreateBWPalette;
          end;
        1, 8:begin // Gray-scale
            FPalette := TFPPalette.Create(0);
            CreateGrayPalette;
          end;
        2:begin // Indexed color (palette color)
            FPalette := TFPPalette.Create(0);
            if not ReadPalette(stream) then exit;
          end;
    end;

    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);
    Img.SetSize(FWidth,FHeight);

    //  color palette
    BufSize:=0;
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  color data block
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Read(FColorDataBlock, SizeOf(FColorDataBlock));
    Stream.Seek(BufSize-SizeOf(FColorDataBlock), soCurrent);
    //  mask
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  compression type
    Encoding:=0;
    Stream.Read(Encoding, SizeOf(Encoding));
    FCompressed:=BEtoN(Encoding) = 1;
    if BEtoN(Encoding)>1 then
      Raise Exception.Create('Unknown compression type');
    If FCompressed then
    begin
      SetLength(FLengthOfLine, FHeight * FChannelCount);
      Stream.ReadBuffer(FLengthOfLine[0], 2 * Length(FLengthOfLine));
      FByteRead:=0;
      Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
      if not ContProgress then exit;
      for H := 0 to High(FLengthOfLine) do
        Inc(FByteRead, BEtoN(FLengthOfLine[H]));
      if not FHeader.Mode in [ 0, 2] then
        FByteRead := FByteRead * FHeader.Depth div 8;
    end else
      FByteRead:= FLineSize;

    ReadScanLine(Stream);
    Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    WriteScanLine(Img);

   {$ifdef FPC_Debug_Image}
    WriteLn('TFPReaderPSD.InternalRead AAA1 ',Stream.position,' ',Stream.size); 
    {$endif}
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TFPReaderPSD.ReadScanLine(Stream: TStream): boolean;
Var
  P : PByte;
  B : Byte;
  I : PtrInt;
  J : integer;
  N : Shortint;
  Count:integer;
  ContProgress: Boolean;
begin
  Result:=false;
  ContProgress:=true;
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
      P:=FScanLine;
      i:=FByteRead;
      repeat
        Count:=0;
        N:=0;
        Stream.ReadBuffer(N,1);
        Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
        if not ContProgress then exit;
        dec(i);
        If N = -128 then
        else
        if N < 0 then
        begin
           Count:=-N+1;
           B:=0;
           Stream.ReadBuffer(B,1);
           dec(i);
           For j := 0 to Count-1 do
           begin
             P[0]:=B;
             inc(p);
           end;
        end
        else
        begin
           Count:=N+1;
           For j := 0 to Count-1 do
           begin
             Stream.ReadBuffer(B,1);
             P[0]:=B;
             inc(p);
             dec(i);
           end;
        end;
      until (i <= 0);
    end;
  Result:=true;
end;

procedure TFPReaderPSD.WriteScanLine(Img: TFPCustomImage);
Var
  Col : Integer;
  C   : TFPColor;
  P, P1, P2, P3   : PByte;
  Z2  : Longint;
  Row : Integer;
  Lab : TLab;
begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Z2:=FHeader.Depth div 8;
  Z2:=Z2 *FHeight*FWidth;
  begin
    case FBytesPerPixel of
      1 : begin
           for Row:=0 to Img.Height-1 do
           begin
             for Col:=0 to Img.Width-1 do
               if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
                 Img.Colors[Col,Row]:=ThePalette[0]
  	       else
                 Img.Colors[Col,Row]:=ThePalette[1];
             inc(P, Img.Width div 8);
           end;
           end;
      8 : begin
           for Row:=0 to Img.Height-1 do
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[P[0]];
               inc(p);
             end;
          end;
      16 : begin
           for Row:=0 to Img.Height-1 do
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[BEtoN(PWord(P)^)];
               inc(p,2);
            end;
          end;
      24 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =9) then
             begin
               Lab.L:=(P[0]);
               Lab.a:=(P1[0]);
               Lab.b:=(P2[0]);
               C:=LabToRGB(Lab);
             end
             else
              With C do
              begin
                Red:=P[0] or (P[0] shl 8);
                green:=P1[0] or (P1[0] shl 8);
                blue:=P2[0] or (P2[0] shl 8);
                alpha:=alphaOpaque;
              end;
              Inc(P);
              Inc(P1);
              Inc(P2);
//              if (Header.Mode =9) then  C:=XYZtoRGB(C); // Lab color
              Img[col, row] := C;
           end;
          end;
      32 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           P3:=P;
           inc(P3,Z2*3);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =4) then
             begin
                 P^ := 255 - P^;
                 P1^ := 255 - P1^;
                 P2^ := 255 - P2^;
                 P3^ := 255 - P3^;
             end;
             C.Red:=P[0] or (P[0] shl 8);
             C.green:=P1[0] or (P1[0] shl 8);
             C.blue:=P2[0] or (P2[0] shl 8);
             C.alpha:=P3[0] or (P3[0] shl 8);
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P);
             Inc(P1);
             Inc(P2);
             Inc(P3);
           end;
          end;
      48 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           C.alpha:=alphaOpaque;
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
              With C do
              begin
                Red:=BEtoN(PWord(P)^);
                green:=BEtoN(PWord(P1)^);
                blue:=BEtoN(PWord(P2)^);
              end;
              Inc(P,2);
              Inc(P1,2);
              Inc(P2,2);
              Img[col, row] := C;
           end;
          end;
      64 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           P3:=P;
           inc(P3,Z2*3);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             C.Red:=BEtoN(PWord(P)^);
             C.green:=BEtoN(PWord(P1)^);
             C.blue:=BEtoN(PWord(P2)^);
             C.alpha:=BEtoN(PWord(P3)^);
             if (FHeader.Mode =4) then
             begin
                 C.red:=$ffff-C.red;
                 C.green:=$ffff-C.green;
                 C.blue:=$ffff-C.blue;
                 C.alpha:=$ffff-C.alpha;
             end;
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P,2);
             Inc(P1,2);
             Inc(P2,2);
             Inc(P3,2);
           end;
          end;
    end;
  end;
end;

function TFPReaderPSD.InternalCheck(Stream: TStream): boolean;
var
  OldPos: Int64;
  n: Integer;
  
begin
  Result:=False;
  if Stream=Nil then 
    exit;
  OldPos := Stream.Position;
  try
    n := SizeOf(FHeader);
    Result:=(Stream.Read(FHeader, n) = n) 
            and (FHeader.Signature = '8BPS')
  finally
    Stream.Position := OldPos;
  end;
end;

constructor TFPReaderPSD.Create;
begin
  inherited Create;
end;

initialization
  ImageHandlers.RegisterImageReader ('PSD Format', 'PSD', TFPReaderPSD);
  ImageHandlers.RegisterImageReader ('PDD Format', 'PDD', TFPReaderPSD);

end.

