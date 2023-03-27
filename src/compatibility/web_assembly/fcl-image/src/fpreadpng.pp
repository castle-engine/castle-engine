{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPReadPNG;

interface

uses
  SysUtils,Classes, FPImage, FPImgCmn, PNGComn, ZStream;

Type

  TSetPixelProc = procedure (x,y:integer; CD : TColordata) of object;
  TConvertColorProc = function (CD:TColorData) : TFPColor of object;

  { TFPReaderPNG }

  TFPReaderPNG = class (TFPCustomImageReader)
    private

      FHeader : THeaderChunk;
      ZData : TMemoryStream;  // holds compressed data until all blocks are read
      Decompress : TDeCompressionStream; // decompresses the data
      FPltte : boolean;     // if palette is used
      FCountScanlines : EightLong; //Number of scanlines to process for each pass
      FScanLineLength : EightLong; //Length of scanline for each pass
      FCurrentPass : byte;
      ByteWidth : byte;          // number of bytes to read for pixel information
      BitsUsed : EightLong; // bitmasks to use to split a byte into smaller parts
      BitShift : byte;  // shift right to do of the bits extracted with BitsUsed for 1 element
      CountBitsUsed : byte;  // number of bit groups (1 pixel) per byte (when bytewidth = 1)
      //CFmt : TColorFormat; // format of the colors to convert from
      StartX,StartY, DeltaX,DeltaY, StartPass,EndPass : integer;  // number and format of passes
      FSwitchLine, FCurrentLine, FPreviousLine : pByteArray;
      FPalette : TFPPalette;
      FSetPixel : TSetPixelProc;
      FConvertColor : TConvertColorProc;
      function GetGrayScale: Boolean;
      function GetHeaderByte(AIndex: Integer): Byte;
      function GetIndexed: Boolean;
      function GetUseAlpha: Boolean;
      function GetWordSized: Boolean;
      procedure ReadChunk;
      procedure HandleData;
      procedure HandleUnknown;
      function ColorGray1 (CD:TColorData) : TFPColor;
      function ColorGray2 (CD:TColorData) : TFPColor;
      function ColorGray4 (CD:TColorData) : TFPColor;
      function ColorGray8 (CD:TColorData) : TFPColor;
      function ColorGray16 (CD:TColorData) : TFPColor;
      function ColorGrayAlpha8 (CD:TColorData) : TFPColor;
      function ColorGrayAlpha16 (CD:TColorData) : TFPColor;
      function ColorColor8 (CD:TColorData) : TFPColor;
      function ColorColor16 (CD:TColorData) : TFPColor;
      function ColorColorAlpha8 (CD:TColorData) : TFPColor;
      function ColorColorAlpha16 (CD:TColorData) : TFPColor;
    protected
      Chunk : TChunk;
      UseTransparent, EndOfFile : boolean;
      TransparentDataValue : TColorData;
      UsingBitGroup : byte;
      DataIndex : longword;
      DataBytes : TColorData;
      function CurrentLine(x:longword) : byte;
      function PrevSample (x:longword): byte;
      function PreviousLine (x:longword) : byte;
      function PrevLinePrevSample (x:longword): byte;
      procedure HandleChunk; virtual;
      procedure HandlePalette; virtual;
      procedure HandleAlpha; virtual;
      function CalcX (relX:integer) : integer;
      function CalcY (relY:integer) : integer;
      function CalcColor: TColorData;
      procedure HandleScanLine (const y : integer; const ScanLine : PByteArray); virtual;
      procedure DoDecompress; virtual;
      function  DoFilter(LineFilter:byte;index:longword; b:byte) : byte; virtual;
      procedure SetPalettePixel (x,y:integer; CD : TColordata);
      procedure SetPalColPixel (x,y:integer; CD : TColordata);
      procedure SetColorPixel (x,y:integer; CD : TColordata);
      procedure SetColorTrPixel (x,y:integer; CD : TColordata);
      function DecideSetPixel : TSetPixelProc; virtual;
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
      class function InternalSize(Str:TStream): TPoint; override;
      //property ColorFormat : TColorformat read CFmt;
      property ConvertColor : TConvertColorProc read FConvertColor;
      property CurrentPass : byte read FCurrentPass;
      property Pltte : boolean read FPltte;
      property ThePalette : TFPPalette read FPalette;
      property Header : THeaderChunk read FHeader;
      property CountScanlines : EightLong read FCountScanlines;
      property ScanLineLength : EightLong read FScanLineLength;
    public
      constructor create; override;
      destructor destroy; override;
      // These 2 match writer properties. Calculated from header values
      Property GrayScale : Boolean Read GetGrayScale;
      Property WordSized : Boolean Read GetWordSized;
      Property Indexed : Boolean Read GetIndexed;
      Property UseAlpha : Boolean Read GetUseAlpha;
      // Raw reader values
      Property BitDepth : Byte Index 0 Read GetHeaderByte;
      Property ColorType : Byte Index 1 Read GetHeaderByte;
      Property Compression : Byte Index 2 Read GetHeaderByte;
      Property Filter : Byte Index 3 Read GetHeaderByte;
      Property Interlace : Byte Index 4 Read GetHeaderByte;
  end;

implementation



const StartPoints : array[0..7, 0..1] of word =
         ((0,0),(0,0),(4,0),(0,4),(2,0),(0,2),(1,0),(0,1));
      Delta : array[0..7,0..1] of word =
         ((1,1),(8,8),(8,8),(4,8),(4,4),(2,4),(2,2),(1,2));
      BitsUsed1Depth : EightLong = ($80,$40,$20,$10,$08,$04,$02,$01);
      BitsUsed2Depth : EightLong = ($C0,$30,$0C,$03,0,0,0,0);
      BitsUsed4Depth : EightLong = ($F0,$0F,0,0,0,0,0,0);

constructor TFPReaderPNG.create;
begin
  inherited;
  chunk.acapacity := 0;
  chunk.data := nil;
  UseTransparent := False;
end;

destructor TFPReaderPNG.destroy;
begin
  with chunk do
    if acapacity > 0 then
      freemem (data);
  inherited;
end;

procedure TFPReaderPNG.ReadChunk;

var ChunkHeader : TChunkHeader;
    readCRC : longword;
    l : longword;
begin
  TheStream.Read (ChunkHeader,sizeof(ChunkHeader));
  with chunk do
    begin
    // chunk header
    with ChunkHeader do
      begin
      {$IFDEF ENDIAN_LITTLE}
      alength := swap(CLength);
      {$ELSE}
      alength := CLength;
      {$ENDIF}
      ReadType := CType;
      end;
    aType := low(TChunkTypes);
    while (aType < high(TChunkTypes)) and (ChunkTypes[aType] <> ReadType) do
      inc (aType);
    if alength > MaxChunkLength then
      raise PNGImageException.Create ('Invalid chunklength');
    if alength > acapacity then
      begin
      if acapacity > 0 then
        freemem (data);
      GetMem (data, alength);
      acapacity := alength;
      end;
    l := TheStream.read (data^, alength);
    if l <> alength then
      raise PNGImageException.Create ('Chunk length exceeds stream length');
    TheStream.Read (readCRC, sizeof(ReadCRC));
    l := CalculateCRC (All1Bits, ReadType, sizeOf(ReadType));
    l := CalculateCRC (l, data^, alength);
    {$IFDEF ENDIAN_LITTLE}
    l := swap(l xor All1Bits);
    {$ELSE}
    l := l xor All1Bits;
    {$ENDIF}
    if ReadCRC <> l then
      raise PNGImageException.Create ('CRC check failed');
    end;
end;

function TFPReaderPNG.GetHeaderByte(AIndex: Integer): Byte;
begin
  With FHeader do
  Case aIndex of
     0 : Result:=BitDepth;
     1 : Result:=ColorType;
     2 : Result:=Compression;
     3 : Result:=Filter;
     4 : Result:=Interlace;
  else
    Result:=0;
  end;
end;

function TFPReaderPNG.GetIndexed: Boolean;
begin
  Result:=ColorType=3;
end;

function TFPReaderPNG.GetUseAlpha: Boolean;
begin
  Result:=ColorType in [4,6]; // Can also be in 3, but that would require scanning the palette
end;

function TFPReaderPNG.GetWordSized: Boolean;
begin
  Result:=BitDepth=16;
end;

function TFPReaderPNG.GetGrayScale: Boolean;
begin
  Result:=ColorType in [0,4];
end;

procedure TFPReaderPNG.HandleData;
var OldSize : longword;
begin
  OldSize := ZData.size;
  ZData.Size := OldSize + Chunk.aLength;
  ZData.Write (chunk.Data^, chunk.aLength);
end;

procedure TFPReaderPNG.HandleAlpha;
  procedure PaletteAlpha;
    var r : integer;
        a : word;
        c : TFPColor;
    begin
      with chunk do
        begin
        if alength > longword(ThePalette.count) then
          raise PNGImageException.create ('To much alpha values for palette');
        for r := 0 to alength-1 do
          begin
          c := ThePalette[r];
          a := data^[r];
          c.alpha := (a shl 8) + a;
          ThePalette[r] := c;
          end;
        end;
    end;
  procedure TransparentGray;
    var a : word;
    begin
      move (chunk.data^[0], a, 2);
      {$IFDEF ENDIAN_LITTLE}
      a := swap (a);
      {$ENDIF}
      TransparentDataValue := a;
      UseTransparent := True;
    end;
  procedure TransparentColor;
    var d : byte;
        r,g,b : word;
        a : TColorData;
    begin
      with chunk do
        begin
        move (data^[0], r, 2);
        move (data^[2], g, 2);
        move (data^[4], b, 2);
        end;
      {$IFDEF ENDIAN_LITTLE}
      r := swap (r);
      g := swap (g);
      b := swap (b);
      {$ENDIF}
      d := header.bitdepth;
      a := (TColorData(b) shl d) shl d;
      a := a + (TColorData(g) shl d) + r;
      TransparentDataValue := a;
      UseTransparent := True;
    end;
begin
  case header.ColorType of
    3 : PaletteAlpha;
    0 : TransparentGray;
    2 : TransparentColor;
  end;
end;

procedure TFPReaderPNG.HandlePalette;
var r : longword;
    c : TFPColor;
    t : word;
begin
  if header.colortype = 3 then
    with chunk do
      begin
      if TheImage.UsePalette then
        FPalette := TheImage.Palette
      else
        FPalette := TFPPalette.Create(0);
      c.Alpha := AlphaOpaque;
      if (aLength mod 3) > 0 then
        raise PNGImageException.Create ('Impossible length for PLTE-chunk');
      r := 0;
      ThePalette.count := 0;
      while r < alength do
        begin
        t := data^[r];
        c.red := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.green := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.blue := t + (t shl 8);
        inc (r);
        ThePalette.Add (c);
        end;
      end;
end;

procedure TFPReaderPNG.SetPalettePixel (x,y:integer; CD : TColordata);
begin  // both PNG and palette have palette
  TheImage.Pixels[x,y] := CD;
end;

procedure TFPReaderPNG.SetPalColPixel (x,y:integer; CD : TColordata);
begin  // PNG with palette, Img without
  TheImage.Colors[x,y] := ThePalette[CD];
end;

procedure TFPReaderPNG.SetColorPixel (x,y:integer; CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and no transparency colordata
  // c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  TheImage.Colors[x,y] := c;
end;

procedure TFPReaderPNG.SetColorTrPixel (x,y:integer; CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and there is a transparency colordata
  //c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  if TransparentDataValue = CD then
    c.alpha := alphaTransparent;
  TheImage.Colors[x,y] := c;
end;

function TFPReaderPNG.CurrentLine(x:longword):byte;
begin
  result := FCurrentLine^[x];
end;

function TFPReaderPNG.PrevSample (x:longword): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FCurrentLine^[x - bytewidth];
end;

function TFPReaderPNG.PreviousLine (x:longword) : byte;
begin
  result := FPreviousline^[x];
end;

function TFPReaderPNG.PrevLinePrevSample (x:longword): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FPreviousLine^[x - bytewidth];
end;

function TFPReaderPNG.DoFilter(LineFilter:byte;index:longword; b:byte) : byte;
var diff : byte;
  procedure FilterSub;
  begin
    diff := PrevSample(index);
  end;
  procedure FilterUp;
  begin
    diff := PreviousLine(index);
  end;
  procedure FilterAverage;
  var l, p : word;
  begin
    l := PrevSample(index);
    p := PreviousLine(index);
    diff := (l + p) div 2;
  end;
  procedure FilterPaeth;
  var dl, dp, dlp : word; // index for previous and distances for:
      l, p, lp : byte;  // r:predictor, Left, Previous, LeftPrevious
      r : integer;
  begin
    l := PrevSample(index);
    lp := PrevLinePrevSample(index);
    p := PreviousLine(index);
    r := l + p - lp;
    dl := abs (r - l);
    dlp := abs (r - lp);
    dp := abs (r - p);
    if (dl <= dp) and (dl <= dlp) then
      diff := l
    else if dp <= dlp then
      diff := p
    else
      diff := lp;
  end;
begin
  case LineFilter of
    0 : diff := 0;
    1 : FilterSub;
    2 : FilterUp;
    3 : FilterAverage;
    4 : FilterPaeth;
  end;
  result := (b + diff) mod $100;
end;

function TFPReaderPNG.DecideSetPixel : TSetPixelProc;
begin
  if Pltte then
    if TheImage.UsePalette then
      result := @SetPalettePixel
    else
      result := @SetPalColPixel
  else
    if UseTransparent then
      result := @SetColorTrPixel
    else
      result := @SetColorPixel;
end;

function TFPReaderPNG.CalcX (relX:integer) : integer;
begin
  result := StartX + (relX * deltaX);
end;

function TFPReaderPNG.CalcY (relY:integer) : integer;
begin
  result := StartY + (relY * deltaY);
end;

function TFPReaderPNG.CalcColor: TColorData;
var cd : longword;
    r : word;
    b : pbyte;
begin
  if UsingBitGroup = 0 then
    begin
    Databytes := 0;
    if Header.BitDepth = 16 then
      begin
        b := @Databytes;
        b^ := 0;
        r := 0;
        while (r < ByteWidth-1) do
        begin
          b^ := FCurrentLine^[DataIndex+r+1];
          inc (b);
          b^ := FCurrentLine^[DataIndex+r];
          inc (b);
          inc (r,2);
        end;
      end
    else move (FCurrentLine^[DataIndex], Databytes, bytewidth);
    {$IFDEF ENDIAN_BIG}
    Databytes:=swap(Databytes);
    {$ENDIF}
    inc (DataIndex,bytewidth);
    end;
  if bytewidth = 1 then
    begin
    cd := (Databytes and BitsUsed[UsingBitGroup]);
    result := cd shr ((CountBitsUsed-UsingBitGroup-1) * BitShift);
    inc (UsingBitgroup);
    if UsingBitGroup >= CountBitsUsed then
      UsingBitGroup := 0;
    end
  else
    result := Databytes;
end;

procedure TFPReaderPNG.HandleScanLine (const y : integer; const ScanLine : PByteArray);
var x, rx : integer;
    c : TColorData;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    X := CalcX(rx);
    c := CalcColor;
    FSetPixel (x,y,c);
    end
end;

function TFPReaderPNG.ColorGray1 (CD:TColorDAta) : TFPColor;
begin
  if CD = 0 then
    result := colBlack
  else
    result := colWhite;
end;

function TFPReaderPNG.ColorGray2 (CD:TColorDAta) : TFPColor;
var c : word;
begin
  c := CD and 3;
  c := c + (c shl 2);
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorGray4 (CD:TColorDAta) : TFPColor;
var c : word;
begin
  c := CD and $F;
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorGray8 (CD:TColorDAta) : TFPColor;
var c : word;
begin
  c := CD and $FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorGray16 (CD:TColorDAta) : TFPColor;
var c : word;
begin
  c := CD and $FFFF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorGrayAlpha8 (CD:TColorData) : TFPColor;
var c : word;
begin
  c := CD and $00FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    c := CD and $FF00;
    alpha := c + (c shr 8);
    end;
end;

function TFPReaderPNG.ColorGrayAlpha16 (CD:TColorData) : TFPColor;
var c : word;
begin
  {$ifdef FPC_LITTLE_ENDIAN}
  c := CD and $FFFF;
  {$else}
  c := (CD shr 16) and $FFFF;
  {$endif}
  with result do
    begin
    red := c;
    green := c;
    blue := c;
  {$ifdef FPC_LITTLE_ENDIAN}
    alpha := (CD shr 16) and $FFFF;
  {$else}
    alpha := CD and $FFFF;
  {$endif}
    end;
end;

function TFPReaderPNG.ColorColor8 (CD:TColorData) : TFPColor;
var c : word;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    CD:=CD shr 8;
    c := CD and $FF;
    green := c + (c shl 8);
    CD:=CD shr 8;
    c := CD and $FF;
    blue := c + (c shl 8);
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorColor16 (CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    CD:=CD shr 16;
    green := CD and $FFFF;
    CD:=CD shr 16;
    blue := CD and $FFFF;
    alpha := alphaOpaque;
    end;
end;

function TFPReaderPNG.ColorColorAlpha8 (CD:TColorData) : TFPColor;
var c : word;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    CD:=CD shr 8;
    c := CD and $FF;
    green := c + (c shl 8);
    CD:=CD shr 8;
    c := CD and $FF;
    blue := c + (c shl 8);
    CD:=CD shr 8;
    c := CD and $FF;
    alpha := c + (c shl 8);
    end;
end;

function TFPReaderPNG.ColorColorAlpha16 (CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    CD:=CD shr 16;
    green := CD and $FFFF;
    CD:=CD shr 16;
    blue := CD and $FFFF;
    CD:=CD shr 16;
    alpha := CD and $FFFF;
    end;
end;

procedure TFPReaderPNG.DoDecompress;

  procedure initVars;
  var r,d : integer;
  begin
    with Header do
      begin
      if interlace=0 then
        begin
        StartPass := 0;
        EndPass := 0;
        FCountScanlines[0] := Height;
        FScanLineLength[0] := Width;
        end
      else
        begin
        StartPass := 1;
        EndPass := 7;
        for r := 1 to 7 do
          begin
          d := Height div delta[r,1];
          if (height mod delta[r,1]) > startpoints[r,1] then
            inc (d);
          FCountScanlines[r] := d;
          d := width div delta[r,0];
          if (width mod delta[r,0]) > startpoints[r,0] then
            inc (d);
          FScanLineLength[r] := d;
          end;
        end;
      Fpltte := (ColorType = 3);
      case colortype of
        0 : case Bitdepth of
              1  : begin
                   FConvertColor := @ColorGray1; //CFmt := cfMono;
                   ByteWidth := 1;
                   end;
              2  : begin
                   FConvertColor := @ColorGray2; //CFmt := cfGray2;
                   ByteWidth := 1;
                   end;
              4  : begin
                   FConvertColor := @ColorGray4; //CFmt := cfGray4;
                   ByteWidth := 1;
                   end;
              8  : begin
                   FConvertColor := @ColorGray8; //CFmt := cfGray8;
                   ByteWidth := 1;
                   end;
              16 : begin
                   FConvertColor := @ColorGray16; //CFmt := cfGray16;
                   ByteWidth := 2;
                   end;
            end;
        2 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColor8; //CFmt := cfBGR24
              ByteWidth := 3;
              end
            else
              begin
              FConvertColor := @ColorColor16; //CFmt := cfBGR48;
              ByteWidth := 6;
              end;
        3 : if BitDepth = 16 then
              ByteWidth := 2
            else
              ByteWidth := 1;
        4 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorGrayAlpha8; //CFmt := cfGrayA16
              ByteWidth := 2;
              end
            else
              begin
              FConvertColor := @ColorGrayAlpha16; //CFmt := cfGrayA32;
              ByteWidth := 4;
              end;
        6 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColorAlpha8; //CFmt := cfABGR32
              ByteWidth := 4;
              end
            else
              begin
              FConvertColor := @ColorColorAlpha16; //CFmt := cfABGR64;
              ByteWidth := 8;
              end;
      end;
      //ByteWidth := BytesNeeded[CFmt];
      case BitDepth of
        1 : begin
            CountBitsUsed := 8;
            BitShift := 1;
            BitsUsed := BitsUsed1Depth;
            end;
        2 : begin
            CountBitsUsed := 4;
            BitShift := 2;
            BitsUsed := BitsUsed2Depth;
            end;
        4 : begin
            CountBitsUsed := 2;
            BitShift := 4;
            BitsUsed := BitsUsed4Depth;
            end;
        8 : begin
            CountBitsUsed := 1;
            BitShift := 0;
            BitsUsed[0] := $FF;
            end;
        end;
      end;
  end;

  procedure Decode;
  var y, rp, ry, rx, l : integer;
      lf : byte;
  begin
    FSetPixel := DecideSetPixel;
    for rp := StartPass to EndPass do
      begin
      FCurrentPass := rp;
      StartX := StartPoints[rp,0];
      StartY := StartPoints[rp,1];
      DeltaX := Delta[rp,0];
      DeltaY := Delta[rp,1];
      if bytewidth = 1 then
        begin
        l := (ScanLineLength[rp] div CountBitsUsed);
        if (ScanLineLength[rp] mod CountBitsUsed) > 0 then
          inc (l);
        end
      else
        l := ScanLineLength[rp]*ByteWidth;
      if (l>0) then
        begin
        GetMem (FPreviousLine, l);
        GetMem (FCurrentLine, l);
        fillchar (FCurrentLine^,l,0);
        try
          for ry := 0 to CountScanlines[rp]-1 do
            begin
            FSwitchLine := FCurrentLine;
            FCurrentLine := FPreviousLine;
            FPreviousLine := FSwitchLine;
            Y := CalcY(ry);
            Decompress.Read (lf, sizeof(lf));
            Decompress.Read (FCurrentLine^, l);
            if lf <> 0 then  // Do nothing when there is no filter used
              for rx := 0 to l-1 do
                FCurrentLine^[rx] := DoFilter (lf, rx, FCurrentLine^[rx]);
            HandleScanLine (y, FCurrentLine);
            end;
        finally
          freemem (FPreviousLine);
          freemem (FCurrentLine);
        end;
        end;
      end;
  end;

begin
  InitVars;
  DeCode;
end;

procedure TFPReaderPNG.HandleChunk;
begin
  case chunk.AType of
    ctIHDR : raise PNGImageException.Create ('Second IHDR chunk found');
    ctPLTE : HandlePalette;
    ctIDAT : HandleData;
    ctIEND : EndOfFile := True;
    cttRNS : HandleAlpha;
    else HandleUnknown;
  end;
end;

procedure TFPReaderPNG.HandleUnknown;
begin
  if (chunk.readtype[0] in ['A'..'Z']) then
    raise PNGImageException.Create('Critical chunk '+chunk.readtype+' not recognized');
end;

// NOTE: It is assumed that signature and IDHDR chunk already have been read.
procedure TFPReaderPNG.InternalRead (Str:TStream; Img:TFPCustomImage);
begin
  {$ifdef FPC_Debug_Image}
  if Str<>TheStream then
    writeln('WARNING: TFPReaderPNG.InternalRead Str<>TheStream');  
  {$endif}
  with Header do
    Img.SetSize (Width, Height);
  ZData := TMemoryStream.Create;
  try
    EndOfFile := false;
    while not EndOfFile do
      begin
      ReadChunk;
      HandleChunk;
      end;
    ZData.position:=0;
    Decompress := TDecompressionStream.Create (ZData);
    try
      DoDecompress;
    finally
      Decompress.Free;
    end;
  finally
    ZData.Free;
    if not img.UsePalette and assigned(FPalette) then
      begin
      FreeAndNil(FPalette);
      end;
  end;
end;

class function TFPReaderPNG.InternalSize(Str: TStream): TPoint;
var
  SigCheck: array[0..7] of byte;
  r: Integer;
  Width, Height: Word;
  StartPos: Int64;
begin
  Result.X := 0;
  Result.Y := 0;

  StartPos := Str.Position;
  // Check Signature
  Str.Read(SigCheck, SizeOf(SigCheck));
  for r := Low(SigCheck) to High(SigCheck) do
  begin
    If SigCheck[r] <> Signature[r] then
      Exit;
  end;
  if not(
        (Str.Seek(10, soFromCurrent)=StartPos+18)
    and (Str.Read(Width, 2)=2)
    and (Str.Seek(2, soFromCurrent)=StartPos+22)
    and (Str.Read(Height, 2)=2))
  then
    Exit;

  {$IFDEF ENDIAN_LITTLE}
  Width := Swap(Width);
  Height := Swap(Height);
  {$ENDIF}

  Result.X := Width;
  Result.Y := Height;
end;

// NOTE: Stream does not rewind here!
function  TFPReaderPNG.InternalCheck (Str:TStream) : boolean;
var SigCheck : array[0..7] of byte;
    r : integer;
begin
  Result:=False;
  if Str=Nil then 
    exit;
  // Check Signature
  if Str.Read(SigCheck, SizeOf(SigCheck)) <> SizeOf(SigCheck) then
    Exit;
  for r := 0 to 7 do
    begin
    If SigCheck[r] <> Signature[r] then
      Exit;
    end;
  // Check IHDR
  ReadChunk;
  move (chunk.data^, FHeader, sizeof(Header));
  with header do
    begin
    {$IFDEF ENDIAN_LITTLE}
    Width := swap(width);
    height := swap (height);
    {$ENDIF}
    result :=(width > 0) and (height > 0) and (compression = 0)
              and (filter = 0) and (Interlace in [0,1]);
    end;
end;

initialization
  ImageHandlers.RegisterImageReader ('Portable Network Graphics', 'png', TFPReaderPNG);
end.

