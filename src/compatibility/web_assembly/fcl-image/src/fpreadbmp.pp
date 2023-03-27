{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps
}

{$mode objfpc}
{$h+}

unit FPReadBMP;

interface

uses FPImage, classes, sysutils, BMPcomn;

type
  TFPReaderBMP = class (TFPCustomImageReader)
    Private
      DeltaX, DeltaY : integer; // Used for the never-used delta option in RLE
      TopDown : boolean;        // If set, bitmap is stored top down instead of bottom up
      continue : boolean;       // needed for onprogress event
      percent : byte;
      percentinterval : longword;
      percentacc : longword;
      Rect : TRect;
      Procedure FreeBufs;       // Free (and nil) buffers.
    protected
      ReadSize : Integer;       // Size (in bytes) of 1 scanline.
      BFI : TBitMapInfoHeader;  // The header as read from the stream.
      FPalette : PFPcolor;      // Buffer with Palette entries. (useless now)
      LineBuf : PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
      RedMask, GreenMask, BlueMask : longword; //Used if Compression=bi_bitfields
      RedShift, GreenShift, BlueShift : shortint;
      // SetupRead will allocate the needed buffers, and read the colormap if needed.
      procedure SetupRead(nPalette, nRowBits: Integer; Stream : TStream); virtual;
      function CountBits(Value : byte) : shortint;
      function ShiftCount(Mask : longword) : shortint;
      function ExpandColor(value : longword) : TFPColor;
      procedure ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
      procedure ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
      procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
      property XPelsPerMeter : integer read BFI.XPelsPerMeter;
      property YPelsPerMeter : integer read BFI.YPelsPerMeter;
  end;

implementation


function RGBAToFPColor(Const RGBA: TColorRGBA) : TFPcolor;

begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=255-A;
    Alpha :=(Alpha shl 8) or Alpha
    end;
end;

Function RGBToFPColor(Const RGB : TColorRGB) : TFPColor;

begin
  with Result,RGB do
    begin  {Use only the high byte to convert the color}
    Red   := (R shl 8) + R;
    Green := (G shl 8) + G;
    Blue  := (B shl 8) + B;
    Alpha := AlphaOpaque;
    end;
end;

Constructor TFPReaderBMP.create;

begin
  inherited create;
end;

Destructor TFPReaderBMP.Destroy;

begin
  FreeBufs;
  inherited destroy;
end;

Procedure TFPReaderBMP.FreeBufs;

begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FPalette:=Nil;
    end;
end;

{ Counts how many bits are set }
function TFPReaderBMP.CountBits(Value : byte) : shortint;
begin
   Result:=PopCnt(Value);
end;

{ If compression is bi_bitfields, there could be arbitrary masks for colors.
  Although this is not compatible with windows9x it's better to know how to read these bitmaps
  We must determine how to switch the value once masked
  Example: 0000 0111 1110 0000, if we shr 5 we have 00XX XXXX for the color, but these bits must be the
  highest in the color, so we must shr (5-(8-6))=3, and we have XXXX XX00.
  A negative value means "shift left"  }
function TFPReaderBMP.ShiftCount(Mask : longword) : shortint;
var tmp : shortint;
begin
  tmp:=0;
  if Mask=0 then
  begin
    Result:=0;
    exit;
  end;

  while (Mask mod 2)=0 do { rightmost bit is 0 }
  begin
    inc(tmp);
    Mask:= Mask shr 1;
  end;
  tmp:=tmp-(8-popcnt(byte(Mask and $FF)));
  Result:=tmp;
end;

function TFPReaderBMP.ExpandColor(value : longword) : TFPColor;
var tmpr, tmpg, tmpb : longword;
    col : TColorRGB;
begin
  {$IFDEF ENDIAN_BIG}
  value:=swap(value);
  {$ENDIF}
  tmpr:=value and RedMask;
  tmpg:=value and GreenMask;
  tmpb:=value and BlueMask;
  if RedShift < 0 then col.R:=byte(tmpr shl (-RedShift))
  else col.R:=byte(tmpr shr RedShift);
  if GreenShift < 0 then col.G:=byte(tmpg shl (-GreenShift))
  else col.G:=byte(tmpg shr GreenShift);
  if BlueShift < 0 then col.B:=byte(tmpb shl (-BlueShift))
  else col.B:=byte(tmpb shr BlueShift);
  Result:=RGBToFPColor(col);
end;

procedure TFPReaderBMP.SetupRead(nPalette, nRowBits: Integer; Stream : TStream);

var
  ColInfo: ARRAY OF TColorRGBA;
  i: Integer;

begin
  if ((BFI.Compression=BI_RGB) and (BFI.BitCount=16)) then { 5 bits per channel, fixed mask }
  begin
    RedMask:=$7C00; RedShift:=7;
    GreenMask:=$03E0; GreenShift:=2;
    BlueMask:=$001F; BlueShift:=-3;
  end
  else if ((BFI.Compression=BI_BITFIELDS) and (BFI.BitCount in [16,32])) then { arbitrary mask }
  begin
    Stream.Read(RedMask,4);
    Stream.Read(GreenMask,4);
    Stream.Read(BlueMask,4);
    {$IFDEF ENDIAN_BIG}
    RedMask:=swap(RedMask);
    GreenMask:=swap(GreenMask);
    BlueMask:=swap(BlueMask);
    {$ENDIF}
    RedShift:=ShiftCount(RedMask);
    GreenShift:=ShiftCount(GreenMask);
    BlueShift:=ShiftCount(BlueMask);
  end
  else if nPalette>0 then
    begin
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    SetLength(ColInfo, nPalette);
    if BFI.ClrUsed>0 then
      Stream.Read(ColInfo[0],BFI.ClrUsed*SizeOf(TColorRGBA))
    else // Seems to me that this is dangerous.
      Stream.Read(ColInfo[0],nPalette*SizeOf(TColorRGBA));
    for i := 0 to High(ColInfo) do
      FPalette[i] := RGBAToFPColor(ColInfo[i]);
    end
  else if BFI.ClrUsed>0 then { Skip palette }
    Stream.Position := Stream.Position + BFI.ClrUsed*SizeOf(TColorRGBA);
  ReadSize:=((nRowBits + 31) div 32) shl 2;
  GetMem(LineBuf,ReadSize);
end;

procedure TFPReaderBMP.InternalRead(Stream:TStream; Img:TFPCustomImage);
// NOTE: Assumes that BMP header already has been read
Var
  Row, i, pallen : Integer;
  BadCompression : boolean;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;
  Stream.Read(BFI,SizeOf(BFI));
  {$IFDEF ENDIAN_BIG}
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  { This will move past any junk after the BFI header }
  Stream.Position:=Stream.Position-SizeOf(BFI)+BFI.Size;
  with BFI do
  begin
    BadCompression:=false;
    if ((Compression=BI_RLE4) and (BitCount<>4)) then BadCompression:=true;
    if ((Compression=BI_RLE8) and (BitCount<>8)) then BadCompression:=true;
    if ((Compression=BI_BITFIELDS) and (not (BitCount in [16,32]))) then BadCompression:=true;
    if not (Compression in [BI_RGB..BI_BITFIELDS]) then BadCompression:=true;
    if BadCompression then
      raise FPImageException.Create('Bad BMP compression mode');
    TopDown:=(Height<0);
    Height:=abs(Height);
    if (TopDown and (not (Compression in [BI_RGB,BI_BITFIELDS]))) then
      raise FPImageException.Create('Top-down bitmaps cannot be compressed');
    Img.SetSize(0,0);
    if BitCount<=8 then
    begin
      Img.UsePalette:=true;
      Img.Palette.Clear;
    end
    else Img.UsePalette:=false;
    Case BFI.BitCount of
      1 : { Monochrome }
        SetupRead(2,Width,Stream);
      4 :
        SetupRead(16,Width*4,Stream);
      8 :
        SetupRead(256,Width*8,Stream);
      16 :
        SetupRead(0,Width*8*2,Stream);
      24:
        SetupRead(0,Width*8*3,Stream);
      32:
        SetupRead(0,Width*8*4,Stream);
    end;
  end;
  Try
    { Note: it would be better to Fill the image palette in setupread instead of creating FPalette.
      FPalette is indeed useless but we cannot remove it since it's not private :\ }
    pallen:=0;
    if BFI.BitCount<=8 then
      if BFI.ClrUsed>0 then pallen:=BFI.ClrUsed
      else pallen:=(1 shl BFI.BitCount);
    if pallen>0 then
    begin
      Img.Palette.Count:=pallen;
      for i:=0 to pallen-1 do
        Img.Palette.Color[i]:=FPalette[i];
    end;
    Img.SetSize(BFI.Width,BFI.Height);

    percent:=0;
    percentinterval:=(Img.Height*4) div 100;
    if percentinterval=0 then percentinterval:=$FFFFFFFF;
    percentacc:=0;

    DeltaX:=-1; DeltaY:=-1;
      if TopDown then
        for Row:=0 to Img.Height-1 do { A rare case of top-down bitmap! }
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end
      else
        for Row:=Img.Height-1 downto 0 do
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end;
    Progress(psEnding,100,false,Rect,'',continue);
  finally
    FreeBufs;
  end;
end;

procedure TFPReaderBMP.ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
var i,j : integer;
    b0, b1 : byte;
begin
  i:=0;
  while true do
  begin
    { let's see if we must skip pixels because of delta... }
    if DeltaY<>-1 then
    begin
      if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
      else j:=ReadSize;            { else skip up to the end of this line }
      while (i<j) do
        begin
          LineBuf[i]:=0;
          inc(i);
        end;

      if Row=DeltaY then { we don't need delta anymore }
        DeltaY:=-1
      else break; { skipping must continue on the next line, we are finished here }
    end;

    Stream.Read(b0,1); Stream.Read(b1,1);
    if b0<>0 then { number of repetitions }
    begin
      if b0+i>ReadSize then
        raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
      j:=i+b0;
      while (i<j) do
      begin
        LineBuf[i]:=b1;
        inc(i);
      end;
    end
    else
      case b1 of 
        0: break; { end of line }
        1: break; { end of file }
        2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
             Stream.Read(b0,1); Stream.Read(b1,1);
             DeltaX:=i+b0; DeltaY:=Row+b1;
           end
        else begin { absolute mode }
               if b1+i>ReadSize then
                 raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
               Stream.Read(LineBuf[i],b1);
               inc(i,b1);
               { aligned on 2 bytes boundary: every group starts on a 2 bytes boundary, but absolute group
                 could end on odd address if there is a odd number of elements, so we pad it  }
               if (b1 mod 2)<>0 then Stream.Seek(1,soFromCurrent); 
             end;
      end;
  end;
end;

procedure TFPReaderBMP.ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
var i,j,tmpsize : integer;
    b0, b1 : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  tmpsize:=ReadSize*2; { ReadSize is in bytes, while nibline is made of nibbles, so it's 2*readsize long }
  getmem(nibline,tmpsize);
  if nibline=nil then
    raise FPImageException.Create('Out of memory');
  try
    i:=0;
    while true do
    begin
      { let's see if we must skip pixels because of delta... }
      if DeltaY<>-1 then
      begin
        if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
        else j:=tmpsize;            { else skip up to the end of this line }
        while (i<j) do
          begin
            NibLine[i]:=0;
            inc(i);
          end;

        if Row=DeltaY then { we don't need delta anymore }
          DeltaY:=-1
        else break; { skipping must continue on the next line, we are finished here }
      end;

      Stream.Read(b0,1); Stream.Read(b1,1);
      if b0<>0 then { number of repetitions }
      begin
        if b0+i>tmpsize then
          raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
        even:=true;
        j:=i+b0;
        while (i<j) do
        begin
          if even then NibLine[i]:=(b1 and $F0) shr 4
          else NibLine[i]:=b1 and $0F;
          inc(i);
          even:=not even;
        end;
      end
      else
        case b1 of 
          0: break; { end of line }
          1: break; { end of file }
          2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
               Stream.Read(b0,1); Stream.Read(b1,1);
               DeltaX:=i+b0; DeltaY:=Row+b1;
             end
          else begin { absolute mode }
                 if b1+i>tmpsize then
                   raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
                 j:=i+b1;
                 even:=true;
                 while (i<j) do
                 begin
                   if even then
                   begin
                     Stream.Read(b0,1);
                     NibLine[i]:=(b0 and $F0) shr 4;
                   end
                   else NibLine[i]:=b0 and $0F;
                   inc(i);
                   even:=not even;
                 end;
               { aligned on 2 bytes boundary: see rle8 for details  }
                 b1:=b1+(b1 mod 2);
                 if (b1 mod 4)<>0 then Stream.Seek(1,soFromCurrent);
               end;
        end;
    end;
    { pack the nibline into the linebuf }
    for i:=0 to ReadSize-1 do
      LineBuf[i]:=(NibLine[i*2] shl 4) or NibLine[i*2+1];
  finally
    FreeMem(nibline)
  end;
end;

procedure TFPReaderBMP.ReadScanLine(Row : Integer; Stream : TStream);
begin
  if BFI.Compression=BI_RLE8 then ExpandRLE8ScanLine(Row,Stream)
  else if BFI.Compression=BI_RLE4 then ExpandRLE4ScanLine(Row,Stream)
  else Stream.Read(LineBuf[0],ReadSize);
end;

procedure TFPReaderBMP.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Column : Integer;

begin
  Case BFI.BitCount of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.Pixels[Column,Row]:=1
       else
         img.Pixels[Column,Row]:=0;
   4 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f;
   8 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   16 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ExpandColor(PWord(LineBuf)[Column]);
   24 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=RGBToFPColor(PColorRGB(LineBuf)[Column]);
   32 :
      for Column:=0 to img.Width-1 do
        if BFI.Compression=BI_BITFIELDS then
          img.colors[Column,Row]:=ExpandColor(PLongWord(LineBuf)[Column])
        else
          img.colors[Column,Row]:=RGBAToFPColor(PColorRGBA(LineBuf)[Column]);
    end;

    inc(percentacc,4);
    if percentacc>=percentinterval then
    begin
      percent:=percent+(percentacc div percentinterval);
      percentacc:=percentacc mod percentinterval;
      Progress(psRunning,percent,false,Rect,'',continue);
    end;
end;

function  TFPReaderBMP.InternalCheck (Stream:TStream) : boolean;
// NOTE: Does not rewind the stream!
var
  BFH:TBitMapFileHeader;
  n: Int64;
begin
  Result:=False;
  if Stream=nil then
    exit;
  n:=SizeOf(BFH);
  Result:=Stream.Read(BFH,n)=n;
  if Result then 
    begin
   {$IFDEF ENDIAN_BIG}
    SwapBMPFileHeader(BFH);
   {$ENDIF}
    Result := BFH.bfType = BMmagic; // Just check magic number
    end;
end;

initialization
  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);
end.
