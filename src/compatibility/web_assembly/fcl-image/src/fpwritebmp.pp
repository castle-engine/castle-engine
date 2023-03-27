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
{ 08/2005 by Giulio Bernardi:
   - Removed FBytesPerPixel, BytesPerPixel property is now deprecated, use BitsPerPixel instead.
   - Rewritten a large part of the file, so we can handle all bmp color depths
   - Support for RLE4 and RLE8 encoding
  03/2015 MvdV finally removed bytesperpixel. 10 years should be enough.
}

{$mode objfpc}{$h+}
unit FPWriteBMP;

interface

uses FPImage, classes, sysutils, BMPComn;

type

  TFPWriterBMP = class (TFPCustomImageWriter)
  private
    StartPosition : int64; { save start of bitmap in the stream, if we must go back and fix something }
    FBpp : byte;
    FRLECompress : boolean;
    BFH : TBitMapFileHeader;
    BFI : TBitMapInfoHeader;
    Colinfo : array of TColorRGBA;
    fXPelsPerMeter,
    fYPelsPerMeter : integer;
    procedure SetColorSize (AValue : Byte);
    function GetColorSize : byte;
    procedure SetBpp (const abpp : byte);
    procedure FillColorMap(Img : TFPCustomImage);
    procedure Setup16bpp;
    function PackWord555(const col : TFPColor) : word;
    function PackWord565(const col : TFPColor) : word;
    function Pack4bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
    function Pack1bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
    procedure CompressScanLineRLE8(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
    procedure CompressScanLineRLE4(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property BitsPerPixel : byte read FBpp write SetBpp;
    property XPelsPerMeter : integer read fXPelsPerMeter write fXPelsPerMeter;
    property YPelsPerMeter : integer read fYPelsPerMeter write fYPelsPerMeter;
    property RLECompress : boolean read FRleCompress write FRleCompress;
  end;


implementation

Function FPColorToRGB(Const Color : TFPColor) : TColorRGB;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    end;
end;

Function FPColorToRGBA(Const Color : TFPColor) : TColorRGBA;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    A:=(Alpha and $FF00) shr 8;
    end;
end;

constructor TFPWriterBMP.create;
begin
  inherited create;
  fXPelsPerMeter:=100;
  fYPelsPerMeter:=100;
  FBpp:=24;
  FRleCompress:=false;
end;

{ Only for compatibility, BytesPerPixel should be removed }
{ ******************************************************* }
procedure TFPWriterBMP.SetColorSize (AValue : byte);
begin
  SetBpp(AValue*8);
end;

function TFPWriterBMP.GetColorSize : byte;
begin
  if FBpp<>15 then Result:=FBpp div 8
  else Result:=2;
end;
{ ******************************************************* }

procedure TFPWriterBMP.SetBpp (const abpp : byte);
begin
  if not (abpp in [1,4,8,15,16,24,32]) then
    raise FPImageException.Create('Invalid color depth');
  FBpp:=abpp;
end;

procedure TFPWriterBMP.FillColorMap(Img : TFPCustomImage);
var BadPalette : boolean;
    i : integer;
begin
  BadPalette:=false;
  if not Img.UsePalette then BadPalette:=true
  else if Img.Palette.Count>(1 shl FBpp) then BadPalette:=true;
  if BadPalette then 
    raise FPImageException.Create('Image palette is too big or absent');
  setlength(ColInfo,Img.Palette.Count);
  BFI.ClrUsed:=Img.Palette.Count;
  for i:=0 to BFI.ClrUsed-1 do
  begin
    ColInfo[i]:=FPColorToRGBA(Img.Palette.Color[i]);
    ColInfo[i].A:=0;
  end;
end;

{ True 16 bit color is 5 bits red, 6 bits green and 5 bits blue.
  Compression must be set to BI_BITFIELDS and we must specify masks for red, green and blue.
  16 bit without compression and masks is 5 bits per channel, so it's 15 bit even if in the header we
  must write 16.
  It's possible to provide custom masks but this is not compatible with windows9x, so we use 555 for 15 bit
  and 565 for 16 bit.
  Masks are longwords stored in the palette instead of palette entries (which are 4 bytes long too, with
  components stored in following order: B G R A. Since we must write a low-endian longword, B is LSB and A
  is the MSB).
  We must write first red mask, then green and then blue.

  This sounds terribly confusing, if you don't understand take a look at
  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/bitmaps_1rw2.asp
   }
procedure TFPWriterBMP.Setup16bpp;
var col : TColorRGBA;
begin
  BFI.Compression:=BI_BITFIELDS;
  setlength(ColInfo,3);
  {      A R G B
  r := $0000F800
  g := $000007E0
  b := $0000001F
  }
  col.A:=0; Col.R:=0; { These are 0 for all the three masks}
  { Red Mask }
  Col.G:=$F8; Col.B:=0;
  ColInfo[0]:=Col;
  { Green Mask }
  Col.G:=$07; Col.B:=$E0;
  ColInfo[1]:=Col;
  { Blue Mask }
  Col.G:=$00; Col.B:=$1F;
  ColInfo[2]:=Col;
end;

{ 16 bit bpp with 555 packing (that is, 15 bit color)
  This is bit dislocation:
  0RRR RRGG GGGB BBBB  }

function TFPWriterBMP.PackWord555(const col : TFPColor) : word;
var tmpcol : TColorRGB;
    tmpr, tmpg, tmpb : word;
begin
  tmpcol:=FPColorToRGB(col);
  tmpb:=tmpcol.b shr 3;
  tmpg:=tmpcol.g and $F8; tmpg:= tmpg shl 2;
  tmpr:=tmpcol.r and $F8; tmpr:= tmpr shl 7;
  tmpb:= tmpr or tmpg or tmpb;
  {$IFDEF ENDIAN_BIG}
  tmpb:=swap(tmpb);
  {$ENDIF}
  Result:=tmpb;
end;

{ 16 bit bpp with 565 packing )
  This is bit dislocation:
  RRRR RGGG GGGB BBBB  }

function TFPWriterBMP.PackWord565(const col : TFPColor) : word;
var tmpcol : TColorRGB;
    tmpr, tmpg, tmpb : word;
begin
  tmpcol:=FPColorToRGB(col);
  tmpb:=tmpcol.b shr 3;
  tmpg:=tmpcol.g and $FC; tmpg:= tmpg shl 3;
  tmpr:=tmpcol.r and $F8; tmpr:= tmpr shl 8;
  tmpb:= tmpr or tmpg or tmpb;
  {$IFDEF ENDIAN_BIG}
  tmpb:=swap(tmpb);
  {$ENDIF}
  Result:=tmpb;
end;

{ First pixel in the most significant nibble, second one in LSN. If we are at the end of the line,
  pad with zero }
function TFPWriterBMP.Pack4bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
var b : byte;
begin
  b:=(img.Pixels[Col,Row] and $F) shl 4;
  if Col<img.Width-1 then
  begin
    inc(Col);
    b:=b + (img.Pixels[Col,Row] and $F);
  end;
  Result:=b;
  inc(col);
end;

{ First pixel in the most significant bit, last one in LSN. If we are at the end of the line,
  pad with zero }
function TFPWriterBMP.Pack1bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
var b : byte;
    sh : shortint;
begin
  b:=0;
  sh:=7;
  while ((Col<Img.Width) and (sh>=0)) do
  begin
    if img.Pixels[Col,Row]<>0 then { set this bit }
      b:=b+(1 shl sh);
    dec(sh);
    inc(Col);
  end;
  Result:=b;
end;

function TFPWriterBMP.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  Result:=False;
  with BFI do
    begin
    Size:=sizeof(TBitMapInfoHeader);
    Width:=Img.Width;
    Height:=Img.Height;
    Planes:=1;
    if FBpp=15 then BitCount:=16
    else BitCount:=FBpp;
    XPelsPerMeter:=fXPelsPerMeter;
    YPelsPerMeter:=fYPelsPerMeter;
    ClrImportant:=0;
    end;
  with BFH do
    begin
    bfType:=BMmagic;//'BM'
    bfOffset:=sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader)+length(ColInfo)*4;
    bfReserved:=0;
    bfSize:=bfOffset+BFI.SizeImage;
    end;
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(BFH);
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  StartPosition:=Stream.Position;
  Stream.Write(bfh,sizeof(TBitMapFileHeader));
  Stream.Write(bfi,sizeof(TBitMapInfoHeader));
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(BFH);
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  Result:=true;
end;

{ This code is rather ugly and difficult to read, but compresses better than gimp.
  Brief explanation:
  A repetition is good if it's made of 3 elements at least: we have 2 bytes instead of 1. Let's call this a 
  "repetition" or "true repetition".
  So we start finding the first repetition from current position.
  Once found, we must decide how to handle elements between current position (i) and the repetition position (j)
  if j-i = 0 we are on the repetition, so we encode it
  if j-i = 1 there is only one pixel. We can't do anything but encode it as a repetition of 1 element.
  if j-i = 2 we have two pixels. These can be a couple (a repetition of 2 elements) or 2 singles
             (2 repetitions of 1 element)
  if j-i > 2 we have two choices. In fact, we must consider that absolute mode is 2 bytes + length of chunk.
             A repetition is always 2 bytes, so for 1 element we leak 1 byte, while for 2 elements we don't leak
             any byte.
             So if we have at most 1 single this means that everything else is made up of couples: it's best to
             use repetitions so that we leak 0 to 1 byte.
             If we have 2 singles or more it's better to use absolute mode, since we leak 2 bytes always,
             without regard to the size of chunk. }

procedure TFPWriterBMP.CompressScanLineRLE8(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles : integer;
    prev,tmp : byte;
begin
  i:=0;
  while (i<Width) do
  begin
    { let's see how bytes are disposed, so that we can choose the best way to compress }
    couples:=0; singles:=1;
    prev:=Aline[i];
    j:=i+1;
    while ((j<Width) and ((j-i)<255)) do
    begin
      if Aline[j]=prev then { this is a couple at least }
      begin
        dec(singles); { so the previous one wasn't a single }
        if (((j+1)<Width) and (Aline[j+1]=prev)) then { at least three equal items, it's a repetition }
        begin
          dec(j); { repetition starts at j-1, since j is the middle pixel and j+1 is the third pixel }
          break;
        end
        else inc(couples) { ok it's a couple }
      end
      else inc(singles); { this is a single if next isn't a couple }
      prev:=Aline[j];
      inc(j);
    end;

    { ok, now that we know more about byte disposition we write data }
    case (j-i) of
      0 : begin { there is a repetition with count>=3 }
            prev:=Aline[i];
            j:=i+1;
            while ((j<Width) and ((j-i)<255)) do
            begin
              if Aline[j]<>prev then break;
              inc(j);
            end;
            tmp:=j-i;
            Stream.Write(tmp,1);
            Stream.Write(prev,1);
          end;
      1 : begin { single value: we write a repetition of 1 }
            tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline[i],1);
          end;
      2 : begin
            if couples=1 then { a couple: we write a repetition of 2 }
            begin
              tmp:=2;
              Stream.Write(tmp,1);
              Stream.Write(Aline[i],1);
            end
            else { two singles: we write two repetitions of 1 each }
            begin
              tmp:=1;
              Stream.Write(tmp,1);
              Stream.Write(Aline[i],1);
              Stream.Write(tmp,1);
              Stream.Write(Aline[i+1],1);
            end;
          end;
      else { here we have two choices }
      begin
        if singles>1 then { it's cheaper to use absolute mode }
        begin
          tmp:=0; Stream.Write(tmp,1);   { escape }
          tmp:=j-i; Stream.Write(tmp,1); { number of pixels in absolute mode }
          Stream.Write(Aline[i],j-i);    { write these pixels... }
          if ((tmp mod 2)<>0) then       { we must end on a 2-byte boundary }
          begin
            tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
          end;
        end
        else { they're nearly all couples, don't use absolute mode }
        begin
          k:=i;
          while (k<j) do
          begin
            if ((k+1<j) and (Aline[k]=Aline[k+1])) then
            begin
              tmp:=2;
              inc(k);
            end
            else tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline[k],1);
            inc(k);
          end;
        end;
      end;
    end;
    i:=j;
  end;
  tmp:=0; Stream.Write(tmp,1); { escape }
  if Row=0 then { last line, end of file }
    tmp:=1;
  Stream.Write(tmp,1);
end;

{ Ok, this is even uglier than the RLE8 version above, and this time gimp compresses better :\
  Differences with RLE8: repetition count is pixel-relative, not byte-relative, but repetition data is made
  of 2 pixels. So you have a repetition when you have pixels repeated in an alternate way, even if you can do
  something like:
  01E0 => E
  0316 => 161.
  A repetition is good if it's made of five elements at least (2 bytes instead of 3).
  In rle4 we consider "single" either a single nibble or 2 (a byte), while a couple is a repetition of 3 or 4
  elements. }

procedure TFPWriterBMP.CompressScanLineRLE4(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles, lastsingle : integer;
    prev1, prev2, prev : word;
    tmp : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  even:=false;
  getmem(nibline,width);
  try
    k:=(Width div 2) + (Width mod 2);
    i:=0;
    while (i<k) do
    begin
      nibline[i*2]:=aline[i] shr 4;
      nibline[i*2+1]:=aline[i] and $F;
      inc(i);
    end;
    i:=0;
    while (i<Width) do
    begin
      { let's see how nibbles are disposed, so that we can choose the best way to compress }
      couples:=0; singles:=1; lastsingle:=-10;
      prev1:=nibline[i];
      prev2:=nibline[i+1];
      j:=i+2;
      while ((j<Width) and ((j-i)<255)) do
      begin
        if nibline[j]=prev1 then { this is a half-couple at least (repetition of 3) }
        begin
          dec(singles); { so the previous one wasn't a single }
          if (((j+1)<Width) and (nibline[j+1]=prev2)) then { at least a couple (repetition of 4) }
          begin
            if (((j+2)<Width) and (nibline[j+2]=prev1)) then { at least a repetition of 5, good }
            begin
              dec(j,2); { repetition starts at j-2: prev1 prev2 prev1* prev2 prev1, we are here * }
              break;
            end
            else
            begin { ok it's a couple }
              inc(couples);
              if (j-i)=254 then { in this rare case, j-i becomes 256. So, force a half-couple and exit }
              begin
                inc(j);
                break;
              end;
              prev1:=256; { this is a couple, don't consider these positions in further scanning }
              prev2:=256;
              inc(j,2);
              continue;
            end
          end
          else
            begin { ok it's a half-couple }
            inc(couples);
            prev:=256; //this is a half-couple, don't consider this position in further scanning.
          end;
        end
        else
        begin
          if lastsingle<>(j-1) then
          begin
            inc(singles); { this is a single if next isn't a couple }
            lastsingle:=j;
          end;
          prev:=nibline[j];
        end;
        prev1:=prev2;
        prev2:=prev;
        even:=not even;
        inc(j);
      end;
      if j>Width then j:=Width; { if j was Width-1 loop was skipped and j is Width+1, so we fix it }

      { ok, now that we know more about byte disposition we write data }
      case (j-i) of
        0 : begin { there is a repetition with count>=5 }
              even:=true;
              prev1:=nibline[i];
              prev2:=nibline[i+1];
              j:=i+2;
              while ((j<Width) and ((j-i)<255)) do
              begin
                if even then if nibline[j]<>prev1 then break;
                if not even then if nibline[j]<>prev2 then break;
                even:=not even;
                inc(j);
              end;
              tmp:=j-i;
              Stream.Write(tmp,1);
              prev:=(prev1 shl 4) + (prev2 and $F);
              tmp:=prev;
              Stream.Write(tmp,1);
            end;
        1 : begin { single value: we write a repetition of 1 }
              tmp:=1;
              Stream.Write(tmp,1);
              tmp:=nibline[i] shl 4;
              Stream.Write(tmp,1);
            end;
        2 : begin { 2 singles in the same byte: we write a repetition of 2 }
              tmp:=2;
              Stream.Write(tmp,1);
              tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
              Stream.Write(tmp,1);
            end;
        3 : begin
              if couples=1 then { a couple: we write a repetition of 3 }
              begin
                tmp:=3;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 and 1 respectively }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=1;
                Stream.Write(tmp,1);
                tmp:=nibline[i+2] shl 4;
                Stream.Write(tmp,1);
              end;
            end;
        4 : begin
              if singles=0 then { a couple: we write a repetition of 4 }
              begin
                tmp:=4;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 each }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i+2] shl 4) + (nibline[i+3] and $F);
                Stream.Write(tmp,1);
              end;
            end;
        else { here we have two choices }
        begin
          if singles>1 then { it's cheaper to use absolute mode }
          begin
            tmp:=0; Stream.Write(tmp,1);    { escape }
            tmp:=j-i; Stream.Write(tmp,1);  { number of pixels in absolute mode }
            k:=i;
            while (k<j) do                  { write these pixels... }
            begin
              tmp:=nibline[k] shl 4;
              inc(k);
              if k<j then
              begin
                tmp:=tmp+(nibline[k] and $F);
                inc(k);
              end;
              Stream.Write(tmp,1);
            end;
            k:=j-i;
            k:=k+(k mod 2);
            if (k mod 4)<>0 then            { we must end on a 2-byte boundary }
            begin
              tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
            end;
          end
          else { they're nearly all couples, don't use absolute mode }
          begin
            k:=i;
            while (k<j) do
            begin
              if ((k+2<j) and (nibline[k]=nibline[k+2])) then
              begin
                if ((k+3<j) and (nibline[k+1]=nibline[k+3])) then tmp:=4
                else tmp:=3;
              end
              else
              begin
                if (k+1>=j) then tmp:=1
                else if ((k+3<j) and (nibline[k+1]=nibline[k+3])) then tmp:=1
                else tmp:=2;
              end;
              Stream.Write(tmp,1);
              prev:=tmp;
              tmp:=nibline[k] shl 4;
              if tmp<>1 then tmp:=tmp+(nibline[k+1] and $F);
              Stream.Write(tmp,1);
              inc(k,prev);
            end;
          end;
        end;
      end;
      i:=j;
    end;
    tmp:=0; Stream.Write(tmp,1); { escape }
    if Row=0 then { last line, end of file }
      tmp:=1;
    Stream.Write(tmp,1);
  finally
    FreeMem(nibline);
  end;
end;

procedure TFPWriterBMP.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize:Integer;
  PadCount : byte;
  aLine: PByte;
  i : Integer;
  tmppos : int64;
  continue : boolean;
  percent : byte;
  percentinterval : longword;
  percentacc : longword;
  Rect : TRect;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  percent:=0;
  percentinterval:=(Img.Height*4) div 100;
  if percentinterval=0 then percentinterval:=$FFFFFFFF;
  percentacc:=0;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;
  if (FRLECompress and (not (FBpp in [4,8]))) then
    raise FPImageException.Create('Can''t use RLE compression with '+IntToStr(FBpp)+' bits per pixel');
  if FRLECompress and (FBpp=4) then BFI.Compression:=BI_RLE4
  else if FRLECompress and (FBpp=8) then BFI.Compression:=BI_RLE8
  else BFI.Compression:=BI_RGB;
  BFI.ClrUsed:=0;
  try
    if FBpp<=8 then FillColorMap(Img); { sets colormap and ClrUsed}
    if FBpp=16 then Setup16bpp; { sets colormap with masks and Compression }
    RowSize:=0; { just to keep the compiler quiet. }
    case FBpp of
      1 : begin
            RowSize:=Img.Width div 8;
            if (Img.Width mod 8)<>0 then
              inc(RowSize);
          end;
      4 : begin
            RowSize:=Img.Width div 2;
            if (Img.Width mod 2)<>0 then
              inc(RowSize);
          end;
      8 : RowSize:=Img.Width;
     15 : RowSize:=Img.Width*2;
     16 : RowSize:=Img.Width*2;
     24 : RowSize:=Img.Width*3;
     32 : RowSize:=Img.Width*4;
    end;
    PadCount:=(4-(RowSize mod 4)) mod 4; { every row must end on 4 byte boundary }
    inc(RowSize,PadCount);
    BFI.SizeImage:=RowSize*Img.Height;

    SaveHeader(Stream,Img); { write the headers }
    for i:=0 to length(ColInfo)-1 do { write the palette (or the masks in 16bpp case) }
      Stream.Write(ColInfo[i],sizeof(TColorRGBA));

    GetMem(aLine,RowSize);
    try
      for Row:=Img.Height-1 downto 0 do
      begin
        i:=0; Col:=0;
        case FBpp of
          1 : while(Col<img.Width) do
              begin
                PByte(aline)[i]:=Pack1bpp(img,Col,Row); { increases Col by 8 each time }
                inc(i);
              end;
          4 : while(Col<img.Width) do
              begin
                PByte(aline)[i]:=Pack4bpp(img,Col,Row); { increases Col by 2 each time }
                inc(i);
              end;
          8 : for Col:=0 to img.Width-1 do
                PByte(aline)[Col]:=img.Pixels[Col,Row];
         15 : for Col:=0 to img.Width-1 do
                PWord(aline)[Col]:=PackWord555(img.colors[Col,Row]);
         16 : for Col:=0 to img.Width-1 do
                PWord(aline)[Col]:=PackWord565(img.colors[Col,Row]);
         24 : for Col:=0 to img.Width-1 do
                PColorRGB(aLine)[Col]:=FPColorToRGB(img.colors[Col,Row]);
         32 : for Col:=0 to img.Width-1 do
                PColorRGBA(aLine)[Col]:=FPColorToRGBA(img.colors[Col,Row]);
        end;
        { pad the scanline with zeros }
        for i:=RowSize-PadCount to RowSize-1 do
          Pbyte(aline)[i]:=0;

        if BFI.Compression=BI_RLE8 then CompressScanLineRLE8(aLine,Row,img.Width,Stream)
        else if BFI.Compression=BI_RLE4 then CompressScanLineRLE4(aLine,Row,img.Width,Stream)
        else Stream.Write(aLine[0],RowSize);

        inc(percentacc,4);
        if percentacc>=percentinterval then
        begin
          percent:=percent+(percentacc div percentinterval);
          percentacc:=percentacc mod percentinterval;
          Progress(psRunning,percent,false,Rect,'',continue);
          if not continue then exit;
        end;
      end;
      { If image is compressed we must fix the headers since we now know the size of the image }
      if BFI.Compression in [BI_RLE4,BI_RLE8] then 
      begin
        tmppos:=Stream.Position-StartPosition-BFH.bfOffset;
        BFI.SizeImage:=tmppos;          { set size of the image }
        tmppos:=Stream.Position;        { remember where we are }
        Stream.Position:=StartPosition; { rewind to the beginning }
        SaveHeader(Stream,Img);         { rewrite headers (this will update BFH.Size too) }
        Stream.Position:=tmppos;        { restore our position }
      end;
      Progress(psEnding,100,false,Rect,'',continue);
    finally
      FreeMem(aLine);
    end;
  finally
    setlength(ColInfo,0);
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TFPWriterBMP);
end.
