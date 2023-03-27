{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    Common stuff for Tiff image format.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
unit FPTiffCmn;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FPimage;

type
  TTiffRational = packed record
    Numerator, Denominator: DWord;
  end;

const
  TiffHandlerName = 'Tagged Image File Format';

  TiffRational0: TTiffRational = (Numerator: 0; Denominator: 0);
  TiffRational72: TTiffRational = (Numerator: 72; Denominator: 1);

  // TFPCustomImage.Extra properties used by TFPReaderTiff and TFPWriterTiff
  TiffExtraPrefix = 'Tiff';
  TiffPhotoMetric = TiffExtraPrefix+'PhotoMetricInterpretation';
  TiffGrayBits = TiffExtraPrefix+'GrayBits'; // CMYK: key plate
  TiffRedBits = TiffExtraPrefix+'RedBits'; // CMYK: cyan
  TiffGreenBits = TiffExtraPrefix+'GreenBits'; // CMYK: magenta
  TiffBlueBits = TiffExtraPrefix+'BlueBits'; // CMYK: yellow
  TiffAlphaBits = TiffExtraPrefix+'AlphaBits';
  TiffArtist = TiffExtraPrefix+'Artist';
  TiffCopyright = TiffExtraPrefix+'Copyright';
  TiffDocumentName = TiffExtraPrefix+'DocumentName';
  TiffDateTime = TiffExtraPrefix+'DateTime';
  TiffImageDescription = TiffExtraPrefix+'ImageDescription';
  TiffHostComputer = TiffExtraPrefix+'HostComputer';
  TiffMake_ScannerManufacturer = TiffExtraPrefix+'Make_ScannerManufacturer';
  TiffModel_Scanner = TiffExtraPrefix+'Model_Scanner';
  TiffOrientation = TiffExtraPrefix+'Orientation';
  TiffResolutionUnit = TiffExtraPrefix+'ResolutionUnit';
  TiffSoftware = TiffExtraPrefix+'Software';
  TiffXResolution = TiffExtraPrefix+'XResolution';
  TiffYResolution = TiffExtraPrefix+'YResolution';
  TiffPageNumber = TiffExtraPrefix+'PageNumber'; // starting at 0
  TiffPageCount = TiffExtraPrefix+'PageCount'; // if >0 the image is a page
  TiffPageName = TiffExtraPrefix+'PageName';
  TiffIsThumbnail = TiffExtraPrefix+'IsThumbnail';
  TiffIsMask = TiffExtraPrefix+'IsMask';
  TiffTileWidth = TiffExtraPrefix+'TileWidth';
  TiffTileLength = TiffExtraPrefix+'TileLength';
  TiffCompression = TiffExtraPrefix+'Compression'; // number

  TiffCompressionNone = 1; { No Compression, but pack data into bytes as tightly as possible,
       leaving no unused bits (except at the end of a row). The component
       values are stored as an array of type BYTE. Each scan line (row)
       is padded to the next BYTE boundary. }
  TiffCompressionCCITTRLE = 2; { CCITT Group 3 1-Dimensional Modified Huffman run length encoding. }
  TiffCompressionCCITTFAX3 = 3; { CCITT Group 3 fax encoding }
  TiffCompressionCCITTFAX4 = 4; { CCITT Group 4 fax encoding }
  TiffCompressionLZW = 5; { LZW }
  TiffCompressionOldJPEG = 6; { JPEG old style}
  TiffCompressionJPEG = 7; { JPEG new style }
  TiffCompressionDeflateAdobe = 8; { Deflate Adobe style }
  TiffCompressionJBIGBW = 9; { RFC2301 JBIG black/white }
  TiffCompressionJBIGCol = 10; { RFC2301 JBIG color }
  TiffCompressionNeXT = 32766; { Next }
  TiffCompressionCCITTRLEW = 32771; { CCITTRLEW }
  TiffCompressionPackBits = 32773; { PackBits Compression, a simple byte-oriented run length scheme.
         See the PackBits section for details. Data Compression applies
         only to raster image data. All other TIFF fields are unaffected. }
  TiffCompressionThunderScan = 32809; { THUNDERSCAN }
  TiffCompressionIT8CTPAD = 32895; { IT8CTPAD }
  TiffCompressionIT8LW = 32896; { IT8LW }
  TiffCompressionIT8MP = 32897; { IT8MP }
  TiffCompressionIT8BL = 32898; { IT8BL }
  TiffCompressionPixarFilm = 32908; { PIXARFILM }
  TiffCompressionPixarLog = 32909; { PIXARLOG }
  TiffCompressionDeflateZLib = 32946; { DeflatePKZip - obsolete,
       same as TiffCompressionDeflateAdobe,
       Macos Finder does not like this, use Adobe instead }
  TiffCompressionDCS = 32947; { DCS }
  TiffCompressionJBIG = 34661; { JBIG }
  TiffCompressionSGILog = 34676; { SGILOG }
  TiffCompressionSGILog24 = 34677; { SGILOG24 }
  TiffCompressionJPEG2000 = 34712; { JP2000 }

  // Planar configuration - TIFF 6.0 spec p. 38
  TiffPlanarConfigurationChunky = 1; //Chunky format
  TiffPlanarConfigurationPlanar = 2; //Planar format
type
  TTiffChunkType = (
    tctStrip,
    tctTile
    );

  { TTiffIFD - Image File Directory }

  TTiffIFD = class
  public
    IFDStart: DWord; // tiff position
    IFDNext: DWord; // tiff position
    Artist: String;
    BitsPerSample: DWord; // tiff position of entry
    BitsPerSampleArray: array of Word;
    CellLength: DWord;
    CellWidth: DWord;
    ColorMap: DWord;// tiff position of entry
    Compression: DWord;
    Predictor: Word;
    Copyright: string;
    DateAndTime: string;
    DocumentName: string;
    ExtraSamples: DWord;// tiff position of entry
    FillOrder: DWord;
    HostComputer: string;
    ImageDescription: string;
    ImageHeight: DWord;
    ImageIsMask: Boolean;
    ImageIsPage: Boolean;
    ImageIsThumbNail: Boolean;
    ImageWidth: DWord;
    Make_ScannerManufacturer: string;
    Model_Scanner: string;
    Orientation: DWord;
    PageNumber: word; // the page number starting at 0, the total number of pages is PageCount
    PageCount: word; // see PageNumber
    PageName: string;
    PhotoMetricInterpretation: DWord;
    PlanarConfiguration: DWord;
    ResolutionUnit: DWord;
    RowsPerStrip: DWord;
    SamplesPerPixel: DWord;
    Software: string;
    StripByteCounts: DWord;// tiff position of entry
    StripOffsets: DWord; // tiff position of entry
    TileWidth: DWord;
    TileLength: DWord; // = Height
    TileOffsets: DWord; // tiff position of entry
    TileByteCounts: DWord; // tiff position of entry
    Tresholding: DWord;
    XResolution: TTiffRational;
    YResolution: TTiffRational;
    // image
    Img: TFPCustomImage;
    FreeImg: boolean;
    RedBits: word;
    GreenBits: word;
    BlueBits: word;
    GrayBits: word;
    AlphaBits: word;
    BytesPerPixel: Word;
    procedure Clear;
    procedure Assign(IFD: TTiffIFD);
    procedure ReadFPImgExtras(Src: TFPCustomImage);
    function ImageLength: DWord; inline;
    constructor Create;
    destructor Destroy; override;
  end;

function TiffRationalToStr(const r: TTiffRational): string;
function StrToTiffRationalDef(const s: string; const Def: TTiffRational): TTiffRational;
procedure ClearTiffExtras(Img: TFPCustomImage);
procedure CopyTiffExtras(SrcImg, DestImg: TFPCustomImage);
procedure WriteTiffExtras(Msg: string; Img: TFPCustomImage);
function TiffCompressionName(c: Word): string;

implementation

function TiffRationalToStr(const r: TTiffRational): string;
begin
  Result:=IntToStr(r.Numerator)+'/'+IntToStr(r.Denominator);
end;

function StrToTiffRationalDef(const s: string; const Def: TTiffRational
  ): TTiffRational;
var
  p: LongInt;
begin
  Result:=Def;
  p:=System.Pos('/',s);
  if p<1 then exit;
  Result.Numerator:=StrToIntDef(copy(s,1,p-1),TiffRational0.Numerator);
  Result.Denominator:=StrToIntDef(copy(s,p+1,length(s)),TiffRational0.Denominator);
end;

procedure ClearTiffExtras(Img: TFPCustomImage);
var
  i: Integer;
begin
  for i:=Img.ExtraCount-1 downto 0 do
    if SysUtils.CompareText(copy(Img.ExtraKey[i],1,4),'Tiff')=0 then
      Img.RemoveExtra(Img.ExtraKey[i]);
end;

procedure CopyTiffExtras(SrcImg, DestImg: TFPCustomImage);
var
  i: Integer;
begin
  ClearTiffExtras(DestImg);
  for i:=SrcImg.ExtraCount-1 downto 0 do
    if SysUtils.CompareText(copy(SrcImg.ExtraKey[i],1,4),'Tiff')=0 then
      DestImg.Extra[SrcImg.ExtraKey[i]]:=SrcImg.ExtraValue[i];
end;

procedure WriteTiffExtras(Msg: string; Img: TFPCustomImage);
var
  i: Integer;
begin
  writeln('WriteTiffExtras ',Msg);
  for i:=0 to Img.ExtraCount-1 do
    //if SysUtils.CompareText(copy(Img.ExtraKey[i],1,4),'Tiff')=0 then
      writeln('  ',i,' ',Img.ExtraKey[i],'=',Img.ExtraValue[i]);
end;

function TiffCompressionName(c: Word): string;
begin
  case c of
  1: Result:='no compression';
  2: Result:='CCITT Group 3 1-Dimensional Modified Huffman run length encoding';
  3: Result:='CCITT Group 3 fax encoding';
  4: Result:='CCITT Group 4 fax encoding';
  5: Result:='LZW';
  6: Result:='JPEG old style';
  7: Result:='JPEG';
  8: Result:='Deflate Adobe style';
  9: Result:='RFC2301 JBIG white/black';
  10: Result:='RFC2301 JBIG color';
  32766: Result:='NeXT';
  32771: Result:='CCITTRLEW';
  32773: Result:='PackBits';
  32809: Result:='THUNDERSCAN';
  32895: Result:='IT8CTPAD';
  32896: Result:='IT8LW';
  32897: Result:='IT8MP';
  32898: Result:='IT8BL';
  32908: Result:='PIXARFILM';
  32909: Result:='PIXARLOG';
  32946: Result:='Deflate ZLib';
  32947: Result:='DCS';
  34661: Result:='JBIG';
  34676: Result:='SGILOG';
  34677: Result:='SGILOG24';
  34712: Result:='JP2000';
  else Result:='unknown('+IntToStr(c)+')';
  end;
end;

{ TTiffIFD }

procedure TTiffIFD.Clear;
begin
  IFDStart:=0;
  IFDNext:=0;
  PhotoMetricInterpretation:=High(PhotoMetricInterpretation);
  PlanarConfiguration:=TiffPlanarConfigurationChunky;
  Compression:=TiffCompressionNone;
  Predictor:=1;
  ImageHeight:=0;
  ImageWidth:=0;
  ImageIsThumbNail:=false;
  ImageIsPage:=false;
  ImageIsMask:=false;
  BitsPerSample:=0;
  SetLength(BitsPerSampleArray,0);
  ResolutionUnit:=0;
  XResolution:=TiffRational0;
  YResolution:=TiffRational0;
  RowsPerStrip:=0;
  StripOffsets:=0;
  StripByteCounts:=0;
  SamplesPerPixel:=0;
  Artist:='';
  HostComputer:='';
  ImageDescription:='';
  Make_ScannerManufacturer:='';
  Model_Scanner:='';
  Copyright:='';
  DateAndTime:='';
  Software:='';
  CellWidth:=0;
  CellLength:=0;
  FillOrder:=0;
  Orientation:=0;
  PageNumber:=0;
  PageCount:=0;
  PageName:='';

  // tiles
  TileWidth:=0;
  TileLength:=0;
  TileOffsets:=0;
  TileByteCounts:=0;

  Tresholding:=0;

  RedBits:=0;
  GreenBits:=0;
  BlueBits:=0;
  GrayBits:=0;
  AlphaBits:=0;
  BytesPerPixel:=0;

  if FreeImg then begin
    FreeImg:=false;
    FreeAndNil(Img);
  end;
end;

procedure TTiffIFD.Assign(IFD: TTiffIFD);
begin
  IFDStart:=IFD.IFDStart;
  IFDNext:=IFD.IFDNext;

  PhotoMetricInterpretation:=IFD.PhotoMetricInterpretation;
  PlanarConfiguration:=IFD.PlanarConfiguration;
  Compression:=IFD.Compression;
  Predictor:=IFD.Predictor;
  ImageHeight:=IFD.ImageHeight;
  ImageWidth:=IFD.ImageWidth;
  ImageIsThumbNail:=IFD.ImageIsThumbNail;
  ImageIsPage:=IFD.ImageIsPage;
  ImageIsMask:=IFD.ImageIsMask;
  BitsPerSample:=IFD.BitsPerSample;
  BitsPerSampleArray:=IFD.BitsPerSampleArray;
  ResolutionUnit:=IFD.ResolutionUnit;
  XResolution:=IFD.XResolution;
  YResolution:=IFD.YResolution;
  RowsPerStrip:=IFD.RowsPerStrip;
  StripOffsets:=IFD.StripOffsets;
  StripByteCounts:=IFD.StripByteCounts;
  SamplesPerPixel:=IFD.SamplesPerPixel;
  Artist:=IFD.Artist;
  HostComputer:=IFD.HostComputer;
  ImageDescription:=IFD.ImageDescription;
  Make_ScannerManufacturer:=IFD.Make_ScannerManufacturer;
  Model_Scanner:=IFD.Model_Scanner;
  Copyright:=IFD.Copyright;
  DateAndTime:=IFD.DateAndTime;
  Software:=IFD.Software;
  CellWidth:=IFD.CellWidth;
  CellLength:=IFD.CellLength;
  FillOrder:=IFD.FillOrder;
  Orientation:=IFD.Orientation;
  PageNumber:=IFD.PageNumber;
  PageCount:=IFD.PageCount;
  PageName:=IFD.PageName;

  // tiles
  TileWidth:=IFD.TileWidth;
  TileLength:=IFD.TileLength;
  TileOffsets:=IFD.TileOffsets;
  TileByteCounts:=IFD.TileByteCounts;

  Tresholding:=IFD.Tresholding;

  RedBits:=IFD.RedBits;
  GreenBits:=IFD.GreenBits;
  BlueBits:=IFD.BlueBits;
  GrayBits:=IFD.GrayBits;
  AlphaBits:=IFD.AlphaBits;
  if (Img<>nil) and (IFD.Img<>nil) then
    Img.Assign(IFD.Img);
end;

procedure TTiffIFD.ReadFPImgExtras(Src: TFPCustomImage);
begin
  Clear;
  PhotoMetricInterpretation:=2;
  if Src.Extra[TiffPhotoMetric]<>'' then
    PhotoMetricInterpretation:=
      StrToInt64Def(Src.Extra[TiffPhotoMetric],High(PhotoMetricInterpretation));
  Artist:=Src.Extra[TiffArtist];
  Copyright:=Src.Extra[TiffCopyright];
  DocumentName:=Src.Extra[TiffDocumentName];
  DateAndTime:=Src.Extra[TiffDateTime];
  HostComputer:=Src.Extra[TiffHostComputer];
  Make_ScannerManufacturer:=Src.Extra[TiffMake_ScannerManufacturer];
  Model_Scanner:=Src.Extra[TiffModel_Scanner];
  ImageDescription:=Src.Extra[TiffImageDescription];
  Software:=Src.Extra[TiffSoftware];
  Orientation:=StrToIntDef(Src.Extra[TiffOrientation],1);
  if not (Orientation in [1..8]) then
    Orientation:=1;
  ResolutionUnit:=StrToIntDef(Src.Extra[TiffResolutionUnit],2);
  if not (ResolutionUnit in [1..3]) then
    ResolutionUnit:=2;
  XResolution:=StrToTiffRationalDef(Src.Extra[TiffXResolution],TiffRational72);
  YResolution:=StrToTiffRationalDef(Src.Extra[TiffYResolution],TiffRational72);
  PageNumber:=StrToIntDef(Src.Extra[TiffPageNumber],0);
  PageCount:=StrToIntDef(Src.Extra[TiffPageCount],0);
  PageName:=Src.Extra[TiffPageName];
  ImageIsPage:=PageCount>0;
  ImageIsThumbNail:=Src.Extra[TiffIsThumbnail]<>'';
  ImageIsMask:=Src.Extra[TiffIsMask]<>'';
  TileWidth:=StrToIntDef(Src.Extra[TiffTileWidth],0);
  TileLength:=StrToIntDef(Src.Extra[TiffTileLength],0);
  Compression:=StrToIntDef(Src.Extra[TiffCompression],TiffCompressionNone);
end;

function TTiffIFD.ImageLength: DWord;
begin
  Result:=ImageHeight;
end;

constructor TTiffIFD.Create;
begin
  PlanarConfiguration:=TiffPlanarConfigurationChunky;
end;

destructor TTiffIFD.Destroy;
begin
  if FreeImg then
    FreeAndNil(Img);
  inherited Destroy;
end;

end.

