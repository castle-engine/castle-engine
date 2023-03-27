{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    Tiff writer for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Working:
   Grayscale 8,16bit (optional alpha),
   RGB 8,16bit (optional alpha),
   Orientation,
   multiple images, pages
   thumbnail
   Compression: deflate

 ToDo:
   Compression: LZW, packbits, jpeg, ...
   Planar
   ColorMap
   separate mask
   fillorder - not needed by baseline tiff reader
   bigtiff 64bit offsets
   endian - currently using system endianess
   orientation with rotation
}
unit FPWriteTiff;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, zbase, zdeflate, FPimage, FPTiffCmn;

type

  { TTiffWriterEntry }

  TTiffWriterEntry = class
  public
    Tag: Word;
    EntryType: Word;
    Count: DWord;
    Data: Pointer;
    DataPos: DWord;
    Bytes: DWord;
    destructor Destroy; override;
  end;

  TTiffWriterChunk = record
    Data: Pointer;
    Bytes: DWord;
  end;
  PTiffWriterChunk = ^TTiffWriterChunk;

  { TTiffWriterChunkOffsets }

  TTiffWriterChunkOffsets = class(TTiffWriterEntry)
  public
    Chunks: PTiffWriterChunk;
    ChunkByteCounts: TTiffWriterEntry;
    constructor Create(ChunkType: TTiffChunkType);
    destructor Destroy; override;
    procedure SetCount(NewCount: DWord);
  end;

  { TFPWriterTiff }

  TFPWriterTiff = class(TFPCustomImageWriter)
  private
    FSaveCMYKAsRGB: boolean;
    fStartPos: Int64;
    FEntries: TFPList; // list of TFPList of TTiffWriterEntry
    fStream: TStream;
    fPosition: DWord;
    procedure ClearEntries;
    procedure SortEntries;
    procedure WriteTiff;
    procedure WriteHeader;
    procedure WriteIFDs;
    procedure WriteEntry(Entry: TTiffWriterEntry);
    procedure WriteData;
    procedure WriteEntryData(Entry: TTiffWriterEntry);
    procedure WriteBuf(var Buf; Count: DWord);
    procedure WriteWord(w: Word);
    procedure WriteDWord(d: DWord);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure AddEntryString(Tag: word; const s: string);
    procedure AddEntryShort(Tag: word; Value: Word);
    procedure AddEntryLong(Tag: word; Value: DWord);
    procedure AddEntryShortOrLong(Tag: word; Value: DWord);
    procedure AddEntryRational(Tag: word; const Value: TTiffRational);
    procedure AddEntry(Tag: Word; EntryType: Word; EntryCount: DWord;
                       Data: Pointer; Bytes: DWord;
                       CopyData: boolean = true);
    procedure AddEntry(Entry: TTiffWriterEntry);
    procedure TiffError(Msg: string);
    procedure EncodeDeflate(var Buffer: Pointer; var Count: DWord);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddImage(Img: TFPCustomImage);
    procedure SaveToStream(Stream: TStream);
    property SaveCMYKAsRGB: boolean read FSaveCMYKAsRGB write FSaveCMYKAsRGB;
  end;

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;

function CompressDeflate(InputData: PByte; InputCount: cardinal;
  out Compressed: PByte; var CompressedCount: cardinal;
  ErrorMsg: PAnsiString = nil): boolean;

implementation

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;
begin
  Result:=integer(TTiffWriterEntry(Entry1).Tag)-integer(TTiffWriterEntry(Entry2).Tag);
end;

function CompressDeflate(InputData: PByte; InputCount: cardinal; out
  Compressed: PByte; var CompressedCount: cardinal; ErrorMsg: PAnsiString
  ): boolean;
var
  stream : z_stream;
  err : integer;
begin
  Result:=false;
  //writeln('CompressDeflate START');
  Compressed:=nil;
  if InputCount=0 then begin
    CompressedCount:=0;
    exit(true);
  end;

  err := deflateInit(stream{%H-}, Z_DEFAULT_COMPRESSION);
  if err <> Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflateInit failed';
    exit;
  end;

  // set input = InputData data
  stream.avail_in := InputCount;
  stream.next_in  := InputData;

  // set output = compressed data
  if CompressedCount=0 then
    CompressedCount:=InputCount;
  GetMem(Compressed,CompressedCount);
  stream.avail_out := CompressedCount;
  stream.next_out := Compressed;

  err := deflate(stream, Z_NO_FLUSH);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflate failed';
    exit;
  end;

  while TRUE do begin
    //writeln('run: total_in=',stream.total_in,' avail_in=',stream.avail_in,' total_out=',stream.total_out,' avail_out=',stream.avail_out);
    if (stream.avail_out=0) then begin
      // need more space
      if CompressedCount<128 then
        CompressedCount:=CompressedCount+128
      else if CompressedCount>High(CompressedCount)-1024 then begin
        if ErrorMsg<>nil then
          ErrorMsg^:='deflate compression failed, because not enough space';
        exit;
      end else
        CompressedCount:=CompressedCount+1024;
      ReAllocMem(Compressed,CompressedCount);
      stream.next_out:=Compressed+stream.total_out;
      stream.avail_out:=CompressedCount-stream.total_out;
    end;
    err := deflate(stream, Z_FINISH);
    if err = Z_STREAM_END then
      break;
    if err<>Z_OK then begin
      if ErrorMsg<>nil then
        ErrorMsg^:='deflate finish failed';
      exit;
    end;
  end;

  //writeln('compressed: total_in=',stream.total_in,' total_out=',stream.total_out);
  CompressedCount:=stream.total_out;
  ReAllocMem(Compressed,CompressedCount);

  err := deflateEnd(stream);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflateEnd failed';
    exit;
  end;
  Result:=true;
end;

{ TFPWriterTiff }

procedure TFPWriterTiff.WriteWord(w: Word);
begin
  if fStream<>nil then
    fStream.WriteWord(w);
  inc(fPosition,2);
end;

procedure TFPWriterTiff.WriteDWord(d: DWord);
begin
  if fStream<>nil then
    fStream.WriteDWord(d);
  inc(fPosition,4);
end;

procedure TFPWriterTiff.ClearEntries;
var
  i: Integer;
  List: TFPList;
  j: Integer;
begin
  for i:=FEntries.Count-1 downto 0 do begin
    List:=TFPList(FEntries[i]);
    for j:=List.Count-1 downto 0 do
      TObject(List[j]).Free;
    List.Free;
  end;
  FEntries.Clear;
end;

procedure TFPWriterTiff.WriteTiff;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TFPWriterTiff.WriteTiff fStream=',fStream<>nil);
  {$ENDIF}
  fPosition:=0;
  WriteHeader;
  WriteIFDs;
  WriteData;
end;

procedure TFPWriterTiff.WriteHeader;
var
  EndianMark: String;
begin
  EndianMark:={$IFDEF FPC_BIG_ENDIAN}'MM'{$ELSE}'II'{$ENDIF};
  WriteBuf(EndianMark[1],2);
  WriteWord(42);
  WriteDWord(8);
end;

procedure TFPWriterTiff.SortEntries;
var
  i, j: Integer;
  Entry: TTiffWriterEntry;
  List: TFPList;
begin
  // Sort Entries by Tag Value Ascending
  for i:= 0 to FEntries.Count-1 do begin
    List := TFPList(FEntries[i]);
    j := 0;
    repeat
        if TTiffWriterEntry(List[j]).Tag > TTiffWriterEntry(List[j+1]).Tag then begin
          Entry := TTiffWriterEntry(List[j+1]);
          List[j] := List[j+1];
          List[j+1] := Entry;
          j := 0;
        end
        else
            j := j+1;
    until j >= List.Count-2;
  end;
end;

procedure TFPWriterTiff.WriteIFDs;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriterEntry;
  NextIFDPos: DWord;
begin
  // Sort the Entries before writing!
  SortEntries;
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write count
    {$IFDEF FPC_Debug_Image}
    writeln('TFPWriterTiff.WriteIFDs List=',i,' Count=',List.Count);
    {$ENDIF}
    WriteWord(List.Count);
    // write array of entries
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      WriteEntry(Entry);
    end;
    // write position of next IFD
    if i<FEntries.Count-1 then
      NextIFDPos:=fPosition+4
    else
      NextIFDPos:=0;
    WriteDWord(NextIFDPos);
  end;
end;

procedure TFPWriterTiff.WriteEntry(Entry: TTiffWriterEntry);
var
  PadBytes: DWord;
begin
  {$IFDEF FPC_Debug_Image}
  //writeln('TFPWriterTiff.WriteEntry Tag=',Entry.Tag,' Type=',Entry.EntryType,' Count=',Entry.Count,' Bytes=',Entry.Bytes);
  {$ENDIF}
  WriteWord(Entry.Tag);
  WriteWord(Entry.EntryType);
  WriteDWord(Entry.Count);
  if Entry.Bytes<=4 then begin
    if Entry.Bytes>0 then
      WriteBuf(Entry.Data^,Entry.Bytes);
    PadBytes:=0;
    WriteBuf(PadBytes,4-Entry.Bytes);
  end else begin
    WriteDWord(Entry.DataPos);
  end;
end;

procedure TFPWriterTiff.WriteData;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriterEntry;
  Chunks: TTiffWriterChunkOffsets;
  k: Integer;
  Bytes: DWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write entry data
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      WriteEntryData(Entry);
    end;
    // write Chunks
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      if Entry is TTiffWriterChunkOffsets then begin
        Chunks:=TTiffWriterChunkOffsets(Entry);
        // write Chunks
        for k:=0 to Chunks.Count-1 do begin
          PDWord(Chunks.Data)[k]:=fPosition;
          Bytes:=Chunks.Chunks[k].Bytes;
          PDWord(Chunks.ChunkByteCounts.Data)[k]:=Bytes;
          {$IFDEF FPC_Debug_Image}
          //writeln('TFPWriterTiff.WriteData Chunk fPosition=',fPosition,' Bytes=',Bytes);
          {$ENDIF}
          if Bytes>0 then
            WriteBuf(Chunks.Chunks[k].Data^,Bytes);
        end;
      end;
    end;
  end;
end;

procedure TFPWriterTiff.WriteEntryData(Entry: TTiffWriterEntry);
begin
  if Entry.Bytes>4 then begin
    Entry.DataPos:=fPosition;
    WriteBuf(Entry.Data^,Entry.Bytes);
  end;
end;

procedure TFPWriterTiff.WriteBuf(var Buf; Count: DWord);
begin
  if Count=0 then exit;
  if (fStream<>nil) then
    fStream.Write(Buf,Count);
  inc(fPosition,Count);
end;

procedure TFPWriterTiff.AddImage(Img: TFPCustomImage);
var
  IFD: TTiffIFD;
  GrayBits, RedBits, GreenBits, BlueBits, AlphaBits: Word;
  ImgWidth, ImgHeight: DWord;
  Compression: Word;
  BitsPerSample: array[0..3] of Word;
  SamplesPerPixel: Integer;
  BitsPerPixel: DWord;
  i: Integer;
  OrientedWidth, OrientedHeight: DWord;
  BytesPerLine: DWord;
  ChunkType: TTiffChunkType;
  ChunkCount: DWord;
  ChunkOffsets: TTiffWriterChunkOffsets;
  ChunkIndex: DWord;
  ChunkBytes: DWord;
  Chunk: PByte;
  ChunkLeft, ChunkTop, ChunkWidth, ChunkHeight: DWord;
  TilesAcross, TilesDown: DWord;
  Run: PByte;
  Col: TFPColor;
  Value: Integer;
  CurEntries: TFPList;
  Shorts: array[0..3] of Word;
  NewSubFileType: DWord;
  cx,cy,x,y,sx: DWord;
  dx,dy: integer;
  ChunkBytesPerLine: DWord;
begin
  ChunkOffsets:=nil;
  Chunk:=nil;
  IFD:=TTiffIFD.Create;
  try
    // add new list of entries
    CurEntries:=TFPList.Create;
    FEntries.Add(CurEntries);

    IFD.ReadFPImgExtras(Img);
    if SaveCMYKAsRGB and (IFD.PhotoMetricInterpretation=5) then
      IFD.PhotoMetricInterpretation:=2;
    if not (IFD.PhotoMetricInterpretation in [0,1,2]) then
      TiffError('PhotoMetricInterpretation="'+Img.Extra[TiffPhotoMetric]+'" not supported');

    GrayBits:=0;
    RedBits:=0;
    GreenBits:=0;
    BlueBits:=0;
    AlphaBits:=0;
    case IFD.PhotoMetricInterpretation of
    0,1:
      begin
        GrayBits:=StrToIntDef(Img.Extra[TiffGrayBits],8);
        BitsPerSample[0]:=GrayBits;
        SamplesPerPixel:=1;
      end;
    2:
      begin
        RedBits:=StrToIntDef(Img.Extra[TiffRedBits],8);
        GreenBits:=StrToIntDef(Img.Extra[TiffGreenBits],8);
        BlueBits:=StrToIntDef(Img.Extra[TiffBlueBits],8);
        BitsPerSample[0]:=RedBits;
        BitsPerSample[1]:=GreenBits;
        BitsPerSample[2]:=BlueBits;
        SamplesPerPixel:=3;
      end;
    end;
    AlphaBits:=StrToIntDef(Img.Extra[TiffAlphaBits],8);
    if AlphaBits>0 then begin
      BitsPerSample[SamplesPerPixel]:=AlphaBits;
      inc(SamplesPerPixel);
    end;

    ImgWidth:=Img.Width;
    ImgHeight:=Img.Height;
    Compression:=IFD.Compression;
    case Compression of
    TiffCompressionNone,
    TiffCompressionDeflateAdobe: ;
    else
      {$ifdef FPC_DEBUG_IMAGE}
      writeln('TFPWriterTiff.AddImage unsupported compression '+TiffCompressionName(Compression)+', using deflate instead.');
      {$endif}
      Compression:=TiffCompressionDeflateAdobe;
    end;

    if IFD.Orientation in [1..4] then begin
      OrientedWidth:=ImgWidth;
      OrientedHeight:=ImgHeight;
    end else begin
      // rotated
      OrientedWidth:=ImgHeight;
      OrientedHeight:=ImgWidth;
    end;

    {$IFDEF FPC_Debug_Image}
    writeln('TFPWriterTiff.AddImage PhotoMetricInterpretation=',IFD.PhotoMetricInterpretation);
    writeln('TFPWriterTiff.AddImage ImageWidth=',ImgWidth,' ImageHeight=',ImgHeight);
    writeln('TFPWriterTiff.AddImage Orientation=',IFD.Orientation);
    writeln('TFPWriterTiff.AddImage ResolutionUnit=',IFD.ResolutionUnit);
    writeln('TFPWriterTiff.AddImage XResolution=',TiffRationalToStr(IFD.XResolution));
    writeln('TFPWriterTiff.AddImage YResolution=',TiffRationalToStr(IFD.YResolution));
    writeln('TFPWriterTiff.AddImage GrayBits=',GrayBits,' RedBits=',RedBits,' GreenBits=',GreenBits,' BlueBits=',BlueBits,' AlphaBits=',AlphaBits);
    writeln('TFPWriterTiff.AddImage Compression=',TiffCompressionName(Compression));
    writeln('TFPWriterTiff.AddImage Page=',IFD.PageNumber,'/',IFD.PageCount);
    {$ENDIF}

    // required meta entries
    AddEntryShortOrLong(256,ImgWidth);
    AddEntryShortOrLong(257,ImgHeight);
    AddEntryShort(259,Compression);
    AddEntryShort(262,IFD.PhotoMetricInterpretation);
    AddEntryShort(274,IFD.Orientation);
    AddEntryShort(296,IFD.ResolutionUnit);
    AddEntryRational(282,IFD.XResolution);
    AddEntryRational(283,IFD.YResolution);
    if AlphaBits>0 then begin
      // ExtraSamples
      AddEntryShort(338,2);// 2=unassociated alpha
    end;
    // BitsPerSample (required)
    AddEntry(258,3,SamplesPerPixel,@BitsPerSample[0],SamplesPerPixel*2);
    AddEntryShort(277,SamplesPerPixel);

    // BitsPerPixel, BytesPerLine
    BitsPerPixel:=0;
    for i:=0 to SamplesPerPixel-1 do
      inc(BitsPerPixel,BitsPerSample[i]);
    BytesPerLine:=(BitsPerPixel*OrientedWidth+7) div 8;

    // optional entries
    NewSubFileType:=0;
    if IFD.ImageIsThumbNail then inc(NewSubFileType,1);
    if IFD.ImageIsPage then inc(NewSubFileType,2);
    if IFD.ImageIsMask then inc(NewSubFileType,4);
    if NewSubFileType>0 then
      AddEntryLong(254,NewSubFileType);
    if IFD.DocumentName<>'' then
      AddEntryString(269,IFD.DocumentName);
    if IFD.ImageDescription<>'' then
      AddEntryString(270,IFD.ImageDescription);
    if IFD.Make_ScannerManufacturer<>'' then
      AddEntryString(271,IFD.Make_ScannerManufacturer);
    if IFD.Model_Scanner<>'' then
      AddEntryString(272,IFD.Model_Scanner);
    if IFD.Software<>'' then
      AddEntryString(305,IFD.Software);
    if IFD.DateAndTime<>'' then
      AddEntryString(306,IFD.DateAndTime);
    if IFD.Artist<>'' then
      AddEntryString(315,IFD.Artist);
    if IFD.HostComputer<>'' then
      AddEntryString(316,IFD.HostComputer);
    if IFD.PageCount>0 then begin
      Shorts[0]:=IFD.PageNumber;
      Shorts[1]:=IFD.PageCount;
      AddEntry(297,3,2,@Shorts[0],2*SizeOf(Word));
    end;
    if IFD.PageName<>'' then
      AddEntryString(285,IFD.PageName);
    if IFD.Copyright<>'' then
      AddEntryString(33432,IFD.Copyright);

    // chunks
    ChunkType:=tctStrip;
    if IFD.TileWidth>0 then begin
      AddEntryShortOrLong(322,IFD.TileWidth);
      AddEntryShortOrLong(323,IFD.TileLength);
      ChunkType:=tctTile;
    end else begin
      // RowsPerStrip (required)
      if OrientedWidth=0 then
        IFD.RowsPerStrip:=8
      else
        IFD.RowsPerStrip:=8192 div BytesPerLine;
      if IFD.RowsPerStrip<1 then
        IFD.RowsPerStrip:=1;
      {$IFDEF FPC_Debug_Image}
      writeln('TFPWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' BytesPerLine=',BytesPerLine,' RowsPerStrip=',IFD.RowsPerStrip);
      {$ENDIF}
      AddEntryShortOrLong(278,IFD.RowsPerStrip);
    end;

    // tags for Offsets and ByteCounts
    ChunkOffsets:=TTiffWriterChunkOffsets.Create(ChunkType);
    AddEntry(ChunkOffsets);
    AddEntry(ChunkOffsets.ChunkByteCounts);
    if (OrientedHeight>0) and (OrientedWidth>0) then begin
      if ChunkType=tctTile then begin
        TilesAcross:=(OrientedWidth+IFD.TileWidth{%H-}-1) div IFD.TileWidth;
        TilesDown:=(OrientedHeight+IFD.TileLength{%H-}-1) div IFD.TileLength;
        ChunkCount:=TilesAcross*TilesDown;
        {$IFDEF FPC_Debug_Image}
        writeln('TFPWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' OrientedHeight=',OrientedHeight,' TileWidth=',IFD.TileWidth,' TileLength=',IFD.TileLength,' TilesAcross=',TilesAcross,' TilesDown=',TilesDown,' ChunkCoun
t=',ChunkCount);
        {$ENDIF}
      end else begin
        ChunkCount:=(OrientedHeight+IFD.RowsPerStrip{%H-}-1) div IFD.RowsPerStrip;
      end;
      ChunkOffsets.SetCount(ChunkCount);
      // create chunks
      for ChunkIndex:=0 to ChunkCount-1 do begin
        if ChunkType=tctTile then begin
          ChunkLeft:=(ChunkIndex mod TilesAcross)*IFD.TileWidth;
          ChunkTop:=(ChunkIndex div TilesAcross)*IFD.TileLength;
          ChunkWidth:=Min(IFD.TileWidth,OrientedWidth-ChunkLeft);
          ChunkHeight:=Min(IFD.TileLength,OrientedHeight-ChunkTop);
          // boundary tiles are padded to a full tile
          // the padding is filled with 0 and compression will get rid of it
          ChunkBytesPerLine:=(BitsPerPixel*IFD.TileWidth+7) div 8;
          ChunkBytes:=ChunkBytesPerLine*IFD.TileLength;
        end else begin
          ChunkLeft:=0;
          ChunkTop:=IFD.RowsPerStrip*ChunkIndex;
          ChunkWidth:=OrientedWidth;
          ChunkHeight:=Min(IFD.RowsPerStrip,OrientedHeight-ChunkTop);
          ChunkBytesPerLine:=BytesPerLine;
          ChunkBytes:=ChunkBytesPerLine*ChunkHeight;
        end;
        GetMem(Chunk,ChunkBytes);
        FillByte(Chunk^,ChunkBytes,0); // fill unused bytes with 0 to help compression

        // Orientation
        if IFD.Orientation in [1..4] then begin
          x:=ChunkLeft; y:=ChunkTop;
          case IFD.Orientation of
          1: begin dx:=1; dy:=1; end;// 0,0 is left, top
          2: begin x:=OrientedWidth-x-1; dx:=-1; dy:=1; end;// 0,0 is right, top
          3: begin x:=OrientedWidth-x-1; dx:=-1; y:=OrientedHeight-y-1; dy:=-1; end;// 0,0 is right, bottom
          4: begin dx:=1; y:=OrientedHeight-y-1; dy:=-1; end;// 0,0 is left, bottom
          end;
        end else begin
          // rotated
          x:=ChunkTop; y:=ChunkLeft;
          case IFD.Orientation of
          5: begin dx:=1; dy:=1; end;// 0,0 is top, left (rotated)
          6: begin dx:=1; y:=OrientedWidth-y-1; dy:=-1; end;// 0,0 is top, right (rotated)
          7: begin x:=OrientedHeight-x-1; dx:=-1; y:=OrientedWidth-y-1; dy:=-1; end;// 0,0 is bottom, right (rotated)
          8: begin x:=OrientedHeight-x-1; dx:=-1; dy:=1; end;// 0,0 is bottom, left (rotated)
          end;
        end;
        //writeln('TFPWriterTiff.AddImage Chunk=',ChunkIndex,'/',ChunkCount,' ChunkBytes=',ChunkBytes,' ChunkRect=',ChunkLeft,',',ChunkTop,',',ChunkWidth,'x',ChunkHeight,' x=',x,' y=',y,' dx=',dx,' dy=',dy);
        sx:=x; // save start x
        for cy:=0 to ChunkHeight-1 do begin
          x:=sx;
          Run:=Chunk+cy*ChunkBytesPerLine;
          for cx:=0 to ChunkWidth-1 do begin
            Col:=Img.Colors[x,y];
            case IFD.PhotoMetricInterpretation of
            0,1:
              begin
                // grayscale
                Value:=(DWord(Col.red)+Col.green+Col.blue) div 3;
                if IFD.PhotoMetricInterpretation=0 then
                  Value:=$ffff-Value;// 0 is white
                if GrayBits=8 then begin
                  Run^:=Value shr 8;
                  inc(Run);
                end else if GrayBits=16 then begin
                  PWord(Run)^:=Value;
                  inc(Run,2);
                end;
                if AlphaBits=8 then begin
                  Run^:=Col.alpha shr 8;
                  inc(Run);
                end else if AlphaBits=16 then begin
                  PWord(Run)^:=Col.alpha;
                  inc(Run,2);
                end;
              end;
            2:
              begin
                // RGB
                if RedBits=8 then begin
                  Run^:=Col.red shr 8;
                  inc(Run);
                end else if RedBits=16 then begin
                  PWord(Run)^:=Col.red;
                  inc(Run,2);
                end;
                if GreenBits=8 then begin
                  Run^:=Col.green shr 8;
                  inc(Run);
                end else if GreenBits=16 then begin
                  PWord(Run)^:=Col.green;
                  inc(Run,2);
                end;
                if BlueBits=8 then begin
                  Run^:=Col.blue shr 8;
                  inc(Run);
                end else if BlueBits=16 then begin
                  PWord(Run)^:=Col.blue;
                  inc(Run,2);
                end;
                if AlphaBits=8 then begin
                  Run^:=Col.alpha shr 8;
                  inc(Run);
                end else if AlphaBits=16 then begin
                  PWord(Run)^:=Col.alpha;
                  inc(Run,2);
                end;
              end;
            end;
            // next x
            inc(x,dx);
          end;
          // next y
          inc(y,dy);
        end;

        // compress
        case Compression of
        TiffCompressionDeflateZLib, TiffCompressionDeflateAdobe: EncodeDeflate(Chunk,ChunkBytes);
        end;

        ChunkOffsets.Chunks[ChunkIndex].Data:=Chunk;
        ChunkOffsets.Chunks[ChunkIndex].Bytes:=ChunkBytes;
        // next chunk
      end;
      // created chunks
    end;

    CurEntries.Sort(@CompareTiffWriteEntries);
  finally
    IFD.Free;
  end;
end;

procedure TFPWriterTiff.SaveToStream(Stream: TStream);
begin
  fStartPos:=Stream.Position;
  // simulate write to compute offsets
  fStream:=nil;
  WriteTiff;
  // write to stream
  fStream:=Stream;
  WriteTiff;
  fStream:=nil;
end;

procedure TFPWriterTiff.InternalWrite(Stream: TStream; Img: TFPCustomImage);
begin
  AddImage(Img);
  SaveToStream(Stream);
end;

procedure TFPWriterTiff.AddEntryString(Tag: word; const s: string);
begin
  if s<>'' then
    AddEntry(Tag,2,length(s)+1,@s[1],length(s)+1)
  else
    AddEntry(Tag,2,0,nil,0);
end;

procedure TFPWriterTiff.AddEntryShort(Tag: word; Value: Word);
begin
  AddEntry(Tag,3,1,@Value,2);
end;

procedure TFPWriterTiff.AddEntryLong(Tag: word; Value: DWord);
begin
  AddEntry(Tag,4,1,@Value,4);
end;

procedure TFPWriterTiff.AddEntryShortOrLong(Tag: word; Value: DWord);
begin
  if Value<=High(Word) then
    AddEntryShort(Tag,Value)
  else
    AddEntryLong(Tag,Value);
end;

procedure TFPWriterTiff.AddEntryRational(Tag: word; const Value: TTiffRational
  );
begin
  AddEntry(Tag,5,1,@Value,8);
end;

procedure TFPWriterTiff.AddEntry(Tag: Word; EntryType: Word; EntryCount: DWord;
  Data: Pointer; Bytes: DWord; CopyData: boolean);
var
  Entry: TTiffWriterEntry;
begin
  Entry:=TTiffWriterEntry.Create;
  Entry.Tag:=Tag;
  Entry.EntryType:=EntryType;
  Entry.Count:=EntryCount;
  if CopyData then begin
    if Bytes>0 then begin
      GetMem(Entry.Data,Bytes);
      System.Move(Data^,Entry.Data^,Bytes);
    end else begin
      Entry.Data:=nil;
    end;
  end else
    Entry.Data:=Data;
  Entry.Bytes:=Bytes;
  AddEntry(Entry);
end;

procedure TFPWriterTiff.AddEntry(Entry: TTiffWriterEntry);
var
  List: TFPList;
begin
  List:=TFPList(FEntries[FEntries.Count-1]);
  List.Add(Entry);
end;

procedure TFPWriterTiff.TiffError(Msg: string);
begin
  raise Exception.Create('TFPWriterTiff.TiffError: '+Msg);
end;

procedure TFPWriterTiff.EncodeDeflate(var Buffer: Pointer; var Count: DWord);
var
  NewBuffer: PByte;
  NewCount: cardinal;
  ErrorMsg: String;
begin
  ErrorMsg:='';
  NewBuffer:=nil;
  try
    NewCount:=Count;
    if not CompressDeflate(Buffer,Count,NewBuffer,NewCount,@ErrorMsg) then
      TiffError(ErrorMsg);
    FreeMem(Buffer);
    Buffer:=NewBuffer;
    Count:=NewCount;
    NewBuffer:=nil;
  finally
    ReAllocMem(NewBuffer,0);
  end;
end;

constructor TFPWriterTiff.Create;
begin
  inherited Create;
  FEntries:=TFPList.Create;
  FSaveCMYKAsRGB:=true;
end;

destructor TFPWriterTiff.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

procedure TFPWriterTiff.Clear;
begin
  ClearEntries;
end;

{ TTiffWriterEntry }

destructor TTiffWriterEntry.Destroy;
begin
  ReAllocMem(Data,0);
  inherited Destroy;
end;

{ TTiffWriterChunkOffsets }

constructor TTiffWriterChunkOffsets.Create(ChunkType: TTiffChunkType);
begin
  EntryType:=4; // long
  ChunkByteCounts:=TTiffWriterEntry.Create;
  ChunkByteCounts.EntryType:=4; // long
  if ChunkType=tctTile then begin
    Tag:=324; // TileOffsets
    ChunkByteCounts.Tag:=325; // TileByteCounts
  end else begin
    Tag:=273; // StripOffsets
    ChunkByteCounts.Tag:=279; // StripByteCounts
  end;
end;

destructor TTiffWriterChunkOffsets.Destroy;
var
  i: Integer;
begin
  if Chunks<>nil then begin
    for i:=0 to Count-1 do
      ReAllocMem(Chunks[i].Data,0);
    ReAllocMem(Chunks,0);
  end;
  inherited Destroy;
end;

procedure TTiffWriterChunkOffsets.SetCount(NewCount: DWord);
var
  Size: DWord;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TTiffWriteStripOffsets.SetCount OldCount=',Count,' NewCount=',NewCount);
  {$ENDIF}
  Count:=NewCount;
  Size:=Count*SizeOf(TTiffWriterChunk);
  ReAllocMem(Chunks,Size);
  if Size>0 then FillByte(Chunks^,Size,0);
  Size:=Count*SizeOf(DWord);
  // Offsets
  ReAllocMem(Data,Size);
  if Size>0 then FillByte(Data^,Size,0);
  Bytes:=Size;
  // ByteCounts
  ReAllocMem(ChunkByteCounts.Data,Size);
  if Size>0 then FillByte(ChunkByteCounts.Data^,Size,0);
  ChunkByteCounts.Count:=Count;
  ChunkByteCounts.Bytes:=Size;
end;

initialization
  if ImageHandlers.ImageWriter[TiffHandlerName]=nil then
    ImageHandlers.RegisterImageWriter (TiffHandlerName, 'tif;tiff', TFPWriterTiff);
end.

