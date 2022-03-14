{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit contains image format loader/saver for GIF images.}
unit ImagingGif;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Imaging, ImagingTypes, ImagingIO, ImagingUtility;

type
  { GIF (Graphics Interchange Format) loader/saver class. GIF was
    (and is still used) popular format for storing images supporting
    multiple images per file and single color transparency.
    Pixel format is 8 bit indexed where each image frame can have
    its own color palette. GIF uses lossless LZW compression
    (patent expired few years ago).
    Imaging can load and save all GIFs with all frames and supports
    transparency. Imaging can load just raw ifIndex8 frames or
    also animate them in ifA8R8G8B8 format. See ImagingGIFLoadAnimated option.}
  TGIFFileFormat = class(TImageFileFormat)
  private
    FLoadAnimated: LongBool;
    function InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
    procedure LZWDecompress(Stream: TStream; Handle: TImagingHandle;
      Width, Height: Integer; Interlaced: Boolean; Data: Pointer);
    procedure LZWCompress(const IO: TIOFunctions; Handle: TImagingHandle;
      Width, Height, BitCount: Integer; Interlaced: Boolean; Data: Pointer);
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  published
    property LoadAnimated: LongBool read FLoadAnimated write FLoadAnimated;
  end;

implementation

const
  SGIFFormatName = 'Graphics Interchange Format';
  SGIFMasks      = '*.gif';
  GIFSupportedFormats: TImageFormats = [ifIndex8];
  GIFDefaultLoadAnimated = True;

type
  TGIFVersion = (gv87, gv89);
  TDisposalMethod = (dmNoRemoval, dmLeave, dmRestoreBackground,
    dmRestorePrevious, dmReserved4, dmReserved5, dmReserved6, dmReserved7);

const
  GIFSignature: TChar3 = 'GIF';
  GIFVersions: array[TGIFVersion] of TChar3 = ('87a', '89a');
  GIFDefaultDelay = 65;

  // Masks for accessing fields in PackedFields of TGIFHeader
  GIFGlobalColorTable = $80;
  GIFColorResolution  = $70;
  GIFColorTableSorted = $08;
  GIFColorTableSize   = $07;

  // Masks for accessing fields in PackedFields of TImageDescriptor
  GIFLocalColorTable  = $80;
  GIFInterlaced       = $40;
  GIFLocalTableSorted = $20;

  // Block identifiers
  GIFPlainText: Byte               = $01;
  GIFGraphicControlExtension: Byte = $F9;
  GIFCommentExtension: Byte        = $FE;
  GIFApplicationExtension: Byte    = $FF;
  GIFImageDescriptor: Byte         = Ord(',');
  GIFExtensionIntroducer: Byte     = Ord('!');
  GIFTrailer: Byte                 = Ord(';');
  GIFBlockTerminator: Byte         = $00;

  // Masks for accessing fields in PackedFields of TGraphicControlExtension
  GIFTransparent    = $01;
  GIFUserInput      = $02;
  GIFDisposalMethod = $1C;

const
  // Netscape sub block types
  GIFAppLoopExtension   = 1;
  GIFAppBufferExtension = 2;

type
  TGIFHeader = packed record
    // File header part
    Signature: TChar3;  // Header Signature (always "GIF")
    Version: TChar3;    // GIF format version("87a" or "89a")
    // Logical Screen Descriptor part
    ScreenWidth: Word;  // Width of Display Screen in Pixels
    ScreenHeight: Word; // Height of Display Screen in Pixels
    PackedFields: Byte; // Screen and color map information
    BackgroundColorIndex: Byte; // Background color index (in global color table)
    AspectRatio: Byte;  // Pixel aspect ratio, ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;        // X position of image with respect to logical screen
    Top: Word;         // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

const
  // GIF extension labels
  GIFExtTypeGraphic     = $F9;
  GIFExtTypePlainText   = $01;
  GIFExtTypeApplication = $FF;
  GIFExtTypeComment     = $FE;

type
  TGraphicControlExtension = packed record
    BlockSize: Byte;
    PackedFields: Byte;
    DelayTime: Word;
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

type
  TGIFIdentifierCode = array[0..7] of AnsiChar;
  TGIFAuthenticationCode = array[0..2] of AnsiChar;
  TGIFApplicationRec = packed record
    Identifier: TGIFIdentifierCode;
    Authentication: TGIFAuthenticationCode;
  end;

const
  CodeTableSize = 4096;
  HashTableSize = 17777;

type
  TReadContext = record
    Inx: Integer;
    Size: Integer;
    Buf: array [0..255 + 4] of Byte;
    CodeSize: Integer;
    ReadMask: Integer;
  end;
  PReadContext = ^TReadContext;

  TWriteContext = record
    Inx: Integer;
    CodeSize: Integer;
    Buf: array [0..255 + 4] of Byte;
  end;
  PWriteContext = ^TWriteContext;

  TOutputContext = record
    W: Integer;
    H: Integer;
    X: Integer;
    Y: Integer;
    BitsPerPixel: Integer;
    Pass: Integer;
    Interlace: Boolean;
    LineIdent: Integer;
    Data: Pointer;
    CurrLineData: Pointer;
  end;

  TImageDict = record
    Tail: Word;
    Index: Word;
    Col: Byte;
  end;
  PImageDict = ^TImageDict;

  PIntCodeTable = ^TIntCodeTable;
  TIntCodeTable = array [0..CodeTableSize - 1] of Word;

  TDictTable = array [0..CodeTableSize - 1] of TImageDict;
  PDictTable = ^TDictTable;

resourcestring
  SGIFDecodingError = 'Error when decoding GIF LZW data';

{
  TGIFFileFormat implementation
}

procedure TGIFFileFormat.Define;
begin
  inherited;
  FName := SGIFFormatName;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FSupportedFormats := GIFSupportedFormats;
  FLoadAnimated := GIFDefaultLoadAnimated;

  AddMasks(SGIFMasks);
  RegisterOption(ImagingGIFLoadAnimated, @FLoadAnimated);
end;

function TGIFFileFormat.InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
begin
  Result := Y;
  case Pass of
    0, 1:
      Inc(Result, 8);
    2:
      Inc(Result, 4);
    3:
      Inc(Result, 2);
  end;
  if Result >= Height then
  begin
    if Pass = 0 then
    begin
      Pass := 1;
      Result := 4;
      if Result < Height then
        Exit;
    end;
    if Pass = 1 then
    begin
      Pass := 2;
      Result := 2;
      if Result < Height then
        Exit;
    end;
    if Pass = 2 then
    begin
      Pass := 3;
      Result := 1;
    end;
  end;
end;

{ GIF LZW decompression code is from JVCL JvGIF.pas unit.}
procedure TGIFFileFormat.LZWDecompress(Stream: TStream; Handle: TImagingHandle; Width, Height: Integer;
  Interlaced: Boolean; Data: Pointer);
var
  MinCodeSize: Byte;
  MaxCode, BitMask, InitCodeSize: Integer;
  ClearCode, EndingCode, FirstFreeCode, FreeCode: Word;
  I, OutCount, Code: Integer;
  CurCode, OldCode, InCode, FinalChar: Word;
  Prefix, Suffix, OutCode: PIntCodeTable;
  ReadCtxt: TReadContext;
  OutCtxt: TOutputContext;
  TableFull: Boolean;

  function ReadCode(var Context: TReadContext): Integer;
  var
    RawCode: Integer;
    ByteIndex: Integer;
    Bytes: Byte;
    BytesToLose: Integer;
  begin
    while (Context.Inx + Context.CodeSize > Context.Size) and
      (Stream.Position < Stream.Size) do
    begin
      // Not enough bits in buffer - refill it - Not very efficient, but infrequently called
      BytesToLose := Context.Inx shr 3;
      // Note biggest Code Size is 12 bits. And this can at worst span 3 Bytes
      Move(Context.Buf[Word(BytesToLose)], Context.Buf[0], 3);
      Context.Inx := Context.Inx and 7;
      Context.Size := Context.Size - (BytesToLose shl 3);
      Stream.Read(Bytes, 1);
      if Bytes > 0 then
        Stream.Read(Context.Buf[Word(Context.Size shr 3)], Bytes);
      Context.Size := Context.Size + (Bytes shl 3);
    end;
    ByteIndex := Context.Inx shr 3;
    RawCode := Context.Buf[Word(ByteIndex)] +
      (Word(Context.Buf[Word(ByteIndex + 1)]) shl 8);
    if Context.CodeSize > 8 then
      RawCode := RawCode + (Integer(Context.Buf[ByteIndex + 2]) shl 16);
    RawCode := RawCode shr (Context.Inx and 7);
    Context.Inx := Context.Inx + Byte(Context.CodeSize);
    Result := RawCode and Context.ReadMask;
  end;

  procedure Output(Value: Byte; var Context: TOutputContext);
  var
    P: PByte;
  begin
    if Context.Y >= Context.H then
      Exit;

    // Only ifIndex8 supported
    P := @PByteArray(Context.CurrLineData)[Context.X];
    P^ := Value;

    {case Context.BitsPerPixel of
      1:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X shr 3];
          if (Context.X and $07) <> 0 then
            P^ := P^ or Word(Value shl (7 - (Word(Context.X and 7))))
          else
            P^ := Byte(Value shl 7);
        end;
      4:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X shr 1];
          if (Context.X and 1) <> 0 then
            P^ := P^ or Value
          else
            P^ := Byte(Value shl 4);
        end;
      8:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X];
          P^ := Value;
        end;
    end;}
    Inc(Context.X);

    if Context.X < Context.W then
      Exit;
    Context.X := 0;
    if Context.Interlace then
      Context.Y := InterlaceStep(Context.Y, Context.H, Context.Pass)
    else
      Inc(Context.Y);

    Context.CurrLineData := @PByteArray(Context.Data)[Context.Y * Context.LineIdent];
  end;

begin
  OutCount := 0;
  OldCode := 0;
  FinalChar := 0;
  TableFull := False;
  GetMem(Prefix, SizeOf(TIntCodeTable));
  GetMem(Suffix, SizeOf(TIntCodeTable));
  GetMem(OutCode, SizeOf(TIntCodeTable) + SizeOf(Word));
  try
    Stream.Read(MinCodeSize, 1);
    if (MinCodeSize < 2) or (MinCodeSize > 9) then
      RaiseImaging(SGIFDecodingError, []);
    // Initial read context
    ReadCtxt.Inx := 0;
    ReadCtxt.Size := 0;
    ReadCtxt.CodeSize := MinCodeSize + 1;
    ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
    // Initialize pixel-output context
    OutCtxt.X := 0;
    OutCtxt.Y := 0;
    OutCtxt.Pass := 0;
    OutCtxt.W := Width;
    OutCtxt.H := Height;
    OutCtxt.BitsPerPixel := MinCodeSize;
    OutCtxt.Interlace := Interlaced;
    OutCtxt.LineIdent := Width;
    OutCtxt.Data := Data;
    OutCtxt.CurrLineData := Data;
    BitMask := (1 shl OutCtxt.BitsPerPixel) - 1;
    // 2 ^ MinCodeSize accounts for all colours in file
    ClearCode := 1 shl MinCodeSize;
    EndingCode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    FirstFreeCode := FreeCode;
    // 2^ (MinCodeSize + 1) includes clear and eoi Code and space too
    InitCodeSize := ReadCtxt.CodeSize;
    MaxCode := 1 shl ReadCtxt.CodeSize;
    Code := ReadCode(ReadCtxt);
    while (Code <> EndingCode) and (Code <> $FFFF) and
      (OutCtxt.Y < OutCtxt.H) do
    begin
      if Code = ClearCode then
      begin
        ReadCtxt.CodeSize := InitCodeSize;
        MaxCode := 1 shl ReadCtxt.CodeSize;
        ReadCtxt.ReadMask := MaxCode - 1;
        FreeCode := FirstFreeCode;
        Code := ReadCode(ReadCtxt);
        CurCode := Code;
        OldCode := Code;
        if Code = $FFFF then
          Break;
        FinalChar := (CurCode and BitMask);
        Output(Byte(FinalChar), OutCtxt);
        TableFull := False;
      end
      else
      begin
        CurCode := Code;
        InCode := Code;
        if CurCode >= FreeCode then
        begin
          CurCode := OldCode;
          OutCode^[OutCount] := FinalChar;
          Inc(OutCount);
        end;
        while CurCode > BitMask do
        begin
          if OutCount > CodeTableSize then
            RaiseImaging(SGIFDecodingError, []);
          OutCode^[OutCount] := Suffix^[CurCode];
          Inc(OutCount);
          CurCode := Prefix^[CurCode];
        end;

        FinalChar := CurCode and BitMask;
        OutCode^[OutCount] := FinalChar;
        Inc(OutCount);
        for I := OutCount - 1 downto 0 do
          Output(Byte(OutCode^[I]), OutCtxt);
        OutCount := 0;
        // Update dictionary
        if not TableFull then
        begin
          Prefix^[FreeCode] := OldCode;
          Suffix^[FreeCode] := FinalChar;
          // Advance to next free slot
          Inc(FreeCode);
          if FreeCode >= MaxCode then
          begin
            if ReadCtxt.CodeSize < 12 then
            begin
              Inc(ReadCtxt.CodeSize);
              MaxCode := MaxCode shl 1;
              ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
            end
            else
              TableFull := True;
          end;
        end;
        OldCode := InCode;
      end;
      Code := ReadCode(ReadCtxt);
    end;
    if Code = $FFFF then
      RaiseImaging(SGIFDecodingError, []);
  finally
    FreeMem(Prefix);
    FreeMem(OutCode);
    FreeMem(Suffix);
  end;
end;

{ GIF LZW compression code is from JVCL JvGIF.pas unit.}
procedure TGIFFileFormat.LZWCompress(const IO: TIOFunctions; Handle: TImagingHandle; Width, Height, BitCount: Integer;
    Interlaced: Boolean; Data: Pointer);
var
  LineIdent: Integer;
  MinCodeSize, Col: Byte;
  InitCodeSize, X, Y: Integer;
  Pass: Integer;
  MaxCode: Integer; { 1 shl CodeSize }
  ClearCode, EndingCode, LastCode, Tail: Integer;
  I, HashValue: Integer;
  LenString: Word;
  Dict: PDictTable;
  HashTable: TList;
  PData: PByte;
  WriteCtxt: TWriteContext;

  function InitHash(P: Integer): Integer;
  begin
    Result := (P + 3) * 301;
  end;

  procedure WriteCode(Code: Integer; var Context: TWriteContext);
  var
    BufIndex: Integer;
    Bytes: Byte;
  begin
    BufIndex := Context.Inx shr 3;
    Code := Code shl (Context.Inx and 7);
    Context.Buf[BufIndex] := Context.Buf[BufIndex] or Byte(Code);
    Context.Buf[BufIndex + 1] := Byte(Code shr 8);
    Context.Buf[BufIndex + 2] := Byte(Code shr 16);
    Context.Inx := Context.Inx + Context.CodeSize;
    if Context.Inx >= 255 * 8 then
    begin
      // Flush out full buffer
      Bytes := 255;
      IO.Write(Handle, @Bytes, 1);
      IO.Write(Handle, @Context.Buf, Bytes);
      Move(Context.Buf[255], Context.Buf[0], 2);
      FillChar(Context.Buf[2], 255, 0);
      Context.Inx := Context.Inx - (255 * 8);
    end;
  end;

  procedure FlushCode(var Context: TWriteContext);
  var
    Bytes: Byte;
  begin
    Bytes := (Context.Inx + 7) shr 3;
    if Bytes > 0 then
    begin
      IO.Write(Handle, @Bytes, 1);
      IO.Write(Handle, @Context.Buf, Bytes);
    end;
    // Data block terminator - a block of zero Size
    Bytes := 0;
    IO.Write(Handle, @Bytes, 1);
  end;

begin
  LineIdent := Width;
  Tail := 0;
  HashValue := 0;
  Col := 0;
  HashTable := TList.Create;
  GetMem(Dict, SizeOf(TDictTable));
  try
    for I := 0 to HashTableSize - 1 do
      HashTable.Add(nil);

    // Initialize encoder variables
    InitCodeSize := BitCount + 1;
    if InitCodeSize = 2 then
      Inc(InitCodeSize);
    MinCodeSize := InitCodeSize - 1;
    IO.Write(Handle, @MinCodeSize, 1);
    ClearCode := 1 shl MinCodeSize;
    EndingCode := ClearCode + 1;
    LastCode := EndingCode;
    MaxCode := 1 shl InitCodeSize;
    LenString := 0;
    // Setup write context
    WriteCtxt.Inx := 0;
    WriteCtxt.CodeSize := InitCodeSize;
    FillChar(WriteCtxt.Buf, SizeOf(WriteCtxt.Buf), 0);
    WriteCode(ClearCode, WriteCtxt);
    Y := 0;
    Pass := 0;

    while Y < Height do
    begin
      PData := @PByteArray(Data)[Y * LineIdent];
      for X := 0 to Width - 1 do
      begin
        // Only ifIndex8 support
        case BitCount of
          8:
            begin
              Col := PData^;
              PData := @PByteArray(PData)[1];
            end;
          {4:
            begin
              if X and 1 <> 0 then
              begin
                Col := PData^ and $0F;
                PData := @PByteArray(PData)[1];
              end
              else
                Col := PData^ shr 4;
            end;
          1:
            begin
              if X and 7 = 7 then
              begin
                Col := PData^ and 1;
                PData := @PByteArray(PData)[1];
              end
              else
                Col := (PData^ shr (7 - (X and $07))) and $01;
            end;}
        end;
        Inc(LenString);
        if LenString = 1 then
        begin
          Tail := Col;
          HashValue := InitHash(Col);
        end
        else
        begin
          HashValue := HashValue * (Col + LenString + 4);
          I := HashValue mod HashTableSize;
          HashValue := HashValue mod HashTableSize;
          while (HashTable[I] <> nil) and
            ((PImageDict(HashTable[I])^.Tail <> Tail) or
            (PImageDict(HashTable[I])^.Col <> Col)) do
          begin
            Inc(I);
            if I >= HashTableSize then
              I := 0;
          end;
          if HashTable[I] <> nil then // Found in the strings table
            Tail := PImageDict(HashTable[I])^.Index
          else
          begin
            // Not found
            WriteCode(Tail, WriteCtxt);
            Inc(LastCode);
            HashTable[I] := @Dict^[LastCode];
            PImageDict(HashTable[I])^.Index := LastCode;
            PImageDict(HashTable[I])^.Tail := Tail;
            PImageDict(HashTable[I])^.Col := Col;
            Tail := Col;
            HashValue := InitHash(Col);
            LenString := 1;
            if LastCode >= MaxCode then
            begin
              // Next Code will be written longer
              MaxCode := MaxCode shl 1;
              Inc(WriteCtxt.CodeSize);
            end
            else
            if LastCode >= CodeTableSize - 2 then
            begin
              // Reset tables
              WriteCode(Tail, WriteCtxt);
              WriteCode(ClearCode, WriteCtxt);
              LenString := 0;
              LastCode := EndingCode;
              WriteCtxt.CodeSize := InitCodeSize;
              MaxCode := 1 shl InitCodeSize;
              for I := 0 to HashTableSize - 1 do
                HashTable[I] := nil;
            end;
          end;
        end;
      end;
      if Interlaced then
        Y := InterlaceStep(Y, Height, Pass)
      else
        Inc(Y);
    end;
    WriteCode(Tail, WriteCtxt);
    WriteCode(EndingCode, WriteCtxt);
    FlushCode(WriteCtxt);
  finally
    HashTable.Free;
    FreeMem(Dict);
  end;
end;

function TGIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
type
  TFrameInfo = record
    Left, Top: Integer;
    Width, Height: Integer;
    Disposal: TDisposalMethod;
    HasTransparency: Boolean;
    HasLocalPal: Boolean;
    TransIndex: Integer;
    BackIndex: Integer;
  end;
var
  Header: TGIFHeader;
  HasGlobalPal: Boolean;
  GlobalPalLength: Integer;
  GlobalPal: TPalette32Size256;
  ScreenWidth, ScreenHeight, I, CachedIndex: Integer;
  BlockID: Byte;
  HasGraphicExt: Boolean;
  GraphicExt: TGraphicControlExtension;
  FrameInfos: array of TFrameInfo;
  AppRead: Boolean;
  CachedFrame: TImageData;
  AnimFrames: TDynImageDataArray;

  function ReadBlockID: Byte;
  begin
    Result := GIFTrailer;
    if GetIO.Read(Handle, @Result, SizeOf(Result)) < SizeOf(Result) then
      Result := GIFTrailer;
  end;

  procedure ReadExtensions;
  var
    BlockSize, BlockType, ExtType: Byte;
    AppRec: TGIFApplicationRec;
    LoopCount: SmallInt;

    procedure SkipBytes;
    begin
      with GetIO do
      repeat
        // Read block sizes and skip them
        Read(Handle, @BlockSize, SizeOf(BlockSize));
        Seek(Handle, BlockSize, smFromCurrent);
      until BlockSize = 0;
    end;

  begin
    HasGraphicExt := False;
    AppRead := False;

    // Read extensions until image descriptor is found. Only graphic extension
    // is stored now (for transparency), others are skipped.
    while BlockID = GIFExtensionIntroducer do
    with GetIO do
    begin
      Read(Handle, @ExtType, SizeOf(ExtType));

      while ExtType in [GIFGraphicControlExtension, GIFCommentExtension, GIFApplicationExtension, GIFPlainText] do
      begin
        if ExtType = GIFGraphicControlExtension then
        begin
          HasGraphicExt := True;
          Read(Handle, @GraphicExt, SizeOf(GraphicExt));
        end
        else if (ExtType = GIFApplicationExtension) and not AppRead then
        begin
          Read(Handle, @BlockSize, SizeOf(BlockSize));
          if BlockSize >= SizeOf(AppRec) then
          begin
            Read(Handle, @AppRec, SizeOf(AppRec));
            if ((AppRec.Identifier = 'NETSCAPE') and (AppRec.Authentication = '2.0')) or
              ((AppRec.Identifier = 'ANIMEXTS') and (AppRec.Authentication = '1.0')) then
            begin
              Read(Handle, @BlockSize, SizeOf(BlockSize));
              while BlockSize <> 0 do
              begin
                BlockType := ReadBlockID;
                Dec(BlockSize);

                case BlockType of
                  GIFAppLoopExtension:
                    if (BlockSize >= SizeOf(LoopCount)) then
                    begin
                      // Read loop count
                      Read(Handle, @LoopCount, SizeOf(LoopCount));
                      Dec(BlockSize, SizeOf(LoopCount));
                      if LoopCount > 0 then
                        Inc(LoopCount); // Netscape extension is really "repeats" not "loops"
                      FMetadata.SetMetaItem(SMetaAnimationLoops, LoopCount);
                    end;
                  GIFAppBufferExtension:
                    begin
                      Dec(BlockSize, SizeOf(Word));
                      Seek(Handle, SizeOf(Word), smFromCurrent);
                    end;
                end;
              end;
              SkipBytes;
              AppRead := True;
            end
            else
              begin
                // Revert all bytes reading
                Seek(Handle, - SizeOf(AppRec) - SizeOf(BlockSize), smFromCurrent);
                SkipBytes;
              end;
          end
          else
            begin
              Seek(Handle, - BlockSize - SizeOf(BlockSize), smFromCurrent);
              SkipBytes;
            end;
        end
        else if ExtType in [GIFCommentExtension, GIFApplicationExtension, GIFPlainText] then
        repeat
          // Read block sizes and skip them
          Read(Handle, @BlockSize, SizeOf(BlockSize));
          Seek(Handle, BlockSize, smFromCurrent);
        until BlockSize = 0;

        // Read ID of following block
        BlockID := ReadBlockID;
        ExtType := BlockID;
      end
    end;
  end;

  procedure CopyLZWData(Dest: TStream);
  var
    CodeSize, BlockSize: Byte;
    InputSize: Integer;
    Buff: array[Byte] of Byte;
  begin
    InputSize := ImagingIO.GetInputSize(GetIO, Handle);
    // Copy codesize to stream
    GetIO.Read(Handle, @CodeSize, 1);
    Dest.Write(CodeSize, 1);
    repeat
      // Read and write data blocks, last is block term value of 0
      GetIO.Read(Handle, @BlockSize, 1);
      Dest.Write(BlockSize, 1);
      if BlockSize > 0 then
      begin
        GetIO.Read(Handle, @Buff[0], BlockSize);
        Dest.Write(Buff[0], BlockSize);
      end;
    until (BlockSize = 0) or (GetIO.Tell(Handle) >= InputSize);
  end;

  procedure ReadFrame;
  var
    ImageDesc: TImageDescriptor;
    Interlaced: Boolean;
    I, Idx, LocalPalLength: Integer;
    LocalPal: TPalette32Size256;
    LZWStream: TMemoryStream;

    procedure RemoveBadFrame;
    begin
      FreeImage(Images[Idx]);
      SetLength(Images, Length(Images) - 1);
    end;

  begin
    Idx := Length(Images);
    SetLength(Images, Idx + 1);
    SetLength(FrameInfos, Idx + 1);
    FillChar(LocalPal, SizeOf(LocalPal), 0);

    with GetIO do
    begin
      // Read and parse image descriptor
      Read(Handle, @ImageDesc, SizeOf(ImageDesc));
      FrameInfos[Idx].HasLocalPal := (ImageDesc.PackedFields and GIFLocalColorTable) = GIFLocalColorTable;
      Interlaced := (ImageDesc.PackedFields and GIFInterlaced) = GIFInterlaced;
      LocalPalLength := ImageDesc.PackedFields and GIFColorTableSize;
      LocalPalLength := 1 shl (LocalPalLength + 1);   // Total pal length is 2^(n+1)

      // From Mozilla source
      if (ImageDesc.Width = 0) or (ImageDesc.Width > Header.ScreenWidth) then
        ImageDesc.Width := Header.ScreenWidth;
      if (ImageDesc.Height = 0) or (ImageDesc.Height > Header.ScreenHeight)  then
        ImageDesc.Height := Header.ScreenHeight;

      FrameInfos[Idx].Left := ImageDesc.Left;
      FrameInfos[Idx].Top := ImageDesc.Top;
      FrameInfos[Idx].Width := ImageDesc.Width;
      FrameInfos[Idx].Height := ImageDesc.Height;
      FrameInfos[Idx].BackIndex := Header.BackgroundColorIndex;

      // Create new image for this frame which would be later pasted onto logical screen
      NewImage(ImageDesc.Width, ImageDesc.Height, ifIndex8, Images[Idx]);

      // Load local palette if there is any
      if FrameInfos[Idx].HasLocalPal then
        for I := 0 to LocalPalLength - 1 do
        begin
          LocalPal[I].A := 255;
          Read(Handle, @LocalPal[I].R, SizeOf(LocalPal[I].R));
          Read(Handle, @LocalPal[I].G, SizeOf(LocalPal[I].G));
          Read(Handle, @LocalPal[I].B, SizeOf(LocalPal[I].B));
        end;

      // Use local pal if present or global pal if present or create
      // default pal if neither of them is present
      if FrameInfos[Idx].HasLocalPal then
        Move(LocalPal, Images[Idx].Palette^, SizeOf(LocalPal))
      else if HasGlobalPal then
        Move(GlobalPal, Images[Idx].Palette^, SizeOf(GlobalPal))
      else
        FillCustomPalette(Images[Idx].Palette, GlobalPalLength, 3, 3, 2);

      if (ImageDesc.Left <= Header.ScreenWidth + 1) and (ImageDesc.Top <= Header.ScreenHeight + 1) then
      begin
        // Resize the screen if needed to fit the frame
        ScreenWidth := Max(ScreenWidth, ImageDesc.Width + ImageDesc.Left);
        ScreenHeight := Max(ScreenHeight, ImageDesc.Height + ImageDesc.Top);
      end
      else
      begin
        // Remove frame outside logical screen
        RemoveBadFrame;
        Exit;
      end;

      // If Graphic Control Extension is present make use of it
      if HasGraphicExt then
      begin
        FrameInfos[Idx].HasTransparency := (GraphicExt.PackedFields and GIFTransparent) = GIFTransparent;
        FrameInfos[Idx].Disposal := TDisposalMethod((GraphicExt.PackedFields and GIFDisposalMethod) shr 2);
        if FrameInfos[Idx].HasTransparency then
        begin
          FrameInfos[Idx].TransIndex := GraphicExt.TransparentColorIndex;
          Images[Idx].Palette[FrameInfos[Idx].TransIndex].A := 0;
        end;
        FMetadata.SetMetaItem(SMetaFrameDelay, Integer(GraphicExt.DelayTime * 10), Idx);
      end
      else
        FrameInfos[Idx].HasTransparency := False;

      LZWStream := TMemoryStream.Create;
      try
        try
          // Copy LZW data to temp stream, needed for correct decompression
          CopyLZWData(LZWStream);
          LZWStream.Position := 0;
          // Data decompression finally
          LZWDecompress(LZWStream, Handle, ImageDesc.Width, ImageDesc.Height, Interlaced, Images[Idx].Bits);
        except
          RemoveBadFrame;
          Exit;
        end;
      finally
        LZWStream.Free;
      end;
    end;
  end;

  procedure CopyFrameTransparent32(const Image, Frame: TImageData; Left, Top: Integer);
  var
    X, Y: Integer;
    Src: PByte;
    Dst: PColor32;
  begin
    Src := Frame.Bits;

    // Copy all pixels from frame to log screen but ignore the transparent ones
    for Y := 0 to Frame.Height - 1 do
    begin
      Dst := @PColor32RecArray(Image.Bits)[(Top + Y) * Image.Width + Left];
      for X := 0 to Frame.Width - 1 do
      begin
        if (Frame.Palette[Src^].A <> 0) then
          Dst^ := Frame.Palette[Src^].Color;
        Inc(Src);
        Inc(Dst);
      end;
    end;
  end;

  procedure AnimateFrame(Index: Integer; var AnimFrame: TImageData);
  var
    I, First, Last: Integer;
    UseCache: Boolean;
    BGColor: TColor32;
  begin
    // We may need to use raw frame 0 to n to correctly animate n-th frame
    Last := Index;
    First := Max(0, Last);
    // See if we can use last animate frame as a basis for this one
    // (so we don't have to use previous raw frames).
    UseCache := TestImage(CachedFrame) and (CachedIndex = Index - 1) and (CachedIndex >= 0) and
      (FrameInfos[CachedIndex].Disposal <> dmRestorePrevious);

    // Reuse or release cache
    if UseCache then
      CloneImage(CachedFrame, AnimFrame)
    else
      FreeImage(CachedFrame);

    // Default color for clearing of the screen
    BGColor := Images[Index].Palette[FrameInfos[Index].BackIndex].Color;

    // Now prepare logical screen for drawing of raw frame at Index.
    // We may need to use all previous raw frames to get the screen
    // to proper state (according to their disposal methods).

    if not UseCache then
    begin
      if FrameInfos[Index].HasTransparency then
        BGColor := Images[Index].Palette[FrameInfos[Index].TransIndex].Color;
      // Clear whole screen
      FillMemoryUInt32(AnimFrame.Bits, AnimFrame.Size, BGColor);

      // Try to maximize First so we don't have to use all 0 to n raw frames
      while First > 0 do
      begin
        if (ScreenWidth = Images[First].Width) and (ScreenHeight = Images[First].Height) then
        begin
          if (FrameInfos[First].Disposal = dmRestoreBackground) and (First < Last) then
            Break;
        end;
        Dec(First);
      end;

      for I := First to Last - 1 do
      begin
        case FrameInfos[I].Disposal of
          dmNoRemoval, dmLeave:
            begin
              // Copy previous raw frame  onto screen
              CopyFrameTransparent32(AnimFrame, Images[I], FrameInfos[I].Left, FrameInfos[I].Top);
            end;
          dmRestoreBackground:
            if (I > First) then
            begin
              // Restore background color
              FillRect(AnimFrame, FrameInfos[I].Left, FrameInfos[I].Top,
                FrameInfos[I].Width, FrameInfos[I].Height, @BGColor);
            end;
          dmRestorePrevious: ; // Do nothing - previous state is already on screen
        end;
      end;
    end
    else if FrameInfos[CachedIndex].Disposal = dmRestoreBackground then
    begin
      // We have our cached result but also need to restore
      // background in a place of cached frame
      if FrameInfos[CachedIndex].HasTransparency then
        BGColor := Images[CachedIndex].Palette[FrameInfos[CachedIndex].TransIndex].Color;
      FillRect(AnimFrame, FrameInfos[CachedIndex].Left, FrameInfos[CachedIndex].Top,
        FrameInfos[CachedIndex].Width, FrameInfos[CachedIndex].Height, @BGColor);
    end;

    // Copy current raw frame to prepared screen
    CopyFrameTransparent32(AnimFrame, Images[Index], FrameInfos[Index].Left, FrameInfos[Index].Top);

    // Cache animated result
    CloneImage(AnimFrame, CachedFrame);
    CachedIndex := Index;
  end;

begin
  AppRead := False;

  SetLength(Images, 0);
  FillChar(GlobalPal, SizeOf(GlobalPal), 0);

  with GetIO do
  begin
    // Read GIF header
    Read(Handle, @Header, SizeOf(Header));
    ScreenWidth := Header.ScreenWidth;
    ScreenHeight := Header.ScreenHeight;
    HasGlobalPal := Header.PackedFields and GIFGlobalColorTable = GIFGlobalColorTable; // Bit 7
    GlobalPalLength := Header.PackedFields and GIFColorTableSize; // Bits 0-2
    GlobalPalLength := 1 shl (GlobalPalLength + 1);   // Total pal length is 2^(n+1)

    // Read global palette from file if present
    if HasGlobalPal then
    begin
      for I := 0 to GlobalPalLength - 1 do
      begin
        GlobalPal[I].A := 255;
        Read(Handle, @GlobalPal[I].R, SizeOf(GlobalPal[I].R));
        Read(Handle, @GlobalPal[I].G, SizeOf(GlobalPal[I].G));
        Read(Handle, @GlobalPal[I].B, SizeOf(GlobalPal[I].B));
      end;
    end;

    // Read ID of the first block
    BlockID := ReadBlockID;

    // Now read all data blocks in the file until file trailer is reached
    while BlockID <> GIFTrailer do
    begin
      // Read blocks until we find the one of known type
      while not (BlockID in [GIFTrailer, GIFExtensionIntroducer, GIFImageDescriptor]) do
        BlockID := ReadBlockID;
      // Read supported and skip unsupported extensions
      ReadExtensions;
      // If image frame is found read it
      if BlockID = GIFImageDescriptor then
        ReadFrame;
      // Read next block's ID
      BlockID := ReadBlockID;
      // If block ID is unknown set it to end-of-GIF marker
      if not (BlockID in [GIFExtensionIntroducer, GIFTrailer, GIFImageDescriptor]) then
        BlockID := GIFTrailer;
    end;

    if FLoadAnimated then
    begin
      // Aniated frames will be stored in AnimFrames
      SetLength(AnimFrames, Length(Images));
      InitImage(CachedFrame);
      CachedIndex := -1;

      for I := 0 to High(Images) do
      begin
        // Create new logical screen
        NewImage(ScreenWidth, ScreenHeight, ifA8R8G8B8, AnimFrames[I]);
        // Animate frames to current log screen
        AnimateFrame(I, AnimFrames[I]);
      end;

      // Now release raw 8bit frames and put animated 32bit ones
      // to output array
      FreeImage(CachedFrame);
      for I := 0 to High(AnimFrames) do
      begin
        FreeImage(Images[I]);
        Images[I] := AnimFrames[I];
      end;
    end;

    Result := True;
  end;
end;

function TGIFFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Header: TGIFHeader;
  ImageDesc: TImageDescriptor;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  I, J: Integer;
  GraphicExt: TGraphicControlExtension;

  procedure FindMaxDimensions(var MaxWidth, MaxHeight: Word);
  var
    I: Integer;
  begin
    MaxWidth := Images[FFirstIdx].Width;
    MaxHeight := Images[FFirstIdx].Height;

    for I := FFirstIdx + 1 to FLastIdx do
    begin
      MaxWidth := Iff(Images[I].Width > MaxWidth, Images[I].Width, MaxWidth);
      MaxHeight := Iff(Images[I].Height > MaxWidth, Images[I].Height, MaxHeight);
    end;
  end;

  procedure SetFrameDelay(Idx: Integer; var Ext: TGraphicControlExtension);
  begin
    if FMetadata.HasMetaItemForSaving(SMetaFrameDelay, Idx) then
      Ext.DelayTime := FMetadata.MetaItemsForSavingMulti[SMetaFrameDelay, Idx] div 10
    else
      Ext.DelayTime := GIFDefaultDelay;
  end;

  procedure SaveGlobalMetadata;
  var
    AppExt: TGIFApplicationRec;
    BlockSize, LoopExtId: Byte;
    Repeats: Word;
  begin
    if FMetadata.HasMetaItemForSaving(SMetaAnimationLoops) then
    with GetIO do
    begin
      FillChar(AppExt, SizeOf(AppExt), 0);
      AppExt.Identifier := 'NETSCAPE';
      AppExt.Authentication := '2.0';
      Repeats := FMetadata.MetaItemsForSaving[SMetaAnimationLoops];
      if Repeats > 0 then
        Dec(Repeats);
      LoopExtId := GIFAppLoopExtension;

      Write(Handle, @GIFExtensionIntroducer, SizeOf(GIFExtensionIntroducer));
      Write(Handle, @GIFApplicationExtension, SizeOf(GIFApplicationExtension));
      BlockSize := 11;
      Write(Handle, @BlockSize, SizeOf(BlockSize));
      Write(Handle, @AppExt, SizeOf(AppExt));
      BlockSize := 3;
      Write(Handle, @BlockSize, SizeOf(BlockSize));
      Write(Handle, @LoopExtId, SizeOf(LoopExtId));
      Write(Handle, @Repeats, SizeOf(Repeats));
      Write(Handle, @GIFBlockTerminator, SizeOf(GIFBlockTerminator));
    end;
  end;

begin
  // Fill header with data, select size of largest image in array as
  // logical screen size
  FillChar(Header, Sizeof(Header), 0);
  Header.Signature := GIFSignature;
  Header.Version := GIFVersions[gv89];
  FindMaxDimensions(Header.ScreenWidth, Header.ScreenHeight);
  Header.PackedFields := GIFColorResolution; // Color resolution is 256
  GetIO.Write(Handle, @Header, SizeOf(Header));

  // Prepare default GC extension with delay
  FillChar(GraphicExt, Sizeof(GraphicExt), 0);
  GraphicExt.DelayTime := GIFDefaultDelay;
  GraphicExt.BlockSize := 4;

  SaveGlobalMetadata;

  for I := FFirstIdx to FLastIdx do
  begin
    if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
    with GetIO, ImageToSave do
    try
      // Write Graphic Control Extension with default delay
      Write(Handle, @GIFExtensionIntroducer, SizeOf(GIFExtensionIntroducer));
      Write(Handle, @GIFGraphicControlExtension, SizeOf(GIFGraphicControlExtension));
      SetFrameDelay(I, GraphicExt);
      Write(Handle, @GraphicExt, SizeOf(GraphicExt));
      // Write frame marker and fill and write image descriptor for this frame
      Write(Handle, @GIFImageDescriptor, SizeOf(GIFImageDescriptor));
      FillChar(ImageDesc, Sizeof(ImageDesc), 0);
      ImageDesc.Width := Width;
      ImageDesc.Height := Height;
      ImageDesc.PackedFields := GIFLocalColorTable or GIFColorTableSize; // Use local color table with 256 entries
      Write(Handle, @ImageDesc, SizeOf(ImageDesc));

      // Write local color table for each frame
      for J := 0 to 255 do
      begin
        Write(Handle, @Palette[J].R, SizeOf(Palette[J].R));
        Write(Handle, @Palette[J].G, SizeOf(Palette[J].G));
        Write(Handle, @Palette[J].B, SizeOf(Palette[J].B));
      end;

      // Finally compress image data
      LZWCompress(GetIO, Handle, Width, Height, 8, False, Bits);

    finally
      if MustBeFreed then
        FreeImage(ImageToSave);
    end;
  end;

  GetIO.Write(Handle, @GIFTrailer, SizeOf(GIFTrailer));
  Result := True;
end;

procedure TGIFFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  ConvertImage(Image, ifIndex8);
end;

function TGIFFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Header: TGIFHeader;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Header, SizeOf(Header));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Header)) and
      (Header.Signature = GIFSignature) and
      ((Header.Version = GIFVersions[gv87]) or (Header.Version = GIFVersions[gv89]));
  end;
end;

initialization
  RegisterImageFileFormat(TGIFFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77 Changes/Bug Fixes -----------------------------------
    - Fixed crash when resaving GIF with animation metadata.
    - Writes frame delays of GIF animations from metadata.
    - Reads and writes looping of GIF animations stored into/from metadata.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Reads frame delays from GIF animations into metadata.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Fixed bug - loading of GIF with NETSCAPE app extensions
      failed with Delphi 2009.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - GIF loading and animation mostly rewritten, based on
      modification by Sergey Galezdinov (ExtraGIF in Extras/Contrib).

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Fixed loading of some rare GIFs, problems with LZW
      decompression.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Better solution to transparency for some GIFs. Background not
      transparent by default.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Made background color transparent by default (alpha = 0).

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Fixed other loading bugs (local pal size, transparency).
    - Added GIF saving.
    - Fixed bug when loading multi-frame GIFs and implemented few animation
      features (disposal methods, ...). 
    - Loading of GIFs working.
    - Unit created with initial stuff!
}

end.
