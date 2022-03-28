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

{ This unit contains image format loader for textures and images
  from Redguard and BattleSpire.}
unit ElderImageryBsi;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ElderImagery, ImagingUtility;

type
  { Class for loading of BSI format textures and images found
    in Redguard and BattleSpire (maybe in other games too, Skynet?). This format
    uses chunk structure similar to PNG (HDR/DAT/END).
    BattleSpire BSI use *.bsi file extension whilst Redguard uses
    texbsi.* mask with number extension (just like Daggerfall).
    Only loading is supported for this format.

    Redguard stores multiple images in one file (usually related like textures for various
    parts of single 3d object). Image data is stored as 8bit. Each image
    can have its own embedded palette or it can use external palette.
    Default palette (fxart\Redguard.col) is applied to textures without the embedded one
    although some texture sets look like their external pal is different (see more *.col
    files in fxart).

    BattleSpire uses 15bit palette and some lighting data, also some of the images
    are RLE compressed. Multiple frames can also be stored in BSI
    (multiple frames vs images in Redguard). BSI in BattleSpire are stored in BSA archive -> this
    version of BSA support compression of files so you need BSA extractor
    which takes BSpire version in account.
    Working BSA extractor and BSI description: https://github.com/ariscop/battlespire-tools.}
  TBSIFileFormat = class(TElderFileFormat)
  private
    function IsMultiBSI(Handle: TImagingHandle): Boolean;
    procedure ConvertHICLToPalette(HICL: PWordArray; Pal: PPalette32);
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SBSIFormatName = 'Bethesda Image';
  SBSIMasks      = '*.bsi,texbsi.*';

resourcestring
  SErrorLoadingChunk = 'Error when reading %s chunk data.';

type
  { BSI chunk header.}
  TChunk = packed record
    ChunkID: TChar4;
    DataSize: UInt32; // In Big Endian!
  end;

  { Additional header of BSI textures.}
  TTextureBSIHeader = packed record
    Name: array[0..8] of AnsiChar;
    ImageSize: UInt32;
  end;

  { Main image info header located in BHDR chunk's data.}
  TBHDRChunk = packed record
    OffsetX: Int16;
    OffsetY: Int16;
    Width: Int16;
    Height: Int16;
    Unk1, Unk2: Byte;
    Unk3, Unk4: Word;
    Frames: Word;
    Unk6, Unk7, Unk8: Word;
    Unk9, Unk10: Byte;
    Compression: Word;
  end;

const
  IFHDSignature: TChar4 = 'IFHD';
  BSIFSignature: TChar4 = 'BSIF';
  BHDRSignature: TChar4 = 'BHDR';
  CMAPSignature: TChar4 = 'CMAP';
  HICLSignature: TChar4 = 'HICL';
  HTBLSignature: TChar4 = 'HTBL';
  DATASignature: TChar4 = 'DATA';
  ENDSignature:  TChar4 = 'END ';

{ TBSIFileFormat class implementation }

procedure TBSIFileFormat.Define;
begin
  inherited;
  FName := SBSIFormatName;
  FFeatures := [ffLoad, ffMultiImage];

  AddMasks(SBSIMasks);
  SetPalette(RedguardPalette);
end;

function TBSIFileFormat.IsMultiBSI(Handle: TImagingHandle): Boolean;
var
  ReadCount, StartPos: LongInt;
  Sig: TChar4;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    StartPos := Tell(Handle);
    // Redguard textures have 13 byte tex header and then IFHD or BSIF
    Seek(Handle, SizeOf(TTextureBSIHeader), smFromCurrent);
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, StartPos, smFromBeginning);
    Result := Result or ((ReadCount = SizeOf(Sig)) and
      ((Sig = IFHDSignature) or (Sig = BSIFSignature)));
  end;
end;

procedure TBSIFileFormat.ConvertHICLToPalette(HICL: PWordArray; Pal: PPalette32);
var
  I, Idx: Integer;
  Col: Word;
begin
  for I := 0 to 127 do
  begin
    // HICL is 256B in size with 128 word colors (R5G5B5).
    // Indices in DATA chunk are scaled to full 8 bits.
    // So either multiply pal entries by 2 or divide every index in DATA by 2 =>
    // since 128 <<< size(DATA) we modify the palette.
    Col := HICL[I];
    Idx := I * 2;
    Pal[Idx].A := 255;
    Pal[Idx].R := MulDiv(((Col shr 11) and 31), 255, 31);
    Pal[Idx].G := MulDiv(((Col shr 6) and 31), 255, 31);
    Pal[Idx].B := MulDiv(((Col shr 1) and 31), 255, 31);
  end;
end;

function TBSIFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Chunk: TChunk;
  ChunkData: Pointer;
  BHDR: TBHDRChunk;
  PalLoaded: TPalette24Size256;
  HICL: PWordArray;
  HTBL: PByteArray;
  IsMulti: Boolean;
  TextureHdr: TTextureBSIHeader;
  PaletteFound: Boolean;

  procedure ReadChunk;
  begin
    GetIO.Read(Handle, @Chunk, SizeOf(Chunk));
    Chunk.DataSize := SwapEndianUInt32(Chunk.DataSize);
  end;

  procedure ReadChunkData;
  var
    ReadBytes: LongInt;
  begin
    FreeMemNil(ChunkData);
    GetMem(ChunkData, Chunk.DataSize);
    ReadBytes := GetIO.Read(Handle, ChunkData, Chunk.DataSize);
    if ReadBytes <> Chunk.DataSize then
      RaiseImaging(SErrorLoadingChunk, [Chunk.ChunkID]);
  end;

  procedure SkipChunkData;
  begin
    GetIO.Seek(Handle, Chunk.DataSize, smFromCurrent);
  end;

  procedure GetBHDR;
  begin
    ReadChunkData;
    BHDR := TBHDRChunk(ChunkData^);
  end;

  procedure GetHICL;
  begin
    ReadChunkData;
    GetMem(HICL, Chunk.DataSize);
    Move(ChunkData^, HICL[0], Chunk.DataSize);
  end;

  procedure GetHTBL;
  begin
    ReadChunkData;
    GetMem(HTBL, Chunk.DataSize);
    Move(ChunkData^, HTBL[0], Chunk.DataSize);
  end;

  procedure GetCMAP;
  begin
    ReadChunkData;
    Move(ChunkData^, PalLoaded, Chunk.DataSize);
    PaletteFound := True;
  end;

  procedure GetDATA;
  begin
    ReadChunkData;
  end;

  function AddImage(Width, Height: LongInt): Integer;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    if not PaletteFound then
      Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec))
    else
      ConvertPalette(PalLoaded, Images[Result].Palette);
  end;

  function AddImageHiColor(Width, Height: LongInt; HICL: PWordArray): Integer;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    ConvertHICLToPalette(HICL, Images[Result].Palette);
  end;

  procedure Reconstruct;
  var
    ImgIndex, I, J, K: Integer;
    RowOffsets: PUInt32Array;
    RowPtr: PByte;
    Offset: UInt32;
    Idx, C: Byte;
    IsRleLine, IsRleRun: Boolean;
    DestLine: PByte;
    Ix, Ir: Integer;
  begin
    if HICL = nil then
    begin
      // No HICL data => mostly Redguard images
      if BHDR.Frames = 1 then
      begin
        // Load single image
        ImgIndex := AddImage(BHDR.Width, BHDR.Height);
        Move(ChunkData^, Images[ImgIndex].Bits^, Images[ImgIndex].Size);
      end
      else
      begin
        // Load animated image:
        // At the beginning of the chunk data there is BHDR.Height * BHDR.Frames
        // 32bit offsets. Each BHDR.Height offsets point to rows of the current frame
        RowOffsets := PUInt32Array(ChunkData);

        for I := 0 to BHDR.Frames - 1 do
        begin
          ImgIndex := AddImage(BHDR.Width, BHDR.Height);
          with Images[ImgIndex] do
          for J := 0 to BHDR.Height - 1 do
            Move(PByteArray(ChunkData)[RowOffsets[I * BHDR.Height + J]],
              PByteArray(Bits)[J * Width], Width);
        end;
      end;
    end
    else
    begin
      if BHDR.Frames = 1 then
      begin
        // Load single image
        ImgIndex := AddImageHiColor(BHDR.Width, BHDR.Height, HICL);
        Move(ChunkData^, Images[ImgIndex].Bits^, Images[ImgIndex].Size);
      end
      else
      begin
        // Load animated BattleSpire image, uses offset list just like Redguard
        // animated textures (but high word must be zeroed first to get valid offset).
        // Frames can also be RLE compressed.
        RowOffsets := PUInt32Array(ChunkData);

        for I := 0 to BHDR.Frames - 1 do
        begin
          ImgIndex := AddImageHiColor(BHDR.Width, BHDR.Height, HICL);

          if BHDR.Compression = 0 then
          begin
            with Images[ImgIndex] do
            for J := 0 to BHDR.Height - 1 do
              for K := 0 to BHDR.Width - 1 do
              begin
                Idx := PByteArray(ChunkData)[RowOffsets[I * BHDR.Height + J] and $FFFF + K];
                PByteArray(Bits)[J * Width + K] := Idx;
              end;
          end
          else
          begin
            with Images[ImgIndex] do
            for J := 0 to BHDR.Height - 1 do
            begin
              Offset := RowOffsets[I * BHDR.Height + J];
              IsRleLine := (Offset and $80000000) = $80000000;
              Offset := Offset and $FFFFFF;
              RowPtr := @PByteArray(ChunkData)[Offset];
              DestLine := @PByteArray(Bits)[J * BHDR.Width];

              if not IsRleLine then
              begin
                Move(PByteArray(ChunkData)[Offset], PByteArray(Bits)[J * Width], Width);
              end
              else
              begin
                Ix := 0;
                while Ix < Width do
                begin
                  C := RowPtr^;
                  Inc(RowPtr);

                  IsRleRun := C >= 128;
                  C := C and 127;
                  if IsRleRun then
                  begin
                    Idx := RowPtr^;
                    Inc(RowPtr);

                    for Ir := 1 to C do
                    begin
                      DestLine^ := Idx;
                      Inc(DestLine);
                    end;
                  end
                  else
                  begin
                    for Ir := 1 to C do
                    begin
                      Idx := RowPtr^;
                      Inc(RowPtr);

                      DestLine^ := Idx;
                      Inc(DestLine);
                    end;
                  end;

                  Inc(Ix, C);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ReadTextureHeader;
  begin
    FillChar(TextureHdr, SizeOf(TextureHdr), 0);
    if IsMulti then
      GetIO.Read(Handle, @TextureHdr, SizeOf(TextureHdr))
    else if Length(Images) = 0 then
      // Ensure that while loop that reads chunks is executed for
      // single-image files
      TextureHdr.ImageSize := 1;
  end;

begin
  ChunkData := nil;
  HICL := nil;
  HTBL := nil;
  SetLength(Images, 0);
  IsMulti := IsMultiBSI(Handle);
  with GetIO do
  begin
    // Redguard textures can contain more than one image. Try to read texture
    // header and if ImageSize is >0 there is another image.
    ReadTextureHeader;
    while TextureHdr.ImageSize > 0 do
    try
      PaletteFound := False;
      ReadChunk;
      SkipChunkData;
      // Read data chunks. If they are recognized their data is stored for
      // later image reconstruction
      repeat
        ReadChunk;
        if Chunk.ChunkID = BHDRSignature then
          GetBHDR
        else if Chunk.ChunkID = HICLSignature then
          GetHICL
        else if Chunk.ChunkID = HTBLSignature then
          GetHTBL
        else if Chunk.ChunkID = CMAPSignature then
          GetCMAP
        else if Chunk.ChunkID = DATASignature then
          GetDATA
        else
          SkipChunkData;
      until Eof(Handle) or (Chunk.ChunkID = ENDSignature);
      // Reconstruct current image according to data read from chunks
      Reconstruct;
      // Read header for next image
      ReadTextureHeader;
    finally
      FreeMemNil(ChunkData);
      FreeMemNil(HICL);
      FreeMemNil(HTBL);
    end;
    Result := True;
  end;
end;

function TBSIFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  Sig: TChar4;
begin
  // First check if have multi-image BSI file (Redguard textures)
  Result := IsMultiBSI(Handle);
  if not Result and (Handle <> nil) then
  with GetIO do
  begin
    // Check standard Bettlespire images with IFHD chunk at
    // the beginning of the file
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Sig)) and (Sig = IFHDSignature);
  end;
end;


{
  Changes/Bug Fixes:

  -- 0.80 -----------------------------------------------------
    - BattleSpire images now have correct colors.

  -- 0.21 -----------------------------------------------------
    - Blue channel of BattleSpire images cracked but others are still unknown.
    - Added support for animated BattleSpire images.
    - Added support for animated Redguard textures.
    - Added support for Redguard textures (Battlespire images still don't figured out).
    - Updated to current Imaging version.

  -- 0.13 -----------------------------------------------------
    - TBSIFileFormat class added

}

end.
