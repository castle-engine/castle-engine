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

{ This unit contains image format loader of Daggerfall texture file format.}
unit ElderImageryTexture;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ElderImagery, ImagingIO, ImagingUtility;

type
  { Class that provides loading of textures from TES2: Daggerfall
    (works for Terminator: FS and maybe other games too).
    Textures are stored in 8bit indexed format with external palette.
    This format is very complicated (more images with subimages,
    non-standard RLE, many unknowns) so module supports only loading.
    These texture files cannot be recognized by filename extension because
    their filenames are in form texture.### where # is number. Use filename
    masks instead. Also note that after loading the input position is not set
    at the exact end of the data so it's not "stream-safe".}
  TTextureFileFormat = class(TElderFileFormat)
  private
    FLastTextureName: string;
    { Deletes non-valid chars from texture name.}
    function RepairName(const S: array of AnsiChar): string;
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    { Internal name of the last texture loaded.}
    property LastTextureName: string read FLastTextureName;
  end;

const
  { Metadata item id for accessing name of last loaded Daggerfall texture.
    Value type is string.}
  SMetaDagTextureName = 'DagTexture.Name';

implementation

const
  STextureFormatName = 'Daggerfall Texture';
  STextureMasks      = '*.dagtexture,texture.*'; // fake ext first, it's used as format id

type
  { Main texture header.}
  TTexHeader = packed record
    ImgCount: Word;                    // Number of images in texture
    TexName: array[0..23] of AnsiChar; // Name of texture
  end;

  { Offset list for texture.}
  TOffset = packed record
    Type1: Word;           // ??
    HdrOffset: Int32;      // Contains offset of Img header from the origin
                           //  of the file
    Type2: Word;           // ??
    Unk: UInt32;           // Ranges from 0 to 4 (0 in 90%)
    Null1: UInt32;         // Always 0
    Null2: UInt32;         // Always 0
  end;

  TOffsetList = array[Word] of TOffset;
  POffsetList = ^TOffsetList;

  { Image header for texture.}
  TTexImgHeader = packed record
    XOff: Word;
    YOff: Word;
    Width: Word;
    Height: Word;
    Unk1: Word;            // $0108 = Image has subimages which are RLE
                           //   compressed data.
                           // $1108 = Image has RLE type compressed data with
                           //   a row offset section before the single image data.
    ImageSize: UInt32;     // Image size (including header)
    ImageOff: UInt32;      // Pointer to start of image data from this header
    Unk2: Word;            // $0000 = Image has subimages in special
                           //   compressed format.
                           // $00C0 = Usual value, regular single image.
                           // NonZero = Regular single image.Unknown what the
                           //   differences indicate
    SubImages: Word;       // Number of subimages (1 = single image)
    Unk3: UInt32;
    Unk4: Word;
  end;

{ TTextureFileFormat }

procedure TTextureFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffMultiImage];
  FName := STextureFormatName;
  AddMasks(STextureMasks);
end;

function TTextureFileFormat.RepairName(const S: array of AnsiChar): string;
var
  I: LongInt;
  First: Boolean;
begin
  I := 1;
  Result := string(S);
  First := False;
  while I <= Length(Result) do
  begin
    if (Ord(Result[I]) < 32) or ((Ord(Result[I]) = 32) and (not First)) then
    begin
      Delete(Result, I, 1);
    end
    else
    begin
      Inc(I);
      First := True;
    end;
  end;
end;

function TTextureFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TTexHeader;
  InputSize, BasePos, HdrPos, Index, I, Bias: LongInt;
  List: POffsetList;
  ImageHdr: TTexImgHeader;

  function AddImage(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec));
  end;

  procedure LoadUncompressed;
  var
    I: LongInt;
  begin
    // Add image and read its pixels row by row
    Index := AddImage(ImageHdr.Width, ImageHdr.Height);
    with GetIO, Images[Index] do
    for I := 0 to ImageHdr.Height - 1 do
    begin
      Read(Handle, @PByteArray(Bits)[I * Width], Width);
      Seek(Handle, 256 - Width, smFromCurrent);
    end;
  end;

  procedure LoadUncompressedSubImages;
  var
    SubOffs: packed array[0..63] of Int32;
    I, StartPos, J, WritePos: LongInt;
    NumZeroes, NumImageBytes: Byte;
    SubWidth, SubHeight: Word;
  begin
    // Read subimages offset list
    StartPos := GetIO.Tell(Handle);
    FillChar(SubOffs, SizeOf(SubOffs), 0);
    GetIO.Read(Handle, @SubOffs, ImageHdr.SubImages * 4);
    for I := 0 to ImageHdr.SubImages - 1 do
    begin
      // Add new subimage and load its pixels
      Index := AddImage(ImageHdr.Width, ImageHdr.Height);
      with GetIO, Images[Index] do
      begin
        Seek(Handle, StartPos + SubOffs[I], smFromBeginning);
        Read(Handle, @SubWidth, 2);
        Read(Handle, @SubHeight, 2);
        // Read rows
        for J := 0 to SubHeight - 1 do
        begin
          WritePos := 0;
          while WritePos < SubWidth do
          begin
            // First there is a number of zero pixels that should be written
            // to this row (slight compression as many images/sprites have
            // many zero pixels)
            Read(Handle, @NumZeroes, 1);
            FillChar(PByteArray(Bits)[J * SubWidth + WritePos], NumZeroes, 0);
            WritePos := WritePos + NumZeroes;
            // Now there is a number of bytes that contain image data and should
            // be copied to this row
            Read(Handle, @NumImageBytes, 1);
            Read(Handle, @PByteArray(Bits)[J * SubWidth + WritePos], NumImageBytes);
            WritePos := WritePos + NumImageBytes;
          end;
        end;
      end;
    end;
  end;

  procedure LoadRLESubImages;
  type
    TRowOff = packed record
      Off: Word;
      RLEStatus: Word;
    end;
  var
    RowOffs: packed array[0..255] of TRowOff;
    I, J, WritePos, NextOffsetPos: LongInt;
    RLEData: Byte;
    ByteCount, RowWidth: SmallInt;
  begin
    NextOffsetPos := GetIO.Tell(Handle);
    for I := 0 to ImageHdr.SubImages - 1 do
    begin
      // Read row offsets for RLE subimage
      FillChar(RowOffs, SizeOf(RowOffs), 0);
      GetIO.Seek(Handle, NextOffsetPos, smFromBeginning);
      GetIO.Read(Handle, @RowOffs, ImageHdr.Height * SizeOf(TRowOff));
      NextOffsetPos := GetIO.Tell(Handle);
      // Add new image
      Index := AddImage(ImageHdr.Width, ImageHdr.Height);
      with GetIO, Images[Index] do
      begin
        for J := 0 to Height - 1 do
        begin
          // Seek to the beginning of the current row in the source
          Seek(Handle, HdrPos + RowOffs[J].Off, smFromBeginning);
          if RowOffs[J].RLEStatus = $8000 then
          begin
            // This row is compressed so it must be decoded (it is different
            // from RLE in IMG/CIF files)
            Read(Handle, @RowWidth, 2);
            WritePos := 0;
            while WritePos < RowWidth do
            begin
              Read(Handle, @ByteCount, 2);
              if ByteCount > 0 then
              begin
                Read(Handle, @PByteArray(Bits)[J * Width + WritePos], ByteCount);
                WritePos := WritePos + ByteCount;
              end
              else
              begin
                Read(Handle, @RLEData, 1);
                FillChar(PByteArray(Bits)[J * Width + WritePos], -ByteCount, RLEData);
                WritePos := WritePos - ByteCount;
              end;
            end;
          end
          else
            // Read uncompressed row
            Read(Handle, @PByteArray(Bits)[J * Width], Width);
        end;
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 0);
  with GetIO do
  begin
    InputSize := GetInputSize(GetIO, Handle);
    BasePos := Tell(Handle);
    Read(Handle, @Hdr, SizeOf(Hdr));
    FLastTextureName := RepairName(Hdr.TexName);
    FMetadata.SetMetaItem(SMetaDagTextureName, FLastTextureName);

    if InputSize = 2586 then
    begin
      // Handle texture.001 and texture.000 files
      // They contain only indices to palette so we create small
      // images with colors defined by these indices
      Bias := 0;
      if Pos('B', FLastTextureName) > 0 then
        Bias := 128;
      for I := 0 to Hdr.ImgCount - 1 do
      begin
        Index := AddImage(16, 16);
        FillMemoryByte(Images[Index].Bits, Images[Index].Size, I + Bias);
      end;
    end
    else if (InputSize = 46) or (InputSize = 126) or (InputSize = 266) then
    begin
      // These textures don't contain any image data
      Exit;
    end
    else
    begin
      GetMem(List, Hdr.ImgCount * SizeOf(TOffset));
      try
        // Load offsets
        for I := 0 to Hdr.ImgCount - 1 do
          Read(Handle, @List[I], SizeOf(TOffset));
        // Load subimages one by one
        for I := 0 to Hdr.ImgCount - 1 do
        begin
          // Jump at position of image header
          Seek(Handle, BasePos + List[I].HdrOffset, smFromBeginning);
          HdrPos := Tell(Handle);
          Read(Handle, @ImageHdr, SizeOf(ImageHdr));
          Seek(Handle, HdrPos + ImageHdr.ImageOff, smFromBeginning);
          // According to number of subimages and RLE settings appropriate
          // procedure is called to load subimages
          if ImageHdr.SubImages = 1 then
          begin
            if (ImageHdr.Unk1 <> $1108) and (ImageHdr.Unk1 <> $0108) then
              LoadUncompressed
            else
              LoadRLESubImages;
          end
          else
          begin
            if (ImageHdr.Unk1 <> $0108) then
              LoadUncompressedSubImages
            else
              LoadRLESubImages;
          end;
        end;
      finally
        FreeMem(List);
      end;
    end;
    Result := True;
  end;
end;

function TTextureFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TTexHeader;
  ReadCount, I: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Hdr)) and (Hdr.ImgCount > 0) and
      (Hdr.ImgCount <= 2048);
    if Result then
    begin
      for I := 0 to High(Hdr.TexName) do
      begin
        if not (Hdr.TexName[I] in [#0, #32, 'a'..'z', 'A'..'Z', '0'..'9', '.',
          '(', ')', '_', ',', '-', '''', '"', '/', '\', #9, '+']) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Last texture name now accessible trough metadata interface.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Initial version created based on my older code (fixed few things).
}

end.
