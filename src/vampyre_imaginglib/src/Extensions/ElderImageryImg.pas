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

{ This unit contains image format loader/saver for IMG file format used
  in Daggerfall and other old Bethesda games.}
unit ElderImageryImg;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingIO, ElderImagery;

type
  { Class for loading and saving of images in IMG format. It is
    8 bit indexed format found in Daggerfall, Arena, Terminator: FS,
    and maybe other old Bethesda games. Files can be RLE compressed
    and may contain palette although most images use external palettes.
    Some files have no header at all so exact file size must be known
    prior to loading (otherwise no-header files wont be recognized or whole
    image could be identified as CIF as they use the same header).}
  TIMGFileFormat = class(TElderFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
  end;

const
  { Info about special images without header.}
  NoHeaderIMGInfos: array[0..18] of TNoHeaderFileInfo = (
    (Size:     64; Width:   8; Height:   8),   // Arena file
    (Size:     90; Width:   9; Height:  10),   // Arena file
    (Size:    128; Width:   8; Height:  16),   // Arena file
    (Size:    720; Width:   9; Height:  80),
    (Size:    990; Width:  45; Height:  22),
    (Size:   1720; Width:  43; Height:  40),
    (Size:   2140; Width: 107; Height:  20),
    (Size:   2916; Width:  81; Height:  36),
    (Size:   3200; Width:  40; Height:  80),
    (Size:   3938; Width: 179; Height:  22),
    (Size:   4096; Width:  64; Height:  64),   // Textures from TES: Arena
    (Size:   4280; Width: 107; Height:  40),
    (Size:   4508; Width: 322; Height:  14),
    (Size:  20480; Width: 320; Height:  64),
    (Size:  26496; Width: 184; Height: 144),
    (Size:  64000; Width: 320; Height: 200),
    (Size:  64768; Width: 320; Height: 200),   // These contain palette
    (Size:  68800; Width: 320; Height: 215),
    (Size: 112128; Width: 512; Height: 219));

implementation

const
  SIMGFormatName = 'Daggerfall Image';
  SIMGMasks      = '*.img';

resourcestring
  SInvalidImageSize = 'Size of image in IMG format cannot exceed 65535 bytes. %s';

{ TIMGFileFormat class implementation }

procedure TIMGFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffSave];
  FName := SIMGFormatName;
  AddMasks(SIMGMasks);
end;

function TIMGFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TImgHeader;
  PalUsed: TPalette24Size256;
  Data: Pointer;
  IsRLE: Boolean;
  InputSize, I: LongInt;

  procedure SetSize(W, H: LongInt);
  begin
    Images[0].Width := W;
    Images[0].Height := H;
    Images[0].Size := W * H;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    InputSize := GetInputSize(GetIO, Handle);
    Format := ifIndex8;
    IsRLE := False;

    // Check if this is one of special images with no header
    I := FindNoHeaderInfo(InputSize, NoHeaderIMGInfos);

    if I >= 0 then
    begin
      // It is no-header image
      NewImage(NoHeaderIMGInfos[I].Width, NoHeaderIMGInfos[I].Height, ifIndex8, Images[0]);
    end
    else
    begin
      // Image has header so use its values
      Read(Handle, @Hdr, SizeOf(Hdr));
      NewImage(Hdr.Width, Hdr.Height, ifIndex8, Images[0]);
      IsRLE := Hdr.Unk = 2;
    end;

    if (Hdr.Unk = 260) or (Hdr.Unk = 264) then
    begin
      // Compressed data from Arena:
      // compression algorithm is unknown to me now
      // if Unk = 264 then after header is word size of original data
      // if Unk = 260 no size after head
      Exit;
    end;

    if not IsRLE then
    begin
      // Read uncompressed data
      GetMem(Bits, Size);
      Read(Handle, Bits, Size);
    end
    else
    begin
      GetMem(Data, Hdr.ImageSize);
      try
        // Read compressed data
        Read(Handle, Data, Hdr.ImageSize);
        DagRLEDecode(Data, Size, Bits);
      finally
        FreeMem(Data);
      end;
    end;

    // Palette handling
    GetMem(Palette, 256 * SizeOf(TColor32Rec));

    if (InputSize = Tell(Handle) + 768) then
    begin
      // Some IMG files has embedded palette
      Read(Handle, @PalUsed, 768);
      for I := Low(PalUsed) to High(PalUsed) do
      begin
        Palette[I].A := $FF;
        Palette[I].R := PalUsed[I].B;
        Palette[I].G := PalUsed[I].G;
        Palette[I].B := PalUsed[I].R;
      end;
      Palette[0].A := 0;
    end
    else
      Move(FARGBPalette[0], Palette[0], Length(FPalette) * SizeOf(TColor32Rec));

    Result := True;
  end;
end;

function TIMGFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Hdr: TImgHeader;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    FillChar(Hdr, SizeOf(Hdr), 0);
    Hdr.Width := Width;
    Hdr.Height := Height;
    // Hdr.ImageSize is Word so max size of image in bytes can be 65535
    if Width * Height > High(Word) then
      RaiseImaging(SInvalidImageSize, [ImageToStr(ImageToSave)]);
    Hdr.ImageSize := Width * Height;
    Write(Handle, @Hdr, SizeOf(Hdr));
    Write(Handle, Bits, Hdr.ImageSize);
    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Initial version created based on my older code (fixed few things).
}

end.
