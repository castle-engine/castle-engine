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

{ This unit contains image format loader/saver for Daggerfall
  multi-image format CIF.}
unit ElderImageryCif;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingIO, ElderImagery;

type
  { Class for loading and saving of multi-images in CIF format. It is
    8 bit indexed format found in Daggerfall. It is basically a sequence of
    images in IMG (see TIMGFileFormat) stored in one file (with exception
    of Weapo*.cif files which are little bit more complex). As with IMG files
    CIF files can be RLE compressed and there are also special CIFs without header.
    Total number of frames in file is known after the whole file was parsed
    so exact file size must be known prior to loading.}
  TCIFFileFormat = class(TElderFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
  end;

const
  { Info about special CIFs without header.}
  NoHeaderCIFInfos: array[0..6] of TNoHeaderFileInfo = (
    (Size:    2601; Width: 17; Height: 17),   // MPOP.RCI
    (Size:    3168; Width: 44; Height:  9),   // NOTE.RCI
    (Size:    4356; Width: 22; Height: 22),   // SPOP.RCI
    (Size:   10752; Width: 32; Height: 16),   // BUTTONS.RCI
    (Size:   49152; Width: 64; Height: 64),   // CHLD00I0.RCI
    (Size:  249856; Width: 64; Height: 64),   // FACES.CIF
    (Size: 2060295; Width: 64; Height: 64));  // TFAC00I0.RCI

implementation

const
  SCIFFormatName = 'Daggerfall MultiImage';
  SCIFMasks      = '*.cif,*.rci';

resourcestring
  SInvalidImageSize = 'Size of image in IMG/CIF format cannot exceed 65535 bytes. %s';

type
  { Header for CIF group files.}
  TCIFGroup = packed record
    Width: Word;
    Height: Word;
    XOff: Word;
    YOff: Word;
    Unk: Word;
    ImageSize: Word;               // Size of Image data (but not always)
    Offsets: array[0..31] of Word; // Offsets from beginning of header to
                                   // image datas. Last offset points to next
                                   // group header
  end;

{ TCIFFileFormat class implementation }

procedure TCIFFileFormat.Define;
begin
  inherited;
  FName := SCIFFormatName;
  AddMasks(SCIFMasks);
end;

function TCIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TImgHeader;
  Group: TCIFGroup;
  Data: Pointer;
  IsWeapon, ISW9, IsStandard, IsFirst: Boolean;
  InputSize, I, FrameWidth, FrameHeight, OldPos, Index, BufferSize: LongInt;
  HasHeader: Boolean;

  function AddImage(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec));
  end;

begin
  SetLength(Images, 0);
  with GetIO do
  begin
    InputSize := GetInputSize(GetIO, Handle);
    HasHeader := True;
    IsWeapon := False;
    IsW9 := False;
    IsFirst := True;
    FrameWidth := 0;
    FrameHeight := 0;

    // Check if this is one of special CIF with no header
    I := FindNoHeaderInfo(InputSize, NoHeaderCIFInfos);

    if I >= 0 then
    begin
      // It is no-header CIF
      FrameWidth := NoHeaderCIFInfos[I].Width;
      FrameHeight := NoHeaderCIFInfos[I].Height;
      HasHeader := False;
    end;

    if HasHeader then
    begin
      OldPos := Tell(Handle);
      // CIF has header so use its values
      Read(Handle, @Hdr, SizeOf(Hdr));

      if Hdr.Unk = $15 then
      begin
        // This file is weapon09.cif (shooting arrows)
        IsWeapon := True;
        IsW9 := True;
      end;

      if Tell(Handle) + Hdr.ImageSize < InputSize then
      begin
        Seek(Handle, Hdr.ImageSize, smFromCurrent);
        Read(Handle, @Group, SizeOf(Group));
        if Group.Offsets[0] = 76 then
          // CIF is regular weapon file
          IsWeapon := True;
      end;
      Seek(Handle, OldPos, smFromBeginning);
    end;

    IsStandard := HasHeader and (not IsWeapon);

    while not Eof(Handle) do
    begin
      if IsStandard then
      begin
        // Handle CIFs in standard format with header
        Read(Handle, @Hdr, SizeOf(Hdr));
        Index := AddImage(Hdr.Width, Hdr.Height);
        if Hdr.Unk <> 2 then
        begin
          // Read uncompressed data
          Read(Handle, Images[Index].Bits, Hdr.ImageSize);
        end
        else
        begin
          GetMem(Data, Hdr.ImageSize);
          try
            // Read RLE compressed data
            Read(Handle, Data, Hdr.ImageSize);
            DagRLEDecode(Data, Images[Index].Size, Images[Index].Bits);
          finally
            FreeMem(Data);
          end;
        end;
      end
      else if not HasHeader then
      begin
        // Handle CIFs in standard format without header
        if Tell(Handle) + FrameWidth * FrameHeight <= InputSize then
        begin
          Index := AddImage(FrameWidth, FrameHeight);
          Read(Handle, Images[Index].Bits, Images[Index].Size);
        end
        else
          Break;
      end
      else if IsWeapon then
      begin
        // Handle CIFs with weapon animations
        if IsFirst and (not IsW9) then
        begin
          // First frame is std IMG file, next ones are not
          // but if IsW9 is true this first frame is missing
          Read(Handle, @Hdr, SizeOf(Hdr));
          Index := AddImage(Hdr.Width, Hdr.Height);
          Read(Handle, Images[Index].Bits, Images[Index].Size);
          IsFirst := False;
        end
        else
        begin
          OldPos := Tell(Handle);
          // Read next group
          Read(Handle, @Group, SizeOf(Group));
          // Read images in group
          I := 0;
          while Group.Offsets[I] <> 0 do
          begin
            BufferSize := Group.Offsets[I + 1] - Group.Offsets[I];
            if BufferSize < 0 then
              BufferSize := Group.Offsets[31] - Group.Offsets[I];

            Seek(Handle, OldPos + Group.Offsets[I], smFromBeginning);
            Index := AddImage(Group.Width, Group.Height);
            // Read current image from current group and decode it
            GetMem(Data, BufferSize);
            try
              Read(Handle, Data, BufferSize);
              DagRLEDecode(Data, Images[Index].Size, Images[Index].Bits);
              Inc(I);
            finally
              FreeMem(Data);
            end;
          end;
          Seek(Handle, OldPos + Group.Offsets[31], smFromBeginning);
        end;
      end;
    end;
    Result := True;
  end;
end;

function TCIFFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Hdr: TImgHeader;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  I: LongInt;
begin
  Result := False;
  for I := FFirstIdx to FLastIdx do
  begin
    if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
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
    finally
      if MustBeFreed then
        FreeImage(ImageToSave);
    end
    else
      Exit;
  end;
  Result := True;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Initial version created based on my older code (fixed few things).
}

end.
