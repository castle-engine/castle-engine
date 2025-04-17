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

{ This unit contains functions for loading and saving Direct3D 9 textures
  using Imaging and for converting images to textures and vice versa.}
unit ImagingDirect3D9;

{$I ImagingOptions.inc}

interface

uses
  Windows, SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats,
  ImagingUtility, Direct3D9;

type
  { Contains some texture capabilities of Direct3D device.}
  TD3DTextureCaps = record
    PowerOfTwo: Boolean;
    CubePowerOfTwo: Boolean;
    VolumePowerOfTwo: Boolean;
    MaxWidth: LongInt;
    MaxHeight: LongInt;
    DXTCompression: Boolean;
    ATI3DcCompression: Boolean;
    MaxAnisotropy: LongInt;
    MaxSimultaneousTextures: LongInt;
  end;

{ Returns some texture capabilities of the given D3D device.}
function GetDeviceTextureCaps(Device: IDirect3DDevice9; var Caps: TD3DTextureCaps): Boolean;
{ Returns True if the given Format is valid texture format for the given D3D device.}
function IsD3DFormatSupported(Device: IDirect3DDevice9; Format: TD3DFormat): Boolean;
{ Returns D3D format equivalent to the given TImageFormatInfo. It returns D3DFMT_UNKNOWN
  if equivalent cannot be found. If returned ConversionTo is not the same
  as input format then image must be first converted to this format for
  the returned D3D format to be valid. You should also check if returned D3D
  format is supported by the current D3D device using IsD3DFormatSupported.}
function ImageFormatToD3DFormat(const Format: TImageFormat; var ConversionTo: TImageFormat): TD3DFormat;
{ Returns TImageFormat equivalent to the given D3D format. If equivalent does
  not exist ifUnknown is returned.}
function D3DFormatToImageFormat(Format: TD3DFormat): TImageFormat;

{ LoadD3DTextureFromFile and similar functions use these default values:
  All mipmap levels are created, Pool is D3DPOOL_MANAGED,
  Usage is 0, Format and size are taken from image.}

{ Creates D3D texture from image in file in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadD3DTextureFromFile(const FileName: string; Device: IDirect3DDevice9;
  var Texture: IDirect3DTexture9; CreatedWidth: PLongInt = nil;
  CreatedHeight: PLongInt = nil): Boolean;
{ Creates D3D texture from image in stream in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadD3DTextureFromStream(Stream: TStream; Device: IDirect3DDevice9;
  var Texture: IDirect3DTexture9; CreatedWidth: PLongInt = nil;
  CreatedHeight: PLongInt = nil): Boolean;
{ Creates D3D texture from image in memory in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadD3DTextureFromMemory(Data: Pointer; Size: LongInt;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9;
  CreatedWidth: PLongInt = nil; CreatedHeight: PLongInt = nil): Boolean;

{ Converts TImageData structure to IDirect3DTexture9 texture.
  Input images is used as main mipmap level and additional requested
  levels are generated from this one. For the details on parameters
  look at CreateD3DTextureFromMultiImage function.}
function CreateD3DTextureFromImage(const Image: TImageData;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9; Width: LongInt = 0;
  Height: LongInt = 0; MipLevels: LongInt = 0; Usage: UInt32 = 0;
  Format: TD3DFormat = D3DFMT_UNKNOWN; Pool: TD3DPool = D3DPOOL_MANAGED;
  CreatedWidth: PLongInt = nil; CreatedHeight: PLongInt = nil): Boolean;
{ Converts images in TDymImageDataArray to one IDirect3DTexture9 texture.
  First image in array is used as main mipmap level and additional images
  are used as subsequent levels. If MipLevels is larger than number of images
  in array missing levels are automatically generated.
  If Device supports only power of two sized textures images are resized.
  If Format is D3DFMT_UNKNOWN then format of input image is used.
  If desired texture format is not supported by hardware default
  A8R8G8B8 format is used instead.
  Width and Height of 0 mean use width and height of main image.
  MipLevels set to 0 mean build all possible levels. For details on
  Usage and Pool parameters look at DirectX SDK docs.
  You can use CreatedWidth and CreatedHeight parameters to query dimensions of
  created texture's largest mipmap level (it could differ from dimensions
  of source image).}
function CreateD3DTextureFromMultiImage(const Images: TDynImageDataArray;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9; Width: LongInt = 0;
  Height: LongInt = 0; MipLevels: LongInt = 0; Usage: UInt32 = 0;
  Format: TD3DFormat = D3DFMT_UNKNOWN; Pool: TD3DPool = D3DPOOL_MANAGED;
  CreatedWidth: PLongInt = nil; CreatedHeight: PLongInt = nil): Boolean;

{ Saves D3D texture to file in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveD3DTextureToFile(const FileName: string; const Texture: IDirect3DTexture9): Boolean;
{ Saves D3D texture to stream in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveD3DTextureToStream(const Ext: string; Stream: TStream; const Texture: IDirect3DTexture9): Boolean;
{ Saves D3D texture to memory in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveD3DTextureToMemory(const Ext: string; Data: Pointer; var Size: LongInt; const Texture: IDirect3DTexture9): Boolean;

{ Converts main level of the D3D texture to TImageData structure. OverrideFormat
  can be used to convert output image to the specified format rather
  than use the format taken from D3D texture, ifUnknown means no conversion.}
function CreateImageFromD3DTexture(const Texture: IDirect3DTexture9;
  var Image: TImageData; OverrideFormat: TImageFormat = ifUnknown): Boolean;
{ Converts D3D texture to TDynImageDataArray array of images. You can specify
  how many mipmap levels of the input texture you want to be converted
  (default is all levels). OverrideFormat can be used to convert output images to
  the specified format rather than use the format taken from D3D texture,
  ifUnknown means no conversion.}
function CreateMultiImageFromD3DTexture(const Texture: IDirect3DTexture9;
  var Images: TDynImageDataArray; MipLevels: LongInt = 0;
  OverrideFormat: TImageFormat = ifUnknown): Boolean;

{ Creates contents of Image to D3D surface. Surface must exist before calling this
  function so it can be used to fill various types of surfaces (textures surfaces,
  offscreen, depth buffer, ...). Surface must be lockable for function to work.}
function CreateD3DSurfaceFromImage(const Image: TImageData; Surface: IDirect3DSurface9): Boolean;
{ Creates image filled with contents of input D3D surface.
  Surface must be lockable for function to work.}
function CreateImageFromD3DSurface(Surface: IDirect3DSurface9; var Image: TImageData): Boolean;

const
  D3DFMT_ATI1 = TD3DFormat(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or
    (Byte('1') shl 24));
  D3DFMT_ATI2 = TD3DFormat(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or
    (Byte('2') shl 24));

implementation

const
  DefaultUsage = 0;
  DefaultPool = D3DPOOL_MANAGED;

function GetDeviceTextureCaps(Device: IDirect3DDevice9;
  var Caps: TD3DTextureCaps): Boolean;
var
  D3DCaps: TD3DCaps9;
begin
  FillChar(Caps, SizeOf(Caps), 0);
  Result := Device <> nil;
  // Get D3D Device Caps and fill our caps
  if Result and (Device.GetDeviceCaps(D3DCaps) = D3D_OK) then
  begin
    Caps.PowerOfTwo := (D3DCaps.TextureCaps and D3DPTEXTURECAPS_POW2) = D3DPTEXTURECAPS_POW2;
    Caps.CubePowerOfTwo := (D3DCaps.TextureCaps and D3DPTEXTURECAPS_CUBEMAP_POW2) = D3DPTEXTURECAPS_CUBEMAP_POW2;
    Caps.VolumePowerOfTwo := (D3DCaps.TextureCaps and D3DPTEXTURECAPS_VOLUMEMAP_POW2) = D3DPTEXTURECAPS_VOLUMEMAP_POW2;
    Caps.MaxWidth := D3DCaps.MaxTextureWidth;
    Caps.MaxHeight := D3DCaps.MaxTextureHeight;
    if (D3DCaps.TextureFilterCaps and D3DPTFILTERCAPS_MINFANISOTROPIC) = D3DPTFILTERCAPS_MINFANISOTROPIC then
      Caps.MaxAnisotropy := D3DCaps.MaxAnisotropy
    else
      Caps.MaxAnisotropy := 0;
    Caps.MaxSimultaneousTextures := D3DCaps.MaxSimultaneousTextures;
    // Texture format caps
    Caps.DXTCompression := IsD3DFormatSupported(Device, D3DFMT_DXT1) and
      IsD3DFormatSupported(Device, D3DFMT_DXT3) and IsD3DFormatSupported(Device, D3DFMT_DXT5);
    Caps.ATI3DcCompression := IsD3DFormatSupported(Device, D3DFMT_ATI1) and
      IsD3DFormatSupported(Device, D3DFMT_ATI2);
  end;
end;

function IsD3DFormatSupported(Device: IDirect3DDevice9; Format: TD3DFormat): Boolean;
var
  Direct3D: IDirect3D9;
  Mode: TD3DDisplayMode;
  Hr: HResult;
begin
  Result := False;
  if Device <> nil then
  begin
    Device.GetDirect3D(Direct3D);
    if Direct3D <> nil then
    begin
      Direct3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, Mode);
      Hr := Direct3D.CheckDeviceFormat(D3DADAPTER_DEFAULT,
        D3DDEVTYPE_HAL, Mode.Format, 0, D3DRTYPE_TEXTURE, Format);
      Result := Succeeded(Hr);
    end;
  end;
end;

function ImageFormatToD3DFormat(const Format: TImageFormat; var ConversionTo: TImageFormat): TD3DFormat;
begin
  Result := D3DFMT_UNKNOWN;
  ConversionTo := Format;
  case Format of
    ifIndex8: Result := D3DFMT_P8;
    ifGray8:        Result := D3DFMT_L8;
    ifA8Gray8:      Result := D3DFMT_A8L8;
    ifGray16:       Result := D3DFMT_L16;
    ifGray32,
    ifGray64:
      begin
        Result := D3DFMT_L16;
        ConversionTo := ifGray16;
      end;
    ifA16Gray16:
      begin
        Result := D3DFMT_A8L8;
        ConversionTo := ifA8Gray8;
      end;
    ifX5R1G1B1:
      begin
        Result := D3DFMT_R3G3B2;
        ConversionTo := ifR3G3B2;
      end;
    ifR3G3B2:       Result := D3DFMT_R3G3B2;
    ifR5G6B5:       Result := D3DFMT_R5G6B5;
    ifA1R5G5B5:     Result := D3DFMT_A1R5G5B5;
    ifA4R4G4B4:     Result := D3DFMT_A4R4G4B4;
    ifX1R5G5B5:     Result := D3DFMT_X1R5G5B5;
    ifX4R4G4B4:     Result := D3DFMT_X4R4G4B4;
    ifR8G8B8:       Result := D3DFMT_R8G8B8;
    ifA8R8G8B8:     Result := D3DFMT_A8R8G8B8;
    ifX8R8G8B8:     Result := D3DFMT_X8R8G8B8;
    ifR16G16B16,
    ifA16R16G16B16,
    ifB16G16R16:
      begin
        Result := D3DFMT_A16B16G16R16;
        ConversionTo := ifA16B16G16R16;
      end;
    ifA16B16G16R16: Result := D3DFMT_A16B16G16R16;
    ifR32F:          Result := D3DFMT_R32F;
    ifA32B32G32R32F: Result := D3DFMT_A32B32G32R32F;
    ifA32R32G32B32F:
      begin
        Result := D3DFMT_A32B32G32R32F;
        ConversionTo := ifA32B32G32R32F;
      end;
    ifR16F:          Result := D3DFMT_R16F;
    ifA16B16G16R16F: Result := D3DFMT_A16B16G16R16F;
    ifA16R16G16B16F:
      begin
        Result := D3DFMT_A16B16G16R16F;
        ConversionTo := ifA16B16G16R16F;
      end;
    ifDXT1: Result := D3DFMT_DXT1;
    ifDXT3: Result := D3DFMT_DXT3;
    ifDXT5: Result := D3DFMT_DXT5;
    ifATI1N: Result := D3DFMT_ATI1;
    ifATI2N: Result := D3DFMT_ATI2;
  end;
end;

function D3DFormatToImageFormat(Format: TD3DFormat): TImageFormat;
begin
  Result := ifUnknown;
  case Format of
    D3DFMT_P8:            Result := ifIndex8;
    D3DFMT_A8,
    D3DFMT_L8:            Result := ifGray8;
    D3DFMT_A8L8,
    D3DFMT_V8U8:          Result := ifA8Gray8;
    D3DFMT_L16:           Result := ifGray16;
    D3DFMT_R3G3B2:        Result := ifR3G3B2;
    D3DFMT_R5G6B5:        Result := ifR5G6B5;
    D3DFMT_X1R5G5B5:      Result := ifX1R5G5B5;
    D3DFMT_A1R5G5B5:      Result := ifA1R5G5B5;
    D3DFMT_A4R4G4B4:      Result := ifA4R4G4B4;
    D3DFMT_X4R4G4B4:      Result := ifX4R4G4B4;
    D3DFMT_R8G8B8:        Result := ifR8G8B8;
    D3DFMT_A8R8G8B8,
    D3DFMT_Q8W8V8U8,
    D3DFMT_A8B8G8R8:      Result := ifA8R8G8B8;
    D3DFMT_X8R8G8B8,
    D3DFMT_X8L8V8U8,
    D3DFMT_X8B8G8R8:      Result := ifX8R8G8B8;
    D3DFMT_A16B16G16R16,
    D3DFMT_Q16W16V16U16:  Result := ifA16B16G16R16;
    D3DFMT_R32F:          Result := ifR32F;
    D3DFMT_A32B32G32R32F: Result := ifA32B32G32R32F;
    D3DFMT_R16F:          Result := ifR16F;
    D3DFMT_A16B16G16R16F: Result := ifA16B16G16R16F;
    D3DFMT_DXT1:          Result := ifDXT1;
    D3DFMT_DXT3:          Result := ifDXT3;
    D3DFMT_DXT5:          Result := ifDXT5;
    D3DFMT_ATI1:          Result := ifATI1N;
    D3DFMT_ATI2:          Result := ifATI2N;
  end;
end;

function LoadD3DTextureFromFile(const FileName: string; Device: IDirect3DDevice9;
  var Texture: IDirect3DTexture9; CreatedWidth, CreatedHeight: PLongInt): Boolean;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromFile(FileName, Images) and (Length(Images) > 0) then
  begin
    Result := CreateD3DTextureFromMultiImage(Images, Device, Texture,
      Images[0].Width, Images[0].Height, 0, DefaultUsage, D3DFMT_UNKNOWN,
      DefaultPool, CreatedWidth, CreatedHeight);
  end
  else
    Result := False;
  FreeImagesInArray(Images);
end;

function LoadD3DTextureFromStream(Stream: TStream; Device: IDirect3DDevice9;
  var Texture: IDirect3DTexture9; CreatedWidth, CreatedHeight: PLongInt): Boolean;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromStream(Stream, Images) and (Length(Images) > 0) then
  begin
    Result := CreateD3DTextureFromMultiImage(Images, Device, Texture,
      Images[0].Width, Images[0].Height, 0, DefaultUsage, D3DFMT_UNKNOWN,
      DefaultPool, CreatedWidth, CreatedHeight);
  end
  else
    Result := False;
  FreeImagesInArray(Images);
end;

function LoadD3DTextureFromMemory(Data: Pointer; Size: LongInt;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9;
  CreatedWidth, CreatedHeight: PLongInt): Boolean;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromMemory(Data, Size, Images) and (Length(Images) > 0) then
  begin
    Result := CreateD3DTextureFromMultiImage(Images, Device, Texture, Images[0].Width,
      Images[0].Height, 0, DefaultUsage, D3DFMT_UNKNOWN, DefaultPool,
      CreatedWidth, CreatedHeight);
  end
  else
    Result := False;
  FreeImagesInArray(Images);
end;

function CreateD3DTextureFromImage(const Image: TImageData;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9; Width,
  Height, MipLevels: LongInt; Usage: UInt32; Format: TD3DFormat;
  Pool: TD3DPool; CreatedWidth, CreatedHeight: PLongInt): Boolean;
var
  Arr: TDynImageDataArray;
begin
  // Just calls function operating on image arrays
  SetLength(Arr, 1);
  Arr[0] := Image;
  Result := CreateD3DTextureFromMultiImage(Arr, Device, Texture, Width, Height,
    MipLevels, Usage, Format, Pool, CreatedWidth, CreatedHeight);
end;

procedure FillLockedRectWithImage(var Rect: TD3DLockedRect; const Image: TImageData);
var
  I, LineBytes: LongInt;
  Info: TImageFormatInfo;
begin
  GetImageFormatInfo(Image.Format, Info);
  LineBytes := Info.GetPixelsSize(Info.Format, Image.Width, 1);
  // Pixels of the image are copied to D3D texture
  if (not Info.IsSpecial) and (LineBytes < Rect.Pitch) then
  begin
    for I := 0 to Image.Height - 1 do
      Move(PByteArray(Image.Bits)[I * LineBytes],
        PByteArray(Rect.pBits)[I * Rect.Pitch], LineBytes);
  end
  else
    Move(Image.Bits^, Rect.pBits^, Image.Size);
end;

function CreateD3DTextureFromMultiImage(const Images: TDynImageDataArray;
  Device: IDirect3DDevice9; var Texture: IDirect3DTexture9; Width,
  Height, MipLevels: LongInt; Usage: UInt32; Format: TD3DFormat;
  Pool: TD3DPool; CreatedWidth, CreatedHeight: PLongInt): Boolean;
var
  I, PossibleLevels, ExistingLevels, CurrentWidth, CurrentHeight: LongInt;
  Caps: TD3DTextureCaps;
  Rect: TD3DLockedRect;
  ConvTo: TImageFormat;
  LevelsArray: TDynImageDataArray;
  NeedsResize, NeedsConvert: Boolean;
begin
  Texture := nil;
  ExistingLevels := 0;
  Result := False;
  // Get texture caps of the current device and test if there is anything to convert
  if GetDeviceTextureCaps(Device, Caps) and (Length(Images) > 0) then
  try
    // First check desired size and modify it if necessary
    if Width <= 0 then Width := Images[0].Width;
    if Height <= 0 then Height := Images[0].Height;
    if Caps.PowerOfTwo then
    begin
      // If device supports only power of 2 texture sizes
      Width := NextPow2(Width);
      Height := NextPow2(Height);
    end;
    Width := ClampInt(Width, 1, Caps.MaxWidth);
    Height := ClampInt(Height, 1, Caps.MaxHeight);

    // Get various mipmap level counts and modify
    // desired MipLevels if its value is invalid
    ExistingLevels := Length(Images);
    PossibleLevels := GetNumMipMapLevels(Width, Height);
    if (MipLevels < 1) or (MipLevels > PossibleLevels) then
      MipLevels := PossibleLevels;

    // Now determine which image format will be used
    if Format = D3DFMT_UNKNOWN then
    begin
      // D3D texture format is not explicitly defined so we use
      // the current format of the input image
      Format := ImageFormatToD3DFormat(Images[0].Format, ConvTo);
      // Format is now either D3DFMT_UNKNOWN or some valid format and
      // ConvTo contains format to which input image must be converted first
      // (if ConvTo and input image's format differ).
      // We must also test if returned D3D format is supported by D3D device
      if (Format = D3DFMT_UNKNOWN) or not IsD3DFormatSupported(Device, Format) then
      begin
        Format := D3DFMT_A8R8G8B8;
        ConvTo := ifA8R8G8B8;
      end;
    end
    else
    begin
      // Image format corresponding to desired D3D format is either found
      // and image is converted to it (if the image is not in this format already)
      // or it is not found (or not supported by hardware) and default format is used
      ConvTo := D3DFormatToImageFormat(Format);
      if (ConvTo = ifUnknown) or not IsD3DFormatSupported(Device, Format) then
      begin
        Format := D3DFMT_A8R8G8B8;
        ConvTo := ifA8R8G8B8;
      end;
    end;

    // Prepare array for mipmap levels
    SetLength(LevelsArray, MipLevels);

    CurrentWidth := Width;
    CurrentHeight := Height;

    for I := 0 to MipLevels - 1 do
    begin
      // Check if we can use input image array as a source for this mipmap level
      if I < ExistingLevels then
      begin
        // Check if input image for this mipmap level has the right
        // size and format
        NeedsResize := not ((Images[I].Width = CurrentWidth) and (Images[I].Height = CurrentHeight));
        NeedsConvert := not (Images[I].Format = ConvTo);

        if NeedsResize or NeedsConvert then
        begin
          // Input image must be resized or converted to different format
          // to become valid mipmap level
          CloneImage(Images[I], LevelsArray[I]);
          if NeedsConvert then
            ConvertImage(LevelsArray[I], ConvTo);
          if NeedsResize then
            ResizeImage(LevelsArray[I], CurrentWidth, CurrentHeight, rfBilinear);
        end
        else
          // Input image can be used without any changes
          LevelsArray[I] := Images[I];
      end
      else
      begin
        // This mipmap level is not present in the input image array
        // so we create a new level
        FillMipMapLevel(LevelsArray[I - 1], CurrentWidth, CurrentHeight, LevelsArray[I]);
      end;
      // Calculate width and height of the next mipmap level
      CurrentWidth := ClampInt(CurrentWidth div 2, 1, CurrentWidth);
      CurrentHeight := ClampInt(CurrentHeight div 2, 1, CurrentHeight);
    end;

    // Finally create D3D texture object
    if Succeeded(Device.CreateTexture(LevelsArray[0].Width,
      LevelsArray[0].Height, MipLevels, Usage, Format, Pool, Texture, nil)) then
    begin
      // Fill each mipmap level
      for I := 0 to MipLevels - 1 do
        if Succeeded(Texture.LockRect(I, Rect, nil, 0)) then
        begin
          FillLockedRectWithImage(Rect, LevelsArray[I]);
          Texture.UnlockRect(I);
        end;
      Result := True;
    end;

    // If user is interested in width and height of created texture lets
    // give him that
    if CreatedWidth <> nil then CreatedWidth^ := LevelsArray[0].Width;
    if CreatedHeight <> nil then CreatedHeight^ := LevelsArray[0].Height;

  finally
    // Free local image copies
    for I := 0 to Length(LevelsArray) - 1 do
    begin
      if ((I < ExistingLevels) and (LevelsArray[I].Bits <> Images[I].Bits)) or
        (I >= ExistingLevels) then
        FreeImage(LevelsArray[I]);
    end;
  end;
end;

function SaveD3DTextureToFile(const FileName: string; const Texture: IDirect3DTexture9): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromD3DTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByName(FileName);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToFile(FileName, Arr);
      if IsDDS then
        PopOptions;
    end;
  end;
end;

function SaveD3DTextureToStream(const Ext: string; Stream: TStream; const Texture: IDirect3DTexture9): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromD3DTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByExt(Ext);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToStream(Ext, Stream, Arr);
      if IsDDS then
        PopOptions;
    end;
  end;
end;

function SaveD3DTextureToMemory(const Ext: string; Data: Pointer; var Size: LongInt; const Texture: IDirect3DTexture9): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromD3DTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByExt(Ext);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToMemory(Ext, Data, Size, Arr);
      if IsDDS then
        PopOptions;
    end;
  end;
end;

function CreateImageFromD3DTexture(const Texture: IDirect3DTexture9;
  var Image: TImageData; OverrideFormat: TImageFormat): Boolean;
var
  Arr: TDynImageDataArray;
begin
  // Just calls function operating on image arrays
  FreeImage(Image);
  SetLength(Arr, 1);
  Result := CreateMultiImageFromD3DTexture(Texture, Arr, 1, OverrideFormat);
  Image := Arr[0];
end;

procedure FillImageWithLockedRect(var Image: TImageData; const Rect: TD3DLockedRect);
var
  I, LineBytes: LongInt;
  Info: TImageFormatInfo;
begin
  GetImageFormatInfo(Image.Format, Info);
  LineBytes := Info.GetPixelsSize(Info.Format, Image.Width, 1);
  // Pixels are copied from D3D texture to the image
  if (not Info.IsSpecial) and (LineBytes < Rect.Pitch) then
  begin
    for I := 0 to Image.Height - 1 do
      Move(PByteArray(Rect.pBits)[I * Rect.Pitch],
        PByteArray(Image.Bits)[I * LineBytes], LineBytes);
  end
  else
    Move(Rect.pBits^, Image.Bits^, Image.Size);
end;

function CreateMultiImageFromD3DTexture(const Texture: IDirect3DTexture9;
  var Images: TDynImageDataArray; MipLevels: LongInt; OverrideFormat: TImageFormat): Boolean;
var
  Rect: TD3DLockedRect;
  Desc: TD3DSurfaceDesc;
  I,ExistingLevels: LongInt;
  CurrentFormat: TImageFormat;
  Info: TImageFormatInfo;
begin
  FreeImagesInArray(Images);
  SetLength(Images, 0);
  Result := False;
  if Texture <> nil then
  begin
    // Check if desired mipmap level count is valid
    ExistingLevels := Texture.GetLevelCount;
    if (MipLevels <= 0) or (MipLevels > ExistingLevels) then
      MipLevels := ExistingLevels;

    Texture.GetLevelDesc(0, Desc);
    // Try to find image format compatible with d3d texture's format
    CurrentFormat := D3DFormatToImageFormat(Desc.Format);
    // Exit if no compatible image format is found
    if CurrentFormat = ifUnknown then
      Exit;

    SetLength(Images, MipLevels);
    GetImageFormatInfo(CurrentFormat, Info);

    for I := 0 to MipLevels - 1 do
    begin
      if Failed(Texture.LockRect(I, Rect, nil, D3DLOCK_READONLY)) then Exit;
      Texture.GetLevelDesc(I, Desc);

      // Create image for the current mipmap level and copy texture data to it
      NewImage(Desc.Width, Desc.Height, CurrentFormat, Images[I]);
      FillImageWithLockedRect(Images[I], Rect);

      // If override format is set each mipmap  level is converted to it
      if OverrideFormat <> ifUnknown then
        ConvertImage(Images[I], OverrideFormat);

      Texture.UnlockRect(I);
    end;
    Result := True;
  end;
end;

function CreateD3DSurfaceFromImage(const Image: TImageData; Surface: IDirect3DSurface9): Boolean;
var
  ConvTo: TImageFormat;
  Desc: TD3DSurfaceDesc;
  Rect: TD3DLockedRect;
  WorkImage: TImageData;
begin
  Result := False;
  if (Surface = nil) or not TestImage(Image) then
    Exit;
  // Get surface's format and find Imaging data format match
  Surface.GetDesc(Desc);
  ConvTo := D3DFormatToImageFormat(Desc.Format);
  // If no Imaging data format was found we must exit
  if ConvTo = ifUnknown then
    Exit;

  if (LongInt(Desc.Width) <> Image.Width) or (LongInt(Desc.Height) <> Image.Height) or
    (Image.Format <> ConvTo) then
  begin
    // Source image has different dimensions or format than dest surface,
    // working image is created
    InitImage(WorkImage);
    NewImage(Desc.Width, Desc.Height, ConvTo, WorkImage);
    StretchRect(Image, 0, 0, Image.Width, Image.Height, WorkImage, 0, 0,
      WorkImage.Width, WorkImage.Height, rfBilinear);
  end
  else
    WorkImage := Image;

  try
    // Lock surface and fill it with image
    if Succeeded(Surface.LockRect(Rect, nil, 0)) then
    begin
      FillLockedRectWithImage(Rect, WorkImage);
      Surface.UnlockRect;
      Result := True;
    end;
  finally
    // Free working image if it is not reference to source image
    if WorkImage.Bits <> Image.Bits then
      FreeImage(WorkImage);
  end;
end;

function CreateImageFromD3DSurface(Surface: IDirect3DSurface9; var Image: TImageData): Boolean;
var
  CurrentFormat: TImageFormat;
  Desc: TD3DSurfaceDesc;
  Rect: TD3DLockedRect;
begin
  Result := False;
  FreeImage(Image);
  if Surface = nil then
    Exit;
  Surface.GetDesc(Desc);
  CurrentFormat := D3DFormatToImageFormat(Desc.Format);
  // Exit if no compatible image format is found
  if CurrentFormat = ifUnknown then
    Exit;

  if Succeeded(Surface.LockRect(Rect, nil, D3DLOCK_READONLY)) then
  begin
    // If surface was successfully locked a new image is created
    // and surface's contents are copied to it
    NewImage(Desc.Width, Desc.Height, CurrentFormat, Image);
    FillImageWithLockedRect(Image, Rect);
    Surface.UnlockRect;
    Result := True;
  end;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - support for cube and volume maps

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Added support for 3Dc compressed texture formats.
    - Added detection of 3Dc support to texture caps.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added CreatedWidth and CreatedHeight parameters to most
      LoadD3DTextureFromXXX/CreateD3DTextureFromXXX functions.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - fixed bug in CreateGLTextureFromMultiImage which caused assert failure
      when creating mipmaps (using FillMipMapLevel) for DXTC formats
    - added support for 16bit half-float texture formats

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - D3D surface support - fill surface with image and vice versa
    - more texture caps added
    - filtered mipmap creation

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - unit created and initial stuff added
}

end.
