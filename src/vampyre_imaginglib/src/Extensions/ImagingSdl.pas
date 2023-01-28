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

{ This unit contains functions for loading/saving SDL surfaces using Imaging
  and for converting images to surfaces and vice versa.}
unit ImagingSdl;

{$I ImagingOptions.inc}

interface

uses
  Classes, sdl, ImagingTypes, Imaging, ImagingUtility;

type
  { This SDL type is redefined here so ImagingExport unit does not
    need sdl unit in the uses list.}
  PSDL_Surface = sdl.PSDL_Surface;

{ LoadSDLSurfaceFromFile and similar functions use SDL_SWSURFACE as Flags when creating
  SDL surface. If you want other Flags to be used load image by standard
  LoadImageFromFile and similar functions and then call CreateSDLSurfaceFromImage
  which has more options.}

{ Creates SDL surface from image in file in format supported by Imaging.}
function LoadSDLSurfaceFromFile(const FileName: string): PSDL_Surface;
{ Creates SDL surface from image in stream in format supported by Imaging.}
function LoadSDLSurfaceFromStream(Stream: TStream): PSDL_Surface;
{ Creates SDL surface from image in memory in format supported by Imaging.}
function LoadSDLSurfaceFromMemory(Data: Pointer; Size: LongInt): PSDL_Surface;

{ Converts image to SDL surface. Flags is used when creating SDL surface
  using SDL_CreateRGBSurface and is passed to it. OverrideFormat can be
  used to convert image to specified format before SDL surface is created,
  ifUnknown means no conversion.}
function CreateSDLSurfaceFromImage(const ImageData: TImageData;
  Flags: UInt32; OverrideFormat: TImageFormat = ifUnknown): PSDL_Surface;

{ Saves SDL surface to file in one of the formats supported by Imaging.}
function SaveSDLSurfaceToFile(const FileName: string; Surface: PSDL_Surface): Boolean;
{ Saves SDL surface to stream in one of the formats supported by Imaging defined by Ext.}
function SaveSDLSurfaceToStream(const Ext: string; Stream: TStream; Surface: PSDL_Surface): Boolean;
{ Saves SDL surface to memory in one of the formats supported by Imaging defined
  by Ext. Size must contain size of available memory before the function
  is called and memory size taken up by the image is returned in this parameter.}
function SaveSDLSurfaceToMemory(const Ext: string; Data: Pointer; var Size: LongInt; Surface: PSDL_Surface): Boolean;

{ Converts SDL surface to TImageData structure. OverrideFormat can be
  used to convert output image to the specified format rather than
  use the format taken from SDL surface, ifUnknown means no conversion.}
function CreateImageFromSDLSurface(Surface: PSDL_Surface; var ImageData: TImageData;
  OverrideFormat: TImageFormat = ifUnknown): Boolean;

implementation

const
  DefaultFlags = SDL_SWSURFACE;

function Iff(Condition: Boolean; const TruePart, FalsePart: TImageFormat): TImageFormat; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function LoadSDLSurfaceFromFile(const FileName: string): PSDL_Surface;
var
  ImageData: TImageData;
begin
  InitImage(ImageData);
  if LoadImageFromFile(FileName, ImageData) then
    Result := CreateSDLSurfaceFromImage(ImageData, DefaultFlags)
  else
    Result := nil;
  FreeImage(ImageData);
end;

function LoadSDLSurfaceFromStream(Stream: TStream): PSDL_Surface;
var
  ImageData: TImageData;
begin
  InitImage(ImageData);
  if LoadImageFromStream(Stream, ImageData) then
    Result := CreateSDLSurfaceFromImage(ImageData, DefaultFlags)
  else
    Result := nil;
  FreeImage(ImageData);
end;

function LoadSDLSurfaceFromMemory(Data: Pointer; Size: LongInt): PSDL_Surface;
var
  ImageData: TImageData;
begin
  InitImage(ImageData);
  if LoadImageFromMemory(Data, Size, ImageData) then
    Result := CreateSDLSurfaceFromImage(ImageData, DefaultFlags)
  else
    Result := nil;
  FreeImage(ImageData);  
end;

function CreateSDLSurfaceFromImage(const ImageData: TImageData;
  Flags: UInt32; OverrideFormat: TImageFormat): PSDL_Surface;
var
  WorkData: TImageData;
  Info: TImageFormatInfo;
  ConvFormat: TImageFormat;
  AMask, RMask, GMask, BMask: UInt32;
  I, LineBytes: LongInt;

  procedure DetermineSDLMasks(var AMask, RMask, GMask, BMask: UInt32);
  begin
    if Info.UsePixelFormat then
    begin
      AMask := Info.PixelFormat.ABitMask;
      RMask := Info.PixelFormat.RBitMask;
      GMask := Info.PixelFormat.GBitMask;
      BMask := Info.PixelFormat.BBitMask;
    end
    else
    begin
      AMask := IffUnsigned(Info.HasAlphaChannel, $FF000000, 0);
      RMask := $00FF0000;
      GMask := $0000FF00;
      BMask := $000000FF;
    end;
  end;

begin
  Result := nil;
  if TestImage(ImageData) then
  begin
    InitImage(WorkData);
    CloneImage(ImageData, WorkData);
    // Image is converted to override format
    if OverrideFormat <> ifUnknown then
      ConvertImage(WorkData, OverrideFormat);

    GetImageFormatInfo(WorkData.Format, Info);
    // Image is first converted to format supported by SDL
    if Info.IsFloatingPoint or Info.IsSpecial then
      ConvFormat := ifA8R8G8B8
    else
      if Info.UsePixelFormat then
      begin
        if Info.BytesPerPixel < 2 then
          ConvFormat := Iff(Info.HasAlphaChannel, ifA4R4G4B4, ifR5G6B5)
        else
          ConvFormat := WorkData.Format;
      end
      else
        if Info.IsIndexed then
          ConvFormat := ifIndex8
        else
          ConvFormat := Iff(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

    ConvertImage(WorkData, ConvFormat);
    GetImageFormatInfo(WorkData.Format, Info);
    // Channel masks are determined based on image's format,
    // only 8/16/24/32bit images should be here now
    DetermineSDLMasks(AMask, RMask, GMask, BMask);

    // SDL surface is created
    Result := SDL_CreateRGBSurface(Flags, WorkData.Width, WorkData.Height,
      Info.BytesPerPixel * 8, RMask, GMask, BMask, AMask);

    if Result <> nil then
    begin
      LineBytes := Info.BytesPerPixel * WorkData.Width;

      if SDL_MustLock(Result) then
        SDL_LockSurface(Result);

      // Pixels of image are copied to SDL surface
      if LineBytes = Result.pitch then
        Move(WorkData.Bits^, Result.pixels^, WorkData.Size)
      else
        for I := 0 to WorkData.Height - 1 do
          Move(PByteArray(WorkData.Bits)[I * LineBytes],
            PByteArray(Result.pixels)[I * Result.pitch], LineBytes);

      if SDL_MustLock(Result) then
        SDL_UnlockSurface(Result);

      // If surface is in indexed format, palette is copied
      if (Info.Format = ifIndex8) and (Result.format.palette <> nil) then
      begin
        Result.format.palette.ncolors := Info.PaletteEntries;
        for I := 0 to Info.PaletteEntries - 1 do
        begin
          Result.format.palette.colors[I].r := WorkData.Palette[I].R;
          Result.format.palette.colors[I].g := WorkData.Palette[I].G;
          Result.format.palette.colors[I].b := WorkData.Palette[I].B;
          Result.format.palette.colors[I].unused := 0;
        end;
      end;
    end;

    FreeImage(WorkData);
  end;
end;

function SaveSDLSurfaceToFile(const FileName: string; Surface: PSDL_Surface): Boolean;
var
  ImageData: TImageData;
begin
  Result := False;
  if CreateImageFromSDLSurface(Surface, ImageData) then
  begin
    Result := SaveImageToFile(FileName, ImageData);
    FreeImage(ImageData);
  end;
end;

function SaveSDLSurfaceToStream(const Ext: string; Stream: TStream; Surface: PSDL_Surface): Boolean;
var
  ImageData: TImageData;
begin
  Result := False;
  if CreateImageFromSDLSurface(Surface, ImageData) then
  begin
    Result := SaveImageToStream(Ext, Stream, ImageData);
    FreeImage(ImageData);
  end;
end;

function SaveSDLSurfaceToMemory(const Ext: string; Data: Pointer; var Size: LongInt; Surface: PSDL_Surface): Boolean;
var
  ImageData: TImageData;
begin
  Result := False;
  if CreateImageFromSDLSurface(Surface, ImageData) then
  begin
    Result := SaveImageToMemory(Ext, Data, Size, ImageData);
    FreeImage(ImageData);
  end;
end;

function CreateImageFromSDLSurface(Surface: PSDL_Surface; var ImageData: TImageData;
  OverrideFormat: TImageFormat): Boolean;
const
  SDL_A8R8G8B8Format: TSDL_PixelFormat = (palette: nil; BitsPerPixel: 32;
    BytesPerPixel: 4; Rloss: 0; Gloss: 0; Bloss: 0; Aloss: 0;
    Rshift: 16; Gshift: 8; Bshift: 0; Ashift: 24;
    Rmask: $00FF0000; Gmask: $0000FF00; Bmask: $000000FF; Amask: $FF000000;
    colorkey: 0; alpha: $FF);
var
  Format: TImageFormat;
  Converted: PSDL_Surface;
  Info: TImageFormatInfo;
  I, LineBytes: LongInt;

  function DetermineImageFormat: TImageFormat;
  var
    Fmt: TImageFormat;
  begin
    Result := ifUnknown;
    case Surface.format.BitsPerPixel of
      8: Result := ifIndex8;
      16:
        begin
          // go trough 16bit formats supported by Imaging and
          // if there is one that matches SDL format's masks then use it
          for Fmt := ifR5G6B5 to ifX4R4G4B4 do
          begin
            GetImageFormatInfo(Fmt, Info);
            if (Info.PixelFormat.ABitMask = Surface.format.AMask) and
              (Info.PixelFormat.RBitMask = Surface.format.RMask) and
              (Info.PixelFormat.GBitMask = Surface.format.GMask) and
              (Info.PixelFormat.BBitMask = Surface.format.BMask) then
              begin
                Result := Fmt;
                Break;
              end;
          end;
        end;
      24:
        begin
          if (Surface.format.RMask = $FF0000) and
             (Surface.format.GMask = $00FF00) and
             (Surface.format.BMask = $0000FF) then
               Result := ifR8G8B8;
        end;
      32:
        begin
          if (Surface.format.RMask = $00FF0000) and
             (Surface.format.GMask = $0000FF00) and
             (Surface.format.BMask = $000000FF) then
             if (Surface.format.AMask = $FF000000) then
               Result := ifA8R8G8B8
             else
               Result := ifX8R8G8B8
        end;
    end;
  end;

begin
  Result := False;
  FreeImage(ImageData);

  // See if surface is in format supported by Imaging and if it is
  // not then it is converted to A8R8G8B8
  Format := DetermineImageFormat;
  if Format = ifUnknown then
  begin
    Converted := SDL_ConvertSurface(Surface, @SDL_A8R8G8B8Format, SDL_SWSURFACE);
    Format := ifA8R8G8B8;
  end
  else
    Converted := Surface;

  if (Converted <> nil) and NewImage(Converted.w, Converted.h, Format, ImageData) then
  begin
    GetImageFormatInfo(Format, Info);
    LineBytes := Info.BytesPerPixel * ImageData.Width;

    if SDL_MustLock(Converted) then
      SDL_LockSurface(Converted);

    // New image is created and pixels are copied from SDL surface
    if LineBytes = Converted.pitch then
        Move(Converted.pixels^, ImageData.Bits^, ImageData.Size)
    else
      for I := 0 to ImageData.Height - 1 do
        Move(PByteArray(Converted.pixels)[I * Converted.pitch],
          PByteArray(ImageData.Bits)[I * LineBytes], LineBytes);

    if SDL_MustLock(Converted) then
      SDL_UnlockSurface(Converted);

    // Copy palette if necessary
    // If surface is in indexed format, palette is copied
    if (Info.Format = ifIndex8) and (Converted.format.palette <> nil) then
    begin
      for I := 0 to Min(Info.PaletteEntries, Converted.format.palette.ncolors) - 1 do
      begin
        ImageData.Palette[I].A := 255;
        ImageData.Palette[I].R := Converted.format.palette.colors[I].r;
        ImageData.Palette[I].G := Converted.format.palette.colors[I].g;
        ImageData.Palette[I].B := Converted.format.palette.colors[I].b;
      end;
    end;

    // Image is converted to override format
    if OverrideFormat <> ifUnknown then
      ConvertImage(ImageData, OverrideFormat);

    Result := True;
  end;

  if Converted <> Surface then
    SDL_FreeSurface(Converted);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Fixed possible int overflow in CreateSDLSurfaceFromImage.

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - unit created and initial stuff added
}

end.
