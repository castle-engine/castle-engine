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
{ This unit contains image format loader/saver for SKY file format used
  in Daggerfall to store sky backdrops.}
unit ElderImagerySky;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ElderImagery;

type
  { Class for loading and saving of images in SKY format. It is
    8 bit indexed format found in Daggerfall, and maybe other old Bethesda
    games. Files are named SKY##.DAT and each contains two sets of 32 images
    (512 by 220 pixels), each with its palette. First set contains sky
    without sun, second set sky with sun. }
  TSKYFileFormat = class(TElderFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SSKYFormatName = 'Daggerfall Sky Images';
  SSKYMasks      = '*.dagsky,sky??.dat';

  SkyWidth     =    512;
  SkyHeight    =    220;
  SkyCount     =     64;
  DataOffset   = 549120;
  PalFileSize  =    776;
  SkyImageSize = SkyWidth * SkyHeight;

  SkyFileId: array[0..5] of Byte = ($08, $03, $00, $00, $23, $B1);

{ TSKYFileFormat class implementation }

procedure TSKYFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffMultiImage];
  FName := SSKYFormatName;
  AddMasks(SSKYMasks);
end;

function TSKYFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  I: Integer;
  Pal24: TPalette24Size256;

  procedure CopyPalette(Dest: PPalette32);
  var
    I: Integer;
  begin
    for I := 0 to 255 do
    begin
      Dest[I].A := 255;
      Dest[I].R := Pal24[I].B;
      Dest[I].G := Pal24[I].G;
      Dest[I].B := Pal24[I].R;
    end;
  end;

begin
  SetLength(Images, SkyCount);
  for I := 0 to SkyCount - 1 do
  begin
    NewImage(SkyWidth, SkyHeight, ifIndex8, Images[I]);
    // Read corresponding palette from file
    GetIO.Seek(Handle, PalFileSize * (I mod 32) + 8, smFromBeginning);
    GetIO.Read(Handle, @Pal24, SizeOf(Pal24));
    CopyPalette(Images[I].Palette);
    // Now read image pixels
    GetIO.Seek(Handle, DataOffset + I * SkyImageSize, smFromBeginning);
    GetIO.Read(Handle, Images[I].Bits, SkyImageSize);
  end;
  Result := True;
end;

function TSKYFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Id: array[0..5] of Byte;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    FillChar(ID, SizeOf(Id), 0);
    ReadCount := Read(Handle, @Id, SizeOf(Id));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Id)) and
      CompareMem(@Id, @SkyFileId, SizeOf(SkyFileId));
  end;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Initial version created.
}

end.
