{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Process icons to ../exported_dark_*/ . }
program process_icons;

{$ifdef MSWINDOWS}{$apptype CONSOLE}{$endif}

uses SysUtils,
  CastleImages, CastleColors, CastleVectors;

procedure ProcessIcon(const InputFileName, OutputFileName, ColorHex : String);
var
  Image: TRGBAlphaImage;
  Color: TVector3Byte;
  X, Y: Integer;
begin
  Image := LoadImage(InputFileName, [TRGBAlphaImage]) as TRGBAlphaImage;
  try
    Color := Vector3Byte(HexToColorRGB(ColorHex));
    for Y := 0 to Image.Height - 1 do
      for X := 0 to Image.Width - 1 do
        // change RGB portion of the color, leave alpha unchanged
        PVector3Byte(Image.PixelPtr(X, Y))^ := Color;
    SaveImage(Image, OutputFileName);
    Writeln('Made: ', OutputFileName);
  finally FreeAndNil(Image) end;
end;

const
  ColorInactive = 'CFC9BD';
  ColorActive = 'F0913A';
  ColorDisabled = '6B655C';

  ExportedDarkInactivePath = '../exported_dark_inactive/';
  ExportedDarkActivePath = '../exported_dark_active/';
  ExportedDarkDisabledPath = '../exported_dark_disabled/';

  IconsToProcessPath = '../exported/';
  IconsToProcessNames: array [0..9] of String = (
    'tool-interact',
    'tool-rotate',
    'tool-scale',
    'tool-select',
    'tool-translate',
    'stop-solid',
    'play-solid',
    'pause-solid',
    'info-solid',
    'file-plus'
  );
  IconsToProcessSizes: array [0..4] of String = (
    '16x16',
    '20x20',
    '24x24',
    '32x32',
    '48x48'
  );
var
  NameIndex, SizeIndex: Integer;
  BaseFileName: String;
begin
  ForceDirectories(ExportedDarkInactivePath);
  ForceDirectories(ExportedDarkActivePath);
  ForceDirectories(ExportedDarkDisabledPath);

  for NameIndex := 0 to High(IconsToProcessNames) do
    for SizeIndex := 0 to High(IconsToProcessSizes) do
    begin
      BaseFileName :=
        IconsToProcessNames[NameIndex] + '_' +
        IconsToProcessSizes[SizeIndex] + '.png';
      ProcessIcon(IconsToProcessPath + BaseFileName,
        ExportedDarkInactivePath + BaseFileName,
        ColorInactive);
      ProcessIcon(IconsToProcessPath + BaseFileName,
        ExportedDarkActivePath + BaseFileName,
        ColorActive);
      ProcessIcon(IconsToProcessPath + BaseFileName,
        ExportedDarkDisabledPath + BaseFileName,
        ColorDisabled);
    end;
end.
