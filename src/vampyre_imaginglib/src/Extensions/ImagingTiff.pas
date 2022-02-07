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

unit ImagingTiff;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility, ImagingIO;

type
  { TIFF (Tag Image File Format) loader/saver base class.}
  TBaseTiffFileFormat = class(TImageFileFormat)
  protected
    FCompression: Integer;
    FJpegQuality: Integer;
    procedure Define; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    { Specifies compression scheme used when saving TIFF images. Supported values
      are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
      5 (CCITT Group 4 fax encoding - for binary images only).
      Default is 1 (LZW). Note that not all images can be stored with
      JPEG compression - these images will be saved with default compression if
      JPEG is set.}
    property Compression: Integer read FCompression write FCompression;
    { Controls compression quality when selected TIFF compression is Jpeg.
      It is number in range 1..100. 1 means small/ugly file,
      100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
    property JpegQuality: Integer read FJpegQuality write FJpegQuality;
  end;

const
  TiffCompressionOptionNone        = 0;
  TiffCompressionOptionLzw         = 1;
  TiffCompressionOptionPackbitsRle = 2;
  TiffCompressionOptionDeflate     = 3;
  TiffCompressionOptionJpeg        = 4;
  TiffCompressionOptionGroup4      = 5;

  { Read only metadata info - name of compression scheme (LZW, none, JPEG, G4, ...)
    used in last loaded TIFF. }
  SMetaTiffCompressionName = 'TiffCompressionName';
  { Original resolution unit of loaded TIFF. Type is UInt.
    RESUNIT_NONE                        = 1;       // no meaningful units
    RESUNIT_INCH                        = 2;       // english
    RESUNIT_CENTIMETER                  = 3;       // metric }
  SMetaTiffResolutionUnit = 'TiffResolutionUnit';

implementation

{$IFNDEF DONT_LINK_FILE_FORMATS}

// So far we have only one TIFF support implementation - libtiff
{$DEFINE USE_LIBTIFF}

// libtiff for FPC ARM is disabled by default due to potential hardfp/softfp
// ABI problems (without linking to any lib FPC generated binary does not call "ld"
// and hardfp exe can run on softfp target). If you know what you're doing enable it.
{$IF Defined(FPC) and Defined(CPUARM)}
  {$UNDEF USE_LIBTIFF}
{$IFEND}

// Not even dynamic linking works at the moment
{$IF Defined(DELPHI) and Defined(MACOS))}
  {$UNDEF USE_LIBTIFF}
{$IFEND}

// Also disable for Delphi ARM targets
{$IF Defined(DELPHI) and Defined(CPUARM))}
  {$UNDEF USE_LIBTIFF}
{$IFEND}

uses
{$IFDEF USE_LIBTIFF}
  ImagingTiffLib,
{$ENDIF}
  ImagingExtFileFormats;

{$ENDIF}

const
  STiffFormatName = 'Tagged Image File Format';
  STiffMasks      = '*.tif,*.tiff';
  TiffDefaultCompression = 1;
  TiffDefaultJpegQuality = 90;

const
  TiffBEMagic: TChar4 = 'MM'#0#42;
  TiffLEMagic: TChar4 = 'II'#42#0;

{
  TBaseTiffFileFormat implementation
}

procedure TBaseTiffFileFormat.Define;
begin
  inherited;
  FName := STiffFormatName;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FCompression := TiffDefaultCompression;
  FJpegQuality := TiffDefaultJpegQuality;

  AddMasks(STiffMasks);
  RegisterOption(ImagingTiffCompression, @FCompression);
  RegisterOption(ImagingTiffJpegQuality, @FJpegQuality);
end;

function TBaseTiffFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Magic: TChar4;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Magic, SizeOf(Magic));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Magic)) and
      ((Magic = TiffBEMagic) or (Magic = TiffLEMagic));
  end;
end;

end.
