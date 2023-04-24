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

{ This is helper unit that registers all image file formats in Extensions package
  to Imaging core loading and saving functions. Just put this unit in your uses
  clause instead of adding every unit that provides new file format support.
  Also new constants for SetOption/GetOption functions for new file formats
  are located here.}
unit ImagingExtFileFormats;

{$I ImagingOptions.inc}

{$ifndef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
  (*
  Castle Game Engine:
  Disable Tiff and JPEG2000 support on all platforms.

  1.Because the implementation is not cross-platform enough.
    This is bad for users who expect cross-platform support
    for image formats.

    E.g. tiff:

    - It is disabled on Arm (32-bit)

    - It is uncomfortable to use on Anroid/iOS/NintendoSwitch where
      we don't automatically distribute libtiff.
      And lack of it means errors at startup, like this on Android:

      E test_bump_mapping: MainActivity: JNI: Could not load libtest_bump_mapping_android.so, exception UnsatisfiedLinkError: dlopen failed: cannot locate symbol "TIFFNumberOfDirectories" referenced by "/data/app/~~Q2PlsieWYj2N3wYOOgQPeQ==/io.castleengine.test.bump.mapping-fDdlkPhP1GVaw7My9A7auw==/lib/arm64/libtest_bump_mapping_android.so"..

  2.It's also cumbersome to use in fpmake.pp.
    While you can add unit conditionally, like below

    { OpenJpeg only compiles on certain platforms,
      see $ifdef in ImagingJpeg2000 (ImagingJpeg2000 compiles but is empty
      on unsupported platforms). }
    if ((Defaults.OS in AllWindowsOSes) and (Defaults.CPU in [x86])) or
      ((Defaults.OS = Linux) and (Defaults.CPU in [x86, x86_64])) or
      ((Defaults.OS = macOS) and (Defaults.CPU in [x86])) then
    begin
      P.Targets.AddUnit('OpenJpeg.pas');
    end;

    ... but the units also require external .o/.a files which I don't know how to make
    work with fpmake "install" (so that other applications can use them too).

  Developers can override this defining CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS.
  *)

  {$DEFINE DONT_LINK_JPEG2000}    // link support for JPEG2000 images
  {$DEFINE DONT_LINK_TIFF}        // link support for TIFF images

{$endif CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}

{.$DEFINE DONT_LINK_PSD}         // link support for PSD images
{.$DEFINE DONT_LINK_PCX}         // link support for PCX images
{.$DEFINE DONT_LINK_XPM}         // link support for XPM images

{$IFNDEF FULL_FEATURE_SET}
  {$DEFINE DONT_LINK_ELDER}     // link support for Elder Imagery images
{$ENDIF}

interface

const
  { Those are new options for GetOption/SetOption interface. }

  { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
    1 means small/ugly file, 100 means large/nice file. Default is 80.}
  ImagingJpeg2000Quality             = 55;
  { Controls whether JPEG 2000 image is saved with full file headers or just
    as code stream. Default value is False (0).}
  ImagingJpeg2000CodeStreamOnly      = 56;
  { Specifies JPEG 2000 image compression type. If True (1), saved JPEG 2000 files
    will be losslessly compressed. Otherwise lossy compression is used.
    Default value is False (0).}
  ImagingJpeg2000LosslessCompression = 57;
  { Specifies JPEG 2000 output scaling. Since JPEG 2000 supports arbitrary Bit Depths,
    the default behaviour is to scale the images up to the next 8^n bit depth.
    This can be disabled by setting this option to False.
    Default value is True. }
  ImagingJpeg2000ScaleOutput = 58;
  { Specifies compression scheme used when saving TIFF images. Supported values
    are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
    5 (CCITT Group 4 fax encoding - for binary images only).
    Default is 1 (LZW). Note that not all images can be stored with
    JPEG compression - these images will be saved with default compression if
    JPEG is set.}
  ImagingTiffCompression             = 65;
  { Controls compression quality when selected TIFF compression is Jpeg.
    It is number in range 1..100. 1 means small/ugly file,
    100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
  ImagingTiffJpegQuality             = 66;
  { If enabled image data is saved as layer of PSD file. This is required
    to get proper transparency when opened in Photoshop for images with
    alpha data (will be opened with one layer, RGB color channels, and transparency).
    If you don't need this Photoshop compatibility turn this option off as you'll get
    smaller file (will be opened in PS as background raster with RGBA channels).
    Default value is True (1). }
  ImagingPSDSaveAsLayer              = 70;

implementation

uses
{$IFNDEF DONT_LINK_FILE_FORMATS}
{$IFNDEF DONT_LINK_JPEG2000}
  ImagingJpeg2000,
{$ENDIF}
{$IFNDEF DONT_LINK_TIFF}
  ImagingTiff,
{$ENDIF}
{$IFNDEF DONT_LINK_PSD}
  ImagingPsd,
{$ENDIF}
{$IFNDEF DONT_LINK_PCX}
  ImagingPcx,
{$ENDIF}
{$IFNDEF DONT_LINK_XPM}
  ImagingXpm,
{$ENDIF}
{$IFNDEF DONT_LINK_ELDER}
  ElderImagery,
{$ENDIF}
{$ENDIF}
  Imaging;

{
  File Notes:

 -- TODOS -----------------------------------------------------
    - nothing now

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Added Group 4 Fax encoding as compression for TIFF files.
    - Added ImagingTiffJpegQuality option.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Allowed JPEG2000 for Mac OS X x86

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - ElderImagery formats are disabled by default, TIFF enabled.
    - Changed _LINK_ symbols according to changes in ImagingOptions.inc.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Allowed JPEG2000 for x86_64 CPUS in Linux

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Better IF conditional to disable JPEG2000 on unsupported platforms.
    - Added PSD and TIFF related stuff.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Created with initial stuff.

}

end.
