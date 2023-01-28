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

{ This unit contains image format alternative loader/saver for Jpeg images
  using Intel Jpeg Library (Win32 only).}
unit ImagingJpegIJL;

{$I ImagingOptions.inc}

{$IFNDEF WIN32}
  {$ERROR 'IJL 1.5 only for Win32'}
{$ENDIF}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingUtility, ImagingIO;

type
  { Class for loading/saving Jpeg images. This is alternative to
    default built-in Jpeg handler (which uses JpegLib).
    This handler uses Intel Jpeg Library 1.5 (DLL needed) and is
    much faster than JpegLib (2-4x). Also supports reading and writing of
    alpha channels in Jpeg files.}
  TJpegFileFormatIJL = class(TImageFileFormat)
  private
    FQuality: LongInt;
    procedure JpegError(Code: Integer);
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
  published
    { Controls Jpeg save compression quality. It is number in range 1..100.
      1 means small/ugly file, 100 means large/nice file. Accessible trough
      ImagingJpegQuality option.}
    property Quality: LongInt read FQuality write FQuality;
  end;

implementation

{$MINENUMSIZE 4}          // Min enum size: 4 B

uses
  Types;

const
  SJpegFormatName = 'JPEG Image (IJL)';
  SJpegMasks      = '*.jpg,*.jpeg,*.jfif,*.jpe,*.jif,*.jpa';
  JpegSupportedFormats: TImageFormats = [ifGray8, ifR8G8B8, ifA8R8G8B8];
  JpegDefaultQuality = 90;
  JpegDefaultProgressive = False;

resourcestring
  SJpegError = 'JPEG Error';

const
  { Jpeg file identifiers.}
  JpegMagic: TChar2 = #$FF#$D8;
  SIJLLibrary = 'ijl15.dll';

const
  IJL_SETUP = -1;
  IJL_OK    = 0;
  IJL_NONE  = 0;
  IJL_OTHER = 255;
  JBUFSIZE  = 4096;    // Size of file I/O buffer (4K).

type
  {
   Purpose:     Possible types of data read/write/other operations to be
                performed by the functions IJL_Read and IJL_Write.
                See the Developer's Guide for details on appropriate usage.
   Fields:
    IJL_JFILE_XXXXXXX   Indicates JPEG data in a stdio file.
    IJL_JBUFF_XXXXXXX   Indicates JPEG data in an addressable buffer.
  }
  TIJLIOType = (
    // Read JPEG parameters (i.e., height, width, channels, sampling, etc.)
    // from a JPEG bit stream.
    IJL_JFILE_READPARAMS       =  0,
    IJL_JBUFF_READPARAMS       =  1,
    // Read a JPEG Interchange Format image.
    IJL_JFILE_READWHOLEIMAGE   =  2,
    IJL_JBUFF_READWHOLEIMAGE   =  3,
    // Read JPEG tables from a JPEG Abbreviated Format bit stream.
    IJL_JFILE_READHEADER       =  4,
    IJL_JBUFF_READHEADER       =  5,
    // Read image info from a JPEG Abbreviated Format bit stream.
    IJL_JFILE_READENTROPY      =  6,
    IJL_JBUFF_READENTROPY      =  7,
    // Write an entire JFIF bit stream.
    IJL_JFILE_WRITEWHOLEIMAGE  =  8,
    IJL_JBUFF_WRITEWHOLEIMAGE  =  9,
    // Write a JPEG Abbreviated Format bit stream.
    IJL_JFILE_WRITEHEADER      = 10,
    IJL_JBUFF_WRITEHEADER      = 11,
    // Write image info to a JPEG Abbreviated Format bit stream.
    IJL_JFILE_WRITEENTROPY     = 12,
    IJL_JBUFF_WRITEENTROPY     = 13,

    // Scaled Decoding Options:
    // Reads a JPEG image scaled to 1/2 size.
    IJL_JFILE_READONEHALF      = 14,
    IJL_JBUFF_READONEHALF      = 15,
    // Reads a JPEG image scaled to 1/4 size.
    IJL_JFILE_READONEQUARTER   = 16,
    IJL_JBUFF_READONEQUARTER   = 17,
    // Reads a JPEG image scaled to 1/8 size.
    IJL_JFILE_READONEEIGHTH    = 18,
    IJL_JBUFF_READONEEIGHTH    = 19,
    // Reads an embedded thumbnail from a JFIF bit stream.
    IJL_JFILE_READTHUMBNAIL    = 20,
    IJL_JBUFF_READTHUMBNAIL    = 21
  );

  {
   Purpose:     Possible color space formats.
                Note these formats do *not* necessarily denote
                the number of channels in the color space.
                There exists separate "channel" fields in the
                JPEG_CORE_PROPERTIES data structure specifically
                for indicating the number of channels in the
                JPEG and/or DIB color spaces.}
  TIJL_COLOR = (
    IJL_RGB         = 1,  // Red-Green-Blue color space.
    IJL_BGR         = 2,  // Reversed channel ordering from IJL_RGB.
    IJL_YCBCR       = 3,  // Luminance-Chrominance color space as defined
                          // by CCIR Recommendation 601.
    IJL_G           = 4,  // Grayscale color space.
    IJL_RGBA_FPX    = 5,  // FlashPix RGB 4 channel color space that
                          // has pre-multiplied opacity.
    IJL_YCBCRA_FPX  = 6   // FlashPix YCbCr 4 channel color space that
                          // has pre-multiplied opacity.
    //IJL_OTHER  = 255    // Some other color space not defined by the IJL.
                          // (This means no color space conversion will
                          //  be done by the IJL.)
  );

  { Purpose:     Possible subsampling formats used in the JPEG.}
  TIJL_JPGSUBSAMPLING = (
    IJL_NOSUBSAMP  = 0,
    IJL_411        = 1,    // Valid on a JPEG w/ 3 channels.
    IJL_422        = 2,    // Valid on a JPEG w/ 3 channels.
    IJL_4114       = 3,    // Valid on a JPEG w/ 4 channels.
    IJL_4224       = 4     // Valid on a JPEG w/ 4 channels.
  );

  { Purpose:     Possible subsampling formats used in the DIB. }
  TIJL_DIBSUBSAMPLING = TIJL_JPGSUBSAMPLING;

  { Purpose:    This is the primary data structure between the IJL and
                the external user.  It stores JPEG state information
                and controls the IJL.  It is user-modifiable.
   Context:     Used by all low-level IJL routines to store
                pseudo-global information.}
  TJpegCoreProperties = packed record
    UseJPEGPROPERTIES : LongBool;            // default = 0
    // DIB specific I/O data specifiers.
    DIBBytes          : PByte;               // default = NULL
    DIBWidth          : UInt32;              // default = 0
    DIBHeight         : UInt32;              // default = 0
    DIBPadBytes       : UInt32;              // default = 0
    DIBChannels       : UInt32;              // default = 3
    DIBColor          : TIJL_COLOR;          // default = IJL_BGR
    DIBSubsampling    : TIJL_DIBSUBSAMPLING; // default = IJL_NONE
    // JPEG specific I/O data specifiers.
    JPGFile           : PAnsiChar;           // default = NULL
    JPGBytes          : PByte;               // default = NULL
    JPGSizeBytes      : UInt32;              // default = 0
    JPGWidth          : UInt32;              // default = 0
    JPGHeight         : UInt32;              // default = 0
    JPGChannels       : UInt32;              // default = 3
    JPGColor          : TIJL_COLOR;          // default = IJL_YCBCR
    JPGSubsampling    : TIJL_JPGSUBSAMPLING; // default = IJL_411
    JPGThumbWidth     : UInt32;              // default = 0
    JPGThumbHeight    : UInt32;              // default = 0
    // JPEG conversion properties.
    NeedsConvert      : LongBool;            // default = TRUE
    NeedsResample     : LongBool;            // default = TRUE
    Quality           : UInt32;              // default = 75
    // Low-level properties.
    PropsAndUnused    : array[0..19987] of Byte;
  end;
  PJpegCoreProperties = ^TJpegCoreProperties;

function ijlInit(var Props: TJpegCoreProperties): Integer; stdcall; external SIJLLibrary;
function ijlFree(var Props: TJpegCoreProperties): Integer; stdcall; external SIJLLibrary;
function ijlRead(var Props: TJpegCoreProperties; IoType  : TIJLIOTYPE): Integer; stdcall; external SIJLLibrary;
function ijlWrite(var Props: TJpegCoreProperties; IoType  : TIJLIOTYPE): Integer; stdcall; external SIJLLibrary;
function ijlErrorStr(Code : Integer) : PAnsiChar; stdcall; external SIJLLibrary;

{ TJpegFileFormatIJL class implementation }

procedure TJpegFileFormatIJL.Define;
begin
  inherited;
  FName := SJpegFormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := False;
  FSupportedFormats := JpegSupportedFormats;

  FQuality := JpegDefaultQuality;

  AddMasks(SJpegMasks);
  RegisterOption(ImagingJpegQuality, @FQuality);
end;

procedure TJpegFileFormatIJL.CheckOptionsValidity;
begin
  // Check if option values are valid
  if not (FQuality in [1..100]) then
    FQuality := JpegDefaultQuality;
end;

procedure TJpegFileFormatIJL.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  if Info.HasAlphaChannel then
    ConvertImage(Image, ifA8R8G8B8)
  else if Info.HasGrayChannel then
    ConvertImage(Image, ifGray8)
  else
    ConvertImage(Image, ifR8G8B8);
end;

function TJpegFileFormatIJL.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  ID: array[0..9] of AnsiChar;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    FillChar(ID, SizeOf(ID), 0);
    ReadCount := Read(Handle, @ID, SizeOf(ID));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(ID)) and
      CompareMem(@ID, @JpegMagic, SizeOf(JpegMagic));
  end;
end;

procedure TJpegFileFormatIJL.JpegError(Code: Integer);
begin
  raise EImagingError.Create(SJpegError + ': ' + ijlErrorStr(Code));
end;

function TJpegFileFormatIJL.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Props: TJpegCoreProperties;
  Status: Integer;
  Buffer: TDynByteArray;
  InputLen: Integer;
  JpegFmt: TImageFormat;
begin
  // Copy IO functions to global var used in JpegLib callbacks
  Result := False;
  SetLength(Images, 1);

  with Images[0] do
  try
    InputLen := GetInputSize(GetIO, Handle);

    Status := IjlInit(Props);
    if Status = IJL_OK then
    begin
      // Load input to memory and read Jpeg props
      SetLength(Buffer, InputLen);
      Props.JPGSizeBytes := InputLen;
      Props.JPGBytes := @Buffer[0];
      GetIO.Read(Handle, @Buffer[0], InputLen);
      Status := ijlRead(Props, IJL_JBUFF_READPARAMS);
    end;

    if Status = IJL_OK then
    begin
      // Set image and DIB props based on Jpeg params read from input
      case Props.JPGChannels of
        1:
          begin
            JpegFmt := ifGray8;
            Props.DIBColor := IJL_G;
          end;
        3:
          begin
            JpegFmt := ifR8G8B8;
            Props.DIBColor := IJL_BGR;
          end;
        4:
          begin
            JpegFmt := ifA8R8G8B8;
            Props.DIBColor := IJL_RGBA_FPX;
          end
      else
        Exit;
      end;

      NewImage(Props.JPGWidth, Props.JPGHeight, JpegFmt, Images[0]);

      Props.DIBWidth := Props.JPGWidth;
      Props.DIBHeight := Props.JPGHeight;
      Props.DIBChannels := Props.JPGChannels;
      Props.DIBPadBytes := 0;
      Props.DIBBytes := Bits;

      // Now read the image bits
      Status := ijlRead(Props, IJL_JBUFF_READWHOLEIMAGE);
    end;

    if Status <> IJL_OK then
      JpegError(Status);

    // Decoded images with alpha are in ABGR format so R and B channels are switched
    if JpegFmt = ifA8R8G8B8 then
      SwapChannels(Images[0], ChannelRed, ChannelBlue);

    Result := True;
  finally
    ijlFree(Props);
  end;
end;

function TJpegFileFormatIJL.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Props: TJpegCoreProperties;
  Status: Integer;
  Info: TImageFormatInfo;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Buffer: TDynByteArray;
begin
  Result := False;
  // Makes image to save compatible with Jpeg saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with ImageToSave do
  try
    Status := ijlInit(Props);
    if Status = IJL_OK then
    begin
      Info := GetFormatInfo(Format);
      // Set all the needed props
      Props.DIBWidth := Width;
      Props.DIBHeight := Height;
      Props.DIBChannels := Info.ChannelCount;
      Props.DIBPadBytes := 0;
      Props.DIBBytes := Bits;

      Props.Quality := FQuality;

      Props.JPGWidth := Width;
      Props.JPGHeight := Height;
      Props.JPGChannels := Info.ChannelCount;
      SetLength(Buffer, Size);
      Props.JPGSizeBytes := Size;
      Props.JPGBytes := @Buffer[0];

      case Info.ChannelCount of
        1:
          begin
            Props.DIBColor := IJL_G;
            Props.JPGColor := IJL_G;
            Props.JPGSubsampling := IJL_NOSUBSAMP;
          end;
        3:
          begin
            Props.DIBColor := IJL_BGR;
            Props.JPGColor := IJL_YCBCR;
            Props.JPGSubsampling := IJL_411;
          end;
        4:
          begin
            Props.DIBColor := IJL_RGBA_FPX;
            Props.JPGColor := IJL_YCBCRA_FPX;
            Props.JPGSubsampling := IJL_4114;
            SwapChannels(ImageToSave, ChannelRed, ChannelBlue); // IJL expects ABGR order
          end;
      end;

      // Encode image
      Status := ijlWrite(Props, IJL_JBUFF_WRITEWHOLEIMAGE);
    end;

    if Status <> IJL_OK then
      JpegError(Status);

    // Write temp buffer to file
    GetIO.Write(Handle, @Buffer[0], Props.JPGSizeBytes);

    Result := True;
  finally
    ijlFree(Props);
    if MustBeFreed then
      FreeImage(ImageToSave)
    else if Format = ifA8R8G8B8 then
      SwapChannels(ImageToSave, ChannelRed, ChannelBlue); // Swap image back to ARGB if not temp
  end;
end;

initialization
  RegisterImageFileFormat(TJpegFileFormatIJL);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Initial version created.
}

end.
