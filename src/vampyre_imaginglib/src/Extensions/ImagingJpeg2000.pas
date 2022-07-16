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

{ This unit contains image format loader/saver for Jpeg 2000 images.}
unit ImagingJpeg2000;

{$I ImagingOptions.inc}

interface

{
  JPEG2000 support needs precompiled C object files and only some targets are
  available.

  Delphi targets: Windows 32b
  FPC targets: Windows 32b, Linux 32+64b, OSX 32b
}

{$IF (Defined(DCC) and Defined(MSWINDOWS) and Defined(CPUX86)) or
     (Defined(FPC) and Defined(MSWINDOWS) and Defined(CPUX86)) or
     (Defined(FPC) and Defined(LINUX) and (Defined(CPUX86) or Defined(CPUX64))) or
     (Defined(FPC) and Defined(MACOS) and Defined(CPUX86))}
uses
  SysUtils, ImagingTypes, Imaging, ImagingColors, ImagingIO, ImagingUtility,
  ImagingExtFileFormats, OpenJpeg;

type
  { Type Jpeg 2000 file (needed for OpenJPEG codec settings).}
  TJpeg2000FileType = (jtInvalid, jtJP2, jtJ2K, jtJPT);

  { Class for loading/saving Jpeg 2000 images. It uses OpenJPEG library
    compiled to object files and linked to Object Pascal program. Jpeg 2000
    supports wide variety of data formats. You can have arbitrary number
    of components/channels, each with different bitdepth and optional
    "signedness". Jpeg 2000 images can be lossy or lossless compressed.

    Imaging can load most data formats (except images
    with component bitdepth > 16 => no Imaging data format equivalents).
    Components with sample separation are loaded correctly, ICC profiles
    or palettes are not used, YCbCr images are translated to RGB.

    You can set various options when saving Jpeg-2000 images. Look at
    properties of TJpeg2000FileFormat for details.}
  TJpeg2000FileFormat = class(TImageFileFormat)
  private
    FQuality: LongInt;
    FCodeStreamOnly: LongBool;
    FLosslessCompression: LongBool;
    FScaleOutput: LongBool;
    function GetFileType(Handle: TImagingHandle): TJpeg2000FileType;
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
    { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
      1 means small/ugly file, 100 means large/nice file. Accessible trough
      ImagingJpeg2000Quality option. Default value is 80.}
    property Quality: LongInt read FQuality write FQuality;
    { Controls whether JPEG 2000 image is saved with full file headers or just
      as code stream. Default value is False. Accessible trough
      ImagingJpeg2000CodeStreamOnly option.}
    property CodeStreamOnly: LongBool read FCodeStreamOnly write FCodeStreamOnly;
    { Specifies JPEG 2000 image compression type. If True, saved JPEG 2000 files
      will be losslessly compressed. Otherwise lossy compression is used.
      Default value is False. Accessible trough
      ImagingJpeg2000LosslessCompression option.}
    property LosslessCompression: LongBool read FLosslessCompression write FLosslessCompression;
    { Specifies JPEG 2000 output scaling. Since JPEG 2000 supports arbitrary Bit Depths,
      the default behaviour is to scale the images up tp the next 8^n bit depth.
      This can be disabled by setting this option to False.
      Default value is True. Accessible through
      ImagingJpeg2000ScaleOutput option.}
    property ScaleOutput: LongBool read FScaleOutput write FScaleOutput;
  end;

implementation

const
  SJpeg2000FormatName = 'JPEG 2000 Image';
  SJpeg2000Masks      = '*.jp2,*.j2k,*.j2c,*.jpx,*.jpc';
  Jpeg2000SupportedFormats: TImageFormats = [ifGray8, ifGray16,
    ifA8Gray8, ifA16Gray16, ifR8G8B8, ifR16G16B16, ifA8R8G8B8, ifA16R16G16B16];
  Jpeg2000DefaultQuality = 80;
  Jpeg2000DefaultCodeStreamOnly = False;
  Jpeg2000DefaultLosslessCompression = False;
  Jpeg2000DefaultScaleOutput = True;

const
  JP2Signature: TChar8 = #0#0#0#$0C#$6A#$50#$20#$20;
  J2KSignature: TChar4 = #$FF#$4F#$FF#$51;

procedure TJpeg2000FileFormat.Define;
begin
  inherited;
  FName := SJpeg2000FormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := Jpeg2000SupportedFormats;

  FQuality := Jpeg2000DefaultQuality;
  FCodeStreamOnly := Jpeg2000DefaultCodeStreamOnly;
  FLosslessCompression := Jpeg2000DefaultLosslessCompression;
  FScaleOutput := Jpeg2000DefaultScaleOutput;

  AddMasks(SJpeg2000Masks);
  RegisterOption(ImagingJpeg2000Quality, @FQuality);
  RegisterOption(ImagingJpeg2000CodeStreamOnly, @FCodeStreamOnly);
  RegisterOption(ImagingJpeg2000LosslessCompression, @FLosslessCompression);
  RegisterOption(ImagingJpeg2000ScaleOutput, @FScaleOutput);
end;

procedure TJpeg2000FileFormat.CheckOptionsValidity;
begin
  // Check if option values are valid
  if not (FQuality in [1..100]) then
    FQuality := Jpeg2000DefaultQuality;
end;

function TJpeg2000FileFormat.GetFileType(Handle: TImagingHandle): TJpeg2000FileType;
var
  ReadCount: LongInt;
  Id: TChar8;
begin
  Result := jtInvalid;
  with GetIO do
  begin
    ReadCount := Read(Handle, @Id, SizeOf(Id));
    if ReadCount = SizeOf(Id) then
    begin
      // Check if we have full JP2 file format or just J2K code stream
      if CompareMem(@Id, @JP2Signature, SizeOf(JP2Signature)) then
        Result := jtJP2
      else if CompareMem(@Id, @J2KSignature, SizeOf(J2KSignature)) then
        Result := jtJ2K;
    end;
    Seek(Handle, -ReadCount, smFromCurrent);
  end;
end;

function TJpeg2000FileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
type
  TChannelInfo = record
    DestOffset: Integer;
    CompType: OPJ_COMPONENT_TYPE;
    Shift: Integer;
    SrcMaxValue: Integer;
    DestMaxValue: Integer;
  end;
var
  FileType: TJpeg2000FileType;
  Buffer: PByte;
  BufferSize, ChannelSize, I: Integer;
  Info: TImageFormatInfo;
  dinfo: popj_dinfo_t;
  parameters: opj_dparameters_t;
  cio: popj_cio_t;
  image: popj_image_t;
  StartPos: Int64;
  Channels: array of TChannelInfo;

  procedure WriteSample(Dest: PByte; ChannelSize, Value: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    case ChannelSize of
      1: Dest^ := Value;
      2: PWord(Dest)^ := Value;
      4: PUInt32(Dest)^ := Value;
    end;
  end;

  procedure CopySample(Src, Dest: PByte; ChannelSize: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    case ChannelSize of
      1: Dest^ := Src^;
      2: PWord(Dest)^ := PWord(Src)^;
      4: PUInt32(Dest)^ := PUInt32(Src)^;
    end;
  end;

  procedure ReadChannel(const Image: TImageData; const Info: TChannelInfo; const Comp: opj_image_comp; BytesPerPixel: Integer);
  var
    X, Y, SX, SY, SrcIdx, LineBytes: Integer;
    DestPtr, NewPtr, LineUpPtr: PByte;
    DontScaleSamples: Boolean;
  begin
    DontScaleSamples := (Info.SrcMaxValue = Info.DestMaxValue) or not FScaleOutput;
    LineBytes := Image.Width * BytesPerPixel;
    DestPtr := @PByteArray(Image.Bits)[Info.DestOffset];
    SrcIdx := 0;

    if (Comp.dx = 1) and (Comp.dy = 1) then
    begin
      // X and Y sample separation is 1 so just need to assign component values
      // to image pixels one by one
      for Y := 0 to Image.Height * Image.Width - 1 do
      begin
        if DontScaleSamples then
          WriteSample(DestPtr, ChannelSize, Comp.data[SrcIdx] + Info.Shift)
        else
          WriteSample(DestPtr, ChannelSize, MulDiv(Comp.data[SrcIdx] + Info.Shift, Info.DestMaxValue, Info.SrcMaxValue));

        Inc(SrcIdx);
        Inc(DestPtr, BytesPerPixel);
      end;
    end
    else
    begin
      // Sample separation is active - component is sub-sampled. Real component
      // dimensions are [Comp.w * Comp.dx, Comp.h * Comp.dy]
      for Y := 0 to Comp.h - 1 do
      begin
        LineUpPtr := @PByteArray(Image.Bits)[Y * Comp.dy * LineBytes + Info.DestOffset];
        DestPtr := LineUpPtr;

        for X := 0 to Comp.w - 1 do
        begin
          if DontScaleSamples then
            WriteSample(DestPtr, ChannelSize, Comp.data[SrcIdx] + Info.Shift)
          else
            WriteSample(DestPtr, ChannelSize, MulDiv(Comp.data[SrcIdx] + Info.Shift, Info.DestMaxValue, Info.SrcMaxValue));

          NewPtr := DestPtr;

          for SX := 1 to Comp.dx - 1 do
          begin
            if X * Comp.dx + SX >= Image.Width then Break;
            // Replicate pixels on line
            Inc(NewPtr, BytesPerPixel);
            CopySample(DestPtr, NewPtr, ChannelSize);
          end;

          Inc(SrcIdx);
          Inc(DestPtr, BytesPerPixel * Comp.dx);
        end;

        for SY := 1 to Comp.dy - 1 do
        begin
          if Y * Comp.dy + SY >= Image.Height then Break;
          // Replicate line
          NewPtr := @PByteArray(Image.Bits)[(Y * Comp.dy + SY) * LineBytes + Info.DestOffset];
          for X := 0 to Image.Width - 1 do
          begin
            CopySample(LineUpPtr, NewPtr, ChannelSize);
            Inc(LineUpPtr, BytesPerPixel);
            Inc(NewPtr, BytesPerPixel);
          end;
        end;
      end;
    end;
  end;

  procedure ConvertYCbCrToRGB(Pixels: PByte; NumPixels, BytesPerPixel: Integer);
  var
    I: Integer;
    PixPtr: PByte;
    CY, CB, CR: Byte;
    CYW, CBW, CRW: Word;
  begin
    PixPtr := Pixels;
    for I := 0 to NumPixels - 1 do
    begin
      if BytesPerPixel in [3, 4] then
      with PColor24Rec(PixPtr)^ do
      begin
        CY := R;
        CB := G;
        CR := B;
        YCbCrToRGB(CY, CB, CR, R, G, B);
      end
      else
      with PColor48Rec(PixPtr)^ do
      begin
        CYW := R;
        CBW := G;
        CRW := B;
        YCbCrToRGB16(CYW, CBW, CRW, R, G, B);
      end;
      Inc(PixPtr, BytesPerPixel);
    end;
  end;

begin
  Result := False;
  image := nil;
  cio := nil;
  opj_set_default_decoder_parameters(@parameters);
  // Determine which codec to use
  FileType := GetFileType(Handle);
  case FileType of
    jtJP2: dinfo := opj_create_decompress(CODEC_JP2);
    jtJ2K: dinfo := opj_create_decompress(CODEC_J2K);
    jtJPT: dinfo := opj_create_decompress(CODEC_JPT);
  else
    Exit;
  end;
  // Set event manager to nil to avoid getting messages
  dinfo.event_mgr := nil;
  // Currently OpenJPEG can load images only from memory so we have to
  // preload whole input to mem buffer. Not good but no other way now.
  // At least we set stream pos to end of JP2 data after loading (we will now
  // the exact size by then).
  StartPos := GetIO.Tell(Handle);
  BufferSize := ImagingIO.GetInputSize(GetIO, Handle);
  GetMem(Buffer, BufferSize);

  SetLength(Images, 1);
  with GetIO, Images[0] do
  try
    Read(Handle, Buffer, BufferSize);
    cio := opj_cio_open(opj_common_ptr(dinfo), Buffer, BufferSize);
    opj_setup_decoder(dinfo, @parameters);
    // Decode image
    image := opj_decode(dinfo, cio);
    if image = nil then
      Exit;

    // Determine which Imaging data format to use according to
    // decoded image components
    case image.numcomps of
      2: case image.comps[0].prec of
            1..8: Format := ifA8Gray8;
           9..16: Format := ifA16Gray16;
         end;
      3: case image.comps[0].prec of
            1..8: Format := ifR8G8B8;
           9..16: Format := ifR16G16B16;
         end;
      4: case image.comps[0].prec of
            1..8: Format := ifA8R8G8B8;
           9..16: Format := ifA16R16G16B16;
         end;
    else
      // There is only one component or there is more than four =>
      // just load the first one as gray
      case image.comps[0].prec of
           1..8: Format := ifGray8;
          9..16: Format := ifGray16;
         17..32: Format := ifGray32;
       end;
    end;
    // Exit if no compatible format was found
    if Format = ifUnknown then
      Exit;

    NewImage(image.x1 - image.x0, image.y1 - image.y0, Format, Images[0]);
    Info := GetFormatInfo(Format);
    ChannelSize := Info.BytesPerPixel div Info.ChannelCount;
    SetLength(Channels, Info.ChannelCount);

    // Get information about all channels/components of JP2 file
    for I := 0 to Info.ChannelCount - 1 do
    begin
      // Get component type for this channel and based on this
      // determine where in dest image bits write this channel's data
      Channels[I].CompType := image.comps[I].comp_type;
      case Channels[I].CompType of
        COMPTYPE_UNKNOWN:
          begin
            if Info.ChannelCount <> 4 then
            begin
              // Missing CDEF box in file - usually BGR order
              Channels[I].DestOffset := image.numcomps - I - 1
            end
            else
            begin
              // Missing CDEF box in file - usually ABGR order
              if I = 3 then
                Channels[I].DestOffset := 3
              else
                Channels[I].DestOffset := image.numcomps - I - 2
            end;
          end;
        COMPTYPE_R:       Channels[I].DestOffset := 2;
        COMPTYPE_G:       Channels[I].DestOffset := 1;
        COMPTYPE_B:       Channels[I].DestOffset := 0;
        COMPTYPE_CB:      Channels[I].DestOffset := 1;
        COMPTYPE_CR:      Channels[I].DestOffset := 0;
        COMPTYPE_OPACITY: Channels[I].DestOffset := 3;
        COMPTYPE_Y:
          case image.color_space of
            CLRSPC_SYCC: Channels[I].DestOffset := 2; // Y is intensity part of YCC
            CLRSPC_GRAY: Channels[I].DestOffset := 0; // Y is independent gray channel
          end;
      end;
      // Scale channel offset
      Channels[I].DestOffset := Channels[I].DestOffset * ChannelSize;
      // Signed componets must be scaled to [0, 1] interval (depends on precision)
      if image.comps[I].sgnd = 1 then
        Channels[I].Shift := 1 shl (image.comps[I].prec - 1);
      // Max channel values used to easier scaling of precisions
      // not supported by Imaging to supported ones (like 12bits etc.).
      Channels[I].SrcMaxValue := 1 shl image.comps[I].prec - 1;
      Channels[I].DestMaxValue := 1 shl (ChannelSize * 8) - 1;
    end;

    // Images components are stored separately in JP2, each can have
    // different dimensions, bitdepth, ...
    for I := 0 to Info.ChannelCount - 1 do
      ReadChannel(Images[0], Channels[I], image.comps[I], Info.BytesPerPixel);

    // If we have YCbCr image we need to convert it to RGB
    if (image.color_space = CLRSPC_SYCC) and (Info.ChannelCount in [3, 4]) then
      ConvertYCbCrToRGB(Bits, Width * Height, Info.BytesPerPixel);

    // Set the input position just after end of image
    Seek(Handle, StartPos + Cardinal(cio.bp) - Cardinal(cio.start), smFromBeginning);

    Result := True;
  finally
    opj_image_destroy(image);
    opj_destroy_decompress(dinfo);
    opj_cio_close(cio);
    FreeMem(Buffer);
  end;
end;

function TJpeg2000FileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  TargetSize, Rate: Single;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Info: TImageFormatInfo;
  I, Z, InvZ, Channel, ChannelSize, NumPixels: Integer;
  Pix: PByte;
  image: popj_image_t;
  cio: popj_cio_t;
  cinfo: popj_cinfo_t;
  parameters: opj_cparameters_t;
  compparams: popj_image_cmptparm_array;
  ColorSpace: OPJ_COLOR_SPACE;

  function GetComponentType(Comp: Integer): OPJ_COMPONENT_TYPE;
  begin
     if Info.HasAlphaChannel and (Comp = Info.ChannelCount - 1) then
       Result := COMPTYPE_OPACITY
     else if Info.HasGrayChannel then
       Result := COMPTYPE_Y
     else if Comp = 2 then
       Result := COMPTYPE_B
     else if Comp = 1 then
       Result := COMPTYPE_G
     else if Comp = 0 then
       Result := COMPTYPE_R
     else
       Result := COMPTYPE_UNKNOWN;
  end;

begin
  Result := False;
  image := nil;
  compparams := nil;
  cinfo := nil;
  cio := nil;
  // Makes image to save compatible with Jpeg 2000 saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    ChannelSize := Info.BytesPerPixel div Info.ChannelCount;

    // Fill component info structures and then create OpenJPEG image
    GetMem(compparams, Info.ChannelCount * SizeOf(opj_image_comptparm));
    for I := 0 to Info.ChannelCount - 1 do
    with compparams[I] do
    begin
      dx := 1;
      dy := 1;
      w  := Width;
      h  := Height;
      prec := (Info.BytesPerPixel div Info.ChannelCount) * 8;
      bpp := prec;
      sgnd := 0;
      comp_type := GetComponentType(I);
      x0 := 0;
      y0 := 0;
    end;

    if Info.HasGrayChannel then
      ColorSpace := CLRSPC_GRAY
    else
      ColorSpace := CLRSPC_SRGB;

    image := opj_image_create(Info.ChannelCount, @compparams[0], ColorSpace);
    if image = nil then Exit;
    image.x1 := Width;
    image.y1 := Height;

    if FCodeStreamOnly then
      cinfo := opj_create_compress(CODEC_J2K)
    else
      cinfo := opj_create_compress(CODEC_JP2);

    // Set event manager to nil to avoid getting messages
    cinfo.event_mgr := nil;  
    // Set compression parameters based current file format properties
    opj_set_default_encoder_parameters(@parameters);
    parameters.cod_format := Iff(FCodeStreamOnly, 0, 1);
    parameters.numresolution := 6;
    parameters.tcp_numlayers := 1;
    parameters.cp_disto_alloc := 1;
    if FLosslessCompression then
    begin
      // Set rate to 0 -> lossless
      parameters.tcp_rates[0] := 0;
    end
    else
    begin
      // Quality -> Rate computation taken from ImageMagick
      Rate := 100.0 / Sqr(115 - FQuality);
      NumPixels := Width * Height * Info.BytesPerPixel;
      TargetSize := (NumPixels * Rate) + 550 + (Info.ChannelCount - 1) * 142;
      parameters.tcp_rates[0] := 1.0 / (TargetSize / NumPixels);
    end;
    // Setup encoder
    opj_setup_encoder(cinfo, @parameters, image);

    // Fill component samples in data with values taken from
    // image pixels.
    // Components should be ordered like this: RGBA, YA, RGB, etc.
    for Channel := 0 to Info.ChannelCount - 1 do
    begin
      Z := Channel;
      InvZ := Info.ChannelCount - 1 - Z;
      if Info.HasAlphaChannel then
      begin
        if Channel = Info.ChannelCount - 1 then
          InvZ := Z
        else
          InvZ := Info.ChannelCount - 2 - Z;
      end;
      Pix := @PByteArray(Bits)[InvZ * ChannelSize];
      for I := 0 to Width * Height - 1 do
      begin
        case ChannelSize of
          1: image.comps[Z].data[I] := Pix^;
          2: image.comps[Z].data[I] := PWord(Pix)^;
          4: UInt32(image.comps[Z].data[I]) := PUInt32(Pix)^;
        end;
        Inc(Pix, Info.BytesPerPixel);
      end;
    end;

    // Open OpenJPEG output
    cio := opj_cio_open(opj_common_ptr(cinfo), nil, 0);
    // Try to encode the image
    if not opj_encode(cinfo, cio, image, nil) then
      Exit;
    // Finally write buffer with encoded image to output
    Write(Handle, cio.buffer, cio_tell(cio));

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
    opj_destroy_compress(cinfo);
    opj_image_destroy(image);
    opj_cio_close(cio);
    FreeMem(compparams);
  end;
end;

procedure TJpeg2000FileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    ConvFormat := IffFormat(Info.ChannelCount = 1, ifGray16, ifA16R16G16B16)
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray16)
  else if Info.IsIndexed then
    ConvFormat := ifA8R8G8B8
  else if Info.BytesPerPixel div Info.ChannelCount > 1 then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16R16G16B16, ifR16G16B16)
  else
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

  ConvertImage(Image, ConvFormat);
end;

function TJpeg2000FileFormat.TestFormat(Handle: TImagingHandle): Boolean;
begin
  Result := False;
  if Handle <> nil then
    Result := GetFileType(Handle) <> jtInvalid;
end;

initialization
  RegisterImageFileFormat(TJpeg2000FileFormat);

{$ELSE}
implementation
begin
{$IFEND}

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.27 Changes ---------------------------------------------
    - by Hanno Hugenberg <hanno.hugenberg@pergamonmed.com>
    - introduced the ImagingJpeg2000ScaleOutput parameter for keeping
      the original decoded images by avoiding upscaling of output images

  -- 0.26.3 Changes/Bug Fixes -----------------------------------
    - Rewritten JP2 loading part (based on PasJpeg2000) to be
      more readable (it's a bit faster too) and handled more JP2 files better:
      components with precisions like 12bit (not direct Imaging equivalent)
      are properly scaled, images/components with offsets are loaded ok.

  -- 0.24.3 Changes/Bug Fixes -----------------------------------
    - Alpha channels are now saved properly in FPC (GCC optimization issue),
      FPC lossy compression enabled again!
    - Added handling of component types (CDEF Box), JP2 images with alpha
      are now properly recognized by other applications.
    - Fixed wrong color space when saving grayscale images

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Removed ifGray32 from supported formats, OpenJPEG crashes when saving them.
    - Added Seek after loading to set input pos to the end of image.
    - Saving added lossy/lossless, quality option added.
    - Initial loading-only version created.

}
end.
