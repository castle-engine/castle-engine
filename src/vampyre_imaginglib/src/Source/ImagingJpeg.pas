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

{ This unit contains image format loader/saver for Jpeg images.}
unit ImagingJpeg;

{$I ImagingOptions.inc}

{ You can choose which Pascal JpegLib implementation will be used.
  IMJPEGLIB is version bundled with Imaging which works with all supported
  compilers and platforms.
  PASJPEG is original JpegLib translation or version modified for FPC
  (and shipped with it). You can use PASJPEG if this version is already
  linked with another part of your program and you don't want to have
  two quite large almost the same libraries linked to your exe.
  This is the case with Lazarus applications for example.}

{$DEFINE IMJPEGLIB}
{ $DEFINE PASJPEG}

{ Automatically use FPC's PasJpeg when compiling with Lazarus. }
{$IF Defined(LCL)}
  {$UNDEF IMJPEGLIB}
  {$DEFINE PASJPEG}
{$IFEND}

{ We usually want to skip the rest of the corrupted file when loading JPEG files
  instead of getting exception. JpegLib's error handler can only be
  exited using setjmp/longjmp ("non-local goto") functions to get error
  recovery when loading corrupted JPEG files. This is implemented in assembler
  and currently available only for 32bit Delphi targets and FPC.}
{$DEFINE ErrorJmpRecovery}
{$IF Defined(DCC) and not Defined(CPUX86)}
  {$UNDEF ErrorJmpRecovery}
{$IFEND}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingColors,
{$IF Defined(IMJPEGLIB)}
  imjpeglib, imjmorecfg, imjcomapi, imjdapimin, imjdeferr, imjerror,
  imjdapistd, imjcapimin, imjcapistd, imjdmarker, imjcparam,
{$ELSEIF Defined(PASJPEG)}
  jpeglib, jmorecfg, jcomapi, jdapimin, jdeferr, jerror,
  jdapistd, jcapimin, jcapistd, jdmarker, jcparam,
{$IFEND}
  ImagingUtility;

{$IF Defined(FPC) and Defined(PASJPEG)}
  { When using FPC's pasjpeg in FPC the channel order is BGR instead of RGB}
  {$DEFINE RGBSWAPPED}
{$IFEND}

type
  { Class for loading/saving Jpeg images. Supports load/save of
    8 bit grayscale and 24 bit RGB images. Jpegs can be saved with optional
    progressive encoding.
    Based on IJG's JpegLib so doesn't support alpha channels and lossless
    coding.}
  TJpegFileFormat = class(TImageFileFormat)
  private
    FGrayScale: Boolean;
  protected
    FQuality: LongInt;
    FProgressive: LongBool;
    procedure SetJpegIO(const JpegIO: TIOFunctions); virtual;
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
    { If True Jpeg images are saved in progressive format. Accessible trough
      ImagingJpegProgressive option.}
    property Progressive: LongBool read FProgressive write FProgressive;
  end;

implementation

const
  SJpegFormatName = 'Joint Photographic Experts Group Image';
  SJpegMasks      = '*.jpg,*.jpeg,*.jfif,*.jpe,*.jif';
  JpegSupportedFormats: TImageFormats = [ifR8G8B8, ifGray8];
  JpegDefaultQuality = 90;
  JpegDefaultProgressive = False;

const
  { Jpeg file identifiers.}
  JpegMagic: TChar2 = #$FF#$D8;
  BufferSize = 16384;

resourcestring
  SJpegError = 'JPEG Error';

type
  TJpegContext = record
    case Byte of
      0: (common: jpeg_common_struct);
      1: (d: jpeg_decompress_struct);
      2: (c: jpeg_compress_struct);
  end;

  TSourceMgr = record
    Pub: jpeg_source_mgr;
    Input: TImagingHandle;
    Buffer: JOCTETPTR;
    StartOfFile: Boolean;
  end;
  PSourceMgr = ^TSourceMgr;

  TDestMgr = record
    Pub: jpeg_destination_mgr;
    Output: TImagingHandle;
    Buffer: JOCTETPTR;
  end;
  PDestMgr = ^TDestMgr;

var
  JIO: TIOFunctions;
  JpegErrorMgr: jpeg_error_mgr;

{ Internal unit jpeglib support functions }

{$IFDEF ErrorJmpRecovery}
  {$IFDEF DCC}
  type
    jmp_buf = record
      EBX,
      ESI,
      EDI,
      ESP,
      EBP,
      EIP: UInt32;
    end;
    pjmp_buf = ^jmp_buf;

  { JmpLib SetJmp/LongJmp Library
    (C)Copyright 2003, 2004 Will DeWitt Jr. <edge@boink.net> }
  function  SetJmp(out jmpb: jmp_buf): Integer;
  asm
  {     ->  EAX     jmpb   }
  {     <-  EAX     Result }
            MOV     EDX, [ESP]  // Fetch return address (EIP)
            // Save task state
            MOV     [EAX+jmp_buf.&EBX], EBX
            MOV     [EAX+jmp_buf.&ESI], ESI
            MOV     [EAX+jmp_buf.&EDI], EDI
            MOV     [EAX+jmp_buf.&ESP], ESP
            MOV     [EAX+jmp_buf.&EBP], EBP
            MOV     [EAX+jmp_buf.&EIP], EDX

            SUB     EAX, EAX
  @@1:
  end;

  procedure LongJmp(const jmpb: jmp_buf; retval: Integer);
  asm
  {     ->  EAX     jmpb   }
  {         EDX     retval }
  {     <-  EAX     Result }
            XCHG    EDX, EAX

            MOV     ECX, [EDX+jmp_buf.&EIP]
            // Restore task state
            MOV     EBX, [EDX+jmp_buf.&EBX]
            MOV     ESI, [EDX+jmp_buf.&ESI]
            MOV     EDI, [EDX+jmp_buf.&EDI]
            MOV     ESP, [EDX+jmp_buf.&ESP]
            MOV     EBP, [EDX+jmp_buf.&EBP]
            MOV     [ESP], ECX  // Restore return address (EIP)

            TEST    EAX, EAX    // Ensure retval is <> 0
            JNZ     @@1
            MOV     EAX, 1
  @@1:
  end;
  {$ENDIF}

type
  TJmpBuf = jmp_buf;
  TErrorClientData = record
    JmpBuf: TJmpBuf;
    ScanlineReadReached: Boolean;
  end;
  PErrorClientData = ^TErrorClientData;
{$ENDIF}

procedure JpegError(CInfo: j_common_ptr);

  procedure RaiseError;
  var
    Buffer: AnsiString;
  begin
    // Create the message and raise exception
    CInfo.err.format_message(CInfo, Buffer);
    // Warning: you can get "Invalid argument index in format" exception when
    // using FPC (see http://bugs.freepascal.org/view.php?id=21229).
    // Fixed in FPC 2.7.1
  {$IF Defined(FPC) and (FPC_FULLVERSION <= 20701)}
    raise EImagingError.CreateFmt(SJPEGError + ' %d', [CInfo.err.msg_code]);
  {$ELSE}
    raise EImagingError.CreateFmt(SJPEGError + ' %d: ' + string(Buffer), [CInfo.err.msg_code]);
  {$IFEND}
  end;

begin
{$IFDEF ErrorJmpRecovery}
  // Only recovers on loads and when header is successfully loaded
  // (error occurs when reading scanlines)
  if (CInfo.client_data <> nil) and
    PErrorClientData(CInfo.client_data).ScanlineReadReached then
  begin
    // Non-local jump to error handler in TJpegFileFormat.LoadData
    longjmp(PErrorClientData(CInfo.client_data).JmpBuf, 1)
  end
  else
    RaiseError;
{$ELSE}
  RaiseError;
{$ENDIF}
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
end;

procedure ReleaseContext(var jc: TJpegContext);
begin
  if jc.common.err = nil then
    Exit;
  jpeg_destroy(@jc.common);
  jpeg_destroy_decompress(@jc.d);
  jpeg_destroy_compress(@jc.c);
  jc.common.err := nil;
end;

procedure InitSource(cinfo: j_decompress_ptr);
begin
  PSourceMgr(cinfo.src).StartOfFile := True;
end;

function FillInputBuffer(cinfo: j_decompress_ptr): Boolean;
var
  NBytes: LongInt;
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  NBytes := JIO.Read(Src.Input, Src.Buffer, BufferSize);

  if NBytes <= 0 then
  begin
    PByteArray(Src.Buffer)[0] := $FF;
    PByteArray(Src.Buffer)[1] := JPEG_EOI;
    NBytes := 2;
  end;
  Src.Pub.next_input_byte := Src.Buffer;
  Src.Pub.bytes_in_buffer := NBytes;
  Src.StartOfFile := False;
  Result := True;
end;

procedure SkipInputData(cinfo: j_decompress_ptr; num_bytes: LongInt);
var
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  if num_bytes > 0 then
  begin
    while num_bytes > Src.Pub.bytes_in_buffer do
    begin
      Dec(num_bytes, Src.Pub.bytes_in_buffer);
      FillInputBuffer(cinfo);
    end;
    Src.Pub.next_input_byte := @PByteArray(Src.Pub.next_input_byte)[num_bytes];
    //Inc(LongInt(Src.Pub.next_input_byte), num_bytes);
    Dec(Src.Pub.bytes_in_buffer, num_bytes);
  end;
end;

procedure TermSource(cinfo: j_decompress_ptr);
var
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  // Move stream position back just after EOI marker so that more that one
  // JPEG images can be loaded from one stream
  JIO.Seek(Src.Input, -Src.Pub.bytes_in_buffer, smFromCurrent);
end;

procedure JpegStdioSrc(var cinfo: jpeg_decompress_struct; Handle:
  TImagingHandle);
var
  Src: PSourceMgr;
begin
  if cinfo.src = nil then
  begin
    cinfo.src := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_PERMANENT,
      SizeOf(TSourceMgr));
    Src := PSourceMgr(cinfo.src);
    Src.Buffer := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_PERMANENT,
      BufferSize * SizeOf(JOCTET));
  end;
  Src := PSourceMgr(cinfo.src);
  Src.Pub.init_source := InitSource;
  Src.Pub.fill_input_buffer := FillInputBuffer;
  Src.Pub.skip_input_data := SkipInputData;
  Src.Pub.resync_to_restart := jpeg_resync_to_restart;
  Src.Pub.term_source := TermSource;
  Src.Input := Handle;
  Src.Pub.bytes_in_buffer := 0;
  Src.Pub.next_input_byte := nil;
end;

procedure InitDest(cinfo: j_compress_ptr);
var
  Dest: PDestMgr;
begin
  Dest := PDestMgr(cinfo.dest);
  Dest.Pub.next_output_byte := Dest.Buffer;
  Dest.Pub.free_in_buffer := BufferSize;
end;

function EmptyOutput(cinfo: j_compress_ptr): Boolean;
var
  Dest: PDestMgr;
begin
  Dest := PDestMgr(cinfo.dest);
  JIO.Write(Dest.Output, Dest.Buffer, BufferSize);
  Dest.Pub.next_output_byte := Dest.Buffer;
  Dest.Pub.free_in_buffer := BufferSize;
  Result := True;
end;

procedure TermDest(cinfo: j_compress_ptr);
var
  Dest: PDestMgr;
  DataCount: LongInt;
begin
  Dest := PDestMgr(cinfo.dest);
  DataCount := BufferSize - Dest.Pub.free_in_buffer;
  if DataCount > 0 then
    JIO.Write(Dest.Output, Dest.Buffer, DataCount);
end;

procedure JpegStdioDest(var cinfo: jpeg_compress_struct; Handle:
  TImagingHandle);
var
  Dest: PDestMgr;
begin
  if cinfo.dest = nil then
    cinfo.dest := cinfo.mem.alloc_small(j_common_ptr(@cinfo),
      JPOOL_PERMANENT, SizeOf(TDestMgr));
  Dest := PDestMgr(cinfo.dest);
  Dest.Buffer := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_IMAGE,
    BufferSize * SIZEOF(JOCTET));
  Dest.Pub.init_destination := InitDest;
  Dest.Pub.empty_output_buffer := EmptyOutput;
  Dest.Pub.term_destination := TermDest;
  Dest.Output := Handle;
end;

procedure SetupErrorMgr(var jc: TJpegContext);
begin
  // Set standard error handlers and then override some
  jc.common.err := jpeg_std_error(JpegErrorMgr);
  jc.common.err.error_exit := JpegError;
  jc.common.err.output_message := OutputMessage;
end;

procedure InitDecompressor(Handle: TImagingHandle; var jc: TJpegContext);
begin
  jpeg_CreateDecompress(@jc.d, JPEG_LIB_VERSION, sizeof(jc.d));
  JpegStdioSrc(jc.d, Handle);
  jpeg_read_header(@jc.d, True);
  jc.d.scale_num := 1;
  jc.d.scale_denom := 1;
  jc.d.do_block_smoothing := True;
  if jc.d.out_color_space = JCS_GRAYSCALE then
  begin
    jc.d.quantize_colors := True;
    jc.d.desired_number_of_colors := 256;
  end;
end;

procedure InitCompressor(Handle: TImagingHandle; var jc: TJpegContext;
  Saver: TJpegFileFormat);
begin
  jpeg_CreateCompress(@jc.c, JPEG_LIB_VERSION, sizeof(jc.c));
  JpegStdioDest(jc.c, Handle);
  if Saver.FGrayScale then
    jc.c.in_color_space := JCS_GRAYSCALE
  else
    jc.c.in_color_space := JCS_RGB;
  jpeg_set_defaults(@jc.c);
  jpeg_set_quality(@jc.c, Saver.FQuality, True);
  if Saver.FProgressive then
    jpeg_simple_progression(@jc.c);
end;

{ TJpegFileFormat class implementation }

procedure TJpegFileFormat.Define;
begin
  FName := SJpegFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := JpegSupportedFormats;

  FQuality := JpegDefaultQuality;
  FProgressive := JpegDefaultProgressive;

  AddMasks(SJpegMasks);
  RegisterOption(ImagingJpegQuality, @FQuality);
  RegisterOption(ImagingJpegProgressive, @FProgressive);
end;

procedure TJpegFileFormat.CheckOptionsValidity;
begin
  // Check if option values are valid
  if not (FQuality in [1..100]) then
    FQuality := JpegDefaultQuality;
end;

function TJpegFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  PtrInc, LinesPerCall, LinesRead, I: Integer;
  Dest: PByte;
  jc: TJpegContext;
  Info: TImageFormatInfo;
  Col32: PColor32Rec;
  NeedsRedBlueSwap: Boolean;
  Pix: PColor24Rec;
{$IFDEF ErrorJmpRecovery}
  ErrorClient: TErrorClientData;
{$ENDIF}

  procedure LoadMetaData;
  var
    ResUnit: TResolutionUnit;
  begin
    // Density unit: 0 - undef, 1 - inch, 2 - cm
    if jc.d.saw_JFIF_marker and (jc.d.density_unit > 0) and
      (jc.d.X_density > 0) and (jc.d.Y_density > 0) then
    begin
      ResUnit := ruDpi;
      if jc.d.density_unit = 2 then
        ResUnit := ruDpcm;
      FMetadata.SetPhysicalPixelSize(ResUnit, jc.d.X_density, jc.d.Y_density);
    end;
  end;

begin
  // Copy IO functions to global var used in JpegLib callbacks
  Result := False;
  SetJpegIO(GetIO);
  SetLength(Images, 1);

  with JIO, Images[0] do
  try
    ZeroMemory(@jc, SizeOf(jc));
    SetupErrorMgr(jc);
  {$IFDEF ErrorJmpRecovery}
    ZeroMemory(@ErrorClient, SizeOf(ErrorClient));
    jc.common.client_data := @ErrorClient;
    if setjmp(ErrorClient.JmpBuf) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  {$ENDIF}
    InitDecompressor(Handle, jc);

    case jc.d.out_color_space of
      JCS_GRAYSCALE: Format := ifGray8;
      JCS_RGB:       Format := ifR8G8B8;
      JCS_CMYK:      Format := ifA8R8G8B8;
    else
      Exit;
    end;

    NewImage(jc.d.image_width, jc.d.image_height, Format, Images[0]);
    jpeg_start_decompress(@jc.d);
    GetImageFormatInfo(Format, Info);
    PtrInc := Width * Info.BytesPerPixel;
    LinesPerCall := 1;
    Dest := Bits;

    // If Jpeg's colorspace is RGB and not YCbCr we need to swap
    // R and B to get Imaging's native order
    NeedsRedBlueSwap := jc.d.jpeg_color_space = JCS_RGB;
  {$IFDEF RGBSWAPPED}
    // Force R-B swap for FPC's PasJpeg
    NeedsRedBlueSwap := True;
  {$ENDIF}

  {$IFDEF ErrorJmpRecovery}
    ErrorClient.ScanlineReadReached := True;
  {$ENDIF}

    while jc.d.output_scanline < jc.d.output_height do
    begin
      LinesRead := jpeg_read_scanlines(@jc.d, @Dest, LinesPerCall);
      if NeedsRedBlueSwap and (Format = ifR8G8B8) then
      begin
        Pix := PColor24Rec(Dest);
        for I := 0 to Width - 1 do
        begin
          SwapValues(Pix.R, Pix.B);
          Inc(Pix);
        end;
      end;
      Inc(Dest, PtrInc * LinesRead);
    end;

    if jc.d.out_color_space = JCS_CMYK then
    begin
      Col32 := Bits;
      // Translate from CMYK to RGB
      for I := 0 to Width * Height - 1 do
      begin
        CMYKToRGB(255 - Col32.B, 255 - Col32.G, 255 - Col32.R, 255 - Col32.A,
          Col32.R, Col32.G, Col32.B);
        Col32.A := 255;
        Inc(Col32);
      end;
    end;

    // Store supported metadata
    LoadMetaData;

    jpeg_finish_output(@jc.d);
    jpeg_finish_decompress(@jc.d);
    Result := True;
  finally
    ReleaseContext(jc);
  end;
end;

function TJpegFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  PtrInc, LinesWritten: LongInt;
  Src, Line: PByte;
  jc: TJpegContext;
  ImageToSave: TImageData;
  Info: TImageFormatInfo;
  MustBeFreed: Boolean;
{$IFDEF RGBSWAPPED}
  I: LongInt;
  Pix: PColor24Rec;
{$ENDIF}

  procedure SaveMetaData;
  var
    XRes, YRes: Double;
  begin
    if FMetadata.GetPhysicalPixelSize(ruDpcm, XRes, YRes, True) then
    begin
      jc.c.density_unit := 2; // Dots per cm
      jc.c.X_density := Round(XRes);
      jc.c.Y_density := Round(YRes)
    end;
  end;

begin
  Result := False;
  // Copy IO functions to global var used in JpegLib callbacks
  SetJpegIO(GetIO);

  // Makes image to save compatible with Jpeg saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with JIO, ImageToSave do
  try
    ZeroMemory(@jc, SizeOf(jc));
    SetupErrorMgr(jc);

    GetImageFormatInfo(Format, Info);
    FGrayScale := Format = ifGray8;
    InitCompressor(Handle, jc, Self);
    jc.c.image_width := Width;
    jc.c.image_height := Height;
    if FGrayScale then
    begin
      jc.c.input_components := 1;
      jc.c.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      jc.c.input_components := 3;
      jc.c.in_color_space := JCS_RGB;
    end;

    PtrInc := Width * Info.BytesPerPixel;
    Src := Bits;
    
  {$IFDEF RGBSWAPPED}
    GetMem(Line, PtrInc);
  {$ENDIF}

    // Save supported metadata
    SaveMetaData;

    jpeg_start_compress(@jc.c, True);
    while (jc.c.next_scanline < jc.c.image_height) do
    begin
    {$IFDEF RGBSWAPPED}
      if Format = ifR8G8B8 then
      begin
        Move(Src^, Line^, PtrInc);
        Pix := PColor24Rec(Line);
        for I := 0 to Width - 1 do
        begin
          SwapValues(Pix.R, Pix.B);
          Inc(Pix, 1);
        end;
      end;
    {$ELSE}
      Line := Src;
    {$ENDIF}

      LinesWritten := jpeg_write_scanlines(@jc.c, @Line, 1);
      Inc(Src, PtrInc * LinesWritten);
    end;

    jpeg_finish_compress(@jc.c);
    Result := True;
  finally
    ReleaseContext(jc);
    if MustBeFreed then
      FreeImage(ImageToSave);
  {$IFDEF RGBSWAPPED}
    FreeMem(Line);
  {$ENDIF}
  end;
end;

procedure TJpegFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  if Info.HasGrayChannel then
    ConvertImage(Image, ifGray8)
  else
    ConvertImage(Image, ifR8G8B8);
end;

function TJpegFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
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

procedure TJpegFileFormat.SetJpegIO(const JpegIO: TIOFunctions);
begin
  JIO := JpegIO;
end;

initialization
  RegisterImageFileFormat(TJpegFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.1 ---------------------------------------------------
    - Able to read corrupted JPEG files - loads partial image
      and skips the corrupted parts (FPC and x86 Delphi).
    - Fixed reading of physical resolution metadata, could cause
      "divided by zero" later on for some files.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Fixed loading of some JPEGs with certain APPN markers (bug in JpegLib).
    - Fixed swapped Red-Blue order when loading Jpegs with
      jc.d.jpeg_color_space = JCS_RGB.
    - Added loading and saving of physical pixel size metadata.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Changed the Jpeg error manager, messages were not properly formatted.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Fixed wrong color space setting in InitCompressor.
    - Fixed problem with progressive Jpegs in FPC (modified JpegLib,
      can't use FPC's PasJpeg in Windows).

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - FPC's PasJpeg wasn't really used in last version, fixed.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Fixed loading of CMYK jpeg images. Could cause heap corruption
      and loaded image looked wrong.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Removed JFIF/EXIF detection from TestFormat. Found JPEGs
      with different headers (Lavc) which weren't recognized. 

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Made public properties for options registered to SetOption/GetOption
      functions.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.
    - Changes in TestFormat, now reads JFIF and EXIF signatures too.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - input position is now set correctly to the end of the image
      after loading is done. Loading of sequence of JPEG files stored in
      single stream works now
    - when loading and saving images in FPC with PASJPEG read and
      blue channels are swapped to have the same chanel order as IMJPEGLIB
    - you can now choose between IMJPEGLIB and PASJPEG implementations

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added SetJpegIO method which is used by JNG image format
}
end.

