{%MainUnit castleimages.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle PNG format using libpng. }

{$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}

type
  ELibPngNotAvailable = class(Exception);
  EPngTransformError = class(Exception);

{ Return the version of used dynamic libpng library
  (taken by querying png_access_version_number).

  Raises ELibPngNotAvailable exception if libpng library
  was not available or had incorrect version (not CastlePngInitialized). }
function CurrentLibpngVersion: PAnsiChar; forward;

{ Use all functions below ONLY when CastlePngInitialized = true. }

{ }
procedure png_transform_to_g1byte(png_ptr: png_structp; info_ptr: png_infop); forward;
procedure png_transform_to_ga2byte(png_ptr: png_structp; info_ptr: png_infop); forward;

{ Apply transformations in such a way that EVERY png format wil be converted to
  RGB 8bit depth, no fill, no alpha.
  So, paletted and grayscales must be converted, bytes got to have appriopriate
  order, alpha channel must be applied and then thrown out.}
procedure png_transform_to_rgb3byte(png_ptr: png_structp; info_ptr: png_infop); forward;

{ Apply transformation to convert EVERY png to RGBA 8 byte depth.
  So, paletted and grayscales must be converted, bytes got to have appriopriate
  order and alpha channel must be added (=1.0) if it is not already present
  in file. }
procedure png_transform_to_rgba4byte(png_ptr: png_structp; info_ptr: png_infop); forward;

{ png file has alpha info if it is has alpha channel (grayscale or rgb)
  or if it has tRNS chunk (for paletted image this stores alpha values
  for each palette color, for grayscale/rgb it determines one particular
  color to mean "transparent"). Function below checks it. }
function png_is_alpha(png_ptr: png_structp; info_ptr: png_infop): boolean; forward;

function png_is_grayscale(png_ptr: png_structp; info_ptr: png_infop): boolean; forward;

function PngColorTypeToStr(const PngColorType: Int32): string; forward;
function PngInterlaceTypeToStr(const PngInterlaceType: Int32): string; forward;
function PngTextCompressionToStr(const PngTextCompression: Int32): string; forward;

var
  { Call InitializePNG to set these variables }
  FCurrentLibpngVersion: AnsiString;
  FCurrentLibpngVersionMajor,
  FCurrentLibpngVersionMinor,
  FCurrentLibpngVersionRelease: Integer;

function CurrentLibpngVersion: PAnsiChar;
begin
  if not CastlePngInitialized then
    raise ELibPngNotAvailable.Create('LibPng (or it''s dependency, ZLib) is not available, or has incorrect version. Make sure that the dynamic libraries for LibPng and ZLib are installed (or placed alongside the exe file on Windows).');

  Result := PAnsiChar(FCurrentLibpngVersion);
end;

{ ------------------------------------------------------------------------------------ }

{ Kilka notek do pisania transformacji libpng :

  pamietaj ze transformacje uaktualniaja stan strukturki png_ptr
  (typu png_structp), natomiast nie uaktualniaja info_ptr
  (typu png_infop). Jak po zaaplikowaniu transformacji w
  uniwersalny sposob dowiedziec sie jaki mamy teraz ColorType
  i BitDepth ? Wydaje sie ze sensowny sposob to png_read_update_info
  a potem png_get_IHDR (samo png_get_IHDR nie wystarczy, musi byc
  png_read_update_info aby uaktualnic strukturke info_ptr;
  w ogole nie wiem po co png_get_IHDR pobiera jako parametr takze
  png_ptr - na png_get_IHDR trzeba patrzec jako na rozkodowanie
  wnetrza strukturki info_ptr) ale nie - read_update_info powoduje
  niezrozumialy warning a potem sprawia ze poprawny odczyt jest
  niemozliwy.

  po zaaplikowaniu kazdej transformacji musimy odswiezyc wartosc ColorType
  i bitDepth. Niestety nie mozemy do tego uzywac png_read_update_info +
  png_get_IHDR. Naprawde nie wiem czemu, ale

  uzywanie png_read_update_info wiecej niz raz powoduje warningi
  libpng "Ignoring extra png_read_update_info() call; row buffer not reallocated"
  ktorych sens pozostaje dla mnie nieodgadniony (dlaczego je ignoruje ?
  przeciez uaktualnia info_ptr a chyba taki jest sens tej funkcji ?).
  Potem przy odczytywaniu obrazka jest blad "Decompression error"
  wiec wniosek - nie uzywac png_read_update_info wiecej niz raz.
  Czyli wolno tego uzywac tylko w taki sposob w jaki napisali w manualu -
  jednorazowo, juz po wykonaniu wszystkich transformacji. }

{ Make sure you have grayscale image, with 8 bits per color.

  Assumes we already have stripped palette, so it's "true" color per pixel.

  Alpha channel is never added/stripped by this, although it will
  be converted to 8 bits too if exists. }
procedure png_transform_rgb_to_grayscale8bit(png_ptr: png_structp;
  var ColorType, BitDepth: UInt32);
begin
  { rgb -> grayscale }
  if (ColorType and PNG_COLOR_MASK_COLOR) <> 0 then
  begin
    png_set_rgb_to_gray_fixed(png_ptr,
      { Error_action = 1 means "silently do the conversion" }
      1,
      { Negative weights means "use the default weight" (matches my
        GrayscaleValuesByte weights, actually my GrayscaleValuesByte
        was copied from libpng documentation...) }
      -1, -1);
    ColorType := ColorType and (not PNG_COLOR_MASK_COLOR);
  end;

  { grayscale-non-8-bit -> 8bit grayscale }
  if BitDepth < 8 then
  begin
    {$ifdef CASTLE_PNG_STATIC}
    png_set_expand_gray_1_2_4_to_8(png_ptr);
    {$else}
    if Assigned(png_set_expand_gray_1_2_4_to_8) then
      png_set_expand_gray_1_2_4_to_8(png_ptr) else
      raise EPngTransformError.Create('Your png library doesn''t have png_set_expand_gray_1_2_4_to_8, needed to handle this image');
    {$endif}

    BitDepth := 8;
  end;

  {now he have grayscale 8/16 bitDepth + maybe alpha, maybe unapplied tRNS}
  {rgb 16 bitdepth -> rgb 8 bitdepth}
  if BitDepth = 16 then
  begin
    png_set_strip_16(png_ptr);
    BitDepth := 8;
  end;
end;

{ Make sure you have RGB (not grayscale) image, with 8 bits per color.

  Assumes we already have stripped palette, so it's "true" color per pixel.

  Alpha channel is never added/stripped by this, although it will
  be converted to 8 bits too if exists. }
procedure png_transform_rgb_to_rgb8bit(png_ptr: png_structp;
  var ColorType, BitDepth: UInt32);
begin
  { grayscale -> 8bit rgb }
  if (ColorType and PNG_COLOR_MASK_COLOR) = 0 then
  begin
    if BitDepth < 8 then
    begin
      {$ifdef CASTLE_PNG_STATIC}
      png_set_expand_gray_1_2_4_to_8(png_ptr);
      {$else}
      if Assigned(png_set_expand_gray_1_2_4_to_8) then
        png_set_expand_gray_1_2_4_to_8(png_ptr) else
        raise EPngTransformError.Create('Your png library doesn''t have png_set_expand_gray_1_2_4_to_8, needed to handle this image');
      {$endif}
      BitDepth := 8;
    end;

    png_set_gray_to_rgb(png_ptr);
    {gray color means ColorType = PNG_COLOR_TYPE_GRAY or GRAY_ALPHA
                                = 0 or MASK_ALPHA
     and that's why here we can simply combine it bitwise with MASK_COOR }
    ColorType := ColorType or PNG_COLOR_MASK_COLOR;
  end;

  {now he have rgb 8/16 bitDepth + maybe alpha, maybe unapplied tRNS}
  {rgb 16 bitdepth -> rgb 8 bitdepth}
  if BitDepth = 16 then
  begin
    png_set_strip_16(png_ptr);
    BitDepth := 8;
  end;
end;

procedure png_transform_remove_alpha(png_ptr: png_structp;
  info_ptr: png_infop;
  var ColorType, BitDepth: UInt32);
var
  bKGD_col_ptr: png_color_16p;
  {my_background_col: png_color_16;}
begin
  { handle alpha and tRNS by combining image with color in bKGD header part
    (we do it just to get rid of alpha channel; in case of PngGraphic bedziemy
    aplikowac pozniej filler jako czwarty bajt i bedziemy ten czwarty bajt
    ignorowac ale generalnie tak czy siak nalezy skombinowac alpha channel
    z obrazkiem zeby obrazek wygladal tak jakby sie tego mogl spodziewac
    autor obrazka.) }
  if (ColorType and PNG_COLOR_MASK_ALPHA) <> 0 then
  begin
    { combinig with background requires a few parameters.
      We set need_expand (4th parameter) to 1 when we take color from file because
      in file it's written in file's original format and so it must be expanded
      to the currently set format (8 bit rgb). However, we are supplying
      my_background_col in 8bit rgb format already so there we set need_expand to 0.

      We set background_gamma (5th parameter) always as 1.0 - bacause that's
      considered the "default" and we want do just the default thing
      (we want to do what author of the image expected). }
    if png_get_bKGD(png_ptr, info_ptr, @bKGD_col_ptr) <> 0 then
    begin  { combine it with supplied bKGD color }
      png_set_background(png_ptr, bKGD_col_ptr, PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
    end else
    begin
      { What should we do now ?
        We can apply image on an arbitrary background color
        (but which one ?) or we can just strip alpha channel.

        Some interesting case from testing:

        - png/bufferfs.png: requires stripping, only then looks good.
          Possibly it's just a bad image ? Opening in GIMP, it shows
          only as a text (butterflies not visible), everything else
          is completely transparent. That's why
          - doing png_set_background with my_background_col = white
            makes this image just a text on clear white background and
          - doing png_set_background with my_background_col = black
            makes this image just a text on clear black background
          If you want to see butterflies there, you cannot use
          png_set_background, you must just strip alpha channel.

        - png/moose/customize-m.png (online, similar version on
          http://lofotenmoose.info/css/destroy/buttons/xl.png):

          This is an interesting hack... The intended result is the
          grayscale image of Michelle Pfeiffer (visible on webpage when composed
          against white bg: http://lofotenmoose.info/css/destroy/buttons/).
          The image file has an alpha channel, that contains the grayscale
          *negative* of the image, and pure black color in normal RGB channels.
          The effect: when applied against white background, the image looks
          good (black alpha channel means to take background, which is white,
          and white alpha channel means to take image, which is black...).
          In some other circumstances, it may look unsensible, e.g. composing
          the image against black background, or simply stripping the alpha
          channel, leaves you with pure black image...

          IOW, to convert this image to RGB, I have to use the alpha channel and
          combine it with white background to make the result look good.
          So when loading to TRGBImage, I should do png_set_background like this:

            with my_background_col do
            begin
              // my_background_col := white color
              red := $FF; green := $FF;  blue := $FF;
            end;
            png_set_background(png_ptr, @my_background_col, PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);

          But this is non-standard, by default I simply strip alpha
          when converting RGBA image to RGB memory, so I don't do it.
          Image doesn't contain bKGD chunk, so I cannot arbitrarily choose
          to compose it against white background. So I'll strip alpha,
          thereby destroying the image (making it black).

          This image can be viewed correctly when loaded to (default for this
          PNG color type) TRGBAlphaImage, and presented against white background.
          So my reading code is correct, the image can be rendered correctly.
      }

      { Version with stripping }
      png_set_strip_alpha(png_ptr);
    end;
    ColorType := ColorType and UInt32(not PNG_COLOR_MASK_ALPHA);
  end;
end;

{ Removes palette from the image, making the image RGB (possibly with alpha). }
procedure png_transform_palette_to_rgb(png_ptr: png_structp;
  info_ptr: png_infop;
  var ColorType, BitDepth: UInt32);
begin
  { palette -> rgb, maybe with alpha }
  if (ColorType and PNG_COLOR_MASK_PALETTE) <> 0 then
  begin
    png_set_palette_to_rgb(png_ptr);

    { we converted palette to rgb; actually it may be RGB or RGBA;
      paletted images can contain alpha channel only using tRNS chunk
      so here we can check whether we got RGB or RGBA by checking
      whether there exists tRNS chunk;
      Ufff; this was a bug corrected after a long day : 21.12.2002}
    if png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0 then
      ColorType := PNG_COLOR_TYPE_RGB_ALPHA else
      ColorType := PNG_COLOR_TYPE_RGB;

    { when expanding palette we always get 8 bit depth because pallete entries
      are always in 8bit RGB }
    BitDepth := 8;
  end;
end;

procedure png_transform_to_g1byte(png_ptr: png_structp; info_ptr: png_infop);
var
  ColorType, BitDepth: UInt32;
begin
  BitDepth := png_get_bit_depth(png_ptr, info_ptr);
  ColorType := png_get_color_type(png_ptr, info_ptr);

  png_transform_palette_to_rgb(png_ptr, info_ptr, ColorType, BitDepth);
  png_transform_rgb_to_grayscale8bit(png_ptr, ColorType, BitDepth);
  png_transform_remove_alpha(png_ptr, info_ptr, ColorType, BitDepth);

  { Test ColorType and BitDepth are as expected. }
  Assert((ColorType = PNG_COLOR_TYPE_GRAY) and (BitDepth = 8),
    'png_transform_to_g1byte failed to apply good png transformations');
end;

procedure png_transform_to_ga2byte(png_ptr: png_structp; info_ptr: png_infop);
var
  ColorType, BitDepth: UInt32;
  TRNSHandled: boolean;
begin
  BitDepth := png_get_bit_depth(png_ptr, info_ptr);
  ColorType := png_get_color_type(png_ptr, info_ptr);

  { At the beginning: tRNS chunk is already handled if it doesn't
    exist, right ? }
  TRNSHandled := png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) = 0;

  { palette -> 8bit rgb }
  if (ColorType and PNG_COLOR_MASK_PALETTE) <> 0 then
  begin
    png_set_palette_to_rgb(png_ptr);

    { paletted images can contain alpha channel only using tRNS chunk
      so here we can check whether we got RGB or RGBA by checking
      whether there exists tRNS chunk;
      Ufff; this was a bug corrected after a long day : 21.12.2002 }
    if not TRNSHandled then
    begin
      ColorType := PNG_COLOR_TYPE_RGB_ALPHA;
      TRNSHandled := true;
    end else
      ColorType := PNG_COLOR_TYPE_RGB;

    { When expanding palette we always get 8 bit depth because
      pallete entries are always in 8bit RGB }
    BitDepth := 8;
  end;

  png_transform_rgb_to_grayscale8bit(png_ptr, ColorType, BitDepth);

  if (not TRNSHandled) and
     (ColorType and PNG_COLOR_MASK_ALPHA = 0) then
  begin
    png_set_tRNS_to_alpha(png_ptr);
    TRNSHandled := true;
  end;

  { In case of some invalid image (Like an image with alpha channel
    and also tRNS chunk ? Does libPNG allow such things ?)
    probably (not confirmed) I may be left here with TRNSHandled = false.
    Ignore this. }

  if (ColorType and PNG_COLOR_MASK_ALPHA = 0) and
     (BitDepth = 8) then
  begin
    png_set_filler(png_ptr, High(byte), PNG_FILLER_AFTER);
    ColorType := ColorType or PNG_COLOR_MASK_ALPHA;
  end;

  { Test ColorType and BitDepth are as expected. }
  Assert((ColorType = PNG_COLOR_TYPE_GRAY_ALPHA) and (BitDepth = 8),
    'png_transform_to_ga2byte failed to apply good png transformations');
end;

procedure png_transform_to_rgb3byte(png_ptr: png_structp; info_ptr: png_infop);
var
  ColorType, BitDepth: UInt32;
begin
  BitDepth := png_get_bit_depth(png_ptr, info_ptr);
  ColorType := png_get_color_type(png_ptr, info_ptr);

  png_transform_palette_to_rgb(png_ptr, info_ptr, ColorType, BitDepth);
  png_transform_rgb_to_rgb8bit(png_ptr, ColorType, BitDepth);
  png_transform_remove_alpha(png_ptr, info_ptr, ColorType, BitDepth);

  Assert((ColorType = PNG_COLOR_TYPE_RGB) and (BitDepth = 8),
    'png_transform_to_rgb3byte failed to apply good png transformations');
end;

procedure png_transform_to_rgba4byte(png_ptr: png_structp; info_ptr: png_infop);
var
  ColorType, BitDepth: UInt32;
  TRNSHandled: boolean;
begin
  BitDepth := png_get_bit_depth(png_ptr, info_ptr);
  ColorType := png_get_color_type(png_ptr, info_ptr);

  { At the beginning: tRNS chunk is already handled if it doesn't
    exist, right ? }
  TRNSHandled := png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) = 0;

  { palette -> 8bit rgb }
  if (ColorType and PNG_COLOR_MASK_PALETTE) <> 0 then
  begin
    png_set_palette_to_rgb(png_ptr);

    { paletted images can contain alpha channel only using tRNS chunk
      so here we can check whether we got RGB or RGBA by checking
      whether there exists tRNS chunk;
      Ufff; this was a bug corrected after a long day : 21.12.2002 }
    if not TRNSHandled then
    begin
      ColorType := PNG_COLOR_TYPE_RGB_ALPHA;
      TRNSHandled := true;
    end else
      ColorType := PNG_COLOR_TYPE_RGB;

    { When expanding palette we always get 8 bit depth because
      pallete entries are always in 8bit RGB }
    BitDepth := 8;
  end;

  png_transform_rgb_to_rgb8bit(png_ptr, ColorType, BitDepth);

  if (not TRNSHandled) and
     (ColorType and PNG_COLOR_MASK_ALPHA = 0) then
  begin
    png_set_tRNS_to_alpha(png_ptr);
    TRNSHandled := true;
  end;

  { In case of some invalid image (Like an image with alpha channel
    and also tRNS chunk ? Does libPNG allow such things ?)
    probably (not confirmed) I may be left here with TRNSHandled = false.
    Ignore this. }

  if (ColorType and PNG_COLOR_MASK_ALPHA = 0) and
     (BitDepth = 8) then
  begin
    png_set_filler(png_ptr, High(byte), PNG_FILLER_AFTER);
    ColorType := ColorType or PNG_COLOR_MASK_ALPHA;
  end;

  Assert((ColorType = PNG_COLOR_TYPE_RGB_ALPHA) and (BitDepth = 8),
    'png_transform_to_rgba4byte failed to apply good png transformations');
end;

function png_is_grayscale(png_ptr: png_structp; info_ptr: png_infop): boolean;
var
  ColorType: UInt32;
begin
  ColorType := png_get_color_type(png_ptr, info_ptr);
  Result := (ColorType and PNG_COLOR_MASK_COLOR) = 0;
end;

function png_is_alpha(png_ptr: png_structp; info_ptr: png_infop): boolean;
var
  ColorType: UInt32;
begin
  ColorType := png_get_color_type(png_ptr, info_ptr);
  Result := ( (ColorType and PNG_COLOR_MASK_ALPHA) <> 0) or
              (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0);
end;

{ *ToStr -------------------------------------------------------------- }

function PngColorTypeToStr(const PngColorType: Int32): string;
begin
  case PngColorType of
    PNG_COLOR_TYPE_GRAY      : Result := 'Gray';
    PNG_COLOR_TYPE_GRAY_ALPHA: Result := 'Gray with alpha';
    PNG_COLOR_TYPE_PALETTE   : Result := 'Paletted';
    PNG_COLOR_TYPE_RGB       : Result := 'RGB';
    PNG_COLOR_TYPE_RGB_ALPHA : Result := 'RGB with alpha';
    else Result := 'unknown';
  end;
end;

function PngInterlaceTypeToStr(const PngInterlaceType: Int32): string;
begin
  case PngInterlaceType of
    PNG_INTERLACE_NONE : Result := 'None';
    PNG_INTERLACE_ADAM7: Result := 'Adam7';
    else Result := 'unknown';
  end;
end;

function PngTextCompressionToStr(const PngTextCompression: Int32): string;
begin
  case PngTextCompression of
    PNG_TEXT_COMPRESSION_NONE: Result := 'None';
    PNG_TEXT_COMPRESSION_zTXT: Result := 'zTXT';
    else Result := 'unknown';
  end;
end;

{ initialize ----------------------------------------------------------------- }

procedure InitializePNGUsingLibpng;
var
  VerNumber: png_uint_32;
begin
  if CastlePngInitialized then
  begin
    { Repeated calls to InitializePNGUsingLibpng should do nothing.
      This is useful because we call it now from LoadPNG, as we need to do it
      later than "initialization" on Android. }
    if FCurrentLibpngVersion = '' then
    begin
      VerNumber := png_access_version_number();
      FCurrentLibpngVersionRelease := VerNumber mod 100;
      FCurrentLibpngVersionMinor := (VerNumber mod 10000) div 100;
      FCurrentLibpngVersionMajor := VerNumber div 10000;
      FCurrentLibpngVersion := AnsiString(Format('%d.%d.%d', [
        FCurrentLibpngVersionMajor,
        FCurrentLibpngVersionMinor,
        FCurrentLibpngVersionRelease
      ]));
      WritelnLog('LibPng detected (version %s).', [FCurrentLibpngVersion]);
    end;
  end;
  { Note: Do not report warning, it will be done by WarningNoLibPng if we'll load any PNG. }
end;

{ error / warning handlers --------------------------------------------------- }

{ function our_png_error_fn and our_png_warning_fn are used as libpng
  error and warning handlers in both SavePNG_LibPng and LoadPNG_LibPng.

  They could use png_get_error_ptr(png_ptr) for some pointer data,
  but for now it is not used. }

procedure our_png_error_fn(png_ptr : png_structp; s : png_const_charp);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
begin
  raise EInvalidPng.Create('PNG error ' + S);
end;

procedure our_png_warning_fn(png_ptr : png_structp; s : png_const_charp);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
begin
  { Ignore this warning.

    It can be safely ignored as I can see:
    https://stackoverflow.com/questions/22745076/libpng-warning-iccp-known-incorrect-srgb-profile
    https://tex.stackexchange.com/questions/125612/warning-pdflatex-libpng-warning-iccp-known-incorrect-srgb-profile

    Not ignoring this would be bothersome, as it seems Photoshop produces
    such files by default. We can get flooded with these warnings easily. }
  if S = 'iCCP: known incorrect sRGB profile' then Exit;

  WritelnWarning('PNG', S);
end;

{ read / write functions -----------------------------------------------------

  Both treat png_get_io_ptr(png_ptr) as TStream
  and read/write from/to that stream. Exceptions are raised if operation
  is not posiible (for example, stream end --- this will happen if PNG file
  is truncated; since we never give back to PNG the number of bytes read,
  it is obvious that we have to detect unexpected stream end ourselves). }

{ We dereference here memory allocated with PNG, so pointer checks
  will make false errors. }
{$ifdef FPC}
{$checkpointer off}
{$endif}

procedure our_png_read_fn(png_ptr: png_structp; data: png_bytep; len: png_size_t);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
begin
  TStream(png_get_io_ptr(png_ptr)).ReadBuffer(data^, len);
end;

procedure our_png_write_fn(png_ptr: png_structp; data: png_bytep; len: png_size_t);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
begin
  TStream(png_get_io_ptr(png_ptr)).WriteBuffer(data^, len);
end;

procedure our_png_flush_fn(png_ptr: png_structp);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
begin
  { we would like to do here something like TStream(png_get_io_ptr(png_ptr)).Flush;
    but there is no "flush" method in TStream. }
end;

{$ifdef KAMBI_CHECK_POINTER}
{ Turn checkpointer back on }
{$checkpointer on}
{$endif}

{ LoadPNG_LibPng ------------------------------------------------------------------- }

function LoadPNG_LibPng(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;

  function ClassAllowed(ImageClass: TEncodedImageClass): boolean;
  begin
    Result := CastleImages.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

var
  PNGResult: TCastleImage absolute Result;
  AllocateWidth, AllocateHeight: Cardinal;
  png_ptr: png_structp;
  info_ptr: png_infop;

  { Allocates Result to given class and applies libpng transforms to
    make resulting data matching given Result class.

    When CheckTargetClass, it checks (and returns @true only when it is OK)
    whether ImageClass is in AllowedImageClasses. }
  function TransformPng(const ImageClass: TCastleImageClass; const CheckTargetClass: Boolean = true): boolean;
  begin
    Result := (not CheckTargetClass) or ClassAllowed(ImageClass);

    if Result then
    begin
      PNGResult := ImageClass.Create(AllocateWidth, AllocateHeight);
      if ImageClass = TGrayscaleImage then
        png_transform_to_g1byte(png_ptr, info_ptr) else
      if ImageClass = TGrayscaleAlphaImage then
        png_transform_to_ga2byte(png_ptr, info_ptr) else
      if ImageClass = TRGBImage then
        png_transform_to_rgb3byte(png_ptr, info_ptr) else
      if ImageClass = TRGBAlphaImage then
        png_transform_to_rgba4byte(png_ptr, info_ptr);
    end;
  end;

var
  row_pointers: {$ifdef FPC}TFPList{$else}TList{$endif};
  i: Cardinal;
  IsAlpha, IsGrayscale: boolean;
begin
 png_ptr := nil;
 try
  { Passing here CurrentLibpngVersion (which is obtained by querying libpng
    library) is a little trick.

    Libpng docs instead advise to pass here PNG_LIBPNG_VER_STRING constant,
    which should be a constant reflecing the libpng API version we have defined
    in CastleInternalPng.
    Then libpng would check whether version is compatible, and eventually
    report error "png.h and png.c versions not compatible".

    However, this check is too strict in our experience.
    As we use only a tiny subset of libpng API (and dynamically check
    for some functions existence, e.g. "Assigned(png_set_expand_gray_1_2_4_to_8)"),
    we work with any libpng 1.x in practice now.
    Our own version check (done in each CurrentLibpngVersion call)
    is sufficient for us. }
  png_ptr := png_create_read_struct(CurrentLibpngVersion,
    nil { we could pass here data to warning/error handlers },
    {$ifdef FPC} @ {$endif} our_png_error_fn,
    {$ifdef FPC} @ {$endif} our_png_warning_fn);
  Check( png_ptr <> nil, 'png_create_read_struct failed');

  info_ptr := png_create_info_struct(png_ptr);
  Check( info_ptr <> nil, 'png_create_info_struct failed');

  png_set_read_fn(png_ptr, Stream,
    {$ifdef FPC} @ {$endif} our_png_read_fn);

  { Note: We could use png_set_read_status_fn to show progress bar of loading.
    Doesn't seem actually necessary, PNG would have to be truly huge to matter. }

  png_read_info(png_ptr, info_ptr);
  AllocateWidth := png_get_image_width(png_ptr, info_ptr);
  AllocateHeight := png_get_image_height(png_ptr, info_ptr);
  Result := nil; // initialize Result to allow easier try..except syntax below

  { Avoid the warning "Interlace handling should be turned on when using png_read_image"
    on interlaced images.
    See also:
    https://sourceforge.net/p/libpng/bugs/165/
      (the bug there can cause this warning in other cases too)
    https://stackoverflow.com/questions/22125146/how-to-fix-libpng-warning-when-reading-interlaced-png }
  png_set_interlace_handling(png_ptr);

  try
   IsAlpha := png_is_alpha(png_ptr, info_ptr);
   IsGrayscale := png_is_grayscale(png_ptr, info_ptr);

   { First, check to what TEncodedImage descendant our image file most matches.

     If it's not on the list of allowed classes, try to transform
     it to the most matching class. When doing this, we try to perform
     the most lossless conversion --- this means that we prefer to
     add image channel or expand grayscale->RGB than the other way around.
     For example, if image file is grayscale+alpha, we prefer to make
     it RGB+alpha (expand grayscale to RGB) than to grayscale (strip alpha).
   }

   if IsGrayScale then
   begin
     if IsAlpha then
     begin
       if not TransformPng(TGrayscaleAlphaImage) then
       if not TransformPng(TRGBAlphaImage) then
       if not TransformPng(TGrayscaleImage) then
       if not TransformPng(TRGBImage) then
         // ignore AllowedImageClasses if we cannot satisfy it, LoadEncodedImage will fix it
         TransformPng(TGrayscaleAlphaImage, false);
     end else
     begin
       if not TransformPng(TGrayscaleImage) then
       if not TransformPng(TGrayscaleAlphaImage) then
       if not TransformPng(TRGBImage) then
       if not TransformPng(TRGBAlphaImage) then
         // ignore AllowedImageClasses if we cannot satisfy it, LoadEncodedImage will fix it
         TransformPng(TGrayscaleImage, false);
     end;
   end else
   begin
     if IsAlpha then
     begin
       if not TransformPng(TRGBAlphaImage) then
       if not TransformPng(TRGBImage) then
       if not TransformPng(TGrayscaleAlphaImage) then
       if not TransformPng(TGrayscaleImage) then
         // ignore AllowedImageClasses if we cannot satisfy it, LoadEncodedImage will fix it
         TransformPng(TRGBAlphaImage, false);
     end else
     begin
       if not TransformPng(TRGBImage) then
       if not TransformPng(TRGBAlphaImage) then
       if not TransformPng(TGrayscaleImage) then
       if not TransformPng(TGrayscaleAlphaImage) then
         // ignore AllowedImageClasses if we cannot satisfy it, LoadEncodedImage will fix it
         TransformPng(TRGBImage, false);
     end;
   end;

   png_read_update_info(png_ptr, info_ptr);
   Assert(png_get_rowbytes(png_ptr, info_ptr) = PngResult.PixelSize * PngResult.Width,
     SysUtils.Format('internal error : applied wrong png transformations, width '+
       '%d with %d bytes per pixel gave row byte length %d instead of %d',
       [ PngResult.Width, PngResult.PixelSize,
         png_get_rowbytes(png_ptr, info_ptr),
         PngResult.PixelSize * PngResult.Width]));

   { now ready row_pointers as ScanLines }
   row_pointers := {$ifdef FPC}TFPList{$else}TList{$endif}.Create;
   try
    row_pointers.Count := result.Height;
    for i := 0 to result.Height-1 do
     row_pointers[i] := PngResult.RowPtr(PngResult.Height -i -1);
    { and go : uncompress image to result.data, therefore loading png image }
    png_read_image(png_ptr, PPPng_Byte(row_pointers.List));
   finally
    row_pointers.Free;
   end;

   png_read_end(png_ptr, nil);
  except Result.Free; raise end;

 finally
  if png_ptr <> nil then
  begin
   if info_ptr = nil then
    png_destroy_read_struct(@png_ptr, nil, nil) else
    png_destroy_read_struct(@png_ptr, @info_ptr, nil);
  end;
 end;
end;

{ SavePNG_LibPng --------------------------------------------------------------------- }

procedure SavePNG_LibPng(const Img: TEncodedImage; const Stream: TStream; const Interlaced: boolean = false);
var
  png_ptr: png_structp;
  info_ptr: png_infop;
  InterlaceType: UInt32;
  row_pointers: {$ifdef FPC}TFPList{$else}TList{$endif};
  i: Cardinal;
  ColorType: Int32;
begin
  if not (Img is TCastleImage) then
    raise EImageSaveError.CreateFmt('Saving to PNG image class %s not possible', [Img.ClassName]);

 png_ptr := nil;
 try
  png_ptr := png_create_write_struct(CurrentLibpngVersion,
    nil { we could pass here data to warning/error handlers },
    {$ifdef FPC} @ {$endif} our_png_error_fn,
    {$ifdef FPC} @ {$endif} our_png_warning_fn);
  Check( png_ptr <> nil, 'png_create_write_struct failed');

  info_ptr := png_create_info_struct(png_ptr);
  Check( info_ptr <> nil, 'png_create_info_struct failed');

  png_set_write_fn(png_ptr, Stream,
    {$ifdef FPC} @ {$endif} our_png_write_fn,
    {$ifdef FPC} @ {$endif} our_png_flush_fn);

  { TODO -- uzyj tu png_set_write_status_fn aby miec progressa }

  if interlaced then
   interlaceType := PNG_INTERLACE_ADAM7 else
   interlaceType := PNG_INTERLACE_NONE;

  if Img is TRGBImage then
   ColorType := PNG_COLOR_TYPE_RGB else
  if Img is TRGBAlphaImage then
   ColorType := PNG_COLOR_TYPE_RGBA else
  if Img is TGrayscaleImage then
   ColorType := PNG_COLOR_TYPE_GRAY else
  if Img is TGrayscaleAlphaImage then
   ColorType := PNG_COLOR_TYPE_GRAY_ALPHA else
   raise EImageSaveError.CreateFmt('Saving to PNG image class %s not possible', [Img.ClassName]);

  png_set_IHDR(png_ptr, info_ptr, Img.Width, Img.Height, 8, ColorType,
    interlaceType, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png_ptr, info_ptr);

  { After writing the header, we can write the image contents,
    in the same format we use for TCastleImage.
    This is much simpler than LoadPNG_LibPng, since we don't need to transform
    the format, e.g. we just use always 8 bits per channel. }

  row_pointers := {$ifdef FPC}TFPList{$else}TList{$endif}.Create;
  try
   row_pointers.Count := Img.Height;
   for i := 0 to Img.Height-1 do
    row_pointers[i] := TCastleImage(Img).RowPtr(Img.Height-i-1);
   png_write_image(png_ptr, PPPng_Byte(row_pointers.List));
  finally
   row_pointers.Free;
  end;

  png_write_end(png_ptr, nil);
 finally
  if png_ptr <> nil then
  begin
   if info_ptr <> nil then
    png_destroy_write_struct(@png_ptr, @info_ptr) else
    png_destroy_write_struct(@png_ptr, nil);
  end;
 end;
end;

{$endif} // defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)
