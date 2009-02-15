{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ function our_png_error_fn and our_png_warning_fn are used as libpng
  error and warning handlers in both SavePNG and LoadPNG.
  They both require png_get_error_ptr(png_ptr) to be valid TDynStringArray
   object. Warning function
    - appends warning to string list
    - calls WarningWrite ONLY if compiled with DEBUG defined
   Error function raises EInvalidPng exception with appropriate message.
    Warnings saved in the string list are appended to the exception
    message - so expection message is error_message + all warning_messages.
 -------------------------------------------------------------------------- }

procedure our_png_error_fn(png_ptr : png_structp; s : png_const_charp);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
var warnings: TDynStringArray;
    errs: string;
    i: integer;
begin
 { we're raising exception EInvalidPng;
   we use error s and collected warnings to construct exception message }
 errs:='Png error - libpng returned error : '+s;
 warnings := TDynStringArray(png_get_error_ptr(png_ptr));
 if warnings.length > 0 then
 begin
  errs := errs+nl+'There were libpng warnings :';
  for i := 0 to warnings.high do errs := errs+nl+warnings[i];
 end;
 raise EInvalidPng.Create(errs);
end;

procedure our_png_warning_fn(png_ptr : png_structp; s : png_const_charp);
  {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
var warnings: TDynStringArray;
begin
 { we're collecting warnings from libpng. }
 warnings := TDynStringArray(png_get_error_ptr(png_ptr));
 warnings.AppendItem(s);
 (* {$ifdef DEBUG} WarningWrite('DEBUG ONLY message : libpng warning : '+s); {$endif} *)
end;

{ read / write funcs. One is used in LoadPNG and the other one in SavePNG but
  thay are very similar : thay both treat png_get_io_ptr(png_ptr) as TStream
  and read/write from/to that stream. Exceptions are raised if operation
  is not posiible (for example, stream end - this will happen m.in. jezeli png
  ktory usilujemy odczytac jest "truncated" - libpng w zaden sposob nie probuje
  nas nigdzie pytac o rozmiar danych do odczytu w strumieniu wiec jest jasne
  ze bledy w rodzaju niespodziewanego konca strumienia sami wychwycimy.

  Acha, mamy tu tez flush function (ktora nic nie robi; jak zrobic
  flush na strumieniu ?!? ).
  --------------------------------------------------------------------------
}

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
 {we would like to do here something like TStream(png_get_io_ptr(png_ptr)).Flush;
  but there is no "flush" method in TStream or any of its descendant; }
end;

{ LoadPNG ------------------------------------------------------------------- }

function _LoadAnyPNG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

  { TODO: this is copied from LoadImage impl, to be implemented more
    gracefully by TImageFormatHandler soon. }

  { check is Conv Forbidden, if it is -> raise exception }
  procedure DoingConversion(Conv: TImageLoadConversion);
  const
    ConvToStr: array[TImageLoadConversion]of string = (
    'delete alpha channel',
    'lose float precision',
    'flatten RGB colors to grayscale',
    'add dummy constant alpha channel',
    'add useless float precision',
    'expand grayscale to RGB (three channels)'
    );
  begin
    if Conv in ForbiddenConvs then
      raise EUnableToLoadImage.Create('LoadImage: to load this image format '+
        'conversion "'+ConvToStr[Conv]+'" must be done, but it is forbidden here');
  end;

  function ClassAllowed(ImageClass: TImageClass): boolean;
  begin
    Result := (High(AllowedImageClasses) = -1) or
      InImageClasses(ImageClass, AllowedImageClasses);
  end;

var
  PNGResult: TImage absolute Result;
  AllocateWidth, AllocateHeight: Cardinal;
  png_ptr: png_structp;
  info_ptr: png_infop;

  { Allocates Result to given class and applies libpng transforms to
    make resulting data matching given Result class.
    Does this (and returns @true) only if ImageClass and NeededConversions
    are allowed. }
  function TransformPng(ImageClass: TImageClass;
    const NeededConversions: TImageLoadConversions): boolean;
  begin
    Result := ClassAllowed(ImageClass) and
      (ForbiddenConvs * NeededConversions = []);

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
  row_pointers: TDynPointerArray;
  i: Cardinal;
  warningslist: TDynStringArray;
  IsAlpha, IsGrayscale: boolean;
begin
 warningslist := TDynStringArray.Create;
 png_ptr := nil;
 try
  png_ptr := png_create_read_struct(SO_PNG_LIBPNG_VER_STRING,
    WarningsList,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_error_fn,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_warning_fn);
  Check( png_ptr <> nil, 'png_create_read_struct failed');

  info_ptr := png_create_info_struct(png_ptr);
  Check( info_ptr <> nil, 'png_create_info_struct failed');

  png_set_read_fn(png_ptr, Stream,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_read_fn);

  { TODO -- uzyj tu png_set_read_status_fn aby miec progressa }

  png_read_info(png_ptr, info_ptr);
  AllocateWidth := png_get_image_width(png_ptr, info_ptr);
  AllocateHeight := png_get_image_height(png_ptr, info_ptr);
  result := nil; { zeby mozna bylo ponizej zapisac wygodnie try..except }

  try
   IsAlpha := png_is_alpha(png_ptr, info_ptr);
   IsGrayscale := png_is_grayscale(png_ptr, info_ptr);

   { First, check to what TImage descendant (we can make four,
     with 1, 2, 3, 4 number of components) our image file most matches.

     If it's not on the list of allowed classes, try to transform
     it to the most matching class. When doing this, we try to perform
     the most lossless convertion --- this means that we prefer to
     add image channel or expand grayscale->RGB than the other way around.
     For example, if image file is grayscale+alpha, we prefer to make
     it RGB+alpha (expand grayscale to RGB) than to grayscale (strip alpha).
   }

   if IsGrayScale then
   begin
     if IsAlpha then
     begin
       if not TransformPng(TGrayscaleAlphaImage, []) then
       if not TransformPng(TRGBAlphaImage, [ilcGrayscaleExpandToRGB]) then
       if not TransformPng(TGrayscaleImage, [ilcAlphaDelete]) then
       if not TransformPng(TRGBImage, [ilcGrayscaleExpandToRGB, ilcAlphaDelete]) then
         raise EUnableToLoadImage.Create('Cannot load PNG image to required class');
     end else
     begin
       if not TransformPng(TGrayscaleImage, []) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcAlphaAdd]) then
       if not TransformPng(TRGBImage, [ilcGrayscaleExpandToRGB]) then
       if not TransformPng(TRGBAlphaImage, [ilcAlphaAdd, ilcGrayscaleExpandToRGB]) then
         raise EUnableToLoadImage.Create('Cannot load PNG image to required class');
     end;
   end else
   begin
     if IsAlpha then
     begin
       if not TransformPng(TRGBAlphaImage, []) then
       if not TransformPng(TRGBImage, [ilcAlphaDelete]) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcRGBFlattenToGrayscale]) then
       if not TransformPng(TGrayscaleImage, [ilcAlphaDelete, ilcRGBFlattenToGrayscale]) then
         raise EUnableToLoadImage.Create('Cannot load PNG image to required class');
     end else
     begin
       if not TransformPng(TRGBImage, []) then
       if not TransformPng(TRGBAlphaImage, [ilcAlphaAdd]) then
       if not TransformPng(TGrayscaleImage, [ilcRGBFlattenToGrayscale]) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcAlphaAdd, ilcRGBFlattenToGrayscale]) then
         raise EUnableToLoadImage.Create('Cannot load PNG image to required class');
     end;
   end;

   png_read_update_info(png_ptr, info_ptr);
   Assert(png_get_rowbytes(png_ptr, info_ptr) = Result.PixelSize * Result.Width,
     SysUtils.Format('internal error : applied wrong png transformations, width '+
       '%d with %d bytes per pixel gave row byte length %d instead of %d',
       [ Result.Width, Result.PixelSize,
         png_get_rowbytes(png_ptr, info_ptr),
         Result.PixelSize * Result.Width]));

   { now ready row_pointers as ScanLines }
   row_pointers := TDynPointerArray.Create(result.Height);
   try
    for i := 0 to result.Height-1 do
     row_pointers[i] := Result.RowPtr(Result.Height -i -1);
    { and go : uncompress image to result.data, therefore loading png image }
    png_read_image(png_ptr, @row_pointers.Items[0]);
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
  FreeAndNil(warningslist);
 end;
end;

function LoadAnyPNG(Stream: TStream;
  FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;
begin
  case FormatRequired of
    frAny         : Result := _LoadAnyPNG(Stream, [], []);
    { For FormatRequired <> frAny, this reflects previous
      frWithoutAlpha/frAny behavior: only RGB or RGBAlpha is possible. }
    frWithoutAlpha:
      if ConvertToRequired then
        Result := _LoadAnyPNG(Stream, [TRGBImage], []) else
        Result := _LoadAnyPNG(Stream, [TRGBImage], [ilcAlphaDelete]);
    frWithAlpha   :
      if ConvertToRequired then
        Result := _LoadAnyPNG(Stream, [TRGBAlphaImage], []) else
        Result := _LoadAnyPNG(Stream, [TRGBAlphaImage], [ilcAlphaAdd]);
    else raise EInternalError.Create('LoadAnyPNG:FormatRequired?');
  end;
end;

function LoadPNG(Stream: TStream): TRGBImage;
begin
 result := _LoadAnyPNG(Stream, [TRGBImage], []) as TRGBImage;
end;

{ Save[Any]PNG --------------------------------------------------------------------- }

procedure SaveAnyPNG(const Img: TImage; Stream: TStream; Interlaced: boolean);
var png_ptr: png_structp;
    info_ptr: png_infop;
    warningslist: TDynStringArray;
    InterlaceType: LongWord;
    row_pointers: TDynPointerArray;
    i: Cardinal;
    ColorType: LongInt;
begin
 warningslist := TDynStringArray.Create;
 png_ptr := nil;
 try
  png_ptr := png_create_write_struct(SO_PNG_LIBPNG_VER_STRING,
    WarningsList,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_error_fn,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_warning_fn);
  Check( png_ptr <> nil, 'png_create_write_struct failed');

  info_ptr := png_create_info_struct(png_ptr);
  Check( info_ptr <> nil, 'png_create_info_struct failed');

  png_set_write_fn(png_ptr, Stream,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_write_fn,
    {$ifdef FPC_OBJFPC} @ {$endif} our_png_flush_fn);

  { TODO -- uzyj tu png_set_write_status_fn aby miec progressa }

  if interlaced then
   interlaceType := PNG_INTERLACE_ADAM7 else
   interlaceType := PNG_INTERLACE_NONE;

  if Img is TRGBImage then
   ColorType := PNG_COLOR_TYPE_RGB else
  if Img is TRGBAlphaImage then
   ColorType := PNG_COLOR_TYPE_RGBA else
   raise Exception.Create('Can''t save to PNG images with this kind');

  png_set_IHDR(png_ptr, info_ptr, Img.Width, Img.Height, 8, ColorType,
    interlaceType, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png_ptr, info_ptr);

  { zapisalismy header i jestesmy bardzo happy bo nie musimy teraz robic tego
    co bylo nasza bieda w LoadPNG : nakladanie odpowiednich transformacji.
    Zawsze zapisujemy obrazek w takim formacie w jakim go przechowujemy w
    TImage - a wiec bitdepth = 8 , type = RGB[A] i juz. }

  { wiec ready row_pointers and go }
  row_pointers := TDynPointerArray.Create(Img.Height);
  try
   for i := 0 to Img.Height-1 do
    row_pointers[i] := Img.RowPtr(Img.Height-i-1);
   png_write_image(png_ptr, @row_pointers.Items[0]);
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
  FreeAndNil(warningslist);
 end;
end;

procedure SavePNG(const Img: TRGBImage; Stream: TStream; Interlaced: boolean);
begin SaveAnyPNG(Img, Stream, Interlaced) end;

procedure SavePNG(const Img: TRGBImage; Stream: TStream); { interlaced = false }
begin SavePNG(Img, Stream, false) end;
