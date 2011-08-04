{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ function our_png_error_fn and our_png_warning_fn are used as libpng
  error and warning handlers in both SavePNG and LoadPNG.

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
  OnWarning(wtMinor, 'PNG', S);
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

{ We dereference here memory allocated with PNG, so pointer checks
  will make false errors. }
{$checkpointer off}

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

{$ifdef KAMBI_CHECK_POINTER}
{ Turn checkpointer back on }
{$checkpointer on}
{$endif}

{ LoadPNG ------------------------------------------------------------------- }

function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

  { DoingConversion and ClassAllowed are only shortcuts to
    global utilities. }
  procedure DoingConversion(const Conv: TImageLoadConversion);
  begin
    Images.DoingConversion(Conv, ForbiddenConvs);
  end;

  function ClassAllowed(ImageClass: TImageClass): boolean;
  begin
    Result := Images.ClassAllowed(ImageClass, AllowedImageClasses);
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
  row_pointers: TFPList;
  i: Cardinal;
  IsAlpha, IsGrayscale: boolean;
begin
 png_ptr := nil;
 try
  png_ptr := png_create_read_struct(SO_PNG_LIBPNG_VER_STRING,
    nil { we could pass here data to warning/error handlers },
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
         raise EUnableToLoadImage.CreateFmt('Cannot load PNG image to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
     end else
     begin
       if not TransformPng(TGrayscaleImage, []) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcAlphaAdd]) then
       if not TransformPng(TRGBImage, [ilcGrayscaleExpandToRGB]) then
       if not TransformPng(TRGBAlphaImage, [ilcAlphaAdd, ilcGrayscaleExpandToRGB]) then
         raise EUnableToLoadImage.CreateFmt('Cannot load PNG image to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
     end;
   end else
   begin
     if IsAlpha then
     begin
       if not TransformPng(TRGBAlphaImage, []) then
       if not TransformPng(TRGBImage, [ilcAlphaDelete]) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcRGBFlattenToGrayscale]) then
       if not TransformPng(TGrayscaleImage, [ilcAlphaDelete, ilcRGBFlattenToGrayscale]) then
         raise EUnableToLoadImage.CreateFmt('Cannot load PNG image to required %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
     end else
     begin
       if not TransformPng(TRGBImage, []) then
       if not TransformPng(TRGBAlphaImage, [ilcAlphaAdd]) then
       if not TransformPng(TGrayscaleImage, [ilcRGBFlattenToGrayscale]) then
       if not TransformPng(TGrayscaleAlphaImage, [ilcAlphaAdd, ilcRGBFlattenToGrayscale]) then
         raise EUnableToLoadImage.CreateFmt('Cannot load PNG image to required %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
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
   row_pointers := TFPList.Create;
   try
    row_pointers.Count := result.Height;
    for i := 0 to result.Height-1 do
     row_pointers[i] := Result.RowPtr(Result.Height -i -1);
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

{ SavePNG --------------------------------------------------------------------- }

procedure SavePNG(Img: TImage; Stream: TStream; Interlaced: boolean);
var png_ptr: png_structp;
    info_ptr: png_infop;
    InterlaceType: LongWord;
    row_pointers: TFPList;
    i: Cardinal;
    ColorType: LongInt;
begin
 png_ptr := nil;
 try
  png_ptr := png_create_write_struct(SO_PNG_LIBPNG_VER_STRING,
    nil { we could pass here data to warning/error handlers },
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
  if Img is TGrayscaleImage then
   ColorType := PNG_COLOR_TYPE_GRAY else
  if Img is TGrayscaleAlphaImage then
   ColorType := PNG_COLOR_TYPE_GRAY_ALPHA else
   raise EUnableToSaveImage.CreateFmt('Saving to PNG image class %s not possible', [Img.ClassName]);

  png_set_IHDR(png_ptr, info_ptr, Img.Width, Img.Height, 8, ColorType,
    interlaceType, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png_ptr, info_ptr);

  { zapisalismy header i jestesmy bardzo happy bo nie musimy teraz robic tego
    co bylo nasza bieda w LoadPNG : nakladanie odpowiednich transformacji.
    Zawsze zapisujemy obrazek w takim formacie w jakim go przechowujemy w
    TImage - a wiec bitdepth = 8 , type = RGB[A] i juz. }

  { wiec ready row_pointers and go }
  row_pointers := TFPList.Create;
  try
   row_pointers.Count := Img.Height;
   for i := 0 to Img.Height-1 do
    row_pointers[i] := Img.RowPtr(Img.Height-i-1);
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

procedure SavePNG(Img: TImage; Stream: TStream); { interlaced = false }
begin
  SavePNG(Img, Stream, false)
end;
