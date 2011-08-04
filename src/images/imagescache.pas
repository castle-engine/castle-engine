{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A cache for loading images (TImagesCache). }
unit ImagesCache;

interface

uses KambiUtils, Images;

{$define read_interface}

type
  { Internal for TImagesCache }
  TCachedImage = record
    References: Cardinal;
    FileName: string;
    Image: TImage;
  end;
  PCachedImage = ^TCachedImage;

  TDynArrayItem_1 = TCachedImage;
  PDynArrayItem_1 = PCachedImage;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I dynarray_1.inc}
  TDynCachedImageArray = TDynArray_1;

  { A cache of loaded images.

    The idea is that instead of @code(Image := LoadImage(...)) call
    @code(Image := Cache.LoadImage_IncReference(...)).
    Later, instead of freeing this image, call
    @code(LoadImage_DecReference(Image)). From your point of view, things
    will work the same. But if you expect to load many images from the
    same FileName, then you will get a great speed and memory saving,
    because image will only be actually loaded once. Notes:

    @unorderedList(
      @item(All passed here FileNames must be absolute, already expanded paths.
        In the future it's expected that this (just like LoadImage, actually)
        will be extended to load images from URLs.)

      @item(Note that in case of problems with loading,
        LoadImage_IncReference may raise an exception, just like normal
        LoadImage. In this case it's guaranteed that no reference will
        be incremented, of course. If LoadImage_IncReference returns
        in a normal way, then it will return something non-@nil, just like
        LoadImage does.)

      @item(For now, LoadImage is always called with
        AllowedImageClasses = [TRGBImage, TRGBAlphaImage,
        TGrayscaleImage, TGrayscaleAlphaImage] and ForbiddenConvs = [].
        Why ? Because this is the use case that I need right now... (I'm going
        to use this for VRML texture nodes.) This will be fixed
        (LoadImage_IncReference will get AllowedImageClasses and
        ForbiddenConvs parameters, will compare and save them in TImageCache
        record) as soon as the need will arise.)

      @item(LoadImage_DecReference alwas sets Image to @nil, like FreeAndNil.)
    )

    Note that before destroying this object you must free all images,
    i.e. call LoadImage_DecReference for all images allocated by
    LoadImage_IncReference. @italic(This class is not a lousy way
    of avoiding memory leaks) --- it would be a bad idea, because it would
    cause sloppy programming, where memory is unnecessarily allocated for
    a long time. In fact, this class asserts in destructor that no images
    are in cache anymore, so if you compiled with assertions enabled,
    this class does the job of memory-leak detector. }
  TImagesCache = class
  private
    CachedImages: TDynCachedImageArray;
    FOnEmpty: TProcedure;
  protected
    { If cache is empty, calls OnEmpty. Note that OnEmpty may destroy current
      instance, so call CheckEmpty only when you finished processing
      --- Self may be invalid afterwards. }
    procedure CheckEmpty;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadImage_IncReference(const FileName: string): TImage;
    procedure LoadImage_DecReference(var Image: TImage);

    function Empty: boolean; virtual;

    { Called when cache becomes empty. This is only for internal usage
      by VRMLNodes unit for now. }
    property OnEmpty: TProcedure read FOnEmpty write FOnEmpty;
  end;

{$undef read_interface}

implementation

uses SysUtils, KambiStringUtils;

{$define read_implementation}
{$I dynarray_1.inc}

{ $define DEBUG_CACHE}

constructor TImagesCache.Create;
begin
  inherited;
  CachedImages := TDynCachedImageArray.Create;
end;

destructor TImagesCache.Destroy;
begin
  if CachedImages <> nil then
  begin
    Assert(CachedImages.Count = 0, ' Some references to images still exist ' +
      'when freeing TImagesCache');
    FreeAndNil(CachedImages);
  end;
  inherited;
end;

function TImagesCache.LoadImage_IncReference(const FileName: string): TImage;
var
  I: Integer;
  C: PCachedImage;
begin
  C := @CachedImages.Items[0];
  for I := 0 to CachedImages.High do
  begin
    if C^.FileName = FileName then
    begin
      Inc(C^.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : image ', FileName, ' : ', C^.References);
      {$endif}

      Exit(C^.Image);
    end;
    Inc(C);
  end;

  { Initialize Result first, before calling CachedImages.Add.
    That's because in case LoadImage raises exception,
    we don't want to add image to cache (because caller would have
    no way to call LoadImage_DecReference later). }

  Result := LoadImage(FileName, [TRGBImage, TRGBAlphaImage,
    TGrayscaleImage, TGrayscaleAlphaImage], []);

  C := CachedImages.Add;
  C^.References := 1;
  C^.FileName := FileName;
  C^.Image := Result;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : image ', FileName, ' : ', 1);
  {$endif}
end;

procedure TImagesCache.LoadImage_DecReference(var Image: TImage);
var
  I: Integer;
  C: PCachedImage;
begin
  C := @CachedImages.Items[0];
  for I := 0 to CachedImages.High do
  begin
    if C^.Image = Image then
    begin
      {$ifdef DEBUG_CACHE}
      Writeln('-- : image ', C^.FileName, ' : ', C^.References - 1);
      {$endif}

      Image := nil;

      if C^.References = 1 then
      begin
        FreeAndNil(C^.Image);
        CachedImages.Delete(I);
        CheckEmpty;
      end else
        Dec(C^.References);

      Exit;
    end;
    Inc(C);
  end;

  raise EInternalError.CreateFmt(
    'TImagesCache.LoadImage_DecReference: no reference found for image %s',
    [PointerToStr(Image)]);
end;

procedure TImagesCache.CheckEmpty;
begin
  { Check Assigned(OnEmpty) first, as it's usually not assigned. }
  if Assigned(OnEmpty) and Empty then
    OnEmpty();
end;

function TImagesCache.Empty: boolean;
begin
  Result := CachedImages.Count = 0;
end;

end.