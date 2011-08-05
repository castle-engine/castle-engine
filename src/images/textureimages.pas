{
  Copyright 2009-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handling of images that will be used as textures.
  This unit is not OpenGL specific, it should be suitable for all 3D libraries. }
unit TextureImages;

{ Some internal comments about why this unit is needed at all.
  For a long, long time all my images were represented by TImage class.
  There was no DDS, no S3TC and everything was beautiful and simple.

  Well, actually, there was one ugly thing: everything that didn't depend
  on OpenGL had to take some parameter like AllowedImageClasses,
  and accept it (and some routines didn't accept it in reality,
  because TImagesCache didn't honour it, because it would severely
  complicate it for no practical gain).

  When DDS and S3TC compressed textures came in, problems started.
  A lot of routines dealing with texture had to be extended to accept
  TS3TCImage, and generally TEncodedImage. That was good, but it made
  loading images for them really uncomfortable: special code to
  take DDS, and optionally extract S3TC from DDS, was needed,
  instead of previous LoadImage. Moreover, TImagesCache.LoadImage_IncReference
  couldn't be used.

  One solution, that I was toying with for 2 days, was to make more things
  to accept TEncodedImage instead of just TImage. For example,
  allow LoadImage to return any TEncodedImage, and let called specify
  whether TS3TCImage is allowed by AllowedImageClasses parameter to
  LoadImage. But this was flawed.
  It required a huge lot of existing code to be changed, basically
  just to fail gracefully if "not (Image is TImage)", which was ugly.
  If you need class checks and casts everywhere around, then something
  is just wrong. Moreover, and this is really the key point, note that
  for textures even TEncodedImage is not enough: eventually more texture
  routines should know about full DDS file, to be able to use mipmaps
  from DDS file. Trying to push this inside normal LoadImage would be simply
  wrong. And no, I don't want to make TDDSImage an ancestor or descendant
  of TEncodedImage --- logically, they should stay independent classes.

  The right solution turned out to be this trivial unit. We simply
  admit that we need different routines to "load texture" and "load simple
  editable image", and we need different cache classes for them.
  Texture loading and cache support TEncodedImage (so they allow S3TC)
  and allow returning associated DDS file, so texture routines are happy.

  As a side result, this also eliminates the need for all those
  AllowedImageClasses fields in various VRML texture etc. nodes.
  This unit, although not dependent on OpenGL, knows which image memory
  formats are suitable for textures, and that's it.
}
{ }

interface

uses Images, DDS, KambiUtils, VideosCache;

{$define read_interface}

const
  { Image classes that are handled by absolutely all OpenGL versions. }
  TextureImageClasses: array [0..3] of TImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

  { All image classes that may be handled by OpenGL.
    Some of them may require specific OpenGL extensions or versions
    (like S3TC or float textures). }
  TextureImageClassesAll: array [0..5] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage,
    TS3TCImage,
    TRGBFloatImage);

{ Load image suitable for a texture.
  This will load image to memory formats supported by common
  3D libraries (like OpenGL), for example it will never return TRGBFloatImage
  (although OpenGL may support it, but we cannot be sure at this point).
  It may return S3TC encoded image.

  If the image comes from a DDS file, it will also return it
  (if not, DDS returned will be @nil). This allows you to e.g. use
  texture mipmaps recorded there. Note that DDS.OwnsFirstImage is set
  to @false, so you can always safely free everything by simple
  @code(FreeAndNil(Image); FreeAndNil(DDS);).

  Overloaded version without DDS parameter assumes you're
  not interested in this information (still it handles DDS files of course,
  it just doesn't return DDS object instance).

  @groupBegin }
function LoadTextureImage(const FileName: string; out DDS: TDDSImage): TEncodedImage; overload;
function LoadTextureImage(const FileName: string): TEncodedImage; overload;
{ @groupEnd }

type
  { Internal for TTexturesImagesVideosCache }
  TCachedTexture = record
    References: Cardinal;
    FileName: string;
    Image: TEncodedImage;
    DDS: TDDSImage;
  end;
  PCachedTexture = ^TCachedTexture;

  TDynArrayItem_1 = TCachedTexture;
  PDynArrayItem_1 = PCachedTexture;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I dynarray_1.inc}
  TDynCachedTextureArray = TDynArray_1;

  { A cache of loaded images for textures.

    Load by TextureImage_IncReference, free by TextureImage_DecReference.
    These replace LoadTextureImage, and manual freeing of Image/DDS.

    If you used IncReference that returns DDS, then you should also
    free using DecReference that takes DDS.
    If you used IncReference without DDS parameter, then also
    free using DecReference without DDS parameter.

    See TImagesCache comments for more detailed instructions how to properly
    use the cache. }
  TTexturesImagesVideosCache = class(TImagesVideosCache)
  private
    CachedTextures: TDynCachedTextureArray;
  public
    constructor Create;
    destructor Destroy; override;

    function TextureImage_IncReference(const FileName: string; out DDS: TDDSImage): TEncodedImage; overload;
    function TextureImage_IncReference(const FileName: string): TEncodedImage; overload;

    procedure TextureImage_DecReference(var Image: TEncodedImage; var DDS: TDDSImage); overload;
    procedure TextureImage_DecReference(var Image: TEncodedImage); overload;
    
    function Empty: boolean; override;
  end;

{$undef read_interface}

implementation

uses SysUtils, KambiStringUtils;

{$define read_implementation}
{$I dynarray_1.inc}

function LoadTextureImage(const FileName: string; out DDS: TDDSImage): TEncodedImage;
begin
  if FileExtToImageFormatDef(ExtractFileExt(FileName),
    false, false, ifBMP) <> ifDDS then
  begin
    Result := LoadImage(FileName, TextureImageClasses, []);
    DDS := nil;
  end else
  begin
    DDS := TDDSImage.Create;
    try
      DDS.LoadFromFile(FileName);
      DDS.OwnsFirstImage := false;
      Result := DDS.Images[0];
    except
      FreeAndNil(DDS);
      raise;
    end;
  end;
end;

function LoadTextureImage(const FileName: string): TEncodedImage;
var
  DDS: TDDSImage;
begin
  Result := LoadTextureImage(FileName, DDS);
  DDS.Free;
end;

{ TTexturesImagesVideosCache ------------------------------------------------- }

{ $define DEBUG_CACHE}

constructor TTexturesImagesVideosCache.Create;
begin
  inherited;
  CachedTextures := TDynCachedTextureArray.Create;
end;

destructor TTexturesImagesVideosCache.Destroy;
begin
  if CachedTextures <> nil then
  begin
    Assert(CachedTextures.Count = 0, ' Some references to texture images still exist ' +
      'when freeing TTexturesImagesVideosCache');
    FreeAndNil(CachedTextures);
  end;
  inherited;
end;

function TTexturesImagesVideosCache.TextureImage_IncReference(
  const FileName: string; out DDS: TDDSImage): TEncodedImage;
var
  I: Integer;
  C: PCachedTexture;
begin
  C := @CachedTextures.Items[0];
  for I := 0 to CachedTextures.Count - 1 do
  begin
    if C^.FileName = FileName then
    begin
      Inc(C^.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : texture image ', FileName, ' : ', C^.References);
      {$endif}

      DDS := C^.DDS;
      Exit(C^.Image);
    end;
    Inc(C);
  end;

  { Initialize Result first, before calling CachedTextures.Add.
    That's because in case LoadTextureImage raises exception,
    we don't want to add image to cache (because caller would have
    no way to call TextureImage_DecReference later). }

  Result := LoadTextureImage(FileName, DDS);

  C := CachedTextures.Add;
  C^.References := 1;
  C^.FileName := FileName;
  C^.Image := Result;
  C^.DDS := DDS;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : texture image ', FileName, ' : ', 1);
  {$endif}
end;

procedure TTexturesImagesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage; var DDS: TDDSImage);
var
  I: Integer;
  C: PCachedTexture;
begin
  C := @CachedTextures.Items[0];
  for I := 0 to CachedTextures.Count - 1 do
  begin
    if C^.Image = Image then
    begin
      {$ifdef DEBUG_CACHE}
      Writeln('-- : texture image ', C^.FileName, ' : ', C^.References - 1);
      {$endif}

      { We cannot simply assert

          C^.DDS = DDS

        because when textures have many references,
        some references may be with and some without DDS information.
        We don't want to force all references to the same URL to always
        have or never have DDS information. (This would be uncomfortable
        for caller, as different nodes may share textures, e.g. VRML Background
        and ImageTexture nodes. They would all be forced to remember DDS
        information this way.)

        So we have to always keep DDS information in the cache,
        and free it, regardless of whether called knows this DDS information.

        Only if passed DDS <> nil (we know caller keeps it) then we can
        check it for correctness. }

      Assert((DDS = nil) or (C^.DDS = DDS), 'Image pointers match in TTexturesImagesVideosCache, DDS pointers should match too');

      Image := nil;
      DDS := nil;

      if C^.References = 1 then
      begin
        FreeAndNil(C^.Image);
        FreeAndNil(C^.DDS);
        CachedTextures.Delete(I);
        CheckEmpty;
      end else
        Dec(C^.References);

      Exit;
    end;
    Inc(C);
  end;

  raise EInternalError.CreateFmt(
    'TTexturesImagesVideosCache.TextureImage_DecReference: no reference found for texture image %s',
    [PointerToStr(Image)]);
end;

function TTexturesImagesVideosCache.TextureImage_IncReference(
  const FileName: string): TEncodedImage;
var
  Dummy: TDDSImage;
begin
  Result := TextureImage_IncReference(FileName, Dummy);
end;

procedure TTexturesImagesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage);
var
  Dummy: TDDSImage;
begin
  Dummy := nil;
  TextureImage_DecReference(Image, Dummy);
end;

function TTexturesImagesVideosCache.Empty: boolean;
begin
  Result := (inherited Empty) and (CachedTextures.Count = 0);
end;

end.
