{
  Copyright 2009-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handling of images for textures.
  This unit is not OpenGL-specific, it should be suitable for all 3D libraries.
  See GLImage for OpenGL-specific handling of textures and other images.

  Texture is any TEncodedImage instance. This includes not only
  a traditional 2D/3D matrix of pixels represented as TCastleImage,
  but also a compressed texture, TS3TCImage. Moreover, a texture
  may have mipmaps defined --- they are stored inside TDDSImage
  instance (that contains a list of TEncodedImage).

  Since not everything can really deal with such flexible definition
  of a texture, we decided to separate some routines specifically
  for textures. For example, you have LoadTextureImage to load full texture
  information --- contrast this with LoadImage routine in Images unit,
  that only returns TCastleImage (a "normal" way to deal with image data). }
unit TextureImages;

interface

uses Images, DDS, CastleUtils, VideosCache, FGL;

const
  { Image classes that are handled by absolutely all OpenGL versions. }
  TextureImageClasses: array [0..3] of TCastleImageClass = (
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
  { Internal for TTexturesImagesVideosCache. @exclude }
  TCachedTexture = class
    References: Cardinal;
    FileName: string;
    Image: TEncodedImage;
    DDS: TDDSImage;
  end;
  TCachedTextureList = specialize TFPGObjectList<TCachedTexture>;

  { A cache of loaded images for textures.

    Load by TextureImage_IncReference, free by TextureImage_DecReference.
    These replace LoadTextureImage, and manual freeing of Image/DDS.

    If you used IncReference that returns DDS, then you should also
    free using DecReference that takes DDS.
    If you used IncReference without DDS parameter, then also
    free using DecReference without DDS parameter.

    The idea is that instead of @code(LoadTextureImage(...)) call
    @code(Cache.TextureImage_IncReference(...)).
    Later, instead of freeing this image, call
    @code(TextureImage_DecReference(Image)). From your point of view, things
    will work the same. But if you expect to load many textures from the
    same FileName, then you will get a great speed and memory saving,
    because image will only be actually loaded once. Notes:

    @unorderedList(
      @item(All passed here FileNames must be absolute, already expanded paths.
        In the future it's expected that this (just like LoadImage
        and LoadTextureImage, actually)
        will be extended to load images from URLs.)

      @item(Note that in case of problems with loading,
        TextureImage_IncReference may raise an exception, just like normal
        LoadTextureImage. In this case it's guaranteed that no reference will
        be incremented, of course. If LoadTextureImage_IncReference returns
        in a normal way, then it will return something non-@nil, just like
        LoadTextureImage does.)

      @item(LoadTextureImage_DecReference alwas sets Image to @nil, like FreeAndNil.)
    )

    Note that before destroying this object you must free all textures,
    i.e. call LoadTextureImage_DecReference for all images allocated by
    LoadTextureImage_IncReference. @italic(This class is not a lousy way
    of avoiding memory leaks) --- it would be a bad idea, because it would
    cause sloppy programming, where memory is unnecessarily allocated for
    a long time. In fact, this class asserts in destructor that no images
    are in cache anymore, so if you compiled with assertions enabled,
    this class does the job of memory-leak detector. }
  TTexturesImagesVideosCache = class(TImagesVideosCache)
  private
    CachedTextures: TCachedTextureList;
  public
    constructor Create;
    destructor Destroy; override;

    function TextureImage_IncReference(const FileName: string; out DDS: TDDSImage): TEncodedImage; overload;
    function TextureImage_IncReference(const FileName: string): TEncodedImage; overload;

    procedure TextureImage_DecReference(var Image: TEncodedImage; var DDS: TDDSImage); overload;
    procedure TextureImage_DecReference(var Image: TEncodedImage); overload;

    function Empty: boolean; override;
  end;

implementation

uses SysUtils, CastleStringUtils;

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
  CachedTextures := TCachedTextureList.Create;
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
  C: TCachedTexture;
begin
  for I := 0 to CachedTextures.Count - 1 do
  begin
    C := CachedTextures[I];
    if C.FileName = FileName then
    begin
      Inc(C.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : texture image ', FileName, ' : ', C.References);
      {$endif}

      DDS := C.DDS;
      Exit(C.Image);
    end;
  end;

  { Initialize Result first, before calling CachedTextures.Add.
    That's because in case LoadTextureImage raises exception,
    we don't want to add image to cache (because caller would have
    no way to call TextureImage_DecReference later). }

  Result := LoadTextureImage(FileName, DDS);

  C := TCachedTexture.Create;
  CachedTextures.Add(C);
  C.References := 1;
  C.FileName := FileName;
  C.Image := Result;
  C.DDS := DDS;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : texture image ', FileName, ' : ', 1);
  {$endif}
end;

procedure TTexturesImagesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage; var DDS: TDDSImage);
var
  I: Integer;
  C: TCachedTexture;
begin
  for I := 0 to CachedTextures.Count - 1 do
  begin
    C := CachedTextures[I];
    if C.Image = Image then
    begin
      {$ifdef DEBUG_CACHE}
      Writeln('-- : texture image ', C.FileName, ' : ', C.References - 1);
      {$endif}

      { We cannot simply assert

          C.DDS = DDS

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

      Assert((DDS = nil) or (C.DDS = DDS), 'Image pointers match in TTexturesImagesVideosCache, DDS pointers should match too');

      Image := nil;
      DDS := nil;

      if C.References = 1 then
      begin
        FreeAndNil(C.Image);
        FreeAndNil(C.DDS);
        CachedTextures.Delete(I);
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
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
