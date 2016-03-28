{
  Copyright 2009-2014 Michalis Kamburelis.

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
  See CastleGLImage for OpenGL-specific handling of textures and other images.

  Texture is any TEncodedImage instance. This includes not only
  a traditional 2D/3D matrix of pixels represented as TCastleImage,
  but also a texture compressed for GPU (TGPUCompressedImage). Moreover, a texture
  may have mipmaps defined --- they are stored inside TCompositeImage
  instance (that contains a list of TEncodedImage).

  Since not everything can really deal with such flexible definition
  of a texture, we decided to separate some routines specifically
  for textures. For example, you have LoadTextureImage to load full texture
  information --- contrast this with LoadImage routine in CastleImages unit,
  that only returns TCastleImage (a "normal" way to deal with image data). }
unit CastleTextureImages;

interface

uses CastleImages, CastleCompositeImage, CastleUtils, FGL, CastleVideos;

const
  { Image classes that are handled by absolutely all OpenGL versions. }
  TextureImageClasses: array [0..3] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

  { All image classes that may be handled by OpenGL.
    Some of them may require specific OpenGL extensions or versions
    (like GPU-compressed or float textures). }
  TextureImageClassesAll: array [0..5] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage,
    TGPUCompressedImage,
    TRGBFloatImage);

{ Load image suitable for a texture.
  This will load image to memory formats supported by common
  3D libraries (like OpenGL), for example it will never return TRGBFloatImage
  (although OpenGL may support it, but we cannot be sure at this point).
  It may return texture compressed using one of the GPU compression algorithms
  (see TTextureCompression).

  If the image comes from a TCompositeImage file (DDS, KTX...), it will also return it
  (if not, Composite returned will be @nil). This allows you to e.g. use
  texture mipmaps recorded there. Note that Composite.OwnsFirstImage is set
  to @false, so you can always safely free everything by simple
  @code(FreeAndNil(Image); FreeAndNil(Composite);).

  Overloaded version without Composite parameter assumes you're
  not interested in this information (still it handles Composite files of course,
  it just doesn't return Composite object instance).

  @groupBegin }
function LoadTextureImage(const URL: string; out Composite: TCompositeImage): TEncodedImage; overload;
function LoadTextureImage(const URL: string): TEncodedImage; overload;
{ @groupEnd }

type
  { A cache of loaded images for textures.

    Load by TextureImage_IncReference, free by TextureImage_DecReference.
    These replace LoadTextureImage, and manual freeing of Image/Composite.

    If you used IncReference that returns Composite, then you should also
    free using DecReference that takes Composite.
    If you used IncReference without Composite parameter, then also
    free using DecReference without Composite parameter.

    The idea is that instead of @code(LoadTextureImage(...)) call
    @code(Cache.TextureImage_IncReference(...)).
    Later, instead of freeing this image, call
    @code(TextureImage_DecReference(Image)). From your point of view, things
    will work the same. But if you expect to load many textures from the
    same URL, then you will get a great speed and memory saving,
    because image will only be actually loaded once. Notes:

    @unorderedList(
      @item(Note that in case of problems with loading,
        TextureImage_IncReference may raise an exception, just like normal
        LoadTextureImage. In this case it's guaranteed that no reference will
        be incremented, of course. If LoadTextureImage_IncReference returns
        in a normal way, then it will return something non-@nil, just like
        LoadTextureImage does.)

      @item(LoadTextureImage_DecReference alwas sets Image to @nil, like FreeAndNil.)

      @item(Since detecting image alpha channel type may be a little time-consuming
        (iteration over all pixels is needed), we also do it here
        and save in cache.)
    )

    Note that before destroying this object you must free all textures,
    i.e. call LoadTextureImage_DecReference for all images allocated by
    LoadTextureImage_IncReference. @italic(This class is not a lousy way
    of avoiding memory leaks) --- it would be a bad idea, because it would
    cause sloppy programming, where memory is unnecessarily allocated for
    a long time. In fact, this class asserts in destructor that no images
    are in cache anymore, so if you compiled with assertions enabled,
    this class does the job of memory-leak detector. }
  TTexturesVideosCache = class(TVideosCache)
  private
    type
      { Internal for TTexturesVideosCache. @exclude }
      TCachedTexture = class
        References: Cardinal;
        URL: string;
        Image: TEncodedImage;
        Composite: TCompositeImage;
        AlphaChannel: TAlphaChannel;
      end;
      TCachedTextureList = specialize TFPGObjectList<TCachedTexture>;
    var
      CachedTextures: TCachedTextureList;
  public
    constructor Create;
    destructor Destroy; override;

    function TextureImage_IncReference(const URL: string; out Composite: TCompositeImage;
      out AlphaChannel: TAlphaChannel): TEncodedImage;
    function TextureImage_IncReference(const URL: string;
      out AlphaChannel: TAlphaChannel): TEncodedImage;

    procedure TextureImage_DecReference(var Image: TEncodedImage; var Composite: TCompositeImage);
    procedure TextureImage_DecReference(var Image: TEncodedImage);

    function Empty: boolean; override;
  end;

implementation

uses SysUtils, CastleStringUtils, CastleLog, CastleURIUtils;

function LoadTextureImage(const URL: string; out Composite: TCompositeImage): TEncodedImage;
begin
  if not TCompositeImage.MatchesURL(URL) then
  begin
    Result := LoadEncodedImage(URL, TextureImageClasses);
    Composite := nil;
  end else
  begin
    Composite := TCompositeImage.Create;
    try
      Composite.LoadFromFile(URL);
      Composite.OwnsFirstImage := false;
      Result := Composite.Images[0];
    except
      FreeAndNil(Composite);
      raise;
    end;
  end;
end;

function LoadTextureImage(const URL: string): TEncodedImage;
var
  Composite: TCompositeImage;
begin
  Result := LoadTextureImage(URL, Composite);
  Composite.Free;
end;

{ TTexturesVideosCache ------------------------------------------------- }

{ $define DEBUG_CACHE}

constructor TTexturesVideosCache.Create;
begin
  inherited;
  CachedTextures := TCachedTextureList.Create;
end;

destructor TTexturesVideosCache.Destroy;
begin
  if CachedTextures <> nil then
  begin
    Assert(CachedTextures.Count = 0, ' Some references to texture images still exist ' +
      'when freeing TTexturesVideosCache');
    FreeAndNil(CachedTextures);
  end;
  inherited;
end;

function TTexturesVideosCache.TextureImage_IncReference(
  const URL: string; out Composite: TCompositeImage; out AlphaChannel: TAlphaChannel): TEncodedImage;
var
  I: Integer;
  C: TCachedTexture;
begin
  for I := 0 to CachedTextures.Count - 1 do
  begin
    C := CachedTextures[I];
    if C.URL = URL then
    begin
      Inc(C.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : texture image ', URL, ' : ', C.References);
      {$endif}

      Composite := C.Composite;
      AlphaChannel := C.AlphaChannel;
      Exit(C.Image);
    end;
  end;

  { Initialize Result first, before calling CachedTextures.Add.
    That's because in case LoadTextureImage raises exception,
    we don't want to add image to cache (because caller would have
    no way to call TextureImage_DecReference later). }

  Result := LoadTextureImage(URL, Composite);
  AlphaChannel := Result.AlphaChannel;

  C := TCachedTexture.Create;
  CachedTextures.Add(C);
  C.References := 1;
  C.URL := URL;
  C.Image := Result;
  C.Composite := Composite;
  C.AlphaChannel := AlphaChannel;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : texture image ', URL, ' : ', 1);
  {$endif}
  // if Log and (AlphaChannel <> acNone) then
  //   WritelnLog('Alpha Detection', 'Texture image ' + URIDisplay(URL) +
  //     ' detected as simple yes/no alpha channel: ' + BoolToStr[AlphaChannel = acSimpleYesNo]);
end;

procedure TTexturesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage; var Composite: TCompositeImage);
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
      Writeln('-- : texture image ', C.URL, ' : ', C.References - 1);
      {$endif}

      { We cannot simply assert

          C.Composite = Composite

        because when textures have many references,
        some references may be with and some without Composite information.
        We don't want to force all references to the same URL to always
        have or never have Composite information. (This would be uncomfortable
        for caller, as different nodes may share textures, e.g. VRML/X3D Background
        and ImageTexture nodes. They would all be forced to remember Composite
        information this way.)

        So we have to always keep Composite information in the cache,
        and free it, regardless of whether called knows this Composite information.

        Only if passed Composite <> nil (we know caller keeps it) then we can
        check it for correctness. }

      Assert((Composite = nil) or (C.Composite = Composite), 'Image pointers match in TTexturesVideosCache, Composite pointers should match too');

      Image := nil;
      Composite := nil;

      if C.References = 1 then
      begin
        FreeAndNil(C.Image);
        FreeAndNil(C.Composite);
        CachedTextures.Delete(I);
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TTexturesVideosCache.TextureImage_DecReference: no reference found for texture image %s',
    [PointerToStr(Image)]);
end;

function TTexturesVideosCache.TextureImage_IncReference(
  const URL: string; out AlphaChannel: TAlphaChannel): TEncodedImage;
var
  Dummy: TCompositeImage;
begin
  Result := TextureImage_IncReference(URL, Dummy, AlphaChannel);
end;

procedure TTexturesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage);
var
  Dummy: TCompositeImage;
begin
  Dummy := nil;
  TextureImage_DecReference(Image, Dummy);
end;

function TTexturesVideosCache.Empty: boolean;
begin
  Result := (inherited Empty) and (CachedTextures.Count = 0);
end;

end.
