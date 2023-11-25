{
  Copyright 2009-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Composite (like KTX or DDS) image file format handling (TCompositeImage). }
unit CastleInternalCompositeImage;

{$I castleconf.inc}

interface

uses Classes, CastleImages;

type
  EInvalidCompositeImage = class(EInvalidImageFormat);
  EInvalidDDS = class(EInvalidCompositeImage);
  EInvalidKTX = class(EInvalidCompositeImage);

  { Type of data in a TCompositeImage file.
    This doesn't take into account mipmaps (they are orthogonal to types here). }
  TCompositeType = (
    ctTexture,
    ctCubeMap,
    ctVolume);

  { Cube map faces.
    Always interpreted in right-handed orientation (like for OpenGL or OpenGL ES)
    by our engine.
    Order matches the order of OpenGL constants
    GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z_ARB. }
  TCubeMapSide = (
    csPositiveX,
    csNegativeX,
    csPositiveY,
    csNegativeY,
    csPositiveZ,
    csNegativeZ);
  TCubeMapSides = set of TCubeMapSide;

  TCubeMapImages = array [TCubeMapSide] of TCastleImage;

  { Composite image file (like KTX or DDS). This supports image that can have mipmaps,
    can be 3D, and can be a cubemap. This is something more than our TCastleImage
    (or TEncodedImage), which can only be a single pixel matrix (it can be 3D,
    but it cannot be a cubemap or have mipmaps).

    In essence, this is a container for a sequence of simple images
    in the @link(Images) property.
    The interpretation of the image sequence
    depends on other fields: first of all @link(CompositeType) and @link(Mipmaps).

    The basic usage of this class is to load a file using LoadFromFile
    or LoadFromStream.

    Note that you can write (change) many properties of this class.
    This allows you to create, or load and edit, composite files.
    You can even later save the composite image back to the stream (like a file) by
    SaveToStream or SaveToFile. Be careful though: you're responsible then
    to set all properties to sensible values. For example, the length
    (and interpretation) of @link(Images) list is determined by other properties
    of this class, so be sure to set them all to something sensible. }
  TCompositeImage = class
  private
    type
      // TODO: not OK that cubemap stuff is DDS-specific in this file.
      { DDS cube map sides.

        Compared with TCubeMapSide type, the meaning
        positive/negative Y faces is swapped.

        Reason: Cube map sides are named and written in DDS file
        in a way natural for DirectX, and DirectX has left-handed coordinate system,
        which means that one axis seems reverted when you want OpenGL right-handed
        coord system (like
        OpenGL, see http://opengl.org/registry/specs/ARB/texture_cube_map.txt).
        See [https://castle-engine.io/x3d_implementation_status.php#section_dds]
        for more. }
      TDDSCubeMapSide = (
        dcsPositiveX,
        dcsNegativeX,
        dcsPositiveY,
        dcsNegativeY,
        dcsPositiveZ,
        dcsNegativeZ);
      TDDSCubeMapSides = set of TDDSCubeMapSide;
    const
      { Convert TDDSCubeMapSide to TCubeMapSide. }
      DDSToCubeMapSide: array [TDDSCubeMapSide] of TCubeMapSide =
      ( csPositiveX,
        csNegativeX,
        csNegativeY,
        csPositiveY,
        csPositiveZ,
        csNegativeZ
      );
      { Convert TCubeMapSide to TDDSCubeMapSide. }
      DDSFromCubeMapSide: array [TCubeMapSide] of TDDSCubeMapSide =
      ( dcsPositiveX,
        dcsNegativeX,
        dcsNegativeY,
        dcsPositiveY,
        dcsPositiveZ,
        dcsNegativeZ
      );
    var
    FImages: TEncodedImageList;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FCompositeType: TCompositeType;
    FMipmaps: boolean;
    FMipmapsCount: Cardinal;
    FCubeMapSides: TDDSCubeMapSides;
    FDepth: Cardinal;
    FOwnsFirstImage: boolean;
    function GetImages(const Index: Integer): TEncodedImage;
    function GetCubeMapSides: TCubeMapSides;
    procedure SetCubeMapSides(const Value: TCubeMapSides);
  public
    { Some DDS files specify unknown GPU texture compression.
      More precisely, the DxgiFormat is equal DXGI_FORMAT_UNKNOWN
      (see https://msdn.microsoft.com/en-us/library/windows/desktop/bb173059(v=vs.85).aspx ).
      This can happen if you used PvrTexTool and compressed to PVRTC2_* formats.

      To be able to read such DDS files anyway,
      set AutomaticCompression to true and set AutomaticCompressionType
      as appropriate. }
    class var AutomaticCompression: boolean;
    class var AutomaticCompressionType: TTextureCompression;

    constructor Create;
    destructor Destroy; override;

    { Images sequence stored in this composite file.

      This has always length > 0 when file is successfully loaded
      (that is, when LoadFromStream method finished without raising any
      exception). }
    property Images: TEncodedImageList read FImages;

    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;

    property CompositeType: TCompositeType read FCompositeType write FCompositeType;

    { Does this composite image contain mipmaps.
      If @true, then all @link(Images) are guaranteed to have sizes
      being power of 2. }
    property Mipmaps: boolean read FMipmaps write FMipmaps;
    { Mipmaps count.
      Always 1 when @link(Mipmaps) = @false, this is usually comfortable. }
    property MipmapsCount: Cardinal read FMipmapsCount write FMipmapsCount;

    { Present cube map sides.
      Valid only when image is loaded and is ctCubeMap. }
    property CubeMapSides: TCubeMapSides read GetCubeMapSides write SetCubeMapSides;

    { Depth of volume (3D) texture.
      Always 1 when CompositeType is not ctVolume, this is usually comfortable. }
    property Depth: Cardinal read FDepth write FDepth;

    { Return given side of cube map.
      Assumes CompositeType = ctCubeMap and CubeMapSides = all.

      Level is mipmap level. Pass 0 for base level.
      When not @link(Mipmaps), Level must be 0. }
    function CubeMapImage(const Side: TCubeMapSide;
      const Level: Cardinal = 0): TEncodedImage;

    { Load composite (KTX or DDS) image from any TStream.
      The image type is recognized from the MimeType extension,
      or (if empty) from Url,
      so make sure that you provide at least one of these parameters.
      @raises(EInvalidCompositeImage In case of any error in the file data.) }
    procedure LoadFromStream(Stream: TStream; const Url: String;
      MimeType: string = '';
      const Options: TLoadImageOptions = []);

    { Load composite (KTX or DDS) image from this Url. }
    procedure LoadFromFile(Url: String; const Options: TLoadImageOptions = []);

    procedure SaveToStream(Stream: TStream; const MimeType: string);
    procedure SaveToFile(const Url: String);

    { Close all loaded image data. Effectively, this releases all data
      loaded by LoadFromStream, reverting the object to the state right
      after creation. }
    procedure Close;

    { When @false, then closing this composite image will not free Images[0].
      Closing happens when you call the @link(Close) method or
      destructor of this object. When this is @false, you're responsible
      to storing and freeing Images[0] later yourself, or you'll get memory
      leaks. }
    property OwnsFirstImage: boolean read FOwnsFirstImage write FOwnsFirstImage
      default true;

    { Convert 3D images in @link(Images) list into a sequences of 2D images.
      Useful utility for 3d (volume) textures.

      Normal loading of 3D composite textures creates single TCastleImage (using Depth
      possibly > 1) for each mipmap level. Such TCastleImage with depth
      is comfortable if you want to load this 3d texture into OpenGL
      (as then the image data is just a continuous memory area,
      loadable by glTexImage3d). But it's not comfortable if you want
      to display it using some 2D GUI. For example, it's not comfortable
      for image viewer like castle-view-image.

      So this method will convert such TCastleImage instances (with Depth > 1)
      into a sequence of TCastleImage instances all with Depth = 1.
      This isn't difficult, memory contents on 3d TCastleImage may be splitted
      into many 2d TCastleImage instances without problems.

      Note that it's safe to do this before saving the image.
      SaveToFile/SaveToStream methods accept both layouts of images
      (because, as said, memory contents actually are the same before
      and after splitting).

      Note that this may free all Images (possibly even whole Images object),
      disregarding OwnsFirstImage (as it would be difficult, since
      it may or may not replace it with new images). }
    procedure Flatten3d;

    { Decompress texture images (if any) on the @link(Images) list,
      replacing them with uncompressed equivalents.
      This can be used to decompress textures compressed using
      GPU compression algorithms, see @link(TTextureCompression).
      See TGPUCompressedImage.Decompress.

      Just like @link(Flatten3d):
      Note that this may free all Images (possibly even whole Images object),
      disregarding OwnsFirstImage (as it would be difficult, since
      it may or may not replace it with new images).

      @raises(ECannotDecompressTexture If some image cannot be decompressed
        for any reason.) }
    procedure DecompressTexture;

    { Does this URL look like it contains composite (KTX, DDS...) contents.
      Guesses by processing the URL with @link(ProcessImageUrl)
      and then looking at final filename extension. }
    class function MatchesUrl(Url: String): boolean;

    procedure AddCubeMapImages(const AImages: TCubeMapImages);
  end;

const
  AllCubeMapSides = [Low(TCubeMapSide) .. High(TCubeMapSide)];

  CompositeTypeToString: array [TCompositeType] of string =
  ( 'Texture', 'CubeMap', 'Volume' );

implementation

uses SysUtils, Math,
  CastleUtils, CastleClassUtils, CastleLog, CastleStringUtils,
  CastleVectors, CastleDownload, CastleUriUtils;

{$I castleinternalcompositeimage_format_handler.inc}
{$I castleinternalcompositeimage_dds.inc}
{$I castleinternalcompositeimage_ktx.inc}

{ TCompositeImage ------------------------------------------------------------------ }

constructor TCompositeImage.Create;
begin
  inherited;
  FOwnsFirstImage := true;
  FImages := TEncodedImageList.Create(false);
end;

destructor TCompositeImage.Destroy;
begin
  Close;
  FreeAndNil(FImages);
  inherited;
end;

procedure TCompositeImage.Close;
var
  I: Integer;
begin
  for I := 0 to Images.Count - 1 do
    if OwnsFirstImage or (I <> 0) then
    begin
      Images[I].Free;
      Images[I] := nil;
    end;

  Images.Count := 0;
end;

function TCompositeImage.GetImages(const Index: Integer): TEncodedImage;
begin
  Result := FImages[Index];
end;

function TCompositeImage.CubeMapImage(const Side: TCubeMapSide;
  const Level: Cardinal): TEncodedImage;
var
  Index: Integer;
begin
  Index := Ord(DDSFromCubeMapSide[Side]);
  if Mipmaps then
    Result := FImages[Cardinal(Index) * FMipmapsCount + Level] else
    Result := FImages[Index];
end;

procedure TCompositeImage.LoadFromStream(Stream: TStream; const Url: String;
  MimeType: string  = '';
  const Options: TLoadImageOptions = []);
var
  Handler: TCompositeFormatHandler;
begin
  Close;

  if liFlipVertically in Options then
    WritelnWarning('ImageTexture.flipVertically for DDS/KTX not implemented yet, the image will be inverted');

  if MimeType = '' then
    MimeType := UriMimeType(Url);

  if MimeType = 'image/x-dds' then
    Handler := TDDSHandler.Create(Self)
  else
  if MimeType = 'image/ktx' then
    Handler := TKTXHandler.Create(Self)
  else
    raise EInvalidCompositeImage.CreateFmt('Cannot recognize composite image MIME type: %s',
      [MimeType]);

  try
    Handler.LoadFromStream(Stream, Url);
  finally FreeAndNil(Handler) end;
end;

procedure TCompositeImage.LoadFromFile(Url: String;
  const Options: TLoadImageOptions = []);
var
  S: TStream;
begin
  Url := ProcessImageUrl(Url);

  S := Download(Url, [soForceMemoryStream]);
  try
    LoadFromStream(S, Url, '', Options);
  finally FreeAndNil(S) end;
end;

procedure TCompositeImage.SaveToStream(Stream: TStream; const MimeType: string);
var
  Handler: TCompositeFormatHandler;
begin
  Assert(Images.Count > 0, 'Images count must be > 0 when saving a composite image');

  if MimeType = 'image/x-dds' then
    Handler := TDDSHandler.Create(Self)
  else
  if MimeType = 'image/ktx' then
    raise EInvalidCompositeImage.Create('Cannot save to the KTX format now')
  else
    raise EInvalidCompositeImage.CreateFmt('Cannot recognize composite image MIME type: %s',
      [MimeType]);

  try
    Handler.SaveToStream(Stream);
  finally FreeAndNil(Handler) end;
end;

procedure TCompositeImage.SaveToFile(const Url: String);
var
  S: TStream;
begin
  S := UrlSaveStream(Url);
  try
    SaveToStream(S, UriMimeType(Url));
  finally FreeAndNil(S) end;
end;

class function TCompositeImage.MatchesUrl(Url: String): boolean;
begin
  Url := ProcessImageUrl(Url);

  Result :=
    (UriMimeType(Url) = 'image/x-dds') or
    (UriMimeType(Url) = 'image/ktx');
end;

procedure TCompositeImage.Flatten3d;
var
  NewImages: TEncodedImageList;
  OldImage, NewImage: TCastleImage;
  I, J: Integer;
begin
  if (CompositeType = ctVolume) and (Depth > 1) then
  begin
    NewImages := TEncodedImageList.Create(false);

    for I := 0 to Images.Count - 1 do
    begin
      if not (Images[I] is TCastleImage) then
        raise Exception.CreateFmt('Cannot do Flatten3d on this image class: %s',
          [Images[I].ClassName]);
      OldImage := TCastleImage(Images[I]);

      for J := 0 to OldImage.Depth - 1 do
      begin
        NewImage := TCastleImageClass(OldImage.ClassType).Create(
          OldImage.Width, OldImage.Height, 1);
        Move(OldImage.PixelPtr(0, 0, J)^, NewImage.RawPixels^,
          OldImage.Width * OldImage.Height * OldImage.PixelSize);
        NewImages.Add(NewImage);
      end;
    end;

    Close;
    FreeAndNil(FImages);
    FImages := NewImages;
  end;
end;

procedure TCompositeImage.DecompressTexture;
var
  OldImage: TGPUCompressedImage;
  I: Integer;
begin
  for I := 0 to Images.Count - 1 do
    if Images[I] is TGPUCompressedImage then
    begin
      OldImage := TGPUCompressedImage(Images[I]);
      Images[I] := OldImage.Decompress;
      FreeAndNil(OldImage);
    end;
end;

function TCompositeImage.GetCubeMapSides: TCubeMapSides;
var
  DDSSide: TDDSCubeMapSide;
begin
  Result := [];
  for DDSSide in FCubeMapSides do
    Include(Result, DDSToCubeMapSide[DDSSide]);
end;

procedure TCompositeImage.SetCubeMapSides(const Value: TCubeMapSides);
var
  Side: TCubeMapSide;
begin
  FCubeMapSides := [];
  for Side in Value do
    Include(FCubeMapSides, DDSFromCubeMapSide[Side]);
end;

procedure TCompositeImage.AddCubeMapImages(const AImages: TCubeMapImages);
var
  StartIndex: Integer;
begin
  StartIndex := Images.Count;
  Images.Count := StartIndex + 6;
  { note that we use DDSFromCubeMapSide on the left side,
    so that Images order is like in TDDSCubeMapSide enum. }
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveX])] := AImages[csPositiveX];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeX])] := AImages[csNegativeX];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveY])] := AImages[csPositiveY];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeY])] := AImages[csNegativeY];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveZ])] := AImages[csPositiveZ];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeZ])] := AImages[csNegativeZ];
end;

end.
