{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base type for all node types that specify 3D sources for texture images. }
  TAbstractTexture3DNode = class(TAbstractSingleTextureNode)
  private
    UsedCache: TTexturesVideosCache;
    FTextureLoaded: boolean;
    FTextureImage: TEncodedImage;
    FTextureComposite: TCompositeImage;
    procedure SetTextureLoaded(const Value: boolean);
  strict protected
    { This does the actual loading of texture data.

      You can assume here that texture is not loaded now,
      UsedCache, FTextureImage, FTextureComposite are all nil.
      Try to load the texture data, making WritelnWarning as appropriate,
      and set all UsedCache, FTextureImage, FTextureComposite, FAlphaChannelData. }
    procedure LoadTextureData; virtual; abstract;
  public
    destructor Destroy; override;

    { Load and unload 3D texture.

      When loading, we will try to set TextureImage and TextureComposite.
      If loading is successful, TextureImage will be always set to non-nil
      (TextureComposite will be set to non-nil only if image came from some
      composite (DDS, KTX...) image).
      If loading failed, both TextureImage and TextureComposite will be @nil.

      Note that even when loading failed (TextureImage remained @nil),
      TextureLoaded value still changes to @true. This is good --- it makes
      assignment @code(TextureLoaded := true) always work, and (more important)
      it prevents trying to load (and displaying uselessly the same
      WritelnWarning) again at successful @code(TextureLoaded := true).

      In case of problems (for example when some URL cannot be loaded),
      we will make WritelnWarning. }
    property TextureLoaded: boolean
      read FTextureLoaded write SetTextureLoaded;

    property TextureImage: TEncodedImage read FTextureImage;
    property TextureComposite: TCompositeImage read FTextureComposite;

  {$I auto_generated_node_helpers/x3dnodes_x3dtexture3dnode.inc}
  end;

  { 3D texture defined as a collection of 2D texture sources at various depths. }
  TComposedTexture3DNode = class(TAbstractTexture3DNode)
  strict private
    FAlphaChannelData: TAlphaChannel;
  strict protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure LoadTextureData; override;

  {$I auto_generated_node_helpers/x3dnodes_composedtexture3d.inc}
  end;

  { 3D texture loaded from a single file like DDS. }
  TImageTexture3DNode = class(TAbstractTexture3DNode)
  strict private
    FAlphaChannelData: TAlphaChannel;
  strict protected
    function AlphaChannelData: TAlphaChannel; override;
    procedure LoadTextureData; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

  {$I auto_generated_node_helpers/x3dnodes_imagetexture3d.inc}
  end;

  { 3D texture defined as an explicit array of pixel values inside the X3D file. }
  TPixelTexture3DNode = class(TAbstractTexture3DNode)
  strict private
    FAlphaChannelData: TAlphaChannel;
  strict protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure LoadTextureData; override;

  {$I auto_generated_node_helpers/x3dnodes_pixeltexture3d.inc}
  end;

  { 3D texture coordinates, partcularly useful with 3D textures. }
  TTextureCoordinate3DNode = class(TAbstractSingleTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_texturecoordinate3d.inc}
  end;

  { 4D (homogeneous 3D) texture coordinates, partcularly useful with 3D textures. }
  TTextureCoordinate4DNode = class(TAbstractSingleTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_texturecoordinate4d.inc}
  end;

  { Transformation of texture coordinates by a 4x4 matrix,
    particularly useful with 3D textures. }
  TTextureTransformMatrix3DNode = class(TAbstractSingleTextureTransformNode)
  public
    function TransformMatrix: TMatrix4; override;

  {$I auto_generated_node_helpers/x3dnodes_texturetransformmatrix3d.inc}
  end;

  { Transformation of texture coordinates in 3D,
    partcularly useful with 3D textures. }
  TTextureTransform3DNode = class(TAbstractSingleTextureTransformNode)
  public
    function TransformMatrix: TMatrix4; override;

  {$I auto_generated_node_helpers/x3dnodes_texturetransform3d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractTexture3DNode ----------------------------------------------------- }

destructor TAbstractTexture3DNode.Destroy;
begin
  TextureLoaded := false;
  inherited;
end;

procedure TAbstractTexture3DNode.SetTextureLoaded(const Value: boolean);
begin
  if Value <> TextureLoaded then
  begin
    if Value then
      LoadTextureData else
    begin
      if UsedCache <> nil then
        UsedCache.TextureImage_DecReference(FTextureImage, FTextureComposite) else
      begin
        FreeAndNil(FTextureImage);
        FreeAndNil(FTextureComposite);
      end;
      UsedCache := nil;
    end;

    FTextureLoaded := Value;
  end;
end;

{ TComposedTexture3DNode ----------------------------------------------------- }

{ TODO: TComposedTexture3DNode cannnot correctly update on changes
  inside nodes in "texture", just like ComposedCubeMapTexture. }

procedure TComposedTexture3DNode.LoadTextureData;

  { Load one slice (child texture). Returns @nil and makes WritelnWarning if failed. }
  function LoadOneSlice(const Index: Integer): TCastleImage;
  var
    Tex: TAbstractTexture2DNode;
  begin
    if not ((FdTexture[Index] <> nil) and
            (FdTexture[Index] is TAbstractTexture2DNode)) then
    begin
      WritelnWarning('X3D', Format('ComposedTexture3D slice %d is not a valid 2D texture node',
        [Index]));
      Exit(nil);
    end;

    Tex := TAbstractTexture2DNode(FdTexture[Index]);
    if not Tex.IsTextureImage then
    begin
      Exit(nil); { Tex.IsTextureImage already did proper WritelnWarning }
    end;

    if not (Tex.TextureImage is TCastleImage) then
    begin
      WritelnWarning('X3D', Format('ComposedTexture3D slice %d is a GPU-compressed texture, not allowed (we need to be able to glue 2D textures for 3D texture)',
        [Index]));
      Exit(nil);
    end;

    Result := TCastleImage(Tex.TextureImage);

    { If any slice has full-range alpha, then assume whole texture does. }
    AlphaMaxVar(FAlphaChannelData, Tex.AlphaChannelFinal);
  end;

var
  I: Integer;
  Slice: TCastleImage;
begin
  if FdTexture.Count = 0 then
  begin
    WritelnWarning('X3D', 'ComposedTexture3D has no textures');
    Exit;
  end;

  FAlphaChannelData := acNone;

  { First Slice is treated specially, as it determines width/height/class
    of the resulting image. }

  Slice := LoadOneSlice(0);
  if Slice = nil then Exit;

  FTextureImage := TCastleImageClass(Slice.ClassType).Create(
    Slice.Width, Slice.Height, FdTexture.Count);

  try
    Move(Slice.RawPixels^, TCastleImage(FTextureImage).RawPixels^,
      Slice.Width * Slice.Height * Slice.PixelSize);

    for I := 1 to FdTexture.Count - 1 do
    begin
      Slice := LoadOneSlice(I);
      if Slice = nil then
      begin
        FreeAndNil(FTextureImage);
        Exit;
      end;

      if (Slice.Width <> FTextureImage.Width) or
         (Slice.Height <> FTextureImage.Height) or
         (Slice.ClassType <> FTextureImage.ClassType) then
      begin
        WritelnWarning('X3D', Format('ComposedTexture3D slice %d doesn''t have matching size/type (%d x %d of %s), all slices must have the same size/type (first slice had %d x %d of %s)',
          [I, Slice        .Width, Slice        .Height, Slice        .ClassName,
              FTextureImage.Width, FTextureImage.Height, FTextureImage.ClassName]));
        FreeAndNil(FTextureImage);
        Exit;
      end;

      Move(Slice.RawPixels^, TCastleImage(FTextureImage).PixelPtr(0, 0, I)^,
        Slice.Width * Slice.Height * Slice.PixelSize);
    end;
  except
    FreeAndNil(FTextureImage);
    raise;
  end;
end;

function TComposedTexture3DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

{ TImageTexture3DNode -------------------------------------------------------- }

constructor TImageTexture3DNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
end;

procedure TImageTexture3DNode.LoadTextureData;
var
  I: Integer;
  FullUrl: string;
begin
  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromBaseUrl(FdUrl.Items[I]);

    try
      FTextureImage := X3DCache.TextureImage_IncReference(
        FullUrl, FTextureComposite, FAlphaChannelData);
    except
      on E: Exception do
      begin
        FTextureComposite := nil;
        WritelnWarning('X3D', Format('Error when loading ImageTexture3D file "%s": %s', [FullUrl, E.Message]));
        Continue;
      end;
    end;

    if FTextureComposite = nil then
    begin
      X3DCache.TextureImage_DecReference(FTextureImage, FTextureComposite);
      WritelnWarning('X3D', Format('Only composite formats (DDS, KTX...) are supported for ImageTexture3D node, but URL is "%s"', [FullUrl]));
      Continue;
    end;

    if FTextureComposite.CompositeType <> ctVolume then
    begin
      X3DCache.TextureImage_DecReference(FTextureImage, FTextureComposite);
      WritelnWarning('X3D', Format('Composite image "%s" given for ImageTexture3D doesn''t describe a volume (3d) texture', [FullUrl]));
      Continue;
    end;

    { Success }
    UsedCache := X3DCache;
    Exit;
  end;
end;

function TImageTexture3DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

{ TPixelTexture3DNode -------------------------------------------------------- }

procedure TPixelTexture3DNode.LoadTextureData;
var
  TexComponents, Width, Height, Depth: Cardinal;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBA: PVector4Byte;
  I: Integer;
begin
  if FdImage.Count < 4 then
  begin
    WritelnWarning('X3D', 'PixelTexture3D.image has too few items (must have at least 4)');
    Exit;
  end;

  TexComponents := FdImage.Items.Items[0];
  Width := FdImage.Items.Items[1];
  Height := FdImage.Items.Items[2];
  Depth := FdImage.Items.Items[3];

  if Cardinal(FdImage.Count) < 4 + Width * Height * Depth then
  begin
    WritelnWarning('X3D', Format('PixelTexture3D.image has too few items, must have at least %d (= 4 + Width * Height * Depth = 4 + %d * %d * %d)',
      [ 4 + Width * Height * Depth,  Width, Height, Depth ]));
    Exit;
  end;

  case TexComponents of
    0: { 0 components is allowed by the spec, and results in an empty image };
    1: begin
         FTextureImage := TGrayscaleImage.Create(Width, Height, Depth);
         G := TGrayscaleImage(FTextureImage).Pixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeX3DColor(FdImage.Items.Items[4 + I], G^);
           Inc(G);
         end;
       end;
    2: begin
         FTextureImage := TGrayscaleAlphaImage.Create(Width, Height, Depth);
         GA := TGrayscaleAlphaImage(FTextureImage).Pixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeX3DColor(FdImage.Items.Items[4 + I], GA^);
           Inc(GA);
         end;
       end;
    3: begin
         FTextureImage := TRGBImage.Create(Width, Height, Depth);
         RGB := TRGBImage(FTextureImage).Pixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeX3DColor(FdImage.Items.Items[4 + I], RGB^);
           Inc(RGB);
         end;
       end;
    4: begin
         FTextureImage := TRGBAlphaImage.Create(Width, Height, Depth);
         RGBA := TRGBAlphaImage(FTextureImage).Pixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeX3DColor(FdImage.Items.Items[4 + I], RGBA^);
           Inc(RGBA);
         end;
       end;
    else
    begin
      WritelnWarning('X3D', 'PixelTexture3D.image has invalid components count (must be between 0..4)');
      Exit;
    end;
  end;

  FAlphaChannelData := FTextureImage.AlphaChannel;
end;

function TPixelTexture3DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

{ TTextureTransformMatrix3DNode ---------------------------------------------- }

function TTextureTransformMatrix3DNode.TransformMatrix: TMatrix4;
begin
  Result := FdMatrix.Value;
end;

{ TTextureTransform3DNode ---------------------------------------------------- }

function TTextureTransform3DNode.TransformMatrix: TMatrix4;
begin
  Result :=
    TranslationMatrix(FdTranslation.Value + FdCenter.Value) *
    RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis) *
    ScalingMatrix(FdScale.Value) *
    TranslationMatrix(-FdCenter.Value);
end;

procedure RegisterTexturing3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TComposedTexture3DNode,
    TImageTexture3DNode,
    TPixelTexture3DNode,
    TTextureCoordinate3DNode,
    TTextureCoordinate4DNode,
    TTextureTransformMatrix3DNode,
    TTextureTransform3DNode
  ]);
end;

{$endif read_implementation}
