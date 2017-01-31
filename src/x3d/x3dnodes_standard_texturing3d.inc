{
  Copyright 2008-2017 Michalis Kamburelis.

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
  TAbstractTexture3DNode = class(TAbstractTextureNode)
  private
    UsedCache: TTexturesVideosCache;
    FTextureLoaded: boolean;
    FTextureImage: TEncodedImage;
    FTextureComposite: TCompositeImage;
    procedure SetTextureLoaded(const Value: boolean);
  protected
    { This does the actual loading of texture data.

      You can assume here that texture is not loaded now,
      UsedCache, FTextureImage, FTextureComposite are all nil.
      Try to load the texture data, making WritelnWarning as appropriate,
      and set all UsedCache, FTextureImage, FTextureComposite, FAlphaChannelData. }
    procedure LoadTextureData; virtual; abstract;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    private FFdRepeatS: TSFBool;
    public property FdRepeatS: TSFBool read FFdRepeatS;

    private FFdRepeatT: TSFBool;
    public property FdRepeatT: TSFBool read FFdRepeatT;

    private FFdRepeatR: TSFBool;
    public property FdRepeatR: TSFBool read FFdRepeatR;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    { Load and unload 3D texture.

      When loading, we will try to set TextureImage and TextureComposite.
      If loading is successfull, TextureImage will be always set to non-nil
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
  private
    FAlphaChannelData: TAlphaChannel;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTexture: TMFNode;
    public property FdTexture: TMFNode read FFdTexture;

    procedure LoadTextureData; override;

    {$I auto_generated_node_helpers/x3dnodes_composedtexture3d.inc}
  end;

  { 3D texture loaded from a single file like DDS. }
  TImageTexture3DNode = class(TAbstractTexture3DNode, IAbstractUrlObject)
  private
    FAlphaChannelData: TAlphaChannel;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    procedure LoadTextureData; override;

    {$I auto_generated_node_helpers/x3dnodes_imagetexture3d.inc}
  end;

  { 3D texture defined as an explicit array of pixel values inside the X3D file. }
  TPixelTexture3DNode = class(TAbstractTexture3DNode)
  private
    FAlphaChannelData: TAlphaChannel;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdImage: TMFInt32;
    public property FdImage: TMFInt32 read FFdImage;

    procedure LoadTextureData; override;

    {$I auto_generated_node_helpers/x3dnodes_pixeltexture3d.inc}
  end;

  { 3D texture coordinates, partcularly useful with 3D textures. }
  TTextureCoordinate3DNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;

    {$I auto_generated_node_helpers/x3dnodes_texturecoordinate3d.inc}
  end;

  { 4D (homogeneous 3D) texture coordinates, partcularly useful with 3D textures. }
  TTextureCoordinate4DNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec4f;
    public property FdPoint: TMFVec4f read FFdPoint;

    {$I auto_generated_node_helpers/x3dnodes_texturecoordinate4d.inc}
  end;

  { Transformation of texture coordinates by a 4x4 matrix,
    particularly useful with 3D textures. }
  TTextureTransformMatrix3DNode = class(TAbstractTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdMatrix: TSFMatrix4f;
    public property FdMatrix: TSFMatrix4f read FFdMatrix;

    function TransformMatrix: TMatrix4Single; override;

    {$I auto_generated_node_helpers/x3dnodes_texturetransformmatrix3d.inc}
  end;

  { Transformation of texture coordinates in 3D,
    partcularly useful with 3D textures. }
  TTextureTransform3DNode = class(TAbstractTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    function TransformMatrix: TMatrix4Single; override;

    {$I auto_generated_node_helpers/x3dnodes_texturetransform3d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractTexture3DNode.CreateNode;
begin
  inherited;

  FFdRepeatS := TSFBool.Create(Self, 'repeatS', false);
   FdRepeatS.Exposed := false;
   FdRepeatS.ChangesAlways := [chTextureRendererProperties];
  AddField(FFdRepeatS);

  FFdRepeatT := TSFBool.Create(Self, 'repeatT', false);
   FdRepeatT.Exposed := false;
   FdRepeatT.ChangesAlways := [chTextureRendererProperties];
  AddField(FFdRepeatT);

  FFdRepeatR := TSFBool.Create(Self, 'repeatR', false);
   FdRepeatR.Exposed := false;
   FdRepeatR.ChangesAlways := [chTextureRendererProperties];
  AddField(FFdRepeatR);

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  AddField(FFdTextureProperties);
end;

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

procedure TComposedTexture3DNode.CreateNode;
begin
  inherited;

  { TODO: just like ComposedCubeMapTexture, we will not correctly
    detect changes inside nodes in "texture". }

  FFdTexture := TMFNode.Create(Self, 'texture', [TAbstractX3DTexture2DNode]);
   FdTexture.ChangesAlways := [chTextureImage];
  AddField(FFdTexture);
end;

class function TComposedTexture3DNode.ClassX3DType: string;
begin
  Result := 'ComposedTexture3D';
end;

class function TComposedTexture3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TComposedTexture3DNode.LoadTextureData;

  { Load one slice (child texture). Returns @nil and makes WritelnWarning if failed. }
  function LoadOneSlice(const Index: Integer): TCastleImage;
  var
    Tex: TAbstractTexture2DNode;
  begin
    if not ((FdTexture[Index] <> nil) and
            (FdTexture[Index] is TAbstractTexture2DNode)) then
    begin
      WritelnWarning('VRML/X3D', Format('ComposedTexture3D slice %d is not a valid 2D texture node',
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
      WritelnWarning('VRML/X3D', Format('ComposedTexture3D slice %d is a GPU-compressed texture, not allowed (we need to be able to glue 2D textures for 3D texture)',
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
  if FdTexture.Items.Count = 0 then
  begin
    WritelnWarning('VRML/X3D', 'ComposedTexture3D has no textures');
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
        WritelnWarning('VRML/X3D', Format('ComposedTexture3D slice %d doesn''t have matching size/type (%d x %d of %s), all slices must have the same size/type (first slice had %d x %d of %s)',
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

procedure TImageTexture3DNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.ChangesAlways := [chTextureImage];
  AddField(FFdUrl);
  { X3D specification comment: [URI] }
end;

class function TImageTexture3DNode.ClassX3DType: string;
begin
  Result := 'ImageTexture3D';
end;

class function TImageTexture3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
      FTextureImage := X3DCache.TextureImage_IncReference(FullUrl, FTextureComposite,
        FAlphaChannelData);
    except
      on E: Exception do
      begin
        FTextureComposite := nil;
        WritelnWarning('VRML/X3D', Format('Error when loading ImageTexture3D file "%s": %s', [FullUrl, E.Message]));
        Continue;
      end;
    end;

    if FTextureComposite = nil then
    begin
      X3DCache.TextureImage_DecReference(FTextureImage, FTextureComposite);
      WritelnWarning('VRML/X3D', Format('Only composite formats (DDS, KTX...) are supported for ImageTexture3D node, but URL is "%s"', [FullUrl]));
      Continue;
    end;

    if FTextureComposite.CompositeType <> ctVolume then
    begin
      X3DCache.TextureImage_DecReference(FTextureImage, FTextureComposite);
      WritelnWarning('VRML/X3D', Format('Composite image "%s" given for ImageTexture3D doesn''t describe a volume (3d) texture', [FullUrl]));
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

procedure TPixelTexture3DNode.CreateNode;
begin
  inherited;

  FFdImage := TMFInt32.Create(Self, 'image', [0, 0, 0, 0]);
   FdImage.ChangesAlways := [chTextureImage];
  AddField(FFdImage);
end;

class function TPixelTexture3DNode.ClassX3DType: string;
begin
  Result := 'PixelTexture3D';
end;

class function TPixelTexture3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

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
    WritelnWarning('VRML/X3D', 'PixelTexture3D.image has too few items (must have at least 4)');
    Exit;
  end;

  TexComponents := FdImage.Items.Items[0];
  Width := FdImage.Items.Items[1];
  Height := FdImage.Items.Items[2];
  Depth := FdImage.Items.Items[3];

  if Cardinal(FdImage.Count) < 4 + Width * Height * Depth then
  begin
    WritelnWarning('VRML/X3D', Format('PixelTexture3D.image has too few items, must have at least %d (= 4 + Width * Height * Depth = 4 + %d * %d * %d)',
      [ 4 + Width * Height * Depth,  Width, Height, Depth ]));
    Exit;
  end;

  case TexComponents of
    0: { 0 components is allowed by the spec, and results in an empty image };
    1: begin
         FTextureImage := TGrayscaleImage.Create(Width, Height, Depth);
         G := TGrayscaleImage(FTextureImage).GrayscalePixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeImageColor(FdImage.Items.Items[4 + I], G^);
           Inc(G);
         end;
       end;
    2: begin
         FTextureImage := TGrayscaleAlphaImage.Create(Width, Height, Depth);
         GA := TGrayscaleAlphaImage(FTextureImage).GrayscaleAlphaPixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeImageColor(FdImage.Items.Items[4 + I], GA^);
           Inc(GA);
         end;
       end;
    3: begin
         FTextureImage := TRGBImage.Create(Width, Height, Depth);
         RGB := TRGBImage(FTextureImage).RGBPixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeImageColor(FdImage.Items.Items[4 + I], RGB^);
           Inc(RGB);
         end;
       end;
    4: begin
         FTextureImage := TRGBAlphaImage.Create(Width, Height, Depth);
         RGBA := TRGBAlphaImage(FTextureImage).AlphaPixels;
         for I := 0 to Width * Height * Depth - 1 do
         begin
           DecodeImageColor(FdImage.Items.Items[4 + I], RGBA^);
           Inc(RGBA);
         end;
       end;
    else
    begin
      WritelnWarning('VRML/X3D', 'PixelTexture3D.image has invalid components count (must be between 0..4)');
      Exit;
    end;
  end;

  FAlphaChannelData := FTextureImage.AlphaChannel;
end;

function TPixelTexture3DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

procedure TTextureCoordinate3DNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chTextureCoordinate];
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureCoordinate3DNode.ClassX3DType: string;
begin
  Result := 'TextureCoordinate3D';
end;

class function TTextureCoordinate3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTextureCoordinate4DNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec4f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chTextureCoordinate];
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureCoordinate4DNode.ClassX3DType: string;
begin
  Result := 'TextureCoordinate4D';
end;

class function TTextureCoordinate4DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTextureTransformMatrix3DNode.CreateNode;
begin
  inherited;

  FFdMatrix := TSFMatrix4f.Create(Self, 'matrix', IdentityMatrix4Single);
   FdMatrix.ChangesAlways := [chTextureTransform];
  AddField(FFdMatrix);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureTransformMatrix3DNode.ClassX3DType: string;
begin
  Result := 'TextureTransformMatrix3D';
end;

class function TTextureTransformMatrix3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TTextureTransformMatrix3DNode.TransformMatrix: TMatrix4Single;
begin
  Result := FdMatrix.Value;
end;

procedure TTextureTransform3DNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chTextureTransform];
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTextureTransform];
  AddField(FFdRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTextureTransform];
  AddField(FFdScale);
  { X3D specification comment: (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTextureTransform];
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureTransform3DNode.ClassX3DType: string;
begin
  Result := 'TextureTransform3D';
end;

class function TTextureTransform3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TTextureTransform3DNode.TransformMatrix: TMatrix4Single;
begin
  Result := TranslationMatrix(VectorAdd(FdTranslation.Value, FdCenter.Value));
  Result := MatrixMult(Result,
    RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis));
  Result := MatrixMult(Result,
    ScalingMatrix(FdScale.Value));
  Result := MatrixMult(Result,
    TranslationMatrix(VectorNegate(FdCenter.Value)));
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
