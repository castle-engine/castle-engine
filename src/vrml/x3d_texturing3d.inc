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

{$ifdef read_interface}
  { }
  TNodeX3DTexture3DNode = class(TNodeX3DTextureNode)
  private
    UsedCache: TTexturesImagesVideosCache;

    FTextureLoaded: boolean;
    procedure SetTextureLoaded(const Value: boolean);
  private
    FTextureImage: TEncodedImage;
    FTextureDDS: TDDSImage;
  protected
    { This does the actual loading of texture data.

      You can assume here that texture is not loaded now,
      UsedCache, FTextureImage, FTextureDDS are all nil.
      Try to load the texture data, making VRMLWarning as appropriate,
      and set all UsedCache, FTextureImage, FTextureDDS. }
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

      When loading, we will try to set TextureImage and TextureDDS.
      If loading is successfull, TextureImage will be always set to non-nil
      (TextureDDS will be set to non-nil only if image came from some DDS image).
      If loading failed, both TextureImage and TextureDDS will be @nil.

      Note that even when loading failed (TextureImage remained @nil),
      TextureLoaded value still changes to @true. This is good --- it makes
      assignment @code(TextureLoaded := true) always work, and (more important)
      it prevents trying to load (and displaying uselessly the same
      VRMLWarning) again at successful @code(TextureLoaded := true).

      In case of problems (for example when some URL cannot be loaded),
      we will make VRMLWarning. }
    property TextureLoaded: boolean
      read FTextureLoaded write SetTextureLoaded;

    property TextureImage: TEncodedImage read FTextureImage;
    property TextureDDS: TDDSImage read FTextureDDS;
  end;

  TNodeComposedTexture3D = class(TNodeX3DTexture3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTexture: TMFNode;
    public property FdTexture: TMFNode read FFdTexture;

    procedure LoadTextureData; override;
  end;

  TNodeImageTexture3D = class(TNodeX3DTexture3DNode, INodeX3DUrlObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    procedure LoadTextureData; override;
  end;

  TNodePixelTexture3D = class(TNodeX3DTexture3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdImage: TMFInt32;
    public property FdImage: TMFInt32 read FFdImage;

    procedure LoadTextureData; override;
  end;

  TNodeTextureCoordinate3D = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;
  end;

  TNodeTextureCoordinate4D = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec4f;
    public property FdPoint: TMFVec4f read FFdPoint;
  end;

  TNodeTextureTransformMatrix3D = class(TNodeX3DTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdMatrix: TSFMatrix4f;
    public property FdMatrix: TSFMatrix4f read FFdMatrix;

    function TransformMatrix: TMatrix4Single; override;
  end;

  TNodeTextureTransform3D = class(TNodeX3DTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DTexture3DNode.CreateNode;
begin
  inherited;

  FFdrepeatS := TSFBool.Create(Self, 'repeatS', false);
   FdrepeatS.Exposed := false;
   FdrepeatS.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create(Self, 'repeatT', false);
   FdrepeatT.Exposed := false;
   FdrepeatT.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatT);

  FFdrepeatR := TSFBool.Create(Self, 'repeatR', false);
   FdrepeatR.Exposed := false;
   FdrepeatR.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatR);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdtextureProperties.Exposed := false;
   FdtextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdtextureProperties);
end;

destructor TNodeX3DTexture3DNode.Destroy;
begin
  TextureLoaded := false;
  inherited;
end;

procedure TNodeX3DTexture3DNode.SetTextureLoaded(const Value: boolean);
begin
  if Value <> TextureLoaded then
  begin
    if Value then
      LoadTextureData else
    begin
      if UsedCache <> nil then
        UsedCache.TextureImage_DecReference(FTextureImage, FTextureDDS) else
      begin
        FreeAndNil(FTextureImage);
        FreeAndNil(FTextureDDS);
      end;
      UsedCache := nil;
    end;

    FTextureLoaded := Value;
  end;
end;

procedure TNodeComposedTexture3D.CreateNode;
begin
  inherited;

  { TODO: just like ComposedCubeMapTexture, we will not correctly
    detect changes inside nodes in "texture". }

  FFdtexture := TMFNode.Create(Self, 'texture', [TNodeX3DTexture2DNode]);
   Fdtexture.ChangesAlways := [chTextureImage];
  Fields.Add(FFdtexture);
end;

class function TNodeComposedTexture3D.ClassNodeTypeName: string;
begin
  Result := 'ComposedTexture3D';
end;

class function TNodeComposedTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeComposedTexture3D.LoadTextureData;

  { Load one slice (child texture). Returns @nil and makes VRMLWarning if failed. }
  function LoadOneSlice(const Index: Integer): TImage;
  var
    Tex: TVRML2DTextureNode;
  begin
    if not ((FdTexture.Items[Index] <> nil) and
            (FdTexture.Items[Index] is TVRML2DTextureNode)) then
    begin
      VRMLWarning(vwIgnorable, Format('ComposedTexture3D slice %d is not a valid 2D texture node',
        [Index]));
      Exit(nil);
    end;

    Tex := TVRML2DTextureNode(FdTexture.Items[Index]);
    if not Tex.IsTextureImage then
    begin
      Exit(nil);{ Tex.IsTextureImage already did proper VRMLWarning }
    end;

    if not (Tex.TextureImage is TImage) then
    begin
      VRMLWarning(vwIgnorable, Format('ComposedTexture3D slice %d is an S3TC compressed texture, not allowed (we need to be able to glue 2D textures for 3D texture)',
        [Index]));
      Exit(nil);
    end;

    Result := TImage(Tex.TextureImage);
  end;

var
  I: Integer;
  Slice: TImage;
begin
  if FdTexture.Items.Count = 0 then
  begin
    VRMLWarning(vwIgnorable, 'ComposedTexture3D has no textures');
    Exit;
  end;

  { First Slice is treated specially, as it determines width/height/class
    of the resulting image. }

  Slice := LoadOneSlice(0);
  if Slice = nil then Exit;

  FTextureImage := TImageClass(Slice.ClassType).Create(
    Slice.Width, Slice.Height, FdTexture.Count);

  try
    Move(Slice.RawPixels^, TImage(FTextureImage).RawPixels^,
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
        VRMLWarning(vwIgnorable, Format('ComposedTexture3D slice %d doesn''t have matching size/type (%d x %d of %s), all slices must have the same size/type (first slice had %d x %d of %s)',
          [I, Slice        .Width, Slice        .Height, Slice        .ClassName,
              FTextureImage.Width, FTextureImage.Height, FTextureImage.ClassName]));
        FreeAndNil(FTextureImage);
        Exit;
      end;

      Move(Slice.RawPixels^, TImage(FTextureImage).PixelPtr(0, 0, I)^,
        Slice.Width * Slice.Height * Slice.PixelSize);
    end;
  except
    FreeAndNil(FTextureImage);
    raise;
  end;
end;

procedure TNodeImageTexture3D.CreateNode;
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
   Fdurl.ChangesAlways := [chTextureImage];
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeImageTexture3D.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture3D';
end;

class function TNodeImageTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeImageTexture3D.LoadTextureData;
var
  I: Integer;
  FullUrl: string;
begin
  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);

    try
      FTextureImage := VRMLCache.TextureImage_IncReference(FullUrl, FTextureDDS);
    except
      on E: Exception do
      begin
        FTextureDDS := nil;
        VRMLWarning(vwIgnorable, Format('Error when loading ImageTexture3D file "%s": %s', [FullUrl, E.Message]));
        Continue;
      end;
    end;

    if FTextureDDS = nil then
    begin
      VRMLCache.TextureImage_DecReference(FTextureImage, FTextureDDS);
      VRMLWarning(vwIgnorable, Format('Only DDS format is supported for ImageTexture3D node, but URL is "%s"', [FullUrl]));
      Continue;
    end;

    if FTextureDDS.DDSType <> dtVolume then
    begin
      VRMLCache.TextureImage_DecReference(FTextureImage, FTextureDDS);
      VRMLWarning(vwIgnorable, Format('DDS image "%s" given for ImageTexture3D doesn''t describe a volume (3d) texture', [FullUrl]));
      Continue;
    end;

    { Success }
    UsedCache := VRMLCache;
    Exit;
  end;
end;

procedure TNodePixelTexture3D.CreateNode;
begin
  inherited;

  FFdimage := TMFInt32.Create(Self, 'image', [0, 0, 0, 0]);
   Fdimage.ChangesAlways := [chTextureImage];
  Fields.Add(FFdimage);
end;

class function TNodePixelTexture3D.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture3D';
end;

class function TNodePixelTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePixelTexture3D.LoadTextureData;
var
  Components, Width, Height, Depth: Cardinal;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBA: PVector4Byte;
  I: Integer;
begin
  if FdImage.Count < 4 then
  begin
    VRMLWarning(vwIgnorable, 'PixelTexture3D.image has too few items (must have at least 4)');
    Exit;
  end;

  Components := FdImage.Items.Items[0];
  Width := FdImage.Items.Items[1];
  Height := FdImage.Items.Items[2];
  Depth := FdImage.Items.Items[3];

  if Cardinal(FdImage.Count) < 4 + Width * Height * Depth then
  begin
    VRMLWarning(vwIgnorable, Format('PixelTexture3D.image has too few items, must have at least %d (= 4 + Width * Height * Depth = 4 + %d * %d * %d)',
      [ 4 + Width * Height * Depth,  Width, Height, Depth ]));
    Exit;
  end;

  case Components of
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
      VRMLWarning(vwIgnorable, 'PixelTexture3D.image has invalid components count (must be between 0..4)');
      Exit;
    end;
  end;
end;

procedure TNodeTextureCoordinate3D.CreateNode;
begin
  inherited;

  FFdpoint := TMFVec3f.Create(Self, 'point', []);
   Fdpoint.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate3D.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate3D';
end;

class function TNodeTextureCoordinate3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTextureCoordinate4D.CreateNode;
begin
  inherited;

  FFdpoint := TMFVec4f.Create(Self, 'point', []);
   Fdpoint.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate4D.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate4D';
end;

class function TNodeTextureCoordinate4D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTextureTransformMatrix3D.CreateNode;
begin
  inherited;

  FFdmatrix := TSFMatrix4f.Create(Self, 'matrix', IdentityMatrix4Single);
   Fdmatrix.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdmatrix);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransformMatrix3D.ClassNodeTypeName: string;
begin
  Result := 'TextureTransformMatrix3D';
end;

class function TNodeTextureTransformMatrix3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeTextureTransformMatrix3D.TransformMatrix: TMatrix4Single;
begin
  Result := FdMatrix.Value;
end;

procedure TNodeTextureTransform3D.CreateNode;
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   Fdcenter.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   Fdrotation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   Fdscale.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   Fdtranslation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransform3D.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform3D';
end;

class function TNodeTextureTransform3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeTextureTransform3D.TransformMatrix: TMatrix4Single;
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
    TNodeComposedTexture3D,
    TNodeImageTexture3D,
    TNodePixelTexture3D,
    TNodeTextureCoordinate3D,
    TNodeTextureCoordinate4D,
    TNodeTextureTransformMatrix3D,
    TNodeTextureTransform3D
  ]);
end;

{$endif read_implementation}
