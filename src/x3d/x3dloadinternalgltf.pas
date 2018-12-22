{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D models in the glTF 2.0 format (@link(LoadGLTF)). }
unit X3DLoadInternalGLTF;

interface

uses X3DNodes, X3DFields;

{ Load 3D model in the GLTF format, converting it to an X3D nodes graph.
  This routine is internally used by the @link(Load3D) to load an GLTF file. }
function LoadGLTF(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, TypInfo, Math, PasGLTF,
  CastleClassUtils, CastleDownload, CastleUtils, CastleURIUtils, CastleLog,
  CastleVectors, CastleStringUtils, CastleTextureImages;

type
  { X3D Appearance node extended to carry some additional information specified
    in glTF materials. }
  TGltfAppearanceNode = class(TAppearanceNode)
  public
    AlphaMode: TPasGLTF.TMaterial.TAlphaMode;
    AlphaCutOff: Single;
    DoubleSided: Boolean;
  end;

function LoadGLTF(const URL: string): TX3DRootNode;
var
  Document: TPasGLTF.TDocument;
  // List of TGltfAppearanceNode nodes, ordered just list glTF materials
  Appearances: TX3DNodeList;
  DefaultAppearance: TGltfAppearanceNode;

  procedure ReadHeader;
  begin
    WritelnLogMultiline('glTF', Format(
      'Asset.Copyright: %s' + NL +
      'Asset.Generator: %s' + NL +
      'Asset.MinVersion: %s' + NL +
      'Asset.Version: %s' + NL +
      'Asset.Empty: %s' + NL +
      'Accessors: %d' + NL +
      'Animations: %d' + NL +
      'Buffers: %d' + NL +
      'BufferViews: %d' + NL +
      'Cameras: %d' + NL +
      'Images: %d' + NL +
      'Materials: %d' + NL +
      'Meshes: %d' + NL +
      'Nodes: %d' + NL +
      'Samplers: %d' + NL +
      'Scenes: %d' + NL +
      'Skins: %d' + NL +
      'Textures: %d' + NL +
      'ExtensionsUsed: %s' + NL +
      'ExtensionsRequired: %s' + NL +
      '',
      [Document.Asset.Copyright,
       Document.Asset.Generator,
       Document.Asset.MinVersion,
       Document.Asset.Version,
       BoolToStr(Document.Asset.Empty, true),

       Document.Accessors.Count,
       Document.Animations.Count,
       Document.Buffers.Count,
       Document.BufferViews.Count,
       Document.Cameras.Count,
       Document.Images.Count,
       Document.Materials.Count,
       Document.Meshes.Count,
       Document.Nodes.Count,
       Document.Samplers.Count,
       Document.Scenes.Count,
       Document.Skins.Count,
       Document.Textures.Count,
       Document.ExtensionsUsed.Text,
       Document.ExtensionsRequired.Text
      ])
    );
    if Document.Scene <> -1 then
      WritelnLog('glTF', 'Current scene %d name: "%s"',
        [Document.Scene, Document.Scenes[Document.Scene].Name]);
    if Document.ExtensionsRequired.IndexOf('KHR_draco_mesh_compression') <> -1 then
      WritelnWarning('Required extension KHR_draco_mesh_compression not supported by glTF reader');
  end;

  function Vector3FromGltf(const V: TPasGLTF.TVector3): TVector3;
  begin
    // as it happens, both structures have the same memory layout, so copy by a fast Move
    Assert(SizeOf(V) = SizeOf(Result));
    Move(V, Result, SizeOf(Result));
  end;

  function Vector4FromGltf(const V: TPasGLTF.TVector4): TVector4;
  begin
    // as it happens, both structures have the same memory layout, so copy by a fast Move
    Assert(SizeOf(V) = SizeOf(Result));
    Move(V, Result, SizeOf(Result));
  end;

  function ReadTextureRepeat(const Wrap: TPasGLTF.TSampler.TWrappingMode): Boolean;
  begin
    Result :=
      (Wrap = TPasGLTF.TSampler.TWrappingMode.Repeat_) or
      (Wrap = TPasGLTF.TSampler.TWrappingMode.MirroredRepeat);
    if Wrap = TPasGLTF.TSampler.TWrappingMode.MirroredRepeat then
      WritelnWarning('glTF', 'MirroredRepeat wrap mode not supported, using simple Repeat');
  end;

  function ReadMinificationFilter(const Filter: TPasGLTF.TSampler.TMinFilter): TAutoMinificationFilter;
  begin
    case Filter of
      TPasGLTF.TSampler.TMinFilter.None                : Result := minDefault;
      TPasGLTF.TSampler.TMinFilter.Nearest             : Result := minNearest;
      TPasGLTF.TSampler.TMinFilter.Linear              : Result := minLinear;
      TPasGLTF.TSampler.TMinFilter.NearestMipMapNearest: Result := minNearestMipmapNearest;
      TPasGLTF.TSampler.TMinFilter.LinearMipMapNearest : Result := minLinearMipmapNearest;
      TPasGLTF.TSampler.TMinFilter.NearestMipMapLinear : Result := minNearestMipmapLinear;
      TPasGLTF.TSampler.TMinFilter.LinearMipMapLinear  : Result := minLinearMipmapLinear;
      else raise EInternalError.Create('Unexpected glTF minification filter');
    end;
  end;

  function ReadMagnificationFilter(const Filter: TPasGLTF.TSampler.TMagFilter): TAutoMagnificationFilter;
  begin
    case Filter of
      TPasGLTF.TSampler.TMagFilter.None   : Result := magDefault;
      TPasGLTF.TSampler.TMagFilter.Nearest: Result := magNearest;
      TPasGLTF.TSampler.TMagFilter.Linear : Result := magLinear;
      else raise EInternalError.Create('Unexpected glTF magnification filter');
    end;
  end;

  function ReadAppearance(const Material: TPasGLTF.TMaterial): TGltfAppearanceNode;
  var
    BaseColorTexture: TImageTextureNode;
    CommonSurfaceShader: TCommonSurfaceShaderNode;
    GltfBaseColorTexture: TPasGLTF.TMaterial.TTexture;
    GltfTexture: TPasGLTF.TTexture;
    GltfImage: TPasGLTF.TImage;
    GltfSampler: TPasGLTF.TSampler;
    TextureProperties: TTexturePropertiesNode;
    BaseColorFactor: TVector4;
  begin
    Result := TGltfAppearanceNode.Create(Material.Name);

    BaseColorFactor := Vector4FromGltf(Material.PBRMetallicRoughness.BaseColorFactor);
    CommonSurfaceShader := TCommonSurfaceShaderNode.Create;
    CommonSurfaceShader.DiffuseFactor := BaseColorFactor.XYZ;
    CommonSurfaceShader.AlphaFactor := BaseColorFactor.W;
    Result.SetShaders([CommonSurfaceShader]);

    Result.AlphaMode := Material.AlphaMode;
    Result.AlphaCutOff := Material.AlphaCutOff;
    Result.DoubleSided := Material.DoubleSided;

    GltfBaseColorTexture := Material.PBRMetallicRoughness.BaseColorTexture;
    if not GltfBaseColorTexture.Empty then
    begin
      BaseColorTexture := TImageTextureNode.Create('', URL);
      CommonSurfaceShader.MultiDiffuseAlphaTexture := BaseColorTexture;
      CommonSurfaceShader.DiffuseTextureCoordinatesId := GltfBaseColorTexture.TexCoord;

      if GltfBaseColorTexture.Index < Document.Textures.Count then
      begin
        GltfTexture := Document.Textures[GltfBaseColorTexture.Index];

        if Between(GltfTexture.Source, 0, Document.Images.Count - 1) then
        begin
          GltfImage := Document.Images[GltfTexture.Source];
          if GltfImage.URI <> '' then
            BaseColorTexture.SetUrl([GltfImage.URI]);
        end;

        if Between(GltfTexture.Sampler, 0, Document.Samplers.Count - 1) then
        begin
          GltfSampler := Document.Samplers[GltfTexture.Sampler];

          BaseColorTexture.RepeatS := ReadTextureRepeat(GltfSampler.WrapS);
          BaseColorTexture.RepeatT := ReadTextureRepeat(GltfSampler.WrapT);

          if (GltfSampler.MinFilter <> TPasGLTF.TSampler.TMinFilter.None) or
             (GltfSampler.MagFilter <> TPasGLTF.TSampler.TMagFilter.None) then
          begin
            TextureProperties := TTexturePropertiesNode.Create;
            TextureProperties.MinificationFilter := ReadMinificationFilter(GltfSampler.MinFilter);
            TextureProperties.MagnificationFilter := ReadMagnificationFilter(GltfSampler.MagFilter);
          end;
        end;
      end;
    end;
  end;

  function AccessorTypeToStr(const AccessorType: TPasGLTF.TAccessor.TType): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TAccessor.TType), Ord(AccessorType));
  end;

  function PrimitiveModeToStr(const Mode: TPasGLTF.TMesh.TPrimitive.TMode): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TMesh.TPrimitive.TMode), Ord(Mode));
  end;

  function GetAccessor(const AccessorIndex: Integer): TPasGLTF.TAccessor;
  begin
    if AccessorIndex < Document.Accessors.Count then
      Result := Document.Accessors[AccessorIndex]
    else
    begin
      Result := nil;
      WritelnWarning('glTF', 'Missing glTF accessor (index %d, but we only have %d accessors)',
        [AccessorIndex, Document.Accessors.Count]);
    end;
  end;

  { The argument ForVertex addresses this statement of the glTF spec:
    """
    For performance and compatibility reasons, each element of
    a vertex attribute must be aligned to 4-byte boundaries
    inside bufferView
    """ }

  procedure AccessorToInt32(const AccessorIndex: Integer; const Field: TMFLong; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTFInt32DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsInt32Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(LongInt) * Len);
    end;
  end;

  procedure AccessorToVector2(const AccessorIndex: Integer; const Field: TMFVec2f; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector2DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector2Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(TVector2) * Len);
    end;
  end;

  procedure AccessorToVector3(const AccessorIndex: Integer; const Field: TMFVec3f; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector3DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector3Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(TVector3) * Len);
    end;
  end;

  procedure AccessorToVector4(const AccessorIndex: Integer; const Field: TMFVec4f; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector4DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector4Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(TVector4) * Len);
    end;
  end;

  { Set SingleTexCoord as a texture coordinate.
    Sets up TexCoordField as a TMultiTextureCoordinateNode instance,
    in case we have multiple texture coordinates. }
  procedure SetMultiTextureCoordinate(const TexCoordField: TSFNode;
    const SingleTexCoord: TTextureCoordinateNode;
    const SingleTexCoordIndex: Integer);
  var
    MultiTexCoord: TMultiTextureCoordinateNode;
  begin
    if TexCoordField.Value <> nil then
      { only this procedure modifies this field,
        so it has to be TMultiTextureCoordinateNode if assigned. }
      MultiTexCoord := TexCoordField.Value as TMultiTextureCoordinateNode
    else
    begin
      MultiTexCoord := TMultiTextureCoordinateNode.Create;
      TexCoordField.Value := MultiTexCoord;
    end;

    MultiTexCoord.FdTexCoord.Count := Max(MultiTexCoord.FdTexCoord.Count, SingleTexCoordIndex + 1);
    MultiTexCoord.FdTexCoord.Items[SingleTexCoordIndex] := SingleTexCoord;
  end;

  procedure ReadPrimitive(const Primitive: TPasGLTF.TMesh.TPrimitive;
    const ParentGroup: TGroupNode);
  var
    AttributeName: TPasGLTFUTF8String;
    Shape: TShapeNode;
    Geometry: TAbstractGeometryNode;
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    Normal: TNormalNode;
    Color: TColorNode;
    ColorRGBA: TColorRGBANode;
    ColorAccessor: TPasGLTF.TAccessor;
    IndexField: TMFLong;
    TexCoordIndex: LongInt;
    Appearance: TGltfAppearanceNode;
  begin
    // create X3D geometry and shape nodes
    if Primitive.Indices <> -1 then
    begin
      case Primitive.Mode of
        TPasGLTF.TMesh.TPrimitive.TMode.Lines        : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        // TODO: these will require unpacking and expressing as TIndexedLineSetNode
        //TPasGLTF.TMesh.TPrimitive.TMode.LineLoop     : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        //TPasGLTF.TMesh.TPrimitive.TMode.LineStrip    : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles    : Geometry := TIndexedTriangleSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip: Geometry := TIndexedTriangleStripSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan  : Geometry := TIndexedTriangleFanSetNode.CreateWithShape(Shape);
        else
          begin
            WritelnWarning('glTF', 'Primitive mode not implemented (in indexed mode): ' + PrimitiveModeToStr(Primitive.Mode));
            Exit;
          end;
      end;
    end else
    begin
      case Primitive.Mode of
        TPasGLTF.TMesh.TPrimitive.TMode.Lines        : Geometry := TLineSetNode.CreateWithShape(Shape);
        // TODO: these will require unpacking and expressing as TIndexedLineSetNode
        //TPasGLTF.TMesh.TPrimitive.TMode.LineLoop     : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        //TPasGLTF.TMesh.TPrimitive.TMode.LineStrip    : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles    : Geometry := TTriangleSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip: Geometry := TTriangleStripSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan  : Geometry := TTriangleFanSetNode.CreateWithShape(Shape);
        else
          begin
            WritelnWarning('glTF', 'Primitive mode not implemented (in non-indexed) mode: ' + PrimitiveModeToStr(Primitive.Mode));
            Exit;
          end;
      end;
    end;

    // read indexes
    IndexField := Geometry.CoordIndexField;
    if IndexField <> nil then
    begin
      Assert(Primitive.Indices <> -1);
      AccessorToInt32(Primitive.Indices, IndexField, false);
    end;

    // parse attributes (initializing Coord, TexCoord and other such nodes)
    // TODO: ForVertex true for all, or just for POSITION?
    for AttributeName in Primitive.Attributes.Keys do
    begin
      if (AttributeName = 'POSITION') and (Geometry.CoordField <> nil) then
      begin
        Coord := TCoordinateNode.Create;
        AccessorToVector3(Primitive.Attributes[AttributeName], Coord.FdPoint, true);
        Geometry.CoordField.Value := Coord;
      end else
      if IsPrefix('TEXCOORD_', AttributeName, false) and (Geometry.TexCoordField <> nil) then
      begin
        TexCoordIndex := StrToInt(PrefixRemove('TEXCOORD_', AttributeName, false));
        TexCoord := TTextureCoordinateNode.Create;
        AccessorToVector2(Primitive.Attributes[AttributeName], TexCoord.FdPoint, false);
        SetMultiTextureCoordinate(Geometry.TexCoordField, TexCoord, TexCoordIndex);
      end else
      if (AttributeName = 'NORMAL') and (Geometry is TAbstractComposedGeometryNode) then
      begin
        Normal := TNormalNode.Create;
        AccessorToVector3(Primitive.Attributes[AttributeName], Normal.FdVector, false);
        TAbstractComposedGeometryNode(Geometry).FdNormal.Value := Normal;
      end else
      if (AttributeName = 'COLOR_0') and (Geometry.ColorField <> nil) then
      begin
        ColorAccessor := GetAccessor(Primitive.Attributes[AttributeName]);
        if ColorAccessor.Type_ = TPasGLTF.TAccessor.TType.Vec4 then
        begin
          ColorRGBA := TColorRGBANode.Create;
          AccessorToVector4(Primitive.Attributes[AttributeName], ColorRGBA.FdColor, false);
          Geometry.ColorField.Value := ColorRGBA;
        end else
        begin
          Color := TColorNode.Create;
          AccessorToVector3(Primitive.Attributes[AttributeName], Color.FdColor, false);
          Geometry.ColorField.Value := Color;
        end;
      end else
        WritelnWarning('glTF', 'Ignoring vertex attribute ' + AttributeName + ', not implemented (for this primitive mode)');
    end;

    // determine Apperance
    if Between(Primitive.Material, 0, Appearances.Count - 1) then
      Appearance := Appearances[Primitive.Material] as TGltfAppearanceNode
    else
    begin
      Appearance := DefaultAppearance;
      if Primitive.Material <> -1 then
        WritelnWarning('glTF', 'Primitive specifies invalid material index %d',
          [Primitive.Material]);
    end;
    Shape.Appearance := Appearance;

    // apply additional TGltfAppearanceNode parameters, not handled by X3D renderer
    Geometry.Solid := not Appearance.DoubleSided;
    // TODO: For now we ignore Appearance.AlphaMode, Appearance.AlphaCutoff.

    // add to X3D
    ParentGroup.AddChildren(Shape);
  end;

  procedure ReadMesh(const Mesh: TPasGLTF.TMesh);
  var
    Primitive: TPasGLTF.TMesh.TPrimitive;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create;
    Group.X3DName := Mesh.Name;
    Result.AddChildren(Group);

    for Primitive in Mesh.Primitives do
      ReadPrimitive(Primitive, Group);
  end;

var
  Stream: TStream;
  Mesh: TPasGLTF.TMesh;
  Material: TPasGLTF.TMaterial;
begin
  Stream := Download(URL, []);
  try
    Result := TX3DRootNode.Create('', URL);
    try
      Document := nil;
      DefaultAppearance := nil;
      Appearances := nil;
      try
        Document := TPasGLTF.TDocument.Create;
        Document.RootPath := InclPathDelim(ExtractFilePath(URIToFilenameSafe(URL)));
        Document.LoadFromStream(Stream);

        ReadHeader;

        // read appearances (called "materials" in glTF; in X3D "material" is something smaller)
        DefaultAppearance := TGltfAppearanceNode.Create;
        DefaultAppearance.Material := TMaterialNode.Create;
        DefaultAppearance.DoubleSided := false;
        DefaultAppearance.AlphaCutOff := 0.5;
        DefaultAppearance.AlphaMode := TPasGLTF.TMaterial.TAlphaMode.Opaque;
        Appearances := TX3DNodeList.Create(false);
        for Material in Document.Materials do
          Appearances.Add(ReadAppearance(Material));

        // read meshes
        for Mesh in Document.Meshes do
          ReadMesh(Mesh);
      finally
        FreeIfUnusedAndNil(DefaultAppearance);
        X3DNodeList_FreeUnusedAndNil(Appearances);
        FreeAndNil(Document);
      end;
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(Stream) end;
end;

end.
