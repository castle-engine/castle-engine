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
  CastleVectors, CastleStringUtils, CastleTextureImages, CastleQuaternions,
  CastleImages;

{ Reading glTF using PasGLTF from Bero:
  https://github.com/BeRo1985/pasgltf/

  To understand glTF, and PasGLTF API, see the glTF specification:
  https://github.com/KhronosGroup/glTF/tree/master/specification/2.0
  This unit converts glTF to an X3D scene graph,
  so you should be familiar with X3D as well:
  https://castle-engine.io/vrml_x3d.php

  Some larger TODOs:

  - In the future, we would like to avoid using
    Accessor.DecodeAsXxx. Instead we should load binary data straight to GPU,
    looking at buffers, already exposed by PasGLTF.
    New X3D node, like BufferGeometry (same as X3DOM) will need to be
    invented for this, and CastleGeometryArrays will need to be rearranged.

  - We do not support PBR materials yet.

  - glTF animations are not supported.

  See https://castle-engine.io/planned_features.php .
}

type
  { X3D Appearance node extended to carry some additional information specified
    in glTF materials. }
  TGltfAppearanceNode = class(TAppearanceNode)
  public
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

  function Matrix4FromGltf(const M: TPasGLTF.TMatrix4x4): TMatrix4;
  begin
    // as it happens, both structures have the same memory layout, so copy by a fast Move
    Assert(SizeOf(M) = SizeOf(Result));
    Move(M, Result, SizeOf(Result));
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

  procedure ReadTexture(const GltfTextureAtMaterial: TPasGLTF.TMaterial.TTexture;
    out ImageTexture: TImageTextureNode; out TexCoordinateId: Integer);
  var
    GltfTexture: TPasGLTF.TTexture;
    GltfImage: TPasGLTF.TImage;
    GltfSampler: TPasGLTF.TSampler;
    TextureProperties: TTexturePropertiesNode;
  begin
    ImageTexture := nil;
    TexCoordinateId := 0;

    if not GltfTextureAtMaterial.Empty then
    begin
      ImageTexture := TImageTextureNode.Create('', URL);
      TexCoordinateId := GltfTextureAtMaterial.TexCoord;

      if GltfTextureAtMaterial.Index < Document.Textures.Count then
      begin
        GltfTexture := Document.Textures[GltfTextureAtMaterial.Index];

        if Between(GltfTexture.Source, 0, Document.Images.Count - 1) then
        begin
          GltfImage := Document.Images[GltfTexture.Source];
          // TODO: Use GltfImage.GetResourceData to optionally load from buffer?
          // But then, we would not be able to load from URLs that only CGE can handle,
          // like http/https, castle-data, castle-android-assets etc.
          // So only use GltfImage.GetResourceData if GltfImage.BufferView >= 0?
          if GltfImage.URI <> '' then
            ImageTexture.SetUrl([GltfImage.URI]);
        end;

        if Between(GltfTexture.Sampler, 0, Document.Samplers.Count - 1) then
        begin
          GltfSampler := Document.Samplers[GltfTexture.Sampler];

          ImageTexture.RepeatS := ReadTextureRepeat(GltfSampler.WrapS);
          ImageTexture.RepeatT := ReadTextureRepeat(GltfSampler.WrapT);

          if (GltfSampler.MinFilter <> TPasGLTF.TSampler.TMinFilter.None) or
             (GltfSampler.MagFilter <> TPasGLTF.TSampler.TMagFilter.None) then
          begin
            TextureProperties := TTexturePropertiesNode.Create;
            TextureProperties.MinificationFilter := ReadMinificationFilter(GltfSampler.MinFilter);
            TextureProperties.MagnificationFilter := ReadMagnificationFilter(GltfSampler.MagFilter);
            ImageTexture.TextureProperties := TextureProperties;
          end;
        end;
      end;
    end;
  end;

  function ReadAppearance(const Material: TPasGLTF.TMaterial): TGltfAppearanceNode;
  var
    CommonSurfaceShader: TCommonSurfaceShaderNode;
    BaseColorFactor: TVector4;
    BaseColorTexture, NormalTexture: TImageTextureNode;
    BaseColorTextureCoordinateId, NormalTextureCoordinateId: Integer;
    AlphaChannel: TAutoAlphaChannel;
  begin
    Result := TGltfAppearanceNode.Create(Material.Name);

    BaseColorFactor := Vector4FromGltf(Material.PBRMetallicRoughness.BaseColorFactor);
    CommonSurfaceShader := TCommonSurfaceShaderNode.Create;
    CommonSurfaceShader.DiffuseFactor := BaseColorFactor.XYZ;
    CommonSurfaceShader.AlphaFactor := BaseColorFactor.W;
    Result.SetShaders([CommonSurfaceShader]);

    Result.DoubleSided := Material.DoubleSided;

    ReadTexture(Material.PBRMetallicRoughness.BaseColorTexture,
      BaseColorTexture, BaseColorTextureCoordinateId);
    CommonSurfaceShader.MultiDiffuseAlphaTexture := BaseColorTexture;
    CommonSurfaceShader.DiffuseTextureCoordinatesId := BaseColorTextureCoordinateId;

    ReadTexture(Material.NormalTexture,
      NormalTexture, NormalTextureCoordinateId);
    CommonSurfaceShader.NormalTexture := NormalTexture;
    CommonSurfaceShader.NormalTextureCoordinatesId := NormalTextureCoordinateId;

    // read alpha channel treatment
    case Material.AlphaMode of
      TPasGLTF.TMaterial.TAlphaMode.Opaque: AlphaChannel := acNone;
      TPasGLTF.TMaterial.TAlphaMode.Blend : AlphaChannel := acBlending;
      TPasGLTF.TMaterial.TAlphaMode.Mask  : AlphaChannel := acTest;
      else raise EInternalError.Create('Unexpected glTF Material.AlphaMode value');
    end;
    // TODO: This should be applied on whole TAppearanceNode, not at particular textures.
    if BaseColorTexture <> nil then
      BaseColorTexture.AlphaChannelField := AlphaChannel;

    // TODO: ignored for now:
    // Appearance.AlphaCutOff := Material.AlphaCutOff;
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

  { glTF specification (unfortunately) defines (0,0) texture coord to be
    at top-left corner, while X3D and OpenGL and OpenGLES expect it be
    at bottom-left corner.
    See
    https://github.com/KhronosGroup/glTF/issues/1021
    https://github.com/KhronosGroup/glTF/issues/674
    https://github.com/KhronosGroup/glTF-Sample-Models/issues/82

    So we flip them.

    TODO: This procedure should be removed, instead we should add
    TImageTextureNode field like "flipY" to flip image at reading.
    This way tex coords will be possible to pass directly from glTF binary
    blob, without additional processing. }
  procedure FlipTexCoordY(const TexCoord: TVector2List);
  var
    Ptr: PVector2;
    I: Integer;
  begin
    Ptr := TexCoord.Ptr(0);
    for I := 0 to TexCoord.Count - 1 do
    begin
      Ptr^.Data[1] := 1 - Ptr^.Data[1];
      Inc(Ptr);
    end;
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
        FlipTexCoordY(TexCoord.FdPoint.Items);
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
      if (AttributeName = 'TANGENT') then
      begin
        { Don't do anything -- we don't store tangents now,
          but we can reliably calculate them when needed,
          so don't warn about them being unimplemented. }
      end else
        WritelnLog('glTF', 'Ignoring vertex attribute ' + AttributeName + ', not implemented (for this primitive mode)');
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

  procedure ReadMesh(const Mesh: TPasGLTF.TMesh; const ParentGroup: TAbstractX3DGroupingNode);
  var
    Primitive: TPasGLTF.TMesh.TPrimitive;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create;
    Group.X3DName := Mesh.Name;
    ParentGroup.AddChildren(Group);

    for Primitive in Mesh.Primitives do
      ReadPrimitive(Primitive, Group);
  end;

  procedure ReadMesh(const MeshIndex: Integer; const ParentGroup: TAbstractX3DGroupingNode);
  begin
    if Between(MeshIndex, 0, Document.Meshes.Count - 1) then
      ReadMesh(Document.Meshes[MeshIndex], ParentGroup)
    else
      WritelnWarning('glTF', 'Mesh index invalid: %d', [MeshIndex]);
  end;

  procedure ReadNode(const NodeIndex: Integer; const ParentGroup: TAbstractX3DGroupingNode);
  var
    Node: TPasGLTF.TNode;
    Transform: TTransformNode;
    NodeMatrix: TMatrix4;
    Translation, Scale: TVector3;
    RotationQuaternion: TQuaternion;
    Rotation: TVector4;
    ChildNodeIndex: Integer;
  begin
    if Between(NodeIndex, 0, Document.Nodes.Count - 1) then
    begin
      Node := Document.Nodes[NodeIndex];
      NodeMatrix := Matrix4FromGltf(Node.Matrix);

      if not TMatrix4.PerfectlyEquals(NodeMatrix, TMatrix4.Identity) then
      begin
        MatrixDecompose(NodeMatrix, Translation, Rotation, Scale);
      end else
      begin
        Translation := Vector3FromGltf(Node.Translation);
        RotationQuaternion.Data.Vector4 := Vector4FromGltf(Node.Rotation);
        Rotation := RotationQuaternion.ToAxisAngle;
        Scale := Vector3FromGltf(Node.Scale);
      end;

      Transform := TTransformNode.Create;
      Transform.X3DName := Node.Name;
      Transform.Translation := Translation;
      Transform.Rotation := Rotation;
      Transform.Scale := Scale;
      ParentGroup.FdChildren.Add(Transform);

      if Node.Mesh <> -1 then
        ReadMesh(Node.Mesh, Transform);

      for ChildNodeIndex in Node.Children do
        ReadNode(ChildNodeIndex, Transform);
    end else
      WritelnWarning('glTF', 'Node index invalid: %d', [NodeIndex]);
  end;

  procedure ReadScene(const SceneIndex: Integer; const ParentGroup: TAbstractX3DGroupingNode);
  var
    Scene: TPasGLTF.TScene;
    NodeIndex: Integer;
  begin
    if Between(SceneIndex, 0, Document.Scenes.Count - 1) then
    begin
      Scene := Document.Scenes[SceneIndex];
      for NodeIndex in Scene.Nodes do
        ReadNode(NodeIndex, ParentGroup);
    end else
      WritelnWarning('glTF', 'Scene index invalid: %d', [SceneIndex]);
  end;

var
  Stream: TStream;
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
        Appearances := TX3DNodeList.Create(false);
        for Material in Document.Materials do
          Appearances.Add(ReadAppearance(Material));

        // read main scene
        if Document.Scene <> -1 then
          ReadScene(Document.Scene, Result);
      finally
        FreeIfUnusedAndNil(DefaultAppearance);
        X3DNodeList_FreeUnusedAndNil(Appearances);
        FreeAndNil(Document);
      end;
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(Stream) end;
end;

end.
