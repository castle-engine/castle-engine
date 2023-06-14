{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine attachments. }

{$ifdef read_interface}
  TAttachment = class abstract
    { SlotName and AttachmentName are map keys, to detect which TAttachment to use. }
    SlotName, AttachmentName: string;
    { The real attachment name.

      For image attachment this refers to texture loader (e.g. atlas) region name.
      This is a unique region name, but multiple TAttachment may refer to the same
      Name (see Escape ship_upgrade.json testcase).

      For path this refers to path name.
      Multiple slots may refer to the same path name,
      and it does *not* have to be the same path geometry then. }
    Name: string;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    Material: TUnlitMaterialNode; //< initialized together with Node, always inside
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); virtual;
    { Create and parse correct TAttachment descendant.
      May return @nil if none possible. }
    class function CreateAndParse(const Json: TJSONObject;
      const ASlotName, AnAttachmentName: string; const Bones: TBoneList): TAttachment;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); virtual; abstract;
    { Some attachments (skinnedmesh) can be attached only to a single bone
      at a time, and must be transformed from world to bone space. }
    procedure TransformToBoneSpace(const Bone: TBone); virtual;
  end;

  TRegionAttachment = class(TAttachment)
    XY: TVector2;
    Scale: TVector2;
    Rotation: Single;
    Width, Height: Integer;
    Path: string;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); override;
    function BuildGeometry(const BaseUrl: string;
      const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
      const TexRotated: boolean): TAbstractGeometryNode; virtual;
  end;

  TAbstractMeshAttachment = class(TRegionAttachment)
  public
    Coord: TCoordinateNode;
    UVs: TVector2List;
    Triangles: TInt32List;
    Vertices: TVector2List;
    Edges: TInt32List; //< unused for now
    { Unused for now.
      Looking as official Spine runtime in spine-csharp and spine-unity,
      it is also unused there. }
    Hull: Integer;
    Invalid: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    function BuildGeometry(const BaseUrl: string;
      const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
      const TexRotated: boolean): TAbstractGeometryNode; override;
  end;

  TMeshVertexBone = class
    Bone: TBone;
    V: TVector2; //< vertex position in local Bone coordinate system
    Weight: Single;
  end;
  TMeshVertexBoneList = {$ifdef FPC}specialize{$endif} TObjectList<TMeshVertexBone>;

  TMeshVertex = class
    Offset: Integer; // Offset relative to where this Vertex is loaded from the list
    Bones: TMeshVertexBoneList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const JsonArray: TJSONArray;
      Index: Integer; const BonesCount: Cardinal; const AllBones: TBoneList);
  end;

  TMeshVertexList = class({$ifdef FPC}specialize{$endif} TObjectList<TMeshVertex>)
    procedure Parse(const JsonArray: TJSONArray; const Bones: TBoneList);
  end;

  TMeshAttachment = class(TAbstractMeshAttachment)
    Weights: TMeshVertexList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
  end;

  TSkinnedMeshAttachment = class(TMeshAttachment)
    procedure TransformToBoneSpace(const Bone: TBone); override;
  end;

  TPathAttachment = class(TAttachment)
    Vertices: TVector2List;
    Lengths: TSingleList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); override;
  end;

  TClippingAttachment = class(TAttachment)
    Vertices: TVector2List;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); override;
  end;

  TAttachmentList = class({$ifdef FPC}specialize{$endif} TObjectList<TAttachment>)
    { Find by slot+attachment name.
      @raises ESpineReadError If does not exist. }
    function Find(const SlotName, AttachmentName: string;
      const FallbackList: TAttachmentList;
      const NilWhenNotFound: boolean): TAttachment;
  end;
{$endif}

{$ifdef read_implementation}

{ TAttachment ---------------------------------------------------------------- }

class function TAttachment.CreateAndParse(const Json: TJSONObject;
  const ASlotName, AnAttachmentName: string; const Bones: TBoneList): TAttachment;
var
  TypeName: string;
begin
  TypeName := Json.Get('type', 'region');
  if TypeName = 'mesh' then
  begin
    Result := TMeshAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json, Bones);
  end else
  if (TypeName = 'region') or
     (TypeName = 'regionsequence') then
  begin
    if TypeName = 'regionsequence' then
      WritelnWarning('Spine', 'Spine region type "regionsequence" is for now rendered just like normal "region"');
    Result := TRegionAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json, Bones);
  end else
  if TypeName = 'path' then
  begin
    Result := TPathAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json, Bones);
  end else
  if TypeName = 'clipping' then
  begin
    Result := TClippingAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json, Bones);
  end else
  if TypeName = 'skinnedmesh' then
  begin
    WritelnWarning('Spine', 'Region type skinnedmesh may not be supported in future CGE versions, please export your model again from latest Spine version');
    Result := TSkinnedMeshAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json, Bones);
  end else
  begin
    WritelnWarning('Spine', 'Spine region (attachment) type "%s" is not supported', [TypeName]);
    Result := nil;
  end;
end;

procedure TAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  Name := Json.Get('name', AttachmentName);
end;

destructor TAttachment.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TAttachment.TransformToBoneSpace(const Bone: TBone);
begin
end;

{ TRegionAttachment ---------------------------------------------------------- }

procedure TRegionAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  inherited;
  XY.X := Json.Get('x', 0.0);
  XY.Y := Json.Get('y', 0.0);
  Scale.X := Json.Get('scaleX', 1.0);
  Scale.Y := Json.Get('scaleY', 1.0);
  Rotation := Json.Get('rotation', 0.0);
  Width := Json.Get('width', 0);
  Height := Json.Get('height', 0);
  Path := Json.Get('path', '');
end;

procedure TRegionAttachment.BuildNodes(const BaseUrl: string;
  const TextureLoader: TTextureLoader);
var
  TexRect: TQuadTexRect;
  TexCoord: TQuadTexCoord;
  Shape: TShapeNode;
  AtlasRegionName: string;
  TexRotated: boolean;
begin
  if Node <> nil then
    WritelnWarning('Spine', 'TRegionAttachment.BuildNodes multiple times on same node');

  Node := TTransformNode.Create(
    Format('Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);
  Node.Translation := Vector3(XY.X, XY.Y, 0);
  Node.Scale := Vector3(Scale.X, Scale.Y, 1);
  Node.Rotation := Vector4(0, 0, 1, DegToRad(Rotation));

  Shape := TShapeNode.Create('', BaseUrl);
  Node.AddChildren(Shape);

  Shape.Appearance := TAppearanceNode.Create('', BaseUrl);
  AtlasRegionName := Name;
  if Path <> '' then
    AtlasRegionName := Path;
  Shape.Appearance.Texture := TextureLoader.UseNode(AtlasRegionName,
    TexCoord, TexRect, TexRotated);

  { Other alpha values are not really useful for Spine.

    Setting AlphaMode explicitly is necessary in case we use TSimpleTextureLoader,
    as then it's easy to have some images with blend, some with opaque,
    and if we render them with different (automatic) alpha modes
    -> it will cause fighting in Z-buffer. This fighting in Z-buffer can be counteracted
    by increasing TSlot.DrawOrderZ, but sometimes it would require really large increase.
    Testcase: private-assets/spine/unholy_images_without_atlas/
    where even TSlot.DrawOrderZ = 1 is not enough (10 is enough).

    So with TSimpleTextureLoader, it's more reliable to use amBlend for all.

    For TAtlas, it's just simpler. Why auto-detect, when we just know to use blending
    in all normal cases. }
  Shape.Appearance.AlphaMode := amBlend;

  if Shape.Appearance.Texture <> nil then
    Shape.Geometry := BuildGeometry(BaseUrl, TexCoord, TexRect, TexRotated);

  Material := TUnlitMaterialNode.Create(
    Format('Material_Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);
  Material.EmissiveColor := Vector3(1, 1, 1);

  Shape.Material := Material;
end;

function TRegionAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
  const TexRotated: boolean): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
  Faces: TIndexedTriangleSetNode;
  Rect: array [0..1] of TVector2;
begin
  { We use here TIndexedTriangleSetNode, not TIndexedFaceSetNode,
    to allow DynamicBatching to merge TRegionAttachment with TMeshAttachment
    into one shape. }
  Faces := TIndexedTriangleSetNode.Create('', BaseUrl);
  Result := Faces;

  Faces.NormalPerVertex := false; // optimization: do not try to smooth normals, no point
  Faces.Solid := false;
  Faces.SetIndex([0, 1, 2, 0, 2, 3]); // 2 triangles forming a quad

  Coord := TCoordinateNode.Create(
    Format('Coordinate_Slot_%s_Attachment_%s', [SlotName, Name]), // name it, as ROUTE may lead to it
    Faces.BaseUrl);
  Rect[0] := Vector2(
    MapRange(TexRect[0].X, 0, 1, -Width / 2, Width / 2),
    MapRange(TexRect[0].Y, 0, 1, -Height / 2, Height / 2));
  Rect[1] := Vector2(
    MapRange(TexRect[1].X, 0, 1, -Width / 2, Width / 2),
    MapRange(TexRect[1].Y, 0, 1, -Height / 2, Height / 2));
  Coord.SetPoint([
    Vector3(Rect[0].X, Rect[0].Y, 0),
    Vector3(Rect[1].X, Rect[0].Y, 0),
    Vector3(Rect[1].X, Rect[1].Y, 0),
    Vector3(Rect[0].X, Rect[1].Y, 0)]);
  Faces.Coord := Coord;

  TexCoordNode := TTextureCoordinateNode.Create('', Faces.BaseUrl);
  TexCoordNode.SetPoint([
    TexCoord[0],
    TexCoord[1],
    TexCoord[2],
    TexCoord[3]]);
  Faces.TexCoord := TexCoordNode;
end;

{ TAbstractMeshAttachment ---------------------------------------------------- }

constructor TAbstractMeshAttachment.Create;
begin
  inherited;
  UVs := TVector2List.Create;
  Triangles := TInt32List.Create;
  Vertices := TVector2List.Create;
  Edges := TInt32List.Create;
end;

destructor TAbstractMeshAttachment.Destroy;
begin
  FreeAndNil(UVs);
  FreeAndNil(Triangles);
  FreeAndNil(Vertices);
  FreeAndNil(Edges);
  inherited;
end;

procedure TAbstractMeshAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  inherited;
  ReadVector2List(Json, 'uvs', UVs);

  ReadInt32List(Json, 'triangles', Triangles);
  if Triangles.Count mod 3 <> 0 then
    WritelnWarning('Spine', 'Triangles list ends in the middle of the triangle');
  if Triangles.Count = 0 then
    WritelnWarning('Spine', 'Triangles empty, mesh is empty');

  ReadInt32List(Json, 'edges', Edges);
  Hull := Json.Get('hull', 0);
end;

function TAbstractMeshAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
  const TexRotated: boolean): TAbstractGeometryNode;
var
  TexCoordNode: TTextureCoordinateNode;
  I: Integer;
  UVRangeX, UVRangeY: TVector2;
  Faces: TIndexedTriangleSetNode;
begin
  Faces := TIndexedTriangleSetNode.Create('', BaseUrl);
  Result := Faces;

  // do not call SetIndex in case Vertices count is known to be invalid
  if Invalid then Exit;

  Faces.NormalPerVertex := false; // optimization: do not try to smooth normals, no point
  Faces.Solid := false;
  Faces.SetIndex(Triangles);

  Coord := TCoordinateNode.Create(
    Format('Coordinate_Slot_%s_Attachment_%s', [SlotName, Name]), // name it, as ROUTE may lead to it
    Faces.BaseUrl);
  for I := 0 to Vertices.Count - 1 do
    Coord.FdPoint.Items.Add(Vector3(Vertices[I].X, Vertices[I].Y, 0));
  Faces.Coord := Coord;

  { We do not guarantee correct rendering when model used
    atlas "whitespace compression" by Spine, and used mesh attachment types.
    I see no way to support it sensibly --- we cannot easily squeeze the geometry
    to eliminate the empty space, because in general it means that triangles
    may need to be cut in half, and need to be changed to quads with new vertexes,
    to avoid rendering the atlas part where there should be whitespace.
    So supporting this would be crazy difficult.

    I don't see official spine runtimes having any code to deal with it either.

    The rendering will be Ok only if the mesh faces used only the texture
    parts within the non-whitespace region (TexRect). Which is true
    in practice in all the examples. }

  if ((TexRect[0].X <> 0) or
      (TexRect[1].X <> 0) or
      (TexRect[0].Y <> 1) or
      (TexRect[1].Y <> 1)) and
     SpineVerboseWarnings then
    WritelnLog('Spine', Format('Slot "%s" with attachment "%s" uses "mesh" attachment type, and you exported atlas with "Strip whitespace X" or "Strip whitespace Y" options selected. '+'Rendering artifacts may occur if mesh faces use the texture in the "whitespace" region. To be safe, export with "Strip whitespace X = Off" and "Strip whitespace Y = Off" in "Texture Packer Settings".',
      [SlotName, Name]));

  if TexRotated then
    WritelnWarning('Spine ' + URIDisplay(BaseUrl), 'Atlas region used for mesh attachment is rotated. This is not supported, texture will be applied incorrectly. Export with "Rotation" turned off in "Texture Packer Settings".');

  TexCoordNode := TTextureCoordinateNode.Create('', Faces.BaseUrl);
  UVRangeX.X := Min(TexCoord[0].X, TexCoord[2].X);
  UVRangeX.Y := Max(TexCoord[0].X, TexCoord[2].X);
  UVRangeY.X := Min(TexCoord[0].Y, TexCoord[2].Y);
  UVRangeY.Y := Max(TexCoord[0].Y, TexCoord[2].Y);
  for I := 0 to UVs.Count - 1 do
    TexCoordNode.FdPoint.Items.Add(Vector2(
      MapRange(UVs[I].X, 0, 1, UVRangeX.X, UVRangeX.Y),
      MapRange(UVs[I].Y, 1, 0, UVRangeY.X, UVRangeY.Y)
    ));
  Faces.FdTexCoord.Value := TexCoordNode;
end;

{ TMeshAttachment ------------------------------------------------------------ }

{ TMeshVertex --------------------------------------------------------- }

constructor TMeshVertex.Create;
begin
  inherited;
  Bones := TMeshVertexBoneList.Create;
end;

destructor TMeshVertex.Destroy;
begin
  FreeAndNil(Bones);
  inherited;
end;

procedure TMeshVertex.Parse(const JsonArray: TJSONArray;
  Index: Integer; const BonesCount: Cardinal; const AllBones: TBoneList);
var
  I, BoneIndex: Integer;
  B: TMeshVertexBone;
begin
  for I := 0 to Integer(BonesCount) - 1 do
  begin
    BoneIndex := JsonArray[Index].AsInteger; Inc(Index);
    if not Between(BoneIndex, 0, AllBones.Count - 1) then
      raise Exception.CreateFmt('Invalid bone index in mesh.vertices: %d', [BoneIndex]);
    B := TMeshVertexBone.Create;
    Bones.Add(B);
    B.Bone := AllBones[BoneIndex];
    B.V.X := JsonArray[Index].AsFloat; Inc(Index);
    B.V.Y := JsonArray[Index].AsFloat; Inc(Index);
    B.Weight := JsonArray[Index].AsFloat; Inc(Index);
  end;
end;

{ TMeshVertexList ----------------------------------------------------- }

procedure TMeshVertexList.Parse(const JsonArray: TJSONArray; const Bones: TBoneList);
var
  BonesCount: Cardinal;
  V: TMeshVertex;
  I: Integer;
  Offset: Integer;
begin
  Offset := 0;
  I := 0;
  while I < JsonArray.Count do
  begin
    BonesCount := Max(0, JsonArray[I].AsInteger);
    if I + BonesCount * 4 < JsonArray.Count then
    begin
      if BonesCount = 0 then
        WritelnWarning('Spine', 'mesh bonesCount is <= 0');
      V := TMeshVertex.Create;
      V.Parse(JsonArray, I + 1, BonesCount, Bones);
      V.Offset := Offset;
      Offset := Offset + BonesCount * 2;
      Add(V);
    end else
    begin
      WritelnWarning('Spine', Format('mesh weights (called "vertices" in JSON) do not have enough items (last bonesCount = %d, but array does not have enough floats)',
        [BonesCount]));
      Break;
    end;

    I := I + 1 + BonesCount * 4;
  end;
end;

{ TMeshAttachment ----------------------------------------------------- }

constructor TMeshAttachment.Create;
begin
  inherited;
  Weights := TMeshVertexList.Create;
end;

destructor TMeshAttachment.Destroy;
begin
  FreeAndNil(Weights);
  inherited;
end;

procedure TMeshAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  JsonArray: TJSONArray;
  I, J: Integer;
  VB: TMeshVertexBone;
  V: TVector2;
begin
  inherited;
  ReadVector2List(Json, 'vertices', Vertices, true);

  if Vertices.Count > UVs.Count then
  begin
    Vertices.Clear;
    JsonArray := Json.Find('vertices', jtArray) as TJSONArray;
    if JsonArray <> nil then
      Weights.Parse(JsonArray, Bones);
    { calculate initial Vertices from Weights.
      Similar to ComputeWorldVertices in spine-runtimes/spine-csharp/src/Attachments/SkinnedMeshAttachment.cs }
    Vertices.Count := Weights.Count;
    for I := 0 to Weights.Count - 1 do
    begin
      V := TVector2.Zero;
      for J := 0 to Weights[I].Bones.Count - 1 do
      begin
        VB := Weights[I].Bones[J];
        V := V + (VB.Bone.ToWorldSpace(VB.V) * VB.Weight);
      end;
      Vertices[I] := V;
    end;
  end;
end;


{ TSkinnedMeshAttachment --------------------------------------------------- }

procedure TSkinnedMeshAttachment.TransformToBoneSpace(const Bone: TBone);
var
  List: TVector3List;
  I: Integer;
begin
  inherited;

  List := Coord.FdPoint.Items;
  for I := 0 to List.Count - 1 do
    List.List^[I] := Bone.ToLocalSpace(List.List^[I]);
end;

{ TPathAttachment ---------------------------------------------------------- }

constructor TPathAttachment.Create;
begin
  inherited;
  Vertices := TVector2List.Create;
  Lengths := TSingleList.Create;
end;

destructor TPathAttachment.Destroy;
begin
  FreeAndNil(Lengths);
  FreeAndNil(Vertices);
  inherited;
end;

procedure TPathAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  VertexCount: Integer;
begin
  inherited;
  VertexCount := Json.Get('vertexCount', 0);
  ReadVector2List(Json, 'vertices', Vertices);
  ReadSingleList(Json, 'lengths', Lengths);
  if Vertices.Count <> VertexCount then
    WritelnWarning('Spine "path" attachment has unexpected "vertices" count: expected %d, has %d', [VertexCount, Vertices.Count]);
  if Lengths.Count <> VertexCount div 3 then
    WritelnWarning('Spine "path" attachment has unexpected "lengths" count: expected %d, has %d', [VertexCount div 3, Lengths.Count]);
end;

procedure TPathAttachment.BuildNodes(const BaseUrl: string;
  const TextureLoader: TTextureLoader);
var
  Shape: TShapeNode;
  ControlPoints: TCoordinateNode;
  Curve: TNurbsCurveNode;
  I: Integer;
begin
  if Node <> nil then
    WritelnWarning('Spine', 'TPathAttachment.BuildNodes multiple times on same node');

  Node := TTransformNode.Create(
    Format('Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);

  Shape := TShapeNode.Create('', BaseUrl);
  Node.AddChildren(Shape);

  Material := TUnlitMaterialNode.Create(
    Format('Material_Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);
  Material.EmissiveColor := WhiteRGB;
  Shape.Material := Material;

  Shape.Render := false;

  ControlPoints := TCoordinateNode.Create('', BaseUrl);
  { we ignore first and last Vertices, they are only handles outside of
    the curve, not taking part in curve calculation }
  for I := 1 to Vertices.Count - 2 do
    ControlPoints.FdPoint.Items.Add(Vector3(Vertices[I], 0));

  Curve := TNurbsCurveNode.Create('', BaseUrl);
  Curve.ControlPoint := ControlPoints;
  Curve.Order := 4; // cubic Bezier
  Curve.Tessellation := ControlPoints.FdPoint.Count * 10; // by default, make quite detailed tesselation
  Curve.PiecewiseBezier;
  Shape.Geometry := Curve;
end;

{ TClippingAttachment ---------------------------------------------------------- }

constructor TClippingAttachment.Create;
begin
  inherited;
  Vertices := TVector2List.Create;
end;

destructor TClippingAttachment.Destroy;
begin
  FreeAndNil(Vertices);
  // If you didn't call VisualizeClipPolygon in BuildNodes, Material node remains unused
  FreeIfUnusedAndNil(Material);
  inherited;
end;

procedure TClippingAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  VertexCount: Integer;
begin
  inherited;
  VertexCount := Json.Get('vertexCount', 0);
  ReadVector2List(Json, 'vertices', Vertices);
  if Vertices.Count <> VertexCount then
    WritelnWarning('Spine "clipping" attachment has unexpected "vertices" count: expected %d, has %d', [VertexCount, Vertices.Count]);
end;

procedure TClippingAttachment.BuildNodes(const BaseUrl: string;
  const TextureLoader: TTextureLoader);

  procedure VisualizeClipPolygon;
  var
    Coordinates: TCoordinateNode;
    Faces: TIndexedFaceSetNode;
    Shape: TShapeNode;
    I: Integer;
  begin
    Shape := TShapeNode.Create('', BaseUrl);
    Shape.Material := Material;
    Node.AddChildren(Shape);

    Faces := TIndexedFaceSetNode.Create('', BaseUrl);
    Faces.NormalPerVertex := false; // optimization: do not try to smooth normals, no point
    Faces.CreaseAngle := 0; // optimization: do not try to smooth normals, no point
    Faces.Solid := false;

    Coordinates := TCoordinateNode.Create('', BaseUrl);
    for I := 0 to Vertices.Count - 1 do
    begin
      Coordinates.FdPoint.Items.Add(Vector3(Vertices[I], 0));
      Faces.FdCoordIndex.Items.Add(I);
    end;

    Faces.Coord := Coordinates;
    Faces.Solid := false;
    Faces.Convex := false;
    Shape.Geometry := Faces;
  end;

  procedure AddClipPlane(const Point, NextPoint: TVector2);
  var
    D: Single;
    Plane: TVector4;
    ClipPlane: TClipPlaneNode;
  begin
    D := PointsDistance(Point, NextPoint);
    Plane := Triangle3(
      Vector3(Point, 0),
      Vector3(NextPoint, 0),
      Vector3((Point + NextPoint) / 2, D)
    ).NormalizedPlane;
    ClipPlane := TClipPlaneNode.Create('', BaseUrl);
    ClipPlane.Plane := Plane;
    Node.AddChildren(ClipPlane);
  end;

var
  I: Integer;
begin
  if Node <> nil then
    WritelnWarning('Spine', 'TClippingAttachment.BuildNodes multiple times on same node');

  Node := TTransformNode.Create(
    Format('Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);

  Material := TUnlitMaterialNode.Create(
    Format('Material_Slot_%s_Attachment_%s', [SlotName, Name]),
    BaseUrl);
  Material.EmissiveColor := WhiteRGB;

  // debug
  //VisualizeClipPolygon;

  for I := 0 to Vertices.Count - 1 do
    AddClipPlane(Vertices[I], Vertices[(I + 1) mod Vertices.Count]);

  // TODO: This is cool, but ClipPlanes don't affect anything.
  // To make this approach cool, we would need ClipPlane.global field.
  // Alternatively, we could make a different approach using stencil buffer.

  WritelnWarning('Spine', 'Spine "clipping" region type is not yet implemented');
end;

{ TAttachmentList ------------------------------------------------------------ }

function TAttachmentList.Find(const SlotName, AttachmentName: string;
  const FallbackList: TAttachmentList;
  const NilWhenNotFound: boolean): TAttachment;
var
  I: Integer;
  Err: string;
begin
  for I := 0 to Count - 1 do
    if (Items[I].SlotName = SlotName) and
       (Items[I].AttachmentName = AttachmentName) then
      Exit(Items[I]);

  if FallbackList <> nil then
    Result := FallbackList.Find(SlotName, AttachmentName, nil, NilWhenNotFound) else
  begin
    Err := Format('Attachment values for slot "%s" and attachment name "%s" not found',
      [SlotName, AttachmentName]);
    if NilWhenNotFound then
    begin
      WritelnLog('Spine', Err); // this is actually normal, as tested on Venice models, so only log, no warning
      Result := nil;
    end else
      raise ESpineReadError.Create(Err);
  end;
end;
{$endif}
