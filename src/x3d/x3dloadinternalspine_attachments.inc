{
  Copyright 2014-2017 Michalis Kamburelis.

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
    { The real attachment name, for image attachment
      this refers to texture loader (e.g. atlas) region name.
      This is a unique region name, but multiple TAttachment may refer to the same
      Name (see Escape ship_upgrade.json testcase). }
    Name: string;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    Material: TMaterialNode; //< initialized together with Node, always inside
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); virtual;
    { Create and parse correct TAttachment descendant.
      May return @nil if none possible. }
    class function CreateAndParse(const Json: TJSONObject;
      const ASlotName, AnAttachmentName: string; const Bones: TBoneList): TAttachment;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); virtual; abstract;
    { Some attachments (skinnedmesh) can be attached only to a single bone
      at a time, and must be transfomed from world to bone space. }
    procedure TransformToBoneSpace(const Bone: TBone); virtual;
  end;

  TRegionAttachment = class(TAttachment)
    XY: TVector2Single;
    Scale: TVector2Single;
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
  strict protected
    Coord: TCoordinateNode;
  public
    UVs: TVector2SingleList;
    Triangles: TLongIntList;
    Vertices: TVector2SingleList;
    Edges: TLongIntList; //< unused for now
    { Unused for now.
      Looking as official Spine runtime in spine-csharp and spine-unity,
      it is also unused there. }
    Hull: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    function BuildGeometry(const BaseUrl: string;
      const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
      const TexRotated: boolean): TAbstractGeometryNode; override;
  end;

  TMeshAttachment = class(TAbstractMeshAttachment)
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
  end;

  TSkinnedMeshVertexBone = class
    Bone: TBone;
    V: TVector2Single; //< vertex position in local Bone coordinate system
    Weight: Single;
  end;
  TSkinnedMeshVertexBoneList = specialize TFPGObjectList<TSkinnedMeshVertexBone>;

  TSkinnedMeshVertex = class
    Bones: TSkinnedMeshVertexBoneList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const JsonArray: TJSONArray;
      Index: Integer; const BonesCount: Cardinal; const AllBones: TBoneList);
  end;

  TSkinnedMeshVertexList = class(specialize TFPGObjectList<TSkinnedMeshVertex>)
    procedure Parse(const JsonArray: TJSONArray; const Bones: TBoneList);
  end;

  TSkinnedMeshAttachment = class(TAbstractMeshAttachment)
    Weights: TSkinnedMeshVertexList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList); override;
    procedure TransformToBoneSpace(const Bone: TBone); override;
  end;

  TAttachmentList = class(specialize TFPGObjectList<TAttachment>)
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
  if TypeName = 'skinnedmesh' then
  begin
    WritelnWarning('Spine', Format('Spine region type "skinnedmesh" is for now not deformed during animation, for attachment "%s"',
      [AnAttachmentName]));
    Result := TSkinnedMeshAttachment.Create;
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
  if TypeName = 'boundingbox' then
  begin
    WritelnWarning('Spine', 'Spine region type "boundingbox" is ignored for now');
    Result := nil;
  end else
    raise ESpineReadError.CreateFmt('Attachment type "%s" not supported', [TypeName]);
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
  XY[0] := Json.Get('x', 0.0);
  XY[1] := Json.Get('y', 0.0);
  Scale[0] := Json.Get('scaleX', 1.0);
  Scale[1] := Json.Get('scaleY', 1.0);
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
    Format('Slot_%s_Attachment_%s', [ToX3DName(SlotName), ToX3DName(Name)]),
    BaseUrl);
  Node.FdTranslation.Value := Vector3Single(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3Single(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4Single(0, 0, 1, DegToRad(Rotation));

  Shape := TShapeNode.Create('', BaseUrl);
  Node.FdChildren.Add(Shape);

  Shape.Appearance := TAppearanceNode.Create('', BaseUrl);
  AtlasRegionName := Name;
  if Path <> '' then
    AtlasRegionName := Path;
  Shape.Appearance.Texture := TextureLoader.UseNode(AtlasRegionName,
    TexCoord, TexRect, TexRotated);
  if Shape.Appearance.Texture <> nil then
    Shape.FdGeometry.Value := BuildGeometry(BaseUrl,
      TexCoord, TexRect, TexRotated);

  Shape.Material := TMaterialNode.Create(
    Format('Material_Slot_%s_Attachment_%s', [ToX3DName(SlotName), ToX3DName(Name)]),
    BaseUrl);
  Shape.Material.ForcePureEmissive;
  Shape.Material.FdEmissiveColor.Value := Vector3Single(1, 1, 1);
  Material := Shape.Material;
end;

function TRegionAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
  const TexRotated: boolean): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
  I: Integer;
  Faces: TIndexedFaceSetNode;
  Rect: array [0..1] of TVector2Single;
begin
  Faces := TIndexedFaceSetNode.Create('', BaseUrl);
  Result := Faces;

  Faces.FdCreaseAngle.Value := 0; // optimization: do not try to smooth normals, no point
  Faces.FdSolid.Value := false;
  Faces.FdCoordIndex.Items.Clear;

  Faces.FdCoordIndex.Items.Add(0);
  Faces.FdCoordIndex.Items.Add(1);
  Faces.FdCoordIndex.Items.Add(2);
  Faces.FdCoordIndex.Items.Add(3);

  Coord := TCoordinateNode.Create('', Faces.BaseUrl);
  Rect[0][0] := MapRange(TexRect[0][0], 0, 1, -Width / 2, Width / 2);
  Rect[1][0] := MapRange(TexRect[1][0], 0, 1, -Width / 2, Width / 2);
  Rect[0][1] := MapRange(TexRect[0][1], 0, 1, -Height / 2, Height / 2);
  Rect[1][1] := MapRange(TexRect[1][1], 0, 1, -Height / 2, Height / 2);
  Coord.FdPoint.Items.Add(Vector3Single(Rect[0][0], Rect[0][1], 0));
  Coord.FdPoint.Items.Add(Vector3Single(Rect[1][0], Rect[0][1], 0));
  Coord.FdPoint.Items.Add(Vector3Single(Rect[1][0], Rect[1][1], 0));
  Coord.FdPoint.Items.Add(Vector3Single(Rect[0][0], Rect[1][1], 0));
  Faces.FdCoord.Value := Coord;

  TexCoordNode := TTextureCoordinateNode.Create('', Faces.BaseUrl);
  for I := 0 to 3 do
    TexCoordNode.FdPoint.Items.Add(TexCoord[I]);
  Faces.FdTexCoord.Value := TexCoordNode;
end;

{ TAbstractMeshAttachment ---------------------------------------------------- }

constructor TAbstractMeshAttachment.Create;
begin
  inherited;
  UVs := TVector2SingleList.Create;
  Triangles := TLongIntList.Create;
  Vertices := TVector2SingleList.Create;
  Edges := TLongIntList.Create;
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
  ReadVector2SingleList(Json, 'uvs', UVs);

  ReadLongIntList(Json, 'triangles', Triangles);
  if Triangles.Count mod 3 <> 0 then
    WritelnWarning('Spine', 'Triangles list ends in the middle of the triangle');
  if Triangles.Count = 0 then
    WritelnWarning('Spine', 'Triangles empty, mesh is empty');

  ReadLongIntList(Json, 'edges', Edges);
  Hull := Json.Get('hull', 0);
end;

function TAbstractMeshAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect;
  const TexRotated: boolean): TAbstractGeometryNode;
var
  TexCoordNode: TTextureCoordinateNode;
  I: Integer;
  UVRangeX, UVRangeY: TVector2Single;
  Faces: TIndexedTriangleSetNode;
begin
  Faces := TIndexedTriangleSetNode.Create('', BaseUrl);
  Result := Faces;

  Faces.FdNormalPerVertex.Value := false; // optimization: do not try to smooth normals, no point
  Faces.FdSolid.Value := false;
  Faces.FdIndex.Items.Clear;

  Faces.FdIndex.Items.Assign(Triangles);

  Coord := TCoordinateNode.Create('', Faces.BaseUrl);
  for I := 0 to Vertices.Count - 1 do
    Coord.FdPoint.Items.Add(Vector3Single(Vertices[I][0], Vertices[I][1], 0));
  Faces.FdCoord.Value := Coord;

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

  if ((TexRect[0][0] <> 0) or
      (TexRect[1][0] <> 0) or
      (TexRect[0][1] <> 1) or
      (TexRect[1][1] <> 1)) and
     LogSpineMeshCompressWhitespaceWarning then
    WritelnLog('Spine', Format('Slot "%s" with attachment "%s" uses "mesh" attachment type, and you exported atlas with "compress whitespace" option selected. Rendering artifacts may occur if mesh faces use the texture in "whitespace" region --- in such case, export again with "compress whitespace" turned off at atlas generation.',
      [SlotName, Name]));

  if TexRotated then
    WritelnWarning('Spine', 'Atlas region is rotated, and is used for mesh attachment. This is not supported (texture will most likely be applied incorrectly), export atlas again with "Rotate" turned off.');

  TexCoordNode := TTextureCoordinateNode.Create('', Faces.BaseUrl);
  UVRangeX[0] := Min(TexCoord[0][0], TexCoord[2][0]);
  UVRangeX[1] := Max(TexCoord[0][0], TexCoord[2][0]);
  UVRangeY[0] := Min(TexCoord[0][1], TexCoord[2][1]);
  UVRangeY[1] := Max(TexCoord[0][1], TexCoord[2][1]);
  for I := 0 to UVs.Count - 1 do
    TexCoordNode.FdPoint.Items.Add(Vector2Single(
      MapRange(UVs[I][0], 0, 1, UVRangeX[0], UVRangeX[1]),
      MapRange(UVs[I][1], 1, 0, UVRangeY[0], UVRangeY[1])
    ));
  Faces.FdTexCoord.Value := TexCoordNode;
end;

{ TMeshAttachment ------------------------------------------------------------ }

procedure TMeshAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  inherited;
  ReadVector2SingleList(Json, 'vertices', Vertices);
  if UVs.Count <> Vertices.Count then
  begin
    WritelnWarning('Spine', Format('uvs and vertices lists have different number of items, respectively %d and %d, for mesh attachment %s',
      [UVs.Count, Vertices.Count, Name]));
    Vertices.Count := UVs.Count;
  end;
end;

{ TSkinnedMeshVertex --------------------------------------------------------- }

constructor TSkinnedMeshVertex.Create;
begin
  inherited;
  Bones := TSkinnedMeshVertexBoneList.Create;
end;

destructor TSkinnedMeshVertex.Destroy;
begin
  FreeAndNil(Bones);
  inherited;
end;

procedure TSkinnedMeshVertex.Parse(const JsonArray: TJSONArray;
  Index: Integer; const BonesCount: Cardinal; const AllBones: TBoneList);
var
  I, BoneIndex: Integer;
  B: TSkinnedMeshVertexBone;
begin
  for I := 0 to Integer(BonesCount) - 1 do
  begin
    BoneIndex := JsonArray[Index].AsInteger; Inc(Index);
    if not Between(BoneIndex, 0, AllBones.Count - 1) then
      raise Exception.CreateFmt('Invalid bone index in skinnedmesh.vertices: %d', [BoneIndex]);
    B := TSkinnedMeshVertexBone.Create;
    Bones.Add(B);
    B.Bone := AllBones[BoneIndex];
    B.V[0] := JsonArray[Index].AsFloat; Inc(Index);
    B.V[1] := JsonArray[Index].AsFloat; Inc(Index);
    B.Weight := JsonArray[Index].AsFloat; Inc(Index);
  end;
end;

{ TSkinnedMeshVertexList ----------------------------------------------------- }

procedure TSkinnedMeshVertexList.Parse(const JsonArray: TJSONArray; const Bones: TBoneList);
var
  BonesCount: Cardinal;
  V: TSkinnedMeshVertex;
  I: Integer;
begin
  I := 0;
  while I < JsonArray.Count do
  begin
    BonesCount := Max(0, JsonArray[I].AsInteger);
    if I + BonesCount * 4 < JsonArray.Count then
    begin
      if BonesCount = 0 then
        WritelnWarning('Spine', 'skinnedmesh bonesCount is <= 0');
      V := TSkinnedMeshVertex.Create;
      V.Parse(JsonArray, I + 1, BonesCount, Bones);
      Add(V);
    end else
    begin
      WritelnWarning('Spine', Format('skinnedmesh weights (called "vertices" in JSON) do not have enough items (last bonesCount = %d, but array does not have enough floats)',
        [BonesCount]));
      Break;
    end;

    I += 1 + BonesCount * 4;
  end;
end;

{ TSkinnedMeshAttachment ----------------------------------------------------- }

constructor TSkinnedMeshAttachment.Create;
begin
  inherited;
  Weights := TSkinnedMeshVertexList.Create;
end;

destructor TSkinnedMeshAttachment.Destroy;
begin
  FreeAndNil(Weights);
  inherited;
end;

procedure TSkinnedMeshAttachment.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  JsonArray: TJSONArray;
  I, J: Integer;
  VB: TSkinnedMeshVertexBone;
  V: TVector2Single;
begin
  inherited;
  JsonArray := Json.Find('vertices', jtArray) as TJSONArray;
  if JsonArray <> nil then
    Weights.Parse(JsonArray, Bones);

  if UVs.Count <> Weights.Count then
  begin
    WritelnWarning('Spine', Format('skinnedmesh weights (called "vertices" in JSON) and uvs have different number of items, respectively %d and %d, for skinnedmesh attachment %s',
      [UVs.Count, Vertices.Count, Name]));
    Exit;
  end;

  { calculate initial Vertices from Weights.
    Similar to ComputeWorldVertices in spine-runtimes/spine-csharp/src/Attachments/SkinnedMeshAttachment.cs }
  Vertices.Count := Weights.Count;
  for I := 0 to Weights.Count - 1 do
  begin
    V := ZeroVector2Single;
    for J := 0 to Weights[I].Bones.Count - 1 do
    begin
      VB := Weights[I].Bones[J];
      V += VB.Bone.ToWorldSpace(VB.V) * VB.Weight;
    end;
    Vertices[I] := V;
  end;
end;

procedure TSkinnedMeshAttachment.TransformToBoneSpace(const Bone: TBone);
var
  List: TVector3SingleList;
  I: Integer;
begin
  inherited;

  List := Coord.FdPoint.Items;
  for I := 0 to List.Count - 1 do
    List.L[I] := Bone.ToLocalSpace(List.L[I]);
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
