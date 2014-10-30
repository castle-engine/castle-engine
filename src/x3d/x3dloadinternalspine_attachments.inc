{
  Copyright 2014-2014 Michalis Kamburelis.

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
    { The real attachment name, unique for a skeleton, for image attachment
      this refers to texture loader (e.g. atlas) region name. }
    Name: string;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    Material: TMaterialNode; //< initialized together with Node, always inside
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject); virtual;
    { Create and parse correct TAttachment descendant.
      May return @nil if none possible. }
    class function CreateAndParse(const Json: TJSONObject;
      const ASlotName, AnAttachmentName: string): TAttachment;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); virtual; abstract;
  end;

  TRegionAttachment = class(TAttachment)
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Width, Height: Integer;
    procedure Parse(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader); override;
    function BuildGeometry(const BaseUrl: string;
      const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect): TAbstractGeometryNode; virtual;
  end;

  TMeshAttachment = class(TRegionAttachment)
    UVs: TVector2SingleList;
    Triangles: TLongIntList;
    Vertices: TVector2SingleList;
    Edges: TLongIntList; //< unused for now
    Hull: Integer; //< unused for now
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject); override;
    function BuildGeometry(const BaseUrl: string;
      const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect): TAbstractGeometryNode; override;
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
  const ASlotName, AnAttachmentName: string): TAttachment;
var
  TypeName: string;
begin
  TypeName := Json.Get('type', 'region');
  if (TypeName = 'mesh') or
     (TypeName = 'skinnedmesh') then
  begin
    if TypeName = 'regionsequence' then
      OnWarning(wtMinor, 'Spine', 'Spine region type "skinnedmesh" is for now rendered just like normal "mesh"');
    Result := TMeshAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json);
  end else
  if (TypeName = 'region') or
     (TypeName = 'regionsequence') then
  begin
    if TypeName = 'regionsequence' then
      OnWarning(wtMinor, 'Spine', 'Spine region type "regionsequence" is for now rendered just like normal "region"');
    Result := TRegionAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json);
  end else
  if TypeName = 'boundingbox' then
  begin
    OnWarning(wtMinor, 'Spine', 'Spine region type "boundingbox" is ignored for now');
    Result := nil;
  end else
    raise ESpineReadError.CreateFmt('Attachment type "%s" not supported', [TypeName]);
end;

procedure TAttachment.Parse(const Json: TJSONObject);
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

{ TRegionAttachment ---------------------------------------------------------- }

procedure TRegionAttachment.Parse(const Json: TJSONObject);
begin
  inherited;
  XY[0] := Json.Get('x', 0.0);
  XY[1] := Json.Get('y', 0.0);
  Scale[0] := Json.Get('scaleX', 1.0);
  Scale[1] := Json.Get('scaleY', 1.0);
  Rotation := Json.Get('rotation', 0.0);
  Width := Json.Get('width', 0);
  Height := Json.Get('height', 0);
end;

procedure TRegionAttachment.BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader);
var
  TexRect: TQuadTexRect;
  TexCoord: TQuadTexCoord;
  Shape: TShapeNode;
begin
  Node := TTransformNode.Create('Attachment_' + ToX3DName(Name), BaseUrl);
  Node.FdTranslation.Value := Vector3Single(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3Single(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4Single(0, 0, 1, DegToRad(Rotation));

  Shape := TShapeNode.Create('', BaseUrl);
  Node.FdChildren.Add(Shape);

  Shape.Appearance := TAppearanceNode.Create('', BaseUrl);
  Shape.Appearance.FdTexture.Value := TextureLoader.UseNode(Name, TexCoord, TexRect);
  Shape.FdGeometry.Value := BuildGeometry(BaseUrl, TexCoord, TexRect);

  Shape.Material := TMaterialNode.Create('Material_' + ToX3DName(Name), BaseUrl);
  Shape.Material.FdDiffuseColor.Value := ZeroVector3Single;
  Shape.Material.FdSpecularColor.Value := ZeroVector3Single;
  Shape.Material.FdAmbientIntensity.Value := 0;
  Assert(Shape.Material.OnlyEmissiveMaterial);
  Shape.Material.FdEmissiveColor.Value := Vector3Single(1, 1, 1);
  Material := Shape.Material;
end;

function TRegionAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect): TAbstractGeometryNode;
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

{ TMeshAttachment ------------------------------------------------------------ }

constructor TMeshAttachment.Create;
begin
  inherited;
  UVs := TVector2SingleList.Create;
  Triangles := TLongIntList.Create;
  Vertices := TVector2SingleList.Create;
  Edges := TLongIntList.Create;
end;

destructor TMeshAttachment.Destroy;
begin
  FreeAndNil(UVs);
  FreeAndNil(Triangles);
  FreeAndNil(Vertices);
  FreeAndNil(Edges);
  inherited;
end;

procedure TMeshAttachment.Parse(const Json: TJSONObject);

  procedure ReadVector2SingleList(const Name: string; const List: TVector2SingleList);
  var
    JsonArray: TJSONArray;
    I: Integer;
    Vec2: TVector2Single;
    Vec2Index: Integer;
  begin
    JsonArray := Json.Find(Name, jtArray) as TJSONArray;
    if JsonArray <> nil then
    begin
      Vec2Index := 0;
      for I := 0 to JsonArray.Count - 1 do
        if JsonArray[I] is TJSONNumber then
        begin
          Vec2[Vec2Index] := JsonArray[I].AsFloat;
          Inc(Vec2Index);
          if Vec2Index = 2 then
          begin
            List.Add(Vec2);
            Vec2Index := 0;
          end;
        end;
      if Vec2Index <> 0 then
        OnWarning(wtMajor, 'Spine', 'Vector2 list (like uvs or vertices) ends in the middle of the vector');
    end;
  end;

  procedure ReadLongIntList(const Name: string; const List: TLongIntList);
  var
    JsonArray: TJSONArray;
    I: Integer;
  begin
    JsonArray := Json.Find(Name, jtArray) as TJSONArray;
    if JsonArray <> nil then
      for I := 0 to JsonArray.Count - 1 do
        if JsonArray[I] is TJSONIntegerNumber then
          List.Add(JsonArray[I].AsInteger);
  end;

begin
  inherited;

  ReadVector2SingleList('uvs', UVs);
  ReadVector2SingleList('vertices', Vertices);
  if UVs.Count <> Vertices.Count then
  begin
    OnWarning(wtMajor, 'Spine', Format('uvs and vertices lists have different number of items, respectively %d and %d, for attachment %s',
      [UVs.Count, Vertices.Count, Name]));
    { TODO: it is also normal for skinnedmesh... For now, cut down,
      although it seems incorrect? Or maybe it's ok for dagger? }
    Vertices.Count := UVs.Count;
  end;

  ReadLongIntList('triangles', Triangles);
  if Triangles.Count mod 3 <> 0 then
    OnWarning(wtMajor, 'Spine', 'Triangles list ends in the middle of the triangle');

  ReadLongIntList('edges', Edges);
  Hull := Json.Get('hull', 0);
end;

function TMeshAttachment.BuildGeometry(const BaseUrl: string;
  const TexCoord: TQuadTexCoord; const TexRect: TQuadTexRect): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
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
    TODO: see how other runtimes handle it?

    The rendering will be Ok only if the mesh faces used only the texture
    parts within the non-whitespace region (TexRect). Which is true
    in practice in all the examples. }

  if (TexRect[0][0] <> 0) or
     (TexRect[1][0] <> 0) or
     (TexRect[0][1] <> 1) or
     (TexRect[1][1] <> 1) then
    WritelnLog('Spine', Format('Slot "%s" with attachment "%s" uses "mesh" attachment type, and you exported atlas with "compress whitespace" option selected. Rendering artifacts may occur if mesh faces use the texture in "whitespace" region --- in such case, export again with "compress whitespace" turned off at atlas generation.',
      [SlotName, Name]));

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
