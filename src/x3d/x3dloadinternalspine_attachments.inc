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
      this refers to atlas region name. }
    Name: string;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject); virtual;
    { Create and parse correct TAttachment descendant.
      May return @nil if none possible. }
    class function CreateAndParse(const Json: TJSONObject;
      const ASlotName, AnAttachmentName: string): TAttachment;
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas); virtual; abstract;
  end;

  TRegionAttachment = class(TAttachment)
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Width, Height: Integer;
    procedure Parse(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas); override;
  end;

  TMeshAttachment = class(TRegionAttachment)
    UVs: TVector2SingleList;
    Triangles: TLongIntList;
    Vertices: TVector3SingleList;
    Edges: TLongIntList; //< unused for now
    Hull: Integer; //< unused for now
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas); override;
  end;

  TAttachmentList = class(specialize TFPGObjectList<TAttachment>)
    { Find by slot+attachment name.
      @raises ESpineReadError If does not exist. }
    function Find(const SlotName, AttachmentName: string;
      const FallbackList: TAttachmentList): TAttachment;
  end;
{$endif}

{$ifdef read_implementation}
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
  end else
    raise ESpineReadError.Create('Only attachment type "region" supported now');
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

procedure TRegionAttachment.BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
var
  AtlasPage: TAtlasPage;
  AtlasRegion: TAtlasRegion;
  Coord: TCoordinateNode;
  Faces: TIndexedFaceSetNode;
  TexCoord: TTextureCoordinateNode;
  Shape: TShapeNode;
  I: Integer;
begin
  Node := TTransformNode.Create('Attachment_' + ToX3DName(Name), BaseUrl);
  Node.FdTranslation.Value := Vector3Single(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3Single(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4Single(0, 0, 1, DegToRad(Rotation));

  Shape := TShapeNode.Create('', BaseUrl);
  Node.FdChildren.Add(Shape);

  Atlas.Find(Name, AtlasPage, AtlasRegion);
  Shape.Appearance := AtlasPage.Node;
  AtlasPage.NodeUsedAsChild := true;

  Faces := TIndexedFaceSetNode.Create('', BaseUrl);
  Faces.FdCreaseAngle.Value := 0; // optimization: do not try to smooth normals, no point
  Faces.FdSolid.Value := false;
  Faces.FdCoordIndex.Items.Clear;
  Faces.FdCoordIndex.Items.Add(0);
  Faces.FdCoordIndex.Items.Add(1);
  Faces.FdCoordIndex.Items.Add(2);
  Faces.FdCoordIndex.Items.Add(3);
  Shape.FdGeometry.Value := Faces;

  Coord := TCoordinateNode.Create('', BaseUrl);
  Coord.FdPoint.Items.Add(Vector3Single(-Width / 2,-Height / 2, 0));
  Coord.FdPoint.Items.Add(Vector3Single( Width / 2,-Height / 2, 0));
  Coord.FdPoint.Items.Add(Vector3Single( Width / 2, Height / 2, 0));
  Coord.FdPoint.Items.Add(Vector3Single(-Width / 2, Height / 2, 0));
  Faces.FdCoord.Value := Coord;

  TexCoord := TTextureCoordinateNode.Create('', BaseUrl);
  for I := 0 to 3 do
    TexCoord.FdPoint.Items.Add(AtlasRegion.TexCoord[I]);
  Faces.FdTexCoord.Value := TexCoord;
end;

{ TMeshAttachment ------------------------------------------------------------ }

constructor TMeshAttachment.Create;
begin
  inherited;
  UVs := TVector2SingleList.Create;
  Triangles := TLongIntList.Create;
  Vertices := TVector3SingleList.Create;
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
begin
  inherited;
end;

procedure TMeshAttachment.BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
begin
  inherited;
end;

{ TAttachmentList ------------------------------------------------------------ }

function TAttachmentList.Find(const SlotName, AttachmentName: string;
  const FallbackList: TAttachmentList): TAttachment;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I].SlotName = SlotName) and
       (Items[I].AttachmentName = AttachmentName) then
      Exit(Items[I]);

  if FallbackList <> nil then
    Result := FallbackList.Find(SlotName, AttachmentName, nil) else
    raise ESpineReadError.CreateFmt('Attachment values for slot "%s" and attachment name "%s" not found',
      [SlotName, AttachmentName]);
end;
{$endif}
