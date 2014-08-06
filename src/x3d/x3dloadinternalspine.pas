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

{ Spine 2D animations loader. }
unit X3DLoadInternalSpine;

interface

uses X3DNodes;

function LoadSpine(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, FGL, FpJson, JSONParser, Math,
  CastleVectors, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils, CastleColors,
  X3DLoadInternalUtils, CastleWarnings;

type
  ESpineReadError = class(Exception);

{ Atlas ---------------------------------------------------------------------- }

type
  TAtlasRegion = class
  public
    Name: string;
    Rotate: boolean;
    XY, Size, Orig, Offset: TVector2Integer;
    { Like XY and Size, but scaled to be within [0..1] range of the texture }
    TextureXY, TextureSize: TVector2Single;
    Index: Integer;
  end;

  TAtlasRegionList = specialize TFPGObjectList<TAtlasRegion>;

  TAtlasPage = class
  public
    TextureURL: string;
    Format: string;
    Filter: string; //< a value allowed by TextureProperties.MinificationFilter and MagnificationFilter
    IsRepeat: boolean;
    Regions: TAtlasRegionList;
    Node: TAppearanceNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BuildNodes(const BaseUrl: string);
  end;

  TAtlasPageList = specialize TFPGObjectList<TAtlasPage>;

  TAtlas = class
    Pages: TAtlasPageList;
    constructor Create;
    destructor Destroy; override;
    { Read .atlas file as produced by Spine, in format of libgdx, see
      https://github.com/libgdx/libgdx/wiki/Texture-packer }
    procedure Parse(const URL: string);
    procedure BuildNodes(const BaseUrl: string);
    { Find atlas page and region corresponding to given region name.
      @raises ESpineReadError If bone does not exist. }
    procedure Find(const RegionName: string; out Page: TAtlasPage; out Region: TAtlasRegion);
  end;

constructor TAtlasPage.Create;
begin
  inherited;
  Regions := TAtlasRegionList.Create;
end;

destructor TAtlasPage.Destroy;
begin
  FreeAndNil(Regions);
  if NodeUsedAsChild then
    { in case NodeUsedAsChild, don't even try FreeIfUnusedAndNil,
      as the check "is it unused" may already cause access violation
      since it may be already freed by freeing parent. }
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TAtlasPage.BuildNodes(const BaseUrl: string);
var
  Texture: TImageTextureNode;
  Region: TAtlasRegion;
  I: Integer;
begin
  { Create Appearance, with Texture child.
    Leave Appearance.material empty, to display unlit. }

  Node := TAppearanceNode.Create('Page_' + ToX3DName(TextureURL), BaseUrl);

  Texture := TImageTextureNode.Create('', BaseUrl);
  Texture.FdUrl.Items.Add(TextureURL);
  Texture.RepeatS := IsRepeat;
  Texture.RepeatT := IsRepeat;
  Node.FdTexture.Value := Texture;

  { The only way to calculate TextureXY and TextureSize is to actually load the texture.
    We use the Texture node for this, this way loaded texture will be reused for
    actual model rendering. }
  Texture.IsTextureLoaded := true;
  if Texture.IsTextureImage then
  begin
    for I := 0 to Regions.Count - 1 do
    begin
      Region := Regions[I];
      Region.TextureXY := Vector2Single(
        Region.XY[0] / Texture.TextureImage.Width,
        Region.XY[1] / Texture.TextureImage.Height);
      Region.TextureSize := Vector2Single(
        Region.Size[0] / Texture.TextureImage.Width,
        Region.Size[1] / Texture.TextureImage.Height);
      Region.TextureXY[1] :=
        { flip top-bottom }
        1 - Region.TextureXY[1]
        { move corner to bottom }
        - Region.TextureSize[1];
    end;
  end else
    OnWarning(wtMajor, 'Spine', SysUtils.Format('Cannot load texture "%s", texture coordinates cannot be correctly calculated based on Spine atlas information',
      [TextureURL]));
end;

constructor TAtlas.Create;
begin
  inherited;
  Pages := TAtlasPageList.Create;
end;

destructor TAtlas.Destroy;
begin
  FreeAndNil(Pages);
  inherited;
end;

procedure TAtlas.Parse(const URL: string);

  { Split a Line divided by character Separator into two strings.
    Assumes that whitespace doesn't matter (so we trim it),
    and Name must not be empty. }
  function Split(const Line: string; const Separator: char;
    out Name, Value: string): boolean;
  var
    Index: Integer;
  begin
    Result := false;
    Index := Pos(Separator, Line);
    if Index <> 0 then
    begin
      Name := Trim(Copy(Line, 1, Index - 1));
      Value := Trim(SEnding(Line, Index + 1));
      if Name <> '' then
        Result := true;
    end;
  end;

  function IsNameValueString(const Line, Name: string; out Value: string): boolean;
  var
    N, V: string;
  begin
    Result := Split(Line, ':', N, V) and (N = Name);
    if Result then
      Value := V;
  end;

  function IsNameValueBoolean(const Line, Name: string; out Value: boolean): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'false' then
        Value := false else
      if ValueStr = 'true' then
        Value := true else
        raise ESpineReadError.CreateFmt('Invalid boolean value "%s"', [ValueStr]);
    end;
  end;

  function IsNameValueInteger(const Line, Name: string; out Value: Integer): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      try
        Value := StrToInt(ValueStr);
      except
        on E: EConvertError do
          raise ESpineReadError.CreateFmt('Invalid integer value "%s": %s', [ValueStr, E.Message]);
      end;
    end;
  end;

  function IsNameValueVector2Integer(const Line, Name: string; out Vector: TVector2Integer): boolean;
  var
    ValueStr, ValueStr0, ValueStr1: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if Split(ValueStr, ',', ValueStr0, ValueStr1) then
      try
        Vector[0] := StrToInt(ValueStr0);
        Vector[1] := StrToInt(ValueStr1);
      except
        on E: EConvertError do
          raise ESpineReadError.CreateFmt('Invalid integer value in vector of 2 integers "%s": %s', [ValueStr, E.Message]);
      end else
        raise ESpineReadError.CreateFmt('Cannot split a vector of 2 integers "%s" by a comma', [ValueStr]);
    end;
  end;

  function IsNameValueFilter(const Line, Name: string; out Filter: string): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'Linear,Linear' then
        Filter := 'AVG_PIXEL' else
      if ValueStr = 'Nearest,Nearest' then
        Filter := 'NEAREST_PIXEL' else
        raise ESpineReadError.CreateFmt('Unsupported filter mode "%s"', [ValueStr]);
    end;
  end;

  function IsNameValueRepeat(const Line, Name: string; out IsRepeat: boolean): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'none' then
        IsRepeat := false else
        { is there anything else allowed for repeat: field ? }
        raise ESpineReadError.CreateFmt('Unsupported repeat mode "%s"', [ValueStr]);
    end;
  end;

var
  Reader: TTextReader;
  Page: TAtlasPage;
  Region: TAtlasRegion;
  Line: string;
begin
  Page := nil;
  Region := nil;
  Reader := TTextReader.Create(URL);
  try
    while not Reader.Eof do
    begin
      Line := Reader.Readln;
      if Page = nil then
      begin
        { start atlas page }
        if Trim(Line) <> '' then
        begin
          Page := TAtlasPage.Create;
          Page.TextureURL := Trim(Line);
          Pages.Add(Page);
        end;
      end else
      if Trim(Line) = '' then
        { end atlas page }
        Page := nil else
      if Line[1] <> ' ' then
      begin
        { read per-page (but not per-region) info }
        Region := nil;
        if IsNameValueString(Line, 'format', Page.Format) then else
        if IsNameValueFilter(Line, 'filter', Page.Filter) then else
        if IsNameValueRepeat(Line, 'repeat', Page.IsRepeat) then else
        if Pos(':', Line) <> 0 then
          raise ESpineReadError.CreateFmt('Unhandled name:value pair "%s"', [Line]) else
        begin
          { new region }
          Region := TAtlasRegion.Create;
          Region.Name := Line;
          Region.TextureXY := Vector2Single(0, 0);
          Region.TextureSize := Vector2Single(1, 1); // more sensible default than just (0,0)
          Page.Regions.Add(Region);
        end;
      end else
      if Region <> nil then
      begin
        { read per-region info }
        if IsNameValueBoolean(Line, 'rotate', Region.Rotate) then else
        if IsNameValueVector2Integer(Line, 'xy', Region.XY) then else
        if IsNameValueVector2Integer(Line, 'size', Region.Size) then else
        if IsNameValueVector2Integer(Line, 'orig', Region.Orig) then else
        if IsNameValueVector2Integer(Line, 'offset', Region.Offset) then else
        if IsNameValueInteger(Line, 'index', Region.Index) then else
          raise ESpineReadError.CreateFmt('Unhandled name:value pair "%s"', [Line]);
      end else
        raise ESpineReadError.Create('Atlas file contains indented line, but no region name specified');
    end;
  finally FreeAndNil(Reader) end;

  WritelnLog('Spine', Format('Atlas parsed, pages: %d', [Pages.Count]));
end;

procedure TAtlas.BuildNodes(const BaseUrl: string);
var
  I: Integer;
begin
  for I := 0 to Pages.Count - 1 do
    Pages[I].BuildNodes(BaseUrl);
end;

procedure TAtlas.Find(const RegionName: string;
  out Page: TAtlasPage; out Region: TAtlasRegion);
var
  I, J: Integer;
begin
  for I := 0 to Pages.Count - 1 do
  begin
    Page := Pages[I];
    for J := 0 to Page.Regions.Count - 1 do
    begin
      Region := Page.Regions[J];
      if Region.Name = RegionName then
        Exit;
    end;
  end;
  raise ESpineReadError.CreateFmt('Region name "%s" not found in atlas', [RegionName]);
end;

{ JSON skeleton -------------------------------------------------------------- }

type
  TBoneList = class;
  TAttachmentList = class;

  TBone = class
    Name: string;
    Length: Single;
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Parent: TBone;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const PossibleParents: TBoneList; const ExpectedParent: boolean);
    procedure BuildNodes(const BaseUrl: string);
  end;

  TBoneList = class(specialize TFPGObjectList<TBone>)
    { Find bone by name.
      @raises ESpineReadError If bone does not exist. }
    function Find(const BoneName: string): TBone;
  end;

  TSlot = class
    Name: string;
    Bone: TBone;
    Color: TCastleColor;
    Attachment: string;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string; const Attachments: TAttachmentList);
  end;

  TSlotList = specialize TFPGObjectList<TSlot>;

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
    { Create and parse correct TAttachment descendant. }
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

  TAttachmentList = class(specialize TFPGObjectList<TAttachment>)
    { Find by slot+attachment name.
      @raises ESpineReadError If does not exist. }
    function Find(const SlotName, AttachmentName: string): TAttachment;
  end;

  TSkin = class
    Attachments: TAttachmentList;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject);
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
  end;

  TSkinList = class(specialize TFPGObjectList<TSkin>)
    { Find by name.
      @raises ESpineReadError If does not exist. }
    function Find(const Name: string): TSkin;
  end;

  TSkeleton = class
    Root: TBone;
    { Bones, in the order specified in JSON file, so it's guaranteed
      that parent is always specified before any of it's children
      (this is very comfortable for processing). }
    Bones: TBoneList;
    Slots: TSlotList;
    Skins: TSkinList;
    DefaultSkin: TSkin;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONData);
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
  end;

procedure TBone.Parse(const Json: TJSONObject;
  const PossibleParents: TBoneList; const ExpectedParent: boolean);
var
  ParentName: string;
begin
  Name := Json.Get('name', '');
  Length := Json.Get('length', 0.0);
  XY[0] := Json.Get('x', 0.0);
  XY[1] := Json.Get('y', 0.0);
  Scale[0] := Json.Get('scaleX', 1.0);
  Scale[1] := Json.Get('scaleY', 1.0);
  Rotation := Json.Get('rotation', 0.0);

  ParentName := Json.Get('parent', '');
  if ParentName <> '' then
    Parent := PossibleParents.Find(ParentName);

  if ExpectedParent then
  begin
    if Parent = nil then
      raise ESpineReadError.CreateFmt('Parent for bone "%s" expected, but not specified', [Name]);
  end else
  begin
    if Parent <> nil then
      raise ESpineReadError.CreateFmt('Parent for bone "%s" not expected, but specified', [Name]);
  end;
end;

destructor TBone.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TBone.BuildNodes(const BaseUrl: string);
begin
  Node := TTransformNode.Create('Bone_' + ToX3DName(Name), BaseUrl);
  Node.FdTranslation.Value := Vector3Single(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3Single(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4Single(0, 0, 1, DegToRad(Rotation));
  if Parent <> nil then
  begin
    NodeUsedAsChild := true;
    Parent.Node.FdChildren.Add(Node);
  end;
end;

function TBoneList.Find(const BoneName: string): TBone;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = BoneName then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Bone name "%s" not found', [BoneName]);
end;

procedure TSlot.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  Name := Json.Get('name', '');
  Bone := Bones.Find(Json.Get('bone', ''));
  Attachment := Json.Get('attachment', '');
  // TODO: Color :=
end;

procedure TSlot.BuildNodes(const BaseUrl: string; const Attachments: TAttachmentList);
var
  A: TAttachment;
begin
  { ignore empty attachment names, as http://esotericsoftware.com/spine-json-format
    says explicitly "Assume no attachment for the setup pose if omitted." }
  if Attachment <> '' then
  begin
    A := Attachments.Find(Name, Attachment);
    A.NodeUsedAsChild := true;
    Bone.Node.FdChildren.Add(A.Node);
  end;
end;

function TAttachmentList.Find(const SlotName, AttachmentName: string): TAttachment;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I].SlotName = SlotName) and
       (Items[I].AttachmentName = AttachmentName) then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Attachment values for slot "%s" and attachment name "%s" not found',
    [SlotName, AttachmentName]);
end;

class function TAttachment.CreateAndParse(const Json: TJSONObject;
  const ASlotName, AnAttachmentName: string): TAttachment;
var
  TypeName: string;
begin
  TypeName := Json.Get('type', 'region');
  if TypeName = 'region' then
  begin
    Result := TRegionAttachment.Create;
    Result.SlotName := ASlotName;
    Result.AttachmentName := AnAttachmentName;
    Result.Parse(Json);
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
  Coord.FdPoint.Items.Add(Vector3Single(    0,      0, 0));
  Coord.FdPoint.Items.Add(Vector3Single(Width,      0, 0));
  Coord.FdPoint.Items.Add(Vector3Single(Width, Height, 0));
  Coord.FdPoint.Items.Add(Vector3Single(    0, Height, 0));
  Faces.FdCoord.Value := Coord;

  TexCoord := TTextureCoordinateNode.Create('', BaseUrl);
  TexCoord.FdPoint.Items.Add(Vector2Single(AtlasRegion.TextureXY[0]                             , AtlasRegion.TextureXY[1]                             ));
  TexCoord.FdPoint.Items.Add(Vector2Single(AtlasRegion.TextureXY[0] + AtlasRegion.TextureSize[0], AtlasRegion.TextureXY[1]                             ));
  TexCoord.FdPoint.Items.Add(Vector2Single(AtlasRegion.TextureXY[0] + AtlasRegion.TextureSize[0], AtlasRegion.TextureXY[1] + AtlasRegion.TextureSize[1]));
  TexCoord.FdPoint.Items.Add(Vector2Single(AtlasRegion.TextureXY[0]                             , AtlasRegion.TextureXY[1] + AtlasRegion.TextureSize[1]));
  Faces.FdTexCoord.Value := TexCoord;
end;

constructor TSkin.Create;
begin
  inherited;
  Attachments := TAttachmentList.Create;
end;

destructor TSkin.Destroy;
begin
  FreeAndNil(Attachments);
  inherited;
end;

procedure TSkin.Parse(const Json: TJSONObject);

  procedure ParseSlotMap(const Json: TJSONObject; const SlotName: string);
  var
    I: Integer;
    Attachment: TAttachment;
  begin
    for I := 0 to Json.Count - 1 do
      if Json.Items[I] is TJSONObject then
      begin
        Attachment := TAttachment.CreateAndParse(
          TJSONObject(Json.Items[I]), SlotName, Json.Names[I]);
        Attachments.Add(Attachment);
      end;
  end;

var
  I: Integer;
begin
  for I := 0 to Json.Count - 1 do
    if Json.Items[I] is TJSONObject then
      ParseSlotMap(TJSONObject(Json.Items[I]), Json.Names[I]);
end;

procedure TSkin.BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
var
  I: Integer;
begin
  for I := 0 to Attachments.Count - 1 do
    Attachments[I].BuildNodes(BaseUrl, Atlas);
end;

function TSkinList.Find(const Name: string): TSkin;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Skin name "%s" not found', [Name]);
end;

constructor TSkeleton.Create;
begin
  inherited;
  Bones := TBoneList.Create;
  Slots := TSlotList.Create;
  Skins := TSkinList.Create;
end;

destructor TSkeleton.Destroy;
begin
  DefaultSkin := nil;
  FreeAndNil(Skins);
  FreeAndNil(Slots);
  Root := nil;
  FreeAndNil(Bones);
  inherited;
end;

procedure TSkeleton.Parse(const Json: TJSONData);
var
  O: TJSONObject;

  procedure ReadBones;
  var
    I: Integer;
    Bone: TBone;
    ChildArray: TJSONArray;
  begin
    ChildArray := O.Find('bones', jtArray) as TJSONArray;
    if ChildArray = nil then
      raise ESpineReadError.Create('Spine JSON skeleton: Missing "bones" array');

    for I := 0 to ChildArray.Count - 1 do
      if ChildArray[I] is TJSONObject then
      begin
        Bone := TBone.Create;
        Bones.Add(Bone);
        if Root = nil then
        begin
          { reading Root bone. It should have no parent. }
          Bone.Parse(TJSONObject(ChildArray[I]), Bones, false);
          Root := Bone;
        end else
          { reading child bone. It must have a parent. }
          Bone.Parse(TJSONObject(ChildArray[I]), Bones, true);
      end;
  end;

  procedure ReadSlots;
  var
    I: Integer;
    Slot: TSlot;
    ChildArray: TJSONArray;
  begin
    ChildArray := O.Find('slots', jtArray) as TJSONArray;
    if ChildArray = nil then
      raise ESpineReadError.Create('Spine JSON skeleton: Missing "slots" array');

    for I := 0 to ChildArray.Count - 1 do
      if ChildArray[I] is TJSONObject then
      begin
        Slot := TSlot.Create;
        Slots.Add(Slot);
        Slot.Parse(TJSONObject(ChildArray[I]), Bones);
      end;
  end;

  procedure ReadSkins;
  var
    I: Integer;
    Skin: TSkin;
    ChildObj: TJSONObject;
  begin
    ChildObj := O.Find('skins', jtObject) as TJSONObject;
    if ChildObj = nil then
      raise ESpineReadError.Create('Spine JSON skeleton: Missing "skins" object');

    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        Skin := TSkin.Create;
        Skins.Add(Skin);
        Skin.Name := ChildObj.Names[I];
        Skin.Parse(TJSONObject(ChildObj.Items[I]));

        if Skin.Name = 'default' then
          DefaultSkin := Skin;
      end;

    if DefaultSkin = nil then
      raise ESpineReadError.Create('Spine JSON skeleton: Missing "default" skin definition');
  end;

begin
  if not (Json is TJSONObject) then
    raise ESpineReadError.Create('Spine JSON skeleton: Expected JSONObject at root');

  O := TJSONObject(Json);

  ReadBones;
  ReadSlots;
  ReadSkins;

  WritelnLog('Spine', Format('Skeleton read, bones: %d, slots: %d, skins: %d',
    [Bones.Count, Slots.Count, Skins.Count]));
end;

procedure TSkeleton.BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
var
  I: Integer;
begin
  for I := 0 to Bones.Count - 1 do
    Bones[I].BuildNodes(BaseUrl);
  for I := 0 to Skins.Count - 1 do
    Skins[I].BuildNodes(BaseUrl, Atlas);
  { TODO: support non-default skins. Slots should look into the current skin first. }
  for I := 0 to Slots.Count - 1 do
    Slots[I].BuildNodes(BaseUrl, DefaultSkin.Attachments);
end;

{ Main loading function ------------------------------------------------------ }

function LoadSpine(const URL: string): TX3DRootNode;
var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  Atlas: TAtlas;
  Skeleton: TSkeleton;
begin
  Atlas := TAtlas.Create;
  try
    Atlas.Parse(ChangeURIExt(URL, '.atlas'));
    Atlas.BuildNodes(URL);

    S := Download(URL);
    try
      P := TJSONParser.Create(S);
      try
        Json := P.Parse;
        try
          Result := TX3DRootNode.Create('', URL);
          try
            if Assigned(Json) then
            begin
              Skeleton := TSkeleton.Create;
              try
                Skeleton.Parse(Json);
                Skeleton.BuildNodes(URL, Atlas);
                Result.FdChildren.Add(Skeleton.Root.Node);
              finally FreeAndNil(Skeleton) end;
            end;
          except FreeAndNil(Result); raise end;
        finally FreeAndNil(Json) end;
      finally FreeAndNil(P) end;
    finally FreeAndNil(S) end;
  finally FreeAndNil(Atlas) end;
end;

end.
