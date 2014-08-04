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

uses SysUtils, Classes, FGL, FpJson, JSONParser,
  CastleVectors, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils, CastleColors;

type
  ESpineReadError = class(Exception);

{ Atlas ---------------------------------------------------------------------- }

type
  TAtlasRegion = class
  public
    Name: string;
    Rotate: boolean;
    XY, Size, Orig, Offset: TVector2Integer;
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
    constructor Create;
    destructor Destroy; override;
  end;

  TAtlasPageList = specialize TFPGObjectList<TAtlasPage>;

  TAtlas = class
    Pages: TAtlasPageList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TAtlasPage.Create;
begin
  inherited;
  Regions := TAtlasRegionList.Create;
end;

destructor TAtlasPage.Destroy;
begin
  FreeAndNil(Regions);
  inherited;
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

{ Read .atlas file as produced by Spine, in format of libgdx, see
  https://github.com/libgdx/libgdx/wiki/Texture-packer }
function ReadAtlas(const AtlasURL: string): TAtlas;

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
  Reader := TTextReader.Create(AtlasURL);
  try
    Result := TAtlas.Create;
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
            Result.Pages.Add(Page);
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
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(Reader) end;
end;

{ JSON skeleton -------------------------------------------------------------- }

type
  TBoneList = class;

  TBone = class
    Name: string;
    Length: Single;
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Parent: TBone;
    procedure Parse(const Json: TJSONObject;
      const PossibleParents: TBoneList; const ExpectedParent: boolean);
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
  end;

  TSlotList = specialize TFPGObjectList<TSlot>;

  TAttachment = class
    { SlotName and AttachmentName are map keys, to detect which TAttachment to use. }
    SlotName, AttachmentName: string;
    { The real attachment name, unique for a skeleton, for image attachment
      this refers to atlas region name. }
    Name: string;
    procedure Parse(const Json: TJSONObject); virtual;
    { Create and parse correct TAttachment descendant. }
    class function CreateAndParse(const Json: TJSONObject;
      const ASlotName, AnAttachmentName: string): TAttachment;
  end;

  TRegionAttachment = class(TAttachment)
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Width, Height: Integer;
    procedure Parse(const Json: TJSONObject); override;
  end;

  TAttachmentList = class(specialize TFPGObjectList<TAttachment>)
    { Find by name.
      @raises ESpineReadError If does not exist. }
    function Find(const Name: string): TAttachment;
  end;

  TSkin = class
    Attachments: TAttachmentList;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject);
  end;

  TSkinList = class(specialize TFPGObjectList<TSkin>)
    { Find by name.
      @raises ESpineReadError If does not exist. }
    function Find(const Name: string): TSkin;
  end;

  TSkeleton = class
    Root: TBone;
    Bones: TBoneList;
    Slots: TSlotList;
    Skins: TSkinList;
    DefaultSkin: TSkin;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONData);
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
  Scale[0] := Json.Get('scaleX', 0.0);
  Scale[1] := Json.Get('scaleY', 0.0);
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

function TAttachmentList.Find(const Name: string): TAttachment;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Attachment name "%s" not found', [Name]);
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

procedure TRegionAttachment.Parse(const Json: TJSONObject);
begin
  inherited;
  XY[0] := Json.Get('x', 0.0);
  XY[1] := Json.Get('y', 0.0);
  Scale[0] := Json.Get('scaleX', 0.0);
  Scale[1] := Json.Get('scaleY', 0.0);
  Rotation := Json.Get('rotation', 0.0);
  Width := Json.Get('width', 0);
  Height := Json.Get('height', 0);
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
end;

{ Main loading function ------------------------------------------------------ }

function LoadSpine(const URL: string): TX3DRootNode;
var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  AtlasURL: string;
  Atlas: TAtlas;
  Skeleton: TSkeleton;
begin
  AtlasURL := ChangeURIExt(URL, '.atlas');
  if URIFileExists(AtlasURL) then
  begin
    Atlas := ReadAtlas(AtlasURL);
    try
      WritelnLog('Spine', Format('Atlas read, pages: %d', [Atlas.Pages.Count]));
    finally FreeAndNil(Atlas) end;
  end;

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
            WritelnLogMultiline('Spine', 'Returned JSON structure: ' +
              Json.ClassName + NL + Json.AsJSON);
            Skeleton := TSkeleton.Create;
            try
              Skeleton.Parse(Json);
              WritelnLog('Spine', Format('Skeleton read, bones: %d, slots: %d, skins: %d',
                [Skeleton.Bones.Count,
                 Skeleton.Slots.Count,
                 Skeleton.Skins.Count]));
            finally FreeAndNil(Skeleton) end;
          end;
        except FreeAndNil(Result); raise end;
      finally FreeAndNil(Json) end;
    finally FreeAndNil(P) end;
  finally FreeAndNil(S) end;
end;

end.
