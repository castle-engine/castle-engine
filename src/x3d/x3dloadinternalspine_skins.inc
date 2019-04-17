{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine skins. }

{$ifdef read_interface}
  TSkin = class
    Attachments: TAttachmentList;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader);
  end;

  TSkinList = class(specialize TObjectList<TSkin>)
    { Find by name.
      @raises ESpineReadError If does not exist and NilOnError = @false. }
    function Find(const Name: string; const NilOnError: boolean): TSkin;
    procedure Parse(const Json: TJSONObject; var DefaultSkin: TSkin; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader);
  end;
{$endif}

{$ifdef read_implementation}

{ TSkin ---------------------------------------------------------------------- }

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

procedure TSkin.Parse(const Json: TJSONObject; const Bones: TBoneList;
  const DragonBonesFormat: boolean);

  procedure ParseSlotMap(const Json: TJSONObject; const SlotName: string);
  var
    I: Integer;
    Attachment: TAttachment;
  begin
    for I := 0 to Json.Count - 1 do
      if Json.Items[I] is TJSONObject then
      begin
        Attachment := TAttachment.CreateAndParse(
          TJSONObject(Json.Items[I]), SlotName, Json.Names[I], Bones);
        if Attachment <> nil then
          Attachments.Add(Attachment);
      end;
  end;

var
  I: Integer;
begin
  { TODO: this does not read skin in Dragon Bones version now. }

  for I := 0 to Json.Count - 1 do
    if Json.Items[I] is TJSONObject then
      ParseSlotMap(TJSONObject(Json.Items[I]), Json.Names[I]);
end;

procedure TSkin.BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader);
var
  I: Integer;
begin
  for I := 0 to Attachments.Count - 1 do
    Attachments[I].BuildNodes(BaseUrl, TextureLoader);
end;

{ TSkinList ------------------------------------------------------------------ }

function TSkinList.Find(const Name: string; const NilOnError: boolean): TSkin;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  if NilOnError then
  begin
    WritelnWarning('Spine', Format('Skin name "%s" not found', [Name]));
    Result := nil;
  end else
    raise ESpineReadError.CreateFmt('Skin name "%s" not found', [Name]);
end;

procedure TSkinList.Parse(const Json: TJSONObject; var DefaultSkin: TSkin; const Bones: TBoneList;
  const DragonBonesFormat: boolean);

  procedure ParseSpine;
  var
    I: Integer;
    Skin: TSkin;
    SkinObject, ChildObj: TJSONObject;
  begin
    ChildObj := Json.Find('skins', jtObject) as TJSONObject;
    if ChildObj = nil then
    begin
      WritelnWarning('Spine JSON skeleton', 'Missing "skins" object, Spine JSON is empty');
      Exit;
    end;

    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        SkinObject := TJSONObject(ChildObj.Items[I]);
        Skin := TSkin.Create;
        Add(Skin);
        Skin.Name := ChildObj.Names[I];
        Skin.Parse(SkinObject, Bones, DragonBonesFormat);

        if Skin.Name = 'default' then
          DefaultSkin := Skin;
      end;
  end;

  procedure ParseDragonBones;
  var
    I: Integer;
    Skin: TSkin;
    SkinObject: TJSONObject;
    ChildArray: TJSONArray;
  begin
    ChildArray := Json.Find('skin', jtArray) as TJSONArray;
    if ChildArray = nil then
      raise ESpineReadError.Create('Dragon Bones JSON skeleton: Missing "skin" array');

    for I := 0 to ChildArray.Count - 1 do
    begin
      SkinObject := TJSONObject(ChildArray[I]);
      Skin := TSkin.Create;
      Add(Skin);
      Skin.Name := SkinObject.Get('name', '');
      Skin.Parse(SkinObject, Bones, DragonBonesFormat);

      if DefaultSkin = nil then // just use 1st skin as default for now
        DefaultSkin := Skin;
    end;
  end;

begin
  if DragonBonesFormat then
    ParseDragonBones
  else
    ParseSpine;

  if DefaultSkin = nil then // DefaultSkin may be nil in case of empty model
    WritelnWarning('Spine JSON skeleton', 'Missing "default" skin definition, Spine JSON is empty');
end;

procedure TSkinList.BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, TextureLoader);
end;
{$endif}
