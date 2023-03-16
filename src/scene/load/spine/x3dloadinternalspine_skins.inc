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

  TSkinList = class({$ifdef FPC}specialize{$endif} TObjectList<TSkin>)
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
    OneSkinObject, SkinsObject: TJSONObject;
    SkinsData: TJSONData;
    SkinsArray: TJSONArray;
  begin
    SkinsData := Json.Find('skins');
    if SkinsData = nil then
    begin
      WritelnWarning('Spine JSON skeleton', 'Missing "skins" data, Spine JSON is empty');
      Exit;
    end;

    if SkinsData is TJSONObject then
    begin
      // older Spine (at least <= 3.6.53) has "skins" as object, with skin names as keys
      SkinsObject := TJSONObject(SkinsData);

      for I := 0 to SkinsObject.Count - 1 do
        if SkinsObject.Items[I] is TJSONObject then
        begin
          OneSkinObject := TJSONObject(SkinsObject.Items[I]);
          Skin := TSkin.Create;
          Add(Skin);
          Skin.Name := SkinsObject.Names[I];
          Skin.Parse(OneSkinObject, Bones, DragonBonesFormat);

          if Skin.Name = 'default' then
            DefaultSkin := Skin;
        end;
    end else
    if SkinsData is TJSONArray then
    begin
      // newer Spine (at least >= 3.8.28-beta) has "skins" as array, quite like Dragon Bones
      SkinsArray := TJSONArray(SkinsData);

      for I := 0 to SkinsArray.Count - 1 do
      begin
        OneSkinObject := SkinsArray[I] as TJSONObject;
        Skin := TSkin.Create;
        Add(Skin);
        Skin.Name := OneSkinObject.Get('name', '');

        // Skin.Parse expects object with attachments inside
        OneSkinObject := OneSkinObject.Find('attachments', jtObject) as TJSONObject;
        if OneSkinObject = nil then
        begin
          WritelnWarning('Spine JSON skeleton', 'No "attachments" within skin "%s"', [Skin.Name]);
          Continue;
        end;

        Skin.Parse(OneSkinObject, Bones, DragonBonesFormat);

        if Skin.Name = 'default' then
          DefaultSkin := Skin;
      end;
    end else
    begin
      WritelnWarning('Spine JSON skeleton', '"skins" data is neither object nor array, not supported');
    end;
  end;

  procedure ParseDragonBones;
  var
    I: Integer;
    Skin: TSkin;
    OneSkinObject: TJSONObject;
    SkinsArray: TJSONArray;
  begin
    SkinsArray := Json.Find('skin', jtArray) as TJSONArray;
    if SkinsArray = nil then
      raise ESpineReadError.Create('Dragon Bones JSON skeleton: Missing "skin" array');

    for I := 0 to SkinsArray.Count - 1 do
    begin
      OneSkinObject := SkinsArray[I] as TJSONObject;
      Skin := TSkin.Create;
      Add(Skin);
      Skin.Name := OneSkinObject.Get('name', '');
      Skin.Parse(OneSkinObject, Bones, DragonBonesFormat);

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
