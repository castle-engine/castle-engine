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

{ Spine skeleton, corresponding to a single Spine JSON file. }

{$ifdef read_interface}
  TSkeleton = class
    Root: TBone;
    { Bones, in the order specified in JSON file, so it's guaranteed
      that parent is always specified before any of it's children
      (this is very comfortable for processing). }
    Bones: TBoneList;
    Slots: TSlotList;
    Skins: TSkinList;
    Animations: TAnimationList;
    { When parsing the skeleton, we calculate default and current skin
      (used to search for attachments). They cannot be @nil.
      It is possible that CurrentSkin = DefaultSkin.
      @groupBegin }
    DefaultSkin: TSkin;
    CurrentSkin: TSkin;
    { @groupEnd }
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const BaseUrl: String; const Json: TJSONData; const SkinName: String);
    procedure BuildNodes(const BaseUrl: String; const TextureLoader: TTextureLoader;
      const Container: TX3DRootNode);
  end;
{$endif}

{$ifdef read_implementation}
constructor TSkeleton.Create;
begin
  inherited;
  Bones := TBoneList.Create;
  Slots := TSlotList.Create;
  Skins := TSkinList.Create;
  Animations := TAnimationList.Create;
end;

destructor TSkeleton.Destroy;
begin
  DefaultSkin := nil;
  FreeAndNil(Skins);
  FreeAndNil(Slots);
  Root := nil;
  FreeAndNil(Bones);
  FreeAndNil(Animations);
  inherited;
end;

procedure TSkeleton.Parse(const BaseUrl: String; const Json: TJSONData; const SkinName: String);

  procedure CalculateCurrentSkin;
  begin
    CurrentSkin := nil;
    if SkinName <> '' then
      CurrentSkin := Skins.Find(SkinName, true);
    if CurrentSkin = nil then
    begin
      { Old version:
        prefer non-default skin, if exists, since default skin may miss some
        attachments, see goblins.json example. }
      // if Skins.Count > 1 then
      //   CurrentSkin := Skins[1] else
      { New version: use DefaultSkin by default, this is more intuitive
        and naturally expected
        (see e.g. hero default/girl skins in "The Unholy Society"). }
      CurrentSkin := DefaultSkin;
    end;
  end;

  procedure SkinLog;
  var
    S: String;
    I: Integer;
  begin
    if Skins.Count = 1 then
    begin
      Assert(CurrentSkin = Skins[0]);
      //WritelnLog('Spine', BaseUrl + ' available skins: 1 (' + CurrentSkin.Name + ').');
    end else
    begin
      S := Format('%s available skins: %d', [BaseUrl, Skins.Count]) + NL;
      for I := 0 to Skins.Count - 1 do
        S := S + (Format('  Available skin %d: %s', [I, Skins[I].Name]) + NL);
      S := S + Format('Using skin: %s', [CurrentSkin.Name]);
      WritelnLogMultiline('Spine', S);
    end;
  end;

var
  O: TJSONObject;
  JsonArmatureArray: TJSONArray;
  DragonBonesFormat: Boolean;
begin
  if not (Json is TJSONObject) then
    raise ESpineReadError.Create('Spine JSON skeleton: Expected JSONObject at root');
  O := TJSONObject(Json);

  DragonBonesFormat :=
    (O.Find('armature', jtArray) <> nil) and
    (O.Find('skeleton', jtObject) = nil);

  if DragonBonesFormat then
  begin
    { In Dragon Bones, almost everything is inside "armature".
      TODO: For now, we only read 1st armature item. }
    JsonArmatureArray := O.Find('armature', jtArray) as TJSONArray;
    if JsonArmatureArray.Count = 0 then
      raise ESpineReadError.Create('Dragon Bones JSON skeleton: no armatures');
    O := JsonArmatureArray[0] as TJSONObject;
  end;

  Bones.Parse(O, Root, DragonBonesFormat);
  Slots.Parse(O, Bones, DragonBonesFormat);
  Skins.Parse(O, DefaultSkin, Bones, DragonBonesFormat);

  CalculateCurrentSkin; // Animations.Parse below requires CurrentSkin initialized
  // This is possible in case of empty Spine file
  if CurrentSkin = nil then
  begin
    Assert(DefaultSkin = nil);
    Exit;
  end;
  SkinLog;

  Animations.Parse(O, Bones, Slots, CurrentSkin);

  // WritelnLog('Spine', Format('Skeleton read, bones: %d, slots: %d, skins: %d, animations: %d',
  //   [Bones.Count, Slots.Count, Skins.Count, Animations.Count]));
end;

procedure TSkeleton.BuildNodes(const BaseUrl: String; const TextureLoader: TTextureLoader;
  const Container: TX3DRootNode);
var
  NavigationInfo: TNavigationInfoNode;
begin
  // This is possible in case of empty Spine
  if DefaultSkin = nil then Exit;

  { add NavigationInfo indicating blendingSort = 2D }
  NavigationInfo := TNavigationInfoNode.Create('', BaseUrl);
  NavigationInfo.BlendingSort := obs2D;
  Container.AddChildren(NavigationInfo);

  Bones.BuildNodes(BaseUrl);
  Root.NodeUsedAsChild := true;
  Container.AddChildren(Root.Node);
  Skins.BuildNodes(BaseUrl, TextureLoader);
  Slots.BuildNodes(BaseUrl, CurrentSkin.Attachments, DefaultSkin.Attachments);
  Animations.BuildNodes(BaseUrl, Container);
end;
{$endif}
