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
    { After parsing skins, we calculate default and current skin
      (used to search for attachments). They cannot be @nil.
      It is possible that CurrentSkin = DefaultSkin.
      For now, CurrentSkin remains local in BuildNodes, no need to make
      it public.
      @groupBegin }
    DefaultSkin: TSkin;
    { @groupEnd }
    { List of bones/slots (all present also on TSkeleton.Bones/Slots list)
      that have their translation/rotation/scale (in case of bones)
      or attachment/color (in case of slots) modified by @italic(any) animation.
      This is useful to gather, as this is the minimal set of things
      reset to bring back model to initial (pose) state.
      Calculated during TSkeleton.Parse, used during TSkeleton.BuildNodes.
      @groupBegin }
    BonesAnimated: TBonesAnimated;
    SlotsAnimated: TSlotsAnimated;
    { @groupEnd }
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONData);
    procedure BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader;
      const Container: TX3DRootNode; const SkinName: string);
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
  BonesAnimated := TBonesAnimated.Create;
  SlotsAnimated := TSlotsAnimated.Create;
end;

destructor TSkeleton.Destroy;
begin
  DefaultSkin := nil;
  FreeAndNil(Skins);
  FreeAndNil(Slots);
  Root := nil;
  FreeAndNil(Bones);
  FreeAndNil(Animations);
  FreeAndNil(BonesAnimated);
  FreeAndNil(SlotsAnimated);
  inherited;
end;

procedure TSkeleton.Parse(const Json: TJSONData);
var
  O: TJSONObject;
begin
  if not (Json is TJSONObject) then
    raise ESpineReadError.Create('Spine JSON skeleton: Expected JSONObject at root');
  O := TJSONObject(Json);

  Bones.Parse(O, Root);
  Slots.Parse(O, Bones);
  Skins.Parse(O, DefaultSkin, Bones);
  Animations.Parse(O, Bones, BonesAnimated, Slots, SlotsAnimated);

  // WritelnLog('Spine', Format('Skeleton read, bones: %d, slots: %d, skins: %d, animations: %d',
  //   [Bones.Count, Slots.Count, Skins.Count, Animations.Count]));
end;

procedure TSkeleton.BuildNodes(const BaseUrl: string; const TextureLoader: TTextureLoader;
  const Container: TX3DRootNode; const SkinName: string);
var
  CurrentSkin: TSkin;

  procedure SkinLog;
  var
    S: string;
    I: Integer;
  begin
    if Skins.Count = 1 then
    begin
      Assert(CurrentSkin = Skins[0]);
      //WritelnLog('Spine', BaseUrl + ' available skins: 1 (' + CurrentSkin.Name + ').');
    end else
    begin
      S := Format(BaseUrl + ' available skins: %d', [Skins.Count]) + NL;
      for I := 0 to Skins.Count - 1 do
        S += Format('  Available skin %d: %s', [I, Skins[I].Name]) + NL;
      S += Format('Using skin: %s', [CurrentSkin.Name]);
      WritelnLogMultiline('Spine', S);
    end;
  end;

var
  NavigationInfo: TNavigationInfoNode;
begin
  { add NavigationInfo indicating blendingSort = 2D }
  NavigationInfo := TNavigationInfoNode.Create('', BaseUrl);
  NavigationInfo.BlendingSort := obs2D;
  Container.FdChildren.Add(NavigationInfo);

  Bones.BuildNodes(BaseUrl, BonesAnimated);
  Root.NodeUsedAsChild := true;
  Container.FdChildren.Add(Root.Node);
  Skins.BuildNodes(BaseUrl, TextureLoader);

  CurrentSkin := nil;
  if SkinName <> '' then
    CurrentSkin := Skins.Find(SkinName, true);
  if CurrentSkin = nil then
  begin
    { prefer non-default skin, if exists, since default skin may miss some
      attachments, see goblins.json example }
    if Skins.Count > 1 then
      CurrentSkin := Skins[1] else
      CurrentSkin := DefaultSkin;
  end;
  if Log then
    SkinLog;

  Slots.BuildNodes(BaseUrl, SlotsAnimated, CurrentSkin.Attachments, DefaultSkin.Attachments, Container);
  Animations.BuildNodes(BaseUrl, Container, BonesAnimated, SlotsAnimated);
end;
{$endif}
