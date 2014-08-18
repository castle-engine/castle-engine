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
    DefaultSkin: TSkin;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONData);
    procedure BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
  end;
{$endif}

{$ifdef read_implementation}
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
begin
  if not (Json is TJSONObject) then
    raise ESpineReadError.Create('Spine JSON skeleton: Expected JSONObject at root');
  O := TJSONObject(Json);

  Bones.Parse(O, Root);
  Slots.Parse(O, Bones);
  Skins.Parse(O, DefaultSkin);

  WritelnLog('Spine', Format('Skeleton read, bones: %d, slots: %d, skins: %d',
    [Bones.Count, Slots.Count, Skins.Count]));
end;

procedure TSkeleton.BuildNodes(const BaseUrl: string; const Atlas: TAtlas);
var
  CurrentSkin: TSkin;
begin
  Bones.BuildNodes(BaseUrl);
  Skins.BuildNodes(BaseUrl, Atlas);

  { prefer non-default skin, if exists, since default skin may miss some
    attachments, see goblins.json example }
  if Skins.Count > 1 then
    CurrentSkin := Skins[1] else
    CurrentSkin := DefaultSkin;
  WritelnLog('Spine', 'Using skin "%s"', [CurrentSkin.Name]);

  Slots.BuildNodes(BaseUrl, CurrentSkin.Attachments, DefaultSkin.Attachments);
end;
{$endif}
