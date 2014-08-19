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

{ Spine animations. }

{$ifdef read_interface}
  TAnimation = class
    Name: string;
    BoneTimelines: TBoneTimelineList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string);
  end;

  TAnimationList = class(specialize TFPGObjectList<TAnimation>)
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string);
  end;
{$endif}

{$ifdef read_implementation}

{ TAnimation ----------------------------------------------------------------- }

constructor TAnimation.Create;
begin
  inherited;
  BoneTimelines := TBoneTimelineList.Create;
end;

destructor TAnimation.Destroy;
begin
  FreeAndNil(BoneTimelines);
  inherited;
end;

procedure TAnimation.Parse(const Json: TJSONObject; const Bones: TBoneList);

  procedure ReadBoneTimelines(const Bone: TBone; const Json: TJSONObject);
  var
    BoneTimeline: TBoneTimeline;
    ChildArray: TJSONArray;
  begin
    ChildArray := Json.Find('translate', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineTranslate.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      BoneTimelines.Add(BoneTimeline);
    end;

    ChildArray := Json.Find('scale', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineScale.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      BoneTimelines.Add(BoneTimeline);
    end;

    ChildArray := Json.Find('rotate', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineRotate.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      BoneTimelines.Add(BoneTimeline);
    end;
  end;

var
  I: Integer;
  Bone: TBone;
  ChildObj: TJSONObject;
begin
  ChildObj := Json.Find('bones', jtObject) as TJSONObject;
  if ChildObj <> nil then
  begin
    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        Bone := Bones.Find(ChildObj.Names[I]);
        ReadBoneTimelines(Bone, TJSONObject(ChildObj.Items[I]));
      end;
  end;

  { TODO: support other timelines, like slots, here }
end;

procedure TAnimation.BuildNodes(const BaseUrl: string);
begin
  { TODO }
end;

{ TAnimationList ------------------------------------------------------------- }

procedure TAnimationList.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  I: Integer;
  Animation: TAnimation;
  ChildObj: TJSONObject;
begin
  ChildObj := Json.Find('animations', jtObject) as TJSONObject;
  if ChildObj <> nil then
  begin
    { Note that we do not raise error when "animations" element is missing,
      it just means for us that there are no animations.
      Not sure whether Spine can actually output such file, though. }

    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        Animation := TAnimation.Create;
        Add(Animation);
        Animation.Name := ChildObj.Names[I];
        Animation.Parse(TJSONObject(ChildObj.Items[I]), Bones);
      end;
  end;
end;

procedure TAnimationList.BuildNodes(const BaseUrl: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl);
end;

{$endif}
