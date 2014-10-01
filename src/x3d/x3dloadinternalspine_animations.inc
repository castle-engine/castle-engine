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
    Node: TTimeSensorNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const BonesAnimated: TBonesAnimated);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode;
      const BonesAnimated: TBonesAnimated);
  end;

  TAnimationList = class(specialize TFPGObjectList<TAnimation>)
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const BonesAnimated: TBonesAnimated);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode;
      const BonesAnimated: TBonesAnimated);
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
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(BoneTimelines);
  inherited;
end;

procedure TAnimation.Parse(const Json: TJSONObject; const Bones: TBoneList;
  const BonesAnimated: TBonesAnimated);

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
      { Note: avoid duplicates on BonesAnimated. Our ToReset mechanism
        (used when building nodes) depends that there are no duplicates on
        these lists.
        And we can have multiple animations affecting the same bone,
        so without explicitly checking below to avoid duplicates,
        we would have trouble. }
      if BonesAnimated.Translation.IndexOf(Bone) = -1 then
        BonesAnimated.Translation.Add(Bone);
    end;

    ChildArray := Json.Find('scale', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineScale.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      BoneTimelines.Add(BoneTimeline);
      if BonesAnimated.Scale.IndexOf(Bone) = -1 then
        BonesAnimated.Scale.Add(Bone);
    end;

    ChildArray := Json.Find('rotate', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineRotate.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      BoneTimelines.Add(BoneTimeline);
      if BonesAnimated.Rotation.IndexOf(Bone) = -1 then
        BonesAnimated.Rotation.Add(Bone);
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

procedure TAnimation.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode; const BonesAnimated: TBonesAnimated);
var
  MaxTime: Single;
  I: Integer;
  Route: TX3DRoute;
  ToReset: TBonesAnimated;
begin
  Node := TTimeSensorNode.Create(AnimationPrefix + ToX3DName(Name), BaseUrl);

  MaxTime := 0;
  for I := 0 to BoneTimelines.Count - 1 do
    MaxTo1st(MaxTime, BoneTimelines[I].MaxTime);
  Node.FdCycleInterval.Value := MaxTime;

  NodeUsedAsChild := true;
  Container.FdChildren.Add(Node);

  ToReset := TBonesAnimated.Create;
  try
    ToReset.Assign(BonesAnimated);

    for I := 0 to BoneTimelines.Count - 1 do
    begin
      BoneTimelines[I].BuildNodes(BaseUrl, MaxTime, Container, ToReset);
      { add animation name to bone timeline, to make it unique in case
        we have many animations }
      BoneTimelines[I].Node.NodeName := Node.NodeName + '_' + BoneTimelines[I].Node.NodeName;

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(BoneTimelines[I].Node.EventSet_fraction);
      Container.Routes.Add(Route);
    end;

    for I := 0 to ToReset.Translation.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(ToReset.Translation[I].ResetTranslation.EventSet_fraction);
      Container.Routes.Add(Route);
    end;

    for I := 0 to ToReset.Scale.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(ToReset.Scale[I].ResetScale.EventSet_fraction);
      Container.Routes.Add(Route);
    end;

    for I := 0 to ToReset.Rotation.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(ToReset.Rotation[I].ResetRotation.EventSet_fraction);
      Container.Routes.Add(Route);
    end;
  finally FreeAndNil(ToReset) end;
end;

{ TAnimationList ------------------------------------------------------------- }

procedure TAnimationList.Parse(const Json: TJSONObject; const Bones: TBoneList;
  const BonesAnimated: TBonesAnimated);
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
        Animation.Parse(TJSONObject(ChildObj.Items[I]), Bones, BonesAnimated);
      end;
  end;
end;

procedure TAnimationList.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode; const BonesAnimated: TBonesAnimated);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, Container, BonesAnimated);
end;

{$endif}
