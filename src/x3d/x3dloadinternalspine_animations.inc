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

{ Spine animations. }

{$ifdef read_interface}
  TAnimation = class
    Name: string;
    BoneTimelines: TBoneTimelineList;
    SlotTimelines: TSlotTimelineList;
    DrawOrderTimelines: TDrawOrderTimelineList;
    Node: TTimeSensorNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const Bones: TBoneList; const BonesAnimated: TBonesAnimated;
      const Slots: TSlotList; const SlotsAnimated: TSlotsAnimated;
      var CurvesRemoved: Cardinal);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode;
      const BonesAnimated: TBonesAnimated; const SlotsAnimated: TSlotsAnimated);
  end;

  TAnimationList = class(specialize TFPGObjectList<TAnimation>)
    procedure Parse(const Json: TJSONObject;
      const Bones: TBoneList; const BonesAnimated: TBonesAnimated;
      const Slots: TSlotList; const SlotsAnimated: TSlotsAnimated);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode;
      const BonesAnimated: TBonesAnimated; const SlotsAnimated: TSlotsAnimated);
    { Add all TimeSensors to EXPORTed nodes. }
    procedure Exported(const RootNode: TX3DRootNode);
  end;
{$endif}

{$ifdef read_implementation}

{ TAnimation ----------------------------------------------------------------- }

constructor TAnimation.Create;
begin
  inherited;
  BoneTimelines := TBoneTimelineList.Create;
  SlotTimelines := TSlotTimelineList.Create;
  DrawOrderTimelines := TDrawOrderTimelineList.Create;
end;

destructor TAnimation.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(BoneTimelines);
  FreeAndNil(SlotTimelines);
  FreeAndNil(DrawOrderTimelines);
  inherited;
end;

procedure TAnimation.Parse(const Json: TJSONObject;
  const Bones: TBoneList; const BonesAnimated: TBonesAnimated;
  const Slots: TSlotList; const SlotsAnimated: TSlotsAnimated;
  var CurvesRemoved: Cardinal);

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
      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
        { Note: avoid duplicates on BonesAnimated. Our ToReset mechanism
          (used when building nodes) depends that there are no duplicates on
          these lists.
          And we can have multiple animations affecting the same bone,
          so without explicitly checking below to avoid duplicates,
          we would have trouble. }
        if BonesAnimated.Translation.IndexOf(Bone) = -1 then
          BonesAnimated.Translation.Add(Bone);
      end else
      begin
        Inc(CurvesRemoved);
        FreeAndNil(BoneTimeline);
      end;
    end;

    ChildArray := Json.Find('scale', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineScale.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
        if BonesAnimated.Scale.IndexOf(Bone) = -1 then
          BonesAnimated.Scale.Add(Bone);
      end else
      begin
        Inc(CurvesRemoved);
        FreeAndNil(BoneTimeline);
      end;
    end;

    ChildArray := Json.Find('rotate', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      BoneTimeline := TBoneTimelineRotate.Create;
      BoneTimeline.Bone := Bone;
      BoneTimeline.Parse(ChildArray);
      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
        if BonesAnimated.Rotation.IndexOf(Bone) = -1 then
          BonesAnimated.Rotation.Add(Bone);
      end else
      begin
        Inc(CurvesRemoved);
        FreeAndNil(BoneTimeline);
      end;
    end;
  end;

  procedure ReadSlotTimelines(const Slot: TSlot; const Json: TJSONObject);
  var
    SlotTimeline: TSlotTimeline;
    ChildArray: TJSONArray;
  begin
    ChildArray := Json.Find('attachment', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      SlotTimeline := TSlotTimelineAttachment.Create;
      SlotTimeline.Slot := Slot;
      SlotTimeline.Parse(ChildArray);
      SlotTimelines.Add(SlotTimeline);
      if SlotsAnimated.Attachment.IndexOf(Slot) = -1 then
        SlotsAnimated.Attachment.Add(Slot);
    end;

    ChildArray := Json.Find('color', jtArray) as TJSONArray;
    if ChildArray <> nil then
    begin
      SlotTimeline := TSlotTimelineColor.Create;
      SlotTimeline.Slot := Slot;
      SlotTimeline.Parse(ChildArray);
      SlotTimelines.Add(SlotTimeline);
      if SlotsAnimated.Color.IndexOf(Slot) = -1 then
        SlotsAnimated.Color.Add(Slot);
    end;
  end;

var
  I: Integer;
  Bone: TBone;
  Slot: TSlot;
  ChildObj: TJSONObject;
  ChildArray: TJSONArray;
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

  ChildObj := Json.Find('slots', jtObject) as TJSONObject;
  if ChildObj <> nil then
  begin
    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        Slot := Slots.Find(ChildObj.Names[I]);
        ReadSlotTimelines(Slot, TJSONObject(ChildObj.Items[I]));
      end;
  end;

  ChildArray := Json.Find('draworder', jtArray) as TJSONArray;
  if ChildArray <> nil then
    DrawOrderTimelines.Parse(ChildArray, Slots, SlotsAnimated);
end;

procedure TAnimation.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode;
  const BonesAnimated: TBonesAnimated; const SlotsAnimated: TSlotsAnimated);
var
  MaxTime: Single;
  I, J: Integer;
  Route: TX3DRoute;
  BonesToReset: TBonesAnimated;
  SlotsToReset: TSlotsAnimated;
begin
  Node := TTimeSensorNode.Create(DefaultAnimationPrefix + ToX3DName(Name), BaseUrl);

  MaxTime := 0;
  for I := 0 to BoneTimelines.Count - 1 do
    MaxVar(MaxTime, BoneTimelines[I].MaxTime);
  for I := 0 to SlotTimelines.Count - 1 do
    MaxVar(MaxTime, SlotTimelines[I].MaxTime);
  MaxVar(MaxTime, DrawOrderTimelines.MaxTime);
  { TimeSensor.cycleInterval should be > 0
    (see testcase spine/test-animation-zero-time for where it's needed).
    Also, this is passed as MaxAnimationTime to other BuildNodes methods,
    that also assume that MaxAnimationTime > 0. }
  if MaxTime <= 0 then
    MaxTime := 1;
  Node.CycleInterval := MaxTime;

  NodeUsedAsChild := true;
  Container.FdChildren.Add(Node);

  BonesToReset := TBonesAnimated.Create;
  try
    BonesToReset.Assign(BonesAnimated);

    for I := 0 to BoneTimelines.Count - 1 do
    begin
      BoneTimelines[I].BuildNodes(BaseUrl, MaxTime, Container, BonesToReset);
      { add animation name to bone timeline, to make it unique in case
        we have many animations }
      BoneTimelines[I].Node.X3DName := Node.X3DName + '_' + BoneTimelines[I].Node.X3DName;

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(BoneTimelines[I].Node.EventSet_fraction);
      Container.AddRoute(Route);
    end;

    for I := 0 to BonesToReset.Translation.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(BonesToReset.Translation[I].ResetTranslation.EventSet_fraction);
      Container.AddRoute(Route);
    end;

    for I := 0 to BonesToReset.Scale.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(BonesToReset.Scale[I].ResetScale.EventSet_fraction);
      Container.AddRoute(Route);
    end;

    for I := 0 to BonesToReset.Rotation.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(BonesToReset.Rotation[I].ResetRotation.EventSet_fraction);
      Container.AddRoute(Route);
    end;
  finally FreeAndNil(BonesToReset) end;

  SlotsToReset := TSlotsAnimated.Create;
  try
    SlotsToReset.Assign(SlotsAnimated);

    for I := 0 to SlotTimelines.Count - 1 do
    begin
      SlotTimelines[I].BuildNodes(BaseUrl, MaxTime, Container, SlotsToReset);
      { add animation name to slot timeline, to make it unique in case
        we have many animations }
      SlotTimelines[I].Node1.Node.X3DName := Node.X3DName + '_' +
        SlotTimelines[I].Node1.Node.X3DName;
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(SlotTimelines[I].Node1.EventSet_fraction);
      Container.AddRoute(Route);

      if SlotTimelines[I].HasNode2 then
      begin
        SlotTimelines[I].Node2.Node.X3DName := Node.X3DName + '_' +
          SlotTimelines[I].Node2.Node.X3DName;
        Route := TX3DRoute.Create;
        Route.SetSourceDirectly(Node.EventFraction_changed);
        Route.SetDestinationDirectly(SlotTimelines[I].Node2.EventSet_fraction);
        Container.AddRoute(Route);
      end;
    end;

    for I := 0 to DrawOrderTimelines.Count - 1 do
    begin
      DrawOrderTimelines[I].BuildNodes(BaseUrl, DrawOrderTimelines.Time,
        MaxTime, Container, SlotsToReset);
      { add animation name to draworder timeline, to make it unique in case
        we have many animations }
      DrawOrderTimelines[I].Node.X3DName := Node.X3DName + '_' +
        DrawOrderTimelines[I].Node.X3DName;
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(DrawOrderTimelines[I].Node.EventSet_fraction);
      Container.AddRoute(Route);
    end;

    for I := 0 to SlotsToReset.Attachment.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(SlotsToReset.Attachment[I].ResetAttachment.EventSet_fraction);
      Container.AddRoute(Route);
    end;

    for I := 0 to SlotsToReset.Color.Count - 1 do
    begin
      for J := 0 to SlotsToReset.Color[I].ResetColor1.Count - 1 do
      begin
        Route := TX3DRoute.Create;
        Route.SetSourceDirectly(Node.EventFraction_changed);
        Route.SetDestinationDirectly(SlotsToReset.Color[I].ResetColor1[J].EventSet_fraction);
        Container.AddRoute(Route);
      end;

      for J := 0 to SlotsToReset.Color[I].ResetColor2.Count - 1 do
      begin
        Route := TX3DRoute.Create;
        Route.SetSourceDirectly(Node.EventFraction_changed);
        Route.SetDestinationDirectly(SlotsToReset.Color[I].ResetColor2[J].EventSet_fraction);
        Container.AddRoute(Route);
      end;
    end;

    for I := 0 to SlotsToReset.DrawOrder.Count - 1 do
    begin
      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(Node.EventFraction_changed);
      Route.SetDestinationDirectly(SlotsToReset.DrawOrder[I].ResetDrawOrder.EventSet_fraction);
      Container.AddRoute(Route);
    end;
  finally FreeAndNil(SlotsToReset) end;
end;

{ TAnimationList ------------------------------------------------------------- }

procedure TAnimationList.Parse(const Json: TJSONObject;
  const Bones: TBoneList; const BonesAnimated: TBonesAnimated;
  const Slots: TSlotList; const SlotsAnimated: TSlotsAnimated);
var
  I: Integer;
  Animation: TAnimation;
  ChildObj: TJSONObject;
  CurvesRemoved: Cardinal;
begin
  ChildObj := Json.Find('animations', jtObject) as TJSONObject;
  if ChildObj <> nil then
  begin
    { Note that we do not raise error when "animations" element is missing,
      it just means for us that there are no animations.
      Not sure whether Spine can actually output such file, though. }

    CurvesRemoved := 0;

    for I := 0 to ChildObj.Count - 1 do
      if ChildObj.Items[I] is TJSONObject then
      begin
        Animation := TAnimation.Create;
        Add(Animation);
        Animation.Name := ChildObj.Names[I];
        Animation.Parse(TJSONObject(ChildObj.Items[I]), Bones, BonesAnimated,
          Slots, SlotsAnimated, CurvesRemoved);
      end;

    // WritelnLog('Spine', 'Spine animations curves parsed, empty curves removed: %d',
    //   [CurvesRemoved]);
  end;
end;

procedure TAnimationList.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode;
  const BonesAnimated: TBonesAnimated; const SlotsAnimated: TSlotsAnimated);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, Container, BonesAnimated, SlotsAnimated);
end;

procedure TAnimationList.Exported(const RootNode: TX3DRootNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    RootNode.ManuallyExportNode(Items[I].Node);
end;

{$endif}
