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

{ Spine animations. }

{$ifdef read_interface}
  TAnimation = class
    Name: string;
    BoneTimelines: TBoneTimelineList;
    SlotTimelines: TSlotTimelineList;
    DrawOrderTimelines: TDrawOrderTimelineList;
    DeformTimelines: TDeformTimelineList;
    WeightedMeshTimelines: TWeightedMeshTimelineList;
    Node: TTimeSensorNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const Bones: TBoneList; const Slots: TSlotList; const CurrentSkin: TSkin;
      const NormalizeCurvesKeyValues: Boolean;
      var CurvesRemoved: Cardinal);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode);
  end;

  TAnimationList = class({$ifdef FPC}specialize{$endif} TObjectList<TAnimation>)
    procedure Parse(const Json: TJSONObject;
      const Bones: TBoneList; const Slots: TSlotList; const CurrentSkin: TSkin;
      const NormalizeCurvesKeyValues: Boolean);
    procedure BuildNodes(const BaseUrl: string; const Container: TX3DRootNode);
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
  DeformTimelines := TDeformTimelineList.Create;
  WeightedMeshTimelines := TWeightedMeshTimelineList.Create;
end;

destructor TAnimation.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(BoneTimelines);
  FreeAndNil(SlotTimelines);
  FreeAndNil(DrawOrderTimelines);
  FreeAndNil(DeformTimelines);
  FreeAndNil(WeightedMeshTimelines);
  inherited;
end;

procedure TAnimation.Parse(const Json: TJSONObject;
  const Bones: TBoneList; const Slots: TSlotList; const CurrentSkin: TSkin;
  const NormalizeCurvesKeyValues: Boolean;
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
      BoneTimeline.Parse(ChildArray, NormalizeCurvesKeyValues);

      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
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
      BoneTimeline.Parse(ChildArray, NormalizeCurvesKeyValues);

      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
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
      BoneTimeline.Parse(ChildArray, NormalizeCurvesKeyValues);

      if BoneTimeline.DoingSomething then
      begin
        BoneTimelines.Add(BoneTimeline);
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
      SlotTimeline.Parse(ChildArray, NormalizeCurvesKeyValues);
      SlotTimelines.Add(SlotTimeline);
    end;

    ChildArray := Json.Find('color', jtArray) as TJSONArray;
    if ChildArray = nil then
      ChildArray := Json.Find('rgba', jtArray) as TJSONArray; // new name around Spine >= 4.0
    if ChildArray <> nil then
    begin
      SlotTimeline := TSlotTimelineColor.Create;
      SlotTimeline.Slot := Slot;
      SlotTimeline.Parse(ChildArray, NormalizeCurvesKeyValues);
      SlotTimelines.Add(SlotTimeline);
    end;
  end;

  procedure ReadDeformTimelines(const SlotName: string;
    const Json: TJSONObject; const Slots: TSlotList);
  var
    I: Integer;
    DeformTimeline: TDeformTimeline;
    Attachment: TAttachment;
    AttachmentName: String;
  begin
    for I := 0 to Json.Count - 1 do
    begin
      AttachmentName := Json.Names[I];
      Attachment := CurrentSkin.Attachments.Find(SlotName, AttachmentName, nil, false);
      if Attachment = nil then
      begin
        WritelnWarning('Cannot find attachment "%s" in slot "%s" to animate using deformation', [
          AttachmentName,
          SlotName
        ]);
        Continue;
      end;
      if not (Attachment is TMeshAttachment) then
      begin
        WritelnWarning('Attachment "%s" in slot "%s" cannot be animated using deformation, as it is not a mesh attachment, it is %s', [
          AttachmentName,
          SlotName,
          Attachment.ClassName
        ]);
        Continue;
      end;
      if not (Json.Items[I] is TJSONArray) then
      begin
        WritelnWarning('Attachment "%s" in slot "%s" cannot be animated: JSON error, expected array', [
          AttachmentName,
          SlotName
        ]);
        Continue;
      end;
      DeformTimeline := TDeformTimeline.Create;
      DeformTimeline.Slot := Slots.Find(SlotName);
      DeformTimeline.Attachment := Attachment as TMeshAttachment;
      DeformTimeline.Parse(Json.Items[I] as TJSONArray, NormalizeCurvesKeyValues);
      DeformTimelines.Add(DeformTimeline);
    end;
  end;

  { There's no such thing as 'weighted mesh timeline', so we generate weighted
    mesh timelines by checking to see if a mesh is affect by bones, and that
    mesh is a weighted mesh or not, then we will calculate vertices affected by
    those bones }
  procedure ReadWeightedMeshTimelines;
  var
    I, J: Integer;
    Attachment: TAttachment;
    WeightedMeshTimeline: TWeightedMeshTimeline;
    AffectedDeformTimeline: TDeformTimeline;
  begin
    for I := 0 to CurrentSkin.Attachments.Count - 1 do
    begin
      Attachment := CurrentSkin.Attachments[I];
      // Make sure this attachment is a weighted mesh
      if (Attachment is TMeshAttachment) and ((Attachment as TMeshAttachment).Weights.Count > 0) then
      begin
        // Backup original bone timeline properties
        for J := 0 to BoneTimelines.Count - 1 do
          BoneTimelines[J].Backup;
        // Search through deform timeline list to find affected one
        AffectedDeformTimeline := nil;
        for J := 0 to DeformTimelines.Count - 1 do
        begin
          if DeformTimelines[J].Attachment.Name = Attachment.Name then
          begin
            AffectedDeformTimeline := DeformTimelines[J];
            AffectedDeformTimeline.IsHandledByWeightedMeshTimeline := True;
            // Backup the original Time
            AffectedDeformTimeline.Backup;
            Break;
          end;
        end;
        // We generate weightedmesh timeline if the number of bone timeline > 0,
        // or has AffectedDeformTimeline
        if (BoneTimelines.Count > 0) or (AffectedDeformTimeline <> nil) then
        begin
          WeightedMeshTimeline := TWeightedMeshTimeline.Create;
          WeightedMeshTimeline.AffectedDeformTimeline := AffectedDeformTimeline;
          WeightedMeshTimeline.Name := IntToStr(I);
          WeightedMeshTimeline.Attachment := Attachment as TMeshAttachment;
          WeightedMeshTimeline.AffectedBoneTimelines := BoneTimelines;
          WeightedMeshTimeline.Parse;
          WeightedMeshTimelines.Add(WeightedMeshTimeline);
        end;
        // Restore bone timeline properties
        for J := 0 to BoneTimelines.Count - 1 do
          BoneTimelines[J].Restore;
      end;
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
  if ChildArray = nil then
    ChildArray := Json.Find('drawOrder', jtArray) as TJSONArray; // try different case
  if ChildArray <> nil then
    DrawOrderTimelines.Parse(ChildArray, Slots);

  ChildObj := Json.Find('deform', jtObject) as TJSONObject;
  if ChildObj <> nil then
  begin
    ChildObj := ChildObj.Find(CurrentSkin.Name) as TJSONObject;
    if ChildObj <> nil then
      for I := 0 to ChildObj.Count - 1 do
        if ChildObj.Items[I] is TJSONObject then
          ReadDeformTimelines(ChildObj.Names[I], TJSONObject(ChildObj.Items[I]), Slots);
  end;

  ReadWeightedMeshTimelines;
end;

procedure TAnimation.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode);
var
  MaxTime: Single;
  I: Integer;
  Route: TX3DRoute;
begin
  Node := TTimeSensorNode.Create(DefaultAnimationPrefix + Name, BaseUrl);

  MaxTime := 0;
  for I := 0 to BoneTimelines.Count - 1 do
    MaxVar(MaxTime, BoneTimelines[I].MaxTime);
  for I := 0 to SlotTimelines.Count - 1 do
    MaxVar(MaxTime, SlotTimelines[I].MaxTime);
  for I := 0 to DeformTimelines.Count - 1 do
    MaxVar(MaxTime, DeformTimelines[I].MaxTime);
  MaxVar(MaxTime, DrawOrderTimelines.MaxTime);
  { TimeSensor.cycleInterval should be > 0
    (see testcase spine/test-animation-zero-time for where it's needed).
    Also, this is passed as MaxAnimationTime to other BuildNodes methods,
    that also assume that MaxAnimationTime > 0. }
  if MaxTime <= 0 then
    MaxTime := 1;
  Node.CycleInterval := MaxTime;

  NodeUsedAsChild := true;
  Container.AddChildren(Node);

  for I := 0 to BoneTimelines.Count - 1 do
  begin
    BoneTimelines[I].BuildNodes(BaseUrl, MaxTime, Container);
    { add animation name to bone timeline, to make it unique in case
      we have many animations }
    BoneTimelines[I].Node.X3DName := Node.X3DName + '_' + BoneTimelines[I].Node.X3DName;

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(Node.EventFraction_changed);
    Route.SetDestinationDirectly(BoneTimelines[I].Node.EventSet_fraction);
    Container.AddRoute(Route);
  end;

  for I := 0 to SlotTimelines.Count - 1 do
  begin
    SlotTimelines[I].BuildNodes(BaseUrl, MaxTime, Container);
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
      MaxTime, Container);
    { add animation name to draworder timeline, to make it unique in case
      we have many animations }
    DrawOrderTimelines[I].Node.X3DName := Node.X3DName + '_' +
      DrawOrderTimelines[I].Node.X3DName;
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(Node.EventFraction_changed);
    Route.SetDestinationDirectly(DrawOrderTimelines[I].Node.EventSet_fraction);
    Container.AddRoute(Route);
  end;

  for I := 0 to DeformTimelines.Count - 1 do
  begin
    if DeformTimelines[I].IsHandledByWeightedMeshTimeline then
      Continue;
    DeformTimelines[I].BuildNodes(MaxTime, Container);
    { add animation name to deform timeline, to make it unique in case
      we have many animations }
    DeformTimelines[I].Node.X3DName := Node.X3DName + '_' +
      DeformTimelines[I].Node.X3DName;
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(Node.EventFraction_changed);
    Route.SetDestinationDirectly(DeformTimelines[I].Node.EventSet_fraction);
    Container.AddRoute(Route);
  end;

  for I := 0 to WeightedMeshTimelines.Count - 1 do
  begin
    WeightedMeshTimelines[I].BuildNodes(MaxTime, Container);
    { add animation name to weighted mesh timeline, to make it unique in case
      we have many animations }
    WeightedMeshTimelines[I].Node.X3DName := Node.X3DName + '_' +
      WeightedMeshTimelines[I].Node.X3DName;
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(Node.EventFraction_changed);
    Route.SetDestinationDirectly(WeightedMeshTimelines[I].Node.EventSet_fraction);
    Container.AddRoute(Route);
  end;
end;

{ TAnimationList ------------------------------------------------------------- }

procedure TAnimationList.Parse(const Json: TJSONObject;
  const Bones: TBoneList; const Slots: TSlotList; const CurrentSkin: TSkin;
  const NormalizeCurvesKeyValues: Boolean);
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
        Animation.Parse(TJSONObject(ChildObj.Items[I]), Bones, Slots,
          CurrentSkin, NormalizeCurvesKeyValues, CurvesRemoved);
      end;

    // WritelnLog('Spine', 'Spine animations curves parsed, empty curves removed: %d',
    //   [CurvesRemoved]);
  end;
end;

procedure TAnimationList.BuildNodes(const BaseUrl: string;
  const Container: TX3DRootNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, Container);
end;

procedure TAnimationList.Exported(const RootNode: TX3DRootNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    RootNode.ExportNode(Items[I].Node);
end;

{$endif}
