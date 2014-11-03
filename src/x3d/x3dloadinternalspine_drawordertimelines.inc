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

{ Spine draworder timeline.

  Although similar to slot timeline, it is specified differently in JSON file,
  and also must be processed a little differently to X3D
  (needs duplicate keys/values to pretent that it's discrete),
  so it is implemented independently of TSlotTimeline class. }

{$ifdef read_interface}
  TDrawOrderTimeline = class
  public
    Slot: TSlot;
    Offset: TLongIntList;
    Node: TPositionInterpolatorNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BuildNodes(const BaseUrl: string;
      const Time: TSingleList; const MaxAnimationTime: Single;
      const Container: TX3DRootNode; const SlotsToReset: TSlotsAnimated);
  end;

  TDrawOrderTimelineList = class(specialize TFPGObjectList<TDrawOrderTimeline>)
  strict private
    FMaxTime: Single;
  public
    Time: TSingleList;
    constructor Create;
    destructor Destroy; override;
    property MaxTime: Single read FMaxTime;
    { Find timeline by a slot. @nil if not exists. }
    function FindSlot(const Slot: TSlot): TDrawOrderTimeline;
    procedure Parse(const Json: TJSONArray; const Slots: TSlotList;
      const SlotsAnimated: TSlotsAnimated);
  end;
{$endif}

{$ifdef read_implementation}

{ TDrawOrderTimeline --------------------------------------------------------- }

constructor TDrawOrderTimeline.Create;
begin
  inherited;
  Offset := TLongIntList.Create;
end;

destructor TDrawOrderTimeline.Destroy;
begin
  FreeAndNil(Offset);
  inherited;
end;

procedure TDrawOrderTimeline.BuildNodes(const BaseUrl: string;
  const Time: TSingleList; const MaxAnimationTime: Single;
  const Container: TX3DRootNode; const SlotsToReset: TSlotsAnimated);
var
  I: Integer;
  Route: TX3DRoute;
begin
  Node := TPositionInterpolatorNode.Create('DrawOrderTimeline_' + ToX3DName(Slot.Name));

  for I := 0 to Time.Count - 1 do
  begin
    { Write the previous value at the same time. This forces "stepped"
      interpolation instead of linear for draw order. }
    if Time[I] <> 0 then
    begin
      Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
      Node.FdKeyValue.Items.Add(Node.FdKeyValue.Items.Last);
    end;

    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
    Node.FdKeyValue.Items.Add(Vector3Single(0, 0, (Slot.DrawOrder + Offset[I] - 0.5) * TSlot.DrawOrderZ));
  end;

  NodeUsedAsChild := true;
  Container.FdChildren.Add(Node);

  { Note: in other anims, we checked Time.Count > 1 now.
    For this, it seems Ok for Time.Count = 1 ? }
  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Node.EventValue_changed);
  Route.SetDestinationDirectly(Slot.Node.FdTranslation.EventIn);
  Container.Routes.Add(Route);

  if SlotsToReset.DrawOrder.Remove(Slot) = -1 then
    OnWarning(wtMajor, 'Spine', 'Slot draworder offset is affected multiple times: slot ' + Slot.Name);
end;

{ TDrawOrderTimelineList ----------------------------------------------------- }

constructor TDrawOrderTimelineList.Create;
begin
  { do not call just "inherited", it would call TObject.Create?
    TODO: investigate and report FPC bug if yes. }
  inherited Create(true);
  Time := TSingleList.Create;
end;

destructor TDrawOrderTimelineList.Destroy;
begin
  FreeAndNil(Time);
  inherited;
end;

procedure TDrawOrderTimelineList.Parse(const Json: TJSONArray;
  const Slots: TSlotList; const SlotsAnimated: TSlotsAnimated);

  procedure AddTimelines;
  var
    I, J: Integer;
    JsonOffsets: TJSONArray;
    JsonOffset, O: TJSONObject;
    SlotName: string;
    Slot: TSlot;
    Timeline: TDrawOrderTimeline;
  begin
    for I := 0 to Json.Count - 1 do
      if Json[I] is TJSONObject then
      begin
        O := TJSONObject(Json[I]);

        JsonOffsets := O.Find('offsets', jtArray) as TJSONArray;
        if JsonOffsets <> nil then
          for J := 0 to JsonOffsets.Count - 1 do
            if JsonOffsets[J] is TJSONObject then
            begin
              JsonOffset := TJSONObject(JsonOffsets[J]);
              SlotName := JsonOffset.Get('slot', '');
              Slot := Slots.Find(SlotName);
              if FindSlot(Slot) = nil then
              begin
                Timeline := TDrawOrderTimeline.Create;
                Timeline.Slot := Slot;
                Add(Timeline);
              end;
              if SlotsAnimated.DrawOrder.IndexOf(Slot) = -1 then
                SlotsAnimated.DrawOrder.Add(Slot);
            end;
      end;
  end;

  procedure ReadTimelines;
  var
    NextTime: Single;
    I, J: Integer;
    JsonOffsets: TJSONArray;
    JsonOffset, O: TJSONObject;
    SlotsSet: TSlotList;
    Timeline: TDrawOrderTimeline;
    SlotName: string;
    Slot: TSlot;
    Offset: Integer;
  begin
    SlotsSet := TSlotList.Create(false);
    try
      for I := 0 to Json.Count - 1 do
        if Json[I] is TJSONObject then
        begin
          O := TJSONObject(Json[I]);

          NextTime := O.Get('time', 0.0);
          if (Time.Count <> 0) and (Time.Last > NextTime) then
            raise ESpineReadError.Create('Timeline must have increasing time values');
          Time.Add(NextTime);
          FMaxTime := NextTime;

          SlotsSet.Clear;

          JsonOffsets := O.Find('offsets', jtArray) as TJSONArray;
          if JsonOffsets <> nil then
          begin
            for J := 0 to JsonOffsets.Count - 1 do
              if JsonOffsets[J] is TJSONObject then
              begin
                JsonOffset := TJSONObject(JsonOffsets[J]);
                SlotName := JsonOffset.Get('slot', '');
                Offset := JsonOffset.Get('offset', 0);
                Slot := Slots.Find(SlotName);
                Timeline := FindSlot(Slot);
                Assert(Timeline <> nil, 'We failed to add slot timeline in AddTimelines');
                if Timeline.Offset.Count <> Time.Count - 1 then
                  OnWarning(wtMajor, 'Spine', 'draworder timeline count incorrect, it seems that the same slot name was specified multiple times for an offset during the same time for slot: ' + Slot.Name);
                Timeline.Offset.Add(Offset);
                SlotsSet.Add(Slot);
              end;
          end;

          { add offset = 0 for slots not specified here }
          for J := 0 to Count - 1 do
          begin
            Timeline := Items[J];
            if SlotsSet.IndexOf(Timeline.Slot) = -1 then
              Timeline.Offset.Add(0);
          end;
        end;
    finally FreeAndNil(SlotsSet) end;
  end;

begin
  { 2 passes: first add timelines, then set them }
  AddTimelines;
  ReadTimelines;
end;

function TDrawOrderTimelineList.FindSlot(const Slot: TSlot): TDrawOrderTimeline;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Slot = Slot then
      Exit(Items[I]);
  Result := nil;
end;

{$endif}
