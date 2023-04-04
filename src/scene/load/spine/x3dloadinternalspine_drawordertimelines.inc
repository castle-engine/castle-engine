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

{ Spine draworder timeline.

  Although similar to slot timeline, it is specified differently in JSON file,
  and also must be processed a little differently to X3D
  (needs duplicate keys/values to pretent that it's discrete),
  so it is implemented independently of TSlotTimeline class. }

{$ifdef read_interface}
  TDrawOrderTimeline = class
  public
    Slot: TSlot;
    DrawOrder: TSingleList;
    Node: TPositionInterpolatorNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BuildNodes(const BaseUrl: string;
      const Time: TSingleList; const MaxAnimationTime: Single;
      const Container: TX3DRootNode);
  end;

  TDrawOrderTimelineList = class({$ifdef FPC}specialize{$endif} TObjectList<TDrawOrderTimeline>)
  strict private
    FMaxTime: Single;
  public
    Time: TSingleList;
    constructor Create;
    destructor Destroy; override;
    property MaxTime: Single read FMaxTime;
    { Find timeline by a slot. @nil if not exists. }
    function FindSlot(const Slot: TSlot): TDrawOrderTimeline;
    procedure Parse(const Json: TJSONArray; const Slots: TSlotList);
  end;
{$endif}

{$ifdef read_implementation}

{ TDrawOrderTimeline --------------------------------------------------------- }

constructor TDrawOrderTimeline.Create;
begin
  inherited;
  DrawOrder := TSingleList.Create;
end;

destructor TDrawOrderTimeline.Destroy;
begin
  FreeAndNil(DrawOrder);
  inherited;
end;

procedure TDrawOrderTimeline.BuildNodes(const BaseUrl: string;
  const Time: TSingleList; const MaxAnimationTime: Single;
  const Container: TX3DRootNode);
var
  I: Integer;
  Route: TX3DRoute;
begin
  Node := TPositionInterpolatorNode.Create('DrawOrderTimeline_' + Slot.Name);

  for I := 0 to Time.Count - 1 do
  begin
    { Write the previous value at the same time. This forces "stepped"
      interpolation instead of linear for draw order. }
    if Time[I] <> 0 then
    begin
      Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
      if I = 0 then
        Node.FdKeyValue.Items.Add(Slot.Node.FdTranslation.Value) else
        Node.FdKeyValue.Items.Add(Node.FdKeyValue.Items.Last);
    end;

    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
    Node.FdKeyValue.Items.Add(Vector3(0, 0, DrawOrder[I] * TSlot.DrawOrderZ));
  end;

  NodeUsedAsChild := true;
  Container.AddChildren(Node);

  { Note: in other anims, we checked Time.Count > 1 now.
    For this, it seems Ok for Time.Count = 1 ? }
  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Node.EventValue_changed);
  Route.SetDestinationDirectly(Slot.Node.FdTranslation.EventIn);
  Container.AddRoute(Route);
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

type
  TSlotsByDrawOrder = {$ifdef FPC}specialize{$endif} TDictionary<Integer, TSlot>;
  TSlotsToFloatDrawOrder = {$ifdef FPC}specialize{$endif} TDictionary<TSlot, Single>;

procedure TDrawOrderTimelineList.Parse(const Json: TJSONArray; const Slots: TSlotList);

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
            end;
      end;
  end;

  { For a subset of slots, we have an integer draw order where they should be.
    Now we want to calculate for them float draw order where they should be,
    squeezing them between ranges of unchanged slots.

    This way we avoid animating draw order of unchanged slots (no need
    to shift them, just because other slots order changed),
    by utilizing the fact that our draw order is in fact float.

    @param SlotsByDrawOrder Input to our algorithm.
    @param SlotsToFloatDrawOrder Output of our algorithm.
  }
  procedure SqueezeSlotDrawOrder(
    const SlotsByDrawOrder: TSlotsByDrawOrder;
    const SlotsToFloatDrawOrder: TSlotsToFloatDrawOrder);
  var
    I, J, LastUnchangedDrawOrder, FollowingUnchangedDrawOrder,
      FirstChanged, LastChanged, NextUnchangedSlot: Integer;
    SlotsOrdered, SlotsUnchanged: TSlotList;
    SlotByDrawOrderPair: {$ifdef FPC}TSlotsByDrawOrder.TDictionaryPair{$else}TPair<Integer, TSlot>{$endif};
  begin
    SlotsOrdered := nil;
    SlotsUnchanged := nil;
    try
      SlotsOrdered := TSlotList.Create(false);
      SlotsUnchanged := TSlotList.Create(false);

      { SlotsUnchanged is kept in draw order order, it contains only slots
        where draw order remains unchanged }
      SlotsUnchanged.Assign(Slots);
      { SlotsOrdered contains only slots where draw order changes or nil values,
        it is indexed by setup pose draw order. }
      SlotsOrdered.Count := Slots.Count;

      for SlotByDrawOrderPair in SlotsByDrawOrder do
      begin
        SlotsOrdered[SlotByDrawOrderPair.Key] := SlotByDrawOrderPair.Value;
        SlotsUnchanged.Remove(SlotByDrawOrderPair.Value);
      end;

      I := 0;
      LastUnchangedDrawOrder := -1;
      NextUnchangedSlot := 0;
      while I < SlotsOrdered.Count do
      begin
        { look for next block of changed slots }
        while (I < SlotsOrdered.Count) and (SlotsOrdered[I] = nil) do
        begin
          { skip over next unchanged slot }
          LastUnchangedDrawOrder := SlotsUnchanged[NextUnchangedSlot].DrawOrder;
          Inc(NextUnchangedSlot);
          Inc(I);
        end;
        FirstChanged := I;
        while (I < SlotsOrdered.Count) and (SlotsOrdered[I] <> nil) do
          Inc(I);
        if NextUnchangedSlot >= SlotsUnchanged.Count then
          FollowingUnchangedDrawOrder := Slots.Count else
          FollowingUnchangedDrawOrder := SlotsUnchanged[NextUnchangedSlot].DrawOrder;
        LastChanged := I - 1;
        for J := FirstChanged to LastChanged do
          SlotsToFloatDrawOrder.AddOrSetValue(SlotsOrdered[J],
            MapRange(J, FirstChanged - 1, LastChanged + 1, LastUnchangedDrawOrder, FollowingUnchangedDrawOrder)
          );
      end;
    finally
      FreeAndNil(SlotsOrdered);
      FreeAndNil(SlotsUnchanged);
    end;
  end;

  procedure ReadTimelines;
  var
    NextTime: Single;
    I, J, DrawOrder: Integer;
    JsonOffsets: TJSONArray;
    JsonOffset, O: TJSONObject;
    SlotsByDrawOrder: TSlotsByDrawOrder;
    SlotsToFloatDrawOrder: TSlotsToFloatDrawOrder;
    Timeline: TDrawOrderTimeline;
    SlotName: string;
    Slot: TSlot;
    SlotsToFloatDrawOrderPair: {$ifdef FPC}TSlotsToFloatDrawOrder.TDictionaryPair{$else}TPair<TSlot, Single>{$endif};
  begin
    SlotsByDrawOrder := nil;
    SlotsToFloatDrawOrder := nil;
    try
      SlotsByDrawOrder := TSlotsByDrawOrder.Create;
      SlotsToFloatDrawOrder := TSlotsToFloatDrawOrder.Create;

      for I := 0 to Json.Count - 1 do
        if Json[I] is TJSONObject then
        begin
          O := TJSONObject(Json[I]);

          NextTime := O.Get('time', 0.0);
          if (Time.Count <> 0) and (Time.Last > NextTime) then
            raise ESpineReadError.Create('Timeline must have increasing time values');
          Time.Add(NextTime);
          FMaxTime := NextTime;

          SlotsByDrawOrder.Clear;
          SlotsToFloatDrawOrder.Clear;

          JsonOffsets := O.Find('offsets', jtArray) as TJSONArray;
          if JsonOffsets <> nil then
          begin
            for J := 0 to JsonOffsets.Count - 1 do
            begin
              { cast checking is it TJSONObject }
              JsonOffset := JsonOffsets[J] as TJSONObject;
              SlotName := JsonOffset.Get('slot', '');
              Slot := Slots.Find(SlotName);
              DrawOrder := Slot.DrawOrder + JsonOffset.Get('offset', 0);
              if (DrawOrder < 0) or
                 (DrawOrder >= Slots.Count) then
              begin
                WritelnWarning('Spine', Format('Animated draworder offset incorrect, goes outside draw order range: slot "%s" with draw order %d',
                  [Slot.Name, DrawOrder]));
                Continue;
              end;
              if SlotsByDrawOrder.ContainsKey(DrawOrder) then
                WritelnWarning('Spine', Format('Animated draworder offset incorrect, multiple slots try to take the same draw order: "%s" and "%s"',
                  [SlotsByDrawOrder[DrawOrder].Name, Slot.Name]));

              { in case of warning above, just overwrite previous slot
                that landed on the same draw order }
              SlotsByDrawOrder.AddOrSetValue(DrawOrder, Slot);
            end;

            SqueezeSlotDrawOrder(SlotsByDrawOrder, SlotsToFloatDrawOrder);

            for SlotsToFloatDrawOrderPair in SlotsToFloatDrawOrder do
            begin
              Slot := SlotsToFloatDrawOrderPair.Key;
              { We do not store just offset, we store calculated DrawOrder
                in a timeline. This way timeline can be exported without
                the knowledge of other draw order timelines. }
              Timeline := FindSlot(Slot);
              Assert(Timeline <> nil, 'We failed to add slot timeline in AddTimelines');
              if Timeline.DrawOrder.Count <> Time.Count - 1 then
                WritelnWarning('Spine', 'draworder timeline count incorrect, it seems that the same slot name was specified multiple times for an offset during the same time for slot: ' +
                  Slot.Name);
              Timeline.DrawOrder.Add(SlotsToFloatDrawOrderPair.Value);
            end;
          end;

          { add offset = 0 for slots not specified here }
          for J := 0 to Count - 1 do
          begin
            Timeline := Items[J];
            if not SlotsToFloatDrawOrder.ContainsKey(Timeline.Slot) then
              Timeline.DrawOrder.Add(Timeline.Slot.DrawOrder);
          end;
        end;
    finally
      FreeAndNil(SlotsByDrawOrder);
      FreeAndNil(SlotsToFloatDrawOrder);
    end;
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
