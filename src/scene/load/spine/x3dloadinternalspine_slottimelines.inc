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

{ Spine slot timelines. }

{$ifdef read_interface}
  { Node with key field and set_fraction event. Like interpolators,
    but not only (e.g. IntegerSequencer). }
  TKeyedNode = record
    EventSet_Fraction: TSFFloatEvent;
    Key: TMFFloat;
    Node: TAbstractChildNode;
    UsedAsChild: boolean;
  end;

  TSlotTimeline = class
  strict private
    FMaxTime: Single;
  public
    Slot: TSlot;
    Time: TSingleList;
    Node1, Node2: TKeyedNode;
    HasNode2: boolean;
    Curve: boolean;
    CurveControlPoints: TVector4List;
    property MaxTime: Single read FMaxTime;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean);
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); virtual;
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); virtual; abstract;
  end;

  TSlotTimelineAttachment = class(TSlotTimeline)
    Attachments: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); override;
  end;

  TSlotTimelineColor = class(TSlotTimeline)
    Colors: TVector4List;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); override;
  end;

  TSlotTimelineList = class({$ifdef FPC}specialize{$endif} TObjectList<TSlotTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

{ TSlotTimeline -------------------------------------------------------------- }

constructor TSlotTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  CurveControlPoints := TVector4List.Create;
end;

destructor TSlotTimeline.Destroy;
begin
  FreeAndNil(Time);
  FreeAndNil(CurveControlPoints);
  inherited;
end;

procedure TSlotTimeline.Parse(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  O: TJSONObject;
  NextTime: Single;
begin
  Curve := false;

  for I := 0 to Json.Count - 1 do
    if Json[I] is TJSONObject then
    begin
      O := TJSONObject(Json[I]);

      NextTime := O.Get('time', 0.0);
      if (Time.Count <> 0) and (Time.Last > NextTime) then
        raise ESpineReadError.Create('Timeline must have increasing time values');
      Time.Add(NextTime);
      FMaxTime := NextTime;

      // only detect whether we have a curve at this point
      if not Curve then
        Curve := O.Find('curve') is TJSONArray;

      ParseSingleValue(O);
    end;

  if Curve then
    ParseControlPoints(Json, NormalizeCurvesKeyValues);
end;

procedure TSlotTimeline.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);

  procedure FillKeys(var Node: TKeyedNode);
  var
    I: Integer;
  begin
    Node.Key.Items.Clear;
    for I := 0 to Time.Count - 1 do
      Node.Key.Items.Add(Time[I] / MaxAnimationTime);
    Node.UsedAsChild := true;
    Container.AddChildren(Node.Node);
  end;

begin
  { We assume that descendant already created Node1/2 in overridden BuildNodes }
  FillKeys(Node1);
  if HasNode2 then
    FillKeys(Node2);
end;

{ TSlotTimelineAttachment ------------------------------------------------------- }

constructor TSlotTimelineAttachment.Create;
begin
  inherited Create;
  Attachments := TStringList.Create;
end;

destructor TSlotTimelineAttachment.Destroy;
begin
  FreeAndNil(Attachments);
  inherited;
end;

procedure TSlotTimelineAttachment.ParseSingleValue(const Json: TJSONObject);
var
  AttachmentName: string;
begin
  AttachmentName := Json.Get('name', '');
  Slot.AttachmentNames.Add(AttachmentName);
  Attachments.Add(AttachmentName);
end;

procedure TSlotTimelineAttachment.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TIntegerSequencerNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  N := TIntegerSequencerNode.Create('SlotTimeline_attachment_' + Slot.Name);
  N.ForceContinuousValue_Changed := true;
  for I := 0 to Attachments.Count - 1 do
    N.FdKeyValue.Items.Add(Slot.AttachmentNames.IndexOf(Attachments[I]));
  EventValueChanged := N.EventValue_changed;
  Node1.EventSet_Fraction := N.EventSet_Fraction;
  Node1.Key := N.FdKey;
  Node1.Node := N;

  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Slot.SwitchNode.FdWhichChoice.EventIn);
  Container.AddRoute(Route);

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if SpineVerboseWarnings and (Time.Count = 1) then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['slot', Slot.Name, 'attachment']));
end;

procedure TSlotTimelineAttachment.ParseControlPoints(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
begin
  WritelnWarning('Invalid Spine JSON: Curve interpolation for slot attachments animation is not possible');
end;

{ TSlotTimelineColor --------------------------------------------------------- }

constructor TSlotTimelineColor.Create;
begin
  inherited Create;
  Colors := TVector4List.Create;
end;

destructor TSlotTimelineColor.Destroy;
begin
  FreeAndNil(Colors);
  inherited;
end;

procedure TSlotTimelineColor.ParseSingleValue(const Json: TJSONObject);
begin
  Colors.Add(HexToColor(Json.Get('color', 'ffffffff')));
end;

procedure TSlotTimelineColor.ParseControlPoints(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  PreviousTime, NextTime: Single;
  PreviousValue, NextValue: TVector4;
  O: TJSONObject;
begin
  Assert(Time.Count = Colors.Count);
  if Json.Count <> Time.Count then
  begin
    WritelnWarning('Invalid Spine JSON: Curve support requires that timeline JSON count is correct');
    Curve := false;
    Exit;
  end;

  for I := 0 to Time.Count - 2 do
  begin
    PreviousTime := Time[I];
    NextTime     := Time[I + 1];
    PreviousValue := Colors.L[I    ];
    NextValue     := Colors.L[I + 1];

    if not (Json[I] is TJSONObject) then
    begin
      WritelnWarning('Invalid Spine JSON: Curve support requires that all JSON timeline chldren are object');
      Curve := false;
      Exit;
    end;

    O := TJSONObject(Json[I]);

    CurveControlPoints.Add(ReadCurveControlPoints('slot(color)', O, NormalizeCurvesKeyValues, 4,
      PreviousTime, NextTime,
      PreviousValue, NextValue));
  end;

  CurveControlPoints.Add(Vector4(0, 0, 1, 1)); // TODO: add last dummy value, because our TCubicBezier* may expect it
end;

procedure TSlotTimelineColor.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TColorInterpolatorNode;
  NA: TScalarInterpolatorNode;
  EventValueChanged1, EventValueChanged2: TX3DEvent;
  Route: TX3DRoute;
begin
(* TODO: colors on curve:
  if Curve then
  begin
    NC := TCubicBezierColorInterpolatorNode.Create('SlotTimeline_color_' + Slot.Name);
    for I := 0 to Colors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(Colors[I]);
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    NC.OptimizeControlPoints;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
  end else
  *)
  begin
    N := TColorInterpolatorNode.Create('SlotTimeline_color_' + Slot.Name);
    for I := 0 to Colors.Count - 1 do
      N.FdKeyValue.Items.Add(Colors[I].XYZ);
    EventValueChanged1 := N.EventValue_changed;
    Node1.EventSet_Fraction := N.EventSet_Fraction;
    Node1.Node := N;
    Node1.Key := N.FdKey;

    NA := TScalarInterpolatorNode.Create('SlotTimeline_alpha_' + Slot.Name);
    for I := 0 to Colors.Count - 1 do
      NA.FdKeyValue.Items.Add(1 - Colors[I][3]);
    EventValueChanged2 := NA.EventValue_changed;
    Node2.EventSet_Fraction := NA.EventSet_Fraction;
    Node2.Node := NA;
    Node2.Key := NA.FdKey;
    HasNode2 := true;
  end;

  inherited;

  for I := 0 to Slot.Materials.Count - 1 do
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged1);
    Route.SetDestinationDirectly(Slot.Materials[I].FdEmissiveColor);
    Container.AddRoute(Route);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged2);
    Route.SetDestinationDirectly(Slot.Materials[I].FdTransparency);
    Container.AddRoute(Route);
  end;

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if SpineVerboseWarnings and (Time.Count = 1) then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['slot', Slot.Name, 'color']));
end;

{$endif}
