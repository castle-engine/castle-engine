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

{ Spine slot timelines. }

{$ifdef read_interface}
  TSlotTimeline = class
  private
    FMaxTime: Single;
  public
    Slot: TSlot;
    Time: TSingleList;
    Node: TIntegerSequencerNode; // actually, anything with FdKey
    NodeUsedAsChild: boolean;
    property MaxTime: Single read FMaxTime;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray);
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const SlotsToReset: TSlotsAnimated); virtual;
  end;

  TSlotTimelineAttachment = class(TSlotTimeline)
    Attachments: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const SlotsToReset: TSlotsAnimated); override;
  end;

  TSlotTimelineColor = class(TSlotTimeline)
    Colors: TVector4SingleList;
    Curve: boolean;
    CurveControlPoints: TVector4SingleList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const SlotsToReset: TSlotsAnimated); override;
  end;

  TSlotTimelineList = class(specialize TFPGObjectList<TSlotTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

{ TSlotTimeline -------------------------------------------------------------- }

constructor TSlotTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
end;

destructor TSlotTimeline.Destroy;
begin
  FreeAndNil(Time);
  inherited;
end;

procedure TSlotTimeline.Parse(const Json: TJSONArray);
var
  I: Integer;
  O: TJSONObject;
  NextTime: Single;
begin
  for I := 0 to Json.Count - 1 do
    if Json[I] is TJSONObject then
    begin
      O := TJSONObject(Json[I]);

      NextTime := O.Get('time', 0.0);
      if (Time.Count <> 0) and (Time.Last > NextTime) then
        raise ESpineReadError.Create('Timeline must have increasing time values');
      Time.Add(NextTime);
      FMaxTime := NextTime;

      ParseSingleValue(O);
    end;
end;

procedure TSlotTimeline.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const SlotsToReset: TSlotsAnimated);
var
  I: Integer;
begin
  { We assume that descendant already created Node in overridden BuildNodes }
  Node.FdKey.Items.Clear;
  for I := 0 to Time.Count - 1 do
    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);

  NodeUsedAsChild := true;
  Container.FdChildren.Add(Node);
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
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const SlotsToReset: TSlotsAnimated);
var
  I: Integer;
  N: TIntegerSequencerNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  N := TIntegerSequencerNode.Create('SlotTimeline_attachment_' + ToX3DName(Slot.Name));
  N.FdForceContinousValue_Changed.Value := true;
  for I := 0 to Attachments.Count - 1 do
    N.FdKeyValue.Items.Add(Slot.AttachmentNames.IndexOf(Attachments[I]));
  EventValueChanged := N.EventValue_changed;
  Node := N;

  inherited;

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe pose
    at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). We just ignore (do not route) such weird animations for now. }
  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged);
    Route.SetDestinationDirectly(Slot.Node.FdWhichChoice.EventIn);
    Container.Routes.Add(Route);

    if SlotsToReset.Attachment.Remove(Slot) = -1 then
      OnWarning(wtMajor, 'Spine', 'Multiple slot timelines affect slot attachment: slot ' + Slot.Name);
  end;
end;

{ TSlotTimelineColor --------------------------------------------------------- }

constructor TSlotTimelineColor.Create;
begin
  inherited Create;
  Colors := TVector4SingleList.Create;
  CurveControlPoints := TVector4SingleList.Create;
end;

destructor TSlotTimelineColor.Destroy;
begin
  FreeAndNil(Colors);
  FreeAndNil(CurveControlPoints);
  inherited;
end;

procedure TSlotTimelineColor.ParseSingleValue(const Json: TJSONObject);
var
  ControlPoints: TVector4Single;
  CurveTypeJson: TJSONData;
begin
  ControlPoints := Vector4Single(0, 0, 1, 1); // default value, in case curve is later changed from linear to curve
  CurveTypeJson := Json.Find('curve');
  if CurveTypeJson <> nil then
  begin
    if CurveTypeJson is TJSONArray then
    begin
      if TJSONArray(CurveTypeJson).Count <> 4 then
        OnWarning(wtMinor, 'Spine', 'Curve type interpolation is an array, but does not have 4 elements (required for Bezier curve array)') else
      begin
        Curve := true;
        ControlPoints[0] := TJSONArray(CurveTypeJson).Floats[0];
        ControlPoints[1] := TJSONArray(CurveTypeJson).Floats[1];
        ControlPoints[2] := TJSONArray(CurveTypeJson).Floats[2];
        ControlPoints[3] := TJSONArray(CurveTypeJson).Floats[3];
      end;
    end;

    { For now, silently ignore warning that we don't handle curve type
      'stepped'. Spine optimizes it's curves by using 'stepped' where
      previous and next values are equal, so this is common, and we would
      flood the warnings console for many models because of this (while in fact
      models are handled Ok since 'stepped' is only for optimization). }
    { else
    if CurveTypeJson.AsString <> 'linear' then
      OnWarning(wtMinor, 'Spine', 'Found "' + CurveTypeJson.AsString + '" interpolation type on slot timeline of slot ' + Slot.Name + ', we do not support this interpolation type');
    }
  end;
  CurveControlPoints.Add(ControlPoints);

  // TODO: Colors.Add(Json.Get('color', xxx))
end;

procedure TSlotTimelineColor.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const SlotsToReset: TSlotsAnimated);
(* TODO:
var
  I: Integer;
  N: TColorInterpolatorNode;
  NC: TCubicBezierColorInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
  *)
begin
(* TODO:
  if Curve then
  begin
    NC := TCubicBezierColorInterpolatorNode.Create('SlotTimeline_color_' + ToX3DName(Slot.Name));
    for I := 0 to Colors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(Colors[I]);
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
  end else
  begin
    // TODO: we will need interpolator like TColorRGBAInterpolatorNode,
    // that outputs both RGB for Material.diffuse and Material.transparency
    N := TColorInterpolatorNode.Create('SlotTimeline_color_' + ToX3DName(Slot.Name));
    for I := 0 to Colors.Count - 1 do
      N.FdKeyValue.Items.Add(Colors[I]);
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;
  *)

  inherited;

(* TODO:
  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe pose
    at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). We just ignore (do not route) such weird animations for now. }
  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged);
    Route.SetDestinationDirectly(Slot.Node.FdTranslation.EventIn);
    Container.Routes.Add(Route);

    if SlotsToReset.Translation.Remove(Slot) = -1 then
      OnWarning(wtMajor, 'Spine', 'Multiple slot timelines affect slot color: slot ' + Slot.Name);
  end;
  *)
end;

{$endif}
