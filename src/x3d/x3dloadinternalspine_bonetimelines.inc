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

{ Spine bone timelines. }

{$ifdef read_interface}
  TBoneTimeline = class abstract
  private
    FMaxTime: Single;
  public
    Bone: TBone;
    Time: TSingleList;
    Curve: boolean;
    CurveControlPoints: TVector4SingleList;
    Node: TAbstractInterpolatorNode;
    NodeUsedAsChild: boolean;
    property MaxTime: Single read FMaxTime;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray); virtual;
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const BonesToReset: TBonesAnimated); virtual;
    function DoingSomething: boolean; virtual; abstract;
  end;

  TBoneTimelineVector2 = class(TBoneTimeline)
    { Position or scale values on the timeline.
      This always has the same length as @link(Time) list. }
    Vectors: TVector2SingleList;
    constructor Create;
    destructor Destroy; override;
  end;

  TBoneTimelineTranslate = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const BonesToReset: TBonesAnimated); override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineScale = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const BonesToReset: TBonesAnimated); override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineRotate = class(TBoneTimeline)
    { Angle values on the timeline.
      This always has the same length as @link(Time) list. }
    Angles: TSingleList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode;
      const BonesToReset: TBonesAnimated); override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineList = class(specialize TFPGObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

const
  SSpine1KeyFrameWarning = 'Timeline on %s "%s" (animation of %s) has only 1 keyframe. We support it in a consistent way (1 keyframe means that the state is constant), but Spine shows such timeline inconsistently. To avoid confusion, set at least 2 keyframes on every timeline.';
  SSpineMultipleTimelinesWarning = 'Multiple timelines affect %s "%s" animation of %s';

{ TBoneTimeline -------------------------------------------------------------- }

constructor TBoneTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  CurveControlPoints := TVector4SingleList.Create;
end;

destructor TBoneTimeline.Destroy;
begin
  FreeAndNil(Time);
  FreeAndNil(CurveControlPoints);
  inherited;
end;

procedure TBoneTimeline.Parse(const Json: TJSONArray);
var
  I: Integer;
  O: TJSONObject;
  NextTime: Single;
  ControlPoints: TVector4Single;
  CurveTypeJson: TJSONData;
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

      ControlPoints := Vector4Single(0, 0, 1, 1); // default value, in case curve is later changed from linear to curve
      CurveTypeJson := O.Find('curve');
      if CurveTypeJson <> nil then
      begin
        if CurveTypeJson is TJSONArray then
        begin
          if TJSONArray(CurveTypeJson).Count <> 4 then
            WritelnWarning('Spine', 'Curve type interpolation is an array, but does not have 4 elements (required for Bezier curve array)') else
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
          WritelnWarning('Spine', 'Found "' + CurveTypeJson.AsString + '" interpolation type on bone timeline of bone ' + Bone.Name + ', we do not support this interpolation type');
        }
      end;
      CurveControlPoints.Add(ControlPoints);

      ParseSingleValue(O);
    end;
  //Writeln('got timeline for bone ', Bone.Name, ' with ', Time.Count, ' items');
end;

procedure TBoneTimeline.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const BonesToReset: TBonesAnimated);
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

{ TBoneTimelineVector2 ------------------------------------------------------- }

constructor TBoneTimelineVector2.Create;
begin
  inherited Create;
  Vectors := TVector2SingleList.Create;
end;

destructor TBoneTimelineVector2.Destroy;
begin
  FreeAndNil(Vectors);
  inherited;
end;

{ TBoneTimelineTranslate ----------------------------------------------------- }

procedure TBoneTimelineTranslate.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2Single(
    Json.Get('x', 0.0),
    Json.Get('y', 0.0)));
end;

procedure TBoneTimelineTranslate.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const BonesToReset: TBonesAnimated);
var
  I: Integer;
  N: TPositionInterpolatorNode;
  NC: TCubicBezierPositionInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  if Curve then
  begin
    NC := TCubicBezierPositionInterpolatorNode.Create('BoneTimeline_translate_' + ToX3DName(Bone.Name));
    for I := 0 to Vectors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(Vector3Single(
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdTranslation.Value[0] + Vectors[I][0],
        Bone.Node.FdTranslation.Value[1] + Vectors[I][1], 0));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for translation on bone ' + Bone.Name);
  end else
  begin
    N := TPositionInterpolatorNode.Create('BoneTimeline_translate_' + ToX3DName(Bone.Name));
    for I := 0 to Vectors.Count - 1 do
      N.FdKeyValue.Items.Add(Vector3Single(
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdTranslation.Value[0] + Vectors[I][0],
        Bone.Node.FdTranslation.Value[1] + Vectors[I][1], 0));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;

  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdTranslation.EventIn);
  Container.AddRoute(Route);

  if BonesToReset.Translation.Remove(Bone) = -1 then
    WritelnWarning('Spine', Format(SSpineMultipleTimelinesWarning, ['bone', Bone.Name, 'translation']));

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if Time.Count = 1 then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'translation']));
end;

function TBoneTimelineTranslate.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Vectors.Count - 1 do
    if not PerfectlyZeroVector(Vectors[I]) then
      Exit(true);
  Result := false;
end;

{ TBoneTimelineScale --------------------------------------------------------- }

procedure TBoneTimelineScale.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2Single(
    Json.Get('x', 1.0),
    Json.Get('y', 1.0)));
end;

procedure TBoneTimelineScale.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const BonesToReset: TBonesAnimated);

  { Combine initial node scale and 2D key scale into a final scale. }
  function FinalScale(const InitialScale: TVector3Single;
    const KeyScale: TVector2Single): TVector3Single;
  begin
    { multiply setup pose transformation (scale) by key scale,
      as bone timeline is relative to setup pose }
    Result[0] := InitialScale[0] * KeyScale[0];
    Result[1] := InitialScale[1] * KeyScale[1];
    { don't just set Result[2] to 1, instead set it to average of X and Y scales.
      This will work better for collisions. At least in case of uniform scale
      (when scale X and Y are equal), this way Approximate3DScale will return
      the same value (equal to all X, Y an Z), which will make collision routines
      work Ok. }
    Result[2] := (Result[0] + Result[1]) / 2;
  end;

var
  I: Integer;
  N: TPositionInterpolatorNode;
  NC: TCubicBezierPositionInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  if Curve then
  begin
    NC := TCubicBezierPositionInterpolatorNode.Create('BoneTimeline_scale_' + ToX3DName(Bone.Name));
    for I := 0 to Vectors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(FinalScale(Bone.Node.FdScale.Value, Vectors[I]));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for scale on bone ' + Bone.Name);
  end else
  begin
    N := TPositionInterpolatorNode.Create('BoneTimeline_scale_' + ToX3DName(Bone.Name));
    for I := 0 to Vectors.Count - 1 do
      N.FdKeyValue.Items.Add(FinalScale(Bone.Node.FdScale.Value, Vectors[I]));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;
  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdScale.EventIn);
  Container.AddRoute(Route);

  if BonesToReset.Scale.Remove(Bone) = -1 then
    WritelnWarning('Spine', Format(SSpineMultipleTimelinesWarning, ['bone', Bone.Name, 'scale']));

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if Time.Count = 1 then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'scale']));
end;

function TBoneTimelineScale.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Vectors.Count - 1 do
    if (Vectors[I][0] <> 1) or
       (Vectors[I][1] <> 1) then
      Exit(true);
  Result := false;
end;

{ TBoneTimelineRotate -------------------------------------------------------- }

constructor TBoneTimelineRotate.Create;
begin
  inherited;
  Angles := TSingleList.Create;
end;

destructor TBoneTimelineRotate.Destroy;
begin
  FreeAndNil(Angles);
  inherited;
end;

procedure TBoneTimelineRotate.ParseSingleValue(const Json: TJSONObject);
begin
  Angles.Add(Json.Get('angle', 0.0));
end;

procedure TBoneTimelineRotate.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode;
  const BonesToReset: TBonesAnimated);
var
  I: Integer;
  N: TOrientationInterpolatorNode;
  NC: TCubicBezier2DOrientationInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  if Curve then
  begin
    NC := TCubicBezier2DOrientationInterpolatorNode.Create('BoneTimeline_rotate_' + ToX3DName(Bone.Name));
    NC.FdAxis.Value := Vector3Single(0, 0, 1);
    for I := 0 to Angles.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdRotation.Value[3] + DegToRad(Angles[I]));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for rotation on bone ' + Bone.Name);
  end else
  begin
    N := TOrientationInterpolatorNode.Create('BoneTimeline_rotate_' + ToX3DName(Bone.Name));
    for I := 0 to Angles.Count - 1 do
      N.FdKeyValue.Items.Add(Vector4Single(0, 0, 1,
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdRotation.Value[3] +
        DegToRad(Angles[I])));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;

  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdRotation.EventIn);
  Container.AddRoute(Route);

  if BonesToReset.Rotation.Remove(Bone) = -1 then
    WritelnWarning('Spine', Format(SSpineMultipleTimelinesWarning, ['bone', Bone.Name, 'rotation']));

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if Time.Count = 1 then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'rotation']));
end;

function TBoneTimelineRotate.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Angles.Count - 1 do
    if Angles[I] <> 0 then
      Exit(true);
  Result := false;
end;

{$endif}
