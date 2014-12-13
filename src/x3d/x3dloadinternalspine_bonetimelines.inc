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
    function Simplify: boolean; virtual; abstract;
  end;

  TBoneTimelineVector2 = class(TBoneTimeline)
    { Position or scale values on the timeline.
      This always has the same length as @link(Time) list. }
    Vectors: TVector2SingleList;
    constructor Create;
    destructor Destroy; override;
    function Simplify: boolean; override;
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
    function Simplify: boolean; override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineList = class(specialize TFPGObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

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
          OnWarning(wtMinor, 'Spine', 'Found "' + CurveTypeJson.AsString + '" interpolation type on bone timeline of bone ' + Bone.Name + ', we do not support this interpolation type');
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

function TBoneTimelineVector2.Simplify: boolean;
var
  I: Integer;
begin
  Result := false;
  if Vectors.Count > 1 then
  begin
    { if some value diferent than 1st, do not optimize }
    for I := 1 to Vectors.Count - 1 do
      if not VectorsPerfectlyEqual(Vectors[I], Vectors[0]) then
        Exit;

    { optimize }
    Vectors.Count := 1;
    Time.Count := Min(Time.Count, 1);
    CurveControlPoints.Count := Min(CurveControlPoints.Count, 1);
    Result := true;
  end;
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

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe pose
    at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). We just ignore (do not route) such weird animations for now. }
  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged);
    Route.SetDestinationDirectly(Bone.Node.FdTranslation.EventIn);
    Container.Routes.Add(Route);

    if BonesToReset.Translation.Remove(Bone) = -1 then
      OnWarning(wtMajor, 'Spine', 'Multiple bone timelines affect bone translation: bone ' + Bone.Name);
  end;
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
      NC.FdKeyValue.Items.Add(Vector3Single(
        { multiple setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdScale.Value[0] * Vectors[I][0],
        Bone.Node.FdScale.Value[1] * Vectors[I][1], 1));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for scale on bone ' + Bone.Name);
  end else
  begin
    N := TPositionInterpolatorNode.Create('BoneTimeline_scale_' + ToX3DName(Bone.Name));
    for I := 0 to Vectors.Count - 1 do
      N.FdKeyValue.Items.Add(Vector3Single(
        { multiple setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdScale.Value[0] * Vectors[I][0],
        Bone.Node.FdScale.Value[1] * Vectors[I][1], 1));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;
  inherited;

  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged);
    Route.SetDestinationDirectly(Bone.Node.FdScale.EventIn);
    Container.Routes.Add(Route);

    if BonesToReset.Scale.Remove(Bone) = -1 then
      OnWarning(wtMajor, 'Spine', 'Multiple bone timelines affect bone scale: bone ' + Bone.Name);
  end;
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

  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(EventValueChanged);
    Route.SetDestinationDirectly(Bone.Node.FdRotation.EventIn);
    Container.Routes.Add(Route);

    if BonesToReset.Rotation.Remove(Bone) = -1 then
      OnWarning(wtMajor, 'Spine', 'Multiple bone timelines affect bone rotation: bone ' + Bone.Name);
  end;
end;

function TBoneTimelineRotate.Simplify: boolean;
var
  I: Integer;
begin
  Result := false;
  if Angles.Count > 1 then
  begin
    { if some value diferent than 1st, do not optimize }
    for I := 1 to Angles.Count - 1 do
      if Angles[I] <> Angles[0] then
        Exit;

    { optimize }
    Angles.Count := 1;
    Time.Count := Min(Time.Count, 1);
    CurveControlPoints.Count := Min(CurveControlPoints.Count, 1);
    Result := true;
  end;
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
