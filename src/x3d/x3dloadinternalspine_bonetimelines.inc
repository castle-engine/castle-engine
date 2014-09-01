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
    Node: TAbstractInterpolatorNode;
    NodeUsedAsChild: boolean;
    property MaxTime: Single read FMaxTime;
    // TODO: curve not supported yet
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray); virtual;
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); virtual;
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
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
  end;

  TBoneTimelineScale = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
  end;

  TBoneTimelineRotate = class(TBoneTimeline)
    { Angle values on the timeline.
      This always has the same length as @link(Time) list. }
    Angles: TSingleList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
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
end;

destructor TBoneTimeline.Destroy;
begin
  FreeAndNil(Time);
  inherited;
end;

procedure TBoneTimeline.Parse(const Json: TJSONArray);
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
  //Writeln('got timeline for bone ', Bone.Name, ' with ', Time.Count, ' items');
end;

procedure TBoneTimeline.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
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
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TPositionInterpolatorNode;
  Route: TX3DRoute;
begin
  N := TPositionInterpolatorNode.Create('BoneTimeline_translate_' + ToX3DName(Bone.Name));
  N.FdKeyValue.Items.Clear;
  for I := 0 to Vectors.Count - 1 do
    N.FdKeyValue.Items.Add(Vector3Single(
      { add setup pose transform, as bone timeline is relative to setup pose }
      Bone.Node.FdTranslation.Value[0] + Vectors[I][0],
      Bone.Node.FdTranslation.Value[1] + Vectors[I][1], 0));

  Node := N;
  inherited;

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe pose
    at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). We just ignore (do not route) such weird animations for now. }
  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(N.EventValue_changed);
    Route.SetDestinationDirectly(Bone.Node.FdTranslation.EventIn);
    Container.Routes.Add(Route);
  end;
end;

{ TBoneTimelineScale --------------------------------------------------------- }

procedure TBoneTimelineScale.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2Single(
    Json.Get('x', 1.0),
    Json.Get('y', 1.0)));
end;

procedure TBoneTimelineScale.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TPositionInterpolatorNode;
  Route: TX3DRoute;
begin
  N := TPositionInterpolatorNode.Create('BoneTimeline_scale_' + ToX3DName(Bone.Name));
  N.FdKeyValue.Items.Clear;
  for I := 0 to Vectors.Count - 1 do
    N.FdKeyValue.Items.Add(Vector3Single(
      { multiple setup pose transform, as bone timeline is relative to setup pose }
      Bone.Node.FdScale.Value[0] * Vectors[I][0],
      Bone.Node.FdScale.Value[1] * Vectors[I][1], 1));

  Node := N;
  inherited;

  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(N.EventValue_changed);
    Route.SetDestinationDirectly(Bone.Node.FdScale.EventIn);
    Container.Routes.Add(Route);
  end;
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
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TOrientationInterpolatorNode;
  Route: TX3DRoute;
begin
  N := TOrientationInterpolatorNode.Create('BoneTimeline_rotate_' + ToX3DName(Bone.Name));
  N.FdKeyValue.Items.Clear;
  for I := 0 to Angles.Count - 1 do
    N.FdKeyValue.Items.Add(Vector4Single(0, 0, 1,
      { add setup pose transform, as bone timeline is relative to setup pose }
      Bone.Node.FdRotation.Value[3] +
      DegToRad(Angles[I])));

  Node := N;
  inherited;

  if Time.Count > 1 then
  begin
    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(N.EventValue_changed);
    Route.SetDestinationDirectly(Bone.Node.FdRotation.EventIn);
    Container.Routes.Add(Route);
  end;
end;

{$endif}
