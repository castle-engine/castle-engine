{
  Copyright 2007-2012 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftCreatures;

interface

uses PrecalculatedAnimation, CastleUtils, CastleClassUtils, Classes, CastleScene,
  SysUtils, VectorMath, Boxes3D, Base3D, Frustum,
  RiftWindow, RiftGame, RiftLoadable, CastleTimeUtils, X3DNodes,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif}, CastleColors;

type
  TCreatureState = (csStand, csBored, csWalk);

const
  CreatureStateName: array [TCreatureState] of string =
  ( 'stand', 'bored', 'walk' );

type
  TCreatureAnimation = class
  public
    FileName: string;
    { Created in TCreatureKind.Load }
    Animation: TCastlePrecalculatedAnimation;

    { Note that Changes values for this same animation have always
      possible = false. (i.e. Animations[S].ChangesPossible[S] = false
      for any given S). }
    ChangesPossible: array [TCreatureState] of boolean;
    ChangesDuration: array [TCreatureState] of TFloatTime;
    { Created in TCreatureKind.Load }
    ChangesAnimation: array [TCreatureState] of TCastlePrecalculatedAnimation;
  end;

  TCreatureKind = class(TLoadable)
  private
    FName: string;
    Animations: array [TCreatureState] of TCreatureAnimation;
  protected
    procedure LoadInternal(const BaseLights: TLightInstancesList); override;
    procedure UnLoadInternal; override;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    { Internal creature name, must be simple (needs to be valid XML element
      name and VRML node name, for easy data reading). }
    property Name: string read FName;

    function LoadSteps: Cardinal; override;

    { Loads creature properties from DataConfig file.
      This is normally called by constructor, so you don't have to call it.
      However, it may be useful for debugging purposes to "reload index.xml"
      file, and then you should call this (and probably you should reload
      this creature kind, if it's already loaded; unless you know what
      you're doing :) ). }
    procedure LoadFromConfig;
  end;

  TCreatureKindList = class(specialize TFPGObjectList<TCreatureKind>)
  public
    procedure Load(const BaseLights: TLightInstancesList);
    procedure UnLoad;
  end;

  ECreatureStateChangeNotPossible = class(Exception);

  TCreature = class(T3DOrient)
  private
    FKind: TCreatureKind;

    FState: TCreatureState;
    procedure SetState(const Value: TCreatureState);
  private
    { SetState actually only "schedules" actual state change at the nearest
      comfortable time (namely, when current animation will get to the state
      when it's sensible to start smooth transition using ChangesAnimations
      animation). }
    ScheduledTransitionBegin: boolean;
    ScheduledTransitionBeginNewState: TCreatureState;
    ScheduledTransitionBeginTime: TFloatTime;

    { If @true, then were in transition from old state now.
      This must occur before any new state changes, so
      if both Scheduled* are true,
      ScheduledTransitionEnd is always < ScheduledTransitionBegin ! }
    ScheduledTransitionEnd: boolean;
    ScheduledTransitionEndOldState: TCreatureState;
    ScheduledTransitionEndTime: TFloatTime;

    { Time from last change of state.
      Note that for each SetState, it usually changes two times in Idle:
      1. First, when ScheduledTransitionBegin "kicks in":
         ScheduledTransitionEnd becomes true, and CurrentStateStartTime is set
         to current WorldTime.
      2. Then, at some point ScheduledTransitionEnd changes to false,
         CurrentStateStartTime is reset to current WorldTime again. }
    CurrentStateStartTime: TFloatTime;

    StandTimeToBeBored: TFloatTime;

    procedure RandomizeStandTimeToBeBored;
  protected
    function GetChild: T3D; override;
  public
    constructor Create(AKind: TCreatureKind); reintroduce;
    property Kind: TCreatureKind read FKind;
    property State: TCreatureState read FState write SetState;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { This is called from @link(Idle) when no state change is scheduled.
      Usually, you want to implement AI here, not directly in Idle. }
    procedure IdleNoStateChangeScheduled(const CompSpeed: Single); virtual;

    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

  TPlayer = class(TCreature)
  private
    { This is set and used only if csWalk }
    WantsToWalkPos, WantsToWalkDir: TVector3Single;

    TargetVisualize: TCastleScene;
  public
    constructor Create(AKind: TCreatureKind);
    destructor Destroy; override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    procedure GLContextClose; override;

    procedure LocationChanged;

    procedure WantsToWalk(const Value: TVector3Single);
  end;

var
  CreaturesKinds: TCreatureKindList;
  PlayerKind: TCreatureKind;

implementation

uses CastleLog, ProgressUnit, Math, GL, GLU, CastleGLUtils, CastleWindow,
  RiftData, RiftVideoOptions;

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AName: string);
var
  S: TCreatureState;
begin
  inherited Create;
  FName := AName;

  for S := Low(S) to High(S) do
    Animations[S] := TCreatureAnimation.Create;

  LoadFromConfig;
end;

destructor TCreatureKind.Destroy;
var
  S: TCreatureState;
begin
  for S := Low(S) to High(S) do
    FreeAndNil(Animations[S]);

  inherited;
end;

procedure TCreatureKind.LoadFromConfig;
const
  DefaultDuration = 0.5;
var
  S, SChange: TCreatureState;
  StatePath: string;
begin
  for S := Low(S) to High(S) do
  begin
    StatePath := 'creatures/' + Name + '/' + CreatureStateName[S] + '/';

    Animations[S].FileName := DataFileNameFromConfig(
      DataConfig.GetValue(StatePath + 'file_name', ''));

    for SChange := Low(SChange) to High(SChange) do
    begin
      Animations[S].ChangesPossible[SChange] := (S <> SChange) and
        DataConfig.GetValue(
          StatePath + CreatureStateName[SChange] + '/possible', true);

      Animations[S].ChangesDuration[SChange] :=
        DataConfig.GetFloat(
          StatePath + CreatureStateName[SChange] + '/duration', DefaultDuration);
    end;
  end;
end;

function TCreatureKind.LoadSteps: Cardinal;
var
  S, SChange: TCreatureState;
begin
  Result := inherited LoadSteps + Ord(High(S)) + 1;
  for S := Low(S) to High(S) do
    for SChange := Low(SChange) to High(SChange) do
      if Animations[S].ChangesPossible[SChange] then
        Inc(Result);
end;

procedure TCreatureKind.LoadInternal(const BaseLights: TLightInstancesList);

  procedure AnimationPrepareResources(A: TCastlePrecalculatedAnimation);
  begin
    A.PrepareResources([prRender, prBoundingBox] + prShadowVolume, false, BaseLights,
      1 { MultiSampling = 1, it is ignored as we do not include prScreenEffects });
  end;

const
  ChangeStateScenesPerTime = 30;
  ChangeStateEqualityEpsilon = 0.01;

var
  S, SChange: TCreatureState;
  RootNodes: TX3DNodeList;
  Times: TSingleList;
begin
  inherited;

  if DebugNoCreatures then
  begin
    Progress.Step(3);
    Exit;
  end;

  for S := Low(S) to High(S) do
  begin
    Animations[S].Animation := TCastlePrecalculatedAnimation.CreateCustomCache(nil, GLContextCache);
    Animations[S].Animation.LoadFromFile(Animations[S].FileName, false, true);
    AnimationPrepareResources(Animations[S].Animation);
    Progress.Step;

    if Log then
      WritelnLog('Creature Animation', 'Loaded ' + Animations[S].FileName);
  end;

  RootNodes := nil;
  Times := nil;
  try
    RootNodes := TX3DNodeList.Create(false);
    Times := TSingleList.Create;

    for S := Low(S) to High(S) do
    begin
      for SChange := Low(SChange) to High(SChange) do
        if Animations[S].ChangesPossible[SChange] then
        begin
          RootNodes.Clear;
          RootNodes.Add(Animations[S].Animation.LastScene.RootNode);
          { We must make DeepCopy here, since new TCastlePrecalculatedAnimation
            will always own this node. }
          RootNodes.Add(Animations[SChange].Animation.FirstScene.RootNode.DeepCopy);

          Times.Count := 0;
          Times.Add(0);
          Times.Add(Animations[S].ChangesDuration[SChange]);

          Animations[S].ChangesAnimation[SChange] :=
            TCastlePrecalculatedAnimation.CreateCustomCache(nil, GLContextCache);
          Animations[S].ChangesAnimation[SChange].Load(
            RootNodes, false, Times,
            ChangeStateScenesPerTime,
            ChangeStateEqualityEpsilon);
          Animations[S].ChangesAnimation[SChange].TimeLoop := false;
          Animations[S].ChangesAnimation[SChange].TimeBackwards := false;
          AnimationPrepareResources(Animations[S].ChangesAnimation[SChange]);

          if Log then
            WritelnLog('Creature Animation',
              Format('Loaded change state "%s" -> "%s"',
                [ CreatureStateName[S],
                  CreatureStateName[SChange] ]));

          Progress.Step;
        end;
    end;
  finally
    FreeAndNil(RootNodes);
    FreeAndNil(Times);
  end;
end;

procedure TCreatureKind.UnLoadInternal;
var
  S, SChange: TCreatureState;
begin
  for S := Low(S) to High(S) do
    { this may be called from inherited destructor, so check for <> nil }
    if Animations[S] <> nil then
    begin
      for SChange := Low(SChange) to High(SChange) do
        FreeAndNil(Animations[S].ChangesAnimation[SChange]);
      { ChangesAnimation contain references to a TX3DNode from basic
        animation (LoadInternal does so), so it's safest to release
        basic animation *at the end* (when nothing references it anymore). }
      FreeAndNil(Animations[S].Animation);
    end;

  inherited;
end;

{ TCreatureKindList -------------------------------------------------------- }

procedure TCreatureKindList.Load(const BaseLights: TLightInstancesList);
var
  I: Integer;
  ProgressCount: Cardinal;
begin
  ProgressCount := 0;
  for I := 0 to Count - 1 do
    ProgressCount += Items[I].LoadSteps;

  Progress.Init(ProgressCount, 'Loading creatures');
  try
    for I := 0 to Count - 1 do
      Items[I].Load(BaseLights);
  finally Progress.Fini end;
end;

procedure TCreatureKindList.UnLoad;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UnLoad;
end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AKind: TCreatureKind);
begin
  inherited Create(nil);

  { initialize state fields }
  FState := csStand;
  CurrentStateStartTime := WorldTime;

  FKind := AKind;

  RandomizeStandTimeToBeBored;
end;

procedure TCreature.RandomizeStandTimeToBeBored;
begin
  StandTimeToBeBored := 10 + Random(5);
end;

procedure TCreature.SetState(const Value: TCreatureState);

  { Return nearest multiple of Multiple that is <= Value.
    You can assume that Value >= 0. }
  function RoundFloatDown(const Value, Multiple: TFloatTime): TFloatTime;
  begin
    Result := Floor(Value / Multiple) * Multiple;
  end;

begin
  if Value <> FState then
  begin
    if not Kind.Animations[FState].ChangesPossible[Value] then
      raise ECreatureStateChangeNotPossible.CreateFmt(
        'State change "%s" -> "%s" is not allowed, ' +
        'because in normal circumstances game mechanics ' +
        'never allow it (change "possible" field in index.xml if you ' +
        'want to allow this change)',
        [ CreatureStateName[FState],
          CreatureStateName[Value] ]);

    { Note that using the "SetState sets scheduled state change"
      works OK if you will call SetState multiple times while waiting for
      comfortable time to actually change state: each SetState simply
      erases previously scheduled time, if any.

      So there will be no unnecessary waiting because of multiple "stacked"
      scheduled changes. }
    ScheduledTransitionBegin := true;
    ScheduledTransitionBeginNewState := Value;
    { calculate ScheduledTransitionBeginTime }
    if ScheduledTransitionEnd then
    begin
      { wait for transition to end, then wait for animation to end }
      ScheduledTransitionBeginTime := ScheduledTransitionEndTime +
        Kind.Animations[FState].Animation.TimeEnd;
    end else
    if Kind.Animations[FState].Animation.TimeDuration = 0 then
    begin
      { That's easy, just switch to new state already ! }
      ScheduledTransitionBeginTime := WorldTime;
    end else
    begin
      { So the current state is at WorldTime - CurrentStateStartTime time.
        When will it pass through it's "TimeEnd" point ? }
      ScheduledTransitionBeginTime := CurrentStateStartTime +
        RoundFloatDown(
          WorldTime - CurrentStateStartTime,
          Kind.Animations[FState].Animation.TimeDurationWithBack);

      { Now at ScheduledTransitionBeginTime the animation was in starting
        position, and this is the time <= now. }

      ScheduledTransitionBeginTime += Kind.Animations[FState].Animation.TimeEnd;

      { Now at ScheduledTransitionBeginTime animation will end !
        This is good time to switch... unless it already occurred
        (which is possible
        1. because Animations[FState].Animation.TimeBackward
        are were currently in the "backward" step
        2. because of floating point errors inside RoundFloatDown...).
        So we correct it. If there were no floating point errors inside
        RoundFloatDown, loop below should execute at most once. }
      while ScheduledTransitionBeginTime < WorldTime do
        ScheduledTransitionBeginTime +=
          Kind.Animations[FState].Animation.TimeDurationWithBack;
    end;
  end;
end;

function TCreature.GetChild: T3D;
begin
  if ScheduledTransitionEnd then
    Result := Kind.Animations[
      ScheduledTransitionEndOldState].ChangesAnimation[FState].
      SceneFromTime(WorldTime - CurrentStateStartTime) else
    Result := Kind.Animations[FState].Animation.
      SceneFromTime(WorldTime - CurrentStateStartTime);
end;

procedure TCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  if ScheduledTransitionEnd then
  begin
    if WorldTime > ScheduledTransitionEndTime then
    begin
      ScheduledTransitionEnd := false;
      CurrentStateStartTime := WorldTime;
    end;

    { Note that ScheduledTransitionBeginTime must be >
      ScheduledScheduledTransitionEndTime (if ScheduledTransitionBegin
      is true). That's why it's OK to check ScheduledTransitionBegin
      below only if ScheduledTransitionEnd was false. }
  end else
  if ScheduledTransitionBegin then
  begin
    if WorldTime > ScheduledTransitionBeginTime then
    begin
      ScheduledTransitionBegin := false;
      ScheduledTransitionEnd := true;

      ScheduledTransitionEndOldState := FState;
      FState := ScheduledTransitionBeginNewState;

      ScheduledTransitionEndTime := ScheduledTransitionBeginTime +
        Kind.Animations[ScheduledTransitionEndOldState].ChangesDuration[FState];

      CurrentStateStartTime := WorldTime;
    end;
  end else
    IdleNoStateChangeScheduled(CompSpeed);
end;

procedure TCreature.IdleNoStateChangeScheduled(const CompSpeed: Single);
begin
  if State = csBored then
    State := csStand else
  if (State = csStand) and
     (WorldTime - CurrentStateStartTime > StandTimeToBeBored) then
  begin
    State := csBored;
    RandomizeStandTimeToBeBored;
  end;
end;

procedure TCreature.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  inherited;

  if DebugRenderBoundingGeometry and
    (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    if not Params.RenderTransformIdentity then
    begin
      glPushMatrix;
      glMultMatrix(Params.RenderTransform);
    end;

    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);

      glDrawBox3DWire(BoundingBox);
    glPopAttrib;

    if not Params.RenderTransformIdentity then
      glPopMatrix;
  end;
end;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create(AKind: TCreatureKind);

  function CreateTargetVisualize: TCastleScene;
  var
    Root: TX3DRootNode;
    Shape: TShapeNode;
    Sphere: TSphereNode;
  begin
    Sphere := TSphereNode.Create('', '');
    Sphere.FdRadius.Value := 0.1;

    Shape := TShapeNode.Create('', '');
    Shape.Material := TMaterialNode.Create('', '');
    Shape.Material.FdDiffuseColor.Value := Red3Single;
    Shape.FdGeometry.Value := Sphere;

    Root := TX3DRootNode.Create('', '');
    Root.FdChildren.Add(Shape);

    Result := TCastleScene.Create(nil);
    Result.Load(Root, true);
  end;

begin
  inherited;
  TargetVisualize := CreateTargetVisualize;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(TargetVisualize);
  inherited;
end;

procedure TPlayer.GLContextClose;
begin
  if TargetVisualize <> nil then
    FreeAndNil(TargetVisualize);
end;

procedure TPlayer.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  RotationAxis: TVector3Single;
  AngleToTarget: Single;

  procedure Rotate;
  var
    AngleRotate: Single;
  begin
    { first adjust direction }
    AngleRotate := CompSpeed;
    if AngleToTarget < 0 then
      AngleRotate := Max(-AngleRotate, AngleToTarget) else
      AngleRotate := Min( AngleRotate, AngleToTarget);
    Direction := RotatePointAroundAxisRad(AngleRotate, Direction, RotationAxis);
  end;

  procedure Move;
  const
    MoveSpeed = 0.5;
  var
    MoveDirectionCurrent, MoveDirectionMax: TVector3Single;
    MoveDirectionCurrentScale: Single;
  begin
    { since Position <> WantsToWalkPos, we know that
      MoveDirectionMax is non-zero }
    MoveDirectionMax := VectorSubtract(WantsToWalkPos, Position);
    MoveDirectionCurrentScale := MoveSpeed * CompSpeed / VectorLen(MoveDirectionMax);
    if MoveDirectionCurrentScale >= 1.0 then
    begin
      { This means that
          Position + MoveDirectionMax * MoveDirectionCurrentScale
        would actually get us too far. So we instead just go to target and
        stop there. }
      Position := WantsToWalkPos;
      State := csStand;
    end else
    begin
      MoveDirectionCurrent := VectorScale(MoveDirectionMax, MoveDirectionCurrentScale);
      Position := VectorAdd(Position, MoveDirectionCurrent);
    end;
  end;

var
  IsTargetPos, IsTargetDir: boolean;
begin
  inherited;

  if (ScheduledTransitionEnd or (not ScheduledTransitionBegin)) and
     (State = csWalk) then
  begin
    { Do walking. If walking ends (because target reached, or can't reach target),
      change to csStand. }

    { TODO: check collisions with the scene before changing Position }

    IsTargetPos := VectorsEqual(Position, WantsToWalkPos);
    IsTargetDir := VectorsEqual(Direction, WantsToWalkDir);

    if not IsTargetDir then
    begin
      { compare Direction and WantsToWalkDir with more tolerance }
      RotationAxis := VectorProduct(Direction, WantsToWalkDir);
      AngleToTarget := RotationAngleRadBetweenVectors(Direction, WantsToWalkDir, RotationAxis);
      if Abs(AngleToTarget) < 0.01 then
        IsTargetDir := true;
    end;

    if IsTargetPos and IsTargetDir then
      State := csStand else
    if not IsTargetDir then
      Rotate else
      Move;
  end;
end;

procedure TPlayer.LocationChanged;
begin
  FState := csStand;
  ScheduledTransitionBegin := false;
  ScheduledTransitionEnd := false;
  CurrentStateStartTime := WorldTime;
end;

procedure TPlayer.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  inherited;

  if DebugRenderWantsToWalk and (State = csWalk) then
  begin
    glPushMatrix();
      glTranslatev(WantsToWalkPos);
      TargetVisualize.Render(Frustum.Move(-WantsToWalkPos), Params);
    glPopMatrix();
  end;
end;

procedure TPlayer.WantsToWalk(const Value: TVector3Single);
begin
  WantsToWalkPos := Value;
  WantsToWalkDir := Normalized(WantsToWalkPos - Direction);
  { fix WantsToWalkDir, to avoid wild rotations.
    Without this, our avatar would wildly change up when walking to some
    higher/lower target. This is coupled with the fact that currently
    we accept any clicked position as walk target --- in a real game,
    it would be more limited where you can walk, and so this safeguard
    could be less critical. }
  if VectorsParallel(WantsToWalkDir, Up) then
    WantsToWalkDir := Direction else
    MakeVectorsOrthoOnTheirPlane(WantsToWalkDir, Up);
  State := csWalk;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowClose(Window: TCastleWindowBase);
begin
  FreeAndNil(CreaturesKinds);
end;

initialization
  CreaturesKinds := TCreatureKindList.Create(true);

  PlayerKind := TCreatureKind.Create('player');
  CreaturesKinds.Add(PlayerKind);

  Window.OnCloseList.Add(@WindowClose);
end.
