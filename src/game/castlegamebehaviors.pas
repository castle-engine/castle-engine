{
  Copyright 2006-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Behaviors (TCastleBehavior descendants) useful in games, for example to express
  being alive, or creature intelligence.

  Using these classes in your own games is completely optional,
  for example you can implement "being alive" yourself in your own game trivially,
  without using TCastleAliveBehavior.
  And then you can make your own decisions about various details
  (e.g. is life a float, or integer? does life equal "precisely
  zero" means being still alive, or dead?). }
unit CastleGameBehaviors;

{$I castleconf.inc}

interface

uses Classes,
  CastleVectors, CastleTransform, CastleTimeUtils;

type
  { Behavior that tracks life points, and determines being alive/dead in game. }
  TCastleAliveBehavior = class(TCastleBehavior)
  strict private
    FLife: Single;
    FMaxLife: Single;
  public
    const
      { Default value for @link(MaxLife) and @link(Life). }
      DefaultLife = 100.0;

    constructor Create(AOwner: TComponent); override;

      FLife := DefaultLife;
      FMaxLife := DefaultLife;

    { Hurt given creature, decreasing it's life by LifeLoss,
      setting last attack direction (may be used by knockback or other effects).
      If all you want to do is to decrease Life, you can also just set @link(Life)
      property.

      HurtDirection should be a normalized vector indicating direction
      in which the attack came.

      AKnockbackDistance, if non-zero, indicates to push creature by given
      length in the direction given by HurtDirection.
      Ignored if HurtDirection is zero.

      Attacker is the other alive creature that caused this damage. It may be @nil
      if no other TCastleAliveBehavior is directly responsible for this damage. This may
      be useful for various purposes, for example the victim may become aware
      of attacker presence when it's attacked. }
    procedure Hurt(const LifeLoss: Single;
      const HurtDirection: TVector3;
      const AKnockbackDistance: Single; const Attacker: TCastleAliveBehavior); virtual;
  published
    { Current Life. The object is considered "dead" when this is <= 0. }
    property Life: Single read FLife write FLife default DefaultLife;

    { Maximum amount of life.
      Can be also used for information (to display on player HUDs and such).

      This is not a strict limit on @link(Life),
      i.e. all the code allows the have @link(Life) > @link(MaxLife)
      to account for special game mechanisms,
      like "magic life boost to make health temporarily larger than normal".
      It is up to your game whether such situation will actually happen. }
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultLife;
  end;

  { Behavior that allows the object to move around the game,
    chasing the enemy, attacking the enemy or running away from danger. }
  TCastleMoveAttackBehavior = class(TCastleBehavior)
  public
    type
      TState = (
        stateIdle,
        stateWalk,
        stateAttack,
        stateFireMissile,
        stateDie,
        stateDieBack,
        stateHurt
      );
  private
    FState: TState;
    FLifeTime: TFloatTime;

    { Last State change time, taken from LifeTime. }
    FStateChangeTime: TFloatTime;

    { Time of last State change to stateAttack or stateFireMissile,
      taken from LifeTime. }
    LastAttackTime, LastFireMissileTime: TFloatTime;
    { Whether Attack or FireMissile was already called within this
      stateAttack or stateFireMissile state. }
    AttackDone, FireMissileDone: Boolean;

    HasAlternativeTarget: Boolean;
    AlternativeTarget: TVector3;
    { Time of last setting HasAlternativeTarget to true and AlternativeTarget
      value, taken from LifeTime. Used to not fall into loop
      when the creature tries to walk to AlternativeTarget, and is not
      permanently blocked (so MoveCollision returns true all the time)
      but at the same time the creature can't get close enough to the
      AlternativeTarget. In such case we use this variable to resign from
      AlternativeTarget after some time. }
    AlternativeTargetTime: Single;

    WaypointsSaved_Begin: TSector;
    WaypointsSaved_End: TSector;
    WaypointsSaved: TWaypointList;
    InternalMiddleForceBoxTime: Single;

    { Last known information about enemy. }
    HasLastSensedEnemy: Boolean;
    LastSensedEnemy: TVector3;
    LastSensedEnemySector: TSector;

    property StateChangeTime: Single read FStateChangeTime;
  protected
    procedure SetState(const Value: TState); virtual;

    { Enemy of this creature.
      TODO: The default implementation in this class returns player (if it exists and is still alive).
      Return @nil for no enemy. }
    function Enemy: TCastleAliveBehavior; virtual;

    { Actually do the attack indicated by AttackAnimation
      and AttackTime and other AttackXxx properties.
      This happens in the middle of AttackAnimation, at the time see AttackTime.

      This can happen only if you defined AttackAnimation for this creature.

      The default implementation here performs a short range attack,
      if enemy is still within reach (AttackMaxDistance; even if it was within
      reach at the start of stateAttack state, the enemy could step back,
      so we need to check AttackMaxDistance again).
      The damage and knockback are defined by TCreatureResource.AttackDamageConst,
      TCreatureResource.AttackDamageRandom, TCreatureResource.AttackKnockbackDistance. }
    procedure Attack; virtual;

    { Actually do the attack indicated by FireMissileAnimation
      and FireMissileTime and other FireMissileXxx properties.
      This happens in the middle of FireMissileAnimation, at the time
      FireMissileTime.

      This can happen only if you defined FireMissileAnimation for this creature.

      The default implementation here creates a new creature with resource
      defined by FireMissileName, if FireMissileName is not empty. }
    procedure FireMissile; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property State: TState read FState default stateIdle;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses SysUtils, Math;

{ TCastleMoveAttackBehavior -------------------------------------------------------- }

constructor TCastleMoveAttackBehavior.Create(AOwner: TComponent);
begin
  inherited;

  if MaxLife > 0 then
  begin
    FState := stateIdle;
    FStateChangeTime := 0;
  end else
  begin
    { This means that the creature is created already in dead state...
      So we start with csDie state and set FStateChangeTime to fake
      the fact that creature was killed long time ago.

      This way the creature is created as a dead corpse, without making
      any kind of dying (or wounded) sound or animation. }
    FState := csDie;
    FStateChangeTime := -1000;
  end;

  WaypointsSaved := TWaypointList.Create(false);
end;

destructor TWalkAttackCreature.Destroy;
begin
  FreeAndNil(WaypointsSaved);
  inherited;
end;

function TWalkAttackCreature.Resource: TWalkAttackCreatureResource;
begin
  Result := TWalkAttackCreatureResource(inherited Resource);
end;

function TWalkAttackCreature.Enemy: TCastleAlive;
begin
  Result := Level.GetPlayer as TCastleAlive;
  if (Result <> nil) and Result.Dead then
    Result := nil; { do not attack dead player }
end;

procedure TWalkAttackCreature.SetState(Value: TState);
begin
  if FState <> Value then
  begin
    { Force old box value for Middle and PreferredHeight calculation,
      for a fraction of a second.

      This is crucial for TWalkAttackCreature.Update logic
      that could otherwise sometimes get stuck and continuously switching
      between walk/idle states, because in idle state Middle indicates
      that we should walk (e.g. distance or angle to enemy is not good enough),
      but right after switching to walk the LocalBoundingBox changes
      (because 1st walk animation frame is suddenly different)
      and the distance/angle seems Ok and we switch back to idle and so on,
      in a loop. Once this unfortunate situation is reached, the creature
      is stuck, blinking between two animation frames and two states,
      and never moving (until something else, like enemy (player)
      position changes).

      Safeguards:

      - Don't set to "forced" when it's already forced, as then it could
        cause InternalMiddleForceBoxValue change after each SetState to the box
        from previous state, and we'll be in a similar trouble
        (but with box values always from previous state).
        Trouble (without this safeguard) is reproducible on fps_game
        with knight flying.

      - Don't set to "forced" when switching between other states than idle/walk,
        as the other states logic doesn't allow for such switching
        (states like attack, fireMissile, hurt, die generally continue until
        their time finished; there are no decisions).
        (I didn't actually observed a need for this safeguard so far,
        but it seems reasonable to limit this hack only to idle/walk situation.)
    }
    if (not InternalMiddleForceBox) and
       ( ((FState = stateIdle) and (Value = csWalk)) or
         ((FState = csWalk) and (Value = stateIdle)) ) then
    begin
      InternalMiddleForceBox := true;
      InternalMiddleForceBoxValue := LocalBoundingBox;
      InternalMiddleForceBoxTime := LifeTime + 0.1;
    end;

    { Some states require special finalization here. }
    case FState of
      stateAttack:
        { In case we didn't reach AttackTime, make sure to fire the Attack now. }
        if not AttackDone then
        begin
          AttackDone := true;
          Attack;
        end;
    end;

    FState := Value;
    FStateChangeTime := LifeTime;

    { Some states require special initialization here. }
    case FState of
      stateAttack:
        begin
          Sound3d(Resource.AttackSoundStart, 1.0);
          LastAttackTime := StateChangeTime;
          AttackDone := false;
        end;
      stateFireMissile:
        begin
          LastFireMissileTime := LifeTime;
          FireMissileDone := false;
        end;
    end;
  end;
end;

procedure TWalkAttackCreature.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateResourceFrame;
  var
    StateTime: Single;
  begin
    { Time from the change to this state. }
    StateTime := LifeTime - StateChangeTime;

    case FState of
      stateIdle:
        FResourceFrame.SetFrame(Level, Resource.IdleAnimation, StateTime, true);
      csWalk:
        if Resource.IdleToWalkAnimation.Defined and
           (StateTime < Resource.IdleToWalkAnimation.Duration) then
          FResourceFrame.SetFrame(Level, Resource.IdleToWalkAnimation, StateTime, false)
        else
          FResourceFrame.SetFrame(Level, Resource.WalkAnimation,
            StateTime - Resource.IdleToWalkAnimation.Duration, true);
      stateAttack:
        FResourceFrame.SetFrame(Level, Resource.AttackAnimation, StateTime, false);
      stateFireMissile:
        FResourceFrame.SetFrame(Level, Resource.FireMissileAnimation, StateTime, false);
      csDie:
        FResourceFrame.SetFrame(Level, Resource.DieAnimation, StateTime, false);
      csDieBack:
        FResourceFrame.SetFrame(Level, Resource.DieBackAnimation, StateTime, false);
      csHurt:
        FResourceFrame.SetFrame(Level, Resource.HurtAnimation, StateTime, false);
      else raise EInternalError.Create('FState ?');
    end;
  end;

var
  EnemySensedNow: boolean;
  SqrDistanceToLastSensedEnemy: Single;

  function ActionAllowed(const Animation: T3DResourceAnimation;
    const LastTime, MinDelay, MaxDistance, MaxAngle: Single): boolean;
  var
    AngleBetweenTheDirectionToEnemy: Single;
  begin
    Result := EnemySensedNow and
      Animation.Defined and
      (LifeTime - LastTime > MinDelay) and
      (SqrDistanceToLastSensedEnemy <= Sqr(MaxDistance));

    if Result then
    begin
      { Calculate and check AngleBetweenTheDirectionToEnemy. }
      AngleBetweenTheDirectionToEnemy := AngleRadBetweenVectors(
        LastSensedEnemy - Middle, Direction);
      Result := AngleBetweenTheDirectionToEnemy <= MaxAngle;
    end;
  end;

  function AttackAllowed: boolean;
  begin
    Result := ActionAllowed(Resource.AttackAnimation, LastAttackTime,
      Resource.AttackMinDelay, Resource.AttackMaxDistance, Resource.AttackMaxAngle);
  end;

  function FireMissileAllowed: boolean;
  begin
    Result := ActionAllowed(Resource.FireMissileAnimation, LastFireMissileTime,
      Resource.FireMissileMinDelay, Resource.FireMissileMaxDistance, Resource.FireMissileMaxAngle);
  end;

  procedure CalculateDirectionToTarget(
    const Target: TVector3;
    out DirectionToTarget: TVector3;
    out AngleBetweenDirectionToTarget: Single);
  begin
    { calculate DirectionToTarget }
    DirectionToTarget := Target - Middle;
    if Gravity then
      MakeVectorsOrthoOnTheirPlane(DirectionToTarget, World.GravityUp);

    { calculate AngleBetweenDirectionToTarget }
    AngleBetweenDirectionToTarget :=
      AngleRadBetweenVectors(DirectionToTarget, Direction);
  end;

  { Call this only when HasLastSensedEnemy }
  procedure CalculateDirectionToEnemy(out DirectionToEnemy: TVector3;
    out AngleBetweenDirectionToEnemy: Single);
  begin
    CalculateDirectionToTarget(LastSensedEnemy,
      DirectionToEnemy, AngleBetweenDirectionToEnemy);
  end;

  procedure CalculateDirectionFromEnemy(
    var DirectionFromEnemy: TVector3;
    var AngleBetweenDirectionFromEnemy: Single);
  begin
    CalculateDirectionToEnemy(
      DirectionFromEnemy, AngleBetweenDirectionFromEnemy);
    DirectionFromEnemy := -DirectionFromEnemy;
    AngleBetweenDirectionFromEnemy :=
      Pi - AngleBetweenDirectionFromEnemy;
  end;

  { This changes Direction to be closer to DirectionToTarget.
    Note that it requires the value of AngleBetweenDirectionToTarget
    effectively }
  procedure RotateDirectionToFaceTarget(const DirectionToTarget: TVector3;
    const AngleBetweenDirectionToTarget: Single);
  const
    AngleChangeSpeed = 5.0;
  var
    AngleChange: Single;
    NewDirection: TVector3;
  begin
    if not VectorsParallel(DirectionToTarget, Direction) then
    begin
      { Rotate Direction, to be closer to DirectionToTarget }

      { calculate AngleChange }
      AngleChange := AngleChangeSpeed * SecondsPassed;
      MinVar(AngleChange, AngleBetweenDirectionToTarget);

      NewDirection := RotatePointAroundAxisRad(AngleChange, Direction,
        TVector3.CrossProduct(Direction, DirectionToTarget));

      { Make sure direction for non-flying creatures is orthogonal to GravityUp. }
      if Gravity then
        MakeVectorsOrthoOnTheirPlane(NewDirection, World.GravityUp);
      Direction := NewDirection;
    end;
  end;

  function CloseEnoughToTarget(const Target: TVector3): boolean;
  const
    MinDistanceToTarget = 0.1;
  var
    SqrDistanceToTarget: Single;
  begin
    if not Gravity then
      SqrDistanceToTarget := PointsDistanceSqr(Middle, Target) else
      SqrDistanceToTarget := PointsDistance2DSqr(Middle, Target, World.GravityCoordinate);
    Result :=
      { If creature is ideally at the target
        (for not Flying creatures, this means "ideally under/above the target"),
        then there is no way to get closer to the target.

        We check this with some "epsilon" (MinDistanceToTarget), as usual, to

        1. Avoid the unnecessary moving when Middle is in fact
           close enough to the target, but lack of floating precision
           can't move it really ideally to Target.

        2. In fact, it's not desirable to get exactly at (or under/above)
           the target, because this could cause undesirable rotations
           of the creature Direction (we usually try to make it in
           the Target direction, so when we stand (almost) exactly
           at Target, creature could try to stupidly rotate around itself). }
      SqrDistanceToTarget <= Sqr(MinDistanceToTarget);
  end;

  { Assuming that I want to walk in DesiredDirection direction,
    is it sensible to do this by moving along current Direction ? }
  function WantToWalkInDesiredDirection(
    const AngleBetweenDesiredDirection: Single): boolean;
  const
    MaxAngleToMoveForward = Pi * 60 / 180;
  begin
    Result :=
      { If AngleBetweenDesiredDirection is too large, there is not much point
        in moving in given direction anyway. We should just change our Direction. }
      (AngleBetweenDesiredDirection <= MaxAngleToMoveForward);
  end;

  { Assuming that I want to get to Target position, is it sensible
    to do this by moving along current Direction ?
    This checks whether current Direction points roughly in the
    direction of the Target, and if were not already as close as possible
    to Target. }
  function WantToWalkToTarget(
    const Target: TVector3;
    const AngleBetweenDirectionToTarget: Single): boolean;
  begin
    Result :=
      WantToWalkInDesiredDirection(AngleBetweenDirectionToTarget) and
      { See comments in CloseEnoughToTarget for reasoning why this is needed. }
      (not CloseEnoughToTarget(Target));
  end;

  { This doesn't take into account current Direction,
    it only looks at our and enemy Middle positions,
    and asks "do I want to get closer" ?
    Use only if HasLastSensedEnemy. }
  function WantToShortenDistanceToEnemy: boolean;
  begin
    { Is it wanted to get closer to the LastSensedEnemy?
      Yes, if it will help make AttackAllowed from false to true.
      See AttackAllowed implementation for conditions. }
    Result :=
      { There is no point in trying to get closer,
        if we would activate WantToRunAway *before* we reach a point from which
        we can attack. }
      (not
        ( (Life <= MaxLife * Resource.RunAwayLife) and
          ( (not Resource.FireMissileAnimation.Defined) or (Resource.FireMissileMaxDistance < Resource.RunAwayDistance) ) and
          ( (not Resource.     AttackAnimation.Defined) or (Resource.     AttackMaxDistance < Resource.RunAwayDistance) )
        )
      ) and
      (
        { Try to see enemy by walking to last known enemy position. }
        (not EnemySensedNow) or

        { If EnemySensedNow and SqrDistanceToLastSensedEnemy is small enough,
          there's no point in getting closer to the enemy. In fact, it would
          be bad to get closer to enemy in this case, as this would allow
          enemy to easier attack (shorter distance --- easier to reach with
          short-range weapon, or easier to aim with long-range weapon). }
        (SqrDistanceToLastSensedEnemy > Sqr(Resource.PreferredDistance))
      );
  end;

  { Is it wanted to get closer to the LastSensedEnemy ?
    And (if it's wanted) is it sensible to do this by moving
    along current Direction ?
    Call this only if HasLastSensedEnemy. }
  function WantToWalkToEnemy(
    const AngleBetweenDirectionToEnemy: Single): boolean;
  begin
    Result := WantToShortenDistanceToEnemy and
      WantToWalkToTarget(LastSensedEnemy, AngleBetweenDirectionToEnemy);
  end;

  function WantToRunAway: boolean;
  begin
    { We want to run away whenever HasLastSensedEnemy, not just when EnemySensedNow.
      Otherwise creature that tries to run away could easily get into a loop
      (flickering state), caused by small VisibilityAngle:
      in DoWalk creature would rotate to face away from enemy,
      but after such rotation enemy becomes invisible,
      so WantToRunAway would become false, and we would switch to idle,
      and in DoIdle creature would rotate back toward the enemy.

      So we run away even when we do not see enemy *now*, it's only enough
      to know last enemy position. This actually makes sense in Real World too. }
    Result := HasLastSensedEnemy and
      (Life <= MaxLife * Resource.RunAwayLife) and
      (SqrDistanceToLastSensedEnemy < Sqr(Resource.RunAwayDistance));
  end;

  procedure InitAlternativeTarget;
  var
    Distance: Single;
    I: Integer;
  begin
    Distance := Resource.RandomWalkDistance;

    AlternativeTarget := Middle;
    { Add random values to the AlternativeTarget, but only on the components
      where creature can reliably move. Creature that cannot fly cannot
      move in gravity (UpIndex) direction. }
    for I := 0 to 2 do
      if (not Gravity) or (I <> World.GravityCoordinate) then
        AlternativeTarget.Data[I] := AlternativeTarget.Data[I] + (Random * Distance * 2 - Distance);

    HasAlternativeTarget := true;

    AlternativeTargetTime := LifeTime;
  end;

  { Assuming current state is idle (the actual State value ignored),
    - perform  "idle" AI
    - and tell whether you changed the state to something else (returns @true if remained idle). }
  function DoIdle: Boolean;
  var
    DirectionToEnemy: TVector3;
    AngleBetweenDirectionToEnemy: Single;
  begin
    Result := true;
    if HasLastSensedEnemy then
    begin
      CalculateDirectionToEnemy(DirectionToEnemy, AngleBetweenDirectionToEnemy);

      if FireMissileAllowed then
      begin
        SetState(stateFireMissile);
        Result := false;
      end else
      if AttackAllowed then
      begin
        SetState(stateAttack);
        Result := false;
      end else
      if WantToRunAway or
         WantToWalkToEnemy(AngleBetweenDirectionToEnemy) then
      begin
        SetState(csWalk);
        Result := false;
      end else
      if Gravity and
         (AngleBetweenDirectionToEnemy < 0.01) and
         BoundingBox.Contains2D(LastSensedEnemy, World.GravityCoordinate) then
      begin
        { Then the enemy (or it's last known position) is right above or below us.
          Since we can't fly, we can't get there. Standing in place
          is one possibility, but it's not really good
          - We become easier target to shoot for enemy with the bow.
          - Most importantly, this way enemy (like player) can stand on our head
            and slash us with a sword without any risk. (This was almost
            a standard technique of killing Werewolf or SpiderQueen bosses).
          So we move a little --- just for the sake of moving. }
        SetState(csWalk);
        InitAlternativeTarget;
        Result := false;
      end else
      begin
        { Continue stateIdle state }
        RotateDirectionToFaceTarget(DirectionToEnemy,
          AngleBetweenDirectionToEnemy);
      end;
    end;
  end;

  procedure DoWalk;

    { This performs the real move, which means that it changes Position
      and Middle along the Direction vector.

      This doesn't check whether this is a sensible move, so use this
      only if you know that the creature really wants to go in this Direction.

      This checks only the basic (i.e. always wanted) things:
      - Collision detection (with level and other collidable stuff like
        player and other creatures)
      - For not Flying creatures, also the check to not fall down from high
        is done. }
    function MoveAlongTheDirection: boolean;

      { Don't be stupid, and don't walk where you see you will fall down. }
      function TooHighAboveTheGround(const NewMiddle: TVector3):
        boolean;
      var
        AboveHeight: Single;
      begin
        Result := false;
        if Gravity then
        begin
          Height(NewMiddle, AboveHeight);
          if AboveHeight > Resource.MaxHeightAcceptableToFall + PreferredHeight then
            Result := true;
        end;
      end;

    begin
      Result :=
        { First check to not step into some deep fall.
          Note that I'm not using here NewMiddle
          (that will be calculated later by Move)
          because they are too close to Middle to be good to test against.
          I'm calculating here where I would get after 0.2 second. }
        (not TooHighAboveTheGround(Middle + Direction * (Resource.MoveSpeed * 0.2))) and

        { Use Move without wall-sliding here.
          Things using MoveAlongTheDirection depend on the fact that
          MoveAlongTheDirection will return false
          if no further way is possible (and wall-sliding would try instead
          to return true and correct target position).

          Our trick with "AlternativeTarget" should handle
          eventual problems with the track of creature, so wall-sliding
          should not be needed. }
        Move(Direction * (Resource.MoveSpeed * SecondsPassed), false, false);
    end;

    { Go the way to LastSensedEnemy, *not* by using waypoints.
      Assumes HasLastSensedEnemy. }
    procedure WalkNormal;
    var
      DirectionToTarget: TVector3;
      AngleBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToEnemy(DirectionToTarget,
        AngleBetweenDirectionToTarget);

      if WantToWalkToEnemy(AngleBetweenDirectionToTarget) then
      begin
        if not MoveAlongTheDirection then
        begin
          { Not able to get to enemy this way ? Maybe there exists
            some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end else
      begin
        { I don't want to walk anymore. So just stand stil. }
        SetState(stateIdle);
        Exit;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleBetweenDirectionToTarget);
    end;

    procedure WalkToWaypoint(const Target: TVector3);
    var
      DirectionToTarget: TVector3;
      AngleBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToTarget(Target, DirectionToTarget,
        AngleBetweenDirectionToTarget);

      if WantToShortenDistanceToEnemy then
      begin
        if not MoveAlongTheDirection then
        begin
          { Not able to get to waypoint this way ? Maybe there exists
            some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end else
      begin
        { I don't want to walk anymore. So just stand stil. }
        SetState(stateIdle);
        Exit;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleBetweenDirectionToTarget);
    end;

  const
    ProbabilityToTryAnotherAlternativeTarget = 0.5;
    AngleBetweenDirectionToTargetToResign = Pi * 1 / 180;
    MaxTimeForAlternativeTarget = 5.0;
  var
    DirectionToTarget: TVector3;
    AngleBetweenDirectionToTarget: Single;
    SectorNow: TSector;
    UseWalkNormal: boolean;
  begin
    if HasAlternativeTarget then
    begin
      if CloseEnoughToTarget(AlternativeTarget) or
         (LifeTime - AlternativeTargetTime > MaxTimeForAlternativeTarget) then
      begin
        HasAlternativeTarget := false;
        Exit;
      end;

      CalculateDirectionToTarget(AlternativeTarget,
        DirectionToTarget, AngleBetweenDirectionToTarget);

      if WantToWalkToTarget(AlternativeTarget,
        AngleBetweenDirectionToTarget) then
      begin
        { Note that MoveAlongTheDirection returns false when
          moving along the current Direction is not good.
          But maybe moving along the DirectionToTarget is possible ?
          So we shouldn't just resign from current AlternativeTarget
          so fast --- maybe it's good, but we have to adjust
          our Direction a little more. That's why I use
          AngleBetweenDirectionToTargetToResign.

          Note that for normal moving (i.e. toward LastSensedEnemy,
          not AlternativeTarget) we in this case just change state
          to stateIdle, and this allows creature to rotate in stateIdle
          state. }
        if (not MoveAlongTheDirection) and
           (AngleBetweenDirectionToTarget <=
             AngleBetweenDirectionToTargetToResign) then
        begin
          if Random <= ProbabilityToTryAnotherAlternativeTarget then
          begin
            { Try yet another alternative way. }
            InitAlternativeTarget;
            Exit;
          end else
          begin
            HasAlternativeTarget := false;
            Exit;
          end;
        end;
      end else
      begin
        { We know that WantToWalkToTarget may return false only because
          were not directed enough for AlternativeTarget.
          (because we already eliminated CloseEnoughToTarget case above).
          In each DoWalk call we will gradually fix this,
          by RotateDirectionToFaceTarget below.
          So do nothing now. Just stay in csWalk mode,
          and do RotateDirectionToFaceTarget below. }
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleBetweenDirectionToTarget);
    end else
    if WantToRunAway then
    begin
      CalculateDirectionFromEnemy(DirectionToTarget,
        AngleBetweenDirectionToTarget);

      if WantToWalkInDesiredDirection(AngleBetweenDirectionToTarget) then
      begin
        if (not MoveAlongTheDirection) and
           (AngleBetweenDirectionToTarget <=
             AngleBetweenDirectionToTargetToResign) then
        begin
          { Maybe there exists some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleBetweenDirectionToTarget);
    end else
    begin
      if not HasLastSensedEnemy then
      begin
        { Nowhere to go; so just stay here. }
        SetState(stateIdle);
        Exit;
      end;

      UseWalkNormal := true;

      SectorNow := Sector;
      if (SectorNow <> LastSensedEnemySector) and
         (SectorNow <> nil) and
         (LastSensedEnemySector <> nil) then
      begin
        { The way to LastSensedEnemy is using waypoints. }

        { Recalculate WaypointsSaved.
          Note that I recalculate only when SectorNow or
          LastSensedEnemySector changed. }
        if (SectorNow <> WaypointsSaved_Begin) or
           (LastSensedEnemySector <> WaypointsSaved_End) then
        begin
          WaypointsSaved_Begin := SectorNow;
          WaypointsSaved_End := LastSensedEnemySector;
          TSectorList.FindWay(WaypointsSaved_Begin, WaypointsSaved_End,
            WaypointsSaved);
        end;

        if WaypointsSaved.Count <> 0 then
        begin
          { There is a space around the waypoint that is within
            more than one sector. SectorWithPoint will then answer
            with any (it's not specified which) sector that has
            given position. This is problematic, because this means
            that the creature will be forced to go once again to the same
            waypoint that it's already at... This way there could arise
            a situation when the creature gets stuck at some waypoint,
            because we constantly detect that it must pass through this
            waypoint. The check for CloseEnoughToTarget below prevents this. }
          if CloseEnoughToTarget(WaypointsSaved[0].Position) then
          begin
            if WaypointsSaved.Count > 1 then
            begin
              WalkToWaypoint(WaypointsSaved[1].Position);
              UseWalkNormal := false;
            end;
          end else
          begin
            WalkToWaypoint(WaypointsSaved[0].Position);
            UseWalkNormal := false;
          end;
        end;
      end;

      if UseWalkNormal then
        WalkNormal;
    end;

    if FireMissileAllowed then
      SetState(stateFireMissile) else
    if AttackAllowed then
      SetState(stateAttack);
  end;

  { Go to the default state, like "idle".
    Doing this instead of SetState(stateIdle) avoids switching to "idle" just for
    a single frame, which looks bad (animation visibly jumps for 1 frame,
    and also animation blending is broken by such 1-frame change,
    since our animation blending now can only transition from last to next animation). }
  procedure BackToDefaultState;
  begin
    if DoIdle then
      SetState(stateIdle);
  end;

  procedure DoAttack;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;
    if (not AttackDone) and (StateTime >= Resource.AttackTime) then
    begin
      AttackDone := true;
      Attack;
    end;
    if StateTime > Resource.AttackAnimation.Duration then
      BackToDefaultState;
  end;

  procedure DoFireMissile;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;
    if (not FireMissileDone) and (StateTime >= Resource.FireMissileTime) then
    begin
      FireMissileDone := true;
      FireMissile;
    end;
    if StateTime > Resource.FireMissileAnimation.Duration then
      BackToDefaultState;
  end;

  procedure DoHurt;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;

    if StateTime > Resource.HurtAnimation.Duration then
    begin
      CancelKnockback;
      BackToDefaultState;
    end;
  end;

  { @true if last attack was from the back of the creature,
    @false if from the front or unknown (when LastHurtDirection is zero). }
  function WasLastAttackBack: boolean;
  begin
    try
      Result := AngleRadBetweenVectors(LastHurtDirection, Direction) < Pi/2;
    except
      on EVectorInvalidOp do Result := false;
    end;
  end;

  procedure DoDie(const AnimationDuration: Single);
  begin
    if Resource.RemoveDead and
      (LifeTime - StateChangeTime > AnimationDuration) then
      RemoveMe := rtRemoveAndFree;
  end;

  procedure UpdateDebugTransform;
  begin
    if RenderDebug then
    begin
      if FDebugAlternativeTargetAxis = nil then
      begin
        FDebugAlternativeTargetAxis := TDebugAxis.Create(Self, BlueRGB);
        FDebugTransform.ParentSpace.AddChildren(FDebugAlternativeTargetAxis.Root);
        FDebugTransform.ChangedScene;
      end;

      FDebugAlternativeTargetAxis.Render := HasAlternativeTarget;
      FDebugAlternativeTargetAxis.ScaleFromBox := BoundingBox;
      FDebugAlternativeTargetAxis.Position := AlternativeTarget;

      if FDebugLastSensedEnemyAxis = nil then
      begin
        FDebugLastSensedEnemyAxis := TDebugAxis.Create(Self, RedRGB);
        FDebugTransform.ParentSpace.AddChildren(FDebugLastSensedEnemyAxis.Root);
        FDebugTransform.ChangedScene;
      end;

      FDebugLastSensedEnemyAxis.Render := HasLastSensedEnemy;
      FDebugLastSensedEnemyAxis.ScaleFromBox := BoundingBox;
      FDebugLastSensedEnemyAxis.Position := LastSensedEnemy;
    end;
  end;

var
  E: TCastleTransform;
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  { eventually turn off InternalMiddleForceBox }
  InternalMiddleForceBox := InternalMiddleForceBox and (LifeTime <= InternalMiddleForceBoxTime);

  if Dead and not (State in [csDie, csDieBack]) then
  begin
    if Resource.DieBackAnimation.Defined and WasLastAttackBack then
      SetState(csDieBack)
    else
      SetState(csDie);
    UpdateResourceFrame;
    Exit;
  end;

  E := Enemy;
  EnemySensedNow := (E <> nil) and (
    (
      { enemy seen }
      (AngleRadBetweenNormals(E.Middle - Middle, Direction) <=
       Resource.VisibilityAngle / 2) and
      LineOfSight(Middle, E.Middle)
    ) or
    (
      { enemy smelled }
      PointsDistanceSqr(E.Middle, Middle) < Sqr(Resource.SmellDistance)
    ) );
  if EnemySensedNow then
  begin
    HasLastSensedEnemy := true;
    LastSensedEnemy := E.Middle;
    LastSensedEnemySector := Sector(E);
  end;

  if HasLastSensedEnemy then
  begin
    SqrDistanceToLastSensedEnemy := PointsDistanceSqr(LastSensedEnemy, Middle);
  end;

  case FState of
    stateIdle: DoIdle;
    csWalk: DoWalk;
    stateAttack: DoAttack;
    stateFireMissile: DoFireMissile;
    csDie    : DoDie(Resource.DieAnimation.Duration);
    csDieBack: DoDie(Resource.DieBackAnimation.Duration);
    csHurt: DoHurt;
  end;

  { Flying creatures may change their direction vector freely.
    However, we want them to keep their sense of up --- they should try
    to keep straight, so their up vector should try to remain close
    to GravityUp, not just change wildly.

    For non-flying, this is not needed, as then Up should always remain equal
    to initial value, which is GravityUp. }
  if not Gravity then
    UpPrefer(World.GravityUp);

  UpdateDebugTransform;
  UpdateResourceFrame;
end;

procedure TWalkAttackCreature.SetLife(const Value: Single);
begin
  if (not Dead) and
    (Life - Value > Resource.MinLifeLossToHurt * MaxLife) and
    ( (Resource.ChanceToHurt = 1.0) or
      (Random < Resource.ChanceToHurt) ) then
    SetState(csHurt);
  inherited;
end;

procedure TWalkAttackCreature.Attack;
var
  E: TCastleAlive;

  function ShortRangeAttackHits: boolean;
  var
    B, EB: TBox3D;
    DistanceLength, DistanceIncrease: Single;
  begin
    if E = nil then Exit(false); { no enemy to hurt }
    B := BoundingBox;
    EB := E.BoundingBox;

    { We would like to check collision between EB and our B translated
      by our Direction now, i.e.
        B.Translate(Direction * ???).Collision(EB)
      But how much should be scale Direction, i.e. what to put for "???" ?
      It must be large enough to compensate even large Resource.AttackMaxDistance,
      it must be small enough so that enemy should not be able to avoid
      our attacks just by standing very close to the creature.

      So we have to check a couple of bounding boxes.
      If we move our boxes by Box3DMinSize(B), we're sure that
      each box will stick to the previous and next. But maybe
      there will be some areas around the sticking points ?
      So B.MinSize / 2 seems safe. }
    DistanceIncrease := B.MinSize / 2;

    DistanceLength := DistanceIncrease;
    while DistanceLength < Resource.AttackMaxDistance do
    begin
      if B.Translate(Direction * DistanceLength).Collision(EB) then
        Exit(true);
      DistanceLength := DistanceLength + DistanceIncrease;
    end;

    { Check one last time for Resource.AttackMaxDistance }
    Result := B.Translate(Direction * Resource.AttackMaxDistance).Collision(EB);
  end;

begin
  E := Enemy;
  if ShortRangeAttackHits then
  begin
    Sound3d(Resource.AttackSoundHit, 1.0);
    AttackHurt(E);
  end;
end;

procedure TWalkAttackCreature.FireMissile;
var
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3;
begin
  if (Resource.FireMissileName <> '') and HasLastSensedEnemy then
  begin
    MissilePosition := LerpLegsMiddle(Resource.FireMissileHeight);
    MissileDirection := LastSensedEnemy - MissilePosition;
    Missile := (Resources.FindName(Resource.FireMissileName) as TCreatureResource).
      CreateCreature(Level, MissilePosition, MissileDirection);
    Missile.Sound3d(Resource.FireMissileSound, 0.0);
  end;
end;

procedure TWalkAttackCreature.UpdateDebugCaption(const Lines: TCastleStringList);
var
  StateName: string;
begin
  inherited;

  case State of
    stateIdle       : StateName := 'Idle';
    csWalk       : StateName := 'Walk';
    stateAttack     : StateName := 'Attack';
    stateFireMissile: StateName := 'FireMissile';
    csDie        : StateName := 'Die';
    csDieBack    : StateName := 'DieBack';
    csHurt       : StateName := 'Hurt';
    else StateName := Format('Custom State %d', [State]);
  end;
  Lines.Add(StateName);

  if HasLastSensedEnemy then
    Lines.Add(Format('Enemy sensed distance: %f',
      [PointsDistance(LastSensedEnemy, Middle)]));
end;

procedure TWalkAttackCreature.Hurt(const LifeLoss: Single;
  const HurtDirection: TVector3;
  const AKnockbackDistance: Single; const Attacker: TCastleAlive);
begin
  inherited Hurt(LifeLoss, HurtDirection,
    AKnockbackDistance * Resource.KnockBackDistance, Attacker);

  { If attacked by Enemy, update LastSensedEnemy fields.
    This way when you attack a creature from the back, it will turn around
    and fight you. }
  if (Attacker <> nil) and (Attacker = Enemy) then
  begin
    HasLastSensedEnemy := true;
    LastSensedEnemy := Attacker.Middle;
    LastSensedEnemySector := Sector(Attacker);
  end;
end;

initialization
  RegisterSerializableClass(TCastleAliveBehavior, 'Alive');
  RegisterSerializableClass(TCastleMoveAttackBehavior, 'Move Attack');
end.
