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
  CastleVectors, CastleTransform, CastleTimeUtils, CastleClassUtils, CastleSectors,
  CastleSoundEngine;

type
  { Behavior that tracks life points, and determines being alive/dead
    for game purposes. }
  TCastleAliveBehavior = class(TCastleBehavior)
  strict private
    FLife: Single;
    FMaxLife: Single;
    FHurtDirection: TVector3;
    FHurtStrength: Single;
    FAttacker: TCastleAliveBehavior;
    FAttackerObserver: TFreeNotificationObserver;
    procedure SetAttacker(const Value: TCastleAliveBehavior);
    procedure AttackerFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    procedure ParentChanged; override;
  public
    const
      { Default value for @link(MaxLife) and @link(Life). }
      DefaultLife = 100.0;

    constructor Create(AOwner: TComponent); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;

    { Hurt given creature, decreasing its @link(Life) by LifeLoss,
      also setting some additional properties that describe the damage.
      These additional properties do not do anything in this class --
      but they may be useful by other effects, e.g. "knockback",
      done by other behaviors.

      Note: If all you want to do is to decrease @link(Life),
      you can also just set @link(Life) property directly.

      @param(AHurtDirection Should be a normalized vector indicating direction
        from which the attack came.

        In this class, it does nothing, merely sets @link(HurtDirection) property.
        Which may be used by other effects.)

      @param(AHurtStrength Describes "strength" of the attack.
        What this "strengh" exactly means is not defined in this class.
        It may cause a "knockback" effect, in which case it may be a knockback
        distance, or a physical force strength, and is meaningful only when
        @link(AHurtDirection) is non-zero.

        In this class, it does nothing, merely sets @link(HurtStrength) property.
        Which may be used by other effects.)

      @param(AnAttacker The other alive creature that caused this damage.
        It may be @nil if no other TCastleAliveBehavior is directly responsible
        for this damage. This may be useful for various purposes,
        for example the victim may become aware of attacker presence
        when it's attacked.

        In this class, it does nothing, merely sets @link(Attacker) property.
        Which may be used by other effects.)
      )}
    procedure Hurt(const LifeLoss: Single;
      const AHurtDirection: TVector3;
      const AHurtStrength: Single;
      const AnAttacker: TCastleAliveBehavior); virtual;

    { Direction from where the last attack came, set by @link(Hurt).
      Zero if there was no specific direction of last attack,
      otherwise a normalized (length 1) vector. }
    property HurtDirection: TVector3 read FHurtDirection;

    { Strengh of the last attack, set by @link(Hurt).
      What this "strengh" exactly means is not defined in this class.
      It may cause a "knockback" effect, in which case it may be a knockback
      distance, or a physical force strength, and is meaningful only when
      @link(AHurtDirection) is non-zero. }
    property HurtStrength: Single read FHurtStrength;

    { Last attacker. Set by the last @link(Hurt) call,
      it may also be set directly. }
    property Attacker: TCastleAliveBehavior read FAttacker write SetAttacker;
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

  { Behavior to play spatial sounds, that automatically follow
    the parent @link(TCastleTransform) transformation. }
  TCastleSoundBehavior = class(TCastleBehavior)
  strict private
    UsedSounds: TSoundList;
    procedure SoundRelease(Sender: TSound);
    function LerpLegsMiddle(const A: Single): TVector3;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Play SoundType where the parent is.

      The exact position is between
      @link(TCastleTransform.Translation Parent.Translation) and
      @link(TCastleTransform.Middle Parent.Middle).
      SoundHeight = 0 means to use
      @link(TCastleTransform.Translation Parent.Translation),
      SoundHeight = 1 means @link(TCastleTransform.Middle Parent.Middle),
      other values imply a linear interpolation between the above two values.

      If TiedToParent then the sound position will be updated
      as the parent will move, and when we will
      be destroyed, sound will stop. If not TiedToParent, then
      the sound will simply be done at parent's position, but then
      it will continue to be played independent of the parent existence
      or position. }
    procedure PlayOnce(const SoundType: TSoundType; const SoundHeight: Single;
      const TiedToParent: boolean = true);
  end;

  { Behavior that allows the creature to move around,
    chasing the enemy, attacking the enemy (by short-range or long-range attack),
    running away from danger.

    Optional dependencies: if present, this behavior will use other behaviors
    on the same parent:

    @unorderedList(
      @item(TCastleSoundBehavior (to play spatial sounds;
        this creature is silent otherwise),)

      @item(TCastleAliveBehavior (to account for the fact that current creature
        may be dead, or badly wounded;
        this creature is assumed indestructible otherwise.))
    ) }
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

    FEnemy: TCastleAliveBehavior;
    FEnemyObserver: TFreeNotificationObserver;

    property StateChangeTime: TFloatTime read FStateChangeTime;
    procedure SetEnemy(const Value: TCastleAliveBehavior);
    procedure EnemyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure PlaySound(const SoundType: TSoundType; const SoundHeight: Single);
  protected
    procedure SetState(const Value: TState); virtual;
    procedure ParentChanged; override;

    { Actually do the attack indicated by AttackAnimation
      and AttackTime and other AttackXxx properties.
      This happens in the middle of AttackAnimation, at the time see AttackTime.

      This can happen only if you defined AttackAnimation for this creature.

      The default implementation here performs a short range attack,
      if enemy is still within reach (AttackMaxDistance; even if it was within
      reach at the start of stateAttack state, the enemy could step back,
      so we need to check AttackMaxDistance again).
      The damage and knockback are defined by TCreatureResource.AttackDamageConst,
      TCreatureResource.AttackDamageRandom, TCreatureResource.AttackHurtStrength. }
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
    function PropertySection(const PropertyName: String): TPropertySection; override;

    { Current state of the creature, automatically changing. }
    property State: TState read FState default stateIdle;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Life, in seconds. }
    property LifeTime: TFloatTime read FLifeTime;
  published
    { Enemy that is being chased and attacked by this creature. }
    property Enemy: TCastleAliveBehavior read FEnemy write SetEnemy;
  end;

implementation

uses SysUtils, Math,
  CastleUtils;

{ TCastleAliveBehavior ------------------------------------------------------- }

constructor TCastleAliveBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FLife := DefaultLife;
  FMaxLife := DefaultLife;
  FAttackerObserver := TFreeNotificationObserver.Create(Self);
  FAttackerObserver.OnFreeNotification := @AttackerFreeNotification;
end;

procedure TCastleAliveBehavior.ParentChanged;
begin
  inherited;
  SingleInstanceOfBehavior;
end;

procedure TCastleAliveBehavior.AttackerFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Attacker := nil;
end;

procedure TCastleAliveBehavior.SetAttacker(const Value: TCastleAliveBehavior);
begin
  if FAttacker <> Value then
  begin
    FAttackerObserver.Observed := Value;
    FAttacker := Value;
  end;
end;

procedure TCastleAliveBehavior.Hurt(const LifeLoss: Single;
  const AHurtDirection: TVector3;
  const AHurtStrength: Single; const AnAttacker: TCastleAliveBehavior);
begin
  Life := Life - LifeLoss;
  FHurtDirection := AHurtDirection;
  FHurtStrength := AHurtStrength;
  Attacker := AnAttacker;
end;

function TCastleAliveBehavior.PropertySection(
  const PropertyName: String): TPropertySection;
begin
  case PropertyName of
    'Life', 'MaxLife':
      Result := psBasic;
    else
      Result := inherited PropertySection(PropertyName);
  end;
end;

{ TSoundData ----------------------------------------------------------------- }

type
  TSoundData = class
  public
    SoundHeight: Single;
  end;

{ TCastleSoundBehavior ------------------------------------------------------- }

constructor TCastleSoundBehavior.Create(AOwner: TComponent);
begin
  inherited;
  UsedSounds := TSoundList.Create(false);
end;

destructor TCastleSoundBehavior.Destroy;
var
  I: Integer;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      UsedSounds[I].UserData.Free;
      UsedSounds[I].UserData := nil;

      { Otherwise OnRelease would call SoundRelease,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here. }
      UsedSounds[I].OnRelease := nil;
      UsedSounds[I].Release;
    end;
    FreeAndNil(UsedSounds);
  end;
  inherited;
end;

procedure TCastleSoundBehavior.SoundRelease(Sender: TSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  UsedSounds.Remove(Sender);
end;

procedure TCastleSoundBehavior.PlayOnce(const SoundType: TSoundType; const SoundHeight: Single;
  const TiedToParent: boolean);
var
  NewSource: TSound;
  SoundPosition: TVector3;
begin
  SoundPosition := LerpLegsMiddle(SoundHeight);
  if Parent.UniqueParent <> nil then // make sound position in world coordinates
    SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);
  NewSource := SoundEngine.Sound3d(SoundType, SoundPosition);
  if TiedToParent and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnRelease := @SoundRelease;
    NewSource.UserData := TSoundData.Create;
  end;
end;

procedure TCastleSoundBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I: Integer;
  SoundPosition: TVector3;
begin
  inherited;
  for I := 0 to UsedSounds.Count - 1 do
  begin
    SoundPosition := LerpLegsMiddle(
      TSoundData(UsedSounds[I].UserData).SoundHeight);
    if Parent.UniqueParent <> nil then // make sound position in world coordinates
      SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);
    UsedSounds[I].Position := SoundPosition;
  end;
end;

function TCastleSoundBehavior.LerpLegsMiddle(const A: Single): TVector3;
begin
  Result := Lerp(A, Parent.Translation, Parent.Middle);
end;

{ TCastleMoveAttackBehavior -------------------------------------------------------- }

constructor TCastleMoveAttackBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FEnemyObserver := TFreeNotificationObserver.Create(Self);
  FEnemyObserver.OnFreeNotification := @EnemyFreeNotification;
end;

procedure TCastleMoveAttackBehavior.EnemyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Enemy := nil;
end;

procedure TCastleMoveAttackBehavior.SetEnemy(const Value: TCastleAliveBehavior);
begin
  if FEnemy <> Value then
  begin
    FEnemyObserver.Observed := Value;
    FEnemy := Value;
  end;
end;

procedure TCastleMoveAttackBehavior.ParentChanged;
var
  Alive: TCastleAliveBehavior;
begin
  inherited;

  SingleInstanceOfBehavior;

  { Note that we don't automatically create TCastleAliveBehavior,
    this would make it complicated when something adds another
    instance of TCastleAliveBehavior -- adding it before or after
    adding TCastleMoveAttackBehavior would have different effect. }
  Alive := Parent.FindBehavior(TCastleAliveBehavior) as TCastleAliveBehavior;

  if (Alive = nil) or (Alive.MaxLife > 0) then
  begin
    FState := stateIdle;
    FStateChangeTime := 0;
  end else
  begin
    { This means that the creature is created already in dead state...
      So we start with stateDie state and set FStateChangeTime to fake
      the fact that creature was killed long time ago.

      This way the creature is created as a dead corpse, without making
      any kind of dying (or wounded) sound or animation. }
    FState := stateDie;
    FStateChangeTime := -1000;
  end;

  WaypointsSaved := TWaypointList.Create(false);
end;

destructor TCastleMoveAttackBehavior.Destroy;
begin
  FreeAndNil(WaypointsSaved);
  inherited;
end;

// TODO do not attack dead Enemy

procedure TCastleMoveAttackBehavior.SetState(const Value: TState);
begin
  if FState <> Value then
  begin
    { Force old box value for Middle and PreferredHeight calculation,
      for a fraction of a second.

      This is crucial for TCastleMoveAttackBehavior.Update logic
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
    if (not Parent.InternalMiddleForceBox) and
       ( ((FState = stateIdle) and (Value = stateWalk)) or
         ((FState = stateWalk) and (Value = stateIdle)) ) then
    begin
      Parent.InternalMiddleForceBox := true;
      Parent.InternalMiddleForceBoxValue := Parent.LocalBoundingBox;
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
          PlaySound(Resource.AttackSoundStart, 1.0);
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

procedure TCastleMoveAttackBehavior.PlaySound(const SoundType: TSoundType; const SoundHeight: Single);
var
  SoundBehavior: TCastleSoundBehavior;
begin
  if SoundType = stNone then Exit;

  SoundBehavior := Parent.FindBehavior(TCastleSoundBehavior) as TCastleSoundBehavior;
  if SoundBehavior <> nil then
    SoundBehavior.PlayOnce(SoundType, SoundHeight);
end;

procedure TCastleMoveAttackBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateResourceFrame;
  var
    StateTime: Single;
  begin
    { Time from the change to this state. }
    StateTime := LifeTime - StateChangeTime;

    case FState of
      stateIdle:
        FResourceFrame.SetFrame(Level, Resource.IdleAnimation, StateTime, true);
      stateWalk:
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
      stateDie:
        FResourceFrame.SetFrame(Level, Resource.DieAnimation, StateTime, false);
      stateDieBack:
        FResourceFrame.SetFrame(Level, Resource.DieBackAnimation, StateTime, false);
      stateHurt:
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
        LastSensedEnemy - Parent.Middle, Parent.Direction);
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
        SetState(stateWalk);
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
        SetState(stateWalk);
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
          So do nothing now. Just stay in stateWalk mode,
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

  FLifeTime += SecondsPassed;

  { eventually turn off InternalMiddleForceBox }
  InternalMiddleForceBox := InternalMiddleForceBox and (LifeTime <= InternalMiddleForceBoxTime);

  if Dead and not (State in [stateDie, stateDieBack]) then
  begin
    if Resource.DieBackAnimation.Defined and WasLastAttackBack then
      SetState(stateDieBack)
    else
      SetState(stateDie);
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
    stateWalk: DoWalk;
    stateAttack: DoAttack;
    stateFireMissile: DoFireMissile;
    stateDie    : DoDie(Resource.DieAnimation.Duration);
    stateDieBack: DoDie(Resource.DieBackAnimation.Duration);
    stateHurt: DoHurt;
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

procedure TCastleMoveAttackBehavior.SetLife(const Value: Single);
begin
  if (not Dead) and
    (Life - Value > Resource.MinLifeLossToHurt * MaxLife) and
    ( (Resource.ChanceToHurt = 1.0) or
      (Random < Resource.ChanceToHurt) ) then
    SetState(stateHurt);
  inherited;
end;

procedure TCastleMoveAttackBehavior.Attack;
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
    Parent.FindRequiredBehavior(TCastleSoundBehavior)
    PlaySound(Resource.AttackSoundHit, 1.0);
    AttackHurt(E);
  end;
end;

procedure TCastleMoveAttackBehavior.FireMissile;
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

procedure TCastleMoveAttackBehavior.UpdateDebugCaption(const Lines: TCastleStringList);
var
  StateName: string;
begin
  inherited;

  case State of
    stateIdle       : StateName := 'Idle';
    stateWalk       : StateName := 'Walk';
    stateAttack     : StateName := 'Attack';
    stateFireMissile: StateName := 'FireMissile';
    stateDie        : StateName := 'Die';
    stateDieBack    : StateName := 'DieBack';
    stateHurt       : StateName := 'Hurt';
    else StateName := Format('Custom State %d', [State]);
  end;
  Lines.Add(StateName);

  if HasLastSensedEnemy then
    Lines.Add(Format('Enemy sensed distance: %f',
      [PointsDistance(LastSensedEnemy, Middle)]));
end;

procedure TCastleMoveAttackBehavior.Hurt(const LifeLoss: Single;
  const HurtDirection: TVector3;
  const AHurtStrength: Single; const Attacker: TCastleAlive);
begin
  inherited Hurt(LifeLoss, HurtDirection,
    AHurtStrength * Resource.KnockBackDistance, Attacker);

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

function TCastleMoveAttackBehavior.PropertySection(
  const PropertyName: String): TPropertySection;
begin
  case PropertyName of
    // 'Life', 'MaxLife': // TODO
    //   Result := psBasic;
    // else
      Result := inherited PropertySection(PropertyName);
  end;
end;

initialization
  RegisterSerializableClass(TCastleAliveBehavior, 'Alive');
  RegisterSerializableClass(TCastleMoveAttackBehavior, 'Move Attack');
end.
