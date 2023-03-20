{
  Copyright 2006-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Creatures. }
unit CastleCreatures
  deprecated 'express creature AI as TCastleBehavior';

{$I castleconf.inc}

interface

{$warnings off} // using deprecated CastleResources in deprecated
uses Classes, Generics.Collections,
  CastleVectors, CastleBoxes, CastleClassUtils, CastleUtils, CastleScene,
  CastleStringUtils, CastleResources, CastleXMLConfig, CastleTransform,
  CastleTransformExtra, CastleSoundEngine, CastleFrustum, X3DNodes, CastleColors,
  CastleDebugTransform, CastleSectors, CastleBehaviors;
{$warnings on}

type
  TCreatureState = type Integer;

const
  csIdle        = TCreatureState(0);
  csWalk        = TCreatureState(1);
  csAttack      = TCreatureState(2);
  csFireMissile = TCreatureState(3);
  csDie         = TCreatureState(4);
  csDieBack     = TCreatureState(5);
  csHurt        = TCreatureState(6);
  { Maximum TCreatureState value reserved by CastleCreatures unit. }
  csMax = csHurt;

type
  TCreature = class;

  TCreatureClass = class of TCreature;

  { Basic abstract resource used by all creatures.
    Basic creature can walk or fly, has life and can be hurt,
    can fall down because of gravity and such.

    A "resource" is an information shared by all creatures of given type,
    for example you can have two instances of TCreatureResource: Werewolf
    and Knight. Actually, they would have to be instances of one of
    the TCreatureResource descendants, like TWalkAttackCreatureResource,
    as TCreatureResource is abstract. Using them you can create and place
    on your your level milions of actual werewolves and knights
    (for example instances of TWalkAttackCreature).
    Every werewolf on the level will have potentially different life and
    state (attacking, walking), but all werewolves will share the same
    resource, so e.g. all werewolves will use the same dying animation
    (TWalkAttackCreatureResource.DieAnimation) and dying sound
    (TCreatureResource.SoundDie).

    Note that some of the information stored in resource
    is only used to initialize new creatures and can be changed later
    during creature life, for example TCreatureResource.DefaultMaxLife
    and TCreatureResource.Flying. }
  TCreatureResource = class(T3DResource)
  strict private
    FFlying: boolean;
    FSoundSuddenPain: TCastleSound;
    FSoundDie: TCastleSound;
    FSoundDieTiedToCreature: boolean;
    FDefaultMaxLife: Single;
    FKnockBackDistance: Single;
    FKnockBackSpeed: Single;
    FCollidesWhenDead: boolean;
    FScaleMin, FScaleMax: Single;

    FRadiusOverride: Single;

    FAttackDamageConst: Single;
    FAttackDamageRandom: Single;
    FAttackKnockbackDistance: Single;

    FFallMinHeightToSound: Single;
    FFallMinHeightToDamage: Single;
    FFallDamageScaleMin: Single;
    FFallDamageScaleMax: Single;
    FFallSound: TCastleSound;

    FMiddleHeight: Single;
  protected
    { Sphere radius for collision detection for alive creatures.
      Must be something <> 0 for collision detection to work.
      Defining it in the creature resource.xml file
      (as radius="xxx" attribute on the root <resource> element)
      overrides the results of this function. }
    function RadiusCalculate(const GravityUp: TVector3): Single; virtual;

    { Can the "up" vector be skewed, that is: not equal to gravity up vector.
      This is used when creating creature in CreateCreature.
      The default implementation here returns @true,
      which allows creature model to point slightly upward/downward.

      Override this to return @false if given creature kind for some reason cannot
      have up vector different. For example, TWalkAttackCreature AI
      assumes that the non-flying creature is always standing up.
      For now, non-flying TWalkAttackCreature cannot "stand up" before walking,
      in case it's up vector gets skewed. }
    function FlexibleUp: boolean; virtual;
  public
    const
      { Default value for TCreatureResource.DefaultMaxLife.
        Yes, it's not a typo, this identifier starts with "DefaultDefault". }
      DefaultDefaultMaxLife = 100.0;
      DefaultFlying = false;
      DefaultKnockBackDistance = 4.0;
      DefaultSoundDieTiedToCreature = true;
      DefaultAttackDamageConst = 0.0;
      DefaultAttackDamageRandom = 0.0;
      DefaultAttackKnockbackDistance = 0.0;
      DefaultFallMinHeightToSound = 1.0;
      DefaultFallSoundName = 'creature_fall';

    constructor Create(AOwner: TComponent); override;

    { Flying creatures are not affected by gravity and
      (in case of TWalkAttackCreatureResource) their move direction is free.

      For all creatures, TCreature.Gravity (inherited from TCastleTransform.Gravity)
      is set to @code("not Flying") at creation. (Except TMissileCreatureResource,
      that has special approach to gravity,
      see TMissileCreatureResource.DirectionFallSpeed.)

      For TWalkAttackCreatureResource, additionally Flying allows to move
      freely, while non-flying creatures are constrained to move
      (and think about moving) only horizontally.

      You can always change the Gravity property of a particular creature
      during it's lifetime, so a creature may start/stop flying during game.
      For example, this is how you can let your creatures to use jetpack and such.
      Be careful about creature @link(Radius) and @link(MiddleHeight) properties
      in this case, make sure that the values (explicitly set or automatically
      calculated) are suitable for both flying and non-flying states. }
    property Flying: boolean read FFlying write FFlying default DefaultFlying;

    property SoundSuddenPain: TCastleSound
      read FSoundSuddenPain write FSoundSuddenPain;

    property SoundDie: TCastleSound
      read FSoundDie write FSoundDie;

    { See TCreature.Sound3d TiedToCreature parameter docs.
      You can set this to false if you want SoundDie to last even
      after the creature object was destroyed. }
    property SoundDieTiedToCreature: boolean
      read FSoundDieTiedToCreature write FSoundDieTiedToCreature
      default DefaultSoundDieTiedToCreature;

    { The default MaxLife for creatures of this resource.

      Note that you can always override it for a particular creature
      instance. You can use a special creature placeholder with
      a specific starting life value
      (see TLevel.Load for placeholders docs,
      and see https://castle-engine.io/manual_high_level_3d_classes.php
      about the creature placeholders).
      Or you can use CreateCreature overloaded version that takes extra MaxLife
      parameter.

      So this is only a "suggested" default for MaxLife of this creature. }
    property DefaultMaxLife: Single
      read FDefaultMaxLife write FDefaultMaxLife {$ifdef FPC}default DefaultDefaultMaxLife{$endif};

    { Create the TCreature instance using this resource.
      Uses TCreature descendant that can best cooperate with this resource,
      e.g. if this resource has settings for short-range fight,
      then the TCreature instance will be able to short-range fight.

      The creature is added to the World, and is owned by World.

      This is the only way to create TCreature instances.

      ADirection passed here is normalized, and then used
      as initial TCreature.Direction value.

      @groupBegin }
    function CreateCreature(
      const ALevel: TAbstractLevel;
      const APosition, ADirection: TVector3;
      const MaxLife: Single): TCreature; overload; virtual;
    function CreateCreature(
      const ALevel: TAbstractLevel;
      const APosition, ADirection: TVector3): TCreature; overload;
    { @groupEnd }

    { Instantiate creature placeholder, by calling CreateCreature. }
    procedure InstantiatePlaceholder(
      const ALevel: TAbstractLevel;
      const APosition, ADirection: TVector3;
      const NumberPresent: boolean; const Number: Int64); override;

    function CreatureClass: TCreatureClass; virtual; abstract;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;

    { Distance this creature is knocked back when hurt (should reflect
      the creature weight, how easy it is to push this creature).

      Will always be multiplied by the knocking distance of the weapon that
      caused the push (which should reflect the force of the weapon blow),
      see TItemWeaponResource.AttackKnockbackDistance.

      Only for TWalkAttackCreature, the final distance the creature
      is knocked back is capped
      by the time of the HurtAnimation (HurtAnimation.Duration).
      When the hurt animation ends, the knockback effect always ends,
      even if the distance (creature * weapon) indicates it should be knocked
      further. Otherwise knockback would work on standing creature,
      which could look bad. This may be changed some day. }
    property KnockBackDistance: Single
      read FKnockBackDistance write FKnockBackDistance
      {$ifdef FPC}default DefaultKnockBackDistance{$endif};

    { See TCastleAlive.KnockBackSpeed. }
    {$warnings off} // using deprecated in deprecated
    property KnockBackSpeed: Single
      read FKnockBackSpeed write FKnockBackSpeed
      {$ifdef FPC}default TCastleAlive.DefaultKnockBackSpeed{$endif};
    {$warnings on}

    { By default dead creatures (corpses) don't collide, this usually looks better. }
    property CollidesWhenDead: boolean
      read FCollidesWhenDead write FCollidesWhenDead default false;

    { Minimum scale when spawning, must be <= ScaleMax.
      When we spawn a creature, we set it's scale to random number in
      [ScaleMin, ScaleMax] range. By default both ScaleMin and ScaleMax are 1,
      resulting in no scaling. }
    property ScaleMin: Single read FScaleMin write FScaleMin {$ifdef FPC}default 1{$endif};

    { Maximum scale when spawning, must be >= ScaleMin.
      When we spawn a creature, we set it's scale to random number in
      [ScaleMin, ScaleMax] range. By default both ScaleMin and ScaleMax are 1,
      resulting in no scaling. }
    property ScaleMax: Single read FScaleMax write FScaleMax {$ifdef FPC}default 1{$endif};

    { Attack damage.
      Used by the creatures that actually do some kind of direct attack.
      For example it is used for short-range attack by TWalkAttackCreatureResource
      (if TWalkAttackCreatureResource.AttackAnimation defined)
      and for hit of TMissileCreatureResource.

      The damage dealt is a random float in the range
      [AttackDamageConst, AttackDamageConst + AttackDamageRandom].

      Both AttackDamageConst and AttackDamageRandom must be >= 0.

      @groupBegin }
    property AttackDamageConst: Single
      read FAttackDamageConst write FAttackDamageConst
      {$ifdef FPC}default DefaultAttackDamageConst{$endif};

    { Attack damage, see @link(AttackDamageConst) for documentation. }
    property AttackDamageRandom: Single
      read FAttackDamageRandom write FAttackDamageRandom
      {$ifdef FPC}default DefaultAttackDamageRandom{$endif};
    { @groupEnd }

    { Attack knockback (how far will the victim be pushed back).
      Used by the creatures that actually do some kind of direct attack.
      For example it is used for short-range attack by TWalkAttackCreatureResource
      (if TWalkAttackCreatureResource.AttackAnimation defined)
      and for hit of TMissileCreatureResource.

      Must be >= 0. Value equal exactly 0 disables any knockback. }
    property AttackKnockbackDistance: Single
      read FAttackKnockbackDistance write FAttackKnockbackDistance
      {$ifdef FPC}default DefaultAttackKnockbackDistance{$endif};

    { Height of the eyes of the creature,
      used for various collision detection routines.
      See TCastleTransform.MiddleHeight for a precise documentation.

      Game developers can use the RenderDebug variable to easily
      visualize the bounding sphere (and other things) around resources.
      The bounding sphere is centered around the point derived from MiddleHeight
      setting and with given creature radius
      (given in resource.xml, or automatically calculated by
      @link(TCreatureResource.RadiusCalculate)). }
    property MiddleHeight: Single
      read FMiddleHeight write FMiddleHeight
      {$ifdef FPC}default TCastleTransform.DefaultMiddleHeight{$endif};

    { When creature is falling down, it needs to fall at least the given distance
      to make a "hurt" sound. }
    property FallMinHeightToSound: Single
      read FFallMinHeightToSound write FFallMinHeightToSound
      {$ifdef FPC}default DefaultFallMinHeightToSound{$endif};

    { When creature is falling down, it needs to fall at least the given distance
      to get some damage from the fall.
      The amount of damage is determined by @link(FallDamageScaleMin),
      @link(FallDamageScaleMax). }
    property FallMinHeightToDamage: Single
      read FFallMinHeightToDamage write FFallMinHeightToDamage
      {$ifdef FPC}default DefaultFallMinHeightToDamage{$endif};

    { When creature is falling down (at least for @link(FallMinHeightToDamage) distance),
      it will take a random damage in range [FallDamageScaleMin, FallDamageScaleMax]. }
    property FallDamageScaleMin: Single
      read FFallDamageScaleMin write FFallDamageScaleMin
      {$ifdef FPC}default DefaultFallDamageScaleMin{$endif};

    { When creature is falling down (at least for @link(FallMinHeightToDamage) distance),
      it will take a random damage in range [FallDamageScaleMin, FallDamageScaleMax]. }
    property FallDamageScaleMax: Single
      read FFallDamageScaleMax write FFallDamageScaleMax
      {$ifdef FPC}default DefaultFallDamageScaleMax{$endif};

    { Sound when falling.
      The default is the sound named 'creature_fall'. }
    property FallSound: TCastleSound
      read FFallSound write FFallSound;

    { Radius used for resolving (some) collisions with the alive creature.
      This can be read from the @code(resource.xml) file.
      When zero, the radius is automatically calculated looking at the
      3D model bounding box, and taking into account gravity direction,
      see @link(TCreatureResource.RadiusCalculate). }
    property RadiusOverride: Single
      read FRadiusOverride write FRadiusOverride;

    { Sphere radius for collision detection for alive creatures.
      Must be something <> 0 for collision detection to work.

      You can define it in the creature resource.xml file,
      by setting radius="xxx" attribute on the root <resource> element.

      If it's not defined (or zero) in resource.xml file,
      then we use automatically calculated radius using RadiusCalculate,
      that is adjusted to the bounding box of the animation.

      This radius is used only for alive creatures, because:

      @unorderedList(
        @item(It would cause incorrect results on many dead creatures.
          Dead creatures usually have very different boxes than alive
          (tall alive humanoid creature probably has a small flat bounding
          box when it's lying dead on the ground). So the same radius would not work
          nicely.)

        @item(It is not necessary to use sphere bounding volumes for dead creatures.
          The main advantage of sphere bounding volumes (over box bounding volumes)
          is for moving (alive) creatures:
          sphere better avoids getting stuck into obstacles (because an animation
          can change the bounding box at any moment).)

        @item(Actually, the results look best when dead creatures don't collide at all.)
      )

      The sphere center is the Middle point ("eye position") of the given creature.
      If the creature may be affected by gravity then
      make sure radius is < than PreferredHeight of the creature,
      see TCastleTransform.PreferredHeight, otherwise creature may get stuck into ground.
      In short, if you use the default implementations,
      PreferredHeight is by default @italic(MiddleHeight (default 0.5) *
      bounding box height). Your radius must be smaller
      for all possible bounding box heights when the creature is not dead. }
    function Radius(const GravityUp: TVector3): Single;
  end;

  { Creature with smart walking and attacking intelligence.
    May stand still (idle), walk, attack, fire missiles, and die.

    Tracks the enemy (remembers last seen enemy 3D position,
    walks/flies to it, possibly through sectors/waypoints ---
    so it can pass through narrow doors in a labyrinth or walk over a narrow bridge).
    Attacks the enemy from the right distance (a short-range attack)
    and/or shoots a missile (adds a missile to the 3D world).
    Runs away from the enemy (when he's too close and/or our health is low).

    There are a lot of settings to achieve particular behavior,
    e.g. cowardly/brave, offensive/defensive, melee/ranged, etc. }
  TWalkAttackCreatureResource = class(TCreatureResource)
  private
    FIdleAnimation: T3DResourceAnimation;
    FIdleToWalkAnimation: T3DResourceAnimation;
    FWalkAnimation: T3DResourceAnimation;
    FAttackAnimation: T3DResourceAnimation;
    FFireMissileAnimation: T3DResourceAnimation;
    FDieAnimation: T3DResourceAnimation;
    FDieBackAnimation: T3DResourceAnimation;
    FHurtAnimation: T3DResourceAnimation;

    FMoveSpeed: Single;
    FMinLifeLossToHurt: Single;
    FChanceToHurt: Single;
    FMaxHeightAcceptableToFall: Single;
    FRandomWalkDistance: Single;
    FRemoveDead: boolean;
    FPreferredDistance: Single;
    FRunAwayLife: Single;
    FRunAwayDistance: Single;
    FVisibilityAngle: Single;
    FSmellDistance: Single;
    FAttackMinDelay: Single;
    FAttackMaxDistance: Single;
    FAttackMaxAngle: Single;
    FAttackTime: Single;
    FAttackSoundHit: TCastleSound;
    FAttackSoundStart: TCastleSound;
    FFireMissileTime: Single;
    FFireMissileMinDelay: Single;
    FFireMissileMaxDistance: Single;
    FFireMissileMaxAngle: Single;
    FFireMissileName: string;
    FFireMissileHeight: Single;
    FFireMissileSound: TCastleSound;
  protected
    function FlexibleUp: boolean; override;
  public
    const
      DefaultMoveSpeed = 10.0;
      DefaultMinLifeLossToHurt = 0.0;
      DefaultChanceToHurt = 1.0;
      DefaultMaxHeightAcceptableToFall = 1.5;
      DefaultRandomWalkDistance = 10.0;
      DefaultRemoveDead = false;
      DefaultPreferredDistance = 2.0;
      DefaultRunAwayLife = 0.3;
      DefaultRunAwayDistance = 10.0;
      DefaultVisibilityAngle = Pi * 120 / 180;
      DefaultSmellDistance = 0.0;

      DefaultAttackTime = 0.0;
      DefaultAttackMinDelay = 2.0;
      DefaultAttackMaxDistance = DefaultPreferredDistance;
      DefaultAttackMaxAngle = Pi / 6;

      DefaultFireMissileTime = 0.0;
      DefaultFireMissileMinDelay = DefaultAttackMinDelay;
      DefaultFireMissileMaxDistance = 30.0;
      DefaultFireMissileMaxAngle = DefaultAttackMaxAngle;
      DefaultFireMissileHeight = 0.5;

    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;
    function CreatureClass: TCreatureClass; override;

    { An animation of standing still (being idle).
      Will be played in a loop, so for best look make sure that
      the beginning and end match. }
    property IdleAnimation: T3DResourceAnimation read FIdleAnimation;

    { An animation when creature changes from standing still to walking.
      Optional.

      For best look: It's beginnig should glue with the end of IdleAnimation,
      it's ending should glue with beginning of WalkAnimation. }
    property IdleToWalkAnimation: T3DResourceAnimation read FIdleToWalkAnimation;

    { An animation of walking.
      Will be played in a loop, so for best look make sure that
      the beginning and end match. }
    property WalkAnimation: T3DResourceAnimation read FWalkAnimation;

    { An animation of short-range attacking. Optional.

      For best look: Beginning and end of it should roughly glue with (any)
      frame of WalkAnimation and IdleAnimation.

      @italic(Design notes:)
      I used to have here property like AttacksWhenWalking for the creature,
      to indicate whether creature changes state like
      "csWalk -> csAttack -> csWalk" or
      "csIdle -> csAttack -> csIdle". But this wasn't good.
      Intelligent creature sometimes attacks when walking (e.g. if it just
      made the distance to the enemy closer) or when standing
      (when the distance was already close enough). And after performing
      the attack, the creature doesn't need to go back to the original state
      before the attack. }
    property AttackAnimation: T3DResourceAnimation read FAttackAnimation;

    { Firing missile animation. Optional. Similar rules like AttackAnimation,
      but here the "highlight" is not directly hurting enemy,
      but firing a new creature (missile).

      You can always override TWalkAttackCreature.FireMissile to do pretty
      much anything you want, and this way treat this as an "alternate attack",
      not necessarily firing a missile.
      It's not really required to actually fire a missile --- it's only what
      happens at the default TWalkAttackCreature.FireMissile implementation,
      and it happens only if FireMissileName is not empty. }
    property FireMissileAnimation: T3DResourceAnimation read FFireMissileAnimation;

    { An animation of dying.

      Dying animation is not displayed in a loop, after it runs
      it's duration we constantly show the final frame,
      at the TCreature instance will keep existing on the level.
      Unless you set RemoveDead to @true, then the dead creature
      will be completely removed from the level.

      For best look: Beginning should roughly glue with any point of
      the idle/attack/walk animations. }
    property DieAnimation: T3DResourceAnimation read FDieAnimation;

    { An optional dying animation, used when the creature is killed
      by hitting it in the back. This may be useful if you want your
      creature to fall face-down when killed from the back or face-up
      when killed from the front. If this is defined, then DieAnimation
      is only used when creature is killed by hitting it from the front.
      The direction of last hit is taken from LastHurtDirection.

      For best look: Just like DieAnimation, beginning should roughly
      glue with any point of the idle/attack/walk animations. }
    property DieBackAnimation: T3DResourceAnimation read FDieBackAnimation;

    { Animation when the creature will be hurt.
      Beginning and end should *more-or-less* look like
      any point of the idle/attack/walk animations. }
    property HurtAnimation: T3DResourceAnimation read FHurtAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving in csWalk. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      {$ifdef FPC}default DefaultMoveSpeed{$endif};

    { The preferred distance between enemy and the creature.
      The creature will try to walk closer to the enemy if the distance is larger.
      (If you want to make the creature to also walk father from the enemy
      when necessary, then set RunAwayLife and RunAwayDistance.)

      This should be <= AttackMaxDistance or FireMissileMaxDistance,
      if you hope to actually perform a short-range or firing missile attack.
      The creature can attack enemy from AttackMaxDistance
      or fire missile from FireMissileMaxDistance,
      but it will walk closer to the enemy if possible --- until the distance
      is PreferredDistance. }
    property PreferredDistance: Single
      read FPreferredDistance write FPreferredDistance
      {$ifdef FPC}default DefaultPreferredDistance{$endif};

    { Minimum delay between one attack and the other, in seconds.
      Note that the duration of AttackAnimation also limits how often creature
      can do an attack (so e.g. setting this to 0.0 doesn't mean that creature
      can constantly attack, if AttackAnimation takes 1 second then at least
      this 1 second will have to pass between actual attack hits). }
    property AttackMinDelay: Single
      read FAttackMinDelay write FAttackMinDelay
      {$ifdef FPC}default DefaultAttackMinDelay{$endif};

    { Maximum distance between enemy and creature to allow creature
      to start attack. The distance is measured between
      enemy (see TWalkAttackCreature.Enemy) and current creature
      Middle (see @link(TCastleTransform.Middle)) points. }
    property AttackMaxDistance: Single
      read FAttackMaxDistance write FAttackMaxDistance
      {$ifdef FPC}default DefaultAttackMaxDistance{$endif};

    { The time point within AttackAnimation at which the short-range attack
      happens. When exactly happens depends on the virtual
      @link(TWalkAttackCreature.Attack) method implementation,
      in the base TWalkAttackCreature it is a short-range
      attack. }
    property AttackTime: Single read FAttackTime write FAttackTime
      {$ifdef FPC}default DefaultAttackTime{$endif};

    { Since most of the creatures will have their weapon
      on their front (teeth, shooting hands, claws, whatever),
      they can attack enemy only when they are facing the enemy.

      More precisely, the attack is allowed to start only when
      the angle between current creature @link(TCastleTransform.Direction Direction)
      and the vector from creature's Middle to the enemy's Middle (see TCastleTransform.Middle)
      is <= AttackMaxAngle.

      This is in radians. }
    property AttackMaxAngle: Single
      read FAttackMaxAngle write FAttackMaxAngle
      {$ifdef FPC}default DefaultAttackMaxAngle{$endif};

    { Sound played when short-range attack hits.
      None (nil) by default. }
    property AttackSoundHit: TCastleSound
      read FAttackSoundHit write FAttackSoundHit;

    { Played at the start of attack animation,
      that is when entering csAttack state.
      To play a sound when the actual hit happens (at AttackTime)
      see AttackSoundHit.
      None (nil) by default. }
    property AttackSoundStart: TCastleSound
      read FAttackSoundStart write FAttackSoundStart;

    { The time (in seconds) since the FireMissileAnimation start when we actually
      spawn a missile. By default zero, which means that we spawn the missile
      right when FireMissileAnimation starts.
      Must be < than the FireMissileAnimation duration, otherwise we will never
      reach tthis time and missile will never be fired. }
    property FireMissileTime: Single
      read FFireMissileTime write FFireMissileTime
      {$ifdef FPC}default DefaultFireMissileTime{$endif};

    { Minimum delay (in seconds) between firing of the missiles.
      The missile will not be fired if a previous missile was fired within last
      FireMissileMinDelay seconds.
      (Even if all other conditions for firing the missile are satisfied.) }
    property FireMissileMinDelay: Single
      read FFireMissileMinDelay write FFireMissileMinDelay
      {$ifdef FPC}default DefaultFireMissileMinDelay{$endif};

    { Maximum distance to the enemy to make firing missiles sensible.
      The creature will only fire the missile if enemy is within this distance.
      The creature will also try to shorten distance to the enemy,
      to get within this distance. }
    property FireMissileMaxDistance: Single
      read FFireMissileMaxDistance write FFireMissileMaxDistance
      {$ifdef FPC}default DefaultFireMissileMaxDistance{$endif};

    { Maximum angle (in radians) between current direction and
      the direction toward enemy to make firing missiles sensible.
      The creature will only fire the missile if enemy is within a cone of this angle. }
    property FireMissileMaxAngle: Single
      read FFireMissileMaxAngle write FFireMissileMaxAngle
      {$ifdef FPC}default DefaultFireMissileMaxAngle{$endif};

    { Name of the creature to fire as missile, at AttackTime during AttackAnimation.
      Leave empty to not fire any missile. }
    property FireMissileName: string
      read FFireMissileName write FFireMissileName;

    { Height (between Position and Middle, usually: legs and eyes)
      of the fired missile (see FireMissileName). }
    property FireMissileHeight: Single
      read FFireMissileHeight write FFireMissileHeight
      {$ifdef FPC}default DefaultFireMissileHeight{$endif};

    { Sound played when missile is fired, see FireMissileName.
      None (nil) by default. }
    property FireMissileSound: TCastleSound
      read FFireMissileSound write FFireMissileSound;

    { Portion of life and distance when the creature decides it's best to run away
      from the enemy. RunAwayLife is expressed as a fraction of MaxLife.
      We run if our @code(Life <= MaxLife * RunAwayLife) and the distance
      to the (last seen) enemy is < RunAwayDistance.
      Set RunAwayLife = 1 to make the creature always try to keep a safe distance
      from the enemy.
      @groupBegin }
    property RunAwayLife: Single
      read FRunAwayLife write FRunAwayLife {$ifdef FPC}default DefaultRunAwayLife{$endif};
    property RunAwayDistance: Single
      read FRunAwayDistance write FRunAwayDistance
      {$ifdef FPC}default DefaultRunAwayDistance{$endif};
    { @groupEnd }

    { Creature sees other things (like enemies) only within a cone
      of this angle. This way, the creature only looks forward, and you can
      sneak upon a creature from the back. Simply set this to >= 2 * Pi
      to remove this limit.

      Note that the creature also becomes aware of the enemy when it is
      hurt by a direct attack, regardless of VisibilityAngle. This way
      if you sneak and attack a creature from the back, it will turn around
      and fight you.

      Creature can also smell others, see SmellDistance. }
    property VisibilityAngle: Single read FVisibilityAngle write FVisibilityAngle
      {$ifdef FPC}default DefaultVisibilityAngle{$endif};

    { Creature smells other things (like enemies) within a sphere of this
      radius. This allows to detect enemy regardless of which direction
      the creature is facing, regardless of whether there is a line of sight
      to the enemy, regardless if enemy is moving.

      This is quite powerful ability to detect enemies,
      if you set this to something large (by default it's zero).
      Detecting enemies allows to more accurately/faster attack them
      and/or run away from them.

      Note: If you want the creature to nicely run from behind the corner,
      be sure to setup good sectors/waypoints in your level.}
    property SmellDistance: Single read FSmellDistance write FSmellDistance
      {$ifdef FPC}default DefaultSmellDistance{$endif};

    { When creature is wounded for more than MaxLife * MinLifeLossToHurt
      points and moreover Random < ChanceToHurt then creature will
      change to csHurt state and be knocked back.
      Changing to csHurt state means that any other state will be
      interrupted (e.g. enemy can interrupt
      creature's attack this way if AttackTime > 0).

      It's expected that "tougher" creatures will have MinLifeLossToHurt
      somewhat higher than DefaultMinLifeLossToHurt and ChanceToHurt
      significantly lower than DefaultChanceToHurt. }
    property MinLifeLossToHurt: Single
      read FMinLifeLossToHurt write FMinLifeLossToHurt
      {$ifdef FPC}default DefaultMinLifeLossToHurt{$endif};

    { See MinLifeLossToHurt. }
    property ChanceToHurt: Single
      read FChanceToHurt write FChanceToHurt
      {$ifdef FPC}default DefaultChanceToHurt{$endif};

    property MaxHeightAcceptableToFall: Single
      read FMaxHeightAcceptableToFall
      write FMaxHeightAcceptableToFall
      {$ifdef FPC}default DefaultMaxHeightAcceptableToFall{$endif};

    property RandomWalkDistance: Single
      read FRandomWalkDistance
      write FRandomWalkDistance
      {$ifdef FPC}default DefaultRandomWalkDistance{$endif};

    property RemoveDead: boolean
      read FRemoveDead write FRemoveDead default DefaultRemoveDead;
  end;

  { Creature that blindly moves in a given direction.
    It just moves into the given direction
    (with some possible twists, e.g. it can be a "homing"
    missile and/or be dragged down by gravity).
    On collision, it hits, potentially hurting the alive 3D object
    that was colliding (player or other creatures).

    Missiles ignore TCreatureResource.Flying, they use their own way to handle
    gravity with DirectionFallSpeed. }
  TMissileCreatureResource = class(TCreatureResource)
  private
    FFlyAnimation: T3DResourceAnimation;
    FDieAnimation: T3DResourceAnimation;
    FMoveSpeed: Single;
    FSoundHit: TCastleSound;
    FCloseDirectionToTargetSpeed: Single;
    FPauseBetweenSoundIdle: Single;
    FSoundIdle: TCastleSound;
    FHitsPlayer: boolean;
    FHitsCreatures: boolean;
    FDirectionFallSpeed: Single;
    FRemoveDead: boolean;
  protected
    function RadiusCalculate(const GravityUp: TVector3): Single; override;
  public
    const
      DefaultMoveSpeed = 10.0;
      DefaultCloseDirectionToTargetSpeed = 0.0;
      DefaultPauseBetweenSoundIdle = 2.0;
      DefaultHitsPlayer = true;
      DefaultHitsCreatures = false;
      DefaultDirectionFallSpeed = 0.0;
      DefaultRemoveDead = true;

    constructor Create(AOwner: TComponent); override;
    function CreatureClass: TCreatureClass; override;
    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;
    function CreateCreature(
      const ALevel: TAbstractLevel;
      const APosition, ADirection: TVector3;
      const MaxLife: Single): TCreature; override;

    property FlyAnimation: T3DResourceAnimation read FFlyAnimation;

    { Die (destroying) animation of a missile. Optional.
      It can depict missile explosion.
      After showing this animation (or immediately when missile hits something,
      if this animation is not specified) the missile is removed from the level
      (unless RemoveDead = @false). }
    property DieAnimation: T3DResourceAnimation read FDieAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      {$ifdef FPC}default DefaultMoveSpeed{$endif};

    { Sound when missile hits anything.
      None (nil) by default. }
    property SoundHit: TCastleSound
      read FSoundHit write FSoundHit;

    { For "homing" missiles, how fast direction to the target is corrected.
      Zero (default) means that the missile is not "homing". }
    property CloseDirectionToTargetSpeed: Single
      read FCloseDirectionToTargetSpeed
      write FCloseDirectionToTargetSpeed
      {$ifdef FPC}default DefaultCloseDirectionToTargetSpeed{$endif};

    { Sound played continuously when the missile is going.
      None (nil) by default.
      @seealso PauseBetweenSoundIdle }
    property SoundIdle: TCastleSound
      read FSoundIdle write FSoundIdle;

    { This should be synchonized with length of SoundIdle sound. }
    property PauseBetweenSoundIdle: Single
      read FPauseBetweenSoundIdle write FPauseBetweenSoundIdle
      {$ifdef FPC}default DefaultPauseBetweenSoundIdle{$endif};

    property HitsPlayer: boolean
      read FHitsPlayer write FHitsPlayer default DefaultHitsPlayer;
    property HitsCreatures: boolean
      read FHitsCreatures write FHitsCreatures default DefaultHitsCreatures;

    { How fast is the missile pulled down by gravity.
      Non-zero value causes missile direction to gradually point
      downward, this way missile flies downward eventually.

      This is quite different (in different units and with slightly different
      effect) than TCastleTransform.FallSpeed, hence a different name and property.
      TMissileCreatureResource doesn't use TCastleTransform.Gravity and so ignores
      T3DResource.FallSpeed, T3DResource.GrowSpeed and other properties.

      0 means to not fall down (missile is not affected by gravity). }
    property DirectionFallSpeed: Single
      read FDirectionFallSpeed write FDirectionFallSpeed
      {$ifdef FPC}default DefaultDirectionFallSpeed{$endif};

    { Should the dead (destroyed) missiles be removed from level.
      Useful if you want to see arrows stuck into walls where they hit.

      This is like TWalkAttackCreatureResource.RemoveDead
      and TStillCreatureResource.RemoveDead, except for missiles the default is @true.
      Also, even if you switch this to @false,
      it's ignored (works like @true) if the missile hit a dynamic
      object (like another creature or player).
      It only works when a missile hit something else than a creature/player,
      which means that it probably hit a static level wall. }
    property RemoveDead: boolean
      read FRemoveDead write FRemoveDead default DefaultRemoveDead;
  end;

  { Creature that just stays still.
    This is just a single 3D animation showing a creature. }
  TStillCreatureResource = class(TCreatureResource)
  private
    FIdleAnimation: T3DResourceAnimation;
    FDieAnimation: T3DResourceAnimation;
    FRemoveDead: boolean;
  public
    const
      DefaultRemoveDead = false;

    constructor Create(AOwner: TComponent); override;
    function CreatureClass: TCreatureClass; override;
    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;

    property IdleAnimation: T3DResourceAnimation read FIdleAnimation;
    property DieAnimation: T3DResourceAnimation read FDieAnimation;
    property RemoveDead: boolean
      read FRemoveDead write FRemoveDead default DefaultRemoveDead;
  end;

  { Base creature, using any TCreatureResource. }
  TCreature = class(TCastleAlive)
  private
    FResource: TCreatureResource;
    FResourceObserver: TFreeNotificationObserver;
    FResourceFrame: TResourceFrame;

    SoundSource: TCastleSoundSource;
    FSoundDieEnabled: boolean;

    FDebugCaptions: TCastleScene;
    FDebugCaptionsTransform: TMatrixTransformNode;
    FDebugCaptionsShape: TShapeNode;
    FDebugCaptionsText: TTextNode;
    FDebugCaptionsFontStyle: TFontStyleNode;

    FDebugTransform: TDebugTransform;

    { Calculated @link(Radius) suitable for this creature.
      This is cached result of @link(TCreatureResource.Radius). }
    FRadius: Single;

    procedure ResourceFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    var
      { Set by CreateCreature. }
      Level: TAbstractLevel;

    procedure SetLife(const Value: Single); override;
    procedure Fall(const FallHeight: Single); override;

    { LerpLegsMiddle interpolates between Position and Middle
      (intuitively, legs and eye positions). }
    function LerpLegsMiddle(const A: Single): TVector3;

    { Hurt given enemy. HurtEnemy may be @nil, in this case we do nothing. }
    procedure AttackHurt(const HurtEnemy: TCastleAlive);

    procedure UpdateDebugCaption(const Lines: TCastleStringList); virtual;

    { Sector where the middle of this 3D object is.
      Used for AI. @nil if none (maybe because we're not part of any world,
      maybe because sectors of the world were not initialized,
      or maybe simply because we're outside of all sectors). }
    function Sector(const OtherTransform: TCastleTransform): TSector; overload;
    function Sector: TSector; overload;
  public
    class var
      { Render debug bounding boxes and captions at every creature. }
      RenderDebug: boolean;

    constructor Create(AOwner: TComponent; const AMaxLife: Single); reintroduce; virtual;
    destructor Destroy; override;
    function GetCollides: boolean; override;

    property Resource: TCreatureResource read FResource;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { You can set this to @false to force the creature to die without
      making any sound. This is really seldom needed, usefull only to avoid
      a loud shriek noise when you kill many creatures at once.
      Primarily for use by debug menu "kill all creatures" and similar things. }
    property SoundDieEnabled: boolean read FSoundDieEnabled
      write FSoundDieEnabled default true;

    { Play SoundType where the creature's position is.

      Exactly, the position is between Position and Middle
      --- SoundHeight = 0 means Position, SoundHeight = 1 means Middle,
      SoundHeight between means ... well, between Position and Middle.
      This can also be higher than 1 or lower than 0, should be treated like
      lerp between Position and Middle.

      If TiedToCreature then the sounds position will be updated
      as the creature will move, and when the creature object will
      be destroyed, sound will stop. If not TiedToCreature, then
      the sound will simply be done at creature's position, but then
      it will continue to be played independent of this creature. }
    procedure Sound3d(const SoundType: TCastleSound; const SoundHeight: Single;
      const TiedToCreature: boolean = true);

    { Can the approximate sphere be used for some collision-detection
      tasks.

      Set to @false in descendants if Resource.Radius
      is not appropriate for this creature state.

      In this class, this is implemented to return @code(not Dead).
      This is usually sensible, since only alive creatures need bounding
      sphere advantages (stairs climbing), and using sphere with dead
      creatures would unnecessarily force the sphere radius to be small
      and Middle to be high. }
    function Sphere(out ARadius: Single): boolean; override;

    { Sphere radius for collision detection for alive creatures.
      Must be something <> 0 for collision detection to work.
      @seealso TCreatureResource.Radius }
    function Radius: Single;

    property CollidesWithMoving default true;
  end;

  TCreatureList = class({$ifdef FPC}specialize{$endif} TObjectList<TCreature>)
  end;

  { Creature using TWalkAttackCreatureResource. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TCreatureState;

    FStateChangeTime: Single;

    { Time of last State change to csAttack or csFireMissile,
      taken from LifeTime. }
    LastAttackTime, LastFireMissileTime: Single;
    { Whether Attack or FireMissile was already called within this
      csAttack or csFireMissile state. }
    AttackDone, FireMissileDone: boolean;

    HasAlternativeTarget: boolean;
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

    FDebugAlternativeTargetAxis: TDebugAxis;
    FDebugLastSensedEnemyAxis: TDebugAxis;
  protected
    { Last known information about enemy. }
    HasLastSensedEnemy: boolean;
    LastSensedEnemy: TVector3;
    LastSensedEnemySector: TSector;

    procedure SetState(Value: TCreatureState); virtual;
    procedure SetLife(const Value: Single); override;
    procedure UpdateDebugCaption(const Lines: TCastleStringList); override;

    { Enemy of this creature. In this class, this always returns global
      World.Player (if it exists and is still alive).
      Return @nil for no enemy. }
    function Enemy: TCastleAlive; virtual;

    { Last State change time, taken from LifeTime. }
    property StateChangeTime: Single read FStateChangeTime;

    { Actually do the attack indicated by AttackAnimation
      and AttackTime and other AttackXxx properties.
      This happens in the middle of AttackAnimation, at the time see AttackTime.

      This can happen only if you defined AttackAnimation for this creature.

      The default implementation here performs a short range attack,
      if enemy is still within reach (AttackMaxDistance; even if it was within
      reach at the start of csAttack state, the enemy could step back,
      so we need to check AttackMaxDistance again).
      The damage and knockback are defined by TCreatureResource.AttackDamageConst,
      TCreatureResource.AttackDamageRandom, TCreatureResource.AttackKnockbackDistance. }
    procedure Attack; virtual;

    { Actually do the attack indicated by FireMissileAnimation
      and FireMissileTime and other FireMissileXxx properties.
      This happens in the middle of FireMissileAnimation, at the time see
      FireMissileTime.

      This can happen only if you defined FireMissileAnimation for this creature.

      The default implementation here creates a new creature with resource
      defined by FireMissileName, if FireMissileName is not empty. }
    procedure FireMissile; virtual;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    destructor Destroy; override;

    function Resource: TWalkAttackCreatureResource;

    property State: TCreatureState read FState default csIdle;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    procedure Hurt(const LifeLoss: Single; const HurtDirection: TVector3;
      const AKnockbackDistance: Single; const Attacker: TCastleAlive); override;
  end;

  { Creature using TMissileCreatureResource. }
  TMissileCreature = class(TCreature)
  private
    LastSoundIdleTime: Single;
    ForceRemoveDead: boolean;
    procedure HitCore;
    procedure HitPlayer;
    procedure HitCreature(Creature: TCreature);
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;
    function Resource: TMissileCreatureResource;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Creature using TStillCreatureResource. }
  TStillCreature = class(TCreature)
  public
    function Resource: TStillCreatureResource;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TCreatureExistsEvent = function(const Creature: TCreature): boolean of object;

var
  DebugTimeStopForCreatures: boolean = false;

implementation

{$warnings off} // using deprecated CastleProgress, CastleGameNotifications in deprecated
uses SysUtils, DOM, Math,
  CastleFilesUtils, CastleGLUtils,
  CastleProgress, CastleGameNotifications, CastleUIControls;
{$warnings on}

{ TCreatureResource -------------------------------------------------------------- }

constructor TCreatureResource.Create(AOwner: TComponent);
begin
  inherited;
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FKnockBackDistance := DefaultKnockBackDistance;
  {$warnings off} // using deprecated in deprecated
  FKnockBackSpeed := TCastleAlive.DefaultKnockBackSpeed;
  {$warnings on}
  FSoundDieTiedToCreature := DefaultSoundDieTiedToCreature;
  FAttackDamageConst := DefaultAttackDamageConst;
  FAttackDamageRandom := DefaultAttackDamageRandom;
  FAttackKnockbackDistance := DefaultAttackKnockbackDistance;
  FMiddleHeight := TCastleTransform.DefaultMiddleHeight;
  FFallMinHeightToSound := DefaultFallMinHeightToSound;
  FFallMinHeightToDamage := DefaultFallMinHeightToDamage;
  FFallDamageScaleMin := DefaultFallDamageScaleMin;
  FFallDamageScaleMax := DefaultFallDamageScaleMax;
  {$warnings off} // using deprecated in deprecated
  FFallSound := SoundEngine.SoundFromName(DefaultFallSoundName, false);
  {$warnings on}
  ScaleMin := 1;
  ScaleMax := 1;
end;

procedure TCreatureResource.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  {$warnings off} // using deprecated in deprecated
  KnockBackSpeed := ResourceConfig.GetFloat('knockback_speed', TCastleAlive.DefaultKnockBackSpeed);
  {$warnings on}
  CollidesWhenDead := ResourceConfig.GetValue('collides_when_dead', false);
  ScaleMin := ResourceConfig.GetFloat('scale_min', 1);
  ScaleMax := ResourceConfig.GetFloat('scale_max', 1);
  KnockBackDistance := ResourceConfig.GetFloat('knockback_distance',
    DefaultKnockBackDistance);
  Flying := ResourceConfig.GetValue('flying',
    DefaultFlying);
  SoundDieTiedToCreature := ResourceConfig.GetValue('sound_die_tied_to_creature',
    DefaultSoundDieTiedToCreature);
  DefaultMaxLife := ResourceConfig.GetFloat('default_max_life',
    DefaultDefaultMaxLife);
  FRadiusOverride := ResourceConfig.GetFloat('radius', 0.0);
  AttackDamageConst := ResourceConfig.GetFloat('attack/damage/const',
    DefaultAttackDamageConst);
  AttackDamageRandom := ResourceConfig.GetFloat('attack/damage/random',
    DefaultAttackDamageRandom);
  AttackKnockbackDistance := ResourceConfig.GetFloat('attack/knockback_distance',
    DefaultAttackKnockbackDistance);
  MiddleHeight := ResourceConfig.GetFloat('middle_height', TCastleTransform.DefaultMiddleHeight);
  FallMinHeightToSound := ResourceConfig.GetFloat('fall/sound/min_height', DefaultFallMinHeightToSound);
  FallMinHeightToDamage := ResourceConfig.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
  FallDamageScaleMin := ResourceConfig.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
  FallDamageScaleMax := ResourceConfig.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);

  {$warnings off} // using deprecated SoundFromName in deprecated
  SoundSuddenPain := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_sudden_pain', ''));
  SoundDie := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_die', ''));
  FallSound := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('fall/sound/name', DefaultFallSoundName), false);
  {$warnings on}
end;

function TCreatureResource.FlexibleUp: boolean;
begin
  Result := true;
end;

function TCreatureResource.CreateCreature(
  const ALevel: TAbstractLevel;
  const APosition, ADirection: TVector3;
  const MaxLife: Single): TCreature;
var
  Scale: Single;
  RootTransform: TCastleRootTransform;
begin
  if ALevel.CreaturesRoot = nil then
    raise Exception.CreateFmt('Cannot add creature "%s" to level, as the level is not loaded yet. Execute TLevel.Load first', [
      Name
    ]);

  RootTransform := ALevel.RootTransform;

  { This is only needed if you did not add creature to <resources>.

    Note: we experimented with moving this to TCreature.PrepareResource,
    call Resource.Prepare from there. But it just doesn't fully work:
    some creatures really want Resource to be prepared
    before PrepareResource (from TCastleViewport.BeforeRender) had a chance to work.
    For example, on missiles like thrown web we do Sound3d that uses LerpLegsMiddle.
    Also TCreature.Idle (which definitely needs Resource) may get called before
    PrepareResource. IOW, PrepareResource is just too late. }
  Prepare(ALevel.PrepareParams);

  Result := CreatureClass.Create(ALevel.FreeAtUnload, MaxLife);
  { set properties that in practice must have other-than-default values
    to sensibly use the creature }
  Result.Level := ALevel;
  Result.FResource := Self;
  Result.FResourceObserver.Observed := Self;
  Result.Orientation := Orientation; // must be set before SetView
  Result.SetView(APosition, ADirection, RootTransform.GravityUp, FlexibleUp);
  Result.Life := MaxLife;
  Result.KnockBackSpeed := KnockBackSpeed;
  {$warnings off} // using deprecated in deprecated
  Result.Gravity := not Flying;
  Result.FallSpeed := FallSpeed;
  Result.GrowSpeed := GrowSpeed;
  {$warnings on}
  Result.CastShadows := CastShadowVolumes;
  Result.MiddleHeight := MiddleHeight;
  Scale := RandomFloatRange(ScaleMin, ScaleMax);
  Result.Scale := Vector3(Scale, Scale, Scale);

  ALevel.CreaturesRoot.Add(Result);
end;

function TCreatureResource.CreateCreature(
  const ALevel: TAbstractLevel;
  const APosition, ADirection: TVector3): TCreature;
begin
  Result := CreateCreature(ALevel, APosition, ADirection, DefaultMaxLife);
end;

procedure TCreatureResource.InstantiatePlaceholder(
  const ALevel: TAbstractLevel;
  const APosition, ADirection: TVector3;
  const NumberPresent: boolean; const Number: Int64);
var
  CreatureDirection: TVector3;
  MaxLife: Single;
begin
  { calculate CreatureDirection }
  CreatureDirection := ADirection;

  { calculate MaxLife }
  if NumberPresent then
    MaxLife := Number
  else
    MaxLife := DefaultMaxLife;

  CreateCreature(ALevel, APosition, CreatureDirection, MaxLife);
end;

function TCreatureResource.Radius(const GravityUp: TVector3): Single;
begin
  if RadiusOverride <> 0 then
    Result := RadiusOverride
  else
    Result := RadiusCalculate(GravityUp);
end;

function TCreatureResource.RadiusCalculate(const GravityUp: TVector3): Single;
var
  GC: Integer;
  Box: TBox3D;
  MaxRadiusForGravity: Single;
begin
  { calculate radius.
    Descendants can override this to provide better radius calculation,
    and user can always override this in resource.xml (in which case,
    RadiusCalculate is never called).

    So it's Ok to make here some assumptions that should suit usual cases,
    but not necessarily all possible cases ---
    e.g. our MaxRadiusForGravity calculation assumes you
    let default TCastleTransform.PreferredHeight algorithm to work. }

  if Animations.Count = 0 then
    Box := TBox3D.Empty
  else
    Box := Animations[0].BoundingBox;

  GC := MaxAbsVectorCoord(GravityUp);

  if Box.IsEmpty then
    Result := 0 else
  if Flying then
    { For Flying creatures, larger Radius (that *really* surrounds whole
      model from middle) is better. Also, MaxRadiusForGravity doesn't concern
      us then. }
    Result := Box.MaxSize / 2 else
  begin
    { Maximum radius value that allows gravity to work,
      assuming default TCastleTransform.PreferredHeight implementation,
      and assuming that Box is the smallest possible bounding box of our creature. }
    MaxRadiusForGravity := 0.9 * MiddleHeight * Box.Data[1].InternalData[GC];
    Result := Min(Box.Radius2D(GC), MaxRadiusForGravity);
  end;
end;

{ TWalkAttackCreatureResource ------------------------------------------------ }

constructor TWalkAttackCreatureResource.Create(AOwner: TComponent);
begin
  inherited;

  MoveSpeed := DefaultMoveSpeed;
  FMinLifeLossToHurt := DefaultMinLifeLossToHurt;
  FChanceToHurt := DefaultChanceToHurt;
  FMaxHeightAcceptableToFall := DefaultMaxHeightAcceptableToFall;
  FRandomWalkDistance := DefaultRandomWalkDistance;
  FRemoveDead := DefaultRemoveDead;
  FPreferredDistance := DefaultPreferredDistance;
  FRunAwayLife := DefaultRunAwayLife;
  FRunAwayDistance := DefaultRunAwayDistance;
  FVisibilityAngle := DefaultVisibilityAngle;
  FSmellDistance := DefaultSmellDistance;
  FAttackTime := DefaultAttackTime;
  FAttackMinDelay := DefaultAttackMinDelay;
  FAttackMaxDistance := DefaultAttackMaxDistance;
  FAttackMaxAngle := DefaultAttackMaxAngle;
  FFireMissileTime :=  DefaultFireMissileTime;
  FFireMissileMaxDistance := DefaultFireMissileMaxDistance;
  FFireMissileMaxAngle := DefaultFireMissileMaxAngle;
  FFireMissileMinDelay := DefaultFireMissileMinDelay;
  FFireMissileHeight := DefaultFireMissileHeight;

  FIdleAnimation := T3DResourceAnimation.Create(Self, 'idle');
  FIdleToWalkAnimation := T3DResourceAnimation.Create(Self, 'idle_to_walk', false);
  FWalkAnimation := T3DResourceAnimation.Create(Self, 'walk');
  FAttackAnimation := T3DResourceAnimation.Create(Self, 'attack', false);
  FFireMissileAnimation := T3DResourceAnimation.Create(Self, 'fire_missile', false);
  FDieAnimation := T3DResourceAnimation.Create(Self, 'die');
  FDieBackAnimation := T3DResourceAnimation.Create(Self, 'die_back', false);
  FHurtAnimation := T3DResourceAnimation.Create(Self, 'hurt');
end;

procedure TWalkAttackCreatureResource.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  MoveSpeed := ResourceConfig.GetFloat('move_speed',
    DefaultMoveSpeed);
  MinLifeLossToHurt := ResourceConfig.GetFloat('min_life_loss_to_hurt',
    DefaultMinLifeLossToHurt);
  ChanceToHurt := ResourceConfig.GetFloat('chance_to_hurt',
    DefaultChanceToHurt);
  MaxHeightAcceptableToFall := ResourceConfig.GetFloat('max_height_acceptable_to_fall',
    DefaultMaxHeightAcceptableToFall);
  RandomWalkDistance := ResourceConfig.GetFloat('random_walk_distance',
    DefaultRandomWalkDistance);
  RemoveDead := ResourceConfig.GetValue('remove_dead', DefaultRemoveDead);
  RunAwayLife := ResourceConfig.GetFloat('run_away/life', DefaultRunAwayLife);
  RunAwayDistance := ResourceConfig.GetFloat('run_away/distance', DefaultRunAwayDistance);
  VisibilityAngle := ResourceConfig.GetFloat('visibility/angle', DefaultVisibilityAngle);
  SmellDistance := ResourceConfig.GetFloat('smell_distance', DefaultSmellDistance);
  PreferredDistance := ResourceConfig.GetFloat('preferred_distance', DefaultPreferredDistance);

  AttackTime := ResourceConfig.GetFloat('attack/time', DefaultAttackTime);
  AttackMaxDistance := ResourceConfig.GetFloat('attack/max_distance', DefaultAttackMaxDistance);
  AttackMaxAngle := ResourceConfig.GetFloat('attack/max_angle', DefaultAttackMaxAngle);
  AttackMinDelay := ResourceConfig.GetFloat('attack/min_delay', DefaultAttackMinDelay);
  {$warnings off} // using deprecated SoundFromName in deprecated
  AttackSoundHit := SoundEngine.SoundFromName(ResourceConfig.GetValue('attack/sound_hit', ''));
  AttackSoundStart := SoundEngine.SoundFromName(ResourceConfig.GetValue('attack/sound_start', ''));
  {$warnings on}

  FireMissileTime :=  ResourceConfig.GetFloat('fire_missile/time', DefaultFireMissileTime);
  FireMissileMaxDistance := ResourceConfig.GetFloat('fire_missile/max_distance', DefaultFireMissileMaxDistance);
  FireMissileMaxAngle := ResourceConfig.GetFloat('fire_missile/max_angle', DefaultFireMissileMaxAngle);
  FireMissileMinDelay := ResourceConfig.GetFloat('fire_missile/min_delay', DefaultFireMissileMinDelay);
  {$warnings off} // using deprecated SoundFromName in deprecated
  FireMissileSound := SoundEngine.SoundFromName(ResourceConfig.GetValue('fire_missile/sound', ''));
  {$warnings on}
  FireMissileName := ResourceConfig.GetValue('fire_missile/name', '');
  FireMissileHeight := ResourceConfig.GetFloat('fire_missile/height', DefaultFireMissileHeight);
end;

function TWalkAttackCreatureResource.CreatureClass: TCreatureClass;
begin
  Result := TWalkAttackCreature;
end;

function TWalkAttackCreatureResource.FlexibleUp: boolean;
begin
  { For non-flying creatures, "up" vector must be always equal to GravityUp. }
  Result := Flying;
end;

{ TMissileCreatureResource ---------------------------------------------------- }

constructor TMissileCreatureResource.Create(AOwner: TComponent);
begin
  inherited;
  FMoveSpeed := DefaultMoveSpeed;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
  FDirectionFallSpeed := DefaultDirectionFallSpeed;
  FFlyAnimation := T3DResourceAnimation.Create(Self, 'fly');
  FDieAnimation := T3DResourceAnimation.Create(Self, 'die', false);
  FRemoveDead := DefaultRemoveDead;
end;

function TMissileCreatureResource.CreatureClass: TCreatureClass;
begin
  Result := TMissileCreature;
end;

procedure TMissileCreatureResource.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  MoveSpeed := ResourceConfig.GetFloat('move_speed',
    DefaultMoveSpeed);
  CloseDirectionToTargetSpeed := ResourceConfig.GetFloat('close_direction_to_target_speed',
    DefaultCloseDirectionToTargetSpeed);
  PauseBetweenSoundIdle := ResourceConfig.GetFloat('pause_between_sound_idle',
    DefaultPauseBetweenSoundIdle);
  HitsPlayer := ResourceConfig.GetValue('hits_player',
    DefaultHitsPlayer);
  HitsCreatures := ResourceConfig.GetValue('hits_creatures',
    DefaultHitsCreatures);
  DirectionFallSpeed := ResourceConfig.GetFloat('direction_fall_speed',
    DefaultDirectionFallSpeed);
  RemoveDead := ResourceConfig.GetValue('remove_dead', DefaultRemoveDead);

  {$warnings off} // using deprecated SoundFromName in deprecated
  SoundHit := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_hit', ''));
  SoundIdle := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_idle', ''));
  {$warnings on}
end;

function TMissileCreatureResource.CreateCreature(
  const ALevel: TAbstractLevel;
  const APosition, ADirection: TVector3;
  const MaxLife: Single): TCreature;
begin
  Result := inherited;

  { Normal gravity is turned off for missiles.

    We tried to enable it once, but:
    1. Normal gravity doesn't change the direction, so arrows fall down
       but they keep pointing upwards. This is noticeable to the player
       that looks at the arrow.
    2. It also conflicts with current hack with "MaximumFallingDistance -= 0.01"
       inside CastleTransform gravity. It could probably be fixed better,
       but since the 1st problem would remain anyway...
    Also growing up doesn't make any sense for missile that explodes on contact
    with ground. So Fall should be overriden to make HitCore,
    and GrowSpeed should be disabled below to be 0. (This is obviously doable.)

    We also want to turn off Gravity to use Gravity=false case when using
    MiddleHeight, so MiddleHeight is always between bounding box bottom and top
    for missiles. See TCastleTransform.MiddleHeight. }

  {$warnings off} // using deprecated in deprecated
  Result.Gravity := false;
  {$warnings on}
end;

function TMissileCreatureResource.RadiusCalculate(const GravityUp: TVector3): Single;
var
  Box: TBox3D;
begin
  Box := FlyAnimation.BoundingBox;

  { Use MinSize for missile, since smaller radius for missiles
    forces player to aim more precisely. Smaller radius may also allow some
    partial collisions to go undetected, but that's not a problem as the
    collisions imperfections are not noticeable for fast moving missiles. }
  if not Box.IsEmpty then
    Result := Box.MinSize / 2
  else
    Result := inherited;
end;

{ TStillCreatureResource ---------------------------------------------------- }

constructor TStillCreatureResource.Create(AOwner: TComponent);
begin
  inherited;
  FIdleAnimation := T3DResourceAnimation.Create(Self, 'idle');
  FDieAnimation := T3DResourceAnimation.Create(Self, 'die', false);
  FRemoveDead := DefaultRemoveDead;
end;

function TStillCreatureResource.CreatureClass: TCreatureClass;
begin
  Result := TStillCreature;
end;

procedure TStillCreatureResource.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;
  RemoveDead := ResourceConfig.GetValue('remove_dead', DefaultRemoveDead);
end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited Create(AOwner);
  CollidesWithMoving := true;
  MaxLife := AMaxLife;
  FSoundDieEnabled := true;

  FResourceObserver := TFreeNotificationObserver.Create(Self);
  FResourceObserver.OnFreeNotification := {$ifdef FPC}@{$endif}ResourceFreeNotification;

  SoundSource := TCastleSoundSource.Create(Self);
  AddBehavior(SoundSource);

  FDebugTransform := TDebugTransform.Create(Self);
  FDebugTransform.Parent := Self;

  FResourceFrame := TResourceFrame.Create(Self);
  Add(FResourceFrame);
end;

procedure TCreature.ResourceFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FResource := nil;
end;

function TCreature.GetCollides: boolean;
begin
  {$warnings off} // internally using deprecated, in a deprecated unit
  Result := (inherited GetCollides) and
    (Resource.CollidesWhenDead or (not Dead));
  {$warnings on}
end;

destructor TCreature.Destroy;
begin
  if Resource <> nil then
    Resource.Release;
  inherited;
end;

procedure TCreature.Sound3d(const SoundType: TCastleSound; const SoundHeight: Single;
  const TiedToCreature: boolean);
var
  PlayingSound: TCastlePlayingSoundSource;
begin
  if SoundType <> nil then
  begin
    PlayingSound := TCastlePlayingSoundSource.Create(nil);
    PlayingSound.Sound := SoundType;
    PlayingSound.FreeOnStop := true;
    PlayingSound.Follow := TiedToCreature;
    PlayingSound.SoundHeight := SoundHeight;
    SoundSource.Play(PlayingSound);
  end;
end;

function TCreature.LerpLegsMiddle(const A: Single): TVector3;
begin
  Result := Lerp(A, Translation, Middle);
end;

procedure TCreature.UpdateDebugCaption(const Lines: TCastleStringList);
begin
  Lines.Add(Format('%s [%f / %f]',
    [Resource.Name, Life, MaxLife]));
end;

procedure TCreature.Fall(const FallHeight: Single);
begin
  inherited;

  if FallHeight > Resource.FallMinHeightToSound then
    Sound3d(Resource.FallSound, 0.1, false);

  if FallHeight > Resource.FallMinHeightToDamage then
    Hurt(Max(0,
      FallHeight * MapRange(Random, 0.0, 1.0,
        Resource.FallDamageScaleMin,
        Resource.FallDamageScaleMax)),
      TVector3.Zero, 0, nil);
end;

procedure TCreature.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateDebugCaptions;
  var
    Root: TX3DRootNode;
    H: Single;
    BBox: TBox3D;
  begin
    if RenderDebug and (FDebugCaptions = nil) then
    begin
      { create FDebugCaptions on demand }

      FDebugCaptionsFontStyle := TFontStyleNode.Create;

      FDebugCaptionsText := TTextNode.Create;
      FDebugCaptionsText.FontStyle := FDebugCaptionsFontStyle;
      FDebugCaptionsText.Solid := false;

      FDebugCaptionsShape := TShapeNode.Create;
      FDebugCaptionsShape.Geometry := FDebugCaptionsText;

      FDebugCaptionsTransform := TMatrixTransformNode.Create;
      FDebugCaptionsTransform.AddChildren(FDebugCaptionsShape);

      Root := TX3DRootNode.Create;
      Root.AddChildren(FDebugCaptionsTransform);

      FDebugCaptions := TCastleScene.Create(Self);
      FDebugCaptions.Load(Root, true);
      FDebugCaptions.Collides := false;
      FDebugCaptions.Pickable := false;
      FDebugCaptions.CastShadows := false;
      FDebugCaptions.ExcludeFromStatistics := true;
      FDebugCaptions.InternalExcludeFromParentBoundingVolume := true;

      Add(FDebugCaptions);
    end;

    if FDebugCaptions <> nil then
      FDebugCaptions.Exists := RenderDebug;

    if RenderDebug then
    begin
      BBox := BoundingBox;
      FDebugCaptionsShape.Render := not BBox.IsEmpty;
      if FDebugCaptionsShape.Render then
      begin
        H := BBox.Data[1].InternalData[World.GravityCoordinate];
        FDebugCaptionsFontStyle.Size := H / 8;
        FDebugCaptionsTransform.Matrix := TransformToCoordsMatrix(
          { move the caption to be at the top }
          World.GravityUp * H,
          { By default, Text is in XY plane. Adjust it to our Orientation. }
          TVector3.CrossProduct(
            TCastleTransform.DefaultUp[Orientation],
            TCastleTransform.DefaultDirection[Orientation]),
          TCastleTransform.DefaultUp[Orientation],
          TCastleTransform.DefaultDirection[Orientation]);

        FDebugCaptionsText.FdString.Items.Clear;
        UpdateDebugCaption(FDebugCaptionsText.FdString.Items);
        FDebugCaptionsText.FdString.Changed;
      end;
    end;
  end;

begin
  inherited;
  if not Exists then Exit;

  { In this case (when Exists, regardless of DebugTimeStopForCreatures),
    TCastleAlive.Update changed LifeTime.
    And LifeTime is used to choose animation frame in GetChild.
    So the creature constantly changes, even when it's
    transformation (things taken into account in TCastleTransform) stay equal. }
  VisibleChangeHere([vcVisibleGeometry]);

  FDebugTransform.Exists := RenderDebug;
  UpdateDebugCaptions;
end;

procedure TCreature.SetLife(const Value: Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    { When dies, we don't play SoundSuddenPain sound. We will play SoundDie. }
    if SoundDieEnabled then
      Sound3d(Resource.SoundDie, 1.0, Resource.SoundDieTiedToCreature);
  end else
  if (Life > 0) and (Life - Value > 5) then
  begin
    Sound3d(Resource.SoundSuddenPain, 1.0);
  end;

  inherited;
end;

procedure TCreature.AttackHurt(const HurtEnemy: TCastleAlive);
begin
  if HurtEnemy <> nil then
    HurtEnemy.Hurt(Resource.AttackDamageConst +
      Random * Resource.AttackDamageRandom, Direction,
      Resource.AttackKnockbackDistance, Self);
end;

function TCreature.Sphere(out ARadius: Single): boolean;
begin
  Result := Exists and (not Dead);
  ARadius := Radius;
end;

function TCreature.Radius: Single;
begin
  if FRadius = 0 then
    FRadius := Resource.Radius(World.GravityUp);
  Result := FRadius;
end;

function TCreature.Sector(const OtherTransform: TCastleTransform): TSector;
begin
  if Level.GetSectors <> nil then
    Result := Level.GetSectors.SectorWithPoint(OtherTransform.Middle)
  else
    Result := nil;
end;

function TCreature.Sector: TSector;
begin
  Result := Sector(Self);
end;

{ TWalkAttackCreature -------------------------------------------------------- }

constructor TWalkAttackCreature.Create(AOwner: TComponent;
  const AMaxLife: Single);
begin
  inherited;

  if MaxLife > 0 then
  begin
    FState := csIdle;
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
  {$warnings off} // using deprecated in deprecated
  Result := Level.GetPlayer as TCastleAlive;
  {$warnings on}
  if (Result <> nil) and Result.Dead then
    Result := nil; { do not attack dead player }
end;

procedure TWalkAttackCreature.SetState(Value: TCreatureState);
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
       ( ((FState = csIdle) and (Value = csWalk)) or
         ((FState = csWalk) and (Value = csIdle)) ) then
    begin
      InternalMiddleForceBox := true;
      InternalMiddleForceBoxValue := LocalBoundingBox;
      InternalMiddleForceBoxTime := LifeTime + 0.1;
    end;

    { Some states require special finalization here. }
    case FState of
      csAttack:
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
      csAttack:
        begin
          Sound3d(Resource.AttackSoundStart, 1.0);
          LastAttackTime := StateChangeTime;
          AttackDone := false;
        end;
      csFireMissile:
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
      csIdle:
        FResourceFrame.SetFrame(Level, Resource.IdleAnimation, StateTime, true);
      csWalk:
        if Resource.IdleToWalkAnimation.Defined and
           (StateTime < Resource.IdleToWalkAnimation.Duration) then
          FResourceFrame.SetFrame(Level, Resource.IdleToWalkAnimation, StateTime, false)
        else
          FResourceFrame.SetFrame(Level, Resource.WalkAnimation,
            StateTime - Resource.IdleToWalkAnimation.Duration, true);
      csAttack:
        FResourceFrame.SetFrame(Level, Resource.AttackAnimation, StateTime, false);
      csFireMissile:
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
    {$warnings off} // using deprecated in deprecated
    if Gravity then
    {$warnings on}
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
      {$warnings off} // using deprecated in deprecated
      if Gravity then
      {$warnings on}
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
    {$warnings off} // using deprecated in deprecated
    if not Gravity then
      SqrDistanceToTarget := PointsDistanceSqr(Middle, Target)
    {$warnings on}
    else
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
      {$warnings off} // using deprecated in deprecated
      if (not Gravity) or (I <> World.GravityCoordinate) then
      {$warnings on}
        AlternativeTarget.InternalData[I] := AlternativeTarget.InternalData[I] + (Random * Distance * 2 - Distance);

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
        SetState(csFireMissile);
        Result := false;
      end else
      if AttackAllowed then
      begin
        SetState(csAttack);
        Result := false;
      end else
      if WantToRunAway or
         WantToWalkToEnemy(AngleBetweenDirectionToEnemy) then
      begin
        SetState(csWalk);
        Result := false;
      end else
      {$warnings off} // using deprecated in deprecated
      if Gravity and
      {$warnings on}
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
        { Continue csIdle state }
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
        {$warnings off} // using deprecated in deprecated
        if Gravity then
        {$warnings on}
        begin
          Height(NewMiddle, AboveHeight);
          {$warnings off} // using deprecated in deprecated
          if AboveHeight > Resource.MaxHeightAcceptableToFall + PreferredHeight then
          {$warnings on}
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
        SetState(csIdle);
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
        SetState(csIdle);
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
          to csIdle, and this allows creature to rotate in csIdle
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
        SetState(csIdle);
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
      SetState(csFireMissile) else
    if AttackAllowed then
      SetState(csAttack);
  end;

  { Go to the default state, like "idle".
    Doing this instead of SetState(csIdle) avoids switching to "idle" just for
    a single frame, which looks bad (animation visibly jumps for 1 frame,
    and also animation blending is broken by such 1-frame change,
    since our animation blending now can only transition from last to next animation). }
  procedure BackToDefaultState;
  begin
    if DoIdle then
      SetState(csIdle);
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
  if (not Exists) or DebugTimeStopForCreatures then Exit;

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
    csIdle: DoIdle;
    csWalk: DoWalk;
    csAttack: DoAttack;
    csFireMissile: DoFireMissile;
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
  {$warnings off} // using deprecated in deprecated
  if not Gravity then
  {$warnings on}
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
  {$warnings off} // using deprecated in deprecated
  E: TCastleAlive;
  {$warnings on}

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
    csIdle       : StateName := 'Idle';
    csWalk       : StateName := 'Walk';
    csAttack     : StateName := 'Attack';
    csFireMissile: StateName := 'FireMissile';
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

{ TMissileCreature ----------------------------------------------------------- }

constructor TMissileCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited;

  { We will check for collisions when missile moves. }
  Collides := false;
end;

function TMissileCreature.Resource: TMissileCreatureResource;
begin
  Result := TMissileCreatureResource(inherited Resource);
end;

procedure TMissileCreature.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateResourceFrame;
  begin
    if Dead and Resource.DieAnimation.Defined then
      FResourceFrame.SetFrame(Level, Resource.DieAnimation, LifeTime - DieTime, false)
    else
      FResourceFrame.SetFrame(Level, Resource.FlyAnimation, LifeTime, true);
  end;

var
  Player: TCastleTransform;

  function MissileMoveAllowed(const OldPos, NewPos: TVector3): boolean;
  var
    SavedPlayerExists: Boolean;
    SavedCreaturesRootExists: Boolean;
  begin
    {$warnings off} // using deprecated in deprecated
    if (not Resource.HitsPlayer) and (Player <> nil) then
    begin
      SavedPlayerExists := Player.Exists;
      Player.Exists := false;
    end;
    {$warnings on}
    if not Resource.HitsCreatures then
    begin
      SavedCreaturesRootExists := Level.CreaturesRoot.Exists;
      Level.CreaturesRoot.Exists := false;
    end;
    try
      Result := MoveAllowed(OldPos, NewPos, false);
    finally
      if not Resource.HitsCreatures then
        Level.CreaturesRoot.Exists := SavedCreaturesRootExists;
      {$warnings off} // using deprecated in deprecated
      if (not Resource.HitsPlayer) and (Player <> nil) then
        Player.Exists := SavedPlayerExists;
      {$warnings on}
    end;
  end;

var
  OldMiddle, NewMiddle: TVector3;
  AngleBetween, AngleChange: Single;
  NewDirection, TargetDirection: TVector3;
  I: Integer;
  C: TCreature;
begin
  inherited;
  if (not Exists) or DebugTimeStopForCreatures then Exit;

  if Dead then
  begin
    if (Resource.RemoveDead or ForceRemoveDead) and
       (LifeTime - DieTime > Resource.DieAnimation.Duration) then
      RemoveMe := rtRemoveAndFree;
    Exit;
  end;

  Player := Level.GetPlayer;

  { Missile moves *always*, regardless of MissileMoveAllowed result.
    Only after move, if the move made us colliding with something --- we explode. }
  OldMiddle := Middle;
  Translate(Direction * (Resource.MoveSpeed * SecondsPassed));
  NewMiddle := Middle;

  if not MissileMoveAllowed(OldMiddle, NewMiddle) then
  begin
    { Check collision missile <-> player.
      Maybe I'll switch to using bounding Sphere here one day?
      No reason for sphere or box, either way, for now. }
    if Resource.HitsPlayer and
      (Player <> nil) and
      Player.BoundingBox.Collision(BoundingBox) then
      HitPlayer;

    if Resource.HitsCreatures then
    begin
      { TODO: this is unclean. We would prefer to use World.WorldSphereCollision,
        wrapped inside MySphereCollision to prevent self-collisions.
        However, we need to know which TCreature was actually hit ---
        so SphereCollision would need to return a hierarchy of T3D objects,
        much like RayCollision.

        For now, just browse World directly. }

      { Check bounding Sphere of the missile <-> creature's BoundingBox.
        Bounding Sphere is better for arrow, that has very large geometry
        but small enough bounding Sphere (because bounding Sphere radius
        may be adjusted by resource.xml). }
      for I := 0 to World.Count - 1 do
        if World[I] is TCreature then
        begin
          C := TCreature(World[I]);
          if (C <> Self) and C.CheckCollides and
            C.BoundingBox.SphereSimpleCollision(Middle, Radius) then
          begin
            HitCreature(C);
            { TODO: projectiles shouldn't do here "break". }
            break;
          end;
        end;
    end;

    HitCore;
  end;

  if Resource.DirectionFallSpeed <> 0 then
  begin
    NewDirection := Direction -
      World.GravityUp * Resource.DirectionFallSpeed * SecondsPassed;
    Direction := NewDirection;
  end;

  if (Resource.CloseDirectionToTargetSpeed <> 0.0) and (Player <> nil) then
  begin
    TargetDirection := Player.Translation - Translation;
    AngleBetween := AngleRadBetweenVectors(TargetDirection, Direction);
    AngleChange := Resource.CloseDirectionToTargetSpeed * SecondsPassed;
    if AngleBetween <= AngleChange then
      Direction := TargetDirection else
    begin
      NewDirection := Direction;
      MakeVectorsAngleRadOnTheirPlane(NewDirection, TargetDirection,
        AngleBetween - AngleChange, NewDirection);
      Direction := NewDirection;
    end;
  end;

  if (LastSoundIdleTime = 0) or
     (LifeTime - LastSoundIdleTime > Resource.PauseBetweenSoundIdle) then
  begin
    LastSoundIdleTime := LifeTime;
    Sound3d(Resource.SoundIdle, 0.0);
  end;

  UpdateResourceFrame;
end;

procedure TMissileCreature.HitCore;
begin
  { TODO: for some missiles, their explosion may hurt everyone around.
    So do here additional checks for collision and hurt player and creatures. }

  Sound3d(Resource.SoundHit, 0, false);
  Hurt(1000 * 1000, TVector3.Zero, 0, Self);
end;

procedure TMissileCreature.HitPlayer;
begin
  ForceRemoveDead := true;
  {$warnings off} // using deprecated in deprecated
  AttackHurt(Level.GetPlayer as TCastleAlive);
  {$warnings on}
end;

procedure TMissileCreature.HitCreature(Creature: TCreature);
begin
  ForceRemoveDead := true;
  AttackHurt(Creature);
end;

{ TStillCreature ----------------------------------------------------------- }

function TStillCreature.Resource: TStillCreatureResource;
begin
  Result := TStillCreatureResource(inherited Resource);
end;

procedure TStillCreature.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateResourceFrame;
  begin
    if Dead and Resource.DieAnimation.Defined then
      FResourceFrame.SetFrame(Level, Resource.DieAnimation, LifeTime - DieTime, false)
    else
      FResourceFrame.SetFrame(Level, Resource.IdleAnimation, LifeTime, true);
  end;

begin
  inherited;
  if (not Exists) or DebugTimeStopForCreatures then Exit;

  if Dead then
  begin
    if Resource.RemoveDead and (LifeTime - DieTime > Resource.DieAnimation.Duration) then
      RemoveMe := rtRemoveAndFree;
  end;

  UpdateResourceFrame;
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  RegisterResourceClass(TWalkAttackCreatureResource, 'WalkAttack');
  RegisterResourceClass(TMissileCreatureResource, 'Missile');
  RegisterResourceClass(TStillCreatureResource, 'Still');
end.
