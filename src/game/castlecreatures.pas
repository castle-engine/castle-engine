{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Creatures. }
unit CastleCreatures;

interface

uses Classes, VectorMath, PrecalculatedAnimation, Boxes3D, CastleClassUtils,
  CastleUtils, CastleScene, SectorsWaypoints,
  CastleResources, CastleXMLConfig, Base3D,
  CastleSoundEngine, Frustum, X3DNodes, CastleColors, FGL;

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

  { Basic kind of creature that can walk or fly, has life and can be hurt. }
  TCreatureKind = class(T3DResource)
  strict private
    FFlying: boolean;
    FSoundSuddenPain: TSoundType;
    FSoundDie: TSoundType;
    FSoundDieTiedToCreature: boolean;
    FDefaultMaxLife: Single;
    FKnockedBackDistance: Single;
    FKnockBackSpeed: Single;

    RadiusConfigured: Single;

    FAttackDamageConst: Single;
    FAttackDamageRandom: Single;
    FAttackKnockbackDistance: Single;

    FFallMinHeightToSound: Single;
    FFallMinHeightToDamage: Single;
    FFallDamageScaleMin: Single;
    FFallDamageScaleMax: Single;
    FFallSound: TSoundType;

    FMiddleHeight: Single;

    { Calculated @link(Radius) suitable for this creature.
      This is set by our @link(Prepare) using RadiusCalculate method. }
    RadiusCalculated: Single;
  protected
    function RadiusCalculate(const GravityUp: TVector3Single): Single; virtual;
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
  public
    const
      { Default value for TCreatureKind.DefaultMaxLife.
        Yes, it's not a typo, this identifier starts with "DefaultDefault". }
      DefaultDefaultMaxLife = 100.0;
      DefaultFlying = false;
      DefaultKnockedBackDistance = 4.0;
      DefaultSoundDieTiedToCreature = true;
      DefaultAttackDamageConst = 0.0;
      DefaultAttackDamageRandom = 0.0;
      DefaultAttackKnockbackDistance = 0.0;

    constructor Create(const AName: string); override;

    { Flying creatures are not affected by gravity and
      (in case of TWalkAttackCreatureKind) their move direction is free.

      For all creatures, TCreature.Gravity (inherited from T3D.Gravity)
      is set to @code("not Flying") (except TMissileCreatureKind,
      that has special approach to gravity,
      see TMissileCreatureKind.DirectionFallSpeed).

      For TWalkAttackCreatureKind, additionally Flying allows to move
      freely, while non-flying creatures are constrained to move
      (and think about moving) only horizontally. }
    property Flying: boolean read FFlying write FFlying default DefaultFlying;

    { Sphere radius for collision detection for alive creatures.
      Must be something <> 0 for collision detection.

      You can define it in the creature resource.xml file,
      by setting radius="xxx" attribute on the root <resource> element.

      If it's not defined (or zero) in resource.xml file,
      then we use automatically calculated radius using RadiusCalculate,
      that is adjusted to the bounding box of animation.

      Note that this radius is not used at all when creature is dead,
      as dead creatures usually have wildly
      different boxes (tall humanoid creature probably has a flat bounding
      box when it's dead lying on the ground), so trying to use (the same)
      radius would only cause problems.
      Using sphere collision is also not necessary for dead creatures.
      See T3D.Sphere for more discussion about when the sphere is a useful
      bounding volume.

      The sphere center is the Middle point ("eye position") of the given creature.
      If the creature may be affected by gravity then
      make sure radius is < than PreferredHeight of the creature,
      see T3D.PreferredHeight, otherwise creature may get stuck into ground.
      In short, if you use the default implementations,
      PreferredHeight is by default @italic(MiddleHeight (default 0.5) *
      bounding box height). Your radius must be smaller
      for all possible bounding box heights when the creature is not dead. }
    function Radius: Single;

    property SoundSuddenPain: TSoundType
      read FSoundSuddenPain write FSoundSuddenPain default stNone;

    property SoundDie: TSoundType
      read FSoundDie write FSoundDie default stNone;

    { See TCreature.Sound3d TiedToCreature parameter docs.
      You can set this to false if you want SoundDie to last even
      after the creature object was destroyed. }
    property SoundDieTiedToCreature: boolean
      read FSoundDieTiedToCreature write FSoundDieTiedToCreature
      default DefaultSoundDieTiedToCreature;

    { The default MaxLife for creatures of this kind.

      Note that you can always override it for a particular creature
      instance. You can use a special creature placeholder with
      a specific starting life value
      (see TGameSceneManager.LoadLevel for placeholders docs,
      and see http://castle-engine.sourceforge.net/castle-development.php
      about the creature placeholders).
      Or you can use CreateCreature overloaded version that takes extra MaxLife
      parameter.

      So this is only a "suggested" default for MaxLife of this creature. }
    property DefaultMaxLife: Single
      read FDefaultMaxLife write FDefaultMaxLife default DefaultDefaultMaxLife;

    { Create the TCreature instance of this kind.
      Uses TCreature descendant that can best cooperate with this kind,
      e.g. if this kind has settings for short-range fight,
      then the TCreature instance will be able to short-range fight.

      The creature is added to the World, and it's owned by World.

      This is the only way to create TCreature instances.

      ADirection passed here is normalized, and then used
      as initial TCreature.Direction value.

      @groupBegin }
    function CreateCreature(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
      const MaxLife: Single): TCreature; virtual; overload;
    function CreateCreature(World: T3DWorld;
      const APosition, ADirection: TVector3Single): TCreature; overload;
    { @groupEnd }

    { Instantiate creature placeholder, by calling CreateCreature. }
    procedure InstantiatePlaceholder(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
      const NumberPresent: boolean; const Number: Int64); override;

    function CreatureClass: TCreatureClass; virtual; abstract;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;

    { Distance the creature is knocked back when hurt (should reflect
      the creature weight, how easy it is to push this creature).

      Will always be multiplied by the knocking distance of the weapon that
      caused the push (which should reflect the force of the weapon blow),
      see TItemWeaponKind.AttackKnockbackDistance.

      Only for TWalkAttackCreature, the final distance the creature
      is knocked back is capped
      by the time of the HurtAnimation (HurtAnimation.Duration).
      When the hurt animation ends, the knockback effect always ends,
      even if the distance (creature * weapon) indicates it should be knocked
      further. Otherwise knockback would work on standing creature,
      which could look bad. This may be changed some day. }
    property KnockedBackDistance: Single
      read FKnockedBackDistance write FKnockedBackDistance
      default DefaultKnockedBackDistance;

    { See T3DAlive.KnockBackSpeed. }
    property KnockBackSpeed: Single
      read FKnockBackSpeed write FKnockBackSpeed default DefaultKnockBackSpeed;

    { Default attack damage and knockback.
      Used only by the creatures that actually do some kind of direct attack.
      For example it is used for short-range attack by TWalkAttackCreatureKind
      (if TWalkAttackCreatureKind.AttackAnimation defined)
      and for hit of TMissileCreatureKind.

      All these values must be >= 0.

      AttackKnockbackDistance = 0 means no knockback.

      @groupBegin }
    property AttackDamageConst: Single
      read FAttackDamageConst write FAttackDamageConst default DefaultAttackDamageConst;
    property AttackDamageRandom: Single
      read FAttackDamageRandom write FAttackDamageRandom default DefaultAttackDamageRandom;
    property AttackKnockbackDistance: Single
      read FAttackKnockbackDistance write FAttackKnockbackDistance default DefaultAttackKnockbackDistance;
    { @groupEnd }

    { See T3DCustomTransform.MiddleHeight. }
    property MiddleHeight: Single
      read FMiddleHeight write FMiddleHeight default DefaultMiddleHeight;

    property FallMinHeightToSound: Single
      read FFallMinHeightToSound write FFallMinHeightToSound default DefaultCreatureFallMinHeightToSound;
    property FallMinHeightToDamage: Single
      read FFallMinHeightToDamage write FFallMinHeightToDamage default DefaultFallMinHeightToDamage;
    property FallDamageScaleMin: Single
      read FFallDamageScaleMin write FFallDamageScaleMin default DefaultFallDamageScaleMin;
    property FallDamageScaleMax: Single
      read FFallDamageScaleMax write FFallDamageScaleMax default DefaultFallDamageScaleMax;
    { Sound when falling.
      The default is the sound named 'creature_fall'. }
    property FallSound: TSoundType
      read FFallSound write FFallSound;
  end;

  { Creature with smart walking and attacking intelligence.
    May stand still (idle), walk, attack, fire missiles, and die.

    Tracks the player (remembers last seen player 3D position,
    walks/flies to it, possibly through sectors/waypoints ---
    so it can pass through narrow doors in a labyrinth or walk over a narrow bridge).
    Attacks the player from the right distance (a short-range attack)
    and/or shoots a missile (adds a missile to the 3D world).
    Runs away from the player (when he's too close and/or our health is low).

    There are a lot of settings to achieve particular behavior,
    e.g. cowardly/brave, offensive/defensive, melee/ranged, etc. }
  TWalkAttackCreatureKind = class(TCreatureKind)
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
    FRemoveCorpse: boolean;
    FPreferredDistance: Single;
    FRunAwayLife: Single;
    FRunAwayDistance: Single;
    FAttackMinDelay: Single;
    FAttackMaxDistance: Single;
    FAttackMaxAngle: Single;
    FAttackTime: Single;
    FAttackSoundHit: TSoundType;
    FAttackSoundStart: TSoundType;
    FFireMissileTime: Single;
    FFireMissileMinDelay: Single;
    FFireMissileMaxDistance: Single;
    FFireMissileMaxAngle: Single;
    FFireMissileName: string;
    FFireMissileHeight: Single;
    FFireMissileSound: TSoundType;
  public
    const
      DefaultMoveSpeed = 10.0;
      DefaultMinLifeLossToHurt = 0.0;
      DefaultChanceToHurt = 1.0;
      DefaultMaxHeightAcceptableToFall = 1.5;
      DefaultRandomWalkDistance = 10.0;
      DefaultRemoveCorpse = false;
      DefaultPreferredDistance = 2.0;
      DefaultRunAwayLife = 0.3;
      DefaultRunAwayDistance = 10.0;

      DefaultAttackTime = 0.0;
      DefaultAttackMinDelay = 2.0;
      DefaultAttackMaxDistance = DefaultPreferredDistance;
      DefaultAttackMaxAngle = Pi / 6;

      DefaultFireMissileTime = 0.0;
      DefaultFireMissileMinDelay = DefaultAttackMinDelay;
      DefaultFireMissileMaxDistance = 30.0;
      DefaultFireMissileMaxAngle = DefaultAttackMaxAngle;
      DefaultFireMissileHeight = 0.5;

    constructor Create(const AName: string); override;

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
      made the distance to the player closer) or when standing
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
      Unless you set RemoveCorpse to @true, then the dead creature
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
      default DefaultMoveSpeed;

    { The preferred distance between enemy and the creature.
      The creature will try to walk closer to the enemy if the distance is larger.
      (If you want to make the creature to also walk father from the enemy
      when necessary, then set RunAwayLife and RunAwayDistance.)

      This should be <= AttackMaxDistance or FireMissileMaxDistance,
      if you hope to actually perform a short-range or firing missile attack.
      The creature can attack player from AttackMaxDistance
      or fire missile from FireMissileMaxDistance,
      but it will walk closer to the enemy if possible --- until the distance
      is PreferredDistance. }
    property PreferredDistance: Single
      read FPreferredDistance write FPreferredDistance
      default DefaultPreferredDistance;

    { Minimum delay between one attack and the other, in seconds.
      Note that the duration of AttackAnimation also limits how often creature
      can do an attack (so e.g. setting this to 0.0 doesn't mean that creature
      can constantly attack, if AttackAnimation takes 1 second then at least
      this 1 second will have to pass between actual attack hits). }
    property AttackMinDelay: Single
      read FAttackMinDelay write FAttackMinDelay
      default DefaultAttackMinDelay;

    { Maximum distance between enemy and creature to allow creature
      to start attack. The distance is measured between
      Player.Position and creature's Middle. }
    property AttackMaxDistance: Single
      read FAttackMaxDistance write FAttackMaxDistance
      default DefaultAttackMaxDistance;

    { The time point within AttackAnimation at which the short-range attack
      happens. When exactly happens depends on the virtual
      @link(TWalkAttackCreature.Attack) method implementation,
      in the base TWalkAttackCreature it is a short-range
      attack. }
    property AttackTime: Single read FAttackTime write FAttackTime
      default DefaultAttackTime;

    { Because most of the creatures will have their weapon
      on their front (teeth, shooting hands, claws, whatever),
      so they can attack player only when their Direction is somewhat
      close to the direction to player.

      More precisely, the attack is allowed to start only when
      the angle between current Direction and the vector
      from creature's Middle to the player's Position
      is <= AttackMaxAngle.

      This is in radians. }
    property AttackMaxAngle: Single
      read FAttackMaxAngle write FAttackMaxAngle
      default DefaultAttackMaxAngle;

    { Sound played when short-range attack hits. }
    property AttackSoundHit: TSoundType
      read FAttackSoundHit write FAttackSoundHit default stNone;

    { Played at the start of attack animation,
      that is when entering csAttack state.
      To play a sound when the actual hit happens (at AttackTime)
      see AttackSoundHit. }
    property AttackSoundStart: TSoundType
      read FAttackSoundStart write FAttackSoundStart default stNone;

    property FireMissileTime: Single
      read FFireMissileTime write FFireMissileTime default DefaultFireMissileTime;
    property FireMissileMinDelay: Single
      read FFireMissileMinDelay write FFireMissileMinDelay default DefaultFireMissileMinDelay;
    property FireMissileMaxDistance: Single
      read FFireMissileMaxDistance write FFireMissileMaxDistance default DefaultFireMissileMaxDistance;
    property FireMissileMaxAngle: Single
      read FFireMissileMaxAngle write FFireMissileMaxAngle default DefaultFireMissileMaxAngle;

    { Name of the creature to fire as missile, at AttackTime during AttackAnimation.
      Leave empty to not fire any missile. }
    property FireMissileName: string
      read FFireMissileName write FFireMissileName;

    { Height (between Position and Middle, usually: legs and eyes)
      of the fired missile (see FireMissileName). }
    property FireMissileHeight: Single
      read FFireMissileHeight write FFireMissileHeight default DefaultFireMissileHeight;

    { Sound played when missile is fired, see FireMissileName. }
    property FireMissileSound: TSoundType
      read FFireMissileSound write FFireMissileSound default stNone;

    { Portion of life and distance when the creature decides it's best to run away
      from the enemy (player). RunAwayLife is expressed as a fraction of MaxLife.
      We run if our @code(Life <= MaxLife * RunAwayLife) and the distance
      to the (last seen) enemy is < RunAwayDistance.
      Set RunAwayLife = 1 to make the creature always try to keep a safe distance
      from the enemy.
      @groupBegin }
    property RunAwayLife: Single
      read FRunAwayLife write FRunAwayLife default DefaultRunAwayLife;
    property RunAwayDistance: Single
      read FRunAwayDistance write FRunAwayDistance default DefaultRunAwayDistance;
    { @groupEnd }

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;
    function CreatureClass: TCreatureClass; override;

    { When creature is wounded for more than MaxLife * MinLifeLossToHurt
      points and moreover Random < ChanceToHurt then creature will
      change to csHurt state and be knocked back.
      Changing to csHurt state means that any other state will be
      interrupted (e.g. player can interrupt
      creature's attack this way if AttackTime > 0).

      It's expected that "tougher" creatures will have MinLifeLossToHurt
      somewhat higher than DefaultMinLifeLossToHurt and ChanceToHurt
      significantly lower than DefaultChanceToHurt. }
    property MinLifeLossToHurt: Single
      read FMinLifeLossToHurt write FMinLifeLossToHurt
      default DefaultMinLifeLossToHurt;

    { See MinLifeLossToHurt. }
    property ChanceToHurt: Single
      read FChanceToHurt write FChanceToHurt
      default DefaultChanceToHurt;

    property MaxHeightAcceptableToFall: Single
      read FMaxHeightAcceptableToFall
      write FMaxHeightAcceptableToFall
      default DefaultMaxHeightAcceptableToFall;

    property RandomWalkDistance: Single
      read FRandomWalkDistance
      write FRandomWalkDistance
      default DefaultRandomWalkDistance;

    property RemoveCorpse: boolean
      read FRemoveCorpse write FRemoveCorpse default DefaultRemoveCorpse;
  end;

  { Creature that blindly moves in a given direction.
    It just moves into the given direction
    (with some possible twists, e.g. it can be a "homing"
    missile and/or be dragged down by gravity).
    On collision, it hits, potentially hurting the alive 3D object
    that was colliding (player or other creatures).

    Missiles ignore TCreatureKind.Flying, they use their own way to handle
    gravity with DirectionFallSpeed. }
  TMissileCreatureKind = class(TCreatureKind)
  private
    FFlyAnimation: T3DResourceAnimation;
    FMoveSpeed: Single;
    FSoundHit: TSoundType;
    FCloseDirectionToTargetSpeed: Single;
    FPauseBetweenSoundIdle: Single;
    FSoundIdle: TSoundType;
    FHitsPlayer: boolean;
    FHitsCreatures: boolean;
    FDirectionFallSpeed: Single;
  protected
    function RadiusCalculate(const GravityUp: TVector3Single): Single; override;
  public
    const
      DefaultMoveSpeed = 10.0;
      DefaultCloseDirectionToTargetSpeed = 0.0;
      DefaultPauseBetweenSoundIdle = 2.0;
      DefaultHitsPlayer = true;
      DefaultHitsCreatures = false;
      DefaultDirectionFallSpeed = 0.0;

    constructor Create(const AName: string); override;

    { The one and only animation of the missile. }
    property FlyAnimation: T3DResourceAnimation read FFlyAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMoveSpeed;

    property SoundHit: TSoundType
      read FSoundHit write FSoundHit default stNone;

    function CreatureClass: TCreatureClass; override;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;

    { For "homing" missiles, how fast direction to the target is corrected.
      Zero (default) means that the missile is not "homing". }
    property CloseDirectionToTargetSpeed: Single
      read FCloseDirectionToTargetSpeed
      write FCloseDirectionToTargetSpeed
      default DefaultCloseDirectionToTargetSpeed;

    { Sound just played when the missile is going. }
    property SoundIdle: TSoundType
      read FSoundIdle write FSoundIdle default stNone;

    { This should be synchonized with length of SoundIdle sound. }
    property PauseBetweenSoundIdle: Single
      read FPauseBetweenSoundIdle write FPauseBetweenSoundIdle
      default DefaultPauseBetweenSoundIdle;

    property HitsPlayer: boolean
      read FHitsPlayer write FHitsPlayer default DefaultHitsPlayer;
    property HitsCreatures: boolean
      read FHitsCreatures write FHitsCreatures default DefaultHitsCreatures;

    { Is the missile pulled down by gravity.
      This causes missile direction to gradually point
      downward, and this way missile flies downward eventually.

      This is quite different (in different units and with slightly different
      effect) than T3D.FallSpeed, hence a different name and property.
      TMissileCreatureKind doesn't use T3D.Gravity and so ignores
      FallSpeed, GrowSpeed and other properties.

      0 means to not fall down (missile is not affected by gravity). }
    property DirectionFallSpeed: Single
      read FDirectionFallSpeed write FDirectionFallSpeed
      default DefaultDirectionFallSpeed;

    function CreateCreature(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
      const MaxLife: Single): TCreature; override;
  end;

  { Creature that just stays still.
    This is just a single 3D animation showing a creature. }
  TStillCreatureKind = class(TCreatureKind)
  private
    FIdleAnimation: T3DResourceAnimation;
  public
    constructor Create(const AName: string); override;
    { The one and only animation of the still creature. }
    property IdleAnimation: T3DResourceAnimation read FIdleAnimation;
    function CreatureClass: TCreatureClass; override;
  end;

  { Base creature, using any TCreatureKind. }
  TCreature = class(T3DAlive)
  private
    FKind: TCreatureKind;
    FLifeTime: Single;

    UsedSounds: TSoundList;
    FSoundDieEnabled: boolean;

    procedure SoundRelease(Sender: TSound);
  protected
    property LifeTime: Single read FLifeTime;
    procedure SetLife(const Value: Single); override;
    function GetExists: boolean; override;
    procedure Fall(const FallHeight: Single); override;

    { Current scene to be rendered.
      Note that this may be called before we're added to World (at the end of our
      construction), so make it work always reliably. }
    { function GetChild: T3D; override; }

    { LerpLegsMiddle interpolates between Position and Middle
      (intuitively, legs and eye positions). }
    function LerpLegsMiddle(const A: Single): TVector3Single;

    procedure AttackHurt;

    function DebugCaption: string; virtual;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); virtual; reintroduce;

    destructor Destroy; override;

    property Kind: TCreatureKind read FKind;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

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
    procedure Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
      TiedToCreature: boolean = true);

    { Can the approximate sphere be used for some collision-detection
      tasks.

      Set to @false in descendants if Kind.Radius
      is not appropriate for this creature state.

      In this class, this is implemented to return @code(not Dead).
      This is usually sensible, since only alive creatures need bounding
      sphere advantages (stairs climbing), and using sphere with dead
      creatures would unnecessarily force the sphere radius to be small
      and Middle to be high. }
    function Sphere(out Radius: Single): boolean; override;

    property CollidesWithMoving default true;
  end;

  TCreatureList = class(specialize TFPGObjectList<TCreature>)
  end;

  { Creature using TWalkAttackCreatureKind. }
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
    AlternativeTarget: TVector3Single;
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
  protected
    { Set by Idle in this class, may be used by descendants
      in their Idle calls (to not calculate the same thing twice). }
    IdleSeesPlayer: boolean;
    IdleSqrDistanceToLastSeenPlayer: Single;

    HasLastSeenPlayer: boolean;
    LastSeenPlayer: TVector3Single;
    LastSeenPlayerSector: TSector;

    procedure SetState(Value: TCreatureState); virtual;

    procedure SetLife(const Value: Single); override;

    function DebugCaption: string; override;

    { Last State change time, taken from LifeTime. }
    property StateChangeTime: Single read FStateChangeTime;

    function GetChild: T3D; override;

    { Actually do the attack indicated by AttackAnimation
      and AttackTime and other AttackXxx properties.
      This happens in the middle of AttackAnimation, at the time see AttackTime.

      This can happen only if you defined AttackAnimation for this creature.

      The default implementation here performs a short range attack,
      if enemy is still within reach (AttackMaxDistance; even if it was within
      reach at the start of csAttack state, the enemy could step back,
      so we need to check AttackMaxDistance again).
      The damage and knockback are defined by TCreatureKind.AttackDamageConst,
      TCreatureKind.AttackDamageRandom, TCreatureKind.AttackKnockbackDistance. }
    procedure Attack; virtual;

    { Actually do the attack indicated by FireMissileAnimation
      and FireMissileTime and other FireMissileXxx properties.
      This happens in the middle of FireMissileAnimation, at the time see
      FireMissileTime.

      This can happen only if you defined FireMissileAnimation for this creature.

      The default implementation here creates a new creature with kind
      defined by FireMissileName, if FireMissileName is not empty. }
    procedure FireMissile; virtual;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    destructor Destroy; override;

    function Kind: TWalkAttackCreatureKind;

    property State: TCreatureState read FState default csIdle;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;

    procedure Hurt(const LifeLoss: Single; const HurtDirection: TVector3Single;
      const AKnockbackDistance: Single); override;
  end;

  { Creature using TMissileCreatureKind. }
  TMissileCreature = class(TCreature)
  private
    LastSoundIdleTime: Single;
    procedure HitCore;
    procedure HitPlayer;
    procedure HitCreature(Creature: TCreature);
  protected
    function GetChild: T3D; override;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    function Kind: TMissileCreatureKind;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Creature using TStillCreatureKind. }
  TStillCreature = class(TCreature)
  protected
    function GetChild: T3D; override;
  public
    function Kind: TStillCreatureKind;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

var
  DebugTimeStopForCreatures: boolean = false;

  { Global callback to control creatures existence. }
  OnCreatureExists: T3DExistsEvent;

implementation

uses SysUtils, DOM, GL, GLU, CastleFilesUtils, CastleGLUtils,
  ProgressUnit, CastleGameNotifications, OpenGLTTFonts, UIControls;

var
  DisableCreatures: Cardinal;

  { OpenGL outline (3D) font for DebugCaption. }
  Font3d: TGLOutlineFont;

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AName: string);
begin
  inherited;
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FKnockedBackDistance := DefaultKnockedBackDistance;
  FKnockBackSpeed := DefaultKnockBackSpeed;
  FSoundDieTiedToCreature := DefaultSoundDieTiedToCreature;
  FAttackDamageConst := DefaultAttackDamageConst;
  FAttackDamageRandom := DefaultAttackDamageRandom;
  FAttackKnockbackDistance := DefaultAttackKnockbackDistance;
  FMiddleHeight := DefaultMiddleHeight;
  FFallMinHeightToSound := DefaultCreatureFallMinHeightToSound;
  FFallMinHeightToDamage := DefaultFallMinHeightToDamage;
  FFallDamageScaleMin := DefaultFallDamageScaleMin;
  FFallDamageScaleMax := DefaultFallDamageScaleMax;
  FFallSound := SoundEngine.SoundFromName(DefaultCreatureFallSoundName, false);
end;

procedure TCreatureKind.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  KnockBackSpeed := ResourceConfig.GetFloat('knock_back_speed',
    DefaultKnockBackSpeed);
  KnockedBackDistance := ResourceConfig.GetFloat('knocked_back_distance',
    DefaultKnockedBackDistance);
  Flying := ResourceConfig.GetValue('flying',
    DefaultFlying);
  SoundDieTiedToCreature := ResourceConfig.GetValue('sound_die_tied_to_creature',
    DefaultSoundDieTiedToCreature);
  DefaultMaxLife := ResourceConfig.GetFloat('default_max_life',
    DefaultDefaultMaxLife);
  RadiusConfigured := ResourceConfig.GetFloat('radius', 0.0);
  AttackDamageConst := ResourceConfig.GetFloat('attack/damage/const',
    DefaultAttackDamageConst);
  AttackDamageRandom := ResourceConfig.GetFloat('attack/damage/random',
    DefaultAttackDamageRandom);
  AttackKnockbackDistance := ResourceConfig.GetFloat('attack/knockback_distance',
    DefaultAttackKnockbackDistance);
  MiddleHeight := ResourceConfig.GetFloat('middle_height', DefaultMiddleHeight);
  FallMinHeightToSound := ResourceConfig.GetFloat('fall/sound/min_height', DefaultCreatureFallMinHeightToSound);
  FallMinHeightToDamage := ResourceConfig.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
  FallDamageScaleMin := ResourceConfig.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
  FallDamageScaleMax := ResourceConfig.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);

  SoundSuddenPain := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_sudden_pain', ''));
  SoundDie := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_die', ''));
  FallSound := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('fall/sound/name', DefaultCreatureFallSoundName), false);
end;

function TCreatureKind.Radius: Single;
begin
  if RadiusConfigured <> 0 then
    Result := RadiusConfigured else
    Result := RadiusCalculated;
end;

function TCreatureKind.RadiusCalculate(const GravityUp: TVector3Single): Single;
var
  GC: Integer;
  Box: TBox3D;
  MaxRadiusForGravity: Single;
begin
  { calculate default RadiusCalculated.
    Descendants can override this to provide better radius calculation
    (or define radius in resource.xml file), so it's Ok to make here
    some assumptions that should suit usual cases, but not necessarily
    all possible cases --- e.g. our MaxRadiusForGravity calculation assumes you
    let default T3DCustomTransform.PreferredHeight algorithm to work. }

  if Animations.Count = 0 then
    Box := EmptyBox3D else
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
      assuming default T3D.PreferredHeight implementation,
      and assuming that Box is the smallest possible bounding box of our creature. }
    MaxRadiusForGravity := 0.9 * MiddleHeight * Box.Data[1, GC];
    Result := Min(Box.Radius2D(GC), MaxRadiusForGravity);
  end;
end;

function TCreatureKind.CreateCreature(World: T3DWorld;
  const APosition, ADirection: TVector3Single;
  const MaxLife: Single): TCreature;
begin
  { This is only needed if you forgot to add creature to <resources>.

    Note: we experimented with moving this to TCreature.PrepareResource,
    call Kind.Prepare from there. But it just doesn't fully work:
    some creatures really want kind to be prepared
    before PrepareResource (from scene manager BeforeDraw) had a chance to work.
    For example, on missiles like thrown web we do Sound3d that uses LerpLegsMiddle.
    Also TCreature.Idle (which definitely needs kind) may get called before
    PrepareResource. IOW, PrepareResource is just too late. }
  Prepare(World.BaseLights, World.GravityUp);

  Result := CreatureClass.Create(World { owner }, MaxLife);
  { set properties that in practice must have other-than-default values
    to sensibly use the creature }
  Result.FKind := Self;
  Result.SetView(APosition, ADirection, World.GravityUp);
  Result.Life := MaxLife;
  Result.KnockBackSpeed := KnockBackSpeed;
  Result.Gravity := not Flying;
  Result.FallSpeed := FallSpeed;
  Result.GrowSpeed := GrowSpeed;
  Result.MiddleHeight := MiddleHeight;

  World.Add(Result);
end;

function TCreatureKind.CreateCreature(World: T3DWorld;
  const APosition, ADirection: TVector3Single): TCreature;
begin
  Result := CreateCreature(World, APosition, ADirection, DefaultMaxLife);
end;

procedure TCreatureKind.InstantiatePlaceholder(World: T3DWorld;
  const APosition, ADirection: TVector3Single;
  const NumberPresent: boolean; const Number: Int64);
var
  CreatureDirection: TVector3Single;
  MaxLife: Single;
begin
  { calculate CreatureDirection }
  CreatureDirection := ADirection;

  { calculate MaxLife }
  if NumberPresent then
    MaxLife := Number else
    MaxLife := DefaultMaxLife;

  CreateCreature(World, APosition, CreatureDirection, MaxLife);
end;

procedure TCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;
  RadiusCalculated := RadiusCalculate(GravityUp);
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(const AName: string);
begin
  inherited;

  MoveSpeed := DefaultMoveSpeed;
  FMinLifeLossToHurt := DefaultMinLifeLossToHurt;
  FChanceToHurt := DefaultChanceToHurt;
  FMaxHeightAcceptableToFall := DefaultMaxHeightAcceptableToFall;
  FRandomWalkDistance := DefaultRandomWalkDistance;
  FRemoveCorpse := DefaultRemoveCorpse;
  FPreferredDistance := DefaultPreferredDistance;
  FRunAwayLife := DefaultRunAwayLife;
  FRunAwayDistance := DefaultRunAwayDistance;
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

procedure TWalkAttackCreatureKind.LoadFromFile(ResourceConfig: TCastleConfig);
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
  RemoveCorpse := ResourceConfig.GetValue('remove_corpse', DefaultRemoveCorpse);
  RunAwayLife := ResourceConfig.GetFloat('run_away/life', DefaultRunAwayLife);
  RunAwayDistance := ResourceConfig.GetFloat('run_away/distance', DefaultRunAwayDistance);

  PreferredDistance := ResourceConfig.GetFloat('preferred_distance', DefaultPreferredDistance);

  AttackTime := ResourceConfig.GetFloat('attack/time', DefaultAttackTime);
  AttackMaxDistance := ResourceConfig.GetFloat('attack/max_distance', DefaultAttackMaxDistance);
  AttackMaxAngle := ResourceConfig.GetFloat('attack/max_angle', DefaultAttackMaxAngle);
  AttackMinDelay := ResourceConfig.GetFloat('attack/min_delay', DefaultAttackMinDelay);
  AttackSoundHit := SoundEngine.SoundFromName(ResourceConfig.GetValue('attack/sound_hit', ''));
  AttackSoundStart := SoundEngine.SoundFromName(ResourceConfig.GetValue('attack/sound_start', ''));

  FireMissileTime :=  ResourceConfig.GetFloat('fire_missile/time', DefaultFireMissileTime);
  FireMissileMaxDistance := ResourceConfig.GetFloat('fire_missile/max_distance', DefaultFireMissileMaxDistance);
  FireMissileMaxAngle := ResourceConfig.GetFloat('fire_missile/max_angle', DefaultFireMissileMaxAngle);
  FireMissileMinDelay := ResourceConfig.GetFloat('fire_missile/min_delay', DefaultFireMissileMinDelay);
  FireMissileSound := SoundEngine.SoundFromName(ResourceConfig.GetValue('fire_missile/sound', ''));
  FireMissileName := ResourceConfig.GetValue('fire_missile/name', '');
  FireMissileHeight := ResourceConfig.GetFloat('fire_missile/height', DefaultFireMissileHeight);
end;

function TWalkAttackCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TWalkAttackCreature;
end;

{ TMissileCreatureKind ---------------------------------------------------- }

constructor TMissileCreatureKind.Create(const AName: string);
begin
  inherited;
  FMoveSpeed := DefaultMoveSpeed;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
  FDirectionFallSpeed := DefaultDirectionFallSpeed;
  FFlyAnimation := T3DResourceAnimation.Create(Self, 'fly');
end;

function TMissileCreatureKind.RadiusCalculate(const GravityUp: TVector3Single): Single;
var
  Box: TBox3D;
begin
  Box := FlyAnimation.BoundingBox;

  { Use MinSize for missile, since smaller radius for missiles
    forces player to aim more precisely. Smaller radius may also allow some
    partial collisions to go undetected, but that's not a problem as the
    collisions imperfections are not noticeable for fast moving missiles. }
  if not Box.IsEmpty then
    Result := Box.MinSize / 2 else
    Result := inherited;
end;

function TMissileCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TMissileCreature;
end;

procedure TMissileCreatureKind.LoadFromFile(ResourceConfig: TCastleConfig);
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

  SoundHit := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_hit', ''));
  SoundIdle := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('sound_idle', ''));
end;

function TMissileCreatureKind.CreateCreature(World: T3DWorld;
  const APosition, ADirection: TVector3Single;
  const MaxLife: Single): TCreature;
begin
  Result := inherited;

  { Normal gravity is turned off for missiles.

    We tried to enable it once, but:
    1. Normal gravity doesn't change the direction, so arrows fall down
       but they keep pointing upwards. This is noticeable to the player
       that looks at the arrow.
    2. It also conflicts with current hack with "MaximumFallingDistance -= 0.01"
       inside Base3D gravity. It could probably be fixed better,
       but since the 1st problem would remain anyway...
    Also growing up doesn't make any sense for missile that explodes on contact
    with ground. So Fall should be overriden to make HitCore,
    and GrowSpeed should be disabled below to be 0. (This is obviously doable.)

    We also want to turn off Gravity to use Gravity=false case when using
    MiddleHeight, so MiddleHeight is always between bounding box bottom and top
    for missiles. See T3DCustomTransform.MiddleHeight. }

  Result.Gravity := false;
end;

{ TStillCreatureKind ---------------------------------------------------- }

constructor TStillCreatureKind.Create(const AName: string);
begin
  inherited;
  FIdleAnimation := T3DResourceAnimation.Create(Self, 'idle');
end;

function TStillCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TStillCreature;
end;

{ TCreatureSoundData --------------------------------------------------- }

type
  TCreatureSoundData = class
  public
    SoundHeight: Single;
  end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited Create(AOwner);
  CollidesWithMoving := true;
  MaxLife := AMaxLife;
  FSoundDieEnabled := true;
  UsedSounds := TSoundList.Create(false);
end;

function TCreature.GetExists: boolean;
begin
  Result := (inherited GetExists) and (DisableCreatures = 0) and
    ((not Assigned(OnCreatureExists)) or OnCreatureExists(Self));
end;

destructor TCreature.Destroy;
var
  I: Integer;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      UsedSounds[I].UserData.Free;
      UsedSounds[I].UserData := nil;

      { Otherwise OnRelease would call TCreature.SoundRelease,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here. }
      UsedSounds[I].OnRelease := nil;
      UsedSounds[I].Release;
    end;
    FreeAndNil(UsedSounds);
  end;

  if Kind <> nil then
    Kind.Release;

  inherited;
end;

procedure TCreature.SoundRelease(Sender: TSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  UsedSounds.Remove(Sender);
end;

procedure TCreature.Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
  TiedToCreature: boolean);
var
  NewSource: TSound;
  SoundPosition: TVector3Single;
begin
  SoundPosition := LerpLegsMiddle(SoundHeight);
  NewSource := SoundEngine.Sound3d(SoundType, SoundPosition);
  if TiedToCreature and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnRelease := @SoundRelease;
    NewSource.UserData := TCreatureSoundData.Create;
  end;
end;

function TCreature.LerpLegsMiddle(const A: Single): TVector3Single;
begin
  Result := Lerp(A, Position, Middle);
end;

procedure TCreature.Render(const Frustum: TFrustum; const Params: TRenderParams);

  procedure DebugBoundingVolumes;
  var
    R: Single;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);

      glDrawBox3DWire(BoundingBox);

      if Sphere(R) then
      begin
        glPushMatrix;
          glTranslatev(Middle);
          CastleGluSphere(R, 10, 10, false, GLU_NONE, GLU_OUTSIDE, GLU_LINE);
        glPopMatrix;
      end;

      glColorv(Yellow3Single);
      glDrawAxisWire(Middle, BoundingBox.AverageSize(true, 0));
    glPopAttrib;
  end;

  procedure DebugCaptions;
  var
    H, FontSize: Single;
  begin
    glPushMatrix;
      H := GetChild.BoundingBox.Data[1, World.GravityCoordinate];
      glTranslatev(World.GravityUp * H);
      { TODO: probably these rotations need adjustment based on Orientation,
        they assume otUpZDirectionX now. }
      glRotatef(90, 0, 0, 1);
      glRotatef(90, 1, 0, 0);
      FontSize := H / 4;
      glScalef(FontSize / Font3d.RowHeight, FontSize / Font3d.RowHeight, 1);

      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
        glColorv(White3Single);
        Font3d.PrintAndMove(DebugCaption);
      glPopAttrib;
    glPopMatrix;
  end;

begin
  inherited;

  if GetExists and Frustum.Box3DCollisionPossibleSimple(BoundingBox) then
  begin
    glPushMatrix;
      glMultMatrix(Transform);
      if RenderDebugCaptions and
         (not Params.Transparent) and Params.ShadowVolumesReceivers then
        DebugCaptions;
    glPopMatrix;

    if RenderDebug3D and
       (not Params.Transparent) and Params.ShadowVolumesReceivers then
      DebugBoundingVolumes;
  end;
end;

function TCreature.DebugCaption: string;
begin
  Result := Format('%s [%s / %s]',
    [Kind.Name, FloatToNiceStr(Life), FloatToNiceStr(MaxLife)]);
end;

procedure TCreature.Fall(const FallHeight: Single);
begin
  inherited;

  if FallHeight > Kind.FallMinHeightToSound then
    Sound3d(Kind.FallSound, 0.1, false);

  if FallHeight > Kind.FallMinHeightToDamage then
    Hurt(Max(0,
      FallHeight * MapRange(Random, 0.0, 1.0,
        Kind.FallDamageScaleMin,
        Kind.FallDamageScaleMax)),
      ZeroVector3Single, 0);
end;

procedure TCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

  procedure UpdateUsedSounds;
  var
    I: Integer;
    SoundPosition: TVector3Single;
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      SoundPosition := LerpLegsMiddle(
        TCreatureSoundData(UsedSounds[I].UserData).SoundHeight);
      UsedSounds[I].Position := SoundPosition;
    end;
  end;

begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  FLifeTime += CompSpeed;

  UpdateUsedSounds;
end;

procedure TCreature.SetLife(const Value: Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    { When dies, we don't play SoundSuddenPain sound. We will play SoundDie. }
    if SoundDieEnabled then
      Sound3d(Kind.SoundDie, 1.0, Kind.SoundDieTiedToCreature);
  end else
  if (Life > 0) and (Life - Value > 5) then
  begin
    Sound3d(Kind.SoundSuddenPain, 1.0);
  end;

  inherited;
end;

procedure TCreature.AttackHurt;
var
  Player: T3DAlive;
begin
  Player := World.Player as T3DAlive;
  if Player = nil then Exit; { no Player to hurt }

  Player.Hurt(Kind.AttackDamageConst +
    Random * Kind.AttackDamageRandom, Direction,
    Kind.AttackKnockbackDistance);
end;

function TCreature.Sphere(out Radius: Single): boolean;
begin
  Result := GetExists and (not Dead);
  Radius := Kind.Radius;
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
      any kind of Die (or wounded) sound or animation. }
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

function TWalkAttackCreature.Kind: TWalkAttackCreatureKind;
begin
  Result := TWalkAttackCreatureKind(inherited Kind);
end;

procedure TWalkAttackCreature.SetState(Value: TCreatureState);
begin
  if FState <> Value then
  begin
    FState := Value;
    FStateChangeTime := LifeTime;
    { Some states require special initialization here. }
    case FState of
      csAttack:
        begin
          Sound3d(Kind.AttackSoundStart, 1.0);
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

procedure TWalkAttackCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

  function ActionAllowed(const Animation: T3DResourceAnimation;
    const LastTime, MinDelay, MaxDistance, MaxAngle: Single): boolean;
  var
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    Result := IdleSeesPlayer and
      Animation.Defined and
      (LifeTime - LastTime > MinDelay) and
      (IdleSqrDistanceToLastSeenPlayer <= Sqr(MaxDistance));

    if Result then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        LastSeenPlayer - Middle, Direction);
      Result := AngleRadBetweenTheDirectionToPlayer <= MaxAngle;
    end;
  end;

  function AttackAllowed: boolean;
  begin
    Result := ActionAllowed(Kind.AttackAnimation, LastAttackTime,
      Kind.AttackMinDelay, Kind.AttackMaxDistance, Kind.AttackMaxAngle);
  end;

  function FireMissileAllowed: boolean;
  begin
    Result := ActionAllowed(Kind.FireMissileAnimation, LastFireMissileTime,
      Kind.FireMissileMinDelay, Kind.FireMissileMaxDistance, Kind.FireMissileMaxAngle);
  end;

  procedure CalculateDirectionToTarget(
    const Target: TVector3Single;
    out DirectionToTarget: TVector3Single;
    out AngleRadBetweenDirectionToTarget: Single);
  begin
    { calculate DirectionToTarget }
    DirectionToTarget := VectorSubtract(Target, Middle);
    if not Kind.Flying then
      MakeVectorsOrthoOnTheirPlane(DirectionToTarget, World.GravityUp);

    { calculate AngleRadBetweenDirectionToTarget }
    AngleRadBetweenDirectionToTarget :=
      AngleRadBetweenVectors(DirectionToTarget, Direction);
  end;

  { Call this only when HasLastSeenPlayer }
  procedure CalculateDirectionToPlayer(out DirectionToPlayer: TVector3Single;
    out AngleRadBetweenDirectionToPlayer: Single);
  begin
    CalculateDirectionToTarget(LastSeenPlayer,
      DirectionToPlayer, AngleRadBetweenDirectionToPlayer);
  end;

  procedure CalculateDirectionFromPlayer(
    var DirectionFromPlayer: TVector3Single;
    var AngleRadBetweenDirectionFromPlayer: Single);
  begin
    CalculateDirectionToPlayer(
      DirectionFromPlayer, AngleRadBetweenDirectionFromPlayer);
    VectorNegateTo1st(DirectionFromPlayer);
    AngleRadBetweenDirectionFromPlayer :=
      Pi - AngleRadBetweenDirectionFromPlayer;
  end;

  { This changes Direction to be closer to DirectionToTarget.
    Note that it requires the value of AngleRadBetweenDirectionToTarget
    effectively }
  procedure RotateDirectionToFaceTarget(const DirectionToTarget: TVector3Single;
    const AngleRadBetweenDirectionToTarget: Single);
  const
    AngleRadChangeSpeed = 5.0;
  var
    AngleRadChange: Single;
    NewDirection: TVector3Single;
  begin
    if not VectorsParallel(DirectionToTarget, Direction) then
    begin
      { Rotate Direction, to be closer to DirectionToTarget }

      { calculate AngleRadChange }
      AngleRadChange := AngleRadChangeSpeed * CompSpeed;
      MinTo1st(AngleRadChange, AngleRadBetweenDirectionToTarget);

      NewDirection := RotatePointAroundAxisRad(AngleRadChange, Direction,
        VectorProduct(Direction, DirectionToTarget));

      { Make sure direction for non-flying creatures is orthogonal to GravityUp. }
      if not Kind.Flying then
        MakeVectorsOrthoOnTheirPlane(NewDirection, World.GravityUp);
      Direction := NewDirection;
    end;
  end;

  function CloseEnoughToTarget(const Target: TVector3Single): boolean;
  const
    MinDistanceToTarget = 0.1;
  var
    SqrDistanceToTarget: Single;
  begin
    if Kind.Flying then
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
    const AngleRadBetweenDesiredDirection: Single): boolean;
  const
    MaxAngleToMoveForward = Pi / 3 { 60 degrees };
  begin
    Result :=
      { If AngleRadBetweenDesiredDirection is too large, there is not much point
        in moving in given direction anyway. We should just change our Direction. }
      (AngleRadBetweenDesiredDirection <= MaxAngleToMoveForward);
  end;

  { Assuming that I want to get to Target position, is it sensible
    to do this by moving along current Direction ?
    This checks whether current Direction points roughly in the
    direction of the Target, and if were not already as close as possible
    to Target. }
  function WantToWalkToTarget(
    const Target: TVector3Single;
    const AngleRadBetweenDirectionToTarget: Single): boolean;
  begin
    Result :=
      WantToWalkInDesiredDirection(AngleRadBetweenDirectionToTarget) and
      { See comments in CloseEnoughToTarget for reasoning why this is needed. }
      (not CloseEnoughToTarget(Target));
  end;

  { This doesn't take into account current Direction,
    it only looks at current Position and LastSeenPlayer position,
    and asks "do I want to get closer" ?
    Use only if HasLastSeenPlayer. }
  function WantToShortenDistanceToPlayer: boolean;
  begin
    Result :=
      { Is it wanted to get closer to the LastSeenPlayer ?

        Yes --- only if it will help make AttackAllowed from false to true.
        See AttackAllowed implementation.

        If IdleSeesPlayer and IdleSqrDistanceToLastSeenPlayer is small enough,
        there's no point in getting closer to the player. In fact, it would
        be bad to get closer to player in this case, as this would allow
        player to easier attack (shorter distance --- easier to reach with
        short-range weapon, or easier to aim with long-range weapon). }
      ( (not IdleSeesPlayer) or
        (IdleSqrDistanceToLastSeenPlayer > Sqr(Kind.PreferredDistance))
      );
  end;

  { Is it wanted to get closer to the LastSeenPlayer ?
    And (if it's wanted) is it sensible to do this by moving
    along current Direction ?
    Call this only if HasLastSeenPlayer. }
  function WantToWalkToPlayer(
    const AngleRadBetweenDirectionToPlayer: Single): boolean;
  begin
    Result := WantToShortenDistanceToPlayer and
      WantToWalkToTarget(LastSeenPlayer, AngleRadBetweenDirectionToPlayer);
  end;

  function WantToRunAway: boolean;
  begin
    Result := IdleSeesPlayer and
      (Life <= MaxLife * Kind.RunAwayLife) and
      (IdleSqrDistanceToLastSeenPlayer < Sqr(Kind.RunAwayDistance));
  end;

  procedure InitAlternativeTarget;
  var
    Distance: Single;
    I: Integer;
  begin
    Distance := Kind.RandomWalkDistance;

    AlternativeTarget := Middle;
    { Add random values to the AlternativeTarget, but only on the components
      where creature can reliably move. Creature that cannot fly cannot
      move in gravity (UpIndex) direction. }
    for I := 0 to 2 do
      if Kind.Flying or (I <> World.GravityCoordinate) then
        AlternativeTarget[I] += Random * Distance * 2 - Distance;

    HasAlternativeTarget := true;

    AlternativeTargetTime := LifeTime;
  end;

  procedure DoIdle;
  var
    DirectionToPlayer: TVector3Single;
    AngleRadBetweenDirectionToPlayer: Single;
  begin
    if HasLastSeenPlayer then
    begin
      CalculateDirectionToPlayer(DirectionToPlayer, AngleRadBetweenDirectionToPlayer);

      if FireMissileAllowed then
        SetState(csFireMissile) else
      if AttackAllowed then
        SetState(csAttack) else
      if WantToRunAway or
         WantToWalkToPlayer(AngleRadBetweenDirectionToPlayer) then
        SetState(csWalk) else
      if (not Kind.Flying) and
         (AngleRadBetweenDirectionToPlayer < 0.01) and
         BoundingBox.PointInside2D(LastSeenPlayer, World.GravityCoordinate) then
      begin
        { Then the player (or it's LastSeenPlayer) is right above or below us.
          Since we can't fly, we can't get there. Standing in place
          is one possibility, but it's not really good
          - We become easier target to shoot for player with the bow.
          - Most importantly, this way player can stand on our head and
            slash us with a sword without any risk. (This was almost
            a standard technique of killing Werewolf or SpiderQueen bosses).
          So we move a little --- just for the sake of moving. }
        SetState(csWalk);
        InitAlternativeTarget;
      end else
      begin
        { Continue csIdle state }
        RotateDirectionToFaceTarget(DirectionToPlayer,
          AngleRadBetweenDirectionToPlayer);
      end;
    end;
  end;

  procedure DoWalk;

    { This performs the real move, which means that it changes Position
      and Middle along the Direction vector.

      This doesn't check whether this is a sensible move, so use this
      only if you know that the creature really wants to go in this Direction.

      This checks only the basic (i.e. always wanted) things:
      - Collision detection (with level, player and other creatures)
      - For not Flying creatures, also the check to not fall down from high
        is done. }
    function MoveAlongTheDirection: boolean;

      { Don't be stupid, and don't walk where you see you will fall down. }
      function TooHighAboveTheGround(const NewMiddle: TVector3Single):
        boolean;
      var
        AboveHeight: Single;
      begin
        Result := false;
        if not Kind.Flying then
        begin
          Height(NewMiddle, AboveHeight);
          if AboveHeight > Kind.MaxHeightAcceptableToFall + PreferredHeight then
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
        (not TooHighAboveTheGround(Middle + Direction * (Kind.MoveSpeed * 0.2))) and

        { Use Move without wall-sliding here.
          Things using MoveAlongTheDirection depend on the fact that
          MoveAlongTheDirection will return false
          if no further way is possible (and wall-sliding would try instead
          to return true and correct target position).

          Our trick with "AlternativeTarget" should handle
          eventual problems with the track of creature, so wall-sliding
          should not be needed. }
        Move(Direction * (Kind.MoveSpeed * CompSpeed), false, false);
    end;

    { Go the way to LastSeenPlayer, *not* by using waypoints.
      Assumes HasLastSeenPlayer. }
    procedure WalkNormal;
    var
      DirectionToTarget: TVector3Single;
      AngleRadBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToPlayer(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToWalkToPlayer(AngleRadBetweenDirectionToTarget) then
      begin
        if not MoveAlongTheDirection then
        begin
          { Not able to get to player this way ? Maybe there exists
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
        AngleRadBetweenDirectionToTarget);
    end;

    procedure WalkToWaypoint(const Target: TVector3Single);
    var
      DirectionToTarget: TVector3Single;
      AngleRadBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToTarget(Target, DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToShortenDistanceToPlayer then
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
        AngleRadBetweenDirectionToTarget);
    end;

  const
    ProbabilityToTryAnotherAlternativeTarget = 0.5;
    AngleRadBetweenDirectionToTargetToResign = Pi / 180 { 1 degree };
    MaxTimeForAlternativeTarget = 5.0;
  var
    DirectionToTarget: TVector3Single;
    AngleRadBetweenDirectionToTarget: Single;
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
        DirectionToTarget, AngleRadBetweenDirectionToTarget);

      if WantToWalkToTarget(AlternativeTarget,
        AngleRadBetweenDirectionToTarget) then
      begin
        { Note that MoveAlongTheDirection returns false when
          moving along the current Direction is not good.
          But maybe moving along the DirectionToTarget is possible ?
          So we shouldn't just resign from current AlternativeTarget
          so fast --- maybe it's good, but we have to adjust
          our Direction a little more. That's why I use
          AngleRadBetweenDirectionToTargetToResign.

          Note that for normal moving (i.e. toward LastSeenPlayer,
          not AlternativeTarget) we in this case just change state
          to csIdle, and this allows creature to rotate in csIdle
          state. }
        if (not MoveAlongTheDirection) and
           (AngleRadBetweenDirectionToTarget <=
             AngleRadBetweenDirectionToTargetToResign) then
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
        AngleRadBetweenDirectionToTarget);
    end else
    if WantToRunAway then
    begin
      CalculateDirectionFromPlayer(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToWalkInDesiredDirection(AngleRadBetweenDirectionToTarget) then
      begin
        if (not MoveAlongTheDirection) and
           (AngleRadBetweenDirectionToTarget <=
             AngleRadBetweenDirectionToTargetToResign) then
        begin
          { Maybe there exists some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);
    end else
    begin
      if not HasLastSeenPlayer then
      begin
        { Nowhere to go; so just stay here. }
        SetState(csIdle);
        Exit;
      end;

      UseWalkNormal := true;

      SectorNow := Sector;
      if (SectorNow <> LastSeenPlayerSector) and
         (SectorNow <> nil) and
         (LastSeenPlayerSector <> nil) then
      begin
        { The way to LastSeenPlayer is using waypoints. }

        { Recalculate WaypointsSaved.
          Note that I recalculate only when SectorNow or
          LastSeenPlayerSector changed. }
        if (SectorNow <> WaypointsSaved_Begin) or
           (LastSeenPlayerSector <> WaypointsSaved_End) then
        begin
          WaypointsSaved_Begin := SectorNow;
          WaypointsSaved_End := LastSeenPlayerSector;
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

  procedure DoAttack;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;
    if (not AttackDone) and (StateTime >= Kind.AttackTime) then
    begin
      AttackDone := true;
      Attack;
    end;
    if StateTime > Kind.AttackAnimation.Duration then
      { csIdle will quickly change to csWalk if it will want to walk. }
      SetState(csIdle);
  end;

  procedure DoFireMissile;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;
    if (not FireMissileDone) and (StateTime >= Kind.FireMissileTime) then
    begin
      FireMissileDone := true;
      FireMissile;
    end;
    if StateTime > Kind.FireMissileAnimation.Duration then
      { csIdle will quickly change to csWalk if it will want to walk. }
      SetState(csIdle);
  end;

  procedure DoHurt;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;

    if StateTime > Kind.HurtAnimation.Duration then
    begin
      CancelKnockback;
      SetState(csIdle);
    end;
  end;

  { @true if last attack was from the back of the creature,
    @false if from the front or unknown (when LastHurtDirection is zero). }
  function WasLackAttackBack: boolean;
  begin
    try
      Result := AngleRadBetweenVectors(LastHurtDirection, Direction) < Pi/2;
    except
      on EVectorMathInvalidOp do Result := false;
    end;
  end;

  procedure DoDie(const AnimationDuration: Single);
  begin
    if Kind.RemoveCorpse and
      (LifeTime - StateChangeTime > AnimationDuration) then
      RemoveMe := rtRemoveAndFree;
  end;

var
  Player: T3DOrient;
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if Dead and not (State in [csDie, csDieBack]) then
  begin
    if Kind.DieBackAnimation.Defined and WasLackAttackBack then
      SetState(csDieBack) else
      SetState(csDie);
    Exit;
  end;

  Player := World.Player;
  IdleSeesPlayer := (Player <> nil) and LineOfSight(Middle, Player.Position);
  if IdleSeesPlayer then
  begin
    HasLastSeenPlayer := true;
    LastSeenPlayer := Player.Position;
    LastSeenPlayerSector := Player.Sector;
  end;

  if HasLastSeenPlayer then
  begin
    IdleSqrDistanceToLastSeenPlayer :=
      PointsDistanceSqr(LastSeenPlayer, Middle);
  end;

  case FState of
    csIdle: DoIdle;
    csWalk: DoWalk;
    csAttack: DoAttack;
    csFireMissile: DoFireMissile;
    csDie    : DoDie(Kind.DieAnimation.Duration);
    csDieBack: DoDie(Kind.DieBackAnimation.Duration);
    csHurt: DoHurt;
  end;

  { Flying creatures may change their direction vector freely.
    However, we want them to keep their sense of up --- they should try
    to keep straight, so their up vector should try to remain close
    to GravityUp, not just change wildly.

    For non-flying, this is not needed, as then Up should always remain equal
    to initial value, which is GravityUp. }
  if Kind.Flying then
    UpPrefer(World.GravityUp);
end;

function TWalkAttackCreature.GetChild: T3D;
var
  StateTime: Single;
begin
  if not Kind.Prepared then Exit(nil);

  { Time from the change to this state. }
  StateTime := LifeTime - StateChangeTime;

  case FState of
    csIdle:
      Result := Kind.IdleAnimation.Scene(StateTime, true);
    csWalk:
      if Kind.IdleToWalkAnimation.Defined and
         (StateTime < Kind.IdleToWalkAnimation.Duration) then
        Result := Kind.IdleToWalkAnimation.Scene(StateTime, false) else
        Result := Kind.WalkAnimation.Scene(
          StateTime - Kind.IdleToWalkAnimation.Duration, true);
    csAttack:
      Result := Kind.AttackAnimation.Scene(StateTime, false);
    csFireMissile:
      Result := Kind.FireMissileAnimation.Scene(StateTime, false);
    csDie:
      Result := Kind.DieAnimation.Scene(StateTime, false);
    csDieBack:
      Result := Kind.DieBackAnimation.Scene(StateTime, false);
    csHurt:
      Result := Kind.HurtAnimation.Scene(StateTime, false);
    else raise EInternalError.Create('FState ?');
  end;
end;

procedure TWalkAttackCreature.SetLife(const Value: Single);
begin
  if (not Dead) and
    (Life - Value > Kind.MinLifeLossToHurt * MaxLife) and
    ( (Kind.ChanceToHurt = 1.0) or
      (Random < Kind.ChanceToHurt) ) then
    SetState(csHurt);
  inherited;
end;

procedure TWalkAttackCreature.Attack;

  function ShortRangeAttackHits: boolean;
  var
    B, PB: TBox3D;
    DistanceLength, DistanceIncrease: Single;
  begin
    if World.Player = nil then Exit(false); { no Plaeyr to hurt }
    B := BoundingBox;
    PB := World.Player.BoundingBox;

    { We would like to check collision between PB and our B translated
      by our Direction now, i.e.
        Boxes3DCollision(Box3DTranslate(B, VectorScale(Direction, ???)), PB)
      But how much should be scale Direction, i.e. what to put for "???" ?
      It must be large enough to compensate even large Kind.AttackMaxDistance,
      it must be small enough so that player should not be able to avoid
      our attacks just by standing very close to the creature.

      So we have to check a couple of bounding boxes.
      If we move our boxes by Box3DMinSize(B), we're sure that
      each box will stick to the previous and next. But maybe
      there will be some areas around the sticking points ?
      So B.MinSize / 2 seems safe. }
    DistanceIncrease := B.MinSize / 2;

    DistanceLength := DistanceIncrease;
    while DistanceLength < Kind.AttackMaxDistance do
    begin
      if B.Translate(VectorScale(Direction, DistanceLength)).Collision(PB) then
        Exit(true);
      DistanceLength += DistanceIncrease;
    end;

    { Check one last time for Kind.AttackMaxDistance }
    Result := B.Translate(
      VectorScale(Direction, Kind.AttackMaxDistance)).Collision(PB);
  end;

begin
  if ShortRangeAttackHits then
  begin
    Sound3d(Kind.AttackSoundHit, 1.0);
    AttackHurt;
  end;
end;

procedure TWalkAttackCreature.FireMissile;
var
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3Single;
begin
  if (Kind.FireMissileName <> '') and HasLastSeenPlayer then
  begin
    MissilePosition := LerpLegsMiddle(Kind.FireMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := (Resources.FindName(Kind.FireMissileName) as TCreatureKind).
      CreateCreature(World, MissilePosition, MissileDirection);
    Missile.Sound3d(Kind.FireMissileSound, 0.0);
  end;
end;

function TWalkAttackCreature.DebugCaption: string;
var
  StateName: string;
begin
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
  Result := (inherited DebugCaption) + ' ' + StateName;
end;

procedure TWalkAttackCreature.Hurt(const LifeLoss: Single;
  const HurtDirection: TVector3Single;
  const AKnockbackDistance: Single);
begin
  inherited Hurt(LifeLoss, HurtDirection,
    AKnockbackDistance * Kind.KnockedBackDistance);
end;

procedure TWalkAttackCreature.Render(const Frustum: TFrustum; const Params: TRenderParams);
var
  AxisSize: Single;
begin
  inherited;

  if RenderDebug3D and GetExists and
     (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    AxisSize := BoundingBox.AverageSize(true, 0);
    if HasAlternativeTarget then
    begin
      glColorv(Blue3Single);
      glDrawAxisWire(AlternativeTarget, AxisSize);
    end;

    if HasLastSeenPlayer then
    begin
      glColorv(Red3Single);
      glDrawAxisWire(LastSeenPlayer, AxisSize);
    end;
  end;
end;

{ TMissileCreature ----------------------------------------------------------- }

constructor TMissileCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited;

  { We will check for collisions when missile moves. }
  Collides := false;
end;

function TMissileCreature.Kind: TMissileCreatureKind;
begin
  Result := TMissileCreatureKind(inherited Kind);
end;

procedure TMissileCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  Player: T3DOrient;

  function MissileMoveAllowed(const OldPos, NewPos: TVector3Single): boolean;
  begin
    if (not Kind.HitsPlayer) and (Player <> nil) then Player.Disable;
    if not Kind.HitsCreatures then Inc(DisableCreatures);
    try
      Result := MoveAllowed(OldPos, NewPos, false);
    finally
      if not Kind.HitsCreatures then Dec(DisableCreatures);
      if (not Kind.HitsPlayer) and (Player <> nil) then Player.Enable;
    end;
  end;

var
  OldMiddle, NewMiddle: TVector3Single;
  AngleBetween, AngleChange: Single;
  NewDirection, TargetDirection: TVector3Single;
  I: Integer;
  C: TCreature;
begin
  inherited;
  Player := World.Player;

  { TODO: do some missile explosion animation for some missiles. }
  if Life <= 0.0 then
    RemoveMe := rtRemoveAndFree;

  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  { Missile moves *always*, regardless of MissileMoveAllowed result.
    Only after move, if the move made us colliding with something --- we explode. }
  OldMiddle := Middle;
  Translate(Direction * (Kind.MoveSpeed * CompSpeed));
  NewMiddle := Middle;

  if not MissileMoveAllowed(OldMiddle, NewMiddle) then
  begin
    { Check collision missile <-> player.
      Maybe I'll switch to using bounding Sphere here one day?
      No reason for sphere or box, either way, for now. }
    if Kind.HitsPlayer and
      (Player <> nil) and
      Player.BoundingBox.Collision(BoundingBox) then
      HitPlayer;

    if Kind.HitsCreatures then
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
        is adjusted by creatures/kinds.xml). }
      for I := 0 to World.Count - 1 do
        if World[I] is TCreature then
        begin
          C := TCreature(World[I]);
          if (C <> Self) and C.GetCollides and
            C.BoundingBox.SphereSimpleCollision(Middle, Kind.Radius) then
          begin
            HitCreature(C);
            { TODO: projectiles shouldn't do here "break". }
            break;
          end;
        end;
    end;

    HitCore;
  end;

  if Kind.DirectionFallSpeed <> 0 then
  begin
    NewDirection := Direction -
      World.GravityUp * Kind.DirectionFallSpeed * CompSpeed;
    Direction := NewDirection;
  end;

  if (Kind.CloseDirectionToTargetSpeed <> 0.0) and (Player <> nil) then
  begin
    TargetDirection := Player.Position - Position;
    AngleBetween := AngleRadBetweenVectors(TargetDirection, Direction);
    AngleChange := Kind.CloseDirectionToTargetSpeed * CompSpeed;
    if AngleBetween <= AngleChange then
      Direction := TargetDirection else
    begin
      NewDirection := Direction;
      MakeVectorsAngleRadOnTheirPlane(NewDirection, TargetDirection,
        AngleBetween - AngleChange);
      Direction := NewDirection;
    end;
  end;

  if (LastSoundIdleTime = 0) or
     (LifeTime - LastSoundIdleTime > Kind.PauseBetweenSoundIdle) then
  begin
    LastSoundIdleTime := LifeTime;
    Sound3d(Kind.SoundIdle, 0.0);
  end;
end;

function TMissileCreature.GetChild: T3D;
begin
  if not Kind.Prepared then Exit(nil);

  Result := Kind.FlyAnimation.Scene(LifeTime, true);
end;

procedure TMissileCreature.HitCore;
begin
  { TODO: for some missiles, their explosion may hurt everyone around.
    So do here additional checks for collision and hurt player and creatures. }

  Sound3d(Kind.SoundHit, 0, false);

  Hurt(1000 * 1000, ZeroVector3Single, 0);
end;

procedure TMissileCreature.HitPlayer;
begin
  AttackHurt;
end;

procedure TMissileCreature.HitCreature(Creature: TCreature);
begin
  Creature.Hurt(Kind.AttackDamageConst +
    Random * Kind.AttackDamageRandom, Direction,
    Kind.AttackKnockbackDistance);
end;

{ TStillCreature ----------------------------------------------------------- }

function TStillCreature.Kind: TStillCreatureKind;
begin
  Result := TStillCreatureKind(inherited Kind);
end;

function TStillCreature.GetChild: T3D;
begin
  if not Kind.Prepared then Exit(nil);

  Result := Kind.IdleAnimation.Scene(LifeTime, true);
end;

procedure TStillCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { TODO: do explosion anim for barrel.
    We should probably have here DestroyAnimation, like DieAnimation,
    and property RemoveCorpse like TWalkAttackCreatureKind. }
  if Life <= 0.0 then
    RemoveMe := rtRemoveAndFree;
end;

{ initialization / finalization ---------------------------------------------- }

const
  Font3dFamily = ffSans;
  Font3dBold = false;
  Font3dItalic = false;

procedure WindowOpen(const Container: IUIContainer);
begin
  Font3d := GLContextCache.Fonts_IncReference(
    Font3dFamily, Font3dBold, Font3dItalic,
    TFontStyleNode.ClassTTF_Font(Font3dFamily, Font3dBold, Font3dItalic));
end;

procedure WindowClose(const Container: IUIContainer);
begin
  if Font3d <> nil then
  begin
    GLContextCache.Fonts_DecReference(Font3dFamily, Font3dBold, Font3dItalic);
    Font3d := nil;
  end;
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);

  RegisterResourceClass(TWalkAttackCreatureKind, 'WalkAttack');
  RegisterResourceClass(TMissileCreatureKind, 'Missile');
  RegisterResourceClass(TStillCreatureKind, 'Still');
end.
