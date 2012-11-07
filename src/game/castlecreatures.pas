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
  TCreature = class;

  TCreatureClass = class of TCreature;

  { Basic kind of creature that can walk or fly, has life and can be hurt. }
  TCreatureKind = class(T3DResource)
  private
    FFlying: boolean;
    FSoundSuddenPain: TSoundType;
    FSoundDying: TSoundType;
    FSoundDyingTiedToCreature: boolean;
    FDefaultMaxLife: Single;
    FKnockedBackDistance: Single;
    FKnockBackSpeed: Single;

    RadiusFromFile: Single;

    FAttackDamageConst: Single;
    FAttackDamageRandom: Single;
    FAttackKnockbackDistance: Single;

    FFallMinHeightToSound: Single;
    FFallMinHeightToDamage: Single;
    FFallDamageScaleMin: Single;
    FFallDamageScaleMax: Single;
    FFallSound: TSoundType;

    FMiddleHeight: Single;
  protected
    { Calculated @link(Radius). In descendants only @link(Prepare) can set this. }
    RadiusFromPrepare: Single;

    function RadiusFromPrepareDefault(
      const AnAnimation: TCastlePrecalculatedAnimation;
      const GravityUp: TVector3Single): Single;
  public
    const
      { Default value for TCreatureKind.DefaultMaxLife.
        Yes, it's not a typo, this identifier starts with "DefaultDefault". }
      DefaultDefaultMaxLife = 100.0;
      DefaultFlying = false;
      DefaultKnockedBackDistance = 6.0 * 0.7;
      DefaultSoundDyingTiedToCreature = true;
      DefaultAttackDamageConst = 10.0;
      DefaultAttackDamageRandom = 10.0;
      DefaultAttackKnockbackDistance = 0.1;

    constructor Create(const AId: string); override;

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

    { Sphere radius for collision detection.
      Must be something <> 0 for collision detection.

      You can define it in the creature resource.xml file,
      by setting radius="xxx" attribute on the root <resource> element.

      If it's not defined (or zero) in resource.xml file,
      then we take calculated radius from RadiusFromPrepare property.
      RadiusFromPrepare is always calculated in @link(Prepare).
      All creature classes in CastleCreatures do calculate it in
      @link(Prepare) method, you can also override it in descendants
      by overriding @link(Prepare) method.

      Radius is not used when creature is dead, as dead creatures usually have wildly
      different boxes (tall humanoid creature probably has a flat bounding
      box when it's dead lying on the ground), so trying to use (the same)
      radius would only cause problems.

      This is always measured from Middle ("eye position") of the given creature.
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

    property SoundDying: TSoundType
      read FSoundDying write FSoundDying default stNone;

    { See TCreature.Sound3d TiedToCreature parameter docs.
      You can set this to false if you want SoundDying to last even
      after the creature object was destroyed. }
    property SoundDyingTiedToCreature: boolean
      read FSoundDyingTiedToCreature write FSoundDyingTiedToCreature
      default DefaultSoundDyingTiedToCreature;

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

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    { Distance the creature is knocked back when hurt (should reflect
      the creature weight, how easy it is to push this creature).

      Will always be multiplied by the knocking distance of the weapon that
      caused the push (which should reflect the force of the weapon blow),
      see TItemShortRangeWeaponKind.AttackKnockbackDistance.

      Only for TWalkAttackCreature, the final distance the creature
      is knocked back is capped
      by the time of the HurtAnimation (HurtAnimation.TimeDurationWithBack).
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
      and for explosion of TMissileCreatureKind.
      For example it is @italic(not) used by creatures that merely fire missiles,
      as in this case the missile is created as another creature
      and it's the missile that causes damage on impact.

      TCreature descendants may call AttackHurt
      in their ActualAttack implementation to easily apply this kind of damage
      to Player.

      All three AttackDamageXxx values must be >= 0.

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

  { Kind of creature with states: standing stil,
    walking (running), performing an attack and dying.

    Tracks the player (remembers last seen player 3D position,
    walks/flies to it, possibly through sectors/waypoints ---
    so it can pass through narrow doors in a labyrinth or walk over a narrow bridge).
    Attacks the player from the right distance (this can be either a melee attack,
    or shooting a missile --- which adds a missile to the 3D world).
    Runs away from the player (when he's too close and/or our health is low).

    There are a lot of settings to achieve particular behavior,
    e.g. cowardly/brave, offensive/defensive, melee/ranged, etc. }
  TWalkAttackCreatureKind = class(TCreatureKind)
  private
    FStandAnimation: TCastlePrecalculatedAnimation;
    FStandToWalkAnimation: TCastlePrecalculatedAnimation;
    FWalkAnimation: TCastlePrecalculatedAnimation;
    FAttackAnimation: TCastlePrecalculatedAnimation;
    FDyingAnimation: TCastlePrecalculatedAnimation;
    FDyingBackAnimation: TCastlePrecalculatedAnimation;
    FHurtAnimation: TCastlePrecalculatedAnimation;

    FStandAnimationFile: string;
    FStandToWalkAnimationFile: string;
    FWalkAnimationFile: string;
    FAttackAnimationFile: string;
    FDyingAnimationFile: string;
    FDyingBackAnimationFile: string;
    FHurtAnimationFile: string;

    FMoveSpeed: Single;
    FMinDelayBetweenAttacks: Single;
    FMaxAttackDistance: Single;
    FPreferredAttackDistance: Single;
    FActualAttackTime: Single;

    FSoundAttackStart: TSoundType;
    FLifeToRunAway: Single;
    FMaxAngleToAttack: Single;
    FMinLifeLossToHurt: Single;
    FChanceToHurt: Single;
    FMaxHeightAcceptableToFall: Single;
    FRandomWalkDistance: Single;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
    const
      DefaultMoveSpeed = 10.0;
      DefaultMinDelayBetweenAttacks = 5.0;
      DefaultMaxAttackDistance = 35.0;
      DefaultPreferredAttackDistance = 30.0 * 0.7;
      DefaultLifeToRunAway = 0.3;
      DefaultActualAttackTime = 0.0;
      { Default TWalkAttackCreatureKind.MaxAngleToAttack. 30 degrees. }
      DefaultMaxAngleToAttack = Pi / 6;
      DefaultMinLifeLossToHurt = 0.0;
      DefaultChanceToHurt = 1.0;
      DefaultMaxHeightAcceptableToFall = 2.0 * 0.7;
      DefaultRandomWalkDistance = 10.0;

    constructor Create(const AId: string); override;

    { An animation of standing still.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property StandAnimation: TCastlePrecalculatedAnimation read FStandAnimation;

    { An animation when creature changes from standing still to walking.
      Beginning must be on time 0.
      It's beginnig must glue with beginning of StandAnimation,
      it's ending must glue with beginning of WalkAnimation. }
    property StandToWalkAnimation: TCastlePrecalculatedAnimation read FStandToWalkAnimation;

    { An animation of walking.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property WalkAnimation: TCastlePrecalculatedAnimation read FWalkAnimation;

    { An animation of attacking.
      Beginning must be on time 0.
      Beginning and end of it should roughly glue with frames WalkAnimation
      and StandAnimation.

      I used to have here property like AttacksWhenWalking for the creature,
      to indicate whether creature changes state like
      "wasWalk -> wasAttack -> wasWalk" or
      "wasStand -> wasAttack -> wasStand". But this wasn't good.
      Intelligent creature sometimes attacks when walking (e.g. if it just
      made the distance to the player closer) or when standing
      (when the distance was already close enough). And after performing
      the attack, the creature doesn't need to go back to state
      before the attack. }
    property AttackAnimation: TCastlePrecalculatedAnimation read FAttackAnimation;

    { An animation of dying.
      Beginning must be on time 0.
      Beginning should *more-or-less* look like any point of the stand/attack/walk
      animations. Note that we can display this animation infinitely,
      so it must work good after Time > it's TimeEnd. }
    property DyingAnimation: TCastlePrecalculatedAnimation read FDyingAnimation;

    { An optional dying animation. May be @nil, and corresponding
      DyingBackAnimationFile may be ''. If not @nil, this will be used
      if creature is killed by hitting it in the back (and normal
      DyingAnimation is used only when it's killed by hitting from the front).

      The direction of last hit is taken from LastHurtDirection. }
    property DyingBackAnimation: TCastlePrecalculatedAnimation read FDyingBackAnimation;

    { Animation when the creature will be hurt.
      Beginning must be on time 0.
      Beginning and end should *more-or-less* look like
      any point of the stand/attack/walk animations.
      Note that this animation will not loop, it will be played
      for TimeDurationWithBack time. }
    property HurtAnimation: TCastlePrecalculatedAnimation read FHurtAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving in wasWalk. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMoveSpeed;

    { Minimum delay between one attack and the other, in seconds.
      Note that actually setting this to 0 doesn't do much ---
      because minumum delay will still be bounded by the duration
      of AttackAnimation. }
    property MinDelayBetweenAttacks: Single
      read FMinDelayBetweenAttacks write FMinDelayBetweenAttacks
      default DefaultMinDelayBetweenAttacks;

    { Maximum distance between player and creature to allow creature
      to start attack. More precisely, this is measured between
      Player.Position and creature's Middle. }
    property MaxAttackDistance: Single
      read FMaxAttackDistance write FMaxAttackDistance
      default DefaultMaxAttackDistance;

    { The preferred distance between player and the creature
      to perform the attack. This must always be <= MaxAttackDistance.
      The idea is that the creature can attack player from MaxAttackDistance,
      but still it will walk closer to the player --- until the distance
      is PreferredAttackDistance. }
    property PreferredAttackDistance: Single
      read FPreferredAttackDistance write FPreferredAttackDistance
      default DefaultPreferredAttackDistance;

    { The time point within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime
      default DefaultActualAttackTime;

    { Player when attacking, that is when entering wasAttack state.
      Played at creature Middle.
      Sometimes you may prefer to rather play a sound at ActualAttack
      (at hit/miss) --- then just do it in overriden ActualAttack. }
    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;

    { Portion of life when the creature decides it's best to run away
      from enemy (player).
      If @code(Life <= MaxLife * LifeToRunAway) and distance to the
      player is too short (shorter than MaxAttackDistance / 4),
      the creature runs away. }
    property LifeToRunAway: Single
      read FLifeToRunAway write FLifeToRunAway default DefaultLifeToRunAway;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    { Because most of the creatures will have their weapon
      on their front (teeth, shooting hands, claws, whatever),
      so they can attack player only when their Direction is somewhat
      close to the direction to player.

      More precisely, the attack is allowed to start only when
      the angle between current Direction and the vector
      from creature's Middle to the player's Position
      is <= MaxAngleToAttack.

      This is in radians. }
    property MaxAngleToAttack: Single
      read FMaxAngleToAttack write FMaxAngleToAttack
      default DefaultMaxAngleToAttack;

    { When creature is wounded for more than MaxLife * MinLifeLossToHurt
      points and moreover Random < ChanceToHurt then creature will
      change to wasHurt state and be knocked back.
      Changing to wasHurt state means that any other state will be
      interrupted (e.g. player can interrupt
      creature's attack this way if ActualAttackTime > 0).

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
  end;

  { Creature that blindly moves in a given direction.
    It just moves into the given direction
    (with some possible twists, e.g. it can be a "homing"
    missile and/or be dragged down by gravity).
    On collision, it explodes, potentially hurting the alive 3D object
    that was colliding (player or other creatures).

    Missiles ignore TCreatureKind.Flying, they use their own way to handle
    gravity with DirectionFallSpeed. }
  TMissileCreatureKind = class(TCreatureKind)
  private
    FAnimation: TCastlePrecalculatedAnimation;
    FAnimationFile: string;
    FMoveSpeed: Single;
    FSoundExplosion: TSoundType;
    FCloseDirectionToTargetSpeed: Single;
    FPauseBetweenSoundIdle: Single;
    FSoundIdle: TSoundType;
    FHitsPlayer: boolean;
    FHitsCreatures: boolean;
    FDirectionFallSpeed: Single;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
    const
      DefaultMoveSpeed = 35.0;
      DefaultCloseDirectionToTargetSpeed = 0.0;
      DefaultPauseBetweenSoundIdle = 2.5;
      DefaultHitsPlayer = true;
      DefaultHitsCreatures = false;
      DefaultDirectionFallSpeed = 0.0;

    constructor Create(const AId: string); override;

    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TCastlePrecalculatedAnimation read FAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMoveSpeed;

    property SoundExplosion: TSoundType
      read FSoundExplosion write FSoundExplosion default stNone;

    function CreatureClass: TCreatureClass; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

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
    This is just a single TCastlePrecalculatedAnimation represented as
    a creature. }
  TStillCreatureKind = class(TCreatureKind)
  private
    FAnimation: TCastlePrecalculatedAnimation;
    FAnimationFile: string;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TCastlePrecalculatedAnimation read FAnimation;

    function CreatureClass: TCreatureClass; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  { Base creature, using any TCreatureKind. }
  TCreature = class(T3DAlive)
  private
    FKind: TCreatureKind;
    FLifeTime: Single;

    UsedSounds: TSoundList;
    FSoundDyingEnabled: boolean;

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
    property SoundDyingEnabled: boolean read FSoundDyingEnabled
      write FSoundDyingEnabled default true;

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

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack,
    wasDying, wasDyingBack, wasHurt, wasSpecial1);

  { Creature using TWalkAttackCreatureKind. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TWalkAttackCreatureState;

    FStateChangeTime: Single;

    { time of last FState change to wasAttack, taken from LifeTime. }
    LastAttackTime: Single;
    { Set to true each time you enter wasAttack, set back to false
      if ActualAttack was called. }
    ActualAttackDone: boolean;

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

    procedure SetState(Value: TWalkAttackCreatureState); virtual;

    { Use this in ActualAttack for short range creatures. }
    function ShortRangeActualAttackHits: boolean;

    procedure SetLife(const Value: Single); override;

    function DebugCaption: string; override;

    { Last State change time, taken from LifeTime. }
    property StateChangeTime: Single read FStateChangeTime;

    function GetChild: T3D; override;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    destructor Destroy; override;

    { Shortcut for TWalkAttackCreatureKind(Kind). }
    function WAKind: TWalkAttackCreatureKind;

    property State: TWalkAttackCreatureState read FState
      default wasStand;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;

    procedure Hurt(const LifeLoss: Single; const HurtDirection: TVector3Single;
      const AKnockbackDistance: Single); override;

    { The method where you must actually do your attack
      --- fire a missile, lower player's life etc.

      This happens in the middle of AttackAnimation,
      see also ActualAttackTime. Of course you should use
      current creature Position, Middle, Direction
      etc. to determine things like missile starting position
      and direction.

      If creature is doing some short-range attack
      you can also just lower here player's Life. Remember in this
      case to check that player is close enough; in general situation,
      you can't depend that player is still within MaxAttackDistance
      --- if ActualAttackTime is large, then player had some time
      to back off between AttackAnimation was started and ActualAttack
      is called. }
    procedure ActualAttack; virtual; abstract;
  end;

  { Creature using TMissileCreatureKind. }
  TMissileCreature = class(TCreature)
  private
    LastSoundIdleTime: Single;
    procedure ExplodeCore;
    procedure ExplodeWithPlayer;
    procedure ExplodeWithCreature(Creature: TCreature);
  protected
    function GetChild: T3D; override;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    { Shortcut for TMissileCreatureKind(Kind). }
    function MissileKind: TMissileCreatureKind;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Creature using TStillCreatureKind. }
  TStillCreature = class(TCreature)
  protected
    function GetChild: T3D; override;
  public
    { Shortcut for TStillCreatureKind(Kind). }
    function StillKind: TStillCreatureKind;

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

constructor TCreatureKind.Create(const AId: string);
begin
  inherited Create(AId);
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FKnockedBackDistance := DefaultKnockedBackDistance;
  FKnockBackSpeed := DefaultKnockBackSpeed;
  FSoundDyingTiedToCreature := DefaultSoundDyingTiedToCreature;
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

procedure TCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  KnockBackSpeed := KindsConfig.GetFloat('knock_back_speed',
    DefaultKnockBackSpeed);
  KnockedBackDistance := KindsConfig.GetFloat('knocked_back_distance',
    DefaultKnockedBackDistance);
  Flying := KindsConfig.GetValue('flying',
    DefaultFlying);
  SoundDyingTiedToCreature := KindsConfig.GetValue('sound_dying_tied_to_creature',
    DefaultSoundDyingTiedToCreature);
  DefaultMaxLife := KindsConfig.GetFloat('default_max_life',
    DefaultDefaultMaxLife);
  RadiusFromFile := KindsConfig.GetFloat('radius',
    0.0);
  AttackDamageConst := KindsConfig.GetFloat('short_range_attack/damage/const',
    DefaultAttackDamageConst);
  AttackDamageRandom := KindsConfig.GetFloat('short_range_attack/damage/random',
    DefaultAttackDamageRandom);
  AttackKnockbackDistance := KindsConfig.GetFloat('short_range_attack/knockback_distance',
    DefaultAttackKnockbackDistance);
  MiddleHeight := KindsConfig.GetFloat('middle_height', DefaultMiddleHeight);
  FallMinHeightToSound := KindsConfig.GetFloat('fall/sound/min_height', DefaultCreatureFallMinHeightToSound);
  FallMinHeightToDamage := KindsConfig.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
  FallDamageScaleMin := KindsConfig.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
  FallDamageScaleMax := KindsConfig.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);

  SoundSuddenPain := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_sudden_pain', ''));
  SoundDying := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_dying', ''));
  FallSound := SoundEngine.SoundFromName(
    KindsConfig.GetValue('fall/sound/name', DefaultCreatureFallSoundName), false);
end;

function TCreatureKind.Radius: Single;
begin
  if RadiusFromFile <> 0 then
    Result := RadiusFromFile else
    Result := RadiusFromPrepare;
end;

function TCreatureKind.RadiusFromPrepareDefault(
  const AnAnimation: TCastlePrecalculatedAnimation;
  const GravityUp: TVector3Single): Single;
var
  GC: Integer;
  Box: TBox3D;
  MaxRadiusForGravity: Single;
begin
  { calculate default RadiusFromPrepare.
    Descendants can override Prepare to provide better RadiusFromPrepare
    (or define radius in resource.xml file), so it's Ok to make here
    some assumptions that should suit usual cases, but not necessarily
    all possible cases --- e.g. our MaxRadiusForGravity calculation assumes you
    let default T3DCustomTransform.PreferredHeight algorithm to work. }

  GC := MaxAbsVectorCoord(GravityUp);
  Box := AnAnimation.BoundingBox;

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

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(const AId: string);
begin
  inherited Create(AId);

  MoveSpeed := DefaultMoveSpeed;
  FMinDelayBetweenAttacks := DefaultMinDelayBetweenAttacks;
  FMaxAttackDistance := DefaultMaxAttackDistance;
  FPreferredAttackDistance := DefaultPreferredAttackDistance;
  FLifeToRunAway := DefaultLifeToRunAway;
  FActualAttackTime := DefaultActualAttackTime;
  FMaxAngleToAttack := DefaultMaxAngleToAttack;
  FMinLifeLossToHurt := DefaultMinLifeLossToHurt;
  FChanceToHurt := DefaultChanceToHurt;
  FMaxHeightAcceptableToFall := DefaultMaxHeightAcceptableToFall;
  FRandomWalkDistance := DefaultRandomWalkDistance;
end;

procedure TWalkAttackCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;

  PreparePrecalculatedAnimation(FStandAnimation      , FStandAnimationFile      , BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FStandToWalkAnimation, FStandToWalkAnimationFile, BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FWalkAnimation       , FWalkAnimationFile       , BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FAttackAnimation     , FAttackAnimationFile     , BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FDyingAnimation      , FDyingAnimationFile      , BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FDyingBackAnimation  , FDyingBackAnimationFile  , BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FHurtAnimation       , FHurtAnimationFile       , BaseLights, DoProgress);

  RadiusFromPrepare := RadiusFromPrepareDefault(StandAnimation, GravityUp);
end;

function TWalkAttackCreatureKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 12;
end;

procedure TWalkAttackCreatureKind.ReleaseCore;
begin
  FStandAnimation := nil;
  FStandToWalkAnimation := nil;
  FWalkAnimation := nil;
  FAttackAnimation := nil;
  FDyingAnimation := nil;
  FDyingBackAnimation := nil;
  FHurtAnimation := nil;

  inherited;
end;

procedure TWalkAttackCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  ActualAttackTime := KindsConfig.GetFloat('actual_attack_time',
    DefaultActualAttackTime);
  MoveSpeed := KindsConfig.GetFloat('move_speed',
    DefaultMoveSpeed);
  MaxAttackDistance := KindsConfig.GetFloat('max_attack_distance',
    DefaultMaxAttackDistance);
  PreferredAttackDistance := KindsConfig.GetFloat('preferred_attack_distance',
    DefaultPreferredAttackDistance);
  MinDelayBetweenAttacks := KindsConfig.GetFloat('min_delay_between_attacks',
    DefaultMinDelayBetweenAttacks);
  LifeToRunAway := KindsConfig.GetFloat('life_to_run_away',
    DefaultLifeToRunAway);
  MaxAngleToAttack := KindsConfig.GetFloat('max_angle_to_attack',
    DefaultMaxAngleToAttack);
  MinLifeLossToHurt := KindsConfig.GetFloat('min_life_loss_to_hurt',
    DefaultMinLifeLossToHurt);
  ChanceToHurt := KindsConfig.GetFloat('chance_to_hurt',
    DefaultChanceToHurt);
  MaxHeightAcceptableToFall := KindsConfig.GetFloat('max_height_acceptable_to_fall',
    DefaultMaxHeightAcceptableToFall);
  RandomWalkDistance := KindsConfig.GetFloat('random_walk_distance',
    DefaultRandomWalkDistance);

  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_attack_start', ''));

  FStandAnimationFile := KindsConfig.GetFileName('stand_animation');
  FStandToWalkAnimationFile := KindsConfig.GetFileName('stand_to_walk_animation');
  FWalkAnimationFile := KindsConfig.GetFileName('walk_animation');
  FAttackAnimationFile := KindsConfig.GetFileName('attack_animation');
  FDyingAnimationFile := KindsConfig.GetFileName('dying_animation');
  FDyingBackAnimationFile := KindsConfig.GetFileName('dying_back_animation', true);
  FHurtAnimationFile := KindsConfig.GetFileName('hurt_animation');
end;

{ TMissileCreatureKind ---------------------------------------------------- }

constructor TMissileCreatureKind.Create(const AId: string);
begin
  inherited Create(AId);
  FMoveSpeed := DefaultMoveSpeed;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
  FDirectionFallSpeed := DefaultDirectionFallSpeed;
end;

procedure TMissileCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
var
  Box: TBox3D;
begin
  inherited;
  PreparePrecalculatedAnimation(FAnimation, FAnimationFile, BaseLights, DoProgress);

  Box := Animation.Scenes[0].BoundingBox;
  { Use MinSize for missile, since smaller radius for missiles
    forces player to aim more precisely. Smaller radius may also allow some
    partial collisions to go undetected, but that's not a problem as the
    collisions imperfections are not noticeable for fast moving missiles. }
  if not Box.IsEmpty then
    RadiusFromPrepare := Box.MinSize / 2;
end;

function TMissileCreatureKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TMissileCreatureKind.ReleaseCore;
begin
  FAnimation := nil;
  inherited;
end;

function TMissileCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TMissileCreature;
end;

procedure TMissileCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  MoveSpeed := KindsConfig.GetFloat('move_speed',
    DefaultMoveSpeed);
  CloseDirectionToTargetSpeed := KindsConfig.GetFloat('close_direction_to_target_speed',
    DefaultCloseDirectionToTargetSpeed);
  PauseBetweenSoundIdle := KindsConfig.GetFloat('pause_between_sound_idle',
    DefaultPauseBetweenSoundIdle);
  HitsPlayer := KindsConfig.GetValue('hits_player',
    DefaultHitsPlayer);
  HitsCreatures := KindsConfig.GetValue('hits_creatures',
    DefaultHitsCreatures);
  DirectionFallSpeed := KindsConfig.GetFloat('direction_fall_speed',
    DefaultDirectionFallSpeed);

  SoundExplosion := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_explosion', ''));
  SoundIdle := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_idle', ''));

  FAnimationFile := KindsConfig.GetFileName('fly_animation');
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
    with ground. So Fall should be overriden to make ExplodeCore,
    and GrowSpeed should be disabled below to be 0. (This is obviously doable.)

    We also want to turn off Gravity to use Gravity=false case when using
    MiddleHeight, so MiddleHeight is always between bounding box bottom and top
    for missiles. See T3DCustomTransform.MiddleHeight. }

  Result.Gravity := false;
end;

{ TStillCreatureKind ---------------------------------------------------- }

procedure TStillCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;
  PreparePrecalculatedAnimation(FAnimation, FAnimationFile, BaseLights, DoProgress);

  RadiusFromPrepare := RadiusFromPrepareDefault(Animation, GravityUp);
end;

function TStillCreatureKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TStillCreatureKind.ReleaseCore;
begin
  FAnimation := nil;
  inherited;
end;

function TStillCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TStillCreature;
end;

procedure TStillCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  FAnimationFile := KindsConfig.GetFileName('stand_animation');
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
  FSoundDyingEnabled := true;
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
    { When dies, we don't play SoundSuddenPain sound. We will play SoundDying. }
    if SoundDyingEnabled then
      Sound3d(Kind.SoundDying, 1.0, Kind.SoundDyingTiedToCreature);
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
    FState := wasStand;
    FStateChangeTime := 0;
  end else
  begin
    { This means that the creature is created already in dead state...
      So we start with wasDying state and set FStateChangeTime to fake
      the fact that creature was killed long time ago.

      This way the creature is created as a dead corpse, without making
      any kind of dying (or wounded) sound or animation. }
    FState := wasDying;
    FStateChangeTime := -1000;
  end;

  WaypointsSaved := TWaypointList.Create(false);
end;

destructor TWalkAttackCreature.Destroy;
begin
  FreeAndNil(WaypointsSaved);
  inherited;
end;

function TWalkAttackCreature.WAKind: TWalkAttackCreatureKind;
begin
  Result := TWalkAttackCreatureKind(Kind);
end;

procedure TWalkAttackCreature.SetState(Value: TWalkAttackCreatureState);
begin
  if FState <> Value then
  begin
    FState := Value;
    FStateChangeTime := LifeTime;
    { Some states require special initialization here. }
    case FState of
      wasAttack:
        begin
          Sound3d(WAKind.SoundAttackStart, 1.0);
          LastAttackTime := StateChangeTime;
          ActualAttackDone := false;
        end;
    end;
  end;
end;

procedure TWalkAttackCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

  { Is attack allowed ? }
  function AttackAllowed: boolean;
  var
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    Result := IdleSeesPlayer and
      (LifeTime - LastAttackTime > WAKind.MinDelayBetweenAttacks) and
      (IdleSqrDistanceToLastSeenPlayer <= Sqr(WAKind.MaxAttackDistance));

    if Result then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        VectorSubtract(LastSeenPlayer, Middle),
        Direction);
      Result := AngleRadBetweenTheDirectionToPlayer <= WAKind.MaxAngleToAttack;
    end;
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
        (IdleSqrDistanceToLastSeenPlayer > Sqr(WAKind.PreferredAttackDistance))
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
      (Life <= MaxLife * WAKind.LifeToRunAway) and
      (IdleSqrDistanceToLastSeenPlayer < Sqr(WAKind.MaxAttackDistance / 4));
  end;

  procedure InitAlternativeTarget;
  var
    Distance: Single;
    I: Integer;
  begin
    Distance := WAKind.RandomWalkDistance;

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

  procedure DoStand;
  var
    DirectionToPlayer: TVector3Single;
    AngleRadBetweenDirectionToPlayer: Single;
  begin
    if HasLastSeenPlayer then
    begin
      CalculateDirectionToPlayer(DirectionToPlayer, AngleRadBetweenDirectionToPlayer);

      if AttackAllowed then
        SetState(wasAttack) else
      if WantToRunAway or
         WantToWalkToPlayer(AngleRadBetweenDirectionToPlayer) then
        SetState(wasWalk) else
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
        SetState(wasWalk);
        InitAlternativeTarget;
      end else
      begin
        { Continue wasStand state }
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
          if AboveHeight > WAKind.MaxHeightAcceptableToFall + PreferredHeight then
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
        (not TooHighAboveTheGround(Middle + Direction * (WAKind.MoveSpeed * 0.2))) and

        { Use Move without wall-sliding here.
          Things using MoveAlongTheDirection depend on the fact that
          MoveAlongTheDirection will return false
          if no further way is possible (and wall-sliding would try instead
          to return true and correct target position).

          Our trick with "AlternativeTarget" should handle
          eventual problems with the track of creature, so wall-sliding
          should not be needed. }
        Move(Direction * (WAKind.MoveSpeed * CompSpeed), false, false);
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
        SetState(wasStand);
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
        SetState(wasStand);
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
          to wasStand, and this allows creature to rotate in wasStand
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
          So do nothing now. Just stay in wasWalk mode,
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
        SetState(wasStand);
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

    if AttackAllowed then
      SetState(wasAttack);
  end;

  procedure DoAttack;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;

    if (not ActualAttackDone) and (StateTime >= WAKind.ActualAttackTime) then
    begin
      ActualAttackDone := true;
      ActualAttack;
    end;

    if StateTime > WAKind.AttackAnimation.TimeEnd then
      { wasStand will quickly change to wasWalk if it will want to walk. }
      SetState(wasStand);
  end;

  procedure DoHurt;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;

    if StateTime > WAKind.HurtAnimation.TimeDurationWithBack then
    begin
      CancelKnockback;
      SetState(wasStand);
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

var
  Player: T3DOrient;
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if Dead and not (State in [wasDying, wasDyingBack]) then
  begin
    if (WAKind.DyingBackAnimation <> nil) and
       WasLackAttackBack then
      SetState(wasDyingBack) else
      SetState(wasDying);
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
    wasStand: DoStand;
    wasWalk: DoWalk;
    wasAttack: DoAttack;
    wasDying, wasDyingBack: ;
    wasHurt: DoHurt;
    wasSpecial1: { Should be handled in descendants. };
    else raise EInternalError.Create('FState ?');
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
    wasStand:
      Result := WAKind.StandAnimation.SceneFromTime(StateTime);
    wasWalk:
      if StateTime < WAKind.StandToWalkAnimation.TimeEnd then
        Result := WAKind.StandToWalkAnimation.SceneFromTime(StateTime) else
        Result := WAKind.WalkAnimation.SceneFromTime(
          StateTime - WAKind.StandToWalkAnimation.TimeEnd);
    wasAttack:
      Result := WAKind.AttackAnimation.SceneFromTime(StateTime);
    wasDying:
      Result := WAKind.DyingAnimation.SceneFromTime(StateTime);
    wasDyingBack:
      Result := WAKind.DyingBackAnimation.SceneFromTime(StateTime);
    wasHurt:
      Result := WAKind.HurtAnimation.SceneFromTime(StateTime);
    else raise EInternalError.Create('FState ?');
  end;
end;

procedure TWalkAttackCreature.SetLife(const Value: Single);
begin
  if (not Dead) and
    (Life - Value > WAKind.MinLifeLossToHurt * MaxLife) and
    ( (WAKind.ChanceToHurt = 1.0) or
      (Random < WAKind.ChanceToHurt) ) then
    SetState(wasHurt);
  inherited;
end;

function TWalkAttackCreature.ShortRangeActualAttackHits: boolean;
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
    It must be large enough to compensate even large WAKind.MaxAttackDistance,
    it must be small enough so that player should not be able to avoid
    our attacks just by standing very close to the creature.

    So we have to check a couple of bounding boxes.
    If we move our boxes by Box3DMinSize(B), we're sure that
    each box will stick to the previous and next. But maybe
    there will be some areas around the sticking points ?
    So B.MinSize / 2 seems safe. }
  DistanceIncrease := B.MinSize / 2;

  DistanceLength := DistanceIncrease;
  while DistanceLength < WAKind.MaxAttackDistance do
  begin
    if B.Translate(VectorScale(Direction, DistanceLength)).Collision(PB) then
      Exit(true);
    DistanceLength += DistanceIncrease;
  end;

  { Check one last time for WAKind.MaxAttackDistance }
  Result := B.Translate(
    VectorScale(Direction, WAKind.MaxAttackDistance)).Collision(PB);
end;

function TWalkAttackCreature.DebugCaption: string;
const
  StateName: array [TWalkAttackCreatureState] of string =
  ( 'Stand', 'Walk', 'Attack', 'Dying', 'DyingBack', 'Hurt', 'Special1' );
begin
  Result := (inherited DebugCaption) + ' ' + StateName[State];
end;

procedure TWalkAttackCreature.Hurt(const LifeLoss: Single;
  const HurtDirection: TVector3Single;
  const AKnockbackDistance: Single);
begin
  inherited Hurt(LifeLoss, HurtDirection,
    AKnockbackDistance * WAKind.KnockedBackDistance);
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

function TMissileCreature.MissileKind: TMissileCreatureKind;
begin
  Result := TMissileCreatureKind(Kind);
end;

procedure TMissileCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  Player: T3DOrient;

  function MissileMoveAllowed(const OldPos, NewPos: TVector3Single): boolean;
  begin
    if (not MissileKind.HitsPlayer) and (Player <> nil) then Player.Disable;
    if not MissileKind.HitsCreatures then Inc(DisableCreatures);
    try
      Result := MoveAllowed(OldPos, NewPos, false);
    finally
      if not MissileKind.HitsCreatures then Dec(DisableCreatures);
      if (not MissileKind.HitsPlayer) and (Player <> nil) then Player.Enable;
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
  Translate(Direction * (MissileKind.MoveSpeed * CompSpeed));
  NewMiddle := Middle;

  if not MissileMoveAllowed(OldMiddle, NewMiddle) then
  begin
    { Check collision missile <-> player.
      Maybe I'll switch to using bounding Sphere here one day?
      No reason for sphere or box, either way, for now. }
    if MissileKind.HitsPlayer and
      (Player <> nil) and
      Player.BoundingBox.Collision(BoundingBox) then
      ExplodeWithPlayer;

    if MissileKind.HitsCreatures then
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
            ExplodeWithCreature(C);
            { TODO: projectiles shouldn't do here "break". }
            break;
          end;
        end;
    end;

    ExplodeCore;
  end;

  if MissileKind.DirectionFallSpeed <> 0 then
  begin
    NewDirection := Direction -
      World.GravityUp * MissileKind.DirectionFallSpeed * CompSpeed;
    Direction := NewDirection;
  end;

  if (MissileKind.CloseDirectionToTargetSpeed <> 0.0) and (Player <> nil) then
  begin
    TargetDirection := Player.Position - Position;
    AngleBetween := AngleRadBetweenVectors(TargetDirection, Direction);
    AngleChange := MissileKind.CloseDirectionToTargetSpeed * CompSpeed;
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
     (LifeTime - LastSoundIdleTime > MissileKind.PauseBetweenSoundIdle) then
  begin
    LastSoundIdleTime := LifeTime;
    Sound3d(MissileKind.SoundIdle, 0.0);
  end;
end;

function TMissileCreature.GetChild: T3D;
begin
  if not Kind.Prepared then Exit(nil);

  Result := MissileKind.Animation.SceneFromTime(LifeTime);
end;

procedure TMissileCreature.ExplodeCore;
begin
  { TODO: for some missiles, their explosion may hurt everyone around.
    So do here additional checks for collision and hurt player and creatures. }

  Sound3d(MissileKind.SoundExplosion, 0, false);

  Hurt(1000 * 1000, ZeroVector3Single, 0);
end;

procedure TMissileCreature.ExplodeWithPlayer;
begin
  AttackHurt;
end;

procedure TMissileCreature.ExplodeWithCreature(Creature: TCreature);
begin
  Creature.Hurt(Kind.AttackDamageConst +
    Random * Kind.AttackDamageRandom, Direction,
    Kind.AttackKnockbackDistance);
end;

{ TStillCreature ----------------------------------------------------------- }

function TStillCreature.StillKind: TStillCreatureKind;
begin
  Result := TStillCreatureKind(Kind);
end;

function TStillCreature.GetChild: T3D;
begin
  if not Kind.Prepared then Exit(nil);

  Result := StillKind.Animation.SceneFromTime(LifeTime);
end;

procedure TStillCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { TODO: do explosion anim for barrel. }
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
end.
