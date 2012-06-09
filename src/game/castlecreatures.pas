{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Creatures. }
unit CastleCreatures;

interface

uses Classes, VectorMath, PrecalculatedAnimation, Boxes3D, CastleClassUtils, CastleUtils,
  CastleScene, SectorsWaypoints,
  CastleResources, ALSoundAllocator, CastleXMLConfig, Base3D,
  ALSoundEngine, Frustum, X3DNodes, CastleColors, FGL;

const
  { Default value for TCreatureKind.DefaultMaxLife.
    Yes, it's not a typo, this identifier starts with "DefaultDefault". }
  DefaultDefaultMaxLife = 100.0;

  DefaultFlying = false;

  DefaultMoveSpeed = 10.0;
  DefaultMinDelayBetweenAttacks = 5.0;
  DefaultMaxAttackDistance = 35.0;
  DefaultPreferredAttackDistance = 30.0 * 0.7;
  DefaultMissileMoveSpeed = 35.0;
  DefaultKnockedBackDistance = 6.0 * 0.7;
  DefaultLifeToRunAway = 0.3;
  DefaultActualAttackTime = 0.0;
  DefaultMaxAngleToAttack = Pi / 6 { 30 degrees };

  DefaultSoundDyingTiedToCreature = true;

  DefaultMinLifeLossToHurt = 0.0;
  DefaultChanceToHurt = 1.0;

  DefaultCloseDirectionToTargetSpeed = 0.0;

  DefaultShortRangeAttackDamageConst = 10.0;
  DefaultShortRangeAttackDamageRandom = 10.0;
  DefaultShortRangeAttackKnockbackDistance = 0.1;

  DefaultMaxHeightAcceptableToFall = 2.0 * 0.7;
  DefaultFallDownLifeLossScale = 1.0;

  DefaultCreatureRandomWalkDistance = 10.0;

  DefaultPauseBetweenSoundIdle = 2.5;

  DefaultHitsPlayer = true;
  DefaultHitsCreatures = false;
  DefaultFallingDownSpeed = 50.0;

  DefaultMiddleHeight = 1.0;

  DefaultCastShadowVolumes = true;

type
  TCreature = class;

  TCreatureClass = class of TCreature;

  TCreatureKind = class(T3DResource)
  private
    FFlying: boolean;
    FSoundSuddenPain: TSoundType;
    FSoundDying: TSoundType;
    FSoundDyingTiedToCreature: boolean;
    FDefaultMaxLife: Single;
    FKnockedBackDistance: Single;
    FKnockBackSpeed: Single;
    FFallingDownSpeed: Single;

    RadiusFromFile: Single;

    FShortRangeAttackDamageConst: Single;
    FShortRangeAttackDamageRandom: Single;
    FShortRangeAttackKnockbackDistance: Single;

    FFallDownLifeLossScale: Single;

    FMiddleHeight: Single;

    FCastShadowVolumes: boolean;
  protected
    { In descendants only Prepare can (and should!) set this. }
    RadiusFromPrepare: Single;
  public
    constructor Create(const AId: string); override;

    { If @true, then the creature flies. Otherwise it always tries to move only
      horizontally (which means that Direction is always orthogonal
      to World.GravityUp), and it falls down when Position is above
      the ground. }
    property Flying: boolean read FFlying write FFlying default DefaultFlying;

    { Camera radius when moving.
      You should make sure that it's something <> 0 for collision detection.

      This is always calculated like:
      If RadiusFromFile <> 0, then take it.
      Otherwise take RadiusFromPrepare.
      So there are 2 ways to initialize this:
      1. Set this in creatures/kinds.xml file.
      2. Set RadiusFromPrepare properly.

      Setting this in creatures/kinds.xml file to non-zero will effectively
      ignore any RadiusFromPrepare value.

      And Prepare should always set RadiusFromPrepare
      to something non-zero. So note that before Prepare was called,
      Radius may remain zero. But you can depend on the fact that
      it's non-zero after Prepare.

      Note that this is always measured from Middle of given
      creature. So take this into account when calcuating
      RadiusFromPrepare or writing it in kinds.xml file. }
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

    { The default MaxLife for this Kind.

      You cannot depend that every creature has this MaxLife ---
      caller can pass any MaxLife value when creating creature by
      CreateCreature.
      This is only a "suggested" default for MaxLife of this creature. }
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
      const MaxLife: Single): TCreature;
    function CreateCreature(World: T3DWorld;
      const APosition, ADirection: TVector3Single): TCreature;
    { @groupEnd }

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

    { Short range attack damage and knockback.
      These will be used only by some creatures, the ones that do
      ShortRangeAttackHurt in their ActualAttack implementation.

      ShortRangeAttackDamageConst and ShortRangeAttackDamageRandom
      and ShortRangeAttackKnockbackDistance must be >= 0.

      ShortRangeAttackKnockbackDistance = 0 means no knockback.

      Exploding missiles also use these properties.
      When missile explodes on impact with other alive 3D object,
      it's damage and knockback are taken from these properties.

      @groupBegin }
    property ShortRangeAttackDamageConst: Single
      read FShortRangeAttackDamageConst
      write FShortRangeAttackDamageConst
      default DefaultShortRangeAttackDamageConst;

    property ShortRangeAttackDamageRandom: Single
      read FShortRangeAttackDamageRandom
      write FShortRangeAttackDamageRandom
      default DefaultShortRangeAttackDamageRandom;

    property ShortRangeAttackKnockbackDistance: Single
      read FShortRangeAttackKnockbackDistance
      write FShortRangeAttackKnockbackDistance
      default DefaultShortRangeAttackKnockbackDistance;
    { @groupEnd }

    property FallDownLifeLossScale: Single
      read FFallDownLifeLossScale
      write FFallDownLifeLossScale
      default DefaultFallDownLifeLossScale;

    { Determines how the creature's Middle will be calculated.
      Actually, this determines how the HeightBetweenLegsAndMiddle
      will be calculated, and this is used to calculate Middle.
      HeightBetweenLegsAndMiddle is just current scene box height
      multiplied by this, so 1.0 means that Middle is at the top
      of the box, 0.0 means at the bottom, 0.5 means at the middle etc. }
    property MiddleHeight: Single
      read FMiddleHeight
      write FMiddleHeight
      default DefaultMiddleHeight;

    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes
      default DefaultCastShadowVolumes;

    { How fast the creature falls down, in units per second.

      May be zero if creature is invulnerable to gravity
      (which is usually, but not always, sensible for flying creatures).
      For now, FallingDownSpeed also turns off "growing up" effect
      to make creature stand above the ground. }
    property FallingDownSpeed: Single
      read FFallingDownSpeed write FFallingDownSpeed default DefaultFallingDownSpeed;
  end;

  { A TCreatureKind that has simple states:
    standing stil, walking (aka running), performing an attack and dying. }
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
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
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
      default DefaultCreatureRandomWalkDistance;
  end;

  { A missile is also a creature.
    It's a little dumb creature, that just moves into the given
    direction (with some possible twists, e.g. it can be a "homing"
    missile and/or be dragged down by gravity).
    On any collision, it explodes, potentially hurting the alive 3D object
    that was colliding.

    Missiles are always Flying for now. }
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
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
    constructor Create(const AId: string); override;

    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TCastlePrecalculatedAnimation read FAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMissileMoveSpeed;

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
  end;

  { A really dumb creature that just stays still during the whole
    game. Basically this is just TCastlePrecalculatedAnimation that is displayed as
    a creature. }
  TStillCreatureKind = class(TCreatureKind)
  private
    FAnimation: TCastlePrecalculatedAnimation;
    FAnimationFile: string;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
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

  TCreature = class(T3DAlive)
  private
    FKind: TCreatureKind;
    FLifeTime: Single;

    { For gravity work. }
    FallingDownStartHeight: Single;
    FIsFallingDown: boolean;

    UsedSounds: TALSoundList;
    FSoundDyingEnabled: boolean;

    procedure SoundSourceUsingEnd(Sender: TALSound);
  protected
    property LifeTime: Single read FLifeTime;
    procedure SetLife(const Value: Single); override;
    function GetExists: boolean; override;

    { Current scene to be rendered.
      Note that this may be called before we're added to World (at the end of our
      construction), so make it work always reliably. }
    { function GetChild: T3D; override; }

    { PositionFromMiddle calculates value of T3DOrient.Position,
      assuming that T3DOrient.Middle is as given.
      Since our Middle implementation is derived from Position,
      PositionFromMiddle is the reverse.

      LerpLegsMiddle interpolated between Position and Middle
      (intuitively, legs and eye positions).
      It must be equal to Lerp(A, Position, Middle)
      (but usually can be calculated more efficiently than calling Lerp).

      In this class they calculate Middle as Position
      moved higher than HeightBetweenLegsAndMiddle.
      So if you want to change the meaning of these functions
      you can simply override only HeightBetweenLegsAndMiddle
      (and everything else will work OK).
      *Or* you can change all Middle, PositionFromMiddle, LerpLegsMiddle
      functions to use any approach to convert between legs and eye position.

      @groupBegin }
    function PositionFromMiddle(const AssumeMiddle: TVector3Single): TVector3Single; virtual;
    function LerpLegsMiddle(const A: Single): TVector3Single; virtual;
    { @groupEnd }

    procedure ShortRangeAttackHurt;

    function DebugCaption: string; virtual;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); virtual; reintroduce;

    destructor Destroy; override;

    property Kind: TCreatureKind read FKind;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { The height of Middle above Position.
      Calculated in this class using GetChild.BoundingBox.Data[1, 2]
      and Kind.MiddleHeight.
      Note that while GetChild may change, this also may change. }
    function HeightBetweenLegsAndMiddle: Single; virtual;

    function Middle: TVector3Single; override;

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

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;

    property Pushable default true;
    { Orientation matching creatures designed for castle1. }
    property Orientation default otUpZDirectionX;
  end;

  TCreatureList = class(specialize TFPGObjectList<TCreature>)
  end;

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack,
    wasDying, wasDyingBack, wasHurt, wasSpecial1);

  { A TCreature that has a kind always of TWalkAttackCreatureKind. }
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
      permanently blocked (so MoveAllowed returns true all the time)
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

  { A TCreature that has a kind always of TMissileCreatureKind. }
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

type
  TExistsEvent = function: boolean;
var
  { Global callback to control creatures existence. }
  OnCreatureExists: TExistsEvent;

implementation

uses SysUtils, DOM, GL, GLU, CastleGameCache,
  CastleFilesUtils, CastleGLUtils, ProgressUnit,
  CastleGameNotifications, CastleGameVideoOptions;

var
  DisableCreatures: Cardinal;

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AId: string);
begin
  inherited Create(AId);
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FKnockedBackDistance := DefaultKnockedBackDistance;
  FKnockBackSpeed := DefaultKnockBackSpeed;
  FSoundDyingTiedToCreature := DefaultSoundDyingTiedToCreature;
  FShortRangeAttackDamageConst := DefaultShortRangeAttackDamageConst;
  FShortRangeAttackDamageRandom := DefaultShortRangeAttackDamageRandom;
  FShortRangeAttackKnockbackDistance := DefaultShortRangeAttackKnockbackDistance;
  FFallDownLifeLossScale := DefaultFallDownLifeLossScale;
  FMiddleHeight := DefaultMiddleHeight;
  FCastShadowVolumes := DefaultCastShadowVolumes;
  FFallingDownSpeed := DefaultFallingDownSpeed;
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
  ShortRangeAttackDamageConst := KindsConfig.GetFloat('short_range_attack/damage/const',
    DefaultShortRangeAttackDamageConst);
  ShortRangeAttackDamageRandom := KindsConfig.GetFloat('short_range_attack/damage/random',
    DefaultShortRangeAttackDamageRandom);
  ShortRangeAttackKnockbackDistance := KindsConfig.GetFloat('short_range_attack/knockback_distance',
    DefaultShortRangeAttackKnockbackDistance);

  FallDownLifeLossScale := KindsConfig.GetFloat('fall_down_life_loss_scale',
    DefaultFallDownLifeLossScale);
  FFallingDownSpeed := KindsConfig.GetFloat('falling_down_speed',
    DefaultFallingDownSpeed);

  MiddleHeight := KindsConfig.GetFloat('middle_position_height',
    DefaultMiddleHeight);

  CastShadowVolumes := KindsConfig.GetValue('casts_shadow',
    DefaultCastShadowVolumes);

  SoundSuddenPain := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_sudden_pain', ''));
  SoundDying := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_dying', ''));
end;

function TCreatureKind.Radius: Single;
begin
  if RadiusFromFile <> 0 then
    Result := RadiusFromFile else
    Result := RadiusFromPrepare;
end;

function TCreatureKind.CreateCreature(World: T3DWorld;
  const APosition, ADirection: TVector3Single;
  const MaxLife: Single): TCreature;
begin
  { This is only needed if you used --debug-no-creatures or forgot
    to add creature to <resources>.

    Note: we experimented with moving this to TCreature.PrepareResource,
    call Kind.Prepare from there. But it just doesn't fully work:
    some creatures really want kind to be prepared
    before PrepareResource (from scene manager BeforeDraw) had a chance to work.
    For example, on missiles like thrown web we do Sound3d that uses LerpLegsMiddle.
    Also TCreature.Idle (which definitely needs kind) may get called before
    PrepareResource. IOW, PrepareResource is just too late. }
  Prepare(World.BaseLights);

  Result := CreatureClass.Create(World { owner }, MaxLife);
  { set properties that in practice must have other-than-default values
    to sensibly use the creature }
  Result.FKind := Self;
  Result.SetView(APosition, ADirection, World.GravityUp);
  Result.Life := MaxLife;
  Result.KnockBackSpeed := KnockBackSpeed;

  World.Add(Result);
end;

function TCreatureKind.CreateCreature(World: T3DWorld;
  const APosition, ADirection: TVector3Single): TCreature;
begin
  Result := CreateCreature(World, APosition, ADirection, DefaultMaxLife);
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
  FRandomWalkDistance := DefaultCreatureRandomWalkDistance;
end;

procedure TWalkAttackCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
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

  RadiusFromPrepare :=
    Min(StandAnimation.Scenes[0].BoundingBox.XYRadius,
        StandAnimation.Scenes[0].BoundingBox.Data[1, 2] * 0.75);
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
    DefaultCreatureRandomWalkDistance);

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
  Flying := true;
  FMoveSpeed := DefaultMissileMoveSpeed;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
end;

procedure TMissileCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
  inherited;
  PreparePrecalculatedAnimation(FAnimation, FAnimationFile, BaseLights, DoProgress);

  RadiusFromPrepare := Animation.Scenes[0].BoundingBox.XYRadius;
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
    DefaultMissileMoveSpeed);
  CloseDirectionToTargetSpeed := KindsConfig.GetFloat('close_direction_to_target_speed',
    DefaultCloseDirectionToTargetSpeed);
  PauseBetweenSoundIdle := KindsConfig.GetFloat('pause_between_sound_idle',
    DefaultPauseBetweenSoundIdle);
  HitsPlayer := KindsConfig.GetValue('hits_player',
    DefaultHitsPlayer);
  HitsCreatures := KindsConfig.GetValue('hits_creatures',
    DefaultHitsCreatures);

  SoundExplosion := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_explosion', ''));
  SoundIdle := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_idle', ''));

  FAnimationFile := KindsConfig.GetFileName('fly_animation');
end;

{ TStillCreatureKind ---------------------------------------------------- }

procedure TStillCreatureKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
  inherited;
  PreparePrecalculatedAnimation(FAnimation, FAnimationFile, BaseLights, DoProgress);

  RadiusFromPrepare := Animation.Scenes[0].BoundingBox.XYRadius;
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

{ TCreatureSoundSourceData --------------------------------------------------- }

type
  TCreatureSoundSourceData = class
  public
    SoundHeight: Single;
  end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited Create(AOwner);
  Pushable := true;
  MaxLife := AMaxLife;
  FSoundDyingEnabled := true;
  UsedSounds := TALSoundList.Create(false);
  Orientation := otUpZDirectionX;
end;

function TCreature.GetExists: boolean;
begin
  Result := (inherited GetExists) and (DisableCreatures = 0) and
    ((not Assigned(OnCreatureExists)) or OnCreatureExists());
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

      { Otherwise OnUsingEnd would call TCreature.SoundSourceUsingEnd,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here. }
      UsedSounds[I].OnUsingEnd := nil;
      UsedSounds[I].DoUsingEnd;
    end;
    FreeAndNil(UsedSounds);
  end;

  if Kind <> nil then
    Kind.Release;

  inherited;
end;

procedure TCreature.SoundSourceUsingEnd(Sender: TALSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  Sender.OnUsingEnd := nil;
  UsedSounds.Remove(Sender);
end;

procedure TCreature.Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
  TiedToCreature: boolean);
var
  NewSource: TALSound;
  SoundPosition: TVector3Single;
begin
  SoundPosition := LerpLegsMiddle(SoundHeight);
  NewSource := SoundEngine.Sound3d(SoundType, SoundPosition);
  if TiedToCreature and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnUsingEnd := @SoundSourceUsingEnd;
    NewSource.UserData := TCreatureSoundSourceData.Create;
  end;
end;

function TCreature.HeightBetweenLegsAndMiddle: Single;
begin
  Result := GetChild.BoundingBox.Data[1, 2] * Kind.MiddleHeight;
end;

function TCreature.PositionFromMiddle(
  const AssumeMiddle: TVector3Single): TVector3Single;
begin
  Result := AssumeMiddle;
  Result[2] -= HeightBetweenLegsAndMiddle;
end;

function TCreature.LerpLegsMiddle(const A: Single): TVector3Single;
begin
  Result := Position;
  Result[2] += HeightBetweenLegsAndMiddle * A;
end;

function TCreature.Middle: TVector3Single;
begin
  Result := inherited Middle;
  Result[2] += HeightBetweenLegsAndMiddle;
end;

procedure TCreature.Render(const Frustum: TFrustum; const Params: TRenderParams);

  procedure RenderBoundingGeometry;
  var
    Q: PGLUQuadric;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);

      glDrawBox3DWire(BoundingBox);

      glPushMatrix;
        glTranslatev(Middle);
        Q := NewGLUQuadric(false, GLU_NONE, GLU_OUTSIDE, GLU_LINE);
        try
          gluSphere(Q, Kind.Radius, 10, 10);
        finally gluDeleteQuadric(Q); end;
      glPopMatrix;
    glPopAttrib;
  end;

  procedure DoRenderDebugCaptions;
  const
    FontSize = 0.5;
  begin
    glPushMatrix;
      glTranslatef(0, 0, GetChild.BoundingBox.Data[1, 2]);
      glRotatef(90, 0, 0, 1);
      glRotatef(90, 1, 0, 0);
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
        DoRenderDebugCaptions;
    glPopMatrix;

    if RenderBoundingBoxes and
       (not Params.Transparent) and Params.ShadowVolumesReceivers then
      RenderBoundingGeometry;
  end;
end;

function TCreature.DebugCaption: string;
begin
  Result := Format('%s [%s / %s]',
    [Kind.Id, FloatToNiceStr(Life), FloatToNiceStr(MaxLife)]);
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
        TCreatureSoundSourceData(UsedSounds[I].UserData).SoundHeight);
      UsedSounds[I].Position := SoundPosition;
    end;
  end;

  procedure DoGravity;

    procedure FalledDown;
    var
      FallenHeight: Single;
    begin
      FallenHeight := FallingDownStartHeight - Position[2];
      if FallenHeight > 1.0 then
      begin
        Sound3d(stCreatureFalledDown, 0.1, false);
        if FallenHeight > 4.0 then
        begin
          Hurt(Max(0,
            Kind.FallDownLifeLossScale *
            FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2)),
            ZeroVector3Single, 0);
        end;
      end;
    end;

  const
    { Beware: GrowingUpSpeed is not only a graphical effect. Too large
      GrowingUpSpeed will allow creature to climb walls that are at high
      (almost-vertical) angle. }
    GrowingUpSpeed = 5.0;
    { HeightMargin is used to safeguard against floating point inaccuracy.
      Without this, creature would too often be considered "falling down"
      or "growing up". }
    HeightMargin = 1.01;
  var
    IsAbove: boolean;
    AboveHeight, RadiusIgnored: Single;
    OldIsFallingDown: boolean;
    FallingDownDistance, MaximumFallingDownDistance: Single;
  begin
    { Gravity does it's work here.
      This is extremely simplified version of Gravity work in TWalkCamera.
      (simplified, because creature doesn't need all these effects). }

    { Note that also here we do collision detection using Middle,
      not Position. See Middle docs for reasoning. }

    OldIsFallingDown := FIsFallingDown;

    IsAbove := MyHeight(Middle, AboveHeight);

    if AboveHeight > HeightBetweenLegsAndMiddle * HeightMargin then
    begin
      { Fall down }
      if not FIsFallingDown then
        FallingDownStartHeight := Position[2];

      FIsFallingDown := true;

      FallingDownDistance := Kind.FallingDownSpeed * CompSpeed;
      if IsAbove then
      begin
        MaximumFallingDownDistance :=
          AboveHeight - HeightBetweenLegsAndMiddle;

        { If you will fall down by exactly
          AboveHeight - HeightBetweenLegsAndMiddle,
          then you will get exatly into collision with the ground.
          So actually this is too large MaximumFallingDownDistance.

          But actually it's OK when Sphere is used, because then wall-sliding
          in MoveAllowed can correct new position,
          so actually it will be slightly above the ground. So falling
          down will work.

          But when Sphere is not used, the situation is worse,
          because then MoveAllowed doesn't do wall-sliding.
          And it will always simply reject such move
          with MaximumFallingDownDistance.
          If FPS is low (so we would like to fall down at once
          by large distance), this is noticeable: in such case, instead
          of falling down, creature hangs over the ground,
          because MoveAllowed simply doesn't allow it fall
          exactly by AboveHeight - HeightBetweenLegsAndMiddle.
          So MaximumFallingDownDistance has to be a little smaller in this case.
          In particular, this was noticeable for the initially dead alien
          creature on "Doom" level, when shadows were on (when shadows were on,
          FPS is low, that's why the bug was noticeable only with shadows = on).

          TODO: the better version would be to improve
          MoveAllowed for Sphere=false case, instead of
          workarounding it here with this epsilon.
          See TBaseTrianglesOctree.MoveAllowed. }
        if not Sphere(RadiusIgnored) then
          MaximumFallingDownDistance -= 0.01;
        MinTo1st(FallingDownDistance, MaximumFallingDownDistance);
      end;

      if not MyMove(Vector3Single(0, 0, -FallingDownDistance), true) then
        FIsFallingDown := false;
    end else
    begin
      FIsFallingDown := false;

      if AboveHeight < HeightBetweenLegsAndMiddle / HeightMargin then
      begin
        { Growing up }
        MyMove(Vector3Single(0, 0, Min(GrowingUpSpeed * CompSpeed,
          HeightBetweenLegsAndMiddle - AboveHeight)), false);
      end;
    end;

    if OldIsFallingDown and (not FIsFallingDown) then
      FalledDown;
  end;

begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  FLifeTime += CompSpeed;

  UpdateUsedSounds;

  if Kind.FallingDownSpeed <> 0.0 then
    DoGravity;
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

procedure TCreature.ShortRangeAttackHurt;
var
  Player: T3DAlive;
begin
  Player := World.Player as T3DAlive;
  if Player = nil then Exit; { no Player to hurt }

  Player.Hurt(Kind.ShortRangeAttackDamageConst +
    Random * Kind.ShortRangeAttackDamageRandom, Direction,
    Kind.ShortRangeAttackKnockbackDistance);
end;

function TCreature.Sphere(out Radius: Single): boolean;
begin
  Result := GetExists and (not Dead);
  Radius := Kind.Radius;
end;

function TCreature.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
const
  VisibleItemDistance = 60.0;
var
  S: string;
begin
  Result := Active;
  if not Result then Exit;

  if Distance <= VisibleItemDistance then
  begin
    S := Format('You see a creature "%s"', [Kind.Id]);

    if Life >= MaxLife then
      S += ' (not wounded)' else
    if Life >= MaxLife / 3 then
      S += ' (wounded)' else
    if Life > 0 then
      S += ' (very wounded)' else
      S += ' (dead)';

    Notifications.Show(S);
  end else
    Notifications.Show('You see some creature, but it''s too far to tell exactly what it is');
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
    if WAKind.Flying then
      SqrDistanceToTarget := PointsDistanceSqr(Middle, Target) else
      SqrDistanceToTarget := PointsDistanceXYSqr(Middle, Target);
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
  begin
    Distance := WAKind.RandomWalkDistance;

    AlternativeTarget := Middle;
    AlternativeTarget[0] += Random * Distance * 2 - Distance;
    AlternativeTarget[1] += Random * Distance * 2 - Distance;
    if WAKind.Flying then
      AlternativeTarget[2] += Random * Distance * 2 - Distance;

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
         BoundingBox.PointInsideXY(LastSeenPlayer) then
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
          MyHeight(NewMiddle, AboveHeight);
          if AboveHeight > WAKind.MaxHeightAcceptableToFall +
              HeightBetweenLegsAndMiddle then
            Result := true;
        end;
      end;

    begin
      Result :=
        { First check to not step into some deep fall.
          Note that I'm not using here NewMiddle
          (that will be calculated later by MyMove)
          because they are too close to Middle to be good to test against.
          I'm calculating here where I would get after 0.2 second. }
        (not TooHighAboveTheGround(Middle + Direction * (WAKind.MoveSpeed * 0.2))) and

        { Use MyMove without wall-sliding here.
          Things using MoveAlongTheDirection depend on the fact that
          MoveAlongTheDirection will return false
          if no further way is possible (and wall-sliding would try instead
          to return true and correct target position).

          Our trick with "AlternativeTarget" should handle
          eventual problems with the track of creature, so wall-sliding
          should not be needed. }
        MyMove(Direction * (WAKind.MoveSpeed * CompSpeed), false, false);
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
  IdleSeesPlayer := (Player <> nil) and MyLineOfSight(Middle, Player.Position);
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

  { self-shadows on creatures look bad, esp. see werewolves at the end
    of "castle hall" level. Changing XxxShadowVolumes here
    is a little hacky (would be cleaner to do it at loading), but easy. }
  TCastleScene(Result).ReceiveShadowVolumes := false;
  Result.CastShadowVolumes := Kind.CastShadowVolumes;
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
      Result := MyMoveAllowed(OldPos, NewPos, false);
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

  { Note that for now missiles are removed from level as soon as they are dead,
    so I don't bother with checking here Dead. }

  OldMiddle := Middle;
  NewMiddle := OldMiddle + Direction * (MissileKind.MoveSpeed * CompSpeed);

  { missile moves *always*, disregarding MissileMoveAllowed result.
    Only after move, if the move made us colliding with something --- we explode. }
  Position := PositionFromMiddle(NewMiddle);

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

  if MissileKind.FallingDownSpeed <> 0 then
  begin
    NewDirection := Direction;
    NewDirection[2] -= MissileKind.FallingDownSpeed * CompSpeed;
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
  { self-shadows on creatures look bad, esp. see werewolves at the end
    of "castle hall" level. Changing XxxShadowVolumes here
    is a little hacky (would be cleaner to do it at loading), but easy. }
  TCastleScene(Result).ReceiveShadowVolumes := false;
  Result.CastShadowVolumes := Kind.CastShadowVolumes;
end;

procedure TMissileCreature.ExplodeCore;
begin
  { TODO: for some missiles, their explosion may hurt everyone around.
    So do here additional checks for collision and hurt player and creatures. }

  { This sound is done using GameSound.Sound3d, not our Sound3d
    --- because when the creature will be destroyed (and missile will
    be destroyed in nearest RemoveFromLevel pass), we want this sound
    to go on. }
  SoundEngine.Sound3d(MissileKind.SoundExplosion, Position);

  Hurt(1000 * 1000, ZeroVector3Single, 0);
end;

procedure TMissileCreature.ExplodeWithPlayer;
begin
  ShortRangeAttackHurt;
end;

procedure TMissileCreature.ExplodeWithCreature(Creature: TCreature);
begin
  Creature.Hurt(Kind.ShortRangeAttackDamageConst +
    Random * Kind.ShortRangeAttackDamageRandom, Direction,
    Kind.ShortRangeAttackKnockbackDistance);
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
  { self-shadows on creatures look bad, esp. see werewolves at the end
    of "castle hall" level. Changing XxxShadowVolumes here
    is a little hacky (would be cleaner to do it at loading), but easy. }
  TCastleScene(Result).ReceiveShadowVolumes := false;
  Result.CastShadowVolumes := Kind.CastShadowVolumes;
end;

procedure TStillCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { TODO: do explosion anim for barrel. }
  if Life <= 0.0 then
    RemoveMe := rtRemoveAndFree;
end;

end.
