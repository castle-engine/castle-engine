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

{ Player (TPlayer). }
unit CastlePlayer
  deprecated 'define your own player class';

{$I castleconf.inc}

interface

uses Classes,
  CastleBoxes, CastleCameras, CastleItems, CastleVectors, CastleInputs,
  CastleKeysMouse, CastleShapes, CastleInternalMaterialProperties, CastleSoundEngine,
  CastleTransformExtra, CastleGLUtils, CastleColors, CastleFrustum, CastleTriangles,
  CastleTimeUtils, CastleScene, CastleDebugTransform, X3DNodes, CastleTransform,
  CastleResources, CastleThirdPersonNavigation;

type
  TPlayerSwimming = (
    { Not swimming. }
    psNo,

    { Swimming on the water surface.
      Conceptually, avatar is submerged in water, but has head above the water. }
    psAboveWater,

    { Swimming under the water surface.
      Conpeptually, avatar is fully submerged in water, and may have trouble
      breathing. Games may simulate lack of oxygen, drowning etc. in this case. }
    psUnderWater
  );

  { Player, 3D object controlling the camera, main enemy of hostile creatures,
    carries a backpack, may cause fadeout effects on screen and such.

    Note that you can operate on player even before level is loaded,
    before TLevel or TCastleViewport are initialized.
    This allows to create player before level is started
    (create it from scratch, or by loading from save game),
    and "carry" the same player instance across various loaded levels.

    @link(Dead) or @link(Blocked) player behaves much like alive and normal player.
    For example, it still has an associated @link(Navigation)
    that can be affected by physics
    (e.g. to apply physics to the dead player body,
    because player was killed when he was flying, or it's
    corpse lays on some moving object of the level --- like elevator).
    However, input shortcuts will be cleared, to prevent user from
    directly moving the player.

    Do not do some stuff when player is dead:
    @unorderedList(
      @item No calling PickItem, DropItem, UseItem.
      @item(No increasing Life (further decreasing Life is OK).
        This implies that once Player is Dead, (s)he cannot be alive again.)
      @item No changing EquippedWeapon, no calling Attack.
    )

    Note that a player has an associated and synchronized @link(Navigation)
    instance. See @link(UseThirdPerson).
  }
  TPlayer = class(TAliveWithInventory)
  private
    type
      { Invisible box, that is added to TPlayer to make it collidable.
        Owner must be TPlayer. }
      TBox = class(TCastleScene)
      strict private
        Box: TBoxNode;
        Shape: TShapeNode;
        TransformNode: TTransformNode;
        procedure UpdateBox;
      public
        constructor Create(AOwner: TComponent); override;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;

    var
      FBox: TBox;
      FDebugTransform: TDebugTransform;
      FRenderDebug: boolean;

      FEquippedWeapon: TItemWeapon;
      FEquippedWeaponResourceFrame: TResourceFrame;

      { If Swimming = psUnderWater, then this is the time (from LifeTime)
        of setting Swimming to psUnderWater. }
      SwimBeginTime: Single;
      { If Swimming = psUnderWater, this is the time of last
        drowning (or 0.0 if there was no drowning yet in this swimming session). }
      SwimLastDrownTime: Single;
      { If Swimming = psUnderWater, this is the time of last stPlayerSwimming sound
        (or 0.0 if there was no stPlayerSwimming played yet in this
        swimming session). }
      SwimLastSoundTime: Single;
      FSwimming: TPlayerSwimming;

      { Did last @link(Update) detected that we're on toxic ground? }
      IsToxic: boolean;
      { Relevant if IsToxic, this is LifeTime when
        last time toxic damage was done. When player steps on toxic for the
        first time, he immediately gets damage, so ToxicLastDamageTime is
        always valid when IsToxic. }
      ToxicLastDamageTime: Single;

      SwimmingChangePlayingSound: TCastlePlayingSound;
      SwimmingPlayingSound: TCastlePlayingSound;

      { Did last @link(Update) detected that we are on the ground. }
      IsOnTheGround: boolean;
      { <> @nil if IsOnTheGround and last ground had some TMaterialProperty. }
      GroundProperty: TMaterialProperty;
      ReallyIsOnTheGroundTime: Single;

      FootstepsPlayingSound: TCastlePlayingSound;
      FootstepsSound: TCastleSound;
      ReallyWalkingOnTheGroundTime: Single;

      FInventoryCurrentItem: Integer;
      FInventoryVisible: boolean;
      FSickProjectionSpeed: Single;
      FBlocked: boolean;

      FFlying: boolean;
      FFlyingTimeOut: TFloatTime;
      { FadeOut settings. }
      FFadeOutIntensity: Single;
      FFadeOutColor: TCastleColor;

      FFallMinHeightToSound: Single;
      FFallMinHeightToDamage: Single;
      FFallDamageScaleMin: Single;
      FFallDamageScaleMax: Single;
      FFallSound: TCastleSound;
      FHeadBobbing: Single;
      FSwimBreath: Single;
      FDrownPause: Single;
      FDrownDamageConst: Single;
      FDrownDamageRandom: Single;
      FSwimSoundPause: Single;
      FEnableNavigationDragging: boolean;
      FFallingEffect: boolean;
      FWalkNavigation: TCastleWalkNavigation;
      FThirdPersonNavigation: TCastleThirdPersonNavigation;
      FUseThirdPerson: Boolean;
      InsideSynchronizeFromCamera: Cardinal;

    procedure SetEquippedWeapon(Value: TItemWeapon);

    { Update Navigation and ThirdPersonNavigation properties, including inputs.
      Call this always when @link(Flying) or @link(Dead) or some key values
      or @link(Swimming) or @link(Blocked) change. }
    procedure UpdateNavigation;

    procedure NavigationFall(const Sender: TCastleNavigation; const FallHeight: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color).
      Color's alpha doesn't matter for now. }
    procedure SetLifeCustomFadeOut(const Value: Single;
      const Color: TCastleColor);

    procedure SetSwimming(const Value: TPlayerSwimming);

    procedure SetFlying(const AValue: Boolean);
    procedure SetFlyingTimeOut(const AValue: TFloatTime);
    procedure SetEnableNavigationDragging(const AValue: Boolean);
    procedure SynchronizeToCamera;
    procedure SynchronizeFromCamera;
    procedure SetUseThirdPerson(const AValue: Boolean);
    function GetRenderOnTop: Boolean;
    procedure SetRenderOnTop(const Value: Boolean);
  protected
    procedure SetLife(const Value: Single); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function LocalHeightCollision(const APosition, GravityUp: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: PTriangle): Boolean; override;
    function LocalSegmentCollision(const Pos1, Pos2: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const ALineOfSight: Boolean): Boolean; override;
    procedure Fall(const FallHeight: Single); override;
    procedure ChangedTransform; override;
  public
    var
      { Various navigation properties that may depend on loaded level. }
      DefaultMoveHorizontalSpeed: Single;
      DefaultMoveVerticalSpeed: Single;
      DefaultPreferredHeight: Single;

    const
      DefaultLife = 100;
      DefaultSickProjectionSpeed = 2.0;
      DefaultPlayerKnockBackSpeed = 20.0;
      DefaultSwimBreath = 30.0;
      DefaultDrownPause = 5.0;
      DefaultDrownDamageConst = 5.0;
      DefaultDrownDamageRandom = 10.0;
      DefaultSwimSoundPause = 3.11111111;
      { TPlayer.FallMinHeightToSound is larger than
        TCreature.FallMinHeightToSound,
        to avoid making "fall" sound when player merely jumps or walks down a steep
        hill. No such need for creature. }
      DefaultFallMinHeightToSound = 4.0;
      DefaultFallSoundName = 'player_fall';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render(const Params: TRenderParams); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    property ListenPressRelease default true;

    { Flying.
      How it interacts with FlyingTimeout: Setting this property
      to any value removes any timeout set by FlyingTimeout.
      That is, setting this to @true makes player fly indefinitely,
      and setting this to @false makes player stop flying (regardless
      if flying was initialized by @code(Flying := true) or @code(FlyingTimeout)). }
    property Flying: boolean read FFlying write SetFlying;

    { Set this to something > 0 to start flying for a given number of seconds.
      The @link(Flying) property will also change to @true for this time.
      It will automatically change back to @false after given number of seconds
      (you can also always just manually switch @link(Flying) back to @false).

      Set this only with value > 0.

      When this is > 0 it means flying with a timeout
      (always @link(Flying) = @true then),
      otherwise it's = 0 (which means were not flying, or flying indefinitely
      long, depending on @link(Flying)).}
    property FlyingTimeOut: TFloatTime read FFlyingTimeOut write SetFlyingTimeOut;

    { Add Item to inventory, updating player InventoryCurrentItem,
      making suitable notification and sound. }
    function PickItemUpdate(var Item: TInventoryItem): Integer; override;

    { Drop item from inventory, updating player InventoryCurrentItem,
      making suitable notification and sound.
      @groupBegin }
    function DropItem(const Index: Integer): TItemOnWorld; override;
    function DropCurrentItem: TItemOnWorld;
    { @groupEnd }

    { Use an item from inventory.
      You can pass Index that is out of range (or call UseCurrentItem
      when InventoryCurrentItem = -1), it will then show
      a notification (by CastleGameNotifications unit) that nothing is selected.
      @groupBegin }
    procedure UseItem(const Index: Integer); override;
    procedure UseCurrentItem;
    { @groupEnd }

    { Change InventoryCurrentItem, cycling, and automatically showing
      the inventory afterwards (if it's not empty).
      Note that you can also always directly change InventoryCurrentItem property. }
    procedure ChangeInventoryCurrentItem(Change: Integer);

    { Weapon the player is using right now, or nil if none.

      You can set this property only to some item existing on Inventory.
      When you drop the current weapon,
      DeleteItem will automatically set this to @nil.

      When setting this property (to nil or non-nil) player may get
      GameMessage about using/not using a weapon. }
    property EquippedWeapon: TItemWeapon read FEquippedWeapon write SetEquippedWeapon;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function Middle: TVector3; override;

    { Cause a fade-out effect on the screen, tinting the screen to the given Color.
      The TPlayer class doesn't do the actual drawing of the fade-out effect
      on the screen, we merely store and animate the FadeOutColor and FadeOutIntensity
      properties. To draw the effect, use a procedure like GLFadeRectangle
      inside your 2D controls drawing code, see engine tutorial for example. }
    procedure FadeOut(const Color: TCastleColor);

    property FadeOutColor: TCastleColor read FFadeOutColor;
    property FadeOutIntensity: Single read FFadeOutIntensity;

    { @noAutoLinkHere }
    procedure Attack; virtual;

    { TLevel sets this property automatically, based on the water volume.
      @exclude }
    property Swimming: TPlayerSwimming read FSwimming write SetSwimming;

    { Load various player properties from an XML file.
      Properties not specified in the indicated file will
      be reset to their default values.
      This is handy to use in a game to allow to configure player behavior
      by simply editing an XML file (instead of hacking code).

      Overloaded parameterless version reads from file
      @code('castle-data:/player.xml').

      Note that the indicated file may not exist, and it will not cause errors.
      Not existing file is equivalent to a file with everything set at default
      values.

      It is Ok to call this multiple times, at any moment.
      This way you can make some debug command to reload player.xml file,
      very useful to test various player properties without restarting the game.

      @groupBegin }
    procedure LoadFromFile; overload;
    procedure LoadFromFile(const URL: string); overload;
    { @groupEnd }

    function Ground: PTriangle;

    procedure LevelChanged;

    { Currently selected inventory item.

      Note: while we try to always sensibly update InventoryCurrentItem,
      to keep the assumptions that
      @orderedList(
        @item(Inventory.Count = 0 => InventoryCurrentItem = -1)
        @item(Inventory.Count > 0 =>
         InventoryCurrentItem between 0 and Inventory.Count - 1))

      but you should @italic(nowhere) depend on these assuptions.
      That's because I want to allow myself freedom to modify Inventory
      in various situations, so InventoryCurrentItem can become
      invalid in many situations.

      So every code should check that
      @unorderedList(
        @item(If InventoryCurrentItem between 0 and Inventory.Count - 1
          then InventoryCurrentItem is selected)
        @item(Else no item is selected (possibly Inventory.Count = 0,
          possibly not)))
    }
    property InventoryCurrentItem: Integer
      read FInventoryCurrentItem write FInventoryCurrentItem
      default -1;

    property InventoryVisible: boolean
      read FInventoryVisible write FInventoryVisible default false;

    property SickProjectionSpeed: Single
      read FSickProjectionSpeed write FSickProjectionSpeed
      {$ifdef FPC}default DefaultSickProjectionSpeed{$endif};

    property CollidesWithMoving default true;
    function Sphere(out Radius: Single): boolean; override;

    { Disables changing the camera by user.
      It's useful when you want to temporarily force camera to some specific
      setting (you can even use handy Player.Navigation.AnimateTo method
      to do this easily, see TCastleWalkNavigation.AnimateTo). }
    property Blocked: boolean read FBlocked write FBlocked;

    { Render 3D children (like EquippedWeapon) on top of everything else. }
    property RenderOnTop: boolean read GetRenderOnTop write SetRenderOnTop;

    property FallMinHeightToSound: Single
      read FFallMinHeightToSound write FFallMinHeightToSound
      {$ifdef FPC}default DefaultFallMinHeightToSound{$endif};
    property FallMinHeightToDamage: Single
      read FFallMinHeightToDamage write FFallMinHeightToDamage
      {$ifdef FPC}default DefaultFallMinHeightToDamage{$endif};
    property FallDamageScaleMin: Single
      read FFallDamageScaleMin write FFallDamageScaleMin
      {$ifdef FPC}default DefaultFallDamageScaleMin{$endif};
    property FallDamageScaleMax: Single
      read FFallDamageScaleMax write FFallDamageScaleMax
      {$ifdef FPC}default DefaultFallDamageScaleMax{$endif};
    { Sound when falling.
      The default is the sound named 'player_fall'. }
    property FallSound: TCastleSound
      read FFallSound write FFallSound;
    { Controls head bobbing, but only when player is walking.
      See TCastleWalkNavigation.HeadBobbing for exact meaning of this.
      TPlayer.Navigation.HeadBobbing is automatically updated as necessary.

      Note that when using CastleLevels, then the headBobbing defined
      inside VRML/X3D file (see
      https://castle-engine.io/x3d_extensions.php#section_ext_head_bobbing )
      is ignored. Instead, Player properties control TCastleWalkNavigation.HeadBobbing
      and TCastleWalkNavigation.HeadBobbingTime. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing
      {$ifdef FPC}default TCastleWalkNavigation.DefaultHeadBobbing{$endif};

    { How many seconds you can swin before you start to drown. }
    property SwimBreath: Single read FSwimBreath write FSwimBreath
      {$ifdef FPC}default DefaultSwimBreath{$endif};

    { How many seconds between each drown event.
      Drown event makes stPlayerDrowning sound and causes damage
      DrownDamageConst + Random * DrownDamageRandom. }
    property DrownPause: Single read FDrownPause write FDrownPause
      {$ifdef FPC}default DefaultDrownPause{$endif};
    property DrownDamageConst: Single read FDrownDamageConst write FDrownDamageConst
      {$ifdef FPC}default DefaultDrownDamageConst{$endif};
    property DrownDamageRandom: Single read FDrownDamageRandom write FDrownDamageRandom
      {$ifdef FPC}default DefaultDrownDamageRandom{$endif};

    { Pause, in seconds, between playing stPlayerSwimming sound.
      This should be something that is not easily synchronized
      with SwimDrownPause. }
    property SwimSoundPause: Single read FSwimSoundPause write FSwimSoundPause
      {$ifdef FPC}default DefaultSwimSoundPause{$endif};

    { Enable navigation falling down effect due to gravity.
      This indirectly controls @link(TCastleWalkNavigation.FallingEffect)
      underneath.

      Note: do not set @code(Navigation.FallingEffect), as it will
      be overridden in our update. Use only this property to turn on/off
      the effect. }
    property FallingEffect: boolean
      read FFallingEffect write FFallingEffect default true;

    { Navigation synchronized with this player instance.
      This is either @link(WalkNavigation) or @link(ThirdPersonNavigation),
      depending on @link(UseThirdPerson).

      You can use this navigation as @link(TCastleViewport.Navigation)
      to allow user to directly control this player in first-person game.
      @link(TLevel.Load) sets this automatically.

      The view vectors (position, direction and up), @link(TCastleWalkNavigation.Gravity),
      and various navigation inputs are automatically adjusted based on the current
      player state (@link(Dead), @link(Blocked)) and global PlayerInput_Xxx
      values, like @link(PlayerInput_Forward).
      The outside code may still directly access and change some navigation
      properties like TCastleWalkNavigation.PreferredHeight,
      TCastleWalkNavigation.RotationHorizontalSpeed
      TCastleWalkNavigation.RotationVerticalSpeed. In fact, it's Ok to call
      TCastleWalkNavigation.Init. }
    function Navigation: TCastleMouseLookNavigation;

    { Navigation synchronized with this player instance,
      when @link(UseThirdPerson) is @false. }
    property WalkNavigation: TCastleWalkNavigation read FWalkNavigation;
    {$ifdef FPC}
    property Camera: TCastleWalkNavigation read FWalkNavigation; deprecated 'use WalkNavigation';
    {$endif}

    { Navigation synchronized with this player instance,
      when @link(UseThirdPerson) is @true.

      The @link(TCastleThirdPersonNavigation.AvatarHierarchy) inside is initialized, don't touch it.

      The @link(TCastleThirdPersonNavigation.Avatar) inside is created,
      you can further configure it if you want to use @link(UseThirdPerson).
      Most of all, you can load some model there,
      like @code(ThirdPersonNavigation.Avatar.Load('castle-data:/avatar.gltf')).
    }
    property ThirdPersonNavigation: TCastleThirdPersonNavigation read FThirdPersonNavigation;

    { Whether to use 1st-person @link(WalkNavigation) or 3rd-person @link(ThirdPersonNavigation).
      @bold(You have to set this before calling @link(TLevel.Load)). }
    property UseThirdPerson: Boolean read FUseThirdPerson write SetUseThirdPerson default false;

    property KnockBackSpeed {$ifdef FPC}default DefaultPlayerKnockBackSpeed{$endif};

    { Enable navigation by dragging. This results in including
      niMouseDragging in TCastleNavigation.Input (when player is not
      @link(Dead) or @link(Blocked)). }
    property EnableNavigationDragging: boolean
      read FEnableNavigationDragging write SetEnableNavigationDragging default true;
    {$ifdef FPC}
    property EnableCameraDragging: boolean
      read FEnableNavigationDragging write SetEnableNavigationDragging default true;
      deprecated 'use EnableNavigationDragging';
    {$endif}

    { Show the debug bounding box of the player.
      Warning: It looks a little confusing (since it's a box around camera). }
    property RenderDebug: boolean
      read FRenderDebug write FRenderDebug default false;
  end;

const
  DefaultAutoOpenInventory = true;

var
  { Automatically open TCastlePlayer inventory when picking up an item. }
  AutoOpenInventory: boolean = DefaultAutoOpenInventory;

var
  { Player inputs that handle navigation.
    @groupBegin }
  PlayerInput_Forward: TInputShortcut;
  PlayerInput_Backward: TInputShortcut;
  PlayerInput_LeftRotate: TInputShortcut;
  PlayerInput_RightRotate: TInputShortcut;
  PlayerInput_LeftStrafe: TInputShortcut;
  PlayerInput_RightStrafe: TInputShortcut;
  PlayerInput_UpRotate: TInputShortcut;
  PlayerInput_DownRotate: TInputShortcut;
  PlayerInput_GravityUp: TInputShortcut;
  PlayerInput_Jump: TInputShortcut;
  PlayerInput_Crouch: TInputShortcut;
  { @groupEnd }

  { Player inputs that deal with items.
    @groupBegin }
  PlayerInput_Attack: TInputShortcut;
  PlayerInput_InventoryShow: TInputShortcut; //< No key/mouse associated by default.
  PlayerInput_InventoryPrevious: TInputShortcut;
  PlayerInput_InventoryNext: TInputShortcut;
  PlayerInput_UseItem: TInputShortcut;
  PlayerInput_DropItem: TInputShortcut; //< No key/mouse associated by default.
  PlayerInput_CancelFlying: TInputShortcut; //< No key/mouse associated by default.
  { @groupEnd }

implementation

uses Math, SysUtils,
  CastleClassUtils, CastleUtils, CastleControls,
  CastleImages, CastleFilesUtils, CastleUIControls, CastleLog,
  CastleGameNotifications, CastleXMLConfig,
  CastleGLImages, CastleConfig, CastleRenderContext, CastleRenderOptions;

{ TPlayer.TBox ----------------------------------------------------------------- }

constructor TPlayer.TBox.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
begin
  inherited;
  Box := TBoxNode.CreateWithTransform(Shape, TransformNode);
  Box.Size := Vector3(1, 1, 1); // this way Transform.Scale determines the size

  UpdateBox;

  Root := TX3DRootNode.Create;
  Root.AddChildren(TransformNode);
  Load(Root, true);

  Visible := false;
end;

procedure TPlayer.TBox.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  if Exists then
    UpdateBox;
  inherited;
end;

procedure TPlayer.TBox.UpdateBox;
var
  B: TBox3D;
  Navigation: TCastleWalkNavigation;
begin
  Assert(not TPlayer(Owner).UseThirdPerson, 'TPlayer.TBox should be used only for 1st person navigation');

  Navigation := TPlayer(Owner).WalkNavigation;

  B.Data[0].X := -Navigation.Radius;
  B.Data[0].Y := -Navigation.Radius;
  B.Data[0].Z := -Navigation.Radius;

  if World <> nil then
    B.Data[0].Data[World.GravityCoordinate] := -Navigation.PreferredHeight;

  B.Data[1].X := Navigation.Radius;
  B.Data[1].Y := Navigation.Radius;
  B.Data[1].Z := Navigation.Radius;

  { We adjust TransformNode.Scale, not Box.Size, because this is faster:
    no need to rebuild box proxy. }
  TransformNode.Scale := B.Size;
  TransformNode.Translation := B.Center;
end;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenPressRelease := true;
  CollidesWithMoving := true;
  Life := DefaultLife;
  MaxLife := DefaultLife;
  DefaultMoveHorizontalSpeed := 1.0;
  DefaultMoveVerticalSpeed := 1.0;
  DefaultPreferredHeight := 0.0;
  RenderLayer := rlFront; // to make RenderOnTop = true
  FFallMinHeightToSound := DefaultFallMinHeightToSound;
  FFallMinHeightToDamage := DefaultFallMinHeightToDamage;
  FFallDamageScaleMin := DefaultFallDamageScaleMin;
  FFallDamageScaleMax := DefaultFallDamageScaleMax;
  FFallSound := SoundEngine.SoundFromName(DefaultFallSoundName, false);
  KnockBackSpeed := DefaultPlayerKnockBackSpeed;
  FSickProjectionSpeed := DefaultSickProjectionSpeed;
  FSwimBreath := DefaultSwimBreath;
  FDrownPause := DefaultDrownPause;
  FDrownDamageConst := DefaultDrownDamageConst;
  FDrownDamageRandom := DefaultDrownDamageRandom;
  FSwimSoundPause := DefaultSwimSoundPause;
  FFallingEffect := true;
  FEnableNavigationDragging := true;
  FWalkNavigation := TCastleWalkNavigation.Create(nil);

  { Setup FThirdPersonNavigation and use Self as AvatarHierarchy }
  FThirdPersonNavigation := TCastleThirdPersonNavigation.Create(nil);
  FThirdPersonNavigation.AvatarHierarchy := Self;
  FThirdPersonNavigation.Avatar := TCastleScene.Create(Self);
  FThirdPersonNavigation.Avatar.PreciseCollisions := true;
  Add(FThirdPersonNavigation.Avatar);

  FInventoryCurrentItem := -1;

  { turn off keys that are totally unavailable for the player }
  WalkNavigation.Input_MoveSpeedInc.MakeClear;
  WalkNavigation.Input_MoveSpeedDec.MakeClear;
  WalkNavigation.Input_IncreasePreferredHeight.MakeClear;
  WalkNavigation.Input_DecreasePreferredHeight.MakeClear;

  WalkNavigation.CheckModsDown := false;
  WalkNavigation.OnFall := {$ifdef FPC}@{$endif}NavigationFall;

  { Although it will be called in every OnUpdate anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Navigation has already good values. }
  UpdateNavigation;

  // once Navigation is initialized, initialize TBox
  FBox := TBox.Create(Self);
  Add(FBox);

  FDebugTransform := TDebugTransform.Create(Self);
  FDebugTransform.Parent := Self;

  FEquippedWeaponResourceFrame := TResourceFrame.Create(Self);
  Add(FEquippedWeaponResourceFrame);

  { We allocate 1 TCastlePlayingSound instance for each of these sounds,
    which reflects the usage: at most 1 footsteps sound is played at a given time,
    at most 1 swimming sound etc. }

  FootstepsPlayingSound := TCastlePlayingSound.Create(nil);
  FootstepsPlayingSound.Loop := true;
  //FootstepsPlayingSound.Sound := ... // will be assigned based on ground type

  SwimmingChangePlayingSound := TCastlePlayingSound.Create(nil);
  {$warnings off} // just to keep deprecated working
  SwimmingChangePlayingSound.Sound := stPlayerSwimmingChange;
  {$warnings on}

  SwimmingPlayingSound := TCastlePlayingSound.Create(nil);
  {$warnings off} // just to keep deprecated working
  SwimmingPlayingSound.Sound := stPlayerSwimming;
  {$warnings on}
end;

destructor TPlayer.Destroy;
begin
  EquippedWeapon := nil; { unregister free notification }

  FreeAndNil(FootstepsPlayingSound);
  FreeAndNil(SwimmingChangePlayingSound);
  FreeAndNil(SwimmingPlayingSound);
  FreeAndNil(FWalkNavigation);
  FreeAndNil(FThirdPersonNavigation);
  inherited;
end;

function TPlayer.Navigation: TCastleMouseLookNavigation;
begin
  if UseThirdPerson then
    Result := ThirdPersonNavigation
  else
    Result := WalkNavigation;
end;

procedure TPlayer.SetFlying(const AValue: boolean);
begin
  FFlyingTimeOut := 0;
  FFlying := AValue;
end;

procedure TPlayer.SetFlyingTimeOut(const AValue: TFloatTime);
begin
  Assert(AValue > 0, 'Only call FlyingTimeOut with TimeOut > 0');
  { It's possible that this is called when timeout is already > 0.
    In this case, we set FFlyingTimeOut to maximum
    --- the effect that will allow player to fly longer wins. }
  FFlyingTimeOut := Max(FFlyingTimeOut, AValue);
  FFlying := true;
end;

function TPlayer.PickItemUpdate(var Item: TInventoryItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Resource.Caption]);
  if Item.Quantity <> 1 then
    S := S + Format(' (quantity %d)', [Item.Quantity]);
  Notifications.Show(S);

  {$warnings off} // just to keep deprecated working
  SoundEngine.Play(stPlayerPickItem);
  {$warnings on}

  Result := inherited PickItemUpdate(Item);

  { Automatically equip the weapon. }
  if (Item is TItemWeapon) and (EquippedWeapon = nil) then
    EquippedWeapon := TItemWeapon(Item);

  { Update InventoryCurrentItem. }
  if not Between(InventoryCurrentItem, 0, Inventory.Count - 1) then
    InventoryCurrentItem := Result;

  if AutoOpenInventory then
    InventoryVisible := true;
end;

function TPlayer.DropItem(const Index: Integer): TItemOnWorld;
var
  S: string;
begin
  Result := inherited DropItem(Index);

  if Result <> nil then
  begin
    if Result.Item = EquippedWeapon then
      EquippedWeapon := nil;

    S := Format('You drop "%s"', [Result.Item.Resource.Caption]);
    if Result.Item.Quantity <> 1 then
      S := S + Format(' (quantity %d)', [Result.Item.Quantity]);
    Notifications.Show(S);

    {$warnings off} // just to keep deprecated working
    SoundEngine.Play(stPlayerDropItem);
    {$warnings on}

    { update InventoryCurrentItem.
      Note that if Inventory.Count = 0 now, then this will
      correctly set InventoryCurrentItem to -1. }
    if InventoryCurrentItem >= Inventory.Count then
      InventoryCurrentItem := Inventory.Count - 1;
  end else
  begin
    { Dropping item failed. }
    if Between(Index, 0, Inventory.Count - 1) then
      Notifications.Show('Not enough room here to drop this item') else
      Notifications.Show('Nothing to drop - select some item first');
  end;
end;

function TPlayer.DropCurrentItem: TItemOnWorld;
begin
  Result := DropItem(InventoryCurrentItem);
end;

procedure TPlayer.UseItem(const Index: Integer);
begin
  if Between(Index, 0, Inventory.Count - 1) then
  begin
    inherited UseItem(Index);
    { update InventoryCurrentItem, because item potentially disappeared.
      Note that if Inventory.Count = 0 now, then this will
      correctly set InventoryCurrentItem to -1. }
    if InventoryCurrentItem >= Inventory.Count then
      InventoryCurrentItem := Inventory.Count - 1;
  end else
    Notifications.Show('Nothing to use - select some item first');
end;

procedure TPlayer.UseCurrentItem;
begin
  UseItem(InventoryCurrentItem);
end;

procedure TPlayer.ChangeInventoryCurrentItem(Change: Integer);
begin
  if Inventory.Count = 0 then
    InventoryCurrentItem := -1 else
  if InventoryCurrentItem >= Inventory.Count then
    InventoryCurrentItem := Inventory.Count - 1 else
  if InventoryCurrentItem < 0 then
    InventoryCurrentItem := 0 else
    InventoryCurrentItem := ChangeIntCycle(
      InventoryCurrentItem, Change, Inventory.Count - 1);

  if Inventory.Count <> 0 then
    InventoryVisible := true;
end;

procedure TPlayer.SetEquippedWeapon(Value: TItemWeapon);
begin
  if Value <> FEquippedWeapon then
  begin
    if FEquippedWeapon <> nil then
      FEquippedWeapon.RemoveFreeNotification(Self);

    FEquippedWeapon := Value;

    if FEquippedWeapon <> nil then
    begin
      Notifications.Show(Format('You''re using weapon "%s" now',
        [EquippedWeapon.Resource.Caption]));
      FEquippedWeapon.Equip;
      FEquippedWeapon.FreeNotification(Self);
    end else
    { This may be causes by EquippedWeapon := nil in destructor,
      do not make any notification in this case. }
    if not (csDestroying in ComponentState) then
      Notifications.Show('You''re no longer using your weapon');
  end;
end;

procedure TPlayer.SynchronizeToCamera;
var
  P, D, U: TVector3;
begin
  // avoid recursive calls between SynchronizeToCamera and SynchronizeFromCamera
  if InsideSynchronizeFromCamera <> 0 then Exit;

  // synchronize Position, Direction, Up *to* Navigation
  if not UseThirdPerson then
  try
    GetView(P, D, U);
    if Parent <> nil then
    begin
      P := Parent.LocalToWorld(P);
      D := Parent.LocalToWorldDirection(D);
      U := Parent.LocalToWorldDirection(U);
    end;
    Navigation.Camera.SetView(P, D, U);
  except
    on EViewportNotAssigned do
      WritelnWarning('Changing TCastlePlayer transformation (like position) before the TCastlePlayer.Navigation is assigned as TCastleViewport.Navigation is ignored');
  end;
end;

procedure TPlayer.SynchronizeFromCamera;
var
  P, D, U: TVector3;
begin
  // synchronize Position, Direction, Up *from* Navigation
  if not UseThirdPerson then
  try
    Navigation.Camera.GetView(P, D, U);
    if Parent <> nil then
    begin
      P := Parent.WorldToLocal(P);
      D := Parent.WorldToLocalDirection(D);
      U := Parent.WorldToLocalDirection(U);
    end;
    Inc(InsideSynchronizeFromCamera);
    SetView(P, D, U);
    Dec(InsideSynchronizeFromCamera);
  except
    on EViewportNotAssigned do
      WritelnWarning('Adjusting TCastlePlayer transformation (like position) in PrepareResources is aborted if TCastleViewport.Navigation is not yet associated with TCastlePlayer.Navigation');
  end;
end;

procedure TPlayer.ChangedTransform;
begin
  inherited;
  { If something called Player.Translation := xxx, update Navigation }
  SynchronizeToCamera;
end;

procedure TPlayer.UpdateNavigation;
var
  NormalNavigationInput: TNavigationInputs;
begin
  WalkNavigation.Gravity := (not Blocked) and (not Flying);
  { Note that when not Navigation.Gravity then FallingEffect will not
    work anyway. }
  WalkNavigation.FallingEffect := FallingEffect and (FSwimming = psNo);

  if Blocked then
  begin
    { When Blocked, we navigate navigation by code. }
    WalkNavigation.Input := [];
  end else
  begin
    NormalNavigationInput := [niNormal, ni3dMouse];
    if EnableNavigationDragging and not Dead then
      Include(NormalNavigationInput, niMouseDragging);

    WalkNavigation.Input := NormalNavigationInput;

    { Rotation keys work always, even when player is dead.
      Initially I disabled them, but after some thought:
      let them work. They work a little strangely (because Up
      is orthogonal to GravityUp), but they still work and player
      can figure it out. }
    WalkNavigation.Input_LeftRotate.Assign(PlayerInput_LeftRotate, false);
    WalkNavigation.Input_RightRotate.Assign(PlayerInput_RightRotate, false);
    WalkNavigation.Input_UpRotate.Assign(PlayerInput_UpRotate, false);
    WalkNavigation.Input_DownRotate.Assign(PlayerInput_DownRotate, false);
    WalkNavigation.Input_GravityUp.Assign(PlayerInput_GravityUp, false);
  end;

  if Blocked then
  begin
    { PreferGravityUpXxx should be ignored actually, because rotations
      don't work now. }
    WalkNavigation.PreferGravityUpForMoving := true;
    WalkNavigation.PreferGravityUpForRotations := false;

    { No need to do MakeClear now on any inputs, as we already set
      Input := []. }

    WalkNavigation.FallSpeedStart := TCastleWalkNavigation.DefaultFallSpeedStart;
    WalkNavigation.FallSpeedIncrease := TCastleWalkNavigation.DefaultFallSpeedIncrease;
    WalkNavigation.HeadBobbing := 0.0;
    WalkNavigation.PreferredHeight := WalkNavigation.Radius * 1.01;

    WalkNavigation.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
    WalkNavigation.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
  end else
  if Dead then
  begin
    WalkNavigation.PreferGravityUpForMoving := true;
    { This is the only case when PreferGravityUpForRotations := false
      is sensible. }
    WalkNavigation.PreferGravityUpForRotations := false;

    WalkNavigation.Input_Jump.MakeClear;
    WalkNavigation.Input_Crouch.MakeClear;

    WalkNavigation.Input_Forward.MakeClear;
    WalkNavigation.Input_Backward.MakeClear;
    WalkNavigation.Input_LeftStrafe.MakeClear;
    WalkNavigation.Input_RightStrafe.MakeClear;

    WalkNavigation.FallSpeedStart := TCastleWalkNavigation.DefaultFallSpeedStart;
    WalkNavigation.FallSpeedIncrease := TCastleWalkNavigation.DefaultFallSpeedIncrease;
    WalkNavigation.HeadBobbing := 0.0;
    WalkNavigation.PreferredHeight := WalkNavigation.Radius * 1.01;

    WalkNavigation.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
    WalkNavigation.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
  end else
  begin
    if Flying then
    begin
      WalkNavigation.PreferGravityUpForMoving := false;
      WalkNavigation.PreferGravityUpForRotations := true;

      { WalkNavigation.HeadBobbing and
        WalkNavigation.PreferredHeight and
        WalkNavigation.FallSpeedStart and
        WalkNavigation.FallSpeedIncrease
        ... don't matter here, because Gravity is false. }

      WalkNavigation.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
      WalkNavigation.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
    end else
    if FSwimming <> psNo then
    begin
      WalkNavigation.PreferGravityUpForMoving := false;
      WalkNavigation.PreferGravityUpForRotations := true;

      WalkNavigation.FallSpeedStart := TCastleWalkNavigation.DefaultFallSpeedStart / 6;
      WalkNavigation.FallSpeedIncrease := 1.0;
      WalkNavigation.HeadBobbing := 0.0;
      WalkNavigation.PreferredHeight := WalkNavigation.Radius * 1.01;

      WalkNavigation.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed / 2;
      WalkNavigation.MoveVerticalSpeed := DefaultMoveVerticalSpeed / 2;
    end else
    begin
      WalkNavigation.PreferGravityUpForMoving := true;
      WalkNavigation.PreferGravityUpForRotations := true;

      WalkNavigation.FallSpeedStart := TCastleWalkNavigation.DefaultFallSpeedStart;
      WalkNavigation.FallSpeedIncrease := TCastleWalkNavigation.DefaultFallSpeedIncrease;
      WalkNavigation.HeadBobbing := HeadBobbing;
      WalkNavigation.PreferredHeight := DefaultPreferredHeight;

      WalkNavigation.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
      WalkNavigation.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
    end;

    WalkNavigation.Input_Jump.Assign(PlayerInput_Jump, false);
    WalkNavigation.Input_Crouch.Assign(PlayerInput_Crouch, false);
    WalkNavigation.Input_Forward.Assign(PlayerInput_Forward, false);
    WalkNavigation.Input_Backward.Assign(PlayerInput_Backward, false);
    WalkNavigation.Input_LeftStrafe.Assign(PlayerInput_LeftStrafe, false);
    WalkNavigation.Input_RightStrafe.Assign(PlayerInput_RightStrafe, false);
  end;
end;

procedure TPlayer.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  { Perform various things related to player swimming. }
  procedure UpdateSwimming;
  begin
    { Swimming possibly was changed by TLevel update }
    if FSwimming = psUnderWater then
    begin
      { Take care of drowning. }
      if not Dead then
      begin
        if LifeTime - SwimBeginTime > SwimBreath then
        begin
          if (SwimLastDrownTime = 0.0) or
             (LifeTime - SwimLastDrownTime > DrownPause) then
          begin
            if SwimLastDrownTime = 0.0 then
              Notifications.Show('You''re drowning');
            SwimLastDrownTime := LifeTime;
            Life := Life - (DrownDamageConst + Random * DrownDamageRandom);
            {$warnings off} // just to keep deprecated working
            SoundEngine.Play(stPlayerDrowning);
            {$warnings on}
          end;
        end;
      end;

      { Take care of playing stPlayerSwimming }
      if (not SwimmingPlayingSound.Playing) and
         ( (SwimLastSoundTime = 0.0) or
           (LifeTime - SwimLastSoundTime > SwimSoundPause) ) then
      begin
        SwimLastSoundTime := LifeTime;
        SoundEngine.Play(SwimmingPlayingSound);
      end;
    end;
  end;

  { Update IsOnTheGround and related variables. }
  procedure UpdateIsOnTheGround;
  const
    { TimeToChangeOnTheGround and ReallyIsOnTheGroundTime play here the
      analogous role as ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSound, see UpdateFootstepsSound. }
    TimeToChangeIsOnTheGround = 0.5;
  begin
    if WalkNavigation.IsOnTheGround then
    begin
      ReallyIsOnTheGroundTime := LifeTime;
      IsOnTheGround := true;
      if Ground <> nil then
        GroundProperty := Ground^.Shape.InternalMaterialProperty
      else
        GroundProperty := nil;
    end else
    if LifeTime - ReallyIsOnTheGroundTime > TimeToChangeIsOnTheGround then
    begin
      GroundProperty := nil;
      IsOnTheGround := false;
    end; { else leave GroundProperty and IsOnTheGround unchanged. }
  end;

  { Update IsToxic and related variables, hurt player if on toxic.
    Must be called after UpdateIsOnTheGround (depends on GroundProperty). }
  procedure UpdateToxic;
  var
    NewIsToxic: boolean;
  begin
    {$warnings off} // using deprecated material props in deprecated CastlePlayer
    NewIsToxic := (GroundProperty <> nil) and GroundProperty.Toxic;
    if NewIsToxic then
    begin
      if (not IsToxic) or
         (LifeTime - ToxicLastDamageTime > GroundProperty.ToxicDamageTime) then
      begin
        ToxicLastDamageTime := LifeTime;
        if not Dead then
        begin
          SoundEngine.Play(stPlayerToxicPain);
          SetLifeCustomFadeOut(Life - (GroundProperty.ToxicDamageConst +
            Random * GroundProperty.ToxicDamageRandom), Green);
        end;
      end;
    end;
    IsToxic := NewIsToxic;
    {$warnings on}
  end;

  { Update FootstepsSound and related variables.
    Must be called after UpdateIsOnTheGround (depends on GroundProperty). }
  procedure UpdateFootstepsSound;
  const
    TimeToChangeFootstepsSound = 0.5;
  var
    NewFootstepsSound: TCastleSound;
  begin
    { The meaning of ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSound:
      WalkNavigation.IsWalkingOnTheGround can change quite rapidly
      (when player quickly presses and releases up/down keys,
      or when he're walking up the stairs, or when he's walking
      on un-flat terrain --- then WalkNavigation.IsWalkingOnTheGround
      switches between @true and @false quite often).
      But it is undesirable to change FootstepsSound
      so often, as this causes footsteps to suddenly stop, then play,
      then stop again etc. --- this doesn't sound good.

      So I use ReallyWalkingOnTheGroundTime to mark myself
      the time when WalkNavigation.IsWalkingOnTheGround was true.
      In normal situation I would set NewFootstepsSound to nil
      when WalkNavigation.IsWalkingOnTheGround = @false.
      But now I set NewFootstepsSound to nil only if
      WalkNavigation.IsWalkingOnTheGround = @false
      for at least TimeToChangeFootstepsSound seconds. }

    { calculate NewFootstepsSound }
    if WalkNavigation.IsWalkingOnTheGround then
    begin
      ReallyWalkingOnTheGroundTime := LifeTime;
      { Since WalkNavigation.IsWalkingOnTheGroundm then for sure
        WalkNavigation.IsOnTheGround, so UpdateIsOnTheGround updated
        GroundProperty field. }
      {$warnings off} // just to keep deprecated working
      if (GroundProperty <> nil) and
         (GroundProperty.FootstepsSound <> nil) then
        NewFootstepsSound := GroundProperty.FootstepsSound
      else
        NewFootstepsSound := stPlayerFootstepsDefault;
      {$warnings on}
    end else
    if LifeTime - ReallyWalkingOnTheGroundTime >
      TimeToChangeFootstepsSound then
      NewFootstepsSound := nil
    else
      NewFootstepsSound := FootstepsSound;

    if FootstepsSound <> NewFootstepsSound then
    begin
      FootstepsPlayingSound.Stop;
      FootstepsPlayingSound.Sound := NewFootstepsSound;
      if NewFootstepsSound <> nil then
        SoundEngine.Play(FootstepsPlayingSound);
      FootstepsSound := NewFootstepsSound;
    end;
  end;

const
  FadeOutSpeed = 2.0;
begin
  inherited;
  if not Exists then Exit;

  SynchronizeFromCamera;

  if FFlyingTimeOut > 0 then
  begin
    FFlyingTimeOut := FFlyingTimeOut - SecondsPassed;
    if FFlyingTimeOut <= 0 then
    begin
      FFlyingTimeOut := 0;
      FFlying := false;
    end;
  end;

  UpdateNavigation;

  UpdateSwimming;

  if FFadeOutIntensity > 0 then
    FFadeOutIntensity := FFadeOutIntensity - FadeOutSpeed * SecondsPassed;

  if (EquippedWeapon <> nil) and
     (InternalLevel <> nil) then
  begin
    FEquippedWeaponResourceFrame.Exists := true;
    EquippedWeapon.EquippedUpdate(InternalLevel, SecondsPassed, FEquippedWeaponResourceFrame);
  end else
    FEquippedWeaponResourceFrame.Exists := false;

  UpdateIsOnTheGround;
  UpdateToxic;
  UpdateFootstepsSound;

  FDebugTransform.Exists := RenderDebug;
end;

procedure TPlayer.FadeOut(const Color: TCastleColor);
begin
  FFadeOutColor := Color;
  FFadeOutIntensity := 1;
end;

procedure TPlayer.NavigationFall(const Sender: TCastleNavigation; const FallHeight: Single);
begin
  Fall(FallHeight);
end;

procedure TPlayer.Fall(const FallHeight: Single);
begin
  inherited;

  if (FSwimming = psNo) and
     (FallHeight > FallMinHeightToSound) then
    SoundEngine.Play(FallSound);

  if (FSwimming = psNo) and (FallHeight > FallMinHeightToDamage) then
    Life := Life - Max(0, FallHeight *
      MapRange(Random, 0.0, 1.0, FallDamageScaleMin, FallDamageScaleMax));
end;

procedure TPlayer.SetLifeCustomFadeOut(const Value: Single;
  const Color: TCastleColor);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    Notifications.Show('You die');
    {$warnings off} // just to keep deprecated working
    SoundEngine.Play(stPlayerDies);
    {$warnings on}
    WalkNavigation.FallOnTheGround;
  end else
  if (Life - Value) > 1 then
  begin
    FadeOut(Color);
    {$warnings off} // just to keep deprecated working
    SoundEngine.Play(stPlayerSuddenPain);
    {$warnings on}
  end;
  inherited SetLife(Value);
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  SetLifeCustomFadeOut(Value, Red);
  { cancel flying when dead }
  if Dead then
    Flying := false;
end;

procedure TPlayer.Attack;
begin
  if (EquippedWeapon <> nil) and
     (InternalLevel <> nil) then
    EquippedWeapon.EquippedAttack(InternalLevel)
  else
    { Cannot attack without weapon equipped.
      If the game will want to have some "always owned weapon"
      (like footkick in Duke, crowbar in HalfLife)
      that game will have to assign it to EquippedWeapon. }
    Notifications.Show('No weapon equipped');
end;

procedure TPlayer.SetSwimming(const Value: TPlayerSwimming);
begin
  if Value <> FSwimming then
  begin
    { If "Swimming = psUnderWater" state changed then play a sound. }
    if (FSwimming = psUnderWater) <>
       (Value = psUnderWater) then
    begin
      if not SwimmingChangePlayingSound.Playing then
        SoundEngine.Play(SwimmingChangePlayingSound); // play once
    end;

    if (FSwimming = psNo) and (Value <> psNo) then
    begin
      { Cancel falling down, otherwise he will fall down into the water
        with the high speed (because in the air FallSpeedStart
        is high and it's increased, but in the water it's much lower
        and not increased at all right now). }
      WalkNavigation.CancelFalling;
    end;

    FSwimming := Value;

    if FSwimming = psUnderWater then
    begin
      SwimBeginTime := LifeTime;
      SwimLastDrownTime := 0.0;
      SwimLastSoundTime := 0.0;
    end;

    { Although UpdateNavigation will be called in nearest Player.Update anyway,
      I want to call it *now*. That's because I want to set
      Navigation.FallSpeedStart to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Navigation.CancelFalling above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFalling ! So next falling down definitely *can*
      happen before next Player.Update. Actually we may be in the middle
      of falling down right now. Fortunately Navigation.Update
      and Navigation.CancelFalling are implemented (or rather fixed :)
      to honour calling CancelFalling and setting FallSpeedStart now.

      So the safeguard below is needed. }
    UpdateNavigation;
  end;
end;

procedure TPlayer.LoadFromFile(const URL: string);
var
  Config: TCastleConfig;
begin
  Config := TCastleConfig.Create(nil);
  try
    Config.RootName := 'player';
    Config.NotModified; { otherwise changing RootName makes it modified, and saved back at freeing }
    Config.URL := URL;

    KnockBackSpeed := Config.GetFloat('knockback_speed', DefaultPlayerKnockBackSpeed);
    WalkNavigation.JumpMaxHeight := Config.GetFloat('jump/max_height', TCastleWalkNavigation.DefaultJumpMaxHeight);
    WalkNavigation.JumpHorizontalSpeedMultiply := Config.GetFloat('jump/horizontal_speed_multiply', TCastleWalkNavigation.DefaultJumpHorizontalSpeedMultiply);
    WalkNavigation.JumpTime := Config.GetFloat('jump/time', TCastleWalkNavigation.DefaultJumpTime);
    WalkNavigation.HeadBobbingTime := Config.GetFloat('head_bobbing_time', TCastleWalkNavigation.DefaultHeadBobbingTime);
    HeadBobbing := Config.GetFloat('head_bobbing', TCastleWalkNavigation.DefaultHeadBobbing);
    SickProjectionSpeed := Config.GetFloat('sick_projection_speed', DefaultSickProjectionSpeed);
    FallMinHeightToSound := Config.GetFloat('fall/sound/min_height', DefaultFallMinHeightToSound);
    FallMinHeightToDamage := Config.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
    FallDamageScaleMin := Config.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
    FallDamageScaleMax := Config.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);
    {$warnings off} // using deprecated SoundFromName in deprecated
    FallSound := SoundEngine.SoundFromName(Config.GetValue('fall/sound/name', DefaultFallSoundName), false);
    {$warnings on}
    FSwimBreath := Config.GetFloat('swim/breath', DefaultSwimBreath);
    FSwimSoundPause := Config.GetFloat('swim/sound_pause', DefaultSwimSoundPause);
    FDrownPause := Config.GetFloat('drown/pause', DefaultDrownPause);
    FDrownDamageConst := Config.GetFloat('drown/damage/const', DefaultDrownDamageConst);
    FDrownDamageRandom := Config.GetFloat('drown/damage/random', DefaultDrownDamageRandom);
  finally FreeAndNil(Config); end;
end;

procedure TPlayer.LoadFromFile;
begin
  LoadFromFile('castle-data:/player.xml');
end;

procedure TPlayer.LevelChanged;
begin
  { Without this, ReallyWalkingOnTheGroundTime could pretend that
    player is walking on the ground, while in fact the player is just
    standing still after new level loaded. }
  ReallyWalkingOnTheGroundTime := -1000.0;

  FootstepsPlayingSound.Stop;
  FootstepsPlayingSound.Sound := nil;
  FootstepsSound := nil;

  ReallyIsOnTheGroundTime := -1000;
  IsOnTheGround := false;
  GroundProperty := nil;

  IsToxic := false;
end;

function TPlayer.Ground: PTriangle;
begin
  if UseThirdPerson then
    Result := nil
  else
    Result := WalkNavigation.AboveGround;
end;

function TPlayer.LocalSegmentCollision(const Pos1, Pos2: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  if ALineOfSight then
    { Player box is collidable (creatures cannot enter on player),
      but is not visible, so ALineOfSight ignores it.
      This allows creatures to see player's middle point. }
    Result := false
  else
    Result := inherited;
end;

function TPlayer.Sphere(out Radius: Single): boolean;
begin
  { We need to use Sphere for Player for collisions and gravity to make sense.
    So we always return true.
    When CollisionSphereRadius <> 0 we return it (just like inherited),
    otherwise we use Navigation.Radius. }
  if CollisionSphereRadius <> 0 then
    Radius := CollisionSphereRadius
  else
    Radius := Navigation.Radius;
  Result := true;
end;

procedure TPlayer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FEquippedWeapon) then
    FEquippedWeapon := nil;
end;

function TPlayer.LocalHeightCollision(const APosition, GravityUp: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
begin
  { instead of allowing inherited to do the work (and allow other stuff
    like items and creatures to stand on player's head), for now just
    make player non-collidable for Height. Otherwise, when trying really
    hard to walk "into" a creature, sometimes the creature may start to
    raise up and stand on your own (player's) head.
    This doesn't have any adverse effects for castle, after all
    items can fall down through us (they are immediately picked up then),
    and creature never fall down because of gravity on our head. }

  Result := false;
  AboveHeight := MaxSingle;
  AboveGround := nil;
end;

function TPlayer.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if not (Blocked or Dead) then
  begin
    if PlayerInput_Attack.IsEvent(Event) then
    begin
      Attack;
      Exit(true);
    end;

    if PlayerInput_CancelFlying.IsEvent(Event) then
    begin
      Flying := false;
      Exit(true);
    end;

    if PlayerInput_InventoryShow.IsEvent(Event) then
    begin
      InventoryVisible := not InventoryVisible;
      Exit(true);
    end;

    if PlayerInput_InventoryPrevious.IsEvent(Event) then
    begin
      ChangeInventoryCurrentItem(-1);
      Exit(true);
    end;

    if PlayerInput_InventoryNext.IsEvent(Event) then
    begin
      ChangeInventoryCurrentItem(+1);
      Exit(true);
    end;

    if PlayerInput_DropItem.IsEvent(Event) then
    begin
      DropCurrentItem;
      Exit(true);
    end;

   if PlayerInput_UseItem.IsEvent(Event) then
    begin
      UseCurrentItem;
      Exit(true);
    end;
  end;
end;

procedure TPlayer.Render(const Params: TRenderParams);
begin
  { Do this before rendering, otherwise we could display weapon in unsynchronized
    position/orientation.
    That's because Navigation could be changed after our Update,
    but before rendering.
    (Testcase: move/rotate using touch control
    in fps_game when you have shooting_eye.) }
  SynchronizeFromCamera;

  inherited;
end;

function TPlayer.Middle: TVector3;
begin
  if UseThirdPerson then
    Result := inherited
  else
  begin
    { For 1st-person player, our Translation is already the suitable "eye position"
      above the ground.

      Note that Player.Gravity remains false for now (only Player.Navigation.Gravity
      is true), so the player is not affected by simple gravity implemented in
      CastleTransform unit, so there's no point in overriding methods like PreferredHeight.
      TCastleWalkNavigation.Gravity does all the work now. }

    Result := Translation;
  end;
end;

procedure TPlayer.SetEnableNavigationDragging(const AValue: Boolean);
begin
  if FEnableNavigationDragging <> AValue then
  begin
    FEnableNavigationDragging := AValue;
    UpdateNavigation;
  end;
end;

procedure TPlayer.SetUseThirdPerson(const AValue: Boolean);
begin
  if World <> nil then
    raise Exception.Create('TODO: For now you cannot change TPlayer.UseThirdPerson once the TLevel (that refers to this TPlayer) has been Loaded');
  FUseThirdPerson := AValue;
  FBox.Exists := not UseThirdPerson;
end;

function TPlayer.GetRenderOnTop: Boolean;
begin
  Result := RenderLayer = rlFront;
end;

procedure TPlayer.SetRenderOnTop(const Value: Boolean);
begin
  if Value then
    RenderLayer := rlFront
  else
    RenderLayer := rlParent;
end;

initialization
  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  PlayerInput_Forward := TInputShortcut.Create(nil, 'Move forward', 'move_forward', igBasic);
  PlayerInput_Forward.Assign(keyW, keyArrowUp);
  PlayerInput_Backward := TInputShortcut.Create(nil, 'Move backward', 'move_backward', igBasic);
  PlayerInput_Backward.Assign(keyS, keyArrowDown);
  PlayerInput_LeftRotate := TInputShortcut.Create(nil, 'Turn left', 'turn_left', igBasic);
  PlayerInput_LeftRotate.Assign(keyArrowLeft);
  PlayerInput_RightRotate := TInputShortcut.Create(nil, 'Turn right', 'turn_right', igBasic);
  PlayerInput_RightRotate.Assign(keyArrowRight);
  PlayerInput_LeftStrafe := TInputShortcut.Create(nil, 'Move left', 'move_left', igBasic);
  PlayerInput_LeftStrafe.Assign(keyA);
  PlayerInput_RightStrafe := TInputShortcut.Create(nil, 'Move right', 'move_right', igBasic);
  PlayerInput_RightStrafe.Assign(keyD);
  PlayerInput_UpRotate := TInputShortcut.Create(nil, 'Look up', 'look_up', igBasic);
  PlayerInput_UpRotate.Assign(keyNone);
  PlayerInput_DownRotate := TInputShortcut.Create(nil, 'Look down', 'look_down', igBasic);
  PlayerInput_DownRotate.Assign(keyNone);
  PlayerInput_GravityUp := TInputShortcut.Create(nil, 'Look straight', 'look_straight', igBasic);
  PlayerInput_GravityUp.Assign(keyNone);
  PlayerInput_Jump := TInputShortcut.Create(nil, 'Jump (or fly/swim up)', 'move_up', igBasic);
  PlayerInput_Jump.Assign(keySpace);
  PlayerInput_Crouch := TInputShortcut.Create(nil, 'Crouch (or fly/swim down)', 'move_down', igBasic);
  PlayerInput_Crouch.Assign(keyC);

  PlayerInput_Attack := TInputShortcut.Create(nil, 'Attack', 'attack', igBasic);
  PlayerInput_Attack.Assign(keyCtrl, keyNone, '', false, buttonLeft);
  PlayerInput_Attack.GroupOrder := -100; { before other (player) shortcuts }
  PlayerInput_InventoryShow := TInputShortcut.Create(nil, 'Inventory show / hide', 'inventory_toggle', igItems);
  PlayerInput_InventoryShow.Assign(keyNone, keyNone, '', false, buttonLeft);
  PlayerInput_InventoryPrevious := TInputShortcut.Create(nil, 'Select previous item', 'inventory_previous', igItems);
  PlayerInput_InventoryPrevious.Assign(keyLeftBracket, keyNone, '', false, buttonLeft, mwUp);
  PlayerInput_InventoryNext := TInputShortcut.Create(nil, 'Select next item', 'inventory_next', igItems);
  PlayerInput_InventoryNext.Assign(keyRightBracket, keyNone, '', false, buttonLeft, mwDown);
  PlayerInput_UseItem := TInputShortcut.Create(nil, 'Use (or equip) selected item', 'item_use', igItems);
  PlayerInput_UseItem.Assign(keyEnter, keyNone, '', false, buttonLeft);
  PlayerInput_DropItem := TInputShortcut.Create(nil, 'Drop selected item', 'item_drop', igItems);
  PlayerInput_DropItem.Assign(keyNone, keyNone, '', false, buttonLeft);
  PlayerInput_CancelFlying := TInputShortcut.Create(nil, 'Cancel flying spell', 'cancel_flying', igOther);
  PlayerInput_CancelFlying.Assign(keyNone, keyNone, '', false, buttonLeft);
end.
