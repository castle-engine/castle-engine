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

{ Player (TPlayer). }
unit CastlePlayer;

interface

uses Boxes3D, Cameras, CastleItems, VectorMath, GL, GLU, CastleInputs, KeysMouse,
  X3DTriangles, CastleTextureProperties, CastleSoundEngine, Classes, Base3D,
  CastleGLUtils, CastleColors, Frustum, CastleTriangles, CastleTimeUtils;

type
  TPlayerSwimming = (psNo,
    { Player is floating on the water.
      Not falling down, but also not drowning.
      This means that player head is above the water surface
      but his feet are in the water. In some sense he/she is swimming,
      in some not. }
    psAboveWater,
    psUnderWater);

  { Player, 3D object controlling the camera, main enemy of hostile creatures,
    carries a backpack, may cause fadeout effects on screen and such.

    Note that you can operate on player even before level is loaded,
    before TCastleSceneManager and such are initialized.
    This allows to create player before level is started
    (create it from scratch, or by loading from save game),
    and "carry" the same player instance across various loaded levels.

    @link(Dead) or @link(Blocked) player behaves much like alive and normal player.
    For example, it still has an associated Camera that can animate by code
    (e.g. to apply physics to the dead player body,
    because player was killed when he was flying, or it's
    corpse lays on some moving object of the level --- like elevator).
    However, Camera input shortcuts will be cleared, to prevent user from
    directly moving the camera and player.

    Do not do some stuff when player is dead:
    @unorderedList(
      @item No calling PickItem, DropItem, UseItem.
      @item(No increasing Life (further decreasing Life is OK).
        This implies that once Player is Dead, (s)he cannot be alive again.)
      @item No changing EquippedWeapon, no calling Attack.
    )

    Note that every player, just like every T3DOrient actually,
    has an associated and magically synchronized T3DOrient.Camera instance.
    Ancestor T3DOrient only takes care about synchronizing the view vectors
    (position, direciton, up) and doesn't care about camera otherwise.
    We synchronize more in TPlayer class:

    @unorderedList(
      @item(@link(Flying) is synchronized with TWalkCamera.Gravity.)
      @item(Various camera inputs are automatically adjusted based on current
        player state (@link(Dead), @link(Blocked)) and global PlayerInput_Xxx
        values, like PlayerInput_Forward.)
    )

    The outside code may still directly access and change many camera
    properties. Camera view vectors (position, direciton, up),
    TWalkCamera.PreferredHeight,
    TWalkCamera.RotationHorizontalSpeed
    TWalkCamera.RotationVerticalSpeed. In fact, it's Ok to call
    TWalkCamera.Init, and it's Ok to assign this Camera to TCastleSceneManager.Camera,
    and TGameSceneManager.LoadLevel does this automatically.
    So scene manager will update Camera.ProjectionMatrix,
    call camera events like TCamera.Press, TCamera.Idle and such.)
  }
  TPlayer = class(T3DAlive)
  private
    FInventory: TInventory;
    FEquippedWeapon: TItemWeapon;
    LifeTime: Single;

    { This means that weapon AttackAnimation is being done.
      This also means that EquippedWeapon <> nil. }
    Attacking: boolean;
    { If Attacking, then this is time of attack start, from LifeTime. }
    AttackStartTime: Single;
    { If Attacking, then this says whether EquippedWeapon.Kind.ActualAttack
      was already called. }
    ActualAttackDone: boolean;

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

    { Did last Idle detected that we're on lava ? }
    IsLava: boolean;
    { Relevant if IsLava, this is LifeTime when
      last time lava damage was done. When player steps on lava for the
      first time, he immediately gets damage, so LavaLastDamageTime is
      always valid when IsLava. }
    LavaLastDamageTime: Single;

    SwimmingChangeSound: TSound;
    SwimmingSound: TSound;

    { Did last Idle detected that we are on the ground. }
    IsOnTheGround: boolean;
    { <> @nil if IsOnTheGround and last ground had some TTextureProperties. }
    GroundProperties: TTextureProperties;
    ReallyIsOnTheGroundTime: Single;

    { There always must be satisfied:
        FootstepsSound <> nil
      if and only if
        FootstepsSoundPlaying <> stNone. }
    FootstepsSound: TSound;
    FootstepsSoundPlaying: TSoundType;
    ReallyWalkingOnTheGroundTime: Single;

    FInventoryCurrentItem: Integer;
    FInventoryVisible: boolean;
    FSickProjectionSpeed: Single;
    FBlocked: boolean;
    FRenderOnTop: boolean;

    FFlying: boolean;
    FFlyingTimeOut: TFloatTime;
    { FadeOut settings. }
    FFadeOutIntensity: Single;
    FFadeOutColor: TVector3Single;

    FFallMinHeightToSound: Single;
    FFallMinHeightToDamage: Single;
    FFallDamageScaleMin: Single;
    FFallDamageScaleMax: Single;
    FFallSound: TSoundType;

    procedure SetEquippedWeapon(Value: TItemWeapon);

    { Update Camera properties, including inputs.
      Call this always when @link(Flying) or @link(Dead) or some key values
      or @link(Swimming) or @link(Blocked) change. }
    procedure UpdateCamera;

    procedure CameraFall(ACamera: TWalkCamera; const FallHeight: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color). }
    procedure SetLifeCustomFadeOut(const Value: Single;
      const Color: TVector3Single);

    procedure SwimmingChangeSoundRelease(Sender: TSound);
    procedure SwimmingSoundRelease(Sender: TSound);
    procedure SetSwimming(const Value: TPlayerSwimming);

    procedure FootstepsSoundRelease(Sender: TSound);
    procedure SetFlying(const AValue: boolean);
    procedure SetFlyingTimeOut(const AValue: TFloatTime);
  protected
    procedure SetLife(const Value: Single); override;
    function GetChild: T3D; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function HeightCollision(const APosition, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
    procedure Fall(const FallHeight: Single); override;
  public
    { Various navigation properties that may depend on loaded level. }
    DefaultMoveHorizontalSpeed: Single;
    DefaultMoveVerticalSpeed: Single;
    DefaultPreferredHeight: Single;

    const
      DefaultLife = 100;
      DefaultSickProjectionSpeed = 2.0;
      DefaultRenderOnTop = true;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

    property Inventory: TInventory read FInventory;

    { Add Item to Inventory,
      with appropriate notification using CastleGameNotifications.
      See also TInventory.Pick (this is a wrapper around it).

      This takes care of adjusting InventoryCurrentItem if needed
      (if no item was selected, then newly picked item becomes selected). }
    function PickItem(Item: TInventoryItem): Integer;

    { Drop item from inventory.
      Like with UseItem, it is Ok to pass here Index out of range.
      @returns(Droppped item, or @nil if the operation was not completed due
        to any reason (e.g. no space on 3D world to fit this item).)
      @groupBegin }
    function DropItem(const Index: Integer): TItemOnWorld;
    function DropCurrentItem: TItemOnWorld;
    { @groupEnd }

    { Use an item from inventory.
      You can pass Index that is out of range (or call UseCurrentItem
      when InventoryCurrentItem = -1), it will then show
      a notification (by CastleGameNotifications unit) that nothing is selected.
      @groupBegin }
    procedure UseItem(const Index: Integer);
    procedure UseCurrentItem;
    { @groupEnd }

    { Change InventoryCurrentItem, cycling, and automatically showing
      the inventory afterwards (if it's not empty).
      Note that you can also always directly change InventoryCurrentItem property. }
    procedure ChangeInventoryCurrentItem(Change: Integer);

    { Weapon the player is using right now, or nil if none.

      EquippedWeapon.Kind must be TItemWeaponKind.

      You can set this property only to some item existing on Inventory.
      When you drop the current weapon,
      DeleteItem will automatically set this to @nil.

      When setting this property (to nil or non-nil) player may get
      GameMessage about using/not using a weapon. }
    property EquippedWeapon: TItemWeapon read FEquippedWeapon write SetEquippedWeapon;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { Cause a fade-out effect on the screen, tinting the screen to the given Color.
      The TPlayer class doesn't do the actual drawing of the fade-out effect
      on the screen, we merely store and animate the FadeOutColor and FadeOutIntensity
      properties. To draw the effect, use a procedure like GLFadeRectangle
      inside your 2D controls drawing code, see engine tutorial for example. }
    procedure FadeOut(const Color: TVector3Single);

    property FadeOutColor: TVector3Single read FFadeOutColor;
    property FadeOutIntensity: Single read FFadeOutIntensity;

    { @noAutoLinkHere }
    procedure Attack;

    { You should set this property as appropriate.
      This object will just use this property (changing it's Camera
      properties etc.). }
    property Swimming: TPlayerSwimming read FSwimming write SetSwimming;

    { Loads player properties from player.xml file.
      This is called from constructor, you can also call this
      later (for debug purposes, if you changed something). }
    procedure LoadFromFile;

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
      read FSickProjectionSpeed write FSickProjectionSpeed;

    property CollidesWithMoving default true;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; override;
    function Sphere(out Radius: Single): boolean; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;

    { Disables changing the camera by user.
      It's useful when you want to temporarily force camera to some specific
      setting (you can even use handy Player.Camera.AnimateTo method
      to do this easily, see TWalkCamera.AnimateTo). }
    property Blocked: boolean read FBlocked write FBlocked;

    { Render 3D children (like EquippedWeapon) on top of everything else. }
    property RenderOnTop: boolean read FRenderOnTop write FRenderOnTop
      default DefaultRenderOnTop;

    property FallMinHeightToSound: Single
      read FFallMinHeightToSound write FFallMinHeightToSound default DefaultPlayerFallMinHeightToSound;
    property FallMinHeightToDamage: Single
      read FFallMinHeightToDamage write FFallMinHeightToDamage default DefaultFallMinHeightToDamage;
    property FallDamageScaleMin: Single
      read FFallDamageScaleMin write FFallDamageScaleMin default DefaultFallDamageScaleMin;
    property FallDamageScaleMax: Single
      read FFallDamageScaleMax write FFallDamageScaleMax default DefaultFallDamageScaleMax;
    { Sound when falling.
      The default is the sound named 'player_fall'. }
    property FallSound: TSoundType
      read FFallSound write FFallSound;
  end;

const
  DefaultAutoOpenInventory = true;

var
  { Automatically open TCastlePlayer inventory when picking up an item.
    Saved/loaded to config file in this unit. }
  AutoOpenInventory: boolean = DefaultAutoOpenInventory;

  PlayerInput_Forward: TInputShortcut;
  PlayerInput_Backward: TInputShortcut;
  PlayerInput_LeftRot: TInputShortcut;
  PlayerInput_RightRot: TInputShortcut;
  PlayerInput_LeftStrafe: TInputShortcut;
  PlayerInput_RightStrafe: TInputShortcut;
  PlayerInput_UpRotate: TInputShortcut;
  PlayerInput_DownRotate: TInputShortcut;
  PlayerInput_GravityUp: TInputShortcut;
  PlayerInput_UpMove: TInputShortcut;
  PlayerInput_DownMove: TInputShortcut;

implementation

uses Math, SysUtils, CastleClassUtils, CastleUtils, X3DNodes, CastleControls,
  CastleImages, CastleFilesUtils, UIControls, PrecalculatedAnimation, CastleOpenAL,
  CastleGameNotifications, CastleXMLConfig, GLImages, CastleConfig,
  CastleResources;

{ TPlayerBox ----------------------------------------------------------------- }

type
  { Invisible box, that is added to make TPlayer collidable thanks to default
    T3DOrient (actually T3DList) methods. Owner must be TPlayer. }
  TPlayerBox = class(T3D)
  public
    function BoundingBox: TBox3D; override;
  end;

function TPlayerBox.BoundingBox: TBox3D;
var
  Camera: TWalkCamera;
begin
  if GetExists then
  begin
    Camera := TPlayer(Owner).Camera;
    Result.Data[0, 0] := -Camera.Radius;
    Result.Data[0, 1] := -Camera.Radius;
    Result.Data[0, 2] := -Camera.RealPreferredHeight;

    Result.Data[1, 0] := Camera.Radius;
    Result.Data[1, 1] := Camera.Radius;
    Result.Data[1, 2] := Camera.Radius;
  end else
    Result := EmptyBox3D;
end;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CollidesWithMoving := true;
  Life := DefaultLife;
  MaxLife := DefaultLife;
  DefaultMoveHorizontalSpeed := 1.0;
  DefaultMoveVerticalSpeed := 1.0;
  DefaultPreferredHeight := 0.0;
  RenderOnTop := DefaultRenderOnTop;
  FFallMinHeightToSound := DefaultPlayerFallMinHeightToSound;
  FFallMinHeightToDamage := DefaultFallMinHeightToDamage;
  FFallDamageScaleMin := DefaultFallDamageScaleMin;
  FFallDamageScaleMax := DefaultFallDamageScaleMax;
  FFallSound := SoundEngine.SoundFromName(DefaultPlayerFallSoundName, false);

  Add(TPlayerBox.Create(Self));

  FInventory := TInventory.Create(Self);
  FInventoryCurrentItem := -1;

  { turn off keys that are totally unavailable for the player }
  Camera.Input_MoveSpeedInc.MakeClear;
  Camera.Input_MoveSpeedDec.MakeClear;
  Camera.Input_IncreasePreferredHeight.MakeClear;
  Camera.Input_DecreasePreferredHeight.MakeClear;

  Camera.CheckModsDown := false;
  Camera.OnFall := @CameraFall;

  LoadFromFile;

  { Although it will be called in every OnIdle anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Camera has already good values. }
  UpdateCamera;
end;

destructor TPlayer.Destroy;
begin
  EquippedWeapon := nil; { unregister free notification }

  FreeAndNil(FInventory);

  if FootstepsSound <> nil then
    FootstepsSound.Release;

  if SwimmingChangeSound <> nil then
    SwimmingChangeSound.Release;

  if SwimmingSound <> nil then
    SwimmingSound.Release;

  inherited;
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

function TPlayer.PickItem(Item: TInventoryItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Kind.Caption]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  Notifications.Show(S);

  SoundEngine.Sound(stPlayerPickItem);

  Result := Inventory.Pick(Item);

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

  function GetItemDropPosition(DroppedItemKind: TItemKind;
    out DropPosition: TVector3Single): boolean;
  var
    ItemBox: TBox3D;
    ItemBoxRadius: Single;
    ItemBoxMiddle: TVector3Single;
  begin
    ItemBox := DroppedItemKind.BoundingBoxRotated;
    ItemBoxMiddle := ItemBox.Middle;
    { Box3DRadius calculates radius around (0, 0, 0) and we want
      radius around ItemBoxMiddle }
    ItemBoxRadius := ItemBox.Translate(VectorNegate(ItemBoxMiddle)).Radius;

    { Calculate DropPosition.

      We must move the item a little before us to
      1. show visually player that the item was dropped
      2. to avoid automatically picking it again

      Note that I take direction from DirectionInGravityPlane,
      not from Direction, otherwise when player is looking
      down he could be able to put item "inside the ground".
      Collision detection with the level below would actually
      prevent putting item "inside the ground", but the item
      would be too close to the player --- he could pick it up
      immediately. }
    DropPosition := Camera.Position +
      Camera.DirectionInGravityPlane *
        (0.6 * (Camera.RealPreferredHeight * Sqrt3 + ItemBox.Diagonal));

    { Now check is DropPosition actually possible
      (i.e. check collisions item<->everything).
      The assumption is that item starts from
      Camera.Position and is moved to DropPosition.

      But actually we must shift both these positions,
      so that we check positions that are ideally in the middle
      of item's BoundingBoxRotated. Otherwise the item
      could get *partially* stuck within the wall, which wouldn't
      look good. }

    Result := World.WorldMoveAllowed(
      ItemBoxMiddle + Camera.Position,
      ItemBoxMiddle + DropPosition, true, ItemBoxRadius,
      ItemBox + Camera.Position,
      ItemBox + DropPosition, false);
  end;

var
  S: string;
  DropPosition: TVector3Single;
  DropppedItem: TInventoryItem;
begin
  Result := nil;

  if Between(Index, 0, Inventory.Count - 1) then
  begin
    if GetItemDropPosition(Inventory[Index].Kind, DropPosition) then
    begin
      DropppedItem := Inventory.Drop(Index);

      if DropppedItem = EquippedWeapon then
        EquippedWeapon := nil;

      S := Format('You drop "%s"', [DropppedItem.Kind.Caption]);
      if DropppedItem.Quantity <> 1 then
        S += Format(' (quantity %d)', [DropppedItem.Quantity]);
      Notifications.Show(S);

      SoundEngine.Sound(stPlayerDropItem);

      { update InventoryCurrentItem.
        Note that if Inventory.Count = 0 now, then this will
        correctly set InventoryCurrentItem to -1. }
      if InventoryCurrentItem >= Inventory.Count then
        InventoryCurrentItem := Inventory.Count - 1;

      Result := DropppedItem.PutOnWorld(World, DropPosition);
    end else
      Notifications.Show('Not enough room here to drop this item');
  end else
    Notifications.Show('Nothing to drop - select some item first');
end;

function TPlayer.DropCurrentItem: TItemOnWorld;
begin
  Result := DropItem(InventoryCurrentItem);
end;

procedure TPlayer.UseItem(const Index: Integer);
begin
  if Between(Index, 0, Inventory.Count - 1) then
  begin
    Inventory.Use(Index);
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
        [EquippedWeapon.Kind.Caption]));
      SoundEngine.Sound(EquippedWeapon.Kind.EquippingSound);
      FEquippedWeapon.FreeNotification(Self);
    end else
      Notifications.Show('You''re no longer using your weapon');

    { Any attack done with previous weapon must be stopped now. }
    Attacking := false;
  end;
end;

procedure TPlayer.UpdateCamera;
const
  CastleCameraInput = [ciNormal, ci3dMouse]; { do not include ciMouseDragging }
begin
  Camera.Gravity := (not Flying) and (not Dead) and (not Blocked);
  { Note that when not Camera.Gravity then FallingEffect will not
    work anyway. }
  Camera.FallingEffect := Swimming = psNo;

  if Blocked then
  begin
    { When Blocked, we navigate camera by code. }
    Camera.Input := [];
  end else
  begin
    Camera.Input := CastleCameraInput;

    { Rotation keys work always, even when player is dead.
      Initially I disabled them, but after some thought:
      let them work. They work a little strangely (because Up
      is orthogonal to GravityUp), but they still work and player
      can figure it out. }
    Camera.Input_LeftRot.Assign(PlayerInput_LeftRot, false);
    Camera.Input_RightRot.Assign(PlayerInput_RightRot, false);
    Camera.Input_UpRotate.Assign(PlayerInput_UpRotate, false);
    Camera.Input_DownRotate.Assign(PlayerInput_DownRotate, false);
    Camera.Input_GravityUp.Assign(PlayerInput_GravityUp, false);
  end;

  if Blocked then
  begin
    { PreferGravityUpXxx should be ignored actually, because rotations
      don't work now. }
    Camera.PreferGravityUpForMoving := true;
    Camera.PreferGravityUpForRotations := false;

    { No need to do MakeClear now on any inputs, as we already set
      Input := []. }

    Camera.FallSpeedStart := DefaultFallSpeedStart;
    Camera.FallSpeedIncrease := DefaultFallSpeedIncrease;
    Camera.HeadBobbing := 0.0;
    Camera.PreferredHeight := Camera.Radius * 1.01;

    Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
    Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
  end else
  if Dead then
  begin
    Camera.PreferGravityUpForMoving := true;
    { This is the only case when PreferGravityUpForRotations := false
      is sensible. }
    Camera.PreferGravityUpForRotations := false;

    Camera.Input_Jump.MakeClear;
    Camera.Input_Crouch.MakeClear;
    Camera.Input_UpMove.MakeClear;
    Camera.Input_DownMove.MakeClear;

    Camera.Input_Forward.MakeClear;
    Camera.Input_Backward.MakeClear;
    Camera.Input_LeftStrafe.MakeClear;
    Camera.Input_RightStrafe.MakeClear;

    Camera.FallSpeedStart := DefaultFallSpeedStart;
    Camera.FallSpeedIncrease := DefaultFallSpeedIncrease;
    Camera.HeadBobbing := 0.0;
    Camera.PreferredHeight := Camera.Radius * 1.01;

    Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
    Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
  end else
  begin
    if Flying then
    begin
      Camera.PreferGravityUpForMoving := false;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.MakeClear;
      Camera.Input_Crouch.MakeClear;
      Camera.Input_UpMove.Assign(PlayerInput_UpMove, false);
      Camera.Input_DownMove.Assign(PlayerInput_DownMove, false);

      { Camera.HeadBobbing and
        Camera.PreferredHeight and
        Camera.FallSpeedStart and
        Camera.FallSpeedIncrease
        ... don't matter here, because Gravity is false. }

      Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
      Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
    end else
    if Swimming <> psNo then
    begin
      Camera.PreferGravityUpForMoving := false;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.MakeClear;
      Camera.Input_Crouch.MakeClear;
      Camera.Input_UpMove.Assign(PlayerInput_UpMove, false);
      Camera.Input_DownMove.Assign(PlayerInput_DownMove, false);

      Camera.FallSpeedStart := DefaultFallSpeedStart / 6;
      Camera.FallSpeedIncrease := 1.0;
      Camera.HeadBobbing := 0.0;
      Camera.PreferredHeight := Camera.Radius * 1.01;

      Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed / 2;
      Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed / 2;
    end else
    begin
      Camera.PreferGravityUpForMoving := true;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.Assign(PlayerInput_UpMove, false);
      Camera.Input_Crouch.Assign(PlayerInput_DownMove, false);
      Camera.Input_UpMove.MakeClear;
      Camera.Input_DownMove.MakeClear;

      Camera.FallSpeedStart := DefaultFallSpeedStart;
      Camera.FallSpeedIncrease := DefaultFallSpeedIncrease;
      Camera.HeadBobbing := DefaultHeadBobbing;
      Camera.PreferredHeight := DefaultPreferredHeight;

      Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
      Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
    end;

    Camera.Input_Forward.Assign(PlayerInput_Forward, false);
    Camera.Input_Backward.Assign(PlayerInput_Backward, false);
    Camera.Input_LeftStrafe.Assign(PlayerInput_LeftStrafe, false);
    Camera.Input_RightStrafe.Assign(PlayerInput_RightStrafe, false);
  end;
end;

procedure TPlayer.FootstepsSoundRelease(Sender: TSound);
begin
  Assert(Sender = FootstepsSound);
  FootstepsSound := nil;
  FootstepsSoundPlaying := stNone;
end;

procedure TPlayer.SwimmingChangeSoundRelease(Sender: TSound);
begin
  Assert(Sender = SwimmingChangeSound);
  SwimmingChangeSound := nil;
end;

procedure TPlayer.SwimmingSoundRelease(Sender: TSound);
begin
  Assert(Sender = SwimmingSound);
  SwimmingSound := nil;
end;

procedure TPlayer.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

  { Perform various things related to player swimming. }
  procedure UpdateSwimming;
  const
    { How many seconds you can swin before you start to drown ? }
    SwimBreathSeconds = 30.0;
    { How many seconds between each drown ? }
    SwimDrownPauseSeconds = 5.0;
    { Pause between playing stPlayerSwimming sound.
      Remember to set this so that it will *not* easily synchronize
      with stPlayerDrowning. }
    SwimSoundPauseSeconds = 3.11111111;
  var
    NewSwimming: TPlayerSwimming;
  begin
    { update Swimming }
    NewSwimming := psNo;
    if World <> nil then
    begin
      if World.Water.PointInside(Position) then
        NewSwimming := psUnderWater else
      if World.Water.PointInside(Position - World.GravityUp * Camera.PreferredHeight) then
        NewSwimming := psAboveWater;
    end;
    Swimming := NewSwimming;

    if Swimming = psUnderWater then
    begin
      { Take care of drowning. }
      if not Dead then
      begin
        if LifeTime - SwimBeginTime > SwimBreathSeconds then
        begin
          if (SwimLastDrownTime = 0.0) or
             (LifeTime - SwimLastDrownTime > SwimDrownPauseSeconds) then
          begin
            if SwimLastDrownTime = 0.0 then
              Notifications.Show('You''re drowning');
            SwimLastDrownTime := LifeTime;
            Life := Life - (5 + Random(10));
            SoundEngine.Sound(stPlayerDrowning);
          end;
        end;
      end;

      { Take care of playing stPlayerSwimming }
      { See comments at creation of SwimmingChangeSound
        for reasons why I should safeguard here and play this sound
        only when SwimmingSound = nil. }
      if (SwimmingSound = nil) and
         ( (SwimLastSoundTime = 0.0) or
           (LifeTime - SwimLastSoundTime > SwimSoundPauseSeconds) ) then
      begin
        SwimLastSoundTime := LifeTime;
        SwimmingSound := SoundEngine.Sound(stPlayerSwimming);
        if SwimmingSound <> nil then
          SwimmingSound.OnRelease := @SwimmingSoundRelease;
      end;
    end;
  end;

  { Update IsOnTheGround and related variables. }
  procedure UpdateIsOnTheGround;
  const
    { TimeToChangeOnTheGround and ReallyIsOnTheGroundTime play here the
      analogous role as ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSoundPlaying, see UpdateFootstepsSoundPlaying. }
    TimeToChangeIsOnTheGround = 0.5;
  begin
    if Camera.IsOnTheGround then
    begin
      ReallyIsOnTheGroundTime := LifeTime;
      IsOnTheGround := true;
      GroundProperties := TexturesProperties.Find(Ground);
    end else
    if LifeTime - ReallyIsOnTheGroundTime > TimeToChangeIsOnTheGround then
    begin
      GroundProperties := nil;
      IsOnTheGround := false;
    end; { else leave GroundProperties and IsOnTheGround unchanged. }
  end;

  { Update IsLava and related variables, hurt player if on lava.
    Must be called after UpdateIsOnTheGround (depends on GroundProperties). }
  procedure UpdateLava;
  var
    NewIsLava: boolean;
  begin
    NewIsLava := (GroundProperties <> nil) and GroundProperties.Lava;
    if NewIsLava then
    begin
      if (not IsLava) or
         (LifeTime - LavaLastDamageTime > GroundProperties.LavaDamageTime) then
      begin
        LavaLastDamageTime := LifeTime;
        if not Dead then
        begin
          SoundEngine.Sound(stPlayerLavaPain);
          SetLifeCustomFadeOut(Life - (GroundProperties.LavaDamageConst +
            Random * GroundProperties.LavaDamageRandom), Green3Single);
        end;
      end;
    end;
    IsLava := NewIsLava;
  end;

  { Update FootstepsSoundPlaying and related variables.
    Must be called after UpdateIsOnTheGround (depends on GroundProperties). }
  procedure UpdateFootstepsSoundPlaying;
  const
    TimeToChangeFootstepsSoundPlaying = 0.5;
  var
    NewFootstepsSoundPlaying: TSoundType;
  begin
    { The meaning of ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSoundPlaying:
      Camera.IsWalkingOnTheGround can change quite rapidly
      (when player quickly presses and releases up/down keys,
      or when he're walking up the stairs, or when he's walking
      on un-flat terrain --- then Camera.IsWalkingOnTheGround
      switches between @true and @false quite often).
      But it is undesirable to change FootstepsSoundPlaying
      so often, as this causes footsteps to suddenly stop, then play,
      then stop again etc. --- this doesn't sound good.

      So I use ReallyWalkingOnTheGroundTime to mark myself
      the time when Camera.IsWalkingOnTheGround was true.
      In normal situation I would set NewFootstepsSoundPlaying to stNone
      when Camera.IsWalkingOnTheGround = @false.
      But now I set NewFootstepsSoundPlaying to stNone only if
      Camera.IsWalkingOnTheGround = @false
      for at least TimeToChangeFootstepsSoundPlaying seconds. }

    { calculate NewFootstepsSoundPlaying }
    if Camera.IsWalkingOnTheGround then
    begin
      ReallyWalkingOnTheGroundTime := LifeTime;
      { Since Camera.IsWalkingOnTheGroundm then for sure
        Camera.IsOnTheGround, so UpdateIsOnTheGround updated
        GroundProperties field. }
      if (GroundProperties <> nil) and
         (GroundProperties.FootstepsSound <> stNone) then
        NewFootstepsSoundPlaying := GroundProperties.FootstepsSound else
        NewFootstepsSoundPlaying := stPlayerFootstepsConcrete { default footsteps sound };
    end else
    if LifeTime - ReallyWalkingOnTheGroundTime >
      TimeToChangeFootstepsSoundPlaying then
      NewFootstepsSoundPlaying := stNone else
      NewFootstepsSoundPlaying := FootstepsSoundPlaying;

    { Once I had an idea here to use AL_LOOPING sound for footsteps.
      But this is not good, because then I would have to manually
      stop this sound whenever player stops walking. This is not so easy.
      This occurs when FootstepsSoundPlaying changes from non-stNone to stNone,
      but this occurs also when CastlePlay starts loading new level, enters
      game menu etc. --- in all these cases player footsteps must stop.
      So it's better (simpler) to simply use non-looping sound for footsteps.
      Whenever old sound for footsteps will end, this procedure will just
      allocate and start new footsteps sound. }

    if FootstepsSoundPlaying <> NewFootstepsSoundPlaying then
    begin
      if FootstepsSoundPlaying <> stNone then
      begin
        { Stop footsteps sound. }
        FootstepsSound.Release;
        { FootstepsSoundRelease should set this to nil. }
        Assert(FootstepsSound = nil);
      end;

      if NewFootstepsSoundPlaying <> stNone then
      begin
        { Start footsteps sound. }
        FootstepsSound := SoundEngine.Sound(NewFootstepsSoundPlaying, false);
        if FootstepsSound <> nil then
        begin
          { Lower the position, to be on our feet. }
          FootstepsSound.Position := Vector3Single(0, 0, -1.0);
          FootstepsSound.OnRelease := @FootstepsSoundRelease;
        end else
          { Failed to allocate sound, so force new
            NewFootstepsSoundPlaying to stNone. }
          NewFootstepsSoundPlaying := stNone;
      end;

      FootstepsSoundPlaying := NewFootstepsSoundPlaying;
    end else
    if FootstepsSoundPlaying <> stNone then
    begin
      { So FootstepsSoundPlaying = NewFootstepsSoundPlaying for sure.
        Make sure that the AL sound is really playing.

        The decision to not use looping sound means that
        end of footsteps sound should be detected
        almost immediately (otherwise player will hear a little pause
        in footsteps, due to the time of OnTimer that calls RefreshUsed
        of source allocator --- it's very short pause, but it's noticeable,
        since footsteps should be rhytmic). I prefer to not rely on RefreshUsed
        for this and instead just check this here. }
      if not FootstepsSound.PlayingOrPaused then
        alSourcePlay(FootstepsSound.ALSource);
    end;

    Assert(
      (FootstepsSound <> nil) =
      (FootstepsSoundPlaying <> stNone));
  end;

const
  FadeOutSpeed = 2.0;
begin
  inherited;
  if not GetExists then Exit;
  LifeTime += CompSpeed;

  if FFlyingTimeOut > 0 then
  begin
    FFlyingTimeOut := FFlyingTimeOut - CompSpeed;
    if FFlyingTimeOut <= 0 then
    begin
      FFlyingTimeOut := 0;
      FFlying := false;
    end;
  end;

  UpdateCamera;

  UpdateSwimming;

  if FFadeOutIntensity > 0 then
    FFadeOutIntensity -= FadeOutSpeed * CompSpeed;

  if Attacking and (not ActualAttackDone) and (LifeTime -
    AttackStartTime >= EquippedWeapon.Kind.ActualAttackTime) then
  begin
    ActualAttackDone := true;
    EquippedWeapon.ActualAttack;
  end;

  UpdateIsOnTheGround;
  UpdateLava;
  UpdateFootstepsSoundPlaying;
end;

procedure TPlayer.FadeOut(const Color: TVector3Single);
begin
  FFadeOutColor := Color;
  FFadeOutIntensity := 1;
end;

procedure TPlayer.CameraFall(ACamera: TWalkCamera; const FallHeight: Single);
begin
  Fall(FallHeight);
end;

procedure TPlayer.Fall(const FallHeight: Single);
begin
  inherited;

  if (Swimming = psNo) and (FallHeight > FallMinHeightToSound) then
    SoundEngine.Sound(FallSound);

  if (Swimming = psNo) and (FallHeight > FallMinHeightToDamage) then
    Life := Life - Max(0, FallHeight *
      MapRange(Random, 0.0, 1.0, FallDamageScaleMin, FallDamageScaleMax));
end;

procedure TPlayer.SetLifeCustomFadeOut(const Value: Single;
  const Color: TVector3Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    Notifications.Show('You die');
    SoundEngine.Sound(stPlayerDies);
    Camera.FallOnTheGround;
  end else
  if (Life - Value) > 1 then
  begin
    FadeOut(Color);
    SoundEngine.Sound(stPlayerSuddenPain);
  end;
  inherited SetLife(Value);
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  SetLifeCustomFadeOut(Value, Red3Single);
end;

procedure TPlayer.Attack;
begin
  if not Attacking then
  begin
    if EquippedWeapon <> nil then
    begin
      SoundEngine.Sound(EquippedWeapon.Kind.SoundAttackStart);
      AttackStartTime := LifeTime;
      Attacking := true;
      ActualAttackDone := false;
    end else
      { TODO: maybe I should allow him to do some "punch" / "kick" here ? }
      Notifications.Show('No weapon equipped');
  end;
end;

procedure TPlayer.SetSwimming(const Value: TPlayerSwimming);
begin
  if Value <> FSwimming then
  begin
    { If "Swimming = psUnderWater" state changed then play a sound. }
    if (FSwimming = psUnderWater) <>
       (Value = psUnderWater) then
    begin
      { If SwimmingChangeSound <> nil, then the
        stPlayerSwimmingChange sound is already played (this may be caused
        when player tries to stay above the water --- he will then repeatedly
        go under and above the water). So do not start it again, to avoid
        bad sound atrifacts (the same sound playing a couple times on top
        of each other). }
      if SwimmingChangeSound = nil then
      begin
        SwimmingChangeSound := SoundEngine.Sound(stPlayerSwimmingChange);
        if SwimmingChangeSound <> nil then
          SwimmingChangeSound.OnRelease := @SwimmingChangeSoundRelease;
      end;
    end;

    if (FSwimming = psNo) and (Value <> psNo) then
    begin
      { Cancel falling down, otherwise he will fall down into the water
        with the high speed (because in the air FallSpeedStart
        is high and it's increased, but in the water it's much lower
        and not increased at all right now). }
      Camera.CancelFalling;
    end;

    FSwimming := Value;

    if Swimming = psUnderWater then
    begin
      SwimBeginTime := LifeTime;
      SwimLastDrownTime := 0.0;
      SwimLastSoundTime := 0.0;
    end;

    { Although UpdateCamera will be called in nearest Player.Idle anyway,
      I want to call it *now*. That's because I want to set
      Camera.FallSpeedStart to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Camera.CancelFalling above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFalling ! So next falling down definitely *can*
      happen before next Player.Idle. Actually we may be in the middle
      of falling down right now. Fortunately Camera.Idle
      and Camera.CancelFalling are implemented (or rather fixed :)
      to honour calling CancelFalling and setting FallSpeedStart now.

      So the safeguard below is needed. }
    UpdateCamera;
  end;
end;

procedure TPlayer.LoadFromFile;
var
  PlayerConfig: TCastleConfig;
begin
  PlayerConfig := TCastleConfig.Create(nil);
  try
    PlayerConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'player.xml';

    KnockBackSpeed :=
      PlayerConfig.GetFloat('player/knock_back_speed', DefaultKnockBackSpeed);
    Camera.MaxJumpHeight :=
      PlayerConfig.GetFloat('player/jump/max_height',
      DefaultMaxJumpHeight);
    Camera.JumpSpeedMultiply :=
      PlayerConfig.GetFloat('player/jump/speed_multiply',
      DefaultJumpSpeedMultiply);
    Camera.JumpPower :=
      PlayerConfig.GetFloat('player/jump/power',
      DefaultJumpPower);
    Camera.HeadBobbingTime :=
      PlayerConfig.GetFloat('player/head_bobbing_time',
      DefaultHeadBobbingTime);
    SickProjectionSpeed := PlayerConfig.GetFloat('player/sick_projection_speed',
      DefaultSickProjectionSpeed);
    FallMinHeightToSound := PlayerConfig.GetFloat('fall/sound/min_height', DefaultPlayerFallMinHeightToSound);
    FallMinHeightToDamage := PlayerConfig.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
    FallDamageScaleMin := PlayerConfig.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
    FallDamageScaleMax := PlayerConfig.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);
    FallSound := SoundEngine.SoundFromName(
      PlayerConfig.GetValue('fall/sound/name', DefaultPlayerFallSoundName), false);
  finally SysUtils.FreeAndNil(PlayerConfig); end;
end;

procedure TPlayer.LevelChanged;
begin
  { Without this, ReallyWalkingOnTheGroundTime could pretend that
    player is walking on the ground, while in fact the player is just
    standing still after new level loaded. }
  ReallyWalkingOnTheGroundTime := -1000.0;

  if FootstepsSoundPlaying <> stNone then
  begin
    { Stop footsteps sound. }
    FootstepsSound.Release;
    { FootstepsSoundRelease should set this to nil. }
    Assert(FootstepsSound = nil);

    FootstepsSoundPlaying := stNone;
  end;

  ReallyIsOnTheGroundTime := -1000;
  IsOnTheGround := false;
  GroundProperties := nil;

  IsLava := false;

  Attacking := false;
end;

function TPlayer.Ground: PTriangle;
begin
  Result := PTriangle(Camera.AboveGround);
end;

function TPlayer.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  if ALineOfSight then
    { Player box is collidable (creatures cannot enter on player),
      but is not visible, so ALineOfSight ignores it.
      This allows creatures to see player's middle point. }
    Result := false else
    Result := inherited;
end;

function TPlayer.Sphere(out Radius: Single): boolean;
begin
  Result := true;
  Radius := Camera.Radius;
end;

function TPlayer.GetChild: T3D;
var
  AttackTime: Single;
  AttackAnim: T3DResourceAnimation;
begin
  Result := nil;
  if (EquippedWeapon <> nil) and
    EquippedWeapon.Kind.Prepared then
  begin
    AttackAnim := EquippedWeapon.Kind.AttackAnimation;
    AttackTime := LifeTime - AttackStartTime;
    if Attacking and (AttackTime <= AttackAnim.Duration) then
    begin
      Result := AttackAnim.Scene(AttackTime, false);
    end else
    begin
      { turn off Attacking, if AttackTime passed }
      Attacking := false;
      { although current weapons animations are just static,
        we use LifeTime to enable any weapon animation
        (like weapon swaying, or some fire over the sword or such) in the future. }
      Result :=  EquippedWeapon.Kind.ReadyAnimation.Scene(LifeTime, true);
    end;
  end;
end;

procedure TPlayer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FEquippedWeapon) then
    FEquippedWeapon := nil;
end;

function TPlayer.HeightCollision(const APosition, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
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

procedure TPlayer.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  { TODO: This implementation is a quick hack, that depends on the fact
    that TPlayer.Render is the *only* thing in the whole engine currently
    calling glDepthRange.

    - The first frame with TPlayer could be incorrect, as 3D objects drawn before
      will have 0..1 glDepthRange that may overlap with our weapon.
      It works now only because default player positions are when
      the weapon doesn't overlap with level in 3D.
    - We never fix the glDepthRange back to 0..1.

    The idea of using glDepthRange for layers seems quite good, it's
    quite a nice solution,
    - you don't have to split rendering layers in passes, you can render
      all objects in one pass, just switching glDepthRange as necessary.
    - you can set glDepthRange for 3D objects inside T3DList,
      like here TPlayer will just affect every child underneath.

    But it has to be implemented in more extensible manner in the future.
    It should also enable X3D layers. }

  if RenderOnTop then glDepthRange(0, 0.1);
    inherited;
  if RenderOnTop then glDepthRange(0.1, 1);
end;

{ initialization / finalization ---------------------------------------- }

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  AutoOpenInventory := Config.GetValue(
    'auto_open_inventory', DefaultAutoOpenInventory);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('auto_open_inventory',
    AutoOpenInventory, DefaultAutoOpenInventory);
end;

initialization
  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  PlayerInput_Forward := TInputShortcut.Create(nil, 'Move forward', 'move_forward', igBasic);
  PlayerInput_Forward.Assign(K_W, K_Up, #0, false, mbLeft);
  PlayerInput_Backward := TInputShortcut.Create(nil, 'Move backward', 'move_backward', igBasic);
  PlayerInput_Backward.Assign(K_S, K_Down, #0, false, mbLeft);
  PlayerInput_LeftStrafe := TInputShortcut.Create(nil, 'Move left', 'move_left', igBasic);
  PlayerInput_LeftStrafe.Assign(K_A, K_None, #0, false, mbLeft);
  PlayerInput_RightStrafe := TInputShortcut.Create(nil, 'Move right', 'move_right', igBasic);
  PlayerInput_RightStrafe.Assign(K_D, K_None, #0, false, mbLeft);
  PlayerInput_LeftRot := TInputShortcut.Create(nil, 'Turn left', 'turn_left', igBasic);
  PlayerInput_LeftRot.Assign(K_Left, K_None, #0, false, mbLeft);
  PlayerInput_RightRot := TInputShortcut.Create(nil, 'Turn right', 'turn_right', igBasic);
  PlayerInput_RightRot.Assign(K_Right, K_None, #0, false, mbLeft);
  PlayerInput_UpRotate := TInputShortcut.Create(nil, 'Look up', 'look_up', igBasic);
  PlayerInput_UpRotate.Assign(K_PageDown, K_None, #0, false, mbLeft);
  PlayerInput_DownRotate := TInputShortcut.Create(nil, 'Look down', 'look_down', igBasic);
  PlayerInput_DownRotate.Assign(K_Delete, K_None, #0, false, mbLeft);
  PlayerInput_GravityUp := TInputShortcut.Create(nil, 'Look straight', 'look_straight', igBasic);
  PlayerInput_GravityUp.Assign(K_End, K_None, #0, false, mbLeft);
  PlayerInput_UpMove := TInputShortcut.Create(nil, 'Jump (or fly/swim up)', 'move_up', igBasic);
  PlayerInput_UpMove.Assign(K_Space, K_None, #0, true, mbRight);
  PlayerInput_DownMove := TInputShortcut.Create(nil, 'Crouch (or fly/swim down)', 'move_down', igBasic);
  PlayerInput_DownMove.Assign(K_C, K_None, #0, false, mbLeft);

  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
