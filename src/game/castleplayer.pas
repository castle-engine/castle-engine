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

{ Player (TPlayer). }
unit CastlePlayer;

interface

uses Boxes3D, Cameras, CastleItems, VectorMath, GL, GLU,
  CastleInputs, CastleResources, KeysMouse,
  Triangle, CastleTextureProperties, CastleSoundEngine, Classes, Base3D,
  CastleGLUtils, CastleColors;

const
  DefaultPlayerLife = 100;
  DefaultSickProjectionSpeed = 2.0;

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
    carries a backpack, may cause blackout effects on screen and such.

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
      @item No calling PickItem, DropItem.
      @item(No increasing Life (further decreasing Life is OK).
        This implies that once Player is Dead, (s)he cannot be alive again.)
      @item No changing EquippedWeapon, no calling Attack.
    ) }
  TPlayer = class(T3DAlive)
  private
    FCamera: TWalkCamera;
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
    FSickProjectionSpeed: Single;

    function GetFlyingMode: boolean;
    procedure SetEquippedWeapon(Value: TItemWeapon);

    { Update Camera properties, including inputs.
      Call this always when FlyingMode or Dead or some key values
      or Swimming or Blocked change. }
    procedure UpdateCamera;

    procedure FalledDown(Camera: TWalkCamera; const FallenHeight: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color). }
    procedure SetLifeCustomBlackOut(const Value: Single;
      const Color: TVector3Single);

    procedure SwimmingChangeSoundRelease(Sender: TSound);
    procedure SwimmingSoundRelease(Sender: TSound);
    procedure SetSwimming(const Value: TPlayerSwimming);

    procedure FootstepsSoundRelease(Sender: TSound);
  protected
    procedure SetLife(const Value: Single); override;
    function GetChild: T3D; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function HeightCollision(const APosition, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
  public
    { When this is > 0 means flying. In seconds. }
    FlyingModeTimeOut: Single;

    { Blackout settings. }
    BlackOutIntensity: TGLfloat;
    BlackOutColor: TVector3f;

    { Various navigation properties that may depend on loaded level. }
    DefaultMoveHorizontalSpeed: Single;
    DefaultMoveVerticalSpeed: Single;
    DefaultPreferredHeight: Single;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FlyingMode: boolean read GetFlyingMode;

    { Start FlyingMode, for TimeOut time (TimeOut time is in seconds).
      After TimeOut time, flying mode will stop.
      Call this only with TimeOut > 0. }
    procedure FlyingModeTimeoutBegin(const TimeOut: Single);

    { Cancel FlyingMode. Useful if you're in the FlyingMode that will
      automatically wear off, but you don't want to wait and you
      want to cancel flying *now*. Ignored if not in FlyingMode.

      Note that while you can call this when Dead or Blocked, this will
      be always ignored (because when Dead or Blocked,
      FlyingMode is always false). }
    procedure CancelFlying;

    property Inventory: TInventory read FInventory;

    { Each player object always has related Camera object.

      Some things are synchronized between player properties and this
      Camera object --- e.g. player's FlyingMode is synchronized
      with Gravity and some Input_Xxx properties of this camera.
      In general, this player object "cooperates" in various ways
      with it's Camera object, that's why it was most comfortable
      to just put Camera inside TPlayer.

      In general you @italic(must not) operate directly on this Camera
      properties or call it's methods (TPlayer instance should do this for you),
      with some exceptions.

      You are allowed to read and write:
      @unorderedList(
        @item(Camera.Position, Direction, Up and InitialCameraXxx ---
          these are exactly player's camera settings.)
        @item(Camera.PreferredHeight. In fact, it's OK to just call
          Camera.Init.)
        @item(Camera.RotationHorizontal/VerticalSpeed
          (you can read and write this --- although it should be
          only for testing/debug purposes, in real game rotation speeds
          should stay constant).)
        @item(Camera.ProjectionMatrix, to update it in game's
          OnResize or such.)
        @item(You can call Camera.KeyDown, MouseDown, Idle.
          In fact it's OK to just assign Camera to Window.Camera.)
        @item(You can assign things to Camera.OnMatrixChanged.)
      )

      You are allowed to read:
      @unorderedList(
        @item Camera.RotationMatrix, Matrix, Frustum.
      )
    }
    property Camera: TWalkCamera read FCamera;

    { Add Item to Inventory, with appropriate GameMessage.
      See also TInventory.Pick (this is a wrapper around it).

      This takes care of adjusting InventoryCurrentItem if needed
      (if no item was selected, then newly picked item becomes selected). }
    function PickItem(Item: TInventoryItem): Integer;

    { Drop given item. ItemIndex must be valid (between 0 and Inventory.Count - 1).
      Returns nil if player somehow cancelled operation and nothing is dropped
      (although not really possible now).
      You *must* take care yourself of returned TInventoryItem object (otherwise you will
      get memory leak !). }
    function DropItem(const ItemIndex: Integer): TInventoryItem;

    { Weapon the player is using right now, or nil if none.

      EquippedWeapon.Kind must be TItemWeaponKind.

      You can set this property only to some item existing on Inventory.
      When you drop the current weapon,
      DeleteItem will automatically set this to @nil.

      When setting this property (to nil or non-nil) player may get
      GameMessage about using/not using a weapon. }
    property EquippedWeapon: TItemWeapon read FEquippedWeapon write SetEquippedWeapon;

    { Adjust some things based on passing time.
      For now, this is for things like FlyingModeTimeout to "wear out".
      @noAutoLinkHere }
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { Make blackout with given Color (so it's not really a "black"out,
      it's fadeout + fadein with given Color; e.g. pass here red
      to get "redout").
      @noAutoLinkHere }
    procedure BlackOut(const Color: TVector3f);

    { Shortcut for BlackOut with red color.
      @noAutoLinkHere }
    procedure RedOut;

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

    property SickProjectionSpeed: Single
      read FSickProjectionSpeed write FSickProjectionSpeed;

    property CollidesWithMoving default true;
    procedure Translate(const T: TVector3Single); override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; override;
    function Sphere(out Radius: Single): boolean; override;
  end;

const
  DefaultUseMouseLook = true;
  DefaultInvertVerticalMouseLook = false;

var
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

  { Game player camera settings.
    Automatically saved/loaded from user preferences using CastleConfig.
    @groupBegin }
  UseMouseLook: boolean = DefaultUseMouseLook;
  InvertVerticalMouseLook: boolean = DefaultInvertVerticalMouseLook;
  MouseLookHorizontalSensitivity: Single;
  MouseLookVerticalSensitivity: Single;
  { @groupEnd }

implementation

uses Math, SysUtils, CastleClassUtils, CastleUtils, X3DNodes, CastleControls,
  Images, CastleFilesUtils, UIControls, PrecalculatedAnimation, CastleOpenAL,
  CastleGameNotifications, CastleXMLConfig, GLImages, DOM, CastleConfig;

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
  Life := DefaultPlayerLife;
  MaxLife := DefaultPlayerLife;
  DefaultMoveHorizontalSpeed := 1.0;
  DefaultMoveVerticalSpeed := 1.0;
  DefaultPreferredHeight := 0.0;

  Add(TPlayerBox.Create(Self));

  FInventory := TInventory.Create(Self);
  FInventoryCurrentItem := -1;

  FCamera := TWalkCamera.Create(nil);

  { turn off keys that are totally unavailable for the player }
  Camera.Input_MoveSpeedInc.MakeClear;
  Camera.Input_MoveSpeedDec.MakeClear;
  Camera.Input_IncreasePreferredHeight.MakeClear;
  Camera.Input_DecreasePreferredHeight.MakeClear;
  Camera.Input_Run.MakeClear; { speed in castle is so fast that we're always running }

  Camera.CheckModsDown := false;
  Camera.OnFalledDown := @FalledDown;

  LoadFromFile;

  { Although it will be called in every OnIdle anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Camera has already good values. }
  UpdateCamera;
end;

destructor TPlayer.Destroy;
begin
  EquippedWeapon := nil; { unregister free notification }

  FreeAndNil(FCamera);
  FreeAndNil(FInventory);

  if FootstepsSound <> nil then
    FootstepsSound.Release;

  if SwimmingChangeSound <> nil then
    SwimmingChangeSound.Release;

  if SwimmingSound <> nil then
    SwimmingSound.Release;

  inherited;
end;

function TPlayer.GetFlyingMode: boolean;
begin
  Result := (FlyingModeTimeOut > 0) and (not Dead) and (not Blocked);
end;

procedure TPlayer.FlyingModeTimeoutBegin(const TimeOut: Single);
begin
  if FlyingModeTimeOut <= 0 then
    Notifications.Show('You start flying');

  { It's possible that FlyingModeTimeoutBegin is called when
    FFlyingModeTimeOut is already > 0. In this case, we set
    FFlyingModeTimeOut to maximum of current FFlyingModeTimeOut and TimeOut
    --- i.e. the effect that will allow player to fly longer wins. }
  FlyingModeTimeOut := Max(FlyingModeTimeOut, TimeOut);
end;

procedure TPlayer.CancelFlying;
begin
  if FlyingMode then
  begin
    FlyingModeTimeOut := 0;
    Notifications.Show('You''re no longer flying');
  end;
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
end;

function TPlayer.DropItem(const ItemIndex: Integer): TInventoryItem;
var
  S: string;
begin
  Result := Inventory.Drop(ItemIndex);

  if Result = EquippedWeapon then
    EquippedWeapon := nil;

  S := Format('You drop "%s"', [Result.Kind.Caption]);
  if Result.Quantity <> 1 then
    S += Format(' (quantity %d)', [Result.Quantity]);
  Notifications.Show(S);

  SoundEngine.Sound(stPlayerDropItem);
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
  Camera.Gravity := (not FlyingMode) and (not Blocked);
  { Note that when not Camera.Gravity then FallingDownEffect will not
    work anyway. }
  Camera.FallingDownEffect := Swimming = psNo;

(* TODO: keep it as local in TPlayer? *)
  Camera.MouseLookHorizontalSensitivity := MouseLookHorizontalSensitivity;
  Camera.MouseLookVerticalSensitivity := MouseLookVerticalSensitivity;
  Camera.InvertVerticalMouseLook := InvertVerticalMouseLook;

  { MouseLook is allowed always, even when player is dead.
    Just like rotation keys.

    Note that when Blocked, rotating will actually
    be disabled by Input := []. But still mouse look will cause mouse
    to remain hidden, which is good (why pop the mouse cursor on game
    win animation?). }
  Camera.MouseLook := UseMouseLook;

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

    Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
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

    Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
    Camera.HeadBobbing := 0.0;
    Camera.PreferredHeight := Camera.Radius * 1.01;

    Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
    Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
  end else
  begin
    if FlyingMode then
    begin
      Camera.PreferGravityUpForMoving := false;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.MakeClear;
      Camera.Input_Crouch.MakeClear;
      Camera.Input_UpMove.Assign(PlayerInput_UpMove, false);
      Camera.Input_DownMove.Assign(PlayerInput_DownMove, false);

      { Camera.HeadBobbing and
        Camera.PreferredHeight and
        Camera.FallingDownStartSpeed and
        Camera.FallingDownSpeedIncrease
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

      Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed / 6;
      Camera.FallingDownSpeedIncrease := 1.0;
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

      Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
      Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
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
  FootstepsSound.OnRelease := nil;
  FootstepsSound := nil;
  FootstepsSoundPlaying := stNone;
end;

procedure TPlayer.SwimmingChangeSoundRelease(Sender: TSound);
begin
  Assert(Sender = SwimmingChangeSound);
  SwimmingChangeSound.OnRelease := nil;
  SwimmingChangeSound := nil;
end;

procedure TPlayer.SwimmingSoundRelease(Sender: TSound);
begin
  Assert(Sender = SwimmingSound);
  SwimmingSound.OnRelease := nil;
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
      if World.WaterBox.PointInside(Position) then
        NewSwimming := psUnderWater else
      if World.WaterBox.PointInside(Position - Camera.PreferredHeight) then
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
          SetLifeCustomBlackout(Life - (GroundProperties.LavaDamageConst +
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
  BlackOutSpeed = 2.0;
begin
  inherited;
  if not GetExists then Exit;
  LifeTime += CompSpeed;

  if FlyingMode then
  begin
    FlyingModeTimeOut := FlyingModeTimeOut - CompSpeed;
    if not FlyingMode then
    begin
      Notifications.Show('You''re no longer flying');
    end;
  end;

  UpdateCamera;

  UpdateSwimming;

  if BlackOutIntensity > 0 then
    BlackOutIntensity -= BlackOutSpeed * CompSpeed;

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

procedure TPlayer.BlackOut(const Color: TVector3f);
begin
  BlackOutColor := Color;
  BlackOutIntensity := 1;
end;

procedure TPlayer.RedOut;
begin
  BlackOut(Red3Single);
end;

procedure TPlayer.FalledDown(Camera: TWalkCamera;
  const FallenHeight: Single);
begin
  if (Swimming = psNo) and (FallenHeight > 4.0) then
  begin
    SoundEngine.Sound(stPlayerFalledDown);
    if FallenHeight > Camera.MaxJumpDistance * 1.5 then
      Life := Life - Max(0, FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));
  end;
end;

procedure TPlayer.SetLifeCustomBlackOut(const Value: Single;
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
    BlackOut(Color);
    SoundEngine.Sound(stPlayerSuddenPain);
  end;
  inherited SetLife(Value);
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  SetLifeCustomBlackOut(Value, Red3Single);
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
        with the high speed (because in the air FallingDownStartSpeed
        is high and it's increased, but in the water it's much lower
        and not increased at all right now). }
      Camera.CancelFallingDown;
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
      Camera.FallingDownStartSpeed to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Camera.CancelFallingDown above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFallingDown ! So next falling down definitely *can*
      happen before next Player.Idle. Actually we may be in the middle
      of falling down right now. Fortunately Camera.Idle
      and Camera.CancelFallingDown are implemented (or rather fixed :)
      to honour calling CancelFallingDown and setting FallingDownStartSpeed now.

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

{TODO: this should disappear, we should depend on T3DOrient doing this}
procedure TPlayer.Translate(const T: TVector3Single);
begin
  Camera.Position := Camera.Position + T;
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
  AttackAnim: TCastlePrecalculatedAnimation;
begin
  Result := nil;
  if (EquippedWeapon <> nil) and
    EquippedWeapon.Kind.Prepared then
  begin
    AttackAnim := EquippedWeapon.Kind.AttackAnimation;
    AttackTime := LifeTime - AttackStartTime;
    if Attacking and (AttackTime <= AttackAnim.TimeEnd) then
    begin
      Result := AttackAnim.SceneFromTime(AttackTime);
    end else
    begin
      { turn off Attacking, if AttackTime passed }
      Attacking := false;
      { although current weapons animations are just static,
        we use LifeTime to enable any weapon animation
        (like weapon swaying, or some fire over the sword or such) in the future. }
      Result :=  EquippedWeapon.Kind.ReadyAnimation.SceneFromTime(LifeTime);
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

{ TConfigOptions ------------------------------------------------------------- }

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  MouseLookHorizontalSensitivity := Config.GetFloat(
    'mouse/horizontal_sensitivity', DefaultMouseLookHorizontalSensitivity);
  MouseLookVerticalSensitivity := Config.GetFloat(
    'mouse/vertical_sensitivity', DefaultMouseLookVerticalSensitivity);
  UseMouseLook := Config.GetValue(
    'mouse/use_mouse_look', DefaultUseMouseLook);
  InvertVerticalMouseLook := Config.GetValue(
    'mouse/invert_vertical_mouse_look', DefaultInvertVerticalMouseLook);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteFloat('mouse/horizontal_sensitivity',
    MouseLookHorizontalSensitivity, DefaultMouseLookHorizontalSensitivity);
  Config.SetDeleteFloat('mouse/vertical_sensitivity',
    MouseLookVerticalSensitivity, DefaultMouseLookVerticalSensitivity);
  Config.SetDeleteValue('mouse/use_mouse_look',
    UseMouseLook, DefaultUseMouseLook);
  Config.SetDeleteValue('mouse/invert_vertical_mouse_look',
    InvertVerticalMouseLook, DefaultInvertVerticalMouseLook);
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
