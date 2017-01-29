{
  Copyright 2006-2017 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses CastleBoxes, CastleCameras, CastleItems, CastleVectors, CastleInputs,
  CastleKeysMouse, X3DTriangles, CastleMaterialProperties, CastleSoundEngine,
  Classes, Castle3D, CastleGLUtils, CastleColors, CastleFrustum, CastleTriangles,
  CastleTimeUtils;

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
    call camera events like TCamera.Press, TCamera.Update and such.)
  }
  TPlayer = class(T3DAliveWithInventory)
  private
    FEquippedWeapon: TItemWeapon;

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

    SwimmingChangeSound: TSound;
    SwimmingSound: TSound;

    { Did last @link(Update) detected that we are on the ground. }
    IsOnTheGround: boolean;
    { <> @nil if IsOnTheGround and last ground had some TMaterialProperty. }
    GroundProperty: TMaterialProperty;
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
    FFadeOutColor: TCastleColor;

    FFallMinHeightToSound: Single;
    FFallMinHeightToDamage: Single;
    FFallDamageScaleMin: Single;
    FFallDamageScaleMax: Single;
    FFallSound: TSoundType;
    FHeadBobbing: Single;
    FSwimBreath: Single;
    FDrownPause: Single;
    FDrownDamageConst: Single;
    FDrownDamageRandom: Single;
    FSwimSoundPause: Single;
    FEnableCameraDragging: boolean;
    FFallingEffect: boolean;

    procedure SetEquippedWeapon(Value: TItemWeapon);

    { Update Camera properties, including inputs.
      Call this always when @link(Flying) or @link(Dead) or some key values
      or @link(Swimming) or @link(Blocked) change. }
    procedure UpdateCamera;

    procedure CameraFall(ACamera: TWalkCamera; const FallHeight: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color).
      Color's alpha doesn't matter for now. }
    procedure SetLifeCustomFadeOut(const Value: Single;
      const Color: TCastleColor);

    procedure SwimmingChangeSoundRelease(Sender: TSound);
    procedure SwimmingSoundRelease(Sender: TSound);
    procedure SetSwimming(const Value: TPlayerSwimming);

    procedure FootstepsSoundRelease(Sender: TSound);
    procedure SetFlying(const AValue: boolean);
    procedure SetFlyingTimeOut(const AValue: TFloatTime);
    procedure SetEnableCameraDragging(const AValue: boolean);
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
      DefaultPlayerKnockBackSpeed = 20.0;
      DefaultSwimBreath = 30.0;
      DefaultDrownPause = 5.0;
      DefaultDrownDamageConst = 5.0;
      DefaultDrownDamageRandom = 10.0;
      DefaultSwimSoundPause = 3.11111111;

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
    function Middle: TVector3Single; override;

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

    { You should set this property as appropriate.
      This object will just use this property (changing it's Camera
      properties etc.). }
    property Swimming: TPlayerSwimming read FSwimming write SetSwimming;

    { Load various player properties from an XML file.
      Properties not specified in the indicated file will
      be reset to their default values.
      This is handy to use in a game to allow to configure player behavior
      by simply editing an XML file (instead of hacking code).

      Overloaded parameterless version reads from file
      @code(ApplicationData('player.xml')).

      Note that the indicated file may not exist, and it will not cause errors.
      Not existing file is equivalent to a file with everything set at default
      values.

      It is Ok to call this multiple times, at any moment.
      This way you can make some debug command to reload player.xml file,
      very useful to test various player properties without restarting the game.

      @groupBegin }
    procedure LoadFromFile;
    procedure LoadFromFile(const URL: string);
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
      default DefaultSickProjectionSpeed;

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
    { Controls head bobbing, but only when player is walking.
      See TWalkCamera.HeadBobbing for exact meaning of this.
      TPlayer.Camera.HeadBobbing is automatically updated as necessary.

      Note that when using CastleLevels, then the headBobbing defined
      inside VRML/X3D file (see
      http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_head_bobbing )
      is ignored. Instead, Player properties control TWalkCamera.HeadBobbing
      and TWalkCamera.HeadBobbingTime. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing default TWalkCamera.DefaultHeadBobbing;

    { How many seconds you can swin before you start to drown. }
    property SwimBreath: Single read FSwimBreath write FSwimBreath default DefaultSwimBreath;

    { How many seconds between each drown event.
      Drown event makes stPlayerDrowning sound and causes damage
      DrownDamageConst + Random * DrownDamageRandom. }
    property DrownPause: Single read FDrownPause write FDrownPause default DefaultDrownPause;
    property DrownDamageConst: Single read FDrownDamageConst write FDrownDamageConst default DefaultDrownDamageConst;
    property DrownDamageRandom: Single read FDrownDamageRandom write FDrownDamageRandom default DefaultDrownDamageRandom;

    { Pause, in seconds, between playing stPlayerSwimming sound.
      This should be something that is not easily synchronized
      with SwimDrownPause. }
    property SwimSoundPause: Single read FSwimSoundPause write FSwimSoundPause default DefaultSwimSoundPause;

    { Enable camera falling down effect due to gravity.
      This indirectly controls @link(TWalkCamera.FallingEffect)
      underneath.

      Note: do not set @code(Camera.FallingEffect), as it will
      be overridden in our update. Use only this property to turn on/off
      the effect. }
    property FallingEffect: boolean
      read FFallingEffect write FFallingEffect default true;
  published
    property KnockBackSpeed default DefaultPlayerKnockBackSpeed;

    { Enable camera navigation by dragging. This results in including
      ciMouseDragging in TCamera.Input (when player is not
      @link(Dead) or @link(Blocked)). }
    property EnableCameraDragging: boolean
      read FEnableCameraDragging write SetEnableCameraDragging default true;
  end;

const
  DefaultAutoOpenInventory = true;

var
  { Automatically open TCastlePlayer inventory when picking up an item. }
  AutoOpenInventory: boolean = DefaultAutoOpenInventory;

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
  PlayerInput_Jump: TInputShortcut;
  PlayerInput_Crouch: TInputShortcut;

implementation

uses Math, SysUtils, CastleClassUtils, CastleUtils, X3DNodes, CastleControls,
  CastleImages, CastleFilesUtils, CastleUIControls,
  CastleOpenAL, CastleGL, CastleGLBoxes,
  CastleGameNotifications, CastleXMLConfig, CastleGLImages, CastleConfig,
  CastleResources, CastleShapes, CastleRenderingCamera;

{ TPlayerBox ----------------------------------------------------------------- }

type
  { Invisible box, that is added to make TPlayer collidable thanks to default
    T3DOrient (actually T3DList) methods. Owner must be TPlayer. }
  TPlayerBox = class(T3D)
  public
    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
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
    Result.Data[0, 2] := -Camera.Radius;
    Result.Data[0, World.GravityCoordinate] := -Camera.RealPreferredHeight;

    Result.Data[1, 0] := Camera.Radius;
    Result.Data[1, 1] := Camera.Radius;
    Result.Data[1, 2] := Camera.Radius;
  end else
    Result := EmptyBox3D;
end;

procedure TPlayerBox.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  inherited;

  {$ifndef OpenGLES} // TODO-es
  if RenderDebug3D and GetExists and
    Frustum.Box3DCollisionPossibleSimple(BoundingBox) and
    (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    { This code uses a lot of deprecated stuff. It is already marked with TODO above. }
    {$warnings off}
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glPushMatrix;
        glMultMatrix(Params.RenderTransform);
        glColorv(Gray);
        glDrawBox3DWire(BoundingBox);
      glPopMatrix;
    glPopAttrib;
    {$warnings on}
  end;
  {$endif}
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
  KnockBackSpeed := DefaultPlayerKnockBackSpeed;
  FSickProjectionSpeed := DefaultSickProjectionSpeed;
  FSwimBreath := DefaultSwimBreath;
  FDrownPause := DefaultDrownPause;
  FDrownDamageConst := DefaultDrownDamageConst;
  FDrownDamageRandom := DefaultDrownDamageRandom;
  FSwimSoundPause := DefaultSwimSoundPause;
  FFallingEffect := true;
  FEnableCameraDragging := true;

  Add(TPlayerBox.Create(Self));

  FInventoryCurrentItem := -1;

  { turn off keys that are totally unavailable for the player }
  Camera.Input_MoveSpeedInc.MakeClear;
  Camera.Input_MoveSpeedDec.MakeClear;
  Camera.Input_IncreasePreferredHeight.MakeClear;
  Camera.Input_DecreasePreferredHeight.MakeClear;

  Camera.CheckModsDown := false;
  Camera.OnFall := @CameraFall;

  { Although it will be called in every OnUpdate anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Camera has already good values. }
  UpdateCamera;
end;

destructor TPlayer.Destroy;
begin
  EquippedWeapon := nil; { unregister free notification }

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

function TPlayer.PickItemUpdate(var Item: TInventoryItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Resource.Caption]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  Notifications.Show(S);

  SoundEngine.Sound(stPlayerPickItem);

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
      S += Format(' (quantity %d)', [Result.Item.Quantity]);
    Notifications.Show(S);

    SoundEngine.Sound(stPlayerDropItem);

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

procedure TPlayer.UpdateCamera;
var
  NormalCameraInput: TCameraInputs;
begin
  Camera.Gravity := (not Blocked) and (not Flying);
  { Note that when not Camera.Gravity then FallingEffect will not
    work anyway. }
  Camera.FallingEffect := FallingEffect and (Swimming = psNo);

  if Blocked then
  begin
    { When Blocked, we navigate camera by code. }
    Camera.Input := [];
  end else
  begin
    NormalCameraInput := [ciNormal, ci3dMouse];
    if EnableCameraDragging and not Dead then
      Include(NormalCameraInput, ciMouseDragging);

    Camera.Input := NormalCameraInput;

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

    Camera.FallSpeedStart := TWalkCamera.DefaultFallSpeedStart;
    Camera.FallSpeedIncrease := TWalkCamera.DefaultFallSpeedIncrease;
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

    Camera.Input_Forward.MakeClear;
    Camera.Input_Backward.MakeClear;
    Camera.Input_LeftStrafe.MakeClear;
    Camera.Input_RightStrafe.MakeClear;

    Camera.FallSpeedStart := TWalkCamera.DefaultFallSpeedStart;
    Camera.FallSpeedIncrease := TWalkCamera.DefaultFallSpeedIncrease;
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

      Camera.FallSpeedStart := TWalkCamera.DefaultFallSpeedStart / 6;
      Camera.FallSpeedIncrease := 1.0;
      Camera.HeadBobbing := 0.0;
      Camera.PreferredHeight := Camera.Radius * 1.01;

      Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed / 2;
      Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed / 2;
    end else
    begin
      Camera.PreferGravityUpForMoving := true;
      Camera.PreferGravityUpForRotations := true;

      Camera.FallSpeedStart := TWalkCamera.DefaultFallSpeedStart;
      Camera.FallSpeedIncrease := TWalkCamera.DefaultFallSpeedIncrease;
      Camera.HeadBobbing := HeadBobbing;
      Camera.PreferredHeight := DefaultPreferredHeight;

      Camera.MoveHorizontalSpeed := DefaultMoveHorizontalSpeed;
      Camera.MoveVerticalSpeed := DefaultMoveVerticalSpeed;
    end;

    Camera.Input_Jump.Assign(PlayerInput_Jump, false);
    Camera.Input_Crouch.Assign(PlayerInput_Crouch, false);
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

procedure TPlayer.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  { Perform various things related to player swimming. }
  procedure UpdateSwimming;
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
        if LifeTime - SwimBeginTime > SwimBreath then
        begin
          if (SwimLastDrownTime = 0.0) or
             (LifeTime - SwimLastDrownTime > DrownPause) then
          begin
            if SwimLastDrownTime = 0.0 then
              Notifications.Show('You''re drowning');
            SwimLastDrownTime := LifeTime;
            Life := Life - (DrownDamageConst + Random * DrownDamageRandom);
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
           (LifeTime - SwimLastSoundTime > SwimSoundPause) ) then
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
      if Ground <> nil then
        GroundProperty := (Ground^.Shape as TShape).MaterialProperty else
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
    NewIsToxic := (GroundProperty <> nil) and GroundProperty.Toxic;
    if NewIsToxic then
    begin
      if (not IsToxic) or
         (LifeTime - ToxicLastDamageTime > GroundProperty.ToxicDamageTime) then
      begin
        ToxicLastDamageTime := LifeTime;
        if not Dead then
        begin
          SoundEngine.Sound(stPlayerToxicPain);
          SetLifeCustomFadeOut(Life - (GroundProperty.ToxicDamageConst +
            Random * GroundProperty.ToxicDamageRandom), Green);
        end;
      end;
    end;
    IsToxic := NewIsToxic;
  end;

  { Update FootstepsSoundPlaying and related variables.
    Must be called after UpdateIsOnTheGround (depends on GroundProperty). }
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
        GroundProperty field. }
      if (GroundProperty <> nil) and
         (GroundProperty.FootstepsSound <> stNone) then
        NewFootstepsSoundPlaying := GroundProperty.FootstepsSound else
        NewFootstepsSoundPlaying := stPlayerFootstepsDefault;
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
        in footsteps, due to the delay between SoundEngine.Refresh calls,
        it's very short pause, but it's noticeable,
        since footsteps should be rhytmic). I prefer to not rely
        on SoundEngine.Refresh for this and instead just check this here. }
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

  if FFlyingTimeOut > 0 then
  begin
    FFlyingTimeOut := FFlyingTimeOut - SecondsPassed;
    if FFlyingTimeOut <= 0 then
    begin
      FFlyingTimeOut := 0;
      FFlying := false;
    end;
  end;

  UpdateCamera;

  UpdateSwimming;

  if FFadeOutIntensity > 0 then
    FFadeOutIntensity -= FadeOutSpeed * SecondsPassed;

  if EquippedWeapon <> nil then
    EquippedWeapon.EquippedUpdate(LifeTime);

  UpdateIsOnTheGround;
  UpdateToxic;
  UpdateFootstepsSoundPlaying;
end;

procedure TPlayer.FadeOut(const Color: TCastleColor);
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
  const Color: TCastleColor);
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
  SetLifeCustomFadeOut(Value, Red);
  { cancel flying when dead }
  if Dead then
    Flying := false;
end;

procedure TPlayer.Attack;
begin
  if EquippedWeapon <> nil then
    EquippedWeapon.EquippedAttack(LifeTime) else
    { TODO: allow to do some "punch" / "kick" here easily }
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

    { Although UpdateCamera will be called in nearest Player.Update anyway,
      I want to call it *now*. That's because I want to set
      Camera.FallSpeedStart to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Camera.CancelFalling above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFalling ! So next falling down definitely *can*
      happen before next Player.Update. Actually we may be in the middle
      of falling down right now. Fortunately Camera.Update
      and Camera.CancelFalling are implemented (or rather fixed :)
      to honour calling CancelFalling and setting FallSpeedStart now.

      So the safeguard below is needed. }
    UpdateCamera;
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
    Camera.JumpMaxHeight := Config.GetFloat('jump/max_height', TWalkCamera.DefaultJumpMaxHeight);
    Camera.JumpHorizontalSpeedMultiply := Config.GetFloat('jump/horizontal_speed_multiply', TWalkCamera.DefaultJumpHorizontalSpeedMultiply);
    Camera.JumpTime := Config.GetFloat('jump/time', TWalkCamera.DefaultJumpTime);
    Camera.HeadBobbingTime := Config.GetFloat('head_bobbing_time', TWalkCamera.DefaultHeadBobbingTime);
    HeadBobbing := Config.GetFloat('head_bobbing', TWalkCamera.DefaultHeadBobbing);
    SickProjectionSpeed := Config.GetFloat('sick_projection_speed', DefaultSickProjectionSpeed);
    FallMinHeightToSound := Config.GetFloat('fall/sound/min_height', DefaultPlayerFallMinHeightToSound);
    FallMinHeightToDamage := Config.GetFloat('fall/damage/min_height', DefaultFallMinHeightToDamage);
    FallDamageScaleMin := Config.GetFloat('fall/damage/scale_min', DefaultFallDamageScaleMin);
    FallDamageScaleMax := Config.GetFloat('fall/damage/scale_max', DefaultFallDamageScaleMax);
    FallSound := SoundEngine.SoundFromName(Config.GetValue('fall/sound/name', DefaultPlayerFallSoundName), false);
    FSwimBreath := Config.GetFloat('swim/breath', DefaultSwimBreath);
    FSwimSoundPause := Config.GetFloat('swim/sound_pause', DefaultSwimSoundPause);
    FDrownPause := Config.GetFloat('drown/pause', DefaultDrownPause);
    FDrownDamageConst := Config.GetFloat('drown/damage/const', DefaultDrownDamageConst);
    FDrownDamageRandom := Config.GetFloat('drown/damage/random', DefaultDrownDamageRandom);
  finally FreeAndNil(Config); end;
end;

procedure TPlayer.LoadFromFile;
begin
  LoadFromFile(ApplicationData('player.xml'));
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
  GroundProperty := nil;

  IsToxic := false;
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
begin
  if EquippedWeapon <> nil then
    Result := EquippedWeapon.EquippedScene(LifeTime) else
    Result := nil;
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
    changing DepthRange (except shadow maps that require normal DepthRange,
    and manually push/pop the DepthRange state).

    - The first frame with TPlayer could be incorrect, as 3D objects drawn before
      will have 0..1 DepthRange that may overlap with our weapon.
      It works now only because default player positions are when
      the weapon doesn't overlap with level in 3D.
    - We never fix the DepthRange back to 0..1.

    The idea of using DepthRange for layers seems quite good, it's
    quite a nice solution,
    - you don't have to split rendering layers in passes, you can render
      all objects in one pass, just switching DepthRange as necessary.
    - you can set DepthRange for 3D objects inside T3DList,
      like here TPlayer will just affect every child underneath.

    But it has to be implemented in more extensible manner in the future.
    It should also enable X3D layers. }

  if RenderOnTop and (RenderingCamera.Target <> rtShadowMap) then
    DepthRange := drNear;

  inherited;

  if RenderOnTop and (RenderingCamera.Target <> rtShadowMap) then
    DepthRange := drFar;
end;

function TPlayer.Middle: TVector3Single;
begin
  { For player, our Position is already the suitable "eye position"
    above the ground.

    Note that Player.Gravity remains false for now (only Player.Camera.Gravity
    is true), so the player is not affected by simple gravity implemented in
    Castle3D unit, so there's no point in overriding methods like PreferredHeight.
    TWalkCamera.Gravity does all the work now. }

  Result := Position;
end;

procedure TPlayer.SetEnableCameraDragging(const AValue: boolean);
begin
  if FEnableCameraDragging <> AValue then
  begin
    FEnableCameraDragging := AValue;
    UpdateCamera;
  end;
end;

initialization
  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  PlayerInput_Forward := TInputShortcut.Create(nil, 'Move forward', 'move_forward', igBasic);
  PlayerInput_Forward.Assign(K_W, K_Up);
  PlayerInput_Backward := TInputShortcut.Create(nil, 'Move backward', 'move_backward', igBasic);
  PlayerInput_Backward.Assign(K_S, K_Down);
  PlayerInput_LeftRot := TInputShortcut.Create(nil, 'Turn left', 'turn_left', igBasic);
  PlayerInput_LeftRot.Assign(K_Left);
  PlayerInput_RightRot := TInputShortcut.Create(nil, 'Turn right', 'turn_right', igBasic);
  PlayerInput_RightRot.Assign(K_Right);
  PlayerInput_LeftStrafe := TInputShortcut.Create(nil, 'Move left', 'move_left', igBasic);
  PlayerInput_LeftStrafe.Assign(K_A);
  PlayerInput_RightStrafe := TInputShortcut.Create(nil, 'Move right', 'move_right', igBasic);
  PlayerInput_RightStrafe.Assign(K_D);
  PlayerInput_UpRotate := TInputShortcut.Create(nil, 'Look up', 'look_up', igBasic);
  PlayerInput_UpRotate.Assign(K_None);
  PlayerInput_DownRotate := TInputShortcut.Create(nil, 'Look down', 'look_down', igBasic);
  PlayerInput_DownRotate.Assign(K_None);
  PlayerInput_GravityUp := TInputShortcut.Create(nil, 'Look straight', 'look_straight', igBasic);
  PlayerInput_GravityUp.Assign(K_None);
  PlayerInput_Jump := TInputShortcut.Create(nil, 'Jump (or fly/swim up)', 'move_up', igBasic);
  PlayerInput_Jump.Assign(K_Space);
  PlayerInput_Crouch := TInputShortcut.Create(nil, 'Crouch (or fly/swim down)', 'move_down', igBasic);
  PlayerInput_Crouch.Assign(K_C);
end.
