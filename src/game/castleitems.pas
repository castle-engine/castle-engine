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

{ Items, things that can be picked up, carried and used. }
unit CastleItems;

interface

uses Boxes3D, X3DNodes, CastleScene, VectorMath, CastleUtils,
  CastleClassUtils, Classes, CastleImages, GL, GLU, CastleGLUtils,
  PrecalculatedAnimation, CastleResources, GLImages,
  CastleXMLConfig, CastleSoundEngine, Frustum, Base3D, FGL, CastleColors;

const
  DefaultWeaponDamageConst = 5.0;
  DefaultWeaponDamageRandom = 5.0;
  DefaultWeaponActualAttackTime = 0.0;
  DefaultWeaponAttackKnockbackDistance = 1.0;
  DefaultItemRotationSpeed = Pi;

type
  TInventoryItem = class;
  TInventoryItemClass = class of TInventoryItem;

  { Kind of item. }
  TItemKind = class(T3DResource)
  private
    FSceneFileName: string;
    FScene: TCastleScene;
    FCaption: string;
    FImageFileName: string;
    FImage: TCastleImage;
    FGLImage: TGLImage;
    FBoundingBoxRotated: TBox3D;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
    { Which TInventoryItem descendant to create when constructing item
      of this kind by CreateItem. }
    function ItemClass: TInventoryItemClass; virtual;
  public
    destructor Destroy; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    property SceneFileName: string read FSceneFileName;

    { Nice caption to display. }
    property Caption: string read FCaption;

    { Note that the Scene is nil if not Prepared. }
    function Scene: TCastleScene;

    { This is a 2d image, to be used for inventory slots etc.
      When you call this for the 1st time, the image will be loaded
      from ImageFileName.

      @noAutoLinkHere }
    function Image: TCastleImage;

    property ImageFileName: string read FImageFileName;

    { OpenGL resource to draw @link(Image). }
    function GLImage: TGLImage;

    { The largest possible bounding box of the 3D item,
      taking into account that actual item 3D model will be rotated when
      placed on world. You usually want to add current item position to this.

      Note that this assumes that initial item Scene does not animate.
      If it animates, possibly the actual bounding box will get larger,
      we don't account for it here (now). }
    property BoundingBoxRotated: TBox3D read FBoundingBoxRotated;

    { Create item. This is how you should create new TInventoryItem instances.
      It is analogous to TCreatureKind.CreateCreature, but now for items.

      Note that the item itself doesn't exist on a 3D world --- you have to
      put it there if you want by TInventoryItem.PutOnWorld. That is because items
      can also exist only in player's backpack and such, and then they
      are independent from 3D world.

      @bold(Examples:)

      You usually define your own item kinds by adding a subdirectory with
      resource.xml file to your game data. See README_about_index_xml_files.txt
      and engine tutorial for examples how to do this. Then you load the item
      kinds with

@longCode(#
var
  Sword: TItemKind;
...
  Resources.LoadFromFiles;
  Sword := Resources.FindName('Sword') as TItemKind;
#)

      where 'Sword' is just our example item kind, assuming that one of your
      resource.xml files has resource with name="Sword".

      Now if you want to add the sword to your 3D world by code:

      Because TInventoryItem instance is automatically
      owned (freed) by the 3D world or inventory that contains it, a simplest
      example how to add an item to your 3D world is this:

@longCode(#
  Sword.CreateItem(1).PutOnWorld(SceneManager.World, Vector3Single(2, 3, 4));
#)

      This adds 1 item of the MyItemKind to the 3D world,
      on position (2, 3, 4). In simple cases you can get SceneManager
      instance from TCastleWindow.SceneManager or TCastleControl.SceneManager.

      If you want to instead add sword to the inventory of Player,
      you can call

@longCode(#
  SceneManager.Player.PickItem(Sword.CreateItem(1));
#)

      This assumes that you use SceneManager.Player property. It's not really
      obligatory, but it's the simplest way to have player with an inventory.
      See engine tutorial for examples how to create player.
      Anyway, if you have any TInventory instance, you can use
      TInventory.Pick to add TInventoryItem this way. }
    function CreateItem(const AQuantity: Cardinal): TInventoryItem;

    { Instantiate placeholder by create new item with CreateItem
      and putting it on level with TInventoryItem.PutOnWorld. }
    procedure InstantiatePlaceholder(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
      const NumberPresent: boolean; const Number: Int64); override;

    function AlwaysPrepared: boolean; override;
  end;

  TItemWeaponKind = class(TItemKind)
  private
    FEquippingSound: TSoundType;
    FAttackAnimation: TCastlePrecalculatedAnimation;
    FAttackAnimationFile: string;
    FReadyAnimation: TCastlePrecalculatedAnimation;
    FReadyAnimationFile: string;
    FActualAttackTime: Single;
    FSoundAttackStart: TSoundType;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
    function ItemClass: TInventoryItemClass; override;
  public
    { Sound to make on equipping. Each weapon can have it's own
      equipping sound. }
    property EquippingSound: TSoundType
      read FEquippingSound write FEquippingSound;

    { Animation of attack with this weapon. TimeBegin must be 0. }
    property AttackAnimation: TCastlePrecalculatedAnimation
      read FAttackAnimation;

    { Animation of keeping weapon ready. }
    property ReadyAnimation: TCastlePrecalculatedAnimation
      read FReadyAnimation;

    { Time within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime
      default DefaultWeaponActualAttackTime;

    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TItemShortRangeWeaponKind = class(TItemWeaponKind)
  private
    FDamageConst: Single;
    FDamageRandom: Single;
    FAttackKnockbackDistance: Single;
  public
    constructor Create(const AName: string); override;

    property DamageConst: Single read FDamageConst write FDamageConst
      default DefaultWeaponDamageConst;
    property DamageRandom: Single read FDamageRandom write FDamageRandom
      default DefaultWeaponDamageRandom;
    property AttackKnockbackDistance: Single
      read FAttackKnockbackDistance write FAttackKnockbackDistance
      default DefaultWeaponAttackKnockbackDistance;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TItemOnWorld = class;

  { An item that can be used, kept in the inventory, or (using PutOnWorld
    that wraps it in TItemOnWorld) dropped on 3D world.
    Thanks to the @link(Quantity) property, this may actually represent
    many "stacked" items, all having the same properties. }
  TInventoryItem = class(TComponent)
  private
    FKind: TItemKind;
    FQuantity: Cardinal;
    FOwner3D: T3D;
  protected
    { Try to sum (stack) the given Item with current TInventoryItem.
      The idea is that if player has 5 arrows, and picks an item representing
      another 5 arrows, then we sum then into one TInventoryItem instance
      representing 10 arrows.

      Various games, and various items,
      may require various approaches: for example, maybe you don't want
      some items to stack at all (e.g. you want to only allow stacking
      for items naturally appearing in vast quantities, like arrows and bolts
      and bullets (if you represent them as TInventoryItem at all)).
      Maybe you want to allow stacking only to centain number, e.g. arrows
      are summed into groups of maximum 20 items, anything above creates new stack?
      Overriding this procedure allows you to do all this.

      You can here increase the Quantity of current item,
      decrease the Quantity of parameter Item. In case the parameter Item
      no longer exists (it's Quantity reaches 0) you have to free it
      and set to @nil the Item parameter, in practice you usually want
      to call then @code(FreeAndNil(Item)).

      The default implementation of this in TInventoryItem class
      allows stacking always, as long as the Kind matches.
      This means that, by default, every TItemKind is exsisting at most once
      in TPlayer.Inventory. }
    procedure Stack(var Item: TInventoryItem); virtual;
  public
    property Kind: TItemKind read FKind;

    { Quantity of this item.
      This must always be >= 1. }
    property Quantity: Cardinal read FQuantity write FQuantity;

    { Splits item (with Quantity >= 2) into two items.
      It returns newly created object with the same properties
      as this object, and with Quantity set to QuantitySplit.
      And it lowers our Quantity by QuantitySplit.

      Always QuantitySplit must be >= 1 and < Quantity. }
    function Split(QuantitySplit: Cardinal): TInventoryItem;

    { Create TItemOnWorld instance referencing this item,
      and add this to the given 3D AWorld.
      Although normal item knows (through Owner3D) the world it lives in,
      but this method may be used for items that don't have an owner yet,
      so we take AWorld parameter explicitly.
      This is how you should create new TItemOnWorld instances.
      It is analogous to TCreatureKind.CreateCreature, but now for items. }
    function PutOnWorld(const AWorld: T3DWorld;
      const APosition: TVector3Single): TItemOnWorld;

    { Use this item.

      In this class, this just prints a message "this item cannot be used".

      Implementation of this method can assume for now that this is one of
      player's owned Items. Implementation of this method can change
      our properties, including Quantity.
      As a very special exception, implementation of this method
      is allowed to set Quantity of Item to 0.

      Never call this method when Player.Dead. Implementation of this
      method may assume that Player is not Dead.

      Caller of this method should always be prepared to immediately
      handle the "Quantity = 0" situation by freeing given item,
      removing it from any list etc. }
    procedure Use; virtual;

    { 3D owner of the item,
      like a player or creature (if the item is in the backpack)
      or the TItemOnWorld instance (if the item is lying on the world,
      pickable). May be @nil only in special situations (when item is moved
      from one 3D to another, and technically it's safer to @nil this
      property).

      The owner is always responsible for freeing this TInventoryItem instance
      (in case of TItemOnWorld, it does it directly;
      in case of player or creature, it does it by TInventory). }
    property Owner3D: T3D read FOwner3D;

    { 3D world of this item, if any.

      Although the TInventoryItem, by itself, is not a 3D thing
      (only pickable TItemOnWorld is a 3D thing).
      But it exists inside a 3D world: either as pickable (TItemOnWorld),
      or as being owned by a 3D object (like player or creature) that are
      part of 3D world. In other words, our Owner3D.World is the 3D world
      this item lives in. }
    function World: T3DWorld;
  end;

  TItemWeapon = class(TInventoryItem)
  public
    procedure Use; override;

    { Perform real attack now.
      This may mean hurting some creature within the range,
      or shooting some missile. You can also play some sound here. }
    procedure ActualAttack; virtual; abstract;

    function Kind: TItemWeaponKind;
  end;

  { List of items, with a 3D object (like a player or creature) owning
    these items. Do not directly change this list, always use
    @link(Pick) or @link(Drop) methods (they make sure that
    items are correctly stacked, that TInventoryItem.Owner3D and memory management
    is good). }
  TInventory = class(specialize TFPGObjectList<TInventoryItem>)
  private
    FOwner3D: T3DAlive;
  public
    constructor Create(const AOwner3D: T3DAlive);

    { Owner of the inventory (like a player or creature).
      Never @nil, always valid for given inventory.
      All items on this list always have the same TInventoryItem.Owner3D value
      as the inventory they are in. }
    property Owner3D: T3DAlive read FOwner3D;

    { Searches for item of given Kind. Returns index of first found,
      or -1 if not found. }
    function FindKind(Kind: TItemKind): Integer;

    { Add Item to Items. Because an item may be stacked with others,
      the actual Item instance may be freed and replaced with other by
      this method.
      Returns index to the added item.

      Using this method means that the memory management of the item
      becomes the responsibility of this list. }
    function Pick(var Item: TInventoryItem): Integer;

    { Drop item with given index.
      ItemIndex must be valid (between 0 and Items.Count - 1).
      You @italic(must) take care yourself of returned TInventoryItem memory
      management. }
    function Drop(const ItemIndex: Integer): TInventoryItem;

    { Pass here items owned by this list, immediately after decreasing
      their Quantity. This frees the item (removing it from the list)
      if it's quantity reached zero. }
    procedure CheckDepleted(const Item: TInventoryItem);

    { Use the item of given index. Calls TInventoryItem.Use, and then checks whether
      the item was depleted (and eventually removes it) by CheckDepleted. }
    procedure Use(const Index: Integer);
  end;

  { Item that is placed on a 3D world, ready to be picked up.
    It's not in anyone's inventory. }
  TItemOnWorld = class(T3DOrient)
  private
    FItem: TInventoryItem;
    Rotation: Single;
  protected
    function GetExists: boolean; override;
    function GetChild: T3D; override;
  public
    { Speed of the rotation of 3D item on world.
      In radians per second, default is DefaultItemRotationSpeed.
      Set to zero to disable rotation. }
    RotationSpeed: Single; static;

    { Does the player automatically picks up items by walking over them.
      Default is @true. If you set this to @false, you most probably want to
      implement some other way of picking up items, use the ExtractItem method. }
    AutoPick: boolean; static;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The Item owned by this TItemOnWorld instance. Never @nil. }
    property Item: TInventoryItem read FItem;

    { Render the item, on current Position with current rotation etc.
      Current matrix should be modelview, this pushes/pops matrix state
      (so it 1. needs one place on matrix stack,
      2. doesn't modify current matrix).

      Pass current viewing Frustum to allow optimizing this
      (when item for sure is not within Frustum, we don't have
      to push it to OpenGL). }
    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    property Collides default false;
    property CollidesWithMoving default true;
    function Middle: TVector3Single; override;

    { Extract the @link(Item), used when picking up the TInventoryItem
      instance referenced by this TItemOnWorld instance. This returns our
      @link(Item) property, and clears it (clearing also TInventoryItem.Owner3D).
      At the next @link(Idle), this TItemOnWorld instance will be freed
      and removed from 3D world.

      It's up to you what to do with resulting TInventoryItem instance.
      You can pick it up, by TPlayer.PickItem,
      or add it back to 3D world by TInventoryItem.PutOnWorld,
      or at least free it (or you'll get a memory leak). }
    function ExtractItem: TInventoryItem;

    function PreferredHeight: Single; override;
  end;

var
  { Global callback to control items on level existence. }
  OnItemOnWorldExists: T3DExistsEvent;

implementation

uses SysUtils, CastleFilesUtils, CastlePlayer, CastleGameNotifications,
  CastleConfig;

{ TItemKind ------------------------------------------------------------ }

destructor TItemKind.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TItemKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  FSceneFileName := KindsConfig.GetFileName('scene');
  FImageFileName := KindsConfig.GetFileName('image');

  FCaption := KindsConfig.GetValue('caption', '');
  if FCaption = '' then
    raise Exception.CreateFmt('Empty caption attribute for item "%s"', [Name]);
end;

function TItemKind.Scene: TCastleScene;
begin
  Result := FScene;
end;

function TItemKind.Image: TCastleImage;
begin
  if FImage = nil then
    FImage := LoadImage(ImageFileName, []);
  Result := FImage;
end;

function TItemKind.GLImage: TGLImage;
begin
  if FGLImage = nil then
    FGLImage := TGLImage.Create(Image);
  Result := FGLImage;
end;

procedure TItemKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;
  PrepareScene(FScene, SceneFileName, BaseLights, DoProgress);
  FBoundingBoxRotated :=
    Scene.BoundingBox.Transform(RotationMatrixDeg(45         , GravityUp)) +
    Scene.BoundingBox.Transform(RotationMatrixDeg(45 + 90    , GravityUp)) +
    Scene.BoundingBox.Transform(RotationMatrixDeg(45 + 90 * 2, GravityUp)) +
    Scene.BoundingBox.Transform(RotationMatrixDeg(45 + 90 * 3, GravityUp));
  { prepare GLImage now }
  GLImage;
end;

function TItemKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TItemKind.ReleaseCore;
begin
  FScene := nil;
  FreeAndNil(FGLImage);
  inherited;
end;

function TItemKind.CreateItem(const AQuantity: Cardinal): TInventoryItem;
begin
  Result := ItemClass.Create(nil { for now, TInventoryItem.Owner is always nil });
  { set properties that in practice must have other-than-default values
    to sensibly use the item }
  Result.FKind := Self;
  Result.FQuantity := AQuantity;
  Assert(Result.Quantity >= 1, 'Item''s Quantity must be >= 1');
end;

function TItemKind.ItemClass: TInventoryItemClass;
begin
  Result := TInventoryItem;
end;

procedure TItemKind.InstantiatePlaceholder(World: T3DWorld;
  const APosition, ADirection: TVector3Single;
  const NumberPresent: boolean; const Number: Int64);
var
  ItemQuantity: Cardinal;
begin
  { calculate ItemQuantity }
  if NumberPresent then
    ItemQuantity := Number else
    ItemQuantity := 1;

  CreateItem(ItemQuantity).PutOnWorld(World, APosition);
end;

function TItemKind.AlwaysPrepared: boolean;
begin
  Result := true;
end;

{ TItemWeaponKind ------------------------------------------------------------ }

procedure TItemWeaponKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;
  PreparePrecalculatedAnimation(FAttackAnimation, FAttackAnimationFile, BaseLights, DoProgress);
  PreparePrecalculatedAnimation(FReadyAnimation , FReadyAnimationFile , BaseLights, DoProgress);
end;

function TItemWeaponKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TItemWeaponKind.ReleaseCore;
begin
  FAttackAnimation := nil;
  FReadyAnimation := nil;
  inherited;
end;

procedure TItemWeaponKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  ActualAttackTime := KindsConfig.GetFloat('actual_attack_time',
    DefaultWeaponActualAttackTime);

  EquippingSound := SoundEngine.SoundFromName(
    KindsConfig.GetValue('equipping_sound', ''));
  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_attack_start', ''));

  FReadyAnimationFile:= KindsConfig.GetFileName('ready_animation');
  FAttackAnimationFile := KindsConfig.GetFileName('attack_animation');
end;

function TItemWeaponKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemWeapon;
end;

{ TItemShortRangeWeaponKind -------------------------------------------------- }

constructor TItemShortRangeWeaponKind.Create(const AName: string);
begin
  inherited;
  FDamageConst := DefaultWeaponDamageConst;
  FDamageRandom := DefaultWeaponDamageRandom;
  FAttackKnockbackDistance := DefaultWeaponAttackKnockbackDistance;
end;

procedure TItemShortRangeWeaponKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  DamageConst := KindsConfig.GetFloat('damage/const',
    DefaultWeaponDamageConst);
  DamageRandom := KindsConfig.GetFloat('damage/random',
    DefaultWeaponDamageRandom);
  AttackKnockbackDistance := KindsConfig.GetFloat('attack_knockback_distance',
    DefaultWeaponAttackKnockbackDistance);
end;

{ TInventoryItem ------------------------------------------------------------ }

procedure TInventoryItem.Stack(var Item: TInventoryItem);
begin
  if Item.Kind = Kind then
  begin
    { Stack Item with us }
    Quantity := Quantity + Item.Quantity;
    FreeAndNil(Item);
  end;
end;

function TInventoryItem.Split(QuantitySplit: Cardinal): TInventoryItem;
begin
  Check(Between(Integer(QuantitySplit), 1, Quantity - 1),
    'You must split >= 1 and less than current Quantity');

  Result := Kind.CreateItem(QuantitySplit);

  FQuantity -= QuantitySplit;
end;

function TInventoryItem.PutOnWorld(const AWorld: T3DWorld;
  const APosition: TVector3Single): TItemOnWorld;
begin
  Result := TItemOnWorld.Create(AWorld { owner });
  { set properties that in practice must have other-than-default values
    to sensibly use the item }
  Result.FItem := Self;
  FOwner3D := Result;
  Result.SetView(APosition, AnyOrthogonalVector(AWorld.GravityUp), AWorld.GravityUp);
  Result.Gravity := true;
  Result.FallSpeed := Kind.FallSpeed;
  Result.GrowSpeed := Kind.GrowSpeed;
  AWorld.Add(Result);
end;

procedure TInventoryItem.Use;
begin
  Notifications.Show('This item cannot be used');
end;

function TInventoryItem.World: T3DWorld;
begin
  if Owner3D <> nil then
    Result := Owner3D.World else
    Result := nil;
end;

{ TItemWeapon ---------------------------------------------------------------- }

procedure TItemWeapon.Use;
begin
  if (Owner3D <> nil) and
     (Owner3D is TPlayer) then
    TPlayer(Owner3D).EquippedWeapon := Self;
end;

function TItemWeapon.Kind: TItemWeaponKind;
begin
  Result := (inherited Kind) as TItemWeaponKind;
end;

{ TInventory ------------------------------------------------------------ }

constructor TInventory.Create(const AOwner3D: T3DAlive);
begin
  inherited Create(true);
  FOwner3D := AOwner3D;
end;

function TInventory.FindKind(Kind: TItemKind): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Kind = Kind then
      Exit;
  Result := -1;
end;

function TInventory.Pick(var Item: TInventoryItem): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    Items[Result].Stack(Item);
    if Item = nil then Break;
  end;

  if Item <> nil then
  begin
    Add(Item);
    Result := Count - 1;
  end else
    Item := Items[Result];

  Item.FOwner3D := Owner3D;
end;

function TInventory.Drop(const ItemIndex: Integer): TInventoryItem;
var
  SelectedItem: TInventoryItem;
  DropQuantity: Cardinal;
begin
  SelectedItem := Items[ItemIndex];

  { For now, always drop 1 item.
    This makes it independent from message boxes, and also suitable for
    items owned by creatures.

  if SelectedItem.Quantity > 1 then
  begin
    DropQuantity := SelectedItem.Quantity;

    if not MessageInputQueryCardinal(Window,
      Format('You have %d items "%s". How many of them do you want to drop ?',
        [SelectedItem.Quantity, SelectedItem.Kind.Caption]),
      DropQuantity, taLeft) then
      Exit(nil);

    if not Between(DropQuantity, 1, SelectedItem.Quantity) then
    begin
      Notifications.Show(Format('You cannot drop %d items', [DropQuantity]));
      Exit(nil);
    end;
  end else }
    DropQuantity := 1;

  if DropQuantity = SelectedItem.Quantity then
  begin
    Result := SelectedItem;
    Extract(SelectedItem); { Extract, not Remove, do not free }
  end else
  begin
    Result := SelectedItem.Split(DropQuantity);
  end;

  Result.FOwner3D := nil;
end;

procedure TInventory.CheckDepleted(const Item: TInventoryItem);
var
  Index: Integer;
begin
  if Item.Quantity = 0 then
  begin
    Index := IndexOf(Item);
    if Index <> -1 then
      Delete(Index);
  end;
end;

procedure TInventory.Use(const Index: Integer);
var
  Item: TInventoryItem;
begin
  Item := Items[Index];
  Item.Use;
  { CheckDepleted will search for new index, since using an item
    may change the items list and so change the index. }
  CheckDepleted(Item);
end;

{ TItemOnWorld ------------------------------------------------------------ }

constructor TItemOnWorld.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CollidesWithMoving := true;
  Gravity := true;

  { Items are not collidable, player can enter them to pick them up.
    For now, this also means that creatures can pass through them,
    which isn't really troublesome now. }
  Collides := false;
end;

destructor TItemOnWorld.Destroy;
begin
  FreeAndNil(FItem);
  inherited;
end;

function TItemOnWorld.GetChild: T3D;
begin
  if (Item = nil) or not Item.Kind.Prepared then Exit(nil);
  Result := Item.Kind.Scene;
end;

procedure TItemOnWorld.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
var
  BoxRotated: TBox3D;
begin
  inherited;
  if RenderDebug3D and GetExists and
    (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    BoxRotated := Item.Kind.BoundingBoxRotated.Translate(Position);
    if Frustum.Box3DCollisionPossibleSimple(BoxRotated) then
    begin
      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
        glColorv(Gray3Single);
        glDrawBox3DWire(BoundingBox);
        glDrawBox3DWire(BoxRotated);
        glColorv(Yellow3Single);
        glDrawAxisWire(Middle, BoxRotated.AverageSize(true, 0));
      glPopAttrib;
    end;
  end;
end;

procedure TItemOnWorld.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  DirectionZero, U: TVector3Single;
begin
  inherited;
  if not GetExists then Exit;

  Rotation += RotationSpeed * CompSpeed;
  U := World.GravityUp; // copy to local variable for speed
  DirectionZero := Normalized(AnyOrthogonalVector(U));
  SetView(RotatePointAroundAxisRad(Rotation, DirectionZero, U), U);

  if AutoPick and
     (World.Player <> nil) and
     (World.Player is TPlayer) and
     (not World.Player.Dead) and
     BoundingBox.Collision(World.Player.BoundingBox) then
    TPlayer(World.Player).PickItem(ExtractItem);

  { Since we cannot live with Item = nil, we free ourselves }
  if Item = nil then
    RemoveMe := rtRemoveAndFree;
end;

function TItemOnWorld.ExtractItem: TInventoryItem;
begin
  { We no longer own this Item, so clear references. }
  Result := Item;
  Result.FOwner3D := nil;
  FItem := nil;
end;

function TItemOnWorld.GetExists: boolean;
begin
  Result := (inherited GetExists) and
    ((not Assigned(OnItemOnWorldExists)) or OnItemOnWorldExists(Self));
end;

function TItemOnWorld.PreferredHeight: Single;
var
  B: TBox3D;
  GC: Integer;
begin
  B := Item.Kind.BoundingBoxRotated;
  if B.IsEmpty then
    Result := 0 else
  begin
    GC := World.GravityCoordinate;
    Result := (B.Data[1, GC] - B.Data[0, GC]) / 2;
  end;
end;

function TItemOnWorld.Middle: TVector3Single;
begin
  Result := (inherited Middle) + World.GravityUp * PreferredHeight;
end;

initialization
  TItemOnWorld.RotationSpeed := DefaultItemRotationSpeed;
  TItemOnWorld.AutoPick := true;
end.
