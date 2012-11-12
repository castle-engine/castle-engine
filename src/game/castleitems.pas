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

type
  TInventoryItem = class;
  TInventoryItemClass = class of TInventoryItem;

  { Kind of item. }
  TItemKind = class(T3DResource)
  private
    FBaseAnimation: T3DResourceAnimation;
    FCaption: string;
    FImageFileName: string;
    FImage: TCastleImage;
    FGLImage: TGLImage;
    FBoundingBoxRotated: TBox3D;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    procedure ReleaseCore; override;
    { Which TInventoryItem descendant to create when constructing item
      of this kind by CreateItem. }
    function ItemClass: TInventoryItemClass; virtual;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;

    { Nice caption to display. }
    property Caption: string read FCaption;

    property BaseAnimation: T3DResourceAnimation read FBaseAnimation;

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
      placed on world. You usually want to add current item position to this. }
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
    FAttackAnimation: T3DResourceAnimation;
    FReadyAnimation: T3DResourceAnimation;
    FAttackTime: Single;
    FAttackSoundStart: TSoundType;
    FAttackSoundHit: TSoundType;
    FAttackDamageConst: Single;
    FAttackDamageRandom: Single;
    FAttackKnockbackDistance: Single;
    FFireMissileName: string;
    FFireMissileAmmo: string;
    FFireMissileSound: TSoundType;
  protected
    function ItemClass: TInventoryItemClass; override;
  public
    const
      DefaultAttackTime = 0.0;
      DefaultAttackDamageConst = 0.0;
      DefaultAttackDamageRandom = 0.0;
      DefaultAttackKnockbackDistance = 0.0;

    constructor Create(const AName: string); override;

    { Sound to make on equipping. Each weapon can have it's own
      equipping sound. }
    property EquippingSound: TSoundType
      read FEquippingSound write FEquippingSound;

    { Animation of attack with this weapon. }
    property AttackAnimation: T3DResourceAnimation read FAttackAnimation;

    { Animation of keeping weapon ready. }
    property ReadyAnimation: T3DResourceAnimation read FReadyAnimation;

    { Common properties for all types of attack (short-range,
      fire missile and such) below. }

    { A time within AttackAnimationat which @link(Attack) method will be called. }
    property AttackTime: Single read FAttackTime write FAttackTime
      default DefaultAttackTime;

    property AttackSoundStart: TSoundType
      read FAttackSoundStart write FAttackSoundStart default stNone;

    { Short-range attack damage and knockback.
      The short-range attack (along with it's sound etc.) is only
      done if one of these is non-zero.
      @groupBegin }
    property AttackDamageConst: Single read FAttackDamageConst write FAttackDamageConst
      default DefaultAttackDamageConst;
    property AttackDamageRandom: Single read FAttackDamageRandom write FAttackDamageRandom
      default DefaultAttackDamageRandom;
    property AttackKnockbackDistance: Single
      read FAttackKnockbackDistance write FAttackKnockbackDistance
      default DefaultAttackKnockbackDistance;
    { @groupEnd }

    { Sound on successfull hit by a short-range attack. }
    property AttackSoundHit: TSoundType read FAttackSoundHit write FAttackSoundHit;

    { Fire missile attack properties.

      FireMissileName indicates creature kind name to be created (like 'Arrow'),
      must be <> '' to actually fire a missile.

      FireMissileAmmo indicates item kind name to use as ammunition (like 'Quiver').
      It may be empty, in which case the ammunition is not necessary to fire
      a missile.
      If it's set, we will check whether owner of the weapon (like a player)
      has at least one item of this kind, and we'll decrease it
      when firing.

      @groupBegin }
    property FireMissileName: string read FFireMissileName write FFireMissileName;
    property FireMissileAmmo: string read FFireMissileAmmo write FFireMissileAmmo;
    { @groupEnd }

    { Sound on missile successfully fired (we had ammunition). }
    property FireMissileSound: TSoundType read FFireMissileSound write FFireMissileSound default stNone;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); override;
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

    { Make real attack, short-range or firing missile.
      Called during weapon TItemWeaponKind.AttackAnimation,
      at the time TItemWeaponKind.AttackTime.
      The default implementation in @className does a short-range
      attack (if AttackDamageConst or AttackDamageRandom or AttackKnockbackDistance
      non-zero) and fires a missile (if FireMissileName not empty). }
    procedure Attack; virtual;

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
    Rotation, LifeTime: Single;
  protected
    function GetExists: boolean; override;
    function GetChild: T3D; override;
  public
    { Speed of the rotation of 3D item on world.
      In radians per second, default is DefaultRotationSpeed.
      Set to zero to disable rotation. }
    RotationSpeed: Single; static;

    { Does the player automatically picks up items by walking over them.
      Default is @true. If you set this to @false, you most probably want to
      implement some other way of picking up items, use the ExtractItem method. }
    AutoPick: boolean; static;

    const
      DefaultRotationSpeed = Pi;

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
  end;

var
  { Global callback to control items on level existence. }
  OnItemOnWorldExists: T3DExistsEvent;

implementation

uses SysUtils, CastleFilesUtils, CastlePlayer, CastleGameNotifications,
  CastleConfig, CastleCreatures;

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(const AName: string);
begin
  inherited;
  FBaseAnimation := T3DResourceAnimation.Create(Self, 'base');
end;

destructor TItemKind.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TItemKind.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  FImageFileName := ResourceConfig.GetFileName('image');

  FCaption := ResourceConfig.GetValue('caption', '');
  if FCaption = '' then
    raise Exception.CreateFmt('Empty caption attribute for item "%s"', [Name]);
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
var
  B: TBox3D;
begin
  inherited;
  B := FBaseAnimation.BoundingBox;
  FBoundingBoxRotated :=
    B.Transform(RotationMatrixDeg(45         , GravityUp)) +
    B.Transform(RotationMatrixDeg(45 + 90    , GravityUp)) +
    B.Transform(RotationMatrixDeg(45 + 90 * 2, GravityUp)) +
    B.Transform(RotationMatrixDeg(45 + 90 * 3, GravityUp));
  { prepare GLImage now }
  GLImage;
end;

procedure TItemKind.ReleaseCore;
begin
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

constructor TItemWeaponKind.Create(const AName: string);
begin
  inherited;
  FAttackAnimation := T3DResourceAnimation.Create(Self, 'attack');
  FReadyAnimation := T3DResourceAnimation.Create(Self, 'ready');
  FAttackTime := DefaultAttackTime;
  FAttackDamageConst := DefaultAttackDamageConst;
  FAttackDamageRandom := DefaultAttackDamageRandom;
  FAttackKnockbackDistance := DefaultAttackKnockbackDistance;
end;

procedure TItemWeaponKind.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  inherited;

  EquippingSound := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('equipping_sound', ''));

  AttackTime := ResourceConfig.GetFloat('attack/time', DefaultAttackTime);
  AttackSoundStart := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('attack/sound_start', ''));
  AttackSoundHit := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('attack/sound_hit', ''));
  AttackDamageConst := ResourceConfig.GetFloat('attack/damage/const',
    DefaultAttackDamageConst);
  AttackDamageRandom := ResourceConfig.GetFloat('attack/damage/random',
    DefaultAttackDamageRandom);
  AttackKnockbackDistance := ResourceConfig.GetFloat('attack/knockback_distance',
    DefaultAttackKnockbackDistance);
  FireMissileName := ResourceConfig.GetValue('fire_missile/name', '');
  FireMissileAmmo := ResourceConfig.GetValue('fire_missile/ammo', '');
  FireMissileSound := SoundEngine.SoundFromName(
    ResourceConfig.GetValue('fire_missile/sound', ''));
end;

function TItemWeaponKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemWeapon;
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

procedure TItemWeapon.Attack;
var
  Own: T3DOrient;
  AttackDC, AttackDR, AttackKD: Single;

  procedure ShortRangeAttack;
  var
    I: Integer;
    Enemy: T3DAlive;
    WeaponBoundingBox: TBox3D;
    SoundDone: boolean;
  begin
    SoundDone := false;

    { Own.Direction may be multiplied by something here for long-range weapons }
    WeaponBoundingBox := Own.BoundingBox.Translate(Own.Direction);
    { Tests: Writeln('WeaponBoundingBox is ', WeaponBoundingBox.ToNiceStr); }
    { TODO: we would prefer to use World.BoxCollision for this,
      but we need to know which creature was hit. }
    for I := 0 to World.Count - 1 do
      if World[I] is T3DAlive then
      begin
        Enemy := T3DAlive(World[I]);
        { Tests: Writeln('Creature bbox is ', C.BoundingBox.ToNiceStr); }
        if (Enemy <> Own) and
          Enemy.BoundingBox.Collision(WeaponBoundingBox) then
        begin
          if not SoundDone then
          begin
            SoundEngine.Sound(Kind.AttackSoundHit);
            SoundDone := true;
          end;
          Enemy.Hurt(AttackDC + Random * AttackDR, Own.Direction, AttackKD);
        end;
      end;
  end;

  procedure FireMissileAttack;

    procedure FireMissile;
    begin
      (Resources.FindName(Kind.FireMissileName) as TCreatureKind).
         CreateCreature(World, Own.Position, Own.Direction);
      SoundEngine.Sound(Kind.FireMissileSound);
    end;

  var
    Inventory: TInventory;
    AmmoIndex: Integer;
    AmmoItem: TInventoryItem;
    AmmoKind: TItemKind;
  begin
    { When FireMissileAmmo is set, check whether the owner has ammunition.
      Currently, only Player may have Inventory with items. }
    if Kind.FireMissileAmmo <> '' then
    begin
      if Own is TPlayer then
      begin
        Inventory := TPlayer(Own).Inventory;
        AmmoKind := Resources.FindName(Kind.FireMissileAmmo) as TItemKind;
        AmmoIndex := Inventory.FindKind(AmmoKind);
        if AmmoIndex = -1 then
        begin
          Notifications.Show('You have no ammunition');
          SoundEngine.Sound(stPlayerInteractFailed);
        end else
        begin
          { delete arrow from player }
          AmmoItem := Inventory[AmmoIndex];
          AmmoItem.Quantity := AmmoItem.Quantity - 1;
          Inventory.CheckDepleted(AmmoItem);
          FireMissile;
        end;
      end;
    end else
      { When FireMissileAmmo is not set, just fire missile }
      FireMissile;
  end;

begin
  { attacking only works when there's an owner (player, in the future creature
    should also be able to use it) of the weapon }
  if (Owner3D <> nil) and
     (Owner3D is T3DOrient) then
  begin
    Own := T3DOrient(Owner3D);

    AttackDC := Kind.AttackDamageConst;
    AttackDR := Kind.AttackDamageRandom;
    AttackKD := Kind.AttackKnockbackDistance;

    if (AttackDC >= 0) or
       (AttackDR >= 0) or
       (AttackKD >= 0) then
      ShortRangeAttack;

    if Kind.FireMissileName <> '' then
      FireMissileAttack;
  end;
end;

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
  Result := Item.Kind.BaseAnimation.Scene(LifeTime, true);
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

  LifeTime += CompSpeed;

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

initialization
  TItemOnWorld.RotationSpeed := TItemOnWorld.DefaultRotationSpeed;
  TItemOnWorld.AutoPick := true;

  RegisterResourceClass(TItemKind, 'Item');
  RegisterResourceClass(TItemWeaponKind, 'Weapon');
end.
