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

{ Items, things that can be picked up, carried and used. }
unit CastleItems;

interface

uses Boxes3D, X3DNodes, CastleScene, VectorMath, CastleUtils,
  CastleClassUtils, Classes, Images, GL, GLU, CastleGLUtils,
  PrecalculatedAnimation, CastleResources,
  CastleXMLConfig, ALSoundEngine, Frustum, Base3D, FGL, CastleColors;

const
  DefaultItemDamageConst = 5.0;
  DefaultItemDamageRandom = 5.0;
  DefaultItemActualAttackTime = 0.0;
  DefaultItemAttackKnockbackDistance = 1.0;

type
  TItem = class;
  TItemClass = class of TItem;

  { Kind of item. }
  TItemKind = class(T3DResource)
  private
  { Design question: Maybe it's better making TSword a descendant of TItem,
    and not creating TItemKind class ? This seems somewhat cleaner
    from OOP approach. Then various functions/properties
    of TItemKind must be handled as "class function" of TItem.
    Answer: I once did this (see "Jamy & Nory"), but it turns out that it's
    not comfortable --- for example I would need associative array
    (like LoadedModels) to keep TItemKind.FScene value
    (to not load and not construct new GL display list each time
    I create TSword instance). }

    FSceneFileName: string;
    FScene: TCastleScene;
    FCaption: string;
    FImageFileName: string;
    FImage: TCastleImage;
    FGLList_DrawImage: TGLuint;
    FBoundingBoxRotated: TBox3D;
    FBoundingBoxRotatedCalculated: boolean;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
    { Which TItem descendant to create when constructing item
      of this kind by CreateItem. }
    function ItemClass: TItemClass; virtual;
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

    { OpenGL display list to draw @link(Image). }
    function GLList_DrawImage: TGLuint;

    { This returns Scene.BoundingBox enlarged a little (along X and Y)
      to account the fact that Scene may be rotated around +Z vector. }
    function BoundingBoxRotated: TBox3D;

    { Create item. This is how you should create new TItem instances.
      It is analogous to TCreatureKind.CreateCreature, but now for items.

      Note that the item itself doesn't exist on a level --- you have to
      put it there if you want by TItem.PutOnLevel. That is because items
      can also exist only in player's backpack and such, and then they
      are independent from 3D world. }
    function CreateItem(const AQuantity: Cardinal): TItem;
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
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
    function ItemClass: TItemClass; override;
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
      default DefaultItemActualAttackTime;

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
    constructor Create(const AId: string); override;

    property DamageConst: Single read FDamageConst write FDamageConst
      default DefaultItemDamageConst;
    property DamageRandom: Single read FDamageRandom write FDamageRandom
      default DefaultItemDamageRandom;
    property AttackKnockbackDistance: Single
      read FAttackKnockbackDistance write FAttackKnockbackDistance
      default DefaultItemAttackKnockbackDistance;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TItemOnLevel = class;

  { An item.
    Thanks to the @link(Quantity) property, this may actually represent
    many "stacked" items, all having the same properties. }
  TItem = class(TComponent)
  private
    FKind: TItemKind;
    FQuantity: Cardinal;
    FOwner3D: T3D;

    { Stackable means that two items are equal and they can be summed
      into one item by adding their Quantity values.
      Practially this means that all properties
      of both items are equal, with the exception of Quantity.

      TODO: protected virtual in the future? }
    function Stackable(Item: TItem): boolean;
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
    function Split(QuantitySplit: Cardinal): TItem;

    { Create TItemOnLevel instance referencing this item,
      and add this to the given 3D AWorld.
      Although normal item knows (through Owner3D) the world it lives in,
      but this method may be used for items that don't have an owner yet,
      so we take AWorld parameter explicitly.
      This is how you should create new TItemOnLevel instances.
      It is analogous to TCreatureKind.CreateCreature, but now for items. }
    function PutOnLevel(const AWorld: T3DWorld;
      const APosition: TVector3Single): TItemOnLevel;

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
      or the TItemOnLevel instance (if the item is lying on the world,
      pickable). May be @nil only in special situations (when item is moved
      from one 3D to another, and technically it's safer to @nil this
      property).

      The owner is always responsible for freeing this TItem instance
      (in case of TItemOnLevel, it does it directly;
      in case of player or creature, it does it by TItemsInventory). }
    property Owner3D: T3D read FOwner3D;

    { 3D world of this item, if any.

      Although the TItem, by itself, is not a 3D thing
      (only pickable TItemOnLevel is a 3D thing).
      But it exists inside a 3D world: either as pickable (TItemOnLevel),
      or as being owned by a 3D object (like player or creature) that are
      part of 3D world. In other words, our Owner3D.World is the 3D world
      this item lives in. }
    function World: T3DWorld;
  end;

  TItemWeapon = class(TItem)
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
    items are correctly stacked, that TItem.Owner3D and memory management
    is good). }
  TItemsInventory = class(specialize TFPGObjectList<TItem>)
  private
    FOwner3D: T3DAlive;

    { Check is given Item "stackable" with any other item on this list.
      Returns index of item on the list that is stackable with given Item,
      or -1 if none. }
    function Stackable(Item: TItem): Integer;
  public
    constructor Create(const AOwner3D: T3DAlive);

    { Owner of the inventory (like a player or creature).
      Never @nil, always valid for given inventory.
      All items on this list always have the same TItem.Owner3D value
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
    function Pick(var Item: TItem): Integer;

    { Drop item with given index.
      ItemIndex must be valid (between 0 and Items.Count - 1).
      You @italic(must) take care yourself of returned TItem memory
      management. }
    function Drop(const ItemIndex: Integer): TItem;

    { Pass here items owned by this list, immediately after decreasing
      their Quantity. This frees the item (removing it from the list)
      if it's quantity reached zero. }
    procedure CheckDepleted(const Item: TItem);
  end;

  { Item that is placed on a 3D world, ready to be picked up.
    It's not in anyone's inventory. }
  TItemOnLevel = class(T3DOrient)
  private
    FItem: TItem;
    Rotation: Single;
  protected
    function GetExists: boolean; override;
    function GetChild: T3D; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The Item owned by this TItemOnLevel instance. Never @nil. }
    property Item: TItem read FItem;

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

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;

    property Collides default false;
    property Pushable default true;
    function Middle: TVector3Single; override;
  end;

  { TODO: Move to castle1 code? }
const
  DefaultAutoOpenInventory = true;

var
  { Automatically open inventory on pickup ?
    Saved/loaded to config file in this unit. }
  AutoOpenInventory: boolean;

  InventoryVisible: boolean;

  { Global callback to control items on level existence. }
  OnItemOnLevelExists: T3DExistsEvent;

implementation

uses SysUtils, CastleFilesUtils, CastlePlayer,
  CastleGameNotifications, CastleConfig, GLImages, CastleGameVideoOptions;

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
    raise Exception.CreateFmt('Empty caption attribute for item "%s"', [Id]);
end;

function TItemKind.Scene: TCastleScene;
begin
  Result := FScene;
end;

function TItemKind.Image: TCastleImage;
begin
  if FImage = nil then
    FImage := LoadImage(ImageFileName, [], []);
  Result := FImage;
end;

function TItemKind.GLList_DrawImage: TGLuint;
begin
  if FGLList_DrawImage = 0 then
    FGLList_DrawImage := ImageDrawToDisplayList(Image);
  Result := FGLList_DrawImage;
end;

function TItemKind.BoundingBoxRotated: TBox3D;
var
  HorizontalSize: Single;
begin
  if not FBoundingBoxRotatedCalculated then
  begin
    FBoundingBoxRotated := Scene.BoundingBox;

    { Note that I *cannot* assume below that Scene.BoundingBox
      middle point is (0, 0, 0). So I just take the largest distance
      from point (0, 0) to any corner of the Box (distance 2D,
      only horizontally) and this tells me the horizontal sizes of the
      bounding box.

      This hurts a little (because of 1 call to Sqrt),
      that's why results of this function are cached if FBoundingBoxRotated. }
    HorizontalSize := Max(Max(
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[0, 0], FBoundingBoxRotated.Data[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[1, 0], FBoundingBoxRotated.Data[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[1, 0], FBoundingBoxRotated.Data[1, 1]))),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[0, 0], FBoundingBoxRotated.Data[1, 1])));
    HorizontalSize := Sqrt(HorizontalSize);
    FBoundingBoxRotated.Data[0, 0] := -HorizontalSize;
    FBoundingBoxRotated.Data[0, 1] := -HorizontalSize;
    FBoundingBoxRotated.Data[1, 0] := +HorizontalSize;
    FBoundingBoxRotated.Data[1, 1] := +HorizontalSize;

    FBoundingBoxRotatedCalculated := true;
  end;
  Result := FBoundingBoxRotated;
end;

procedure TItemKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
  inherited;
  PrepareScene(FScene, SceneFileName, BaseLights, DoProgress);
end;

function TItemKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TItemKind.ReleaseCore;
begin
  FScene := nil;
  inherited;
end;

function TItemKind.CreateItem(const AQuantity: Cardinal): TItem;
begin
  Result := ItemClass.Create(nil { for now, TItem.Owner is always nil });
  { set properties that in practice must have other-than-default values
    to sensibly use the item }
  Result.FKind := Self;
  Result.FQuantity := AQuantity;
  Assert(Result.Quantity >= 1, 'Item''s Quantity must be >= 1');
end;

function TItemKind.ItemClass: TItemClass;
begin
  Result := TItem;
end;

{ TItemWeaponKind ------------------------------------------------------------ }

procedure TItemWeaponKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
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
    DefaultItemActualAttackTime);

  EquippingSound := SoundEngine.SoundFromName(
    KindsConfig.GetValue('equipping_sound', ''));
  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue('sound_attack_start', ''));

  FReadyAnimationFile:= KindsConfig.GetFileName('ready_animation');
  FAttackAnimationFile := KindsConfig.GetFileName('attack_animation');
end;

function TItemWeaponKind.ItemClass: TItemClass;
begin
  Result := TItemWeapon;
end;

{ TItemShortRangeWeaponKind -------------------------------------------------- }

constructor TItemShortRangeWeaponKind.Create(const AId: string);
begin
  inherited;
  FDamageConst := DefaultItemDamageConst;
  FDamageRandom := DefaultItemDamageRandom;
  FAttackKnockbackDistance := DefaultItemAttackKnockbackDistance;
end;

procedure TItemShortRangeWeaponKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  DamageConst := KindsConfig.GetFloat('damage/const',
    DefaultItemDamageConst);
  DamageRandom := KindsConfig.GetFloat('damage/random',
    DefaultItemDamageRandom);
  AttackKnockbackDistance := KindsConfig.GetFloat('attack_knockback_distance',
    DefaultItemAttackKnockbackDistance);
end;

{ TItem ------------------------------------------------------------ }

function TItem.Stackable(Item: TItem): boolean;
begin
  Result := Item.Kind = Kind;
end;

function TItem.Split(QuantitySplit: Cardinal): TItem;
begin
  Check(Between(Integer(QuantitySplit), 1, Quantity - 1),
    'You must split >= 1 and less than current Quantity');

  Result := Kind.CreateItem(QuantitySplit);

  FQuantity -= QuantitySplit;
end;

function TItem.PutOnLevel(const AWorld: T3DWorld;
  const APosition: TVector3Single): TItemOnLevel;
begin
  Result := TItemOnLevel.Create(AWorld { owner });
  { set properties that in practice must have other-than-default values
    to sensibly use the item }
  Result.FItem := Self;
  FOwner3D := Result;
  Result.SetView(APosition, AnyOrthogonalVector(AWorld.GravityUp), AWorld.GravityUp);
  AWorld.Add(Result);
end;

procedure TItem.Use;
begin
  Notifications.Show('This item cannot be used');
end;

function TItem.World: T3DWorld;
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

{ TItemsInventory ------------------------------------------------------------ }

constructor TItemsInventory.Create(const AOwner3D: T3DAlive);
begin
  inherited Create(true);
  FOwner3D := AOwner3D;
end;

function TItemsInventory.Stackable(Item: TItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Stackable(Item) then
      Exit;
  Result := -1;
end;

function TItemsInventory.FindKind(Kind: TItemKind): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Kind = Kind then
      Exit;
  Result := -1;
end;

function TItemsInventory.Pick(var Item: TItem): Integer;
begin
  Result := Stackable(Item);
  if Result <> -1 then
  begin
    { Stack Item with existing item }
    Items[Result].Quantity := Items[Result].Quantity + Item.Quantity;
    FreeAndNil(Item);
    Item := Items[Result];
  end else
  begin
    Add(Item);
    Result := Count - 1;
  end;

  Item.FOwner3D := Owner3D;
end;

function TItemsInventory.Drop(const ItemIndex: Integer): TItem;
var
  SelectedItem: TItem;
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

procedure TItemsInventory.CheckDepleted(const Item: TItem);
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

{ TItemOnLevel ------------------------------------------------------------ }

constructor TItemOnLevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Pushable := true;

  { Items are not collidable, player can enter them to pick them up.
    For now, this also means that creatures can pass through them,
    which isn't really troublesome now. }
  Collides := false;
end;

destructor TItemOnLevel.Destroy;
begin
  FreeAndNil(FItem);
  inherited;
end;

function TItemOnLevel.GetChild: T3D;
begin
  if (Item = nil) or not Item.Kind.Prepared then Exit(nil);
  Result := Item.Kind.Scene;
end;

procedure TItemOnLevel.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  inherited;
  if GetExists and RenderBoundingBoxes and
    (not Params.Transparent) and Params.ShadowVolumesReceivers and
    Frustum.Box3DCollisionPossibleSimple(BoundingBox) then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);
      glDrawBox3DWire(BoundingBox);
    glPopAttrib;
  end;
end;

const
  ItemRadius = 1.0;

procedure TItemOnLevel.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
const
  FallingDownSpeed = 10.0;
var
  AboveHeight: Single;
  ShiftedPosition: TVector3Single;
  FallingDownLength: Single;
  PickedItem: TItem;
begin
  inherited;
  if not GetExists then Exit;

  Rotation += 2.61 * CompSpeed;
  Direction := Vector3Single(Sin(Rotation), Cos(Rotation), 0);
  Up := World.GravityUp; // TODO: should be initially set for all items,
  // and rotations should be around it

  ShiftedPosition := Position;
  ShiftedPosition[2] += ItemRadius;

  { Note that I'm using ShiftedPosition, not Position,
    and later I'm comparing "AboveHeight > ItemRadius",
    instead of "AboveHeight > 0".
    Otherwise, I risk that when item will be placed perfectly on the ground,
    it may "slip through" this ground down. }

  MyHeight(ShiftedPosition, AboveHeight);
  if AboveHeight > ItemRadius then
  begin
    { Item falls down because of gravity. }

    FallingDownLength := CompSpeed * FallingDownSpeed;
    MinTo1st(FallingDownLength, AboveHeight - ItemRadius);

    MyMove(Vector3Single(0, 0, -FallingDownLength), true,
      { TODO: wall-sliding here breaks left life potion on gate:
        it must be corrected (possibly by correcting the large sword mesh)
        to not "slip down" from the sword. }
      false);
  end;

  if (World.Player <> nil) and
     (World.Player is TPlayer) and
     (not World.Player.Dead) and
     BoundingBox.Collision(World.Player.BoundingBox) then
  begin
    { We no longer own this Item, so clear references. }
    PickedItem := Item;
    PickedItem.FOwner3D := nil;
    FItem := nil;
    TPlayer(World.Player).PickItem(PickedItem);

    { Since we cannot live with Item = nil, we free ourselves }
    RemoveMe := rtRemoveAndFree;
    if AutoOpenInventory then
      InventoryVisible := true;
  end;
end;

function TItemOnLevel.PointingDeviceActivate(const Active: boolean;
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
    S := Format('You see an item "%s"', [Item.Kind.Caption]);
    if Item.Quantity <> 1 then
      S += Format(' (quantity %d)', [Item.Quantity]);
    Notifications.Show(S);
  end else
    Notifications.Show('You see some item, but it''s too far to tell exactly what it is');
end;

function TItemOnLevel.GetExists: boolean;
begin
  Result := (inherited GetExists) and
    ((not Assigned(OnItemOnLevelExists)) or OnItemOnLevelExists(Self));
end;

function TItemOnLevel.Middle: TVector3Single;
begin
  Result := inherited Middle;
  Result[2] += ItemRadius;
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
  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
