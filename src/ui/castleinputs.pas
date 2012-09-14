{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Key and mouse shortcuts (TInputShortcut) and global management of them.

  TInputShortcut instance represents a key/mouse shortcut.
  Instances of this class are spread throughout the engine.
  We have two different ways of using TInputShortcut instance:

  @unorderedList(
    @item(
      @bold(Global)

      TInputShortcut instance with TInputShortcut.Group <> igLocal
      is called "global". Such instance is automatically (at construction)
      added to InputsAll and InputsGroup[Group] lists.

      The purpose of such global input map is to be able to detect key conflicts,
      be able to restore whole input map to default,
      load/save them to the user config file, and so on.
      All shortcuts used in a typical 3D game, with normal CastleLevel
      and CastlePlayer usage, are global.

      Global shortcuts are owned (they will be freed by) this unit
      (more specifically, they will be freed by InputsAll).
      When creating them, pass @nil to the Owner parameter
      of constructor TInputShortcut.Create.
      This implies that InputsAll and InputsGroup[Group] lists will never shrink,
      which is useful --- once added, shortcuts will not disappear.
      Global TInputShortcut instances are always in practice also global variables.

      For example CastleSceneManager unit contains Input_PointingDeviceActivate.
      For example CastlePlayer contains many inputs.
    )

    @item(
      @bold(Local)

      TInputShortcut instance with TInputShortcut.Group = igLocal
      is called "local". Basically, it means it's a normal component,
      it's not magically present on any global list, it's not magically
      managed by any list.

      For example TWalkCamera contains a number of them like
      TWalkCamera.Input_Forward.

      Although it seems like "global" inputs are such a good idea, there
      are some reasons for having some inputs "local":

      @unorderedList(
        @itemSpacing Compact
        @item(You can set them locally, obviously, not program-wide.)
        @item(You can set them from Lazarus object inspector e.g. for cameras.)
        @item(We cannot add shortcuts of both TExamineCamera and TWalkCamera
          to global, as they would conflict (e.g. "up arrow key" is used
          by both by default). The InputsAll doesn't have (for now) any mechanism
          to indicate that only one of the cameras will be in practice
          used for a given TCastleSceneManager.)
        @item(We cannot add shortcuts of TCamera descendants also because
          CastlePlayer has shortcuts that override camera shortcuts.
          One day this problem may disappear, when TPlayer and TCamera
          will become integrated more.)
      )
    )
  )

  You create new inputs by simply constructing new TInputShortcut instances.
  Make sure you add all global inputs before calling @code(Config.Load),
  as some functionality assumes that all shortcuts are already added
  at the time @code(Config.Load) is called.
  The engine units themselves never call @code(Config.Load), it is left
  to the final application.

  This unit also defines new CastleScript function: @code(shortcut),
  see [http://castle-engine.sourceforge.net/castle_script.php#function_shortcut]. }
unit CastleInputs;

interface

uses KeysMouse, CastleUtils, CastleClassUtils, Classes,
  FGL, GenericStructList, CastleConfig, CastleScript, UIControls;

type
  TInputShortcut = class;

  { A keyboard and/or mouse shortcut for activating some action.

    Action may be activated by:
    @unorderedList(
      @itemSpacing compact
      @item at most two key shortcuts,
      @item and one mouse button shortcut,
      @item and one character shortcut,
      @item and one mouse wheel shortcut.
    )

    The difference between @italic(key shortcut) and @italic(character shortcut):
    @italic("Key") is something that can be expressed as TKey value.
    @italic("Character") is something that can be expressed as Char value.
    They are keys like "control key" (K_Ctrl) or "shift key" (K_Shift)
    that cannot be expressed (on their own) as characters.
    Characters are both more and less specific: character "A" (upper letter "a")
    is activated by pressing "a", but only when Shift is pressed or CapsLock is on
    (window system / GUI toolkit may also allow other ways to input characters). }
  TInputShortcut = class(TComponent)
  private
    FKey1: TKey;
    FKey2: TKey;
    FCharacter: Char;
    FMouseButtonUse: boolean;
    FMouseButton: TMouseButton;
    FMouseWheel: TMouseWheelDirection;

    procedure SetKey1(const Value: TKey);
    procedure SetKey2(const Value: TKey);
    procedure SetCharacter(const Value: Char);
    procedure SetMouseButtonUse(const Value: boolean);
    procedure SetMouseButton(const Value: TMouseButton);
    procedure SetMouseWheel(const Value: TMouseWheelDirection);
  private
    FDefaultKey1: TKey;
    FDefaultKey2: TKey;
    FDefaultCharacter: Char;
    FDefaultMouseButtonUse: boolean;
    FDefaultMouseButton: TMouseButton;
    FDefaultMouseWheel: TMouseWheelDirection;
  protected
    { Called always right after the key/character/mouse
      shortcut value changed. Note that this is called only when
      the "current" values (Key1, Key2, Character, MouseButtonUse, MouseButton,
      MouseWheel) changed, and it's not called when just the DefaultXxx
      values changed. }
    procedure Changed; virtual;
  public
    procedure MakeDefault;

    { Assigns to this object the default values from Source. }
    procedure AssignFromDefault(Source: TInputShortcut);

    { Copy Source properties to this object.
      It always copies "current" properties (Key1, Key2, Character,
      MouseButtonUse, MouseButton, MouseWheel), and optionally (if CopyDefaults)
      also copies the DefaultXxx properties. }
    procedure Assign(Source: TInputShortcut; CopyDefaults: boolean); reintroduce;

    { Set keys/mouse buttons of this shortcut.
      Sets both current and default properties. }
    procedure Assign(
      const AKey1: TKey;
      const AKey2: TKey = K_None;
      const ACharacter: Char = #0;
      const AMouseButtonUse: boolean = false;
      const AMouseButton: TMouseButton = mbLeft;
      const AMouseWheel: TMouseWheelDirection = mwNone);

    { Make this input impossible to activate by the user.
      This sets both keys to K_None, Character to #0, MouseButtonUse
      to @false, and MouseWheel to mwNone. }
    procedure MakeClear;

    { Given a set of currently pressed keys and mouse buttons,
      decide whether this input is currently pressed. }
    function IsPressed(Pressed: TKeysPressed;
      const MousePressed: TMouseButtons): boolean;

    { Looking at Container's currently pressed keys and mouse buttons,
      decide whether this input is currently pressed. }
    function IsPressed(Container: IUIContainer): boolean;

    { Check does given Key or ACharacter correspond to this input shortcut.
      If Key = K_None and ACharacter = #0, result is always @false. }
    function IsKey(Key: TKey; ACharacter: Char): boolean;

    { Check does given mouse button correspond to this input shortcut.
      In practice, just checks MouseButtonUse and if @true, compares
      AMouseButton with MouseButton. }
    function IsMouseButton(AMouseButton: TMouseButton): boolean;

    function IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;

    { Check does given key or mouse button or mouse wheel use activates
      this shortcut.

      For key/character press, set AKey <> K_None or ACharacter <> #0.
      For mouse button press, set AMousePress to @true
      and pass relevant AMouseButton. For mouse wheel, pass AMouseWheel
      <> mwNone. Pass only one of these three events here,
      for example if you AMousePress to @true then pass
      AKey = K_None and ACharacter = #0 and AMouseWheel = mwNone.

      Basically, this is a "dispatcher" that simply calls one of the IsKey or
      IsMouseButton or IsMouseWheel methods. It's sometimes more comfortable
      to use this instead of taking care of them separately. }
    function IsEvent(AKey: TKey; ACharacter: Char;
      AMousePress: boolean; AMouseButton: TMouseButton;
      AMouseWheel: TMouseWheelDirection): boolean;

    { Describe this input shortcut. If it's not active at all
      (like after MakeClear), we will use NoneString. }
    function Description(const NoneString: string): string;

    { Modifier keys that are relevant to recognize this shortcut. }
    function Modifiers: TModifierKeys;
  published
    { Key/mouse properties on TInputShortcut are declared without
      "default" specifier, to always save them in Lazarus LFM file.
      Reason: various class (TExamineCamera, TWalkCamera, TCastleSceneManager)
      create them setting different default values.
      If we would declare that default for Key1 is K_None,
      then you couldn't set in Lazarus e.g. TWalkCamera.Input_Forward.Key1 to K_None.
      Such K_None would not be saved to LFM (since it would equal Key1 default
      value), but when reading the LFM back it would change into K_Up
      (because TWalkCamera creates Input_Forward with K_Up by default). }

    { Key shortcuts for given command. You can set any of them to K_None
      to indicate that no key is assigned.
      @groupBegin }
    property Key1: TKey read FKey1 write SetKey1;
    property Key2: TKey read FKey2 write SetKey2;
    { @groupEnd }

    { Character shortcut for given command. You can set this to #0
      to indicate that no character shortcut is assigned. }
    property Character: Char read FCharacter write SetCharacter;

    { Mouse shortcut for given command. You can set MouseButtonUse to @false
      if you don't want to use this.
      @groupBegin }
    property MouseButtonUse: boolean read FMouseButtonUse write SetMouseButtonUse;
    property MouseButton: TMouseButton read FMouseButton write SetMouseButton;
    { @groupEnd }

    { Mouse wheel to activate this command. Note that mouse wheels cannot be
      continously pressed (our method IsPressed doesn't look at it),
      so this is only suitable for commands that work in steps
      (not continously). }
    property MouseWheel: TMouseWheelDirection read FMouseWheel
      write SetMouseWheel;

    { Default values for properties key/mouse.
      You can change them --- this will change what MakeDefault does.

      Note that setting these properties doesn't automatically set
      corresponding "current" property. E.g. @code(DefaultKey1 := K_Space;)
      doesn't change the value of Key1 property --- only DefaultKey1
      changes. You can explicitly change Key1 property, or just call
      MakeDefault afterwards, if you want this to happen.
      @groupBegin }
    property DefaultKey1: TKey read FDefaultKey1 write FDefaultKey1;
    property DefaultKey2: TKey read FDefaultKey2 write FDefaultKey2;
    property DefaultCharacter: Char
      read FDefaultCharacter write FDefaultCharacter;
    property DefaultMouseButtonUse: boolean
      read FDefaultMouseButtonUse write FDefaultMouseButtonUse;
    property DefaultMouseButton: TMouseButton
      read FDefaultMouseButton write FDefaultMouseButton;
    property DefaultMouseWheel: TMouseWheelDirection
      read FDefaultMouseWheel write FDefaultMouseWheel;
    { @groupEnd }

    { }
    { TODO: Maybe introduce a way to limit (TKey, or all shortcuts?)
      to activate only when specific modifier is pressed.

      Right now both TWalkCamera and TExamineCamera check modifiers
      and have not configurable behavior:

      - TWalkCamera allows inputs only when modifiers = [].
        Except Input_Right/LeftRot and Input_Up/DownRotate that have special
        meaning when Ctrl is pressed (see TWalkCamera.AllowSlowerRotations).
      - TExamineCamera allows Inputs_Move only when modifiers = [mkCtrl].
        Other TExamineCamera are allowed only when modifiers = []. }
  end;

  TInputGroup = (igLocal, igBasic, igItems, igOther);

  { A wrapper around TInputShortcut instance
    (used to describe key/mouse shortcut for given action)
    with additional properties describing the group of the action,
    action name etc.

    Note that "castle" doesn't use TInputShortcut.Character,
    it's always #0. We detect keys only by TKey, as that's enough for now
    (and detecting by character also would complicate looking for duplicates,
    as comparison TKey <-> char is not possible without knowing involved
    modifiers). }
  TInputConfiguration = class
  private
    FCaption: string;
    FGroup: TInputGroup;
    FShortcut: TInputShortcut;
    FName: string;
    FGroupOrder: Integer;
    { Index of InputsAll. For now this is useful only for sorting,
      to decide order when GroupOrder is equal between two items. }
    Index: Integer;
  public
    { Constructor. Note that TInputShortcut instance passed here is owned
      by this object, i.e. it will be freed in our destructor. }
    constructor Create(const ACaption: string;
      const AName: string;
      const AGroup: TInputGroup;
      const AKey1: TKey;
      const AKey2: TKey = K_None;
      const ACharacter: Char = #0;
      const AMouseButtonUse: boolean = false;
      const AMouseButton: TMouseButton = mbLeft;
      const AMouseWheel: TMouseWheelDirection = mwNone);
    destructor Destroy; override;

    { Nice name to show user. With spaces, localized characters etc. }
    property Caption: string read FCaption;

    { Short name. Used for config file entries, for CastleScript
      @code(shortcut()) function
      [castle-engine.sourceforge.net/castle_script.php#function_shortcut]
      and such.
      Must be a valid identifier, that is stick to English letters, underscores
      and digits (and do not start with a digit). }
    property Name: string read FName;

    { Group of the global shortcut, or igLocal indicating a local shortcut.
      Games may use this group to better show the keys configuration for user,
      presenting together keys from the same group. }
    property Group: TInputGroup read FGroup;

    { Order of the shortcut within it's @link(Group).
      The order may be important, as menus may show InputsGroup
      to user in this order.
      This order is applied (the group is actually sorted by GroupOrder)
      when reading config file.
      For equal GroupOrder, the order of creation (equal to the order
      on InputsAll list) decides which is first. }
    property GroupOrder: Integer read FGroupOrder write FGroupOrder;

    { The key/mouse shortcut for this action.
      You can directly change fields of this action. }
    property Shortcut: TInputShortcut read FShortcut;

    { Add to Shortcut new key or mouse button or mouse wheel.
      Only one of them (parameters NewXxx like for TInputShortcut.IsEvent). }
    procedure AddShortcut(const NewKey: TKey;
      const NewMousePress: boolean; const NewMouseButton: TMouseButton;
      const NewMouseWheel: TMouseWheelDirection);

    { Nice text describing the shortcut value. }
    function Description: string;
  end;

  TInputConfigurationList = class(specialize TFPGObjectList<TInputConfiguration>)
  private
    procedure LoadFromConfig(const Config: TCastleConfig);
    procedure SaveToConfig(const Config: TCastleConfig);
    function FindName(const Name: string): TInputConfiguration;
  public
    { Seeks for a Shortcut that has matching key or mouse button or mouse wheel.
      @nil if not found. }
    function SeekMatchingShortcut(const Key: TKey;
      const MousePress: boolean; const MouseButton: TMouseButton;
      const MouseWheel: TMouseWheelDirection): TInputConfiguration;
    procedure RestoreDefaults;
    function SeekConflict(out ConflictDescription: string): boolean;
  end;

  TInputChangedEvent = procedure (InputConfiguration: TInputConfiguration) of object;
  PInputChangedEvent = ^TInputChangedEvent;

  TInputChangedEventList = class(specialize TGenericStructList<TInputChangedEvent>)
  public
    procedure ExecuteAll(InputConfiguration: TInputConfiguration);
  end;

var
  { Basic shortcuts. }
  CastleInput_Forward: TInputConfiguration;
  CastleInput_Backward: TInputConfiguration;
  CastleInput_LeftRot: TInputConfiguration;
  CastleInput_RightRot: TInputConfiguration;
  CastleInput_LeftStrafe: TInputConfiguration;
  CastleInput_RightStrafe: TInputConfiguration;
  CastleInput_UpRotate: TInputConfiguration;
  CastleInput_DownRotate: TInputConfiguration;
  CastleInput_GravityUp: TInputConfiguration;
  CastleInput_UpMove: TInputConfiguration;
  CastleInput_DownMove: TInputConfiguration;

  { Other shortcuts. }
  CastleInput_Interact: TInputConfiguration;

  { List of all global inputs.
    Will be created in initialization and freed in finalization of this unit.
    All TInputConfiguration instances will automatically add to this. }
  InputsAll: TInputConfigurationList;
  InputsGroup: array [igBasic..High(TInputGroup)] of TInputConfigurationList;

implementation

uses SysUtils, CastleStringUtils;

{ TInputShortcut ------------------------------------------------------------- }

procedure TInputShortcut.MakeDefault;
begin
  AssignFromDefault(Self);
end;

procedure TInputShortcut.AssignFromDefault(Source: TInputShortcut);
begin
  FKey1 := Source.DefaultKey1;
  FKey2 := Source.DefaultKey2;
  FCharacter := Source.DefaultCharacter;
  FMouseButtonUse := Source.DefaultMouseButtonUse;
  FMouseButton := Source.DefaultMouseButton;
  FMouseWheel := Source.DefaultMouseWheel;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.Assign(const AKey1: TKey;
  const AKey2: TKey;
  const ACharacter: Char;
  const AMouseButtonUse: boolean;
  const AMouseButton: TMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  FDefaultKey1 := AKey1;
  FDefaultKey2 := AKey2;
  FDefaultCharacter := ACharacter;
  FDefaultMouseButtonUse := AMouseButtonUse;
  FDefaultMouseButton := AMouseButton;
  FDefaultMouseWheel := AMouseWheel;
  MakeDefault;
end;

procedure TInputShortcut.Assign(Source: TInputShortcut; CopyDefaults: boolean);
begin
  if CopyDefaults then
  begin
    DefaultKey1 := Source.DefaultKey1;
    DefaultKey2 := Source.DefaultKey2;
    DefaultCharacter := Source.DefaultCharacter;
    DefaultMouseButtonUse := Source.DefaultMouseButtonUse;
    DefaultMouseButton := Source.DefaultMouseButton;
    DefaultMouseWheel := Source.DefaultMouseWheel;
  end;

  FKey1 := Source.Key1;
  FKey2 := Source.Key2;
  FCharacter := Source.Character;
  FMouseButtonUse := Source.MouseButtonUse;
  FMouseButton := Source.MouseButton;
  FMouseWheel := Source.MouseWheel;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.MakeClear;
begin
  FKey1 := K_None;
  FKey2 := K_None;
  FCharacter := #0;
  FMouseButtonUse := false;
  FMouseWheel := mwNone;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

function TInputShortcut.IsPressed(Pressed: TKeysPressed;
  const MousePressed: TMouseButtons): boolean;
begin
  Result :=
    ( (Pressed <> nil) and (Pressed.Keys[Key1] or
                            Pressed.Keys[Key2] or
                            Pressed.Characters[Character]) ) or
    ( MouseButtonUse and (MouseButton in MousePressed) );
end;

function TInputShortcut.IsPressed(Container: IUIContainer): boolean;
begin
  Result := IsPressed(Container.Pressed, Container.MousePressed);
end;

function TInputShortcut.IsKey(Key: TKey; ACharacter: Char): boolean;
begin
  Result :=
    ( (Key <> K_None) and ( (Key = Key1) or (Key = Key2) ) ) or
    ( (Character <> #0) and (Character = ACharacter) );
end;

function TInputShortcut.IsMouseButton(AMouseButton: TMouseButton): boolean;
begin
  Result := MouseButtonUse and (AMouseButton = MouseButton);
end;

function TInputShortcut.IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;
begin
  Result := (AMouseWheel <> mwNone) and (AMouseWheel = MouseWheel);
end;

function TInputShortcut.IsEvent(AKey: TKey; ACharacter: Char;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection): boolean;
begin
  if AMousePress then
    Result := IsMouseButton(AMouseButton) else
  if AMouseWheel <> mwNone then
    Result := IsMouseWheel(AMouseWheel) else
    Result := IsKey(AKey, ACharacter);
end;

function TInputShortcut.Description(const NoneString: string): string;
begin
  Result := '';

  { It's important for this description to be really compact (as it's
    used in situations like menu items (see "The Castle")), that's why
    I mess with checking various cases and trying to make shorter string
    for this. }

  if (Key1 <> K_None) or (Key2 <> K_None) then
  begin
    if (Key1 <> K_None) and (Key2 <> K_None) then
      Result := Format('key "%s" or "%s"', [KeyToStr(Key1), KeyToStr(Key2)]) else
    if Key1 <> K_None then
      Result += Format('key "%s"', [KeyToStr(Key1)]) else
      Result += Format('key "%s"', [KeyToStr(Key2)]);
  end;

  if Character <> #0 then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('char "%s"', [CharToNiceStr(Character)]);
  end;

  if MouseButtonUse then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('mouse "%s"', [MouseButtonStr[MouseButton]]);
  end;

  if MouseWheel <> mwNone then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('wheel "%s"', [MouseWheelDirectionStr[MouseWheel]]);
  end;

  if Result = '' then
    Result := NoneString;
end;

procedure TInputShortcut.Changed;
begin
end;

procedure TInputShortcut.SetKey1(const Value: TKey);
begin
  FKey1 := Value;
  Changed;
end;

procedure TInputShortcut.SetKey2(const Value: TKey);
begin
  FKey2 := Value;
  Changed;
end;

procedure TInputShortcut.SetCharacter(const Value: Char);
begin
  FCharacter := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButtonUse(const Value: boolean);
begin
  FMouseButtonUse := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton(const Value: TMouseButton);
begin
  FMouseButton := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseWheel(const Value: TMouseWheelDirection);
begin
  FMouseWheel := Value;
  Changed;
end;

function TInputShortcut.Modifiers: TModifierKeys;
var
  MK: TModifierKey;
begin
  Result := [];
  for MK := Low(MK) to High(MK) do
  begin
    if (Key1 = ModifierKeyToKey[MK]) or (Key2 = ModifierKeyToKey[MK]) then
      Include(Result, MK);
  end;
end;

{ TInputConfigurationList ----------------------------------------------------- }

function TInputConfigurationList.SeekMatchingShortcut(
  const Key: TKey;
  const MousePress: boolean; const MouseButton: TMouseButton;
  const MouseWheel: TMouseWheelDirection): TInputConfiguration;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Shortcut.IsEvent(Key, #0, MousePress, MouseButton, MouseWheel) then
      Exit;
  end;
  Result := nil;
end;

procedure TInputConfigurationList.RestoreDefaults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Shortcut.MakeDefault;
end;

procedure TInputConfigurationList.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Config.SetDeleteValue('inputs/' + Items[I].Name + '/key1',
      Items[I].Shortcut.Key1, Items[I].Shortcut.DefaultKey1);
    Config.SetDeleteValue('inputs/' + Items[I].Name + '/key2',
      Items[I].Shortcut.Key2, Items[I].Shortcut.DefaultKey2);
    Config.SetDeleteValue('inputs/' + Items[I].Name + '/mouse_button_use',
      Items[I].Shortcut.MouseButtonUse, Items[I].Shortcut.DefaultMouseButtonUse);
    Config.SetDeleteValue('inputs/' + Items[I].Name + '/mouse_button',
      Ord(Items[I].Shortcut.MouseButton), Ord(Items[I].Shortcut.DefaultMouseButton));
    Config.SetDeleteValue('inputs/' + Items[I].Name + '/mouse_wheel',
      Ord(Items[I].Shortcut.MouseWheel), Ord(Items[I].Shortcut.DefaultMouseWheel));
  end;
end;

function SortInputConfiguration(const A, B: TInputConfiguration): Integer;
begin
  Result := A.GroupOrder - B.GroupOrder;
  { since TFPSList.Sort is not stable, we use Index to keep order predictable
    when GroupOrder is equal }
  if Result = 0 then
    Result := A.Index - B.Index;
end;

procedure TInputConfigurationList.LoadFromConfig(const Config: TCastleConfig);
var
  I: Integer;
  ConflictDescription: string;
  G: TInputGroup;
begin
  { we assume that all inputs are added now, so we do some finalizing operations
    now, like checking defaults for conflicts and sorting by GroupOrder. }
  if SeekConflict(ConflictDescription) then
    raise EInternalError.Create(
      'Default key/mouse shortcuts layout has conflicts: ' + ConflictDescription);

  for G := Low(G) to High(G) do
    InputsGroup[G].Sort(@SortInputConfiguration);

  for I := 0 to Count - 1 do
  begin
    Items[I].Shortcut.Key1 := Config.GetValue(
      'inputs/' + Items[I].Name + '/key1', Items[I].Shortcut.DefaultKey1);
    Items[I].Shortcut.Key2 := Config.GetValue(
      'inputs/' + Items[I].Name + '/key2', Items[I].Shortcut.DefaultKey2);
    Items[I].Shortcut.MouseButtonUse := Config.GetValue(
      'inputs/' + Items[I].Name + '/mouse_button_use',
      Items[I].Shortcut.DefaultMouseButtonUse);
    Items[I].Shortcut.MouseButton := TMouseButton(Config.GetValue(
      'inputs/' + Items[I].Name + '/mouse_button',
      Ord(Items[I].Shortcut.DefaultMouseButton)));
    Items[I].Shortcut.MouseWheel := TMouseWheelDirection(Config.GetValue(
      'inputs/' + Items[I].Name + '/mouse_wheel',
      Ord(Items[I].Shortcut.DefaultMouseWheel)));
  end;

  if SeekConflict(ConflictDescription) then
  begin
    WarningWrite(
      'Your key/mouse shortcuts layout has conflicts. This can happen if you ' +
      'just upgraded the game to newer version, and the newer version has new ' +
      'key/mouse shortcuts or has different default key/mouse shortcuts than previous ' +
      'version. It can also happen if you manually edited the configuration ' +
      'file. I will reset your key/mouse shortcuts to default now.' +nl+
      nl+
      'Detailed conflict description: ' + ConflictDescription);
    RestoreDefaults;
  end;
end;

function TInputConfigurationList.SeekConflict(
  out ConflictDescription: string): boolean;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := I + 1 to Count - 1 do
    begin
      if Items[J].Shortcut.IsKey(Items[I].Shortcut.Key1, #0) or
         Items[J].Shortcut.IsKey(Items[I].Shortcut.Key2, #0) or
         (Items[I].Shortcut.MouseButtonUse and
           Items[J].Shortcut.IsMouseButton(Items[I].Shortcut.MouseButton)) then
      begin
        ConflictDescription := Format('"%s" conflicts with "%s"',
          [Items[I].Caption, Items[J].Caption]);
        Exit(true);
      end;
    end;
  Result := false;
end;

function TInputConfigurationList.FindName(const Name: string): TInputConfiguration;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  Result := nil;
end;

{ TInputChangedEventList -------------------------------------------------- }

procedure TInputChangedEventList.ExecuteAll(
  InputConfiguration: TInputConfiguration);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](InputConfiguration);
end;

{ TInputConfiguration ---------------------------------------------------------- }

constructor TInputConfiguration.Create(const ACaption: string;
  const AName: string;
  const AGroup: TInputGroup;
  const AKey1: TKey;
  const AKey2: TKey;
  const ACharacter: Char;
  const AMouseButtonUse: boolean;
  const AMouseButton: TMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  inherited Create;
  FCaption := ACaption;
  FName := AName;
  FGroup := AGroup;

  FShortcut := TInputShortcut.Create(nil);
  FShortcut.Assign(AKey1, AKey2, ACharacter, AMouseButtonUse, AMouseButton, AMouseWheel);

  Index := InputsAll.Count;
  InputsAll.Add(Self);

  InputsGroup[Group].Add(Self);
end;

destructor TInputConfiguration.Destroy;
begin
  FreeAndNil(FShortcut);
  inherited;
end;

procedure TInputConfiguration.AddShortcut(const NewKey: TKey;
  const NewMousePress: boolean; const NewMouseButton: TMouseButton;
  const NewMouseWheel: TMouseWheelDirection);
begin
  if NewMousePress then
  begin
    Shortcut.MouseButtonUse := NewMousePress;
    Shortcut.MouseButton := NewMouseButton;
  end else
  if NewMouseWheel <> mwNone then
    Shortcut.MouseWheel := NewMouseWheel else
  if Shortcut.Key1 = K_None then
    Shortcut.Key1 := NewKey else
  if Shortcut.Key2 = K_None then
    Shortcut.Key2 := NewKey else
  begin
    { We move the previous Key1 to Key2, and set Key1 to new key.
      This looks nice for user when Shortcut is displayed as the
      menu argument. }
    Shortcut.Key2 := Shortcut.Key1;
    Shortcut.Key1 := NewKey;
  end;
end;

function TInputConfiguration.Description: string;
begin
  Result := Shortcut.Description(Format('"%s" key', [Caption]));
end;

{ TCasScriptShortcut --------------------------------------------------------- }

type
  TCasScriptShortcut = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class procedure Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

class function TCasScriptShortcut.ShortName: string;
begin
  Result := 'shortcut';
end;

class procedure TCasScriptShortcut.Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  N: string;
  I: TInputConfiguration;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptString);
  N := TCasScriptString(Arguments[0]).Value;
  if InputsAll <> nil then
  begin
    I := InputsAll.FindName(N);
    if I <> nil then
      TCasScriptString(AResult).Value := I.Description else
      TCasScriptString(AResult).Value := Format('(shortcut name "%s" undefined)', [N]);
  end else
    TCasScriptString(AResult).Value := 'input names not available (finalization of CastleInputs unit is already done)';
end;

{ initialization / finalization ---------------------------------------------- }

procedure DoInitialization;
var
  G: TInputGroup;
begin
  InputsAll := TInputConfigurationList.Create(true);

  for G := Low(InputsGroup) to High(InputsGroup) do
    InputsGroup[G] := TInputConfigurationList.Create(false);

  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  { Basic shortcuts. }
  CastleInput_Forward := TInputConfiguration.Create('Move forward', 'move_forward', igBasic,
    K_W, K_Up, #0, false, mbLeft);
  CastleInput_Backward := TInputConfiguration.Create('Move backward', 'move_backward', igBasic,
    K_S, K_Down, #0, false, mbLeft);
  CastleInput_LeftStrafe := TInputConfiguration.Create('Move left', 'move_left', igBasic,
    K_A, K_None, #0, false, mbLeft);
  CastleInput_RightStrafe := TInputConfiguration.Create('Move right', 'move_right', igBasic,
    K_D, K_None, #0, false, mbLeft);
  CastleInput_LeftRot := TInputConfiguration.Create('Turn left', 'turn_left', igBasic,
    K_Left, K_None, #0, false, mbLeft);
  CastleInput_RightRot := TInputConfiguration.Create('Turn right', 'turn_right', igBasic,
    K_Right, K_None, #0, false, mbLeft);
  CastleInput_UpRotate := TInputConfiguration.Create('Look up', 'look_up', igBasic,
    K_PageDown, K_None, #0, false, mbLeft);
  CastleInput_DownRotate := TInputConfiguration.Create('Look down', 'look_down', igBasic,
    K_Delete, K_None, #0, false, mbLeft);
  CastleInput_GravityUp := TInputConfiguration.Create('Look straight', 'look_straight', igBasic,
    K_End, K_None, #0, false, mbLeft);
  CastleInput_UpMove := TInputConfiguration.Create('Jump (or fly/swim up)', 'move_up', igBasic,
    K_Space, K_None, #0, true, mbRight);
  CastleInput_DownMove := TInputConfiguration.Create('Crouch (or fly/swim down)', 'move_down', igBasic,
    K_C, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  CastleInput_Interact := TInputConfiguration.Create('Interact (press button / open door etc.)', 'interact', igOther,
    K_E, K_None, #0, false, mbLeft);

  Config.OnLoad.Add(@InputsAll.LoadFromConfig);
  Config.OnSave.Add(@InputsAll.SaveToConfig);

  FunctionHandlers.RegisterHandler(@TCasScriptShortcut(nil).Handle, TCasScriptShortcut, [TCasScriptString], false);
end;

procedure DoFinalization;
var
  G: TInputGroup;
begin
  if (InputsAll <> nil) and (Config <> nil) then
  begin
    Config.OnLoad.Remove(@InputsAll.LoadFromConfig);
    Config.OnSave.Remove(@InputsAll.SaveToConfig);
  end;

  for G := Low(InputsGroup) to High(InputsGroup) do
    FreeAndNil(InputsGroup[G]);

  FreeAndNil(InputsAll);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.