{
  Copyright 2003-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Key and mouse shortcuts (TInputShortcut) and global management of them.

  See https://castle-engine.io/manual_key_mouse.php for overview.

  TInputShortcut instance represents a key/mouse shortcut.
  Instances of this class are spread throughout the engine.
  We have two different ways of using TInputShortcut instance:

  @unorderedList(
    @item(
      @bold(Global)

      TInputShortcut instance with TInputShortcut.Group <> igLocal
      is called "global". Such instance is automatically (at construction)
      added to InputsAll and InputsGroup(Group) lists.

      The purpose of such global input map is to be able to detect key conflicts,
      be able to restore whole input map to default,
      load/save them to the user config file, and so on.
      For example @link(CastleViewport.Input_Interact) is global.

      Global shortcuts are owned (they will be freed by) this unit
      (more specifically, they will be freed by InputsAll).
      When creating them, pass @nil to the Owner parameter
      of constructor TInputShortcut.Create.
      This implies that InputsAll and InputsGroup(Group) lists will never shrink,
      which is useful --- once added, shortcuts will not disappear.
      Global TInputShortcut instances are always in practice also global variables.
    )

    @item(
      @bold(Local)

      TInputShortcut instance with TInputShortcut.Group = igLocal
      is called "local". It means it's a normal component,
      it's not automatically present on any global list,
      so it doesn't conflict with other global shortcuts.

      For example TCastleWalkNavigation contains a number of them like
      TCastleWalkNavigation.Input_Forward.
    )
  )

  You create new inputs by simply constructing new TInputShortcut instances.
  Make sure you add all global inputs before calling @code(Config.Load),
  as some functionality assumes that all shortcuts are already added
  at the time @code(Config.Load) is called.
  The engine units themselves never call @code(Config.Load), it is left
  to the final application. }
unit CastleInputs;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections, Contnrs,
  CastleKeysMouse, CastleUtils, CastleClassUtils,
  CastleXMLConfig
  {$ifndef CASTLE_STRICT_CLI} , CastleUIControls, CastleGameControllers {$endif};

type
  { Type of input, for TInputShortcut.Group. }
  TInputGroup = (igLocal, igBasic, igItems, igOther);
  TInputGroupNotLocal = igBasic..High(TInputGroup);

  { Abstract input binding, like a given key / mouse button / game contoller
    press. The TInputShortcut can be triggered by one or more of these. }
  TInputShortcutBinding = class(TComponent)
  public
    {$ifndef CASTLE_STRICT_CLI}
    { Is the input currently pressed.
      If returns @true, HowMuch is set to a value between 0.0 and 1.0
      that says "how much" the input is pressed (e.g. with what pressure
      is the game controller axis pressed).
      If returns @false, HowMuch is always 0.0. }
    function IsPressed(const Container: TCastleContainer;
      out HowMuch: Single): Boolean; virtual;
    {$endif}

    { Does receiving @link(TCastleUserInterface) press with this Event
      means that this input has been just pressed. }
    function IsEvent(const Event: TInputPressRelease): Boolean; virtual;

    { Description of the binding. }
    function ToString: String; override;
  end;

  { List of TInputShortcutBinding.
    This is a TComponentList, so removed components are auto-removed from this list. }
  TInputShortcutBindingList = class(TComponentList)
    {$ifndef CASTLE_STRICT_CLI}
    { Is the input currently pressed. }
    function IsPressed(const Container: TCastleContainer;
      out HowMuch: Single): Boolean;
    {$endif}

    { Does receiving @link(TCastleUserInterface) press with this Event
      means that this input has been just pressed. }
    function IsEvent(const Event: TInputPressRelease): Boolean;

    { Description of all bindings, separated by ' or '
      Empty string if the list is empty. }
    function ToString: String; override;
  end;

  TGameControllerAxis = (
    gaLeftStick,
    gaRightStick,
    gaLeftTrigger,
    gaRightTrigger
  );

  { Trigger TInputShortcut by a game controller axis.
    For example, trigger action when user moves the left stick of the controller.
    Right now, this only causes IsPressed (so axis can be detected
    as pressed) and never reacts tp IsEvent (since axis doesn't report there). }
  TInputShortcutBindingControllerAxis = class(TInputShortcutBinding)
  strict private
    FAxis: TGameControllerAxis;
    FPositive: Boolean;
    FCoord: T2DAxis;
    FDeadZone: Single;
    FControllerIndex: Integer;
  public
    const
      DefaultDeadZone = 0.1;

    { Which game contoller axis is taken into account. }
    property Axis: TGameControllerAxis read FAxis write FAxis default gaLeftStick;

    { Whether the posititive or negative value of the axis causes the pressed state. }
    property Positive: Boolean read FPositive write FPositive default true;

    { Which axis coordinate is taken into account.

      @unorderedList(
        @item 0 means to look at axis X,
        @item 1 (only available for 2D axes) means to look at axis Y coordinate.
      ) }
    property Coord: T2DAxis read FCoord write FCoord default 0;

    { Values of the axis smaller (or equal) than this DeadZone are ignored.
      This makes sense when it's 0.0 (no deadzone, any value <> 0 will trigger)
      or values < 1.0 (like 0.1, to ignore small movement of the axis). }
    property DeadZone: Single read FDeadZone write FDeadZone
      {$ifdef FPC}default DefaultDeadZone{$endif};

    { Which game controller is taken into account.
      Value -1 (default) means to look at all controllers and choose maximum. }
    property ControllerIndex: Integer read FControllerIndex write FControllerIndex default -1;

    constructor Create(AOwner: TComponent); override;
    {$ifndef CASTLE_STRICT_CLI}
    function IsPressed(const Container: TCastleContainer;
      out HowMuch: Single): Boolean; override;
    {$endif}
    //function IsEvent(const Event: TInputPressRelease): Boolean; override;
    function ToString: String; override;
  end;

  { Trigger TInputShortcut by a game controller button.
    For example, trigger action when user clicks "A" or "right bumper". }
  TInputShortcutBindingControllerButton = class(TInputShortcutBinding)
  strict private
    FButton: TGameControllerButton;
    FControllerIndex: Integer;
  public
    { Which game controller button is taken into account. }
    property Button: TGameControllerButton read FButton write FButton
      default gbNorth;

    { Which game controller is taken into account.
      Value -1 (default) means to look at all controllers and choose maximum. }
    property ControllerIndex: Integer read FControllerIndex write FControllerIndex
      default -1;

    constructor Create(AOwner: TComponent); override;
    {$ifndef CASTLE_STRICT_CLI}
    // function IsPressed(const Container: TCastleContainer;
    //   out HowMuch: Single): Boolean; override;
    {$endif}
    function IsEvent(const Event: TInputPressRelease): Boolean; override;
    function ToString: String; override;
  end;

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
    @italic("KeyString") is an UTF-8 character that can be expressed as String value.

    They are keys like "control key" (keyCtrl) or "shift key" (keyShift)
    that cannot be expressed (on their own) as KeyString.
    KeyString is sometimes more, sometimes less specific than Key:
    character "A" (upper letter "a")
    is activated by pressing "a", but only when Shift is pressed or CapsLock is on
    (window system / GUI toolkit may also allow other ways to input characters).

    Name of this component is additionally used if this is a global
    input shortcut (with Group <> igLocal, see CastleInputs unit documentation):
    it is used for config file entries and for the CastleScript
    @code(shortcut()) function
    [https://castle-engine.io/castle_script.php#function_shortcut] .
    Any valid Pascal identifier will be Ok for these purposes. }
  TInputShortcut = class(TComponent)
  private
    FKey1: TKey;
    FKey2: TKey;
    FKeyString: String;
    FMouseButtonUse: boolean;
    FMouseButton: TCastleMouseButton;
    FMouseButtonCheckModifiers: TModifierKeys;
    FMouseButtonModifiers: TModifierKeys;
    FMouseButton2Use: boolean;
    FMouseButton2: TCastleMouseButton;
    FMouseButton2CheckModifiers: TModifierKeys;
    FMouseButton2Modifiers: TModifierKeys;
    FMouseWheel: TMouseWheelDirection;

    FDefaultKey1: TKey;
    FDefaultKey2: TKey;
    FDefaultKeyString: String;
    FDefaultMouseButtonUse: boolean;
    FDefaultMouseButton: TCastleMouseButton;
    FDefaultMouseButtonCheckModifiers: TModifierKeys;
    FDefaultMouseButtonModifiers: TModifierKeys;
    FDefaultMouseButton2Use: boolean;
    FDefaultMouseButton2: TCastleMouseButton;
    FDefaultMouseButton2CheckModifiers: TModifierKeys;
    FDefaultMouseButton2Modifiers: TModifierKeys;
    FDefaultMouseWheel: TMouseWheelDirection;

    FCaption: string;
    FGroup: TInputGroup;
    FGroupOrder: Integer;
    { Index of InputsAll. For now this is useful only for sorting,
      to decide order when GroupOrder is equal between two items. }
    Index: Integer;
    FBindings: TInputShortcutBindingList;

    procedure SetKey1(const Value: TKey);
    procedure SetKey2(const Value: TKey);
    procedure SetKeyString(const Value: String);
    function GetCharacter: Char;
    procedure SetCharacter(const AValue: Char);
    procedure SetMouseButtonUse(const Value: boolean);
    procedure SetMouseButton(const Value: TCastleMouseButton);
    procedure SetMouseButtonCheckModifiers(const Value: TModifierKeys);
    procedure SetMouseButtonModifiers(const Value: TModifierKeys);
    procedure SetMouseButton2Use(const Value: boolean);
    procedure SetMouseButton2(const Value: TCastleMouseButton);
    procedure SetMouseButton2CheckModifiers(const Value: TModifierKeys);
    procedure SetMouseButton2Modifiers(const Value: TModifierKeys);
    procedure SetMouseWheel(const Value: TMouseWheelDirection);
  protected
    { Called always right after the shortcut value changed (like key or mouse buton).
      Called only when the current values changed,
      not called when just the default values changed. }
    procedure Changed; virtual;
  public
    { Constructor that always creates local shortcuts (with Group = igLocal).
      Caption and Name are left empty (they do not have to be set for
      local shortcuts; although you may wish to later assign Name anyway,
      if you use this as sub-component in Lazarus). }
    constructor Create(AOwner: TComponent); overload; override;

    { Flexible constructor that allows to set Group and choose global or local
      shortcut. }
    constructor Create(const AOwner: TComponent;
      const ACaption: string;
      const AName: string;
      const AGroup: TInputGroup); reintroduce; overload;

    destructor Destroy; override;

    procedure MakeDefault;

    { Assigns to this object the default values from Source. }
    procedure AssignFromDefault(Source: TInputShortcut);

    { Copy Source properties to this object.
      It always copies current properties (Key1, MouseButton etc.),
      and only optionally (if CopyDefaults) also copies the DefaultXxx properties. }
    procedure Assign(Source: TInputShortcut; CopyDefaults: boolean); reintroduce; overload;

    { Set keys/mouse buttons of this shortcut.

      Sets both current and default properties (e.g. both @link(Key1) and
      @link(DefaultKey1) to the same value).
      Note that, right after using this method, saving the input to a config file
      (using TInputShortcutList.SaveToConfig) will actually just "clear"
      the input information from the config file (because we do not save
      the value when it is equal to the default).
      Use @link(AssignCurrent) to only assign the curent value, leaving default
      untouched. }
    procedure Assign(
      const AKey1: TKey;
      const AKey2: TKey = keyNone;
      AKeyString: String = '';
      const AMouseButtonUse: boolean = false;
      const AMouseButton: TCastleMouseButton = buttonLeft;
      const AMouseWheel: TMouseWheelDirection = mwNone); reintroduce; overload;

    { Set keys/mouse buttons of this shortcut.
      Sets only the "current" properties (e.g. it changes @link(Key1),
      leaving @link(DefaultKey1) unmodified). }
    procedure AssignCurrent(
      const AKey1: TKey;
      const AKey2: TKey = keyNone;
      AKeyString: String = '';
      const AMouseButtonUse: boolean = false;
      const AMouseButton: TCastleMouseButton = buttonLeft;
      const AMouseWheel: TMouseWheelDirection = mwNone);

    { Make this input impossible to activate by the user.
      This sets both keys to keyNone, KeyString to '',
      both MouseButtonUse and MouseButton2Use to @false,
      and MouseWheel to mwNone. }
    procedure MakeClear(const ClearAlsoDefaultState: boolean = false);

    {$ifndef CASTLE_STRICT_CLI}
    { Is the input pressed.
      Checks the Container's pressed keys and mouse buttons. }
    function IsPressed(const Container: TCastleContainer): Boolean; overload;

    { Is the input pressed, and in addition return "how much" it is pressed.

      When result is @true, the additional output parameter HowMuch says
      "how much" input is pressed.

      This for now is only relevant for the game controllers axis, where

      @unorderedList(
        @item(HowMuch = 0.0 would mean it is exactly equal to
          @link(TInputShortcutBindingControllerAxis.DeadZone).
          This is actually not possible, "exactly equal to DeadZone"
          doesn't trigger the input, so it has to be at least very slightly
          above DeadZone.)
        @item(HowMuch = 1.0 means the axis (absolue) value is at maximum,
          which means 1.0.)
        @item(Values in between imply the input is somewhere in between.)
      )

      TODO: In the future, reading MouseLookDelta may also be possible here.
      which some field indicating "what delta is considered max".

      When result is @true but the device doesn't support "how much"
      measurement (it's not a gamepad axis), then HowMuch is just 1.0.

      When result is @false, HowMuch is always set to 0.0. }
    function IsPressed(const Container: TCastleContainer;
      out HowMuch: Single): Boolean; overload;
    {$endif}

    { Does receiving @link(TCastleUserInterface) press with this Event
      means that this input has been just pressed. }
    function IsEvent(const Event: TInputPressRelease): Boolean; overload;

    { Check does given Key or AKeyString correspond to this input shortcut.
      If Key = keyNone and AString = '', result is always @false. }
    function IsKey(const Key: TKey; AKeyString: String): boolean;
      deprecated 'use IsEvent';

    { Check does given mouse button correspond to this input shortcut. }
    function IsMouseButton(const AMouseButton: TCastleMouseButton;
      const ModifiersDown: TModifierKeys): boolean;
      deprecated 'use IsEvent';

    function IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;
      deprecated 'use IsEvent';

    { Describe the current value (which key, mouse buttons and such) of this
      shortcut. If there is no way to press this shortcut (all properties
      like Key1 and such are empty, like after MakeClear), we will use NoneString.

      The overloaded version without NoneString parameter will
      assume that NoneString should describe the shortcut @link(Caption).
      This way, if user cleared (deleted all key/mouse buttons assigned)
      the shortcut in the game configuration, and we show him a message like:

      @longCode(# 'Press ' + Input.Description + ' to do something' #)

      then user will see it as @code('Press "use" key to do something')
      and will know that (s)he should configure the "use" key.
      @groupBegin }
    function Description(const NoneString: string): string; overload;
    function Description: string; overload;
    { @groupEnd }

    { Modifier keys that are relevant to recognize this shortcut. }
    function Modifiers: TModifierKeys;

    { Nice name to show user. With spaces, localized characters etc. }
    property Caption: string read FCaption;

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

    { Add to shortcut new key or mouse button or mouse wheel. }
    procedure Add(const NewEvent: TInputPressRelease);

    { Load a particular input from a config file.
      Use this to load what was previously saved with @link(SaveToConfig). }
    procedure LoadFromConfig(const Config: TCastleConfig; ConfigPath: String);

    { Save a particular input to a config file.
      This creates an XML element named @code(Name) under the indicated ConfigPath
      in the config file.

      Note: It is often easier to group your controls in TInputShortcutList,
      and call @link(TInputShortcutList.SaveToConfig) to save everything. }
    procedure SaveToConfig(const Config: TCastleConfig; ConfigPath: String);
  published
    { Key/mouse properties on TInputShortcut are declared without
      "default" specifier, to always save them in Lazarus LFM file.
      Reason: various class (TCastleExamineNavigation, TCastleWalkNavigation, TCastleViewport)
      create them setting different default values.
      If we would declare that default for Key1 is keyNone,
      then you couldn't set in Lazarus e.g. TCastleWalkNavigation.Input_Forward.Key1 to keyNone.
      Such keyNone would not be saved to LFM (since it would equal Key1 default
      value), but when reading the LFM back it would change into keyArrowUp
      (because TCastleWalkNavigation creates Input_Forward with keyArrowUp by default). }

    { Key shortcuts for given command. You can set any of them to keyNone
      to indicate that no key is assigned.
      @groupBegin }
    property Key1: TKey read FKey1 write SetKey1;
    property Key2: TKey read FKey2 write SetKey2;
    { @groupEnd }

    { Character shortcut for given command, may be UTF-8 character (multi-byte).
      You can set this to '' to indicate that no character shortcut is assigned. }
    property KeyString: String read FKeyString write SetKeyString;

    {$ifdef FPC}
    property Character: Char read GetCharacter write SetCharacter;
      deprecated 'use KeyString';
    {$endif}

    { Mouse shortcut for given command.
      Only relevant if MouseButtonUse is @true.
      Then we check for MouseButton being pressed.

      MouseButtonCheckModifiers determines what subset of modifiers (Ctrl, Shift, Alt)
      to check, whether they match MouseButtonModifiers. For example,
      MouseButtonCheckModifiers = [mkShift, mkCtrl]
      and MouseButtonModifiers = [mkCtrl] means that you have to press Ctrl,
      and you cannot keep Shift pressed, to activate this shortcut.

      By default MouseButtonCheckModifiers and MouseButtonModifiers are both empty sets,
      which means that any state of modifiers is OK.

      @groupBegin }
    property MouseButtonUse: boolean read FMouseButtonUse write SetMouseButtonUse;
    property MouseButton: TCastleMouseButton read FMouseButton write SetMouseButton;
    property MouseButtonCheckModifiers: TModifierKeys read FMouseButtonCheckModifiers write SetMouseButtonCheckModifiers;
    property MouseButtonModifiers: TModifierKeys read FMouseButtonModifiers write SetMouseButtonModifiers;
    { @groupEnd }

    { Alternative mouse shortcut for given command.
      Only relevant if MouseButton2Use is @true.
      All properties work analogously to MouseButtonUse, MouseButton,
      MouseButtonCheckModifiers, MouseButtonModifiers.
      @groupBegin }
    property MouseButton2Use: boolean read FMouseButton2Use write SetMouseButton2Use;
    property MouseButton2: TCastleMouseButton read FMouseButton2 write SetMouseButton2;
    property MouseButton2CheckModifiers: TModifierKeys read FMouseButton2CheckModifiers write SetMouseButton2CheckModifiers;
    property MouseButton2Modifiers: TModifierKeys read FMouseButton2Modifiers write SetMouseButton2Modifiers;
    { @groupEnd }

    { Mouse wheel to activate this command. Note that mouse wheels cannot be
      continuously pressed (our method IsPressed doesn't look at it),
      so this is only suitable for commands that work in steps
      (not continuously). }
    property MouseWheel: TMouseWheelDirection read FMouseWheel
      write SetMouseWheel;

    { Default values for properties key/mouse.
      You can change them --- this will change what MakeDefault does.

      Note that setting these properties doesn't automatically set
      corresponding "current" property. E.g. @code(DefaultKey1 := keySpace;)
      doesn't change the value of Key1 property --- only DefaultKey1
      changes. You can explicitly change Key1 property, or just call
      MakeDefault afterwards, if you want this to happen.
      @groupBegin }
    property DefaultKey1: TKey read FDefaultKey1 write FDefaultKey1;
    property DefaultKey2: TKey read FDefaultKey2 write FDefaultKey2;
    property DefaultKeyString: String
      read FDefaultKeyString write FDefaultKeyString;

    property DefaultMouseButtonUse: boolean
      read FDefaultMouseButtonUse write FDefaultMouseButtonUse;
    property DefaultMouseButton: TCastleMouseButton
      read FDefaultMouseButton write FDefaultMouseButton;
    property DefaultMouseButtonCheckModifiers: TModifierKeys
      read FDefaultMouseButtonCheckModifiers write FDefaultMouseButtonCheckModifiers;
    property DefaultMouseButtonModifiers: TModifierKeys
      read FDefaultMouseButtonModifiers write FDefaultMouseButtonModifiers;

    property DefaultMouseButton2Use: boolean
      read FDefaultMouseButton2Use write FDefaultMouseButton2Use;
    property DefaultMouseButton2: TCastleMouseButton
      read FDefaultMouseButton2 write FDefaultMouseButton2;
    property DefaultMouseButton2CheckModifiers: TModifierKeys
      read FDefaultMouseButton2CheckModifiers write FDefaultMouseButton2CheckModifiers;
    property DefaultMouseButton2Modifiers: TModifierKeys
      read FDefaultMouseButton2Modifiers write FDefaultMouseButton2Modifiers;

    property DefaultMouseWheel: TMouseWheelDirection
      read FDefaultMouseWheel write FDefaultMouseWheel;
    { @groupEnd }

    { Add here any additonal bindings. Use this now to react to game controller
      axis and buttons. }
    property Bindings: TInputShortcutBindingList read FBindings;

    { }
    { TODO: Maybe introduce a way to limit (TKey, or all shortcuts?)
      to activate only when specific modifier is pressed.

      Right now both TCastleWalkNavigation and TCastleExamineNavigation check modifiers
      and have not configurable behavior:

      - TCastleWalkNavigation allows inputs only when modifiers = [].
        Except Input_Right/LeftRot and Input_Up/DownRotate that have special
        meaning when Ctrl is pressed (see TCastleWalkNavigation.AllowSlowerRotations).
      - TCastleExamineNavigation allows Inputs_Move only when modifiers = [mkCtrl].
        Other TCastleExamineNavigation are allowed only when modifiers = []. }
  end;

  { Group of TInputShortcut, to easily manage (search, load, save...)
    the inputs. }
  TInputShortcutList = class({$ifdef FPC}specialize{$endif} TObjectList<TInputShortcut>)
  public
    { Find shortcut by name, returns @nil if not found. }
    function FindName(const Name: string): TInputShortcut;

    { Seeks for a shortcut that has matching key or mouse button or mouse wheel.
      @nil if not found. }
    function SeekMatchingShortcut(const Event: TInputPressRelease): TInputShortcut;
    procedure RestoreDefaults;
    function SeekConflict(out ConflictDescription: string): boolean;

    { Load customized input shortcuts from a config file,
      for example from @link(UserConfig).

      This should be used to load inputs previously saved with @link(SaveToConfig).
      Provide the same value of ConfigPath as you used with @link(SaveToConfig). }
    procedure LoadFromConfig(const Config: TCastleConfig; ConfigPath: String = '');

    { Save customized input shortcuts to a config file,
      for example to a @link(UserConfig).

      This will create an XML element <input> under an indicated ConfigPath
      in the config file. Be sure to save each unique TInputShortcutList
      with a different ConfigPath, to not collide in the config file.

      Note that we only save to config file the values when they differ
      from default. E.g. TInputShortcut.Key1 will be explicitly saved
      only if it is different than TInputShortcut.DefaultKey1.
      Otherwise the information about this Key1 for this TInputShortcut
      will be removed from the config file (to force using default next time
      this config is loaded). }
    procedure SaveToConfig(const Config: TCastleConfig; ConfigPath: String = '');
  end;

{ List of all global inputs.
  All TInputShortcut instances with group other than igLocal
  will be automatically added here. }
function InputsAll: TInputShortcutList;

function InputsGroup(const Group: TInputGroupNotLocal): TInputShortcutList;

implementation

uses SysUtils, Generics.Defaults, TypInfo, Math,
  CastleStringUtils, CastleVectors, CastleLog;

{ TInputShortcTInputShortcutBinding ------------------------------------------ }

{$ifndef CASTLE_STRICT_CLI}
function TInputShortcutBinding.IsPressed(
  const Container: TCastleContainer; out HowMuch: Single): Boolean;
begin
  HowMuch := 0.0;
  Result := false;
end;
{$endif}

function TInputShortcutBinding.IsEvent(
  const Event: TInputPressRelease): Boolean;
begin
  Result := false;
end;

function TInputShortcutBinding.ToString: String;
begin
  Result := '';
end;

{ TInputShortcutBindingList -------------------------------------------------- }

{$ifndef CASTLE_STRICT_CLI}
function TInputShortcutBindingList.IsPressed(
  const Container: TCastleContainer; out HowMuch: Single): Boolean;
var
  Binding: TInputShortcutBinding;
  I: Integer;
begin
  // default result, for empty list
  HowMuch := 0.0;
  Result := false;

  for I := 0 to Count - 1 do
  begin
    Binding := Items[I] as TInputShortcutBinding;
    if Binding.IsPressed(Container, HowMuch) then
      Exit(true);
  end;
end;
{$endif}

function TInputShortcutBindingList.IsEvent(
  const Event: TInputPressRelease): Boolean;
var
  Binding: TInputShortcutBinding;
  I: Integer;
begin
  Result := false;

  for I := 0 to Count - 1 do
  begin
    Binding := Items[I] as TInputShortcutBinding;
    if Binding.IsEvent(Event) then
      Exit(true);
  end;
end;

function TInputShortcutBindingList.ToString: String;
var
  Binding: TInputShortcutBinding;
  BindingStr: String;
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Binding := Items[I] as TInputShortcutBinding;
    BindingStr := Binding.ToString;
    if BindingStr <> '' then
      Result := SAppendPart(Result, ' or ', BindingStr);
  end;
end;

{ TInputShortcutBindingControllerAxis ---------------------------------------- }

constructor TInputShortcutBindingControllerAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAxis := gaLeftStick;
  FPositive := true;
  FCoord := 0;
  FDeadZone := DefaultDeadZone;
  FControllerIndex := -1; // -1 means all controllers
end;

{$ifndef CASTLE_STRICT_CLI}
function TInputShortcutBindingControllerAxis.IsPressed(
  const Container: TCastleContainer; out HowMuch: Single): Boolean;

  { Check IsPressed for a particular controller index.
    ControllerIndex must be valid (>= 0 and < Controllers.Count).
    Returns is the input pressed, and how much, on this controller. }
  function ForController(const ControllerIndex: Integer;
    out HowMuch: Single): Boolean;
  var
    AxisValue2D: TVector2;
    AxisValue: Single;
  begin
    HowMuch := 0.0;
    Result := false;

    Assert(ControllerIndex >= 0);
    Assert(ControllerIndex < Controllers.Count);

    // calculate AxisValue2D
    case Axis of
      gaLeftStick:
        AxisValue2D := Controllers[ControllerIndex].AxisLeft;
      gaRightStick:
        AxisValue2D := Controllers[ControllerIndex].AxisRight;
      gaLeftTrigger:
        AxisValue2D := Vector2(Controllers[ControllerIndex].AxisLeftTrigger, 0);
      gaRightTrigger:
        AxisValue2D := Vector2(Controllers[ControllerIndex].AxisRightTrigger, 0);
      {$ifdef COMPILER_CASE_ANALYSIS}
      else raise Exception.CreateFmt('Unknown TGameControllerAxis %d', [Ord(Axis)]);
      {$endif}
    end;

    // calculate AxisValue
    AxisValue := AxisValue2D.Items[Coord];

    { Check DeadZone and Positive.
      Use strict equality for DeadZone, so that DeadZone = 0.0
      still doesn't activate when input is zero. }
    Assert(DeadZone >= 0.0);
    if (     Positive  and (AxisValue >  DeadZone)) or
       ((not Positive) and (AxisValue < -DeadZone)) then
    begin
      Result := true;
      HowMuch := MapRangeTo01(Abs(AxisValue), DeadZone, 1.0);
    end;
  end;

var
  ControllerResult: Boolean;
  ControllerHowMuch: Single;
  I: Integer;
begin
  HowMuch := 0.0;
  Result := false;

  if ControllerIndex = -1 then
  begin
    for I := 0 to Controllers.Count - 1 do
    begin
      ControllerResult := ForController(I, ControllerHowMuch);
      if ControllerResult then
      begin
        if not Result then
        begin
          Result := true;
          HowMuch := ControllerHowMuch;
        end else
        begin
          Assert(Result and ControllerResult);
          // we take maximum of HowMuch
          HowMuch := Max(HowMuch, ControllerHowMuch);
        end;
      end;
    end;
  end else
  if ControllerIndex < Controllers.Count then
    Result := ForController(ControllerIndex, HowMuch)
  else
    WritelnWarning('TInputShortcutBindingControllerAxis refers to non-existing controlled index %d', [
      ControllerIndex
    ]);
end;
{$endif}

function TInputShortcutBindingControllerAxis.ToString: String;
var
  AxisString: String;
begin
  AxisString := GetEnumName(TypeInfo(TGameControllerAxis), Ord(FAxis));
  Result := Format('Controller axis %s (positive %s, coord %d, deadzone %f, controller index %d): ', [
    AxisString,
    BoolToStr(FPositive, true),
    FCoord,
    FDeadZone,
    FControllerIndex
  ]);
end;

{ TInputShortcutBindingControllerButton ------------------------------------- }

constructor TInputShortcutBindingControllerButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := gbNorth; // default button
  FControllerIndex := -1; // -1 means all controllers
end;

function TInputShortcutBindingControllerButton.IsEvent(
  const Event: TInputPressRelease): Boolean;
begin
  Result :=
    (Event.EventType = itGameController) and
    ( (ControllerIndex = -1) or
      (Event.Controller.ControllerIndex = ControllerIndex) ) and
    (Event.Controller.Button = Button);
end;

function TInputShortcutBindingControllerButton.ToString: String;
var
  ButtonString: String;
begin
  ButtonString := GetEnumName(TypeInfo(TGameControllerButton), Ord(FButton));
  Result := Format('Controller button %s (controller index %d): ', [
    ButtonString,
    FControllerIndex
  ]);
end;

{ TInputShortcut ------------------------------------------------------------- }

constructor TInputShortcut.Create(const AOwner: TComponent;
  const ACaption: string;
  const AName: string;
  const AGroup: TInputGroup);
begin
  inherited Create(AOwner);
  FCaption := ACaption;
  Name := AName;
  FGroup := AGroup;
  FBindings := TInputShortcutBindingList.Create;

  if Group <> igLocal then
  begin
    Index := InputsAll.Count;
    InputsAll.Add(Self);
    InputsGroup(Group).Add(Self);
  end;
end;

constructor TInputShortcut.Create(AOwner: TComponent);
begin
  Create(AOwner, '', '', igLocal);
end;

destructor TInputShortcut.Destroy;
begin
  FreeAndNil(FBindings);
  inherited Destroy;
end;

function TInputShortcut.GetCharacter: Char;
begin
  if Length(KeyString) = 1 then
    Result := KeyString[1]
  else
    Result := #0;
end;

procedure TInputShortcut.SetCharacter(const AValue: Char);
begin
  if AValue = #0 then
    KeyString := ''
  else
    KeyString := AValue;
end;

procedure TInputShortcut.MakeDefault;
begin
  AssignFromDefault(Self);
end;

procedure TInputShortcut.AssignFromDefault(Source: TInputShortcut);
begin
  FKey1 := Source.DefaultKey1;
  FKey2 := Source.DefaultKey2;
  FKeyString := Source.DefaultKeyString;
  FMouseButtonUse := Source.DefaultMouseButtonUse;
  FMouseButton := Source.DefaultMouseButton;
  FMouseButtonCheckModifiers := Source.DefaultMouseButtonCheckModifiers;
  FMouseButtonModifiers := Source.DefaultMouseButtonModifiers;
  FMouseButton2Use := Source.DefaultMouseButton2Use;
  FMouseButton2 := Source.DefaultMouseButton2;
  FMouseButton2CheckModifiers := Source.DefaultMouseButton2CheckModifiers;
  FMouseButton2Modifiers := Source.DefaultMouseButton2Modifiers;
  FMouseWheel := Source.DefaultMouseWheel;
  FBindings.Clear; // TODO: DefaultXxx do not express additional bindings now

  { we don't set here properties, but directly set FXxx fields,
    so that we can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.Assign(const AKey1: TKey;
  const AKey2: TKey;
  AKeyString: String;
  const AMouseButtonUse: boolean;
  const AMouseButton: TCastleMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  // only for backward compatibility (when this parameter was Char) convert #0 to ''
  if AKeyString = #0 then
    AKeyString := '';

  FDefaultKey1 := AKey1;
  FDefaultKey2 := AKey2;
  FDefaultKeyString := AKeyString;
  FDefaultMouseButtonUse := AMouseButtonUse;
  FDefaultMouseButton := AMouseButton;
  FDefaultMouseButtonCheckModifiers := []; // not set by parameters, just reset
  FDefaultMouseButtonModifiers := []; // not set by parameters, just reset
  FDefaultMouseButton2Use := false; // not set by parameters, just reset
  FDefaultMouseButton2 := buttonLeft; // not set by parameters, just reset
  FDefaultMouseButton2CheckModifiers := []; // not set by parameters, just reset
  FDefaultMouseButton2Modifiers := []; // not set by parameters, just reset
  FDefaultMouseWheel := AMouseWheel;
  MakeDefault;
end;

procedure TInputShortcut.AssignCurrent(const AKey1: TKey;
  const AKey2: TKey;
  AKeyString: String;
  const AMouseButtonUse: boolean;
  const AMouseButton: TCastleMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  // only for backward compatibility (when this parameter was Char) convert #0 to ''
  if AKeyString = #0 then
    AKeyString := '';

  FKey1 := AKey1;
  FKey2 := AKey2;
  FKeyString := AKeyString;
  FMouseButtonUse := AMouseButtonUse;
  FMouseButton := AMouseButton;
  FMouseButtonCheckModifiers := []; // not set by parameters, just reset
  FMouseButtonModifiers := []; // not set by parameters, just reset
  FMouseButton2Use := false; // not set by parameters, just reset
  FMouseButton2 := buttonLeft; // not set by parameters, just reset
  FMouseButton2CheckModifiers := []; // not set by parameters, just reset
  FMouseButton2Modifiers := []; // not set by parameters, just reset
  FMouseWheel := AMouseWheel;
  FBindings.Clear; // TODO: not copying FBindings now
  Changed;
end;

procedure TInputShortcut.Assign(Source: TInputShortcut; CopyDefaults: boolean);
begin
  if CopyDefaults then
  begin
    DefaultKey1 := Source.DefaultKey1;
    DefaultKey2 := Source.DefaultKey2;
    DefaultKeyString := Source.DefaultKeyString;
    DefaultMouseButtonUse := Source.DefaultMouseButtonUse;
    DefaultMouseButton := Source.DefaultMouseButton;
    DefaultMouseButtonCheckModifiers := Source.DefaultMouseButtonCheckModifiers;
    DefaultMouseButtonModifiers := Source.DefaultMouseButtonModifiers;
    DefaultMouseButton2Use := Source.DefaultMouseButton2Use;
    DefaultMouseButton2 := Source.DefaultMouseButton2;
    DefaultMouseButton2CheckModifiers := Source.DefaultMouseButton2CheckModifiers;
    DefaultMouseButton2Modifiers := Source.DefaultMouseButton2Modifiers;
    DefaultMouseWheel := Source.DefaultMouseWheel;
  end;

  FKey1 := Source.Key1;
  FKey2 := Source.Key2;
  FKeyString := Source.KeyString;
  FMouseButtonUse := Source.MouseButtonUse;
  FMouseButton := Source.MouseButton;
  FMouseButtonCheckModifiers := Source.MouseButtonCheckModifiers;
  FMouseButtonModifiers := Source.MouseButtonModifiers;
  FMouseButton2Use := Source.MouseButton2Use;
  FMouseButton2 := Source.MouseButton2;
  FMouseButton2CheckModifiers := Source.MouseButton2CheckModifiers;
  FMouseButton2Modifiers := Source.MouseButton2Modifiers;
  FMouseWheel := Source.MouseWheel;

  FBindings.Clear; // TODO: not copying FBindings now

  { we don't set here properties, but directly set FXxx fields,
    so that we can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.MakeClear(const ClearAlsoDefaultState: boolean);
begin
  FKey1 := keyNone;
  FKey2 := keyNone;
  FKeyString := '';
  FMouseButtonUse := false;
  FMouseButton := buttonLeft;
  FMouseButtonCheckModifiers := [];
  FMouseButtonModifiers := [];
  FMouseButton2Use := false;
  FMouseButton2 := buttonLeft;
  FMouseButton2CheckModifiers := [];
  FMouseButton2Modifiers := [];
  FMouseWheel := mwNone;
  FBindings.Clear;

  if ClearAlsoDefaultState then
  begin
    FDefaultKey1 := keyNone;
    FDefaultKey2 := keyNone;
    FDefaultKeyString := '';
    FDefaultMouseButtonUse := false;
    FDefaultMouseButton := buttonLeft;
    FDefaultMouseButtonCheckModifiers := [];
    FDefaultMouseButtonModifiers := [];
    FDefaultMouseButton2Use := false;
    FDefaultMouseButton2 := buttonLeft;
    FDefaultMouseButton2CheckModifiers := [];
    FDefaultMouseButton2Modifiers := [];
    FDefaultMouseWheel := mwNone;
  end;

  { we don't set here properties, but directly set FXxx fields,
    so that we can call Changed only once. }
  Changed;
end;

{$ifndef CASTLE_STRICT_CLI}
function TInputShortcut.IsPressed(const Container: TCastleContainer): Boolean;
var
  IgnoreHowMuch: Single;
begin
  Result := IsPressed(Container, IgnoreHowMuch);
end;

function TInputShortcut.IsPressed(const Container: TCastleContainer;
  out HowMuch: Single): Boolean;
var
  KeysPressed: TKeysPressed;
  MousePressed: TCastleMouseButtons;
begin
  KeysPressed := Container.Pressed;
  MousePressed := Container.MousePressed;

  { Hardcoded testing for keys and mouse.
    TODO: Move this to TInputShortcutBinding descendants. }
  Result :=
    ( (KeysPressed <> nil) and
      ( KeysPressed.Keys[Key1] or
        KeysPressed.Keys[Key2] or
        KeysPressed.Strings[KeyString]
      )
    ) or
    ( MouseButtonUse and
      (MouseButton in MousePressed) and
      (ModifiersDown(KeysPressed) * MouseButtonCheckModifiers = MouseButtonModifiers)
    ) or
    ( MouseButton2Use and
      (MouseButton2 in MousePressed) and
      (ModifiersDown(KeysPressed) * MouseButton2CheckModifiers = MouseButton2Modifiers)
    );

  { Simple HowMuch for keys and mouse buttons.
    TODO: Introduce some "acceleration" and "deceleration" for them. }
  if Result then
    HowMuch := 1.0
  else
    HowMuch := 0.0;

  // test FBindings
  if not Result then
    Result := FBindings.IsPressed(Container, HowMuch);
end;
{$endif}

function TInputShortcut.IsKey(const Key: TKey; AKeyString: String): boolean;
begin
  // only for backward compatibility (when this parameter was Char) convert #0 to ''
  if AKeyString = #0 then
    AKeyString := '';

  Result :=
    ( (Key <> keyNone) and ( (Key = Key1) or (Key = Key2) ) ) or
    ( (KeyString <> '') and (KeyString = AKeyString) );
end;

function TInputShortcut.IsMouseButton(const AMouseButton: TCastleMouseButton;
  const ModifiersDown: TModifierKeys): boolean;
begin
  Result :=
    ( MouseButtonUse and
      (AMouseButton = MouseButton) and
      (ModifiersDown * MouseButtonCheckModifiers = MouseButtonModifiers)
    ) or
    ( MouseButton2Use and
      (AMouseButton = MouseButton2) and
      (ModifiersDown * MouseButton2CheckModifiers = MouseButton2Modifiers)
    );
end;

function TInputShortcut.IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;
begin
  Result := (AMouseWheel <> mwNone) and (AMouseWheel = MouseWheel);
end;

function TInputShortcut.IsEvent(const Event: TInputPressRelease): boolean;
begin
  Result := false;
  case Event.EventType of
    itKey        : Result := IsKey(Event.Key, Event.KeyString);
    itMouseButton: Result := IsMouseButton(Event.MouseButton, Event.ModifiersDown);
    itMouseWheel : Result := IsMouseWheel(Event.MouseWheel);
    else ;
  end;

  if not Result then
    Result := FBindings.IsEvent(Event);
end;

function TInputShortcut.Description(const NoneString: string): string;
var
  BindingsStr: String;
begin
  Result := '';

  { It's important for this description to be really compact (as it's
    used in situations like menu items (see "The Castle")), that's why
    I mess with checking various cases and trying to make shorter string
    for this. }

  if (Key1 <> keyNone) or (Key2 <> keyNone) then
  begin
    if (Key1 <> keyNone) and (Key2 <> keyNone) then
      Result := Format('key "%s" or "%s"', [KeyToStr(Key1), KeyToStr(Key2)]) else
    if Key1 <> keyNone then
      Result := Result + (Format('key "%s"', [KeyToStr(Key1)])) else
      Result := Result + Format('key "%s"', [KeyToStr(Key2)]);
  end;

  if KeyString <> '' then
  begin
    if Result <> '' then Result := Result + ' or ';
    Result := Result + Format('char "%s"', [KeyStringToNiceStr(KeyString)]);
  end;

  if MouseButtonUse then
  begin
    if Result <> '' then Result := Result + ' or ';
    Result := Result + Format('mouse "%s"', [MouseButtonStr[MouseButton]]);
    if MouseButtonCheckModifiers <> [] then
      Result := Result + '+' + ModifierKeysToNiceStr(MouseButtonModifiers);
  end;

  if MouseButton2Use then
  begin
    if Result <> '' then Result := Result + ' or ';
    Result := Result + Format('mouse "%s"', [MouseButtonStr[MouseButton2]]);
    if MouseButton2CheckModifiers <> [] then
      Result := Result + '+' + ModifierKeysToNiceStr(MouseButton2Modifiers);
  end;

  if MouseWheel <> mwNone then
  begin
    if Result <> '' then Result := Result + ' or ';
    Result := Result + Format('wheel "%s"', [MouseWheelDirectionStr[MouseWheel]]);
  end;

  BindingsStr := FBindings.ToString;
  if BindingsStr <> '' then
    Result := SAppendPart(Result, ' or ', BindingsStr);

  if Result = '' then
    Result := NoneString;
end;

function TInputShortcut.Description: string;
begin
  Result := Description(Format('"%s" key', [Caption]));
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

procedure TInputShortcut.SetKeyString(const Value: String);
begin
  FKeyString := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButtonUse(const Value: boolean);
begin
  FMouseButtonUse := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton(const Value: TCastleMouseButton);
begin
  FMouseButton := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButtonCheckModifiers(const Value: TModifierKeys);
begin
  FMouseButtonCheckModifiers := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButtonModifiers(const Value: TModifierKeys);
begin
  FMouseButtonModifiers := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton2Use(const Value: boolean);
begin
  FMouseButton2Use := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton2(const Value: TCastleMouseButton);
begin
  FMouseButton2 := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton2CheckModifiers(const Value: TModifierKeys);
begin
  FMouseButton2CheckModifiers := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton2Modifiers(const Value: TModifierKeys);
begin
  FMouseButton2Modifiers := Value;
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

procedure TInputShortcut.Add(const NewEvent: TInputPressRelease);
begin
  case NewEvent.EventType of
    itMouseButton:
      begin
        // TODO: add to MouseButton2Xxx, when MouseButtonUse already used
        MouseButtonUse := true;
        MouseButton := NewEvent.MouseButton;
        MouseButtonCheckModifiers := []; // reset
        MouseButtonModifiers := []; // reset
      end;
    itMouseWheel:
      MouseWheel := NewEvent.MouseWheel;
    itKey:
      if Key1 = keyNone then
        Key1 := NewEvent.Key else
      if Key2 = keyNone then
        Key2 := NewEvent.Key else
      begin
        { We move the previous Key1 to Key2, and set Key1 to new key.
          This looks nice for user when this is displayed as the menu argument. }
        Key2 := Key1;
        Key1 := NewEvent.Key;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TInputShortcut.Add: NewEvent.EventType?');
    {$endif}
  end;
end;

procedure TInputShortcut.SaveToConfig(const Config: TCastleConfig; ConfigPath: String);
begin
  // add slash at the end of ConfigPath, if necessary
  if (ConfigPath <> '') and (ConfigPath[Length(ConfigPath)] <> '/') then
    ConfigPath := ConfigPath + '/';

  Config.SetDeleteKey(ConfigPath + Name + '/key1',
    Key1, DefaultKey1);
  Config.SetDeleteKey(ConfigPath + Name + '/key2',
    Key2, DefaultKey2);

  Config.SetDeleteValue(ConfigPath + Name + '/mouse_button_use',
    MouseButtonUse, DefaultMouseButtonUse);
  Config.SetDeleteValue(ConfigPath + Name + '/mouse_button',
    Ord(MouseButton), Ord(DefaultMouseButton));
  // TODO
  // Config.SetDeleteValue(ConfigPath + Name + '/mouse_button_check_modifiers',
  //   ModifiersToString(MouseButtonCheckModifiers), ModifiersToString(DefaultMouseButtonCheckModifiers));
  // Config.SetDeleteValue(ConfigPath + Name + '/mouse_button_modifiers',
  //   ModifiersToString(MouseButtonModifiers), ModifiersToString(DefaultMouseButtonModifiers));

  Config.SetDeleteValue(ConfigPath + Name + '/mouse_button2_use',
    MouseButton2Use, DefaultMouseButton2Use);
  Config.SetDeleteValue(ConfigPath + Name + '/mouse_button2',
    Ord(MouseButton2), Ord(DefaultMouseButton2));
  // TODO
  // Config.SetDeleteValue(ConfigPath + Name + '/mouse_button2_check_modifiers',
  //   ModifiersToString(MouseButton2CheckModifiers), ModifiersToString(DefaultMouseButton2CheckModifiers));
  // Config.SetDeleteValue(ConfigPath + Name + '/mouse_button2_modifiers',
  //   ModifiersToString(MouseButton2Modifiers), ModifiersToString(DefaultMouseButton2Modifiers));

  Config.SetDeleteValue(ConfigPath + Name + '/mouse_wheel',
    Ord(MouseWheel), Ord(DefaultMouseWheel));

  // TODO: FBindings not loaded/saved
end;

procedure TInputShortcut.LoadFromConfig(const Config: TCastleConfig; ConfigPath: String);
begin
  // add slash at the end of ConfigPath, if necessary
  if (ConfigPath <> '') and (ConfigPath[Length(ConfigPath)] <> '/') then
    ConfigPath := ConfigPath + '/';

  Key1 := Config.GetKey(
    ConfigPath + Name + '/key1', DefaultKey1);
  Key2 := Config.GetKey(
    ConfigPath + Name + '/key2', DefaultKey2);

  MouseButtonUse := Config.GetValue(
    ConfigPath + Name + '/mouse_button_use', DefaultMouseButtonUse);
  MouseButton := TCastleMouseButton(Config.GetValue(
    ConfigPath + Name + '/mouse_button', Ord(DefaultMouseButton)));
  // TODO
  // MouseButtonCheckModifiers := StringToModifiers(Config.GetValue(
  //   ConfigPath + Name + '/mouse_button_check_modifiers', ModifiersToString(DefaultMouseButtonCheckModifiers)));
  // MouseButtonModifiers := StringToModifiers(Config.GetValue(
  //   ConfigPath + Name + '/mouse_button_modifiers', ModifiersToString(DefaultMouseButtonModifiers)));

  MouseButton2Use := Config.GetValue(
    ConfigPath + Name + '/mouse_button2_use', DefaultMouseButton2Use);
  MouseButton2 := TCastleMouseButton(Config.GetValue(
    ConfigPath + Name + '/mouse_button2', Ord(DefaultMouseButton2)));
  // TODO
  // MouseButton2CheckModifiers := StringToModifiers(Config.GetValue(
  //   ConfigPath + Name + '/mouse_button2_check_modifiers', ModifiersToString(DefaultMouseButton2CheckModifiers)));
  // MouseButton2Modifiers := StringToModifiers(Config.GetValue(
  //   ConfigPath + Name + '/mouse_button2_modifiers', ModifiersToString(DefaultMouseButton2Modifiers)));

  MouseWheel := TMouseWheelDirection(Config.GetValue(
    ConfigPath + Name + '/mouse_wheel', Ord(DefaultMouseWheel)));

  // TODO: FBindings not loaded/saved
end;

{ TInputShortcutList ----------------------------------------------------- }

function TInputShortcutList.SeekMatchingShortcut(
  const Event: TInputPressRelease): TInputShortcut;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.IsEvent(Event) then
      Exit;
  end;
  Result := nil;
end;

procedure TInputShortcutList.RestoreDefaults;
var
  I: TInputShortcut;
begin
  for I in Self do
    I.MakeDefault;
end;

procedure TInputShortcutList.SaveToConfig(const Config: TCastleConfig; ConfigPath: String = '');
var
  I: TInputShortcut;
begin
  // add slash at the end of ConfigPath, if necessary
  if (ConfigPath <> '') and (ConfigPath[Length(ConfigPath)] <> '/') then
    ConfigPath := ConfigPath + '/';

  ConfigPath := ConfigPath + 'inputs/';

  for I in Self do
    I.SaveToConfig(Config, ConfigPath);
end;

function SortInputShortcut({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TInputShortcut): Integer;
begin
  Result := A.GroupOrder - B.GroupOrder;
  { since TFPSList.Sort is not stable, we use Index to keep order predictable
    when GroupOrder is equal }
  if Result = 0 then
    Result := A.Index - B.Index;
end;

procedure TInputShortcutList.LoadFromConfig(const Config: TCastleConfig; ConfigPath: String);
type
  TInputShortcutComparer = {$ifdef FPC}specialize{$endif} TComparer<TInputShortcut>;
var
  I: TInputShortcut;
  ConflictDescription: string;
  G: TInputGroupNotLocal;
begin
  { we assume that all inputs are added now, so we do some finalizing operations
    now, like checking defaults for conflicts and sorting by GroupOrder. }
  if SeekConflict(ConflictDescription) then
    raise EInternalError.Create(
      'Default key/mouse shortcuts layout has conflicts: ' + ConflictDescription);

  for G := Low(TInputGroupNotLocal) to High(TInputGroupNotLocal) do
    InputsGroup(G).Sort(TInputShortcutComparer.Construct({$ifdef FPC}@{$endif}SortInputShortcut));

  // add slash at the end of ConfigPath, if necessary
  if (ConfigPath <> '') and (ConfigPath[Length(ConfigPath)] <> '/') then
    ConfigPath := ConfigPath + '/';

  ConfigPath := ConfigPath + 'inputs/';

  for I in Self do
    I.LoadFromConfig(Config, ConfigPath);

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

function TInputShortcutList.SeekConflict(
  out ConflictDescription: string): boolean;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := I + 1 to Count - 1 do
    begin
      if Items[J].IsKey(Items[I].Key1, '') or
         Items[J].IsKey(Items[I].Key2, '') or
         (Items[I].MouseButtonUse  and Items[J].IsMouseButton(Items[I].MouseButton , Items[I].MouseButtonModifiers )) or
         (Items[I].MouseButton2Use and Items[J].IsMouseButton(Items[I].MouseButton2, Items[I].MouseButton2Modifiers)) then
      begin
        ConflictDescription := Format('"%s" conflicts with "%s"',
          [Items[I].Caption, Items[J].Caption]);
        Exit(true);
      end;
    end;
  Result := false;
end;

function TInputShortcutList.FindName(const Name: string): TInputShortcut;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  Result := nil;
end;

{ initialization / finalization ---------------------------------------------- }

var
  UnitInitialized, UnitFinalization: Boolean;
  FInputsAll: TInputShortcutList;
  FInputsGroup: array [TInputGroupNotLocal] of TInputShortcutList;

procedure DoInitialization;
var
  G: TInputGroupNotLocal;
begin
  if (not UnitInitialized) and (not UnitFinalization) then
  begin
    FInputsAll := TInputShortcutList.Create(true);
    for G := Low(TInputGroupNotLocal) to High(TInputGroupNotLocal) do
      FInputsGroup[G] := TInputShortcutList.Create(false);
    UnitInitialized := true;
  end;
end;

function InputsAll: TInputShortcutList;
begin
  DoInitialization;
  Result := FInputsAll;
end;

function InputsGroup(const Group: TInputGroupNotLocal): TInputShortcutList;
begin
  DoInitialization;
  Result := FInputsGroup[Group];
end;

procedure DoFinalization;
var
  G: TInputGroupNotLocal;
begin
  UnitFinalization := true;
  for G := Low(TInputGroupNotLocal) to High(TInputGroupNotLocal) do
    FreeAndNil(FInputsGroup[G]);
  FreeAndNil(FInputsAll);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.
