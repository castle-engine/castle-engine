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

{ Inputs (key/mouse shortcuts) of a typical 3D FPS game.

  The @italic(low-level management of inputs) is based on TInputGroup instances
  throughout various engine components (like TWalkCamera.Input_Forward).

  This unit contains a @italic(high-level, centralized management of inputs).
  It allows to detect key conflicts, restore whole inputs to default,
  load/save them to the user config file, show descriptions of inputs, and such.
  Units in src/game/ (notably, CastlePlayer) may use the inputs defined here to set
  low-level inputs. For example, CastleInput_Forward may be used to set camera's
  TWalkCamera.Input_Forward, as long as the player is not dead.

  You can add new inputs simply by creating new TInputConfiguration instances.
  Just make sure you do it before calling @code(Config.Load),
  as some functionality assumes that all shortcuts are already added
  at the time @code(Config.Load) is called.

  It also defines new CastleScript function: @code(shortcut),
  see [http://castle-engine.sourceforge.net/castle_script.php#function_shortcut]. }
unit CastleInputs;

interface

uses KeysMouse, Cameras, CastleUtils, CastleClassUtils, Classes,
  FGL, GenericStructList, CastleConfig, CastleScript;

type
  TInputGroup = (kgBasic, kgItems, kgOther);

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
    FName: string;
    FGroup: TInputGroup;
    FShortcut: TInputShortcut;
    FConfigName: string;
    FGroupOrder: Integer;
    { Index of CastleAllInputs. For now this is useful only for sorting,
      to decide order when GroupOrder is equal between two items. }
    Index: Integer;
    procedure ShortcutChanged(Shortcut: TInputShortcut);
  public
    { Constructor. Note that TInputShortcut instance passed here is owned
      by this object, i.e. it will be freed in our destructor. }
    constructor Create(const AName: string;
      const AConfigName: string;
      const AGroup: TInputGroup;
      const AKey1: TKey;
      const AKey2: TKey = K_None;
      const ACharacter: Char = #0;
      const AMouseButtonUse: boolean = false;
      const AMouseButton: TMouseButton = mbLeft;
      const AMouseWheel: TMouseWheelDirection = mwNone);
    destructor Destroy; override;

    property Name: string read FName;
    property ConfigName: string read FConfigName;
    property Group: TInputGroup read FGroup;

    { Order of the shortcut within it's @link(Group).
      The order may be important, as menus may show CastleGroupInputs
      to user in this order.
      This order is applied (the group is actually sorted by GroupOrder)
      when reading config file.
      For equal GroupOrder, the order of creation (or so, the order
      on CastleAllInputs list) decides. }
    property GroupOrder: Integer read FGroupOrder write FGroupOrder;

    { The key/mouse shortcut for this action.
      You can directly change fields of this action,
      but don't mess with it's OnChanged property --- we will use
      it in this class internally. }
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
    function FindConfigName(const ConfigName: string): TInputConfiguration;
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

  { List of all configurable shortcuts.
    Will be created in initialization and freed in finalization of this unit.
    All TInputConfiguration instances will automatically add to this. }
  CastleAllInputs: TInputConfigurationList;
  CastleGroupInputs: array [TInputGroup] of TInputConfigurationList;

  OnInputChanged: TInputChangedEventList;

const
  DefaultUseMouseLook = true;
  DefaultInvertVerticalMouseLook = false;

var
  { Game player camera settings.
    Automatically saved/loaded from user preferences using CastleConfig.
    @groupBegin }
  UseMouseLook: boolean = DefaultUseMouseLook;
  InvertVerticalMouseLook: boolean = DefaultInvertVerticalMouseLook;
  MouseLookHorizontalSensitivity: Single;
  MouseLookVerticalSensitivity: Single;
  { @groupEnd }

implementation

uses SysUtils;

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
    Config.SetDeleteValue('inputs/' + Items[I].ConfigName + '/key1',
      Items[I].Shortcut.Key1, Items[I].Shortcut.DefaultKey1);
    Config.SetDeleteValue('inputs/' + Items[I].ConfigName + '/key2',
      Items[I].Shortcut.Key2, Items[I].Shortcut.DefaultKey2);
    Config.SetDeleteValue('inputs/' + Items[I].ConfigName + '/mouse_button_use',
      Items[I].Shortcut.MouseButtonUse, Items[I].Shortcut.DefaultMouseButtonUse);
    Config.SetDeleteValue('inputs/' + Items[I].ConfigName + '/mouse_button',
      Ord(Items[I].Shortcut.MouseButton), Ord(Items[I].Shortcut.DefaultMouseButton));
    Config.SetDeleteValue('inputs/' + Items[I].ConfigName + '/mouse_wheel',
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
    CastleGroupInputs[G].Sort(@SortInputConfiguration);

  for I := 0 to Count - 1 do
  begin
    Items[I].Shortcut.Key1 := Config.GetValue(
      'inputs/' + Items[I].ConfigName + '/key1', Items[I].Shortcut.DefaultKey1);
    Items[I].Shortcut.Key2 := Config.GetValue(
      'inputs/' + Items[I].ConfigName + '/key2', Items[I].Shortcut.DefaultKey2);
    Items[I].Shortcut.MouseButtonUse := Config.GetValue(
      'inputs/' + Items[I].ConfigName + '/mouse_button_use',
      Items[I].Shortcut.DefaultMouseButtonUse);
    Items[I].Shortcut.MouseButton := TMouseButton(Config.GetValue(
      'inputs/' + Items[I].ConfigName + '/mouse_button',
      Ord(Items[I].Shortcut.DefaultMouseButton)));
    Items[I].Shortcut.MouseWheel := TMouseWheelDirection(Config.GetValue(
      'inputs/' + Items[I].ConfigName + '/mouse_wheel',
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
          [Items[I].Name, Items[J].Name]);
        Exit(true);
      end;
    end;
  Result := false;
end;

function TInputConfigurationList.FindConfigName(const ConfigName: string): TInputConfiguration;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ConfigName = ConfigName then
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

constructor TInputConfiguration.Create(const AName: string;
  const AConfigName: string;
  const AGroup: TInputGroup;
  const AKey1: TKey;
  const AKey2: TKey;
  const ACharacter: Char;
  const AMouseButtonUse: boolean;
  const AMouseButton: TMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  inherited Create;
  FName := AName;
  FConfigName := AConfigName;
  FGroup := AGroup;

  FShortcut := TInputShortcut.Create(nil);
  FShortcut.Assign(AKey1, AKey2, ACharacter, AMouseButtonUse, AMouseButton, AMouseWheel);
  FShortcut.OnChanged := @ShortcutChanged;

  Index := CastleAllInputs.Count;
  CastleAllInputs.Add(Self);

  CastleGroupInputs[Group].Add(Self);
end;

destructor TInputConfiguration.Destroy;
begin
  FreeAndNil(FShortcut);
  inherited;
end;

procedure TInputConfiguration.ShortcutChanged(Shortcut: TInputShortcut);
begin
  Assert(Shortcut = Self.Shortcut);
  OnInputChanged.ExecuteAll(Self);
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
  Result := Shortcut.Description(Format('"%s" key', [Name]));
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
  if CastleAllInputs <> nil then
  begin
    I := CastleAllInputs.FindConfigName(N);
    if I <> nil then
      TCasScriptString(AResult).Value := I.Description else
      TCasScriptString(AResult).Value := Format('(shortcut name "%s" undefined)', [N]);
  end else
    TCasScriptString(AResult).Value := 'input names not available (finalization of CastleInputs unit is already done)';
end;

{ initialization / finalization ---------------------------------------------- }

procedure DoInitialization;
var
  InputGroup: TInputGroup;
begin
  OnInputChanged := TInputChangedEventList.Create;
  CastleAllInputs := TInputConfigurationList.Create(true);

  for InputGroup := Low(InputGroup) to High(InputGroup) do
    CastleGroupInputs[InputGroup] := TInputConfigurationList.Create(false);

  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  { Basic shortcuts. }
  CastleInput_Forward := TInputConfiguration.Create('Move forward', 'move_forward', kgBasic,
    K_W, K_Up, #0, false, mbLeft);
  CastleInput_Backward := TInputConfiguration.Create('Move backward', 'move_backward', kgBasic,
    K_S, K_Down, #0, false, mbLeft);
  CastleInput_LeftStrafe := TInputConfiguration.Create('Move left', 'move_left', kgBasic,
    K_A, K_None, #0, false, mbLeft);
  CastleInput_RightStrafe := TInputConfiguration.Create('Move right', 'move_right', kgBasic,
    K_D, K_None, #0, false, mbLeft);
  CastleInput_LeftRot := TInputConfiguration.Create('Turn left', 'turn_left', kgBasic,
    K_Left, K_None, #0, false, mbLeft);
  CastleInput_RightRot := TInputConfiguration.Create('Turn right', 'turn_right', kgBasic,
    K_Right, K_None, #0, false, mbLeft);
  CastleInput_UpRotate := TInputConfiguration.Create('Look up', 'look_up', kgBasic,
    K_PageDown, K_None, #0, false, mbLeft);
  CastleInput_DownRotate := TInputConfiguration.Create('Look down', 'look_down', kgBasic,
    K_Delete, K_None, #0, false, mbLeft);
  CastleInput_GravityUp := TInputConfiguration.Create('Look straight', 'look_straight', kgBasic,
    K_End, K_None, #0, false, mbLeft);
  CastleInput_UpMove := TInputConfiguration.Create('Jump (or fly/swim up)', 'move_up', kgBasic,
    K_Space, K_None, #0, true, mbRight);
  CastleInput_DownMove := TInputConfiguration.Create('Crouch (or fly/swim down)', 'move_down', kgBasic,
    K_C, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  CastleInput_Interact := TInputConfiguration.Create('Interact (press button / open door etc.)', 'interact', kgOther,
    K_E, K_None, #0, false, mbLeft);

  Config.OnLoad.Add(@CastleAllInputs.LoadFromConfig);
  Config.OnSave.Add(@CastleAllInputs.SaveToConfig);

  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);

  FunctionHandlers.RegisterHandler(@TCasScriptShortcut(nil).Handle, TCasScriptShortcut, [TCasScriptString], false);
end;

procedure DoFinalization;
var
  InputGroup: TInputGroup;
begin
  if (CastleAllInputs <> nil) and (Config <> nil) then
  begin
    Config.OnLoad.Remove(@CastleAllInputs.LoadFromConfig);
    Config.OnSave.Remove(@CastleAllInputs.SaveToConfig);
  end;

  for InputGroup := Low(InputGroup) to High(InputGroup) do
    FreeAndNil(CastleGroupInputs[InputGroup]);

  FreeAndNil(CastleAllInputs);
  FreeAndNil(OnInputChanged);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.