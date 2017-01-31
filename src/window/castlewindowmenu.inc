{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleWindow menu bar. }

{$ifdef read_interface}
  TMenu = class;

  { A basic class representing basic menu building block.
    This can be a separator, clickable menu item or something that
    expands to submenu. }
  TMenuEntry = class
  private
    { if CorrectParentWindow <> nil call CorrectParentWindow.Menu*. }
    procedure MenuInitialize;
    procedure MenuFinalize;
  private
    { Stuff internal for proper communication between menu and
      TCastleWindowCustom implementation. }

    { ParentWindow to call MenuFinalize, MenuInitialize.

      Used only on root menu item
      (the one with ParentMenu = @nil). From TMenuEntry implementations,
      you should use CorrectParentWindow, that looks for it recursively.
      This way we only have to update it on SetMainMenu (no need to keep
      it recursively updated in all TMenuEntry instances). }
    ParentWindow: TCastleWindowCustom;

    { Internal handle of this menu item within CastleWindow backend.
      This is used by CastleWindow backend implementation, to store the
      internal resource corresponding to this menu entry.
      The exact interpretation depends on CastleWindow backend,
      and should not be messed with. }
    Handle: Pointer;

    FParentMenu: TMenu;
    FParentMenuPosition: Cardinal;

    function CorrectParentWindow: TCastleWindowCustom;
  protected
    { Recursively clear TMenuEntry.Handle values. }
    procedure ClearHandles; virtual;
  public
    { For optimization purposes, you may surround many menu changes
      inside MenuUpdateBegin + MenuUpdateEnd calls.
      Make sure window is not closed / opened between them.
      @groupBegin }
    procedure MenuUpdateBegin;
    procedure MenuUpdateEnd;
    { @groupEnd }

    { This is the parent TMenu that has this item in it's Entries list.
      @nil is there is not parent menu yet (e.g. because it's not assigned
      yet, or because this is the main menu). }
    property ParentMenu: TMenu read FParentMenu;

    { Position on ParentMenu. Indexed from 0, so
      ParentMenu.Entries[ParentMenuPosition] should be always equal to Self
      (as long as ParentMenu <> @nil. }
    property ParentMenuPosition: Cardinal read FParentMenuPosition;
  end;

  TMenuEntryList = specialize TFPGObjectList<TMenuEntry>;

  TMenuEntryWithCaption = class(TMenuEntry)
  private
    FCaption: String;
    procedure SetCaption(const Value: String);
  private
    FEnabled: boolean;
    procedure SetEnabled(Value: boolean);
  public
    constructor Create(const ACaption: String);

    { Caption of this menu entry. Will be shown to user.

      To indicate that some letter of a caption is a mnemonic
      for this menu item put '_' (underscore) char before this
      letter. Depending on CastleWindow-backend-dependent,
      mnemonics should allow user to easier activate this menu item
      with keyboard (e.g. Alt+mnemonic to dropdown item in MainMenu).

      You can put two consecutive underscore chars in Caption to
      indicate that here Caption should display literally one underscore
      character.

      (Convention to mark mnemonics with '_' is just copied from gtk,
      from gtk_label_new_with_mnemonic. So no, I didn't invent
      another new convention here.)

      It is undefined what will happen if you put underscore chars
      before *two* or more letters in Caption.
      I.e. you can't define multiple mnemonics for one menu item.
      I.e. this will not crash your program,
      but results may be CastleWindow-backend-dependent.

      Just like in GTK toolkit gtk_label_new_with_mnemonic and
      gtk_menu_item_new_with_mnemonic. }
    property Caption: String read FCaption write SetCaption;

    property Enabled: boolean read FEnabled write SetEnabled
      default true;
  end;

  TMenuItemRadioGroup = class;

  { TMenuEntry that contains a list of menu entries.
    This is the basic class to represent a drop-down menu, a submenu etc. }
  TMenu = class(TMenuEntryWithCaption)
  private
    FEntries: TMenuEntryList;
    function GetEntries(Index: Integer): TMenuEntry;
  protected
    procedure ClearHandles; override;
  public
    constructor Create(const ACaption: String);
    destructor Destroy; override;

    { Items (entries) on the menu. Items are owned by this menu instance
      (are automatically freed at destruction, at @link(Delete) and such). }
    property Entries [Index: Integer]: TMenuEntry read GetEntries; default;
    function Count: Integer;
    procedure Insert(Index: Integer; Value: TMenuEntry);
    { Add at the end. Same as Insert(@link(Count), Value). }
    procedure Append(Value: TMenuEntry);
    procedure Delete(Index: Integer);
    procedure Clear;

    { @deprecated Deprecated name for @link(Count). }
    function EntriesCount: Integer; deprecated;
    { @deprecated Deprecated name for @link(Delete). }
    procedure EntryDelete(Index: Integer); deprecated;
    { @deprecated Deprecated name for @link(Clear). }
    procedure EntriesDeleteAll; deprecated;
    { @deprecated Deprecated name for @link(Clear). }
    procedure DeleteAll;

    { Append a number of radio menu items.
      This is a comfortable shortcut for an often task of adding
      many TMenuItemRadio items that create a new radio group
      (TMenuItemRadioGroup).

      For each item of Items list, we'll add a new TMenuItemRadio instance.

      @unorderedList(
        @item(TMenuItemRadio.Caption will be set to the Items[I]
          string. Additionally it will be quoted by SQuoteMenuEntryCaption
          to avoid interpreting underscore characters (if QuoteCaption).)

        @item(TMenuItemRadio.IntData will be set to BaseIntData + the number
          of this item. (This is usually most comfortable,
          you can handle this radio group by "case" with a range.))

        @item(TMenuItemRadio.Group will be equal. More precisely:
          along with creating the first TMenuItemRadio, we will also
          create new TMenuItemRadioGroup. For the rest of TMenuItemRadio,
          we'll assign this group.)

        @item(TMenuItemRadio.Checked will be set to @true on only
          one item: the one numbered SelectedIndex.
          Pass SelectedIndex negative (or >= than items count)
          to have no radio item checked by default.)

        @item(TMenuItemRadio.AutoCheckedToggle will be set according
          to ou parameter AAutoCheckedToggle.)
      )

      We return the newly created TMenuItemRadioGroup.
      If Items is empty, this does nothing and returns @nil.
    }
    function AppendRadioGroup(const Items: array of string;
      BaseIntData: Cardinal; SelectedIndex: Integer;
      AAutoCheckedToggle: boolean; QuoteCaption: boolean = true): TMenuItemRadioGroup;
  end;

  { TMenuEntry that is a simple, clickable menu item.
    User expects that something will happend when he clicks on TMenuItem.
    You can also specify key shortcuts for this menu item. }
  TMenuItem = class(TMenuEntryWithCaption)
  private
    FIntData: Integer;
    FSmallId: Integer;

    FCharKey: char;
    FKey: TKey;
    FModifiers: TModifierKeys;
  public
    { An integer value, for any purpose you want. Useful to identify this item in
      TCastleWindowCustom.OnMenuClick callback. }
    property IntData: Integer read FIntData write FIntData;

    { Unique small identifier of this menu item.
      This is initialized at creation of this object, to be unique among
      all currently existing (created and not destroyed) TMenuItem instances.

      The intention is that this SmallId should be the smallest possible value >= 0.
      So it's not something like PtrUInt(Self).
      This may be useful when you're constructing a menu
      using some API --- you usually want to identify
      your menu item by some unique id and you want this id to be small
      (e.g. under WinAPI menu item's id must fit in 16 bit integer).

      @seealso MenuItemFromSmallId }
    property SmallId: Integer read FSmallId;

    { Called when this menu item will be choosen by user (by clicking,
      or pressing the matching key shortcut).

      You can override this and return true if you handled the event.
      If this will return false, CastleWindow will call EventMenuClick
      (OnMenuClick).

      When entering this method, current OpenGL context is set to
      the context of the window that the clicked menu belongs to.

      Default implementation of this method in this class always returns false. }
    function DoClick: boolean; virtual;

    { Key shortcut for this menu item.

      When user presses indicated Key (if it's not K_None)
      or CharKey (if it's not #0) then DoClick will be called
      (which results in TCastleWindowCustom.EventMenuClick,
      and TCastleWindowCustom.OnMenuClick,
      if DoClick returns false).

      Handling of menu key shortcuts is completely within the CastleWindow
      (usually, it's even done by the underlying backend like GTK or WinAPI).
      You will not get EventPress (OnPress) messages for keypresses that translate
      to menu clicks. Although TCastleWindowCustom.Pressed.Keys will be
      still appropriately updated.

      Note that Caption of this object should not contain
      description of Key or CharKey. The backend will automatically show the
      key description in an appropriate way.

      You should use at most one of these properties, not both.
      So either use Key or CharKey, and leave the other one at default
      value (Key = K_None, CharKey = #0).

      The CharKey values within range CtrlA..CtrlZ mean
      that user must press the given letter with Ctrl modfier. This rule
      includes some special chars like
      @unorderedList(
        @item CharKey = #8 (equal to CharBackSpace, also equal to CtrlH) or
        @item CharKey = #9 (equal to CharTab, also equal to CtrlI) or
        @item CharKey = #13 (equal to CharEnter, also equal to CtrlM).
      )
      The CharKey values above do @bold(not) mean the backspace or tab or enter
      keys, they mean the combination of Ctrl key + respective letter.
      If you want to have backspace/tab/enter keys to activate the menu item,
      use Key = K_BackSpace, Key = K_Tab or Key =  K_Enter.

      TODO: for now, this must be assigned before adding this menu item
      to parent (actually, before the parent menu is added to window MainMenu)
      to work reliably. Don't change this property afterwards.
      Usually, you will set this property by an argument to the constructor.

      @groupBegin }
    property CharKey: char read FCharKey write FCharKey default #0;
    property Key: TKey read FKey write FKey default K_None;
    { @groupEnd }

    { Modifiers (shift, ctrl and such) required to activate this menu item.
      These have to be pressed along with @link(Key) or @link(CharKey).

      Note that CharKey may already indicate some modifiers,
      if it's one of the CtrlA ... CtrlZ then the Ctrl key is required
      and if it's an uppercase letter then the Shift key is required.
      There's no need to include this modifier in this property then.

      TODO: for now, this must be assigned before adding this menu item
      to parent (actually, before the parent menu is added to window MainMenu)
      to work reliably. Don't change this property afterwards. }
    property Modifiers: TModifierKeys read FModifiers write FModifiers;

    { Returns as S the string that describes key shortcut
      (@link(CharKey), @link(Key), @link(Modifiers)) and then returns true.
      If both CharKey = #0 and Key = K_None then returns false and
      S is undefined. }
    function KeyString(out S: string): boolean;

    function KeyMatches(const AKey: TKey; const ACharKey: char;
      const AModifiers: TModifierKeys): boolean;

    { Caption with optional key description (returned by KeyString)
      appended. }
    function CaptionWithKey: string;

    constructor Create(const ACaption: String; AIntData: Integer);
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char);
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey);

    destructor Destroy; override;
  end;

  { TMenuEntry that acts as a visual separator (horizontal line
    or something like that) between menu items. This is not clickable
    by the user. Separators may be sometimes ignored in toplevel menus
    (e.g. WinAPI does not allow separator in toplevel menu bar). }
  TMenuSeparator = class(TMenuEntry)
  end;

  { TMenuItem that should visualize Checked state somehow to the user. }
  TMenuItemChecked = class(TMenuItem)
  private
    FChecked: boolean;
    FAutoCheckedToggle: boolean;
  protected
    { Called when Checked property is assigned. }
    procedure SetChecked(Value: boolean); virtual;

    { Called from DoClick when AutoCheckedToggle is @true.
      It's supposed to actually implement the AutoCheckedToggle behavior.

      Note that this is overriden ina a "dirty" way
      (i.e. not calling "inherited") in TMenuItemRadio descendant. }
    procedure DoAutoCheckedToggle; virtual;
  public
    property Checked: boolean read FChecked write SetChecked;

    { If @true then each time user chooses this menu item,
      @link(Checked) is changed to @code(not Checked). }
    property AutoCheckedToggle: boolean read FAutoCheckedToggle
      write FAutoCheckedToggle;

    { Overriden to handle AutoCheckedToggle. In this class,
      DoClick still returns false (like in ancestor class). }
    function DoClick: boolean; override;

    constructor Create(const ACaption: String; AIntData: Integer;
      AChecked, AAutoCheckedToggle: boolean);
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char;
      AChecked, AAutoCheckedToggle: boolean);
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey;
      AChecked, AAutoCheckedToggle: boolean);
  end;

  { Menu radio item. Similar to TMenuItemChecked, but it belongs to a group and
    within this group only one (or none) radio button can be checked.

    Note that AutoCheckedToggle property has a little different
    meaning in this class: whenever user will click on some item,
    it will be automatically set to Checked = @true and the rest
    of items within this group will be set to Checked = @false.

    You can of course operate on Checked property explicitly,
    setting it to @true or @false. }
  TMenuItemRadio = class(TMenuItemChecked)
  private
    FGroup: TMenuItemRadioGroup;
    procedure SetGroup(Value: TMenuItemRadioGroup);
    procedure CreateCommon;
  protected
    procedure SetChecked(Value: boolean); override;
    procedure DoAutoCheckedToggle; override;
  public
    constructor Create(const ACaption: String; AIntData: Integer;
      AChecked, AAutoCheckedToggle: boolean);
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char;
      AChecked, AAutoCheckedToggle: boolean);
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey;
      AChecked, AAutoCheckedToggle: boolean);

    destructor Destroy; override;

    { The list of radio items within this group. This is never @nil.

      Assigning this property is equivalent to adding yourself to the
      wanted group. I.e. @code(Item.Group := NewGroup;)
      is equivalent to NewGroup.Add(Item). }
    property Group: TMenuItemRadioGroup read FGroup write SetGroup;
  end;

  { A group of radio buttons.

    An instance of this object is always
    created and destroyed automatically by TMenuItemRadio objects.
    All TMenuItemRadio within the same group share the same
    TMenuItemRadioGroup instance.

    Never modify anything within this group using general TFPGObjectList
    properties. Instead always use methods defined here. }
  TMenuItemRadioGroup = class(specialize TFPGObjectList<TMenuItemRadio>)
  private
    FSelected: TMenuItemRadio;
    procedure SetSelected(Value: TMenuItemRadio);
    { Index of @link(Selected) menu item. -1 if Selected is @nil. }
    function SelectedIndex: Integer;
  public
    { Adds an existing radio menu item to this group.
      After this, Item.Group will point to this group object.

      Note that Item.Checked may be changed to @false if there
      already is a selected item in this group. If the added
      item already has Item.Checked = @false then for sure
      it will not be changed (SelectedIndex can be left as -1). }
    procedure Add(Item: TMenuItemRadio);

    procedure Remove(Item: TMenuItemRadio);

    { This is the only currently Checked item.

      It can be nil if no item is checked currently
      (this can happen if you added all initial items with
      Checked = @false, or if you later removed the checked item from the
      group; we need a state to represent this situation, that's why
      it's allowed for radio group to have none radio item chosen). }
    property Selected: TMenuItemRadio read FSelected write SetSelected;

    { Return previous (from @link(Selected)) menu item in this group.
      If there is no previous item, but still @link(Selected) <> nil,
      return @link(Selected).
      Only when there's no @link(Selected) item, returns @false. }
    function Previous(out Item: TMenuItemRadio): boolean;

    { Return next (from @link(Selected)) menu item in this group.
      If there is no next item, but still @link(Selected) <> nil,
      return @link(Selected).
      Only when there's no @link(Selected) item, returns @false. }
    function Next(out Item: TMenuItemRadio): boolean;

    (* procedure DebugWriteln; *)
  end;

  { Menu item that toggles TCastleWindowCustom.FullScreen.

    This already has a nice caption, key shortcut (F11 on most platforms,
    except Mac OS X where it's Command+F11 (on LCL backend) or Ctrl+F11
    (on other backends), since the bare F11 is reserved on Mac OS X).
    And it has a DoClick implementation that takes care of the actual job.

    So you can just add it to your menu, and forget about it. }
  TMenuItemToggleFullScreen = class(TMenuItemChecked)
    constructor Create(const InitialFullScreen: boolean);
    function DoClick: boolean; override;
  end;

{$endif read_interface}

{$ifdef read_interface_2}

{ Search for menu item with given SmallId.
  SearchSmallId must be a SmallId of some existing (i.e. created and not
  destroyed yet) TMenuItem. This function returns this TMenuItem. }
function MenuItemFromSmallId(SearchSmallId: Integer): TMenuItem;

{ Returns S with each '__' replaced with single '_',
  any other '_' removed.

  In other words: with mnemonics (as defined by
  @link(TMenuEntryWithCaption.Caption) removed. }
function SRemoveMnemonics(const S: string): string;

{ Returns S with each underscore '_' replaced by two underscores, '__'.

  In other words: S will not contain any mnemonics.
  If you will assign S to @link(TMenuEntryWithCaption.Caption),
  then this menu entry caption will display exactly S,
  without any mnemonics. Single '_' in S will be displayed exactly
  as single '_'. }
function SQuoteMenuEntryCaption(const S: string): string;

{$endif read_interface_2}

{$ifdef read_implementation}

{ Implementation of these classes is NOT dependent on CastleWindow
  implementation (i.e. it's not GTK or WinAPI specific).
  That was the original idea of this unit. Once these classes
  were even declared in a separate unit (that was not dependent on CastleWindow
  unit).

  Such separation makes implementation of these things simple --
  it's not cluttered with GTK/WinAPI specific details.
}

{ TMenuEntry ------------------------------------------------------------ }

function TMenuEntry.CorrectParentWindow: TCastleWindowCustom;
begin
  if ParentMenu = nil then
    Result := ParentWindow else
    Result := ParentMenu.CorrectParentWindow;
end;

procedure TMenuEntry.MenuInitialize;
var
  P: TCastleWindowCustom;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuInitialize;
end;

procedure TMenuEntry.MenuFinalize;
var
  P: TCastleWindowCustom;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuFinalize;
end;

procedure TMenuEntry.MenuUpdateBegin;
var
  P: TCastleWindowCustom;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuUpdateBegin;
end;

procedure TMenuEntry.MenuUpdateEnd;
var
  P: TCastleWindowCustom;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuUpdateEnd;
end;

procedure TMenuEntry.ClearHandles;
begin
  Handle := nil;
end;

{ TMenuEntryWithCaption ---------------------------------------- }

constructor TMenuEntryWithCaption.Create(const ACaption: String);
begin
  inherited Create;
  FCaption := ACaption;
  FEnabled := true;
end;

procedure TMenuEntryWithCaption.SetCaption(const Value: String);
var
  P: TCastleWindowCustom;
begin
  FCaption := Value;

  P := CorrectParentWindow;
  if (P <> nil) and (not P.Closed) then P.MenuUpdateCaption(Self);
end;

procedure TMenuEntryWithCaption.SetEnabled(Value: boolean);
var
  P: TCastleWindowCustom;
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;

    P := CorrectParentWindow;
    if (P <> nil) and (not P.Closed) then P.MenuUpdateEnabled(Self);
  end;
end;

{ TMenu ------------------------------------------------------------ }

constructor TMenu.Create(const ACaption: String);
begin
  inherited Create(ACaption);
  FEntries := TMenuEntryList.Create(true);
end;

destructor TMenu.Destroy;
begin
  FreeAndNil(FEntries);
  inherited;
end;

function TMenu.GetEntries(Index: Integer): TMenuEntry;
begin
  Result := FEntries[Index];
end;

function TMenu.EntriesCount: Integer;
begin
  Result := Count;
end;

function TMenu.Count: Integer;
begin
  Result := FEntries.Count;
end;

procedure TMenu.Insert(Index: Integer; Value: TMenuEntry);
var
  I: Integer;
  P: TCastleWindowCustom;
begin
  FEntries.Insert(Index, Value);

  { update Value.FParentMenu }
  Value.FParentMenu := Self;

  { update changed FParentMenuPosition values (this includes
    Value.FParentMenuPosition) }
  for I := Index to Count - 1 do
    FEntries[I].FParentMenuPosition := I;

  P := CorrectParentWindow;
  if (P <> nil) and (not P.Closed) then P.MenuInsert(Self, Index, Value);
end;

procedure TMenu.Append(Value: TMenuEntry);
begin
  Insert(FEntries.Count, Value);
end;

procedure TMenu.EntryDelete(Index: Integer);
begin
  Delete(Index);
end;

procedure TMenu.Delete(Index: Integer);
var
  I: Integer;
  Entry: TMenuEntry;
  P: TCastleWindowCustom;
begin
  { extract Entry from Entries list }
  Entry := FEntries[Index];
  FEntries.FreeObjects := false; // temporarily disable FreeObjects
  FEntries.Delete(Index);
  FEntries.FreeObjects := true;

  { update changed FParentMenuPosition values }
  for I := Index to Count - 1 do
    FEntries[I].FParentMenuPosition := I;

  P := CorrectParentWindow;
  if (P <> nil) and (not P.Closed) then P.MenuDelete(Self, Index, Entry);

  FreeAndNil(Entry);
end;

procedure TMenu.EntriesDeleteAll;
begin
  Clear;
end;

procedure TMenu.DeleteAll;
begin
  Clear;
end;

procedure TMenu.Clear;
var
  I: Integer;
begin
  MenuUpdateBegin;
  for I := Count - 1 downto 0 do
    Delete(I);
  MenuUpdateEnd;
end;

function TMenu.AppendRadioGroup(const Items: array of string;
  BaseIntData: Cardinal; SelectedIndex: Integer;
  AAutoCheckedToggle: boolean; QuoteCaption: boolean): TMenuItemRadioGroup;
var
  Radio: TMenuItemRadio;
  I: Integer;
  S: string;
begin
  Result := nil;
  for I := 0 to High(Items) do
  begin
    S := Items[I];
    if QuoteCaption then
      S := SQuoteMenuEntryCaption(S);
    Radio := TMenuItemRadio.Create(S,
      Cardinal(I) + BaseIntData, I = SelectedIndex, AAutoCheckedToggle);
    if Result = nil then
      Result := Radio.Group else
      Radio.Group := Result;
    Append(Radio);
  end;
end;

procedure TMenu.ClearHandles;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Entries[I].ClearHandles;
end;

{ TMenuItem ------------------------------------------------------------ }

var
  { This list contains only TMenuItem objects (and some nils).
    Position on this list is object's SmallId.
    The current implementation of SmallId in this unit is a resource-eater
    (MenuItems only grows !), it can be optimized in many ways.
    But I don't do it because I don't need
    it for now -- all my programs use a limited set of menu items, and usually
    free those menu items right before exiting. So there's no need to optimize
    this for them. }
  MenuItems: TMenuEntryList;

function TMenuItem.DoClick: boolean;
begin
 Result := false;
end;

function TMenuItem.KeyString(out S: string): boolean;
begin
  Result := CastleWindow.KeyString(CharKey, Key, Modifiers, S);
end;

function TMenuItem.KeyMatches(const AKey: TKey; const ACharKey: char;
  const AModifiers: TModifierKeys): boolean;
begin
  { AModifiers (currently pressed) must be a superset of required Modifiers. }
  if not (AModifiers >= Modifiers) then
    Exit(false);

  { For CtrlH (= CharBackspace), *do not* match backspace key. }
  if (Key = K_None) and (CharKey = CharBackSpace) then
    Result := (ACharKey = CharBackSpace) and not (AKey = K_BackSpace) else

  if (Key = K_None) and (CharKey = CharEnter) then
    Result := (ACharKey = CharEnter) and not (AKey = K_Enter) else

  if (Key = K_None) and (CharKey = CharTab) then
    Result := (ACharKey = CharTab) and not (AKey = K_Tab) else

    Result :=
      ( (AKey <> K_None) and (Key = AKey) ) or
      ( (ACharKey <> #0) and (CharKey = ACharKey) );
end;

function TMenuItem.CaptionWithKey: string;
var KeyStr: string;
begin
 Result := Caption;
 if KeyString(KeyStr) then
  Result += ' [' + KeyStr + ']';
end;

constructor TMenuItem.Create(const ACaption: String; AIntData: Integer);
begin
 inherited Create(ACaption);
 FIntData := AIntData;

 FSmallId := MenuItems.Count;
 MenuItems.Add(Self);
end;

constructor TMenuItem.Create(const ACaption: String; AIntData: Integer;
  ACharKey: char);
begin
 Create(ACaption, AIntData);
 FCharKey := ACharKey;
end;

constructor TMenuItem.Create(const ACaption: String; AIntData: Integer;
  AKey: TKey);
begin
 Create(ACaption, AIntData);
 FKey := AKey;
end;

destructor TMenuItem.Destroy;
begin
 { We must set MenuItems[SmallId] so that MenuItemFromSmallId
   will never try to dereference a non-existing object.
   Note that we can't simply delete SmallId item from MenuItems
   because that would shift all following objects (and made their SmallId
   not corresponging to their index on MenuItems). }
 MenuItems[SmallId] := nil;
 inherited;
end;

function MenuItemFromSmallId(SearchSmallId: Integer): TMenuItem;
var MenuItem: TMenuItem;
    i: Integer;
begin
 for i := 0 to MenuItems.Count-1 do
 begin
  MenuItem := TMenuItem(MenuItems[i]);
  if (MenuItem <> nil) and (MenuItem.SmallId = SearchSmallId) then
   Exit(MenuItem);
 end;

 raise EInternalError.CreateFmt(
   'MenuItemFromSmallId: SearchSmallId %d not found', [SearchSmallId]);
end;

{ TMenuItemChecked ------------------------------------------------------------ }

procedure TMenuItemChecked.DoAutoCheckedToggle;
begin
  Checked := not Checked;
end;

function TMenuItemChecked.DoClick: boolean;
begin
 if inherited DoClick then Exit(true);

 if AutoCheckedToggle then
   DoAutoCheckedToggle;

 Result := false;
end;

procedure TMenuItemChecked.SetChecked(Value: boolean);
var
  P: TCastleWindowCustom;
begin
 if Value <> Checked then
 begin
  FChecked := Value;

  P := CorrectParentWindow;
  if (P <> nil) and (not P.Closed) then P.MenuUpdateChecked(Self);
 end;
end;

constructor TMenuItemChecked.Create(const ACaption: String; AIntData: Integer;
  AChecked, AAutoCheckedToggle: boolean);
begin
 inherited Create(ACaption, AIntData);
 FChecked := AChecked;
 FAutoCheckedToggle := AAutoCheckedToggle;
end;

constructor TMenuItemChecked.Create(const ACaption: String; AIntData: Integer;
  ACharKey: char;
  AChecked, AAutoCheckedToggle: boolean);
begin
 inherited Create(ACaption, AIntData, ACharKey);
 FChecked := AChecked;
 FAutoCheckedToggle := AAutoCheckedToggle;
end;

constructor TMenuItemChecked.Create(const ACaption: String; AIntData: Integer;
  AKey: TKey;
  AChecked, AAutoCheckedToggle: boolean);
begin
 inherited Create(ACaption, AIntData, AKey);
 FChecked := AChecked;
 FAutoCheckedToggle := AAutoCheckedToggle;
end;

{ TMenuItemRadio ------------------------------------------------------------- }

constructor TMenuItemRadio.Create(const ACaption: String; AIntData: Integer;
  AChecked, AAutoCheckedToggle: boolean);
begin
  inherited;
  CreateCommon;
end;

constructor TMenuItemRadio.Create(const ACaption: String; AIntData: Integer;
  ACharKey: char;
  AChecked, AAutoCheckedToggle: boolean);
begin
  inherited;
  CreateCommon;
end;

constructor TMenuItemRadio.Create(const ACaption: String; AIntData: Integer;
  AKey: TKey;
  AChecked, AAutoCheckedToggle: boolean);
begin
  inherited;
  CreateCommon;
end;

procedure TMenuItemRadio.CreateCommon;
begin
  FGroup := TMenuItemRadioGroup.Create(false);
  FGroup.Add(Self);
end;

destructor TMenuItemRadio.Destroy;
begin
  if FGroup.Count > 1 then
    FGroup.Remove(Self) else
    FreeAndNil(FGroup);

  inherited;
end;

procedure TMenuItemRadio.SetGroup(Value: TMenuItemRadioGroup);
begin
  Value.Add(Self);
end;

procedure TMenuItemRadio.SetChecked(Value: boolean);
begin
  { MenuUpdateChecked inside inherited here is only for this menu item.
    Later Group.SetSelected takes care to notify about the changes
    the rest of the group. }
  inherited;

  if Group.Selected <> Self then
  begin
    if Checked then
      Group.Selected := Self;
  end else
  begin
    if not Checked then
      Group.Selected := nil;
  end;
end;

procedure TMenuItemRadio.DoAutoCheckedToggle;
begin
  Group.Selected := Self;
end;

{ TMenuItemRadioGroup -------------------------------------------------------- }

procedure TMenuItemRadioGroup.Add(Item: TMenuItemRadio);
begin
  if Count = 0 then
  begin
    { Then this is the addition of the first radio item called from
      TMenuItemRadio.CreateCommon. Handle this just by calling inherited. }
    inherited Add(Item);
  end else
  begin
    { Remove item from it's previous group }
    if Item.FGroup.Count > 1 then
      Item.FGroup.Remove(Item) else
      SysUtils.FreeAndNil(Item.FGroup);

    inherited Add(Item);

    Item.FGroup := Self;
  end;

  if Item.Checked then
  begin
    if Selected <> nil then
      Item.Checked := false else
      FSelected := Item;
  end;
end;

procedure TMenuItemRadioGroup.Remove(Item: TMenuItemRadio);
begin
  if Selected = Item then
    FSelected := nil;
  inherited Remove(Item);
  Item.FGroup := nil;
end;

procedure TMenuItemRadioGroup.SetSelected(Value: TMenuItemRadio);
var
  I: Integer;
  Item: TMenuItemRadio;
  CheckedFast, SelectedFound: boolean;
  P: TCastleWindowCustom;
begin
  { When count is 0, we have no items belonging to our group.
    So selected may only be nil.

    This case is never used now: existing TMenuItemRadioGroup are always
    created when needed, and are automatically managed by TMenuItemRadio
    items, and are freed when there are no more items.
    So actually count is never 0, *for now*. We may allow creating
    and keeping empty TMenuItemRadioGroup at some point. }
  if Count = 0 then
  begin
    Assert(Value = nil, 'TMenuItemRadioGroup has no items, SetSelected may only be called with nil parameter');
    FSelected := nil;
    Exit;
  end;

  P := Items[0].CorrectParentWindow;
  if (P <> nil) and P.Closed then P := nil;

  { When not CheckedFast, then each SetChecked is expensive.
    So it's better to operate here on internal FChecked field
    and call MenuFinalize/MenuInitialize only once.
    When CheckedFast, don't do this, as using MenuFinalize/MenuInitialize
    would be slower then (and would recreate menu,
    e.g. hiding detached menus on GTK). }
  CheckedFast := (P = nil) or P.MenuUpdateCheckedFast;

  if not CheckedFast then
    P.MenuFinalize;

  FSelected := Value;
  if FSelected <> nil then
  begin
    SelectedFound := false;

    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      Item.FChecked := Item = Selected;
      if Item.FChecked then
        SelectedFound := true;
      if CheckedFast then
        if P <> nil then P.MenuUpdateChecked(Item);
    end;

    Assert(SelectedFound, 'Menu item set as TMenuItemRadioGroup.Selected is not part of this TMenuItemRadioGroup');
  end;

  if not CheckedFast then
    P.MenuInitialize;
end;

(*
procedure TMenuItemRadioGroup.DebugWriteln;
var
  I: Integer;
begin
  Writeln('--------------------');
  for I := 0 to Count - 1 do
  begin
    Writeln(I, ': ', Items[I].Caption:20, ' ', Items[I].Checked);
    Assert(Items[I].Group = Self);
    Assert((not Items[I].Checked) or (Selected = Items[I]));
  end;
  Writeln('--------------------');
end;
*)

function TMenuItemRadioGroup.SelectedIndex: Integer;
begin
  if Selected <> nil then
  begin
    Result := IndexOf(Selected);
    Assert(Result <> -1, 'Selected item of TMenuItemRadioGroup does not belong to this group');
  end else
    Result := -1;
end;

function TMenuItemRadioGroup.Previous(out Item: TMenuItemRadio): boolean;
var
  SI: Integer;
begin
  SI := SelectedIndex;
  if SI <> -1 then
  begin
    Item := Items[Max(SI - 1, 0)];
    Result := true;
  end else
  begin
    Item := nil;
    Result := false;
  end;
end;

function TMenuItemRadioGroup.Next(out Item: TMenuItemRadio): boolean;
var
  SI: Integer;
begin
  SI := SelectedIndex;
  if SI <> -1 then
  begin
    Item := Items[Min(SI + 1, Count - 1)];
    Result := true;
  end else
  begin
    Item := nil;
    Result := false;
  end;
end;

{ TMenuItemToggleFullScreen -------------------------------------------------- }

constructor TMenuItemToggleFullScreen.Create(const InitialFullScreen: boolean);
begin
  inherited Create('_Full Screen', 0, K_F11, InitialFullScreen, true);
  {$ifdef DARWIN} Modifiers := [mkCtrl]; {$endif}
end;

function TMenuItemToggleFullScreen.DoClick: boolean;
begin
  Result := inherited;
  if not Result then
  begin
    with CorrectParentWindow do FullScreen := not FullScreen;
    Result := true;
  end;
end;

{ init/fini ------------------------------------------------------------ }

procedure CastleWindowMenu_Init;
begin
 MenuItems := TMenuEntryList.Create(false);
end;

procedure CastleWindowMenu_Fini;
begin
 FreeAndNil(MenuItems);
end;

{ Some useful things ---------------------------------------- }

function SRemoveMnemonics(const S: string): string;
var SPos, ResultPos: Integer;
begin
 { I'm utlizing here the fact that Result for sure will be
   shorter or equal to S }
 SetLength(Result, Length(S));

 ResultPos := 1;
 SPos := 1;
 while SPos <= Length(S) do
 begin
  if S[SPos] = '_' then
  begin
   if (SPos < Length(S)) and (S[SPos + 1] = '_') then
   begin
    Result[ResultPos] := '_';
    Inc(ResultPos);
    Inc(SPos, 2);
   end else
   begin
    Inc(SPos);
   end;
  end else
  begin
   Result[ResultPos] := S[SPos];
   Inc(ResultPos);
   Inc(SPos);
  end;
 end;

 SetLength(Result, ResultPos - 1);
end;

function SQuoteMenuEntryCaption(const S: string): string;
begin
 Result := StringReplace(S, '_', '__', [rfReplaceAll]);
end;

{$endif read_implementation}
