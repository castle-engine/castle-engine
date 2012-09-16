{
  Copyright 2004-2012 Michalis Kamburelis.

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
      TCastleWindowBase implementation. }

    { ParentWindow to call MenuFinalize, MenuInitialize.

      Used only on root menu item
      (the one with ParentMenu = @nil). From TMenuEntry implementations,
      you should use CorrentParentWindow, that looks for it recursively.
      This way we only have to update it on SetMainMenu (no need to keep
      it recursively updated in all TMenuEntry instances). }
    ParentWindow: TCastleWindowBase;

    { Internal handle of this menu item within CastleWindow backend.
      This is used by CastleWindow backend implementation, to store the
      intgernal resource corresponding to this menu entry.
      The exact interpretation depends on CastleWindow backend,
      and should not be messed with. }
    Handle: Pointer;

    FParentMenu: TMenu;
    FParentMenuPosition: Cardinal;

    function CorrectParentWindow: TCastleWindowBase;
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
  public
    { Items (entries) on the menu. Items are owned by this menu instance
      (are automatically freed at destruction, at EntryDelete and such). }
    property Entries[Index: Integer]:TMenuEntry read GetEntries; default;
    function EntriesCount: Integer;
    procedure Insert(Index: Integer; Value: TMenuEntry);
    { Add at the end. Same as Insert(EntriesCount, Value). }
    procedure Append(Value: TMenuEntry);
    procedure EntryDelete(Index: Integer);
    procedure EntriesDeleteAll;
    constructor Create(const ACaption: String);
    destructor Destroy; override;

    { Append a number of radio menu items.
      This is a comfortable shortcut for an often task of adding
      many TMenuItemRadio items that create a new radio group
      (TMenuItemRadioGroup).

      Otherwise, it creates new

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
  public
    { This is any data for user-specific purposes. }
    property IntData: Integer read FIntData write FIntData;

    { This is internally initialized at creation of this object.
      Each existing TMenuItem will have a unique SmallId. Each "existing" --
      -- this means that after destroying some TMenuItem object, it's SmallId
      may be reused by some TMenuItem created after that (even if this does not
      occur now).

      The intention is that this SmallId should be smallest possible value >= 0.
      So it's NOT something like PtrInt(Self).
      This means that as long as you won't create, say, more than 60 000
      menu items in your program (and that's a reasonable assumption) you
      can count on the fact that "every SmallId is >= 0 and < 60 000".

      And this makes SmallId very useful when you're constructing a menu
      using some API (like glut, Motif, WinAPI) -- you usually want to identify
      your menu item by some unique id and you want this id to be small
      (e.g. under WinAPI menu item's id must fit in 16 bit integer).
      That's where PtrInt(Self) is not useful and that's where
      this SmallId is useful.

      @seealso MenuItemFromSmallId }
    property SmallId: Integer read FSmallId;

    { This will be called when this MenuItem will be choosen by a user
      (or user will press key matching with our Key/CharKey).

      You can override this and return true if you handled the command.
      If this will return false, CastleWindow will call EventMenuCommand
      (OnMenuCommand).

      When entering this method, current OpenGL context is set to
      the context of the window that the clicked menu belongs to.

      Default implementation of this method in this class always returns false. }
    function DoCommand: boolean; virtual;

    { Key shortcuts for this menu item.

      If Key <> K_None and user will press this Key
      then this object's DoCommand will be called
      (and TCastleWindowBase.EventMenuCommnd, and TCastleWindowBase.OnMenuCommand,
      if DoCommand returns false).
      Similiar, is CharKey <> #0 and user will press a key with CharKey
      then it will be treated as this menu item click.

      Such key press redirection is handled internally in CastleWindow
      (possibly it's even internally handled in some library on top
      of which CastleWindow is implemented, e.g. WinAPI and GTK menus
      can be explicitly connected with some key press
      (i.e. without doing any special redirection keypress -> DoCommand
      in CastleWindow).
      So you will not get EventPress (OnPress) messages for such
      keypresses (that translate to menu click). Although
      TCastleWindowBase.Pressed.Keys will be appropriately updated, of course.

      Note: Caption of this object should not contain
      description of Key and CharKey, such description will be automatically
      shown to user (maybe by adding something like ' [Ctrl+O]' to displayed
      Caption, maybe in some better (more visually-pleasing) way).

      Note: you don't have to give really corresponding Key and CharKey
      values, as such correspondency may be dependent on state of modifier keys
      and OS configuration.

      In fact, usually you SHOULD give Key = K_None or c = #0.
      E.g. you should NOT set Key = K_C and CharKey = 'c'.
      Instead you should set Key = K_C and CharKey = #0 or
      Key = K_None and CharKey = 'c'. This way you avoid some uncertainty:
      Key = K_C is something other than CharKey = 'c' (e.g. Shift+C
      generates K_C, but does not translate as CharKey = 'c' (it is translated
      as CharKey = 'C' ('C' uppercase !)).

      Notes about CharKey values #8 (CharBackSpace = CtrlH),
      #9 (CharTab = CtrlI), #13 (CharEnter = CtrlM): these are problematic,
      since they may mean backspace/tab/enter or combinations with Ctrl key.
      Our algorithm to deal with this:

      @orderedList(
        @item(If your CharKey is one of the CharBackSpace/Tab/Enter constants,
          @italic(and @link(Key) is K_None), then you mean the combination
          with Ctrl key. That's how it's displayed, and moreover: keyboard
          presses with K_BackSpace/K_Tab/K_Enter do @italic(not) trigger
          this menu item.

          This way menu items with Ctrl+H or Ctrl+M or Ctrl+I do not block
          you from assigning keyboard K_BackSpace/K_Tab/K_Enter
          to some other useful action.)

        @item(If you really want to assign keyboard backspace/tab/enter
          presses to menu item, use TKey constants K_BackSpace, K_Tab, K_Enter.)
      )

      Design notes: I am no longer using TMenuItemKey class.
      Instead I put this functionality into base TMenuItem class.
      Before, I had in this class TMenuItemChecked functionality,
      but it's better to put here TMenuItemKey functionality.
      That's because in this class we have a natural way to specify
      TMenuItem that does not translate to any key (Key = K_None and
      CharKey = #0). While with TMenuItemChecked it would require
      additional property ShowChecked. And I HAVE to integrate into
      TMenuItem class some functionality -- otherwise, if I would
      separated both TMenuItemKey and TMenuItemChecked classes,
      then I would be forced to create additional TMenuItemKeyChecked
      class.

      TODO:  change these to writeable

      @groupBegin }
    property CharKey: char read FCharKey default #0;
    property Key: TKey read FKey default K_None;
    { @groupEnd }

    { This returns as S the string that describes CharKey (if CharKey <> #0)
      or Key (if Key <> K_None) and then returns true.
      If both CharKey = #0 and Key = K_None then returns false and
      S is undefined. }
    function KeyString(out S: string): boolean;

    function KeyMatches(const AKey: TKey; const ACharKey: char): boolean;

    { This is Caption with optional key description (returned by KeyString)
      appended in '[' ... ']'. }
    function CaptionWithKey: string;

    constructor Create(const ACaption: String; AIntData: Integer); overload;
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char); overload;
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey); overload;

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

    { This is called from DoCommand when AutoCheckedToggle is @true.
      It's supposed to actually implement the AutoCheckedToggle behavior.

      Note that this is overriden ina a "dirty" way
      (i.e. not calling "inherited") in TMenuItemRadio descendant. }
    procedure DoAutoCheckedToggle; virtual;
  public
    property Checked: boolean read FChecked write SetChecked;

    { true means that each time user chooses this menu item,
      Checked is changed to not Checked. }
    property AutoCheckedToggle: boolean read FAutoCheckedToggle
      write FAutoCheckedToggle;

    { This always returns false.
      It is overriden here only to handle AutoCheckedToggle }
    function DoCommand: boolean; override;

    constructor Create(const ACaption: String; AIntData: Integer;
      AChecked, AAutoCheckedToggle: boolean); overload;
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char;
      AChecked, AAutoCheckedToggle: boolean); overload;
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey;
      AChecked, AAutoCheckedToggle: boolean); overload;
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
      AChecked, AAutoCheckedToggle: boolean); overload;
    constructor Create(const ACaption: String; AIntData: Integer; ACharKey: char;
      AChecked, AAutoCheckedToggle: boolean); overload;
    constructor Create(const ACaption: String; AIntData: Integer; AKey: TKey;
      AChecked, AAutoCheckedToggle: boolean); overload;

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

    (* procedure DebugWriteln; *)
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
  implementation (i.e. it's not GTK or XLib or WinAPI or Glut specific).
  That was the original idea of this unit. Once these classes
  were even declared in a separate unit (that was not dependent on CastleWindow
  unit).

  Such separation makes implementation of these things simple --
  it's not cluttered with GTK/Xlib/WinAPI/Glut specific details.
}

{ TMenuEntry ------------------------------------------------------------ }

function TMenuEntry.CorrectParentWindow: TCastleWindowBase;
begin
  if ParentMenu = nil then
    Result := ParentWindow else
    Result := ParentMenu.CorrectParentWindow;
end;

procedure TMenuEntry.MenuInitialize;
var
  P: TCastleWindowBase;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuInitialize;
end;

procedure TMenuEntry.MenuFinalize;
var
  P: TCastleWindowBase;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuFinalize;
end;

procedure TMenuEntry.MenuUpdateBegin;
var
  P: TCastleWindowBase;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuUpdateBegin;
end;

procedure TMenuEntry.MenuUpdateEnd;
var
  P: TCastleWindowBase;
begin
  P := CorrectParentWindow;
  if P <> nil then P.MenuUpdateEnd;
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
  P: TCastleWindowBase;
begin
  FCaption := Value;

  P := CorrectParentWindow;
  if (P <> nil) and (not P.Closed) then P.MenuUpdateCaption(Self);
end;

procedure TMenuEntryWithCaption.SetEnabled(Value: boolean);
var
  P: TCastleWindowBase;
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
  Result := FEntries.Count;
end;

procedure TMenu.Insert(Index: Integer; Value: TMenuEntry);
var
  I: Integer;
begin
  MenuFinalize;

  FEntries.Insert(Index, Value);

  { update Value.FParentMenu }
  Value.FParentMenu := Self;

  { update changed FParentMenuPosition values (this includes
    Value.FParentMenuPosition) }
  for I := Index to EntriesCount - 1 do
    FEntries[I].FParentMenuPosition := I;

  MenuInitialize;
end;

procedure TMenu.Append(Value: TMenuEntry);
begin
  Insert(FEntries.Count, Value);
end;

procedure TMenu.EntryDelete(Index: Integer);
var
  I: Integer;
begin
  MenuFinalize;

  FEntries.Delete(Index);

  { update changed FParentMenuPosition values }
  for I := Index to EntriesCount - 1 do
    FEntries[I].FParentMenuPosition := I;

  MenuInitialize;
end;

procedure TMenu.EntriesDeleteAll;
begin
  MenuFinalize;
  FEntries.Clear;
  MenuInitialize;
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

function TMenuItem.DoCommand: boolean;
begin
 Result := false;
end;

function TMenuItem.KeyString(out S: string): boolean;
begin
 if CharKey <> #0 then
 begin
  S := CharToNiceStr(CharKey, false);
  Result := true;
 end else
 if Key <> K_None then
 begin
  S := KeyToStr(Key);
  Result := true;
 end else
 Result := false;
end;

function TMenuItem.KeyMatches(const AKey: TKey; const ACharKey: char): boolean;
begin
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

function TMenuItemChecked.DoCommand: boolean;
begin
 if inherited DoCommand then Exit(true);

 if AutoCheckedToggle then
   DoAutoCheckedToggle;

 Result := false;
end;

procedure TMenuItemChecked.SetChecked(Value: boolean);
var
  P: TCastleWindowBase;
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
  CheckedFast: boolean;
  P: TCastleWindowBase;
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

  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    Item.FChecked := Item = Selected;
    if CheckedFast then
      if P <> nil then P.MenuUpdateChecked(Item);
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