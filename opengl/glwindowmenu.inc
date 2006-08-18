{
  Copyright 2004-2006 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ This file declares and implements GLWindow menu classes.
  TMenu, TMenuSeparator, TMenuItem, TMenuItemChecked etc.
}

{$ifdef read_interface}

  { This is a basic class representing basic menu building block.
    This can be a separator, clickable menu item or something that
    expands to submenu. }
  TMenuEntry = class
  private
    { if ParentWindow <> nil, TGLWindow(ParentWindow).MainMenuChanged. }
    procedure Changed;

    { This is something internal for proper communication between this module
      and GLWindow.

      This will be updated by glwindow in Init and MainMenuChanged
      (to Self) and in Close (to nil). This will be done processing from MainMenu
      recursively. This will be used by TMenuEntry to do Changed. }
    ParentWindow: TGLWindow;
  end;

  TObjectsListItem_2 = TMenuEntry;
  {$I objectslist_2.inc}
  TMenuEntriesList = TObjectsList_2;

  TMenuEntryWithCaption = class(TMenuEntry)
  private
    FCaption: String;
    procedure SetCaption(const Value: String);

    FEnabled: boolean;
    procedure SetEnabled(Value: boolean);
  public
    constructor Create(const ACaption: String);

    { Caption of this menu entry. Will be shown to user.

      To indicate that some letter of a caption is a mnemonic
      for this menu item put '_' (underscore) char before this
      letter. Depending on GLWindow-backend-dependent,
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
      but results may be GLWindow-backend-dependent.

      Just like in GTK toolkit gtk_label_new_with_mnemonic and
      gtk_menu_item_new_with_mnemonic. }
    property Caption: String read FCaption write SetCaption;

    property Enabled: boolean read FEnabled write SetEnabled
      default true;
  end;

  { TMenuEntry that contains a list of menu entries.
    This is the basic class to represent a drop-down menu, a submenu etc. }
  TMenu = class(TMenuEntryWithCaption)
  private
    FEntries: TMenuEntriesList;
    function GetEntries(Index: Integer): TMenuEntry;
  public
    { This objects OWNS his Entries. This means that when you Destroy this
      object, all Entries[] on the list will be also destroyed.
      This also means that EntryDelete frees deleted objects. }
    property Entries[Index: Integer]:TMenuEntry read GetEntries; default;
    function EntriesCount: Integer;
    procedure Insert(Index: Integer; Value: TMenuEntry);
    { Same as Insert(EntriesCount, Value) }
    procedure Append(Value: TMenuEntry);
    procedure EntryDelete(Index: Integer);
    procedure EntriesDeleteAll;
    constructor Create(const ACaption: String);
    destructor Destroy; override;
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
      So it's NOT something like TPointerSInt(Self).
      This means that as long as you won't create, say, more than 60 000
      menu items in your program (and that's a reasonable assumption) you
      can count on the fact that "every SmallId is >= 0 and < 60 000".

      And this makes SmallId very useful when you're constructing a menu
      using some API (like glut, Motif, WinAPI) -- you usually want to identify
      your menu item by some unique id and you want this id to be small
      (e.g. under WinAPI menu item's id must fit in 16 bit integer).
      That's where TPointerSInt(Self) is not useful and that's where
      this SmallId is useful.

      @seealso MenuItemFromSmallId }
    property SmallId: Integer read FSmallId;

    { This will be called when this MenuItem will be choosen by a user
      (or user will press key matching with our Key/CharKey).

      You can override this and return true if you handled the command.
      If this will return false, GLWindow will call EventMenuCommand
      (OnMenuCommand).

      When entering this method, current OpenGL context is set to
      the context of the window that the clicked menu belongs to.

      Default implementation of this method in this class always returns false. }
    function DoCommand: boolean; virtual;

    { If Key <> K_None and user will press this Key
      then this object's DoCommand will be called
      (and TGLWindow.EventMenuCommnd, and TGLWindow.OnMenuCommand,
      if DoCommand returns false).
      Similiar, is CharKey <> #0 and user will press a key with CharKey
      then it will be treated as this menu item click.

      Such key press redirection is handled internally in GLWindow
      (possibly it's even internally handled in some library on top
      of which GLWindow is implemented, e.g. WinAPI and GTK menus
      can be explicitly connected with some key press
      (i.e. without doing any special redirection keypress -> DoCommand
      in GLWindow).
      So you will not get EventKeyDown (OnKeyDown) messages for such
      keypresses (that translate to menu click). Although TGLWindow.KeysDown
      will be appropriately updated, of course.

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

      TODO:  change these to writeable }
    property CharKey: char read FCharKey; { = #0 }
    property Key: TKey read FKey; { = K_None }

    { This returns as S the string that describes CharKey (if CharKey <> #0)
      or Key (if Key <> K_None) and then returns true.
      If both CharKey = #0 and Key = K_None then returns false and
      S is undefined. }
    function KeyString(out S: string): boolean;

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
    procedure SetChecked(Value: boolean);
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

{$endif read_interface}

{$ifdef read_interface_2}

{ SearchSmallId MUST be a SmallId of some existing (i.e. Created and not
  Destroyed yet) TMenuItem. This function returns this TMenuItem.
  (i.e. such Item: TMenuItem that Item.SmallId = SearchSmallId) }
function MenuItemFromSmallId(SearchSmallId: Integer): TMenuItem;

{ Returns S with each '__' replaced with single '_',
  any other '_' removed.

  In other words: with mnemonics (as defined by
  @link(TMenuEntryWithCaption.Caption) removed. }
function SRemoveMnemonics(const S: string): string;

{ Returns S with each underscore '_' replaced by two underscores, '__'.

  In other words: S does will not contain any mnemonics.
  If you will assign S to @link(TMenuEntryWithCaption.Caption),
  then this menu entry caption will display exactly S,
  without any mnemonics. Single '_' in S will be displayed exactly
  as single '_'. }
function SQuoteMenuEntryCaption(const S: string): string;

{$endif read_interface_2}

{$ifdef read_implementation}

{ Implementation of these classes is NOT dependent on GLWindow
  implementation (i.e. it's not GTK or XLib or WinAPI or Glut specific).
  That was the original idea of this unit. Once these classes
  were even declared in a separate unit (that was not dependent on GLWindow
  unit).

  Such separation makes implementation of these things simple --
  it's not cluttered with GTK/Xlib/WinAPI/Glut specific details.
  Implementation of GLWindows-specific things is also simple --
  usually it's only one large function that is called from Init
  and from trivial TGLWindow.MainMenuChangedImplDepend.
  However, such implementation is not efficient. So maybe this
  will change in the future.
}

{$I ObjectsList_2.inc}

{ TMenuEntry ------------------------------------------------------------ }

procedure TMenuEntry.Changed;
begin
 if ParentWindow <> nil then TGLWindow(ParentWindow).MainMenuChanged;
end;

{ TMenuEntryWithCaption ---------------------------------------- }

constructor TMenuEntryWithCaption.Create(const ACaption: String);
begin
  inherited Create;
  FCaption := ACaption;
  FEnabled := true;
end;

procedure TMenuEntryWithCaption.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed;
end;

procedure TMenuEntryWithCaption.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

{ TMenu ------------------------------------------------------------ }

constructor TMenu.Create(const ACaption: String);
begin
 inherited Create(ACaption);
 FEntries := TMenuEntriesList.Create;
end;

destructor TMenu.Destroy;
begin
 FreeWithContentsAndNil(FEntries);
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
begin
 FEntries.Insert(Index, Value);
 Changed;
end;

procedure TMenu.Append(Value: TMenuEntry);
begin
 Insert(FEntries.Count, Value);
end;

procedure TMenu.EntryDelete(Index: Integer);
begin
  FEntries[Index].Free;
  FEntries.Delete(Index);
  Changed;
end;

procedure TMenu.EntriesDeleteAll;
begin
  FEntries.FreeContents;
  Changed;
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
  MenuItems: TMenuEntriesList;

function TMenuItem.DoCommand: boolean;
begin
 Result := false;
end;

function TMenuItem.KeyString(out S: string): boolean;
begin
 if CharKey <> #0 then
 begin
  S := DescribeKey(CharKey);
  Result := true;
 end else
 if Key <> K_None then
 begin
  S := KeyToStr(Key);
  Result := true;
 end else
 Result := false;
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

function TMenuItemChecked.DoCommand: boolean;
begin
 if inherited DoCommand then Exit(true);

 { Changing Checked will also call Changed; }
 if AutoCheckedToggle then Checked := not Checked;
 Result := false;
end;

procedure TMenuItemChecked.SetChecked(Value: boolean);
begin
 if Value <> Checked then
 begin
  FChecked := Value;
  Changed;
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

{ init/fini ------------------------------------------------------------ }

procedure GLWindowMenu_Init;
begin
 MenuItems := TMenuEntriesList.Create;
end;

procedure GLWindowMenu_Fini;
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