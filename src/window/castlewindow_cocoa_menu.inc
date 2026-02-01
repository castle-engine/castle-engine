{%MainUnit castlewindow.pas}
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of TCastleWindow.MainMenu for Cocoa.

  Lots of inspiration from LCL Cocoa widgetset code.
}

type
  TCocoaMenuItem = objcclass(NSMenuItem)
    ParentWindow: TCastleWindow;
    MenuItem: TMenuItem;
    procedure castleClick(sender: id); message 'castleClick:';
  end;

procedure TCocoaMenuItem.castleClick(sender: id);
begin
  //WritelnLog('DoMenuClick ', MenuItem.Caption);
  ParentWindow.DoMenuClick(MenuItem);
end;

const
  CocoaMenuState: array [Boolean] of NSInteger = (NSOffState, NSOnState);

procedure TCastleWindow.BackendMenuInitialize;

  { Cocoa menu item keyEquivalent which represents menu item key shortcut. }
  procedure CocoaMenuKey(const KeyString: String; const Key: TKey; const Modifiers: TModifierKeys;
    out CocoaKeyEquivalent: String;
    out CocoaKeyModifier: NSUInteger);
  var
    // KeyString expressed as a single 8-bit character, or 0 if not possible
    KeyString1: AnsiChar;
  begin
    if Length(KeyString) = 1 then
      KeyString1 := KeyString[1]
    else
      KeyString1 := #0;

    case KeyString1 of
      CtrlA .. CtrlZ:
        begin
          CocoaKeyEquivalent := Chr(Ord(KeyString1) - Ord(CtrlA) + Ord('a'));
          CocoaKeyModifier := NSCommandKeyMask {NSControlKeyMask};
        end;
      else
        begin
          CocoaKeyEquivalent := KeyString;
          CocoaKeyModifier := 0;

          if CocoaKeyEquivalent = '' then
            case Key of
              keyPageUp:     CocoaKeyEquivalent := UnicodeToUTF8(NSPageUpFunctionKey);
              keyPageDown:   CocoaKeyEquivalent := UnicodeToUTF8(NSPageDownFunctionKey);
              keyHome:       CocoaKeyEquivalent := UnicodeToUTF8(NSHomeFunctionKey);
              keyEnd:        CocoaKeyEquivalent := UnicodeToUTF8(NSEndFunctionKey);
              keyArrowUp:    CocoaKeyEquivalent := UnicodeToUTF8(NSUpArrowFunctionKey);
              keyArrowDown:  CocoaKeyEquivalent := UnicodeToUTF8(NSDownArrowFunctionKey);
              keyArrowLeft:  CocoaKeyEquivalent := UnicodeToUTF8(NSLeftArrowFunctionKey);
              keyArrowRight: CocoaKeyEquivalent := UnicodeToUTF8(NSRightArrowFunctionKey);
              keyDelete:     CocoaKeyEquivalent := UnicodeToUTF8(NSDeleteFunctionKey);
              keyInsert:     CocoaKeyEquivalent := UnicodeToUTF8(NSInsertFunctionKey);
              keyF1:         CocoaKeyEquivalent := UnicodeToUTF8(NSF1FunctionKey);
              keyF2:         CocoaKeyEquivalent := UnicodeToUTF8(NSF2FunctionKey);
              keyF3:         CocoaKeyEquivalent := UnicodeToUTF8(NSF3FunctionKey);
              keyF4:         CocoaKeyEquivalent := UnicodeToUTF8(NSF4FunctionKey);
              keyF5:         CocoaKeyEquivalent := UnicodeToUTF8(NSF5FunctionKey);
              keyF6:         CocoaKeyEquivalent := UnicodeToUTF8(NSF6FunctionKey);
              keyF7:         CocoaKeyEquivalent := UnicodeToUTF8(NSF7FunctionKey);
              keyF8:         CocoaKeyEquivalent := UnicodeToUTF8(NSF8FunctionKey);
              keyF9:         CocoaKeyEquivalent := UnicodeToUTF8(NSF9FunctionKey);
              keyF10:        CocoaKeyEquivalent := UnicodeToUTF8(NSF10FunctionKey);
              keyF11:        CocoaKeyEquivalent := UnicodeToUTF8(NSF11FunctionKey);
              keyF12:        CocoaKeyEquivalent := UnicodeToUTF8(NSF12FunctionKey);
              else ;
            end;
        end;
    end;

    if mkShift in Modifiers then
      CocoaKeyModifier := CocoaKeyModifier or NSShiftKeyMask;
    if mkCtrl in Modifiers then
      CocoaKeyModifier := CocoaKeyModifier or NSCommandKeyMask {NSControlKeyMask};
    if mkAlt in Modifiers then
      CocoaKeyModifier := CocoaKeyModifier or NSAlternateKeyMask;
  end;

  procedure AddMenuItem(const ParentMenu: NSMenu; const Entry: TMenuEntry);
  var
    I: Integer;
    EntryItem: TMenuItem;
    EntryMenu: TMenu;
    CocoaSubMenu: NSMenu;
    CocoaMenuItem: NSMenuItem; // sometimes, but not always, TCocoaMenuItem
    CocoaKeyEquivalent: String;
    CocoaKeyModifier: NSUInteger;
  begin
    if Entry is TMenuItem then
    begin
      EntryItem := TMenuItem(Entry);
      CocoaMenuKey(EntryItem.KeyString, EntryItem.Key, EntryItem.Modifiers,
        CocoaKeyEquivalent, CocoaKeyModifier);
      CocoaMenuItem := TCocoaMenuItem.alloc.initWithTitle_action_keyEquivalent(
        NSString.stringWithUTF8String(PChar(SRemoveMnemonics(EntryItem.Caption))),
        ObjCSelector('castleClick:'),
        NSSTR(CocoaKeyEquivalent)
      );
      TCocoaMenuItem(CocoaMenuItem).MenuItem := EntryItem;
      TCocoaMenuItem(CocoaMenuItem).ParentWindow := Self;
      CocoaMenuItem.SetEnabled(EntryItem.Enabled and (not MenuForcefullyDisabled));
      CocoaMenuItem.SetKeyEquivalentModifierMask(CocoaKeyModifier);
      CocoaMenuItem.setTarget(CocoaMenuItem); // determines where do we look for castleClick: selector
      if EntryItem is TMenuItemChecked then
        CocoaMenuItem.setState(CocoaMenuState[TMenuItemChecked(EntryItem).Checked]);
    end else
    if Entry is TMenuSeparator then
    begin
      CocoaMenuItem := NSMenuItem.SeparatorItem;
    end else
    if Entry is TMenu then
    begin
      EntryMenu := TMenu(Entry);
      CocoaSubMenu := NSMenu.alloc.initWithTitle(
        NSString.stringWithUTF8String(PChar(SRemoveMnemonics(EntryMenu.Caption))));
      CocoaSubMenu.setAutoenablesItems(false);
      for I := 0 to EntryMenu.Count - 1 do
        AddMenuItem(CocoaSubMenu, EntryMenu.Entries[I]);
      CocoaMenuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(
        NSString.stringWithUTF8String(PChar(SRemoveMnemonics(EntryMenu.Caption))), nil,
        NSSTR('')).autorelease;
      CocoaMenuItem.SetEnabled(EntryMenu.Enabled and (not MenuForcefullyDisabled));
      CocoaMenuItem.setSubmenu(CocoaSubMenu);
    end else
      raise EInternalError.Create('Not implemented TMenuEntry subclass');

    ParentMenu.AddItem(CocoaMenuItem);
    Entry.Handle := CocoaMenuItem;
  end;

var
  I: Integer;
begin
  NSApp.MainMenu.setAutoenablesItems(false);

  for I := 0 to MainMenu.Count - 1 do
    AddMenuItem(NSApp.MainMenu, MainMenu.Entries[I]);
end;

procedure TCastleWindow.BackendMenuFinalize;
var
  I: Integer;
begin
  for I := 0 to MainMenu.Count - 1 do
    NSApp.mainMenu.RemoveItem(NSMenuItem(MainMenu.Entries[I].Handle));
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
var
  CocoaMenuItem: NSMenuItem;
begin
  if Entry.Handle <> nil then
  begin
    CocoaMenuItem := NSMenuItem(Entry.Handle);
    CocoaMenuItem.SetTitle(NSSTR(Entry.Caption));
  end;
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
var
  CocoaMenuItem: NSMenuItem;
begin
  if Entry.Handle <> nil then
  begin
    CocoaMenuItem := NSMenuItem(Entry.Handle);
    CocoaMenuItem.SetEnabled(Entry.Enabled and (not MenuForcefullyDisabled));
  end;
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
var
  CocoaMenuItem: NSMenuItem;
begin
  if Entry.Handle <> nil then
  begin
    CocoaMenuItem := NSMenuItem(Entry.Handle);
    CocoaMenuItem.setState(CocoaMenuState[Entry.Checked]);
  end;
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.SetMenuForcefullyDisabled(const Value: Boolean);

  procedure UpdateForcefullyDisabled(const Menu: TMenu);
  var
    I: Integer;
  begin
    MenuUpdateEnabled(Menu);
    for I := 0 to Menu.Count - 1 do
      if Menu[I] is TMenu then
        UpdateForcefullyDisabled(TMenu(Menu[I]))
      else
      if Menu[I] is TMenuEntryWithCaption then
        MenuUpdateEnabled(TMenuEntryWithCaption(Menu[I]));
  end;

begin
  if FMenuForcefullyDisabled <> Value then
  begin
    FMenuForcefullyDisabled := Value;
    if MainMenu <> nil then
      UpdateForcefullyDisabled(MainMenu);
  end;
end;
