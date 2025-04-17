{%MainUnit ../castlewindow.pas}
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := false;
end;

procedure signal_menu_item_activate(AMenuItemGtk: PGtkMenuItem;
  data: gpointer); cdecl; forward;

procedure gtk_check_menu_item_set_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem; draw_as_radio:gboolean);
  cdecl; external gtklib;

function gtk_check_menu_item_get_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem):gboolean;
  cdecl; external gtklib;

function signal_menu_bar_activation(AGtkMenuBar: PGtkMenuBar;
  Event: PGdkEventButton; UserData: gpointer): gboolean; cdecl;
var
  Window: TCastleWindow absolute UserData;
begin
  { For reasons of using MenuRecreateForbidden, see signal_menu_item_select
    comments. }
  Inc(Window.MenuRecreateForbidden);
  try
    Window.ReleaseAllKeysAndMouse;
  finally
    Dec(Window.MenuRecreateForbidden);
  end;

  Result := gfalse;
end;

{ This is the only event I found that can be used in GTK 2 to
  "sniff" when menu is open. See ReleaseAllKeysAndMouse
  for comments why it's important to sniff it.

  Note that in GTK 2 even when menu is open, key up will correctly
  be called when you release keys over item in main menu bar
  (i.e. if your MainMenu contains directly TMenuItem).
  In this case key up is just passed to our WindowGtk.
  But this is of course only a special case, in general
  we still must take care ourselves to call ReleaseAllKeysAndMouse
  when needed. That's why proc below is needed.

  Note that we use MenuRecreateForbidden here, to prevent modification
  of menu from within ReleaseAllKeysAndMouse. Reason: menu recreate actually
  destroys and recreates GTK resources for this menu, and this causes
  GTK to SEGFAULT right after calling this callback.
  Not surprising --- we just destroyed the very resource that received focus...

  Menu recreate calls could happen when some KeyUp
  message will cause e.g. TWindowState.SetStandardState. For example,
  this happens in castle-model-viewer when KeyUp may produce message to KeySensor about
  key release, and this may produce ChangedAll on TCastleScene, which produces
  CastleWindowProgress run...

  MenuRecreateForbidden is not an ideal solution, menu should be eventually
  recreated at some later point, TODO... For now this works Ok,
  as TWindowState.SetStandardState only changes Enabled to false and later back
  to true, so actually recreating menu is not needed. }

function signal_menu_item_select(AItem: PGtkItem;
  UserData: gpointer): gboolean; cdecl;
var
  Window: TCastleWindow absolute UserData;
begin
  Inc(Window.MenuRecreateForbidden);
  try
    Window.ReleaseAllKeysAndMouse;
  finally
    Dec(Window.MenuRecreateForbidden);
  end;

  Result := gfalse;
end;

{ Block signal_menu_item_activate on this menu item.
  Useful inside signal_menu_item_activate, see there for comments.

  Note: MenuItemBlock/Unblock were changed in CASTLE_WINDOW_GTK_2
  to use g_signal_handlers_block/unblock_by_func instead of
  gtk_signal_handler_block/unblock_by_func. Although it should be
  perfectly correct to use gtk_signal_handler_xxx functions still in GTK_2,
  this caused some warnings printed by gtk:
    after doing gtk_signal_handler_block_by_func,
    next unblocking of this signal and any subsequent blocking and unblocking
    were writing
      (castle-model-viewer:4151): Gtk-WARNING **: unable to find signal handler for
      object(GtkCheckMenuItem:0x8358538) with func(0x80ec484) and data(0x6)
    (but indeed 0x8358538 and 0x80ec484 and 0x6 were exactly parameters
    of blocked signal ! In other words, it seems that blocking signal by
    gtk_signal_handler_block_by_func is doing something wrong that
    causes all subsequent block/unblock of this signal to display
    this strange warning.
  Switching to g_signal_handlers_block/unblock_xxx solves the problem,
  no warnings are displayed and everything works OK. }
procedure MenuItemBlock(MenuItemGtk: PGtkMenuItem; MenuItem: TMenuItem);
begin
 g_signal_handlers_block_by_func( GTK_OBJECT(MenuItemGtk),
   @signal_menu_item_activate, Pointer(PtrUInt(MenuItem.SmallId)));
end;

{ Unblock signal_menu_item_activate from this menu item.
  Useful inside signal_menu_item_activate, see there for comments. }
procedure MenuItemUnblock(MenuItemGtk: PGtkMenuItem; MenuItem: TMenuItem);
begin
 g_signal_handlers_unblock_by_func( GTK_OBJECT(MenuItemGtk),
   @signal_menu_item_activate, Pointer(PtrUInt(MenuItem.SmallId)));
end;

{ Initialize MainMenu.Handle (to GtkMenuBar) and insert it to window_vbox.

  It will not clear the previous value of MainMenu.Handle,
  it does not try to remove previous MainMenu.Handle from window_vbox
  etc. --- you have to take care of that yourself. This simply assumes
  that window_vbox is empty and current value of MainMenu.Handle
  is clear. }

procedure TCastleWindow.BackendMenuInitialize;

  { Install GTK menu accelerator (key shortcut) from given MenuItem. }
  procedure InstallGtkAccelerator(const Item: PGtkMenuItem;
    const MenuItem: TMenuItem);
  var
    Key: TKey;
    CharKey: Char;
    Modifiers: TModifierKeys;
    gdk_keyval: GUint;
    modifier_type: TGdkModifierType;
  begin
    Key := MenuItem.Key;
    if Length(MenuItem.KeyString) = 1 then
      CharKey := MenuItem.KeyString[1]
    else
      CharKey := #0;
    Modifiers := MenuItem.Modifiers;

    { Calculate GDK keyval gdk_keyval and modifier_type.
      (Look for possible gdk_keyval values in gdk/gdkkeysyms.h header
      (e.g. /usr/include/gtk-1.2/gdk/gdkkeysyms.h on my system)). }
    modifier_type := 0;
    case CharKey of
      CtrlA .. CtrlZ:
        { I could handle here specially three keys:
            CharBackspace: gdk_keyval := GDK_KEY_BackSpace;
            CharTab: gdk_keyval := GDK_KEY_Tab;
            CharEnter: gdk_keyval := GDK_KEY_Return;
          but it would be bad idea --- I would have no way to specify
          then actual Ctrl+H, Ctrl+I, Ctrl+M shortcuts.
          (While Backspace, Tab, Enter can always be specified by TKey
          constants keyBackSpace, keyTab, keyEnter).
        }
        begin
          gdk_keyval := GDK_KEY_A + Ord(CharKey) - Ord(CtrlA);
          modifier_type := GDK_CONTROL_MASK;
        end;

      CharEscape: gdk_keyval := GDK_KEY_Escape;
      '0' .. '9': gdk_keyval := GDK_KEY_0 + Ord(CharKey) - Ord('0');
      'a' .. 'z': gdk_keyval := GDK_KEY_A + Ord(CharKey) - Ord('a');
      'A' .. 'Z':
        begin
          gdk_keyval := GDK_KEY_A + Ord(CharKey) - Ord('A');
          modifier_type := GDK_SHIFT_MASK;
        end;
      '?': gdk_keyval := GDK_KEY_question;
      else
        case Key of
          keyBackSpace: gdk_keyval := GDK_KEY_BackSpace;
          keyTab: gdk_keyval := GDK_KEY_Tab;
          keyEnter: gdk_keyval := GDK_KEY_Return;

          { This is not proper translation: keyXxx constants do not make
            difference between left/right Ctrl/Shift/Alt,
            while GDK_KEY_Xxx constants do.

            Some solution would be to make two accelerators for one
            menu item -- TODO when this will be useful. }
          keyShift: gdk_keyval := GDK_KEY_Shift_L;
          keyCtrl: gdk_keyval := GDK_KEY_Control_L;
          keyAlt: gdk_keyval := GDK_KEY_Alt_L;

          keyEscape: gdk_keyval := GDK_KEY_Escape;
          keySpace: gdk_keyval := GDK_KEY_Space;
          keyPageUp: gdk_keyval := GDK_KEY_Page_Up;
          keyPageDown: gdk_keyval := GDK_KEY_Page_Down;
          keyEnd: gdk_keyval := GDK_KEY_End;
          keyHome: gdk_keyval := GDK_KEY_Home;
          keyArrowLeft: gdk_keyval := GDK_KEY_Left;
          keyArrowUp: gdk_keyval := GDK_KEY_Up;
          keyArrowRight: gdk_keyval := GDK_KEY_Right;
          keyArrowDown: gdk_keyval := GDK_KEY_Down;
          keyInsert: gdk_keyval := GDK_KEY_Insert;
          keyDelete: gdk_keyval := GDK_KEY_Delete;

          keyNumpadPlus: gdk_keyval := GDK_KEY_Plus;
          keyNumpadMinus: gdk_keyval := GDK_KEY_Minus;

          key0 .. key9: gdk_keyval := GDK_KEY_0 + Ord(Key) - Ord(key0);
          keyA .. keyZ: gdk_keyval := GDK_KEY_A + Ord(Key) - Ord(keyA);

          keyF1 : gdk_keyval := GDK_KEY_F1;
          keyF2 : gdk_keyval := GDK_KEY_F2;
          keyF3 : gdk_keyval := GDK_KEY_F3;
          keyF4 : gdk_keyval := GDK_KEY_F4;
          keyF5 : gdk_keyval := GDK_KEY_F5;
          keyF6 : gdk_keyval := GDK_KEY_F6;
          keyF7 : gdk_keyval := GDK_KEY_F7;
          keyF8 : gdk_keyval := GDK_KEY_F8;
          keyF9 : gdk_keyval := GDK_KEY_F9;
          keyF10: gdk_keyval := GDK_KEY_F10;
          keyF11: gdk_keyval := GDK_KEY_F11;
          keyF12: gdk_keyval := GDK_KEY_F12;

          keyComma: gdk_keyval := GDK_KEY_Comma;
          keyPeriod: gdk_keyval := GDK_KEY_Period;
          else Exit;
        end;
    end;

    if mkShift in Modifiers then
      modifier_type := modifier_type or GDK_SHIFT_MASK;
    if mkCtrl in Modifiers then
      modifier_type := modifier_type or GDK_CONTROL_MASK;
    if mkAlt in Modifiers then
      modifier_type := modifier_type or GDK_MOD1_MASK;

    gtk_widget_add_accelerator(PGtkWidget(Item), 'activate', window_accel_group,
      gdk_keyval, modifier_type, GTK_ACCEL_VISIBLE);
  end;

  { Assuming MainMenu <> nil, builds appropriate GtkMenuBar.
    Signals signal_menu_item_activate and signal_menu_bar_activation are
    registered. Everything that should be shown is shown.
    You just have to insert MainMenu.Handle in your window. }
  procedure MakeMainMenu;

    function MenuEntryToGtkMenuItem(Entry: TMenuEntry): PGtkMenuItem;

      { Returns fresh gtk_menu_new() }
      function NewGtkMenu(): PGtkMenu;
      begin
        Result := PGtkMenu(gtk_menu_new());
        { Do not show ! No "gtk_widget_show(GTK_WIDGET(Result));",
          you do not want to show GtkMenu (it will be automatically
          shown when user selects it with a mouse click etc.)  }
      end;

    var
      SubMenu: PGtkMenu;
      I: Integer;
      EntryItem: TMenuItem;
      EntryChecked: TMenuItemChecked;
      EntryMenu: TMenu;
    begin
      if Entry is TMenuItem then
      begin
        EntryItem := TMenuItem(Entry);
        if EntryItem is TMenuItemChecked then
        begin
          EntryChecked := TMenuItemChecked(EntryItem);
          Result := PGtkMenuItem(gtk_check_menu_item_new_with_mnemonic(
            PChar(EntryItem.Caption)));
          gtk_check_menu_item_set_active(PGtkCheckMenuItem(Result),
            EntryChecked.Checked);
          gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(Result), gtrue);

          { To implement TMenuItemRadio I use normal GTK check menu items
            and just make them look like radio items.

            That's because we implement radio behavior (radio groups,
            turning Checked on and turning Checked off of the rest)
            ourselves. So there's no need to map our groups to GTK radio
            groups (even though they follow the same design...).
            But in GTK "at each instant exactly one of the radio menu
            items from a group is selected." So I cannot just create
            radio items carelessly, I have to set their groups correctly.

            So it's easier for me just to use normal check menu items
            and only make them look like radios. }
          gtk_check_menu_item_set_draw_as_radio(
            PGtkCheckMenuItem(Result), Entry is TMenuItemRadio);
        end else
          Result := PGtkMenuItem(gtk_menu_item_new_with_mnemonic(
            PChar(EntryItem.Caption)));

        gtk_object_set_user_data(GTK_OBJECT(Result), Pointer(Self));
        gtk_signal_connect(GTK_OBJECT(Result), 'activate',
          GTK_SIGNAL_FUNC(@signal_menu_item_activate),
          Pointer(PtrUInt(EntryItem.SmallId)));

        InstallGtkAccelerator(Result, EntryItem);
      end else
      if Entry is TMenuSeparator then
      begin
        { gtk_menu_item_new() creates a separator menu item, this can be found
          in GTK FAQ. }
        Result := PGtkMenuItem(gtk_menu_item_new());
      end else
      if Entry is TMenu then
      begin
        EntryMenu := TMenu(Entry);
        Result := PGtkMenuItem(gtk_menu_item_new_with_mnemonic(
          PChar(EntryMenu.Caption)));

        SubMenu := NewGtkMenu();

        for i := 0 to EntryMenu.Count - 1 do
         gtk_menu_append(GTK_WIDGET(SubMenu),
           GTK_WIDGET( MenuEntryToGtkMenuItem(EntryMenu.Entries[i]) ));

        gtk_menu_item_set_submenu(Result, GTK_WIDGET(SubMenu));
      end else
        raise EInternalError.Create('Not implemented TMenuEntry subclass');

      Entry.Handle := Result;

      if Entry is TMenuEntryWithCaption then
        MenuUpdateEnabled(TMenuEntryWithCaption(Entry));

      gtk_signal_connect(GTK_OBJECT(Result), 'select',
        GTK_SIGNAL_FUNC(@signal_menu_item_select), Self);

      gtk_widget_show(GTK_WIDGET(Result));
    end;

  var
    I: Integer;
  begin
    MainMenu.Handle := PGtkMenuBar(gtk_menu_bar_new());

    MenuUpdateEnabled(MainMenu);

    gtk_widget_show(GTK_WIDGET(MainMenu.Handle));
    for I := 0 to MainMenu.Count - 1 do
      { Don't put TMenuSeparator in menu bar because that would look ugly. }
      if not (MainMenu.Entries[I] is TMenuSeparator) then
        gtk_menu_bar_append(GTK_WIDGET(MainMenu.Handle),
          GTK_WIDGET( MenuEntryToGtkMenuItem(MainMenu.Entries[I]) ));

    { How to catch the moment when the menu bar is opened/selected/clicked
      etc. by the user ? It should be caught when user enters menu by clicking
      on menu bar or pressing some menu key (like F10 in GTK 2).

      There is no way do this nicely in GTK 1 ?
      So I'm using threre very non-elegant soltuion to catch button-press-event.
      GTK 1 does not allow user to enter menu by F10 or some other key like that. }
    gtk_signal_connect(GTK_OBJECT(MainMenu.Handle), 'button-press-event',
      GTK_SIGNAL_FUNC(@signal_menu_bar_activation), Self);
  end;

begin
  if MenuRecreateForbidden <> 0 then Exit;

  MakeMainMenu;
  gtk_box_pack_start(PGtkBox(window_vbox), GTK_WIDGET(MainMenu.Handle),
    gfalse, gfalse, 0);
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  if MenuRecreateForbidden <> 0 then Exit;

  { According to GTK docs (at gtk_container_remove),
    the simplest way to remove GtkMenuBar (in MainMenu.Handle)
    from window_vbox is to simply do gtk_widget_destroy on MainMenu.Handle.
    This will take care of removing MainMenu.Handle appropriately. }
  gtk_widget_destroy(GTK_WIDGET(MainMenu.Handle));

  MainMenu.ClearHandles;
end;

{ GTK >= 2.16 already includes "gtk_menu_item_set_label":
  http://library.gnome.org/devel/gtk/2.21/GtkMenuItem.html#gtk-menu-item-set-label
  But we cannot use GTK 2.16 (e.g. Debian stable still only has
  GTK 2.12 on 2010-05).

  So we implement it ourselves, following ideas from
  http://mail.gnome.org/archives/gtk-app-devel-list/2003-October/msg00171.html
  http://mail.gnome.org/archives/gtk-app-devel-list/2003-October/msg00180.html }
procedure gtk_menu_item_set_label(EntryGtk: PGtkMenuItem; Caption: PChar);
var
  LabelGtk: PGtkWidget;
begin
  LabelGtk := gtk_bin_get_child(PGtkBin(EntryGtk));
  if (LabelGtk <> nil) and GTK_IS_LABEL(LabelGtk) then
    gtk_label_set_text_with_mnemonic(PGtkLabel(LabelGtk), Caption);
end;

{ Extract PGtkWidget corresponding to given Entry.
  Returns @false (and does log warning, and sets EntryGtk to nil) if failed. }
function CheckEntryGtk(Entry: TMenuEntryWithCaption; out EntryGtk: PGtkWidget): boolean;
begin
  EntryGtk := PGtkWidget(Entry.Handle);
  Result := EntryGtk <> nil;
  if (not Result) and
     ((Entry.CorrectParentWindow = nil) or Entry.CorrectParentWindow.MainMenuVisible) then
    WritelnWarning('Menu', 'Menu entry "' + Entry.Caption + '" has Handle = nil (this means that corresponding GTK menu item object was not created, but it should)');
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
var
  EntryGtk: PGtkWidget;
begin
  if not CheckEntryGtk(Entry, EntryGtk) then Exit;

  { MainMenu is a little special: it's Caption is ignored,
    and it's not really a GtkMenuItem so gtk_menu_item_set_label call
    would be invalid. }
  if Entry <> MainMenu then
    gtk_menu_item_set_label(PGtkMenuItem(EntryGtk), PChar(Entry.Caption));
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
var
  EntryGtk: PGtkWidget;
begin
  if not CheckEntryGtk(Entry, EntryGtk) then Exit;

  gtk_widget_set_sensitive(PGtkWidget(EntryGtk), Entry.Enabled);
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
var
  EntryGtk: PGtkWidget;
begin
  if not CheckEntryGtk(Entry, EntryGtk) then Exit;

  { Use MenuItemBlock, otherwise gtk_check_menu_item_set_active will
    call signal_menu_item_activate (which is pointless, and breaks
    assertion there). }
  MenuItemBlock(PGtkMenuItem(EntryGtk), Entry);
  try
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(EntryGtk), Entry.Checked);
  finally
    MenuItemUnblock(PGtkMenuItem(EntryGtk), Entry);
  end;
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;
