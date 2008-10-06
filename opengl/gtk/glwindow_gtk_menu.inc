{
  Copyright 2004-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

function TGLWindow.RedirectKeyDownToMenuCommand: boolean;
begin
 Result := false;
end;

procedure signal_menu_item_activate(AMenuItemGtk: PGtkMenuItem;
  data: gpointer); cdecl; forward;

{$ifdef GLWINDOW_GTK_2}
procedure gtk_check_menu_item_set_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem; draw_as_radio:gboolean);
  cdecl; external gtklib;

function gtk_check_menu_item_get_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem):gboolean;
  cdecl; external gtklib;
{$endif}

function signal_menu_bar_activation(AGtkMenuBar: PGtkMenuBar;
  Event: PGdkEventButton; UserData: gpointer): gboolean; cdecl;
var
  Glwin: TGLWindow absolute UserData;
begin
  { For reasons of using MenuRecreateForbidden, see signal_menu_item_select
    comments. }
  Inc(Glwin.MenuRecreateForbidden);
  try
    Glwin.ReleaseAllKeysAndMouse;
  finally
    Dec(Glwin.MenuRecreateForbidden);
  end;

  Result := false;
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
  message will cause e.g. SetStandardGLWindowState. For example,
  this happens in view3dscene when KeyUp may produce message to KeySensor about
  key release, and this may produce ChangedAll on VRML scene, which produces
  ProgressGL run...

  MenuRecreateForbidden is not an ideal solution, menu should be eventually
  recreated at some later point, TODO... For now this works Ok,
  as SetStandardGLWindowState only changes Enabled to false and later back
  to true, so actually recreating menu is not needed. }

function signal_menu_item_select(AItem: PGtkItem;
  UserData: gpointer): gboolean; cdecl;
var
  Glwin: TGLWindow absolute UserData;
begin
  Inc(Glwin.MenuRecreateForbidden);
  try
    Glwin.ReleaseAllKeysAndMouse;
  finally
    Dec(Glwin.MenuRecreateForbidden);
  end;

  Result := false;
end;

{ Block signal_menu_item_activate on this menu item.
  Useful inside signal_menu_item_activate, see there for comments.

  Note: MenuItemBlock/Unblock were changed in GLWINDOW_GTK_2
  to use g_signal_handlers_block/unblock_by_func instead of
  gtk_signal_handler_block/unblock_by_func. Although it should be
  perfectly correct to use gtk_signal_handler_xxx functions still in GTK_2,
  this caused some warnings printed by gtk:
    after doing gtk_signal_handler_block_by_func,
    next unblocking of this signal and any subsequent blocking and unblocking
    were writing
      (view3dscene:4151): Gtk-WARNING **: unable to find signal handler for
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
 {$ifdef GLWINDOW_GTK_2}
 g_signal_handlers_block_by_func
 {$else}
 gtk_signal_handler_block_by_func
 {$endif}
 ( GTK_OBJECT(MenuItemGtk),
   {$ifndef GLWINDOW_GTK_2} TGtkSignalFunc {$endif} (@signal_menu_item_activate),
   Pointer(PtrUInt(MenuItem.SmallId)));
end;

{ Unblock signal_menu_item_activate from this menu item.
  Useful inside signal_menu_item_activate, see there for comments. }
procedure MenuItemUnblock(MenuItemGtk: PGtkMenuItem; MenuItem: TMenuItem);
begin
 {$ifdef GLWINDOW_GTK_2}
 g_signal_handlers_unblock_by_func
 {$else}
 gtk_signal_handler_unblock_by_func
 {$endif}
 ( GTK_OBJECT(MenuItemGtk),
   {$ifndef GLWINDOW_GTK_2} TGtkSignalFunc {$endif} (@signal_menu_item_activate),
   Pointer(PtrUInt(MenuItem.SmallId)));
end;

procedure TGLWindow.init_window_menu_bar;

  { This tells GTK that key shortcut to Item is as specified in
    Key and CharKey. Key and Charkey are taken from TMenuItem.Key/CharKey
    properties. }
  procedure InstallGtkAccelerator(Item: PGtkMenuItem; Key: TKey; CharKey: char);
  var gdk_keyval: GUint;
      modifier_type: TGdkModifierType;
  begin
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
        constants K_BackSpace, K_Tab, K_Enter).
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
       K_BackSpace: gdk_keyval := GDK_KEY_BackSpace;
       K_Tab: gdk_keyval := GDK_KEY_Tab;
       K_Enter: gdk_keyval := GDK_KEY_Return;

       { This is not proper translation: K_Xxx constants do not make
         difference between left/right Ctrl/Shift/Alt,
         while GDK_KEY_Xxx constants do.

         Some solution would be to make two accelerators for one
         menu item -- TODO when this will be useful. }
       K_Shift: gdk_keyval := GDK_KEY_Shift_L;
       K_Ctrl: gdk_keyval := GDK_KEY_Control_L;
       K_Alt: gdk_keyval := GDK_KEY_Alt_L;

       K_Escape: gdk_keyval := GDK_KEY_Escape;
       K_Space: gdk_keyval := GDK_KEY_Space;
       K_PageUp: gdk_keyval := GDK_KEY_Page_Up;
       K_PageDown: gdk_keyval := GDK_KEY_Page_Down;
       K_End: gdk_keyval := GDK_KEY_End;
       K_Home: gdk_keyval := GDK_KEY_Home;
       K_Left: gdk_keyval := GDK_KEY_Left;
       K_Up: gdk_keyval := GDK_KEY_Up;
       K_Right: gdk_keyval := GDK_KEY_Right;
       K_Down: gdk_keyval := GDK_KEY_Down;
       K_Insert: gdk_keyval := GDK_KEY_Insert;
       K_Delete: gdk_keyval := GDK_KEY_Delete;

       K_Numpad_Plus: gdk_keyval := GDK_KEY_Plus;
       K_Numpad_Minus: gdk_keyval := GDK_KEY_Minus;

       K_0 .. K_9: gdk_keyval := GDK_KEY_0 + Key - K_0;
       K_A .. K_Z: gdk_keyval := GDK_KEY_A + Key - K_A;

       K_F1 : gdk_keyval := GDK_KEY_F1;
       K_F2 : gdk_keyval := GDK_KEY_F2;
       K_F3 : gdk_keyval := GDK_KEY_F3;
       K_F4 : gdk_keyval := GDK_KEY_F4;
       K_F5 : gdk_keyval := GDK_KEY_F5;
       K_F6 : gdk_keyval := GDK_KEY_F6;
       K_F7 : gdk_keyval := GDK_KEY_F7;
       K_F8 : gdk_keyval := GDK_KEY_F8;
       K_F9 : gdk_keyval := GDK_KEY_F9;
       K_F10: gdk_keyval := GDK_KEY_F10;
       K_F11: gdk_keyval := GDK_KEY_F11;
       K_F12: gdk_keyval := GDK_KEY_F12;

       K_Comma: gdk_keyval := GDK_KEY_Comma;
       K_Period: gdk_keyval := GDK_KEY_Period;
       else Exit;
      end;
   end;

   gtk_widget_add_accelerator(PGtkWidget(Item), 'activate', window_accel_group,
     gdk_keyval, modifier_type, GTK_ACCEL_VISIBLE);
  end;

  { Assuming MainMenu <> nil, builds appropriate GtkMenuBar.
    Signals signal_menu_item_activate and signal_menu_bar_activation are
    registered. Everything that should be shown is shown.
    You just have to insert resulting object in your window. }
  function get_main_menu: PGtkMenuBar;

    function MenuEntryToGtkMenuItem(Entry: TMenuEntry): PGtkMenuItem;

      function NewGtkMenu(): PGtkMenu;
      { Returns fresh gtk_menu_new() (only with standard PGtkTearoffMenuItem) }
      var Tearoff: PGtkWidget;
      begin
       Result := PGtkMenu(gtk_menu_new());
       { Do not show ! No "gtk_widget_show(GTK_WIDGET(Result));",
         you do not want to show GtkMenu (it will be automatically
         shown when user selects it with a mouse click etc.)  }

       Tearoff := gtk_tearoff_menu_item_new();
       gtk_widget_show(Tearoff);
       gtk_menu_append(
         {$ifdef GLWINDOW_GTK_2} GTK_WIDGET {$endif} (Result),
         Tearoff);
      end;

      {$ifdef GLWINDOW_GTK_1}
      function gtk_check_menu_item_new_with_mnemonic(Caption: PChar): PGtkWidget;
      begin
       Result := gtk_check_menu_item_new_with_label(
         PChar(SRemoveMnemonics(Caption)));
      end;

      function gtk_menu_item_new_with_mnemonic(Caption: PChar): PGtkWidget;
      begin
       Result := gtk_menu_item_new_with_label(
         PChar(SRemoveMnemonics(Caption)));
      end;
      {$endif}

    var SubMenu: PGtkMenu;
        i: Integer;
    begin
     if Entry is TMenuItem then
     begin
      if Entry is TMenuItemChecked then
      begin
       Result := PGtkMenuItem(gtk_check_menu_item_new_with_mnemonic(
         PChar(TMenuItem(Entry).Caption)));
       gtk_check_menu_item_set_active(PGtkCheckMenuItem(Result),
         TMenuItemChecked(Entry).Checked);
       gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(Result), true);

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
       {$ifdef GLWINDOW_GTK_2}
       gtk_check_menu_item_set_draw_as_radio(
         PGtkCheckMenuItem(Result), Entry is TMenuItemRadio);
       {$endif}
      end else
       Result := PGtkMenuItem(gtk_menu_item_new_with_mnemonic(
         PChar(TMenuItem(Entry).Caption)));

      gtk_object_set_user_data(GTK_OBJECT(Result), Pointer(Self));
      gtk_signal_connect(GTK_OBJECT(Result), 'activate',
        GTK_SIGNAL_FUNC(@signal_menu_item_activate),
        Pointer(PtrUInt(TMenuItem(Entry).SmallId)));

      InstallGtkAccelerator(Result,
        TMenuItem(Entry).Key, TMenuItem(Entry).CharKey);
     end else
     if Entry is TMenuSeparator then
     begin
      { gtk_menu_item_new() creates a separator menu item, this can be found
        in GTK FAQ. }
      Result := PGtkMenuItem(gtk_menu_item_new());
     end else
     if Entry is TMenu then
     begin
      Result := PGtkMenuItem(gtk_menu_item_new_with_mnemonic(
        PChar(TMenu(Entry).Caption)));

      SubMenu := NewGtkMenu();

      for i := 0 to TMenu(Entry).EntriesCount - 1 do
       gtk_menu_append(
         {$ifdef GLWINDOW_GTK_2} GTK_WIDGET {$endif} (SubMenu),
         GTK_WIDGET( MenuEntryToGtkMenuItem(TMenu(Entry).Entries[i]) ));

      gtk_menu_item_set_submenu(Result, GTK_WIDGET(SubMenu));
     end else
      raise EInternalError.Create('Not implemented TMenuEntry subclass');

     if Entry is TMenuEntryWithCaption then
     begin
       gtk_widget_set_sensitive(GTK_WIDGET(Result),
         TMenuEntryWithCaption(Entry).Enabled);
     end;

     gtk_signal_connect(GTK_OBJECT(Result), 'select',
       GTK_SIGNAL_FUNC(@signal_menu_item_select), Self);

     gtk_widget_show(GTK_WIDGET(Result));
    end;

  var i: Integer;
  begin
   Result := PGtkMenuBar(gtk_menu_bar_new());

   gtk_widget_set_sensitive(GTK_WIDGET(Result), MainMenu.Enabled);

   gtk_widget_show(GTK_WIDGET(Result));
   for i := 0 to MainMenu.EntriesCount - 1 do
    { Don't put TMenuSeparator in menu bar because that would look ugly. }
    if not (MainMenu.Entries[i] is TMenuSeparator) then
     gtk_menu_bar_append(
       {$ifdef GLWINDOW_GTK_2} GTK_WIDGET {$endif} (Result),
       GTK_WIDGET( MenuEntryToGtkMenuItem(MainMenu.Entries[i]) ));

   { How to catch the moment when the menu bar is opened/selected/clicked
     etc. by the user ? It should be catched when user enters menu by clicking
     on menu bar or pressing some menu key (like F10 in GTK 2).

     There is no way do this nicely in GTK 1 ?
     So I'm using threre very non-elegant soltuion to catch button-press-event.
     GTK 1 does not allow user to enter menu by F10 or some other key like that. }
   gtk_signal_connect(GTK_OBJECT(Result), 'button-press-event',
     GTK_SIGNAL_FUNC(@signal_menu_bar_activation), Self);
  end;

begin
 window_menu_bar := get_main_menu;
 gtk_box_pack_start(PGtkBox(window_vbox), GTK_WIDGET(window_menu_bar),
   false, false, 0);
end;
