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

{ @abstract(Helper for CastleWindow implementation of menu-related things
  under WinAPI.)

  It converts menu structure from classes in castlewindowmenu.inc to WinAPI menu
  (HMenu handle).

  Let me elaborate here a little about how menu shortcuts are handled
  when CastleWindow is implemented on top of WinAPI:

  @orderedList(
    @item(
      1st thing to realize is that WinAPI is stupid and AFAIK you simply
      can't explicitly say to WinAPI : this menu item's key shortcut
      is xxx.

      Instead you have to do two things:

      @orderedList(
        @item(
          When creating menu item, specify it's caption as
          "Real caption" + CharTab + "textual description of menu's key shortcut")

        @item(
          Create accelerator table (e.g. with CreateAcceleratorTable)
          that specifies associations "key shortcuts" -> "command numbers to
          be returned by WM_COMMAND".

          Then you have to use that accelerator table in your message loop
          using TranslateAccelerators. This will actually translate WM_KEYDOWN
          messages to WM_COMMAND.)
      )

      What's stupid about this?
      It's stupid because there actually need not be any connection
      between what "textual description of menu's key shortcut"
      you gave in 1 and what actual key shortcuts you associate
      with which commands in 2.
      You can easily specify none or inccorrect textual key descriptions for
      some menu items. Moreover you can create "textual descriptions"
      in non-standard way, e.g. some programs name Ctrl + O shortcut
      as 'Ctrl+o' and some as 'Ctrl+O'.)

    @item(
      OK, so what I'm doing with it?

      Well, in this particular case stupidity of WinAPI means that I have
      less work. That's because I already implemented mechanism to
      create textual key descriptions in TMenuItem.KeyString
      and I already created mechanism to translate key downs to
      appropriately translate key presses to menu items.
      I had to do this because my CastleWindow unit must
      work when implemented on top of Xlib, where I don't have
      a concept of a "menu item with key shortcut" available.

      So what I'm doing?
      I'm doing 1.1 in this unit using my TMenuItem.KeyString, and
      I'm ignoring 1.2 (i.e. I'm doing equivalent things myself in CastleWindow unit,
      not using any WinAPI accelerator tables).)
  )
}

{ We use ParentAllowsEnabled trick to take into account main Menu.Enabled
  state. WinAPI doesn't allow the menu bar or popup to be just disabled,
  so instead we disable all direct children of main Manu
  if Menu.Enabled = false.

  In other words, when main menu creates it's children, it passes
  ParentAllowsEnabled = main Menu.Enabled.
  When other menu creates it's children, it passes
  ParentAllowsEnabled = @true. }
function MakeWinapiMenuCore(Menu: TMenu;
  MenuBar: boolean; ParentAllowsEnabled: boolean): HMenu;

  function SMnemonicsToWin(const S: string): string;
  var
    SPos, ResultPos: Integer;
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
          { '__' replace with '_' }
          Result[ResultPos] := '_';
          Inc(ResultPos);
          Inc(SPos, 2);
        end else
        begin
          { '_' (not starting '__') replace with '&' }
          Result[ResultPos] := '&';
          Inc(ResultPos);
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

  function EnabledFlag(Enabled: boolean): UInt;
  begin
    if Enabled then
      Result := MF_ENABLED else
      Result := MF_GRAYED;
  end;

  procedure AppendGLMenu(Menu: TMenu; ParentAllowsEnabled: boolean);
  begin
    { I'm casting MakeWinapiMenu result (:HMenu)
      to UINT to avoid range check errors }
    OSCheck( AppendMenu(Result,
      MF_STRING or MF_POPUP or EnabledFlag(Menu.Enabled and ParentAllowsEnabled),
      UINT(MakeWinapiMenuCore(Menu, false, true)),
      PChar(SMnemonicsToWin(Menu.Caption))) );
  end;

  procedure AppendGLMenuItem(MenuItem: TMenuItem; ParentAllowsEnabled: boolean);
  var
    Flags: UInt;
    KeyStr, S: string;
  begin
    Flags := MF_STRING or EnabledFlag(MenuItem.Enabled and ParentAllowsEnabled);

    { If I understand docs properly, MF_UNCHECKED is actually meaningless
      here as I don't use any customized bitmaps for menu check marks.
      But I wrote it for consistency. }
    if MenuItem is TMenuItemChecked then
    begin
      if TMenuItemChecked(MenuItem).Checked then
        Flags := Flags or MF_CHECKED else
        Flags := Flags or MF_UNCHECKED;
    end;

    { Don't use here MenuItem.CaptionWithKey.
      Instead use #9 to delimit key names.
      This way our key shortcuts will be drawn nicely. }
    S := SMnemonicsToWin(MenuItem.Caption);
    if MenuItem.KeyString(KeyStr) then
      S := S + #9 + KeyStr;

    OSCheck( AppendMenu(Result, Flags, MenuItem.SmallId, PChar(S)) );

    if (MenuItem is TMenuItemRadio) and TMenuItemRadio(MenuItem).Checked then
      OSCheck( CheckMenuRadioItem(Result,
        MenuItem.SmallId, MenuItem.SmallId, MenuItem.SmallId, MF_BYCOMMAND) );
  end;

  procedure AppendGLMenuSeparator;
  begin
    OSCheck( AppendMenu(Result, MF_SEPARATOR, 0, nil) );
  end;

var
  M: TMenuEntry;
  i: Integer;
begin
  if MenuBar then
  begin
    Result := CreateMenu;
    OSCheck( Result <> 0, 'CreateMenu');
  end else
  begin
    Result := CreatePopupMenu;
    OSCheck( Result <> 0, 'CreatePopupMenu');
  end;

  for i := 0 to Menu.Count - 1 do
  begin
    M := Menu[i];
    if M is TMenuItem then
      AppendGLMenuItem(TMenuItem(M), ParentAllowsEnabled) else
    if M is TMenuSeparator then
    begin
      if not MenuBar then AppendGLMenuSeparator;
    end else
    if M is TMenu then
      AppendGLMenu(TMenu(M), ParentAllowsEnabled) else
      raise EInternalError.Create('Not implemented TMenuEntry subclass');
  end;
end;

{ Convert Menu structure to a WinAPI menu. Resulting menu is menu bar
  (obtained with CreateMenu) if MenuBar = true, else it's popup menu
  (obtained with CreatePopupMenu).

  Each TMenuItem will be added with identifier taken from it's SmallId.

  If MenuBar then TMenuSeparator entries in Menu.Entries[] are ignored
  (WinAPI toplevel menu bar cannot have a separator, that's quite sensible
  actually). Of course separators at lower depths are not ignored. }
function MakeWinapiMenu(Menu: TMenu; MenuBar: boolean): HMenu;
begin
  Result := MakeWinapiMenuCore(Menu, MenuBar, Menu.Enabled);
end;
