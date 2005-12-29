{
  Copyright 2004-2005 Michalis Kamburelis.

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

{ @abstract(Helper unit for WINAPI GLWindow implementation.)

  It converts menu structure from classes in GLWindowMenu to WinAPI menu
  (HMenu handle).

  Let me elaborate here a little about how menu shortcuts are handled
  when GLWindow is implemented on top of WinAPI:

  A. 1st thing to realize is that WinAPI is stupid and AFAIK you simply
     can't explicitly say to WinAPI : this menu item's key shortcut
     is xxx.

     Instead you have to do two things:
     1. When creating menu item, specify it's caption as
        "Real caption" + CharTab + "textual description of menu's key shortcut"

     2. Create accelerator table (e.g. with CreateAcceleratorTable)
        that specifies associations "key shortcuts" -> "command numbers to
        be returned by WM_COMMAND".

        Then you have to use that accelerator table in your message loop
        using TranslateAccelerators. This will actually translate WM_KEYDOWN
        messages to WM_COMMAND.

     What's stupid with this ?
     It's stupid because there actually need not be any connection
     between what "textual description of menu's key shortcut"
     you gave in 1 and what actual key shortcuts you associate
     with which commands in 2.
     You can easily specify no or inccorrect textual key descriptions for
     some menu items. Moreover you can create "textual descriptions"
     in non-standard way, e.g. some programs name Ctrl + O shortcut
     as 'Ctrl+o' and some as 'Ctrl+O'.

  B. OK, so what I'm doing with it ?

     Well, in this particular case stupidity of WinAPI means that I have
     less work. That's because I already implemented mechanism to
     create textual key descriptions in TMenuItem.KeyString
     and I already created mechanism to translate key downs to
     appropriately translate key presses to menu items.
     I had to do this because my GLWindow unit must at least basically
     work when implemented on top of glut or Xlib, where I don't have
     a concept of a "menu item with key shortcut" available.

     So what I'm doing ?
     I'm doing A.1 in this unit using my TMenuItem.KeyString, and
     I'm ignoring A.2 (i.e. I'm doing equivalent things myself in GLWindow unit,
     not using any WinAPI accelerator tables).
  }

unit GLWindowWinAPIMenu;

interface

uses GLWindow, Windows;

{ Convert Menu structure to a WinAPI menu. Resulting menu is menu bar
  (obtained with CreateMenu) if MenuBar = true, else it's popup menu
  (obtained with CreatePopupMenu).

  Each TMenuItem will be added with identifier taken from it's SmallId.

  If MenuBar then TMenuSeparator entries in Menu.Entries[] are ignored
  (WinAPI toplevel menu bar cannot have a separator, that's quite sensible
  actually). Of course separators at lower depths are not ignored. }
function WindowsMenuFromGLWindowMenu(Menu: TMenu; MenuBar: boolean): HMenu;

implementation

uses KambiUtils;

function WindowsMenuFromGLWindowMenu(Menu: TMenu; MenuBar: boolean): HMenu;

  function SMnemonicsToWin(const S: string): string;
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

  procedure AppendGLMenu(Menu: TMenu);
  begin
   { I'm casting WindowsMenuFromGLWindowMenu result (:HMenu)
     to UINT to avoid range check errors }
   KambiOSCheck( AppendMenu(Result, MF_STRING or MF_POPUP,
     UINT(WindowsMenuFromGLWindowMenu(Menu, false)),
     PChar(SMnemonicsToWin(Menu.Caption))) );
  end;

  procedure AppendGLMenuItem(MenuItem: TMenuItem);
  var Flags: UInt;
      KeyStr, S: string;
  begin
   Flags := MF_STRING;

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

   KambiOSCheck( AppendMenu(Result, Flags, MenuItem.SmallId, PChar(S)) );
  end;

  procedure AppendGLMenuSeparator;
  begin
   KambiOSCheck( AppendMenu(Result, MF_SEPARATOR, 0, nil) );
  end;

var M: TMenuEntry;
    i: Integer;
begin
 if MenuBar then
 begin
  Result := CreateMenu;
  KambiOSCheck( Result <> 0, 'CreateMenu');
 end else
 begin
  Result := CreatePopupMenu;
  KambiOSCheck( Result <> 0, 'CreatePopupMenu');
 end;

 for i := 0 to Menu.EntriesCount-1 do
 begin
  M := Menu[i];
  if M is TMenuItem then
   AppendGLMenuItem(TMenuItem(M)) else
  if M is TMenuSeparator then
  begin
   if not MenuBar then AppendGLMenuSeparator;
  end else
  if M is TMenu then
   AppendGLMenu(TMenu(M)) else
   raise EInternalError.Create('Not implemented TMenuEntry subclass');
 end;
end;

end.