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

{ This is a hack used in TProjectForm.FormKeyDown to
  - Have some menu items with simple shortcuts, like Home, F, Ctrl+Z
    (and *visible* in menu with these shortcuts, to be discoverable)
  - but still allow user to interact with text input (like TEdit)
    using Home, F, Ctrl+Z etc.

  The intuitive result should be:
  - using Home, F, Ctrl+Z etc. when TEdit is focused, makes normal text-editing
    operation
  - only if no text-editing is focused,
    it goes to the specific custom action we wanted to assign to it.

  Rejected workarounds:
  - use only shortcuts that avoid common text-editing.
    Like Ctrl+Home, Ctrl+F, Ctrl+Alt+Z.
    Rejected because these are important shortcuts.
    For ease of use, for consistency with other software --
    we want to keep them easy.
  - use simple shortcuts, but not set them as menu item shortcuts.
    Just dispatch them yourself if e.g. TCastleControl is focused.
    Rejected: then UI sucks, we cannot show shortcuts in menu in standard way.

  This workaround depends on Lazarus behaviors (tested on WinAPI, GTK, Cocoa
  backends):
  - Key being used to activate menu item is still send to TForm1.FormKeyDown
    when form has KeyPreview
  - We can resign from "normal" key handling (which means running menu item
    for these special keys) by assigning Key := 0
  - Our 2 important text-editing states are TEdit and editing in TTreeView,
    the latter is luckily just an internal TEdit too.
  - So we just interpret and implement TEdit operations manually.

  Note: This is still ugly and inherently unreliable workaround, because we
  interpret what some special built-in keys should do. So we need to
  capture all such keys, and implement same logic...
  E.g. pressing "f" key, with and without Shift, with and without CapsLock,
  should be consistent with system-handled "g" key.
  - What happens if we don't include special handling for some key,
    but we should? In the worst case it will not work with edit boxes,
    and always activate menu command.
  - What happens if we included unnecessarily some key in this treatment?
    Everything should be OK, if only we simulate default behavior OK.

  Tested: It is necessary, and good workaround,
  - (FPC 3.2.2, LCL 2.2) on Windows with WinAPI widgetset.
  - (FPC 3.2.2, LCL 2.2) on Linux with GTK widgetset.
  - (FPC 3.2.2, LCL 2.2) on macOS with Cocoa widgetset.

  See https://github.com/michaliskambi/lcl-test-menu-special-shortcuts
  for separated LCL application testing this.

  Note: TTreeView also handles Home / End.
  Strangely, it behaves like we want out-of-the-box:
  it intercepts Home / End when focused, regardless of if some menu item
  has such shortcut.
  Tested this is true on:
  - (FPC 3.2.2, LCL 2.2) on Windows with WinAPI widgetset.
  - (FPC 3.2.2, LCL 2.2) on Linux with GTK widgetset.
  - (FPC 3.2.2, LCL 2.2) on macOS with Cocoa widgetset.
}
unit CastleLclEditHack;

interface

uses {$ifdef MSWINDOWS} { for GetKeyState } Windows, {$endif}
  SysUtils, Classes, StdCtrls;

{ Process the given key to realize default TEdit action on it.
  Processes only a subset of keys for which FormProject has possibly a menu shortcut. }
procedure ProcessEditKey(const E: TEdit; var Key: Word; const Shift: TShiftState);

implementation

uses LCLType, StrUtils, CastleStringUtils;

procedure ProcessEditKey(const E: TEdit; var Key: Word; const Shift: TShiftState);

  { When pressing a letter, should we make it upper?
    It sucks, but in this workaround we cannot depend on OnKeyPress reaching us
    (it doesn't reach us when menu item intercepts the key)
    so we need to calculate key->char here. }
  function LettersUpCase: Boolean;
  begin
    Result := false;
    // TODO: This is Windows-only.
    {$ifdef MSWINDOWS}
    { See https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getkeystate .
      Read low-order bit of VK_CAPITAL (caps lock) state. }
    if (GetKeyState(VK_CAPITAL) and 1) <> 0 then
      Result := true;
    {$endif}
    if ssShift in Shift then
      Result := not Result;
  end;

  procedure InsertChar(const C: String);
  begin
    if E.ReadOnly then
      Beep
    else
      // TODO: in Overwrite mode, with SelText = '', delete next char first
      E.SelText := C;
  end;

var
  SavedSelStart: Integer;
begin
  case Key of
    VK_HOME:
      begin
        if Shift = [ssShift] then
        begin
          SavedSelStart := E.SelStart;
          E.SelStart := 0;
          E.SelLength := SavedSelStart;
        end else
          E.SelStart := 0
      end;
    {.$ifdef LCLCocoa}
    { If user didn't adjust Home/End system-wide, then actually
      by default Home/End do nothing in TEdit.
      So we could disable Home...
      but it seems more useful to make Home/End just work in CGE TEdit.

      For easier testing, we enable our End handling on all platforms. }
    VK_END:
      begin
        if Shift = [ssShift] then
          E.SelLength := Length(E.Text) - E.SelStart
        else
          E.SelStart := Length(E.Text);
      end;
    {.$endif}
    VK_0..VK_9: InsertChar(Chr(Ord('0') + Key - VK_0));
    VK_F: InsertChar(IfThen(LettersUpCase, 'F', 'f'));
    VK_Z:
      if Shift = [ssCtrl] then
        E.Undo
      else
        Exit; // resign from special handling
      //if Shift = [ssCtrl, ssShift] then
      //  E.Redo
      //else
    VK_C:
      if Shift = [ssCtrl] then
        E.CopyToClipboard
      else
        Exit; // resign from special handling
    VK_V:
      if Shift = [ssCtrl] then
      begin
        if E.ReadOnly then
          Beep
        else
          E.PasteFromClipboard;
      end else
        Exit; // resign from special handling
    VK_X:
      if Shift = [ssCtrl] then
      begin
        if E.ReadOnly then
          Beep
        else
          E.CutToClipboard
      end else
        Exit; // resign from special handling
    VK_DELETE:
      if E.ReadOnly then
        Beep
      else
      begin
        if E.SelText <> '' then
          E.SelText := ''
        else
        begin
          SavedSelStart := E.SelStart;
          E.Text :=
            Copy(E.Text, 1, E.SelStart) +
            SEnding(E.Text, E.SelStart + 2);
          E.SelStart := SavedSelStart;
        end;
      end;
    else
      Exit; // resign from special handling
  end;

  Key := 0; // prevent key reaching normal menu/action event
end;

end.
