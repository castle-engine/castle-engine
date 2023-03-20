{
  Copyright 2022-2023 Michalis Kamburelis.

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

type
  { Abstact class to wrap capabilities of TEdit and (editable) TComboBox. }
  TEditBox = class
  protected
    function GetSelLength: Integer; virtual; abstract;
    function GetSelStart: Integer; virtual; abstract;
    function GetSelText: String; virtual; abstract;
    function GetText: String; virtual; abstract;
    procedure SetSelLength(const AValue: Integer); virtual; abstract;
    procedure SetSelStart(const AValue: Integer); virtual; abstract;
    procedure SetSelText(const AValue: String); virtual; abstract;
    procedure SetText(const AValue: String); virtual; abstract;

    property Text: String read GetText write SetText;
    property SelText: String read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;

    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure Undo; virtual;
    function ReadOnly: Boolean; virtual; abstract;
  public
    { Process the given key to realize default editing action on it.
      Processes only a subset of keys for which FormProject has possibly a menu shortcut. }
    procedure ProcessKey(var Key: Word; const Shift: TShiftState);
  end;

  { Wrapper around TEdit. }
  TEditBoxForEdit = class(TEditBox)
  private
    E: TEdit;
  protected
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: String; override;
    function GetText: String; override;
    procedure SetSelLength(const AValue: Integer); override;
    procedure SetSelStart(const AValue: Integer); override;
    procedure SetSelText(const AValue: String); override;
    procedure SetText(const AValue: String); override;

    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Undo; override;
    function ReadOnly: Boolean; override;
  public
    constructor Create(const AEdit: TEdit);
  end;

  { Wrapper around TComboBox. }
  TEditBoxForComboBox = class(TEditBox)
  private
    E: TComboBox;
  protected
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: String; override;
    function GetText: String; override;
    procedure SetSelLength(const AValue: Integer); override;
    procedure SetSelStart(const AValue: Integer); override;
    procedure SetSelText(const AValue: String); override;
    procedure SetText(const AValue: String); override;

    function ReadOnly: Boolean; override;
  public
    constructor Create(const AEdit: TComboBox);
  end;

{ Process a key pressed with given ActiveControl being focused.

  If this was a key that typically interacts with TEdit controls,
  then perform the typical TEdit action directly
  (do not allow menu item to catch it) and set Key to 0.

  Does something only for keys that are used as some menu shortcut
  in CGE editor, and are also used to edit text. Like Delete or Ctrl+Z
  or 1. }
procedure ProcessKeyToPerformEdit(const ActiveControl: TComponent;
  var Key: Word; const Shift: TShiftState);

implementation

uses LCLType, LCLVersion, StrUtils, Clipbrd,
  CastleStringUtils, CastleUnicode;

{ TEditBox ------------------------------------------------------------------- }

procedure TEditBox.ProcessKey(var Key: Word; const Shift: TShiftState);

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
    if ReadOnly then
      Beep
    else
      // TODO: in Overwrite mode, with SelText = '', delete next char first
      SelText := C;
  end;

var
  SavedSelStart: Integer;
begin
  case Key of
    VK_HOME:
      begin
        if Shift = [ssShift] then
        begin
          SavedSelStart := SelStart;
          SelStart := 0;
          SelLength := SavedSelStart;
        end else
          SelStart := 0
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
          SelLength := StringLength(Text) - SelStart
        else
          SelStart := StringLength(Text);
      end;
    {.$endif}
    VK_0..VK_9:
      begin
        if ssShift in Shift then
          Exit; // resign from special handling, to allow to type !@#$... using alphanumeric keys
        InsertChar(Chr(Ord('0') + Key - VK_0));
      end;
    VK_NUMPAD0..VK_NUMPAD9:
      begin
        if Shift <> [] then
          Exit; // resign from special handling
        InsertChar(Chr(Ord('0') + Key - VK_NUMPAD0));
      end;
    VK_F: InsertChar(IfThen(LettersUpCase, 'F', 'f'));
    VK_Z:
      if Shift = [ssCtrl] then
        Undo
      else
        Exit; // resign from special handling
      //if Shift = [ssCtrl, ssShift] then
      //  Redo
      //else
    VK_C:
      if Shift = [ssCtrl] then
        CopyToClipboard
      else
        Exit; // resign from special handling
    VK_V:
      if Shift = [ssCtrl] then
      begin
        if ReadOnly then
          Beep
        else
          PasteFromClipboard;
      end else
        Exit; // resign from special handling
    VK_X:
      if Shift = [ssCtrl] then
      begin
        if ReadOnly then
          Beep
        else
          CutToClipboard
      end else
        Exit; // resign from special handling
    VK_DELETE:
      if ReadOnly then
        Beep
      else
      begin
        if SelText <> '' then
          SelText := ''
        else
        begin
          SavedSelStart := SelStart;
          Text :=
            UTF8Copy(Text, 1, SelStart) +
            UTF8SEnding(Text, SelStart + 2);
          SelStart := SavedSelStart;
        end;
      end;
    else
      Exit; // resign from special handling
  end;

  Key := 0; // prevent key reaching normal menu/action event
end;

procedure TEditBox.CopyToClipboard;
begin
  if SelText <> '' then
    Clipboard.AsText := SelText;
end;

procedure TEditBox.CutToClipboard;
begin
  CopyToClipboard;
  SelText := '';
end;

procedure TEditBox.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    SelText := Clipboard.AsText;
end;

procedure TEditBox.Undo;
begin
  // do nothing
end;

{ TEditBoxForEdit ------------------------------------------------------------ }

function TEditBoxForEdit.GetSelLength: Integer;
begin
  Result := E.SelLength;
end;

function TEditBoxForEdit.GetSelStart: Integer;
begin
  Result := E.SelStart;
end;

function TEditBoxForEdit.GetSelText: String;
begin
  Result := E.SelText;
end;

function TEditBoxForEdit.GetText: String;
begin
  Result := E.Text;
end;

procedure TEditBoxForEdit.SetSelLength(const AValue: Integer);
begin
  E.SelLength := AValue;
end;

procedure TEditBoxForEdit.SetSelStart(const AValue: Integer);
begin
  E.SelStart := AValue;
end;

procedure TEditBoxForEdit.SetSelText(const AValue: String);
begin
  E.SelText := AValue;
end;

procedure TEditBoxForEdit.SetText(const AValue: String);
begin
  E.Text := AValue;
end;

constructor TEditBoxForEdit.Create(const AEdit: TEdit);
begin
  inherited Create;
  E := AEdit;
end;

procedure TEditBoxForEdit.CopyToClipboard;
begin
  E.CopyToClipboard;
end;

procedure TEditBoxForEdit.CutToClipboard;
begin
  E.CutToClipboard;
end;

procedure TEditBoxForEdit.PasteFromClipboard;
begin
  E.PasteFromClipboard;
end;

procedure TEditBoxForEdit.Undo;
begin
  E.Undo;
end;

function TEditBoxForEdit.ReadOnly: Boolean;
begin
  Result := E.ReadOnly;
end;

{ TEditBoxForComboBox ------------------------------------------------------------ }

function TEditBoxForComboBox.GetSelLength: Integer;
begin
  Result := E.SelLength;
end;

function TEditBoxForComboBox.GetSelStart: Integer;
begin
  Result := E.SelStart;
end;

function TEditBoxForComboBox.GetSelText: String;
begin
  Result := E.SelText;
end;

function TEditBoxForComboBox.GetText: String;
begin
  Result := E.Text;
end;

procedure TEditBoxForComboBox.SetSelLength(const AValue: Integer);
begin
  E.SelLength := AValue;
end;

procedure TEditBoxForComboBox.SetSelStart(const AValue: Integer);
begin
  E.SelStart := AValue;
end;

procedure TEditBoxForComboBox.SetSelText(const AValue: String);
begin
  E.SelText := AValue;
  { Without these lines, by default setting SelText keeps it selected,
    unlike on TEdit. }
  E.SelStart := E.SelStart + StringLength(AValue);
  E.SelLength := 0;
end;

procedure TEditBoxForComboBox.SetText(const AValue: String);
begin
  E.Text := AValue;
end;

constructor TEditBoxForComboBox.Create(const AEdit: TComboBox);
begin
  inherited Create;
  E := AEdit;
end;

//procedure TEditBoxForComboBox.CopyToClipboard;
//begin
//  E.CopyToClipboard;
//end;
//
//procedure TEditBoxForComboBox.CutToClipboard;
//begin
//  E.CutToClipboard;
//end;
//
//procedure TEditBoxForComboBox.PasteFromClipboard;
//begin
//  E.PasteFromClipboard;
//end;
//
//procedure TEditBoxForComboBox.Undo;
//begin
//  E.Undo;
//end;

function TEditBoxForComboBox.ReadOnly: Boolean;
begin
  Result := E.ReadOnly;
end;

{ global --------------------------------------------------------------------- }

var
  CachedForComboBox: TEditBoxForComboBox;
  CachedForEdit: TEditBoxForEdit;

procedure ProcessKeyToPerformEdit(const ActiveControl: TComponent; var Key: Word; const Shift: TShiftState);

  {$if LCL_FULLVERSION >= 2020000}
    {$define HAS_COMBO_EDIT_BOX}
  {$endif}

  {$ifndef HAS_COMBO_EDIT_BOX}
  // Adjusted from TComboBoxStyleHelper.HasEditBox in latest LCL.
  function ComboHasEditBox(const Style: TComboBoxStyle): Boolean;
  const
    ArrHasEditBox: array[TComboBoxStyle] of Boolean = (
      True,  // csDropDown
      True,  // csSimple
      False, // csDropDownList
      False, // csOwnerDrawFixed
      False, // csOwnerDrawVariable
      True,  // csOwnerDrawEditableFixed
      True   // csOwnerDrawEditableVariable
    );
  begin
    Result := ArrHasEditBox[Style];
  end;
  {$endif}

var
  EditBox: TEditBox;
begin
  if (ActiveControl is TComboBox) and
     {$ifdef HAS_COMBO_EDIT_BOX}
     (TComboBox(ActiveControl).Style.HasEditBox)
     {$else}
     ComboHasEditBox(TComboBox(ActiveControl).Style)
     {$endif}
     then
  begin
    { Instead of creating new TEditBoxForComboBox instance at each ProcessKeyToPerformEdit
      call, we just reuse one cached instance. }
    if CachedForComboBox = nil then
      CachedForComboBox := TEditBoxForComboBox.Create(nil);
    CachedForComboBox.E := TComboBox(ActiveControl);
    EditBox := CachedForComboBox;
  end else

  if ActiveControl is TEdit then
  begin
    if CachedForEdit = nil then
      CachedForEdit := TEditBoxForEdit.Create(nil);
    CachedForEdit.E := TEdit(ActiveControl);
    EditBox := CachedForEdit;
  end else
    EditBox := nil;

  if EditBox <> nil then
    EditBox.ProcessKey(Key, Shift);
end;

finalization
  FreeAndNil(CachedForComboBox);
  FreeAndNil(CachedForEdit);
end.
