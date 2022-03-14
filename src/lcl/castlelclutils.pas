{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for cooperation between LCL and "Castle Game Engine". }
unit CastleLCLUtils;

{$I castleconf.inc}

interface

uses Dialogs, Classes, Controls, LCLType, Graphics, EditBtn,
  CastleFileFilters, CastleKeysMouse, CastleVectors;

{ Convert file filters into LCL Dialog.Filter, Dialog.FilterIndex.
  Suitable for both open and save dialogs (TOpenDialog, TSaveDialog
  both descend from TFileDialog).

  Input filters are either given as a string FileFilters
  (encoded just like for TFileFilterList.AddFiltersFromString),
  or as TFileFilterList instance.

  Output filters are either written to LCLFilter, LCLFilterIndex
  variables, or set appropriate properties of given Dialog instance.

  When AllFields is false, then filters starting with "All " in the name,
  like "All files", "All images", are not included in the output.

  @groupBegin }
procedure FileFiltersToDialog(const FileFilters: string;
  Dialog: TFileDialog; const AllFields: boolean = true);
procedure FileFiltersToDialog(const FileFilters: string;
  const Edit: TFileNameEdit; const AllFields: boolean = true);
procedure FileFiltersToDialog(const FileFilters: string;
  out LCLFilter: string; out LCLFilterIndex: Integer; const AllFields: boolean = true);
procedure FileFiltersToDialog(FFList: TFileFilterList;
  Dialog: TFileDialog; const AllFields: boolean = true);
procedure FileFiltersToDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer; const AllFields: boolean = true);
{ @groupEnd }

{ Make each '&' inside string '&&', this way the string will not contain
  special '&x' sequences when used as a TMenuItem.Caption and such. }
function SQuoteLCLCaption(const S: string): string;

{ Deprecated names, use the identifiers without "Open" in new code.
  @deprecated
  @groupBegin }
procedure FileFiltersToOpenDialog(const FileFilters: string;
  Dialog: TFileDialog); deprecated;
procedure FileFiltersToOpenDialog(const FileFilters: string;
  out LCLFilter: string; out LCLFilterIndex: Integer); deprecated;
procedure FileFiltersToOpenDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer); deprecated;
{ @groupEnd }

{ Convert Key (Lazarus key code) to Castle Game Engine TKey.
  Returns keyNone if not possible. }
function KeyLCLToCastle(const Key: Word; const Shift: TShiftState): TKey;

{ Convert TKey and/or character code into Lazarus key code (VK_xxx)
  and shift state.
  Sets LazKey to VK_UNKNOWN (zero) when conversion not possible
  (or when Key = keyNone and KeyString = '').

  @groupBegin }
procedure KeyCastleToLCL(const Key: TKey; const KeyString: String;
  out LazKey: Word; out Shift: TShiftState);
procedure KeyCastleToLCL(const Key: TKey; KeyString: String;
  const Modifiers: TModifierKeys;
  out LazKey: Word; out Shift: TShiftState);
{ @groupEnd }

{ Convert Lazarus Controls.TMouseButton value to Castle Game Engine
  CastleKeysMouse.TCastleMouseButton.

  (By coincidence, my type name and values are the same as used by LCL;
  but beware --- the order of values in my type is different (buttonMiddle
  is in the middle in my type)). }
function MouseButtonLCLToCastle(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: TCastleMouseButton): boolean;

const
  CursorCastleToLCL: array [TMouseCursor] of TCursor =
  ( crDefault, // mcDefault
    crNone,    // mcNone
    crNone,    // mcForceNone

    crArrow,       // mcStandard
    crHourGlass,   // mcWait
    crIBeam,       // mcText
    crHandPoint,   // mcHand
    crSizeNS,      // mcResizeVertical
    crSizeWE,      // mcResizeHorizontal
    crSizeNW,      // mcResizeTopLeft
    crSizeN,       // mcResizeTop
    crSizeNE,      // mcResizeTopRight
    crSizeW,       // mcResizeLeft
    crSizeE,       // mcResizeRight
    crSizeSW,      // mcResizeBottomLeft
    crSizeS,       // mcResizeBottom
    crSizeSE       // mcResizeBottomRight
  );

function FilenameToURISafeUTF8(const FileName: string): string;
function URIToFilenameSafeUTF8(const URL: string): string;

{ Convert LCL color values to our colors (vectors). }
function ColorToVector3(const Color: TColor): TVector3;
function ColorToVector3Byte(const Color: TColor): TVector3Byte;

type
  TControlInputPressReleaseEvent = procedure (Sender: TObject; const Event: TInputPressRelease) of object;
  TControlInputMotionEvent = procedure (Sender: TObject; const Event: TInputMotion) of object;

  { Convert LCL OnKeyDown and OnUTF8KeyPress into a single CGE event OnPress.

    In VCL and LCL KeyPress (or UTF8KeyPress in LCL) and KeyDown
    are separate events. While in CGE they are one event,
    and passed as one event to TCastleUserInterface.KeyDown,
    because this is more comfortable to process (as it corresponds
    to one user action, and allows configurable TInputShortcut to work).
  }
  TLCLKeyPressHandler = class
  private
    FOnPress: TControlInputPressReleaseEvent;
    FUnfinishedKeyDown: Boolean;
    FUnfinishedKeyDownKey: Word;
    FUnfinishedKeyDownShift: TShiftState;
    FUnfinishedKeyPress: Boolean;
    FUnfinishedKeyPressKey: TUTF8Char;
  public
    { Call these events when corresponding Lazarus event
      (OnKeyDown, OnUTF8KeyPress) occurs.

      They may call OnPress in turn. }
    procedure KeyDown(const Key: Word; const Shift: TShiftState);
    procedure UTF8KeyPress(const UTF8Key: TUTF8Char);

    { Call this when Lazarus OnKeyUp event occurs.

      It may call OnPress in return.
      Receiving "key up" event from Lazarus may indicate that we have
      to do OnPress now.

      This matters for keys without corresponding String/Char representation
      (thus, without UTF8KeyPress call), like keyArrowLeft.
      If we have buffered to "send the OnPress for this key",
      we need to send it now, before the OnRelease for the same key is generated.
      Otherwise the KeyUp would cause OnRelease of the arrow key,
      and later Flush call would send OnPress for the same key. }
    procedure BeforeKeyUp(const Key: Word; const Shift: TShiftState);

    { If some keypress is half-finished, report it now.
      This should be called before e.g. Update event, to report
      events that result in OnKeyDown but not OnUTF8KeyPress,
      or OnUTF8KeyPress but not OnKeyDown. }
    procedure Flush;

    { Called when we collect enough information to make a CGE press event. }
    property OnPress: TControlInputPressReleaseEvent read FOnPress write FOnPress;
  end;

implementation

uses SysUtils, FileUtil, LazUTF8, LCLProc,
  CastleClassUtils, CastleStringUtils, CastleURIUtils, CastleLog;

procedure FileFiltersToDialog(const FileFilters: string;
  Dialog: TFileDialog; const AllFields: boolean);
var
  LCLFilter: string;
  LCLFilterIndex: Integer;
begin
  FileFiltersToDialog(FileFilters, LCLFilter, LCLFilterIndex, AllFields);
  Dialog.Filter := LCLFilter;
  Dialog.FilterIndex := LCLFilterIndex;
end;

procedure FileFiltersToDialog(const FileFilters: string;
  const Edit: TFileNameEdit; const AllFields: boolean);
var
  LCLFilter: string;
  LCLFilterIndex: Integer;
begin
  FileFiltersToDialog(FileFilters, LCLFilter, LCLFilterIndex, AllFields);
  Edit.Filter := LCLFilter;
  Edit.FilterIndex := LCLFilterIndex;
end;

procedure FileFiltersToDialog(const FileFilters: string;
  out LCLFilter: string; out LCLFilterIndex: Integer; const AllFields: boolean);
var
  FFList: TFileFilterList;
begin
  FFList := TFileFilterList.Create(true);
  try
    FFList.AddFiltersFromString(FileFilters);
    FileFiltersToDialog(FFList, LCLFilter, LCLFilterIndex, AllFields);
  finally FreeAndNil(FFList) end;
end;

procedure FileFiltersToDialog(FFList: TFileFilterList;
  Dialog: TFileDialog; const AllFields: boolean);
var
  LCLFilter: string;
  LCLFilterIndex: Integer;
begin
  FileFiltersToDialog(FFList, LCLFilter, LCLFilterIndex, AllFields);
  Dialog.Filter := LCLFilter;
  Dialog.FilterIndex := LCLFilterIndex;
end;

procedure FileFiltersToDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer; const AllFields: boolean);
var
  Filter: TFileFilter;
  I, J: Integer;
begin
  LCLFilter := '';

  { initialize LCLFilterIndex.
    Will be corrected for AllFields=false case, and will be incremented
    (because LCL FilterIndex counts from 1) later. }

  LCLFilterIndex := FFList.DefaultFilter;

  for I := 0 to FFList.Count - 1 do
  begin
    Filter := FFList[I];
    if (not AllFields) and IsPrefix('All ', Filter.Name) then
    begin
      { then we don't want to add this to LCLFilter.
        We also need to fix LCLFilterIndex, to shift it. }
      if I = FFList.DefaultFilter then
        LCLFilterIndex := 0 else
      if I < FFList.DefaultFilter then
        Dec(LCLFilterIndex);
      Continue;
    end;

    LCLFilter += Filter.Name + '|';

    for J := 0 to Filter.Patterns.Count - 1 do
    begin
      if J <> 0 then LCLFilter += ';';
      LCLFilter += Filter.Patterns[J];
    end;

    LCLFilter += '|';
  end;

  { LCL FilterIndex counts from 1. }
  Inc(LCLFilterIndex);
end;

function SQuoteLCLCaption(const S: string): string;
begin
  Result := StringReplace(S, '&', '&&', [rfReplaceAll]);
end;

{ FileFiltersToOpenDialog are deprecated, just call versions without "Open". }
procedure FileFiltersToOpenDialog(const FileFilters: string;
  Dialog: TFileDialog);
begin
  FileFiltersToDialog(FileFilters, Dialog);
end;

procedure FileFiltersToOpenDialog(const FileFilters: string;
  out LCLFilter: string; out LCLFilterIndex: Integer);
begin
  FileFiltersToDialog(FileFilters, LCLFilter, LCLFilterIndex);
end;

procedure FileFiltersToOpenDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer);
begin
  FileFiltersToDialog(FFList, LCLFilter, LCLFilterIndex);
end;

const
  { Ctrl key on most systems, Command key on macOS. }
  ssCtrlOrCommand = {$ifdef DARWIN} ssMeta {$else} ssCtrl {$endif};

function KeyLCLToCastle(const Key: Word; const Shift: TShiftState): TKey;
begin
  Result := keyNone;
  case Key of
    VK_BACK:       Result := keyBackSpace;
    VK_TAB:        Result := keyTab;
    VK_RETURN:     Result := keyEnter;
    VK_SHIFT:      Result := keyShift;
    VK_CONTROL:    Result := keyCtrl;
    VK_MENU:       Result := keyAlt;
    VK_ESCAPE:     Result := keyEscape;
    VK_SPACE:      Result := keySpace;
    VK_PRIOR:      Result := keyPageUp;
    VK_NEXT:       Result := keyPageDown;
    VK_END:        Result := keyEnd;
    VK_HOME:       Result := keyHome;
    VK_LEFT:       Result := keyArrowLeft;
    VK_UP:         Result := keyArrowUp;
    VK_RIGHT:      Result := keyArrowRight;
    VK_DOWN:       Result := keyArrowDown;
    VK_INSERT:     Result := keyInsert;
    VK_DELETE:     Result := keyDelete;
    VK_ADD:        Result := keyNumpadPlus;
    VK_SUBTRACT:   Result := keyNumpadMinus;
    VK_SNAPSHOT:   Result := keyPrintScreen;
    VK_NUMLOCK:    Result := keyNumLock;
    VK_SCROLL:     Result := keyScrollLock;
    VK_CAPITAL:    Result := keyCapsLock;
    VK_PAUSE:      Result := keyPause;
    VK_OEM_COMMA:  Result := keyComma;
    VK_OEM_PERIOD: Result := keyPeriod;
    VK_NUMPAD0:    Result := keyNumpad0;
    VK_NUMPAD1:    Result := keyNumpad1;
    VK_NUMPAD2:    Result := keyNumpad2;
    VK_NUMPAD3:    Result := keyNumpad3;
    VK_NUMPAD4:    Result := keyNumpad4;
    VK_NUMPAD5:    Result := keyNumpad5;
    VK_NUMPAD6:    Result := keyNumpad6;
    VK_NUMPAD7:    Result := keyNumpad7;
    VK_NUMPAD8:    Result := keyNumpad8;
    VK_NUMPAD9:    Result := keyNumpad9;
    VK_CLEAR:      Result := keyNumpadBegin;
    VK_MULTIPLY:   Result := keyNumpadMultiply;
    VK_DIVIDE:     Result := keyNumpadDivide;
    VK_OEM_MINUS:  Result := keyMinus;
    VK_OEM_PLUS:
      if ssShift in Shift then
        Result := keyPlus
      else
        Result := keyEqual;
    Ord('0') .. Ord('9'):
      Result := TKey(Ord(key0)  + Ord(Key) - Ord('0'));
    Ord('A') .. Ord('Z'):
      Result := TKey(Ord(keyA)  + Ord(Key) - Ord('A'));
    VK_F1 .. VK_F12:
      Result := TKey(Ord(keyF1) + Ord(Key) - VK_F1);
  end;
end;

procedure KeyCastleToLCL(const Key: TKey; const KeyString: String;
  out LazKey: Word; out Shift: TShiftState);
begin
  KeyCastleToLCL(Key, KeyString, [], LazKey, Shift);
end;

procedure KeyCastleToLCL(const Key: TKey; KeyString: String;
  const Modifiers: TModifierKeys;
  out LazKey: Word; out Shift: TShiftState);
var
  KeyChar: Char;
begin
  // only for backward compatibility (when this parameter was Char) convert #0 to ''
  if KeyString = #0 then
    KeyString := '';

  Shift := [];
  LazKey := VK_UNKNOWN;
  case Key of
    keyBackSpace:        LazKey := VK_BACK;
    keyTab:              LazKey := VK_TAB;
    keyEnter:            LazKey := VK_RETURN;
    keyShift:            LazKey := VK_SHIFT;
    keyCtrl:             LazKey := VK_CONTROL;
    keyAlt:              LazKey := VK_MENU;
    keyEscape:           LazKey := VK_ESCAPE;
    keySpace:            LazKey := VK_SPACE;
    keyPageUp:           LazKey := VK_PRIOR;
    keyPageDown:         LazKey := VK_NEXT;
    keyEnd:              LazKey := VK_END;
    keyHome:             LazKey := VK_HOME;
    keyArrowLeft:        LazKey := VK_LEFT;
    keyArrowUp:          LazKey := VK_UP;
    keyArrowRight:       LazKey := VK_RIGHT;
    keyArrowDown:        LazKey := VK_DOWN;
    keyInsert:           LazKey := VK_INSERT;
    keyDelete:           LazKey := VK_DELETE;
    keyNumpadPlus:       LazKey := VK_ADD;
    keyNumpadMinus:      LazKey := VK_SUBTRACT;
    keyPrintScreen:      LazKey := VK_SNAPSHOT;
    keyNumLock:          LazKey := VK_NUMLOCK;
    keyScrollLock:       LazKey := VK_SCROLL;
    keyCapsLock:         LazKey := VK_CAPITAL;
    keyPause:            LazKey := VK_PAUSE;
    keyComma:            LazKey := VK_OEM_COMMA;
    keyPeriod:           LazKey := VK_OEM_PERIOD;
    keyNumpad0:          LazKey := VK_NUMPAD0;
    keyNumpad1:          LazKey := VK_NUMPAD1;
    keyNumpad2:          LazKey := VK_NUMPAD2;
    keyNumpad3:          LazKey := VK_NUMPAD3;
    keyNumpad4:          LazKey := VK_NUMPAD4;
    keyNumpad5:          LazKey := VK_NUMPAD5;
    keyNumpad6:          LazKey := VK_NUMPAD6;
    keyNumpad7:          LazKey := VK_NUMPAD7;
    keyNumpad8:          LazKey := VK_NUMPAD8;
    keyNumpad9:          LazKey := VK_NUMPAD9;
    keyNumpadBegin:      LazKey := VK_CLEAR;
    keyNumpadMultiply:   LazKey := VK_MULTIPLY;
    keyNumpadDivide:     LazKey := VK_DIVIDE;
    keyMinus:            LazKey := VK_OEM_MINUS;
    keyEqual:            LazKey := VK_OEM_PLUS;

    { TKey ranges }
    key0 ..key9  : LazKey := Ord('0') + Ord(Key) - Ord(key0);
    keyA ..keyZ  : LazKey := Ord('A') + Ord(Key) - Ord(keyA);
    keyF1..keyF12: LazKey :=    VK_F1 + Ord(Key) - Ord(keyF1);

    else
      if Length(KeyString) = 1 then
      begin
        KeyChar := KeyString[1];
        case KeyChar of
          { follow TMenuItem.Key docs: when Key is keyNone, only KeyChar indicates
            CharBackSpace / CharTab / CharEnter, convert them to Ctrl+xxx shortcuts }
          //CharBackSpace:              LazKey := VK_BACK;
          //CharTab:                    LazKey := VK_TAB;
          //CharEnter:                  LazKey := VK_RETURN;
          CharEscape:                 LazKey := VK_ESCAPE;
          ' ':                        LazKey := VK_SPACE;
          CharDelete:                 LazKey := VK_DELETE;
          '+':                        LazKey := VK_ADD;
          '-':                        LazKey := VK_SUBTRACT;
          ',':                        LazKey := VK_OEM_COMMA;
          '.':                        LazKey := VK_OEM_PERIOD;
          '*':                        LazKey := VK_MULTIPLY;
          '/':                        LazKey := VK_DIVIDE;
          '=':                        LazKey := VK_OEM_PLUS;

          { Char ranges }
          '0' .. '9' : LazKey := Ord(KeyChar);
          { for latter: uppercase letters are VK_xxx codes }
          'A' .. 'Z' : begin LazKey := Ord(KeyChar); Shift := [ssShift]; end;
          'a' .. 'z' : begin LazKey := Ord(UpCase(KeyChar)); end;
          CtrlA .. CtrlZ:
            begin
              LazKey := Ord('A') + Ord(KeyChar) - Ord(CtrlA);
              Shift := [ssCtrlOrCommand];
            end;
        end;
      end;
  end;

  if mkShift in Modifiers then
    Shift += [ssShift];
  if mkCtrl in Modifiers then
    Shift += [ssCtrlOrCommand];
  if mkAlt in Modifiers then
    Shift += [ssAlt];
end;

function MouseButtonLCLToCastle(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: TCastleMouseButton): boolean;
begin
  Result := true;
  case MouseButton of
    Controls.mbLeft  : MyMouseButton := CastleKeysMouse.buttonLeft;
    Controls.mbRight : MyMouseButton := CastleKeysMouse.buttonRight;
    Controls.mbMiddle: MyMouseButton := CastleKeysMouse.buttonMiddle;
    Controls.mbExtra1: MyMouseButton := CastleKeysMouse.buttonExtra1;
    Controls.mbExtra2: MyMouseButton := CastleKeysMouse.buttonExtra2;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else Result := false;
    {$endif}
  end;
end;

function FilenameToURISafeUTF8(const FileName: string): string;
begin
  Result := FilenameToURISafe(UTF8ToSys(FileName));
end;

function URIToFilenameSafeUTF8(const URL: string): string;
begin
  Result := SysToUTF8(URIToFilenameSafe(URL));
end;

function ColorToVector3(const Color: TColor): TVector3;
begin
  Result := Vector3(ColorToVector3Byte(Color));
end;

function ColorToVector3Byte(const Color: TColor): TVector3Byte;
var
  Col: LongInt;
begin
  Col := ColorToRGB(Color);
  RedGreenBlue(Col, Result.X, Result.Y, Result.Z);
end;

{ TLCLKeyPressHandler -------------------------------------------------------- }

procedure TLCLKeyPressHandler.KeyDown(const Key: Word; const Shift: TShiftState);
begin
  if FUnfinishedKeyDown then
    Flush; // the previous press will only have KeyDown information
  Assert(not FUnfinishedKeyDown);

  FUnfinishedKeyDown := true;
  FUnfinishedKeyDownKey := Key;
  FUnfinishedKeyDownShift := Shift;

  // collected both KeyDown and KeyPress
  if FUnfinishedKeyDown and FUnfinishedKeyPress then
    Flush;
end;

procedure TLCLKeyPressHandler.BeforeKeyUp(const Key: Word; const Shift: TShiftState);
begin
  if FUnfinishedKeyDown and (FUnfinishedKeyDownKey = Key) then
    Flush;
end;

procedure TLCLKeyPressHandler.UTF8KeyPress(const UTF8Key: TUTF8Char);
begin
  if FUnfinishedKeyPress then
    Flush; // the previous press will only have KeyPress information
  Assert(not FUnfinishedKeyPress);

  FUnfinishedKeyPress := true;
  FUnfinishedKeyPressKey := UTF8Key;

  // collected both KeyDown and KeyPress
  if FUnfinishedKeyDown and FUnfinishedKeyPress then
    Flush;
end;

procedure TLCLKeyPressHandler.Flush;
var
  Modifiers: TModifierKeys;
  Key: TKey;
  KeyString: String;
begin
  // early exit in most usual case
  if not (FUnfinishedKeyDown or FUnfinishedKeyPress) then Exit;

  Modifiers := [];
  Key := keyNone;
  KeyString := '';

  if FUnfinishedKeyDown then
  begin
    if ssShift in FUnfinishedKeyDownShift then Include(Modifiers, mkShift);
    if ssAlt   in FUnfinishedKeyDownShift then Include(Modifiers, mkAlt);
    if ssCtrl  in FUnfinishedKeyDownShift then Include(Modifiers, mkCtrl);
    Key := KeyLCLToCastle(FUnfinishedKeyDownKey, FUnfinishedKeyDownShift);
  end;

  if FUnfinishedKeyPress then
    KeyString := FUnfinishedKeyPressKey;

  { Thanks to checking this,
    1. LCL presses that cannot be represented as either TKey or KeyString
       will not be reported to CGE,
    2. If Flush was called when FUnfinishedKeyDown = false and
       FUnfinishedKeyPress = false, then nothing will happen.
       (Although right now we prevent this anyway by early exit above.) }

  if Assigned(OnPress) and ((Key <> keyNone) or (KeyString <> '')) then
  begin
    // fix empty KeyString in some cases (needed at least with GTK2 backend)
    if KeyString = '' then
      case Key of
        keyTab   : KeyString := CharTab;
        keyDelete: KeyString := CharDelete;
        else ;
      end;

    OnPress(Self, InputKey(TVector2.Zero, Key, KeyString, Modifiers));
  end;

  FUnfinishedKeyDown := false;
  FUnfinishedKeyPress := false;
end;

end.
