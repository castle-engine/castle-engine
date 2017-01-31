{
  Copyright 2008-2017 Michalis Kamburelis.

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

uses Dialogs, Classes, Controls, CastleFileFilters, CastleKeysMouse,
  Graphics, CastleVectors;

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

  In addition, this tries to convert Key to a character (MyCharKey).
  It's awful that this function has to do conversion to Char,
  but that's the way of VCL and LCL: KeyPress and KeyDown
  are separate events. While I want to have them in one event,
  and passed as one event to TUIControl.KeyDown. }
procedure KeyLCLToCastle(const Key: Word; const Shift: TShiftState;
  out MyKey: TKey; out MyCharKey: char);

{ Convert TKey and/or character code into Lazarus key code (VK_xxx)
  and shift state.
  Sets LazKey to VK_UNKNOWN (zero) when conversion not possible
  (or when Key is K_None and CharKey = #0).

  Note that this is not a perfect reverse of KeyLCLToCastle function.
  It can't, as there are ambiguities (e.g. character 'A' may
  be a key K_A with mkShift in modifiers).

  @groupBegin }
procedure KeyCastleToLCL(const Key: TKey; const CharKey: char;
  const Modifiers: TModifierKeys;
  out LazKey: Word; out Shift: TShiftState);
procedure KeyCastleToLCL(const Key: TKey; const CharKey: char;
  out LazKey: Word; out Shift: TShiftState);
{ @groupEnd }

{ Convert Lazarus Controls.TMouseButton value to Castle Game Engine
  CastleKeysMouse.TMouseButton.

  (By coincidence, my type name and values are the same as used by LCL;
  but beware --- the order of values in my type is different (mbMiddle
  is in the middle in my type)). }
function MouseButtonLCLToCastle(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: CastleKeysMouse.TMouseButton): boolean;

const
  CursorCastleToLCL: array [TMouseCursor] of TCursor =
  ( crDefault, crNone, crNone, crDefault { mcCustom treat like mcDefault },
    crArrow, crHourGlass, crIBeam, crHandPoint );

function FilenameToURISafeUTF8(const FileName: string): string;
function URIToFilenameSafeUTF8(const URL: string): string;

{ Convert LCL color values to our colors (vectors). }
function ColorToVector3(const Color: TColor): TVector3Single;
function ColorToVector3Byte(const Color: TColor): TVector3Byte;

implementation

uses SysUtils, LazUTF8, FileUtil, LCLType, LCLProc,
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
  { Ctrl key on most systems, Command key on Mac OS X. }
  ssCtrlOrCommand = {$ifdef DARWIN} ssMeta {$else} ssCtrl {$endif};

procedure KeyLCLToCastle(const Key: Word; const Shift: TShiftState;
  out MyKey: TKey; out MyCharKey: char);
begin
  MyKey := K_None;
  MyCharKey := #0;

  case Key of
    VK_BACK:       begin MyKey := K_BackSpace;       MyCharKey := CharBackSpace; end;
    VK_TAB:        begin MyKey := K_Tab;             MyCharKey := CharTab;       end;
    VK_RETURN:     begin MyKey := K_Enter;           MyCharKey := CharEnter;     end;
    VK_SHIFT:            MyKey := K_Shift;
    VK_CONTROL:          MyKey := K_Ctrl;
    VK_MENU:             MyKey := K_Alt;
    VK_ESCAPE:     begin MyKey := K_Escape;          MyCharKey := CharEscape;    end;
    VK_SPACE:      begin MyKey := K_Space;           MyCharKey := ' ';           end;
    VK_PRIOR:            MyKey := K_PageUp;
    VK_NEXT:             MyKey := K_PageDown;
    VK_END:              MyKey := K_End;
    VK_HOME:             MyKey := K_Home;
    VK_LEFT:             MyKey := K_Left;
    VK_UP:               MyKey := K_Up;
    VK_RIGHT:            MyKey := K_Right;
    VK_DOWN:             MyKey := K_Down;
    VK_INSERT:           MyKey := K_Insert;
    VK_DELETE:     begin MyKey := K_Delete;          MyCharKey := CharDelete; end;
    VK_ADD:        begin MyKey := K_Numpad_Plus;     MyCharKey := '+';        end;
    VK_SUBTRACT:   begin MyKey := K_Numpad_Minus;    MyCharKey := '-';        end;
    VK_SNAPSHOT:         MyKey := K_PrintScreen;
    VK_NUMLOCK:          MyKey := K_NumLock;
    VK_SCROLL:           MyKey := K_ScrollLock;
    VK_CAPITAL:          MyKey := K_CapsLock;
    VK_PAUSE:            MyKey := K_Pause;
    VK_OEM_COMMA:  begin MyKey := K_Comma;           MyCharKey := ','; end;
    VK_OEM_PERIOD: begin MyKey := K_Period;          MyCharKey := '.'; end;
    VK_NUMPAD0:    begin MyKey := K_Numpad_0;        MyCharKey := '0'; end;
    VK_NUMPAD1:    begin MyKey := K_Numpad_1;        MyCharKey := '1'; end;
    VK_NUMPAD2:    begin MyKey := K_Numpad_2;        MyCharKey := '2'; end;
    VK_NUMPAD3:    begin MyKey := K_Numpad_3;        MyCharKey := '3'; end;
    VK_NUMPAD4:    begin MyKey := K_Numpad_4;        MyCharKey := '4'; end;
    VK_NUMPAD5:    begin MyKey := K_Numpad_5;        MyCharKey := '5'; end;
    VK_NUMPAD6:    begin MyKey := K_Numpad_6;        MyCharKey := '6'; end;
    VK_NUMPAD7:    begin MyKey := K_Numpad_7;        MyCharKey := '7'; end;
    VK_NUMPAD8:    begin MyKey := K_Numpad_8;        MyCharKey := '8'; end;
    VK_NUMPAD9:    begin MyKey := K_Numpad_9;        MyCharKey := '9'; end;
    VK_CLEAR:            MyKey := K_Numpad_Begin;
    VK_MULTIPLY:   begin MyKey := K_Numpad_Multiply; MyCharKey := '*'; end;
    VK_DIVIDE:     begin MyKey := K_Numpad_Divide;   MyCharKey := '/'; end;
    VK_OEM_MINUS:  begin MyKey := K_Minus;           MyCharKey := '-'; end;
    VK_OEM_PLUS:
      if ssShift in Shift then
      begin
        MyKey := K_Plus ; MyCharKey := '+';
      end else
      begin
        MyKey := K_Equal; MyCharKey := '=';
      end;

    Ord('0') .. Ord('9'):
      begin
        MyKey := TKey(Ord(K_0)  + Ord(Key) - Ord('0'));
        MyCharKey := Chr(Key);
      end;

    Ord('A') .. Ord('Z'):
      begin
        MyKey := TKey(Ord(K_A)  + Ord(Key) - Ord('A'));
        if ssCtrlOrCommand in Shift then
          MyCharKey := Chr(Ord(CtrlA) + Ord(Key) - Ord('A')) else
        begin
          MyCharKey := Chr(Key);
          if not (ssShift in Shift) then
            MyCharKey := LoCase(MyCharKey);
        end;
      end;

    VK_F1 .. VK_F12  : MyKey := TKey(Ord(K_F1) + Ord(Key) - VK_F1);
  end;

  if (MyKey = K_None) and (MyCharKey = #0) then
    WritelnLog('LCL', 'Cannot translate LCL VK_xxx key %s with shift %s to Castle Game Engine key',
      [DbgsVKCode(Key), DbgS(Shift)]);
end;

procedure KeyCastleToLCL(const Key: TKey; const CharKey: char;
  out LazKey: Word; out Shift: TShiftState);
begin
  KeyCastleToLCL(Key, CharKey, [], LazKey, Shift);
end;

procedure KeyCastleToLCL(const Key: TKey; const CharKey: char;
  const Modifiers: TModifierKeys;
  out LazKey: Word; out Shift: TShiftState);
begin
  Shift := [];
  LazKey := VK_UNKNOWN;
  case Key of
    K_BackSpace:        LazKey := VK_BACK;
    K_Tab:              LazKey := VK_TAB;
    K_Enter:            LazKey := VK_RETURN;
    K_Shift:            LazKey := VK_SHIFT;
    K_Ctrl:             LazKey := VK_CONTROL;
    K_Alt:              LazKey := VK_MENU;
    K_Escape:           LazKey := VK_ESCAPE;
    K_Space:            LazKey := VK_SPACE;
    K_PageUp:           LazKey := VK_PRIOR;
    K_PageDown:         LazKey := VK_NEXT;
    K_End:              LazKey := VK_END;
    K_Home:             LazKey := VK_HOME;
    K_Left:             LazKey := VK_LEFT;
    K_Up:               LazKey := VK_UP;
    K_Right:            LazKey := VK_RIGHT;
    K_Down:             LazKey := VK_DOWN;
    K_Insert:           LazKey := VK_INSERT;
    K_Delete:           LazKey := VK_DELETE;
    K_Numpad_Plus:      LazKey := VK_ADD;
    K_Numpad_Minus:     LazKey := VK_SUBTRACT;
    K_PrintScreen:      LazKey := VK_SNAPSHOT;
    K_NumLock:          LazKey := VK_NUMLOCK;
    K_ScrollLock:       LazKey := VK_SCROLL;
    K_CapsLock:         LazKey := VK_CAPITAL;
    K_Pause:            LazKey := VK_PAUSE;
    K_Comma:            LazKey := VK_OEM_COMMA;
    K_Period:           LazKey := VK_OEM_PERIOD;
    K_Numpad_0:         LazKey := VK_NUMPAD0;
    K_Numpad_1:         LazKey := VK_NUMPAD1;
    K_Numpad_2:         LazKey := VK_NUMPAD2;
    K_Numpad_3:         LazKey := VK_NUMPAD3;
    K_Numpad_4:         LazKey := VK_NUMPAD4;
    K_Numpad_5:         LazKey := VK_NUMPAD5;
    K_Numpad_6:         LazKey := VK_NUMPAD6;
    K_Numpad_7:         LazKey := VK_NUMPAD7;
    K_Numpad_8:         LazKey := VK_NUMPAD8;
    K_Numpad_9:         LazKey := VK_NUMPAD9;
    K_Numpad_Begin:     LazKey := VK_CLEAR;
    K_Numpad_Multiply:  LazKey := VK_MULTIPLY;
    K_Numpad_Divide:    LazKey := VK_DIVIDE;
    K_Minus:            LazKey := VK_OEM_MINUS;
    K_Equal:            LazKey := VK_OEM_PLUS;

    { TKey ranges }
    K_0 ..K_9  : LazKey := Ord('0') + Ord(Key) - Ord(K_0);
    K_A ..K_Z  : LazKey := Ord('A') + Ord(Key) - Ord(K_A);
    K_F1..K_F12: LazKey :=    VK_F1 + Ord(Key) - Ord(K_F1);

    else
      case CharKey of
        { follow TMenuItem.Key docs: when Key is K_None, only CharKey indicates
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
        '0' .. '9' : LazKey := Ord(CharKey);
        { for latter: uppercase letters are VK_xxx codes }
        'A' .. 'Z' : begin LazKey := Ord(CharKey); Shift := [ssShift]; end;
        'a' .. 'z' : begin LazKey := Ord(UpCase(CharKey)); end;
        CtrlA .. CtrlZ:
          begin
            LazKey := Ord('A') + Ord(CharKey) - Ord(CtrlA);
            Shift := [ssCtrlOrCommand];
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
  out MyMouseButton: CastleKeysMouse.TMouseButton): boolean;
begin
  Result := true;
  case MouseButton of
    Controls.mbLeft  : MyMouseButton := CastleKeysMouse.mbLeft;
    Controls.mbRight : MyMouseButton := CastleKeysMouse.mbRight;
    Controls.mbMiddle: MyMouseButton := CastleKeysMouse.mbMiddle;
    Controls.mbExtra1: MyMouseButton := CastleKeysMouse.mbExtra1;
    Controls.mbExtra2: MyMouseButton := CastleKeysMouse.mbExtra2;
    else Result := false;
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

function ColorToVector3(const Color: TColor): TVector3Single;
begin
  Result := Vector3Single(ColorToVector3Byte(Color));
end;

function ColorToVector3Byte(const Color: TColor): TVector3Byte;
var
  Col: LongInt;
begin
  Col := ColorToRGB(Color);
  RedGreenBlue(Col, Result[0], Result[1], Result[2]);
end;

end.
