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

{ Utilities for Delphi UI that works the same in VCL and FMX. }
unit CastleInternalDelphiUtils;

{$I castleconf.inc}

interface

uses UITypes, Classes,
  CastleKeysMouse;

{ Convert mouse button (from VCL or FMX) to CGE. }
function MouseButtonToCastle(const MouseButton: TMouseButton;
  out CastleMouseButton: TCastleMouseButton): Boolean;

{ Convert key (from VCL or FMX) to CGE.
  If not possible, returns false, and sets CastleKey to keyNone, CastleKeyString to ''. }
function KeyToCastle(const Key: Word; const Shift: TShiftState;
  out CastleKey: TKey; out CastleKeyString: String): Boolean;

type
  TDesignTimeProjectPathEvent = function: String;

var
  OnGetDesignTimeProjectPath: TDesignTimeProjectPathEvent;

implementation

uses Windows, // contains VK_xxx in Delphi
  CastleStringUtils;

function MouseButtonToCastle(const MouseButton: TMouseButton;
  out CastleMouseButton: TCastleMouseButton): Boolean;
begin
  Result := true;
  case MouseButton of
    TMouseButton.mbLeft  : CastleMouseButton := buttonLeft;
    TMouseButton.mbRight : CastleMouseButton := buttonRight;
    TMouseButton.mbMiddle: CastleMouseButton := buttonMiddle;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else Result := false;
    {$endif}
  end;
end;

function KeyToCastle(const Key: Word; const Shift: TShiftState;
  out CastleKey: TKey; out CastleKeyString: String): Boolean;
begin
  Result := false;
  CastleKey := keyNone;
  CastleKeyString := '';

  case Key of
    VK_BACK:       CastleKey := keyBackSpace;
    VK_TAB:        CastleKey := keyTab;
    VK_RETURN:     CastleKey := keyEnter;
    VK_SHIFT:      CastleKey := keyShift;
    VK_CONTROL:    CastleKey := keyCtrl;
    VK_MENU:       CastleKey := keyAlt;
    VK_ESCAPE:     CastleKey := keyEscape;
    VK_SPACE:      CastleKey := keySpace;
    VK_PRIOR:      CastleKey := keyPageUp;
    VK_NEXT:       CastleKey := keyPageDown;
    VK_END:        CastleKey := keyEnd;
    VK_HOME:       CastleKey := keyHome;
    VK_LEFT:       CastleKey := keyArrowLeft;
    VK_UP:         CastleKey := keyArrowUp;
    VK_RIGHT:      CastleKey := keyArrowRight;
    VK_DOWN:       CastleKey := keyArrowDown;
    VK_INSERT:     CastleKey := keyInsert;
    VK_DELETE:     CastleKey := keyDelete;
    VK_ADD:        CastleKey := keyNumpadPlus;
    VK_SUBTRACT:   CastleKey := keyNumpadMinus;
    VK_SNAPSHOT:   CastleKey := keyPrintScreen;
    VK_NUMLOCK:    CastleKey := keyNumLock;
    VK_SCROLL:     CastleKey := keyScrollLock;
    VK_CAPITAL:    CastleKey := keyCapsLock;
    VK_PAUSE:      CastleKey := keyPause;
    VK_OEM_COMMA:  CastleKey := keyComma;
    VK_OEM_PERIOD: CastleKey := keyPeriod;
    VK_NUMPAD0:    CastleKey := keyNumpad0;
    VK_NUMPAD1:    CastleKey := keyNumpad1;
    VK_NUMPAD2:    CastleKey := keyNumpad2;
    VK_NUMPAD3:    CastleKey := keyNumpad3;
    VK_NUMPAD4:    CastleKey := keyNumpad4;
    VK_NUMPAD5:    CastleKey := keyNumpad5;
    VK_NUMPAD6:    CastleKey := keyNumpad6;
    VK_NUMPAD7:    CastleKey := keyNumpad7;
    VK_NUMPAD8:    CastleKey := keyNumpad8;
    VK_NUMPAD9:    CastleKey := keyNumpad9;
    VK_CLEAR:      CastleKey := keyNumpadBegin;
    VK_MULTIPLY:   CastleKey := keyNumpadMultiply;
    VK_DIVIDE:     CastleKey := keyNumpadDivide;
    VK_OEM_MINUS:  CastleKey := keyMinus;
    VK_OEM_PLUS:
      if ssShift in Shift then
        CastleKey := keyPlus
      else
        CastleKey := keyEqual;
    Ord('0') .. Ord('9'):
      begin
        CastleKey := TKey(Ord(key0)  + Ord(Key) - Ord('0'));
        CastleKeyString := Chr(Key);
      end;
    Ord('A') .. Ord('Z'):
      begin
        CastleKey := TKey(Ord(keyA)  + Ord(Key) - Ord('A'));
        if ssShift in Shift then // TODO: this will ignore caps lock
          CastleKeyString := UpCase(Chr(Key))
        else
          CastleKeyString := LoCase(Chr(Key));
      end;
    VK_F1 .. VK_F12:
      CastleKey := TKey(Ord(keyF1) + Ord(Key) - VK_F1);
  end;

  Result := (CastleKey <> keyNone) or (CastleKeyString <> '');
end;

end.
