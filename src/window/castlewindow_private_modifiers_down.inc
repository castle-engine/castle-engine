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

{ Small helpful utilities for CastleWindow implementations that are based
  on a system where left shift and right shift keys (or Alt, or Ctrl keys) 
  are given as different keys. This situation requires some special care
  because in our TKey type (and so in TCastleWindowCustom.KeysDown table)
  we do not differentiate between right and left Shift key (or Alt, or Ctrl).
  
  Currently this concerns GTK and Xlib implementations.
  
  If you want to use this file in some CastleWindow implementation,
  define CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN when compiling that implementation.
  It is important to define CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN
  instead of simply including this file in CastleWindow-implementation-specific
  file, because this allows ReleaseAllKeysAndMouse to use some
  special code when CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN.
  
  Then you have to call SetPrivateModifiersDown with
  appropriate arguments at appropriate times. And that's all.
  SetPrivateModifiersDown will take care of calling DoKeyDown(k) and DoKeyUp(k)
  for you for k = K_Shift, K_Ctrl, K_Alt. }
  
{$ifdef read_window_interface}
private
  { We keep here internally state of left shift key
    (in PrivateModifiersDown[mkShift, false]), right shift key
    (in PrivateModifiersDown[mkShift, true]) and the same for Alt and Ctrl key
    (so it's like PrivateModifiersDown[TModifierKey, IsRight]).
    I have to do this because Xlib differs left and right keys.
    And I want to set KeysDown[K_Shift] always to "one of the shift keys is down".

    E.g. imagine this:
    1. user presses left shift key. We do DoKeyDown(K_Shift).
    2. then he presses right shift key. Maybe we do DoKeyDown(K_Shift),
       maybe not. It doesn't matter. What matters is that we have still
       KeysDown[K_Shift] = true, no problem.
    3. now user releases left shift. And what ? We can't do DoKeyUp(K_Shift)
       now, because K_Shift should be still true (right shift is still down).
       So we have to keep somewhere states of left shift and right shift
       separately. (NOT only in KeysDown, where we have one K_Shift)

    That's what PrivateModifiersDown is for. }
  PrivateModifiersDown:array[TModifierKey, boolean]of boolean;
  
  { Set PrivateModifiersDown[Key, IsRight] := Value.
    Assume that k = ModifierKeyToKey[ModKey].
      If Value then it can call DoKeyDown(k), if state of k should change.
      If not Value then it can call DoKeyUp(k), if state of k should change. }
  procedure SetPrivateModifiersDown(ModKey:TModifierKey; IsRight:boolean; 
    Value:boolean);
    
{$endif read_window_interface}
  
{$ifdef read_implementation}

procedure TCastleWindowCustom.SetPrivateModifiersDown(ModKey:TModifierKey; IsRight:boolean; 
  Value:boolean);
var OldState, NewState:boolean;
begin
 { OldState and NewState are states of KeysDown[ModifierKeyToKey[ModKey]] }
 OldState:=PrivateModifiersDown[ModKey, true] or PrivateModifiersDown[ModKey, false];
 PrivateModifiersDown[ModKey, IsRight]:=Value;
 NewState:=PrivateModifiersDown[ModKey, true] or PrivateModifiersDown[ModKey, false];
 
 if OldState and (not NewState) then 
  DoKeyUp(ModifierKeyToKey[ModKey]) else
 if (not OldState) and NewState then
  DoKeyDown(ModifierKeyToKey[ModKey], #0);
end;

{$endif read_implementation}