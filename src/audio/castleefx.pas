{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenAL EFX (Effects Extension) API.

  Documentation is in OpenAL SDK for Windows (installable under Linux
  through wine). }
unit CastleEFX;

{$I castleconf.inc}

interface

uses CastleVectors, CastleOpenAL, Math;

{ Macro below must be named "extdecl", to workaround Lazarus code tools
  known bug http://bugs.freepascal.org/view.php?id=7431 }
{$define extdecl := cdecl}

{$ifndef PASDOC}
{$I efx.inc}
{$I efx-creative.inc}
{$endif PASDOC}

{ Checks is EFX extension, along with all it's entry points, present
  for given OpenAL device. If true, it will also initialize the entry
  points of all EFX functions in this unit.

  Note that if even OpenAL is not avaiable (ALInited = @false), then this
  will also always return @false. }
function Load_EFX(Device: PALCDevice): boolean;

implementation

function Load_EFX(Device: PALCDevice): boolean;
begin
  Result := ALInited and alcIsExtensionPresent(Device, ALC_EXT_EFX_NAME);

  if Result then
  begin
    Pointer(alGenEffects) := alGetProcAddress('alGenEffects'); if not Assigned(alGenEffects) then Exit(false);
    Pointer(alDeleteEffects) := alGetProcAddress('alDeleteEffects'); if not Assigned(alDeleteEffects) then Exit(false);
    Pointer(alIsEffect) := alGetProcAddress('alIsEffect'); if not Assigned(alIsEffect) then Exit(false);
    Pointer(alEffecti) := alGetProcAddress('alEffecti'); if not Assigned(alEffecti) then Exit(false);
    Pointer(alEffectiv) := alGetProcAddress('alEffectiv'); if not Assigned(alEffectiv) then Exit(false);
    Pointer(alEffectf) := alGetProcAddress('alEffectf'); if not Assigned(alEffectf) then Exit(false);
    Pointer(alEffectfv) := alGetProcAddress('alEffectfv'); if not Assigned(alEffectfv) then Exit(false);
    Pointer(alGetEffecti) := alGetProcAddress('alGetEffecti'); if not Assigned(alGetEffecti) then Exit(false);
    Pointer(alGetEffectiv) := alGetProcAddress('alGetEffectiv'); if not Assigned(alGetEffectiv) then Exit(false);
    Pointer(alGetEffectf) := alGetProcAddress('alGetEffectf'); if not Assigned(alGetEffectf) then Exit(false);
    Pointer(alGetEffectfv) := alGetProcAddress('alGetEffectfv'); if not Assigned(alGetEffectfv) then Exit(false);
    Pointer(alGenFilters) := alGetProcAddress('alGenFilters'); if not Assigned(alGenFilters) then Exit(false);
    Pointer(alDeleteFilters) := alGetProcAddress('alDeleteFilters'); if not Assigned(alDeleteFilters) then Exit(false);
    Pointer(alIsFilter) := alGetProcAddress('alIsFilter'); if not Assigned(alIsFilter) then Exit(false);
    Pointer(alFilteri) := alGetProcAddress('alFilteri'); if not Assigned(alFilteri) then Exit(false);
    Pointer(alFilteriv) := alGetProcAddress('alFilteriv'); if not Assigned(alFilteriv) then Exit(false);
    Pointer(alFilterf) := alGetProcAddress('alFilterf'); if not Assigned(alFilterf) then Exit(false);
    Pointer(alFilterfv) := alGetProcAddress('alFilterfv'); if not Assigned(alFilterfv) then Exit(false);
    Pointer(alGetFilteri) := alGetProcAddress('alGetFilteri'); if not Assigned(alGetFilteri) then Exit(false);
    Pointer(alGetFilteriv) := alGetProcAddress('alGetFilteriv'); if not Assigned(alGetFilteriv) then Exit(false);
    Pointer(alGetFilterf) := alGetProcAddress('alGetFilterf'); if not Assigned(alGetFilterf) then Exit(false);
    Pointer(alGetFilterfv) := alGetProcAddress('alGetFilterfv'); if not Assigned(alGetFilterfv) then Exit(false);
    Pointer(alGenAuxiliaryEffectSlots) := alGetProcAddress('alGenAuxiliaryEffectSlots'); if not Assigned(alGenAuxiliaryEffectSlots) then Exit(false);
    Pointer(alDeleteAuxiliaryEffectSlots) := alGetProcAddress('alDeleteAuxiliaryEffectSlots'); if not Assigned(alDeleteAuxiliaryEffectSlots) then Exit(false);
    Pointer(alIsAuxiliaryEffectSlot) := alGetProcAddress('alIsAuxiliaryEffectSlot'); if not Assigned(alIsAuxiliaryEffectSlot) then Exit(false);
    Pointer(alAuxiliaryEffectSloti) := alGetProcAddress('alAuxiliaryEffectSloti'); if not Assigned(alAuxiliaryEffectSloti) then Exit(false);
    Pointer(alAuxiliaryEffectSlotiv) := alGetProcAddress('alAuxiliaryEffectSlotiv'); if not Assigned(alAuxiliaryEffectSlotiv) then Exit(false);
    Pointer(alAuxiliaryEffectSlotf) := alGetProcAddress('alAuxiliaryEffectSlotf'); if not Assigned(alAuxiliaryEffectSlotf) then Exit(false);
    Pointer(alAuxiliaryEffectSlotfv) := alGetProcAddress('alAuxiliaryEffectSlotfv'); if not Assigned(alAuxiliaryEffectSlotfv) then Exit(false);
    Pointer(alGetAuxiliaryEffectSloti) := alGetProcAddress('alGetAuxiliaryEffectSloti'); if not Assigned(alGetAuxiliaryEffectSloti) then Exit(false);
    Pointer(alGetAuxiliaryEffectSlotiv) := alGetProcAddress('alGetAuxiliaryEffectSlotiv'); if not Assigned(alGetAuxiliaryEffectSlotiv) then Exit(false);
    Pointer(alGetAuxiliaryEffectSlotf) := alGetProcAddress('alGetAuxiliaryEffectSlotf'); if not Assigned(alGetAuxiliaryEffectSlotf) then Exit(false);
    Pointer(alGetAuxiliaryEffectSlotfv) := alGetProcAddress('alGetAuxiliaryEffectSlotfv'); if not Assigned(alGetAuxiliaryEffectSlotfv) then Exit(false);
  end;
end;

end.
