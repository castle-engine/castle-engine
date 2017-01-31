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

{ Simple demo of EFX effects.

  Run with two command-line parameters:
  - URL (usually just a filename) of sound to test,
  - demo number (int between 0 ... 2).

  For example, try
  ./efx_demo ../../../demo-models/sound/werewolf_howling.wav 0
  ./efx_demo ../../../demo-models/sound/werewolf_howling.wav 1
  ./efx_demo ../../../demo-models/sound/werewolf_howling.wav 2
}
program efx_demo;

uses SysUtils, CastleUtils, CastleOpenAL, CastleALUtils, CastleEFX,
  CastleSoundEngine, CastleParameters;

type
  TDemoMode = (
    demoFilterLowPass,
    demoFilterLPEffectReverb,
    { Note that OPENAL_SOFT can't handle demoAll version.
      demoFilterLowPass, demoFilterLPEffectReverb are actually a
      dumbed down versions of demoAll, to pass on OpenAL Soft 1.7.411
      (current as of 2009-03-25) and 1.4.272 (current in Debian testing).
      OpenAL Soft doesn't support EFX fully yet. In particular:

      - OpenAL soft can create only 1 aux slot for now (this is confirmed
        explicitly in source code OpenAL32/alAuxEffectSlot.c)

      - OpenAL soft doesn't support Flanger effect (looking in source code,
        only AL_EFFECT_NULL and AL_EFFECT_REVERB effect types are supported)

      - OpenAL Soft supports only filter types AL_FILTER_NULL and AL_FILTER_LOWPASS.
    }
    demoAll);

var
  DemoMode: TDemoMode = demoFilterLowPass;

  Slots: array [0.. 3] of TALuint;
  Effects: array [0..1] of TALuint;
  Filters: array [0..0] of TALuint;
  SlotsCount: Cardinal = 4;
  EffectsCount: Cardinal = 2;

procedure InitEFX(Source: TALuint);
var
  SlotsCount: Cardinal;
begin
  { Below is based on the "tutorials" (actually it's just one long program
    listing) inside OpenAL SDL doc about EFX. }

  CheckAL('before creating EFX objects');

  { Create a low-pass filter, apply as direct filter to source. }
  alGenFilters(High(Filters) + 1, @Filters);
  CheckAL('after alGenFilters');

  { Set Filter type to Low-Pass and set parameters }
  alFilteri(Filters[0], AL_FILTER_TYPE, AL_FILTER_LOWPASS);
  CheckAL('Low Pass Filter not supported');
  alFilterf(Filters[0], AL_LOWPASS_GAIN, 0.5);
  alFilterf(Filters[0], AL_LOWPASS_GAINHF, 0.5);

  alSourcei(Source, AL_DIRECT_FILTER, Filters[0]);
  CheckAL('Failed to apply a direct path filter');

  if DemoMode <> demoFilterLowPass then
  begin
    if DemoMode = demoAll then
    begin
      SlotsCount := 4;
      EffectsCount := 2;
    end else
    begin
      SlotsCount := 1;
      EffectsCount := 1;
    end;

    { Try to create Auxiliary Effect Slots }
    alGenAuxiliaryEffectSlots(SlotsCount, @Slots);
    CheckAL('after alGenAuxiliaryEffectSlots');

    { Try to create Effects }
    alGenEffects(EffectsCount, @Effects);
    CheckAL('after alGenEffects');

    { Set first Effect Type to Reverb and change Decay Time }
    alEffecti(Effects[0], AL_EFFECT_TYPE, AL_EFFECT_REVERB);
    CheckAL('Reverb Effect not supported');
    alEffectf(Effects[0], AL_REVERB_DECAY_TIME, 5.0);

    if EffectsCount > 1 then
    begin
      { Set second Effect Type to Flanger and change Phase }
      alEffecti(Effects[1], AL_EFFECT_TYPE, AL_EFFECT_FLANGER);
      CheckAL('Flanger effect not supported');
      alEffecti(Effects[1], AL_FLANGER_PHASE, 180);
    end;

    { Attaching an Effect to an Auxiliary Effect Slot. }
    alAuxiliaryEffectSloti(Slots[0], AL_EFFECTSLOT_EFFECT, Effects[0]);
    CheckAL('Unable to load effect into effect slot');

    { Configuring Source Auxiliary Sends. }
    { Set Source Send 0 to feed Slots[0] without filtering }
    alSource3i(Source, AL_AUXILIARY_SEND_FILTER, Slots[0], 0, 0);
    CheckAL('Failed to configure Source Send 0');

    if EffectsCount > 1 then
    begin
      { Set Source Send 1 to feed Slots[1] with filter Filters[0] }
      alSource3i(Source, AL_AUXILIARY_SEND_FILTER, Slots[1], 1, Filters[0]);
      CheckAL('Failed to configure Source Send 1');
    end;
  end;

(* Testing garbage:

  { Disable Send 0 }
  alSource3i(Source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, 0);
  CheckAL('Failed to disable Source Send 0');

  alSource3i(Source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 1, 0);
  CheckAL('Failed to disable Source Send 1');

  { Filter 'Source', a generated Source }
  { Remove filter from 'Source' }
  alSourcei(Source, AL_DIRECT_FILTER, AL_FILTER_NULL);
  CheckAL('Failed to remove direct filter');

  { Filter the Source send 0 from 'Source' to
    Auxiliary Effect Slot Slots[0]
    using Filter Filters[0] }
  alSource3i(Source, AL_AUXILIARY_SEND_FILTER, Slots[0], 0, Filters[0]);
  CheckAL('Failed to apply aux send filter');
  { Remove Filter from Source Auxiliary Send }
  alSource3i(Source, AL_AUXILIARY_SEND_FILTER, Slots[0], 0, AL_FILTER_NULL);
  CheckAL('Failed to remove filter');
*)
end;

procedure CloseEFX;
begin
  alDeleteFilters(High(Filters) + 1, @Filters);

  if DemoMode <> demoFilterLowPass then
  begin
    alDeleteEffects(EffectsCount, @Effects);
    alDeleteAuxiliaryEffectSlots(SlotsCount, @Slots);
  end;
end;

var
  Buffer: TSoundBuffer;
  Sound: TSound;
begin
  SoundEngine.ParseParameters;
  SoundEngine.MinAllocatedSources := 1;
  SoundEngine.ALContextOpen;
  try
    { parse params }
    Parameters.CheckHigh(2);
    Buffer := SoundEngine.LoadBuffer(Parameters[1]);
    DemoMode := TDemoMode(StrToInt(Parameters[2]));

    { play sound }
    Sound := SoundEngine.AllocateSound(1);
    if Sound = nil then
      raise Exception.Create('Not possible to allocate even 1 source');
    Sound.Buffer := Buffer;

    if SoundEngine.EFXSupported then
    begin
      Writeln('EFX supported, applying effects (demo number ', Ord(DemoMode), ')');
      InitEFX(Sound.ALSource);
    end else
      Writeln('EFX not supported, not using any effects');

    alSourcePlay(Sound.ALSource);
    CheckAL('starting playing sound');
    while alGetSource1i(Sound.ALSource, AL_SOURCE_STATE) = AL_PLAYING do Sleep(100);
    CheckAL('playing sound');

    if SoundEngine.EFXSupported then
      CloseEFX;
  finally
    SoundEngine.StopAllSources;
    SoundEngine.FreeBuffer(Buffer);
    SoundEngine.ALContextClose;
  end;
end.
