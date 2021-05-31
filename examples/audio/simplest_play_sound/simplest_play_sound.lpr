{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simply load and play sound using CastleSoundEngine. }
program simplest_play_sound;

uses SysUtils, CastleUtils,
  CastleLog, CastleSoundEngine, CastleParameters, CastleTimeUtils, CastleVectors,
  CastleApplicationProperties;

var
  Buffer: TSoundBuffer;
  URL: string;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  // put in log various info about sound loading
  InitializeLog;
  SoundEngine.LogSoundLoading := true;

  { parse params }
  SoundEngine.ParseParameters;
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    URL := Parameters[1]
  else
    URL := 'castle-data:/temple-adam-goh.ogg';
    //'castle-data:/tone.wav';

  { Load and play sound, without any spatialization.
    Sound backend (like OpenAL) will be automatically initialized when needed below.
    Although you could also initialize it explicitly by SoundEngine.ContextOpen,
    check SoundEngine.Information, SoundEngine.IsContextOpenSuccess etc. }
  Buffer := SoundEngine.LoadBuffer(URL);
  Writeln('Sound loaded, duration in seconds: ', Buffer.Duration:1:2);
  SoundEngine.PlaySound(Buffer);

  { Wait enough time to finish playing. (because PlaySound above doesn't block).
    In this simple program, we just sleep enough time
    to finish playing sound, with some margin. Alternative, more precise way
    to do this would be query is sound playing (call SoundEngine.Refresh
    from time to time, and watch out for TInternalPlayingSound.OnRelease event;
    PlaySound returns TInternalPlayingSound instance for such purposes). }
  Sleep(Round(Buffer.Duration * 1000) + 500);
end.
