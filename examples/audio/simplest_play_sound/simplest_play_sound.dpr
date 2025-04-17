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

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  {$if defined(FPC) and (not defined(CASTLE_DISABLE_THREADS))}
    {$info Thread support enabled.}
    {$ifdef UNIX} CThreads, {$endif}
  {$endif}
  SysUtils,
  CastleUtils, CastleLog, CastleSoundEngine, CastleParameters, CastleTimeUtils,
  CastleVectors, CastleApplicationProperties;

var
  Url: String;
  Sound: TCastleSound;
begin
  ApplicationProperties.OnWarning.Add(
    {$ifdef FPC}@{$endif} ApplicationProperties.WriteWarningOnConsole);

  // put in log various info about sound loading
  InitializeLog;
  SoundEngine.LogSoundLoading := true;

  { parse params }
  SoundEngine.ParseParameters;
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    Url := Parameters[1]
  else
    Url := 'castle-data:/temple-adam-goh.ogg';
    //'castle-data:/tone.wav';

  { Sound backend (like OpenAL) will be automatically initialized when needed below.
    Although you could also initialize it explicitly by SoundEngine.ContextOpen,
    check SoundEngine.Information, SoundEngine.IsContextOpenSuccess etc. }

  { Load and play sound, without any spatialization. }
  Sound := TCastleSound.Create(nil);
  try
    Sound.Url := Url;
    Writeln('Sound loaded, duration in seconds: ', Sound.Duration:1:2);
    SoundEngine.Play(Sound);

    { Wait enough time to finish playing.
      In this simple program, we just sleep enough time
      to finish playing the sound + 0.1 second delay. }
    Sleep(Round(Sound.Duration * 1000) + 100);

    { The above approach to wait is not suitable for most real applications,
      where you don't want to call Sleep as it just hangs your process.
      The proper solution is to use TCastlePlayingSound,
      and watch until TCastlePlayingSound.Playing is false,
      or register event on TCastlePlayingSound.OnStop.

      Example:

    PlayingSound := TCastlePlayingSound.Create(nil);
    PlayingSound.Sound := Sound;
    PlayingSound.FreeOnStop := true;
    SoundEngine.Play(PlayingSound);
    while PlayingSound.Playing do
    begin
      // do anything, like
      // Sleep(10);
      // Appplication.ProcessAllMessages;
    end;
    }
  finally
    FreeAndNil(Sound);
  end;
end.
