{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simply load and play sound file using OpenAL. }
program alplay;

uses SysUtils, CastleUtils, CastleOpenAL,
  CastleWarnings, CastleSoundEngine, CastleParameters, CastleTimeUtils, VectorMath;

var
  Buffer: TALBuffer;
  FileName: string;
  Duration: TFloatTime;
begin
  OnWarning := @OnWarningWrite;

  { add here InitializeLog('1.0') (from CastleLog unit) to see various info
    about OpenAL and sound loading }

  { parse params }
  SoundEngine.ParseParameters;
  Parameters.CheckHigh(1);
  FileName := Parameters[1];

  { Change the default MinAllocatedSources (it may be larger for the default
    engine usage, as we expect that some sound mixing will be needed;
    for this demo, 1 is enough). }
  SoundEngine.MinAllocatedSources := 1;

  { Load and play sound, without any spatialization.
    OpenAL will be automatically initialized when needed below.
    Although you could also initialize it explicitly by SoundEngine.ALContextOpen,
    check SoundEngine.SoundInitializationReport, SoundEngine.ALActive etc. }
  Buffer := SoundEngine.LoadBuffer(FileName, Duration);
  Writeln('Sound loaded, duration in seconds: ', Duration:1:2);
  SoundEngine.PlaySound(Buffer, false, false, 0, 1, 0, 1, ZeroVector3Single);

  { Wait enough time to finish playing. (because PlaySound above doesn't block).
    In this simple program, we just sleep enough time
    to finish playing sound, with some margin. Alternative, more precise way
    to do this would be query is sound playing (call SoundEngine.RefreshUsedSources
    from time to time, and watch out for TALSound.OnRelease event;
    PlaySound returns TALSound instance for such purposes). }
  Sleep(Round(Duration * 1000)+ 100);
end.
