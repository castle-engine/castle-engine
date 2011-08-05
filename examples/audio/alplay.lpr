{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a simple demo of SoundFile unit:
  just load and play given sound file through OpenAL.

  Obviously this is not supposed to be a real music player,
  as OpenAL is not really designed to play music with high quality
  --- OpenAL should be mainly for 3D sounds, where mixing of many sounds
  and spatial sound effects are important.
  Besides, the handling of stereo sounds is different between
  Linux and Windows OpenaL implementations --- Windows impl
  plays sounds stereo and without any spatial sound effects,
  Linux impl converts them to mono sounds and plays with spatial sound effects.
  In the book "Programming Linux games" there were mentions how to
  directly pass some stereo data to OpenAL using Loki extension,
  but I don't know is it still up-to-date information.
}
program alplay;

uses SysUtils, KambiUtils, KambiOpenAL, ALUtils, SoundFile, KambiTimeUtils,
  KambiWarnings, ALSoundEngine, KambiParameters;

var
  Buffer, Source: TALuint;
  FileName: string;
  FAL: TALSoundFile;
begin
  OnWarning := @OnWarningWrite;

  SoundEngine.ParseParameters;
  SoundEngine.MinAllocatedSources := 1;
  SoundEngine.ALContextOpen;
  try
    Writeln(SoundEngine.SoundInitializationReport);
    if not SoundEngine.ALActive then
      Halt;

    { prepare al state }
    { turn off any environmental effects }
    alDistanceModel(AL_NONE);
    {$ifndef UNIX}
    alDopplerFactor(0.0); { turning doppler to 0 does not work under Unix impl }
    {$endif}

    alCreateBuffers(1, @Buffer);
    alCreateSources(1, @Source);
    CheckAL('preparing source and buffer');

    try
      { parse params }
      Parameters.CheckHigh(1);
      FileName := Parameters[1];

      { load file to Buffer, and print some info about file format. }
      FAL := TALSoundFile.Create(TSoundFile.CreateFromFile(FileName), true);
      try
        FAL.alBufferData(Buffer);
        Writeln(
          'File ' + FileName + ' loaded :' + nl +
          '  Class : ' + FAL.SoundFile.ClassName + nl +
          '  Format : ' + ALDataFormatToStr(FAL.SoundFile.DataFormat) + nl +
          '  Size (of sample in file) : ', FAL.SoundFile.DataSize, nl,
          '  Frequency : ',FAL.SoundFile.Frequency, nl,
          '  Duration : ', FAL.SoundFile.Duration:0:2, ' seconds', nl,
          'Size (as returned by querying Buffer) : ', alGetBuffer1sizei(Buffer, AL_SIZE), nl
        );
      finally FAL.Free end;

      { play sound }
      alSourcei(Source, AL_BUFFER, Buffer);
      alSourcePlay(Source);
      CheckAL('starting playing sound');
      while alGetSource1i(Source, AL_SOURCE_STATE) = AL_PLAYING do Delay(1000);
      CheckAL('playing sound');
    finally
      alDeleteSources(1, @Source);
      alDeleteBuffers(1, @Buffer);
    end;
  finally
    SoundEngine.ALContextClose;
  end;
end.
