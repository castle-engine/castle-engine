{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple program that queries OpenAL implementation about various things.
  Useful for checking OpenAL version and extensions, and also to catch
  some minor differences/incompatibilities between various OpenAL
  implementations.

  You can run this with an optional command-line option:
  an URL of any sound file we can load
  (right now, the allowed formats are wav and OggVorbis;
  see CastleSoundFile unit for up-to-date list). Then the sound file will
  be loaded as OpenAL sound, and some additional tests will be performed. }
program algets;

uses CastleOpenAL, CastleALUtils, SysUtils, CastleUtils, CastleVectors,
  CastleStringUtils, CastleEFX, CastleSoundEngine, CastleTimeUtils, CastleParameters;

{ force compatibility : use alCreateSources/Buffers instead of alGen*.

  This is used only to test alCreateSources/Buffers
  --- this should NOT be usually used since it makes algets work
  incorrectly : printed values will not be implementation-defaults but will
  be corrected by mine routines alCreate*. }
{ $define FORCE_COMPAT}

{ Do and write a LOT of gets. Actually, do all possible queries
  of global state and sampleBuffer and sampleSource state.
  Then load test.wav to sampleBuffer and link sampleBuffer to sampleSource,
  and do all possible gets of sampleBuffer and sampleSource state once more.

  Why ? I wanted to see what VENDOR/RENDERER etc. strings are on my
  Win and Linux OpenAL implementations. And (since I know that Linux
  version is a little newer, at least has some more interface)
  I wanted to make sure that my both versions of OpenAL (Linux and Win)
  have THE SAME default state. (I'm trying to find some errors under Win
  - and I want to make sure that Win OpenAL version uses the same defaults
  as Linux OpenAL version). }
procedure Gets;

  function DistanceModelToStr(enum:TALenum):string;
  begin
    case enum of
      AL_NONE : result:='NONE';
      AL_INVERSE_DISTANCE : result:='INVERSE_DISTANCE';
      AL_INVERSE_DISTANCE_CLAMPED : result:='INVERSE_DISTANCE_CLAMPED';
      else result:='!!! UNKNOWN !!!!';
    end;
  end;

  function TwoVectorsToNiceStr(const tv:TALTwoVectors3f):string;
  begin
    result:='at : ' +VectorToNiceStr(tv[0]) +', up : ' +VectorToNiceStr(tv[1]);
  end;

var
  SampleSource, SampleBuffer:TALuint;

  function SampleSourceState:string;
  begin
   result:=
     'Sample Source state -------------------------------' +nl+
     {$ifdef VER3_1_1}
     {$warning Workarounding FPC 3.1.1 internal error 200211262 in algets.lpr}
     {$else}
     'POSITION : '+ VectorToNiceStr(alGetSource3f(SampleSource, AL_POSITION)) +nl+
     'VELOCITY : '+ VectorToNiceStr(alGetSource3f(SampleSource, AL_VELOCITY)) +nl+
     {$endif}
     'GAIN : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_GAIN)) +nl+
     'RELATIVE : '+ BoolToStr(alGetSource1bool(SampleSource, AL_SOURCE_RELATIVE), true) +nl+
     'LOOPING : '+ BoolToStr(alGetSource1bool(SampleSource, AL_LOOPING), true) +nl+
     'BUFFER : '+ IntToStr(alGetSource1ui(SampleSource, AL_BUFFER)) +nl+
     'BUFFERS_QUEUED : '+ IntToStr(alGetSource1ui(SampleSource, AL_BUFFERS_QUEUED)) +nl+
     'BUFFERS_PROCESSED : '+ IntToStr(alGetSource1ui(SampleSource, AL_BUFFERS_PROCESSED)) +nl+
     'MIN_GAIN : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_MIN_GAIN)) +nl+
     'MAX_GAIN : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_MAX_GAIN)) +nl+
     'REFERENCE_DISTANCE : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_REFERENCE_DISTANCE)) +nl+
     'ROLLOFF_FACTOR : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_ROLLOFF_FACTOR)) +nl+
     'MAX_DISTANCE : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_MAX_DISTANCE)) +nl+
     'PITCH : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_PITCH)) +nl+
     {$ifdef VER3_1_1}
     {$warning Workarounding FPC 3.1.1 internal error 200211262 in algets.lpr}
     {$else}
     'DIRECTION : '+ VectorToNiceStr(alGetSource3f(SampleSource, AL_DIRECTION)) +nl+
     {$endif}
     'CONE_INNER_ANGLE : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_CONE_INNER_ANGLE)) +nl+
     'CONE_OUTER_ANGLE : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_CONE_OUTER_ANGLE)) +nl+
     'CONE_OUTER_GAIN : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_CONE_OUTER_GAIN)) +nl+
  //   ' : '+ FloatToNiceStr(alGetSource1f(SampleSource, AL_)) +nl+
      {
      * ENVIRONMENT_IASIG
      * DIRECT_IASIG -- more IASIG reverberation environment variables.
      * DIRECT_HIGH_FREQUENCY_IASIG
      * ROOM_IASIG
      * ROOM_HIGH_FREQUENCY_IASIG
      }
     nl;
  end;

  function SampleBufferState:string;
  begin
   result:=
     'Sample Buffer state -------------------------------' +nl+
     'FREQUENCY : '+ IntToStr(alGetBuffer1i(SampleBuffer, AL_FREQUENCY)) +nl+
     'SIZE : '+ IntToStr(alGetBuffer1sizei(SampleBuffer, AL_SIZE)) +nl+
      { BITS -- sample bit resolution.
      * CHANNELS -- channels provided by Buffer, usually only one
      }
     nl;
  end;

var
  TestSoundURL: string;
  Device: PALCDevice;
  IgnoredDuration: TFloatTime;
begin
  {$ifdef FORCE_COMPAT}
  alCreateSources(1, @SampleSource);
  alCreateBuffers(1, @SampleBuffer);
  {$else}
  alGenSources(1, @SampleSource);
  alGenBuffers(1, @SampleBuffer);
  {$endif}

  try
    Write(
      'GetString --------------------------------', nl,
      'VERSION : ', alGetString(AL_VERSION), nl,
      'RENDERER : ', alGetString(AL_RENDERER), nl,
      'VENDOR : ', alGetString(AL_VENDOR), nl,
      'EXTENSIONS : ', alGetString(AL_EXTENSIONS), nl,

      'ALC_DEFAULT_DEVICE_SPECIFIER : ', SoundEngine.GetContextString(ALC_DEFAULT_DEVICE_SPECIFIER), nl,
      'ALC_DEVICE_SPECIFIER : ', SoundEngine.GetContextString(ALC_DEVICE_SPECIFIER), nl,
      'ALC_EXTENSIONS : ', SoundEngine.GetContextString(ALC_EXTENSIONS), nl,

      nl,
      'Get globals --------------------------------', nl,
      'DISTANCE_MODEL : ',DistanceModelToStr(alGetInteger(AL_DISTANCE_MODEL)), nl,
      'DOPPLER_FACTOR : ',FloatToNiceStr(alGetFloat(AL_DOPPLER_FACTOR)), nl,
      { Undocumented in al specs, and not valid in Creative Windows implementation.
        Not checked in Linux implementation yet.
      'DISTANCE_SCALE : ',FloatToNiceStr(alGetFloat(AL_DISTANCE_SCALE)), nl, }
      'DOPPLER_VELOCITY : ',FloatToNiceStr(alGetFloat(AL_DOPPLER_VELOCITY)), nl,
      nl,
      'Listener state -------------------------------', nl,
      'POSITION : ', VectorToNiceStr(alGetListener3f(AL_POSITION)), nl,
      'VELOCITY : ', VectorToNiceStr(alGetListener3f(AL_VELOCITY)), nl,
      'GAIN : ', FloatToNiceStr(alGetListener1f(AL_GAIN)), nl,
      'ORIENTATION : ', TwoVectorsToNiceStr(alGetListenerOrientation), nl,
      { ENVIRONMENT_IASIG }
      nl,
      SampleSourceState,
      SampleBufferState
    );

    if Parameters.High > 0 then
    begin
      TestSoundURL := Parameters[1];

      TALSoundFile.alBufferDataFromFile(SampleBuffer, TestSoundURL, IgnoredDuration);
      alSourcei(SampleSource, AL_BUFFER, SampleBuffer);

      Write(
        '----------------------------------------------------------------', nl,
        'after loading ', TestSoundURL, ' to SampleBuffer and linking', nl,
        '  SampleBuffer with SampleSource :', nl,
        nl,
        SampleSourceState,
        SampleBufferState
      );
    end;

    Writeln('EFX  -------------------------------');
    Device := alcGetContextsDevice(alcGetCurrentContext());
    Writeln('ALC_EXT_EFX extension present: ', alcIsExtensionPresent(Device, ALC_EXT_EFX_NAME));
    Writeln('EFX supported (ALC_EXT_EFX extension and all entry points): ', SoundEngine.EFXSupported);

    if SoundEngine.EFXSupported then
    begin
      Writeln('ALC_MAX_AUXILIARY_SENDS: ', alcGetInterger1(Device, ALC_MAX_AUXILIARY_SENDS));
    end else
    begin

    end;

  finally
    alDeleteSources(1, @SampleSource);
    alDeleteBuffers(1, @SampleBuffer);
  end;

  CheckAL('Gets');
end;

begin
  SoundEngine.ParseParameters;
  SoundEngine.MinAllocatedSources := 1;
  SoundEngine.ALContextOpen;
  try
    if SoundEngine.ALActive then
      Gets
    else
      Writeln('Sound engine not initialized: ' + SoundEngine.SoundInitializationReport);
  finally
    SoundEngine.ALContextClose;
  end;
end.
