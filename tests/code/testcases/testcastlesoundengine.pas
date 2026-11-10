// -*- compile-command: "./test_single_testcase.sh TTestCastleSoundEngine" -*-
{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleSoundEngine unit. }
unit TestCastleSoundEngine;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleSoundEngine = class(TCastleTestCase)
  published
    procedure TestLoadBufferException;
    procedure TestNotPcmEncodingWarning;
    procedure TestImportancePriority;
    procedure TestSoundFromDataUri;
    { TCastleSoundReverb: defaults, change notification, Assign, SetDefaults. }
    procedure TestReverbParameters;
    { SoundEngine.Reverb with the sound backend: enabling, changing
      (also out of range) and disabling the reverb while spatial and non-spatial
      sounds play, without errors. }
    procedure TestReverbBackend;
  end;

implementation

uses CastleFilesUtils, CastleSoundEngine, CastleApplicationProperties, CastleDownload,
  CastleLog, CastleVectors;

type
  TChangeCounter = class
    Count: Integer;
    procedure Changed(Sender: TObject);
  end;

procedure TChangeCounter.Changed(Sender: TObject);
begin
  Inc(Count);
end;

procedure TTestCastleSoundEngine.TestReverbParameters;
var
  Reverb, Reverb2: TCastleSoundReverb;
  Counter: TChangeCounter;
begin
  Counter := TChangeCounter.Create;
  Reverb := TCastleSoundReverb.Create;
  Reverb2 := TCastleSoundReverb.Create;
  try
    { defaults of the EFX (EAX) reverb }
    AssertFalse(Reverb.Enabled);
    AssertSameValue(1.0, Reverb.Density, 0.0001);
    AssertSameValue(0.32, Reverb.Gain, 0.0001);
    AssertSameValue(1.49, Reverb.DecayTime, 0.0001);
    AssertSameValue(0.83, Reverb.DecayHFRatio, 0.0001);
    AssertSameValue(5000, Reverb.HFReference, 0.0001);
    AssertTrue(Reverb.DecayHFLimit);
    AssertVectorEquals(TVector3.Zero, Reverb.ReflectionsPan);
    AssertTrue(Reverb.Parameters.DecayHFLimit);
    AssertSameValue(1.49, Reverb.Parameters.DecayTime, 0.0001);

    { OnChange only when something changes }
    Reverb.OnChange := {$ifdef FPC}@{$endif} Counter.Changed;
    Reverb.DecayTime := 1.49;
    AssertEquals(0, Counter.Count);
    Reverb.DecayTime := 4;
    AssertEquals(1, Counter.Count);
    AssertSameValue(4, Reverb.Parameters.DecayTime, 0.0001);
    Reverb.Enabled := true;
    Reverb.Enabled := true;
    AssertEquals(2, Counter.Count);
    Reverb.LateReverbPan := Vector3(0, 0, -1);
    AssertEquals(3, Counter.Count);
    Reverb.DecayHFLimit := false;
    AssertEquals(4, Counter.Count);

    { Assign copies all parameters (and notifies) }
    Reverb2.Assign(Reverb);
    AssertTrue(Reverb2.Enabled);
    AssertSameValue(4, Reverb2.DecayTime, 0.0001);
    AssertFalse(Reverb2.DecayHFLimit);
    AssertVectorEquals(Vector3(0, 0, -1), Reverb2.LateReverbPan);
    Reverb.Assign(Reverb2);
    AssertEquals(5, Counter.Count);

    { SetDefaults resets the parameters, but not Enabled }
    Reverb.SetDefaults;
    AssertEquals(6, Counter.Count);
    AssertTrue(Reverb.Enabled);
    AssertSameValue(1.49, Reverb.DecayTime, 0.0001);
    AssertTrue(Reverb.DecayHFLimit);
    AssertVectorEquals(TVector3.Zero, Reverb.LateReverbPan);
  finally
    FreeAndNil(Reverb);
    FreeAndNil(Reverb2);
    FreeAndNil(Counter);
  end;
end;

procedure TTestCastleSoundEngine.TestReverbBackend;
var
  Sound: TCastleSound;
  Spatial, NonSpatial: TCastlePlayingSound;
  SavedReverb: TCastleSoundReverb;
begin
  SoundEngine.ContextOpen;
  if not SoundEngine.IsContextOpenSuccess then
  begin
    Writeln('Sound backend cannot be initialized, TestReverbBackend doesn''t really do anything');
    { Reverb can still be configured, it's just not supported now }
    AssertFalse(SoundEngine.ReverbSupported);
    Exit;
  end;
  if not SoundEngine.ReverbSupported then
    Writeln('Sound backend has no reverb (e.g. OpenAL without EFX), TestReverbBackend only checks that the reverb is ignored')
  else
    Writeln('Sound backend has reverb, TestReverbBackend tests it');

  { OpenAL errors are reported as warnings, make them fail the test }
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  SavedReverb := TCastleSoundReverb.Create;
  Sound := TCastleSound.Create(nil);
  Spatial := TCastlePlayingSound.Create(nil);
  NonSpatial := TCastlePlayingSound.Create(nil);
  try
    SavedReverb.Assign(SoundEngine.Reverb);
    Sound.Url := 'castle-data:/sound/beep_mono.wav';

    Spatial.Sound := Sound;
    Spatial.Loop := true;
    SoundEngine.InternalPlay(Spatial, true, Vector3(1, 0, -2));
    NonSpatial.Sound := Sound;
    NonSpatial.Loop := true;
    SoundEngine.Play(NonSpatial);

    { a cave }
    SoundEngine.Reverb.DecayTime := 3.5;
    SoundEngine.Reverb.Density := 0.8;
    SoundEngine.Reverb.LateReverbPan := Vector3(0, 0, -0.5);
    SoundEngine.Reverb.Enabled := true;
    { out of range values are clamped (no OpenAL error) }
    SoundEngine.Reverb.DecayTime := 1000;
    SoundEngine.Reverb.Gain := -1;
    SoundEngine.Reverb.HFReference := 0;

    { let the sounds play with the reverb a moment }
    Sleep(300);

    { sounds started while the reverb is enabled }
    Spatial.Stop;
    SoundEngine.InternalPlay(Spatial, true, Vector3(-1, 0, -2));

    SoundEngine.Reverb.Enabled := false;
    SoundEngine.Reverb.DecayTime := 0.5;
    SoundEngine.Reverb.Enabled := true;
    Spatial.Stop;
    NonSpatial.Stop;
  finally
    SoundEngine.Reverb.Assign(SavedReverb);
    FreeAndNil(Spatial);
    FreeAndNil(NonSpatial);
    FreeAndNil(Sound);
    FreeAndNil(SavedReverb);
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
  end;
end;

procedure TTestCastleSoundEngine.TestLoadBufferException;
var
  Sound: TCastleSound;
begin
  // we want "Sound.Url := " on invalid file to cause exception, not warning
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  try
    Sound := TCastleSound.Create(nil);
    try
      try
        Sound.Url := 'castle-data:/sound/non-existing.wav';
        if not SoundEngine.IsContextOpenSuccess then
          Writeln('Sound backend cannot be initialized, TestLoadBufferException doesn''t really do anything')
        else
          Fail('Should have raised ESoundFileError 1');
      except on Exception do ; end;

      try
        Sound.Url := 'castle-data:/sound/non-existing.ogg';
        if not SoundEngine.IsContextOpenSuccess then
          Writeln('Sound backend cannot be initialized, TestLoadBufferException doesn''t really do anything')
        else
          Fail('Should have raised ESoundFileError 2');
      except on Exception do ; end;

      try
        Sound.Url := 'castle-data:/sound/invalid.wav';
        if not SoundEngine.IsContextOpenSuccess then
          Writeln('Sound backend cannot be initialized, TestLoadBufferException doesn''t really do anything')
        else
          Fail('Should have raised ESoundFileError 3');
      except on Exception do ; end;

      try
        Sound.Url := 'castle-data:/sound/invalid.ogg';
        if not SoundEngine.IsContextOpenSuccess then
          Writeln('Sound backend cannot be initialized, TestLoadBufferException doesn''t really do anything')
        else
          Fail('Should have raised ESoundFileError 4');
      except on Exception do ; end;
    finally
      FreeAndNil(Sound);
    end;
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
  end;
end;

procedure TTestCastleSoundEngine.TestNotPcmEncodingWarning;
begin
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  try
    if SoundEngine.IsContextOpenSuccess then
    begin
      try
        SoundEngine.RepositoryURL := 'castle-data:/sound/not_pcm_encoding/index.xml';
        Fail('Should have raised EWavLoadError');
      except
        on E: Exception do
        begin
          if Pos('Only uncompressed (PCM) WAV files are supported', E.Message) > 0 then
          begin
            // good, we expect this
          end else
            raise;
        end;
      end;
    end else
      Writeln('Sound backend cannot be initialized, TestNotPcmEncodingWarning doesn''t really do anything');
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
  end;
end;

procedure TTestCastleSoundEngine.TestImportancePriority;
var
  Params: TPlaySoundParameters;
begin
  Params := TPlaySoundParameters.Create;
  try
    AssertSameValue(0.5, Params.Priority);
  finally FreeAndNil(Params) end;
end;

procedure TTestCastleSoundEngine.TestSoundFromDataUri;
var
  Sound: TCastleSound;
  Stream: TStream;
begin
  if not SoundEngine.IsContextOpenSuccess then
  begin
    Writeln('Sound backend cannot be initialized, TestSoundFromDataUri ignored');
    Exit;
  end;

  Sound := TCastleSound.Create(nil);
  try
    Stream := Download('castle-data:/game/alien_sudden_pain.wav', []);
    try
      Sound.LoadFromStream(Stream, 'audio/x-wav');
    finally FreeAndNil(Stream) end;
    AssertSameValue(1.46, Sound.Duration, 0.01);
  finally FreeAndNil(Sound) end;

  Sound := TCastleSound.Create(nil);
  try
    Stream := Download('castle-data:/game/alien_sudden_pain.wav', []);
    try
      Sound.LoadFromStream(Stream, 'audio/wav');
    finally FreeAndNil(Stream) end;
    AssertSameValue(1.46, Sound.Duration, 0.01);
  finally FreeAndNil(Sound) end;
end;

initialization
  RegisterTest(TTestCastleSoundEngine);
end.
