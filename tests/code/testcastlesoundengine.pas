// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleSoundEngine" -*-
{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
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
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry, CastleTestCase;

type
  TTestCastleSoundEngine = class(TCastleTestCase)
  private
    procedure WavNonPcmWarning(const Category, S: string);
  published
    procedure TestLoadBufferException;
    procedure TestNotPcmEncodingWarning;
    procedure TestImportancePriority;
  end;

implementation

uses CastleFilesUtils, CastleSoundEngine, CastleApplicationProperties, CastleDownload;

procedure TTestCastleSoundEngine.TestLoadBufferException;
begin
  try
    SoundEngine.LoadBuffer('castle-data:/sound/non-existing.wav');
    if not SoundEngine.IsContextOpenSuccess then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 1');
  except on EDownloadError{ESoundFileError} do ; end;

  try
    SoundEngine.LoadBuffer('castle-data:/sound/non-existing.ogg');
    if not SoundEngine.IsContextOpenSuccess then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 2');
  except on EDownloadError{ESoundFileError} do ; end;

  try
    SoundEngine.LoadBuffer('castle-data:/sound/invalid.wav');
    if not SoundEngine.IsContextOpenSuccess then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 3');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer('castle-data:/sound/invalid.ogg');
    if not SoundEngine.IsContextOpenSuccess then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 4');
  except on ESoundFileError do ; end;
end;

type
  EWavNonPcm = class(Exception);

procedure TTestCastleSoundEngine.WavNonPcmWarning(const Category, S: string);
begin
  if Pos('Loading WAV files not in PCM format not implemented', S) <> 0 then
    raise EWavNonPcm.Create('Good, we have warning: ' + S);
end;

procedure TTestCastleSoundEngine.TestNotPcmEncodingWarning;
begin
  if SoundEngine.IsContextOpenSuccess then
  begin
    ApplicationProperties.OnWarning.Add(@WavNonPcmWarning);
    try
      SoundEngine.RepositoryURL := 'castle-data:/sound/not_pcm_encoding/index.xml';
      Fail('Should have raised EWavNonPcm');
    except
      on EWavNonPcm do ; // good, we expect this
    end;
    ApplicationProperties.OnWarning.Remove(@WavNonPcmWarning);
  end else
    Writeln('OpenAL cannot be initialized, TestNotPcmEncodingWarning doesn''t really do anything');
end;

procedure TTestCastleSoundEngine.TestImportancePriority;
var
  Params: TPlaySoundParameters;
begin
  Params := TPlaySoundParameters.Create;
  try
    AssertSameValue(0.0, Params.Priority);
    AssertEquals(0.0, Params.Importance);

    Params.Importance := LevelEventSoundImportance;
    AssertSameValue(1.0, Params.Priority);
    AssertEquals(LevelEventSoundImportance, Params.Importance);

    Params.Importance := PlayerSoundImportance;
    AssertSameValue(0.31, Params.Priority, 0.01);
    AssertEquals(PlayerSoundImportance, Params.Importance);

    Params.Importance := DefaultSoundImportance;
    AssertSameValue(0.1, Params.Priority, 0.01);
    AssertEquals(DefaultSoundImportance, Params.Importance);
  finally FreeAndNil(Params) end;
end;

initialization
  RegisterTest(TTestCastleSoundEngine);
end.
