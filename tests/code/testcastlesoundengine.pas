{
  Copyright 2017-2018 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleTestCase;

type
  TTestCastleSoundEngine = class(TCastleTestCase)
  private
    procedure WavNonPcmWarning(Sender: TObject; const Category, S: string);
  published
    procedure TestLoadBufferException;
    procedure TestNotPcmEncodingWarning;
  end;

implementation

uses CastleFilesUtils, CastleSoundEngine, CastleApplicationProperties;

procedure TTestCastleSoundEngine.TestLoadBufferException;
begin
  try
    SoundEngine.LoadBuffer(ApplicationData('sound/non-existing.wav'));
    if not SoundEngine.ALActive then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 1');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/non-existing.ogg'));
    if not SoundEngine.ALActive then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 2');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/invalid.wav'));
    if not SoundEngine.ALActive then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 3');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/invalid.ogg'));
    if not SoundEngine.ALActive then
      Writeln('OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 4');
  except on ESoundFileError do ; end;
end;

type
  EWavNonPcm = class(Exception);

procedure TTestCastleSoundEngine.WavNonPcmWarning(Sender: TObject; const Category, S: string);
begin
  if Pos('Loading WAV files not in PCM format not implemented', S) <> 0 then
    raise EWavNonPcm.Create('Good, we have warning: ' + S);
end;

procedure TTestCastleSoundEngine.TestNotPcmEncodingWarning;
begin
  if SoundEngine.ALActive then
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

initialization
  RegisterTest(TTestCastleSoundEngine);
end.
