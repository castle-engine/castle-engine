{
  Copyright 2017-2017 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastleSoundEngine = class(TCastleBaseTestCase)
    procedure TestLoadBufferException;
  end;

implementation

uses CastleFilesUtils, CastleSoundEngine;

procedure TTestCastleSoundEngine.TestLoadBufferException;
begin
  try
    SoundEngine.LoadBuffer(ApplicationData('sound/non-existing.wav'));
    if not SoundEngine.ALActive then
      Writeln('WARNING: OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 1');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/non-existing.ogg'));
    if not SoundEngine.ALActive then
      Writeln('WARNING: OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 2');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/invalid.wav'));
    if not SoundEngine.ALActive then
      Writeln('WARNING: OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 3');
  except on ESoundFileError do ; end;

  try
    SoundEngine.LoadBuffer(ApplicationData('sound/invalid.ogg'));
    if not SoundEngine.ALActive then
      Writeln('WARNING: OpenAL cannot be initialized, TestLoadBufferException doesn''t really do anything')
    else
      Fail('Should have raised ESoundFileError 4');
  except on ESoundFileError do ; end;
end;

initialization
  RegisterTest(TTestCastleSoundEngine);
end.
