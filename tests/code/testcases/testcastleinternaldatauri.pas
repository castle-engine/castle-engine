{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalDataUri unit. }
unit TestCastleInternalDataUri;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleInternalDataUri = class(TCastleTestCase)
    { Test reading data URI encoded with base64. }
    procedure TestDataUri;
  end;

implementation

uses CastleDownload, CastleInternalSoundFile, CastleImages, CastleUriUtils,
  CastleSoundBase, CastleUtils;

procedure TTestCastleInternalDataUri.TestDataUri;
const
  ImageDataUri = {$I data_uri_embedded/bricks_base64.inc};
  WavDataUri = {$I data_uri_embedded/werewolf_howling_wav_base64.inc};
var
  S: TStream;
  Img: TCastleImage;
  SoundFile: TSoundFile;
begin
  S := Download(ImageDataUri);
  try
  finally FreeAndNil(S) end;

  Img := LoadImage(ImageDataUri);
  try
    AssertEquals(1024, Img.Width);
    AssertEquals(1024, Img.Height);
  finally FreeAndNil(Img) end;

  SoundFile := TSoundFile.Create(WavDataUri);
  try
    // Writeln('Loaded: ', UriCaption(SoundFile.URL));
    // Writeln('  Format: ', DataFormatToStr(SoundFile.DataFormat));
    // Writeln('  Frequency: ', SoundFile.Frequency);
    // Writeln('  Duration: ', SoundFile.Duration:1:2);

    AssertTrue(SoundFile.DataFormat = sfMono16);
    AssertSameValue(3.75, SoundFile.Duration, 0.01);
    AssertEquals(22050, SoundFile.Frequency);
    AssertEquals('data:audio/x-wav;base64,...', UriCaption(SoundFile.URL));
  finally FreeAndNil(SoundFile) end;
end;

initialization
  RegisterTest(TTestCastleInternalDataUri);
end.
