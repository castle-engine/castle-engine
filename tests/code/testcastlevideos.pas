// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestVideos" -*-
{
  Copyright 2013-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleVideos unit. }
unit TestCastleVideos;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif}, CastleVideos;

type
  TTestVideos = class(TCastleTestCase)
  published
    procedure TestLoad;
  end;

implementation

uses CastleFilesUtils;

procedure TTestVideos.TestLoad;
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile('castle-data:/videos/video1_@counter(4).png');
    AssertTrue(Video.Count = 3);
    Video.LoadFromFile('castle-data:/videos/video2_@counter(4).png');
    AssertTrue(Video.Count = 3);
    Video.LoadFromFile('castle-data:/videos/video_single.png');
    AssertTrue(Video.Count = 1);

    try
      Video.LoadFromFile('castle-data:/videos/video_not_existing.png');
      Fail('Should fail');
    except
      on E: Exception do
      begin
//        Writeln(E.Message);
      end;
    end;

    try
      Video.LoadFromFile('castle-data:/videos/video_not_existing@counter(1).png');
      Fail('Should fail');
    except
      on E: Exception do
      begin
//        Writeln(E.Message);
        AssertTrue(Pos('cannot be loaded', E.Message) <> 0);
      end;
    end;
  finally FreeAndNil(Video) end;
end;

{$ifndef CASTLE_TESTER}
initialization
 RegisterTest(TTestVideos);
{$endif}
end.
