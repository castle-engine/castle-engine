// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestX3DLoadGltf" -*-
{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test X3DLoadInternalGltf unit. }
unit TestX3DLoadGltf;

{$I tests.inc}

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry,
  CastleTestCase;

type
  TTestX3DLoadGltf = class(TCastleTestCase)
  published
    procedure TestProcessStepTimeline;
  end;

implementation

uses X3DLoad, CastleVectors, X3DLoadInternalGltf, CastleUtils;

procedure TTestX3DLoadGltf.TestProcessStepTimeline;
var
  Times: TSingleList;
  Values: TVector3List;
begin
  Times := TSingleList.Create;
  Values := TVector3List.Create;
  try
    Times.AddRange([0 / 3, 1 / 3, 2 / 3]);
    Values.AddRange([
      Vector3(0, 0, 0),
      Vector3(1, 1, 1),
      Vector3(2, 2, 2)
    ]);

    ProcessStepTimeline(Times, Values);

    AssertEquals(5, Times.Count);
    AssertEquals(0 / 3, Times[0]);
    AssertEquals(1 / 3, Times[1]);
    AssertEquals(1 / 3, Times[2]);
    AssertEquals(2 / 3, Times[3]);
    AssertEquals(2 / 3, Times[4]);

    AssertEquals(5, Values.Count);
    AssertVectorEquals(Vector3(0, 0, 0), Values[0]);
    AssertVectorEquals(Vector3(0, 0, 0), Values[1]);
    AssertVectorEquals(Vector3(1, 1, 1), Values[2]);
    AssertVectorEquals(Vector3(1, 1, 1), Values[3]);
    AssertVectorEquals(Vector3(2, 2, 2), Values[4]);
 finally
    FreeAndNil(Times);
    FreeAndNil(Values);
  end;
end;

initialization
  RegisterTest(TTestX3DLoadGltf);
end.
