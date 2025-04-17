{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalTools unit. }
unit TestCastleInternalTools;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleInternalTools = class(TCastleTestCase)
  published
    procedure TestStringToDependency;
  end;

implementation

uses CastleInternalTools;

procedure TTestCastleInternalTools.TestStringToDependency;
var
  Deps: TProjectDependencies;
begin
  AssertTrue(depPng = StringToDependency('png'));
  Deps := TProjectDependencies.Create;
  try
    Deps.Dependencies := [depPng];
    Deps.CloseDependencies;
    AssertTrue([depPng, depZlib] = Deps.Dependencies);
  finally FreeAndNil(Deps) end;
end;

initialization
  RegisterTest(TTestCastleInternalTools);
end.
