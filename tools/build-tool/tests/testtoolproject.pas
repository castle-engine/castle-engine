{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatic tests of ToolProject unit. }
unit TestToolProject;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleTestCase,
  ToolProject;

type
  TTestToolProject = class(TCastleTestCase)
  published
    procedure TestMacros;
  end;

implementation

uses CastleStringUtils, CastleURIUtils, CastleFilesUtils, CastleVectors,
  CastleConfig;

procedure TTestToolProject.TestMacros;
var
  P: TCastleProject;
begin
  P := TCastleProject.Create(URIToFilenameSafe(ResolveCastleDataURL('castle-data:/test_project/')));
  try
    AssertEquals('My Test Project', P.ReplaceMacros('${CAPTION}'));
    AssertEquals('test_project', P.ReplaceMacros('${NAME}'));
    AssertEquals('blabla My Test Project asdasdas', P.ReplaceMacros('blabla ${CAPTION} asdasdas'));
    AssertEquals('blabla asdkoko', P.ReplaceMacros('blabla ${IF true}asd${ELSE}koko${ENDIF}'));
    AssertEquals('blabla 3', P.ReplaceMacros('blabla ${CALCULATE 1+2}'));
    AssertEquals('blabla ohyes:My Test Project', P.ReplaceMacros('blabla ${CALCULATE if(${CAPTION} <> '''', ''ohyes:'' + ${CAPTION}, ''undefined caption'')}'));
  finally FreeAndNil(P) end;
end;

initialization
  RegisterTest(TTestToolProject);
end.
