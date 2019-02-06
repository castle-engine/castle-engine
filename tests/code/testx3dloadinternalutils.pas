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

{ Test X3DLoadInternalUtils unit. }
unit TestX3DLoadInternalUtils;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestX3DLoadInternalUtils = class(TTestCase)
  published
    procedure TestToX3DName;
  end;

implementation

uses X3DLoadInternalUtils;

procedure TTestX3DLoadInternalUtils.TestToX3DName;
begin
  AssertEquals('', ToX3DName(''));
  AssertEquals('_1', ToX3DName('1'));
  AssertEquals('a_sdsd_XYZ_123', ToX3DName('a_sdsd XYZ-123'));
end;

initialization
  RegisterTest(TTestX3DLoadInternalUtils);
end.
