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

{ Test CastleInternalDelphiUtils unit. }
unit TestCastleInternalDelphiUtils;

interface

{$ifdef FPC}
{ This tests Delphi-specific CastleInternalDelphiUtils,
  ignore if compiled with FPC. }
implementation
{$else}

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleInternalDelphiUtils = class(TCastleTestCase)
    procedure TestSimpleKeyToString;
  end;

implementation

uses CastleInternalDelphiUtils, CastleKeysMouse, CastleStringUtils;

procedure TTestCastleInternalDelphiUtils.TestSimpleKeyToString;
begin
  AssertEquals('a', SimpleKeyToString(keyA, []));
  AssertEquals('A', SimpleKeyToString(keyA, [ssShift]));
  AssertEquals('=', SimpleKeyToString(keyEqual, []));
  AssertEquals('+', SimpleKeyToString(keyEqual, [ssShift]));
  AssertEquals('', SimpleKeyToString(keyNone, []));
  AssertEquals(CharBackSpace, SimpleKeyToString(keyBackSpace, []));
  AssertEquals(CharBackSpace, SimpleKeyToString(keyBackSpace, [ssShift]));
  AssertEquals('0', SimpleKeyToString(key0, []));
  AssertEquals('0', SimpleKeyToString(key0, [ssShift]));
end;

initialization
  RegisterTest(TTestCastleInternalDelphiUtils);
{$endif}
end.
