// -*- compile-command: "./test_single_testcase.sh TTestX3DLoadInternalUtils" -*-
{
  Copyright 2019-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test X3DLoadInternalUtils unit.

  Leave UTF-8 BOM in this file for Delphi!
  To let it interpret the string literals (like using Chinese) in this file correctly. }
unit TestX3DLoadInternalUtils;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestX3DLoadInternalUtils = class(TCastleTestCase)
  published
    procedure TestX3DNameEncode;
  end;

implementation

uses X3DLoadInternalUtils, CastleUnicode;

procedure TTestX3DLoadInternalUtils.TestX3DNameEncode;
begin
  AssertEquals('', EncodeX3DName(''));
  AssertEquals('', DecodeX3DName(DecodeX3DName('')));

  AssertEquals('CastleEncoded_1', EncodeX3DName('1'));
  AssertEquals('1', DecodeX3DName(EncodeX3DName('1')));

  AssertEquals('CastleEncoded_a_sdsd$32$XYZ-123', EncodeX3DName('a_sdsd XYZ-123'));
  AssertEquals('a_sdsd XYZ-123', DecodeX3DName(EncodeX3DName('a_sdsd XYZ-123')));

  AssertEquals('CastleEncoded_a_sdsd$32$$36$$32$XYZ-123', EncodeX3DName('a_sdsd $ XYZ-123'));
  AssertEquals('a_sdsd $ XYZ-123', DecodeX3DName(EncodeX3DName('a_sdsd $ XYZ-123')));

  AssertEquals(8, StringLength('1MyName样'));
  AssertEquals('CastleEncoded_1MyName$26679$', EncodeX3DName('1MyName样'));
  AssertEquals('1MyName样', DecodeX3DName('CastleEncoded_1MyName$26679$'));

  // MyName样 is OK as X3D name, so it passes without any change
  AssertEquals('MyName样', EncodeX3DName('MyName样'));
  AssertEquals('MyName样', DecodeX3DName('MyName样'));
end;

initialization
  RegisterTest(TTestX3DLoadInternalUtils);
end.
