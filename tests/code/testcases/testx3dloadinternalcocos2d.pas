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

{ Test X3DLoadInternalCocos2d unit. }
unit TestX3DLoadInternalCocos2d;

interface

uses CastleTester;

type
  TTestX3DLoadInternalCocos2d = class(TCastleTestCase)
  published
    procedure TestCocos2dParsingBool;
    procedure TestCocos2dParsingVectors;
  end;

implementation

uses SysUtils, DOM,
  CastleXmlUtils, X3DLoadInternalCocos2d;

procedure TTestX3DLoadInternalCocos2d.TestCocos2dParsingBool;
var
  D: TXMLDocument;
  RootE, E: TDOMElement;
begin
  D := TXMLDocument.Create;
  try
    RootE := D.CreateElement('root');
    { D.AppendChild not needed with FPC 3.2.2 or Delphi 10.2, but needed with Delphi 11.3 }
    D.AppendChild(RootE);

    E := RootE.CreateChild('true');
    AssertEquals(true, Cocos2dReadBool(E));

    E := RootE.CreateChild('false');
    AssertEquals(false, Cocos2dReadBool(E));

    E := RootE.CreateChild('something');
    AssertEquals(false, Cocos2dReadBool(E));
  finally FreeAndNil(D) end;
end;

procedure TTestX3DLoadInternalCocos2d.TestCocos2dParsingVectors;
var
  A, B, C, D: Integer;
  Valid: Boolean;
begin
  Valid := Cocos2dReadDual('{60,88}', A, B);
  AssertTrue(Valid);
  AssertEquals(A, 60);
  AssertEquals(B, 88);

  Valid := Cocos2dReadQuad('{{1,2},{60,88}}', A, B, C, D);
  AssertTrue(Valid);
  AssertEquals(A, 1);
  AssertEquals(B, 2);
  AssertEquals(C, 60);
  AssertEquals(D, 88);

  // Michalis got a testcase in private with:
  // <string>{0,0},{60,88}</string>
  Valid := Cocos2dReadQuad('{1,2},{60,88}', A, B, C, D);
  AssertTrue(Valid);
  AssertEquals(A, 1);
  AssertEquals(B, 2);
  AssertEquals(C, 60);
  AssertEquals(D, 88);
end;

initialization
  RegisterTest(TTestX3DLoadInternalCocos2d);
end.
