{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test of CastleUIControls unit. }
unit TestCastleUIControls;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  CastleBaseTestCase, CastleUIControls;

type
  TTestCastleUIControls = class(TCastleBaseTestCase)
  published
    procedure TestRectOverrides;
  end;

implementation

uses CastleRectangles;

type
  TNothingOverrriden = class(TCastleUserInterface)
  end;

  // TIntOverrriden = class(TCastleUserInterface)
  //   function Rect: TRectangle; override;
  // end;

  TFloatOverrriden = class(TCastleUserInterface)
    function Rect: TFloatRectangle; override;
  end;

// function TIntOverrriden.Rect: TRectangle;
// begin
//   Result := Rectangle(10, 20, 30, 40);
// end;

function TFloatOverrriden.Rect: TFloatRectangle;
begin
  Result := FloatRectangle(100, 200, 300, 400);
end;

procedure TTestCastleUIControls.TestRectOverrides;
var
  N: TNothingOverrriden;
  // I: TIntOverrriden;
  F: TFloatOverrriden;
begin
  N := TNothingOverrriden.Create(nil);
  try
    AssertRectsEqual(FloatRectangle(0, 0, 100, 100), N.Rect);
    AssertEquals(100, N.EffectiveWidth);
    AssertEquals(100, N.EffectiveHeight);

    N.Width := 0;
    N.Height := 0;
    AssertRectsEqual(TFloatRectangle.Empty, N.Rect);
    AssertEquals(0, N.EffectiveWidth);
    AssertEquals(0, N.EffectiveHeight);
  finally FreeAndNil(N); end;

  // I := TIntOverrriden.Create(nil);
  // try
  //   AssertRectsEqual(FloatRectangle(10, 20, 30, 40), I.Rect);
  // finally FreeAndNil(I); end;

  F := TFloatOverrriden.Create(nil);
  try
    AssertRectsEqual(FloatRectangle(100, 200, 300, 400), F.Rect);
    AssertEquals(300, F.EffectiveWidth);
    AssertEquals(400, F.EffectiveHeight);
  finally FreeAndNil(F); end;
end;

initialization
  RegisterTest(TTestCastleUIControls);
end.
