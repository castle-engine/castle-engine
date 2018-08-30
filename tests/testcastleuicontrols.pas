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

  TIntOverrriden = class(TCastleUserInterface)
    function Rect: TRectangle; override;
  end;

  TFloatOverrriden = class(TCastleUserInterface)
    function FloatRect: TFloatRectangle; override;
  end;

function TIntOverrriden.Rect: TRectangle;
begin
  Result := Rectangle(10, 20, 30, 40);
end;

function TFloatOverrriden.FloatRect: TFloatRectangle;
begin
  Result := FloatRectangle(100, 200, 300, 400);
end;

procedure TTestCastleUIControls.TestRectOverrides;
var
  N: TNothingOverrriden;
  I: TIntOverrriden;
  F: TFloatOverrriden;
begin
  N := TNothingOverrriden.Create(nil);
  try
    AssertRectsEqual(TRectangle.Empty, N.Rect);
    AssertRectsEqual(TFloatRectangle.Empty, N.FloatRect);
  finally FreeAndNil(N); end;

  I := TIntOverrriden.Create(nil);
  try
    AssertRectsEqual(Rectangle(10, 20, 30, 40), I.Rect);
    AssertRectsEqual(FloatRectangle(10, 20, 30, 40), I.FloatRect);
  finally FreeAndNil(I); end;

  F := TFloatOverrriden.Create(nil);
  try
    AssertRectsEqual(Rectangle(100, 200, 300, 400), F.Rect);
    AssertRectsEqual(FloatRectangle(100, 200, 300, 400), F.FloatRect);
  finally FreeAndNil(F); end;
end;

initialization
  RegisterTest(TTestCastleUIControls);
end.
