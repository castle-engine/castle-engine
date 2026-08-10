{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleTransformManipulate unit. }
unit TestCastleTransformManipulate;

interface

uses Classes, SysUtils,
  CastleTester;

type
  TTestCastleTransformManipulate = class(TCastleTestCase)
  published
    procedure TestSelectedCountAndFree;
  end;

implementation

uses CastleTransformManipulate, CastleTransform, CastleControls;

procedure TTestCastleTransformManipulate.TestSelectedCountAndFree;
var
  B: TCastleButton;
  T1, T2, T3: TCastleTransform;
  Manipulate: TCastleTransformManipulate;
begin
  // initialize all variables to nil, to allow easy finally clause later
  Manipulate := nil;
  T1 := nil;
  T2 := nil;
  T3 := nil;
  B := nil;

  try
    Manipulate := TCastleTransformManipulate.Create(nil);
    T1 := TCastleTransform.Create(nil);
    T2 := TCastleTransform.Create(nil);
    T3 := TCastleTransform.Create(nil);
    B := TCastleButton.Create(nil);

    AssertEquals(0, Manipulate.SelectedCount);

    Manipulate.SetSelected(nil); // allowed

    AssertEquals(0, Manipulate.SelectedCount);

    Manipulate.SetSelected([T1, nil, B]);

    AssertEquals(1, Manipulate.SelectedCount);
    AssertTrue(Manipulate.Selected[0] = T1);

    Manipulate.SetSelected([T1, nil, B, T3]);

    AssertEquals(2, Manipulate.SelectedCount);
    AssertTrue(Manipulate.Selected[0] = T1);
    AssertTrue(Manipulate.Selected[1] = T3);

    FreeAndNil(T1);

    AssertEquals(1, Manipulate.SelectedCount);
    AssertTrue(Manipulate.Selected[0] = T3);
  finally
    FreeAndNil(Manipulate);
    FreeAndNil(T1);
    FreeAndNil(T2);
    FreeAndNil(T3);
    FreeAndNil(B);
  end;
end;

initialization
  RegisterTest(TTestCastleTransformManipulate);
end.
