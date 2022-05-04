// -*- compile-command: "./test_single_testcase.sh TTestLocalizationGetText" -*-
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleLocalizationGetText unit. }
unit TestCastleLocalizationGetText;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestLocalizationGetText = class(TCastleTestCase)
  published
    {$ifdef FPC} // CastleLocalizationGetText not available for Delphi yet
    procedure TestTranslateUi;
    {$endif}
  end;

implementation

{$ifdef FPC} // CastleLocalizationGetText not available for Delphi yet

uses CastleLocalizationGetText, CastleClassUtils, CastleUIControls,
  CastleComponentSerialize, CastleControls;

procedure TTestLocalizationGetText.TestTranslateUi;

  procedure CheckEnglish;
  var
    UiOwner: TComponent;
    //Ui: TCastleUserInterface;
    L: TCastleLabel;
    E: TCastleEdit;
  begin
    UiOwner := TComponent.Create(nil);
    try
      {Ui := }UserInterfaceLoad('castle-data:/localization_gettext/gamestatemain.castle-user-interface', UiOwner);
      L := UiOwner.FindRequiredComponent('Label1') as TCastleLabel;
      AssertEquals('My Label', L.Caption);
      E := UiOwner.FindRequiredComponent('Edit') as TCastleEdit;
      AssertEquals('Initial Edit Contents', E.Text);
    finally FreeAndNil(UiOwner) end;
  end;

  procedure CheckPolish;
  var
    UiOwner: TComponent;
    //Ui: TCastleUserInterface;
    L: TCastleLabel;
    E: TCastleEdit;
  begin
    UiOwner := TComponent.Create(nil);
    try
      {Ui := }UserInterfaceLoad('castle-data:/localization_gettext/gamestatemain.castle-user-interface', UiOwner);
      L := UiOwner.FindRequiredComponent('Label1') as TCastleLabel;
      AssertEquals('Moja Etykieta', L.Caption);
      E := UiOwner.FindRequiredComponent('Edit') as TCastleEdit;
      AssertEquals('Początkowa zawartość', E.Text);
    finally FreeAndNil(UiOwner) end;
  end;

begin
  CheckEnglish;
  TranslateAllDesigns('castle-data:/localization_gettext/user_interface.pl.mo');
  CheckPolish;
  TranslateAllDesigns('');
  CheckEnglish;
end;
{$endif}

initialization
  RegisterTest(TTestLocalizationGetText);
end.
