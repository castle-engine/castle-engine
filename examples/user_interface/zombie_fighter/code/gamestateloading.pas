{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ State with sample loading. }
unit GameStateLoading;

interface

uses Classes, CastleControls, CastleUIState;

type
  TStateLoading = class(TUIState)
  strict private
    const
      FakeLoadingStepsCount = 17;
    var
      LabelPercent: TCastleLabel;
      { Variable that simulates loading progress,
        we will grow it from 0 to FakeLoadingStepsCount during loading. }
      FakeLoadingSteps: Cardinal;
      LoadNextPart: Boolean;
  public
    procedure Start; override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateLoading: TStateLoading;

implementation

uses SysUtils,
  CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize,
  GameStatePlay;

{ TStateLoading ------------------------------------------------------------- }

procedure TStateLoading.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_loading.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelPercent := UiOwner.FindRequiredComponent('LabelPercent') as TCastleLabel;

  FakeLoadingSteps := 0;
  LoadNextPart := false; // do not load before some render happened
end;

procedure TStateLoading.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  if LoadNextPart then
  begin
    if FakeLoadingSteps = FakeLoadingStepsCount then
    begin
      // finished, go to StatePlay
      TUIState.Current := StatePlay;
    end else
    begin
      { Fake loading. Just do something time-consuming there. }
      Sleep(100);
      Inc(FakeLoadingSteps);
      LabelPercent.Caption := IntToStr(Round(100 * FakeLoadingSteps / FakeLoadingStepsCount)) + '%';
      LoadNextPart := false; // do not load before some render happened, to show new LabelPercent
    end;
  end;
end;

procedure TStateLoading.Render;
begin
  inherited;
  // to show the progress, our loading occasionally waits for render to happen
  LoadNextPart := true;
end;

end.
