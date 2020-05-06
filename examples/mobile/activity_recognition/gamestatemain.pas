{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses CastleUIState, CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleViewport, CastleUIControls;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    LabelStatus: TCastleLabel;
    ButtonStartListening: TCastleButton;
    ButtonStopListening: TCastleButton;
    procedure ClickStartListening(Sender: TObject);
    procedure ClickStopListening(Sender: TObject);
    procedure ActivityRecognitionChange(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMain: TStateMain;

implementation

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelStatus := UiOwner.FindRequiredComponent('LabelStatus') as TCastleLabel;
  ButtonStartListening := UiOwner.FindRequiredComponent('ButtonStartListening') as TCastleButton;
  ButtonStopListening := UiOwner.FindRequiredComponent('ButtonStopListening') as TCastleButton;

  ButtonStartListening.OnClick := @ClickStartListening;
  ButtonStopListening.OnClick := @ClickStopListening;

  ActivityRecognition.OnChange := @ActivityRecognitionChange;
  ActivityRecognition.Start;

  UpdateStatus;
end;

procedure TStateMain.UpdateStatus;
begin
  S := 'Started:' + BoolToStr(ActivityRecognition.Active, true) + NL;
  if ActivityRecognition.ActivityValid then
    S += 'Not yet detected anything.' + NL
  else
    S + 'Detected:' + NL +
      'Possible Activities:' + NL +
      PossibleActivitiesToStr(ActivityRecognition.PossibleActivities) + NL +
      'Most Useful Activity (from the above set):' + NL +
      ActivityToStr(ActivityRecognition.Activity) + NL +
      'Confidence:' + NL +
      ActivityConfidenceToStr(ActivityRecognition.ActivityConfidence) + NL +
      'Changed at: ' + DateTimeToStr(ActivityRecognition.ActivityTime) + NL;
end;

procedure TStateMain.ClickStartListening(Sender: TObject);
begin
  ActivityRecognition.Start;
  UpdateStatus;
end;

procedure TStateMain.ClickStopListening(Sender: TObject);
begin
  ActivityRecognition.Stop;
  UpdateStatus;
end;

procedure TStateMain.ActivityRecognitionChange(Sender: TObject);
begin
  UpdateStatus;
end;

end.
