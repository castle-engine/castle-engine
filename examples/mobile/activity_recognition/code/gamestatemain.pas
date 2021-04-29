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

uses Classes,
  CastleUIState, CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleViewport, CastleUIControls,
  CastleActivityRecognition;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    ActivityRecognition: TActivityRecognition;
    LabelStatus: TCastleLabel;
    ButtonStartListening: TCastleButton;
    ButtonStopListening: TCastleButton;
    procedure ClickStartListening(Sender: TObject);
    procedure ClickStopListening(Sender: TObject);
    procedure ActivityRecognitionChange(Sender: TObject);
    procedure UpdateStatus;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleUtils, CastleComponentSerialize;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelStatus := DesignedComponent('LabelStatus') as TCastleLabel;
  ButtonStartListening := DesignedComponent('ButtonStartListening') as TCastleButton;
  ButtonStopListening := DesignedComponent('ButtonStopListening') as TCastleButton;

  ButtonStartListening.OnClick := @ClickStartListening;
  ButtonStopListening.OnClick := @ClickStopListening;

  ActivityRecognition := TActivityRecognition.Create(FreeAtStop);
  ActivityRecognition.OnChange := @ActivityRecognitionChange;
  ActivityRecognition.Start;

  UpdateStatus;
end;

procedure TStateMain.UpdateStatus;
var
  S: String;
begin
  S := 'Started:' + BoolToStr(ActivityRecognition.Started, true) + NL + NL;
  if not ActivityRecognition.ActivityValid then
    S += 'Not yet detected anything.' + NL
  else
    S += 'Detected:' + NL +
      NL +
      'Possible Activities:' + NL +
      PossibleActivitiesToStr(ActivityRecognition.PossibleActivities, ',') + NL +
      NL +
      'Most Useful Activity' + NL +
      '(from the Possible set):' + NL +
      ActivityToStr(ActivityRecognition.Activity) + NL +
      NL +
      'Confidence:' + NL +
      ActivityConfidenceToStr(ActivityRecognition.ActivityConfidence) + NL +
      NL +
      'Changed at: ' + FormatDateTime('yyyy"-"mm"-"dd" "tt', ActivityRecognition.ActivityTime) + NL;
  LabelStatus.Caption := S;
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
