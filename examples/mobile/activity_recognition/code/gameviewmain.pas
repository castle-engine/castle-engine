{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleViewport, CastleUIControls,
  CastleActivityRecognition;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelStatus: TCastleLabel;
    ButtonStartListening: TCastleButton;
    ButtonStopListening: TCastleButton;
  private
    ActivityRecognition: TActivityRecognition;
    procedure ClickStartListening(Sender: TObject);
    procedure ClickStopListening(Sender: TObject);
    procedure ActivityRecognitionChange(Sender: TObject);
    procedure UpdateStatus;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleComponentSerialize;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  ButtonStartListening.OnClick := {$ifdef FPC}@{$endif} ClickStartListening;
  ButtonStopListening.OnClick := {$ifdef FPC}@{$endif} ClickStopListening;

  ActivityRecognition := TActivityRecognition.Create(FreeAtStop);
  ActivityRecognition.OnChange := {$ifdef FPC}@{$endif} ActivityRecognitionChange;
  ActivityRecognition.Start;

  UpdateStatus;
end;

procedure TViewMain.UpdateStatus;
var
  S: String;
begin
  S := 'Started:' + BoolToStr(ActivityRecognition.Started, true) + NL + NL;
  if not ActivityRecognition.ActivityValid then
    S := S + 'Not yet detected anything.' + NL
  else
    S := S + 'Detected:' + NL +
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

procedure TViewMain.ClickStartListening(Sender: TObject);
begin
  ActivityRecognition.Start;
  UpdateStatus;
end;

procedure TViewMain.ClickStopListening(Sender: TObject);
begin
  ActivityRecognition.Stop;
  UpdateStatus;
end;

procedure TViewMain.ActivityRecognitionChange(Sender: TObject);
begin
  UpdateStatus;
end;

end.
