{
  Copyright 2020-2021 Michalis Kamburelis.

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
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    type
      TTestMode = (tmMove, tmBox, tmSphere, tmRay);
    var
      { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
      LabelFps: TCastleLabel;
      SceneMovingBox: TCastleScene;
      SceneMovingSphere: TCastleScene;
      SceneMovingRay: TCastleScene;
      ButtonTestMove: TCastleButton;
      ButtonTestBox: TCastleButton;
      ButtonTestSphere: TCastleButton;
      ButtonTestRay: TCastleButton;

      FTestMode: TTestMode;
    procedure SetTestMode(const Value: TTestMode);
    procedure ClickTestMove(Sender: TObject);
    procedure ClickTestBox(Sender: TObject);
    procedure ClickTestSphere(Sender: TObject);
    procedure ClickTestRay(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  SceneMovingBox := UiOwner.FindRequiredComponent('SceneMovingBox') as TCastleScene;
  SceneMovingSphere := UiOwner.FindRequiredComponent('SceneMovingSphere') as TCastleScene;
  SceneMovingRay := UiOwner.FindRequiredComponent('SceneMovingRay') as TCastleScene;
  ButtonTestMove := UiOwner.FindRequiredComponent('ButtonTestMove') as TCastleButton;
  ButtonTestBox := UiOwner.FindRequiredComponent('ButtonTestBox') as TCastleButton;
  ButtonTestSphere := UiOwner.FindRequiredComponent('ButtonTestSphere') as TCastleButton;
  ButtonTestRay := UiOwner.FindRequiredComponent('ButtonTestRay') as TCastleButton;

  ButtonTestMove.OnClick := @ClickTestMove;
  ButtonTestBox.OnClick := @ClickTestBox;
  ButtonTestSphere.OnClick := @ClickTestSphere;
  ButtonTestRay.OnClick := @ClickTestRay;

  SetTestMode(tmMove);
end;

procedure TStateMain.SetTestMode(const Value: TTestMode);
begin
  FTestMode := Value;

  ButtonTestMove.Pressed := FTestMode = tmMove;
  ButtonTestBox.Pressed := FTestMode = tmBox;
  ButtonTestSphere.Pressed := FTestMode = tmSphere;
  ButtonTestRay.Pressed := FTestMode = tmRay;

  SceneMovingBox.Exists := FTestMode = tmBox;
  SceneMovingSphere.Exists := FTestMode in [tmMove, tmSphere];
  SceneMovingRay.Exists := FTestMode = tmRay;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickTestMove(Sender: TObject);
begin
  SetTestMode(tmMove);
end;

procedure TStateMain.ClickTestBox(Sender: TObject);
begin
  SetTestMode(tmBox);
end;

procedure TStateMain.ClickTestSphere(Sender: TObject);
begin
  SetTestMode(tmSphere);
end;

procedure TStateMain.ClickTestRay(Sender: TObject);
begin
  SetTestMode(tmRay);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  // TODO
  case FTestMode of
    tmMove: ;
    tmBox: ;
    tmSphere: ;
    tmRay: ;
  end;
end;

end.
