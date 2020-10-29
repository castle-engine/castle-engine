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
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonAnimationWalk: TCastleButton;
    ButtonAnimationAttack: TCastleButton;
    ButtonAnimationStop: TCastleButton;
    ButtonWeaponAxe: TCastleButton;
    ButtonWeaponSword: TCastleButton;
    ButtonWeaponNone: TCastleButton;
    SceneHumanoid: TCastleScene;
    SceneAxe: TCastleScene;
    SceneSword: TCastleScene;
    procedure ClickAnimationWalk(Sender: TObject);
    procedure ClickAnimationAttack(Sender: TObject);
    procedure ClickAnimationStop(Sender: TObject);
    procedure ClickWeaponAxe(Sender: TObject);
    procedure ClickWeaponSword(Sender: TObject);
    procedure ClickWeaponNone(Sender: TObject);
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
  ButtonAnimationWalk := UiOwner.FindRequiredComponent('ButtonAnimationWalk') as TCastleButton;
  ButtonAnimationAttack := UiOwner.FindRequiredComponent('ButtonAnimationAttack') as TCastleButton;
  ButtonAnimationStop := UiOwner.FindRequiredComponent('ButtonAnimationStop') as TCastleButton;
  ButtonWeaponAxe := UiOwner.FindRequiredComponent('ButtonWeaponAxe') as TCastleButton;
  ButtonWeaponSword := UiOwner.FindRequiredComponent('ButtonWeaponSword') as TCastleButton;
  ButtonWeaponNone := UiOwner.FindRequiredComponent('ButtonWeaponNone') as TCastleButton;
  SceneHumanoid := UiOwner.FindRequiredComponent('SceneHumanoid') as TCastleScene;
  SceneAxe := UiOwner.FindRequiredComponent('SceneAxe') as TCastleScene;
  SceneSword := UiOwner.FindRequiredComponent('SceneSword') as TCastleScene;

  ButtonAnimationWalk.OnClick := @ClickAnimationWalk;
  ButtonAnimationAttack.OnClick := @ClickAnimationAttack;
  ButtonAnimationStop.OnClick := @ClickAnimationStop;
  ButtonWeaponAxe.OnClick := @ClickWeaponAxe;
  ButtonWeaponSword.OnClick := @ClickWeaponSword;
  ButtonWeaponNone.OnClick := @ClickWeaponNone;

  { initially hold axe }
  ClickWeaponAxe(nil);
end;

procedure TStateMain.ClickAnimationWalk(Sender: TObject);
begin
  SceneHumanoid.PlayAnimation('walk', true);
end;

procedure TStateMain.ClickAnimationAttack(Sender: TObject);
begin
  SceneHumanoid.PlayAnimation('attack', true);
end;

procedure TStateMain.ClickAnimationStop(Sender: TObject);
begin
  SceneHumanoid.StopAnimation;
end;

procedure TStateMain.ClickWeaponAxe(Sender: TObject);
begin
  SceneAxe.Exists := true;
  SceneSword.Exists := false;
end;

procedure TStateMain.ClickWeaponSword(Sender: TObject);
begin
  SceneAxe.Exists := false;
  SceneSword.Exists := true;
end;

procedure TStateMain.ClickWeaponNone(Sender: TObject);
begin
  SceneAxe.Exists := false;
  SceneSword.Exists := false;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
