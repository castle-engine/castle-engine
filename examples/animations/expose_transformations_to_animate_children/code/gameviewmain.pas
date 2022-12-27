{
  Copyright 2020-2022 Michalis Kamburelis.

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
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
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
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ButtonAnimationWalk := DesignedComponent('ButtonAnimationWalk') as TCastleButton;
  ButtonAnimationAttack := DesignedComponent('ButtonAnimationAttack') as TCastleButton;
  ButtonAnimationStop := DesignedComponent('ButtonAnimationStop') as TCastleButton;
  ButtonWeaponAxe := DesignedComponent('ButtonWeaponAxe') as TCastleButton;
  ButtonWeaponSword := DesignedComponent('ButtonWeaponSword') as TCastleButton;
  ButtonWeaponNone := DesignedComponent('ButtonWeaponNone') as TCastleButton;
  SceneHumanoid := DesignedComponent('SceneHumanoid') as TCastleScene;
  SceneAxe := DesignedComponent('SceneAxe') as TCastleScene;
  SceneSword := DesignedComponent('SceneSword') as TCastleScene;

  ButtonAnimationWalk.OnClick := {$ifdef FPC}@{$endif}ClickAnimationWalk;
  ButtonAnimationAttack.OnClick := {$ifdef FPC}@{$endif}ClickAnimationAttack;
  ButtonAnimationStop.OnClick := {$ifdef FPC}@{$endif}ClickAnimationStop;
  ButtonWeaponAxe.OnClick := {$ifdef FPC}@{$endif}ClickWeaponAxe;
  ButtonWeaponSword.OnClick := {$ifdef FPC}@{$endif}ClickWeaponSword;
  ButtonWeaponNone.OnClick := {$ifdef FPC}@{$endif}ClickWeaponNone;

  { initially hold axe }
  ClickWeaponAxe(nil);
end;

procedure TViewMain.ClickAnimationWalk(Sender: TObject);
begin
  SceneHumanoid.PlayAnimation('walk', true);
end;

procedure TViewMain.ClickAnimationAttack(Sender: TObject);
begin
  SceneHumanoid.PlayAnimation('attack', true);
end;

procedure TViewMain.ClickAnimationStop(Sender: TObject);
begin
  SceneHumanoid.StopAnimation;
end;

procedure TViewMain.ClickWeaponAxe(Sender: TObject);
begin
  SceneAxe.Exists := true;
  SceneSword.Exists := false;
end;

procedure TViewMain.ClickWeaponSword(Sender: TObject);
begin
  SceneAxe.Exists := false;
  SceneSword.Exists := true;
end;

procedure TViewMain.ClickWeaponNone(Sender: TObject);
begin
  SceneAxe.Exists := false;
  SceneSword.Exists := false;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
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
