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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleControls, CastleCameras,
  CastleUIControls, CastleKeysMouse, CastleTransform, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Button1: TCastleButton;
    MyBull: TCastleTransform;
    MainViewport: TCastleViewport;
    WalkNavigation1: TCastleWalkNavigation;
  private
    procedure Click1(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleLog, CastleScene, CastleSceneCore, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

procedure TViewMain.Click1(Sender: TObject);
begin
  MyBull.Translation := MyBull.Translation + Vector3(0, 0.5, 0);
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Start Idle animation on all chickens with random initial time,
    to make their animations unsynchronized -- looks more natural. }
  procedure UnsynchronizeChickenAnimations;
  var
    I: Integer;
    Params: TPlayAnimationParameters;
    ChickenScene: TCastleScene;
  begin
    for I := 1 to 6 do
    begin
      ChickenScene := DesignedComponent('SceneChickenRig' + IntToStr(I)) as TCastleScene;
      Params := TPlayAnimationParameters.Create;
      try
        Params.Name := 'Idle';
        Params.InitialTime := RandomFloatRange(0, ChickenScene.AnimationDuration('Idle'));
        Params.Loop := true;
        ChickenScene.PlayAnimation(Params);
      finally FreeAndNil(Params) end;
    end;
  end;

begin
  inherited;
  Button1.OnClick := {$ifdef FPC}@{$endif} Click1;
  UnsynchronizeChickenAnimations;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if Container.Pressed[keyP] then
    MyBull.Translation := MyBull.Translation + Vector3(4 * SecondsPassed, 0, 0);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  Hit: TCastleTransform;
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

  { Allow user to "escape" from mouse look, e.g. to click on button. }
  if Event.IsMouseButton(buttonRight) then
  begin
    WalkNavigation1.MouseLook := not WalkNavigation1.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyX) then
  begin
    WritelnLog('some log!');
    Exit(true);
  end;

  if Event.IsMouseButton(buttonLeft) or Event.IsKey(keyZ) then
  begin
    Hit := MainViewport.TransformHit(MainViewport.RenderRect.Center, true);
    if (Hit <> nil) and
       (Hit.RigidBody <> nil) then
    begin
      Hit.RigidBody.ApplyImpulse(
        MainViewport.Camera.WorldDirection * 50,
        MainViewport.Camera.WorldTranslation
      );
      Exit(true);
    end;
  end;
end;

end.
