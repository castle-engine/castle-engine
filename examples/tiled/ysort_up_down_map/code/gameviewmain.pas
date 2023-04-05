{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Player: TCastleTransform;
    PlayerBody: TCastleScene;
  public
    PlayerRigidBody: TCastleRigidBody;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
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
  PlayerRigidBody := Player.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
end;

procedure TViewMain.Update(const SecondsPassed: single; var HandleInput: boolean);

  procedure ProcessPlayerVel;
  var
    Vel: TVector3;
  const
    PlayerSpeed: single = 100;
  begin
    // Vel := PlayerRigidBody.LinearVelocity;
    Vel := TVector3.Zero;
    if Container.Pressed.Items[keyA] or Container.Pressed.Items[keyArrowLeft] then
      Vel.X := -1
    else if Container.Pressed.Items[keyD] or Container.Pressed.Items[keyArrowRight] then
      Vel.X := 1;
    if Container.Pressed.Items[keyW] or Container.Pressed.Items[keyArrowUp] then
      Vel.Y := 1
    else if Container.Pressed.Items[keyS] or Container.Pressed.Items[keyArrowDown] then
      Vel.Y := -1;

    if Vel.X > 0 then PlayerBody.AutoAnimation := 'walk_right'
    else
    if Vel.X < 0 then PlayerBody.AutoAnimation := 'walk_left'
    else
    if Vel.Y > 0 then PlayerBody.AutoAnimation := 'walk_up'
    else
    if Vel.Y < 0 then PlayerBody.AutoAnimation := 'walk_down';

    Vel := Vel.Normalize;
    PlayerRigidBody.LinearVelocity := Vel * PlayerSpeed;

  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil,
    'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  ProcessPlayerVel;
end;

function TViewMain.Press(const Event: TInputPressRelease): boolean;
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
