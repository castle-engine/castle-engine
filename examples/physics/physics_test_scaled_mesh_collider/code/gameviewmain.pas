{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleViewport, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    ButtonDropSphere: TCastleButton;
    ButtonScale1, ButtonScale5, ButtonScale10: TCastleButton;
    SceneLevel: TCastleScene;
  private
    procedure ClickDropSphere(Sender: TObject);
    procedure ClickScale1(Sender: TObject);
    procedure ClickScale5(Sender: TObject);
    procedure ClickScale10(Sender: TObject);
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
  CastleUtils, CastleTransform;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonDropSphere.OnClick := {$ifdef FPC}@{$endif} ClickDropSphere;
  ButtonScale1.OnClick := {$ifdef FPC}@{$endif} ClickScale1;
  ButtonScale5.OnClick := {$ifdef FPC}@{$endif} ClickScale5;
  ButtonScale10.OnClick := {$ifdef FPC}@{$endif} ClickScale10;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyS) then
  begin
    ClickDropSphere(nil);
    Exit(true); // key was handled
  end;
end;

procedure TViewMain.ClickDropSphere(Sender: TObject);
var
  Sphere: TCastleSphere;
begin
  Sphere := TCastleSphere.Create(FreeAtStop);
  Sphere.Color := Vector4(
    RandomFloatRange(0.5, 1.0),
    RandomFloatRange(0.5, 1.0),
    RandomFloatRange(0.5, 1.0),
    1
  );
  Sphere.Translation := Vector3(
    RandomFloatRange(-1, 1),
    10,
    RandomFloatRange(-1, 1)
  );
  { Add TCastleRigidBody and TCastleSphereCollider instances,
    with all properties set to default values, to have the sphere
    affected by physics. }
  Sphere.AddBehavior(TCastleRigidBody.Create(Sphere));
  Sphere.AddBehavior(TCastleSphereCollider.Create(Sphere));
  MainViewport.Items.Add(Sphere);
end;

procedure TViewMain.ClickScale1(Sender: TObject);
begin
  SceneLevel.Scale := Vector3(1, 1, 1);
end;

procedure TViewMain.ClickScale5(Sender: TObject);
begin
  SceneLevel.Scale := Vector3(5, 5, 5);
end;

procedure TViewMain.ClickScale10(Sender: TObject);
begin
  SceneLevel.Scale := Vector3(10, 10, 10);
end;

end.
