{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main "playing game" view, where most of the game logic takes place. }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor, loaded from gameviewplay.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    Sphere: TCastleTransform;
    BulletBody: TCastleRigidBody;
    WalkNavigation: TCastleWalkNavigation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math,
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils,
  GameViewMenu;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;
end;

procedure TViewPlay.Stop;
begin
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonRight) then
  begin
    WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyEnter) then
  begin
    Sphere.Translation := Vector3(0, 2.17, -5.51);
    BulletBody.ApplyImpulse(Vector3(0,0,100), Vector3(0,0,0));
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    Container.View := ViewMenu;
    Exit(true);
  end;
end;

end.
