{
  Copyright 2021-2023 Michalis Kamburelis.

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

uses Classes, Math,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleVectors;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ImagePlayer: TCastleImageControl;
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
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  MoveSpeed = 800;
var
  PlayerPosition: TVector2;
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  PlayerPosition := ImagePlayer.Translation;

  if Container.Pressed[keyArrowLeft] then
    PlayerPosition := PlayerPosition + Vector2(-MoveSpeed * SecondsPassed, 0);
  if Container.Pressed[keyArrowRight] then
    PlayerPosition := PlayerPosition + Vector2( MoveSpeed * SecondsPassed, 0);
  if Container.Pressed[keyArrowDown] then
    PlayerPosition := PlayerPosition + Vector2(0, -MoveSpeed * SecondsPassed);
  if Container.Pressed[keyArrowUp] then
    PlayerPosition := PlayerPosition + Vector2(0,  MoveSpeed * SecondsPassed);

  { update player position to fall down }
  PlayerPosition.Y := Max(PlayerPosition.Y - SecondsPassed * 400, 0);
  ImagePlayer.Translation := PlayerPosition;
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

  if Event.IsKey(keySpace) then
  begin
    ImagePlayer.Color := Vector4(Random, Random, Random, 1);
    Exit(true); // event was handled
  end;

  if Event.IsMouseButton(buttonLeft) then
  begin
    ImagePlayer.Translation := ImagePlayer.Parent.ContainerToLocalPosition(Event.Position);
    Exit(true); // event was handled
  end;
end;

end.
