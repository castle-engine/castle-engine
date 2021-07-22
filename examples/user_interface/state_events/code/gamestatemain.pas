{
  Copyright 2021-2021 Michalis Kamburelis.

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

uses Classes, Math,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleVectors;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ImagePlayer: TCastleImageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

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
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ImagePlayer := DesignedComponent('ImagePlayer') as TCastleImageControl;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  MoveSpeed = 800;
var
  PlayerPosition: TVector2;
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  PlayerPosition := ImagePlayer.AnchorDelta;

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
  ImagePlayer.AnchorDelta := PlayerPosition;
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

  if Event.IsKey(keySpace) then
  begin
    ImagePlayer.Color := Vector4(Random, Random, Random, 1);
    Exit(true); // event was handled
  end;

  if Event.IsMouseButton(buttonLeft) then
  begin
    ImagePlayer.AnchorDelta := ImagePlayer.Parent.ContainerToLocalPosition(Event.Position);
    Exit(true); // event was handled
  end;
end;

end.
