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

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelSceneUnderMouse: TCastleLabel;
    LabelSceneClicked: TCastleLabel;
    MainViewport: TCastleViewport;
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
var
  SceneName: String;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if MainViewport.TransformUnderMouse <> nil then
    SceneName := MainViewport.TransformUnderMouse.Name
  else
    SceneName := 'none';
  LabelSceneUnderMouse.Caption := 'Scene Under Mouse/Touch Position: ' + SceneName;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  SceneName: String;
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

  if Event.IsMouseButton(buttonLeft) then
  begin
    if MainViewport.TransformUnderMouse <> nil then
      SceneName := MainViewport.TransformUnderMouse.Name
    else
      SceneName := 'none';
    LabelSceneClicked.Caption := 'Scene Last Clicked/Touched: ' + SceneName;
    Exit(true); // event was handled
  end;
end;

end.
