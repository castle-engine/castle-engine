{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleScene, CastleTimeUtils;

type
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    LabelFps: TCastleLabel;
    MainLight: TCastlePointLight;

    Time: TFloatTime;
    procedure UpdateMainLightLocation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses Math,
  CastleUtils { Delphi needs this to define Float };

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
  MainLight := DesignedComponent('MainLight') as TCastlePointLight;

  Time := 0;
  UpdateMainLightLocation;
end;

procedure TStateMain.UpdateMainLightLocation;
const
  Radius = 2;
var
  S, C: Float;
begin
  SinCos(Time * 2, S, C);
  MainLight.Translation := Vector3(S * Radius, C * Radius + 1, 2);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  Time := Time + Container.Fps.SecondsPassed;
  UpdateMainLightLocation;
end;

end.
