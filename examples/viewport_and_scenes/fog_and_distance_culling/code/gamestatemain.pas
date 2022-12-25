{
  Copyright 2003-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state, where most of the application logic takes place.  }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    LabelInfo: TCastleLabel;
    SceneMain: TCastleScene;
    MainViewport: TCastleViewport;
    Fog1: TCastleFog;

    FFogCulling: Boolean;
    procedure SetFogCulling(const Value: Boolean);
    property FogCulling: Boolean read FFogCulling write SetFogCulling;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  X3DNodes, CastleUtils, CastleStringUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.SetFogCulling(const Value: Boolean);
begin
  if FFogCulling <> Value then
  begin
    FFogCulling := Value;

    if FogCulling then
    begin
      MainViewport.Fog := Fog1;
      SceneMain.DistanceCulling := Fog1.VisibilityRange;
    end else
    begin
      MainViewport.Fog := nil;
      SceneMain.DistanceCulling := 0;
    end;
  end;
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelInfo := DesignedComponent('LabelInfo') as TCastleLabel;
  SceneMain := DesignedComponent('SceneMain') as TCastleScene;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  Fog1 := DesignedComponent('Fog1') as TCastleFog;

  FogCulling := true;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelInfo.Caption := Format(
    'Rendered Shapes: %d / %d' + NL +
    'Fog culling: %s (toggle by Ctrl+F)' + NL +
    'Frustum culling of each shape: %s (toggle by Ctrl+C)', [
    MainViewport.Statistics.ShapesRendered,
    MainViewport.Statistics.ShapesVisible,
    BoolToStr(FogCulling, true),
    BoolToStr(SceneMain.ShapeFrustumCulling, true)
  ]);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(CtrlF) then
    FogCulling := not FogCulling;

  if Event.IsKey(CtrlC) then
    SceneMain.ShapeFrustumCulling := not SceneMain.ShapeFrustumCulling;
end;

end.
