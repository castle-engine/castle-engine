{
  Copyright 2026 Popa Marius Adrian.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow volumes from several lights (TCastleViewport.ShadowVolumesMaxLights).

  The design (data/gameviewmain.castle-user-interface) has three colored
  point lights with Shadows = true, a few objects that cast shadows,
  and a viewport with ShadowVolumesMaxLights = 3,
  so every object casts three colored shadows.
  With ShadowVolumesMaxLights = 1 only the first light casts shadows,
  the others are turned off in its shadow (the classic behavior).
  Here we move the lights around in circles. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport: TCastleViewport;
    Light1, Light2, Light3: TCastlePointLight;
    LabelInfo: TCastleLabel;
  private
    Lights: array [0..2] of TCastlePointLight;
    LightsTime: Single;
    LightsMove: Boolean;
    procedure UpdateLights;
    procedure UpdateInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils;

const
  LightsRadius = 5.0;
  LightsHeight = 4.0;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.UpdateLights;
var
  I: Integer;
  Angle: Single;
begin
  for I := 0 to High(Lights) do
  begin
    Angle := LightsTime * 0.5 + I * 2 * Pi / Length(Lights);
    Lights[I].Translation := Vector3(
      Cos(Angle) * LightsRadius, LightsHeight, Sin(Angle) * LightsRadius);
  end;
end;

procedure TViewMain.UpdateInfo;
var
  S: String;
  I: Integer;
begin
  S := Format('ShadowVolumesMaxLights: %d (press M to change)', [MainViewport.ShadowVolumesMaxLights]) + NL;
  for I := 0 to High(Lights) do
    S := S + Format('Light %d: %s (press %d to toggle)', [I + 1,
      Iff(Lights[I].Exists, 'on', 'off'), I + 1]) + NL;
  S := S +
    Format('Shadow volumes: %s (press S)', [Iff(MainViewport.ShadowVolumes, 'on', 'off')]) + NL +
    Format('Show shadow volumes: %s (press V)', [Iff(MainViewport.ShadowVolumesRender, 'yes', 'no')]) + NL +
    Format('Lights move: %s (press Space)', [Iff(LightsMove, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
  LabelInfo.Caption := S;
end;

procedure TViewMain.Start;
begin
  inherited;
  Lights[0] := Light1;
  Lights[1] := Light2;
  Lights[2] := Light3;
  LightsMove := true;
  UpdateLights;
  UpdateInfo;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  if LightsMove then
  begin
    LightsTime := LightsTime + SecondsPassed;
    UpdateLights;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to High(Lights) do
    if Event.IsKey(TKey(Ord(key1) + I)) then
    begin
      Lights[I].Exists := not Lights[I].Exists;
      UpdateInfo;
      Exit(true);
    end;

  if Event.IsKey(keyM) then
  begin
    if MainViewport.ShadowVolumesMaxLights = 1 then
      MainViewport.ShadowVolumesMaxLights := 3
    else
      MainViewport.ShadowVolumesMaxLights := 1;
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keyS) then
  begin
    MainViewport.ShadowVolumes := not MainViewport.ShadowVolumes;
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keyV) then
  begin
    MainViewport.ShadowVolumesRender := not MainViewport.ShadowVolumesRender;
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keySpace) then
  begin
    LightsMove := not LightsMove;
    UpdateInfo;
    Exit(true);
  end;
end;

end.
