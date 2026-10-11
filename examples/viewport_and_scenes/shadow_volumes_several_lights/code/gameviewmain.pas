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

  Three colored point lights circle above a few objects.
  Each light has Shadows = true, so with ShadowVolumesMaxLights >= 3
  every object casts three colored shadows.
  With ShadowVolumesMaxLights = 1 only the first light casts shadows,
  the others are turned off in its shadow (the classic behavior). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleTransform, CastleCameras;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    Viewport: TCastleViewport;
    Lights: array [0..2] of TCastlePointLight;
    LightsTime: Single;
    LightsMove: Boolean;
    LabelInfo: TCastleLabel;
    procedure BuildScene;
    procedure UpdateLights;
    procedure UpdateInfo;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleColors, CastleUtils, CastleRenderOptions;

const
  LightColors: array [0..2] of TCastleColorRGB = (
    (X: 1.0; Y: 0.25; Z: 0.25),
    (X: 0.25; Y: 1.0; Z: 0.25),
    (X: 0.3; Y: 0.4; Z: 1.0)
  );
  LightsRadius = 5.0;
  LightsHeight = 4.0;

procedure TViewMain.BuildScene;

  { Add a primitive. Primitives are closed (2-manifold),
    so they can cast shadow volumes. }
  procedure AddPrimitive(const P: TCastleAbstractPrimitive; const Position: TVector3);
  begin
    P.Translation := Position;
    P.Material := pmPhong;
    P.Color := Vector4(0.9, 0.9, 0.9, 1);
    Viewport.Items.Add(P);
  end;

var
  Floor: TCastleBox;
  Box: TCastleBox;
  Sphere: TCastleSphere;
  Cylinder: TCastleCylinder;
  Navigation: TCastleExamineNavigation;
  FillLight: TCastlePointLight;
  I: Integer;
begin
  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.BackgroundColor := Vector4(0.05, 0.05, 0.08, 1);
  { Shadow volumes from all 3 lights. }
  Viewport.ShadowVolumesMaxLights := 3;
  InsertFront(Viewport);

  Viewport.Camera.SetView(Vector3(0, 9, 13), Vector3(0, -0.6, -1), Vector3(0, 1, 0));
  Viewport.Camera.ProjectionFar := 0; // infinite, required by shadow volumes

  Navigation := TCastleExamineNavigation.Create(FreeAtStop);
  Viewport.InsertFront(Navigation);

  { Floor receives shadows. Doesn't need to cast (it would not be visible). }
  Floor := TCastleBox.Create(FreeAtStop);
  Floor.Size := Vector3(20, 0.2, 20);
  Floor.Translation := Vector3(0, -0.1, 0);
  Floor.CastShadows := false;
  { Phong lighting, without gamma correction: see the README about
    the color space when adding light passes. }
  Floor.Material := pmPhong;
  Viewport.Items.Add(Floor);

  Box := TCastleBox.Create(FreeAtStop);
  Box.Size := Vector3(1.5, 2, 1.5);
  AddPrimitive(Box, Vector3(-2, 1, 0));

  Sphere := TCastleSphere.Create(FreeAtStop);
  Sphere.Radius := 0.9;
  AddPrimitive(Sphere, Vector3(2, 0.9, 0.5));

  Cylinder := TCastleCylinder.Create(FreeAtStop);
  Cylinder.Radius := 0.4;
  Cylinder.Height := 3;
  AddPrimitive(Cylinder, Vector3(0, 1.5, -2));

  for I := 0 to High(Lights) do
  begin
    Lights[I] := TCastlePointLight.Create(FreeAtStop);
    Lights[I].Color := LightColors[I];
    Lights[I].Intensity := 12;
    { Shadows = true sets shadowVolumes and shadowVolumesMain on the light. }
    Lights[I].Shadows := true;
    Viewport.Items.Add(Lights[I]);
  end;

  { A dim light without shadows (Shadows = false):
    it lights everything, also in the shadows of the other lights. }
  FillLight := TCastlePointLight.Create(FreeAtStop);
  FillLight.Translation := Vector3(0, 12, 0);
  FillLight.Intensity := 15;
  Viewport.Items.Add(FillLight);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.Anchor(hpLeft, 10);
  LabelInfo.Anchor(vpTop, -10);
  LabelInfo.Color := White;
  LabelInfo.FontSize := 20;
  InsertFront(LabelInfo);
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
  S := Format('ShadowVolumesMaxLights: %d (press M to change)', [Viewport.ShadowVolumesMaxLights]) + NL;
  for I := 0 to High(Lights) do
    S := S + Format('Light %d: %s (press %d to toggle)', [I + 1,
      Iff(Lights[I].Exists, 'on', 'off'), I + 1]) + NL;
  S := S +
    Format('Shadow volumes: %s (press S)', [Iff(Viewport.ShadowVolumes, 'on', 'off')]) + NL +
    Format('Show shadow volumes: %s (press V)', [Iff(Viewport.ShadowVolumesRender, 'yes', 'no')]) + NL +
    Format('Lights move: %s (press Space)', [Iff(LightsMove, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
  LabelInfo.Caption := S;
end;

procedure TViewMain.Start;
begin
  inherited;
  LightsMove := true;
  BuildScene;
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
    if Viewport.ShadowVolumesMaxLights = 1 then
      Viewport.ShadowVolumesMaxLights := 3
    else
      Viewport.ShadowVolumesMaxLights := 1;
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keyS) then
  begin
    Viewport.ShadowVolumes := not Viewport.ShadowVolumes;
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keyV) then
  begin
    Viewport.ShadowVolumesRender := not Viewport.ShadowVolumesRender;
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
