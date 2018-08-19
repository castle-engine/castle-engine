{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleVectors,
  CastleFilesUtils, CastleSceneCore, CastleSceneManager, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleCameras, X3DNodes;

var
  Window: TCastleWindow;

{ buttons to change headlight ------------------------------------------------ }

type
  TButtons = class(TCastleVerticalGroup)
    ButtonHeadlightOn, ButtonHeadlightOff: TCastleButton;
    ButtonHeadlightDirectional: TCastleButton;
    ButtonHeadlightSpot: TCastleButton;
    ButtonHeadlightSpotSharp: TCastleButton;
    ButtonHeadlightPoint: TCastleButton;

    procedure ClickOn(Sender: TObject);
    procedure ClickOff(Sender: TObject);
    procedure ClickDirectional(Sender: TObject);
    procedure ClickSpot(Sender: TObject);
    procedure ClickSpotSharp(Sender: TObject);
    procedure ClickPoint(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

constructor TButtons.Create(AOwner: TComponent);
begin
  inherited;

  ButtonHeadlightOn := TCastleButton.Create(Self);
  ButtonHeadlightOn.Caption := 'Headlight: On';
  ButtonHeadlightOn.OnClick := @ClickOn;
  InsertFront(ButtonHeadlightOn);

  ButtonHeadlightOff := TCastleButton.Create(Self);
  ButtonHeadlightOff.Caption := 'Headlight: Off';
  ButtonHeadlightOff.OnClick := @ClickOff;
  InsertFront(ButtonHeadlightOff);

  ButtonHeadlightDirectional := TCastleButton.Create(Self);
  ButtonHeadlightDirectional.Caption := 'Headlight Type: Directional (Like Sun)';
  ButtonHeadlightDirectional.OnClick := @ClickDirectional;
  InsertFront(ButtonHeadlightDirectional);

  ButtonHeadlightSpot := TCastleButton.Create(Self);
  ButtonHeadlightSpot.Caption := 'Headlight Type: Spot (Cone with Direction)';
  ButtonHeadlightSpot.OnClick := @ClickSpot;
  InsertFront(ButtonHeadlightSpot);

  ButtonHeadlightSpotSharp := TCastleButton.Create(Self);
  ButtonHeadlightSpotSharp.Caption := 'Headlight Type: Spot (Cone with Direction), Sharp Edge';
  ButtonHeadlightSpotSharp.OnClick := @ClickSpotSharp;
  InsertFront(ButtonHeadlightSpotSharp);

  ButtonHeadlightPoint := TCastleButton.Create(Self);
  ButtonHeadlightPoint.Caption := 'Headlight Type: Point (Uniform In All Directions)';
  ButtonHeadlightPoint.OnClick := @ClickPoint;
  InsertFront(ButtonHeadlightPoint);
end;

procedure TButtons.ClickOn(Sender: TObject);
begin
  Window.SceneManager.UseHeadlight := hlOn;
end;

procedure TButtons.ClickOff(Sender: TObject);
begin
  Window.SceneManager.UseHeadlight := hlOff;
end;

procedure TButtons.ClickDirectional(Sender: TObject);
begin
  Window.SceneManager.HeadlightNode := TDirectionalLightNode.Create;
end;

procedure TButtons.ClickSpot(Sender: TObject);
var
  Spot: TSpotLightNode;
begin
  Spot := TSpotLightNode.Create;
  Spot.AmbientIntensity := 0.5; // make stuff outside light also a bit brighter
  Spot.CutOffAngle := 0.4;
  Spot.BeamWidth := 0.35;
  Window.SceneManager.HeadlightNode := Spot;
end;

procedure TButtons.ClickSpotSharp(Sender: TObject);
var
  Spot: TSpotLightNode;
begin
  Spot := TSpotLightNode.Create;
  Spot.AmbientIntensity := 0.5; // make stuff outside light also a bit brighter
  Spot.CutOffAngle := 0.4;
  Spot.BeamWidth := 0.4;
  Window.SceneManager.HeadlightNode := Spot;
end;

procedure TButtons.ClickPoint(Sender: TObject);
begin
  Window.SceneManager.HeadlightNode := TPointLightNode.Create;
end;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  LevelScene: TCastleScene;
  Buttons: TButtons;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Load level }
  LevelScene := TCastleScene.Create(Application);
  LevelScene.Load(ApplicationData('level.x3d'));
  LevelScene.Spatial := [ssRendering, ssDynamicCollisions];
  LevelScene.ProcessEvents := true;
  LevelScene.Attributes.PhongShading := true; // prettier lights

  Window.SceneManager.Items.Add(LevelScene);
  Window.SceneManager.MainScene := LevelScene;

  { level.x3d, exported from Blender, has initially camera in Examine
    mode, with non-perfect gravity up.
    Change camera properties to be good for walking. }
  Window.SceneManager.NavigationType := ntWalk;
  Window.SceneManager.WalkCamera.MoveSpeed := 5;
  Window.SceneManager.WalkCamera.GravityUp := Vector3(0, 1, 0);

  { Make (initially) headlight "on".
    Default value of UseHeadlight is hlMainScene, which makes it dependent
    on MainScene settings. (see TCastleSceneManager.UseHeadlight docs for details) }
  Window.SceneManager.UseHeadlight := hlOn;

  Buttons := TButtons.Create(Application);
  Buttons.Anchor(vpBottom, 10);
  Buttons.Anchor(hpLeft, 10);
  Window.Controls.InsertFront(Buttons);
end;

initialization
  ApplicationProperties.ApplicationName := 'headlight_test';
  InitializeLog;

  Application.OnInitialize := @ApplicationInitialize;
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
