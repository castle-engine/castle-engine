{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game state where you actually play a game. }
unit GameStatePlay;

interface

uses Classes, CastleControls, CastleUIState, CastleOnScreenMenu,
  CastleViewport, CastleSceneCore, CastleScene,
  CastleCameras, CastleKeysMouse;

type
  TStatePlay = class(TUIState)
  strict private
    Background: TCastleRectangleControl;
    WalkNavigation: TCastleWalkNavigation;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    MapViewportRect: TCastleRectangleControl;
    MapViewport: TCastleViewport;
    ButtonBack: TCastleButton;
    LabelInstructions: TCastleLabel;
    procedure BackClick(Sender: TObject);
  public
    procedure Start; override;
    procedure Resume; override;
    procedure Pause; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

implementation

uses CastleVectors, CastleColors, CastleWindow, CastleUIControls,
  CastleFilesUtils, CastleUtils, CastleTriangles, CastleShapes,
  GameStateMainMenu, GameStateAskDialog;

{ TStatePlay ------------------------------------------------------------- }

procedure TStatePlay.Start;
begin
  inherited;

  Background := TCastleRectangleControl.Create(FreeAtStop);
  Background.Color := Black;
  Background.FullSize := true;
  InsertFront(Background);

  Scene := TCastleScene.Create(FreeAtStop);
  Scene.Load('castle-data:/level1.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Scene.RenderOptions.PhongShading := true; // looks better
  { zombie sprites are rendered using blending, and you can see multiple
    sprites sometimes at once. So we need sorting, to render them correctly.
    This is actually the default now. }
  //Scene.RenderOptions.BlendingSort := bs3D;

  WalkNavigation := TCastleWalkNavigation.Create(FreeAtStop);
  WalkNavigation.MoveSpeed := 10;
  { turn off head bobbing, it makes a feeling that sprites sometimes "tremble" }
  WalkNavigation.HeadBobbing := 0;

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.AutoCamera := true;
  Viewport.FullSize := false;
  Viewport.Left := 10;
  Viewport.Bottom := 10;
  Viewport.Width := 800;
  Viewport.Height := 748;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
  Viewport.Navigation := WalkNavigation;
  InsertFront(Viewport);

  MapViewportRect := TCastleRectangleControl.Create(FreeAtStop);
  MapViewportRect.FullSize := false;
  MapViewportRect.Left := 820;
  MapViewportRect.Bottom := 10;
  MapViewportRect.Width := 256;
  MapViewportRect.Height := 256;
  MapViewportRect.Color := Silver;
  InsertFront(MapViewportRect);

  MapViewport := TCastleViewport.Create(FreeAtStop);
  MapViewport.FullSize := false;
  MapViewport.Left := 10;
  MapViewport.Bottom := 10;
  MapViewport.Width := 236;
  MapViewport.Height := 236;
  MapViewport.Items := Viewport.Items;
  MapViewport.Transparent := true;
  MapViewport.NavigationType := ntNone;
  MapViewport.AutoCamera := false;
  MapViewport.Camera.SetView(
    Vector3(5, 92.00, 0.99),
    Vector3(0, -1, 0),
    Vector3(0, 0, 1));
  MapViewportRect.InsertFront(MapViewport);

  LabelInstructions := TCastleLabel.Create(FreeAtStop);
  LabelInstructions.Caption :=
    'Walk around using AWSD and arrow keys.' + NL +
    'Click on a zombie sprite to show a dialog.';
  LabelInstructions.Anchor(vpTop, -100);
  LabelInstructions.Anchor(hpRight, -10);
  LabelInstructions.Color := Yellow;
  InsertFront(LabelInstructions);

  ButtonBack := TCastleButton.Create(FreeAtStop);
  ButtonBack.Caption := 'Back to Main Menu';
  ButtonBack.OnClick := @BackClick;
  ButtonBack.Anchor(vpTop, -10);
  ButtonBack.Anchor(hpRight, -10);
  InsertFront(ButtonBack);
end;

procedure TStatePlay.Resume;
begin
  inherited;

  { Without setting ForceCaptureInput, inputs are only passed
    when mouse cursor is over the Viewport.

    Usually you set such things in Start method, but here we need to be
    prepared that we may be covered by the transparent StateAskDialog state.
    When StateAskDialog is active, we do *not* want to forcefully capture input
    (it would allow user to move by mouse dragging when StateAskDialog is open).
    So we set this in Resume, and turn off in Pause. }
  StateContainer.ForceCaptureInput := Viewport.Navigation;
end;

procedure TStatePlay.Pause;
begin
  StateContainer.ForceCaptureInput := nil;
  inherited;
end;

procedure TStatePlay.BackClick(Sender: TObject);
begin
  TUIState.Current := TStateMainMenu.CreateUntilStopped
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;
var
  Triangle: PTriangle;
  Male: Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    Triangle := Viewport.TriangleHit;
    if (Triangle <> nil) and // we clicked on something that has triangle information (e.g. because it has Spatial with ssDynamicCollisions)
       (Triangle^.MaterialInfo <> nil)  and // the clicked triangle has a material information
       ( (Triangle^.MaterialInfo.Node.X3DName = 'MA_female_zombie_material') or
         (Triangle^.MaterialInfo.Node.X3DName = 'MA_male_zombie_material')) then
    begin
      Male := Triangle^.MaterialInfo.Node.X3DName = 'MA_male_zombie_material';
      TUIState.Push(TStateAskDialog.CreateUntilStopped(Male));
    end;
  end;
end;

end.
