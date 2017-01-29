{
  Copyright 2016-2017 Michalis Kamburelis.

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
  CastleSceneManager, CastleSceneCore, CastleScene,
  CastleCameras, CastleKeysMouse;

type
  TStatePlay = class(TUIState)
  strict private
    SimpleBackground: TCastleSimpleBackground;
    SceneManager: TCastleSceneManager;
    Scene: TCastleScene;
    ViewportRect: TCastleRectangleControl;
    Viewport: TCastleViewport;
    ButtonBack: TCastleButton;
    LabelInstructions: TCastleLabel;
    procedure BackClick(Sender: TObject);
  public
    procedure Start; override;
    procedure Resume; override;
    procedure Pause; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses CastleVectors, CastleColors, CastleWindow, CastleUIControls,
  CastleFilesUtils, CastleUtils, X3DTriangles,
  GameStateMainMenu, GameStateAskDialog;

{ TStatePlay ------------------------------------------------------------- }

procedure TStatePlay.Start;
begin
  inherited;

  SimpleBackground := TCastleSimpleBackground.Create(FreeAtStop);
  SimpleBackground.Color := Black;
  InsertFront(SimpleBackground);

  Scene := TCastleScene.Create(FreeAtStop);
  Scene.Load(ApplicationData('level1.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  { zombie sprites are rendered using blending, and you can see multiple
    sprites sometimes at once. So we need sorting, to render them correctly.
    This is actually the default now. }
  //Scene.Attributes.BlendingSort := bs3D;

  SceneManager := TCastleSceneManager.Create(FreeAtStop);
  SceneManager.FullSize := false;
  SceneManager.Left := 10;
  SceneManager.Bottom := 10;
  SceneManager.Width := 800;
  SceneManager.Height := 748;
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;
  (SceneManager.RequiredCamera as TUniversalCamera).NavigationType := ntWalk;
  (SceneManager.RequiredCamera as TUniversalCamera).Walk.MoveSpeed := 10;
  InsertFront(SceneManager);

  ViewportRect := TCastleRectangleControl.Create(FreeAtStop);
  ViewportRect.FullSize := false;
  ViewportRect.Left := 820;
  ViewportRect.Bottom := 10;
  ViewportRect.Width := 256;
  ViewportRect.Height := 256;
  ViewportRect.Color := Silver;
  InsertFront(ViewportRect);

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := false;
  Viewport.Left := 10;
  Viewport.Bottom := 10;
  Viewport.Width := 236;
  Viewport.Height := 236;
  Viewport.SceneManager := SceneManager;
  Viewport.Transparent := true;
  (Viewport.RequiredCamera as TUniversalCamera).NavigationType := ntNone;
  (Viewport.RequiredCamera as TUniversalCamera).SetView(
    Vector3Single(5, 92.00, 0.99),
    Vector3Single(0, -1, 0),
    Vector3Single(0, 0, 1));
  ViewportRect.InsertFront(Viewport);

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
    when mouse cursor is over the SceneManager.

    Usually you set such things in Start method, but here we need to be
    prepared that we may be covered by the transparent StateAskDialog state.
    When StateAskDialog is active, we do *not* want to forcefully capture input
    (it would allow user to move by mouse dragging when StateAskDialog is open).
    So we set this in Resume, and turn off in Pause. }
  StateContainer.ForceCaptureInput := SceneManager;
end;

procedure TStatePlay.Pause;
begin
  StateContainer.ForceCaptureInput := nil;
  inherited;
end;

procedure TStatePlay.BackClick(Sender: TObject);
begin
  TUIState.Current := StateMainMenu;
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;
var
  Triangle: PTriangle;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    Triangle := SceneManager.TriangleHit;
    if (Triangle <> nil) and
       ( (Triangle^.MaterialNode.X3DName = 'MA_female_zombie_material') or
         (Triangle^.MaterialNode.X3DName = 'MA_male_zombie_material')) then
    begin
      StateAskDialog.Male := Triangle^.MaterialNode.X3DName = 'MA_male_zombie_material';
      TUIState.Push(StateAskDialog);
    end;
  end;
end;

end.
