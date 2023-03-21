{
  Copyright 2016-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game view where you actually play a game. }
unit GameViewPlay;

interface

uses Classes, CastleControls, CastleOnScreenMenu, CastleUIControls,
  CastleViewport, CastleSceneCore, CastleScene,
  CastleCameras, CastleKeysMouse;

type
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport, MapViewport: TCastleViewport;
    ButtonBack: TCastleButton;
  strict private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Resume; override;
    procedure Pause; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

implementation

uses CastleVectors, CastleColors,
  CastleFilesUtils, CastleUtils, CastleTriangles, CastleShapes,
  CastleComponentSerialize,
  GameViewMainMenu, GameViewAskDialog;

{ TViewPlay ------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;

  { turn off head bobbing, it makes a feeling that sprites sometimes "tremble" }
//  WalkNavigation.HeadBobbing := 0;

  // see https://castle-engine.io/multiple_viewports_to_display_one_world
  MapViewport.Items.Remove(MapViewport.Camera);
  MapViewport.Items := MainViewport.Items;
  MapViewport.Items.Add(MapViewport.Camera);

  MapViewport.Camera.SetView(
    Vector3(5, 92.00, 0.99),
    Vector3(0, -1, 0),
    Vector3(0, 0, 1));

  ButtonBack.OnClick := {$ifdef FPC}@{$endif}ClickBack;
end;

procedure TViewPlay.Resume;
begin
  inherited;

  { Without setting ForceCaptureInput, inputs are only passed
    when mouse cursor is over the Viewport.

    Usually you set such things in Start method, but here we need to be
    prepared that we may be covered by the transparent ViewAskDialog view.
    When ViewAskDialog is active, we do *not* want to forcefully capture input
    (it would allow user to move by mouse dragging when ViewAskDialog is open).
    So we set this in Resume, and turn off in Pause. }
  Container.ForceCaptureInput := MainViewport.Navigation;
end;

procedure TViewPlay.Pause;
begin
  Container.ForceCaptureInput := nil;
  inherited;
end;

procedure TViewPlay.ClickBack(Sender: TObject);
begin
  Container.View := TViewMainMenu.CreateUntilStopped;
end;

function TViewPlay.Press(const Event: TInputPressRelease): boolean;
var
  Triangle: PTriangle;
  Male: Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    Triangle := MainViewport.TriangleHit;
    if (Triangle <> nil) and // we clicked on something that has triangle information (e.g. because it has Spatial with ssDynamicCollisions)
       (Triangle^.MaterialInfo <> nil)  and // the clicked triangle has a material information
       ( (Triangle^.MaterialInfo.Node.X3DName = 'MA_female_zombie_material') or
         (Triangle^.MaterialInfo.Node.X3DName = 'MA_male_zombie_material')) then
    begin
      Male := Triangle^.MaterialInfo.Node.X3DName = 'MA_male_zombie_material';
      Container.PushView(TViewAskDialog.CreateUntilStopped(Male));
    end;
  end;
end;

end.
