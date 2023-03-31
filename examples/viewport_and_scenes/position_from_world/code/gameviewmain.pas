{
  Copyright 2022-2023 Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    MainSphere: TCastleSphere;
    TextAttached: TCastleText;
    LabelAttached: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleLog;

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
  SphereWorldPosition, SphereWorldPosition1: TVector3;
  SphereViewportPosition, SphereViewportPosition1: TVector2;
  Sphere1UnitSizeOnScreen, TextAttachedScale: Single;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { calculate SphereViewportPosition, position of the sphere middle in 2D viewport coordinates. }
  SphereWorldPosition := MainSphere.LocalToWorld(Vector3(0, 0, 0));
  SphereViewportPosition := MainViewport.PositionFromWorld(SphereWorldPosition);

  { SphereViewportPosition is immediately useful to determine LabelAttached position }
  LabelAttached.Translation := SphereViewportPosition;

  { calculate Sphere1UnitSizeOnScreen, how large is 1 unit in 3D space around sphere, on the screen. }
  SphereWorldPosition1 := SphereWorldPosition + Vector3(0, 1, 0);
  SphereViewportPosition1 := MainViewport.PositionFromWorld(SphereWorldPosition1);
  Sphere1UnitSizeOnScreen := PointsDistance(SphereViewportPosition, SphereViewportPosition1);

  { use Sphere1UnitSizeOnScreen to keep the TextAttached size similar on screen,
    regardless of how far is the scene. }
  TextAttachedScale := 100 / Sphere1UnitSizeOnScreen;
  TextAttached.Scale := Vector3(TextAttachedScale, TextAttachedScale, TextAttachedScale);
end;

end.
