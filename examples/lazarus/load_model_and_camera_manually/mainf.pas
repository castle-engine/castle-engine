{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleControl, CastleCameras, CastleScene, CastleViewport;

type
  TMainForm = class(TForm)
    Control1: TCastleControl;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    Viewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    Background: TCastleBackground;
    Scene: TCastleScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses CastleVectors, CastleColors;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Viewport := TCastleViewport.Create(Self);
  Viewport.FullSize := true;
  Control1.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Self);
  Scene.Load('castle-data:/bridge_level/bridge_final.x3dv');
  Scene.PreciseCollisions := true;

  Viewport.Items.Add(Scene);

  WalkNavigation := TCastleWalkNavigation.Create(Self);
  WalkNavigation.PreferredHeight := 2;
  WalkNavigation.Radius := 0.1;
  WalkNavigation.MoveSpeed := 10.0; // default is 1
  WalkNavigation.Gravity := true;
  Viewport.InsertFront(WalkNavigation);

  // Set camera vectors using Castle Game Engine.
  Viewport.Camera.SetWorldView(
    Vector3(-46.30, -4.49, 4.89), // position
    Vector3(0.96, 0.03, -0.27), // direction
    Vector3(-0.03, 1.00, 0.01)  // up (current)
  );
  Viewport.Camera.ProjectionNear := WalkNavigation.Radius / 2;

  Background := TCastleBackground.Create(Self);
  Background.SkyTopColor := RedRGB;
  Viewport.Background := Background;
end;

end.
