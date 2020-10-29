{
  Copyright 2014-2018 Michalis Kamburelis.

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
  ExtCtrls,
  CastleControl, CastleDialogs, CastleViewport, CastleScene;

type
  TMain = class(TForm)
    ButtonLoad1: TButton;
    ButtonLoad2: TButton;
    CastleControl2: TCastleControlBase;
    CastleControl1: TCastleControlBase;
    LabelFps1: TLabel;
    LabelFps2: TLabel;
    OpenDialog: TCastleOpen3DDialog;
    Timer: TTimer;
    procedure ButtonLoad1Click(Sender: TObject);
    procedure ButtonLoad2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    Viewport1, Viewport2: TCastleViewport;
    procedure Load(const Viewport: TCastleViewport; const SceneUrl: String);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses X3DLoad, CastleLCLUtils, CastleSceneCore;

{$R *.lfm}

{ TMain }

{ Simple demo that two TCastleControlBase instances within the same application
  work without problems, and share OpenGL resources.
  Try to open the same model (like X3D or glTF file), with same textures,
  in both controls:
  the OpenGL texture resources will be shared between two controls.

  Observe the FPS (Frames per second) counters of both controls.
  To test that our LimitFPS mechanism (that conserves your CPU)
  works correctly for multiple TCastleControlBase components on a form,
  load a model that:
  1. Is easy to display. You will know it's an easy 3D model when
     the "only render" fps number will be very large.
  2. And has some animation. The goal is to force the engine to redraw
     it continuously. (Alternatively, you could make any 3D model spin
     in Examine mode by pressing "right arrow" key.)
     For example: load demo_models/movie_texture/fireplace_final.wrl
     from demo models [https://castle-engine.io/demo_models.php] .
  Now observe the "real" FPS number under both controls:
  it should reach roughly 100 (default LimitFPS value), and stay there.
  Small differences from 100 (+/- 5) are Ok, it's not a precise mechanism.
}

procedure TMain.Load(const Viewport: TCastleViewport; const SceneUrl: String);
var
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Self);
  Scene.Load(SceneUrl);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport.Items.MainScene.Free; // clear previous scene, if any
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Viewport.AssignDefaultCamera;
  Viewport.AssignDefaultNavigation;
end;

procedure TMain.ButtonLoad1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    Load(Viewport1, OpenDialog.Url);
end;

procedure TMain.ButtonLoad2Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    Load(Viewport2, OpenDialog.Url);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToDialog(LoadScene_FileFilters, OpenDialog);

  Viewport1 := TCastleViewport.Create(Self);
  Viewport1.AutoCamera := true;
  Viewport1.AutoNavigation := true;
  Viewport1.FullSize := true;
  CastleControl1.Controls.InsertFront(Viewport1);

  Viewport2 := TCastleViewport.Create(Self);
  Viewport2.AutoCamera := true;
  Viewport2.AutoNavigation := true;
  Viewport2.FullSize := true;
  CastleControl2.Controls.InsertFront(Viewport2);
end;

procedure TMain.TimerTimer(Sender: TObject);
begin
  LabelFps1.Caption := 'FPS: ' + CastleControl1.Fps.ToString;
  LabelFps2.Caption := 'FPS: ' + CastleControl2.Fps.ToString;
end;

end.
