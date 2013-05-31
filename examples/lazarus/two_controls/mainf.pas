unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CastleControl;

type

  { TMain }

  TMain = class(TForm)
    ButtonLoad1: TButton;
    ButtonLoad2: TButton;
    CastleControl2: TCastleControl;
    CastleControl1: TCastleControl;
    LabelFps1: TLabel;
    LabelFps2: TLabel;
    OpenDialog: TOpenDialog;
    Timer: TTimer;
    procedure ButtonLoad1Click(Sender: TObject);
    procedure ButtonLoad2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses X3DLoad, CastleLCLUtils, CastleSceneCore;

{$R *.lfm}

{ TMain }

{ Simple demo that two TCastleControl controls within the same application
  work without problems, and share OpenGL resources.
  Try to open the same 3D file, with some textures, in both windows:
  the OpenGL texture resources will be shared between two controls.

  Observe the FPS (Frames per second) counters of both controls.
  To test that our LimitFPS mechanism (that conserves your CPU)
  works correctly for multiple TCastleControl components on a form,
  load a 3D model that:
  1. Is easy to display. You will know it's an easy 3D model when
     the 1st (not "real") fps number will be very large.
  2. And has some animation. The goal is to force the engine to redraw
     it continously. (Alternatively, you could make any 3D model spin
     in Examine mode by pressing "right arrow" key.)
     For example: load demo_models/movie_texture/fireplace_final.wrl
     from demo models [http://castle-engine.sourceforge.net/demo_models.php] .
  Now observe the "real" FPS number under both controls:
  it should reach roughly 100 (default LimitFPS value), and stay there.
  Small differences from 100 (+/- 5) are Ok, it's not a precise mechanism.
}

procedure TMain.ButtonLoad1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    CastleControl1.Load(URIToFilenameSafeUTF8(OpenDialog.FileName));
    CastleControl1.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    CastleControl1.MainScene.ProcessEvents := true;
  end;
end;

procedure TMain.ButtonLoad2Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    CastleControl2.Load(URIToFilenameSafeUTF8(OpenDialog.FileName));
    CastleControl2.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    CastleControl2.MainScene.ProcessEvents := true;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToDialog(Load3D_FileFilters, OpenDialog);
end;

procedure TMain.TimerTimer(Sender: TObject);
begin
  LabelFps1.Caption := Format('FPS: %f (real : %f)',
    [CastleControl1.Fps.FrameTime, CastleControl1.Fps.RealTime]);
  LabelFps2.Caption := Format('FPS: %f (real : %f)',
    [CastleControl2.Fps.FrameTime, CastleControl2.Fps.RealTime]);
end;

end.

