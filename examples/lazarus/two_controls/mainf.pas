unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleControl;

type

  { TMain }

  TMain = class(TForm)
    ButtonLoad1: TButton;
    ButtonLoad2: TButton;
    CastleControl2: TCastleControl;
    CastleControl1: TCastleControl;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    procedure ButtonLoad1Click(Sender: TObject);
    procedure ButtonLoad2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  the OpenGL texture resources will be shared between two controls. }

procedure TMain.ButtonLoad1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    CastleControl1.Load(OpenDialog1.FileName);
    CastleControl1.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    CastleControl1.MainScene.ProcessEvents := true;
  end;
end;

procedure TMain.ButtonLoad2Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    CastleControl2.Load(OpenDialog2.FileName);
    CastleControl2.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    CastleControl2.MainScene.ProcessEvents := true;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToDialog(Load3D_FileFilters, OpenDialog1);
  FileFiltersToDialog(Load3D_FileFilters, OpenDialog2);
end;

end.

