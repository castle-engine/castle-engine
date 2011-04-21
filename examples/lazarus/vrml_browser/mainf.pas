{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, VRMLGLScene, Cameras, KambiVRMLBrowser,
  LCLRecentFiles, KambiXMLConfig, Buttons, ExtCtrls, StdCtrls, RecentFiles;

type
  TMain = class(TForm)
    ButtonChangeCamera: TButton;
    EditPositionX: TEdit;
    EditPositionY: TEdit;
    EditPositionZ: TEdit;
    EditDirectionZ: TEdit;
    EditDirectionY: TEdit;
    EditDirectionX: TEdit;
    EditUpZ: TEdit;
    EditUpY: TEdit;
    EditUpX: TEdit;
    GroupBoxCamera: TGroupBox;
    Browser: TKamVRMLBrowser;
    LabelPosition: TLabel;
    LabelDirection: TLabel;
    LabelUp: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFocusGLControl: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAboutOpenGL: TMenuItem;
    MenuItem1: TMenuItem;
    MenuShowVrmlConsole: TMenuItem;
    MenuItemView: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSep1: TMenuItem;
    MenuOpen: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelBottom: TPanel;
    Timer1: TTimer;
    MenuItem2: TMenuItem;
    MenuMouseLookToggle: TMenuItem;
    Config: TKamXMLConfig;
    RecentFiles: TKamRecentFiles;
    MenuAggressiveUpdateToggle: TMenuItem;
    procedure BrowserCameraChanged(Camera: TCamera);
    procedure ButtonChangeCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure MenuAboutOpenGLClick(Sender: TObject);
    procedure MenuFocusGLControlClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuShowVrmlConsoleClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuMouseLookToggleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RecentFilesOpenRecent(const FileName: string);
    procedure MenuAggressiveUpdateToggleClick(Sender: TObject);
  private
    SceneFileName: string;
    procedure OpenScene(const FileName: string);
    procedure UpdateCaption;
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses LCLType, VectorMath, Boxes3D, VRMLNodes, VRMLGLRenderer,
  GL, GLU, GLExt, KambiClassUtils, KambiUtils, Object3DAsVRML,
  KambiGLUtils, VRMLScene, KambiFilesUtils,
  OpenGLInformation, KambiLCLUtils, VrmlConsoleF, VRMLErrors;

procedure TMain.OpenScene(const FileName: string);
begin
  VrmlConsole.WasWarnings := false;
  VrmlConsole.Memo1.Lines.Append('--- Loading ' + FileName);

  Browser.Load(FileName);
  Browser.Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.Scene.ProcessEvents := true;

  SceneFileName := FileName;
  UpdateCaption;

  if VrmlConsole.WasWarnings then
  begin
    MenuShowVrmlConsole.Checked := true;
    VrmlConsole.Visible := MenuShowVrmlConsole.Checked;
  end;

  RecentFiles.Add(FileName);
end;

procedure TMain.MenuOpenClick(Sender: TObject);
begin
  if SceneFileName <> '' then
    OpenDialog1.FileName := SceneFileName;
  if OpenDialog1.Execute then
    OpenScene(OpenDialog1.FileName);
end;

procedure TMain.UpdateCaption;
var
  S: string;
begin
  if SceneFileName <> '' then
    S := ExtractFileName(SceneFileName) else
    S := 'No Scene';
  S += ' - vrml_browser' +
    Format(' - FPS : %f (real : %f)', [Browser.Fps.FrameTime, Browser.Fps.RealTime]);
  Caption := S;
end;

procedure TMain.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.MenuShowVrmlConsoleClick(Sender: TObject);
begin
  VrmlConsole.Visible := MenuShowVrmlConsole.Checked;
end;

procedure TMain.Timer1Timer(Sender: TObject);
begin
  UpdateCaption; { to update FPS }
end;

procedure TMain.MenuMouseLookToggleClick(Sender: TObject);
var
  Walk: TWalkCamera;
begin
  { TODO: for this to really work Ok, for newly loaded scenes we should
    also set their MouseLook and inputs, otherwise MouseLook and menu checked
    values may get not synchronized. }

  if Browser.Camera is TWalkCamera then
    Walk := TWalkCamera(Browser.Camera) else
  if Browser.Camera is TUniversalCamera then
    Walk := TUniversalCamera(Browser.Camera).Walk else
    Walk := nil;

  if Walk <> nil then
  begin
    Walk.MouseLook := (Sender as TMenuItem).Checked;

    if Walk.MouseLook then
    begin
      Walk.Input_LeftStrafe.AssignFromDefault(Walk.Input_LeftRot);
      Walk.Input_RightStrafe.AssignFromDefault(Walk.Input_RightRot);
      Walk.Input_LeftRot.AssignFromDefault(Walk.Input_LeftStrafe);
      Walk.Input_RightRot.AssignFromDefault(Walk.Input_RightStrafe);
    end else
    begin
      Walk.Input_LeftStrafe.MakeDefault;
      Walk.Input_RightStrafe.MakeDefault;
      Walk.Input_LeftRot.MakeDefault;
      Walk.Input_RightRot.MakeDefault;
    end;
  end;
end;

procedure TMain.MenuAboutOpenGLClick(Sender: TObject);
begin
  TOpenGLInformation.Execute;
end;

procedure TMain.MenuFocusGLControlClick(Sender: TObject);
begin
  Browser.SetFocus;
end;

function MyGetApplicationName: string;
begin
  Result := 'vrml_browser';
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToOpenDialog(LoadVRML_FileFilters, OpenDialog1);

  { load config settings, in particular recent files }
  OnGetApplicationName := @MyGetApplicationName;
  Config.FileName := UserConfigFile('.conf');
  RecentFiles.LoadFromConfig(Config, 'recent_files');
  RecentFiles.NextMenuItem := MenuSep1;

  Browser.Fps.Active := true;

  UpdateCaption;

  { Uncomment these to use shadows:

  Browser.ShadowVolumesPossible := true;
  Browser.ShadowVolumes := true;
  }

  MenuFocusGLControl.ShortCut := ShortCut(VK_Escape, []);

  VrmlConsole := TVrmlConsole.Create(Application);
  VRMLWarning := @VRMLWarning_VrmlConsole;

  if Parameters.High >= 1 then
    OpenScene(Parameters[1]);
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  { save config settings }
  RecentFiles.SaveToConfig(Config, 'recent_files');
  Config.Flush;
end;

procedure TMain.RecentFilesOpenRecent(const FileName: string);
begin
  OpenScene(FileName);
end;

procedure TMain.MenuAggressiveUpdateToggleClick(Sender: TObject);
begin
  Browser.AggressiveUpdate := (Sender as TMenuItem).Checked;
end;

procedure TMain.FormDeactivate(Sender: TObject);
begin
  Browser.ReleaseAllKeysAndMouse;
end;

procedure TMain.ButtonChangeCameraClick(Sender: TObject);
begin
  Browser.Camera.SetView(
    Vector3Single(
      StrToFloat(EditPositionX.Text),
      StrToFloat(EditPositionY.Text),
      StrToFloat(EditPositionZ.Text)),
    Vector3Single(
      StrToFloat(EditDirectionX.Text),
      StrToFloat(EditDirectionY.Text),
      StrToFloat(EditDirectionZ.Text)),
    Vector3Single(
      StrToFloat(EditUpX.Text),
      StrToFloat(EditUpY.Text),
      StrToFloat(EditUpZ.Text)));
end;

procedure TMain.BrowserCameraChanged(Camera: TCamera);
var
  Pos, Dir, Up: TVector3Single;
begin
  Camera.GetView(Pos, Dir, Up);
  { Note that Dir, Up returned here are always normalized }

  EditPositionX.Text := FloatToNiceStr(Pos[0]);
  EditPositionY.Text := FloatToNiceStr(Pos[1]);
  EditPositionZ.Text := FloatToNiceStr(Pos[2]);

  EditDirectionX.Text := FloatToNiceStr(Dir[0]);
  EditDirectionY.Text := FloatToNiceStr(Dir[1]);
  EditDirectionZ.Text := FloatToNiceStr(Dir[2]);

  EditUpX.Text := FloatToNiceStr(Up[0]);
  EditUpY.Text := FloatToNiceStr(Up[1]);
  EditUpZ.Text := FloatToNiceStr(Up[2]);
end;

initialization
  {$I mainf.lrs}
end.

