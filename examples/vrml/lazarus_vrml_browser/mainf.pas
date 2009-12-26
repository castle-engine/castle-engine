unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, VRMLGLScene, Navigation, KambiVRMLBrowser,
  LCLRecentFiles, KambiXMLCfg, Buttons, ExtCtrls, StdCtrls, RecentFiles;

type

  { TMain }

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
    GroupBox1: TGroupBox;
    Browser: TKamVRMLBrowser;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    procedure BrowserNavigatorChanged(Navigator: TNavigator);
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

uses LCLType, VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer,
  GL, GLU, GLExt, KambiClassUtils, KambiUtils, Object3dAsVRML,
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
  S += ' - lazarus_vrml_browser' +
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
begin
  { TODO: for this to really work Ok, for newly loaded scenes we should
    also set their MouseLook and inputs, otherwise MouseLook and menu checked
    values may get not synchronized. }

  if Browser.Navigator is TWalkNavigator then
  begin
    Browser.WalkNav.MouseLook := (Sender as TMenuItem).Checked;
    Browser.UpdateMouseLook;

    if Browser.WalkNav.MouseLook then
    begin
      Browser.WalkNav.Input_LeftStrafe.AssignFromDefault(Browser.WalkNav.Input_LeftRot);
      Browser.WalkNav.Input_RightStrafe.AssignFromDefault(Browser.WalkNav.Input_RightRot);
      Browser.WalkNav.Input_LeftRot.AssignFromDefault(Browser.WalkNav.Input_LeftStrafe);
      Browser.WalkNav.Input_RightRot.AssignFromDefault(Browser.WalkNav.Input_RightStrafe);
    end else
    begin
      Browser.WalkNav.Input_LeftStrafe.MakeDefault;
      Browser.WalkNav.Input_RightStrafe.MakeDefault;
      Browser.WalkNav.Input_LeftRot.MakeDefault;
      Browser.WalkNav.Input_RightRot.MakeDefault;
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
  Result := 'lazarus_vrml_browser';
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToOpenDialog(LoadAsVRML_FileFilters, OpenDialog1);

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
var
  Pos, Dir, Up: TVector3Single;
begin
  Pos := Vector3Single(
    StrToFloat(EditPositionX.Text),
    StrToFloat(EditPositionY.Text),
    StrToFloat(EditPositionZ.Text));

  Dir := Vector3Single(
    StrToFloat(EditDirectionX.Text),
    StrToFloat(EditDirectionY.Text),
    StrToFloat(EditDirectionZ.Text));

  Up := Vector3Single(
    StrToFloat(EditUpX.Text),
    StrToFloat(EditUpY.Text),
    StrToFloat(EditUpZ.Text));

  { Length of direction vector affects speed.
    For simplicity, we don't allow user to change this here
    (although keys +/- do this in Walk mode), we keep previous CameraDir
    length. }
  VectorAdjustToLengthTo1st(Dir, VectorLen(Browser.WalkNav.CameraDir));

  { First convert all to float. Then set Navigator properties.
    This way in case of exception in StrToFloat, previous
    Navigator properties remain OK. }

  if Browser.Navigator is TWalkNavigator then
  begin
    Browser.WalkNav.CameraPos := Pos;
    Browser.WalkNav.CameraDir := Dir;
    Browser.WalkNav.CameraUp := Up;
  end else
    MessageDlg('Setting camera properties in EXAMINE navigation not implemented.',
      mtError, [mbOk], 0);
end;

procedure TMain.BrowserNavigatorChanged(Navigator: TNavigator);
var
  Pos, Dir, Up: TVector3Single;
begin
  Navigator.GetCameraVectors(Pos, Dir, Up);

  EditPositionX.Text := FloatToNiceStr(Pos[0]);
  EditPositionY.Text := FloatToNiceStr(Pos[1]);
  EditPositionZ.Text := FloatToNiceStr(Pos[2]);

  { Length of direction vector affects speed.
    For simplicity, we don't show it to user here (it could have small
    values, and would look like all "0.00" while in fact being non-zero).
    Instead. we show the normalized dir. }
  Dir := Normalized(Dir);

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

