{
  Copyright 2008-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit MainF;

interface

uses Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, CastleScene, CastleCameras, CastleControl, CastleLog,
  CastleLCLRecentFiles, CastleConfig, Buttons, ExtCtrls, StdCtrls, CastleRecentFiles,
  CastleViewport, CastleDialogs, CastleControls;

type
  { Main form. }
  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ButtonExamine: TSpeedButton;
    ButtonFly: TSpeedButton;
    ButtonNone: TSpeedButton;
    ButtonScreenshot: TBitBtn;
    ButtonChangeCamera: TButton;
    ButtonWalk: TSpeedButton;
    ButtonWarnings: TButton;
    EditPositionX: TEdit;
    EditPositionY: TEdit;
    EditPositionZ: TEdit;
    EditDirectionZ: TEdit;
    EditDirectionY: TEdit;
    EditDirectionX: TEdit;
    EditUpZ: TEdit;
    EditUpY: TEdit;
    EditUpX: TEdit;
    GroupNavigationType: TGroupBox;
    GroupBoxCamera: TGroupBox;
    Browser: TCastleControl;
    ImagePoweredBy: TImage;
    LabelFps: TLabel;
    LabelPosition: TLabel;
    LabelDirection: TLabel;
    LabelUp: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFocusGLControl: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAboutOpenGL: TMenuItem;
    MenuWebsite: TMenuItem;
    MenuSep2: TMenuItem;
    MenuShowConsole: TMenuItem;
    MenuItemView: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSep1: TMenuItem;
    MenuOpen: TMenuItem;
    OpenDialog1: TCastleOpenSceneDialog;
    PanelBottom: TPanel;
    SaveScreenshotDialog: TCastleSaveImageDialog;
    Timer1: TTimer;
    MenuMouseLookToggle: TMenuItem;
    procedure ButtonNavigationTypeClick(Sender: TObject);
    procedure ButtonScreenshotClick(Sender: TObject);
    procedure BrowserCameraChanged(Camera: TObject);
    procedure ButtonChangeCameraClick(Sender: TObject);
    procedure ButtonWarningsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure MenuAboutOpenGLClick(Sender: TObject);
    procedure MenuFocusGLControlClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuShowConsoleClick(Sender: TObject);
    procedure MenuWebsiteClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuMouseLookToggleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    SceneUrl: String;
    CameraChanged: boolean;
    ButtonsNavigationType: array [TNavigationType] of TSpeedButton;
    CrosshairCtl: TCastleCrosshair;
    CrosshairActive: Boolean; //< there is something to touch under the crosshair
    RecentFiles: TCastleRecentFiles;
    Viewport: TCastleViewport;
    MainScene: TCastleScene; //< main loaded scene
    NavigationType: TNavigationType;
    Navigation: TCastleNavigation;

    procedure OpenScene(const Url: String);
    procedure UpdateCaption;
    procedure UpdateCrosshairImage;
    procedure PointingDeviceSensorsChange(Sender: TObject);
    procedure RecentFilesOpenRecent(const Url: String);
    procedure ChangeNavigationType(const NewNavigationType: TNavigationType);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses LCLType, LCLIntf, CastleVectors, CastleBoxes, X3DNodes,
  CastleClassUtils, CastleUtils, X3DLoad, CastleUriUtils,
  CastleGLUtils, CastleSceneCore, CastleFilesUtils, CastleParameters,
  CastleApplicationProperties,
  OpenGLInformation, CastleLCLUtils, ConsoleF, CastleImages, CastleSoundEngine;

{$R *.lfm}

procedure TMain.OpenScene(const Url: String);

  procedure LoadScene(const Url: String);
  begin
    MainScene.Free; // free previous MainScene

    MainScene := TCastleScene.Create(Self);
    MainScene.Load(Url);
    MainScene.PreciseCollisions := true;
    MainScene.ProcessEvents := true;

    Viewport.Items.Add(MainScene);
    // deprecated, but must be set for AssignDefaultCamera to reflect current camera
    Viewport.Items.MainScene := MainScene;

    Viewport.AssignDefaultCamera;
  end;

begin
  Console.WarningsCount := 0; // reset
  ButtonWarnings.Visible := false;
  ButtonWarnings.Enabled := false;
  Console.Memo1.Lines.Append('--- Loading ' + Url);

  LoadScene(Url);

  SceneUrl := Url;
  UpdateCaption;

  ButtonWarnings.Caption := Format('Warnings (%d)', [Console.WarningsCount]);
  ButtonWarnings.Visible := Console.WarningsCount <> 0;
  ButtonWarnings.Enabled := Console.WarningsCount <> 0;

  RecentFiles.Add(Url);

  { for changing the crosshair shape }
  MainScene.OnPointingDeviceSensorsChange := @PointingDeviceSensorsChange;
end;

procedure TMain.MenuOpenClick(Sender: TObject);
begin
  if SceneUrl <> '' then
    OpenDialog1.Url := SceneUrl;
  if OpenDialog1.Execute then
    OpenScene(OpenDialog1.Url);
end;

procedure TMain.UpdateCaption;
var
  S: string;
begin
  if SceneUrl <> '' then
    S := UriCaption(SceneUrl)
  else
    S := 'No Scene';
  S := S + ' | Castle Game Engine - Lazarus Model Viewer';
  Caption := S;
end;

procedure TMain.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.MenuShowConsoleClick(Sender: TObject);
begin
  Console.Show;
end;

procedure TMain.MenuWebsiteClick(Sender: TObject);
begin
  if not OpenUrl('https://castle-engine.io/') then
    MessageDlg('WWW browser not found on your system.', mtError, [mbClose], 0);
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  Pos, Dir, Up: TVector3;
begin
  LabelFps.Caption := 'FPS: ' + Browser.Container.Fps.ToString;

  { Update edit boxes about camera only from time to time.
    Otherwise, updating edit controls
    on every frame would lower performance of OpenGL
    (e.g. when rotating object in Examine mode) }
  if CameraChanged then
  begin
    CameraChanged := false;

    Viewport.Camera.GetView(Pos, Dir, Up);
    { Note that Dir, Up returned here are always normalized }

    EditPositionX.Text := FloatToStrDisplay(Pos[0]);
    EditPositionY.Text := FloatToStrDisplay(Pos[1]);
    EditPositionZ.Text := FloatToStrDisplay(Pos[2]);

    EditDirectionX.Text := FloatToStrDisplay(Dir[0]);
    EditDirectionY.Text := FloatToStrDisplay(Dir[1]);
    EditDirectionZ.Text := FloatToStrDisplay(Dir[2]);

    EditUpX.Text := FloatToStrDisplay(Up[0]);
    EditUpY.Text := FloatToStrDisplay(Up[1]);
    EditUpZ.Text := FloatToStrDisplay(Up[2]);
  end;
end;

procedure TMain.MenuMouseLookToggleClick(Sender: TObject);
var
  WalkNav: TCastleWalkNavigation;
begin
  if Navigation is TCastleWalkNavigation then
  begin
    WalkNav := Navigation as TCastleWalkNavigation;
    WalkNav.MouseLook := (Sender as TMenuItem).Checked;
    UpdateCrosshairImage;
    Repaint;
  end;
end;

procedure TMain.PointingDeviceSensorsChange(Sender: TObject);
var
  OverSensor: Boolean;
  SensorList: TPointingDeviceSensorList;
begin
  { check if the crosshair (mouse) is over any sensor }
  OverSensor := false;
  SensorList := MainScene.PointingDeviceSensors;
  if (SensorList <> nil) then
    OverSensor := (SensorList.EnabledCount>0);

  if CrosshairActive <> OverSensor then
  begin
    CrosshairActive := OverSensor;
    UpdateCrosshairImage;
  end;
end;

procedure TMain.UpdateCrosshairImage;
begin
  CrosshairCtl.Exists := (
    (Navigation is TCastleWalkNavigation) and
    (Navigation as TCastleWalkNavigation).MouseLook
  );
  if CrosshairActive then
    CrosshairCtl.Shape := csCrossRect
  else
    CrosshairCtl.Shape := csCross;
end;

procedure TMain.MenuAboutOpenGLClick(Sender: TObject);
begin
  TOpenGLInformation.Execute;
end;

procedure TMain.MenuFocusGLControlClick(Sender: TObject);
begin
  Browser.SetFocus;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  ApplicationProperties.ApplicationName := 'model_3d_viewer';
  InitializeLog;

  { load config settings }
  UserConfig.Load;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.OnCameraChanged := @BrowserCameraChanged;
  Viewport.Camera.Add(TCastleDirectionalLight.Create(Self)); // headlight
  Viewport.PreventInfiniteFallingDown := true;
  Browser.Controls.InsertFront(Viewport);

  RecentFiles := TCastleRecentFiles.Create(Self);
  RecentFiles.OnOpenRecent := @RecentFilesOpenRecent;
  RecentFiles.LoadFromConfig(UserConfig);
  RecentFiles.NextMenuItem := MenuSep1;

  UpdateCaption;

  Console := TConsole.Create(Application);

  ButtonsNavigationType[ntExamine] := ButtonExamine;
  ButtonsNavigationType[ntWalk] := ButtonWalk;
  ButtonsNavigationType[ntFly] := ButtonFly;
  ButtonsNavigationType[ntNone] := ButtonNone;

  // synchronize NavigationType value with actual state
  NavigationType := ntNone;

  // create ntExamine navigation (this changes NavigationType and Navigation)
  ChangeNavigationType(ntExamine);
  Assert(NavigationType = ntExamine);
  ButtonsNavigationType[NavigationType].Down := true;

  CrosshairCtl := TCastleCrosshair.Create(Browser);
  CrosshairCtl.Exists := false;  // start as invisible
  Browser.Controls.InsertFront(CrosshairCtl);

  if Parameters.High >= 1 then
    OpenScene(Parameters[1]);
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  { save config settings }
  RecentFiles.SaveToConfig(UserConfig);
  UserConfig.Save;
end;

procedure TMain.RecentFilesOpenRecent(const Url: String);
begin
  OpenScene(Url);
end;

procedure TMain.FormDeactivate(Sender: TObject);
begin
  Browser.ReleaseAllKeysAndMouse;
end;

procedure TMain.ButtonChangeCameraClick(Sender: TObject);
begin
  Viewport.Camera.SetView(
    Vector3(
      StrToFloatDot(EditPositionX.Text),
      StrToFloatDot(EditPositionY.Text),
      StrToFloatDot(EditPositionZ.Text)),
    Vector3(
      StrToFloatDot(EditDirectionX.Text),
      StrToFloatDot(EditDirectionY.Text),
      StrToFloatDot(EditDirectionZ.Text)),
    Vector3(
      StrToFloatDot(EditUpX.Text),
      StrToFloatDot(EditUpY.Text),
      StrToFloatDot(EditUpZ.Text)));
end;

procedure TMain.ButtonWarningsClick(Sender: TObject);
begin
  Console.Show;
end;

procedure TMain.BrowserCameraChanged(Camera: TObject);
begin
  CameraChanged := true;
end;

procedure TMain.ButtonNavigationTypeClick(Sender: TObject);
var
  NewNavigationType, NT: TNavigationType;
begin
  { This is a handler for all four navigation type buttons.
    Scan ButtonsNavigationType to detect which is Sender. }
  for NT := Low(NT) to High(NT) do
    if ButtonsNavigationType[NT] = Sender then
      NewNavigationType := NT;
  ChangeNavigationType(NewNavigationType);
end;

procedure TMain.ChangeNavigationType(const NewNavigationType: TNavigationType);
begin
  if NavigationType <> NewNavigationType then
  begin
    FreeAndNil(Navigation); // this will also remove Navigation from Viewport children
    case NewNavigationType of
      ntExamine:
        begin
          Navigation := TCastleExamineNavigation.Create(Self);
        end;
      ntWalk:
        begin
          Navigation := TCastleWalkNavigation.Create(Self);
          (Navigation as TCastleWalkNavigation).Gravity := true;
        end;
      ntFly:
        begin
          Navigation := TCastleWalkNavigation.Create(Self);
          (Navigation as TCastleWalkNavigation).Gravity := false;
        end;
    end;
    NavigationType := NewNavigationType;
    if Navigation <> nil then // we leave Navigation=nil in case of NavigationType=ntNone
      Viewport.InsertFront(Navigation);
  end;
end;

procedure TMain.ButtonScreenshotClick(Sender: TObject);
var
  Image: TRGBImage;
begin
  if SaveScreenshotDialog.Execute then
  begin
    Image := Browser.Container.SaveScreen;
    try
      SaveImage(Image, SaveScreenshotDialog.Url);
    finally FreeAndNil(Image) end;
  end;
end;

end.
