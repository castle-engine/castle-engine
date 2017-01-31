{
  Copyright 2008-2017 Michalis Kamburelis.

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

uses Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, CastleScene, CastleCameras, CastleControl, CastleLog,
  CastleLCLRecentFiles, CastleConfig, Buttons, ExtCtrls, StdCtrls, CastleRecentFiles,
  CastleSceneManager, CastleDialogs, CastleControls;

type
  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ButtonExamine: TSpeedButton;
    ButtonFly: TSpeedButton;
    ButtonNone: TSpeedButton;
    ButtonScreenshot: TBitBtn;
    ButtonChangeCamera: TButton;
    ButtonWalk: TSpeedButton;
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
    OpenDialog1: TCastleOpen3DDialog;
    PanelBottom: TPanel;
    SaveScreenshotDialog: TCastleSaveImageDialog;
    Timer1: TTimer;
    MenuMouseLookToggle: TMenuItem;
    RecentFiles: TCastleRecentFiles;
    procedure ButtonNavigationTypeClick(Sender: TObject);
    procedure ButtonScreenshotClick(Sender: TObject);
    procedure BrowserCameraChanged(Camera: TCamera);
    procedure ButtonChangeCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure MenuAboutOpenGLClick(Sender: TObject);
    procedure MenuFocusGLControlClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuShowConsoleClick(Sender: TObject);
    procedure MenuWebsiteClick(Sender: TObject);
    procedure SceneManagerBoundNavigationInfoChanged(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuMouseLookToggleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RecentFilesOpenRecent(const URL: string);
  private
    SceneURL: string;
    CameraChanged: boolean;
    ButtonsNavigationType: array [TNavigationType] of TSpeedButton;
    CrosshairCtl: TCastleCrosshair;
    CrosshairActive: Boolean;    // there is something to touch under the crosshair

    procedure OpenScene(const URL: string);
    procedure UpdateCaption;
    procedure UpdateCrosshairImage;
    procedure OnPointingDeviceSensorsChange(Sender: TObject);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses LCLType, LCLIntf, CastleVectors, CastleBoxes, X3DNodes, CastleRenderer,
  CastleClassUtils, CastleUtils, X3DLoad, CastleURIUtils,
  CastleGLUtils, CastleSceneCore, CastleFilesUtils, CastleParameters,
  CastleApplicationProperties,
  OpenGLInformation, CastleLCLUtils, ConsoleF, CastleImages, CastleSoundEngine;

procedure TMain.OpenScene(const URL: string);
begin
  Console.WasWarnings := false;
  Console.Memo1.Lines.Append('--- Loading ' + URL);

  Browser.Load(URL);
  Browser.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.MainScene.ProcessEvents := true;

  SceneURL := URL;
  UpdateCaption;

  if Console.WasWarnings then
  begin
    MenuShowConsole.Checked := true;
    Console.Visible := MenuShowConsole.Checked;
  end;

  RecentFiles.Add(URL);

  { after loading the scene, make sure to update ButtonsNavigationType state.
    Although during scene loading, OnBoundNavigationInfoChanged was already
    called, but at that time Camera was nil. }
  SceneManagerBoundNavigationInfoChanged(nil);

  { for changing the crosshair shape }
  Browser.MainScene.OnPointingDeviceSensorsChange := @OnPointingDeviceSensorsChange;

  { simple Browser.Load always recreates the Camera each time, which means
    that we have to restore all camera properties that should be
    "persistent" when loading scenes.
    For now, this means mouse look stuff.

    Note that you can instead load your scene manually (see *trivial*
    TCastleControl.Load implementation), and this way avoid recreating
    the camera. But then, you will have the opposite problem, you will
    have to explicitly update camera properties that *should* change when
    new scene is loaded, like the default viewpoint and navigation mode.
    Which isn't really a serious problem (you have comfortable
    TCastleSceneCore.CameraFromNavigationInfo and CameraFromViewpoint
    to deal with it). It's your choice, anyway. }
  MenuMouseLookToggleClick(MenuMouseLookToggle);
end;

procedure TMain.MenuOpenClick(Sender: TObject);
begin
  if SceneURL <> '' then
    OpenDialog1.URL := SceneURL;
  if OpenDialog1.Execute then
    OpenScene(OpenDialog1.URL);
end;

procedure TMain.UpdateCaption;
var
  S: string;
begin
  if SceneURL <> '' then
    S := URICaption(SceneURL) else
    S := 'No Scene';
  S += ' - ' +  ApplicationName +
    Format(' - FPS : %f (real : %f)', [Browser.Fps.FrameTime, Browser.Fps.RealTime]);
  Caption := S;
end;

procedure TMain.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.MenuShowConsoleClick(Sender: TObject);
begin
  Console.Visible := MenuShowConsole.Checked;
end;

procedure TMain.MenuWebsiteClick(Sender: TObject);
begin
  if not OpenURL('http://castle-engine.sourceforge.net/') then
    MessageDlg('WWW browser not found on your system.', mtError, [mbClose], 0);
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  Pos, Dir, Up: TVector3Single;
begin
  UpdateCaption; { to update FPS }

  { Update edit boxes about camera only from time to time.
    Otherwise, updating edit controls
    on every move would cause refresh rate of OpenGL context to suffer
    (e.g. when rotating object in Examine mode) }
  if CameraChanged then
  begin
    CameraChanged := false;

    Browser.Camera.GetView(Pos, Dir, Up);
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
end;

procedure TMain.MenuMouseLookToggleClick(Sender: TObject);
var
  Walk: TWalkCamera;
begin
  if Browser.Camera is TWalkCamera then
    Walk := TWalkCamera(Browser.Camera) else
  if Browser.Camera is TUniversalCamera then
    Walk := TUniversalCamera(Browser.Camera).Walk else
    Walk := nil;

  if Walk <> nil then begin
    Walk.MouseLook := (Sender as TMenuItem).Checked;
    UpdateCrosshairImage;
    Repaint;
  end;
end;

procedure TMain.OnPointingDeviceSensorsChange(Sender: TObject);
var
  OverSensor: Boolean;
  SensorList: TPointingDeviceSensorList;
begin
  { check if the crosshair (mouse) is over any sensor }
  OverSensor := false;
  SensorList := Browser.MainScene.PointingDeviceSensors;
  if (SensorList <> nil) then
    OverSensor := (SensorList.EnabledCount>0);

  if CrosshairActive <> OverSensor then
  begin
    CrosshairActive := OverSensor;
    UpdateCrosshairImage;
  end;
end;

procedure TMain.UpdateCrosshairImage;
var
  Walk: TWalkCamera;
begin
  if Browser.Camera is TWalkCamera then
    Walk := TWalkCamera(Browser.Camera) else
  if Browser.Camera is TUniversalCamera then
    Walk := TUniversalCamera(Browser.Camera).Walk else
    Walk := nil;

  CrosshairCtl.Exists := ((Walk <> nil) and Walk.MouseLook);

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

function MyGetApplicationName: string;
begin
  Result := 'model_3d_viewer';
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  { load config settings }
  OnGetApplicationName := @MyGetApplicationName;
  UserConfig.Load;
  SoundEngine.LoadFromConfig(UserConfig);
  RecentFiles.LoadFromConfig(UserConfig);

  RecentFiles.NextMenuItem := MenuSep1;

  UpdateCaption;

  { Uncomment these to enable shadow volumes (your model must have appropriate
    light). This is *not* necessary if you want to use only shadow maps.
    Note that TOpenGLControl.StencilBits is only available in new Lazarus,
    http://bugs.freepascal.org/view.php?id=22170 .
  Browser.StencilBits := 8;
  // Browser.ShadowVolumes := true; // not necessary, this is true by default
  }

  MenuFocusGLControl.ShortCut := ShortCut(VK_Escape, []);

  Console := TConsole.Create(Application);
  ApplicationProperties.OnWarning.Add(@Console.WarningToConsole);

  ButtonsNavigationType[ntExamine] := ButtonExamine;
  ButtonsNavigationType[ntWalk] := ButtonWalk;
  ButtonsNavigationType[ntFly] := ButtonFly;
  ButtonsNavigationType[ntNone] := ButtonNone;

  CrosshairCtl := TCastleCrosshair.Create(Browser);
  CrosshairCtl.Exists := false;  // start as invisible
  Browser.Controls.InsertFront(CrosshairCtl);

  if Parameters.High >= 1 then
    OpenScene(Parameters[1]);
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  { save config settings }
  SoundEngine.SaveToConfig(UserConfig);
  RecentFiles.SaveToConfig(UserConfig);
  UserConfig.Save;
end;

procedure TMain.RecentFilesOpenRecent(const URL: string);
begin
  OpenScene(URL);
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
begin
  CameraChanged := true;
end;

procedure TMain.SceneManagerBoundNavigationInfoChanged(Sender: TObject);
var
  NavigationType: TNavigationType;
begin
  { For safety, we check here Camera existence and class.
    Camera may not exist yet (it is intially nil, and is freed / recreated
    inside Browser.Load call in OpenScene).
    In this program, it is always TUniversalCamera (because by default
    TCastleSceneManager and TCastlScene always create the most versatile
    TUniversalCamera). }

  if (Browser.Camera <> nil) and
     (Browser.Camera is TUniversalCamera) then
  begin
    NavigationType := (Browser.Camera as TUniversalCamera).NavigationType;

    { make the appropriate button on ButtonsNavigationType pressed.
      Thanks to TSpeedButton.GroupIndex, all others will be automatically released. }
    ButtonsNavigationType[NavigationType].Down := true;
  end;
end;

procedure TMain.ButtonNavigationTypeClick(Sender: TObject);
var
  NavigationType, NT: TNavigationType;
begin
  { Just like in SceneManagerBoundNavigationInfoChanged:
    in practice, our Camera is always of TUniversalCamera class.
    But check for safety. }
  if (Browser.Camera <> nil) and
     (Browser.Camera is TUniversalCamera) then
  begin
    { this is a handler for all four navigation type buttons.
      Scan ButtonsNavigationType to detect which is Sender. }
    for NT := Low(NT) to High(NT) do
      if ButtonsNavigationType[NT] = Sender then
        NavigationType := NT;

    (Browser.Camera as TUniversalCamera).NavigationType := NavigationType;
  end;
end;

procedure TMain.ButtonScreenshotClick(Sender: TObject);
var
  Image: TRGBImage;
begin
  if SaveScreenshotDialog.Execute then
  begin
    Image := Browser.SaveScreen;
    try
      SaveImage(Image, SaveScreenshotDialog.URL);
    finally FreeAndNil(Image) end;
  end;
end;

initialization
  {$I mainf.lrs}
end.
