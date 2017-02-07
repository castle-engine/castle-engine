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

{ Simple Precomputed Radiance Transfer implementation.
  Self-shadowing with diffuse lighting.

  Navigate with mouse or keyboard (like view3dscene in Examine mode).

  AWSD, QE move the light.
  R, Shift+R change light radius.
  L, Shift+L change light intensity scale.
}

program radiance_transfer;

{$I castleconf.inc}

uses SysUtils, Classes, Math,
  CastleVectors, X3DNodes, CastleGL, CastleWindow,
  CastleClassUtils, CastleUtils, CastleRenderingCamera,
  CastleGLUtils, CastleScene, CastleKeysMouse, CastleSceneManager,
  CastleFilesUtils, CastleLog, CastleSphericalHarmonics, CastleImages,
  CastleGLCubeMaps, CastleStringUtils, CastleParameters, CastleColors,
  CastleApplicationProperties;

type
  TViewMode = (vmNormal, vmSimpleOcclusion, vmFull);

var
  Window: TCastleWindowCustom;
  Scene: TCastleScene;
  ViewMode: TViewMode = vmFull;
  LightRadius: Single;
  LightPos: TVector3Single;
  RenderParams: TBasicRenderParams;

const
  { This is currently not synched with actual SHBasisCount used to generate
    the Scene. We just always prepare LightSHBasisCount components,
    eventually some of them will not be used in DoRadianceTransfer.

    While this is not optimal, this also may allow to use different SHBasis
    for different shapes within the Scene in the future. }

  LightSHBasisCount = 25;

var
  { This is calculated at the beginning of each Draw.
    Can be used then by DoRadianceTransfer. }
  LightSHBasis: array [0..LightSHBasisCount - 1] of Single;

  { Intensity specific for this light.
    Right now, we have only one light here, but the point is that we could
    have any number of lights.
    Only in 0..1 (as it's used as color component). }
  LightIntensity: Single = 1.0;

  { All lights intensity (obtained by getting light maps) are scaled
    by this. Can be in any range. }
  LightIntensityScale: Single = 100.0;

procedure DrawLight(ForMap: boolean);
begin
  glPushMatrix;
    glTranslatev(LightPos);

    if not ForMap then
    begin
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      glColor4f(1, 1, 0, 0.1);
    end else
      glColor3f(LightIntensity, LightIntensity, LightIntensity);

    CastleGluSphere(LightRadius, 10, 10);

    if not ForMap then
      glDisable(GL_BLEND);
  glPopMatrix;
end;

type
  TMySceneManager = class(TCastleSceneManager)
    procedure RenderFromViewEverything; override;
  end;

procedure TMySceneManager.RenderFromViewEverything;
{ It would be cleaner to override Draw (and move SHVectorGLCapture there)
  and Render3D (and move DrawLight there). But then, our debugging view
  of SHVectorGLCapture (under 3d model) would not be visible. }
begin
  RenderContext.Clear([cbColor, cbDepth], Black);
  glLoadMatrix(RenderingCamera.Matrix);

  if not Scene.BoundingBox.IsEmpty then
  begin
    { SHVectorGLCapture wil draw maps, get them,
      and calculate LightSHBasis describing the light contribution
      (this will be used then by Scene.Render, during DoRadianceTransfer). }

    SHVectorGLCapture(LightSHBasis, Scene.BoundingBox.Center,
      @DrawLight, 100, 100, LightIntensityScale);
    glViewport(ScreenRect);
  end;

  Scene.Render(RenderingCamera.Frustum, RenderParams);

  DrawLight(false);
end;

var
  SceneManager: TMySceneManager;

procedure UpdateViewMode; forward;

procedure Open(Container: TUIContainer);
begin
  glEnable(GL_LIGHT0);
  UpdateViewMode;
end;

type
  THelper = class
    function DoRadianceTransfer(Node: TAbstractGeometryNode;
      RadianceTransfer: PVector3Single;
      const RadianceTransferCount: Cardinal): TVector3Single;
  end;

function THelper.DoRadianceTransfer(Node: TAbstractGeometryNode;
  RadianceTransfer: PVector3Single;
  const RadianceTransferCount: Cardinal): TVector3Single;
var
  I: Integer;
begin
  Assert(RadianceTransferCount > 0);

  if ViewMode = vmSimpleOcclusion then
  begin
    Result := RadianceTransfer[0];
  end else
  begin
    Result := ZeroVector3Single;
    for I := 0 to Min(RadianceTransferCount, LightSHBasisCount) - 1 do
    begin
      Result[0] += RadianceTransfer[I][0] * LightSHBasis[I];
      Result[1] += RadianceTransfer[I][1] * LightSHBasis[I];
      Result[2] += RadianceTransfer[I][2] * LightSHBasis[I];
    end;
  end;
end;

procedure UpdateViewMode;
begin
  if ViewMode = vmNormal then
    Scene.Attributes.OnRadianceTransfer := nil else
    Scene.Attributes.OnRadianceTransfer := @THelper(nil).DoRadianceTransfer;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);
begin
  case Item.IntData of
    10: ViewMode := vmNormal;
    11: ViewMode := vmSimpleOcclusion;
    12: ViewMode := vmFull;
    20: with Scene.Attributes do Lighting := not Lighting;
    100: Window.SaveScreenDialog(FileNameAutoInc(SUnformattable(ApplicationName) + '_screen_%d.png'));
    200: Window.Close;
    else Exit;
  end;
  UpdateViewMode;
  Window.Invalidate;
end;

procedure Update(Container: TUIContainer);

  procedure ChangeLightPosition(Coord, Change: Integer);
  begin
    LightPos[Coord] += Change * Window.Fps.UpdateSecondsPassed *
      { scale by Box3DAvgSize, to get similar move on all models }
      Scene.BoundingBox.AverageSize;
    Window.Invalidate;
  end;

  procedure ChangeLightRadius(Change: Float);
  begin
    LightRadius *= Power(Change, Window.Fps.UpdateSecondsPassed);
    Window.Invalidate;
  end;

  procedure ChangeLightIntensityScale(Change: Float);
  begin
    LightIntensityScale *= Power(Change, Window.Fps.UpdateSecondsPassed);
    Window.Invalidate;
  end;

begin
  if Window.Pressed[K_A] then ChangeLightPosition(0, -1);
  if Window.Pressed[K_D] then ChangeLightPosition(0,  1);
  if Window.Pressed[K_S] then ChangeLightPosition(2, -1);
  if Window.Pressed[K_W] then ChangeLightPosition(2,  1);
  if Window.Pressed[K_Q] then ChangeLightPosition(1, -1);
  if Window.Pressed[K_E] then ChangeLightPosition(1,  1);

  if Window.Pressed[K_R] then
  begin
    if mkShift in Window.Pressed.Modifiers then
      ChangeLightRadius(1/1.8) else
      ChangeLightRadius(1.8);
  end;

  if Window.Pressed[K_L] then
  begin
    if mkShift in Window.Pressed.Modifiers then
      ChangeLightIntensityScale(1/1.5) else
      ChangeLightIntensityScale(1.5);
  end;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
  Radio: TMenuItemRadio;
  RadioGroup: TMenuItemRadioGroup;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');

    Radio := TMenuItemRadio.Create('_Normal (no PRT)', 10, ViewMode = vmNormal, true);
    RadioGroup := Radio.Group;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('_Simple Occlusion', 11, ViewMode = vmSimpleOcclusion, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('_Full Radiance Transfer', 12, ViewMode = vmFull, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Apply OpenGL _Lighting', 20, Scene.Attributes.Lighting, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Save Screen ...', 100, K_F5));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);
end;

var
  URL: string = 'data/chinchilla_with_prt.wrl.gz';
begin
  Window := TCastleWindowCustom.Create(Application);

  { parse command-line parameters }
  Window.ParseParameters;
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    URL := Parameters[1];

  RenderParams := TBasicRenderParams.Create;

  Scene := TCastleScene.Create(Application);
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  Scene.Load(URL);

  if Scene.BoundingBox.IsEmpty then
  begin
    LightRadius := 1;
    LightPos := Vector3Single(2, 0, 0);
  end else
  begin
    LightRadius := Scene.BoundingBox.AverageSize;
    LightPos := Scene.BoundingBox.Center;
    LightPos[0] += Scene.BoundingBox.Data[1][0] - Scene.BoundingBox.Data[0][0] + LightRadius;
  end;

  SceneManager := TMySceneManager.Create(Application);
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;

  Window.MainMenu := CreateMainMenu;
  Window.OnMenuClick := @MenuClick;

  Window.Controls.InsertFront(SceneManager);

  Window.OnOpen := @Open;
  Window.OnUpdate := @Update;
  Window.SetDemoOptions(K_F11, CharEscape, true);

  InitializeSHBasisMap;

  Window.OpenAndRun;

  FreeAndNil(RenderParams);
end.
