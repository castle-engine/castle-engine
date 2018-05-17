{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Terrain initialization and main UI and logic. }
unit TerrainInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleBoxes, CastleKeysMouse, CastleColors,
  CastleUtils, CastleWindow, CastleGLUtils, CastleParameters,
  CastleCameras, CastleVectors, CastleFilesUtils, CastleTerrain, CastleMessages,
  CastleStringUtils, CastleOnScreenMenu, CastleUIControls, CastleImages,
  CastleGLShaders, CastleGLImages, X3DFields, X3DNodes,
  CastleTransform, CastleFrustum, CastleSceneManager, CastleURIUtils,
  CastleRectangles, CastleControls, CastleRendererBaseTypes,
  CastleApplicationProperties, CastleLog, CastleScene, X3DLoad,
  TerrainScene;

type
  TTerrainType = (ttNoise, ttCasScript, ttImage, ttGrid);

var
  { global stuff }
  Window: TCastleWindow;
  Scene: TTerrainScene; //< terrain
  EnvironmentScene: TCastleScene; //< defines sky (background) and fog
  ExamineCamera: TExamineCamera;
  WalkCamera: TWalkCamera;
  CurrentTerrain: TTerrain;
  TerrainTypeRadio: TMenuItemRadioGroup;

  { settings }
  TerrainType: TTerrainType = ttNoise; {< this is tied with Terrain class }
  Wireframe: boolean = false;
  NoiseInterpolation: TNoiseInterpolation = niCosine;
  NoiseBlur: boolean = false;
  Subdivision: Cardinal = 6;
  Size: Single = 10.0;
  OnScreenMenuVisible: boolean = true;

procedure UpdateScene;
begin
  Scene.Regenerate(CurrentTerrain, 1 shl Subdivision + 1, Size);
end;

{ TBaseOnScreenMenu ---------------------------------------------------------- }

type
  { Base class for on-screen menus in this demo. }
  TBaseOnScreenMenu = class(TCastleOnScreenMenu)
  public
    SubdivisionSlider: TCastleIntegerSlider;
    SizeSlider: TCastleFloatSlider;
    ImageHeightScaleSlider: TCastleFloatSlider;

    constructor Create(AOwner: TComponent); override;

    procedure SubdivisionChanged(Sender: TObject);
    procedure SizeChanged(Sender: TObject);
    procedure ImageHeightScaleChanged(Sender: TObject);
  end;

constructor TBaseOnScreenMenu.Create(AOwner: TComponent);

  procedure ShaderSlider(const Name: string; const ValuePointer: PSingle;
    const Min, Max: Single);
  begin
    Add(Name, TCastleFloatSlider.Create(
      Self, ValuePointer, Min, Max, @Scene.UpdateShader));
  end;

var
  I: Integer;
begin
  inherited;

  { sliders used by both Controls* }
  SubdivisionSlider := TCastleIntegerSlider.Create(Self);
  SubdivisionSlider.Min := 2;
  SubdivisionSlider.Max := 10;
  SubdivisionSlider.Value := Subdivision;
  SubdivisionSlider.OnChange := @SubdivisionChanged;

  SizeSlider := TCastleFloatSlider.Create(Self);
  SizeSlider.Min := 5;
  SizeSlider.Max := 100;
  SizeSlider.Value := Size;
  SizeSlider.OnChange := @SizeChanged;

  ImageHeightScaleSlider := TCastleFloatSlider.Create(Self);
  ImageHeightScaleSlider.Min := 0.0;
  ImageHeightScaleSlider.Max := 2.0;
  ImageHeightScaleSlider.Value := 0.25;
  ImageHeightScaleSlider.OnChange := @ImageHeightScaleChanged;

  Anchor(hpLeft, 10);
  Anchor(vpBottom, 10);

  Add('Terrain shader:');

  for I := Low(Scene.TextureHeights) to High(Scene.TextureHeights) do
    ShaderSlider('Texture Height ' + IntToStr(I), @Scene.TextureHeights[I], 0, 10);
  for I := Low(Scene.UVScale) to High(Scene.UVScale) do
    ShaderSlider('UV Scale ' + IntToStr(I), @Scene.UVScale[I], 0.01, 0.5);
  ShaderSlider('Texture Mix', @Scene.TextureMix, 0, 1);
  ShaderSlider('Normal Dark', @Scene.NormalDark, 0, 1);
  ShaderSlider('Normal Darkening', @Scene.NormalDarkening, 0, 1);

  Add('Terrain shape:');
end;

procedure TBaseOnScreenMenu.SubdivisionChanged(Sender: TObject);
begin
  Subdivision := SubdivisionSlider.Value;
  UpdateScene;
end;

procedure TBaseOnScreenMenu.SizeChanged(Sender: TObject);
begin
  Size := SizeSlider.Value;
  UpdateScene;
  { TODO: Workaround: In case of UseTriangulatedNode = true,
    it seems that UpdateScene doesn't properly cause octree rebuild.
    But we need it for walk mode (WalkCamera) gravity to work. }
  Scene.ChangedAll;
end;

procedure TBaseOnScreenMenu.ImageHeightScaleChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainImage).ImageHeightScale := ImageHeightScaleSlider.Value;
  UpdateScene;
end;

{ TOnScreenMenuImage --------------------------------------------------------- }

type
  { Configure TTerrainImage parameters
    (but for TTerrainNoise, better use more specialized TOnScreenMenuNoise). }
  TOnScreenMenuImage = class(TBaseOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TOnScreenMenuImage.Create(AOwner: TComponent);
begin
  inherited;
  Add('Subdivision (render details)', SubdivisionSlider);
  Add('Size', SizeSlider);
  Add('Image scale (load image first)', ImageHeightScaleSlider);
end;

{ TOnScreenMenuNoise --------------------------------------------------------- }

type
  { Configure TTerrainNoise parameters }
  TOnScreenMenuNoise = class(TBaseOnScreenMenu)
  public
    OctavesSlider: TCastleFloatSlider;
    AmplitudeSlider: TCastleFloatSlider;
    FrequencySlider: TCastleFloatSlider;
    SmoothnessSlider: TCastleFloatSlider;
    HeterogeneousSlider: TCastleFloatSlider;
    SeedSlider: TCastleIntegerSlider;

    constructor Create(AOwner: TComponent); override;

    procedure OctavesChanged(Sender: TObject);
    procedure AmplitudeChanged(Sender: TObject);
    procedure FrequencyChanged(Sender: TObject);
    procedure SmoothnessChanged(Sender: TObject);
    procedure HeterogeneousChanged(Sender: TObject);
    procedure SeedChanged(Sender: TObject);
  end;

constructor TOnScreenMenuNoise.Create(AOwner: TComponent);
begin
  inherited;

  OctavesSlider := TCastleFloatSlider.Create(Self);
  OctavesSlider.Min := 0.0;
  OctavesSlider.Max := 20.0;
  OctavesSlider.Value := 7.0;
  OctavesSlider.OnChange := @OctavesChanged;

  SmoothnessSlider := TCastleFloatSlider.Create(Self);
  SmoothnessSlider.Min := 1.0;
  SmoothnessSlider.Max := 10.0;
  SmoothnessSlider.Value := 1.5;
  SmoothnessSlider.OnChange := @SmoothnessChanged;

  HeterogeneousSlider := TCastleFloatSlider.Create(Self);
  HeterogeneousSlider.Min := 0.0;
  HeterogeneousSlider.Max := 2.0;
  HeterogeneousSlider.Value := 0.5;
  HeterogeneousSlider.OnChange := @HeterogeneousChanged;

  AmplitudeSlider := TCastleFloatSlider.Create(Self);
  AmplitudeSlider.Min := 0.1;
  AmplitudeSlider.Max := 10.0;
  AmplitudeSlider.Value := 8.0;
  AmplitudeSlider.OnChange := @AmplitudeChanged;

  FrequencySlider := TCastleFloatSlider.Create(Self);
  FrequencySlider.Min := 0.001;
  FrequencySlider.Max := 0.1;
  FrequencySlider.Value := 0.05;
  FrequencySlider.OnChange := @FrequencyChanged;

  SeedSlider := TCastleIntegerSlider.Create(Self);
  SeedSlider.Min := 0;
  SeedSlider.Max := 99;
  SeedSlider.Value := 0;
  SeedSlider.OnChange := @SeedChanged;

  Add('Octaves', OctavesSlider);
  Add('Smoothness', SmoothnessSlider);
  Add('Heterogeneous', HeterogeneousSlider);
  Add('Amplitude (scales height)', AmplitudeSlider);
  Add('Frequency (scales size)', FrequencySlider);
  Add('Seed', SeedSlider);
  Add('Subdivision (render details)', SubdivisionSlider);
  Add('Size', SizeSlider);
  Add('Image scale (load image first)', ImageHeightScaleSlider);
end;

procedure TOnScreenMenuNoise.OctavesChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Octaves := OctavesSlider.Value;
  UpdateScene;
end;

procedure TOnScreenMenuNoise.SmoothnessChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Smoothness := SmoothnessSlider.Value;
  UpdateScene;
end;

procedure TOnScreenMenuNoise.HeterogeneousChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Heterogeneous := HeterogeneousSlider.Value;
  UpdateScene;
end;

procedure TOnScreenMenuNoise.AmplitudeChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Amplitude := AmplitudeSlider.Value;
  UpdateScene;
end;

procedure TOnScreenMenuNoise.FrequencyChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Frequency := FrequencySlider.Value;
  UpdateScene;
end;

procedure TOnScreenMenuNoise.SeedChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Seed := SeedSlider.Value;
  UpdateScene;
end;

{ TOnScreenMenuGeneral ------------------------------------------------------- }

type
  { Configure any TCurrentTerrain. }
  TOnScreenMenuGeneral = class(TBaseOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
  end;
constructor TOnScreenMenuGeneral.Create(AOwner: TComponent);
begin
  inherited;
  Add('Subdivision (render details)', SubdivisionSlider);
  Add('Size', SizeSlider);
  // do not include ImageHeightScaleSlider, should not be used with this terrain
end;

{ global routines ------------------------------------------------------------ }

var
  { UI controls }
  OnScreenMenuNoise: TOnScreenMenuNoise;
  OnScreenMenuImage: TOnScreenMenuImage;
  OnScreenMenuGeneral: TOnScreenMenuGeneral;
  SceneManager: TCastleSceneManager;

{ Current TCastleOnScreenMenu, or none, based on Terrain class and OnScreenMenuVisible. }
function CurrentOnScreenMenu: TBaseOnScreenMenu;
begin
  if not OnScreenMenuVisible then Exit(nil);

  if CurrentTerrain is TTerrainNoise then
    Result := OnScreenMenuNoise
  else
  if CurrentTerrain is TTerrainImage then
    Result := OnScreenMenuImage
  else
    Result := OnScreenMenuGeneral;
end;

procedure SetTerrain(Value: TTerrain);
begin
  if CurrentTerrain <> Value then
  begin
    FreeAndNil(CurrentTerrain);
    CurrentTerrain := Value;

    Window.Controls.MakeSingle(TBaseOnScreenMenu, CurrentOnScreenMenu, true);

    if CurrentTerrain is TTerrainImage then
      TTerrainImage(CurrentTerrain).ImageHeightScale := CurrentOnScreenMenu.ImageHeightScaleSlider.Value;

    if CurrentTerrain is TTerrainNoise then
    begin
      TTerrainNoise(CurrentTerrain).Octaves := OnScreenMenuNoise.OctavesSlider.Value;
      TTerrainNoise(CurrentTerrain).Smoothness := OnScreenMenuNoise.SmoothnessSlider.Value;
      TTerrainNoise(CurrentTerrain).Heterogeneous := OnScreenMenuNoise.HeterogeneousSlider.Value;
      TTerrainNoise(CurrentTerrain).Amplitude := OnScreenMenuNoise.AmplitudeSlider.Value;
      TTerrainNoise(CurrentTerrain).Frequency := OnScreenMenuNoise.FrequencySlider.Value;
      TTerrainNoise(CurrentTerrain).Seed := OnScreenMenuNoise.SeedSlider.Value;
      TTerrainNoise(CurrentTerrain).Interpolation := NoiseInterpolation;
      TTerrainNoise(CurrentTerrain).Blur := NoiseBlur;
    end;

    if CurrentTerrain is TTerrainGrid then
    begin
      { Scale grid coords to fit in a similar box as random generation
        from TTerrainNoise produces. }
      TTerrainGrid(CurrentTerrain).GridX1 := -1;
      TTerrainGrid(CurrentTerrain).GridY1 := -1;
      TTerrainGrid(CurrentTerrain).GridX2 := 1;
      TTerrainGrid(CurrentTerrain).GridY2 := 1;
      TTerrainGrid(CurrentTerrain).GridHeightScale := 0.0002;
    end;

    { calculate TerrainType }
    if CurrentTerrain is TTerrainGrid then
      TerrainType := ttGrid else
    if CurrentTerrain is TTerrainNoise then
      TerrainType := ttNoise else
    if CurrentTerrain is TTerrainCasScript then
      TerrainType := ttCasScript else
    if CurrentTerrain is TTerrainImage then
      TerrainType := ttImage else
      raise EInternalError.Create('Unknown TerrainType from Terrain class');

    { set TerrainTypeRadio.Selected (since these radios do not have AutoChecked,
      and this is good (because some have Ok/Cancel dialog and user can
      cancel)). }
    TerrainTypeRadio.Selected := TerrainTypeRadio.Items[Ord(TerrainType)];

    UpdateScene;
  end;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);

  procedure ExportToX3D(const URL: string; const UseTriangulatedNode: boolean);
  var
    OldUseTriangulatedNode: boolean;
  begin
    if UseTriangulatedNode = Scene.UseTriangulatedNode then
      Scene.Save(URL)
    else
    begin
      { we could write it shorter,
        knowing that OldUseTriangulatedNode = not UseTriangulatedNode in this case,
        but it seems more dirty. }

      OldUseTriangulatedNode := Scene.UseTriangulatedNode;
      Scene.UseTriangulatedNode := UseTriangulatedNode;
      UpdateScene;

      Scene.Save(URL);

      Scene.UseTriangulatedNode := OldUseTriangulatedNode;
      UpdateScene;
    end;
  end;

  procedure ToggleWalk;
  var
    MoveLimit: TBox3D;
    Y: Single;
  begin
    if SceneManager.Camera = ExamineCamera then
    begin
      SceneManager.Camera := WalkCamera;

      Y := CurrentTerrain.Height(0, 0) + WalkCamera.PreferredHeight;
      WalkCamera.SetView(
        Vector3(0, Y, 0),
        Vector3(0, 0, -1),
        Vector3(0, 1, 0));

      // make gravity work even if your position is over the world bbox
      MoveLimit := SceneManager.Items.BoundingBox;
      MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
      SceneManager.MoveLimit := MoveLimit;
    end else
      SceneManager.Camera := ExamineCamera;
  end;

  procedure ToggleFog;
  var
    FogNode: TFogNode;
  begin
    FogNode := EnvironmentScene.Node('MainFog') as TFogNode;
    FogNode.Bound := not FogNode.Bound;
  end;

var
  URL: string;
  Expression: string;
  NewTerrain: TTerrain;
begin
  case Item.IntData of
    10: begin
          Wireframe := not Wireframe;
          if Wireframe then
            Scene.Attributes.WireframeEffect := weWireframeOnly
          else
            Scene.Attributes.WireframeEffect := weNormal;
        end;
    50: begin
          URL := '';
          if Window.FileDialog('Open SRTM (.hgt) terrain file', URL, true,
            'All Files|*|' +
            '*SRTM (*.hgt) terrain|*.hgt') then
          begin
            try
              NewTerrain := TTerrainSRTM.CreateFromFile(URL);
            except
              on E: Exception do
              begin
                MessageOk(Window, Format('Error when loading file "%s" as terrain: %s',
                  [URL, E.Message]));
                Exit;
              end;
            end;

            SetTerrain(NewTerrain);
          end;
        end;
    60: begin
          Expression := '';
          if MessageInputQuery(Window, 'Pass CastleScript function expression, using X and Y variables, and calculating height of the terrain at given point.' + nl + nl +
            '(For example, try "sin(x*2) * sin(y*2)").', Expression) then
          begin
            try
              NewTerrain := TTerrainCasScript.Create(Expression);
            except
              on E: Exception do
              begin
                MessageOk(Window, Format('Error when interpreting expression: %s',
                  [E.Message]));
                Exit;
              end;
            end;

            SetTerrain(NewTerrain);
          end;
        end;
    70: SetTerrain(TTerrainNoise.Create);
    80: SetTerrain(TTerrainImage.Create);
    100: Window.Close;
    110:
      begin
        NoiseBlur := not NoiseBlur;
        if CurrentTerrain is TTerrainNoise then
        begin
          (CurrentTerrain as TTerrainNoise).Blur := NoiseBlur;
          UpdateScene;
        end;
      end;
    120: ToggleWalk;
    130: Scene.Lighting := not Scene.Lighting;
    140: Scene.Textured := not Scene.Textured;
    142: ToggleFog;
    145:
      begin
        OnScreenMenuVisible := not OnScreenMenuVisible;
        Window.Controls.MakeSingle(TBaseOnScreenMenu, CurrentOnScreenMenu, true);
      end;
    150:
      begin
        if CurrentTerrain is TTerrainImage then
        begin
          URL := TTerrainImage(CurrentTerrain).ImageURL;
          if Window.FileDialog('Open image file', URL, true, LoadImage_FileFilters) then
          begin
            try
              TTerrainImage(CurrentTerrain).LoadImage(URL);
              UpdateScene;
            except
              on E: Exception do
              begin
                MessageOk(Window, Format('Error when loading image "%s": %s',
                  [URL, E.Message]));
              end;
            end;
          end;
        end else
          MessageOk(Window, 'Not an image terrain');
      end;
    160:
      begin
        if CurrentTerrain is TTerrainImage then
        begin
          TTerrainImage(CurrentTerrain).ClearImage;
          UpdateScene;
        end else
          MessageOk(Window, 'Not an image terrain');
      end;
    200..299:
      begin
        NoiseInterpolation := TNoiseInterpolation(Item.IntData - 200);
        if CurrentTerrain is TTerrainNoise then
        begin
          (CurrentTerrain as TTerrainNoise).Interpolation := NoiseInterpolation;
          UpdateScene;
        end;
      end;
    300:
      begin
        Scene.UseTriangulatedNode := not Scene.UseTriangulatedNode;
        UpdateScene;
      end;
    1000:
      begin
        URL := '';
        if Window.FileDialog('Save terrain to', URL, false, SaveX3D_FileFilters) then
          ExportToX3D(URL, false);
      end;
    1001:
      begin
        URL := '';
        if Window.FileDialog('Save terrain to', URL, false, SaveX3D_FileFilters) then
          ExportToX3D(URL, true);
      end;
  end;
end;

function CreateMainMenu: TMenu;
const
  NoiseInterpolationNames: array [TNoiseInterpolation] of string =
  ('None', 'Linear', 'Cosine (Default)', 'Spline');
var
  M: TMenu;
  Radio: TMenuItemRadio;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    Radio := TMenuItemRadio.Create('Random terrain', 70, TerrainType = ttNoise, false);
    TerrainTypeRadio := Radio.Group;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Terrain defined by CastleScript expression ...', 60, TerrainType = ttCasScript, false);
    Radio.Group := TerrainTypeRadio;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Pure image terrain', 80, TerrainType = ttImage, false);
    Radio.Group := TerrainTypeRadio;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Terrain from SRTM (.hgt) file ...', 50, CtrlO, TerrainType = ttGrid, false);
    Radio.Group := TerrainTypeRadio;
    M.Append(Radio);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Export to _X3D (ElevationGrid) ...', 1000));
    M.Append(TMenuItem.Create('Export to _X3D (IndexedTriangleStripSet) ...', 1001));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Walk (AWSD, mose look)', 120, 'c', false { SceneManager.Camera = WalkCamera }, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 100, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('_Wireframe', 10, 'e', Wireframe, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Use IndexedTriangleStripSet node', 300,
      true { Scene.UseTriangulatedNode }, true));
    M.Append(TMenuItemChecked.Create('Lighting', 130, 'l',
      true { Scene.Lighting }, true));
    M.Append(TMenuItemChecked.Create('Textured', 140, 't',
      true { Scene.Textured }, true));
    M.Append(TMenuItemChecked.Create('Fog', 142, 'f',
      true { default Fog on EnvironmentScene }, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Sliders Visible', 145, K_F1, OnScreenMenuVisible, true));
    Result.Append(M);
  M := TMenu.Create('_Noise');
    M.AppendRadioGroup(NoiseInterpolationNames, 200, Ord(NoiseInterpolation), true);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Blur', 110, NoiseBlur, true));
    Result.Append(M);
  M := TMenu.Create('_Image');
    M.Append(TMenuItem.Create('_Load image', 150));
    M.Append(TMenuItem.Create('_Clear image', 160));
    Result.Append(M);
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { automatically scale user interface to reference sizes }
  Window.Container.UIReferenceWidth := 1600;
  Window.Container.UIReferenceHeight := 900;
  Window.Container.UIScaling := usEncloseReferenceSize;

  ExamineCamera := TExamineCamera.Create(Window);
  ExamineCamera.Init(Box3D(
    Vector3(-1, -1, -1),
    Vector3( 1,  1,  1)), { Radius } 0.2);
  ExamineCamera.SetView(
    Vector3(0, 20, 0),
    Vector3(0, -1, 0),
    Vector3(0, 0, -1)
  );

  WalkCamera := TWalkCamera.Create(Window);
  WalkCamera.Init(
    Vector3(0, 10, 0),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0),
    Vector3(0, 1, 0),
    { PreferredHeight } 2,
    { Radius } 0.02);
  WalkCamera.MoveSpeed := 10.0;
  WalkCamera.Gravity := true;
  WalkCamera.MouseLook := true;

  Scene := TTerrainScene.Create(Window);
  Scene.Spatial := [ssDynamicCollisions]; // for proper walking

  SceneManager := Window.SceneManager;
  SceneManager.Items.Add(Scene);
  SceneManager.Camera := ExamineCamera;

  EnvironmentScene := TCastleScene.Create(Window);
  EnvironmentScene.Load(ApplicationData('environment/environment.x3dv'));
  SceneManager.Items.Add(EnvironmentScene);
  SceneManager.MainScene := EnvironmentScene;

  Window.OnMenuClick := @MenuClick;
  Window.FpsShowOnCaption := true;

  OnScreenMenuNoise := TOnScreenMenuNoise.Create(nil);
  OnScreenMenuImage := TOnScreenMenuImage.Create(nil);
  OnScreenMenuGeneral := TOnScreenMenuGeneral.Create(nil);

  if CurrentTerrain = nil then
  begin
    if Parameters.High = 1 then
      SetTerrain(TTerrainSRTM.CreateFromFile(Parameters[1]))
    else
      { some default terrain }
      SetTerrain(TTerrainNoise.Create);
  end;
end;

initialization
  { Set ApplicationName and Version early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'terrain';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  Window.MainMenu := CreateMainMenu;
  Window.Caption := ApplicationName;
finalization
  FreeAndNil(CurrentTerrain);
  FreeAndNil(OnScreenMenuNoise);
  FreeAndNil(OnScreenMenuImage);
  FreeAndNil(OnScreenMenuGeneral);
end.
