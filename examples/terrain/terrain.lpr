{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display, walk over the terrain.

  Terrain data may be obtained from various sources:

  1. There's terrain data reader from a very simple SRTM-3 *.hgt file
     (pass $1 an URL (just a filename in simple cases) of .hgt file).

     See http://www2.jpl.nasa.gov/srtm/, see (linked there)
     http://dds.cr.usgs.gov/srtm/ for sample data for whole Earth.
     In you speak Polish, nice overview is also on
     http://netgis.geo.uw.edu.pl/srtm/.
     Sample files for Poland are on http://netgis.geo.uw.edu.pl/srtm/Poland/,
     for Europe http://netgis.geo.uw.edu.pl/srtm/Europe/.

     You can run the program with command-line parameter to pass URL
     of such .hgt file to load on start.

  2. You can also define terrain as an explicit function using CastleScript
     expression syntax, [http://castle-engine.sourceforge.net/castle_script.php].
     Try e.g. function like
     - sin(x) + sin(y)
     - (sin(x) + sin(x*2) / 2 + sin(x*4) / 4)  *
       (sin(y) + sin(y*2) / 2 + sin(y*4) / 4)
       (sum sinusoides of various frequencies and amplitudes).

  3. You can also (and this was actually the primary reason for this program
     in the 1st place) generate random terrain.

  Terrain is rendered with GLSL shader, with nice colors, multitextures etc.
  To support large data, we render with ultra-simplified geo clipmaps
  (look in wireframe mode to see what it does). View is centered around
  current Walk position (move in Walk mode to also change the portion
  visible in Examine mode).

  TODO:
  - add sky, just for the effect?
}
program terrain;

{$I castleconf.inc}

uses SysUtils, Classes, CastleBoxes, CastleKeysMouse, CastleColors,
  CastleUtils, CastleWindow, CastleGL, CastleGLUtils, CastleParameters,
  CastleCameras, CastleVectors, CastleFilesUtils, CastleTerrain, CastleMessages,
  CastleStringUtils, CastleOnScreenMenu, CastleUIControls, CastleImages,
  RenderTerrains, CastleGLShaders, CastleGLImages, X3DFields, X3DNodes,
  Castle3D, CastleFrustum, CastleSceneManager, CastleURIUtils,
  CastleRectangles, CastleControls;

type
  TTerrainType = (ttNoise, ttCasScript, ttImage, ttGrid);

var
  { global stuff }
  Window: TCastleWindow;
  ExamineCamera: TExamineCamera;
  WalkCamera: TWalkCamera;
  CurrentTerrain: TTerrain;
  TerrainTypeRadio: TMenuItemRadioGroup;

  { gl helpers }
  GLSLProgram: TGLSLProgram;
  GLTexSand, GLTexBread, GLTexRock: TGLuint;

  { settings }
  TerrainType: TTerrainType = ttNoise; {< this is tied with Terrain class }
  Wireframe: boolean = false;
  NoiseInterpolation: TNoiseInterpolation = niCosine;
  NoiseBlur: boolean = false;
  Subdivision: Cardinal = 6;
  Shader: boolean = true;
  Lighting: boolean = true;
  KeepCameraAboveGround: boolean = true;
  BaseSize: Single = 1.0;
  LayersCount: Cardinal = 3;
  ControlsVisible: boolean = true;
  Fog: boolean = false;
  BackgroundColor: TCastleColor;
  SpecializedGridRendering: boolean = true;

type
  TCommonMenu = class(TCastleOnScreenMenu)
  public
    SubdivisionSlider: TCastleIntegerSlider;
    BaseSizeSlider: TCastleFloatSlider;
    LayersCountSlider: TCastleIntegerSlider;
    ImageHeightScaleSlider: TCastleFloatSlider;

    constructor Create(AOwner: TComponent); override;

    procedure SubdivisionChanged(Sender: TObject);
    procedure BaseSizeChanged(Sender: TObject);
    procedure LayersCountChanged(Sender: TObject);
    procedure ImageHeightScaleChanged(Sender: TObject);
  end;

  TControlsNoise = class(TCommonMenu)
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

  { For any TTerrainImage except
    TTerrainNoise (that has it's own TControlsNoise). }
  TControlsImage = class(TCommonMenu)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { For any TCurrentTerrain. }
  TControlsGeneral = class(TCommonMenu)
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure TCommonMenu.SubdivisionChanged(Sender: TObject);
begin
  Subdivision := SubdivisionSlider.Value;
end;

procedure TCommonMenu.BaseSizeChanged(Sender: TObject);
begin
  BaseSize := BaseSizeSlider.Value;
end;

procedure TCommonMenu.LayersCountChanged(Sender: TObject);
begin
  LayersCount := LayersCountSlider.Value;
end;

procedure TCommonMenu.ImageHeightScaleChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainImage).ImageHeightScale := ImageHeightScaleSlider.Value;
end;

procedure TControlsNoise.OctavesChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Octaves := OctavesSlider.Value;
end;

procedure TControlsNoise.SmoothnessChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Smoothness := SmoothnessSlider.Value;
end;

procedure TControlsNoise.HeterogeneousChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Heterogeneous := HeterogeneousSlider.Value;
end;

procedure TControlsNoise.AmplitudeChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Amplitude := AmplitudeSlider.Value;
end;

procedure TControlsNoise.FrequencyChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Frequency := FrequencySlider.Value;
end;

procedure TControlsNoise.SeedChanged(Sender: TObject);
begin
  (CurrentTerrain as TTerrainNoise).Seed := SeedSlider.Value;
end;

constructor TCommonMenu.Create(AOwner: TComponent);
begin
  inherited;

  { sliders used by both Controls* }
  SubdivisionSlider := TCastleIntegerSlider.Create(Self);
  SubdivisionSlider.Min := 2;
  SubdivisionSlider.Max := 10;
  SubdivisionSlider.Value := Subdivision;
  SubdivisionSlider.OnChange := @SubdivisionChanged;

  BaseSizeSlider := TCastleFloatSlider.Create(Self);
  BaseSizeSlider.Min := 0.1;
  BaseSizeSlider.Max := 20;
  BaseSizeSlider.Value := BaseSize;
  BaseSizeSlider.OnChange := @BaseSizeChanged;

  LayersCountSlider := TCastleIntegerSlider.Create(Self);
  LayersCountSlider.Min := 1;
  LayersCountSlider.Max := 10;
  LayersCountSlider.Value := LayersCount;
  LayersCountSlider.OnChange := @LayersCountChanged;

  ImageHeightScaleSlider := TCastleFloatSlider.Create(Self);
  ImageHeightScaleSlider.Min := 0.0;
  ImageHeightScaleSlider.Max := 2.0;
  ImageHeightScaleSlider.Value := 0.25;
  ImageHeightScaleSlider.OnChange := @ImageHeightScaleChanged;

  Anchor(hpLeft, 10);
  Anchor(vpBottom, 10);
end;

constructor TControlsNoise.Create(AOwner: TComponent);
begin
  inherited;

  OctavesSlider := TCastleFloatSlider.Create(Self);
  OctavesSlider.Min := 0.0;
  OctavesSlider.Max := 20.0;
  OctavesSlider.Value := 4.0;
  OctavesSlider.OnChange := @OctavesChanged;

  SmoothnessSlider := TCastleFloatSlider.Create(Self);
  SmoothnessSlider.Min := 1.0;
  SmoothnessSlider.Max := 10.0;
  SmoothnessSlider.Value := 2.0;
  SmoothnessSlider.OnChange := @SmoothnessChanged;

  HeterogeneousSlider := TCastleFloatSlider.Create(Self);
  HeterogeneousSlider.Min := 0.0;
  HeterogeneousSlider.Max := 2.0;
  HeterogeneousSlider.Value := 0.0;
  HeterogeneousSlider.OnChange := @HeterogeneousChanged;

  AmplitudeSlider := TCastleFloatSlider.Create(Self);
  AmplitudeSlider.Min := 0.1;
  AmplitudeSlider.Max := 10.0;
  AmplitudeSlider.Value := 1.0;
  AmplitudeSlider.OnChange := @AmplitudeChanged;

  FrequencySlider := TCastleFloatSlider.Create(Self);
  FrequencySlider.Min := 0.5;
  FrequencySlider.Max := 10.0;
  FrequencySlider.Value := 1.0;
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
  Add('Size of the most detailed layer (and export)', BaseSizeSlider);
  Add('Layers Count (render farther)', LayersCountSlider);
  Add('Image scale (load image first)', ImageHeightScaleSlider);
end;

constructor TControlsImage.Create(AOwner: TComponent);
begin
  inherited;
  Add('Subdivision (render details)', SubdivisionSlider);
  Add('Size of the most detailed layer (and export)', BaseSizeSlider);
  Add('Layers Count (render farther)', LayersCountSlider);
  Add('Image scale (load image first)', ImageHeightScaleSlider);
end;

constructor TControlsGeneral.Create(AOwner: TComponent);
begin
  inherited;
  Add('Subdivision (render details)', SubdivisionSlider);
  Add('Size of the most detailed layer (and export)', BaseSizeSlider);
  Add('Layers Count (render farther)', LayersCountSlider);
  // do not include ImageHeightScaleSlider, should not be used with this terrain
end;

var
  { ui controls }
  ControlsNoise: TControlsNoise;
  ControlsImage: TControlsImage;
  ControlsGeneral: TControlsGeneral;
  SceneManager: TCastleSceneManager;

{ Current TCastleOnScreenMenu, or none, based on Terrain class and ControlsVisible. }
function CurrentControls: TCommonMenu;
begin
  if not ControlsVisible then Exit(nil);

  if CurrentTerrain is TTerrainNoise then
    Result := ControlsNoise else
  if CurrentTerrain is TTerrainImage then
    Result := ControlsImage else
    Result := ControlsGeneral;
end;

procedure SetTerrain(Value: TTerrain);
begin
  if CurrentTerrain <> Value then
  begin
    FreeAndNil(CurrentTerrain);
    CurrentTerrain := Value;

    Window.Controls.MakeSingle(TCommonMenu, CurrentControls, true);

    if CurrentTerrain is TTerrainImage then
      TTerrainImage(CurrentTerrain).ImageHeightScale := CurrentControls.ImageHeightScaleSlider.Value;

    if CurrentTerrain is TTerrainNoise then
    begin
      TTerrainNoise(CurrentTerrain).Octaves := ControlsNoise.OctavesSlider.Value;
      TTerrainNoise(CurrentTerrain).Smoothness := ControlsNoise.SmoothnessSlider.Value;
      TTerrainNoise(CurrentTerrain).Heterogeneous := ControlsNoise.HeterogeneousSlider.Value;
      TTerrainNoise(CurrentTerrain).Amplitude := ControlsNoise.AmplitudeSlider.Value;
      TTerrainNoise(CurrentTerrain).Frequency := ControlsNoise.FrequencySlider.Value;
      TTerrainNoise(CurrentTerrain).Seed := ControlsNoise.SeedSlider.Value;
      TTerrainNoise(CurrentTerrain).Interpolation := NoiseInterpolation;
      TTerrainNoise(CurrentTerrain).Blur := NoiseBlur;
    end;

    if CurrentTerrain is TTerrainGrid then
    begin
      TTerrainGrid(CurrentTerrain).GridX1 := GridX1;
      TTerrainGrid(CurrentTerrain).GridY1 := GridY1;
      TTerrainGrid(CurrentTerrain).GridX2 := GridX2;
      TTerrainGrid(CurrentTerrain).GridY2 := GridY2;
      TTerrainGrid(CurrentTerrain).GridHeightScale := GridHeightScale;
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
  end;
end;

type
  T3DTerrain = class(T3D)
    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

procedure T3DTerrain.Render(const Frustum: TFrustum; const Params: TRenderParams);

  procedure WalkCameraAboveGround;
  var
    P: TVector3Single;
  begin
    P := WalkCamera.Position;
    P[2] := CurrentTerrain.Height(P[0], P[1]) + 0.1;
    WalkCamera.Position := P;
  end;

var
  VisibilityEnd: Single;
begin
  if Params.Transparent or (not Params.ShadowVolumesReceivers) then Exit;

  if (SceneManager.Camera = WalkCamera) and KeepCameraAboveGround then
    WalkCameraAboveGround;

  if Wireframe then
  begin
    glPushAttrib(GL_POLYGON_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  end else
  begin
    glPushAttrib(GL_ENABLE_BIT);
    glEnable(GL_DEPTH_TEST);
  end;

  glPushMatrix;
    glMultMatrix(Params.RenderTransform);

    if (CurrentTerrain is TTerrainGrid) and
       SpecializedGridRendering then
    begin
      { DrawGrid cannot make normals for now, so no LIGHTING or other stuff }
      DrawGrid(TTerrainGrid(CurrentTerrain));
    end else
    begin
      glPushAttrib(GL_ENABLE_BIT);
        if Lighting then
        begin
          glEnable(GL_LIGHTING);
          glEnable(GL_LIGHT0);
          glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
          glEnable(GL_COLOR_MATERIAL);
          glEnable(GL_NORMALIZE);
        end;

        if Shader and (GLSLProgram <> nil) then
        begin
          CurrentProgram := GLSLProgram;

          glActiveTexture(GL_TEXTURE0);
          glBindTexture(GL_TEXTURE_2D, GLTexSand);
          GLSLProgram.SetUniform('tex_sand', 0);

          glActiveTexture(GL_TEXTURE1);
          glBindTexture(GL_TEXTURE_2D, GLTexBread);
          GLSLProgram.SetUniform('tex_bread', 1);

          glActiveTexture(GL_TEXTURE2);
          glBindTexture(GL_TEXTURE_2D, GLTexRock);
          GLSLProgram.SetUniform('tex_rock', 2);

          GLSLProgram.SetUniform('z0', 0.8);
          GLSLProgram.SetUniform('z1', 1.0);
          GLSLProgram.SetUniform('z2', 1.2);
          GLSLProgram.SetUniform('z3', 1.5);
          GLSLProgram.SetUniform('color_scale', 0.2);
          GLSLProgram.SetUniform('tex_scale', 0.8);
          if Fog then
          begin
            { This is the exact visibility end.
              In practice, we may want to make it a litlle smaller
              (to hide RoundGridCell hack in renderterrains.pas),
              or ignore problems and make it a little larger
              (otherwise, we needlessly render corners of terrain squares). }
            VisibilityEnd := BaseSize * (1 shl (LayersCount-1));
            GLSLProgram.SetUniform('fog_start', VisibilityEnd * 0.7);
            GLSLProgram.SetUniform('fog_end', VisibilityEnd);
            GLSLProgram.SetUniform('fog_color', Vector3SingleCut(BackgroundColor));
          end;
        end else
          CurrentProgram := nil;

        DrawTerrain(CurrentTerrain, Subdivision,
          WalkCamera.Position[0], WalkCamera.Position[1], BaseSize, LayersCount);
      glPopAttrib;
    end;

  glPopMatrix;

  glPopAttrib;
end;

function T3DTerrain.BoundingBox: TBox3D;
{ Instead of trying to figure out what is a suitable bounding box,
  just assume we fill the whole 3D space.
  It must be large to always consider terrain within frustum,
  and to force TCastleSceneManager.Projection implementation
  to calculate sufficiently large ProjectionFar. }
const
  M = 10000;
  InfiniteBox: TBox3D = (Data:((-M, -M, -M), (M, M, M)));
begin
  Result := InfiniteBox;
end;

{ Load GLSL program from files, add #define if Fog, relink. }
procedure GLSLProgramRegenerate;
var
  Prefix: string;
begin
  if GLSLProgram = nil then
  begin
    Writeln('Warning: Your graphic card doesn''t support GLSL shaders.');
    Exit;
  end;

  GLSLProgram.DetachAllShaders;
  Prefix := '#define HEIGHT_IS_Z' + NL;
  if Fog then Prefix += '#define FOG' + NL;
  GLSLProgram.AttachVertexShader(Prefix + FileToString(ApplicationData('terrain.vs')));
  GLSLProgram.AttachFragmentShader(Prefix + FileToString(ApplicationData('terrain.fs')));
  { For this test program, we eventually allow shader to run in software.
    We display debug info, so user should know what's going on. }
  GLSLProgram.Link;
  { Only warn on non-used uniforms. This is more comfortable for shader
    development, you can easily comment shader parts. }
  GLSLProgram.UniformNotFoundAction := uaWarning;
  Writeln('----------------------------- Shader debug info:');
  Writeln(GLSLProgram.DebugInfo);
end;

procedure Open(Container: TUIContainer);

  function LoadTexture(const Name: string): TGLuint;
  begin
    Result := LoadGLTexture(ApplicationData('textures/' + Name),
      TextureFilter(minLinearMipmapLinear, magLinear), Texture2DRepeat);
  end;

begin
  RenderTerrainsOpenGL;

  ControlsNoise := TControlsNoise.Create(nil);
  ControlsImage := TControlsImage.Create(nil);
  ControlsGeneral := TControlsGeneral.Create(nil);

  if CurrentTerrain = nil then
  begin
    if Parameters.High = 1 then
      SetTerrain(TTerrainSRTM.CreateFromFile(Parameters[1])) else
      { some default terrain }
      SetTerrain(TTerrainNoise.Create);
  end;

  { load textures }
  GLTexSand := LoadTexture('sand.png');
  GLTexBread := LoadTexture('bread.png');
  GLTexRock := LoadTexture('rock_d01.png');

  { initialize GLSL program }
  if TGLSLProgram.ClassSupport <> gsNone then
    GLSLProgram := TGLSLProgram.Create else
    Shader := false;
  GLSLProgramRegenerate;
end;

procedure Close(Container: TUIContainer);
begin
  FreeAndNil(GLSLProgram);

  FreeAndNil(ControlsNoise);
  FreeAndNil(ControlsImage);
  FreeAndNil(ControlsGeneral);

  RenderTerrainsCloseGL;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);

  procedure ExportToX3D(const URL: string;
    const AddShadersTextures: boolean);
  var
    CountSteps: Cardinal;
    Size: Single;
    XRange, ZRange: TVector2Single;
    Root: TX3DRootNode;
    Shape: TShapeNode;
    Shader: TComposedShaderNode;
    TexSand: TImageTextureNode;
    TexBread: TImageTextureNode;
    TexRock: TImageTextureNode;
    Part: TShaderPartNode;
  begin
    CountSteps := 1 shl Subdivision + 1;
    Size := BaseSize * 2;
    { Note about XY (our TTerrain) -> XZ (X3D TerrainNode) conversion:
      we change Y into Z, this also means that Z must go into the other
      direction (when Y increases, Z decreases) to produce the same look. }
    XRange[0] := WalkCamera.Position[0] - Size/2;
    ZRange[0] := WalkCamera.Position[1] + Size/2; // Z direction is inverted
    XRange[1] := WalkCamera.Position[0] + Size/2;
    ZRange[1] := WalkCamera.Position[1] - Size/2; // Z direction is inverted

    Root := TX3DRootNode.Create;
    try
      Shape := CurrentTerrain.CreateNode(CountSteps, Size, XRange, ZRange,
        @ColorFromHeight);
      Root.FdChildren.Add(Shape);

      if AddShadersTextures then
      begin
        Shader := TComposedShaderNode.Create;
        Shape.Appearance.FdShaders.Add(Shader);
        Shader.FdLanguage.Value := 'GLSL';

        { Add shader. Setup everything, like for rendering (without fog). }
        TexSand := TImageTextureNode.Create;
        TexSand.FdUrl.Items.Add('textures/sand.png');
        TexBread := TImageTextureNode.Create;
        TexBread.FdUrl.Items.Add('textures/bread.png');
        TexRock := TImageTextureNode.Create;
        TexRock.FdUrl.Items.Add('textures/rock_d01.png');
        Shader.AddCustomField(TSFNode.Create(Shader, 'tex_sand', [], TexSand));
        Shader.AddCustomField(TSFNode.Create(Shader, 'tex_bread', [], TexBread));
        Shader.AddCustomField(TSFNode.Create(Shader, 'tex_rock', [], TexRock));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'z0', 0.8));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'z1', 1.0));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'z2', 1.2));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'z3', 1.5));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'color_scale', 0.2));
        Shader.AddCustomField(TSFFloat.Create(Shader, 'tex_scale', 0.8));

        Part := TShaderPartNode.Create;
        Shader.FdParts.Add(Part);
        Part.FdType.Value := 'FRAGMENT';
        Part.FdUrl.Items.Add('terrain.fs');

        Part := TShaderPartNode.Create;
        Shader.FdParts.Add(Part);
        Part.FdType.Value := 'VERTEX';
        Part.FdUrl.Items.Add('terrain.vs');
      end;

      Save3D(Root, URL, 'terrain', '', xeClassic);
    finally FreeAndNil(Root) end;
  end;

var
  URL: string;
  Expression: string;
  NewTerrain: TTerrain;
begin
  case Item.IntData of
    10: Wireframe := not Wireframe;
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
          (CurrentTerrain as TTerrainNoise).Blur := NoiseBlur;
      end;
    120:
      begin
        if SceneManager.Camera = ExamineCamera then
          SceneManager.Camera := WalkCamera else
          SceneManager.Camera := ExamineCamera;
      end;
    125: KeepCameraAboveGround := not KeepCameraAboveGround;
    130: Lighting := not Lighting;
    140: Shader := not Shader;
    142:
      begin
        Fog := not Fog;
        GLSLProgramRegenerate;
      end;
    145:
      begin
        ControlsVisible := not ControlsVisible;
        Window.Controls.MakeSingle(TCommonMenu, CurrentControls, true);
      end;
    147: SpecializedGridRendering := not SpecializedGridRendering;
    150:
      begin
        if CurrentTerrain is TTerrainImage then
        begin
          URL := TTerrainImage(CurrentTerrain).ImageURL;
          if Window.FileDialog('Open image file', URL, true,
            LoadImage_FileFilters) then
          begin
            try
              TTerrainImage(CurrentTerrain).LoadImage(URL);
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
          TTerrainImage(CurrentTerrain).ClearImage else
          MessageOk(Window, 'Not an image terrain');
      end;
    170:
      if Window.ColorDialog(BackgroundColor) then
        SceneManager.BackgroundColor := BackgroundColor;
    200..299:
      begin
        NoiseInterpolation := TNoiseInterpolation(Item.IntData - 200);
        if CurrentTerrain is TTerrainNoise then
          (CurrentTerrain as TTerrainNoise).Interpolation := NoiseInterpolation;
      end;
    1000, 1001:
      begin
        URL := '';
        if Window.FileDialog('Save terrain to X3D', URL, false,
          X3DVersion.FileFilters(xeClassic)) then
          ExportToX3D(URL, Item.IntData = 1001);
      end;
  end;
  Window.Invalidate;
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
    M.Append(TMenuItem.Create('Export to _X3D (basic) ...', 1000));
    M.Append(TMenuItem.Create('Export to _X3D (with our shaders and textures) ...', 1001));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Walk', 120, 'c', SceneManager.Camera = WalkCamera, true));
    M.Append(TMenuItemChecked.Create('Keep Above the Ground (in Walk)', 125, KeepCameraAboveGround, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 100, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('_Wireframe', 10, 'w', Wireframe, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Shader', 140, 's', Shader, true));
    M.Append(TMenuItemChecked.Create('Fog (when Shader)', 142, 'f', Fog, true));
    M.Append(TMenuItemChecked.Create('Lighting (when no Shader)', 130, 'l', Lighting, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Specialized grid (SRTM) rendering (also ignores Subdivision and Layers count)', 147, 'g', SpecializedGridRendering, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Background and Fog Color ...', 170));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Sliders Visible', 145, K_F1, ControlsVisible, true));
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

begin
  Window := TCastleWindow.Create(Application);

  Window.ParseParameters(StandardParseOptions);
  Parameters.CheckHighAtMost(1);

  try
    ExamineCamera := TExamineCamera.Create(Window);
    ExamineCamera.Init(Box3D(
      Vector3Single(-1, -1, -1),
      Vector3Single( 1,  1,  1)), { Radius } 0.2);

    WalkCamera := TWalkCamera.Create(Window);
    WalkCamera.Init(Vector3Single(0, 0, 0) { position },
      Vector3Single(0, 1, 0) { direction },
      Vector3Single(0, 0, 1) { up },
      Vector3Single(0, 0, 1),
      { PreferredHeight: unused, we don't use Gravity here } 0,
      { Radius } 0.02);
    WalkCamera.MoveSpeed := 0.5;

    SceneManager := Window.SceneManager;
    SceneManager.Items.Add(T3DTerrain.Create(Window));
    SceneManager.Camera := ExamineCamera;

    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    { Do not enable
      - SwapFullScreen_Key: (which may do Close+Open) is for now broken here
        (we should readd appropriate Controls* and camera to Window.Controls,
        sliders should be recreated but with default values coming from
        last values, terrain should be updated with sliders values;
        This isn't difficult, but would complicate source code for little gain.)
      - Close_CharKey: it would make it too easy to close. }
    Window.FpsShowOnCaption := true;
    Window.Caption := ApplicationName;
    Window.OpenAndRun;
  finally FreeAndNil(CurrentTerrain) end;
end.
