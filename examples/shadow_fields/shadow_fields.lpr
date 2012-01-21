{
  Copyright 2008-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow fields demo. See README.

  Run with 0 to 3 parameters: filenames of shadow caster, shadow receiver,
  light source. All three things must be 3D models understood by my engine
  (X3D, VRML, Collada, etc.), for shadow caster and light source there
  must also exist a xxx.shadow_field file in the same directory
  (use precompute_shadow_field to make this, with --light for light source).

  You can omit any of the filenames, even run with no parameters, then
  the default models from models/ subdir will be picked up.

  Navigate with mouse or keyboard (like view3dscene in Examine mode,
  see http://castle-engine.sourceforge.net/view3dscene.php docs).
  (For shadow receiver you can define Navigation node in VRML and navigate
  there in Walk mode.)

  Change by menu which object you're actually navigating --- by default
  you just navigate the whole scene.

  When you navigate the shadow caster
  or light source, you can move the object
  (drag with middle or right mouse button), or scale (-/+ keys).
  Rotations don't work now, they would require rotating
  before looking into shadow field and (more difficult) rotating of SH
  -- this is just not implemented now. }

program shadow_fields;

uses SysUtils, GL, CastleGLUtils, VectorMath, Boxes3D, CastleColors,
  CastleWindow, CastleScene, Cameras, CastleWarnings, CastleParameters,
  ShadowFields, CastleUtils, CubeMap, X3DNodes, CastleSceneManager, Base3D,
  SphericalHarmonics, GLCubeMap, CastleMessages, Shape, CastleStringUtils;

var
  Window: TCastleWindowCustom;

  SceneCaster, SceneReceiver, SceneLocalLight: TCastleScene;
  SceneManager: TCastleSceneManager;
  CasterOOF: TShadowField;
  LocalLightSRF: TShadowField;
  GLList_EnvLight: TGLuint;
  { Recalculated at the beginning of each Draw. }
  EnvLightSHVector: TSHVectorSingle;
  RenderParams: TBasicRenderParams;

  { Shadow fields always have MaxSHBasis recorded.
    But you can use less here, to speed up. }
  SHCount: Cardinal = MaxSHBasis;

  LightIntensity: Single = 50;

  UseShadowFields: boolean = true;
  UseSH: boolean = true;
  UseInterpolation: TSFInterpolation = siTrilinear;
  UseEnvLight: boolean = false;

type
  TNavigatorType = (ntAll, ntCaster, ntLocalLight, ntSFExplorer, ntEnvLight);

var
  Navigator: TNavigatorType = ntAll;
  NavigatorRadio: array [TNavigatorType] of TMenuItemRadio;
  { TCamera instance corresponding to current Navigator value }
  NavigatorCurrent: TCamera;
  NavigatorAll: TCamera;
  NavigatorCaster: TExamineCamera;
  NavigatorLocalLight: TExamineCamera;
  NavigatorSFExplorer: TExamineCamera;
  NavigatorEnvLight: TExamineCamera;

procedure NavigatorChanged;
begin
  case Navigator of
    ntAll       : NavigatorCurrent := NavigatorAll;
    ntCaster    : NavigatorCurrent := NavigatorCaster;
    ntLocalLight: NavigatorCurrent := NavigatorLocalLight;
    ntSFExplorer: NavigatorCurrent := NavigatorSFExplorer;
    ntEnvLight  : NavigatorCurrent := NavigatorEnvLight;
    else raise EInternalError.Create('Navigator?');
  end;

  { NavigatorCurrent may be any of NavigatorXxx.
    However, NavigatorAll must be treated specially, as it's already present
    inside SceneManager.Camera (we don't want to make it also referenced
    from Window.Controls). }
  NavigatorAll.IgnoreAllInputs := NavigatorAll <> NavigatorCurrent;
  if NavigatorAll <> NavigatorCurrent then
    Window.Controls.MakeSingle(TCamera, NavigatorCurrent) else
    Window.Controls.MakeSingle(TCamera, nil);

  if NavigatorRadio[Navigator] <> nil then
    NavigatorRadio[Navigator].Checked := true;
end;

procedure DrawEnvLight(ForMap: boolean);
begin
  glPushMatrix;
    glTranslatev(NavigatorEnvLight.MoveAmount);
    glScalef(NavigatorEnvLight.ScaleFactor,
             NavigatorEnvLight.ScaleFactor,
             NavigatorEnvLight.ScaleFactor);

    if not ForMap then
    begin
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      glColor4f(1, 1, 0, 0.1);
    end else
      glColor3f(1, 1, 1);

    glCallList(GLList_EnvLight);

    if not ForMap then
      glDisable(GL_BLEND);
  glPopMatrix;
end;

type
  TMySceneManager = class(TCastleSceneManager)
    procedure RenderFromViewEverything; override;
    function Headlight(out CustomHeadlight: TAbstractLightNode): boolean; override;
  end;

procedure TMySceneManager.RenderFromViewEverything;

  procedure DrawSFExplorerMaps;
  const
    Scale = 3;

    procedure DrawOneMap(Field: TShadowField;
      const FieldMoveAmount: TVector3Single;
      const FieldScale: Single;
      const ShiftX, ShiftY: Integer);
    var
      Map: PCubeMapByte;
      Side: TCubeMapSide;
    begin
      Map := Field.EnvMapFromPoint(VectorSubtract(
        NavigatorSFExplorer.MoveAmount, FieldMoveAmount), FieldScale);
      if Map <> nil then
      begin
        for Side := Low(Side) to High(Side) do
        begin
          SetWindowPos(CubeMapInfo[Side].ScreenX * CubeMapSize * Scale + ShiftX,
                       CubeMapInfo[Side].ScreenY * CubeMapSize * Scale + ShiftY);
          { Since ordering of bytes in our env maps is matching OpenGL
            pixel ordering, I can just draw these like TGrayscaleImage
            by glDrawPixels. }
          glDrawPixels(CubeMapSize, CubeMapSize, GL_LUMINANCE,
            GL_UNSIGNED_BYTE, @(Map^[Side]));
        end;
      end;
    end;

  begin
    glPixelZoom(Scale, Scale);

    DrawOneMap(CasterOOF,
      NavigatorCaster.MoveAmount,
      NavigatorCaster.ScaleFactor, 100, 100);
    DrawOneMap(LocalLightSRF,
      NavigatorLocalLight.MoveAmount,
      NavigatorLocalLight.ScaleFactor, 100, 300);

    glPixelZoom(1, 1);
  end;

var
  H: TLightInstance;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(NavigatorAll.Matrix);

  if UseEnvLight and (not SceneReceiver.BoundingBox.IsEmpty) then
  begin
    { SHVectorGLCapture wil draw maps, get them,
      and calculate EnvLightSHVector describing the light contribution
      (this will be used then by SceneReceiver.Render, during VertexColor). }

    SHVectorGLCapture(EnvLightSHVector, SceneReceiver.BoundingBox.Middle,
      @DrawEnvLight, 50, 50, 1 { no ScaleColor ---
        we will apply light intensity at VertexColor });
    glViewport(0, 0, Window.Width, Window.Height);
  end;

  Check(HeadlightInstance(H), 'Headlight must be true, we defined it such');

  { BaseLights will contain our headlight, based on camera (NavigatorAll)
    properties }
  RenderParams.FBaseLights.Assign(BaseLights);

  H.Node.FdOn.Send(false);
  SceneReceiver.Render(nil, RenderParams);

  { turn on headlight for following rendering }
  H.Node.FdOn.Send(true);

  glPushMatrix;
    glTranslatev(NavigatorCaster.MoveAmount);
    glScalef(NavigatorCaster.ScaleFactor,
             NavigatorCaster.ScaleFactor,
             NavigatorCaster.ScaleFactor);
    SceneCaster.Render(nil, RenderParams);
  glPopMatrix;

  glPushMatrix;
    glTranslatev(NavigatorLocalLight.MoveAmount);
    glScalef(NavigatorLocalLight.ScaleFactor,
             NavigatorLocalLight.ScaleFactor,
             NavigatorLocalLight.ScaleFactor);
    SceneLocalLight.Render(nil, RenderParams);
  glPopMatrix;

  { GL_LIGHTING is disabled by TCastleScene renderer now }

  glPushMatrix;
    DrawEnvLight(false);
  glPopMatrix;

  glEnable(GL_DEPTH_TEST);
    glColorv(Blue3Single);
    glPointSize(10); { VRML renderer will reset point size }
    glBegin(GL_POINTS);
      glVertexv(NavigatorSFExplorer.MoveAmount);
    glEnd;
  glDisable(GL_DEPTH_TEST);

  DrawSFExplorerMaps;
end;

function TMySceneManager.Headlight(out CustomHeadlight: TAbstractLightNode): boolean;
begin
  Result := true;
  CustomHeadlight := nil;
end;

procedure Open(Window: TCastleWindowBase);
begin
  GLList_EnvLight := glGenListsCheck(1, 'GLList_EnvLight');
  glNewList(GLList_EnvLight, GL_COMPILE);
    CastleGluSphere(1, 10, 10);
  glEndList;
end;

procedure Close(Window: TCastleWindowBase);
begin
  SceneCaster.GLContextClose;
  SceneReceiver.GLContextClose;
  SceneLocalLight.GLContextClose;
end;

type
  THelper = class
    procedure VertexColor(var Color: TVector3Single;
      Shape: TShape; const VertexPosition: TVector3Single;
      VertexIndex: Integer);
  end;

procedure THelper.VertexColor(var Color: TVector3Single;
  Shape: TShape; const VertexPosition: TVector3Single;
  VertexIndex: Integer);
const
  { TODO: this is needed to make results with CalculateByEnvMaps
    and CalculateBySH. Why? I thought I normalized all integrals
    correctly. }
  SHLightIntensity = 4*Pi;
var
  Position: TVector3Single;
  CasterBeforeLocalLight: boolean;

  procedure CalculateByEnvMaps;
  { Implementation using directly environment maps has some shortcomings
    that are not planned to be corrected (simply because they would be
    too slow, and/or I know how to do them much faster by spherical harmonics,
    so there's no point in implementing them for env maps version ---
    env maps version was/is only for testing anyway):
    - there's no interpolation (would be very slow)
    - we don't handle here environmental light
      (to fix, you have to change SHVectorGLCapture to return also the raw map)
    - we don't take into account that different pixels on the env map have
      different solid angle
  }
  var
    CasterMap, LocalLightMap: PCubeMapByte;
    B: LongWord;
    Side: TCubeMapSide;
    Pixel: Cardinal;
    C: Single;
  begin
    CasterMap := CasterOOF.EnvMapFromPoint(VectorSubtract(
      Position, NavigatorCaster.MoveAmount),
      NavigatorCaster.ScaleFactor);

    if not (CasterBeforeLocalLight or UseEnvLight) then
      CasterMap := nil;

    LocalLightMap := LocalLightSRF.EnvMapFromPoint(VectorSubtract(
      Position, NavigatorLocalLight.MoveAmount),
      NavigatorLocalLight.ScaleFactor);

    if LocalLightMap = nil then
      { Too far from light }
      Color := Vector3Single(0, 0, 0) else
    begin
      { TODO: we completely ignore here BRDF and cos() for light equations! }

      B := 0;
      if CasterMap = nil then
      begin
        { Too far from shadow caster, so just integrate over the light }

        for Side := Low(Side) to High(Side) do
          for Pixel := 0 to Sqr(CubeMapSize) - 1 do
            B += LocalLightMap^[Side, Pixel];
      end else
      begin
        { This loop is the time bottleneck of the whole algorithm.
          This must work extra-fast.
          To do this, we calculate everything in int values.

          To multiply two bytes (that represent 0..1 floats) we can just
          multiply them normally, then divide by 256. Actually, this division
          can be done only once, after the loop.

          It's important to make sure that B can hold all these values:
          it's 6 * 16^2 of maximum High(Word) = 2^16 values.
          Since B is LongWord (max 2^32), we're Ok.
          }

        for Side := Low(Side) to High(Side) do
          for Pixel := 0 to Sqr(CubeMapSize) - 1 do
            B += Word(LocalLightMap^[Side, Pixel]) *
                          CasterMap^[Side, Pixel];

        B := B div 256;
      end;

      { normalize B, apply LightIntensity }
      C := (B / 255) * LightIntensity / (6 * Sqr(CubeMapSize));
      Color[0] *= C;
      Color[1] *= C;
      Color[2] *= C;
    end;
  end;

  procedure CalculateBySH;
  var
    CasterVector, LightVector: PSHVectorSingle;
    B: Single;
    C: Single;
    LM: Cardinal;
  begin
    CasterVector := CasterOOF.SHVectorFromPoint(VectorSubtract(
      Position, NavigatorCaster.MoveAmount),
      NavigatorCaster.ScaleFactor, UseInterpolation, SHCount);

    if not (CasterBeforeLocalLight or UseEnvLight) then
      CasterVector := nil;

    { TODO: currently, I can do only double product on SH.
      So I either use env light, or local light --- never both. }

    if UseEnvLight then
      LightVector := @EnvLightSHVector else
      LightVector := LocalLightSRF.SHVectorFromPoint(VectorSubtract(
        Position, NavigatorLocalLight.MoveAmount),
        NavigatorLocalLight.ScaleFactor, UseInterpolation, SHCount);

    if LightVector = nil then
      { Too far from light }
      Color := Vector3Single(0, 0, 0) else
    begin
      B := 0;

      if CasterVector = nil then
      begin
        { Too far from shadow caster, so just assume like caster oof
          is pure white. So make a double product with SH vector representing
          pure white environment map. See README notes at the end --- such
          vector is just zero, except for first factor which is equal to
          SHBasis0. }

        B += LightVector^[0] * SHBasis0;
      end else
      begin
        for LM := 0 to SHCount - 1 do
          B += LightVector^[LM] * CasterVector^[LM];
      end;

      { apply LightIntensity }
      C := B * LightIntensity * SHLightIntensity;
      Color[0] *= C;
      Color[1] *= C;
      Color[2] *= C;
    end;
  end;

begin
  Position := MatrixMultPoint(Shape.State.Transform, VertexPosition);

  CasterBeforeLocalLight :=
    PointsDistanceSqr(Position, NavigatorCaster.MoveAmount) <
    PointsDistanceSqr(Position, NavigatorLocalLight.MoveAmount);

  if UseSH then
    CalculateBySH else
    CalculateByEnvMaps;
end;

procedure UseShadowFieldsChanged;
begin
  if UseShadowFields then
    SceneReceiver.Attributes.OnVertexColor := @THelper(nil).VertexColor else
    SceneReceiver.Attributes.OnVertexColor := nil;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
  RadioGroup: TMenuItemRadioGroup;

  procedure AddNavigatorMenu(const Caption: string; const Nav: TNavigatorType);
  begin
    NavigatorRadio[Nav] := TMenuItemRadio.Create(Caption,
      10 + Ord(Nav), Navigator = Nav, true);
    if RadioGroup = nil then
      RadioGroup := NavigatorRadio[Nav].Group else
      NavigatorRadio[Nav].Group := RadioGroup;
    M.Append(NavigatorRadio[Nav]);
  end;

  procedure AddInterpolationMenu(const Caption: string; const I: TSFInterpolation);
  var
    Radio: TMenuItemRadio;
  begin
    Radio := TMenuItemRadio.Create(Caption,
      30 + Ord(I), UseInterpolation = I, true);
    if RadioGroup = nil then
      RadioGroup := Radio.Group else
      Radio.Group := RadioGroup;
    M.Append(Radio);
  end;

begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItemChecked.Create('_Shadow fields', 100, UseShadowFields, true));
    M.Append(TMenuItemChecked.Create('Use spherical _harmonics', 120, UseSH, true));
    M.Append(TMenuItemChecked.Create('Use environmetal _light', 130, UseEnvLight, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set number of spherical harmonics factors to use...', 140));
    M.Append(TMenuItem.Create('Set light intensity...', 150));
    M.Append(TMenuSeparator.Create);
    RadioGroup := nil;
    AddInterpolationMenu('_Interpolation none', siNone);
    AddInterpolationMenu('Interpolation linear (radius)', siLinearRadius);
    AddInterpolationMenu('Interpolation bilinear (4 map points)', siBilinear);
    AddInterpolationMenu('Interpolation trlinear', siTrilinear);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Apply OpenGL _Lighting', 110,
      SceneReceiver.Attributes.Lighting, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);

  M := TMenu.Create('_Navigate');
    RadioGroup := nil;
    AddNavigatorMenu('_All', ntAll);
    AddNavigatorMenu('Shadow _caster', ntCaster);
    AddNavigatorMenu('Local _light source', ntLocalLight);
    AddNavigatorMenu('_Shadow fields explorer', ntSFExplorer);
    AddNavigatorMenu('_Environmental light source', ntEnvLight);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Next', 20, K_Space));
    Result.Append(M);
end;

procedure MenuCommand(Window: TCastleWindowBase; Item: TMenuItem);
var
  NewSHCount: Cardinal;
begin
  case Item.IntData of
    10..19:
      begin
        Navigator := TNavigatorType(Item.IntData - 10);
        NavigatorChanged;
      end;

    20:
      begin
        if Navigator = High(Navigator) then
          Navigator := Low(Navigator) else
          Navigator := Succ(Navigator);
        NavigatorChanged;
      end;

    30..39: UseInterpolation := TSFInterpolation(Item.IntData - 30);

    100:
      begin
        UseShadowFields := not UseShadowFields;
        UseShadowFieldsChanged;
      end;

    110: with SceneReceiver.Attributes do Lighting := not Lighting;
    120: UseSH := not UseSH;
    130: UseEnvLight := not UseEnvLight;

    140:
      begin
        NewSHCount := SHCount;
        if MessageInputQueryCardinal(Window,
          Format('Number of spherical harmonic factors to use: (must be between 1 and %d)',
            [MaxSHBasis]), NewSHCount, taLeft) then
        begin
          if not Between(NewSHCount, 1, MaxSHBasis) then
            MessageOk(Window, Format('Must be between 1 and %d.', [MaxSHBasis]), taLeft) else
            SHCount := NewSHCount;
        end;
      end;

    150: MessageInputQuery(Window, 'Light intensity:', LightIntensity, taLeft);

    200: Window.Close;
    else Exit;
  end;
  Window.PostRedisplay;
end;

var
  BoxSum: TBox3D;
  ShadowCasterFileName: string = 'models/humanoid_stand.wrl';
  ShadowReceiverFileName: string = 'models/plane.wrl';
  LocalLightFileName: string = 'models/sphere.wrl';
  V: TVector3Single;
begin
  Window := TCastleWindowCustom.Create(Application);

  Parameters.CheckHighAtMost(3);

  if Parameters.High >= 1 then
    ShadowCasterFileName := Parameters[1];
  if Parameters.High >= 2 then
    ShadowReceiverFileName := Parameters[2];
  if Parameters.High >= 3 then
    LocalLightFileName := Parameters[3];

  try
    OnWarning := @OnWarningWrite;

    RenderParams := TBasicRenderParams.Create;

    SceneCaster := TCastleScene.Create(nil);
    SceneCaster.Load(ShadowCasterFileName);

    SceneReceiver := TCastleScene.Create(nil);
    SceneReceiver.Load(ShadowReceiverFileName);

    SceneLocalLight := TCastleScene.Create(nil);
    SceneLocalLight.Load(LocalLightFileName);

    CasterOOF := TShadowField.Create;
    CasterOOF.LoadFromFile(ChangeFileExt(ShadowCasterFileName, ShadowFieldExt));

    LocalLightSRF := TShadowField.Create;
    LocalLightSRF.LoadFromFile(ChangeFileExt(LocalLightFileName, ShadowFieldExt));

    SceneManager := TMySceneManager.Create(nil);
    Window.Controls.Add(SceneManager);

    { initialize navigators }

    NavigatorAll := SceneReceiver.CreateCamera(Window);
    SceneManager.Camera := NavigatorAll;

    NavigatorCaster := TExamineCamera.Create(Window);
    NavigatorCaster.ModelBox := SceneCaster.BoundingBox;

    NavigatorLocalLight := TExamineCamera.Create(Window);
    NavigatorLocalLight.ModelBox := SceneLocalLight.BoundingBox;

    BoxSum := SceneCaster.BoundingBox + SceneReceiver.BoundingBox;
    SceneManager.DefaultVisibilityLimit := 100;

    { calculate starting local light position,
      and set this as NavigatorLocalLight.MoveAmount }
    if BoxSum.IsEmpty then
    begin
      V := Vector3Single(0, 0, 1);
    end else
    begin
      V := BoxSum.Middle;
      V[0] := BoxSum.Data[0][0];
    end;
    NavigatorLocalLight.MoveAmount := V;

    NavigatorSFExplorer := TExamineCamera.Create(Window);
    { use SceneCaster.BoundingBox for light's box, this determines the speed
      of moving light source with mouse. }
    NavigatorSFExplorer.ModelBox := SceneCaster.BoundingBox;

    { calculate starting sf explorer position,
      and set this as NavigatorSFExplorer.MoveAmount }
    if BoxSum.IsEmpty then
    begin
      V := Vector3Single(0, 0, 1);
    end else
    begin
      V := BoxSum.Middle;
      V[0] := BoxSum.Data[1][0];
    end;
    NavigatorSFExplorer.MoveAmount := V;

    NavigatorEnvLight := TExamineCamera.Create(Window);
    { use SceneCaster.BoundingBox for light's box, this determines the speed
      of moving light source with mouse. }
    NavigatorEnvLight.ModelBox := SceneCaster.BoundingBox;

    { calculate starting env light position,
      and set this as NavigatorEnvLight.MoveAmount }
    if BoxSum.IsEmpty then
    begin
      V := Vector3Single(0, 0, 1);
    end else
    begin
      V := BoxSum.Middle;
      V[1] := BoxSum.Data[0][1];
    end;
    NavigatorEnvLight.MoveAmount := V;

    NavigatorChanged;

    Window.Caption := 'shadow_fields';
    Window.MainMenu := CreateMainMenu;
    Window.OnMenuCommand := @MenuCommand;
    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.SetDemoOptions(K_F11, CharEscape, true);

    { initialize UseShadowFieldsChanged }
    UseShadowFieldsChanged;

    InitializeSHBasisMap;

    Window.OpenAndRun;
  finally
    FreeAndNil(SceneCaster);
    FreeAndNil(SceneReceiver);
    FreeAndNil(SceneLocalLight);
    FreeAndNil(CasterOOF);
    FreeAndNil(LocalLightSRF);
    FreeAndNil(RenderParams);
    FreeAndNil(SceneManager);
  end;
end.
