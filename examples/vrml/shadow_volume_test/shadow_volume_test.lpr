{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A program to demonstrate and test shadows by shadow volumes algorithm.

  Code of this program is a little more complicated than the ccde of
  a simple straightforward "shadows demo" would be, because one of the goals of
  this program is to experiment and test various
  variants of shadow volume making. A real program using shadows
  should just use siStencilOpSeparate approach, eventually
  (as a fallback, if siStencilOpSeparate is not available --- it requires
  OpenGL >= 2.0) siGLCullFace2Passes. All other approaches are practically
  useles, and implemented here only for testing purposes.

  This program displays two models:

  1. static scene

  2. shadow caster that casts shadows (on itself and on the scene).
    Ideally, shadow caster should be composed from a number of closed
    manifold parts to make rendering fast (see documentation of
    TVRMLGLScene.RenderShadowQuads for description of silhouette optimization
    that requires edges to be manifold). When program starts, it tells you
    how many edges in shadow caster were detected to be manifold --- ideally,
    all edges are manifold.

    If you want, you can leave the static scene empty (well, it's
    usually comfortable to set there only initial camera node),
    and put all geometry inside shadow caster. This way everything
    will cast shadows on everything --- the most realistic way.
    shadow_volume_test_fountain.sh demonstrates this on "The Castle"
    level.

  3. we also need separate VRML file with exactly one light definition.
    This will be the light used to cast shadows (it may be directional
    or positional, it's all handled).
    The scene (in point 1.) can contain additional lights,
    but they will not make additional shadows.

  Run the program with three command-line parameters for these models.
  Or just run one of the shadow_volume_test_xxx.sh scripts
  that use already prepared test VRML models from subdirectory models/.

  You can navigate in the scene like usual (arrows keys,
  PageUp/PageDown, Insert/Delete, +/- to change move speed etc. ---
  for full documentation see view3dscene key docs for Walk mode).
  You can also manipulate shadow caster (to see that the shadows
  are really dynamic :) ) using the keys:
    asd qwe, Ctrl + asd qwe, 2, 34
      = keys to move, rotate and scale shadow caster
}

program shadow_volume_test;

{$apptype CONSOLE}

uses GLWindow, GL, GLU, GLExt, KambiGLUtils, VRMLGLScene,
  VRMLNodes, Cameras, Boxes3D, SysUtils,
  KambiUtils, VectorMath, VRMLGLLightSet, VRMLFields,
  KambiClassUtils, KambiFilesUtils, KambiStringUtils, VRMLCameraUtils,
  ShadowTests, GLWinMessages, VRMLErrors, GLShadowVolumeRenderer,
  BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts, KambiSceneManager,
  RaysWindow, UIControls, RenderingCameraUnit;

type
  { Shadow implementations. Roughly ordered from the worse to the best. }
  TShadowsImplementation =
  (
    { No shadows. Now, *that's* really fast :) }
    siNone,

    { Use 1 pass rendering all shadow quads,
      use 1 bit of stencil buffer, invert on hit.

      This requires only 1 stencil bit. But it's absolutely incorrect
      method, and will work good only for the simplest shadow casters
      (e.g. ./shadow_volume_test_extrasimple.sh --- one triangle). }
    siInvertTrick,

    { Render front shadow quads incrementing stencil, then render
      back shadow quads decrementing stencil. Use our own code
      to detect which faces are front and which are back.

      Note that this implementation always renders all edges (not only silhouette
      edges), so it's very non-optimal for manifold scenes.
      Besides, siGLCullFace2Passes (even when it renders all edges)
      was found to be as fast, so this implementation is just useless
      pratically --- use only for testing purposes. }
    siEngineCullFace2Passes,

    { Like siEngineCullFace2Passes, but
      uses OpenGL to cull front/back faces.
      Although OpenGL culling is for sure faster than culling done by our
      code, on the other hand I have to pass the same set of faces two times.
      While when I cull by my own code, in the second pass I already
      detected which faces are back. So practically when culling with my own
      code, I have 2 times less culling tests to do.

      Note that glStencilOpSeparate (avail in OpenGL >= 2.0)
      allows to combine these two advantages: only one pass will
      be needed, and OpenGL can detect back/front faces.
      There are extensions for older OpenGL that also allow this. }
    siGLCullFace2Passes,

    { This means that StencilTwoSided (glStencilOpSeparate or some OpenGL
      extension) will be used.
      Allows only one pass, and OpenGL detects front/back faces.
      So this is the most desirable and practical approach. }
    siStencilTwoSided
  );

var
  Glw: TGLUIWindow;

  Scene, ShadowCaster: TVRMLGLScene;
  ShadowCasterNav: TExamineCamera;
  SceneNav: TWalkCamera;
  LightSet: TVRMLGLLightSet;

  { MainLightPosition[3] = 0 means it's directional. }
  MainLightPosition: TVector4Single;

  { This is sum of bounding boxes of scene and
    untransformed shadow caster. This is used as an approximate
    box where our the world is. }
  SceneBoundingBox: TBox3D;

  ShowShadowQuads: boolean = false;
  IsRenderEdgesForShadows: boolean = false;
  IsRenderStatus: boolean = true;
  IsRenderShadowCaster: boolean = true;
  AllowSilhouetteOptimization: boolean = true;

  ShadowsImplementation: TShadowsImplementation = siStencilTwoSided;
  ShadowsImplementationRadioGroup: TMenuItemRadioGroup;
  ShadowsImplementationRadio:
    array [TShadowsImplementation] of TMenuItemRadio;

  SV: TGLShadowVolumeRenderer;

  Font: TGLBitmapFont;

{ scene manager ------------------------------------------------------------ }

type
  TMySceneManager = class(TKamSceneManager)
    procedure RenderFromViewEverything; override;
    procedure ApplyProjection; override;
  end;

var
  SceneManager: TMySceneManager;

procedure TMySceneManager.RenderFromViewEverything;

  procedure RenderEverything(const LightsEnabled: Cardinal);
  begin
    Scene.Render(nil, LightsEnabled, tgAll);
    if IsRenderShadowCaster then
    begin
      glPushMatrix;
        glMultMatrix(ShadowCasterNav.Matrix);
        ShadowCaster.Render(nil, LightsEnabled, tgAll);
      glPopMatrix;
    end;
  end;

  procedure RenderFrontShadowQuads;
  begin
    ShadowTests.RenderFrontShadowQuads(ShadowCaster,
      MainLightPosition,
      { TODO: this should be something like RenderingCamera.CameraPosition
        to be fully correct, not just SceneNav.Position.
        Reason: for rendering generated textures, since we're part
        of scene manager now.
        For now, RenderingCamera doesn't have CameraPosition available,
        and I'm too lazy to add it only for this testing code. }
      SceneNav.Position,
      ShadowCasterNav.Matrix);
  end;

  procedure RenderBackShadowQuads;
  begin
    ShadowTests.RenderBackShadowQuads(ShadowCaster);
  end;

  procedure RenderAllShadowQuadsAndCaps;
  begin
    ShadowCaster.RenderShadowVolumeCore(SV, false, ShadowCasterNav.Matrix,
      AllowSilhouetteOptimization);
  end;

  procedure DoRenderEdgesForShadows;
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_POLYGON_OFFSET_LINE);
      glPolygonOffset(1, 1);
      glDepthFunc(GL_LEQUAL);

      { F12 performs somewhat obscure function: exchange the order
        of rendering of ManifoldEdges and BorderEdges.
        What for ? Well, if some edge has more than 2 neighbors,
        then it's present in both ManifoldEdges and BorderEdges.
        You may want to identify such edges sometimes (as the model
        could possibly be fixed to have edges with only 2 neighbors there).
        This is a way to do it: press and release F12 repeatedly,
        then edges that are both in ManifoldEdges and BorderEdges
        will flicker blue-yellow.

        Well, I warned that this is somewhat obscure feature, didn't I ? :) }

      if Glw.Pressed[K_F12] then
      begin
        glColor4f(0, 0, 1, 0.3);
        ShadowCaster.RenderBorderEdges(ShadowCasterNav.Matrix);

        glColor4f(1, 1, 0, 0.3);
        ShadowCaster.RenderSilhouetteEdges(MainLightPosition,
          ShadowCasterNav.Matrix);
      end else
      begin
        glColor4f(1, 1, 0, 0.3);
        ShadowCaster.RenderSilhouetteEdges(MainLightPosition,
          ShadowCasterNav.Matrix);

        glColor4f(0, 0, 1, 0.3);
        ShadowCaster.RenderBorderEdges(ShadowCasterNav.Matrix);
      end;
    glPopAttrib;
  end;

  { Rendering with hard shadows by SV algorithm. }
  procedure RenderWithShadows(const LightsEnabled: Cardinal);
  var
    StencilShadowBits: TGLuint;
    NewLightsEnabled: Cardinal;
  begin
    if (ShadowsImplementation = siStencilTwoSided) and
       (not SV.StencilTwoSided) then
    begin
      ShadowsImplementation := siGLCullFace2Passes;
      ShadowsImplementationRadioGroup.Selected :=
        ShadowsImplementationRadio[ShadowsImplementation];

      MessageOK(Glw, 'Stencil two-sided approach not available ' +
        '(requires OpenGL >= 2.0 for glStencilOpSeparate or some extension ' +
        'on older OpenGLs). Falling back to ' +
        'GLCullFace2Passes implementation', taMiddle);
    end;

    { Which stencil bits should be used by shadow volume algorithm ?

      Not only *while rendering* shadow quads but also *after this rendering*
      value in stencil buffer may be > 1 (so you really need more than 1 bit
      to hold it in stencil buffer).

      Why ? It's obvious that *while rendering* (e.g. right after rendering
      all front quads) this value may be > 1. But when the point
      is in the shadow because it's inside more than one shadow
      (cast by 2 different shadow quads) then even *after rendering*
      this point will have value > 1.

      So it's important that this constant spans a couple of bits.
      More precisely, it should be the maximum number of possibly overlapping
      front shadow quads from any possible camera view.

      If it's not enough, using INCR/DECR with "WRAP" will help --- but inevitably
      it will show some artifacts at some places.
      Run ./shadow_volume_test_parallels.sh to see such nasty case.

      For siInvertTrick it should always be 1 ---
      that's the point of siInvertTrick. }
    if ShadowsImplementation <> siInvertTrick then
      StencilShadowBits := $FF else
      StencilShadowBits := 1;

    glStencilMask(StencilShadowBits);

    RenderEverything(LightsEnabled);
    glEnable(GL_STENCIL_TEST);
      { Note that stencil buffer is set to all 0 now. }

      { Calculate shadows to the stencil buffer.
        Don't write anything to depth or color buffers. }
      glSetDepthAndColorWriteable(GL_FALSE);

        glStencilFunc(GL_ALWAYS, 0, 0);
        case ShadowsImplementation of
          siStencilTwoSided:
            begin
              SV.StencilSetupKind := ssFrontAndBack;
              SV.InitSceneOnlySetupStencil;
              RenderAllShadowQuadsAndCaps;
            end;
          siGLCullFace2Passes, siEngineCullFace2Passes:
            begin
              { Render front facing shadow quads. }
              SV.StencilSetupKind := ssFront;
              SV.InitSceneOnlySetupStencil;
              if ShadowsImplementation = siGLCullFace2Passes then
              begin
                glEnable(GL_CULL_FACE);
                glCullFace(GL_BACK);
                RenderAllShadowQuadsAndCaps;
              end else
                RenderFrontShadowQuads;

              { Render back facing shadow quads. }
              SV.StencilSetupKind := ssBack;
              SV.InitSceneOnlySetupStencil;
              if ShadowsImplementation = siGLCullFace2Passes then
              begin
                glCullFace(GL_FRONT);
                RenderAllShadowQuadsAndCaps;
                glDisable(GL_CULL_FACE);
              end else
                RenderBackShadowQuads;
            end;
          siInvertTrick:
            begin
              glStencilOp(GL_KEEP, GL_KEEP, GL_INVERT);
              RenderAllShadowQuadsAndCaps;
            end;
          else raise Exception.Create('Unknown ShadowsImplementation value');
        end;

      glSetDepthAndColorWriteable(GL_TRUE);

      { Now render everything once again, with lights turned on.
        But render only things not in shadow.

        What should I do with depth buffer now ? Currently it contains the
        current scene. So I should either clear it with
        glClear(GL_DEPTH_BUFFER_BIT), or make sure that
        DepthFunc accepts new pixel when depth values are equal.
        glClear(GL_DEPTH_BUFFER_BIT) approach has one real problem: after such
        rendering, depth buffer values will be filled only on the non-shadowed
        parts, which will hurt us if we will try to render something else
        on top of the scene --- like silhouette edges. }
      glPushAttrib(
          GL_DEPTH_BUFFER_BIT { for glDepthFunc } or
          GL_LIGHTING_BIT { for LightSet.RenderLights });
        glDepthFunc(GL_LEQUAL);
        NewLightsEnabled := LightsEnabled;
        LightSet.Render(NewLightsEnabled);
        { setup stencil : don't modify stencil, stencil test passes only for =0 }
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        glStencilFunc(GL_EQUAL, 0, StencilShadowBits);

        RenderEverything(NewLightsEnabled);
      glPopAttrib;
    glDisable(GL_STENCIL_TEST);
  end;

var
  LightsEnabled: Cardinal;
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glLoadMatrix(RenderingCamera.Matrix);

  LightsEnabled := 1;

  SV.InitFrustumAndLight(RenderingCamera.Frustum, MainLightPosition);

  SV.InitSceneDontSetupStencil(
    Box3DTransform(ShadowCaster.BoundingBox, ShadowCasterNav.Matrix));

  if (ShadowsImplementation = siNone) or (not SV.SceneShadowPossiblyVisible) then
  begin
    { For only ShadowsImplementation = siNone, we could just render
      LightSet, no need to push/pop GL_LIGHTING_BIT. But if user will
      later switch ShadowsImplementation to something else, we can't
      let lights affect it. }
    glPushAttrib(GL_LIGHTING_BIT);
      LightSet.Render(LightsEnabled);
      RenderEverything(LightsEnabled);
    glPopAttrib;
  end else
    RenderWithShadows(LightsEnabled);

  if ShowShadowQuads then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glColor4f(1, 1, 0, 0.3);
      glDepthMask(GL_FALSE);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      RenderAllShadowQuadsAndCaps;
    glPopAttrib;
  end;

  if IsRenderEdgesForShadows then
    DoRenderEdgesForShadows;
end;

procedure TMySceneManager.ApplyProjection;

  procedure UpdateCameraProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    SceneNav.ProjectionMatrix := ProjectionMatrix;
  end;

begin
  glViewport(0, 0, ContainerWidth, ContainerHeight);

  FProjectionNear := Box3DAvgSize(SceneBoundingBox) * 0.05;
  { TODO: switch to using NV_depth_clamp where possible.
    Enable this for good when shadow culling for infinite frustum will work...
    otherwise, right now, we lose a little time for this, even in z-pass. }
  { For z-fail, we use far projection plane in infinity. }
  FProjectionFar  := ZFarInfinity;

  FPerspectiveView := true;
  FPerspectiveViewAngles[1] := 30.0;
  FPerspectiveViewAngles[0] := AdjustViewAngleDegToAspectRatio(
    FPerspectiveViewAngles[1], ContainerWidth / ContainerHeight);

  ProjectionGLPerspective(FPerspectiveViewAngles[1], ContainerWidth / ContainerHeight,
    ProjectionNear, ProjectionFar);
  UpdateCameraProjectionMatrix;
end;

{ status text ---------------------------------------------------------------- }

type
  TStatusText = class(TUIControl)
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TStatusText.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TStatusText.Draw;
begin
  if IsRenderStatus then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_LIGHTING);
      glColorv(Yellow3Single);
      Font.PrintStrings(
        [ Format('FPS : %f (real : %f)', [Glw.Fps.FrameTime, Glw.Fps.RealTime]),
          Format('Shadow possibly visible (sv culling): %s', [BoolToStr[SV.SceneShadowPossiblyVisible]]),
          Format('Z-fail required: %s', [BoolToStr[SV.ZFail]]),
          Format('Z-fail and light cap required: %s', [BoolToStr[SV.ZFailAndLightCap]]),
          Format('INCR/DECR_WRAP available: %s', [BoolToStr[SV.WrapAvailable]]) ],
        0, 5, 5);
    glPopAttrib;
  end;
end;

{ glw callbacks ------------------------------------------------------------ }

procedure Open(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.1, 0.1, 0.1, 0.0);

  { Tests:
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.6, 0.6, 0.6, 1)); }

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

  SV := TGLShadowVolumeRenderer.Create;
  SV.GLContextOpen;

  Scene.PrepareResources([prRender, prBoundingBox], false);
  ShadowCaster.PrepareResources([prRender, prBoundingBox] + prShadowVolume, false);
end;

procedure Close(glwin: TGLWindow);
begin
  Scene.GLContextClose;
  ShadowCaster.GLContextClose;

  FreeAndNil(Font);

  FreeAndNil(SV);
end;

var
  FirstIdle: boolean = true;

procedure ShowManifoldStatistics;
var
  S: string;
begin
  S := Format('Model has' +nl+
    '%d manifold edges' +nl+
    '%d border edges' +nl+
    nl+
    'For shadow volumes silhouette optimization, it''s crucial that ' +
    'the model has mostly (if not only) manifold edges. So if rendering ' +
    'is slow, you may want to improve your model: make all edges '+
    'manifold (with exactly two neighbors) and order vertexes consistently ' +
    '(e.g. all CCW outside).' +nl+
    nl+
    'Also note that if model is not a perfect manifold (some border edges exist) ' +
    'then in some cases artifacts may occur. See "VRML engine documentation" ' +
    'on [http://vrmlengine.sourceforge.net/vrml_engine_doc.php] ' +
    '(chapter "Shadows") for more info. To workaround these artifacts, you ' +
    'may turn off silhouette optimization from the menu. But usually ' +
    'it''s much more efficient to correct your model to be manifold.',
    [ ShadowCaster.ManifoldEdges.Count,
      ShadowCaster.BorderEdges.Count ]);

  MessageOK(Glw, S, taMiddle);
end;

procedure Idle(glwin: TGLWindow);
begin
  if FirstIdle then
  begin
    { We do MessageOK in first OnIdle, instead of e.g. in OnOpen,
      because in OnIdle we're already corrected initialized
      (first OnResize and OnOpen for sure are done now, and OnDraw may
      work as usual) and so the background under MessageOK is good now. }
    ShowManifoldStatistics;
    FirstIdle := false;
  end;
end;

{ menu ----------------------------------------------------------------------- }

procedure MenuCommand(glwin: TGLWindow; MenuItem: TMenuItem);

  procedure SetViewpointForWholeScene;
  var
    Position, Direction, Up, GravityUp: TVector3Single;
  begin
    CameraViewpointForWholeScene(SceneBoundingBox, 0, 2, true, true,
      Position, Direction, Up, GravityUp);
    SceneNav.Init(Position, Direction, Up, GravityUp, 0, 0);
  end;

var
  NodeMatrix: TNodeMatrixTransform_1;
begin
  case MenuItem.IntData of
    Ord(Low(TShadowsImplementation)) ..
    Ord(High(TShadowsImplementation)):
      begin
        ShadowsImplementation := TShadowsImplementation(MenuItem.IntData);
        Glwin.PostRedisplay;
      end;
    8:begin
        AllowSilhouetteOptimization := not AllowSilhouetteOptimization;
        Glwin.PostRedisplay;
      end;
    10, 11:
      begin
        Writeln(MakeVRMLCameraStr(MenuItem.IntData - 9, false,
          SceneNav.Position,
          SceneNav.Direction,
          SceneNav.Up,
          SceneNav.GravityUp));
      end;
    20:
      begin
        NodeMatrix := TNodeMatrixTransform_1.Create('ShadowCasterNav_Matrix', '');
        try
          NodeMatrix.FdMatrix.Value := ShadowCasterNav.Matrix;
          SaveVRMLClassic(NodeMatrix, StdOutStream, '');
        finally NodeMatrix.Free end;
      end;
    30:
      begin
        ShowShadowQuads := not ShowShadowQuads;
        Glwin.PostRedisplay;
      end;
    40:
      begin
        IsRenderEdgesForShadows := not IsRenderEdgesForShadows;
        Glwin.PostRedisplay;
      end;
    50:
      begin
        IsRenderStatus := not IsRenderStatus;
        Glwin.PostRedisplay;
      end;
    60:
      begin
        IsRenderShadowCaster := not IsRenderShadowCaster;
        Glwin.PostRedisplay;
      end;
    120: Glwin.SaveScreenDialog(FileNameAutoInc(SUnformattable(Parameters[0]) + '_screen_%d.png'));
    130: SetViewpointForWholeScene;
  end;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;

  procedure AppendShadowsImplementationRadio(const Caption: string;
    SI: TShadowsImplementation);
  begin
    ShadowsImplementationRadio[SI] := TMenuItemRadio.Create(Caption, Ord(SI),
      ShadowsImplementation = SI, true);
    M.Append(ShadowsImplementationRadio[SI]);
    if ShadowsImplementationRadioGroup = nil then
      ShadowsImplementationRadioGroup := ShadowsImplementationRadio[SI].Group else
      ShadowsImplementationRadio[SI].Group := ShadowsImplementationRadioGroup;
  end;

begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Method');
    ShadowsImplementationRadioGroup := nil;
    AppendShadowsImplementationRadio('_No shadows', siNone);
    AppendShadowsImplementationRadio('_Invert trick (usable only with' +
      ' simplest shadow casters)', siInvertTrick);
    AppendShadowsImplementationRadio('2 passes, cull faces using our _engine ' +
      '(currently can''t use silhouette optim, so it''s slower than it could be, ' +
      'and only for positional lights, and no caps (so z-fail will not work))',
      siEngineCullFace2Passes);
    AppendShadowsImplementationRadio('2 passes, cull faces using _OpenGL',
      siGLCullFace2Passes);
    AppendShadowsImplementationRadio('_Stencil two-sided (best choice, ' +
      'requires OpenGL >= 2.0 or some extensions)', siStencilTwoSided);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Allow silhouette optimization', 8,
      AllowSilhouetteOptimization, true));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('Show shadow caster (turning off may cause' +
      ' artifacts: shadows on screen where the caster would be)', 60,
      IsRenderShadowCaster, true));
    M.Append(TMenuItemChecked.Create('Show shadows _quads', 30,
      ShowShadowQuads, true));
    M.Append(TMenuItemChecked.Create('Show _edges for shadows (only for manifold' +
      ' scenes; yellow edges are 2-manifold, blue are border edges)', 40,
      IsRenderEdgesForShadows, true));
    M.Append(TMenuItemChecked.Create('Show status text', 50,
      IsRenderStatus, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Save screen ...', 120, K_F5));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Jump to calculated viewpoint to see the whole scene', 130));
    Result.Append(M);
  M := TMenu.Create('_Console');
    M.Append(TMenuItem.Create('Print current _camera (for VRML 1.0)', 10));
    M.Append(TMenuItem.Create('Print current _camera (for VRML 2.0, X3D classic)', 11));
    M.Append(TMenuItem.Create('Print shadow caster _transformation', 20));
    Result.Append(M);
end;

{ main ----------------------------------------------------------------------- }

var
  LightSetVrmlName: string;
  SceneVrmlName: string;
  ShadowCasterVrmlName: string;
  L: PLightInstance;
begin
  Glw := TGLUIWindow.Create(Application);

  try
    { parse params }
    Glw.ParseParameters(StandardParseOptions);
    Parameters.CheckHigh(3);
    LightSetVrmlName := Parameters[1];
    SceneVrmlName := Parameters[2];
    ShadowCasterVrmlName := Parameters[3];

    { init vrml-related objects }
    VRMLWarning := @VRMLWarning_Write;
    LightSet := TVRMLGLLightSet.Create(LoadVRMLClassic(LightSetVrmlName, false), true);
    Scene := TVRMLGLScene.Create(nil);
    Scene.Load(SceneVrmlName);
    Scene.Attributes.PreserveOpenGLState := true;
    ShadowCaster := TVRMLGLScene.Create(nil);
    ShadowCaster.Load(ShadowCasterVrmlName);
    ShadowCaster.Attributes.PreserveOpenGLState := true;
    SceneBoundingBox := Box3DSum(Scene.BoundingBox, ShadowCaster.BoundingBox);

    { calculate MainLightPosition }
    L := LightSet.Lights.Pointers[0];
    if L^.LightNode is TVRMLPositionalLightNode then
      MainLightPosition := Vector4Single(L^.Location, 1) else
    if L^.LightNode is TVRMLDirectionalLightNode then
      MainLightPosition := Vector4Single(L^.Direction, 0) else
      raise Exception.CreateFmt('Light node "%s" cannot be used to cast shadows, it has no position and no direction',
        [L^.LightNode.NodeTypeName]);

    { init SceneManager, with a Scene inside }
    SceneManager := TMySceneManager.Create(Glw);
    Glw.Controls.Add(SceneManager);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    { init SceneNav }
    SceneNav := TWalkCamera.Create(Glw);
    Scene.CameraFromNavigationInfo(SceneNav, Scene.BoundingBox, 'FLY');
    Scene.CameraFromViewpoint(SceneNav, false, false);
    SceneNav.ExclusiveEvents := false; { default for now, but just to be safe... }
    SceneManager.Camera := SceneNav;

    { init ShadowCasterNav }
    ShadowCasterNav := TExamineCamera.Create(Glw);
    ShadowCasterNav.ModelBox := ShadowCaster.BoundingBox;
    ShadowCasterNav.ExclusiveEvents := false; { default for now, but just to be safe... }

    ShadowCasterNav.Inputs_Move[0, false].Key1 := K_A;
    ShadowCasterNav.Inputs_Move[0, true ].Key1 := K_D;
    ShadowCasterNav.Inputs_Move[1, false].Key1 := K_Q;
    ShadowCasterNav.Inputs_Move[1, true ].Key1 := K_E;
    ShadowCasterNav.Inputs_Move[2, false].Key1 := K_S;
    ShadowCasterNav.Inputs_Move[2, true ].Key1 := K_W;

    ShadowCasterNav.Inputs_Rotate[0, false].Key1 := K_A;
    ShadowCasterNav.Inputs_Rotate[0, true ].Key1 := K_D;
    ShadowCasterNav.Inputs_Rotate[1, false].Key1 := K_Q;
    ShadowCasterNav.Inputs_Rotate[1, true ].Key1 := K_E;
    ShadowCasterNav.Inputs_Rotate[2, false].Key1 := K_S;
    ShadowCasterNav.Inputs_Rotate[2, true ].Key1 := K_W;

    ShadowCasterNav.Input_Home.Key1 := K_None;
    ShadowCasterNav.Input_StopRotating.Key1 := K_2;
    ShadowCasterNav.Input_ScaleLarger.Key1 := K_3;
    ShadowCasterNav.Input_ScaleLarger.Character := #0;
    ShadowCasterNav.Input_ScaleSmaller.Key1 := K_4;
    ShadowCasterNav.Input_ScaleSmaller.Character := #0;

    Glw.Controls.Add(ShadowCasterNav); { use ShadowCasterNav as a 2nd camera }

    Glw.Controls.Add(TStatusText.Create(Glw));

    Glw.MainMenu := CreateMainMenu;
    Glw.OnMenuCommand := @MenuCommand;
    Glw.AutoRedisplay := true;
    Glw.StencilBufferBits := 8;
    Glw.OnOpen := @Open;
    Glw.OnClose := @Close;
    Glw.OnIdle := @Idle;
    Glw.OpenAndRun;
  finally
    Scene.Free;
    ShadowCaster.Free;
    LightSet.Free;
  end;
end.

{
  Local Variables:
  compile-command: "fpcdebug shadow_volume_test.lpr"
  End:
}
