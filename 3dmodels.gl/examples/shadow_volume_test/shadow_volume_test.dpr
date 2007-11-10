{
  Copyright 2003-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
    TVRMLFlatSceneGL.RenderShadowQuads for description of silhouette optimization
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

  You can navigate over the scene like usual (arrows keys,
  PageUp/PageDown, Insert/Delete, +/- to change move speed etc. ---
  for full documentation see view3dscene key docs for Walk mode).
  You can also manipulate shadow caster (to see that the shadows
  are really dynamic :) ) using the keys:
    asd qwe, Ctrl + asd qwe, 2, 34
      = keys to move, rotate and scale shadow caster
}

program shadow_volume_test;

{$apptype CONSOLE}

uses GLWindow, GLW_Navigated, OpenGLh, KambiGLUtils, VRMLFlatSceneGL,
  VRMLNodes, MatrixNavigation, VRMLFlatScene, Boxes3d, SysUtils,
  KambiUtils, VectorMath, VRMLLightSetGL, VRMLFields,
  KambiClassUtils, KambiFilesUtils, KambiStringUtils, VRMLCameraUtils,
  ShadowTests, GLWinMessages, VRMLErrors, ShadowVolumesHelper,
  BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts;

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
      be needed, and OpenGL can detect back/front faces. }
    siGLCullFace2Passes,

    { This means that glStencilOpSeparate will be used.
      Requires OpenGL >= 2.0, but allows only one pass, and OpenGL detects
      front/back faces. So this is the most desirable and practical approach. }
    siStencilOpSeparate
  );

var
  Scene, ShadowCaster: TVRMLFlatSceneGL;
  ShadowCasterNav: TMatrixExaminer;
  LightSet: TVRMLLightSetGL;

  { MainLightPosition[3] = 0 means it's directional. }
  MainLightPosition: TVector4Single;

  { This is sum of bounding boxes of scene and
    untransformed shadow caster. This is used as an approximate
    box where our the world is. }
  SceneBoundingBox: TBox3d;

  ShowShadowQuads: boolean = false;
  IsRenderEdgesForShadows: boolean = false;
  IsRenderStatus: boolean = true;
  IsRenderShadowCaster: boolean = true;
  AllowSilhouetteOptimization: boolean = true;

  ShadowsImplementation: TShadowsImplementation = siStencilOpSeparate;
  ShadowsImplementationRadioGroup: TMenuItemRadioGroup;
  ShadowsImplementationRadio:
    array [TShadowsImplementation] of TMenuItemRadio;

  SVHelper: TShadowVolumesHelper;

  Font: TGLBitmapFont;

{ glw callbacks ------------------------------------------------------------ }

procedure Draw(glwin: TGLWindow);

  procedure RenderEverything;
  begin
    Scene.Render(nil, tgAll);
    if IsRenderShadowCaster then
    begin
      glPushMatrix;
        glMultMatrix(ShadowCasterNav.Matrix);
        ShadowCaster.Render(nil, tgAll);
      glPopMatrix;
    end;
  end;

  procedure RenderFrontShadowQuads;
  begin
    ShadowTests.RenderFrontShadowQuads(ShadowCaster,
      MainLightPosition, Glw.NavWalker.CameraPos, ShadowCasterNav.Matrix);
  end;

  procedure RenderBackShadowQuads;
  begin
    ShadowTests.RenderBackShadowQuads(ShadowCaster);
  end;

  procedure RenderAllShadowQuadsAndCaps;
  begin
    ShadowCaster.RenderShadowVolume(SVHelper, false, ShadowCasterNav.Matrix,
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

      if Glwin.KeysDown[K_F12] then
      begin
        RenderBorderEdges(ShadowCaster, ShadowCasterNav.Matrix);
        RenderSilhouetteEdges(ShadowCaster, MainLightPosition,
          ShadowCasterNav.Matrix);
      end else
      begin
        RenderSilhouetteEdges(ShadowCaster, MainLightPosition,
          ShadowCasterNav.Matrix);
        RenderBorderEdges(ShadowCaster, ShadowCasterNav.Matrix);
      end;
    glPopAttrib;
  end;

  { Rendering with hard shadows by SV algorithm. }
  procedure RenderWithShadows;
  var
    StencilShadowBits: TGLuint;
  begin
    if (ShadowsImplementation = siStencilOpSeparate) and
       (glStencilOpSeparate = nil) then
    begin
      ShadowsImplementation := siGLCullFace2Passes;
      ShadowsImplementationRadioGroup.Selected :=
        ShadowsImplementationRadio[ShadowsImplementation];

      MessageOK(Glwin, 'glStencilOpSeparate not available ' +
        '(OpenGL version required is >= 2.0), falling back to ' +
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

    RenderEverything;
    glEnable(GL_STENCIL_TEST);
      { Note that stencil buffer is set to all 0 now. }

      { Calculate shadows to the stencil buffer.
        Don't write anything to depth or color buffers. }
      glSetDepthAndColorWriteable(GL_FALSE);

        glStencilFunc(GL_ALWAYS, 0, 0);
        case ShadowsImplementation of
          siStencilOpSeparate:
            begin
              SVHelper.StencilSetupKind := ssSeparate;
              SVHelper.InitSceneOnlySetupStencil;
              RenderAllShadowQuadsAndCaps;
            end;
          siGLCullFace2Passes, siEngineCullFace2Passes:
            begin
              { Render front facing shadow quads. }
              SVHelper.StencilSetupKind := ssForFront;
              SVHelper.InitSceneOnlySetupStencil;
              if ShadowsImplementation = siGLCullFace2Passes then
              begin
                glEnable(GL_CULL_FACE);
                glCullFace(GL_BACK);
                RenderAllShadowQuadsAndCaps;
              end else
                RenderFrontShadowQuads;

              { Render back facing shadow quads. }
              SVHelper.StencilSetupKind := ssForBack;
              SVHelper.InitSceneOnlySetupStencil;
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
        LightSet.RenderLights;
        { setup stencil : don't modify stencil, stencil test passes only for =0 }
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        glStencilFunc(GL_EQUAL, 0, StencilShadowBits);

        RenderEverything;
      glPopAttrib;
    glDisable(GL_STENCIL_TEST);
  end;

begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glLoadMatrix(Glw.Navigator.Matrix);

  SVHelper.InitFrustumAndLight(Glw.NavWalker.Frustum, MainLightPosition);

  SVHelper.InitSceneDontSetupStencil(
    BoundingBoxTransform(ShadowCaster.BoundingBox, ShadowCasterNav.Matrix));

  if (ShadowsImplementation = siNone) or (not SVHelper.SceneShadowPossiblyVisible) then
  begin
    { For only ShadowsImplementation = siNone, we could just render
      LightSet, no need to push/pop GL_LIGHTING_BIT. But if user will
      later switch ShadowsImplementation to something else, we can't
      let lights affect it. }
    glPushAttrib(GL_LIGHTING_BIT);
      LightSet.RenderLights;
      RenderEverything;
    glPopAttrib;
  end else
    RenderWithShadows;

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

  if IsRenderStatus then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_LIGHTING);
      glColorv(Yellow3Single);
      Font.Projection2DPrintStrings(5, 5, Format(
        'FPS : %f (real : %f)' +nl+
        'Shadow possibly visible (sv culling): %s' + nl+
        'Z-fail required: %s' + nl+
        'Z-fail and light cap required: %s' + nl+
        'INCR/DECR_WRAP available: %s',
        [ Glw.FpsFrameTime, Glw.FpsRealTime,
          BoolToStr[SVHelper.SceneShadowPossiblyVisible],
          BoolToStr[SVHelper.ZFail],
          BoolToStr[SVHelper.ZFailAndLightCap],
          BoolToStr[SVHelper.WrapAvailable]]));
    glPopAttrib;
  end;
end;

procedure ResizeGL(glwin: TGLWindow);

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    Glw.NavWalker.ProjectionMatrix := ProjectionMatrix;
  end;

begin
  glViewport(0, 0, glwin.Width, glwin.Height);
  ProjectionGLPerspective(30, glwin.Width/glwin.Height,
    Box3dAvgSize(SceneBoundingBox)*0.05,
    { TODO: switch to using NV_depth_clamp where possible.
      Enable this for good when shadow culling for infinite frustum will work...
      otherwise, right now, we lose a little time for this, even in z-pass. }
    { For z-fail, we use far projection plane in infinity. }
    ZFarInfinity
    { Box3dAvgSize(SceneBoundingBox)*20.0 });
  UpdateNavigatorProjectionMatrix;
end;

procedure InitGL(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.1, 0.1, 0.1, 0.0);

  { Tests:
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.6, 0.6, 0.6, 1)); }

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

  SVHelper := TShadowVolumesHelper.Create;
  SVHelper.InitGLContext;

  Scene.PrepareRender([tgAll], [prBoundingBox]);
  ShadowCaster.PrepareRender([tgAll], [prBoundingBox] + prShadowVolume);
end;

procedure CloseGL(glwin: TGLWindow);
begin
  Scene.CloseGL;
  ShadowCaster.CloseGL;
  LightSet.CloseGL;

  FreeAndNil(Font);

  FreeAndNil(SVHelper);
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

procedure IdleGL(glwin: TGLWindow);
begin
  if FirstIdle then
  begin
    { We do MessageOK in first OnIdle, instead of e.g. in OnInit,
      because in OnIdle we're already corrected initialized
      (first OnResize and OnInit for sure are done now, and OnDraw may
      work as usual) and so the background under MessageOK is good now. }
    ShowManifoldStatistics;
    FirstIdle := false;
  end;

  ShadowCasterNav.Idle(glwin.FpsCompSpeed, @Glwin.KeysDown, Glwin.MousePressed);
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  ShadowCasterNav.KeyDown(key, c, @Glwin.KeysDown);
end;

{ menu ----------------------------------------------------------------------- }

procedure MenuCommand(glwin: TGLWindow; MenuItem: TMenuItem);

  procedure SetViewpointForWholeScene;
  var
    CameraPos, CameraDir, CameraUp, GravityUp: TVector3Single;
  begin
    CameraViewpointForWholeScene(SceneBoundingBox,
      CameraPos, CameraDir, CameraUp, GravityUp);
    Glw.NavWalker.Init(CameraPos, CameraDir, CameraUp, GravityUp, 0.0, 0.0);
  end;

var
  NodeMatrix: TNodeMatrixTransform;
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
        Writeln(MakeVRMLCameraStr(MenuItem.IntData - 9,
          Glw.NavWalker.CameraPos,
          Glw.NavWalker.CameraDir,
          Glw.NavWalker.CameraUp,
          Glw.NavWalker.GravityUp));
      end;
    20:
      begin
        NodeMatrix := TNodeMatrixTransform.Create('ShadowCasterNav_Matrix', '');
        try
          NodeMatrix.FdMatrix.Matrix := ShadowCasterNav.Matrix;
          SavetoVRMLFile(NodeMatrix, StdOutStream, '');
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
    120: Glwin.SaveScreenDialog(FNameAutoInc(Parameters[0] + '_screen_%d.png'));
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
    AppendShadowsImplementationRadio('_StencilOpSeparate (best choice, ' +
      'requires OpenGL >= 2.0)', siStencilOpSeparate);
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
    M.Append(TMenuItem.Create('Print current _camera (for VRML 2.0)', 11));
    M.Append(TMenuItem.Create('Print shadow caster _transformation', 20));
    Result.Append(M);
end;

{ main ----------------------------------------------------------------------- }

var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  LightSetVrmlName: string;
  SceneVrmlName: string;
  ShadowCasterVrmlName: string;
  L: PActiveLight;
begin
  try
    { parse params }
    Glw.ParseParameters(StandardParseOptions);
    Parameters.CheckHigh(3);
    LightSetVrmlName := Parameters[1];
    SceneVrmlName := Parameters[2];
    ShadowCasterVrmlName := Parameters[3];

    { init vrml-related objects }
    VRMLNonFatalError := @VRMLNonFatalError_WarningWrite;
    LightSet := TVRMLLightSetGL.Create(
      ParseVRMLFile(LightSetVrmlName, false), true, 0, -1);
    Scene := TVRMLFlatSceneGL.Create(
      ParseVRMLFile(SceneVrmlName, false), true, roSceneAsAWhole);
    Scene.Attributes.UseLights := true;
    Scene.Attributes.FirstGLFreeLight := 1;
    ShadowCaster := TVRMLFlatSceneGL.Create(
      ParseVRMLFile(ShadowCasterVrmlName, false), true, roSceneAsAWhole);
    SceneBoundingBox := Box3dSum(Scene.BoundingBox,
      ShadowCaster.BoundingBox);

    { calculate MainLightPosition }
    L := LightSet.Lights.Pointers[0];
    if L^.LightNode is TNodeGeneralPositionalLight then
      MainLightPosition := Vector4Single(L^.TransfLocation, 1) else
    if L^.LightNode is TNodeGeneralDirectionalLight then
      MainLightPosition := Vector4Single(L^.TransfNormDirection, 0) else
      raise Exception.CreateFmt('TVRMLLightSetGL.TurnLightsOffForShadows: ' +
        'light node "%s" cannot be used to cast shadows, it has no position ' +
        'and no direction', [L^.LightNode.NodeTypeName]);

    { init ShadowCasterNav }
    ShadowCasterNav := TMatrixExaminer.Create(@Glw.PostRedisplayOnMatrixChanged);
    ShadowCasterNav.ModelBox := ShadowCaster.BoundingBox;

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
    ShadowCasterNav.Input_ScaleSmaller.Key1 := K_4;

    { init Glw.Navigator }
    Glw.Navigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
    Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, GravityUp);
    VectorAdjustToLengthTo1st(CamDir, Box3dAvgSize(SceneBoundingBox)*0.02);
    Glw.NavWalker.Init(CamPos, CamDir, CamUp, GravityUp, 0.0, 0.0);

    Glw.MainMenu := CreateMainMenu;
    Glw.OnMenuCommand := @MenuCommand;
    Glw.AutoRedisplay := true;
    Glw.StencilBufferBits := 8;
    Glw.OnInit := @InitGL;
    Glw.OnClose := @CloseGL;
    Glw.OnDraw := @Draw;
    Glw.OnResize := @ResizeGL;
    Glw.OnIdle := @IdleGL;
    Glw.OnKeyDown := @KeyDown;
    Glw.InitLoop;
  finally
    ShadowCasterNav.Free;
    Scene.Free;
    ShadowCaster.Free;
    LightSet.Free;
  end;
end.

{
  Local Variables:
  compile-command: "fpcdebug shadow_volume_test.dpr"
  End:
}
