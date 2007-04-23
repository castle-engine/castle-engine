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

  Be aware that code of this program is a little messy at some places,
  because one of the goals of this program is to experiment and test with various
  variants of shadow volume making. The most practical version to be
  used right now is to define USE_WRAP (requires OpenGL >= 1.4 or EXT_stencil_wrap
  extension, available almost everywhere) and
  USE_STENCIL_OP_SEPARATE (requires OpenGL >= 2.0).
  If you have OpenGL < 2.0, define CULL_USING_OPENGL instead of
  USE_STENCIL_OP_SEPARATE.

  This program displays two models:
  - static scene
  - shadow caster that casts shadows (on itself and on the scene).
    Ideally, shadow caster should be composed from a number of closed
    manifold parts to make rendering fast (see documentation of
    TVRMLFlatSceneGL.RenderShadowQuads for description of two different
    shadow quads drawing algorithms). When program starts, it tells you
    whether shadow caster was detected to be a closed manifold.
  - we also need separate VRML file with exactly one light definition.
    This will be the light used to cast shadows (right now, this must
    be a positional light). The scene can contain additional lights,
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

  TODO: move this to menu items:
    m = prints current ShadowCaster transformation as VRML MatrixTransformation
        node
    space = toggle shadow quads showing
}

program shadow_volume_test;

{ This requires only 1 stencil bit. But it's absolutely incorrect
  method, and will work good only for the simplest shadow casters
  (e.g. ./shadow_volume_test_extrasimple.sh --- one triangle). }
{ $define XOR_TRICK}

{ This is highly adviced. Requires gl >= 1.4 or EXT_stencil_wrap extension }
{$define USE_WRAP}

{ This uses OpenGL to cull front/back faces.
  Although OpenGL culling is for sure faster than culling done by our
  code, on the other hand I have to pass the same set of faces two times.
  While when I cull by my own code, in the second pass I already
  detected which faces are back. So practically when culling with my own
  code, I have 2 times less culling tests to do.

  Note that glStencilOpSeparate (avail in OpenGL >= 2.0)
  allows to combine these two advantages: only one pass will
  be needed, and OpenGL can detect back/front faces. }
{$define CULL_USING_OPENGL}

{ This means that glStencilOpSeparate will be used.
  Requires OpenGL >= 2.0, but allows only one pass, and OpenGL detects
  front/back faces. So this is the most desirable and practical approach. }
{$define USE_STENCIL_OP_SEPARATE}

{$apptype CONSOLE}

uses GLWindow, GLW_Navigated, OpenGLh, KambiGLUtils, VRMLFlatSceneGL,
  VRMLNodes, MatrixNavigation, VRMLFlatScene, Boxes3d, SysUtils,
  KambiUtils, VectorMath, VRMLLightSetGL, VRMLFields,
  KambiClassUtils, KambiFilesUtils, KambiStringUtils, VRMLCameraUtils,
  ShadowTests, GLWinMessages;

var
  Scene, ShadowCaster: TVRMLFlatSceneGL;
  ShadowCasterNav: TMatrixExaminer;
  LightSet: TVRMLLightSetGL;

  ShowShadowQuads: boolean = false;
  IsRenderSilhouetteEdges: boolean = false;

{ glw callbacks ------------------------------------------------------------ }

const
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

    If it's not enough, USE_WRAP will help a little --- but inevitably
    it will show some artifacts at some places.
    Run ./shadow_volume_test_parallels.sh to see such nasty case.

    For XOR_TRICK it should always be 1 --- that's the point of XOR_TRICK. }
  StencilShadowBits = {$ifdef XOR_TRICK} $1 {$else} $FF {$endif};

procedure Draw(glwin: TGLWindow);

  procedure RenderEverything;
  begin
    Scene.Render(nil, tgAll);
    glPushMatrix;
      glMultMatrix(ShadowCasterNav.Matrix);
      ShadowCaster.Render(nil, tgAll);
    glPopMatrix;
  end;

  procedure RenderFrontShadowQuads;
  begin
    ShadowTests.RenderFrontShadowQuads(ShadowCaster,
      LightSet.Lights.Items[0].TransfLocation,
      Glw.NavWalker.CameraPos, ShadowCasterNav.Matrix);
  end;

  procedure RenderBackShadowQuads;
  begin
    ShadowTests.RenderBackShadowQuads(ShadowCaster);
  end;

  procedure RenderAllShadowQuads;
  begin
    ShadowCaster.RenderShadowQuads(
      LightSet.Lights.Items[0].TransfLocation,
      ShadowCasterNav.Matrix);
  end;

  procedure DoRenderSilhouetteEdges;
  begin
    if ShadowCaster.ManifoldEdges <> nil then
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_POLYGON_OFFSET_LINE);
        glPolygonOffset(1, 1);
        glColor4f(1, 1, 0, 0.3);
        RenderSilhouetteEdges(ShadowCaster,
          LightSet.Lights.Items[0].TransfLocation,
          ShadowCasterNav.Matrix);
      glPopAttrib;
    end;
  end;

var
  StencilOpIncrWrap, StencilOpDecrWrap: TGLenum;
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glLoadMatrix(Glw.Navigator.Matrix);

  StencilOpIncrWrap := {$ifdef USE_WRAP} GL_INCR_WRAP_EXT {$else} GL_INCR {$endif};
  StencilOpDecrWrap := {$ifdef USE_WRAP} GL_DECR_WRAP_EXT {$else} GL_DECR {$endif};

  { Normal rendering, without shadows, would go now simply like
      LightSet.RenderLights;
      RenderEverything;
    Rendering with hard shadows by SV algorithm below: }
  RenderEverything;
  glEnable(GL_STENCIL_TEST);
    { Note that stencil buffer is set to all 0 now. }

    { Calculate shadows to the stencil buffer.
      Don't write anything to depth or color buffers. }
    glSetDepthAndColorWriteable(GL_FALSE);
      glStencilFunc(GL_ALWAYS, 0, 0);
      {$ifdef USE_STENCIL_OP_SEPARATE}
        glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, StencilOpIncrWrap);
        glStencilOpSeparate(GL_BACK , GL_KEEP, GL_KEEP, StencilOpDecrWrap);
        RenderAllShadowQuads;
      {$else USE_STENCIL_OP_SEPARATE}
        {$ifndef XOR_TRICK}

          { For each fragment that passes depth-test, *increase* it's stencil
            value by 1. Render front facing shadow quads. }
          glStencilOp(GL_KEEP, GL_KEEP, StencilOpIncrWrap);
          {$ifdef CULL_USING_OPENGL}
          glEnable(GL_CULL_FACE);
          glCullFace(GL_BACK);
          RenderAllShadowQuads;
          {$else}
          RenderFrontShadowQuads;
          {$endif}

          { For each fragment that passes depth-test, *decrease* it's stencil
            value by 1. Render back facing shadow quads. }
          glStencilOp(GL_KEEP, GL_KEEP, StencilOpDecrWrap);
          {$ifdef CULL_USING_OPENGL}
          glCullFace(GL_FRONT);
          RenderAllShadowQuads;
          glDisable(GL_CULL_FACE);
          {$else}
          RenderBackShadowQuads;
          {$endif}

        {$else XOR_TRICK}
          glStencilOp(GL_KEEP, GL_KEEP, StencilOpIncrWrap);
          RenderAllShadowQuads;
        {$endif XOR_TRICK}
      {$endif USE_STENCIL_OP_SEPARATE}

    glSetDepthAndColorWriteable(GL_TRUE);

    { Now render everything once again, with lights turned on.
      But render only things not in shadow. }
    glClear(GL_DEPTH_BUFFER_BIT);
    glPushAttrib(GL_LIGHTING_BIT);
      LightSet.RenderLights;
      { setup stencil : don't modify stencil, stencil test passes only for =0 }
      glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
      glStencilFunc(GL_EQUAL, 0, StencilShadowBits);

      RenderEverything;
    glPopAttrib;
  glDisable(GL_STENCIL_TEST);

  if ShowShadowQuads then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glColor4f(1, 1, 0, 0.3);
      glDepthMask(GL_FALSE);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      RenderAllShadowQuads;
    glPopAttrib;
  end;

  if IsRenderSilhouetteEdges then
    DoRenderSilhouetteEdges;
end;

procedure ResizeGL(glwin: TGLWindow);
begin
  glViewport(0, 0, glwin.Width, glwin.Height);
  ProjectionGLPerspective(30, glwin.Width/glwin.Height,
    Box3dAvgSize(scene.BoundingBox)*0.05,
    Box3dAvgSize(scene.BoundingBox)*20.0);
end;

procedure InitGL(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.1, 0.1, 0.1, 0.0);

  glStencilMask(StencilShadowBits);

  { Tests:
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.6, 0.6, 0.6, 1)); }

  Scene.PrepareRender([tgAll], [prBoundingBox]);
  ShadowCaster.PrepareRender([tgAll], [prBoundingBox] + prShadowQuads);
end;

procedure CloseGL(glwin: TGLWindow);
begin
  Scene.CloseGL;
  ShadowCaster.CloseGL;
  LightSet.CloseGL;
end;

var
  FirstIdle: boolean = true;

procedure IdleGL(glwin: TGLWindow);
var
  S: string;
begin
  if FirstIdle then
  begin
    { We do MessageOK in first OnIdle, instead of e.g. in OnInit,
      because in OnIdle we're already corrected initialized
      (first OnResize and OnInit for sure are done now, and OnDraw may
      work as usual) and so the background under MessageOK is good now. }
    if ShadowCaster.ManifoldEdges <> nil then
      S := 'Shadow caster is composed from a closed manifolds.' +nl+
           'That''s GOOD --- shadows will be fast.' else
      S := 'Shadow caster is not composed from a closed manifolds.' +nl+
           'That''s BAD --- shadows may be really slow.';

    MessageOK(Glwin, S, taMiddle);
    FirstIdle := false;
  end;

  ShadowCasterNav.Idle(glwin.FpsCompSpeed, @Glwin.KeysDown, Glwin.MousePressed);
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
var NodeMatrix: TNodeMatrixTransform;
begin
  ShadowCasterNav.KeyDown(key, c, @Glwin.KeysDown);

  { TODO: change this to menu (and update docs) }
  case c of
    CtrlC:
       begin
         Writeln(MakeVRMLCameraStr(1,
           Glw.NavWalker.CameraPos,
           Glw.NavWalker.CameraDir,
           Glw.NavWalker.CameraUp,
           Glw.NavWalker.GravityUp));
       end;
   'm':begin
         NodeMatrix := TNodeMatrixTransform.Create('ShadowCasterNav_Matrix', '');
         try
           NodeMatrix.FdMatrix.Matrix := ShadowCasterNav.Matrix;
           SavetoVRMLFile(NodeMatrix, StdOutStream, '');
         finally NodeMatrix.Free end;
       end;
   ' ':begin
         ShowShadowQuads := not ShowShadowQuads;
         glwin.PostRedisplay;
       end;
   CtrlE:begin
         IsRenderSilhouetteEdges := not IsRenderSilhouetteEdges;
         glwin.PostRedisplay;
       end;
  end;
end;

{ main ------------------------------------------------------------ }

var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  LightSetVrmlName: string;
  SceneVrmlName: string;
  ShadowCasterVrmlName: string;
begin
  try
    { parse params }
    Glw.ParseParameters(StandardParseOptions);
    Parameters.CheckHigh(3);
    LightSetVrmlName := Parameters[1];
    SceneVrmlName := Parameters[2];
    ShadowCasterVrmlName := Parameters[3];

    { init vrml-related objects }
    LightSet := TVRMLLightSetGL.Create(
      ParseVRMLFile(LightSetVrmlName, false), true, 0, -1);
    Scene := TVRMLFlatSceneGL.Create(
      ParseVRMLFile(SceneVrmlName, false), true, roSceneAsAWhole);
    Scene.Attributes.UseLights := true;
    Scene.Attributes.FirstGLFreeLight := 1;
    ShadowCaster := TVRMLFlatSceneGL.Create(
      ParseVRMLFile(ShadowCasterVrmlName, false), true, roSceneAsAWhole);

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
    VectorAdjustToLengthTo1st(CamDir, Box3dAvgSize(Scene.BoundingBox)*0.02);
    Glw.NavWalker.Init(CamPos, CamDir, CamUp, GravityUp, 0.0, 0.0);

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
