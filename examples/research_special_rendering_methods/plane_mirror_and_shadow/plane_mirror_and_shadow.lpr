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

{ Demo of mirror plane and plane-projected shadows.
  Since they both utilize similar technique with stencil buffer,
  it was comfortable to implement them both in one demo program.

  Some docs about this on WWW (I also used some old knowledge, and
  "Real-time rendering" by Moller + Haines):
  - mirror:
    http://www.opengl.org/resources/code/samples/mjktips/Reflect.html
    http://www.opengl.org/resources/code/samples/mjktips/TimHall_Reflections.txt
  - plane-projected shadows:
    first part of http://www.devmaster.net/articles/shadows/
    http://www.devmaster.net/articles/shadowprojection/

  Keys:
    awsd change light position[x, y], qQ change light position[z]

    pP to change plane distance to the object

    Navigation: like view3dscene in Examine mode,
    see http://castle-engine.sourceforge.net/view3dscene.php.

    See menu shortcuts for other keys.

  Command-line params:
    Optional $1 is the initial 3d model URL to load.
    If not given, a simple internal cube model is loaded.

    You can later change it using "Open" menu item anyway.
    You can load any model in VRML / X3D (or any other supported
    format, see [http://castle-engine.sourceforge.net/view3dscene.php]).
    See inside ../3d_rendering_processing/data/ directory,
    or [http://castle-engine.sourceforge.net/demo_models.php]
    for some interesting models. :)
}
program plane_mirror_and_shadow;

{$I castleconf.inc}

uses SysUtils, Classes,
  CastleVectors, CastleBoxes, X3DNodes, CastleGL, CastleWindow, CastleRenderer,
  CastleClassUtils, CastleUtils, X3DLoad, CastleLog,
  CastleGLUtils, CastleScene, CastleCameras, CastleRenderingCamera, CastleParameters,
  CastleFilesUtils, CastleStringUtils, CastleKeysMouse, CastleSceneManager,
  CastleColors, CastleURIUtils, CastleApplicationProperties;

var
  Window: TCastleWindowCustom;

  Scene: TCastleScene;
  SceneForShadow: TCastleScene;
  RenderParams: TBasicRenderParams;
  LightNode: TPointLightNode;
  LightInstance: TLightInstance;

  { URL of currently loaded Scene.
    '' means to load internal cube model. }
  SceneURL: string;

  { 4th component must be always 1, because LightNode
    is always created as positional }
  LightPosition: TVector4Single = (0, 0, 3, 1);

  RotationAngle: Single;

  PlaneDistance: Single = 1.0;

  ShadowBlend: boolean = false;
  UseStencil: boolean = true;
  Shadow: boolean = false;
  Mirror: boolean = true;

const
  ClearColor: TCastleColor = (0.5, 0.5, 0.5, 1);

var
  PlaneConstCoord: Integer = 2;
  { PlaneOtherCoord* are always the other 2 numbers from 0..2 range,
    ordered, left over from PlaneConstCoord. }
  PlaneOtherCoord1: Integer = 0;
  PlaneOtherCoord2: Integer = 1;

function Vector3Split(const ValueOnConstCoord, ValueOnOtherCoords: Single):
  TVector3Single;
begin
  Result[PlaneConstCoord] := ValueOnConstCoord;
  Result[PlaneOtherCoord1] := ValueOnOtherCoords;
  Result[PlaneOtherCoord2] := ValueOnOtherCoords;
end;

function Vector4Split(const ValueOnConstCoord, ValueOnOtherCoords,
  ValueLast: Single): TVector4Single;
begin
  Result[PlaneConstCoord] := ValueOnConstCoord;
  Result[PlaneOtherCoord1] := ValueOnOtherCoords;
  Result[PlaneOtherCoord2] := ValueOnOtherCoords;
  Result[3] := ValueLast;
end;

type
  { Our specialized scene manager.
    This isn't too clean, this program doesn't play nicely with our scene
    manager.

    - We override RenderFromViewEverything, without calling inherited,
      to basically override the whole rendering work:
      - we'll clear gl buffers ourselves,
      - load camera matrix ourselves,
      - and we do not need scene manager's automatic headlight handling
        (in InitializeLights, normally called by RenderFromViewEverything).

    - We also do not add Scene or SceneForShadow currently for scene manager:
      no point right now.

    SceneManager is here mostly used to keep Camera. }
  TMySceneManager = class(TCastleSceneManager)
  public
    procedure RenderFromViewEverything; override;
  end;

procedure TMySceneManager.RenderFromViewEverything;

  function PlaneProjectedShadowMatrix(
    const Plane: TVector4Single;
    const LightPosition: TVector4Single): TMatrix4Single;
  var
    Dot: Single;
  begin
    Dot := VectorDotProduct(LightPosition, Plane);

    { Based on http://www.devmaster.net/articles/shadows/ }

    Result[0][0] := Dot  - LightPosition[0] * Plane[0];
    Result[1][0] :=      - LightPosition[0] * Plane[1];
    Result[2][0] :=      - LightPosition[0] * Plane[2];
    Result[3][0] :=      - LightPosition[0] * Plane[3];

    Result[0][1] :=      - LightPosition[1] * Plane[0];
    Result[1][1] := Dot  - LightPosition[1] * Plane[1];
    Result[2][1] :=      - LightPosition[1] * Plane[2];
    Result[3][1] :=      - LightPosition[1] * Plane[3];

    Result[0][2] :=      - LightPosition[2] * Plane[0];
    Result[1][2] :=      - LightPosition[2] * Plane[1];
    Result[2][2] := Dot  - LightPosition[2] * Plane[2];
    Result[3][2] :=      - LightPosition[2] * Plane[3];

    Result[0][3] :=      - LightPosition[3] * Plane[0];
    Result[1][3] :=      - LightPosition[3] * Plane[1];
    Result[2][3] :=      - LightPosition[3] * Plane[2];
    Result[3][3] := Dot  - LightPosition[3] * Plane[3];
  end;

var
  Plane: TVector4Single;

  procedure DoShadow;
  begin
    glPushMatrix();
      glMultMatrix(PlaneProjectedShadowMatrix(Plane, LightPosition));
      glPushAttrib(GL_ENABLE_BIT or GL_DEPTH_BUFFER_BIT);
        glDisable(GL_LIGHTING);
        glColorv(Vector4Single(0, 0, 0, 0.2));

        if ShadowBlend then
        begin
          glEnable(GL_BLEND);
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        end;

        glDepthMask(GL_FALSE);

        if UseStencil then
        begin
          { stencil is already set such that only drawing over stencil is allowed }
          glDisable(GL_DEPTH_TEST);
        end else
        begin
          glEnable(GL_POLYGON_OFFSET_FILL);
          glEnable(GL_POLYGON_OFFSET_LINE);
          glEnable(GL_POLYGON_OFFSET_POINT);
          glPolygonOffset(-10, -10);
        end;

        glPushMatrix();
          glRotatef(RotationAngle, 1, 1, 1);
          { Render scene for shadow.
            SceneForShadow is a normal 3D model and it's rendered
            almost exactly like Scene (the same set of glVertex etc.),
            but it's rendered with Attributes.Mode = rmPureGeometry
            (we want it's color to be consistently black). }
          glPushAttrib(GL_DEPTH_BUFFER_BIT);
            { we want it to always pass depth test, stencil protects us from drawing
              in bad places }
            glDepthFunc(GL_ALWAYS);

            { TODO: RenderingCamera.Frustum is actually invalid.
              But we pass TestShapeVisibility = nil, and we don't use
              VisibilitySensor inside these models,
              so frustum value isn't really used. }

            SceneForShadow.Render(nil, RenderingCamera.Frustum, RenderParams);
          glPopAttrib();
        glPopMatrix();
      glPopAttrib();
    glPopMatrix();
  end;

  procedure DrawFloor; forward;

  procedure DoMirror;

    { Clears depth buffer (sets it's values to 1, i.e. maximum).
      Contrary to RenderContext.Clear([cbDepth]...) this honours stencil test,
      since it does the job by rasterizing a rectangle over the screen.

      Assumes that current matrix mode is modelview
      (it pushes / pops matrices and attribs and at the end restores
      matrix mode to modelview).

      Tests show that it has no impact on speed, it's really fast,
      just like glClear(GL_DEPTH_BUFFER_BIT).  }
    procedure ClearDepthBufferHonouringStencil;
    var
      SavedProjectionMatrix: TMatrix4Single;
    begin
      SavedProjectionMatrix := ProjectionMatrix;
      ProjectionMatrix := IdentityMatrix4Single;

      glPushMatrix;
        glLoadIdentity;

        glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_VIEWPORT_BIT
          or GL_DEPTH_BUFFER_BIT);
          glDisable(GL_LIGHTING); { saved by GL_ENABLE_BIT }
          glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); { saved by GL_COLOR_BUFFER_BIT }

          glDepthFunc(GL_ALWAYS); { saved by GL_DEPTH_BUFFER_BIT }
          glDepthRange(1, 1); { saved by GL_VIEWPORT_BIT }

          glRectf(-1, -1, 1,  1);
        glPopAttrib;
      glPopMatrix;

      ProjectionMatrix := SavedProjectionMatrix;
    end;

  begin
    { In this simple demo program, I could clear depth here just by
        glClear(GL_DEPTH_BUFFER_BIT);

      In more general, real-world program (when multiple non-coplanar mirrors
      may be present on the scene, that is something like DoMirror
      may be called multiple times) more intelligent approach is needed,
      that clears the depth buffer only where stencil allows (we will later
      set this to mirror's depth anyway, at the end of DoMirror). }
    ClearDepthBufferHonouringStencil;

    glPushAttrib(GL_ENABLE_BIT);

      glEnable(GL_CLIP_PLANE0);

        glPushMatrix();
          { TODO: make reflection matrix based on any Plane equation }
          glTranslatev(Vector3Split(-Plane[3], 0));
          glScalev(Vector3Split(-1, 1));
          glClipPlane(GL_CLIP_PLANE0, Vector4Double(Vector4Split(1, 0, 0)));
          glTranslatev(Vector3Split( Plane[3], 0));

          { note that the LightPosition should also be mirrored.
            No need to do it explicitly --- Scene renderer will automatically
            use light, transforming it by current modelview matrix,
            so it will get properly mirrored. }

          glRotatef(RotationAngle, 1, 1, 1);
          { Render scene for mirror. This is rendered just as usual,
            with depth testing working, opaque/transparent materials
            inside working as usual etc.

            We swap CCW to CW --- sides that were CCW previously (and had to
            be culled, or have normal vectors pointing from them) are now CW. }
          glFrontFace(GL_CW);

          { TODO: RenderingCamera.Frustum is actually invalid.
            But we pass TestShapeVisibility = nil, and we don't use
            VisibilitySensor inside these models,
            so frustum value isn't really used. }

          Scene.Render(nil, RenderingCamera.Frustum, RenderParams);
          glFrontFace(GL_CCW);
        glPopMatrix();

      glDisable(GL_CLIP_PLANE0);

      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      DrawFloor;
    glPopAttrib();
  end;

var
  Box: TBox3D;
  BoxMaxSize: Single;

  { Draw a simple OpenGL plane, with vertexes and normal vector for lighting.
    The plane lies in the dimension chosen by ConstCoord (0, 1 or 2).
    The other two plane coordinates are understood to be RestOf3dCoords from
    ZCoord. }
  procedure DrawPlane(const X1, Y1, X2, Y2, ConstValue: Single; const ConstCoord: Integer);

    procedure Vertex(const X, Y: Single);
    var
      XCoord, YCoord: Integer;
      V: TVector3Single;
    begin
      if ConstCoord = 1 then
      begin
        { TODO: this should be default RestOf3dCoords behavior,
          it makes normal directions consistent. }
        XCoord := 2;
        YCoord := 0;
      end else
        RestOf3dCoords(ConstCoord, XCoord, YCoord);
      V[XCoord] := X;
      V[YCoord] := Y;
      V[ConstCoord] := ConstValue;
      glVertexv(V);
    end;

  begin
    glBegin(GL_QUADS);
      glNormalv(UnitVector3Single[ConstCoord]);
      Vertex(X1, Y1);
      Vertex(X2, Y1);
      Vertex(X2, Y2);
      Vertex(X1, Y2);
    glEnd();
  end;

  procedure DrawFloor;
  begin
    { Set and enable light for direct fixed-function pipeline rendering
      (used by DrawPlane). This is reset after every TCastleScene.Render,
      so we have to repeat it. }
    glLightv(GL_LIGHT0, GL_POSITION, LightPosition);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    { For VRML Scene rendering, this is always enabled anyway,
      but for the plane (drawn without VRML renderer) this is needed. }
    glEnable(GL_DEPTH_TEST);

    { Render the plane where the shadow lies.
      The plane size and position is calculated to have a nice shadow receiver
      for Scene. }
    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(0.2, 0.2, 0, 0.3));
    glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(0  , 1  , 0, 0.3));
    DrawPlane(Box.Data[0, PlaneOtherCoord1] - BoxMaxSize, Box.Data[0, PlaneOtherCoord2] - BoxMaxSize,
              Box.Data[1, PlaneOtherCoord1] + BoxMaxSize, Box.Data[1, PlaneOtherCoord2] + BoxMaxSize,
              Box.Data[0, PlaneConstCoord] - PlaneDistance * BoxMaxSize,
              PlaneConstCoord);
  end;

begin
  RenderContext.Clear([cbColor, cbDepth, cbStencil], ClearColor);
  glLoadMatrix(RenderingCamera.Matrix);

  { set light position for Scene (by LightInstance and LightNode) }
  LightInstance.Location := Vector3SingleCut(LightPosition);
  LightNode.FdLocation.Value := Vector3SingleCut(LightPosition);

  { draw point indicating LightPosition }
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);

    glColorv(Yellow);
    RenderContext.PointSize := 10; { VRML renderer will reset it }
    glBegin(GL_POINTS);
      glVertexv(LightPosition);
    glEnd;
  glPopAttrib;

  Box := Scene.BoundingBox;
  if not Box.IsEmpty then
  begin
    { Render normal Scene }
    glPushMatrix();
      glRotatef(RotationAngle, 1, 1, 1);

      { TODO: RenderingCamera.Frustum is actually invalid.
        But we pass TestShapeVisibility = nil, and we don't use
        VisibilitySensor inside these models,
        so frustum value isn't really used. }

      Scene.Render(nil, RenderingCamera.Frustum, RenderParams);
    glPopMatrix();

    BoxMaxSize := Box.MaxSize;

    { set stencil to 1 where plane is drawn }
    if UseStencil then
    begin
      glStencilFunc(GL_ALWAYS, 1, 1);
      glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
      glEnable(GL_STENCIL_TEST);
    end;

    { For shadow, first draw of floor draws floor normally (with lighting etc.)
      to color, depth and stencil buffer.

      For mirror, this is more difficult. We also want to draw to stencil
      buffer, to prepare it's test. For color and depth buffers: we just
      want to clear them. This way we prepare for drawing mirrored scene
      in them. So we clear color buffer to ClearColor.
      Depth buffer will be cleared later in DoMirror.

      Note: in this simple demo program, it would be easier to just draw mirror
      *first* in Draw, before anything else is drawn. Then we're sure that
      color and depth buffer are clear, so no need to clear them.

      Disadvantages:
      - (Really small one:) this will cover more pixels, since mirror
        could possibly be occluded later by some objects. But this is really
        minor (non-existent, really) problem.

      - Real problem: such approach would break in a real-world program, when

        1. There can be more than 1 mirror (I mean, more than 1 mirror plane)
           in a scene, and mirrors may partially occlude each other.
           So I have to resort mirrors visibility while drawing them anyway,
           so the depth buffer can't stay always clear. (Otherwise, I would have
           to render to depth buffer everything else (including other mirrors)
           to get mirror visibility, to not override other mirrors.
           This makes one additional pass for each mirror, so is not acceptable.)

        2. Mirror surface itself may be partially transparent.
           (Example: a glass window, this is both partially transparent and mirror;
           TODO: very promising example, make a model with it actually to demo)
           So, like other partially transparent objects, it should be rendered
           after all opaque objects are rendered.

       So in a real-world general program, drawing mirror *first* doesn't really
       help. It would only work for this simple demo program with only 1 mirror.
       I decided to implement here a more complicated but also closer
       to real-world approach.
    }
    if Mirror then
    begin
      glDepthMask(GL_FALSE);
      glDisable(GL_LIGHTING);
      glColorv(ClearColor);
    end;

    DrawFloor;

    if Mirror then
    begin
      glEnable(GL_LIGHTING);
      glDepthMask(GL_TRUE);
    end;

    { Calculate Plane.
      Assumuing PlaneConstCoord = 2,Plane equation is
        Z = Box.Data[0, 2] - PlaneDistance * BoxMaxSize
      So it's
        0 * x + 0 * y + 1 * z - (Box.Data[0, 2] - PlaneDistance * BoxMaxSize) = 0
    }
    Plane := Vector4Split(1, 0,
      - (Box.Data[0, PlaneConstCoord] - PlaneDistance * BoxMaxSize));

    { everything else is drawn only on the floor }
    if UseStencil then
    begin
      glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
      glStencilFunc(GL_EQUAL, 1, 1);
    end;

    if Mirror then
      DoMirror;

    if Shadow then
      DoShadow;

    if UseStencil then
      glDisable(GL_STENCIL_TEST);
  end;
end;

var
  SceneManager: TCastleSceneManager;

procedure Open(Container: TUIContainer);
begin
end;

procedure Close(Container: TUIContainer);
begin
  Scene.GLContextClose;
  SceneForShadow.GLContextClose;
end;

procedure Update(Container: TUIContainer);

  procedure ChangeLightPosition(Coord, Change: Integer);
  begin
    LightPosition[Coord] += Change * Window.Fps.UpdateSecondsPassed * 5;
  end;

begin
  if Window.Pressed[K_A] then ChangeLightPosition(0, -1);
  if Window.Pressed[K_D] then ChangeLightPosition(0,  1);
  if Window.Pressed[K_S] then ChangeLightPosition(1, -1);
  if Window.Pressed[K_W] then ChangeLightPosition(1,  1);

  if Window.Pressed[K_Q] then
  begin
    if mkShift in Window.Pressed.Modifiers then
      ChangeLightPosition(2,  1) else
      ChangeLightPosition(2, -1);
  end;

  if Window.Pressed[K_P] then
  begin
    if mkShift in Window.Pressed.Modifiers then
      PlaneDistance -= Window.Fps.UpdateSecondsPassed * 5 else
      PlaneDistance += Window.Fps.UpdateSecondsPassed * 5;
  end;

  RotationAngle += Window.Fps.UpdateSecondsPassed * 5;
end;

{ menu ----------------------------------------------------------------------- }

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);
var
  S: string;
begin
  case MenuItem.IntData of
    10:
      begin
        S := SceneURL;
        if Window.FileDialog('Open 3D model file', S, true,
          Load3D_FileFilters) then
        begin
          SceneForShadow.BeforeNodesFree; { loading Scene will free also SceneForShadow.RootNode }

          Scene.Load(S, false);
          SceneForShadow.Load(Scene.RootNode, false);

          SceneURL := S;
          { reinit camera, since Scene.BoundingBox changed }
          (SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.1);
        end;
      end;
    12: Window.Close;
    60..62:
      begin
        PlaneConstCoord := MenuItem.IntData - 60;
        RestOf3dCoords(PlaneConstCoord, PlaneOtherCoord1, PlaneOtherCoord2);
      end;
    70: Shadow := not Shadow;
    80: Mirror := not Mirror;
    90: UseStencil := not UseStencil;
    100: ShadowBlend := not ShadowBlend;
  end;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
  R: TMenuItemRadio;
  RG: TMenuItemRadioGroup;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',         10, CtrlO));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',             12, CharEscape));
    Result.Append(M);

  M := TMenu.Create('_View');

    R := TMenuItemRadio.Create('_Plane X = const', 60, PlaneConstCoord = 0, true);
    RG := R.Group;
    M.Append(R);

    R := TMenuItemRadio.Create('_Plane Y = const', 61, PlaneConstCoord = 1, true);
    R.Group := RG;
    M.Append(R);

    R := TMenuItemRadio.Create('_Plane Z = const', 62, PlaneConstCoord = 2, true);
    R.Group := RG;
    M.Append(R);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Shadow', 70, Shadow, true));
    M.Append(TMenuItemChecked.Create('_Mirror', 80, Mirror, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Use stencil buffer', 90, UseStencil, true));
    M.Append(TMenuItemChecked.Create(
      '_Shadow by blending (looks better, but on most models causes artifacts)',
      100, ShadowBlend, true));
    Result.Append(M);
end;

{ main program --------------------------------------------------------------- }

var
  RootNode: TX3DRootNode;
begin
  Window := TCastleWindowCustom.Create(Application);

  { parse parameters  }
  Window.ParseParameters(StandardParseOptions);
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    SceneURL := Parameters[1] else
    SceneURL := '';

  SceneManager := TMySceneManager.Create(Application);
  SceneManager.DefaultVisibilityLimit := 100;
  Window.Controls.InsertFront(SceneManager);

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  { calculate RootNode }
  if SceneURL <> '' then
    RootNode := Load3D(SceneURL, true) else
    { use box, just to show anything }
    RootNode := LoadX3DClassicFromString('#VRML V1.0 ascii' + LineEnding +  'Cube { }', '');

  Scene := TCastleScene.Create(nil);
  try
    Scene.Load(RootNode, true);

    { init SceneForShadow.
      It doesn't own RootNode, and always has RootNode = Scene.RootNode }
    SceneForShadow := TCastleScene.Create(nil);
    SceneForShadow.Load(RootNode, false);
    SceneForShadow.Attributes.Mode := rmPureGeometry;

    { init light that we'll control }
    LightNode := TPointLightNode.Create;
    LightNode.FdLocation.Value := Vector3SingleCut(LightPosition);

    LightInstance.Node := LightNode;
    LightInstance.Transform := IdentityMatrix4Single;
    LightInstance.TransformScale := 1;
    LightInstance.Location := Vector3SingleCut(LightPosition);
    LightInstance.Radius := 1000;

    { parameters for render calls must contain our custom light }
    RenderParams := TBasicRenderParams.Create;
    RenderParams.FBaseLights.Add(LightInstance);

    { init SceneManager.Camera }
    SceneManager.Camera := TExamineCamera.Create(Window);
    (SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.02);

    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    Window.AutoRedisplay := true;

    Window.StencilBits := 1;

    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnUpdate := @Update;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally
    FreeAndNil(SceneForShadow);
    FreeAndNil(Scene);
    FreeAndNil(RenderParams);
    FreeAndNil(LightNode);
  end;
end.
