{
  Copyright 2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager: TSceneManager class. }
unit SceneManagerUnit;

interface

uses VectorMath, VRMLGLScene, VRMLScene, Cameras,
  VRMLGLHeadLight, ShadowVolumes, GL, GLCubeMap, UIControls, Base3D;

type
  { Scene manager that knows about all 3D things inside your world.

    Single scenes/models (like TVRMLGLScene or TVRMLGLAnimation instances)
    can be rendered directly, but it's not always comfortable.
    Scenes have to assume that they are "one of the many" inside your 3D world,
    which means that multi-pass rendering techniques have to be implemented
    at a higher level. This concerns the need for multiple passes from
    the same camera (for shadow volumes) and multiple passes from different
    cameras (for generating textures for shadow maps, cube map environment etc.).

    Scene manager overcomes this limitation. A single SceneManager object
    knows about all 3D things in your world, and renders them all for you,
    taking care of doing multiple rendering passes for particular features.
    Naturally, it also serves as container for all your visible 3D scenes.

    Idea is to make here TScene, that will be the ancestor of both
    TVRMLScene (and so also TVRMLGLScene) and TVRMLAnimation (and so
    also TVRMLAnimation), and allows to add other objects of your own
    that don't fit for whatever reason inside normal TVRMLGLScene/Animation.
    Scene manager will maintain a list (later maybe a hierarchical tree?)
    of TScene objects.

    TODO: for now this simply works with only a single TVRMLGLScene instance.
    So it just simplifies rendering of a single TVRMLGLScene.

    TSceneManager.Render can assume that it's the *only* manager rendering
    to the screen (although you can safely render more 3D geometry *after*
    calling TSceneManager.Render). So it's Render method takes care of

    @unorderedList(
      @item(clearing the screen,)
      @item(rendering the background of the scene (from main Scene),)
      @item(rendering the headlight (from the properties of main Scene),)
      @item(rendering the scene from given Camera,)
      @item(and making multiple passes for shadow volumes and generated textures.)
    )

    TODO: this should also provide "wrapper" methods around all owned
    scenes. Much like TVRMLGLAnimation does now for scene items.
    Except that we would like to eliminate these TVRMLGLAnimation methods,
    everything should use SceneManager then.

    This is a TUIControl descendant, which means it's adviced usage
    is to add this to TGLUIWindow.Controls or TKamOpenGLControl.Controls.
    TODO: This will pass TUIControl events to all the scenes inside.
    (for now, it does not, and scene should be added independently to Controls.)
  }
  TSceneManager = class(TUIControl)
  private
    FScene: TVRMLGLScene;
    FCamera: TCamera;

    FShadowVolumesPossible: boolean;
    FShadowVolumes: boolean;
    FShadowVolumesDraw: boolean;
    SV: TShadowVolumes;

    FBackgroundWireframe: boolean;
  protected
    { Render one pass, from current (in RenderState) camera view,
      for specific lights setup, for given TransparentGroup. }
    procedure RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup); virtual;

    procedure RenderShadowVolumes; virtual;

    { Render everything from current (in RenderState) camera view.
      RenderState.Target says to where we generate image.
      Takes care of making many passes for shadow volumes,
      but doesn't take care of updating generated textures. }
    procedure RenderFromView; virtual;

    { Render the headlight. Called by RenderFromView, when camera matrix
      is set. Should enable or disable OpenGL GL_LIGHT0 for headlight.

      Implementation in this class uses headlight defined
      in the Scene, following NavigationInfo.headlight and KambiHeadlight
      nodes. }
    procedure RenderHeadLight; virtual;

    { Render the 3D part of scene. Called by RenderFromView at the end,
      when everything (clearing, background, headlight, loading camera
      matrix) is done and all that remains is to pass to OpenGL actual 3D world. }
    procedure RenderFromView3D; virtual;
  public
    procedure GLContextInit; override;
    procedure GLContextClose; override;

    property Scene: TVRMLGLScene read FScene write FScene;
    property Camera: TCamera read FCamera write FCamera;

    { Should we make shadow volumes possible.
      This should indicate if OpenGL context was (possibly) initialized
      with stencil buffer. }
    property ShadowVolumesPossible: boolean read FShadowVolumesPossible write FShadowVolumesPossible;

    { Should we render with shadow volumes.
      You can change this at any time, to switch rendering shadows on/off.

      This works only if ShadowVolumesPossible is @true.

      Note that the shadow volumes algorithm makes some requirements
      about the 3D model: it must be 2-manifold, that is have a correctly
      closed volume. Otherwise, rendering results may be bad. You can check
      Scene.BorderEdges.Count before using this: BorderEdges.Count = 0 means
      that model is Ok, correct manifold.

      For shadows to be actually used you still need a light source
      marked as the main shadows light (kambiShadows = kambiShadowsMain = TRUE),
      see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows]. }
    property ShadowVolumes: boolean read FShadowVolumes write FShadowVolumes;

    { Actually draw the shadow volumes to the color buffer, for debugging.
      If shadows are rendered (see ShadowVolumesPossible and ShadowVolumes),
      you can use this to actually see shadow volumes, for debug / demo
      purposes. Shadow volumes will be rendered on top of the scene,
      as yellow blended polygons. }
    property ShadowVolumesDraw: boolean read FShadowVolumesDraw write FShadowVolumesDraw;

    { If yes then the scene background will be rendered wireframe,
      over the background filled with glClearColor.

      There's a catch here: this works only if the background is actually
      internally rendered as a geometry. If the background is rendered
      by clearing the screen (this is an optimized case of sky color
      being just one simple color, and no textures),
      then it will just cover the screen as normal, like without wireframe.
      This is uncertain situation anyway (what should the wireframe
      look like in this case anyway?), so I don't consider it a bug.

      Useful especially for debugging when you want to see how your background
      geometry looks like. }
    property BackgroundWireframe: boolean
      read FBackgroundWireframe write FBackgroundWireframe default false;

    procedure PrepareRender;
    procedure Render;

    { What changes happen when viewer camera changes.
      You may want to use it when calling Scene.ViewerChanges.

      Implementation in this class is correlated with RenderHeadlight. }
    function ViewerToChanges: TVisibleSceneChanges; virtual;
  end;

implementation

uses SysUtils, RenderStateUnit, KambiGLUtils;

procedure TSceneManager.GLContextInit;
begin
  inherited;

  { We actually need to do it only if ShadowVolumesPossible.
    But we can as well do it always, it's harmless (just checks some GL
    extensions). (Otherwise we'd have to handle SetShadowVolumesPossible.) }
  SV := TShadowVolumes.Create;
  SV.InitGLContext;
end;

procedure TSceneManager.GLContextClose;
begin
  FreeAndNil(SV);

  inherited;
end;

procedure TSceneManager.PrepareRender;
var
  Options: TPrepareRenderOptions;
  TG: TTransparentGroups;
begin
  Options := [prBackground, prBoundingBox];
  if ShadowVolumesPossible and
     ShadowVolumes and
     Scene.MainLightForShadowsExists then
    Options := Options + prShadowVolume;

  TG := [tgAll];
  if ShadowVolumesPossible then
    TG := TG + [tgOpaque, tgTransparent];

  { RenderState.Camera* must be already set,
    since PrepareRender may do some operations on texture gen modes
    in WORLDSPACE*. }
  RenderState.CameraFromCameraObject(Camera);

  Scene.PrepareRender(TG, Options);
end;

procedure TSceneManager.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
begin
  if InShadow then
    Scene.RenderFrustum(RenderState.CameraFrustum, TransparentGroup, @Scene.LightRenderInShadow) else
    Scene.RenderFrustum(RenderState.CameraFrustum, TransparentGroup, nil);
end;

procedure TSceneManager.RenderShadowVolumes;
begin
  Scene.RenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure TSceneManager.RenderHeadLight;
begin
  TVRMLGLHeadlight.RenderOrDisable(Scene.Headlight, 0,
    RenderState.Target = rtScreen, Camera);
end;

function TSceneManager.ViewerToChanges: TVisibleSceneChanges;
begin
  if Scene.Headlight <> nil then
    Result := [prVisibleSceneNonGeometry] else
    Result := [];
end;

procedure TSceneManager.RenderFromView3D;

  procedure RenderNoShadows;
  begin
    RenderScene(false, tgAll);
  end;

  procedure RenderWithShadows(const MainLightPosition: TVector4Single);
  begin
    SV.InitFrustumAndLight(RenderState.CameraFrustum, MainLightPosition);
    SV.Render(nil, @RenderScene, @RenderShadowVolumes, ShadowVolumesDraw);
  end;

begin
  if ShadowVolumesPossible and
     ShadowVolumes and
     Scene.MainLightForShadowsExists then
    RenderWithShadows(Scene.MainLightForShadows) else
    RenderNoShadows;
end;

procedure TSceneManager.RenderFromView;
var
  ClearBuffers: TGLbitfield;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if Scene.Background <> nil then
  begin
    glLoadMatrix(RenderState.CameraRotationMatrix);

    if BackgroundWireframe then
    begin
      { Color buffer needs clear *now*, before drawing background. }
      glClear(GL_COLOR_BUFFER_BIT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      try
        Scene.Background.Render;
      finally glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); end;
    end else
      Scene.Background.Render;
  end else
    ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

  if ShadowVolumesPossible and
     ShadowVolumes and
     Scene.MainLightForShadowsExists then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  glClear(ClearBuffers);

  glLoadMatrix(RenderState.CameraMatrix);

  RenderHeadLight;

  RenderFromView3D;
end;

procedure TSceneManager.Render;
begin
  { This assertion can break only if you misuse UseControls property, setting it
    to false (disallowing ContainerResize), and then trying to use Render. }
  Assert(ContainerSizeKnown, 'SceneManager did not receive ContainerResize event yet, cannnot Render');

  Scene.UpdateGeneratedTextures(@RenderFromView,
    Scene.WalkProjectionNear, Scene.WalkProjectionFar,
    { For now assume viewport fills the whole container,
      see ../../../doc/TODO.scene_manager_viewport }
    0, 0, ContainerWidth, ContainerHeight);

  RenderState.Target := rtScreen;
  RenderState.CameraFromCameraObject(Camera);
  RenderFromView;
end;

end.
