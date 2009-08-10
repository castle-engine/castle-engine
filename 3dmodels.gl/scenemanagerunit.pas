{
  Copyright 2009 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ Scene manager: TSceneManager class. }
unit SceneManagerUnit;

interface

uses VectorMath, VRMLGLScene, VRMLScene, Navigation,
  VRMLGLHeadLight, ShadowVolumes, GL, GLCubeMap;

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
      @item(rendering the scene from camera given by Navigator,)
      @item(and making multiple passes for shadow volumes and generated textures.)
    )

    TODO: this should also provide "wrapper" methods around all owned
    scenes. Much like TVRMLGLAnimation does now for scene items.
    Except that we would like to eliminate these TVRMLGLAnimation methods,
    everything should use SceneManager then.
  }
  TSceneManager = class
  private
    FScene: TVRMLGLScene;
    FNavigator: TNavigator;

    FShadowVolumesPossible: boolean;
    FShadowVolumes: boolean;
    FShadowVolumesDraw: boolean;
    FSV: TShadowVolumes;

    FViewportX: TGLint;
    FViewportY: TGLint;
    FViewportWidth: TGLsizei;
    FViewportHeight: TGLsizei;

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
    property Scene: TVRMLGLScene read FScene write FScene;
    property Navigator: TNavigator read FNavigator write FNavigator;

    property ShadowVolumesPossible: boolean read FShadowVolumesPossible write FShadowVolumesPossible;
    property ShadowVolumes: boolean read FShadowVolumes write FShadowVolumes;
    property ShadowVolumesDraw: boolean read FShadowVolumesDraw write FShadowVolumesDraw;
    property SV: TShadowVolumes read FSV write FSV;

    property ViewportX: TGLint read FViewportX write FViewportX;
    property ViewportY: TGLint read FViewportY write FViewportY;
    property ViewportWidth: TGLsizei read FViewportWidth write FViewportWidth;
    property ViewportHeight: TGLsizei read FViewportHeight write FViewportHeight;

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
    function ViewerToChanges: TPostRedisplayChanges; virtual;
  end;

implementation

uses RenderStateUnit, KambiGLUtils;

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
  RenderState.CameraFromNavigator(Navigator);

  Scene.PrepareRender(TG, Options);
end;

procedure TSceneManager.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
begin
  if TransparentGroup = tgTransparent then
    Scene.LastRender_SumNext;

  if InShadow then
    Scene.RenderFrustum(RenderState.CameraFrustum, TransparentGroup, @Scene.LightRenderInShadow) else
    Scene.RenderFrustum(RenderState.CameraFrustum, TransparentGroup, nil);
end;

procedure TSceneManager.RenderShadowVolumes;
begin
  Scene.InitAndRenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure TSceneManager.RenderHeadLight;
begin
  TVRMLGLHeadlight.RenderOrDisable(Scene.Headlight, 0,
    RenderState.Target = rtScreen, Navigator);
end;

function TSceneManager.ViewerToChanges: TPostRedisplayChanges;
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
  Scene.UpdateGeneratedTextures(@RenderFromView,
    Scene.WalkProjectionNear, Scene.WalkProjectionFar,
    true, 0, 0,
    ViewportX, ViewportY,
    ViewportWidth, ViewportHeight);

  RenderState.CameraFromNavigator(Navigator);
  RenderState.Target := rtScreen;
  RenderFromView;
end;

end.
