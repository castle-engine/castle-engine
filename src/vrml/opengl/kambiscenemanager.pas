{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager: TKamSceneManager class. }
unit KambiSceneManager;

interface

uses Classes, VectorMath, VRMLGLScene, VRMLScene, Cameras,
  VRMLGLHeadLight, ShadowVolumes, GL, UIControls, Base3D,
  KeysMouse, VRMLTriangle, Boxes3D;

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

    @link(Items) property keeps a tree of TBase3D objects.
    All our 3D objects, like TVRMLScene (and so also TVRMLGLScene)
    and TVRMLAnimation (and so also TVRMLGLAnimation) descend from
    TBase3D, and you can add them to the scene manager.
    And naturally you can implement your own TBase3D descendants,
    representing any 3D (possibly dynamic, animated and even interactive) object.

    TKamSceneManager.Render can assume that it's the @italic(only) manager rendering
    to the screen (although you can safely render more 3D geometry *after*
    calling TKamSceneManager.Render). So it's Render method takes care of

    @unorderedList(
      @item(clearing the screen,)
      @item(rendering the background of the scene (from main Scene),)
      @item(rendering the headlight (from the properties of main Scene),)
      @item(rendering the scene from given Camera,)
      @item(and making multiple passes for shadow volumes and generated textures.)
    )

    For some of these features, you'll have to set the @link(MainScene) property.

    This is a TUIControl descendant, which means it's adviced usage
    is to add this to TGLUIWindow.Controls or TKamOpenGLControl.Controls.
    This passes relevant TUIControl events to all the TBase3D objects inside.
  }
  TKamSceneManager = class(TUIControl)
  private
    FMainScene: TVRMLGLScene;
    FCamera: TCamera;
    FItems: TBase3DList;

    FShadowVolumesPossible: boolean;
    FShadowVolumes: boolean;
    FShadowVolumesDraw: boolean;
    SV: TShadowVolumes;

    FBackgroundWireframe: boolean;

    ApplyProjectionNeeded: boolean;
    FOnCameraChanged: TNotifyEvent;
    FOnBoundViewpointChanged: TNotifyEvent;
    FCameraBox: TBox3D;

    procedure SetMainScene(const Value: TVRMLGLScene);
    procedure SetCamera(const Value: TCamera);
    procedure SetShadowVolumesPossible(const Value: boolean);

    procedure ItemsVisibleChange(Sender: TObject);
    procedure ItemsAndCameraCursorChange(Sender: TObject);

    { camera callbacks }
    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    procedure CameraGetHeight(ACamera: TWalkCamera;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
    procedure CameraVisibleChange(ACamera: TObject);

    { scene callbacks }
    procedure SceneBoundViewpointChanged(Scene: TVRMLScene);
    procedure SceneBoundViewpointVectorsChanged(Scene: TVRMLScene);
  protected
    { These variables are writeable from overridden ApplyProjection. }
    FAngleOfViewX: Single;
    FAngleOfViewY: Single;
    FWalkProjectionNear: Single;
    FWalkProjectionFar : Single;

    procedure SetContainer(const Value: IUIContainer); override;

    { Triangles to ignore by all collision detection in scene manager.
      The default implementation in this class resturns always @false,
      so nothing is ignored. You can override it e.g. to ignore your "water"
      material, when you want player to dive under the water. }
    function CollisionIgnoreItem(const Octree: TVRMLBaseTrianglesOctree;
      const Triangle: PVRMLTriangle): boolean; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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

    { Sets OpenGL projection matrix, based on MainScene's currently
      bound Viewpoint, NavigationInfo and used camera.

      Takes care of updating Camera.ProjectionMatrix,
      AngleOfViewX, AngleOfViewY, WalkProjectionNear, WalkProjectionFar.

      This is automatically called at the beginning of our Render method,
      if it's needed (after MainScene or Container sizes or some other
      stuff changed).

      @seealso TVRMLGLScene.GLProjection }
    procedure ApplyProjection; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GLContextInit; override;
    procedure GLContextClose; override;

    { Prepare rendering resources, to make next @link(Render) call execute fast.

      If DisplayProgressTitle <> '', we will display progress bar during
      loading. This is especially useful for long precalculated animations
      (TVRMLGLAnimation with a lot of ScenesCount), they show nice
      linearly increasing progress bar. }
    procedure PrepareRender(const DisplayProgressTitle: string = '');

    procedure BeforeDraw; override;
    procedure Draw(const Focused: boolean); override;
    function DrawStyle: TUIControlDrawStyle; override;

    { What changes happen when viewer camera changes.
      You may want to use it when calling Scene.ViewerChanges.

      Implementation in this class is correlated with RenderHeadlight. }
    function ViewerToChanges: TVisibleSceneChanges; virtual;

    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;

    function KeyDown(Key: TKey; C: char): boolean; override;
    function KeyUp(Key: TKey; C: char): boolean; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function AllowSuspendForInput: boolean; override;

    { Overridden in TKamSceneManager to catch events regardless of mouse position. }
    function PositionInside(const X, Y: Integer): boolean; override;

    { Camera angles of view, in degrees.
      Set by every ApplyProjection call.

      @groupBegin }
    property AngleOfViewX: Single read FAngleOfViewX write FAngleOfViewX;
    property AngleOfViewY: Single read FAngleOfViewY write FAngleOfViewY;
    { @groupEnd }

    { Projection near/far values, for Walk navigation.
      ApplyProjection calculates it.

      This is the best projection near/far for Walk mode
      (although GLProjection may use some other values for other modes
      (like Examine), it will always calculate values for Walk mode anyway.)

      WalkProjectionFar may be ZFarInfinity.

      @groupBegin }
    property WalkProjectionNear: Single read FWalkProjectionNear;
    property WalkProjectionFar : Single read FWalkProjectionFar ;
    { @groupEnd }

    { Create default TCamera suitable for navigating in this scene.
      This is automatically used to initialize @link(Camera) property
      when @link(Camera) is @nil at ApplyProjection call.

      The implementation in base TKamSceneManager uses MainScene.CreateCamera
      (so it will follow your VRML/X3D scene Viewpoint, NavigationInfo and such).
      If MainScene is not assigned, we will just create a simple
      TExamineCamera. }
    function CreateDefaultCamera(AOwner: TComponent): TCamera; virtual;

    { If non-empty, then camera position will be limited to this box.

      When this property specifies an EmptyBox3D (the default value),
      camera position is limited to not fall because of gravity
      below minimal 3D world plane. That is, viewer can freely move
      around in 3D world, he/she only cannot fall below "minimal plane"
      when falling is caused by the gravity. "Minimal plane" is derived from
      GravityUp and Items.BoundingBox. }
    property CameraBox: TBox3D read FCameraBox write FCameraBox;
  published
    { Tree of 3D objects within your world. This is the place where you should
      add your scenes to have them handled by scene manager.
      You may also set your main TVRMLGLScene (if you have any) as MainScene.

      TBase3DList is also TBase3D instance, so yes --- this may be a tree
      of TBase3D, not only a flat list.

      Note that scene manager "hijacks" TBase3D callbacks TBase3D.OnCursorChange and
      TBase3D.OnVisibleChange. }
    property Items: TBase3DList read FItems;

    { The main scene of your 3D world. It's not necessary to set this
      (after all, your 3D world doesn't even need to have any TVRMLGLScene
      instance). This @italic(must be) also added to our @link(Items)
      (otherwise things will work strangely).

      When set, this is used for a couple of things:

      @unorderedList(
        @item Decides what headlight is used (by TVRMLGLScene.Headlight).
        @item Decides what background is rendered (by TVRMLGLScene.Background).
        @item(Decides if, and where, the main light casting shadows is
          (see TVRMLGLScene.MainLightForShadowsExists, TVRMLGLScene.MainLightForShadows).)
        @item Sets OpenGL projection for the scene, see ApplyProjection.

        @item(Synchronizes our @link(Camera) with VRML/X3D viewpoints.
          This means that @link(Camera) will be updated when VRML/X3D events
          change current Viewpoint, for example you can animate the camera
          by animating viewpoint (or it's transformation) or bind camera
          to a viewpoint.

          Note that scene manager "hijacks" some Scene events:
          TVRMLScene.OnBoundViewpointVectorsChanged and TVRMLScene.ViewpointStack.OnBoundChanged
          for this purpose. If you want to know when viewpoint changes,
          you can use scene manager's event OnBoundViewpointChanged.)
      )

      The above stuff is only sensible when done once per scene manager,
      that's why we need MainScene property to indicate this.
      (We cannot just use every 3D object from @link(Items) for this.)

      Freeing MainScene will automatically set this to @nil. }
    property MainScene: TVRMLGLScene read FMainScene write SetMainScene;

    { Camera used to render.

      Cannot be @nil when rendering. If you don't assign anything here,
      we'll create a default camera object at the nearest ApplyProjection
      call (this is the first moment when we really must have some camera).
      This default camera will be created by CreateDefaultCamera.

      This camera @italic(should not) be inside some other container
      (like on TGLUIWindow.Controls or TKamOpenGLControl.Controls list).
      Scene manager will handle passing events to the camera on it's own,
      we will also pass our own Container to Camera.Container.
      This is desired, this way events are correctly passed
      and interpreted before passing them to 3D objects.
      And this way we avoid the question whether camera should be before
      or after the scene manager on the Controls list (as there's really
      no perfect ordering for them).

      Scene manager will "hijack" some Camera events:
      TCamera.OnVisibleChange, TWalkCamera.OnMoveAllowed,
      TWalkCamera.OnGetCameraHeight, TCamera.OnCursorChange.
      Scene manager will handle them in a proper way.

      @seealso OnCameraChanged }
    property Camera: TCamera read FCamera write SetCamera;

    { Should we make shadow volumes possible.
      This should indicate if OpenGL context was (possibly) initialized
      with stencil buffer. }
    property ShadowVolumesPossible: boolean read FShadowVolumesPossible write SetShadowVolumesPossible default false;

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
    property ShadowVolumes: boolean read FShadowVolumes write FShadowVolumes default false;

    { Actually draw the shadow volumes to the color buffer, for debugging.
      If shadows are rendered (see ShadowVolumesPossible and ShadowVolumes),
      you can use this to actually see shadow volumes, for debug / demo
      purposes. Shadow volumes will be rendered on top of the scene,
      as yellow blended polygons. }
    property ShadowVolumesDraw: boolean read FShadowVolumesDraw write FShadowVolumesDraw default false;

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

    { Called on any camera change. Exactly when TCamera generates it's
      OnVisibleChange event. }
    property OnCameraChanged: TNotifyEvent read FOnCameraChanged write FOnCameraChanged;

    { Called when bound Viewpoint node changes. This is called exactly when
      TVRMLScene.ViewpointStack.OnBoundChanged is called. }
    property OnBoundViewpointChanged: TNotifyEvent read FOnBoundViewpointChanged write FOnBoundViewpointChanged;
  end;

procedure Register;

implementation

uses SysUtils, RenderStateUnit, KambiGLUtils, ProgressUnit;

procedure Register;
begin
  RegisterComponents('Kambi', [TKamSceneManager]);
end;

constructor TKamSceneManager.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TBase3DList.Create(Self);
  FItems.OnVisibleChange := @ItemsVisibleChange;
  FItems.OnCursorChange := @ItemsAndCameraCursorChange;
  { Items is displayed and streamed with TKamSceneManager
    (and in the future this should allow design Items.List by IDE),
    so set some sensible Name. }
  FItems.Name := 'Items';

  FCameraBox := EmptyBox3D;
end;

destructor TKamSceneManager.Destroy;
begin
  if FMainScene <> nil then
  begin
    FMainScene.RemoveFreeNotification(Self);
    FMainScene := nil;
  end;

  if FCamera <> nil then
  begin
    FCamera.RemoveFreeNotification(Self);

    { Yes, this setting FCamera to nil is needed, it's not just paranoia.

      Consider e.g. when our Camera is owned by Self
      (e.g. because it was created in ApplyProjection by CreateDefaultCamera).
      This means that this camera will be freed in "inherited" destructor call
      below. Since we just did FCamera.RemoveFreeNotification, we would have
      no way to set FCamera to nil, and FCamera would then remain as invalid
      pointer.

      And when SceneManager is freed it sends a free notification
      (this is also done in "inherited" destructor) to TGLUIWindow instance,
      which causes removing us from TGLUIWindow.Controls list,
      which causes SetContainer(nil) call that tries to access Camera.

      This scenario would cause segfault, as FCamera pointer is invalid
      as this time. }

    FCamera := nil;
  end;

  inherited;
end;

procedure TKamSceneManager.ItemsVisibleChange(Sender: TObject);
begin
  VisibleChange;
end;

procedure TKamSceneManager.GLContextInit;
begin
  inherited;

  { We actually need to do it only if ShadowVolumesPossible.
    But we can as well do it always, it's harmless (just checks some GL
    extensions). (Otherwise we'd have to handle SetShadowVolumesPossible.) }
  SV := TShadowVolumes.Create;
  SV.InitGLContext;
end;

procedure TKamSceneManager.GLContextClose;
begin
  Items.GLContextClose;

  FreeAndNil(SV);

  inherited;
end;

function TKamSceneManager.CreateDefaultCamera(AOwner: TComponent): TCamera;
var
  Box: TBox3d;
begin
  Box := Items.BoundingBox;
  if MainScene <> nil then
    Result := MainScene.CreateCamera(AOwner, Box) else
  begin
    Result := TExamineCamera.Create(AOwner);
    (Result as TExamineCamera).Init(Box,
      { CameraRadius = } Box3dAvgSize(Box, 1.0) * 0.005);
  end;
end;

procedure TKamSceneManager.ApplyProjection;
begin
  if Camera = nil then
    Camera := CreateDefaultCamera(Self);

  if MainScene <> nil then
    MainScene.GLProjection(Camera, Items.BoundingBox,
      ContainerWidth, ContainerHeight, ShadowVolumesPossible,
      FAngleOfViewX, FAngleOfViewY, FWalkProjectionNear, FWalkProjectionFar);
end;

procedure TKamSceneManager.SetMainScene(const Value: TVRMLGLScene);
begin
  if FMainScene <> Value then
  begin
    if FMainScene <> nil then
    begin
      FMainScene.RemoveFreeNotification(Self);
      FMainScene.OnBoundViewpointVectorsChanged := nil;
      FMainScene.ViewpointStack.OnBoundChanged := nil;
    end;

    FMainScene := Value;

    if FMainScene <> nil then
    begin
      FMainScene.FreeNotification(Self);
      FMainScene.OnBoundViewpointVectorsChanged := @SceneBoundViewpointVectorsChanged;
      FMainScene.ViewpointStack.OnBoundChanged := @SceneBoundViewpointChanged;

      { Call initial ViewerChanged (this allows ProximitySensors to work
        as soon as ProcessEvents becomes true). }
      if Camera <> nil then
        MainScene.ViewerChanged(Camera, ViewerToChanges);
    end;

    ApplyProjectionNeeded := true;
  end;
end;

procedure TKamSceneManager.SetCamera(const Value: TCamera);
begin
  if FCamera <> Value then
  begin
    if FCamera <> nil then
    begin
      FCamera.RemoveFreeNotification(Self);
      FCamera.OnVisibleChange := nil;
      FCamera.OnCursorChange := nil;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := nil;
        TWalkCamera(FCamera).OnGetCameraHeight := nil;
      end;

      FCamera.Container := nil;
    end;

    FCamera := Value;

    if FCamera <> nil then
    begin
      FCamera.FreeNotification(Self);
      { Unconditionally change FCamera.OnVisibleChange callback,
        to override TGLUIWindow / TKamOpenGLControl that also try
        to "hijack" this camera's event. }
      FCamera.OnVisibleChange := @CameraVisibleChange;
      FCamera.OnCursorChange := @ItemsAndCameraCursorChange;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := @CameraMoveAllowed;
        TWalkCamera(FCamera).OnGetCameraHeight := @CameraGetHeight;
      end;

      FCamera.Container := Container;
      if ContainerSizeKnown then
        FCamera.ContainerResize(ContainerWidth, ContainerHeight);

      { Call initial ViewerChanged (this allows ProximitySensors to work
        as soon as ProcessEvents becomes true). }
      if MainScene <> nil then
        MainScene.ViewerChanged(Camera, ViewerToChanges);
    end;

    { Changing camera changes also the view rapidly. }
    if MainScene <> nil then
      MainScene.ViewChangedSuddenly;

    ApplyProjectionNeeded := true;
  end;
end;

procedure TKamSceneManager.SetContainer(const Value: IUIContainer);
begin
  inherited;

  { Keep Camera.Container always the same as our Container }
  if Camera <> nil then
    Camera.Container := Container;
end;

procedure TKamSceneManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FCamera then
    begin
      FCamera := nil;
      { Need ApplyProjection, to create new default camera before rendering. }
      ApplyProjectionNeeded := true;
    end;

    if AComponent = FMainScene then
      FMainScene := nil;

    { Maybe ApplyProjectionNeeded := true also for MainScene cleaning?
      But ApplyProjection doesn't set projection now, when MainScene is @nil. }
  end;
end;

procedure TKamSceneManager.SetShadowVolumesPossible(const Value: boolean);
begin
  if FShadowVolumesPossible <> Value then
  begin
    FShadowVolumesPossible := Value;
    ApplyProjectionNeeded := true;
  end;
end;

procedure TKamSceneManager.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  ApplyProjectionNeeded := true;

  if Camera <> nil then
  begin
    Camera.ContainerResize(AContainerWidth, AContainerHeight);
  end;
end;

procedure TKamSceneManager.PrepareRender(const DisplayProgressTitle: string);
var
  Options: TPrepareRenderOptions;
  TG: TTransparentGroups;
begin
  Options := [prBackground, prBoundingBox];
  TG := [tgAll];

  if ShadowVolumesPossible and
     ShadowVolumes and
     (MainScene <> nil) and
     MainScene.MainLightForShadowsExists then
  begin
    Options := Options + prShadowVolume;
    TG := TG + [tgOpaque, tgTransparent];
  end;

  { Apply projection now, as TVRMLGLScene.GLProjection calculates
    BackgroundSkySphereRadius, which is used by MainScene.Background.
    Otherwise our preparations of "prBackground" here would be useless,
    as BackgroundSkySphereRadius will change later, and MainScene.Background
    will have to be recreated. }
  Assert(ContainerSizeKnown, 'SceneManager did not receive ContainerResize event yet, cannnot PrepareRender');
  if ApplyProjectionNeeded then
  begin
    ApplyProjectionNeeded := false;
    ApplyProjection;
  end;

  { RenderState.Camera* must be already set,
    since PrepareRender may do some operations on texture gen modes
    in WORLDSPACE*. }
  RenderState.CameraFromCameraObject(Camera);

  if DisplayProgressTitle <> '' then
  begin
    Progress.Init(Items.PrepareRenderSteps, DisplayProgressTitle, true);
    try
      Items.PrepareRender(TG, Options, true);
    finally Progress.Fini end;
  end else
    Items.PrepareRender(TG, Options, false);
end;

procedure TKamSceneManager.BeforeDraw;
begin
  inherited;
  PrepareRender;
end;

procedure TKamSceneManager.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
begin
  Items.Render(RenderState.CameraFrustum, TransparentGroup, InShadow);
end;

procedure TKamSceneManager.RenderShadowVolumes;
begin
  Items.RenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure TKamSceneManager.RenderHeadLight;
var
  H: TVRMLGLHeadlight;
begin
  if MainScene <> nil then
    H := MainScene.Headlight { this may still return @nil if no headlight } else
    H := nil;

  TVRMLGLHeadlight.RenderOrDisable(H, 0, RenderState.Target = rtScreen, Camera);
end;

function TKamSceneManager.ViewerToChanges: TVisibleSceneChanges;
var
  H: TVRMLGLHeadlight;
begin
  if MainScene <> nil then
    H := MainScene.Headlight { this may still return @nil if no headlight } else
    H := nil;

  if H <> nil then
    Result := [prVisibleSceneNonGeometry] else
    Result := [];
end;

procedure TKamSceneManager.RenderFromView3D;

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
     (MainScene <> nil) and
     MainScene.MainLightForShadowsExists then
    RenderWithShadows(MainScene.MainLightForShadows) else
    RenderNoShadows;
end;

procedure TKamSceneManager.RenderFromView;
var
  ClearBuffers: TGLbitfield;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if (MainScene <> nil) and
     (MainScene.Background <> nil) then
  begin
    glLoadMatrix(RenderState.CameraRotationMatrix);

    if BackgroundWireframe then
    begin
      { Color buffer needs clear *now*, before drawing background. }
      glClear(GL_COLOR_BUFFER_BIT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      try
        MainScene.Background.Render;
      finally glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); end;
    end else
      MainScene.Background.Render;
  end else
    ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

  if ShadowVolumesPossible and
     ShadowVolumes and
     (MainScene <> nil) and
     MainScene.MainLightForShadowsExists then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  glClear(ClearBuffers);

  glLoadMatrix(RenderState.CameraMatrix);

  RenderHeadLight;

  RenderFromView3D;
end;

procedure TKamSceneManager.Draw(const Focused: boolean);
begin
  inherited;

  { This assertion can break only if you misuse UseControls property, setting it
    to false (disallowing ContainerResize), and then trying to use Render.

    We need to know container size now, for UpdateGeneratedTextures lower
    and for ApplyProjection. }
  Assert(ContainerSizeKnown, 'SceneManager did not receive ContainerResize event yet, cannnot Render');

  if ApplyProjectionNeeded then
  begin
    ApplyProjectionNeeded := false;
    ApplyProjection;
  end;

  { TODO: do UpdateGeneratedTextures for all Items }

  if MainScene <> nil then
  begin
    MainScene.UpdateGeneratedTextures(@RenderFromView,
      WalkProjectionNear, WalkProjectionFar,
      { For now assume viewport fills the whole container,
        see ../../../doc/TODO.scene_manager_viewport }
      0, 0, ContainerWidth, ContainerHeight);
  end;

  RenderState.Target := rtScreen;
  RenderState.CameraFromCameraObject(Camera);
  RenderFromView;
end;

function TKamSceneManager.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds3D;
end;

function TKamSceneManager.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Camera <> nil then
  begin
    Result := Camera.KeyDown(Key, C);
    if Result then Exit;
  end;

  Result := Items.KeyDown(Key, C);
end;

function TKamSceneManager.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Camera <> nil then
  begin
    Result := Camera.KeyUp(Key, C);
    if Result then Exit;
  end;

  Result := Items.KeyUp(Key, C);
end;

function TKamSceneManager.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Camera <> nil then
  begin
    Result := Camera.MouseDown(Button);
    if Result then Exit;
  end;

  Result := Items.MouseDown(Button);
end;

function TKamSceneManager.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Camera <> nil then
  begin
    Result := Camera.MouseUp(Button);
    if Result then Exit;
  end;

  Result := Items.MouseUp(Button);
end;

function TKamSceneManager.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  RayOrigin, RayDirection: TVector3Single;
begin
  Result := inherited;
  if (not Result) and (Camera <> nil) then
  begin
    Result := Camera.MouseMove(OldX, OldY, NewX, NewY);
    if not Result then
    begin
      Camera.Ray(NewX, NewY, AngleOfViewX, AngleOfViewY, RayOrigin, RayDirection);
      Result := Items.MouseMove(RayOrigin, RayDirection);
    end;
  end;

  { update the cursor, since scene under the cursor possibly changed. }
  ItemsAndCameraCursorChange(Self);
end;

procedure TKamSceneManager.ItemsAndCameraCursorChange(Sender: TObject);
begin
  { We have to treat Camera.Cursor specially:
    - mcNone because of mouse look means result in unconditionally mcNone.
      Other Items.Cursor, MainScene.Cursor etc. is ignored then.
    - otherwise, Camera.Cursor is ignored, show 3D objects cursor. }
  if (Camera <> nil) and (Camera.Cursor = mcNone) then
  begin
    Cursor := mcNone;
    Exit;
  end;

  if MainScene <> nil then
    Cursor := MainScene.Cursor else
    Cursor := mcDefault;

  { TODO: how to account for other Items?
    (this UpdateCursor is already called for all Items.OnCursorChange.
    I just don't know yet how to handle them.)
    Topmost 3d object should determine the cursor, right? }
end;

procedure TKamSceneManager.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  { As for LetOthersHandleMouseAndKeys: let Camera decide it.
    By default, camera has ExclusiveEvents = false and will let
    LetOthersHandleMouseAndKeys remain = true, that's Ok.

    Our Items do not have HandleMouseAndKeys or LetOthersHandleMouseAndKeys
    stuff, as it would not be controllable for them: 3D objects do not
    have strict front-to-back order, so we would not know in what order
    call their Idle methods, so we have to let many Items handle keys anyway.
    So, it's consistent to just treat 3D objects as "cannot definitely
    mark keys/mouse as handled". Besides, currently 3D objects do not
    get Pressed information at all. }

  if Camera <> nil then
  begin
    LetOthersHandleMouseAndKeys := not Camera.ExclusiveEvents;
    Camera.Idle(CompSpeed, HandleMouseAndKeys, LetOthersHandleMouseAndKeys);
  end else
    LetOthersHandleMouseAndKeys := true;

  Items.Idle(CompSpeed);
end;

function TKamSceneManager.AllowSuspendForInput: boolean;
begin
  Result := (Camera = nil) or Camera.AllowSuspendForInput;
end;

function TKamSceneManager.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

procedure TKamSceneManager.CameraVisibleChange(ACamera: TObject);
begin
  if MainScene <> nil then
    { MainScene.ViewerChanged will cause MainScene.[On]VisibleChange,
      that (assuming here that MainScene is also on Items) will cause
      ItemsVisibleChange that will cause our own VisibleChange.
      So this way MainScene.ViewerChanged will also cause our VisibleChange. }
    MainScene.ViewerChanged(Camera, ViewerToChanges) else
    VisibleChange;

  if Assigned(OnCameraChanged) then
    OnCameraChanged(ACamera);
end;

function TKamSceneManager.CollisionIgnoreItem(const Octree: TVRMLBaseTrianglesOctree;
  const Triangle: PVRMLTriangle): boolean;
begin
  Result := false;
end;

function TKamSceneManager.CameraMoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Items.MoveAllowed(ACamera.Position, ProposedNewPos, NewPos,
    ACamera.CameraRadius, @CollisionIgnoreItem);

  if Result then
  begin
    if IsEmptyBox3D(FCameraBox) then
    begin
      { Don't let user to fall outside of the box because of gravity. }
      if BecauseOfGravity then
        Result := SimpleKeepAboveMinPlane(NewPos, Items.BoundingBox,
          ACamera.GravityUp);
    end else
      Result := Box3DPointInside(NewPos, FCameraBox);
  end;
end;

procedure TKamSceneManager.CameraGetHeight(ACamera: TWalkCamera;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItem: PVRMLTriangle;
begin
  Items.GetCameraHeight(ACamera.Position, ACamera.GravityUp,
    @CollisionIgnoreItem,
    IsAboveTheGround, SqrHeightAboveTheGround, GroundItem);
end;

procedure TKamSceneManager.SceneBoundViewpointChanged(Scene: TVRMLScene);
begin
  if Camera <> nil then
    Scene.CameraBindToViewpoint(Camera, false);

  { bound Viewpoint.fieldOfView changed, so update projection }
  ApplyProjectionNeeded := true;

  if Assigned(OnBoundViewpointChanged) then
    OnBoundViewpointChanged(Self);
end;

procedure TKamSceneManager.SceneBoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  if Camera <> nil then
    Scene.CameraBindToViewpoint(Camera, true);
end;

end.
