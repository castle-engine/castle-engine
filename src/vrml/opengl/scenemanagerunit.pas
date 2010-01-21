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

uses Classes, VectorMath, VRMLGLScene, VRMLScene, Cameras,
  VRMLGLHeadLight, ShadowVolumes, GL, GLCubeMap, UIControls, Base3D,
  KeysMouse, VRMLTriangle;

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

    For some of these features, you'll have to set the @link(MainScene) property.

    This is a TUIControl descendant, which means it's adviced usage
    is to add this to TGLUIWindow.Controls or TKamOpenGLControl.Controls.
    This passes relevant TUIControl events to all the TBase3D objects inside.
  }
  TSceneManager = class(TUIControl)
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

    procedure SetMainScene(const Value: TVRMLGLScene);
    procedure SetCamera(const Value: TCamera);
    procedure SetShadowVolumesPossible(const Value: boolean);

    procedure ItemsVisibleChange(Sender: TObject);

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

    { Overridden in TSceneManager to catch events regardless of mouse position. }
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
  published
    { Tree of 3D objects within your world. This is the place where you should
      add your scenes to have them handled by scene manager.
      You may also set your main TVRMLGLScene (if you have any) as MainScene.

      TBase3DList is also TBase3D instance, so yes --- this may be a tree
      of TBase3D, not only a flat list. }
    property Items: TBase3DList read FItems;

    { The main scene of your 3D world. It's not necessary to set this
      (after all, your world doesn't even need to have any TVRMLGLScene
      instance). It may be, but doesn't have to be, also added to
      our @link(Items).

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

          Note that scene manager "hijacks" Scene events
          OnBoundViewpointVectorsChanged and ViewpointStack.OnBoundChanged
          for this purpose. If you want to know when viewpoint changes,
          you can use scene manager's event OnBoundViewpointChanged.)
      )

      The above stuff is only sensible when done once per scene manager,
      that's why we need MainScene property to indicate this.
      (We cannot just use every 3D object from @link(Items) for this.)

      Freeing MainScene will automatically set this to @nil. }
    property MainScene: TVRMLGLScene read FMainScene write SetMainScene;

    { Camera used to render. Cannot be @nil when rendering.

      Your camera must be inside some container
      (i.e. on TGLUIWindow.Controls or TKamOpenGLControl.Controls list),
      at least at the time of MouseMove methods call
      (that is, when container passed mouse events to this scene).

      Scene manager will "hijack" some Camera events:
      OnVisibleChange, OnMoveAllowed, OnGetCameraHeight. Scene manager
      will handle them in a proper way. Fear not, Camera.OnVisibleChange
      will still cause our own TSceneManager.VisibleChange,
      so your window will be properly repainted if scene manager is on the
      Controls list (and it should be). Also, each camera change
      (Camera.OnVisibleChange) will be reported to OnCameraChanged,
      so you can catch it there. }
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
  RegisterComponents('Kambi', [TSceneManager]);
end;

constructor TSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TBase3DList.Create(Self);
  FItems.OnVisibleChange := @ItemsVisibleChange;
  { Items is displayed and streamed with TSceneManager
    (and in the future this should allow design Items.List by IDE),
    so set some sensible Name. }
  FItems.Name := 'Items';
end;

destructor TSceneManager.Destroy;
begin
  if FMainScene <> nil then
    FMainScene.RemoveFreeNotification(Self);

  if FCamera <> nil then
    FCamera.RemoveFreeNotification(Self);

  inherited;
end;

procedure TSceneManager.ItemsVisibleChange(Sender: TObject);
begin
  VisibleChange;
end;

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
  Items.GLContextClose;

  FreeAndNil(SV);

  inherited;
end;

procedure TSceneManager.ApplyProjection;
begin
  if MainScene <> nil then
    MainScene.GLProjection(Camera, Items.BoundingBox,
      ContainerWidth, ContainerHeight, ShadowVolumesPossible,
      FAngleOfViewX, FAngleOfViewY, FWalkProjectionNear, FWalkProjectionFar);
end;

procedure TSceneManager.SetMainScene(const Value: TVRMLGLScene);
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

procedure TSceneManager.SetCamera(const Value: TCamera);
begin
  if FCamera <> Value then
  begin
    if FCamera <> nil then
    begin
      FCamera.RemoveFreeNotification(Self);
      FCamera.OnVisibleChange := nil;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := nil;
        TWalkCamera(FCamera).OnGetCameraHeight := nil;
      end;
    end;

    FCamera := Value;

    if FCamera <> nil then
    begin
      FCamera.FreeNotification(Self);
      { Unconditionally change FCamera.OnVisibleChange callback,
        to override TGLUIWindow / TKamOpenGLControl that also try
        to "hijack" this camera's event. }
      FCamera.OnVisibleChange := @CameraVisibleChange;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := @CameraMoveAllowed;
        TWalkCamera(FCamera).OnGetCameraHeight := @CameraGetHeight;
      end;

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

procedure TSceneManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FCamera then
      FCamera := nil;

    if AComponent = FMainScene then
      FMainScene := nil;

    { Maybe ApplyProjectionNeeded := true, for both FCamera and FMainScene
      clearing ? But ApplyProjection will simply fail now when Camera = nil. }
  end;
end;

procedure TSceneManager.SetShadowVolumesPossible(const Value: boolean);
begin
  if FShadowVolumesPossible <> Value then
  begin
    FShadowVolumesPossible := Value;
    ApplyProjectionNeeded := true;
  end;
end;

procedure TSceneManager.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  ApplyProjectionNeeded := true;
end;

procedure TSceneManager.PrepareRender(const DisplayProgressTitle: string);
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

procedure TSceneManager.BeforeDraw;
begin
  inherited;
  PrepareRender;
end;

procedure TSceneManager.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
begin
  Items.Render(RenderState.CameraFrustum, TransparentGroup, InShadow);
end;

procedure TSceneManager.RenderShadowVolumes;
begin
  Items.RenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure TSceneManager.RenderHeadLight;
var
  H: TVRMLGLHeadlight;
begin
  if MainScene <> nil then
    H := MainScene.Headlight { this may still return @nil if no headlight } else
    H := nil;

  TVRMLGLHeadlight.RenderOrDisable(H, 0, RenderState.Target = rtScreen, Camera);
end;

function TSceneManager.ViewerToChanges: TVisibleSceneChanges;
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
     (MainScene <> nil) and
     MainScene.MainLightForShadowsExists then
    RenderWithShadows(MainScene.MainLightForShadows) else
    RenderNoShadows;
end;

procedure TSceneManager.RenderFromView;
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

procedure TSceneManager.Draw(const Focused: boolean);
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

function TSceneManager.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds3D;
end;

function TSceneManager.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Items.KeyDown(Key, C);
end;

function TSceneManager.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Items.KeyUp(Key, C);
end;

function TSceneManager.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Items.MouseDown(Button);
end;

function TSceneManager.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Items.MouseUp(Button);
end;

function TSceneManager.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  RayOrigin, RayDirection: TVector3Single;
begin
  Result := inherited;
  if Result then Exit;

  Camera.Ray(NewX, NewY, AngleOfViewX, AngleOfViewY, RayOrigin, RayDirection);
  Result := Items.MouseMove(RayOrigin, RayDirection);
end;

procedure TSceneManager.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  Items.Idle(CompSpeed);

  { Even if mouse is over the scene, still allow others (like a Camera
    underneath) to always handle mouse and keys in their Idle. }
  LetOthersHandleMouseAndKeys := true;
end;

function TSceneManager.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

procedure TSceneManager.CameraVisibleChange(ACamera: TObject);
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

function TSceneManager.CollisionIgnoreItem(const Octree: TVRMLBaseTrianglesOctree;
  const Triangle: PVRMLTriangle): boolean;
begin
  Result := false;
end;

function TSceneManager.CameraMoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Items.MoveAllowed(ACamera.Position, ProposedNewPos, NewPos,
    ACamera.CameraRadius, @CollisionIgnoreItem);

  if Result then
  begin
    { TODO: allow here specification of other box, like LevelBox for castle level. }

    { Don't let user to fall outside of the box because of gravity. }
    if Result and BecauseOfGravity then
      Result := SimpleKeepAboveMinPlane(NewPos, Items.BoundingBox,
        ACamera.GravityUp);
  end;
end;

procedure TSceneManager.CameraGetHeight(ACamera: TWalkCamera;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItem: PVRMLTriangle;
begin
  Items.GetCameraHeight(ACamera.Position, ACamera.GravityUp,
    @CollisionIgnoreItem,
    IsAboveTheGround, SqrHeightAboveTheGround, GroundItem);
end;

procedure TSceneManager.SceneBoundViewpointChanged(Scene: TVRMLScene);
begin
  if Camera <> nil then
    Scene.CameraBindToViewpoint(Camera, false);

  { bound Viewpoint.fieldOfView changed, so update projection }
  ApplyProjectionNeeded := true;

  if Assigned(OnBoundViewpointChanged) then
    OnBoundViewpointChanged(Self);
end;

procedure TSceneManager.SceneBoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  if Camera <> nil then
    Scene.CameraBindToViewpoint(Camera, true);
end;

end.
