{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TGLWindowVRMLBrowser class, simple VRML browser in a single
  TGLWindow window. }
unit GLWindowVRMLBrowser;

interface

uses Classes, VectorMath, GLWindow, VRMLNodes, VRMLGLScene, VRMLScene,
  Navigation, VRMLGLHeadLight, ShadowVolumes, SceneManagerUnit;

type
  { A simple VRML browser in a window. This manages TVRMLGLScene and
    navigator (automatically adjusted to NavigationInfo.type).
    Octress are also automatically used (you only have to set Scene.Spatial
    to anything <> [], like typical [ssRendering, ssDynamicCollisions]).
    You simply call @link(Load) method and all is done.

    This class tries to be a thin (not really "opaque")
    wrapper around Scene / Navigator objects. Which means that
    you can access many functionality by directly accessing
    Scene or Navigator objects methods/properties.
    In particular you're permitted to access and call:

    @unorderedList(
      @item(@link(TVRMLScene.ProcessEvents Scene.ProcessEvents))
      @item(@link(TVRMLScene.Spatial Scene.Spatial),
        and other octree properties)
      @item(@link(TVRMLScene.RegisterCompiledScript Scene.RegisterCompiledScript))
      @item(@link(TVRMLScene.LogChanges Scene.LogChanges))
      @item(Changing VRML graph:

        You can freely change @link(TVRMLScene.RootNode Scene.RootNode)
        contents, provided that you call appropriate Scene.ChangedXxx method.

        You can also freely call events on the VRML nodes.

        You can access BackgroundStack and other stacks.)

      @item(Automatically managed Scene properties, like
        @link(TVRMLScene.BoundingBox Scene.BoundingBox),
        @link(TVRMLScene.TrianglesList Scene.TrianglesList),
        @link(TVRMLScene.ManifoldEdges Scene.ManifoldEdges),
        @link(TVRMLScene.ManifoldEdges Scene.BorderEdges)
        are also free to use.)

      @item(You can also change @link(TVRMLGLScene.Optimization
        Scene.Optimization). You can also change rendering attributes
        by @link(TVRMLGLScene.Attributes Scene.Attributes).)
    )

    Some important things that you @italic(cannot) mess with:

    @unorderedList(
      @item(Don't create/free Scene, Navigator and such objects yourself.
        This class manages them, they are always non-nil.)
    )

    This is very simple to use, but note that for more advanced uses
    you're not really expected to extend this class. Instead, you can
    implement something more suitable for you using your own
    TVRMLGLScene and navigator management.
    In other words, remember that this class just provides
    a simple "glue" between the key components of our engine.
    For specialized cases, more complex scenarios may be needed.

    If you're looking for Lazarus component that does basically the same
    (easy VRML browser), you want to check out TKamVRMLBrowser
    (file @code(../packages/components/kambivrmlbrowser.pas)). }
  TGLWindowVRMLBrowser = class(TGLUIWindow)
  private
    FScene: TVRMLGLScene;

    AngleOfViewX, AngleOfViewY: Single;

    function MoveAllowed(ANavigator: TWalkNavigator;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    procedure GetCameraHeight(ANavigator: TWalkNavigator;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    procedure ScenePostRedisplay(Scene: TVRMLScene);
    procedure VisibleChange(ANavigator: TObject);
    procedure BoundViewpointChanged(Scene: TVRMLScene);
    procedure BoundViewpointVectorsChanged(Scene: TVRMLScene);
    procedure GeometryChanged(Scene: TVRMLScene;
      const SomeLocalGeometryChanged: boolean);

    procedure UpdateCursor(Sender: TObject);
  private
    FShadowVolumesPossible: boolean;
    procedure SetShadowVolumesPossible(const Value: boolean);
  private
    FShadowVolumes: boolean;
    FShadowVolumesDraw: boolean;
    SV: TShadowVolumes;

    SceneManager: TSceneManager;
    { Set all SceneManager properties. This is a temporary solution,
      in the future initializing SceneManager properties should be integrated
      with this unit, and generally most of this unit's functionality
      should move to scene manager. }
    procedure InitSceneManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Creates new @link(Scene), with new navigator and such. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);

    property Scene: TVRMLGLScene read FScene;

    procedure EventBeforeDraw; override;
    procedure EventDraw; override;
    procedure EventInit; override;
    procedure EventClose; override;
    procedure EventIdle; override;
    procedure EventResize; override;
    procedure EventMouseDown(Btn: TMouseButton); override;
    procedure EventMouseUp(Btn: TMouseButton); override;
    procedure EventMouseMove(NewX, NewY: Integer); override;
    procedure EventKeyDown(Key: TKey; C: char); override;
    procedure EventKeyUp(Key: TKey; C: char); override;

    { Should we make shadow volumes possible?

      This can be changed only when the context is not initialized,
      that is only when the window is currently closed.
      Reason: to make shadows possible, we have to initialize gl context
      specially (with stencil buffer).

      Note that the shadows will not be actually rendered until you also
      set ShadowVolumes := true. }
    property ShadowVolumesPossible: boolean
      read FShadowVolumesPossible write SetShadowVolumesPossible default false;

    { Should we render with shadow volumes?
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
    property ShadowVolumes: boolean
      read FShadowVolumes write FShadowVolumes default false;

    { Actually draw the shadow volumes to the color buffer, for debugging.
      If shadows are rendered (see ShadowVolumesPossible and ShadowVolumes),
      you can use this to actually see shadow volumes, for debug / demo
      purposes. Shadow volumes will be rendered on top of the scene,
      as yellow blended polygons. }
    property ShadowVolumesDraw: boolean
      read FShadowVolumesDraw write FShadowVolumesDraw default false;
  end;

implementation

uses Boxes3d, VRMLOpenGLRenderer, GL, GLU,
  KambiClassUtils, KambiUtils, SysUtils, Object3dAsVRML,
  KambiGLUtils, KambiFilesUtils, VRMLTriangle,
  RaysWindow, BackgroundGL;

{ This uses OctreeCollisions, so either OctreeDynamicCollisions
  or OctreeCollidableTriangles, whichever is available. }

constructor TGLWindowVRMLBrowser.Create(AOwner: TComponent);
begin
  inherited;

  SceneManager := TSceneManager.Create;

  Load(nil, true);
end;

destructor TGLWindowVRMLBrowser.Destroy;
begin
  FreeAndNil(SceneManager);
  FreeAndNil(FScene);
  Navigator.Free;
  inherited;
end;

procedure TGLWindowVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadAsVRML(SceneFileName, false), true);
end;

procedure TGLWindowVRMLBrowser.Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);
begin
  FreeAndNil(FScene);
  Navigator.Free;

  FScene := TVRMLGLScene.Create(ARootNode, OwnsRootNode, roSeparateShapes);

  { initialize octrees titles }
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';

  { init Navigator }
  Navigator := Scene.CreateNavigator(nil);
  Navigator.OnVisibleChange := @VisibleChange;

  if Navigator is TWalkNavigator then
  begin
    WalkNav.OnMoveAllowed := @MoveAllowed;
    WalkNav.OnGetCameraHeight := @GetCameraHeight;
  end;

  { prepare for events procesing (although we let the decision whether
    to turn ProcessEvent := true to the caller). }
  Scene.ResetWorldTimeAtLoad;
  Scene.OnPostRedisplay := @ScenePostRedisplay;
  Scene.OnBoundViewpointVectorsChanged := @BoundViewpointVectorsChanged;
  Scene.ViewpointStack.OnBoundChanged := @BoundViewpointChanged;
  Scene.OnGeometryChanged := @GeometryChanged;
  Scene.OnPointingDeviceSensorsChange := @UpdateCursor;

  InitSceneManager;

  { Call initial ViewerChanged (this allows ProximitySensors to work
    as soon as ProcessEvent becomes true). }
  Scene.ViewerChanged(Navigator, SceneManager.ViewerToChanges);

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  if not Closed then
  begin
    EventResize;
    PostRedisplay;
  end;
end;

procedure TGLWindowVRMLBrowser.InitSceneManager;
begin
  SceneManager.Scene := Scene;
  SceneManager.Navigator := Navigator;
  SceneManager.ShadowVolumesPossible := ShadowVolumesPossible;
  SceneManager.ShadowVolumes := ShadowVolumes;
  SceneManager.ShadowVolumesDraw := ShadowVolumesDraw;
  SceneManager.SV := SV;
  SceneManager.ViewportX := 0;
  SceneManager.ViewportY := 0;
  SceneManager.ViewportWidth := Width;
  SceneManager.ViewportHeight := Height;
end;

procedure TGLWindowVRMLBrowser.EventBeforeDraw;
begin
  InitSceneManager;
  SceneManager.PrepareRender;
  inherited;
end;

procedure TGLWindowVRMLBrowser.EventDraw;
begin
  InitSceneManager;
  SceneManager.Render;
  inherited;
end;

procedure TGLWindowVRMLBrowser.EventInit;
begin
  inherited;
  glEnable(GL_LIGHTING);

  if ShadowVolumesPossible then
  begin
    SV := TShadowVolumes.Create;
    SV.InitGLContext;
  end;
end;

procedure TGLWindowVRMLBrowser.EventClose;
begin
  { This may be called in case of some exceptions, so we better we prepared
    for Scene = nil case. }
  if Scene <> nil then
    Scene.CloseGL;

  FreeAndNil(SV);

  inherited;
end;

procedure TGLWindowVRMLBrowser.EventIdle;
begin
  inherited;
  Scene.IncreaseWorldTime(Fps.IdleSpeed);
end;

procedure TGLWindowVRMLBrowser.EventResize;
begin
  inherited;
  Scene.GLProjection(Navigator, Scene.BoundingBox,
    Width, Height, AngleOfViewX, AngleOfViewY, ShadowVolumesPossible);
end;

procedure TGLWindowVRMLBrowser.EventMouseDown(Btn: TMouseButton);
begin
  inherited;
  { TODO: this should be done automatically by adding Scene to Controls. }
  Scene.MouseDown(MouseX, MouseY, Btn, MousePressed);
end;

procedure TGLWindowVRMLBrowser.EventMouseUp(Btn: TMouseButton);
begin
  inherited;
  { TODO: this should be done automatically by adding Scene to Controls. }
  Scene.MouseUp(MouseX, MouseY, Btn, MousePressed);
end;

procedure TGLWindowVRMLBrowser.UpdateCursor(Sender: TObject);

  function SensorsCount: Cardinal;
  begin
    if Scene.PointingDeviceSensors <> nil then
      Result := Scene.PointingDeviceSensors.EnabledCount else
      Result := 0;
    if Scene.PointingDeviceActiveSensor <> nil then
      Inc(Result);
  end;

begin
  { I want to keep assertion that CursorNonMouseLook = gcHand when
    we're over or keeping active some pointing-device sensors. }
  if SensorsCount <> 0 then
    CursorNonMouseLook := gcHand else
    CursorNonMouseLook := gcDefault;
end;

procedure TGLWindowVRMLBrowser.EventMouseMove(NewX, NewY: Integer);
begin
  inherited;

  { TODO: this should be done automatically by adding Scene to Controls.
    How to pass Navigator to it?
    How to pass AngleOfViewX, AngleOfViewY? }
  Scene.MouseMove(Navigator, AngleOfViewX, AngleOfViewY,
    MouseX, MouseY, NewX, NewY, MousePressed, Pressed);
end;

procedure TGLWindowVRMLBrowser.EventKeyDown(Key: TKey; C: char);
begin
  inherited;
  Scene.KeyDown(Key, C, Pressed);
end;

procedure TGLWindowVRMLBrowser.EventKeyUp(Key: TKey; C: char);
begin
  inherited;
  Scene.KeyUp(Key, C, Pressed);
end;

function TGLWindowVRMLBrowser.MoveAllowed(ANavigator: TWalkNavigator;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if Scene.OctreeCollisions <> nil then
  begin
    Result := Scene.OctreeCollisions.MoveAllowed(
      ANavigator.CameraPos, ProposedNewPos, NewPos, ANavigator.CameraRadius);
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;

  { Don't let user to fall outside of the box because of gravity. }
  if Result and BecauseOfGravity then
    Result := SimpleKeepAboveMinPlane(NewPos, Scene.BoundingBox,
      ANavigator.GravityUp);
end;

procedure TGLWindowVRMLBrowser.GetCameraHeight(ANavigator: TWalkNavigator;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItem: PVRMLTriangle;
begin
  if Scene.OctreeCollisions <> nil then
  begin
    Scene.OctreeCollisions.GetCameraHeight(
      ANavigator.CameraPos,
      ANavigator.GravityUp,
      IsAboveTheGround, SqrHeightAboveTheGround, GroundItem,
      nil, nil);
  end else
  begin
    { When octree is not available, we actually don't want gravity to
      cause falling down. So return values pretending we're standing
      still on the ground. }
    IsAboveTheGround := true;
    SqrHeightAboveTheGround := Sqr(ANavigator.CameraPreferredHeight);
  end;
end;

procedure TGLWindowVRMLBrowser.ScenePostRedisplay(Scene: TVRMLScene);
begin
  PostRedisplay;
end;

procedure TGLWindowVRMLBrowser.VisibleChange(ANavigator: TObject);
begin
  { Navigator.OnVisibleChange callback is initialized in constructor
    before Scene is initialized. So to be on the safest side, we check
    here Scene <> nil. }

  if Scene <> nil then
  begin
    InitSceneManager;
    Scene.ViewerChanged(Navigator, SceneManager.ViewerToChanges);
  end;
end;

procedure TGLWindowVRMLBrowser.BoundViewpointChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, false);
end;

procedure TGLWindowVRMLBrowser.BoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, true);
end;

procedure TGLWindowVRMLBrowser.GeometryChanged(Scene: TVRMLScene;
  const SomeLocalGeometryChanged: boolean);
begin
  { Scene.GeometryChanged possibly cleared pointing device info by
    PointingDeviceClear. This means that cursor must be updated.
    TODO: call this automatically by Scene? }
  UpdateCursor(Scene);
end;

procedure TGLWindowVRMLBrowser.SetShadowVolumesPossible(const Value: boolean);
begin
  if not Closed then
    raise Exception.Create('You can''t change ShadowVolumesPossible ' +
      'while the context is already initialized');
  FShadowVolumesPossible := Value;
  if ShadowVolumesPossible then
    StencilBufferBits := 8 else
    StencilBufferBits := 0;
end;

end.
