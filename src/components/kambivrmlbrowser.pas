{
  Copyright 2008-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TKamVRMLBrowser component, simple VRML browser in a Lazarus component. }
unit KambiVRMLBrowser;

interface

uses Classes, KambiGLControl, VectorMath, Controls,
  VRMLNodes, VRMLGLScene, VRMLScene, Cameras, VRMLGLHeadlight, Areas,
  SceneManagerUnit;

type
  { A simple VRML browser as a Lazarus component. This manages TVRMLGLScene,
    camera (automatically adjusted to NavigationInfo.type).
    Octress are also automatically used (you only have to set Scene.Spatial
    to anything <> [], like typical [ssRendering, ssDynamicCollisions]).
    You simply call @link(Load) method and all is done.

    This class tries to be a thin (not really "opaque")
    wrapper around Scene / Camera objects. Which means that
    you can access many functionality by directly accessing
    Scene or Camera objects methods/properties.
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

      @item(You can change @link(TVRMLGLScene.Optimization
        Scene.Optimization). You can also change rendering attributes
        by @link(TVRMLGLScene.Attributes Scene.Attributes).)
    )

    Some important things that you @italic(cannot) mess with:

    @unorderedList(
      @item(Don't create/free Scene, Camera and such objects yourself.
        This class manages them, they are always non-nil.)
    )

    This is very simple to use, but note that for more advanced uses
    you're not really expected to extend this class. Instead, you can
    implement something more suitable for you using your own
    TVRMLGLScene and camera management.
    In other words, remember that this class just provides
    a simple "glue" between the key components of our engine.
    For specialized cases, more complex scenarios may be needed.

    If you're looking for GLWindow descendants that does basically the same
    (easy VRML browser), you want to check out TGLWindowVRMLBrowser
    (file @code(../glwindow/glwindowvrmlbrowser.pas)). }
  TKamVRMLBrowser = class(TKamOpenGLControl)
  private
    SceneManager: TSceneManager;
    FOnCameraChanged: TNotifyEvent;

    function MoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    procedure GetCameraHeight(ACamera: TWalkCamera;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    procedure CameraVisibleChange(ACamera: TObject);
    procedure BoundViewpointChanged(Scene: TVRMLScene);
    procedure BoundViewpointVectorsChanged(Scene: TVRMLScene);

    procedure UpdateCursor(Sender: TObject);

    function GetShadowVolumes: boolean;
    function GetShadowVolumesDraw: boolean;
    function GetShadowVolumesPossible: boolean;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesDraw(const Value: boolean);
    procedure SetShadowVolumesPossible(const Value: boolean);
  protected
    procedure DoBeforeDraw; override;
    procedure DoDraw; override;
    procedure DoGLContextInit; override;
  public
    constructor Create(AOwner :TComponent); override;

    { Creates new @link(Scene), with new camera and such. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);

    function Scene: TVRMLGLScene;

    procedure Resize; override;
  published
    property OnCameraChanged: TNotifyEvent
      read FOnCameraChanged write FOnCameraChanged;

    { Should we make shadow volumes possible.

      This can be changed only when the context is not initialized,
      that is only when the window is currently closed.
      Reason: to make shadows possible, we have to initialize gl context
      specially (with stencil buffer).

      Note that the shadows will not be actually rendered until you also
      set ShadowVolumes := true. }
    property ShadowVolumesPossible: boolean
      read GetShadowVolumesPossible write SetShadowVolumesPossible default false;

    { See TSceneManager.ShadowVolumes. }
    property ShadowVolumes: boolean
      read GetShadowVolumes write SetShadowVolumes default false;

    { See TSceneManager.ShadowVolumesDraw. }
    property ShadowVolumesDraw: boolean
      read GetShadowVolumesDraw write SetShadowVolumesDraw default false;
  end;

procedure Register;

implementation

{ The implementation is very very similar to GLWindowVRMLBrowser implementation.
  In fact, some code parts are even identical... A work to move more
  logic to TVRMLScene (which could be used then by both this unit and
  GLWindowVRMLBrowser, without any code duplication) is ongoing. }

{ This uses OctreeCollisions, so either OctreeDynamicCollisions
  or OctreeCollidableTriangles, whichever is available. }

uses Boxes3d, VRMLOpenGLRenderer, GL, GLU,
  KambiClassUtils, KambiUtils, SysUtils, Object3dAsVRML,
  KambiGLUtils, KambiFilesUtils, VRMLTriangle,
  RaysWindow, BackgroundGL, KeysMouse;

procedure Register;
begin
  RegisterComponents('Kambi',[TKamVRMLBrowser]);
end;

{ TKamVRMLBrowser ----------------------------------------------------------- }

constructor TKamVRMLBrowser.Create(AOwner :TComponent);
begin
  inherited;

  SceneManager := TSceneManager.Create(Self);
  Controls.Add(SceneManager);

  Load(nil, true);
end;

procedure TKamVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadAsVRML(SceneFileName, false), true);
end;

procedure TKamVRMLBrowser.Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);
begin
  { destroy Scene and Camera, we will recreate them }
  SceneManager.Scene.Free;
  SceneManager.Scene := nil;
  Camera.Free;

  SceneManager.Scene := TVRMLGLScene.Create(Self);
  SceneManager.Scene.Load(ARootNode, OwnsRootNode);

  { initialize octrees titles }
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';

  { init Camera }
  Camera := Scene.CreateCamera(Self);
  Camera.OnVisibleChange := @CameraVisibleChange;
  Scene.Camera := Camera;
  SceneManager.Camera := Camera;

  if Camera is TWalkCamera then
  begin
    (Camera as TWalkCamera).OnMoveAllowed := @MoveAllowed;
    (Camera as TWalkCamera).OnGetCameraHeight := @GetCameraHeight;
  end;

  { prepare for events procesing (although we let the decision whether
    to turn ProcessEvent := true to the caller). }
  Scene.ResetWorldTimeAtLoad;
  Scene.OnBoundViewpointVectorsChanged := @BoundViewpointVectorsChanged;
  Scene.ViewpointStack.OnBoundChanged := @BoundViewpointChanged;
  Scene.OnPointingDeviceSensorsChange := @UpdateCursor;

  { Call initial ViewerChanged (this allows ProximitySensors to work
    as soon as ProcessEvent becomes true). }
  Scene.ViewerChanged(Camera, SceneManager.ViewerToChanges);

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  { Add Scene to Controls, making it receive all TUIControl treatment,
    like events etc. }
  Controls.Insert(0, Scene);

  if ContextInitialized then
  begin
    Resize;
    Invalidate;
  end;
end;

function TKamVRMLBrowser.Scene: TVRMLGLScene;
begin
  Result := SceneManager.Scene;
end;

procedure TKamVRMLBrowser.DoBeforeDraw;
begin
  SceneManager.PrepareRender;
  inherited;
end;

procedure TKamVRMLBrowser.DoDraw;
begin
  SceneManager.Render;
  inherited;
end;

procedure TKamVRMLBrowser.DoGLContextInit;
begin
  inherited;
  glEnable(GL_LIGHTING);
end;

procedure TKamVRMLBrowser.Resize;
begin
  inherited;
  if not MakeCurrent then Exit;
  Scene.GLProjection(Camera, Scene.BoundingBox,
    Width, Height, ShadowVolumesPossible);
end;

procedure TKamVRMLBrowser.UpdateCursor(Sender: TObject);

  function SensorsCount: Cardinal;
  begin
    if Scene.PointingDeviceSensors <> nil then
      Result := Scene.PointingDeviceSensors.EnabledCount else
      Result := 0;
    if Scene.PointingDeviceActiveSensor <> nil then
      Inc(Result);
  end;

begin
  { I want to keep assertion that CursorNonMouseLook = crHandPoint when
    we're over or keeping active some pointing-device sensors. }
  if SensorsCount <> 0 then
    CursorNonMouseLook := crHandPoint else
    CursorNonMouseLook := crDefault;
end;

function TKamVRMLBrowser.MoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if Scene.OctreeCollisions <> nil then
  begin
    Result := Scene.OctreeCollisions.MoveAllowed(
      ACamera.Position, ProposedNewPos, NewPos, ACamera.CameraRadius);
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;

  { Don't let user to fall outside of the box because of gravity. }
  if Result and BecauseOfGravity then
    Result := SimpleKeepAboveMinPlane(NewPos, Scene.BoundingBox,
      ACamera.GravityUp);
end;

procedure TKamVRMLBrowser.GetCameraHeight(ACamera: TWalkCamera;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItem: PVRMLTriangle;
begin
  if Scene.OctreeCollisions <> nil then
  begin
    Scene.OctreeCollisions.GetCameraHeight(
      ACamera.Position,
      ACamera.GravityUp,
      IsAboveTheGround, SqrHeightAboveTheGround, GroundItem,
      nil, nil);
  end else
  begin
    { When octree is not available, we actually don't want gravity to
      cause falling down. So return values pretending we're standing
      still on the ground. }
    IsAboveTheGround := true;
    SqrHeightAboveTheGround := Sqr(ACamera.CameraPreferredHeight);
  end;
end;

procedure TKamVRMLBrowser.CameraVisibleChange(ACamera: TObject);
begin
  { Camera.OnVisibleChange callback is initialized in constructor
    before Scene is initialized. So to be on the safest side, we check
    here Scene <> nil. }

  if Scene <> nil then
  begin
    Scene.ViewerChanged(Camera, SceneManager.ViewerToChanges);
  end;

  if Assigned(OnCameraChanged) then
    OnCameraChanged(ACamera);
end;

procedure TKamVRMLBrowser.BoundViewpointChanged(Scene: TVRMLScene);
begin
  Scene.CameraBindToViewpoint(Camera, false);
end;

procedure TKamVRMLBrowser.BoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  Scene.CameraBindToViewpoint(Camera, true);
end;

function TKamVRMLBrowser.GetShadowVolumes: boolean;
begin
  Result := SceneManager.ShadowVolumes;
end;

procedure TKamVRMLBrowser.SetShadowVolumes(const Value: boolean);
begin
  SceneManager.ShadowVolumes := Value;
end;

function TKamVRMLBrowser.GetShadowVolumesDraw: boolean;
begin
  Result := SceneManager.ShadowVolumesDraw;
end;

procedure TKamVRMLBrowser.SetShadowVolumesDraw(const Value: boolean);
begin
  SceneManager.ShadowVolumesDraw := Value;
end;

function TKamVRMLBrowser.GetShadowVolumesPossible: boolean;
begin
  Result := SceneManager.ShadowVolumesPossible;
end;

procedure TKamVRMLBrowser.SetShadowVolumesPossible(const Value: boolean);
begin
  if ContextInitialized then
    raise Exception.Create('You can''t change ShadowVolumesPossible ' +
      'while the context is already initialized');
  SceneManager.ShadowVolumesPossible := Value;

  { TODO: hmm, there's no way to request stencil buffer from TOpenGLControl?
    Looking in the sources, for GTK always at least 1-bit stencil buffer is
    requested (pretty stupid and worthless?), for Windows nothing is requested.
    There's no way to workaround it, this must be fixed in OpenGLContext
    for TKamVRMLBrowser to work reliably. }

{  if SceneManager.ShadowVolumesPossible then
    StencilBufferBits := 8 else
    StencilBufferBits := 0;}
end;

end.
