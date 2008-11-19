{
  Copyright 2008 Michalis Kamburelis.

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

{ TGLWindowVRMLBrowser class, simple VRML browser in a single
  TGLWindow window. }
unit GLWindowVRMLBrowser;

interface

uses VectorMath, GLWindow, VRMLNodes, VRMLGLScene, VRMLScene, Navigation,
  VRMLGLHeadLight, ShadowVolumes;

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
  TGLWindowVRMLBrowser = class(TGLWindowNavigated)
  private
    FScene: TVRMLGLScene;

    CameraRadius: Single;

    AngleOfViewX, AngleOfViewY: Single;

    function MoveAllowed(ANavigator: TWalkNavigator;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    procedure GetCameraHeight(ANavigator: TWalkNavigator;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    procedure ScenePostRedisplay(Scene: TVRMLScene);
    procedure MatrixChanged(ANavigator: TNavigator);
    procedure BoundViewpointChanged(Scene: TVRMLScene);
    procedure BoundViewpointVectorsChanged(Scene: TVRMLScene);
    procedure GeometryChanged(Scene: TVRMLScene);

    procedure UpdateCursor;

    FShadowVolumesPossible: boolean;
    procedure SetShadowVolumesPossible(const Value: boolean);
    FShadowVolumes: boolean;
    FShadowVolumesDraw: boolean;
    SV: TShadowVolumes;

    procedure RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
    procedure RenderShadowVolumes;
  public
    constructor Create;
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
  KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, KambiFilesUtils, VRMLOctreeItems,
  RaysWindow, BackgroundGL;

{ This uses OctreeCollisions, so either OctreeDynamicCollisions
  or OctreeCollidableTriangles, whichever is available. }

constructor TGLWindowVRMLBrowser.Create;
begin
  inherited;

  { we manage Navigator ourselves, this makes code more consequent to follow }
  OwnsNavigator := false;

  Load(nil, true);
end;

destructor TGLWindowVRMLBrowser.Destroy;
begin
  FreeAndNil(Scene);
  Navigator.Free;
  Navigator := nil;
  inherited;
end;

procedure TGLWindowVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadAsVRML(SceneFileName, false), true);
end;

procedure TGLWindowVRMLBrowser.Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);
begin
  FreeAndNil(Scene);
  Navigator.Free;
  Navigator := nil;

  FScene := TVRMLGLScene.Create(ARootNode, OwnsRootNode, roSeparateShapeStates);

  { initialize octrees titles }
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeStateOctreeProgressTitle := 'Building ShapeState octree';

  { init Navigator }
  Navigator := Scene.CreateNavigator(CameraRadius);
  Navigator.OnMatrixChanged := @MatrixChanged;

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

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  if not Closed then
  begin
    EventResize;
    PostRedisplay;
  end;
end;

procedure TGLWindowVRMLBrowser.EventBeforeDraw;
var
  Options: TPrepareRenderOptions;
  TG: TTransparentGroups;
begin
  Options := [prBackground, prBoundingBox];
  if ShadowVolumesPossible then
    Options := Options + prShadowVolume;

  TG := [tgAll];
  if ShadowVolumesPossible then
    TG := TG + [tgOpaque, tgTransparent];

  Scene.PrepareRender(TG, Options);

  inherited;
end;

procedure TGLWindowVRMLBrowser.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
begin
  if InShadow then
    Scene.RenderFrustum(Navigator.Frustum, TransparentGroup, @Scene.LightRenderInShadow) else
    Scene.RenderFrustum(Navigator.Frustum, TransparentGroup, nil);
end;

procedure TGLWindowVRMLBrowser.RenderShadowVolumes;
begin
  Scene.InitAndRenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure TGLWindowVRMLBrowser.EventDraw;

  procedure RenderNoShadows;
  begin
    RenderScene(false, tgAll);
  end;

  procedure RenderWithShadows(const MainLightPosition: TVector4Single);
  begin
    SV.InitFrustumAndLight(Navigator.Frustum, MainLightPosition);
    SV.Render(nil, @RenderScene, @RenderShadowVolumes, ShadowVolumesDraw);
  end;

var
  ClearBuffers: TGLbitfield;
  MainLightPosition: TVector4Single;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if Scene.Background <> nil then
  begin
    glLoadMatrix(Navigator.RotationOnlyMatrix);
    Scene.Background.Render;
  end else
    ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

  if ShadowVolumesPossible and ShadowVolumes then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  glClear(ClearBuffers);

  TVRMLGLHeadlight.RenderOrDisable(Scene.Headlight, 0);

  glLoadMatrix(Navigator.Matrix);
  if ShadowVolumesPossible and
     ShadowVolumes and
     Scene.MainLightForShadows(MainLightPosition) then
    RenderWithShadows(MainLightPosition) else
    RenderNoShadows;

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
  Scene.GLProjection(Navigator, Scene.BoundingBox, CameraRadius,
    Width, Height, AngleOfViewX, AngleOfViewY, ShadowVolumesPossible);
end;

procedure TGLWindowVRMLBrowser.EventMouseDown(Btn: TMouseButton);
begin
  inherited;
  if Btn = mbLeft then
    Scene.PointingDeviceActive := true;
end;

procedure TGLWindowVRMLBrowser.EventMouseUp(Btn: TMouseButton);
begin
  inherited;
  if Btn = mbLeft then
    Scene.PointingDeviceActive := false;
end;

procedure TGLWindowVRMLBrowser.UpdateCursor;

  function SensorsCount: Cardinal;
  begin
    if Scene.PointingDeviceSensors <> nil then
      Result := Scene.PointingDeviceSensors.Count else
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
var
  Ray0, RayVector: TVector3Single;
  OverPoint: TVector3Single;
  Item: POctreeItem;
begin
  inherited;

  if (Scene.OctreeCollisions <> nil) and
     (Navigator is TWalkNavigator) then
  begin
    Ray(NewX, NewY, AngleOfViewX, AngleOfViewY, Ray0, RayVector);

    Item := Scene.OctreeCollisions.RayCollision(
      OverPoint, Ray0, RayVector, true, nil, false, nil);

    Scene.PointingDeviceMove(OverPoint, Item);

    UpdateCursor;
  end;
end;

procedure TGLWindowVRMLBrowser.EventKeyDown(Key: TKey; C: char);
begin
  inherited;
  Scene.KeyDown(Key, C, @KeysDown);
end;

procedure TGLWindowVRMLBrowser.EventKeyUp(Key: TKey; C: char);
begin
  inherited;
  Scene.KeyUp(Key, C);
end;

function TGLWindowVRMLBrowser.MoveAllowed(ANavigator: TWalkNavigator;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if Scene.OctreeCollisions <> nil then
  begin
    Result := Scene.OctreeCollisions.MoveAllowed(
      ANavigator.CameraPos, ProposedNewPos, NewPos, CameraRadius,
      { Don't let user to fall outside of the box because of gravity. }
      BecauseOfGravity);
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

procedure TGLWindowVRMLBrowser.GetCameraHeight(ANavigator: TWalkNavigator;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItem: POctreeItem;
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

procedure TGLWindowVRMLBrowser.MatrixChanged(ANavigator: TNavigator);
begin
  { Navigator.OnMatrixChanged callback is initialized in constructor
    before Scene is initialized. So to be on the safest side, we check
    here Scene <> nil. }

  if (Scene <> nil) and
     (Navigator is TWalkNavigator) then
    Scene.ViewerPositionChanged(WalkNav.CameraPos);
  PostRedisplay;
end;

procedure TGLWindowVRMLBrowser.BoundViewpointChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, CameraRadius, false);
end;

procedure TGLWindowVRMLBrowser.BoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, CameraRadius, true);
end;

procedure TGLWindowVRMLBrowser.GeometryChanged(Scene: TVRMLScene);
begin
  { Scene.GeometryChanged possibly cleared pointing device info by
    PointingDeviceClear. This means that cursor must be updated. }
  UpdateCursor;
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
