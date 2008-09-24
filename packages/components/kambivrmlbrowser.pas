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

{ TKamVRMLBrowser component, simple VRML browser in a Lazarus component. }
unit KambiVRMLBrowser;

{ This is the weak point of our engine right now: updating octree on changes
  of geometry. It takes some time, as we currently just rebuild the octree.
  On the other hand, it's needed to perform collision detection
  (this also includes picking touch sensors and such) on the up-to-date
  model (in case e.g. touch sensor geometry moves). In some rare
  cases (more precisely: when TVRMLScene.ChangedFields is not optimized
  for this particular field and falls back to TVRMLScene.ChangedAll)
  not updating octree may even cause the octree pointers to be invalid.

  Both problems are being worked on. In the meantime
  - you can define REBUILD_OCTREE to have always accurate and stable
    collision detection, at the expense of a slowdowns in case of intensive
    animations.
  - you can undefine REBUILD_OCTREE to always use the first octree.
    This makes collision detection work with the original geometry,
    and sometimes may make it unstable. OTOH, all works fast. }
{ $define REBUILD_OCTREE}

interface

uses Classes, KambiGLControl, VectorMath,
  VRMLNodes, VRMLGLScene, VRMLScene, MatrixNavigation;

type
  { A simple VRML browser as a Lazarus component. This manages TVRMLGLScene,
    navigator (only Walk navigation) and octrees.
    You simply call @link(Load) method and all is done.

    This is very simple to use, but note that for more advanced uses
    you're not really expected to extend this class. Instead, you can
    implement something more suitable for you using your own
    TVRMLGLScene, navigator and octrees management.
    In other words, remember that this class just provides
    a simple "glue" between the key components of our engine.
    For specialized cases, more complex scenarios may be needed.

    If you're looking for GLWindow descendants that does basically the same
    (easy VRML browser), you want to check out TGLWindowVRMLBrowser
    (file @code(../../3dmodels.gl/glwindowvrmlbrowser.pas)). }
  TKamVRMLBrowser = class(TKamOpenGLControl)
  private
    FScene: TVRMLGLScene;

    { CameraRadius is needed for collision detection }
    CameraRadius: Single;

    function AngleOfViewX: Single;

    function MoveAllowed(ANavigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    procedure ScenePostRedisplay(Scene: TVRMLScene);
    procedure MatrixChanged(ANavigator: TMatrixNavigator);
    procedure BoundViewpointVectorsChanged(Scene: TVRMLScene);
    procedure GeometryChanged(Scene: TVRMLScene);
  protected
    procedure Paint; override;
    procedure DoGLContextInit; override;
    procedure DoGLContextClose; override;

    {
    procedure EventIdle; override;
    procedure EventMouseDown(Btn: TMouseButton); override;
    procedure EventMouseUp(Btn: TMouseButton); override;
    procedure EventMouseMove(NewX, NewY: Integer); override;
    procedure EventKeyDown(Key: TKey; C: char); override;
    procedure EventKeyUp(Key: TKey); override;}
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;

    { Creates new @link(Scene), with new navigator and such. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);

    property Scene: TVRMLGLScene read FScene;

    procedure Resize; override;
  end;

procedure Register;

implementation

uses Boxes3d, VRMLOpenGLRenderer, GL, GLU,
  KambiClassUtils, KambiUtils, SysUtils, Object3dAsVRML,
  KambiGLUtils, KambiFilesUtils, VRMLTriangleOctree,
  RaysWindow, BackgroundGL;

procedure Register;
begin
  RegisterComponents('Kambi',[TKamVRMLBrowser]);
end;

{ TKamVRMLBrowser ----------------------------------------------------------- }

constructor TKamVRMLBrowser.Create(AOwner :TComponent);
begin
  inherited;

  Navigator := TMatrixWalker.Create(@MatrixChanged);
  OwnsNavigator := true;

  Load(nil, true);
end;

destructor TKamVRMLBrowser.Destroy;
begin
  FreeAndNil(Scene);
  inherited;
end;

procedure TKamVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadAsVRML(SceneFileName, false), true);
end;

procedure TKamVRMLBrowser.Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);
var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
begin
  FreeAndNil(Scene);

  FScene := TVRMLGLScene.Create(ARootNode, OwnsRootNode, roSeparateShapeStates);

  { build octrees }
  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Building triangle octree');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Building ShapeState octree');

  Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, GravityUp);

  { init Navigator }
  NavWalker.Init(CamPos,
    VectorAdjustToLength(CamDir, Box3dAvgSize(Scene.BoundingBox, 1.0) * 0.01 * 0.4),
    CamUp, GravityUp,
    0.0, 0.0 { unused, we don't use Gravity here });

  { init collision detection }
  CameraRadius := Box3dAvgSize(Scene.BoundingBox, 1.0) * 0.01;
  NavWalker.OnMoveAllowed := @MoveAllowed;

  { prepare for events procesing (although we let the decision whether
    to turn ProcessEvent := true to the caller). }
  Scene.ResetWorldTimeAtLoad;
  Scene.OnPostRedisplay := @ScenePostRedisplay;
  Scene.OnBoundViewpointVectorsChanged := @BoundViewpointVectorsChanged;
  Scene.OnGeometryChanged := @GeometryChanged;

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;
end;

{ TODO: nowhere to put this for TKamVRMLBrowser. Needed?
procedure TKamVRMLBrowser.EventBeforeDraw;
begin
  inherited;
  Scene.PrepareRender([tgAll], [prBackground, prBoundingBox]);
end;
}

procedure TKamVRMLBrowser.Paint;
begin
  inherited;

  if Scene.Background <> nil then
  begin
    glLoadMatrix(Navigator.RotationOnlyMatrix);
    Scene.Background.Render;
    glClear(GL_DEPTH_BUFFER_BIT);
  end else
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadMatrix(Navigator.Matrix);
  Scene.RenderFrustumOctree(NavWalker.Frustum, tgAll);

  SwapBuffers;
end;

procedure TKamVRMLBrowser.DoGLContextInit;
begin
  inherited;
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  { Manually call Resize now, to set projection. }
  Resize;
end;

procedure TKamVRMLBrowser.DoGLContextClose;
begin
  Scene.CloseGL;
  inherited;
end;

{TODO:
procedure TKamVRMLBrowser.EventIdle;
begin
  inherited;
  Scene.IncreaseWorldTime(IdleSpeed);
end;
}

const
  AngleOfViewY = 45.0;

function TKamVRMLBrowser.AngleOfViewX: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(
    AngleOfViewY, Width / Height);
end;

procedure TKamVRMLBrowser.Resize;

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    NavWalker.ProjectionMatrix := ProjectionMatrix;
  end;

var
  WalkProjectionNear, WalkProjectionFar: Single;
begin
  inherited;
  if not MakeCurrent then Exit;

  glViewport(0, 0, Width, Height);

  WalkProjectionNear := CameraRadius * 0.6;
  WalkProjectionFar := Box3dMaxSize(Scene.BoundingBox, 1.0) * 3.0;

  Scene.BackgroundSkySphereRadius :=
    TBackgroundGL.NearFarToSkySphereRadius(
      WalkProjectionNear, WalkProjectionFar);

  ProjectionGLPerspective(AngleOfViewY, Width / Height,
    WalkProjectionNear, WalkProjectionFar);

  UpdateNavigatorProjectionMatrix;
end;

(*TODO:
procedure TKamVRMLBrowser.EventMouseDown(Btn: TMouseButton);
begin
  inherited;
  if Btn = mbLeft then
    Scene.PointingDeviceActive := true;
end;

procedure TKamVRMLBrowser.EventMouseUp(Btn: TMouseButton);
begin
  inherited;
  if Btn = mbLeft then
    Scene.PointingDeviceActive := false;
end;

procedure TKamVRMLBrowser.EventMouseMove(NewX, NewY: Integer);
var
  Ray0, RayVector: TVector3Single;
  OverPoint: TVector3Single;
  Item: POctreeItem;
  ItemIndex: Integer;

  function SensorsCount: Cardinal;
  begin
    if Scene.PointingDeviceSensors <> nil then
      Result := Scene.PointingDeviceSensors.Count else
      Result := 0;
    if Scene.PointingDeviceActiveSensor <> nil then
      Inc(Result);
  end;

begin
  inherited;

  if Scene.DefaultTriangleOctree <> nil then
  begin
    Ray(NewX, NewY, AngleOfViewX, AngleOfViewY, Ray0, RayVector);

    ItemIndex := Scene.DefaultTriangleOctree.RayCollision(
      OverPoint, Ray0, RayVector, true, NoItemIndex, false, nil);

    if ItemIndex = NoItemIndex then
      Item := nil else
      Item := Scene.DefaultTriangleOctree.OctreeItems.Pointers[ItemIndex];

    Scene.PointingDeviceMove(OverPoint, Item);

    { I want to keep assertion that CursorNonMouseLook = gcHand when
      we're over or keeping active some pointing-device sensors. }
    if SensorsCount <> 0 then
      CursorNonMouseLook := gcHand else
      CursorNonMouseLook := gcDefault;
  end;
end;

procedure TKamVRMLBrowser.EventKeyDown(Key: TKey; C: char);
begin
  inherited;
  Scene.KeyDown(Key, C, @KeysDown);
end;

procedure TKamVRMLBrowser.EventKeyUp(Key: TKey);
begin
  inherited;
  Scene.KeyUp(Key);
end;
*)

function TKamVRMLBrowser.MoveAllowed(ANavigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Scene.DefaultTriangleOctree.MoveAllowed(
    ANavigator.CameraPos, ProposedNewPos, NewPos, CameraRadius,
    NoItemIndex, nil);
end;

procedure TKamVRMLBrowser.ScenePostRedisplay(Scene: TVRMLScene);
begin
  Invalidate;
end;

procedure TKamVRMLBrowser.MatrixChanged(ANavigator: TMatrixNavigator);
begin
  { Navigator.OnMatrixChanged callback is initialized in constructor
    before Scene is initialized. So to be on the safest side, we check
    here Scene <> nil. }

  if Scene <> nil then
    Scene.ViewerPositionChanged(NavWalker.CameraPos);
  Invalidate;
end;

procedure TKamVRMLBrowser.BoundViewpointVectorsChanged(Scene: TVRMLScene);
var
  CameraPos: TVector3Single;
  CameraDir: TVector3Single;
  CameraUp: TVector3Single;
  GravityUp: TVector3Single;
begin
  TVRMLViewpointNode(Scene.ViewpointStack.Top).GetCameraVectors(
    CameraPos, CameraDir, CameraUp, GravityUp);
  NavWalker.SetInitialCameraLookDir(CameraPos, CameraDir, CameraUp, true);
end;

procedure TKamVRMLBrowser.GeometryChanged(Scene: TVRMLScene);
begin
{$ifdef REBUILD_OCTREE}
  Scene.PointingDeviceClear;
  CursorNonMouseLook := gcDefault;

  Scene.DefaultTriangleOctree.Free;
  Scene.DefaultShapeStateOctree.Free;

  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Building triangle octree');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Building ShapeState octree');
{$endif REBUILD_OCTREE}
end;

end.
