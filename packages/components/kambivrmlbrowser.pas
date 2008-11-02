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

interface

uses Classes, KambiGLControl, VectorMath, Controls,
  VRMLNodes, VRMLGLScene, VRMLScene, Navigation, VRMLGLHeadlight;

type
  { A simple VRML browser as a Lazarus component. This manages TVRMLGLScene,
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

      @item(You can change @link(TVRMLGLScene.Optimization
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

    If you're looking for GLWindow descendants that does basically the same
    (easy VRML browser), you want to check out TGLWindowVRMLBrowser
    (file @code(../../3dmodels.gl/glwindowvrmlbrowser.pas)). }
  TKamVRMLBrowser = class(TKamOpenGLControl)
  private
    FOnNavigatorChanged: TNavigatorNotifyFunc;
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
  protected
    procedure DoBeforeDraw; override;
    procedure DoDraw; override;
    procedure DoGLContextInit; override;
    procedure DoGLContextClose; override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;

    { Creates new @link(Scene), with new navigator and such. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);

    property Scene: TVRMLGLScene read FScene;

    procedure Resize; override;
    procedure Idle; override;
  published
    property OnNavigatorChanged: TNavigatorNotifyFunc
      read FOnNavigatorChanged write FOnNavigatorChanged;
  end;

procedure Register;

implementation

{ The implementation is very very similar to GLWindowVRMLBrowser implementation.
  In fact, some code parts are even identical... A work to move more
  logic to TVRMLScene (which could be used then by both this unit and
  GLWindowVRMLBrowser, without any code duplication) is ongoing. }

{ TODO: this could use either OctreeCollisions or OctreeCollidableTriangles,
  whichever is available. }

uses Boxes3d, VRMLOpenGLRenderer, GL, GLU,
  KambiClassUtils, KambiUtils, SysUtils, Object3dAsVRML,
  KambiGLUtils, KambiFilesUtils, VRMLOctreeItems,
  RaysWindow, BackgroundGL, Keys;

procedure Register;
begin
  RegisterComponents('Kambi',[TKamVRMLBrowser]);
end;

{ TKamVRMLBrowser ----------------------------------------------------------- }

constructor TKamVRMLBrowser.Create(AOwner :TComponent);
begin
  inherited;

  { we manage Navigator ourselves, this makes code more consequent to follow }
  OwnsNavigator := false;

  Load(nil, true);
end;

destructor TKamVRMLBrowser.Destroy;
begin
  FreeAndNil(Scene);
  Navigator.Free;
  Navigator := nil;
  inherited;
end;

procedure TKamVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadAsVRML(SceneFileName, false), true);
end;

procedure TKamVRMLBrowser.Load(ARootNode: TVRMLNode; const OwnsRootNode: boolean);
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

  if ContextInitialized then
  begin
    Resize;
    Invalidate;
  end;
end;

procedure TKamVRMLBrowser.DoBeforeDraw;
begin
  Scene.PrepareRender([tgAll], [prBackground, prBoundingBox]);
  inherited;
end;

procedure TKamVRMLBrowser.DoDraw;
begin
  if Scene.Background <> nil then
  begin
    glLoadMatrix(Navigator.RotationOnlyMatrix);
    Scene.Background.Render;
    glClear(GL_DEPTH_BUFFER_BIT);
  end else
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  TVRMLGLHeadlight.RenderOrDisable(Scene.Headlight, 0);

  glLoadMatrix(Navigator.Matrix);
  if Navigator is TWalkNavigator then
    Scene.RenderFrustum(WalkNav.Frustum, tgAll) else
    Scene.Render(nil, tgAll);

  inherited;
end;

procedure TKamVRMLBrowser.DoGLContextInit;
begin
  inherited;
  glEnable(GL_LIGHTING);
  { Manually call Resize now, to set projection. }
  Resize;
end;

procedure TKamVRMLBrowser.DoGLContextClose;
begin
  { This may be called by inherited destructor, when our own destructor
    already destroyed Scene. (Testcase: open form with this component
    in Lazarus, then close Lazarus). So we secure against it. }
  if Scene <> nil then
    Scene.CloseGL;

  inherited;
end;

procedure TKamVRMLBrowser.Idle;
begin
  inherited;
  Scene.IncreaseWorldTime(Fps.IdleSpeed);
end;

procedure TKamVRMLBrowser.Resize;
begin
  inherited;
  if not MakeCurrent then Exit;
  Scene.GLProjection(Navigator, Scene.BoundingBox, CameraRadius,
    Width, Height, AngleOfViewX, AngleOfViewY);
end;

const
  { Cannot access Controls.mbLeft within TKamVRMLBrowser, as we have there
    method named "Controls". }
  ContolsMBLeft = Controls.mbLeft;

procedure TKamVRMLBrowser.MouseDown(Button: Controls.TMouseButton;
  Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  if Button = ContolsMBLeft then
    Scene.PointingDeviceActive := true;
end;

procedure TKamVRMLBrowser.MouseUp(Button: Controls.TMouseButton;
  Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  if Button = ContolsMBLeft then
    Scene.PointingDeviceActive := false;
end;

procedure TKamVRMLBrowser.UpdateCursor;

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
    we're over or keeping active some pointing-device sensors.
    (Since we don't use MouseLook with TKamGLControl for now, I just
    directly change Cursor. }
  if SensorsCount <> 0 then
    Cursor := crHandPoint else
    Cursor := crDefault;
end;

procedure TKamVRMLBrowser.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
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

procedure TKamVRMLBrowser.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  inherited;

  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);

  if (MyKey <> K_None) or (MyCharKey <> #0) then
    Scene.KeyDown(MyKey, MyCharKey, @KeysDown);
end;

procedure TKamVRMLBrowser.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  inherited;

  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);

  if (MyKey <> K_None) or (MyCharKey <> #0) then
    Scene.KeyUp(Key, MyCharKey);
end;

function TKamVRMLBrowser.MoveAllowed(ANavigator: TWalkNavigator;
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

procedure TKamVRMLBrowser.GetCameraHeight(ANavigator: TWalkNavigator;
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

procedure TKamVRMLBrowser.ScenePostRedisplay(Scene: TVRMLScene);
begin
  Invalidate;
end;

procedure TKamVRMLBrowser.MatrixChanged(ANavigator: TNavigator);
begin
  { Navigator.OnMatrixChanged callback is initialized in constructor
    before Scene is initialized. So to be on the safest side, we check
    here Scene <> nil. }

  if (Scene <> nil) and
     (Navigator is TWalkNavigator) then
    Scene.ViewerPositionChanged(WalkNav.CameraPos);
  Invalidate;

  if Assigned(OnNavigatorChanged) then
    OnNavigatorChanged(ANavigator);
end;

procedure TKamVRMLBrowser.BoundViewpointChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, CameraRadius, false);
end;

procedure TKamVRMLBrowser.BoundViewpointVectorsChanged(Scene: TVRMLScene);
begin
  Scene.NavigatorBindToViewpoint(Navigator, CameraRadius, true);
end;

procedure TKamVRMLBrowser.GeometryChanged(Scene: TVRMLScene);
begin
  { Scene.GeometryChanged possibly cleared pointing device info by
    PointingDeviceClear. This means that cursor must be updated. }
  UpdateCursor;
end;

end.
