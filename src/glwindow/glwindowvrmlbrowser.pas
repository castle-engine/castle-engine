{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple VRML browser window (TGLWindowVRMLBrowser descending from TGLWindow). }
unit GLWindowVRMLBrowser;

interface

uses Classes, VectorMath, GLWindow, VRMLNodes, VRMLGLScene,
  KambiSceneManager;

type
  { Window with an OpenGL context, most comfortable to render 3D worlds
    with 2D controls above. Add your 3D stuff to the scene manager
    available in @link(SceneManager) property. Add your 2D stuff
    to the @link(TGLUIWindow.Controls) property (from ancestor TGLUIWindow).

    You can directly access the SceneManager and configure it however you like.

    You have comfortable @link(Load) method that simply loads a single 3D model
    to your world.

    If you're looking for analogous Lazarus component
    (that does basically the same, but can be placed on a Lazarus form)
    see TKamVRMLBrowser component. }
  TGLWindowVRMLBrowser = class(TGLUIWindow)
  private
    FSceneManager: TKamSceneManager;

    function GetShadowVolumes: boolean;
    function GetShadowVolumesDraw: boolean;
    function GetShadowVolumesPossible: boolean;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesDraw(const Value: boolean);
    procedure SetShadowVolumesPossible(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;

    { Load a single 3D model to your world
      (removing other models, and resetting the camera).

      This is nice for simple 3D model browsers, but usually for games you
      don't want to use this method --- it's more flexible to create TVRMLGLScene
      yourself, and add it to scene manager yourself, see engine examples like
      scene_manager_basic.lpr. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);

    function MainScene: TVRMLGLScene;
    property SceneManager: TKamSceneManager read FSceneManager;

    { Should we make shadow volumes possible.

      This can be changed only when the context is not initialized,
      that is only when the window is currently closed.
      Reason: to make shadows possible, we have to initialize gl context
      specially (with stencil buffer).

      Note that the shadows will not be actually rendered until you also
      set ShadowVolumes := true. }
    property ShadowVolumesPossible: boolean
      read GetShadowVolumesPossible write SetShadowVolumesPossible default false;

    { See TKamSceneManager.ShadowVolumes. }
    property ShadowVolumes: boolean
      read GetShadowVolumes write SetShadowVolumes default false;

    { See TKamSceneManager.ShadowVolumesDraw. }
    property ShadowVolumesDraw: boolean
      read GetShadowVolumesDraw write SetShadowVolumesDraw default false;
  end;

implementation

uses SysUtils, X3DLoad;

constructor TGLWindowVRMLBrowser.Create(AOwner: TComponent);
begin
  inherited;

  FSceneManager := TKamSceneManager.Create(Self);
  Controls.Add(SceneManager);
end;

procedure TGLWindowVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadVRML(SceneFileName, false), true);
end;

procedure TGLWindowVRMLBrowser.Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);
begin
  { destroy MainScene and Camera, we will recreate them }
  SceneManager.MainScene.Free;
  SceneManager.MainScene := nil;
  SceneManager.Items.Clear;
  SceneManager.Camera.Free;

  SceneManager.MainScene := TVRMLGLScene.Create(Self);
  SceneManager.MainScene.Load(ARootNode, OwnsRootNode);
  SceneManager.Items.Add(SceneManager.MainScene);

  { initialize octrees titles }
  MainScene.TriangleOctreeProgressTitle := 'Building triangle octree';
  MainScene.ShapeOctreeProgressTitle := 'Building Shape octree';

  { just to make our Camera always non-nil }
  SceneManager.Camera := SceneManager.CreateDefaultCamera(SceneManager);
end;

function TGLWindowVRMLBrowser.MainScene: TVRMLGLScene;
begin
  Result := SceneManager.MainScene;
end;

function TGLWindowVRMLBrowser.GetShadowVolumes: boolean;
begin
  Result := SceneManager.ShadowVolumes;
end;

procedure TGLWindowVRMLBrowser.SetShadowVolumes(const Value: boolean);
begin
  SceneManager.ShadowVolumes := Value;
end;

function TGLWindowVRMLBrowser.GetShadowVolumesDraw: boolean;
begin
  Result := SceneManager.ShadowVolumesDraw;
end;

procedure TGLWindowVRMLBrowser.SetShadowVolumesDraw(const Value: boolean);
begin
  SceneManager.ShadowVolumesDraw := Value;
end;

function TGLWindowVRMLBrowser.GetShadowVolumesPossible: boolean;
begin
  Result := SceneManager.ShadowVolumesPossible;
end;

procedure TGLWindowVRMLBrowser.SetShadowVolumesPossible(const Value: boolean);
begin
  if not Closed then
    raise Exception.Create('You can''t change ShadowVolumesPossible ' +
      'while the context is already initialized');
  SceneManager.ShadowVolumesPossible := Value;
  if SceneManager.ShadowVolumesPossible then
    StencilBufferBits := 8 else
    StencilBufferBits := 0;
end;

end.
