{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple VRML browser as a Lazarus component (TKamVRMLBrowser). }
unit KambiVRMLBrowser;

interface

uses Classes, KambiGLControl, VectorMath, Controls, Cameras,
  VRMLNodes, VRMLGLScene, KambiSceneManager;

type
  { A simple VRML browser as a Lazarus component. This creates the scene manager
    for you, and gives you an ultra-simple @link(Load) method to
    load 3D model from file.

    You can directly access the SceneManager. You can also directly
    access the loaded scene: this is available in @link(Scene) property,
    it's always the same thing as SceneManager.MainScene.

    If you're looking for GLWindow descendants that does basically the same
    (easy VRML browser), you want to check out TGLWindowVRMLBrowser
    (file @code(../glwindow/glwindowvrmlbrowser.pas)). }
  TKamVRMLBrowser = class(TKamOpenGLControl)
  private
    FSceneManager: TKamSceneManager;

    function GetShadowVolumes: boolean;
    function GetShadowVolumesDraw: boolean;
    function GetShadowVolumesPossible: boolean;
    function GetOnCameraChanged: TNotifyEvent;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesDraw(const Value: boolean);
    procedure SetShadowVolumesPossible(const Value: boolean);
    procedure SetOnCameraChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner :TComponent); override;

    { Creates new @link(Scene), with new camera and such. }
    procedure Load(const SceneFileName: string);
    procedure Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);

    function Scene: TVRMLGLScene;
    function Camera: TCamera;
  published
    property SceneManager: TKamSceneManager read FSceneManager;

    property OnCameraChanged: TNotifyEvent
      read GetOnCameraChanged write SetOnCameraChanged;

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

procedure Register;

implementation

uses SysUtils, X3DLoad;

procedure Register;
begin
  RegisterComponents('Kambi',[TKamVRMLBrowser]);
end;

{ TKamVRMLBrowser ----------------------------------------------------------- }

constructor TKamVRMLBrowser.Create(AOwner :TComponent);
begin
  inherited;

  FSceneManager := TKamSceneManager.Create(Self);
  Controls.Add(SceneManager);
end;

procedure TKamVRMLBrowser.Load(const SceneFileName: string);
begin
  Load(LoadVRML(SceneFileName, false), true);
end;

procedure TKamVRMLBrowser.Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);
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
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';

  { just to make our Camera always non-nil }
  SceneManager.Camera := SceneManager.CreateDefaultCamera(SceneManager);
end;

function TKamVRMLBrowser.Scene: TVRMLGLScene;
begin
  Result := SceneManager.MainScene;
end;

function TKamVRMLBrowser.Camera: TCamera;
begin
  Result := SceneManager.Camera;
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
  if GLInitialized then
    raise Exception.Create('You can''t change ShadowVolumesPossible ' +
      'while the context is already initialized');
  SceneManager.ShadowVolumesPossible := Value;

  { TODO: hmm, there's no way to request stencil buffer from TOpenGLControl?
    Looking in the sources, for GTK always at least 1-bit stencil buffer is
    requested (pretty useless?), for Windows nothing is requested.
    There's no way to workaround it, this must be fixed in OpenGLContext
    for TKamVRMLBrowser to work reliably. }

{  if SceneManager.ShadowVolumesPossible then
    StencilBufferBits := 8 else
    StencilBufferBits := 0;}
end;

function TKamVRMLBrowser.GetOnCameraChanged: TNotifyEvent;
begin
  Result := SceneManager.OnCameraChanged;
end;

procedure TKamVRMLBrowser.SetOnCameraChanged(const Value: TNotifyEvent);
begin
  SceneManager.OnCameraChanged := Value;
end;

end.
