{
  Copyright 2008-2012 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftPlay;

interface

procedure Play;

implementation

uses GL, CastleGLUtils, CastleWindow, CastleStringUtils, VectorMath, CastleFilesUtils,
  WindowModes, Cameras, SysUtils, GLExt, X3DNodes,
  RaysWindow, Math, UIControls, GLRenderer,
  RiftVideoOptions, RiftLocations, RiftCreatures, RiftWindow, RiftGame,
  RiftNotifications, RenderingCameraUnit, Base3D,
  RiftSceneManager;

var
  Player: TPlayer;
  UserQuit: boolean;
  { 0 = 3D Scene goes only to depth buffer, this is what user should see
    1 = debug, both scene and image are displayed and blended
    2 = debug, only 3D Scene is displayed }
  DebugScene3DDisplay: Integer = 0;
  AngleOfViewX, AngleOfViewY: Single;
  SceneCamera: TWalkCamera;

{ TGameSceneManager ---------------------------------------------------------- }

type
  TGameSceneManager = class(TRiftSceneManager)
    procedure ApplyProjection; override;
    procedure Render3D(const Params: TRenderParams); override;
    procedure RenderShadowVolume; override;
    function MainLightForShadows(out AMainLightPosition: TVector4Single): boolean; override;
  end;

procedure TGameSceneManager.Render3D(const Params: TRenderParams);
var
  H: TLightInstance;
begin
  { This whole rendering is considered as opaque
    (CurrentLocation is rendered only to depth buffer,
    and image is always opaque;

    TODO: player model should actually be splitted into opaque/trasparent
    parts here. Doesn't matter for now, as current player is fully
    opaque.

    TODO: we should do something better with ShadowVolumesReceivers here?
    Make Player not receiver? }
  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    { If DebugScene3DDisplay = 0, render scene only to depth buffer. }
    if DebugScene3DDisplay = 0 then
      glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

    CurrentLocation.Scene.Render(nil, Params);

    if DebugScene3DDisplay = 0 then
      glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

    SetWindowPosZero;

    if DebugScene3DDisplay <> 2 then
    begin
      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        if DebugScene3DDisplay = 1 then
        begin
          { GL_CONSTANT_ALPHA is available as part of ARB_imaging, since GL 1.4
            as standard. glBlendColor is available since 1.2 as standard. }
          if (GL_version_1_2 and GL_ARB_imaging) or GL_version_1_4 then
          begin
            glBlendColor(0, 0, 0, 0.5);
            glBlendFunc(GL_CONSTANT_ALPHA, GL_ONE_MINUS_CONSTANT_ALPHA);
            glEnable(GL_BLEND);
          end;
        end;
        if Params.InShadow then
          glCallList(CurrentLocation.GLList_ShadowedImage) else
          glCallList(CurrentLocation.GLList_Image);
      glPopAttrib;
    end;

    if not DebugNoCreatures then
    begin
      if Params.InShadow and HeadlightInstance(H) then
      begin
        H.Node.FdAmbientIntensity.Send(1);
        H.Node.FdColor.Send(Vector3Single(0.2, 0.2, 0.2));
      end;
      Player.Render(RenderingCamera.Frustum, Params);
      if Params.InShadow and HeadlightInstance(H) then
      begin
        H.Node.FdAmbientIntensity.Send(H.Node.FdAmbientIntensity.DefaultValue);
        H.Node.FdColor.Send(H.Node.FdColor.DefaultValue);
      end;
    end;
  end;
end;

procedure TGameSceneManager.RenderShadowVolume;
begin
  if not DebugNoCreatures then
    Player.Scene.RenderShadowVolume(ShadowVolumeRenderer, false, Player.Transform);
end;

function TGameSceneManager.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
begin
  AMainLightPosition := {Vector4Single(SceneCamera.InitialPosition, 1);}
    Vector4Single(1, 1, -1, 0);
  Result := true;
end;

procedure TGameSceneManager.ApplyProjection;
var
  ZFar: TGLfloat;
begin
  if RenderShadowsPossible then
    ZFar := ZFarInfinity else
    ZFar := 100;

  SceneCamera.ProjectionMatrix := PerspectiveProjection(
    AngleOfViewY, Window.Width / Window.Height, 0.01, ZFar);
end;

{ rest ----------------------------------------------------------------------- }

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
var
  FileName: string;
begin
  case C of
    CharEscape: UserQuit := true;
    { TODO: only debug feature. }
    's':
      begin
        Inc(DebugScene3DDisplay);
        if DebugScene3DDisplay = 3 then DebugScene3DDisplay := 0;
        { When DebugScene3DDisplay = 0, the scene only goes to depth buffer,
          so make it faster. }
        if DebugScene3DDisplay = 0 then
          CurrentLocation.Scene.Attributes.Mode := rmDepth else
          CurrentLocation.Scene.Attributes.Mode := rmFull;
      end;
    else
      case Key of
        K_F5:
          begin
            FileName := FileNameAutoInc('rift_screen_%d.png');
            Window.SaveScreen(FileName);
            Notifications.Show(Format('Saved screenshot to "%s"', [FileName]));
          end;
      end;
  end;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
var
  RayOrigin, RayDirection, SelectedPoint: TVector3Single;
begin
  if Button = mbLeft then
  begin
    SceneCamera.CustomRay(
      0, 0, Window.Width, Window.Height, Window.Height,
      Window.MouseX, Window.MouseY,
      { Always uses perspective projection, for now }
      true, Vector2Single(AngleOfViewX, AngleOfViewY), ZeroVector4Single,
      RayOrigin, RayDirection);
    if CurrentLocation.Scene.OctreeCollisions.RayCollision(
         SelectedPoint, RayOrigin, RayDirection, true, nil, false, nil) <> nil then
    begin
      Player.WantsToWalk(SelectedPoint);
    end;
  end;
end;

procedure Idle(Window: TCastleWindowBase);
var
  Remove: TRemoveType;
begin
  WorldTime += Window.Fps.IdleSpeed;

  if not DebugNoCreatures then
  begin
    Remove := rtNone;
    Player.Idle(Window.Fps.IdleSpeed, Remove);
    { for now resulting Remove ignored }
  end;
end;

procedure InitLocation;
var
  ScenePosition, SceneDirection, SceneUp, SceneGravityUp: TVector3Single;
  GeneralViewpoint: TAbstractViewpointNode;
  RadAngleOfViewX, RadAngleOfViewY: Single;
begin
  GeneralViewpoint := CurrentLocation.Scene.GetPerspectiveViewpoint(
    ScenePosition, SceneDirection, SceneUp, SceneGravityUp,
    CurrentLocation.SceneCameraDescription);

  SceneCamera.Init(ScenePosition, SceneDirection, SceneUp, SceneGravityUp, 0, 0);

  { calculate AngleOfViewX, AngleOfViewY }

  if (GeneralViewpoint <> nil) and
     (GeneralViewpoint is TViewpointNode) then
    RadAngleOfViewY := TViewpointNode(GeneralViewpoint).FdFieldOfView.Value else
    RadAngleOfViewY := DefaultViewpointFieldOfView;

  RadAngleOfViewX := TViewpointNode.ViewpointAngleOfView(
    RadAngleOfViewY, Window.Width / Window.Height);

  { now, corrent RadAngleOfViewY, since RadAngleOfViewX may force it to be
    smaller than FdFieldOfView.Value --- see VRML spec and
    TViewpointNode.ViewpointAngleOfView comments. }
  RadAngleOfViewY := AdjustViewAngleRadToAspectRatio(
    RadAngleOfViewX, Window.Height / Window.Width);

  AngleOfViewX := RadToDeg(RadAngleOfViewX);
  AngleOfViewY := RadToDeg(RadAngleOfViewY);

  if DebugScene3DDisplay = 0 then
    CurrentLocation.Scene.Attributes.Mode := rmDepth else
    CurrentLocation.Scene.Attributes.Mode := rmFull;

  Player.Camera.Init(CurrentLocation.InitialPosition,
    CurrentLocation.InitialDirection,
    CurrentLocation.InitialUp,
    UnitVector3Single[2], 0, 0);

  Player.LocationChanged;
end;

procedure Play;
var
  SavedMode: TGLMode;
  SceneManager: TGameSceneManager;
begin
  Notifications.Clear;

  SceneManager := nil;
  Player := nil;

  try
    SceneManager := TGameSceneManager.Create(nil);
    SceneManager.ShadowVolumesPossible := RenderShadowsPossible;
    SceneManager.ShadowVolumes := RenderShadows;

    Locations.Load(SceneManager.BaseLights);
    CreaturesKinds.Load(SceneManager.BaseLights);

    Player := TPlayer.Create(PlayerKind);

    SavedMode := TGLMode.CreateReset(Window, 0, false, nil, nil, @NoClose);
    try
      Window.FpsShowOnCaption := DebugMenuFps;
      Window.AutoRedisplay := true;

      { We have two cameras: Player.Camera determines player avatar position.
        It used to be controllable by keys by a hacky solution, not available
        now. It's now controllable only indirectly, by clicking on target
        location.

        SceneCamera determines the rendering camera.
        To "fine position" the camera vs 2D image, it may be useful to
        temporaiily comment out "SceneCamera.IgnoreAllInputs := true" line,
        and allow to move SceneCamera. }

      SceneCamera := TWalkCamera.Create(nil);
      SceneCamera.IgnoreAllInputs := true;
      SceneManager.Camera := SceneCamera;

      Window.Controls.Add(Notifications);
      Window.Controls.Add(SceneManager);
      InitLocation;

      Window.OnKeyDown := @KeyDown;
      Window.OnMouseDown := @MouseDown;
      Window.OnIdle := @Idle;
      Window.OnDrawStyle := ds3D;

      Window.EventResize;

      UserQuit := false;
      repeat
        Application.ProcessMessage(true, true);
      until UserQuit;

      FreeAndNil(SceneCamera);
    finally FreeAndNil(SavedMode); end;
  finally
    FreeAndNil(Player);
    FreeAndNil(SceneManager);
  end;
end;

end.
