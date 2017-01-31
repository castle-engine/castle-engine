{
  Copyright 2008-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftPlay;

{$I castleconf.inc}

interface

procedure Play;

implementation

uses CastleGL, CastleGLUtils, CastleWindow, CastleStringUtils, CastleVectors, CastleFilesUtils,
  CastleWindowModes, CastleCameras, SysUtils, X3DNodes,
  CastleRays, Math, CastleUIControls, CastleRenderer,
  RiftVideoOptions, RiftLocations, RiftCreatures, RiftWindow, RiftGame,
  CastleGameNotifications, CastleRenderingCamera, Castle3D,
  RiftSceneManager, CastleKeysMouse;

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
    function CalculateProjection: TProjection; override;
    procedure Render3D(const Params: TRenderParams); override;
    procedure RenderShadowVolume; override;
    function MainLightForShadows(out AMainLightPosition: TVector4Single): boolean; override;
  end;

procedure TGameSceneManager.Render3D(const Params: TRenderParams);
var
  H: TLightInstance;
  SavedProjectionMatrix: TMatrix4Single;
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

    CurrentLocation.Scene.Render(RenderingCamera.Frustum, Params);

    if DebugScene3DDisplay = 0 then
      glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

    {$ifndef OpenGLES} //TODO-es
    if DebugScene3DDisplay <> 2 then
    begin
      SavedProjectionMatrix := ProjectionMatrix;
      OrthoProjection(0, Window.Width, 0, Window.Height); // need 2D projection
      glPushMatrix; // TGLImage.Draw will reset matrix, so save it
        glPushAttrib(GL_ENABLE_BIT);
          glDisable(GL_LIGHTING);
          if DebugScene3DDisplay = 1 then
          begin
            if GLFeatures.BlendConstant then
            begin
              glBlendColor(0, 0, 0, 0.5);
              glBlendFunc(GL_CONSTANT_ALPHA, GL_ONE_MINUS_CONSTANT_ALPHA);
              glEnable(GL_BLEND);
            end;
          end;
          if Params.InShadow then
            CurrentLocation.GLShadowedImage.Draw(0, 0) else
            CurrentLocation.GLImage.Draw(0, 0);
        glPopAttrib;
      glPopMatrix;
      ProjectionMatrix := SavedProjectionMatrix; // restore 3D projection
    end;
    {$endif}

    if not DebugNoCreatures then
    begin
      if Params.InShadow and HeadlightInstance(H) then
      begin
        H.Node.FdAmbientIntensity.Send(1);
        H.Node.FdColor.Send(Vector3Single(0.2, 0.2, 0.2));
      end;
      RiftPlay.Player.Render(RenderingCamera.Frustum, Params);
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
    RiftPlay.Player.RenderShadowVolume(ShadowVolumeRenderer, true, IdentityMatrix4Single);
end;

function TGameSceneManager.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
begin
  AMainLightPosition := {Vector4Single(SceneCamera.InitialPosition, 1);}
    Vector4Single(1, 1, -1, 0);
  Result := true;
end;

function TGameSceneManager.CalculateProjection: TProjection;
begin
  Result.ProjectionType := ptPerspective;
  Result.PerspectiveAngles[0] := AngleOfViewX;
  Result.PerspectiveAngles[1] := AngleOfViewY;
  Result.ProjectionNear := 0.01;
  Result.ProjectionFarFinite := 100;
  if GLFeatures.ShadowVolumesPossible and ShadowVolumes then
    Result.ProjectionFar := ZFarInfinity else
    Result.ProjectionFar := Result.ProjectionFarFinite;
end;

{ rest ----------------------------------------------------------------------- }

var
  SceneManager: TGameSceneManager;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
var
  URL: string;
  RayOrigin, RayDirection, SelectedPoint: TVector3Single;
begin
  case Event.EventType of
    itKey:
      case Event.KeyCharacter of
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
          case Event.Key of
            K_F5:
              begin
                URL := FileNameAutoInc('rift_screen_%d.png');
                Window.SaveScreen(URL);
                Notifications.Show(Format('Saved screenshot to "%s"', [URL]));
              end;
          end;
      end;
    itMouseButton:
      begin
        if Event.MouseButton = mbLeft then
        begin
          { This is a cleaner way to query mouse picks
            if CurrentLocation.Scene was part of scene manager.
            But for now, this example does rendering tricks
            (see TGameSceneManager.Render3D) and avoids using SceneManager
            is normal way. TODO: to be fixed.

          if SceneManager.MouseRayHit <> nil then
            Player.WantsToWalk(SceneManager.MouseRayHit.Last.Point);
          }

          SceneCamera.CustomRay(Window.Rect, Window.MousePosition,
            SceneManager.Projection, RayOrigin, RayDirection);
          if CurrentLocation.Scene.InternalOctreeCollisions.RayCollision(
               SelectedPoint, RayOrigin, RayDirection, true, nil, false, nil) <> nil then
          begin
            Player.WantsToWalk(SelectedPoint);
          end;
        end;
      end;
  end;
end;

procedure Update(Container: TUIContainer);
var
  Remove: TRemoveType;
begin
  WorldTime += Window.Fps.UpdateSecondsPassed;

  if not DebugNoCreatures then
  begin
    Remove := rtNone;
    Player.Update(Window.Fps.UpdateSecondsPassed, Remove);
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

  Player.SetView(CurrentLocation.InitialPosition,
    CurrentLocation.InitialDirection,
    CurrentLocation.InitialUp);

  Player.LocationChanged;
end;

procedure Play;
var
  SavedMode: TGLMode;
begin
  Notifications.Clear;

  SceneManager := nil;
  Player := nil;

  try
    SceneManager := TGameSceneManager.Create(nil);
    SceneManager.ShadowVolumes := ShadowVolumes;

    Locations.Load(SceneManager.BaseLights);
    CreaturesKinds.Load(SceneManager.BaseLights);

    Player := TPlayer.Create(PlayerKind);

    SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);
    try
      Window.FpsShowOnCaption := DebugMenuFps;
      Window.AutoRedisplay := true;

      { SceneCamera determines the rendering camera.
        To "fine position" the camera vs 2D image, it may be useful to
        temporaiily comment out "SceneCamera.Input := []" line,
        and allow to move SceneCamera. }

      SceneCamera := TWalkCamera.Create(nil);
      SceneCamera.Input := [];
      SceneManager.Camera := SceneCamera;

      Window.Controls.InsertBack(Notifications);
      Window.Controls.InsertBack(SceneManager);
      InitLocation;

      Window.OnPress := @Press;
      Window.OnUpdate := @Update;

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
