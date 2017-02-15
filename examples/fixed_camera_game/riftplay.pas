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

uses Math, SysUtils,
  CastleGL, CastleGLUtils, CastleWindow, CastleStringUtils, CastleVectors,
  CastleFilesUtils, CastleWindowModes, CastleCameras, X3DNodes,
  CastleRays, CastleUIControls, CastleRenderer, CastleImages, CastleGLImages,
  CastleGameNotifications, CastleRenderingCamera, Castle3D, CastleRectangles,
  RiftSceneManager, CastleKeysMouse,
  RiftVideoOptions, RiftLocations, RiftCreatures, RiftWindow, RiftGame;

type
  TDebugDisplay = (
    { 3D Scene goes only to depth buffer, this is what user should see. }
    ddNormal,
    { Debug, both scene and image are displayed and blended. }
    ddBlend3D,
    { Debug, only 3D scene is displayed. }
    ddOnly3D);

var
  Player: TPlayer;
  UserQuit: boolean;
  DebugDisplay: TDebugDisplay = ddNormal;
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

  { Draw Image centered on screen, scaled to fit inside the window size.
    The goal is to always match the 3D scene projection,
    used when doing depth-buffer tests. }
  procedure DrawCentered(const Image: TGLImage);
  begin
    if DebugDisplay = ddBlend3D then
    begin
      if GLFeatures.BlendConstant then
      begin
        Image.Alpha := acBlending;
        Image.BlendingSourceFactor := bsConstantAlpha;
        Image.BlendingDestinationFactor := bdOneMinusConstantAlpha;
        Image.BlendingConstantColor := Vector4Single(0, 0, 0, 0.5);
      end else
       Image.Alpha := acNone;
    end else
      Image.Alpha := acNone;

    Image.Draw(Image.Rect.FitInside(Window.Rect));
  end;

var
  H: TLightInstance;
  SavedProjectionMatrix: TMatrix4Single;
begin
  { Render the location 3D scene.
    If DebugDisplay = ddNormal, render it only to the depth buffer. }
  if DebugDisplay = ddNormal then
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  CurrentLocation.Scene.Render(RenderingCamera.Frustum, Params);
  if DebugDisplay = ddNormal then
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

  { Render the location 2D image. }
  if (not Params.Transparent) and
     Params.ShadowVolumesReceivers and
     (DebugDisplay <> ddOnly3D) then
  begin
    SavedProjectionMatrix := ProjectionMatrix;
      OrthoProjection(0, Window.Width, 0, Window.Height); // need 2D projection
      {$ifndef OpenGLES}
      { TGLImage.Draw will reset modelview matrix
        (that should keep camera matrix, in non-OpenGLES renderer now),
        so save it }
      glPushMatrix;
      {$endif}
        if Params.InShadow then
          DrawCentered(CurrentLocation.GLShadowedImage)
        else
          DrawCentered(CurrentLocation.GLImage);
      {$ifndef OpenGLES}
      glPopMatrix;
      {$endif}
    ProjectionMatrix := SavedProjectionMatrix; // restore 3D projection
  end;

  { Render the 3D characters, both to depth and color buffers. }
  if not DebugNoCreatures then
  begin
    if Params.InShadow and HeadlightInstance(H) and

      { We implement PlayerKind.ReceiveShadowVolumes=false
        by simply making player look the same,
        regardless if Params.InShadow is true or false.

        We do not implement ReceiveShadowVolumes=false case by setting
        player TCastleScene.ReceiveShadowVolumes=false, that would be wrong:

        - The DrawCentered image done above would always draw over the
          player, obscuring the visible player, since scenes with
          ReceiveShadowVolumes=false are drawn underneath
          by TGLShadowVolumeRenderer.Render.

        - Moreover the player's shadow would be visible on the image
          (drawn by DrawCentered), since player still casts shadow volumes.
      }

      PlayerKind.ReceiveShadowVolumes then
    begin
      H.Node.AmbientIntensity := 1;
      H.Node.Color := Vector3Single(0.2, 0.2, 0.2);
    end;
    RiftPlay.Player.Render(RenderingCamera.Frustum, Params);
    if Params.InShadow and HeadlightInstance(H) then
    begin
      H.Node.AmbientIntensity := H.Node.FdAmbientIntensity.DefaultValue;
      H.Node.Color := H.Node.FdColor.DefaultValue;
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
  { The light casting shadows.
    TODO: Hardcoded for now, in the future could be determined per-location
    (e.g. looking at location light with shadowVolumesMain, would be consistent
    with standard 3D rendering of shadow volumes). }
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
    Result.ProjectionFar := ZFarInfinity
  else
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
      if Event.KeyCharacter = CharEscape then
        UserQuit := true
      else
      { Debug keys }
      case Event.Key of
        K_F5:
          begin
            URL := FileNameAutoInc('rift_screen_%d.png');
            Window.SaveScreen(URL);
            Notifications.Show(Format('Saved screenshot to "%s"', [URL]));
          end;
        K_F2:
          begin
            if DebugDisplay = High(DebugDisplay) then
              DebugDisplay := Low(DebugDisplay)
            else
              DebugDisplay := Succ(DebugDisplay);
            { When DebugDisplay = ddNormal, the scene only goes to depth buffer,
              so make it faster. }
            if DebugDisplay = ddNormal then
              CurrentLocation.Scene.Attributes.Mode := rmDepth
            else
              CurrentLocation.Scene.Attributes.Mode := rmFull;
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

  if DebugDisplay = ddNormal then
    CurrentLocation.Scene.Attributes.Mode := rmDepth
  else
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
