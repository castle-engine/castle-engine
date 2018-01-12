{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "The Rift".

  "The Rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "The Rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "The Rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ State in which you actually play the game (TStatePlay). }
unit GameStatePlay;

//TODO
{$I castleconf.inc}

interface

uses
  CastleUIState, CastleSceneManager, X3DNodes, CastleProjection,
  CastleTransform, CastleVectors, CastleKeysMouse, CastleControls,
  GameCreatures, GameLocations;

type
  { State in which you actually play the game. }
  TStatePlay = class(TUIState)
  strict private
    type
      TGameSceneManager = class(TCastleSceneManager)
      public
        State: TStatePlay; // TODO
        function CalculateProjection: TProjection; override;
        procedure Render3D(const Params: TRenderParams); override;
      end;

    var
      Player: TPlayer;
      CurrentLocation: TLocation;
      AngleOfViewX, AngleOfViewY: Single;
      SceneManager: TGameSceneManager;
      InfoLabel: TCastleLabel;
    procedure InitLocation;
  public
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses Math, SysUtils,
  //TODO
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleGLUtils, CastleStringUtils, CastleProgress, CastleCameras, CastleUtils,
  CastleFilesUtils, CastleUIControls, CastleRenderer, CastleImages, CastleGLImages,
  CastleGameNotifications, CastleRectangles, CastleRenderingCamera, CastleColors,
  GameStateMainMenu;

{ TStatePlay.TGameSceneManager ----------------------------------------------- }

procedure TStatePlay.TGameSceneManager.Render3D(const Params: TRenderParams);

  { Draw Image centered on screen, scaled to fit inside the window size.
    The goal is to always match the 3D scene projection. }
  procedure DrawCentered(const Image: TGLImage);
  begin
    Image.Draw(Image.Rect.FitInside(Container.Rect));
  end;

var
  H: TLightInstance;
  SavedProjectionMatrix: TMatrix4;
begin
  // TODO: test
  // inherited;
  // Exit;

  { this is done by TCastleAbstractViewport.Render3D in normal circumstances }
  Params.Frustum := @RenderingCamera.Frustum;

  { Render the location 3D scene }
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  State.CurrentLocation.Scene.Render(Params);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

  { Render the location 2D image. }
  if (not Params.Transparent) and
     Params.ShadowVolumesReceivers then
  begin
    SavedProjectionMatrix := RenderContext.ProjectionMatrix;
      OrthoProjection(FloatRectangle(Container.Rect)); // need 2D projection
      Assert(not GLFeatures.EnableFixedFunction);
      {$ifndef OpenGLES}
      { With GLFeatures.EnableFixedFunction = true,
        TGLImage.Draw will reset modelview matrix
        (that should keep camera matrix, in non-OpenGLES renderer now),
        so save it. }
      glPushMatrix;
      {$endif}
        if Params.InShadow then
          DrawCentered(State.CurrentLocation.ShadowedImage)
        else
          DrawCentered(State.CurrentLocation.Image);
      {$ifndef OpenGLES}
      glPopMatrix;
      {$endif}
    RenderContext.ProjectionMatrix := SavedProjectionMatrix; // restore 3D projection
  end;

  { Render the 3D characters, both to depth and color buffers. }
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

    CreatureKinds.PlayerKind.ReceiveShadowVolumes then
  begin
    H.Node.AmbientIntensity := 1;
    H.Node.Color := Vector3(0.2, 0.2, 0.2);
  end;
  State.Player.Render(Params);
  if Params.InShadow and HeadlightInstance(H) then
  begin
    H.Node.AmbientIntensity := H.Node.FdAmbientIntensity.DefaultValue;
    H.Node.Color := H.Node.FdColor.DefaultValue;
  end;
end;

function TStatePlay.TGameSceneManager.CalculateProjection: TProjection;
begin
  Result.ProjectionType := ptPerspective;
  Result.PerspectiveAngles[0] := State.AngleOfViewX;
  Result.PerspectiveAngles[1] := State.AngleOfViewY;
  Result.ProjectionNear := 0.01;
  Result.ProjectionFarFinite := 100;
  if GLFeatures.ShadowVolumesPossible and ShadowVolumes then
    Result.ProjectionFar := ZFarInfinity
  else
    Result.ProjectionFar := Result.ProjectionFarFinite;
end;

{ TStatePlay ----------------------------------------------------------------------- }

function TStatePlay.Press(const Event: TInputPressRelease): boolean;
var
  URL: string;
begin
  Result := inherited;

  case Event.EventType of
    itKey:
      if Event.KeyCharacter = CharEscape then
        TUIState.Current := StateMainMenu
      else
      { Debug keys }
      case Event.Key of
        K_F5:
          begin
            URL := FileNameAutoInc(ApplicationName + '_screen_%d.png');
            Container.SaveScreen(URL);
            Notifications.Show(Format('Saved screenshot to "%s"', [URL]));
          end;
      end;
    itMouseButton:
      begin
        if Event.MouseButton = mbLeft then
        begin
          if SceneManager.MouseRayHit <> nil then
            Player.WantsToWalk(SceneManager.MouseRayHit.Last.Point);
        end;
      end;
  end;
end;

procedure TStatePlay.InitLocation;
var
  ScenePosition, SceneDirection, SceneUp, SceneGravityUp: TVector3;
  GeneralViewpoint: TAbstractViewpointNode;
  RadAngleOfViewX, RadAngleOfViewY: Single;
begin
  GeneralViewpoint := CurrentLocation.Scene.GetPerspectiveViewpoint(
    ScenePosition, SceneDirection, SceneUp, SceneGravityUp,
    CurrentLocation.SceneCameraDescription);

  SceneManager.WalkCamera.Init(
    ScenePosition, SceneDirection, SceneUp, SceneGravityUp, 0, 0);

  // TODO: is this even necessary?
  { calculate AngleOfViewX, AngleOfViewY }

  if (GeneralViewpoint <> nil) and
     (GeneralViewpoint is TViewpointNode) then
    RadAngleOfViewY := TViewpointNode(GeneralViewpoint).FieldOfView
  else
    RadAngleOfViewY := DefaultViewpointFieldOfView;

  RadAngleOfViewX := TViewpointNode.ViewpointAngleOfView(
    RadAngleOfViewY, StateContainer.Width / StateContainer.Height);

  { now, corrent RadAngleOfViewY, since RadAngleOfViewX may force it to be
    smaller than FdFieldOfView.Value --- see X3D spec and
    TViewpointNode.ViewpointAngleOfView comments. }
  RadAngleOfViewY := AdjustViewAngleRadToAspectRatio(
    RadAngleOfViewX, StateContainer.Height / StateContainer.Width);

  AngleOfViewX := RadToDeg(RadAngleOfViewX);
  AngleOfViewY := RadToDeg(RadAngleOfViewY);

  Player.SetView(
    CurrentLocation.InitialPosition,
    CurrentLocation.InitialDirection,
    CurrentLocation.InitialUp);

  Player.LocationChanged;
end;

procedure TStatePlay.Start;
var
  Location: TLocation;
  CreatureKind: TCreatureKind;
begin
  inherited;
  CurrentLocation := Locations.StartLocation;

  Notifications.Clear;
  InsertFront(Notifications);

  SceneManager := TGameSceneManager.Create(FreeAtStop);
  SceneManager.State := Self;
  InsertFront(SceneManager);

  Progress.Init(Locations.Count + CreatureKinds.Count, 'Preparing');
  try
    for Location in Locations do
    begin
      Location.Load(SceneManager.PrepareParams);
      Progress.Step;
    end;
    for CreatureKind in CreatureKinds do
    begin
      CreatureKind.Load(SceneManager.PrepareParams);
      Progress.Step;
    end;
  finally Progress.Fini end;

  SceneManager.Items.Add(CurrentLocation.Scene);
  { set as MainScene, to allow location VRML / X3D file to determine
    headlight, viewpoint, shadow volumes light... }
  SceneManager.MainScene := CurrentLocation.Scene;

  Player := TPlayer.Create(CreatureKinds.PlayerKind);
  SceneManager.Items.Add(Player);

  { Tip: To nicely position the camera vs 2D image, it may be useful to
    temporarily use normal ntWalk or ntExamine camera. }
  SceneManager.NavigationType := ntNone;

  InfoLabel := TCastleLabel.Create(FreeAtStop);
  InfoLabel.Color := White;
  InfoLabel.Anchor(hpRight, -5);
  InfoLabel.Anchor(vpTop, -5);
  InsertFront(InfoLabel);

  InitLocation;
end;

procedure TStatePlay.Stop;
begin
  FreeAndNil(Player);
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  InfoLabel.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    '[Click] to move.' + NL +
    '[F5] to make screenshot.' + NL +
    '[Escape] exit.';
end;

end.
