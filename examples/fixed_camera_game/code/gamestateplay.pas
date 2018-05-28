{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ State in which you actually play the game (TStatePlay). }
unit GameStatePlay;

interface

uses
  CastleUIState, CastleSceneManager, X3DNodes, CastleProjection,
  CastleTransform, CastleVectors, CastleKeysMouse, CastleControls,
  GameCreatures, GameLocations;

type
  { State in which you actually play the game. }
  TStatePlay = class(TUIState)
  strict private
    Player: TPlayer;
    CurrentLocation: TLocation;
    SceneManager: TCastleSceneManager;
    InfoLabel: TCastleLabel;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses Math, SysUtils,
  CastleGLUtils, CastleStringUtils, CastleProgress, CastleUtils, CastleCameras,
  CastleFilesUtils, CastleUIControls, CastleRenderer, CastleImages,
  CastleGameNotifications, CastleRectangles, CastleColors,
  GameStateMainMenu;

{ TStatePlay ----------------------------------------------------------------------- }

procedure TStatePlay.Resize;
begin
  inherited;
  CurrentLocation.Scene.SceneManagerRect := SceneManager.ScreenRect;
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

  SceneManager := TCastleSceneManager.Create(FreeAtStop);
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
  Player.SetView(
    CurrentLocation.PlayerPosition,
    CurrentLocation.PlayerDirection,
    CurrentLocation.PlayerUp);
  Player.LocationChanged;
  SceneManager.Items.Add(Player);

  { set NavigationType after SceneManager.MainScene is assigned.
    This way newly created camera will have positio/dir/up derived
    from location Viewpoint. }
  SceneManager.NavigationType := ntNone;

  InfoLabel := TCastleLabel.Create(FreeAtStop);
  InfoLabel.Color := White;
  InfoLabel.Anchor(hpRight, -5);
  InfoLabel.Anchor(vpTop, -5);
  InsertFront(InfoLabel);
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
    '[F2] toggles debug rendering.' + NL +
    '[F5] to make screenshot.' + NL +
    '[Escape] exit.';
end;

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
        K_F2:
          CurrentLocation.Scene.RenderInternalModel :=
            not CurrentLocation.Scene.RenderInternalModel;
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

end.
