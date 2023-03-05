{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View in which you actually play the game (TViewPlay). }
unit GameViewPlay;

interface

uses
  CastleUIControls, CastleViewport, X3DNodes, CastleProjection,
  CastleTransform, CastleVectors, CastleKeysMouse, CastleControls,
  GameCreatures, GameLocations;

type
  { View in which you actually play the game. }
  TViewPlay = class(TCastleView)
  strict private
    Player: TPlayer;
    CurrentLocation: TLocation;
    Viewport: TCastleViewport;
    InfoLabel: TCastleLabel;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses Math, SysUtils,
  CastleGLUtils, CastleStringUtils, CastleUtils, CastleCameras,
  CastleFilesUtils, CastleImages, CastleScene,
  CastleGameNotifications, CastleRectangles, CastleColors,
  GameViewMainMenu;

{ TViewPlay ----------------------------------------------------------------------- }

procedure TViewPlay.Resize;
begin
  inherited;
  CurrentLocation.Scene.ViewportRect := Viewport.RenderRect.Round;
end;

procedure TViewPlay.Start;
var
  Location: TLocation;
  CreatureKind: TCreatureKind;
  LightForShadows: TCastleDirectionalLight;
begin
  inherited;
  CurrentLocation := Locations.StartLocation;

  Notifications.Clear;
  InsertFront(Notifications);

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  InsertFront(Viewport);

  { TODO: light type, direction, should be configured in .castle-transform
    that also contains location model.
    This is prevented now because it needs a special TLocationScene to render. }
  LightForShadows := TCastleDirectionalLight.Create(FreeAtStop);
  LightForShadows.Shadows := true;
  LightForShadows.Direction := Vector3(-1, 1, 1);
  Viewport.Items.Add(LightForShadows);

  for Location in Locations do
  begin
    Location.Load(Viewport.PrepareParams);
  end;
  for CreatureKind in CreatureKinds do
  begin
    CreatureKind.Load(Viewport.PrepareParams);
  end;

  Viewport.Items.Add(CurrentLocation.Scene);
  { set as MainScene, to allow location VRML / X3D file to determine
    headlight, viewpoint, shadow volumes light... }
  Viewport.Items.MainScene := CurrentLocation.Scene;

  Player := TPlayer.Create(CreatureKinds.PlayerKind);
  Player.SetView(
    CurrentLocation.PlayerPosition,
    CurrentLocation.PlayerDirection,
    // hardcode up vector to +Y, this is easier for moving calculations
    Vector3(0, 1, 0));
  Player.LocationChanged;
  Viewport.Items.Add(Player);

  InfoLabel := TCastleLabel.Create(FreeAtStop);
  InfoLabel.Color := White;
  InfoLabel.Anchor(hpRight, -5);
  InfoLabel.Anchor(vpTop, -5);
  InsertFront(InfoLabel);
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Player);
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  InfoLabel.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    '[Click] to move.' + NL +
    '[F2] toggles debug rendering.' + NL +
    '[F5] to make screenshot.' + NL +
    '[Escape] exit.';
end;

function TViewPlay.Press(const Event: TInputPressRelease): boolean;
var
  URL: string;
begin
  Result := inherited;

  case Event.EventType of
    itKey:
      if Event.KeyCharacter = CharEscape then
        Container.View := ViewMainMenu
      else
      { Debug keys }
      case Event.Key of
        keyF2:
          CurrentLocation.Scene.RenderInternalModel :=
            not CurrentLocation.Scene.RenderInternalModel;
        keyF5:
          begin
            URL := Container.SaveScreenToDefaultFile;
            if URL <> '' then
              Notifications.Show(Format('Saved screenshot to "%s"', [URL]));
          end;
      end;
    itMouseButton:
      begin
        if Event.MouseButton = buttonLeft then
        begin
          if Viewport.MouseRayHit <> nil then
            Player.WantsToWalk(Viewport.MouseRayHit.Last.Point);
        end;
      end;
  end;
end;

end.
