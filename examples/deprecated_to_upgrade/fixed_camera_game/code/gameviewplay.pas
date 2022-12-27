{
  Copyright 2008-2022 Michalis Kamburelis.

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
  CastleGLUtils, CastleStringUtils, CastleProgress, CastleUtils, CastleCameras,
  CastleFilesUtils, CastleImages,
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
begin
  inherited;
  CurrentLocation := Locations.StartLocation;

  Notifications.Clear;
  InsertFront(Notifications);

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  InsertFront(Viewport);

  Progress.Init(Locations.Count + CreatureKinds.Count, 'Preparing');
  try
    for Location in Locations do
    begin
      Location.Load(Viewport.PrepareParams);
      Progress.Step;
    end;
    for CreatureKind in CreatureKinds do
    begin
      CreatureKind.Load(Viewport.PrepareParams);
      Progress.Step;
    end;
  finally Progress.Fini end;

  Viewport.Items.Add(CurrentLocation.Scene);
  { set as MainScene, to allow location VRML / X3D file to determine
    headlight, viewpoint, shadow volumes light... }
  Viewport.Items.MainScene := CurrentLocation.Scene;

  Player := TPlayer.Create(CreatureKinds.PlayerKind);
  Player.SetView(
    CurrentLocation.PlayerPosition,
    CurrentLocation.PlayerDirection,
    CurrentLocation.PlayerUp);
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
