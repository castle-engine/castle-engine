{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display the game map, play the game. }
unit GameStatePlay;

interface

uses CastleUIState, CastleControls, CastleTiledMap, CastleUIControls;

type
  TStatePlay = class(TUIState)
  strict private
    MapControl: TCastleTiledMapControl;
    ButtonQuit: TCastleButton;
    procedure ClickQuit(Sender: TObject);
  public
    { Set this before starting this state. }
    MapName: String;
    procedure Start; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize, CastleUtils,
  GameStateMainMenu, GameUnit;

procedure TStatePlay.Start;
var
  Ui: TCastleUserInterface;
  UiOwner: TComponent;
  RandomUnit: TUnit;
  I: Integer;
begin
  inherited;

  { UiOwner allows to search for components using FindRequiredComponent,
    and makes sure the entire UI will be freed when state stops
    (because UiOwner is owned by FreeAtStop). }
  UiOwner := TComponent.Create(FreeAtStop);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/state_play.castle-user-interface', UiOwner);
  InsertFront(Ui);

  MapControl := UiOwner.FindRequiredComponent('MapControl') as TCastleTiledMapControl;
  MapControl.URL := 'castle-data:/maps/' + MapName + '.tmx';

  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;
  ButtonQuit.OnClick := @ClickQuit;

  for I := 1 to 10 do
  begin
    RandomUnit := TUnit.Create(FreeAtStop);
    RandomUnit.Initialize(TUnitKind(Random(Ord(High(TUnitKind)) + 1)),
      RandomIntRange(3, 10),
      RandomIntRange(3, 10),
      RandomIntRange(3, 10));
    RandomUnit.Ui.Left := Random(Round(MapControl.EffectiveWidth));
    RandomUnit.Ui.Bottom := Random(Round(MapControl.EffectiveHeight));
    MapControl.InsertFront(RandomUnit.Ui);
  end;
end;

procedure TStatePlay.ClickQuit(Sender: TObject);
begin
  TUIState.Current := StateMainMenu;
end;

end.
