{ Simple "menu" user interface, that allows to run the game or quit.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMenu;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls;

type
  { Simple "menu" user interface, that allows to run the game or quit. }
  TStateMenu = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_menu.castle-user-interface. }
    ButtonPlay, ButtonQuit: TCastleButton;
    procedure ClickPlay(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMenu: TStateMenu;

implementation

uses CastleApplicationProperties, CastleWindow,
  GameStatePlay;

{ TStateMenu ----------------------------------------------------------------- }

procedure TStateMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_menu.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  ButtonPlay := UiOwner.FindRequiredComponent('ButtonPlay') as TCastleButton;
  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;

  ButtonPlay.OnClick := @ClickPlay;
  ButtonQuit.OnClick := @ClickQuit;
  // Hide "Quit" button on mobile/console platforms, where users don't expect such button
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TStateMenu.ClickPlay(Sender: TObject);
begin
  TUIState.Current := StatePlay;
end;

procedure TStateMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
