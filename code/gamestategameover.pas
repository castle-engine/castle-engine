unit GameStateGameOver;

interface

uses Classes,
  CastleUIState, CastleControls;

type
  TStateGameOver = class(TUIState)
  private
     ButtonMenu: TCastleButton;

     procedure ClickMenu(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateGameOver: TStateGameOver;

implementation

uses GameStateMenu;

procedure TStateGameOver.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStateGameOver.Start;
begin
  inherited;

  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonMenu.OnClick := @ClickMenu;
end;

end.
