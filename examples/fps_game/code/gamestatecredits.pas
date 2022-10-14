unit GameStateCredits;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TStateCredits = class(TUIState)
  private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBack: TCastleButton;
  end;

var
  StateCredits: TStateCredits;

implementation

uses GameStateMenu;

procedure TStateCredits.ClickBack(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

constructor TStateCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatecredits.castle-user-interface';
end;

procedure TStateCredits.Start;
begin
  inherited;
  ButtonBack.OnClick  := {$ifdef FPC}@{$endif} ClickBack;
end;

end.
