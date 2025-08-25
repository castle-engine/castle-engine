unit GameViewWin;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewWin = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    // ButtonXxx: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewWin: TViewWin;

implementation

constructor TViewWin.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewwin.castle-user-interface';
end;

procedure TViewWin.Start;
begin
  inherited;
  { Executed once when view starts. }
end;

procedure TViewWin.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

end.
