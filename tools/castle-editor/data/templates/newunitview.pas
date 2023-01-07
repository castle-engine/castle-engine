unit ${UNIT_NAME};

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  ${VIEW_CLASS_NAME} = class(TCastleView)
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
  ${VIEW_VARIABLE_NAME}: ${VIEW_CLASS_NAME};

implementation

constructor ${VIEW_CLASS_NAME}.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := '${DESIGN_FILE_URL}';
end;

procedure ${VIEW_CLASS_NAME}.Start;
begin
  inherited;
  { Executed once when view starts. }
end;

procedure ${VIEW_CLASS_NAME}.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

end.
