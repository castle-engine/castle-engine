unit ${UNIT_NAME};

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  ${STATE_CLASS_NAME} = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    // ButtonXxx: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ${STATE_VARIABLE_NAME}: ${STATE_CLASS_NAME};

implementation

constructor ${STATE_CLASS_NAME}.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := '${DESIGN_FILE_URL}';
end;

procedure ${STATE_CLASS_NAME}.Start;
begin
  inherited;
end;

end.
