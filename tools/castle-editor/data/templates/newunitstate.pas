unit ${UNIT_NAME};

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  ${STATE_CLASS_NAME} = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
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
  { Find components, by name, that we need to access from code }
  // ButtonXxx := DesignedComponent('ButtonXxx') as TCastleButton;
end;

end.
