unit ${UNIT_NAME};

interface

uses Classes,
  CastleUIState, CastleControls;

type
  ${STATE_CLASS_NAME} = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    // ButtonXxx: TCastleButton;
  public
    procedure Start; override;
  end;

implementation

procedure ${STATE_CLASS_NAME}.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('${DESIGN_FILE_URL}', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  // ButtonXxx := UiOwner.FindRequiredComponent('ButtonXxx') as TCastleButton;
end;

end.
