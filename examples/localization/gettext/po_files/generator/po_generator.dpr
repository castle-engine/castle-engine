{ Generate ../user_interface.pot for UI files. }

uses CastleLocalizationGetText, CastleFilesUtils,
  { Use this unit to allow deserializing all used UI classes. }
  CastleControls, CastleViewport;

begin
  StringToFile('../user_interface.pot',
    GenerateGetTextPo('../../data/*.castle-user-interface'));
end.
