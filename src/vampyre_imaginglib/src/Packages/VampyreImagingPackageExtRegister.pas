unit VampyreImagingPackageExtRegister;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImagingComponents;

procedure Register;

implementation

procedure Register;
begin
  // Register file formats for TGraphic again (once done when registering base package) to
  // have the additional file formats. RegisterTypes can handle being called
  // more than once and registers only the new formats to LCL.
  { Castle Game Engine: disabled, just like we disabled COMPONENT_SET_LCL.
    TODO: investigate, submit bug or PR upstream. }
  //ImagingComponents.RegisterTypes;
end;

end.
