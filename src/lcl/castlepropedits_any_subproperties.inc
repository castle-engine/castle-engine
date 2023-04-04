{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TSubPropertiesEditor ----------------------------------------------------- }

{$ifdef read_interface}
type
  { Editor for any published SubComponent, you want to be able to
    expand it in the LCL object inspector. }
  TSubPropertiesEditor = class(TClassPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TSubPropertiesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{$endif read_implementation}
