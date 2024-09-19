{%MainUnit castleinternalloadsaveifc.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ IFC JSON reading and writing. }

{$ifdef read_interface}

type
  { IFC file representation.

    This class is not defined in the IFC standard, but it is consistent
    with how the IFC file is actually represented, in both STEP and JSON encodings.
    The file starts with some metadata (in STEP header, on JSON top-level object)
    and then follows the "data" which is TIfcProject instance. }
  TIfcFile = class
    EncodingType: String;
    Version: String;
    SchemaIdentifier: String;
    OriginatingSystem: String;
    PreprocessorVersion: String;
    TimeStamp: TDateTime;
    Data: TIfcProject;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function IfcJsonLoad(const Json: TJsonData): TIfcFile;
begin
  Result := TIfcFile.Create;
  // TODO
end;

{$endif read_implementation}
