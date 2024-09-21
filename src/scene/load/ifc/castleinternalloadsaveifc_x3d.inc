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

{ IFC conversion to X3D. }

{$ifdef read_interface}

{ Convert IFC file to X3D. }
function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;

{$endif read_interface}

{$ifdef read_implementation}

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;
begin
  Result := TX3DRootNode.Create('', BaseUrl);
  try
    if Ifc.Version <> '' then
      Result.MetadataString['IFC_Version'] := Ifc.Version;
    if Ifc.SchemaIdentifier <> '' then
      Result.MetadataString['IFC_SchemaIdentifier'] := Ifc.SchemaIdentifier;
    if Ifc.OriginatingSystem <> '' then
      Result.MetadataString['IFC_OriginatingSystem'] := Ifc.OriginatingSystem;
    if Ifc.PreprocessorVersion <> '' then
      Result.MetadataString['IFC_PreprocessorVersion'] := Ifc.PreprocessorVersion;
    if Ifc.TimeStamp <> '' then
      Result.MetadataString['IFC_TimeStamp'] := Ifc.TimeStamp;

    // TODO: rest
  except FreeAndNil(Result); raise end;
end;

{$endif read_implementation}