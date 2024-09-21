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
      { TODO: "metadata" of TX3DRootNode is not serialized, but META is.
        Yeah, there are effectively two ways to store metadata in X3D. }
      //Result.MetadataString['IFC_Version'] := Ifc.Version;
      Result.Meta['IFC_Version'] := Ifc.Version;
    if Ifc.SchemaIdentifier <> '' then
      Result.Meta['IFC_SchemaIdentifier'] := Ifc.SchemaIdentifier;
    if Ifc.OriginatingSystem <> '' then
      Result.Meta['IFC_OriginatingSystem'] := Ifc.OriginatingSystem;
    if Ifc.PreprocessorVersion <> '' then
      Result.Meta['IFC_PreprocessorVersion'] := Ifc.PreprocessorVersion;
    if Ifc.TimeStamp <> '' then
      Result.Meta['IFC_TimeStamp'] := Ifc.TimeStamp;

    // TODO: rest
  except FreeAndNil(Result); raise end;
end;

{$endif read_implementation}