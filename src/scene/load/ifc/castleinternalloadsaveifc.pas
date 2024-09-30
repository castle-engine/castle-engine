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

{ Loading and saving of Industry Foundation Classes (IFC).

  See:

  @unorderedList(
    @item(Specs:
      @unorderedList(
        @item Most important, latest spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/index.html
        @item https://www.buildingsmart.org/standards/bsi-standards/industry-foundation-classes/
        @item https://technical.buildingsmart.org/standards/ifc/
        @item https://technical.buildingsmart.org/standards/ifc/ifc-schema-specifications/
      )
    )
    @item Great overview by Dion Moult: https://www.youtube.com/watch?v=h2Rv9iu7yDk
    @item Test files: https://github.com/buildingSMART/Sample-Test-Files/
  )
}
unit CastleInternalLoadSaveIfc;

{$I castleconf.inc}
{$scopedenums on}

interface

uses Contnrs, Generics.Collections, SysUtils, Classes,
  FpJson, JSONParser, JSONScanner,
  CastleVectors, X3DNodes, CastleUriUtils, CastleLog;

{$define read_interface}
{$I castleinternalloadsaveifc_ifc_types.inc}
{$I castleinternalloadsaveifc_ifc_standard_types.inc}
{$I castleinternalloadsaveifc_json.inc}
{$I castleinternalloadsaveifc_x3d.inc}
{$undef read_interface}

implementation

uses TypInfo,
  X3DLoad, CastleInternalRttiUtils, CastleStringUtils;

{$define read_implementation}
{$I castleinternalloadsaveifc_ifc_types.inc}
{$I castleinternalloadsaveifc_ifc_standard_types.inc}
{$I castleinternalloadsaveifc_json.inc}
{$I castleinternalloadsaveifc_x3d.inc}
{$undef read_implementation}

{ Loading -------------------------------------------------------------------- }

{ Main routine that converts IFC -> X3D nodes, doing most of the work. }
function LoadIfc(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  JsonParser: TJsonParser;
  Json: TJsonData;
  IfcFile: TIfcFile;
begin
  JsonParser := TJsonParser.Create(Stream, [joComments]);
  try
    Json := JsonParser.Parse;
    try
      IfcFile := IfcJsonLoad(Json);
      try
        Result := IfcToX3D(IfcFile, BaseUrl);
      finally FreeAndNil(IfcFile) end;
    finally FreeAndNil(Json) end;
  finally FreeAndNil(JsonParser) end;
end;

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadIfc;

  // These are own own MIME types, as specs don't define any MIME type for IFC.
  // https://technical.buildingsmart.org/standards/ifc/ifc-formats/
  // TODO: Only JSON variant supported now.
  //ModelFormat.MimeTypes.Add('application/x-ifc');
  ModelFormat.MimeTypes.Add('application/x-ifc-json');
  //ModelFormat.FileFilterName := 'IFC (*.ifcjson, *.ifc)';
  ModelFormat.FileFilterName := 'IFC (*.ifcjson)';
  //ModelFormat.Extensions.Add('.ifc');
  ModelFormat.Extensions.Add('.ifcjson');
  RegisterModelFormat(ModelFormat);

  UriMimeExtensions['.ifcjson'] := 'application/x-ifc-json';
finalization
  FinalizeIfcClasses;
end.
