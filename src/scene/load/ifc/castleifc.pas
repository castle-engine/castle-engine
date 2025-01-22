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

{ Loading and saving of Industry Foundation Classes (IFC),
  see @url(https://castle-engine.io/ifc IFC in the Castle Game Engine documentation). }
unit CastleIfc;

{$I castleconf.inc}
{$scopedenums on}

interface

uses Contnrs, Generics.Collections, SysUtils, Classes,
  FpJson, JSONParser, JSONScanner,
  CastleUtils, CastleVectors, X3DNodes, CastleUriUtils, CastleLog,
  CastleStringUtils, CastleBoxes;

type
  {$define read_interface_types}
  {$I castleifc_ifc_types.inc}
  {$I castleifc_ifc_standard_types.inc}
  {$I castleifc_json.inc}
  {$I castleifc_x3d.inc}
  {$undef read_interface_types}

{$define read_interface}
{$I castleifc_ifc_types.inc}
{$I castleifc_ifc_standard_types.inc}
{$I castleifc_json.inc}
{$I castleifc_x3d.inc}
{$undef read_interface}

implementation

uses TypInfo, RttiUtils,
  X3DLoad, X3DFields, CastleInternalRttiUtils, CastleClassUtils,
  CastleDownload, CastleColors, CastleQuaternions;

{$define read_implementation}
{$I castleifc_ifc_types.inc}
{$I castleifc_ifc_standard_types.inc}
{$I castleifc_json.inc}
{$I castleifc_x3d.inc}
{$undef read_implementation}

{ Loading -------------------------------------------------------------------- }

{ Main routine that converts IFC -> X3D nodes, doing most of the work. }
function LoadIfc(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

  {.$define DEBUG_IFC_ROUND_TRIP}

  {$ifdef DEBUG_IFC_ROUND_TRIP}
  { Only for debugging purposes: save back the IFC data to JSON.
    This is an easy test whether IfcJsonLoad understood everything correctly,
    and whether IfcJsonSave saves everything correctly. }
  procedure DebugSaveBack(const IfcFile: TIfcFile);
  var
    JsonObj: TJsonObject;
    JsonString: String;
    Stream: TStream;
  begin
    Stream := UrlSaveStream(DeleteUriExt(BaseUrl) + '-debug-roundtrip.ifcjson');
    try
      JsonObj := IfcJsonSave(IfcFile);
      try
        JsonString := JsonObj.FormatJSON;
        WriteStr(Stream, JsonString);
      finally FreeAndNil(JsonObj) end;
    finally FreeAndNil(Stream) end;
  end;
  {$endif DEBUG_IFC_ROUND_TRIP}

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
        {$ifdef DEBUG_IFC_ROUND_TRIP}
        DebugSaveBack(IfcFile);
        {$endif DEBUG_IFC_ROUND_TRIP}
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
  FreeAndNil(FIfcClasses);
end.
