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
  {$I castleifc_load_to_x3d.inc}
  {$I castleifc_save_from_x3d.inc}
  {$undef read_interface_types}

{$define read_interface}
{$I castleifc_ifc_types.inc}
{$I castleifc_ifc_standard_types.inc}
{$I castleifc_json.inc}
{$I castleifc_load_to_x3d.inc}
{$I castleifc_save_from_x3d.inc}
{$undef read_interface}

implementation

uses TypInfo, Math,
  X3DLoad, X3DFields, CastleInternalRttiUtils, CastleClassUtils,
  CastleDownload, CastleColors, CastleQuaternions,
  // units below useful only for triangulation in saving
  CastleTriangles, CastleShapes, CastleSceneCore;

{$define read_implementation}
{$I castleifc_ifc_types.inc}
{$I castleifc_ifc_standard_types.inc}
{$I castleifc_json.inc}
{$I castleifc_load_to_x3d.inc}
{$I castleifc_save_from_x3d.inc}
{$undef read_implementation}

{ TModelFormat registration -------------------------------------------------- }

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadIfc;
  ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveIfc;

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
