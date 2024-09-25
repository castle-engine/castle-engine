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

  This unit is internal and most engine users should not use it directly.
  To load and save IFC files, just use standard Castle Game Engine API:

  @orderedList(
    @item(Load using LoadNode, save using SaveNode.

       This means you load / save X3D nodes graph, rooted in TX3DRootNode.
    )

    @item(Use TCastleScene: TCastleScene.Load, TCastleScene.Save, setting TCastleScene.Url.

       This is the most common way to load and save models in Castle Game Engine.
       See https://castle-engine.io/viewport_and_scenes
       about viewports and TCastleScene.

       Underneath, TCastleScene methods just use LoadNode / SaveNode,
       and the X3D nodes graph is exposed in TCastleScene.RootNode.
    )
  )

  Using this unit however may be useful as an additional,
  unofficial for now (not guaranteed forever), way to load and save IFC files.
  Using this unit, you loadi / save IFC in 2 steps:

  @orderedList(
    @item(file is loaded/saved as TIfcFile instance using
      @link(IfcJsonLoad) and @link(IfcJsonSave),)

    @item(and then you can convert TIfcFile instance to TX3DRootNode
      that you can load to TCastleScene. Use @link(IfcToX3D) for this.

      There is also @link(X3DToIfc) but you may not need it, if you
      don't modify the X3D nodes graph at runtime. Just keep your
      TIfcFile instance as a "single source of truth".
    )
  )

  The advantage of this unofficial way of dealing with IFC? In short,
  you work more directly with IFC data. It makes sense if you want to
  integrate your application with IFC internals.

  @unorderedList(
    @item(You use classes from this uni, ith exactly the same
      class and property names as IFC specification uses.)

    @item(You don't wonder "how did this metadata map to X3D node"
      because you don't need to use X3D nodes to investigate what's
      inside the IFC file.)

    @item(Round-trip (read IFC, save it back) preserves more exactly
      the input. Because we don't lose intermediate TIfcFile in the
      process -- you load to TIfcFile, then you save it back.

      In contrast, when working with TX3DRootNode, the reader discards
      the original IFC data, and the writer creates new IFC data from
      X3D nodes, using @link(X3DToIfc).
      Note that we will try to make @link(X3DToIfc) perfect anyway.
      But if you keep around the TIfcFile instance, then it's guaranteed
      to be perfect, always, without any effort.)
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

uses TypInfo, RttiUtils,
  X3DLoad, CastleInternalRttiUtils, CastleStringUtils, CastleClassUtils,
  CastleDownload;

{$define read_implementation}
{$I castleinternalloadsaveifc_ifc_types.inc}
{$I castleinternalloadsaveifc_ifc_standard_types.inc}
{$I castleinternalloadsaveifc_json.inc}
{$I castleinternalloadsaveifc_x3d.inc}
{$undef read_implementation}

{ Loading -------------------------------------------------------------------- }

{ Main routine that converts IFC -> X3D nodes, doing most of the work. }
function LoadIfc(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

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
        DebugSaveBack(IfcFile);
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
