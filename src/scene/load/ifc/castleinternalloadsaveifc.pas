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
  - https://www.buildingsmart.org/standards/bsi-standards/industry-foundation-classes/
  - https://technical.buildingsmart.org/standards/ifc/
  - Specs https://technical.buildingsmart.org/standards/ifc/ifc-schema-specifications/
    Most important: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/index.html
  - Excellent intro: https://www.youtube.com/watch?v=h2Rv9iu7yDk
  - Test files: https://github.com/buildingSMART/Sample-Test-Files/
}
unit CastleInternalLoadSaveIfc;

{$I castleconf.inc}

interface

implementation

uses Contnrs, Generics.Collections, SysUtils, Classes,
  FpJson, JSONParser, JSONScanner,
  CastleVectors, X3DLoad, X3DNodes, CastleUriUtils;

{ IFC types and classes ------------------------------------------------------ }

type
  // forward declarations
  TIfcRepresentationItem = class;
  TIfcStyledItem = class;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReal.htm }
  TIfcReal = Single;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPositiveLengthMeasure.htm }
  TIfcPositiveLengthMeasure = Single;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDimensionCount.htm }
  TIfcDimensionCount = 1..3;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLabel.htm }
  TIfcLabel = String;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcText.htm }
  TIfcText = String;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIdentifier.htm }
  TIfcIdentifier = String;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationStyle.htm }
  TIfcPresentationStyle = class
    Name: TIfcLabel;
  end;
  TIfcPresentationStyleList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPresentationStyle>;

  { Collection of all those items, that are assigned to a single layer.
    This is either TIfcRepresentationItem or (TODO) TIfcRepresentation.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLayeredItem.htm }
  TIfcLayeredItem = TObject;
  TIfcLayeredItemList = Contnrs.TObjectList;

  { The presentation layer assignment provides the layer name (and optionally
    a description and an identifier) for a collection of geometric
    representation items.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationLayerAssignment.htm }
  TIfcPresentationLayerAssignment = class
    { Name of the layer. }
    Name: TIfcLabel;

    { Additional description of the layer. }
    Description: TIfcText;

    { The set of layered items, which are assigned to this layer.
      Always at least 1 element. }
    AssignedItems: TIfcLayeredItemList;

    { An (internal) identifier assigned to the layer. }
    Identifier: TIfcIdentifier;

    constructor Create;
    destructor Destroy; override;
  end;

  { Most commonly these IfcRepresentationItem's are geometric or topological
    representation items, that can (but not need to) have presentation
    style information assigned.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRepresentationItem.htm }
  TIfcRepresentationItem = class
    { Assignment of the representation item to a single or multiple layer(s).
      @nil if none. }
    LayerAssignment: TIfcPresentationLayerAssignment;

    { Reference to the IfcStyledItem that provides presentation information
      to the representation, e.g. a curve style, including colour and thickness
      to a geometric curve.
      @nil if none. }
    StyledByItem: TIfcStyledItem;
  end;

  { https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStyledItem.htm }
  TIfcStyledItem = class(TIfcRepresentationItem)
    { A geometric representation item to which the style is assigned.
      May be @nil. }
    Item: TIfcRepresentationItem;

    { Representation styles which are assigned,
      either to an geometric representation item, or to a material definition.
      Always has at least 1 element. }
    Styles: TIfcPresentationStyleList;

    { The word, or group of words, by which the styled item is referred to. }
    Name: TIfcLabel;

    constructor Create;
    destructor Destroy; override;
  end;

  { The IfcDirection provides a direction in two or three dimensional space.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDirection.htm }
  TIfcDirection = class
    { Defined as 2 or 3 values of TIfcReal type.
      In practice, this is just a 3D vector for our engine. }
    DirectionRatios: TVector3;

    { The number of dimensions specified in the IFC file, 2 or 3.
      Regardless of it, DirectionRatios always has the Z value defined:
      we set Z to 0 when it is not defined in IFC. }
    Dim: Single;
  end;

  { common supertype of all geometric items used within a representation.
    It is positioned within a geometric coordinate system,
    directly or indirectly through intervening items.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricRepresentationItem.htm }
  TIfcGeometricRepresentationItem = class abstract(TIfcRepresentationItem)
  end;

  { 3D shape.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSolidModel.htm }
  TIfcSolidModel = class abstract(TIfcGeometricRepresentationItem)
  const
    { The space dimensionality of this class, it is always 3. }
    Dim = 3;
  end;

  { Represents the 3D shape by a sweeping representation scheme allowing
    a two dimensional planar cross section to sweep through space.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptAreaSolid.htm }
  TIfcSweptAreaSolid = class abstract(TIfcSolidModel)
    { The surface defining the area to be swept.
      It is given as a profile definition within the xy plane
      of the position coordinate system.}
    // TODO: SweptArea: TIfcProfileDef;

    { Position coordinate system for the resulting swept solid of the sweeping operation. }
    // TODO: Position: TIfcAxis2Placement3D;
  end;

  { Defined by sweeping a cross section provided by a profile definition.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExtrudedAreaSolid.htm }
  TIfcExtrudedAreaSolid = class(TIfcSweptAreaSolid)
    { The direction in which the surface, provided by SweptArea is to be swept.
      The ExtrudedDirection shall not be perpendicular to the local z-axis. }
    ExtrudedDirection: TIfcDirection;

    { The distance the surface is to be swept along the ExtrudedDirection. }
    Depth: TIfcPositiveLengthMeasure;
  end;

  { Most abstract and root class for all entity definitions.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoot.htm }
  TIfcRoot = class abstract
    GlobalId: TGUID;

    { Information about the current ownership of that object. }
    // TODO: OwnerHistory: TIfcOwnerHistory;

    Name: TIfcLabel;
    Description: TIfcText;
  end;

  { Any semantically treated thing or process.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectDefinition.htm }
  TIfcObjectDefinition = class abstract(TIfcRoot)
    // TODO
  end;

  { Project context in which objects, type objects, property sets,
    and properties are defined.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcContext.htm }
  TIfcContext = class abstract(TIfcObjectDefinition)
    // TODO
  end;

  { Context for information to be exchanged or shared,
    it may represent a construction project but does not have to.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm }
  TIfcProject = class(TIfcContext)
  end;

{ TIfcStyledItem ------------------------------------------------------------- }

constructor TIfcStyledItem.Create;
begin
  inherited;
  Styles := TIfcPresentationStyleList.Create(true);
end;

destructor TIfcStyledItem.Destroy;
begin
  FreeAndNil(Styles);
  inherited;
end;

{ TIfcPresentationLayerAssignment ------------------------------------------- }

constructor TIfcPresentationLayerAssignment.Create;
begin
  inherited;
  AssignedItems := TIfcLayeredItemList.Create(true);
end;

destructor TIfcPresentationLayerAssignment.Destroy;
begin
  FreeAndNil(AssignedItems);
  inherited;
end;

{ Loading -------------------------------------------------------------------- }

{ Main routine that converts IFC -> X3D nodes, doing most of the work. }
function LoadIfc(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Project: TIfcProject;
  JsonParser: TJSONParser;
  Json: TJSONData;
begin
  JsonParser := TJSONParser.Create(Stream, [joComments]);
  try
    Json := JsonParser.Parse;
    try
      Project := TIfcProject.Create;
      try
        // TODO: read Json to Project
        Result := TX3DRootNode.Create('', BaseUrl);
        try
          // TODO: convert Project to X3D nodes
        except FreeAndNil(Result); raise end;
      finally FreeAndNil(Project) end;
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
end.
