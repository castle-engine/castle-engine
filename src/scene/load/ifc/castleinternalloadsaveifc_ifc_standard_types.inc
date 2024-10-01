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

{ Types and classes that correspond to the IFC standard. }

{$ifdef read_interface}

//type // continues "type" from castleinternalloadsaveifc_ifc_types.inc

  // forward declarations
  TIfcRepresentationItem = class;
  TIfcStyledItem = class;
  TIfcRelAggregatesList = class;
  TIfcProductList = class;
  TIfcRelVoidsElement = class;
  TIfcRelVoidsElementList = class;
  TIfcOpeningElement = class;
  TIfcRepresentationList = class;

  { Float type used throughout IFC.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReal.htm }
  TIfcReal = Single;

  { Positive distance.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPositiveLengthMeasure.htm }
  TIfcPositiveLengthMeasure = Single;

  { Distance.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLengthMeasure.htm }
  TIfcLengthMeasure = Single;

  { Number of dimensions.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDimensionCount.htm }
  TIfcDimensionCount = 1..3;

  { Text label.

    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLabel.htm }
  TIfcLabel = String;

  { Text.

    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcText.htm }
  TIfcText = String;

  { Identifier.

    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIdentifier.htm }
  TIfcIdentifier = String;

  { Presentation information assigned to geometric representation items.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationStyle.htm }
  TIfcPresentationStyle = class(TIfcPersistent)
  private
    FName: TIfcLabel;
  published
    property Name: TIfcLabel read FName write FName;
  end;
  TIfcPresentationStyleList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPresentationStyle>;

  { Collection of all those items, that are assigned to a single layer.
    This is either TIfcRepresentationItem or TIfcRepresentation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLayeredItem.htm }
  TIfcLayeredItem = TObject;

  { List of TIfcLayeredItem. }
  TIfcLayeredItemList = Contnrs.TObjectList;

  { The presentation layer assignment provides the layer name (and optionally
    a description and an identifier) for a collection of geometric
    representation items.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationLayerAssignment.htm }
  TIfcPresentationLayerAssignment = class(TIfcPersistent)
  private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FAssignedItems: TIfcLayeredItemList;
    FIdentifier: TIfcIdentifier;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { Name of the layer. }
    property Name: TIfcLabel read FName write FName;

    { Additional description of the layer. }
    property Description: TIfcText read FDescription write FDescription;

    { The set of layered items, which are assigned to this layer.
      Always at least 1 element. }
    property AssignedItems: TIfcLayeredItemList read FAssignedItems write FAssignedItems;

    { An (internal) identifier assigned to the layer. }
    property Identifier: TIfcIdentifier read FIdentifier write FIdentifier;
  end;

  { Geometric or topological representation items,
    that can (but not need to) have presentation style information assigned.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRepresentationItem.htm }
  TIfcRepresentationItem = class(TIfcPersistent)
  private
    FLayerAssignment: TIfcPresentationLayerAssignment;
    FStyledByItem: TIfcStyledItem;
  published
    { Assignment of the representation item to a single or multiple layer(s).
      @nil if none. }
    property LayerAssignment: TIfcPresentationLayerAssignment read FLayerAssignment write FLayerAssignment;

    { Reference to the IfcStyledItem that provides presentation information
      to the representation, e.g. a curve style, including colour and thickness
      to a geometric curve.
      @nil if none. }
    property StyledByItem: TIfcStyledItem read FStyledByItem write FStyledByItem;
  end;
  TIfcRepresentationItemList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentationItem>)
  end;

  { Style information for products.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStyledItem.htm }
  TIfcStyledItem = class(TIfcRepresentationItem)
  private
    FItem: TIfcRepresentationItem;
    FStyles: TIfcPresentationStyleList;
    FName: TIfcLabel;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { A geometric representation item to which the style is assigned.
      May be @nil. }
    property Item: TIfcRepresentationItem read FItem write FItem;

    { Representation styles which are assigned,
      either to an geometric representation item, or to a material definition.
      Always has at least 1 element. }
    property Styles: TIfcPresentationStyleList read FStyles write FStyles;

    { The word, or group of words, by which the styled item is referred to. }
    property Name: TIfcLabel read FName write FName;
  end;

  { Direction in 2D or 3D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDirection.htm }
  TIfcDirection = class(TIfcPersistent)
  private
    FDirectionRatios: TVector3;
    FDim: Single;
  public
    { Defined as 2 or 3 values of TIfcReal type.
      In practice, this is just a 3D vector for our engine.
      Note: This is serialized / deserialized to IFC, but it cannot be published
      in Pascal. }
    property DirectionRatios: TVector3 read FDirectionRatios write FDirectionRatios;
  published
    { The number of dimensions specified in the IFC file, 2 or 3.
      Regardless of it, DirectionRatios always has the Z value defined:
      we set Z to 0 when it is not defined in IFC. }
    property Dim: Single read FDim write FDim;
  end;

  { Common supertype of all geometric items used within a representation.
    It is positioned within a geometric coordinate system,
    directly or indirectly through intervening items.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricRepresentationItem.htm }
  TIfcGeometricRepresentationItem = class abstract(TIfcRepresentationItem)
  end;

  { 3D shape.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSolidModel.htm }
  TIfcSolidModel = class abstract(TIfcGeometricRepresentationItem)
  const
    { The space dimensionality of this class, it is always 3. }
    Dim = 3;
  end;

  { Represents the 3D shape by a sweeping representation scheme allowing
    a two dimensional planar cross section to sweep through space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptAreaSolid.htm }
  TIfcSweptAreaSolid = class abstract(TIfcSolidModel)
    { The surface defining the area to be swept.
      It is given as a profile definition within the xy plane
      of the position coordinate system.}
    // TODO: SweptArea: TIfcProfileDef;

    { Position coordinate system for the resulting swept solid of the sweeping operation. }
    // TODO: Position: TIfcAxis2Placement3D;
  end;

  { Defined by sweeping a cross section provided by a profile definition.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExtrudedAreaSolid.htm }
  TIfcExtrudedAreaSolid = class(TIfcSweptAreaSolid)
  private
    FExtrudedDirection: TIfcDirection;
    FDepth: TIfcPositiveLengthMeasure;
  published
    { The direction in which the surface, provided by SweptArea is to be swept.
      The ExtrudedDirection shall not be perpendicular to the local z-axis. }
    property ExtrudedDirection: TIfcDirection read FExtrudedDirection write FExtrudedDirection;

    { The distance the surface is to be swept along the ExtrudedDirection. }
    property Depth: TIfcPositiveLengthMeasure read FDepth write FDepth;
  end;

  { Most abstract and root class for all entity definitions.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoot.htm }
  TIfcRoot = class abstract(TIfcPersistent)
  private
    FGlobalId: String; // actually TGUID;
    FName: TIfcLabel;
    FDescription: TIfcText;
  published
    { Information about the current ownership of that object. }
    // TODO: OwnerHistory: TIfcOwnerHistory;

    { Globally unique identifier for the object instance.
      Note: Not declared as TGUID, because TGUID cannot be published,
      but also we don't really need it: for our purposes, this identifier
      is just an arbitrary string. }
    property GlobalId: String read FGlobalId write FGlobalId;

    property Name: TIfcLabel read FName write FName;
    property Description: TIfcText read FDescription write FDescription;
  end;

  { Any semantically treated thing or process.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectDefinition.htm }
  TIfcObjectDefinition = class abstract(TIfcRoot)
  private
    FIsDecomposedBy: TIfcRelAggregatesList;
    // TODO: rest
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property IsDecomposedBy: TIfcRelAggregatesList read FIsDecomposedBy write FIsDecomposedBy;
  end;
  TIfcObjectDefinitionList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcObjectDefinition>;

  { Any semantically treated thing or process. }
  TIfcObject = class abstract(TIfcObjectDefinition)
    // TODO
  end;

  { Define the object coordinate system. }
  TIfcObjectPlacement = class abstract(TIfcPersistent)
    // TODO
  end;

  { Abstract point.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPoint.htm }
  TIfcPoint = class abstract(TIfcGeometricRepresentationItem)
  private
    FDim: TIfcDimensionCount;
  published
    { The dimensions are derived from the descendant classes.
      Determined by the number of coordinate components in case of
      TIfcCartesianPoint or by the dimensionality
      of the basis curve or surface in other cases. }
    property Dim: TIfcDimensionCount read FDim write FDim;
  end;

  { A regular 3D point (may be specified as 2D too, in which case we set Z to 0).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianPoint.htm }
  TIfcCartesianPoint = class(TIfcPoint)
  private
    FCoordinates: TVector3;
  public
    { Actual 3D point value.
      Note: This is serialized / deserialized to IFC, but it cannot be published
      in Pascal. }
    property Coordinates: TVector3 read FCoordinates write FCoordinates;
  //published
  end;

  { Abstract supertype of placement subtypes that define the location of an item,
    or an entire shape representation, and provide its orientation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlacement.htm }
  TIfcPlacement = class abstract(TIfcRepresentationItem)
  private
    FLocation: TIfcPoint;
  published
    property Location: TIfcPoint read FLocation write FLocation;

    // TODO: We could define this to be super-consistent with IFC standard:
    // function Dim: TIfcDimensionCount; begin Result := Location.Dim; end;
  end;

  { Location and orientation to place items in a two-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement2D.htm }
  TIfcAxis2Placement2D = class(TIfcPlacement)
  private
    FRefDirection: TIfcDirection;
  published
    { Direction of the local X axis. May be @nil. }
    property RefDirection: TIfcDirection read FRefDirection write FRefDirection;

    // TODO: We could define something like this to be super-consistent with IFC standard:
    {
    function P(const Axis: Integer): TVector3;
      case
        0: Result := RefDirection.DirectionRatios;
        1: Result := RefDirection.DirectionRatios rotate around Z by 90 degrees;
        else raise EInternalError.Create('Invalid Axis');
      end;
    }
  end;

  { Location and orientations to place items in a three-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement3D.htm }
  TIfcAxis2Placement3D = class(TIfcPlacement)
  private
    FAxis: TIfcDirection;
    FRefDirection: TIfcDirection;
  published
    { Direction of the local Z axis. May be @nil. }
    property Axis: TIfcDirection read FAxis write FAxis;

    { Direction of the local X axis. May be @nil.
      If necessary an adjustment is made to maintain orthogonality to
      the Axis direction.
      If Axis and/or RefDirection is omitted, these directions are taken from
      the geometric coordinate system. }
    property RefDirection: TIfcDirection read FRefDirection write FRefDirection;

    // TODO: We could define something like this to be super-consistent with IFC standard:
    {
    function P(const Axis: Integer): TVector3;
      case
        0: Result := RefDirection.DirectionRatios;
        1: Result := TVector3.CrossProduct(Axis.DirectionRatios, RefDirection.DirectionRatios);
        2: Result := Axis.DirectionRatios;
        else raise EInternalError.Create('Invalid Axis');
      end;
    }
  end;

  { Always either TIfcAxis2Placement2D or TIfcAxis2Placement3D.
    This is not a class, but a choice in IFC standard.
    So in Pascal we also don't define it as new class (to keep our inheritance
    of classes consistent with IFC).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement.htm }
  TIfcAxis2Placement = TIfcPlacement;

  { Relative placement of a product
    - in relation to the placement of another product
    - or the absolute placement of a product
      within the geometric representation context of the project.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLocalPlacement.htm
  }
  TIfcLocalPlacement = class(TIfcObjectPlacement)
  private
    FRelativePlacement: TIfcAxis2Placement;
  published
    property RelativePlacement: TIfcAxis2Placement read FRelativePlacement write FRelativePlacement;
  end;

  { Any object that relates to a geometric or spatial context.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProduct.htm }
  TIfcProduct = class abstract(TIfcObject)
  private
    FObjectPlacement: TIfcObjectPlacement;
  published
    { Object coordinate system and placement of the product in space. }
    // TODO: rest of fields
    property ObjectPlacement: TIfcObjectPlacement read FObjectPlacement write FObjectPlacement;
  end;
  TIfcProductList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProduct>)
  end;

  { Elements that might be used to define a spatial structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialElement.htm }
  TIfcSpatialElement = class abstract(TIfcProduct)
  private
    FLongName: TIfcLabel;
    // TODO: rest of fields
  published
    property LongName: TIfcLabel read FLongName write FLongName;
  end;

  TIfcRelationship = class abstract(TIfcRoot)
  end;

  { Relationship that connects objects under some criteria. }
  TIfcRelConnects = class abstract(TIfcRelationship)
  end;

  { Relationship to assign elements to a certain level of the spatial project structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelContainedInSpatialStructure.htm }
  TIfcRelContainedInSpatialStructure = class(TIfcRelConnects)
  private
    FRelatedElements: TIfcProductList;
    FRelatingStructure: TIfcSpatialElement;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { Set of products, which are contained within this level of
      the spatial structure hierarchy. }
    property RelatedElements: TIfcProductList read FRelatedElements write FRelatedElements;

    { Spatial structure element, within which the element is contained. }
    property RelatingStructure: TIfcSpatialElement read FRelatingStructure write FRelatingStructure;
  end;
  TIfcRelContainedInSpatialStructureList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelContainedInSpatialStructure>;

  { Spatial structure can be used to provide a spatial organization of a project. }
  TIfcSpatialStructureElement = class abstract(TIfcSpatialElement)
  private
    // TODO
    FContainsElements: TIfcRelContainedInSpatialStructureList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property ContainsElements: TIfcRelContainedInSpatialStructureList read FContainsElements write FContainsElements;
  end;

  { Defined area of land on which the project construction is to be completed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSite.htm }
  TIfcSite = class(TIfcSpatialStructureElement)
    // TODO
  end;

  { Any built facility, like a building, bridge. }
  TIfcFacility = class(TIfcSpatialStructureElement)
  end;

  { Building facility.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuilding.htm }
  TIfcBuilding = class(TIfcFacility)
  end;

  { Storey (~floor).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingStorey.htm }
  TIfcBuildingStorey = class(TIfcSpatialStructureElement)
  private
    FElevation: TIfcLengthMeasure;
  published
    { Elevation of the base of this storey, relative to the 0,00
      internal reference height of the building. }
    property Elevation: TIfcLengthMeasure read FElevation write FElevation;
  end;

  TIfcRelDecomposes = class abstract(TIfcRelationship)
  end;

  { Generalization of all components that make up a facility. }
  TIfcElement = class abstract(TIfcProduct)
  private
    FHasOpenings: TIfcRelVoidsElementList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { List of openings associated with this element. }
    property HasOpenings: TIfcRelVoidsElementList read FHasOpenings write FHasOpenings;
  end;

  { All existence dependent elements which modify the shape and appearance
    of the associated master element.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFeatureElement.htm }
  TIfcFeatureElement = class abstract(TIfcElement)
  end;

  { Existence dependent element which modifies the shape and appearance
    of the associated master element. }
  TIfcFeatureElementSubtraction = class abstract(TIfcFeatureElement)
  private
    FVoidsElements: TIfcRelVoidsElementList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { Reference to the Voids Relationship that uses this Opening Element
      to create a void within an Element. }
    property VoidsElements: TIfcRelVoidsElementList read FVoidsElements write FVoidsElements;
  end;

  { Relationship between an opening element and an element that fills
    (or partially fills) the opening element. It is an one-to-one relationship.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelFillsElement.htm }
  TIfcRelFillsElement = class(TIfcRelConnects)
  private
    FRelatingOpeningElement: TIfcOpeningElement;
    FRelatedBuildingElement: TIfcElement;
  published
    { Opening Element being filled by virtue of this relationship. }
    property RelatingOpeningElement: TIfcOpeningElement read FRelatingOpeningElement write FRelatingOpeningElement;
    { Reference to element that occupies fully or partially the associated opening. }
    property RelatedBuildingElement: TIfcElement read FRelatedBuildingElement write FRelatedBuildingElement;
  end;
  TIfcRelFillsElementList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelFillsElement>;

  { Used by TIfcOpeningElement.PredefinedType. }
  TIfcOpeningElementTypeEnum = (NotDefined, Opening, Recess, UserDefined);

  { Opening: a void within any element that has physical manifestation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOpeningElement.htm }
  TIfcOpeningElement = class(TIfcFeatureElementSubtraction)
  private
    FPredefinedType: TIfcOpeningElementTypeEnum;
    FHasFillings: TIfcRelFillsElementList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PredefinedType: TIfcOpeningElementTypeEnum read FPredefinedType write FPredefinedType;
    property HasFillings: TIfcRelFillsElementList read FHasFillings write FHasFillings;
  end;

  { Part of the construction of a built facility, built elements are all
    physically existent and tangible things.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuiltElement.htm }
  TIfcBuiltElement = class(TIfcElement)
  end;

  { Used for TIfcWindow.PredefinedType. }
  TIfcWindowTypeEnum = (NotDefined, LightDome, SkyLight, Window, UserDefined);

  { Basic configuration of the window type in terms of the number of window
    panels and the subdivision of the total window.
    Used for TIfcWindow.PartitioningType.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindowTypePartitioningEnum.htm }
  TIfcWindowTypePartitioningEnum = (
    NotDefined,
    DoublePanelHorizontal,
    DoublePanelVertical,
    SinglePanel,
    TriplePanelBottom,
    TriplePanelHorizontal,
    TriplePanelLeft,
    TriplePanelRight,
    TriplePanelTop,
    TriplePanelVertical,
    UserDefined
  );

  { Window building element.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindow.htm }
  TIfcWindow = class(TIfcBuiltElement)
  private
    FOverallHeight: TIfcPositiveLengthMeasure;
    FOverallWidth: TIfcPositiveLengthMeasure;
    FPredefinedType: TIfcWindowTypeEnum;
    FPartitioningType: TIfcWindowTypePartitioningEnum;
    FUserDefinedPartitioningType: TIfcLabel;
  published
    { Overall measure of the height, it reflects the Z Dimension of
      a bounding box, enclosing the window opening. }
    property OverallHeight: TIfcPositiveLengthMeasure read FOverallHeight write FOverallHeight;

    { Overall measure of the width, it reflects the X Dimension of
      a bounding box, enclosing the window opening. }
    property OverallWidth: TIfcPositiveLengthMeasure read FOverallWidth write FOverallWidth;

    { Predefined generic type for a window that is specified in an enumeration.
      By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PredefinedType: TIfcWindowTypeEnum read FPredefinedType write FPredefinedType;

    { General layout of the window in terms of the partitioning of panels.
      By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PartitioningType: TIfcWindowTypePartitioningEnum read FPartitioningType write FPartitioningType;

    { Shall only be provided, if the value of PartitioningType is set to
      TIfcWindowTypePartitioningEnum.NotDefined. }
    property UserDefinedPartitioningType: TIfcLabel read FUserDefinedPartitioningType write FUserDefinedPartitioningType;
  end;

  { Relationship between a building element and one opening element that
    creates a void in the element. It is a one-to-one relationship.
    This relationship implies a boolean operation of subtraction between
    the geometric bodies of the element and the opening.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelVoidsElement.htm }
  TIfcRelVoidsElement = class(TIfcPersistent)
  private
    FRelatingBuildingElement: TIfcElement;
    FRelatedOpeningElement: TIfcFeatureElementSubtraction;
  published
    property RelatingBuildingElement: TIfcElement read FRelatingBuildingElement write FRelatingBuildingElement;
    property RelatedOpeningElement: TIfcFeatureElementSubtraction read FRelatedOpeningElement write FRelatedOpeningElement;
  end;
  TIfcRelVoidsElementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelVoidsElement>)
  end;

  { General composition/decomposition (or whole/part) relationship.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAggregates.htm }
  TIfcRelAggregates = class(TIfcPersistent)
  private
    FRelatingObject: TIfcObjectDefinition;
    FRelatedObjects: TIfcObjectDefinitionList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property RelatingObject: TIfcObjectDefinition read FRelatingObject write FRelatingObject;
    property RelatedObjects: TIfcObjectDefinitionList read FRelatedObjects write FRelatedObjects;
  end;
  TIfcRelAggregatesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelAggregates>)
  end;

  { Project context in which objects, type objects, property sets,
    and properties are defined.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcContext.htm }
  TIfcContext = class abstract(TIfcObjectDefinition)
  public
    // TODO: rest
    constructor Create; override;
    destructor Destroy; override;
  end;

  { Context for information to be exchanged or shared,
    it may represent a construction project but does not have to.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm }
  TIfcProject = class(TIfcContext)
  end;

  { How to display the product. }
  TIfcRepresentation = class abstract(TIfcPersistent)
  private
    FItems: TIfcRepresentationItemList;
    // TODO rest
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    { Set of geometric representation items that are defined for this representation. }
    property Items: TIfcRepresentationItemList read FItems write FItems;
  end;

  { List of TIfcRepresentation. }
  TIfcRepresentationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentation>)
  end;

  TIfcShapeModel = class abstract(TIfcRepresentation)
    // TODO
  end;

  { Representation of a product or a product component within a specific
    geometric representation context.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShapeRepresentation.htm }
  TIfcShapeRepresentation = class(TIfcShapeModel)
    // TODO
  end;

  { Representation of a product, including its (geometric or topological)
    representation. A product can have zero, one or many geometric representations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProductRepresentation.htm }
  TIfcProductRepresentation = class abstract(TIfcPersistent)
  private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FRepresentations: TIfcRepresentationList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name: TIfcLabel read FName write FName;
    property Description: TIfcText read FDescription write FDescription;
    { Contained list of representations (including shape representations). }
    property Representations: TIfcRepresentationList read FRepresentations write FRepresentations;
  end;

var
  { List of all IFC classes that are defined in this unit. }
  IfcClasses: TIfcPersistentClassList;

{ Make sure IfcClasses is assigned and filled with contents. }
procedure InitializeIfcClasses;

{ Free and nil IfcClasses. }
procedure FinalizeIfcClasses;

{$endif read_interface}

{$ifdef read_implementation}

{ TIfcStyledItem ------------------------------------------------------------- }

constructor TIfcStyledItem.Create;
begin
  inherited;
  Styles := TIfcPresentationStyleList.Create(true);
end;

destructor TIfcStyledItem.Destroy;
begin
  FreeAndNil(FStyles);
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
  FreeAndNil(FAssignedItems);
  inherited;
end;

{ TIfcObjectDefinition ------------------------------------------------------- }

constructor TIfcObjectDefinition.Create;
begin
  inherited;
  IsDecomposedBy := TIfcRelAggregatesList.Create(true);
end;

destructor TIfcObjectDefinition.Destroy;
begin
  FreeAndNil(FIsDecomposedBy);
  inherited;
end;

{ TIfcSpatialStructureElement ------------------------------------------------ }

constructor TIfcSpatialStructureElement.Create;
begin
  inherited;
  ContainsElements := TIfcRelContainedInSpatialStructureList.Create(true);
end;

destructor TIfcSpatialStructureElement.Destroy;
begin
  FreeAndNil(FContainsElements);
  inherited;
end;

{ TIfcElement --------------------------------------------------------------- }

constructor TIfcElement.Create;
begin
  inherited;
  HasOpenings := TIfcRelVoidsElementList.Create(true);
end;

destructor TIfcElement.Destroy;
begin
  FreeAndNil(FHasOpenings);
  inherited;
end;

{ TIfcRelAggregates --------------------------------------------------------- }

constructor TIfcRelAggregates.Create;
begin
  inherited;
  RelatedObjects := TIfcObjectDefinitionList.Create(true);
end;

destructor TIfcRelAggregates.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  inherited;
end;

{ TIfcContext --------------------------------------------------------------- }

constructor TIfcContext.Create;
begin
  inherited;
  IsDecomposedBy := TIfcRelAggregatesList.Create(true);
end;

destructor TIfcContext.Destroy;
begin
  FreeAndNil(FIsDecomposedBy);
  inherited;
end;

{ TIfcOpeningElement -------------------------------------------------------- }

constructor TIfcOpeningElement.Create;

begin
  inherited;
  HasFillings := TIfcRelFillsElementList.Create(true);
end;

destructor TIfcOpeningElement.Destroy;
begin
  FreeAndNil(FHasFillings);
  inherited;
end;

{ TIfcProductRepresentation ------------------------------------------------- }

constructor TIfcProductRepresentation.Create;
begin
  inherited;
  Representations := TIfcRepresentationList.Create(true);
end;

destructor TIfcProductRepresentation.Destroy;
begin
  FreeAndNil(FRepresentations);
  inherited;
end;

{ TIfcRepresentation -------------------------------------------------------- }

constructor TIfcRepresentation.Create;
begin
  inherited;
  Items := TIfcRepresentationItemList.Create(true);
end;

destructor TIfcRepresentation.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TIfcFeatureElementSubtraction --------------------------------------------- }

constructor TIfcFeatureElementSubtraction.Create;
begin
  inherited;
  VoidsElements := TIfcRelVoidsElementList.Create(true);
end;

destructor TIfcFeatureElementSubtraction.Destroy;
begin
  FreeAndNil(FVoidsElements);
  inherited;
end;

{ TIfcRelContainedInSpatialStructure --------------------------------------- }

constructor TIfcRelContainedInSpatialStructure.Create;
begin
  inherited;
  RelatedElements := TIfcProductList.Create(true);
end;

destructor TIfcRelContainedInSpatialStructure.Destroy;
begin
  FreeAndNil(FRelatedElements);
  inherited;
end;

{ initialization, finalization ----------------------------------------------- }

procedure InitializeIfcClasses;
begin
  if IfcClasses = nil then
  begin
    IfcClasses := TIfcPersistentClassList.Create;
    IfcClasses.Add(TIfcPresentationStyle);
    IfcClasses.Add(TIfcPresentationLayerAssignment);
    IfcClasses.Add(TIfcRepresentationItem);
    IfcClasses.Add(TIfcStyledItem);
    IfcClasses.Add(TIfcDirection);
    IfcClasses.Add(TIfcGeometricRepresentationItem);
    IfcClasses.Add(TIfcSolidModel);
    IfcClasses.Add(TIfcSweptAreaSolid);
    IfcClasses.Add(TIfcExtrudedAreaSolid);
    IfcClasses.Add(TIfcRoot);
    IfcClasses.Add(TIfcObjectDefinition);
    IfcClasses.Add(TIfcObject);
    IfcClasses.Add(TIfcObjectPlacement);
    IfcClasses.Add(TIfcPoint);
    IfcClasses.Add(TIfcCartesianPoint);
    IfcClasses.Add(TIfcPlacement);
    IfcClasses.Add(TIfcAxis2Placement2D);
    IfcClasses.Add(TIfcAxis2Placement3D);
    IfcClasses.Add(TIfcAxis2Placement);
    IfcClasses.Add(TIfcLocalPlacement);
    IfcClasses.Add(TIfcProduct);
    IfcClasses.Add(TIfcSpatialElement);
    IfcClasses.Add(TIfcSpatialStructureElement);
    IfcClasses.Add(TIfcSite);
    IfcClasses.Add(TIfcFacility);
    IfcClasses.Add(TIfcBuilding);
    IfcClasses.Add(TIfcBuildingStorey);
    IfcClasses.Add(TIfcRelDecomposes);
    IfcClasses.Add(TIfcElement);
    IfcClasses.Add(TIfcFeatureElement);
    IfcClasses.Add(TIfcFeatureElementSubtraction);
    IfcClasses.Add(TIfcWindow);
    IfcClasses.Add(TIfcRelVoidsElement);
    IfcClasses.Add(TIfcRelFillsElement);
    IfcClasses.Add(TIfcBuiltElement);
    IfcClasses.Add(TIfcOpeningElement);
    IfcClasses.Add(TIfcRelAggregates);
    IfcClasses.Add(TIfcContext);
    IfcClasses.Add(TIfcProject);
    IfcClasses.Add(TIfcRepresentation);
    IfcClasses.Add(TIfcShapeModel);
    IfcClasses.Add(TIfcShapeRepresentation);
    IfcClasses.Add(TIfcProductRepresentation);
  end;
end;

procedure FinalizeIfcClasses;
begin
  FreeAndNil(IfcClasses);
end;

{$endif read_implementation}
