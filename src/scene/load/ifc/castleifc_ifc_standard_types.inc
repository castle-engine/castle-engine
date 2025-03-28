{%MainUnit castleifc.pas}
{
  Copyright 2024-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types and classes that correspond to the IFC standard.

  Overview:

  We strived to express IFC concepts using the most straghtforward / natural
  Pascal concepts.

  - So most IFC types and their inheritance are naturally expressed
    using Pascal classes and their inheritance.

  - The exceptions are:
    - Simple types: strings, integers, floats. These map directly to Pascal types.
    - Enumerations: they map to Pascal enums.
    - Sets (lists): are expressed using generic TObjectList<xxx> descendants.
      With the exception of string lists, which are TCastleStringList.

  - Everything is RTTI friendly (classes have their properties published,
    "default" and "stored" matter etc.), to allow our JSON
    serialization / deserialization
    use RTTI to handle all IFC classes in a uniform fashion.

  Details how everything is defined:

  - All IFC classes descend from TIfcPersistent.
    It right now descends from TComponent,
    all IFC classes will be owned by TIfcFile instance when read by IfcJsonLoad.

  - Class name corresponds to IFC class name, just add "T" prefix.

    Moreover, if class name ends with "List", add "_Spec" suffix,
    so that we can have both e.g. TIfcCartesianPointList (Pascal TObjectList)
    and TIfcCartesianPointList_Spec (IFC class IfcCartesianPointList).

    Moreover, some classes have "_Boxed" prefix because they exist both
    as classes, and as simple types. The latter is useful when
    TIfcValue is expected.

    Our TIfcPersistent.IfcClassName is prepared to handle these cases already,
    so you don't need to override it.

  - For all IFC fields:

    - Place all fields with F in "strict private" section.

    - Publish them as properties in "published" section.

    - Use "default" and "stored" as usual in Pascal.
      When the property equals default, or is not stored,
      it will not be serialized to IFC JSON.

      Not using "default" (or "stored") means it is serialized always to IFC JSON.
      (Usual Pascal rules for "default" apply, e.g. all Strings implicitly
      have default '' always.)

    - Properties of any type supported by TPropertyType
      (from CastleInternalRttiUtils) are supported.
      This means
      - integers,
      - booleans,
      - floats,
      - strings,
      - enums,
      - properties that refer to an optional instance of another IFC class.
      - properties that are a list of instances of another IFC class.

    - All properties *except* lists should be read-write.
      Lists should be read-only.

    - The list properties (that are JSON arrays) should be classes
      equal to, or descending from, TObjectList<Xxx> descendants.

      If it descends from TObjectList<Xxx>, keep the name with suffix "List".
      The name of class is actually important for 2 hacks we need to do in JSON
      (de)serialization, so make sure it is exactly the child class name + "List".

      Their published property should be read-only.
      (This avoids mistakes like reinitializing the list in descendant,
      since the property will be read-only even in descendant.)

      The list instance has to be created in constructor,
      with "false" parameter so it *does not* own the objects.
      We use TComponent ownership for this, as IFC is a graph
      (even with cycles), and "freeing everyting along with one single owner"
      is the simplest reliable solution to manage memory.

      The list instance is FreeAndNil'ed in destructor.

      Supported lists are TObjectList<TIfcXxx> descendants,
      and TCastleStringList (for lists of strings, like IfcLabel lists).

    - The vector properties should be treated using
      TCastleVector3PersistentSimple,
      TCastleVector4PersistentSimple (unused for now) etc.

      This is similar to regular CGE components,
      see https://castle-engine.io/custom_components#_publishing_vectors_and_colors ,
      but a bit simpler: we don't need to define a TVector3 in public section,
      we define TCastleVector3PersistentSimple in published section,
      and user code can access it like "MyIfcDirection.DirectionRatios.Value".

  - Enumerated values in Pascal should reflect IFC.

    Make them PascalCase for easier reading, but keep the underscores.
    That is, enum values must match IFC spec, but not in case-sensitive manner
    (because GetEnumValue and CGE PropertySetEnumStr do not care about case).

    Change the order of enum values compared to IFC,
    to make things easier: first enum value should be the default.
    This allows to have simpler constructors,
    no need to initialize enum properties.
    This in practice usually means you should place the last IFC value
    ("NotDefined" or "UserDefined", usually) as the first enum value in Pascal.

  These classes express:

  - Everything from the IFC standard that we handle.

  - Moreover, they contain methods to build/update nodes used
    by Castle Game Engine to display given IFC entities.
    These methods are implemented in castleifc_load_to_x3d.inc
    yet they are defined in this file, inside IFC classes.
    Havine these methods inside IFC classes is beneficial,
    as we can use IFC classes inheritance naturally (e.g. some "build
    nodes" methods extend ancestor in natural way).
}

{ Define all inverse properties from IFC.
  Keep defined, undefine only for testing now. }
{$define CASTLE_IFC_INVERSE_PROPS}

{$ifdef read_interface_types}

  // forward declarations
  TIfcRepresentationItem = class;
  TIfcStyledItem = class;
  TIfcRelAggregatesList = class;
  TIfcProductList = class;
  TIfcRelVoidsElement = class;
  TIfcRelVoidsElementList = class;
  TIfcOpeningElement = class;
  TIfcRepresentationList = class;
  TIfcAddressList = class;
  TIfcOrganization = class;
  TIfcOrganizationList = class;
  TIfcPerson = class;
  TIfcPersonAndOrganizationList = class;
  TIfcOrganizationRelationshipList = class;
  TIfcCartesianPointList = class;
  TIfcProductRepresentation = class;
  TIfcRelDeclares = class;
  TIfcDefinitionSelectList = class;
  TIfcContext = class;
  TIfcObjectPlacementList = class;
  TIfcRelDeclaresList = class;
  TIfcRepresentationContextList = class;
  TIfcRelDefinesByTypeList = class;
  TIfcExternalReference = class;
  TIfcObject = class;
  TIfcObjectList = class;
  TIfcRelDefinesByPropertiesList = class;
  TIfcRelDefinesByTemplateList = class;
  TIfcShapeAspectList = class;
  TIfcPoint = class;
  TIfcRepresentation = class;
  TIfcProductRepresentationList = class;
  TIfcProfilePropertiesList = class;
  TIfcPropertyList = class;
  TIfcRelContainedInSpatialStructureList = class;
  TIfcRelFillsElementList = class;
  TIfcElement = class;
  TIfcElementList = class;
  TIfcTypeObjectList = class;
  TIfcPropertySetTemplateList = class;
  TIfcPropertyTemplateList = class;
  TIfcProperty = class;
  TIfcComplexPropertyList = class;
  TIfcMaterial = class;
  TIfcMaterialList = class;
  TIfcMaterialConstituentSet = class;
  TIfcMaterialDefinition = class;
  TIfcMaterialPropertiesList = class;
  TIfcMaterialLayerSet = class;
  TIfcGeometricRepresentationContext = class;
  TIfcGeometricRepresentationSubContextList = class;
  TIfcProduct = class;
  TIfcCartesianPoint = class;
  TIfcFeatureElementSubtraction = class;
  TIfcRepresentationMap = class;
  TIfcPolygonalFaceSetList = class;
  TIfcIndexedPolygonalFace = class;
  TIfcTextureCoordinateList = class;
  TIfcCartesianTransformationOperator2D = class;
  TIfcSurfaceTextureList = class;
  TIfcTessellatedFaceSet = class;
  TIfcTextureCoordinateIndicesList = class;
  TIfcRelContainedInSpatialStructure = class;
  TIfcRelPositionsList = class;
  TIfcGridAxisList = class;
  TIfcGrid = class;
  TIfcRelAssociatesMaterialList = class;
  TIfcMaterialProfileSet = class;
  TIfcNamedUnit = class;

  // forward declarations resolved in ..._autogenerated.inc
  TIfcIndexedColourMap = class;
  TIfcCoordinateOperation = class;
  TIfcFeatureElementAddition = class;
  TIfcRelReferencedInSpatialStructure = class;

  { Float type used throughout IFC.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReal.htm }
  TIfcReal = Single;

  { Positive distance.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPositiveLengthMeasure.htm }
  TIfcPositiveLengthMeasure = Single;

  { Ratio measure that is greater than zero.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPositiveRatioMeasure.htm }
  TIfcPositiveRatioMeasure = Single;

  { Distance.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLengthMeasure.htm }
  TIfcLengthMeasure = Single;

  TIfcNonNegativeLengthMeasure = Single;

  TIfcAreaMeasure = Single;

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

  { Indication of date and time by measuring the number of seconds
    which have elapsed since 1 January 1970, 00:00:00 UTC.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTimeStamp.htm}
  TIfcTimeStamp = Int64;

  TIfcURIReference = String;

  { Dimensionless measure to express ratio values ranging from 0.0 to 1.0.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcNormalisedRatioMeasure.htm }
  TIfcNormalisedRatioMeasure = Single;

  TIfcLogical = Boolean;

  { Presentation information assigned to geometric representation items.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationStyle.htm }
  TIfcPresentationStyle = class(TIfcPersistent)
  strict private
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
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FAssignedItems: TIfcLayeredItemList;
    FIdentifier: TIfcIdentifier;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Name of the layer. }
    property Name: TIfcLabel read FName write FName;

    { Additional description of the layer. }
    property Description: TIfcText read FDescription write FDescription;

    { The set of layered items, which are assigned to this layer.
      Always at least 1 element. }
    property AssignedItems: TIfcLayeredItemList read FAssignedItems;

    { An (internal) identifier assigned to the layer. }
    property Identifier: TIfcIdentifier read FIdentifier write FIdentifier;
  end;
  TIfcPresentationLayerAssignmentList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPresentationLayerAssignment>)
  end;

  { Geometric or topological representation items,
    that can (but not need to) have presentation style information assigned.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRepresentationItem.htm }
  TIfcRepresentationItem = class(TIfcPersistent)
  strict private
    FLayerAssignment: TIfcPresentationLayerAssignment;
    FStyledByItem: TIfcStyledItem;
  public
    { Build a node to display this using Castle Game Engine.
      Returns @nil if not possible / not implemented.

      @param(ParentProduct Parent TIfcProduct, to be used with
        TCastleIfcMapping.ConnectShapeAndProduct call.

        It can be @nil (and TCastleIfcMapping.ConnectShapeAndProduct
        is already prepared to handle it too) if no particular TIfcProduct
        is parent of this, which (right now) only happens for some internal
        usage in UpdateCrossSection for throw-away shape instances.
      )

      @param(LitAppearance Appearance that should be used for all shapes created
        here, if they are supposed to be lit (made of polygons).
        May be given as @nil.)

      @param(UnlitAppearance Appearance that should be used for all shapes created
        here, if they are supposed to be unlit (made of lines, points).
        May be given as @nil.)
    }
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; virtual;
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
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
  end;

  { Style information for products.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStyledItem.htm }
  TIfcStyledItem = class(TIfcRepresentationItem)
  strict private
    FItem: TIfcRepresentationItem;
    FStyles: TIfcPresentationStyleList;
    FName: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { A geometric representation item to which the style is assigned.
      May be @nil. }
    property Item: TIfcRepresentationItem read FItem write FItem;

    { Representation styles which are assigned,
      either to an geometric representation item, or to a material definition.
      Always has at least 1 element. }
    property Styles: TIfcPresentationStyleList read FStyles;

    { The word, or group of words, by which the styled item is referred to. }
    property Name: TIfcLabel read FName write FName;
  end;

  { Direction in 2D or 3D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDirection.htm }
  TIfcDirection = class(TIfcPersistent)
  strict private
    FDirectionRatios: TCastleVector3PersistentSimple;

    { Not necessary in CGE now:
      Dim is not explicitly specified in IFC, but it is derived from
      the contents of IFC. Needs special logic, if ever needed in CGE. }
    // FDim: TIfcDimensionCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Create instance with given direction ratios. }
    constructor CreateVector(const AOwner: TComponent; const AVector: TVector3);

    { Not necessary in CGE now:
      The number of dimensions specified in the IFC file, 2 or 3.
      Regardless of it, DirectionRatios always has the Z value defined:
      we set Z to 0 when it is not defined in IFC. }
    // property Dim: TIfcDimensionCount read FDim write FDim;
  published
    { Defined as 2 or 3 values of TIfcReal type.
      In practice, this is just a 3D vector for our engine. }
    property DirectionRatios: TCastleVector3PersistentSimple read FDirectionRatios;
  end;

  { Common supertype of all geometric items used within a representation.
    It is positioned within a geometric coordinate system,
    directly or indirectly through intervening items.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricRepresentationItem.htm }
  TIfcGeometricRepresentationItem = class abstract(TIfcRepresentationItem)
  end;

  TIfcBoundingBox = class(TIfcGeometricRepresentationItem)
  strict private
    FCorner: TIfcCartesianPoint;
    FXDim: TIfcPositiveLengthMeasure;
    FYDim: TIfcPositiveLengthMeasure;
    FZDim: TIfcPositiveLengthMeasure;
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { Location of the bottom left corner (having the minimum values). }
    property Corner: TIfcCartesianPoint read FCorner write FCorner;

    { Length attribute (measured along the edge parallel to the X Axis). }
    property XDim: TIfcPositiveLengthMeasure read FXDim write FXDim;

    { Width attribute (measured along the edge parallel to the Y Axis). }
    property YDim: TIfcPositiveLengthMeasure read FYDim write FYDim;

    { Height attribute (measured along the edge parallel to the Z Axis). }
    property ZDim: TIfcPositiveLengthMeasure read FZDim write FZDim;
  end;

  { Abstract supertype of all tessellated geometric models.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTessellatedItem.htm }
  TIfcTessellatedItem = class abstract(TIfcGeometricRepresentationItem)
  strict private
    FLayerAssignment: TIfcPresentationLayerAssignmentList;
    FStyledByItem: TIfcStyledItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Assignment of the representation item to a single or multiple layer(s). }
    property LayerAssignment: TIfcPresentationLayerAssignmentList
      read FLayerAssignment;

    { Reference to the IfcStyledItem that provides presentation information
      to the representation, e.g. a curve style, including colour
      and thickness to a geometric curve. }
    property StyledByItem: TIfcStyledItem read FStyledByItem write FStyledByItem;
  end;

  { Abstract supertype of list of points.

    Note: The name in Pascal has suffix "Spec" (as for "specification class")
    to differentiate from @link(TIfcCartesianPointList) which is a list of
    @link(TIfcCartesianPoint) using @code(TObjectList<TIfcCartesianPoint>).
    We need both list types. Luckily, we can name this class however we want,
    as it's abstract and thus not used in IFC files.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianPointList.htm }
  TIfcCartesianPointList_Spec = class abstract(TIfcGeometricRepresentationItem)
  public
    procedure UpdateCoordinate(const Coord: TCoordinateNode); virtual; abstract;
  end;

  { Ordered collection of two-dimentional Cartesian points.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianPointList2D.htm }
  TIfcCartesianPointList2D = class(TIfcCartesianPointList_Spec)
  strict private
    FCoordList: TVector2List;
    FTagList: TCastleStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCoordinate(const Coord: TCoordinateNode); override;
  published
    { Lst of Cartesian points provided by two coordinates. }
    property CoordList: TVector2List read FCoordList;

    { List of tags corresponding to each point that may be used to
      identify a basis curve according to the Tag attribute at
      IfcOffsetCurveByDistances. }
    property TagList: TCastleStringList read FTagList;
  end;

  { Ordered collection of three-dimentional Cartesian points.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianPointList3D.htm }
  TIfcCartesianPointList3D = class(TIfcCartesianPointList_Spec)
  strict private
    FCoordList: TVector3List;
    FTagList: TCastleStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCoordinate(const Coord: TCoordinateNode); override;
  published
    { Lst of Cartesian points provided by three coordinates. }
    property CoordList: TVector3List read FCoordList;

    { List of tags corresponding to each point that may be used to
      identify a basis curve according to the Tag attribute at
      IfcOffsetCurveByDistances. }
    property TagList: TCastleStringList read FTagList;
  end;

  { Abstract supertype of all entities used for presentation appearance definitions. }
  TIfcPresentationItem = class abstract(TIfcPersistent)
  end;

  { Include image textures in surface styles.
    These image textures can be applied repeating across the surface
    or mapped with a particular scale upon the surface.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyleWithTextures.htm }
  TIfcSurfaceStyleWithTextures = class(TIfcPresentationItem)
  strict private
    FTextures: TIfcSurfaceTextureList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The textures applied to the surface. In case of more than one
      surface texture is included, the IfcSurfaceStyleWithTextures defines
      a multi texture. }
    property Textures: TIfcSurfaceTextureList read FTextures;
  end;
  TIfcSurfaceStyleWithTexturesList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcSurfaceStyleWithTextures>;

  { 2-dimensional image-based texture map.
    It can either be given by referencing an external image file through an
    URL reference (@link(TIfcImageTexture)),
    including the image file as a blob (long binary) into the data set
    (@link(TIfcBlobTexture)),
    or by explicitly including an array of pixels (@link(TIfcPixelTexture)). }
  TIfcSurfaceTexture = class abstract(TIfcPresentationItem)
  strict private
    FRepeatS: Boolean;
    FRepeatT: Boolean;
    FMode: TIfcIdentifier;
    FTextureTransform: TIfcCartesianTransformationOperator2D;
    FParameter: TCastleStringList;
    FIsMappedBy: TIfcTextureCoordinateList;
    FUsedInStyles: TIfcSurfaceStyleWithTexturesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { How the texture wraps in the S direction, @true by default. }
    property RepeatS: Boolean read FRepeatS write FRepeatS default true;

    { How the texture wraps in the T direction, @true by default. }
    property RepeatT: Boolean read FRepeatT write FRepeatT default true;

    { Hold the type of map this corresponds to, chosen from the following list:
      AMBIENT, NORMAL, EMISSIVE, METALLICROUGHNESS, OCCLUSION, SHININESS, SPECULAR, DIFFUSE.
      If Mode is omitted, DIFFUSE is assumed. }
    property Mode: TIfcIdentifier read FMode write FMode;

    { 2D transformation that is applied to the texture coordinates.
      It affects the way texture coordinates are applied to the surfaces
      of geometric representation items.
      The 2D transformation supports changes to the size, orientation,
      and position of textures on shapes. Mirroring is not allowed to be used
      in the IfcCartesianTransformationOperator. }
    property TextureTransform: TIfcCartesianTransformationOperator2D read FTextureTransform write FTextureTransform;

    { This attribute is deprecated and shall no longer be used. }
    property Parameter: TCastleStringList read FParameter write FParameter;

    { Texture coordinates, either provided by a corresponding list of texture vertices
      to vertex-based geometric items or by a texture coordinate generator,
      that applies the surface texture to the surfaces of the geometric items. }
    property IsMappedBy: TIfcTextureCoordinateList read FIsMappedBy write FIsMappedBy;

    property UsedInStyles: TIfcSurfaceStyleWithTexturesList read FUsedInStyles;
  end;
  TIfcSurfaceTextureList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcSurfaceTexture>)
  end;

  { 2-dimensional texture, defined by an URL.

    See https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcImageTexture.htm }
  TIfcImageTexture = class(TIfcSurfaceTexture)
  strict private
    FURLReference: TIfcURIReference;
  published
    { Location, provided as an URI, at which the image texture is
      electronically published. }
    property URLReference: TIfcURIReference read FURLReference write FURLReference;
  end;

  { An abstract supertype of the different kinds to apply texture coordinates
    to geometries. For vertex based geometries an explicit assignment
    of 2D texture vertices to the 3D geometry points is supported by
    the subtype IfcTextureMap, in addition there can be a procedural
    description of how texture coordinates shall be applied to geometric items.
    If no IfcTextureCoordinate is provided for the IfcSurfaceTexture,
    the default mapping shall be used.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureCoordinate.htm }
  TIfcTextureCoordinate = class abstract(TIfcPresentationItem)
  strict private
    FMaps: TIfcSurfaceTextureList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the one (or many in case of multi textures
      with identity transformation to geometric surfaces) subtype(s)
      of @link(TIfcSurfaceTexture) that are mapped to a geometric
      surface by the texture coordinate transformation. }
    property Maps: TIfcSurfaceTextureList read FMaps;
  end;
  TIfcTextureCoordinateList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcTextureCoordinate>)
  end;

  { Ordered collection of 2D texture coordinates.

    Name in Pascal is with "_Spec" suffix to clarify this is "specification
    defined list-like class" and not just TObjectList<TIfcTextureVertexList>.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureVertexList.htm }
  TIfcTextureVertexList_Spec = class(TIfcPresentationItem)
  strict private
    FTexCoordsList: TVector2List;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { List of texture vertices defined by S-coordinate and T-coordinate. }
    property TexCoordsList: TVector2List read FTexCoordsList;
  end;

  { Mapping of the 2-dimensional texture coordinates to the surface onto which
    it is mapped.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedTextureMap.htm }
  TIfcIndexedTextureMap = class abstract(TIfcTextureCoordinate)
  strict private
    FMappedTo: TIfcTessellatedFaceSet;
    FTexCoords: TIfcTextureVertexList_Spec;
  published
    { Reference to the IfcTessellatedFaceSet to which it applies
      the texture map. }
    property MappedTo: TIfcTessellatedFaceSet read FMappedTo write FMappedTo;

    { Indexable list of texture vertices. }
    property TexCoords: TIfcTextureVertexList_Spec read FTexCoords write FTexCoords;
  end;
  TIfcIndexedTextureMapList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcIndexedTextureMap>;

  { Mapping of the 2-dimensional texture coordinates to a set of polygonal
    bounded faces onto which it is mapped. It is used for mapping the texture
    to faces of an IfcPolygonalFaceSet. Such faces may have inner loops.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedPolygonalTextureMap.htm }
  TIfcIndexedPolygonalTextureMap = class(TIfcIndexedTextureMap)
  strict private
    FTexCoordIndices: TIfcTextureCoordinateIndicesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Set of texture coordinate indices for polygonal faces
      with and without inner loops. }
    property TexCoordIndices: TIfcTextureCoordinateIndicesList read FTexCoordIndices;
  end;

  { Provide the texture coordinates for an @link(TIfcIndexedPolygonalFace).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureCoordinateIndices.htm }
  TIfcTextureCoordinateIndices = class(TIfcPersistent)
  strict private
    FTexCoordIndex: TIntegerList;
    FTexCoordsOf: TIfcIndexedPolygonalFace;
    FToTexMap: TIfcIndexedPolygonalTextureMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { List of index pointers into the @link(TIfcTextureVertexList_Spec). }
    property TexCoordIndex: TIntegerList read FTexCoordIndex;

    { The @link(TIfcIndexedPolygonalFace) for which the texture
      coordinate are provided. }
    property TexCoordsOf: TIfcIndexedPolygonalFace read FTexCoordsOf write FTexCoordsOf;

    property ToTexMap: TIfcIndexedPolygonalTextureMap read FToTexMap write FToTexMap;
  end;
  TIfcTextureCoordinateIndicesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcTextureCoordinateIndices>)
  end;

  { Representation of a planar face being part of a face set.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedPolygonalFace.htm }
  TIfcIndexedPolygonalFace = class(TIfcTessellatedItem)
  strict private
    FCoordIndex: TIntegerList;
    FToFaceSet: TIfcPolygonalFaceSetList;
    FHasTexCoords: TIfcTextureCoordinateIndices;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { One-dimensional list with the indices for the three or more points,
      that define the vertices of the outer loop. If the tessellated face
      set is closed, indicated by @link(TIfcPolygonalFaceSet.Closed),
      then the points, defining the outer loop, shall connect counter clockwise,
      as seen from the outside of the body, so that the resulting normal
      will point outwards. }
    property CoordIndex: TIntegerList read FCoordIndex;

    { Reference to the IfcPolygonalFaceSet for which this face is associated. }
    property ToFaceSet: TIfcPolygonalFaceSetList read FToFaceSet;

    { Optional reference to the IfcTextureCoordinateIndices that provide
      the texture coordinates for applying textures to this face. }
    property HasTexCoords: TIfcTextureCoordinateIndices read FHasTexCoords write FHasTexCoords;
  end;
  TIfcIndexedPolygonalFaceList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcIndexedPolygonalFace>;

  { Topological model limited to planar faces and straight edges.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTessellatedFaceSet.htm }
  TIfcTessellatedFaceSet = class abstract(TIfcTessellatedItem)
  strict private
    FCoordinates: TIfcCartesianPointList3D;
    FHasColours: TIfcIndexedColourMap;
    FHasTextures: TIfcIndexedTextureMapList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Ordered list of Cartesian points. }
    property Coordinates: TIfcCartesianPointList3D read FCoordinates write FCoordinates;

    { Reference to the indexed colour map providing the corresponding
      colour RGB values to the faces. }
    property HasColours: TIfcIndexedColourMap read FHasColours write FHasColours;

    { Reference to the indexed texture map providing the corresponding
      texture coordinates to the vertices. }
    property HasTextures: TIfcIndexedTextureMapList read FHasTextures;
  end;

  { Tessellated face set with all faces being bound by polygons.
    This is what is most commonly referred to as "a mesh" in 3D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPolygonalFaceSet.htm }
  TIfcPolygonalFaceSet = class(TIfcTessellatedFaceSet)
  strict private
    FClosed: Boolean;
    FFaces: TIfcIndexedPolygonalFaceList;
    FPnIndex: TIntegerList;
  protected
    function WantsGlobalId: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { Indication whether the face set is a closed shell (TRUE)
      or an open shell (FALSE).

      IFC spec says: If omitted no such information can be asserted.
      For our implementation, "no such information can be asserted" means
      it shall be treated as an open shell, so we don't introduce a 3rd state. }
    property Closed: Boolean read FClosed write FClosed default false;

    { List of polygonal faces. }
    property Faces: TIfcIndexedPolygonalFaceList read FFaces;

    { The list of integers defining the locations in the IfcCartesianPointList3D
      to obtain the point coordinates for the indices at the indexed
      polygonal faces. If the PnIndex is not provided the indices
      at the indexed polygonal faces point directly into the IfcCartesianPointList3D. }
    property PnIndex: TIntegerList read FPnIndex;
  end;
  TIfcPolygonalFaceSetList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPolygonalFaceSet>)
  end;

  { Tessellated face set with all faces being bound by triangles.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTriangulatedFaceSet.htm }
  TIfcTriangulatedFaceSet = class(TIfcTessellatedFaceSet)
  strict private
    FNormals: TVector3List;
    FClosed: Boolean;
    FCoordIndex: TVector3IntegerList;
    FPnIndex: TIntegerList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { Ordered list of three directions for normals. }
    property Normals: TVector3List read FNormals;

    { Indication whether the face set is a closed shell (@true)
      or an open shell (@false).

      IFC spec says: If omitted no such information can be asserted.
      For our implementation, "no such information can be asserted" means
      it shall be treated as an open shell, so the default is just @false,
      we don't introduce a 3rd state. }
    property Closed: Boolean read FClosed write FClosed default false;

    { List of indices for the vertices of the triangles. }
    property CoordIndex: TVector3IntegerList read FCoordIndex;

    { The list of integers defining the locations in the
      @link(TIfcCartesianPointList3D) to obtain the point coordinates for
      the indices withint the CoordIndex. If the PnIndex is not provided
      the indices point directly into the @link(TIfcCartesianPointList3D). }
    property PnIndex: TIntegerList read FPnIndex;
  end;

  { Curve in 2D or 3D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurve.htm }
  TIfcCurve = class abstract(TIfcGeometricRepresentationItem)
  public
    { Update CrossSection to be used by IfcExtrudedAreaSolid.
      In the base class, this reuses BuildNode and extracts cross section
      information from a temporary node.
      Override to provide more optimized implementation.

      Set CrossSectionConvex to @false if the cross-section
      is potentially concave. By default we assume faster @true, so convex. }
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean); virtual;

    { Space dimensionality of this abstract class, defined differently for all
      subtypes. }
    // Not necessary in CGE now: derived: function Dim: TIfcDimensionCount; abstract;
  end;
  TIfcCurveList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcCurve>;

  TIfcBoundedCurve = class abstract(TIfcCurve)
  end;

  { Preferred way of trimming.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTrimmingPreference.htm }
  TIfcTrimmingPreference = (
    Unspecified,
    Cartesian,
    Parameter
  );

  { Bounded curve that is trimmed at both ends.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTrimmedCurve.htm }
  TIfcTrimmedCurve = class(TIfcBoundedCurve)
  strict private
    FBasisCurve: TIfcCurve;
    FTrim1: TIfcCartesianPointList;
    FTrim2: TIfcCartesianPointList;
    FSenseAgreement: Boolean;
    FMasterRepresentation: TIfcTrimmingPreference;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { The curve to be trimmed. For curves with multiple representations
      any parameter values given as Trim1 or Trim2 refer to the
      master representation of the BasisCurve only. }
    property BasisCurve: TIfcCurve read FBasisCurve write FBasisCurve;

    { The first trimming point.
      TODO: We support only TIfcCartesianPoint, but IFC spec allows more,
      IfcTrimmingSelect. }
    property Trim1: TIfcCartesianPointList read FTrim1;

    { The second trimming point.
      TODO: We support only TIfcCartesianPoint, but IFC spec allows more,
      IfcTrimmingSelect. }
    property Trim2: TIfcCartesianPointList read FTrim2;

    { Flag to indicate whether the direction of the trimmed curve agrees
      with or is opposed to the direction of the basis curve. }
    property SenseAgreement: Boolean
      read FSenseAgreement write FSenseAgreement default false;

    { Where both parameter and point are present at either end of the curve
      this indicates the preferred form. }
    property MasterRepresentation: TIfcTrimmingPreference
      read FMasterRepresentation write FMasterRepresentation
      default TIfcTrimmingPreference.Unspecified;
  end;

  { Bounded curve with only linear segments defined by a list of Cartesian points.
    If the first and the last Cartesian point in the list are identical,
    then the polyline is a closed curve, otherwise it is an open curve.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPolyline.htm }
  TIfcPolyline = class(TIfcBoundedCurve)
  strict private
    FPoints: TIfcCartesianPointList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean); override;
  published
    { Points defining the polyline. }
    property Points: TIfcCartesianPointList read FPoints;
  end;

  { Choice between IfcArcIndex and IfcLineIndex. }
  TIfcSegmentIndexSelect = class(TIfcPersistent)
  end;
  TIfcSegmentIndexSelectList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcSegmentIndexSelect>;

  { Single or multiple straight segments within a poly curve.
    Can have 2 or more points.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLineIndex.htm}
  TIfcLineIndex = class(TIfcSegmentIndexSelect)
  strict private
    FValue: TIntegerList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Value: TIntegerList read FValue;
  end;

  { Single circular arc segment within a poly curve.
    Should always have 3 points.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcArcIndex.htm }
  TIfcArcIndex = class(TIfcSegmentIndexSelect)
  strict private
    FValue: TIntegerList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Value: TIntegerList read FValue;
  end;

  { Bounded curve with only linear and circular arc segments defined by a
    Cartesian point list and an optional list of segments.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedPolyCurve.htm }
  TIfcIndexedPolyCurve = class(TIfcBoundedCurve)
  strict private
    FPoints: TIfcCartesianPointList_Spec;
    FSegments: TIfcSegmentIndexSelectList;
    FSelfIntersect: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { Points defining the polyline. }
    property Points: TIfcCartesianPointList_Spec read FPoints write FPoints;

    { List of straight line and circular arc segments, each providing a list
      of indices into the Cartesian point list. }
    property Segments: TIfcSegmentIndexSelectList read FSegments;

    { Indication of whether the curve intersects itself or not;
      this is for information only. }
    property SelfIntersect: Boolean read FSelfIntersect write FSelfIntersect default false;
  end;

  { 3D shape.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSolidModel.htm }
  TIfcSolidModel = class abstract(TIfcGeometricRepresentationItem)
  const
    { The space dimensionality of this class, it is always 3. }
    Dim = 3;
  end;

  { Selection of resource level objects that are to be related to
    an resource level relationship object.
    This must be one of the classes listed in spec on
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResourceObjectSelect.htm }
  TIfcResourceObjectSelect = TIfcPersistent;

  { Base entity for relationships between resource-level entities.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResourceLevelRelationship.htm }
  TIfcResourceLevelRelationship = class abstract(TIfcPersistent)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
  published
    { A name used to identify or qualify the relationship. }
    property Name: TIfcLabel read FName write FName;

    { A description that may apply additional information about the relationship. }
    property Description: TIfcText read FDescription write FDescription;
  end;

  { Whilst the IfcPropertyDependencyRelationship may be used to describe
    the dependency, and it may do so in terms of the expression of how
    the dependency operates, it is not possible through the current IFC model
    for the value of the related property to be actually derived from
    the value of the relating property.
    The determination of value according to the dependency is required
    to be performed by an application that can then use the Expression attribute
    to flag the form of the dependency.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyDependencyRelationship.htm }
  TIfcPropertyDependencyRelationship = class(TIfcResourceLevelRelationship)
  strict private
    FDependingProperty: TIfcProperty;
    FDependantProperty: TIfcProperty;
    FExpression: TIfcText;
  published
    { The property on which the relationship depends. }
    property DependingProperty: TIfcProperty read FDependingProperty write FDependingProperty;

    { The dependant property. }
    property DependantProperty: TIfcProperty read FDependantProperty write FDependantProperty;

    { Further describe the nature of the dependency relation. }
    property Expression: TIfcText read FExpression write FExpression;
  end;
  TIfcPropertyDependencyRelationshipList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPropertyDependencyRelationship>;

  { Abstract base entity for relationships between resource-level entities. }
  TIfcExternalReferenceRelationship = class abstract(TIfcResourceLevelRelationship)
  strict private
    FRelatingReference: TIfcExternalReference;
    // FRelatedResourceObjects: TIfcResourceObjectSelectList;
  published
    { An external reference that can be used to tag an object
      within the range of IfcResourceObjectSelect. }
    property RelatingReference: TIfcExternalReference read FRelatingReference write FRelatingReference;

    { Objects within the list of IfcResourceObjectSelect
      that can be tagged by an external reference to a dictionary,
      library, catalogue, classification or documentation. }
    // TODO: How to express it?
    // TIfcResourceObjectSelect is not a normal class.
    // Using just TIfcPersistentList means we could have list of abstract classes, which is not allowed, as sometimes at deserialization we need to know the type beforehand.
    // TODO // property RelatedResourceObjects: TIfcResourceObjectSelectList read FRelatedResourceObjects;
  end;
  TIfcExternalReferenceRelationshipList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcExternalReferenceRelationship>)
  end;

  { Identification of information that is not explicitly represented
    in the current model.
    The Location (as a URL) is given to describe the place where
    the information can be found.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternalReference.htm }
  TIfcExternalReference = class(TIfcPersistent)
  strict private
    FLocation: TIfcURIReference;
    FIdentification: TIfcIdentifier;
    FName: TIfcLabel;
    FHasExternalReferenceForResources: TIfcExternalReferenceRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Location, where the external source (classification, document or library)
      can be accessed by electronic means. The electronic location is
      provided as an URI, and would normally be given as an URL location
      string. }
    property Location: TIfcURIReference read FLocation write FLocation;

    { Unique identifier of the referenced item within the external source
      (classification, document or library). }
    property Identification: TIfcIdentifier read FIdentification write FIdentification;

    { Optional name to further specify the reference. It can provide
      a human readable identifier (which does not necessarily need to have
      a counterpart in the internal structure of the document). }
    property Name: TIfcLabel read FName write FName;

    { Reference to all associations between this external reference
      and objects within the IfcResourceObjectSelect that are tagged
      by the external reference. }
    property HasExternalReferenceForResources: TIfcExternalReferenceRelationshipList read FHasExternalReferenceForResources;
  end;

  { Defines whether the definition of a profile shape shall be geometrically
    resolved into a curve or into a surface. }
  TIfcProfileTypeEnum = (
    Area,
    Curve
  );

  { Supertype of all definitions of standard and arbitrary profiles within IFC.
    It is used to define a standard set of commonly used section profiles
    by their parameters or by their explicit curve geometry.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProfileDef.htm }
  TIfcProfileDef = class(TIfcPersistent)
  strict private
    FProfileType: TIfcProfileTypeEnum;
    FProfileName: TIfcLabel;
    FHasExternalReference: TIfcExternalReferenceRelationshipList;
    FHasProperties: TIfcProfilePropertiesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Update CrossSection to be used by IfcExtrudedAreaSolid.
      In the base class, this makes a warning "not implemented".
      Override (and do not call "inherited") to provide a useful implementation. }
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean; const ParentName: String); virtual;
  published
    { Type of geometry into which this profile definition shall be resolved. }
    property ProfileType: TIfcProfileTypeEnum read FProfileType write FProfileType;

    { Human-readable name of the profile. }
    property ProfileName: TIfcLabel read FProfileName write FProfileName;

    { Reference to external information, e.g. library, classification,
      or document information, which is associated with the profile. }
    property HasExternalReference: TIfcExternalReferenceRelationshipList read FHasExternalReference;

    { Additional properties of the profile, for example mechanical properties. }
    property HasProperties: TIfcProfilePropertiesList read FHasProperties;
  end;
  TIfcProfileDefList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcProfileDef>;

  { Defines profile by composition of other profiles.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompositeProfileDef.htm }
  TIfcCompositeProfileDef = class(TIfcProfileDef)
  strict private
    FProfiles: TIfcProfileDefList;
    FLabel: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The profiles which are used to define the composite profile. }
    property Profiles: TIfcProfileDefList read FProfiles;

    { The name by which the composition may be referred. }
    property &Label: TIfcLabel read FLabel write FLabel;
  end;

  { Closed profile IfcArbitraryClosedProfileDef defines an arbitrary
    two-dimensional profile for the use within the swept surface geometry,
    the swept area solid or a sectioned spine. It is given by an outer boundary
    from which the surface or solid can be constructed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcArbitraryClosedProfileDef.htm }
  TIfcArbitraryClosedProfileDef = class(TIfcProfileDef)
  strict private
    FOuterCurve: TIfcCurve;
  public
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean; const ParentName: String); override;
  published
    { Bounded curve, defining the outer boundaries of the arbitrary profile. }
    property OuterCurve: TIfcCurve read FOuterCurve write FOuterCurve;
  end;

  { Arbitrary closed two-dimensional profile with holes.
    It is given by an outer boundary and inner boundaries.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcArbitraryProfileDefWithVoids.htm }
  TIfcArbitraryProfileDefWithVoids = class(TIfcArbitraryClosedProfileDef)
  strict private
    FInnerCurves: TIfcCurveList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean; const ParentName: String); override;
  published
    { Set of bounded curves, defining the inner boundaries of the arbitrary profile. }
    property InnerCurves: TIfcCurveList read FInnerCurves;
  end;

  TIfcRoleEnum = (
    Architect,
    BuildingOperator,
    BuildingOwner,
    CivilEngineer,
    Client,
    CommissioningEngineer,
    ConstructionManager,
    Consultant,
    Contractor,
    CostEngineer,
    ElectricalEngineer,
    Engineer,
    FacilitiesManager,
    FieldConstructionManager,
    Manufacturer,
    MechanicalEngineer,
    Owner,
    ProjectManager,
    Reseller,
    StructuralEngineer,
    Subcontractor,
    Supplier,
    UserDefined
  );

  TIfcActorRole = class(TIfcPersistent)
  strict private
    FRole: TIfcRoleEnum;
    FUserDefinedRole: TIfcLabel;
    FDescription: TIfcText;
    FHasExternalReference: TIfcExternalReferenceRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The name of the role played by an actor. }
    property Role: TIfcRoleEnum read FRole write FRole;

    { Allows for specification of user defined roles
      beyond the enumeration values provided by @link(Role). }
    property UserDefinedRole: TIfcLabel read FUserDefinedRole write FUserDefinedRole;

    { A textual description relating the nature of the role played by an actor. }
    property Description: TIfcText read FDescription write FDescription;

    { Reference to external information, e.g. library, classification,
      or document information, which is associated with the actor role. }
    property HasExternalReference: TIfcExternalReferenceRelationshipList read FHasExternalReference;
  end;
  TIfcActorRoleList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcActorRole>;

  { Person acting on behalf of an organization.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPersonAndOrganization.htm }
  TIfcPersonAndOrganization = class(TIfcPersistent)
  strict private
    FThePerson: TIfcPerson;
    FTheOrganization: TIfcOrganization;
    FRoles: TIfcActorRoleList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Person who is related to the organization. }
    property ThePerson: TIfcPerson read FThePerson write FThePerson;

    { Organization to which the person is related. }
    property TheOrganization: TIfcOrganization read FTheOrganization write FTheOrganization;

    { Roles played by the person within the context of an organization. }
    property Roles: TIfcActorRoleList read FRoles;
  end;
  TIfcPersonAndOrganizationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPersonAndOrganization>)
  end;

  { Individual human being.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPerson.htm }
  TIfcPerson = class(TIfcPersistent)
  strict private
    FIdentification: TIfcIdentifier;
    FFamilyName: TIfcLabel;
    FGivenName: TIfcLabel;
    FMiddleNames: TCastleStringList;
    FPrefixTitles: TCastleStringList;
    FSuffixTitles: TCastleStringList;
    FRoles: TIfcActorRoleList;
    FAddresses: TIfcAddressList;
    FEngagedIn: TIfcPersonAndOrganizationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Identification of the person. }
    property Identification: TIfcIdentifier read FIdentification write FIdentification;

    { The name by which the family identity of the person may be recognized. }
    property FamilyName: TIfcLabel read FFamilyName write FFamilyName;

    { The name by which a person is known within a family. }
    property GivenName: TIfcLabel read FGivenName write FGivenName;

    { Additional names given to a person. }
    property MiddleNames: TCastleStringList read FMiddleNames;

    { The word, or group of words, which specify the person's social
      and/or professional standing and appear before his/her names. }
    property PrefixTitles: TCastleStringList read FPrefixTitles;

    { The word, or group of words, which specify the person's social
      and/or professional standing and appear after his/her names.}
    property SuffixTitles: TCastleStringList read FSuffixTitles;

    { Roles played by the person. }
    property Roles: TIfcActorRoleList read FRoles;

    { Postal and telecommunication addresses of a person. }
    property Addresses: TIfcAddressList read FAddresses;

    { The inverse relationship to IfcPersonAndOrganization relationships in which IfcPerson is engaged. }
    property EngagedIn: TIfcPersonAndOrganizationList read FEngagedIn;
  end;
  TIfcPersonList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPerson>)
  end;

  { Organization relationship.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOrganizationRelationship.htm }
  TIfcOrganizationRelationship = class(TIfcResourceLevelRelationship)
  strict private
    FRelatingOrganization: TIfcOrganization;
    FRelatedOrganizations: TIfcOrganizationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Organization which is the relating part of the relationship between organizations. }
    property RelatingOrganization: TIfcOrganization read FRelatingOrganization write FRelatingOrganization;

    { The other, possibly dependent, organizations which are the related parts of the relationship between organizations.  }
    property RelatedOrganizations: TIfcOrganizationList read FRelatedOrganizations;
  end;
  TIfcOrganizationRelationshipList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcOrganizationRelationship>)
  end;

  { Named and structured grouping with a corporate identity.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOrganization.htm }
  TIfcOrganization = class(TIfcPersistent)
  strict private
    FIdentification: TIfcIdentifier;
    FName: TIfcLabel;
    FDescription: TIfcText;
    FRoles: TIfcActorRoleList;
    FAddresses: TIfcAddressList;
    FIsRelatedBy: TIfcOrganizationRelationshipList;
    FRelates: TIfcOrganizationRelationshipList;
    FEngages: TIfcPersonAndOrganizationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Identification of the organization. }
 	  property Identification: TIfcIdentifier read FIdentification write FIdentification;

    { The word, or group of words, by which the organization is referred to. }
    property Name: TIfcLabel read FName write FName;

    { Text that relates the nature of the organization. }
    property Description: TIfcText read FDescription write FDescription;

    { Roles played by the organization. }
    property Roles: TIfcActorRoleList read FRoles;

    { Postal and telecom addresses of an organization. }
    property Addresses: TIfcAddressList read FAddresses;

    { The inverse relationship for relationship RelatedOrganizations of IfcOrganizationRelationship. }
    property IsRelatedBy: TIfcOrganizationRelationshipList read FIsRelatedBy;

    { The inverse relationship for relationship RelatingOrganization of IfcOrganizationRelationship. }
    property Relates: TIfcOrganizationRelationshipList read FRelates;

    { Inverse relationship to IfcPersonAndOrganization relationships in which IfcOrganization is engaged. }
    property Engages: TIfcPersonAndOrganizationList read FEngages;
  end;
  TIfcOrganizationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcOrganization>)
  end;

  { Logical location of the address.

    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAddressTypeEnum.htm }
  TIfcAddressTypeEnum = (
    UserDefined,
    DistributionPoint,
    Home,
    Office,
    Site
  );

  { Various kinds of postal and telecom addresses.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAddress.htm }
  TIfcAddress = class abstract(TIfcPersistent)
  strict private
    FPurpose: TIfcAddressTypeEnum;
    FDescription: TIfcText;
    FUserDefinedPurpose: TIfcLabel;
    FOfPerson: TIfcPersonList;
    FOfOrganization: TIfcOrganizationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Logical location of the address. }
    property Purpose: TIfcAddressTypeEnum read FPurpose write FPurpose;

    { The nature of the address. }
    property Description: TIfcText read FDescription write FDescription;

    { Yser specific purpose of the address beyond the enumeration values
      provided by @link(Purpose). }
    property UserDefinedPurpose: TIfcLabel read FUserDefinedPurpose write FUserDefinedPurpose;

    { Inverse relationship to Person to whom address is associated. }
    property OfPerson: TIfcPersonList read FOfPerson;

    { Inverse relationship to Organization to whom address is associated. }
    property OfOrganization: TIfcOrganizationList read FOfOrganization;
  end;
  TIfcAddressList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcAddress>)
  end;

  { Address to which telephone, electronic mail and other forms of
    telecommunications should be addressed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTelecomAddress.htm }
  TIfcTelecomAddress = class(TIfcAddress)
  strict private
    FTelephoneNumbers: TCastleStringList;
    FFacsimileNumbers: TCastleStringList;
    FPagerNumber: TIfcLabel;
    FElectronicMailAddresses: TCastleStringList;
    FWWWHomePageURL: TIfcURIReference;
    FMessagingIDs: TCastleStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The list of telephone numbers at which telephone messages may be received. }
    property TelephoneNumbers: TCastleStringList read FTelephoneNumbers;

    { The list of fax numbers at which fax messages may be received. }
    property FacsimileNumbers: TCastleStringList read FFacsimileNumbers;

    { The pager number at which paging messages may be received. }
    property PagerNumber: TIfcLabel read FPagerNumber write FPagerNumber;

    { The list of Email addresses at which Email messages may be received. }
    property ElectronicMailAddresses: TCastleStringList read FElectronicMailAddresses;

    { The world wide web address at which the preliminary page of information for the person or organization can be located. }
    property WWWHomePageURL: TIfcURIReference read FWWWHomePageURL write FWWWHomePageURL;

    { IDs or addresses for any other means of telecommunication, for example
      instant messaging, voice-over-IP, or file transfer protocols.
      The communication protocol is indicated by the URI value with
      scheme designations such as irc:, sip:, or ftp:. }
    property MessagingIDs: TCastleStringList read FMessagingIDs;
  end;

  { IFC compliant application developed by an application developer. }
  TIfcApplication = class(TIfcPersistent)
  strict private
    FApplicationDeveloper: TIfcOrganization;
    FVersion: TIfcLabel;
    FApplicationFullName: TIfcLabel;
    FApplicationIdentifier: TIfcIdentifier;
  published
    { Name of the application developer. }
    property ApplicationDeveloper: TIfcOrganization read FApplicationDeveloper write FApplicationDeveloper;

    { Version number of this software as specified by the developer of the application. }
    property Version: TIfcLabel read FVersion write FVersion;

    { Full name of the application as specified by the application developer. }
    property ApplicationFullName: TIfcLabel read FApplicationFullName write FApplicationFullName;

    { Short identifying name for the application. }
    property ApplicationIdentifier: TIfcIdentifier read FApplicationIdentifier write FApplicationIdentifier;
  end;

  TIfcStateEnum = (
    NotDefined, //< Enum value added in CGE.
    Locked,
    ReadOnly,
    ReadOnlyLocked,
    ReadWrite,
    ReadWriteLocked
  );

  TIfcChangeActionEnum =  (
    NotDefined,
    Added,
    Deleted,
    Modified,
    NoChange
  );

  { All history and identification related information.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOwnerHistory.htm }
  TIfcOwnerHistory = class(TIfcPersistent)
  strict private
    FOwningUser: TIfcPersonAndOrganization;
    FOwningApplication: TIfcApplication;
    FState: TIfcStateEnum;
    FChangeAction: TIfcChangeActionEnum;
    FLastModifiedDate: TIfcTimeStamp;
    FLastModifyingUser: TIfcPersonAndOrganization;
    FLastModifyingApplication: TIfcApplication;
    FCreationDate: TIfcTimeStamp;
  published
    { End user who currently "owns" this object. }
    property OwningUser: TIfcPersonAndOrganization read FOwningUser write FOwningUser;

    { Application which currently "owns" this object on behalf of
      the owning user of the application. }
    property OwningApplication: TIfcApplication read FOwningApplication write FOwningApplication;

    { Current access state of the object. }
    property State: TIfcStateEnum read FState write FState
      default TIfcStateEnum.NotDefined;

    { Actions associated with changes made to the object. }
    property ChangeAction: TIfcChangeActionEnum read FChangeAction write FChangeAction
      default TIfcChangeActionEnum.NotDefined;

    { When the the last modification was made by LastModifyingUser
      and LastModifyingApplication.}
    property LastModifiedDate: TIfcTimeStamp read FLastModifiedDate write FLastModifiedDate default 0;

    { User who carried out the last modification using LastModifyingApplication. }
    property LastModifyingUser: TIfcPersonAndOrganization read FLastModifyingUser write FLastModifyingUser;

    { Application used to make the last modification. }
    property LastModifyingApplication: TIfcApplication read FLastModifyingApplication write FLastModifyingApplication;

    { When was the entity first created by the original OwningApplication.
      Once defined this value remains unchanged through the lifetime of the entity. }
    property CreationDate: TIfcTimeStamp read FCreationDate write FCreationDate default 0;
  end;

  { Most abstract and root class for all entity definitions.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoot.htm

    Note: GlobalId is defined in TIfcPersistent already, to support some testcases.
    Though IFC 4.3 specification says only IfcRoot has GlobalId. }
  TIfcRoot = class abstract(TIfcPersistent)
  strict private
    FOwnerHistory: TIfcOwnerHistory;
    FName: TIfcLabel;
    FDescription: TIfcText;
  protected
    function WantsGlobalId: Boolean; override;
  published
    { Information about the current ownership of that object. }
    property OwnerHistory: TIfcOwnerHistory read FOwnerHistory write FOwnerHistory;

    property Name: TIfcLabel read FName write FName;
    property Description: TIfcText read FDescription write FDescription;
  end;

  { Either TIfcObjectDefinition or TIfcPropertyDefinition.

    This is not a class (it is a choice) in IFC standard,
    but it makes sense to make it a regular class,
    as both TIfcObjectDefinition and TIfcPropertyDefinition descend
    directly from IfcRoot in the IFC standard, and it allows to have type-safe
    TIfcDefinitionSelectList.

    This is not abstract, because we may need to create "stubs" of this class
    when resolving references in IFC file. }
  TIfcDefinitionSelect = class abstract(TIfcRoot)
  end;

  { List of TIfcDefinitionSelect. }
  TIfcDefinitionSelectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcDefinitionSelect>)
  end;

  TIfcRelationship = class abstract(TIfcRoot)
  end;

  TIfcRelAssociates = class abstract(TIfcRelationship)
  strict private
    FRelatedObjects: TIfcDefinitionSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Set of object or property definitions to which the external references
      or information is associated.}
    property RelatedObjects: TIfcDefinitionSelectList read FRelatedObjects;
  end;
  TIfcRelAssociatesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelAssociates>)
  end;

  { eneral supertype for all material related information items in IFC
    that have occurrence specific assignment parameters to assign a set
    of materials with shape parameters to a reference geometry item of that
    component.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialUsageDefinition.htm }
  TIfcMaterialUsageDefinition = class abstract(TIfcPersistent)
  strict private
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    FAssociatedTo: TIfcRelAssociatesMaterialList;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    { Use of the IfcMaterialUsageDefinition subtypes within the material
      association of an element occurrence. The association is established
      by the IfcRelAssociatesMaterial relationship. }
    property AssociatedTo: TIfcRelAssociatesMaterialList read FAssociatedTo;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  end;

  { Usage of IfcMaterialProfileSet in terms of its location relative
    to the associated element geometry.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProfileSetUsage.htm }
  TIfcMaterialProfileSetUsage = class(TIfcMaterialUsageDefinition)
  strict private
    FForProfileSet: TIfcMaterialProfileSet;
    FCardinalPoint: Integer;
    FReferenceExtent: TIfcPositiveLengthMeasure;
  published
    { The IfcMaterialProfileSet set to which the usage is applied. }
    property ForProfileSet: TIfcMaterialProfileSet read FForProfileSet write FForProfileSet;

    { Index reference to a significant point in the section profile.
      Describes how the section is aligned relative to the (longitudinal)
      axis of the member it is associated with.
      This parametric specification of profile alignment can be provided
      redundantly to the explicit alignment defined by
      ForProfileSet.MaterialProfiles[*].Profile. }
    property CardinalPoint: Integer read FCardinalPoint write FCardinalPoint;

    { Extent of the extrusion of the elements body shape representation to
      which the IfcMaterialProfileSetUsage applies. It is used as the reference
      value for the upper OffsetValues[2] provided by
      the IfcMaterialProfileWithOffsets subtype for included material profiles. }
    property ReferenceExtent: TIfcPositiveLengthMeasure read FReferenceExtent write FReferenceExtent;
  end;

  { Selection of either a material definition or a material usage definition
    that can be assigned to an element, a resource or another entity within
    this specification.

    In IFC specification, this is a choice, not a class.
    But we make it a class, as it is more convenient for our purposes.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialSelect.htm }
  TIfcMaterialSelect = class(TIfcPersistent)
  end;

  { Relationship between a material definition and elements
    or element types to which this material definition applies. }
  TIfcRelAssociatesMaterial = class(TIfcRelAssociates)
  strict private
    FRelatingMaterial: TIfcMaterialSelect;
  published
    { Material definition assigned to the elements or element types. }
    property RelatingMaterial: TIfcMaterialSelect read FRelatingMaterial write FRelatingMaterial;
  end;
  TIfcRelAssociatesMaterialList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelAssociatesMaterial>)
  end;

  { Generalization of all characteristics (i.e. a grouping of individual
    properties), that may be assigned to objects. }
  TIfcPropertyDefinition = class(TIfcDefinitionSelect)
  strict private
    FHasContext: TIfcRelDeclares;
    FHasAssociations: TIfcRelAssociatesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the relationship IfcRelDeclares and
      thus to the IfcProject or IfcProjectLibrary. }
    property HasContext: TIfcRelDeclares read FHasContext write FHasContext;

    { Reference to the relationship IfcRelAssociates and thus to those
      externally defined concepts, like classifications, documents,
      or library information, which are associated to the property definition. }
    property HasAssociations: TIfcRelAssociatesList read FHasAssociations;
  end;

  { Declaration of objects (subtypes of IfcObject) or properties
    (subtypes of IfcPropertyDefinition) to a project or project library
    (represented by IfcProject, or IfcProjectLibrary). }
  TIfcRelDeclares = class(TIfcRelationship)
  strict private
    FRelatingContext: TIfcContext;
    FRelatedDefinitions: TIfcDefinitionSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the IfcProject to which additional information is assigned. }
    property RelatingContext: TIfcContext read FRelatingContext write FRelatingContext;

    { Set of object or property definitions that are assigned to a context and
      to which the unit and representation context definitions of that context
      apply. }
    property RelatedDefinitions: TIfcDefinitionSelectList read FRelatedDefinitions;
  end;
  TIfcRelDeclaresList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDeclares>)
  end;

  { Any semantically treated thing or process.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectDefinition.htm }
  TIfcObjectDefinition = class abstract(TIfcDefinitionSelect)
  strict private
    // TODO FHasAssignments: TIfcRelAssignsList;
    // TODO FNests: TIfcRelNestsList;
    // TODO FIsNestedBy: TIfcRelNestsList;
    FHasContext: TIfcRelDeclares;
    FIsDecomposedBy: TIfcRelAggregatesList;
    FDecomposes: TIfcRelAggregatesList;
    FHasAssociations: TIfcRelAssociatesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; virtual;
  published
    { Reference to the relationship objects, that assign
      (by an association relationship) other subtypes of IfcObject to
      this object instance. Examples are the association to products,
      processes, controls, resources or groups. }
    // TODO property HasAssignments: TIfcRelAssignsList read FHasAssignments;

    { References to the decomposition relationship being a nesting. }
    // TODO property Nests: TIfcRelNestsList read FNests;

    { References to the decomposition relationship being a nesting. }
    // TODO property IsNestedBy: TIfcRelNestsList read FIsNestedBy;

    { References to the context providing context information such as project
      unit or representation context. It should only be asserted for
      the uppermost non-spatial object. }
    property HasContext: TIfcRelDeclares read FHasContext write FHasContext;

    { References to the decomposition relationship being an aggregation. }
    property IsDecomposedBy: TIfcRelAggregatesList read FIsDecomposedBy;

    { References to the decomposition relationship being an aggregation. }
    property Decomposes: TIfcRelAggregatesList read FDecomposes;

    { Reference to the relationship objects, that associates external
      references or other resource definitions to the object.
      Examples are the association to library, documentation or classification. }
    property HasAssociations: TIfcRelAssociatesList read FHasAssociations;

    { Add given TIfcObjectDefinition instance to the @link(IsDecomposedBy) list
      of this object. Adds also new intermediate TIfcRelAggregates instance.

      This shall be used to add top-level IfcSpatialStructureElement
      instances to the project, e.g.:

      @unorderedList(
        @item TIfcSite instance inside IfcProject,
        @item IfcBuilding instance inside TIfcSite,
        @item IfcBuildingStorey instance inside IfcBuilding
      )

      @seealso TIfcSpatialElement.AddContainedElement }
    procedure AddIsDecomposedBy(const ChildObject: TIfcObjectDefinition);
  end;
  TIfcObjectDefinitionList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcObjectDefinition>;

  TIfcRelDefines = class(TIfcRelationship)
  end;

  { Relationship between an object taking part in an object type
    decomposition and an object occurrence taking part in an occurrence
    decomposition of that type.
    The IfcRelDefinesByObject is a 1-to-N relationship, as it allows for
    the assignment of one declaring object information to a single or
    to many reflected objects.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelDefinesByObject.htm }
  TIfcRelDefinesByObject = class(TIfcRelDefines)
  strict private
    FRelatedObjects: TIfcObjectList;
    FRelatingObject: TIfcObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Objects being part of an object occurrence decomposition,
      acting as the "reflecting parts" in the relationship. }
    property RelatedObjects: TIfcObjectList read FRelatedObjects;

    { Object being part of an object type decomposition,
      acting as the "declaring part" in the relationship. }
    property RelatingObject: TIfcObject read FRelatingObject write FRelatingObject;
  end;
  TIfcRelDefinesByObjectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDefinesByObject>)
  end;

  { Any semantically treated thing or process. }
  TIfcObject = class abstract(TIfcObjectDefinition)
  strict private
    FObjectType: TIfcLabel;
    FIsDeclaredBy: TIfcRelDefinesByObjectList;
    FDeclares: TIfcRelDefinesByObjectList;
    FIsTypedBy: TIfcRelDefinesByTypeList;
    FIsDefinedBy: TIfcRelDefinesByPropertiesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The type denotes a particular type that indicates the object further. }
    property ObjectType: TIfcLabel read FObjectType write FObjectType;

    { Link to the relationship object pointing to the declaring object that
      provides the object definitions for this object occurrence. }
    property IsDeclaredBy: TIfcRelDefinesByObjectList read FIsDeclaredBy;

    { Link to the relationship object pointing to the reflected object(s)
      that receives the object definitions. }
    property Declares: TIfcRelDefinesByObjectList read FDeclares;

    { Set of relationships to the object type that provides
      the type definitions for this object occurrence. }
    property IsTypedBy: TIfcRelDefinesByTypeList read FIsTypedBy;

    { Set of relationships to property set definitions attached to this object. }
    property IsDefinedBy: TIfcRelDefinesByPropertiesList read FIsDefinedBy;
  end;
  TIfcObjectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcObject>)
  end;

  { Generalization of all individual property sets that can be assigned to
    an object or type object.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertySetDefinition.htm }
  TIfcPropertySetDefinition = class(TIfcPropertyDefinition)
  strict private
    FDefinesType: TIfcTypeObjectList;
    FIsDefinedBy: TIfcRelDefinesByTemplateList;
    FDefinesOccurrence: TIfcRelDefinesByPropertiesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The type object to which the property set is assigned. }
    property DefinesType: TIfcTypeObjectList read FDefinesType;

    { Relation to the property set template, via the objectified relationship
      IfcRelDefinesByTemplate, that, if given, provides the definition template
      for the property set or quantity set and its properties. }
    property IsDefinedBy: TIfcRelDefinesByTemplateList read FIsDefinedBy;

    { Reference to the relation to one or many object occurrences that are
      characterized by the property set definition. A single property set can be
      assigned to multiple object occurrences using the objectified relationship
      IfcRefDefinesByProperties. }
    property DefinesOccurrence: TIfcRelDefinesByPropertiesList read FDefinesOccurrence;
  end;
  TIfcPropertySetDefinitionList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPropertySetDefinition>;

  { Container that holds properties within a property tree. }
  TIfcPropertySet = class(TIfcPropertySetDefinition)
  strict private
    FHasProperties: TIfcPropertyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Contained set of properties. }
    property HasProperties: TIfcPropertyList read FHasProperties;
  end;
  TIfcPropertySetList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPropertySet>;

  { Information about a type, common to all occurrences of this type. }
  TIfcTypeObject = class(TIfcObjectDefinition)
  strict private
    FApplicableOccurrence: TIfcIdentifier;
    FHasPropertySets: TIfcPropertySetDefinitionList;
    FTypes: TIfcRelDefinesByTypeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Data type of the occurrence object, to which the assigned type object can relate. }
    property ApplicableOccurrence: TIfcIdentifier read FApplicableOccurrence write FApplicableOccurrence;

    { Set of unique property sets, that are associated with the object type
      and are common to all object occurrences referring to this object type. }
    property HasPropertySets: TIfcPropertySetDefinitionList read FHasPropertySets;

    { Reference to the relationship IfcRelDefinesByType and thus
      to those occurrence objects, which are defined by this type. }
    property Types: TIfcRelDefinesByTypeList read FTypes;
  end;
  TIfcTypeObjectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcTypeObject>)
  end;

  { Abstract supertype of placement subtypes that define the location of an item,
    or an entire shape representation, and provide its orientation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlacement.htm }
  TIfcPlacement = class abstract(TIfcRepresentationItem)
  strict private
    FLocation: TIfcPoint;
  public
    { Get translation and rotation defined here.
      Rotation is expressed just like @link(TCastleTransform.Rotation)
      and @link(TTransformNode.Rotation), so it's axis and angle (in radians). }
    procedure GetTransform(out Translation: TVector3; out Rotation: TVector4); virtual;
  published
    property Location: TIfcPoint read FLocation write FLocation;

    // Not necessary in CGE now:
    // function Dim: TIfcDimensionCount; begin Result := Location.Dim; end;
  end;

  { Always either TIfcAxis2Placement2D or TIfcAxis2Placement3D.
    This is not a class, but a choice in IFC standard.
    So in Pascal we also don't define it as new class (to keep our inheritance
    of classes consistent with IFC).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement.htm }
  TIfcAxis2Placement = TIfcPlacement;

  { Parameterized planar curve.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConic.htm }
  TIfcConic = class abstract(TIfcCurve)
  strict private
    FPosition: TIfcAxis2Placement;
  private
    { Helper to implement BuildNode for TIfcEllipse and TIfcCircle.
      Radius and RadiusY meaning is just like to TCircle2DNode. }
    function BuildEllipseOrCircle(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode;
      const Radius, RadiusY: Single): TAbstractChildNode;
  published
    { The location and orientation of the conic. }
    property Position: TIfcAxis2Placement read FPosition write FPosition;
  end;

  { Ellipse.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEllipse.htm }
  TIfcEllipse = class(TIfcConic)
  strict private
    FSemiAxis1: TIfcPositiveLengthMeasure;
    FSemiAxis2: TIfcPositiveLengthMeasure;
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { The first radius of the ellipse. }
    property SemiAxis1: TIfcPositiveLengthMeasure read FSemiAxis1 write FSemiAxis1;

    { The second radius of the ellipse. }
    property SemiAxis2: TIfcPositiveLengthMeasure read FSemiAxis2 write FSemiAxis2;
  end;

  { Circle.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCircle.htm }
  TIfcCircle = class(TIfcConic)
  strict private
    FRadius: TIfcPositiveLengthMeasure;
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { Radius of the circle, greater than zero. }
    property Radius: TIfcPositiveLengthMeasure read FRadius write FRadius;
  end;

  { An abstract supertype of different kinds of geometric transformations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianTransformationOperator.htm }
  TIfcCartesianTransformationOperator = class abstract(TIfcRepresentationItem)
  strict private
    FAxis1: TIfcDirection;
    FAxis2: TIfcDirection;
    FLocalOrigin: TIfcCartesianPoint;
    FScale: TIfcReal;
  protected
    { The derived scale of the transformation, equal to scale
      if that is non-zero (it is zero if not specified in file), or 1.0 otherwise. }
    function Scl: Single;

    { The derived scale of the transformation along the axis 2 (normally the y axis). }
    function Scl2: Single; virtual;

    { The derived scale of the transformation along the axis 3 (normally the z axis). }
    function Scl3: Single; virtual;

    function Axis1Vector: TVector3;
    function Axis2Vector: TVector3;
    function Axis3Vector: TVector3; virtual;
  public
    procedure GetTransform(out Translation: TVector3;
      out Rotation: TVector4; out AScale: TVector3); virtual;
  published
    { The direction used to determine U[1], the derived X axis direction. }
    property Axis1: TIfcDirection read FAxis1 write FAxis1;

    { The direction used to determine U[2], the derived Y axis direction. }
    property Axis2: TIfcDirection read FAxis2 write FAxis2;

    { The required translation, specified as a cartesian point.
      The actual translation included in the transformation is
      from the geometric origin to the local origin. }
    property LocalOrigin: TIfcCartesianPoint read FLocalOrigin write FLocalOrigin;

    { The scaling value specified for the transformation. }
    property Scale: TIfcReal read FScale write FScale;
  end;

  { Geometric transformation in 2D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianTransformationOperator2D.htm }
  TIfcCartesianTransformationOperator2D = class(TIfcCartesianTransformationOperator)
  end;

  { Geometric transformation in three-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianTransformationOperator3D.htm }
  TIfcCartesianTransformationOperator3D = class(TIfcCartesianTransformationOperator)
  strict private
    FAxis3: TIfcDirection;
  protected
    function Axis3Vector: TVector3; override;
  published
    { The direction used to determine U[3], the derived Z axis direction. }
    property Axis3: TIfcDirection read FAxis3 write FAxis3;
  end;

  { Transformation operator in 3D that allows non uniform scaling.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianTransformationOperator3DnonUniform.htm }
  TIfcCartesianTransformationOperator3DnonUniform = class(TIfcCartesianTransformationOperator3D)
  strict private
    FScale2: TIfcReal;
    FScale3: TIfcReal;
  protected
    function Scl2: Single; override;
    function Scl3: Single; override;
  published
    { The scaling value specified for the transformation in the direction of Axis2.
      This is normally the Y scale factor. }
    property Scale2: TIfcReal read FScale2 write FScale2;

    { The scaling value specified for the transformation in the direction of Axis3.
      This is normally the Z scale factor. }
    property Scale3: TIfcReal read FScale3 write FScale3;
  end;

  { Inserted instance of a source definition (to be compared with a block /
    shared cell / macro definition). The instance is inserted by applying
    a Cartesian transformation operator as the MappingTarget.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMappedItem.htm }
  TIfcMappedItem = class(TIfcRepresentationItem)
  strict private
    FMappingSource: TIfcRepresentationMap;
    FMappingTarget: TIfcCartesianTransformationOperator;
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { A representation map that is the source of the mapped item.
      It can be seen as a block (or cell or marco) definition. }
    property MappingSource: TIfcRepresentationMap read FMappingSource write FMappingSource;

    { A representation item that is the target onto which the mapping source
      is mapped. It is constraint to be a Cartesian transformation operator. }
    property MappingTarget: TIfcCartesianTransformationOperator read FMappingTarget write FMappingTarget;
  end;
  TIfcMappedItemList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMappedItem>;

  { Base definition (also referred to as block, cell or macro)
    called MappedRepresentation within the MappingOrigin.
    The MappingOrigin defines the coordinate system in which
    the MappedRepresentation is defined. }
  TIfcRepresentationMap = class(TIfcPersistent)
  strict private
    FMappingOrigin: TIfcAxis2Placement;
    FMappedRepresentation: TIfcRepresentation;
    FHasShapeAspects: TIfcShapeAspectList;
    FMapUsage: TIfcMappedItemList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { An axis2 placement that defines the position about which
      the mapped representation is mapped. }
    property MappingOrigin: TIfcAxis2Placement read FMappingOrigin write FMappingOrigin;

    { A representation that is mapped to at least one mapped item. }
    property MappedRepresentation: TIfcRepresentation read FMappedRepresentation write FMappedRepresentation;

    { Reference to the shape aspect that represents part of the shape
      or its feature distinctively. }
    property HasShapeAspects: TIfcShapeAspectList read FHasShapeAspects;

    property MapUsage: TIfcMappedItemList read FMapUsage;
  end;
  TIfcRepresentationMapList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentationMap>;

  { Type definition of a product without being already inserted
    into a project structure (without having a placement). }
  TIfcTypeProduct = class(TIfcTypeObject)
  strict private
    FRepresentationMaps: TIfcRepresentationMapList;
    FTag: TIfcLabel;
    //FReferencedBy: TIfcRelAssignsToProductList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { List of unique representation maps. Each representation map describes
      a block definition of the shape of the product style.
      By providing more than one representation map, a multi-view
      block definition can be given. }
 	  property RepresentationMaps: TIfcRepresentationMapList read FRepresentationMaps;

    { The tag (or label) identifier at the particular type of a product,
      e.g. the article number (like the EAN). It is the identifier
      at the specific level. }
    property Tag: TIfcLabel read FTag write FTag;

    { Reference to the IfcRelAssignsToProduct relationship,
      by which other products, processes, controls, resources or actors
      (as subtypes of IfcObjectDefinition) can be related to this product type. }
    // TODO // property ReferencedBy: TIfcRelAssignsToProductList read FReferencedBy;
  end;

  { List of commonly shared property set definitions of an element and
    an optional set of product representations. }
  TIfcElementType = class abstract(TIfcTypeProduct)
  strict private
    FElementType: TIfcLabel;
  published
    { The type denotes a particular type that indicates the object further.
      The use has to be established at the level of instantiable subtypes. }
    property ElementType: TIfcLabel read FElementType write FElementType;
  end;

  TIfcAirTerminalTypeEnum = (
    NotDefined,
    Diffuser,
    Grille,
    Louvre,
    Register,
    UserDefined
  );

  TIfcSanitaryTerminalTypeEnum = (
    NotDefined,
    Bath,
    Bidet,
    Cistern,
    SanitaryFountain,
    Shower,
    Sink,
    ToiletPan,
    Urinal,
    WashHandBasin,
    WCSeat,
    UserDefined
  );

  TIfcBuildingElementPartTypeEnum = (
    NotDefined,
    Apron,
    ArmourUnit,
    Insulation,
    PrecastPanel,
    SafetyCage,
    UserDefined
  );

  TIfcElementAssemblyTypeEnum = (
    NotDefined,
    Abutment,
    Accessory_Assembly,
    Arch,
    Beam_Grid,
    Braced_Frame,
    Cross_Bracing,
    Deck,
    DilatationPanel,
    EntranceWorks,
    Girder,
    Grid,
    Mast,
    Pier,
    Pylon,
    Rail_Mechanical_Equipment_Assembly,
    Reinforcement_Unit,
    Rigid_Frame,
    Shelter,
    SignalAssembly,
    Slab_Field,
    SumpBuster,
    SupportingAssembly,
    SuspensionAssembly,
    TrackPanel,
    Traction_Switching_Assembly,
    Traffic_Calming_Device,
    Truss,
    TurnoutPanel,
    UserDefined
  );

  { Commonly shared property set definitions of @link(TIfcElementAssembly).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementAssemblyType.htm }
  TIfcElementAssemblyType = class(TIfcElementType)
  strict private
    FPredefinedType: TIfcElementAssemblyTypeEnum;
  published
    property PredefinedType: TIfcElementAssemblyTypeEnum read FPredefinedType write FPredefinedType
      default TIfcElementAssemblyTypeEnum.NotDefined;
  end;

  { Commonly shared property set definitions of @link(TIfcElementComponent).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementComponentType.htm }
  TIfcElementComponentType = class abstract(TIfcElementType)
  strict private
    FElementType: TIfcLabel;
  published
    { The type denotes a particular type that indicates the object further. }
    property ElementType: TIfcLabel read FElementType write FElementType;
  end;

  TIfcReinforcingBarTypeEnum = (
    NotDefined,
    Anchoring,
    Edge,
    Ligature,
    Main,
    Punching,
    Ring,
    Shear,
    SpaceBar,
    Stud,
    UserDefined
  );

  TIfcReinforcingBarSurfaceEnum = (
    Plain,
    Textured
  );

  { Commonly shared property set definitions of @link(TIfcReinforcingElement).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingElementType.htm }
  TIfcReinforcingElementType = class(TIfcElementComponentType)
  end;

  { Commonly shared property set definitions of @link(TIfcReinforcingBar).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingBarType.htm }
  TIfcReinforcingBarType = class(TIfcReinforcingElementType)
  strict private
    FNominalDiameter: TIfcPositiveLengthMeasure;
    FCrossSectionArea: TIfcAreaMeasure;
    FBarLength: TIfcPositiveLengthMeasure;
    FPredefinedType: TIfcReinforcingBarTypeEnum;
    FBarSurface: TIfcReinforcingBarSurfaceEnum;
    FBendingShapeCode: TIfcLabel;
    // TODO: FBendingParameters
  published
    { The role, purpose or usage of the bar. }
    property PredefinedType: TIfcReinforcingBarTypeEnum read FPredefinedType write FPredefinedType;

    { The nominal diameter defining the cross-section size of the reinforcing bar. }
    property NominalDiameter: TIfcPositiveLengthMeasure read FNominalDiameter write FNominalDiameter;

    { The effective cross-section area of the reinforcing bar or group of bars. }
    property CrossSectionArea: TIfcAreaMeasure read FCrossSectionArea write FCrossSectionArea;

    { The total length of the reinforcing bar. The total length of bended
      bars are calculated according to local standards with corrections
      for the bends. }
    property BarLength: TIfcPositiveLengthMeasure read FBarLength write FBarLength;

    { Indicator for whether the bar surface is plain or textured. }
    property BarSurface: TIfcReinforcingBarSurfaceEnum read FBarSurface write FBarSurface;

    { Shape code per a standard like ACI 315, ISO 3766, or a similar standard. }
    property BendingShapeCode: TIfcLabel read FBendingShapeCode write FBendingShapeCode;

    // TODO: FBendingParameters
  end;

  { Commonly shared property set definitions of @link(TIfcBuildingElementPart).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingElementPartType.htm }
  TIfcBuildingElementPartType = class(TIfcElementComponentType)
  strict private
    FPredefinedType: TIfcBuildingElementPartTypeEnum;
  published
    { Subtype of building element part. }
    property PredefinedType: TIfcBuildingElementPartTypeEnum read FPredefinedType write FPredefinedType
      default TIfcBuildingElementPartTypeEnum.NotDefined;
  end;

  { Commonly shared property set definitions of @link(TIfcFurnishingElement).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFurnishingElementType.htm }
  TIfcFurnishingElementType = class(TIfcElementType)
  end;

  { Types of system furniture, used by
    @link(TIfcSystemFurnitureElementType.PredefinedType),
    @link(TIfcSystemFurnitureElement.PredefinedType).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSystemFurnitureElementTypeEnum.htm }
  TIfcSystemFurnitureElementTypeEnum = (
    NotDefined,
    Panel,
    Subrack,
    Worksurface,
    UserDefined
  );

  { Commonly shared property set definitions of @link(TIfcSystemFurnitureElement).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSystemFurnitureElementType.htm }
  TIfcSystemFurnitureElementType = class(TIfcFurnishingElementType)
  strict private
    FPredefinedType: TIfcSystemFurnitureElementTypeEnum;
  published
    { Predefined types of system furniture. }
    property PredefinedType: TIfcSystemFurnitureElementTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSystemFurnitureElementTypeEnum.NotDefined;
  end;

  { Types of furniture from which the type required can be selected.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFurnitureTypeEnum.htm }
  TIfcFurnitureTypeEnum = (
    NotDefined,
    Bed,
    Chair,
    Desk,
    FileCabinet,
    Shelf,
    Sofa,
    Table,
    TechnicalCabinet,
    UserDefined
  );

  { Where the assembly is intended to take place, either in a factory
    or on the building site.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAssemblyPlaceEnum.htm }
  TIfcAssemblyPlaceEnum = (
    NotDefined,
    Factory,
    Site
  );

  { Commonly shared property set definitions of @link(TIfcFurniture).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFurnitureType.htm }
  TIfcFurnitureType = class(TIfcFurnishingElementType)
  strict private
    FPredefinedType: TIfcFurnitureTypeEnum;
    FAssemblyPlace: TIfcAssemblyPlaceEnum;
  published
    { Designation of where the assembly is intended to take place. }
    property AssemblyPlace: TIfcAssemblyPlaceEnum read FAssemblyPlace write FAssemblyPlace
      default TIfcAssemblyPlaceEnum.NotDefined;

    { Predefined types of furniture. }
    property PredefinedType: TIfcFurnitureTypeEnum read FPredefinedType write FPredefinedType
      default TIfcFurnitureTypeEnum.NotDefined;
  end;

  { A distribution element type is used to define the common properties
    of a certain type of a distribution element that may be applied
    to many instances of that feature type to assign a specific style.
    Distribution element types (or the instantiable subtypes) may
    be exchanged without being already assigned to occurrences.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionElementType.htm }
  TIfcDistributionElementType = class(TIfcElementType)
  end;

  { Distribution flow element types (or the instantiable subtypes)
    may be exchanged without being already assigned to occurrences.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionFlowElementType.htm }
  TIfcDistributionFlowElementType = class abstract(TIfcDistributionElementType)
  end;

  { Commonly shared property set definitions of @link(TIfcFlowTerminal).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowTerminalType.htm }
  TIfcFlowTerminalType = class abstract(TIfcDistributionFlowElementType)
  strict private
    FElementType: TIfcLabel;
  published
    { The type denotes a particular type that indicates the object further. }
    property ElementType: TIfcLabel read FElementType write FElementType;
  end;

  { Commonly shared property set definitions of @link(TIfcAirTerminal).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirTerminalType.htm }
  TIfcAirTerminalType = class(TIfcFlowTerminalType)
  strict private
    FPredefinedType: TIfcAirTerminalTypeEnum;
  published
    { A list of types to further identify the object. }
    property PredefinedType: TIfcAirTerminalTypeEnum read FPredefinedType write FPredefinedType
      default TIfcAirTerminalTypeEnum.NotDefined;
  end;

  { Commonly shared property set definitions of @link(TIfcSanitaryTerminal).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSanitaryTerminalType.htm }
  TIfcSanitaryTerminalType = class(TIfcFlowTerminalType)
  strict private
    FPredefinedType: TIfcSanitaryTerminalTypeEnum;
  published
    { Identifies the predefined types of sanitary terminal from which
      the type required may be set. }
    property PredefinedType: TIfcSanitaryTerminalTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSanitaryTerminalTypeEnum.NotDefined;
  end;

  { A energy conversion type is used to define the common properties
    of a energy conversion device that may be applied to many occurrences
    of that type. An energy conversion device is a building systems device
    that converts energy from one form into another such as a boiler
    (combusting gas to heat water), chiller
    (using a refrigeration cycle to cool a liquid), or a cooling coil
    (using the phase-change characteristics of a refrigerant to cool air).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEnergyConversionDeviceType.htm }
  TIfcEnergyConversionDeviceType = class abstract(TIfcDistributionFlowElementType)
  end;

  { Typical types of boilers.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoilerTypeEnum.htm }
  TIfcBoilerTypeEnum = (
    NotDefined,
    Steam,
    Water,
    UserDefined
  );

  { Used to define a boiler type specification indicating the specific product
    information that is common to all occurrences of that product type.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoilerType.htm }
  TIfcBoilerType = class(TIfcEnergyConversionDeviceType)
  strict private
    FPredefinedType: TIfcBoilerTypeEnum;
  published
    { Defines types of boilers. }
    property PredefinedType: TIfcBoilerTypeEnum read FPredefinedType write FPredefinedType default TIfcBoilerTypeEnum.NotDefined;
  end;

  { Common properties of a certain type of built element that are applied
    to all occurrences of that type. }
  TIfcBuiltElementType = class(TIfcElementType)
  end;

  { Predefined types of beams that can further specify an
    @link(TIfcBeam) or @link(TIfcBeamType).}
  TIfcBeamTypeEnum = (
    NotDefined,
    Beam,
    Cornice,
    Diaphragm,
    EdgeBeam,
    Girder_Segment,
    Hatstone,
    Hollowcore,
    Joist,
    Lintel,
    Piercap,
    Spandrel,
    T_Beam,
    UserDefined
  );

  { Commonly shared information for occurrences of beams.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBeamType.htm }
  TIfcBeamType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcBeamTypeEnum;
  published
    { Predefined generic type for a beam that is specified in an enumeration. }
    property PredefinedType: TIfcBeamTypeEnum read FPredefinedType write FPredefinedType default TIfcBeamTypeEnum.NotDefined;
  end;

  { Defines a list of commonly shared property set definitions of an element
    and an optional set of product representations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionControlElementType.htm }
  TIfcDistributionControlElementType = class(TIfcElementType)
  strict private
    FElementType: TIfcLabel;
  published
    { The type denotes a particular type that indicates the object further. }
    property ElementType: TIfcLabel read FElementType write FElementType;
  end;

  { Range of different types of actuator that can be specified.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcActuatorTypeEnum.htm }
  TIfcActuatorTypeEnum = (
    NotDefined,
    ElectricActuator,
    HandOperatedActuator,
    HydraulicActuator,
    PneumaticActuator,
    ThermostaticActuator,
    UserDefined
  );

  { Commonly shared information for occurrences of actuators.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcActuatorType.htm }
  TIfcActuatorType = class(TIfcDistributionControlElementType)
  strict private
    FPredefinedType: TIfcActuatorTypeEnum;
  published
    property PredefinedType: TIfcActuatorTypeEnum read FPredefinedType write FPredefinedType
      default TIfcActuatorTypeEnum.NotDefined;
  end;

  TIfcSlabTypeEnum = (
    NotDefined,
    Approach_Slab,
    BaseSlab,
    Floor,
    Landing,
    Paving,
    Roof,
    Sidewalk,
    TrackSlab,
    Wearing,
    UserDefined
  );

  { Commonly shared information for occurrences of slabs.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSlabType.htm }
  TIfcSlabType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcSlabTypeEnum;
  published
    property PredefinedType: TIfcSlabTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSlabTypeEnum.NotDefined;
  end;

  TIfcBuildingElementProxyTypeEnum = (
    NotDefined,
    Complex,
    Element,
    Partial,
    ProvisionForSpace,
    ProvisionForVoid,
    UserDefined
  );

  { Commonly shared property set definitions of a building element proxy
    and an optional set of product representations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingElementProxyType.htm }
  TIfcBuildingElementProxyType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcBuildingElementProxyTypeEnum;
  published
    property PredefinedType: TIfcBuildingElementProxyTypeEnum read FPredefinedType write FPredefinedType
      default TIfcBuildingElementProxyTypeEnum.NotDefined;
  end;

  { Used for @link(TIfcWindow.PredefinedType) and
    @link(TIfcWindowType.PredefinedType). }
  TIfcWindowTypeEnum = (NotDefined, LightDome, SkyLight, Window, UserDefined);

  { Basic configuration of the window type in terms of the number of window
    panels and the subdivision of the total window.
    Used for @link(TIfcWindow.PartitioningType) and
    @link(TIfcWindowType.PartitioningType).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindowTypePartitioningEnum.htm }
  TIfcWindowTypePartitioningEnum = (
    NotDefined,
    Double_Panel_Horizontal,
    Double_Panel_Vertical,
    Single_Panel,
    Triple_Panel_Bottom,
    Triple_Panel_Horizontal,
    Triple_Panel_Left,
    Triple_Panel_Right,
    Triple_Panel_Top,
    Triple_Panel_Vertical,
    UserDefined
  );

  { Commonly shared information for occurrences of windows. }
  TIfcWindowType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcWindowTypeEnum;
    FPartitioningType: TIfcWindowTypePartitioningEnum;
    FParameterTakesPrecedence: Boolean;
    FUserDefinedPartitioningType: TIfcLabel;
  published
    { Predefined types of a window element from which the type required may be set. }
    property PredefinedType: TIfcWindowTypeEnum read FPredefinedType write FPredefinedType
      default TIfcWindowTypeEnum.NotDefined;

    { Type defining the general layout of the window type in terms of the partitioning of panels. }
    property PartitioningType: TIfcWindowTypePartitioningEnum read FPartitioningType write FPartitioningType
      default TIfcWindowTypePartitioningEnum.NotDefined;

    { Reflects, whether the parameter given in the attached lining and panel
      properties exactly define the geometry (TRUE),
      or whether the attached style shape take precedence (FALSE).
      In the last case the parameter have only informative value.

      TODO: Should be an enum with 3 values?
      Since it is optional, and it seems "false" is not exactly equal to "not specified". }
    property ParameterTakesPrecedence: Boolean read FParameterTakesPrecedence write FParameterTakesPrecedence default false;

    { Designator for the user defined partitioning type, shall only be provided,
      if the value of PartitioningType is set to UserDefined. }
    property UserDefinedPartitioningType: TIfcLabel read FUserDefinedPartitioningType write FUserDefinedPartitioningType;
  end;

  { Define the object coordinate system.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectPlacement.htm }
  TIfcObjectPlacement = class abstract(TIfcPersistent)
  strict private
    FPlacementRelTo: TIfcObjectPlacement;
    FPlacesObject: TIfcProductList;
    FReferencedByPlacements: TIfcObjectPlacementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Build TTransformNode representing this TIfcObjectPlacement.

      This is not used for TIfcObjectPlacement that are directly referenced
      by TIfcObjectDefinition, in which case TIfcObjectDefinition creates
      TTransformNode, and only calls our @link(UpdateNode) to set it's parameters.

      This is used for TIfcObjectPlacement instances that are @italic(not)
      directly referenced by TIfcObjectDefinition (but may be still useful,
      e.g. as placementRelTo targets for other TIfcObjectPlacement instances). }
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;

    { Update the TTransformNode with the transformation expressed by this
      TIfcObjectPlacement (local transformation, not accounding for any
      transformation implied by @link(PlacementRelTo)). }
    procedure UpdateNode(const Mapping: TCastleIfcMapping;
      const Transform: TTransformNode); virtual;
  published
    { Reference to object placement that provides the relative placement with
      its placement in a grid, local coordinate system or linear referenced
      placement. }
    property PlacementRelTo: TIfcObjectPlacement read FPlacementRelTo write FPlacementRelTo;

    { The IfcObjectPlacement shall be used to provide a placement
      and an object coordinate system for instances of IfcProduct. }
    property PlacesObject: TIfcProductList read FPlacesObject;

    { Inverse of @link(PlacementRelTo). }
    property ReferencedByPlacements: TIfcObjectPlacementList read FReferencedByPlacements;
  end;
  TIfcObjectPlacementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcObjectPlacement>)
  end;

  { Abstract point.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPoint.htm }
  TIfcPoint = class abstract(TIfcGeometricRepresentationItem)
  (*Not necessary in CGE now:
    Dim is not explicitly specified in IFC, but it is derived from
    the contents of IFC. Needs special logic, if ever needed in CGE.

  strict private
    FDim: TIfcDimensionCount;
  public
    { The dimensions are derived from the descendant classes.
      Determined by the number of coordinate components in case of
      TIfcCartesianPoint or by the dimensionality
      of the basis curve or surface in other cases. }
    property Dim: TIfcDimensionCount read FDim write FDim;
  *)
  end;

  { A regular 3D point (may be specified as 2D too, in which case we set Z to 0).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianPoint.htm }
  TIfcCartesianPoint = class(TIfcPoint)
  strict private
    FCoordinates: TCastleVector3PersistentSimple;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Create a new instance with given coordinates. }
    constructor CreateVector(const AOwner: TComponent; const AVector: TVector3);
  published
    { Actual 3D point value. }
    property Coordinates: TCastleVector3PersistentSimple read FCoordinates;
  end;
  TIfcCartesianPointList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcCartesianPoint>)
  end;

  { Location and orientation to place items in a two-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement2D.htm }
  TIfcAxis2Placement2D = class(TIfcPlacement)
  strict private
    FRefDirection: TIfcDirection;
  public
    { Axes of the created coordinate system. Always normalized and orthogonal. }
    function P(const AxisCoord: T2DAxis): TVector2;

    procedure GetTransform(out Translation: TVector3; out Rotation: TVector4); override;
  published
    { Direction of the local X axis. May be @nil. }
    property RefDirection: TIfcDirection read FRefDirection write FRefDirection;
  end;

  { Location and orientations to place items in a three-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement3D.htm }
  TIfcAxis2Placement3D = class(TIfcPlacement)
  strict private
    FAxis: TIfcDirection;
    FRefDirection: TIfcDirection;
  public
    { Axes of the created coordinate system. Always normalized and orthogonal. }
    function P(const AxisCoord: T3DAxis): TVector3;

    procedure GetTransform(out Translation: TVector3; out Rotation: TVector4); override;
  published
    { Direction of the local Z axis. May be @nil. }
    property Axis: TIfcDirection read FAxis write FAxis;

    { Direction of the local X axis. May be @nil.
      If necessary an adjustment is made to maintain orthogonality to
      the Axis direction.
      If Axis and/or RefDirection is omitted, these directions are taken from
      the geometric coordinate system. }
    property RefDirection: TIfcDirection read FRefDirection write FRefDirection;
  end;

  { 2D position coordinate system to which the parameters of the different profiles relate to.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcParameterizedProfileDef.htm }
  TIfcParameterizedProfileDef = class(TIfcProfileDef)
  strict private
    FPosition: TIfcAxis2Placement2D;
  published
    { Position coordinate system of the parameterized profile definition.
      If unspecified, no translation and no rotation is applied. }
    property Position: TIfcAxis2Placement2D read FPosition write FPosition;
  end;

  { Rectangle as the profile definition used by the swept surface geometry
    or the swept area solid.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRectangleProfileDef.htm }
  TIfcRectangleProfileDef = class(TIfcParameterizedProfileDef)
  strict private
    FXDim: TIfcPositiveLengthMeasure;
    FYDim: TIfcPositiveLengthMeasure;
  public
    procedure UpdateCrossSection(const CrossSection: TVector2List;
      var CrossSectionConvex: Boolean; const ParentName: String); override;
  published
    { The extent of the rectangle in the direction of the x-axis.. }
    property XDim: TIfcPositiveLengthMeasure read FXDim write FXDim;

    { The extent of the rectangle in the direction of the y-axis. }
    property YDim: TIfcPositiveLengthMeasure read FYDim write FYDim;
  end;

  { Represents the 3D shape by a sweeping representation scheme allowing
    a two dimensional planar cross section to sweep through space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptAreaSolid.htm }
  TIfcSweptAreaSolid = class abstract(TIfcSolidModel)
  strict private
    FSweptArea: TIfcProfileDef;
    FPosition: TIfcAxis2Placement3D;
  published
    { The surface defining the area to be swept.
      It is given as a profile definition within the xy plane
      of the position coordinate system.}
    property SweptArea: TIfcProfileDef read FSweptArea write FSweptArea;

    { Position coordinate system for the resulting swept solid of the sweeping operation. }
    property Position: TIfcAxis2Placement3D read FPosition write FPosition;
  end;

  { Defined by sweeping a cross section provided by a profile definition.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExtrudedAreaSolid.htm }
  TIfcExtrudedAreaSolid = class(TIfcSweptAreaSolid)
  strict private
    FExtrudedDirection: TIfcDirection;
    FDepth: TIfcPositiveLengthMeasure;
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode; override;
  published
    { The direction in which the surface, provided by SweptArea is to be swept.
      The ExtrudedDirection shall not be perpendicular to the local z-axis. }
    property ExtrudedDirection: TIfcDirection read FExtrudedDirection write FExtrudedDirection;

    { The distance the surface is to be swept along the ExtrudedDirection. }
    property Depth: TIfcPositiveLengthMeasure read FDepth write FDepth {$ifdef FPC}default 0.0{$endif};
  end;

  { Relative placement of a product
    - in relation to the placement of another product
    - or the absolute placement of a product
      within the geometric representation context of the project.
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLocalPlacement.htm
  }
  TIfcLocalPlacement = class(TIfcObjectPlacement)
  strict private
    FRelativePlacement: TIfcAxis2Placement;
  protected
    function WantsGlobalId: Boolean; override;
  public
    procedure UpdateNode(const Mapping: TCastleIfcMapping;
      const Transform: TTransformNode); override;
  published
    property RelativePlacement: TIfcAxis2Placement read FRelativePlacement write FRelativePlacement;
  end;

  { Any object that relates to a geometric or spatial context.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProduct.htm }
  TIfcProduct = class abstract(TIfcObject)
  strict private
    FObjectPlacement: TIfcObjectPlacement;
    FRepresentation: TIfcProductRepresentation;
    FPositionedRelativeTo: TIfcRelPositionsList;
    FReferencedInStructures: TIfcRelReferencedInSpatialStructure;
    function GetTranslation: TVector3;
    procedure SetTranslation(const Value: TVector3);
    { Return the value of @link(TransformSupported)
      and regardless of it's value,
      set Translation and Rotation to our transformation. }
    function GetTransform(out Translation: TVector3; out Rotation: TVector4): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; override;

    { Add mesh representation to the given product.

      The mesh is expressed as a set of 3D points (Coordinates)
      and indexes to these coordinates (Indexes) that determine polygons.
      The indexes are 0-based and can be separated by -1 to delimit polygons.

      See examples/ifc/ for an example usage of this routine to
      define walls and floors.

      @param(Context Geometric representation context instance,
        usually @link(TIfcProject.ModelContext) (for 3D models) or
        @link(TIfcProject.PlanContext) (for 2D floor plans).
      )
    }
    procedure AddMeshRepresentation(
      const Context: TIfcGeometricRepresentationContext;
      const Coordinates: array of TVector3;
      const Indexes: array of Integer;
      const Solid: Boolean = true);

    { Add box representation to the given product.

      Right now, the Box is transformed into a proper mesh representation,
      so this is just equivalent to calling AddMeshRepresentation with
      the 8 box corners and proper indexes to define 6 box sides. }
    procedure AddBoxRepresentation(
      const Context: TIfcGeometricRepresentationContext;
      const Box: TBox3D);

    { Does @link(ObjectPlacement) express a 3D transformation we can
      handle and @link(Translation) reliably gets and sets it. }
    function TransformSupported: Boolean;

    { Translation of this product.

      @unorderedList(
        @item(
          Using this property when @link(TransformSupported) is @true
          is most intuitive. It can then reliably get and set 3D translation.
          The translation is relative to the parent or to other object
          (determined by @link(TIfcObjectPlacement.PlacementRelTo),
          can be set using @link(SetTransformRelativeTo).)
        )

        @item(
          Using this property when @link(TransformSupported) is @false
          is still possible.

          The interpretation depends on the configuration of the @link(ObjectPlacement).
          E.g. it may specify that it's a 2D transformation
          (so setting a translation may change it to 3D, and right now it may
          not preserve 2D rotation).
          In general, IFC offers various possibilities to define the placement
          of an object, relative to various parents.

          If in doubt, just don't use this property when @link(TransformSupported)
          is @false. Instead, in that case (@link(TransformSupported) = @false),
          investigate and change the @link(ObjectPlacement) properties.
        )
      )
    }
    property Translation: TVector3 read GetTranslation write SetTranslation;

    { Make @link(Translation) of this product relative to
      a given different product.
      This sets @link(TIfcObjectPlacement.PlacementRelTo) underneath. }
    procedure SetTransformRelativeTo(const OtherProduct: TIfcProduct);
  published
    { Object coordinate system and placement of the product in space. }
    property ObjectPlacement: TIfcObjectPlacement read FObjectPlacement write FObjectPlacement;

    { Representations of the product. }
    property Representation: TIfcProductRepresentation read FRepresentation write FRepresentation;

    // TODO // property ReferencedBy: TIfcRelAssignsToProductList read FReferencedBy;

    property PositionedRelativeTo: TIfcRelPositionsList read FPositionedRelativeTo;

    property ReferencedInStructures: TIfcRelReferencedInSpatialStructure
      read FReferencedInStructures write FReferencedInStructures;
  end;
  TIfcProductList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProduct>)
  end;

  { Abstract entity definition for positioning and annotating elements that
    are used to position other elements relatively.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPositioningElement.htm }
  TIfcPositioningElement = class abstract(TIfcProduct)
  strict private
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    FContainedInStructure: TIfcRelContainedInSpatialStructure;
    FPositions: TIfcRelPositionsList;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    { Relationship to a spatial structure element, to which the
      positioning element is primarily associated. }
    property ContainedInStructure: TIfcRelContainedInSpatialStructure read FContainedInStructure write FContainedInStructure;

    property Positions: TIfcRelPositionsList read FPositions;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  end;

  TIfcGridTypeEnum = (
    NotDefined,
    Irregular,
    Radial,
    Rectangular,
    Triangular,
    UserDefined
  );

  { Derived location of the intersection between two grid axes.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVirtualGridIntersection.htm }
  TIfcVirtualGridIntersection = class(TIfcPersistent)
  strict private
    FIntersectingAxes: TIfcGridAxisList;
    FOffsetDistances: TCastleVector3PersistentSimple;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Two grid axes which intersects at exactly one intersection. }
    property IntersectingAxes: TIfcGridAxisList read FIntersectingAxes;

    { Offset distances to the grid axes. }
    property OffsetDistances: TCastleVector3PersistentSimple read FOffsetDistances;
  end;
  TIfcVirtualGridIntersectionList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcVirtualGridIntersection>;

  { An individual axis in the context of a design grid.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGridAxis.htm }
  TIfcGridAxis = class(TIfcPersistent)
  strict private
    FAxisTag: TIfcLabel;
    FAxisCurve: TIfcCurve;
    FSameSense: Boolean;
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    FPartOfU: TIfcGrid;
    FPartOfV: TIfcGrid;
    FPartOfW: TIfcGrid;
    FHasIntersections: TIfcVirtualGridIntersectionList;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
  published
    { The tag or name for this grid axis. }
    property AxisTag: TIfcLabel read FAxisTag write FAxisTag;

    { Underlying curve which provides the geometry for this grid axis. }
    property AxisCurve: TIfcCurve read FAxisCurve write FAxisCurve;

    { Defines whether the original sense of curve is used or whether
      it is reversed in the context of the grid axis. }
    property SameSense: Boolean read FSameSense write FSameSense;

    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    { If provided, the IfcGridAxis is part of the UAxes of IfcGrid. }
    property PartOfU: TIfcGrid read FPartOfU write FPartOfU;

    { If provided, the IfcGridAxis is part of the VAxes of IfcGrid. }
    property PartOfV: TIfcGrid read FPartOfV write FPartOfV;

    { If provided, the IfcGridAxis is part of the WAxes of IfcGrid. }
    property PartOfW: TIfcGrid read FPartOfW write FPartOfW;

    { The reference to a set of @link(TIfcVirtualGridIntersection)s,
      that connect other grid axes to this grid axis. }
    property HasIntersections: TIfcVirtualGridIntersectionList read FHasIntersections;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  end;
  TIfcGridAxisList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcGridAxis>)
  end;

  { Planar design grid defined in 3D space used as an aid in locating
    structural and design elements.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGrid.htm }
  TIfcGrid = class(TIfcPositioningElement)
  strict private
    FUAxes: TIfcGridAxisList;
    FVAxes: TIfcGridAxisList;
    FWAxes: TIfcGridAxisList;
    FPredefinedType: TIfcGridTypeEnum;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; override;
  published
    { List of grid axes defining the first row of grid lines. }
    property UAxes: TIfcGridAxisList read FUAxes;

    { List of grid axes defining the second row of grid lines. }
    property VAxes: TIfcGridAxisList read FVAxes;

    { List of grid axes defining the third row of grid lines.
      It may be given in the case of a triangular grid. }
    property WAxes: TIfcGridAxisList read FWAxes;

    { Predefined types to define the particular type of the grid. }
    property PredefinedType: TIfcGridTypeEnum read FPredefinedType write FPredefinedType
      default TIfcGridTypeEnum.NotDefined;
  end;

  { Elements that might be used to define a spatial structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialElement.htm }
  TIfcSpatialElement = class abstract(TIfcProduct)
  strict private
    FLongName: TIfcLabel;
    FContainsElements: TIfcRelContainedInSpatialStructureList;
    // TODO: rest of fields
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; override;
  published
    property LongName: TIfcLabel read FLongName write FLongName;
    property ContainsElements: TIfcRelContainedInSpatialStructureList read FContainsElements;

    { Add given Element (TIfcElement, descendant of TIfcProduct)
      to the @link(ContainsElements) list of this spatial element.

      This adds an intermediate TIfcRelContainedInSpatialStructure instance,
      and expands the @link(ContainsElements) list of this spatial element,
      and expands the @link(TIfcElement.ContainedInStructure) of the given Element.

      It may reuse existing TIfcRelContainedInSpatialStructure instance
      (BonsaiBIM also does this, using one TIfcRelContainedInSpatialStructure
      to refer to multiple children).

      This should be used to add smaller elements, like TIfcWall, TIfcWindow
      into larger spatial elements, like TIfcBuildingStorey, TIfcSpace. }
    procedure AddContainedElement(const Element: TIfcElement);

    { Add all contained elemenents to the List.
      This enumerates relations inside @link(ContainsElements)
      and the elements they refer to.

      This is not recursive, it does not go into elements that are children
      of other elements. }
    procedure GetContainedElements(const List: TIfcElementList);
  end;

  TIfcSpatialZoneTypeEnum = (
    NotDefined,
    Construction,
    FireSafety,
    Interference,
    Lighting,
    Occupancy,
    Reservation,
    Security,
    Thermal,
    Transport,
    Ventilation,
    UserDefined
  );

  { Non-hierarchical and potentially overlapping decomposition of the project
    under some functional consideration. A spatial zone might be used
    to represent a thermal zone, a construction zone, a lighting zone,
    a usable area zone. }
  TIfcSpatialZone = class(TIfcSpatialElement)
  strict private
    FPredefinedType: TIfcSpatialZoneTypeEnum;
  published
    { Predefined types to define the particular type of the spatial zone. }
    property PredefinedType: TIfcSpatialZoneTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSpatialZoneTypeEnum.NotDefined;
  end;

  { Relationship that connects objects under some criteria. }
  TIfcRelConnects = class abstract(TIfcRelationship)
  end;

  { Relationship informs on the positioning dependency between
    a product and a positioning element.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelPositions.htm }
  TIfcRelPositions = class(TIfcRelConnects)
  strict private
    FRelatingPositioningElement: TIfcPositioningElement;
    FRelatedProducts: TIfcProductList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Positioning element defining the source of the relative position. }
    property RelatingPositioningElement: TIfcPositioningElement read FRelatingPositioningElement write FRelatingPositioningElement;

    { Relatively positioned product. }
    property RelatedProducts: TIfcProductList read FRelatedProducts;
  end;
  TIfcRelPositionsList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelPositions>)
  end;

  { Relationship to assign elements to a certain level of the spatial project structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelContainedInSpatialStructure.htm }
  TIfcRelContainedInSpatialStructure = class(TIfcRelConnects)
  strict private
    FRelatedElements: TIfcProductList;
    FRelatingStructure: TIfcSpatialElement;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
  published
    { Set of products, which are contained within this level of
      the spatial structure hierarchy.

      Note: The name is a bit confusing, it's actually a list of TIfcProduct
      instances, not TIfcElement instances. TIfcElement is a descendant of TIfcProduct.
      This all follows IFC 4.3 spec. }
    property RelatedElements: TIfcProductList read FRelatedElements;

    { Spatial structure element, within which the element is contained. }
    property RelatingStructure: TIfcSpatialElement read FRelatingStructure write FRelatingStructure;
  end;
  TIfcRelContainedInSpatialStructureList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelContainedInSpatialStructure>)
  end;

  TIfcElementCompositionEnum = (
    Element,
    Complex,
    Partial
  );

  { Spatial structure can be used to provide a spatial organization of a project.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialStructureElement.htm }
  TIfcSpatialStructureElement = class abstract(TIfcSpatialElement)
  strict private
    FCompositionType: TIfcElementCompositionEnum;
  published
    { Denotes, whether the predefined spatial structure element represents
      itself, or an aggregate (complex) or a part (part).
      The interpretation is given separately for each subtype
      of spatial structure element.

      Pascal default Element matches IFC spec default:
      If no CompositionType is asserted, the default value ''ELEMENT'' applies. }
    property CompositionType: TIfcElementCompositionEnum
      read FCompositionType write FCompositionType default TIfcElementCompositionEnum.Element;
  end;

  { Compound measure of plane angle in degrees, minutes, seconds,
    and optionally millionth-seconds of arc.
    IFC defines it as list of 3 or 4 integers.
    We considered using TCastleVector4PersistentSimple for this,
    but actually a regular class with 4 integer properties is better,
    fits IFC JSON encoding. }
  TIfcCompoundPlaneAngleMeasure = class(TIfcPersistent)
  strict private
    FDegrees: Integer;
    FMinutes: Integer;
    FSeconds: Integer;
    FMicroSeconds: Integer;
  published
    property Degrees: Integer read FDegrees write FDegrees;
    property Minutes: Integer read FMinutes write FMinutes;
    property Seconds: Integer read FSeconds write FSeconds;
    property MicroSeconds: Integer read FMicroSeconds write FMicroSeconds;
  end;

  { Address for delivery of paper based mail and other postal deliveries. }
  TIfcPostalAddress = class(TIfcAddress)
  strict private
    FInternalLocation: TIfcLabel;
    FAddressLines: TCastleStringList;
    FPostalBox: TIfcLabel;
    FTown: TIfcLabel;
    FRegion: TIfcLabel;
    FPostalCode: TIfcLabel;
    FCountry: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Internal location for the address. }
    property InternalLocation: TIfcLabel read FInternalLocation write FInternalLocation;

    { The postal address. }
    property AddressLines: TCastleStringList read FAddressLines;

    { An address that is implied by an identifiable mail drop. }
    property PostalBox: TIfcLabel read FPostalBox write FPostalBox;

    { The name of a town. }
    property Town: TIfcLabel read FTown write FTown;

    { The name of a region. }
    property Region: TIfcLabel read FRegion write FRegion;

    { The code that is used by the country's postal service. }
    property PostalCode: TIfcLabel read FPostalCode write FPostalCode;

    { An ISO 3166 2-digit country code. }
    property Country: TIfcLabel read FCountry write FCountry;
  end;

  { Defined area of land on which the project construction is to be completed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSite.htm }
  TIfcSite = class(TIfcSpatialStructureElement)
  strict private
    FRefLatitude: TIfcCompoundPlaneAngleMeasure;
    FRefLongitude: TIfcCompoundPlaneAngleMeasure;
    FRefElevation: TIfcLengthMeasure;
    FLandTitleNumber: TIfcLabel;
    FSiteAddress: TIfcPostalAddress;
  published
    { World Latitude at reference poin. }
    property RefLatitude: TIfcCompoundPlaneAngleMeasure read FRefLatitude write FRefLatitude;

    { World Longitude at reference point. }
    property RefLongitude: TIfcCompoundPlaneAngleMeasure read FRefLongitude write FRefLongitude;

    { Datum elevation relative to sea level. }
    property RefElevation: TIfcLengthMeasure read FRefElevation write FRefElevation;

    { The land title number. }
    property LandTitleNumber: TIfcLabel read FLandTitleNumber write FLandTitleNumber;

    { Address given to the site for postal purposes. }
    property SiteAddress: TIfcPostalAddress read FSiteAddress write FSiteAddress;
  end;

  { Any built facility, like a building, bridge. }
  TIfcFacility = class(TIfcSpatialStructureElement)
  end;

  { Building facility.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuilding.htm }
  TIfcBuilding = class(TIfcFacility)
  strict private
    FElevationOfRefHeight: TIfcLengthMeasure;
    FElevationOfTerrain: TIfcLengthMeasure;
    FBuildingAddress: TIfcPostalAddress;
  published
    { Elevation above sea level of the reference height used for all storey elevation measures. }
    property ElevationOfRefHeight: TIfcLengthMeasure read FElevationOfRefHeight write FElevationOfRefHeight;

    { Elevation above the minimal terrain level around the foot print of the building. }
    property ElevationOfTerrain: TIfcLengthMeasure read FElevationOfTerrain write FElevationOfTerrain;

    { Address given to the building for postal purposes. }
    property BuildingAddress: TIfcPostalAddress read FBuildingAddress write FBuildingAddress;
  end;

  { Storey (~floor).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingStorey.htm }
  TIfcBuildingStorey = class(TIfcSpatialStructureElement)
  strict private
    FElevation: TIfcLengthMeasure;
  published
    { Elevation of the base of this storey, relative to the 0,00
      internal reference height of the building. }
    property Elevation: TIfcLengthMeasure read FElevation write FElevation;
  end;

  TIfcRelDecomposes = class abstract(TIfcRelationship)
  end;

  { Geometric and topological constraints that facilitate the physical connection of two objects. }
  TIfcConnectionGeometry = class abstract(TIfcPersistent)
  end;

  { Relationship provides the generalization of the connectivity
    between elements. It is a 1 to 1 relationship. }
  TIfcRelConnectsElements = class(TIfcRelConnects)
  strict private
    FConnectionGeometry: TIfcConnectionGeometry;
    FRelatingElement: TIfcElement;
    FRelatedElement: TIfcElement;
  published
    { Geometric shape representation of the connection geometry
      that is provided in the object coordinate system of the RelatingElement
      (mandatory) and in the object coordinate system of the RelatedElement (optionally). }
    property ConnectionGeometry: TIfcConnectionGeometry read FConnectionGeometry write FConnectionGeometry;

    { Reference to a subtype of IfcElement that is connected by the connection relationship
      in the role of RelatingElement. }
    property RelatingElement: TIfcElement read FRelatingElement write FRelatingElement;

    { Reference to a subtype of IfcElement that is connected by the connection relationship
      in the role of RelatedElement. }
    property RelatedElement: TIfcElement read FRelatedElement write FRelatedElement;
  end;
  TIfcRelConnectsElementsList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelConnectsElements>)
  end;

  TIfcRelInterferesElements = class(TIfcRelConnects)
  strict private
    // TODO FRelatingElement: TIfcInterferenceSelect;
    // TODO FRelatedElement: TIfcInterferenceSelect;
    FInterferenceGeometry: TIfcConnectionGeometry;
    FInterferenceType: TIfcIdentifier;
    FImpliedOrder: Boolean;
    FInterferenceSpace: TIfcSpatialZone;
  published
    { Reference to a subtype of IfcElement or IfcSpatialElement that is
      the RelatingElement in the interference relationship. }
    // TODO property RelatingElement: TIfcInterferenceSelect read FRelatingElement write FRelatingElement;

    { Reference to a subtype of IfcElement or IfcSpatialElement that is
      the RelatedElement in the interference relationship. }
    // TODO property RelatedElement: TIfcInterferenceSelect read FRelatedElement write FRelatedElement;

    { The geometric shape representation of the interference geometry that is
      provided in the object coordinate system of the RelatingElement (mandatory)
      and in the object coordinate system of the RelatedElement (optionally). }
    property InterferenceGeometry: TIfcConnectionGeometry read FInterferenceGeometry write FInterferenceGeometry;

    { Optional identifier that describes the nature of the interference. }
    property InterferenceType: TIfcIdentifier read FInterferenceType write FInterferenceType;

    { Logical value indicating if the RelatingElement is considered a source and
      the RelatedElement a target (giving a formal orientation to the relation).
      It shall be provided in regards to InterferenceGeometry usage and InterferenceType declaration. }
    property ImpliedOrder: Boolean read FImpliedOrder write FImpliedOrder;

    { Optional attribute that expresses the interfering space for IfcSpatialElement occurrences. }
    property InterferenceSpace: TIfcSpatialZone read FInterferenceSpace write FInterferenceSpace;
  end;
  TIfcRelInterferesElementsList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelInterferesElements>;

  { Relationship between an element and one projection element that creates a modifier to the shape of the element.}
  TIfcRelProjectsElement = class(TIfcRelDecomposes)
  strict private
    FRelatingElement: TIfcElement;
    FRelatedFeatureElement: TIfcFeatureElementAddition;
  published
    { Element at which a projection is created by the associated IfcProjectionElement. }
    property RelatingElement: TIfcElement read FRelatingElement write FRelatingElement;

    { Reference to the IfcFeatureElementAddition that defines an addition
      to the volume of the element, by using a Boolean addition operation.
      An example is a projection at the associated element. }
    property RelatedFeatureElement: TIfcFeatureElementAddition read FRelatedFeatureElement write FRelatedFeatureElement;
  end;
  TIfcRelProjectsElementList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelProjectsElement>;

  { Relationship that is made between two elements that require the realization
    of that relationship by means of further realizing elements. }
  TIfcRelConnectsWithRealizingElements = class(TIfcRelConnectsElements)
  strict private
    FRealizingElements: TIfcElementList;
    FConnectionType: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Rlements that realize a connection relationship. }
    property RealizingElements: TIfcElementList read FRealizingElements;

    { The type of the connection given for informal purposes,
      it may include labels, like 'joint', 'rigid joint', 'flexible joint', etc. }
    property ConnectionType: TIfcLabel read FConnectionType write FConnectionType;
  end;
  TIfcRelConnectsWithRealizingElementsList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelConnectsWithRealizingElements>;

  TIfcPhysicalOrVirtualEnum = (
    NotDefined,
    Physical,
    Virtual
  );

  TIfcInternalOrExternalEnum = (
    NotDefined,
    External,
    External_Earth,
    External_Fire,
    External_Water,
    Internal
  );

  { The space boundary defines the physical or virtual delimiter
    of a space by the relationship IfcRelSpaceBoundary to the surrounding
    elements. }
  TIfcRelSpaceBoundary = class(TIfcRelConnects)
  strict private
    // TODO FRelatingSpace: TIfcSpaceBoundarySelect;
    FRelatedBuildingElement: TIfcElement;
    FConnectionGeometry: TIfcConnectionGeometry;
    FPhysicalOrVirtualBoundary: TIfcPhysicalOrVirtualEnum;
    FInternalOrExternalBoundary: TIfcInternalOrExternalEnum;
  published
    { Reference to one space that is delimited by this boundary. }
    // TODO property RelatingSpace: TIfcSpaceBoundarySelect read FRelatingSpace write FRelatingSpace;

    { Reference to the Element that defines the Space Boundaries. }
    property RelatedBuildingElement: TIfcElement read FRelatedBuildingElement write FRelatedBuildingElement;

    { Physical representation of the space boundary.
      Provided as a curve or surface given within the LCS of the space. }
    property ConnectionGeometry: TIfcConnectionGeometry read FConnectionGeometry write FConnectionGeometry;

    { Defines whether the Space Boundary is PHYISICAL or VIRTUAL. }
    property PhysicalOrVirtualBoundary: TIfcPhysicalOrVirtualEnum read FPhysicalOrVirtualBoundary write FPhysicalOrVirtualBoundary
      default TIfcPhysicalOrVirtualEnum.NotDefined;

    { Defines whether the Space Boundary is INTERNAL, or EXTERNAL, i.e. adjacent to open space that can be a partially enclosed space, such as terrace. }
    property InternalOrExternalBoundary: TIfcInternalOrExternalEnum read FInternalOrExternalBoundary write FInternalOrExternalBoundary
      default TIfcInternalOrExternalEnum.NotDefined;
  end;
  TIfcRelSpaceBoundaryList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelSpaceBoundary>;

  { Relationship between an element and one to many coverings,
    which cover that element. }
  TIfcRelCoversBldgElements = class(TIfcRelConnects)
  strict private
    FRelatingBuildingElement: TIfcElement;
    // TODO FRelatedCoverings: TIfcCoveringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the element that is covered. }
    property RelatingBuildingElement: TIfcElement read FRelatingBuildingElement write FRelatingBuildingElement;

    { Relationship to the set of coverings that are assigned to this element. }
    // TODO property RelatedCoverings: TIfcCoveringList read FRelatedCoverings write FRelatedCoverings;
  end;
  TIfcRelCoversBldgElementsList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelCoversBldgElements>;

  { Relationship between an element and one to many surface feature elements
    that adhere to the surface of the element. }
  TIfcRelAdheresToElement = class(TIfcRelDecomposes)
  strict private
    FRelatingElement: TIfcElement;
    // TODO FRelatedSurfaceFeatures: TIfcSurfaceFeatureList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Element to which the IfcSurfaceFeature is adhered to. }
    property RelatingElement: TIfcElement read FRelatingElement write FRelatingElement;

    { The IfcSurfaceFeature(s) that adheres to the surface of the parent element. }
    // TODO property RelatedSurfaceFeatures: TIfcSurfaceFeatureList read FRelatedSurfaceFeatures write FRelatedSurfaceFeatures;
  end;
  TIfcRelAdheresToElementList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelAdheresToElement>;

  { Generalization of all components that make up a facility. }
  TIfcElement = class abstract(TIfcProduct)
  strict private
    FTag: TIfcIdentifier;
    FFillsVoids: TIfcRelFillsElementList;
    FConnectedTo: TIfcRelConnectsElementsList;
    FIsInterferedByElements: TIfcRelInterferesElementsList;
    FInterferesElements: TIfcRelInterferesElementsList;
    FHasProjections: TIfcRelProjectsElementList;
    FHasOpenings: TIfcRelVoidsElementList;
    FIsConnectionRealization: TIfcRelConnectsWithRealizingElementsList;
    FProvidesBoundaries: TIfcRelSpaceBoundaryList;
    FConnectedFrom: TIfcRelConnectsElementsList;
    FContainedInStructure: TIfcRelContainedInSpatialStructureList;
    FHasCoverings: TIfcRelCoversBldgElementsList;
    FHasSurfaceFeatures: TIfcRelAdheresToElementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; override;

    { Add an opening that creates a hole (or just niche) in this element.
      You can add any TIfcFeatureElementSubtraction, but typically you will
      add TIfcOpeningElement.
      This automatically creates TIfcRelVoidsElement relationship between
      this element and the opening. }
    procedure AddOpening(const Opening: TIfcFeatureElementSubtraction);

    { Add a connected element, e.g. window should be connected to the wall.
      This adds TIfcRelConnectsElements relationship and extends the ConnectedTo,
      ConnectedFrom lists.

      Spec of https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWall.htm
      says: " Walls with openings that have already been modeled within
      the enclosing geometry may use the relationship IfcRelConnectsElements
      to associate the wall with embedded elements such as doors and windows."

      The connected element is not rendered just because it is connected.
      It should still be also added using e.g.
      @link(TIfcSpatialElement.AddContainedElement) to be rendered. }
    procedure AddConnected(const Connected: TIfcElement);
  published
    { The tag (or label) identifier at the particular instance of a product,
      e.g. the serial number, or the position number.
      Note: This obscures TComponent.Tag property. }
    property Tag: TIfcIdentifier read FTag write FTag;

    { Reference to the IfcRelFillsElement relationship that puts the element as a filling into the opening created within another element. }
    property FillsVoids: TIfcRelFillsElementList read FFillsVoids write FFillsVoids;

    { Reference to the element connection relationship. }
	  property ConnectedTo: TIfcRelConnectsElementsList read FConnectedTo;

    { Reference to the interference relationship to indicate the element that is interfered. }
	  property IsInterferedByElements: TIfcRelInterferesElementsList read FIsInterferedByElements;

    { Reference to the interference relationship to indicate the element that interferes. }
	  property InterferesElements: TIfcRelInterferesElementsList read FInterferesElements;

    { Projection relationship that adds a feature (using a Boolean union) to the IfcBuiltElement. }
	  property HasProjections: TIfcRelProjectsElementList read FHasProjections;

    { List of openings associated with this element. }
	  property HasOpenings: TIfcRelVoidsElementList read FHasOpenings;

    { Reference to the connection relationship with realizing element. }
    property IsConnectionRealization: TIfcRelConnectsWithRealizingElementsList read FIsConnectionRealization;

    { Space boundaries by virtue of the relationship IfcRelSpaceBoundary. }
    property ProvidesBoundaries: TIfcRelSpaceBoundaryList read FProvidesBoundaries;

    { Element connection relationship. }
    property ConnectedFrom: TIfcRelConnectsElementsList read FConnectedFrom;

    { Containment relationship to the spatial structure element, to which the element is primarily associated. }
    property ContainedInStructure: TIfcRelContainedInSpatialStructureList read FContainedInStructure;

    // { Reference to IfcCovering by virtue of the relationship IfcRelCoversBldgElements. }
    property HasCoverings: TIfcRelCoversBldgElementsList read FHasCoverings;

    { Reference to the IfcRelAdheresToElement relationship that adheres a IfcSurfaceFeature to an element. }
    property HasSurfaceFeatures: TIfcRelAdheresToElementList read FHasSurfaceFeatures;
  end;
  TIfcElementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcElement>)
  end;

  { Represents complex element assemblies aggregated from several elements,
    such as discrete elements, building elements, or other elements.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementAssembly.htm }
  TIfcElementAssembly = class(TIfcElement)
  strict private
    FAssemblyPlace: TIfcAssemblyPlaceEnum;
    FPredefinedType: TIfcElementAssemblyTypeEnum;
  published
    { A designation of where the assembly is intended to take place. }
    property AssemblyPlace: TIfcAssemblyPlaceEnum read FAssemblyPlace write FAssemblyPlace
      default TIfcAssemblyPlaceEnum.NotDefined;

    { A list of types to further identify the object. }
    property PredefinedType: TIfcElementAssemblyTypeEnum read FPredefinedType write FPredefinedType
      default TIfcElementAssemblyTypeEnum.NotDefined;
  end;

  { Representation for minor items included in, added to or connecting
    to or between elements, which usually are not of interest from
    the overall building structure viewpoint. However, these small parts
    may have vital and load carrying functions within the construction.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementComponent.htm }
  TIfcElementComponent = class(TIfcElement)
  end;

  { A reinforcing element represents bars, wires, strands, meshes,
    tendons, and other components embedded in concrete in such a manner
    that the reinforcement and the concrete act together in resisting forces.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingElement.htm }
  TIfcReinforcingElement = class abstract(TIfcElementComponent)
  strict private
    FSteelGrade: TIfcLabel;
  published
    { @deprecated
      Attribute deprecated. Use material association at IfcReinforcingElementType instead. }
    property SteelGrade: TIfcLabel read FSteelGrade write FSteelGrade;
  end;

  { A reinforcing bar is usually made of steel with manufactured deformations
    in the surface, and used in concrete and masonry construction to provide
    additional strength.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingBar.htm }
  TIfcReinforcingBar = class(TIfcReinforcingElement)
  strict private
    FNominalDiameter: TIfcPositiveLengthMeasure;
    FCrossSectionArea: TIfcAreaMeasure;
    FBarLength: TIfcPositiveLengthMeasure;
    FPredefinedType: TIfcReinforcingBarTypeEnum;
    FBarSurface: TIfcReinforcingBarSurfaceEnum;
  published
    { @deprecated }
    property NominalDiameter: TIfcPositiveLengthMeasure read FNominalDiameter write FNominalDiameter;

    { The effective cross-section area of the reinforcing bar or group of bars. }
    property CrossSectionArea: TIfcAreaMeasure read FCrossSectionArea write FCrossSectionArea;

    { @deprecated }
    property BarLength: TIfcPositiveLengthMeasure read FBarLength write FBarLength;

    { The role, purpose or usage of the bar. }
    property PredefinedType: TIfcReinforcingBarTypeEnum read FPredefinedType write FPredefinedType;

    { @deprecated }
    property BarSurface: TIfcReinforcingBarSurfaceEnum read FBarSurface write FBarSurface;
  end;

  { Represents major components as subordinate parts of a building element.
    Typical usage examples include precast concrete sandwich walls,
    where the layers may have different geometry representations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingElementPart.htm }
  TIfcBuildingElementPart = class(TIfcElementComponent)
  strict private
    FPredefinedType: TIfcBuildingElementPartTypeEnum;
  published
    { Subtype of building element part. }
    property PredefinedType: TIfcBuildingElementPartTypeEnum read FPredefinedType write FPredefinedType
      default TIfcBuildingElementPartTypeEnum.NotDefined;
  end;

  { Generalization of all elements that participate in a distribution system.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionElement.htm }
  TIfcDistributionElement = class(TIfcElement)
    // TODO HasPorts
  end;

  { Distribution system that facilitate the distribution of energy or matter,
    such as air, water or power.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionFlowElement.htm }
  TIfcDistributionFlowElement = class(TIfcDistributionElement)
    // TODO HasControlElements
  end;

  { Device used to perform energy conversion or heat transfer and
    typically participates in a flow distribution system.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEnergyConversionDevice.htm }
  TIfcEnergyConversionDevice = class(TIfcDistributionFlowElement)
  end;

  { Closed, pressure-rated vessel in which water or other fluid is heated
    using an energy source such as natural gas, heating oil, or electricity.
    The fluid in the vessel is then circulated out of the boiler for use in
    various processes or heating applications.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoiler.htm }
  TIfcBoiler = class(TIfcEnergyConversionDevice)
  strict private
    FPredefinedType: TIfcBoilerTypeEnum;
  published
    { List of types to further identify the object. }
    property PredefinedType: TIfcBoilerTypeEnum read FPredefinedType write FPredefinedType
      default TIfcBoilerTypeEnum.NotDefined;
  end;

  { Permanently attached element that acts as a terminus or
    beginning of a distribution system (such as an air outlet, drain,
    water closet, or sink).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowTerminal.htm }
  TIfcFlowTerminal = class(TIfcDistributionFlowElement)
  end;

  { A sanitary terminal is a fixed appliance or terminal usually supplied
    with water and used for drinking, cleaning or foul water disposal
    or that is an item of equipment directly used with such an appliance
    or terminal.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSanitaryTerminal.htm }
  TIfcSanitaryTerminal = class(TIfcFlowTerminal)
  strict private
    FPredefinedType: TIfcSanitaryTerminalTypeEnum;
  published
    property PredefinedType: TIfcSanitaryTerminalTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSanitaryTerminalTypeEnum.NotDefined;
  end;

  { An air terminal is a terminating or origination point for the transfer
    of air between distribution system(s) and one or more spaces.
    It can also be used for the transfer of air between adjacent spaces.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirTerminal.htm }
  TIfcAirTerminal = class(TIfcFlowTerminal)
  strict private
    FPredefinedType: TIfcAirTerminalTypeEnum;
  published
    property PredefinedType: TIfcAirTerminalTypeEnum read FPredefinedType write FPredefinedType
      default TIfcAirTerminalTypeEnum.NotDefined;
  end;

  { Generalization of all furniture related objects.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFurnishingElement.htm }
  TIfcFurnishingElement = class(TIfcElement)
  end;

  { Components of modular furniture which are not directly placed
    in a building structure but aggregated inside furniture.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSystemFurnitureElement.htm }
  TIfcSystemFurnitureElement = class(TIfcFurnishingElement)
  strict private
    FPredefinedType: TIfcSystemFurnitureElementTypeEnum;
  published
    { List of types to further identify the object. }
    property PredefinedType: TIfcSystemFurnitureElementTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSystemFurnitureElementTypeEnum.NotDefined;
  end;

  { Complete furnishings such as a table, desk, chair, or cabinet,
    which may or may not be permanently attached to a building structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFurniture.htm }
  TIfcFurniture = class(TIfcFurnishingElement)
  strict private
    FPredefinedType: TIfcFurnitureTypeEnum;
  published
    { List of types to further identify the object. }
    property PredefinedType: TIfcFurnitureTypeEnum read FPredefinedType write FPredefinedType
      default TIfcFurnitureTypeEnum.NotDefined;
  end;

  { All existence dependent elements which modify the shape and appearance
    of the associated master element.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFeatureElement.htm }
  TIfcFeatureElement = class abstract(TIfcElement)
  end;

  { Existence dependent element which modifies the shape and appearance
    of the associated master element. }
  TIfcFeatureElementSubtraction = class abstract(TIfcFeatureElement)
  strict private
    FVoidsElements: TIfcRelVoidsElementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the Voids Relationship that uses this Opening Element
      to create a void within an Element. }
    property VoidsElements: TIfcRelVoidsElementList read FVoidsElements;
  end;

  { Relationship between an opening element and an element that fills
    (or partially fills) the opening element. It is an one-to-one relationship.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelFillsElement.htm }
  TIfcRelFillsElement = class(TIfcRelConnects)
  strict private
    FRelatingOpeningElement: TIfcOpeningElement;
    FRelatedBuildingElement: TIfcElement;
  published
    { Opening Element being filled by virtue of this relationship. }
    property RelatingOpeningElement: TIfcOpeningElement read FRelatingOpeningElement write FRelatingOpeningElement;
    { Reference to element that occupies fully or partially the associated opening. }
    property RelatedBuildingElement: TIfcElement read FRelatedBuildingElement write FRelatedBuildingElement;
  end;
  TIfcRelFillsElementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelFillsElement>)
  end;

  { Used by TIfcOpeningElement.PredefinedType. }
  TIfcOpeningElementTypeEnum = (NotDefined, Opening, Recess, UserDefined);

  { Opening: a void within any element that has physical manifestation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOpeningElement.htm }
  TIfcOpeningElement = class(TIfcFeatureElementSubtraction)
  strict private
    FPredefinedType: TIfcOpeningElementTypeEnum;
    FHasFillings: TIfcRelFillsElementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PredefinedType: TIfcOpeningElementTypeEnum read FPredefinedType write FPredefinedType
      default TIfcOpeningElementTypeEnum.NotDefined;

    property HasFillings: TIfcRelFillsElementList read FHasFillings;
  end;

  { Part of the construction of a built facility, built elements are all
    physically existent and tangible things.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuiltElement.htm }
  TIfcBuiltElement = class(TIfcElement)
  end;

  { Beam is a horizontal, or nearly horizontal, structural member that is
    capable of withstanding load primarily by resisting bending.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBeam.htm }
  TIfcBeam = class(TIfcBuiltElement)
  strict private
    FPredefinedType: TIfcBeamTypeEnum;
  published
    { Predefined generic type for a beam that is specified in an enumeration. }
    property PredefinedType: TIfcBeamTypeEnum read FPredefinedType write FPredefinedType default TIfcBeamTypeEnum.NotDefined;
  end;

  { @deprecated Removed in IFC4.3.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_1/FINAL/HTML/schema/ifcsharedbldgelements/lexical/ifcbeamstandardcase.htm }
  TIfcBeamStandardCase = class(TIfcBeam)
  end;

  TIfcDoorTypeEnum = (
    NotDefined,
    Boom_Barrier,
    Door,
    Gate,
    Trapdoor,
    Turnstile,
    UserDefined
  );

  TIfcDoorTypeOperationEnum = (
    NotDefined,
    Double_Door_Double_Swing,
    Double_Door_Folding,
    Double_Door_Lifting_Vertical,
    Double_Door_Single_Swing,
    Double_Door_Single_Swing_Opposite_Left,
    Double_Door_Single_Swing_Opposite_Right,
    Double_Door_Sliding,
    Double_Swing_Left,
    Double_Swing_Right,
    Folding_To_Left,
    Folding_To_Right,
    Lifting_Horizontal,
    Lifting_Vertical_Left,
    Lifting_Vertical_Right,
    Revolving,
    Revolving_Vertical,
    RollingUp,
    Single_Swing_Left,
    Single_Swing_Right,
    Sliding_To_Left,
    Sliding_To_Right,
    Swing_Fixed_Left,
    Swing_Fixed_Right,
    UserDefined
  );

  { Door. }
  TIfcDoor = class(TIfcBuiltElement)
  strict private
    FOverallHeight: TIfcPositiveLengthMeasure;
    FOverallWidth: TIfcPositiveLengthMeasure;
    FPredefinedType: TIfcDoorTypeEnum;
    FOperationType: TIfcDoorTypeOperationEnum;
    FUserDefinedOperationType: TIfcLabel;
  published
    { Overall measure of the height, it reflects the Z Dimension of
      a bounding box, enclosing the body of the door opening.
      If omitted, the OverallHeight should be taken from the geometric
      representation of the IfcOpeningElement in which the door is inserted.

      NOTE The body of the door might be taller then the door opening
      (e.g. in cases where the door lining includes a casing).
      In these cases the OverallHeight shall still be given
      as the door opening height, and not as the total height of the door lining. }
    property OverallHeight: TIfcPositiveLengthMeasure read FOverallHeight write FOverallHeight;

    { Overall measure of the width, it reflects the X Dimension of
      a bounding box, enclosing the body of the door opening.
      If omitted, the OverallWidth should be taken from the geometric
      representation of the IfcOpeningElement in which the door is inserted.

      NOTE The body of the door might be wider then the door opening
      (e.g. in cases where the door lining includes a casing).
      In these cases the OverallWidth shall still be given
      as the door opening width, and not as the total width of the door lining. }
    property OverallWidth: TIfcPositiveLengthMeasure read FOverallWidth write FOverallWidth;

    { A list of types to further identify the object.
      Some property sets may be specifically applicable to one of these types. }
    property PredefinedType: TIfcDoorTypeEnum read FPredefinedType write FPredefinedType;

    { Type defining the general layout and operation of the door type
      in terms of the partitioning of panels and panel operations.

      NOTE The OperationType shall only be used, if no type object IfcDoorType
      is assigned, providing its own IfcDoorType.OperationType. }
    property OperationType: TIfcDoorTypeOperationEnum read FOperationType write FOperationType;

    { Designator for the user defined operation type, shall only be provided,
      if the value of OperationType is set to TIfcDoorTypeOperationEnum.UserDefined. }
    property UserDefinedOperationType: TIfcLabel read FUserDefinedOperationType write FUserDefinedOperationType;
  end;

  { Shared information for occurrences of doors.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDoorType.htm }
  TIfcDoorType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcDoorTypeEnum;
    FOperationType: TIfcDoorTypeOperationEnum;
    FParameterTakesPrecedence: Boolean;
    FUserDefinedOperationType: TIfcLabel;
  published
    { A list of types to further identify the object. }
    property PredefinedType: TIfcDoorTypeEnum read FPredefinedType write FPredefinedType default TIfcDoorTypeEnum.NotDefined;

    { Type defining the general layout and operation of the door type
      in terms of the partitioning of panels and panel operations. }
    property OperationType: TIfcDoorTypeOperationEnum read FOperationType write FOperationType default TIfcDoorTypeOperationEnum.NotDefined;

    { Does the parameter given in the attached
      lining and panel properties exactly define the geometry (TRUE),
      or whether the attached style shape take precedence (FALSE).
      In the last case the parameter have only informative value.

      TODO: If not provided, no such information can be inferred.
      Should we have 3rd value and make this enum? }
    property ParameterTakesPrecedence: Boolean read FParameterTakesPrecedence write FParameterTakesPrecedence default false;

    { Designator for the user defined operation type, shall only be provided,
      if the value of OperationType is set to TIfcDoorTypeOperationEnum.UserDefined. }
    property UserDefinedOperationType: TIfcLabel read FUserDefinedOperationType write FUserDefinedOperationType;
  end;

  { Component of the construction that may enclose a space vertically.
    The slab may provide the lower support (floor) or upper construction
    (roof slab) in any space in a building.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSlab.htm }
  TIfcSlab = class(TIfcBuiltElement)
  strict private
    FPredefinedType: TIfcSlabTypeEnum;
  published
    property PredefinedType: TIfcSlabTypeEnum read FPredefinedType write FPredefinedType
      default TIfcSlabTypeEnum.NotDefined;
  end;

  { @deprecated Deprecated and later removed in IFC 4.3.
    Defined only to handle old IFC models,
    testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/slab-standard-case.ifcjson

    Old spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_1/FINAL/HTML/schema/ifcsharedbldgelements/lexical/ifcslabstandardcase.htm }
  TIfcSlabStandardCase = class(TIfcSlab)
  end;

  { Proxy definition that provides the same functionality as subtypes of
    IfcBuiltElement, but without having a predefined meaning
    of the special type of building element it represents.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingElementProxy.htm }
  TIfcBuildingElementProxy = class(TIfcBuiltElement)
  strict private
    FPredefinedType: TIfcBuildingElementProxyTypeEnum;
  published
    property PredefinedType: TIfcBuildingElementProxyTypeEnum read FPredefinedType write FPredefinedType
      default TIfcBuildingElementProxyTypeEnum.NotDefined;
  end;

  TIfcColumnTypeEnum = (
    NotDefined,
    Column,
    PierStem,
    PierStem_Segment,
    Pilaster,
    StandColumn,
    UserDefined
  );

  { Vertical structural or architectural member which often is aligned with
    a structural grid intersection.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcColumn.htm }
  TIfcColumn = class(TIfcBuiltElement)
  strict private
    FPredefinedType: TIfcColumnTypeEnum;
  published
    { Predefined generic type for a column that is specified in an enumeration. }
    property PredefinedType: TIfcColumnTypeEnum read FPredefinedType write FPredefinedType
      default TIfcColumnTypeEnum.NotDefined;
  end;

  { @deprecated Deprecated and later removed in IFC 4.3.
    Defined only to handle old IFC models
    Old spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_1/FINAL/HTML/schema/ifcsharedbldgelements/lexical/ifccolumnstandardcase.htm }
  TIfcColumnStandardCase = class(TIfcColumn)
  end;

  TIfcWallTypeEnum = (
    NotDefined,
    ElementedWall,
    Movable,
    Parapet,
    Partitioning,
    PlumbingWall,
    Polygonal,
    RetainingWall,
    Shear,
    SolidWall,
    Standard,
    WaveWall,
    UserDefined
  );

  { Wall represents a vertical construction that may bound or subdivide spaces.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWall.htm }
  TIfcWall = class(TIfcBuiltElement)
  strict private
    FPredefinedType: TIfcWallTypeEnum;
  published
    { Predefined generic type for a wall. }
    property PredefinedType: TIfcWallTypeEnum read FPredefinedType write FPredefinedType
      default TIfcWallTypeEnum.NotDefined;
  end;

  { @deprecated Deprecated and later removed in IFC 4.3.
    Defined only to handle old IFC models,
    testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/wall-elemented-case.ifcjson .

    Old spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_1/FINAL/HTML/schema/ifcsharedbldgelements/lexical/ifcwallelementedcase.htm }
  TIfcWallElementedCase = class(TIfcWall)
  end;

  { Shared information for occurrences of walls.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWallType.htm }
  TIfcWallType = class(TIfcBuiltElementType)
  strict private
    FPredefinedType: TIfcWallTypeEnum;
  published
    { Identifies the predefined types of a wall element from which
      the type required may be set. }
    property PredefinedType: TIfcWallTypeEnum read FPredefinedType write FPredefinedType default TIfcWallTypeEnum.NotDefined;
  end;

  { Wall with certain constraints for the provision of parameters and
    with certain constraints for the geometric representation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWallStandardCase.htm }
  TIfcWallStandardCase = class(TIfcWall)
  end;

  { Window building element.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindow.htm }
  TIfcWindow = class(TIfcBuiltElement)
  strict private
    FOverallHeight: TIfcPositiveLengthMeasure;
    FOverallWidth: TIfcPositiveLengthMeasure;
    FPredefinedType: TIfcWindowTypeEnum;
    FPartitioningType: TIfcWindowTypePartitioningEnum;
    FUserDefinedPartitioningType: TIfcLabel;
  published
    { Overall measure of the height, it reflects the Z Dimension of
      a bounding box, enclosing the window opening. }
    property OverallHeight: TIfcPositiveLengthMeasure read FOverallHeight write FOverallHeight {$ifdef FPC}default 0.0{$endif};

    { Overall measure of the width, it reflects the X Dimension of
      a bounding box, enclosing the window opening. }
    property OverallWidth: TIfcPositiveLengthMeasure read FOverallWidth write FOverallWidth {$ifdef FPC}default 0.0{$endif};

    { Predefined generic type for a window that is specified in an enumeration.
      By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PredefinedType: TIfcWindowTypeEnum read FPredefinedType write FPredefinedType
      default TIfcWindowTypeEnum.NotDefined;

    { General layout of the window in terms of the partitioning of panels.
      By default NotDefined if not specified, we deliberately made NotDefined
      the enum value with memory representation 0 in Pascal. }
    property PartitioningType: TIfcWindowTypePartitioningEnum read FPartitioningType write FPartitioningType
      default TIfcWindowTypePartitioningEnum.NotDefined;

    { Shall only be provided, if the value of PartitioningType is set to
      TIfcWindowTypePartitioningEnum.NotDefined. }
    property UserDefinedPartitioningType: TIfcLabel read FUserDefinedPartitioningType write FUserDefinedPartitioningType;
  end;

  { Relationship between a building element and one opening element that
    creates a void in the element. It is a one-to-one relationship.
    This relationship implies a boolean operation of subtraction between
    the geometric bodies of the element and the opening.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelVoidsElement.htm }
  TIfcRelVoidsElement = class(TIfcRoot)
  strict private
    FRelatingBuildingElement: TIfcElement;
    FRelatedOpeningElement: TIfcFeatureElementSubtraction;
  public
    function BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
  published
    property RelatingBuildingElement: TIfcElement read FRelatingBuildingElement write FRelatingBuildingElement;
    property RelatedOpeningElement: TIfcFeatureElementSubtraction read FRelatedOpeningElement write FRelatedOpeningElement;
  end;
  TIfcRelVoidsElementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelVoidsElement>)
  end;

  { General composition/decomposition (or whole/part) relationship.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAggregates.htm }
  TIfcRelAggregates = class(TIfcRelDecomposes)
  strict private
    FRelatingObject: TIfcObjectDefinition;
    FRelatedObjects: TIfcObjectDefinitionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
  published
    property RelatingObject: TIfcObjectDefinition read FRelatingObject write FRelatingObject;
    property RelatedObjects: TIfcObjectDefinitionList read FRelatedObjects;
  end;
  TIfcRelAggregatesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelAggregates>)
  end;

  { Type for selecting between more specialised select types IfcSimpleValue,
    IfcMeasureValue and IfcDerivedMeasureValue.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcValue.htm }
  TIfcValue = class abstract(TIfcPersistent)
  end;

  { Real (float) value wrapped in a class, so it can be used where TIfcValue is expected. }
  TIfcReal_Boxed = class(TIfcValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  TIfcMeasureValue = class(TIfcValue)
  end;

  { Value of an angle in a plane.

    Usually measured in radian (rad, m/m = 1), but also grads may be used.
    The grad unit has to be declared as a conversion based unit based on
    radian unit.

    In IFC spec, this is not a class. But in Pascal we express it as class
    descending from @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMeasureValue.htm
    IfcMeasureValue) as this makes sense looking at IFC JSON structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlaneAngleMeasure.htm }
  TIfcPlaneAngleMeasure = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { An area measure is the value of the extent of a surface.
    Usually measured in square metre (m2).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAreaMeasure.htm }
  TIfcAreaMeasure_Boxed = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Value of the amount of matter that a body contains.
    Usually measured in kilograms (kg) or grams (g).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMassMeasure.htm }
  TIfcMassMeasure = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Value of a distance.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLengthMeasure.htm }
  TIfcLengthMeasure_Boxed = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Measure of the quantity of a medium acting on a unit area.
    Usually measured in Pascals (Pa, N/m2).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPressureMeasure.htm }
  TIfcPressureMeasure = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Value of the solid content of a body.
    Usually measured in cubic metre (m3).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVolumeMeasure.htm }
  TIfcVolumeMeasure = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Value for the degree of heat of a body.
    Usually measured in degrees Kelvin (K).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcThermodynamicTemperatureMeasure.htm }
  TIfcThermodynamicTemperatureMeasure = class(TIfcMeasureValue)
  strict private
    FValue: TIfcReal;
  published
    property Value: TIfcReal read FValue write FValue;
  end;

  { Abstract unit: a physical quantity, with a value of one, which is used as
    a standard in terms of which other quantities are expressed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUnit.htm

    This is not a class, but a type choice in IFC standard.
    We express it as class, as this works: all 3 choices can just descend from this class. }
  TIfcUnit = class abstract(TIfcPersistent)
  end;
  TIfcUnitList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcUnit>)
  end;

  { A set of units which may be assigned. }
  TIfcUnitAssignment = class(TIfcPersistent)
  strict private
    FUnits: TIfcUnitList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Units to be included within a unit assignment. }
    property Units: TIfcUnitList read FUnits;
  end;

  { A derived unit element is one of the unit quantities which makes
    up a derived unit.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDerivedUnitElement.htm }
  TIfcDerivedUnitElement = class(TIfcPersistent)
  strict private
    FUnit: TIfcNamedUnit;
    FExponent: Integer;
  published
    { The fixed quantity which is used as the mathematical factor. }
    property &Unit: TIfcNamedUnit read FUnit write FUnit;

    { The power that is applied to the unit attribute. }
    property Exponent: Integer read FExponent write FExponent;
  end;
  TIfcDerivedUnitElementList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcDerivedUnitElement>;

  TIfcDerivedUnitEnum = (
    UserDefined,
    AccelerationUnit,
    AngularVelocityUnit,
    AreaDensityUnit,
    CompoundPlaneAngleUnit,
    CurvatureUnit,
    DynamicViscosityUnit,
    HeatFluxDensityUnit,
    HeatingValueUnit,
    IntegerCountRateUnit,
    IonConcentrationUnit,
    IsothermalMoistureCapacityUnit,
    KinematicViscosityUnit,
    LinearForceUnit,
    LinearMomentUnit,
    LinearStiffnessUnit,
    LinearVelocityUnit,
    LuminousIntensityDistributionUnit,
    MassDensityUnit,
    MassFlowRateUnit,
    MassPerLengthUnit,
    ModulusOfElasticityUnit,
    ModulusOfLinearSubgradeReactionUnit,
    ModulusOfRotationalSubgradeReactionUnit,
    ModulusOfSubgradeReactionUnit,
    MoistureDiffusivityUnit,
    MolecularWeightUnit,
    MomentOfInertiaUnit,
    pHUnit,
    PlanarForceUnit,
    RotationalFrequencyUnit,
    RotationalMassUnit,
    RotationalStiffnessUnit,
    SectionAreaIntegralUnit,
    SectionModulusUnit,
    ShearModulusUnit,
    SoundPowerLevelUnit,
    SoundPowerUnit,
    SoundPressureLevelUnit,
    SoundPressureUnit,
    SpecificHeatCapacityUnit,
    TemperatureGradientUnit,
    TemperatureRateOfChangeUnit,
    ThermalAdmittanceUnit,
    ThermalConductanceUnit,
    ThermalExpansionCoefficientUnit,
    ThermalResistanceUnit,
    ThermalTransmittanceUnit,
    TorqueUnit,
    VaporPermeabilityUnit,
    VolumetricFlowRateUnit,
    WarpingConstantUnit,
    WarpingMomentUnit
  );

  { A derived unit is a unit that is formed from an expression of other units. }
  TIfcDerivedUnit = class(TIfcUnit)
  strict private
    FElements: TIfcDerivedUnitElementList;
    FUnitType: TIfcDerivedUnitEnum;
    FUserDefinedType: TIfcLabel;
    FName: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The group of units and their exponents that define the derived unit. }
    property Elements: TIfcDerivedUnitElementList read FElements;

    { Type of the derived unit chosen from an enumeration of derived unit types
      for use in IFC models. }
    property UnitType: TIfcDerivedUnitEnum read FUnitType write FUnitType;

    { Type of the derived unit if the UnitType attribute is set to UserDefined. }
    property UserDefinedType: TIfcLabel read FUserDefinedType write FUserDefinedType;

    { Name of the unit in addition to the unit type, particularly when the derived
      unit elements refer to conversion or context based units. }
    property Name: TIfcLabel read FName write FName;

    { Dimensional exponents derived. }
    // Not necessary in CGE now: function Dimensions: TIfcDimensionalExponents;
  end;

  { Define currency for money. }
  TIfcMonetaryUnit = class(TIfcUnit)
  strict private
    FCurrency: TIfcLabel;
  published
    { Code or name of the currency. Permissible values are the three-letter
      alphabetic currency codes as per ISO 4217,for example
      CNY, EUR, GBP, JPY, USD. }
    property Currency: TIfcLabel read FCurrency write FCurrency;
  end;

  { The dimensionality of any quantity can be expressed as a product
    of powers of the dimensions of base quantities. The dimensional exponents
    entity defines the powers of the dimensions of the base quantities.
    All the physical quantities are founded on seven base quantities
    (ISO 31 (clause 2)). }
  TIfcDimensionalExponents = class(TIfcPersistent)
  strict private
    FLengthExponent: Integer;
    FMassExponent: Integer;
    FTimeExponent: Integer;
    FElectricCurrentExponent: Integer;
    FThermodynamicTemperatureExponent: Integer;
    FAmountOfSubstanceExponent: Integer;
    FLuminousIntensityExponent: Integer;
  published
    property LengthExponent: Integer read FLengthExponent write FLengthExponent default 0;
    property MassExponent: Integer read FMassExponent write FMassExponent default 0;
    property TimeExponent: Integer read FTimeExponent write FTimeExponent default 0;
    property ElectricCurrentExponent: Integer read FElectricCurrentExponent write FElectricCurrentExponent default 0;
    property ThermodynamicTemperatureExponent: Integer read FThermodynamicTemperatureExponent write FThermodynamicTemperatureExponent default 0;
    property AmountOfSubstanceExponent: Integer read FAmountOfSubstanceExponent write FAmountOfSubstanceExponent default 0;
    property LuminousIntensityExponent: Integer read FLuminousIntensityExponent write FLuminousIntensityExponent default 0;
  end;

  TIfcUnitEnum = (
    UserDefined,
    AbsorbedDoseUnit,
    AmountOfSubstanceUnit,
    AreaUnit,
    DoseEquivalentUnit,
    ElectricCapacitanceUnit,
    ElectricChargeUnit,
    ElectricConductanceUnit,
    ElectricCurrentUnit,
    ElectricResistanceUnit,
    ElectricVoltageUnit,
    EnergyUnit,
    ForceUnit,
    FrequencyUnit,
    IlluminanceUnit,
    InductanceUnit,
    LengthUnit,
    LuminousFluxUnit,
    LuminousIntensityUnit,
    MagneticFluxDensityUnit,
    MagneticFluxUnit,
    MassUnit,
    PlaneAngleUnit,
    PowerUnit,
    PressureUnit,
    RadioactivityUnit,
    SolidAngleUnit,
    ThermodynamicTemperatureUnit,
    TimeUnit,
    VolumeUnit
  );

  { A unit quantity associated with the word, or group of words,
    by which the unit is identified.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcNamedUnit.htm }
  TIfcNamedUnit = class(TIfcUnit)
  strict private
    FDimensions: TIfcDimensionalExponents;
    FUnitType: TIfcUnitEnum;
  published
    { Dimensional exponents of the SI base units by which the named unit is defined. }
    property Dimensions: TIfcDimensionalExponents read FDimensions write FDimensions;

    { The type of the unit. }
    property UnitType: TIfcUnitEnum read FUnitType write FUnitType default TIfcUnitEnum.UserDefined;
  end;

  TIfcSIPrefix = (
    NotDefined, //< Enum value added in CGE.
    Atto,
    Centi,
    Deca,
    Deci,
    Exa,
    Femto,
    Giga,
    Hecto,
    Kilo,
    Mega,
    Micro,
    Milli,
    Nano,
    Peta,
    Pico,
    Tera
  );

  TIfcSIUnitName = (
    Ampere,
    Becquerel,
    Candela,
    Coulomb,
    Cubic_Metre,
    Degree_Celsius,
    Farad,
    Gram,
    Gray,
    Henry,
    Hertz,
    Joule,
    Kelvin,
    Lumen,
    Lux,
    Metre,
    Mole,
    Newton,
    Ohm,
    Pascal,
    Radian,
    Second,
    Siemens,
    Sievert,
    Square_Metre,
    Steradian,
    Tesla,
    Volt,
    Watt,
    Weber
  );

  { Standard base SI units such as meter and second,
    and derived SI units such as Pascal, square meter and cubic meter. }
  TIfcSIUnit = class(TIfcNamedUnit)
  strict private
    FPrefix: TIfcSIPrefix;
    FName: TIfcSIUnitName;
  published
    { The SI Prefix for defining decimal multiples and submultiples of the unit. }
    property Prefix: TIfcSIPrefix read FPrefix write FPrefix
      default TIfcSIPrefix.NotDefined;

    { The word, or group of words, by which the SI unit is referred to. }
    property Name: TIfcSIUnitName read FName write FName;

    // Not necessary in CGE now: We could calculate this:
    // Dimensions: TIfcDimensionalExponents
    // This attribute is formally derived.
  end;

  { This has two usages:

    1. For representing measure value together with its unit on the
    entity type attribute level; thus overriding the IFC model global
    unit assignments.

    2. For conversion based unit to give the conversion rate and its base.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMeasureWithUnit.htm }
  TIfcMeasureWithUnit = class(TIfcPersistent)
  strict private
    FValueComponent: TIfcValue;
    FUnitComponent: TIfcUnit;
  published
    { Value of the physical quantity when expressed in the specified units. }
    property ValueComponent: TIfcValue read FValueComponent write FValueComponent;

    { The unit in which the physical quantity is expressed. }
    property UnitComponent: TIfcUnit read FUnitComponent write FUnitComponent;
  end;

  { A unit that has a conversion rate to a base unit.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConversionBasedUnit.htm }
  TIfcConversionBasedUnit = class(TIfcNamedUnit)
  strict private
    FName: TIfcLabel;
    FConversionFactor: TIfcMeasureWithUnit;
    FHasExternalReference: TIfcExternalReferenceRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Word, or group of words, by which the conversion based unit is referred to. }
    property Name: TIfcLabel read FName write FName;

    { The physical quantity from which the converted unit is derived. }
    property ConversionFactor: TIfcMeasureWithUnit read FConversionFactor write FConversionFactor;

    { Reference to external information, e.g. library, classification,
      or document information, which is associated with the conversion-based unit. }
    property HasExternalReference: TIfcExternalReferenceRelationshipList read FHasExternalReference;
  end;

  { Unit which is converted from another unit by applying a conversion factor and an offset.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConversionBasedUnitWithOffset.htm }
  TIfcConversionBasedUnitWithOffset = class(TIfcConversionBasedUnit)
  strict private
    FConversionOffset: TIfcReal;
  published
    { A positive or negative offset to add after the inherited
      @link(ConversionFactor) was applied. }
    property ConversionOffset: TIfcReal read FConversionOffset write FConversionOffset;
  end;

  { Context to which the IfcRepresentation of a product is related. }
  TIfcRepresentationContext = class(TIfcPersistent)
  strict private
    FContextIdentifier: TIfcLabel;
    FContextType: TIfcLabel;
    FRepresentationsInContext: TIfcRepresentationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The optional identifier of the representation context as used within a project. }
    property ContextIdentifier: TIfcLabel read FContextIdentifier write FContextIdentifier;

    { The description of the type of a representation context.
      The supported values for context type are to be specified by implementers agreements. }
    property ContextType: TIfcLabel read FContextType write FContextType;

    { All shape representations that are defined in the same representation context. }
    property RepresentationsInContext: TIfcRepresentationList read FRepresentationsInContext;
  end;
  TIfcRepresentationContextList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentationContext>)
  end;

  { Context that applies to several shape representations of products within a project. }
  TIfcGeometricRepresentationContext = class(TIfcRepresentationContext)
  strict private
    FCoordinateSpaceDimension: TIfcDimensionCount;
    FPrecision: TIfcReal;
    FWorldCoordinateSystem: TIfcAxis2Placement;
    FTrueNorth: TIfcDirection;
    FHasSubContexts: TIfcGeometricRepresentationSubContextList;
    FHasCoordinateOperation: TIfcCoordinateOperation;
  protected
    function WantsGlobalId: Boolean; override;
  public
    const
      { The following values for @link(ContextType) are limited
        to these 3 constants for TIfcGeometricRepresentationContext. }
      TypeModel = 'Model';
      TypePlan = 'Plan';
      TypeNotDefined = 'NotDefined';
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The integer dimension count of the coordinate space modeled
      in a geometric representation context.}
    property CoordinateSpaceDimension: TIfcDimensionCount read FCoordinateSpaceDimension write FCoordinateSpaceDimension;

    { Value of the model precision for geometric models. }
    property Precision: TIfcReal read FPrecision write FPrecision;

    { Establishment of the engineering coordinate system
      (often referred to as the world coordinate system in CAD)
      for all representation contexts used by the project. }
    property WorldCoordinateSystem: TIfcAxis2Placement read FWorldCoordinateSystem write FWorldCoordinateSystem;

    { Direction of the true north, or geographic northing direction, relative to the underlying project coordinate system. }
    property TrueNorth: TIfcDirection read FTrueNorth write FTrueNorth;

    { The set of IfcGeometricRepresentationSubContexts that refer to this
      IfcGeometricRepresentationContext. }
    property HasSubContexts: TIfcGeometricRepresentationSubContextList read FHasSubContexts;

    { Indicates conversion between coordinate systems. }
    property HasCoordinateOperation: TIfcCoordinateOperation read FHasCoordinateOperation write FHasCoordinateOperation;
  end;

  TIfcGeometricProjectionEnum = (
    NotDefined,
    Elevation_View,
    Graph_View,
    Model_View,
    Plan_View,
    Reflected_Plan_View,
    Section_View,
    Sketch_View,
    UserDefined
  );

  { Context that applies to several shape representations of a product
    being a sub context, sharing the WorldCoordinateSystem,
    CoordinateSpaceDimension, Precision and TrueNorth attributes with
    the parent IfcGeometricRepresentationContext.  }
  TIfcGeometricRepresentationSubContext = class(TIfcRepresentationContext)
  strict private
    FParentContext: TIfcGeometricRepresentationContext;
    FTargetScale: TIfcPositiveRatioMeasure;
    FTargetView: TIfcGeometricProjectionEnum;
    FUserDefinedTargetView: TIfcLabel;
  published
    { Parent context from which the sub context derives its world coordinate system,
      precision, space coordinate dimension and true north. }
    property ParentContext: TIfcGeometricRepresentationContext read FParentContext write FParentContext;

    { The target plot scale of the representation to which
      this representation context applies.

      NOTE Scale indicates the target plot scale for the representation
      sub context, all annotation styles are given in plot dimensions
      according to this target plot scale.
      If multiple instances of IfcGeometricRepresentationSubContext
      are given having the same TargetView value, the target plot scale
      applies up to the next smaller scale, or up to unlimited small scale. }
    property TargetScale: TIfcPositiveRatioMeasure read FTargetScale write FTargetScale;

    { Target view of the representation to which this representation context applies. }
    property TargetView: TIfcGeometricProjectionEnum read FTargetView write FTargetView;

    { User defined target view, this attribute value shall be given,
      if the TargetView attribute is set to @link(TIfcGeometricProjectionEnum.UserDefined). }
    property UserDefinedTargetView: TIfcLabel read FUserDefinedTargetView write FUserDefinedTargetView;
  end;
  TIfcGeometricRepresentationSubContextList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcGeometricRepresentationSubContext>)
  end;

  { Relationships between property set definitions and objects.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelDefinesByProperties.htm }
  TIfcRelDefinesByProperties = class(TIfcRelDefines)
  strict private
    FRelatedObjects: TIfcObjectDefinitionList;
    FRelatingPropertyDefinition: TIfcPropertySetDefinitionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the objects (or single object) to which
      the property definition applies. }
    property RelatedObjects: TIfcObjectDefinitionList read FRelatedObjects;

    { Reference to the property set definition for that object
      or set of objects.

      Note: In IFC specification, this is IfcPropertySetDefinitionSelect,
      which is a choice between IfcPropertySetDefinition and
      IfcPropertySetDefinitionSet (equal to TIfcPropertySetDefinitionList,
      it is just list of IfcPropertySetDefinition). To make it simple in Pascal,
      we only allow the latter, TIfcPropertySetDefinitionList. }
    property RelatingPropertyDefinition: TIfcPropertySetDefinitionList read FRelatingPropertyDefinition write FRelatingPropertyDefinition;
  end;
  TIfcRelDefinesByPropertiesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDefinesByProperties>)
  end;

  { Generalization of all property and property set templates.
    Templates define the collection, types, names, applicable measure types
    and units of individual properties used in a project.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyTemplateDefinition.htm}
  TIfcPropertyTemplateDefinition = class(TIfcPropertyDefinition)
  end;

  TIfcPropertySetTemplateTypeEnum = (
    NotDefined,
    PSet_MaterialDriven,
    PSet_OccurrenceDriven,
    PSet_PerformanceDriven,
    PSet_ProfileDriven,
    PSet_TypeDrivenOnly,
    PSet_TypeDrivenOverride,
    Qto_OccurrenceDriven,
    Qto_TypeDrivenOnly,
    Qto_TypeDrivenOverride
  );

  { Abstract supertype comprising the templates for all dynamically extensible
    properties, either as an IfcComplexPropertyTemplate, or an
    IfcSimplePropertyTemplate. }
  TIfcPropertyTemplate = class abstract(TIfcPropertyTemplateDefinition)
  strict private
    // TODO FPartOfComplexTemplate: TIfcComplexPropertyTemplateList;
    FPartOfPsetTemplate: TIfcPropertySetTemplateList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to a complex property template. }
    // TODO property PartOfComplexTemplate: TIfcComplexPropertyTemplateList read FPartOfComplexTemplate;

    { Reference to the IfcPropertySetTemplate that defines the scope for
      the IfcPropertyTemplate. }
    property PartOfPsetTemplate: TIfcPropertySetTemplateList read FPartOfPsetTemplate;
  end;
  TIfcPropertyTemplateList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPropertyTemplate>)
  end;

  { Template for all dynamically extensible property sets represented by IfcPropertySet.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertySetTemplate.htm}
  TIfcPropertySetTemplate = class(TIfcPropertyTemplateDefinition)
  strict private
    FTemplateType: TIfcPropertySetTemplateTypeEnum;
    FApplicableEntity: TIfcIdentifier;
    FHasPropertyTemplates: TIfcPropertyTemplateList;
    FDefines: TIfcRelDefinesByTemplateList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Property set type defining whether the property set is applicable to
      a type (subtypes of IfcTypeObject), to an occurrence (subtypes of IfcObject),
      or as a special case to a performance history.

      The attribute ApplicableEntity may further refine the applicability
      to a single or multiple entity type(s). }
    property TemplateType: TIfcPropertySetTemplateTypeEnum read FTemplateType write FTemplateType
      default TIfcPropertySetTemplateTypeEnum.NotDefined;

    { The attribute optionally defines the data type of the applicable type
      or occurrence object. }
    property ApplicableEntity: TIfcIdentifier read FApplicableEntity write FApplicableEntity;

    { Set of IfcPropertyTemplate's that are defined within the scope of
      the IfcPropertySetTemplate. }
    property HasPropertyTemplates: TIfcPropertyTemplateList read FHasPropertyTemplates;

    { Relation to the property sets, via the objectified relationship
      IfcRelDefinesByTemplate, that, if given, utilize the definition template. }
    property Defines: TIfcRelDefinesByTemplateList read FDefines;
  end;
  TIfcPropertySetTemplateList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcPropertySetTemplate>)
  end;

  { Relationship between property set template and property sets..}
  TIfcRelDefinesByTemplate = class(TIfcRelDefines)
  strict private
    FRelatedPropertySets: TIfcPropertySetDefinitionList;
    FRelatingTemplate: TIfcPropertySetTemplate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { One or many property sets or quantity sets that obtain
      their definitions from the single property set template. }
    property RelatedPropertySets: TIfcPropertySetDefinitionList read FRelatedPropertySets;

    { Property set template that provides the common definition
      of related property sets. }
    property RelatingTemplate: TIfcPropertySetTemplate read FRelatingTemplate write FRelatingTemplate;
  end;
  TIfcRelDefinesByTemplateList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDefinesByTemplate>)
  end;

  { Relationship between an object type and object occurrences.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelDefinesByType.htm }
  TIfcRelDefinesByType = class(TIfcRelDefines)
  strict private
    FRelatedObjects: TIfcObjectList;
    FRelatingType: TIfcTypeObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RelatedObjects: TIfcObjectList read FRelatedObjects;

    { Reference to the type (or style) information for that object or set of objects. }
    property RelatingType: TIfcTypeObject read FRelatingType write FRelatingType;
  end;
  TIfcRelDefinesByTypeList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDefinesByType>)
  end;

  { Project context in which objects, type objects, property sets,
    and properties are defined.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcContext.htm }
  TIfcContext = class abstract(TIfcObjectDefinition)
  strict private
    FObjectType: TIfcLabel;
    FLongName: TIfcLabel;
    FPhase: TIfcLabel;
    FRepresentationContexts: TIfcRepresentationContextList;
    FUnitsInContext: TIfcUnitAssignment;
    FIsDefinedBy: TIfcRelDefinesByPropertiesList;
    FDeclares: TIfcRelDeclaresList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping): TTransformNode; override;
  published
    { The object type denotes a particular type that indicates the object
      further. The use has to be established at the level of instantiable
      subtypes. }
    property ObjectType: TIfcLabel read FObjectType write FObjectType;

    { Long name for the context as used for reference purposes. }
    property LongName: TIfcLabel read FLongName write FLongName;

    { Current project phase, or life-cycle phase of this project. }
    property Phase: TIfcLabel read FPhase write FPhase;

    { Context of the representations used within the context. }
    property RepresentationContexts: TIfcRepresentationContextList read FRepresentationContexts;

    { Units globally assigned to measure types used within the context. }
    property UnitsInContext: TIfcUnitAssignment read FUnitsInContext write FUnitsInContext;

    { Set of relationships to property set definitions attached to this context. }
    property IsDefinedBy: TIfcRelDefinesByPropertiesList read FIsDefinedBy;

    { Reference to the IfcRelDeclares relationship that assigns the uppermost
      entities of included hierarchies to this context instance. }
    property Declares: TIfcRelDeclaresList read FDeclares;
  end;

  { Collects all library elements that are included within a referenced project data set. }
  TIfcProjectLibrary = class(TIfcContext)
  end;

  { Context for information to be exchanged or shared,
    it may represent a construction project but does not have to.

    Note that this initializes property @link(Name) to 'My Project',
    as the project name must be non-empty (at least for conversion scripts
    on https://github.com/buildingsmart-community/ifcJSON and
    https://github.com/michaliskambi/ifcJSON ).
    This is the same initial project name as used by BonsaiBIM.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm }
  TIfcProject = class(TIfcContext)
  public
    constructor Create(AOwner: TComponent); override;

    { Setup units, following metric system and radians.
      This matches glTF, X3D and CGE conventions.

      Valid IFC project must have units defined.
      BonsaiBIM expects it.
      See also valid notes on https://stackoverflow.com/questions/51665572/required-data-for-ifc . }
    procedure SetupUnits;

    { Find a 3D TIfcGeometricRepresentationContext called "Model" in the project.
      This should be used to add all new 3D models to the project.

      Returns @nil if not found, which can happen only temporarily when the IFC
      file is constructed -- eventually, all valid IFC files should have
      a mandatory 3D context, see
      https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricRepresentationContext.htm .

      Call @link(SetupModelContext) to make sure this is non-nil. }
    function ModelContext: TIfcGeometricRepresentationContext;

    { Find a 2D TIfcGeometricRepresentationContext called "Plan" in the project.
      This should be used to add new 2D plans to the project.

      Returns @nil if not found. This is normal, 2D (plan) context is optional
      in IFC. }
    function PlanContext: TIfcGeometricRepresentationContext;

    { Add a 3D geometric representation context named "Model" to the project,
      which should be used for models displayed in 3D view.
      Does not add it if the project already has a suitable 3D context with this name.
      Makes @link(ModelContext) non-nil. }
    procedure SetupModelContext;

    { Find TIfcSpatialElement where it seems most suitable to insert new elements.
      This is just a guess, a proper IFC editor should allow user to choose it,
      as there may be multiple valid choices (e.g. TIfcSite, TIfcBuilding,
      TIfcBuldingStorey).

      Returns @nil if we cannot determine it. }
    function BestContainer: TIfcSpatialElement;
  end;

  { How to display the product. }
  TIfcRepresentation = class abstract(TIfcPersistent)
  strict private
    FContextOfItems: TIfcRepresentationContext;
    FRepresentationIdentifier: TIfcLabel;
    FRepresentationType: TIfcLabel;
    FItems: TIfcRepresentationItemList;
    FRepresentationMap: TIfcRepresentationMapList;
    FLayerAssignments: TIfcPresentationLayerAssignmentList;
    FOfProductRepresentation: TIfcProductRepresentationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Definition of the representation context for which
      the different subtypes of representation are valid. }
    property ContextOfItems: TIfcRepresentationContext read FContextOfItems write FContextOfItems;

    { The optional identifier of the representation as used within a project. }
    property RepresentationIdentifier: TIfcLabel read FRepresentationIdentifier write FRepresentationIdentifier;

    { The description of the type of a representation context. }
    property RepresentationType: TIfcLabel read FRepresentationType write FRepresentationType;

    { Set of geometric representation items that are defined for this representation. }
    property Items: TIfcRepresentationItemList read FItems;

    { Use of the representation within an IfcRepresentationMap. }
    property RepresentationMap: TIfcRepresentationMapList read FRepresentationMap;

    { Assignment of the whole representation to a single or multiple layer(s).  }
    property LayerAssignments: TIfcPresentationLayerAssignmentList read FLayerAssignments;

    { Reference to the product representations to which
      this individual representation applies. }
    property OfProductRepresentation: TIfcProductRepresentationList read FOfProductRepresentation;
  end;

  { List of TIfcRepresentation. }
  TIfcRepresentationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentation>)
  public
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
  end;

  TIfcShapeModel = class abstract(TIfcRepresentation)
    // TODO rest of IFC properties from spec
  end;
  TIfcShapeModelList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcShapeModel>)
  end;

  { Representation of a product or a product component within a specific
    geometric representation context.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShapeRepresentation.htm }
  TIfcShapeRepresentation = class(TIfcShapeModel)
  protected
    function WantsGlobalId: Boolean; override;
    // TODO rest of IFC properties from spec
  end;

  { Representation of a product, including its (geometric or topological)
    representation. A product can have zero, one or many geometric representations.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProductRepresentation.htm }
  TIfcProductRepresentation = class abstract(TIfcPersistent)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FRepresentations: TIfcRepresentationList;
  protected
    function WantsGlobalId: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const Mapping: TCastleIfcMapping;
      const ParentProduct: TIfcProduct;
      const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
  published
    { The word or group of words by which the product representation is known. }
    property Name: TIfcLabel read FName write FName;

    { The word or group of words that characterize the product representation. }
    property Description: TIfcText read FDescription write FDescription;

    { Contained list of representations (including shape representations). }
    property Representations: TIfcRepresentationList read FRepresentations;
  end;
  TIfcProductRepresentationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProductRepresentation>)
  end;

  { Grouping of shape representation items that represent aspects
    (or components) of the shape of a product.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShapeAspect.htm }
  TIfcShapeAspect = class(TIfcPersistent)
  strict private
    FShapeRepresentations: TIfcShapeModelList;
    FName: TIfcLabel;
    FDescription: TIfcText;
    FPProductDefinitional: Boolean;
    // FPartOfProductDefinitionShape: TIfcProductRepresentationSelect;
    FHasExternalReferences: TIfcExternalReferenceRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { List of shape representations. Each member defines a valid
      representation of a particular type within a particular
      representation context as being an aspect (or part) of a product
      definition. }
    property ShapeRepresentations: TIfcShapeModelList read FShapeRepresentations;

    { The word or group of words by which the shape aspect is known. }
    property Name: TIfcLabel read FName write FName;

    { The word or group of words that characterize the shape aspect. It can
      be used to add additional meaning to the name of the aspect. }
    property Description: TIfcText read FDescription write FDescription;

    property ProductDefinitional: Boolean read FPProductDefinitional write FPProductDefinitional;

    // TODO: property PartOfProductDefinitionShape: TIfcProductRepresentationSelect read FPartOfProductDefinitionShape write FPartOfProductDefinitionShape;

    property HasExternalReferences: TIfcExternalReferenceRelationshipList read FHasExternalReferences;
  end;
  TIfcShapeAspectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcShapeAspect>)
  end;

  { Defines a representation of a product, including its (geometric or topological) representation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProductRepresentation.htm }
  TIfcProductDefinitionShape = class(TIfcProductRepresentation)
  strict private
    FShapeOfProduct: TIfcProductList;
    FHasShapeAspects: TIfcShapeAspectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShapeOfProduct: TIfcProductList read FShapeOfProduct;

    { Reference to the shape aspect that represents part of
      the shape or its feature distinctively. }
    property HasShapeAspects: TIfcShapeAspectList read FHasShapeAspects;
  end;

  { Abstract supertype of all property related entities defined as
    dependent resource entities within the specification. }
  TIfcPropertyAbstraction = class abstract(TIfcPersistent)
  strict private
    FHasExternalReferences: TIfcExternalReferenceRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to an external reference, e.g. library, classification,
      or document information, that is associated to the property definition. }
    property HasExternalReferences: TIfcExternalReferenceRelationshipList read FHasExternalReferences;
  end;

  { Abstract supertype of all extensible property collections
    that are applicable to certain characterized entities. }
  TIfcExtendedProperties = class(TIfcPropertyAbstraction)
  strict private
    FName: TIfcIdentifier;
    FDescription: TIfcText;
    FProperties: TIfcPropertyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The name given to the set of properties. }
    property Name: TIfcIdentifier read FName write FName;

    { Description for the set of properties. }
    property Description: TIfcText read FDescription write FDescription;

    { The set of properties provided for this extended property collection. }
    property Properties: TIfcPropertyList read FProperties;
  end;

  TIfcProfileProperties = class(TIfcExtendedProperties)
  strict private
    FProfileDefinition: TIfcProfileDef;
  published
    property ProfileDefinition: TIfcProfileDef read FProfileDefinition write FProfileDefinition;
  end;
  TIfcProfilePropertiesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProfileProperties>)
  end;

  { Abstract generalization for all types of properties that can be
    associated with IFC objects through the property set mechanism. }
  TIfcProperty = class abstract(TIfcPersistent)
  strict private
    FName: TIfcIdentifier;
    FSpecification: TIfcText;
    FPartOfPset: TIfcPropertySetList;
    FPropertyForDependance: TIfcPropertyDependencyRelationshipList;
    FPropertyDependsOn: TIfcPropertyDependencyRelationshipList;
    FPartOfComplex: TIfcComplexPropertyList;
    // TODO FHasConstraints: TIfcResourceConstraintRelationshipList;
    // TODO FHasApprovals: TIfcResourceApprovalRelationshipList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Name for this property. This label is the significant name string
      that defines the semantic meaning for the property. }
    property Name: TIfcIdentifier read FName write FName;

    { URI reference to a location with semantic definition or informative
      text to explain the property. }
    property Specification: TIfcText read FSpecification write FSpecification;

    { Deprecated name for @link(Specification).
      IFC 4.3 changed the name to @link(Specification). }
    property Description: TIfcText read FSpecification write FSpecification stored false;

    { Reference to the IfcPropertySet by which the IfcProperty is referenced. }
    property PartOfPset: TIfcPropertySetList read FPartOfPset write FPartOfPset;

    { Property on whose value that of another property depends. }
    property PropertyForDependance: TIfcPropertyDependencyRelationshipList read FPropertyForDependance;

    { Relating property on which the value of the property depends. }
    property PropertyDependsOn: TIfcPropertyDependencyRelationshipList read FPropertyDependsOn;

    { Reference to the IfcComplexProperty in which the IfcProperty is contained. }
    property PartOfComplex: TIfcComplexPropertyList read FPartOfComplex;

    { User-defined constraints for the property. }
	  // TODO property HasConstraints: TIfcResourceConstraintRelationshipList read FHasConstraints;

    { User-defined approvals for the property. }
    // TODO property HasApprovals: TIfcResourceApprovalRelationshipList read FHasApprovals;
  end;
  TIfcPropertyList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProperty>)
  end;

  { Generalization of a single property object. }
  TIfcSimpleProperty = class abstract(TIfcProperty)
  end;

  { Property with a single (numeric or descriptive) value assigned. }
  TIfcPropertySingleValue = class(TIfcSimpleProperty)
  strict private
    FNominalValue: String;
    FUnit: TIfcUnit;
  published
    { Value and measure type of this property.

      TODO: This is IfcValue in IFC standard,
      https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcValue.htm .
      It's just String in Pascal, our JSON reader will convert other simple types
      to strings. }
    property NominalValue: String read FNominalValue write FNominalValue;

    { Unit for the nominal value, if not given. }
    property &Unit: TIfcUnit read FUnit write FUnit;
  end;

  { Define complex properties to be handled completely within a property set.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcComplexProperty.htm }
  TIfcComplexProperty = class(TIfcProperty)
  strict private
    FUsageName: TIfcIdentifier;
    FHasProperties: TIfcPropertyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Usage description of the IfcComplexProperty within the property
      set which references the IfcComplexProperty. }
    property UsageName: TIfcIdentifier read FUsageName write FUsageName;

    { Set of properties that can be used within this complex property
      (may include other complex properties). }
    property HasProperties: TIfcPropertyList read FHasProperties;
  end;
  TIfcComplexPropertyList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcComplexProperty>)
  end;

  { Set of material properties to associated material definitions.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProperties.htm }
  TIfcMaterialProperties = class(TIfcExtendedProperties)
  strict private
    FMaterial: TIfcMaterialDefinition;
  published
    { Reference to the material definition to which the set of properties is assigned. }
    property Material: TIfcMaterialDefinition read FMaterial write FMaterial;
  end;
  TIfcMaterialPropertiesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialProperties>)
  end;

  { General supertype for all material related information items in IFC
    that have common material related properties that may include association
    of material with some shape parameters or assignments to identified parts
    of a component.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialDefinition.htm }
  TIfcMaterialDefinition = class abstract(TIfcMaterialSelect)
  strict private
    FAssociatedTo: TIfcRelAssociatesMaterialList;
    FHasExternalReferences: TIfcExternalReferenceRelationshipList;
    FHasProperties: TIfcMaterialPropertiesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Use of the IfcMaterialDefinition subtypes within the material association
      of an element occurrence or element type. }
 	  property AssociatedTo: TIfcRelAssociatesMaterialList read FAssociatedTo;

    { Reference to external references, e.g. library, classification,
      or document information, that are associated to the material. }
    property HasExternalReferences: TIfcExternalReferenceRelationshipList read FHasExternalReferences;

    { Material properties assigned to instances of subtypes of IfcMaterialDefinition. }
    property HasProperties: TIfcMaterialPropertiesList read FHasProperties;
  end;

  { Single and identifiable cross section of an element which is constructed
    of a number of profiles (one or more).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProfile.htm }
  TIfcMaterialProfile = class(TIfcMaterialDefinition)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FMaterial: TIfcMaterial;
    FProfile: TIfcProfileDef;
    FPriority: Integer;
    FCategory: TIfcLabel;
    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    FToMaterialProfileSet: TIfcMaterialProfileSet;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  published
    { The name by which the material profile is known. }
    property Name: TIfcLabel read FName write FName;

    { Definition of the material profile in descriptive terms. }
    property Description: TIfcText read FDescription write FDescription;

    { Optional reference to the material from which the profile is constructed. }
    property Material: TIfcMaterial read FMaterial write FMaterial;

    { Identification of the profile for which this material profile is associating material. }
    property Profile: TIfcProfileDef read FProfile write FProfile;

    { The relative priority of the profile, expressed as normalised integer range [0..100]. Controls how profiles intersect in connections and corners of building elements: A profile from one element protrudes into (i.e. displaces) a profile from another element in a joint of these elements if the former element's profile has higher priority than the latter. The priority value for a material profile in an element has to be set and maintained by software applications in relation to the material profiles in connected elements. }
    property Priority: Integer read FPriority write FPriority;

    { Category of the material profile, e.g. the role it has in the profile set it belongs to. The list of keywords might be extended by model view definitions, however the following keywords shall apply in general: * 'LoadBearing' — the material profile having a load bearing function. * 'Insulation' — the material profile having an insolating function. * 'Finish' — the material profile being the finish. }
    property Category: TIfcLabel read FCategory write FCategory;

    {$ifdef CASTLE_IFC_INVERSE_PROPS}
    { Material profile set in which this material profile is included. }
    property ToMaterialProfileSet: TIfcMaterialProfileSet read FToMaterialProfileSet write FToMaterialProfileSet;
    {$endif CASTLE_IFC_INVERSE_PROPS}
  end;
  TIfcMaterialProfileList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialProfile>;

  { Designation by which individual material(s) of a prismatic element
    (for example, beam or column) constructed of a single or multiple
    material profiles is known.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProfileSet.htm }
  TIfcMaterialProfileSet = class(TIfcMaterialDefinition)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FMaterialProfiles: TIfcMaterialProfileList;
    FCompositeProfile: TIfcCompositeProfileDef;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The name by which the material profile set is known. }
 	  property Name: TIfcLabel read FName write FName;

    { Definition of the material profile set in descriptive terms. }
    property Description: TIfcText read FDescription write FDescription;

    { Identification of the profiles from which the material profile set is composed. }
    property MaterialProfiles: TIfcMaterialProfileList read FMaterialProfiles;

    { Reference to the composite profile definition for which this
      material profile set associates material to each of its individual profiles.
      If only a single material profile is used (the most typical case)
      then no CompositeProfile is asserted.  }
    property CompositeProfile: TIfcCompositeProfileDef read FCompositeProfile write FCompositeProfile;
  end;

  { Relationship between part and whole in material definitions (as in composite materials).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialRelationship.htm }
  TIfcMaterialRelationship = class(TIfcResourceLevelRelationship)
  strict private
    FRelatingMaterial: TIfcMaterial;
    FRelatedMaterials: TIfcMaterialList;
    FMaterialExpression: TIfcLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the relating material (the composite). }
    property RelatingMaterial: TIfcMaterial read FRelatingMaterial write FRelatingMaterial;

    { Reference to related materials (as constituents of composite material). }
    property RelatedMaterials: TIfcMaterialList read FRelatedMaterials;

    property MaterialExpression: TIfcLabel read FMaterialExpression write FMaterialExpression;
  end;
  TIfcMaterialRelationshipList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialRelationship>;

  { Defines presentation information relating to IfcMaterial.
    It allows for multiple presentations of the same material
    for different geometric representation contexts.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialDefinitionRepresentation.htm }
  TIfcMaterialDefinitionRepresentation = class(TIfcProductRepresentation)
  strict private
    FRepresentedMaterial: TIfcMaterial;
  published
    { Reference to the material to which the representation applies. }
    property RepresentedMaterial: TIfcMaterial read FRepresentedMaterial write FRepresentedMaterial;
  end;
  TIfcMaterialDefinitionRepresentationList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialDefinitionRepresentation>;

  { Homogeneous or inhomogeneous substance that can be used
    to form elements (physical products or their components). }
  TIfcMaterial = class(TIfcMaterialDefinition)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FCategory: TIfcLabel;
    FHasRepresentation: TIfcMaterialDefinitionRepresentation;
    FIsRelatedWith: TIfcMaterialRelationshipList;
    FRelatesTo: TIfcMaterialRelationship;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Name of the material. }
    property Name: TIfcLabel read FName write FName;

    { Definition of the material in more descriptive terms than given by attributes Name or Category. }
    property Description: TIfcText read FDescription write FDescription;

    { Definition of the category (group or type) of material, in more general terms than given by attribute Name. }
    property Category: TIfcLabel read FCategory write FCategory;

    { Reference to the IfcMaterialDefinitionRepresentation that
      provides presentation information to a representation common
      to this material in style definitions. }
    property HasRepresentation: TIfcMaterialDefinitionRepresentation read FHasRepresentation write FHasRepresentation;

    { Reference to a material relationship indicating that this material
      is a part (or constituent) in a material composite. }
    property IsRelatedWith: TIfcMaterialRelationshipList read FIsRelatedWith;

    { Reference to a material relationship indicating that this material
      composite has parts (or constituents). }
    property RelatesTo: TIfcMaterialRelationship read FRelatesTo write FRelatesTo;
  end;
  TIfcMaterialList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterial>)
  end;

  { Single and identifiable part of an element which is constructed of a
    number of part (one or more) each having an individual material.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialConstituent.htm }
  TIfcMaterialConstituent = class(TIfcMaterialDefinition)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FMaterial: TIfcMaterial;
    FFraction: TIfcNormalisedRatioMeasure;
    FCategory: TIfcLabel;
    FToMaterialConstituentSet: TIfcMaterialConstituentSet;
  published
    { The name by which the material constituent is known. }
    property Name: TIfcLabel read FName write FName;

    { Definition of the material constituent in descriptive terms. }
    property Description: TIfcText read FDescription write FDescription;

    { Reference to the material from which the constituent is constructed. }
    property Material: TIfcMaterial read FMaterial write FMaterial;

    { Optional provision of a fraction of the total amount (volume or weight)
      that applies to the IfcMaterialConstituentSet
      that is contributed by this IfcMaterialConstituent. }
    property Fraction: TIfcNormalisedRatioMeasure read FFraction write FFraction;

    { Category of the material constituent, e.g. the role it has in the constituent set it belongs to. }
    property Category: TIfcLabel read FCategory write FCategory;

    { Material constituent set in which this material constituent is included. }
    property ToMaterialConstituentSet: TIfcMaterialConstituentSet read FToMaterialConstituentSet write FToMaterialConstituentSet;
  end;
  TIfcMaterialConstituentList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialConstituent>;

  { Collection of individual material constituents, each assigning a material
    to a part of an element. The parts are only identified by a keyword
    (as opposed to an IfcMaterialLayerSet or IfcMaterialProfileSet where
    each part has an individual shape parameter (layer thickness or layer profile). }
  TIfcMaterialConstituentSet = class(TIfcMaterialDefinition)
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FMaterialConstituents: TIfcMaterialConstituentList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Name by which the constituent set is known. }
    property Name: TIfcLabel read FName write FName;

    { Definition of the material constituent in descriptive terms. }
    property Description: TIfcText read FDescription write FDescription;

    { Identification of the constituents from which the material constituent set is composed. }
    property MaterialConstituents: TIfcMaterialConstituentList read FMaterialConstituents;
  end;

  { Single and identifiable part of an element which is constructed
    of a number of layers (one or more).

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialLayer.htm }
  TIfcMaterialLayer = class(TIfcMaterialDefinition)
  strict private
    FMaterial: TIfcMaterial;
    FLayerThickness: TIfcNonNegativeLengthMeasure;
    FIsVentilated: TIfcLogical;
    FName: TIfcLabel;
    FDescription: TIfcText;
    FCategory: TIfcLabel;
    FPriority: Integer;
    FToMaterialLayerSet: TIfcMaterialLayerSet;
  published
    { Optional reference to the material from which the layer is constructed. }
    property Material: TIfcMaterial read FMaterial write FMaterial;

    { The thickness of the material layer. }
    property LayerThickness: TIfcNonNegativeLengthMeasure read FLayerThickness write FLayerThickness;

    { Indication of whether the material layer represents an air layer (or cavity).

      TODO: IFC specification says this is OPTIONAL, and default is "unknown",
      so maybe this should be an enum with 3 values in Pascal? }
    property IsVentilated: TIfcLogical read FIsVentilated write FIsVentilated default false;

    { The name by which the material layer is known. }
    property Name: TIfcLabel read FName write FName;

    { Definition of the material layer in more descriptive terms than given by attributes Name or Category. }
    property Description: TIfcText read FDescription write FDescription;

    { Category of the material layer, e.g. the role it has in the layer set it belongs to. }
    property Category: TIfcLabel read FCategory write FCategory;

    { The relative priority of the layer, expressed as normalised integer range [0..100]. }
    property Priority: Integer read FPriority write FPriority default 0;

    { Reference to the IfcMaterialLayerSet in which the material layer is included. }
    property ToMaterialLayerSet: TIfcMaterialLayerSet read FToMaterialLayerSet write FToMaterialLayerSet;
  end;
  TIfcMaterialLayerList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcMaterialLayer>;

  { Designation by which materials of an element constructed of a number
    of material layers is known and through which the relative positioning
    of individual layers can be expressed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialLayerSet.htm }
  TIfcMaterialLayerSet = class(TIfcMaterialDefinition)
  strict private
    FMaterialLayers: TIfcMaterialLayerList;
    FLayerSetName: TIfcLabel;
    FDescription: TIfcText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Identification of the IfcMaterialLayer's from which the IfcMaterialLayerSet is composed. }
    property MaterialLayers: TIfcMaterialLayerList read FMaterialLayers;

    { The name by which the IfcMaterialLayerSet is known. }
    property LayerSetName: TIfcLabel read FLayerSetName write FLayerSetName;

    { Definition of the IfcMaterialLayerSet in descriptive terms. }
    property Description: TIfcText read FDescription write FDescription;
  end;

  TIfcLayerSetDirectionEnum = (
    Axis1,
    Axis2,
    Axis3
  );

  TIfcDirectionSenseEnum = (
    Positive,
    Negative
  );

  { Determines the usage of IfcMaterialLayerSet in terms of its location
    and orientation relative to the associated element geometry. }
  TIfcMaterialLayerSetUsage = class(TIfcMaterialDefinition)
  strict private
    FForLayerSet: TIfcMaterialLayerSet;
    FLayerSetDirection: TIfcLayerSetDirectionEnum;
    FDirectionSense: TIfcDirectionSenseEnum;
    FOffsetFromReferenceLine: TIfcLengthMeasure;
    FReferenceExtent: TIfcPositiveLengthMeasure;
  published
    { The IfcMaterialLayerSet set to which the usage is applied. }
    property ForLayerSet: TIfcMaterialLayerSet read FForLayerSet write FForLayerSet;

    { Orientation of the material layer set relative to element reference geometry. }
    property LayerSetDirection: TIfcLayerSetDirectionEnum read FLayerSetDirection write FLayerSetDirection;

    { Denotes whether the material layer set is oriented in positive or negative sense
      along the specified axis (defined by LayerSetDirection). }
    property DirectionSense: TIfcDirectionSenseEnum read FDirectionSense write FDirectionSense;

    { Offset of the material layer set base line from reference geometry of element. }
    property OffsetFromReferenceLine: TIfcLengthMeasure read FOffsetFromReferenceLine write FOffsetFromReferenceLine;

    { Extent of the extrusion of the elements body shape representation to which
      the IfcMaterialLayerSetUsage applies. }
    property ReferenceExtent: TIfcPositiveLengthMeasure read FReferenceExtent write FReferenceExtent;
  end;

{$endif read_interface_types}

{$ifdef read_interface}

{ List of all IFC classes that are defined in this unit.

  This includes all IFC non-abstract classes
  and also some abstract classes that are referred to by lists
  (because, to check and resolve references in IFC files,
  we need to know about them).

  To register new, custom IFC classes, just add new items to this list,
  like @code(IfcClasses.Add(TIfcMyNewClass)). }
function IfcClasses: TIfcPersistentClassList;

{$endif read_interface}

{$ifdef read_implementation}

const
  { Default rotation in X3D TTransformNode.Rotation.
    For CGE, axis doesn't matter when angle = 0 and we could even use
    TVector4.Zero (zero axis is allowed in this special case)
    but we use DefaultX3DRotation to not cause "noise" in output X3D file.

    See also TCastleTransform.InternalBuildNode and RotationHasZDefault
    comments. }
  DefaultX3DRotation: TVector4 = (X: 0; Y: 0; Z: 1; W: 0);

{ TIfcRoot --------------------------------------------------------------- }

function TIfcRoot.WantsGlobalId: Boolean;
begin
  Result := true;
end;

{ TIfcStyledItem ------------------------------------------------------------- }

constructor TIfcStyledItem.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := TIfcPresentationStyleList.Create(false);
end;

destructor TIfcStyledItem.Destroy;
begin
  FreeAndNil(FStyles);
  inherited;
end;

{ TIfcPresentationLayerAssignment ------------------------------------------- }

constructor TIfcPresentationLayerAssignment.Create(AOwner: TComponent);
begin
  inherited;
  FAssignedItems := TIfcLayeredItemList.Create(false);
end;

destructor TIfcPresentationLayerAssignment.Destroy;
begin
  FreeAndNil(FAssignedItems);
  inherited;
end;

{ TIfcObjectDefinition ------------------------------------------------------- }

constructor TIfcObjectDefinition.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FHasAssignments := TIfcRelAssignsList.Create(false);
  FIsDecomposedBy := TIfcRelAggregatesList.Create(false);
  FDecomposes := TIfcRelAggregatesList.Create(false);
  FHasAssociations := TIfcRelAssociatesList.Create(false);
end;

destructor TIfcObjectDefinition.Destroy;
begin
  // TODO FreeAndNil(FHasAssignments);
  FreeAndNil(FIsDecomposedBy);
  FreeAndNil(FDecomposes);
  FreeAndNil(FHasAssociations);
  inherited;
end;

procedure TIfcObjectDefinition.AddIsDecomposedBy(const ChildObject: TIfcObjectDefinition);
var
  RelAggregates: TIfcRelAggregates;
begin
  RelAggregates := TIfcRelAggregates.Create(Owner);
  RelAggregates.RelatingObject := Self;
  RelAggregates.RelatedObjects.Add(ChildObject);

  IsDecomposedBy.Add(RelAggregates);

  ChildObject.Decomposes.Add(RelAggregates);
end;

{ TIfcLocalPlacement --------------------------------------------------------- }

function TIfcLocalPlacement.WantsGlobalId: Boolean;
begin
  Result := true;
end;

{ TIfcProduct --------------------------------------------------------------- }

constructor TIfcProduct.Create(AOwner: TComponent);
begin
  inherited;
  FPositionedRelativeTo := TIfcRelPositionsList.Create(false);
end;

destructor TIfcProduct.Destroy;
begin
  FreeAndNil(FPositionedRelativeTo);
  inherited;
end;

procedure TIfcProduct.AddMeshRepresentation(
  const Context: TIfcGeometricRepresentationContext;
  const Coordinates: array of TVector3;
  const Indexes: array of Integer;
  const Solid: Boolean);
var
  PolygonalFaceSet: TIfcPolygonalFaceSet;
  NewRepresentation: TIfcRepresentation;
  IfcCoordinates: TIfcCartesianPointList3D;
  FaceBegin, FaceEnd, I: Integer;
  Face: TIfcIndexedPolygonalFace;
begin
  Assert(Context <> nil);

  IfcCoordinates := TIfcCartesianPointList3D.Create(Owner);
  IfcCoordinates.CoordList.AddRange(Coordinates);

  PolygonalFaceSet := TIfcPolygonalFaceSet.Create(Owner);
  PolygonalFaceSet.Coordinates := IfcCoordinates;
  PolygonalFaceSet.Closed := Solid;

  // split Indexes into faces
  FaceBegin := 0;
  while FaceBegin < Length(Indexes) do
  begin
    // move FaceBegin to point to next index >= 0
    while (FaceBegin < Length(Indexes)) and
          (Indexes[FaceBegin] < 0) do
      Inc(FaceBegin);
    if FaceBegin >= Length(Indexes) then Break;
    Assert(Indexes[FaceBegin] >= 0);

    // move FaceEnd to point to next < 0 (or beyond the end)
    FaceEnd := FaceBegin;
    while (FaceEnd < Length(Indexes)) and
          (Indexes[FaceEnd] >= 0) do
      Inc(FaceEnd);

    { FaceEnd always moves at least 1 step, because it steps over
      at least one index >= 0.
      This is important, it also means that our loop (doing "FaceBegin := FaceEnd"
      at the end) will always move forward, never hang. }
    Assert(FaceEnd > FaceBegin);

    if FaceEnd - FaceBegin < 3 then
    begin
      WritelnWarning('IFC', 'Degenerate face with less than 3 vertices');
    end else
    begin
      Face := TIfcIndexedPolygonalFace.Create(Owner);
      Face.ToFaceSet.Add(PolygonalFaceSet);
      for I := FaceBegin to FaceEnd - 1 do
        Face.CoordIndex.Add(Indexes[I] + 1); // indexes are 1-based in IFC
      PolygonalFaceSet.Faces.Add(Face);
    end;

    FaceBegin := FaceEnd;
  end;

  NewRepresentation := TIfcShapeRepresentation.Create(Owner);
  NewRepresentation.RepresentationIdentifier := 'Body';
  { Seems like RepresentationType=Tesselation best matches mesh data,
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShapeRepresentation.htm . }
  NewRepresentation.RepresentationType := 'Tessellation';
  NewRepresentation.ContextOfItems := Context;
  NewRepresentation.Items.Add(PolygonalFaceSet);

  if Representation = nil then
    Representation := TIfcProductDefinitionShape.Create(Owner);
  Representation.Representations.Add(NewRepresentation);

  Context.RepresentationsInContext.Add(NewRepresentation);

  // link back from NewRepresentation -> (product) Representation
  NewRepresentation.OfProductRepresentation.Add(Representation);
end;

procedure TIfcProduct.AddBoxRepresentation(
  const Context: TIfcGeometricRepresentationContext;
  const Box: TBox3D);
var
  Coordinates: array [0..7] of TVector3;
begin
  { Coordinate index in octal notation indicates which of 8 corners it is. }
  Coordinates[0] := Box.Data[0];
  Coordinates[1] := Box.Data[0]; Coordinates[1].Data[0] := Box.Data[1][0];
  Coordinates[2] := Box.Data[0]; Coordinates[2].Data[1] := Box.Data[1][1];
  Coordinates[4] := Box.Data[0]; Coordinates[4].Data[2] := Box.Data[1][2];

  Coordinates[3] := Box.Data[1]; Coordinates[3].Data[2] := Box.Data[0][2];
  Coordinates[5] := Box.Data[1]; Coordinates[5].Data[1] := Box.Data[0][1];
  Coordinates[6] := Box.Data[1]; Coordinates[6].Data[0] := Box.Data[0][0];
  Coordinates[7] := Box.Data[1];

  AddMeshRepresentation(Context, Coordinates, [
    // Quads that form the box
    { TODO: This is inverse order than TOcclusionCullingUtilsRenderer.GLContextOpen,
      and it seems necessary to be CCW outside.
      - Double-check below is good for CCW outside.
      - Possibly adjust TOcclusionCullingUtilsRenderer.GLContextOpen
        or document why it doesn't matter there.
    }
    2, 3, 1, 0, -1,
    3, 7, 5, 1, -1,
    7, 6, 4, 5, -1,
    6, 2, 0, 4, -1,
    6, 7, 3, 2, -1,
    1, 5, 4, 0, -1
  ]);
end;

function TIfcProduct.GetTransform(
  out Translation: TVector3; out Rotation: TVector4): Boolean;
var
  UseRelativePlacement: TIfcAxis2Placement;
  UseObjectPlacement: TIfcLocalPlacement;
begin
  Result := false;
  Translation := TVector3.Zero;
  Rotation := DefaultX3DRotation;

  // check ObjectPlacement
  if ObjectPlacement = nil then
  begin
    { Nothing more to calculate, but it's all OK --
      ObjectPlacement=nil means that we're relative to parent.
      TIfcProduct.SetTransform can deal with it, it will create new
      ObjectPlacement. }
    Result := true;
  end else
  if ObjectPlacement is TIfcLocalPlacement then
  begin
    { We can continue, because we have TIfcLocalPlacement.
      But is this TIfcLocalPlacement really relative to parent?
      Check it and set Result.
      But still, continue, even when Result=false, to allow
      implementations to work even when Result=false. }
    UseObjectPlacement := TIfcLocalPlacement(ObjectPlacement);
    Result :=
      (
        { Check PlacesObject that should include Self, or be empty.
          https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectPlacement.htm
          allows placesObject to be empty, from what I understand, BonsaiBIM
          also can generate such placement. }
        (UseObjectPlacement.PlacesObject.IndexOf(Self) <> -1) or
        (UseObjectPlacement.PlacesObject.Count = 0)
      );

    { If UseObjectPlacement.RelativePlacement = nil, no problem,
      TIfcProduct.SetTransform, it will create new
      UseObjectPlacement.RelativePlacement . }
    UseRelativePlacement := UseObjectPlacement.RelativePlacement;
    if UseRelativePlacement <> nil then
    begin
      UseRelativePlacement.GetTransform(Translation, Rotation);

      { While GetTransform is defined for any valid UseRelativePlacement
        class, but our SetTranslation can handle only TIfcAxis2Placement3D
        (since 2D version cannot express all 3D positions/rotations,
        naturally). }
      Result := Result and (UseRelativePlacement is TIfcAxis2Placement3D);
    end;
  end else
  begin
    { We cannot continue, because ObjectPlacement is something else
      than TIfcLocalPlacement.
      We cannot calculate relative transform for it. }
  end;
end;

procedure TIfcProduct.SetTransformRelativeTo(const OtherProduct: TIfcProduct);
begin
  if (OtherProduct = nil) or (OtherProduct = Self) then
  begin
    { In this case, this TIfcProduct is not relative to anythign non-standard.
      So just set ObjectPlacement.PlacementRelTo to nil.
      Or leave ObjectPlacement as nil. }
    if ObjectPlacement is TIfcLocalPlacement then
      TIfcLocalPlacement(ObjectPlacement).PlacementRelTo := nil;
  end else
  begin
    // make sure OtherProduct.ObjectPlacement <> nil
    if OtherProduct.ObjectPlacement = nil then
      OtherProduct.ObjectPlacement := TIfcLocalPlacement.Create(Owner);
    // make sure ObjectPlacement <> nil
    if ObjectPlacement = nil then
      ObjectPlacement := TIfcLocalPlacement.Create(Owner);
    ObjectPlacement.PlacementRelTo := OtherProduct.ObjectPlacement;

    { Inverse of PlacementRelTo, according to spec of IfcObjectPlacement.
      Seems unused by BonsaiBIM and FreeCAD, but spec says it's there. }
    OtherProduct.ObjectPlacement.ReferencedByPlacements.Add(ObjectPlacement);
  end;
end;

function TIfcProduct.TransformSupported: Boolean;
var
  DummyTranslation: TVector3;
  DummyRotation: TVector4;
begin
  Result := GetTransform(DummyTranslation, DummyRotation);
end;

function TIfcProduct.GetTranslation: TVector3;
var
  DummyRotation: TVector4;
begin
  GetTransform(Result, DummyRotation);
end;

procedure TIfcProduct.SetTranslation(const Value: TVector3);
var
  NewLocation: TIfcCartesianPoint;
  NewRelativePlacement: TIfcAxis2Placement3D;
  NewObjectPlacement: TIfcLocalPlacement;
begin
  // create NewObjectPlacement and assign it to ObjectPlacement
  if (ObjectPlacement <> nil) and
     (ObjectPlacement is TIfcLocalPlacement)
     (*
     { Do not check it below.
       If developer wants to set TIfcProduct.SetTranslation even
       when we're relative to something else, let them. }
     and
     (TIfcLocalPlacement(ObjectPlacement).PlacementRelTo = nil) and
     (
       { Check PlacesObject that should include Self, or be empty.
        https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjectPlacement.htm
        allows placesObject to be empty, from what I understand, BonsaiBIM
        also can generate such placement. }
       (TIfcLocalPlacement(ObjectPlacement).PlacesObject.IndexOf(Self) <> -1) or
       (TIfcLocalPlacement(ObjectPlacement).PlacesObject.Count = 0)
     )
     *)
     then
  begin
    NewObjectPlacement := TIfcLocalPlacement(ObjectPlacement);
  end else
  begin
    NewObjectPlacement := TIfcLocalPlacement.Create(Owner);
    NewObjectPlacement.PlacesObject.Add(Self);
    ObjectPlacement := NewObjectPlacement;
  end;

  // create NewRelativePlacement and assign it to NewObjectPlacement.RelativePlacement
  if (NewObjectPlacement.RelativePlacement <> nil) and
     (NewObjectPlacement.RelativePlacement is TIfcAxis2Placement3D) then
  begin
    NewRelativePlacement := TIfcAxis2Placement3D(NewObjectPlacement.RelativePlacement);
  end else
  begin
    NewRelativePlacement := TIfcAxis2Placement3D.Create(Owner);
    NewObjectPlacement.RelativePlacement := NewRelativePlacement;
  end;

  // create NewLocation and assign it to NewRelativePlacement.Location
  if (NewRelativePlacement.Location <> nil) and
     (NewRelativePlacement.Location is TIfcCartesianPoint) then
  begin
    NewLocation := TIfcCartesianPoint(NewRelativePlacement.Location);
  end else
  begin
    NewLocation := TIfcCartesianPoint.Create(Owner);
    NewRelativePlacement.Location := NewLocation;
  end;

  NewLocation.Coordinates.Value := Value;
end;

{ TIfcPositioningElement ----------------------------------------------------- }

constructor TIfcPositioningElement.Create(AOwner: TComponent);
begin
  inherited;
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FPositions := TIfcRelPositionsList.Create(false);
  {$endif}
end;

destructor TIfcPositioningElement.Destroy;
begin
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FreeAndNil(FPositions);
  {$endif}
  inherited;
end;

{ TIfcVirtualGridIntersection ------------------------------------------------ }

constructor TIfcVirtualGridIntersection.Create(AOwner: TComponent);
begin
  inherited;
  FIntersectingAxes := TIfcGridAxisList.Create(false);
end;

destructor TIfcVirtualGridIntersection.Destroy;
begin
  FreeAndNil(FIntersectingAxes);
  inherited;
end;

{ TIfcGridAxis -------------------------------------------------------------- }

constructor TIfcGridAxis.Create(AOwner: TComponent);
begin
  inherited;
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FHasIntersections := TIfcVirtualGridIntersectionList.Create(false);
  {$endif}
end;

destructor TIfcGridAxis.Destroy;
begin
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FreeAndNil(FHasIntersections);
  {$endif}
  inherited;
end;

{ TIfcGrid ------------------------------------------------------------------- }

constructor TIfcGrid.Create(AOwner: TComponent);
begin
  inherited;
  FUAxes := TIfcGridAxisList.Create(false);
  FVAxes := TIfcGridAxisList.Create(false);
  FWAxes := TIfcGridAxisList.Create(false);
end;

destructor TIfcGrid.Destroy;
begin
  FreeAndNil(FUAxes);
  FreeAndNil(FVAxes);
  FreeAndNil(FWAxes);
  inherited;
end;

{ TIfcSpatialElement --------------------------------------------------------- }

constructor TIfcSpatialElement.Create(AOwner: TComponent);
begin
  inherited;
  FContainsElements := TIfcRelContainedInSpatialStructureList.Create(false);
end;

destructor TIfcSpatialElement.Destroy;
begin
  FreeAndNil(FContainsElements);
  inherited;
end;

procedure TIfcSpatialElement.AddContainedElement(const Element: TIfcElement);
var
  Relation: TIfcRelContainedInSpatialStructure;
begin
  if (ContainsElements.Count = 1) and
     (ContainsElements[0] is TIfcRelContainedInSpatialStructure) and
     ((ContainsElements[0] as TIfcRelContainedInSpatialStructure).RelatingStructure = Self) then
  begin
    { Reuse existing TIfcRelContainedInSpatialStructure instance }
    Relation := ContainsElements[0] as TIfcRelContainedInSpatialStructure;
  end else
  begin
    Relation := TIfcRelContainedInSpatialStructure.Create(Owner);
    Relation.RelatingStructure := Self;
    ContainsElements.Add(Relation);
  end;

  Relation.RelatedElements.Add(Element);

  Element.ContainedInStructure.Add(Relation);
end;

procedure TIfcSpatialElement.GetContainedElements(const List: TIfcElementList);
var
  RelContainedInSpatialStructure: TIfcRelContainedInSpatialStructure;
  Product: TIfcProduct;
begin
  for RelContainedInSpatialStructure in ContainsElements do
    for Product in RelContainedInSpatialStructure.RelatedElements do
      { The RelatedElements name is confusing, it actually contains any
        TIfcProduct instances, not just TIfcElement instances.
        We filter them for TIfcElement, to make us consistent with
        AddContainedElement. }
      if Product is TIfcElement then
        List.Add(TIfcElement(Product));
end;

{ TIfcRelPositions ----------------------------------------------------------- }

constructor TIfcRelPositions.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedProducts := TIfcProductList.Create(false);
end;

destructor TIfcRelPositions.Destroy;
begin
  FreeAndNil(FRelatedProducts);
  inherited;
end;

{ TIfcElement --------------------------------------------------------------- }

constructor TIfcElement.Create(AOwner: TComponent);
begin
  inherited;
  FFillsVoids := TIfcRelFillsElementList.Create(false);
  FConnectedTo := TIfcRelConnectsElementsList.Create(false);
  FIsInterferedByElements := TIfcRelInterferesElementsList.Create(false);
  FInterferesElements := TIfcRelInterferesElementsList.Create(false);
  FHasProjections := TIfcRelProjectsElementList.Create(false);
  FHasOpenings := TIfcRelVoidsElementList.Create(false);
  FIsConnectionRealization := TIfcRelConnectsWithRealizingElementsList.Create(false);
  FProvidesBoundaries := TIfcRelSpaceBoundaryList.Create(false);
  FConnectedFrom := TIfcRelConnectsElementsList.Create(false);
  FContainedInStructure := TIfcRelContainedInSpatialStructureList.Create(false);
  FHasCoverings := TIfcRelCoversBldgElementsList.Create(false);
  FHasSurfaceFeatures := TIfcRelAdheresToElementList.Create(false);
end;

destructor TIfcElement.Destroy;
begin
  FreeAndNil(FFillsVoids);
  FreeAndNil(FConnectedTo);
  FreeAndNil(FIsInterferedByElements);
  FreeAndNil(FInterferesElements);
  FreeAndNil(FHasProjections);
  FreeAndNil(FHasOpenings);
  FreeAndNil(FIsConnectionRealization);
  FreeAndNil(FProvidesBoundaries);
  FreeAndNil(FConnectedFrom);
  FreeAndNil(FContainedInStructure);
  FreeAndNil(FHasCoverings);
  FreeAndNil(FHasSurfaceFeatures);
  inherited;
end;

procedure TIfcElement.AddOpening(const Opening: TIfcFeatureElementSubtraction);
var
  RelVoidsElement: TIfcRelVoidsElement;
begin
  RelVoidsElement := TIfcRelVoidsElement.Create(Owner);
  RelVoidsElement.RelatingBuildingElement := Self;
  RelVoidsElement.RelatedOpeningElement := Opening;

  HasOpenings.Add(RelVoidsElement);

  Opening.VoidsElements.Add(RelVoidsElement);
end;

procedure TIfcElement.AddConnected(const Connected: TIfcElement);
var
  RelConnectsElements: TIfcRelConnectsElements;
begin
  RelConnectsElements := TIfcRelConnectsElements.Create(Owner);
  RelConnectsElements.RelatingElement := Self;
  RelConnectsElements.RelatedElement := Connected;

  ConnectedTo.Add(RelConnectsElements);

  Connected.ConnectedFrom.Add(RelConnectsElements);
end;

{ TIfcRelAggregates --------------------------------------------------------- }

constructor TIfcRelAggregates.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedObjects := TIfcObjectDefinitionList.Create(false);
end;

destructor TIfcRelAggregates.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  inherited;
end;

{ TIfcOpeningElement -------------------------------------------------------- }

constructor TIfcOpeningElement.Create(AOwner: TComponent);

begin
  inherited;
  FHasFillings := TIfcRelFillsElementList.Create(false);
end;

destructor TIfcOpeningElement.Destroy;
begin
  FreeAndNil(FHasFillings);
  inherited;
end;

{ TIfcShapeRepresentation --------------------------------------------------- }

function TIfcShapeRepresentation.WantsGlobalId: Boolean;
begin
  Result := true;
end;

{ TIfcProductRepresentation ------------------------------------------------- }

constructor TIfcProductRepresentation.Create(AOwner: TComponent);
begin
  inherited;
  FRepresentations := TIfcRepresentationList.Create(false);
end;

destructor TIfcProductRepresentation.Destroy;
begin
  FreeAndNil(FRepresentations);
  inherited;
end;

function TIfcProductRepresentation.WantsGlobalId: Boolean;
begin
  Result := true;
end;

{ TIfcProject -------------------------------------------------------------- }

constructor TIfcProject.Create(AOwner: TComponent);
begin
  inherited;
  Name := 'My Project';
end;

procedure TIfcProject.SetupUnits;
var
  UnitAssignment: TIfcUnitAssignment;
  DimensionalExponents: TIfcDimensionalExponents;
  IfcUnit: TIfcSIUnit;
begin
  UnitAssignment := TIfcUnitAssignment.Create(Owner);

  DimensionalExponents := TIfcDimensionalExponents.Create(Owner);

  IfcUnit := TIfcSIUnit.Create(Owner);
  IfcUnit.UnitType := TIfcUnitEnum.PlaneAngleUnit;
  IfcUnit.Name := TIfcSIUnitName.Radian;
  IfcUnit.Dimensions := DimensionalExponents;
  UnitAssignment.Units.Add(IfcUnit);

  DimensionalExponents := TIfcDimensionalExponents.Create(Owner);
  DimensionalExponents.LengthExponent := 1;

  IfcUnit := TIfcSIUnit.Create(Owner);
  IfcUnit.UnitType := TIfcUnitEnum.LengthUnit;
  IfcUnit.Name := TIfcSIUnitName.Metre;
  IfcUnit.Dimensions := DimensionalExponents;
  UnitAssignment.Units.Add(IfcUnit);

  DimensionalExponents := TIfcDimensionalExponents.Create(Owner);
  DimensionalExponents.LengthExponent := 2;

  IfcUnit := TIfcSIUnit.Create(Owner);
  IfcUnit.UnitType := TIfcUnitEnum.AreaUnit;
  IfcUnit.Name := TIfcSIUnitName.Square_Metre;
  IfcUnit.Dimensions := DimensionalExponents;
  UnitAssignment.Units.Add(IfcUnit);

  DimensionalExponents := TIfcDimensionalExponents.Create(Owner);
  DimensionalExponents.LengthExponent := 3;

  IfcUnit := TIfcSIUnit.Create(Owner);
  IfcUnit.UnitType := TIfcUnitEnum.VolumeUnit;
  IfcUnit.Name := TIfcSIUnitName.Cubic_Metre;
  IfcUnit.Dimensions := DimensionalExponents;
  UnitAssignment.Units.Add(IfcUnit);

  UnitsInContext := UnitAssignment;
end;

function TIfcProject.ModelContext: TIfcGeometricRepresentationContext;
var
  C: TIfcRepresentationContext;
begin
  for C in RepresentationContexts do
  begin
    if (C is TIfcGeometricRepresentationContext) and
       (C.ContextType = TIfcGeometricRepresentationContext.TypeModel) and
       (TIfcGeometricRepresentationContext(C).CoordinateSpaceDimension = 3) then
      Exit(TIfcGeometricRepresentationContext(C));
  end;
  Result := nil;
end;

function TIfcProject.PlanContext: TIfcGeometricRepresentationContext;
var
  C: TIfcRepresentationContext;
begin
  for C in RepresentationContexts do
  begin
    if (C is TIfcGeometricRepresentationContext) and
       (C.ContextType = TIfcGeometricRepresentationContext.TypePlan) and
       (TIfcGeometricRepresentationContext(C).CoordinateSpaceDimension = 2) then
      Exit(TIfcGeometricRepresentationContext(C));
  end;
  Result := nil;
end;

procedure TIfcProject.SetupModelContext;
var
  NewContext: TIfcGeometricRepresentationContext;
  WorldCoordinateSystem: TIfcAxis2Placement3D;
begin
  if ModelContext <> nil then
    Exit; // nothing needs to be done

  WorldCoordinateSystem := TIfcAxis2Placement3D.Create(Owner);
  WorldCoordinateSystem.Location := TIfcCartesianPoint.CreateVector(Owner, TVector3.Zero);
  WorldCoordinateSystem.Axis := TIfcDirection.CreateVector(Owner, TVector3.One[2]);
  WorldCoordinateSystem.RefDirection := TIfcDirection.CreateVector(Owner, TVector3.One[0]);

  NewContext := TIfcGeometricRepresentationContext.Create(Owner);
  NewContext.ContextType := TIfcGeometricRepresentationContext.TypeModel;
  NewContext.CoordinateSpaceDimension := 3;
  NewContext.Precision := SingleEpsilon;
  NewContext.WorldCoordinateSystem := WorldCoordinateSystem;

  RepresentationContexts.Add(NewContext);

  Assert(ModelContext <> nil);
end;

function TIfcProject.BestContainer: TIfcSpatialElement;

  function SpatialElementInside(const Parent: TIfcObjectDefinition): TIfcSpatialElement;
  var
    RelAggregates: TIfcRelAggregates;
    RelatedObject: TIfcObjectDefinition;
  begin
    for RelAggregates in Parent.IsDecomposedBy do
      for RelatedObject in RelAggregates.RelatedObjects do
        if RelatedObject is TIfcSpatialElement then
          Exit(TIfcSpatialElement(RelatedObject));
    Result := nil;
  end;

var
  NextResult: TIfcSpatialElement;
begin
  Result := nil;
  NextResult := SpatialElementInside(Self);
  while NextResult <> nil do
  begin
    Result := NextResult;
    NextResult := SpatialElementInside(Result);
  end;
end;

{ TIfcRepresentation -------------------------------------------------------- }

constructor TIfcRepresentation.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TIfcRepresentationItemList.Create(false);
  FRepresentationMap := TIfcRepresentationMapList.Create(false);
  FLayerAssignments := TIfcPresentationLayerAssignmentList.Create(false);
  FOfProductRepresentation := TIfcProductRepresentationList.Create(false);
end;

destructor TIfcRepresentation.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FRepresentationMap);
  FreeAndNil(FLayerAssignments);
  FreeAndNil(FOfProductRepresentation);
  inherited;
end;

{ TIfcFeatureElementSubtraction --------------------------------------------- }

constructor TIfcFeatureElementSubtraction.Create(AOwner: TComponent);
begin
  inherited;
  FVoidsElements := TIfcRelVoidsElementList.Create(false);
end;

destructor TIfcFeatureElementSubtraction.Destroy;
begin
  FreeAndNil(FVoidsElements);
  inherited;
end;

{ TIfcRelContainedInSpatialStructure ----------------------------------------- }

constructor TIfcRelContainedInSpatialStructure.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedElements := TIfcProductList.Create(false);
end;

destructor TIfcRelContainedInSpatialStructure.Destroy;
begin
  FreeAndNil(FRelatedElements);
  inherited;
end;

{ TIfcPostalAddress ---------------------------------------------------------- }

constructor TIfcPostalAddress.Create(AOwner: TComponent);
begin
  inherited;
  FAddressLines := TCastleStringList.Create;
end;

destructor TIfcPostalAddress.Destroy;
begin
  FreeAndNil(FAddressLines);
  inherited;
end;

{ TIfcRelConnectsWithRealizingElements --------------------------------------- }

constructor TIfcRelConnectsWithRealizingElements.Create(AOwner: TComponent);
begin
  inherited;
  FRealizingElements := TIfcElementList.Create(false);
end;

destructor TIfcRelConnectsWithRealizingElements.Destroy;
begin
  FreeAndNil(FRealizingElements);
  inherited;
end;

{ TIfcRelCoversBldgElements -------------------------------------------------- }

constructor TIfcRelCoversBldgElements.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FRelatedCoverings := TIfcCoveringList.Create(false);
end;

destructor TIfcRelCoversBldgElements.Destroy;
begin
  // TODO FreeAndNil(FRelatedCoverings);
  inherited;
end;

{ TIfcRelAdheresToElement --------------------------------------------------- }

constructor TIfcRelAdheresToElement.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FRelatedSurfaceFeatures := TIfcSurfaceFeatureList.Create(false);
end;

destructor TIfcRelAdheresToElement.Destroy;
begin
  // TODO FreeAndNil(FRelatedSurfaceFeatures);
  inherited;
end;

{ TIfcCartesianPoint --------------------------------------------------------- }

constructor TIfcCartesianPoint.Create(AOwner: TComponent);
begin
  inherited;
  FCoordinates := TCastleVector3PersistentSimple.Create;
end;

constructor TIfcCartesianPoint.CreateVector(const AOwner: TComponent; const AVector: TVector3);
begin
  Create(AOwner); // calling virtual constructor, so may call descendant
  Coordinates.Value := AVector;
end;

destructor TIfcCartesianPoint.Destroy;
begin
  FreeAndNil(FCoordinates);
  inherited;
end;

{ TIfcAxis2Placement2D ------------------------------------------------------ }

function TIfcAxis2Placement2D.P(const AxisCoord: T2DAxis): TVector2;

  { Derive orthogonal complement of a 2D vector.
    Follows https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOrthogonalComplement.htm }
  function IfcOrthogonalComplement(const V: TVector2): TVector2;
  begin
    Result := Vector2(-V.Y, V.X);
  end;

var
  D: TVector2;
begin
  { See
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement2D.htm
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuild2Axes.htm }
  if RefDirection <> nil then
    D := RefDirection.DirectionRatios.Value.XY.Normalize
  else
    D := Vector2(1, 0);

  case AxisCoord of
    0: Result := D;
    1: Result := IfcOrthogonalComplement(D);
    else raise EInternalError.Create('TIfcAxis2Placement2D.P: Invalid Axis');
  end;
end;

{ TIfcAxis2Placement3D ------------------------------------------------------ }

function TIfcAxis2Placement3D.P(const AxisCoord: T3DAxis): TVector3;
var
  D1: TVector3; // Z Axis fixed and normalized, from Axis
  D2: TVector3; // X axis fixed and normalized, from RefDirection
  RefDirectionNormalized, XVec: TVector3;
begin
  { See https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildAxes.htm }
  if Axis <> nil then
    D1 := Axis.DirectionRatios.Value.Normalize
  else
    D1 := TVector3.One[2];

  { See https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFirstProjAxis.htm,
    we effectively implement it, but simplifying -- the formal definition doesn't account for
    where it's actually used. }
  if RefDirection <> nil then
    RefDirectionNormalized := RefDirection.DirectionRatios.Value.Normalize
  else
  if not TVector3.Equals(D1, TVector3.One[0]) then
    RefDirectionNormalized := TVector3.One[0]
  else
    RefDirectionNormalized := TVector3.One[1];
  { Note: this should be 0 when RefDirectionNormalized and D1 are orthogonal,
    which is the most common case. }
  XVec := TVector3.DotProduct(RefDirectionNormalized, D1) * D1;
  D2 := (RefDirectionNormalized - XVec).Normalize;

  case AxisCoord of
    0: Result := D2;
    1: Result := TVector3.CrossProduct(D1, D2);
    2: Result := D1;
    else raise EInternalError.Create('TIfcAxis2Placement3D.P: Invalid AxisCoord');
  end;
end;

{ TIfcDirection ------------------------------------------------------------- }

constructor TIfcDirection.Create(AOwner: TComponent);
begin
  inherited;
  FDirectionRatios := TCastleVector3PersistentSimple.Create;
end;

constructor TIfcDirection.CreateVector(const AOwner: TComponent; const AVector: TVector3);
begin
  Create(AOwner); // calling virtual constructor, so may call descendant
  DirectionRatios.Value := AVector;
end;

destructor TIfcDirection.Destroy;
begin
  FreeAndNil(FDirectionRatios);
  inherited;
end;

{ TIfcAddress -------------------------------------------------------------- }

constructor TIfcAddress.Create(AOwner: TComponent);
begin
  inherited;
  FOfPerson := TIfcPersonList.Create(false);
  FOfOrganization := TIfcOrganizationList.Create(false);
end;

destructor TIfcAddress.Destroy;
begin
  FreeAndNil(FOfPerson);
  FreeAndNil(FOfOrganization);
  inherited;
end;

{ TIfcTelecomAddress --------------------------------------------------------- }

constructor TIfcTelecomAddress.Create(AOwner: TComponent);
begin
  inherited;
  FTelephoneNumbers := TCastleStringList.Create;
  FFacsimileNumbers := TCastleStringList.Create;
  FElectronicMailAddresses := TCastleStringList.Create;
  FMessagingIDs := TCastleStringList.Create;
end;

destructor TIfcTelecomAddress.Destroy;
begin
  FreeAndNil(FTelephoneNumbers);
  FreeAndNil(FFacsimileNumbers);
  FreeAndNil(FElectronicMailAddresses);
  FreeAndNil(FMessagingIDs);
  inherited;
end;

{ TIfcActorRole -------------------------------------------------------------- }

constructor TIfcActorRole.Create(AOwner: TComponent);
begin
  inherited;
  FHasExternalReference := TIfcExternalReferenceRelationshipList.Create(false);
end;

destructor TIfcActorRole.Destroy;
begin
  FreeAndNil(FHasExternalReference);
  inherited;
end;

{ TIfcPersonAndOrganization -------------------------------------------------- }

constructor TIfcPersonAndOrganization.Create(AOwner: TComponent);
begin
  inherited;
  FRoles := TIfcActorRoleList.Create(false);
end;

destructor TIfcPersonAndOrganization.Destroy;
begin
  FreeAndNil(FRoles);
  inherited;
end;

{ TIfcPerson --------------------------------------------------------------- }

constructor TIfcPerson.Create(AOwner: TComponent);
begin
  inherited;
  FAddresses := TIfcAddressList.Create(false);
  FEngagedIn := TIfcPersonAndOrganizationList.Create(false);
  FRoles := TIfcActorRoleList.Create(false);
  FMiddleNames := TCastleStringList.Create;
  FPrefixTitles := TCastleStringList.Create;
  FSuffixTitles := TCastleStringList.Create;
end;

destructor TIfcPerson.Destroy;
begin
  FreeAndNil(FAddresses);
  FreeAndNil(FEngagedIn);
  FreeAndNil(FRoles);
  FreeAndNil(FMiddleNames);
  FreeAndNil(FPrefixTitles);
  FreeAndNil(FSuffixTitles);
  inherited;
end;

{ TIfcProfileDef ------------------------------------------------------------- }

constructor TIfcProfileDef.Create(AOwner: TComponent);
begin
  inherited;
  FHasExternalReference := TIfcExternalReferenceRelationshipList.Create(false);
  FHasProperties := TIfcProfilePropertiesList.Create(false);
end;

destructor TIfcProfileDef.Destroy;
begin
  FreeAndNil(FHasExternalReference);
  FreeAndNil(FHasProperties);
  inherited;
end;

{ TIfcArbitraryProfileDefWithVoids ------------------------------------------- }

constructor TIfcArbitraryProfileDefWithVoids.Create(AOwner: TComponent);
begin
  inherited;
  FInnerCurves := TIfcCurveList.Create(false);
end;

destructor TIfcArbitraryProfileDefWithVoids.Destroy;
begin
  FreeAndNil(FInnerCurves);
  inherited;
end;

{ TIfcOrganization --------------------------------------------------------- }

constructor TIfcOrganization.Create(AOwner: TComponent);
begin
  inherited;
  FAddresses := TIfcAddressList.Create(false);
  FIsRelatedBy := TIfcOrganizationRelationshipList.Create(false);
  FRelates := TIfcOrganizationRelationshipList.Create(false);
  FEngages := TIfcPersonAndOrganizationList.Create(false);
  FRoles := TIfcActorRoleList.Create(false);
end;

destructor TIfcOrganization.Destroy;
begin
  FreeAndNil(FAddresses);
  FreeAndNil(FIsRelatedBy);
  FreeAndNil(FRelates);
  FreeAndNil(FEngages);
  FreeAndNil(FRoles);
  inherited;
end;

{ TIfcOrganizationRelationship --------------------------------------------- }

constructor TIfcOrganizationRelationship.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedOrganizations := TIfcOrganizationList.Create(false);
end;

destructor TIfcOrganizationRelationship.Destroy;
begin
  FreeAndNil(FRelatedOrganizations);
  inherited;
end;

{ TIfcTessellatedItem -------------------------------------------------------- }

constructor TIfcTessellatedItem.Create(AOwner: TComponent);
begin
  inherited;
  FLayerAssignment := TIfcPresentationLayerAssignmentList.Create(false);
end;

destructor TIfcTessellatedItem.Destroy;
begin
  FreeAndNil(FLayerAssignment);
  inherited;
end;

{ TIfcCartesianPointList2D --------------------------------------------------- }

constructor TIfcCartesianPointList2D.Create(AOwner: TComponent);
begin
  inherited;
  FCoordList := TVector2List.Create;
  FTagList := TCastleStringList.Create;
end;

destructor TIfcCartesianPointList2D.Destroy;
begin
  FreeAndNil(FCoordList);
  FreeAndNil(FTagList);
  inherited;
end;

{ TIfcCartesianPointList3D --------------------------------------------------- }

constructor TIfcCartesianPointList3D.Create(AOwner: TComponent);
begin
  inherited;
  FCoordList := TVector3List.Create;
  FTagList := TCastleStringList.Create;
end;

destructor TIfcCartesianPointList3D.Destroy;
begin
  FreeAndNil(FCoordList);
  FreeAndNil(FTagList);
  inherited;
end;

{ TIfcSurfaceStyleWithTextures ----------------------------------------------- }

constructor TIfcSurfaceStyleWithTextures.Create(AOwner: TComponent);
begin
  inherited;
  FTextures := TIfcSurfaceTextureList.Create(false);
end;

destructor TIfcSurfaceStyleWithTextures.Destroy;
begin
  FreeAndNil(FTextures);
  inherited;
end;

{ TIfcSurfaceTexture -------------------------------------------------------- }

constructor TIfcSurfaceTexture.Create(AOwner: TComponent);
begin
  inherited;
  FRepeatS := true;
  FRepeatT := true;
  FParameter := TCastleStringList.Create;
  FIsMappedBy := TIfcTextureCoordinateList.Create(false);
  FUsedInStyles := TIfcSurfaceStyleWithTexturesList.Create(false);
end;

destructor TIfcSurfaceTexture.Destroy;
begin
  FreeAndNil(FParameter);
  FreeAndNil(FIsMappedBy);
  FreeAndNil(FUsedInStyles);
  inherited;
end;

{ TIfcTextureCoordinateIndices ----------------------------------------------- }

constructor TIfcTextureCoordinateIndices.Create(AOwner: TComponent);
begin
  inherited;
  FTexCoordIndex := TIntegerList.Create;
end;

destructor TIfcTextureCoordinateIndices.Destroy;
begin
  FreeAndNil(FTexCoordIndex);
  inherited;
end;

{ TIfcTextureCoordinate ----------------------------------------------------- }

constructor TIfcTextureCoordinate.Create(AOwner: TComponent);
begin
  inherited;
  FMaps := TIfcSurfaceTextureList.Create(false);
end;

destructor TIfcTextureCoordinate.Destroy;
begin
  FreeAndNil(FMaps);
  inherited;
end;

{ TIfcTextureVertexList_Spec -------------------------------------------------- }

constructor TIfcTextureVertexList_Spec.Create(AOwner: TComponent);
begin
  inherited;
  FTexCoordsList := TVector2List.Create;
end;

destructor TIfcTextureVertexList_Spec.Destroy;
begin
  FreeAndNil(FTexCoordsList);
  inherited;
end;

{ TIfcIndexedPolygonalTextureMap --------------------------------------------- }

constructor TIfcIndexedPolygonalTextureMap.Create(AOwner: TComponent);
begin
  inherited;
  FTexCoordIndices := TIfcTextureCoordinateIndicesList.Create(false);
end;

destructor TIfcIndexedPolygonalTextureMap.Destroy;
begin
  FreeAndNil(FTexCoordIndices);
  inherited;
end;

{ TIfcIndexedPolygonalFace --------------------------------------------------- }

constructor TIfcIndexedPolygonalFace.Create(AOwner: TComponent);
begin
  inherited;
  FCoordIndex := TIntegerList.Create;
  FToFaceSet := TIfcPolygonalFaceSetList.Create(false);
end;

destructor TIfcIndexedPolygonalFace.Destroy;
begin
  FreeAndNil(FCoordIndex);
  FreeAndNil(FToFaceSet);
  inherited;
end;

{ TIfcTessellatedFaceSet ----------------------------------------------------- }

constructor TIfcTessellatedFaceSet.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FHasColours := TIfcIndexedColourMapList.Create(false);
  FHasTextures := TIfcIndexedTextureMapList.Create(false);
end;

destructor TIfcTessellatedFaceSet.Destroy;
begin
  // TODO FreeAndNil(FHasColours);
  FreeAndNil(FHasTextures);
  inherited;
end;

{ TIfcPolygonalFaceSet ------------------------------------------------------- }

constructor TIfcPolygonalFaceSet.Create(AOwner: TComponent);
begin
  inherited;
  FFaces := TIfcIndexedPolygonalFaceList.Create(false);
  FPnIndex := TIntegerList.Create;
end;

destructor TIfcPolygonalFaceSet.Destroy;
begin
  FreeAndNil(FFaces);
  FreeAndNil(FPnIndex);
  inherited;
end;

function TIfcPolygonalFaceSet.WantsGlobalId: Boolean;
begin
  { Needed to make IfcIndexedPolygonalFace.ToFaceSet be a reference.
    Testcase: save any model with non-empty IfcPolygonalFaceSet. }
  Result := true;
end;

{ TIfcTriangulatedFaceSet ---------------------------------------------------- }

constructor TIfcTriangulatedFaceSet.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := TVector3List.Create;
  FCoordIndex := TVector3IntegerList.Create;
  FPnIndex := TIntegerList.Create;
end;

destructor TIfcTriangulatedFaceSet.Destroy;
begin
  FreeAndNil(FNormals);
  FreeAndNil(FCoordIndex);
  FreeAndNil(FPnIndex);
  inherited;
end;

{ TIfcPolyline ------------------------------------------------------------- }

constructor TIfcPolyline.Create(AOwner: TComponent);
begin
  inherited;
  FPoints := TIfcCartesianPointList.Create(false);
end;

destructor TIfcPolyline.Destroy;
begin
  FreeAndNil(FPoints);
  inherited;
end;

{ TIfcTrimmedCurve ----------------------------------------------------------- }

constructor TIfcTrimmedCurve.Create(AOwner: TComponent);
begin
  inherited;
  FTrim1 := TIfcCartesianPointList.Create(false);
  FTrim2 := TIfcCartesianPointList.Create(false);
end;

destructor TIfcTrimmedCurve.Destroy;
begin
  FreeAndNil(FTrim1);
  FreeAndNil(FTrim2);
  inherited;
end;

{ TIfcPropertyDefinition ----------------------------------------------------- }

constructor TIfcPropertyDefinition.Create(AOwner: TComponent);
begin
  inherited;
  FHasAssociations := TIfcRelAssociatesList.Create(false);
end;

destructor TIfcPropertyDefinition.Destroy;
begin
  FreeAndNil(FHasAssociations);
  inherited;
end;

{ TIfcRelDeclares ------------------------------------------------------------ }

constructor TIfcRelDeclares.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedDefinitions := TIfcDefinitionSelectList.Create(false);
end;

destructor TIfcRelDeclares.Destroy;
begin
  FreeAndNil(FRelatedDefinitions);
  inherited;
end;

{ TIfcRelAssociates ---------------------------------------------------------- }

constructor TIfcRelAssociates.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedObjects := TIfcDefinitionSelectList.Create(false);
end;

destructor TIfcRelAssociates.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  inherited;
end;

{ TIfcObjectPlacement -------------------------------------------------------- }

constructor TIfcObjectPlacement.Create(AOwner: TComponent);
begin
  inherited;
  FPlacesObject := TIfcProductList.Create(false);
  FReferencedByPlacements := TIfcObjectPlacementList.Create(false);
end;

destructor TIfcObjectPlacement.Destroy;
begin
  FreeAndNil(FPlacesObject);
  FreeAndNil(FReferencedByPlacements);
  inherited;
end;

{ TIfcContext ---------------------------------------------------------------- }

constructor TIfcContext.Create(AOwner: TComponent);
begin
  inherited;
  FRepresentationContexts := TIfcRepresentationContextList.Create(false);
  FIsDefinedBy := TIfcRelDefinesByPropertiesList.Create(false);
  FDeclares := TIfcRelDeclaresList.Create(false);
end;

destructor TIfcContext.Destroy;
begin
  FreeAndNil(FRepresentationContexts);
  FreeAndNil(FIsDefinedBy);
  FreeAndNil(FDeclares);
  inherited;
end;

{ TIfcRepresentationContext -------------------------------------------------- }

constructor TIfcRepresentationContext.Create(AOwner: TComponent);
begin
  inherited;
  FRepresentationsInContext := TIfcRepresentationList.Create(false);
end;

destructor TIfcRepresentationContext.Destroy;
begin
  FreeAndNil(FRepresentationsInContext);
  inherited;
end;

{ TIfcGeometricRepresentationContext ----------------------------------------- }

constructor TIfcGeometricRepresentationContext.Create(AOwner: TComponent);
begin
  inherited;
  FHasSubContexts := TIfcGeometricRepresentationSubContextList.Create(false);
end;

destructor TIfcGeometricRepresentationContext.Destroy;
begin
  FreeAndNil(FHasSubContexts);
  inherited;
end;

function TIfcGeometricRepresentationContext.WantsGlobalId: Boolean;
begin
  Result := true;
end;

{ TIfcUnitAssignment --------------------------------------------------------- }

constructor TIfcUnitAssignment.Create(AOwner: TComponent);
begin
  inherited;
  FUnits := TIfcUnitList.Create(false);
end;

destructor TIfcUnitAssignment.Destroy;
begin
  FreeAndNil(FUnits);
  inherited;
end;

{ TIfcRelDefinesByProperties ------------------------------------------------- }

constructor TIfcRelDefinesByProperties.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedObjects := TIfcObjectDefinitionList.Create(false);
  FRelatingPropertyDefinition := TIfcPropertySetDefinitionList.Create(false);
end;

destructor TIfcRelDefinesByProperties.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  FreeAndNil(FRelatingPropertyDefinition);
  inherited;
end;

{ TIfcPropertyTemplate ------------------------------------------------------- }

constructor TIfcPropertyTemplate.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FPartOfComplexTemplate := TIfcComplexPropertyTemplateList.Create(false);
  FPartOfPsetTemplate := TIfcPropertySetTemplateList.Create(false);
end;

destructor TIfcPropertyTemplate.Destroy;
begin
  // TODO FreeAndNil(FPartOfComplexTemplate);
  FreeAndNil(FPartOfPsetTemplate);
  inherited;
end;

{ TIfcPropertySetTemplate ---------------------------------------------------- }

constructor TIfcPropertySetTemplate.Create(AOwner: TComponent);
begin
  inherited;
  FHasPropertyTemplates := TIfcPropertyTemplateList.Create(false);
  FDefines := TIfcRelDefinesByTemplateList.Create(false);
end;

destructor TIfcPropertySetTemplate.Destroy;
begin
  FreeAndNil(FHasPropertyTemplates);
  FreeAndNil(FDefines);
  inherited;
end;

{ TIfcRelDefinesByTemplate --------------------------------------------------- }

constructor TIfcRelDefinesByTemplate.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedPropertySets := TIfcPropertySetDefinitionList.Create(false);
end;

destructor TIfcRelDefinesByTemplate.Destroy;
begin
  FreeAndNil(FRelatedPropertySets);
  inherited;
end;

{ TIfcConversionBasedUnit ---------------------------------------------------- }

constructor TIfcConversionBasedUnit.Create(AOwner: TComponent);
begin
  inherited;
  FHasExternalReference := TIfcExternalReferenceRelationshipList.Create(false);
end;

destructor TIfcConversionBasedUnit.Destroy;
begin
  FreeAndNil(FHasExternalReference);
  inherited;
end;

{ TIfcProductDefinitionShape ------------------------------------------------- }

constructor TIfcProductDefinitionShape.Create(AOwner: TComponent);
begin
  inherited;
  FShapeOfProduct := TIfcProductList.Create(false);
  FHasShapeAspects := TIfcShapeAspectList.Create(false);
end;

destructor TIfcProductDefinitionShape.Destroy;
begin
  FreeAndNil(FShapeOfProduct);
  FreeAndNil(FHasShapeAspects);
  inherited;
end;

{ TIfcRelDefinesByObject ----------------------------------------------------- }

constructor TIfcRelDefinesByObject.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedObjects := TIfcObjectList.Create(false);
end;

destructor TIfcRelDefinesByObject.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  inherited;
end;

{ TIfcPropertySetDefinition -------------------------------------------------- }

constructor TIfcPropertySetDefinition.Create(AOwner: TComponent);
begin
  inherited;
  FDefinesType := TIfcTypeObjectList.Create(false);
  FIsDefinedBy := TIfcRelDefinesByTemplateList.Create(false);
  FDefinesOccurrence := TIfcRelDefinesByPropertiesList.Create(false);
end;

destructor TIfcPropertySetDefinition.Destroy;
begin
  FreeAndNil(FDefinesType);
  FreeAndNil(FIsDefinedBy);
  FreeAndNil(FDefinesOccurrence);
  inherited;
end;

{ TIfcObject ----------------------------------------------------------------- }

constructor TIfcObject.Create(AOwner: TComponent);
begin
  inherited;
  FIsDeclaredBy := TIfcRelDefinesByObjectList.Create(false);
  FDeclares := TIfcRelDefinesByObjectList.Create(false);
  FIsTypedBy := TIfcRelDefinesByTypeList.Create(false);
  FIsDefinedBy := TIfcRelDefinesByPropertiesList.Create(false);
end;

destructor TIfcObject.Destroy;
begin
  FreeAndNil(FIsDeclaredBy);
  FreeAndNil(FDeclares);
  FreeAndNil(FIsTypedBy);
  FreeAndNil(FIsDefinedBy);
  inherited;
end;

{ TIfcPropertySet ------------------------------------------------------------ }

constructor TIfcPropertySet.Create(AOwner: TComponent);
begin
  inherited;
  FHasProperties := TIfcPropertyList.Create(false);
end;

destructor TIfcPropertySet.Destroy;
begin
  FreeAndNil(FHasProperties);
  inherited;
end;

{ TIfcTypeObject ------------------------------------------------------------- }

constructor TIfcTypeObject.Create(AOwner: TComponent);
begin
  inherited;
  FHasPropertySets := TIfcPropertySetDefinitionList.Create(false);
  FTypes := TIfcRelDefinesByTypeList.Create(false);
end;

destructor TIfcTypeObject.Destroy;
begin
  FreeAndNil(FHasPropertySets);
  FreeAndNil(FTypes);
  inherited;
end;

{ TIfcCartesianTransformationOperator ---------------------------------------- }

type
  T3Axis = array [0..2] of TVector3;

{ Get rotation as axis-angle (X3D) from 3 axes of the new coordinate system
  (IFC convention). }
function RotationFrom3Axis(const NewAxis: T3Axis): TVector4;
var
  RotationMatrix: TMatrix3;
  I, J: Integer;
  RotationQuat: TQuaternion;
begin
  // optimize: early exit in the most common case (no rotation)
  if TVector3.Equals(NewAxis[0], TVector3.One[0]) and
     TVector3.Equals(NewAxis[1], TVector3.One[1]) and
     TVector3.Equals(NewAxis[2], TVector3.One[2]) then
    Exit(DefaultX3DRotation);

  for I := 0 to 2 do
    for J := 0 to 2 do
      RotationMatrix.Data[I, J] := NewAxis[I][J];
  RotationQuat := QuatFromRotationMatrix(RotationMatrix);
  Result := RotationQuat.ToAxisAngle;
end;

function TIfcCartesianTransformationOperator.Scl: Single;
begin
  if Scale <> 0 then
    Result := Scale
  else
    Result := 1;
end;

function TIfcCartesianTransformationOperator.Scl2: Single;
begin
  Result := Scl; // in this classs, Scl2 is equal Scl
end;

function TIfcCartesianTransformationOperator.Scl3: Single;
begin
  Result := Scl; // in this classs, Scl3 is equal Scl
end;

function TIfcCartesianTransformationOperator.Axis1Vector: TVector3;
begin
  if Axis1 <> nil then
    Result := Axis1.DirectionRatios.Value
  else
    Result := TVector3.One[0];
end;

function TIfcCartesianTransformationOperator.Axis2Vector: TVector3;
begin
  if Axis2 <> nil then
    Result := Axis2.DirectionRatios.Value
  else
    Result := TVector3.One[1];
end;

function TIfcCartesianTransformationOperator.Axis3Vector: TVector3;
begin
  Result := TVector3.One[2]; // will be customized by TIfcCartesianTransformationOperator3D
end;

procedure TIfcCartesianTransformationOperator.GetTransform(
  out Translation: TVector3; out Rotation: TVector4; out AScale: TVector3);
var
  NewAxis: T3Axis;
begin
  if LocalOrigin <> nil then
    Translation := LocalOrigin.Coordinates.Value
  else
    Translation := TVector3.Zero;

  NewAxis[0] := Axis1Vector;
  NewAxis[1] := Axis2Vector;
  NewAxis[2] := Axis3Vector;
  Rotation := RotationFrom3Axis(NewAxis);

  AScale := Vector3(Scl, Scl2, Scl3);
end;

{ TIfcCartesianTransformationOperator3D -------------------------------------- }

function TIfcCartesianTransformationOperator3D.Axis3Vector: TVector3;
begin
  if Axis3 <> nil then
    Result := Axis3.DirectionRatios.Value
  else
    Result := TVector3.One[2];
end;

function TIfcCartesianTransformationOperator3DnonUniform.Scl2: Single;
begin
  if Scale2 <> 0 then
    Result := Scale2
  else
    Result := Scl;
end;

function TIfcCartesianTransformationOperator3DnonUniform.Scl3: Single;
begin
  if Scale3 <> 0 then
    Result := Scale3
  else
    Result := Scl;
end;

{ TIfcRepresentationMap ------------------------------------------------------ }

constructor TIfcRepresentationMap.Create(AOwner: TComponent);
begin
  inherited;
  FHasShapeAspects := TIfcShapeAspectList.Create(false);
  FMapUsage := TIfcMappedItemList.Create(false);
end;

destructor TIfcRepresentationMap.Destroy;
begin
  FreeAndNil(FHasShapeAspects);
  FreeAndNil(FMapUsage);
  inherited;
end;

{ TIfcTypeProduct ------------------------------------------------------------ }

constructor TIfcTypeProduct.Create(AOwner: TComponent);
begin
  inherited;
  FRepresentationMaps := TIfcRepresentationMapList.Create(false);
end;

destructor TIfcTypeProduct.Destroy;
begin
  FreeAndNil(FRepresentationMaps);
  inherited;
end;

{ TIfcRelDefinesByType ------------------------------------------------------- }

constructor TIfcRelDefinesByType.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedObjects := TIfcObjectList.Create(false);
end;

destructor TIfcRelDefinesByType.Destroy;
begin
  FreeAndNil(FRelatedObjects);
  inherited;
end;

{ TIfcShapeAspect ------------------------------------------------------------ }

constructor TIfcShapeAspect.Create(AOwner: TComponent);
begin
  inherited;
  FShapeRepresentations := TIfcShapeModelList.Create(false);
  FHasExternalReferences := TIfcExternalReferenceRelationshipList.Create(false);
end;

destructor TIfcShapeAspect.Destroy;
begin
  FreeAndNil(FShapeRepresentations);
  FreeAndNil(FHasExternalReferences);
  inherited;
end;

{ TIfcLineIndex ------------------------------------------------------------- }

constructor TIfcLineIndex.Create(AOwner: TComponent);
begin
  inherited;
  FValue := TIntegerList.Create;
end;

destructor TIfcLineIndex.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

{ TIfcArcIndex --------------------------------------------------------------- }

constructor TIfcArcIndex.Create(AOwner: TComponent);
begin
  inherited;
  FValue := TIntegerList.Create;
end;

destructor TIfcArcIndex.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

{ TIfcIndexedPolyCurve ------------------------------------------------------- }

constructor TIfcIndexedPolyCurve.Create(AOwner: TComponent);
begin
  inherited;
  FSegments := TIfcSegmentIndexSelectList.Create(false);
end;

destructor TIfcIndexedPolyCurve.Destroy;
begin
  FreeAndNil(FSegments);
  inherited;
end;

{ TIfcExternalReference ------------------------------------------------------ }

constructor TIfcExternalReference.Create(AOwner: TComponent);
begin
  inherited;
  FHasExternalReferenceForResources := TIfcExternalReferenceRelationshipList.Create(false);
end;

destructor TIfcExternalReference.Destroy;
begin
  FreeAndNil(FHasExternalReferenceForResources);
  inherited;
end;

{ TIfcPropertyAbstraction ---------------------------------------------------- }

constructor TIfcPropertyAbstraction.Create(AOwner: TComponent);
begin
  inherited;
  FHasExternalReferences := TIfcExternalReferenceRelationshipList.Create(false);
end;

destructor TIfcPropertyAbstraction.Destroy;
begin
  FreeAndNil(FHasExternalReferences);
  inherited;
end;

{ TIfcExtendedProperties ----------------------------------------------------- }

constructor TIfcExtendedProperties.Create(AOwner: TComponent);
begin
  inherited;
  FProperties := TIfcPropertyList.Create(false);
end;

destructor TIfcExtendedProperties.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

{ TIfcProperty --------------------------------------------------------------- }

constructor TIfcProperty.Create(AOwner: TComponent);
begin
  inherited;
  FPartOfPset := TIfcPropertySetList.Create(false);
  FPropertyForDependance := TIfcPropertyDependencyRelationshipList.Create(false);
  FPropertyDependsOn := TIfcPropertyDependencyRelationshipList.Create(false);
  FPartOfComplex := TIfcComplexPropertyList.Create(false);
  // TODO FHasConstraints := TIfcResourceConstraintRelationshipList.Create(false);
  // TODO FHasApprovals := TIfcResourceApprovalRelationshipList.Create(false);
end;

destructor TIfcProperty.Destroy;
begin
  FreeAndNil(FPartOfPset);
  FreeAndNil(FPropertyForDependance);
  FreeAndNil(FPropertyDependsOn);
  FreeAndNil(FPartOfComplex);
  // TODO FreeAndNil(FHasConstraints);
  // TODO FreeAndNil(FHasApprovals);
  inherited;
end;

{ TIfcComplexProperty ------------------------------------------------------- }

constructor TIfcComplexProperty.Create(AOwner: TComponent);
begin
  inherited;
  FHasProperties := TIfcPropertyList.Create(false);
end;

destructor TIfcComplexProperty.Destroy;
begin
  FreeAndNil(FHasProperties);
  inherited;
end;

{ TIfcMaterialDefinition ----------------------------------------------------- }

constructor TIfcMaterialDefinition.Create(AOwner: TComponent);
begin
  inherited;
  FAssociatedTo := TIfcRelAssociatesMaterialList.Create(false);
  FHasExternalReferences := TIfcExternalReferenceRelationshipList.Create(false);
  FHasProperties := TIfcMaterialPropertiesList.Create(false);
end;

destructor TIfcMaterialDefinition.Destroy;
begin
  FreeAndNil(FAssociatedTo);
  FreeAndNil(FHasExternalReferences);
  FreeAndNil(FHasProperties);
  inherited;
end;

{ TIfcMaterialRelationship --------------------------------------------------- }

constructor TIfcMaterialRelationship.Create(AOwner: TComponent);
begin
  inherited;
  FRelatedMaterials := TIfcMaterialList.Create(false);
end;

destructor TIfcMaterialRelationship.Destroy;
begin
  FreeAndNil(FRelatedMaterials);
  inherited;
end;

{ TIfcMaterial --------------------------------------------------------------- }

constructor TIfcMaterial.Create(AOwner: TComponent);
begin
  inherited;
  FIsRelatedWith := TIfcMaterialRelationshipList.Create(false);
end;

destructor TIfcMaterial.Destroy;
begin
  FreeAndNil(FIsRelatedWith);
  inherited;
end;

{ TIfcMaterialLayerSet ------------------------------------------------------- }

constructor TIfcMaterialLayerSet.Create(AOwner: TComponent);
begin
  inherited;
  FMaterialLayers := TIfcMaterialLayerList.Create(false);
end;

destructor TIfcMaterialLayerSet.Destroy;
begin
  FreeAndNil(FMaterialLayers);
  inherited;
end;

{ TIfcMaterialConstituentSet ------------------------------------------------- }

constructor TIfcMaterialConstituentSet.Create(AOwner: TComponent);
begin
  inherited;
  FMaterialConstituents := TIfcMaterialConstituentList.Create(false);
end;

destructor TIfcMaterialConstituentSet.Destroy;
begin
  FreeAndNil(FMaterialConstituents);
  inherited;
end;

{ TIfcCompositeProfileDef ---------------------------------------------------- }

constructor TIfcCompositeProfileDef.Create(AOwner: TComponent);
begin
  inherited;
  FProfiles := TIfcProfileDefList.Create(false);
end;

destructor TIfcCompositeProfileDef.Destroy;
begin
  FreeAndNil(FProfiles);
  inherited;
end;

{ TIfcMaterialProfileSet ----------------------------------------------------- }

constructor TIfcMaterialProfileSet.Create(AOwner: TComponent);
begin
  inherited;
  FMaterialProfiles := TIfcMaterialProfileList.Create(false);
end;

destructor TIfcMaterialProfileSet.Destroy;
begin
  FreeAndNil(FMaterialProfiles);
  inherited;
end;

{ TIfcMaterialUsageDefinition ------------------------------------------------ }

constructor TIfcMaterialUsageDefinition.Create(AOwner: TComponent);
begin
  inherited;
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FAssociatedTo := TIfcRelAssociatesMaterialList.Create(false);
  {$endif CASTLE_IFC_INVERSE_PROPS}
end;

destructor TIfcMaterialUsageDefinition.Destroy;
begin
  {$ifdef CASTLE_IFC_INVERSE_PROPS}
  FreeAndNil(FAssociatedTo);
  {$endif CASTLE_IFC_INVERSE_PROPS}
  inherited;
end;

{ TIfcDerivedUnit ------------------------------------------------------------ }

constructor TIfcDerivedUnit.Create(AOwner: TComponent);
begin
  inherited;
  FElements := TIfcDerivedUnitElementList.Create(false);
end;

destructor TIfcDerivedUnit.Destroy;
begin
  FreeAndNil(FElements);
  inherited;
end;

{ initialization, finalization ----------------------------------------------- }

var
  FIfcClasses: TIfcPersistentClassList;

function IfcClasses: TIfcPersistentClassList;

  procedure InitializeIfcClasses;
  begin
    FIfcClasses := TIfcPersistentClassList.Create;
    FIfcClasses.Add(TIfcPresentationStyle);
    FIfcClasses.Add(TIfcPresentationLayerAssignment);
    FIfcClasses.Add(TIfcRepresentationItem);
    FIfcClasses.Add(TIfcDirection);
    FIfcClasses.Add(TIfcGeometricRepresentationItem);
    FIfcClasses.Add(TIfcSolidModel);
    FIfcClasses.Add(TIfcSweptAreaSolid);
    FIfcClasses.Add(TIfcExtrudedAreaSolid);
    FIfcClasses.Add(TIfcRoot);
    FIfcClasses.Add(TIfcObjectDefinition);
    FIfcClasses.Add(TIfcObject);
    FIfcClasses.Add(TIfcObjectPlacement);
    FIfcClasses.Add(TIfcPoint);
    FIfcClasses.Add(TIfcCartesianPoint);
    FIfcClasses.Add(TIfcPlacement);
    FIfcClasses.Add(TIfcAxis2Placement2D);
    FIfcClasses.Add(TIfcAxis2Placement3D);
    FIfcClasses.Add(TIfcLocalPlacement);
    FIfcClasses.Add(TIfcProduct);
    FIfcClasses.Add(TIfcSpatialElement);
    FIfcClasses.Add(TIfcSpatialStructureElement);
    FIfcClasses.Add(TIfcSite);
    FIfcClasses.Add(TIfcFacility);
    FIfcClasses.Add(TIfcBuilding);
    FIfcClasses.Add(TIfcBuildingStorey);
    FIfcClasses.Add(TIfcRelDecomposes);
    FIfcClasses.Add(TIfcElement);
    FIfcClasses.Add(TIfcFeatureElement);
    FIfcClasses.Add(TIfcFeatureElementSubtraction);
    FIfcClasses.Add(TIfcWindow);
    FIfcClasses.Add(TIfcRelVoidsElement);
    FIfcClasses.Add(TIfcRelFillsElement);
    FIfcClasses.Add(TIfcBuiltElement);
    FIfcClasses.Add(TIfcBeam);
    FIfcClasses.Add(TIfcBeamType);
    FIfcClasses.Add(TIfcBeamStandardCase);
    FIfcClasses.Add(TIfcOpeningElement);
    FIfcClasses.Add(TIfcRelAggregates);
    FIfcClasses.Add(TIfcContext);
    FIfcClasses.Add(TIfcProject);
    FIfcClasses.Add(TIfcRepresentation);
    FIfcClasses.Add(TIfcShapeModel);
    FIfcClasses.Add(TIfcShapeRepresentation);
    FIfcClasses.Add(TIfcProductRepresentation);
    FIfcClasses.Add(TIfcStyledItem);
    FIfcClasses.Add(TIfcPersonAndOrganization);
    FIfcClasses.Add(TIfcAddress);
    FIfcClasses.Add(TIfcPerson);
    FIfcClasses.Add(TIfcProfileDef);
    FIfcClasses.Add(TIfcOrganization);
    FIfcClasses.Add(TIfcOrganizationRelationship);
    FIfcClasses.Add(TIfcOwnerHistory);
    FIfcClasses.Add(TIfcApplication);
    FIfcClasses.Add(TIfcWall);
    FIfcClasses.Add(TIfcWallStandardCase);
    FIfcClasses.Add(TIfcWallElementedCase);
    FIfcClasses.Add(TIfcPolyline);
    FIfcClasses.Add(TIfcPropertyDefinition);
    FIfcClasses.Add(TIfcRelDeclares);
    FIfcClasses.Add(TIfcPostalAddress);
    FIfcClasses.Add(TIfcCompoundPlaneAngleMeasure);
    FIfcClasses.Add(TIfcDefinitionSelect);
    FIfcClasses.Add(TIfcDimensionalExponents);
    FIfcClasses.Add(TIfcMonetaryUnit);
    FIfcClasses.Add(TIfcDerivedUnit);
    FIfcClasses.Add(TIfcPlaneAngleMeasure);
    FIfcClasses.Add(TIfcAreaMeasure_Boxed);
    FIfcClasses.Add(TIfcMassMeasure);
    FIfcClasses.Add(TIfcLengthMeasure_Boxed);
    FIfcClasses.Add(TIfcPressureMeasure);
    FIfcClasses.Add(TIfcVolumeMeasure);
    FIfcClasses.Add(TIfcThermodynamicTemperatureMeasure);
    FIfcClasses.Add(TIfcRepresentationContext);
    FIfcClasses.Add(TIfcUnitAssignment);
    FIfcClasses.Add(TIfcRelDefinesByProperties);
    FIfcClasses.Add(TIfcSIUnit);
    FIfcClasses.Add(TIfcGeometricRepresentationContext);
    FIfcClasses.Add(TIfcConversionBasedUnit);
    FIfcClasses.Add(TIfcConversionBasedUnitWithOffset);
    FIfcClasses.Add(TIfcMeasureWithUnit);
    FIfcClasses.Add(TIfcProjectLibrary);
    FIfcClasses.Add(TIfcProductDefinitionShape);
    FIfcClasses.Add(TIfcRelAssociatesMaterial);
    FIfcClasses.Add(TIfcRelContainedInSpatialStructure);
    FIfcClasses.Add(TIfcRelDefinesByType);
    FIfcClasses.Add(TIfcTypeObject);
    FIfcClasses.Add(TIfcWindowType);
    FIfcClasses.Add(TIfcShapeAspect);
    FIfcClasses.Add(TIfcExternalReference);
    FIfcClasses.Add(TIfcExternalReferenceRelationship);
    FIfcClasses.Add(TIfcArbitraryClosedProfileDef);
    FIfcClasses.Add(TIfcRelDefinesByObject);
    FIfcClasses.Add(TIfcRepresentationMap);
    FIfcClasses.Add(TIfcProfileProperties);
    FIfcClasses.Add(TIfcRelConnectsElements);
    FIfcClasses.Add(TIfcRelInterferesElements);
    FIfcClasses.Add(TIfcRelProjectsElement);
    FIfcClasses.Add(TIfcRelConnectsWithRealizingElements);
    FIfcClasses.Add(TIfcRelSpaceBoundary);
    FIfcClasses.Add(TIfcRelCoversBldgElements);
    FIfcClasses.Add(TIfcRelAdheresToElement);
    FIfcClasses.Add(TIfcSpatialZone);
    FIfcClasses.Add(TIfcPropertySetDefinition);
    FIfcClasses.Add(TIfcPropertySet);
    FIfcClasses.Add(TIfcPropertyTemplate);
    FIfcClasses.Add(TIfcPropertySetTemplate);
    FIfcClasses.Add(TIfcRelDefinesByTemplate);
    FIfcClasses.Add(TIfcPropertySingleValue);
    FIfcClasses.Add(TIfcComplexProperty);
    FIfcClasses.Add(TIfcMaterialLayerSet);
    FIfcClasses.Add(TIfcMaterialLayer);
    FIfcClasses.Add(TIfcMaterialLayerSetUsage);
    FIfcClasses.Add(TIfcMaterial);
    FIfcClasses.Add(TIfcMaterialDefinitionRepresentation);
    FIfcClasses.Add(TIfcMaterialRelationship);
    FIfcClasses.Add(TIfcMaterialConstituent);
    FIfcClasses.Add(TIfcMaterialConstituentSet);
    FIfcClasses.Add(TIfcActuatorType);
    FIfcClasses.Add(TIfcColumn);
    FIfcClasses.Add(TIfcPolygonalFaceSet);
    FIfcClasses.Add(TIfcTriangulatedFaceSet);
    FIfcClasses.Add(TIfcSurfaceTexture);
    FIfcClasses.Add(TIfcImageTexture);
    FIfcClasses.Add(TIfcTextureVertexList_Spec);
    FIfcClasses.Add(TIfcIndexedTextureMap);
    FIfcClasses.Add(TIfcIndexedPolygonalTextureMap);
    FIfcClasses.Add(TIfcTextureCoordinateIndices);
    FIfcClasses.Add(TIfcIndexedPolygonalFace);
    FIfcClasses.Add(TIfcCartesianPointList3D);
    FIfcClasses.Add(TIfcTelecomAddress);
    FIfcClasses.Add(TIfcActorRole);
    FIfcClasses.Add(TIfcSlab);
    FIfcClasses.Add(TIfcSlabStandardCase);
    FIfcClasses.Add(TIfcIndexedPolyCurve);
    FIfcClasses.Add(TIfcLineIndex);
    FIfcClasses.Add(TIfcArcIndex);
    FIfcClasses.Add(TIfcCartesianPointList2D);
    FIfcClasses.Add(TIfcDoor);
    FIfcClasses.Add(TIfcUnit);
    FIfcClasses.Add(TIfcPersistent);
    FIfcClasses.Add(TIfcRelAssociates);
    FIfcClasses.Add(TIfcProperty);
    FIfcClasses.Add(TIfcSegmentIndexSelect);
    FIfcClasses.Add(TIfcDoorType);
    FIfcClasses.Add(TIfcWallType);
    FIfcClasses.Add(TIfcGeometricRepresentationSubContext);
    FIfcClasses.Add(TIfcReal_Boxed);
    FIfcClasses.Add(TIfcSlabType);
    FIfcClasses.Add(TIfcBuildingElementProxy);
    FIfcClasses.Add(TIfcBuildingElementProxyType);
    FIfcClasses.Add(TIfcRectangleProfileDef);
    FIfcClasses.Add(TIfcBoundingBox);
    FIfcClasses.Add(TIfcTrimmedCurve);
    FIfcClasses.Add(TIfcEllipse);
    FIfcClasses.Add(TIfcMappedItem);
    FIfcClasses.Add(TIfcCartesianTransformationOperator2D);
    FIfcClasses.Add(TIfcCartesianTransformationOperator3D);
    FIfcClasses.Add(TIfcCartesianTransformationOperator3DnonUniform);
    FIfcClasses.Add(TIfcCurve);
    FIfcClasses.Add(TIfcArbitraryProfileDefWithVoids);
    FIfcClasses.Add(TIfcSurfaceStyleWithTextures);
    FIfcClasses.Add(TIfcDistributionElementType);
    FIfcClasses.Add(TIfcBoilerType);
    FIfcClasses.Add(TIfcFurnishingElementType);
    FIfcClasses.Add(TIfcSystemFurnitureElementType);
    FIfcClasses.Add(TIfcFurnitureType);
    FIfcClasses.Add(TIfcFurnishingElement);
    FIfcClasses.Add(TIfcSystemFurnitureElement);
    FIfcClasses.Add(TIfcFurniture);
    FIfcClasses.Add(TIfcCircle);
    FIfcClasses.Add(TIfcPositioningElement);
    FIfcClasses.Add(TIfcVirtualGridIntersection);
    FIfcClasses.Add(TIfcGridAxis);
    FIfcClasses.Add(TIfcGrid);
    FIfcClasses.Add(TIfcRelPositions);
    FIfcClasses.Add(TIfcCompositeProfileDef);
    FIfcClasses.Add(TIfcMaterialProfile);
    FIfcClasses.Add(TIfcMaterialProfileSet);
    FIfcClasses.Add(TIfcMaterialUsageDefinition);
    FIfcClasses.Add(TIfcMaterialProfileSetUsage);
    FIfcClasses.Add(TIfcDerivedUnitElement);
    FIfcClasses.Add(TIfcColumnStandardCase);
    FIfcClasses.Add(TIfcElementAssemblyType);
    FIfcClasses.Add(TIfcElementComponentType);
    FIfcClasses.Add(TIfcBuildingElementPartType);
    FIfcClasses.Add(TIfcFlowTerminalType);
    FIfcClasses.Add(TIfcAirTerminalType);
    FIfcClasses.Add(TIfcSanitaryTerminalType);
    FIfcClasses.Add(TIfcElementAssembly);
    FIfcClasses.Add(TIfcElementComponent);
    FIfcClasses.Add(TIfcBuildingElementPart);
    FIfcClasses.Add(TIfcDistributionElement);
    FIfcClasses.Add(TIfcDistributionFlowElement);
    FIfcClasses.Add(TIfcEnergyConversionDevice);
    FIfcClasses.Add(TIfcBoiler);
    FIfcClasses.Add(TIfcFlowTerminal);
    FIfcClasses.Add(TIfcSanitaryTerminal);
    FIfcClasses.Add(TIfcAirTerminal);
    FIfcClasses.Add(TIfcReinforcingElement);
    FIfcClasses.Add(TIfcReinforcingBar);
    FIfcClasses.Add(TIfcReinforcingElementType);
    FIfcClasses.Add(TIfcReinforcingBarType);
    FIfcClasses.Add(TIfcTypeProduct);
    {$define read_add_classes}
    {$I castleifc_ifc_standard_types_autogenerated.inc}
    {$undef read_add_classes}
  end;

begin
  if FIfcClasses = nil then
    InitializeIfcClasses;
  Result := FIfcClasses;
end;

{$endif read_implementation}
