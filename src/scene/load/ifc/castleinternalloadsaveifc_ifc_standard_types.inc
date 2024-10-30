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

{ Types and classes that correspond to the IFC standard.

  Some conventions:

  - All IFC classes descend from TIfcPersistent.
    It right now descends from TComponent,
    all IFC classes will be owned by TIfcFile instance when read by IfcJsonLoad.

  - For all IFC fields:

    - Place all fields with F in "strict private" section.

    - Publish them as properties in "published" section.

    - Use "default" as usual in Pascal. When the property equals default,
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

  These classes express

  - Everything from the IFC standard that we handle.

  - Moreover, they contain methods to build/update nodes used
    by Castle Game Engine to display given IFC entities.
    These methods are implemented in castleinternalloadsaveifc_x3d.inc
    yet they are defined in this file, inside IFC classes.
    Havine these methods inside IFC classes is beneficial,
    as we can use IFC classes inheritance naturally (e.g. some "build
    nodes" methods extend ancestor in natural way).
}

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

  { Indication of date and time by measuring the number of seconds
    which have elapsed since 1 January 1970, 00:00:00 UTC.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTimeStamp.htm}
  TIfcTimeStamp = Int64;

  TIfcURIReference = String;

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

  { Geometric or topological representation items,
    that can (but not need to) have presentation style information assigned.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRepresentationItem.htm }
  TIfcRepresentationItem = class(TIfcPersistent)
  strict private
    FLayerAssignment: TIfcPresentationLayerAssignment;
    FStyledByItem: TIfcStyledItem;
  public
    { Build a node to display this using Castle Game Engine.
      Does nothing (adds nothing to Parent) if not possible / not implemented. }
    function BuildNode(const BaseUrl: String): TAbstractChildNode; virtual;
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
    function BuildNode(const BaseUrl: String): TAbstractChildNode;
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

    { TODO: Dim is not explicitly specified in IFC, but it is derived from
      the contents of IFC. Needs special logic, if ever needed in CGE. }
    //FDim: TIfcDimensionCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The number of dimensions specified in the IFC file, 2 or 3.
      Regardless of it, DirectionRatios always has the Z value defined:
      we set Z to 0 when it is not defined in IFC. }
    //TODO: property Dim: TIfcDimensionCount read FDim write FDim;
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

  { Curve in 2D or 3D.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurve.htm }
  TIfcCurve = class abstract(TIfcGeometricRepresentationItem)
    { Space dimensionality of this abstract class, defined differently for all
      subtypes. }
    // TODO: derived: function Dim: TIfcDimensionCount; abstract;
  end;

  TIfcBoundedCurve = class abstract(TIfcCurve)
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
    function BuildNode(const BaseUrl: String): TAbstractChildNode; override;
  published
    { Points defining the polyline. }
    property Points: TIfcCartesianPointList read FPoints;
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

  { Supertype of all definitions of standard and arbitrary profiles within IFC.
    It is used to define a standard set of commonly used section profiles
    by their parameters or by their explicit curve geometry.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProfileDef.htm }
  TIfcProfileDef = class(TIfcPersistent)
  published
  (*
TODO:
1 	ProfileType 	IfcProfileTypeEnum

Defines the type of geometry into which this profile definition shall be resolved, either a curve or a surface area. In case of curve the profile should be referenced by a swept surface, in case of area the profile should be referenced by a swept area solid.
2 	ProfileName 	OPTIONAL IfcLabel

Human-readable name of the profile, for example according to a standard profile table. As noted above, machine-readable standardized profile designations should be provided in IfcExternalReference.ItemReference.
	HasExternalReference 	SET [0:?] OF IfcExternalReferenceRelationship FOR RelatedResourceObjects

Reference to external information, e.g. library, classification, or document information, which is associated with the profile.
IFC4-CHANGE New inverse attribute
	HasProperties 	SET [0:?] OF IfcProfileProperties FOR ProfileDefinition

Additional properties of the profile, for example mechanical properties.
  *)
  end;

  { Closed profile IfcArbitraryClosedProfileDef defines an arbitrary
    two-dimensional profile for the use within the swept surface geometry,
    the swept area solid or a sectioned spine. It is given by an outer boundary
    from which the surface or solid can be constructed.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcArbitraryClosedProfileDef.htm }
  TIfcArbitraryClosedProfileDef = class(TIfcProfileDef)
  strict private
    FOuterCurve: TIfcCurve;
  published
    { Bounded curve, defining the outer boundaries of the arbitrary profile. }
    property OuterCurve: TIfcCurve read FOuterCurve write FOuterCurve;
  end;

  { Person acting on behalf of an organization.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPersonAndOrganization.htm }
  TIfcPersonAndOrganization = class(TIfcPersistent)
  strict private
    FThePerson: TIfcPerson;
    FTheOrganization: TIfcOrganization;
    //FRoles: TIfcActorRoleList; // TODO
  published
    { Person who is related to the organization. }
    property ThePerson: TIfcPerson read FThePerson write FThePerson;

    { Organization to which the person is related. }
    property TheOrganization: TIfcOrganization read FTheOrganization write FTheOrganization;

    { Roles played by the person within the context of an organization. }
    // property Roles: TIfcActorRoleList read FRoles; // TODO
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
    //FMiddleNames: TIfcLabelList;
    //FPrefixTitles: TIfcLabelList;
    //FSuffixTitles: TIfcLabelList;
    //FRoles: TIfcActorRoleList;
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
    //property MiddleNames: TIfcLabelList read FMiddleNames; // TODO serialization/deserialization for string list needed

    { The word, or group of words, which specify the person's social and/or professional standing and appear before his/her names. }
    //property PrefixTitles: TIfcLabelList read FPrefixTitles; // TODO serialization/deserialization for string list needed

    { The word, or group of words, which specify the person's social and/or professional standing and appear after his/her names.}
    //property SuffixTitles: TIfcLabelList read FSuffixTitles; // TODO serialization/deserialization for string list needed

    { Roles played by the person. }
    //property Roles: TIfcActorRoleList read FRoles; // TODO

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
    //FRoles: TIfcActorRoleList;
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
    //property Roles: TIfcActorRoleList read FRoles; // TODO

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

  { Selection of either a material definition or a material usage definition
    that can be assigned to an element, a resource or another entity within
    this specification.

    In IFC specification, this is a choice, not a class.
    But we make it a class, as it is more convenient for our purposes. }
  TIfcMaterialSelect = class(TIfcPersistent)
  end;

  { Objectified relationship between a material definition and elements
    or element types to which this material definition applies. }
  TIfcRelAssociatesMaterial = class(TIfcRelAssociates)
  strict private
    FRelatingMaterial: TIfcMaterialSelect;
  published
    { Material definition assigned to the elements or element types. }
    property RelatingMaterial: TIfcMaterialSelect read FRelatingMaterial write FRelatingMaterial;
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
    FIsDecomposedBy: TIfcRelAggregatesList;
    FHasContext: TIfcRelDeclares;
    // TODO: rest
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const BaseUrl: String): TTransformNode; virtual;
  published
    { References to the decomposition relationship being an aggregation. }
    property IsDecomposedBy: TIfcRelAggregatesList read FIsDecomposedBy;

    { References to the context providing context information such as project
      unit or representation context. It should only be asserted for
      the uppermost non-spatial object. }
    property HasContext: TIfcRelDeclares read FHasContext write FHasContext;
  end;
  TIfcObjectDefinitionList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcObjectDefinition>;

  { Any semantically treated thing or process. }
  TIfcObject = class abstract(TIfcObjectDefinition)
    // TODO
  end;
  TIfcObjectList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcObject>)
  end;

  { Information about a type, common to all occurrences of this type. }
  TIfcTypeObject = class(TIfcObjectDefinition)
  strict private
    FApplicableOccurrence: TIfcIdentifier;
    // TODO: FHasPropertySets: TIfcPropertySetDefinitionList;
    FTypes: TIfcRelDefinesByTypeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Data type of the occurrence object, to which the assigned type object can relate. }
    property ApplicableOccurrence: TIfcIdentifier read FApplicableOccurrence write FApplicableOccurrence;

    { Set of unique property sets, that are associated with the object type
      and are common to all object occurrences referring to this object type. }
    // TODO: property HasPropertySets: TIfcPropertySetDefinitionList read FHasPropertySets;

    { Reference to the relationship IfcRelDefinesByType and thus
      to those occurrence objects, which are defined by this type. }
    property Types: TIfcRelDefinesByTypeList read FTypes;
  end;

  { Type definition of a product without being already inserted
    into a project structure (without having a placement). }
  TIfcTypeProduct = class(TIfcTypeObject)
  strict private
    //FRepresentationMaps: TIfcRepresentationMapList;
    FTag: TIfcLabel;
    //FReferencedBy: TIfcRelAssignsToProductList;
  published
    { List of unique representation maps. Each representation map describes
      a block definition of the shape of the product style.
      By providing more than one representation map, a multi-view
      block definition can be given. }
 	  // TODO // property RepresentationMaps: TIfcRepresentationMapList read FRepresentationMaps;

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

  { Common properties of a certain type of built element that are applied
    to all occurrences of that type. }
  TIfcBuiltElementType = class(TIfcElementType)
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

  { Define the object coordinate system. }
  TIfcObjectPlacement = class abstract(TIfcPersistent)
  strict private
    FPlacementRelTo: TIfcObjectPlacement;
    FPlacesObject: TIfcProductList;
    FReferencedByPlacements: TIfcObjectPlacementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateNode(const Transform: TTransformNode); virtual;
  published
    { Reference to object placement that provides the relative placement with
      its placement in a grid, local coordinate system or linear referenced
      placement. }
    property PlacementRelTo: TIfcObjectPlacement read FPlacementRelTo write FPlacementRelTo;

    { The IfcObjectPlacement shall be used to provide a placement
      and an object coordinate system for instances of IfcProduct. }
    property PlacesObject: TIfcProductList read FPlacesObject;

    property ReferencedByPlacements: TIfcObjectPlacementList read FReferencedByPlacements;
  end;
  TIfcObjectPlacementList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcObjectPlacement>)
  end;

  { Abstract point.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPoint.htm }
  TIfcPoint = class abstract(TIfcGeometricRepresentationItem)
  (*TODO: Dim is not explicitly specified in IFC, but it is derived from
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
  published
    { Actual 3D point value. }
    property Coordinates: TCastleVector3PersistentSimple read FCoordinates;
  end;
  TIfcCartesianPointList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcCartesianPoint>)
  end;

  { Abstract supertype of placement subtypes that define the location of an item,
    or an entire shape representation, and provide its orientation.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlacement.htm }
  TIfcPlacement = class abstract(TIfcRepresentationItem)
  strict private
    FLocation: TIfcPoint;
  public
    procedure UpdateNode(const Transform: TTransformNode); virtual;
  published
    property Location: TIfcPoint read FLocation write FLocation;

    // TODO: We could define this to be super-consistent with IFC standard:
    // function Dim: TIfcDimensionCount; begin Result := Location.Dim; end;
  end;

  { Location and orientation to place items in a two-dimensional space.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2Placement2D.htm }
  TIfcAxis2Placement2D = class(TIfcPlacement)
  strict private
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
  strict private
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
    function BuildNode(const BaseUrl: String): TAbstractChildNode; override;
  published
    { The direction in which the surface, provided by SweptArea is to be swept.
      The ExtrudedDirection shall not be perpendicular to the local z-axis. }
    property ExtrudedDirection: TIfcDirection read FExtrudedDirection write FExtrudedDirection;

    { The distance the surface is to be swept along the ExtrudedDirection. }
    property Depth: TIfcPositiveLengthMeasure read FDepth write FDepth {$ifdef FPC}default 0.0{$endif};
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
  strict private
    FRelativePlacement: TIfcAxis2Placement;
  public
    procedure UpdateNode(const Transform: TTransformNode); override;
  published
    property RelativePlacement: TIfcAxis2Placement read FRelativePlacement write FRelativePlacement;
  end;

  { Any object that relates to a geometric or spatial context.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProduct.htm }
  TIfcProduct = class abstract(TIfcObject)
  strict private
    FObjectPlacement: TIfcObjectPlacement;
    FRepresentation: TIfcProductRepresentation;
  public
    function BuildNode(const BaseUrl: String): TTransformNode; override;
  published
    { Object coordinate system and placement of the product in space. }
    property ObjectPlacement: TIfcObjectPlacement read FObjectPlacement write FObjectPlacement;

    { Representations of the product. }
    property Representation: TIfcProductRepresentation read FRepresentation write FRepresentation;

    // TODO // property ReferencedBy: TIfcRelAssignsToProductList read FReferencedBy;

    // TODO // property PositionedRelativeTo: TIfcRelPositionsList read FPositionedRelativeTo;

    // TODO // property ReferencedInStructures: TIfcRelReferencedInSpatialStructure read FReferencedInStructures;
  end;
  TIfcProductList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcProduct>)
  end;

  { Elements that might be used to define a spatial structure.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialElement.htm }
  TIfcSpatialElement = class abstract(TIfcProduct)
  strict private
    FLongName: TIfcLabel;
    // TODO: rest of fields
  published
    property LongName: TIfcLabel read FLongName write FLongName;
  end;

  { Relationship that connects objects under some criteria. }
  TIfcRelConnects = class abstract(TIfcRelationship)
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
    function BuildNode(const BaseUrl: String): TAbstractChildNode;
  published
    { Set of products, which are contained within this level of
      the spatial structure hierarchy. }
    property RelatedElements: TIfcProductList read FRelatedElements;

    { Spatial structure element, within which the element is contained. }
    property RelatingStructure: TIfcSpatialElement read FRelatingStructure write FRelatingStructure;
  end;
  TIfcRelContainedInSpatialStructureList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelContainedInSpatialStructure>;

  { Spatial structure can be used to provide a spatial organization of a project. }
  TIfcSpatialStructureElement = class abstract(TIfcSpatialElement)
  strict private
    // TODO
    FContainsElements: TIfcRelContainedInSpatialStructureList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const BaseUrl: String): TTransformNode; override;
  published
    property ContainsElements: TIfcRelContainedInSpatialStructureList read FContainsElements;
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
    // TODO: FAddressLines: TIfcLabelList;
    FPostalBox: TIfcLabel;
    FTown: TIfcLabel;
    FRegion: TIfcLabel;
    FPostalCode: TIfcLabel;
    FCountry: TIfcLabel;
  published
    { Internal location for the address. }
    property InternalLocation: TIfcLabel read FInternalLocation write FInternalLocation;

    { The postal address. }
    // property AddressLines: TIfcLabelList read FAddressLines; // TODO serialization/deserialization for string list needed

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

  { Generalization of all components that make up a facility. }
  TIfcElement = class abstract(TIfcProduct)
  strict private
    FTag: TIfcIdentifier;
    // TODO
    // FFillsVoids: TIfcRelFillsElement;
    // FConnectedTo: TIfcRelConnectsElementsList;
    // FIsInterferedByElements: TIfcRelInterferesElementsList;
    // FInterferesElements: TIfcRelInterferesElementsList;
    // FHasProjections: TIfcRelProjectsElementList;
    FHasOpenings: TIfcRelVoidsElementList;
    // TODO
    // FIsConnectionRealization: TIfcRelConnectsWithRealizingElementsList;
    // FProvidesBoundaries: TIfcRelSpaceBoundaryList;
    // FConnectedFrom: TIfcRelConnectsElementsList;
    // FContainedInStructure: TIfcRelContainedInSpatialStructure;
    // TODO
    // FHasCoverings: TIfcRelCoversBldgElementsList;
    // FHasSurfaceFeatures: TIfcRelAdheresToElementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The tag (or label) identifier at the particular instance of a product,
      e.g. the serial number, or the position number.
      Note: This obscures TComponent.Tag property. }
    property Tag: TIfcIdentifier read FTag write FTag;

    // TODO
    // { Reference to the IfcRelFillsElement relationship that puts the element as a filling into the opening created within another element. }
    // property FillsVoids: TIfcRelFillsElement read FFillsVoids write FFillsVoids;

    // { Reference to the element connection relationship. }
	  // property ConnectedTo: TIfcRelConnectsElementsList read FConnectedTo;

    // { Reference to the interference relationship to indicate the element that is interfered. }
	  // property IsInterferedByElements: TIfcRelInterferesElementsList read FIsInterferedByElements;

    // { Reference to the interference relationship to indicate the element that interferes. }
	  // property InterferesElements: TIfcRelInterferesElementsList read FInterferesElements;

    // { Projection relationship that adds a feature (using a Boolean union) to the IfcBuiltElement. }
	  // property HasProjections: TIfcRelProjectsElementList read FHasProjections;

    { List of openings associated with this element. }
	  property HasOpenings: TIfcRelVoidsElementList read FHasOpenings;

    // TODO
    // { Reference to the connection relationship with realizing element. }
    // property IsConnectionRealization: TIfcRelConnectsWithRealizingElementsList read FIsConnectionRealization;

    // { Space boundaries by virtue of the objectified relationship IfcRelSpaceBoundary. }
    // property ProvidesBoundaries: TIfcRelSpaceBoundaryList read FProvidesBoundaries;

    // { Element connection relationship. }
    // property ConnectedFrom: TIfcRelConnectsElementsList read FConnectedFrom;

    { Containment relationship to the spatial structure element, to which the element is primarily associated. }
    // TODO: should be list according to one testcase?
    // TODO property ContainedInStructure: TIfcRelContainedInSpatialStructure read FContainedInStructure write FContainedInStructure;

    // TODO
    // { Reference to IfcCovering by virtue of the objectified relationship IfcRelCoversBldgElements. }
    // property HasCoverings: TIfcRelCoversBldgElementsList read FHasCoverings;

    // { Reference to the IfcRelAdheresToElement relationship that adheres a IfcSurfaceFeature to an element. }
    // property HasSurfaceFeatures: TIfcRelAdheresToElementList read FHasSurfaceFeatures;
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
  TIfcRelFillsElementList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcRelFillsElement>;

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
  TIfcRelVoidsElement = class(TIfcPersistent)
  strict private
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
  TIfcRelAggregates = class(TIfcRelDecomposes)
  strict private
    FRelatingObject: TIfcObjectDefinition;
    FRelatedObjects: TIfcObjectDefinitionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const BaseUrl: String): TAbstractChildNode;
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

  TIfcMeasureValue = class(TIfcValue)
  end;

  { Value of an angle in a plane.

    Usually measured in radian (rad, m/m = 1), but also grads may be used.
    The grad unit has to be declared as a conversion based unit based on
    radian unit. }
  TIfcPlaneAngleMeasure = class(TIfcMeasureValue)
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
    // TODO: FElements: TIfcDerivedUnitElementList;
    FUnitType: TIfcDerivedUnitEnum;
    FUserDefinedType: TIfcLabel;
    FName: TIfcLabel;
  published
    { The group of units and their exponents that define the derived unit. }
    // TODO: property Elements: TIfcDerivedUnitElementList read FElements;

    { Type of the derived unit chosen from an enumeration of derived unit types
      for use in IFC models. }
    property UnitType: TIfcDerivedUnitEnum read FUnitType write FUnitType;

    { Type of the derived unit if the UnitType attribute is set to UserDefined. }
    property UserDefinedType: TIfcLabel read FUserDefinedType write FUserDefinedType;

    { Name of the unit in addition to the unit type, particularly when the derived
      unit elements refer to conversion or context based units. }
    property Name: TIfcLabel read FName write FName;

    { Dimensional exponents derived. }
    // TODO: function Dimensions: TIfcDimensionalExponents;
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

    // TODO: We could calculate this:
    // Dimensions: TIfcDimensionalExponents
    // This attribute is formally derived.
  end;

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

  { A unit that has a conversion rate to a base unit. }
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
    // TODO // FHasSubContexts: TIfcGeometricRepresentationSubContextList;
    // TODO // FHasCoordinateOperation: TIfcCoordinateOperation;
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
    // TODO: property HasSubContexts: TIfcGeometricRepresentationSubContextList read FHasSubContexts;

    { Indicates conversion between coordinate systems. }
    // TODO: property HasCoordinateOperation: TIfcCoordinateOperation read FHasCoordinateOperation write FHasCoordinateOperation;
  end;

  TIfcRelDefines = class(TIfcRelationship)
  end;

  { The objectified relationship IfcRelDefinesByProperties defines
    the relationships between property set definitions and objects.

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelDefinesByProperties.htm }
  TIfcRelDefinesByProperties = class(TIfcRelDefines)
  strict private
    FRelatedObjects: TIfcObjectDefinitionList;
    // TODO // FRelatingPropertyDefinition: TIfcPropertySetDefinitionSelect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Reference to the objects (or single object) to which
      the property definition applies. }
    property RelatedObjects: TIfcObjectDefinitionList read FRelatedObjects;

    { Reference to the property set definition for that object
      or set of objects. }
    // TODO // property RelatingPropertyDefinition: TIfcPropertySetDefinitionSelect read FRelatingPropertyDefinition write FRelatingPropertyDefinition;
  end;
  TIfcRelDefinesByPropertiesList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRelDefinesByProperties>)
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
    function BuildNode(const BaseUrl: String): TTransformNode; override;
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

    Spec: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm }
  TIfcProject = class(TIfcContext)
  end;

  { How to display the product. }
  TIfcRepresentation = class abstract(TIfcPersistent)
  strict private
    FItems: TIfcRepresentationItemList;
    // TODO rest
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Set of geometric representation items that are defined for this representation. }
    property Items: TIfcRepresentationItemList read FItems;
  end;

  { List of TIfcRepresentation. }
  TIfcRepresentationList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcRepresentation>)
  public
    function BuildNode(const BaseUrl: String): TAbstractChildNode;
  end;

  TIfcShapeModel = class abstract(TIfcRepresentation)
    // TODO
  end;
  TIfcShapeModelList = class({$ifdef FPC}specialize{$endif} TObjectList<TIfcShapeModel>)
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
  strict private
    FName: TIfcLabel;
    FDescription: TIfcText;
    FRepresentations: TIfcRepresentationList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildNode(const BaseUrl: String): TAbstractChildNode;
  published
    { The word or group of words by which the product representation is known. }
    property Name: TIfcLabel read FName write FName;

    { The word or group of words that characterize the product representation. }
    property Description: TIfcText read FDescription write FDescription;

    { Contained list of representations (including shape representations). }
    property Representations: TIfcRepresentationList read FRepresentations;
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

var
  { List of all (non-abstract) IFC classes that are defined in this unit.

    TODO: We didn't strictly check if all classes on this list are non-abstract.
    In practice, it doesn't matter for current code. }
  IfcClasses: TIfcPersistentClassList;

{ Make sure IfcClasses is assigned and filled with contents. }
procedure InitializeIfcClasses;

{ Free and nil IfcClasses. }
procedure FinalizeIfcClasses;

{$endif read_interface}

{$ifdef read_implementation}

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
  FIsDecomposedBy := TIfcRelAggregatesList.Create(false);
end;

destructor TIfcObjectDefinition.Destroy;
begin
  FreeAndNil(FIsDecomposedBy);
  inherited;
end;

{ TIfcSpatialStructureElement ------------------------------------------------ }

constructor TIfcSpatialStructureElement.Create(AOwner: TComponent);
begin
  inherited;
  FContainsElements := TIfcRelContainedInSpatialStructureList.Create(false);
end;

destructor TIfcSpatialStructureElement.Destroy;
begin
  FreeAndNil(FContainsElements);
  inherited;
end;

{ TIfcElement --------------------------------------------------------------- }

constructor TIfcElement.Create(AOwner: TComponent);
begin
  inherited;
  // TODO
  // FConnectedTo := TIfcRelConnectsElementsList.Create(false);
  // FIsInterferedByElements := TIfcRelInterferesElementsList.Create(false);
  // FInterferesElements := TIfcRelInterferesElementsList.Create(false);
  // FHasProjections := TIfcRelProjectsElementList.Create(false);
  FHasOpenings := TIfcRelVoidsElementList.Create(false);
  // TODO
  // FIsConnectionRealization := TIfcRelConnectsWithRealizingElementsList.Create(false);
  // FProvidesBoundaries := TIfcRelSpaceBoundaryList.Create(false);
  // FConnectedFrom := TIfcRelConnectsElementsList.Create(false);
  // FHasCoverings := TIfcRelCoversBldgElementsList.Create(false);
  // FHasSurfaceFeatures := TIfcRelAdheresToElementList.Create(false);
end;

destructor TIfcElement.Destroy;
begin
  // FreeAndNil(FConnectedTo);
  // FreeAndNil(FIsInterferedByElements);
  // FreeAndNil(FInterferesElements);
  // FreeAndNil(FHasProjections);
  FreeAndNil(FHasOpenings);
  // FreeAndNil(FIsConnectionRealization);
  // FreeAndNil(FProvidesBoundaries);
  // FreeAndNil(FConnectedFrom);
  // FreeAndNil(FHasCoverings);
  // FreeAndNil(FHasSurfaceFeatures);
  inherited;
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

{ TIfcRepresentation -------------------------------------------------------- }

constructor TIfcRepresentation.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TIfcRepresentationItemList.Create(false);
end;

destructor TIfcRepresentation.Destroy;
begin
  FreeAndNil(FItems);
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

{ TIfcRelContainedInSpatialStructure --------------------------------------- }

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

{ TIfcCartesianPoint -------------------------------------------------------- }

constructor TIfcCartesianPoint.Create(AOwner: TComponent);
begin
  inherited;
  FCoordinates := TCastleVector3PersistentSimple.Create;
end;

destructor TIfcCartesianPoint.Destroy;
begin
  FreeAndNil(FCoordinates);
  inherited;
end;

{ TIfcDirection ------------------------------------------------------------- }

constructor TIfcDirection.Create(AOwner: TComponent);
begin
  inherited;
  FDirectionRatios := TCastleVector3PersistentSimple.Create;
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

{ TIfcPerson --------------------------------------------------------------- }

constructor TIfcPerson.Create(AOwner: TComponent);
begin
  inherited;
  FAddresses := TIfcAddressList.Create(false);
  FEngagedIn := TIfcPersonAndOrganizationList.Create(false);
end;

destructor TIfcPerson.Destroy;
begin
  FreeAndNil(FAddresses);
  FreeAndNil(FEngagedIn);
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
end;

destructor TIfcOrganization.Destroy;
begin
  FreeAndNil(FAddresses);
  FreeAndNil(FIsRelatedBy);
  FreeAndNil(FRelates);
  FreeAndNil(FEngages);
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
end;

destructor TIfcRelDefinesByProperties.Destroy;
begin
  FreeAndNil(FRelatedObjects);
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

{ TIfcTypeObject ------------------------------------------------------------- }

constructor TIfcTypeObject.Create(AOwner: TComponent);
begin
  inherited;
  // TODO FHasPropertySets := TIfcRelDefinesByPropertiesList.Create(false);
  FTypes := TIfcRelDefinesByTypeList.Create(false);
end;

destructor TIfcTypeObject.Destroy;
begin
  // TODO FreeAndNil(FHasPropertySets);
  FreeAndNil(FTypes);
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
    IfcClasses.Add(TIfcStyledItem);
    IfcClasses.Add(TIfcPresentationLayerAssignment);
    IfcClasses.Add(TIfcPersonAndOrganization);
    IfcClasses.Add(TIfcAddress);
    IfcClasses.Add(TIfcPerson);
    IfcClasses.Add(TIfcOrganization);
    IfcClasses.Add(TIfcOrganizationRelationship);
    IfcClasses.Add(TIfcOwnerHistory);
    IfcClasses.Add(TIfcApplication);
    IfcClasses.Add(TIfcWall);
    IfcClasses.Add(TIfcWallStandardCase);
    IfcClasses.Add(TIfcPolyline);
    IfcClasses.Add(TIfcPropertyDefinition);
    IfcClasses.Add(TIfcRelDeclares);
    IfcClasses.Add(TIfcPostalAddress);
    IfcClasses.Add(TIfcCompoundPlaneAngleMeasure);
    IfcClasses.Add(TIfcDefinitionSelect);
    IfcClasses.Add(TIfcDimensionalExponents);
    IfcClasses.Add(TIfcMonetaryUnit);
    IfcClasses.Add(TIfcDerivedUnit);
    IfcClasses.Add(TIfcPlaneAngleMeasure);
    IfcClasses.Add(TIfcRepresentationContext);
    IfcClasses.Add(TIfcUnitAssignment);
    IfcClasses.Add(TIfcRelDefinesByProperties);
    IfcClasses.Add(TIfcSIUnit);
    IfcClasses.Add(TIfcGeometricRepresentationContext);
    IfcClasses.Add(TIfcConversionBasedUnit);
    IfcClasses.Add(TIfcMeasureWithUnit);
    IfcClasses.Add(TIfcProjectLibrary);
    IfcClasses.Add(TIfcProductDefinitionShape);
    IfcClasses.Add(TIfcRelAssociatesMaterial);
    IfcClasses.Add(TIfcRelContainedInSpatialStructure);
    IfcClasses.Add(TIfcRelDefinesByType);
    IfcClasses.Add(TIfcTypeObject);
    IfcClasses.Add(TIfcWindowType);
    IfcClasses.Add(TIfcShapeAspect);
    IfcClasses.Add(TIfcExternalReference);
    IfcClasses.Add(TIfcExternalReferenceRelationship);
    IfcClasses.Add(TIfcArbitraryClosedProfileDef);
  end;
end;

procedure FinalizeIfcClasses;
begin
  FreeAndNil(IfcClasses);
end;

{$endif read_implementation}
