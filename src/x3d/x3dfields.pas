{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D fields (TX3DField and many descendants). }
unit X3DFields;

{$I castleconf.inc}

interface

uses Classes, SysUtils, DOM, FGL,
  CastleVectors, X3DLexer, CastleUtils, CastleClassUtils,
  CastleImages, CastleStringUtils, CastleInterfaces,
  X3DTime, CastleGenericLists, CastleColors, CastleQuaternions;

{$define read_interface}

const
  DefaultRotation: TVector4Single = (0, 0, 1, 0);

type
  EX3DFieldAssign = class(Exception);
  EX3DFieldAssignInvalidClass = class(EX3DFieldAssign);
  { Raised by various X3D methods searching for X3D items (nodes, fields,
    events and such) when given item cannot be found. }
  EX3DNotFound = class(Exception);

  TX3DEvent = class;

  { Writer of VRML/X3D to stream. }
  TX3DWriter = {abstact} class
  private
    Indent: string;
    DoDiscardNextIndent: boolean;
    FEncoding: TX3DEncoding;
    FStream: TStream;
  public
    { Which VRML/X3D version are we writing. Read-only after creation. }
    Version: TX3DVersion;

    constructor Create(AStream: TStream;
      const AVersion: TX3DVersion; const AEncoding: TX3DEncoding);
    destructor Destroy; override;

    property Encoding: TX3DEncoding read FEncoding;

    procedure IncIndent;
    procedure DecIndent;

    { Comfortable routines that simply write given string to a stream.
      @groupBegin }
    procedure Write(const S: string);
    procedure Writeln(const S: string); overload;
    procedure Writeln; overload;
    procedure WriteIndent(const S: string);
    procedure WritelnIndent(const S: string);
    { @groupEnd }

    { Causes next WriteIndent or WritelnIndent too not write the Indent.
      Useful in some cases to improve readability of generated VRML file. }
    procedure DiscardNextIndent;
  end;

  { Reading of VRML/X3D from stream.
    Common knowledge for both classic and XML reader.
    X3DNodes unit extends this into TX3DReaderNames. }
  TX3DReader = class
  private
    FVersion: TX3DVersion;
    FBaseUrl: string;
    AngleConversionFactor: Float;
  public
    LengthConversionFactor: Float;

    constructor Create(const ABaseUrl: string;
      const AVersion: TX3DVersion);
    constructor CreateCopy(Source: TX3DReader);

    { Base path for resolving URLs from nodes in this namespace.
      See TX3DNode.BaseUrl. }
    property BaseUrl: string read FBaseUrl;

    { VRML/X3D version number. For resolving node class names and other stuff. }
    property Version: TX3DVersion read FVersion;

    { Apply unit conversion.
      If this is angle conversion factor, it is stored and used internally.
      If this is length conversion factor, we update our
      LengthConversionFactor property, but it's callers responsibility
      to make use of it. (You want to use here TX3DRootNode.Scale.) }
    procedure UnitConversion(const Category, Name: string;
      const ConversionFactor: Float);
  end;

  TSaveToXmlMethod = (sxNone, sxAttribute, sxAttributeCustomQuotes, sxChildElement);

  { Possible things that happen when given field is changed.
    Used by TX3DField.ExecuteChanges. }
  TX3DChange = (
    { Something visible in the geometry changed.
      See vcVisibleGeometry.
      This means that VisibleChangeHere with vcVisibleGeometry included should
      be called. }
    chVisibleGeometry,

    { Something visible changed, but not geometry.
      See vcVisibleNonGeometry.
      This means that VisibleChangeHere with vcVisibleNonGeometry included should
      be called. }
    chVisibleNonGeometry,

    { Call VisibleChangeHere to redisplay the scene.

      If you include one of the chVisibleGeometry or chVisibleNonGeometry
      then this flag (chRedisplay) makes no effect.
      Otherwise, this flag should be used if your change requires
      redisplay of the 3D view for some other reason. }
    chRedisplay,

    { Transformation of children of this node changed.

      Caller will analyze the scene (your children) to know what this implicates,
      don't include other flags with this. }
    chTransform,

    { Coordinate (both VRML 1.0 and >= 2.0) node "point" field changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chCoordinate,

    { Something visible in VRML 1.0 state node (that may be present
      in TX3DGraphTraverseState.LastNodes) changed, but not geometry.
      Excluding Coordinate node change (this one should go through chCoordinate
      only).

      This is allowed, and ignored, on nodes that are not part of VRML 1.0
      state. (This is useful for alphaChannel field, that is declared
      in TAbstractGeometryNode, and so is part of some VRML 1.0 state nodes
      but is also part of VRML >= 2.0 nodes.)

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can (and should) include chUseBlending and
      chTextureImage for appropriate changes. }
    chVisibleVRML1State,

    { Some visible geometry changed because of VRML 1.0 state node change.
      This is for VRML 1.0 state node changes, excluding non-geometry changes
      (these go to chVisibleVRML1State) and Coordinate changes (these go to
      chCoordinate).

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeometryVRML1State,

    { Something visible in VRML >= 2.0 Material (or TwoSidedMaterial) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can (and should) include chUseBlending for appropriate
      Material changes. }
    chMaterial2,

    { Something that may affect UseBlending calculation possibly changed.
      This is guaranteed to work only when used together with
      chVisibleVRML1State and chMaterial2. It's understood that only
      shapes that use given material need UseBlending recalculated. }
    chUseBlending,

    { Light property that is also reflected in TLightInstance structure.
      Only allowed on node's descending from TAbstractLightNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: include also chLightLocationDirection when appropriate. }
    chLightInstanceProperty,

    { Light's location and/or direction changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: include also chLightInstanceProperty when appropriate. }
    chLightLocationDirection,

    { TCastleSceneCore.MainLightForShadows possibly changed because of this change.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chLightForShadowVolumes,

    { Switch.whichChoice changed, for VRML >= 2.0.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chSwitch2,

    { X3DColorNode colors changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chColorNode,

    { X3DTextureCoordinateNode coords changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTextureCoordinate,

    { VRML >= 2.0 TextureTransform changed.
      Not for multi-texture node changes, only the simple nodes changes.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTextureTransform,

    { Geometry node visible (or collidable) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeometry,

    { X3DEnvironmentalSensorNode bounds (size/center) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chEnvironmentalSensorBounds,

    { TimeDependent node is start/stop/pause/resume time changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTimeStopStart,

    { Viewpoint vectors (position, direction, up, gravity up) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chViewpointVectors,

    { Viewpoint projection changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chViewpointProjection,

    { Texture image (data) needs reloading (url or source SFImage
      data changed). This is for TAbstractTexture2DNode, or TAbstractTexture3DNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can mix it with chVisibleVRML1State or
      chTextureRendererProperties. }
    chTextureImage,

    { Texture properties used by the renderer changed (something other than
      only the texture data). This is for fields contained in X3DTextureNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can mix it with chTextureImage. }
    chTextureRendererProperties,

    { Texture properties inside TextureProperties node changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTexturePropertiesNode,

    { What is considered a shadow caster changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chShadowCasters,

    { Mark the generated texture node (parent of this field) as requiring update
      (assuming it's "update" field value wants it too).

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeneratedTextureUpdateNeeded,

    { VRML >= 2.0 FontStyle changed.
      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chFontStyle,

    { HeadLight on status changed.
      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chHeadLightOn,

    { Clip plane visible change (enabled or plane equation). }
    chClipPlane,

    { Enabled field of the pointing-device drag sensor changed.
      Use only for TSFBool fields within TAbstractDragSensorNode. }
    chDragSensorEnabled,

    { NavigationInfo field value used in TCastleSceneCore.CameraFromNavigationInfo
      changed. }
    chNavigationInfo,

    { ScreenEffect.enabled changed }
    chScreenEffectEnabled,

    { X3DBackgroundNode properties that are stored inside TBackground
      display list have changed. }
    chBackground,

    { Everything changed and needs to be recalculated.
      This is needed for changes on stuff internally cached in
      TCastleSceneCore, TCastleScene, TShape that cannot be expressed
      as one of above flags.

      Use only as a last resort, as this is very costly!
      (And in an ideal implementation, should not be needed.)

      Don't include other flags with this. }
    chEverything,

    { Higher-level shadow maps fields changed.
      They have to be processed to lower-level fields by calling
      TCastleSceneCore.ProcessShadowMapsReceivers.

      Don't include other flags with this. }
    chShadowMaps,

    { Shading changed from wireframe to non-wireframe. }
    chWireframe);
  TX3DChanges = set of TX3DChange;

{ ---------------------------------------------------------------------------- }
{ @section(Base fields classes) }

  { Base class for any item within VRML/X3D file: a node, a field, a route,
    a prototype etc. We need a common base class for all such things
    to store PositionInParent.

    About ancestry: TX3DFieldOrEvent make use of Assign mechanism
    and so need to descend from TPersistent. TX3DNode make use
    of interfaces and so must descend from something like
    TNonRefCountedInterfacedXxx. These are the only reasons, for now,
    why this descends from TNonRefCountedInterfacedPersistent. }
  TX3DFileItem = class(TNonRefCountedInterfacedPersistent)
  private
    FPositionInParent: Integer;

    { Secondary order for saving items to VRML/X3D file.
      When PositionInParent are equal, this decides which item is first.
      It may be useful, since SortPositionInParent is not a stable sort
      (because QuickSort is not stable), so using this to preserve order
      may be helpful.

      TX3DFileItemList.Add sets this, which allows to preserve
      order when saving. }
    PositionOnList: Integer;
  public
    constructor Create;

    { Position of this item within parent VRML/X3D node, used for saving
      the VRML/X3D graph to file. Default value -1 means "undefined".

      For normal usage and processing of VRML graph, this is totally not needed.
      This position doesn't dictate actual meaning of VRML graph.
      If you're looking to change order of nodes, you probably want
      to rather look at something like ReplaceItems within TMFNode or such.

      This field is purely a hint when encoding VRML file how to order
      VRML items (nodes, fields, routes, protos) within parent node
      or the VRML file. Reason: VRML allows non-unique node names.
      Each DEF XXX overrides all previous ("previous" in lexical sense,
      i.e. normal order of tokens in the file) DEF XXX with the same XXX,
      thus hiding previous node name "XXX".
      This means that when saving VRML file we have to be very careful
      about the order of items, such that e.g. all routes are specified
      when appropriate node names are bound.

      This is a relative position, relative to other PositionInParent
      value of other TX3DFileItem items. So it's not necessary
      to keep all PositionInParent different or successive within some
      parent. When saving, we will sort everything according to
      PositionInParent.

      See e.g. ../../../demo_models/x3d/tricky_def_use.x3dv
      for tests of some tricky layout. When reading such file we have
      to record PositionInParent to be able to save such file correctly. }
    property PositionInParent: Integer
      read FPositionInParent write FPositionInParent default -1;

    { Save to stream. }
    procedure SaveToStream(Writer: TX3DWriter); virtual; abstract;

    { How is this saved to X3D XML encoding. This determines when
      SaveToStream is called. It also cooperates with some SaveToStream
      implementations, guiding how the item is actually saved.
      By default it is sxChildElement. }
    function SaveToXml: TSaveToXmlMethod; virtual;
  end;

  TX3DFileItemList = class(specialize TFPGObjectList<TX3DFileItem>)
  public
    procedure SortPositionInParent;
    { Sort all items by PositionInParent and then save them all to stream. }
    procedure SaveToStream(Writer: TX3DWriter);
    procedure Add(Item: TX3DFileItem);
  end;

  { Base class for VRML/X3D field or event. }
  TX3DFieldOrEvent = class(TX3DFileItem)
  private
    { To optimize memory usage (which may really be huge in case of VRML/X3D
      with many nodes, esp. when VRML/X3D uses a lot of prototypes),
      this is only created when needed (when it is not empty).
      Otherwise it is @nil. }
    FIsClauseNames: TCastleStringList;

    FX3DName: string;

    { A really simple (but good enough for now) implementation of
      AddAlternativeName:
      - there are only 0 (none), 1, 2, and 3 VRML major versions
      - each VRML major version has exactly one alt name
      - alt name is never '' ('' means that alt name doesn't exist) }
    FAlternativeNames: array [0..3] of string;

    FParentNode: TX3DFileItem;
    FParentInterfaceDeclaration: TX3DFileItem;

    function GetIsClauseNames(const Index: Integer): string;

    { Return FIsClauseNames, initializing it if necessary to not be nil. }
    function IsClauseNamesCreate: TCastleStringList;
  protected
    procedure FieldOrEventAssignCommon(Source: TX3DFieldOrEvent);
  public
    constructor Create(AParentNode: TX3DFileItem; const AX3DName: string);
    destructor Destroy; override;

    { Name of the field or event.

      Most fields/events are inside some X3D node, and then
      they have a non-empty name. But in some special cases we
      also use temporary fields with an empty name.

      Note that you cannot change this after object creation, since
      Name is used for various purposes (like to generate names for
      TX3DField.ExposedEvents).

      Note that this property is deliberately not called @code(Name).
      In the future, this class may descend from the standard TComponent
      class, that defines a @code(Name) field with a special restrictions
      (it must be a valid Pascal identifier), which cannot apply to X3D node names
      (that can have quite free names, see
      http://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/grammar.html ).
      We don't want to confuse these two properties. }
    property X3DName: string read FX3DName;
    property Name: string read FX3DName; deprecated 'use X3DName';

    { VRML node containing this field/event.
      This must always contain an instance
      of TX3DNode class (although it cannot be declared such, since X3DFields
      unit cannot depend on X3DNodes interface).

      It may be @nil for special fields/events when parent node is unknown. }
    property ParentNode: TX3DFileItem read FParentNode;

    { "IS" clauses of this field/event, used when this field/event
      is inside prototype definition.

      This is an array, as one item may have many "IS" clauses (for a field,
      only one "IS" clause should refer to another field;
      but you can have many "IS" clauses connecting events,
      also exposedField may have "IS" clause that should be interpreted
      actually as links to it's exposed events).
      See e.g. @code(demo_models/x3d/proto_events_test_3.x3dv).

      Note that having "IS" clauses doesn't mean that the field should
      be considered "without any value". This is not a good way of thinking,
      as an exposed field may have an "IS" clause, but linking it to an event,
      and thus such field has it's value (default value, if not specified
      in the file), event though it also has an "IS" clause.
      Although there is TX3DField.ValueFromIsClause, which indicates
      whether current value was obtained from "IS" clause.

      To be able to significantly optimize memory, we do not expose IsClauseNames
      as TCastleStringList. Instead operate on them only using below functions.
      Note that IsClauseNamesAssign can also accept @nil as parameter.

      @groupBegin }
    property IsClauseNames[const Index: Integer]: string read GetIsClauseNames;
    function IsClauseNamesCount: Integer;
    procedure IsClauseNamesAssign(const SourceIsClauseNames: TCastleStringList);
    procedure IsClauseNamesAdd(const S: string);
    { @groupEnd }

    { Parse only "IS" clause, if it's not present --- don't do nothing.
      For example, for the TX3DField descendant, this does not try to parse
      field value. }
    procedure ParseIsClause(Lexer: TX3DLexer);

    { Add alternative name for the same field/event, to be used in different
      VRML version.

      When VRML major version is exactly equal VrmlMajorVersion,
      the AlternativeName should be used --- for both reading and writing
      of this field/event. In some cases, when reading, we may also allow
      all versions (both original and alternative), but this is mostly
      for implementation simplicity --- don't count on it.

      A special value 0 for VrmlMajorVersion means that this is just
      an alternative name, that should be allowed when reading (as alternative
      to normal Name), and never used when writing.

      Alternative names is a very handy mechanism for cases when
      the only thing that changed between VRML versions is the field
      name. Example: Switch node's children/choice, LOD node's children/level,
      Polyline2D lineSegments/point.

      Note that this also works for ExposedEvents with exposed TX3DField:
      if a field has alternative names, then it's exposed events always also
      have appropriate alternative names. }
    procedure AddAlternativeName(const AlternativeName: string;
      VrmlMajorVersion: Integer); virtual;

    { Returns if S matches current Name or one of the alternative names.
      Think about it like simple test "Name = S", but actually this
      checks also names added by AddAlternativeName method. }
    function IsName(const S: string): boolean;

    { Return how this field should be named for given VRML version.
      In almost all cases, this simply returns current Name.
      But it can also return a name added by AddAlternativeName method. }
    function NameForVersion(Version: TX3DVersion): string; overload;
    function NameForVersion(Writer: TX3DWriter): string; overload;

    { For fields contained in TX3DInterfaceDeclaration.

      This should always be @nil (if the field is normal, standard field,
      not coming from interface declaration in VRML file) or an instance of
      TX3DInterfaceDeclaration. (But it cannot be declared such,
      since TX3DInterfaceDeclaration is not known in this unit). }
    property ParentInterfaceDeclaration: TX3DFileItem
      read FParentInterfaceDeclaration write FParentInterfaceDeclaration;

    { Nice and concise field description for user.
      Describes parent node type, name and field/event's name. }
    function NiceName: string;

    { Save IS clauses to stream, only for classic encoding.
      For each IS clause, writeln field/event name followed by "IS" clause. }
    procedure SaveToStreamClassicIsClauses(Writer: TX3DWriter);
  end;

  TX3DFieldOrEventList = specialize TFPGObjectList<TX3DFieldOrEvent>;
  TX3DEventReceiveList = class;
  TX3DFieldClass = class of TX3DField;

  { Base class for all VRML/X3D fields.

    Common notes for all descendants: most of them expose field or property
    "Value", this is (surprise, surprise!) the value of the field.
    Many of them also expose DefaultValue and DefaultValueExists
    fields/properties, these should be the default VRML value for this field.
    You can even change DefaultValue after the object is created.

    Most of descendants include constructor that initializes
    both DefaultValue and Value to the same thing, as this is what
    you usually want.

    Some notes about @code(Assign) method (inherited from TPersistent and
    overridied appropriately in TX3DField descendants):

    @orderedList(
      @item(There are some exceptions, but usually
        assignment is possible only when source and destination field classes
        are equal.)

      @item(Assignment (by @code(Assign), inherited from TPersistent)
        tries to copy everything: name (with alternative names), default value,
        IsClauseNames, ValueFromIsClause, Exposed, and of course current value.

        Exceptions are things related to hierarchy of containers:
        ParentNode, ParentInterfaceDeclaration. Also ExposedEventsLinked.

        If you want to copy only the current value, use AssignValue
        (or AssignLerp, where available).))
  }
  TX3DField = class(TX3DFieldOrEvent)
  private
    FExposed: boolean;
    FExposedEvents: array [boolean] of TX3DEvent;
    FChangesAlways: TX3DChanges;
    FValueFromIsClause: boolean;
    FExposedEventsLinked: boolean;

    procedure SetExposed(Value: boolean);
    function GetExposedEvents(InEvent: boolean): TX3DEvent;
    procedure SetExposedEventsLinked(const Value: boolean);
  protected
    { Save field value to a stream. Must be overriden for each specific
      field.

      For classic encoding, FieldSaveToStream and SaveToStream write
      Indent, Name, ' ', then call SaveToStreamValue, then write @link(NL).

      IS clauses are not saved by FieldSaveToStream or SaveToStream.
      (They must be saved specially, by SaveToStreamClassicIsClauses
      or special XML output.)
      SaveToStream still checks ValueFromIsClause, if ValueFromIsClause
      we will not call SaveToStreamValue. So when overriding
      SaveToStreamValue, you can safely assume that ValueFromIsClause
      is @false. }
    procedure SaveToStreamValue(Writer: TX3DWriter); virtual; abstract;

    { Save method of SaveToStreamValue. May assume things that
      SaveToStreamValue may issume, for example: if this is used at all,
      then at least field value is not default (so there is a need to write
      this field) and such. }
    function SaveToXmlValue: TSaveToXmlMethod; virtual;

    { Call this inside overriden Assign methods.
      I don't want to place this inside TX3DField.Assign, since I want
      "inherited" in Assign methods to cause exception. }
    procedure VRMLFieldAssignCommon(Source: TX3DField);

    procedure AssignValueRaiseInvalidClass(Source: TX3DField);

    { Class of the fields allowed in the exposed events of this field.
      This should usually be using ClassType of this object,
      and this is the default implementation of this method in TX3DField.

      You can override this to return some ancestor (from which, and to which,
      you can assign) if your TX3DField descendant
      doesn't change how the @code(Assign) method works.
      E.g. TSFTextureUpdate class, that wants to be fully compatible with normal
      TSFString. }
    class function ExposedEventsFieldClass: TX3DFieldClass; virtual;

    { Handle exposed input event. In TX3DField class, this does everything
      usually needed --- assigns value, sends an output event, notifies
      @link(Changed).

      You can override this for some special purposes. For special needs,
      you do not even need to call @code(inherited) in overriden versions.
      This is suitable e.g. for cases when TimeSensor.set_startTime or such
      must be ignored. }
    procedure ExposedEventReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime); virtual;
  public
    { Normal constructor.

      @italic(Descendants implementors notes:)
      when implementing constructors in descendants,
      remember that Create in this class actually just calls CreateUndefined,
      and CreateUndefined is virtual. So when calling @code(inherited Create),
      be aware that actually you may be calling your own overriden
      CreateUndefined.

      In fact, in descendants you should focus on moving all the work to
      CreateUndefined constructor.
      The Create constructor should be just a comfortable extension of
      CreateUndefined, that does the same and addiionally gets parameters
      that specify default field value. }
    constructor Create(AParentNode: TX3DFileItem; const AName: string);

    { Virtual constructor, that you can use to construct field instance when
      field class is known only at runtime.

      The idea is that in some cases, you need to create fields using
      variable like FieldClass: TX3DFieldClass. See e.g. TX3DInterfaceDeclaration,
      VRML 2.0 feature that simply requires this ability, also
      implementation of TX3DSimpleMultField.Parse and
      TX3DSimpleMultField.CreateItemBeforeParse.

      Later you can initialize such instance from string using it's Parse method.

      Note that some exceptional fields simply cannot work when initialized
      by this constructor: these are SFEnum and SFBitMask fields.
      They simply need to know their TSFEnum.EnumNames, or
      TSFBitMask.FlagNames + TSFBitMask.NoneString + TSFBitMask.AllString
      before they can be parsed. I guess that's one of the reasons why these
      field types were entirely removed from VRML 2.0. }
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); virtual;

    destructor Destroy; override;

    { Parse inits properties from Lexer.

      In this class, Parse only appends to IsClauseNames:
      if we stand on "IS" clause (see VRML 2.0 spec about "IS" clause)
      and IsClauseAllowed then we append specified identifier to
      IsClauseNames.

      If "IS" clause not found, we call ParseValue which should
      actually parse field's value.
      Descendants should override ParseValue. }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReader; IsClauseAllowed: boolean);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); virtual; abstract;

    { Parse field value from X3D XML encoded attribute using a Lexer.
      Attributes in X3D are generally encoded such that normal
      @code(ParseValue(Lexer, nil)) call is appropriate,
      so this is done in this class. }
    procedure ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader); virtual;

    { Parse field value from X3D XML encoded attribute.

      Implementation in this class creates a Lexer to parse the string,
      and calls ParseXMLAttributeLexer. }
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); virtual;

    { Parse field's value from XML Element children.
      This is used to read SFNode / MFNode field value inside <field>
      (for interface declaration default field value) and <fieldValue>
      inside <ProtoInstance>. }
    procedure ParseXMLElement(Element: TDOMElement; Reader: TX3DReader); virtual;

    { Save the field to the stream.
      Field name (if set, omitted if empty) and value are saved.
      Unless the current field value equals default value and
      FieldSaveWhenDefault is @false (default), then nothing is saved.

      IS clauses are not saved here (because they often have to be treated
      specially anyway, for XML encoding, for prototype declarations etc.). }
    procedure FieldSaveToStream(Writer: TX3DWriter;
      FieldSaveWhenDefault: boolean = false;
      XmlAvoidSavingNameBeforeValue: boolean = false);

    { Save the field to the stream.

      This simply calls FieldSaveToStream(Writer).
      See FieldSaveToStream for more comments and when you need control over
      FieldSaveWhenDefault behavior.

      It doesn't actually save anything if field value is defined
      and equals default value. }
    procedure SaveToStream(Writer: TX3DWriter); override;
    function SaveToXml: TSaveToXmlMethod; override;

    { Does current field value came from expanding "IS" clause.
      If yes, then saving this field to stream will only save it's "IS" clauses,
      never saving actual value. }
    property ValueFromIsClause: boolean
      read FValueFromIsClause write FValueFromIsClause;

    { Zwraca zawsze false w tej klasie. Mozesz to przedefiniowac w podklasach
      aby SaveToStream nie zapisywalo do strumienia pol o wartosci domyslnej. }
    function EqualsDefaultValue: boolean; virtual;

    { @true if the SecondValue object has exactly the same type and properties.
      For this class, this returns just (SecondValue.Name = Name).

      All descendants (that add some property that should be compared)
      should override this like

      @longCode(#
        Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
          (SecondValue is TMyType) and
          (TMyType(SecondValue).MyProperty = MyProperty);
      #)

      For varius floating-point fields in this unit:
      we compare each float using EqualityEpsilon,
      i.e. if the difference is < EqualityEpsilon then the floats
      are assumed equal. Pass EqualityEpsilon = 0.0
      to perform *exact* comparison (this case will be optimized
      in implementation, by using routines like CompareMem
      instead of comparing float-by-float).

      Note that this *doesn't* compare the default values of two fields
      instances. This compares only the current values of two fields
      instances, and eventually some other properties that affect
      parsing (like names for TSFEnum and TSFBitMask) or allowed
      future values (like TSFFloat.MustBeNonnegative).
    }
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; virtual; reintroduce;

    { Compare value of this field, with other field, fast.

      This compares only the values of the fields, not other properties
      (it doesn't care about names of the fields or such, or default values;
      only current values). In other words, it compares only the things
      copied by AssignValue.

      This tries to compare very fast, which means that for large
      (multi-valued) fields it may give up and answer @false even
      when they are in fact equal. So this is usable only for optimization
      purposes: when it answers @true, it is @true. When it answers @false,
      it actually doesn't know.

      Default implementation in this class (@classname) just returns @false. }
    function FastEqualsValue(SecondValue: TX3DField): boolean; virtual;

    { Does this field generate/accept events, that is
      an "exposedField" (in VRML 2.0) or "inputOutput" (in X3D). }
    property Exposed: boolean read FExposed write SetExposed default true;

    { These are the set_xxx and xxx_changed events exposed by this field.
      @nil if Exposed is @false. }
    property ExposedEvents [InEvent: boolean]: TX3DEvent
      read GetExposedEvents;

    { Exposed events of this field. @nil if this field is not exposed.
      EventIn is always equivalent to ExposedEvents[true],
      EventOut is always equivalent to ExposedEvents[false].
      @groupBegin }
    function EventIn: TX3DEvent;
    function EventOut: TX3DEvent;
    { @groupEnd }

    { When @true (default) we will automatically handle exposed events
      behavior. This means that we will listen on EventIn,
      and when something will be received we will set current field's value
      and produce appropriate EventOut.

      You almost certainly want to leave this as @true in all typical
      situations, as it takes care of implementing required exposed events
      behavior.

      That said, in special cases you may decide to break this. }
    property ExposedEventsLinked: boolean
      read FExposedEventsLinked write SetExposedEventsLinked
      default true;

    { Field type in X3D, like @code('SFString') or @code('MFInt32').
      As for VRML/X3D interface declaration statements.
      In base TX3DField class, this returns @code(XFAny)
      (name indicating any type, used by instantreality and us). }
    class function X3DType: string; virtual;
    class function TypeName: string; deprecated 'use X3DType';

    { Create TX3DEvent descendant suitable as exposed event for this field. }
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; virtual;

    { Copies the current field value. Contrary to TPersistent.Assign, this
      doesn't copy the rest of properties.

      After setting, our ValueFromIsClause is always changed to @false.
      You can manually change it to @true, if this copy indeed was done
      following "IS" clause.

      @raises(EX3DFieldAssignInvalidClass
        Usually it's required the Source class to be equal to our class,
        if Source classes cannot be assigned we raise EX3DFieldCannotAssignClass.)

      @raises(EX3DFieldAssign
        Raised in case of any field assignment problem. It's guaranteed that
        in case of such problem, our value will not be modified before
        raising the exception.

        EX3DFieldAssignInvalidClass inherits from EX3DFieldAssign,
        so actually EX3DFieldAssignInvalidClass is just a special case of this
        exceptiion.)

      @italic(Descendants implementors notes):

      In this class, implementation takes care of
      setting our ValueFromIsClause to @false. In descendants,
      you should do like

      @longCode(#
        if Source is <appropriate class> then
        begin
          inherited;
          Value := Source.value;
        end else
          AssignValueRaiseInvalidClass(Source);
      #)
    }
    procedure AssignValue(Source: TX3DField); virtual;

    { Set field's default value from the current value.

      Note that for now this doesn't guarantee that every possible field's value
      can be stored as default value. In case of trouble, it will silently
      record "no default is known" information, so e.g. EqualsDefaultValue
      will always return @false.
      Our default value mechanisms are sometimes
      limited, not every value can be a default value. For example,
      for multiple-valued nodes, we usually cannot save arrays longer than
      one as default value. This is not a problem, since X3D specification
      doesn't specify too long default values. But it may be a problem
      for prototypes, since then user can assign any value as default value.
      May be corrected in the future. }
    procedure AssignDefaultValueFromValue; virtual;

    { Assigns value to this node calculated from linear interpolation
      between two given nodes Value1, Value2. Just like other lerp
      functions in our units (like @link(CastleVectors.Lerp)).

      Like AssignValue, this copies only the current value.
      All other properties (like Name, IsClauseNames, ValueFromIsClause,
      default value) are untouched.

      There are some special precautions for this:

      @unorderedList(
        @item(First of all, AssignLerp is defined only for fields where
          CanAssignLerp returns @true, so always check CanAssignLerp first.
          All float-based fields should have this implemented.)

        @item(Use this only if Value1 and Value2
          are equal or descendant of target (Self) class.)

        @item(For multiple-value fields, counts of Value1 and Value2
          must be equal, or EX3DMultFieldDifferentCount will be raised.)
      )

      @raises(EX3DMultFieldDifferentCount When field is multiple-value
        field and Value1.Count <> Value2.Count.)
    }
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); virtual;

    { @abstract(Is AssignLerp usable on this field type?)

      @italic(Descendants implementors notes):
      In this class, this always returns @false. }
    function CanAssignLerp: boolean; virtual;

    procedure AddAlternativeName(const AlternativeName: string;
      VrmlMajorVersion: Integer); override;

    { Notify ParentNode.Scene that the value of this field changed. }
    procedure Changed;

    { What always happens when the value of this field changes.

      This is included in the @link(ExecuteChanges) method result. So instead of
      using this property, you could always override @link(ExecuteChanges)
      method. But often it's easier to use the property.

      By default this is an empty set. This is suitable for
      things that aren't *directly* an actual content (but only an
      intermediate value to change other stuff). This includes
      all metadata fields and nodes, all fields in event utilities,
      Script node, interpolators...

      See TX3DChange for possible values. }
    property ChangesAlways: TX3DChanges read FChangesAlways write FChangesAlways;

    { What happens when the value of this field changes.
      This is called, exactly once, by TCastleSceneCore.InternalChangedField
      to determine what must be done when we know that value of this field changed.

      In overridden descendants, this can also do something immediately.
      Overriding this is similar to registering your callback in OnReceive
      list, with two benefits:

      @orderedList(
        @item(This method may be not called (although no guarantees)
          when the actual field value did not change.
          In contrast, the OnReceive event is always fired,
          even when you send the same value to an exposed field,
          because VRML/X3D events and routes must be fired anyway.)

        @item(This is useful also for fields that are not exposed,
          and can be changed only by ObjectPascal code.)
      )

      So overridding this is closer to "do something when field value changes"
      than registering callback in OnReceive list. }
    function ExecuteChanges: TX3DChanges; virtual;

    { Set the value of the field, notifying the scenes and events engine.
      This sets the value of this field in the nicest possible way for
      any possible TCastleSceneCore (with events on or off) containing the node
      with this field.

      Precise specification:

      @unorderedList(
        @item(If this is an exposed field and we have events engine working:

          We will send this value through
          it's input event. In this case, this is equivalent to doing
          @code(EventIn.Send(Value, Scene.Time)).
          The scenes (including events engine) will be notified correctly
          by exposed events handler already.)

        @item(Otherwise, we will just set the fields value.
          And then notify the scenes (including events engine).)
      ) }
    procedure Send(Value: TX3DField);

    { Notifications when exposed field received new value through VRML/X3D event.
      Only for exposed fields (@nil for not exposed fields).
      This is simply a shortcut for @code(EventOut.OnReceive),
      see TX3DEvent.OnReceive for details how does this work.

      Note that this observes the "out" event (not the "in" event).
      This way you know inside the handler that the field value is already
      changed as appropriate. Inside "in" event handlers, you would not
      know this (it would depend on the order in which handlers are run,
      one "in" handler sets the field value).

      Note that "out" event handlers are executed before Scene is notified
      about the field value change (before TCastleSceneCore.InternalChangedField is called).
      This is also usually exactly what you want --- you can change the scene
      graph inside the event handler (for example, load something on
      Inline.load or Inline.url changes), and let the TX3DField.ChangesAlways
      cause appropriate action on this change. }
    function OnReceive: TX3DEventReceiveList;
  end;

  TX3DFieldList = class(specialize TFPGObjectList<TX3DField>)
  private
    function GetByName(const AName: string): TX3DField;
  public
    { Access field by name.
      Raises EX3DNotFound if the given Name doesn't exist. }
    property ByName[const AName: string]: TX3DField read GetByName;

    { Searches for a field with given Name, returns it's index or -1 if not found. }
    function IndexOf(const AName: string): integer;

    { Returns if EventName is an event implicitly exposed by one of our
      exposed fields (i.e. set_xxx or xxx_changed). If yes, then
      returns index of event, and the event reference itself
      (so always @code(Fields[ReturnedIndex].ExposedEvent[ReturnedEvent.InEvent]
      = ReturnedEvent)). Otherwise, returns -1. }
    function IndexOfExposedEvent(const EventName: string;
      out Event: TX3DEvent): Integer;
  end;

  TX3DSingleField = class(TX3DField)
  end;
  TX3DSingleFieldClass = class of TX3DSingleField;

  TX3DSingleFieldList = specialize TFPGObjectList<TX3DSingleField>;

  EX3DMultFieldDifferentCount = class(Exception);

  TX3DMultField = class(TX3DField)
  protected
    { Get or set the number of items, see @link(Count).
      @groupBegin }
    function GetCount: Integer; virtual; abstract;
    procedure SetCount(const Value: Integer); virtual; abstract;
    { @groupEnd }
  public
    { Number of items in this field.

      Remember that increasing this generally sets new items to undefined
      values (see SetCount documentation of particular descendant for docs).
      So you usually want to initialize them afterwards to something correct. }
    property Count: Integer read GetCount write SetCount;

    { If SecondValue.Count <> Count, raises EX3DMultFieldDifferentCount }
    procedure CheckCountEqual(SecondValue: TX3DMultField);
  end;

  { Field holding multiple values. Remember that such field may always have
    any number of items, including zero.

    Note that we keep MF fields contents in TFPSList or TCastleStringList instances
    (RawItems in this class, also accessible as Items (with more concrete
    class) in descendants). This means that they are in compact form,
    easy for reading, or even for feeding the list into OpenGL.
    That's the main reason why I did not simply implement
    TX3DSimpleMultField as a descendant of TX3DSingleFieldsList:
    A long list of vertexes, MFVec3f, would be kept as a list of pointers
    to a lot of TSFVec3f instances. This would be quite memory-consuming,
    and very uncomfortable for access. On the contrary, current implementation
    keeps all these vertexes inside one TVector3SingleList instance,
    that internally keeps all items in one continuos piece of memory.

    @italic(Descendants implementors notes): to make new descendant:

    @unorderedList(
      @item(In CreateUndefined you have to initialize FItemClass and create
        RawItems. In destructor of this class, RawItems are freed, so
        don't worry about this.)

      @item(Override RawItemsAdd.)

      @item(
        If your ItemClass doesn't work 100% correctly when it's initialized
        by CreateUndefined, you may have to override CreateItemBeforeParse.
        Fortunately, VRML specification was careful to choose as multi-valued field
        types' only fields that can behave nicely when initialized by
        CreateUndefined (and in fact VRML 2.0 removed the "bad fields" entirely).)

      @item(Not strictly required, but usually it's comfortable to have
        a constructor that allows you to init default field value
        from some "array of TXxx".)
    ) }
  TX3DSimpleMultField = class(TX3DMultField)
  private
    InvalidIndexWarnings: Cardinal;
  protected
    fItemClass: TX3DSingleFieldClass;

    { This creates new instance of class ItemClass. It doesn't have to
      have initialized value (in other words, it can be created by
      CreateUndefined), since we'll call his Parse method immediately.
      Default implementation in this class uses simply ItemClass.CreateUndefined. }
    function CreateItemBeforeParse: TX3DSingleField; virtual;

    { Add Item at the end of RawItems. It's guaranteed that Item
      passes here will be of class ItemClass. You should copy
      Item contents as appropriate (remember that Item instance itself
      may be freed soon, so copy contents, not only some reference). }
    procedure RawItemsAdd(Item: TX3DSingleField); virtual abstract;

    procedure WritelnWarning_InvalidIndex(const Index, ACount: Integer);
  protected
    { SaveToStreamValue overriden for MF fields. This class handles
      SaveToStreamValue fully, no need to override it again in
      descendants. }
    procedure SaveToStreamValue(Writer: TX3DWriter); override;

    { RawItemToString(i) must change RawItems[i] into a string that can be used to
      store this is text stream. In descendants, you have to override this. }
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; virtual; abstract;

    { This says when we should do newline when writing this field into a stream.
      It's has purely aesthetical meaning, nothing more.
      In this class SaveToStreamDoNewLineAfterRawItem always returns @true
      (although SaveToStreamValue may sometimes ignore it, if it knows better). }
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; virtual;

    { Get or set the number of items.
      In TX3DSimpleMultField descendants, these simply get/set RawItems.Count
      (you could do it directly as well, since operating on RawItems directly
      is Ok).
      @groupBegin }
    function GetCount: Integer; override;
    procedure SetCount(const Value: Integer); override;
    { @groupEnd }

    procedure Clear;
  public
    { Items of this field. Either TFPSList or TCastleStringList.

      @italic(Descendants implementors notes): You have to initialize this field
      in descendants' constructor, it will be always freed in our
      destructor. }
    RawItems: TObject;

    { A corresponding SF field class. All items that will be passed
      to RawItemsAdd will be of this class. }
    property ItemClass: TX3DSingleFieldClass read fItemClass;

    { Parse MF field. This class handles parsing fully, usually no need to
      override this more in descendants. It uses ItemClass.Parse method. }
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    destructor Destroy; override;

    { Checks equality between this and SecondValue field.
      In addition to inherited(Equals), this also checks that
      Count and ItemClass are equal. All descendants must check
      for equality every item on SecondValue.Items[I] and Items[I]. }
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader); override;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Single value fields) }

  { SFBitMask VRML 1.0 field.

    TSFBitMask is one of the exceptional field types that cannot
    be 100% correctly initialized by CreateUndefined, since
    EnumNames will be left undefined. }
  TSFBitMask = class(TX3DSingleField)
  private
    fAllString, fNoneString: string;
    fFlagNames: TStringList;

    { Value of this field, as a bit mask.
      VRML 1.0 specification guarantees that SFBitMask has 32 or less flags.
      Actually, defined field values have no more than 3 fields, and
      VRML > 1.0 dropped SFBitMask entirely. So 32 is always enough. }
    fFlags: set of 0..31;
    function GetFlags(i: integer): boolean;
    procedure SetFlags(i: integer; value: boolean);
    function GetFlagNames(i: integer): string;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Value of this field. You can use Index from the range 0 .. FlagsCount - 1. }
    property Flags[i: integer]:boolean read GetFlags write SetFlags;
    function FlagsCount: integer;
    property FlagNames[i: integer]:string read GetFlagNames;

    { Special strings that will be understood by parser as ALL or NONE
      bit values. AllString selects all flags, NoneString selects none.
      AllString may be '' is there's no such string, NoneString
      should never be '' (otherwise, user could not be able to specify
      some SFBitMask values --- NoneString is the only way to specify 0).

      There is usually little sense in using them like "ALL | something"
      (because it means just "ALL") or "NONE | something" (because it means
      just "something"). But it's allowed syntactically.

      @groupBegin }
    property AllString: string read fAllString;
    property NoneString: string read fNoneString;
    { @groupEnd }

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Are all flag values set to @true currently. }
    function AreAllFlags(value: boolean): boolean;

    { Constructor.

      Remember that arrays AFlagNames and AFlags
      (AFlags is initial value of Flags) must have equal length.
      Eventually, AFlags may be longer (excessive items will be ignored). }
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AFlagNames: array of string;
      const ANoneString, AAllString: string; const AFlags: array of boolean);

    destructor Destroy; override;

    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;

    class function X3DType: string; override;
  end;

  TSFBool = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: boolean;
    DefaultValue: boolean;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: boolean);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: boolean); overload;
  end;

  { SFEnum VRML 1.0 field.

    TSFEnum is one of the exceptional field types that cannot
    be 100% correctly initialized by CreateUndefined, since
    EnumNames will be left undefined. }
  TSFEnum = class(TX3DSingleField)
  private
    FEnumNames: TStringList;
    function GetEnumNames(i: integer): string;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Value between 0 .. EnumCount - 1. By default 0. }
    Value: integer;

    DefaultValue: integer;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AEnumNames: array of string; const AValue: integer);
    destructor Destroy; override;

    property EnumNames[i: integer]:string read GetEnumNames;
    function EnumNamesCount: integer;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;

    procedure Send(const AValue: LongInt); overload;
  end;

  TSFFloat = class(TX3DSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Single;
    FAngle: boolean;
    procedure SetValue(const AValue: Single);
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Single;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: Single); overload;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: Single;
      AMustBeNonnegative: boolean); overload;

    property Value: Single read FValue write SetValue;

    { If @true then when trying to set Value to something < 0,
      we'll negate it (in other words, we'll keep Value >= 0 always).
      This is nice e.g. for Sphere.FdRadius field --- some incorrect VRML specify
      negative sphere radius. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;

    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: Single); overload;
  end;

  { VRML/X3D field holding a double-precision floating point value. }
  TSFDouble = class(TX3DSingleField)
  private
    FValue: Double;
    FAngle: boolean;
    procedure SetValue(const AValue: Double);
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Double;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: Double);

    property Value: Double read FValue write SetValue;

    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: Double); overload;
  end;

  TSFTime = class(TSFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: Double); overload;
  end;

  TSFImage = class(TX3DSingleField)
  strict private
    FValue: TCastleImage;
    procedure SetValue(const AValue: TCastleImage);
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Current image, expressed as the @link(TCastleImage) instance.

      The image instance is by default owned by this object,
      which means that we will free it in destructor or when setting
      another value.

      Value may be IsEmpty, and then we know that there is no image
      recorded in this field. Value may never be @nil. }
    property Value: TCastleImage read FValue write SetValue;

    { @param(AValue is the initial value for Value.

        Note - our constructor COPIES passed reference AValue, not it's contents
        (I mean, we do Value := AValue, NOT Value := ImageCopy(AValue),
        so don't Free image given to us (at least, don't do this without clearing
        our Value field)).
        You can pass AValue = nil, then Value will be inited to null image
        TRGBImage.Create.) }
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TCastleImage);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    destructor Destroy; override;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TSFLong = class(TX3DSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Longint;
    procedure SetValue(const AValue: Longint);
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Longint;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: Longint); overload;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: Longint;
      AMustBeNonnegative: boolean); overload;

    property Value: Longint read FValue write SetValue;

    { See TSFFloat.MustBeNonnegative for explanation of this. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: LongInt); virtual; overload;
  end;

  TSFInt32 = class(TSFLong)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: LongInt); override;
  end;

  TSFMatrix3f = class(TX3DSingleField)
  private
    FValue: TMatrix3Single;
    DefaultValue: TMatrix3Single;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TMatrix3Single);

    property Value: TMatrix3Single read FValue write FValue;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TMatrix3Single); overload;
  end;

  TSFMatrix3d = class(TX3DSingleField)
  private
    FValue: TMatrix3Double;
    DefaultValue: TMatrix3Double;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TMatrix3Double);

    property Value: TMatrix3Double read FValue write FValue;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TMatrix3Double); overload;
  end;

  TSFMatrix4f = class(TX3DSingleField)
  private
    FValue: TMatrix4Single;
    DefaultValue: TMatrix4Single;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TMatrix4Single);

    property Value: TMatrix4Single read FValue write FValue;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    { Return average scale for current matrix Value.

      Note that this doesn't correctly extract scale from matrix,
      as that is too difficcult. Insted it does simple extraction,
      which will work for identity, translation and scaling matrices
      (but e.g. will fail miserably (generate nonsense results) when
      looking at some rotation matrices). }
    function TransformScale: Single;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TMatrix4Single); virtual; overload;
  end;

  { VRML 1.0 SFMatrix field. }
  TSFMatrix = class(TSFMatrix4f)
  public
    class function X3DType: string; override;
    procedure Send(const AValue: TMatrix4Single); override;
  end;

  TSFMatrix4d = class(TX3DSingleField)
  private
    FValue: TMatrix4Double;
    DefaultValue: TMatrix4Double;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TMatrix4Double);

    property Value: TMatrix4Double read FValue write FValue;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TMatrix4Double); overload;
  end;

  TSFRotation = class(TX3DSingleField)
  private
    DefaultAxis: TVector3Single;
    DefaultRotationRad: Single;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function GetValue: TVector4Single;
    procedure SetValue(const AValue: TVector4Single);
    function GetValueDeg: TVector4Single;
    procedure SetValueDeg(const AValue: TVector4Single);
  public
    Axis: TVector3Single;
    RotationRad: Single;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AnAxis: TVector3Single; const ARotationRad: Single); overload;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AValue: TVector4Single); overload;

    { Current rotation value, with last component expressing rotation in radians.

      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property Value: TVector4Single read GetValue write SetValue;

    { Current rotation value, with last component expressing rotation in degrees.

      So this is just like @link(Value), but last component is in degrees.
      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property ValueDeg: TVector4Single read GetValueDeg write SetValueDeg;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    { Rotate point Pt around Self. }
    function RotatedPoint(const pt: TVector3Single): TVector3Single;

    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function EqualsDefaultValue: boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector4Single); overload;
  end;

  TSFString = class(TX3DSingleField)
  private
    FValue: string;
    FDefaultValue: string;
    FDefaultValueExists: boolean;
  protected
    procedure SetValue(const NewValue: string); virtual;
    procedure SetDefaultValue(const NewDefaultValue: string); virtual;
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    property DefaultValue: string read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists;
    property Value: string read FValue write SetValue;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: string);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;

    procedure Send(const AValue: string); overload;
  end;

  { String field that contains a value from a specified set.
    This wraps a commonly used VRML/X3D construct where SFString field
    is used to hold values from some limited set, thus emulating
    an "enumerated" field.

    Access the EnumValue to get / set the field value as an integer,
    which is an index to ValueNames array. }
  TSFStringEnum = class(TSFString)
  private
    FEnumNames: TStringList;
    FEnumValue: Integer;
    FDefaultEnumValue: Integer;
    procedure SetEnumValue(const NewEnumValue: Integer);
    procedure SetDefaultEnumValue(const NewDefaultEnumValue: Integer);
  protected
    function StringToEnumValue(const NewValue: string): Integer;
    procedure SetValue(const NewValue: string); override;
    procedure SetDefaultValue(const NewDefaultValue: string); override;
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AEnumNames: array of string; const AValue: Integer);
    destructor Destroy; override;
    property EnumValue: Integer
      read FEnumValue write SetEnumValue;
    property DefaultEnumValue: Integer
      read FDefaultEnumValue write SetDefaultEnumValue;
    procedure SendEnumValue(const NewValue: Integer);
  end;

  TSFVec2f = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector2Single;

    DefaultValue: TVector2Single;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector2Single);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector2Single); overload;
  end;

  TSFVec3f = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector3Single;

    DefaultValue: TVector3Single;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector3Single);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector3Single); virtual; overload;
    { Change only a given component of the vector. }
    procedure Send(const Index: Integer; const ComponentValue: Single); overload;
  end;

  TSFColor = class(TSFVec3f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: TVector3Single); override;
  end;

  TSFVec4f = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector4Single;

    DefaultValue: TVector4Single;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector4Single);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector4Single); virtual; overload;
  end;

  TSFColorRGBA = class(TSFVec4f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: TVector4Single); override;
  end;

  TSFVec2d = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector2Double;

    DefaultValue: TVector2Double;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector2Double);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector2Double); overload;
  end;

  TSFVec3d = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector3Double;

    DefaultValue: TVector3Double;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector3Double);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector3Double); overload;
  end;

  TSFVec4d = class(TX3DSingleField)
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TVector4Double;

    DefaultValue: TVector4Double;
    DefaultValueExists: boolean;

    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const AValue: TVector4Double);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector4Double); overload;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Multiple value (MF) fields)

  General implementation comments for MF fields:

    Field DefaultValuesCount may have three valid values (for now):
    -1 (means "no default value for this field")
    0 (means "default value of this field is empty")
    1 (means "default value of this field is one-item array with DefaultValue").

    As you can see, it's not possible to express default values with more
    than one item. That's OK, because nodes from VRML 1.0 and 2.0 specifications
    never have such field (and VRML 2.0 prototypes (that have user-defined
    default field values) actually don't need it). So, for now, more flexible
    DefaultValuesCount is not needed.

    CreateUndefined sets DefaultValuesCount to -1.
}

  { }
  TMFBool = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: boolean;
    function GetItems: TBooleanList;
    procedure SetItems(const Value: TBooleanList);
    function GetItemsSafe(Index: Integer): boolean;
    procedure SetItemsSafe(Index: Integer; const Value: boolean);
  protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TBooleanList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const InitialContent: array of boolean);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return false, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: boolean
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of boolean); overload;
  end;

  TMFLong = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Longint;
    FSaveToStreamLineUptoNegative: boolean;
    WrongVertexIndexWarnings: Integer;
    function GetItemsSafe(Index: Integer): LongInt;
    procedure SetItemsSafe(Index: Integer; const Value: LongInt);
    function GetItems: TLongIntList;
    procedure SetItems(const Value: TLongIntList);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    { When saving to VRML/X3D classic encoding,
      make newline after each negative value on the list.
      This makes a nice output for fields like IndexedFaceSet.coordIndex. }
    property SaveToStreamLineUptoNegative: boolean
      read FSaveToStreamLineUptoNegative write FSaveToStreamLineUptoNegative
      default false;

    property Items: TLongintList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of Longint);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: LongInt
      read GetItemsSafe write SetItemsSafe;

    { Call WritelnWarning reporting that an invalid vertex index
      is caused from this field. This simply calls WritelnWarning
      formatting appropriate message.

      Additionally this guards
      us against producing too many warnings from the same field.
      When a given threshold will be reached, further
      WritelnWarning_WrongVertexIndex calls for this field instance
      will be simply ignored. This is a good thing, as some invalid models
      have really an incredible amount of invalid indexes, and the very
      amount of lines printed on console makes viewing these invalid files
      (at least, the valid parts of them) impossible.

      Example test cases:
      content/examples/Basic/HumanoidAnimation/AllenStandShootRifleM24.x3d
      and
      content/examples/Basic/HumanoidAnimation/NancyDiving.x3dv
      from http://www.web3d.org/ example models. }
    procedure WritelnWarning_WrongVertexIndex(
      const GeometryX3DType: string;
      const VertexNum: Integer; const CoordCount: Integer);

    procedure Send(const AValue: array of LongInt); virtual; overload;
  end;

  TMFInt32 = class(TMFLong)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: array of LongInt); override;
  end;

  TMFMatrix3f = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TMatrix3Single;
    function GetItems: TMatrix3SingleList;
    procedure SetItems(const Value: TMatrix3SingleList);
    function GetItemsSafe(Index: Integer): TMatrix3Single;
    procedure SetItemsSafe(Index: Integer; const Value: TMatrix3Single);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TMatrix3SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TMatrix3Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return identity matrix, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TMatrix3Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TMatrix3Single); overload;
  end;

  TMFMatrix3d = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TMatrix3Double;
    function GetItems: TMatrix3DoubleList;
    procedure SetItems(const Value: TMatrix3DoubleList);
    function GetItemsSafe(Index: Integer): TMatrix3Double;
    procedure SetItemsSafe(Index: Integer; const Value: TMatrix3Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TMatrix3DoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TMatrix3Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return identity matrix, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TMatrix3Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TMatrix3Double); overload;
  end;

  TMFMatrix4f = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TMatrix4Single;
    function GetItems: TMatrix4SingleList;
    procedure SetItems(const Value: TMatrix4SingleList);
    function GetItemsSafe(Index: Integer): TMatrix4Single;
    procedure SetItemsSafe(Index: Integer; const Value: TMatrix4Single);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TMatrix4SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TMatrix4Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return identity matrix, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TMatrix4Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TMatrix4Single); overload;
  end;

  TMFMatrix4d = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TMatrix4Double;
    function GetItems: TMatrix4DoubleList;
    procedure SetItems(const Value: TMatrix4DoubleList);
    function GetItemsSafe(Index: Integer): TMatrix4Double;
    procedure SetItemsSafe(Index: Integer; const Value: TMatrix4Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TMatrix4DoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TMatrix4Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return identity matrix, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TMatrix4Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TMatrix4Double); overload;
  end;

  TMFVec2f = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector2Single;
    function GetItemsSafe(Index: Integer): TVector2Single;
    procedure SetItemsSafe(Index: Integer; const Value: TVector2Single);
    function GetItems: TVector2SingleList;
    procedure SetItems(const Value: TVector2SingleList);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector2SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector2Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero vector, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector2Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector2Single); overload;
  end;

  TMFVec3f = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector3Single;
    function GetItemsSafe(Index: Integer): TVector3Single;
    procedure SetItemsSafe(Index: Integer; const Value: TVector3Single);
    function GetItems: TVector3SingleList;
    procedure SetItems(const Value: TVector3SingleList);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector3SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector3Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero vector, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector3Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector3Single); virtual; overload;
  end;

  TMFColor = class(TMFVec3f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: array of TVector3Single); override;
  end;

  TMFVec4f = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector4Single;
    function GetItemsSafe(Index: Integer): TVector4Single;
    procedure SetItemsSafe(Index: Integer; const Value: TVector4Single);
    function GetItems: TVector4SingleList;
    procedure SetItems(const Value: TVector4SingleList);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector4SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector4Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return (0, 0, 0, 1) vector, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector4Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector4Single); virtual; overload;
  end;

  TMFColorRGBA = class(TMFVec4f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: array of TVector4Single); override;
  end;

  TMFVec2d = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector2Double;
    function GetItems: TVector2DoubleList;
    procedure SetItems(const Value: TVector2DoubleList);
    function GetItemsSafe(Index: Integer): TVector2Double;
    procedure SetItemsSafe(Index: Integer; const Value: TVector2Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector2DoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector2Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero vector, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector2Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector2Double); overload;
  end;

  TMFVec3d = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector3Double;
    function GetItems: TVector3DoubleList;
    procedure SetItems(const Value: TVector3DoubleList);
    function GetItemsSafe(Index: Integer): TVector3Double;
    procedure SetItemsSafe(Index: Integer; const Value: TVector3Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector3DoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector3Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero vector, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector3Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector3Double); overload;
  end;

  TMFVec4d = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector4Double;
    function GetItems: TVector4DoubleList;
    procedure SetItems(const Value: TVector4DoubleList);
    function GetItemsSafe(Index: Integer): TVector4Double;
    procedure SetItemsSafe(Index: Integer; const Value: TVector4Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector4DoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of TVector4Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return (0, 0, 0, 1), Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector4Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector4Double); overload;
  end;

  TMFRotation = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: TVector4Single;
    function GetItems: TVector4SingleList;
    procedure SetItems(const Value: TVector4SingleList);
    function GetItemsSafe(Index: Integer): TVector4Single;
    procedure SetItemsSafe(Index: Integer; const Value: TVector4Single);
  protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
  public
    property Items: TVector4SingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const InitialContent: array of TVector4Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return DefaultRotation, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TVector4Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of TVector4Single); overload;
  end;

  TMFFloat = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Single;
    FAngle: boolean;
    function GetItems: TSingleList;
    procedure SetItems(const Value: TSingleList);
    function GetItemsSafe(Index: Integer): Single;
    procedure SetItemsSafe(Index: Integer; const Value: Single);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
    function CreateItemBeforeParse: TX3DSingleField; override;
  public
    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    property Items: TSingleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const InitialContent: array of Single);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: Single
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of Single); overload;
  end;

  TMFDouble = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Double;
    function GetItems: TDoubleList;
    procedure SetItems(const Value: TDoubleList);
    function GetItemsSafe(Index: Integer): Double;
    procedure SetItemsSafe(Index: Integer; const Value: Double);
  protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    property Items: TDoubleList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const InitialContent: array of Double);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return zero, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: Double
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of Double); overload;
  end;

  TMFTime = class(TMFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFString = class(TX3DSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: string;
    function GetItems: TCastleStringList;
    procedure SetItems(const Value: TCastleStringList);
    function GetItemsSafe(Index: Integer): string;
    procedure SetItemsSafe(Index: Integer; const Value: string);
  protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    property Items: TCastleStringList read GetItems write SetItems;
    procedure RawItemsAdd(Item: TX3DSingleField); override;
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string; const InitialContent: array of string);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return '', Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: string
      read GetItemsSafe write SetItemsSafe;

    procedure Send(const AValue: array of string); overload;
  end;

  { Stores information about available VRML/X3D field classes.
    The only use for now is to make a mapping from VRML/X3D field name to
    actual class (needed by VRML/X3D interface declarations). }
  TX3DFieldsManager = class
  private
    Registered: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterClass(AClass: TX3DFieldClass);
    procedure RegisterClasses(const Classes: array of TX3DFieldClass);

    { Return field class for given name. Returns @nil if not found. }
    function X3DTypeToClass(const X3DType: string): TX3DFieldClass;
  end;

  {$I x3devents.inc}
  {$I x3devents_descendants.inc}

function X3DFieldsManager: TX3DFieldsManager;

{ Decode color from integer value, following VRML/X3D SFImage specification.
  @groupBegin }
procedure DecodeImageColor(const Pixel: LongWord; var G: Byte);
procedure DecodeImageColor(const Pixel: LongWord; var GA: TVector2Byte);
procedure DecodeImageColor(const Pixel: LongWord; var RGB: TVector3Byte);
procedure DecodeImageColor(const Pixel: LongWord; var RGBA: TVector4Byte);

procedure DecodeImageColor(const Pixel: LongInt; var G: Byte);
procedure DecodeImageColor(const Pixel: LongInt; var GA: TVector2Byte);
procedure DecodeImageColor(const Pixel: LongInt; var RGB: TVector3Byte);
procedure DecodeImageColor(const Pixel: LongInt; var RGBA: TVector4Byte);
{ @groupEnd }

const
  X3DChangeToStr: array [TX3DChange] of string =
  ( 'Visible Geometry',
    'Visible Non-Geometry',
    'Redisplay',
    'Transform',
    'Coordinate',
    'VRML 1.0 State (but not affecting geometry or Coordinate)',
    'VRML 1.0 State (affecting geometry, but not Coordinate)',
    'Material',
    'Blending',
    'Light active property',
    'Light location/direction',
    'Light for shadow volumes',
    'Switch choice',
    'Color node',
    'Texture coordinate',
    'Texture transform',
    'Geometry',
    'Environmental sensor bounds',
    'Time stop/start/pause/resume',
    'Viewpoint vectors',
    'Viewpoint projection',
    'Texture image',
    'Texture renderer properties',
    'TextureProperties node',
    'Shadow caster',
    'Generated texture update',
    'FontStyle',
    'HeadLight on',
    'ClipPlane',
    'X3DDragSensorNode.enabled',
    'NavigationInfo',
    'ScreenEffect.enabled',
    'Background',
    'Everything',
    'Shadow maps',
    'Wireframe');

function X3DChangesToStr(const Changes: TX3DChanges): string;

{$undef read_interface}

implementation

uses Math, X3DNodes, CastleXMLUtils, CastleLog;

{$define read_implementation}

{$I x3devents.inc}
{$I x3devents_descendants.inc}

{ TX3DWriter ----------------------------------------------------------------- }

const
  { IndentIncrement is string or char. It's used by SaveToStream }
  IndentIncrement = CharTab;

constructor TX3DWriter.Create(AStream: TStream; const AVersion: TX3DVersion;
  const AEncoding: TX3DEncoding);
begin
  inherited Create;
  Version := AVersion;
  FStream := AStream;
  FEncoding := AEncoding;
end;

destructor TX3DWriter.Destroy;
begin
  inherited;
end;

procedure TX3DWriter.IncIndent;
var
  L: Integer;
begin
  L := Length(Indent) + 1;
  SetLength(Indent, L);
  Indent[L] := IndentIncrement;
end;

procedure TX3DWriter.DecIndent;
begin
  SetLength(Indent, Length(Indent) - 1);
end;

procedure TX3DWriter.Write(const S: string);
begin
  WriteStr(FStream, S);
end;

procedure TX3DWriter.Writeln(const S: string);
begin
  WriteStr(FStream, S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.Writeln;
begin
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.WriteIndent(const S: string);
begin
  if DoDiscardNextIndent then
    DoDiscardNextIndent := false else
    WriteStr(FStream, Indent);
  WriteStr(FStream, S);
end;

procedure TX3DWriter.WritelnIndent(const S: string);
begin
  WriteIndent(S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.DiscardNextIndent;
begin
  DoDiscardNextIndent := true;
end;

{ TX3DReader ----------------------------------------------------------------- }

constructor TX3DReader.Create(
  const ABaseUrl: string; const AVersion: TX3DVersion);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FVersion := AVersion;
  AngleConversionFactor := 1;
  LengthConversionFactor := 1;
end;

constructor TX3DReader.CreateCopy(Source: TX3DReader);
begin
  inherited Create;
  FBaseUrl := Source.BaseUrl;
  FVersion := Source.Version;
  AngleConversionFactor := Source.AngleConversionFactor;
  LengthConversionFactor := Source.LengthConversionFactor;
end;

procedure TX3DReader.UnitConversion(const Category, Name: string;
  const ConversionFactor: Float);
begin
  if (Version.Major < 3) or
     ( (Version.Major = 3) and
       (Version.Minor < 3) ) then
    WritelnWarning('X3D', 'UNIT declaration found, but X3D version is < 3.3');

  { store UNIT inside Reader }
  if Category = 'angle' then
    AngleConversionFactor := ConversionFactor else
  if Category = 'force' then
    { TODO } else
  if Category = 'length' then
    LengthConversionFactor := ConversionFactor else
  if Category = 'mass' then
    { TODO } else
    WritelnWarning('X3D', Format('UNIT category "%s" unknown. Only the categories listed in X3D specification as base units are allowed',
      [Category]));
end;

{ TX3DFileItem -------------------------------------------------------------- }

constructor TX3DFileItem.Create;
begin
  inherited;
  FPositionInParent := -1;
end;

function TX3DFileItem.SaveToXml: TSaveToXmlMethod;
begin
  Result := sxChildElement;
end;

{ TX3DFileItemList --------------------------------------------------------- }

function IsSmallerPositionInParent(const A, B: TX3DFileItem): Integer;
begin
  Result := A.PositionInParent - B.PositionInParent;
  if Result = 0 then
    Result := A.PositionOnList - B.PositionOnList;
end;

procedure TX3DFileItemList.SortPositionInParent;
begin
  Sort(@IsSmallerPositionInParent);
end;

procedure TX3DFileItemList.SaveToStream(Writer: TX3DWriter);
var
  I: Integer;
begin
  SortPositionInParent;
  for I := 0 to Count - 1 do
    Items[I].SaveToStream(Writer);
end;

procedure TX3DFileItemList.Add(Item: TX3DFileItem);
begin
  Item.PositionOnList := Count;
  inherited Add(Item);
end;

{ TX3DFieldOrEvent ---------------------------------------------------------- }

constructor TX3DFieldOrEvent.Create(AParentNode: TX3DFileItem;
  const AX3DName: string);
begin
  inherited Create;
  FParentNode := AParentNode;
  FX3DName := AX3DName;
end;

destructor TX3DFieldOrEvent.Destroy;
begin
  FreeAndNil(FIsClauseNames);
  inherited;
end;

function TX3DFieldOrEvent.IsClauseNamesCreate: TCastleStringList;
begin
  if FIsClauseNames = nil then
    FIsClauseNames := TCastleStringList.Create;
  Result := FIsClauseNames;
end;

function TX3DFieldOrEvent.GetIsClauseNames(const Index: Integer): string;
begin
  if FIsClauseNames = nil then
    raise Exception.CreateFmt('IsClauseNames item index %d does not exist, because IsClauseNames is empty',
      [Index]);
  Result := FIsClauseNames[Index];
end;

function TX3DFieldOrEvent.IsClauseNamesCount: Integer;
begin
  if FIsClauseNames = nil then
    Result := 0 else
    Result := FIsClauseNames.Count;
end;

procedure TX3DFieldOrEvent.IsClauseNamesAssign(
  const SourceIsClauseNames: TCastleStringList);
begin
  if (SourceIsClauseNames <> nil) and
     (SourceIsClauseNames.Count <> 0) then
    IsClauseNamesCreate.Assign(SourceIsClauseNames) else
    FreeAndNil(FIsClauseNames);
end;

procedure TX3DFieldOrEvent.IsClauseNamesAdd(const S: string);
begin
  IsClauseNamesCreate.Add(S);
end;

procedure TX3DFieldOrEvent.ParseIsClause(Lexer: TX3DLexer);
begin
  if Lexer.TokenIsKeyword(vkIS) then
  begin
    Lexer.NextToken;
    IsClauseNamesCreate.Add(Lexer.TokenName);
    Lexer.NextToken;
  end;
end;

procedure TX3DFieldOrEvent.AddAlternativeName(const AlternativeName: string;
  VrmlMajorVersion: Integer);
begin
  FAlternativeNames[VrmlMajorVersion] := AlternativeName;
end;

function TX3DFieldOrEvent.IsName(const S: string): boolean;
var
  I: Integer;
begin
  { No field is ever named ''.
    Actually, we sometimes use '' for special "unnamed fields",
    in this case it's Ok that no name matches their name.
    Besides, we don't want empty FAlternativeNames to match when
    searching for S = ''. }

  if S = '' then
    Exit(false);

  for I := Low(FAlternativeNames) to High(FAlternativeNames) do
    if FAlternativeNames[I] = S then
      Exit(true);

  Result := X3DName = S;
end;

function TX3DFieldOrEvent.NameForVersion(
  Version: TX3DVersion): string;
begin
  Result := FAlternativeNames[Version.Major];
  if Result = '' then
    Result := X3DName;
end;

function TX3DFieldOrEvent.NameForVersion(
  Writer: TX3DWriter): string;
begin
  Result := NameForVersion(Writer.Version);
end;

procedure TX3DFieldOrEvent.FieldOrEventAssignCommon(Source: TX3DFieldOrEvent);
begin
  FX3DName := Source.X3DName;
  IsClauseNamesAssign(Source.FIsClauseNames);
  FPositionInParent := Source.PositionInParent;
  FAlternativeNames := Source.FAlternativeNames;
end;

function TX3DFieldOrEvent.NiceName: string;
begin
  Result := '';

  if ParentNode <> nil then
    Result += TX3DNode(ParentNode).NiceName + '.';

  if X3DName <> '' then
    Result += X3DName
  else
    Result += '<not named field>';
end;

procedure TX3DFieldOrEvent.SaveToStreamClassicIsClauses(Writer: TX3DWriter);
var
  N: string;
  I: Integer;
begin
  N := NameForVersion(Writer);

  { When N = '', we assume that field/event has only one "IS" clause.
    Otherwise results don't make any sense. }
  for I := 0 to IsClauseNamesCount - 1 do
  begin
    if N <> '' then
      Writer.WriteIndent(N + ' ');
    Writer.Writeln('IS ' + IsClauseNames[I]);
  end;
end;

{ TX3DField ------------------------------------------------------------- }

constructor TX3DField.Create(AParentNode: TX3DFileItem;
  const AName: string);
begin
  CreateUndefined(AParentNode, AName,
    true { default Exposed = true for normal constructor });
end;

constructor TX3DField.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited Create(AParentNode, AName);

  FExposedEventsLinked := true;

  { Set Exposed by the property, to force FExposedEvents initialization }
  FExposed := false;
  Exposed := AExposed;
end;

destructor TX3DField.Destroy;
begin
  FreeAndNil(FExposedEvents[false]);
  FreeAndNil(FExposedEvents[true]);
  inherited;
end;

function TX3DField.GetExposedEvents(InEvent: boolean): TX3DEvent;
begin
  Result := FExposedEvents[InEvent];
end;

function TX3DField.EventIn: TX3DEvent;
begin
  Result := FExposedEvents[true];
end;

function TX3DField.EventOut: TX3DEvent;
begin
  Result := FExposedEvents[false];
end;

procedure TX3DField.ExposedEventReceive(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
var
  ValuePossiblyChanged: boolean;
begin
  Assert(Exposed);
  Assert(Event = FExposedEvents[true]);
  Assert(Value is ExposedEventsFieldClass);

  { When not ValuePossiblyChanged, we don't have to call InternalChangedField.
    (Although we still have to call FExposedEvents[false].Send,
    to push the change through the routes.)
    This may be an important optimization when simple field's change
    causes large time-consuming work in InternalChangedField, e.g. consider
    Switch.whichChoice which means currently rebuilding a lot of things. }
  ValuePossiblyChanged := not FastEqualsValue(Value);

  { This is trivial handling of exposed events: just set our value,
    and call out event. }

  AssignValue(Value);

  FExposedEvents[false].Send(Value, Time);

  { Tests:
  if not ValuePossiblyChanged then
    writeln('ignored field ', Name, ' change, since values the same'); }
  if ValuePossiblyChanged then
    Changed;
end;

procedure TX3DField.Changed;
var
  Parent: TX3DNode;
begin
  if ParentNode <> nil then
  begin
    Parent := ParentNode as TX3DNode;
    if Parent.Scene <> nil then
      Parent.Scene.InternalChangedField(Self);
  end;
end;

function TX3DField.ExecuteChanges: TX3DChanges;
begin
  Result := ChangesAlways;
end;

procedure TX3DField.Send(Value: TX3DField);
var
  ValuePossiblyChanged: boolean;
begin
  if Exposed and (ParentNode <> nil) and
    ( (ParentNode as TX3DNode).Scene <> nil ) then
  begin
    EventIn.Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
  end else
  begin
    ValuePossiblyChanged := not FastEqualsValue(Value);
    { Call AssignValue regardless of ValuePossiblyChanged.
      Reason: AssignValue also removes "IS" clause. }
    AssignValue(Value);
    if ValuePossiblyChanged then Changed;
  end;
end;

const
  SetPrefix = 'set_';
  ChangedSuffix = '_changed';

procedure TX3DField.SetExposedEventsLinked(const Value: boolean);
begin
  if FExposedEventsLinked <> Value then
  begin
    FExposedEventsLinked := Value;
    if Exposed then
    begin
      if ExposedEventsLinked then
        FExposedEvents[true].OnReceive.Add(@ExposedEventReceive) else
        FExposedEvents[true].OnReceive.Remove(@ExposedEventReceive);
    end;
  end;
end;

class function TX3DField.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TX3DFieldClass(ClassType);
end;

class function TX3DField.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TX3DEvent.Create(AParentNode, AName, ExposedEventsFieldClass, AInEvent);
end;

procedure TX3DField.SetExposed(Value: boolean);
var
  I: Integer;
begin
  if Value <> Exposed then
  begin
    FExposed := Value;
    if Exposed then
    begin
      FExposedEvents[false] := CreateEvent(ParentNode, X3DName + ChangedSuffix, false);
      FExposedEvents[false].ParentExposedField := Self;
      FExposedEvents[true] := CreateEvent(ParentNode, SetPrefix + X3DName, true);
      FExposedEvents[true].ParentExposedField := Self;

      for I := Low(FAlternativeNames) to High(FAlternativeNames) do
        if FAlternativeNames[I] <> '' then
        begin
          FExposedEvents[false].AddAlternativeName(
            FAlternativeNames[I] + ChangedSuffix, I);
          FExposedEvents[true].AddAlternativeName(
            SetPrefix + FAlternativeNames[I], I);
        end;

      if ExposedEventsLinked then
        FExposedEvents[true].OnReceive.Add(@ExposedEventReceive);
    end else
    begin
      if ExposedEventsLinked then
        FExposedEvents[true].OnReceive.Remove(@ExposedEventReceive);

      FreeAndNil(FExposedEvents[false]);
      FreeAndNil(FExposedEvents[true]);
    end;
  end;
end;

procedure TX3DField.FieldSaveToStream(Writer: TX3DWriter;
  FieldSaveWhenDefault, XmlAvoidSavingNameBeforeValue: boolean);
var
  N: string;
begin
  N := NameForVersion(Writer);

  if (not ValueFromIsClause) and
     (FieldSaveWhenDefault or (not EqualsDefaultValue)) then
  case Writer.Encoding of
    xeClassic:
      begin
        if N <> '' then
          Writer.WriteIndent(N + ' ');
        SaveToStreamValue(Writer);
        Writer.Writeln;
      end;
    xeXML:
      { for xml encoding, field must be named, unless explicitly not wanted by XmlAvoidSavingNameBeforeValue }
      if (N <> '') or XmlAvoidSavingNameBeforeValue then
      begin
        if (SaveToXml in [sxAttribute, sxAttributeCustomQuotes]) and
           (not XmlAvoidSavingNameBeforeValue) then
        begin
          Writer.Writeln;
          Writer.WriteIndent(N + '=');
        end;
        if SaveToXml = sxAttribute then
          Writer.Write('"');
        SaveToStreamValue(Writer);
        if SaveToXml = sxAttribute then
          Writer.Write('"');
      end;
    else raise EInternalError.Create('TX3DField.FieldSaveToStream Encoding?');
  end;
end;

procedure TX3DField.SaveToStream(Writer: TX3DWriter);
begin
  FieldSaveToStream(Writer);
end;

function TX3DField.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttribute;
end;

function TX3DField.SaveToXml: TSaveToXmlMethod;
begin
  { Detect sxNone for XML encoding, this allows better output in many cases,
    also avoids <fieldValue> inside <ProtoInstance> when the field value actually
    doesn't have to be specified.
    When FieldSaveToStream saves field value? FieldSaveToStream checks

     (not ValueFromIsClause) and
     (FieldSaveWhenDefault or (not EqualsDefaultValue))

    SaveToStream calls FieldSaveToStream with default FieldSaveWhenDefault = false. }

  if (not ValueFromIsClause) and (not EqualsDefaultValue) then
    Result := SaveToXmlValue else
    Result := sxNone;
end;

function TX3DField.EqualsDefaultValue: boolean;
begin
  Result := false;
end;

function TX3DField.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := SecondValue.X3DName = X3DName;
end;

function TX3DField.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := false;
end;

procedure TX3DField.Parse(Lexer: TX3DLexer; Reader: TX3DReader; IsClauseAllowed: boolean);
begin
  if IsClauseAllowed and Lexer.TokenIsKeyword(vkIS) then
    ParseIsClause(Lexer) else
    ParseValue(Lexer, Reader);
end;

procedure TX3DField.ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  ParseValue(Lexer, Reader);
end;

procedure TX3DField.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Lexer: TX3DLexer;
begin
  Lexer := TX3DLexer.CreateForPartialStream(AttributeValue, Reader.Version);
  try
    try
      ParseXMLAttributeLexer(Lexer, Reader);
    except
      on E: EX3DClassicReadError do
        WritelnWarning('VRML/X3D', 'Error when reading field "' + X3DName + '" value: ' + E.Message);
    end;
  finally FreeAndNil(Lexer) end;
end;

procedure TX3DField.ParseXMLElement(Element: TDOMElement; Reader: TX3DReader);
var
  I: TXMLElementIterator;
begin
  I := Element.ChildrenIterator;
  try
    if I.GetNext then
      WritelnWarning('VRML/X3D', Format('X3D field "%s" is not SFNode or MFNode, but a node value (XML element "%s") is specified',
        [X3DName, I.Current.TagName]));
  finally FreeAndNil(I) end;
end;

procedure TX3DField.VRMLFieldAssignCommon(Source: TX3DField);
var
  NameChanges, ExposedChanges: boolean;
  I: Integer;
begin
  NameChanges := X3DName <> Source.X3DName;
  ExposedChanges := Exposed <> Source.Exposed;

  FieldOrEventAssignCommon(Source);

  ValueFromIsClause := Source.ValueFromIsClause;

  Exposed := Source.Exposed;
  Assert(Exposed = (ExposedEvents[false] <> nil));
  Assert(Exposed = (ExposedEvents[true] <> nil));

  { This is a little tricky: we copied Exposed value by SetExposed,
    to actually create or destroy exposed events.

    But note that events in
    ExposedEvents have names dependent on our name. So we have to eventually
    change their names too. This is not needed if exposed
    changes from true->false (then events will be destroyed),
    changes from false->true (then events will be created with already new names),
    stays as false->false (then events don't exist).
    So it's needed only when exposed was true, and stays true, but name changed.
  }
  if NameChanges and Exposed and (not ExposedChanges) then
  begin
    FExposedEvents[false].FX3DName := X3DName + ChangedSuffix;
    FExposedEvents[true].FX3DName := SetPrefix + X3DName;
  end;

  Assert((not Exposed) or (FExposedEvents[false].FX3DName = X3DName + ChangedSuffix));
  Assert((not Exposed) or (FExposedEvents[true].FX3DName = SetPrefix + X3DName));

  { Once again an issue with dependency of ExposedEvents on our name:
    potentially alternative names changed,
    so we have to redo this in exposed events. }
  if Exposed then
  begin
    for I := Low(FAlternativeNames) to High(FAlternativeNames) do
      if FAlternativeNames[I] <> '' then
      begin
        FExposedEvents[false].FAlternativeNames[I] :=
          FAlternativeNames[I] + ChangedSuffix;
        FExposedEvents[true].FAlternativeNames[I] :=
          SetPrefix + FAlternativeNames[I];
      end else
      begin
        FExposedEvents[false].FAlternativeNames[I] := '';
        FExposedEvents[true].FAlternativeNames[I] := '';
      end;
  end;
end;

procedure TX3DField.AssignValueRaiseInvalidClass(Source: TX3DField);
begin
  raise EX3DFieldAssignInvalidClass.CreateFmt('Cannot assign VRML/X3D field ' +
    '%s (%s) from %s (%s)',
    [        X3DName,        X3DType,
      Source.X3DName, Source.X3DType]);
end;

procedure TX3DField.AssignValue(Source: TX3DField);
begin
  ValueFromIsClause := false;
end;

procedure TX3DField.AssignDefaultValueFromValue;
begin
  { do nothing in this class }
end;

procedure TX3DField.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  { do nothing, CanAssignLerp is false }
end;

function TX3DField.CanAssignLerp: boolean;
begin
  Result := false;
end;

procedure TX3DField.AddAlternativeName(const AlternativeName: string;
  VrmlMajorVersion: Integer);
begin
  inherited;

  if Exposed then
  begin
    Assert(FExposedEvents[false] <> nil);
    Assert(FExposedEvents[true] <> nil);

    FExposedEvents[false].AddAlternativeName(
      AlternativeName + ChangedSuffix, VrmlMajorVersion);
    FExposedEvents[true].AddAlternativeName(
      SetPrefix + AlternativeName, VrmlMajorVersion);
  end;
end;

{ Note that TX3DField.X3DType cannot be abstract:
  it may be used if source event is of XFAny type in warning message
  in TX3DRoute.SetEndingInternal }
class function TX3DField.X3DType: string;
begin
  Result := 'XFAny';
end;

class function TX3DField.TypeName: string;
begin
  Result := X3DType;
end;

function TX3DField.OnReceive: TX3DEventReceiveList;
begin
  if FExposedEvents[false] <> nil then
    Result := FExposedEvents[false].OnReceive else
    Result := nil;
end;

{ TX3DFieldList ------------------------------------------------------------- }

function TX3DFieldList.IndexOf(const AName: string): integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result].IsName(AName) then
      Exit;
  Result := -1;
end;

function TX3DFieldList.GetByName(const AName: string): TX3DField;
var
  I: integer;
begin
  I := IndexOf(AName);
  if I <> -1 then
    Result := Items[I] else
    raise EX3DNotFound.CreateFmt('Field name "%s" not found', [AName]);
end;

function TX3DFieldList.IndexOfExposedEvent(const EventName: string;
  out Event: TX3DEvent): Integer;
var
  InEvent: boolean;
begin
  { This implementation is quite optimized.
    Instead of browsing all fields and their ExposedEvents,
    looking for EventName event, instead we examine EventName
    to look whether this has any chance of being set_xxx or xxx_changed
    event. So we utilize the fact that exposed events have consistent
    naming. }

  if IsPrefix(SetPrefix, EventName, false) then
  begin
    InEvent := true;
    Result := IndexOf(SEnding(EventName, Length(SetPrefix) + 1));
  end else
  if IsSuffix(ChangedSuffix, EventName, false) then
  begin
    InEvent := false;
    Result := IndexOf(Copy(EventName, 1,
      Length(EventName) - Length(ChangedSuffix)));
  end else
    Result := -1;

  { check is field really exposed now }
  if (Result <> -1) and (not Items[Result].Exposed) then
  begin
    Result := -1;
  end;

  if Result <> -1 then
  begin
    Event := Items[Result].ExposedEvents[InEvent];
  end;
end;

{ TX3DMultField ------------------------------------------------------------- }

procedure TX3DMultField.CheckCountEqual(SecondValue: TX3DMultField);
begin
  if SecondValue.Count <> Count then
    raise EX3DMultFieldDifferentCount.CreateFmt(
      'Different length of multiple-value fields "%s" and "%s": "%d" and "%d"',
      [ X3DName,
        SecondValue.X3DName,
        Count,
        SecondValue.Count ]);
end;

{ TX3DSimpleMultField ------------------------------------------------------- }

destructor TX3DSimpleMultField.Destroy;
begin
  RawItems.Free;
  inherited;
end;

function TX3DSimpleMultField.GetCount: Integer;
begin
  if RawItems is TFPSList then
    Result := TFPSList(RawItems).Count else
    Result := TCastleStringList(RawItems).Count;
end;

procedure TX3DSimpleMultField.SetCount(const Value: Integer);
begin
  if RawItems is TFPSList then
    TFPSList(RawItems).Count := Value else
    TCastleStringList(RawItems).Count := Value;
end;

procedure TX3DSimpleMultField.Clear;
begin
  if RawItems is TFPSList then
    TFPSList(RawItems).Clear else
    TCastleStringList(RawItems).Clear;
end;

function TX3DSimpleMultField.CreateItemBeforeParse: TX3DSingleField;
begin
  result := ItemClass.CreateUndefined(ParentNode, '', false);
end;

procedure TX3DSimpleMultField.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TX3DSingleField;
begin
  Clear;

  SingleItem := nil;
  try
    SingleItem := CreateItemBeforeParse;

    if Lexer.Token = vtOpenSqBracket then
    begin
      Lexer.NextToken;

      { List size may be increased rapidly during parsing.
        Prepare for it by allocating some size in advance. }
      if RawItems is TFPSList then
        TFPSList(RawItems).Capacity := 64;

      while Lexer.Token <> vtCloseSqBracket do
      { we always look now at "]" or next single value }
      begin
        SingleItem.ParseValue(Lexer, Reader);
        RawItemsAdd(SingleItem);

        if Lexer.Token = vtCloseSqBracket then break;

        if Lexer.Version.Major < 2 then
        begin
          Lexer.CheckTokenIs(vtComma);
          Lexer.NextToken;
        end;
      end;

      { Our handling of commas is specified by VRML 1.0 spec:
        - When the list has no items, "[]" is allowed but "[,]" is not.
        - When there are some items on the list, the last item *may*
          be followed by a comma.
        For VRML >= 2.0 this all doesn't matter, comma is just a whitespace
        and Lexer will never return such token. }

      Lexer.NextToken;
    end else
    begin
      { one single field - not enclosed in [] brackets }
      SingleItem.ParseValue(Lexer, Reader);
      RawItemsAdd(SingleItem);
    end;

  finally FreeAndNil(SingleItem) end;
end;

procedure TX3DSimpleMultField.ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TX3DSingleField;
begin
  { This is much easier and simpler in XML encoding than it was
    in classic encoding. We don't have to check for [ and ] tokens,
    comma is ignored (it was only for VRML 1.0 anyway), we just read
    single values up to the end of stream. }

  Clear;

  SingleItem := CreateItemBeforeParse;
  try
    { List size may be increased rapidly during parsing.
      Prepare for it by allocating some size in advance. }
    if RawItems is TFPSList then
      TFPSList(RawItems).Capacity := 64;

    while Lexer.Token <> vtEnd do
    begin
      SingleItem.ParseValue(Lexer, Reader);
      RawItemsAdd(SingleItem);
    end;
  finally FreeAndNil(SingleItem) end;
end;

procedure TX3DSimpleMultField.SaveToStreamValue(Writer: TX3DWriter);
var
  i: integer;
  WriteIndentNextTime: boolean;
  IndentMultiValueFields: boolean;
begin
  case Writer.Encoding of
    xeClassic:
      { The general "for I := ..." code below can handle correctly any RawItems.Count
        value. But for aesthetics, i.e. more clear output for humans,
        I handle the RawItems.Count = 0 and 1 cases separately. }
      case Count of
        0: Writer.Write('[]');
        1: Writer.Write(RawItemToString(0, Writer.Encoding));
        else
          begin
            Writer.Writeln('[');
            Writer.IncIndent;

            { For really long fields, writing indentation before each item
              can cost a significant disk space. So do not indent when
              there are many items. }
            IndentMultiValueFields := Count <= 10;

            WriteIndentNextTime := IndentMultiValueFields;
            for i := 0 to Count-1 do
            begin
              if WriteIndentNextTime then Writer.WriteIndent('');
              Writer.Write(RawItemToString(i, Writer.Encoding) +',');
              { After the last item we always write newline,
                no matter what's SaveToStreamDoNewLineAfterRawItem }
              if (i = Count - 1) or
                 SaveToStreamDoNewLineAfterRawItem(i) then
                begin Writer.Writeln; WriteIndentNextTime := IndentMultiValueFields end else
                begin Writer.Write(' '); WriteIndentNextTime := false; end;
            end;

            Writer.DecIndent;
            Writer.WriteIndent(']');
          end;
      end;
    xeXML:
      for I := 0 to Count - 1 do
      begin
        Writer.Write(RawItemToString(I, Writer.Encoding));
        if I <> Count - 1 then
          Writer.Write(' ');
      end;
    else raise EInternalError.Create('TX3DSimpleMultField.SaveToStreamValue Encoding?');
  end;
end;

function TX3DSimpleMultField.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  result := true;
end;

function TX3DSimpleMultField.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TX3DSimpleMultField) and
    (TX3DSimpleMultField(SecondValue).Count = Count) and
    (TX3DSimpleMultField(SecondValue).ItemClass = ItemClass);
end;

procedure TX3DSimpleMultField.WritelnWarning_InvalidIndex(
  const Index, ACount: Integer);
const
  MaxInvalidIndexWarnings = 10;
begin
  Inc(InvalidIndexWarnings);
  if InvalidIndexWarnings < MaxInvalidIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Invalid index for field %s (%s): index is %d, but we have only %d items', [NiceName, X3DType, Index, ACount])) else
  if InvalidIndexWarnings = MaxInvalidIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Invalid index for field %s (%s) reported for the %dth time. Further warnings regarding this field will not be reported (to avoid wasting time on printing countless warnings...)',
      [NiceName, X3DType, InvalidIndexWarnings]));
end;

{ simple helpful parsing functions ---------------------------------------- }

{ This returns Float, not just Single, because it's used by
  TSFDouble and ParseVector(double version),
  that want double-precision preserved. }
function ParseFloat(Lexer: TX3DLexer): Float;
begin
  Lexer.CheckTokenIs(TokenNumbers, 'float number');
  result := Lexer.TokenFloat;
  Lexer.NextToken;
end;

procedure ParseVector(var Vector: array of Single; Lexer: TX3DLexer); overload;
var
  i: integer;
begin
  for i := 0 to High(Vector) do Vector[i] := ParseFloat(Lexer);
end;

procedure ParseVector(var Vector: array of Double; Lexer: TX3DLexer); overload;
var
  i: integer;
begin
  for i := 0 to High(Vector) do Vector[i] := ParseFloat(Lexer);
end;

function ParseLongWord(Lexer: TX3DLexer): LongWord;
begin
  Lexer.CheckTokenIs(vtInteger);
  result := Lexer.TokenInteger;
  Lexer.NextToken;
end;

{ TSFBool -------------------------------------------------------------------- }

constructor TSFBool.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: boolean);
begin
  inherited Create(AParentNode, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFBool.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure VRML2BooleanIntegerWarning;
  begin
    if Lexer.Version.Major >= 2 then
      WritelnWarning('VRML/X3D', 'In VRML >= 2.0 you cannot express boolean values ' +
        'as 0 (instead of FALSE) or 1 (instead of TRUE)');
  end;

const
  SBoolExpected = 'boolean constant (TRUE, FALSE)';
begin
  Lexer.CheckTokenIs([vtKeyword, vtInteger], SBoolExpected);
  if Lexer.Token = vtKeyword then
  begin
    if Lexer.TokenKeyword = vkTrue then Value := true else
      if Lexer.TokenKeyword = vkFalse then Value := false else
        raise EX3DParserError.Create(Lexer,
          'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end else
  begin
    if Lexer.TokenInteger = 1 then
    begin
      Value := true;
      VRML2BooleanIntegerWarning;
    end else
    if Lexer.TokenInteger = 0 then
    begin
      Value := false;
      VRML2BooleanIntegerWarning;
    end else
      raise EX3DParserError.Create(Lexer,
        'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end;
  Lexer.NextToken;
end;

const
  BoolKeywords: array [TX3DEncoding, boolean] of string =
  ( ('FALSE', 'TRUE'), ('false', 'true') );

procedure TSFBool.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(BoolKeywords[Writer.Encoding, Value]);
end;

function TSFBool.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFBool.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFBool) and
    (TSFBool(SecondValue).Value = Value);
end;

function TSFBool.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFBool) and
    (TSFBool(SecondValue).Value = Value);
end;

procedure TSFBool.Assign(Source: TPersistent);
begin
  if Source is TSFBool then
  begin
    DefaultValue       := TSFBool(Source).DefaultValue;
    DefaultValueExists := TSFBool(Source).DefaultValueExists;
    Value              := TSFBool(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFBool.AssignValue(Source: TX3DField);
begin
  if Source is TSFBool then
  begin
    inherited;
    Value := TSFBool(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFBool.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFBool.X3DType: string;
begin
  Result := 'SFBool';
end;

procedure TSFBool.Send(const AValue: Boolean);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFBool.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFBool.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFBoolEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFFloat ------------------------------------------------------------------- }

procedure TSFFloat.SetValue(const AValue: Single);
begin
  if MustBeNonnegative then
    FValue := Abs(AValue) else
    FValue := AValue;
end;

constructor TSFFloat.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: Single);
begin
  Create(AParentNode, AName, AValue, false);
end;

constructor TSFFloat.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: Single; AMustBeNonnegative: boolean);
begin
  inherited Create(AParentNode, AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue; { Set property, zeby SetValue moglo ew. zmienic Value }
  AssignDefaultValueFromValue;
end;

procedure TSFFloat.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Value := ParseFloat(Lexer);
  if Angle then
    FValue *= Reader.AngleConversionFactor;
end;

procedure TSFFloat.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(FloatToRawStr(Value));
end;

function TSFFloat.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value)
end;

function TSFFloat.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFFloat) and
    (TSFFloat(SecondValue).MustBeNonnegative = MustBeNonnegative) and
    FloatsEqual(TSFFloat(SecondValue).Value, Value, EqualityEpsilon);
end;

function TSFFloat.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFFloat) and
    (TSFFloat(SecondValue).Value = Value);
end;

procedure TSFFloat.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := Lerp(A, (Value1 as TSFFloat).Value, (Value2 as TSFFloat).Value);
end;

function TSFFloat.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFFloat.Assign(Source: TPersistent);
begin
  if Source is TSFFloat then
  begin
    DefaultValue       := TSFFloat(Source).DefaultValue;
    DefaultValueExists := TSFFloat(Source).DefaultValueExists;
    FValue             := TSFFloat(Source).Value;
    FMustBeNonnegative := TSFFloat(Source).MustBeNonnegative;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFFloat.AssignValue(Source: TX3DField);
begin
  if Source is TSFFloat then
  begin
    inherited;
    Value := TSFFloat(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFFloat.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFFloat.X3DType: string;
begin
  Result := 'SFFloat';
end;

procedure TSFFloat.Send(const AValue: Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFFloat.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFFloat.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFFloatEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFDouble -------------------------------------------------------------------- }

constructor TSFDouble.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: Double);
begin
  inherited Create(AParentNode, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFDouble.SetValue(const AValue: Double);
begin
  FValue := AValue;
end;

procedure TSFDouble.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Value := ParseFloat(Lexer);
  if Angle then
    FValue *= Reader.AngleConversionFactor;
end;

procedure TSFDouble.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(FloatToRawStr(Value));
end;

function TSFDouble.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFDouble.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFDouble) and
    FloatsEqual(TSFDouble(SecondValue).Value, Value, EqualityEpsilon);
end;

function TSFDouble.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFDouble) and
    (TSFDouble(SecondValue).Value = Value);
end;

procedure TSFDouble.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := Lerp(A, (Value1 as TSFDouble).Value, (Value2 as TSFDouble).Value);
end;

function TSFDouble.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFDouble.Assign(Source: TPersistent);
begin
  if Source is TSFDouble then
  begin
    DefaultValue       := TSFDouble(Source).DefaultValue;
    DefaultValueExists := TSFDouble(Source).DefaultValueExists;
    FValue             := TSFDouble(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFDouble.AssignValue(Source: TX3DField);
begin
  if Source is TSFDouble then
  begin
    inherited;
    Value := TSFDouble(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFDouble.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFDouble.X3DType: string;
begin
  Result := 'SFDouble';
end;

procedure TSFDouble.Send(const AValue: Double);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFDouble.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFDouble.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFDoubleEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFTime -------------------------------------------------------------------- }

class function TSFTime.X3DType: string;
begin
  Result := 'SFTime';
end;

class function TSFTime.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFTimeEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFTime.Send(const AValue: Double);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFTime.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFImage ------------------------------------------------------------------- }

constructor TSFImage.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: TCastleImage);
begin
  inherited Create(AParentNode, AName);

  if AValue <> nil then
    { use SetValue setter, to free previous value if needed }
    Value := AValue;
end;

constructor TSFImage.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited;

  { Value must be initialized to non-nil. }
  FValue := TRGBImage.Create;
end;

destructor TSFImage.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TSFImage.SetValue(const AValue: TCastleImage);
begin
  if FValue <> AValue then
  begin
    FreeAndNil(FValue);
    FValue := AValue;
  end;
end;

procedure DecodeImageColor(const Pixel: LongWord; var G: Byte);
begin
  G := Pixel and $FF;
end;

procedure DecodeImageColor(const Pixel: LongWord; var GA: TVector2Byte);
begin
  GA[0] := (pixel shr 8) and $FF;
  GA[1] := pixel and $FF;
end;

procedure DecodeImageColor(const Pixel: LongWord; var RGB: TVector3Byte);
begin
  RGB[0] := (pixel shr 16) and $FF;
  RGB[1] := (pixel shr 8) and $FF;
  RGB[2] := pixel and $FF;
end;

procedure DecodeImageColor(const Pixel: LongWord; var RGBA: TVector4Byte);
begin
  RGBA[0] := (pixel shr 24) and $FF;
  RGBA[1] := (pixel shr 16) and $FF;
  RGBA[2] := (pixel shr 8) and $FF;
  RGBA[3] := pixel and $FF;
end;

{ We have to turn range checking off, because converting from LongInt
  to LongWord below may cause range check errors. Yes, we want to
  directly treat LongInt as 4 bytes here, because DecodeImageColor
  works on separate bytes. See
  http://castle-engine.sourceforge.net/x3d_implementation_texturing3d.php
  comments about PixelTexture3D. }

{$include norqcheckbegin.inc}

procedure DecodeImageColor(const Pixel: LongInt; var G: Byte);
begin
  DecodeImageColor(LongWord(Pixel), G);
end;

procedure DecodeImageColor(const Pixel: LongInt; var GA: TVector2Byte);
begin
  DecodeImageColor(LongWord(Pixel), GA);
end;

procedure DecodeImageColor(const Pixel: LongInt; var RGB: TVector3Byte);
begin
  DecodeImageColor(LongWord(Pixel), RGB);
end;

procedure DecodeImageColor(const Pixel: LongInt; var RGBA: TVector4Byte);
begin
  DecodeImageColor(LongWord(Pixel), RGBA);
end;

{$include norqcheckend.inc}

procedure TSFImage.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  w, h, comp: LongWord;
  i: Cardinal;
  RGBPixels: PArray_Vector3Byte;
  RGBAlphaPixels: PArray_Vector4Byte;
  GrayscalePixels: PByteArray;
  GrayscaleAlphaPixels: PArray_Vector2Byte;
begin
  { Note that we should never let Value to be nil too long,
    because even if this method exits with exception, Value should
    always remain non-nil.
    That's why I'm doing below Value.Empty instead of FreeAndNil(Value).
    This way if e.g. TRGBImage.Create with out of mem exception,
    Value will still remain non-nil.
  }

  Value.Empty;

  w := ParseLongWord(Lexer);
  h := ParseLongWord(Lexer);
  comp := ParseLongWord(Lexer);

  { If w or h =0 then w*h = 0 so we don't have to read anything more.
    We leave Value.IsEmpty in this case. }
  if (w <> 0) and (h <> 0) then
  begin
    case comp of
      1:begin
          Value := TGrayscaleImage.Create(w, h);
          GrayscalePixels := PByteArray(Value.RawPixels);
          for i := 0 to W * H - 1 do
            DecodeImageColor(ParseLongWord(Lexer), GrayscalePixels^[I]);
        end;
      2:begin
          Value := TGrayscaleAlphaImage.Create(w, h);
          GrayscaleAlphaPixels := PArray_Vector2Byte(Value.RawPixels);
          for i := 0 to W * H - 1 do
            DecodeImageColor(ParseLongWord(Lexer), GrayscaleAlphaPixels^[i]);
        end;
      3:begin
          Value := TRGBImage.Create(w, h);
          RGBPixels := PArray_Vector3Byte(Value.RawPixels);
          for i := 0 to W * H - 1 do
            DecodeImageColor(ParseLongWord(Lexer), RGBPixels^[i]);
        end;
      4:begin
          Value := TRGBAlphaImage.Create(w, h);
          RGBAlphaPixels := PArray_Vector4Byte(Value.RawPixels);
          for i := 0 to W * H - 1 do
            DecodeImageColor(ParseLongWord(Lexer), RGBAlphaPixels^[i]);
        end;
      else raise EX3DParserError.Create(Lexer, Format('Invalid components count'+
             ' for SFImage : is %d, should be 1, 2, 3 or 4.',[comp]));
    end;
  end;
end;

procedure TSFImage.SaveToStreamValue(Writer: TX3DWriter);
var
  ga: TVector2Byte;
  rgb: TVector3Byte;
  rgba: TVector4Byte;
  i: Cardinal;
  pixel: LongWord;
begin
  if Value.IsEmpty then
    Writer.Write('0 0 1') else
  begin
    Writer.Writeln(Format('%d %d %d', [
      Value.Width,
      Value.Height,
      Value.ColorComponentsCount]));
    Writer.IncIndent;
    Writer.WriteIndent('');
    {$I NoRQCheckBegin.inc}
    if Value is TGrayscaleImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        pixel := TGrayscaleImage(Value).GrayscalePixels[i];
        Writer.Write(Format('0x%.2x ', [pixel]));
      end;
    end else
    if Value is TGrayscaleAlphaImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        ga := TGrayscaleAlphaImage(Value).GrayscaleAlphaPixels[i];
        pixel := (ga[0] shl 8) or ga[1];
        Writer.Write(Format('0x%.4x ', [pixel]));
      end;
    end else
    if Value is TRGBImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        rgb := TRGBImage(Value).RGBPixels[i];
        pixel := (rgb[0] shl 16) or (rgb[1] shl 8) or rgb[2];
        Writer.Write(Format('0x%.6x ', [pixel]));
      end;
    end else
    if Value is TRGBAlphaImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        rgba := TRGBAlphaImage(Value).AlphaPixels[i];
        pixel := (rgba[0] shl 24) or (rgba[1] shl 16) or (rgba[2] shl 8) or rgba[3];
        Writer.Write(Format('0x%.8x ', [pixel]));
      end;
    end else
      raise Exception.Create('TSFImage.SaveToStreamValue - not implemented TCastleImage descendant');
    {$I NoRQCheckEnd.inc}
    Writer.DecIndent;
  end;
end;

function TSFImage.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFImage) and
    { TODO: compare values
    (TSFImage(SecondValue).Value = Value) }true;
end;

procedure TSFImage.Assign(Source: TPersistent);
begin
  if Source is TSFImage then
  begin
    Value := TSFImage(Source).Value.MakeCopy;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFImage.AssignValue(Source: TX3DField);
begin
  if Source is TSFImage then
  begin
    inherited;
    Value := TSFImage(Source).Value.MakeCopy;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFImage.X3DType: string;
begin
  Result := 'SFImage';
end;

class function TSFImage.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFImageEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFLong -------------------------------------------------------------------- }

procedure TSFLong.SetValue(const AValue: Longint);
begin
  if MustBeNonnegative then
    FValue := Abs(AValue) else
    FValue := AValue;
end;

constructor TSFLong.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: Longint);
begin
  Create(AParentNode, AName, AValue, false);
end;

constructor TSFLong.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: Longint; AMustBeNonnegative: boolean);
begin
  inherited Create(AParentNode, AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue; { Set using property, zeby SetValue moglo ew. zmienic Value }
  AssignDefaultValueFromValue;
end;

procedure TSFLong.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Lexer.CheckTokenIs(vtInteger);

  { Check is TokenInteger outside of 32-bit range. }
  if (Lexer.TokenInteger >= Low(LongInt)) and
     (Lexer.TokenInteger <= High(LongInt)) then
  begin
    Value := Lexer.TokenInteger;
  end else
  begin
    WritelnWarning('VRML/X3D', Format('Integer in the file is out of 32-bit range: %d',
      [Lexer.TokenInteger]));
    Value := -1;
  end;

  Lexer.NextToken;
end;

procedure TSFLong.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(IntToStr(Value));
end;

function TSFLong.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value)
end;

function TSFLong.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  { Note that this means that SFInt32 and SFLong will actually be considered
    equal. That's Ok, we want this. }
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFLong) and
    (TSFLong(SecondValue).MustBeNonnegative = MustBeNonnegative) and
    (TSFLong(SecondValue).Value = Value);
end;

function TSFLong.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFLong) and
    (TSFLong(SecondValue).Value = Value);
end;

procedure TSFLong.Assign(Source: TPersistent);
begin
  if Source is TSFLong then
  begin
    DefaultValue       := TSFLong(Source).DefaultValue;
    DefaultValueExists := TSFLong(Source).DefaultValueExists;
    FValue             := TSFLong(Source).Value;
    FMustBeNonnegative := TSFLong(Source).MustBeNonnegative;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFLong.AssignValue(Source: TX3DField);
begin
  if Source is TSFLong then
  begin
    inherited;
    Value := TSFLong(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFLong.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFLong.X3DType: string;
begin
  Result := 'SFLong';
end;

procedure TSFLong.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFLong.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFLong.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFLongEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFInt32 ------------------------------------------------------------------- }

class function TSFInt32.X3DType: string;
begin
  Result := 'SFInt32';
end;

procedure TSFInt32.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFInt32.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFInt32.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFInt32Event.Create(AParentNode, AName, AInEvent);
end;

{ ----------------------------------------------------------------------------
  Common SF fields based on matrices implementation }

{$define IMPLEMENT_SF_CLASS_USING_MATRICES :=
constructor TSF_CLASS.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: TSF_STATIC_ITEM);
begin
  inherited Create(AParentNode, AName);
  FValue := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSF_CLASS.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  Column: integer;
begin
  for Column := 0 to TSF_MATRIX_COLS - 1 do
    ParseVector(FValue[Column], Lexer);
end;

procedure TSF_CLASS.SaveToStreamValue(Writer: TX3DWriter);
var
  Column: integer;
begin
  Writer.Writeln(VectorToRawStr(FValue[0]));

  Writer.IncIndent;
  for Column := 1 to TSF_MATRIX_COLS - 1 do
    Writer.WritelnIndent(VectorToRawStr(FValue[Column]));
  Writer.DecIndent;
end;

function TSF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSF_CLASS) and
   MatricesEqual(TSF_CLASS(SecondValue).FValue, FValue, EqualityEpsilon);
end;

function TSF_CLASS.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSF_CLASS) and
    MatricesPerfectlyEqual(TSF_CLASS(SecondValue).Value, Value);
end;

procedure TSF_CLASS.AssignLerp(const A: Double; Value1, Value2: TX3DField);
var
  Column: integer;
  M1, M2: PSF_STATIC_ITEM;
begin
  M1 := @((Value1 as TSF_CLASS).FValue);
  M2 := @((Value2 as TSF_CLASS).FValue);
  for Column := 0 to TSF_MATRIX_COLS - 1 do
    FValue[Column] := Lerp(A, M1^[Column], M2^[Column]);
end;

function TSF_CLASS.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSF_CLASS.Assign(Source: TPersistent);
begin
  if Source is TSF_CLASS then
  begin
    FValue := TSF_CLASS(Source).FValue;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSF_CLASS.AssignValue(Source: TX3DField);
begin
  if Source is TSF_CLASS then
  begin
    inherited;
    FValue := TSF_CLASS(Source).FValue;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

function TSF_CLASS.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and
    MatricesPerfectlyEqual(DefaultValue, Value);
end;

procedure TSF_CLASS.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSF_CLASS.Send(const AValue: TSF_STATIC_ITEM);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSF_CLASS.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSF_CLASS.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSF_EVENT.Create(AParentNode, AName, AInEvent);
end;
}

{$define TSF_CLASS := TSFMatrix3f}
{$define TSF_STATIC_ITEM := TMatrix3Single}
{$define PSF_STATIC_ITEM := PMatrix3Single}
{$define TSF_MATRIX_COLS := 3}
{$define TSF_SCALAR := Single}
{$define TSF_EVENT := TSFMatrix3fEvent}
IMPLEMENT_SF_CLASS_USING_MATRICES

{$define TSF_CLASS := TSFMatrix3d}
{$define TSF_STATIC_ITEM := TMatrix3Double}
{$define PSF_STATIC_ITEM := PMatrix3Double}
{$define TSF_MATRIX_COLS := 3}
{$define TSF_SCALAR := Double}
{$define TSF_EVENT := TSFMatrix3dEvent}
IMPLEMENT_SF_CLASS_USING_MATRICES

{$define TSF_CLASS := TSFMatrix4f}
{$define TSF_STATIC_ITEM := TMatrix4Single}
{$define PSF_STATIC_ITEM := PMatrix4Single}
{$define TSF_MATRIX_COLS := 4}
{$define TSF_SCALAR := Single}
{$define TSF_EVENT := TSFMatrix4fEvent}
IMPLEMENT_SF_CLASS_USING_MATRICES

{$define TSF_CLASS := TSFMatrix4d}
{$define TSF_STATIC_ITEM := TMatrix4Double}
{$define PSF_STATIC_ITEM := PMatrix4Double}
{$define TSF_MATRIX_COLS := 4}
{$define TSF_SCALAR := Double}
{$define TSF_EVENT := TSFMatrix4dEvent}
IMPLEMENT_SF_CLASS_USING_MATRICES

{ TSFMatrix3f ------------------------------------------------------------------ }

class function TSFMatrix3f.X3DType: string;
begin
  Result := 'SFMatrix3f';
end;

{ TSFMatrix3d ------------------------------------------------------------------ }

class function TSFMatrix3d.X3DType: string;
begin
  Result := 'SFMatrix3d';
end;

{ TSFMatrix4f ------------------------------------------------------------------ }

class function TSFMatrix4f.X3DType: string;
begin
  Result := 'SFMatrix4f';
end;

function TSFMatrix4f.TransformScale: Single;
begin
  { This is a simple method of extracting average scaling factor from
    a matrix. Works OK for combination of identity, scaling,
    translation matrices.
    Fails awfully on rotation (and possibly many other) matrices. }
  Result := Approximate3DScale(
    FValue[0, 0],
    FValue[1, 1],
    FValue[2, 2]);
end;

{ TSFMatrix4d ------------------------------------------------------------------ }

class function TSFMatrix4d.X3DType: string;
begin
  Result := 'SFMatrix4d';
end;

{ TSFMatrix ------------------------------------------------------------------ }

class function TSFMatrix.X3DType: string;
begin
  Result := 'SFMatrix';
end;

procedure TSFMatrix.Send(const AValue: TMatrix4Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFMatrix.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFRotation ---------------------------------------------------------------- }

constructor TSFRotation.Create(AParentNode: TX3DFileItem;
  const AName: string;
  const AnAxis: TVector3Single; const ARotationRad: Single);
begin
  inherited Create(AParentNode, AName);

  Axis := AnAxis;
  RotationRad := ARotationRad;

  AssignDefaultValueFromValue;
end;

constructor TSFRotation.Create(AParentNode: TX3DFileItem;
  const AName: string;
  const AValue: TVector4Single);
var
  AnAxis: TVector3Single absolute AValue;
begin
  inherited Create(AParentNode, AName);

  Axis := AnAxis;
  RotationRad := AValue[3];

  AssignDefaultValueFromValue;
end;

procedure TSFRotation.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  ParseVector(Axis, Lexer);
  RotationRad := ParseFloat(Lexer) * Reader.AngleConversionFactor;
end;

function TSFRotation.GetValue: TVector4Single;
begin
  Move(Axis[0], Result[0], SizeOf(Single) * 3);
  Result[3] := RotationRad;
end;

procedure TSFRotation.SetValue(const AValue: TVector4Single);
begin
  Axis[0] := AValue[0];
  Axis[1] := AValue[1];
  Axis[2] := AValue[2];
  RotationRad := AValue[3];
end;

function TSFRotation.GetValueDeg: TVector4Single;
begin
  Move(Axis[0], Result[0], SizeOf(Single) * 3);
  Result[3] := RadToDeg(RotationRad);
end;

procedure TSFRotation.SetValueDeg(const AValue: TVector4Single);
begin
  Axis[0] := AValue[0];
  Axis[1] := AValue[1];
  Axis[2] := AValue[2];
  RotationRad := DegToRad(AValue[3]);
end;

procedure TSFRotation.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(VectorToRawStr(Axis) +' ' +FloatToRawStr(RotationRad));
end;

function TSFRotation.RotatedPoint(const pt: TVector3Single): TVector3Single;
begin
  if not ZeroVector(Axis) then
    Result := RotatePointAroundAxisRad(RotationRad, pt, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML spec says that SFRotation should always specify a normalized vector. }
    Result := Pt;
    WritelnWarning('VRML/X3D', Format('SFRotation field (%s) specifies rotation around zero vector', [NiceName]));
  end;
end;

function TSFRotation.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFRotation) and
    VectorsEqual(TSFRotation(SecondValue).Axis, Axis, EqualityEpsilon) and
    FloatsEqual(TSFRotation(SecondValue).RotationRad, RotationRad, EqualityEpsilon);
end;

function TSFRotation.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFRotation) and
    VectorsPerfectlyEqual(TSFRotation(SecondValue).Axis, Axis) and
    (TSFRotation(SecondValue).RotationRad = RotationRad);
end;

function TSFRotation.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and
    VectorsPerfectlyEqual(DefaultAxis, Axis) and
    (DefaultRotationRad = RotationRad);
end;

procedure TSFRotation.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  { interpolate using slerp (testcase when linear interpolation on axis/vector fails:
    god triangle in escape_universe) }
  Value := SLerp(A, (Value1 as TSFRotation).Value, (Value2 as TSFRotation).Value);
end;

function TSFRotation.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFRotation.Assign(Source: TPersistent);
begin
  if Source is TSFRotation then
  begin
    Axis        := TSFRotation(Source).Axis;
    RotationRad := TSFRotation(Source).RotationRad;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFRotation.AssignValue(Source: TX3DField);
begin
  if Source is TSFRotation then
  begin
    inherited;
    Axis := TSFRotation(Source).Axis;
    RotationRad := TSFRotation(Source).RotationRad;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFRotation.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultAxis := Axis;
  DefaultRotationRad := RotationRad;
  DefaultValueExists := true;
end;

class function TSFRotation.X3DType: string;
begin
  Result := 'SFRotation';
end;

procedure TSFRotation.Send(const AValue: Tvector4Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFRotation.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFRotation.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFRotationEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFString ------------------------------------------------------------------ }

constructor TSFString.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: string);
begin
  inherited Create(AParentNode, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFString.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Lexer.CheckTokenIs(vtString);
  Value := Lexer.TokenString;
  Lexer.NextToken;
end;

procedure TSFString.SaveToStreamValue(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic: Writer.Write(StringToX3DClassic(Value));
    xeXML    : Writer.Write(StringToX3DXml(Value));
    else raise EInternalError.Create('TSFString.SaveToStreamValue Encoding?');
  end;
end;

function TSFString.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFString.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFString) and
    (TSFString(SecondValue).Value = Value);
end;

function TSFString.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFString) and
    (TSFString(SecondValue).Value = Value);
end;

procedure TSFString.Assign(Source: TPersistent);
begin
  if Source is TSFString then
  begin
    DefaultValue       := TSFString(Source).DefaultValue;
    DefaultValueExists := TSFString(Source).DefaultValueExists;
    Value              := TSFString(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFString.AssignValue(Source: TX3DField);
begin
  if Source is TSFString then
  begin
    inherited;
    Value := TSFString(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFString.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFString.X3DType: string;
begin
  Result := 'SFString';
end;

procedure TSFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
begin
  { SFString has quite special interpretation, it's just attrib
    name. It would not be usefull trying to use TX3DLexer here,
    it's easier just to handle this as a special case.

    Uhm... some X3D XML files commit the reverse mistake
    as for MFString: they *include* additional quotes around the string.
    Spec says that for SFString, such quotes are not needed.
    Example: openlibraries trunk/media files.

    I detect this, warn and strip quotes. }
  if (Length(AttributeValue) >= 2) and
     (AttributeValue[1] = '"') and
     (AttributeValue[Length(AttributeValue)] = '"') then
  begin
    WritelnWarning('VRML/X3D', 'X3D XML: found extra quotes around SFString value. Assuming this is a mistake, and stripping quotes from ''' + AttributeValue + '''. Fix your model: SFString field values should not be enclosed in extra quotes!');
    Value := Copy(AttributeValue, 2, Length(AttributeValue) - 2);
  end else
    Value := AttributeValue;
end;

procedure TSFString.Send(const AValue: AnsiString);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFString.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

function TSFString.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttributeCustomQuotes;
end;

class function TSFString.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFStringEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFString.SetValue(const NewValue: string);
begin
  FValue := NewValue;
end;

procedure TSFString.SetDefaultValue(const NewDefaultValue: string);
begin
  FDefaultValue := NewDefaultValue;
end;

{ TSFStringEnum -------------------------------------------------------------- }

constructor TSFStringEnum.Create(AParentNode: TX3DFileItem;
  const AName: string; const AEnumNames: array of string; const AValue: integer);
begin
  FEnumNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AEnumNames, FEnumNames);

  inherited Create(AParentNode, AName, FEnumNames[AValue]);
  { inherited Create will assign Value, and in SetValue should cause setting
    our FEnumValue }
  Assert(AValue = FEnumValue);
end;

destructor TSFStringEnum.Destroy;
begin
  FreeAndNil(FEnumNames);
  inherited;
end;

class function TSFStringEnum.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFString;
end;

function TSFStringEnum.StringToEnumValue(const NewValue: string): Integer;
var
  UpperValue: string;
begin
  UpperValue := UpperCase(NewValue);
  if UpperValue <> NewValue then
    WritelnWarning('VRML/X3D', Format('Field "%s" value should be uppercase, but is not: "%s"',
      [X3DName, NewValue]));

  Result := FEnumNames.IndexOf(UpperValue);
  if Result = -1 then
  begin
    Result := DefaultEnumValue;
    WritelnWarning('VRML/X3D', Format('Unknown "%s" field value: "%s"',
      [X3DName, NewValue]));
  end;
end;

procedure TSFStringEnum.SetValue(const NewValue: string);
begin
  inherited SetValue(NewValue);
  { calculate new FEnumValue, IOW convert string NewValue to integer }
  FEnumValue := StringToEnumValue(NewValue);
end;

procedure TSFStringEnum.SetEnumValue(const NewEnumValue: Integer);
begin
  inherited SetValue(FEnumNames[NewEnumValue]);
  FEnumValue := NewEnumValue;
end;

procedure TSFStringEnum.SendEnumValue(const NewValue: Integer);
begin
  inherited Send(FEnumNames[NewValue]);
end;

procedure TSFStringEnum.SetDefaultValue(const NewDefaultValue: string);
begin
  inherited SetDefaultValue(NewDefaultValue);
  FDefaultEnumValue := StringToEnumValue(NewDefaultValue);
end;

procedure TSFStringEnum.SetDefaultEnumValue(const NewDefaultEnumValue: Integer);
begin
  inherited SetDefaultValue(FEnumNames[NewDefaultEnumValue]);
  FDefaultEnumValue := NewDefaultEnumValue;
end;

{ ----------------------------------------------------------------------------
  Common SF fields based on vectors implementation }

{$define IMPLEMENT_SF_CLASS_USING_VECTORS :=
constructor TSF_CLASS.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: TSF_STATIC_ITEM);
begin
  inherited Create(AParentNode, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSF_CLASS.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  ParseVector(Value, Lexer);
end;

procedure TSF_CLASS.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(VectorToRawStr(Value));
end;

function TSF_CLASS.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and VectorsPerfectlyEqual(DefaultValue, Value);
end;

function TSF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSF_CLASS) and
    VectorsEqual(TSF_CLASS(SecondValue).Value, Value, EqualityEpsilon);
end;

function TSF_CLASS.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSF_CLASS) and
    VectorsPerfectlyEqual(TSF_CLASS(SecondValue).Value, Value);
end;

procedure TSF_CLASS.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := Lerp(A, (Value1 as TSF_CLASS).Value, (Value2 as TSF_CLASS).Value);
end;

function TSF_CLASS.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSF_CLASS.Assign(Source: TPersistent);
begin
  if Source is TSF_CLASS then
  begin
    DefaultValue       := TSF_CLASS(Source).DefaultValue;
    DefaultValueExists := TSF_CLASS(Source).DefaultValueExists;
    Value              := TSF_CLASS(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSF_CLASS.AssignValue(Source: TX3DField);
begin
  if Source is TSF_CLASS then
  begin
    inherited;
    Value := TSF_CLASS(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSF_CLASS.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSF_CLASS.Send(const AValue: TSF_STATIC_ITEM);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSF_CLASS.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSF_CLASS.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSF_EVENT.Create(AParentNode, AName, AInEvent);
end;
}

{$define TSF_CLASS := TSFVec2f}
{$define TSF_SCALAR := Single}
{$define TSF_STATIC_ITEM := TVector2Single}
{$define TSF_EVENT := TSFVec2fEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec3f}
{$define TSF_SCALAR := Single}
{$define TSF_STATIC_ITEM := TVector3Single}
{$define TSF_EVENT := TSFVec3fEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec4f}
{$define TSF_SCALAR := Single}
{$define TSF_STATIC_ITEM := TVector4Single}
{$define TSF_EVENT := TSFVec4fEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec2d}
{$define TSF_SCALAR := Double}
{$define TSF_STATIC_ITEM := TVector2Double}
{$define TSF_EVENT := TSFVec2dEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec3d}
{$define TSF_SCALAR := Double}
{$define TSF_STATIC_ITEM := TVector3Double}
{$define TSF_EVENT := TSFVec3dEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec4d}
{$define TSF_SCALAR := Double}
{$define TSF_STATIC_ITEM := TVector4Double}
{$define TSF_EVENT := TSFVec4dEvent}
IMPLEMENT_SF_CLASS_USING_VECTORS

{ TSFVec2f ------------------------------------------------------------------- }

class function TSFVec2f.X3DType: string;
begin
  Result := 'SFVec2f';
end;

{ TSFVec3f ------------------------------------------------------------------- }

class function TSFVec3f.X3DType: string;
begin
  Result := 'SFVec3f';
end;

procedure TSFVec3f.Send(const Index: Integer; const ComponentValue: Single);
var
  V: TVector3Single;
begin
  V := Value;
  V[Index] := ComponentValue;
  Send(V);
end;

{ TSFColor ------------------------------------------------------------------- }

class function TSFColor.X3DType: string;
begin
  Result := 'SFColor';
end;

procedure TSFColor.Send(const AValue: TVector3Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFColor.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFColor.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFColorEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFVec4f ------------------------------------------------------------------- }

class function TSFVec4f.X3DType: string;
begin
  Result := 'SFVec4f';
end;

{ TSFColorRGBA --------------------------------------------------------------- }

class function TSFColorRGBA.X3DType: string;
begin
  Result := 'SFColorRGBA';
end;

procedure TSFColorRGBA.Send(const AValue: TVector4Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFColorRGBA.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFColorRGBA.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFColorRGBAEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFVec2d ------------------------------------------------------------------- }

class function TSFVec2d.X3DType: string;
begin
  Result := 'SFVec2d';
end;

{ TSFVec3d ------------------------------------------------------------------- }

class function TSFVec3d.X3DType: string;
begin
  Result := 'SFVec3d';
end;

{ TSFVec4d ------------------------------------------------------------------- }

class function TSFVec4d.X3DType: string;
begin
  Result := 'SFVec4d';
end;

{ TSFBitMask ------------------------------------------------------------ }

constructor TSFBitMask.Create(AParentNode: TX3DFileItem;
  const AName: string; const AFlagNames: array of string;
  const ANoneString, AAllString: string; const AFlags: array of boolean);
var
  i: integer;
begin
  inherited Create(AParentNode, AName);

  fFlagNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AFlagNames, fFlagNames);
  for i := 0 to FlagsCount-1 do Flags[i] := AFlags[i];
  fNoneString := ANoneString;
  fAllString := AAllString;

  Assert(NoneString <> '', 'NoneString must be defined for SFBitMask');
end;

destructor TSFBitMask.Destroy;
begin
  fFlagNames.Free;
  inherited;
end;

function TSFBitMask.GetFlags(i: integer): boolean;
begin
  result := i in fFlags
end;

procedure TSFBitMask.SetFlags(i: integer; value: boolean);
begin
  if value then Include(fFlags, i) else Exclude(fFlags, i)
end;

function TSFBitMask.FlagsCount: integer;
begin
  result := fFlagNames.Count
end;

function TSFBitMask.GetFlagNames(i: integer): string;
begin
  result := fFlagNames[i]
end;

procedure TSFBitMask.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure InterpretTokenAsFlagName;
  var
    i: integer;
  begin
    Lexer.CheckTokenIs(vtName, 'bit mask constant');
    i := fFlagNames.IndexOf(Lexer.TokenName);
    if i >= 0 then
      Flags[i] := true else
    if Lexer.TokenName = fAllString then
      fFlags := [0..FlagsCount-1] else
    if Lexer.TokenName = fNoneString then
      { Don't set anything. Note that this doesn't clear other flags,
        so e.g. "( FLAG_1 | NONE )" equals just "FLAG_1". } else
      raise EX3DParserError.Create(Lexer,
        'Expected bit mask constant, got '+Lexer.DescribeToken);
  end;

begin
  fFlags:=[];

  if Lexer.Token = vtOpenBracket then
  begin
    repeat
      Lexer.NextToken;
      InterpretTokenAsFlagName;
      Lexer.NextToken;
    until Lexer.Token <> vtBar;
    Lexer.CheckTokenIs(vtCloseBracket);
    Lexer.NextToken;
  end else
  begin
    InterpretTokenAsFlagName;
    Lexer.NextToken;
  end;
end;

function TSFBitMask.AreAllFlags(value: boolean): boolean;
var
  i: integer;
begin
  for i := 0 to FlagsCount-1 do
    if Flags[i] <> value then exit(false);
  exit(true);
end;

procedure TSFBitMask.SaveToStreamValue(Writer: TX3DWriter);
var
  i: integer;
  PrecedeWithBar: boolean;
begin
  { This is an VRML 1.0 (and Inventor) type. The existing specs only say
    how to encode it for classic encoding. For XML, we just use the same format. }
  if AreAllFlags(false) then
    Writer.Write(NoneString) else
  begin
    { We don't really need AllString to express that all bit are set
      (we could as well just name them all), but it looks nicer. }
    if (AllString <> '') and AreAllFlags(true) then
      Writer.Write(AllString) else
    begin
      PrecedeWithBar := false;
      Writer.Write('(');
      for i := 0 to FlagsCount-1 do
        if Flags[i] then
        begin
          if PrecedeWithBar then Writer.Write('|') else PrecedeWithBar := true;
          Writer.Write(FlagNames[i]);
        end;
      Writer.Write(')');
    end;
  end;
end;

function TSFBitMask.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFBitMask) and
    (TSFBitMask(SecondValue).FFlagNames.Equals(FFlagNames)) and
    (TSFBitMask(SecondValue).FFlags = FFlags) and
    (TSFBitMask(SecondValue).AllString = AllString) and
    (TSFBitMask(SecondValue).NoneString = NoneString);
end;

procedure TSFBitMask.Assign(Source: TPersistent);
begin
  if Source is TSFBitMask then
  begin
    FAllString  := TSFBitMask(Source).AllString;
    FNoneString := TSFBitMask(Source).NoneString;
    FFlags      := TSFBitMask(Source).FFlags;
    FFlagNames.Assign(TSFBitMask(Source).FFlagNames);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFBitMask.AssignValue(Source: TX3DField);
begin
  if Source is TSFBitMask then
  begin
    inherited;
    FFlags := TSFBitMask(Source).FFlags;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFBitMask.X3DType: string;
begin
  Result := 'SFBitMask';
end;

{ TSFEnum ----------------------------------------------------------------- }

constructor TSFEnum.Create(AParentNode: TX3DFileItem;
  const AName: string; const AEnumNames: array of string; const AValue: integer);
begin
  inherited Create(AParentNode, AName);

  FEnumNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AEnumNames, FEnumNames);
  Value := AValue;
  AssignDefaultValueFromValue;
end;

destructor TSFEnum.Destroy;
begin
  FreeAndNil(FEnumNames);
  inherited;
end;

function TSFEnum.GetEnumNames(i: integer): string;
begin
  result := FEnumNames[i]
end;

function TSFEnum.EnumNamesCount: integer;
begin
  result := FEnumNames.Count
end;

procedure TSFEnum.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  val: integer;
begin
  Lexer.CheckTokenIs(vtName, 'enumerated type constant');
  val := FEnumNames.IndexOf(Lexer.TokenName);
  if val = -1 then
   raise EX3DParserError.Create(Lexer,
     'Expected enumerated type constant, got '+Lexer.DescribeToken);
  Value := val;
  Lexer.NextToken;
end;

procedure TSFEnum.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(EnumNames[Value]);
end;

function TSFEnum.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFEnum.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSFEnum) and
    (TSFEnum(SecondValue).FEnumNames.Equals(FEnumNames)) and
    (TSFEnum(SecondValue).Value = Value);
end;

procedure TSFEnum.Assign(Source: TPersistent);
begin
  if Source is TSFEnum then
  begin
    DefaultValue       := TSFEnum(Source).DefaultValue;
    DefaultValueExists := TSFEnum(Source).DefaultValueExists;
    Value              := TSFEnum(Source).Value;
    FEnumNames.Assign(TSFEnum(Source).FEnumNames);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFEnum.AssignValue(Source: TX3DField);
begin
  if Source is TSFEnum then
  begin
    inherited;
    Value := TSFEnum(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFEnum.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFEnum.X3DType: string;
begin
  Result := 'SFEnum';
end;

procedure TSFEnum.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFEnum.Create(ParentNode, X3DName, [], AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ multiple value fields ----------------------------------------------------- }

{ Note that because of FPC 2.0.2 bug, code below will not compile
  with FPC 2.0.2 in objfpc mode. For objfpc mode I would have to
  change below Items.Items[I] to Items.L[I],
  i.e. Items property of my dynamic array classes will not work
  correctly in objfpc mode in FPC 2.0.2.
  Fixed in FPC 2.0.3 and 2.1.1 (revision 2911).
}

{$ifdef FPC_OBJFPC}
  {$ifdef VER2_0_2}
    {$fatal This code will not compile with FPC 2.0.2 in objfpc mode}
  {$endif}
{$endif}

{$define IMPLEMENT_MF_CLASS:=
constructor TMF_CLASS.Create(AParentNode: TX3DFileItem;
  const AName: string;
  const InitialContent: array of TMF_STATIC_ITEM);
begin
  inherited Create(AParentNode, AName);

  Items.AddArray(InitialContent);

  AssignDefaultValueFromValue;
end;

constructor TMF_CLASS.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited;

  FItemClass := TMF_CLASS_ITEM;

  RawItems := TMF_DYN_STATIC_ITEM_ARRAY.Create;

  DefaultValuesCount := -1;
end;

function TMF_CLASS.GetItems: TMF_DYN_STATIC_ITEM_ARRAY;
begin
  Result := TMF_DYN_STATIC_ITEM_ARRAY(RawItems)
end;

procedure TMF_CLASS.SetItems(const Value: TMF_DYN_STATIC_ITEM_ARRAY);
begin
  TMF_DYN_STATIC_ITEM_ARRAY(RawItems).Assign(Value);
end;

procedure TMF_CLASS.RawItemsAdd(Item: TX3DSingleField);
begin
  Items.Add(TMF_CLASS_ITEM(Item).Value);
end;

procedure TMF_CLASS.Assign(Source: TPersistent);
begin
  if Source is TMF_CLASS then
  begin
    DefaultValuesCount := TMF_CLASS(Source).DefaultValuesCount;
    DefaultValue       := TMF_CLASS(Source).DefaultValue;
    Items.Assign(TMF_CLASS(Source).Items);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TMF_CLASS.AssignValue(Source: TX3DField);
begin
  if Source is TMF_CLASS then
  begin
    inherited;
    Items.Assign(TMF_CLASS(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TMF_CLASS.AssignDefaultValueFromValue;
begin
  inherited;

  (* initialize default value of the field: DefaultValuesCount, and (only
     in case of DefaultValuesCount = 1 for now) initialize also DefaultValue *)
  case Items.Count of
    0: DefaultValuesCount := 0;
    1: begin
         DefaultValuesCount := 1;
         DefaultValue := Items.L[0];
       end;
    else DefaultValuesCount := -1;
  end;
end;

procedure TMF_CLASS.Send(const AValue: array of TMF_STATIC_ITEM);
var
  FieldValue: TX3DField;
begin
  FieldValue := TMF_CLASS.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

function TMF_CLASS.GetItemsSafe(Index: Integer): TMF_STATIC_ITEM;
begin
  if (Index >= 0) and (Index < Items.Count) then
    Result := Items.L[Index] else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
    Result := TMF_DYN_DEFAULT_SAFE_VALUE;
  end;
end;

procedure TMF_CLASS.SetItemsSafe(Index: Integer; const Value: TMF_STATIC_ITEM);
begin
  if (Index >= 0) and (Index < Items.Count) then
    Items.L[Index] := Value else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
  end;
end;

class function TMF_CLASS.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMF_EVENT.Create(AParentNode, AName, AInEvent);
end;
}

{ Choose one of macros IMPLEMENT_MF_CLASS_USING_* to specify
  which comparison operator is suitable for given field type.

  For some types operator "=" may not be available, like for vectors
  (where it would be unsure what it does --- precise or epsilon comparison?).
  Then using other comparison, like CompareMem, may be suitable.
}

{$define IMPLEMENT_MF_CLASS_USING_EQUALITY_OP:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result :=
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
     (DefaultValue = Items.L[0]));
end;

function TMF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not (TMF_CLASS(SecondValue).Items.L[I] = Items.L[I]) then
    Exit(false);
end;
}

{$define IMPLEMENT_MF_CLASS_USING_VECTORS:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result :=
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
      VectorsPerfectlyEqual(DefaultValue, Items.L[0]) );
end;

function TMF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not VectorsEqual(TMF_CLASS(SecondValue).Items.L[I], Items.L[I],
     EqualityEpsilon) then
    Exit(false);
end;

function TMF_CLASS.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := VectorToRawStr(Items.L[ItemNum])
end;

procedure TMF_CLASS.AssignLerp(const A: Double; Value1, Value2: TX3DField);
var
  I: Integer;
  Val1, Val2: TMF_CLASS;
  Items1, Items2: TMF_DYN_STATIC_ITEM_ARRAY;
begin
  Val1 := Value1 as TMF_CLASS;
  Val2 := Value2 as TMF_CLASS;

  Val1.CheckCountEqual(Val2);
  Items.Count := Val1.Items.Count;

  Items1 := Val1.Items;
  Items2 := Val2.Items;

  for I := 0 to Items.Count - 1 do
    Items.L[I] := Lerp(A, Items1.L[I], Items2.L[I]);
end;

function TMF_CLASS.CanAssignLerp: boolean;
begin
  Result := true;
end;
}

{$define IMPLEMENT_MF_CLASS_USING_MATRICES:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result :=
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
      MatricesPerfectlyEqual(DefaultValue, Items.L[0]) );
end;

function TMF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not MatricesEqual(TMF_CLASS(SecondValue).Items.L[I], Items.L[I],
     EqualityEpsilon) then
    Exit(false);
end;

function TMF_CLASS.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
var
  Column: Integer;
begin
  Result := VectorToRawStr(Items.L[ItemNum][0]);
  for Column := 1 to TSF_MATRIX_COLS - 1 do
    Result += ' ' + VectorToRawStr(Items.L[ItemNum][Column]);
end;

procedure TMF_CLASS.AssignLerp(const A: Double; Value1, Value2: TX3DField);
var
  I: Integer;
  Val1, Val2: TMF_CLASS;
  Items1, Items2: TMF_DYN_STATIC_ITEM_ARRAY;
begin
  Val1 := Value1 as TMF_CLASS;
  Val2 := Value2 as TMF_CLASS;

  Val1.CheckCountEqual(Val2);
  Items.Count := Val1.Items.Count;

  Items1 := Val1.Items;
  Items2 := Val2.Items;

  for I := 0 to Items.Count - 1 do
    Items.L[I] := Lerp(A, Items1.L[I], Items2.L[I]);
end;

function TMF_CLASS.CanAssignLerp: boolean;
begin
  Result := true;
end;
}

{$define IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result :=
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
     (DefaultValue = Items.L[0]) );
end;

function TMF_CLASS.Equals(SecondValue: TX3DField;
  const EqualityEpsilon: Double): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not FloatsEqual(TMF_CLASS(SecondValue).Items.L[I], Items.L[I],
     EqualityEpsilon) then
    Exit(false);
end;
}

{$define TMF_CLASS := TMFBool}
{$define TMF_STATIC_ITEM := boolean}
{$define TMF_CLASS_ITEM := TSFBool}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TBooleanList}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := false}
{$define TMF_EVENT := TMFBoolEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{$define TMF_CLASS := TMFLong}
{$define TMF_STATIC_ITEM := Longint}
{$define TMF_CLASS_ITEM := TSFLong}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TLongintList}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := 0}
{$define TMF_EVENT := TMFLongEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{$define TMF_CLASS := TMFVec2f}
{$define TMF_STATIC_ITEM := TVector2Single}
{$define TMF_CLASS_ITEM := TSFVec2f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector2SingleList}
{$define TMF_SCALAR := Single}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := ZeroVector2Single}
{$define TMF_EVENT := TMFVec2fEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec3f}
{$define TMF_STATIC_ITEM := TVector3Single}
{$define TMF_CLASS_ITEM := TSFVec3f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector3SingleList}
{$define TMF_SCALAR := Single}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := ZeroVector3Single}
{$define TMF_EVENT := TMFVec3fEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec4f}
{$define TMF_STATIC_ITEM := TVector4Single}
{$define TMF_CLASS_ITEM := TSFVec4f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector4SingleList}
{$define TMF_SCALAR := Single}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := Vector4Single(0, 0, 0, 1)}
{$define TMF_EVENT := TMFVec4fEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec2d}
{$define TMF_STATIC_ITEM := TVector2Double}
{$define TMF_CLASS_ITEM := TSFVec2d}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector2DoubleList}
{$define TMF_SCALAR := Double}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := ZeroVector2Double}
{$define TMF_EVENT := TMFVec2dEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec3d}
{$define TMF_STATIC_ITEM := TVector3Double}
{$define TMF_CLASS_ITEM := TSFVec3d}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector3DoubleList}
{$define TMF_SCALAR := Double}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := ZeroVector3Double}
{$define TMF_EVENT := TMFVec3dEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec4d}
{$define TMF_STATIC_ITEM := TVector4Double}
{$define TMF_CLASS_ITEM := TSFVec4d}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector4DoubleList}
{$define TMF_SCALAR := Double}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := Vector4Double(0, 0, 0, 1)}
{$define TMF_EVENT := TMFVec4dEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFRotation}
{$define TMF_STATIC_ITEM := TVector4Single}
{$define TMF_CLASS_ITEM := TSFRotation}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TVector4SingleList}
{$define TMF_SCALAR := Single}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := DefaultRotation}
{$define TMF_EVENT := TMFRotationEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFFloat}
{$define TMF_STATIC_ITEM := Single}
{$define TMF_CLASS_ITEM := TSFFloat}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TSingleList}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := 0}
{$define TMF_EVENT := TMFFloatEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL

{$define TMF_CLASS := TMFDouble}
{$define TMF_STATIC_ITEM := Double}
{$define TMF_CLASS_ITEM := TSFDouble}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDoubleList}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := 0}
{$define TMF_EVENT := TMFDoubleEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL

{$define TMF_CLASS := TMFString}
{$define TMF_STATIC_ITEM := string}
{$define TMF_CLASS_ITEM := TSFString}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TCastleStringList}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := ''}
{$define TMF_EVENT := TMFStringEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{$define TMF_CLASS := TMFMatrix3f}
{$define TMF_STATIC_ITEM := TMatrix3Single}
{$define TMF_CLASS_ITEM := TSFMatrix3f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TMatrix3SingleList}
{$define TMF_SCALAR := Single}
{$define TSF_MATRIX_COLS := 3}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := IdentityMatrix3Single}
{$define TMF_EVENT := TMFMatrix3fEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_MATRICES

{$define TMF_CLASS := TMFMatrix3d}
{$define TMF_STATIC_ITEM := TMatrix3Double}
{$define TMF_CLASS_ITEM := TSFMatrix3d}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TMatrix3DoubleList}
{$define TMF_SCALAR := Double}
{$define TSF_MATRIX_COLS := 3}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := IdentityMatrix3Double}
{$define TMF_EVENT := TMFMatrix3dEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_MATRICES

{$define TMF_CLASS := TMFMatrix4f}
{$define TMF_STATIC_ITEM := TMatrix4Single}
{$define TMF_CLASS_ITEM := TSFMatrix4f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TMatrix4SingleList}
{$define TMF_SCALAR := Single}
{$define TSF_MATRIX_COLS := 4}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := IdentityMatrix4Single}
{$define TMF_EVENT := TMFMatrix4fEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_MATRICES

{$define TMF_CLASS := TMFMatrix4d}
{$define TMF_STATIC_ITEM := TMatrix4Double}
{$define TMF_CLASS_ITEM := TSFMatrix4d}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TMatrix4DoubleList}
{$define TMF_SCALAR := Double}
{$define TSF_MATRIX_COLS := 4}
{$define TMF_DYN_DEFAULT_SAFE_VALUE := IdentityMatrix4Double}
{$define TMF_EVENT := TMFMatrix4dEvent}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_MATRICES

{ TMFBool ------------------------------------------------------------------ }

function TMFBool.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := BoolKeywords[Encoding][Items[ItemNum]];
end;

class function TMFBool.X3DType: string;
begin
  Result := 'MFBool';
end;

{ TMFLong -------------------------------------------------------------------- }

function TMFLong.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := SaveToStreamLineUptoNegative and (Items[ItemNum] < 0);
end;

function TMFLong.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := IntToStr(Items[ItemNum])
end;

class function TMFLong.X3DType: string;
begin
  Result := 'MFLong';
end;

procedure TMFLong.WritelnWarning_WrongVertexIndex(
  const GeometryX3DType: string;
  const VertexNum: Integer; const CoordCount: Integer);
const
  MaxWrongVertexIndexWarnings = 10;
begin
  Inc(WrongVertexIndexWarnings);
  if WrongVertexIndexWarnings < MaxWrongVertexIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Wrong vertex index in indexed node %s (not enouch points in Coordinate node defined: index is %d, we have only %d vertices)',
      [GeometryX3DType, VertexNum, CoordCount])) else
  if WrongVertexIndexWarnings = MaxWrongVertexIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Wrong vertex index in indexed node %s reported for the %dth time. Further warnings regarding this field will not be reported (to avoid wasting time on printing countless warnings...)',
      [GeometryX3DType, WrongVertexIndexWarnings]));
end;

{ TMFInt32 ------------------------------------------------------------------- }

class function TMFInt32.X3DType: string;
begin
  Result := 'MFInt32';
end;

procedure TMFInt32.Send(const AValue: array of LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TMFInt32.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TMFInt32.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFInt32Event.Create(AParentNode, AName, AInEvent);
end;

{ TMFMatrix3f ------------------------------------------------------------------- }

class function TMFMatrix3f.X3DType: string;
begin
  Result := 'MFMatrix3f';
end;

{ TMFMatrix3d ------------------------------------------------------------------- }

class function TMFMatrix3d.X3DType: string;
begin
  Result := 'MFMatrix3d';
end;

{ TMFMatrix4f ------------------------------------------------------------------- }

class function TMFMatrix4f.X3DType: string;
begin
  Result := 'MFMatrix4f';
end;

{ TMFMatrix4d ------------------------------------------------------------------- }

class function TMFMatrix4d.X3DType: string;
begin
  Result := 'MFMatrix4d';
end;

{ TMFVec2f ------------------------------------------------------------------- }

class function TMFVec2f.X3DType: string;
begin
  Result := 'MFVec2f';
end;

{ TMFVec3f ------------------------------------------------------------------- }

class function TMFVec3f.X3DType: string;
begin
  Result := 'MFVec3f';
end;

{ TMFColor ------------------------------------------------------------------- }

class function TMFColor.X3DType: string;
begin
  Result := 'MFColor';
end;

procedure TMFColor.Send(const AValue: array of TVector3Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TMFColor.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TMFColor.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFColorEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFVec4f ------------------------------------------------------------------- }

class function TMFVec4f.X3DType: string;
begin
  Result := 'MFVec4f';
end;

{ TMFColorRGBA --------------------------------------------------------------- }

class function TMFColorRGBA.X3DType: string;
begin
  Result := 'MFColorRGBA';
end;

procedure TMFColorRGBA.Send(const AValue: array of TVector4Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TMFColorRGBA.Create(ParentNode, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TMFColorRGBA.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFColorRGBAEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFVec2d ------------------------------------------------------------------- }

class function TMFVec2d.X3DType: string;
begin
  Result := 'MFVec2d';
end;

{ TMFVec3d ------------------------------------------------------------------- }

class function TMFVec3d.X3DType: string;
begin
  Result := 'MFVec3d';
end;

{ TMFVec4d ------------------------------------------------------------------- }

class function TMFVec4d.X3DType: string;
begin
  Result := 'MFVec4d';
end;

{ TMFRotation ---------------------------------------------------------------- }

class function TMFRotation.X3DType: string;
begin
  Result := 'MFRotation';
end;

{ TMFFloat ------------------------------------------------------------------- }

function TMFFloat.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFFloat.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := FloatToRawStr(Items[ItemNum]);
end;

procedure TMFFloat.AssignLerp(const A: Double; Value1, Value2: TX3DField);
var
  I: Integer;
  Val1, Val2: TMFFloat;
begin
  Val1 := Value1 as TMFFloat;
  Val2 := Value2 as TMFFloat;

  Val1.CheckCountEqual(Val2);
  Items.Count := Val1.Items.Count;

  for I := 0 to Items.Count - 1 do
    Items[I] := Lerp(A, Val1.Items[I], Val2.Items[I]);
end;

function TMFFloat.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFFloat.X3DType: string;
begin
  Result := 'MFFloat';
end;

function TMFFloat.CreateItemBeforeParse: TX3DSingleField;
begin
  Result := inherited;
  { Assign our Angle to single item used for parsing.
    This way float values on MFFloat fields will be correctly converted to radians,
    important e.g. for Background.skyAngle,groundAngle. }
  (Result as TSFFloat).Angle := Angle;
end;

{ TMFDouble -------------------------------------------------------------------- }

function TMFDouble.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFDouble.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := FloatToRawStr(Items[ItemNum]);
end;

procedure TMFDouble.AssignLerp(const A: Double; Value1, Value2: TX3DField);
var
  I: Integer;
  Val1, Val2: TMFDouble;
begin
  Val1 := Value1 as TMFDouble;
  Val2 := Value2 as TMFDouble;

  Val1.CheckCountEqual(Val2);
  Items.Count := Val1.Items.Count;

  for I := 0 to Items.Count - 1 do
    Items[I] := Lerp(A, Val1.Items[I], Val2.Items[I]);
end;

function TMFDouble.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFDouble.X3DType: string;
begin
  Result := 'MFDouble';
end;

{ TMFTime -------------------------------------------------------------------- }

class function TMFTime.X3DType: string;
begin
  Result := 'MFTime';
end;

class function TMFTime.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFTimeEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFString ------------------------------------------------------------------ }

function TMFString.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  case Encoding of
    xeClassic: Result := StringToX3DClassic(Items[ItemNum]);
    xeXML    : Result := StringToX3DXmlMulti(Items[ItemNum]);
    else raise EInternalError.Create('TMFString.RawItemToString Encoding?');
  end;
end;

class function TMFString.X3DType: string;
begin
  Result := 'MFString';
end;

procedure TMFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Lexer: TX3DLexer;
begin
  { For MFString, it's very common that normal parsing fails because
    of missing double quotes, even in models from
    http://www.web3d.org/x3d/content/examples/Basic/
    Although specification clearly says that MFString
    components should always be enclosed within double
    quotes. We just do what Xj3D seems to do, that is
    we handle this as a single string (producing a warning). }

  try
    Lexer := TX3DLexer.CreateForPartialStream(AttributeValue, Reader.Version);
    try
      ParseXMLAttributeLexer(Lexer, Reader);
    finally FreeAndNil(Lexer) end;

    { Surround in try..except both CreateForPartialStream and ParseXMLAttributeLexer,
      as CreateForPartialStream can already cause exception in case of
      demo-models/x3d/test_single_quotes_mfstring.x3d . }
  except
    on E: Exception do
    begin
      WritelnWarning('VRML/X3D', 'Error when reading MFString field "' + X3DName + '" value. Possibly missing double quotes (treating as a single string): ' + E.Message);
      Items.Count := 0;
      Items.Add(AttributeValue);
    end;
  end;
end;

function TMFString.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttributeCustomQuotes;
end;

procedure TMFString.SaveToStreamValue(Writer: TX3DWriter);
begin
  { MFString in XML encoding is surrounded by single quotes }
  if Writer.Encoding = xeXML then Writer.Write('''');
  inherited;
  if Writer.Encoding = xeXML then Writer.Write('''');
end;

{ TX3DFieldsManager --------------------------------------------------------- }

constructor TX3DFieldsManager.Create;
begin
  inherited;
  Registered := TStringList.Create;
  { All VRML/X3D names are case-sensitive. }
  Registered.CaseSensitive := true;
end;

destructor TX3DFieldsManager.Destroy;
begin
  FreeAndNil(Registered);
  inherited;
end;

procedure TX3DFieldsManager.RegisterClass(AClass: TX3DFieldClass);
begin
  Registered.AddObject(AClass.X3DType, TObject(AClass));
end;

procedure TX3DFieldsManager.RegisterClasses(
  const Classes: array of TX3DFieldClass);
var
  I: Integer;
begin
  for I := 0 to High(Classes) do
    RegisterClass(Classes[I]);
end;

function TX3DFieldsManager.X3DTypeToClass(
  const X3DType: string): TX3DFieldClass;
var
  I: Integer;
begin
  I := Registered.IndexOf(X3DType);
  if I <> -1 then
    Result := TX3DFieldClass(Registered.Objects[I]) else
    Result := nil;
end;

var
  FX3DFieldsManager: TX3DFieldsManager;

function X3DFieldsManager: TX3DFieldsManager;
{ This function automatically creates FX3DFieldsManager instance.
  I don't do this in initialization of this unit, since (because
  of circular uses clauses) X3DFieldsManager may be referenced
  before our initialization (e.g. by initialization of X3DNodes). }
begin
  if FX3DFieldsManager = nil then
    FX3DFieldsManager := TX3DFieldsManager.Create;
  Result := FX3DFieldsManager;
end;

{ global utilities ----------------------------------------------------------- }

function X3DChangesToStr(const Changes: TX3DChanges): string;
var
  C: TX3DChange;
begin
  Result := '';
  for C := Low(C) to High(C) do
    if C in Changes then
    begin
      if Result <> '' then Result += ',';
      Result += X3DChangeToStr[C];
    end;
  Result := '[' + Result + ']';
end;

initialization
  X3DFieldsManager.RegisterClasses([
    TSFBitMask,
    TSFEnum,
    TSFBool,     TMFBool,
    TSFFloat,    TMFFloat,
    TSFImage,
    TSFLong,     TMFLong,
    TSFInt32,    TMFInt32,

    TSFMatrix3f, TMFMatrix3f,
    TSFMatrix,
    TSFMatrix3d, TMFMatrix3d,
    TSFMatrix4f, TMFMatrix4f,
    TSFMatrix4d, TMFMatrix4d,

    TSFRotation, TMFRotation,
    TSFString,   TMFString,
    TSFDouble,   TMFDouble,
    TSFTime,     TMFTime,
    TSFVec2f,    TMFVec2f,
    TSFVec3f,    TMFVec3f,
    TSFColor,    TMFColor,
    TSFVec4f,    TMFVec4f,
    TSFVec2d,    TMFVec2d,
    TSFVec3d,    TMFVec3d,
    TSFVec4d,    TMFVec4d,
    TSFColorRGBA,TMFColorRGBA
    ]);
finalization
  FreeAndNil(FX3DFieldsManager);
end.
