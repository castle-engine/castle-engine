{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TX3DNode class. Huge class, the basic building block of X3D nodes. }

{$ifdef read_interface}

  { Private stuff for TX3DNode.DeepCopy and friends implementation. }
  TX3DNodeDeepCopyState = class
  private
    type
      TNodeNodeMap = {$ifdef FPC}specialize{$endif} TDictionary<TX3DNode, TX3DNode>;
      TExportExportMap = {$ifdef FPC}specialize{$endif} TDictionary<TX3DExport, TX3DExport>;
    var
      NodeOriginalToCopy: TNodeNodeMap;
      ExportOriginalToCopy: TExportExportMap;
  public
    constructor Create;
    destructor Destroy; override;
    { Return a copy or OriginalNode.

      To keep sharing of nodes (DEF/USE mechanism) within the newly
      created copy, we need a list of already duplicated children.
      This method uses and updates such list. When called for the
      first time with OriginalNode, it actually creates a duplicate
      (by OriginalNode.DeepCopy). Next time, it will just return
      this copy. }
    function DeepCopy(const OriginalNode: TX3DNode): TX3DNode;
  end;

  { Simple component system inside TX3DNode.
    See @link(TX3DNode.Functionality). }
  TNodeFunctionality = class
  strict private
    FParent: TX3DNode;
  public
    constructor Create(const AParent: TX3DNode);
    property Parent: TX3DNode read FParent;
  end;

  TNodeFunctionalityList = {$ifdef FPC}specialize{$endif} TObjectList<TNodeFunctionality>;

  TNodeFunctionalityClass = class of TNodeFunctionality;

  TFindNodeOption = (fnNilOnMissing, fnOnlyActive);
  TFindNodeOptions = set of TFindNodeOption;

  { X3D node. Every X3D node class descends from this. }
  TX3DNode = class(TX3DFileItem)
  strict private
    FFunctionalityList: TNodeFunctionalityList; //< may be @nil if none
    FTransformFunctionality: TTransformFunctionality;
    FTimeFunctionality: TTimeDependentFunctionality;
    FGenTexFunctionality: TGeneratedTextureFunctionality;
  private
    FX3DName: string;
    FBaseUrl: string;
    FVRML1Children, FVRML1Parents: TX3DNodeList;
    FKeepExisting: Cardinal;
    FParentFields: TX3DFieldList;
    FPrototypes: TX3DPrototypeBaseList;
    FRoutes: TX3DRouteList;
    FImports, FExports: TX3DFileItemList;
    FFields, FFieldsSFNode, FFieldsMFNode: TX3DFieldList;
    FEvents: TX3DEventList;
    FPrototypeInstance: boolean;
    FPrototypeInstanceSourceNode: TX3DPrototypeNode;
    FPrototypeInstanceHelpers: TX3DNode;
    FDefaultContainerField: string;
    FHasInterfaceDeclarations: TX3DAccessTypes;
    FInterfaceDeclarations: TX3DInterfaceDeclarationList;
    FCDataField: TMFString;
    FDestructionNotifications: TNodeDestructionNotificationList;
    FScene: TX3DEventsEngine;
    FInternalSceneShape: TObject;

    { Internally used by PROTO instantiating mechanism,
      in TX3DPrototypeNode.InstantiateIsClauses
      and TX3DPrototypeNode.Instantiate.

      This is always @false outside of TX3DPrototypeNode.Instantiate
      implementation (as TX3DPrototypeNode.Instantiate sets it to @true
      and changes back to @false for appropriate nodes). }
    NeedsInstantiateIsClause: boolean;

    function GetVRML1Child(const I: Integer): TX3DNode;
    function GetVRML1Parent(const I: Integer): TX3DNode;
    procedure SetVRML1Child(const I: Integer; const Value: TX3DNode);
    function GetParentFieldsItem(Index: Integer): TX3DField;
    function GetParentFieldsNodeItem(Index: Integer): TX3DNode;
    procedure RemoveParentField(const Field: TX3DField);
    procedure AddParentField(const Field: TX3DField);
    procedure SetHasInterfaceDeclarations(const Value: TX3DAccessTypes);
    { Internal save to stream utility.
      When CurrentContainerField is not empty, it means we know our containing field
      (and should adjust containerField= for XML encoding output). }
    procedure NodeSaveToStream(Writer: TX3DWriter; const CurrentContainerField: string = '');
    function TryFindNodeStateTraverse(
      ANode: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
    function TryFindNodeTransformTraverse(
      ANode: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
    function GetPrototypes(const Index: Integer): TX3DPrototypeBase;
    function GetRoutes(const Index: Integer): TX3DRoute;
    function GetImports(const Index: Integer): TX3DImport;
    function GetExports(const Index: Integer): TX3DExport;
    function GetFields(const Index: Integer): TX3DField;
    function GetEvents(const Index: Integer): TX3DEvent;
    procedure UnregisterSceneCallback(Node: TX3DNode);
  protected

    { Does actual DeepCopy work. You can override this to copy some
      more properties for descendants. }
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; virtual;

    { This should be a mere call to constructor of your own class.

      In TX3DNode, this simply calls default virtual constructor,
      which is Ok for all normal nodes. But we have some special nodes,
      like TX3DPrototypeNode or TX3DUnknownNode, that simply cannot
      be created by default constructor. They need to override this. }
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; virtual;

    { Register new TNodeFunctionality instance to be returned by @link(Functionality),
      and automatically freed when this node is freed. }
    procedure AddFunctionality(const F: TNodeFunctionality);
  protected
    { Are VRML 1.0 children allowed.
      This condition is checked in VRML1ChildAdd, so it's strictly
      impossible to add a node that is not allowed.

      Note that in some special cases VRML1ChildrenAllowed and
      VRML1ChildrenParsingAllowed values may be changed during object lifetime.
      Currently, this may concern TX3DUnknownNode.

      Default @false. }
    VRML1ChildrenAllowed: boolean;

    { VRML 1.0 children allowed to be added during parsing.
      This is used only by *Inline nodes for now, that do not allow
      reading children during parsing but may get new children
      in memory. So their VRML1ChildrenParsingAllowed must be empty,
      but VRML1ChildrenAllowed must allow all.

      When VRML1ChildrenAllowed is @false then VRML1ChildrenParsingAllowed should
      also be @false.

      Default @false. }
    VRML1ChildrenParsingAllowed: boolean;

    { Should SaveToStream save our VRML1Children.
      In this class default implementation returns @true,
      this is what you will want in 99% of cases.
      It's useful to set this to false if you use
      VRML1Children internally, e.g. *Inline nodes. }
    class function VRML1ChildrenSaveToStream: boolean; virtual;

    { Enumerate all active child nodes of given node.

      "Active nodes" are the ones affecting current look or collisions,
      e.g. from Switch node only one child will be enumerated.
      See @link(Traverse) for more precise definition.

      "Direct" means that this enumerates only direct descendants,
      i.e. this is not recursive. See methods like Traverse or EnumerateNodes
      if you want recursive behavior.

      This can enumerate both @link(VRML1Children) nodes
      and nodes within TSFNode and TMFNode fields.

      Default implementation in this class enumerates all Children
      nodes of VRML 1.0. If you need to remove some children
      for VRML 1.0 (e.g. for Switch or LOD nodes)
      or add some children for VRML 2.0 you
      have to override this. @bold(You do not need to call inherited
      when overriding this --- in fact, you should not, if you
      want to omit some nodes.)

      Stops and returns immediately if Func returns non-nil for some child. }
    function DirectEnumerateActive(
      Func: TEnumerateChildrenFunction): Pointer; virtual;

    { Enumerate all active child nodes of given node,
      and may additionally modify StateStack. It's used by Traverse.

      Default implementation in this class simply calls
      DirectEnumerateActive, ignoring StateStack, and this is suitable
      for 99% of nodes. However, for some special nodes (only Collision
      node for now), they have to modify state during traversing into
      various children, and then they can override this.

      Stops and returns immediately in Func returns non-nil for some child. }
    function DirectEnumerateActiveForTraverse(
      Func: TEnumerateChildrenFunction;
      StateStack: TX3DGraphTraverseStateStack): Pointer; virtual;

    { Simply enumerate all direct descendant nodes.
      That is, all children in VRML 1.0 style and
      all nodes in SFNode and MFNode fields.
      This includes prototype stuff, if this node is expanded
      from prototype: PrototypeInstanceSourceNode and PrototypeInstanceHelpers. }
    function DirectEnumerateAll(
      Func: TEnumerateChildrenFunction): Pointer;

    { This enumerates direct descendant nodes of this node.
      This is equivalent to DirectEnumerateActive or
      DirectEnumerateAll, depending on value of OnlyActive param. }
    function DirectEnumerate(
      Func: TEnumerateChildrenFunction;
      OnlyActive: boolean): Pointer;

    { Override these methods to determine what happens when
      given node is traversed during Traverse call.
      The main use of this is to operate on TX3DGraphTraverseStateStack.

      Remember to always call inherited when overriding.
      In BeforeTraverse and MiddleTraverse you should call inherited
      at the beginning, in AfterTraverse inherited should be called at the end.

      Besides changing StateStack.Top fields, you can do push/pop
      on the stack. Remember that if you do StateStack.Push in BeforeTraverse,
      and then you @italic(must call StateStack.Pop in AfterTraverse).

      @groupBegin }
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); virtual;
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); virtual;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); virtual;
    { @groupEnd }

    { Parse VRML node body element. Usually, this is a field.
      May also be VRML 1.0 style child node.
      May also be VRML 2.0 Script node interface declaration, etc.
      --- see VRML 2.0 grammar spec.

      This should be overriden to parse special features within particular
      nodes. While generally VRML is very clean and there's no need to
      override this, there's one use for this currently:

      @orderedList(
        @item(Since we handle a couple of VRML flavors (at least
          Inventor, VRML 1.0 and VRML 97), sometimes the same node has
          different fields to express the same things in various VRML flavors.
          So it may be useful to parse a field and copy it's value into
          other fields.

          Example: TShapeHintsNode_1 in Inventor parses "hints" field,
          and copies it's value to other fields as appropriate.
          "hints" field is not exposed in TShapeHintsNode_1 interface,
          so everything is clean in the interface, and under the hood
          TShapeHintsNode_1 can "magically" handle "hints" field for Inventor.)
      )

      When overriding, always check inherited result first, and exit if
      inherited handled successfully.
      Otherwise either read your stuff and return @true
      (Lexer should advance to the position of next "nodeBodyElement").
      Or return @false without changing Lexer position. }
    function ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
      const APositionInParent: Integer): boolean; virtual;

    { Called at the end of parsing this node (including children) in any encoding. }
    procedure ParseAfter(Reader: TX3DReaderNames); virtual;

    procedure CreateNode; virtual;

    { The field where CDATA section from XML is added.
      Used when loading X3D in XML encoding: XML elements may contain
      CDATA sections, that are added to "url" field.
      See X3D XML encoding specification about
      "Encapsulating Script node code"
      [http://www.web3d.org/x3d/specifications/ISO-IEC-19776-1.2-X3DEncodings-XML/Part01/concepts.html#EncapsulatingScriptNodeCode].
      We also allow this for shader nodes (sensible (follows the intention
      of the spec) and compatible with InstantReality).

      When not assigned, then CDATA section for this node is not allowed.

      This should be set in descendants constructor. }
    property CDataField: TMFString read FCDataField write FCDataField;

    { React when this node is processed as an immediate child of a grouping node,
      within grouping node's BeforeTraverse. }
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); virtual;
  public
    { Node fields.

      For normal nodes, all fields are created and added
      using AddField from the constructor. Fields default values are set,
      and current field values are set to these defaults.
      Later, we only modify these fields current values (e.g. when parsing).

      However, there are special node classes that set their fields differently.
      TX3DPrototypeNode has their fields set according to it's VRML 2.0 prototype.
      TX3DUnknownNode may have it's fields set by VRML 1.0 "fields" feature
      (so it's Fields are initialized by parsing it).

      Nodes with HasInterfaceDeclarations have some Fields and Events
      added when reading node.

      All fields on this list are owned by this instance.
      @groupBegin }
    property Fields [const Index: Integer]: TX3DField read GetFields;
    function FieldsCount: Integer;
    procedure AddField(const Value: TX3DField);
    function IndexOfField(const AName: string): Integer;
    { @groupEnd }

    { Explicit events (that is, not exposed by some field) of this node.
      For exposed events, see each field's property ExposedEvents.
      @groupBegin }
    property Events [const Index: Integer]: TX3DEvent read GetEvents;
    function EventsCount: Integer;
    procedure AddEvent(const Value: TX3DEvent);
    function IndexOfEvent(const AName: string): Integer;
    { @groupEnd }

    { Search by name for given field.
      When RaiseOnError=false (default) returns @nil when not found.
      When RaiseOnError=true raises EX3DNotFound when not found.
      @raises EX3DNotFound }
    function Field(const AName: string; const RaiseOnError: Boolean = false): TX3DField;

    { Search by name for given field or event (exposed by some field or not).
      When RaiseOnError=false (default) returns @nil when not found.
      When RaiseOnError=true raises EX3DNotFound when not found.
      @raises EX3DNotFound }
    function FieldOrEvent(const AName: string; const RaiseOnError: Boolean = false): TX3DFieldOrEvent;

    { Search by name for given event (exposed by some field or not).
      When RaiseOnError=false (default) returns @nil when not found.
      When RaiseOnError=true raises EX3DNotFound when not found.
      @raises EX3DNotFound }
    function AnyEvent(const AName: string; const RaiseOnError: Boolean = false): TX3DEvent;

    { VRML 1.0 children nodes. These are nodes directly specified inside
      a VRML 1.0 node, they don't belong to any node field. (In VRML 1.0,
      there was no SFNode / MFNode fields.)

      In VRML 2.0, this is always empty.

      VRML 1.0 nodes may have any number of children.
      The children nodes refer back to it's parent nodes in VRML1Parents list.
      When travelling over VRML/X3D graph, remember that cycles
      are possible, because of DEF/USE.
      Obviously, they possible when travelling along the VRML1Parents list.
      We currently assume that there are no cycles when we treat the graph
      as directed, but it may change one day (some VRML/X3D models
      create real cycles).

      Adding/removing stuff from the VRML1Children list keeps track
      of how many times a node is used. If the child node has no parents,
      it will be freed.
      Actually, nodes can be children of both nodes (VRML 1.0 style,
      then VRML1Children and VRML1Parents is used) or fields (TMFNode or TSFNode,
      in VRML 2.0 style; then ParentFields is used). So the node is freed
      only when it's not referenced by any node and not referenced by any
      field. Generally, it's the parent that takes care of reference-counting
      and freeing the children, not the other way around.

      Note that given node instance may be a children of a single node
      multiple times, through DEF/USE mechanism. The order of children
      is important and preserved. (On the other hand, the order of
      VRML1Parents is not important, as that list is mostly for reference-counting.)

      You can replace one children with another like
      @code(VRML1Children[I] := NewChildren;).
      This works like a shortcut for
      @code(VRML1ChildRemove(I); VRML1ChildAdd(I, NewChildren);).
      But 1. it's more efficient; 2. it's safer --- if Children[I]
      is already equal to NewChildren, it does nothing.

      @groupBegin }
    property VRML1Children [const I: Integer]: TX3DNode read GetVRML1Child write SetVRML1Child;
    function VRML1ChildrenCount: integer;
    { @groupEnd }

    { Add a VRML 1.0 child node at given position.
      Index (position) must be in [0..VRML1ChildrenCount].
      Items at and above Index position are moved to the right,
      to insert new child at Index position. }
    procedure VRML1ChildAdd(const Index: Integer; const Child: TX3DNode); overload;

    { Add a VRML 1.0 child node at the end of VRML1Children list. }
    procedure VRML1ChildAdd(const Child: TX3DNode); overload;

    procedure VRML1ChildRemove(const I: Integer);
    procedure VRML1ChildrenClear;

    { Remove a VRML 1.0 children at index I and return it (do not free it).
      Compare this with VRML1Remove, that removes children I and
      frees it if it's no longer used by anything.

      This removes children I, appropriately adjusting
      all parent / children links, but even if a node is unused after removing,
      it is not freed. It's always returned. }
    function VRML1ChildExtract(const I: Integer): TX3DNode;

    { All VRML 1.0 parent nodes. VRML1Parents is a reverse of
      VRML1Children --- it lists all the nodes where we are on VRML1Children
      list.

      @groupBegin }
    property VRML1Parents [const I: Integer] :TX3DNode read GetVRML1Parent;
    function VRML1ParentsCount: integer;
    { @groupEnd }

    { All SFNode and MFNode fields where this node is referenced.
      This is somewhat analogous for VRML1Parents, but for VRML 2.0.

      ParentFieldsNode is just for your comfort, it returns always
      appropriate field's ParentNode property value
      (i.e. @code((ParentField[Index] as TSFNode).ParentNode)
      or @code((ParentField[Index] as TMFNode).ParentNode)).

      @groupBegin }
    property ParentFields[Index: Integer]: TX3DField read GetParentFieldsItem;
    property ParentFieldsNode[Index: Integer]: TX3DNode
      read GetParentFieldsNodeItem;
    function ParentFieldsCount: Integer;
    { @groupEnd }

    { Free this object (if it's not @nil) @italic(also removing
      it from @bold(all) parent nodes and fields).

      By design, normal destructor (Destroy called by Free)
      doesn't care about removing references to this object from
      it's parents. That's because it's the parents that usually
      initialize freeing of their children, and they free child
      when it's reference count is 0. So this freeing method
      is special in this regard.

      Use this if you really want to remove all node occurrences from the middle
      of VRML hierarchy. }
    procedure FreeRemovingFromAllParents;

    { Free this node if it is not referenced by any parent fields or nodes.
      Takes into account that node may have VRML 1.0 parent nodes
      and VRML 2.0 / X3D parent fields (SFNode or MFNode).
      This is a safe way of removing
      a node that may, but doesn't have to, be part of some VRML/X3D graph.
      The idea is that if a node is a part of some graph,
      we don't need to do anything (since you should have
      a reference to the entine graph somewhere anyway), otherwise node is
      considered unused and freed.

      Analogous to standard TObject.Free, this also works when called on
      a @nil value (does nothing in this case).

      For safety, it's advised to set reference to @nil after calling FreeIfUnused.
      You can use FreeIfUnusedAndNil utility for this (that employs a trick
      to @nil the visible reference even before freeing, which is even safer).
      This is analogous to standard FreeAndNil. }
    procedure FreeIfUnused;

    { Increase KeepExisting property value to disable automatic freeing
      of the node when unused.
      This may be useful in cases when you remove the node from VRML graph
      hierarchy, but you want to keep a valid reference to it anyway
      (for example, maybe you'll free it later by hand or add it somewhere
      else).

      Simple methods KeepExistingBegin and KeepExistingEnd simply increase /
      decrease KeepExisting value by 1. This is the most usual usage.

      @groupBegin }
    property KeepExisting: Cardinal read FKeepExisting write FKeepExisting;
    procedure KeepExistingBegin;
    procedure KeepExistingEnd;
    { @groupEnd }

    { Name of this node. When saving/loading, this comes from VRML/X3D
      "DEF" construct. Empty value means that the name is not defined.
      Do not change this during loading / saving of the X3D graph,
      or searching for nodes e.g. by @link(EnumerateNodes).

      Note that this property is deliberately not called @code(Name).
      In the future, this class may descend from the standard TComponent
      class, that defines a @code(Name) field with a special restrictions
      (it must be a valid Pascal identifier), which cannot apply to X3D node names
      (that can have quite free names, see
      http://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/grammar.html ).
      We don't want to confuse these two properties. }
    property X3DName: string read FX3DName write FX3DName;
    {$ifdef FPC}
    property NodeName: string read FX3DName write FX3DName; deprecated 'use X3DName';
    {$endif FPC}

    { Base URL for all URLs inside our fields.
      Used everywhere as a base for relative URLs, to handle fields that refer
      to external files like textures, other 3D models (ImageTexture,
      Inline nodes, many others).

      It must be an absolute URL. Currently, it doesn't have to contain
      a protocol, and is then interpreted as a path on local filesystem
      (just like it had file:// prefix). Still, it must be an absolute path.

      This is set in constructor, and eventually adjusted by
      various parsing routines.
      This way we could, if only we would like to, resolve nodes
      like Inline or ImageTexture immediately after parsing them. }
    property BaseUrl: string read FBaseUrl write FBaseUrl;

    {$ifdef FPC}
    { Old deprecated name for BaseUrl. @deprecated }
    property WWWBasePath: string read FBaseUrl write FBaseUrl; deprecated;
    {$endif}

    { Returns an absolute path, assuming that RelativePath is relative
      path from BaseUrl or that RelativePath is already absolute. }
    function PathFromBaseUrl(const RelativePath: string): string;

    { Parse node. This should set values of your fields, VRML 1.0 Children
      list, BaseUrl.

      In special cases like TX3DUnknownNode this may
      actually initialize whole Fields list (by VRML 1.0 "fields" extensibility
      feature). }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); virtual;

    { Parse node body, i.e. mainly node's fields. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    { Constructor. Initializes various properties:

      @unorderedList(
        @item(Name, BaseUrl are initialized from given parameters.)
        @item(The @link(Fields), @link(Events) lists are filled
          in every descendant, to have all the fields/events defined
          by the specification.)
        @item(DefaultContainerField, and other node-specific
          stuff, is filled in descendants. This is actually implemented
          in CreateNode, that is called at the end of this constructor.)
      )
    }
    constructor Create(const AX3DName: string = '';
      const ABaseUrl: string = ''); virtual;

    destructor Destroy; override;

    { Type of the node in X3D, like @code('Group') or @code('Shape'). Never empty.
      Constant for the class lifetime (even for special
      TX3DUnknownNode and TX3DPrototypeNode, where this is calculated
      at runtime).

      Note that VRML/X3D is generally case-sensitive, so this property is too.

      In TX3DNode, this returns ClassX3DType, which is suitable
      for most nodes. See ClassX3DType. }
    function X3DType: string; virtual;
    function NodeTypeName: string; deprecated 'use X3DType';

    { Node type name in VRML/X3D, for this class.
      Normal VRML/X3D node classes should override this to return something
      non-empty, and then X3DType automatically will return the same value.

      Empty for classes that don't have a hardcoded VRML/X3D node name,
      like a special TX3DUnknownNode. Such special classes should override
      then X3DType to return actual non-empty name there.

      You usually should call X3DType. The only use of this method
      is that it works on classes (it's "class function"), without needing
      at actual instance. }
    class function ClassX3DType: string; virtual;

    { Traverse all the nodes of VRML graph that are active.

      An "active" part of the VRML graph are the nodes that actually
      change what the VRML file represents, in terms of geometry,
      collision detection etc. For example, the Switch node has only
      one child usually active. Nodes that merely influence
      the active graph by some events and routes do not have to be
      active (these nodes may change what the VRML file actually represents,
      but only by changing other nodes).

      For all nodes of NodeClass TraversingFunc
      will be called. If if returns non-nil for some node, the processing
      of following nodes (children and siblings) will be aborted and result
      will be returned.

      Traverse not only enumerates these
      nodes, it also collects all state (transformation, etc ---
      see TX3DGraphTraverseState) that affects how given node should
      be presented.

      Also, TraversingInfo is passed to each TraversingFunc call.
      This allows you to investigate, during TraversingFunc call, the parents
      hierarchy (you can't use VRML1Parents / ParentFields of the current node,
      since a node may have many parents). Traverse calls are
      naturally recursive, and so the stack of TraversingInfo
      structures is naturally build and destroyed by recursive calls.
      For the root node (the one where you called Traverse without
      specifying initial TraversingInfo), ParentInfo is simply @nil.

      The scheme of how Traverse works:

      @preformatted(
        BeforeTraverse;

        TraverseIntoChildren := true;
        if Self is NodeClass then TraversingFunc (Self, State, TraverseIntoChildren);

        MiddleTraverse;

        if TraverseIntoChildren is still true then
          for all children returned by DirectEnumerateActive
            call their Traverse(State);

        AfterTraverse;

        Add Self to State.VRML1State (for VRML 1.0 state nodes);
      )

      Note: setting TraverseIntoChildren to false means that some
      part of the tree is explicitly omitted from traversing.
      Use this only if you know what you're doing, as for some
      nodes they actually affect their parents too (for example,
      chilren within VRML 1.0 Group affect other nodes too;
      global lights within VRML >= 2.0 affect all nodes; and so on...).
      Usually, you will use this only for separator-like nodes
      (for VRML >= 2.0, all Group, Transform, Switch etc. act like separators),
      and only when you will somehow traverse into these nodes anyway.

      We guarantee that AfterTraverse will be called if BeforeTraverse
      was called and finished (that is, AfterTraverse is called in the
      @code(finally..end)).

      During traversing, you can only modify the children (direct or not)
      nodes of the current node. }
    function Traverse(
      NodeClass: TX3DNodeClass;
      TraversingFunc: TTraversingFunc): Pointer;

    { Like @link(Traverse), but it takes explicit starting state stack
      and starting ParentInfo. Not generally useful, use only for special
      purposes. }
    function TraverseInternal(StateStack: TX3DGraphTraverseStateStack;
      NodeClass: TX3DNodeClass;
      TraversingFunc: TTraversingFunc;
      ParentInfo: PTraversingInfo): Pointer;

    { Like @link(Traverse), but only enters children.
      This does the job of Traverse normally omitted if your @link(Traverse)
      callback returns TraverseIntoChildren = @false.
      Using this method, you can traverse into children explicitly
      from your callback. }
    function TraverseIntoChildren(StateStack: TX3DGraphTraverseStateStack;
      NodeClass: TX3DNodeClass;
      TraversingFunc: TTraversingFunc;
      ParentInfo: PTraversingInfo): Pointer;

    { Enumerate all our children of some class. Recursively.
      Enumerates also this instance (Self), if it satisfies the
      NodeClass and SeekNodeName conditions.

      This enumerates both VRML 1.0 children (@link(VRML1Children)) as well as
      VRML >= 2.0 (including X3D) children (nodes in TSFNode and TMFNode fields).

      If OnlyActive then it will enumerate only active parts
      of the graph ("active" as defined by @link(Traverse)),
      so it will work as a simpler version of Traverse
      (simpler, because it doesn't track any state).

      If not OnlyActive then it will simply enumerate all nodes.
      This will include then also prototype helpers, if this node
      was expanded from prototype: see PrototypeInstanceSourceNode
      and PrototypeInstanceHelpers.

      If you give SeekNodeName parameter, we'll only look for nodes
      with X3DName = SeekNodeName. When SeekNodeName = '' it,
      consistently, looks for unnamed nodes (where X3DName = '').

      Enumerates children only after enumerating Self.
      So you can modify children before enumerating them in the Proc callback.
      You can only modify your children in the Proc callback.

      @groupBegin }
    procedure EnumerateNodes(
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TX3DNodeClass;
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TX3DNodeClass;
      const SeekNodeName: string;
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    { @groupEnd }

    { Search nodes, like enumerating nodes but allows early exit during
      the enumeration.
      We search for a node for which Proc(Node) returns something non-nil.

      For every node, the given Proc callback is run.
      If the Proc callback returns something non-nil,
      the enumeration of the following nodes stops,
      and we return the callback result. }
    function SearchNodes(const Proc: TX3DNodeSearchProc;
      const OnlyActive: boolean): Pointer;

    { TryFindNodeByName and TryFindNode seek for a node with
      given class (and node name, in case of TryFindNodeByName).
      If OnlyActive then they seek among only active nodes
      ("active" as defined by @link(Traverse)), otherwise all nodes.

      These functions are quite like EnumerateNodes, except
      they stop at the first occurrence and return it.

      TryFindNodeByName and TryFindNode return @nil if such node
      is not found. FindNodeByName and FindNode raise exception EX3DNotFound
      in this case.
      @raises(EX3DNotFound FindNodeByName and FindNode raise
        this exception when node is not found.)
      @groupBegin }
    function TryFindNodeByName(FindClass: TX3DNodeClass;
      const FindName: string;
      OnlyActive: boolean): TX3DNode; deprecated 'use FindNode(NodeClass: TX3DNodeClass, FindName: String, Options: TFindNodeOptions = [])';
    function FindNodeByName(FindClass: TX3DNodeClass;
      const FindName: string;
      OnlyActive: boolean): TX3DNode; deprecated 'use FindNode(NodeClass: TX3DNodeClass, FindName: String, Options: TFindNodeOptions = [])';
    function TryFindNode(FindClass: TX3DNodeClass;
      OnlyActive: boolean): TX3DNode; deprecated 'use FindNode(NodeClass: TX3DNodeClass, FindName: String, Options: TFindNodeOptions = [])';
    function FindNode(FindClass: TX3DNodeClass;
      OnlyActive: boolean): TX3DNode; overload; deprecated 'use FindNode(NodeClass: TX3DNodeClass, FindName: String, Options: TFindNodeOptions = [])';
    { @groupEnd }

    { Find a node by name and class, in this node and children (recursively).

      By default it searches both active and inactive graph parts.
      Add fnOnlyActive to search only in active parts.

      @raises(EX3DNotFound When node is not found.
        Unless fnNilOnMissing in Options, then it returns @nil on missing node,
        and EX3DNotFound is never raised.)
    }
    function FindNode(const NodeClass: TX3DNodeClass; const FindName: String;
      const Options: TFindNodeOptions = []): TX3DNode; overload;

    function FindNode(const FindName: string): TX3DNode; overload; deprecated 'use FindNode(NodeClass: TX3DNodeClass, FindName: String, Options: TFindNodeOptions = [])';

    {$ifdef GENERIC_METHODS}

    { Find a node of given type and given name, in this node and children (recursively).
      See FindNode for description of Options. }
    {$ifdef FPC}generic{$endif} function Find<T: TX3DNode>(const FindName: string;
      const Options: TFindNodeOptions = []): T;

    {$endif}

    { Find the first node with given class (NodeClass),
      return it's state or just transformation.
      Similar to @link(Traverse), but stops as soon as the given node is found.

      Returns @false when not found ("out" parametrs
      Node, State and Transform are undefined then).
      @groupBegin }
    function TryFindNodeState(
      NodeClass: TX3DNodeClass;
      out Node: TX3DNode;
      out State: TX3DGraphTraverseState): boolean; deprecated 'use FindNode to look for nodes easier; or iterate over TCastleScene.Shapes.TraverseList if you really need to know nodes with their TX3DGraphTraverseState';
    function TryFindNodeTransform(
      NodeClass: TX3DNodeClass;
      out Node: TX3DNode;
      out Transform: TMatrix4;
      out TransformScale: Single): boolean; deprecated 'use FindNode to look for nodes easier; or iterate over TCastleScene.Shapes.TraverseList if you really need to know nodes with their transformations';
    { @groupEnd }

    { Seeks Self and parent nodes (from VRML1Parents and ParentFields,
      recursively) for given node name.

      In other words, this is similar to TryNodeByName or NodeByName,
      but it goes "upward" in graph hierarchy. Note that this
      never restricts itself only to "active" graph part
      ("active" as defined by @link(Traverse))
      because you really can't detect what is the "active"
      part of the graph when going upward.

      @raises(EX3DNotFound
        FindParentByName raises this exception when node is not found.)

      @groupBegin }
    function TryFindParentByName(const FindName: string): TX3DNode;
    function FindParentByName(const FindName: string): TX3DNode;
    { @groupEnd }

    { Looks for a given Node in parents (and self), recursively.
      Similar to TryFindParentByName. Returns @true only if found. }
    function HasParentNode(Node: TX3DNode): boolean;

    { Search immediate parents of this node for a node with given FindName.
      Returns @nil if not found. }
    function TryFindDirectParentByName(const FindName: string): TX3DNode;

    { Looks for a given node in our children (direct and not, including self).
      If OnlyActive, then only active parts are searched
      ("active" as defined by @link(Traverse)). }
    function IsNodePresent(Node: TX3DNode; OnlyActive: boolean): boolean;

    { Count the occurrences of given node class in our children.
      For example, you can pass NodeClass = TAbstractLightNode to count
      the light sources in the scene.

      If CountOnlyActiveNodes, then only active parts are searched
      ("active" as defined by @link(Traverse)).

      This traverses both VRML 1.0 children nodes and VRML 2.0 nodes
      inside SFNode and MFNode fields. }
    function NodesCount(NodeClass: TX3DNodeClass;
      CountOnlyActiveNodes: boolean): integer;

    { Save node to a stream. Saves everything, including node name,
      node type, and node contents. }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Do the nodes of (exactly) this class should be included
      in VRML 1.0 state (see TX3DGraphTraverseState.VRML1State). }
    class function VRML1StateNode(out StateNode: TVRML1StateNode): boolean;

    { Some nodes are present only in specific VRML/X3D version.
      This functions decides it.

      For example some nodes can only work in VRML < 2.0,
      some others only in VRML >= 2.0. There are even some pairs
      of nodes: for example TConeNode_1 works with VRML < 2.0,
      TConeNode works with VRML >= 2.0.

      NodesManager will use this.

      Default implementation of this function returns always @true.
      Generally, I don't try to set this too aggresively ---
      in other words, for all cases when it's sensible, I allow
      nodes to be used in every VRML/X3D version, even when official
      specification doesn't. This means that when reading VRML 1.0
      files actually a large part of VRML 2.0 is allowed too,
      and also while reading VRML 2.0 many constructs from VRML 1.0
      (officially no longer present in VRML 2.0) are allowed too.
      I'm trying to support what I call a "sum of VRML 1.0 and 2.0".

      In practice I only use this function when various VRML/X3D versions
      specify the same node name but

      @unorderedList(
        @item(With different fields.

          For example Cone and Cylinder have slightly different fields,
          due to the fact that VRML 2.0 resigned from using TSFBitMask fields.)

        @item(With different behavior.

          For example definitions of Sphere for VRML 1.0
          and 2.0 are practically equal. However, the behavior from where
          to take texture and material info is different --- in VRML 1.0
          we take last Texture2, Material etc. nodes, while in VRML 2.0
          we look in parent Shape's "appearance" field. So once again
          two different Sphere classes are needed.)
      ) }
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      virtual;

    { Enumerates all children nodes (recursively),
      allowing you to decide for each node to replace or remove it.

      So this is something like EnumerateNodes,
      except that it allows you to remove the nodes. It always
      enumerates all nodes, not only active (e.g. it enumerates all
      Switch node children, not only the chosen one).

      Note that (unlike regular EnumerateNodes) @bold(this doesn't
      report Self to Func !). Which is natural, since this may remove
      nodes by normal VRML1ChildRemove calls, so it needs to know ParentNode
      of the removed node.

      For each node Func will be called, with ParentNode and Node set.
      If you change the Node to something else, then the old node will
      be removed and new Node inserted in the same place.
      If new Node is @nil, then only the old node will be removed.

      Nodes are traversed in depth-first search. Node is first reported
      to Func, and then (if it's not replaced) we descend into this Node.

      @returns The number of removed (and possibly replaced) nodes. }
    function EnumerateReplaceChildren(
      Func: TEnumerateReplaceNodesFunction): Cardinal;

    { Removes all children (and their children, recursively) with
      node names matchig Wildcard. You can use * and ? special chars
      in the Wildcard.
      @returns The number of removed nodes. }
    function RemoveChildrenWithMatchingName(
      const Wildcard: string; IgnoreCase: Boolean): Cardinal;

    property Prototypes [const Index: Integer]: TX3DPrototypeBase read GetPrototypes;
    function PrototypesCount: Integer;
    procedure AddPrototype(const Value: TX3DPrototypeBase);

    property Routes [const Index: Integer]: TX3DRoute read GetRoutes;
    function RoutesCount: Integer;
    procedure AddRoute(const Value: TX3DRoute); overload;
    procedure AddRoute(const Source, Destination: TX3DFieldOrEvent); overload;
    procedure RemoveRoute(const Value: TX3DRoute); overload;
    procedure RemoveRoute(const Index: Integer); overload;

    property ImportsList [const Index: Integer]: TX3DImport read GetImports;
    function ImportsCount: Integer;
    procedure AddImport(const Value: TX3DImport);

    property ExportsList [const Index: Integer]: TX3DExport read GetExports;
    function ExportsCount: Integer;
    procedure AddExport(const Value: TX3DExport);

    { Create a deep copy of this node and all it's children.

      New copy is completely independent from original,
      having all children nodes (in both VRML 1.0 sense (Children)
      and VRML >= 2.0 (inside SFNode and MFNode fields)) also
      duplicated. New copy has protypes, routes, interface declarations
      and generally everything established like in the original, using copied
      nodes.

      Doesn't copy things which are dependent on container hierarchy.
      (So copying them would be more dangerous than useful.)
      This means: DestructionNotifications, Scene, VRML1Parents,
      ParentFields. VRML1Parents and ParentFields will be set for children
      anyway (to appropriate copies).

      Caller owns this newly created copy --- as returned by this method,
      it's not linked anywhere. }
    function DeepCopy: TX3DNode;

    { Internal notes:

      TODO: memory leaks are known to be possible in some difficult cases
      with PrototypeInstanceHelpers. See e.g.
      ../../../demo-models/prototypes/warnings/errors/proto_leak.wrl and
      ../../../demo-models/prototypes/warnings/errors/proto_leak_2.wrl
      for simple testcases. Reason: PrototypeInstanceHelpers may contain,
      by DEF statements, links to Self.
      This causes circular dependency (Self is child of some node on
      PrototypeInstanceHelpers, but PrototypeInstanceHelpers will
      be freed only if Self is freed) causing some memory to be left
      always allocated.

      PrototypeInstanceHelpers are actually always TX3DRootNode,
      may be declared as such in the future if needed.
    }

    { PrototypeInstance = @true indicates that this node was created
      from a non-external prototype instantiation.

      Then PrototypeInstanceSourceNode is non-nil and indicates
      parsed prototype node (and PrototypeInstanceSourceNode.Prototype
      gives you even a link to the actual prototype specification).

      PrototypeInstanceSourceNode is used for events: any ROUTEs
      specified @italic(outside) of prototype and
      leading to/from instantiated prototype should actually lead
      to PrototypeInstanceSourceNode events (not to events of Self).
      Reason: prototype events may be different than actual expanded
      node events, and ROUTEs want to lead to prototype events.
      This is implemented when expanding prototype
      (@link(TX3DPrototypeNode.Instantiate))
      and when linking ROUTE (TX3DRoute.SetSource, TX3DRoute.SetDestination).

      PrototypeInstanceHelpers may be @nil if empty, or may contain
      a list of other nodes duplicated along with the main prototype node.
      From VRML spec:

      @italic(Any other nodes and accompanying scene graphs
      are not part of the transformation hierarchy, but may be referenced
      by ROUTE statements or Script nodes in the prototype definition.)

      Note that for TX3DPrototypeNode (within PrototypeInstanceSourceNode)
      these have a little different meaning: they describe the
      @italic(nested prototype), if any, that was used to create this node.
      This may happen if the node was expanded from one prototype within
      another. (Usually, you shouldn't be concerned about this;
      see TX3DPrototypeNode.Instantiate implementation comments for
      gory details about this.)

      @groupBegin }
    property PrototypeInstance: boolean read FPrototypeInstance;
    property PrototypeInstanceSourceNode: TX3DPrototypeNode
      read FPrototypeInstanceSourceNode;
    property PrototypeInstanceHelpers: TX3DNode read FPrototypeInstanceHelpers;
    { @groupEnd }

    { Default value of "containerField" attribute for this node in X3D XML
      encoding. }
    property DefaultContainerField: string
      read FDefaultContainerField write FDefaultContainerField;

    { For some special VRML / X3D nodes (like Script, ComposedShader)
      that allow the definition of additional fields/events within.

      In X3D specification this is marked like

      @preformatted(
        # And any number of:
        fieldType [in]     fieldName
        fieldType [in,out] fieldName    initialValue
        fieldType [out]    fieldName
        fieldType []       fieldName    initialValue
      )

      If HasInterfaceDeclarations is not [], then InterfaceDeclarations
      will be non-nil and parser (classic VRML parser in this unit,
      X3D XML reader too) will read this from VRML files.
      Moreover, for each interface declaration, also appropriate field/event
      will be added to the list of @link(Fields) or @link(Events),
      so fields/events created by interface declarations will function
      just like other standard fields everywhere.

      @groupBegin }
    property HasInterfaceDeclarations: TX3DAccessTypes
      read FHasInterfaceDeclarations
      write SetHasInterfaceDeclarations default [];

    property InterfaceDeclarations: TX3DInterfaceDeclarationList
      read FInterfaceDeclarations;
    { @groupEnd }

    { Callbacks registered here will be called when this node is destroyed.
      @groupBegin }
    procedure AddDestructionNotification(const Event: TNodeDestructionNotification);
    procedure RemoveDestructionNotification(const Event: TNodeDestructionNotification);
    { @groupEnd }

    { Scene that will be notified about changes to this node.
      This is necessary to allow the scene to properly process VRML/X3D
      events, and also to react properly to any other changes to the nodes
      (like when you change some field directly by ObjectPascal code,
      e.g. by TX3DField.Send).

      May be @nil if none.

      A given TX3DNode may be renderable only by a single renderer.
      This means it can be placed only within one TCastleScene. }
    property Scene: TX3DEventsEngine read FScene write FScene;

    { Internally used by @link(Scene) to store some information at node.
      @exclude }
    property InternalSceneShape: TObject
      read FInternalSceneShape write FInternalSceneShape;

    { This will be always called by VRML parsers after adding new item
      to our InterfaceDeclarations.

      In this class, this simply adds
      IDecl.FieldOrEvent to our normal fields/events by IDecl.AddFieldOrEvent.
      You may override this in subclasses to react in some special way
      to new fields/events, for example Script node may register here
      to receive notification when input event is received. }
    procedure PostAddInterfaceDeclaration(IDecl: TX3DInterfaceDeclaration); virtual;

    { Add to node InterfaceDeclarations given field.
      This should only be used with nodes having
      HasInterfaceDeclarations = @true, like Script or ComposedShader. }
    procedure AddCustomField(AField: TX3DField);

    { Add to node InterfaceDeclarations given field or event.
      This should only be used with nodes having
      HasInterfaceDeclarations = @true, like Script or ComposedShader. }
    procedure AddCustomFieldOrEvent(AFieldOrEvent: TX3DFieldOrEvent);

    { What happens when transformation of the node changes.
      This is internally used by TCastleSceneCore when Transform (or similar)
      node affecting this node changes. It's an important optimization,
      to avoid large if clause with many "is" checks that would be quite slow.
      @exclude }
    function TransformationChange: TNodeTransformationChange; virtual;

    { Nice and concise node description for user.
      Shows node type and name. }
    function NiceName: string;

    { Detach this node from parent scene, recursively.
      Useful if you take a node and want to put it in another TCastleSceneCore.

      @italic(You almost never need to call this method from your application).
      This is done automatically for you when TCastleSceneCore is destroyed.
      And a node can only be part of one TCastleSceneCore instance
      at one time.

      Notable exceptions when you need to call this:

      @orderedList(
        @item(
          If you process RootNode graph and extract some node from it
          (that is, delete node from one RootNode graph,
          but instead of freeing it you insert it into some other VRML/X3D graph)
          you must call it to manually
          detach this node (and all it's children) from a previous TCastleSceneCore
          instance.
        )

        @item(
          Right now the TCastleSceneCore doesn't call RootNode.UnregisterScene
          when TCastleSceneCore.OwnsRootNode.
          This is done for speed, because it's not needed in 99% cases.

          However, if your scene contains some node that isn't freed
          because of @link(KeepExistingBegin),
          then such node will remain attached to non-existing scene.
          You should call UnregisterScene on such node manually, for now.
        )
      )
    }
    procedure UnregisterScene;

    { Find field set by given event of this node.
      @nil if not found (including when this Event doesn't actually belong to this node). }
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; virtual;

    { Find field that sends given event of this node.
      @nil if not found (including when this Event doesn't actually belong to this node). }
    function FieldSendingEvent(const Event: TX3DEvent): TX3DField; virtual;

    { Return an instance performing given functionality of this class
      (like a fog, transformation, material...), or @nil if none.
      This implements a simple component system inside TX3DNode,
      allowing some functionality to have common interface and implementation
      across a few nodes that are not otherwise related in the ancestor hierarchy.

      Descendants should register TNodeFunctionality classes using
      @link(AddFunctionality). }
    function Functionality(
      const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality;

    { Fast shortcut to Functionality(TTransformFunctionality). }
    property TransformFunctionality: TTransformFunctionality read FTransformFunctionality;
    { Fast shortcut to Functionality(TTimeDependentFunctionality). }
    property TimeFunctionality: TTimeDependentFunctionality read FTimeFunctionality;
    { Fast shortcut to Functionality(TGeneratedTextureFunctionality). }
    property GenTexFunctionality: TGeneratedTextureFunctionality read FGenTexFunctionality;
  end;

  TX3DNodeList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DNode>)
    { Find node by name, @nil if not found.

      For empty node name, always returns @nil. This follows the definition
      of @link(TX3DNode.X3DName):
      empty means that node has no name, so it should not be found
      by searches by name, ever.

      @seealso(IndexOfName Returns the node index. This is sometimes more
        flexible (you can always quickly get actual node knowing the index,
        by Items[Index], but not the other way around). It is also sometimes
        less comfortable (often you're not interested in node index).) }
    function FindName(const Name: string): TX3DNode;

    { Find index of a node with given name, -1 if not found. }
    function IndexOfName(const Name: string): Integer;

    procedure AddIfNotExists(const Node: TX3DNode);
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
    procedure Assign(const Source: TX3DNodeList); overload;
    procedure Assign(const Source: array of TX3DNode); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeDeepCopyState ----------------------------------------------------- }

constructor TX3DNodeDeepCopyState.Create;
begin
  inherited;
  NodeOriginalToCopy := TNodeNodeMap.Create;
  ExportOriginalToCopy := TExportExportMap.Create;
end;

destructor TX3DNodeDeepCopyState.Destroy;
begin
  FreeAndNil(NodeOriginalToCopy);
  FreeAndNil(ExportOriginalToCopy);
  inherited;
end;

function TX3DNodeDeepCopyState.DeepCopy(const OriginalNode: TX3DNode): TX3DNode;
begin
  if not NodeOriginalToCopy.TryGetValue(OriginalNode, Result) then
  begin
    { DeepCopyCore will expand NodeOriginalToCopy at the beginning. }
    Result := OriginalNode.DeepCopyCore(Self);
  end;
end;

{ TNodeFunctionality --------------------------------------------------------- }

constructor TNodeFunctionality.Create(const AParent: TX3DNode);
begin
  inherited Create;
  FParent := AParent;
end;

{ TX3DNode ------------------------------------------------------------------ }

constructor TX3DNode.Create(const AX3DName, ABaseUrl: string);
begin
  inherited Create;
  VRML1ChildrenAllowed := false;
  VRML1ChildrenParsingAllowed := false;

  FX3DName := AX3DName;
  FBaseUrl := ABaseUrl;

  FParentFields := TX3DFieldList.Create(false);
  FFields := TX3DFieldList.Create(false);
  FEvents := TX3DEventList.Create(false);

  FHasInterfaceDeclarations := [];
  FInterfaceDeclarations := nil;

  CreateNode;
end;

destructor TX3DNode.Destroy;
var
  I: Integer;
begin
  { This whole unit, including AnyNodeDestructionNotifications,
    may be already finalized when calling this. }
  if AnyNodeDestructionNotifications <> nil then
    AnyNodeDestructionNotifications.ExecuteAll(Self);

  { Note that we call AnyNodeDestructionNotifications and FDestructionNotifications
    early in the destructor, to allow callbacks to work on still properly initialized
    instances. }
  if FDestructionNotifications <> nil then
  begin
    FDestructionNotifications.ExecuteAll(Self);
    FreeAndNil(FDestructionNotifications);
  end;

  if FVRML1Children <> nil then
    VRML1ChildrenClear;

  if PrototypeInstance then
  begin
    FreeAndNil(FPrototypeInstanceSourceNode);
    FreeAndNil(FPrototypeInstanceHelpers);
    FPrototypeInstance := false;
  end;

  FreeAndNil(FPrototypes);
  FreeAndNil(FRoutes);
  FreeAndNil(FImports);
  FreeAndNil(FExports);
  FreeAndNil(FFieldsSFNode);
  FreeAndNil(FFieldsMFNode);
  FreeAndNil(FFunctionalityList);

  { First free Fields and Events, before freeing InterfaceDeclarations.
    Reason: Fields and Events may contains references to InterfaceDeclarations
    items (since parsing added them there by PostAddInterfaceDeclaration(IDecl)).
    So these references have to be valid, and omitted by checking
    ParentInterfaceDeclaration <> nil. }

  if FEvents <> nil then
  begin
    for I := 0 to FEvents.Count - 1 do
      if FEvents[I].ParentInterfaceDeclaration = nil then
      begin
        FEvents[I].Free;
        FEvents[I] := nil;
      end;
    FreeAndNil(FEvents);
  end;

  if FFields <> nil then
  begin
    for I := 0 to FFields.Count - 1 do
      if FFields[I].ParentInterfaceDeclaration = nil then
      begin
        FFields[I].Free;
        FFields[I] := nil;
      end;
    FreeAndNil(FFields);
  end;

  FreeAndNil(FInterfaceDeclarations);

  FreeAndNil(FVRML1Children);
  FreeAndNil(FVRML1Parents);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TX3DNode.AddDestructionNotification(const Event: TNodeDestructionNotification);
begin
  { create FDestructionNotifications on demand }
  if FDestructionNotifications = nil then
    FDestructionNotifications := TNodeDestructionNotificationList.Create;
  FDestructionNotifications.Add(Event);
end;

procedure TX3DNode.RemoveDestructionNotification(const Event: TNodeDestructionNotification);
begin
  if FDestructionNotifications <> nil then
    FDestructionNotifications.Remove(Event);
end;

procedure TX3DNode.FreeIfUnused;
begin
  if (Self <> nil) and
     (VRML1ParentsCount = 0) and
     (FParentFields.Count = 0) and
     (KeepExisting = 0) then
  begin
    { For FPC <= 2.2.2:
      This is written as "Self.Destroy" to actually do the desctruction,
      freeing memory etc. If I would just call it "Destroy", it would
      perform what destructor does but leaving object instance unfreed.

      In FPC 2.2.4, 2.4.0, 2.4.2, 2.4.4, 2.6.0, calling Destroy always
      does the actual destruction, there seems to be no difference
      between "Self.Destroy" and "Destroy" below. }
    Self.Destroy;
  end;
end;

procedure TX3DNode.KeepExistingBegin;
begin
  Inc(FKeepExisting);
end;

procedure TX3DNode.KeepExistingEnd;
begin
  Dec(FKeepExisting);
end;

procedure TX3DNode.VRML1ChildAdd(const Index: Integer; const Child: TX3DNode);
begin
  Check(VRML1ChildrenAllowed, 'Node "' + X3DType + '" is not allowed to have child node of type ' + Child.X3DType);

  if Child.FVRML1Parents = nil then
    Child.FVRML1Parents := TX3DNodeList.Create(false);
  Child.FVRML1Parents.Add(Self);

  if FVRML1Children = nil then
    FVRML1Children := TX3DNodeList.Create(false);
  FVRML1Children.Insert(Index, Child);
end;

procedure TX3DNode.VRML1ChildAdd(const Child: TX3DNode);
begin
  VRML1ChildAdd(VRML1ChildrenCount, Child);
end;

procedure TX3DNode.VRML1ChildRemove(const I: Integer);
var
  OldChild: TX3DNode;
begin
  Assert(FVRML1Children <> nil);
  Assert(Between(I, 0, FVRML1Children.Count - 1));

  OldChild := FVRML1Children[I];
  FVRML1Children.Delete(i);
  Assert(OldChild.FVRML1Parents <> nil);
  OldChild.FVRML1Parents.Remove(Self);
  OldChild.FreeIfUnused;
end;

function TX3DNode.VRML1ChildExtract(const I: Integer): TX3DNode;
begin
  Assert(FVRML1Children <> nil);
  Assert(Between(I, 0, FVRML1Children.Count - 1));

  Result := FVRML1Children[I];
  FVRML1Children.Delete(i);
  Assert(Result.FVRML1Parents <> nil);
  Result.FVRML1Parents.Remove(Self);

  { VRML1ChildRemove now does
      OldChild.FreeIfUnused;
    but ExtractChild doesn't do it. }
end;

procedure TX3DNode.SetVRML1Child(const I: Integer; const Value: TX3DNode);
var
  OldChild: TX3DNode;
begin
  Assert(FVRML1Children <> nil);
  Assert(Between(I, 0, FVRML1Children.Count - 1));

  { Inefficient implementation: VRML1ChildRemove(I); VRML1ChildAdd(I, Value); }

  if Value <> FVRML1Children[I] then
  begin
    Check(VRML1ChildrenAllowed,
      'Node '+X3DType+' is not allowed to have child node of type '+
      Value.X3DType);

    OldChild := FVRML1Children[I];
    FVRML1Children[I] := Value;

    Assert(OldChild.FVRML1Parents <> nil);
    OldChild.FVRML1Parents.Remove(Self);
    OldChild.FreeIfUnused;

    if Value.FVRML1Parents = nil then
      Value.FVRML1Parents := TX3DNodeList.Create(false);
    Value.FVRML1Parents.Add(Self);
  end;
end;

procedure TX3DNode.VRML1ChildrenClear;
begin
  while VRML1ChildrenCount > 0 do
    VRML1ChildRemove(0);
end;

function TX3DNode.GetVRML1Child(const I: Integer): TX3DNode;
begin
  Assert(FVRML1Children <> nil);
  Assert(Between(I, 0, FVRML1Children.Count - 1));

  Result := FVRML1Children[I];
end;

function TX3DNode.GetVRML1Parent(const I: Integer): TX3DNode;
begin
  Assert(FVRML1Parents <> nil);
  Assert(Between(I, 0, FVRML1Parents.Count - 1));

  Result := FVRML1Parents[I];
end;

function TX3DNode.VRML1ChildrenCount: Integer;
begin
  if FVRML1Children <> nil then
    Result := FVRML1Children.Count
  else
    Result := 0;
end;

function TX3DNode.VRML1ParentsCount: Integer;
begin
  if FVRML1Parents <> nil then
    Result := FVRML1Parents.Count
  else
    Result := 0;
end;

procedure TX3DNode.FreeRemovingFromAllParents;
var
  i, j: integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  if Self = nil then exit;

  for i := 0 to VRML1ParentsCount - 1 do
  begin
    Assert(FVRML1Parents[i].FVRML1Children <> nil);
    j := FVRML1Parents[i].FVRML1Children.IndexOf(Self);
    FVRML1Parents[i].FVRML1Children.Delete(j);
    { nie musimy sie tu martwic usuwaniem naszego Parenta z listy
      FVRML1Parents ktora
     wlasnie przegladamy bo przeciez i tak zaraz zrobimy sobie Destroy; }
  end;

  for I := 0 to FParentFields.Count - 1 do
  begin
    if FParentFields[I] is TSFNode then
    begin
      SF := TSFNode(FParentFields[I]);
      { We remove accessing private SF.FValue,
        not SF.Value property setter,
        to avoid checking our reference count (and possibly calling
        our destructor) by this setter. }
      SF.FValue := nil;
    end else
    if FParentFields[I] is TMFNode then
    begin
      MF := TMFNode(FParentFields[I]);
      { Again we remove using internal methods, that shouldn't be used
        by normal usage from outside: we call directly FItems methods
        (instead of calling MFNode.RemoveItem method that would call our
        RemoveParentField that possibly calls our destructor). }
      J := MF.FItems.IndexOf(Self);
      Assert(J <> -1, 'Node must be present on Items list of parent MFNode');
      MF.FItems.Delete(J);
    end else
      raise EInternalError.Create('TX3DNode.ParentFields not SF or MF Node class');
  end;

  Self.Destroy;
end;

function TX3DNode.GetParentFieldsItem(Index: Integer): TX3DField;
begin
  Result := FParentFields[Index];
end;

function TX3DNode.GetParentFieldsNodeItem(Index: Integer): TX3DNode;
var
  F: TX3DField;
begin
  F := ParentFields[Index];
  if F is TSFNode then
    Result := TSFNode(F).ParentNode else
    Result := (F as TMFNode).ParentNode;
end;

function TX3DNode.ParentFieldsCount: Integer;
begin
  Result := FParentFields.Count;
end;

function TX3DNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to VRML1ChildrenCount - 1 do
  begin
    Result := Func(Self, VRML1Children[I]);
    if Result <> nil then Exit;
  end;
end;

function TX3DNode.DirectEnumerateActiveForTraverse(
  Func: TEnumerateChildrenFunction;
  StateStack: TX3DGraphTraverseStateStack): Pointer;
begin
  Result := DirectEnumerateActive(Func);
end;

function TX3DNode.DirectEnumerateAll(
  Func: TEnumerateChildrenFunction): Pointer;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  Result := nil;

  for I := 0 to VRML1ChildrenCount - 1 do
  begin
    Result := Func(Self, VRML1Children[I]);
    if Result <> nil then Exit;
  end;

  if FFieldsSFNode <> nil then
    for I := 0 to FFieldsSFNode.Count - 1 do
    begin
      SF := TSFNode(FFieldsSFNode[I]);
      if (SF.Value <> nil) and not SF.WeakLink then
      begin
        Result := Func(Self, SF.Value);
        if Result <> nil then Exit;
      end;
    end;

  { Use InternalItems for an extra epsilon of speed. }

  if FFieldsMFNode <> nil then
    for I := 0 to FFieldsMFNode.Count - 1 do
    begin
      MF := TMFNode(FFieldsMFNode[I]);
      for J := 0 to MF.InternalItems.Count - 1 do
      begin
        Result := Func(Self, MF.InternalItems[J]);
        if Result <> nil then Exit;
      end;
    end;

  if PrototypeInstance then
  begin
    Assert(PrototypeInstanceSourceNode <> nil);
    Result := Func(Self, PrototypeInstanceSourceNode);
    if Result <> nil then Exit;

    if PrototypeInstanceHelpers <> nil then
    begin
      Result := Func(Self, PrototypeInstanceHelpers);
      if Result <> nil then Exit;
    end;
  end;
end;

function TX3DNode.DirectEnumerate(
  Func: TEnumerateChildrenFunction;
  OnlyActive: boolean): Pointer;
begin
  if OnlyActive then
    Result := DirectEnumerateActive(Func) else
    Result := DirectEnumerateAll(Func);
end;

procedure TX3DNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  if PrototypeInstance then
    Inc(StateStack.Top.InsidePrototype);
end;

procedure TX3DNode.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
end;

procedure TX3DNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  if PrototypeInstance then
    Dec(StateStack.Top.InsidePrototype);
end;

type
  TTraverseEnumerator = class
    StateStack: TX3DGraphTraverseStateStack;
    NodeClass: TX3DNodeClass;
    TraversingFunc: TTraversingFunc;
    ParentInfo: PTraversingInfo;
    function DoTraverseIntoChildren(Child: TX3DNode): Pointer;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TTraverseEnumerator.DoTraverseIntoChildren(Child: TX3DNode): Pointer;
  var
    ParentInfoForChildren: TTraversingInfo;
  begin
    ParentInfoForChildren.Node := Child;
    ParentInfoForChildren.ParentInfo := ParentInfo;
    ParentInfo := @ParentInfoForChildren;
    try
      Result := Child.DirectEnumerateActiveForTraverse(
        {$ifdef FPC}@{$endif} EnumerateChildrenFunction, StateStack);
    finally
      ParentInfo := ParentInfoForChildren.ParentInfo;
    end;
  end;

  function TTraverseEnumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  var
    StateNode: TVRML1StateNode;
    IsTraverseIntoChildren: boolean;
  begin
    Result := nil;

    Child.BeforeTraverse(StateStack);
    try
      IsTraverseIntoChildren := true;
      if Child is NodeClass then
      begin
        Result := TraversingFunc(Child, StateStack, ParentInfo, IsTraverseIntoChildren);
        if Result <> nil then Exit;
      end;

      Child.MiddleTraverse(StateStack);

      if IsTraverseIntoChildren then
      begin
        Result := DoTraverseIntoChildren(Child);
        if Result <> nil then Exit;
      end;
    finally Child.AfterTraverse(StateStack) end;

    if Child.VRML1StateNode(StateNode) then
      StateStack.Top.VRML1State.Nodes[StateNode] := Child;
  end;

function TX3DNode.TraverseInternal(StateStack: TX3DGraphTraverseStateStack;
  NodeClass: TX3DNodeClass;
  TraversingFunc: TTraversingFunc;
  ParentInfo: PTraversingInfo): Pointer;
var
  Enumerator: TTraverseEnumerator;
begin
  Enumerator := TTraverseEnumerator.Create;
  try
    Enumerator.StateStack := StateStack;
    Enumerator.NodeClass := NodeClass;
    Enumerator.TraversingFunc := TraversingFunc;
    Enumerator.ParentInfo := ParentInfo;
    Result := Enumerator.EnumerateChildrenFunction(nil, Self);

    { Check that, if all went without exception, Enumerator.ParentInfo
      returned to original state. }
    Assert(Enumerator.ParentInfo = ParentInfo);
  finally FreeAndNil(Enumerator) end;
end;

function TX3DNode.TraverseIntoChildren(StateStack: TX3DGraphTraverseStateStack;
  NodeClass: TX3DNodeClass;
  TraversingFunc: TTraversingFunc;
  ParentInfo: PTraversingInfo): Pointer;
var
  Enumerator: TTraverseEnumerator;
begin
  Enumerator := TTraverseEnumerator.Create;
  try
    Enumerator.StateStack := StateStack;
    Enumerator.NodeClass := NodeClass;
    Enumerator.TraversingFunc := TraversingFunc;
    Enumerator.ParentInfo := ParentInfo;
    Result := Enumerator.DoTraverseIntoChildren(Self);

    { Check that, if all went without exception, Enumerator.ParentInfo
      returned to original state. }
    Assert(Enumerator.ParentInfo = ParentInfo);
  finally FreeAndNil(Enumerator) end;
end;

var
  TraverseSingleStack: TX3DGraphTraverseStateStack;

function TX3DNode.Traverse(NodeClass: TX3DNodeClass;
  TraversingFunc: TTraversingFunc): Pointer;
begin
  TraverseSingleStack.PushClear;
  try
    Result := TraverseInternal(TraverseSingleStack, NodeClass, TraversingFunc, nil);
  finally TraverseSingleStack.Pop end;
end;

function TX3DNode.X3DType: string;
begin
 result := ClassX3DType;
end;

function TX3DNode.NodeTypeName: string;
begin
 result := X3DType;
end;

class function TX3DNode.ClassX3DType: string;
begin
 result := '';
end;

function BlenderRelativePath(const RelativePath: string): boolean;
begin
  Result := IsPrefix('//', RelativePath, false);
end;

function TX3DNode.PathFromBaseUrl(const RelativePath: string): string;
begin
  { This is a workaround for Blender errorneous VRML 1.0 export.
    Blender exports relative paths by prefixing them by "//"
    (that's a general convention used internally by Blender, AFAIK).
    Here we simply remove this "//". }
  if BlenderRelativePath(RelativePath) then
    Result := CombineURI(BaseUrl, SEnding(RelativePath, 3))
  else
    Result := CombineURI(BaseUrl, RelativePath);
end;

procedure TX3DNode.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  Handled: boolean;
  Position: Integer;
  ChildNode: TX3DNode;
begin
  VRML1ChildrenClear;

  Position := 0;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  while Lexer.Token <> vtCloseCurlyBracket do
  begin
    Handled := ParseNodeBodyElement(Lexer, Reader, Position);

    { VRML 1.0 children nodes are handled as a last resort here
      (that's also why they can't be inside our ParseNodeBodyElement).
      That's because ParseNode just raises exception in case of unknown
      node, so I have to catch first everything else (like "hints" field
      of TShapeHintsNode_1). }
    if not Handled then
    begin
      if VRML1ChildrenParsingAllowed then
      begin
        ChildNode := ParseNode(Lexer, Reader, false);
        ChildNode.PositionInParent := Position;
        VRML1ChildAdd(ChildNode);
      end else
      begin
        raise EX3DParserError.Create(Lexer,
          Format('Invalid X3D node content (probably unknown or not allowed field, prototype or VRML 1.0-style children) inside "%s": got %s',
            [X3DType, Lexer.DescribeToken]));
      end;
    end;

    Inc(Position);
  end;
  Lexer.NextToken;

  FBaseUrl := Reader.BaseUrl;

  ParseAfter(Reader);
end;

function TX3DNode.ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
  const APositionInParent: Integer): boolean;

  procedure ParseExtensibilityFields;

    procedure ReadOneField;
    var
      X3DType: string;
      //FieldName: string;
      FieldType: TX3DFieldClass;
    begin
      Lexer.CheckTokenIs(vtName, 'Field type name');
      X3DType := Lexer.TokenName;
      FieldType := X3DFieldsManager.X3DTypeToClass(X3DType);
      if FieldType = nil then
        raise EX3DParserError.Create(
          Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

      Lexer.NextToken;

      Lexer.CheckTokenIs(vtName, 'Field name');
      //FieldName := Lexer.TokenName;

      Lexer.NextToken;

      { TODO: we should actually do something with obtained here
        FieldName, FieldType }
    end;

  begin
    { We parse VRML 1.0 "fields" extensibility feature in a way similar to
      MF fields syntax, this was the intention (although not clarified precisely)
      of VRML 1.0 spec. }
    if Lexer.Token = vtOpenSqBracket then
    begin
      Lexer.NextToken;

      while Lexer.Token <> vtCloseSqBracket do
      begin
        ReadOneField;

        if Lexer.Token = vtCloseSqBracket then break;

        { In VRML >= 2.0 the comma is simply a whitespace and will be ignored
          by the lexer. }
        if Lexer.Version.Major < 2 then
        begin
          Lexer.CheckTokenIs(vtComma);
          Lexer.NextToken;
        end;
      end;

      { consume final "]" }
      Lexer.NextToken;
    end else
    begin
      { one single field - not enclosed in [] brackets }
      ReadOneField;
    end;
  end;

  procedure ParseExtensibilityIsA;
  var
    IsAField: TMFString;
  begin
    IsAField := TMFString.Create(Self, false, '', []);
    try
      IsAField.Parse(Lexer, Reader, false);

      { TODO: we should actually do something with obtained here
        isA value }
    finally FreeAndNil(IsAField) end;
  end;

var
  I: Integer;
  Route: TX3DRoute;
  Proto: TX3DPrototypeBase;
  Event: TX3DEvent;
  IDecl: TX3DInterfaceDeclaration;
  Import: TX3DImport;
  ExportItem: TX3DExport; { "export" is a keyword in Pascal }
begin
  Result := false;

  { If I would know that all fields used are standard, I could
    check first for if Lexer.TokenName[0] in ['a'..'z'], since all
    standard field names start lowercase. But of course I can't,
    all VRML versions allow to define your own nodes and fields. }
  if Lexer.Token = vtName then
  begin
    I := IndexOfField(Lexer.TokenName);
    if I >= 0 then
    begin
      Result := true;

      { Advance to the next token. Usually, it should be just "Lexer.NextToken;"
        But I have to add here some dirty hack to allow SFString fields
        to contain strings not enclosed in double quotes in VRML 1.0.
        So I call here NextTokenForceVTString before SFString field.

        For VRML >= 2.0, this nonsense feature was fortunately removed,
        and that's good because in VRML >= 2.0 you must be able to use
        keyword "IS" here, so calling NextTokenForceVTString would be bad. }
      if (Fields[I] is TSFString) and (Lexer.Version.Major <= 1) then
        Lexer.NextTokenForceVTString else
        Lexer.NextToken;

      Fields[I].Parse(Lexer, Reader, true);
      Fields[I].PositionInParent := APositionInParent;
    end else
    begin
      Event := AnyEvent(Lexer.TokenName);
      if Event <> nil then
      begin
        Result := true;
        Lexer.NextToken;
        Event.Parse(Lexer);
        Event.PositionInParent := APositionInParent;
      end else
      if Lexer.TokenName = 'fields' then
      begin
        Result := true;
        Lexer.NextToken;
        ParseExtensibilityFields;
      end else
      if Lexer.TokenName = 'isA' then
      begin
        Result := true;
        Lexer.NextToken;
        ParseExtensibilityIsA;
      end;
    end;
  end else
  if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(HasInterfaceDeclarations)) then
  begin
    Result := true;

    { since we're here, HasInterfaceDeclarations is <> [] }
    Assert(InterfaceDeclarations <> nil);

    IDecl := TX3DInterfaceDeclaration.Create(Self);
    InterfaceDeclarations.Add(IDecl);
    IDecl.Parse(Lexer, Reader, true, true);
    IDecl.PositionInParent := APositionInParent;
    PostAddInterfaceDeclaration(IDecl);
  end else
  if Lexer.TokenIsKeyword(vkPROTO) then
  begin
    Result := true;

    Proto := TX3DPrototype.Create;
    AddPrototype(Proto);
    Proto.Parse(Lexer, Reader);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkEXTERNPROTO) then
  begin
    Result := true;

    Proto := TX3DExternalPrototype.Create;
    AddPrototype(Proto);
    Proto.Parse(Lexer, Reader);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkROUTE) then
  begin
    Result := true;

    Route := TX3DRoute.Create;
    AddRoute(Route);
    Route.Parse(Lexer, Reader);
    Route.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkIMPORT) then
  begin
    Result := true;

    Import := TX3DImport.Create;
    AddImport(Import);
    Import.Parse(Lexer, Reader);
    Import.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkEXPORT) then
  begin
    Result := true;

    ExportItem := TX3DExport.Create;
    AddExport(ExportItem);
    ExportItem.Parse(Lexer, Reader);
    ExportItem.PositionInParent := APositionInParent;
  end;
end;

procedure TX3DNode.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  Position: Integer;

  procedure ParseXMLAttributes;
  var
    Attr: TDOMAttr;
    AttrNode: TDOMNode;
    AttrIndex, Index: Integer;
  begin
    { enumerate over all attributes }
    for AttrIndex := 0 to Integer(Element.Attributes.Length) - 1 do
    begin
      AttrNode := Element.Attributes[AttrIndex];
      Assert(AttrNode.NodeType = ATTRIBUTE_NODE);
      Attr := AttrNode as TDOMAttr;

      { containerField and DEF attributes are handled in ParseNode,
        we can safely ignore them now. }
      if (Attr.Name = SAttrContainerField) or
         (Attr.Name = SAttrDEF) then
        Continue;

      Index := IndexOfField(Attr.NodeName8);
      if Index >= 0 then
      begin
        Fields[Index].ParseXMLAttribute(Attr.NodeValue8, Reader);
        { Note that saving PositionInParent from XML attributes order
          isn't really useful, as FPC DOM unit already sorted
          Element.Attributes (TDOMNamedNodeMap) by name. }
        Fields[Index].PositionInParent := Position;
        Inc(Position);
      end else
        WritelnWarning('X3D', 'Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.NodeName8 + '" in node "' + X3DType + '"');
    end;
  end;

  procedure ParseXMLChildrenNodes;
  var
    FieldIndex: Integer;
    Child: TX3DNode;
    ContainerField: string;
    SF: TSFNode;
    MF: TMFNode;
    Route: TX3DRoute;
    I: TXMLElementIterator;
    Proto: TX3DPrototype;
    ExternProto: TX3DExternalPrototype;
    IDecl: TX3DInterfaceDeclaration;
    Import: TX3DImport;
    ExportItem: TX3DExport;
  begin
    I := Element.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName = 'ROUTE' then
        begin
          Route := TX3DRoute.Create;
          Route.PositionInParent := Position;
          AddRoute(Route);
          Route.ParseXML(I.Current, Reader);
        end else
        if I.Current.TagName = 'IS' then
        begin
          ParseISStatement(Self, I.Current, Position);
        end else
        if I.Current.TagName = 'IMPORT' then
        begin
          Import := TX3DImport.Create;
          Import.PositionInParent := Position;
          AddImport(Import);
          Import.ParseXML(I.Current, Reader);
        end else
        if I.Current.TagName = 'EXPORT' then
        begin
          ExportItem := TX3DExport.Create;
          ExportItem.PositionInParent := Position;
          AddExport(ExportItem);
          ExportItem.ParseXML(I.Current, Reader);
        end else
        if I.Current.TagName = 'ProtoDeclare' then
        begin
          Proto := TX3DPrototype.Create;
          Proto.PositionInParent := Position;
          AddPrototype(Proto);
          Proto.ParseXML(I.Current, Reader);
        end else
        if I.Current.TagName = 'ExternProtoDeclare' then
        begin
          ExternProto := TX3DExternalPrototype.Create;
          ExternProto.PositionInParent := Position;
          AddPrototype(ExternProto);
          ExternProto.ParseXML(I.Current, Reader);
        end else
        if I.Current.TagName = 'field' then
        begin
          IDecl := TX3DInterfaceDeclaration.Create(Self);
          try
            IDecl.ParseXML(I.Current, Reader, true);
            IDecl.PositionInParent := Position;
            if IDecl.AccessType in HasInterfaceDeclarations then
            begin
              InterfaceDeclarations.Add(IDecl);
              PostAddInterfaceDeclaration(IDecl);
            end else
            begin
              FreeAndNil(IDecl);
              WritelnWarning('X3D', 'X3D XML: specified <field> inside node, but this node doesn''t allow interface declaration with such accessType');
            end;
          except
            FreeAndNil(IDecl);
            raise;
          end;
        end else
        begin
          Child := ParseXMLNode(I.Current, ContainerField, Reader, true);
          if Child <> nil then
          try
            Child.PositionInParent := Position;
            FieldIndex := IndexOfField(ContainerField);

            if (FieldIndex = -1) and
               (ContainerField <> Child.DefaultContainerField) and
               (Child.DefaultContainerField <> '') then
            begin
              { Retry with DefaultContainerField value, since it exists
                and is different than current ContainerField. }
              FieldIndex := IndexOfField(Child.DefaultContainerField);
              if FieldIndex >= 0 then
                WritelnWarning('X3D', 'X3D XML: containerField indicated unknown field name ("' + ContainerField + '" by node "' + Child.X3DType + '" inside node "' + X3DType + '"), using the default containerField value "' + Child.DefaultContainerField + '" succeeded');
            end;

            if FieldIndex >= 0 then
            begin
              if Fields[FieldIndex] is TSFNode then
              begin
                SF := Fields[FieldIndex] as TSFNode;

                if SF.Value <> nil then
                  WritelnWarning('X3D', 'X3D XML: More than one value specified for SFNode field ' + SF.NiceName);

                { Although field doesn't have a set position in XML X3D
                  encoding, when saving later in classic encoding we
                  need some order of fields. This is yet another problem
                  with non-unique names, something defined in XML X3D
                  may be not possible to save in other encoding:

                  <Group>
                    <Shape> ... <Appearance DEF="XXX" ....> </Shape>
                    <ROUTE ... using "XXX" name ...>
                    <Shape> ... <Appearance DEF="XXX" ....> </Shape>
                    <ROUTE ... using "XXX" name ...>
                  </Group>

                  This is uneasy to save in classic encoding, since
                  you cannot insert ROUTE in the middle of "children"
                  field of Group node in classic encoding.
                }
                Fields[FieldIndex].PositionInParent := Position;
                SF.Value := Child;
                SF.WarningIfChildNotAllowed(Child);
              end else
              if Fields[FieldIndex] is TMFNode then
              begin
                MF := Fields[FieldIndex] as TMFNode;
                Fields[FieldIndex].PositionInParent := Position;
                MF.Add(Child);
                MF.WarningIfChildNotAllowed(Child);
              end else
                WritelnWarning('X3D', 'X3D field "' + ContainerField + '" is not SFNode or MFNode, but a node value (XML element) is specified');
            end else
              WritelnWarning('X3D', 'Unknown X3D field name (indicated by containerField value) "' + ContainerField + '" by node "' + Child.X3DType + '" inside node "' + X3DType + '"');
          finally
            { Any WritelnWarning above makes the Child unused.
              Note that WritelnWarning may, but doesn't have, raise an exception. }
            Child.FreeIfUnused;
            Child := nil;
          end;
        end;
        Inc(Position);
      end;
    finally FreeAndNil(I) end;
  end;

  procedure ParseXMLCdata;
  var
    I: TXMLCDataIterator;
    CDataExists: boolean;
    CData: string;
  begin
    CDataExists := false;
    CData := '';

    I := TXMLCDataIterator.Create(Element);
    try
      if I.GetNext then
      begin
        CDataExists := true;
        repeat
          CData := CData + I.Current;
        until not I.GetNext;
      end;
    finally FreeAndNil(I) end;

    if CDataExists then
      if CDataField <> nil then
        CDataField.Items.Add(CData) else
        WritelnWarning('X3D', Format('X3D node %s doesn''t allow CDATA section, but it''s specified',
          [X3DType]));
  end;

begin
  Position := 0;
  { The order below is important: first parse XML attributes,
    then elements, since VRML DEF mechanism says that DEF order
    is significant. }
  ParseXMLAttributes;
  ParseXMLChildrenNodes;
  { This must be called *after* parsing fields, to add CDATA at the *end*
    of "url" fields, like spec requires. }
  ParseXMLCdata;

  ParseAfter(Reader);
end;

procedure TX3DNode.ParseAfter(Reader: TX3DReaderNames);
begin
end;

procedure TX3DNode.CreateNode;
begin
end;

type
  TEnumerateNodes0Enumerator = class
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes0Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    Proc(Child);
    Child.DirectEnumerate({$ifdef FPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes0Enumerator;
begin
  Enumerator := TEnumerateNodes0Enumerator.Create;
  try
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes1Enumerator = class
    NodeClass: TX3DNodeClass;
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes1Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    if Child is NodeClass then Proc(Child);
    Child.DirectEnumerate({$ifdef FPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(nodeClass: TX3DNodeClass;
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes1Enumerator;
begin
  Enumerator := TEnumerateNodes1Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes2Enumerator = class
    NodeClass: TX3DNodeClass;
    SeekNodeName: string;
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes2Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    if (Child is NodeClass) and
       (Child.X3DName = SeekNodeName) then
      Proc(Child);
    Child.DirectEnumerate({$ifdef FPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(NodeClass: TX3DNodeClass;
  const SeekNodeName: string;
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes2Enumerator;
begin
  Enumerator := TEnumerateNodes2Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.SeekNodeName := SeekNodeName;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TSearchNodes0Helper = class
    Proc: TX3DNodeSearchProc;
    OnlyActive: boolean;
    function Callback(Node, Child: TX3DNode): Pointer;
  end;

  function TSearchNodes0Helper.Callback(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := Proc(Child);
    if Result <> nil then Exit;

    Result := Child.DirectEnumerate({$ifdef FPC}@{$endif} Self.Callback, OnlyActive);
  end;

function TX3DNode.SearchNodes(
  const Proc: TX3DNodeSearchProc; const OnlyActive: boolean): Pointer;
var
  Helper: TSearchNodes0Helper;
begin
  Helper := TSearchNodes0Helper.Create;
  try
    Helper.Proc := Proc;
    Helper.OnlyActive := OnlyActive;
    Result := Helper.Callback(nil, Self);
  finally FreeAndNil(Helper) end;
end;

type
  TFindNodeHelper = class
    NodeClass: TX3DNodeClass;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TFindNodeHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if Node is NodeClass then
      Result := Node else
      Result := nil;
  end;

function TX3DNode.TryFindNode(FindClass: TX3DNodeClass; OnlyActive: boolean): TX3DNode;
var
  Helper: TFindNodeHelper;
begin
  Helper := TFindNodeHelper.Create;
  try
    Helper.NodeClass := FindClass;
    Result := TX3DNode(SearchNodes({$ifdef FPC}@{$endif} Helper.Callback, OnlyActive));
  finally FreeAndNil(Helper) end;
end;

function TX3DNode.FindNode(FindClass: TX3DNodeClass; OnlyActive: boolean): TX3DNode;
begin
  {$warnings off} // usind deprecated in deprecated
  result := TryFindNode(FindClass, OnlyActive);
  {$warnings on}
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node class "%s" not found', [FindClass.ClassName]);
end;

type
  TFindNodeByNameHelper = class
    NodeClass: TX3DNodeClass;
    NodeName: string;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TFindNodeByNameHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if (Node is NodeClass) and (Node.X3DName = NodeName) then
      Result := Node else
      Result := nil;
  end;

function TX3DNode.TryFindNodeByName(
  FindClass: TX3DNodeClass; const FindName: string; OnlyActive: boolean): TX3DNode;
var
  Helper: TFindNodeByNameHelper;
begin
  Helper := TFindNodeByNameHelper.Create;
  try
    Helper.NodeClass := FindClass;
    Helper.NodeName := FindName;
    Result := TX3DNode(SearchNodes({$ifdef FPC}@{$endif} Helper.Callback, OnlyActive));
  finally FreeAndNil(Helper) end;
end;

function TX3DNode.FindNodeByName(
  FindClass: TX3DNodeClass; const FindName: string;
  OnlyActive: boolean): TX3DNode;
begin
  {$warnings off} // usind deprecated in deprecated
  Result := TryFindNodeByName(FindClass, FindName, OnlyActive);
  {$warnings on}
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node name "%s" of class "%s" not found', [
      FindName,
      FindClass.ClassName
    ]);
end;

function TX3DNode.FindNode(const FindName: string): TX3DNode;
begin
  Result := FindNode(TX3DNode, FindName);
end;

function TX3DNode.FindNode(const NodeClass: TX3DNodeClass; const FindName: string;
  const Options: TFindNodeOptions): TX3DNode;
begin
  {$warnings off} // usind deprecated, but in the long run this all should be collapses into non-deprecated method
  Result := TryFindNodeByName(NodeClass, FindName, fnOnlyActive in Options);
  {$warnings on}
  if (Result = nil) and (not (fnNilOnMissing in Options)) then
    raise EX3DNotFound.CreateFmt('Node name "%s" of class "%s" not found', [
      FindName,
      NodeClass.ClassName
    ]);
end;

{$ifdef GENERIC_METHODS}

{$ifdef FPC}generic{$endif} function TX3DNode.Find<T>(const FindName: string;
  const Options: TFindNodeOptions): T;
begin
  {$warnings off} // usind deprecated, but in the long run this all should be collapses into non-deprecated method
  Result := TryFindNodeByName(T, FindName, fnOnlyActive in Options) as T;
  {$warnings on}
  if (Result = nil) and (not (fnNilOnMissing in Options)) then
    raise EX3DNotFound.CreateFmt('Node name "%s" of class "%s" not found', [
      FindName,
      T.ClassName
    ]);
end;

{$endif}

{ TX3DNode.TryFindNodeState/Transform ----------------------------------------- }

type
  TTryFindNodeStateResult = class
    Node: TX3DNode;
    State: TX3DGraphTraverseState;
  end;

function TX3DNode.TryFindNodeStateTraverse(
  ANode: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
var
  Res: TTryFindNodeStateResult;
begin
  Res := TTryFindNodeStateResult.Create;
  Res.Node := ANode;
  Res.State := TX3DGraphTraverseState.CreateCopy(StateStack.Top);
  Result := Res;
end;

function TX3DNode.TryFindNodeState(
  NodeClass: TX3DNodeClass;
  out Node: TX3DNode; out State: TX3DGraphTraverseState): boolean;
var
  Res: TTryFindNodeStateResult;
begin
  Res := TTryFindNodeStateResult(Traverse(NodeClass, {$ifdef FPC}@{$endif} TryFindNodeStateTraverse));
  Result := Res <> nil;
  if Result then
  begin
    Node := Res.Node;
    State := Res.State;
  end;
end;

type
  TTryFindNodeTransformResult = class
    Node: TX3DNode;
    Transform: TMatrix4;
    TransformScale: Single;
  end;

function TX3DNode.TryFindNodeTransformTraverse(
  ANode: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
var
  Res: TTryFindNodeTransformResult;
begin
  Res := TTryFindNodeTransformResult.Create;
  Res.Node := ANode;
  Res.Transform := StateStack.Top.Transformation.Transform;
  Res.TransformScale := StateStack.Top.Transformation.Scale;
  Result := Res;
end;

function TX3DNode.TryFindNodeTransform(
  NodeClass: TX3DNodeClass;
  out Node: TX3DNode;
  out Transform: TMatrix4;
  out TransformScale: Single): boolean;
var
  Res: TTryFindNodeTransformResult;
begin
  Res := TTryFindNodeTransformResult(Traverse(NodeClass,
    {$ifdef FPC}@{$endif} TryFindNodeTransformTraverse));
  Result := Res <> nil;
  if Result then
  begin
    Node := Res.Node;
    Transform := Res.Transform;
    TransformScale := Res.TransformScale;
  end;
end;

function TX3DNode.TryFindParentByName(const FindName: string): TX3DNode;
var
  I: Integer;
begin
  if X3DName = FindName then
    result := Self else
  begin
    result := nil;

    for I := 0 to VRML1ParentsCount - 1 do
    begin
      result := VRML1Parents[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;

    for I := 0 to ParentFieldsCount - 1 do
    begin
      result := ParentFieldsNode[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;
  end;
end;

function TX3DNode.FindParentByName(const FindName: string): TX3DNode;
begin
  result := TryFindParentByName(FindName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node name "%s" not found in parents', [FindName]);
end;

function TX3DNode.TryFindDirectParentByName(const FindName: string): TX3DNode;
var
  I: Integer;
begin
  for I := 0 to VRML1ParentsCount - 1 do
  begin
    Result := VRML1Parents[I];
    if Result.X3DName = FindName then Exit;
  end;

  for I := 0 to ParentFieldsCount - 1 do
  begin
    Result := ParentFieldsNode[I];
    if Result.X3DName = FindName then Exit;
  end;

  Result := nil;
end;

function TX3DNode.HasParentNode(Node: TX3DNode): boolean;
var
  I: Integer;
begin
  if Self = Node then
    result := true else
  begin
    for i := 0 to VRML1ParentsCount - 1 do
      if VRML1Parents[i].HasParentNode(Node) then Exit(true);

    for i := 0 to ParentFieldsCount - 1 do
      if ParentFieldsNode[i].HasParentNode(Node) then Exit(true);

    result := False;
  end;
end;

type
  TIsNodePresentHelper = class
    SeekNode: TX3DNode;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TIsNodePresentHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if Node = SeekNode then
      Result := Node { doesn't matter what, anything non-nil } else
      Result := nil;
  end;

function TX3DNode.IsNodePresent(Node: TX3DNode; OnlyActive: boolean): boolean;
var
  Helper: TIsNodePresentHelper;
begin
  Helper := TIsNodePresentHelper.Create;
  try
    Helper.SeekNode := Node;
    Result := SearchNodes({$ifdef FPC}@{$endif} Helper.Callback, OnlyActive) <> nil;
  finally FreeAndNil(Helper) end;
end;

type
  TNodesCountHelper = class
    Counter: integer;
    procedure CountNode(node: TX3DNode);
  end;

  procedure TNodesCountHelper.CountNode(node: TX3DNode);
  begin Inc(Counter) end;

function TX3DNode.NodesCount(NodeClass: TX3DNodeClass;
  CountOnlyActiveNodes: boolean): integer;
var
  C: TNodesCountHelper;
begin
  C := TNodesCountHelper.Create;
  try
    EnumerateNodes(NodeClass, {$ifdef FPC}@{$endif} C.CountNode, CountOnlyActiveNodes);
    result := C.Counter;
  finally C.Free end;
end;

procedure TX3DNode.SaveToStream(Writer: TX3DWriter);
begin
  NodeSaveToStream(Writer);
  if Writer.Encoding = xeClassic then Writer.Writeln;
end;

procedure TX3DNode.NodeSaveToStream(Writer: TX3DWriter; const CurrentContainerField: string);

  { Collect node contents to save.
    You want to save here exactly what you read in ParseNodeBodyElement. }
  function ContentsToSave: TX3DFileItemList;
  var
    I: Integer;
  begin
    Result := TX3DFileItemList.Create(false);
    try
      if HasInterfaceDeclarations <> [] then
      begin
        for I := 0 to InterfaceDeclarations.Count - 1 do
        begin
          { Saving interface declaration within X3D node (like custom Effect or ComposedShader field)
            should always save the value, as there's no intrinsic "default".

            Testcase: terrain demo saving fields, they have defaults often equal to current value,
            and could result in incorrect X3D XML like:

            <field accessType="inputOutput" type="SFFloat" name="h0">5</field>

            instead of

            <field accessType="inputOutput" type="SFFloat" name="h0" value="5" />
          }
          if InterfaceDeclarations[I].Field <> nil then
            InterfaceDeclarations[I].Field.UnassignDefaultValue;
          Result.Add(InterfaceDeclarations[I]);
        end;
      end;
      for I := 0 to PrototypesCount - 1 do
        Result.Add(Prototypes[I]);

      for I := 0 to FieldsCount - 1 do
      begin
        { Saving InterfaceDeclarations already handled saving fields
          with ParentInterfaceDeclaration <> nil, so no need to save them again. }
        if Fields[I].ParentInterfaceDeclaration = nil then
          Result.Add(Fields[I]);

        if Fields[I].Exposed then
        begin
          { exposed events may have their own IS clauses, save them }
          Result.Add(Fields[I].EventIn);
          Result.Add(Fields[I].EventOut);
        end;
      end;

      if VRML1ChildrenSaveToStream then
        for I := 0 to VRML1ChildrenCount - 1 do
          Result.Add(VRML1Children[I]);

      for I := 0 to EventsCount - 1 do
        if { Saving InterfaceDeclarations already handled saving events
             with ParentInterfaceDeclaration <> nil, so no need to save them again. }
           (Events[I].ParentInterfaceDeclaration = nil) then
          Result.Add(Events[I]);

      for I := 0 to RoutesCount - 1 do
        Result.Add(Routes[I]);

      for I := 0 to ImportsCount - 1 do
        Result.Add(ImportsList[I]);

      for I := 0 to ExportsCount - 1 do
        Result.Add(ExportsList[I]);
    except FreeAndNil(Result) end;
  end;

  { Gather IS clauses of fields/events.
    For XML encoding, also gather from fields/events inside interface
    declarations (for classic encoding,
    they are saved by TX3DInterfaceDeclaration.SaveToStream). }
  procedure CollectIsClauses(out NodeFields, ProtoFields: TCastleStringList);

    procedure AddIsClauses(Item: TX3DFieldOrEvent);
    var
      N: string;
      I: Integer;
    begin
      N := Item.NameForVersion(Writer.Version);
      if N = '' then Exit; { unnamed fields/events cannot be saved here }
      for I := 0 to Item.IsClauseNamesCount - 1 do
      begin
        NodeFields.Add(N);
        ProtoFields.Add(Item.IsClauseNames[I]);
      end;
    end;

  var
    I: Integer;
  begin
    NodeFields := TCastleStringList.Create;
    ProtoFields := TCastleStringList.Create;

    for I := 0 to FieldsCount - 1 do
    begin
      if (Writer.Encoding <> xeClassic) or
         (Fields[I].ParentInterfaceDeclaration = nil) then
        AddIsClauses(Fields[I]);

      if Fields[I].Exposed then
      begin
        { exposed events may have their own IS clauses, save them }
        AddIsClauses(Fields[I].EventIn);
        AddIsClauses(Fields[I].EventOut);
      end;
    end;

    for I := 0 to EventsCount - 1 do
      if (Writer.Encoding <> xeClassic) or
         (Events[I].ParentInterfaceDeclaration = nil) then
        AddIsClauses(Events[I]);
  end;

  procedure Classic;
  var
    FileItems: TX3DFileItemList;
    NodeFields, ProtoFields: TCastleStringList;
    I: Integer;
  begin
    if (Writer as TX3DWriterNames).NodeNames.Bound(Self) then
    begin
      Writer.WriteIndent('USE ' + EncodeX3DName(X3DName));
    end else
    begin
      (Writer as TX3DWriterNames).NodeNames.Bind(Self, false);

      CollectIsClauses(NodeFields, ProtoFields);
      try
        Writer.WriteIndent('');
        if X3DName <> '' then Writer.Write('DEF ' + EncodeX3DName(X3DName) + ' ');
        Writer.Writeln(X3DType +' {');

        Writer.IncIndent;
        FileItems := ContentsToSave;
        try
          FileItems.SaveToStream(Writer);
        finally FreeAndNil(FileItems) end;

        { save IS clauses }
        for I := 0 to NodeFields.Count - 1 do
          Writer.WritelnIndent(Format('%s IS %s', [NodeFields[I], ProtoFields[I]]));
        Writer.DecIndent;

        Writer.WriteIndent('}');
      finally
        FreeAndNil(NodeFields);
        FreeAndNil(ProtoFields);
      end;

      (Writer as TX3DWriterNames).NodeNames.Bind(Self, true);
    end;
  end;

  procedure Xml;
  var
    FileItems: TX3DFileItemList;
    HasContent: boolean;
    I: Integer;
    WriteContainerField: string;
    ElementName: string;
    ProtoInstance: boolean;
    NodeFields, ProtoFields: TCastleStringList;
  begin
    { Knowing CurrentContainerField and DefaultContainerField,
      we can always correctly set containerField= ourselves. }
    if (CurrentContainerField <> '') and
       (CurrentContainerField <> DefaultContainerField) then
      WriteContainerField := ' containerField=' + StringToX3DXml(CurrentContainerField) else
      WriteContainerField := '';

    { For XML encoding, TX3DPrototypeNode is saved a little specially. }
    ProtoInstance := Self is TX3DPrototypeNode;

    if ProtoInstance then
      ElementName := 'ProtoInstance' else
      ElementName := X3DType;

    if (Writer as TX3DWriterNames).NodeNames.Bound(Self) then
    begin
      Writer.WritelnIndent(Format('<%s USE=%s%s />',
        [ElementName, StringToX3DXml(EncodeX3DName(X3DName)), WriteContainerField]));
    end else
    begin
      (Writer as TX3DWriterNames).NodeNames.Bind(Self, false);

      CollectIsClauses(NodeFields, ProtoFields);
      try
        Writer.WriteIndent('<' + ElementName);
        if ProtoInstance then
          Writer.Write(' name=' + StringToX3DXml(X3DType));
        if X3DName <> '' then Writer.Write(' DEF=' +StringToX3DXml(EncodeX3DName(X3DName)));
        Writer.Write(WriteContainerField);

        HasContent := false;
        FileItems := ContentsToSave;
        try
          FileItems.SortPositionInParent;

          Writer.IncIndent;
          for I := 0 to FileItems.Count - 1 do
            case FileItems[I].SaveToXml of
              sxAttribute, sxAttributeCustomQuotes:
                if ProtoInstance then
                  { for a prototype, every field must be wrapped in <fieldValue> }
                  HasContent := true else
                  FileItems[I].SaveToStream(Writer);
              sxChildElement: HasContent := true;
              else ;
            end;
          Writer.DecIndent;

          if NodeFields.Count <> 0 then
            HasContent := true;

          if HasContent then
          begin
            Writer.Writeln('>');
            Writer.IncIndent;

            for I := 0 to FileItems.Count - 1 do
              if ProtoInstance and
                 (FileItems[I] is TX3DField) and
                 (FileItems[I].SaveToXml <> sxNone) then
              begin
                { prototype instance fields must be wrapped in <fieldValue> }
                Writer.WriteIndent('<fieldValue name=' +
                  StringToX3DXml((FileItems[I] as TX3DField).X3DName));
                if FileItems[I].SaveToXml = sxChildElement then
                begin
                  Writer.Writeln('>');
                  Writer.IncIndent;
                  FileItems[I].SaveToStream(Writer);
                  Writer.DecIndent;
                  Writer.WritelnIndent('</fieldValue>');
                end else
                begin
                  Assert(FileItems[I].SaveToXml in [sxAttribute, sxAttributeCustomQuotes]);
                  Writer.IncIndent;
                  Writer.Writeln;
                  Writer.WriteIndent('value=');
                  (FileItems[I]  as TX3DField).FieldSaveToStream(Writer, true, true);
                  Writer.DecIndent;
                  Writer.Writeln(' />');
                end;
              end else
              if FileItems[I].SaveToXml = sxChildElement then
                FileItems[I].SaveToStream(Writer);

            { save IS clauses }
            { TODO: should be before all, but *after* interface declarations and fieldValue }
            if NodeFields.Count <> 0 then
            begin
              Writer.WritelnIndent('<IS>');
              Writer.IncIndent;
              for I := 0 to NodeFields.Count - 1 do
                Writer.WritelnIndent(Format('<connect nodeField=%s protoField=%s />',
                  [ StringToX3DXml(NodeFields[I]),
                    StringToX3DXml(ProtoFields[I])]));
              Writer.DecIndent;
              Writer.WritelnIndent('</IS>');
            end;

            Writer.DecIndent;
            Writer.WritelnIndent('</' + ElementName + '>');
          end else
            Writer.Writeln(' />');
        finally FreeAndNil(FileItems) end;
      finally
        FreeAndNil(NodeFields);
        FreeAndNil(ProtoFields);
      end;

      (Writer as TX3DWriterNames).NodeNames.Bind(Self, true);
    end;
  end;

begin
  if PrototypeInstance and
     { TX3DPrototypeNode has somewhat different meaning of PrototypeInstance,
       we want to save it directly (otherwise
       PrototypeInstanceSourceNode.SaveToStream could cause another
       recursive PrototypeInstanceSourceNode.SaveToStream with
       nested proto). For example test read + save
       demo_models/x3d/key_sensor.x3dv, to see that check below is needed. }
     not (Self is TX3DPrototypeNode) then
  begin
    { If this is an expanded prototype, then delegate writing to the
      PrototypeInstanceSourceNode. }
    PrototypeInstanceSourceNode.NodeSaveToStream(Writer, CurrentContainerField);

    { What to do about
        NodeNames.Bind(Self)
      called from PrototypeInstanceSourceNode.SaveToStream ?
      This means that PrototypeInstanceSourceNode (TX3DPrototypeNode)
      is bound to given name.
      But when reading, we bound Self node (the actual expanded proto)
      to the same name.
      Routes when saving check this (to make sure correct names are bound).
      So we bind again Self, instead of PrototypeInstanceSourceNode,
      to this name. }

    (Writer as TX3DWriterNames).NodeNames.Bind(Self, true);
  end else
  case Writer.Encoding of
    xeClassic: Classic;
    xeXML    : Xml;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DNode.SaveToStream Encoding?');
    {$endif}
  end;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
class function TX3DNode.VRML1StateNode(out StateNode: TVRML1StateNode): boolean;
var
  SN: TVRML1StateNode;
begin
  { We're in "class function", so Self = reference to our class.
    So simple comparison "Self = ..." is what we want. }
  for SN := Low(SN) to High(SN) do
    if VRML1StateClasses[SN] = Self then
    begin
      StateNode := SN;
      Result := true;
      Exit;
    end;
  Result := false;
end;
{$warnings on}

class function TX3DNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := true;
end;

procedure TX3DNode.RemoveParentField(const Field: TX3DField);
begin
  if FParentFields.Remove(Field) = -1 then
    raise EInternalError.CreateFmt('RemoveParentField cannot find indicated parent, node %s, parent field %s', [
      NiceName,
      Field.NiceName
    ]);
  FreeIfUnused;
end;

procedure TX3DNode.AddParentField(const Field: TX3DField);
begin
  FParentFields.Add(Field);
end;

class function TX3DNode.VRML1ChildrenSaveToStream: boolean;
begin
  Result := true;
end;

function TX3DNode.EnumerateReplaceChildren(
  Func: TEnumerateReplaceNodesFunction): Cardinal;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
  NewNode: TX3DNode;
begin
  { I don't use EnumerateNodes since I have to enumerate them myself,
    since they may be removed during enumeration.
    The code below mimics TX3DNode.DirectEnumerateAll implementation,
    but it takes into account that nodes may be removed. }

  Result := 0;

  I := 0;
  while I < VRML1ChildrenCount do
  begin
    NewNode := VRML1Children[I];
    Func(Self, NewNode);
    if NewNode <> VRML1Children[I] then
    begin
      VRML1ChildRemove(I);
      Inc(Result);
      if NewNode <> nil then
      begin
        VRML1ChildAdd(I, NewNode);
        Inc(I);
      end;
    end else
    begin
      Result := Result + VRML1Children[I].EnumerateReplaceChildren(Func);
      Inc(I);
    end;
  end;

  if FFieldsSFNode <> nil then
    for I := 0 to FFieldsSFNode.Count - 1 do
    begin
      SF := TSFNode(FFieldsSFNode[I]);
      if (SF.Value <> nil) and not SF.WeakLink then
      begin
        NewNode := SF.Value;
        Func(Self, NewNode);
        if NewNode <> SF.Value then
        begin
          SF.Value := NewNode;
          Inc(Result);
        end else
        begin
          Result := Result + SF.Value.EnumerateReplaceChildren(Func);
        end;
      end;
    end;

  if FFieldsMFNode <> nil then
    for I := 0 to FFieldsMFNode.Count - 1 do
    begin
      MF := TMFNode(FFieldsMFNode[I]);
      J := 0;
      while J < MF.InternalItems.Count do
      begin
        NewNode := MF.InternalItems[J];
        Func(Self, NewNode);
        if NewNode <> MF.InternalItems[J] then
        begin
          MF.Delete(J);
          Inc(Result);
          if NewNode <> nil then
          begin
            MF.Add(J, NewNode);
            Inc(J);
          end;
        end else
        begin
          Result := Result + MF.InternalItems[J].EnumerateReplaceChildren(Func);
          Inc(J);
        end;
      end;
    end;
end;

  type
    TRemoveChildrenWithMatchingNameHelper = class
      Wildcard: string;
      IgnoreCase: boolean;
      procedure DoIt(ParentNode: TX3DNode; var Node: TX3DNode);
    end;

  procedure TRemoveChildrenWithMatchingNameHelper.DoIt(
    ParentNode: TX3DNode; var Node: TX3DNode);
  begin
    if IsWild(Node.X3DName, Wildcard, IgnoreCase) then
      Node := nil;
  end;

function TX3DNode.RemoveChildrenWithMatchingName(
  const Wildcard: string; IgnoreCase: Boolean): Cardinal;
var
  Helper: TRemoveChildrenWithMatchingNameHelper;
begin
  Helper := TRemoveChildrenWithMatchingNameHelper.Create;
  try
    Helper.Wildcard := Wildcard;
    Helper.IgnoreCase := IgnoreCase;
    Result := EnumerateReplaceChildren({$ifdef FPC}@{$endif} Helper.DoIt);
  finally FreeAndNil(Helper) end;
end;

function TX3DNode.Field(const AName: string; const RaiseOnError: Boolean): TX3DField;
var
  I: Integer;
begin
  I := FFields.IndexOfName(AName);
  if I <> -1 then
    Result := FFields[I]
  else
  { not found }
  if RaiseOnError then
    raise EX3DNotFound.CreateFmt('Cannot find field "%s" on node "%s"', [AName, NiceName])
  else
    Result := nil;
end;

function TX3DNode.FieldOrEvent(const AName: string; const RaiseOnError: Boolean): TX3DFieldOrEvent;
var
  I: Integer;
  ResultEvent: TX3DEvent;
begin
  I := FFields.IndexOfName(AName);
  if I <> -1 then
    Exit(FFields[I]);

  { I use helper ResultEvent below, instead of passing
    "TX3DEvent(Result)" as last param: don't know why,
    but with FPC 2.2.0 this cast may fail (even though it shouldn't
    be checked at all?), testcase:
      view3dscene www.web3d.org/x3d/content/examples/Basic/CAD/CADGeometryPrototypes.x3d
  }

  I := FFields.IndexOfExposedEvent(AName, ResultEvent);
  if I <> -1 then
    Exit(ResultEvent);

  I := FEvents.IndexOfName(AName);
  if I <> -1 then
    Exit(Events[I]);

  { not found }
  if RaiseOnError then
    raise EX3DNotFound.CreateFmt('Cannot find field or event "%s" on node "%s"', [AName, NiceName])
  else
    Result := nil;
end;

function TX3DNode.AnyEvent(const AName: string; const RaiseOnError: Boolean): TX3DEvent;
var
  I: Integer;
begin
  I := FFields.IndexOfExposedEvent(AName, Result);
  if I <> -1 then
    Exit; { Result is already set }

  I := FEvents.IndexOfName(AName);
  if I <> -1 then
    Exit(Events[I]);

  { not found }
  if RaiseOnError then
    raise EX3DNotFound.CreateFmt('Cannot find event "%s" on node "%s"', [AName, NiceName])
  else
    Result := nil;
end;

procedure TX3DNode.SetHasInterfaceDeclarations(const Value: TX3DAccessTypes);
begin
  if Value <> HasInterfaceDeclarations then
  begin
    FHasInterfaceDeclarations := Value;
    if HasInterfaceDeclarations <> [] then
    begin
      { make sure InterfaceDeclarations is non-nil }
      if FInterfaceDeclarations = nil then
        FInterfaceDeclarations := TX3DInterfaceDeclarationList.Create(true);
    end else
    begin
      { make sure InterfaceDeclarations is nil }
      FreeAndNil(FInterfaceDeclarations);
    end;
  end;
end;

function TX3DNode.DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DNodeClass(ClassType).Create(X3DName, BaseUrl);
end;

function TX3DNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  I: Integer;
  IDecl: TX3DInterfaceDeclaration;
begin
  Result := DeepCopyCreate(CopyState);

  try

    { We expand CopyState.NodeOriginalToCopy now, right after DeepCopyCreate.
      This is needed, as later during DeepCopyCore we may need this node
      in case of loops within hierarchy.

      For example, internal
      routes from TX3DPrototypeNode are established to handle "IS" clauses
      for events. There routes are to/from TX3DPrototypeNode, and are
      also placed within this TX3DPrototypeNode instance. So when copying
      node routes, this node must already be present in CopyState arrays.

      Also, in the future we will have to allow loops in Script nodes
      (USE within Script node may refer to the same node). So again loop
      will be created. }
    CopyState.NodeOriginalToCopy.Add(Self, Result);

    for I := 0 to VRML1ChildrenCount - 1 do
      Result.VRML1ChildAdd(CopyState.DeepCopy(VRML1Children[I]));

    { Copy InterfaceDeclarations first, before copying Fields and Events
      (as some Fields and Events come from InterfaceDeclarations). }
    Result.HasInterfaceDeclarations := HasInterfaceDeclarations;

    if InterfaceDeclarations <> nil then
    begin
      for I := 0 to InterfaceDeclarations.Count - 1 do
      begin
        IDecl := InterfaceDeclarations[I].DeepCopy(Result, CopyState);
        Result.InterfaceDeclarations.Add(IDecl);
        Result.PostAddInterfaceDeclaration(IDecl);
      end;
    end;

    { TODO: No need to copy prototypes for now?

      This DeepCopy is used for now by protos expanding.
      It does not need prototype links (as protos are already expanded when copying,
      and they don't need anything more).

      for I := 0 to Prototypes.Count - 1 do
        ...(Prototypes[I]);
    }

    Assert(FieldsCount = Result.FieldsCount);
    Assert(EventsCount = Result.EventsCount);

    for I := 0 to FieldsCount - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Fields[I].ParentInterfaceDeclaration = nil then
      begin
        Result.Fields[I].InternalAssignDeepCopy(Fields[I], CopyState);

        if Result.Fields[I].Exposed then
        begin
          Result.Fields[I].EventIn .InternalAssignDeepCopy(Fields[I].EventIn , CopyState);
          Result.Fields[I].EventOut.InternalAssignDeepCopy(Fields[I].EventOut, CopyState);
        end;
      end;

    for I := 0 to EventsCount - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Events[I].ParentInterfaceDeclaration = nil then
        Result.Events[I].InternalAssignDeepCopy(Events[I], CopyState);

    for I := 0 to RoutesCount - 1 do
      Result.AddRoute(Routes[I].DeepCopy(CopyState));

    for I := 0 to ImportsCount - 1 do
      Result.AddImport((ImportsList[I] as TX3DImport).DeepCopy(CopyState));

    for I := 0 to ExportsCount - 1 do
      Result.AddExport((ExportsList[I] as TX3DExport).DeepCopy(CopyState));

    if PrototypeInstance then
    begin
      Result.FPrototypeInstance := PrototypeInstance;
      Result.FPrototypeInstanceSourceNode :=
        CopyState.DeepCopy(PrototypeInstanceSourceNode) as TX3DPrototypeNode;
      if PrototypeInstanceHelpers <> nil then
        Result.FPrototypeInstanceHelpers := CopyState.DeepCopy(PrototypeInstanceHelpers);
    end;

    Result.DefaultContainerField := DefaultContainerField;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TX3DNode.DeepCopy: TX3DNode;
var
  CopyState: TX3DNodeDeepCopyState;
begin
  CopyState := TX3DNodeDeepCopyState.Create;
  try
    Result := CopyState.DeepCopy(Self);
  finally FreeAndNil(CopyState); end;
end;

procedure TX3DNode.PostAddInterfaceDeclaration(IDecl: TX3DInterfaceDeclaration);
begin
  IDecl.AddFieldOrEvent(Self);
end;

procedure TX3DNode.AddCustomFieldOrEvent(AFieldOrEvent: TX3DFieldOrEvent);
var
  IDecl: TX3DInterfaceDeclaration;
begin
  IDecl := TX3DInterfaceDeclaration.Create(Self);
  IDecl.FieldOrEvent := AFieldOrEvent;
  AFieldOrEvent.ParentInterfaceDeclaration := IDecl;
  AFieldOrEvent.ParentNode := Self;
  InterfaceDeclarations.Add(IDecl);
  PostAddInterfaceDeclaration(IDecl);
end;

procedure TX3DNode.AddCustomField(AField: TX3DField);
begin
  AddCustomFieldOrEvent(AField);
end;

function TX3DNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcNone;
end;

function TX3DNode.NiceName: string;
begin
  Result := X3DType;

  // show ClassName *and* X3DType e.g. for Group:TX3DRootNode
  if not SameText(ClassName, 'T' + X3DType + 'Node') then
    Result := SAppendPart(Result, ':', ClassName);

  if X3DName <> '' then
    Result := Result + '(' + X3DName + ')';
end;

function TX3DNode.GetFields(const Index: Integer): TX3DField;
begin
  Result := FFields[Index];
end;

function TX3DNode.FieldsCount: Integer;
begin
  Result := FFields.Count;
end;

procedure TX3DNode.AddField(const Value: TX3DField);
begin
  FFields.Add(Value);

  { add the field also to FFieldsSFNode or FFieldsMFNode }
  if Value is TSFNode then
  begin
    if FFieldsSFNode = nil then
      FFieldsSFNode := TX3DFieldList.Create(false);
    FFieldsSFNode.Add(Value);
  end else
  if Value is TMFNode then
  begin
    if FFieldsMFNode = nil then
      FFieldsMFNode := TX3DFieldList.Create(false);
    FFieldsMFNode.Add(Value);
  end;
end;

function TX3DNode.IndexOfField(const AName: string): Integer;
begin
  Result := FFields.IndexOfName(AName);
end;

function TX3DNode.GetEvents(const Index: Integer): TX3DEvent;
begin
  Result := FEvents[Index];
end;

function TX3DNode.EventsCount: Integer;
begin
  Result := FEvents.Count;
end;

procedure TX3DNode.AddEvent(const Value: TX3DEvent);
begin
  FEvents.Add(Value);
end;

function TX3DNode.IndexOfEvent(const AName: string): Integer;
begin
  Result := FEvents.IndexOfName(AName);
end;

function TX3DNode.GetPrototypes(const Index: Integer): TX3DPrototypeBase;
begin
  if FPrototypes = nil then
    raise ERangeError.CreateFmt('Prototypes list is empty, but tried to access index %d', [Index]);
  Result := FPrototypes[Index];
end;

function TX3DNode.PrototypesCount: Integer;
begin
  if FPrototypes <> nil then
    Result := FPrototypes.Count else
    Result := 0;
end;

procedure TX3DNode.AddPrototype(const Value: TX3DPrototypeBase);
begin
  { create FPrototypes at demand }
  if FPrototypes = nil then
    FPrototypes := TX3DPrototypeBaseList.Create(true);
  FPrototypes.Add(Value);
end;

function TX3DNode.GetRoutes(const Index: Integer): TX3DRoute;
begin
  if FRoutes = nil then
    raise ERangeError.CreateFmt('Routes list is empty, but tried to access index %d', [Index]);
  Result := FRoutes[Index];
end;

function TX3DNode.RoutesCount: Integer;
begin
  if FRoutes <> nil then
    Result := FRoutes.Count else
    Result := 0;
end;

procedure TX3DNode.AddRoute(const Value: TX3DRoute);
begin
  { create FRoutes at demand }
  if FRoutes = nil then
    FRoutes := TX3DRouteList.Create(true);
  FRoutes.Add(Value);
end;

procedure TX3DNode.AddRoute(const Source, Destination: TX3DFieldOrEvent);
var
  Route: TX3DRoute;
begin
  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Source);
  Route.SetDestinationDirectly(Destination);
  AddRoute(Route);
end;

procedure TX3DNode.RemoveRoute(const Value: TX3DRoute);
begin
  Assert(FRoutes <> nil);

  RemoveRoute(FRoutes.IndexOf(Value));
end;

procedure TX3DNode.RemoveRoute(const Index: Integer);
begin
  Assert(FRoutes <> nil);
  Assert(Between(Index, 0, FRoutes.Count - 1));

  FRoutes.Delete(Index);
end;

function TX3DNode.GetImports(const Index: Integer): TX3DImport;
begin
  if FImports = nil then
    raise ERangeError.CreateFmt('Imports list is empty, but tried to access index %d', [Index]);
  Result := FImports[Index] as TX3DImport;
end;

function TX3DNode.ImportsCount: Integer;
begin
  if FImports <> nil then
    Result := FImports.Count else
    Result := 0;
end;

procedure TX3DNode.AddImport(const Value: TX3DImport);
begin
  { create FImports at demand }
  if FImports = nil then
    FImports := TX3DFileItemList.Create(true);
  FImports.Add(Value);
end;

function TX3DNode.GetExports(const Index: Integer): TX3DExport;
begin
  if FExports = nil then
    raise ERangeError.CreateFmt('Exports list is empty, but tried to access index %d', [Index]);
  Result := FExports[Index] as TX3DExport;
end;

function TX3DNode.ExportsCount: Integer;
begin
  if FExports <> nil then
    Result := FExports.Count else
    Result := 0;
end;

procedure TX3DNode.AddExport(const Value: TX3DExport);
begin
  { create FExports at demand }
  if FExports = nil then
    FExports := TX3DFileItemList.Create(true);
  FExports.Add(Value);
end;

procedure TX3DNode.UnregisterSceneCallback(Node: TX3DNode);
begin
  Node.Scene := nil;
end;

procedure TX3DNode.UnregisterScene;
begin
  EnumerateNodes(TX3DNode, {$ifdef FPC}@{$endif} UnregisterSceneCallback, false);
end;

function TX3DNode.FieldSetByEvent(const Event: TX3DEvent): TX3DField;
var
  F: TX3DField;
  I: Integer;
begin
  for I := 0 to FieldsCount - 1 do
  begin
    F := Fields[I];
    if F.EventIn = Event then
      Exit(F);
  end;
  Result := nil;
end;

function TX3DNode.FieldSendingEvent(const Event: TX3DEvent): TX3DField;
var
  F: TX3DField;
  I: Integer;
begin
  for I := 0 to FieldsCount - 1 do
  begin
    F := Fields[I];
    if F.EventOut = Event then
      Exit(F);
  end;
  Result := nil;
end;

procedure TX3DNode.AddFunctionality(const F: TNodeFunctionality);

  procedure RefreshShortcuts;
  begin
    FTransformFunctionality := Functionality(TTransformFunctionality) as TTransformFunctionality;
    FTimeFunctionality := Functionality(TTimeDependentFunctionality) as TTimeDependentFunctionality;
    FGenTexFunctionality := Functionality(TGeneratedTextureFunctionality) as TGeneratedTextureFunctionality;
  end;

begin
  Assert(not ( (F is TTransformFunctionality) and (FTransformFunctionality <> nil) ), 'Do not add 2 functionalities implementing TTransformFunctionality');
  Assert(not ( (F is TTimeDependentFunctionality) and (FTimeFunctionality <> nil) ), 'Do not add 2 functionalities implementing TTimeDependentFunctionality');
  Assert(not ( (F is TGeneratedTextureFunctionality) and (FGenTexFunctionality <> nil) ), 'Do not add 2 functionalities implementing TGeneratedTextureFunctionality');

  if FFunctionalityList = nil then
    FFunctionalityList := TNodeFunctionalityList.Create(true);
  FFunctionalityList.Add(F);

  RefreshShortcuts;
end;

function TX3DNode.Functionality(
  const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality;
var
  I: Integer;
  MaybeResult: TNodeFunctionality;
begin
  Result := nil;
  if FFunctionalityList <> nil then
    for I := 0 to FFunctionalityList.Count - 1 do
    begin
      MaybeResult := FFunctionalityList[I];
      if MaybeResult.InheritsFrom(FunctionalityClass) then
        Exit(MaybeResult);
    end;
end;

procedure TX3DNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
end;

{ TX3DNodeList ------------------------------------------------------------- }

function TX3DNodeList.FindName(const Name: string): TX3DNode;
var
  I: Integer;
begin
  if Name = '' then
    Exit(nil);

  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

function TX3DNodeList.IndexOfName(const Name: string): Integer;
begin
  if Name = '' then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Items[Result].X3DName = Name then
      Exit;
  Result := -1;
end;

procedure TX3DNodeList.AddIfNotExists(const Node: TX3DNode);
begin
  if IndexOf(Node) = -1 then
    Add(Node);
end;

function TX3DNodeList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TX3DNodeList) and
    (TX3DNodeList(SecondValue).Count = Count);

  if Result then
    for I := 0 to Count - 1 do
      if Items[I] <> TX3DNodeList(SecondValue)[I] then
        Exit(false);
end;

procedure TX3DNodeList.Assign(const Source: TX3DNodeList);
begin
  Clear;
  AddRange(Source);
end;

procedure TX3DNodeList.Assign(const Source: array of TX3DNode);
begin
  Clear;
  AddRange(Source);
end;

{$endif read_implementation}
