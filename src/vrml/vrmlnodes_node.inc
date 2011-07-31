{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLNode class. Huge class, the basic building block of VRML nodes. }

{$ifdef read_interface}

  { Private stuff for TVRMLNode.DeepCopy and friends implementation. }
  TVRMLNodeDeepCopyState = class
  private
    { These two lists must always have exactly the same length. }
    Original, New: TVRMLNodesList;
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
    function DeepCopy(OriginalNode: TVRMLNode): TVRMLNode;
  end;

  { Basic VRML node interface class.
    All other interfaces for VRML nodes descend from this. }
  IVRMLNode = interface
  ['{5BD33327-430F-47EC-9241-AD899F072BF8}']
  end;

  { VRML node. Every VRML node class descends from this. }
  TVRMLNode = class(TVRMLFileItem, IVRMLNode)
  private
    fNodeName: string;
    FWWWBasePath: string;
    FVRML1Children, FVRML1Parents: TVRMLNodesList;
    FKeepExisting: Cardinal;
    FParentFields: TVRMLFieldsList;
    FPrototypes: TVRMLPrototypeBasesList;
    FRoutes: TVRMLRoutesList;
    FImportsList, FExportsList: TVRMLFileItemsList;
    FFields: TVRMLFieldsList;
    FEvents: TVRMLEventsList;
    FPrototypeInstance: boolean;
    FPrototypeInstanceSourceNode: TVRMLPrototypeNode;
    FPrototypeInstanceHelpers: TVRMLNode;
    FDefaultContainerField: string;
    FHasInterfaceDeclarations: TVRMLAccessTypes;
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;
    FCDataField: TMFString;
    FDestructionNotifications: TDynNodeDestructionNotificationArray;
    FScene: TVRMLEventsEngine;
    FShapeTrees: TObject;

    { Internally used by PROTO instantiating mechanism,
      in TVRMLPrototypeNode.InstantiateIsClauses
      and TVRMLPrototypeNode.Instantiate.

      This is always @false outside of TVRMLPrototypeNode.Instantiate
      implementation (as TVRMLPrototypeNode.Instantiate sets it to @true
      and changes back to @false for appropriate nodes). }
    NeedsInstantiateIsClause: boolean;

    function GetVRML1Child(i: integer): TVRMLNode;
    function GetVRML1Parent(i: integer): TVRMLNode;
    procedure SetVRML1Child(I: Integer; Value: TVRMLNode);
    function GetParentFieldsItem(Index: Integer): TVRMLField;
    function GetParentFieldsNodeItem(Index: Integer): TVRMLNode;
    procedure RemoveParentField(Field: TVRMLField);
    procedure AddParentField(Field: TVRMLField);
    procedure TryFindNode_Found(Node: TVRMLNode);
    procedure SetHasInterfaceDeclarations(const Value: TVRMLAccessTypes);
    { Internal save to stream utility.
      When CurrentContainerField is not empty, it means we know our containing field
      (and should adjust containerField= for XML encoding output). }
    procedure NodeSaveToStream(Writer: TX3DWriter; const CurrentContainerField: string = '');
  protected

    { Does actual DeepCopy work. You can override this to copy some
      more properties for descendants. }
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; virtual;

    { This should be a mere call to constructor of your own class.

      In TVRMLNode, this simply calls default virtual constructor,
      which is Ok for all normal nodes. But we have some special nodes,
      like TVRMLPrototypeNode or TVRMLUnknownNode, that simply cannot
      be created by default constructor. They need to override this. }
    function DeepCopyCreate(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; virtual;
  protected
    { Are VRML 1.0 children allowed.
      This condition is checked in VRML1ChildAdd, so it's strictly
      impossible to add a node that is not allowed.

      Note that in some special cases VRML1ChildrenAllowed and
      VRML1ChildrenParsingAllowed values may be changed during object lifetime.
      Currently, this may concern TVRMLUnknownNode.

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

      "Active nodes" are the ones affecting current VRML graph look
      or collisions, e.g. from Switch
      node only one child will be enumerated.
      See @link(Traverse) for more precise definition.

      "Direct" means that this enumerates only direct
      descendants, i.e. this is not recursive.
      See methods like Traverse or EnumerateNodes if you
      want recursive behavior.

      This can enumerate both @link(VRML1Children) nodes
      nodes within TSFNode and TMFNode fields.

      Default implementation in this class returns all Children
      nodes of VRML 1.0. If you need to remove some children
      for VRML 1.0 (e.g. for Switch or LOD nodes)
      or add some children for VRML 2.0 you
      have to override this. You're not required to call
      inherited when overriding this. }
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); virtual;

    { Enumerate all active child nodes of given node,
      and may additionally modify StateStack. It's used by Traverse.

      Default implementation in this class simply calls
      DirectEnumerateActive, ignoring StateStack, and this is suitable
      for 99% of nodes. However, for some special nodes (only Collision
      node for now), they have to modify state during traversing into
      various children, and then they can override this. }
    procedure DirectEnumerateActiveForTraverse(
      Func: TEnumerateChildrenFunction;
      StateStack: TVRMLGraphTraverseStateStack); virtual;

    { Simply enumerate all direct descendant nodes.
      That is, all children in VRML 1.0 style and
      all nodes in SFNode and MFNode fields.
      This includes prototype stuff, if this node is expanded
      from prototype: PrototypeInstanceSourceNode and PrototypeInstanceHelpers. }
    procedure DirectEnumerateAll(
      Func: TEnumerateChildrenFunction);

    { This enumerates direct descendant nodes of this node.
      This is equivalent to DirectEnumerateActive or
      DirectEnumerateAll, depending on value of OnlyActive param. }
    procedure DirectEnumerate(
      Func: TEnumerateChildrenFunction;
      OnlyActive: boolean);

    { Override these methods to determine what happens when
      given node is traversed during Traverse call.
      The main use of this is to operate on TVRMLGraphTraverseStateStack.

      Remember to always call inherited when overriding.
      In BeforeTraverse and MiddleTraverse you should call inherited
      at the beginning, in AfterTraverse inherited should be called at the end.

      Besides changing StateStack.Top fields, you can do push/pop
      on the stack. Remember that if you do StateStack.Push in BeforeTraverse,
      and then you @italic(must call StateStack.Pop in AfterTraverse).

      @groupBegin }
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); virtual;
    procedure MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack); virtual;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); virtual;
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

          Example: TNodeShapeHints in Inventor parses "hints" field,
          and copies it's value to other fields as appropriate.
          "hints" field is not exposed in TNodeShapeHints interface,
          so everything is clean in the interface, and under the hood
          TNodeShapeHints can "magically" handle "hints" field for Inventor.)
      )

      When overriding, always check inherited result first, and exit if
      inherited handled successfully.
      Otherwise either read your stuff and return @true
      (Lexer should advance to the position of next "nodeBodyElement").
      Or return @false without changing Lexer position. }
    function ParseNodeBodyElement(Lexer: TVRMLLexer; Names: TVRMLNames;
      const APositionInParent: Integer): boolean; virtual;

    { Called at the end of parsing (in both classic and XML encoding). }
    procedure ParseAfter(Names: TVRMLNames); virtual;

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
  public
    { Node fields.

      For normal nodes, all Fields are created and added
      to Fields list in constructor. Fields default values are set,
      and of course current field values are set to these defaults.
      Later, we only modify these fields current values (e.g. when parsing).

      However, there are special node classes that set their Fields differently.
      TVRMLPrototypeNode has their fields set according to it's VRML 2.0 prototype.
      TVRMLUnknownNode may have it's fields set by VRML 1.0 "fields" feature
      (so it's Fields are initialized by parsing it).

      Nodes with HasInterfaceDeclarations have some Fields and Events
      added when reading node.

      All fields on this list are owned by this object. }
    property Fields: TVRMLFieldsList read FFields;

    { Explicit events (that is, not exposed by some field) of this node.
      For exposed events, see each field's property ExposedEvents. }
    property Events: TVRMLEventsList read FEvents;

    { Search by name for given field or event (exposed by some field or not).

      @nil if not found. }
    function FieldOrEvent(const Name: string): TVRMLFieldOrEvent;

    { Search by name for given event (exposed by some field or not).

      @nil if not found. }
    function AnyEvent(const Name: string): TVRMLEvent;

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
    property VRML1Children [i: integer]: TVRMLNode read GetVRML1Child write SetVRML1Child;
    function VRML1ChildrenCount: integer;
    { @groupEnd }

    { Add a VRML 1.0 child node at given position.
      Index (position) must be in [0..VRML1ChildrenCount].
      Items at and above Index position are moved to the right,
      to insert new child at Index position. }
    procedure VRML1ChildAdd(Index: Integer; child: TVRMLNode); overload;

    { Add a VRML 1.0 child node at the end of VRML1Children list. }
    procedure VRML1ChildAdd(child: TVRMLNode); overload;

    procedure VRML1ChildRemove(i: integer);
    procedure VRML1ChildrenClear;

    { Remove a VRML 1.0 children at index I and return it (do not free it).
      Compare this with VRML1Remove, that removes children I and
      frees it if it's no longer used by anything.

      This removes children I, appropriately adjusting
      all parent / children links, but even if a node is unused after removing,
      it is not freed. It's always returned. }
    function VRML1ChildExtract(I: Integer): TVRMLNode;

    { VRML 1.0 parent nodes. VRML1Parents is a reverse of
      VRML1Children --- it lists all the nodes where we are on VRML1Children
      list.

      @groupBegin }
    property VRML1Parents [i: integer] :TVRMLNode read GetVRML1Parent;
    function VRML1ParentsCount: integer;
    { @groupEnd }

    { This lists all SFNode and MFNode fields where this node is referenced.
      This is somewhat analogous for VRML1Parents, but for VRML 2.0.

      ParentFieldsNode is just for your comfort, it returns always
      appropriate field's ParentNode property value
      (i.e. @code((ParentField[Index] as TSFNode).ParentNode)
      or @code((ParentField[Index] as TMFNode).ParentNode)).

      @groupBegin }
    property ParentFields[Index: Integer]: TVRMLField read GetParentFieldsItem;
    property ParentFieldsNode[Index: Integer]: TVRMLNode
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

      For safety, it's adviced to set reference to @nil after calling FreeIfUnused.
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

    { Name of this node, as defined by VRML "DEF" construct.

      NodeName = '' oznacza ze obiekt nie mial zdefiniowanej nazwy.

      It's named NodeName, to not confuse this with TVRMLField.Name.
      (Even though TVRMLField and TVRMLNode classes have nothing in common.
      TSFNode descends from TVRMLField and @italic(contains) TVRMLNode
      instance in it's Value field. Once I wanted to just make
      TSFNode = TVRMLNode and TVRMLNode descendant of TVRMLField,
      but this wasn't a good idea: TSFNode may be NULL, but still
      it has a field name, so it should be nicely represented as
      TSFNode instance with Value = nil.)

      Note that this is writeable property, so you can change NodeName
      at runtime. Beware that some operations depend that node names
      don't change during their work: loading and saving nodes
      from stream (since these operations keep current collection names to
      read / write VRML DEF / USE statements), searching for nodes by name. }
    property NodeName: string read fNodeName write FNodeName;

    { WWWBasePath is the base URL path for all URLs
      in node's fields. This is used by all nodes that get some
      url (like Texture2 and WWWInline in VRML 1.0, ImageTexture
      and Inline in VRML 2.0 etc.).

      This way URL's in node's fields may contain relative names.
      If WWWBasePath doesn't begin with <proto>:// it is understood
      to be a file:// base path.

      TODO: chwilowo, poniewaz tylko odwolania do lokalnych plikow
      sa zaimplementowane, cale to bajanie o URL'ach to tylko mowa
      "jak kiedys bedzie". Chwilowo WWWBasePath musi byc lokalna sciezka
      (i to absolutna, bezwzgledna sciezka).

      WWWBasePath is set in constructor, and eventually adjusted by
      various parsing/converting routines (TVRMLNode.Parse, but also
      potentially other things from Object3DAsVRML).
      This way we could, if only we would like to, resolve nodes
      like Inline or ImageTexture immediately after parsing them. }
    property WWWBasePath: string read FWWWBasePath write FWWWBasePath;

    { This returns absolute path, assuming that RelativePath is relative
      path from WWWBasePath or that RelativePath is already absolute. }
    function PathFromWWWBasePath(const RelativePath: string): string;

    { Parse node. This should set values of your fields, VRML 1.0 Children
      list, WWWBasePath.

      In special cases like TVRMLUnknownNode this may
      actually initialize whole Fields list (by VRML 1.0 "fields" extensibility
      feature). }
    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); virtual;

    { Parse node body, i.e. mainly node's fields. }
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames);

    { Constructor. Initializes various properties:

      @unorderedList(
        @item(NodeName, WWWBasePath are initialized
          directly from given parameters.)
        @item(The @link(Fields), @link(Events) lists are filled
          in every descendant, to have all the fields/events defined
          by the specification.)
        @item(DefaultContainerField, and other node-specific
          stuff, is filled in descendants. This is actually implemented
          in CreateNode, that is called at the end of this constructor.)
      )
    }
    constructor Create(const ANodeName, AWWWBasePath: string); virtual;

    destructor Destroy; override;

    { Name of the VRML/X3D node. Never empty.
      Constant for the class lifetime (even for special
      TVRMLUnknownNode and TVRMLPrototypeNode, where this is calculated
      at runtime).

      Note that VRML/X3D is generally case-sensitive, so this property is too.

      In TVRMLNode, this returns ClassNodeTypeName, which is suitable
      for most nodes. See ClassNodeTypeName. }
    function NodeTypeName: string; virtual;

    { Name of the VRML/X3D node associated with this class.
      Normal VRML/X3D node classes should override this to return something
      non-empty, and then NodeTypeName automatically will return the same value.

      Empty for classes that don't have a hardcoded VRML/X3D node name,
      like a special TVRMLUnknownNode. Such special classes should override
      then NodeTypeName to return actual non-empty name there.

      You usually should call NodeTypeName. The only use of this method
      is that it works on classes (it's "class function"), without needing
      at actual instance. }
    class function ClassNodeTypeName: string; virtual;

    { Traverse all the nodes of VRML graph that are active.

      An "active" part of the VRML graph are the nodes that actually
      change what the VRML file represents, in terms of geometry,
      collision detection etc. For example, the Switch node has only
      one child usually active. Nodes that merely influence
      the active graph by some events and routes do not have to be
      active (these nodes may change what the VRML file actually represents,
      but only by changing other nodes).

      For all nodes of NodeClass TraversingFunc
      will be called.

      Traverse not only enumerates these
      nodes, it also collects all state (transformation, etc ---
      see TVRMLGraphTraverseState) that affects how given node should
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

  Add Self to State.LastNodes (for VRML 1.0 state nodes, see
    TraverseStateLastNodesClasses);
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
    procedure Traverse(
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc);

    { Like @link(Traverse), but it takes explicit starting state stack
      and starting ParentInfo. Not generally useful, use only for special
      purposes. }
    procedure TraverseInternal(StateStack: TVRMLGraphTraverseStateStack;
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc;
      ParentInfo: PTraversingInfo);

    { Like @link(Traverse), but only enters children.
      This does the job of Traverse normally omitted if your @link(Traverse)
      callback returns TraverseIntoChildren = @false.
      Using this method, you can traverse into children explicitly
      from your callback. }
    procedure TraverseIntoChildren(StateStack: TVRMLGraphTraverseStateStack;
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc;
      ParentInfo: PTraversingInfo);

    { Enumerate all our children of some class. Recursively.
      Zwroci do proc() takze sam obiekt na ktorym EnumerateNodes zostalo
      wywolane, jezeli tylko ten obiekt jest klasy nodeClass.

      This enumerates both @link(VRML1Children) as well as
      nodes in TSFNode and TMFNode fields.

      If OnlyActive then it will enumerate only active parts
      of the graph ("active" as defined by @link(Traverse)),
      so it will work as a simpler version of Traverse
      (simpler, because it doesn't track any state).

      If not OnlyActive then it will simply enumerate all nodes.
      This will include then also prototype helpers, if this node
      was expanded from prototype: see PrototypeInstanceSourceNode
      and PrototypeInstanceHelpers.

      If you give SeekNodeName parameter, we'll only look for nodes
      with NodeName = SeekNodeName. When SeekNodeName = '' it,
      consistently, looks for unnamed nodes (where NodeName = '').

      Enumerates children only after enumerating Self.
      So you can modify children before enumerating them in the Proc callback.
      You can only modify your children in the Proc callback.

      @groupBegin }
    procedure EnumerateNodes(
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TVRMLNodeClass;
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TVRMLNodeClass;
      const SeekNodeName: string;
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    { @groupEnd }

    { TryFindNodeByName and TryFindNode seek for a node with
      given class (and node name, in case of TryFindNodeByName).
      If OnlyActive then they seek among only active nodes
      ("active" as defined by @link(Traverse)), otherwise all nodes.

      These functions are quite like EnumerateNodes, except
      they stop at the first occurence and return it.

      TryFindNodeByName and TryFindNode return @nil if such node
      is not found. FindNodeByName and FindNode raise exception
      in this case.

      @groupBegin }
    function TryFindNodeByName(FindClass: TVRMLNodeClass;
      const FindName: string;
      OnlyActive: boolean): TVRMLNode;
    function FindNodeByName(FindClass: TVRMLNodeClass;
      const FindName: string;
      OnlyActive: boolean): TVRMLNode;
    function TryFindNode(FindClass: TVRMLNodeClass;
      OnlyActive: boolean): TVRMLNode;
    function FindNode(FindClass: TVRMLNodeClass;
      OnlyActive: boolean): TVRMLNode;
    { @groupEnd }

    { Find the first node with given class (NodeClass),
      return it's state or just transformation.
      Similar to @link(Traverse), but stops as soon as the given node is found.

      Returns @false when not found ("out" parametrs
      Node, State and Transform are undefined then).
      @groupBegin }
    function TryFindNodeState(
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out State: TVRMLGraphTraverseState): boolean;
    function TryFindNodeTransform(
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out Transform: TMatrix4Single;
      out TransformScale: Single): boolean;
    { @groupEnd }

    { Seeks Self and parent nodes (from VRML1Parents and ParentFields,
      recursively) for given node name.

      In other words, this is similar to TryNodeByName or NodeByName,
      but it goes "upward" in graph hierarchy. Note that this
      never restricts itself only to "active" graph part
      ("active" as defined by @link(Traverse))
      because you really can't detect what is the "active"
      part of the graph when going upward.

      @groupBegin }
    function TryFindParentByName(const FindName: string): TVRMLNode;
    function FindParentByName(const FindName: string): TVRMLNode;
    { @groupEnd }

    { Looks for a given Node in parents (and self), recursively.
      Similar to TryFindParentByName. Returns @true only if found. }
    function HasParent(Node: TVRMLNode): boolean;

    { Search immediate parents of this node for a node with given FindName.
      Returns @nil if not found. }
    function TryFindDirectParentByName(const FindName: string): TVRMLNode;

    { Looks for a given node in our children (direct and not, including self).
      If OnlyActive, then only active parts are searched
      ("active" as defined by @link(Traverse)). }
    function IsNodePresent(Node: TVRMLNode; OnlyActive: boolean): boolean;

    { Count the occurrences of given node class in our children.
      For example, you can pass NodeClass = TNodeX3DLightNode to count
      the light sources in the scene.

      If CountOnlyActiveNodes, then only active parts are searched
      ("active" as defined by @link(Traverse)).

      This traverses both VRML 1.0 children nodes and VRML 2.0 nodes
      inside SFNode and MFNode fields. }
    function NodesCount(NodeClass: TVRMLNodeClass;
      CountOnlyActiveNodes: boolean): integer;

    { Save node to a stream. Saves everything, including node name,
      node type, and node contents. }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Do the nodes of (exactly) this class should be included
      in VRML 1.0 state (see TVRMLGraphTraverseState.LastNodes). }
    class function VRML1StateNode(out StateNode: TVRML1StateNode): boolean;

    { Some nodes are present only in specific VRML/X3D version.
      This functions decides it.

      For example some nodes can only work in VRML < 2.0,
      some others only in VRML >= 2.0. There are even some pairs
      of nodes: for example TNodeCone_1 works with VRML < 2.0,
      TNodeCone works with VRML >= 2.0.

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
    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
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

    property Prototypes: TVRMLPrototypeBasesList read FPrototypes;
    property Routes: TVRMLRoutesList read FRoutes;
    property ImportsList: TVRMLFileItemsList read FImportsList;
    property ExportsList: TVRMLFileItemsList read FExportsList;

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
    function DeepCopy: TVRMLNode;

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
      (@link(TVRMLPrototypeNode.Instantiate))
      and when linking ROUTE (TVRMLRoute.SetSource, TVRMLRoute.SetDestination).

      PrototypeInstanceHelpers may be @nil if empty, or may contain
      a list of other nodes duplicated along with the main prototype node.
      From VRML spec:

      @preformatted(
        Any other nodes and accompanying scene graphs
        are not part of the transformation hierarchy, but may be referenced
        by ROUTE statements or Script nodes in the prototype definition.)

      TODO: memory leaks are known to be possible in some difficult cases
      with PrototypeInstanceHelpers. See e.g.
      ../../../demo_models/vrml_2/warnings/errors/proto_leak.wrl and
      ../../../demo_models/vrml_2/warnings/errors/proto_leak_2.wrl
      for simple testcases. Reason: PrototypeInstanceHelpers may contain,
      by DEF statements, links to Self.
      This causes circular dependency (Self is child of some node on
      PrototypeInstanceHelpers, but PrototypeInstanceHelpers will
      be freed only if Self is freed) causing some memory to be left
      always allocated.

      Note that for TVRMLPrototypeNode (within PrototypeInstanceSourceNode)
      these have a little different meaning: they describe the
      @italic(nested prototype), if any, that was used to create this node.
      This may happen if the node was expanded from one prototype within
      another. (Usually, you shouldn't be concerned about this;
      see TVRMLPrototypeNode.Instantiate implementation comments for
      gory details about this.)

      @groupBegin }
    property PrototypeInstance: boolean read FPrototypeInstance;
    property PrototypeInstanceSourceNode: TVRMLPrototypeNode
      read FPrototypeInstanceSourceNode;
    property PrototypeInstanceHelpers: TVRMLNode read FPrototypeInstanceHelpers;
    { @groupEnd }

    { Should we use this node when URN is required by EXTERNPROTO ?

      Implementors note: in this class, this returns @false.
      You can use constants like URNVRML97Nodes and URNKambiNodes to help
      implementing this. }
    class function URNMatching(const URN: string): boolean; virtual;

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
    property HasInterfaceDeclarations: TVRMLAccessTypes
      read FHasInterfaceDeclarations
      write SetHasInterfaceDeclarations default [];

    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;
    { @groupEnd }

    { Functions registered here will be called when this TVRMLNode descendant
      will be destroyed. }
    property DestructionNotifications: TDynNodeDestructionNotificationArray
      read FDestructionNotifications;

    { Scene that will be notified about changes to this node.
      This is necessary to allow the scene to properly process VRML/X3D
      events, and also to react properly to any other changes to the nodes
      (like when you change some field directly by ObjectPascal code,
      e.g. by TVRMLField.Send).

      May be @nil if none.

      A given TVRMLNode may be renderable only by a single renderer.
      This means it can be placed only within one TVRMLGLScene
      or one TVRMLGLAnimation (nothing else gives you a renderer).

      A given TVRMLNode may be placed inside many TVRMLScene instances
      (including TVRMLGLScene). But if more than one scene is used,
      then all the scenes must be static (TVRMLScene.ProcessEvents = false
      and TVRMLScene.Static = true). Static scenes are ones
      that never change, that is never have any TVRMLScene.ChangedField called.
      This means that events processing is disabled, and also
      other dynamic scene changes from code are disallowed.

      These two rules are both at action when you think about
      a node inside many TVRMLGLScene instances. In this case,
      all such scene instances must be created with the same
      renderer (hence the need for TVRMLGLScene.CreateCustomRenderer)
      and all must be static. This is actually exactly how
      TVRMLGLAnimation works: it internally creates a single renderer
      and a series of TVRMLGLScene instances. }
    property Scene: TVRMLEventsEngine read FScene write FScene;

    { Managed and used by TVRMLScene to keep TVRMLShapeTreesList
      for every INodeTransform. }
    property ShapeTrees: TObject read FShapeTrees write FShapeTrees;

    { This will be always called by VRML parsers after adding new item
      to our InterfaceDeclarations.

      In this class, this simply adds
      IDecl.FieldOrEvent to our normal fields/events by IDecl.AddFieldOrEvent.
      You may override this in subclasses to react in some special way
      to new fields/events, for example Script node may register here
      to receive notification when input event is received. }
    procedure PostAddInterfaceDeclaration(IDecl: TVRMLInterfaceDeclaration); virtual;

    { Add to node InterfaceDeclarations given field.
      Sets the field's exposed to given Exposed parameter, for comfort.
      This should only be used with nodes having
      HasInterfaceDeclarations = @true, like Script or ComposedShader. }
    procedure AddCustomField(Field: TVRMLField; const Exposed: boolean = false);

    { What happens when transformation of the node changes.
      This is internally used by TVRMLScene when Transform (or similar)
      node affecting this node changes. It's an important optimization,
      to avoid large if clause with many "is" and "Supports" checks
      that would be quite slow.
      @exclude }
    function TransformationChange: TNodeTransformationChange; virtual;

    { Nice and concise node description for user.
      Shows node type and name. }
    function NiceName: string;
  end;

  TObjectsListItem_3 = TVRMLNode;
  {$I objectslist_3.inc}
  TVRMLNodesList = class(TObjectsList_3)
    { @deprecated Deprecated name for FindName. }
    function FindNodeName(const Name: string): Integer;

    { Find node by name, @nil if not found.

      For empty node name, always returns @nil. This follows the definition
      of NodeName: empty means that node has no name, so it should not be found
      by searches by name, ever.

      @seealso(IndexOfName Returns the node index. This is sometimes more
        flexible (you can always quickly get actual node knowing the index,
        by Items[Index], but not the other way around). It is also sometimes
        less comfortable (often you're not interested in node index). }
    function FindName(const Name: string): TVRMLNode;

    { Find index of a node with given name, -1 if not found. }
    function IndexOfName(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVRMLNodeDeepCopyState ----------------------------------------------------- }

constructor TVRMLNodeDeepCopyState.Create;
begin
  inherited;
  Original := TVRMLNodesList.Create;
  New := TVRMLNodesList.Create;
end;

destructor TVRMLNodeDeepCopyState.Destroy;
begin
  FreeAndNil(Original);
  FreeAndNil(New);
  inherited;
end;

function TVRMLNodeDeepCopyState.DeepCopy(OriginalNode: TVRMLNode): TVRMLNode;
var
  I: Integer;
begin
  Assert(New.Count = Original.Count);

  I := Original.IndexOf(OriginalNode);
  if I = -1 then
  begin
    { DeepCopyCore will expand (Original, New) lists by (OriginalNode, Result).
      This is needed, see DeepCopyCore comments. }
    Result := OriginalNode.DeepCopyCore(Self);
  end else
    Result := New[I];
end;

{ TVRMLNode ------------------------------------------------------------------ }

constructor TVRMLNode.Create(const ANodeName, AWWWBasePath: string);
begin
  inherited Create;
  VRML1ChildrenAllowed := false;
  VRML1ChildrenParsingAllowed := false;

  FNodeName := ANodeName;
  FWWWBasePath := AWWWBasePath;

  FVRML1Children := TVRMLNodesList.Create;
  FVRML1Parents := TVRMLNodesList.Create;
  FParentFields := TVRMLFieldsList.Create;
  FFields := TVRMLFieldsList.Create;

  FEvents := TVRMLEventsList.Create;

  FPrototypes := TVRMLPrototypeBasesList.Create;
  FRoutes := TVRMLRoutesList.Create;
  FImportsList := TVRMLFileItemsList.Create;
  FExportsList := TVRMLFileItemsList.Create;

  FHasInterfaceDeclarations := [];
  FInterfaceDeclarations := nil;

  FDestructionNotifications := TDynNodeDestructionNotificationArray.Create;

  CreateNode;
end;

destructor TVRMLNode.Destroy;
var
  I: Integer;
begin
  { This whole unit, including AnyNodeDestructionNotifications,
    may be already finalized when calling this. }
  if AnyNodeDestructionNotifications <> nil then
    AnyNodeDestructionNotifications.ExecuteAll(Self);

  if DestructionNotifications <> nil then
  begin
    DestructionNotifications.ExecuteAll(Self);
    FreeAndNil(FDestructionNotifications);
  end;

  if FVRML1Children <> nil then VRML1ChildrenClear;

  if PrototypeInstance then
  begin
    FreeAndNil(FPrototypeInstanceSourceNode);
    FreeAndNil(FPrototypeInstanceHelpers);
    FPrototypeInstance := false;
  end;

  FreeWithContentsAndNil(FPrototypes);

  FreeWithContentsAndNil(FRoutes);
  FreeWithContentsAndNil(FImportsList);
  FreeWithContentsAndNil(FExportsList);

  { First free Fields and Events, before freeing InterfaceDeclarations.
    Reason: Fields and Events may contains references to InterfaceDeclarations
    items (since parsing added them there by PostAddInterfaceDeclaration(IDecl)).
    So these references have to be valid, and omitted by checking
    ParentInterfaceDeclaration <> nil. }

  if FEvents <> nil then
  begin
    for I := 0 to FEvents.Count - 1 do
      if FEvents[I].ParentInterfaceDeclaration = nil then
        FEvents.FreeAndNil(I);
    FreeAndNil(FEvents);
  end;

  if FFields <> nil then
  begin
    for I := 0 to FFields.Count - 1 do
      if FFields[I].ParentInterfaceDeclaration = nil then
        FFields.FreeAndNil(I);
    FreeAndNil(FFields);
  end;

  FreeWithContentsAndNil(FInterfaceDeclarations);

  FreeAndNil(FVRML1Children);
  FreeAndNil(FVRML1Parents);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TVRMLNode.FreeIfUnused;
begin
  if (Self <> nil) and
     (FVRML1Parents.Count = 0) and
     (FParentFields.Count = 0) and
     (KeepExisting = 0) then
  begin
    { This is written as "Self.Destroy" to actually do the desctruction,
      freeing memory etc. If I would just call it "Destroy", it would
      perform what destructor does but leaving object instance unfreed. }
    Self.Destroy;
  end;
end;

procedure TVRMLNode.KeepExistingBegin;
begin
  Inc(FKeepExisting);
end;

procedure TVRMLNode.KeepExistingEnd;
begin
  Dec(FKeepExisting);
end;

procedure TVRMLNode.VRML1ChildAdd(Index: Integer; child: TVRMLNode);
begin
 Check(VRML1ChildrenAllowed,
   'Node '+NodeTypeName+' is not allowed to have child node of type '+
   Child.NodeTypeName);
 child.FVRML1Parents.Add(Self);
 FVRML1Children.Insert(Index, child);
end;

procedure TVRMLNode.VRML1ChildAdd(child: TVRMLNode);
begin
  VRML1ChildAdd(FVRML1Children.Count, child);
end;

procedure TVRMLNode.VRML1ChildRemove(i: integer);
var
  OldChild: TVRMLNode;
begin
  OldChild := FVRML1Children[i];
  FVRML1Children.Delete(i);
  OldChild.FVRML1Parents.Remove(Self);
  OldChild.FreeIfUnused;
end;

function TVRMLNode.VRML1ChildExtract(I: Integer): TVRMLNode;
begin
  Result := FVRML1Children[i];
  FVRML1Children.Delete(i);
  Result.FVRML1Parents.Remove(Self);

  { VRML1ChildRemove now does
      OldChild.FreeIfUnused;
    but ExtractChild doesn't do it. }
end;

procedure TVRMLNode.SetVRML1Child(I: Integer; Value: TVRMLNode);
var
  OldChild: TVRMLNode;
begin
  { Inefficient implementation: VRML1ChildRemove(I); VRML1ChildAdd(I, Value); }

  if Value <> FVRML1Children[I] then
  begin
    Check(VRML1ChildrenAllowed,
      'Node '+NodeTypeName+' is not allowed to have child node of type '+
      Value.NodeTypeName);

    OldChild := FVRML1Children[i];
    FVRML1Children[I] := Value;

    OldChild.FVRML1Parents.Remove(Self);
    OldChild.FreeIfUnused;

    Value.FVRML1Parents.Add(Self);
  end;
end;

procedure TVRMLNode.VRML1ChildrenClear;
begin
 while FVRML1Children.Count > 0 do VRML1ChildRemove(0);
end;

function TVRMLNode.GetVRML1Child(i: integer): TVRMLNode;
begin
  result := FVRML1Children[i];
end;

function TVRMLNode.GetVRML1Parent(i: integer): TVRMLNode;
begin
  result := FVRML1Parents[i];
end;

function TVRMLNode.VRML1ChildrenCount: integer;
begin
  result := FVRML1Children.Count
end;

function TVRMLNode.VRML1ParentsCount: integer;
begin
  result := FVRML1Parents.Count
end;

procedure TVRMLNode.FreeRemovingFromAllParents;
var
  i, j: integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  if Self = nil then exit;

  for i := 0 to FVRML1Parents.Count - 1 do
  begin
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
      raise EInternalError.Create('TVRMLNode.ParentFields not SF or MF Node class');
  end;

  Self.Destroy;
end;

function TVRMLNode.GetParentFieldsItem(Index: Integer): TVRMLField;
begin
  Result := FParentFields[Index];
end;

function TVRMLNode.GetParentFieldsNodeItem(Index: Integer): TVRMLNode;
var
  F: TVRMLField;
begin
  F := ParentFields[Index];
  if F is TSFNode then
    Result := TSFNode(F).ParentNode else
    Result := (F as TMFNode).ParentNode;
end;

function TVRMLNode.ParentFieldsCount: Integer;
begin
  Result := FParentFields.Count;
end;

procedure TVRMLNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to VRML1ChildrenCount - 1 do
    Func(Self, VRML1Children[I]);
end;

procedure TVRMLNode.DirectEnumerateActiveForTraverse(
  Func: TEnumerateChildrenFunction;
  StateStack: TVRMLGraphTraverseStateStack);
begin
  DirectEnumerateActive(Func);
end;

procedure TVRMLNode.DirectEnumerateAll(
  Func: TEnumerateChildrenFunction);
var
  I, J: Integer;
  F: TVRMLField;
  SF: TSFNode;
  MF: TMFNode;
begin
  for I := 0 to VRML1ChildrenCount - 1 do
    Func(Self, VRML1Children[I]);

  for I := 0 to Fields.Count - 1 do
  begin
    F := Fields[I];
    if F is TSFNode then
    begin
      SF := TSFNode(F);
      if SF.Value <> nil then
        Func(Self, SF.Value);
    end else
    if F is TMFNode then
    begin
      MF := TMFNode(F);
      for J := 0 to MF.Items.Count - 1 do
        Func(Self, MF.Items[J]);
    end;
  end;

  if PrototypeInstance then
  begin
    Assert(PrototypeInstanceSourceNode <> nil);
    Func(Self, PrototypeInstanceSourceNode);

    if PrototypeInstanceHelpers <> nil then
      Func(Self, PrototypeInstanceHelpers);
  end;
end;

procedure TVRMLNode.DirectEnumerate(
  Func: TEnumerateChildrenFunction;
  OnlyActive: boolean);
begin
  if OnlyActive then
    DirectEnumerateActive(Func) else
    DirectEnumerateAll(Func);
end;

procedure TVRMLNode.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  if PrototypeInstance then
    Inc(StateStack.Top.InsidePrototype);
end;

procedure TVRMLNode.MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
end;

procedure TVRMLNode.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  if PrototypeInstance then
    Dec(StateStack.Top.InsidePrototype);
end;

type
  TTraverseEnumerator = class
    StateStack: TVRMLGraphTraverseStateStack;
    NodeClass: TVRMLNodeClass;
    TraversingFunc: TTraversingFunc;
    ParentInfo: PTraversingInfo;
    procedure DoTraverseIntoChildren(Child: TVRMLNode);
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TTraverseEnumerator.DoTraverseIntoChildren(Child: TVRMLNode);
  var
    ParentInfoForChildren: TTraversingInfo;
  begin
    ParentInfoForChildren.Node := Child;
    ParentInfoForChildren.ParentInfo := ParentInfo;
    ParentInfo := @ParentInfoForChildren;
    try
      Child.DirectEnumerateActiveForTraverse(@EnumerateChildrenFunction, StateStack);
    finally
      ParentInfo := ParentInfoForChildren.ParentInfo;
    end;
  end;

  procedure TTraverseEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  var
    StateNode: TVRML1StateNode;
    IsTraverseIntoChildren: boolean;
  begin
    Child.BeforeTraverse(StateStack);
    try
      IsTraverseIntoChildren := true;
      if Child is NodeClass then
        TraversingFunc(Child, StateStack, ParentInfo, IsTraverseIntoChildren);

      Child.MiddleTraverse(StateStack);

      if IsTraverseIntoChildren then
        DoTraverseIntoChildren(Child);
    finally Child.AfterTraverse(StateStack) end;

    if Child.VRML1StateNode(StateNode) then
      StateStack.Top.SetLastNodes(StateNode, Child, false);
  end;

procedure TVRMLNode.TraverseInternal(StateStack: TVRMLGraphTraverseStateStack;
  NodeClass: TVRMLNodeClass;
  TraversingFunc: TTraversingFunc;
  ParentInfo: PTraversingInfo);
var
  Enumerator: TTraverseEnumerator;
begin
  Enumerator := TTraverseEnumerator.Create;
  try
    Enumerator.StateStack := StateStack;
    Enumerator.NodeClass := NodeClass;
    Enumerator.TraversingFunc := TraversingFunc;
    Enumerator.ParentInfo := ParentInfo;
    Enumerator.EnumerateChildrenFunction(nil, Self);

    { Check that, if all went without exception, Enumerator.ParentInfo
      returned to original state. }
    Assert(Enumerator.ParentInfo = ParentInfo);
  finally FreeAndNil(Enumerator) end;
end;

procedure TVRMLNode.TraverseIntoChildren(StateStack: TVRMLGraphTraverseStateStack;
  NodeClass: TVRMLNodeClass;
  TraversingFunc: TTraversingFunc;
  ParentInfo: PTraversingInfo);
var
  Enumerator: TTraverseEnumerator;
begin
  Enumerator := TTraverseEnumerator.Create;
  try
    Enumerator.StateStack := StateStack;
    Enumerator.NodeClass := NodeClass;
    Enumerator.TraversingFunc := TraversingFunc;
    Enumerator.ParentInfo := ParentInfo;
    Enumerator.DoTraverseIntoChildren(Self);

    { Check that, if all went without exception, Enumerator.ParentInfo
      returned to original state. }
    Assert(Enumerator.ParentInfo = ParentInfo);
  finally FreeAndNil(Enumerator) end;
end;

var
  TraverseSingleStack: TVRMLGraphTraverseStateStack;

procedure TVRMLNode.Traverse(NodeClass: TVRMLNodeClass;
  TraversingFunc: TTraversingFunc);
begin
  TraverseSingleStack.PushClear;
  try
    TraverseInternal(TraverseSingleStack, NodeClass, TraversingFunc, nil);
  finally TraverseSingleStack.Pop end;
end;

function TVRMLNode.NodeTypeName: string;
begin
 result := ClassNodeTypeName;
end;

class function TVRMLNode.ClassNodeTypeName: string;
begin
 result := '';
end;

function TVRMLNode.PathFromWWWBasePath(const RelativePath: string): string;
begin
  { This is a workaround for Blender errorneous VRML 1.0 export.
    Blender exports relative paths by prefixing them by "//"
    (that's a general convention used internally by Blender, AFAIK).
    Here I simply remove this "//". }
  if IsPrefix('//', RelativePath) then
    Result := CombinePaths(WWWBasePath, SEnding(RelativePath, 3)) else
    Result := CombinePaths(WWWBasePath, RelativePath);
end;

procedure TVRMLNode.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
var
  Handled: boolean;
  Position: Integer;
  ChildNode: TVRMLNode;
begin
  VRML1ChildrenClear;

  Position := 0;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  while Lexer.Token <> vtCloseCurlyBracket do
  begin
    Handled := ParseNodeBodyElement(Lexer, Names, Position);

    { VRML 1.0 children nodes are handled as a last resort here
      (that's also why they can't be inside our ParseNodeBodyElement).
      That's because ParseNode just raises exception in case of unknown
      node, so I have to catch first everything else (like "hints" field
      of TNodeShapeHints). }
    if not Handled then
    begin
      if VRML1ChildrenParsingAllowed then
      begin
        ChildNode := ParseNode(Lexer, Names, false);
        ChildNode.PositionInParent := Position;
        VRML1ChildAdd(ChildNode);
      end else
      begin
        raise EVRMLParserError.Create(Lexer,
          Format('Invalid VRML node content (probably unknown or not allowed' +
            ' field, prototype or VRML 1.0-style children) inside "%s": got %s',
            [NodeTypeName, Lexer.DescribeToken]));
      end;
    end;

    Inc(Position);
  end;
  Lexer.NextToken;

  FWWWBasePath := Names.WWWBasePath;

  ParseAfter(Names);
end;

function TVRMLNode.ParseNodeBodyElement(Lexer: TVRMLLexer; Names: TVRMLNames;
  const APositionInParent: Integer): boolean;

  procedure ParseExtensibilityFields;

    procedure ReadOneField;
    var
      FieldTypeName: string;
      //FieldName: string;
      FieldType: TVRMLFieldClass;
    begin
      Lexer.CheckTokenIs(vtName, 'Field type name');
      FieldTypeName := Lexer.TokenName;
      FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
      if FieldType = nil then
        raise EVRMLParserError.Create(
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
    IsAField := TMFString.Create(Self, '', []);
    try
      IsAField.Parse(Lexer, Names, false);

      { TODO: we should actually do something with obtained here
        isA value }
    finally FreeAndNil(IsAField) end;
  end;

var
  I: integer;
  Route: TVRMLRoute;
  Proto: TVRMLPrototypeBase;
  Event: TVRMLEvent;
  IDecl: TVRMLInterfaceDeclaration;
  Import: TVRMLImport;
  ExportItem: TVRMLExport; { "export" is a keyword in Pascal }
begin
  Result := false;

  { If I would know that all fields used are standard, I could
    check first for if Lexer.TokenName[0] in ['a'..'z'], since all
    standard field names start lowercase. But of course I can't,
    all VRML versions allow to define your own nodes and fields. }
  if Lexer.Token = vtName then
  begin
    I := Fields.IndexOf(Lexer.TokenName);
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

      Fields[I].Parse(Lexer, Names, true);
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

    IDecl := TVRMLInterfaceDeclaration.Create(Self);
    InterfaceDeclarations.Add(IDecl);
    IDecl.Parse(Lexer, Names, true, true);
    IDecl.PositionInParent := APositionInParent;
    PostAddInterfaceDeclaration(IDecl);
  end else
  if Lexer.TokenIsKeyword(vkPROTO) then
  begin
    Result := true;

    Proto := TVRMLPrototype.Create;
    Prototypes.Add(Proto);
    Proto.Parse(Lexer, Names);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkEXTERNPROTO) then
  begin
    Result := true;

    Proto := TVRMLExternalPrototype.Create;
    Prototypes.Add(Proto);
    Proto.Parse(Lexer, Names);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkROUTE) then
  begin
    Result := true;

    Route := TVRMLRoute.Create;
    Routes.Add(Route);
    Route.Parse(Lexer, Names);
    Route.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkIMPORT) then
  begin
    Result := true;

    Import := TVRMLImport.Create;
    ImportsList.Add(Import);
    Import.Parse(Lexer, Names);
    Import.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkEXPORT) then
  begin
    Result := true;

    ExportItem := TVRMLExport.Create;
    ExportsList.Add(ExportItem);
    ExportItem.Parse(Lexer, Names);
    ExportItem.PositionInParent := APositionInParent;
  end;
end;

procedure TVRMLNode.ParseXML(Element: TDOMElement; Names: TVRMLNames);
var
  Position: Integer;

  procedure ParseXMLAttributes;
  var
    Attr: TDOMAttr;
    AttrNode: TDOMNode;
    AttrIndex, Index: Integer;
  begin
    { enumerate over all attributes }
    for AttrIndex := 0 to Element.Attributes.Length - 1 do
    begin
      AttrNode := Element.Attributes[AttrIndex];
      Assert(AttrNode.NodeType = ATTRIBUTE_NODE);
      Attr := AttrNode as TDOMAttr;

      { containerField and DEF attributes are handled in ParseNode,
        we can safely ignore them now. }
      if (Attr.Name = SAttrContainerField) or
         (Attr.Name = SAttrDEF) then
        Continue;

      Index := Fields.IndexOf(Attr.Name);
      if Index >= 0 then
      begin
        Fields[Index].ParseXMLAttribute(Attr.Value, Names);
        { Note that saving PositionInParent from XML attributes order
          isn't really useful, as FPC DOM unit already sorted
          Element.Attributes (TDOMNamedNodeMap) by name. }
        Fields[Index].PositionInParent := Position;
        Inc(Position);
      end else
        OnWarning(wtMajor, 'VRML/X3D', 'Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.Name + '" in node "' + NodeTypeName + '"');
    end;
  end;

  procedure ParseXMLChildrenNodes;
  var
    FieldIndex: Integer;
    Child: TVRMLNode;
    ContainerField: string;
    SF: TSFNode;
    MF: TMFNode;
    Route: TVRMLRoute;
    I: TXMLElementIterator;
    Proto: TVRMLPrototype;
    ExternProto: TVRMLExternalPrototype;
    IDecl: TVRMLInterfaceDeclaration;
    Import: TVRMLImport;
    ExportItem: TVRMLExport;
  begin
    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        if I.Current.TagName = 'ROUTE' then
        begin
          Route := TVRMLRoute.Create;
          Route.PositionInParent := Position;
          Routes.Add(Route);
          Route.ParseXML(I.Current, Names);
        end else
        if I.Current.TagName = 'IS' then
        begin
          ParseISStatement(Self, I.Current, Position);
        end else
        if I.Current.TagName = 'IMPORT' then
        begin
          Import := TVRMLImport.Create;
          Import.PositionInParent := Position;
          ImportsList.Add(Import);
          Import.ParseXML(I.Current, Names);
        end else
        if I.Current.TagName = 'EXPORT' then
        begin
          ExportItem := TVRMLExport.Create;
          ExportItem.PositionInParent := Position;
          ExportsList.Add(ExportItem);
          ExportItem.ParseXML(I.Current, Names);
        end else
        if I.Current.TagName = 'ProtoDeclare' then
        begin
          Proto := TVRMLPrototype.Create;
          Proto.PositionInParent := Position;
          Prototypes.Add(Proto);
          Proto.ParseXML(I.Current, Names);
        end else
        if I.Current.TagName = 'ExternProtoDeclare' then
        begin
          ExternProto := TVRMLExternalPrototype.Create;
          ExternProto.PositionInParent := Position;
          Prototypes.Add(ExternProto);
          ExternProto.ParseXML(I.Current, Names);
        end else
        if I.Current.TagName = 'field' then
        begin
          IDecl := TVRMLInterfaceDeclaration.Create(Self);
          try
            IDecl.ParseXML(I.Current, Names, true);
            IDecl.PositionInParent := Position;
            if IDecl.AccessType in HasInterfaceDeclarations then
            begin
              InterfaceDeclarations.Add(IDecl);
              PostAddInterfaceDeclaration(IDecl);
            end else
            begin
              FreeAndNil(IDecl);
              OnWarning(wtMajor, 'VRML/X3D', 'X3D XML: specified <field> inside node, but this node doesn''t allow interface declaration with such accessType');
            end;
          except
            FreeAndNil(IDecl);
            raise;
          end;
        end else
        begin
          Child := ParseXMLNode(I.Current, ContainerField, Names, true);
          if Child <> nil then
          try
            Child.PositionInParent := Position;
            FieldIndex := Fields.IndexOf(ContainerField);

            if (FieldIndex = -1) and
               (ContainerField <> Child.DefaultContainerField) and
               (Child.DefaultContainerField <> '') then
            begin
              { Retry with DefaultContainerField value, since it exists
                and is different than current ContainerField. }
              FieldIndex := Fields.IndexOf(Child.DefaultContainerField);
              if FieldIndex >= 0 then
                OnWarning(wtMajor, 'VRML/X3D', 'X3D XML: containerField indicated unknown field name ("' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + NodeTypeName + '"), using the default containerField value "' + Child.DefaultContainerField + '" succeded');
            end;

            if FieldIndex >= 0 then
            begin
              if Fields[FieldIndex] is TSFNode then
              begin
                SF := Fields[FieldIndex] as TSFNode;

                if SF.Value <> nil then
                  OnWarning(wtMajor, 'VRML/X3D', 'X3D XML: More than one value specified for SFNode field ' + SF.NiceName);

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
                OnWarning(wtMajor, 'VRML/X3D', 'X3D field "' + ContainerField + '" is not SFNode or MFNode, but a node value (XML element) is specified');
            end else
              OnWarning(wtMajor, 'VRML/X3D', 'Unknown X3D field name (indicated by containerField value) "' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + NodeTypeName + '"');
          finally
            { Any OnWarning above makes the Child unused.
              Note that OnWarning may, but doesn't have, raise an exception. }
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
        OnWarning(wtMajor, 'VRML/X3D', Format('VRML/X3D node %s doesn''t allow CDATA section, but it''s specified',
          [NodeTypeName]));
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

  ParseAfter(Names);
end;

procedure TVRMLNode.ParseAfter(Names: TVRMLNames);
begin
end;

procedure TVRMLNode.CreateNode;
begin
end;

type
  TEnumerateNodes0Enumerator = class
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes0Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    Proc(Child);

    Child.DirectEnumerate(@EnumerateChildrenFunction, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(
  Proc: TVRMLNodeProc; OnlyActive: boolean);
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
    NodeClass: TVRMLNodeClass;
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes1Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    if Child is NodeClass then Proc(Child);

    Child.DirectEnumerate(@EnumerateChildrenFunction, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(nodeClass: TVRMLNodeClass;
  Proc: TVRMLNodeProc; OnlyActive: boolean);
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
    NodeClass: TVRMLNodeClass;
    SeekNodeName: string;
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes2Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    if (Child is NodeClass) and
       (Child.NodeName = SeekNodeName) then
      Proc(Child);

    Child.DirectEnumerate(@EnumerateChildrenFunction, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(NodeClass: TVRMLNodeClass;
  const SeekNodeName: string;
  Proc: TVRMLNodeProc; OnlyActive: boolean);
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
  BreakTryFindNode = class(TCodeBreaker)
  public
    FoundNode: TVRMLNode;
    constructor Create(AFoundNode: TVRMLNode);
  end;

  constructor BreakTryFindNode.Create(AFoundNode: TVRMLNode);
  begin
    inherited Create;
    FoundNode := AFoundNode;
  end;

procedure TVRMLNode.TryFindNode_Found(Node: TVRMLNode);
begin
  raise BreakTryFindNode.Create(Node);
end;

function TVRMLNode.TryFindNode(FindClass: TVRMLNodeClass;
  OnlyActive: boolean): TVRMLNode;
begin
  try
    EnumerateNodes(FindClass, {$ifdef FPC_OBJFPC} @ {$endif}
      TryFindNode_Found, OnlyActive);
    Result := nil;
  except
    on B: BreakTryFindNode do Result := B.FoundNode;
  end;
end;

function TVRMLNode.FindNode(FindClass: TVRMLNodeClass; OnlyActive: boolean): TVRMLNode;
begin
  result := TryFindNode(FindClass, OnlyActive);
  Check(result <> nil,
    'Node class '+FindClass.ClassName+' not found (by TVRMLNode.FindNode)');
end;

function TVRMLNode.TryFindNodeByName(
  FindClass: TVRMLNodeClass; const FindName: string;
  OnlyActive: boolean): TVRMLNode;
begin
  try
    EnumerateNodes(FindClass, FindName, {$ifdef FPC_OBJFPC} @ {$endif}
      TryFindNode_Found, OnlyActive);
    Result := nil;
  except
    on B: BreakTryFindNode do Result := B.FoundNode;
  end;
end;

function TVRMLNode.FindNodeByName(
  FindClass: TVRMLNodeClass; const FindName: string;
  OnlyActive: boolean): TVRMLNode;
begin
  result := TryFindNodeByName(FindClass, FindName, OnlyActive);
  Check(result <> nil,
    'Node name '+FindName+' not found (by TVRMLNode.FindNodeByName)');
end;

{ TVRMLNode.TryFindNodeState/Transform ----------------------------------------- }

  type
    PVRMLNode = ^TVRMLNode;
    PVRMLGraphTraverseState = ^TVRMLGraphTraverseState;
    BreakTryFindNodeState = class(TCodeBreaker);
    TTryFindNodeStateObj = class
      PNode: PVRMLNode;
      PState: PVRMLGraphTraverseState;
      procedure TraverseFunc(
        ANode: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
        ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    end;

  procedure TTryFindNodeStateObj.TraverseFunc(
    ANode: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
    ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  begin
    PNode^ := ANode;
    PState^ := TVRMLGraphTraverseState.CreateCopy(StateStack.Top);
    raise BreakTryFindNodeState.Create;
  end;

function TVRMLNode.TryFindNodeState(
  NodeClass: TVRMLNodeClass;
  out Node: TVRMLNode; out State: TVRMLGraphTraverseState): boolean;
var
  Obj: TTryFindNodeStateObj;
begin
  Obj := TTryFindNodeStateObj.Create;
  try
    try
      Obj.PNode := @Node;
      Obj.PState := @State;
      Traverse(NodeClass, @Obj.TraverseFunc);
      result := false;
    except
      on BreakTryFindNodeState do result := true;
    end;
  finally Obj.Free end;
end;

  type
    TTryFindNodeTransformObj = class
      PNode: PVRMLNode;
      PTransform: PMatrix4Single;
      PTransformScale: PSingle;
      procedure TraverseFunc(
        ANode: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
        ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    end;

  procedure TTryFindNodeTransformObj.TraverseFunc(
    ANode: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
    ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  begin
    PNode^ := ANode;
    { to dlatego TryFindNodeTransform jest szybsze od TryFindNodeState :
      w TryFindNodeState trzeba tutaj kopiowac cale state,
      w TryFindNodeTransform wystarczy skopiowac transformacje. }
    PTransform^ := StateStack.Top.Transform;
    PTransformScale^ := StateStack.Top.TransformScale;
    raise BreakTryFindNodeState.Create;
  end;

function TVRMLNode.TryFindNodeTransform(
  NodeClass: TVRMLNodeClass;
  out Node: TVRMLNode;
  out Transform: TMatrix4Single;
  out TransformScale: Single): boolean;
var
  Obj: TTryFindNodeTransformObj;
begin
  Obj := TTryFindNodeTransformObj.Create;
  try
    try
      Obj.PNode := @Node;
      Obj.PTransform := @Transform;
      Obj.PTransformScale := @TransformScale;
      Traverse(NodeClass, @Obj.TraverseFunc);
      Result := false;
    except
      on BreakTryFindNodeState do Result := true;
    end;
  finally Obj.Free end;
end;

function TVRMLNode.TryFindParentByName(const FindName: string): TVRMLNode;
var
  I: integer;
begin
  if NodeName = FindName then
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

function TVRMLNode.FindParentByName(const FindName: string): TVRMLNode;
begin
  result := TryFindParentByName(FindName);
  Check(result <> nil, 'Node name '+FindName+' not found in parents');
end;

function TVRMLNode.TryFindDirectParentByName(const FindName: string): TVRMLNode;
var
  I: Integer;
begin
  for I := 0 to VRML1ParentsCount - 1 do
  begin
    Result := VRML1Parents[I];
    if Result.NodeName = FindName then Exit;
  end;

  for I := 0 to ParentFieldsCount - 1 do
  begin
    Result := ParentFieldsNode[I];
    if Result.NodeName = FindName then Exit;
  end;

  Result := nil;
end;

function TVRMLNode.HasParent(Node: TVRMLNode): boolean;
var
  I: integer;
begin
  if Self = Node then
    result := true else
  begin
    for i := 0 to VRML1ParentsCount - 1 do
      if VRML1Parents[i].HasParent(Node) then Exit(true);

    for i := 0 to ParentFieldsCount - 1 do
      if ParentFieldsNode[i].HasParent(Node) then Exit(true);

    result := False;
  end;
end;

type
  TIsNodePresentSeeker = class
    SeekNode: TVRMLNode;
    procedure Seek(Node: TVRMLNode);
  end;

  BreakIsNodePresent = class(TCodeBreaker);

  procedure TIsNodePresentSeeker.Seek(Node: TVRMLNode);
  begin
    if Node = SeekNode then
      raise BreakIsNodePresent.Create;
  end;

function TVRMLNode.IsNodePresent(Node: TVRMLNode;
  OnlyActive: boolean): boolean;
var
  Seeker: TIsNodePresentSeeker;
begin
  Seeker := TIsNodePresentSeeker.Create;
  try
    Seeker.SeekNode := Node;
    try
      EnumerateNodes({$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek, OnlyActive);
      Result := false;
    except
      on BreakIsNodePresent do Result := true;
    end;
  finally FreeAndNil(Seeker) end;
end;

type
  TNodeCounter = class
    Counter: integer;
    procedure CountNode(node: TVRMLNode);
  end;

  procedure TNodeCounter.CountNode(node: TVRMLNode);
  begin Inc(Counter) end;

function TVRMLNode.NodesCount(NodeClass: TVRMLNodeClass;
  CountOnlyActiveNodes: boolean): integer;
var
  C: TNodeCounter;
begin
  C := TNodeCounter.Create;
  try
    EnumerateNodes(NodeClass,
      {$ifdef FPC_OBJFPC} @ {$endif} C.CountNode, CountOnlyActiveNodes);
    result := C.Counter;
  finally C.Free end;
end;

procedure TVRMLNode.SaveToStream(Writer: TX3DWriter);
begin
  NodeSaveToStream(Writer);
  if Writer.Encoding = xeClassic then Writer.Writeln;
end;

procedure TVRMLNode.NodeSaveToStream(Writer: TX3DWriter; const CurrentContainerField: string);

  { Collect node contents to save.
    You want to save here exactly what you read in ParseNodeBodyElement. }
  function ContentsToSave: TVRMLFileItemsList;
  var
    I: integer;
  begin
    Result := TVRMLFileItemsList.Create;
    try
      if HasInterfaceDeclarations <> [] then
      begin
        for I := 0 to InterfaceDeclarations.Count - 1 do
          Result.Add(InterfaceDeclarations[I]);
      end;

      for I := 0 to Prototypes.Count - 1 do
        Result.Add(Prototypes[I]);

      for I := 0 to Fields.Count - 1 do
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

      for I := 0 to Events.Count - 1 do
        if { Saving InterfaceDeclarations already handled saving events
             with ParentInterfaceDeclaration <> nil, so no need to save them again. }
           (Events[I].ParentInterfaceDeclaration = nil) then
          Result.Add(Events[I]);

      for I := 0 to Routes.Count - 1 do
        Result.Add(Routes[I]);

      for I := 0 to ImportsList.Count - 1 do
        Result.Add(ImportsList[I]);

      for I := 0 to ExportsList.Count - 1 do
        Result.Add(ExportsList[I]);
    except FreeAndNil(Result) end;
  end;

  { Gather IS clauses of fields/events.
    For XML encoding, also gather from fields/events inside interface
    declarations (for classic encoding,
    they are saved by TVRMLInterfaceDeclaration.SaveToStream). }
  procedure CollectIsClauses(out NodeFields, ProtoFields: TDynStringArray);

    procedure AddIsClauses(Item: TVRMLFieldOrEvent);
    var
      N: string;
      I: Integer;
    begin
      N := Item.NameForVersion(Writer.Version);
      if N = '' then Exit; { unnamed fields/events cannot be saved here }
      for I := 0 to Item.IsClauseNames.Count - 1 do
      begin
        NodeFields.Add(N);
        ProtoFields.Add(Item.IsClauseNames[I]);
      end;
    end;

  var
    I: Integer;
  begin
    NodeFields := TDynStringArray.Create;
    ProtoFields := TDynStringArray.Create;

    for I := 0 to Fields.Count - 1 do
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

    for I := 0 to Events.Count - 1 do
      if (Writer.Encoding <> xeClassic) or
         (Events[I].ParentInterfaceDeclaration = nil) then
        AddIsClauses(Events[I]);
  end;

  procedure Classic;
  var
    FileItems: TVRMLFileItemsList;
    NodeFields, ProtoFields: TDynStringArray;
    I: Integer;
  begin
    if (Writer as TX3DWriterNames).NodeNames.Bound(Self) then
    begin
      Writer.WriteIndent('USE ' + NodeName);
    end else
    begin
      (Writer as TX3DWriterNames).NodeNames.Bind(Self, false);

      CollectIsClauses(NodeFields, ProtoFields);
      try
        Writer.WriteIndent('');
        if NodeName <> '' then Writer.Write('DEF ' +NodeName +' ');
        Writer.Writeln(NodeTypeName +' {');

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
    FileItems: TVRMLFileItemsList;
    HasContent: boolean;
    I: Integer;
    WriteContainerField: string;
    ElementName: string;
    ProtoInstance: boolean;
    NodeFields, ProtoFields: TDynStringArray;
  begin
    { Knowing CurrentContainerField and DefaultContainerField,
      we can always correctly set containerField= ourselves. }
    if (CurrentContainerField <> '') and
       (CurrentContainerField <> DefaultContainerField) then
      WriteContainerField := ' containerField=' + StringToX3DXml(CurrentContainerField) else
      WriteContainerField := '';

    { For XML encoding, TVRMLPrototypeNode is saved a little specially. }
    ProtoInstance := Self is TVRMLPrototypeNode;

    if ProtoInstance then
      ElementName := 'ProtoInstance' else
      ElementName := NodeTypeName;

    if (Writer as TX3DWriterNames).NodeNames.Bound(Self) then
    begin
      Writer.WritelnIndent(Format('<%s USE=%s%s />',
        [ElementName, StringToX3DXml(NodeName), WriteContainerField]));
    end else
    begin
      (Writer as TX3DWriterNames).NodeNames.Bind(Self, false);

      CollectIsClauses(NodeFields, ProtoFields);
      try
        Writer.WriteIndent('<' + ElementName);
        if ProtoInstance then
          Writer.Write(' name=' + StringToX3DXml(NodeTypeName));
        if NodeName <> '' then Writer.Write(' DEF=' +StringToX3DXml(NodeName));
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
                 (FileItems[I] is TVRMLField) and
                 (FileItems[I].SaveToXml <> sxNone) then
              begin
                { prototype instance fields must be wrapped in <fieldValue> }
                Writer.WriteIndent('<fieldValue name=' +
                  StringToX3DXml((FileItems[I] as TVRMLField).Name));
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
                  (FileItems[I]  as TVRMLField).FieldSaveToStream(Writer, true, true);
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
     { TVRMLPrototypeNode has somewhat different meaning of PrototypeInstance,
       we want to save it directly (otherwise
       PrototypeInstanceSourceNode.SaveToStream could cause another
       recursive PrototypeInstanceSourceNode.SaveToStream with
       nested proto). For example test read + save
       demo_models/x3d/key_sensor.x3dv, to see that check below is needed. }
     not (Self is TVRMLPrototypeNode) then
  begin
    { If this is an expanded prototype, then delegate writing to the
      PrototypeInstanceSourceNode. }
    PrototypeInstanceSourceNode.NodeSaveToStream(Writer, CurrentContainerField);

    { What to do about
        NodeNames.Bind(Self)
      called from PrototypeInstanceSourceNode.SaveToStream ?
      This means that PrototypeInstanceSourceNode (TVRMLPrototypeNode)
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
    else raise EInternalError.Create('TVRMLNode.SaveToStream Encoding?');
  end;
end;

class function TVRMLNode.VRML1StateNode(out StateNode: TVRML1StateNode): boolean;
var
  SN: TVRML1StateNode;
begin
  { We're in "class function", so Self = reference to our class.
    So simple comparison "Self = ..." is what we want. }
  for SN := Low(SN) to High(SN) do
    if TraverseStateLastNodesClasses[SN] = Self then
    begin
      StateNode := SN;
      Result := true;
      Exit;
    end;
  Result := false;
end;

class function TVRMLNode.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := true;
end;

procedure TVRMLNode.RemoveParentField(Field: TVRMLField);
begin
  Check(FParentFields.Remove(Field) <> -1, 'RemoveParentField: parent not found');
  FreeIfUnused;
end;

procedure TVRMLNode.AddParentField(Field: TVRMLField);
begin
  FParentFields.Add(Field);
end;

class function TVRMLNode.VRML1ChildrenSaveToStream: boolean;
begin
  Result := true;
end;

function TVRMLNode.EnumerateReplaceChildren(
  Func: TEnumerateReplaceNodesFunction): Cardinal;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
  NewNode: TVRMLNode;
begin
  { I don't use EnumerateNodes since I have to enumerate them myself,
    since they may be removed during enumeration.
    The code below mimics TVRMLNode.DirectEnumerateAll implementation,
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
      Result += VRML1Children[I].EnumerateReplaceChildren(Func);
      Inc(I);
    end;
  end;

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
      begin
        NewNode := SF.Value;
        Func(Self, NewNode);
        if NewNode <> SF.Value then
        begin
          SF.Value := NewNode;
          Inc(Result);
        end else
        begin
          Result += SF.Value.EnumerateReplaceChildren(Func);
        end;
      end;
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
      J := 0;
      while J < MF.Items.Count do
      begin
        NewNode := MF.Items[J];
        Func(Self, NewNode);
        if NewNode <> MF.Items[J] then
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
          Result += MF.Items[J].EnumerateReplaceChildren(Func);
          Inc(J);
        end;
      end;
    end;
  end;
end;

  type
    TRemoveChildrenWithMatchingNameHelper = class
      Wildcard: string;
      IgnoreCase: boolean;
      procedure DoIt(ParentNode: TVRMLNode; var Node: TVRMLNode);
    end;

  procedure TRemoveChildrenWithMatchingNameHelper.DoIt(
    ParentNode: TVRMLNode; var Node: TVRMLNode);
  begin
    if IsWild(Node.NodeName, Wildcard, IgnoreCase) then
      Node := nil;
  end;

function TVRMLNode.RemoveChildrenWithMatchingName(
  const Wildcard: string; IgnoreCase: Boolean): Cardinal;
var
  Helper: TRemoveChildrenWithMatchingNameHelper;
begin
  Helper := TRemoveChildrenWithMatchingNameHelper.Create;
  try
    Helper.Wildcard := Wildcard;
    Helper.IgnoreCase := IgnoreCase;
    Result := EnumerateReplaceChildren(@Helper.DoIt);
  finally FreeAndNil(Helper) end;
end;

class function TVRMLNode.URNMatching(const URN: string): boolean;
begin
  Result := false;
end;

function TVRMLNode.FieldOrEvent(const Name: string): TVRMLFieldOrEvent;
var
  I: Integer;
  ResultEvent: TVRMLEvent;
begin
  I := Fields.IndexOf(Name);
  if I <> -1 then
    Exit(Fields[I]);

  { I use helper ResultEvent below, instead of passing
    "TVRMLEvent(Result)" as last param: don't know why,
    but with FPC 2.2.0 this cast may fail (even though it shouldn't
    be checked at all?), testcase:
      view3dscene www.web3d.org/x3d/content/examples/Basic/CAD/CADGeometryPrototypes.x3d
  }

  I := Fields.IndexOfExposedEvent(Name, ResultEvent);
  if I <> -1 then
    Exit(ResultEvent);

  I := Events.IndexOf(Name);
  if I <> -1 then
    Exit(Events[I]);

  Result := nil; { not found }
end;

function TVRMLNode.AnyEvent(const Name: string): TVRMLEvent;
var
  I: Integer;
begin
  I := Fields.IndexOfExposedEvent(Name, Result);
  if I <> -1 then
    Exit; { Result is already set }

  I := Events.IndexOf(Name);
  if I <> -1 then
    Exit(Events[I]);

  Result := nil; { not found }
end;

procedure TVRMLNode.SetHasInterfaceDeclarations(const Value: TVRMLAccessTypes);
begin
  if Value <> HasInterfaceDeclarations then
  begin
    FHasInterfaceDeclarations := Value;
    if HasInterfaceDeclarations <> [] then
    begin
      { make sure InterfaceDeclarations is non-nil }
      if FInterfaceDeclarations = nil then
        FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
    end else
    begin
      { make sure InterfaceDeclarations is nil }
      FreeWithContentsAndNil(FInterfaceDeclarations);
    end;
  end;
end;

{ I couldn't implement this as TVRMLField method, as VRMLFields
  cannot declare TVRMLNode in the interface, so also
  cannot declare TVRMLNodeDeepCopyState. }
procedure FieldDeepCopyContents(Destination, Source: TVRMLField;
  CopyState: TVRMLNodeDeepCopyState);
var
  J: Integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  { First make "shallow copy" of the field by simple Assign }
  Destination.Assign(Source);

  if Destination is TSFNode then
  begin
    SF := TSFNode(Destination);
    if SF.Value <> nil then
      SF.Value := CopyState.DeepCopy(SF.Value);
  end else
  if Destination is TMFNode then
  begin
    MF := TMFNode(Destination);
    for J := 0 to MF.Count - 1 do
      MF.Replace(J, CopyState.DeepCopy(MF.Items[J]));
  end;
end;

procedure EventDeepCopyContents(Destination, Source: TVRMLEvent);
begin
  { For events, simple Assign does all copy we need --- at least for now. }
  Destination.Assign(Source);
end;

function TVRMLNode.DeepCopyCreate(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
begin
  Result := TVRMLNodeClass(ClassType).Create(NodeName, WWWBasePath);
end;

function TVRMLNode.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
var
  I: Integer;
  IDecl: TVRMLInterfaceDeclaration;
begin
  Result := DeepCopyCreate(CopyState);

  try

    { We expand CopyState arrays now, right after DeepCopyCreate.
      This is needed, as later during DeepCopyCore we may need this node
      in case of loops within hierarchy.

      For example, internal
      routes from TVRMLPrototypeNode are established to handle "IS" clauses
      for events. There routes are to/from TVRMLPrototypeNode, and are
      also placed within this TVRMLPrototypeNode instance. So when copying
      node routes, this node must already be present in CopyState arrays.

      Also, in the future we will have to allow loops in Script nodes
      (USE within Script node may refer to the same node). So again loop
      will be created. }
    CopyState.Original.Add(Self);
    CopyState.New.Add(Result);

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

      This DeepCopy is used for now by protos expanding and by TVRMLGLAnimation.
      Neither need prototype links (as protos are already expanded when copying,
      and they don't need anything more).

      for I := 0 to Prototypes.Count - 1 do
        ...(Prototypes[I]);
    }

    Assert(Fields.Count = Result.Fields.Count);
    Assert(Events.Count = Result.Events.Count);

    for I := 0 to Fields.Count - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Fields[I].ParentInterfaceDeclaration = nil then
      begin
        FieldDeepCopyContents(Result.Fields[I], Fields[I], CopyState);

        if Result.Fields[I].Exposed then
        begin
          EventDeepCopyContents(Result.Fields[I].EventIn , Fields[I].EventIn );
          EventDeepCopyContents(Result.Fields[I].EventOut, Fields[I].EventOut);
        end;
      end;

    for I := 0 to Events.Count - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Events[I].ParentInterfaceDeclaration = nil then
        EventDeepCopyContents(Result.Events[I], Events[I]);

    for I := 0 to Routes.Count - 1 do
      Result.Routes.Add(Routes[I].DeepCopy(CopyState));

    for I := 0 to ImportsList.Count - 1 do
      Result.ImportsList.Add((ImportsList[I] as TVRMLImport).DeepCopy(CopyState));

    for I := 0 to ExportsList.Count - 1 do
      Result.ExportsList.Add((ExportsList[I] as TVRMLExport).DeepCopy(CopyState));

    if PrototypeInstance then
    begin
      Result.FPrototypeInstance := PrototypeInstance;
      Result.FPrototypeInstanceSourceNode :=
        CopyState.DeepCopy(PrototypeInstanceSourceNode) as TVRMLPrototypeNode;
      if PrototypeInstanceHelpers <> nil then
        Result.FPrototypeInstanceHelpers := CopyState.DeepCopy(PrototypeInstanceHelpers);
    end;

    Result.DefaultContainerField := DefaultContainerField;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TVRMLNode.DeepCopy: TVRMLNode;
var
  CopyState: TVRMLNodeDeepCopyState;
begin
  CopyState := TVRMLNodeDeepCopyState.Create;
  try
    Result := CopyState.DeepCopy(Self);
  finally FreeAndNil(CopyState); end;
end;

procedure TVRMLNode.PostAddInterfaceDeclaration(IDecl: TVRMLInterfaceDeclaration);
begin
  IDecl.AddFieldOrEvent(Self);
end;

procedure TVRMLNode.AddCustomField(Field: TVRMLField; const Exposed: boolean);
var
  IDecl: TVRMLInterfaceDeclaration;
begin
  Field.Exposed := Exposed;

  IDecl := TVRMLInterfaceDeclaration.Create(Self);
  IDecl.FieldOrEvent := Field;
  Field.ParentInterfaceDeclaration := IDecl;
  InterfaceDeclarations.Add(IDecl);
  PostAddInterfaceDeclaration(IDecl);
end;

function TVRMLNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcNone;
end;

function TVRMLNode.NiceName: string;
begin
  Result := NodeTypeName;
  if NodeName <> '' then
    Result += '(' + NodeName + ')';
end;

{ TVRMLNodesList ------------------------------------------------------------- }

function TVRMLNodesList.FindNodeName(const Name: string): Integer;
begin
  Result := IndexOfName(Name);
end;

function TVRMLNodesList.FindName(const Name: string): TVRMLNode;
var
  I: Integer;
begin
  if Name = '' then
    Exit(nil);

  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.NodeName = Name then
      Exit;
  end;
  Result := nil;
end;

function TVRMLNodesList.IndexOfName(const Name: string): Integer;
begin
  if Name = '' then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Items[Result].NodeName = Name then
      Exit;
  Result := -1;
end;

procedure VRMLNodesList_FreeUnusedAndNil(var List: TVRMLNodesList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      if List.Items[I].VRML1ParentsCount + List.Items[I].ParentFieldsCount = 0 then
        List.Items[I].Free;
    FreeAndNil(List);
  end;
end;

{$endif read_implementation}
