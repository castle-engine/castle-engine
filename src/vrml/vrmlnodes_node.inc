{
  Copyright 2002-2010 Michalis Kamburelis.

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

  { VRML node. Every VRML node class descends from this.

    @italic(Descendant implementors notes:: Each descendant should
    override constructor to create and add his fields and events.
    Like @code(Fields.Add(TSFFloat.Create('width', 2, true))).
    Also, you should define FdXxx properties that allow fast,
    comfortable and type-secure way to retrieve and set these fields. }
  TVRMLNode = class(TVRMLFileItem, IVRMLNode)
  private
    fNodeName: string;
    FWWWBasePath: string;
    FChildren, FParentNodes: TVRMLNodesList;
    function GetChildrenItem(i: integer): TVRMLNode;
    function GetParentNodesItem(i: integer): TVRMLNode;
    procedure SetChildrenItem(I: Integer; Value: TVRMLNode);
  private
    FParentFields: TVRMLFieldsList;
    function GetParentFieldsItem(Index: Integer): TVRMLField;
    function GetParentFieldsNodeItem(Index: Integer): TVRMLNode;
    procedure RemoveParentField(Field: TVRMLField);
    procedure AddParentField(Field: TVRMLField);
    function GetSmartChildren(Index: Integer): TVRMLNode;
    procedure AndSuggestedVRMLVersion(
      var Result: boolean;
      var VerMajor, VerMinor, SuggestionPriority: Integer;
      const NewResult: boolean;
      const NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer);
    procedure TryFindNode_Found(Node: TVRMLNode);
  private
    FPrototypes: TVRMLPrototypeBasesList;
    FRoutes: TVRMLRoutesList;
    FImportsList, FExportsList: TVRMLFileItemsList;
    FFields: TVRMLFieldsList;
    FEvents: TVRMLEventsList;
    FPrototypeInstance: boolean;
    FPrototypeInstanceSourceNode: TVRMLPrototypeNode;
    FPrototypeInstanceHelpers: TVRMLNode;
    FDefaultContainerField: string;
    FExplicitContainerField: string;
    FHasInterfaceDeclarations: TVRMLAccessTypes;
    procedure SetHasInterfaceDeclarations(const Value: TVRMLAccessTypes);
  private
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;
    FCDataAllowed: boolean;
    FCDataExists: boolean;
    FCData: string;
    FDestructionNotifications: TDynNodeDestructionNotificationArray;
    FEventsEngine: TVRMLEventsEngine;

    { Internally used by PROTO instantiating mechanism,
      in TVRMLPrototypeNode.InstantiateIsClauses
      and TVRMLPrototypeNode.Instantiate.

      This is always @false outside of TVRMLPrototypeNode.Instantiate
      implementation (as TVRMLPrototypeNode.Instantiate sets it to @true
      and changes back to @false for appropriate nodes). }
    NeedsInstantiateIsClause: boolean;
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
    fAllowedChildren: boolean;
    fParsingAllowedChildren: boolean;

    { This enumerates all active child nodes of given node.

      "Active nodes" are the ones affecting current VRML graph look
      or collisions, e.g. from Switch
      node only one child will be enumerated.
      See @link(Traverse) for more precise definition.

      "Direct" means that this enumerates only direct
      descendants, i.e. this is not recursive.
      See methods like Traverse or EnumerateNodes if you
      want recursive behavior.

      This can enumerate both @link(Children) nodes in VRML 1.0
      style and nodes within TSFNode and TMFNode fields.

      Default implementation in this class returns all Children
      nodes of VRML 1.0. If you need to remove some children
      for VRML 1.0 (e.g. for Switch or LOD nodes)
      or add some children for VRML 2.0 you
      have to override this. You're not required to call
      inherited when overriding this. }
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); virtual;

    { This enumerates all active child nodes of given node,
      and may additionally modify StateStack. It's used by Traverse.

      Default implementation in this class simply calls
      DirectEnumerateActive, ignoring StateStack, and this is suitable
      for 99% of nodes. However, for some special nodes (only Collision
      node for now), they have to modify state during traversing into
      various children, and then they can override this. }
    procedure DirectEnumerateActiveForTraverse(
      Func: TEnumerateChildrenFunction;
      StateStack: TVRMLGraphTraverseStateStack); virtual;

    { This simply enumerates all direct descendant nodes of
      this node. I.e. all children in VRML 1.0 style and
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

    { You can override these methods to determine what happens when
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

    (* This will be called by SaveToStream within { }.
       Usually you want to save here what you read in your overridden
       ParseNodeBodyElement. *)
    procedure SaveContentsToStream(SaveProperties: TVRMLSaveToStreamProperties;
      NodeNames: TVRMLNodeNames); virtual;

    { Called at the end of parsing (in both classic and XML encoding). }
    procedure ParseAfter(Names: TVRMLNames); virtual;
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

    { Children property lists children VRML nodes, in the sense of VRML 1.0.
      In VRML 2.0, nodes never have any Children nodes expressed on this
      list (however, their children nodes may be expressed as items
      of TMFNode / TSFNode fields).

      Kazdy VRML nodes moze miec dowolnie wiele Children.
      Kiedy jakis node jest na liscie Children
      jednego node'a to ma swojego rodzica na swojej liscie ParentNodes.
      Wiec w ten sposob mozemy podrozowac po grafie w obie strony.
      (pamietaj ze graf VRML'a nie ma cykli gdy na niego patrzec jak na graf
      skierowany (a takim wlasnie jest) ale kazdy node moze miec wiele rodzicow
      wiec jezeli potraktujemy go jako graf nieskierowany to mozemy otrzymac
      cykle; wszystko przez to ze node moze miec wiele ParentNodes bo moze
      uzywac mechanizmu USE).

      Kiedy jakis node jest na liscie Children innego node'a to gdy ten inny
      node bedzie go kasowal ze swojej listy Children (a w destruktorze
      kazdy node kasuje wszystkich ze swojej listy Children) to wywola
      jego destruktora. Innymi slowy, gdy jakis node jest czyims dzieckiem
      to jest reference-counted i automatycznie zwalniany.
      Actually, nodes can be children of both nodes (VRML 1.0 style,
      then Children and ParentNodes is used) or fields (TMFNode or TSFNode,
      in VRML 2.0 style; then ParentFields is used). So the node is freed
      only when it's not referenced by any node and not referenced by any
      field.

      Wazna konwencja : jak widac, rodzic automatycznie martwi sie o swoje
      dzieci. Natomiast dziecko w swoim Free nie martwi sie o uaktualnienie
      swoich rodzicow.

      Zwracam tez uwage ze RemoveChild wymaga indeksu. Okreslanie dziecka
      jako children: TVRMLNode jest nie-1-znaczne bo przeciez jeden node
      moze miec kilka razy to samo dziecko (i w rezultacie, nawiasem mowiac,
      kazde dziecko moze miec wiele razy tego samego Parenta). A nie chcemy
      przeciez pomieszac sobie kolejnosci w Children (ona determinuje
      przeciez kolejnosc przegladania grafu, a wiec Renderowania itp.)
      (Natomiast mozemy sobie pozwolic i nieraz pozwalamy na ew. pomieszanie
      kolejnosci w ParentNodes; inaczej musielibysmy z kazdym ParentNodes pamietac
      swoj index na jego liscie). Tak wiec na listach Children i ParentNodes
      moga byc duplikaty i zdecydowanie nie powinnismy nigdzie niefrasobliwie
      "czyscic" tych list przez DeleteDuplicates;

      You can also replace one children with another by writing
      to this property, like @code(Children[I] := NewChildren;).
      This works like a shortcut for
      @code(RemoveChild(I); AddChild(I, NewChildren);).
      But 1. it's more efficient; 2. it's safer --- if
      Children[I] is already equal to NewChildren, then
      first @code(RemoveChild(I);) would free this children and following
      AddChild would be totally wrong. }
    property Children[i: integer]: TVRMLNode
      read GetChildrenItem write SetChildrenItem;

    function ChildrenCount: integer;

    { AddChild z Index (musi byc w zakresie 0..ChildrenCount)
      przesuwa elementy o numerach Index i wiekszych w prawo i
      wstawia child na wskazane Index.    }
    procedure AddChild(Index: Integer; child: TVRMLNode); overload;

    { AddChild bez Indexu - dodaje na koniec listy Children. }
    procedure AddChild(child: TVRMLNode); overload;

    procedure RemoveChild(i: integer);
    procedure RemoveAllChildren;

    { Remove and return children indexed I, and @italic(never free it).

      Compare this with RemoveChild, that removes children I and
      frees it if it's reference count gets 0.

      ExtractChild removes children I, appropriately adjusting
      all parent/children links, but even if reference count of the node
      will get zero, ExtractChild will not free it.
      ExtractChild always returns extracted child. }
    function ExtractChild(I: Integer): TVRMLNode;

    { All nodes where this node is referenced as a child.
      This counts "parents" in the VRML 1.0 / Inventor sense.

      @groupBegin }
    property ParentNodes[i: integer]:TVRMLNode read GetParentNodesItem;
    function ParentNodesCount: integer;
    { @groupEnd }

    { This lists all SFNode and MFNode fields where this node is referenced.
      This is somewhat analogous for ParentNodes, but for VRML 2.0.

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

      Use this if you really want to remove all node occurences from the middle
      of VRML hierarchy. }
    procedure FreeRemovingFromAllParents;

    { Free if this node is not referenced by any VRML 1.0 parent node
      or VRML 2.0 SFNode or MFNode fields. This is a safe way of removing
      a node that may, but doesn't have to, be part of VRML graph.
      The idea is that if node is a part of the graph,
      we will do nothing (assuming you have
      a reference to the entine graph somewhere), otherwise node is
      considered unused and freed.

      For safety, I advice to usually set reference to @nil after calling
      FreeIfUnused, like

@longCode(#
  Child.FreeIfUnused;
  Child := nil;
#)
       }
    procedure FreeIfUnused;

    { AllowedChildren okresla jakie dzieci moga byc dziecmi tego node'a.
      Warunek ten bedzie sprawdzany w AddChild wiec nigdy nie uda ci sie dodac
      node'a ktory nie jest tutaj dozwolony.

      ParsingAllowedChildren okresla jakie dzieci moga byc odczytane
      ze strumienia jako dzieci tego node'a. Chwilowo ma to zastosowanie
      tylko dla wezlow *Inline ktore w strumieniu nie moze miec zapisanych
      zadnych dzieci ale laduja swoje inline jako swoje Child.
      Wiec musza miec ParsingAllowedChildren=[] i AllowedChildren = All.

      TODO: jak bedzie mi to potrzebne to zaimplementuje te pola jako
      tablice TDynVRMLNodeClassArray z dodatkowym polem Any. Taka tablica
      bedzie pasowala do wszystkiego gdy Any = true, wpp. tylko do wymienionych
      na niej elementow. Wartosc *AllowedChildren = true tutaj odpowiadac
      bedzie Any = true tam, *AllowedChildren = false oznacza Any = false i Items=[].
      Wartosc Any jest potrzebna zeby na zapas powiedziec : wszystkie node'y
      sa tu dozwolone, nawet takie o jakich jeszcze nie wie NodesManager -
      - a jest to przeciez czesta sytuacja.

      Naturalnie ParsingAllowedChildren musi sie zawierac w AllowedChildren
      bo inaczej parsowanie moze wygenerowac wyjatek (wywolujac
      AddChild z niedozwolonym argumentem). W tej chwili oznacza to tylko
      ze nie moze byc ParsingAllowedChildren = true i AllowedChildren = false.

      Note that in some special cases AllowedChildren and
      ParsingAllowedChildren values may be changed during object lifetime.
      Currently, this may concern TVRMLUnknownNode. }
    property AllowedChildren: boolean read fAllowedChildren default false;
    property ParsingAllowedChildren: boolean
      read fParsingAllowedChildren default false;

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

    { Konstruktor. Inicjuje wszystko (jak to konstruktor), w szczegolnosci :
      @unorderedList(
        @item(inicjuje NodeName, WWWBasePath na podstawie podanych tu parametrow)
        @item(
          inicjuje tablice Fields ustawiajac polom ich defaultowe wartosci
          (dla implementatorow podklas TVRMLNode:
          w klasie TVRMLNode inicjujemy Fields na tablice o 0 elementach;
          w konstruktorze podklasy musisz wywolac Fields.Add(...)
          aby dodac sobie odpowiednie pola))
        @item(
          [Parsing]AllowedChildren
          (dla implementatorow podklas TVRMLNode:
          w klasie TVRMLNode inicjujemy je na zbiory puste (tzn. chwilowo po prostu
          na false) po prostu dlatego ze wydaje sie to byc najczestsza wartoscia.
          W konstruktorze podklas mozesz swobodnie zmienic wartosci tych pol.))
      ) }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); virtual;

    { CreateParse simply does Create and then calls Parse. }
    constructor CreateParse(const ANodeName: string; Lexer: TVRMLLexer; Names: TVRMLNames);

    destructor Destroy; override;

    { NodeTypeName zwraca nazwe klasy w VRML'u. Zawsze jest <>''.
      To ma byc normalna nazwa node'a, taka ktora odczytujemy
      i zapisujemy bezposrednio z/do pliku VRMLa (wobec tego jest ona tez
      case-sensitive, jak caly VRML).

      Nie zmienia sie przez caly czas zycia obiektu, tzn. raz zainicjowana w
      konstruktorze juz taka pozostaje. Even for special nodes, like
      TVRMLUnknownNode and TVRMLPrototypeNode (where this is determined
      at runtime, since these special nodes are used to instantiate special
      nodes that are not built-in) --- but still even for these special
      nodes, NodeTypeName is constant for the life of this object.

      W tej klasie NodeTypeName zwraca ClassNodeTypeName. Uwagi do
      implementacji podklas TVRMLNode dotyczace tej funkcji - patrz
      ClassNodeTypeName. }
    function NodeTypeName: string; virtual;

    { ClassNodeTypeName zwraca nazwe klasy VRMLa zwiazanej z tym node'm lub
      '' w przypadku klas ktore nie maja zwiazanej ze soba 1-znacznej nazwy
      typu wezla VRMLa ktory reprezentuja (a poniewaz kazda klasa wezla VRMLa
      musi miec NodeTypeName <> '' wiec oznacza to ze te wyjatkowe klasy ustalaja
      sobie NodeTypeName w jakis inny, specjalny sposob - jedyny dostepny
      w tej chwili przyklad tego to TVRMLUnknownNode (chociaz nie wykluczam sobie
      w tym momencie czy nie pojawi sie kiedys jeszcze jakis inny tego typu node))

      Jezeli masz do dyspozycji instancje obiektu to nie powinienes uzywac
      tej funkcji. Jedyna jej zaleta ponad NodeTypeName jest ze jest funkcja
      klasy. Jezeli masz do dyspozycji tylko klase obiektu to uzywajac
      tej funkcji musisz sie zdecydowac co zrobic jesli dostaniesz w odpowiedzi
      '' (albo jakos sie zabezpieczyc zeby nigdy w danym kontekscie takich
      klas nie miec...)

      Uwagi do implementacji podklas TVRMLNode:
      W tej klasie ClassNodeTypeName zwraca ''. Wszystkie niesbtrakcyjne
      klasy wezlow VRMLa musza pokrywac albo ClassNodeTypeName (i w ten
      sposob staja sie normalnymi klasami ktore maja zawszetaka sama wartosc
      NodeTypeName dla swojej klasy) albo NodeClassTypeName (i w ten sposob
      staja sie klasami specjalnymi, jak TVRMLUnknownNode, ktore nie maja
      stalej wartosci NodeTypeName). }
    class function ClassNodeTypeName: string; virtual;

    { Traverse enumerates all nodes of VRML graph that are active.

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
      hierarchy (you can't use ParentNodes / ParentFields of the current node,
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

  if Self is NodeClass then TraversingAfterFunc (Self, State);

  dodaj Self do stanu State do LastNode (o ile Self wsrod
    TraverseStateLastNodesClasses);
)

      Note: I didn't decide yet whether TraversingAfterFunc
      should be before or after AfterTraverse call. Report if you have
      any good reason for any setting.

      Note: setting TraverseIntoChildren to false means that some
      part of the tree is explicitly omitted from traversing.
      Use this only if you know what you're doing, as for some
      nodes they actually affect their parents too (for example,
      chilren within VRML 1.0 Group affect other nodes too;
      global lights within VRML >= 2.0 affect all nodes; and so on...).
      Usually, you will use this only for separator-like nodes
      (for VRML >= 2.0, all Group, Transform, Switch etc. act like separators),
      and only when you will somehow traverse into these nodes anyway.

      Jezeli zostalo wykonane BeforeTraverse, na pewno zostanie wykonane tez
      AfterTraverse (wywolanie AfterTraverse jest w finally..end).

      Kolejnosc w jakiej przechodzi graf jest naturalnie istotna.
      W czasie wykonywania Traverse mozesz modyfikowac tylko node'y dzieci
      (bezposrednie i niebezposrednie) node'a na ktorym wlasnie stoisz. }
    procedure Traverse(
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc;
      TraversingAfterFunc: TTraversingAfterFunc = nil);

    { This is like @link(Traverse), but it takes explicit starting state stack
      and starting ParentInfo. Not generally useful, use only for special
      purposes. }
    procedure TraverseInternal(StateStack: TVRMLGraphTraverseStateStack;
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc;
      TraversingAfterFunc: TTraversingAfterFunc;
      ParentInfo: PTraversingInfo);

    { Enumerate all our children of some class. Recursively.
      Zwroci do proc() takze sam obiekt na ktorym EnumerateNodes zostalo
      wywolane, jezeli tylko ten obiekt jest klasy nodeClass.

      This enumerates both VRML 1.0 @link(Children) as well as
      nodes in TSFNode and TMFNode fields.

      If OnlyActive then it will enumerate only active parts
      of the graph ("active" as defined by @link(Traverse)),
      so it will work as a simpler version of Traverse
      (simpler, because it doesn't track any state).

      If not OnlyActive then it will simply enumerate all nodes.
      This will include then also prototype helpers, if this node
      was expanded from prototype: see PrototypeInstanceSourceNode
      and PrototypeInstanceHelpers.

      Wersja z argumentem SeekNodeName wymaga ponadto aby node mial NodeName=
      SeekNodeName (gdy SeekNodeName = '' to znajduje nienazwane node'y,
      wiec wartosc '' nie jest tu traktowana specjalnie).

      Zaczyna przegladac dzieci dopiero jak przegladnie Self. Jezeli np.
      w proc. zmodyfikowales (np. dodales) wlasne Children to EnumerateNodes
      will enumerate these new children. To ma znaczenie np. w
      TVRMLScene.LoadAllInlined gdzie w proc robimy LoadInlined. Poniewaz
      EnumerateNodes przeglada dzieci po wywolaniu proc., wiadomo ze
      przegladnie tez nowo zaladowane dziecko.

      BTW modyfikowanie dzieci node'a ktory wlasnie dostales do proc()
      to jedyna dozwolona modyfikacja na hierarchii VRMLa ktora mozesz
      wykonywac w czasie EnumerateNodes.

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

    { Znajdz pierwszy Node (zadanej klasy NodeClass) razem ze State
      (lub tylko z Transform).
      Dziala jak TraverseFromDefaultState ktore zatrzymuje sie po pierwszej udanej probie.
      W przypadku TryFindNodeTransform nie musisz o tym pamietac,
      no i TryFindNodeTransform dziala nieco szybciej.

      Zwraca false and sets Node, State and Transform to undefined
      (because they are "out" params) if not found.

      @groupBegin }
    function TryFindNodeState(
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out State: TVRMLGraphTraverseState): boolean;
    function TryFindNodeTransform(
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out Transform: TMatrix4Single;
      out AverageScaleTransform: Single): boolean;
    { @groupEnd }

    { This seeks Self and parent nodes (from ParentNodes and ParentFields,
      recursively), for given node name.

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

    { Przeszukuje podobnie jak powyzsze FindParentByName. Zwraca true
      jesli znalazl tam gdzies node Node. }
    function HasParent(Node: TVRMLNode): boolean;

    { Searches immediate parents of this node for a node with given FindName.
      Returns @nil if not found. }
    function TryFindDirectParentByName(const FindName: string): TVRMLNode;

    { sprawdza czy istnieje w grafie VRML'a zaczepionym w danym punkcie
      node Node.

      If OnlyActive, then only active parts are searched
      ("active" as defined by @link(Traverse)). }
    function IsNodePresent(Node: TVRMLNode; OnlyActive: boolean): boolean;

    { policz ile jest node'ow danej klasy.
      Uzywajac np. TVRMLLightNode mozesz
      sprawdzic czy na scenie zostalo zdefiniowane jakiekolwiek swiato.

      If CountOnlyActiveNodes, then only active parts are searched
      ("active" as defined by @link(Traverse)).

      This traverses both VRML 1.0 children nodes and VRML 2.0 nodes
      inside SFNode and MFNode fields. }
    function NodesCount(NodeClass: TVRMLNodeClass;
      CountOnlyActiveNodes: boolean): integer;

    (*Save node to stream. This saves everything, including node name,
      node type, then node contents within { }.

      We use SaveProperties.NodeNames, pretty much like when parsing.
      If a node name is already bound with this node, then we know
      we have to write only USE ... statement. Otherwise we write
      full node contents, with eventual DEF ... statement.

      Note that if ChildrenSaveToStream returns @false
      we don't write our Children. Currently this is used by various inline
      nodes (WWWInline, Inline, etc.).
    *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;

    { szuka tej klasy node'a (rzeczywistej koncowej klasy, z ClassType) w
      TraverseStateLastNodesClasses. Zwraca indeks lub -1 jesli nie znalazl. }
    class function TraverseStateLastNodesIndex: Integer;

    { Some of the nodes are meant to be handled only for specific
      VRML versions. This functions says whether this node is supposed
      to be present in given VRML version. VerMajor and VerMinor
      arguments are expected in the same form as TVRMLLexer.VRMLVerMajor,
      TVRMLLexer.VRMLVerMinor.

      For example some nodes can only work in VRML < 2.0,
      some others only in VRML >= 2.0. There are even some pairs
      of nodes: for example TNodeCone_1 works with VRML < 2.0,
      TNodeCone_2 works with VRML >= 2.0.

      NodesManager will use this.

      Default implementation of this function returns always @true.
      Generally, I don't try to set this too aggresively ---
      in other words, for all cases when it's sensible, I allow
      nodes to be used in every VRML version, even when official
      specification doesn't. This means that when reading VRML 1.0
      files actually a large part of VRML 2.0 is allowed too,
      and also while reading VRML 2.0 many constructs from VRML 1.0
      (officially no longer present in VRML 2.0) are allowed too.
      I'm trying to support what I call a "sum of VRML 1.0 and 2.0".

      In practice I only use this function when both VRML 1.0 and 2.0
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
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      virtual;

    { MFNode field of this node that should be treated as general
      "children" field of this node. This is used in some places,
      like SmartAddChild.

      Should return nil if this node doesn't have such
      field (that's the default implementation in this class).
      This should always return the same value for given class instance
      (in other words, don't implement this to sometimes return one field,
      sometimes the other, sometimes nil, etc.). }
    function ChildrenField: TMFNode; virtual;

    { These operate on children nodes, in either VRML 2.0 style
      (if ChildrenField is non-nil, then these get/set ChildrenField.Items)
      or in VRML 1.0 style (if ChildrenField is nil, then these get/set
      our Children).

      This is useful to operate on grouping nodes both in VRML 1.0 and VRML 2.0
      style using the same code. }
    procedure SmartAddChild(Node: TVRMLNode);
    property SmartChildren[Index: Integer]: TVRMLNode read GetSmartChildren;
    function SmartChildrenCount: integer;
    function SmartExtractChild(Index: Integer): TVRMLNode;

    { SuggestedVRMLVersion determines what VRML header to use
      when saving the node to file. Returns @true and sets out arguments
      if some version is preferred, otherwise returns @false.

      SuggestionPriority should be used to indicate the "strongness"
      of this suggestion. The idea is that if there are two nodes
      that have different VRML version suggestions, then the one
      with greater SuggestionPriority "wins".

      Currently used priorities:

      @unorderedList(
        @item(1000 for nodes that are only in VRML <= 1.0,
          or only in VRML >= 2.0. This applies to majority of nodes.)

        @item(100 for nodes that are only for VRML >= 2.0 according to specifications,
          but in our engine they were often used also in VRML 1.0 files.
          That is possible thanks to our implementation, see
          [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_mix_vrml_1_2].
          This concerns Fog, Background and a few other nodes.

          By using weaker priority for such nodes, VRML 1.0 files with
          "bits" of VRML 2.0 will be retained as VRML 1.0. (They are readable
          only by our engine anyway.) On the other hand, only Background
          node will be correctly saved as VRML 2.0 file.)

        @item(2000 for nodes that are only in X3D, that is VRML >= 3.0.

          X3D nodes suggest version 3.2 (not 3.0, because some features (shaders)
          are only in ammendment 1 I think, so 3.1 is safer and for simplicity
          (since I looked mostly at 3.2) 3.2 is even better).

          They use priority 2000, so they are slightly stronger than
          VRML 97 nodes. This way a model with mixed VRML 97 and X3D nodes
          will be judged as X3D. And that's Ok, because almost everything
          (incompatible exceptions are some NURBS and geo changes)
          valid in VRML 97 is also valid in X3D.)

        @item(10 * 1000 is used by TVRMLRootNode_1 and TVRMLRootNode_2
          ForceVersion mechanism. This way we can always force VRML version
          using this node, which is useful to write files with the same
          VRML version as was read from disk.)
      )

      Default implementation in this class enumerates all
      SFNode and MFNoden fields and Children nodes
      and determines their suggested VRML version. }
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; virtual;

    { Returns should SaveToStream save our Children.
      In this class default implementation returns @true,
      this is what you will want in 99% of cases.
      It's useful to set this to false if you use
      Children internally, e.g. *Inline nodes. }
    class function ChildrenSaveToStream: boolean; virtual;

    { Enumerates all children nodes (recursively),
      allowing you to decide for each node to replace or remove it.

      So this is something like EnumerateNodes,
      except that it allows you to remove the nodes. It always
      enumerates all nodes, not only active (e.g. it enumerates all
      Switch node children, not only the chosen one).

      Note that (unlike regular EnumerateNodes) @bold(this doesn't
      report Self to Func !). Which is natural, since this may remove
      nodes by normal RemoveChild calls, so it needs to know ParentNode
      of the removed node.

      For each node Func will be called, with ParentNode and Node set.
      If you change the Node to something else, then the old node will
      be removed and new Node inserted in the same place.
      If new Node is @nil, then only the old node will be removed.

      Nodes are traversed in depth-first search. Node is first reported
      to Func, and then (if it's not replaced) we descend into this Node.

      @returns The number of removed nodes. }
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
      This means: DestructionNotifications, EventsEngine, ParentNodes,
      ParentFields. ParentNodes and ParentFields will be set for children
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
      ../../../kambi_vrml_test_suite/vrml_2/warnings/errors/proto_leak.wrl and
      ../../../kambi_vrml_test_suite/vrml_2/warnings/errors/proto_leak_2.wrl
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

    { Traverses all Blender objects/meshes instances in this model,
      assuming that this VRML node was created by Blender VRML 1.0 or 2.0
      exporter.

      For each Blender object (which means, for each Blender mesh instantiation),
      this calls TraversingFunc. Since each Blender object is unique in file,
      you can be sure that each BlenderObjectNode will be enumerated only
      once by TraversingFunc, @italic(as long as this file was really
      made by Blender exporter). As for BlenderObjectName, Blender VRML 1.0
      exporter doesn't write object names (only meshes), so it's always ''
      for VRML 1.0.

      Mesh may occur many times in the file, and both Blender exporters
      correctly use VRML DEF/USE mechanism, so the same BlenderMeshNode
      and BlenderMeshName may be enumerated many times by TraversingFunc.

      Implementation of this follows the logic of Blender VRML 1.0 and 2.0
      standard exporters, there's no other way to implement this.
      If you wrote in Python your own Blender exporter for VRML,
      this method may obviously not work. But it's guaranteed that this
      method will not crash or anything on any VRML model. The worst thing
      that can happen on all VRML models is simply that TraversingFunc will
      enumerate something that doesn't correspond to any Blender object... }
    procedure TraverseBlenderObjects(
      TraversingFunc: TBlenderTraversingFunc); overload;

    { Default value of "containerField" attribute for this node in X3D XML
      encoding. }
    property DefaultContainerField: string
      read FDefaultContainerField write FDefaultContainerField;

    { Value of "containerField" attribute specified explicitly
      for this node in X3D XML encoding. This is practically usable
      (read/write) only by X3D XML reader. }
    property ExplicitContainerField: string
      read FExplicitContainerField write FExplicitContainerField;

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

    { Does this node allow CDATA section when encoded in XML.
      See X3D XML encoding specification about
      "Encapsulating Script node code", instantreality also uses
      CDATA to encode shader source code within XML file and this
      seems sensible (following the intention of the spec?).

      This is only used to produce eventual warnings when CDATA is
      encountered. Whether or not CDataAllowed is @true, we will parse
      CDATA anyway into @link(CData) value.

      This should be set in descendants constructor. }
    property CDataAllowed: boolean read FCDataAllowed write FCDataAllowed;

    { CDATA section when this node is encoded in XML.
      See X3D XML encoding specification.
      When CDataExists = @false, CData is always empty.

      @groupBegin }
    property CDataExists: boolean read FCDataExists write FCDataExists;
    property CData: string read FCData write FCData;
    { @groupEnd }

    { Functions registered here will be called when this TVRMLNode descendant
      will be destroyed. }
    property DestructionNotifications: TDynNodeDestructionNotificationArray
      read FDestructionNotifications;

    { Events processing object for this node, or @nil if none.

      Currently this can be only an instance of TVRMLScene class.
      TVRMLScene.ProcessEvents is always @true while it's set
      as EventsEngine.

      Note: While it is possble and perfectly fine to have
      the same VRML node included in more than one TVRMLScene instance,
      only one of such scenes may have ProcessEvents = @true. Otherwise,
      some things could get unsynchronized, like TVRMLScene.Time that is
      also recorded in TVRMLRoute to avoid loops.
      So a node may inside at most one TVRMLScene with events processing
      at the same time. So this simple property is enough (no need to
      change it to something like TVRMLScenesList). This is also the reason
      why this shouldn't be treated as a "parent TVRMLScene" for arbritrary
      purposes, it's only for events processing things! }
    property EventsEngine: TVRMLEventsEngine
      read FEventsEngine write FEventsEngine;

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
  end;

  TObjectsListItem_3 = TVRMLNode;
  {$I objectslist_3.inc}
  TVRMLNodesList = class(TObjectsList_3)
    function FindNodeName(const Name: string): Integer;
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

constructor TVRMLNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited Create;
  FAllowedChildren := false;
  FParsingAllowedChildren := false;

  FNodeName := ANodeName;
  FWWWBasePath := AWWWBasePath;

  FChildren := TVRMLNodesList.Create;
  FParentNodes := TVRMLNodesList.Create;
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

  if FChildren <> nil then RemoveAllChildren;

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

  FreeAndNil(FChildren);
  FreeAndNil(FParentNodes);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TVRMLNode.FreeIfUnused;
begin
  if (FParentNodes.Count = 0) and
     (FParentFields.Count = 0) then
  begin
    { This is written as "Self.Destroy" to actually do the desctruction,
      freeing memory etc. If I would just call it "Destroy", it would
      perform what destructor does but leaving object instance unfreed. }
    Self.Destroy;
  end;
end;

procedure TVRMLNode.AddChild(Index: Integer; child: TVRMLNode);
begin
 Check( {is child allowed in AllowedChildren ?} AllowedChildren,
   'Node '+NodeTypeName+' is not allowed to have child node of type '+
   Child.NodeTypeName);
 child.FParentNodes.Add(Self);
 FChildren.Insert(Index, child);
end;

procedure TVRMLNode.AddChild(child: TVRMLNode);
begin
 AddChild(FChildren.Count, child);
end;

procedure TVRMLNode.RemoveChild(i: integer);
var
  OldChild: TVRMLNode;
begin
  OldChild := FChildren[i];
  FChildren.Delete(i);
  OldChild.FParentNodes.Remove(Self);
  OldChild.FreeIfUnused;
end;

function TVRMLNode.ExtractChild(I: Integer): TVRMLNode;
begin
  Result := FChildren[i];
  FChildren.Delete(i);
  Result.FParentNodes.Remove(Self);

  { RemoveChild now does
      OldChild.FreeIfUnused;
    but ExtractChild doesn't do it. }
end;

procedure TVRMLNode.SetChildrenItem(I: Integer; Value: TVRMLNode);
var
  OldChild: TVRMLNode;
begin
  { Inefficient implementation: RemoveChild(I); AddChild(I, Value); }

  if Value <> FChildren[I] then
  begin
    Check( {is child allowed in AllowedChildren ?} AllowedChildren,
      'Node '+NodeTypeName+' is not allowed to have child node of type '+
      Value.NodeTypeName);

    OldChild := FChildren[i];
    FChildren[I] := Value;

    OldChild.FParentNodes.Remove(Self);
    OldChild.FreeIfUnused;

    Value.FParentNodes.Add(Self);
  end;
end;

procedure TVRMLNode.RemoveAllChildren;
begin
 while FChildren.Count > 0 do RemoveChild(0);
end;

function TVRMLNode.GetChildrenItem(i: integer): TVRMLNode; begin result := FChildren[i] end;
function TVRMLNode.GetParentNodesItem(i: integer): TVRMLNode; begin result := FParentNodes[i] end;

function TVRMLNode.ChildrenCount: integer; begin result := FChildren.Count end;
function TVRMLNode.ParentNodesCount: integer; begin result := FParentNodes.Count end;

procedure TVRMLNode.FreeRemovingFromAllParents;
var
  i, j: integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  if Self = nil then exit;

  for i := 0 to FParentNodes.Count - 1 do
  begin
    j := FParentNodes[i].FChildren.IndexOf(Self);
    FParentNodes[i].FChildren.Delete(j);
    { nie musimy sie tu martwic usuwaniem naszego Parenta z listy
      FParentNodes ktora
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
  for I := 0 to ChildrenCount - 1 do
    Func(Self, Children[I]);
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
  SF: TSFNode;
  MF: TMFNode;
begin
  for I := 0 to ChildrenCount - 1 do
    Func(Self, Children[I]);

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
        Func(Self, SF.Value);
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
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
    TraversingAfterFunc: TTraversingAfterFunc;
    ParentInfo: PTraversingInfo;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TTraverseEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  var
    LastNodesIndex: Integer;
    TraverseIntoChildren: boolean;
    ParentInfoForChildren: TTraversingInfo;
  begin
    Child.BeforeTraverse(StateStack);
    try
      TraverseIntoChildren := true;
      if Child is NodeClass then
        TraversingFunc(Child, StateStack, ParentInfo, TraverseIntoChildren);

      Child.MiddleTraverse(StateStack);

      if TraverseIntoChildren then
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

    finally Child.AfterTraverse(StateStack) end;

    if Assigned(TraversingAfterFunc) and
       (Child is NodeClass) then
      TraversingAfterFunc(Child, StateStack, ParentInfo);

    LastNodesIndex := Child.TraverseStateLastNodesIndex;
    if LastNodesIndex <> -1 then
      StateStack.Top.FLastNodes.Nodes[LastNodesIndex] := Child;
  end;

procedure TVRMLNode.TraverseInternal(StateStack: TVRMLGraphTraverseStateStack;
  NodeClass: TVRMLNodeClass;
  TraversingFunc: TTraversingFunc;
  TraversingAfterFunc: TTraversingAfterFunc;
  ParentInfo: PTraversingInfo);
var
  Enumerator: TTraverseEnumerator;
begin
  Enumerator := TTraverseEnumerator.Create;
  try
    Enumerator.StateStack := StateStack;
    Enumerator.NodeClass := NodeClass;
    Enumerator.TraversingFunc := TraversingFunc;
    Enumerator.TraversingAfterFunc := TraversingAfterFunc;
    Enumerator.ParentInfo := ParentInfo;
    Enumerator.EnumerateChildrenFunction(nil, Self);

    { Check that, if all went without exception, Enumerator.ParentInfo
      returned to original state. }
    Assert(Enumerator.ParentInfo = ParentInfo);
  finally FreeAndNil(Enumerator) end;
end;

var
  TraverseSingleStack: TVRMLGraphTraverseStateStack;

procedure TVRMLNode.Traverse(
  NodeClass: TVRMLNodeClass;
  TraversingFunc: TTraversingFunc;
  TraversingAfterFunc: TTraversingAfterFunc);
begin
  TraverseSingleStack.PushClear;
  try
    TraverseInternal(TraverseSingleStack, NodeClass,
      TraversingFunc, TraversingAfterFunc, nil);
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

constructor TVRMLNode.CreateParse(const ANodeName: string; Lexer: TVRMLLexer; Names: TVRMLNames);
begin
  Create(ANodeName, '');
  Parse(Lexer, Names);
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
  RemoveAllChildren;

  { In classic VRML encoding, CDATA never exists. }
  CDataExists := false;
  CData := '';

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
      if ParsingAllowedChildren then
      begin
        ChildNode := ParseNode(Lexer, Names, false);
        ChildNode.PositionInParent := Position;
        AddChild(ChildNode);
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
        if Lexer.VRMLVerMajor < 2 then
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
      if (Fields[I] is TSFString) and (Lexer.VRMLVerMajor <= 1) then
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
        Fields[Index].PositionInParent := Position;
        Inc(Position);
      end else
        VRMLWarning(vwSerious, 'Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.Name + '" in node "' + NodeTypeName + '"');
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
              VRMLWarning(vwSerious, 'X3D XML: specified <field> inside node, but this node doesn''t allow interface declaration with such accessType');
            end;
          except
            FreeAndNil(IDecl);
            raise;
          end;
        end else
        begin
          Child := ParseXMLNode(I.Current, ContainerField, Names, true);
          if Child <> nil then
          begin
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
                VRMLWarning(vwSerious, 'X3D XML: containerField indicated unknown field name ("' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + NodeTypeName + '"), using the default containerField value "' + Child.DefaultContainerField + '" succeded');
            end;

            if FieldIndex >= 0 then
            begin
              if Fields[FieldIndex] is TSFNode then
              begin
                SF := Fields[FieldIndex] as TSFNode;
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
                MF.AddItem(Child);
                MF.WarningIfChildNotAllowed(Child);
              end else
              begin
                Child.FreeIfUnused;
                Child := nil;
                VRMLWarning(vwSerious, 'X3D field "' + ContainerField + '" is not SFNode or MFNode, but a node value (XML element) is specified');
              end;
            end else
            begin
              try
                VRMLWarning(vwSerious, 'Unknown X3D field name (indicated by containerField value) "' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + NodeTypeName + '"');
              finally
                Child.FreeIfUnused;
                Child := nil;
              end;
            end;
          end;
        end;
        Inc(Position);
      end;
    finally FreeAndNil(I) end;
  end;

  procedure ParseXMLCdata;
  var
    I: TXMLCDataIterator;
  begin
    CDataExists := false;
    CData := '';

    I := TXMLCDataIterator.Create(Element);
    try
      if I.GetNext then
      begin
        CDataExists := true;
        if not CDataAllowed then
          VRMLWarning(vwSerious, Format('VRML / X3D node %s doesn''t allow CDATA section, but it''s specified',
            [NodeTypeName]));
        { append all CData sections to CData }
        repeat
          CData := CData + I.Current;
        until not I.GetNext;
      end;
    finally FreeAndNil(I) end;
  end;

begin
  Position := 0;
  { The order below is important: first parse XML attributes,
    then elements, since VRML DEF mechanism says that DEF order
    is significant. }
  ParseXMLAttributes;
  ParseXMLChildrenNodes;
  ParseXMLCdata;

  ParseAfter(Names);
end;

procedure TVRMLNode.ParseAfter(Names: TVRMLNames);
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
      PAverageScaleTransform: PSingle;
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
    PAverageScaleTransform^ := StateStack.Top.AverageScaleTransform;
    raise BreakTryFindNodeState.Create;
  end;

function TVRMLNode.TryFindNodeTransform(
  NodeClass: TVRMLNodeClass;
  out Node: TVRMLNode;
  out Transform: TMatrix4Single;
  out AverageScaleTransform: Single): boolean;
var
  Obj: TTryFindNodeTransformObj;
begin
  Obj := TTryFindNodeTransformObj.Create;
  try
    try
      Obj.PNode := @Node;
      Obj.PTransform := @Transform;
      Obj.PAverageScaleTransform := @AverageScaleTransform;
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

    for I := 0 to ParentNodesCount - 1 do
    begin
      result := ParentNodes[I].TryFindParentByName(FindName);
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
  for I := 0 to ParentNodesCount - 1 do
  begin
    Result := ParentNodes[I];
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
    for i := 0 to ParentNodesCount - 1 do
      if ParentNodes[i].HasParent(Node) then Exit(true);

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

procedure TVRMLNode.SaveContentsToStream(SaveProperties: TVRMLSaveToStreamProperties;
  NodeNames: TVRMLNodeNames);
var
  I: integer;
  FileItems: TVRMLFileItemsList;
begin
  FileItems := TVRMLFileItemsList.Create;
  try
    if HasInterfaceDeclarations <> [] then
    begin
      for I := 0 to InterfaceDeclarations.Count - 1 do
        FileItems.Add(InterfaceDeclarations[I]);
    end;

    for I := 0 to Prototypes.Count - 1 do
      FileItems.Add(Prototypes[I]);

    for I := 0 to Fields.Count - 1 do
    begin
      { Saving InterfaceDeclarations already handled saving fields
        with ParentInterfaceDeclaration <> nil, so no need to save them again. }
      if Fields[I].ParentInterfaceDeclaration = nil then
        FileItems.Add(Fields[I]);

      if Fields[I].Exposed then
      begin
        { exposed events may have their own IS clauses, save them }
        FileItems.Add(Fields[I].EventIn);
        FileItems.Add(Fields[I].EventOut);
      end;
    end;

    if ChildrenSaveToStream then
      for I := 0 to ChildrenCount - 1 do
        FileItems.Add(Children[I]);

    for I := 0 to Events.Count - 1 do
      if { Saving InterfaceDeclarations already handled saving events
           with ParentInterfaceDeclaration <> nil, so no need to save them again. }
         (Events[I].ParentInterfaceDeclaration = nil) then
        FileItems.Add(Events[I]);

    for I := 0 to Routes.Count - 1 do
      FileItems.Add(Routes[I]);

    for I := 0 to ImportsList.Count - 1 do
      FileItems.Add(ImportsList[I]);

    for I := 0 to ExportsList.Count - 1 do
      FileItems.Add(ExportsList[I]);

    FileItems.SaveToStream(SaveProperties, NodeNames);
  finally FreeAndNil(FileItems) end;
end;

procedure TVRMLNode.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
begin
  if PrototypeInstance and
     { TVRMLPrototypeNode has somewhat different meaning of PrototypeInstance,
       we want to save it directly (otherwise
       PrototypeInstanceSourceNode.SaveToStream could cause another
       recursive PrototypeInstanceSourceNode.SaveToStream with
       nested proto). For example test read + save
       kambi_vrml_test_suite/x3d/key_sensor.x3dv, to see that check
       below is needed. }
     not (Self is TVRMLPrototypeNode) then
  begin
    { If this is an expanded prototype, than delegate writing to the
      PrototypeInstanceSourceNode. }
    PrototypeInstanceSourceNode.SaveToStream(SaveProperties, NodeNames);

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

    (NodeNames as TVRMLNodeNames).Bind(Self);
  end else
  if (NodeNames as TVRMLNodeNames).Bound(Self) then
  begin
    SaveProperties.WritelnIndent('USE ' + NodeName);
  end else
  begin
    { write us to stream }
    SaveProperties.WriteIndent('');
    if NodeName <> '' then SaveProperties.Write('DEF ' +NodeName +' ');
    SaveProperties.Writeln(NodeTypeName +' {');

    SaveProperties.IncIndent;
    SaveContentsToStream(SaveProperties, NodeNames as TVRMLNodeNames);
    SaveProperties.DecIndent;

    SaveProperties.WritelnIndent('}');

    { update NodeNames.

      TODO: same problem here as when reading VRML file.
      We call Bind after writing node contents, because
      we assume there are no cycles... but in case of Script nodes,
      cycles are unfortunately possible. }
    (NodeNames as TVRMLNodeNames).Bind(Self);
  end;
end;

class function TVRMLNode.TraverseStateLastNodesIndex: Integer;
{ zwraca indeks do tablicy TraverseStateLastNodesClasses taki ze element
  na tej pozycji = NodeClass. Zwraca -1 jesli nie znalazl. }
begin
 { jestesmy w metodzie klasy, wiec Self to klasa (dlatego ponizej mozemy
   porownywac TraverseStateLastNodesClasses[result] z Self) }
 for result := 0 to HighTraverseStateLastNodes do
  if TraverseStateLastNodesClasses[result] = Self then Exit;
 result := -1;
end;

class function TVRMLNode.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
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

function TVRMLNode.ChildrenField: TMFNode;
begin
  Result := nil;
end;

procedure TVRMLNode.SmartAddChild(Node: TVRMLNode);
begin
  if ChildrenField = nil then
    AddChild(Node) else
    ChildrenField.AddItem(Node);
end;

function TVRMLNode.GetSmartChildren(Index: Integer): TVRMLNode;
begin
  if ChildrenField = nil then
    Result := Children[Index] else
    Result := ChildrenField.Items[Index];
end;

function TVRMLNode.SmartChildrenCount: integer;
begin
  if ChildrenField = nil then
    Result := ChildrenCount else
    Result := ChildrenField.Items.Count;
end;

function TVRMLNode.SmartExtractChild(Index: Integer): TVRMLNode;
begin
  if ChildrenField = nil then
    Result := ExtractChild(Index) else
    Result := ChildrenField.ExtractItem(Index);
end;

function TVRMLNode.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
  NewResult: boolean;
  NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer;
  ChildIndex: Integer;
begin
  Result := false;

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
      begin
        NewResult := SF.Value.SuggestedVRMLVersion(
          NewVerMajor, NewVerMinor, NewSuggestionPriority);
        AndSuggestedVRMLVersion(
          Result, VerMajor, VerMinor, SuggestionPriority,
          NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
      end;
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
      for J := 0 to MF.Items.Count - 1 do
      begin
        NewResult := MF.Items[J].SuggestedVRMLVersion(
          NewVerMajor, NewVerMinor, NewSuggestionPriority);
        AndSuggestedVRMLVersion(
          Result, VerMajor, VerMinor, SuggestionPriority,
          NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
      end;
    end;
  end;

  for ChildIndex := 0 to ChildrenCount - 1 do
  begin
    NewResult :=  Children[ChildIndex].SuggestedVRMLVersion(
      NewVerMajor, NewVerMinor, NewSuggestionPriority);
    AndSuggestedVRMLVersion(
      Result, VerMajor, VerMinor, SuggestionPriority,
      NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
  end;
end;

procedure TVRMLNode.AndSuggestedVRMLVersion(
  var Result: boolean;
  var VerMajor, VerMinor, SuggestionPriority: Integer;
  const NewResult: boolean;
  const NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer);
begin
  if NewResult then
  begin
    if (not Result) or (NewSuggestionPriority >= SuggestionPriority) then
    begin
      VerMajor := NewVerMajor;
      VerMinor := NewVerMinor;
      SuggestionPriority := NewSuggestionPriority;
      Result := true;
    end;
  end;
end;

class function TVRMLNode.ChildrenSaveToStream: boolean;
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
  while I < ChildrenCount do
  begin
    NewNode := Children[I];
    Func(Self, NewNode);
    if NewNode <> Children[I] then
    begin
      RemoveChild(I);
      Inc(Result);
      if NewNode <> nil then
      begin
        AddChild(I, NewNode);
        Inc(I);
      end;
    end else
    begin
      Result += Children[I].EnumerateReplaceChildren(Func);
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
          MF.RemoveItem(J);
          Inc(Result);
          if NewNode <> nil then
          begin
            MF.AddItem(J, NewNode);
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

type
  TBlenderObjectsTraverser = class
    TraversingFunc: TBlenderTraversingFunc;
    procedure Traverse(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

procedure TBlenderObjectsTraverser.Traverse(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
var
  GeometryNode: TVRMLGeometryNode absolute Node;
  BlenderObjectNode: TVRMLNode;
  BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode;
  BlenderMeshName: string;
begin
  if GeometryNode is TVRMLGeometryNode_1 then
  begin
    { Shape node generated by Blender VRML 1.0 exporter should have
      one parent, and this is his mesh. This mesh may have may
      parents, and these are his objects. }
    if ParentInfo <> nil then
    begin
      BlenderMeshNode := ParentInfo^.Node;
      BlenderMeshName := BlenderMeshNode.NodeName;

      ParentInfo := ParentInfo^.ParentInfo;

      if ParentInfo <> nil then
      begin
        BlenderObjectNode := ParentInfo^.Node;
        { Unfortunately, this will always be ''. Blender VRML 1.0 exporter
          doesn't write this. }
        BlenderObjectName := BlenderObjectNode.NodeName;
        TraversingFunc(BlenderObjectNode, BlenderObjectName,
          BlenderMeshNode, BlenderMeshName, GeometryNode, StateStack);
      end;
    end;
  end else
  if (StateStack.Top.ParentShape <> nil) and (ParentInfo <> nil) then
  begin
    { For VRML 2.0 exporter, the situation is actually quite similar, but
      we have to remove ME_ and OB_ prefixes from node names.
      Oh, and VRML 2.0 exporter actually does write object names.

      Initially we do ParentInfo := ParentInfo^.Parent,
      since we want to start from parent Shape node.
      That's how VRML 2.0 Blender exporter writes. }

    ParentInfo := ParentInfo^.ParentInfo;

    if ParentInfo <> nil then
    begin
      BlenderMeshNode := ParentInfo^.Node;
      BlenderMeshName := PrefixRemove('ME_', BlenderMeshNode.NodeName, false);

      ParentInfo := ParentInfo^.ParentInfo;

      if ParentInfo <> nil then
      begin
        BlenderObjectNode := ParentInfo^.Node;
        BlenderObjectName := PrefixRemove('OB_', BlenderObjectNode.NodeName, false);
        TraversingFunc(BlenderObjectNode, BlenderObjectName,
          BlenderMeshNode, BlenderMeshName, GeometryNode, StateStack);
      end;
    end;
  end;
end;

procedure TVRMLNode.TraverseBlenderObjects(
  TraversingFunc: TBlenderTraversingFunc);
var
  Traverser: TBlenderObjectsTraverser;
begin
  Traverser := TBlenderObjectsTraverser.Create;
  try
    Traverser.TraversingFunc := TraversingFunc;
    Traverse(TVRMLGeometryNode, @Traverser.Traverse);
  finally FreeAndNil(Traverser) end;
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
      MF.ReplaceItem(J, CopyState.DeepCopy(MF.Items[J]));
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

    for I := 0 to ChildrenCount - 1 do
      Result.AddChild(CopyState.DeepCopy(Children[I]));

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

    Result.CDataAllowed := CDataAllowed;
    Result.CDataExists := CDataExists;
    Result.CData := CData;

    Result.DefaultContainerField := DefaultContainerField;
    Result.ExplicitContainerField := ExplicitContainerField;
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

{ TVRMLNodesList ------------------------------------------------------------- }

function TVRMLNodesList.FindNodeName(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].NodeName = Name then
      Exit;
  Result := -1;
end;

procedure VRMLNodesList_FreeWithNonParentedContentsAndNil(var List: TVRMLNodesList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      if List.Items[I].ParentNodesCount + List.Items[I].ParentFieldsCount = 0 then
        List.Items[I].Free;
    FreeAndNil(List);
  end;
end;

{$endif read_implementation}
