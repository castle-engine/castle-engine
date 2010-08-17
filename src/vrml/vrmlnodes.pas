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

(*
  @abstract(Define all VRML / X3D nodes, along with other bulding blocks
  of VRML / X3D (prototypes, routes etc.).)

  This is the most important unit for VRML processing,
  as VRML file is basically just a nodes' graph.
  In fact, we represent VRML file as just one root node
  (if necessary, we will wrap actual file nodes within
  one "artificial" node, see TVRMLRootNode_1 or TVRMLRootNode_2).

  The chapter "Reading, writing, processing VRML scene graph"
  in the documentation on
  [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.scene_graph.html]
  is almost completely devoted to documenting the design of this single unit.

  Nodes can be loaded/saved from stream in "classic" encoding
  (the only encoding available before X3D), using internally
  the lexer (in VRMLLexer unit) and parser of VRML fields
  (in VRMLFields unit).

  For VRML nodes' graph processing, this unit offers a lot TVRMLNode
  methods. See e.g. TVRMLNode.Traverse, TVRMLNode.EnumerateNodes and
  TVRMLNode.FindNode. Traverse is especially important, since it
  walks through VRML graph just as VRML specification says
  (e.g. visiting only one child from a Switch node's children),
  gathering some state (useful especially for VRML 1.0, but also
  used for various things in later VRML versions).
  When you want to e.g. render VRML graph, you can just traverse
  the graph and render each pair of State with a geometry node
  (some TVRMLGeometryNode instance).
  (Alternatively, simple renderer can also use TVRMLGeometryNode.Triangulate).

  Many node classes have also specific routines useful for manipulating
  them (for example loading the contents of various Inline and Texture nodes,
  extracting transformation from nodes that can change it etc.).
  TVRMLGeometryNode in particular has some useful routines
  for calculating bounding box, vertex/triangles counts and
  to triangulate the node.

  This unit doesn't depend on OpenGL, or any other particular rendering
  method. So it's suitable also for VRMLRayTracer, and every other possible
  renderer that will ever get implemented.

  VRML node classes names, and inheritance:

  @unorderedList(
    @item(Pascal classes named like TNodeXxx are @bold(official) node classes,
      that is they implement VRML / X3D specification node Xxx.
      This also applies to some extension nodes (for example,
      http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php).
      The important fact is that these node names are somehow available to
      the user, required by some specification etc.

      X3D abstract node classes also fall into this category (they are
      specified in X3D specification; although nothing actually forces us
      to implement these abstract X3D classes, and VRML author cannot directly use
      them, I decided it's a proper way to define these classes.).

      They all descend from TVRMLNode. For example, TNodeIndexedFaceSet.

      There are also Pascal interfaces, like INodeXxx that represent
      VRML node Xxx interface, more on this below. They all descend from IVRMLNode.

      The second group are @bold(unofficial) node classes. These are internal
      to our engine, help our implementation, and often may help
      a programmer to process some VRML files --- but they are not visible
      to final VRML user/author. They are named TVRMLXxxNode,
      like TVRMLGeometryNode, TVRMLTextureNode, TVRMLLightNode, TVRMLUnknownNode.

      So you can immediately tell which nodes come from
      VRML / X3D specification and which nodes are only added by our engine
      to simplify implementation.

      Before X3D, adding my own TVRMLXxxNode classes was the only way to
      add some interesting inheritance among VRML nodes.
      This allows to reuse implementation, and also simplifies some processing:
      for example, you can search the scene for any TVRMLLightNode, this
      way looking for all PointLight, SpotLight etc. nodes.)

    @item(
      Optional suffix _1 or _2 or _3 after the node class name indicates that
      this node class is used only for given major VRML version.
      _1 means VRML 1.0, _2 means VRML 2.0 (aka 97), _3 means VRML 3.0 (aka X3D).
      Also _2 sometimes mean both VRML 2.0 and 3.0.

      This suffix is used when there are nodes with the same name in VRML
      specifications, but they have to be handled by different Pascal classes
      since they are incompatible.

      For example, we have TNodeIndexedFaceSet_1 for VRML 1.0 and
      TNodeIndexedFaceSet_2 for VRML >= 2.0 (97, X3D).)

    @item(
      Since X3D introduced abstract node classes, and defines node
      inheritance, we follow this by actually defining in our engine
      all the X3D abstract classes and using them. Inheritance of our
      Pascal classes reflects inheritance of nodes in X3D specification.

      Many X3D nodes are declared as both Pascal interface and class.
      That's because we preserve X3D inheritance graph, and when node
      descends from more than one VRML class --- the rest of VRML classes
      has to be expressed as Pascal interfaces. For now, these interfaces
      have little use beside simple "is" checking, but they may be more
      useful in the future.)
  )

  As for VRML versions handling:

  @unorderedList(
    @item(
      We handle VRML 1.0, VRML 2.0 (aka 97) and X3D (aka VRML 3.x).

      Every correct VRML / X3D file in classic and XML encoding should be parsed
      by this unit.
      See [http://vrmlengine.sourceforge.net/vrml_implementation_status.php]
      for much more detailed information about supported features.)

    @item(
      Also many Inventor 1.0 files are correctly parsed.
      We handle Inventor 1.0 mostly like VRML 1.0, also some small
      things and nodes specific for Inventor 1.0 are implemented here, see
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_iv_in_vrml].)

    @item(
      Note that structures in this unit are @italic(not) focused
      on either VRML 1.0 or VRML >= 2.0. On the contrary: we try to handle
      the @italic(sum of all VRML and X3D). When reading VRML 1.0,
      many VRML 2.0 constructs (that not conflict with anything in VRML 1.0)
      are allowed, and the other way around too.

      Internally, we do not convert VRML 1.0-specific constructs
      to VRML 2.0 constructs (or the other way around).
      See [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.vrml_1_2_sum.html]
      for more in-depth explanation of how, and why, we handle both
      old-style (Inventor, VRML 1.0) and new-style (VRML 2.0, X3D) VRML
      syntax.)
  )

  Note that when reading VRML files, we generally do not change the
  VRML graph. So we're able to save exactly the same VRML graph
  back to another file. See also
  [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/ch02s04.html#id2671890].
  This makes an excellent opportunity for writing various VRML
  processing tools, that can simply read the file, change whatever
  they want, and write the file back --- knowing that the "untouched"
  parts of VRML graph are preserved perfectly.

  Your own units can define new VRML/X3D nodes, by simply descending
  from TVRMLNode (or other, more specialized, descendant).
  This is possible thanks to NodesManager --- you should "register"
  your new classes there, to be able to parse them.

  Examples of defining your own VRML node types (without modifying
  sources of this unit, or any other unit) are in these programs:
  - bezier_curves (VRMLBezierCurve)
  - malfunction (unit LevelUnit) defined various nodes like
    MalfunctionLevelInfo. This way "malfunction" specific data
    is kept inside VRML/X3D nodes graph, it's parsed by our VRML/X3D parser
    and such.

  TODO:
  - Eventually, moving all non-abstract nodes out of this unit
    could be nice. But
    1. for now, stuff that is inside TVRMLGraphTraverseState.LastNodes
       must be defined in this unit
    2. I don't know if this is really good for users --- it adds
       another unit to your uses clause.

  Co do zapisywania VRMLa :
  - kazde pole zapisuje 1 lub wiecej calych linii
  - kazdy node zapisuje najpierw linie [DEF NodeName] NodeKindName {
    potem swoje pola
    potem swoje subnode'y
    potem linie }
    zmieniajac Indent o IndentIncrement zdefiniowane w VRMLFields
  - linie sa konczone przez nl
  - mechanizm DEF/USE pol jest zapisywany dobrze, tzn. jezeli
    jakies pole jest obecne wiecej niz raz w drzewie VRML'a to jest
    zapisywane do pliku tylko raz, kazdy nastepny zapis to tylko
    zapisanie 'USE <NodeName>'
  - w wiekszosci przypadkow pola o wartosciach domyslnych nie sa zapisywane.
    W zasadzie zapisywanie pol o wartosciach domyslnych nie byloby bledem,
    choc mogloby denerwowac userow. Ale my naprawde tego potrzebujemy ze
    wzgledu na male rozszerzenia VRMLa 1.0 jakie tu zaimplementowalem
    (patrz wyzej). Jesli chcemy zapisywac poprawne pliki VRMLa to nie mozemy
    dopuszczac zeby np. eksport pliku 3DS na VRMLa dodawal jakies pole
    "mirror" do kazdego Materialu. Wiec nie bedziemy zapisywac "mirror" kiedy
    "mirror" = [0.0] i w ten sposob rozwiazujemy problem. Jednoczesnie,
    jesli ktos rzeczywiscie stworzyl plik podajac wlasciwosc "mirror"
    rozna od domyslnej to uwzglednimy wszedzie to pole i zapiszemy je w
    razie potrzeby z powrotem do pliku VRMLa.

  Files organization: X3D nodes are inside x3d_COMPONET_NAME.inc files.
  That's a nice thing, since X3D components provide a natural way to group
  the vast number of nodes into files. Some remaining nodes that are not part
  of X3D are in other vrmlnodes_xxx.inc files, for example
  vrmlnodes_1.inc contains only VRML-1.0 specific nodes.
*)

unit VRMLNodes;

{$I kambiconf.inc}

interface

uses VectorMath, Classes, SysUtils, VRMLLexer, KambiUtils, KambiClassUtils,
  VRMLFields, Boxes3D, Images, TTFontsTypes, BackgroundBase, VRMLErrors,
  Videos, VRMLTime, Base3D,
  KambiScript, VRMLKambiScript, KambiOctree, DDS, TextureImages,
  KambiXMLRead, DOM;

{$define read_interface}

const
  DefaultMaterial_1AmbientColor: TVector3Single = (0.2, 0.2, 0.2);
  DefaultMaterial_2AmbientIntensity = 0.2;
  DefaultMaterialDiffuseColor: TVector3Single = (0.8, 0.8, 0.8);
  DefaultMaterialSpecularColor: TVector3Single = (0, 0, 0);
  DefaultMaterialEmissiveColor: TVector3Single = (0, 0, 0);
  DefaultMaterialShininess = 0.2;
  DefaultMaterialTransparency = 0.0;
  DefaultMaterialMirror = 0.0;
  DefaultMaterialReflSpecularExp = 1000000;
  DefaultMaterialTransSpecularExp = 1000000;

{ -----------------------------------------------------------------------------
  dluuuga deklaracja "type" w ktorej wiele rzeczy jest zdefiniowanych
  wzajemnie (rekurencyjnie). }

type
  { forward declarations } { }
  TVRMLNodesList = class;
  TVRMLLightNodesList = class;
  TVRMLNode = class;
  TNodeCoordinate3 = class;
  TNodeShapeHints = class;
  TNodeFontStyle_1 = class;
  TNodeMaterial_1 = class;
  TNodeMaterialBinding = class;
  TNodeNormal = class;
  TNodeNormalBinding = class;
  TNodeTexture2 = class;
  TNodeTextureCoordinate2 = class;
  TVRMLGeometryNode = class;
  TVRMLLightNode = class;
  TNodeKambiTriangulation = class;
  TNodeX3DShapeNode = class;
  TVRMLTextureNode = class;
  TNodeBlendMode = class;
  TNodeX3DTextureNode = class;
  TVRMLEventsEngine = class;

  TVRMLNodeClass = class of TVRMLNode;

  TVRMLNodeProc = procedure (node: TVRMLNode) of object;

  TVRML1StateNode =
  (
    vsCoordinate3,
    vsShapeHints,
    vsFontStyle,
    vsMaterial,
    vsMaterialBinding,
    vsNormal,
    vsNormalBinding,
    vsTexture2,
    vsTextureCoordinate2,
    vsKambiTriangulation
  );

  { Nodes that wiill be saved inside TVRMLGraphTraverseState.LastNodes.
    These are nodes that affect how following nodes are rendered,
    mostly for VRML 1.0 "state". }
  TTraverseStateLastNodes = record
    case Integer of
      0: ( Nodes: array [TVRML1StateNode] of TVRMLNode; );
      1: ( Coordinate3 :TNodeCoordinate3;
           ShapeHints :TNodeShapeHints;
           FontStyle :TNodeFontStyle_1;
           Material :TNodeMaterial_1;
           MaterialBinding :TNodeMaterialBinding;
           Normal :TNodeNormal;
           NormalBinding :TNodeNormalBinding;
           Texture2 :TNodeTexture2;
           TextureCoordinate2 :TNodeTextureCoordinate2;
           KambiTriangulation: TNodeKambiTriangulation;
           { additions here must be synchronized with additions to
             TVRML1StateNode }
         );
  end;

  { Keep track of a used light.
    When the light is used, not only it's node is important (in LightNode),
    but also current transformation (this transformation affects things
    like actual position and direction). This record keeps it all.

    Moreover, it provides precalculated properties like TransfLocation
    and TransfNormDirection. Their computation too often would be somewhat
    expensive, and thanks to this class we can calculate them just once
    when they are found by traversing VRML file.

    @bold(This record may be initialized only by
    TVRMLLightNode.CreateActiveLight.
    Update (when transform changes) by TVRMLLightNode.UpdateActiveLight.) }
  TActiveLight = record
    LightNode: TVRMLLightNode;

    Transform: TMatrix4Single;
    AverageScaleTransform: Single;

    { TransfLocation to juz przeliczone Transformation*Location dla swiatel
      TVRMLPositionalLightNode. }
    TransfLocation: TVector3Single;

    { TransfNormDirection to juz
      znormalizowane i transformowane Direction dla swiatel Directional i Spot
      (jest transformowane tak zeby przesuniecia nie mialy znaczenia,
      licza sie tylko obroty i skalowania). }
    TransfNormDirection: TVector3Single;

    { For VRML 2.0 positional lights, this is precalculated node's radius
      scaled by Transform. }
    TransfRadius: Single;
  end;
  PActiveLight = ^TActiveLight;

  TDynArrayItem_1 = TActiveLight;
  PDynArrayItem_1 = PActiveLight;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynActiveLightArray = class(TDynArray_1)
  public
    { -1 jesli nie ma }
    function IndexOfLightNode(LightNode: TVRMLLightNode): integer;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
  end;
  TArray_ActiveLight = TInfiniteArray_1;
  PArray_ActiveLight = PInfiniteArray_1;

  TPointingDeviceSensorsList = class;

  { This describes current "state" (current transformation and such)
    when traversing VRML graph.

    For VRML >= 2.0 this could be simpler, as VRML >= 2.0 doesn't need
    to keep track for example of the @link(LastNodes).
    But we want to still handle VRML 1.0, 100% correctly, so here we are:
    this class contains whole state needed for any VRML/X3D version. }
  TVRMLGraphTraverseState = class
  private
    FLastNodes: TTraverseStateLastNodes;
    FOwnedLastNodes: array [TVRML1StateNode] of boolean;
    procedure CommonCreate;

    { Sets FLastNodes to NewLostNodes.

      During doing this, old FLastNodes are freed if they were owned.
      New nodes are not owned.

      This takes care of checking for each TVRML1StateNode
      if the old node is equal to new one. If yes, then the node
      if not freed (regardless of "owned" status), and the "owned"
      status is left as-is (not chaned to false).
      This way calling thing like @code(AssignLastNodes(FLastNodes)),
      is a valid harmless operation. }
    procedure AssignLastNodes(const NewLostNodes: TTraverseStateLastNodes);
  public
    { Nodes that are saved during VRML/X3D traversing.
      These nodes affect some following nodes in the graph,
      mostly for VRML 1.0.

      They are never @nil (traversing code must always take care to initialize
      them to default nodes at the beginning).

      Note that TVRMLGraphTraverseState instance doesn't have to own
      these nodes (doesn't free them, and doesn't track of when they are
      freed). E.g. nodes' TVRMLNode.ParentsCount doesn't take into account
      that they are owned by this state.
      Although for some tricks (but not during normal VRML/X3D traversing)
      some nodes are owned, by using SetLastNodes with OwnNode = @true.

      For nodes that are within TraverseStateLastNodesClasses
      (and thus are stored inside LastNodes): it's guaranteed
      they don't affect the state (of this class) during traversing
      (that is, they don't do anything special in
      TVRMLNode.BeforeTraverse / TVRMLNode.MiddleTraverse / TVRMLNode.AfterTraverse).
      So it's guaranteed that changing some field's value of a node
      within TraverseStateLastNodesClasses affects @italic(only)
      the shapes that have given node inside State.LastNodes.
      TVRMLScene.ChangedFields depends on that. }
    property LastNodes: TTraverseStateLastNodes read FLastNodes;

    procedure SetLastNodes(const StateNode: TVRML1StateNode;
      const Node: TVRMLNode; const OwnNode: boolean);
  public
    { Lights active in this state, two separate versions for each VRML flavor
      needed here.

      VRML2ActiveLights should be for any VRML >= 2 (including X3D),
      VRML1ActiveLights should be for any VRML <= 1 (including Inventor).

      VRML 2.0 may have more lights active
      (since DirectionalLight affects all siblings, and other lights affect
      potentially all). But VRML 2.0 may also have some lights less,
      since some lights are limited by radius and may be determined to
      not light here.

      Also, note that VRML2ActiveLights cannot be fully calculated in Traverse
      pass (contrary to everything else in TVRMLGraphTraverseState).
      DirectionalLights are calculated here, but positional lights have
      to be calculated later (VRML 2 spec says that they affect whole scene,
      based on their radius, and regardless of their position in VRML graph;
      so this is not possible to fill during Traverse call).
      See how UpdateVRML2ActiveLights in TVRMLScene does this.

      It's guaranteed that VRML/X3D lights change only these
      *ActiveLights properties during traversing, not anything else.
      TVRMLScene.ChangedFields (may) depend on that. }
    VRML1ActiveLights, VRML2ActiveLights: TDynActiveLightArray;

    { This returns VRML1ActiveLights or VRML2ActiveLights, based on VRML
      flavor used to render with this state.

      More precisely, it checks "VRML flavor" by looking at ShapeNode:
      when ShapeNode is @nil, we're in VRML 1 mode, otherwise in VRML 2 mode. }
    function CurrentActiveLights: TDynActiveLightArray;

  public
    { Current transformation. }
    Transform: TMatrix4Single;

    { Inverted @link(Transform) matrix. This matrix is crucial for some
      special effects (for example, it's needed for calculating in tangent space
      for bump mapping).

      This is not calculated using any complex matrix inversion algorithms.
      Instead, this is calculated by updating this along the way, just like
      we calculate normal @link(Transform). So it's simple, quick, and
      guaranteed to be correct @italic(assuming that user didn't use
      VRML 1.0 MatrixTransform along the way). That's why MatrixTransform node
      was removed from VRML 2.0, it breaks such things.

      Also, any scale with zero component along the way will make this
      partially invalid (we'll substitute identity in place of inverted scaling
      matrix). This is unavoidable, there's no reverse matrix for scaling
      with zero factor, since one resulting point may correpond to infinitely many
      source points (i.e., it's natural that such scaling function cannot be
      reversed). }
    InvertedTransform: TMatrix4Single;

    { This is a uniform scale caused by matrix Transform.

      I.e., if we could extract the scaling from Transform, then it would
      be AverageScaleTransform. Assuming that the scale is uniform.
      If we used any non-uniform scale along the way, this is the average scale.

      This is calculated by updating it along the way, just like Transform
      is updated. This way it's calculated easily --- contrary to the
      non-trivial operation of extracting a scale from a 4x4 matrix.
      It's also 100% correct, @italic(assuming that user didn't use
      VRML 1.0 MatrixTransform along the way). }
    AverageScaleTransform: Single;

    { Copy Transform from Source.Transform. Along with related fields,
      like InvertedTransform and AverageScaleTransform. }
    procedure AssignTransform(Source: TVRMLGraphTraverseState);

  public
    { Current texture transformation. Usable only for VRML 1.0, in VRML 2.0
      texture transformations don't accumulate like modelview transformations. }
    TextureTransform: TMatrix4Single;

    ShapeNode: TNodeX3DShapeNode;

    constructor CreateCopy(Source: TVRMLGraphTraverseState);
    constructor Create(const ADefaultLastNodes: TTraverseStateLastNodes); overload;

    { Standard create, uses global StateDefaultNodes as initial
      LastNodes state.

      Using a global StateDefaultNodes is useful, since this way
      the references to default VRML 1.0 state nodes are always
      valid (so you can e.g. quickly copy TVRMLGraphTraverseState,
      and free the previous TVRMLGraphTraverseState instance,
      and the node references stay the same).

      Also this improves caching in some cases, because sometimes
      we assume that nodes are equal only when their references are equal
      (e.g. TVRMLGraphTraverseState.Equals does this, and this is used
      by Shape caching in TVRMLOpenGLRendererContextCache). }
    constructor Create; overload;

    destructor Destroy; override;

    procedure Assign(Source: TVRMLGraphTraverseState);

    { Clear all state, just like this TVRMLGraphTraverseState instance
      would be just constructed. }
    procedure Clear;

    { Compare with other TVRMLGraphTraverseState instance.
      The idea is that two states are equal if, when applied to the same
      VRML node, they "mean" the same thing, i.e. produce the same shape,
      behavior etc. }
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { This is like @link(Equals) but it ignores some fields that are
      ignored when rendering using
      TVRMLOpenGLRenderer.RenderShapeNoTransform.
      For example, it ignores Transform, AverageScaleTransform, InvertedTransform. }
    function EqualsNoTransform(SecondValue: TVRMLGraphTraverseState): boolean;

    { Returns texture node that should be used for nodes within this State.
      Regardless of VRML/X3D version. May return multi-texture
      (TNodeMultiTexture), or normal 2D texture (TVRMLTextureNode),
      or some other TNodeX3DTextureNode descendant (cube map, 3d texture).

      Details:
      If ShapeNode <> nil, this returns texture node taken from
      ShapeNode.Texture (note that it may be nil, if Apperance
      of Appearance.Texture node is NULL in VRML).
      Otherwise it returns texture from LastNodes.Texture2. }
    function Texture: TNodeX3DTextureNode;

    { Returns BlendMode for this state, or @nil if not present.

      Details: if ShapeNode <> nil (VRML >= 2.0), and it's Appearance <> nil, and it's
      a KambiAppearance and it has BlendMode <> nil... then returns it.
      Otherwise @nil. }
    function BlendMode: TNodeBlendMode;

  public
    { Information if you're within any inline node or expanded prototype.
      InsideInline = 0 means you're not inside any inline node,
      1 means you're inside one inline, 2 means you're within content
      inlined from yet another inline node, and so on.
      Analogous for InsidePrototype.

      These are measured from the node where you
      started TVRMLNode.Traverse call, that is they assume that the initial
      node from where you're traversing is at level 0 (not inside inline
      or expanded prototype).

      These are useful to establish "run-time name scope" of X3D,
      see X3D spec 4.4.7 (needed e.g. when handling Anchor node with
      "#Viewpoint" URL).
      Interpreting this for our implementation,
      specification says that if you traverse
      from node X, then all traversed nodes with
      InsideInline = InsidePrototype = 0 are within the same name scope.

      Also this is useful for searching for the first bindable node after
      loading the file. Specification says to ignore inline content
      in this case (although prototype content is Ok in this case).

      When scriping will be implemented, probably analogous
      InsideScriptCreatedNode will also be needed, as the spec says
      that bindable nodes should not be searched within things like
      "Browser.createX3DFromString()".

      @groupBegin }
    InsideInline: Cardinal;
    InsidePrototype: Cardinal;
    { @groupEnd }

    { This is > 0 when traversing nodes that do not participate in
      collision detection.

      This counts how many times are we inside Collision node that
      prevents us from colliding.
      More precise, this is increased when we traverse inside
      Collision.children with Collision.enabled = FALSE or
      Collision.proxy <> NULL (since then Collision.children are
      not collidable). }
    InsideIgnoreCollision: Cardinal;

    { This is > 0 when traversing nodes that are not visible.

      This counts how many times are we inside Collision.proxy
      (with Collision.enabled = TRUE). Collision.proxy is never
      visible. }
    InsideInvisible: Cardinal;

    { Active pointing device sensors in this state.
      This can contain only nodes descending from
      X3DPointingDeviceSensorNode, and additionally an Anchor node.

      This list automatically honours VRML / X3D rules for what
      pointing device sensor is active: pointing device within some
      group node affects all children in this group node.
      (And when multiple pointing device sensors are within the same
      grouping node, they all work.) }
    PointingDeviceSensors: TPointingDeviceSensorsList;
  end;

  { Stack of TVRMLGraphTraverseState.

    Allows you for much faster
    creation/destruction of TVRMLGraphTraverseState instances.
    Although you can always construct / destruct TVRMLGraphTraverseState
    as normal objects, in some cases this is too slow: when traversing VRML graph
    (e.g. profile change_vrml_by_code_2 with ChangeField), merely
    creating/destroying TVRMLGraphTraverseState instances takes a noticeable
    amount of time.

    This stack allows you to do this faster, first of all by
    internally using a prepared pool of instances.

    Each PushClear call creates a clear state instance, and places
    it on the stack.
    Each Push call creates a copy of current top and places it on the stack.
    Each Pop removes and destroys the last instance added by Push.

    Naturally, you can call Push and Top only when the stack is not empty.
    In practice, using the stack always starts in TVRMLNode.Traverse,
    where we push initial clear state. So the stack passed to various
    callbacks, TVRMLNode.BeforeTraverse and such is always guaranteed non-empty.

    Note that for speed purposes all Traverse calls actually
    share a single stack. That is,
    to avoid creating TVRMLGraphTraverseStateStack instance each time
    (because even creating TVRMLGraphTraverseStateStack
    takes some time (as it prepares a pool of TVRMLGraphTraverseState
    instances, to allow fast push/pop)), TVRMLNode simply reuses a single
    global TVRMLGraphTraverseStateStack instance. This means that,
    if you execute Traverse while being inside other Traverse, you
    must first finish innermost Traverse before continuing with the outer. }
  TVRMLGraphTraverseStateStack = class
  private
    Items: array of TVRMLGraphTraverseState;
    ItemsAllocated: Cardinal;
    procedure GrowItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PushClear;
    procedure Push;
    procedure Pop;

    { Peek at the top of the stack. }
    function Top: TVRMLGraphTraverseState;
  end;

  PTraversingInfo = ^TTraversingInfo;
  TTraversingInfo = record
    Node: TVRMLNode;
    ParentInfo: PTraversingInfo;
  end;

  { Used as a callback by TVRMLNode.Traverse. }
  TTraversingFunc = procedure (Node: TVRMLNode;
    StateStack: TVRMLGraphTraverseStateStack;
    ParentInfo: PTraversingInfo;
    var TraverseIntoChildren: boolean) of object;

  TTraversingAfterFunc = procedure (Node: TVRMLNode;
    StateStack: TVRMLGraphTraverseStateStack;
    ParentInfo: PTraversingInfo) of object;

  { Used as a callback by TVRMLNode.TraverseBlenderObjects. }
  TBlenderTraversingFunc = procedure (
    BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
    BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
    GeometryNode: TVRMLGeometryNode;
    StateStack: TVRMLGraphTraverseStateStack) of object;

  TEnumerateChildrenFunction =
    procedure (Node, Child: TVRMLNode) of object;

  TEnumerateReplaceNodesFunction =
    procedure (ParentNode: TVRMLNode; var Node: TVRMLNode) of object;

  TNewTriangleProc = procedure (const Tri: TTriangle3Single;
    Shape: TObject; State: TVRMLGraphTraverseState;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer) of object;

  TSFNode = class;
  TMFNode = class;
  TVRMLPrototypeNode = class;
  TVRMLPrototypeBasesList = class;
  TVRMLRoutesList = class;
  TVRMLInterfaceDeclaration = class;
  TVRMLNames = class;
  TVRMLNodeNames = class;
  TVRMLPrototypeNames = class;

  TVRMLAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);
  TVRMLAccessTypes = set of TVRMLAccessType;

  TVRMLInterfaceDeclarationsList = class;

  TNodeDestructionNotification = procedure (Node: TVRMLNode) of object;

  TDynArrayItem_2 = TNodeDestructionNotification;
  PDynArrayItem_2 = ^TNodeDestructionNotification;
  {$define DYNARRAY_2_IS_FUNCTION}
  {$define DYNARRAY_2_IS_FUNCTION_METHOD}
  {$define DYNARRAY_2_USE_EQUALITY}
  {$I dynarray_2.inc}
  TDynNodeDestructionNotificationArray = class(TDynArray_2)
  public
    { This calls all functions (all Items) passing them Node parameter. }
    procedure ExecuteAll(Node: TVRMLNode);
  end;

{$I vrmlnodes_node.inc}

  TVRMLNodeClassesList = class(TList)
  private
    function GetItems(Index: Integer): TVRMLNodeClass;
    procedure SetItems(Index: Integer; Value: TVRMLNodeClass);
  public
    property Items[Index: Integer]: TVRMLNodeClass
      read GetItems write SetItems; default;
    procedure AssignArray(
      const AItemsArray: array of TVRMLNodeClass);
    function IndexOf(NodeClass: TVRMLNodeClass): Integer; overload;
    { This is equivalent to IndexOf(NodeClass.ClassType),
      taking care of necessary typecasts. }
    function IndexOf(Node: TVRMLNode): Integer; overload;

    { This looks for a node class that is ancestor of given Node,
      in other words that satisfies @code(Node is Items[Result]).
      So, comparing to IndexOf, this may also find something when
      IndexOf doesn't, since this doesn't require an exact
      match --- only "is" match.

      Returns -1 if not found. }
    function IndexOfAnyAncestor(Node: TVRMLNode): Integer;

    procedure Add(Value: TVRMLNodeClass);

    { Add all node classes registered in NodesManager that implement given
      interface Interf. }
    procedure AddRegisteredImplementing(Interf: TGUID);
  end;

  { SFNode VRML field.
    It's defined in this unit, not in VRMLFields, since it uses
    TVRMLNode definition. NULL value of the field is indicated by
    Value field = nil.

    Note that we store AllowedChildren list, which is a list of
    classes allowed as a Value (also nil is always allowed).
    But this is used only to produce warnings for a user.
    You should never assert that Value actually is one the requested
    classes. We want to keep here even not allowed items,
    because we want operation "read from VRML file + write to VRML file"
    to be as non-destructible as possible. So if user wrote
    invalid class hierarchy, we will output this invalid class hierarchy. }
  TSFNode = class(TVRMLSingleField)
  private
    FValue: TVRMLNode;
    FParentNode: TVRMLNode;
    FAllowedChildrenAll: boolean;
    FAllowedChildren: TVRMLNodeClassesList;
    procedure SetValue(AValue: TVRMLNode);
  private
    FDefaultValue: TVRMLNode;
    FDefaultValueExists: boolean;
    procedure SetDefaultValue(ADefaultValue: TVRMLNode);
    procedure SetDefaultValueExists(AValue: boolean);
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties;
      NodeNames: TObject); override;
  public
    constructor CreateUndefined(AParentNode: TVRMLFileItem;
      const AName: string); override;
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      const AnAllowedChildren: array of TVRMLNodeClass;
      AValue: TVRMLNode = nil); overload;
    { Constructor that takes AnAllowedChildren as TVRMNodeClassesList.
      Note that we copy the contents of AnAllowedChildren, not the
      reference. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AnAllowedChildren: TVRMLNodeClassesList;
      AValue: TVRMLNode = nil); overload;
    { Constructor that initializes AllowedChildren to all
      classes implementing AllowedChildrenInterface. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AllowedChildrenInterface: TGUID;
      AValue: TVRMLNode = nil); overload;
    destructor Destroy; override;

    { This says that all children are allowed, regardless of
      AllowedChildren value.

      CreateUndefined creates always object with
      AllowedChildrenAll = @true (otherwise AllowedChildren list is empty
      and nothing would be allowed; but for e.g. fields in instantiated
      prototypes, we must initially just allow all, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing).

      Other constructors set AllowedChildren
      to something meaningful and set this to @false. You can change this
      to @true then if you consciously want to turn this check off. }
    property AllowedChildrenAll: boolean
      read FAllowedChildrenAll write FAllowedChildrenAll;

    { DefaultValue of SFNode field.

      While X3D specification says for all SFNode fields that their
      default value is NULL, this is not necessarily true for PROTO
      SFNode fiels. So we have to take into account that any DefaultValue
      is possible.

      Note that this doesn't have to be @nil, but will be irrelevant
      if not DefaultValueExists. (Once I had an idea to automatically
      set DefaultValue to @nil when DefaultValueExists is set to @false,
      but this was uncomfortable (like "what to do when DefaultValue
      is assigned non-nil when DefaultValueExists is false?").)

      Freeing of this is automatically managed, just like the normal
      @link(Value) property. This means that you can simply set
      DefaultValue to @nil or some existing node, and eventual memory
      deallocation of previous DefaultValue node (if unused) will happen
      automatically. }
    property DefaultValue: TVRMLNode
      read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write SetDefaultValueExists default false;

    property Value: TVRMLNode read FValue write SetValue;
    procedure ParseValue(Lexer: TVRMLLexer; Names: TObject); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Names: TObject); override;
    procedure ParseXMLElement(Element: TDOMElement; Names: TObject); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;
    procedure AssignDefaultValueFromValue; override;

    { VRML node containing this field. May be @nil if unknown, in special
      cases.

      Note that this property is exactly the same as
      TVRMLFieldOrEvent.ParentNode,
      contains always the same value. But this is declared as TVRMLNode,
      so it's more comfortable. }
    property ParentNode: TVRMLNode read FParentNode;

    class function VRMLTypeName: string; override;

    { Checks is Child allowed as a value of thia SFNode,
      and makes VRMLWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      VRMLWarning message will suggest that this Child is used as value
      of this node. In other words, you should only pass as Child
      a node that you want to assign as Value to this field,
      otherwise VRMLWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TVRMLNode);

    function ChildAllowed(Child: TVRMLNode): boolean;
    function CurrentChildAllowed: boolean;

    { Calls Func for our @link(Value), assuming it's set (non-nil) and valid
      (allowed by ChildAllowed).

      The main use for this is to simplify implementation of
      @link(TVRMLNode.DirectEnumerateActive) overrides in TVRMLNode descendants. }
    procedure EnumerateValid(Func: TEnumerateChildrenFunction);
  end;

  { MFNode VRML field.

    Just like SFNode, it's defined in this unit, as it uses TVRMLNode.
    Note that items of MFNode @italic(cannot) be nil (i.e. VRML doesn't
    allow to use NULL inside MFNode), contrary to SFNode.

    Note that TMFNode implementation doesn't use TVRMLSimpleMultField.
    Reasons ? 1. We don't want to use TDynArray descendant.
    We want to use TVRMLNodesList. 2. We don't want to do parsing
    using SFNode, because MFNode doesn't allow NULL items.

    Just like for TSFNode:
    Note that we store AllowedChildren list, which is a list of
    classes allowed as Items.
    But this is used only to produce warnings for a user.
    You should never assert that every item actually is one the requested
    classes.  }
  TMFNode = class(TVRMLMultField)
  private
    FItems: TVRMLNodesList;
    FDefaultItems: TVRMLNodesList;
    FDefaultValueExists: boolean;
    FParentNode: TVRMLNode;
    FAllowedChildren: TVRMLNodeClassesList;
    FAllowedChildrenAll: boolean;
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties;
      NodeNames: TObject); override;
    { Get or set the number of items.
      When increasing this, remember that new items of TMFNode
      will be @nil, and generally you should initialize them to
      something else then @nil (VRML/X3D don't really allow
      NULL items inside MFNode fields).
      @groupBegin }
    function GetCount: Integer; override;
    procedure SetCount(const Value: Integer); override;
    { @groupEnd }
  public
    constructor CreateUndefined(AParentNode: TVRMLFileItem;
      const AName: string); override;
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      const AnAllowedChildren: array of TVRMLNodeClass); overload;
    { Constructor that takes AnAllowedChildren as TVRMNodeClassesList.
      Note that we copy the contents of AnAllowedChildren, not the
      reference. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AnAllowedChildren: TVRMLNodeClassesList); overload;
    { Constructor that initializes AllowedChildren to all
      classes implementing AllowedChildrenInterface. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AllowedChildrenInterface: TGUID); overload;
    destructor Destroy; override;

    { This says that all children are allowed, regardless of
      AllowedChildren value.

      CreateUndefined creates always object with
      AllowedChildrenAll = @true (otherwise AllowedChildren list is empty
      and nothing would be allowed; but for e.g. fields in instantiated
      prototypes, we must initially just allow all, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing).

      Other constructors set AllowedChildren
      to something meaningful and set this to @false. You can change this
      to @true then if you consciously want to turn this check off. }
    property AllowedChildrenAll: boolean
      read FAllowedChildrenAll write FAllowedChildrenAll;

    { TODO:
      Replace TMFNode.Items by a list with notifications?
      Although leave TMFNode methods with trivial implementations
      (TMFNode.Add calls Items.Add), because this way you don't have to write
      "Items." everywhere. }

    { Lists items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like @link(Add). They take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields. }
    property Items: TVRMLNodesList read FItems;

    procedure Add(Node: TVRMLNode); overload;
    procedure Add(Position: Integer; Node: TVRMLNode); overload;

    { @deprecated Deprecated names for @link(Add). }
    procedure AddItem(Node: TVRMLNode); overload;
    procedure AddItem(Position: Integer; Node: TVRMLNode); overload;

    procedure Delete(Index: Integer);
    { Remove child with given Index, and return it, @italic(never freeing it).
      This is analogous to TVRMLNode.ExtractChild, see there for more
      explanation. }
    function Extract(Index: Integer): TVRMLNode;
    procedure Clear;
    procedure AssignItems(SourceItems: TVRMLNodesList);
    procedure Replace(Index: Integer; Node: TVRMLNode);

    procedure ParseValue(Lexer: TVRMLLexer; Names: TObject); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Names: TObject); override;
    procedure ParseXMLElement(Element: TDOMElement; Names: TObject); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;
    procedure AssignDefaultValueFromValue; override;

    property ParentNode: TVRMLNode read FParentNode;

    class function VRMLTypeName: string; override;

    { Checks is Child allowed on the list of nodes of this MFNode,
      and makes VRMLWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      VRMLWarning message will suggest that this Child is added to
      this node. In other words, you should only pass as Child
      a node that you want to add (e.g. by @link(Add)) to this field,
      otherwise VRMLWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TVRMLNode);

    function ChildAllowed(Child: TVRMLNode): boolean;

    { Lists default items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like AssignDefaultItems (they take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields). }
    property DefaultItems: TVRMLNodesList read FDefaultItems;

    { Operate on DefaultItems, just like analogous AssignItems and
      Clear.
      @groupBegin }
    procedure AssignDefaultItems(SourceItems: TVRMLNodesList);
    procedure ClearDefault;
    { @groupEnd }

    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists default false;

    { Calls Func for all current children that are valid
      (allowed by ChildAllowed).

      The main use for this is to simplify implementation of
      @link(TVRMLNode.DirectEnumerateActive) overrides in TVRMLNode descendants. }
    procedure EnumerateValid(Func: TEnumerateChildrenFunction);
  end;

{ Specific VRML nodes -------------------------------------------------------- }

{$I x3d_core.inc}
{$I x3d_time.inc}
{$I x3d_grouping.inc}
{$I x3d_networking.inc}
{$I x3d_rendering.inc}
{$I x3d_shape.inc}
{$I x3d_geometry3d.inc}
{$I x3d_geometry2d.inc}
{$I x3d_text.inc}
{$I x3d_sound.inc}
{$I x3d_lighting.inc}
{$I x3d_texturing.inc}
{$I x3d_interpolation.inc}
{$I x3d_pointingdevicesensor.inc}
{$I x3d_keydevicesensor.inc}
{$I x3d_environmentalsensor.inc}
{$I x3d_navigation.inc}
{$I x3d_environmentaleffects.inc}
{$I x3d_geospatial.inc}
{$I x3d_h-anim.inc}
{$I x3d_nurbs.inc}
{$I x3d_dis.inc}
{$I x3d_scripting.inc}
{$I x3d_eventutilities.inc}
{$I x3d_shaders.inc}
{$I x3d_cadgeometry.inc}
{$I x3d_texturing3d.inc}
{$I x3d_cubemaptexturing.inc}
{$I x3d_layering.inc}
{$I x3d_layout.inc}
{$I x3d_rigidbodyphysics.inc}
{$I x3d_picking.inc}
{$I x3d_followers.inc}
{$I x3d_particlesystems.inc}

{$I vrmlnodes_1.inc}
{$I vrmlnodes_inventor.inc}
{$I vrmlnodes_97_hanim.inc}
{$I vrmlnodes_97_nurbs.inc}
{$I vrmlnodes_kambi.inc}
{$I vrmlnodes_avalon.inc}

{ TVRMLUnknownNode --------------------------------------------------- }

  (* @abstract(TVRMLUnknownNode represents a node with an unrecognized type.)

    If we approach some node with not recognized name we create TVRMLUnknownNode.
    TVRMLUnknownNode has very special Parse method.
    We want to use "fields" and "isA" VRML 1.0 extensibility features here.
    (TODO - these extensibility features are not implemented yet;
     for now all unrecognized nodes are of kind 1))
    We have three cases :

    @orderedList(
      @item(
        Unknown node that doesn't have the "fields" field. It CAN be parsed
        by simply looking for matching "}". Such node will use default
        BoundingBox method implementation ---
        wiec w praktyce taki node nie bedzie robil zadnego renderingu i mial
        BoundingBox = EmptyBox3D.
        Uwaga - *AllowedChildren bedzie = false.)

      @item(
        node ktory ma pole "fields" ale nie ma pola "isA" albo nie jest
        tam podany zaden znany typ node'a. Taki node bedzie mial
        wypelnione podczas parsowania pola Fields i bedzie parsowany
        normalnie ale ciagle nie bedzie mial zadnego wplywu na renderowanie
        sceny VRML'a, podobnie jak w przypadku 1. (chociaz jego dzieci BEDA
        mialy - taki node bedzie zasadniczo dzialal jak node Group)
        Uwaga - *AllowedChildren bedzie = true.)

      @item(
        wreszcie node ktory ma pola "fields" i "isA" i znalezlismy
        wsrod isA jakis znany nam typ node'a. Czyli wiemy ze mamy
        jakies rozszerzenie znanego nam node'a. Taki node bedzie mial
        dynamicznie utworzone pola Fields i bedzie normalnie parsowany
        (tak jak typ 2) a ponadto taki node bedzie mial taki wplyw
        na model VRML'a jakby byl typem node'a ktory jest pierwszym
        znanym nam typem na liscie "isA".
        Uwaga - *AllowedChildren bedzie = *AllowedChildren znanego typu node'a.)
    )

    Ten node nigdy nie powinien byc tworzony tak jak normalny node
    --- wrecz normalne wirtualne Create(const AName: string) spowoduje
    wyjatek ! Tworz obiekty tego typu tylko uzywajac CreateUnknownParse,
    lub CreateUnknown. This way we can safely assume that NodeTypeName
    is always correctly set.

    Spostrzezenie : ten mechanizm jest calkiem dobry - node'y kazdego
    typu, nawet 1, moga byc nazywane i mozna sie pozniej do nich
    odwolywac przez USE. Jezeli node jest typu 2 i 3 to nawet
    ich SubNode'y beda wlaczone w ten standardowy mechanizm !
    Po Parse node'u unknown typu 1) robimy VRMLWarning
    (bo dokladnie to zaszlo --- to jest nieprawidlowy node, ale umiemy sobie
    poradzic).
  *)
  TVRMLUnknownNode = class(TVRMLNode)
  private
    fNodeTypeName: string;
  protected
    function DeepCopyCreate(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    function NodeTypeName: string; override;
    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); override;

    { base Create will throw exception. Always use CreateUnknown* }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    constructor CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
    constructor CreateUnknownParse(const ANodeName, ANodeTypeName :string;
      Lexer: TVRMLLexer; Names: TVRMLNames);
  end;

{ TVRMLInterfaceDeclaration -------------------------------------------------- }

  { Interface declaration, used in VRML (exposed) prototypes and
    for nodes with dynamic fields (Script, ComposedShader).
    See VRML 2.0 spec.

    Each interface specification is a field or an event, stored
    in FieldOrEvent. FieldOrEvent is @nil before parsing.

    Field value is not initialized if you passed FieldValue = @false
    to @link(Parse) (although IsClauseNames will
    always be initialized). FieldValue = @true is used for prototype
    (not external) declarations and nodes with interface declarations
    (Script, ComposedShader etc.).
    In the future maybe some property like
    FieldValueInitialized will be exposed here, if needed at some point.

    Interface declaration doesn't have much properties, since all
    the information is contained within FieldOrEvent
    instance, like Name, field class type, out or in (in case of event),
    exposed or not (in case of field), IsClauseNames. }
  TVRMLInterfaceDeclaration = class(TVRMLFileItem)
  private
    FFieldOrEvent: TVRMLFieldOrEvent;

    { kept in synch with FFieldOrEvent by SetFieldOrEvent }
    FField: TVRMLField;
    FEvent: TVRMLEvent;

    procedure SetFieldOrEvent(const Value: TVRMLFieldOrEvent);
  private
    FParentNode: TVRMLNode;
  public
    constructor Create(AParentNode: TVRMLNode);
    destructor Destroy; override;

    { Containing node, if any, for this VRML interface declaration.
      This must also be set to FieldOrEvent.ParentNode created for this
      interface declaration. }
    property ParentNode: TVRMLNode read FParentNode;

    { Field or event of this interface declaration.
      Is non-nil after parsing.

      You can assign to this property, to constructs interface
      declarations (and so also prototypes) in your own code
      (e.g. this is used X3D XML reader). Just be careful, and remember
      that this object owns FieldOrEvent (that is, will free it
      at destruction). }
    property FieldOrEvent: TVRMLFieldOrEvent
      read FFieldOrEvent write SetFieldOrEvent;

    { Create a copy of current FieldOrEvent.
      Sets NewParentNode as Result.ParentNode.
      Note the new copy will not have ParentIntefaceDeclaration set
      (as the idea is that you own created copy, not this TVRMLInterfaceDeclaration
      instance). }
    function CopyFieldOrEvent(NewParentNode: TVRMLNode): TVRMLFieldOrEvent;

    { Create a copy of current FieldOrEvent, and add it to Node.Fields
      or Node.Events. }
    procedure CopyAndAddFieldOrEvent(Node: TVRMLNode);

    { Copies only reference to FieldOrEvent, adding it to Node.Fields
      or Node.Events. }
    procedure AddFieldOrEvent(Node: TVRMLNode);

    { Return FieldOrEvent casted as appropriate class.
      @nil if such cast is not possible, for example when
      FieldOrEvent is an event and you try to use Field method.
      @groupBegin }
    property Field: TVRMLField read FField;
    property Event: TVRMLEvent read FEvent;
    { @groupEnd }

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames;
      FieldValue, IsClauseAllowed: boolean);

    { Parse interface declaration encoded in XML.

      Note that classic VRML parser has here IsClauseAllowed: boolean
      parameter, this was set to @true when parsing InterfaceDeclarations
      of special nodes (Script, ComposedShader etc.), since they could
      have IS clause (at least, as far as I understood the spec).
      But for X3D XML encoding, it's not available, since (AFAI understand
      the X3D XML encoding spec) the <IS> element inside node body may
      point from nodeField to any interface field of this node, including
      InterfaceDeclarations. So ParseISStatement handles this. }
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames;
      FieldValue: boolean);

    { Save this interface declaration to stream.
      This assumes that it starts at the beginning of the line,
      and at the end always writes NL, so at the end it's also
      at the beginning of some line.

      @param(FieldValue If @true then we will always save
        Field value or IS clause to stream, along with this interface
        decl (if this interface declaration has the Field set).
        Otherwise, field's value will not be saved, only IS clause
        if present.)
    }
    procedure IDeclSaveToStream(SaveProperties: TVRMLSaveToStreamProperties;
      NodeNames: TVRMLNodeNames; FieldValue: boolean);

    { Save this interface declaration to stream.

      Saves with field value, just by calling
      IDeclSaveToStream(SaveProperties, NodeNames, true).

      @seealso IDeclSaveToStream }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;

    { Returns access type, corresponding to current @link(Event)
      and @link(Field) values.

      Result is undefined if both Event
      and Field are @nil (which may happen when it's not initialized
      (e.g. parsed) yet) or when both are non-nil (which should never
      happen). }
    function AccessType: TVRMLAccessType;

    function DeepCopy(NewParentNode: TVRMLNode;
      CopyState: TVRMLNodeDeepCopyState): TVRMLInterfaceDeclaration;
  end;

  TObjectsListItem_2 = TVRMLInterfaceDeclaration;
  {$I objectslist_2.inc}
  TVRMLInterfaceDeclarationsList = class(TObjectsList_2)
  public
    { Find field or event with given Name.
      @nil if not found. }
    function TryFindName(const Name: string): TVRMLFieldOrEvent;

    { Find field with given Name.
      @nil if not found. }
    function TryFindFieldName(const Name: string): TVRMLField;

    { Find event with given Name.
      @nil if not found. }
    function TryFindEventName(const Name: string): TVRMLEvent;
  end;

{ TVRMLPrototype ------------------------------------------------------------- }

  TVRMLPrototypeBase = class;

  EVRMLPrototypeInstantiateError = class(Exception);

  { VRML node related to a given prototype.

    This node will have fields
    initialized according to associated Prototype.InterfaceDeclarations.
    This way you can simply parse this node (just like any other node)
    to parse prototype instance.

    The prototype may be instantiated. After parsing you can
    do it by @link(Instantiate) method. In case of non-external prototype,
    this should always be possible (for a valid VRML files, that is),
    in case of external prototype this may requite loading the external
    prototype file.

    This node cannot be created by standard Create method,
    always use CreatePrototypeNode. }
  TVRMLPrototypeNode = class(TVRMLNode)
  private
    FPrototype: TVRMLPrototypeBase;

    procedure PrepareInstantiateIsClause(Node, Child: TVRMLNode);

    (*This searches Node for fields/events with "IS" clauses, and handles them:
      for fields, this means copying field value from Self to Child
      (and setting ValueFromIsClause).
      for events, this means adding internal routes to route event
      from/to Self to/from Child.

      Handled IS clauses are removed (to not be seen by next calls
      of InstantiateIsClauses on the same node, or on it's copy,
      see below).

      It also descends recursively into all children nodes.
      Note that it descends into all
      nodes, even the ones that came from another prototype
      (PrototypeInstance is @true). It doesn't try to omit them,
      as they may have "IS" clauses that refer to our fields.
      Consider part of key_sensor.x3dv test:

@preformatted(
         PROTO SimpleText [
           inputOutput MFString onestring ""
         ] { Shape { geometry Text { string IS onestring } } }

         PROTO PressedText [
           inputOutput MFString againstring ""
         ] { SimpleText { onestring IS againstring } }

         PressedText { againstring "zero" }
)

      After expanding SimpleText within PressedText, we have
      @code(Shape { geometry Text { string IS againstring } }),
      that is we resolved "IS onestring" to yet another IS clause:
      "IS againstring". Which means that when expanding PressedText,
      we have to process everything again, to eventually fill "againstring"
      value. *)
    procedure InstantiateIsClauses(Node, Child: TVRMLNode);

    { Handle "IS" clause on Destination field/event, by copying it's value
      from Source.

      For fields basically does Destination.AssignValue(Source).
      In case of EVRMLFieldAssign, make VRMLWarning with clear message.

      For events establishes internal route.

      Assumes that all Source "IS" clauses are not expanded yet.
      In fact, Source field/event always comes from this node,
      that is TVRMLPrototypeNode. So we just expand prototype within
      TVRMLPrototypeNode. If this TVRMLPrototypeNode is itself within
      another prototype, there's no way Source.IsClauseNames could be expanded
      already.

      Above paragraph means that when Source.IsClauseNames exist,
      we know what to do. If this is about fields, then we have to copy
      field's value (in case in Source field value was specified explicitly)
      and also assign Source.IsClauseNames as new Destination.IsClauseNames
      (eventually, higher IS clauses will override field's value).
      For events, we also copy IsClauseNames, in addition to creating
      internal route (is internal route still necessary? for exposedFields?
      seems so, I'm not sure...). }
    procedure FieldOrEventHandleIsClause(
      Destination, Source: TVRMLFieldOrEvent;
      NewIsClauseNames: TDynStringArray);
  protected
    function DeepCopyCreate(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    { This constructor will raise exception for TVRMLPrototypeNode.
      Always use CreatePrototypeNode for this node class. }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    constructor CreatePrototypeNode(const ANodeName, AWWWBasePath :string;
      APrototype: TVRMLPrototypeBase);

    function NodeTypeName: string; override;

    property Prototype: TVRMLPrototypeBase read FPrototype;

    { Instantiate the prototype, that is create new VRML node
      (of "normal" classs, not TVRMLPrototypeNode) using prototype description.

      For non-external prototype, in essense it just takes Prototype.Node
      and returns it's copy. For external prototype it first loads external file,
      and then uses non-external prototype there. Eventually,
      for external prototype we may also use build-in node (if URN will
      indicate so).

      Actually, the process is a little more involved (see below for
      details), but the idea is that returned node can be simply inserted
      into VRML hierarchy and works just like a normal node.
      The important feature is that returned instance class is the same
      that was specified as a first prototype node. For example, if the
      prototype should expand to Material node, then this returns
      TNodeMaterial. Just like Material node would be normally specified,
      not created by some prototype.

      Note that this TVRMLPrototypeNode becomes "owned" by returned
      node instance, in PrototypeInstanceSourceNode.
      (that's needed for returned node's SaveToStream to work correctly).

      Details:
      @unorderedList(
        @item(
          Prototype.Node may be just a wrapper, i.e. TVRMLRootNode_1
          or TVRMLRootNode_2.

          In this case the first children of Prototype.Node is used
          to create instance. The rest of the wrapper (with this first children
          removed, to not cause cycles) is also duplicated and set
          as new node's PrototypeInstanceHelpers.)

        @item(
          Returned Node (with all it's helpers in PrototypeInstanceHelpers)
          has "IS" clauses everywhere filled, according to our field values.)

        @item(NodeName of returned node is copied from our NodeName.)

        @item(
          For SaveToStream to work, returned Node has PrototypeInstance = @true,
          and PrototypeInstanceSourceNode set to Self. This allows SaveToStream
          to correctly save using PrototypeInstanceSourceNode, instead
          of writing actual node contents.))

      @raises(EVRMLPrototypeInstantiateError if for some reason
        the prototype cannot be instantiated.
        You can catch this and replace with VRMLWarning, if possible.)
    }
    function Instantiate: TVRMLNode;
  end;

  TVRMLPrototypeBase = class(TVRMLFileItem)
  private
    FName: string;
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;

    FWWWBasePath: string;

    { Parses InterfaceDeclarations. Also inits WWWBasePath from
      Names.WWWBasePath, by the way. }
    procedure ParseInterfaceDeclarations(ExternalProto: boolean;
      Lexer: TVRMLLexer; Names: TVRMLNames);

    { Parse interface declarations in XML encoding.
      Handle sequence of <field> elements.

      Note: unlike classic ParseInterfaceDeclarations,
      this doesn't set WWWBasePath, do it yourself (because often
      you do not have 'ProtoInterface', so you would have to do it yourself
      anyway). }
    procedure ParseInterfaceDeclarationsXML(ExternalProto: boolean;
      Element: TDOMElement; Names: TVRMLNames);

    { Saves Name, and interface declarations enclosed
      within [ ]. In descendant, you should first write the keyword PROTO
      or EXTERNPROTO, then call this, then write the rest of your prototype. }
    procedure SaveInterfaceDeclarationsToStream(
      SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TVRMLNodeNames;
      ExternalProto: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;

    { Parse prototype, and add it to Names.Prototypes by @link(Bind).
      @groupBegin }
    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); virtual; abstract;
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames); virtual; abstract;
    { @groupEnd }

    { The base URL path used to resolve urls inside.
      For now, used by EXTERNPROTO urls.
      See TVRMLNode.WWWBasePath for more comments. }
    property WWWBasePath: string read FWWWBasePath write FWWWBasePath;
  end;

  TObjectsListItem_4 = TVRMLPrototypeBase;
  {$I objectslist_4.inc}
  TVRMLPrototypeBasesList = class(TObjectsList_4);

  TVRMLPrototype = class(TVRMLPrototypeBase)
  private
    FNode: TVRMLNode;
  public
    destructor Destroy; override;

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); override;
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames); override;
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;

    { These are actual prototype contents: all nodes, prototypes, routes
      defined within this prototype.

      We wrap this all inside a single
      VRML node, just like we do when reading whole VRML file:
      if the whole thing is exactly one VRML node, then this is this node.
      Otherwise, we wrap inside artificial TVRMLRootNode_1 or
      TVRMLRootNode_2.

      You have permission to write to this property, to be able to
      write code that constructs prototypes (like needed by X3D XML reader).
      Just be careful. This node is owned by prototype instance (will be
      freed at destructor). }
    property Node: TVRMLNode read FNode write FNode;
  end;

  TVRMLExternalPrototype = class(TVRMLPrototypeBase)
  private
    FURLList: TMFString;

    { FReferencedPrototype has links to other parts of the VRML graph.
      Not only FReferencedPrototype.Node, but also
      FReferencedPrototype.InterfaceDeclaration may have links to it:
      if referenced node has SFNode or MFNode fields and their default
      values have "USE ..." clauses.
      So it's best to keep whole ReferencedPrototypeNode (whole VRML file
      that contained this prototype) loaded. }
    ReferencedPrototypeNode: TVRMLNode;

    FReferencedPrototype: TVRMLPrototype;

    FReferencedClass: TVRMLNodeClass;
  public
    constructor Create;
    destructor Destroy; override;
    property URLList: TMFString read FURLList;

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); override;
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames); override;
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;

    property ReferencedPrototype: TVRMLPrototype read FReferencedPrototype;
    property ReferencedClass: TVRMLNodeClass read FReferencedClass;

    { Loads URL, until the first success. Sets either ReferencedClass to non-nil
      (if it's built-in node) or ReferencedPrototype (if prototype expansion
      found in external file). }
    procedure LoadReferenced;
    procedure UnloadReferenced;
  end;

{ TVRMLRoute ----------------------------------------------------------------- }

  TVRMLRoute = class(TVRMLFileItem)
  private
    FSourceNode: TVRMLNode;
    FSourceEvent: TVRMLEvent;

    FDestinationNode: TVRMLNode;
    FDestinationEvent: TVRMLEvent;

    LastEventTime: TVRMLTime;
    FInternal: boolean;

    procedure DestructionNotification(Node: TVRMLNode);

    procedure UnsetEnding(
      var Node: TVRMLNode; var Event: TVRMLEvent;
      const DestEnding: boolean;
      RemoveFromDestructionNotification: boolean = true);

    procedure SetEnding(const NodeName, FieldOrEventName: string;
      Names: TVRMLNames;
      var Node: TVRMLNode; var Event: TVRMLEvent;
      const DestEnding: boolean);

    { Set Event, based on FieldOrEvent (may be actual event,
      or exposed field containing it) and DestEnding.
      Assumes that Event is clear on enter (cleared by UnsetEnding). }
    procedure SetEndingInternal(
      const Node: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent;
      var Event: TVRMLEvent;
      const DestEnding: boolean);

    procedure SetEndingDirectly(
      const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent;
      var Node: TVRMLNode; var Event: TVRMLEvent;
      const DestEnding: boolean);

    procedure EventReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    constructor Create;
    destructor Destroy; override;

    { Source event properties. Either all three are @nil, or:

      @unorderedList(
        @item(SourceEvent is assigned, meaning is self-explanatory.

          Note: if you want to get it's exposed field, remember this
          is available in SourceEvent.ParentExposedField.)

        @item(SourceNode must also be assigned and
          this must be the node enclosing SourceEvent. That is, the node that
          has SourceEvent as one of explicit (on TVRMLNode.Events list) or
          implicit (exposed by some field) event.)
      )

      @groupBegin }
    property SourceNode: TVRMLNode read FSourceNode;
    property SourceEvent: TVRMLEvent read FSourceEvent;
    { @groupEnd }

    { Destination event properties.
      Analogous to SourceEvent, SourceNode.

      @groupBegin }
    property DestinationNode: TVRMLNode read FDestinationNode;
    property DestinationEvent: TVRMLEvent read FDestinationEvent;
    { @groupEnd }

    { Set source/destination of the route.

      This does everything that VRML parser should
      do when parsed VRML route. It looks for given node name
      (in Names.Nodes, then Names.Imported),
      then it looks for field/event within this node,
      and if everything is successfull --- sets route properties.

      If something goes wrong, VRMLWarning is generated
      and route ending is left unset.

      @groupBegin }
    procedure SetSource(
      const SourceNodeName, SourceFieldOrEventName: string;
      Names: TVRMLNames);

    procedure SetDestination(
      const DestinationNodeName, DestinationFieldOrEventName: string;
      Names: TVRMLNames);
    { @groupEnd }

    { These set source/destination of the route in more direct way.

      FieldOrEvent is used to set SourceEvent (or DestinationEvent).
      FieldOrEvent may be the actual event to set,
      or exposed field containing this event.

      You specify explictly NewNode, which is not checked in any way.
      We don't check whether it exists, whether it contains given
      FieldOrEvent,  etc. --- you have to guarantee this yourself.
      Also, remember that normal SetSource actually look for events
      inside PrototypeInstanceSourceNode, if exists --- if you want this,
      you have to do this yourself when using these SetXxxDirectly.
      It is used to set SourceNode (or DestinationNode).
      Overloaded versions that don't take NewNode parameter just assume
      that NewNode can be taken from FieldOrEvent.ParentNode.

      @groupBegin }
    procedure SetSourceDirectly(
      const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent);

    procedure SetDestinationDirectly(
      const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent);

    procedure SetSourceDirectly(const FieldOrEvent: TVRMLFieldOrEvent);
    procedure SetDestinationDirectly(const FieldOrEvent: TVRMLFieldOrEvent);
    { @groupEnd }

    { Parse the route (classic VRML encoding).
      Implementation should be able to safely assume that current token
      is ROUTE. }
    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames);

    { Parse the route (XML encoding).
      Given Element here must have TagName = 'ROUTE'. }
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames);

    { Save a ROUTE to VRML file.

      Will generate VRMLWarning when route cannot be saved.
      This can happen when SourceNode or SourceEvent
      or DestinationNode or DestinationEvent are @nil.
      Also, if SourceNode and DestinationNode are without a name,
      or the name is not currently bound in SaveProperties.NodeNames.
    }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;

    { Clear the memory when the last event passed through this route.
      Route must remember such thing, to avoid loops in routes.
      This is following VRML 2.0 / X3D specifications, that explicitly
      say that only one event per ROUTE per timestamp is allowed.

      Use ResetLastEventTime when you really want to reset this memory.
      In practice, this should be used only by TVRMLScene.ResetTime
      implementation. }
    procedure ResetLastEventTime;

    { Internal routes are created by PROTO expansion code, which
      needs to create internal routes to implement "IS" clauses for events.

      These routes work exactly like normal routes, except:
      @unorderedList(
        @item(They are not saved to file (SaveToStream will ignore
          internal route).)
        @item(It's allowed (in fact, this will always happen for current
          internal routes) to route one input event to another input event.)
      )
    }
    property Internal: boolean read FInternal write FInternal default false;

    function DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLRoute;
  end;

  TObjectsListItem_5 = TVRMLRoute;
  {$I objectslist_5.inc}
  TVRMLRoutesList = class(TObjectsList_5);

  TVRMLImport = class(TVRMLFileItem)
  public
    InlineNodeName, ImportedNodeName, ImportedNodeAlias: string;

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames);

    { Parse the IMPORT declaration (XML encoding).
      Given Element here must have TagName = 'IMPORT'. }
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames);

    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;
    function DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLImport;
  end;

  TVRMLExport = class(TVRMLFileItem)
  public
    ExportedNodeName, ExportedNodeAlias: string;

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames);

    { Parse the EXPORT declaration (XML encoding).
      Given Element here must have TagName = 'EXPORT'. }
    procedure ParseXML(Element: TDOMElement; Names: TVRMLNames);

    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject); override;
    function DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLExport;
  end;

{$I vrmlnodes_eventsengine.inc}

{ Node names ----------------------------------------------------------------- }

  { List to keep node names while parsing VRML file.
    This assumes that all strings are node names and their Objects[]
    are TVRMLNode instances with given names. }
  TVRMLNodeNames = class(TStringListCaseSens)
  private
    FAutoRemove: boolean;
    procedure DestructionNotification(Node: TVRMLNode);
  public
    constructor Create(const AAutoRemove: boolean);
    destructor Destroy; override;

    { If @true (determined at construction time),
      then destroyed nodes will be automatically removed from this list.
      This allows you to safely destroy node instances during this
      objects lifetime, without worrying that some dangling pointers remain
      on this list.

      Internally, this is done by registering
      itself for AnyNodeDestructionNotifications. }
    property AutoRemove: boolean read FAutoRemove;

    procedure Bind(Node: TVRMLNode); overload;
    procedure Bind(Node: TVRMLNode; const BindToName: string); overload;

    { Find node bound to given name. @nil if none. }
    function Bound(const Name: string): TVRMLNode;

    { Check is Node bound in the current namespace.
      @false means that node is not within this namespace,
      possibly it's name was hidden by other node with the same name.

      Doesn't check is Node bound to it's name (Node.NodeName) or something
      else. So this assumes that node can only be bound (if at all)
      only to it's own name, which is true during parsing
      (when nothing can change in the middle of parsing). }
    function Bound(Node: TVRMLNode): boolean;
  end;

  TVRMLPrototypeNames = class(TStringListCaseSens)
  public
    procedure Bind(Proto: TVRMLPrototypeBase);

    { Find proto bound to given name. @nil if none. }
    function Bound(const Name: string): TVRMLPrototypeBase;
  end;

  TVRMLImportableNames = class(TStringListCaseSens)
  public
    destructor Destroy; override;

    { Bind Exported names to given Inline node name.
      Exported instance becomes owner by this TVRMLImportableNames instance.
      InlineName must be <> '' here. }
    procedure Bind(const InlineName: string; Exported: TVRMLNodeNames);
  end;

  { Container tracking VRML/X3D node and prototype names during parsing.
    Used by both classic and XML VRML/X3D readers. }
  TVRMLNames = class
  private
    FVRMLVerMajor, FVRMLVerMinor: integer;
    FWWWBasePath: string;
    FNodes: TVRMLNodeNames;
    FPrototypes: TVRMLPrototypeNames;
    FImported: TVRMLNodeNames;
    FExported: TVRMLNodeNames;
    FImportable: TVRMLImportableNames;
  public
    constructor Create(const AAutoRemoveNodes: boolean;
      const AWWWBasePath: string;
      const AVRMLVerMajor, AVRMLVerMinor: Integer);
    destructor Destroy; override;

    { Base path for resolving URLs from nodes in this namespace.
      See TVRMLNode.WWWBasePath. }
    property WWWBasePath: string read FWWWBasePath;

    { VRML version numbers, for resolving node class names.
      Conventions the same as for TVRMLLexer.VRMLVerMajor,
      TVRMLLexer.VRMLVerMinor. }
    property VRMLVerMajor: Integer read FVRMLVerMajor;
    property VRMLVerMinor: Integer read FVRMLVerMinor;

    { Current namespace for DEF/USE.

      This is a list without duplicates with all
      currently known node names. Objects[] of this list point to
      actual TVRMLNode instances. If many instances had the same NodeName,
      only the last instance will be referenced here, following VRML spec
      (last DEF takes precedence).

      Internal notes: ParseNode doesn't modify this, only TVRMLNode.Parse
      can do this. }
    property Nodes: TVRMLNodeNames read FNodes;

    { Current namespace of PROTO names. }
    property Prototypes: TVRMLPrototypeNames read FPrototypes;

    { Currently IMPORTed nodes.

      The nodes on this list are "bound" to their aliases,
      as this is the name under which they are visible in the current namespace.
      Alias is the identifier after the "AS" keyword in the "IMPORT" declaration
      (or, if no "AS xxx" clause was present, then alias is just the name
      under which node was exported). }
    property Imported: TVRMLNodeNames read FImported;

    { Currently EXPORTed nodes from this scene.

      The nodes on this list are "bound" to their
      aliases, as this is the name under which they are visible for
      the outside VRML scenes (that can import these nodes).
      Alias is the identifier after the "AS" keyword in "EXPORT" declaration
      (or, if no "AS xxx" clause, then alias is just normal node name). }
    property Exported: TVRMLNodeNames read FExported;

    { Currently loaded Inlines with importable nodes.

      The mechanism is that when you load an Inline node, the resulting
      "Exported" nodes (from the namespace within the Inline) get added
      to this "Importable" list. Then the "IMPORT" clause in this
      namespace can make "Importable" nodes into actually "Imported".

      This is a list with strings representing Inline node names
      (there's no way to IMPORT from unnamed Inline nodes).
      Objects[] of this list are instances of TVRMLNodeNames
      corresponding to exported names within the inline. }
    property Importable: TVRMLImportableNames read FImportable;

    procedure DoExport(E: TVRMLExport);
    procedure DoImport(I: TVRMLImport);
  end;

{ TraverseStateLastNodesClasses ---------------------------------------------- }

const
  { opis patrz TTraverseStateLastNodes }
  TraverseStateLastNodesClasses :
    array [TVRML1StateNode] of TVRMLNodeClass =
    ( TNodeCoordinate3, TNodeShapeHints, TNodeFontStyle_1,
      TNodeMaterial_1, TNodeMaterialBinding, TNodeNormal, TNodeNormalBinding,
      TNodeTexture2, TNodeTextureCoordinate2,
      TNodeKambiTriangulation
      { additions here must be synchronized with additions to
        TTraverseStateLastNodes }
    );

{ TNodesManager ------------------------------------------------------------ }

type
  ENodesManagerError = class(EVRMLError);
  ENodeClassRegisterError = class(ENodesManagerError);
  TNodesManager = class
  private
    { Strings[] is ClassNodeTypeName. Objects[] is the actual class
      (typecast to TVRMLNodeClass is safe). }
    FRegistered: TStringList;
    function GetRegistered(Index: Integer): TVRMLNodeClass;
  public
    constructor Create;
    destructor Destroy; override;

    { Mozesz rejestrowac tylko klasy o ClassNodeTypeName <> '' (w tej procedurze
      to sprawdzimy i ew. rzucimy wyjatek ENodeClassRegisterError).

      Nie mozesz zarejestrowac dwa razy tej samej klasy,
      spowoduje to ENodeClassRegisterError.

      Natomiast mozesz zarejestrowac wiele razy rozne klasy o tym samym
      ClassNodeTypeName. For example TNodeCone_1 and TNodeCone_2.
      They will be chosen in NodeTypeNameToClass using their ForVRMLVersion. }
    procedure RegisterNodeClass(NodeClass: TVRMLNodeClass);
    procedure RegisterNodeClasses(const NodeClasses: array of TVRMLNodeClass);

    { This unregisters class NodeClass, i.e. it removes it from
      our map table.

      @raises(ENodesManagerError if NodeClass.ClassNodeTypeName = ''
        (so it cannot be even registered), or if
        ((NodeClass was not registered) and ErrorIfNotRegistered)) }
    procedure UnRegisterNodeClass(NodeClass: TVRMLNodeClass;
      ErrorIfNotRegistered: boolean = true);

    { NodesManager zostal stworzony wlasnie po to aby zaimplementowac
      funkcje TypeNameToClass: odwzorowuje ona nazwe typu VRMLa
      na klase VRMLa ktora ma takie samo ClassNodeTypeName.

      Aby takie cos
      przeprowadzic potrzebny byl gdzies ekwiwalent globalnej tablicy
      przechowujacej wszystkie stworzone klasy wezlow VRMLa --- takim
      odpowiednikiem jest wlasnie ta klasa do ktorej trzeba rejestrowac wezly.
      Bedzie szukac wsrod zarejestrowanych klas klasy o zadanym
      ClassNodeTypeName, wybierajac tylko klase ktorej
      @code(ForVRMLVersion(VerMajor, VerMinor)) will return @true.

      Jesli nie znajdzie zwroci nil. }
    function NodeTypeNameToClass(const ANodeTypeName: string;
      const VerMajor, VerMinor: Integer): TVRMLNodeClass;

    { Return class that matches given URL. This is useful for EXTERNROTOs.
      Returns @nil if not found. }
    function URNToClass(const URN: string): TVRMLNodeClass;

    { Enumerate all registered classes, from Registered[0] to
      Registered[RegisteredCount - 1].

      @groupBegin }
    property Registered [Index: Integer]: TVRMLNodeClass read GetRegistered;
    function RegisteredCount: Cardinal;
    { @groupEnd }
  end;

var
  { tworzony i niszczony w init/fini tego modulu }
  NodesManager: TNodesManager;

{ global procedures ---------------------------------------------------------- }

{$I vrmlnodes_encoding_classic.inc}
{$I vrmlnodes_encoding_xml.inc}

{ Create and assign all State.Nodes. }
procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);

{ Free and nil all State.Nodes. }
procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);

{ Free all VRML nodes with no parents on the list, then free and @nil the list
  itself. }
procedure VRMLNodesList_FreeWithNonParentedContentsAndNil(var List: TVRMLNodesList);

const
  ProjectionTypeToStr: array [TProjectionType] of string =
  ('Orthographic', 'Perspective');

const
  { consts for TNodeAsciiText.FdJustification.Value }
  JUSTIFICATION_LEFT = 0;
  JUSTIFICATION_CENTER = 1;
  JUSTIFICATION_RIGHT = 2;

  { consts for TNode(Material|Normal)Binding.FdValue.Value }
  BIND_DEFAULT = 0;
  BIND_OVERALL = 1;
  BIND_PER_PART = 2;
  BIND_PER_PART_INDEXED = 3;
  BIND_PER_FACE = 4;
  BIND_PER_FACE_INDEXED = 5;
  BIND_PER_VERTEX = 6;
  BIND_PER_VERTEX_INDEXED = 7;

  { consts for TNodeShapeHints.FdVertexOrdering.Value }
  VERTORDER_UNKNOWN = 0;
  VERTORDER_CLOCKWISE = 1;
  VERTORDER_COUNTERCLOCKWISE = 2;

  { consts for TNodeShapeHints.FdShapeType.Value }
  SHTYPE_UNKNOWN = 0;
  SHTYPE_SOLID = 1;

  { consts for TNodeShapeHints.FdFaceType.Value }
  FACETYPE_UNKNOWN = 0;
  FACETYPE_CONVEX = 1;

  { consts for TNodeFontStyle.FdFamily.Value }
  FSFAMILY_SERIF = 0;
  FSFAMILY_SANS = 1;
  FSFAMILY_TYPEWRITER = 2;

  { consts for TNodeFontStyle.FdStyleFlags[] }
  FSSTYLE_BOLD = 0;
  FSSTYLE_ITALIC = 1;

  { consts for TNodeCone.FdParts.Flags[] }
  CONE_PARTS_SIDES = 0;
  CONE_PARTS_BOTTOM = 1;

  { consts for TNodeCylinder.FdParts.Flags[] }
  CYLINDER_PARTS_SIDES = 0;
  CYLINDER_PARTS_TOP = 1;
  CYLINDER_PARTS_BOTTOM = 2;

  { consts for TNodeTexture2.FdWrapS/WrapT.Value }
  TEXWRAP_REPEAT = 0;
  TEXWRAP_CLAMP = 1;

  DefaultHeightMapScale = 0.01;
  DefaultVRML1CreaseAngle = 0.5;

  DefaultViewpointFieldOfView = Pi / 4;
  DefaultNavigationInfoHeadlight = true;

  DefaultRenderedTextureWidth  = 128;
  DefaultRenderedTextureHeight = 128;

  DefaultShadowMapScale = 1.1;
  DefaultShadowMapBias = 4.0;

{ TODO: these Detail parameters below should depend on object's distance
  from viewer. But there is a problem : we need those parameters defined
  when implementing Vertices/TrianglesCount and Triangulate. }
var
  { cylinder, cone, sphere and disk slices/stacks (slices for all objects
    must be equal to perfectly "match" when objects are connected
    (e.g. sphere connected with cylinder). Stacks and RectDivisions
    nie sa do tego zmuszone ale i tak nie ma zadnego sensownego powodu
    zeby z gory mowic ze dana bryla potrzebuje mniej stacks a inna wiecej).

    For the meaning of Detail_Quadric* consts look at definition of glu
    quadric functions (it is not guaranteed that our code will use this
    functions but we will always honour this Detail parameters in the same way).

    For the meaning of Detail_RectDivisions (used only in Cube for now) look
    at KambiGLUtils.DrawGLPlane.

    For now, you can change these variables only @italic(before using anything)
    from this module.

    These variables @italic(must) always honour Min values listed below. }
  Detail_QuadricSlices: Cardinal = 30;
  Detail_QuadricStacks: Cardinal = 20;
  Detail_RectDivisions: Cardinal = 2;

const
  { uzywaj w programie zawsze tych stalych zamiast zakladac ze maja one
    konkretne wartosci, ale mozesz oczywiscie przyjac zalozenie ze na pewno
    sa one Cardinalami (sa >=0) }
  MinQuadricSlices: Cardinal = 3; { mimo ze OpenGL akceptuje minimum 2, ale dla 2 wynik jest bez sensu }
  MinQuadricStacks: Cardinal = 1;
  MinRectDivisions: Cardinal = 0;

const
  { URNs used to indicate standard VRML / X3D nodes.

    Funny thing, I actually didn't found anywhere a definite official
    statement that they are using such-and-such URNs.

    X3D specification refers to RFC
    [http://www.ietf.org/rfc/rfc3541.txt?number=3541] which, basically,
    just says "we like URNs and we'll use them" and nothing more.
    Same thing for VRML 97 spec
    [http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/extensions.html].
    There is no precise answer e.g. what URN should be used to
    Indicate some standard VRML 97 / X3D node.

    I constructed URNs below looking at examples in the RFC,
    annotated by a funny line "The following examples are not
    guaranteed to be real. They are presented for pedagogical reasons only."

    @groupBegin }
  URNVRML97Nodes = 'urn:web3d:vrml97:node:';
  URNX3DNodes = 'urn:web3d:x3d:node:';
  { @groupEnd }

  { URN used to indicate VRML / X3D nodes that are Kambi VRML game engine
    extensions. }
  URNKambiNodes = 'urn:vrmlengine.sourceforge.net:node:';

const
  AllAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput];
  RestrictedAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly];

var
  { Functions registered here will be called when any TVRMLNode descendant
    will be destroyed. }
  AnyNodeDestructionNotifications: TDynNodeDestructionNotificationArray;

  { Starting state nodes for TVRMLGraphTraverseState.Create.

    This is read-only from outside, initialized and finalized in
    this unit. }
  StateDefaultNodes: TTraverseStateLastNodes;

{ Find a range within "key" field corresponding to given Fraction.
  Returns the index of @bold(right) range delimiter.
  So for normal ranges (between two values of "key" field) it's
  always between 1 and FdKey.Count - 1. Result 0 indicates we're
  before the left limit, and result equal FdKey.Count indicates
  we're after right limit.

  Result is always between 0 and FdKey.Count.

  Output T is the value between 0..1 indicating where within
  the range we are. It's undefined when Result is 0 or Key.Count
  (indicating we're outside limits).

  Call this only when FdKey.Count > 0.

  This is useful to interpreting TNodeX3DInterpolatorNode.KeyRange
  and such fields. }
function KeyRange(Key: TDynSingleArray;
  const Fraction: Single; out T: Single): Integer;

var
  { Global list of all TVRMLChangesEngine instances.

    This makes it easy in TVRMLChangesEngine implementation to add/remove
    itself from this list. Alternative would be to make ChangeListeners
    in each TVRMLNode, which would be more efficient (only scenes that
    really contain given node would be notified), but also difficult
    (we would need to be much more careful when adding / removing nodes
    from the TVRMLScene.RootNode graph). }
  ChangeListeners: TVRMLChangesEnginesList;

{$undef read_interface}

implementation

uses
  { Fonts for Text, FontStyle, AsciiText nodes }
  { Bitstream Vera Sans }
  TTF_BitstreamVeraSans_Unit,
  TTF_BitstreamVeraSans_Bold_Unit,
  TTF_BitstreamVeraSans_Italic_Unit,
  TTF_BitstreamVeraSans_Bold_Italic_Unit,
  { Bitstream Vera Sans Mono }
  TTF_BitstreamVeraSansMono_Unit,
  TTF_BitstreamVeraSansMono_Bold_Unit,
  TTF_BitstreamVeraSansMono_Italic_Unit,
  TTF_BitstreamVeraSansMono_Bold_Italic_Unit,
  { Bitstream Vera Serif }
  TTF_BitstreamVeraSerif_Unit,
  TTF_BitstreamVeraSerif_Bold_Unit,
  TTF_BitstreamVeraSerif_Italic_Unit,
  TTF_BitstreamVeraSerif_Bold_Italic_Unit,

  Math, Triangulator, Object3DAsVRML, KambiZStream, VRMLCameraUtils,
  KambiStringUtils, KambiFilesUtils, RaysWindow, StrUtils, KambiURLUtils,
  VRMLGeometry, KambiLog, KambiScriptParser, Base64,
  {$ifdef KAMBI_HAS_NURBS} NURBS, {$endif} Quaternions, Cameras, KambiXMLUtils;

{$define read_implementation}

{$define GeometryNotImplemented :=
  function TGeometryNotImplemented.LocalBoundingBox(State: TVRMLGraphTraverseState;
    ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
  begin
    Result := EmptyBox3D;
  end;

  function TGeometryNotImplemented.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
    ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
  begin
    Result := 0;
  end;

  function TGeometryNotImplemented.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
    ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
  begin
    Result := 0;
  end;

  procedure TGeometryNotImplemented.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
    ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
  begin
  end;
}

{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}
{$I objectslist_4.inc}
{$I objectslist_5.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}

{$I vrmlnodes_boundingboxes.inc}
{$I vrmlnodes_verticesandtrianglescounting.inc}
{$I vrmlnodes_triangulating.inc}
{$I vrmlnodes_coordpolygons.inc}
{$I vrmlnodes_eventsengine.inc}
{$I vrmlnodes_cone_cylinder.inc}
{$I vrmlnodes_sphere.inc}
{$I vrmlnodes_box.inc}

{$I x3d_core.inc}
{$I x3d_time.inc}
{$I x3d_grouping.inc}
{$I x3d_networking.inc}
{$I x3d_rendering.inc}
{$I x3d_shape.inc}
{$I x3d_geometry3d.inc}
{$I x3d_geometry2d.inc}
{$I x3d_text.inc}
{$I x3d_sound.inc}
{$I x3d_lighting.inc}
{$I x3d_texturing.inc}
{$I x3d_interpolation.inc}
{$I x3d_pointingdevicesensor.inc}
{$I x3d_keydevicesensor.inc}
{$I x3d_environmentalsensor.inc}
{$I x3d_navigation.inc}
{$I x3d_environmentaleffects.inc}
{$I x3d_geospatial.inc}
{$I x3d_h-anim.inc}
{$I x3d_nurbs.inc}
{$I x3d_dis.inc}
{$I x3d_scripting.inc}
{$I x3d_eventutilities.inc}
{$I x3d_shaders.inc}
{$I x3d_cadgeometry.inc}
{$I x3d_texturing3d.inc}
{$I x3d_cubemaptexturing.inc}
{$I x3d_layering.inc}
{$I x3d_layout.inc}
{$I x3d_rigidbodyphysics.inc}
{$I x3d_picking.inc}
{$I x3d_followers.inc}
{$I x3d_particlesystems.inc}

{$I vrmlnodes_1.inc}
{$I vrmlnodes_inventor.inc}
{$I vrmlnodes_97_hanim.inc}
{$I vrmlnodes_97_nurbs.inc}
{$I vrmlnodes_kambi.inc}
{$I vrmlnodes_avalon.inc}
{$I vrmlnodes_encoding_classic.inc}
{$I vrmlnodes_encoding_xml.inc}

resourcestring
  SExpectedInterfaceDeclaration =
    'Expected interface declaration (for VRML 2.0: eventIn, eventOut, field or ' +
    'exposedField keyword; for X3D: inputOnly, outputOnly, initializeOnly or ' +
    'inputOutput keyword) but found %s';
  SExpectedFieldType =
    'Expected field type name (like SFVec2f etc.) but found %s';

function InterfaceDeclarationKeywords(
  const AccessTypes: TVRMLAccessTypes): TVRMLKeywords;
begin
  Result := [];
  if atInputOnly in AccessTypes then
    Result += [vkEventIn, vkInputOnly];
  if atOutputOnly in AccessTypes then
    Result += [vkEventOut, vkOutputOnly];
  if atInitializeOnly in AccessTypes then
    Result += [vkField, vkInitializeOnly];
  if atInputOutput in AccessTypes then
    Result += [vkExposedField, vkInputOutput];
end;

{ TDynActiveLightArray --------------------------------------------------------- }

function TDynActiveLightArray.IndexOfLightNode(LightNode: TVRMLLightNode): integer;
begin
 for result := 0 to High do
  if Items[result].LightNode = LightNode then exit;
 result := -1;
end;

function TDynActiveLightArray.Equals(SecondValue: TObject): boolean;

  function ActiveLightEquals(const L1, L2: TActiveLight): boolean;
  begin
    Result := (L1.LightNode = L2.LightNode) and
      MatricesPerfectlyEqual(L1.Transform, L2.Transform);

    { No need to compare TransfLocation or TransfNormDirection,
      as they are just precalculated based on LightNode and Transform. }
  end;

var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TDynActiveLightArray) and
    (TDynActiveLightArray(SecondValue).Count = Count);
  if Result then
    for I := 0 to High do
      if not ActiveLightEquals(Items[I], TDynActiveLightArray(SecondValue).Items[I]) then
        Exit(false);
end;

{ TVRMLGraphTraverseState ---------------------------------------------------- }

procedure TVRMLGraphTraverseState.CommonCreate;
begin
  inherited Create;
  VRML1ActiveLights := TDynActiveLightArray.Create;
  VRML2ActiveLights := TDynActiveLightArray.Create;
  PointingDeviceSensors := TPointingDeviceSensorsList.Create;
end;

constructor TVRMLGraphTraverseState.CreateCopy(Source: TVRMLGraphTraverseState);
begin
  CommonCreate;
  Assign(Source);
end;

constructor TVRMLGraphTraverseState.Create(const ADefaultLastNodes: TTraverseStateLastNodes);
begin
  CommonCreate;

  Transform := IdentityMatrix4Single;
  AverageScaleTransform := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  AssignLastNodes(ADefaultLastNodes);
end;

constructor TVRMLGraphTraverseState.Create;
begin
  Create(StateDefaultNodes);
end;

destructor TVRMLGraphTraverseState.Destroy;
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
  begin
    if FOwnedLastNodes[SN] then FLastNodes.Nodes[SN].Free;
    FLastNodes.Nodes[SN] := nil;
  end;

  FreeAndNil(VRML1ActiveLights);
  FreeAndNil(VRML2ActiveLights);
  FreeAndNil(PointingDeviceSensors);

  inherited;
end;

procedure TVRMLGraphTraverseState.Clear;
begin
  Transform := IdentityMatrix4Single;
  AverageScaleTransform := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  AssignLastNodes(StateDefaultNodes);
  ShapeNode := nil;
  InsideInline := 0;
  InsidePrototype := 0;
  InsideIgnoreCollision := 0;
  InsideInvisible := 0;

  PointingDeviceSensors.Count := 0;
  VRML1ActiveLights.Count := 0;
  VRML2ActiveLights.Count := 0;
end;

procedure TVRMLGraphTraverseState.Assign(
  Source: TVRMLGraphTraverseState);
begin
  AssignTransform(Source);

  TextureTransform := Source.TextureTransform;
  AssignLastNodes(Source.FLastNodes);
  ShapeNode := Source.ShapeNode;
  InsideInline := Source.InsideInline;
  InsidePrototype := Source.InsidePrototype;
  InsideIgnoreCollision := Source.InsideIgnoreCollision;
  InsideInvisible := Source.InsideInvisible;

  PointingDeviceSensors.Assign(Source.PointingDeviceSensors);
  VRML1ActiveLights.Assign(Source.VRML1ActiveLights);
  VRML2ActiveLights.Assign(Source.VRML2ActiveLights);
end;

procedure TVRMLGraphTraverseState.AssignTransform(
  Source: TVRMLGraphTraverseState);
begin
  Transform := Source.Transform;
  AverageScaleTransform := Source.AverageScaleTransform;
  InvertedTransform := Source.InvertedTransform;
end;

function TVRMLGraphTraverseState.Equals(SecondValue: TObject):
  boolean;
var
  SV: TVRMLGraphTraverseState;
  SN: TVRML1StateNode;
begin
  { InsideInline, InsidePrototype, InsideIgnoreCollision, InsideInvisible,
    PointingDeviceSensors
    are currently ignored by Equals,
    since Equals is used for TVRMLOpenGLRenderer where difference
    in these is not important. This may be clarified in the interface
    and improved later. }

  Result :=
    (SecondValue <> nil) and
    (SecondValue is TVRMLGraphTraverseState);

  if not Result then Exit;

  SV := TVRMLGraphTraverseState(SecondValue);

  Result :=
    VRML1ActiveLights.Equals(SV.VRML1ActiveLights) and
    VRML2ActiveLights.Equals(SV.VRML2ActiveLights) and
    { no need to compare InvertedTransform, it should be equal when normal
      Transform is equal. }
    MatricesPerfectlyEqual(Transform, SV.Transform) and
    (AverageScaleTransform = SV.AverageScaleTransform) and
    MatricesPerfectlyEqual(TextureTransform, SV.TextureTransform) and
    (ShapeNode = SV.ShapeNode);

  if Result then
  begin
    for SN := Low(SN) to High(SN) do
      if SV.LastNodes.Nodes[SN] <> LastNodes.Nodes[SN] then
        Exit(false);
  end;
end;

function TVRMLGraphTraverseState.EqualsNoTransform(
  SecondValue: TVRMLGraphTraverseState): boolean;
var
  SN: TVRML1StateNode;
begin
  { InsideInline, InsidePrototype, InsideIgnoreCollision, InsideInvisible,
    PointingDeviceSensors,
    ActiveLights, Transform, AverageScaleTransform, InvertedTransform,
    TextureTransform are ignored by
    TVRMLOpenGLRenderer.RenderShapeNoTransform }

  Result := (ShapeNode = SecondValue.ShapeNode);

  if Result then
  begin
    for SN := Low(SN) to High(SN) do
      if SecondValue.LastNodes.Nodes[SN] <> LastNodes.Nodes[SN] then
        Exit(false);
  end;
end;

function TVRMLGraphTraverseState.Texture: TNodeX3DTextureNode;
begin
  if ShapeNode = nil then
    Result := LastNodes.Texture2 else
    Result := ShapeNode.Texture;
end;

function TVRMLGraphTraverseState.BlendMode: TNodeBlendMode;
var
  Node: TVRMLNode;
begin
  Result := nil;
  if ShapeNode <> nil then
  begin
    Node := ShapeNode.FdAppearance.Value;
    if (Node <> nil) and (Node is TNodeKambiAppearance) then
    begin
      Node := TNodeKambiAppearance(Node).FdBlendMode.Value;
      if (Node <> nil) and (Node is TNodeBlendMode) then
        Result := TNodeBlendMode(Node);
    end;
  end;
end;

function TVRMLGraphTraverseState.CurrentActiveLights: TDynActiveLightArray;
begin
  if ShapeNode = nil then
    Result := VRML1ActiveLights else
    Result := VRML2ActiveLights;
end;

procedure TVRMLGraphTraverseState.SetLastNodes(const StateNode: TVRML1StateNode;
  const Node: TVRMLNode; const OwnNode: boolean);
begin
  if FLastNodes.Nodes[StateNode] <> Node then
  begin
    Assert(Node is TraverseStateLastNodesClasses[StateNode]);

    if FOwnedLastNodes[StateNode] then
      FreeAndNil(FLastNodes.Nodes[StateNode]);

    FLastNodes.Nodes[StateNode] := Node;
    FOwnedLastNodes[StateNode] := OwnNode;
  end;
end;

procedure TVRMLGraphTraverseState.AssignLastNodes(
  const NewLostNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    if FLastNodes.Nodes[SN] <> NewLostNodes.Nodes[SN] then
    begin
      if FOwnedLastNodes[SN] then FreeAndNil(FLastNodes.Nodes[SN]);
      FOwnedLastNodes[SN] := false;
      FLastNodes.Nodes[SN] := NewLostNodes.Nodes[SN];
    end;
end;

{ TVRMLGraphTraverseStateStack --------------------------------------------- }

constructor TVRMLGraphTraverseStateStack.Create;
begin
  inherited;
end;

destructor TVRMLGraphTraverseStateStack.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    FreeAndNil(Items[I]);
  inherited;
end;

procedure TVRMLGraphTraverseStateStack.GrowItems;
var
  I, OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(Items, OldLen + 100);
  for I := OldLen to Length(Items) - 1 do
    Items[I] := TVRMLGraphTraverseState.Create;
end;

procedure TVRMLGraphTraverseStateStack.PushClear;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;

  { We could instead do Clear in Pop, and then we would know that all
    non allocated instances are always clear.

    But this would be slower: Push is called very often,
    much more often than PushClear (which is called only once
    for every traverse, while Push possibly many times).
    And Pop is called for every Push and PushClear.
    So it's better to optimize the most often called (Pop) than the
    least often called (PushClear). }

  Items[ItemsAllocated].Clear;
  Inc(ItemsAllocated);
end;

procedure TVRMLGraphTraverseStateStack.Push;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;

  Items[ItemsAllocated].Assign(Items[ItemsAllocated - 1]);
  Inc(ItemsAllocated);
end;

procedure TVRMLGraphTraverseStateStack.Pop;
begin
  Dec(ItemsAllocated);
end;

function TVRMLGraphTraverseStateStack.Top: TVRMLGraphTraverseState;
begin
  Result := Items[ItemsAllocated - 1];
end;

{$I vrmlnodes_node.inc}

{ TVRMLNodeClassesList ------------------------------------------------------- }

function TVRMLNodeClassesList.GetItems(Index: Integer): TVRMLNodeClass;
begin
  Result := TVRMLNodeClass(inherited Items[Index]);
end;

procedure TVRMLNodeClassesList.SetItems(Index: Integer; Value: TVRMLNodeClass);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TVRMLNodeClassesList.AssignArray(
  const AItemsArray: array of TVRMLNodeClass);
var
  I: Integer;
begin
  Count := High(AItemsArray) + 1;
  for I := 0 to High(AItemsArray) do
    Items[I] := AItemsArray[I];
end;

function TVRMLNodeClassesList.IndexOf(NodeClass: TVRMLNodeClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = NodeClass then
      Exit;
  Result := -1;
end;

function TVRMLNodeClassesList.IndexOf(Node: TVRMLNode): Integer;
begin
  Result := IndexOf(TVRMLNodeClass(Node.ClassType));
end;

function TVRMLNodeClassesList.IndexOfAnyAncestor(Node: TVRMLNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Node is TVRMLNodeClass(Items[Result]) then
      Exit;
  Result := -1;
end;

procedure TVRMLNodeClassesList.Add(Value: TVRMLNodeClass);
begin
  inherited Add(Pointer(Value));
end;

procedure TVRMLNodeClassesList.AddRegisteredImplementing(Interf: TGUID);
var
  I: Integer;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
    if Supports(NodesManager.Registered[I], Interf) then
      Add(NodesManager.Registered[I]);
end;

{ TSFNode --------------------------------------------------------------------- }

constructor TSFNode.CreateUndefined(AParentNode: TVRMLFileItem;
  const AName: string);
begin
  inherited;
  Value := nil;

  FAllowedChildren := TVRMLNodeClassesList.Create;
  FAllowedChildrenAll := true;

  FDefaultValue := nil;
  FDefaultValueExists := false;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  const AnAllowedChildren: array of TVRMLNodeClass;
  AValue: TVRMLNode);
begin
  inherited Create(AParentNode, AName);

  { FParentNode is just a copy of inherited (TVRMLFieldOrEvent) FParentNode,
    but casted to TVRMLNode }
  FParentNode := AParentNode;

  FAllowedChildren.AssignArray(AnAllowedChildren);
  FAllowedChildrenAll := false;

  Value := AValue;
  AssignDefaultValueFromValue;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AnAllowedChildren: TVRMLNodeClassesList;
  AValue: TVRMLNode);
begin
  Create(AParentNode, AName, [], AValue);

  FAllowedChildren.Assign(AnAllowedChildren);
  FAllowedChildrenAll := false;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AllowedChildrenInterface: TGUID;
  AValue: TVRMLNode);
var
  AllowedChildren: TVRMLNodeClassesList;
begin
  { TODO: this unnecessarily creates long list of AllowedChildren,
    and depends on current NodesManager registered nodes.
    Some day I may improve this, to just store AllowedChildrenInterface
    inside, and check at runtime "Supports".
    Same thing for MFNode analogous constructor. }

  AllowedChildren := TVRMLNodeClassesList.Create;
  AllowedChildren.AddRegisteredImplementing(AllowedChildrenInterface);
  try
    Create(AParentNode, AName, AllowedChildren, AValue);
  finally FreeAndNil(AllowedChildren) end;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  { To delete Self from DefaultValue.FParentFields, and eventually free DefaultValue. }
  DefaultValue := nil;
  FreeAndNil(FAllowedChildren);
  inherited;
end;

function TSFNode.ChildAllowed(Child: TVRMLNode): boolean;
begin
  Result := (Child = nil) or
    AllowedChildrenAll or
    (FAllowedChildren.IndexOfAnyAncestor(Child) <> -1);
end;

function TSFNode.CurrentChildAllowed: boolean;
begin
  Result := ChildAllowed(Value);
end;

procedure TSFNode.WarningIfChildNotAllowed(Child: TVRMLNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.NodeTypeName, Name]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.NodeTypeName]);
    VRMLWarning(vwSerious, S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TSFNode.ParseValue(Lexer: TVRMLLexer; Names: TObject);
begin
  if (Lexer.Token = vtKeyword) and (Lexer.TokenKeyword = vkNULL) then
  begin
    Value := nil;
    Lexer.NextToken;
  end else
  begin
    { This is one case when we can use NilIfUnresolvedUSE = @true }
    Value := ParseNode(Lexer, Names as TVRMLNames, true);
    if Value <> nil then
      WarningIfChildNotAllowed(Value);
  end;
end;

procedure TSFNode.ParseXMLAttribute(const AttributeValue: string; Names: TObject);
const
  SNull = 'NULL';
begin
  { For SFNode and MFNode, X3D XML encoding has special handling:
    field value just indicates the node name, or NULL.
    (other values for SFNode / MFNode cannot be expressed inside
    the attribute). }

  Value := (Names as TVRMLNames).Nodes.Bound(AttributeValue);
  if Value = nil then
  begin
    if AttributeValue <> SNull then
      VRMLWarning(vwSerious, Format('Invalid node name for SFNode field: "%s"', [AttributeValue]));
  end else
  begin
    WarningIfChildNotAllowed(Value);
  end;
end;

procedure TSFNode.ParseXMLElement(Element: TDOMElement; Names: TObject);
var
  Child: TVRMLNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
begin
  I := TXMLElementIterator.Create(Element);
  try
    if I.GetNext then
    begin
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Names as TVRMLNames, true);
      if Child <> nil then
      begin
        Value := Child;
        WarningIfChildNotAllowed(Child);
      end;

      if I.GetNext then
        VRMLWarning(vwSerious, Format('X3D field "%s" is SFNode, but it contains more than one XML element (2nd element is "%s")',
          [Name, I.Current.TagName]));
    end;
  finally FreeAndNil(I) end;
end;

procedure TSFNode.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties;
  NodeNames: TObject);
begin
  if Value = nil then
    SaveProperties.Write('NULL') else
  begin
    { TVRMLNode.SaveToStream normally starts from new line with an indent...
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    SaveProperties.DiscardNextIndent;
    Value.SaveToStream(SaveProperties, NodeNames);
  end;
end;

function TSFNode.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (Value = DefaultValue);
end;

function TSFNode.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Double): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFNode) and
   (TSFNode(SecondValue).Value = Value);
end;

procedure TSFNode.Assign(Source: TPersistent);
begin
  if Source is TSFNode then
  begin
    { Assign using Value property, so that FParentFields will get
      correctly updated. }
    Value              := TSFNode(Source).Value;
    DefaultValue       := TSFNode(Source).DefaultValue;
    DefaultValueExists := TSFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TSFNode.AssignValue(Source: TVRMLField);
begin
  if Source is TSFNode then
  begin
    inherited;
    Value := TSFNode(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFNode.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSFNode.SetValue(AValue: TVRMLNode);
begin
  if FValue <> AValue then
  begin
    if FValue <> nil then
      FValue.RemoveParentField(Self);

    FValue := AValue;

    if AValue <> nil then
      FValue.AddParentField(Self);
  end;
end;

procedure TSFNode.SetDefaultValue(ADefaultValue: TVRMLNode);
begin
  if FDefaultValue <> ADefaultValue then
  begin
    if FDefaultValue <> nil then
      FDefaultValue.RemoveParentField(Self);

    FDefaultValue := ADefaultValue;

    if ADefaultValue <> nil then
      FDefaultValue.AddParentField(Self);
  end;
end;

procedure TSFNode.SetDefaultValueExists(AValue: boolean);
begin
  FDefaultValueExists := AValue;
end;

class function TSFNode.VRMLTypeName: string;
begin
  Result := 'SFNode';
end;

procedure TSFNode.EnumerateValid(Func: TEnumerateChildrenFunction);
begin
  if (Value <> nil) and CurrentChildAllowed then
    Func(ParentNode, Value);
end;

{ TMFNode -------------------------------------------------------------------- }

constructor TMFNode.CreateUndefined(AParentNode: TVRMLFileItem;
  const AName: string);
begin
  inherited;
  FItems := TVRMLNodesList.Create;

  FAllowedChildren := TVRMLNodeClassesList.Create;
  FAllowedChildrenAll := true;

  FDefaultItems := TVRMLNodesList.Create;
  FDefaultValueExists := false;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  const AnAllowedChildren: array of TVRMLNodeClass);
begin
  inherited Create(AParentNode, AName);
  FParentNode := AParentNode;

  FAllowedChildren.AssignArray(AnAllowedChildren);
  FAllowedChildrenAll := false;

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AnAllowedChildren: TVRMLNodeClassesList);
begin
  Create(AParentNode, AName, []);

  FAllowedChildren.Assign(AnAllowedChildren);
  FAllowedChildrenAll := false;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AllowedChildrenInterface: TGUID);
var
  AllowedChildren: TVRMLNodeClassesList;
begin
  AllowedChildren := TVRMLNodeClassesList.Create;
  AllowedChildren.AddRegisteredImplementing(AllowedChildrenInterface);
  try
    Create(AParentNode, AName, AllowedChildren);
  finally FreeAndNil(AllowedChildren) end;
end;

destructor TMFNode.Destroy;
begin
  Clear;
  ClearDefault;
  FreeAndNil(FItems);
  FreeAndNil(FDefaultItems);
  FreeAndNil(FAllowedChildren);
  inherited;
end;

procedure TMFNode.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties;
  NodeNames: TObject);
var
  I: Integer;
begin
  { We code Count = 0 and Count = 1 cases separately just to get a more
    compact look in these common situations. }
  if Count = 0 then
    SaveProperties.Write('[]') else
  if Count = 1 then
  begin
    { TVRMLNode.SaveToStream normally starts from new line with an indent...
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    SaveProperties.DiscardNextIndent;
    Items[0].SaveToStream(SaveProperties, NodeNames);
  end else
  begin
    SaveProperties.Writeln('[');
    SaveProperties.IncIndent;
    for I := 0 to Count - 1 do
      Items[I].SaveToStream(SaveProperties, NodeNames);
    SaveProperties.DecIndent;
    SaveProperties.WriteIndent(']');
  end;
end;

function TMFNode.GetCount: integer;
begin
  Result := Items.Count;
end;

procedure TMFNode.SetCount(const Value: Integer);
var
  I: Integer;
begin
  if Value < Items.Count then
  begin
    for I := Value to Items.Count - 1 do
      Items[I].RemoveParentField(Self);
    Items.Count := Value;
  end else
  if Value > Items.Count then
  begin
    { TObjectsList makes sure that increasing count sets new items to nil }
    Items.Count := Value;
  end;
end;

procedure TMFNode.Add(Node: TVRMLNode);
begin
  Items.Add(Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.Add(Position: Integer; Node: TVRMLNode);
begin
  Items.Insert(Position, Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.AddItem(Node: TVRMLNode);
begin
  Add(Node);
end;

procedure TMFNode.AddItem(Position: Integer; Node: TVRMLNode); overload;
begin
  Add(Position, Node);
end;

procedure TMFNode.Delete(Index: Integer);
begin
  Items[Index].RemoveParentField(Self);
  Items.Delete(Index);
end;

function TMFNode.Extract(Index: Integer): TVRMLNode;
begin
  Result := Items[Index];

  { Instead of calling Result.RemoveParentField(Self), which would possibly
    free Result, we manually call FParentFields.Delete. }
  Result.FParentFields.Remove(Self);

  Items.Delete(Index);
end;

procedure TMFNode.Replace(Index: Integer; Node: TVRMLNode);
begin
  if FItems[Index] <> Node then
  begin
    FItems[Index].RemoveParentField(Self);
    FItems[Index] := Node;
    FItems[Index].AddParentField(Self);
  end;
end;

procedure TMFNode.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].RemoveParentField(Self);
  FItems.Count := 0;
end;

procedure TMFNode.ClearDefault;
var
  I: Integer;
begin
  for I := 0 to FDefaultItems.Count - 1 do
    FDefaultItems[I].RemoveParentField(Self);
  FDefaultItems.Count := 0;
end;

procedure TMFNode.AssignItems(SourceItems: TVRMLNodesList);
var
  I: Integer;
begin
  Clear;

  Items.Assign(SourceItems);

  for I := 0 to Count - 1 do
    Items[I].AddParentField(Self);
end;

procedure TMFNode.AssignDefaultItems(SourceItems: TVRMLNodesList);
var
  I: Integer;
begin
  ClearDefault;

  DefaultItems.Assign(SourceItems);

  for I := 0 to DefaultItems.Count - 1 do
    DefaultItems[I].AddParentField(Self);
end;

function TMFNode.ChildAllowed(Child: TVRMLNode): boolean;
begin
  Result := (Child = nil) or
    AllowedChildrenAll or
    (FAllowedChildren.IndexOfAnyAncestor(Child) <> -1);
end;

procedure TMFNode.WarningIfChildNotAllowed(Child: TVRMLNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.NodeTypeName, Name]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.NodeTypeName]);
    VRMLWarning(vwSerious, S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TMFNode.ParseValue(Lexer: TVRMLLexer; Names: TObject);

  procedure ParseOneItem;
  var
    Node: TVRMLNode;
  begin
    Node := ParseNode(Lexer, Names as TVRMLNames, false);
    Add(Node);
    WarningIfChildNotAllowed(Node);
  end;

begin
  Clear;

  { Note that we ignore commas here, because MFNode is in VRML 2.0 only. }
  if Lexer.Token = vtOpenSqBracket then
  begin
    Lexer.NextToken;

    while Lexer.Token <> vtCloseSqBracket do
      ParseOneItem;

    Lexer.NextToken;
  end else
  begin
    { one single item - not enclosed in [] brackets }
    ParseOneItem;
  end;
end;

procedure TMFNode.ParseXMLAttribute(const AttributeValue: string; Names: TObject);
var
  Node: TVRMLNode;
begin
  Node := (Names as TVRMLNames).Nodes.Bound(AttributeValue);
  if Node = nil then
  begin
    { NULL not allowed for MFNode, unlike the SFNode }
    VRMLWarning(vwSerious, Format('Invalid node name for MFNode field: "%s"', [AttributeValue]));
  end else
  begin
    Add(Node);
    WarningIfChildNotAllowed(Node);
  end;
end;

procedure TMFNode.ParseXMLElement(Element: TDOMElement; Names: TObject);
var
  Child: TVRMLNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Names as TVRMLNames, true);
      if Child <> nil then
      begin
        Add(Child);
        WarningIfChildNotAllowed(Child);
      end;
    end;
  finally FreeAndNil(I) end;
end;

function TMFNode.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and DefaultItems.Equals(Items);
end;

function TMFNode.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TMFNode) and
    (TMFNode(SecondValue).Items.Equals(Items));
end;

procedure TMFNode.Assign(Source: TPersistent);
begin
  if Source is TMFNode then
  begin
    AssignItems(TMFNode(Source).Items);
    AssignDefaultItems(TMFNode(Source).DefaultItems);
    DefaultValueExists := TMFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TMFNode.AssignValue(Source: TVRMLField);
begin
  if Source is TMFNode then
  begin
    inherited;
    AssignItems(TMFNode(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TMFNode.AssignDefaultValueFromValue;
begin
  inherited;
  AssignDefaultItems(Items);
  DefaultValueExists := true;
end;

class function TMFNode.VRMLTypeName: string;
begin
  Result := 'MFNode';
end;

procedure TMFNode.EnumerateValid(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if ChildAllowed(Items[I]) then
      Func(ParentNode, Items[I]);
end;

{ TVRMLUnknownNode ---------------------------------------------------------------- }

function TVRMLUnknownNode.NodeTypeName: string;
begin
 result := fNodeTypeName;
end;

procedure TVRMLUnknownNode.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
{ TODO: tutaj zrobic parsowanie node'ow unknown typu 2) i 3),
  VRMLWarning tez nie trzeba zawsze rzucac. }
begin
  { w przypadku TVRMLUnknownNode musimy fAllowedChildren i fParseAllowedChildren
    inicjowac na podstawie parsowania. }
  fAllowedChildren := false;
  fParsingAllowedChildren := false;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  ParseIgnoreToMatchingCurlyBracket(Lexer, Names);

  FWWWBasePath := Names.WWWBasePath;

  VRMLWarning(vwSerious, 'Unknown VRML node of type '''+NodeTypeName+
    ''' (named '''+NodeName+''')');
end;

constructor TVRMLUnknownNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 { ponizej : "bezpiecznik" zeby nigdy nie tworzyc tego node'a normalnie,
   zeby zawsze fNodeTypeName bylo ustalone. }
 raise Exception.Create('You cannot create Unknown node using default constructor');
end;

constructor TVRMLUnknownNode.CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
begin
 inherited Create(ANodeName, AWWWBasePath);
 fNodeTypeName := ANodeTypeName;
end;

constructor TVRMLUnknownNode.CreateUnknownParse(const ANodeName, ANodeTypeName :string;
  Lexer: TVRMLLexer; Names: TVRMLNames);
begin
 CreateUnknown(ANodeName, '', ANodeTypeName);
 Parse(Lexer, Names);
end;

function TVRMLUnknownNode.DeepCopyCreate(
  CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
begin
  Result := TVRMLUnknownNode.CreateUnknown(NodeName, WWWBasePath,
    NodeTypeName);
end;

{ TVRMLInterfaceDeclaration -------------------------------------------------- }

constructor TVRMLInterfaceDeclaration.Create(AParentNode: TVRMLNode);
begin
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TVRMLInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FFieldOrEvent);
  FField := nil;
  FEvent := nil;

  inherited;
end;

procedure TVRMLInterfaceDeclaration.SetFieldOrEvent(
  const Value: TVRMLFieldOrEvent);
begin
  FFieldOrEvent := Value;

  { set FField and FEvent, for fast access to them }
  if FFieldOrEvent = nil then
  begin
    FField := nil;
    FEvent := nil;
  end else
  if FFieldOrEvent is TVRMLField then
  begin
    FField := TVRMLField(FFieldOrEvent);
    FEvent := nil;
  end else
  begin
    Assert(FFieldOrEvent is TVRMLEvent);
    FField := nil;
    FEvent := TVRMLEvent(FFieldOrEvent);
  end;
end;

procedure TVRMLInterfaceDeclaration.Parse(Lexer: TVRMLLexer; Names: TVRMLNames;
  FieldValue, IsClauseAllowed: boolean);
var
  FieldTypeName: string;
  Access: TVRMLAccessType;
  FieldType: TVRMLFieldClass;
  Name: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  if Lexer.Token = vtKeyword then
  begin
    case Lexer.TokenKeyword of
      vkEventIn, vkInputOnly: Access := atInputOnly;
      vkEventOut, vkOutputOnly: Access := atOutputOnly;
      vkField, vkInitializeOnly: Access := atInitializeOnly;
      vkExposedField, vkInputOutput: Access := atInputOutput;
      else raise EVRMLParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
    end;
  end else
    raise EVRMLParserError.Create(
      Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'field type (for interface declaration)');
  FieldTypeName := Lexer.TokenName;
  FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
  if FieldType = nil then
    raise EVRMLParserError.Create(
      Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'name (for interface declaration)');
  Name := Lexer.TokenName;

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := TVRMLEvent.Create(ParentNode, Name, FieldType, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      begin
        FieldOrEvent := FieldType.CreateUndefined(ParentNode, Name);
        Field.Exposed := Access = atInputOutput;
      end;
    else raise EInternalError.Create('Access ? in TVRMLInterfaceDeclaration.Parse');
  end;

  Lexer.NextToken;

  if Event <> nil then
  begin
    if IsClauseAllowed then
      Event.Parse(Lexer);
  end else
  begin
    if FieldValue then
      Field.Parse(Lexer, Names, IsClauseAllowed) else
    if IsClauseAllowed then
      Field.ParseIsClause(Lexer);
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

procedure TVRMLInterfaceDeclaration.ParseXML(
  Element: TDOMElement; Names: TVRMLNames; FieldValue: boolean);
var
  Access: TVRMLAccessType;
  AccessIndex: Integer;
  AccessName: string;
  FieldTypeName: string;
  FieldType: TVRMLFieldClass;
  Name, FieldActualValue: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  { calculate Access }
  if DOMGetAttribute(Element, 'accessType', AccessName) then
  begin
    AccessIndex := ArrayPosStr(AccessName,
      ['inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput']);
    if AccessIndex <> -1 then
      Access := TVRMLAccessType(AccessIndex) else
      raise EX3DXmlError.CreateFmt('Access type "%s" unknown', [AccessName]);
  end else
    raise EX3DXmlError.Create('Missing access type in X3D interface declaration');

  { calculate FieldType }
  if DOMGetAttribute(Element, 'type', FieldTypeName) then
  begin
    FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
    if FieldType = nil then
      raise EX3DXmlError.CreateFmt('Field type "%s" unknown', [FieldTypeName]);
  end else
    raise EX3DXmlError.Create('Missing field type in X3D interface declaration');

  if not DOMGetAttribute(Element, 'name', Name) then
    raise EX3DXmlError.Create('Missing name in X3D interface declaration');

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := TVRMLEvent.Create(ParentNode, Name, FieldType, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      begin
        FieldOrEvent := FieldType.CreateUndefined(ParentNode, Name);
        Field.Exposed := Access = atInputOutput;
      end;
    else raise EInternalError.Create('AccessType ?');
  end;

  if Event <> nil then
  begin
    { Classic VRML parser has here
        if IsClauseAllowed then Event.Parse(Lexer);
      but for X3D XML encoding this is not needed, see comments above. }
  end else
  begin
    if FieldValue then
    begin
      if DOMGetAttribute(Element, 'value', FieldActualValue) then
        Field.ParseXMLAttribute(FieldActualValue, Names) else
        Field.ParseXMLElement(Element, Names);
    end;

    { Classic VRML parser has here
        else if IsClauseAllowed then Field.ParseIsClause(Lexer);
      but for X3D XML encoding this is not needed, see comments above. }
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

function TVRMLInterfaceDeclaration.CopyFieldOrEvent(
  NewParentNode: TVRMLNode): TVRMLFieldOrEvent;
var
  F: TVRMLField absolute Result;
  E: TVRMLEvent absolute Result;
begin
  if Field <> nil then
  begin
    { F := copy of Field }
    F := TVRMLFieldClass(Field.ClassType).CreateUndefined(NewParentNode, Field.Name);
    F.Assign(Field);

    { CreateUndefined creates field without any default value,
      so it will always get saved later to file.

      But this is not nice: for non-node fields, it merely makes
      resulting file longer. For node fields (SFNode and MFNode)
      this means that node value will be written to file. But this
      is bad, since this means that node contents will have to duplicated,
      if node is not named or it's name is unbound now (e.g. overridden
      by other node name) (otherwise "USE Xxx" could be used, which
      is acceptable).

      See ../../kambi_vrml_test_suite/x3d/proto_sfnode_default.x3dv
      and tricky_def_use.x3dv for
      examples (open and save it back e.g. in view3dscene).

      So to make it work right, we have to set DefaultValue for our
      fields, in particular for TSFNode and TMFNode fields.
      So that EqualsDefaultValue will work Ok when saving to file. }
    F.AssignDefaultValueFromValue;
  end else
  if Event <> nil then
  begin
    { E := copy of Event }
    E := TVRMLEvent.Create(NewParentNode,
      Event.Name, Event.FieldClass, Event.InEvent);
  end else
    raise EInternalError.Create('interface declaration but no Field or Event');

  Result.ParentInterfaceDeclaration := nil;
end;

procedure TVRMLInterfaceDeclaration.AddFieldOrEvent(
  Node: TVRMLNode);
begin
  if Field <> nil then
    Node.Fields.Add(Field) else
  begin
    Assert(Event <> nil);
    Node.Events.Add(Event);
  end;
end;

procedure TVRMLInterfaceDeclaration.CopyAndAddFieldOrEvent(
  Node: TVRMLNode);
var
  Copy: TVRMLFieldOrEvent;
begin
  Copy := CopyFieldOrEvent(Node);
  if Copy is TVRMLField then
    Node.Fields.Add(TVRMLField(Copy)) else
  begin
    Assert(Copy is TVRMLEvent);
    Node.Events.Add(TVRMLEvent(Copy));
  end;
end;

procedure TVRMLInterfaceDeclaration.IDeclSaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TVRMLNodeNames;
  FieldValue: boolean);

  function ATName(const AcessType: TVRMLAccessType): string;
  const
    Names: array
      [ boolean { is it X3D ? },
        TVRMLAccessType] of string =
      ( ('eventIn', 'eventOut', 'field', 'exposedField'),
        ('inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput') );
  begin
    Result := Names[SaveProperties.VerMajor >= 3, AcessType];
  end;

begin
  if Event <> nil then
  begin
    if Event.InEvent then
      SaveProperties.WriteIndent(ATName(atInputOnly) + ' ') else
      SaveProperties.WriteIndent(ATName(atOutputOnly) + ' ');
    SaveProperties.Write(Event.FieldClass.VRMLTypeName + ' ');
    SaveProperties.DiscardNextIndent;
    Event.EventSaveToStream(SaveProperties, true);
  end else
  begin
    if Field.Exposed then
      SaveProperties.WriteIndent(ATName(atInputOutput) + ' ') else
      SaveProperties.WriteIndent(ATName(atInitializeOnly) + ' ');
    SaveProperties.Write(Field.VRMLTypeName + ' ');

    { When saving from interface declaration, you can only
      1. write sole field name
      2. write field name + value (if FieldValue = @true)
      3. write field name + exactly one IS clause }

    if ( FieldValue and
         (not Field.ValueFromIsClause) and
         (Field.IsClauseNames.Count = 0) ) then
    begin
      { Field.SaveToStream normally starts from new line with an indent...
        In this case, we want it to start on the same line, so indent must
        be discarded. }
      SaveProperties.DiscardNextIndent;
      Field.FieldSaveToStream(SaveProperties, NodeNames, true, true);
      { In this case, SaveProperties.Writeln will be done by Field.SaveToStream.
        (we pass SaveWhenDefault anyway, so we can be sure that
        this newline will be done). }
    end else

    if Field.IsClauseNames.Count = 1 then
    begin
      SaveProperties.DiscardNextIndent;
      Field.FieldSaveToStream(SaveProperties, NodeNames, true, false);
    end else

    begin
      SaveProperties.Writeln(Field.Name);
    end;
  end;
end;

procedure TVRMLInterfaceDeclaration.SaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
begin
  IDeclSaveToStream(SaveProperties, NodeNames as TVRMLNodeNames, true);
end;

function TVRMLInterfaceDeclaration.AccessType: TVRMLAccessType;
begin
  if Event <> nil then
  begin
    if Event.InEvent then
      Result := atInputOnly else
      Result := atOutputOnly;
  end else
  if Field <> nil then
  begin
    if Field.Exposed then
      Result := atInputOutput else
      Result := atInitializeOnly;
  end else
    { Result is undefined in this case, but we don't want to signal any error }
    Result := atInitializeOnly
end;

function TVRMLInterfaceDeclaration.DeepCopy(
  NewParentNode: TVRMLNode;
  CopyState: TVRMLNodeDeepCopyState): TVRMLInterfaceDeclaration;
begin
  Result := TVRMLInterfaceDeclaration.Create(NewParentNode);
  Result.FieldOrEvent := CopyFieldOrEvent(NewParentNode);
  Result.FieldOrEvent.ParentInterfaceDeclaration := Result;
end;

{ TVRMLInterfaceDeclarationsList --------------------------------------------- }

function TVRMLInterfaceDeclarationsList.TryFindName(
  const Name: string): TVRMLFieldOrEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FieldOrEvent;
    if Result.Name = Name then
      Exit;
  end;
  Result := nil;
end;

function TVRMLInterfaceDeclarationsList.TryFindFieldName(const Name: string): TVRMLField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TVRMLField then
    begin
      Result := Items[I].Field;
      if Result.Name = Name then
        Exit;
    end;
  Result := nil;
end;

function TVRMLInterfaceDeclarationsList.TryFindEventName(const Name: string): TVRMLEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TVRMLEvent then
    begin
      Result := Items[I].Event;
      if Result.Name = Name then
        Exit;
    end;
  Result := nil;
end;

{ TVRMLPrototypeNode --------------------------------------------------------- }

constructor TVRMLPrototypeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  raise EInternalError.Create('TVRMLPrototypeNode node must be created' +
    ' using CreatePrototypeNode, never default constructor');
end;

constructor TVRMLPrototypeNode.CreatePrototypeNode(
  const ANodeName, AWWWBasePath :string;
  APrototype: TVRMLPrototypeBase);
var
  I: TVRMLInterfaceDeclaration;
  Index: Integer;
  ProtoInitial: TVRMLPrototypeBase;
begin
  inherited Create(ANodeName, AWWWBasePath);
  FPrototype := APrototype;

  ProtoInitial := Prototype;

  if (ProtoInitial is TVRMLExternalPrototype) and
     (TVRMLExternalPrototype(ProtoInitial).ReferencedPrototype <> nil) then
    { It's important to use ReferencedPrototype, not just current
      Prototype, in this case. That's because field's default values
      should be set by constructor (before parsing the specified
      fields), and TVRMLExternalPrototype doesn't have default values
      available.

      If ReferencedPrototype = nil (e.g. because couldn't
      be loaded) then Instantiate will not be able to instantiate
      it anyway (and will produce appropriate VRMLWarning). }
    ProtoInitial := TVRMLExternalPrototype(ProtoInitial).ReferencedPrototype;

  for Index := 0 to ProtoInitial.InterfaceDeclarations.Count - 1 do
  begin
    I := ProtoInitial.InterfaceDeclarations.Items[Index];
    I.CopyAndAddFieldOrEvent(Self);
  end;
end;

function TVRMLPrototypeNode.DeepCopyCreate(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
begin
  Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, WWWBasePath,
    { TODO: for now, we don't copy proto, instead simply passing the same
      proto reference. }
    Prototype);
end;

function TVRMLPrototypeNode.NodeTypeName: string;
begin
  Result := Prototype.Name;
end;

procedure TVRMLPrototypeNode.FieldOrEventHandleIsClause(
  Destination, Source: TVRMLFieldOrEvent;
  NewIsClauseNames: TDynStringArray);
var
  DestinationField, SourceField: TVRMLField;
  DestinationEvent, SourceEvent: TVRMLEvent;
  Route: TVRMLRoute;
const
  InEventName: array [boolean] of string = ( 'output', 'input' );
begin
  { When Source.IsClauseNames.Count <> 0, then we're expanded
    within the definition of another prototype.
    So Destination.IsClauseNames referers to Source
    (from current Prototype), but Source.IsClauseNames refers to yet
    another, enclosing, prototype (that will be expanded later --- for
    now we're within enclosing prototype definition).

    See comments in the interface for more. }

  NewIsClauseNames.AppendDynArray(Source.IsClauseNames);

  if Source is TVRMLField then
  begin
    Assert(Destination is TVRMLField);
    SourceField := Source as TVRMLField;
    DestinationField := Destination as TVRMLField;

    try
      DestinationField.AssignValue(SourceField);
      DestinationField.ValueFromIsClause := true;
    except
      on E: EVRMLFieldAssignInvalidClass do
      begin
        VRMLWarning(vwSerious, Format('Within prototype "%s", ' +
          'field of type %s (named "%s") references ' +
          '(by "IS" clause) field of different type %s (named "%s")',
          [Prototype.Name,
           DestinationField.VRMLTypeName,
           Destination.Name,
           SourceField.VRMLTypeName,
           Source.Name]));
      end;
      on E: EVRMLFieldAssign do
      begin
        VRMLWarning(vwSerious, Format('Error when expanding prototype "%s": ',
          [Prototype.Name]) + E.Message);
      end;
    end;
  end else
  if Source is TVRMLEvent then
  begin
    Assert(Destination is TVRMLEvent);
    SourceEvent := Source as TVRMLEvent;
    DestinationEvent := Destination as TVRMLEvent;

    if SourceEvent.InEvent <> DestinationEvent.InEvent then
    begin
      VRMLWarning(vwSerious, Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.Name,
          InEventName[DestinationEvent.InEvent],
          InEventName[SourceEvent.InEvent] ]));
      Exit;
    end;

    if SourceEvent.FieldClass <> DestinationEvent.FieldClass then
    begin
      VRMLWarning(vwSerious, Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.Name,
          DestinationEvent.FieldClass.VRMLTypeName,
          SourceEvent.FieldClass.VRMLTypeName ]));
      Exit;
    end;

    Route := TVRMLRoute.Create;
    Route.Internal := true;

    try
      if SourceEvent.InEvent then
      begin
        Route.SetSourceDirectly(SourceEvent);
        Route.SetDestinationDirectly(DestinationEvent);
      end else
      begin
        Route.SetSourceDirectly(DestinationEvent);
        Route.SetDestinationDirectly(SourceEvent);
      end;

      Routes.Add(Route);
    except
      FreeAndNil(Route);
      raise;
    end;
  end;
end;

procedure TVRMLPrototypeNode.InstantiateIsClauses(
  Node, Child: TVRMLNode);

  { In terminology of VRML/X3D specs,
    InstanceField/Event is the one in "prototype definition"
    (that is, inside prototype content) and
    OutField/Event is the one in "prototype declaration".

    This is where we implement table "Rules for mapping PROTOTYPE
    declarations to node instances" in specifications about
    "PROTO definition semantics". }

  procedure ExpandEvent(InstanceEvent: TVRMLEvent); forward;

  procedure ExpandField(InstanceField: TVRMLField);
  var
    OurField: TVRMLField;
    OurEvent: TVRMLEvent;
    OurFieldIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TDynStringArray;
  begin
    if InstanceField.IsClauseNames.Count <> 0 then
    begin
      NewIsClauseNames := TDynStringArray.Create;
      try
        for I := 0 to InstanceField.IsClauseNames.Count - 1 do
        begin
          IsClauseName := InstanceField.IsClauseNames.Items[I];
          OurFieldIndex := Fields.IndexOf(IsClauseName);
          if OurFieldIndex <> -1 then
          begin
            OurField := Fields[OurFieldIndex];
            FieldOrEventHandleIsClause(InstanceField, OurField, NewIsClauseNames);

            if InstanceField.Exposed and OurField.Exposed then
            begin
              { We have to "break" exposed fields inside TVRMLPrototypeNode,
                that is they will not automatically get changes and forwarding
                in event to out event. Reason:

                - This causes cycles in routes (remember that
                  FieldOrEventHandleIsClause creates internal routes
                  for "IS" between events.) When something sets event,
                  by sending to InstanceField.EventIn, then this will
                  be forwarded to InstanceField.EventOut. But also
                  will be forwarded to OurField.EventIn (this must be done,
                  in case other "IS" clauses interact with these exposed
                  events). Without breaking ExposedEventsLinked,
                  OurField.EventIn would forward this once again to
                  InstanceField.EventOut, causing all routes from
                  InstanceField.EventOut to the outside be detected as loops,
                  as another event travels through them.

                - Lastly, this is not needed. Copying field's value
                  is useless, as field's value in TVRMLPrototypeNode
                  is useless (field value must get to actual field in expanded
                  hierarchy).
              }
              OurField.ExposedEventsLinked := false;

              { Note that I pass here NewIsClauseNames, that is
                is our in/out event will have specialized IS clauses,
                they will be assigned to whole exposed field.
                This is Ok, as event may only point to another event
                higher in proto nesting. And exposed field may also refer
                to another event (inputOnly or outputOnly) higher in
                proto nesting. }

              FieldOrEventHandleIsClause(InstanceField.EventIn , OurField.EventIn , NewIsClauseNames);
              FieldOrEventHandleIsClause(InstanceField.EventOut, OurField.EventOut, NewIsClauseNames);
            end;
          end else
          if InstanceField.Exposed then
          begin
            { exposed field may also reference by IS clause the simple
              "only input" or "only output" event. }
            { TODO: untested case }
            OurEvent := AnyEvent(IsClauseName);
            if OurEvent <> nil then
            begin
              if OurEvent.InEvent then
                FieldOrEventHandleIsClause(InstanceField.EventIn , OurEvent, NewIsClauseNames) else
                FieldOrEventHandleIsClause(InstanceField.EventOut, OurEvent, NewIsClauseNames);
            end else
              VRMLWarning(vwSerious, Format('Within prototype "%s", exposed field "%s" references (by "IS" clause) non-existing field/event name "%s"',
                [Prototype.Name, InstanceField.Name, IsClauseName]));
          end else
            VRMLWarning(vwSerious, Format('Within prototype "%s", field "%s" references (by "IS" clause) non-existing field "%s"',
              [Prototype.Name, InstanceField.Name, IsClauseName]));
        end;

        InstanceField.IsClauseNames.Assign(NewIsClauseNames);
      finally FreeAndNil(NewIsClauseNames) end;
    end;

    if InstanceField.Exposed then
    begin
      (*Completely independent from the fact whether InstanceField.IsClause,
        it's exposed events may also have their own IS clauses, for example

          DirectionalLight {
            on IS light_on_initial_value
            set_on IS light_on_setter
            on_changed IS light_on_getter
          }
      *)

      ExpandEvent(InstanceField.EventIn);
      ExpandEvent(InstanceField.EventOut);
    end;
  end;

  procedure ExpandEvent(InstanceEvent: TVRMLEvent);
  var
    OurEvent: TVRMLEvent;
    OurEventIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TDynStringArray;
  begin
    if InstanceEvent.IsClauseNames.Count <> 0 then
    begin
      NewIsClauseNames := TDynStringArray.Create;
      try
        for I := 0 to InstanceEvent.IsClauseNames.Count - 1 do
        begin
          IsClauseName := InstanceEvent.IsClauseNames.Items[I];

          { Event from prototype definition can only correspond to the
            same event type of prototype declaration. It cannot reference
            implicit event (within exposed field) of prototype declaration.
            Which is good, since otherwise it would be difficult to implement
            (Self (TVRMLPrototypeNode) is used to keep events, but it doesn't
            keep actual field values (there are kept within actual expanded nodes).

            This also means that searching below by Events.IndexOf is Ok,
            no need to use AnyEvent to search. }

          OurEventIndex := Events.IndexOf(IsClauseName);
          if OurEventIndex <> -1 then
          begin
            OurEvent := Events[OurEventIndex];
            FieldOrEventHandleIsClause(InstanceEvent, OurEvent,
              NewIsClauseNames);
          end else
            VRMLWarning(vwSerious, Format('Within prototype "%s", event "%s" references (by "IS" clause) non-existing event "%s"',
              [Prototype.Name, InstanceEvent.Name, IsClauseName]));
        end;

        InstanceEvent.IsClauseNames.Assign(NewIsClauseNames);
      finally FreeAndNil(NewIsClauseNames) end;
    end;
  end;

var
  I: Integer;
begin
  { The NeedsInstantiateIsClause flag is needed for InstantiateIsClauses.
     By checking this flag (and setting back to @false after handling):

     1. We ensure that every node is expanded only once (otherwise,
        in case of funny DEF/USE usage, we could process the same node twice
        by InstantiateIsClauses).

     2. We avoid instantiating "IS" clauses in nodes outside the NodeCopy.
        Such nodes with "IS" clauses may be added by InstantiateIsClauses,
        when our own PROTO has fields with SFNode / MFNode type,
        and they contain "IS" clauses that should be handled by other
        (outer) proto. See e.g. kambi_vrml_test_suite/vrml_2/proto_nested_expand.wrl
        testcase.

     (The 2nd point could also be fixed by simply moving recursive call to
     Child.DirectEnumerateAll(@InstantiateIsClauses) to the beginning,
     not the end, of InstantiateIsClauses. But this would leave 1st point
     unfixed.) }

  if not Child.NeedsInstantiateIsClause then Exit;
  Child.NeedsInstantiateIsClause := false;

  for I := 0 to Child.Fields.Count - 1 do
    ExpandField(Child.Fields[I]);

  for I := 0 to Child.Events.Count - 1 do
    ExpandEvent(Child.Events[I]);

  Child.DirectEnumerateAll(@InstantiateIsClauses);
end;

procedure TVRMLPrototypeNode.PrepareInstantiateIsClause(
  Node, Child: TVRMLNode);
begin
  if not Child.NeedsInstantiateIsClause then
  begin
    Child.NeedsInstantiateIsClause := true;
    Child.DirectEnumerateAll(@PrepareInstantiateIsClause);
  end;
end;

function TVRMLPrototypeNode.Instantiate: TVRMLNode;

  procedure InstantiateNonExternalPrototype(Proto: TVRMLPrototype);
  var
    NodeCopy, NewPrototypeInstanceHelpers: TVRMLNode;
  begin
    { Even when Proto.Node is a wrapper (TVRMLRootNode_*),
      we want to copy the whole Proto.Node, instead of copying separately
      Proto.Node.SmartChildren[0], Proto.Node.SmartChildren[1] etc.
      This way, DEF / USE links, routes links (internal, for nested
      protos "IS" clauses, and non-internal) are preserved as they should. }

    NodeCopy := Proto.Node.DeepCopy;

    Assert(NodeCopy.PrototypeInstance =
      (NodeCopy.PrototypeInstanceSourceNode <> nil));
    Assert(NodeCopy.PrototypeInstance or
      (NodeCopy.PrototypeInstanceHelpers = nil));

    try
      { First, set NeedsInstantiateIsClause := true everywhere
        inside NodeCopy. (We can assume that NeedsInstantiateIsClause
        was @false everywhere before this.) }
      PrepareInstantiateIsClause(nil, NodeCopy);

      InstantiateIsClauses(nil, NodeCopy);
    except
      FreeAndNil(NodeCopy);
      raise;
    end;

    if (Proto.Node is TVRMLRootNode_1) or
       (Proto.Node is TVRMLRootNode_2) then
    begin
      if NodeCopy.SmartChildrenCount = 0 then
      begin
        { If exception occurs before NodeCopy is connected to Result,
          NodeCopy should be simply freed. }
        FreeAndNil(NodeCopy);
        raise EVRMLPrototypeInstantiateError.CreateFmt(
          'Prototype "%s" has no nodes, cannot instantiate',
          [Proto.Name]);
      end;

      { ExtractChild/Item methods were really invented specially for this case.

        We have to remove Result from NodeCopy, to avoid cycles
        (that can cause mem leaks) because Result.FPrototypeInstanceHelpers
        has to keep pointer to NodeCopy.

        At the same time, Result must not be freed here because of ref count = 0... }
      Result := NodeCopy.SmartExtractChild(0);

      Assert(Result.PrototypeInstance =
        (Result.PrototypeInstanceSourceNode <> nil));
      Assert(Result.PrototypeInstance or
        (Result.PrototypeInstanceHelpers = nil));

      { NewPrototypeInstanceHelpers is used to keep the rest of
        NodeCopy.SmartChildren[1...] that should accompany this node. }
      NewPrototypeInstanceHelpers := NodeCopy;
    end else
    begin
      Result := NodeCopy;
      NewPrototypeInstanceHelpers := nil;
    end;

    Result.NodeName := NodeName;

    (* Result and NodeCopy may come from another prototype.
       For example,

         PROTO SimpleText [
           inputOutput MFString string ""
         ] { Shape { geometry Text { string IS string } } }

         PROTO PressedText [
           inputOutput MFString string ""
         ] { SimpleText { string IS string } }

         PressedText { string "zero" }

       (this is a simplified part of ../../../kambi_vrml_test_suite/x3d/key_sensor.x3dv
       file). In such case, when PressedText, you may get Shape that already
       was expanded from SimpleText. So NodeCopy will have PrototypeInstance
       = true. Or NodeCopy may be TVRMLRootNode_1/2 wrapper, then it's
       first item (that is assigned to Result) will
       have PrototypeInstance = true.

       We have assertions above to check that their PrototypeInstance*
       properties are in valid state.

       Note that we take proper actions to not leak memory (so do
       not blindly overwrite Result.PrototypeInstanceSourceNode and ...Helpers).
       Also, existing Result.PrototypeInstanceSourceNode and ...Helpers
       must be retained, as they may contain routes for proper functioning
       of this nested prototype.

       What can we do? We have to keep them somewhere, but move out
       of the way. Current approach is to overuse
       TVRMLPrototypeNode.PrototypeInstanceSourceNode and ...Helpers
       fiels for this. (They should be empty right now, as one
       TVRMLPrototypeNode is expanded only once.)
       We store there information about nested prototype.
    *)
    if Result.PrototypeInstance then
    begin
      Assert(not PrototypeInstance);
      Assert(PrototypeInstanceSourceNode = nil);
      Assert(PrototypeInstanceHelpers = nil);

      FPrototypeInstance := Result.PrototypeInstance;
      FPrototypeInstanceSourceNode := Result.PrototypeInstanceSourceNode;
      FPrototypeInstanceHelpers := Result.PrototypeInstanceHelpers;
    end;

    { Note: set PrototypeInstance to @true *after* InstantiateIsClauses,
      otherwise InstantiateIsClauses would not enter this node.
      TODO: bad comment above? }
    Result.FPrototypeInstance := true;
    Result.FPrototypeInstanceSourceNode := Self;
    Result.FPrototypeInstanceHelpers := NewPrototypeInstanceHelpers;
  end;

  procedure InstantiateExternalPrototype(Proto: TVRMLExternalPrototype);
  begin
    if Proto.ReferencedPrototype = nil then
      raise EVRMLPrototypeInstantiateError.CreateFmt(
        'External prototype "%s" cannot be loaded, so cannot instantiate nodes using it',
        [Proto.Name]);

    { Note that we do not check whether ReferencedPrototype actually
      has the same fields/events as declared for externproto.
      Although when expanding IS clauses, missing declarations
      or incorrect types or field/event will be catched, so the necessary
      things will be checked when expanding. }

    InstantiateNonExternalPrototype(Proto.ReferencedPrototype);
  end;

begin
  if Prototype is TVRMLPrototype then
    InstantiateNonExternalPrototype(Prototype as TVRMLPrototype) else
  if Prototype is TVRMLExternalPrototype then
    InstantiateExternalPrototype(Prototype as TVRMLExternalPrototype) else
    raise EVRMLPrototypeInstantiateError.CreateFmt(
      'Cannot instantiate prototype "%s": '+
      'unknown prototype class %s', [Prototype.Name, Prototype.ClassName]);
end;

{ TVRMLPrototypeBase --------------------------------------------------------- }

constructor TVRMLPrototypeBase.Create;
begin
  inherited;
  FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
end;

destructor TVRMLPrototypeBase.Destroy;
begin
  FreeWithContentsAndNil(FInterfaceDeclarations);
  inherited;
end;

procedure TVRMLPrototypeBase.ParseInterfaceDeclarations(ExternalProto: boolean;
  Lexer: TVRMLLexer; Names: TVRMLNames);
var
  I: TVRMLInterfaceDeclaration;
begin
  while Lexer.Token <> vtCloseSqBracket do
  begin
    I := TVRMLInterfaceDeclaration.Create(nil);
    InterfaceDeclarations.Add(I);

    if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(AllAccessTypes)) then
    begin
      I.Parse(Lexer, Names, not ExternalProto, false);
    end else
      raise EVRMLParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
  end;

  { eat "]" token }
  Lexer.NextToken;

  FWWWBasePath := Names.WWWBasePath;
end;

procedure TVRMLPrototypeBase.ParseInterfaceDeclarationsXML(ExternalProto: boolean;
  Element: TDOMElement; Names: TVRMLNames);
var
  I: TVRMLInterfaceDeclaration;
  Iter: TXMLElementIterator;
begin
  Iter := TXMLElementIterator.Create(Element);
  try
    while Iter.GetNext do
    begin
      if Iter.Current.TagName = 'field' then
      begin
        I := TVRMLInterfaceDeclaration.Create(nil);
        InterfaceDeclarations.Add(I);
        I.ParseXML(Iter.Current, Names, not ExternalProto);
      end else
        VRMLWarning(vwSerious, 'X3D XML: only <field> elements expected in prototype interface');
    end;
  finally FreeAndNil(Iter) end;
end;

procedure TVRMLPrototypeBase.SaveInterfaceDeclarationsToStream(
  SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TVRMLNodeNames;
  ExternalProto: boolean);
var
  I: Integer;
begin
  SaveProperties.Writeln(Name + ' [');
  SaveProperties.IncIndent;
  for I := 0 to InterfaceDeclarations.Count - 1 do
  begin
    InterfaceDeclarations.Items[I].IDeclSaveToStream(
      SaveProperties, NodeNames, not ExternalProto);
  end;
  SaveProperties.DecIndent;
  SaveProperties.WritelnIndent(']');
end;

{ TVRMLPrototype ------------------------------------------------------------- }

destructor TVRMLPrototype.Destroy;
begin
  FreeAndNil(FNode);
  inherited;
end;

procedure TVRMLPrototype.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
var
  OldNames: TVRMLNames;
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(false, Lexer, Names);

  Lexer.CheckTokenIs(vtOpenCurlyBracket);

  Lexer.NextToken;
  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Names.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldNames := Names;
  Names := TVRMLNames.Create(true,
    OldNames.WWWBasePath, OldNames.VRMLVerMajor, OldNames.VRMLVerMinor);
  try
    Names.Prototypes.Assign(OldNames.Prototypes);
    FNode := ParseVRMLStatements(Lexer, Names, vtCloseCurlyBracket, false);
  finally
    FreeAndNil(Names);
    Names := OldNames;
  end;

  { consume last vtCloseCurlyBracket, ParseVRMLStatements doesn't do it }
  Lexer.NextToken;

  Names.Prototypes.Bind(Self);
end;

procedure TVRMLPrototype.ParseXML(Element: TDOMElement; Names: TVRMLNames);
var
  OldNames: TVRMLNames;
  NewName: string;
  E: TDOMElement;
begin
  WWWBasePath := Names.WWWBasePath;

  if DOMGetAttribute(Element, 'name', NewName) then
    Name := NewName else
    raise EX3DXmlError.Create('Missing "name" for <ProtoDeclare> element');

  E := DOMGetChildElement(Element, 'ProtoInterface', false);
  if E <> nil then
    ParseInterfaceDeclarationsXML(false, E, Names);

  E := DOMGetChildElement(Element, 'ProtoBody', false);
  if E = nil then
    raise EX3DXmlError.Create('Missing <ProtoBody> inside <ProtoDeclare> element');

  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Names.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldNames := Names;
  Names := TVRMLNames.Create(true,
    OldNames.WWWBasePath, OldNames.VRMLVerMajor, OldNames.VRMLVerMinor);
  try
    Names.Prototypes.Assign(OldNames.Prototypes);
    Node := ParseVRMLStatements(E, false, nil, Names);
  finally
    FreeAndNil(Names);
    Names := OldNames;
  end;

  Names.Prototypes.Bind(Self);
end;

procedure TVRMLPrototype.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
var
  OldNodeNames: TVRMLNodeNames;
begin
  SaveProperties.WriteIndent('PROTO ');

  SaveInterfaceDeclarationsToStream(SaveProperties, NodeNames as TVRMLNodeNames, false);

  { Inside prototype has it's own DEF/USE scope. }
  OldNodeNames := NodeNames as TVRMLNodeNames;
  NodeNames := TVRMLNodeNames.Create(false);
  try
    SaveProperties.WritelnIndent('{');
    { Node may be TVRMLRootNode_* here, that's OK,
      TVRMLRootNode_*.SaveToStream will magically handle this right. }
    SaveProperties.IncIndent;
    Node.SaveToStream(SaveProperties, NodeNames);
    SaveProperties.DecIndent;
    SaveProperties.WritelnIndent('}');
  finally
    FreeAndNil(NodeNames);
    NodeNames := OldNodeNames;
  end;
end;

{ TVRMLExternalPrototype ----------------------------------------------------- }

constructor TVRMLExternalPrototype.Create;
begin
  inherited;
  FURLList := TMFString.Create(nil, '', []);
end;

destructor TVRMLExternalPrototype.Destroy;
begin
  UnloadReferenced;
  FreeAndNil(FURLList);
  inherited;
end;

procedure TVRMLExternalPrototype.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(true, Lexer, Names);

  URLList.Parse(Lexer, Names, false);

  Names.Prototypes.Bind(Self);

  LoadReferenced;
end;

procedure TVRMLExternalPrototype.ParseXML(Element: TDOMElement; Names: TVRMLNames);
var
  NewName, URLListValue: string;
begin
  WWWBasePath := Names.WWWBasePath;

  if DOMGetAttribute(Element, 'name', NewName) then
    Name := NewName else
    raise EX3DXmlError.Create('Missing "name" for <ExternProtoDeclare> element');

  ParseInterfaceDeclarationsXML(true, Element, Names);

  if DOMGetAttribute(Element, 'url', URLListValue) then
    URLList.ParseXMLAttribute(URLListValue, Names) else
    raise EX3DXmlError.Create('Missing "url" for <ExternProtoDeclare> element');

  Names.Prototypes.Bind(Self);

  LoadReferenced;
end;

procedure TVRMLExternalPrototype.SaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
begin
  SaveProperties.WriteIndent('EXTERNPROTO ');

  SaveInterfaceDeclarationsToStream(SaveProperties,
    NodeNames as TVRMLNodeNames, true);

  { SaveProperties.NodeNames will be ignored by URLList
    (TMFString.SaveToStream), don't worry about it. }

  URLList.SaveToStream(SaveProperties, NodeNames);
end;

procedure TVRMLExternalPrototype.LoadReferenced;

  procedure LoadInterfaceDeclarationsValues;
  var
    IIndex: Integer;
    I: TVRMLInterfaceDeclaration;
    ReferencedField: TVRMLField;
  begin
    { We should load default values for our fields now,
      since InterfaceDeclaration of external prototype doesn't
      specify default values. }
    for IIndex := 0 to InterfaceDeclarations.Count - 1 do
    begin
      I := InterfaceDeclarations[IIndex];
      if I.Field <> nil then
      begin
        ReferencedField := ReferencedPrototype.InterfaceDeclarations.
          TryFindFieldName(I.Field.Name);
        if ReferencedField <> nil then
        begin
          try
            I.Field.AssignValue(ReferencedField);
          except
            on E: EVRMLFieldAssign do
            begin
              VRMLWarning(vwSerious, Format(
                'Error when linking external prototype "%s" with prototype "%s": ',
                [Name, ReferencedPrototype.Name]) + E.Message);
            end;
          end;
        end else
          VRMLWarning(vwSerious, Format('Prototype "%s" referenced by external ' +
            'prototype "%s" doesn''t have field "%s"',
            [ReferencedPrototype.Name, Name, I.Field.Name]));
      end;
    end;
  end;

var
  PrototypeNames: TVRMLPrototypeNames;

  function LoadFromExternalVRML(const RelativeURL: string): boolean;
  var
    URL: string;

    procedure ProtoWarning(const S: string);
    begin
      VRMLWarning(vwIgnorable, Format('Cannot load external prototype from URL "%s": ',
        [URL]) + S);
    end;

    { Find PROTO (but not EXTERNPROTO) with matching Name.
      Name is ignored if ''.
      @nil if not found. }
    function TryFindProtoNonExternal(const Name: string): TVRMLPrototype;
    var
      I: Integer;
    begin
      for I := 0 to PrototypeNames.Count - 1 do
        if PrototypeNames.Objects[I] is TVRMLPrototype then
        begin
          Result := TVRMLPrototype(PrototypeNames.Objects[I]);
          if (Name = '') or (Result.Name = Name) then
            Exit;
        end;
      Result := nil;
    end;

  var
    Anchor: string;
  begin
    Result := false;

    { TODO: in case of prototype libraries, it's wasteful to load URL each time
      for each external prototype. We should have some cache of prototypes. }

    URL := CombinePaths(WWWBasePath, RelativeURL);
    URLExtractAnchor(URL, Anchor);
    try
      ReferencedPrototypeNode := LoadVRML(URL, false, PrototypeNames);
    except
      on E: Exception do
      begin
        ProtoWarning(E.Message);
        Exit;
      end;
    end;

    FReferencedPrototype := TryFindProtoNonExternal(Anchor);
    if FReferencedPrototype = nil then
    begin
      FreeAndNil(ReferencedPrototypeNode);
      if Anchor = '' then
        ProtoWarning('No PROTO found') else
        ProtoWarning(Format('No PROTO named "%s" found', [Anchor]));
      Exit;
    end;

    Result := true;

    LoadInterfaceDeclarationsValues;
  end;

  function LoadFromURN(const URN: string): boolean;
  begin
    FReferencedClass := NodesManager.URNToClass(URN);
    Result := ReferencedClass <> nil;
    if not Result then
      VRMLWarning(vwSerious, Format('Unknown node URN "%s"', [URN]));
  end;

var
  I: Integer;
  S: string;
  Loaded: boolean;
begin
  UnloadReferenced;

  PrototypeNames := TVRMLPrototypeNames.Create;
  try
    for I := 0 to URLList.Count - 1 do
    begin
      S := URLList.Items.Items[I];
      if IsPrefix('urn:', S) then
        Loaded := LoadFromURN(S) else
        Loaded := LoadFromExternalVRML(S);
      if Loaded then
        Break;
    end;
  finally FreeAndNil(PrototypeNames); end;
end;

procedure TVRMLExternalPrototype.UnloadReferenced;
begin
  { FReferencedPrototype will be freed as part of ReferencedPrototypeNode }
  FReferencedPrototype := nil;

  FreeAndNil(ReferencedPrototypeNode);

  FReferencedClass := nil;
end;

{ TNodesManager ------------------------------------------------------------ }

constructor TNodesManager.Create;
begin
  inherited;
  FRegistered := TStringListCaseSens.Create;
end;

destructor TNodesManager.Destroy;
begin
  FRegistered.Free;
  inherited;
end;

procedure TNodesManager.RegisterNodeClass(NodeClass: TVRMLNodeClass);
begin
  if NodeClass.ClassNodeTypeName = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassNodeTypeName so it cannot be registered in TNodesManager');

  if FRegistered.IndexOfObject(TObject(Pointer(NodeClass))) <> -1 then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+
      ' was already registered in TNodesManager');

  FRegistered.AddObject(NodeClass.ClassNodeTypeName, TObject(Pointer(NodeClass)));
end;

procedure TNodesManager.RegisterNodeClasses(
  const NodeClasses: array of TVRMLNodeClass);
var
  I: Integer;
begin
  for i := 0 to High(NodeClasses) do RegisterNodeClass(NodeClasses[i]);
end;

procedure TNodesManager.UnRegisterNodeClass(NodeClass: TVRMLNodeClass;
  ErrorIfNotRegistered: boolean);
var i: Integer;
begin
  if NodeClass.ClassNodeTypeName = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassNodeTypeName so it cannot be unregistered (or even registered) '+
      'in TNodesManager');

  i := FRegistered.IndexOfObject(TObject(Pointer(NodeClass)));
  if i <> - 1 then
    FRegistered.Delete(i) else
  if ErrorIfNotRegistered then
    ENodesManagerError.Create('Node class "' + NodeClass.ClassName +
      '" was not registered, so you cannot unregister it');
end;

function TNodesManager.NodeTypeNameToClass(const ANodeTypeName: string;
  const VerMajor, VerMinor: Integer): TVRMLNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TVRMLNodeClass(FRegistered.Objects[I]);
    if (FRegistered[I] = ANodeTypeName) and
       Result.ForVRMLVersion(VerMajor, VerMinor) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.URNToClass(const URN: string): TVRMLNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TVRMLNodeClass(FRegistered.Objects[I]);
    if Result.URNMatching(URN) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.GetRegistered(Index: Integer): TVRMLNodeClass;
begin
  Result := TVRMLNodeClass(FRegistered.Objects[Index]);
end;

function TNodesManager.RegisteredCount: Cardinal;
begin
  Result := FRegistered.Count;
end;

{ TVRMLRoute ----------------------------------------------------------------- }

constructor TVRMLRoute.Create;
begin
  inherited;
  ResetLastEventTime;
end;

destructor TVRMLRoute.Destroy;
begin
  { We have to unset, to call
    DestructionNotifications.DeleteFirstEqual(...) on our nodes before
    we get destroyed. Otherwise nodes would have invalid references
    on DestructionNotifications list. }

  UnsetEnding(FSourceNode     , FSourceEvent     , false);
  UnsetEnding(FDestinationNode, FDestinationEvent, true);
  inherited;
end;

procedure TVRMLRoute.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
var
  SourceNodeName, SourceEventName: string;
  DestinationNodeName, DestinationEventName: string;
begin
  { We don't use NextTokenForceVTName here, as then the dot "."
    is treated like part the VRML name. So this assumes that VRML node names
    are really correct VRML names. }

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  SourceNodeName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  SourceEventName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIsKeyword(vkTO);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  DestinationNodeName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  DestinationEventName := Lexer.TokenName;

  Lexer.NextToken;

  SetSource     (SourceNodeName     , SourceEventName     , Names);
  SetDestination(DestinationNodeName, DestinationEventName, Names);
end;

procedure TVRMLRoute.ParseXML(Element: TDOMElement; Names: TVRMLNames);

  function RequiredAttrib(const AttrName: string): string;
  begin
    if not DOMGetAttribute(Element, AttrName, Result) then
    begin
      VRMLWarning(vwSerious, 'Missing ROUTE ' + AttrName + ' attribute');
      Result := '';
    end;
  end;

var
  SourceNodeName, SourceEventName: string;
  DestinationNodeName, DestinationEventName: string;
begin
  SourceNodeName := RequiredAttrib('fromNode');
  SourceEventName := RequiredAttrib('fromField');
  DestinationNodeName := RequiredAttrib('toNode');
  DestinationEventName := RequiredAttrib('toField');

  SetSource     (SourceNodeName     , SourceEventName     , Names);
  SetDestination(DestinationNodeName, DestinationEventName, Names);
end;

procedure TVRMLRoute.UnsetEnding(
  var Node: TVRMLNode; var Event: TVRMLEvent;
  const DestEnding: boolean;
  RemoveFromDestructionNotification: boolean);
begin
  if (Event <> nil) and (not DestEnding) then
    Event.OnReceive.Remove(@EventReceive);

  if Node <> nil then
  begin
    if RemoveFromDestructionNotification then
      Node.DestructionNotifications.Remove(@DestructionNotification);
    Node := nil;
  end;

  Event := nil;
end;

procedure TVRMLRoute.EventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Assert(Event = SourceEvent);

  { Follow ROUTE only when LastEventTime is older than current Time.
    This avoids loops in ROUTEs, following VRML / X3D specs. }
  if Time > LastEventTime then
  begin
    LastEventTime := Time;
    if DestinationEvent <> nil then
      DestinationEvent.Send(Value, Time);
  end else
  if Log then
    WritelnLog('VRMLRoute', Format(
      'Route from %s.%s ignored another event at <= timestamp (%f, while last event was on %f). Potential routes loop avoided',
      [ SourceNode.NodeName, SourceEvent.Name,
        Time.Seconds, LastEventTime.Seconds ]));
end;

procedure TVRMLRoute.ResetLastEventTime;
begin
  LastEventTime := OldestVRMLTime;
end;

type
  ERouteSetEndingError = class(EVRMLError);

const
  DestEndingNames: array [boolean] of string =
  ('source', 'destination');

procedure TVRMLRoute.SetEndingInternal(
  const Node: TVRMLNode;
  const FieldOrEvent: TVRMLFieldOrEvent;
  var Event: TVRMLEvent;
  const DestEnding: boolean);
var
  ExposedField: TVRMLField;
begin
  if FieldOrEvent is TVRMLField then
  begin
    ExposedField := TVRMLField(FieldOrEvent);
    if not ExposedField.Exposed then
      raise ERouteSetEndingError.CreateFmt('Route %s specifies field "%s" (for node "%s"), but this is not an exposed field (cannot generate/receive events)',
        [ DestEndingNames[DestEnding], FieldOrEvent.Name, Node.NodeName ]);
    Event := ExposedField.ExposedEvents[DestEnding];
  end else
  begin
    Assert(FieldOrEvent is TVRMLEvent);
    Event := TVRMLEvent(FieldOrEvent);
  end;

  if (not Internal) and (Event.InEvent <> DestEnding) then
  begin
    if DestEnding then
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: destination of the route (%s, type %s) can only be output event',
        [ Event.Name, Event.FieldClass.VRMLTypeName ]) else
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: source of the route (%s, type %s) can only be input event',
        [ Event.Name, Event.FieldClass.VRMLTypeName ]);
  end;

  if (SourceEvent <> nil) and
     (DestinationEvent <> nil) and
     (SourceEvent.FieldClass <> DestinationEvent.FieldClass) and
     { destination field can be XFAny (for Logger.write) as an exception. }
     (not (DestinationEvent.FieldClass = TVRMLField)) then
    raise ERouteSetEndingError.CreateFmt('Route has different event types for source (%s, type %s) and destination (%s, type %s)',
      [ SourceEvent     .Name, SourceEvent     .FieldClass.VRMLTypeName,
        DestinationEvent.Name, DestinationEvent.FieldClass.VRMLTypeName ]);

  if (Event <> nil) and (not DestEnding) then
    Event.OnReceive.Add(@EventReceive);
end;

procedure TVRMLRoute.SetEnding(const NodeName, FieldOrEventName: string;
  Names: TVRMLNames;
  var Node: TVRMLNode; var Event: TVRMLEvent;
  const DestEnding: boolean);
var
  N: TVRMLNode;
  FieldOrEvent: TVRMLFieldOrEvent;
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    N := Names.Nodes.Bound(NodeName);
    if N = nil then
      N := Names.Imported.Bound(NodeName);
    if N = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s node name "%s" not found',
        [ DestEndingNames[DestEnding], NodeName ]);

    Node := N;
    if Node.PrototypeInstanceSourceNode <> nil then
    begin
      Node := Node.PrototypeInstanceSourceNode;
      { Actually, SearchNode.PrototypeInstanceSourceNode may also be non-nil,
        in case we have nested prototype. But it doesn't matter,
        that is we don't want to go all the way down to find the
        final PrototypeInstanceSourceNode --- we only want to see the first
        PrototypeInstanceSourceNode. }
    end;

    Node.DestructionNotifications.Add(@DestructionNotification);

    FieldOrEvent := Node.FieldOrEvent(FieldOrEventName);
    if FieldOrEvent = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s field/event name "%s" (for node "%s", type "%s") not found',
        [ DestEndingNames[DestEnding], FieldOrEventName, NodeName, Node.NodeTypeName ]);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      VRMLWarning(vwSerious, E.Message);
    end;
  end;
end;

procedure TVRMLRoute.SetSource(
  const SourceNodeName, SourceFieldOrEventName: string;
  Names: TVRMLNames);
begin
  SetEnding(SourceNodeName, SourceFieldOrEventName,
    Names, FSourceNode, FSourceEvent, false);
end;

procedure TVRMLRoute.SetDestination(
  const DestinationNodeName, DestinationFieldOrEventName: string;
  Names: TVRMLNames);
begin
  SetEnding(DestinationNodeName, DestinationFieldOrEventName,
    Names, FDestinationNode, FDestinationEvent, true);
end;

procedure TVRMLRoute.SetEndingDirectly(
  const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent;
  var Node: TVRMLNode; var Event: TVRMLEvent;
  const DestEnding: boolean);
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    Node := NewNode;
    Node.DestructionNotifications.Add(@DestructionNotification);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      VRMLWarning(vwSerious, E.Message);
    end;
  end;
end;

procedure TVRMLRoute.SetSourceDirectly(
  const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FSourceNode, FSourceEvent,
    false);
end;

procedure TVRMLRoute.SetDestinationDirectly(
  const NewNode: TVRMLNode; const FieldOrEvent: TVRMLFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FDestinationNode, FDestinationEvent,
    true);
end;

procedure TVRMLRoute.SetSourceDirectly(const FieldOrEvent: TVRMLFieldOrEvent);
begin
  SetSourceDirectly(FieldOrEvent.ParentNode as TVRMLNode, FieldOrEvent);
end;

procedure TVRMLRoute.SetDestinationDirectly(
  const FieldOrEvent: TVRMLFieldOrEvent);
begin
  SetDestinationDirectly(FieldOrEvent.ParentNode as TVRMLNode, FieldOrEvent);
end;

type
  EVRMLRouteSaveError = class(EVRMLError);

procedure TVRMLRoute.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
var
  Output: string;

  procedure WriteEnding(Node: TVRMLNode;
    Event: TVRMLEvent; const S: string);
  var
    Index: Integer;
    BoundNode: TVRMLNode;
  begin
    { Check Node }
    if Node = nil then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node not assigned (look for warnings when reading this VRML file)', [S]);
    if Node.NodeName = '' then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node not named', [S]);

    Index := (NodeNames as TVRMLNodeNames).IndexOf(Node.NodeName);
    if Index = -1 then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound',
        [S, Node.NodeName]);

    BoundNode := (NodeNames as TVRMLNodeNames).Objects[Index] as TVRMLNode;
    { Just like when setting node by TVRMLRoute.SetEnding:
      we actually keep the Node that contains the route, which is
      sometimes TVRMLPrototypeNode hidden inside PrototypeInstanceSourceNode. }
    if BoundNode.PrototypeInstanceSourceNode <> nil then
      BoundNode := BoundNode.PrototypeInstanceSourceNode;
    if BoundNode <> Node then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound (another node bound to the same name)',
        [S, Node.NodeName]);

    Output += Node.NodeName + '.';

    { Check Event }
    if Event = nil then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s event not assigned', [S]);

    { Check we have a name. }
    if Event.Name = '' then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s event not named', [S]);
    Output += Event.Name;
  end;

begin
  if Internal then Exit;

  try
    Output := 'ROUTE ';
    WriteEnding(SourceNode     , SourceEvent     , 'source'     );
    Output += ' TO ';
    WriteEnding(DestinationNode, DestinationEvent, 'destination');
    SaveProperties.WritelnIndent(Output);
  except
    on E: EVRMLRouteSaveError do
    begin
      VRMLWarning(vwSerious, E.Message);
    end;
  end;
end;

procedure TVRMLRoute.DestructionNotification(Node: TVRMLNode);
begin
  { UnsetEnding is called with RemoveFromDestructionNotification = false.
    Reason:
    1. Removing from DestructionNotification is not needed,
       since the Node is destroyed anyway, along with it's
       DestructionNotification list.
    2. Moreover, right now we probably iterate over DestructionNotification,
       so removing items from it is very bad idea (indexes shift,
       pointers change if reallocation occurs). }

  if Node = FSourceNode then
    UnsetEnding(FSourceNode     , FSourceEvent     , false, false);

  if Node = FDestinationNode then
    UnsetEnding(FDestinationNode, FDestinationEvent, true , false);
end;

function TVRMLRoute.DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLRoute;
var
  NewSourceNode, NewDestinationNode: TVRMLNode;
  NewSourceEvent, NewDestinationEvent: TVRMLEvent;
begin
  Result := TVRMLRoute.Create;
  Result.Internal := Internal;

  if (SourceNode <> nil) and
     (SourceEvent <> nil) then
  begin
    NewSourceNode := CopyState.DeepCopy(SourceNode);
    NewSourceEvent := NewSourceNode.AnyEvent(SourceEvent.Name);
    if NewSourceEvent = nil then
      raise EInternalError.CreateFmt('Route source node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewSourceNode.NodeName, NewSourceNode.NodeTypeName,
	  NewSourceEvent.Name ]);
    Result.SetSourceDirectly(NewSourceNode, NewSourceEvent);
  end;

  if (DestinationNode <> nil) and
     (DestinationEvent <> nil) then
  begin
    NewDestinationNode := CopyState.DeepCopy(DestinationNode);
    NewDestinationEvent := NewDestinationNode.AnyEvent(DestinationEvent.Name);
    if NewDestinationEvent = nil then
      raise EInternalError.CreateFmt('Route destination node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewDestinationNode.NodeName, NewDestinationNode.NodeTypeName,
	  NewDestinationEvent.Name ]);
    Result.SetDestinationDirectly(NewDestinationNode, NewDestinationEvent);
  end;
end;

{ TVRMLImport ---------------------------------------------------------------- }

procedure TVRMLImport.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'Inline node name');
  InlineNodeName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'imported node name');
  ImportedNodeName := Lexer.TokenName;

  Lexer.NextToken;
  if Lexer.TokenIsKeyword(vkAS) then
  begin
    Lexer.NextToken;
    Lexer.CheckTokenIs(vtName, 'alias for imported node name');
    ImportedNodeAlias := Lexer.TokenName;

    Lexer.NextToken;
  end else
    ImportedNodeAlias := ImportedNodeName;

  Names.DoImport(Self);
end;

procedure TVRMLImport.ParseXML(Element: TDOMElement; Names: TVRMLNames);
begin
  if not DOMGetAttribute(Element, 'inlineDEF', InlineNodeName) then
  begin
    VRMLWarning(vwSerious, 'Missing IMPORT "inlineDEF" attribute');
    Exit;
  end;

  if not DOMGetAttribute(Element, 'importedDEF', ImportedNodeName) then
  begin
    VRMLWarning(vwSerious, 'Missing IMPORT "importedDEF" attribute, looking for older "exportedDEF"');
    if not DOMGetAttribute(Element, 'exportedDEF', ImportedNodeName) then
    begin
      VRMLWarning(vwSerious, 'Missing IMPORT attribute: neighter "importedDEF" nor older "exportedDEF" found');
      Exit;
    end;
  end;

  if not DOMGetAttribute(Element, 'AS', ImportedNodeAlias) then
    ImportedNodeAlias := ImportedNodeName;

  Names.DoImport(Self);
end;

procedure TVRMLImport.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
begin
  SaveProperties.WriteIndent('IMPORT ' + InlineNodeName + '.' + ImportedNodeName);
  if ImportedNodeName <> ImportedNodeAlias then
    SaveProperties.Write(' AS ' + ImportedNodeAlias);
  SaveProperties.Writeln('');
end;

function TVRMLImport.DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLImport;
begin
  Result := TVRMLImport.Create;
  Result.InlineNodeName := InlineNodeName;
  Result.ImportedNodeName := ImportedNodeName;
  Result.ImportedNodeAlias := ImportedNodeAlias;
end;

{ TVRMLExport ---------------------------------------------------------------- }

procedure TVRMLExport.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'exported node name');
  ExportedNodeName := Lexer.TokenName;

  Lexer.NextToken;
  if Lexer.TokenIsKeyword(vkAS) then
  begin
    Lexer.NextToken;
    Lexer.CheckTokenIs(vtName, 'alias for exported node name');
    ExportedNodeAlias := Lexer.TokenName;

    Lexer.NextToken;
  end else
    ExportedNodeAlias := ExportedNodeName;

  Names.DoExport(Self);
end;

procedure TVRMLExport.ParseXML(Element: TDOMElement; Names: TVRMLNames);
begin
  if not DOMGetAttribute(Element, 'localDEF', ExportedNodeName) then
  begin
    VRMLWarning(vwSerious, 'Missing EXPORT "localDEF" attribute');
    Exit;
  end;

  if not DOMGetAttribute(Element, 'AS', ExportedNodeAlias) then
    ExportedNodeAlias := ExportedNodeName;

  Names.DoExport(Self);
end;

procedure TVRMLExport.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
begin
  SaveProperties.WriteIndent('EXPORT ' + ExportedNodeName);
  if ExportedNodeName <> ExportedNodeAlias then
    SaveProperties.Write(' AS ' + ExportedNodeAlias);
  SaveProperties.Writeln('');
end;

function TVRMLExport.DeepCopy(CopyState: TVRMLNodeDeepCopyState): TVRMLExport;
begin
  Result := TVRMLExport.Create;
  Result.ExportedNodeName := ExportedNodeName;
  Result.ExportedNodeAlias := ExportedNodeAlias;
end;

{ TVRMLNodeNames ----------------------------------------------------------- }

constructor TVRMLNodeNames.Create(const AAutoRemove: boolean);
begin
  inherited Create;
  FAutoRemove := AAutoRemove;
  if AutoRemove then
    AnyNodeDestructionNotifications.Add(@DestructionNotification);
end;

destructor TVRMLNodeNames.Destroy;
begin
  if AutoRemove then
    AnyNodeDestructionNotifications.Remove(@DestructionNotification);
  inherited;
end;

procedure TVRMLNodeNames.DestructionNotification(Node: TVRMLNode);
var
  I: Integer;
begin
  I := IndexOfObject(Node);
  if I >= 0 then
    Delete(I);
end;

procedure TVRMLNodeNames.Bind(Node: TVRMLNode; const BindToName: string);
var
  I: Integer;
begin
  if BindToName <> '' then
  begin
    I := IndexOf(BindToName);
    if I <> -1 then
      Objects[I] := Node else
      AddObject(BindToName, Node);
  end;
end;

procedure TVRMLNodeNames.Bind(Node: TVRMLNode);
begin
  Bind(Node, Node.NodeName);
end;

function TVRMLNodeNames.Bound(const Name: string): TVRMLNode;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
    Result := Objects[I] as TVRMLNode else
    Result := nil;
end;

function TVRMLNodeNames.Bound(Node: TVRMLNode): boolean;
begin
  Result := IndexOfObject(Node) <> -1;
end;

{ TVRMLPrototypeNames -------------------------------------------------------- }

procedure TVRMLPrototypeNames.Bind(Proto: TVRMLPrototypeBase);
var
  I: Integer;
begin
  I := IndexOf(Proto.Name);
  if I <> - 1 then
    Objects[I] := Proto else
    AddObject(Proto.Name, Proto);
end;

function TVRMLPrototypeNames.Bound(const Name: string): TVRMLPrototypeBase;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
    Result := Objects[I] as TVRMLPrototypeBase else
    Result := nil;
end;

{ TVRMLImportableNames ------------------------------------------------------- }

destructor TVRMLImportableNames.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited;
end;

procedure TVRMLImportableNames.Bind(const InlineName: string; Exported: TVRMLNodeNames);
var
  I: Integer;
begin
  I := IndexOf(InlineName);
  if I = -1 then
    AddObject(InlineName, Exported) else
  begin
    Objects[I].Free;
    Objects[I] := Exported;
  end;
end;

{ TVRMLNames ----------------------------------------------------------------- }

constructor TVRMLNames.Create(const AAutoRemoveNodes: boolean;
  const AWWWBasePath: string;
  const AVRMLVerMajor, AVRMLVerMinor: Integer);
begin
  inherited Create;
  FWWWBasePath := AWWWBasePath;
  FVRMLVerMajor := AVRMLVerMajor;
  FVRMLVerMinor := AVRMLVerMinor;
  FNodes := TVRMLNodeNames.Create(AAutoRemoveNodes);
  FPrototypes := TVRMLPrototypeNames.Create;
  FImported := TVRMLNodeNames.Create(AAutoRemoveNodes);
  FExported := TVRMLNodeNames.Create(AAutoRemoveNodes);
  FImportable := TVRMLImportableNames.Create;
end;

destructor TVRMLNames.Destroy;
begin
  FreeAndNil(FNodes);
  FreeAndNil(FPrototypes);
  FreeAndNil(FImported);
  FreeAndNil(FExported);
  FreeAndNil(FImportable);
  inherited;
end;

procedure TVRMLNames.DoExport(E: TVRMLExport);
var
  ExportedNode: TVRMLNode;
begin
  ExportedNode := Nodes.Bound(E.ExportedNodeName);
  if ExportedNode = nil then
  begin
    VRMLWarning(vwSerious, Format('Exported node name "%s" not found', [E.ExportedNodeName]));
    Exit;
  end;

  Exported.Bind(ExportedNode, E.ExportedNodeAlias);
end;

procedure TVRMLNames.DoImport(I: TVRMLImport);
var
  ImportedNames: TVRMLNodeNames;
  ImportedNamesIndex: Integer;
  ImportedNode: TVRMLNode;
begin
  ImportedNamesIndex := Importable.IndexOf(I.InlineNodeName);
  if ImportedNamesIndex = -1 then
  begin
    VRMLWarning(vwSerious, Format('Inline node name "%s" not found (or nothing was EXPORTed from it), cannot IMPORT', [I.InlineNodeName]));
    Exit;
  end;

  ImportedNames := Importable.Objects[ImportedNamesIndex] as TVRMLNodeNames;

  ImportedNode := ImportedNames.Bound(I.ImportedNodeName) as TVRMLNode;
  if ImportedNode = nil then
  begin
    VRMLWarning(vwSerious, Format('Imported node name "%s" not found in inline "%s"', [I.ImportedNodeName, I.InlineNodeName]));
    Exit;
  end;

  Imported.Bind(ImportedNode, I.ImportedNodeAlias);
end;

{ global procedures ---------------------------------------------------------- }

procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    StateNodes.Nodes[SN] := TraverseStateLastNodesClasses[SN].Create('', '');
end;

procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    FreeAndNil(StateNodes.Nodes[SN]);
end;

function KeyRange(Key: TDynSingleArray;
  const Fraction: Single; out T: Single): Integer;
var
  A, B: Integer;
begin
  Assert(Key.Count > 0);

  if Fraction <= Key.Items[0] then
    Result := 0 else
  if Fraction >= Key.Items[Key.High] then
    Result := Key.Count else
  begin
    { Then for sure we're between two Key values.
      Note that we know that Key.Count > 1 (otherwise, Key.First = Key.Last
      so one of <= or >= comparisons above would occur). }
    Assert(Key.Count > 1);

    { Always A < B.
      We're considering range from Key[A] to Key[B].

      Remember that we cannot actually depend on the fact that
      Key values are non-decreasing. They should be non-decreasing,
      and we have to output correct result only when they are non-decreasing,
      but we also have to terminate (with any result) in any situation.
      Reason: Key values are supplied in X3D file, so they may be broken
      in every possible way. }

    A := 0;
    B := Key.High;
    while B - A > 1 do
    begin
      Result := (A + B) div 2;
      { A < B => (A + B) < 2B => (A + B) div 2 < B => Result < B.
        Also, Result > A (the only way how Result could be = A
        would be when B = A + 1, but we eliminated this case by "while"
        condition".

        This is good, it means A < Result < B, so Result is good candidate
        for next A or B, it will for sure shorten the distance
        between A and B. }
      Assert(A < Result);
      Assert(Result < B);
      if Fraction <= Key[Result] then
        B := Result else
        A := Result;
    end;
    Result := B;

    if Key[B] > Key[A] then
      T := (Fraction - Key[A]) / (Key[B] - Key[A]) else
      T := 0;
  end;
end;

{ TDynNodeDestructionNotifications ------------------------------------------- }

procedure TDynNodeDestructionNotificationArray.ExecuteAll(Node: TVRMLNode);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
    Items[I](Node);
end;

{ unit init/fini ------------------------------------------------------------ }

initialization
  AnyNodeDestructionNotifications := TDynNodeDestructionNotificationArray.Create;

  VRMLFieldsManager.RegisterClasses([TSFNode, TMFNode]);

  NodesManager := TNodesManager.Create;

  RegistedInventorNodes;
  RegisterVRML1Nodes;
  RegisterVRML97HAnimNodes;
  {$ifdef KAMBI_HAS_NURBS} RegisterVRML97NodesNurbs; {$endif}
  RegisterKambiNodes;
  RegisterAvalonNodes;

  { X3D components registration : }

  RegisterCoreNodes;
  RegisterTimeNodes;
  RegisterNetworkingNodes;
  RegisterGroupingNodes;
  RegisterRenderingNodes;
  RegisterShapeNodes;
  RegisterGeometry3DNodes;
  RegisterGeometry2DNodes;
  RegisterTextNodes;
  RegisterSoundNodes;
  RegisterLightingNodes;
  RegisterTexturingNodes;
  RegisterInterpolationNodes;
  RegisterPointingDeviceSensorNodes;
  RegisterKeyDeviceSensorNodes;
  RegisterEnvironmentalSensorNodes;
  RegisterNavigationNodes;
  RegisterEnvironmentalEffectsNodes;
  RegisterGeospatialNodes;
  RegisterHAnimNodes;
  {$ifdef KAMBI_HAS_NURBS} RegisterNURBSNodes; {$endif}
  RegisterDISNodes;
  RegisterScriptingNodes;
  RegisterEventUtilitiesNodes;
  RegisterShadersNodes;
  RegisterCADGeometryNodes;
  RegisterTexturing3DNodes;
  RegisterCubeMapTexturingNodes;
  RegisterLayeringNodes;
  RegisterLayoutNodes;
  RegisterRigidBodyPhysicsNodes;
  RegisterPickingNodes;
  RegisterFollowersNodes;
  RegisterParticleSystemsNodes;

  TraverseState_CreateNodes(StateDefaultNodes);
  TraverseSingleStack := TVRMLGraphTraverseStateStack.Create;

  ChangeListeners := TVRMLChangesEnginesList.Create;
finalization
  FreeAndNil(ChangeListeners);

  TraverseState_FreeAndNilNodes(StateDefaultNodes);
  FreeAndNil(TraverseSingleStack);

  FreeAndNil(NodesManager);
  FreeAndNil(AnyNodeDestructionNotifications);
end.
