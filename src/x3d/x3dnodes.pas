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

(*
  @abstract(Nodes and other important bulding blocks
  of VRML/X3D (prototypes, routes and so on).)

  This is the central unit for VRML/X3D processing, as VRML/X3D file
  is basically just a graph of nodes. We represent whole VRML/X3D file
  by it's root node. This is what we load, save and process in this unit.

  The chapter "Reading, writing, processing VRML scene graph"
  in the documentation on
  [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.scene_graph.html]
  is almost completely devoted to documenting the design of this single unit.

  @bold(Various uses of this unit:)

  @unorderedList(
    @item(Nodes can be loaded or saved from the stream in a classic or
      XML encoding.
      For classic encoding we use a lexer in X3DLexer unit.
      For XML encoding, we use standard FPC DOM unit.
      Loading and saving of fields (in both encodings) is inside X3DFields unit.

      When reading VRML/X3D files, we generally do not change the VRML/X3D graph.
      So we're able to save exactly the same VRML/X3D graph
      back to another file. See also
      [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.writing_vrml.html#section.vrml_preserving].
      This allows writing various VRML/X3D
      processing tools, that can simply read the file, change whatever
      they want, and write the file back --- knowing that the "untouched"
      parts of graph are preserved perfectly.)

    @item(TX3DNode class offers a lot of methods to process VRML/X3D graph.
      See TX3DNode.Traverse, TX3DNode.EnumerateNodes and
      TX3DNode.FindNode. TX3DNode.Traverse is especially important, as it
      walks through VRML/X3D graph just as the specification says
      (accumulating transformation, visiting only active children of
      nodes like Switch or LOD),
      gathering some state (useful especially for VRML 1.0, but also
      used for various things in later VRML/X3D versions).

      When you want to render VRML/X3D graph, you can just traverse
      the graph and render each geometry node (TAbstractGeometryNode instance)
      knowing it's state (that will contain transformation and such).
      Alternatively, simple renderer can also use TAbstractGeometryNode.Triangulate.)

    @item(TAbstractGeometryNode is an important descendant of TX3DNode,
      as it defines stuff actually visible in the 3D world.
      It has useful routines for calculating bounding volumes,
      triangulating and such.

      But note that usually it's more comfortable
      to load your scene to TCastleScene or TCastleSceneCore and then
      query the shapes list in TCastleSceneCore.Shapes --- this is usually
      more comfortable, also TCastleSceneCore and TShape cache some results
      for speed.)

    @item(This unit doesn't depend on OpenGL, or any other particular rendering
      method. So it's suitable also for CastleRayTracer, and every other possible
      renderer that will ever get implemented.)

    @item(Your own units can define new VRML/X3D nodes, by declaring
      new classes descending from TX3DNode (or other, more specialized,
      descendant). You should register your new classes by calling
      @link(TNodesManager.RegisterNodeClasses NodesManager.RegisterNodeClasses).

      Examples of defining your own VRML/X3D node types (without modifying
      sources of this unit, or any other unit) are for example in the
      X3DBezierCurve unit in @code(bezier_curves) demo,
      and LevelUnit in malfunction.)
  )

  @bold(Node class names, and inheritance:)

  @unorderedList(
    @item(Normal VRML/X3D nodes are defined by classses
      named like @code(TXxxNode). These nodes can be specified inside the VRML/X3D
      files. See VRML/X3D specifications, and also our extensions specification,
      on [http://castle-engine.sourceforge.net/vrml_x3d.php].

      There are also abstract node classes. Their definitions are helpful
      for handling some functionality common to many descendants,
      and to declare allowed children in SFNode/MFNode fields.
      Abstract node classes are named like @code(TAbstractXxxNode).
      Some of the abstract nodes are also defined by X3D specification,
      and some of them are just our own inventions.

      Finally, there are some special-purpose node classes that play
      important role in our VRML/X3D organization.
      They are not abstract, but also their exact instances
      are not created under normal circumstances.
      These are named like @code(TX3DXxxNode), currently
      these are only: TX3DNode, TX3DRootNode, TX3DUnknownNode, TX3DPrototypeNode.

      All node classes descend from the base TX3DNode class.

      Some abstract nodes have also Pascal interfaces, like IAbstractXxxNode.
      Some ideas of X3D specification (although not many)
      need multiple inheritance, so interfaces have to be used.
      They all descend from IX3DNode.)

    @item(
      Optional suffix _1 or _2 at the node class name indicates that
      this is only for a specific VRML/X3D standard version.
      Suffix _1 indicates nodes specific to VRML 1.0.
      Suffix _2 indicates nodes specific to VRML 2.0 (aka 97),
      that are not available in X3D.
      Latest X3D nodes do not have any suffix
      (to not clutter the source code that simply wants to use the latest
      and best version of the standard).

      For example, we have TIndexedFaceSetNode_1 for VRML 1.0 and
      TIndexedFaceSetNode for VRML 2.0 and X3D.)
  )

  @bold(VRML/X3D versions handling:)

  @unorderedList(
    @item(
      We handle VRML 1.0, VRML 2.0 (aka VRML 97) and X3D (aka VRML 3.x).

      Every correct VRML / X3D file in classic and XML encoding should be parsed
      by this unit.
      See [http://castle-engine.sourceforge.net/x3d_implementation_status.php]
      for much more detailed information about supported features.)

    @item(
      Also many Inventor 1.0 files are correctly parsed.
      We handle Inventor 1.0 mostly like VRML 1.0, also some small
      things and nodes specific for Inventor 1.0 are implemented here, see
      [http://castle-engine.sourceforge.net/x3d_extensions.php#ext_iv_in_vrml].)

    @item(
      Note that structures in this unit are @italic(not) focused
      on either VRML 1.0 or VRML >= 2.0. On the contrary: we try to handle
      the @italic(sum of all VRML and X3D). When reading VRML 1.0,
      many VRML 2.0 constructs (that do not conflict with anything in VRML 1.0)
      are allowed, and the other way around too.

      Internally, we do not convert VRML 1.0-specific constructs
      to VRML 2.0/X3D constructs (or the other way around).
      See [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.vrml_1_2_sum.html]
      for more in-depth explanation of how, and why, we handle both
      old-style (Inventor, VRML 1.0) and new-style (VRML 2.0, X3D)
      syntax.)
  )

  @bold(Files organization:) X3D nodes are inside x3d_COMPONET_NAME.inc files.
  This way X3D specification components provide a natural way to group
  the vast number of nodes into files. Some remaining nodes that are not part
  of X3D are in other x3dnodes_xxx.inc files, for example
  x3dnodes_1.inc contains only VRML 1.0-specific nodes.
*)

unit X3DNodes;

{$I castleconf.inc}

interface

uses SysUtils, FGL, Classes, XMLRead, DOM,
  CastleVectors, X3DLexer, CastleUtils, CastleClassUtils,
  X3DFields, CastleBoxes, CastleImages, CastleColors,
  CastleVideos, X3DTime, Castle3D, CastleMaterialProperties,
  CastleScript, X3DCastleScript, CastleInternalOctree, CastleCompositeImage,
  CastleTextureImages, CastleKeysMouse, CastleSoundEngine, CastleStringUtils,
  CastleTextureFontData, CastleGenericLists, CastleShaders, CastleRays;

{$define read_interface}

const
  DefaultMaterial_1AmbientColor: TVector3Single = (0.2, 0.2, 0.2);
  DefaultMaterialAmbientIntensity = 0.2;
  DefaultMaterialDiffuseColor: TVector3Single = (0.8, 0.8, 0.8);
  DefaultMaterialSpecularColor: TVector3Single = (0, 0, 0);
  DefaultMaterialEmissiveColor: TVector3Single = (0, 0, 0);
  DefaultMaterialShininess = 0.2;
  DefaultMaterialTransparency = 0.0;
  DefaultMaterialMirror = 0.0;
  DefaultMaterialReflSpecularExp = 1000000;
  DefaultMaterialTransSpecularExp = 1000000;

{ -----------------------------------------------------------------------------
  Looong "type" declaration below, with many class definitions depending
  on each other. }

type
  { forward declarations } { }
  TX3DNodeList = class;
  TX3DNode = class;
  TCoordinate3Node_1 = class;
  TShapeHintsNode_1 = class;
  TFontStyleNode_1 = class;
  TMaterialNode_1 = class;
  TMaterialBindingNode_1 = class;
  TNormalNode = class;
  TNormalBindingNode_1 = class;
  TTexture2Node_1 = class;
  TTextureCoordinate2Node_1 = class;
  TAbstractGeometryNode = class;
  TAbstractLightNode = class;
  TAbstractShapeNode = class;
  TAbstractTexture2DNode = class;
  TBlendModeNode = class;
  TAbstractTextureNode = class;
  TX3DEventsEngine = class;
  TClipPlaneNode = class;
  THAnimHumanoidNode = class;
  TLocalFogNode = class;
  TEffectNode = class;
  TX3DRootNode = class;
  TX3DGraphTraverseState = class;

  TX3DNodeClass = class of TX3DNode;

  TX3DNodeProc = procedure (Node: TX3DNode) of object;
  TX3DNodeSearchProc = function (Node: TX3DNode): Pointer of object;

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
    vsTextureCoordinate2
  );

  { Nodes that will be saved inside TX3DGraphTraverseState.LastNodes.
    These are nodes that affect how following nodes are rendered,
    mostly for VRML 1.0 "state". }
  TTraverseStateLastNodes = record
    case Integer of
      0: ( Nodes: array [TVRML1StateNode] of TX3DNode; );
      1: ( Coordinate3: TCoordinate3Node_1;
           ShapeHints: TShapeHintsNode_1;
           FontStyle: TFontStyleNode_1;
           Material: TMaterialNode_1;
           MaterialBinding: TMaterialBindingNode_1;
           Normal: TNormalNode;
           NormalBinding: TNormalBindingNode_1;
           Texture2: TTexture2Node_1;
           TextureCoordinate2: TTextureCoordinate2Node_1;
           { additions here must be synchronized with additions to
             TVRML1StateNode }
         );
  end;

  { Light source instance in the scene. References VRML/X3D
    light source node (see @link(Node)), and keeps track of light source
    transformation in the 3D world. For the sake of speed
    also stores a couple of light's properties already multiplied
    by the transformation.

    This record may be initialized only by TAbstractLightNode.CreateLightInstance.
    Update it (when transform changes) by TAbstractLightNode.UpdateLightInstance. }
  TLightInstance = object
    Node: TAbstractLightNode;

    Transform: TMatrix4Single;
    TransformScale: Single;

    { Light location, already transformed by the @link(Transform) matrix.
      For TAbstractPositionalLightNode lights. }
    Location: TVector3Single;

    { Light direction, already normalized and
      transformed by the @link(Transform) matrix.
      For spot and directional lights. }
    Direction: TVector3Single;

    { Light radius, already transformed by the @link(Transform) matrix.
      For lights with radius (positional lights in VRML >= 2.0,
      that is TAbstractPositionalLightNode with HasRadius = true). }
    Radius: Single;

    { Are light location, direction (in this class and inside Node fields)
      expressed in world coordinates. If not, they are expressed in scene
      coordinates.

      This matters if you render the scene using TCastleScene,
      and transform it by T3DTransform or direct OpenGL modelview changes.
      By default (WorldCoordinates = false) we assume that light is defined
      in scene space, so it will be transformed by the whole modelview matrix
      (camera matrix with scene transformations).
      When this is true, during rendering we take care to transform this light
      only by camera matrix (not additional scene transformation).
      Useful for example for headlight. }
    WorldCoordinates: boolean;

    { Position expressed in homogeneous coordinates.
      For positional lights, the last component is always 1.
      For directional lights, the last component is always 0. }
    function Position: TVector4Single;

    { Light contribution to the specified vertex color.
      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      TX3DGraphTraverseState.Emission should be added to
      TLightInstance.Contribution (for each light),
      and resulting color should be processed by TFogNode.ApplyFog.

      We do not clamp color components to (0, 1). This would be a waste of time,
      you should clamp only at the end (or never). This also allows
      to multiply / accumulate values outside of the (0, 1) range
      during calculations. OpenGL also clamps only at the end. }
    function Contribution(
      const Point: TVector3Single; const PointPlaneNormal: TVector4Single;
      State: TX3DGraphTraverseState;
      const CamPosition: TVector3Single): TVector3Single;

    { Light contribution, without knowing the camera or full material.
      We have a 3D vertex, we know it lies on a plane with given normal,
      and we have light information. Try to calculate VRML/X3D lighting
      equation as close as possible to the fully correct version (see regular
      @link(Contribution) method) with this information.

      The specular lighting part must be simply ignored in this case.  }
    function ContributionCameraIndependent(
      const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single): TVector3Single;
  end;
  PLightInstance = ^TLightInstance;

  TLightInstancesList = class(specialize TGenericStructList<TLightInstance>)
  public
    { Find given light node. Return -1 if not found. }
    function IndexOfNode(Node: TAbstractLightNode): integer;
    { Find light with given node name. Return @nil if not found. }
    function FindName(NodeName: string): PLightInstance;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { Append List to our contents, setting every light's WorldCoordinates = @true. }
    procedure AppendInWorldCoordinates(const AList: TLightInstancesList);
  end;

  { Clipping plane, along with a transformation. }
  TClipPlane = record
    Node: TClipPlaneNode;
    Transform: TMatrix4Single;
  end;
  PClipPlane = ^TClipPlane;

  TClipPlaneList = class(specialize TGenericStructList<TClipPlane>)
  public
    { Find record with given TClipPlaneNode, returns -1 if not found. }
    function IndexOfNode(Node: TClipPlaneNode): Integer;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
  end;

  TPointingDeviceSensorList = class;

  { Current state (transformation and such) when traversing VRML/X3D graph.

    For VRML/X3D >= 2.0 this could be simpler, as VRML/X3D >= 2.0 doesn't need
    to keep track for example of the @link(LastNodes).
    But we want to still handle VRML 1.0, 100% correctly, so here we are:
    this class contains whole state needed for any VRML/X3D version. }
  TX3DGraphTraverseState = class
  private
    FLastNodes: TTraverseStateLastNodes;
    FOwnedLastNodes: array [TVRML1StateNode] of boolean;
    procedure CommonCreate;

    { Sets FLastNodes to NewLastNodes.

      During doing this, old FLastNodes are freed if they were owned.
      New nodes are not owned.

      This takes care of checking for each TVRML1StateNode
      if the old node is equal to new one. If yes, then the node
      if not freed (regardless of "owned" status), and the "owned"
      status is left as-is (not chaned to false).
      This way calling thing like @code(AssignLastNodes(FLastNodes)),
      is a valid harmless operation. }
    procedure AssignLastNodes(const NewLastNodes: TTraverseStateLastNodes);
  public
    { Nodes that are saved during VRML/X3D traversing.
      These nodes affect some following nodes in the graph,
      mostly for VRML 1.0.

      They are never @nil (traversing code must always take care to initialize
      them to default nodes at the beginning).

      Note that TX3DGraphTraverseState instance doesn't have to own
      these nodes (doesn't free them, and doesn't track of when they are
      freed). E.g. nodes' TX3DNode.ParentsCount doesn't take into account
      that they are owned by this state.
      Although for some tricks (but not during normal VRML/X3D traversing)
      some nodes are owned, by using SetLastNodes with OwnNode = @true.

      For nodes that are within TraverseStateLastNodesClasses
      (and thus are stored inside LastNodes): it's guaranteed
      they don't affect the state (of this class) during traversing
      (that is, they don't do anything special in
      TX3DNode.BeforeTraverse / TX3DNode.MiddleTraverse / TX3DNode.AfterTraverse).
      So it's guaranteed that changing some field's value of a node
      within TraverseStateLastNodesClasses affects @italic(only)
      the shapes that have given node inside State.LastNodes.
      TCastleSceneCore.InternalChangedField depends on that. }
    property LastNodes: TTraverseStateLastNodes read FLastNodes;

    procedure SetLastNodes(const StateNode: TVRML1StateNode;
      const Node: TX3DNode; const OwnNode: boolean);
  public
    { Lights active in this state.

      May be @nil if empty. This way we optimize creation / assignment time,
      which happen very often with TX3DGraphTraverseState during VRML/X3D
      traversing.

      Note that VRML >= 2.0 "global" lights are added from TCastleSceneCore,
      not during the traverse pass. }
    Lights: TLightInstancesList;

    procedure AddLight(const Light: TLightInstance);
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

    { A uniform scale of the matrix @link(Transform). If the matrix
      causes non-uniform scaling, this value represents an average scale.

      This is updated while traversing the VRML graph, just like
      the @link(Transform) matrix is updated. This way it's calculated
      fast and easy --- we do not actually extract it from a matrix
      (as long as you don't use explicit MatrixTransform in the VRML/X3D file). }
    TransformScale: Single;

    { Copy transformation-related fields from Source.
      Copies @link(Transform) matrix, along with related information
      like InvertedTransform and TransformScale.
      Copies also the @link(ClipPlanes) list, as it contains the transformation
      information. }
    procedure AssignTransform(Source: TX3DGraphTraverseState);

  public
    { Current texture transformation. Usable only for VRML 1.0, in VRML 2.0
      texture transformations don't accumulate like modelview transformations. }
    TextureTransform: TMatrix4Single;

    ShapeNode: TAbstractShapeNode;

    constructor CreateCopy(Source: TX3DGraphTraverseState);

    { Standard constructor.
      Uses global StateDefaultNodes as default nodes for VRML 1.0 state.
      This makes it fast, and improves cache (more nodes have equal reference). }
    constructor Create;

    destructor Destroy; override;

    procedure Assign(Source: TX3DGraphTraverseState);

    { Clear the whole state, just like this TX3DGraphTraverseState instance
      would be just constructed. }
    procedure Clear;

    { Compare with other TX3DGraphTraverseState instance.
      True if these two states, when applied to the same geometry,
      result in the same TGeometryArrays output.
      If IgnoreTransform then we should ignore transformation during comparison
      (it means that renderer is absolutely sure that different transformation
      of geometry doesn't affect the generated arrays). }
    function Equals(SecondValue: TX3DGraphTraverseState;
      const IgnoreTransform: boolean): boolean; {$ifdef TOBJECT_HAS_EQUALS} reintroduce; {$endif}

    { Returns texture node that should be used for nodes within this State.
      Regardless of VRML/X3D version. May return multi-texture
      (TMultiTextureNode), or normal 2D texture (TAbstractTexture2DNode),
      or some other TAbstractTextureNode descendant (cube map, 3d texture).

      Details:
      If ShapeNode <> nil, this returns texture node taken from
      ShapeNode.Texture (note that it may be nil, if Apperance
      of Appearance.Texture node is NULL in VRML).
      Otherwise it returns texture from LastNodes.Texture2. }
    function Texture: TAbstractTextureNode;

    { Returns BlendMode for this state, or @nil if not present. }
    function BlendMode: TBlendModeNode;

  public
    { Information if you're within any inline node or expanded prototype.
      InsideInline = 0 means you're not inside any inline node,
      1 means you're inside one inline, 2 means you're within content
      inlined from yet another inline node, and so on.
      Analogous for InsidePrototype.

      These are measured from the node where you
      started TX3DNode.Traverse call, that is they assume that the initial
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
    PointingDeviceSensors: TPointingDeviceSensorList;

    { For Humanoid skeleton, these contain cummulated joint transformation. }
    HumanoidTransform, HumanoidInvertedTransform: TMatrix4Single;
    { Humanoid node containing us, or @nil if none. }
    Humanoid: THAnimHumanoidNode;

    { ClipPlanes affecting nodes within this state.

      They are collected here regardless of whether they are enabled or not.
      This allows efficient implementation of @code(ClipPlane.enabled)
      dynamic changes.

      Ordered from the most global to most local ones.
      So, following the X3D specification, we should consider the first
      clip planes on this list more important.

      Always @nil if empty. This allows us to optimize TX3DGraphTraverseState
      processing. }
    ClipPlanes: TClipPlaneList;

    { Local fog settings. When @nil, it means use global fog (or no fog,
      if no global fog defined in file). }
    LocalFog: TLocalFogNode;

    { Effects (TEffectNode) affecting this state. }
    Effects: TX3DNodeList;

    function AddClipPlane: PClipPlane;

    { Calculate emission color of given shape.

      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      Emission should be added to
      TLightInstance.Contribution (for each light),
      and resulting color should be processed by TFogNode.ApplyFog.

      When LightingCalculationOn = @false we actually take diffuseColor
      instead of emissiveColor. This is useful if you want to force
      the scene completely unlit, usually diffuseColor is more useful for this
      (since emissiveColor is often black on everything). }
    function Emission(LightingCalculationOn: boolean): TVector3Single;
  end;

  { Stack of TX3DGraphTraverseState.

    Allows you for much faster
    creation/destruction of TX3DGraphTraverseState instances.
    Although you can always construct / destruct TX3DGraphTraverseState
    as normal objects, in some cases this is too slow: when traversing VRML/X3D graph
    (e.g. profile animate_3d_model_by_code_2), merely
    creating/destroying TX3DGraphTraverseState instances takes a noticeable
    amount of time.

    This stack allows you to do this faster, first of all by
    internally using a prepared pool of instances.

    Each PushClear call creates a clear state instance, and places
    it on the stack.
    Each Push call creates a copy of current top and places it on the stack.
    Each Pop removes and destroys the last instance added by Push.

    Naturally, you can call Push and Top only when the stack is not empty.
    In practice, using the stack always starts in TX3DNode.Traverse,
    where we push initial clear state. So the stack passed to various
    callbacks, TX3DNode.BeforeTraverse and such is always guaranteed non-empty.

    Note that for speed purposes all Traverse calls actually
    share a single stack. That is,
    to avoid creating TX3DGraphTraverseStateStack instance each time
    (because even creating TX3DGraphTraverseStateStack
    takes some time (as it prepares a pool of TX3DGraphTraverseState
    instances, to allow fast push/pop)), TX3DNode simply reuses a single
    global TX3DGraphTraverseStateStack instance. This means that,
    if you execute Traverse while being inside other Traverse, you
    must first finish innermost Traverse before continuing with the outer. }
  TX3DGraphTraverseStateStack = class
  private
    Items: array of TX3DGraphTraverseState;
    ItemsAllocated: Cardinal;
    procedure GrowItems;
  public
    constructor Create;
    destructor Destroy; override;

    { Remove everything. }
    procedure Clear;

    { Push a clear state on the stack. Clear state has everything set
      like a TX3DGraphTraverseState right after creating. }
    procedure PushClear;
    { Push a copy of current top on the stack. }
    procedure Push;
    { Push a copy of given Item on the stack.
      We copy by TX3DGraphTraverseState.Assign, we don't copy the reference. }
    procedure Push(const Item: TX3DGraphTraverseState);
    procedure Pop;

    { Peek at the top of the stack. }
    function Top: TX3DGraphTraverseState;
    function PreviousTop: TX3DGraphTraverseState;
  end;

  PTraversingInfo = ^TTraversingInfo;
  TTraversingInfo = record
    Node: TX3DNode;
    ParentInfo: PTraversingInfo;
  end;

  { Used as a callback by TX3DNode.Traverse. }
  TTraversingFunc = function (Node: TX3DNode;
    StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo;
    var TraverseIntoChildren: boolean): Pointer of object;

  TTraversingAfterFunc = procedure (Node: TX3DNode;
    StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo) of object;

  TEnumerateChildrenFunction =
    function (Node, Child: TX3DNode): Pointer of object;

  TEnumerateReplaceNodesFunction =
    procedure (ParentNode: TX3DNode; var Node: TX3DNode) of object;

  TSFNode = class;
  TMFNode = class;
  TX3DPrototypeNode = class;
  TX3DPrototypeBase = class;
  TX3DPrototypeBaseList = class;
  TX3DRoute = class;
  TX3DRouteList = class;
  TX3DImport = class;
  TX3DExport = class;
  TX3DInterfaceDeclaration = class;
  TX3DNodeNames = class;
  TX3DReaderNames = class;
  TX3DPrototypeNames = class;

  TX3DAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);
  TX3DAccessTypes = set of TX3DAccessType;

  TX3DInterfaceDeclarationList = class;

  TNodeDestructionNotification = procedure (Node: TX3DNode) of object;

  TNodeDestructionNotificationList = class(specialize TGenericStructList<TNodeDestructionNotification>)
  public
    { Call all functions. }
    procedure ExecuteAll(Node: TX3DNode);
  end;

  TNodeTransformationChange = (
    ntcNone,
    ntcSwitch, //< TSwitchNode
    ntcLOD, //< TAbstractLODNode
    ntcTransform, //< ITransformNode
    ntcGeometry, //< TAbstractGeometryNode
    ntcBackground, //< TAbstractBackgroundNode
    ntcFog, //< TFogNode
    ntcViewpoint, //< TAbstractViewpointNode
    ntcLight, //< TAbstractLightNode
    ntcProximitySensor, //< TProximitySensorNode
    ntcVisibilitySensor //< TVisibilitySensorNode
  );

  { @exclude Internal for TX3DNodesCache. }
  TCachedNode = class
  private
    URL: string;
    References: Cardinal;
    Node: TX3DRootNode;
  end;
  TCachedNodeList = specialize TFPGObjectList<TCachedNode>;

  { Cache for resources not specific to renderer (OpenGL).
    Includes all TTexturesVideosCache resources (texture, movie
    data) and adds cache for 3D models. }
  TX3DNodesCache = class(TTexturesVideosCache)
  private
    CachedNodes: TCachedNodeList;
    InsideFree3DNodeDelete: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Load 3D model, just like Load3D but with a cache.
      URL must be absolute (not relative).

      Note that this should not be used if you plan to modify the model graph
      (for example by VRML/X3D events). In such case, the cache should not
      be used, as it would make all the model instances shared.
      For example, if you inline the same model multiple times, you could not
      modify one instance independent from another. }
    function Load3D(const URL: string): TX3DRootNode;

    { Unload previously loaded here 3D model.
      Node may be @nil (then it's ignored), or something loaded by
      Load3D (then it's released and changed to @nil). }
    procedure Free3D(var Node: TX3DRootNode);

    function Empty: boolean; override;
  end;

{$I x3dnodes_node.inc}
{$I x3dnodes_generatedtextures.inc}

  TX3DNodeClassesList = class(TList)
  private
    function GetItems(Index: Integer): TX3DNodeClass;
    procedure SetItems(Index: Integer; Value: TX3DNodeClass);
  public
    property Items[Index: Integer]: TX3DNodeClass
      read GetItems write SetItems; default;
    procedure AssignArray(
      const AItemsArray: array of TX3DNodeClass);
    function IndexOf(NodeClass: TX3DNodeClass): Integer; overload;
    { Equivalent to IndexOf(NodeClass.ClassType),
      taking care of necessary typecasts. }
    function IndexOf(Node: TX3DNode): Integer; overload;

    { Looks for a node class that is ancestor of given Node,
      in other words that satisfies the @code(Node is Items[Result]) condition.
      Contrast this with IndexOf method, which looks only for an exact
      class match.

      Returns -1 if not found. }
    function IndexOfAnyAncestor(Node: TX3DNode): Integer;

    procedure Add(Value: TX3DNodeClass);

    { Add all node classes registered in NodesManager that implement given
      interface Interf. }
    procedure AddRegisteredImplementing(Interf: TGUID);
  end;

  TAllowedChildren = (acAll, acClasses, acInterface);

  { VRML/X3D field holding a reference to a single node.
    It's defined in this unit, not in X3DFields, since it uses
    TX3DNode definition. NULL value of the field is indicated by
    Value field = nil.

    Note that we store AllowedChildren list, which is a list of
    classes allowed as a Value (also nil is always allowed).
    But this is used only to produce warnings for a user.
    You should never assert that Value actually is one the requested
    classes. We want to keep here even not allowed items,
    because we want operation "read from VRML file + write to VRML file"
    to be as non-destructible as possible. So if user wrote
    invalid class hierarchy, we will output this invalid class hierarchy. }
  TSFNode = class(TX3DSingleField)
  private
    FValue: TX3DNode;
    FParentNode: TX3DNode;
    AllowedChildren: TAllowedChildren;
    AllowedChildrenClasses: TX3DNodeClassesList;
    AllowedChildrenInterface: TGUID;
    FDefaultValue: TX3DNode;
    FDefaultValueExists: boolean;
    FWeakLink: boolean;
    procedure SetValue(AValue: TX3DNode);
    procedure SetDefaultValue(ADefaultValue: TX3DNode);
    procedure SetDefaultValueExists(AValue: boolean);
    procedure SetWeakLink(const AValue: boolean);
    procedure WarningIfUnusedWeakLink;
    procedure DestructionNotification(Node: TX3DNode);
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
  public
    { Construct a field allowing any children class.
      Suitable only for special cases. For example, in instantiated prototypes,
      we must initially just allow all children, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing. }
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;
    constructor Create(AParentNode: TX3DNode; const AName: string;
      const AAllowedChildrenClasses: array of TX3DNodeClass;
      AValue: TX3DNode = nil); overload;
    { Constructor that takes a list of allowed children classes.
      Note that we copy the contents of AAllowedChildrenClasses,
      not the reference. }
    constructor Create(AParentNode: TX3DNode; const AName: string;
      AAllowedChildrenClasses: TX3DNodeClassesList;
      AValue: TX3DNode = nil); overload;
    { Constructor that allows as children any implementor of given interface. }
    constructor Create(AParentNode: TX3DNode; const AName: string;
      AnAllowedChildrenInterface: TGUID;
      AValue: TX3DNode = nil); overload;
    destructor Destroy; override;

    { Default value of SFNode field.

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
    property DefaultValue: TX3DNode
      read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write SetDefaultValueExists default false;

    property Value: TX3DNode read FValue write SetValue;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    procedure ParseXMLElement(Element: TDOMElement; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    { VRML node containing this field. May be @nil if unknown, in special
      cases.

      Note that this property is exactly the same as
      TX3DFieldOrEvent.ParentNode,
      contains always the same value. But this is declared as TX3DNode,
      so it's more comfortable. }
    property ParentNode: TX3DNode read FParentNode;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Checks is the Child allowed as a value of this SFNode,
      and makes WritelnWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      WritelnWarning message will suggest that this Child is used as value
      of this node. In other words, you should only pass as Child
      a node that you want to assign as Value to this field,
      otherwise WritelnWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TX3DNode);

    function ChildAllowed(Child: TX3DNode): boolean;
    function CurrentChildAllowed: boolean;

    { Calls Func for our @link(Value), assuming it's set (non-nil).
      The main use for this is to simplify implementation of
      TX3DNode.DirectEnumerateActive overrides in TX3DNode descendants. }
    function Enumerate(Func: TEnumerateChildrenFunction): Pointer;

    procedure Send(const AValue: TX3DNode); overload;

    { Use weak links to deal with cycles in the X3D graph.

      Marking a field as a @italic(weak link) can only be done
      when the field value is empty, right when the field is created,
      in @link(TX3DNode.CreateNode) descendant.

      Being a @italic(weak link) means two things:

      @orderedList(
        @item(The nodes inside a weak link are not enumerated
          when traversing the X3D graph in @italic(any) way.
          This includes @link(TX3DNode.EnumerateNodes),
          @link(TX3DNode.Traverse) and all others.
          Nodes implementing @link(TX3DNode.DirectEnumerateActive)
          should also omit these fields.)

        @item(A weak link does not create a reference count
          preventing the node from being freed (or freeing
          it automatically when ref count drops to zero).
          Instead, weak links merely observe the nodes, and automatically
          set their value to @nil when the node gets freed.)
      )

      If effect, this avoids loops when enumerating (and avoids
      recursive loops in reference counts, which would cause memory leaks),
      but use this only when you know that the node
      must occur somewhere else in the X3D graph anyway (or it's OK to
      ignore it).
      For example, this is useful for
      @link(TGeneratedShadowMapNode.Light), as we know that the light
      must occur somewhere else in the graph anyway to be useful.
    }
    property WeakLink: boolean
      read FWeakLink write SetWeakLink default false;
  end;

  TSFNodeEventHelper = class helper for TSFNodeEvent
    procedure Send(const Value: TX3DNode; const Time: TX3DTime); overload;
    procedure Send(const Value: TX3DNode); overload;
  end;

  { VRML/X3D field holding a list of nodes.

    Just like SFNode, it's defined in this unit, as it uses TX3DNode.
    Note that items of MFNode @italic(cannot) be nil (i.e. VRML/X3D doesn't
    allow to use NULL inside MFNode), contrary to SFNode.

    Note that TMFNode implementation doesn't use TX3DSimpleMultField.
    One reason is that we don't want to parse MFNode items
    by SFNode parser, because MFNode doesn't allow NULL items.
    (In the past, another argument was that we want to use TX3DNodeList
    and it wasn't compatible with TX3DSimpleMultField.
    But now TX3DNodeList descends from TFPSList, so it isn't a problem.)

    Just like for TSFNode:
    Note that we store AllowedChildren list, which is a list of
    classes allowed as Items.
    But this is used only to produce warnings for a user.
    You should never assert that every item actually is one the requested
    classes.  }
  TMFNode = class(TX3DMultField)
  private
    FItems: TX3DNodeList;
    FDefaultItems: TX3DNodeList;
    FDefaultValueExists: boolean;
    FParentNode: TX3DNode;
    AllowedChildren: TAllowedChildren;
    AllowedChildrenClasses: TX3DNodeClassesList;
    AllowedChildrenInterface: TGUID;
    function GetItems(const Index: Integer): TX3DNode;
  protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
    { Get or set the number of items.

      When increasing this, remember that new items of TMFNode
      will be @nil. You @bold(must immediately initialize them to
      something else then @nil) by the @link(Replace) method.
      Other TMFNode methods, and outside code working with MFNodes,
      usually assumes that all MFNode children are non-nil.
      (As VRML/X3D spec don't really allow NULL items inside MFNode fields.)
      @groupBegin }
    function GetCount: Integer; override;
    procedure SetCount(const Value: Integer); override;
    { @groupEnd }
  public
    { Construct a field allowing any children class.
      Suitable only for special cases. For example, in instantiated prototypes,
      we must initially just allow all children, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing. }
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;
    constructor Create(AParentNode: TX3DNode; const AName: string;
      const AAllowedChildrenClasses: array of TX3DNodeClass); overload;
    { Constructor that takes a list of allowed children classes.
      Note that we copy the contents of AAllowedChildrenClasses,
      not the reference. }
    constructor Create(AParentNode: TX3DNode; const AName: string;
      AAllowedChildrenClasses: TX3DNodeClassesList); overload;
    { Constructor that allows as children any implementor of given interface. }
    constructor Create(AParentNode: TX3DNode; const AName: string;
      AnAllowedChildrenInterface: TGUID); overload;
    destructor Destroy; override;

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
    property Items: TX3DNodeList read FItems;

    property ItemsArray[Index: Integer]: TX3DNode read GetItems; default;

    procedure Add(Node: TX3DNode); overload;
    procedure Add(Position: Integer; Node: TX3DNode); overload;

    procedure Delete(Index: Integer);
    { Search list for given node, and, if found, remove it.
      Returns the index of removed item, or -1 if not found. }
    function Remove(const Node: TX3DNode): Integer;
    { Remove child with given Index, and return it, @italic(never freeing it).
      This is analogous to TX3DNode.ExtractChild, see there for more
      explanation. }
    function Extract(Index: Integer): TX3DNode;
    procedure Clear;
    procedure AssignItems(SourceItems: TX3DNodeList);
    procedure Replace(Index: Integer; Node: TX3DNode);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    procedure ParseXMLElement(Element: TDOMElement; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    property ParentNode: TX3DNode read FParentNode;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Checks is Child allowed on the list of nodes of this MFNode,
      and makes WritelnWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      WritelnWarning message will suggest that this Child is added to
      this node. In other words, you should only pass as Child
      a node that you want to add (e.g. by @link(Add)) to this field,
      otherwise WritelnWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TX3DNode);

    function ChildAllowed(Child: TX3DNode): boolean;

    { Lists default items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like AssignDefaultItems (they take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields). }
    property DefaultItems: TX3DNodeList read FDefaultItems;

    { Operate on DefaultItems, just like analogous AssignItems and @link(Clear).
      @groupBegin }
    procedure AssignDefaultItems(SourceItems: TX3DNodeList);
    procedure ClearDefault;
    { @groupEnd }

    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists default false;

    { Calls Func for all current children.
      Stops if Func returns something non-nil.
      The main use for this is to simplify implementation of
      TX3DNode.DirectEnumerateActive overrides in TX3DNode descendants. }
    function Enumerate(Func: TEnumerateChildrenFunction): Pointer;
  end;

{ Specific VRML/X3D nodes ---------------------------------------------------- }

{$I x3dnodes_standard_core.inc}
{$I x3dnodes_standard_time.inc}
{$I x3dnodes_standard_grouping.inc}
{$I x3dnodes_standard_networking.inc}
{$I x3dnodes_standard_rendering.inc}
{$I x3dnodes_standard_shape.inc}
{$I x3dnodes_standard_geometry3d.inc}
{$I x3dnodes_standard_geometry2d.inc}
{$I x3dnodes_standard_text.inc}
{$I x3dnodes_standard_sound.inc}
{$I x3dnodes_standard_lighting.inc}
{$I x3dnodes_standard_texturing.inc}
{$I x3dnodes_standard_interpolation.inc}
{$I x3dnodes_standard_interpolation_cubic_bezier.inc}
{$I x3dnodes_standard_pointingdevicesensor.inc}
{$I x3dnodes_standard_keydevicesensor.inc}
{$I x3dnodes_standard_environmentalsensor.inc}
{$I x3dnodes_standard_navigation.inc}
{$I x3dnodes_standard_environmentaleffects.inc}
{$I x3dnodes_standard_geospatial.inc}
{$I x3dnodes_standard_h-anim.inc}
{$I x3dnodes_standard_nurbs.inc}
{$I x3dnodes_standard_dis.inc}
{$I x3dnodes_standard_scripting.inc}
{$I x3dnodes_standard_eventutilities.inc}
{$I x3dnodes_standard_shaders.inc}
{$I x3dnodes_standard_cadgeometry.inc}
{$I x3dnodes_standard_texturing3d.inc}
{$I x3dnodes_standard_cubemaptexturing.inc}
{$I x3dnodes_standard_layering.inc}
{$I x3dnodes_standard_layout.inc}
{$I x3dnodes_standard_rigidbodyphysics.inc}
{$I x3dnodes_standard_picking.inc}
{$I x3dnodes_standard_followers.inc}
{$I x3dnodes_standard_particlesystems.inc}

{$I x3dnodes_1.inc}
{$I x3dnodes_inventor.inc}
{$I x3dnodes_97_hanim.inc}
{$I x3dnodes_97_nurbs.inc}
{$I x3dnodes_castle.inc}
{$I x3dnodes_instantreality.inc}
{$I x3dnodes_bitmanagement.inc}

{ TX3DUnknownNode --------------------------------------------------- }

  { Not recognized VRML/X3D node type. Used for nodes found when parsing
    VRML/X3D file that are not implemented.

    TX3DUnknownNode is parsed (in classic VRML encoding) in a special way,
    to be able to omit it gracefully.
    While such "unknown" node doesn't really do match in our graph,
    it works correctly with VRML/X3D DEF/USE mechanism.

    Never instantiate this class by a standard constructor.
    Always use CreateUnknown constructor, this way we can safely assume
    that X3DType is always correctly set. }
  TX3DUnknownNode = class(TX3DNode)
  private
    fX3DType: string;
  protected
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
  public
    function X3DType: string; override;
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;

    { base Create will throw exception. Always use CreateUnknown* }
    constructor Create(const AName: string; const ABaseUrl: string); override;

    constructor CreateUnknown(const AName, ABaseUrl: string; const AX3DType :string);
  end;

{ TX3DInterfaceDeclaration -------------------------------------------------- }

  { Interface declaration, used in VRML/X3D (exposed) prototypes and
    for nodes with dynamic fields (Script, ComposedShader).
    See VRML 2.0 and X3D specs.

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
  TX3DInterfaceDeclaration = class(TX3DFileItem)
  private
    FFieldOrEvent: TX3DFieldOrEvent;

    { kept in synch with FFieldOrEvent by SetFieldOrEvent }
    FField: TX3DField;
    FEvent: TX3DEvent;

    procedure SetFieldOrEvent(const Value: TX3DFieldOrEvent);
  private
    FParentNode: TX3DNode;
  public
    constructor Create(AParentNode: TX3DNode);
    destructor Destroy; override;

    { Containing node, if any, for this VRML interface declaration.
      This must also be set to FieldOrEvent.ParentNode created for this
      interface declaration. }
    property ParentNode: TX3DNode read FParentNode;

    { Field or event of this interface declaration.
      Is non-nil after parsing.

      You can assign to this property, to constructs interface
      declarations (and so also prototypes) in your own code
      (e.g. this is used X3D XML reader). Just be careful, and remember
      that this object owns FieldOrEvent (that is, will free it
      at destruction). }
    property FieldOrEvent: TX3DFieldOrEvent
      read FFieldOrEvent write SetFieldOrEvent;

    { Create a copy of current FieldOrEvent.
      Sets NewParentNode as Result.ParentNode.
      Note the new copy will not have ParentIntefaceDeclaration set
      (as the idea is that you own created copy, not this TX3DInterfaceDeclaration
      instance). }
    function CopyFieldOrEvent(NewParentNode: TX3DNode): TX3DFieldOrEvent;

    { Create a copy of current FieldOrEvent, and add it to Node.Fields
      or Node.Events. }
    procedure CopyAndAddFieldOrEvent(Node: TX3DNode);

    { Copies only reference to FieldOrEvent, adding it to Node.Fields
      or Node.Events. }
    procedure AddFieldOrEvent(Node: TX3DNode);

    { Return FieldOrEvent casted as appropriate class.
      @nil if such cast is not possible, for example when
      FieldOrEvent is an event and you try to use Field method.
      @groupBegin }
    property Field: TX3DField read FField;
    property Event: TX3DEvent read FEvent;
    { @groupEnd }

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames;
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
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames;
      FieldValue: boolean);

    { Save this interface declaration to stream.
      This assumes that it starts at the beginning of the line,
      and at the end always writes NL, so at the end it's also
      at the beginning of some line.

      For XML encoding, IS clauses are not saved here.
      They must be saved by containing node.

      @param(FieldValue If @true then we will always save
        Field value or (if classic encoding) IS clauses to stream,
        along with this interface
        declaration (if this interface declaration has the Field set).
        Otherwise, field's value will not be saved, only IS clauses
        if present.)
    }
    procedure IDeclSaveToStream(Writer: TX3DWriter; FieldValue: boolean);

    { Save this interface declaration to stream.
      @seealso IDeclSaveToStream }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Returns access type, corresponding to current @link(Event)
      and @link(Field) values.

      Result is undefined if both Event
      and Field are @nil (which may happen when it's not initialized
      (e.g. parsed) yet) or when both are non-nil (which should never
      happen). }
    function AccessType: TX3DAccessType;

    function DeepCopy(NewParentNode: TX3DNode;
      CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
  end;

  TX3DInterfaceDeclarationList = class(specialize TFPGObjectList<TX3DInterfaceDeclaration>)
  public
    { Find field or event with given Name.
      @nil if not found. }
    function TryFindName(const Name: string): TX3DFieldOrEvent;

    { Find field with given Name.
      @nil if not found. }
    function TryFindFieldName(const Name: string): TX3DField;

    { Find event with given Name.
      @nil if not found. }
    function TryFindEventName(const Name: string): TX3DEvent;
  end;

{ TX3DPrototype ------------------------------------------------------------- }

  { }
  EX3DPrototypeInstantiateError = class(Exception);

  { Node with information about a VRML/X3D prototype.

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
  TX3DPrototypeNode = class(TX3DNode)
  private
    FPrototype: TX3DPrototypeBase;

    function PrepareInstantiateIsClause(Node, Child: TX3DNode): Pointer;

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
    function InstantiateIsClauses(Node, Child: TX3DNode): Pointer;

    { Handle "IS" clause on Destination field/event, by copying it's value
      from Source.

      For fields basically does Destination.AssignValue(Source).
      In case of EX3DFieldAssign, make WritelnWarning with clear message.

      For events establishes internal route.

      Assumes that all Source "IS" clauses are not expanded yet.
      In fact, Source field/event always comes from this node,
      that is TX3DPrototypeNode. So we just expand prototype within
      TX3DPrototypeNode. If this TX3DPrototypeNode is itself within
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
      Destination, Source: TX3DFieldOrEvent;
      NewIsClauseNames: TCastleStringList);
  protected
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
  public
    { This constructor will raise exception for TX3DPrototypeNode.
      Always use CreatePrototypeNode for this node class. }
    constructor Create(const AName, ABaseUrl: string); override;
    constructor CreatePrototypeNode(const AName, ABaseUrl: string;
      APrototype: TX3DPrototypeBase);
    function X3DType: string; override;

    property Prototype: TX3DPrototypeBase read FPrototype;

    { Instantiate the prototype, that is create new VRML node
      (of "normal" classs, not TX3DPrototypeNode) using prototype description.

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
      TMaterialNode. Just like Material node would be normally specified,
      not created by some prototype.

      Note that this TX3DPrototypeNode becomes "owned" by returned
      node instance, in PrototypeInstanceSourceNode.
      (that's needed for returned node's SaveToStream to work correctly).

      Details:
      @unorderedList(
        @item(
          Prototype.Node may be just a wrapper, i.e. TX3DRootNode.

          In this case the first children of Prototype.Node is used
          to create instance. The rest of the wrapper (with this first children
          removed, to not cause cycles) is also duplicated and set
          as new node's PrototypeInstanceHelpers.)

        @item(
          Returned Node (with all it's helpers in PrototypeInstanceHelpers)
          has "IS" clauses everywhere filled, according to our field values.)

        @item(Name of returned node is copied from our Name.)

        @item(
          For SaveToStream to work, returned Node has PrototypeInstance = @true,
          and PrototypeInstanceSourceNode set to Self. This allows SaveToStream
          to correctly save using PrototypeInstanceSourceNode, instead
          of writing actual node contents.))

      @raises(EX3DPrototypeInstantiateError if for some reason
        the prototype cannot be instantiated.
        Outside code should catch this and replace with ApplicationProperties.OnWarning, if possible.)
    }
    function Instantiate: TX3DNode;
  end;

  TX3DPrototypeBase = class(TX3DFileItem)
  private
    FX3DName: string;
    FInterfaceDeclarations: TX3DInterfaceDeclarationList;

    FBaseUrl: string;

    { Parses InterfaceDeclarations. Also inits BaseUrl from
      Names.BaseUrl, by the way. }
    procedure ParseInterfaceDeclarations(ExternalProto: boolean;
      Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse interface declarations in XML encoding.
      Handle sequence of <field> elements.

      Note: unlike classic ParseInterfaceDeclarations,
      this doesn't set BaseUrl, do it yourself (because often
      you do not have 'ProtoInterface', so you would have to do it yourself
      anyway). }
    procedure ParseInterfaceDeclarationsXML(ExternalProto: boolean;
      Element: TDOMElement; Reader: TX3DReaderNames);

    { Saves interface declarations of the prototype.
      For classic encoding, they are already enclosed in [ ]. }
    procedure SaveInterfaceDeclarationsToStream(
      Writer: TX3DWriter; ExternalProto: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property X3DName: string read FX3DName write FX3DName;
    property Name: string read FX3DName write FX3DName; deprecated 'use X3DName';
    property InterfaceDeclarations: TX3DInterfaceDeclarationList
      read FInterfaceDeclarations;

    { Parse prototype, and add it to Names.Prototypes.
      Adds to @code(Names) by @code(Names.Prototypes.Bind(Self)).
      @groupBegin }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); virtual; abstract;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); virtual; abstract;
    { @groupEnd }

    { The base URL path used to resolve urls inside.
      For now, used by EXTERNPROTO urls.
      See TX3DNode.BaseUrl for more comments. }
    property BaseUrl: string read FBaseUrl write FBaseUrl;
  end;

  TX3DPrototypeBaseList = class(specialize TFPGObjectList<TX3DPrototypeBase>);

  TX3DPrototype = class(TX3DPrototypeBase)
  private
    FNode: TX3DRootNode;
  public
    destructor Destroy; override;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); override;
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Prototype contents: all nodes, prototypes, routes defined inside. }
    property Node: TX3DRootNode read FNode;
  end;

  TX3DExternalPrototype = class(TX3DPrototypeBase)
  private
    FURLList: TMFString;

    { FReferencedPrototype has links to other parts of the VRML graph.
      Not only FReferencedPrototype.Node, but also
      FReferencedPrototype.InterfaceDeclaration may have links to it:
      if referenced node has SFNode or MFNode fields and their default
      values have "USE ..." clauses.
      So it's best to keep whole ReferencedPrototypeNode (whole VRML file
      that contained this prototype) loaded. }
    ReferencedPrototypeNode: TX3DRootNode;

    FReferencedPrototype: TX3DPrototype;

    FReferencedClass: TX3DNodeClass;
  public
    constructor Create;
    destructor Destroy; override;
    property URLList: TMFString read FURLList;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); override;
    procedure SaveToStream(Writer: TX3DWriter); override;

    property ReferencedPrototype: TX3DPrototype read FReferencedPrototype;
    property ReferencedClass: TX3DNodeClass read FReferencedClass;

    { Loads URL, until the first success. Sets either ReferencedClass to non-nil
      (if it's built-in node) or ReferencedPrototype (if prototype expansion
      found in external file). }
    procedure LoadReferenced;
    procedure UnloadReferenced;
  end;

{ TX3DRoute ----------------------------------------------------------------- }

  { }
  TX3DRoute = class(TX3DFileItem)
  private
    FSourceNode: TX3DNode;
    FSourceEvent: TX3DEvent;

    FDestinationNode: TX3DNode;
    FDestinationEvent: TX3DEvent;

    LastEventTime: TX3DTime;
    FInternal: boolean;

    procedure DestructionNotification(Node: TX3DNode);

    procedure UnsetEnding(
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean;
      RemoveFromDestructionNotification: boolean = true);

    procedure SetEnding(const NodeName, FieldOrEventName: string;
      Reader: TX3DReaderNames;
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean);

    { Set Event, based on FieldOrEvent (may be actual event,
      or exposed field containing it) and DestEnding.
      Assumes that Event is clear on enter (cleared by UnsetEnding). }
    procedure SetEndingInternal(
      const Node: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
      var Event: TX3DEvent;
      const DestEnding: boolean);

    procedure SetEndingDirectly(
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean);

    procedure EventReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
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
          has SourceEvent as one of explicit (on TX3DNode.Events list) or
          implicit (exposed by some field) event.)
      )

      @groupBegin }
    property SourceNode: TX3DNode read FSourceNode;
    property SourceEvent: TX3DEvent read FSourceEvent;
    { @groupEnd }

    { Destination event properties.
      Analogous to SourceEvent, SourceNode.

      @groupBegin }
    property DestinationNode: TX3DNode read FDestinationNode;
    property DestinationEvent: TX3DEvent read FDestinationEvent;
    { @groupEnd }

    { Set source/destination of the route.

      This does everything that VRML parser should
      do when parsed VRML route. It looks for given node name
      (in Names.Nodes, then Names.Imported),
      then it looks for field/event within this node,
      and if everything is successfull --- sets route properties.

      If something goes wrong, WritelnWarning is generated
      and route ending is left unset.

      @groupBegin }
    procedure SetSource(
      const SourceNodeName, SourceFieldOrEventName: string;
      Reader: TX3DReaderNames);

    procedure SetDestination(
      const DestinationNodeName, DestinationFieldOrEventName: string;
      Reader: TX3DReaderNames);
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
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);

    procedure SetDestinationDirectly(
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);

    procedure SetSourceDirectly(const FieldOrEvent: TX3DFieldOrEvent);
    procedure SetDestinationDirectly(const FieldOrEvent: TX3DFieldOrEvent);
    { @groupEnd }

    { Parse the route (classic VRML encoding).
      Implementation should be able to safely assume that current token
      is ROUTE. }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the route (XML encoding).
      Given Element here must have TagName = 'ROUTE'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    { Save a ROUTE to VRML file.

      Will generate WritelnWarning when route cannot be saved.
      This can happen when SourceNode or SourceEvent
      or DestinationNode or DestinationEvent are @nil.
      Also, if SourceNode and DestinationNode are without a name,
      or the name is not currently bound in Writer.NodeNames.
    }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Clear the memory when the last event passed through this route.
      Route must remember such thing, to avoid loops in routes.
      This is following VRML 2.0 / X3D specifications, that explicitly
      say that only one event per ROUTE per timestamp is allowed.

      Use ResetLastEventTime when you really want to reset this memory.
      In practice, this should be used only by TCastleSceneCore.ResetTime
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

    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DRoute;
  end;

  TX3DRouteList = class(specialize TFPGObjectList<TX3DRoute>);

  TX3DImport = class(TX3DFileItem)
  public
    InlineNodeName, ImportedNodeName, ImportedNodeAlias: string;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the IMPORT declaration (XML encoding).
      Given Element here must have TagName = 'IMPORT'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
  end;

  TX3DExport = class(TX3DFileItem)
  public
    ExportedNodeName, ExportedNodeAlias: string;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the EXPORT declaration (XML encoding).
      Given Element here must have TagName = 'EXPORT'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
  end;

{$I x3dnodes_eventsengine.inc}

{ Node names ----------------------------------------------------------------- }

  { }
  TX3DNodeNameRec = object
    Node: TX3DNode;
    Name: string;
    Finished: boolean;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DNodeNameRec;
  end;
  PX3DNodeNameRec = ^TX3DNodeNameRec;

  { List to track node names while parsing VRML/X3D file. }
  TX3DNodeNames = class(specialize TGenericStructList<TX3DNodeNameRec>)
  private
    FAutoRemove: boolean;
    procedure DestructionNotification(Node: TX3DNode);
    function IndexOfNode(Node: TX3DNode): Integer;
    function IndexOfName(const Name: string): Integer;
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

    { Associate given node with it's own name.

      If not NodeFinished, then we understand that we're parsing / saving
      the node's contents now. If NodeFinished, then we know the node
      contents are fully parsed / saved now. This information helps
      us to detect cycles in VRML/X3D DEF/USE graph.
      For now, we just disallow such cycles. Still, we allow
      ROUTEs from inside the node, so calling with NodeFinished = false
      is still useful for parsing. }
    procedure Bind(Node: TX3DNode; const NodeFinished: boolean); overload;
    procedure Bind(Node: TX3DNode; const NodeFinished: boolean; const BindToName: string); overload;

    { Find node bound to given name. @nil if none. }
    function Bound(const Name: string; out NodeFinished: boolean): TX3DNode;

    { Check is Node bound in the current namespace.
      @false means that node is not within this namespace,
      possibly it's name was hidden by other node with the same name.

      Doesn't check is Node bound to it's name (Node.Name) or something
      else. So this assumes that node can only be bound (if at all)
      only to it's own name, which is true during parsing
      (when nothing can change in the middle of parsing). }
    function Bound(Node: TX3DNode): boolean;

    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DNodeNames;
  end;

  TX3DPrototypeNames = class(TStringListCaseSens)
  public
    procedure Bind(Proto: TX3DPrototypeBase);

    { Find proto bound to given name. @nil if none. }
    function Bound(const Name: string): TX3DPrototypeBase;
  end;

  TX3DImportableNames = class(TStringListCaseSens)
  public
    destructor Destroy; override;

    { Bind Exported names to given Inline node name.
      Exported instance becomes owner by this TX3DImportableNames instance.
      InlineName must be <> '' here. }
    procedure Bind(const InlineName: string; Exported: TX3DNodeNames);
  end;

  TX3DVersion = X3DLexer.TX3DVersion;
  TX3DEncoding = X3DLexer.TX3DEncoding;

  {$I x3dnodes_load.inc}

{ TraverseStateLastNodesClasses ---------------------------------------------- }

const
  { Classes corresponding to nodes on TTraverseStateLastNodes. }
  TraverseStateLastNodesClasses :
    array [TVRML1StateNode] of TX3DNodeClass =
    ( TCoordinate3Node_1, TShapeHintsNode_1, TFontStyleNode_1,
      TMaterialNode_1, TMaterialBindingNode_1, TNormalNode, TNormalBindingNode_1,
      TTexture2Node_1, TTextureCoordinate2Node_1
      { additions here must be synchronized with additions to
        TTraverseStateLastNodes }
    );

{ TNodesManager ------------------------------------------------------------ }

type
  { }
  ENodesManagerError = class(EX3DError);
  ENodeClassRegisterError = class(ENodesManagerError);
  TNodesManager = class
  private
    { Strings[] is ClassX3DType. Objects[] is the actual class
      (typecast to TX3DNodeClass is safe). }
    FRegistered: TStringList;
    function GetRegistered(Index: Integer): TX3DNodeClass;
  public
    constructor Create;
    destructor Destroy; override;

    { Make the given node class known to the parser and other routines.
      We associate the node class with it's TX3DNode.ClassX3DType
      (make sure it's not empty).

      It is OK to register two different node classes with the same node.
      For example, VRML 1.0 TConeNode_1 class and VRML 2.0/X3D TConeNode_2
      class both have a name 'Cone' (and will be correctly chosen during parsing).
      But you cannot register two times the same NodeClass.

      @groupBegin }
    procedure RegisterNodeClass(NodeClass: TX3DNodeClass);
    procedure RegisterNodeClasses(const NodeClasses: array of TX3DNodeClass);
    { @groupEnd }

    { Unregisters given node class, removing it from our table.

      @raises(ENodesManagerError if NodeClass.ClassX3DType = ''
        (so it cannot be even registered), or if
        ((NodeClass was not registered) and ErrorIfNotRegistered)) }
    procedure UnRegisterNodeClass(NodeClass: TX3DNodeClass;
      ErrorIfNotRegistered: boolean = true);

    { Return node class for a given name. This method is the main purpose
      of TNodesManager: to map node names into node classes.

      Searches in nodes registered by RegisterNodeClass and such.
      During searching, looks not only for matching node name,
      but also at matching VRML/X3D version, checking
      @code(ForVRMLVersion(Version)).

      Returns @nil when not found. }
    function X3DTypeToClass(const AX3DType: string;
      const Version: TX3DVersion): TX3DNodeClass;

    { Return class that matches given URL. This is useful for EXTERNROTOs.
      Returns @nil if not found. }
    function URNToClass(const URN: string): TX3DNodeClass;

    { Enumerate all registered classes, from Registered[0] to
      Registered[RegisteredCount - 1].

      @groupBegin }
    property Registered [Index: Integer]: TX3DNodeClass read GetRegistered;
    function RegisteredCount: Cardinal;
    { @groupEnd }
  end;

var
  { Nodes manager instance. In normal circumstances, this is the only
    instance of TNodesManager class ever created. It is created / destroyed
    in this unit's initialization / finalization. }
  NodesManager: TNodesManager;

  { Should be make a warning (using WritelnWarning) when loading data from
    an URI with absolute path. This is quite useful if you want to
    be able to move/copy the files to some other system/location,
    as absolute paths prevent it. }
  WarnAboutAbsoluteFilenames: boolean = true;

{ global procedures ---------------------------------------------------------- }

{$I x3dnodes_encoding_classic.inc}
{$I x3dnodes_encoding_xml.inc}
{$I x3dnodes_save.inc}

{ Create and assign all State.Nodes. }
procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);

{ Free and nil all State.Nodes. }
procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);

{ Free all unused VRML/X3D nodes on the list, then free and @nil the list
  itself. }
procedure X3DNodeList_FreeUnusedAndNil(var List: TX3DNodeList);

const
  ProjectionTypeToStr: array [TProjectionType] of string =
  ('Orthographic', 'Perspective');

const
  { Constants for @link(TAsciiTextNode_1.FdJustification).Value.
    @groupBegin }
  JUSTIFICATION_LEFT = 0 deprecated 'use fjBegin (from an enumerated type TX3DFontJustify) with TAsciiTextNode_1.Justify or TFontStyleNode.Justify properties';
  JUSTIFICATION_CENTER = 1 deprecated 'use fjMiddle (from an enumerated type TX3DFontJustify) with TAsciiTextNode_1.Justify or TFontStyleNode.Justify properties';
  JUSTIFICATION_RIGHT = 2 deprecated 'use fjEnd (from an enumerated type TX3DFontJustify) with TAsciiTextNode_1.Justify or TFontStyleNode.Justify properties';
  { @groupEnd }

  { Constants for
    @link(TMaterialBindingNode_1.FdValue).Value and
    @link(TNormalBindingNode_1.FdValue).Value.
    @groupBegin }
  BIND_DEFAULT = 0;
  BIND_OVERALL = 1;
  BIND_PER_PART = 2;
  BIND_PER_PART_INDEXED = 3;
  BIND_PER_FACE = 4;
  BIND_PER_FACE_INDEXED = 5;
  BIND_PER_VERTEX = 6;
  BIND_PER_VERTEX_INDEXED = 7;
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdVertexOrdering).Value.
    @groupBegin }
  VERTORDER_UNKNOWN = 0;
  VERTORDER_CLOCKWISE = 1;
  VERTORDER_COUNTERCLOCKWISE = 2;
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdShapeType).Value.
    @groupBegin }
  SHTYPE_UNKNOWN = 0;
  SHTYPE_SOLID = 1;
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdFaceType).Value.
    @groupBegin }
  FACETYPE_UNKNOWN = 0;
  FACETYPE_CONVEX = 1;
  { @groupEnd }

  { Constants for @link(TFontStyleNode.FdFamily).Value.
    @groupBegin }
  FSFAMILY_SERIF = 0 deprecated 'use ffSerif (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  FSFAMILY_SANS = 1 deprecated 'use ffSans (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  FSFAMILY_TYPEWRITER = 2 deprecated 'use ffTypeWriter (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  { @groupEnd }

  { Constants for VRML 1.0 @link(TFontStyleNode_1.FdStyle) flags.
    @groupBegin }
  FSSTYLE_BOLD = 0 deprecated 'use TFontStyleNode.Bold as a simple boolean';
  FSSTYLE_ITALIC = 1 deprecated 'use TFontStyleNode.Italic as a simple boolean';
  { @groupEnd }

  { Constants for TConeNode.FdParts.Flags.
    @groupBegin }
  CONE_PARTS_SIDES = 0;
  CONE_PARTS_BOTTOM = 1;
  { @groupEnd }

  { Constants for TCylinderNode.FdParts.Flags.
    @groupBegin }
  CYLINDER_PARTS_SIDES = 0;
  CYLINDER_PARTS_TOP = 1;
  CYLINDER_PARTS_BOTTOM = 2;
  { @groupEnd }

  { Constants for @link(TTexture2Node_1.FdWrapS).Value and @link(TTexture2Node_1.FdWrapT).Value.
    @groupBegin }
  TEXWRAP_REPEAT = 0 deprecated 'use TAbstractTexture2DNode.RepeatS or TAbstractTexture2DNode.RepeatT boolean properties';
  TEXWRAP_CLAMP = 1 deprecated 'use TAbstractTexture2DNode.RepeatS or TAbstractTexture2DNode.RepeatT boolean properties';
  { @groupEnd }

  DefaultHeightMapScale = 0.01;
  DefaultVRML1CreaseAngle = 0.5;

  DefaultViewpointFieldOfView = Pi / 4;
  DefaultNavigationInfoHeadlight = true;

  DefaultRenderedTextureWidth  = 128;
  DefaultRenderedTextureHeight = 128;

  VRML1Version: TX3DVersion = (Major: 1; Minor: 0);
  VRML2Version: TX3DVersion = (Major: 2; Minor: 0);
  { Latest X3D version supported. }
  X3DVersion: TX3DVersion = (Major: 3; Minor: 2);

  xeClassic = X3DLexer.xeClassic;
  xeXML = X3DLexer.xeXML;

const
  { Minimal values for
    @link(DefaultTriangulationSlices),
    @link(DefaultTriangulationStacks),
    @link(DefaultTriangulationDivisions).

    Note that MinTriangulationSlices can be lower (2), it works,
    but the result isn't really sensible.
    @groupBegin }
  MinTriangulationSlices: Cardinal = 3;
  MinTriangulationStacks: Cardinal = 1;
  MinTriangulationDivisions: Cardinal = 0;
  { @groupEnd }

var
  { Triangulation settings.

    "Slices" divide the circumference of the circle, like the slices of a pizza.
    "Stacks" divide the height of the object, like the stacks of a cake or tower.
    These are used for quadrics - cylinder, cone, sphere and disk.

    "Divisions" divide the cube side.
    This is beneficial for better Gouraud shading.

    You can change these variables only @italic(before using anything)
    from this module. If you want to change them inside VRML/X3D
    file (for example, to affect only part of the scene), use the
    Triangulation node, see
    http://castle-engine.sourceforge.net/x3d_implementation_geometry3d_extensions.php#section_triangulation

    These variables @italic(must) always honour
    @link(MinTriangulationSlices),
    @link(MinTriangulationStacks),
    @link(MinTriangulationDivisions) limits.

    @groupBegin }
  DefaultTriangulationSlices: Cardinal = 30;
  DefaultTriangulationStacks: Cardinal = 20;
  DefaultTriangulationDivisions: Cardinal = 2;
  { @groupEnd }

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

  { URN to indicate BitManagement nodes. This should work, according to
    http://www.bitmanagement.com/developer/contact/examples/layer/index.html
    example EXTERNPROTO. }
  URNBitManagementNodes = 'urn:inet:bitmanagement.de:node:';

  { Standard prefix name for a time sensor to be recognized as a "named animation"
    for TCastleSceneCore.PlayAnimation and friends. }
  DefaultAnimationPrefix = '';

  AllAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput];
  RestrictedAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly];

var
  { Functions registered here will be called when any TX3DNode descendant
    will be destroyed. }
  AnyNodeDestructionNotifications: TNodeDestructionNotificationList;

  { Cache, for all the resources not tied with renderer context. }
  X3DCache: TX3DNodesCache;

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

  This is useful to interpreting TAbstractInterpolatorNode.KeyRange
  and such fields. }
function KeyRange(Key: TSingleList;
  const Fraction: Single; out T: Single): Integer;

{ Free TX3DNode if it is unused (see TX3DNode.FreeIfUnused),
  setting reference to @nil. Analogous to standard FreeAndNil,
  but checks if node is used first. }
procedure FreeIfUnusedAndNil(var Obj);

{ Does this URN indicate an X3D node that is a Castle Game Engine extension.
  This is a helper for implementing @link(TX3DNode.URNMatching). }
function URNMatchingCastle(const URN, ClassX3DType: string): boolean;

{$undef read_interface}

implementation

uses
  { Fonts for Text, FontStyle, AsciiText nodes }
  CastleTextureFont_DjvSans_20,
  {$ifdef CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}
  CastleTextureFont_DjvSansB_20,
  CastleTextureFont_DjvSansO_20,
  CastleTextureFont_DjvSansBO_20,

  CastleTextureFont_DjvMono_20,
  CastleTextureFont_DjvMonoB_20,
  CastleTextureFont_DjvMonoO_20,
  CastleTextureFont_DjvMonoBO_20,

  CastleTextureFont_DjvSerif_20,
  CastleTextureFont_DjvSerifB_20,
  CastleTextureFont_DjvSerifI_20,
  CastleTextureFont_DjvSerifBI_20,
  {$endif CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}

  Math, X3DLoad, CastleZStream, X3DCameraUtils,
  CastleFilesUtils, StrUtils, CastleURIUtils, CastleUnicode,
  CastleLog, CastleScriptParser, CastleDataURI, URIParser, CastleDownload,
  CastleNURBS, CastleQuaternions, CastleCameras, CastleXMLUtils, CastleOpenDocument;

{$define read_implementation}

resourcestring
  SExpectedInterfaceDeclaration =
    'Expected interface declaration (for VRML 2.0: eventIn, eventOut, field or ' +
    'exposedField keyword; for X3D: inputOnly, outputOnly, initializeOnly or ' +
    'inputOutput keyword) but found %s';
  SExpectedFieldType =
    'Expected field type name (like SFVec2f etc.) but found %s';
  SLoadError        = 'Error %s when loading %s from URL "%s": %s';

{$define GeometryNotImplemented :=
  function TGeometryNotImplemented.LocalBoundingBox(State: TX3DGraphTraverseState;
    ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
  begin
    Result := EmptyBox3D;
  end;

  function TGeometryNotImplemented.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
    ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
  begin
    Result := 0;
  end;

  function TGeometryNotImplemented.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
    ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
  begin
    Result := 0;
  end;
}

{$I x3dnodes_extrusion.inc}
{$I x3dnodes_elevationgrid.inc}
{$I x3dnodes_boundingboxes.inc}
{$I x3dnodes_verticesandtrianglescounting.inc}
{$I x3dnodes_coordpolygons.inc}
{$I x3dnodes_eventsengine.inc}
{$I x3dnodes_cone_cylinder.inc}
{$I x3dnodes_sphere.inc}
{$I x3dnodes_box.inc}
{$I x3dnodes_save.inc}
{$I x3dnodes_load.inc}
{$I x3dnodes_encoding_classic.inc}
{$I x3dnodes_encoding_xml.inc}
{$I x3dnodes_generatedtextures.inc}
{$I x3dnodes_node.inc}

{ Nodes from standard X3D components }
{$I x3dnodes_standard_core.inc}
{$I x3dnodes_standard_time.inc}
{$I x3dnodes_standard_grouping.inc}
{$I x3dnodes_standard_networking.inc}
{$I x3dnodes_standard_rendering.inc}
{$I x3dnodes_standard_shape.inc}
{$I x3dnodes_standard_geometry3d.inc}
{$I x3dnodes_standard_geometry2d.inc}
{$I x3dnodes_standard_text.inc}
{$I x3dnodes_standard_sound.inc}
{$I x3dnodes_standard_lighting.inc}
{$I x3dnodes_standard_texturing.inc}
{$I x3dnodes_standard_interpolation.inc}
{$I x3dnodes_standard_interpolation_cubic_bezier.inc}
{$I x3dnodes_standard_pointingdevicesensor.inc}
{$I x3dnodes_standard_keydevicesensor.inc}
{$I x3dnodes_standard_environmentalsensor.inc}
{$I x3dnodes_standard_navigation.inc}
{$I x3dnodes_standard_environmentaleffects.inc}
{$I x3dnodes_standard_geospatial.inc}
{$I x3dnodes_standard_h-anim.inc}
{$I x3dnodes_standard_nurbs.inc}
{$I x3dnodes_standard_dis.inc}
{$I x3dnodes_standard_scripting.inc}
{$I x3dnodes_standard_eventutilities.inc}
{$I x3dnodes_standard_shaders.inc}
{$I x3dnodes_standard_cadgeometry.inc}
{$I x3dnodes_standard_texturing3d.inc}
{$I x3dnodes_standard_cubemaptexturing.inc}
{$I x3dnodes_standard_layering.inc}
{$I x3dnodes_standard_layout.inc}
{$I x3dnodes_standard_rigidbodyphysics.inc}
{$I x3dnodes_standard_picking.inc}
{$I x3dnodes_standard_followers.inc}
{$I x3dnodes_standard_particlesystems.inc}

{ More nodes }
{$I x3dnodes_1.inc}
{$I x3dnodes_inventor.inc}
{$I x3dnodes_97_hanim.inc}
{$I x3dnodes_97_nurbs.inc}
{$I x3dnodes_castle.inc}
{$I x3dnodes_instantreality.inc}
{$I x3dnodes_bitmanagement.inc}

{ Auto-generated nodes code }
{$I auto_generated_node_helpers/x3dnodes_anchor.inc}
{$I auto_generated_node_helpers/x3dnodes_appearance.inc}
{$I auto_generated_node_helpers/x3dnodes_arc2d.inc}
{$I auto_generated_node_helpers/x3dnodes_arcclose2d.inc}
{$I auto_generated_node_helpers/x3dnodes_audioclip.inc}
{$I auto_generated_node_helpers/x3dnodes_background.inc}
{$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
{$I auto_generated_node_helpers/x3dnodes_billboard.inc}
{$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
{$I auto_generated_node_helpers/x3dnodes_booleanfilter.inc}
{$I auto_generated_node_helpers/x3dnodes_booleansequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantoggle.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_boundedphysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_box.inc}
{$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
{$I auto_generated_node_helpers/x3dnodes_cadface.inc}
{$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
{$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
{$I auto_generated_node_helpers/x3dnodes_circle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_clipplane.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
{$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
{$I auto_generated_node_helpers/x3dnodes_collision.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
{$I auto_generated_node_helpers/x3dnodes_colordamper.inc}
{$I auto_generated_node_helpers/x3dnodes_color.inc}
{$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_colorrgba.inc}
{$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_composedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_composedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_composedtexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_coneemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_cone.inc}
{$I auto_generated_node_helpers/x3dnodes_contact.inc}
{$I auto_generated_node_helpers/x3dnodes_contour2d.inc}
{$I auto_generated_node_helpers/x3dnodes_contourpolyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinatedamper.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinatedouble.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_cylinder.inc}
{$I auto_generated_node_helpers/x3dnodes_cylindersensor.inc}
{$I auto_generated_node_helpers/x3dnodes_directionallight.inc}
{$I auto_generated_node_helpers/x3dnodes_disentitymanager.inc}
{$I auto_generated_node_helpers/x3dnodes_disentitytypemapping.inc}
{$I auto_generated_node_helpers/x3dnodes_disk2d.inc}
{$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
{$I auto_generated_node_helpers/x3dnodes_effect.inc}
{$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
{$I auto_generated_node_helpers/x3dnodes_elevationgrid.inc}
{$I auto_generated_node_helpers/x3dnodes_espdutransform.inc}
{$I auto_generated_node_helpers/x3dnodes_explosionemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_extrusion.inc}
{$I auto_generated_node_helpers/x3dnodes_fillproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_floatvertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_fog.inc}
{$I auto_generated_node_helpers/x3dnodes_fontstyle.inc}
{$I auto_generated_node_helpers/x3dnodes_forcephysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
{$I auto_generated_node_helpers/x3dnodes_geocoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_geoelevationgrid.inc}
{$I auto_generated_node_helpers/x3dnodes_geolocation.inc}
{$I auto_generated_node_helpers/x3dnodes_geolod.inc}
{$I auto_generated_node_helpers/x3dnodes_geometadata.inc}
{$I auto_generated_node_helpers/x3dnodes_geoorigin.inc}
{$I auto_generated_node_helpers/x3dnodes_geopositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_geoproximitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_geotouchsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_geotransform.inc}
{$I auto_generated_node_helpers/x3dnodes_geoviewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_group.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimdisplacer.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimhumanoid.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsegment.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsite.inc}
{$I auto_generated_node_helpers/x3dnodes_imagecubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedfaceset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedlineset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedquadset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtriangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_inline.inc}
{$I auto_generated_node_helpers/x3dnodes_integersequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_integertrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiheadlight.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
{$I auto_generated_node_helpers/x3dnodes_kambinavigationinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_kambioctreeproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_keysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_layer.inc}
{$I auto_generated_node_helpers/x3dnodes_layerset.inc}
{$I auto_generated_node_helpers/x3dnodes_layoutgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_layout.inc}
{$I auto_generated_node_helpers/x3dnodes_layoutlayer.inc}
{$I auto_generated_node_helpers/x3dnodes_linepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_lineset.inc}
{$I auto_generated_node_helpers/x3dnodes_loadsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_localfog.inc}
{$I auto_generated_node_helpers/x3dnodes_lod.inc}
{$I auto_generated_node_helpers/x3dnodes_logger.inc}
{$I auto_generated_node_helpers/x3dnodes_material.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix3vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix4vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrixtransform.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataboolean.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatadouble.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatafloat.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatainteger.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataset.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatastring.inc}
{$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_movietexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturetransform.inc}
{$I auto_generated_node_helpers/x3dnodes_navigationinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_normal.inc}
{$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbscurve2d.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbscurve.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbspatchsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbspositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsset.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbssurfaceinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbssweptsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsswungsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbstexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbstrimmedsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationdamper.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_orthoviewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_packagedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_particlesystem.inc}
{$I auto_generated_node_helpers/x3dnodes_pickablegroup.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture.inc}
{$I auto_generated_node_helpers/x3dnodes_planesensor.inc}
{$I auto_generated_node_helpers/x3dnodes_pointemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_pointlight.inc}
{$I auto_generated_node_helpers/x3dnodes_pointpicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_pointset.inc}
{$I auto_generated_node_helpers/x3dnodes_polyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_polylineemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_polypoint2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positionchaser2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positionchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_positiondamper2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positiondamper.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_primitivepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_programshader.inc}
{$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_proximitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_quadset.inc}
{$I auto_generated_node_helpers/x3dnodes_receiverpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_rectangle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_renderedtexture.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
{$I auto_generated_node_helpers/x3dnodes_scalarchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_screeneffect.inc}
{$I auto_generated_node_helpers/x3dnodes_screenfontstyle.inc}
{$I auto_generated_node_helpers/x3dnodes_screengroup.inc}
{$I auto_generated_node_helpers/x3dnodes_script.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderpart.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderprogram.inc}
{$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
{$I auto_generated_node_helpers/x3dnodes_shape.inc}
{$I auto_generated_node_helpers/x3dnodes_signalpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_sound.inc}
{$I auto_generated_node_helpers/x3dnodes_sphere.inc}
{$I auto_generated_node_helpers/x3dnodes_spheresensor.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_spotlight.inc}
{$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_staticgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_stringsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_surfaceemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_switch.inc}
{$I auto_generated_node_helpers/x3dnodes_teapot.inc}
{$I auto_generated_node_helpers/x3dnodes_texcoorddamper2d.inc}
{$I auto_generated_node_helpers/x3dnodes_text3d.inc}
{$I auto_generated_node_helpers/x3dnodes_text.inc}
{$I auto_generated_node_helpers/x3dnodes_texturebackground.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate4d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinategenerator.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_textureproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransform3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransform.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransformmatrix3d.inc}
{$I auto_generated_node_helpers/x3dnodes_timesensor.inc}
{$I auto_generated_node_helpers/x3dnodes_timetrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_toggler.inc}
{$I auto_generated_node_helpers/x3dnodes_touchsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_transform.inc}
{$I auto_generated_node_helpers/x3dnodes_transformsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_transmitterpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset2d.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_twosidedmaterial.inc}
{$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}
{$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpointgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_viewport.inc}
{$I auto_generated_node_helpers/x3dnodes_visibilitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_volumeemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_volumepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_windphysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_worldinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbackgroundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbindablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dchasernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dchildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcolornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcomposedgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddampernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddragsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmentalsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmenttexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfogobject.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfollowernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfontstylenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometricpropertynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgroupingnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinfonode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dkeydevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlayernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlayoutnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dmaterialnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dmetadataobject.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnetworksensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnormalnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbscontrolcurvenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbssurfacegeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparametricgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparticleemitternode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparticlephysicsmodelnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpickableobject.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpicksensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpointingdevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dscriptnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsequencernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshadernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsoundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsoundsourcenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexture2dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexture3dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturecoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturetransformnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtimedependentnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtouchsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtriggernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3durlobject.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dvertexattributenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dviewpointnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dviewportnode.inc}

{ TLightInstance ------------------------------------------------------------- }

function TLightInstance.Contribution(
  const Point: TVector3Single; const PointPlaneNormal: TVector4Single;
  State: TX3DGraphTraverseState;
  const CamPosition: TVector3Single): TVector3Single;
{$I x3dnodes_lightcontribution.inc}

function TLightInstance.ContributionCameraIndependent(
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single)
  :TVector3Single;
{$define CAMERA_INDEP}
{$I x3dnodes_lightcontribution.inc}
{$undef CAMERA_INDEP}

function TLightInstance.Position: TVector4Single;
begin
  if Node is TAbstractPositionalLightNode then
    Result := Vector4Single(Location, 1) else
    Result := Vector4Single(-Direction, 0);
end;

{ TLightInstancesList ----------------------------------------------------- }

function TLightInstancesList.IndexOfNode(Node: TAbstractLightNode): integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TLightInstancesList.FindName(NodeName: string): PLightInstance;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Ptr(I);
    if Result^.Node.X3DName = NodeName then
      Exit;
  end;
  Result := nil;
end;

function TLightInstancesList.Equals(SecondValue: TObject): boolean;

  function LightInstanceEquals(const L1, L2: TLightInstance): boolean;
  begin
    Result := (L1.Node = L2.Node) and
      MatricesPerfectlyEqual(L1.Transform, L2.Transform);

    { No need to compare things like Location or Direction,
      as they are just precalculated based on Node and Transform. }
  end;

var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TLightInstancesList) and
    (TLightInstancesList(SecondValue).Count = Count);
  if Result then
    for I := 0 to Count - 1 do
      if not LightInstanceEquals(L[I], TLightInstancesList(SecondValue).L[I]) then
        Exit(false);
end;

procedure TLightInstancesList.AppendInWorldCoordinates(const AList: TLightInstancesList);
var
  OldCount: Integer;
  I: Integer;
begin
  OldCount := Count;
  Count := Count + AList.Count;
  for I := 0 to AList.Count - 1 do
  begin
    L[OldCount + I] := AList.L[I];
    L[OldCount + I].WorldCoordinates := true;
  end;
end;

{ TClipPlaneList --------------------------------------------------------- }

function TClipPlaneList.IndexOfNode(Node: TClipPlaneNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TClipPlaneList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TClipPlaneList) and
    (TClipPlaneList(SecondValue).Count = Count);

  if Result then
    for I := 0 to Count - 1 do
      if (L[I].Node <> TClipPlaneList(SecondValue).L[I].Node) or
         MatricesPerfectlyEqual(L[I].Transform, TClipPlaneList(SecondValue).L[I].Transform) then
        Exit(false);
end;

{ TX3DGraphTraverseState ---------------------------------------------------- }

var
  { Starting state nodes for TX3DGraphTraverseState.Create. }
  StateDefaultNodes: TTraverseStateLastNodes;

procedure TX3DGraphTraverseState.CommonCreate;
begin
  inherited Create;
  PointingDeviceSensors := TPointingDeviceSensorList.Create(false);
end;

constructor TX3DGraphTraverseState.CreateCopy(Source: TX3DGraphTraverseState);
begin
  CommonCreate;
  Assign(Source);
end;

constructor TX3DGraphTraverseState.Create;
begin
  CommonCreate;

  Transform := IdentityMatrix4Single;
  TransformScale := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  { THAnimHumanoidNode.BeforeTraverse will initialize it anyway.
    But set it also here, just in case we have Joint without surrounding
    Humanoid node. (Otherwise MatrixMultPoint may raise errors in
    THAnimJointNode.ApplyTransform, when multiplying points with 0 matrix,
    testcase is
    view3dscene ~/3dmodels/vrmlx3d/hanim/tecfa.unige.ch/vrml/objects/avatars/blaxxun/kambi_hanim_10_test.wrl.) }
  HumanoidTransform := IdentityMatrix4Single;
  HumanoidInvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  AssignLastNodes(StateDefaultNodes);
end;

destructor TX3DGraphTraverseState.Destroy;
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
  begin
    if FOwnedLastNodes[SN] then FLastNodes.Nodes[SN].Free;
    FLastNodes.Nodes[SN] := nil;
  end;

  FreeAndNil(Lights);
  FreeAndNil(PointingDeviceSensors);
  FreeAndNil(ClipPlanes);
  FreeAndNil(Effects);

  inherited;
end;

procedure TX3DGraphTraverseState.Clear;
begin
  Transform := IdentityMatrix4Single;
  TransformScale := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  HumanoidTransform := IdentityMatrix4Single;
  HumanoidInvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  AssignLastNodes(StateDefaultNodes);
  ShapeNode := nil;
  InsideInline := 0;
  InsidePrototype := 0;
  InsideIgnoreCollision := 0;
  InsideInvisible := 0;
  LocalFog := nil;

  PointingDeviceSensors.Count := 0;
  FreeAndNil(Lights);
  FreeAndNil(ClipPlanes);
  FreeAndNil(Effects);
end;

procedure TX3DGraphTraverseState.AddLight(const Light: TLightInstance);
begin
  if Lights = nil then
    Lights := TLightInstancesList.Create;
  Lights.Add(Light);
end;

function TX3DGraphTraverseState.AddClipPlane: PClipPlane;
begin
  if ClipPlanes = nil then
    ClipPlanes := TClipPlaneList.Create;
  Result := ClipPlanes.Add();
end;

procedure TX3DGraphTraverseState.Assign(Source: TX3DGraphTraverseState);
begin
  AssignTransform(Source);

  TextureTransform := Source.TextureTransform;
  AssignLastNodes(Source.FLastNodes);
  ShapeNode := Source.ShapeNode;
  InsideInline := Source.InsideInline;
  InsidePrototype := Source.InsidePrototype;
  InsideIgnoreCollision := Source.InsideIgnoreCollision;
  InsideInvisible := Source.InsideInvisible;
  Humanoid := Source.Humanoid;
  LocalFog := Source.LocalFog;

  PointingDeviceSensors.Assign(Source.PointingDeviceSensors);

  if Source.Lights <> nil then
  begin
    if Lights = nil then
      Lights := TLightInstancesList.Create;
    Lights.Assign(Source.Lights);
  end else
    FreeAndNil(Lights);
end;

procedure TX3DGraphTraverseState.AssignTransform(
  Source: TX3DGraphTraverseState);
begin
  Transform := Source.Transform;
  TransformScale := Source.TransformScale;
  InvertedTransform := Source.InvertedTransform;
  HumanoidTransform := Source.HumanoidTransform;
  HumanoidInvertedTransform := Source.HumanoidInvertedTransform;

  if PointingDeviceSensors.Count <> 0 then
  begin
    PointingDeviceSensors.Transform := Source.PointingDeviceSensors.Transform;
    PointingDeviceSensors.InvertedTransform := Source.PointingDeviceSensors.InvertedTransform;
  end;

  if Source.ClipPlanes <> nil then
  begin
    if ClipPlanes = nil then
      ClipPlanes := TClipPlaneList.Create;
    ClipPlanes.Assign(Source.ClipPlanes);
  end else
    FreeAndNil(ClipPlanes);

  if Source.Effects <> nil then
  begin
    if Effects = nil then
      Effects := TX3DNodeList.Create(false);
    Effects.Assign(Source.Effects);
  end else
    FreeAndNil(Effects);
end;

function TX3DGraphTraverseState.Equals(SecondValue: TX3DGraphTraverseState;
  const IgnoreTransform: boolean): boolean;
var
  SN: TVRML1StateNode;
begin
  { Many fields are ignored by Equals, as they have no effect on generated
    TGeometryArrays for shapes. Like InsideInline, InsidePrototype and many
    others. }

  Result :=
    (IgnoreTransform or MatricesPerfectlyEqual(Transform, SecondValue.Transform)) and
    MatricesPerfectlyEqual(TextureTransform, SecondValue.TextureTransform) and
    (ShapeNode = SecondValue.ShapeNode) and
    (LocalFog = SecondValue.LocalFog);

  if Result then
  begin
    for SN := Low(SN) to High(SN) do
      if SecondValue.LastNodes.Nodes[SN] <> LastNodes.Nodes[SN] then
        Exit(false);
  end;
end;

function TX3DGraphTraverseState.Texture: TAbstractTextureNode;
begin
  if ShapeNode = nil then
    Result := LastNodes.Texture2 else
    Result := ShapeNode.Texture;
end;

function TX3DGraphTraverseState.BlendMode: TBlendModeNode;
var
  Node: TX3DNode;
begin
  Result := nil;
  if ShapeNode <> nil then
  begin
    Node := ShapeNode.FdAppearance.Value;
    if (Node <> nil) and (Node is TAppearanceNode) then
    begin
      Node := TAppearanceNode(Node).FdBlendMode.Value;
      if (Node <> nil) and (Node is TBlendModeNode) then
        Result := TBlendModeNode(Node);
    end;
  end;
end;

procedure TX3DGraphTraverseState.SetLastNodes(const StateNode: TVRML1StateNode;
  const Node: TX3DNode; const OwnNode: boolean);
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

procedure TX3DGraphTraverseState.AssignLastNodes(
  const NewLastNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    if FLastNodes.Nodes[SN] <> NewLastNodes.Nodes[SN] then
    begin
      if FOwnedLastNodes[SN] then FreeAndNil(FLastNodes.Nodes[SN]);
      FOwnedLastNodes[SN] := false;
      FLastNodes.Nodes[SN] := NewLastNodes.Nodes[SN];
    end;
end;

function TX3DGraphTraverseState.Emission(LightingCalculationOn: boolean): TVector3Single;
var
  M1: TMaterialNode_1;
  M2: TMaterialNode;
begin
  if ShapeNode <> nil then
  begin
    M2 := ShapeNode.Material;
    if M2 <> nil then
    begin
      if LightingCalculationOn then
        Result := M2.FdEmissiveColor.Value else
        Result := M2.FdDiffuseColor.Value;
    end else
    begin
      if LightingCalculationOn then
        { Default VRML 2.0 Material.emissiveColor }
        Result := ZeroVector3Single else
        { Default VRML 2.0 Material.diffuseColor }
        Result := Vector3Single(0.8, 0.8, 0.8);
    end;
  end else
  begin
    M1 := LastNodes.Material;
    if LightingCalculationOn then
      Result := M1.EmissiveColor3Single(0) else
      Result := M1.DiffuseColor3Single(0);
  end;
end;

{ TX3DGraphTraverseStateStack --------------------------------------------- }

constructor TX3DGraphTraverseStateStack.Create;
begin
  inherited;
end;

destructor TX3DGraphTraverseStateStack.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    FreeAndNil(Items[I]);
  inherited;
end;

procedure TX3DGraphTraverseStateStack.GrowItems;
var
  I, OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(Items, OldLen + 8);
  for I := OldLen to Length(Items) - 1 do
    Items[I] := nil;
end;

procedure TX3DGraphTraverseStateStack.PushClear;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

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

procedure TX3DGraphTraverseStateStack.Push;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

  Items[ItemsAllocated].Assign(Items[ItemsAllocated - 1]);
  Inc(ItemsAllocated);
end;

procedure TX3DGraphTraverseStateStack.Push(const Item: TX3DGraphTraverseState);
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

  Items[ItemsAllocated].Assign(Item);
  Inc(ItemsAllocated);
end;

procedure TX3DGraphTraverseStateStack.Pop;
begin
  Dec(ItemsAllocated);
end;

function TX3DGraphTraverseStateStack.Top: TX3DGraphTraverseState;
begin
  Result := Items[ItemsAllocated - 1];
end;

function TX3DGraphTraverseStateStack.PreviousTop: TX3DGraphTraverseState;
begin
  Result := Items[ItemsAllocated - 2];
end;

procedure TX3DGraphTraverseStateStack.Clear;
begin
  ItemsAllocated := 0;
end;

{ TX3DNodesCache ------------------------------------------------------------ }

{ $define DEBUG_CACHE}

var
  CurrentlyLoading: TCastleStringList;

constructor TX3DNodesCache.Create;
begin
  inherited;
  CachedNodes := TCachedNodeList.Create;
end;

destructor TX3DNodesCache.Destroy;
begin
  if CachedNodes <> nil then
  begin
    CachedNodes.Pack; { remove nil items, see InsideFree3DNodeDelete mechanism }
    Assert(CachedNodes.Count = 0, ' Some references to 3D models still exist when freeing TX3DNodesCache');
    FreeAndNil(CachedNodes);
  end;
  inherited;
end;

function TX3DNodesCache.Load3D(const URL: string): TX3DRootNode;
var
  I, Index: Integer;
  C: TCachedNode;
begin
  for I := 0 to CachedNodes.Count - 1 do
  begin
    C := CachedNodes[I];
    if (C <> nil) and (C.URL = URL) then
    begin
      Inc(C.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : 3D model ', URL, ' : ', C.References);
      {$endif}

      Exit(C.Node);
    end;
  end;

  { Add URL to CurrentlyLoading, detecting an infinite loop,
    see https://sourceforge.net/p/castle-engine/tickets/11/ }
  if CurrentlyLoading.IndexOf(URL) <> -1 then
  begin
    raise EX3DError.CreateFmt('3D model references itself (through EXTERNPROTO or Inline), cannot load: %s',
      [URL]);
  end;
  CurrentlyLoading.Add(URL);

  { Initialize Result first, before calling CachedNodes.Add.
    That's because in case Load3D raises exception,
    we don't want to add image to cache (because caller would have
    no way to call Free3D later). }

  Result := X3DLoad.Load3D(URL, false);

  { Remove URL from CurrentlyLoading }
  Index := CurrentlyLoading.IndexOf(URL);
  Assert(Index <> -1);
  CurrentlyLoading.Delete(Index);

  C := TCachedNode.Create;
  CachedNodes.Add(C);
  C.References := 1;
  C.URL := URL;
  C.Node := Result;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : 3D model ', URL, ' : ', 1);
  {$endif}
end;

procedure TX3DNodesCache.Free3D(var Node: TX3DRootNode);
var
  I: Integer;
  C: TCachedNode;
begin
  if Node = nil then Exit;

  for I := 0 to CachedNodes.Count - 1 do
  begin
    C := CachedNodes[I];
    if (C <> nil) and (C.Node = Node) then
    begin
      {$ifdef DEBUG_CACHE}
      Writeln('-- : 3D model ', C.URL, ' : ', C.References - 1);
      {$endif}

      Node := nil;

      if C.References = 1 then
      begin
        if InsideFree3DNodeDelete then
        begin
          { Deleting a node may cause recursive Free3D call that may also remove
            something, and shift our indexes.
            So only nil the item.
            Testcase when it's needed:
            http://www.web3d.org/x3d/content/examples/Basic/CAD/_pages/page02.html }
          FreeAndNil(C.Node);
          CachedNodes[I] := nil;
        end else
        begin
          InsideFree3DNodeDelete := true;
          FreeAndNil(C.Node);
          CachedNodes.Delete(I);
          InsideFree3DNodeDelete := false;
        end;
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt('Free3D: no reference found for 3D model %s',
    [PointerToStr(Node)]);
end;

function TX3DNodesCache.Empty: boolean;
begin
  CachedNodes.Pack; { remove nil items, see InsideFree3DNodeDelete mechanism }
  Result := (inherited Empty) and (CachedNodes.Count = 0);
end;

{ TX3DNodeClassesList ------------------------------------------------------- }

function TX3DNodeClassesList.GetItems(Index: Integer): TX3DNodeClass;
begin
  Result := TX3DNodeClass(inherited Items[Index]);
end;

procedure TX3DNodeClassesList.SetItems(Index: Integer; Value: TX3DNodeClass);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TX3DNodeClassesList.AssignArray(
  const AItemsArray: array of TX3DNodeClass);
var
  I: Integer;
begin
  Count := High(AItemsArray) + 1;
  for I := 0 to High(AItemsArray) do
    Items[I] := AItemsArray[I];
end;

function TX3DNodeClassesList.IndexOf(NodeClass: TX3DNodeClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = NodeClass then
      Exit;
  Result := -1;
end;

function TX3DNodeClassesList.IndexOf(Node: TX3DNode): Integer;
begin
  Result := IndexOf(TX3DNodeClass(Node.ClassType));
end;

function TX3DNodeClassesList.IndexOfAnyAncestor(Node: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Node is TX3DNodeClass(Items[Result]) then
      Exit;
  Result := -1;
end;

procedure TX3DNodeClassesList.Add(Value: TX3DNodeClass);
begin
  inherited Add(Pointer(Value));
end;

procedure TX3DNodeClassesList.AddRegisteredImplementing(Interf: TGUID);
var
  I: Integer;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
    if Supports(NodesManager.Registered[I], Interf) then
      Add(NodesManager.Registered[I]);
end;

{ TSFNode --------------------------------------------------------------------- }

constructor TSFNode.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited;
  Value := nil;

  AllowedChildren := acAll;
  { AllowedChildrenClasses may remain nil in this case }

  FDefaultValue := nil;
  FDefaultValueExists := false;
end;

constructor TSFNode.Create(AParentNode: TX3DNode; const AName: string;
  const AAllowedChildrenClasses: array of TX3DNodeClass;
  AValue: TX3DNode);
begin
  inherited Create(AParentNode, AName);

  { FParentNode is just a copy of inherited (TX3DFieldOrEvent) FParentNode,
    but casted to TX3DNode }
  FParentNode := AParentNode;

  AllowedChildren := acClasses;
  if AllowedChildrenClasses = nil then
    AllowedChildrenClasses := TX3DNodeClassesList.Create;
  AllowedChildrenClasses.AssignArray(AAllowedChildrenClasses);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

constructor TSFNode.Create(AParentNode: TX3DNode; const AName: string;
  AAllowedChildrenClasses: TX3DNodeClassesList;
  AValue: TX3DNode);
begin
  Create(AParentNode, AName, [], AValue);

  Assert(AllowedChildren = acClasses);
  Assert(AllowedChildrenClasses <> nil);
  AllowedChildrenClasses.Assign(AAllowedChildrenClasses);
end;

constructor TSFNode.Create(AParentNode: TX3DNode; const AName: string;
  AnAllowedChildrenInterface: TGUID;
  AValue: TX3DNode);
begin
  inherited Create(AParentNode, AName);

  { FParentNode is just a copy of inherited (TX3DFieldOrEvent) FParentNode,
    but casted to TX3DNode }
  FParentNode := AParentNode;

  AllowedChildren := acInterface;
  AllowedChildrenInterface := AnAllowedChildrenInterface;

  Value := AValue;
  AssignDefaultValueFromValue;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  { To delete Self from DefaultValue.FParentFields, and eventually free DefaultValue. }
  DefaultValue := nil;
  FreeAndNil(AllowedChildrenClasses);
  inherited;
end;

function TSFNode.ChildAllowed(Child: TX3DNode): boolean;
begin
  case AllowedChildren of
    acAll      : Result := true;
    acClasses  : Result := (Child = nil) or (AllowedChildrenClasses.IndexOfAnyAncestor(Child) <> -1);
    acInterface: Result := (Child = nil) or Supports(Child, AllowedChildrenInterface);
    else raise EInternalError.Create('AllowedChildren?');
  end;
end;

function TSFNode.CurrentChildAllowed: boolean;
begin
  Result := ChildAllowed(Value);
end;

procedure TSFNode.WarningIfChildNotAllowed(Child: TX3DNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.X3DType, X3DName]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.X3DType]);
    WritelnWarning('VRML/X3D', S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TSFNode.WarningIfUnusedWeakLink;
begin
  if WeakLink and
     (Value <> nil) and
     (Value.FVRML1Parents.Count = 0) and
     (Value.FParentFields.Count = 0) and
     (Value.KeepExisting = 0) then
  begin
    FValue.FreeIfUnused; // we know it will be freed now
    FValue := nil;
    { do a warning after freeing FValue, to avoid memory leaks in case OnWarning makes exception }
    WritelnWarning('VRML/X3D', Format('A node inside the field "%s" must be already used elsewhere (use USE clause, do not declare a new node here)',
      [NiceName]));
  end;
end;

procedure TSFNode.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  if (Lexer.Token = vtKeyword) and (Lexer.TokenKeyword = vkNULL) then
  begin
    Value := nil;
    Lexer.NextToken;
  end else
  begin
    { This is one case when we can use NilIfUnresolvedUSE = @true }
    Value := ParseNode(Lexer, Reader as TX3DReaderNames, true);
    if Value <> nil then
    begin
      WarningIfChildNotAllowed(Value);
      WarningIfUnusedWeakLink;
    end;
  end;
end;

procedure TSFNode.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
const
  SNull = 'NULL';
var
  UsedNodeFinished: boolean;
  V: TX3DNode;
begin
  { For SFNode and MFNode, X3D XML encoding has special handling:
    field value just indicates the node name, or NULL.
    (other values for SFNode / MFNode cannot be expressed inside
    the attribute). }

  V := (Reader as TX3DReaderNames).Nodes.Bound(AttributeValue, UsedNodeFinished);
  if (V <> nil) and (not UsedNodeFinished) then
  begin
    WritelnWarning('VRML/X3D', Format('Cycles in VRML/X3D graph: SFNode value inside node "%s" refers to the same name', [AttributeValue]));
    Value := nil;
    Exit;
  end;

  Value := V;
  if Value = nil then
  begin
    if AttributeValue <> SNull then
      WritelnWarning('VRML/X3D', Format('Invalid node name for SFNode field: "%s"', [AttributeValue]));
  end else
  begin
    WarningIfChildNotAllowed(Value);
    WarningIfUnusedWeakLink;
  end;
end;

procedure TSFNode.ParseXMLElement(Element: TDOMElement; Reader: TX3DReader);
var
  Child: TX3DNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
begin
  I := Element.ChildrenIterator;
  try
    if I.GetNext then
    begin
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Reader as TX3DReaderNames, true);
      if Child <> nil then
      begin
        Value := Child;
        WarningIfChildNotAllowed(Child);
      end;

      if I.GetNext then
        WritelnWarning('VRML/X3D', Format('X3D field "%s" is SFNode, but it contains more than one XML element (2nd element is "%s")',
          [X3DName, I.Current.TagName]));
    end;
  finally FreeAndNil(I) end;
end;

procedure TSFNode.SaveToStreamValue(Writer: TX3DWriter);
begin
  if Value = nil then
    { For XML encoding, note that the NULL value can only be saved
      as an XML attribute (not child element).
      Also, there's no way to specify containerField for NULL value
      --- and that's Ok, since NULL is the default value of all SFNode fields,
      so it's never actually written in normal cases. }
    Writer.Write('NULL') else
  begin
    { TX3DNode.SaveToStream normally starts from new line with an indent.
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    if Writer.Encoding = xeClassic then
      Writer.DiscardNextIndent;

    Value.NodeSaveToStream(Writer, NameForVersion(Writer.Version));
  end;
end;

function TSFNode.SaveToXmlValue: TSaveToXmlMethod;
begin
  { NULL can only be encoded as an attribute in XML encoding }
  if Value = nil then
    Result := sxAttribute else
    Result := sxChildElement;
end;

function TSFNode.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (Value = DefaultValue);
end;

function TSFNode.Equals(SecondValue: TX3DField;
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
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFNode.AssignValue(Source: TX3DField);
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

procedure TSFNode.SetValue(AValue: TX3DNode);
begin
  if FValue <> AValue then
  begin
    if FValue <> nil then
    begin
      if WeakLink then
        FValue.RemoveDestructionNotification(@DestructionNotification)
      else
        FValue.RemoveParentField(Self);
    end;

    FValue := AValue;

    if AValue <> nil then
    begin
      if WeakLink then
        FValue.AddDestructionNotification(@DestructionNotification)
      else
        FValue.AddParentField(Self);
    end;
  end;
end;

procedure TSFNode.SetDefaultValue(ADefaultValue: TX3DNode);
begin
  if FDefaultValue <> ADefaultValue then
  begin
    if FDefaultValue <> nil then
    begin
      if WeakLink then
        FDefaultValue.RemoveDestructionNotification(@DestructionNotification)
      else
        FDefaultValue.RemoveParentField(Self);
    end;

    FDefaultValue := ADefaultValue;

    if ADefaultValue <> nil then
    begin
      if WeakLink then
        FDefaultValue.AddDestructionNotification(@DestructionNotification)
      else
        FDefaultValue.AddParentField(Self);
    end;
  end;
end;

procedure TSFNode.DestructionNotification(Node: TX3DNode);
begin
  if WeakLink then
  begin
    if FValue = Node then
      FValue := nil;
    if FDefaultValue = Node then
      FDefaultValue := nil;
  end;
end;

procedure TSFNode.SetDefaultValueExists(AValue: boolean);
begin
  FDefaultValueExists := AValue;
end;

class function TSFNode.X3DType: string;
begin
  Result := 'SFNode';
end;

function TSFNode.Enumerate(Func: TEnumerateChildrenFunction): Pointer;
begin
  { checking CurrentChildAllowed is not really necessary here,
    and costs time, because it may do a slow Supports() call }
  //if (Value <> nil) and CurrentChildAllowed and not WeakLink then

  if (Value <> nil) and not WeakLink then
    Result := Func(ParentNode, Value)
  else
    Result := nil;
end;

class function TSFNode.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFNodeEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFNode.Send(const AValue: TX3DNode);
var
  FieldValue: TSFNode;
begin
  { We construct using CreateUndefined constructor,
    to have AllowedChildren = acAll }
  FieldValue := TSFNode.CreateUndefined(ParentNode, X3DName,
    false { Exposed = false, because no need to be true });
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

procedure TSFNode.SetWeakLink(const AValue: boolean);
begin
  if FWeakLink <> AValue then
  begin
    if Value <> nil then
      raise EInternalError.Create('TSFNode.WeakLink cannot change when some node is already assigned');
    FWeakLink := AValue;
  end;
end;

{ TSFNodeEventHelper --------------------------------------------------------- }

procedure TSFNodeEventHelper.Send(const Value: TX3DNode; const Time: TX3DTime);
var
  Field: TX3DField;
begin
  Field := CreateTemp;
  (Field as TSFNode).Value := Value;
  try
    Self.Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFNodeEventHelper.Send(const Value: TX3DNode);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TMFNode -------------------------------------------------------------------- }

constructor TMFNode.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited;
  FItems := TX3DNodeList.Create(false);

  AllowedChildren := acAll;
  { AllowedChildrenClasses may remain nil in this case }

  FDefaultItems := TX3DNodeList.Create(false);
  FDefaultValueExists := false;
end;

constructor TMFNode.Create(AParentNode: TX3DNode; const AName: string;
  const AAllowedChildrenClasses: array of TX3DNodeClass);
begin
  inherited Create(AParentNode, AName);
  FParentNode := AParentNode;

  AllowedChildren := acClasses;
  if AllowedChildrenClasses = nil then
    AllowedChildrenClasses := TX3DNodeClassesList.Create;
  AllowedChildrenClasses.AssignArray(AAllowedChildrenClasses);

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

constructor TMFNode.Create(AParentNode: TX3DNode; const AName: string;
  AAllowedChildrenClasses: TX3DNodeClassesList);
begin
  Create(AParentNode, AName, []);

  Assert(AllowedChildren = acClasses);
  Assert(AllowedChildrenClasses <> nil);
  AllowedChildrenClasses.Assign(AAllowedChildrenClasses);
end;

constructor TMFNode.Create(AParentNode: TX3DNode; const AName: string;
  AnAllowedChildrenInterface: TGUID);
begin
  inherited Create(AParentNode, AName);
  FParentNode := AParentNode;

  AllowedChildren := acInterface;
  AllowedChildrenInterface := AnAllowedChildrenInterface;

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

destructor TMFNode.Destroy;
begin
  Clear;
  ClearDefault;
  FreeAndNil(FItems);
  FreeAndNil(FDefaultItems);
  FreeAndNil(AllowedChildrenClasses);
  inherited;
end;

procedure TMFNode.SaveToStreamValue(Writer: TX3DWriter);
var
  I: Integer;
  N: string;
begin
  N := NameForVersion(Writer.Version);
  case Writer.Encoding of
    xeClassic:
      { We code Count = 0 and Count = 1 cases separately just to get a more
        compact look in these common situations. }
      if Count = 0 then
        Writer.Write('[]') else
      if Count = 1 then
      begin
        { TX3DNode.SaveToStream normally starts from new line with an indent...
          In this case, we want it to start on the same line, so indent must
          be discarded. }
        Writer.DiscardNextIndent;
        Items[0].NodeSaveToStream(Writer, N);
      end else
      begin
        Writer.Writeln('[');
        Writer.IncIndent;
        for I := 0 to Count - 1 do
        begin
          Items[I].NodeSaveToStream(Writer, N);
          Writer.Writeln;
        end;
        Writer.DecIndent;
        Writer.WriteIndent(']');
      end;
    xeXML:
      for I := 0 to Count - 1 do
        Items[I].NodeSaveToStream(Writer, N);
    else raise EInternalError.Create('TMFNode.SaveToStreamValue Encoding?');
  end;
end;

function TMFNode.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxChildElement;
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
    { TFPGObjectList makes sure that increasing count sets new items to nil }
    Items.Count := Value;
  end;
end;

procedure TMFNode.Add(Node: TX3DNode);
begin
  Items.Add(Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.Add(Position: Integer; Node: TX3DNode);
begin
  Items.Insert(Position, Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.Delete(Index: Integer);
begin
  Items[Index].RemoveParentField(Self);
  Items.Delete(Index);
end;

function TMFNode.Remove(const Node: TX3DNode): Integer;
begin
  Result := Items.IndexOf(Node);
  if Result <> -1 then
    Delete(Result);
end;

function TMFNode.Extract(Index: Integer): TX3DNode;
begin
  Result := Items[Index];

  { Instead of calling Result.RemoveParentField(Self), which would possibly
    free Result, we manually call FParentFields.Delete. }
  Result.FParentFields.Remove(Self);

  Items.Delete(Index);
end;

procedure TMFNode.Replace(Index: Integer; Node: TX3DNode);
begin
  if FItems[Index] <> Node then
  begin
    { Replace is the only method that must work even when items are nil }
    if FItems[Index] <> nil then
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

procedure TMFNode.AssignItems(SourceItems: TX3DNodeList);
var
  I: Integer;
begin
  Clear;

  Items.Assign(SourceItems);

  for I := 0 to Count - 1 do
    Items[I].AddParentField(Self);
end;

procedure TMFNode.AssignDefaultItems(SourceItems: TX3DNodeList);
var
  I: Integer;
begin
  ClearDefault;

  DefaultItems.Assign(SourceItems);

  for I := 0 to DefaultItems.Count - 1 do
    DefaultItems[I].AddParentField(Self);
end;

function TMFNode.ChildAllowed(Child: TX3DNode): boolean;
begin
  case AllowedChildren of
    acAll      : Result := true;
    acClasses  : Result := (Child = nil) or (AllowedChildrenClasses.IndexOfAnyAncestor(Child) <> -1);
    acInterface: Result := (Child = nil) or Supports(Child, AllowedChildrenInterface);
    else raise EInternalError.Create('AllowedChildren?');
  end;
end;

procedure TMFNode.WarningIfChildNotAllowed(Child: TX3DNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.X3DType, X3DName]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.X3DType]);
    WritelnWarning('VRML/X3D', S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TMFNode.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure ParseOneItem;
  var
    Node: TX3DNode;
  begin
    Node := ParseNode(Lexer, Reader as TX3DReaderNames, false);
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

procedure TMFNode.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Node: TX3DNode;
  UsedNodeFinished: boolean;
begin
  Node := (Reader as TX3DReaderNames).Nodes.Bound(AttributeValue, UsedNodeFinished);
  if Node = nil then
  begin
    { NULL not allowed for MFNode, unlike the SFNode }
    WritelnWarning('VRML/X3D', Format('Invalid node name for MFNode field: "%s"', [AttributeValue]));
  end else
  if not UsedNodeFinished then
  begin
    WritelnWarning('VRML/X3D', Format('Cycles in VRML/X3D graph: MFNode value inside node "%s" refers to the same name', [AttributeValue]));
  end else
  begin
    Add(Node);
    WarningIfChildNotAllowed(Node);
  end;
end;

procedure TMFNode.ParseXMLElement(Element: TDOMElement; Reader: TX3DReader);
var
  Child: TX3DNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
begin
  { Clear is necessary because MFNode default value may be non-empty,
    when it's defined by a prototype.
    See http://web3d.org/pipermail/x3d-public_web3d.org/2016-May/004771.html }
  Clear;

  I := Element.ChildrenIterator;
  try
    while I.GetNext do
    begin
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Reader as TX3DReaderNames, true);
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

function TMFNode.Equals(SecondValue: TX3DField;
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
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TMFNode.AssignValue(Source: TX3DField);
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

class function TMFNode.X3DType: string;
begin
  Result := 'MFNode';
end;

function TMFNode.Enumerate(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  { checking ChildAllowed is not really necessary here,
    and costs time, because it may do a slow Supports() call }
  //if ChildAllowed(Items[I]) then
  begin
    Result := Func(ParentNode, Items[I]);
    if Result <> nil then Exit;
  end;
end;

function TMFNode.GetItems(const Index: Integer): TX3DNode;
begin
  Result := FItems[Index];
end;

class function TMFNode.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFNodeEvent.Create(AParentNode, AName, AInEvent);
end;

{ TX3DUnknownNode ---------------------------------------------------------------- }

function TX3DUnknownNode.X3DType: string;
begin
 result := fX3DType;
end;

procedure TX3DUnknownNode.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

(*TODO: use "fields" and "isA" VRML 1.0 extensibility features here.

  For now, we just always parse up to the matching closing "}".
  The VRML1Children*Allowed is set to false.

  We should handle:

  - node that has "fields", but not "isA" field (or "isA" points to
    another unknown node name). In this case we can create Fields
    array and parse fields of this node. We will not know how to handle
    this node anyway, but at least we'll know it's fields names and values,
    and we'll be able to save it back to stream.

    The VRML1Children*Allowed set to true? To allow it to work like
    VRML 1.0 Group?

  - node that has "fields" and valid "isA". In this case we can create Fields
    array, parse fields of this node. Then create a helper node
    of type given by "isA", and replace (or add as a child)
    our TX3DUnknownNode by this helper node.

    The VRML1Children*Allowed should be copied from referred "isA" node.
*)
begin
  { In TX3DUnknownNode case, VRML1Children*Allowed must be initialized during
    parsing. }
  VRML1ChildrenAllowed := false;
  VRML1ChildrenParsingAllowed := false;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  ParseIgnoreToMatchingCurlyBracket(Lexer, Reader);

  FBaseUrl := Reader.BaseUrl;

  WritelnWarning('VRML/X3D', 'Unknown node of type "'+ X3DType + '" (named "'+ X3DName +'")');
end;

constructor TX3DUnknownNode.Create(const AName: string; const ABaseUrl: string);
begin
  { Safety check: never create a TX3DUnknownNode instance by this method,
    to not leave FX3DType unset. }
  raise Exception.Create('You cannot create Unknown node using default constructor');
end;

constructor TX3DUnknownNode.CreateUnknown(const AName, ABaseUrl: string; const AX3DType :string);
begin
  inherited Create(AName, ABaseUrl);
  fX3DType := AX3DType;
end;

function TX3DUnknownNode.DeepCopyCreate(
  CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DUnknownNode.CreateUnknown(X3DName, BaseUrl, X3DType);
end;

{ TX3DInterfaceDeclaration -------------------------------------------------- }

constructor TX3DInterfaceDeclaration.Create(AParentNode: TX3DNode);
begin
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TX3DInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FFieldOrEvent);
  FField := nil;
  FEvent := nil;

  inherited;
end;

procedure TX3DInterfaceDeclaration.SetFieldOrEvent(
  const Value: TX3DFieldOrEvent);
begin
  FFieldOrEvent := Value;

  { set FField and FEvent, for fast access to them }
  if FFieldOrEvent = nil then
  begin
    FField := nil;
    FEvent := nil;
  end else
  if FFieldOrEvent is TX3DField then
  begin
    FField := TX3DField(FFieldOrEvent);
    FEvent := nil;
  end else
  begin
    Assert(FFieldOrEvent is TX3DEvent);
    FField := nil;
    FEvent := TX3DEvent(FFieldOrEvent);
  end;
end;

procedure TX3DInterfaceDeclaration.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames;
  FieldValue, IsClauseAllowed: boolean);
var
  X3DType: string;
  Access: TX3DAccessType;
  FieldType: TX3DFieldClass;
  ParsedName: string;
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
      else raise EX3DParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
    end;
  end else
    raise EX3DParserError.Create(
      Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'field type (for interface declaration)');
  X3DType := Lexer.TokenName;
  FieldType := X3DFieldsManager.X3DTypeToClass(X3DType);
  if FieldType = nil then
    raise EX3DParserError.Create(
      Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'name (for interface declaration)');
  ParsedName := Lexer.TokenName;

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := FieldType.CreateEvent(ParentNode, ParsedName, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      FieldOrEvent := FieldType.CreateUndefined(ParentNode, ParsedName, Access = atInputOutput);
    else raise EInternalError.Create('Access ? in TX3DInterfaceDeclaration.Parse');
  end;

  Lexer.NextToken;

  if Event <> nil then
  begin
    if IsClauseAllowed then
      Event.Parse(Lexer);
  end else
  begin
    if FieldValue then
      Field.Parse(Lexer, Reader, IsClauseAllowed) else
    if IsClauseAllowed then
      Field.ParseIsClause(Lexer);
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

procedure TX3DInterfaceDeclaration.ParseXML(
  Element: TDOMElement; Reader: TX3DReaderNames; FieldValue: boolean);
var
  Access: TX3DAccessType;
  AccessIndex: Integer;
  AccessName: string;
  FieldX3DType: string;
  FieldType: TX3DFieldClass;
  ParsedName, FieldActualValue: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  { calculate Access }
  if Element.AttributeString('accessType', AccessName) then
  begin
    AccessIndex := ArrayPosStr(AccessName,
      ['inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput']);
    if AccessIndex <> -1 then
      Access := TX3DAccessType(AccessIndex) else
      raise EX3DXmlError.CreateFmt('Access type "%s" unknown', [AccessName]);
  end else
    raise EX3DXmlError.Create('Missing access type in X3D interface declaration');

  { calculate FieldType }
  if Element.AttributeString('type', FieldX3DType) then
  begin
    FieldType := X3DFieldsManager.X3DTypeToClass(FieldX3DType);
    if FieldType = nil then
      raise EX3DXmlError.CreateFmt('Field type "%s" unknown', [FieldX3DType]);
  end else
    raise EX3DXmlError.Create('Missing field type in X3D interface declaration');

  if not Element.AttributeString('name', ParsedName) then
    raise EX3DXmlError.Create('Missing name in X3D interface declaration');

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := FieldType.CreateEvent(ParentNode, ParsedName, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      FieldOrEvent := FieldType.CreateUndefined(ParentNode, ParsedName, Access = atInputOutput);
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
      if Element.AttributeString('value', FieldActualValue) then
        Field.ParseXMLAttribute(FieldActualValue, Reader) else
        Field.ParseXMLElement(Element, Reader);
    end;

    { Classic VRML parser has here
        else if IsClauseAllowed then Field.ParseIsClause(Lexer);
      but for X3D XML encoding this is not needed, see comments above. }
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

function TX3DInterfaceDeclaration.CopyFieldOrEvent(
  NewParentNode: TX3DNode): TX3DFieldOrEvent;
var
  F: TX3DField absolute Result;
  E: TX3DEvent absolute Result;
begin
  if Field <> nil then
  begin
    { F := copy of Field }
    F := TX3DFieldClass(Field.ClassType).CreateUndefined(NewParentNode,
      Field.X3DName, Field.Exposed);
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

      See ../../../demo_models/x3d/proto_sfnode_default.x3dv
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
    E := Event.FieldClass.CreateEvent(NewParentNode, Event.X3DName, Event.InEvent);
    { Although above constructor already copied most event properties,
      some were omitted (like IsClauseNames --- important for Script with
      eventIn/out events with IS clauses inside prototypes).
      Assign call below takes care of them. }
    E.Assign(Event);
  end else
    raise EInternalError.Create('interface declaration but no Field or Event');

  Result.ParentInterfaceDeclaration := nil;
end;

procedure TX3DInterfaceDeclaration.AddFieldOrEvent(
  Node: TX3DNode);
begin
  if Field <> nil then
    Node.AddField(Field) else
  begin
    Assert(Event <> nil);
    Node.AddEvent(Event);
  end;
end;

procedure TX3DInterfaceDeclaration.CopyAndAddFieldOrEvent(
  Node: TX3DNode);
var
  Copy: TX3DFieldOrEvent;
begin
  Copy := CopyFieldOrEvent(Node);
  if Copy is TX3DField then
    Node.AddField(TX3DField(Copy)) else
  begin
    Assert(Copy is TX3DEvent);
    Node.AddEvent(TX3DEvent(Copy));
  end;
end;

procedure TX3DInterfaceDeclaration.IDeclSaveToStream(
  Writer: TX3DWriter; FieldValue: boolean);

  function ATName(const AccessType: TX3DAccessType): string;
  const
    Names: array
      [ boolean { is it X3D or XML encoding ? },
        TX3DAccessType] of string =
      ( ('eventIn', 'eventOut', 'field', 'exposedField'),
        ('inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput') );
  begin
    Result := Names[(Writer.Encoding = xeXML) or (Writer.Version.Major >= 3),
      AccessType];
  end;

var
  N: string;
begin
  N := FieldOrEvent.NameForVersion(Writer.Version);

  case Writer.Encoding of
    xeClassic:
      if Event <> nil then
      begin
        if Event.InEvent then
          Writer.WriteIndent(ATName(atInputOnly) + ' ') else
          Writer.WriteIndent(ATName(atOutputOnly) + ' ');
        Writer.Write(Event.FieldClass.X3DType + ' ');
        if Event.IsClauseNamesCount <> 0 then
        begin
          Writer.DiscardNextIndent;
          { Note that there may be many IS clauses. This will still work Ok:
            first IS clause will "belong" to this interface declaration,
            the rest will look like normal IS clauses. }
          Event.SaveToStreamClassicIsClauses(Writer);
        end else
          Writer.Writeln(N);
      end else
      begin
        if Field.Exposed then
          Writer.WriteIndent(ATName(atInputOutput) + ' ') else
          Writer.WriteIndent(ATName(atInitializeOnly) + ' ');
        Writer.Write(Field.X3DType + ' ');

        { When saving from interface declaration, you can only
          1. write sole field name
          2. write field name + value (if FieldValue = @true)
          3. write field name + IS clause (only one allowed) }

        if ( FieldValue and
             (not Field.ValueFromIsClause) and
             (Field.IsClauseNamesCount = 0) ) then
        begin
          { Field.SaveToStream normally starts from new line with an indent...
            In this case, we want it to start on the same line, so indent must
            be discarded. }
          Writer.DiscardNextIndent;
          Field.FieldSaveToStream(Writer, true);
          { In this case, Writer.Writeln will be done by Field.SaveToStream.
            (we pass SaveWhenDefault anyway, so we can be sure that
            this newline will be done). }
        end else

        if Field.IsClauseNamesCount <> 0 then
        begin
          Writer.DiscardNextIndent;
          { Note that there may be many IS clauses. This will still work Ok:
            first IS clause will "belong" to this interface declaration,
            the rest will look like normal IS clauses. }
          Field.SaveToStreamClassicIsClauses(Writer);
        end else

        begin
          Writer.Writeln(N);
        end;
      end;
    xeXML:
      { We don't save IS clauses here for XML encoding. They must be saved
        inside containing TX3DNode. }
      if Event <> nil then
      begin
        Writer.WritelnIndent(Format('<field accessType=%s type=%s name=%s />',
          [ Iff(Event.InEvent,
              StringToX3DXml(ATName(atInputOnly)),
              StringToX3DXml(ATName(atOutputOnly))),
            StringToX3DXml(Event.FieldClass.X3DType),
            StringToX3DXml(N) ]));
      end else
      begin
        Writer.WriteIndent(Format('<field accessType=%s type=%s name=%s',
          [ Iff(Field.Exposed,
              StringToX3DXml(ATName(atInputOutput)),
              StringToX3DXml(ATName(atInitializeOnly))),
            StringToX3DXml(Field.X3DType),
            StringToX3DXml(N) ]));

        if ( FieldValue and
             (not Field.ValueFromIsClause) and
             (Field.IsClauseNamesCount = 0) ) then
        begin
          if Field.SaveToXml in [sxAttribute, sxAttributeCustomQuotes] then
          begin
            Writer.Write(' value=');
            Field.FieldSaveToStream(Writer, true, true);
            Writer.Writeln(' />');
          end else
          begin
            Writer.Writeln('>');
            { Parameter XmlAvoidSavingNameBeforeValue doesn't matter here }
            Field.FieldSaveToStream(Writer, true);
            Writer.WritelnIndent('</field>');
          end;
        end else
          { no field value, no IS clauses.
            This can happen for field/event declaration inside <ExternProto>.
            Just close <field> element. }
          Writer.Writeln(' />');
      end;

    else raise EInternalError.Create('TX3DInterfaceDeclaration.IDeclSaveToStream Encoding?');
  end;
end;

procedure TX3DInterfaceDeclaration.SaveToStream(Writer: TX3DWriter);
begin
  IDeclSaveToStream(Writer, true);
end;

function TX3DInterfaceDeclaration.AccessType: TX3DAccessType;
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

function TX3DInterfaceDeclaration.DeepCopy(
  NewParentNode: TX3DNode;
  CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
begin
  Result := TX3DInterfaceDeclaration.Create(NewParentNode);
  Result.FieldOrEvent := CopyFieldOrEvent(NewParentNode);
  Result.FieldOrEvent.ParentInterfaceDeclaration := Result;
end;

{ TX3DInterfaceDeclarationList --------------------------------------------- }

function TX3DInterfaceDeclarationList.TryFindName(
  const Name: string): TX3DFieldOrEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FieldOrEvent;
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindFieldName(const Name: string): TX3DField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DField then
    begin
      Result := Items[I].Field;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindEventName(const Name: string): TX3DEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DEvent then
    begin
      Result := Items[I].Event;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

{ TX3DPrototypeNode --------------------------------------------------------- }

constructor TX3DPrototypeNode.Create(const AName, ABaseUrl: string);
begin
  raise EInternalError.Create('TX3DPrototypeNode node must be created' +
    ' using CreatePrototypeNode, never default constructor');
end;

constructor TX3DPrototypeNode.CreatePrototypeNode(
  const AName, ABaseUrl: string;
  APrototype: TX3DPrototypeBase);
var
  I: TX3DInterfaceDeclaration;
  Index: Integer;
  ProtoInitial: TX3DPrototypeBase;
begin
  inherited Create(AName, ABaseUrl);
  FPrototype := APrototype;

  ProtoInitial := Prototype;

  if (ProtoInitial is TX3DExternalPrototype) and
     (TX3DExternalPrototype(ProtoInitial).ReferencedPrototype <> nil) then
    { It's important to use ReferencedPrototype, not just current
      Prototype, in this case. That's because field's default values
      should be set by constructor (before parsing the specified
      fields), and TX3DExternalPrototype doesn't have default values
      available.

      If ReferencedPrototype = nil (e.g. because couldn't
      be loaded) then Instantiate will not be able to instantiate
      it anyway (and will produce appropriate WritelnWarning). }
    ProtoInitial := TX3DExternalPrototype(ProtoInitial).ReferencedPrototype;

  for Index := 0 to ProtoInitial.InterfaceDeclarations.Count - 1 do
  begin
    I := ProtoInitial.InterfaceDeclarations.Items[Index];
    I.CopyAndAddFieldOrEvent(Self);
  end;
end;

function TX3DPrototypeNode.DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DPrototypeNode.CreatePrototypeNode(X3DName, BaseUrl,
    { TODO: for now, we don't copy proto, instead simply passing the same
      proto reference. }
    Prototype);
end;

function TX3DPrototypeNode.X3DType: string;
begin
  Result := Prototype.X3DName;
end;

procedure TX3DPrototypeNode.FieldOrEventHandleIsClause(
  Destination, Source: TX3DFieldOrEvent;
  NewIsClauseNames: TCastleStringList);
var
  DestinationField, SourceField: TX3DField;
  DestinationEvent, SourceEvent: TX3DEvent;
  Route: TX3DRoute;
  I: Integer;
const
  InEventName: array [boolean] of string = ( 'output', 'input' );
begin
  { When Source.IsClauseNamesCount <> 0, then we're expanded
    within the definition of another prototype.
    So Destination.IsClauseNames referers to Source
    (from current Prototype), but Source.IsClauseNames refers to yet
    another, enclosing, prototype (that will be expanded later --- for
    now we're within enclosing prototype definition).

    See comments in the interface for more. }

  { Intuitively: NewIsClauseNames.AddStrings(Source.IsClauseNames); }
  for I := 0 to Source.IsClauseNamesCount - 1 do
    NewIsClauseNames.Add(Source.IsClauseNames[I]);

  if Source is TX3DField then
  begin
    Assert(Destination is TX3DField);
    SourceField := Source as TX3DField;
    DestinationField := Destination as TX3DField;

    try
      DestinationField.AssignValue(SourceField);
      DestinationField.ValueFromIsClause := true;
    except
      on E: EX3DFieldAssignInvalidClass do
      begin
        WritelnWarning('VRML/X3D', Format('Within prototype "%s", ' +
          'field of type %s (named "%s") references ' +
          '(by "IS" clause) field of different type %s (named "%s")',
          [Prototype.X3DName,
           DestinationField.X3DType,
           Destination.X3DName,
           SourceField.X3DType,
           Source.X3DName]));
      end;
      on E: EX3DFieldAssign do
      begin
        WritelnWarning('VRML/X3D', Format('Error when expanding prototype "%s": ',
          [Prototype.X3DName]) + E.Message);
      end;
    end;
  end else
  if Source is TX3DEvent then
  begin
    Assert(Destination is TX3DEvent);
    SourceEvent := Source as TX3DEvent;
    DestinationEvent := Destination as TX3DEvent;

    if SourceEvent.InEvent <> DestinationEvent.InEvent then
    begin
      WritelnWarning('VRML/X3D', Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.X3DName,
          InEventName[DestinationEvent.InEvent],
          InEventName[SourceEvent.InEvent] ]));
      Exit;
    end;

    if SourceEvent.FieldClass <> DestinationEvent.FieldClass then
    begin
      WritelnWarning('VRML/X3D', Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.X3DName,
          DestinationEvent.FieldClass.X3DType,
          SourceEvent.FieldClass.X3DType ]));
      Exit;
    end;

    Route := TX3DRoute.Create;
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

      AddRoute(Route);
    except
      FreeAndNil(Route);
      raise;
    end;
  end;
end;

function TX3DPrototypeNode.InstantiateIsClauses(Node, Child: TX3DNode): Pointer;

  { In terminology of VRML/X3D specs,
    InstanceField/Event is the one in "prototype definition"
    (that is, inside prototype content) and
    OutField/Event is the one in "prototype declaration".

    This is where we implement table "Rules for mapping PROTOTYPE
    declarations to node instances" in specifications about
    "PROTO definition semantics". }

  procedure ExpandEvent(InstanceEvent: TX3DEvent); forward;

  procedure ExpandField(InstanceField: TX3DField);
  var
    OurField: TX3DField;
    OurEvent: TX3DEvent;
    OurFieldIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TCastleStringList;
  begin
    if InstanceField.IsClauseNamesCount <> 0 then
    begin
      NewIsClauseNames := TCastleStringList.Create;
      try
        for I := 0 to InstanceField.IsClauseNamesCount - 1 do
        begin
          IsClauseName := InstanceField.IsClauseNames[I];
          OurFieldIndex := IndexOfField(IsClauseName);
          if OurFieldIndex <> -1 then
          begin
            OurField := Fields[OurFieldIndex];
            FieldOrEventHandleIsClause(InstanceField, OurField, NewIsClauseNames);

            if InstanceField.Exposed and OurField.Exposed then
            begin
              { We have to "break" exposed fields inside TX3DPrototypeNode,
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
                  is useless, as field's value in TX3DPrototypeNode
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
              WritelnWarning('VRML/X3D', Format('Within prototype "%s", exposed field "%s" references (by "IS" clause) non-existing field/event name "%s"',
                [Prototype.X3DName, InstanceField.X3DName, IsClauseName]));
          end else
            WritelnWarning('VRML/X3D', Format('Within prototype "%s", field "%s" references (by "IS" clause) non-existing field "%s"',
              [Prototype.X3DName, InstanceField.X3DName, IsClauseName]));
        end;

        InstanceField.IsClauseNamesAssign(NewIsClauseNames);
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

  procedure ExpandEvent(InstanceEvent: TX3DEvent);
  var
    OurEvent: TX3DEvent;
    OurEventIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TCastleStringList;
  begin
    if InstanceEvent.IsClauseNamesCount <> 0 then
    begin
      NewIsClauseNames := TCastleStringList.Create;
      try
        for I := 0 to InstanceEvent.IsClauseNamesCount - 1 do
        begin
          IsClauseName := InstanceEvent.IsClauseNames[I];

          { Event from prototype definition can only correspond to the
            same event type of prototype declaration. It cannot reference
            implicit event (within exposed field) of prototype declaration.
            Which is good, since otherwise it would be difficult to implement
            (Self (TX3DPrototypeNode) is used to keep events, but it doesn't
            keep actual field values (there are kept within actual expanded nodes).

            This also means that searching below by Events.IndexOf is Ok,
            no need to use AnyEvent to search. }

          OurEventIndex := IndexOfEvent(IsClauseName);
          if OurEventIndex <> -1 then
          begin
            OurEvent := Events[OurEventIndex];
            FieldOrEventHandleIsClause(InstanceEvent, OurEvent, NewIsClauseNames);
          end else
            WritelnWarning('VRML/X3D', Format('Within prototype "%s", event "%s" references (by "IS" clause) non-existing event "%s"',
              [Prototype.X3DName, InstanceEvent.X3DName, IsClauseName]));
        end;

        InstanceEvent.IsClauseNamesAssign(NewIsClauseNames);
      finally FreeAndNil(NewIsClauseNames) end;
    end;
  end;

var
  I: Integer;
begin
  Result := nil;

  { The NeedsInstantiateIsClause flag is needed for InstantiateIsClauses.
     By checking this flag (and setting back to @false after handling):

     1. We ensure that every node is expanded only once (otherwise,
        in case of funny DEF/USE usage, we could process the same node twice
        by InstantiateIsClauses).

     2. We avoid instantiating "IS" clauses in nodes outside the NodeCopy.
        Such nodes with "IS" clauses may be added by InstantiateIsClauses,
        when our own PROTO has fields with SFNode / MFNode type,
        and they contain "IS" clauses that should be handled by other
        (outer) proto. See e.g. demo_models/vrml_2/proto_nested_expand.wrl
        testcase.

     (The 2nd point could also be fixed by simply moving recursive call to
     Child.DirectEnumerateAll(@InstantiateIsClauses) to the beginning,
     not the end, of InstantiateIsClauses. But this would leave 1st point
     unfixed.) }

  if not Child.NeedsInstantiateIsClause then Exit;
  Child.NeedsInstantiateIsClause := false;

  for I := 0 to Child.FieldsCount - 1 do
    ExpandField(Child.Fields[I]);

  for I := 0 to Child.EventsCount - 1 do
    ExpandEvent(Child.Events[I]);

  Child.DirectEnumerateAll(@Self.InstantiateIsClauses);
end;

function TX3DPrototypeNode.PrepareInstantiateIsClause(
  Node, Child: TX3DNode): Pointer;
begin
  Result := nil;
  if not Child.NeedsInstantiateIsClause then
  begin
    Child.NeedsInstantiateIsClause := true;
    Child.DirectEnumerateAll(@Self.PrepareInstantiateIsClause);
  end;
end;

function TX3DPrototypeNode.Instantiate: TX3DNode;

  procedure InstantiateNonExternalPrototype(Proto: TX3DPrototype);
  var
    NodeCopy, NewPrototypeInstanceHelpers: TX3DRootNode;
  begin
    { We want to copy the whole Proto.Node, instead of copying separately
      Proto.Node.FdChildren[0], Proto.Node.FdChildren[1] etc.
      This way, DEF / USE links, routes links (internal, for nested
      protos "IS" clauses, and non-internal) are preserved as they should. }

    NodeCopy := Proto.Node.DeepCopy as TX3DRootNode;

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

    if NodeCopy.FdChildren.Count = 0 then
    begin
      { If exception occurs before NodeCopy is connected to Result,
        NodeCopy should be simply freed. }
      FreeAndNil(NodeCopy);
      raise EX3DPrototypeInstantiateError.CreateFmt(
        'Prototype "%s" has no nodes, cannot instantiate',
        [Proto.X3DName]);
    end;

    { ExtractChild/Item methods were really invented specially for this case.

      We have to remove Result from NodeCopy, to avoid cycles
      (that can cause mem leaks) because Result.FPrototypeInstanceHelpers
      has to keep pointer to NodeCopy.

      At the same time, Result must not be freed here because of ref count = 0... }
    Result := NodeCopy.FdChildren.Extract(0);

    Assert(Result.PrototypeInstance =
      (Result.PrototypeInstanceSourceNode <> nil));
    Assert(Result.PrototypeInstance or
      (Result.PrototypeInstanceHelpers = nil));

    { NewPrototypeInstanceHelpers is used to keep the rest of
      NodeCopy.FdChildren[1...] that should accompany this node. }
    NewPrototypeInstanceHelpers := NodeCopy;

    Result.X3DName := X3DName;

    (* Result and NodeCopy may come from another prototype.
       For example,

         PROTO SimpleText [
           inputOutput MFString string ""
         ] { Shape { geometry Text { string IS string } } }

         PROTO PressedText [
           inputOutput MFString string ""
         ] { SimpleText { string IS string } }

         PressedText { string "zero" }

       (this is a simplified part of ../../../demo_models/x3d/key_sensor.x3dv
       file). In such case, when PressedText, you may get Shape that already
       was expanded from SimpleText. So NodeCopy will have PrototypeInstance
       = true. Or NodeCopy may be TX3DRootNode wrapper, then it's
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
       TX3DPrototypeNode.PrototypeInstanceSourceNode and ...Helpers
       fiels for this. (They should be empty right now, as one
       TX3DPrototypeNode is expanded only once.)
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

  procedure InstantiateExternalPrototype(Proto: TX3DExternalPrototype);
  begin
    if Proto.ReferencedPrototype = nil then
      raise EX3DPrototypeInstantiateError.CreateFmt(
        'External prototype "%s" cannot be loaded, so cannot instantiate nodes using it',
        [Proto.X3DName]);

    { Note that we do not check whether ReferencedPrototype actually
      has the same fields/events as declared for externproto.
      Although when expanding IS clauses, missing declarations
      or incorrect types or field/event will be caught, so the necessary
      things will be checked when expanding. }

    InstantiateNonExternalPrototype(Proto.ReferencedPrototype);
  end;

begin
  if Prototype is TX3DPrototype then
    InstantiateNonExternalPrototype(Prototype as TX3DPrototype) else
  if Prototype is TX3DExternalPrototype then
    InstantiateExternalPrototype(Prototype as TX3DExternalPrototype) else
    raise EX3DPrototypeInstantiateError.CreateFmt(
      'Cannot instantiate prototype "%s": '+
      'unknown prototype class %s', [Prototype.X3DName, Prototype.ClassName]);
end;

{ TX3DPrototypeBase --------------------------------------------------------- }

constructor TX3DPrototypeBase.Create;
begin
  inherited;
  FInterfaceDeclarations := TX3DInterfaceDeclarationList.Create(true);
end;

destructor TX3DPrototypeBase.Destroy;
begin
  FreeAndNil(FInterfaceDeclarations);
  inherited;
end;

procedure TX3DPrototypeBase.ParseInterfaceDeclarations(ExternalProto: boolean;
  Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  I: TX3DInterfaceDeclaration;
begin
  while Lexer.Token <> vtCloseSqBracket do
  begin
    I := TX3DInterfaceDeclaration.Create(nil);
    InterfaceDeclarations.Add(I);

    if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(AllAccessTypes)) then
    begin
      I.Parse(Lexer, Reader, not ExternalProto, false);
    end else
      raise EX3DParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
  end;

  { eat "]" token }
  Lexer.NextToken;

  FBaseUrl := Reader.BaseUrl;
end;

procedure TX3DPrototypeBase.ParseInterfaceDeclarationsXML(ExternalProto: boolean;
  Element: TDOMElement; Reader: TX3DReaderNames);
var
  I: TX3DInterfaceDeclaration;
  Iter: TXMLElementIterator;
begin
  Iter := Element.ChildrenIterator;
  try
    while Iter.GetNext do
    begin
      if Iter.Current.TagName = 'field' then
      begin
        I := TX3DInterfaceDeclaration.Create(nil);
        InterfaceDeclarations.Add(I);
        I.ParseXML(Iter.Current, Reader, not ExternalProto);
      end else
        WritelnWarning('VRML/X3D', 'X3D XML: only <field> elements expected in prototype interface');
    end;
  finally FreeAndNil(Iter) end;
end;

procedure TX3DPrototypeBase.SaveInterfaceDeclarationsToStream(
  Writer: TX3DWriter; ExternalProto: boolean);
var
  I: Integer;
begin
  if Writer.Encoding = xeClassic then Writer.Writeln('[');
  Writer.IncIndent;
  for I := 0 to InterfaceDeclarations.Count - 1 do
    InterfaceDeclarations.Items[I].IDeclSaveToStream(Writer, not ExternalProto);
  Writer.DecIndent;
  if Writer.Encoding = xeClassic then Writer.WritelnIndent(']');
end;

{ TX3DPrototype ------------------------------------------------------------- }

destructor TX3DPrototype.Destroy;
begin
  FreeAndNil(FNode);
  inherited;
end;

procedure TX3DPrototype.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  OldReader: TX3DReaderNames;
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FX3DName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(false, Lexer, Reader);

  Lexer.CheckTokenIs(vtOpenCurlyBracket);

  Lexer.NextToken;
  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Reader.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldReader := Reader;
  Reader := TX3DReaderNames.CreateCopy(true, OldReader);
  try
    Reader.Prototypes.Assign(OldReader.Prototypes);
    FNode := ParseStatements(Lexer, Reader, vtCloseCurlyBracket, false);
  finally
    FreeAndNil(Reader);
    Reader := OldReader;
  end;

  { consume last vtCloseCurlyBracket, ParseStatements doesn't do it }
  Lexer.NextToken;

  Reader.Prototypes.Bind(Self);
end;

procedure TX3DPrototype.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  OldReader: TX3DReaderNames;
  NewName: string;
  E: TDOMElement;
begin
  BaseUrl := Reader.BaseUrl;

  if Element.AttributeString('name', NewName) then
    X3DName := NewName
  else
    raise EX3DXmlError.Create('Missing "name" for <ProtoDeclare> element');

  E := Element.ChildElement('ProtoInterface', false);
  if E <> nil then
    ParseInterfaceDeclarationsXML(false, E, Reader);

  E := Element.ChildElement('ProtoBody', false);
  if E = nil then
    raise EX3DXmlError.CreateFmt('Missing <ProtoBody> inside <ProtoDeclare> element of prototype "%s"', [X3DName]);

  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Reader.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldReader := Reader;
  Reader := TX3DReaderNames.CreateCopy(true, OldReader);
  try
    Reader.Prototypes.Assign(OldReader.Prototypes);
    FNode := ParseStatements(E, false, nil, Reader);
  finally
    FreeAndNil(Reader);
    Reader := OldReader;
  end;

  Reader.Prototypes.Bind(Self);
end;

procedure TX3DPrototype.SaveToStream(Writer: TX3DWriter);
var
  OldNodeNames: TX3DNodeNames;
  WriterNames: TX3DWriterNames;
begin
  case Writer.Encoding of
    xeClassic: Writer.WriteIndent('PROTO ' + X3DName + ' ');
    xeXML    : Writer.WritelnIndent('<ProtoDeclare name=' + StringToX3DXml(X3DName) + '>');
    else raise EInternalError.Create('TX3DPrototype.SaveToStream Encoding?');
  end;

  if Writer.Encoding = xeXML then
  begin
    Writer.IncIndent;
    Writer.WritelnIndent('<ProtoInterface>');
    Writer.IncIndent;
  end;

  SaveInterfaceDeclarationsToStream(Writer, false);

  if Writer.Encoding = xeXML then
  begin
    Writer.DecIndent;
    Writer.WritelnIndent('</ProtoInterface>');
  end;

  WriterNames := Writer as TX3DWriterNames;

  { Inside prototype has it's own DEF/USE scope. }
  OldNodeNames := WriterNames.NodeNames;
  WriterNames.NodeNames := TX3DNodeNames.Create(false);
  try
    case Writer.Encoding of
      xeClassic: Writer.WritelnIndent('{');
      xeXML    : Writer.WritelnIndent('<ProtoBody>');
      else raise EInternalError.Create('TX3DPrototype.SaveToStream 2 Encoding?');
    end;
    { Node may be TX3DRootNode here, that's OK,
      TX3DRootNode.SaveToStream will magically handle this right. }
    Writer.IncIndent;
    Node.SaveToStream(Writer);
    Writer.DecIndent;
    case Writer.Encoding of
      xeClassic: Writer.WritelnIndent('}');
      xeXML    : Writer.WritelnIndent('</ProtoBody>');
      else raise EInternalError.Create('TX3DPrototype.SaveToStream 3 Encoding?');
    end;
  finally
    FreeAndNil(WriterNames.NodeNames);
    WriterNames.NodeNames := OldNodeNames;
  end;

  if Writer.Encoding = xeXML then
  begin
    Writer.DecIndent;
    Writer.WritelnIndent('</ProtoDeclare>');
  end;
end;

{ TX3DExternalPrototype ----------------------------------------------------- }

constructor TX3DExternalPrototype.Create;
begin
  inherited;
  FURLList := TMFString.Create(nil, '', []);
end;

destructor TX3DExternalPrototype.Destroy;
begin
  UnloadReferenced;
  FreeAndNil(FURLList);
  inherited;
end;

procedure TX3DExternalPrototype.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FX3DName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(true, Lexer, Reader);

  URLList.Parse(Lexer, Reader, false);

  Reader.Prototypes.Bind(Self);

  LoadReferenced;
end;

procedure TX3DExternalPrototype.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  NewName, URLListValue: string;
begin
  BaseUrl := Reader.BaseUrl;

  if Element.AttributeString('name', NewName) then
    X3DName := NewName
  else
    raise EX3DXmlError.Create('Missing "name" for <ExternProtoDeclare> element');

  ParseInterfaceDeclarationsXML(true, Element, Reader);

  if Element.AttributeString('url', URLListValue) then
    URLList.ParseXMLAttribute(URLListValue, Reader) else
    raise EX3DXmlError.Create('Missing "url" for <ExternProtoDeclare> element');

  Reader.Prototypes.Bind(Self);

  LoadReferenced;
end;

procedure TX3DExternalPrototype.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXTERNPROTO ' + X3DName + ' ');

        SaveInterfaceDeclarationsToStream(Writer, true);

        { Writer.NodeNames will be ignored by URLList
          (TMFString.SaveToStream), don't worry about it. }
        URLList.SaveToStream(Writer);
      end;
    xeXML:
      begin
        Writer.WriteIndent('<ExternProtoDeclare name=' + StringToX3DXml(X3DName) + ' url=');
        URLList.FieldSaveToStream(Writer, true, true);
        Writer.Writeln('>');

        Writer.IncIndent;
        SaveInterfaceDeclarationsToStream(Writer, true);
        Writer.DecIndent;

        Writer.WritelnIndent('</ExternProtoDeclare>');
      end;
    else raise EInternalError.Create('TX3DExternalPrototype.SaveToStream Encoding?');
  end;
end;

procedure TX3DExternalPrototype.LoadReferenced;

  procedure LoadInterfaceDeclarationsValues;
  var
    IIndex: Integer;
    I: TX3DInterfaceDeclaration;
    ReferencedField: TX3DField;
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
          TryFindFieldName(I.Field.X3DName);
        if ReferencedField <> nil then
        begin
          try
            I.Field.AssignValue(ReferencedField);
          except
            on E: EX3DFieldAssign do
            begin
              WritelnWarning('VRML/X3D', Format(
                'Error when linking external prototype "%s" with prototype "%s": ',
                [X3DName, ReferencedPrototype.X3DName]) + E.Message);
            end;
          end;
        end else
          WritelnWarning('VRML/X3D', Format('Prototype "%s" referenced by external ' +
            'prototype "%s" doesn''t have field "%s"',
            [ReferencedPrototype.X3DName, X3DName, I.Field.X3DName]));
      end;
    end;
  end;

  function LoadFromExternalVRML(const RelativeURL: string): boolean;
  var
    URL: string;
    PrototypeNames: TX3DPrototypeNames;

    procedure ProtoWarning(const S: string);
    begin
      WritelnWarning('VRML/X3D', Format('Cannot load external prototype from URL "%s": ',
        [URL]) + S);
    end;

    { Find PROTO (but not EXTERNPROTO) with matching Name.
      Name is ignored if ''.
      @nil if not found. }
    function TryFindProtoNonExternal(const Name: string): TX3DPrototype;
    var
      I: Integer;
    begin
      if PrototypeNames <> nil then
        for I := 0 to PrototypeNames.Count - 1 do
          if PrototypeNames.Objects[I] is TX3DPrototype then
          begin
            Result := TX3DPrototype(PrototypeNames.Objects[I]);
            if (Name = '') or (Result.X3DName = Name) then
              Exit;
          end;
      Result := nil;
    end;

  var
    Anchor: string;
  begin
    Result := false;

    URL := CombineURI(BaseUrl, RelativeURL);
    URIExtractAnchor(URL, Anchor);
    try
      ReferencedPrototypeNode := X3DCache.Load3D(URL);
      PrototypeNames := ReferencedPrototypeNode.PrototypeNames;
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
      X3DCache.Free3D(ReferencedPrototypeNode);
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
      WritelnWarning('VRML/X3D', Format('Unknown node URN "%s"', [URN]));
  end;

var
  I: Integer;
  S: string;
  ProtoLoaded: boolean;
begin
  UnloadReferenced;

  for I := 0 to URLList.Count - 1 do
  begin
    S := URLList.Items[I];
    if IsPrefix('urn:', S) then
      ProtoLoaded := LoadFromURN(S)
    else
      ProtoLoaded := LoadFromExternalVRML(S);
    if ProtoLoaded then
      Break;
  end;
end;

procedure TX3DExternalPrototype.UnloadReferenced;
begin
  { FReferencedPrototype will be freed as part of ReferencedPrototypeNode }
  FReferencedPrototype := nil;

  X3DCache.Free3D(ReferencedPrototypeNode);

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

procedure TNodesManager.RegisterNodeClass(NodeClass: TX3DNodeClass);
begin
  if NodeClass.ClassX3DType = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassX3DType so it cannot be registered in TNodesManager');

  if FRegistered.IndexOfObject(TObject(Pointer(NodeClass))) <> -1 then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+
      ' was already registered in TNodesManager');

  FRegistered.AddObject(NodeClass.ClassX3DType, TObject(Pointer(NodeClass)));
end;

procedure TNodesManager.RegisterNodeClasses(
  const NodeClasses: array of TX3DNodeClass);
var
  I: Integer;
begin
  for i := 0 to High(NodeClasses) do RegisterNodeClass(NodeClasses[i]);
end;

procedure TNodesManager.UnRegisterNodeClass(NodeClass: TX3DNodeClass;
  ErrorIfNotRegistered: boolean);
var i: Integer;
begin
  if NodeClass.ClassX3DType = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassX3DType so it cannot be unregistered (or even registered) '+
      'in TNodesManager');

  i := FRegistered.IndexOfObject(TObject(Pointer(NodeClass)));
  if i <> - 1 then
    FRegistered.Delete(i) else
  if ErrorIfNotRegistered then
    ENodesManagerError.Create('Node class "' + NodeClass.ClassName +
      '" was not registered, so you cannot unregister it');
end;

function TNodesManager.X3DTypeToClass(const AX3DType: string;
  const Version: TX3DVersion): TX3DNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TX3DNodeClass(FRegistered.Objects[I]);
    if (FRegistered[I] = AX3DType) and
       Result.ForVRMLVersion(Version) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.URNToClass(const URN: string): TX3DNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TX3DNodeClass(FRegistered.Objects[I]);
    if Result.URNMatching(URN) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.GetRegistered(Index: Integer): TX3DNodeClass;
begin
  Result := TX3DNodeClass(FRegistered.Objects[Index]);
end;

function TNodesManager.RegisteredCount: Cardinal;
begin
  Result := FRegistered.Count;
end;

{ TX3DRoute ----------------------------------------------------------------- }

constructor TX3DRoute.Create;
begin
  inherited;
  ResetLastEventTime;
end;

destructor TX3DRoute.Destroy;
begin
  { We have to unset, to call
    RemoveDestructionNotification(...) on our nodes before
    we get destroyed. Otherwise nodes would have invalid references
    on TX3DNode.FDestructionNotifications list. }

  UnsetEnding(FSourceNode     , FSourceEvent     , false);
  UnsetEnding(FDestinationNode, FDestinationEvent, true);
  inherited;
end;

procedure TX3DRoute.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
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

  SetSource     (SourceNodeName     , SourceEventName     , Reader);
  SetDestination(DestinationNodeName, DestinationEventName, Reader);
end;

procedure TX3DRoute.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

  function RequiredAttrib(const AttrName: string): string;
  begin
    if not Element.AttributeString(AttrName, Result) then
    begin
      WritelnWarning('VRML/X3D', 'Missing ROUTE ' + AttrName + ' attribute');
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

  SetSource     (SourceNodeName     , SourceEventName     , Reader);
  SetDestination(DestinationNodeName, DestinationEventName, Reader);
end;

procedure TX3DRoute.UnsetEnding(
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean;
  RemoveFromDestructionNotification: boolean);
begin
  if (Event <> nil) and (not DestEnding) then
    Event.OnReceive.Remove(@EventReceive);

  if Node <> nil then
  begin
    if RemoveFromDestructionNotification then
      Node.RemoveDestructionNotification(@DestructionNotification);
    Node := nil;
  end;

  Event := nil;
end;

procedure TX3DRoute.EventReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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
    WritelnLog('VRML/X3D', Format(
      'Route %s.%s -> %s.%s ignored another event at <= timestamp (%f.%d, while last event was on %f.%d). Potential routes loop avoided',
      [ SourceNode.X3DName, SourceEvent.X3DName,
        DestinationNode.X3DName, DestinationEvent.X3DName,
        Time.Seconds, Time.PlusTicks,
        LastEventTime.Seconds, LastEventTime.PlusTicks ]));
end;

procedure TX3DRoute.ResetLastEventTime;
begin
  LastEventTime := OldestX3DTime;
end;

type
  ERouteSetEndingError = class(EX3DError);

const
  DestEndingNames: array [boolean] of string =
  ('source', 'destination');

procedure TX3DRoute.SetEndingInternal(
  const Node: TX3DNode;
  const FieldOrEvent: TX3DFieldOrEvent;
  var Event: TX3DEvent;
  const DestEnding: boolean);
var
  ExposedField: TX3DField;
begin
  if FieldOrEvent is TX3DField then
  begin
    ExposedField := TX3DField(FieldOrEvent);
    if not ExposedField.Exposed then
      raise ERouteSetEndingError.CreateFmt('Route %s specifies field "%s" (for node "%s"), but this is not an exposed field (cannot generate/receive events)',
        [ DestEndingNames[DestEnding], FieldOrEvent.X3DName, Node.X3DName ]);
    Event := ExposedField.ExposedEvents[DestEnding];
  end else
  begin
    Assert(FieldOrEvent is TX3DEvent);
    Event := TX3DEvent(FieldOrEvent);
  end;

  if (not Internal) and (Event.InEvent <> DestEnding) then
  begin
    if DestEnding then
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: destination of the route (%s, type %s) can only be output event',
        [ Event.X3DName, Event.FieldClass.X3DType ]) else
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: source of the route (%s, type %s) can only be input event',
        [ Event.X3DName, Event.FieldClass.X3DType ]);
  end;

  if (SourceEvent <> nil) and
     (DestinationEvent <> nil) and
     (SourceEvent.FieldClass <> DestinationEvent.FieldClass) and
     { destination field can be XFAny (for some InstantReality nodes) as an exception. }
     (not (DestinationEvent.FieldClass = TX3DField)) then
    raise ERouteSetEndingError.CreateFmt('Route has different event types for source (%s, type %s) and destination (%s, type %s)',
      [ SourceEvent     .X3DName, SourceEvent     .FieldClass.X3DType,
        DestinationEvent.X3DName, DestinationEvent.FieldClass.X3DType ]);

  if (Event <> nil) and (not DestEnding) then
    Event.OnReceive.Add(@EventReceive);
end;

procedure TX3DRoute.SetEnding(const NodeName, FieldOrEventName: string;
  Reader: TX3DReaderNames;
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean);
var
  N: TX3DNode;
  FieldOrEvent: TX3DFieldOrEvent;
  IgnoreNodeFinished: boolean;
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    N := Reader.Nodes.Bound(NodeName, IgnoreNodeFinished);
    if N = nil then
      N := Reader.Imported.Bound(NodeName, IgnoreNodeFinished);
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

    Node.AddDestructionNotification(@DestructionNotification);

    FieldOrEvent := Node.FieldOrEvent(FieldOrEventName);
    if FieldOrEvent = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s field/event name "%s" (for node "%s", type "%s") not found',
        [ DestEndingNames[DestEnding], FieldOrEventName, NodeName, Node.X3DType ]);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      WritelnWarning('VRML/X3D', E.Message);
    end;
  end;
end;

procedure TX3DRoute.SetSource(
  const SourceNodeName, SourceFieldOrEventName: string;
  Reader: TX3DReaderNames);
begin
  SetEnding(SourceNodeName, SourceFieldOrEventName,
    Reader, FSourceNode, FSourceEvent, false);
end;

procedure TX3DRoute.SetDestination(
  const DestinationNodeName, DestinationFieldOrEventName: string;
  Reader: TX3DReaderNames);
begin
  SetEnding(DestinationNodeName, DestinationFieldOrEventName,
    Reader, FDestinationNode, FDestinationEvent, true);
end;

procedure TX3DRoute.SetEndingDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean);
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    Node := NewNode;
    Node.AddDestructionNotification(@DestructionNotification);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      WritelnWarning('VRML/X3D', E.Message);
    end;
  end;
end;

procedure TX3DRoute.SetSourceDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FSourceNode, FSourceEvent,
    false);
end;

procedure TX3DRoute.SetDestinationDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FDestinationNode, FDestinationEvent,
    true);
end;

procedure TX3DRoute.SetSourceDirectly(const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetSourceDirectly(FieldOrEvent.ParentNode as TX3DNode, FieldOrEvent);
end;

procedure TX3DRoute.SetDestinationDirectly(
  const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetDestinationDirectly(FieldOrEvent.ParentNode as TX3DNode, FieldOrEvent);
end;

type
  EX3DRouteSaveError = class(EX3DError);

procedure TX3DRoute.SaveToStream(Writer: TX3DWriter);

  procedure Ending(Node: TX3DNode; Event: TX3DEvent; const S: string;
    out NodeName, EventName: string);
  var
    BoundNode: TX3DNode;
    IgnoreNodeFinished: boolean;
  begin
    { Check Node }
    if Node = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node not assigned (look for warnings when reading this VRML file)', [S]);
    if Node.X3DName = '' then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node not named', [S]);

    BoundNode := (Writer as TX3DWriterNames).NodeNames.Bound(Node.X3DName, IgnoreNodeFinished);
    if BoundNode = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound',
        [S, Node.X3DName]);

    { Just like when setting node by TX3DRoute.SetEnding:
      we actually keep the Node that contains the route, which is
      sometimes TX3DPrototypeNode hidden inside PrototypeInstanceSourceNode. }
    if BoundNode.PrototypeInstanceSourceNode <> nil then
      BoundNode := BoundNode.PrototypeInstanceSourceNode;
    if BoundNode <> Node then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound (another node bound to the same name)',
        [S, Node.X3DName]);

    NodeName := Node.X3DName;

    { Check Event }
    if Event = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s event not assigned', [S]);

    { Check we have a name. }
    if Event.X3DName = '' then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s event not named', [S]);
    EventName := Event.X3DName;
  end;

var
  SourceNodeName, SourceEventName, DestinationNodeName, DestinationEventName: string;
begin
  if Internal then Exit;

  try
    Ending(SourceNode     , SourceEvent     , 'source'     , SourceNodeName     , SourceEventName     );
    Ending(DestinationNode, DestinationEvent, 'destination', DestinationNodeName, DestinationEventName);

    case Writer.Encoding of
      xeClassic:
        Writer.WritelnIndent(Format('ROUTE %s.%s TO %s.%s',
          [SourceNodeName     , SourceEventName,
           DestinationNodeName, DestinationEventName]));
      xeXML:
        Writer.WritelnIndent(Format('<ROUTE fromNode=%s fromField=%s toNode=%s toField=%s />',
          [StringToX3DXml(SourceNodeName)     , StringToX3DXml(SourceEventName),
           StringToX3DXml(DestinationNodeName), StringToX3DXml(DestinationEventName)]));
      else raise EInternalError.Create('TX3DRoute.SaveToStream Encoding?');
    end;
  except
    on E: EX3DRouteSaveError do
      WritelnWarning('VRML/X3D', E.Message);
  end;
end;

procedure TX3DRoute.DestructionNotification(Node: TX3DNode);
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

function TX3DRoute.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DRoute;
var
  NewSourceNode, NewDestinationNode: TX3DNode;
  NewSourceEvent, NewDestinationEvent: TX3DEvent;
begin
  Result := TX3DRoute.Create;
  Result.Internal := Internal;

  if (SourceNode <> nil) and
     (SourceEvent <> nil) then
  begin
    NewSourceNode := CopyState.DeepCopy(SourceNode);
    NewSourceEvent := NewSourceNode.AnyEvent(SourceEvent.X3DName);
    if NewSourceEvent = nil then
      raise EInternalError.CreateFmt('Route source node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewSourceNode.X3DName,
          NewSourceNode.X3DType,
	  NewSourceEvent.X3DName ]);
    Result.SetSourceDirectly(NewSourceNode, NewSourceEvent);
  end;

  if (DestinationNode <> nil) and
     (DestinationEvent <> nil) then
  begin
    NewDestinationNode := CopyState.DeepCopy(DestinationNode);
    NewDestinationEvent := NewDestinationNode.AnyEvent(DestinationEvent.X3DName);
    if NewDestinationEvent = nil then
      raise EInternalError.CreateFmt('Route destination node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewDestinationNode.X3DName,
          NewDestinationNode.X3DType,
	  NewDestinationEvent.X3DName ]);
    Result.SetDestinationDirectly(NewDestinationNode, NewDestinationEvent);
  end;
end;

{ TX3DImport ---------------------------------------------------------------- }

procedure TX3DImport.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
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

  Reader.DoImport(Self);
end;

procedure TX3DImport.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
begin
  if not Element.AttributeString('inlineDEF', InlineNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing IMPORT "inlineDEF" attribute');
    Exit;
  end;

  if not Element.AttributeString('importedDEF', ImportedNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing IMPORT "importedDEF" attribute, looking for older "exportedDEF"');
    if not Element.AttributeString('exportedDEF', ImportedNodeName) then
    begin
      WritelnWarning('VRML/X3D', 'Missing IMPORT attribute: neighter "importedDEF" nor older "exportedDEF" found');
      Exit;
    end;
  end;

  if not Element.AttributeString('AS', ImportedNodeAlias) then
    ImportedNodeAlias := ImportedNodeName;

  Reader.DoImport(Self);
end;

procedure TX3DImport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('IMPORT ' + InlineNodeName + '.' + ImportedNodeName);
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS ' + ImportedNodeAlias);
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent(Format('<IMPORT inlineDEF=%s importedDEF=%s',
          [ StringToX3DXml(InlineNodeName),
            StringToX3DXml(ImportedNodeName) ]));
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(ImportedNodeAlias));
        Writer.Writeln(' />');
      end;
    else raise EInternalError.Create('TX3DImport.SaveToStream Encoding?');
  end;
end;

function TX3DImport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
begin
  Result := TX3DImport.Create;
  Result.InlineNodeName := InlineNodeName;
  Result.ImportedNodeName := ImportedNodeName;
  Result.ImportedNodeAlias := ImportedNodeAlias;
end;

{ TX3DExport ---------------------------------------------------------------- }

procedure TX3DExport.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
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

  Reader.DoExport(Self);
end;

procedure TX3DExport.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
begin
  if not Element.AttributeString('localDEF', ExportedNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing EXPORT "localDEF" attribute');
    Exit;
  end;

  if not Element.AttributeString('AS', ExportedNodeAlias) then
    ExportedNodeAlias := ExportedNodeName;

  Reader.DoExport(Self);
end;

procedure TX3DExport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXPORT ' + ExportedNodeName);
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS ' + ExportedNodeAlias);
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent('<EXPORT localDEF=' + StringToX3DXml(ExportedNodeName));
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(ExportedNodeAlias));
        Writer.Writeln(' />');
      end;
    else raise EInternalError.Create('TX3DExport.SaveToStream Encoding?');
  end;
end;

function TX3DExport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
begin
  Result := TX3DExport.Create;
  Result.ExportedNodeName := ExportedNodeName;
  Result.ExportedNodeAlias := ExportedNodeAlias;
end;

{ TX3DNodeNameRec ------------------------------------------------------------ }

function TX3DNodeNameRec.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DNodeNameRec;
begin
  Result.Node := CopyState.DeepCopy(Node);
  Result.Name := Name;
  Result.Finished := Finished;
end;

{ TX3DNodeNames ----------------------------------------------------------- }

constructor TX3DNodeNames.Create(const AAutoRemove: boolean);
begin
  inherited Create;
  FAutoRemove := AAutoRemove;
  if AutoRemove then
    AnyNodeDestructionNotifications.Add(@DestructionNotification);
end;

destructor TX3DNodeNames.Destroy;
begin
  { This may happen after X3DNodes unit finalization
    (e.g. simplest_vrml_browser_with_shadow_volumes demo_models/shadow_volumes/stonehenge.wrl,
    where TX3DRootNode with some ExportedNames is freed from CastleWindow
    unit finalization, because Application owns Window that owns Scene).
    So secure from AnyNodeDestructionNotifications being nil. }
  if AutoRemove and (AnyNodeDestructionNotifications <> nil) then
    AnyNodeDestructionNotifications.Remove(@DestructionNotification);
  inherited;
end;

function TX3DNodeNames.IndexOfNode(Node: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TX3DNodeNames.IndexOfName(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Name = Name then
      Exit;
  Result := -1;
end;

procedure TX3DNodeNames.DestructionNotification(Node: TX3DNode);
var
  I: Integer;
begin
  I := IndexOfNode(Node);
  if I >= 0 then
    Delete(I);
end;

procedure TX3DNodeNames.Bind(Node: TX3DNode; const NodeFinished: boolean; const BindToName: string);
var
  I: Integer;
  P: PX3DNodeNameRec;
begin
  if BindToName <> '' then
  begin
    I := IndexOfName(BindToName);
    if I <> -1 then
      P := Ptr(I) else
      P := Add;
    P^.Node := Node;
    P^.Name := BindToName;
    P^.Finished := NodeFinished;
  end;
end;

procedure TX3DNodeNames.Bind(Node: TX3DNode; const NodeFinished: boolean);
begin
  Bind(Node, NodeFinished, Node.X3DName);
end;

function TX3DNodeNames.Bound(const Name: string; out NodeFinished: boolean): TX3DNode;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I <> -1 then
  begin
    Result := L[I].Node;
    NodeFinished := L[I].Finished;
  end else
    Result := nil;
end;

function TX3DNodeNames.Bound(Node: TX3DNode): boolean;
begin
  Result := IndexOfNode(Node) <> -1;
end;

function TX3DNodeNames.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DNodeNames;
var
  I: Integer;
begin
  Result := TX3DNodeNames.Create(AutoRemove);
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Items[I].DeepCopy(CopyState);
end;

{ TX3DPrototypeNames -------------------------------------------------------- }

procedure TX3DPrototypeNames.Bind(Proto: TX3DPrototypeBase);
var
  I: Integer;
begin
  I := IndexOf(Proto.X3DName);
  if I <> - 1 then
    Objects[I] := Proto else
    AddObject(Proto.X3DName, Proto);
end;

function TX3DPrototypeNames.Bound(const Name: string): TX3DPrototypeBase;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
    Result := Objects[I] as TX3DPrototypeBase else
    Result := nil;
end;

{ TX3DImportableNames ------------------------------------------------------- }

destructor TX3DImportableNames.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited;
end;

procedure TX3DImportableNames.Bind(const InlineName: string; Exported: TX3DNodeNames);
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

{ global procedures ---------------------------------------------------------- }

procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    StateNodes.Nodes[SN] := TraverseStateLastNodesClasses[SN].Create;
end;

procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    FreeAndNil(StateNodes.Nodes[SN]);
end;

function KeyRange(Key: TSingleList;
  const Fraction: Single; out T: Single): Integer;
var
  A, B: Integer;
begin
  Assert(Key.Count > 0);
  Assert(not IsNan(Fraction));

  if Fraction <= Key.First then
    Result := 0 else
  if Fraction >= Key.Last then
    Result := Key.Count else
  begin
    { Then for sure we're between two Key values.
      Note that we know that Key.Count > 1 (otherwise, Key.First = Key.Last
      so one of <= or >= comparisons above would occur; we check
      IsNan(Fraction) at the beginning to eliminate Fraction=NaN case). }
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
    B := Key.Count - 1;
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

procedure FreeIfUnusedAndNil(var Obj);
var
  Temp: TX3DNode;
begin
  Temp := TX3DNode(Obj);
  Pointer(Obj) := nil;
  Temp.FreeIfUnused;
end;

function URNMatchingCastle(const URN, ClassX3DType: string): boolean;
begin
  Result :=
    (URN = 'urn:castle-engine.sourceforge.io:node:' + ClassX3DType) or
    (URN = 'urn:castle-engine.sourceforge.net:node:' + ClassX3DType) or
    (URN = 'urn:vrmlengine.sourceforge.net:node:' + ClassX3DType);
end;

{ TNodeDestructionNotificationList ------------------------------------------- }

procedure TNodeDestructionNotificationList.ExecuteAll(Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I](Node);
end;

{ unit init/fini ------------------------------------------------------------ }

procedure X3DNodesFinalization;
begin
  TraverseState_FreeAndNilNodes(StateDefaultNodes);
  FreeAndNil(TraverseSingleStack);
  FreeAndNil(X3DCache);

  FreeAndNil(NodesManager);
  FreeAndNil(AnyNodeDestructionNotifications);

  FreeAndNil(CurrentlyLoading);
end;

initialization
  AnyNodeDestructionNotifications := TNodeDestructionNotificationList.Create;

  X3DFieldsManager.RegisterClasses([TSFNode, TMFNode]);

  NodesManager := TNodesManager.Create;

  RegistedInventorNodes;
  RegisterVRML1Nodes;
  RegisterVRML97HAnimNodes;
  RegisterVRML97NodesNurbs;
  RegisterKambiNodes;
  RegisterInstantRealityNodes;
  RegisterBitManagementNodes;

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
  RegisterInterpolationCubicBezierNodes;
  RegisterPointingDeviceSensorNodes;
  RegisterKeyDeviceSensorNodes;
  RegisterEnvironmentalSensorNodes;
  RegisterNavigationNodes;
  RegisterEnvironmentalEffectsNodes;
  RegisterGeospatialNodes;
  RegisterHAnimNodes;
  RegisterNURBSNodes;
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

  X3DCache := TX3DNodesCache.Create;
  TraverseState_CreateNodes(StateDefaultNodes);
  TraverseSingleStack := TX3DGraphTraverseStateStack.Create;

  CurrentlyLoading := TCastleStringList.Create;
finalization
  { Because of various finalization order (some stuff may be owned
    e.g. by CastleWindow.Application, and freed at CastleWindow finalization,
    which may be done after X3DNodes finalization) we may defer
    finalization for later. }
  if (X3DCache = nil) or X3DCache.Empty then
    X3DNodesFinalization else
    X3DCache.OnEmpty := @X3DNodesFinalization;
end.
