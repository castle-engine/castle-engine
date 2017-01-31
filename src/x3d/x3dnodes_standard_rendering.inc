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

{$ifdef read_interface}
  { }
  TCoordRangeHandler = procedure (const RangeNumber: Cardinal;
    BeginIndex, EndIndex: Integer) of object;

  TIndexedPolygonHandler = procedure (const Indexes: array of Cardinal)
    of object;

  ENotCoordinateBasedNode = class(EX3DError);

  { Geometry nodes are the only nodes that produce something visible.
    Much of the X3D language is about describing how geometry nodes are displayed
    (materials, transformations, lighting). }
  TAbstractGeometryNode = class(TAbstractNode)
  private
    HasBounds2DST: boolean;
    Bounds2DS, Bounds2DT: Integer;
    FIgnoredSolid: boolean; {< keeps value of Solid property when FdSolid is nil }
    FIgnoredConvex: boolean; {< keeps value of Convex property when FdConvex is nil }
    { Workaround http://bugs.freepascal.org/view.php?id=17402
      by these methods. }
    function CallProxyLocalBoundingBox(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
    function CallProxyBoundingBox(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
    function CallProxyTrianglesCount(
      OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
    function CallProxyVerticesCount(
      OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
    function GetSolid: boolean;
    procedure SetSolid(const Value: boolean);
    function GetConvex: boolean;
    procedure SetConvex(const Value: boolean);
  public
    { Constructor for node.
      Only sets DefaultContainerField to 'geometry', since this is valid
      for all X3D nodes descending from TAbstractGeometryNode. }
    procedure CreateNode; override;

    { Calculate bounding box of this geometry node.
      They require State of this node during VRML traverse state --- this
      is mainly for VRML 1.0 nodes, that depend on such state.

      LocalBoundingBox gives a bounding box ignoring current transformation
      (or, equivalently, assuming like Transform = IdentityMatrix).
      Normal BoundingBox gives a bounding box taking current transformation
      into account.

      @italic(Notes for descendants implementors:)

      The default implementations of these methods in TAbstractGeometryNode
      try to be smart and cover all common bases, so that you have to do
      as little work as possible to implement working descendant.

      @orderedList(
        @item(
          For nodes based on coordinates (when @link(Coord) returns @true),
          LocalBoundingBox and BoundingBox
          already have optimal and correct implementation in this class.
          Using Coord and CoordIndex, no other information is needed.)

        @item(
          For other nodes, we first check ProxyGeometry and ProxyState.
          If ProxyGeometry is non-nil, we assume these came from @link(Proxy)
          call and we will use them to calculate
          bounding boxes, local and not local.

          So for nodes with @link(Proxy) overridden, you don't have
          to implement bounding box calculation, instead a ProxyGeometry
          will be created and provided here by the caller.
          This will work Ok if @link(Proxy) node will
          have bounding box calculation implemented.

          You can always override these methods, if you don't want
          to use proxy (for example, maybe there exists much faster
          method to calculate bounding box, or maybe tighter
          bounding box may be calculated directly).)

        @item(
          For other nodes (not coordinate-based and without a proxy):

          The default implementation of LocalBoundingBox just calls
          BoundingBox with a specially modified State, such that
          Transform is identity.

          The default implementation of BoundingBox, in turn, just calls
          LocalBoundingBox and transforms this bounding box.

          So the default implementations call each other, and will loop
          infinitely... But if you override any one of them
          (local or not local), the other one will magically work.

          Note that the default implementation of LocalBoundingBox
          may be non-optimal as far as time is concerned,
          as we'll do useless multiplications by identity matrix.
          And the default implementation of BoundingBox may generate
          non-optimal bounding box, more direct approach (transforming
          each vertex) may give much tightier bounding box.

          So you only have to override one method --- although if you
          want the best implementation, fastest and with the best tight
          bounding boxes, you may need to override both of them for some nodes.)
      )

      @groupBegin }
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode;
      ProxyState: TX3DGraphTraverseState): TBox3D; virtual;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode;
      ProxyState: TX3DGraphTraverseState): TBox3D; virtual;
    { @groupEnd }

    { Calculate vertex and triangles count of this node.

      They require State of this node during VRML traverse state --- this
      is mainly for VRML 1.0 nodes, that depend on such state.
      OverTriangulate has the same meaning as for TShape.Triangulate.

      Vertices count calculates number of different vertexes in this node.
      That is, it doesn't eliminate doubles in cases like Coordinate node
      with multiple points the same. But if some face is known to use
      twice the same vertex index, then this counts like a single vertex.
      The idea is that this indicates rendering speed.

      For triangles count, the returned value may be different then
      actual if some faces were non-convex. Things like TriangulateFace
      may remove degenerate triangles, so actual number of triangles may
      be slightly less. So don't depend on TrianglesCount as a precise
      measure --- but it's a good fast measure of complexity of given
      node, how fast it will be rendered, used with collision detection etc.

      @italic(Notes for descendants implementors:)

      For coordinate-based nodes (when @link(Coord) returns @true),
      VerticesCount is already implemented in this class.
      Using Coord method, no other information is needed.

      For other nodes, the default implementation of
      both VerticesCount and TrianglesCount in this TAbstractGeometryNode
      class will use ProxyGeometry and ProxyState to do the work.
      You should override these methods if @link(Proxy) is not available
      (so caller will always pass ProxyGeometry = @nil)
      or some faster approach is possible.

      @groupBegin }
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode;
      ProxyState: TX3DGraphTraverseState): Cardinal; virtual;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode;
      ProxyState: TX3DGraphTraverseState): Cardinal; virtual;
    { @groupEnd }

    { Return node's list of coordinates. Returns @false if node is
      not based on coordinates. Returns @true and sets ACoord
      if the node is based on coordinates. Even when returns @true,
      it can set ACoord = @nil, which means that node is based on
      coordinates but they are empty right now (so for example
      bounding box may be considered empty).

      In base TAbstractGeometryNode class this always returns @false.

      Override this for descendants that have some kind of "coord" field,
      then this should return @true and set ACoord to coord.point field,
      assuming that coord is set and specifies Coordinate node.
      Override this even if coordinates affect the look indirectly,
      e.g. NURBS "controlPoint" fields also should be returned here.
      Otherwise should return @true and set ACoord = @nil.

      For VRML 1.0, coord may be taken from State, that's why we have to
      pass current traverse state here. }
    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; virtual;

    { Return node's list of coordinates, raising exception if node
      is not based on coordinates.

      This is just like the @link(Coord) method,
      except it simply returns the coordinates, not the boolean result.
      If virtual @link(Coord) returns @false (indicating the node
      is not coordinate-based) this raises ENotCoordinateBasedNode.

      @raises(ENotCoordinateBasedNode If node is not coordinate-based,
        that is @link(Coord) returns false.)
    }
    function Coordinates(State: TX3DGraphTraverseState): TMFVec3f;

    { Node's list of coordinate indexes.

      In base TAbstractGeometryNode class this always returns @nil.

      Override this for descendants that have some kind of "coordIndex"
      or "index" field used to index @link(Coord) array. }
    function CoordIndex: TMFLong; virtual;

    { Returns an information how to split @link(Coord) array into ranges.

      When CoordIndex = @nil, then if the node's @link(Coord) array
      can be divided into some "ranges", we will use this information.
      This is used (and should be overridden) for X3D non-indexed nodes,
      like fanCount or stripCount or vertexCount.

      What precisely is a "range of coordinates" is not specified
      here. It may be a line stip, or one triangle strip, etc. ---
      depending on the descendant.

      Returns @true if this is available. In this case, RangeCount must
      be set to something <> nil, and the rest of returned variables
      are mainly to generate proper warnings by MakeCoordRanges. }
    function CoordRangesCounts(out RangeCount: TLongIntList;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; virtual;

    { Splits @link(Coord) array into ranges.

      If CoordIndex is assigned, then a "range of coordinates" is
      just a range of non-negative indexes within CoordIndex.
      Otherwise (when CoordIndex = @nil), CoordRangesCounts must
      return @true and we will use RangeCount to split coordinates.

      Call this only for nodes with coordinates, that is only when
      @link(Coord) returns @true. }
    procedure MakeCoordRanges(
      State: TX3DGraphTraverseState;
      CoordRangeHandler: TCoordRangeHandler);

    { Splits coordinate-based node into polygons.

      Indexes in PolygonHandler point to CoordIndex, if assigned,
      or directly to Coord. The ordering of generated polygons is correct,
      so what pointed CCW in the node field, will still point CCW
      according to generated PolygonHandler indexes.

      In this class this does nothing. Some, but not all, coordinate-based
      nodes (the ones when @link(Coord) returns @true) override this.
      So currently, whether this is implemented is coordinated with
      CastleInternalNormals and such internal needs. }
    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); virtual;

    { Node's texture coordinates. Returns @false if node
      cannot have texture coordinates.

      Returns @true and sets ATexCoord to a node defining texture coords.
      ATexCoord may be set to TAbstractTextureCoordinateNode descendant or
      to TTextureCoordinate2Node_1 (latter one only for VRML <= 1.0).
      ATexCoord can also be set to @nil in this case, which means that
      this node has a field for texCoord, but it's empty right now.

      In base TAbstractGeometryNode class this looks at TexCoordField,
      eventually returns @false. }
    function TexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; virtual;

    { Node's texCoord field, or @nil if not available.
      Various nodes may have different exact rules about what is
      allowed here, but everything allows TextureCoordinateGenerator
      and ProjectedTextureCoordinate instances.

      This gives you more possibilities than the @link(TexCoord) method
      (as you can assign texCoord using this), however it may be not available
      in all cases --- for example VRML 1.0 nodes do not have texCoord field,
      but they may have a texture coordinate node (from the state). }
    function TexCoordField: TSFNode; virtual;

    { Converts this node to another node class that may be better supported.

      Typically, converts some complex geometry node (like
      Extrusion or Teapot) into more common node like IndexedFaceSet
      or IndexedTriangleSet. TShape class wraps this method into
      a more comfortable interface, that is TShape methods
      simply automatically convert geometry nodes to their proxy versions
      if needed.

      In the base TAbstractGeometryNode class, returns @nil indicating
      that no conversion is known.

      The resulting node's Name (if the result is not @nil) must be
      equal to our Name.

      Some Proxy implementations (especially for VRML 1.0) will have
      to create new State (TX3DGraphTraverseState) instance along
      with a new geometry node. You should do this by copying the State
      into a new TX3DGraphTraverseState instance, and modyfying
      the State reference. Simply speaking, do

      @longcode(#
        State := TX3DGraphTraverseState.CreateCopy(State);)
      #)

      You should @italic(not) just modify the fields
      of the provided State instance. (Reasoning: some proxy methods
      rely on getting the original State, e.g. with original MaterialBinding,
      not the transformed state, to work correctly. And some proxy methods
      need to modify the state differently, depending on OverTriangulate
      value.)

      You can modify State variable only when returning non-nil geometry. }
    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; virtual;

    { Does the @link(Proxy) method look at the OverTriangulate parameter
      at all. Default implementation in TAbstractGeometryNode returns safer @true.
      Descendants may override and return @false, to allow
      e.g. more caching in TShape. }
    function ProxyUsesOverTriangulate: boolean; virtual;

    { Returns color.point field, assuming that "color" field is set and
      specifies Color (or ColorRGBA) node. Otherwise returns @nil.

      Note that only one of Color and ColorRGBA may return non-nil,
      since "color" field may contain only one of them.

      @groupBegin }
    function Color: TMFVec3f;
    function ColorRGBA: TMFColorRGBA;
    { @groupEnd }

    function FogCoord: TMFFloat; virtual;
    function Attrib: TMFNode; virtual;

    function ColorField: TSFNode; virtual;

    { Is backface culling used.
      @nil if given geometry node doesn't have a field to control it. }
    function SolidField: TSFBool; virtual;

    { Are faces guaranteed to be convex.
      @nil if given geometry node doesn't have a field to control it. }
    function ConvexField: TSFBool; virtual;

    { Should renderer automatically generate 3D texture coordinates,
      in case we will apply 3D texture on this geometry.

      The generated coordinates will follow the X3D specification
      at "Texturing3D" component: "Texture coordinate generation
      for primitive objects". The 3D texture space will be mapped nicely
      to the shape bounding box.

      Implementation in this class (TAbstractGeometryNode) returns always
      @false. Override it for primitives that have no texture coordinates
      to return @true. }
    function AutoGenerate3DTexCoords: boolean; virtual;

    function TransformationChange: TNodeTransformationChange; override;

    { Calculate texture S, T coordinates for BOUNDS2D texture mapping.
      This mapping is like the default IndexedFaceSet texture mapping,
      following X3D spec. We also use it for other geometry nodes,
      and even allow explicitly requesting it by TextureCoordinateGenerator.mode = "BOUNDS2D". }
    procedure GetTextureBounds2DST(const LocalBoxSizes: TVector3Single;
      out S, T: Integer);

    { Is this object lit, disregarding the material.
      Default implementation in TAbstractGeometryNode says @true. }
    function Lit(State: TX3DGraphTraverseState): boolean; virtual;

    { Should renderer setup an extra texture slot with font texture
      when rendering this node. This is useful for rendering Text nodes,
      that want to cooperate with normal texturing and shading,
      and additionally they want to use extra texture determined by font
      (not by Appearance node). }
    function FontTextureNode: TAbstractTexture2DNode; virtual;

    { Are faces convex. This knowledge is used when calculating normal vectors
      by CastleInternalNormals. }
    property Convex: boolean read GetConvex write SetConvex;

    { Is backface culling used. }
    property Solid: boolean read GetSolid write SetSolid;

    {$I auto_generated_node_helpers/x3dnodes_x3dgeometrynode.inc}
  end;

  { Base node type for all geometric property node types. }
  TAbstractGeometricPropertyNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dgeometricpropertynode.inc}
  end;

  { Base node for a visible geometry in X3D. }
  TAbstractX3DGeometryNode = class(TAbstractGeometryNode)
  end;

  { Base node for color specifications. }
  TAbstractColorNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dcolornode.inc}
  end;

  { Base node type for all composed 3D geometry. }
  TAbstractComposedGeometryNode = class(TAbstractX3DGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;

    private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    private FFdNormal: TSFNode;
    public property FdNormal: TSFNode read FFdNormal;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    private FFdNormalPerVertex: TSFBool;
    public property FdNormalPerVertex: TSFBool read FFdNormalPerVertex;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdRadianceTransfer: TMFVec3f;
    public property FdRadianceTransfer: TMFVec3f read FFdRadianceTransfer;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function TexCoordField: TSFNode; override;
    function ColorField: TSFNode; override;
    function SolidField: TSFBool; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    { Returns normal vectors that are specified explicitly.
      The vectors are taken from the Normal node inside our FdNormal field.
      In case of problems (no Normal node specified, invalid node specified etc.)
      returns @nil. }
    function Normal: TMFVec3f;

    { Returns normal vectors that are specified explicitly.
      The vectors are taken from the Normal node inside our FdNormal field.
      In case of problems (no Normal node specified, invalid node specified etc.)
      returns @nil. }
    function NormalItems: TVector3SingleList;

    {$I auto_generated_node_helpers/x3dnodes_x3dcomposedgeometrynode.inc}
  end;

  { Base node type for all coordinate node types. }
  TAbstractCoordinateNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;

    { Count of coordinates within. }
    function CoordCount: Cardinal; virtual; abstract;

    {$I auto_generated_node_helpers/x3dnodes_x3dcoordinatenode.inc}
  end;

  { Base node type for all normal node types. }
  TAbstractNormalNode = class(TAbstractGeometricPropertyNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dnormalnode.inc}
  end;

  { Clip the geometry with a plane. }
  TClipPlaneNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdPlane: TSFVec4f;
    public property FdPlane: TSFVec4f read FFdPlane;

    {$I auto_generated_node_helpers/x3dnodes_clipplane.inc}
  end;

  { Set of RGB colours. }
  TColorNode = class(TAbstractColorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdColor: TMFColor;
    public property FdColor: TMFColor read FFdColor;

    {$I auto_generated_node_helpers/x3dnodes_color.inc}
  end;

  { Set of RGBA colours. }
  TColorRGBANode = class(TAbstractColorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdColor: TMFColorRGBA;
    public property FdColor: TMFColorRGBA read FFdColor;

    {$I auto_generated_node_helpers/x3dnodes_colorrgba.inc}
  end;

  { Set of 3D coordinates to be used in the @code(Coord)
    field of vertex-based geometry nodes. }
  TCoordinateNode = class(TAbstractCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;

    function CoordCount: Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_coordinate.inc}
  end;

  { Geometry formed by constructing polylines from 3D vertices. }
  TIndexedLineSetNode = class(TAbstractX3DGeometryNode)
  private
    procedure EventSet_ColorIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure EventSet_CoordIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_colorIndex: TMFInt32Event;
    public property EventSet_colorIndex: TMFInt32Event read FEventSet_colorIndex;

    { Event in } { }
    private FEventSet_coordIndex: TMFInt32Event;
    public property EventSet_coordIndex: TMFInt32Event read FEventSet_coordIndex;

    private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    private FFdColorIndex: TMFInt32;
    public property FdColorIndex: TMFInt32 read FFdColorIndex;

    private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndex: TMFLong; override;

    function ColorField: TSFNode; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedlineset.inc}
  end;
  TIndexedLineSetNode_2 = TIndexedLineSetNode;

  { Triangles that form a fan shape around the first vertex. }
  TIndexedTriangleFanSetNode = class(TAbstractComposedGeometryNode)
  private
    procedure EventSet_IndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_index: TMFInt32Event;
    public property EventSet_index: TMFInt32Event read FEventSet_index;

    private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedtrianglefanset.inc}
  end;

  { A collection of triangles. }
  TIndexedTriangleSetNode = class(TAbstractComposedGeometryNode)
  private
    procedure EventSet_IndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_index: TMFInt32Event;
    public property EventSet_index: TMFInt32Event read FEventSet_index;

    private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedtriangleset.inc}
  end;

  { Strips of triangles. }
  TIndexedTriangleStripSetNode = class(TAbstractComposedGeometryNode)
  private
    procedure EventSet_IndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_index: TMFInt32Event;
    public property EventSet_index: TMFInt32Event read FEventSet_index;

    private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedtrianglestripset.inc}
  end;

  { Lines. }
  TLineSetNode = class(TAbstractX3DGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    private FFdVertexCount: TMFInt32;
    public property FdVertexCount: TMFInt32 read FFdVertexCount;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function CoordRangesCounts(out RangeCount: TLongIntList;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;

    function ColorField: TSFNode; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_lineset.inc}
  end;

  { Normal (information for lighting) node.

    Note: It's identical for all VRML and X3D versions
    (including ancient VRML 1.0). }
  TNormalNode = class(TAbstractNormalNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    class function URNMatching(const URN: string): boolean; override;

    private FFdVector: TMFVec3f;
    public property FdVector: TMFVec3f read FFdVector;

    {$I auto_generated_node_helpers/x3dnodes_normal.inc}
  end;

  { Points. }
  TPointSetNode = class(TAbstractX3DGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function ColorField: TSFNode; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    {$I auto_generated_node_helpers/x3dnodes_pointset.inc}
  end;
  TPointSetNode_2 = TPointSetNode;

  { Triangles that form a fan shape around the first vertex.
    In constrast to @link(TIndexedTriangleFanSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleFanSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFanCount: TMFInt32;
    public property FdFanCount: TMFInt32 read FFdFanCount;

    function CoordRangesCounts(out RangeCount: TLongIntList;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_trianglefanset.inc}
  end;

  { A collection of triangles.
    In constrast to @link(TIndexedTriangleSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_triangleset.inc}
  end;

  { Strips of triangles.
    In constrast to @link(TIndexedTriangleStripSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleStripSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdStripCount: TMFInt32;
    public property FdStripCount: TMFInt32 read FFdStripCount;

    function CoordRangesCounts(out RangeCount: TLongIntList;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_trianglestripset.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractGeometryNode ---------------------------------------------------------- }

procedure TAbstractGeometryNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'geometry';
  { to make notes with and without FdSolid behave consistent }
  FIgnoredSolid := true;
  FIgnoredConvex := true;
end;

function TAbstractGeometryNode.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := false;
end;

function TAbstractGeometryNode.Coordinates(State: TX3DGraphTraverseState): TMFVec3f;
begin
  if not Coord(State, Result) then
    raise ENotCoordinateBasedNode.CreateFmt('Node %s is not a coordinate-based node',
      [X3DType]);
end;

function TAbstractGeometryNode.CoordIndex: TMFLong;
begin
  Result := nil;
end;

function TAbstractGeometryNode.CoordRangesCounts(
  out RangeCount: TLongIntList;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  Result := false;
end;

procedure TAbstractGeometryNode.MakeCoordRanges(
  State: TX3DGraphTraverseState;
  CoordRangeHandler: TCoordRangeHandler);
var
  BeginIndex, EndIndex: Integer;
  RangeNumber: Cardinal;
  RangeCount: TLongIntList;
  SRanges, SRangeName: string;
  RangeMinimumCount: Cardinal;
  C: TMFVec3f;
begin
  C := Coordinates(State);

  if C = nil then
    Exit;

  if CoordIndex <> nil then
  begin
    BeginIndex := 0;
    RangeNumber := 0;
    while BeginIndex < CoordIndex.Count do
    begin
      EndIndex := BeginIndex;
      while (EndIndex < CoordIndex.Count) and
            (CoordIndex.Items.Items[EndIndex] >= 0) do
        Inc(EndIndex);
      CoordRangeHandler(RangeNumber, BeginIndex, EndIndex);
      Inc(RangeNumber);
      BeginIndex := EndIndex + 1;
    end;
  end else
  begin
    if not CoordRangesCounts(RangeCount, SRanges, SRangeName,
      RangeMinimumCount) then
      raise EInternalError.CreateFmt('%s.MakeCoordRanges: either CoordIndex or CoordRangesCounts must be defined to split coordinates', [ClassName]);
    EndIndex := 0;
    if RangeCount.Count > 0 then
      for RangeNumber := 0 to RangeCount.Count - 1 do
      begin
        BeginIndex := EndIndex;
        EndIndex := BeginIndex + RangeCount.Items[RangeNumber];
        { Note that EndIndex *may* be equal to C.Count,
          as EndIndex is not taken into account by CoordRangeHandler. }
        if EndIndex > C.Count then
        begin
          WritelnWarning('VRML/X3D', Format('Too much %s (not enough coordinates) in %s',
            [SRanges, X3DType]));
          Break;
        end;
        if Cardinal(EndIndex - BeginIndex) >= RangeMinimumCount then
          CoordRangeHandler(RangeNumber, BeginIndex, EndIndex) else
          WritelnWarning('VRML/X3D', Format('%s is less than %d in %s',
            [SRangeName, RangeMinimumCount, X3DType]));
      end;
  end;
end;

procedure TAbstractGeometryNode.CoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
begin
  { Nothing to do in this class. }
end;

function TAbstractGeometryNode.TexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := TexCoordField <> nil;
  if Result and
     (TexCoordField.Value <> nil) and
     TexCoordField.CurrentChildAllowed then
    ATexCoord := TexCoordField.Value else
    ATexCoord := nil;
end;

function TAbstractGeometryNode.TexCoordField: TSFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := true;
end;

function TAbstractGeometryNode.FogCoord: TMFFloat;
begin
  Result := nil;
end;

function TAbstractGeometryNode.Attrib: TMFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := false;
end;

function TAbstractGeometryNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcGeometry;
end;

procedure TAbstractGeometryNode.GetTextureBounds2DST(const LocalBoxSizes: TVector3Single;
  out S, T: Integer);
var
  C1, C2: Integer;
begin
  { Calculate SCoord and TCoord. Following VRML spec:
    SCoord is the coord where LocalBBoxSize is largest,
    TCoord is the second-to-largest (and if some sizes are equal,
    then X is more important than Y than Z).

    Do it once (setting HasBounds2DST), this fixes JoeH-AnimKick0a.x3dv
    renderering, see x3d-public thread about view3dscene 3.8.0 announcement. }

  if not HasBounds2DST then
  begin
    HasBounds2DST := true;
    Bounds2DS := MaxVectorCoord(LocalBoxSizes);
    RestOf3dCoords(Bounds2DS, C1, C2);
    if LocalBoxSizes[C1] >= LocalBoxSizes[C2] then
      Bounds2DT := C1 else
      Bounds2DT := C2;
  end;

  S := Bounds2DS;
  T := Bounds2DT;
end;

function TAbstractGeometryNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  Result := true;
end;

function TAbstractGeometryNode.ConvexField: TSFBool;
begin
  Result := nil;
end;

function TAbstractGeometryNode.GetConvex: boolean;
var
  F: TSFBool;
begin
  F := ConvexField;
  if F <> nil then
    Result := F.Value else
    Result := FIgnoredConvex;
end;

procedure TAbstractGeometryNode.SetConvex(const Value: boolean);
var
  F: TSFBool;
begin
  F := ConvexField;
  if F <> nil then
    F.Send(Value) else
    FIgnoredConvex := Value;
end;

function TAbstractGeometryNode.SolidField: TSFBool;
begin
  Result := nil;
end;

function TAbstractGeometryNode.GetSolid: boolean;
var
  F: TSFBool;
begin
  F := SolidField;
  if F <> nil then
    Result := F.Value else
    Result := FIgnoredSolid;
end;

procedure TAbstractGeometryNode.SetSolid(const Value: boolean);
var
  F: TSFBool;
begin
  F := SolidField;
  if F <> nil then
    F.Send(Value) else
    FIgnoredSolid := Value;
end;

function TAbstractGeometryNode.ColorField: TSFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.Color: TMFVec3f;
var
  F: TSFNode;
begin
  F := ColorField;
  if (F <> nil) and
     (F.Value <> nil) and
     (F.Value is TColorNode) then
    Result := TColorNode(F.Value).FdColor else
    Result := nil;
end;

function TAbstractGeometryNode.ColorRGBA: TMFColorRGBA;
var
  F: TSFNode;
begin
  F := ColorField;
  if (F <> nil) and
     (F.Value <> nil) and
     (F.Value is TColorRGBANode) then
    Result := TColorRGBANode(F.Value).FdColor else
    Result := nil;
end;

function TAbstractGeometryNode.FontTextureNode: TAbstractTexture2DNode;
begin
  Result := nil;
end;

{ Normal X3D spec nodes ------------------------------------------------------ }

procedure TAbstractColorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'color';
end;

procedure TAbstractComposedGeometryNode.CreateNode;
begin
  inherited;

  FFdAttrib := TMFNode.Create(Self, 'attrib', [TAbstractVertexAttributeNode]);
   FdAttrib.ChangesAlways := [chGeometry];
  AddField(FFdAttrib);

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  AddField(FFdColor);

  FFdCoord := TSFNode.Create(Self, 'coord', [TAbstractCoordinateNode]);
   FdCoord.ChangesAlways := [chGeometry];
  AddField(FFdCoord);

  FFdFogCoord := TSFNode.Create(Self, 'fogCoord', [TFogCoordinateNode]);
   FdFogCoord.ChangesAlways := [chGeometry];
  AddField(FFdFogCoord);

  FFdNormal := TSFNode.Create(Self, 'normal', [TAbstractNormalNode]);
   FdNormal.ChangesAlways := [chGeometry];
  AddField(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chGeometry];
  AddField(FFdTexCoord);

  FFdCcw := TSFBool.Create(Self, 'ccw', true);
   FdCcw.Exposed := false;
   FdCcw.ChangesAlways := [chGeometry];
  AddField(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdColorPerVertex.Exposed := false;
   FdColorPerVertex.ChangesAlways := [chGeometry];
  AddField(FFdColorPerVertex);

  FFdNormalPerVertex := TSFBool.Create(Self, 'normalPerVertex', true);
   FdNormalPerVertex.Exposed := false;
   FdNormalPerVertex.ChangesAlways := [chGeometry];
  AddField(FFdNormalPerVertex);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);

  FFdRadianceTransfer := TMFVec3f.Create(Self, 'radianceTransfer', []);
   FdRadianceTransfer.ChangesAlways := [chGeometry];
  AddField(FFdRadianceTransfer);
end;

function TAbstractComposedGeometryNode.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdCoord.Value <> nil) and
     (FdCoord.Value is TCoordinateNode) then
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint else
    ACoord := nil;
end;

function TAbstractComposedGeometryNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TAbstractComposedGeometryNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TAbstractComposedGeometryNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TAbstractComposedGeometryNode.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TAbstractComposedGeometryNode.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

function TAbstractComposedGeometryNode.Normal: TMFVec3f;
begin
  if (FdNormal.Value <> nil) and
     (FdNormal.Value is TNormalNode) then
    Result := TNormalNode(FdNormal.Value).FdVector else
    Result := nil;
end;

function TAbstractComposedGeometryNode.NormalItems: TVector3SingleList;
begin
  if (FdNormal.Value <> nil) and
     (FdNormal.Value is TNormalNode) then
    Result := TNormalNode(FdNormal.Value).FdVector.Items else
    Result := nil;
end;

function TAbstractComposedGeometryNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdnormal.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TAbstractCoordinateNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'coord';
end;

procedure TClipPlaneNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
   FdEnabled.ChangesAlways := [chClipPlane];
  AddField(FFdEnabled);

  FFdPlane := TSFVec4f.Create(Self, 'plane', Vector4Single(0, 1, 0, 0));
   FdPlane.ChangesAlways := [chClipPlane];
  AddField(FFdPlane);
  { X3D specification comment: [0,1] }

  { X3D specification (XML encoding, edition 2)
    says that containerField of this should be "color"... which doesn't
    make sense? I leave DefaultContainerField as "children"
    (inherited from X3DChilNode. }
end;

class function TClipPlaneNode.ClassX3DType: string;
begin
  Result := 'ClipPlane';
end;

class function TClipPlaneNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TColorNode.CreateNode;
begin
  inherited;

  FFdColor := TMFColor.Create(Self, 'color', []);
   FdColor.ChangesAlways := [chColorNode];
  AddField(FFdColor);
  { X3D specification comment: [0,1] }
end;

class function TColorNode.ClassX3DType: string;
begin
  Result := 'Color';
end;

class function TColorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TColorRGBANode.CreateNode;
begin
  inherited;

  FFdColor := TMFColorRGBA.Create(Self, 'color', []);
   FdColor.ChangesAlways := [chColorNode];
  AddField(FFdColor);
  { X3D specification comment: [0,1] }
end;

class function TColorRGBANode.ClassX3DType: string;
begin
  Result := 'ColorRGBA';
end;

class function TColorRGBANode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCoordinateNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chCoordinate];
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TCoordinateNode.ClassX3DType: string;
begin
  Result := 'Coordinate';
end;

class function TCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCoordinateNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TIndexedLineSetNode.CreateNode;
begin
  inherited;

  FEventSet_colorIndex := TMFInt32Event.Create(Self, 'set_colorIndex', true);
  AddEvent(FEventSet_colorIndex);
  Eventset_colorIndex.OnReceive.Add(@EventSet_ColorIndexReceive);

  FEventSet_coordIndex := TMFInt32Event.Create(Self, 'set_coordIndex', true);
  AddEvent(FEventSet_coordIndex);
  Eventset_coordIndex.OnReceive.Add(@EventSet_CoordIndexReceive);

  FFdAttrib := TMFNode.Create(Self, 'attrib', [TAbstractVertexAttributeNode]);
   FdAttrib.ChangesAlways := [chGeometry];
  AddField(FFdAttrib);

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  AddField(FFdColor);

  FFdCoord := TSFNode.Create(Self, 'coord', [TAbstractCoordinateNode]);
   FdCoord.ChangesAlways := [chGeometry];
  AddField(FFdCoord);

  FFdFogCoord := TSFNode.Create(Self, 'fogCoord', [TFogCoordinateNode]);
   FdFogCoord.ChangesAlways := [chGeometry];
  AddField(FFdFogCoord);

  FFdColorIndex := TMFInt32.Create(Self, 'colorIndex', []);
   FdColorIndex.Exposed := false;
   FdColorIndex.SaveToStreamLineUptoNegative := true;
   FdColorIndex.ChangesAlways := [chGeometry];
  AddField(FFdColorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdColorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdColorPerVertex.Exposed := false;
   FdColorPerVertex.ChangesAlways := [chGeometry];
  AddField(FFdColorPerVertex);

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', []);
   FdCoordIndex.Exposed := false;
   FdCoordIndex.SaveToStreamLineUptoNegative := true;
   FdCoordIndex.ChangesAlways := [chGeometry];
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }
end;

class function TIndexedLineSetNode.ClassX3DType: string;
begin
  Result := 'IndexedLineSet';
end;

class function TIndexedLineSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

class function TIndexedLineSetNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TIndexedLineSetNode.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdCoord.Value <> nil) and
     (FdCoord.Value is TCoordinateNode) then
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint else
    ACoord := nil;
end;

function TIndexedLineSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TIndexedLineSetNode.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TIndexedLineSetNode.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

function TIndexedLineSetNode.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TIndexedLineSetNode.EventSet_ColorIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdColorIndex.Assign(Value);
  FdColorIndex.Changed;
end;

procedure TIndexedLineSetNode.EventSet_CoordIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdCoordIndex.Assign(Value);
  FdCoordIndex.Changed;
end;

function TIndexedLineSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { VRML 2.0 specification says that IndexedLineSet should always be unlit }
  Result := false;
end;

procedure TIndexedTriangleFanSetNode.CreateNode;
begin
  inherited;

  FEventSet_index := TMFInt32Event.Create(Self, 'set_index', true);
  AddEvent(FEventSet_index);
  Eventset_index.OnReceive.Add(@EventSet_IndexReceive);

  FFdIndex := TMFInt32.Create(Self, 'index', []);
   FdIndex.Exposed := false;
   FdIndex.ChangesAlways := [chGeometry];
  AddField(FFdIndex);
  { X3D specification comment: [0,Inf) or -1 }
end;

class function TIndexedTriangleFanSetNode.ClassX3DType: string;
begin
  Result := 'IndexedTriangleFanSet';
end;

class function TIndexedTriangleFanSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TIndexedTriangleFanSetNode.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleFanSetNode.EventSet_IndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Assign(Value);
  FdIndex.Changed;
end;

procedure TIndexedTriangleSetNode.CreateNode;
begin
  inherited;

  FEventSet_index := TMFInt32Event.Create(Self, 'set_index', true);
  AddEvent(FEventSet_index);
  Eventset_index.OnReceive.Add(@EventSet_IndexReceive);

  FFdIndex := TMFInt32.Create(Self, 'index', []);
   FdIndex.Exposed := false;
   FdIndex.ChangesAlways := [chGeometry];
  AddField(FFdIndex);
  { X3D specification comment: [0,Inf) }
end;

class function TIndexedTriangleSetNode.ClassX3DType: string;
begin
  Result := 'IndexedTriangleSet';
end;

class function TIndexedTriangleSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TIndexedTriangleSetNode.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleSetNode.EventSet_IndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Assign(Value);
  FdIndex.Changed;
end;

procedure TIndexedTriangleStripSetNode.CreateNode;
begin
  inherited;

  FEventSet_index := TMFInt32Event.Create(Self, 'set_index', true);
  AddEvent(FEventSet_index);
  Eventset_index.OnReceive.Add(@EventSet_IndexReceive);

  FFdIndex := TMFInt32.Create(Self, 'index', []);
   FdIndex.Exposed := false;
   FdIndex.ChangesAlways := [chGeometry];
  AddField(FFdIndex);
  { X3D specification comment: [0,Inf) or -1 }
end;

class function TIndexedTriangleStripSetNode.ClassX3DType: string;
begin
  Result := 'IndexedTriangleStripSet';
end;

class function TIndexedTriangleStripSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TIndexedTriangleStripSetNode.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleStripSetNode.EventSet_IndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Assign(Value);
  FdIndex.Changed;
end;

procedure TLineSetNode.CreateNode;
begin
  inherited;

  FFdAttrib := TMFNode.Create(Self, 'attrib', [TAbstractVertexAttributeNode]);
   FdAttrib.ChangesAlways := [chGeometry];
  AddField(FFdAttrib);

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  AddField(FFdColor);

  FFdCoord := TSFNode.Create(Self, 'coord', [TAbstractCoordinateNode]);
   FdCoord.ChangesAlways := [chGeometry];
  AddField(FFdCoord);

  FFdFogCoord := TSFNode.Create(Self, 'fogCoord', [TFogCoordinateNode]);
   FdFogCoord.ChangesAlways := [chGeometry];
  AddField(FFdFogCoord);

  FFdVertexCount := TMFInt32.Create(Self, 'vertexCount', []);
   FdVertexCount.ChangesAlways := [chGeometry];
  AddField(FFdVertexCount);
  { X3D specification comment: [2,Inf) }
end;

class function TLineSetNode.ClassX3DType: string;
begin
  Result := 'LineSet';
end;

class function TLineSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TLineSetNode.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdCoord.Value <> nil) and
     (FdCoord.Value is TCoordinateNode) then
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint else
    ACoord := nil;
end;

function TLineSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TLineSetNode.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TLineSetNode.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

function TLineSetNode.CoordRangesCounts(
  out RangeCount: TLongIntList;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  RangeCount := FdVertexCount.Items;
  SRanges := 'lines';
  SRangeName := 'Vertex count';
  RangeMinimumCount := 2;
  Result := true;
end;

function TLineSetNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TLineSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { X3D specification says that LineSet should always be unlit }
  Result := false;
end;

procedure TNormalNode.CreateNode;
begin
  inherited;

  FFdVector := TMFVec3f.Create(Self, 'vector', []);
   FdVector.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdVector);
  { X3D specification comment: [-1,1] }

  DefaultContainerField := 'normal';
end;

class function TNormalNode.ClassX3DType: string;
begin
  Result := 'Normal';
end;

class function TNormalNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPointSetNode.CreateNode;
begin
  inherited;

  FFdAttrib := TMFNode.Create(Self, 'attrib', [TAbstractVertexAttributeNode]);
   FdAttrib.ChangesAlways := [chGeometry];
  AddField(FFdAttrib);

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  AddField(FFdColor);

  FFdCoord := TSFNode.Create(Self, 'coord', [TAbstractCoordinateNode]);
   FdCoord.ChangesAlways := [chGeometry];
  AddField(FFdCoord);

  FFdFogCoord := TSFNode.Create(Self, 'fogCoord', [TFogCoordinateNode]);
   FdFogCoord.ChangesAlways := [chGeometry];
  AddField(FFdFogCoord);
end;

class function TPointSetNode.ClassX3DType: string;
begin
  Result := 'PointSet';
end;

class function TPointSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

class function TPointSetNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TPointSetNode.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdCoord.Value <> nil) and
     (FdCoord.Value is TCoordinateNode) then
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint else
    ACoord := nil;
end;

function TPointSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TPointSetNode.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TPointSetNode.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

function TPointSetNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TPointSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  Result := false;
end;

procedure TTriangleFanSetNode.CreateNode;
begin
  inherited;

  FFdFanCount := TMFInt32.Create(Self, 'fanCount', []);
   FdFanCount.ChangesAlways := [chGeometry];
  AddField(FFdFanCount);
  { X3D specification comment: [3,Inf) }
end;

class function TTriangleFanSetNode.ClassX3DType: string;
begin
  Result := 'TriangleFanSet';
end;

class function TTriangleFanSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TTriangleFanSetNode.CoordRangesCounts(
  out RangeCount: TLongIntList;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  RangeCount := FdFanCount.Items;
  SRanges := 'fans';
  SRangeName := 'Fan count';
  RangeMinimumCount := 3;
  Result := true;
end;

procedure TTriangleSetNode.CreateNode;
begin
  inherited;
end;

class function TTriangleSetNode.ClassX3DType: string;
begin
  Result := 'TriangleSet';
end;

class function TTriangleSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTriangleStripSetNode.CreateNode;
begin
  inherited;

  FFdStripCount := TMFInt32.Create(Self, 'stripCount', []);
   FdStripCount.ChangesAlways := [chGeometry];
  AddField(FFdStripCount);
  { X3D specification comment: [3,Inf) }
end;

class function TTriangleStripSetNode.ClassX3DType: string;
begin
  Result := 'TriangleStripSet';
end;

class function TTriangleStripSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TTriangleStripSetNode.CoordRangesCounts(
  out RangeCount: TLongIntList;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  RangeCount := FdStripCount.Items;
  SRanges := 'strips';
  SRangeName := 'Strip count';
  RangeMinimumCount := 3;
  Result := true;
end;

procedure RegisterRenderingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TClipPlaneNode,
    TColorNode,
    TColorRGBANode,
    TCoordinateNode,
    TIndexedLineSetNode,
    TIndexedTriangleFanSetNode,
    TIndexedTriangleSetNode,
    TIndexedTriangleStripSetNode,
    TLineSetNode,
    TNormalNode,
    TPointSetNode,
    TTriangleFanSetNode,
    TTriangleSetNode,
    TTriangleStripSetNode
  ]);
end;

{$endif read_implementation}
