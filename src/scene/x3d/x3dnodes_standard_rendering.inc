{%MainUnit x3dnodes.pas}
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

{$ifdef read_interface}
  { }
  TCoordRangeHandler = procedure (const RangeNumber: Cardinal;
    BeginIndex, EndIndex: Integer) of object;

  TIndexedPolygonHandler = procedure (const Indexes: array of Cardinal)
    of object;

  ENotCoordinateBasedNode = class(EX3DError);

  { Base node for a visible geometry in X3D. }
  TAbstractGeometryNode = class(TAbstractNode)
  strict private
    HasBounds2DST: boolean;
    Bounds2DS, Bounds2DT: T3DAxis;
    FIgnoredSolid: boolean; {< keeps value of Solid property when FdSolid is nil }
    FIgnoredConvex: boolean; {< keeps value of Convex property when FdConvex is nil }
    MappingWarningDone: Boolean;
    { Workaround http://bugs.freepascal.org/view.php?id=17402
      by these methods. }
    function CallProxyLocalBoundingBox(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
    function CallProxyBoundingBox(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
    function CallProxyTrianglesCount(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
    function CallProxyVerticesCount(
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
    function GetSolid: boolean;
    procedure SetSolid(const Value: boolean);
    function GetConvex: boolean;
    procedure SetConvex(const Value: boolean);
  protected
    { Returns an information how to split @link(InternalCoord) array into ranges.

      When CoordIndex = @nil, then if the node's @link(InternalCoord) array
      can be divided into some "ranges", we will use this information.
      This is used (and should be overridden) for X3D non-indexed nodes,
      like fanCount or stripCount or vertexCount.

      What precisely is a "range of coordinates" is not specified
      here. It may be a line stip, or one triangle strip, etc. ---
      depending on the descendant.

      Returns @true if this is available. In this case, RangeCount must
      be set to something <> nil, and the rest of returned variables
      are mainly to generate proper warnings by MakeCoordRanges. }
    function InternalCoordRangesCounts(out RangeCount: TInt32List;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; virtual;
  public
    { Used only for reading glTF now, in the future will be used for skin animation on GPU. @exclude }
    InternalSkinJoints: TVector4IntegerList;
    { Used only for reading glTF now, in the future will be used for skin animation on GPU. @exclude }
    InternalSkinWeights: TVector4List;

    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;

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
          For nodes based on coordinates (when @link(InternalCoord) returns @true),
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

      For coordinate-based nodes (when @link(InternalCoord) returns @true),
      VerticesCount is already implemented in this class.
      Using Coord method, no other information is needed.

      For other nodes, the default implementation of
      both VerticesCount and TrianglesCount in this TAbstractGeometryNode
      class will use ProxyGeometry and ProxyState to do the work.
      You should override these methods if @link(Proxy) is not available
      (so caller will always pass ProxyGeometry = @nil)
      or some faster approach is possible.

      @groupBegin }
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode;
      ProxyState: TX3DGraphTraverseState): Cardinal; virtual;
    function TrianglesCount(State: TX3DGraphTraverseState;
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
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; virtual;

    { Return node's list of coordinates, raising exception if node
      is not based on coordinates.

      This is just like the @link(InternalCoord) method,
      except it simply returns the coordinates, not the boolean result.
      If virtual @link(InternalCoord) returns @false (indicating the node
      is not coordinate-based) this raises ENotCoordinateBasedNode.

      @raises(ENotCoordinateBasedNode If node is not coordinate-based,
        that is @link(InternalCoord) returns false.)
    }
    function InternalCoordinates(State: TX3DGraphTraverseState): TMFVec3f;

    { Node's "coord" field where you can place TCoordinateNode, or @nil if not available.

      This gives you more possibilities than the @link(InternalCoord)
      and @link(InternalCoordinates) methods (as you can assign texCoord using this).
      However, it doesn't work for old VRML 1.0 (since they have coordinate
      information, but no "coord" field). }
    function CoordField: TSFNode; virtual;

    { Node's "normal" field where you can place TNormalNode, or @nil if not available. }
    function NormalField: TSFNode; virtual;

    { Node's "tangent" field where you can place TTangentNode, or @nil if not available. }
    function TangentField: TSFNode; virtual;

    { Returns normal vectors that are specified explicitly.
      The vectors are taken from the Normal node inside our FdNormal field.
      In case of problems (no Normal node specified, invalid node specified etc.)
      returns @nil. }
    function InternalNormal: TVector3List;

    { Returns tangent vectors that are specified explicitly, see @link(TAbstractComposedGeometryNode.Tangent).
      Returns @nil if not specified. }
    function InternalTangent: TVector3List;

    { Node's list of coordinate indexes.

      In base TAbstractGeometryNode class this always returns @nil.

      Override this for descendants that have some kind of "coordIndex"
      or "index" field used to index @link(InternalCoord) array. }
    function CoordIndexField: TMFLong; virtual;

    { Splits @link(InternalCoord) array into ranges.

      If CoordIndex is assigned, then a "range of coordinates" is
      just a range of non-negative indexes within CoordIndex.
      Otherwise (when CoordIndex = @nil), InternalCoordRangesCounts must
      return @true and we will use RangeCount to split coordinates.

      Call this only for nodes with coordinates, that is only when
      @link(InternalCoord) returns @true. }
    procedure InternalMakeCoordRanges(
      State: TX3DGraphTraverseState;
      CoordRangeHandler: TCoordRangeHandler);

    { Splits coordinate-based node into polygons.

      Indexes in PolygonHandler point to CoordIndex, if assigned,
      or directly to Coord. The ordering of generated polygons is correct,
      so what pointed CCW in the node field, will still point CCW
      according to generated PolygonHandler indexes.

      In this class this does nothing. Some, but not all, coordinate-based
      nodes (the ones when @link(InternalCoord) returns @true) override this.
      So currently, whether this is implemented is coordinated with
      CastleInternalNormals and such internal needs. }
    procedure InternalCoordPolygons(
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
    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; virtual;

    { Node's texCoord field, or @nil if not available.
      Various nodes may have different exact rules about what is
      allowed here, but everything allows TextureCoordinateGenerator
      and ProjectedTextureCoordinate instances.

      This gives you more possibilities than the @link(InternalTexCoord) method
      (as you can assign texCoord using this), however it may be not available
      in all cases --- for example VRML 1.0 nodes do not have texCoord field,
      but they may have a texture coordinate node (from the state). }
    function TexCoordField: TSFNode; virtual;

    { Find a texture coordinate node with given Mapping value.
      Returns nil (and Index = -1) if not found.
      This implements X3D 4.0 section
      "12.2.4 Texture mapping specified in material nodes".

      If MakeWarning, we make a warning if Mapping is not empty,
      and yet it doesn't match anything. This is not allowed, according to spec. }
    function FindTextureMapping(const Mapping: String;
      const MakeWarning: Boolean = true): TAbstractSingleTextureCoordinateNode; overload;
    function FindTextureMapping(const Mapping: String; out TexCoordIndex: Integer;
      const MakeWarning: Boolean = true): TAbstractSingleTextureCoordinateNode; overload;

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
        State := TX3DGraphTraverseState.CreateCopy(State);
      #)

      You should @italic(not) just modify the fields
      of the provided State instance. (Reasoning: some proxy methods
      rely on getting the original State, e.g. with original MaterialBinding,
      not the transformed state, to work correctly.)

      You can modify State variable only when returning non-nil geometry. }
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; virtual;

    { Returns color.point field, assuming that "color" field is set and
      specifies Color (or ColorRGBA) node. Otherwise returns @nil.

      Note that only one of Color and ColorRGBA may return non-nil,
      since "color" field may contain only one of them.

      @groupBegin }
    function InternalColor: TMFVec3f;
    function InternalColorRGBA: TMFColorRGBA;
    function InternalColorNode: TAbstractColorNode;
    { @groupEnd }

    function InternalFogCoord: TMFFloat; virtual;
    function AttribField: TMFNode; virtual;

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
    procedure GetTextureBounds2DST(const LocalBoxSizes: TVector3;
      out S, T: T3DAxis);

    { Is this object lit, disregarding the material.
      Default implementation in TAbstractGeometryNode says @true. }
    function Lit(State: TX3DGraphTraverseState): boolean; virtual;

    { Should renderer setup an extra texture slot with font texture
      when rendering this node. This is useful for rendering Text nodes,
      that want to cooperate with normal texturing and shading,
      and additionally they want to use extra texture determined by font
      (not by Appearance node). }
    function FontTextureNode: TAbstractTexture2DNode; virtual;

    { Are faces guaranteed to be convex.
      If your faces may be concave, you must set this to @false,
      otherwise the results (rendering and collisions) may be incorrect. }
    property Convex: boolean read GetConvex write SetConvex;

    { Is backface culling used. }
    property Solid: boolean read GetSolid write SetSolid;

    { Create an instance of this geometry node,
      and make it a child of a new shape node (TShapeNode).
      This way you get a ready shape that can be used inside the X3D scene graph
      to show this geometry.

      You usually want to add the resulting Shape node to some scene,
      e.g. like this:

      @longCode(#
      Box := TBoxNode.CreateWithShape(Shape);

      Root := TX3DRootNode.Create;
      Root.AddChildren(Shape);

      Scene := TCastleScene.Create(nil);
      Scene.Load(Root, true);
      #)

      Remember that once the node is a children of another node,
      then this node will be automatically freed.
      So you should not free the resulting geometry node.
      Instead, free the resulting Shape node,
      or add it to some scene, as shown above.
    }
    constructor CreateWithShape(out Shape: TShapeNode);

    constructor CreateShape(out Shape: TShapeNode); deprecated 'use CreateWithShape';

    { Create an instance of this geometry node,
      and make it a child of a new shape node (TShapeNode),
      and make it a child of a new transform node (TTransformNode).
      This way you get a ready transformation that can be used inside
      the X3D scene graph to show and transform this geometry.

      You usually want to add the resulting Transform node to some scene,
      e.g. like this:

      @longCode(#
      Box := TBoxNode.CreateWithTransform(Shape, Transform);

      Root := TX3DRootNode.Create;
      Root.AddChildren(Transform);

      Scene := TCastleScene.Create(nil);
      Scene.Load(Root, true);
      #)

      Remember that once the node is a children of another node,
      then this node will be automatically freed.
      So you should not free the resulting geometry node
      or Shape node.
      Instead, free the resulting Transform node,
      or add it to some scene, as shown above.
    }
    constructor CreateWithTransform(out Shape: TShapeNode; out Transform: TTransformNode);

    constructor CreateTransform(out Shape: TShapeNode; out Transform: TTransformNode); deprecated 'use CreateWithTransform';

  {$I auto_generated_node_helpers/x3dnodes_x3dgeometrynode.inc}
  end;

  TAbstractGeometryNodeClass = class of TAbstractGeometryNode;

  { Base node type for all geometric property node types. }
  TAbstractGeometricPropertyNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dgeometricpropertynode.inc}
  end;

  TAbstractX3DGeometryNode = TAbstractGeometryNode deprecated 'use TAbstractGeometryNode';

  { Base node for color specifications. }
  TAbstractColorNode = class(TAbstractGeometricPropertyNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dcolornode.inc}
  end;

  TAbstractVertexAttributeNode = class;

  { Base node type for all composed 3D geometry. }
  TAbstractComposedGeometryNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    { Access the coordinate node, and inside this node access it's points list. }
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordField: TSFNode; override;
    function TexCoordField: TSFNode; override;
    function ColorField: TSFNode; override;
    function SolidField: TSFBool; override;
    function AttribField: TMFNode; override;
    function NormalField: TSFNode; override;
    function TangentField: TSFNode; override;

    function InternalFogCoord: TMFFloat; override;

  {$I auto_generated_node_helpers/x3dnodes_x3dcomposedgeometrynode.inc}
  end;

  { Base node type for all coordinate node types. }
  TAbstractCoordinateNode = class(TAbstractGeometricPropertyNode)
  public
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
  protected
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
  {$I auto_generated_node_helpers/x3dnodes_clipplane.inc}
  end;

  { Set of RGB colours. }
  TColorNode = class(TAbstractColorNode)
  {$I auto_generated_node_helpers/x3dnodes_color.inc}
  end;

  { Set of RGBA colours. }
  TColorRGBANode = class(TAbstractColorNode)
  {$I auto_generated_node_helpers/x3dnodes_colorrgba.inc}
  end;

  { Set of 3D coordinates to be used in the @code(Coord)
    field of vertex-based geometry nodes. }
  TCoordinateNode = class(TAbstractCoordinateNode)
  public
    function CoordCount: Cardinal; override;
  {$I auto_generated_node_helpers/x3dnodes_coordinate.inc}
  end;

  { Mode for line primitive defined by TIndexedLineSetNode or TLineSetNode. }
  TLineMode = (
    { Lines form a strip (polyline).

      In case of TIndexedLineSetNode, indexes (TIndexedLineSetNode.FdIndex)
      may to be separated by -1 to define many strips
      in an TIndexedLineSetNode.

      In case of TLineSetNode, count (TIndexedLineSetNode.FdVertexCount)
      defines how many vertexes are for each strip. }
    lmStrip,

    { Lines form a loop (closed polyline).
      In case of TIndexedLineSetNode, indexes (TIndexedLineSetNode.FdIndex)
      may to be separated by -1 to define many loop
      in an TIndexedLineSetNode.

      In case of TLineSetNode, count (TIndexedLineSetNode.FdVertexCount)
      defines how many vertexes are for each loop. }
    lmLoop,

    { Each pair of vertexes forms a line.
      If the number of vertexes is odd, the last (unmatched) vertex is ignored.
      In case of TIndexedLineSetNode, all indexes must be >= 0.
      In case of TLineSetNode, vertexCount is ignored. }
    lmPair
  );

  { Geometry formed by constructing polylines from 3D vertices. }
  TIndexedLineSetNode = class(TAbstractGeometryNode)
  strict private
    procedure EventSet_ColorIndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventSet_CoordIndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndexField: TMFLong; override;
    function CoordField: TSFNode; override;
    function ColorField: TSFNode; override;
    function InternalFogCoord: TMFFloat; override;
    function AttribField: TMFNode; override;

  {$I auto_generated_node_helpers/x3dnodes_indexedlineset.inc}
  end;
  TIndexedLineSetNode_2 = TIndexedLineSetNode;

  { Triangles that form a fan shape around the first vertex. }
  TIndexedTriangleFanSetNode = class(TAbstractComposedGeometryNode)
  strict private
    procedure EventSet_IndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; override;
    function CoordIndexField: TMFLong; override;
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_indexedtrianglefanset.inc}
  end;

  { A collection of triangles. }
  TIndexedTriangleSetNode = class(TAbstractComposedGeometryNode)
  strict private
    procedure EventSet_IndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; override;
    function CoordIndexField: TMFLong; override;
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_indexedtriangleset.inc}
  end;

  { Strips of triangles. }
  TIndexedTriangleStripSetNode = class(TAbstractComposedGeometryNode)
  strict private
    procedure EventSet_IndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; override;
    function CoordIndexField: TMFLong; override;
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_indexedtrianglestripset.inc}
  end;

  { Lines. }
  TLineSetNode = class(TAbstractGeometryNode)
  strict private
    FVertexCountForPair: TInt32List;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    function InternalCoordRangesCounts(out RangeCount: TInt32List;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;
  public
    destructor Destroy; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordField: TSFNode; override;
    function ColorField: TSFNode; override;
    function InternalFogCoord: TMFFloat; override;
    function AttribField: TMFNode; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;

  {$I auto_generated_node_helpers/x3dnodes_lineset.inc}
  end;

  { Normal (information for lighting) node. }
  TNormalNode = class(TAbstractNormalNode)
  {$I auto_generated_node_helpers/x3dnodes_normal.inc}
  end;

  { Tangent vectors information, for bump mapping.
    The "vector" field should contain normalized tangent vectors,
    in right-handed coordinate system.

    The ordering of the tangent vectors is exactly the same as ordering
    of normal vectors would be,
    so e.g. @link(TAbstractComposedGeometryNode.NormalPerVertex) affects
    the interpretation of tangent vectors as well. }
  TTangentNode = class(TAbstractGeometricPropertyNode)
    {$I auto_generated_node_helpers/x3dnodes_tangent.inc}
  public
    { Set tangent vectors from a list of 4D vectors following glTF specification.
      https://github.com/KhronosGroup/glTF/tree/master/specification/2.0 says
      "XYZW vertex tangents where the w component is a sign value
      (-1 or +1) indicating handedness of the tangent basis". }
    procedure SetVector4D(const List: TVector4List);
  end;

  { Points. }
  TPointSetNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordField: TSFNode; override;
    function ColorField: TSFNode; override;
    function InternalFogCoord: TMFFloat; override;
    function AttribField: TMFNode; override;

  {$I auto_generated_node_helpers/x3dnodes_pointset.inc}
  end;
  TPointSetNode_2 = TPointSetNode;

  { Triangles that form a fan shape around the first vertex.
    In constrast to @link(TIndexedTriangleFanSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleFanSetNode = class(TAbstractComposedGeometryNode)
  protected
    function InternalCoordRangesCounts(out RangeCount: TInt32List;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_trianglefanset.inc}
  end;

  { A collection of triangles.
    In constrast to @link(TIndexedTriangleSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_triangleset.inc}
  end;

  { Strips of triangles.
    In contrast to @link(TIndexedTriangleStripSetNode), this doesn't use indexes
    to compose vertexes into triangles. }
  TTriangleStripSetNode = class(TAbstractComposedGeometryNode)
  protected
    function InternalCoordRangesCounts(out RangeCount: TInt32List;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; override;
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_trianglestripset.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

const
  ColorModeNames: array [TColorMode] of String = (
    'REPLACE', 'MODULATE'
  );

  LineModeNames: array [TLineMode] of String = (
    'STRIP', 'LOOP', 'PAIR'
  );

{ TAbstractGeometryNode ---------------------------------------------------------- }

constructor TAbstractGeometryNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;

  { To make nodes without FdSolid and FdConvex field behave consistently,
    we keep the value you set by SetSolid / SetConvex to return it in GetSolid / GetConvex.
    The defaults are "true", which are more efficient (e.g. solid=true
    doesn't activate two-sided lighting) and correspond to X3D defaults on other nodes. }
  FIgnoredSolid := true;
  FIgnoredConvex := true;
end;

destructor TAbstractGeometryNode.Destroy;
begin
  FreeAndNil(InternalSkinJoints);
  FreeAndNil(InternalSkinWeights);
  inherited;
end;

constructor TAbstractGeometryNode.CreateWithShape(out Shape: TShapeNode);
begin
  Create;

  Shape := TShapeNode.Create;
  Shape.Geometry := Self;
end;

constructor TAbstractGeometryNode.CreateShape(out Shape: TShapeNode);
begin
  CreateWithShape(Shape);
end;

constructor TAbstractGeometryNode.CreateWithTransform(out Shape: TShapeNode; out Transform: TTransformNode);
begin
  Create;

  Shape := TShapeNode.Create;
  Shape.Geometry := Self;

  Transform := TTransformNode.Create;
  Transform.AddChildren(Shape);
end;

constructor TAbstractGeometryNode.CreateTransform(out Shape: TShapeNode; out Transform: TTransformNode);
begin
  CreateWithTransform(Shape, Transform);
end;

function TAbstractGeometryNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := false;
end;

function TAbstractGeometryNode.InternalCoordinates(State: TX3DGraphTraverseState): TMFVec3f;
begin
  if not InternalCoord(State, Result) then
    raise ENotCoordinateBasedNode.CreateFmt('Node %s is not a coordinate-based node',
      [X3DType]);
end;

function TAbstractGeometryNode.CoordField: TSFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.CoordIndexField: TMFLong;
begin
  Result := nil;
end;

function TAbstractGeometryNode.InternalCoordRangesCounts(
  out RangeCount: TInt32List;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  Result := false;
end;

procedure TAbstractGeometryNode.InternalMakeCoordRanges(
  State: TX3DGraphTraverseState;
  CoordRangeHandler: TCoordRangeHandler);
var
  BeginIndex, EndIndex: Integer;
  RangeNumber: Cardinal;
  RangeCount: TInt32List;
  SRanges, SRangeName: string;
  RangeMinimumCount: Cardinal;
  C: TMFVec3f;
begin
  C := InternalCoordinates(State);

  if C = nil then
    Exit;

  if CoordIndexField <> nil then
  begin
    BeginIndex := 0;
    RangeNumber := 0;
    while BeginIndex < CoordIndexField.Count do
    begin
      EndIndex := BeginIndex;
      while (EndIndex < CoordIndexField.Count) and
            (CoordIndexField.Items.Items[EndIndex] >= 0) do
        Inc(EndIndex);
      CoordRangeHandler(RangeNumber, BeginIndex, EndIndex);
      Inc(RangeNumber);
      BeginIndex := EndIndex + 1;
    end;
  end else
  begin
    if not InternalCoordRangesCounts(RangeCount, SRanges, SRangeName,
      RangeMinimumCount) then
      raise EInternalError.CreateFmt('%s.InternalMakeCoordRanges: either CoordIndex or InternalCoordRangesCounts must be defined to split coordinates', [ClassName]);
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

procedure TAbstractGeometryNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
begin
  { Nothing to do in this class. }
end;

function TAbstractGeometryNode.InternalTexCoord(State: TX3DGraphTraverseState;
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

function TAbstractGeometryNode.FindTextureMapping(const Mapping: String;
  const MakeWarning: Boolean): TAbstractSingleTextureCoordinateNode;
var
  TexCoordIndex: Integer;
begin
  Result := FindTextureMapping(Mapping, TexCoordIndex, MakeWarning);
end;

function TAbstractGeometryNode.FindTextureMapping(const Mapping: String;
  out TexCoordIndex: Integer; const MakeWarning: Boolean): TAbstractSingleTextureCoordinateNode;
var
  T: TX3DNode;
  MultiCoord: TMultiTextureCoordinateNode;
  I: Integer;
begin
  Result := nil;
  TexCoordIndex := -1;

  if TexCoordField = nil then
    Exit;

  T := TexCoordField.Value;
  if T is TMultiTextureCoordinateNode then
  begin
    MultiCoord := TMultiTextureCoordinateNode(T);
    for I := 0 to MultiCoord.FdTexCoord.Count - 1 do
      if (MultiCoord.FdTexCoord[I] is TAbstractSingleTextureCoordinateNode) and
         { Mapping empty matches first item.
           All children of MultiTextureCoordinate should be TAbstractSingleTextureCoordinateNode
           in valid files. }
         ( (Mapping = '') or
           (Mapping = TAbstractSingleTextureCoordinateNode(MultiCoord.FdTexCoord[I]).Mapping) ) then
      begin
        Result := TAbstractSingleTextureCoordinateNode(MultiCoord.FdTexCoord[I]);
        TexCoordIndex := I;
        Exit;
      end;

    if MakeWarning and (Mapping <> '') and (not MappingWarningDone) then
    begin
      WritelnWarning('Texture mapping "%s" not found within MultiTextureCoordinate list', [Mapping]);
      MappingWarningDone := true;
    end;
  end else
  if T is TAbstractSingleTextureCoordinateNode then
  begin
    if ( (Mapping = '') or
         (Mapping = TAbstractSingleTextureCoordinateNode(T).Mapping) ) then
    begin
      Result := TAbstractSingleTextureCoordinateNode(T);
      TexCoordIndex := 0;
      Exit;
    end;

    if MakeWarning and (Mapping <> '') and (not MappingWarningDone) then
    begin
      WritelnWarning('Texture mapping "%s" not matching the only texture coordinate node possible', [Mapping]);
      MappingWarningDone := true;
    end;
  end;
end;

function TAbstractGeometryNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.InternalFogCoord: TMFFloat;
begin
  Result := nil;
end;

function TAbstractGeometryNode.AttribField: TMFNode;
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

procedure TAbstractGeometryNode.GetTextureBounds2DST(const LocalBoxSizes: TVector3;
  out S, T: T3DAxis);
var
  C1, C2: T3DAxis;
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
    RestOf3DCoords(Bounds2DS, C1, C2);
    if LocalBoxSizes[C1] >= LocalBoxSizes[C2] then
      Bounds2DT := C1
    else
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

function TAbstractGeometryNode.InternalColorNode: TAbstractColorNode;
var
  F: TSFNode;
begin
  F := ColorField;
  if (F <> nil) and
     (F.Value is TAbstractColorNode) then
    Result := TAbstractColorNode(F.Value)
  else
    Result := nil;
end;

function TAbstractGeometryNode.InternalColor: TMFVec3f;
var
  F: TSFNode;
begin
  F := ColorField;
  if (F <> nil) and
     (F.Value is TColorNode) then
    Result := TColorNode(F.Value).FdColor
  else
    Result := nil;
end;

function TAbstractGeometryNode.InternalColorRGBA: TMFColorRGBA;
var
  F: TSFNode;
begin
  F := ColorField;
  if (F <> nil) and
     (F.Value is TColorRGBANode) then
    Result := TColorRGBANode(F.Value).FdColor
  else
    Result := nil;
end;

function TAbstractGeometryNode.FontTextureNode: TAbstractTexture2DNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.NormalField: TSFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.TangentField: TSFNode;
begin
  Result := nil;
end;

function TAbstractGeometryNode.InternalNormal: TVector3List;
begin
  if (NormalField <> nil) and
     (NormalField.Value is TNormalNode) then
    Result := TNormalNode(NormalField.Value).FdVector.Items
  else
    Result := nil;
end;

function TAbstractGeometryNode.InternalTangent: TVector3List;
begin
  if (TangentField <> nil) and
     (TangentField.Value is TTangentNode) then
    Result := TTangentNode(TangentField.Value).FdVector.Items
  else
    Result := nil;
end;

{ TAbstractComposedGeometryNode ------------------------------------------------------ }

function TAbstractComposedGeometryNode.CoordField: TSFNode;
begin
  Result := FdCoord;
end;

function TAbstractComposedGeometryNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdCoord.Value is TCoordinateNode then // also checks that FdCoord.Value <> nil
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint
  else
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

function TAbstractComposedGeometryNode.InternalFogCoord: TMFFloat;
begin
  {$ifndef CASTLE_SLIM_NODES}
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth
  else
  {$endif}
    Result := nil;
end;

function TAbstractComposedGeometryNode.AttribField: TMFNode;
begin
  Result := {$ifndef CASTLE_SLIM_NODES} FdAttrib {$else} nil {$endif};
end;

function TAbstractComposedGeometryNode.NormalField: TSFNode;
begin
  Result := FdNormal;
end;

function TAbstractComposedGeometryNode.TangentField: TSFNode;
begin
  Result := FdTangent;
end;

function TAbstractComposedGeometryNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  {$ifndef CASTLE_SLIM_NODES}
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  {$ifndef CASTLE_SLIM_NODES}
  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}

  Result := FdNormal.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdTangent.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ TClipPlaneNode ------------------------------------------------------------- }

procedure TClipPlaneNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
var
  CP: PClipPlane;
begin
  inherited;
  CP := State.AddClipPlane;
  CP^.Node := Self;
  CP^.Transform := State.Transformation.Transform;
end;

{ TCoordinateNode ------------------------------------------------------------ }

function TCoordinateNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

{ TIndexedLineSetNode -------------------------------------------------------- }

constructor TIndexedLineSetNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_colorIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_ColorIndexReceive);
  Eventset_coordIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_CoordIndexReceive);
end;

class function TIndexedLineSetNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TIndexedLineSetNode.CoordField: TSFNode;
begin
  Result := FdCoord;
end;

function TIndexedLineSetNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdCoord.Value is TCoordinateNode then // also checks that FdCoord.Value <> nil
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint
  else
    ACoord := nil;
end;

function TIndexedLineSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TIndexedLineSetNode.InternalFogCoord: TMFFloat;
begin
  {$ifndef CASTLE_SLIM_NODES}
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth
  else
  {$endif}
    Result := nil;
end;

function TIndexedLineSetNode.AttribField: TMFNode;
begin
  Result := {$ifndef CASTLE_SLIM_NODES} FdAttrib {$else} nil {$endif};
end;

function TIndexedLineSetNode.CoordIndexField: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TIndexedLineSetNode.EventSet_ColorIndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdColorIndex.Assign(Value);
  FdColorIndex.Changed;
end;

procedure TIndexedLineSetNode.EventSet_CoordIndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdCoordIndex.Assign(Value);
  FdCoordIndex.Changed;
end;

function TIndexedLineSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { VRML 2.0 specification says that IndexedLineSet should always be unlit }
  Result := false;
end;

{ TIndexedTriangleFanSetNode ------------------------------------------------- }

constructor TIndexedTriangleFanSetNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_index.AddNotification({$ifdef FPC}@{$endif} EventSet_IndexReceive);
end;

function TIndexedTriangleFanSetNode.CoordIndexField: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleFanSetNode.EventSet_IndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Send(Value);
end;

function TIndexedTriangleFanSetNode.FieldSetByEvent(const Event: TX3DEvent): TX3DField;
begin
  if Event = EventSet_index then
    Exit(FdIndex);
  Result := inherited;
end;

{ TIndexedTriangleSetNode ---------------------------------------------------- }

constructor TIndexedTriangleSetNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_index.AddNotification({$ifdef FPC}@{$endif} EventSet_IndexReceive);
end;

function TIndexedTriangleSetNode.CoordIndexField: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleSetNode.EventSet_IndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Send(Value);
end;

function TIndexedTriangleSetNode.FieldSetByEvent(const Event: TX3DEvent): TX3DField;
begin
  if Event = EventSet_index then
    Exit(FdIndex);
  Result := inherited;
end;

{ TIndexedTriangleStripSetNode ----------------------------------------------- }

constructor TIndexedTriangleStripSetNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_index.AddNotification({$ifdef FPC}@{$endif} EventSet_IndexReceive);
end;

function TIndexedTriangleStripSetNode.CoordIndexField: TMFLong;
begin
  Result := FdIndex;
end;

procedure TIndexedTriangleStripSetNode.EventSet_IndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdIndex.Send(Value);
end;

function TIndexedTriangleStripSetNode.FieldSetByEvent(const Event: TX3DEvent): TX3DField;
begin
  if Event = EventSet_index then
    Exit(FdIndex);
  Result := inherited;
end;

{ TLineSetNode --------------------------------------------------------------- }

destructor TLineSetNode.Destroy;
begin
  FreeAndNil(FVertexCountForPair);
  inherited;
end;

function TLineSetNode.CoordField: TSFNode;
begin
  Result := FdCoord;
end;

function TLineSetNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdCoord.Value is TCoordinateNode then // also checks that FdCoord.Value <> nil
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint
  else
    ACoord := nil;
end;

function TLineSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TLineSetNode.InternalFogCoord: TMFFloat;
begin
  {$ifndef CASTLE_SLIM_NODES}
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth
  else
  {$endif}
    Result := nil;
end;

function TLineSetNode.AttribField: TMFNode;
begin
  Result := {$ifndef CASTLE_SLIM_NODES} FdAttrib {$else} nil {$endif};
end;

function TLineSetNode.InternalCoordRangesCounts(
  out RangeCount: TInt32List;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
var
  CoordCount: Integer;
begin
  if Mode = lmPair then
  begin
    { For lmPair, return just the count of coordinates.
      These vertexes form one big "lines primitive" for rendering. }
    if FVertexCountForPair = nil then
      FVertexCountForPair := TInt32List.Create;
    if FdCoord.Value is TCoordinateNode then // also checks that FdCoord.Value <> nil
      CoordCount := TCoordinateNode(FdCoord.Value).FdPoint.Count
    else
      CoordCount := 0;
    FVertexCountForPair.Count := 1;
    FVertexCountForPair[0] := CoordCount;
    RangeCount := FVertexCountForPair;
  end else
    RangeCount := FdVertexCount.Items;

  SRanges := 'lines';
  SRangeName := 'Vertex count';
  RangeMinimumCount := 2;
  Result := true;
end;

function TLineSetNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  {$ifndef CASTLE_SLIM_NODES}
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  {$ifndef CASTLE_SLIM_NODES}
  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}
end;

function TLineSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { X3D specification says that LineSet should always be unlit }
  Result := false;
end;

{ TTangentNode --------------------------------------------------------------- }

procedure TTangentNode.SetVector4D(const List: TVector4List);
var
  I, Len: Integer;
  Source: PVector4;
  Dest: PVector3;
  W: Single;
begin
  Len := List.Count;
  FdVector.Items.Count := Len;
  if Len = 0 then Exit;

  Source := PVector4(List.L);
  Dest := PVector3(FdVector.Items.L);
  for I := 0 to Len - 1 do
  begin
    // convert tangents to right-handed
    // slower: Dest^ := Source^.XYZ * Source^.W;
    W := Source^.W;
    Dest^.X := Source^.X * W;
    Dest^.Y := Source^.Y * W;
    Dest^.Z := Source^.Z * W;
    Inc(Source);
    Inc(Dest);
  end;
end;

class function TPointSetNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TPointSetNode.CoordField: TSFNode;
begin
  Result := FdCoord;
end;

function TPointSetNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdCoord.Value is TCoordinateNode then // also checks that FdCoord.Value <> nil
    ACoord := TCoordinateNode(FdCoord.Value).FdPoint
  else
    ACoord := nil;
end;

function TPointSetNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TPointSetNode.InternalFogCoord: TMFFloat;
begin
  {$ifndef CASTLE_SLIM_NODES}
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth
  else
  {$endif}
    Result := nil;
end;

function TPointSetNode.AttribField: TMFNode;
begin
  Result := {$ifndef CASTLE_SLIM_NODES} FdAttrib {$else} nil {$endif};
end;

function TPointSetNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  {$ifndef CASTLE_SLIM_NODES}
  Result := Fdattrib.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}

  Result := Fdcolor.Enumerate(Func);
  if Result <> nil then Exit;

  Result := Fdcoord.Enumerate(Func);
  if Result <> nil then Exit;

  {$ifndef CASTLE_SLIM_NODES}
  Result := FdfogCoord.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}
end;

function TPointSetNode.Lit(State: TX3DGraphTraverseState): boolean;
begin
  Result := false;
end;

{ TTriangleFanSetNode -------------------------------------------------------- }

function TTriangleFanSetNode.InternalCoordRangesCounts(
  out RangeCount: TInt32List;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  RangeCount := FdFanCount.Items;
  SRanges := 'fans';
  SRangeName := 'Fan count';
  RangeMinimumCount := 3;
  Result := true;
end;

{ TTriangleStripSetNode ------------------------------------------------------ }

function TTriangleStripSetNode.InternalCoordRangesCounts(
  out RangeCount: TInt32List;
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
    TTangentNode,
    TPointSetNode,
    TTriangleFanSetNode,
    TTriangleSetNode,
    TTriangleStripSetNode
  ]);
end;

{$endif read_implementation}
