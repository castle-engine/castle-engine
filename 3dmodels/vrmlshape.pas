{
  Copyright 2003-2006,2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ VRML shape (TVRMLShape class) and a simple tree of shapes
  (TVRMLShapeTree class). }

unit VRMLShape;

interface

uses SysUtils, Classes, VectorMath, Boxes3d, VRMLNodes, KambiClassUtils,
  KambiUtils, VRMLTriangleOctree, Frustum, KambiOctree;

{$define read_interface}

const
  DefLocalTriangleOctreeMaxDepth = 10;
  DefLocalTriangleOctreeLeafCapacity = 64 { TODO: why is this different then def?
    this is based on view3dscene defaults?
    castle uses normal defaults?
    maybe use them generally? };
  DefLocalTriangleOctreeLimits: TOctreeLimits = (
    MaxDepth: DefLocalTriangleOctreeMaxDepth;
    LeafCapacity: DefLocalTriangleOctreeLeafCapacity
  );

type
  { Internal type for TVRMLShape
    @exclude }
  TVRMLShapeValidities = set of (svLocalBBox, svBBox,
    svVerticesCountNotOver,  svVerticesCountOver,
    svTrianglesCountNotOver, svTrianglesCountOver,
    svBoundingSphere, svEnableDisplayList);

  { Possible spatial structure types that may be managed by TVRMLShape,
    see TVRMLShape.Spatial. }
  TVRMLShapeSpatialStructure = (
    { Create the TVRMLShape.OctreeTriangles.
      This is an octree containing all triangles. }
    ssTriangles);
  TVRMLShapeSpatialStructures = set of TVRMLShapeSpatialStructure;

  TVRMLShape = class;

  TShapeTraverseFunc = procedure (Shape: TVRMLShape) of object;

  { Tree of VRML shapes.

    Although VRML model already provides the tree (graph of VRML nodes),
    it's a little too complicated to be used at each render call.
    It's especially true for VRML <= 1.0 (where properties may "leak out"
    from one node to the next), VRML >= 2.0 cleaned a lot here but still
    some work must be done when traversing (like accumulating transformations).

    So we process VRML tree to this tree, which is much simpler tree with
    all the geometry nodes (TVRMLGeometryNode) along with their state
    (TVRMLGraphTraverseState) as leafs (TVRMLShape). }
  TVRMLShapeTree = class
  private
    FParentScene: TObject;
  public
    constructor Create(AParentScene: TObject);

    { Parent TVRMLScene instance. This cannot be declared here as
      TVRMLScene (this would create circular unit dependency),
      but it always is TVRMLScene. }
    property ParentScene: TObject read FParentScene write FParentScene;

    procedure Traverse(Func: TShapeTraverseFunc;
      OnlyActive: boolean); virtual; abstract;

    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; virtual; abstract;

    { Look for shape with GeometryNode.NodeName = GeometryNodeName.
      Returns @nil if not found. }
    function FindGeometryNodeName(const GeometryNodeName: string;
      OnlyActive: boolean = false): TVRMLShape;

    { Look for shae with GeometryNode that has a parent named ParentNodeName.
      Parent is searched by GeometryNode.TryFindParentNodeByName.
      Returns @nil if not found. }
    function FindShapeWithParentNamed(const ParentNodeName: string;
      OnlyActive: boolean = false): TVRMLShape;

    { Assuming that the model was created by Blender VRML 1 or 2 exporter,
      this searches for a first shape that was created from Blender
      mesh named BlenderMeshName.

      It follows the logic of two Blender exporters.

      If it doesn't find matching node, returns nil. Otherwise, returns
      the matching shape.

      Note that FindBlenderObject would be usually more sensible
      (since there can be only one shape from given Blender object),
      but Blender VRML 1.0 exporter doesn't export anywhere Blender object
      name. So when working with VRML 1.0, you're stuck with looking
      for mesh names. }
    function FindBlenderMesh(const BlenderMeshName: string;
      OnlyActive: boolean = false): TVRMLShape;
  end;

  { Shape is a geometry node @link(GeometryNode) instance and it's
    @link(State). For VRML >= 2.0, this usually corresponds to
    a single instance of actual VRML @code(Shape) node.
    It allows to perform many operations that need to know both geometry
    and it's current state (parent Shape node, current transformation and such).

    This class caches results of methods LocalBoundingBox, BoundingBox,
    and most others (see TVRMLShapeValidities for hints).
    This means that things work fast, but this also means that
    you must manually call @link(Changed)
    when you changed some properties of GeometryNode or contents of State.

    But note that you can't change GeometryNode or State to different
    objects --- they are readonly properties.

    Also note that if you're using @link(TVRMLScene) class
    then you don't have to worry about calling @link(Changed)
    of items in @link(TVRMLScene.Shapes).
    All you have to do is to call appropriate @code(Changed*)
    methods of @link(TVRMLScene). }
  TVRMLShape = class(TVRMLShapeTree)
  private
    FLocalBoundingBox: TBox3d;
    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLShapeValidities;
    FGeometryNode: TVRMLGeometryNode;
    FState: TVRMLGraphTraverseState;
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;
    FEnableDisplayList: boolean;

    procedure ValidateBoundingSphere;

    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
    function CreateTriangleOctree(const ALimits: TOctreeLimits;
      const ProgressTitle: string): TVRMLTriangleOctree;

    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FOctreeTriangles: TVRMLTriangleOctree;

    FSpatial: TVRMLShapeSpatialStructures;
    procedure SetSpatial(const Value: TVRMLShapeSpatialStructures);
  public
    constructor Create(AParentScene: TObject;
      AGeometryNode: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
    destructor Destroy; override;

    { GeometryNode to wskaznik na obiekt w RootNode }
    property GeometryNode: TVRMLGeometryNode read FGeometryNode;

    { State is OWNED by this TVRMLShape class - I mean, we will do State.Free
      in destructor }
    property State: TVRMLGraphTraverseState read FState;

    { specyfikacja co robia [Local]BoundingBox, VerticesCount i TrianglesCount -
      patrz VRMLNodes.TVRMLGeometryNode }
    function LocalBoundingBox: TBox3d;
    function BoundingBox: TBox3d;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;

    { This calculates bounding sphere basing on BoundingBox.
      In the future this may be changed to use BoundingSphere method
      of @link(TVRMLGeometryNode), when I will implement it.
      For now, BoundingSphere is always worse approximation of bounding
      volume than @link(BoundingBox) (i.e. BoundingSphere is always
      larger) but it may be useful in some cases when
      detecting collision versus bounding sphere is much faster than detecting
      them versus bounding box.

      BoundingSphereRadiusSqr = 0 and BoundingSphereCenter is undefined
      if Box is empty. }
    function BoundingSphereCenter: TVector3Single;
    function BoundingSphereRadiusSqr: Single;

    { This is an information for the TVRMLGLScene renderer
      whether this can be stored in a display list.

      If @false then rendering of this shape cannot be stored
      inside a display list, it must be passed to TVRMLOpenGLRenderer
      in each frame. This is basically a hack to render some nodes that
      change too dynamically to store them in display list.
      For now, it's forced to @false on nodes using MovieTexture. }
    function EnableDisplayList: boolean;

    { This is exactly equivalent to getting
      @link(BoundingSphereCenter) and @link(BoundingSphereRadiusSqr)
      and then using @link(TFrustum.SphereCollisionPossible).

      But it may be a little faster since it avoids some small speed problems
      (like copying memory contents when you get values of
      BoundingSphereXxx properties and checking twice are
      BoundingSphereXxx calculated). }
    function FrustumBoundingSphereCollisionPossible(
      const Frustum: TFrustum): TFrustumCollisionPossible;

    { This is exactly equivalent to getting
      @link(BoundingSphereCenter) and @link(BoundingSphereRadiusSqr)
      and then using @link(TFrustum.SphereCollisionPossibleSimple).

      But it may be a little faster since it avoids some small speed problems. }
    function FrustumBoundingSphereCollisionPossibleSimple(
      const Frustum: TFrustum): boolean;

    procedure Changed;

    { The dynamic octree containing all triangles.
      It contains only triangles within this shape.

      There is no distinction here between collidable / visible
      (as for TVRMLScene octrees), since the whole shape may be
      visible and/or collidable.

      The triangles are specified in local coordinate system of this shape
      (that is, they are independent from transformation within State.Transform).
      This allows the tree to remain unmodified when transformation of this
      shape changes.

      This is automatically managed (initialized, updated, and used)
      by parent TVRMLScene. You usually don't need to know about this
      octree from outside.

      To initialize this, add ssTriangles to @link(Spatial) property,
      otherwise it's @nil. Parent TVRMLScene will take care of this
      (when parent TVRMLScene.Spatial contains ssDynamicCollisions, then
      all shapes contain ssTriangles within their Spatial).

      Parent TVRMLScene will take care to keep this octree always updated.

      Parent TVRMLScene will also take care of actually using
      this octree: TVRMLScene.OctreeCollisions methods actually use the
      octrees of specific shapes at the bottom. }
    property OctreeTriangles: TVRMLTriangleOctree read FOctreeTriangles;

    { Which spatial structrues (octrees, for now) should be created and managed.
      This works analogous to TVRMLScene.Spatial, but this manages
      octrees within this TVRMLShape. }
    property Spatial: TVRMLShapeSpatialStructures read FSpatial write SetSpatial;

    { Properties of created triangle octrees.
      See VRMLTriangleOctree unit comments for description.

      Default value comes from DefLocalTriangleOctreeLimits.

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      @groupBegin }
    function TriangleOctreeLimits: POctreeLimits;

    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;
    { @groupEnd }

    { Update octree, if initialized. }
    procedure LocalGeometryChanged;

    { Internally used by TVRMLScene. Says if local geometry change is scheduled
      (actual change will be done by TVRMLScene.DoGeometryChanged).

      ScheduledLocalGeometryChangedCoord means that coordinates changed.
      If this is all that changed (ScheduledLocalGeometryChanged = @false),
      then this means that model edges structure remains the same
      (this is helpful e.g. to avoid recalculating Manifold/BorderEdges
      in parent scene).

      @groupBegin }
    ScheduledLocalGeometryChanged: boolean;
    ScheduledLocalGeometryChangedCoord: boolean;
    { @groupEnd }

    { Looking at material, decide if it's opaque or (partially)
      transparent.

      For VRML >= 2.0, shape is transparent if material exists and
      has transparency > 0 (epsilon).

      For VRML <= 1.0, for now shape is transparent if all it's
      transparent values (in VRML 1.0, material node has actually many
      material values) have transparency > 0 (epsilon). }
    function Transparent: boolean;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    { Is shape visible, according to VRML Collision node rules.
      Ths is simply a shortcut (with more obvious name) for
      @code(State.InsideInvisible = 0). }
    function Visible: boolean;

    { Is shape collidable, according to VRML Collision node rules.
      Ths is simply a shortcut (with more obvious name) for
      @code(State.InsideIgnoreCollision = 0). }
    function Collidable: boolean;
  end;

  TObjectsListItem_2 = TVRMLShapeTree;
  {$I objectslist_2.inc}
  TVRMLShapeTreesList = TObjectsList_2;

  { Internal (non-leaf) node of the TVRMLShapeTree.
    This is practically just a list of other children
    (other TVRMLShapeTree items).

    All children are considered "active" by this class.

    This class owns it's children TVRMLShapeTree.
    Since TVRMLShapeTree is a simple tree structure, there are no duplicates
    possible, that is given TVRMLShapeTree instance may be within only
    one parent TVRMLShapeTree. (VRML node's parenting mechanism is more
    complicated than this, because of DEF/USE mechanism.) }
  TVRMLShapeTreeGroup = class(TVRMLShapeTree)
  private
    FChildren: TVRMLShapeTreesList;
  public
    constructor Create(AParentScene: TObject);
    destructor Destroy; override;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    property Children: TVRMLShapeTreesList read FChildren;
  end;

  { Node of the TVRMLShapeTree representing an alternative,
    choosing one (or none) child from it's children list as active.

    It's ideal for representing the VRML >= 2.0 Switch node
    (not possible for VRML 1.0 Switch node, as it may affect also other
    nodes after Switch). Actually, it even has a SwitchNode link that is
    used to decide which child to choose (using SwitchNode.FdWhichChoice).  }
  TVRMLShapeTreeSwitch = class(TVRMLShapeTreeGroup)
  private
    FSwitchNode: TNodeSwitch_2;
  public
    property SwitchNode: TNodeSwitch_2 read FSwitchNode write FSwitchNode;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
  end;

  { Node of the TVRMLShapeTree representing the LOD (level of detail) VRML
    concept. It chooses one child from it's children list as active.
    Represents the VRML >= 2.0 LOD node
    (not possible for VRML 1.0 LOD node, as it may affect also other
    nodes after LOD).

    To choose which child is active we need to know the LOD node,
    with it's transformation in VRML graph.
    This information is in LODNode and LODInvertedTransform properties.

    Also, we need to know the current viewer position.
    This is passed as ViewerPosition to CalculateLevel.
    Note that this class doesn't call CalculateLevel by itself, never.
    You have to call CalculateLevel, and use it to set Level property,
    from parent scene to make this LOD work. (Reasoning behind this decision:
    parent scene has LastViewerPosition and such, and parent scene
    knows whether to initiate level_changes event sending.) }
  TVRMLShapeTreeLOD = class(TVRMLShapeTreeGroup)
  private
    FLODNode: TVRMLLODNode;
    FLODInvertedTransform: TMatrix4Single;
    FLevel: Cardinal;
    FWasLevel_ChangedSend: boolean;
  public
    property LODNode: TVRMLLODNode read FLODNode write FLODNode;
    function LODInvertedTransform: PMatrix4Single;

    { Calculate @link(Level). This only calculates level, doesn't
      assign @link(Level) property or send level_changed event. }
    function CalculateLevel(const ViewerPosition: TVector3Single): Cardinal;

    { Current level, that is index of the active child of this LOD node.
      This is always < Children.Count, unless there are no children.
      In this case it's 0.

      Should be calculated by CalculateLevel. By default
      we simply use the first (highest-detail) LOD as active.
      So if you never assign this (e.g. because TVRMLScene.IsLastViewer
      = @false, that is user position is never known) we'll always
      use the highest-detail children. }
    property Level: Cardinal read FLevel write FLevel default 0;

    property WasLevel_ChangedSend: boolean
      read FWasLevel_ChangedSend write FWasLevel_ChangedSend default false;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
  end;

  TVRMLShapesList = class;

  { Iterates over all TVRMLShape items that would be enumerated by
    Tree.Traverse. Sometimes it's easier to write code using this iterator
    than to create callbacks and use TVRMLShapeTree.Traverse. }
  TVRMLShapeTreeIterator = class
  private
    List: TVRMLShapesList;
    CurrentIndex: Integer;
    FCurrent: TVRMLShape;
  public
    constructor Create(Tree: TVRMLShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: TVRMLShape read FCurrent;
  end;

  TObjectsListItem_1 = TVRMLShape;
  {$I objectslist_1.inc}
  TVRMLShapesList = class(TObjectsList_1)
  private
    AddedCount: Integer;
    procedure AddToList(Shape: TVRMLShape);
    procedure AddToListIfVisible(Shape: TVRMLShape);
    procedure AddToListIfCollidable(Shape: TVRMLShape);
    procedure AddToListIfVisibleAndCollidable(Shape: TVRMLShape);
  public
    constructor Create;

    { Constructor that initializes list contents by traversing given tree. }
    constructor Create(Tree: TVRMLShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);
  end;

{$undef read_interface}

implementation

uses ProgressUnit, VRMLTriangle, VRMLScene, VRMLErrors;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I macprecalcvaluereturn.inc}

{ TVRMLShapeTree ------------------------------------------------------------ }

constructor TVRMLShapeTree.Create(AParentScene: TObject);
begin
  inherited Create;
  FParentScene := AParentScene;
end;

function TVRMLShapeTree.FindGeometryNodeName(
  const GeometryNodeName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.GeometryNode.NodeName = GeometryNodeName then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

function TVRMLShapeTree.FindShapeWithParentNamed(
  const ParentNodeName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.GeometryNode.TryFindParentByName(ParentNodeName) <> nil then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

function TVRMLShapeTree.FindBlenderMesh(
  const BlenderMeshName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;

      if { detect Blender meshes generated by VRML 1 exporter }
         ( (Result.GeometryNode is TVRMLGeometryNode_1) and
           (Result.GeometryNode.TryFindDirectParentByName(BlenderMeshName) <> nil) ) or
         { detect Blender meshes generated by VRML 2 (aka 97) exporter }
         ( (Result.State.ParentShape <> nil) and
           (Result.State.ParentShape.TryFindDirectParentByName(
             'ME_' + BlenderMeshName) <> nil) ) then
        Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

{ TVRMLShape -------------------------------------------------------------- }

constructor TVRMLShape.Create(AParentScene: TObject;
  AGeometryNode: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
begin
  inherited Create(AParentScene);

  FTriangleOctreeLimits := DefLocalTriangleOctreeLimits;

  FGeometryNode := AGeometryNode;
  FState := AState;
end;

destructor TVRMLShape.Destroy;
begin
  FreeAndNil(FState);
  FreeAndNil(FOctreeTriangles);
  inherited;
end;

function TVRMLShape.TriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TVRMLShape.LocalBoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svLocalBBox}
{$define PRECALC_VALUE := FLocalBoundingBox}
{$define PRECALC_VALUE_CALCULATE := GeometryNode.LocalBoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShape.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svBBox}
{$define PRECALC_VALUE := FBoundingBox}
{$define PRECALC_VALUE_CALCULATE := GeometryNode.BoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShape.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := GeometryNode.VerticesCount(State,OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := svVerticesCountNotOver}
  {$define PRECALC_VALUE := FVerticesCountNotOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := svVerticesCountOver}
  {$define PRECALC_VALUE := FVerticesCountOver}
  PRECALC_VALUE_RETURN
 end;
end;

function TVRMLShape.TrianglesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := GeometryNode.TrianglesCount(State,OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := svTrianglesCountNotOver}
  {$define PRECALC_VALUE := FTrianglesCountNotOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := svTrianglesCountOver}
  {$define PRECALC_VALUE := FTrianglesCountOver}
  PRECALC_VALUE_RETURN
 end;
end;

function TVRMLShape.EnableDisplayList: boolean;
{$define PRECALC_VALUE_ENUM := svEnableDisplayList}
{$define PRECALC_VALUE := FEnableDisplayList}
{$define PRECALC_VALUE_CALCULATE :=
  (not (State.Texture is TNodeMovieTexture))}
PRECALC_VALUE_RETURN

procedure TVRMLShape.Changed;
begin
 Validities := [];
end;

procedure TVRMLShape.ValidateBoundingSphere;
begin
 if not (svBoundingSphere in Validities) then
 begin
  BoundingSphereFromBox3d(BoundingBox, FBoundingSphereCenter,
    FBoundingSphereRadiusSqr);
  Include(Validities, svBoundingSphere);
 end;
end;

function TVRMLShape.BoundingSphereCenter: TVector3Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereCenter;
end;

function TVRMLShape.BoundingSphereRadiusSqr: Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereRadiusSqr;
end;

function TVRMLShape.FrustumBoundingSphereCollisionPossible(
  const Frustum: TFrustum): TFrustumCollisionPossible;
begin
 ValidateBoundingSphere;
 Result := Frustum.SphereCollisionPossible(
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TVRMLShape.FrustumBoundingSphereCollisionPossibleSimple(
  const Frustum: TFrustum): boolean;
begin
 ValidateBoundingSphere;
 Result := Frustum.SphereCollisionPossibleSimple(
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

procedure TVRMLShape.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, GeometryNode, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

function TVRMLShape.CreateTriangleOctree(
  const ALimits: TOctreeLimits;
  const ProgressTitle: string): TVRMLTriangleOctree;
begin
  Result := TVRMLTriangleOctree.Create(ALimits, LocalBoundingBox);
  try
    Result.Triangles.AllowedCapacityOverflow := TrianglesCount(false);
    try
      if (ProgressTitle <> '') and
         (Progress.UserInterface <> nil) and
         (not Progress.Active) then
      begin
        Progress.Init(TrianglesCount(false), ProgressTitle, true);
        try
          TriangleOctreeToAdd := Result;
          GeometryNode.LocalTriangulate(State, false,  @AddTriangleToOctreeProgress);
        finally Progress.Fini end;
      end else
        GeometryNode.LocalTriangulate(State, false,  @Result.AddItemTriangle);
    finally
      Result.Triangles.AllowedCapacityOverflow := 4;
    end;
  except Result.Free; raise end;
end;

procedure TVRMLShape.SetSpatial(const Value: TVRMLShapeSpatialStructures);
var
  Old, New: boolean;
begin
  if Value <> Spatial then
  begin
    { Handle OctreeTriangles }

    Old := ssTriangles in Spatial;
    New := ssTriangles in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeTriangles);
    end else
    if New and not Old then
    begin
      FOctreeTriangles := CreateTriangleOctree(
        FTriangleOctreeLimits,
        TriangleOctreeProgressTitle);
    end;

    FSpatial := Value;
  end;
end;

procedure TVRMLShape.LocalGeometryChanged;
begin
  { Remember to do FreeAndNil on octrees below.
    Although we will recreate octrees right after rebuilding,
    it's still good to nil them right after freeing.
    Otherwise, when exception will raise from CreateXxxOctree,
    Scene.OctreeXxx will be left as invalid pointer. }

  if OctreeTriangles <> nil then
  begin
    { TODO: make sure PointingDeviceClear; is called by parent TVRMLScene }

    FreeAndNil(FOctreeTriangles);
    FOctreeTriangles := CreateTriangleOctree(
      FTriangleOctreeLimits,
      TriangleOctreeProgressTitle);
  end;
end;

function TVRMLShape.Transparent: boolean;
var
  M: TNodeMaterial_2;
begin
  { For VRML 1.0, there may be multiple materials on a node.
    Some of them may be transparent, some not --- we arbitrarily
    decide for now that AllMaterialsTransparent decides whether
    blending should be used or not. We may change this in th
    future to AnyMaterialsTransparent, since this will be more
    consistent with X3D ColorRGBA treatment?

    We do not try to split node into multiple instances.
    This is difficult and memory-consuming task, so we just
    depend on VRML author to split his geometry nodes if he
    wants it.

    Obviously, we also drop the idea of splitting the geometry
    into separate triangles and deciding whether to use blending
    for each separate triangle. Or to sort every separate triangle.
    This would obviously get very very slow for models with lots
    of triangles.
  }

  if State.ParentShape <> nil then
  begin
    M := State.ParentShape.Material;
    Result := (M <> nil) and (M.FdTransparency.Value > SingleEqualityEpsilon);
  end else
    Result := State.LastNodes.Material.AllMaterialsTransparent;
end;

procedure TVRMLShape.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
begin
  Func(Self);
end;

function TVRMLShape.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  if ((not OnlyVisible) or Visible) and
     ((not OnlyCollidable) or Collidable) then
    Result := 1 else
    Result := 0;
end;

function TVRMLShape.Visible: boolean;
begin
  Result := State.InsideInvisible = 0;
end;

function TVRMLShape.Collidable: boolean;
begin
  Result := State.InsideIgnoreCollision = 0;
end;

{ TVRMLShapeTreeGroup -------------------------------------------------------- }

constructor TVRMLShapeTreeGroup.Create(AParentScene: TObject);
begin
  inherited Create(AParentScene);
  FChildren := TVRMLShapeTreesList.Create;
end;

destructor TVRMLShapeTreeGroup.Destroy;
begin
  FreeWithContentsAndNil(FChildren);
  inherited;
end;

procedure TVRMLShapeTreeGroup.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].Traverse(Func, OnlyActive);
end;

function TVRMLShapeTreeGroup.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do
    Result += FChildren.Items[I].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);
end;

{ TVRMLShapeTreeSwitch ------------------------------------------------------- }

procedure TVRMLShapeTreeSwitch.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Children.Items[WhichChoice].Traverse(Func, OnlyActive);
  end else
    inherited;
end;

function TVRMLShapeTreeSwitch.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Result := Children.Items[WhichChoice].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable) else
      Result := 0;
  end else
    Result := inherited;
end;

{ TVRMLShapeTreeLOD ------------------------------------------------------- }

function TVRMLShapeTreeLOD.LODInvertedTransform: PMatrix4Single;
begin
  Result := @FLODInvertedTransform;
end;

function TVRMLShapeTreeLOD.CalculateLevel(const ViewerPosition: TVector3Single): Cardinal;
var
  Viewer: TVector3Single;
  Dummy: Single;
begin
  if (Children.Count = 0) or
     (LODNode.FdRange.Count = 0) then
    Result := 0 else
  begin
    try
      Viewer := MatrixMultPoint(LODInvertedTransform^, ViewerPosition);
      Result := KeyRange(LODNode.FdRange.Items,
        PointsDistance(Viewer, LODNode.FdCenter.Value), Dummy);
      { Now we know Result is between 0..LODNode.FdRange.Count.
        Following X3D spec "Specifying too few levels will result in
        the last level being used repeatedly for the lowest levels of detail",
        so just clamp to last children. }
      MinTo1st(Result, Children.Count - 1);
    except
      on E: ETransformedResultInvalid do
      begin
        VRMLNonFatalError(Format('Cannot transform viewer position %s to LOD node local coordinate space, transformation results in direction (not point): %s',
          [ VectorToRawStr(ViewerPosition), E.Message ]));
        Result := 0;
      end;
    end;
  end;

  Assert(
    ( (Children.Count = 0) and (Result = 0) ) or
    ( (Children.Count > 0) and (Result < Cardinal(Children.Count)) ) );
end;

procedure TVRMLShapeTreeLOD.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Children.Items[Level].Traverse(Func, OnlyActive) else
      inherited;
  end;
end;

function TVRMLShapeTreeLOD.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Result := Children.Items[Level].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable) else
      Result := inherited;
  end else
    Result := 0;
end;

{ TVRMLShapeTreeIterator ----------------------------------------------------- }

{ Current implementation is very simple: just call Tree.Traverse,
  collecting shapes to a list in constructor. Then simply iterate
  over this list.

  I *did* implement once more sophisticated TVRMLShapeTreeIterator version,
  that was traversing one step further in each GetNext.
  It was building a simple stack of items to make efficient push/pop while
  walking down/up the tree of TVRMLShapesTree.

  See SVN (on sourceforge) revision 3839 (local kambi disk:
  ~/sources/vrmlengine/private/old/vrmlshape_test.pas).

  Theoretically, the sophisticated version was supposed to be much better,
  as memory use was much smaller (only the depth of the shapes tree,
  while currently it's the number of all leaves).
  And speed was supposed to be better: O(1) constructor
  and GetNext all the time (while currently constructor is O(leaves count)).

  In practice however, it turned out that this sophisticated version
  was useless. Time measures shown that current "naive" and simple
  version is even very very slightly faster in some cases.
  Time measure is in kambi_vrml_game_engine/tests/testvrmlscene.pas,
  define ITERATOR_SPEED_TEST and test for yourself.

  So in practice good memory allocator in FPC
  (as this is the bottleneck of the naive version, since List is potentially
  resized on adding each new shape) outperforms the sophisticated algorithm.

  So right now we're back to simple version. Maybe the "sophisticated"
  implementation will be restored some day... }

constructor TVRMLShapeTreeIterator.Create(Tree: TVRMLShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  inherited Create;
  List := TVRMLShapesList.Create(Tree, OnlyActive, OnlyVisible, OnlyCollidable);
  CurrentIndex := -1;
end;

destructor TVRMLShapeTreeIterator.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

function TVRMLShapeTreeIterator.GetNext: boolean;
begin
  Inc(CurrentIndex);
  Result := CurrentIndex < List.Count;
  if Result then
    FCurrent := List.Items[CurrentIndex];
end;

{ TVRMLShapesList ------------------------------------------------------- }

constructor TVRMLShapesList.Create;
begin
  inherited;
end;

constructor TVRMLShapesList.Create(Tree: TVRMLShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  Create;

  { We know exactly how many shapes are present. So set Count once,
    calculating by ShapesCount. This will be faster than resizing
    in each AddToList. (Confirmed e.g. by profiling change_vrml_by_code_2). }
  AddedCount := 0;
  Count := Tree.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);

  if OnlyVisible and OnlyCollidable then
    Tree.Traverse(@AddToListIfVisibleAndCollidable, OnlyActive) else
  if OnlyVisible then
    Tree.Traverse(@AddToListIfVisible, OnlyActive) else
  if OnlyCollidable then
    Tree.Traverse(@AddToListIfCollidable, OnlyActive) else
    Tree.Traverse(@AddToList, OnlyActive);

  Assert(AddedCount = Count);
end;

procedure TVRMLShapesList.AddToList(Shape: TVRMLShape);
begin
  Items[AddedCount] := Shape;
  Inc(AddedCount);
end;

procedure TVRMLShapesList.AddToListIfVisible(Shape: TVRMLShape);
begin
  if Shape.Visible then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

procedure TVRMLShapesList.AddToListIfCollidable(Shape: TVRMLShape);
begin
  if Shape.Collidable then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

procedure TVRMLShapesList.AddToListIfVisibleAndCollidable(Shape: TVRMLShape);
begin
  if Shape.Visible and Shape.Collidable then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

end.
