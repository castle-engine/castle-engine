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
  KambiUtils, VRMLTriangleOctree;

{$define read_interface}

const
  DefLocalTriangleOctreeMaxDepth = 10;
  DefLocalTriangleOctreeLeafCapacity = 64;

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
  public
    procedure Traverse(Func: TShapeTraverseFunc;
      OnlyActive: boolean); virtual; abstract;

    function ShapesCount(OnlyActive: boolean): Cardinal; virtual; abstract;

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
    function CreateTriangleOctree(const AMaxDepth, ALeafCapacity: Integer;
      const ProgressTitle: string): TVRMLTriangleOctree;

    FTriangleOctreeMaxDepth: Integer;
    FTriangleOctreeLeafCapacity: Integer;
    FTriangleOctreeProgressTitle: string;

    FOctreeTriangles: TVRMLTriangleOctree;

    FSpatial: TVRMLShapeSpatialStructures;
    procedure SetSpatial(const Value: TVRMLShapeSpatialStructures);
  public
    constructor Create(AGeometryNode: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
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
      and then using @link(VectorMath.FrustumSphereCollisionPossible).

      But it may be a little faster since it avoids some small speed problems
      (like copying memory contents when you get values of
      BoundingSphereXxx properties and checking twice are
      BoundingSphereXxx calculated). }
    function FrustumBoundingSphereCollisionPossible(
      const Frustum: TFrustum): TFrustumCollisionPossible;

    { This is exactly equivalent to getting
      @link(BoundingSphereCenter) and @link(BoundingSphereRadiusSqr)
      and then using @link(VectorMath.FrustumSphereCollisionPossibleSimple).

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

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      @groupBegin }
    property     TriangleOctreeMaxDepth: Integer
      read      FTriangleOctreeMaxDepth
      write     FTriangleOctreeMaxDepth
      default DefLocalTriangleOctreeMaxDepth;

    property     TriangleOctreeLeafCapacity: Integer
       read     FTriangleOctreeLeafCapacity
      write     FTriangleOctreeLeafCapacity
      default DefLocalTriangleOctreeLeafCapacity;

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
    function ShapesCount(OnlyActive: boolean): Cardinal; override;
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
    constructor Create;
    destructor Destroy; override;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(OnlyActive: boolean): Cardinal; override;

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
    function ShapesCount(OnlyActive: boolean): Cardinal; override;
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
    constructor Create(Tree: TVRMLShapeTree; OnlyActive: boolean);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: TVRMLShape read FCurrent;
  end;

  TObjectsListItem_1 = TVRMLShape;
  {$I objectslist_1.inc}
  TVRMLShapesList = class(TObjectsList_1)
  private
    procedure AddToList(Shape: TVRMLShape);
  public
    constructor Create;

    { Constructor that initializes list contents by traversing given tree. }
    constructor Create(Tree: TVRMLShapeTree; OnlyActive: boolean);
  end;

{$undef read_interface}

implementation

uses ProgressUnit, VRMLTriangle;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I macprecalcvaluereturn.inc}

{ TVRMLShapeTree ------------------------------------------------------------ }

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

constructor TVRMLShape.Create(AGeometryNode: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
begin
  inherited Create;

  FTriangleOctreeMaxDepth := DefLocalTriangleOctreeMaxDepth;
  FTriangleOctreeLeafCapacity := DefLocalTriangleOctreeLeafCapacity;

  FGeometryNode := AGeometryNode;
  FState := AState;
end;

destructor TVRMLShape.Destroy;
begin
  FreeAndNil(State);
  FreeAndNil(FOctreeTriangles);
  inherited;
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
 Result := FrustumSphereCollisionPossible(Frustum,
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TVRMLShape.FrustumBoundingSphereCollisionPossibleSimple(
  const Frustum: TFrustum): boolean;
begin
 ValidateBoundingSphere;
 Result := FrustumSphereCollisionPossibleSimple(Frustum,
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
  const AMaxDepth, ALeafCapacity: integer;
  const ProgressTitle: string): TVRMLTriangleOctree;
begin
  Result := TVRMLTriangleOctree.Create(AMaxDepth, ALeafCapacity, LocalBoundingBox);
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
        TriangleOctreeMaxDepth,
        TriangleOctreeLeafCapacity,
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
      TriangleOctreeMaxDepth,
      TriangleOctreeLeafCapacity,
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

function TVRMLShape.ShapesCount(OnlyActive: boolean): Cardinal;
begin
  Result := 1;
end;

{ TVRMLShapeTreeGroup -------------------------------------------------------- }

constructor TVRMLShapeTreeGroup.Create;
begin
  inherited;
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

function TVRMLShapeTreeGroup.ShapesCount(OnlyActive: boolean): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do
    Result += FChildren.Items[I].ShapesCount(OnlyActive);
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

function TVRMLShapeTreeSwitch.ShapesCount(OnlyActive: boolean): Cardinal;
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Result := Children.Items[WhichChoice].ShapesCount(OnlyActive) else
      Result := 0;
  end else
    Result := inherited;
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

constructor TVRMLShapeTreeIterator.Create(Tree: TVRMLShapeTree; OnlyActive: boolean);
begin
  inherited Create;
  List := TVRMLShapesList.Create(Tree, OnlyActive);
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

constructor TVRMLShapesList.Create(Tree: TVRMLShapeTree; OnlyActive: boolean);
begin
  Create;
  Tree.Traverse(@AddToList, OnlyActive);
end;

procedure TVRMLShapesList.AddToList(Shape: TVRMLShape);
begin
  Add(Shape);
end;

end.
