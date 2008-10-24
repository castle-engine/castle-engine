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
}

{ @abstract(@link(TVRMLShapeState) class.) }

unit VRMLShapeState;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3d, VRMLNodes, KambiClassUtils,
  KambiUtils;

{$define read_interface}

type
  { Internal type for TVRMLShapeState }
  TVRMLShapeStateValidities = set of (svLocalBBox, svBBox,
    svVerticesCountNotOver,  svVerticesCountOver,
    svTrianglesCountNotOver, svTrianglesCountOver,
    svBoundingSphere, svEnableDisplayList);

  { This class represents a pair of objects: @link(GeometryNode) and
    @link(State). It allows to perform some operations that need
    to know both things.

    This class caches results of methods LocalBoundingBox, BoundingBox,
    and most others (see TVRMLShapeStateValidities for hints).
    This means that things work fast, but this also means that
    you must manually call @link(Changed)
    when you changed some properties of GeometryNode or contents of State.

    But note that you can't change GeometryNode or State to different
    objects --- they are readonly properties.

    Also note that if you're using @link(TVRMLScene) class
    then you don't have to worry about calling @link(Changed)
    of items in @link(TVRMLScene.ShapeStates).
    All you have to do is to call appropriate @code(Changed*)
    methods of @link(TVRMLScene). }
  TVRMLShapeState = class
  private
    FLocalBoundingBox: TBox3d;
    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLShapeStateValidities;
    FGeometryNode: TVRMLGeometryNode;
    FState: TVRMLGraphTraverseState;
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;
    FEnableDisplayList: boolean;

    procedure ValidateBoundingSphere;
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

      If @false then rendering of this shapestate cannot be stored
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
  end;

  TObjectsListItem_1 = TVRMLShapeState;
  {$I ObjectsList_1.inc}
  TVRMLShapeStatesList = class(TObjectsList_1)
    { szuka elementu ktorego GeometryNode.NodeName = GeometryNodeName.
      Zwraca jego indeks lub -1 jesli nie znalazl. }
    function IndexOfGeometryNodeName(const GeometryNodeName: string): integer;

    { szuka elementu ktorego GeometryNode ma rodzica o nazwie ParentNodeName,
      rodzic taki jest szukany metoda GeometryNode.TryFindParentNodeByName. }
    function IndexOfShapeWithParentNamed(const ParentNodeName: string): integer;

    { Assuming that the model was created by Blender VRML 1 or 2 exporter,
      this searches for a first shapestate that was created from Blender
      mesh named BlenderMeshName.

      It follows the logic of two Blender exporters.

      If it doesn't find matching node, returns -1. Otherwise, an index
      of matching shapestate.

      Note that IndexOfBlenderObject would be usually more sensible
      (since there can be only one shapestate from given Blender object),
      but Blender VRML 1.0 exporter doesn't export anywhere Blender object
      name. So when working with VRML 1.0, you're stuck with looking
      for mesh names. }
    function IndexOfBlenderMesh(const BlenderMeshName: string): Integer;
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}
{$I macprecalcvaluereturn.inc}

{ TVRMLShapeState -------------------------------------------------------------- }

constructor TVRMLShapeState.Create(AGeometryNode: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
begin
 inherited Create;
 FGeometryNode := AGeometryNode;
 FState := AState;
end;

destructor TVRMLShapeState.Destroy;
begin
 State.Free;
 inherited;
end;

function TVRMLShapeState.LocalBoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svLocalBBox}
{$define PRECALC_VALUE := FLocalBoundingBox}
{$define PRECALC_VALUE_CALCULATE := GeometryNode.LocalBoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShapeState.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svBBox}
{$define PRECALC_VALUE := FBoundingBox}
{$define PRECALC_VALUE_CALCULATE := GeometryNode.BoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShapeState.VerticesCount(OverTriangulate: boolean): Cardinal;
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

function TVRMLShapeState.TrianglesCount(OverTriangulate: boolean): Cardinal;
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

function TVRMLShapeState.EnableDisplayList: boolean;
{$define PRECALC_VALUE_ENUM := svEnableDisplayList}
{$define PRECALC_VALUE := FEnableDisplayList}
{$define PRECALC_VALUE_CALCULATE :=
  (not (State.Texture is TNodeMovieTexture))}
PRECALC_VALUE_RETURN

procedure TVRMLShapeState.Changed;
begin
 Validities := [];
end;

procedure TVRMLShapeState.ValidateBoundingSphere;
begin
 if not (svBoundingSphere in Validities) then
 begin
  Box3dBoundingSphere(BoundingBox, FBoundingSphereCenter,
    FBoundingSphereRadiusSqr);
  Include(Validities, svBoundingSphere);
 end;
end;

function TVRMLShapeState.BoundingSphereCenter: TVector3Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereCenter;
end;

function TVRMLShapeState.BoundingSphereRadiusSqr: Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereRadiusSqr;
end;

function TVRMLShapeState.FrustumBoundingSphereCollisionPossible(
  const Frustum: TFrustum): TFrustumCollisionPossible;
begin
 ValidateBoundingSphere;
 Result := FrustumSphereCollisionPossible(Frustum,
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TVRMLShapeState.FrustumBoundingSphereCollisionPossibleSimple(
  const Frustum: TFrustum): boolean;
begin
 ValidateBoundingSphere;
 Result := FrustumSphereCollisionPossibleSimple(Frustum,
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

{ TVRMLShapeStatesList ------------------------------------------------------- }

function TVRMLShapeStatesList.IndexOfGeometryNodeName(const GeometryNodeName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].GeometryNode.NodeName = GeometryNodeName then exit;
 result := -1;
end;

function TVRMLShapeStatesList.IndexOfShapeWithParentNamed(const ParentNodeName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].GeometryNode.TryFindParentByName(ParentNodeName)<>nil then exit;
 result := -1;
end;

function TVRMLShapeStatesList.IndexOfBlenderMesh(
  const BlenderMeshName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if { detect Blender meshes generated by VRML 1 exporter }
       ( (Items[Result].GeometryNode is TVRMLGeometryNode_1) and
         (Items[Result].GeometryNode.TryFindDirectParentByName(BlenderMeshName) <> nil) ) or
       { detect Blender meshes generated by VRML 2 (aka 97) exporter }
       ( (Items[Result].State.ParentShape <> nil) and
         (Items[Result].State.ParentShape.TryFindDirectParentByName(
           'ME_' + BlenderMeshName) <> nil) ) then
      Exit;

  Result := -1;
end;

end.
