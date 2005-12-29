{
  Copyright 2003-2004 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
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
    svBoundingSphere);

  { This class represents a pair of objects: @link(ShapeNode) and
    @link(State). It allows to perform some operations that need
    to know both things.

    This class caches results of methods [Local]BoundingBox,
    VerticesCount and TrianglesCount. This means that things work fast,
    but this also means that you must manually call
    @link(Changed)
    when you changed some properties of ShapeNode or contents of State.

    But note that you can't change ShapeNode or State to different
    objects --- they are readonly properties.

    Also note that if you're using @link(TVRMLFlatScene) class
    then you don't have to worry about calling @link(Changed)
    of items in @link(TVRMLFlatScene.ShapeStates).
    All you have to do is to call appropriate @code(Changed*)
    methods of @link(TVRMLFlatScene). }
  TVRMLShapeState = class
  private
    FLocalBoundingBox: TBox3d;
    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLShapeStateValidities;
    FShapeNode: TNodeGeneralShape;
    FState: TVRMLGraphTraverseState;
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;

    procedure ValidateBoundingSphere;
  public
    { ShapeNode to wskaznik na obiekt w RootNode }
    property ShapeNode: TNodeGeneralShape read FShapeNode;

    { State is OWNED by this TVRMLShape class - I mean, we will do State.Free
      in destructor }
    property State: TVRMLGraphTraverseState read FState;

    { specyfikacja co robia [Local]BoundingBox, VerticesCount i TrianglesCount -
      patrz VRMLNodes.TNodeGeneralShape }
    function LocalBoundingBox: TBox3d;
    function BoundingBox: TBox3d;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;

    { This calculates bounding sphere basing on BoundingBox.
      In the future this may be changed to use BoundingSphere method
      of @link(TNodeGeneralShape), when I will implement it.
      For now, BoundingSphere is always worse approximation of bounding
      volume than @link(BoundingBox) (i.e. BoundingSphere is always
      larger) but it may be useful in some cases when
      detecting collision versus bounding sphere is much faster than detecting
      them versus bounding box.

      BoundingSphereRadiusSqr = 0 and BoundingSphereCenter is undefined
      if Box is empty. }
    function BoundingSphereCenter: TVector3Single;
    function BoundingSphereRadiusSqr: Single;

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
    constructor Create(AShapeNode: TNodeGeneralShape; AState: TVRMLGraphTraverseState);
    destructor Destroy; override;
  end;

  TObjectsListItem_1 = TVRMLShapeState;
  {$I ObjectsList_1.inc}
  TVRMLShapeStatesList = class(TObjectsList_1)
    { szuka elementu ktorego ShapeNode.NodeName = ShapeNodeName.
      Zwraca jego indeks lub -1 jesli nie znalazl. }
    function IndexOfShapeNodeName(const ShapeNodeName: string): integer;
    
    { szuka elementu ktorego ShapeNode ma rodzica o nazwie ParentNodeName,
      rodzic taki jest szukany metoda ShapeNode.TryFindParentNodeByName. }
    function IndexOfShapeWithParentNamed(const ParentNodeName: string): integer;
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}
{$I macprecalcvaluereturn.inc}

{ TVRMLShapeState -------------------------------------------------------------- }

function TVRMLShapeState.LocalBoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svLocalBBox}
{$define PRECALC_VALUE := FLocalBoundingBox}
{$define PRECALC_VALUE_CALCULATE := ShapeNode.LocalBoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShapeState.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := svBBox}
{$define PRECALC_VALUE := FBoundingBox}
{$define PRECALC_VALUE_CALCULATE := ShapeNode.BoundingBox(State)}
PRECALC_VALUE_RETURN

function TVRMLShapeState.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := ShapeNode.VerticesCount(State,OverTriangulate)}
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
 {$define PRECALC_VALUE_CALCULATE := ShapeNode.TrianglesCount(State,OverTriangulate)}
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

constructor TVRMLShapeState.Create(AShapeNode: TNodeGeneralShape; AState: TVRMLGraphTraverseState);
begin
 inherited Create;
 FShapeNode := AShapeNode;
 FState := AState;
end;

destructor TVRMLShapeState.Destroy;
begin
 State.Free;
 inherited;
end;

{ TVRMLShapeStatesList ------------------------------------------------------- }

function TVRMLShapeStatesList.IndexOfShapeNodeName(const ShapeNodeName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].ShapeNode.NodeName = ShapeNodeName then exit;
 result := -1;
end;

function TVRMLShapeStatesList.IndexOfShapeWithParentNamed(const ParentNodeName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].ShapeNode.TryFindParentNodeByName(ParentNodeName)<>nil then exit;
 result := -1;
end;

end.
