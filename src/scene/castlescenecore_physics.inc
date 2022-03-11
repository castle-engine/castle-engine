{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Physics integration code between CastleSceneCore and Kraft physics engine. }

{$ifdef read_interface}

  { Collide as a mesh, constructed from the given scene shapes.
    Place this inside @link(TRigidBody.Collider) property. }
  TMeshCollider = class(TCollider)
  strict private
    Mesh: TKraftMesh;
    procedure AddTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    { Mesh which is used to collide.
      It does not have to be convex.

      The @link(TCastleTransform.Collides Scene.Collides) and
      @link(TCastleTransform.CheckCollides Scene.CheckCollides) do not matter here.
      Using the TCollisionNode inside Scene matters (so the scene may collide
      as something different than it's visible, see
      https://castle-engine.io/x3d_implementation_navigation.php ). }
    Scene: TCastleSceneCore;
  end;

(*
  TODO:
  For now TConvexHullCollider is commented out, as:
  - I'm unsure does this work correctly (in "Wyrd Forest" I had problems with it).
  - Also, maybe instead of a new class, we should expose "Convex: boolean"
    on TMeshCollider.

  { Collide as a convex hull constructed from the given scene shape.
    Place this inside @link(TRigidBody.Collider) property. }
  TConvexHullCollider = class(TCollider)
  strict private
    ConvexHull: TKraftConvexHull;
    procedure AddTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    { Mesh which is used to collide.
      It does not have to be convex.

      The @link(TCastleTransform.Collides Scene.Collides) and
      @link(TCastleTransform.CheckCollides Scene.CheckCollides) do not matter here.
      Using the TCollisionNode inside Scene matters (so the scene may collide
      as something different than it's visible, see
      https://castle-engine.io/x3d_implementation_navigation.php ). }
    Scene: TCastleSceneCore;
  end;
*)

{$endif read_interface}

{$ifdef read_implementation}

{ utilities ------------------------------------------------------------------ }

// TODO: dirty copy-paste from castletransform_physics.inc,
// we should add CastleInternalKraft unit at some point.
function VectorToKraft(const V: TVector3): TKraftVector3;
begin
  Move(V, Result, SizeOf(V));
end;

{ TMeshCollider ------------------------------------------------------------- }

procedure TMeshCollider.AddTriangle(Shape: TObject;
  const Position: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  { TODO: it's probably not optimal to call AddTriangle
    for each triangle, we should instead call Load with a list. }

  Mesh.AddTriangle(
    Mesh.AddVertex(VectorToKraft(Position.Data[0])),
    Mesh.AddVertex(VectorToKraft(Position.Data[1])),
    Mesh.AddVertex(VectorToKraft(Position.Data[2])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[0])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[1])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[2]))
  );
end;

function TMeshCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
var
  ShapesList: TShapeList;
  I: Integer;
begin
  if Scene = nil then
    raise Exception.Create('You must assign a TMeshCollider.Scene to determine the mesh triangles');

  Mesh := TKraftMesh.Create(APhysics);

  ShapesList := Scene.Shapes.TraverseList(true);
  for I := 0 to ShapesList.Count - 1 do
    if ShapesList[I].Collidable then
      ShapesList[I].Triangulate(false, {$ifdef FPC}@{$endif}AddTriangle);

  if Mesh.CountVertices = 0 then
    raise Exception.Create('The scene assigned to TMeshCollider.Scene is empty (no collidable triangles)');
  Mesh.Finish;

  Result := TKraftShapeMesh.Create(APhysics, ARigidBody, Mesh);
end;

{ TConvexHullCollider ------------------------------------------------------------- }

(*
procedure TConvexHullCollider.AddTriangle(Shape: TObject;
  const Position: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  { TODO: it's probably not optimal to call AddVertex
    for each triangle, we should instead call Load with a list.

    Also, we coud use InternalCoordinates if available (for geometry
    possibly generated by proxy) to not duplicate coordinates needlessly
    when geometry is indexed.
  }
  ConvexHull.AddVertex(VectorToKraft(Position.Data[0]));
  ConvexHull.AddVertex(VectorToKraft(Position.Data[1]));
  ConvexHull.AddVertex(VectorToKraft(Position.Data[2]));
end;

function TConvexHullCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
var
  SI: TShapeTreeIterator;
begin
  if Scene = nil then
    raise Exception.Create('You must assign a TConvexHullCollider.Scene to determine the convex full shape');

  ConvexHull := TKraftConvexHull.Create(APhysics);
  SI := TShapeTreeIterator.Create(Scene.Shapes, true);
  try
    while SI.GetNext do
      if SI.Current.Collidable then
        SI.Current.Triangulate(false, @AddTriangle);
  finally FreeAndNil(SI) end;
  if ConvexHull.CountVertices = 0 then
    raise Exception.Create('The scene assigned to TConvexHullCollider.Scene is empty (no collidable triangles)');
  ConvexHull.Build;
  ConvexHull.Finish;

  Result := TKraftShapeConvexHull.Create(APhysics, ARigidBody, ConvexHull);
end;
*)

{$endif read_implementation}
