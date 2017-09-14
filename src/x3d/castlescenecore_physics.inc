{
  Copyright 2017-2017 Michalis Kamburelis.

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
    Scene: TCastleSceneCore;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ utilities ------------------------------------------------------------------ }

// TODO: dirty copy-paste from castle3d_physics.inc,
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
  SI: TShapeTreeIterator;
begin
  if Scene = nil then
    raise Exception.Create('You must assign a TMeshCollider.Scene to determine the mesh triangles');

  Mesh := TKraftMesh.Create(APhysics);
  SI := TShapeTreeIterator.Create(Scene.Shapes, true);
  try
    while SI.GetNext do
      if SI.Current.Collidable then
        SI.Current.Triangulate(false, @AddTriangle);
  finally FreeAndNil(SI) end;
  Mesh.Finish;

  Result := TKraftShapeMesh.Create(APhysics, ARigidBody, Mesh);
end;

{$endif read_implementation}
