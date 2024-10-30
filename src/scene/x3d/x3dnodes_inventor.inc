{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inventor nodes that didn't make it into any VRML version. } { }

{$ifdef read_interface}
  { IndexedTriangleMesh (from Inventor 1.0). }
  TIndexedTriangleMeshNode_1 = class(TIndexedFacesOrTrianglesNode_1)
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_indexedtrianglemesh_1.inc}
  end;

  { RotationXYZ node (from Inventor). It's not in VRML 1.0 spec.
    But some invalid VRML 1.0 models use it,
    e.g. [http://www-vrl.umich.edu/sel_prj/EECS498/]. }
  TRotationXYZNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_rotationxyz_1.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}
function TIndexedTriangleMeshNode_1.InternalTexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.VRML1State.TextureCoordinate2;
end;

const
  AxisNames: array [0..2] of String = ('X', 'Y', 'Z');

procedure TRotationXYZNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
var
  RotationAxisAngle: TVector4;
begin
  inherited;
  RotationAxisAngle := Vector4(TVector3.One[FdAxis.Value], FdAngle.Value);
  StateStack.Top.Transformation.Multiply(RotationAxisAngle, Vector3(1, 1, 1), TVector3.Zero);
end;

procedure RegistedInventorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TIndexedTriangleMeshNode_1,
    TRotationXYZNode_1
  ]);
end;
{$endif read_implementation}
