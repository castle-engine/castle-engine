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

{ Inventor nodes that didn't make it into any VRML version. } { }

{$ifdef read_interface}
  { IndexedTriangleMesh (from Inventor 1.0). }
  TIndexedTriangleMeshNode_1 = class(TIndexedFacesOrTrianglesNode_1)
    class function ClassX3DType: string; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
  end;

  { RotationXYZ node (from Inventor). It's not in VRML 1.0 spec.
    But some invalid VRML 1.0 models use it,
    e.g. [http://www-vrl.umich.edu/sel_prj/EECS498/]. }
  TRotationXYZNode = class(TAbstractTransformationNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdAxis: TSFEnum;
    public property FdAxis: TSFEnum read FFdAxis;

    private FFdAngle: TSFFloat;
    public property FdAngle: TSFFloat read FFdAngle;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TIndexedTriangleMeshNode_1.ClassX3DType: string;
begin
  result := 'IndexedTriangleMesh';
end;

function TIndexedTriangleMeshNode_1.TexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

procedure TRotationXYZNode.CreateNode;
begin
  inherited;

  FFdAxis := TSFEnum.Create(Self, 'axis' , ['X', 'Y', 'Z'], 0);
   FdAxis.ChangesAlways := [chEverything];
  AddField(FFdAxis);

  FFdAngle := TSFFloat.Create(Self, 'angle', 0);
   FdAngle.ChangesAlways := [chEverything];
  AddField(FFdAngle);
end;

class function TRotationXYZNode.ClassX3DType: string;
begin
  Result := 'RotationXYZ';
end;

procedure TRotationXYZNode.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
const
  AxisVectors: array[0..2]of TVector3Single =
  ( (1, 0, 0), (0, 1, 0), (0, 0, 1) );
begin
  RotationMatricesRad(FdAngle.Value, AxisVectors[FdAxis.Value],
    Matrix, InvertedMatrix);
end;

function TRotationXYZNode.TransformScale: Single;
begin
  Result := 1;
end;

procedure RegistedInventorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TIndexedTriangleMeshNode_1,
    TRotationXYZNode
  ]);
end;
{$endif read_implementation}
