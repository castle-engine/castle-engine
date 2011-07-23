{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inventor nodes that didn't make it into any VRML version. } { }

{$ifdef read_interface}
  { IndexedTriangleMesh (from Inventor 1.0). }
  TNodeIndexedTriangleMesh_1 = class(TNodeIndexed_Faces_Or_Triangles_1)
    class function ClassNodeTypeName: string; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TexCoord(State: TVRMLGraphTraverseState;
      out ATexCoord: TVRMLNode): boolean; override;
  end;

  { RotationXYZ node (from Inventor). It's not in VRML 1.0 spec.
    But some invalid VRML 1.0 models use it,
    e.g. [http://www-vrl.umich.edu/sel_prj/EECS498/]. }
  TNodeRotationXYZ = class(TVRMLTransformationNode)
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdAxis: TSFEnum;
    public property FdAxis: TSFEnum read FFdAxis;

    private FFdAngle: TSFFloat;
    public property FdAngle: TSFFloat read FFdAngle;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeIndexedTriangleMesh_1.ClassNodeTypeName: string;
begin
  result := 'IndexedTriangleMesh';
end;

function TNodeIndexedTriangleMesh_1.TexCoord(State: TVRMLGraphTraverseState;
  out ATexCoord: TVRMLNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

procedure TNodeRotationXYZ.CreateNode;
begin
  inherited;

  FFdAxis := TSFEnum.Create(Self, 'axis' , ['X', 'Y', 'Z'], 0);
   FdAxis.ChangesAlways := [chEverything];
  Fields.Add(FFdAxis);

  FFdAngle := TSFFloat.Create(Self, 'angle', 0);
   FdAngle.ChangesAlways := [chEverything];
  Fields.Add(FFdAngle);
end;

class function TNodeRotationXYZ.ClassNodeTypeName: string;
begin
  Result := 'RotationXYZ';
end;

procedure TNodeRotationXYZ.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
const
  AxisVectors: array[0..2]of TVector3Single =
  ( (1, 0, 0), (0, 1, 0), (0, 0, 1) );
begin
  RotationMatricesRad(FdAngle.Value, AxisVectors[FdAxis.Value],
    Matrix, InvertedMatrix);
end;

function TNodeRotationXYZ.TransformScale: Single;
begin
  Result := 1;
end;

procedure RegistedInventorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeIndexedTriangleMesh_1,
    TNodeRotationXYZ
  ]);
end;
{$endif read_implementation}
