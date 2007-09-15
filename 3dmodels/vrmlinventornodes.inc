{
  Copyright 2002-2007 Michalis Kamburelis.

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

{ Inventor nodes that didn't make it into VRML version. } { }

{$ifdef read_interface}
  { IndexedTriangleMesh --- from Inventor 1.0. }
  TNodeIndexedTriangleMesh_1 = class(TNodeIndexed_Faces_Or_Triangles_1)
    class function ClassNodeTypeName: string; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  { RotationXYZ node from Inventor. It's not in VRML 1.0 spec.
    But some invalid VRML 1.0 models use it,
    e.g. [http://www-vrl.umich.edu/sel_prj/EECS498/]. }
  TNodeRotationXYZ = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdAxis: TSFEnum index 0 read GetFieldAsSFEnum;
    property FdAngle: TSFFloat index 1 read GetFieldAsSFFloat;

    function MatrixTransformation: TMatrix4Single; override;
    function AverageScaleTransform: Single; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeIndexedTriangleMesh_1.ClassNodeTypeName: string;
begin
 result := 'IndexedTriangleMesh';
end;

constructor TNodeRotationXYZ.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFEnum.Create('axis' , ['X', 'Y', 'Z'], 0));
 Fields.Add(TSFFloat.Create('angle', 0));
end;

class function TNodeRotationXYZ.ClassNodeTypeName: string;
begin
 Result := 'RotationXYZ';
end;

function TNodeRotationXYZ.MatrixTransformation: TMatrix4Single;
const
  AxisVectors: array[0..2]of TVector3Single =
  ( (1, 0, 0), (0, 1, 0), (0, 0, 1) );
begin
 Result := RotationMatrixRad(FdAngle.Value, AxisVectors[FdAxis.Value]);
end;

function TNodeRotationXYZ.AverageScaleTransform: Single;
begin
  Result := 1;
end;
{$endif read_implementation}
