{
  Copyright 2008 Michalis Kamburelis.

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

{ Nodes that are defined by Avalon extensions, see
  [http://instant-reality.com/] and in particular the specifications on
  [http://instant-reality.com/documentation/nodetype/]. }

{$ifdef read_interface}
  { MatrixTransform node for VRML >= 2.0 from Avalon extension,
    see [http://instant-reality.com/documentation/nodetype/MatrixTransform/]. }
  TNodeMatrixTransform_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    private FFdmatrix: TSFMatrix4f;
    public property Fdmatrix: TSFMatrix4f read FFdmatrix;
  end;

  TNodeTeapot = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    function Proxy: TVRMLGeometryNode; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeMatrixTransform_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdmatrix := TSFMatrix4f.Create('matrix', IdentityMatrix4Single);
  Fields.Add(FFdmatrix);

  DefaultContainerField := 'children';
end;

class function TNodeMatrixTransform_2.ClassNodeTypeName: string;
begin
  Result := 'MatrixTransform';
end;

class function TNodeMatrixTransform_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeMatrixTransform_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeMatrixTransform_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

procedure TNodeMatrixTransform_2.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;

  { Inherited TVRMLGroupingNode already saved State.Transform and such. }

  State.Transform := MultMatrices(State.Transform, FdMatrix.Value);

  { We simply are not able to invert matrices for now,
    so InvertedMatrix = just IdentityMatrix4Single.
  State.InvertedTransform :=
    MultMatrices(IdentityMatrix4Single, State.InvertedTransform);
  }

  State.AverageScaleTransform *= FdMatrix.AverageScaleTransform;
end;

function TNodeMatrixTransform_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeTeapot.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdsize := TSFVec3f.Create('size', Vector3Single(3, 3, 3));
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeTeapot.ClassNodeTypeName: string;
begin
  Result := 'Teapot';
end;

function TNodeTeapot.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTeapot.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeTeapot.Proxy: TVRMLGeometryNode;
const
  TeapotCoord {$I teapot_data/teapot_coord.inc};
  TeapotCoordIndex {$I teapot_data/teapot_coord_index.inc};
var
  CoordNode: TNodeCoordinate;
  IFS: TNodeIndexedFaceSet_2 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
  try
    CoordNode := TNodeCoordinate.Create('', WWWBasePath);
    IFS.FdCoord.Value := CoordNode;
    CoordNode.FdPoint.Items.AssignArray(TeapotCoord);

    { Scale Coords according to "size" field.
      Assumes that original coords are good for default size (3, 3, 3). }
    if not FdSize.EqualsDefaultValue then
    begin
      CoordNode.FdPoint.Items.MultiplyComponents(
        VectorScale(FdSize.Value, 1/3));
    end;

    IFS.FdCoordIndex.Items.AssignArray(TeapotCoordIndex);

    IFS.FdSolid.Value := FdSolid.Value;

    { Set CreaseAngle to anything larger than Pi, to make this completely
      smooth later using fast CreateSmoothNormalsCoordinateNode routine. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

procedure RegisterAvalonNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeMatrixTransform_2,
    TNodeTeapot
  ]);
end;
{$endif read_implementation}
