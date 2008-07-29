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

procedure RegisterAvalonNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeMatrixTransform_2
  ]);
end;
{$endif read_implementation}
