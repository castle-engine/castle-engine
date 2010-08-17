{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2010-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestVRMLNodesOptimizedProxy;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, VectorMath, VRMLNodes;

type
  TTestVRMLNodesOptimizedProxy = class(TTestCase)
  published
    { Some of the geometry nodes, even though they have Proxy,
      have also optimized versions for some operations (like calculating
      bboxes).

      Test that they are actually used (not falling back on
      proxy), by giving them a nasty proxy that will bomb when you try
      to use it's methods. }
    procedure TestGeometryUsesOptimizedMethods;
  end;

implementation

uses Boxes3D, VRMLShape;

{ TNastyProxy ---------------------------------------------------------------- }

type
  ENastyProxy = class(Exception);

  TNastyProxy = class(TVRMLGeometryNode)
    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode;
      ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode;
      ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode;
      ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode;
      ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    procedure Triangulate(Shape: TObject; State: TVRMLGraphTraverseState;
      OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
    procedure LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
      OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState); override;
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

function TNastyProxy.BoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode;
  ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  raise ENastyProxy.Create('Something tried to use unoptimized BoundingBox');
  Result := EmptyBox3D; { silence compiler warnings }
end;

function TNastyProxy.LocalBoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode;
  ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  raise ENastyProxy.Create('Something tried to use unoptimized LocalBoundingBox');
  Result := EmptyBox3D; { silence compiler warnings }
end;

function TNastyProxy.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TVRMLGeometryNode;
  ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  raise ENastyProxy.Create('Something tried to use unoptimized VerticesCount');
  Result := 0; { silence compiler warnings }
end;

function TNastyProxy.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TVRMLGeometryNode;
  ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  raise ENastyProxy.Create('Something tried to use unoptimized TrianglesCount');
  Result := 0; { silence compiler warnings }
end;

procedure TNastyProxy.Triangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  raise ENastyProxy.Create('Something tried to use unoptimized Triangulate');
end;

procedure TNastyProxy.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  raise ENastyProxy.Create('Something tried to use unoptimized LocalTriangulate');
end;

function TNastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  raise ENastyProxy.Create('Something tried to use Proxy on TNastyProxy');
  Result := nil; { silence compiler warnings }
end;

{ Geometry nodes returning TNastyProxy --------------------------------------- }

type
  TNodeCone_1_NastyProxy = class(TNodeCone_1)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeCone_2_NastyProxy = class(TNodeCone_2)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeCylinder_1_NastyProxy = class(TNodeCylinder_1)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeCylinder_2_NastyProxy = class(TNodeCylinder_2)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeBox_NastyProxy = class(TNodeBox)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeCube_1_NastyProxy = class(TNodeCube_1)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeSphere_1_NastyProxy = class(TNodeSphere_1)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

  TNodeSphere_2_NastyProxy = class(TNodeSphere_2)
    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
  end;

function TNodeCone_1_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeCone_2_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeCylinder_1_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeCylinder_2_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeBox_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeCube_1_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeSphere_1_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

function TNodeSphere_2_NastyProxy.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := TNastyProxy.Create(NodeName, WWWBasePath);
end;

{ TTestVRMLNodesOptimizedProxy ----------------------------------------------- }

procedure TTestVRMLNodesOptimizedProxy.TestGeometryUsesOptimizedMethods;
var
  G: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  Shape: TVRMLShape;

  procedure FinalizeNode;
  begin
    FreeAndNil(Shape);
    FreeAndNil(G);
  end;

  procedure InitializeNode(NodeClass: TVRMLNodeClass);
  begin
    FinalizeNode;
    G := (NodeClass.Create('', '')) as TVRMLGeometryNode;
    Shape := TVRMLShape.Create(nil, G, TVRMLGraphTraverseState.CreateCopy(State), nil);
  end;

begin
  G := nil;
  Shape := nil;
  State := TVRMLGraphTraverseState.Create;
  try

    { create each node, and try to call the methods that should
      be optimized (should *not* call the proxy). }
    InitializeNode(TNodeCone_1_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeCone_2_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeCylinder_1_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeCylinder_2_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeBox_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeCube_1_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeSphere_1_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    InitializeNode(TNodeSphere_2_NastyProxy);
    Shape.BoundingBox;
    Shape.LocalBoundingBox;
    Shape.TrianglesCount(false);
    Shape.TrianglesCount(true);

    FinalizeNode;
  finally FreeAndNil(State) end;
end;

initialization
 RegisterTest(TTestVRMLNodesOptimizedProxy);
end.
