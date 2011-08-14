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
  { Suffix O means OverTriangulate,
           NO means not OverTriangulate }
  NastyGeometry, GoodGeometry, ProxyGeometryO, ProxyGeometryNO: TVRMLGeometryNode;
  State, ProxyStateO, ProxyStateNO: TVRMLGraphTraverseState;
  NastyShape, ProxyShapeO, ProxyShapeNO: TVRMLShape;

  procedure FinalizeNode;
  begin
    FreeAndNil(ProxyShapeO);
    FreeAndNil(ProxyGeometryO);

    if ProxyStateO <> State then
      FreeAndNil(ProxyStateO) else
      ProxyStateO := nil;

    FreeAndNil(ProxyShapeNO);
    FreeAndNil(ProxyGeometryNO);

    if ProxyStateNO <> State then
      FreeAndNil(ProxyStateNO) else
      ProxyStateNO := nil;

    FreeAndNil(GoodGeometry);
    FreeAndNil(NastyShape);
    FreeAndNil(NastyGeometry);
  end;

  procedure InitializeNode(NodeClass: TVRMLNodeClass; GoodNodeClass: TVRMLNodeClass);
  begin
    FinalizeNode;
    NastyGeometry := (NodeClass.Create('', '')) as TVRMLGeometryNode;
    NastyShape := TVRMLShape.Create(nil, NastyGeometry, TVRMLGraphTraverseState.CreateCopy(State), nil);

    { create also proxy, inside it's own shape.
      This can be used to test that proxy results, *if* they would be used,
      would be the same. }
    GoodGeometry := (GoodNodeClass.Create('', '')) as TVRMLGeometryNode;

    ProxyStateO := State;
    ProxyGeometryO := GoodGeometry.Proxy(ProxyStateO, true);
    if ProxyGeometryO <> nil then
      ProxyShapeO := TVRMLShape.Create(nil, ProxyGeometryO, TVRMLGraphTraverseState.CreateCopy(ProxyStateO), nil) else
      ProxyShapeO := nil;

    ProxyStateNO := State;
    ProxyGeometryNO := GoodGeometry.Proxy(ProxyStateNO, false);
    if ProxyGeometryNO <> nil then
      ProxyShapeNO := TVRMLShape.Create(nil, ProxyGeometryNO, TVRMLGraphTraverseState.CreateCopy(ProxyStateNO), nil) else
      ProxyShapeNO := nil;
  end;

  procedure AssertBoxEqual(const B1, B2: TBox3D);
  begin
    Assert(FloatsEqual(B1.Data[0][0], B2.Data[0][0], 0.1));
    Assert(FloatsEqual(B1.Data[0][1], B2.Data[0][1], 0.1));
    Assert(FloatsEqual(B1.Data[0][2], B2.Data[0][2], 0.1));
    Assert(FloatsEqual(B1.Data[1][0], B2.Data[1][0], 0.1));
    Assert(FloatsEqual(B1.Data[1][1], B2.Data[1][1], 0.1));
    Assert(FloatsEqual(B1.Data[1][2], B2.Data[1][2], 0.1));
  end;

  { Check node has optimized (not using proxy) versions of
    BoundingBox, LocalBoundingBox, TrianglesCount.
    And check their results match results of the proxy. }
  procedure CheckNodeBBoxAndTrisCount;
  begin
    AssertBoxEqual(NastyShape.BoundingBox, ProxyShapeO.BoundingBox);
    AssertBoxEqual(NastyShape.BoundingBox, ProxyShapeNO.BoundingBox);
    AssertBoxEqual(NastyShape.LocalBoundingBox, ProxyShapeO.LocalBoundingBox);
    AssertBoxEqual(NastyShape.LocalBoundingBox, ProxyShapeNO.LocalBoundingBox);
    Assert(NastyShape.TrianglesCount(false) = ProxyShapeNO.TrianglesCount(false));
    Assert(NastyShape.TrianglesCount(true ) = ProxyShapeO .TrianglesCount(true ));
  end;

begin
  NastyGeometry := nil;
  GoodGeometry := nil;
  NastyShape := nil;
  ProxyShapeO := nil;
  ProxyShapeNO := nil;
  ProxyStateO := nil;
  ProxyStateNO := nil;
  ProxyGeometryO := nil;
  ProxyGeometryNO := nil;
  State := TVRMLGraphTraverseState.Create;
  try

    { create each node, and try to call the methods that should
      be optimized (should *not* call the proxy).
      For nodes that we know *have* some proxy, we can also compare
      do the optimized results match the proxy results.
      So we test do optimized BoundingBox etc.
      1. are used
      and
      2. return correct result }
    InitializeNode(TNodeCone_1_NastyProxy, TNodeCone_1);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeCone_2_NastyProxy, TNodeCone_2);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeCylinder_1_NastyProxy, TNodeCylinder_1);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeCylinder_2_NastyProxy, TNodeCylinder_2);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeBox_NastyProxy, TNodeBox);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeCube_1_NastyProxy, TNodeCube_1);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeSphere_1_NastyProxy, TNodeSphere_1);
    CheckNodeBBoxAndTrisCount;

    InitializeNode(TNodeSphere_2_NastyProxy, TNodeSphere_2);
    CheckNodeBBoxAndTrisCount;
  finally
    FinalizeNode;
    FreeAndNil(State);
  end;
end;

initialization
 RegisterTest(TTestVRMLNodesOptimizedProxy);
end.
