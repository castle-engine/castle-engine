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
    Assert(FloatsEqual(B1[0][0], B2[0][0], 0.1));
    Assert(FloatsEqual(B1[0][1], B2[0][1], 0.1));
    Assert(FloatsEqual(B1[0][2], B2[0][2], 0.1));
    Assert(FloatsEqual(B1[1][0], B2[1][0], 0.1));
    Assert(FloatsEqual(B1[1][1], B2[1][1], 0.1));
    Assert(FloatsEqual(B1[1][2], B2[1][2], 0.1));
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
    (*TODO: why does this fail with segfault inside ProxyShapeO.BoundingBox,

#0  0x081b5b4f in TGI_BBOX_CALCULATOR__GETVERTEXFROMINDEX (INDEX=32, this=0xb67bdf80, result={4.6752597e-34, 5.62874266e-34, -3.96868563e-06}) at ./src/vrml/vrmlnodes_boundingboxes.inc:49
#1  0x081787c0 in TVERTTRANSFORM_CALCULATOR__GETTRANSFORMED (INDEX=32, this=0xb67be6a0, result={-1.9994874, 5.68249765e-34, -1.9994874}) at /home/michalis/sources/vrmlengine/trunk/kambi_vrml_game_engine/src/3d/boxes3d.pas:824
#2  0x0817864f in CALCULATEBOUNDINGBOXFROMINDICES (GETVERTINDEX=0x81b5c90 <TGI_BBOX_CALCULATOR__GETINDEX>, VERTSINDICESCOUNT=2940, GETVERTEX=0x8178790 <TVERTTRANSFORM_CALCULATOR__GETTRANSFORMED>, result={{-1.99952793, -1.99952078, 5.14164214e-34}, {-1.99952221, -1.99953985, 1.40129846e-45}}) at /home/michalis/sources/vrmlengine/trunk/kambi_vrml_game_engine/src/3d/boxes3d.pas:797
#3  0x08178879 in CALCULATEBOUNDINGBOXFROMINDICES (GETVERTINDEX=0x81b5c90 <TGI_BBOX_CALCULATOR__GETINDEX>, VERTSINDICESCOUNT=2940, GETVERTEX=0x81b5af0 <TGI_BBOX_CALCULATOR__GETVERTEXFROMINDEX>, TRANSFORM={{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}, result={{-1.99952793, -1.99952078, 5.14164214e-34}, {-1.99952221, -1.99953985, 1.40129846e-45}}) at /home/michalis/sources/vrmlengine/trunk/kambi_vrml_game_engine/src/3d/boxes3d.pas:841
#4  0x081b5da4 in INDEXEDCOORDS_BOUNDINGBOX (GEOMETRY=0xb67c9300, STATE=0xb67e9620, COORD=0xdeadbeef, COORDINDEX=0xb77c4260, result={{-1.99952793, -1.99952078, 5.14164214e-34}, {-1.99952221, -1.99953985, 1.40129846e-45}}) at ./src/vrml/vrmlnodes_boundingboxes.inc:83
#5  0x081b60d1 in TVRMLGEOMETRYNODE__BOUNDINGBOX (STATE=0xb67e9620, PROXYGEOMETRY=0x0, PROXYSTATE=0x0, this=0xb67c9300, result={{-1.99952793, -1.99952078, 5.14164214e-34}, {-1.99952221, -1.99953985, 1.40129846e-45}}) at ./src/vrml/vrmlnodes_boundingboxes.inc:138
#6  0x082ae1b9 in TVRMLSHAPE__BOUNDINGBOX (this=0xb64d6180, result={{-1.00000012, -1, 5.49513671e-34}, {5.68289621e-34, -1.99954653, 5.68289621e-34}}) at ./src/vrml/vrmlshape.pas:869
#7  0x080817e4 in CHECKNODEBBOXANDTRISCOUNT (parentfp=0xbffff124) at tests/testvrmlnodesoptimizedproxy.pas:283
#8  0x0808177a in TTESTVRMLNODESOPTIMIZEDPROXY__TESTGEOMETRYUSESOPTIMIZEDMETHODS (this=0xb67b9a80) at tests/testvrmlnodesoptimizedproxy.pas:332

    ?*)
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
