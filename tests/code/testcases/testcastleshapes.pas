// -*- compile-command: "./test_single_testcase.sh TTestCastleShapes" -*-
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleShapes unit. }
unit TestCastleShapes;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase {$else}CastleTester{$endif};

type
  TTestCastleShapes = class(TCastleTestCase)
    procedure TestShapeParentNodes;
  end;

implementation

uses CastleSceneCore, X3DNodes, CastleShapes;

procedure TTestCastleShapes.TestShapeParentNodes;
var
  Box: TBoxNode;
  Shape: TShapeNode;
  T1: TTransformNode;
  T2: TTransformNode;
  Shapes: TShapeList;
  RootNode: TX3DRootNode;
  Scene: TCastleSceneCore;
begin
  Box := TBoxNode.CreateWithShape(Shape);
  Shape.X3DName := 'CommonShape';

  T1 := TTransformNode.Create('T1');
  T1.AddChildren(Shape);

  T2 := TTransformNode.Create('T2');
  T2.AddChildren(Shape);

  RootNode := TX3DRootNode.Create('Root');
  RootNode.AddChildren(T1);
  RootNode.AddChildren(T2);
  RootNode.AddChildren(Shape);

  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(RootNode, true);

    Shapes := Scene.Shapes.TraverseList(true);
    AssertEquals(3, Shapes.Count);

    AssertTrue(Shapes[0].OriginalGeometry = Box);
    AssertTrue(Shapes[0].Node = Shape);
    AssertTrue(Shapes[0].GeometryParentNode = Shape);
    AssertTrue(Shapes[0].GeometryGrandParentNode = T1);
    AssertTrue(Shapes[0].GeometryGrandGrandParentNode = RootNode);
    AssertEquals('CommonShape', Shapes[0].GeometryParentNodeName);
    AssertEquals('T1', Shapes[0].GeometryGrandParentNodeName);
    AssertEquals('Root', Shapes[0].GeometryGrandGrandParentNodeName);

    AssertTrue(Shapes[1].OriginalGeometry = Box);
    AssertTrue(Shapes[1].Node = Shape);
    AssertTrue(Shapes[1].GeometryParentNode = Shape);
    AssertTrue(Shapes[1].GeometryGrandParentNode = T2);
    AssertTrue(Shapes[1].GeometryGrandGrandParentNode = RootNode);
    AssertEquals('CommonShape', Shapes[1].GeometryParentNodeName);
    AssertEquals('T2', Shapes[1].GeometryGrandParentNodeName);
    AssertEquals('Root', Shapes[1].GeometryGrandGrandParentNodeName);

    AssertTrue(Shapes[2].OriginalGeometry = Box);
    AssertTrue(Shapes[2].Node = Shape);
    AssertTrue(Shapes[2].GeometryParentNode = Shape);
    AssertTrue(Shapes[2].GeometryGrandParentNode = RootNode);
    AssertTrue(Shapes[2].GeometryGrandGrandParentNode = nil);
    AssertEquals('CommonShape', Shapes[2].GeometryParentNodeName);
    AssertEquals('Root', Shapes[2].GeometryGrandParentNodeName);
    AssertEquals('', Shapes[2].GeometryGrandGrandParentNodeName);
  finally FreeAndNil(Scene) end;
end;

initialization
  RegisterTest(TTestCastleShapes);
end.
