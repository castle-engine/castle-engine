{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple example how to build a 3D object (which is equivalent to
  VRML/X3D nodes graph) by pure ObjectPascal code.

  Check out latest X3D specification,
  http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/index.html,
  and our
  http://castle-engine.sourceforge.net/x3d_implementation_status.php
  for a complete list of supported VRML/X3D nodes. }

uses SysUtils, CastleVectors, X3DNodes;

var
  Root: TX3DRootNode;
  Sphere: TSphereNode;
  SphereShape: TShapeNode;
  IndexedFaceSetCoordinate: TCoordinateNode;
  IndexedFaceSet: TIndexedFaceSetNode;
  IndexedFaceSetShape: TShapeNode;
  IndexedFaceSetShapeTranslated: TTransformNode;
  Appearance: TAppearanceNode;
begin
  { This will be used as appearance for both sphere and IndexedFaceSet }
  Appearance := TAppearanceNode.Create;
  Appearance.Material := TMaterialNode.Create;

  Sphere := TSphereNode.Create;
  Sphere.Radius := 2;

  SphereShape := TShapeNode.Create;
  SphereShape.Appearance := Appearance;
  SphereShape.Geometry := Sphere;

  IndexedFaceSetCoordinate := TCoordinateNode.Create;
  IndexedFaceSetCoordinate.FdPoint.Items.AddArray(
    [Vector3Single(0, 0, 0),
     Vector3Single(1, 0, 0),
     Vector3Single(1, 1, 0),
     Vector3Single(0, 1, 0),

     Vector3Single(0, 0, 0.5),
     Vector3Single(1, 0, 0.5),
     Vector3Single(1, 1, 0.5),
     Vector3Single(0, 1, 0.5)
    ]);

  IndexedFaceSet := TIndexedFaceSetNode.Create;
  IndexedFaceSet.FdCoordIndex.Items.AddArray(
    { Two quad faces. These are just indexes for
      the array placed in IndexedFaceSet.FdCoordinate array. }
    [0, 1, 2, 3, 0, -1, 4, 5, 6, 7, 4]);
  IndexedFaceSet.FdCoord.Value := IndexedFaceSetCoordinate;

  IndexedFaceSetShape := TShapeNode.Create;
  IndexedFaceSetShape.Appearance := Appearance;
  IndexedFaceSetShape.Geometry := IndexedFaceSet;

  IndexedFaceSetShapeTranslated := TTransformNode.Create;
  IndexedFaceSetShapeTranslated.Translation := Vector3Single(2, 0, 0);
  IndexedFaceSetShapeTranslated.FdChildren.Add(IndexedFaceSetShape);

  Root := TX3DRootNode.Create;
  try
    Root.FdChildren.Add(SphereShape);
    Root.FdChildren.Add(IndexedFaceSetShapeTranslated);

    { The Root holds now a tree of your VRML/X3D nodes, thus describing
      your 3D object. You can do whatever you want with it:
      example line below trivially saves Root to VRML/X3D file
      (that can be later loaded by our engine). }
    Save3D(Root, 'test.x3d', 'build_3d_object_by_code');

    { You also could create a TCastleSceneCore, or even TCastleScene
      to render this 3D object directly in OpenGL (without saving to any
      file). For example in scene_manager_basic.lpr you could replace
        Scene.Load(ApplicationData('boxes.x3dv'));
      with
        Scene.Load(Root, false);
      to render your constructed 3D object.

      Note that you can even rebuild and change the 3D object *after*
      it's already loaded and rendered by TCastleScene.
      See demos animate_3d_model_by_code.lpr and animate_3d_model_by_code_2.lpr. }

  finally FreeAndNil(Root) end;
end.
