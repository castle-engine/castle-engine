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

{ Simple example how to build a 3D object (which is equivalent to
  VRML/X3D nodes graph) by pure ObjectPascal code.

  Check out latest X3D specification,
  http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/index.html,
  as our
  http://vrmlengine.sourceforge.net/vrml_implementation_status.php
  for a complete list of supported VRML/X3D nodes.

  Note: suffix "_2" at the end of some class names is used to indicate
  that we want VRML 2.0/X3D versions of the nodes (as opposed to older,
  not compatible, VRML 1.0 versions). }

uses SysUtils, VectorMath, VRMLNodes;

var
  Root: TNodeGroup_2;
  Sphere: TNodeSphere_2;
  SphereShape: TNodeShape;
  IndexedFaceSetCoordinate: TNodeCoordinate;
  IndexedFaceSet: TNodeIndexedFaceSet_2;
  IndexedFaceSetShape: TNodeShape;
  IndexedFaceSetShapeTranslated: TNodeTransform_2;
  Appearance: TNodeAppearance;
begin
  { This will be used as appearance for both sphere and IndexedFaceSet }
  Appearance := TNodeAppearance.Create('', '');
  Appearance.FdMaterial.Value := TNodeMaterial_2.Create('', '');

  Sphere := TNodeSphere_2.Create('', '');
  Sphere.FdRadius.Value := 2;

  SphereShape := TNodeShape.Create('', '');
  SphereShape.FdAppearance.Value := Appearance;
  SphereShape.FdGeometry.Value := Sphere;

  IndexedFaceSetCoordinate := TNodeCoordinate.Create('', '');
  IndexedFaceSetCoordinate.FdPoint.Items.AppendArray(
    [Vector3Single(0, 0, 0),
     Vector3Single(1, 0, 0),
     Vector3Single(1, 1, 0),
     Vector3Single(0, 1, 0),

     Vector3Single(0, 0, 0.5),
     Vector3Single(1, 0, 0.5),
     Vector3Single(1, 1, 0.5),
     Vector3Single(0, 1, 0.5)
    ]);

  IndexedFaceSet := TNodeIndexedFaceSet_2.Create('', '');
  IndexedFaceSet.FdCoordIndex.Items.AppendArray(
    { Two quad faces. These are just indexes for
      the array placed in IndexedFaceSet.FdCoordinate array. }
    [0, 1, 2, 3, 0, -1, 4, 5, 6, 7, 4]);
  IndexedFaceSet.FdCoord.Value := IndexedFaceSetCoordinate;

  IndexedFaceSetShape := TNodeShape.Create('', '');
  IndexedFaceSetShape.FdAppearance.Value := Appearance;
  IndexedFaceSetShape.FdGeometry.Value := IndexedFaceSet;

  IndexedFaceSetShapeTranslated := TNodeTransform_2.Create('', '');
  IndexedFaceSetShapeTranslated.FdTranslation.Value := Vector3Single(2, 0, 0);
  IndexedFaceSetShapeTranslated.FdChildren.Add(IndexedFaceSetShape);

  Root := TNodeGroup_2.Create('', '');
  try
    Root.FdChildren.Add(SphereShape);
    Root.FdChildren.Add(IndexedFaceSetShapeTranslated);

    { The Root holds now a tree of your VRML/X3D nodes, thus describing
      your 3D object. You can do whatever you want with it:
      example line below trivially saves Root to VRML/X3D file
      (that can be later loaded by our engine). }
    SaveVRML(Root, 'test.wrl', 'build_3d_object_by_code', '', xeClassic);

    { You also could create a TVRMLScene, or even TVRMLGLScene
      to render this 3D object directly in OpenGL (without saving to any
      file). For example in scene_manager_basic.lpr you could replace
        Scene.Load('models/boxes.x3dv');
      with
        Scene.Load(Root, false);
      to render your constructed 3D object.

      Note that you can even rebuild and change the 3D object *after*
      it's already loaded and rendered by TVRMLGLScene.
      See demos change_vrml_by_code.lpr and change_vrml_by_code_2.lpr. }

  finally FreeAndNil(Root) end;
end.
