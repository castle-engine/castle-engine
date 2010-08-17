{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Run with $1 = filename ("-" means stdin) of VRML file generated
  by standard Blender VRML 1.0 or 2.0 exporter.
  This will output Blender hierarchy information extracted from VRML file,
  that tries to match as closely as possible original Blender hierarchy.
  Note that VRML 1.0 exporter doesn't record object names, so they
  are always empty. }
program test_blender_exported_hierarchy;

uses SysUtils, KambiUtils, VRMLShape, VRMLScene;

procedure Traverse(Shape: TVRMLShape);
begin
  Writeln(
    'Blender object "', Shape.BlenderObjectName, '" (VRML ', Shape.BlenderObjectNode.NodeTypeName, ') -> ' +
              'mesh "', Shape.BlenderMeshName, '" (VRML ', Shape.BlenderMeshNode.NodeTypeName, ')');
end;

var
  Scene: TVRMLScene;
  SI: TVRMLShapeTreeIterator;
begin
  Scene := TVRMLScene.Create(nil);
  try
    Scene.Load(Parameters[1], true);

    SI := TVRMLShapeTreeIterator.Create(Scene.Shapes, { OnlyActive } true);
    try
      while SI.GetNext do Traverse(SI.Current);
    finally FreeAndNil(SI) end;
  finally FreeAndNil(Scene) end;
end.