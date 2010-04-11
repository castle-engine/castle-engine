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

uses SysUtils, KambiUtils, VRMLNodes;

type
  TDummy = class
    procedure Traverse(
      BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
      BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
      Geometry: TVRMLGeometryNode;
      StateStack: TVRMLGraphTraverseStateStack);
  end;

procedure TDummy.Traverse(
  BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
  Geometry: TVRMLGeometryNode;
  StateStack: TVRMLGraphTraverseStateStack);
begin
  Writeln(
    'Blender object "', BlenderObjectName, '" (VRML ', BlenderObjectNode.NodeTypeName, ') -> ' +
              'mesh "', BlenderMeshName, '" (VRML ', BlenderMeshNode.NodeTypeName, ')');
end;

var
  Node: TVRMLNode;
  { No need to actually create Dummy, this is used only because FPC doesn't
    allow class procedures to be passed directly as method pointers. }
  Dummy: TDummy = nil;
begin
  Node := LoadVRMLClassic(Parameters[1], true);
  try
    Node.TraverseBlenderObjects(@Dummy.Traverse);
  finally FreeAndNil(Node) end;
end.