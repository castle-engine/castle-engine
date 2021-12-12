{
  Copyright 2008-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Pass on command-line the URL (usually just a filename)
  of 3D model file and the name of placeholder detection routine
  (like "x3dshape" or "blender").
  We will output information about detected placeholders in the file.

  For example,
    ./placeholder_names model.x3d blender
    ./placeholder_names ../fps_game/data/example_level/example_level.gltf blender
  will assume that model.x3d was created using standard Blender X3D exporter,
  and will show Blender object names for every shape in the model.

  See TLevel.Load for a description where we use
  placeholders in the engine. }
program placeholder_names;

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, CastleUtils, CastleShapes, CastleSceneCore, CastleParameters,
  X3DNodes;

var
  PlaceholderName: TPlaceholderName;
  Scene: TCastleSceneCore;
  ShapeList: TShapeList;
  Shape: TShape;
  PlaceholderNameKey: string;
begin
  { show PlaceholderNames }
  Writeln(Format('Available placeholder detection methods (%d):',
    [PlaceholderNames.Count]));
  for PlaceholderNameKey in PlaceholderNames.Keys do
    Writeln('  ', PlaceholderNameKey);

  if Parameters.High = 0 then
  begin

    Writeln(NL+
      'placeholder_names: Outputs information about detected placeholders in the scene file.' + NL +
      NL +
      'Call with 2 arguments:' + NL +
      '- URL (can be just a filename) of scene file' + NL +
      '- name of placeholder detection routine, see above for possible values' + NL +
      NL +
      'Examples:' + NL +
      '  ./placeholder_names scene.x3d blender' + NL +
      '  ./placeholder_names ../fps_game/data/example_level/example_level.gltf blender'
    );
    Halt(1);
  end;

  { calculate PlaceholderName looking at command-line parameter }
  Parameters.CheckHigh(2);
  if not PlaceholderNames.TryGetValue(Parameters[2], PlaceholderName) then
    raise Exception.CreateFmt('Placeholder detection method "%s" not found',
      [Parameters[2]]);

  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(Parameters[1]);
    ShapeList := Scene.Shapes.TraverseList({ OnlyActive } true);
    for Shape in ShapeList do
    begin
      if PlaceholderName(Shape) <> '' then
        Writeln(Format('Detected placeholder "%s" for shape. Shape geometry node name "%s", geometry parent node name "%s", geometry grand-parent node name "%s", geometry grand-grand-parent node name "%s".',
          [PlaceholderName(Shape),
           Shape.OriginalGeometry.X3DName,
           Shape.GeometryParentNodeName,
           Shape.GeometryGrandParentNodeName,
           Shape.GeometryGrandGrandParentNodeName]));
    end;
  finally FreeAndNil(Scene) end;
end.
