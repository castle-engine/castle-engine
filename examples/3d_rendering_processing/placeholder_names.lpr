{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Pass on command-line the URL (usually just a filename; "-" means stdin)
  of 3D model file and the name of placeholder detection routine
  (like "x3dshape" or "blender").
  We will output information about detected placeholders in the file.

  For example,
    ./placeholder_names model.x3d blender
  will assume that model.x3d was created using standard Blender X3D exporter,
  and will show Blender object names for every shape in the model.

  See TGameSceneManager.LoadLevel for a description where we use
  placeholders in the engine. }
program placeholder_names;

uses SysUtils, CastleUtils, CastleShapes, CastleSceneCore, CastleParameters,
  X3DNodes;

var
  PlaceholderName: TPlaceholderName;
  Scene: TCastleSceneCore;
  Shape: TShape;
  SI: TShapeTreeIterator;
  I: Integer;
begin
  { show PlaceholderNames }
  Writeln(Format('Available placeholder detection methods (%d):',
    [PlaceholderNames.Count]));
  for I := 0 to PlaceholderNames.Count - 1 do
    Writeln('  ', PlaceholderNames.Keys[I]);

  { calculate PlaceholderName looking at command-line parameter }
  Parameters.CheckHigh(2);
  I := PlaceholderNames.IndexOf(Parameters[2]);
  if I <> -1 then
    PlaceholderName := PlaceholderNames.Data[I] else
    raise Exception.CreateFmt('Placeholder detection method "%s" not found',
      [Parameters[2]]);

  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(Parameters[1], true);
    SI := TShapeTreeIterator.Create(Scene.Shapes, { OnlyActive } true);
    try
      while SI.GetNext do
      begin
        Shape := SI.Current;
        if PlaceholderName(Shape) <> '' then
          Writeln(Format('Detected placeholder "%s" for shape. Shape geometry node name "%s", geometry parent node name "%s", geometry grand-parent node name "%s", geometry grand-grand-parent node name "%s".',
            [PlaceholderName(Shape),
             Shape.OriginalGeometry.X3DName,
             Shape.GeometryParentNodeName,
             Shape.GeometryGrandParentNodeName,
             Shape.GeometryGrandGrandParentNodeName]));
      end;
    finally FreeAndNil(SI) end;
  finally FreeAndNil(Scene) end;
end.