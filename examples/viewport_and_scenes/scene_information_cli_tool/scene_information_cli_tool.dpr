{
  Copyright 2010-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Command-line application to output some scene information.
  See README.md for description.  }
program scene_information_cli_tool;

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils,
  CastleVectors, CastleScene, CastleShapes, CastleTriangles, CastleUtils,
  CastleFilesUtils, CastleParameters, CastleUriUtils;

type
  { Utility class for WritelnSceneTriangles. }
  TTriangleHandler = class
    TriangleIndex: Integer;
    procedure HandleTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  end;

procedure TTriangleHandler.HandleTriangle(Shape: TObject;
  const Position: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  Writeln('Triangle ', TriangleIndex, ' positions (in world coordinates):');
  Writeln(Position.ToString);
  Inc(TriangleIndex);
end;

{ Write all triangles in the scene.

  Note: This is not an operation you should do in a normal application.
  Let Castle Game Engine handle your triangles, in an efficient fashion :)

  Note 2: For most operations, including CGE internal usage,
  we actually don't split entire scene into triangles like this.
  Instead we have TCastleShape.GeometryArrays which constructs
  arrays to pass the data to GPU in an efficient way.
  The TCastleShape.GeometryArrays is better than "just a list of triangles"
  because

  - it contains all the per-vertex information we need, not only positions

  - it expresses all the primitives we need

  - it is indexed if possible.

  See `examples/research_special_rendering_methods/new_renderer_skeleton/new_renderer_skeleton.lpr`
  for more comments. }
procedure WritelnSceneTriangles(const Scene: TCastleScene);
var
  ShapeList: TShapeList;
  Shape: TShape;
  Handler: TTriangleHandler;
begin
  ShapeList := Scene.Shapes.TraverseList(true);
  Handler := TTriangleHandler.Create;
  try
    for Shape in ShapeList do
      { Note: There's also LocalTriangulate instead of Triangulate,
        to have Position in local shape coordinates. }
      Shape.Triangulate({$ifdef FPC}@{$endif} Handler.HandleTriangle, true);
    Writeln('Listed triangles count: ', Handler.TriangleIndex);
  finally FreeAndNil(Handler) end;
end;

var
  InputFileName: String = 'castle-data:/teapot.x3dv';
  Scene: TCastleScene;
begin
  if Parameters.High = 1 then
    InputFileName := Parameters[1];

  Scene := TCastleScene.Create(nil);
  try
    Scene.Load(InputFileName);
    Writeln('Loaded scene: ', UriDisplay(Scene.Url));
    Writeln('Triangles: ', Scene.TrianglesCount);
    Writeln('Vertices: ', Scene.VerticesCount);
    Writeln('Bounding box: ', Scene.BoundingBox.ToString);
    Writeln('Shapes: ', Scene.Shapes.ShapesCount(false, false, false));

    Writeln('------------------------');
    Writeln('Animations: ', Scene.AnimationsList.Count);
    Writeln('Animation names: ');
    Writeln(Scene.AnimationsList.Text);

    // long output
    // Writeln('------------------------');
    // WritelnSceneTriangles(Scene);

    Writeln('------------------------');
    Writeln('Castle Game Engine version: ' + CastleEngineVersion);
    Writeln('Compiled with ' + SCompilerDescription);
    Writeln('Platform: ' + SPlatformDescription);

  finally FreeAndNil(Scene) end;
end.
