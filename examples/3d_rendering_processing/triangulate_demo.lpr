{ Simple demo how to get direct list of 3D scene triangles. }
program triangulate_demo;

uses SysUtils, CastleVectors, CastleSceneCore, CastleShapes, CastleTriangles,
  CastleFilesUtils;

type
  TTriangleHandler = class
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
  Writeln('Triangle position (in world coordinates):');
  Write(Position.ToString);
end;

var
  Scene: TCastleSceneCore;
  ShapeList: TShapeList;
  Shape: TShape;
  Handler: TTriangleHandler;
begin
  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load('castle-data:/bridge_final.x3dv');

    ShapeList := Scene.Shapes.TraverseList(true);
    Handler := TTriangleHandler.Create;
    try
      for Shape in ShapeList do
        { Try also LocalTriangulate instead of Triangulate,
          to have Position in local shape coordinates. }
        Shape.Triangulate(true, @Handler.HandleTriangle);
    finally FreeAndNil(Handler) end;

    { An alternative method: use Scene.InternalOctreeVisibleTriangles.Triangles.
      This is available only when Scene.Spatial contains appropriate flag.
      This method is useful in larger programs, when besides writing triangles,
      you want to e.g. render or perform collision detection with the scene.
      In such case, you will have Scene.InternalOctreeVisibleTriangles
      created anyway. So you can use it also to get triangles list.

    var
      TriangleInfo: PTriangle;

      ...

    Scene.Spatial := Scene.Spatial + [ssVisibleTriangles];
    Scene.TriangleOctreeLimits^.MaxDepth := 1;
    for I := 0 to Scene.InternalOctreeVisibleTriangles.Triangles.Count - 1 do
    begin
      TriangleInfo := @(Scene.InternalOctreeVisibleTriangles.Triangles.List^[I]);
      Writeln('Triangle position (in world coordinates):');
      Write(TriangleInfo^.World.Triangle.ToString);
    end;
    }

  finally FreeAndNil(Scene) end;
end.
