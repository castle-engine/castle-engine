{ Simple demo how to get direct list of 3D scene triangles. }
program triangulate_demo;

uses SysUtils, CastleVectors, CastleSceneCore, CastleShapes, CastleTriangles,
  CastleFilesUtils;

type
  TTriangleHandler = class
    procedure HandleTriangle(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
  end;

procedure TTriangleHandler.HandleTriangle(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
begin
  Writeln('Triangle position (in world coordinates): ',
    TriangleToNiceStr(Position));
end;

var
  Scene: TCastleSceneCore;
  SI: TShapeTreeIterator;
  Handler: TTriangleHandler;
begin
  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(ApplicationData('bridge_final.x3dv'));

    SI := TShapeTreeIterator.Create(Scene.Shapes, true);
    try
      Handler := TTriangleHandler.Create;
      try
        while SI.GetNext do
          { Try also LocalTriangulate instead of Triangulate,
            to have Position in local shape coordinates. }
          SI.Current.Triangulate(true, @Handler.HandleTriangle);
      finally FreeAndNil(Handler) end;
    finally FreeAndNil(SI) end;

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
      Writeln('Triangle position (in world coordinates): ', TriangleToNiceStr(TriangleInfo^.World.Triangle));
    end;
    }

  finally FreeAndNil(Scene) end;
end.
