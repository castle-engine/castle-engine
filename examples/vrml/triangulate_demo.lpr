{ Simple demo how to get direct list of 3D scene triangles. }

uses SysUtils, VectorMath, VRMLScene, VRMLShape;

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
  Scene: TVRMLScene;
  SI: TVRMLShapeTreeIterator;
  Handler: TTriangleHandler;
begin
  Scene := TVRMLScene.Create(nil);
  try
    Scene.Load('models/bridge_final.x3dv');

    SI := TVRMLShapeTreeIterator.Create(Scene.Shapes, true);
    try
      Handler := TTriangleHandler.Create;
      try
        while SI.GetNext do
          { Try also LocalTriangulate instead of Triangulate,
            to have Position in local shape coordinates. }
          SI.Current.Triangulate(true, @Handler.HandleTriangle);
      finally FreeAndNil(Handler) end;
    finally FreeAndNil(SI) end;

    { An alternative method: use Scene.OctreeVisibleTriangles.Triangles.
      This is available only when Scene.Spatial contains appropriate flag.
      This method is useful in larger programs, when besides writing triangles,
      you want to e.g. render or perform collision detection with the scene.
      In such case, you will have Scene.OctreeVisibleTriangles
      created anyway. So you can use it also to get triangles list.

    var
      TriangleInfo: PVRMLTriangle;

      ...

    Scene.Spatial := Scene.Spatial + [ssVisibleTriangles];
    Scene.TriangleOctreeLimits^.MaxDepth := 1;
    for I := 0 to Scene.OctreeVisibleTriangles.Triangles.Count - 1 do
    begin
      TriangleInfo := @(Scene.OctreeVisibleTriangles.Triangles.List^[I]);
      Writeln('Triangle position (in world coordinates): ', TriangleToNiceStr(TriangleInfo^.World.Triangle));
    end;
    }

  finally FreeAndNil(Scene) end;
end.
