{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo/test of various ways to push indexed vertex data to OpenGL.

  For maximum simplicity, it only renders one shape (IndexedFaceSet)
  from X3D/VRML model (other 3D model formats are converted to VRML
  on load under the hood). }
program gl_primitive_performance;

uses VectorMath, VRMLNodes, GL, GLExt, GLWindow,
  KambiUtils, SysUtils, Classes, KambiWarnings,
  KambiGLUtils, KambiFilesUtils, VRMLScene,
  ProgressUnit, VRMLShape, KambiSceneManager, Base3D;

type
  TRenderMode = (
    { The worst versions: do not pass indexes to OpenGL, instead
      pass direct vector values. This implies no vertex sharing
      (well, almost: within polygon and triangle_fan verts will be shared).
      For polygon and triangle_fan this also makes a lot of primiive
      restarts, which may hurt quite like state changes. }
    rmDirectManyPolygons, rmDirectManyFans, rmDirectOnceTriangles,
    { Vertex arrays, each vertex is specified by glArrayElement. }
    rmVARTrianglesByVertex, rmVARLockedTrianglesByVertex,
    { Vertex arrays, all vertexes are passed by 1 call to glDrawElements. }
    rmVARTriangles, rmVARLockedTriangles,
    { Vertex buffers. }
    rmVBOTrianglesByVertex,
    rmVBOTriangles
    );

var
  Window: TGLUIWindow;
  Scene: TVRMLScene;
  Shape: TVRMLShape;
  Vertexes: TDynVector3SingleArray;
  CoordIndex: TDynLongIntArray;
  TrianglesCoordIndex: TDynLongIntArray;

  Mode: TRenderMode = rmDirectOnceTriangles;
  Wireframe: boolean;
  VBOVertex: TGLuint;

type
  TMySceneManager = class(TKamSceneManager)
    procedure Render3D(const Params: TRenderParams); override;
  end;

procedure TMySceneManager.Render3D(const Params: TRenderParams);
var
  I: Integer;
begin
  if not (Params.TransparentGroup in [tgAll, tgOpaque]) then Exit;

  glMultMatrix(Shape.State.Transform);

  { render scene }
  case Mode of
    rmDirectManyPolygons:
      begin
        glBegin(GL_POLYGON);
          for I := 0 to CoordIndex.Count - 1 do
          begin
            if CoordIndex.Items[I] < 0 then
            begin
              glEnd;
              glBegin(GL_POLYGON);
            end else
              glVertexv(Vertexes.Items[CoordIndex.Items[I]]);
          end;
        glEnd;
      end;
    rmDirectManyFans:
      begin
        glBegin(GL_TRIANGLE_FAN);
          for I := 0 to CoordIndex.Count - 1 do
          begin
            if CoordIndex.Items[I] < 0 then
            begin
              glEnd;
              glBegin(GL_TRIANGLE_FAN);
            end else
              glVertexv(Vertexes.Items[CoordIndex.Items[I]]);
          end;
        glEnd;
      end;
    rmDirectOnceTriangles:
      begin
        glBegin(GL_TRIANGLES);
          for I := 0 to TrianglesCoordIndex.Count - 1 do
            glVertexv(Vertexes.Items[TrianglesCoordIndex.Items[I]]);
        glEnd;
      end;
    rmVARTrianglesByVertex,
    rmVARLockedTrianglesByVertex,
    rmVBOTrianglesByVertex:
      begin
        glBegin(GL_TRIANGLES);
          for I := 0 to TrianglesCoordIndex.Count - 1 do
            glArrayElement(TrianglesCoordIndex.Items[I]);
        glEnd;
      end;
    rmVARTriangles,
    rmVARLockedTriangles,
    rmVBOTriangles:
      begin
        glDrawElements(GL_TRIANGLES, TrianglesCoordIndex.Count, GL_UNSIGNED_INT,
          TrianglesCoordIndex.ItemsArray);
      end;

  end;
end;

procedure Open(Window: TGLWindow);
begin
  Writeln('OpenGL has locking VAR support: ', GL_EXT_compiled_vertex_array);
  Writeln('Vertex arrays optimal fill: ',
    glGetInteger(GL_MAX_ELEMENTS_VERTICES), ' vertexes, ',
    glGetInteger(GL_MAX_ELEMENTS_INDICES), ' indices.');
//  glEnable(GL_LIGHTING);
//  glEnable(GL_LIGHT0);
//  glEnable(GL_DEPTH_TEST);
  glColor3f(1, 1, 0);
end;

var
  { Used only by LockArraysBegin and LockArraysEnd. }
  ArraysLocked: boolean = false;

  procedure LockArraysBegin;
  begin
    { See
      [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
      for description of GL_EXT_compiled_vertex_array.
      Note that I can't use it when TrianglesCoordIndex has no items
      (because glLockArraysEXT(0, 0) causes OpenGL error "invalid value".) }
    ArraysLocked := GL_EXT_compiled_vertex_array and
      (TrianglesCoordIndex.Count <> 0);
    if ArraysLocked then
      glLockArraysEXT(0, TrianglesCoordIndex.Count);
  end;

  procedure LockArraysEnd;
  begin
    if ArraysLocked then
    begin
      glUnlockArraysEXT;
      ArraysLocked := false;
    end;
  end;

procedure MenuCommand(Window: TGLWindow; Item: TMenuItem);
const
  VARModes = [
    rmVARTrianglesByVertex, rmVARLockedTrianglesByVertex,
    rmVARTriangles        , rmVARLockedTriangles
  ];
  VARLockedModes = [
    rmVARLockedTrianglesByVertex,
    rmVARLockedTriangles
  ];
  VBOModes = [rmVBOTrianglesByVertex, rmVBOTriangles];
begin
  case Item.IntData of
    10: begin
          Wireframe := not Wireframe;
          if Wireframe then
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE) else
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        end;
    20..40:
        begin
          { simply disable/destroy all vertex arrays/buffers from
            previous Mode }
          LockArraysEnd;
          glDisableClientState(GL_VERTEX_ARRAY);
          if Mode in VBOModes then
          begin
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
            glDeleteBuffersARB(1, @VBOVertex);
          end;

          Mode := TRenderMode(Item.IntData - 20);

          { enable/create vertex arrays/buffers for new Mode }
          if Mode in VARModes then
          begin
            glVertexPointer(3, GL_FLOAT, 0, Vertexes.ItemsArray);
            glEnableClientState(GL_VERTEX_ARRAY);

            if Mode in VARLockedModes then
              LockArraysBegin;
          end else
          if Mode in VBOModes then
          begin
            glGenBuffersARB(1, @VBOVertex);
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBOVertex);
            glBufferDataARB(GL_ARRAY_BUFFER_ARB,
              3 * SizeOf(Single) * Vertexes.Count, Vertexes.ItemsArray,
              GL_STATIC_DRAW_ARB);
            glVertexPointer(3, GL_FLOAT, 0, nil);
            glEnableClientState(GL_VERTEX_ARRAY);
          end;
        end;
    200: Window.Close;
    else Exit;
  end;
  Window.PostRedisplay;
end;

function CreateMainMenu: TMenu;

  procedure AddRenderModes(const Base: Integer; M: TMenu);
  const
    ModeNames: array [TRenderMode] of string =
    ( 'Direct: many POLYGONs',
      'Direct: many TRIANGLE__FANs',
      'Direct: once TRIANGLES',
      'Vertex array: glArrayElement',
      'Vertex array: glArrayElement, locked',
      'Vertex array: glDrawElements',
      'Vertex array: glDrawElements, locked',
      'Vertex buffer object: glArrayElement',
      'Vertex buffer object: glDrawElements'
    );
  var
    RadioGroup: TMenuItemRadioGroup;
    Radio: TMenuItemRadio;
    RM: TRenderMode;
  begin
    RadioGroup := nil;

    for RM := Low(RM) to High(RM) do
    begin
      Radio := TMenuItemRadio.Create(ModeNames[RM], Base + Ord(RM),
        Mode = RM, true);
      if RadioGroup <> nil then
        Radio.Group := RadioGroup else
        RadioGroup := Radio.Group;
      M.Append(Radio);
    end;
  end;

var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    AddRenderModes(20, M);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Wireframe', 10, Wireframe, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);
end;

procedure MakeTrianglesCoordIndex;

  procedure Poly(BeginIndex, EndIndex: Integer);
  var
    FirstIndex, C: Integer;
  begin
    FirstIndex := BeginIndex;

    while BeginIndex + 2 < EndIndex do
    begin
      C := TrianglesCoordIndex.Count;
      TrianglesCoordIndex.Count := TrianglesCoordIndex.Count + 3;
      TrianglesCoordIndex.Items[C    ] := CoordIndex.Items[FirstIndex    ];
      TrianglesCoordIndex.Items[C + 1] := CoordIndex.Items[BeginIndex + 1];
      TrianglesCoordIndex.Items[C + 2] := CoordIndex.Items[BeginIndex + 2];
      Inc(BeginIndex);
    end;
  end;

var
  BeginIndex, EndIndex: Integer;
begin
  BeginIndex := 0;
  while BeginIndex < CoordIndex.Count do
  begin
    EndIndex := BeginIndex;
    while (EndIndex < CoordIndex.Count) and
          (CoordIndex.Items[EndIndex] >= 0) do
      Inc(EndIndex);
    Poly(BeginIndex, EndIndex);
    BeginIndex := EndIndex + 1;
  end;
end;

procedure DumpShapeTree(Shape: TVRMLShapeTree; const Indent: string = '');

  procedure DumpShape(Shape: TVRMLShape);
  var
    Vertexes: TDynVector3SingleArray;
    CoordIndex: TDynLongIntArray;
  begin
    if Shape.Geometry is TNodeIndexedFaceSet_1 then
    begin
      Vertexes := TNodeIndexedFaceSet_1(Shape.Geometry).Coordinates(Shape.State).Items;
      CoordIndex := TNodeIndexedFaceSet_1(Shape.Geometry).CoordIndex.Items;
    end else
    if Shape.Geometry is TNodeIndexedFaceSet then
    begin
      Vertexes := TNodeIndexedFaceSet(Shape.Geometry).Coordinates(Shape.State).Items;
      CoordIndex := TNodeIndexedFaceSet(Shape.Geometry).CoordIndex.Items;
    end else
    begin
      Writeln('Geometry: ', Shape.Geometry.NodeTypeName);
      Exit;
    end;

    Writeln(Format('Geometry: IndexedFaceSet, Vertexes count: %d, CoordIndex count: %d, Triangles count: %d',
      [Vertexes.Count, CoordIndex.Count, Shape.TrianglesCount(false)]));
  end;

  procedure DumpShapeGroup(Shape: TVRMLShapeTreeGroup);
  var
    List: TVRMLShapeTreeList;
    I: Integer;
  begin
    Writeln('Grouping shape: ', Shape.ClassName);
    List := TVRMLShapeTreeGroup(Shape).Children;
    for I := 0 to List.Count - 1 do
    begin
      Write(Indent, I:4, ': ');
      DumpShapeTree(List[I], Indent + '      ');
    end;
  end;

begin
  if Shape is TVRMLShape then
    DumpShape(TVRMLShape(Shape)) else
  if Shape is TVRMLShapeTreeGroup then
    DumpShapeGroup(TVRMLShapeTreeGroup(Shape)) else
    Writeln('Unknown shape type: ', Shape.ClassName);
end;

var
  ShapeNum: Integer;
  SceneManager: TMySceneManager;
begin
  Window := TGLUIWindow.Create(Application);

  Parameters.CheckHighAtLeast(1);
  Parameters.CheckHighAtMost(2);
  try
    OnWarning := @OnWarningWrite;

    Scene := TVRMLScene.Create(nil);
    Scene.Load(Parameters[1]);

    if Parameters.High >= 2 then
      ShapeNum := StrToInt(Parameters[2]) else
      ShapeNum := 0;

    Writeln('-------- Scene shapes tree:');
    DumpShapeTree(Scene.Shapes);
    Writeln('-------- End of scene shapes tree');

    if not (Scene.Shapes is TVRMLShapeTreeGroup) then
      raise Exception.Create('Shapes tree starts from LOD or Switch, not supported in this trivial demo');
    if ShapeNum >= TVRMLShapeTreeGroup(Scene.Shapes).Children.Count then
      raise Exception.CreateFmt('No shape number %d (means empty scene if shape number = 0)', [ShapeNum]);
    if not (TVRMLShapeTreeGroup(Scene.Shapes).Children[ShapeNum] is TVRMLShape) then
      raise Exception.Create('Specified shape is compound (LOD or Switch), not supported in this trivial demo');
    Shape := TVRMLShape(TVRMLShapeTreeGroup(Scene.Shapes).Children[ShapeNum]);

    { Get info about vertexes, coordindex from 1st shape on our scene.

      Don't even think about using it in production code, the simple
      code below omits a lot of X3D/VRML complexity just to get to
      the raw (and extremely dumbed down) vertex data. }
    if Shape.Geometry is TNodeIndexedFaceSet_1 then
    begin
      Vertexes := TNodeIndexedFaceSet_1(Shape.Geometry).Coordinates(Shape.State).Items;
      CoordIndex := TNodeIndexedFaceSet_1(Shape.Geometry).CoordIndex.Items;
    end else
    if Shape.Geometry is TNodeIndexedFaceSet then
    begin
      Vertexes := TNodeIndexedFaceSet(Shape.Geometry).Coordinates(Shape.State).Items;
      CoordIndex := TNodeIndexedFaceSet(Shape.Geometry).CoordIndex.Items;
    end else
      raise Exception.Create('Specified shape is not IndexedFaceSet');

    { create coordIndex for rendering by separate triangles }
    TrianglesCoordIndex := TDynLongIntArray.Create;
    TrianglesCoordIndex.AllowedCapacityOverflow := CoordIndex.Count;
    MakeTrianglesCoordIndex;

    Writeln('Vertexes count: ', Vertexes.Count);
    Writeln('CoordIndex count: ', CoordIndex.Count);
    Writeln('Triangles count: ', Shape.TrianglesCount(false), ' (made: ', TrianglesCoordIndex.Count div 3, ')');

    SceneManager := TMySceneManager.Create(Application);
    SceneManager.Items.Add(Scene);
    Window.Controls.Add(SceneManager);

    { to get quickier FPS update }
    Window.FpsCaptionUpdateInterval := 1000;
    Window.AutoRedisplay := true;

    Window.MainMenu := CreateMainMenu;
    Window.OnMenuCommand := @MenuCommand;

    Window.OnOpen := @Open;
    Window.OpenAndRun;
  finally
    FreeAndNil(Scene);
    FreeAndNil(TrianglesCoordIndex);
  end;
end.
