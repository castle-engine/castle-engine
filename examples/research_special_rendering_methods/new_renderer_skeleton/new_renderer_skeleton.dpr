{
  Copyright 2017-2019, 2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A skeleton how you can test a new rendering target implementation
  (like Metal or Vulkan or Direct3D).

  In the future, Castle Game Engine should allow to "plug" a new renderer,
  such that the underlying renderer can be switched at runtime,
  and the public engine API stays the same.
  So all the engine classes (like TCastleScene, TCastleViewport, TCastleWindow,
  TCastleButton, TDrawableImage...) will work, regardless of the renderer.

  Also, you will then no longer need to initialize some semi-internal things
  shown in this program: initializing Params, ProjectionMatrix, RenderingCamera
  should be done automatically by TCastleViewport (once TCastleViewport
  is independent from OpenGL).

  In the meantime, you can use this approach, which creates a descendant
  of TCastleSceneCore, to implement a new renderer.

  This approach should allow you to quickly test whether your renderer works.
  Once we have an alternative renderer working, Michalis promises to quickly
  implement an architecture to comfortably "plug" your renderer
  to Castle Game Engine!
}

uses SysUtils, TypInfo, Classes,
  CastleVectors, CastleCameras, X3DNodes, CastleSceneCore, CastleShapes,
  CastleTransform, CastleProjection, CastleFrustum, CastleViewport, CastleScene,
  CastleInternalGeometryArrays;

var
  { Projection, as a trivial global variable. }
  ProjectionMatrix: TMatrix4;

{ Vulkan shape and scene ----------------------------------------------------- }

type
  { In this class you can store Vulkan-specific information that is per-shape.
    Inside the TCastleSceneVulkan implementation,
    you know that all your shapes are descendants of TVulkanShape. }
  TVulkanShape = class(TShape)
  end;

  { Scene that can be rendered using Vulkan. }
  TCastleSceneVulkan = class(TCastleSceneCore)
  protected
    function CreateShape(const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState;
      const ParentInfo: PTraversingInfo): TShape; override;
  public
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const Params: TPrepareParams); override;
    procedure LocalRender(const Params: TRenderParams); override;
  end;

function TCastleSceneVulkan.CreateShape(const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState; const ParentInfo: PTraversingInfo): TShape;
begin
  Result := TVulkanShape.Create(Self, AGeometry, AState, ParentInfo);
end;

procedure TCastleSceneVulkan.PrepareResources(
  const Options: TPrepareResourcesOptions;
  const Params: TPrepareParams);
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  ShapeList := Shapes.TraverseList(false, false);
  for Shape in ShapeList do
  begin
    Writeln('Prepare to render shape: ', Shape.NiceName);

    { TODO: Load Shape data to GPU now
      (you can cast Shape to TShapeVulkan as necessary).

      - E.g. load geometry data (Shape.GeometryArrays) to VBO.
        You should call Shape.GeometryArrays.FreeData afterwards,
        to not keep the data on CPU anymore.

      - E.g. load textures to GPU.

      - You should be prepared that some data may be already loaded.
        So all the loading should look like

          if not Shape.SomethingLoaded then
          begin
            Shape.SomethingLoaded := true;
            // load something here ...
          end;

      For a first renderer test you can also instead load on-demand
      from the LocalRender implementation.
    }

  end;
end;

procedure TCastleSceneVulkan.LocalRender(const Params: TRenderParams);

  function GetSceneModelView: TMatrix4;
  begin
    Result := Params.RenderingCamera.CurrentMatrix * Params.Transform^;
  end;

  function PrimitiveToStr(const Primitive: TGeometryPrimitive): string;
  begin
    Result := GetEnumName(TypeInfo(TGeometryPrimitive), Ord(Primitive));
  end;

var
  SceneModelView, ShapeModelView: TMatrix4;
  ShapeList: TShapeList;
  Shape: TShape;
  GeometryArrays: TGeometryArrays;
begin
  SceneModelView := GetSceneModelView;
  ShapeList := Shapes.TraverseList(true, true);
  for Shape in ShapeList do
  begin
    ShapeModelView := SceneModelView * Shape.State.Transformation.Transform;
    Writeln('Rendering shape: ', Shape.NiceName);
    Writeln('Projection matrix:');
    Writeln(ProjectionMatrix.ToString('    '));
    Writeln('Modelview matrix: ');
    Writeln(ShapeModelView.ToString('    '));

    GeometryArrays := Shape.GeometryArrays;
    try
      Writeln('Geometry:',
        ' Primitive: ', PrimitiveToStr(GeometryArrays.Primitive),
        ', HasIndexes: ',  GeometryArrays.HasIndexes,
        ', IndexesCount: ', GeometryArrays.IndexesCount,
        ', Count: ', GeometryArrays.Count);

      { TODO: Render Shape here (you can cast it to TShapeVulkan as necessary).
        Load Shape.GeometryArrays to GPU,
        and pass parameters (like projection and modelview matrix) to shaders.
      }
    finally FreeAndNil(GeometryArrays) end;
  end;
end;

{ Vulkan application and window ---------------------------------------------- }

type
  TVulkanApplication = class(TComponent)
  public
    // TODO: Process some inputs in TVulkanWindow, allow setting this to true.
    Quit: boolean;
  end;

  TVulkanWindow = class(TComponent)
  public
    Width, Height: Integer;
    procedure Open;
  end;

procedure TVulkanWindow.Open;
begin
  // TODO: create the Vulkan context, show the window
end;

{ initialization ------------------------------------------------------------- }

var
  Application: TVulkanApplication;
  Window: TVulkanWindow;
  Viewport: TCastleViewport;
  Scene: TCastleSceneVulkan;
  Camera: TCastleCamera;
  RenderParams: TRenderParams;
begin
  Application := TVulkanApplication.Create(nil);
  try
    Window := TVulkanWindow.Create(Application);
    Window.Width := 1024;
    Window.Height := 768;
    Window.Open;

    Viewport := TCastleViewport.Create(Application);
    Viewport.FullSize := true;
    //Window.Controls.InsertFront(Viewport);

    Camera := Viewport.Camera;
    Camera.SetWorldView(
      Vector3(0, 0, 0), // position
      Vector3(0, 0, -1), // direction
      Vector3(0, 0, 1) // up
    );

    Scene := TCastleSceneVulkan.Create(Application);
    Scene.Load('castle-data:/car.gltf');
    Scene.PrepareResources([], nil);

    { Prepare rendering parameters
      (this is done by TCastleViewport in normal circumstances).

      Note: Creating and using TRenderParams instance (or any descendant of it)
      means that you deal with internal stuff.

      - The API of TRenderParams is internal, it may change at any moment.

      - You should not construct or modify TRenderParams in a normal CGE application.
        In normal application, TCastleViewport prepares TRenderParams instance,
        and TCastleScene uses it.
        How the TRenderParams is prepared, and how is it used -- it's
        internal for normal applications.
    }
    RenderParams := TBasicRenderParams.Create;
    RenderParams.RenderingCamera := TRenderingCamera.Create;
    RenderParams.RenderingCamera.Target := rtScreen;
    RenderParams.RenderingCamera.FromCameraObject(Camera);
    RenderParams.Frustum := @RenderParams.RenderingCamera.Frustum;

    while not Application.Quit do
    begin
      { TODO: Clear the screen contents (color, depth) now. }

      { Prepare projection
        (this is done by TCastleViewport in normal circumstances). }
      ProjectionMatrix := PerspectiveProjectionMatrixDeg(
        60, Window.Width / Window.Height, 0.1, 1000);

      { In a real rendering, Scene.Render may be called more than once
        per frame, with different values of
        RenderParams.Transparent and RenderParams.ShadowVolumesReceivers,
        that filter various shapes.
        You can temporarily ignore this issue (until you will want to
        support blending (partial transparency) in your renderer). }

      // Render the Scene
      Scene.Render(RenderParams);

      { TODO: do something like Window.Flush or Window.SwapBuffers,
        to make sure GPU will execute the rendering commands ASAP. }

      { Testing: Wait for a key press, give user's a chance to press Ctrl + C :) }
      Readln;
    end;
  finally
    if RenderParams <> nil then
      FreeAndNil(RenderParams.RenderingCamera);
    FreeAndNil(RenderParams);
    FreeAndNil(Application);
  end;
end.
