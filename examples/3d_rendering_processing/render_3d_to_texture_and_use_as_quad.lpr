{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use TGLRenderToTexture to render a 3D scene (SourceScene) to a texture,
  grab this texture to a normal memory (as TRGBImage),
  and use it as a texture for a quad inside an interactive 3D scene (RuntimeScene).

  Usage: Run this, and press an arrow key or drag with mouse -- to confirm
  that what you see is just a quad with a texture:)
  Change the line "SourceScene.Load" below to set the source 3D scene visible
  on the texture.

  You could also save the texture grabbed this way to a file.
  Or really do anything with it ---
  you can process the TRGBImage instance in any way.

  TODO:
    This could be done much more efficiently, without copying
    the image from GPU to normal memory. TGLRenderToTexture can render
    to an OpenGL texture resource TGLTextureId, but right now
    PixelTexture / ImageTexture cannot be initialized from a TGLTextureId.

    To put it in different words,
    our usage of TGLRenderToTexture in this demo is unoptimal.
    We use TGLRenderToTexture to render off-screen,
    and then we grab the texture from FBO using the unoptimal SaveScreen_NoFlush.
    Using the TGLRenderToTexture to render straight to TGLRenderToTexture.Texture
    would be much better.

  You could also use RenderTexture to make this efficiently (without
  copying the texture data to normal memory), but it would be more tricky,
  it would require setting special camera views for RenderTexture.
  Right now, RenderTexture is most suited to grab to texture a view from the same
  scene, not to render one scene to be displayed as sprite on another scene.
}

uses SysUtils, CastleWindow, CastleSceneCore, CastleScene, CastleVectors,
  CastleFilesUtils, CastleImages, CastleGLImages, CastleRectangles,
  X3DNodes;

var
  Window: TCastleWindow;

const
  TextureWidth = 1024;
  TextureHeight = TextureWidth;

{ Load and render a 3D scene to a new texture. }
function CreateSpriteTexture: TCastleImage;
var
  SourceScene: TCastleScene;
  RenderToTexture: TGLRenderToTexture;
begin
  { nil everything first, to be able to wrap in a simple try..finally clause }
  SourceScene := nil;
  RenderToTexture := nil;

  try
    SourceScene := TCastleScene.Create(nil);
    SourceScene.Load(ApplicationData(
      //'bridge_final.x3dv'
      //'car.x3d'
      'raptor_1.x3d'
    ));

    { TODO: Instead of over-using the Window.SceneManager here,
      and rendering below using the

        Window.Container.EventBeforeRender;
        Window.Container.EventRender;

      ... it would be cleaner to create and render a new SourceSceneManager,
      local to this function.
      But this requires adding a method to TGLContainer (or TRenderContext)
      to render a given TUIControl, as you cannot just call "SceneManager.Render"
      (or any other "TUIControl.Render") without the necessary preparations. }

    Window.SceneManager.Items.Add(SourceScene);
    Window.SceneManager.MainScene := SourceScene;

    RenderToTexture := TGLRenderToTexture.Create(TextureWidth, TextureHeight);
    RenderToTexture.Buffer := tbNone;
    RenderToTexture.GLContextOpen;
    RenderToTexture.RenderBegin;

    { everything rendered between RenderBegin and RenderEnd is done off-screen. }
    Window.Container.EventBeforeRender;
    Window.Container.EventRender;

    Result := SaveScreen_NoFlush(TRGBImage,
      Rectangle(0, 0, TextureWidth, TextureHeight), RenderToTexture.ColorBuffer);

    RenderToTexture.RenderEnd;
  finally
    FreeAndNil(RenderToTexture);
    FreeAndNil(SourceScene);
  end;
end;

{ Create X3D graph describing a simple scene with a quad,
  textured with given SpriteTexture.
  The SpriteTexture becomes owned by the resulting X3D nodes graph,
  do not free it yourself anymore. }
function CreateRuntimeSceneNode(const SpriteTexture: TCastleImage): TX3DRootNode;
var
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Coordinate: TCoordinateNode;
  QuadSet: TQuadSetNode;
  Texture: TPixelTextureNode;
begin
  Texture := TPixelTextureNode.Create;
  Texture.FdImage.Value := SpriteTexture;
  { clamp mode, not repeat --- avoids artifacts at the edges }
  Texture.RepeatS := false;
  Texture.RepeatT := false;

  Appearance := TAppearanceNode.Create;
  // You could assign some material to make it lit.
  // Appearance.Material := TMaterialNode.Create;
  Appearance.Texture := Texture;

  Coordinate := TCoordinateNode.Create;
  Coordinate.FdPoint.Items.AddArray(
    [Vector3Single(0, 0, 0),
     Vector3Single(1, 0, 0),
     Vector3Single(1, 1, 0),
     Vector3Single(0, 1, 0)
    ]);

  QuadSet := TQuadSetNode.Create;
  QuadSet.FdCoord.Value := Coordinate;
  QuadSet.Solid := false; // see from both sides?

  { In real usecases, you could now assing explicit texture
    coordinates to QuadSet.FdTexCoord, instead of using the default
    calculated tex coordinates. }

  Shape := TShapeNode.Create;
  Shape.Appearance := Appearance;
  Shape.Geometry := QuadSet;

  Result := TX3DRootNode.Create;
  Result.FdChildren.Add(Shape);
end;

var
  RuntimeScene: TCastleScene;
  SpriteTexture: TCastleImage;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  SpriteTexture := CreateSpriteTexture;
  // You can save the image to disk.
  // SaveImage(SpriteTexture, '/tmp/test.png');

  RuntimeScene := TCastleScene.Create(Application);
  RuntimeScene.Load(CreateRuntimeSceneNode(SpriteTexture), true);
  RuntimeScene.Spatial := [ssRendering, ssDynamicCollisions];
  RuntimeScene.ProcessEvents := true;

  Window.SceneManager.Items.Add(RuntimeScene);
  Window.SceneManager.MainScene := RuntimeScene;

  { Force recreating camera next time, to see whole RuntimeScene automatically.
    Otherwise, the current camera was already initialized to see whole
    SourceScene in CreateSpriteTexture.
    This is an unfortunate side-effect of over-using Window.SceneManager
    inside CreateSpriteTexture, see TODO there. }
  Window.SceneManager.Camera.Free;

  Application.Run;
end.
