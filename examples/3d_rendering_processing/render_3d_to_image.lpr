{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use Window.Container.RenderControl to render various scenes to TDrawableImage. }

uses SysUtils,
  CastleWindow, CastleScene, CastleVectors,
  CastleFilesUtils, CastleImages, CastleRectangles, CastleGLImages,
  CastleSceneManager, CastleURIUtils;

var
  Window: TCastleWindowBase;
  SceneManager: TCastleSceneManager;
  Image: TDrawableImage;

const
  ImageWidth = 1024;
  ImageHeight = ImageWidth; //< Square image.

procedure RenderScene(const Url: string);
var
  Scene: TCastleScene;
  DestImage: TCastleImage;
  ImageUrl: string;
begin
  Image.RenderToImageBegin;

  Scene := TCastleScene.Create(nil);
  try
    Scene.Load(Url);

    SceneManager.Items.Clear;
    SceneManager.Items.Add(Scene);
    { Setting MainScene allows SceneManager to adjust
      viewpoint, headlight, background etc. based on MainScene contents. }
    SceneManager.MainScene := Scene;
    { Freeing camera each time, forces recreating best camera at render. }
    SceneManager.Camera.Free;

    Window.Container.RenderControl(SceneManager,
      Rectangle(0, 0, ImageWidth, ImageHeight));
  finally FreeAndNil(Scene) end;

  Image.RenderToImageEnd;

  { For demo, grab the contents of Image:TDrawableImage to normal memory.
    Note that *this will always be slow* (grabbing contents from GPU->CPU
    is always slow). In real applications, it is best to hold on to TDrawableImage
    instance, and use it for rendering directly,
    e.g. by TCastleImageControl.DrawableImage,
    or simply by drawing it later with TDrawableImage.Draw. }
  if not Application.OpenGLES then
  begin
    DestImage := Image.GetContents(TRGBAlphaImage);
    try
      ImageUrl := ChangeURIExt(ExtractURIName(Url), '.png');
      SaveImage(DestImage, ImageUrl);
      Writeln('Saved ', ImageUrl);
    finally FreeAndNil(DestImage) end;
  end;
end;

begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  { Make the window invisible, as it will only be used as OpenGL context
    for off-screen rendering in this demo.

    Note: Not all CastleWindow backends support the Visible:=false option,
    some backends will just keep showing the window. }
  Window.Visible := false;

  { Create scene manager, which is also a viewport by default,
    so it is a 2D control that shows 3D world.

    Note that in this simple demo, we could as well create
    TCastleWindow instead of TCastleWindowBase.
    And then
    - Instead of creating "TCastleSceneManager.Create",
      we could have just used ready Window.SceneManager.
    - There would be no point in calling SceneManager.GLContextOpen then.
      The scene manager on "Window.SceneManager" is present on
      "Window.Controls" list, and as such it already has GL resources initialized
      (GLContextOpen was already called on it).

    However, I'm showing below an alternative way where we create
    TCastleSceneManager explicitly, to emphasize that the control passed
    to Window.Container.RenderControl does *not* need to be present
    on "Window.Controls" list.

    Note that you *could* create new TCastleSceneManager inside
    each RenderScene, instead of reusing one SceneManager instance.
    But then it may be slower, as TCastleSceneManager.GLContextOpen
    will be called many times.
    Although right now, TCastleSceneManager.GLContextOpen
    doesn't do anything, so this optimization actually doesn't matter.
  }

  SceneManager := TCastleSceneManager.Create(Application);
  SceneManager.FullSize := false;
  SceneManager.Width := ImageWidth;
  SceneManager.Height := ImageHeight;
  SceneManager.GLContextOpen;

  { Create an image as a destination for all off-screen rendering.

    By reusing the same Image instance for all RenderScene calls, we make it faster.
    The image data, as well as FBO, is created once,
    instead of for each RenderScene call.
    But it would also be correct (just slower) to create new TDrawableImage in each
    RenderScene call. }
  Image := TDrawableImage.Create(
    TRGBAlphaImage.Create(ImageWidth, ImageHeight), true, true);
  try

    RenderScene('castle-data:/boxes.x3dv');
    RenderScene('castle-data:/bridge_final.x3dv');
    RenderScene('castle-data:/car.x3d');
    RenderScene('castle-data:/teapot.x3dv');

  finally FreeAndNil(Image) end;
end.
