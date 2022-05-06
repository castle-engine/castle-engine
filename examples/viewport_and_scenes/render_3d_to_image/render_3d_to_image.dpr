{
  Copyright 2016-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use Window.Container.RenderControl to render various scenes to TDrawableImage. }

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils,
  CastleWindow, CastleScene, CastleVectors, CastleLog,
  CastleFilesUtils, CastleImages, CastleRectangles, CastleGLImages,
  CastleViewport, CastleURIUtils, CastleCameras;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
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

    Viewport.Items.MainScene.Free; // free previous MainScene
    Viewport.Items.Add(Scene);
    { Setting MainScene allows Viewport to adjust
      viewpoint, headlight, background etc. based on MainScene contents. }
    Viewport.Items.MainScene := Scene;
    Viewport.AssignDefaultCamera;

    Window.Container.RenderControl(Viewport,
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
      WritelnLog('Saved ', ImageUrl);
    finally FreeAndNil(DestImage) end;
  end;
end;

begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  { Make the window invisible, as it will only be used as OpenGL context
    for off-screen rendering in this demo.

    Note: Not all CastleWindow backends support the Visible:=false option,
    some backends will just keep showing the window. }
  Window.Visible := false;

  { Create viewport (a 2D control that shows 3D world).

    Note that calling manually Viewport.GLContextOpen would not be necessary
    if Viewport was already part of Window.Controls.
    In this example, I wanted to show more general approach, that works
    regardless if the control passed to Window.Container.RenderControl
    is present on "Window.Controls" list.

    Note that you *could* create new TCastleViewport inside
    each RenderScene, instead of reusing one Viewport instance.
    But then it may be slower, as TCastleViewport.GLContextOpen
    will be called many times.
    Although right now, TCastleViewport.GLContextOpen
    doesn't do anything, so this optimization actually doesn't matter.
  }

  Viewport := TCastleViewport.Create(Application);
  Viewport.AutoCamera := false;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Viewport.FullSize := false;
  Viewport.Width := ImageWidth;
  Viewport.Height := ImageHeight;
  Viewport.GLContextOpen;

  { Create an image as a destination for all off-screen rendering.

    By reusing the same Image instance for all RenderScene calls, we make it faster.
    The image data, as well as FBO, is created once,
    instead of for each RenderScene call.
    But it would also be correct (just slower) to create new TDrawableImage in each
    RenderScene call. }
  Image := TDrawableImage.Create(
    TRGBAlphaImage.Create(ImageWidth, ImageHeight), true, true);
  try
    RenderScene('castle-data:/car.gltf');
    RenderScene('castle-data:/teapot.x3dv');
  finally FreeAndNil(Image) end;
end.
