{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ Called when first OpenGL context opens. }
function OnGLContextOpen: TGLContextEventList; deprecated 'use ApplicationProperties.OnGLContextOpen';
{ Called when last OpenGL context closes. }
function OnGLContextClose: TGLContextEventList; deprecated 'use ApplicationProperties.OnGLContextClose';
{ Is any OpenGL context open. }
function IsGLContextOpen: boolean; deprecated 'use ApplicationProperties.IsGLContextOpen';

const
  prLeft = prLow deprecated;
  prRight = prHigh deprecated;
  prBottom = prLow deprecated;
  prTop = prHigh deprecated;

  hpLeft   = CastleRectangles.hpLeft  ;
  hpMiddle = CastleRectangles.hpMiddle;
  hpRight  = CastleRectangles.hpRight ;
  vpBottom = CastleRectangles.vpBottom;
  vpMiddle = CastleRectangles.vpMiddle;
  vpTop    = CastleRectangles.vpTop   ;

type
  { Internal for communication with CastleWindow or CastleControl.
    See @link(GetMainContainer).
    @exclude }
  TOnMainContainer = function: TCastleContainer of object;

var
  { Internal for communication with CastleWindow or CastleControl.
    You can set this, but read it only through GetMainContainer function.
    See @link(GetMainContainer).
    @exclude }
  OnMainContainer: TOnMainContainer = nil;

{ Internal for communication with CastleWindow or CastleControl.
  Returns the main UI container.
  It may be used in rare situations when the UI control may not be added
  to the container yet, but it needs to know the container.

  Returns @nil if OnMainContainer not assigned (possible if you don't use
  CastleWindow or CastleControl), or OnMainContainer returns @nil
  (possible if you don't set Application.MainWindow or TCastleControl.MainControl).

  @exclude }
function GetMainContainer: TCastleContainer;

{ Render control contents to an RGBA image, using off-screen rendering.
  The background behind the control is filled with BackgroundColor
  (which may be transparent, e.g. with alpha = 0).

  The rendering is done using off-screen FBO.
  Which means that you can request any size, you are @bold(not) limited
  to your current window / control size.

  Make sure that the control is nicely positioned to fill the ViewportRect.
  Usually you want to adjust control size and position,
  and disable UI scaling (set TCastleUserInterface.EnableUIScaling = @false
  if you use TCastleContainer.UIScaling).

  This is the @italic(easiest) way to make off-screen rendering,
  i.e. to render 3D things (like TCastleScene in TCastleViewport)
  into an image. This is @italic(not the fastest way), as it creates
  new TGLRenderToTexture instance each time,
  and it grabs the image contents to CPU.
  If you want a faster approach, use @link(TCastleContainer.RenderControl)
  and render into @link(TDrawableImage) using @link(TDrawableImage.RenderToImageBegin)
  and @link(TDrawableImage.RenderToImageEnd).
}
function RenderControlToImage(const Container: TCastleContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage; overload;

{$endif read_interface}

{$ifdef read_implementation}

function OnGLContextOpen: TGLContextEventList;
begin
  Result := ApplicationProperties.OnGLContextOpen;
end;

function OnGLContextClose: TGLContextEventList;
begin
  Result := ApplicationProperties.OnGLContextClose;
end;

function IsGLContextOpen: boolean;
begin
  Result := ApplicationProperties.IsGLContextOpen;
end;

function RenderControlToImage(const Container: TCastleContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;

  function CreateTargetTexture(const W, H: Integer): TGLTextureId;
  var
    InitialImage: TCastleImage;
  begin
    InitialImage := TRGBAlphaImage.Create(W, H);
    try
      InitialImage.URL := 'generated:/temporary-render-to-texture';
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));
      Result := LoadGLTexture(InitialImage,
        TextureFilter(minNearest, magNearest),
        Texture2DClampToEdge, nil, true);
    finally FreeAndNil(InitialImage) end;
  end;

var
  W, H: Integer;
  RenderToTexture: TGLRenderToTexture;
  TargetTexture: TGLTextureId;
begin
  W := ViewportRect.Width;
  H := ViewportRect.Height;
  RenderToTexture := TGLRenderToTexture.Create(W, H);
  try
    // RenderToTexture.Buffer := tbNone;
    // RenderToTexture.ColorBufferAlpha := true;

    RenderToTexture.Buffer := tbColor;
    TargetTexture := CreateTargetTexture(W, H);
    RenderToTexture.SetTexture(TargetTexture, GL_TEXTURE_2D);
    RenderToTexture.GLContextOpen;
    RenderToTexture.RenderBegin;

    { actually render }
    RenderContext.Clear([cbColor], BackgroundColor);
    Container.RenderControl(Control, ViewportRect);

    RenderToTexture.RenderEnd;

    Result := TRGBAlphaImage.Create(W, H);
    SaveTextureContents(Result, TargetTexture);

    RenderToTexture.GLContextClose;

    glFreeTexture(TargetTexture);
  finally FreeAndNil(RenderToTexture) end;
end;

function GetMainContainer: TCastleContainer;
begin
  if Assigned(OnMainContainer) then
    Result := OnMainContainer()
  else
    //raise Exception.Create('OnMainContainer not assigned. Use CastleWindow or CastleControl unit before starting TCastleView');
    // returning nil is more flexible to the caller
    Result := nil;
end;

procedure InitializeInputInspectorDebug;
begin
  TCastleContainer.InputInspector.Key := keyF8;
  TCastleContainer.InputInspector.PressFingers := 3;
end;

procedure InitializeInputInspector;
begin
  TCastleContainer.FInputInspector := TInputInspector.Create(nil);
  TCastleContainer.InputInspector.Key := keyNone;
  TCastleContainer.InputInspector.KeyModifiers := [];
  TCastleContainer.InputInspector.PressFingers := 0;
  TCastleContainer.InputInspector.PressTime := 1;

  ApplicationProperties.AddInitializeDebugListener(
    {$ifdef FPC}@{$endif} InitializeInputInspectorDebug);
end;

procedure FinalizeInputInspector;
begin
  FreeAndNil(TCastleContainer.FInputInspector);
end;

{$endif read_implementation}
