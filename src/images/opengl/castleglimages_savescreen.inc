{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: saving screen. }

{$ifdef read_interface}

type
  TColorBuffer = (
    cbFront,
    cbBack,
    cbColorAttachment0
  );

{ Notes about saving images from cbFront buffer:

  Don't do it. It just not defined what will be returned when you read from
  the front buffer. When our OpenGL context is covered by some other window,
  then glReadPixels *may* return pixels with contents of obscuring window.
  It doesn't help to draw right before trying to save buffer contents,
  reading from front buffer is just not reliable.

  The only reliable way to save screen contents is to draw something to back
  buffer and (without doing any swapbuffers) read it from cbBack buffer.
  This is only possible if you have double-buffered window, of course.
}

{ Save the current color buffer contents to image.

  The suffix "NoFlush" is there to remind you that this
  function grabs the @italic(current) buffer contents. Usually you want to
  redraw the screen to the back buffer, and call this function to capture
  back buffer @italic(before) swapping, since this is the only reliable
  way to capture OpenGL screen.
  Just use TCastleWindowCustom.SaveScreen to do it automatically.

  Version with ImageClass can save to any image format from PixelsImageClasses.

  Version with TCastleImage instance just uses this instance to save the image.
  You must pass here already created TCastleImage instance, it's class,
  Width and Height will be used when saving.

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
function SaveScreen_NoFlush(
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TRGBImage; overload;

function SaveScreen_NoFlush(const ImageClass: TCastleImageClass;
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TCastleImage; overload;

procedure SaveScreen_NoFlush(const Image: TCastleImage;
  const Left, Bottom: Integer; const ReadBuffer: TColorBuffer); overload;
{ @groupEnd }

{ Captures current screen as a TGLImageCore instance, ready to be drawn on 2D screen. }
function SaveScreenToGL_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer;
  const SmoothScaling: boolean = false): TGLImageCore;

{$endif read_interface}

{$ifdef read_implementation}

const
  ColorBufferGL: array [TColorBuffer] of TGLenum = (
    GL_FRONT,
    GL_BACK,
    GL_COLOR_ATTACHMENT0
  );

{ This is the basis for all other SaveScreen* functions below. }
procedure SaveScreen_NoFlush(const Image: TCastleImage;
  const Left, Bottom: Integer; const ReadBuffer: TColorBuffer);

  procedure ReadPixels(const Image: TCastleImage);
  begin
    BeforePackImage(Image);
    try
      SetReadBuffer(ColorBufferGL[ReadBuffer]);
      glReadPixels(Left, Bottom, Image.Width, Image.Height, ImageGLFormat(Image),
        ImageGLType(Image), Image.RawPixels);
    finally AfterPackImage(Image) end;
  end;

{$ifdef OpenGLES}

{ Under OpenGLES, the only type+format combination that is guaranteed
  to work is GL_RGBA + GL_UNSIGNED_BYTE,
  see http://www.khronos.org/opengles/sdk/docs/man/xhtml/glReadPixels.xml .
  So grab image to TRGBAlphaImage, and later convert it as necessary. }

var
  ImageRGBA: TRGBAlphaImage;
begin
  if Image is TRGBAlphaImage then
    ReadPixels(Image) else
  begin
    ImageRGBA := TRGBAlphaImage.Create(Image.Width, Image.Height, Image.Depth);
    try
      ReadPixels(ImageRGBA);
      Image.Assign(ImageRGBA);
    finally FreeAndNil(ImageRGBA) end;
  end;
{$else}
begin
  ReadPixels(Image);
{$endif}
end;

function SaveScreen_NoFlush(const ImageClass: TCastleImageClass;
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TCastleImage;
begin
  Result := ImageClass.Create(Rect.Width, Rect.Height);
  try
    Result.URL := 'save-screen';
    SaveScreen_NoFlush(Result, Rect.Left, Rect.Bottom, ReadBuffer);
  except Result.Free; raise end;
end;

function SaveScreen_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer): TRGBImage;
begin
  Result := SaveScreen_NoFlush(TRGBImage, Rect, ReadBuffer) as TRGBImage;
end;

function SaveScreenToGL_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer;
  const SmoothScaling: boolean): TGLImageCore;
var
  ScreenImage: TRGBImage;
begin
  ScreenImage := SaveScreen_NoFlush(Rect, ReadBuffer);
  try
    Result := TGLImageCore.Create(ScreenImage, SmoothScaling);
  finally FreeAndNil(ScreenImage) end;
end;

{$endif read_implementation}
