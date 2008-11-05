{
  Copyright 2001-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Using images in OpenGL (as textures and as normal images).

  This unit implements various OpenGL utilities relates to handling images
  in TImage classes. This includes
  @unorderedList(
    @item(Loading TImage instance as OpenGL texture.
      Wrapper around glTexImage2D and other texture-related operations.
      See LoadGLTexture.)

    @item(Drawing TImage instance in OpenGL buffer.
      Wrapper around glDrawPixels and related things.
      See ImageDraw.)

    @item(Screen saving, that is saving OpenGL buffer contents to TImage instance.
      Wrapper around glReadPixels and related things.
      See TGLWindow.SaveScreen, based on SaveScreen_noflush in this unit.)
  )

  See @link(Images) unit for functions to load, save, process
  images. Images unit is the non-OpenGL-related helper of this unit.

  This unit hides from you some specifics of OpenGL images handling :

  @unorderedList(
    @item(
      Don't worry about pixel store alignment, this unit handles it for you.

      Since internally our image formats have no alignment, we call
      something like
      @preformatted(
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
)
      when appropriate. Actually, we use @code(Before/After Pack/Unpack NotAlignedImage)
      procedures from KambiGLUtils unit. )

    @item(
      Don't worry about texture size being power of 2, or about maximum texture
      size.

      This unit checks OpenGL capabilities, and if needed will scale your
      texture to have power of 2 sizes that fit within OpenGL
      GL_MAX_TEXTURE_SIZE limit.

      @bold(Notes about power-of-two constraint:) Newer OpenGL allows using
      non-power-of-two textures when extension ARB_texture_non_power_of_two
      is supported (or version is >= 2.0). We can use this --- the code is
      ready, just uncomment it in TextureNonPowerOfTwo.
      But by default we don't use this, that is: we will scale textures
      to be power of 2, even if we have ARB_texture_non_power_of_two or
      new OpenGL.

      Reason: well, looks like every vendor screwed it up. Embarassing.

      @unorderedList(
        @item(On Mesa, small artifacts occur (strange cracks appears on
          non-power-of-2 texture, see kambi_vrml_test_suite/inlined_textures.wrl).
          That's the @italic(best) result compared to other vendors, see below.)

        @item(On ATI (fglrx on Linux on Mac Book Pro),
          the extension is not present and OpenGL 2.0 entry points
          are not fully present. (Although GL_VERSION claims 2.1 version,
          glBlendEquationSeparate entry point from GL 2.0 is not present.)
          For safety, I just assume that ATI is not able to make OpenGL 2.0,
          so no extension and no 2.0 means textures must be power of two.

          Just for kicks, I tried anyway to pass texture 4x3 (after all,
          GL_VERSION claims 2.1 so this should be supported), and... uhh...
          libGL segfaulted. Congrats, ATI.)

        @item(I thought: Mesa is always buggy and Radeon should not be treated
          as OpenGL 2.0, so I can live with this. Now let's try
          great NVidia, they will for sure do this right. Well, yes, it works
          correctly on NVidia (GeForce FX 5200)... but the slowdown is
          enormous. For trivial box with 4x3 texture (see
          kambi_vrml_test_suite/inlined_textures.wrl), that normally runs with
          virtually infinite speed, suddenly the speed becomes like 1 frame per second !
          Other example when the slowdown is enormous: castle/levels/castle_hall.wrl

          You can test yourself (with view3dscene using
          kambi_vrml_test_suite/inlined_textures.wrl model;
          just compile view3dscene to use non-power-of-2 textures;
          contact me if you want a binary compiled as such for testing.)

          Such slowdown is not acceptable, I prefer to loose texture quality
          by scaling them to powers of 2 in this case...)
      )
    )
  )

  Internally, this unit depends on the knowledge on how pixels are stored
  in TRGBImage and similar classes. For example we know that
  TRGBImage stores image in format that OpenGL would call "GL_RGB
  using GL_UNSIGNED_BYTE, without any alignment". Which means that
  Image.RGBPixels is a pointer to array like
  @code(packed array[0..Image.Height - 1,  0..Image.Width - 1] of TVector3Byte).
  So we have rows of TVector3Byte structures, stored from lowest row to
  highest row.
}
unit GLImages;

{$I openglmac.inc}

interface

uses GL, GLU, GLExt, Images, VectorMath, KambiGLUtils, Videos;

const
  { All routines in this unit that take TImage paramater
    accept only TImage descendants enumerated here.
    Note that *not everywhere* this is checked (especially if you
    compile with -dRELEASE) so just be sure that you're always passing
    only TImage instances of correct class (e.g. using
    InImageClasses(MyImage, GLImageClasses)). }
  GLImageClasses: array [0..3] of TImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

{ These functions return appropriate GL_xxx format and type
  for given TImage descendant. If you will pass here Img
  that is not a descendant of one of GLImageClasses,
  they will return GL_INVALID_ENUM.

  Note that OpenGL does not guarantee that GL_INVALID_ENUM <> GL_RGB, GL_RGBA
  etc. (even if every OpenGL implementation has constants defined that in a way
  that satisfies this). So better to not assume that instead of
  checking InImageClasses(MyImage, GLImageClasses)
  you can simply check ImageGLFormat(MyImage) <> GL_INVALID_ENUM.

  (But this fact can be used to make routines in this unit like
  ImageDraw work faster, because I don't guarantee anywhere that
  ImageDraw will check at runtime that passed Image has class
  in GLImageClasses. So ImageDraw simply passes to OpenGL values
  returned by ImageGLFormat/Type, so in case of incorrect
  Image class OpenGL will get GL_INVALID_ENUM. Since it's not guaranteed
  that GL_INVALID_ENUM <> GL_RGB etc., it's not guaranteed that OpenGL
  will singal error, but it was never guaranteed that ImageDraw will
  signal some error in this case.

  So this way ImageDraw does not do any checks using GLImageFormats,
  even when compiled with -dDEBUG. Everything is done in OpenGL.
  And, in practice, current OpenGL implementations *will* signal errors
  so things are checked.).

  @groupBegin }
function ImageGLFormat(const Img: TImage): TGLenum;
function ImageGLType(const Img: TImage): TGLenum;
{ @groupEnd }

{ Loading images ------------------------------------------------------------- }

{ This calls @link(Images.LoadImage) and creates a display-list with
  an ImageDraw call for this image.
  Image will be loaded with AllowedImageClasses = LoadAsClass and
  ForbiddenConvs = LoadForbiddenConvs, see @link(Images.LoadImage)
  for description what these parameters mean.
  LoadAsClass may contain only classes present in GLImageClasses. }
function LoadImageToDisplayList(const FileName: string;
  const LoadAsClass: array of TImageClass;
  const LoadForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TGLuint; overload;

{ Draws the image as 2D on screen.
  This calls OpenGL glDrawPixels command on this image.

  Don't worry about OpenGL's UNPACK_ALIGNMENT,
  we will take care here about this
  (changing it and restoring to previous value if necessary). }
procedure ImageDraw(const Image: TImage);

{ Same as @link(ImageDraw), but will draw only RowsCount rows
  starting from Row0. }
procedure ImageDrawRows(const Image: TImage; Row0, RowsCount: integer);

{ Same as @link(ImageDraw), but will omit 1st CutX columns (1st from the left)
  and 1st CutY rows (1st from down).

  This is implemented using
  glPixelStorei(GL_UNPACK_ROW_LENGTH / GL_UNPACK_SKIP_PIXELS /
  GL_UNPACK_SKIP_ROWS). So it works fast.
  Don't worry, we will take care here of changing (and later restoring
  to previous values) such unpack settings, just like GL_UNPACK_ALIGNMENT. }
procedure ImageDrawCutted(const image: TImage; cutx, cuty: cardinal);

{ This creates new display list with a call to ImageDraw(Img) inside. }
function ImageDrawToDisplayList(const img: TImage): TGLuint;

{ Saving screen to TRGBImage ----------------------------------- }

{ Note about saving images from GL_FRONT:
  in general, it's not predictable to save image from GL_FRONT OpenGL buffer
  (or any part of front buffer). That's because when our window will
  be covered by other window (of other programs or our own window
  (like other instances of TGLWindow or dialog windows produced
  by TGLWindow.FileDialog, in case you use GLWindow unit)) then
  glReadPixels will return pixel array filled with contents of
  *those other windows*.

  Prefixing functions below, SaveScreen_noflush, with things like
    TGLWindow.FlushRedisplay, or even
    TGLWindow.PostRedisplay + TGLWindow.FlushRedisplay, or even
    an explicit call to Draw procedure and an explicit call
      to SwapBuffers / glFlush, or oven
    only an explicit call to Draw procedure (without glFlush/swapbuffers)
  ... DOES NOT help. If we are covered by some other
  window, glReadPixels on front buffer will simply return invalid
  contents.

  This means that the only really reliable way to save screen contents
  is to draw something to BACK buffer and (without doing any swapbuffers)
  do SaveScreen_noflush(GL_BACK) (where ReadBuffer may be some part of back
  buffer, not necessarily only simple GL_BACK). This is only possible
  if you have double-buffered window, of course.
}

{ Saves the current color buffer contents
  to an image file or to TRGBImage object.
  Sidenote: useful function to generate image
  filename for game screenshots is @link(FnameAutoInc) in @link(KambiUtils)
  unit.

  It does glReadBuffer(ReadBuffer) and then glReadPixels
  with appropriate parameters. In case of overloaded version
  that takes a FileName, it then saves image to file using @link(SaveImage).

  It has such strange name (_noflush) to remind you that this
  function does not do anything like @link(TGLWindow.FlushRedisplay)
  but you should usually take care of doing something like that
  before saving contents of OpenGL front buffer. In other words,
  remember that this function saves the *current* contents of
  color buffer -- so be sure that it contains what you want
  before using this function.

  Note that you can pass here any ReadBuffer value allowed by
  glReadBuffer OpenGL function).

  Version with ImageClass can save to any image format from GLImageClasses.

  @groupBegin }
procedure SaveScreen_noflush(const FileName: string; ReadBuffer: TGLenum); overload;
function SaveScreen_noflush(ReadBuffer: TGLenum): TRGBImage; overload;
function SaveScreen_noflush(xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TRGBImage; overload;

function SaveScreen_noflush(
  ImageClass: TImageClass;
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TImage; overload;
{ @groupEnd }

{ Saves the current color buffer (captured like
  @link(SaveScreen_noflush)) into the display list to redraw it.
  That is, it returns newly created display list that contains
  call to ImageDraw on a captured image.

  @groupBegin }
function SaveScreenToDisplayList_noflush(ReadBuffer: TGLenum): TGLuint; overload;
function SaveScreenToDisplayList_noflush(xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TGLuint; overload;
{ @groupEnd }

{ ----------------------------------------------------------------------
  Adjusting image size to load them as textures. }

{ Resizes the image to a size accepted as texture size for OpenGL.
  It tries to resize to larger size, not smaller, to avoid losing image
  information. Usually you don't have to call this, LoadGLTexture*
  functions call it automatically when needed.

  @groupBegin }
procedure ResizeForTextureSize(var r: TImage);
function ResizeToTextureSize(const r: TImage): TImage;
{ @groupEnd }

{ Tests if texture has proper size for OpenGL, that is for passing
  it to glTexImage2d. This checks glGet(GL_MAX_TEXTURE_SIZE),
  so requires initialized OpenGL context. }
function IsTextureSized(const r: TImage): boolean;

{ Loading textures ----------------------------------------------------------- }

{ Load new texture. It generates new texture number by glGenTextures.
  This takes care of UNPACK_ALIGNMENT (if needed, we'll change it and
  later revert back, so that the texture is correctly loaded).

  If you omit WrapS / WrapT parameters then they will not be set
  (so default OpenGL values will be used, since we always initialize
  new texture here).

  Changes currently bound texture to this one (returned).

  GrayscaleIsAlpha is meaningful only if the image is TGrayscaleImage class.
  If GrayscaleIsAlpha is @false, then we'll load GL_LUMINANCE texture
  (this basically behaves like normal RGB texture, except that it has
  only one channel and stores grayscale colors). If GrayscaleIsAlpha is @true,
  the texture will be loaded as GL_ALPHA texture (it will modify only the
  fragments alpha value, it doesn't have any "color" in the normal sense,
  it's only for opacity).

  @groupBegin }
function LoadGLTexture(const image: TImage; minFilter, magFilter: TGLenum;
  GrayscaleIsAlpha: boolean = false): TGLuint; overload;
function LoadGLTexture(const image: TImage;
  minFilter, magFilter, WrapS, WrapT: TGLenum;
  GrayscaleIsAlpha: boolean = false): TGLuint; overload;
function LoadGLTexture(const FileName: string;
  minFilter, magFilter, WrapS, WrapT: TGLenum;
  GrayscaleIsAlpha: boolean = false): TGLuint; overload;
{ @groupEnd }

{ Load texture into already reserved texture number.

  Besides this, works exactly like LoadGLTexture.
  If you omit WrapS / WrapT parameters then they will not be set.
  Changes currently bound texture to TexNum.

  You can use this to set "default unnamed OpenGL texture" parameters
  by passing TexNum = 0.

  @groupBegin }
procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TImage;
  minFilter, magFilter, wrapS, wrapT: TGLenum;
  GrayscaleIsAlpha: boolean = false); overload;
procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TImage;
  minFilter, magFilter: TGLenum;
  GrayscaleIsAlpha: boolean = false); overload;
{ @groupEnd }

{ As LoadGLTexture, but the texture will be modified using ColorModulatorByte.
  If not Assigned(ColorModulatorByte) then this will simply return
  LoadGLTexture(Image, MinFilter, MagFilter, WrapS, WrapT).
  Else it will return
  LoadGLTexture(ImageModulated(Image), MinFilter, MagFilter, WrapS, WrapT)
  (without introducing any memoty leaks). }
function LoadGLTextureModulated(const Image: TImage;
  MinFilter, MagFilter, WrapS, WrapT: TGLenum;
  ColorModulatorByte: TColorModulatorByteFunc): TGLuint;

type
  { Sequence of OpenGL textures to be played as a video. }
  TGLVideo = class
  private
    FItems: array of TGLuint;
    FCount: Integer;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FFramesPerSecond: Single;
  public
    { Constructor that initializes video from TVideo class.

      TVideo passed here must be already @link(TVideo.Loaded Loaded).

      Note that this class doesn't descend
      or keep reference to TVideo instance. The idea is that after
      creating TGLVideo instance, you can often free original TVideo
      instance (if you care only about playing the movie). This can
      conserve memory greatly, as TVideo keeps all frames in the memory,
      and so is rather memory-costly.
      (Actually, TGLVideo itself may eat a lot of texture memory,
      so be careful with large videos anyway.) }
    constructor Create(Video: TVideo;
      MinFilter, MagFilter, WrapS, WrapT: TGLenum;
      ColorModulatorByte: TColorModulatorByteFunc = nil);

    destructor Destroy; override;

    property Count: Integer read FCount;
    function IndexFromTime(const Time: Single): Integer;
    function GLTextureFromTime(const Time: Single): TGLuint;

    { See TVideo.FramesPerSecond. }
    property FramesPerSecond: Single read FFramesPerSecond;

    { See TVideo.TimeLoop. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;

    { See TVideo.TimeBackwards. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards;
  end;

implementation

uses SysUtils, KambiUtils;

function ImageGLFormat(const Img: TImage): TGLenum;
begin
  if Img is TRGBImage then
    Result := GL_RGB else
  if Img is TRGBAlphaImage then
    Result := GL_RGBA else
  if Img is TGrayscaleImage then
    Result := GL_LUMINANCE else
  if Img is TGrayscaleAlphaImage then
    Result := GL_LUMINANCE_ALPHA else
    Result := GL_INVALID_ENUM;
end;

function ImageGLType(const Img: TImage): TGLenum;
begin
  if (Img is TRGBImage) or
     (Img is TRGBAlphaImage) or
     (Img is TGrayscaleImage) or
     (Img is TGrayscaleAlphaImage) then
    Result := GL_UNSIGNED_BYTE else
    Result := GL_INVALID_ENUM;
end;

{ Loading images ------------------------------------------------------------- }

function LoadImageToDisplayList(const FileName: string;
  const LoadAsClass: array of TImageClass;
  const LoadForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TGLuint;
var
  Img: TImage;
begin
  Img := LoadImage(FileName, LoadAsClass, LoadForbiddenConvs,
    ResizeToX, ResizeToY);
  try
    Result := ImageDrawToDisplayList(Img);
  finally Img.Free end;
end;

procedure ImageDraw(const Image: TImage);
var UnpackData: TUnpackNotAlignedData;
begin
 BeforeUnpackImage(UnpackData, image);
 try
  with image do
   glDrawPixels(Width, Height, ImageGLFormat(image), ImageGLType(image), RawPixels);
 finally AfterUnpackImage(UnpackData, image) end;
end;

procedure ImageDrawRows(const Image: TImage; Row0, RowsCount: integer);
var UnpackData: TUnpackNotAlignedData;
begin
 BeforeUnpackImage(UnpackData, image);
 try
  with image do
   glDrawPixels(Width, RowsCount, ImageGLFormat(image), ImageGLType(image), Image.RowPtr(Row0));
 finally AfterUnpackImage(UnpackData, image) end;
end;

procedure ImageDrawCutted(const image: TImage; cutx, cuty: cardinal);
var pixUnpack: TPixelStoreUnpack;
    width, height: cardinal;
begin
 width := image.Width;
 height := image.Height;
 if (cutx >= width) or (cuty >= height) then exit; { no need to draw anything }

 SavePixelStoreUnpack(pixUnpack);
 try
  width := width - cutx;
  height := height - cuty;
  glPixelStorei(GL_UNPACK_ROW_LENGTH, image.Width);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, cutx);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, cuty);
  {nie uzywamy tutaj Before/After Unpack NotAligned Image bo i tak
   zawsze robimy Save/Load Pixel Store Unpack wiec najwygodniej jest
   po prostu ustalic zawsze GL_UNPACK_ALIGNMENT na 1}
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glDrawPixels(width, height, ImageGLFormat(image), ImageGLType(image), image.RawPixels);
 finally LoadPixelStoreUnpack(pixUnpack) end;
end;

function ImageDrawToDisplayList(const Img: TImage): TGLuint;
begin
  Result := glGenListsCheck(1, 'ImageDrawToDisplayList');
  glNewList(Result, GL_COMPILE);
  try
    ImageDraw(Img);
  finally glEndList end;
end;

{ Saving screen to TRGBImage ------------------------------------------------ }

{ This is the basis for all other SaveScreen* functions below. }
function SaveScreen_noflush(
  ImageClass: TImageClass;
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TImage;
var
  PackData: TPackNotAlignedData;
begin
  Result := ImageClass.Create(width, height);
  try
    BeforePackNotAlignedRGBImage(packData, width);
    try
      glReadBuffer(ReadBuffer);
      glReadPixels(xpos, ypos, width, height, ImageGLFormat(Result),
        ImageGLType(Result), Result.RawPixels);
    finally AfterPackNotAlignedRGBImage(packData, width) end;
  except Result.Free; raise end;
end;

function SaveScreen_noflush(
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TRGBImage;
begin
  Result := TRGBImage(SaveScreen_noflush(TRGBImage, xpos, ypos, width, height, ReadBuffer));
end;

procedure SaveScreen_noflush(const FileName: string; ReadBuffer: TGLenum);
var img: TRGBImage;
begin
 try
  img := SaveScreen_noflush(ReadBuffer);
  SaveImage(img, FileName);
 finally Img.Free end;
end;

function SaveScreen_noflush(ReadBuffer: TGLenum): TRGBImage;
var viewport: TVector4i;
begin
 glGetIntegerv(GL_VIEWPORT, @viewport);
 result := SaveScreen_noflush(viewport[0], viewport[1], viewport[2], viewport[3], ReadBuffer);
end;

function SaveScreenToDisplayList_noflush(ReadBuffer: TGLenum): TGLuint; overload;
var img: TImage;
begin
 img := SaveScreen_noflush(ReadBuffer);
 try
  result := ImageDrawToDisplayList(img);
 finally Img.Free end;
end;

function SaveScreenToDisplayList_noflush(xpos, ypos, width, height: integer; ReadBuffer: TGLenum): TGLuint; overload;
var img: TImage;
begin
 img := SaveScreen_noflush(xpos, ypos, width, height, ReadBuffer);
 try
  result := ImageDrawToDisplayList(img);
 finally Img.Free end;
end;

{ ----------------------------------------------------------------------
  Adjusting image size to load them as textures. }

function TextureNonPowerOfTwo: boolean;
begin
  Result := false
    { Using this makes OpenGL *sooo* slow...
      see e.g. castle/levels/castle_hall_final.wrl
      model or kambi_vrml_test_suite/inlined_textures.wrl.
      So it's better to scale textures to be power of 2. }
    {GL_ARB_texture_non_power_of_two or GL_version_2_0};
end;

function IsTextureSized(const r: TImage): boolean;
var maxTexSize: Cardinal;
begin
 maxTexSize := glGetInteger(GL_MAX_TEXTURE_SIZE);

 if TextureNonPowerOfTwo then
   Result :=
     (r.Width <= maxTexSize) and
     (r.Height <= maxTexSize) else
   Result :=
     IsPowerOf2(r.Width) and
     IsPowerOf2(r.Height) and
     (BiggestPowerOf2(r.Width) <= maxTexSize) and
     (BiggestPowerOf2(r.Height) <= maxTexSize);
end;

procedure ResizeForTextureSize(var r: TImage);
var newR: TImage;
begin
 if not IsTextureSized(r) then
 begin
  newR := ResizeToTextureSize(r);
  FreeAndNil(r);
  r := newR;
 end;
end;

function ResizeToTextureSize(const r: TImage): TImage;
var maxTexSize: Cardinal;

  function BestTexSize(size: Cardinal): Cardinal;
  begin
   if size > maxTexSize then
    result := maxTexSize else
   begin
    if TextureNonPowerOfTwo or IsPowerOf2(size) then
     result := size else
     result := 1 shl (Biggest2Exponent(size)+1);
     {result jakie otrzymamy w ostatnim przypisaniu jest na pewno < maxTexSize bo
      skoro size <= maxTexSize i not IsPowerOf2(size) to size < maxTexSize a maxTexSize
      samo jest potega dwojki. }
   end;
  end;

begin
 maxTexSize := glGetInteger(GL_MAX_TEXTURE_SIZE);
 result := r.MakeResized(BestTexSize(r.Width), BestTexSize(r.Height));
end;

{ implementacja procedur LoadGLTextures_XXX
  -----------------------------------------------------------------------------}

function LoadGLTexture(const image: TImage; minFilter, magFilter: TGLenum;
  GrayscaleIsAlpha: boolean): TGLuint;
begin
  glGenTextures(1, @result);
  LoadGLGeneratedTexture(result, image, minFilter, magFilter,
    GrayscaleIsAlpha);
end;

function LoadGLTexture(const image: TImage; minFilter, magFilter,
  wrapS, wrapT: TGLenum; GrayscaleIsAlpha: boolean): TGLuint; overload;
begin
 result := LoadGLTexture(Image, MinFilter, MagFilter, GrayscaleIsAlpha);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, WrapS);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, WrapT);
end;

function LoadGLTexture(const FileName: string;
  MinFilter, MagFilter, WrapS, WrapT: TGLenum; GrayscaleIsAlpha: boolean): TGLuint;
var Image: TImage;
begin
 Image := LoadImage(FileName, GLImageClasses, []);
 try
  Result := LoadGLTexture(Image, MinFilter, MagFilter, WrapS, WrapT,
    GrayscaleIsAlpha);
 finally Image.Free end;
end;

procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TImage;
  minFilter, magFilter: TGLenum; GrayscaleIsAlpha: boolean);
var
  ImageInternalFormat: TGLuint;
  ImageFormat: TGLuint;

  { Calls glTexImage2D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes). }
  procedure glTexImage2DImage(Image: TImage);

    { This is like glTexImage2DImage, but it doesn't take care
      of Image size. }
    procedure Core(Image: TImage);
    var
      UnpackData: TUnpackNotAlignedData;
    begin
      { Nawet jesli ladujemy obrazek o ktorym wiemy ze ma wymiary dobre
        dla glTexImage2d, musimy zadbac o jego aligment : bo co by bylo
        jesli tekstura ma szerokosc 1 lub 2  ?
        Poza tym, planuje dodac tutaj robienie borderow dla tekstury, a wtedy
        wymiar dobry dla glTexImage2d to rownie dobrze 2^n+2 (a wiec prawie zawsze
        niepodzielne na 4). }
      BeforeUnpackImage(UnpackData, Image);
      try
        glTexImage2D(GL_TEXTURE_2D, 0, ImageInternalFormat,
          Image.Width, Image.Height, 0, ImageFormat, ImageGLType(Image),
          Image.RawPixels);
      finally AfterUnpackImage(UnpackData, Image) end;
    end;

  var
    ImgGood: TImage;
  begin
    if IsTextureSized(Image) then
      Core(Image) else
    begin
      ImgGood := ResizeToTextureSize(Image);
      try
        Core(ImgGood);
      finally ImgGood.Free end;
    end;
  end;

  { Calls gluBuild2DMipmaps for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    gluBuild2DMipmaps doesn't require size to be a power of 2, so no problems
    here. }
  procedure gluBuild2DMipmapsImage(Image: TImage);
  var
    UnpackData: TUnpackNotAlignedData;
  begin
    BeforeUnpackImage(UnpackData, Image);
    try
      gluBuild2DMipmaps(GL_TEXTURE_2D, ImageInternalFormat,
        Image.Width, Image.Height, ImageFormat, ImageGLType(Image),
        Image.RawPixels);
    finally AfterUnpackImage(UnpackData, Image) end;
  end;

  procedure LoadMipmapped(const image: TImage);
  begin
    if GL_SGIS_generate_mipmap then
    begin
      { hardware-accelerated mipmap generation.
        Thanks go to Eric Grange for mentioning it on
        [http://www.pascalgamedevelopment.com/forums/viewtopic.php?p=20514]
        Documentation is on
        [http://oss.sgi.com/projects/ogl-sample/registry/SGIS/generate_mipmap.txt] }
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
      glTexImage2DImage(Image);
    end else
    begin
      gluBuild2DMipmapsImage(Image);
    end;
  end;

  procedure LoadNormal(const image: TImage);
  begin
    if GL_SGIS_generate_mipmap then
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexImage2DImage(Image);
  end;

const
  MIPMAP_FLAGS_ARRAY :array[0..3]of TGLenum =
  ( GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR );
begin
  if (Image is TGrayscaleImage) and GrayscaleIsAlpha then
  begin
    { To treat texture as pure alpha channel, both internalFormat and format
      must be ALPHA }
    ImageInternalFormat := GL_ALPHA;
    ImageFormat := GL_ALPHA;
  end else
  begin
    ImageInternalFormat := Image.ColorComponentsCount;
    ImageFormat := ImageGLFormat(Image);
  end;

  { bind the texture, set min and mag filters }
  glBindTexture(GL_TEXTURE_2D, texnum);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, minFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, magFilter);

  { give the texture data }
  if ArrayPosCard(minFilter, MIPMAP_FLAGS_ARRAY) >= 0 then
    LoadMipmapped(Image) else
    LoadNormal(Image);
end;

procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TImage;
  minFilter, magFilter, wrapS, wrapT: TGLenum; GrayscaleIsAlpha: boolean);
begin
 LoadGLGeneratedTexture(TexNum, Image, MinFilter, MagFilter, GrayscaleIsAlpha);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, WrapS);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, WrapT);
end;

function LoadGLTextureModulated(const Image: TImage; MinFilter, MagFilter,
  WrapS, WrapT: TGLenum; ColorModulatorByte: TColorModulatorByteFunc): TGLuint;
var ImageModul: TImage;
begin
 if Assigned(ColorModulatorByte) then
 begin
  ImageModul := Image.MakeModulatedRGB(ColorModulatorByte);
  try
   Result := LoadGLTexture(ImageModul, MinFilter, MagFilter, WrapS, WrapT);
  finally ImageModul.Free; end;
 end else
  Result := LoadGLTexture(Image, MinFilter, MagFilter, WrapS, WrapT);
end;

{ TGLVideo ------------------------------------------------------------------- }

constructor TGLVideo.Create(Video: TVideo;
  MinFilter, MagFilter, WrapS, WrapT: TGLenum;
  ColorModulatorByte: TColorModulatorByteFunc = nil);
var
  I: Integer;
begin
  inherited Create;

  Check(Video.Loaded, 'Video must be loaded before using TGLVideo.Create');

  FCount := Video.Count;

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
    FItems[I] := LoadGLTextureModulated(Video.Items[I],
      MinFilter, MagFilter, WrapS, WrapT, ColorModulatorByte);

  FTimeLoop := Video.TimeLoop;
  FTimeBackwards := Video.TimeBackwards;
  FFramesPerSecond := Video.FramesPerSecond;
end;

destructor TGLVideo.Destroy;
begin
  if Count > 0 then
    glDeleteTextures(Count, @FItems[0]);

  inherited;
end;

function TGLVideo.IndexFromTime(const Time: Single): Integer;
begin
  Result := TVideo.FrameIndexFromTime(Time, Count, FramesPerSecond,
    TimeLoop, TimeBackwards);
end;

function TGLVideo.GLTextureFromTime(const Time: Single): TGLuint;
begin
  Result := FItems[IndexFromTime(Time)];
end;

end.
