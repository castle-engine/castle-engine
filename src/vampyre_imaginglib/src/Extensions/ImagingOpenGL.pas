{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit contains functions for loading and saving OpenGL textures
  using Imaging and for converting images to textures and vice versa.}
unit ImagingOpenGL;

{$I ImagingOptions.inc}

{ Define this symbol if you want to use dglOpenGL header.}
{$DEFINE OPENGL_USE_DGL_HEADERS}

{$IFDEF OPENGL_NO_EXT_HEADERS}
  {$UNDEF OPENGL_USE_DGL_HEADERS}
{$ENDIF}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats,
{$IF Defined(OPENGL_USE_DGL_HEADERS)}
  dglOpenGL,
{$ELSE}
  gl, glext,
{$IFEND}
 ImagingUtility;

type
  { Various texture capabilities of installed OpenGL driver.}
  TGLTextureCaps = record
    MaxTextureSize: LongInt;     // Max size of texture in pixels supported by HW
    NonPowerOfTwo: Boolean;      // HW has full support for NPOT textures
    DXTCompression: Boolean;     // HW supports S3TC/DXTC compressed textures
    ATI3DcCompression: Boolean;  // HW supports ATI 3Dc compressed textures (ATI2N)
    LATCCompression: Boolean;    // HW supports LATC/RGTC compressed textures (ATI1N+ATI2N)
    FloatTextures: Boolean;      // HW supports floating point textures
    MaxAnisotropy: LongInt;      // Max anisotropy for aniso texture filtering
    MaxSimultaneousTextures: LongInt; // Number of texture units
    ClampToEdge: Boolean;        // GL_EXT_texture_edge_clamp
    TextureLOD: Boolean;         // GL_SGIS_texture_lod
    VertexTextureUnits: Integer; // Texture units accessible in vertex programs
  end;

{ Returns texture capabilities of installed OpenGL driver.}
function GetGLTextureCaps(var Caps: TGLTextureCaps): Boolean;
{ Function which can be used to retrieve GL extension functions.}
function GetGLProcAddress(const ProcName: string): Pointer;
{ Returns True if the given GL extension is supported.}
function IsGLExtensionSupported(const Extension: string): Boolean;
{ Returns True if the given image format can be represented as GL texture
  format. GLFormat, GLType, and GLInternal are parameters for functions like
  glTexImage. Note that GLU functions like gluBuildMipmaps cannot handle some
  formats returned by this function (i.e. GL_UNSIGNED_SHORT_5_5_5_1 as GLType).
  If you are using compressed or floating-point images make sure that they are
  supported by hardware using GetGLTextureCaps, ImageFormatToGL does not
  check this.}
function ImageFormatToGL(Format: TImageFormat; var GLFormat: GLenum;
  var GLType: GLenum; var GLInternal: GLint; const Caps: TGLTextureCaps): Boolean;

{ All GL textures created by Imaging functions have default parameters set -
  that means that no glTexParameter calls are made so default filtering,
  wrapping, and other parameters are used. Created textures
  are left bound by glBindTexture when function is exited.}

{ Creates GL texture from image in file in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadGLTextureFromFile(const FileName: string; CreatedWidth: PLongInt = nil;
  CreatedHeight: PLongInt = nil): GLuint;
{ Creates GL texture from image in stream in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadGLTextureFromStream(Stream: TStream; CreatedWidth: PLongInt = nil;
  CreatedHeight: PLongInt = nil): GLuint;
{ Creates GL texture from image in memory in format supported by Imaging.
  You can use CreatedWidth and Height parameters to query dimensions of created textures
  (it could differ from dimensions of source image).}
function LoadGLTextureFromMemory(Data: Pointer; Size: LongInt;
  CreatedWidth: PLongInt = nil; CreatedHeight: PLongInt = nil): GLuint;

{ Converts TImageData structure to OpenGL texture.
  Input images is used as main mipmap level and additional requested
  levels are generated from this one. For the details on parameters
  look at CreateGLTextureFromMultiImage function.}
function CreateGLTextureFromImage(const Image: TImageData;
  Width: LongInt = 0; Height: LongInt = 0; MipMaps: Boolean = True;
  OverrideFormat: TImageFormat = ifUnknown; CreatedWidth: PLongInt = nil;
  CreatedHeight: PLongInt = nil): GLuint;
{ Converts images in TDymImageDataArray to one OpenGL texture.
  Image at index MainLevelIndex in the array is used as main mipmap level and
  additional images are used as subsequent levels. If there is not enough images
  in array missing levels are automatically generated (and if there is enough images
  but they have wrong dimensions or format then they are resized/converted).
  If driver supports only power of two sized textures images are resized.
  OverrideFormat can be used to convert image into specific format before
  it is passed to OpenGL, ifUnknown means no conversion.
  If desired texture format is not supported by hardware default
  A8R8G8B8 format is used instead for color images and ifGray8 is used
  for luminance images. DXTC (S3TC) compressed and floating point textures
  are created if supported by hardware.
  Width and Height can be used to set size of main mipmap level according
  to your needs, Width and Height of 0 mean use width and height of input
  image that will become main level mipmap.
  MipMaps set to True mean build all possible levels, False means use only level 0.
  You can use CreatedWidth and CreatedHeight parameters to query dimensions of
  created texture's largest mipmap level (it could differ from dimensions
  of source image).}
function CreateGLTextureFromMultiImage(const Images: TDynImageDataArray;
  Width: LongInt = 0; Height: LongInt = 0; MipMaps: Boolean = True;
  MainLevelIndex: LongInt = 0; OverrideFormat: TImageFormat = ifUnknown;
  CreatedWidth: PLongInt = nil; CreatedHeight: PLongInt = nil): GLuint;

{ Saves GL texture to file in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveGLTextureToFile(const FileName: string; const Texture: GLuint): Boolean;
{ Saves GL texture to stream in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveGLTextureToStream(const Ext: string; Stream: TStream; const Texture: GLuint): Boolean;
{ Saves GL texture to memory in one of formats supported by Imaging.
  Saves all present mipmap levels.}
function SaveGLTextureToMemory(const Ext: string; Data: Pointer; var Size: LongInt; const Texture: GLuint): Boolean;

{ Converts main level of the GL texture to TImageData structure. OverrideFormat
  can be used to convert output image to the specified format rather
  than use the format taken from GL texture, ifUnknown means no conversion.}
function CreateImageFromGLTexture(const Texture: GLuint;
  var Image: TImageData; OverrideFormat: TImageFormat = ifUnknown): Boolean;
{ Converts GL texture to TDynImageDataArray array of images. You can specify
  how many mipmap levels of the input texture you want to be converted
  (default is all levels). OverrideFormat can be used to convert output images to
  the specified format rather than use the format taken from GL texture,
  ifUnknown means no conversion.}
function CreateMultiImageFromGLTexture(const Texture: GLuint;
  var Images: TDynImageDataArray; MipLevels: LongInt = 0;
  OverrideFormat: TImageFormat = ifUnknown): Boolean;

var
  { Standard behaviour of image->texture functions like CreateGLTextureFrom(Multi)Image is:
    If graphic card supports non power of 2 textures and image is nonpow2 then
    texture is created directly from image.
    If graphic card does not support them input image is rescaled (bilinear)
    to power of 2 size.
    If you set PasteNonPow2ImagesIntoPow2 to True then instead of rescaling, a new
    pow2 texture is created and nonpow2 input image is pasted into it
    keeping its original size. This could be useful for some 2D stuff
    (and its faster than rescaling of course). Note that this is applied
    to all rescaling smaller->bigger operations that might occur during
    image->texture process (usually only pow2/nonpow2 stuff and when you
    set custom Width & Height in CreateGLTextureFrom(Multi)Image).}
  PasteNonPow2ImagesIntoPow2: Boolean = False;
  { Standard behavior if GL_ARB_texture_non_power_of_two extension is not supported
    is to rescale image to power of 2 dimensions. NPOT extension is exposed only
    when HW has full support for NPOT textures but some cards
    (pre-DX10 ATI Radeons, some other maybe) have partial NPOT support. 
    Namely Radeons can use NPOT textures but not mipmapped. If you know what you are doing
    you can disable NPOT support check so the image won't be rescaled to POT
    by setting DisableNPOTSupportCheck to True.}
  DisableNPOTSupportCheck: Boolean = False;

implementation

const
  // Cube map constants
  GL_TEXTURE_BINDING_CUBE_MAP       = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X    = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X    = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y    = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z    = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    = $851A;

  // Texture formats
  GL_COLOR_INDEX                    = $1900;
  GL_STENCIL_INDEX                  = $1901;
  GL_DEPTH_COMPONENT                = $1902;
  GL_RED                            = $1903;
  GL_GREEN                          = $1904;
  GL_BLUE                           = $1905;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;
  GL_BGR_EXT                        = $80E0;
  GL_BGRA_EXT                       = $80E1;

  // Texture internal formats
  GL_ALPHA4                         = $803B;
  GL_ALPHA8                         = $803C;
  GL_ALPHA12                        = $803D;
  GL_ALPHA16                        = $803E;
  GL_LUMINANCE4                     = $803F;
  GL_LUMINANCE8                     = $8040;
  GL_LUMINANCE12                    = $8041;
  GL_LUMINANCE16                    = $8042;
  GL_LUMINANCE4_ALPHA4              = $8043;
  GL_LUMINANCE6_ALPHA2              = $8044;
  GL_LUMINANCE8_ALPHA8              = $8045;
  GL_LUMINANCE12_ALPHA4             = $8046;
  GL_LUMINANCE12_ALPHA12            = $8047;
  GL_LUMINANCE16_ALPHA16            = $8048;
  GL_INTENSITY                      = $8049;
  GL_INTENSITY4                     = $804A;
  GL_INTENSITY8                     = $804B;
  GL_INTENSITY12                    = $804C;
  GL_INTENSITY16                    = $804D;
  GL_R3_G3_B2                       = $2A10;
  GL_RGB4                           = $804F;
  GL_RGB5                           = $8050;
  GL_RGB8                           = $8051;
  GL_RGB10                          = $8052;
  GL_RGB12                          = $8053;
  GL_RGB16                          = $8054;
  GL_RGBA2                          = $8055;
  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGBA8                          = $8058;
  GL_RGB10_A2                       = $8059;
  GL_RGBA12                         = $805A;
  GL_RGBA16                         = $805B;
  GL_RGB565                         = $8D62;

  // Floating point texture formats
  GL_RGBA32F_ARB                    = $8814;
  GL_INTENSITY32F_ARB               = $8817;
  GL_LUMINANCE32F_ARB               = $8818;
  GL_RGBA16F_ARB                    = $881A;
  GL_INTENSITY16F_ARB               = $881D;
  GL_LUMINANCE16F_ARB               = $881E;

  // Compressed texture formats
  // S3TC/DXTC
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT   = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  = $83F3;
  // 3Dc LATC
  GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI          = $8837;
  GL_COMPRESSED_LUMINANCE_LATC1_EXT              = $8C70;
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT       = $8C71;
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT        = $8C72;
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = $8C73;
  // ETC1 GL_OES_compressed_ETC1_RGB8_texture
  GL_ETC1_RGB_OES          = $8D64;
  // PVRTC GL_IMG_texture_compression_pvrtc
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG  = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;
  // AMD ATC
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD      = $8C93;
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD  = $87EE;
  // ETC2/EAC
  GL_COMPRESSED_R11_EAC                        = $9270;
  GL_COMPRESSED_SIGNED_R11_EAC                 = $9271;
  GL_COMPRESSED_RG11_EAC                       = $9272;
  GL_COMPRESSED_SIGNED_RG11_EAC                = $9273;
  GL_COMPRESSED_RGB8_ETC2                      = $9274;
  GL_COMPRESSED_SRGB8_ETC2                     = $9275;
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2  = $9276;
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
  GL_COMPRESSED_RGBA8_ETC2_EAC                 = $9278;
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC          = $9279;

  // Various GL extension constants
  GL_MAX_TEXTURE_UNITS              = $84E2;
  GL_TEXTURE_MAX_ANISOTROPY_EXT     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

  // Texture source data formats
  GL_UNSIGNED_BYTE_3_3_2            = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1         = $8034;
  GL_UNSIGNED_INT_8_8_8_8           = $8035;
  GL_UNSIGNED_INT_10_10_10_2        = $8036;
  GL_UNSIGNED_BYTE_2_3_3_REV        = $8362;
  GL_UNSIGNED_SHORT_5_6_5           = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV       = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV     = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV     = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV       = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV    = $8368;
  GL_HALF_FLOAT_ARB                 = $140B;

  // Other GL constants
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;


{$IFDEF MSWINDOWS}
  GLLibName = 'opengl32.dll';
{$ENDIF}
{$IFDEF UNIX}
  GLLibName = 'libGL.so';
{$ENDIF}

type
  TglCompressedTexImage2D = procedure (Target: GLenum; Level: GLint;
    InternalFormat: GLenum; Width: GLsizei; Height: GLsizei; Border: GLint;
    ImageSize: GLsizei; const Data: PGLvoid);
    {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  glCompressedTexImage2D: TglCompressedTexImage2D = nil;
  ExtensionBuffer: string = '';

{$IFDEF MSWINDOWS}
function wglGetProcAddress(ProcName: PAnsiChar): Pointer; stdcall; external GLLibName;
{$ENDIF}
{$IFDEF UNIX}
function glXGetProcAddress(ProcName: PAnsiChar): Pointer; cdecl; external GLLibName;
{$ENDIF}

function IsGLExtensionSupported(const Extension: string): Boolean;
var
  ExtPos: LongInt;
begin
  if ExtensionBuffer = '' then
    ExtensionBuffer := glGetString(GL_EXTENSIONS);

  ExtPos := Pos(Extension, ExtensionBuffer);
  Result := ExtPos > 0;
  if Result then
  begin
    Result := ((ExtPos + Length(Extension) - 1) = Length(ExtensionBuffer)) or
      not (ExtensionBuffer[ExtPos + Length(Extension)] in ['_', 'A'..'Z', 'a'..'z']);
  end;
end;

function GetGLProcAddress(const ProcName: string): Pointer;
begin
{$IFDEF MSWINDOWS}
  Result := wglGetProcAddress(PAnsiChar(AnsiString(ProcName)));
{$ENDIF}
{$IFDEF UNIX}
  Result := glXGetProcAddress(PAnsiChar(AnsiString(ProcName)));
{$ENDIF}
end;

function GetGLTextureCaps(var Caps: TGLTextureCaps): Boolean;
begin
  // Check DXTC support and load extension functions if necessary
  Caps.DXTCompression := IsGLExtensionSupported('GL_ARB_texture_compression') and
    IsGLExtensionSupported('GL_EXT_texture_compression_s3tc');
  if Caps.DXTCompression then
    glCompressedTexImage2D := GetGLProcAddress('glCompressedTexImage2D');
  Caps.DXTCompression := Caps.DXTCompression and (@glCompressedTexImage2D <> nil);
  Caps.ATI3DcCompression := Caps.DXTCompression and
    IsGLExtensionSupported('GL_ATI_texture_compression_3dc');
  Caps.LATCCompression := Caps.DXTCompression and
    IsGLExtensionSupported('GL_EXT_texture_compression_latc');
  // Check non power of 2 textures
  Caps.NonPowerOfTwo := IsGLExtensionSupported('GL_ARB_texture_non_power_of_two');
  // Check for floating point textures support
  Caps.FloatTextures := IsGLExtensionSupported('GL_ARB_texture_float');
  // Get max texture size
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Caps.MaxTextureSize);
  // Get max anisotropy
  if IsGLExtensionSupported('GL_EXT_texture_filter_anisotropic') then
    glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @Caps.MaxAnisotropy)
  else
    Caps.MaxAnisotropy := 0;
  // Get number of texture units
  if IsGLExtensionSupported('GL_ARB_multitexture') then
    glGetIntegerv(GL_MAX_TEXTURE_UNITS, @Caps.MaxSimultaneousTextures)
  else
    Caps.MaxSimultaneousTextures := 1;
  // Get number of vertex texture units
  if IsGLExtensionSupported('GL_ARB_vertex_shader') then
    glGetIntegerv(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS, @Caps.VertexTextureUnits)
  else
    Caps.VertexTextureUnits := 1;
  // Get max texture size
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Caps.MaxTextureSize);
  // Clamp texture to edge?
  Caps.ClampToEdge := IsGLExtensionSupported('GL_EXT_texture_edge_clamp');
  // Texture LOD extension?
  Caps.TextureLOD := IsGLExtensionSupported('GL_SGIS_texture_lod');

  Result := True;
end;

function ImageFormatToGL(Format: TImageFormat; var GLFormat: GLenum;
  var GLType: GLenum; var GLInternal: GLint; const Caps: TGLTextureCaps): Boolean;
begin
  GLFormat := 0;
  GLType := 0;
  GLInternal := 0;
  case Format of
    // Gray formats
    ifGray8, ifGray16:
      begin
        GLFormat   := GL_LUMINANCE;
        GLType     := Iff(Format = ifGray8, GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT);
        GLInternal := Iff(Format = ifGray8, GL_LUMINANCE8, GL_LUMINANCE16);
      end;
    ifA8Gray8, ifA16Gray16:
      begin
        GLFormat   := GL_LUMINANCE_ALPHA;
        GLType     := Iff(Format = ifA8Gray8, GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT);
        GLInternal := Iff(Format = ifA8Gray8, GL_LUMINANCE8_ALPHA8, GL_LUMINANCE16_ALPHA16);
      end;
    // RGBA formats
    ifR3G3B2:
      begin
        GLFormat   := GL_RGB;
        GLType     := GL_UNSIGNED_BYTE_3_3_2;
        GLInternal := GL_R3_G3_B2;
      end;
    ifR5G6B5:
      begin
        GLFormat   := GL_RGB;
        GLType     := GL_UNSIGNED_SHORT_5_6_5;
        GLInternal := GL_RGB5; //GL_RGB565 ot working on Radeons
      end;
    ifA1R5G5B5, ifX1R5G5B5:
      begin
        GLFormat   := GL_BGRA_EXT;
        GLType     := GL_UNSIGNED_SHORT_1_5_5_5_REV;
        GLInternal := Iff(Format = ifA1R5G5B5, GL_RGB5_A1, GL_RGB5);
      end;
    ifA4R4G4B4, ifX4R4G4B4:
      begin
        GLFormat   := GL_BGRA_EXT;
        GLType     := GL_UNSIGNED_SHORT_4_4_4_4_REV;
        GLInternal := Iff(Format = ifA4R4G4B4, GL_RGBA4, GL_RGB4);
      end;
    ifR8G8B8:
      begin
        GLFormat   := GL_BGR_EXT;
        GLType     := GL_UNSIGNED_BYTE;
        GLInternal := GL_RGB8;
      end;
    ifA8R8G8B8, ifX8R8G8B8:
      begin
        GLFormat   := GL_BGRA_EXT;
        GLType     := GL_UNSIGNED_BYTE;
        GLInternal := Iff(Format = ifA8R8G8B8, GL_RGBA8, GL_RGB8);
      end;
    ifR16G16B16, ifB16G16R16:
      begin
        GLFormat   := Iff(Format = ifR16G16B16, GL_BGR_EXT, GL_RGB);
        GLType     := GL_UNSIGNED_SHORT;
        GLInternal := GL_RGB16;
      end;
    ifA16R16G16B16, ifA16B16G16R16:
      begin
        GLFormat   := Iff(Format = ifA16R16G16B16, GL_BGRA_EXT, GL_RGBA);
        GLType     := GL_UNSIGNED_SHORT;
        GLInternal := GL_RGBA16;
      end;
    // Floating-Point formats
    ifR32F:
      begin
        GLFormat   := GL_RED;
        GLType     := GL_FLOAT;
        GLInternal := GL_LUMINANCE32F_ARB;
      end;
    ifA32R32G32B32F, ifA32B32G32R32F:
      begin
        GLFormat   := Iff(Format = ifA32R32G32B32F, GL_BGRA_EXT, GL_RGBA);
        GLType     := GL_FLOAT;
        GLInternal := GL_RGBA32F_ARB;
      end;
    ifR16F:
      begin
        GLFormat   := GL_RED;
        GLType     := GL_HALF_FLOAT_ARB;
        GLInternal := GL_LUMINANCE16F_ARB;
      end;
    ifA16R16G16B16F, ifA16B16G16R16F:
      begin
        GLFormat   := Iff(Format = ifA16R16G16B16F, GL_BGRA_EXT, GL_RGBA);
        GLType     := GL_HALF_FLOAT_ARB;
        GLInternal := GL_RGBA16F_ARB;
      end;
    // Special formats
    ifDXT1: GLInternal := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
    ifDXT3: GLInternal := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
    ifDXT5: GLInternal := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
    ifATI1N: GLInternal := GL_COMPRESSED_LUMINANCE_LATC1_EXT;
    ifATI2N:
      begin
        GLInternal := GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT;
        if not Caps.LATCCompression and Caps.ATI3DcCompression then
          GLInternal := GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI;
      end;
  end;
  Result := GLInternal <> 0;
end;

function LoadGLTextureFromFile(const FileName: string; CreatedWidth, CreatedHeight: PLongInt): GLuint;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromFile(FileName, Images) and (Length(Images) > 0) then
  begin
    Result := CreateGLTextureFromMultiImage(Images, Images[0].Width,
      Images[0].Height, True, 0, ifUnknown, CreatedWidth, CreatedHeight);
  end
  else
    Result := 0;
  FreeImagesInArray(Images);
end;

function LoadGLTextureFromStream(Stream: TStream; CreatedWidth, CreatedHeight: PLongInt): GLuint;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromStream(Stream, Images) and (Length(Images) > 0) then
  begin
    Result := CreateGLTextureFromMultiImage(Images, Images[0].Width,
      Images[0].Height, True, 0, ifUnknown, CreatedWidth, CreatedHeight);
  end
  else
    Result := 0;
  FreeImagesInArray(Images);
end;

function LoadGLTextureFromMemory(Data: Pointer; Size: LongInt; CreatedWidth, CreatedHeight: PLongInt): GLuint;
var
  Images: TDynImageDataArray;
begin
  if LoadMultiImageFromMemory(Data, Size, Images)  and (Length(Images) > 0) then
  begin
    Result := CreateGLTextureFromMultiImage(Images, Images[0].Width,
      Images[0].Height, True, 0, ifUnknown, CreatedWidth, CreatedHeight);
  end
  else
    Result := 0;
  FreeImagesInArray(Images);
end;

function CreateGLTextureFromImage(const Image: TImageData;
  Width, Height: LongInt; MipMaps: Boolean; OverrideFormat: TImageFormat;
  CreatedWidth, CreatedHeight: PLongInt): GLuint;
var
  Arr: TDynImageDataArray;
begin
  // Just calls function operating on image arrays
  SetLength(Arr, 1);
  Arr[0] := Image;
  Result := CreateGLTextureFromMultiImage(Arr, Width, Height, MipMaps, 0,
    OverrideFormat, CreatedWidth, CreatedHeight);
end;

function CreateGLTextureFromMultiImage(const Images: TDynImageDataArray;
  Width, Height: LongInt; MipMaps: Boolean; MainLevelIndex: LongInt; OverrideFormat: TImageFormat;
  CreatedWidth, CreatedHeight: PLongInt): GLuint;
const
  BlockCompressedFormats: TImageFormats = [ifDXT1, ifDXT3, ifDXT5, ifATI1N, ifATI2N];
var
  I, MipLevels, PossibleLevels, ExistingLevels, CurrentWidth, CurrentHeight: LongInt;
  Caps: TGLTextureCaps;
  GLFormat: GLenum;
  GLType: GLenum;
  GLInternal: GLint;
  Desired, ConvTo: TImageFormat;
  Info: TImageFormatInfo;
  LevelsArray: TDynImageDataArray;
  NeedsResize, NeedsConvert: Boolean;
  UnpackAlignment, UnpackSkipRows, UnpackSkipPixels, UnpackRowLength: LongInt;

  procedure PasteImage(var Image: TImageData; Width, Height: LongInt);
  var
    Clone: TImageData;
  begin
    CloneImage(Image, Clone);
    NewImage(Width, Height, Clone.Format, Image);
    FillRect(Image, 0, 0, Width, Height, Clone.Bits);
    CopyRect(Clone, 0, 0, Clone.Width, Clone.Height, Image, 0, 0);
    FreeImage(Clone);
  end;

begin
  Result := 0;
  ExistingLevels := Length(Images);

  if GetGLTextureCaps(Caps) and (ExistingLevels > 0) then
  try
    // Check if requested main level is at valid index
    if (MainLevelIndex < 0) or (MainLevelIndex > High(Images)) then
      MainLevelIndex := 0;

    // First check desired size and modify it if necessary
    if Width <= 0 then Width := Images[MainLevelIndex].Width;
    if Height <= 0 then Height := Images[MainLevelIndex].Height;
    if not Caps.NonPowerOfTwo and not DisableNPOTSupportCheck then
    begin
      // If device supports only power of 2 texture sizes
      Width := NextPow2(Width);
      Height := NextPow2(Height);
    end;
    Width := ClampInt(Width, 1, Caps.MaxTextureSize);
    Height := ClampInt(Height, 1, Caps.MaxTextureSize);

    // Get various mipmap level counts and modify
    // desired MipLevels if its value is invalid
    PossibleLevels := GetNumMipMapLevels(Width, Height);
    if MipMaps then
      MipLevels := PossibleLevels
    else
      MipLevels := 1;

    // Prepare array for mipmap levels. Make it larger than necessary - that
    // way we can use the same index for input images and levels in the large loop below
    SetLength(LevelsArray, MipLevels + MainLevelIndex);

    // Now determine which image format will be used
    if OverrideFormat = ifUnknown then
      Desired := Images[MainLevelIndex].Format
    else
      Desired := OverrideFormat;

    // Check if the hardware supports floating point and compressed textures
    GetImageFormatInfo(Desired, Info);
    if Info.IsFloatingPoint and not Caps.FloatTextures then
      Desired := ifA8R8G8B8;
    if (Desired in [ifDXT1, ifDXT3, ifDXT5]) and not Caps.DXTCompression then
      Desired := ifA8R8G8B8;
    if (Desired = ifATI1N)  and not Caps.LATCCompression then
      Desired := ifGray8;
    if (Desired = ifATI2N) and not (Caps.ATI3DcCompression or Caps.LATCCompression) then
      Desired := ifA8Gray8;

    // Try to find GL format equivalent to image format and if it is not
    // found use one of default formats
    if not ImageFormatToGL(Desired, GLFormat, GLType, GLInternal, Caps) then
    begin
      GetImageFormatInfo(Desired, Info);
      if Info.HasGrayChannel then
        ConvTo := ifGray8
      else
        ConvTo := ifA8R8G8B8;
      if not ImageFormatToGL(ConvTo, GLFormat, GLType, GLInternal, Caps) then
        Exit;
    end
    else
      ConvTo := Desired;

    CurrentWidth := Width;
    CurrentHeight := Height;
    // If user is interested in width and height of created texture lets
    // give him that
    if CreatedWidth <> nil then CreatedWidth^ := CurrentWidth;
    if CreatedHeight <> nil then CreatedHeight^ := CurrentHeight;

    // Store old pixel unpacking settings
    glGetIntegerv(GL_UNPACK_ALIGNMENT, @UnpackAlignment);
    glGetIntegerv(GL_UNPACK_SKIP_ROWS, @UnpackSkipRows);
    glGetIntegerv(GL_UNPACK_SKIP_PIXELS, @UnpackSkipPixels);
    glGetIntegerv(GL_UNPACK_ROW_LENGTH, @UnpackRowLength);
    // Set new pixel unpacking settings
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);

    // Generate new texture, bind it and set
    glGenTextures(1, @Result);
    glBindTexture(GL_TEXTURE_2D, Result);
    if glIsTexture(Result) <> GL_TRUE then
      Exit;

    for I := MainLevelIndex to MipLevels - 1 + MainLevelIndex do
    begin
      // Check if we can use input image array as a source for this mipmap level
      if I < ExistingLevels then
      begin
        // Check if input image for this mipmap level has the right
        // size and format
        NeedsConvert := not (Images[I].Format = ConvTo);
        if ConvTo in BlockCompressedFormats then
        begin
          // Input images in DXTC will have min dimensions of 4, but we need
          // current Width and Height to be lesser (for glCompressedTexImage2D)
          NeedsResize := not ((Images[I].Width = Max(4, CurrentWidth)) and
            (Images[I].Height = Max(4, CurrentHeight)));
        end
        else
          NeedsResize := not ((Images[I].Width = CurrentWidth) and (Images[I].Height = CurrentHeight));

        if NeedsResize or NeedsConvert then
        begin
          // Input image must be resized or converted to different format
          // to become valid mipmap level
          CloneImage(Images[I], LevelsArray[I]);
          if NeedsConvert then
            ConvertImage(LevelsArray[I], ConvTo);
          if NeedsResize then
          begin
            if (not PasteNonPow2ImagesIntoPow2) or (LevelsArray[I].Width > CurrentWidth) or
              (LevelsArray[I].Height > CurrentHeight)then
            begin
              // If pasteNP2toP2 is disabled or if source is bigger than target
              // we rescale image, otherwise we paste it with the same size
              ResizeImage(LevelsArray[I], CurrentWidth, CurrentHeight, rfBilinear)
            end
            else
              PasteImage(LevelsArray[I], CurrentWidth, CurrentHeight);
          end;
        end
        else
          // Input image can be used without any changes
          LevelsArray[I] := Images[I];
      end
      else
      begin
        // This mipmap level is not present in the input image array
        // so we create a new level
        FillMipMapLevel(LevelsArray[I - 1], CurrentWidth, CurrentHeight, LevelsArray[I]);
      end;

      if ConvTo in BlockCompressedFormats then
      begin
        // Note: GL DXTC texture snaller than 4x4 must have width and height
        // as expected for non-DXTC texture (like 1x1 -  we cannot
        // use LevelsArray[I].Width and LevelsArray[I].Height - they are
        // at least 4 for DXTC images). But Bits and Size passed to
        // glCompressedTexImage2D must contain regular 4x4 DXTC block.
        glCompressedTexImage2D(GL_TEXTURE_2D, I - MainLevelIndex, GLInternal, CurrentWidth,
          CurrentHeight, 0, LevelsArray[I].Size, LevelsArray[I].Bits)
      end
      else
      begin
        glTexImage2D(GL_TEXTURE_2D, I - MainLevelIndex, GLInternal, CurrentWidth,
          CurrentHeight, 0, GLFormat, GLType, LevelsArray[I].Bits);
      end;

      // Calculate width and height of the next mipmap level
      CurrentWidth := ClampInt(CurrentWidth div 2, 1, CurrentWidth);
      CurrentHeight := ClampInt(CurrentHeight div 2, 1, CurrentHeight);
    end;

    // Restore old pixel unpacking settings
    glPixelStorei(GL_UNPACK_ALIGNMENT, UnpackAlignment);
    glPixelStorei(GL_UNPACK_SKIP_ROWS, UnpackSkipRows);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, UnpackSkipPixels);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, UnpackRowLength);
  finally
    // Free local image copies
    for I := 0 to Length(LevelsArray) - 1 do
    begin
      if ((I < ExistingLevels) and (LevelsArray[I].Bits <> Images[I].Bits)) or
        (I >= ExistingLevels) then
        FreeImage(LevelsArray[I]);
    end;
  end;
end;

function SaveGLTextureToFile(const FileName: string; const Texture: GLuint): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromGLTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByName(FileName);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToFile(FileName, Arr);
      if IsDDS then
        PopOptions;
    end;
    FreeImagesInArray(Arr);
  end;
end;

function SaveGLTextureToStream(const Ext: string; Stream: TStream; const Texture: GLuint): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromGLTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByExt(Ext);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToStream(Ext, Stream, Arr);
      if IsDDS then
        PopOptions;
    end;
    FreeImagesInArray(Arr);
  end;
end;

function SaveGLTextureToMemory(const Ext: string; Data: Pointer; var Size: LongInt; const Texture: GLuint): Boolean;
var
  Arr: TDynImageDataArray;
  Fmt: TImageFileFormat;
  IsDDS: Boolean;
begin
  Result := CreateMultiImageFromGLTexture(Texture, Arr);
  if Result then
  begin
    Fmt := FindImageFileFormatByExt(Ext);
    if Fmt <> nil then
    begin
      IsDDS := SameText(Fmt.Extensions[0], 'dds');
      if IsDDS then
      begin
        PushOptions;
        SetOption(ImagingDDSSaveMipMapCount, Length(Arr));
      end;
      Result := SaveMultiImageToMemory(Ext, Data, Size, Arr);
      if IsDDS then
        PopOptions;
    end;
    FreeImagesInArray(Arr);
  end;
end;

function CreateImageFromGLTexture(const Texture: GLuint;
  var Image: TImageData; OverrideFormat: TImageFormat): Boolean;
var
  Arr: TDynImageDataArray;
begin
  // Just calls function operating on image arrays
  FreeImage(Image);
  SetLength(Arr, 1);
  Result := CreateMultiImageFromGLTexture(Texture, Arr, 1, OverrideFormat);
  Image := Arr[0];
end;

function CreateMultiImageFromGLTexture(const Texture: GLuint;
  var Images: TDynImageDataArray; MipLevels: LongInt; OverrideFormat: TImageFormat): Boolean;
var
  I, Width, Height, ExistingLevels: LongInt;
begin
  FreeImagesInArray(Images);
  SetLength(Images, 0);
  Result := False;
  if glIsTexture(Texture) = GL_TRUE then
  begin
    // Check if desired mipmap level count is valid
    glBindTexture(GL_TEXTURE_2D, Texture);
    if MipLevels <= 0 then
    begin
      glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @Width);
      glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @Height);
      MipLevels := GetNumMipMapLevels(Width, Height);
    end;
    SetLength(Images, MipLevels);
    ExistingLevels := 0;

    for I := 0 to MipLevels - 1 do
    begin
      // Get the current level size
      glGetTexLevelParameteriv(GL_TEXTURE_2D, I, GL_TEXTURE_WIDTH, @Width);
      glGetTexLevelParameteriv(GL_TEXTURE_2D, I, GL_TEXTURE_HEIGHT, @Height);
      // Break when the mipmap chain is broken
      if (Width = 0) or (Height = 0) then
        Break;
      // Create new image and copy texture data
      NewImage(Width, Height, ifA8R8G8B8, Images[I]);
      glGetTexImage(GL_TEXTURE_2D, I, GL_BGRA_EXT, GL_UNSIGNED_BYTE, Images[I].Bits);
      Inc(ExistingLevels);
    end;
    // Resize mipmap array if necessary
    if MipLevels <> ExistingLevels then
      SetLength(Images, ExistingLevels);
    // Convert images to desired format if set
    if OverrideFormat <> ifUnknown then
      for I := 0 to Length(Images) - 1 do
        ConvertImage(Images[I], OverrideFormat);

    Result := True;
  end;
end;

initialization

{
  File Notes:

  -- TODOS ----------------------------------------------------

  -- 0.77.1 ---------------------------------------------------
    - Added some new compressed formats IDs

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Fixed GetGLProcAddress in Unicode Delphi. Compressed
      textures didn't work because of this.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added support for GLScene's OpenGL header.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Added 3Dc compressed texture formats support.
    - Added detection of 3Dc formats to texture caps.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Added DisableNPOTSupportCheck option and related functionality.
    - Added some new texture caps detection.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Added PasteNonPow2ImagesIntoPow2 option and related functionality.
    - Better NeedsResize determination for small DXTC textures -
      avoids needless resizing.
    - Added MainLevelIndex to CreateMultiImageFromGLTexture.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added CreatedWidth and CreatedHeight parameters to most
      LoadGLTextureFromXXX/CreateGLTextureFromXXX functions.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - fixed bug in CreateGLTextureFromMultiImage which caused assert failure
      when creating mipmaps (using FillMipMapLevel) for DXTC formats
    - changed single channel floating point texture formats from
      GL_INTENSITY..._ARB to GL_LUMINANCE..._ARB
    - added support for half float texture formats (GL_RGBA16F_ARB etc.)   

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - filtered mipmap creation
    - more texture caps added
    - fixed memory leaks in SaveGLTextureTo... functions

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - unit created and initial stuff added
}

end.
