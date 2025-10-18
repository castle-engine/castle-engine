{%MainUnit castleimages.pas}
{
  Copyright 2001-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Possible compression of textures for GPU.
    The compressed texture formats may be automatically created for you by CGE,
    see https://castle-engine.io/creating_data_auto_generated_textures.php . }
  TTextureCompression = (
    { S3TC DXT1 compression, @bold(for opaque RGB images (no alpha channel)).
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC.
      It is also supported by a small number of Android devices.

      Note that the tcDxt1_RGB and tcDxt1_RGBA are the same compression method.
      Their behavior only differs when rendering:
      in case of tcDxt1_RGB, the alpha information is not used,
      while in case of tcDxt1_RGBA, the renderer is using alpha-testing. }
    tcDxt1_RGB,

    { S3TC DXT1 compression, @bold(for RGBA images with simple yes/no alpha channel).
      The renderer will use alpha-testing when rendering such images.
      See above tcDxt1_RGB description for details. }
    tcDxt1_RGBA,

    { S3TC DXT3 compression, @bold(for RGBA images with full alpha channel),
      best for images with sharp alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt3,

    { S3TC DXT5 compression, @bold(for RGBA images with full alpha channel),
      best for images with smooth alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt5,

    { PowerVR texture compression (PVRTC) format.
      Supported by some Android and iOS devices,
      using PowerVR GPU by Imagination Technologies.
      See http://en.wikipedia.org/wiki/PVRTC . }
    tcPvrtc1_4bpp_RGB,
    tcPvrtc1_2bpp_RGB,
    tcPvrtc1_4bpp_RGBA,
    tcPvrtc1_2bpp_RGBA,
    tcPvrtc2_4bpp,
    tcPvrtc2_2bpp,

    { ATI texture compression format, @bold(for RGB images without alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGB,

    { ATI texture compression format, @bold(with sharp alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_ExplicitAlpha,

    { ATI texture compression format, @bold(with smooth alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_InterpolatedAlpha,

    { ETC texture compression, @bold(without alpha).
      See http://en.wikipedia.org/wiki/Ericsson_Texture_Compression .
      Available on almost all Android OpenGLES 2.0 devices,
      unfortunately it doesn't support alpha channel. }
    tcETC1,

    { ASTC compression with alpha - should be available on all modern mobile GPU.
      See https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_texture_compression_astc_hdr.txt}
    tcASTC_4x4_RGBA,
    tcASTC_5x4_RGBA,
    tcASTC_5x5_RGBA,
    tcASTC_6x5_RGBA,
    tcASTC_6x6_RGBA,
    tcASTC_8x5_RGBA,
    tcASTC_8x6_RGBA,
    tcASTC_8x8_RGBA,
    tcASTC_10x5_RGBA,
    tcASTC_10x6_RGBA,
    tcASTC_10x8_RGBA,
    tcASTC_10x10_RGBA,
    tcASTC_12x10_RGBA,
    tcASTC_12x12_RGBA,
    tcASTC_4x4_SRGB8_ALPHA8,
    tcASTC_5x4_SRGB8_ALPHA8,
    tcASTC_5x5_SRGB8_ALPHA8,
    tcASTC_6x5_SRGB8_ALPHA8,
    tcASTC_6x6_SRGB8_ALPHA8,
    tcASTC_8x5_SRGB8_ALPHA8,
    tcASTC_8x6_SRGB8_ALPHA8,
    tcASTC_8x8_SRGB8_ALPHA8,
    tcASTC_10x5_SRGB8_ALPHA8,
    tcASTC_10x6_SRGB8_ALPHA8,
    tcASTC_10x8_SRGB8_ALPHA8,
    tcASTC_10x10_SRGB8_ALPHA8,
    tcASTC_12x10_SRGB8_ALPHA8,
    tcASTC_12x12_SRGB8_ALPHA8
  );
  TTextureCompressions = set of TTextureCompression;

{$endif read_interface}

{$ifdef read_interface_ending}

type
  TTextureCompressionInfo = {$ifdef FPC} object {$else} record {$endif}
    Name: string;
    RequiresPowerOf2: boolean;
    AlphaChannel: TAlphaChannel;

    { When generating to DDS (that has reverted row order with respect to OpenGL),
      most of the compressed textures should be stored as flipped.
      When reading, we expect them to be already flipped.
      When loading to OpenGL, they will effectively be flipped again
      (since OpenGL expects bottom-to-top order, while we load it
      image in top-to-bottom order), thus making the image correct.

      The exceptions are DXT* formats, that we can read correctly (unflipped)
      from DDS.

      This is only a limitation of the DDS format.

      For KTX, we can generate KTX images using PowerVR Texture Tools
      that already have a correct (bottom-to-top) orientation.
      So we can have textures compressed to PVRTC1_4bpp_RGB
      with correct orientation.

      This field is ignored if FileExtension is not .dds. }
    DDSFlipped: boolean;

    { File extension/format the engine expects when try load GPU compressed
      version of texture. }
    FileExtension: String;
  end;

const
  TextureCompressionInfo: array [TTextureCompression] of TTextureCompressionInfo =
  ( (Name: 'DXT1_RGB'                    ; RequiresPowerOf2: false; AlphaChannel: acNone    ; DDSFlipped: false; FileExtension: '.dds'),
    (Name: 'DXT1_RGBA'                   ; RequiresPowerOf2: false; AlphaChannel: acTest    ; DDSFlipped: false; FileExtension: '.dds'),
    (Name: 'DXT3'                        ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: false; FileExtension: '.dds'),
    (Name: 'DXT5'                        ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: false; FileExtension: '.dds'),
    { See http://community.imgtec.com/files/pvrtc-texture-compression-user-guide/
      "PVRTC2 vs PVRTC1" section --- PVRTC1 require power-of-two. } { }
    (Name: 'PVRTC1_4bpp_RGB'             ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'PVRTC1_2bpp_RGB'             ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'PVRTC1_4bpp_RGBA'            ; RequiresPowerOf2: true ; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'PVRTC1_2bpp_RGBA'            ; RequiresPowerOf2: true ; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'PVRTC2_4bpp'                 ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'PVRTC2_2bpp'                 ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    { Tests show that ATITC does not need power-of-two sizes. }
    (Name: 'ATITC_RGB'                   ; RequiresPowerOf2: false; AlphaChannel: acNone    ; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'ATITC_RGBA_ExplicitAlpha'    ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    (Name: 'ATITC_RGBA_InterpolatedAlpha'; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: '.dds'),
    { TODO: unconfirmed RequiresPowerOf2 for ETC1. } { }
    (Name: 'ETC1'                        ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true; FileExtension: '.ktx'),

    (Name: 'ASTC_4x4_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_5x4_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_5x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_6x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_6x6_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x6_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x8_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x5_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x6_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x8_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x10_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_12x10_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_12x12_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_4x4_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_5x4_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_5x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_6x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_6x6_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x6_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_8x8_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x5_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x6_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x8_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_10x10_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_12x10_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif}),
    (Name: 'ASTC_12x12_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true; FileExtension: {$ifdef USE_ASTCENC} '.atcs' {$else} '.ktx' {$endif})
  );

{ Convert TTextureCompression enum to string. }
function TextureCompressionToString(const TextureCompression: TTextureCompression): string;

{ Convert string to TTextureCompression enum. Possible values correspond
  to names listed in TextureCompressionInfo array, they are also equal
  to enum Pascal names without leading "tc".
  Compares given strig ignoring the case.
  @raises(Exception If the string value does not name any
    TTextureCompression value.) }
function StringToTextureCompression(const S: string): TTextureCompression;

{ Convert TTextureCompression to integer value used by OpenGL, OpenGLES and KTX. }
function TextureCompressionToOpenGL(const Compression: TTextureCompression): UInt32;

{ Convert integer value used by OpenGL, OpenGLES and KTX to TTextureCompression.

  Returns @false if given GlCompression is not supported.
  Note that this is different from TextureCompressionToOpenGL,
  which always has some value OpenGL / OpenGLES / KTX constant for every
  TTextureCompression. }
function OpenGLToTextureCompression(const GlCompression: UInt32;
  out Compression: TTextureCompression): Boolean;

var
  { Is the value of @link(SupportedTextureCompression) determined
    by the renderer (like OpenGL context) parameters. }
  SupportedTextureCompressionKnown: boolean;

  { Which texture compression values are supported by
    the renderer (like OpenGL context). }
  SupportedTextureCompression: TTextureCompressions;

{$endif read_interface_ending}

{$ifdef read_implementation}

{ Constants that specify texture compression for OpenGL.
  Internal for both CastleInternalCompositeImage (used by KTX reader)
  and for CastleGLImage (used to load images to OpenGL / OpenGLES).
}

const
  { From https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc.txt }
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG  = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;

  { https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt }
  GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG = $9138;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG = $9137;

  { https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_ATC_texture.txt }
  GL_ATC_RGB_AMD                     = $8C92;
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD     = $8C93;
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;

  { https://www.khronos.org/registry/gles/extensions/OES/OES_compressed_ETC1_RGB8_texture.txt }
  GL_ETC1_RGB8_OES = $8D64;

  { Copied from desktop OpenGL.
    Matching
    https://www.khronos.org/registry/gles/extensions/NV/NV_texture_compression_s3tc.txt }
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

  { Copied from /usr/include/GL/glcorearb.h, under GL_VERSION_4_3 1. }
  { Unused for now:
  GL_COMPRESSED_RGB8_ETC2                      = $9274;
  GL_COMPRESSED_SRGB8_ETC2                     = $9275;
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2  = $9276;
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
  GL_COMPRESSED_RGBA8_ETC2_EAC                 = $9278;
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC          = $9279;
  GL_COMPRESSED_R11_EAC                        = $9270;
  GL_COMPRESSED_SIGNED_R11_EAC                 = $9271;
  GL_COMPRESSED_RG11_EAC                       = $9272;
  GL_COMPRESSED_SIGNED_RG11_EAC                = $9273;
  }

  { https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_texture_compression_astc_hdr.txt }
  GL_COMPRESSED_RGBA_ASTC_4x4_KHR           = $93B0;
  GL_COMPRESSED_RGBA_ASTC_5x4_KHR           = $93B1;
  GL_COMPRESSED_RGBA_ASTC_5x5_KHR           = $93B2;
  GL_COMPRESSED_RGBA_ASTC_6x5_KHR           = $93B3;
  GL_COMPRESSED_RGBA_ASTC_6x6_KHR           = $93B4;
  GL_COMPRESSED_RGBA_ASTC_8x5_KHR           = $93B5;
  GL_COMPRESSED_RGBA_ASTC_8x6_KHR           = $93B6;
  GL_COMPRESSED_RGBA_ASTC_8x8_KHR           = $93B7;
  GL_COMPRESSED_RGBA_ASTC_10x5_KHR          = $93B8;
  GL_COMPRESSED_RGBA_ASTC_10x6_KHR          = $93B9;
  GL_COMPRESSED_RGBA_ASTC_10x8_KHR          = $93BA;
  GL_COMPRESSED_RGBA_ASTC_10x10_KHR         = $93BB;
  GL_COMPRESSED_RGBA_ASTC_12x10_KHR         = $93BC;
  GL_COMPRESSED_RGBA_ASTC_12x12_KHR         = $93BD;

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR   = $93D0;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR   = $93D1;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR   = $93D2;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR   = $93D3;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR   = $93D4;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR   = $93D5;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR   = $93D6;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR   = $93D7;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR  = $93D8;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR  = $93D9;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR  = $93DA;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR = $93DB;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR = $93DC;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR = $93DD;

function TextureCompressionToString(const TextureCompression: TTextureCompression): string;
begin
  Result := TextureCompressionInfo[TextureCompression].Name;
end;

function StringToTextureCompression(const S: string): TTextureCompression;
var
  SLower: string;
begin
  SLower := LowerCase(S);
  for Result := Low(Result) to High(Result) do
    if SLower = LowerCase(TextureCompressionInfo[Result].Name) then
      Exit;
  raise Exception.CreateFmt('Invalid texture compression name "%s"', [S]);
end;

function TextureCompressionToOpenGL(const Compression: TTextureCompression): UInt32;
begin
  case Compression of
    tcDxt1_RGB        : Result := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
    tcDxt1_RGBA       : Result := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
    tcDxt3            : Result := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
    tcDxt5            : Result := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;

    tcPvrtc1_4bpp_RGB : Result := GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG;
    tcPvrtc1_2bpp_RGB : Result := GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG;
    tcPvrtc1_4bpp_RGBA: Result := GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;
    tcPvrtc1_2bpp_RGBA: Result := GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG;
    tcPvrtc2_4bpp     : Result := GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG;
    tcPvrtc2_2bpp     : Result := GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG;

    tcATITC_RGB                   : Result := GL_ATC_RGB_AMD;
    tcATITC_RGBA_InterpolatedAlpha: Result := GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD;
    tcATITC_RGBA_ExplicitAlpha    : Result := GL_ATC_RGBA_EXPLICIT_ALPHA_AMD;

    tcETC1            : Result := GL_ETC1_RGB8_OES;

    tcASTC_4x4_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_4x4_KHR;
    tcASTC_5x4_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_5x4_KHR;
    tcASTC_5x5_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_5x5_KHR;
    tcASTC_6x5_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_6x5_KHR;
    tcASTC_6x6_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_6x6_KHR;
    tcASTC_8x5_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_8x5_KHR;
    tcASTC_8x6_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_8x6_KHR;
    tcASTC_8x8_RGBA           : Result := GL_COMPRESSED_RGBA_ASTC_8x8_KHR;
    tcASTC_10x5_RGBA          : Result := GL_COMPRESSED_RGBA_ASTC_10x5_KHR;
    tcASTC_10x6_RGBA          : Result := GL_COMPRESSED_RGBA_ASTC_10x6_KHR;
    tcASTC_10x8_RGBA          : Result := GL_COMPRESSED_RGBA_ASTC_10x8_KHR;
    tcASTC_10x10_RGBA         : Result := GL_COMPRESSED_RGBA_ASTC_10x10_KHR;
    tcASTC_12x10_RGBA         : Result := GL_COMPRESSED_RGBA_ASTC_12x10_KHR;
    tcASTC_12x12_RGBA         : Result := GL_COMPRESSED_RGBA_ASTC_12x12_KHR;
    tcASTC_4x4_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR;
    tcASTC_5x4_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR;
    tcASTC_5x5_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR;
    tcASTC_6x5_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR;
    tcASTC_6x6_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR;
    tcASTC_8x5_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR;
    tcASTC_8x6_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR;
    tcASTC_8x8_SRGB8_ALPHA8   : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR;
    tcASTC_10x5_SRGB8_ALPHA8  : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR;
    tcASTC_10x6_SRGB8_ALPHA8  : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR;
    tcASTC_10x8_SRGB8_ALPHA8  : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR;
    tcASTC_10x10_SRGB8_ALPHA8 : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR;
    tcASTC_12x10_SRGB8_ALPHA8 : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR;
    tcASTC_12x12_SRGB8_ALPHA8 : Result := GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR;

    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('Compression value not supported by OpenGL / KTX constant, which should be impossible for current TTextureCompression values.');
    {$endif}
  end;
end;

function OpenGLToTextureCompression(const GlCompression: UInt32; out Compression: TTextureCompression): Boolean;
begin
  Result := true;
  case GlCompression of
    GL_COMPRESSED_RGB_S3TC_DXT1_EXT          : Compression := tcDxt1_RGB;
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT         : Compression := tcDxt1_RGBA;
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT         : Compression := tcDxt3;
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT         : Compression := tcDxt5;

    GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG       : Compression := tcPvrtc1_4bpp_RGB;
    GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG       : Compression := tcPvrtc1_2bpp_RGB;
    GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG      : Compression := tcPvrtc1_4bpp_RGBA;
    GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG      : Compression := tcPvrtc1_2bpp_RGBA;
    GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG      : Compression := tcPvrtc2_4bpp;
    GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG      : Compression := tcPvrtc2_2bpp;

    GL_ATC_RGB_AMD                           : Compression := tcATITC_RGB;
    GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD       : Compression := tcATITC_RGBA_InterpolatedAlpha;
    GL_ATC_RGBA_EXPLICIT_ALPHA_AMD           : Compression := tcATITC_RGBA_ExplicitAlpha;

    GL_ETC1_RGB8_OES                         : Compression := tcETC1;

    GL_COMPRESSED_RGBA_ASTC_4x4_KHR          : Compression := tcASTC_4x4_RGBA;
    GL_COMPRESSED_RGBA_ASTC_5x4_KHR          : Compression := tcASTC_5x4_RGBA;
    GL_COMPRESSED_RGBA_ASTC_5x5_KHR          : Compression := tcASTC_5x5_RGBA;
    GL_COMPRESSED_RGBA_ASTC_6x5_KHR          : Compression := tcASTC_6x5_RGBA;
    GL_COMPRESSED_RGBA_ASTC_6x6_KHR          : Compression := tcASTC_6x6_RGBA;
    GL_COMPRESSED_RGBA_ASTC_8x5_KHR          : Compression := tcASTC_8x5_RGBA;
    GL_COMPRESSED_RGBA_ASTC_8x6_KHR          : Compression := tcASTC_8x6_RGBA;
    GL_COMPRESSED_RGBA_ASTC_8x8_KHR          : Compression := tcASTC_8x8_RGBA;
    GL_COMPRESSED_RGBA_ASTC_10x5_KHR         : Compression := tcASTC_10x5_RGBA;
    GL_COMPRESSED_RGBA_ASTC_10x6_KHR         : Compression := tcASTC_10x6_RGBA;
    GL_COMPRESSED_RGBA_ASTC_10x8_KHR         : Compression := tcASTC_10x8_RGBA;
    GL_COMPRESSED_RGBA_ASTC_10x10_KHR        : Compression := tcASTC_10x10_RGBA;
    GL_COMPRESSED_RGBA_ASTC_12x10_KHR        : Compression := tcASTC_12x10_RGBA;
    GL_COMPRESSED_RGBA_ASTC_12x12_KHR        : Compression := tcASTC_12x12_RGBA;

    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR  : Compression := tcASTC_4x4_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR  : Compression := tcASTC_5x4_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR  : Compression := tcASTC_5x5_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR  : Compression := tcASTC_6x5_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR  : Compression := tcASTC_6x6_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR  : Compression := tcASTC_8x5_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR  : Compression := tcASTC_8x6_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR  : Compression := tcASTC_8x8_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR : Compression := tcASTC_10x5_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR : Compression := tcASTC_10x6_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR : Compression := tcASTC_10x8_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR: Compression := tcASTC_10x10_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR: Compression := tcASTC_12x10_SRGB8_ALPHA8;
    GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR: Compression := tcASTC_12x12_SRGB8_ALPHA8;

    else Result := false;
  end;
end;

{$endif read_implementation}
