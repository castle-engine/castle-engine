{ Constants that specify texture compression for OpenGL.
  Internal for both CastleCompositeImage (used by KTX reader)
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
