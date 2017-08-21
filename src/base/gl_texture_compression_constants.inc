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
