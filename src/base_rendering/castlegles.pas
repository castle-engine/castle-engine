{$ifdef CASTLE_DELPHI_PACKAGE}
  {$if not (defined(ANDROID) or defined(IOS))}
    {$message fatal 'This unit should not be included in CGE Delphi package, which for now is only for Windows OpenGL.'}
  {$endif}
{$endif}

{ OpenGL ES 2 and 3 headers for Castle Game Engine.

  Origin and copyrights:

  - C header: licensed under the SGI Free Software B License Version
    2.0. For details, see http://oss.sgi.com/projects/FreeB/ .

  - Ported/Translated for FreePascal by Benjamin 'BeRo' Rosseaux
    benjamin@rosseaux.com - http://www.rosseaux.com .

  - Added OpenGL ES 3. Trung Le (kagamma).

  - Various modifications by Michalis Kamburelis for Castle Game Engine.
}
unit CastleGLES;

{$i castleconf.inc}

{$ifdef FPC}
  {$packrecords C}
{$endif}

{$ifdef WASI}
  {$message fatal 'This unit should not be used with WebAssembly, instead the CastleInternalWebGL provides compatible API'}
{$endif}

interface

uses CTypes, SysUtils,
  CastleDynLib;

type
  PGLubyte = ^GLubyte;
  PGLboolean  = ^GLboolean;
  PGLenum  = ^GLenum;
  PGLfloat  = ^GLfloat;
  PGLint  = ^GLint;
  PGLsizei  = ^GLsizei;
  PGLuint  = ^GLuint;
  PGLchar = PAnsiChar;
  PPGLchar = PPAnsiChar;

  {-------------------------------------------------------------------------
   * Data type definitions
   *----------------------------------------------------------------------- }

  GLvoid = pointer;
  TGLvoid = GLvoid;

  GLenum = CUInt32;
  TGLenum = GLenum;

  GLboolean = byte;
  TGLboolean = GLboolean;

  GLbitfield = CUInt32;
  TGLbitfield = GLbitfield;

  GLbyte = shortint;
  TGLbyte = GLbyte;

  GLshort = smallint;
  TGLshort = GLshort;

  GLint = CInt32;
  TGLint = GLint;

  GLsizei = CInt32;
  TGLsizei = GLsizei;

  GLubyte = byte;
  TGLubyte = GLubyte;

  GLushort = word;
  TGLushort = GLushort;

  GLuint = CUInt32;
  TGLuint = GLuint;

  GLfloat = single;
  TGLfloat = GLfloat;

  GLclampf = single;
  TGLclampf = GLclampf;

  GLfixed = CInt32;
  TGLfixed = GLfixed;
  { GL types for handling large vertex buffer objects  }

  GLintptr = {$ifdef FPC} PtrInt {$else} NativeInt {$endif};
  TGLintptr = GLintptr;

  GLsizeiptr = {$ifdef FPC} PtrInt {$else} NativeInt {$endif};
  TGLsizeiptr = GLsizeiptr;
  { OpenGL ES 3.0 core versions  }
  GLuint64 = QWord;
  TGLuint64 = GLuint64;
  PGLuint64 = ^GLuint64;

  GLint64 = Int64;
  TGLint64 = GLint64;
  PGLint64 = ^GLint64;

  GLsync = Pointer;
  TGLsync = GLsync;

const
  GL_ES_VERSION_2_0 = 1;
  { ClearBufferMask  }
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;
  { Boolean  }
  GL_FALSE = 0;
  GL_TRUE = 1;
  { BeginMode  }
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  { AlphaFunction (not supported in ES20)  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { BlendingFactorDest  }
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  { BlendingFactorSrc  }
  {      GL_ZERO  }
  {      GL_ONE  }
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
  {      GL_SRC_ALPHA  }
  {      GL_ONE_MINUS_SRC_ALPHA  }
  {      GL_DST_ALPHA  }
  {      GL_ONE_MINUS_DST_ALPHA  }
  { BlendEquationSeparate  }
  GL_FUNC_ADD = $8006;
  GL_BLEND_EQUATION = $8009;
  { same as BLEND_EQUATION  }
  GL_BLEND_EQUATION_RGB = $8009;
  GL_BLEND_EQUATION_ALPHA = $883D;
  { BlendSubtract  }
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  { Separate Blend Functions  }
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  { Buffer Objects  }
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_STREAM_DRAW = $88E0;
  GL_STATIC_DRAW = $88E4;
  GL_DYNAMIC_DRAW = $88E8;
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  { CullFaceMode  }
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_FRONT_AND_BACK = $0408;
  { DepthFunction  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { EnableCap  }
  GL_TEXTURE_2D = $0DE1;
  GL_CULL_FACE = $0B44;
  GL_BLEND = $0BE2;
  GL_DITHER = $0BD0;
  GL_STENCIL_TEST = $0B90;
  GL_DEPTH_TEST = $0B71;
  GL_SCISSOR_TEST = $0C11;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_COVERAGE = $80A0;
  { ErrorCode  }
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;
  { FrontFaceDirection  }
  GL_CW = $0900;
  GL_CCW = $0901;
  { GetPName  }
  GL_LINE_WIDTH = $0B21;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
  GL_VIEWPORT = $0BA2;
  GL_SCISSOR_BOX = $0C10;
  {      GL_SCISSOR_TEST  }
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  {      GL_POLYGON_OFFSET_FILL  }
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  { GetTextureParameter  }
  {      GL_TEXTURE_MAG_FILTER  }
  {      GL_TEXTURE_MIN_FILTER  }
  {      GL_TEXTURE_WRAP_S  }
  {      GL_TEXTURE_WRAP_T  }
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  { HintMode  }
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;
  { HintTarget  }
  GL_GENERATE_MIPMAP_HINT = $8192;
  { DataType  }
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_FIXED = $140C;
  { PixelFormat  }
  GL_DEPTH_COMPONENT = $1902;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  { PixelType  }
  {      GL_UNSIGNED_BYTE  }
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  { Shaders  }
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
  GL_SHADER_TYPE = $8B4F;
  GL_DELETE_STATUS = $8B80;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  { StencilFunction  }
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  { StencilOp  }
  {      GL_ZERO  }
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_INVERT = $150A;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  { StringName  }
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;
  { TextureMagFilter  }
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  { TextureMinFilter  }
  {      GL_NEAREST  }
  {      GL_LINEAR  }
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  { TextureParameterName  }
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  { TextureTarget  }
  {      GL_TEXTURE_2D  }
  GL_TEXTURE = $1702;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  { TextureUnit  }
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  { TextureWrapMode  }
  GL_REPEAT = $2901;
  GL_CLAMP_TO_EDGE = $812F;
  GL_MIRRORED_REPEAT = $8370;
  { Uniform Types  }
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_CUBE = $8B60;
  { Vertex Arrays  }
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  { Read Format  }
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  { Shader Source  }
  GL_COMPILE_STATUS = $8B81;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_SHADER_COMPILER = $8DFA;
  { Shader Binary  }
  GL_SHADER_BINARY_FORMATS = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  { Shader Precision-Specified Types  }
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;
  { Framebuffer Object.  }
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGB565 = $8D62;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_STENCIL_INDEX = $1901;
  GL_STENCIL_INDEX8 = $8D48;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_NONE = 0;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

  {-------------------------------------------------------------------------
   * GL core functions.
   *----------------------------------------------------------------------- }

var
  glActiveTexture_Proc : procedure(texture:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glAttachShader_Proc : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glBindAttribLocation_Proc : procedure(_program:GLuint; index:GLuint; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindBuffer_Proc : procedure(target:GLenum; buffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindFramebuffer_Proc : procedure(target:GLenum; framebuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindRenderbuffer_Proc : procedure(target:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindTexture_Proc : procedure(target:GLenum; texture:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlendColor_Proc : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlendEquation_Proc : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlendEquationSeparate_Proc : procedure(modeRGB:GLenum; modeAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlendFunc_Proc : procedure(sfactor:GLenum; dfactor:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlendFuncSeparate_Proc : procedure(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glBufferData_Proc : procedure(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glBufferSubData_Proc : procedure(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCheckFramebufferStatus_Proc : function(target:GLenum):GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClear_Proc : procedure(mask:GLbitfield);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearColor_Proc : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearDepthf_Proc : procedure(depth:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearStencil_Proc : procedure(s:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glColorMask_Proc : procedure(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCompileShader_Proc : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glCompressedTexImage2D_Proc : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
    border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glCompressedTexSubImage2D_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;
    height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCopyTexImage2D_Proc : procedure(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint;
    width:GLsizei; height:GLsizei; border:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCopyTexSubImage2D_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint;
    y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCreateProgram_Proc : function:GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCreateShader_Proc : function(_type:GLenum):GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCullFace_Proc : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glDeleteBuffers_Proc : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glDeleteFramebuffers_Proc : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteProgram_Proc : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glDeleteRenderbuffers_Proc : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteShader_Proc : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glDeleteTextures_Proc : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDepthFunc_Proc : procedure(func:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDepthMask_Proc : procedure(flag:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDepthRangef_Proc : procedure(zNear:GLclampf; zFar:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDetachShader_Proc : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDisable_Proc : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDisableVertexAttribArray_Proc : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDrawArrays_Proc : procedure(mode:GLenum; first:GLint; count:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glDrawElements_Proc : procedure(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEnable_Proc : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEnableVertexAttribArray_Proc : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFinish_Proc : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFlush_Proc : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFramebufferRenderbuffer_Proc : procedure(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFramebufferTexture2D_Proc : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFrontFace_Proc : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenBuffers_Proc : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenerateMipmap_Proc : procedure(target:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenFramebuffers_Proc : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenRenderbuffers_Proc : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenTextures_Proc : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetActiveAttrib_Proc : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;
    _type:pGLenum; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetActiveUniform_Proc : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;
    _type:pGLenum; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetAttachedShaders_Proc : procedure(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glGetAttribLocation_Proc : function(_program:GLuint; name:PAnsiChar):CInt32;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetBooleanv_Proc : procedure(pname:GLenum; params:pGLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetBufferParameteriv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetError_Proc : function:GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetFloatv_Proc : procedure(pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetFramebufferAttachmentParameteriv_Proc : procedure(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetIntegerv_Proc : procedure(pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetProgramiv_Proc : procedure(_program:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetProgramInfoLog_Proc : procedure(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetRenderbufferParameteriv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetShaderiv_Proc : procedure(shader:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetShaderInfoLog_Proc : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetShaderPrecisionFormat_Proc : procedure(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetShaderSource_Proc : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glGetString_Proc : function(name:GLenum):PGLubyte;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetTexParameterfv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetTexParameteriv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetUniformfv_Proc : procedure(_program:GLuint; location:GLint; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetUniformiv_Proc : procedure(_program:GLuint; location:GLint; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glGetUniformLocation_Proc : function(_program:GLuint; name:PAnsiChar):CInt32;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetVertexAttribfv_Proc : procedure(index:GLuint; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetVertexAttribiv_Proc : procedure(index:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetVertexAttribPointerv_Proc : procedure(index:GLuint; pname:GLenum; pointer:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glHint_Proc : procedure(target:GLenum; mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsBuffer_Proc : function(buffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsEnabled_Proc : function(cap:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsFramebuffer_Proc : function(framebuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsProgram_Proc : function(_program:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsRenderbuffer_Proc : function(renderbuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsShader_Proc : function(shader:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsTexture_Proc : function(texture:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glLineWidth_Proc : procedure(width:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glLinkProgram_Proc : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glPixelStorei_Proc : procedure(pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glPolygonOffset_Proc : procedure(factor:GLfloat; units:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glReadPixels_Proc : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum;
    _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glReleaseShaderCompiler_Proc : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glRenderbufferStorage_Proc : procedure(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSampleCoverage_Proc : procedure(value:GLclampf; invert:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glScissor_Proc : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
  glShaderBinary_Proc : procedure(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
  glShaderSource_Proc : procedure(shader:GLuint; count:GLsizei; _string:PPGLchar; length:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilFunc_Proc : procedure(func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilFuncSeparate_Proc : procedure(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilMask_Proc : procedure(mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilMaskSeparate_Proc : procedure(face:GLenum; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilOp_Proc : procedure(fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glStencilOpSeparate_Proc : procedure(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glTexImage2D_Proc : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
    border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexParameterf_Proc : procedure(target:GLenum; pname:GLenum; param:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glTexParameterfv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexParameteri_Proc : procedure(target:GLenum; pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glTexParameteriv_Proc : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glTexSubImage2D_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;
    height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform1f_Proc : procedure(location:GLint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform1fv_Proc : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform1i_Proc : procedure(location:GLint; x:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform1iv_Proc : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform2f_Proc : procedure(location:GLint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform2fv_Proc : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform2i_Proc : procedure(location:GLint; x:GLint; y:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform2iv_Proc : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform3f_Proc : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform3fv_Proc : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform3i_Proc : procedure(location:GLint; x:GLint; y:GLint; z:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform3iv_Proc : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform4f_Proc : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform4fv_Proc : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform4i_Proc : procedure(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniform4iv_Proc : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniformMatrix2fv_Proc : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniformMatrix3fv_Proc : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glUniformMatrix4fv_Proc : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUseProgram_Proc : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glValidateProgram_Proc : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttrib1f_Proc : procedure(indx:GLuint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glVertexAttrib1fv_Proc : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttrib2f_Proc : procedure(indx:GLuint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glVertexAttrib2fv_Proc : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttrib3f_Proc : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glVertexAttrib3fv_Proc : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttrib4f_Proc : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glVertexAttrib4fv_Proc : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glVertexAttribPointer_Proc : procedure(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei;
    ptr:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glViewport_Proc : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  { OpenGL ES 3.0 APIs }
  glReadBuffer_Proc: procedure(Src: GLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDrawRangeElements_Proc: procedure(Mode: GLenum; Start, Endd: GLuint; Count: GLsizei; Kind: GLenum; Indices: Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexImage3D_Proc: procedure(target:TGLenum; level:TGLint; internalformat:TGLint; width:TGLsizei; height:TGLsizei;
            depth:TGLsizei; border:TGLint; format:TGLenum; _type:TGLenum; pixels:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexSubImage3D_Proc: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
            width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; _type:TGLenum;
            pixels:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCopyTexSubImage3D_Proc: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
            x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCompressedTexImage3D_Proc: procedure(target:TGLenum; level:TGLint; internalformat:TGLenum; width:TGLsizei; height:TGLsizei;
            depth:TGLsizei; border:TGLint; imageSize:TGLsizei; data:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCompressedTexSubImage3D_Proc: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
            width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; imageSize:TGLsizei;
            data:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenQueries_Proc: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteQueries_Proc: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsQuery_Proc: function(id:TGLuint): GLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBeginQuery_Proc: procedure(target:TGLenum; id:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEndQuery_Proc: procedure(target:TGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetQueryiv_Proc: procedure(target:TGLenum; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetQueryObjectuiv_Proc: procedure(id:TGLuint; pname:TGLenum; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUnmapBuffer_Proc: function(Target: GLuint): GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetBufferPointerv_Proc: procedure(target:TGLenum; pname:TGLenum; params:Ppointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDrawBuffers_Proc: procedure(n:TGLsizei; bufs:PGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix2x3fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix3x2fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix2x4fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix4x2fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix3x4fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformMatrix4x3fv_Proc: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBlitFramebuffer_Proc: procedure(srcX0:TGLint; srcY0:TGLint; srcX1:TGLint; srcY1:TGLint; dstX0:TGLint;
            dstY0:TGLint; dstX1:TGLint; dstY1:TGLint; mask:TGLbitfield; filter:TGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glRenderbufferStorageMultisample_Proc: procedure(target:TGLenum; samples:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFramebufferTextureLayer_Proc: procedure(target:TGLenum; attachment:TGLenum; texture:TGLuint; level:TGLint; layer:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glMapBufferRange_Proc: function(Target: GLuint; Offset: GLintptr; Len: GLsizeiptr; Access: GLbitfield): Pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFlushMappedBufferRange_Proc: procedure(target:TGLenum; offset:TGLintptr; length:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindVertexArray_Proc: procedure(A: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteVertexArrays_Proc: procedure(Count: GLuint; P: PGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenVertexArrays_Proc: procedure(Count: GLuint; P: PGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsVertexArray_Proc: function(arr:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetIntegeri_v_Proc: procedure(target:TGLenum; ind:TGLuint; data:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBeginTransformFeedback_Proc: procedure(Mode: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEndTransformFeedback_Proc: procedure();{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindBufferRange_Proc: procedure(target:TGLenum; ind:TGLuint; buffer:TGLuint; offset:TGLintptr; size:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindBufferBase_Proc: procedure(Target, Ind, Buffer: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTransformFeedbackVaryings_Proc: procedure(Prog: GLuint; Count: GLsizei; const Varyings: PPAnsiChar; BufferMode: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetTransformFeedbackVarying_Proc: procedure(prog:TGLuint; ind:TGLuint; bufSize:TGLsizei; length:PGLsizei; size:PGLsizei;
            _type:PGLenum; name:PGLchar); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribIPointer_Proc: procedure(ind:TGLuint; size:TGLint; _type:TGLenum; stride:TGLsizei; p:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetVertexAttribIiv_Proc: procedure(ind:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetVertexAttribIuiv_Proc: procedure(ind:TGLuint; pname:TGLenum; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribI4i_Proc: procedure(ind:TGLuint; x:TGLint; y:TGLint; z:TGLint; w:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribI4ui_Proc: procedure(ind:TGLuint; x:TGLuint; y:TGLuint; z:TGLuint; w:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribI4iv_Proc: procedure(ind:TGLuint; v:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribI4uiv_Proc: procedure(ind:TGLuint; v:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetUniformuiv_Proc: procedure(prog:TGLuint; location:TGLint; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetFragDataLocation_Proc: function(prog:TGLuint; name:PGLchar):TGLint; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform1ui_Proc: procedure(location:TGLint; v0:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform2ui_Proc: procedure(location:TGLint; v0:TGLuint; v1:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform3ui_Proc: procedure(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform4ui_Proc: procedure(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint; v3:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform1uiv_Proc: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform2uiv_Proc: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform3uiv_Proc: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniform4uiv_Proc: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearBufferiv_Proc: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearBufferuiv_Proc: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearBufferfv_Proc: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClearBufferfi_Proc: procedure(buffer:TGLenum; drawbuffer:TGLint; depth:TGLfloat; stencil:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetStringi_Proc: function(name:TGLenum; ind:TGLuint):PGLubyte; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCopyBufferSubData_Proc: procedure(readTarget:TGLenum; writeTarget:TGLenum; readOffset:TGLintptr; writeOffset:TGLintptr; size:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetUniformIndices_Proc: procedure(prog:TGLuint; uniformCount:TGLsizei; uniformNames:PPGLchar; uniformIndices:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetActiveUniformsiv_Proc: procedure(prog:TGLuint; uniformCount:TGLsizei; uniformIndices:PGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetUniformBlockIndex_Proc: function(prog:TGLuint; uniformBlockName:PGLchar):TGLuint; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetActiveUniformBlockiv_Proc: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetActiveUniformBlockName_Proc: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; bufSize:TGLsizei; length:PGLsizei; uniformBlockName:PGLchar); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUniformBlockBinding_Proc: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; uniformBlockBinding:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDrawArraysInstanced_Proc: procedure(Mode: GLuint; First, Count, InstanceCount: GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDrawElementsInstanced_Proc: procedure(Mode: GLuint; Count, Kind: GLsizei; Indices: PGLuint; InstanceCount: GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFenceSync_Proc: function(condition:TGLenum; flags:TGLbitfield):TGLsync; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsSync_Proc: function(sync:TGLsync):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteSync_Proc: procedure(sync:TGLsync); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glClientWaitSync_Proc: function(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64):TGLenum; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glWaitSync_Proc: procedure(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetInteger64v_Proc: procedure(pname:TGLenum; data:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetSynciv_Proc: procedure(sync:TGLsync; pname:TGLenum; count:TGLsizei; length:PGLsizei; values:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetInteger64i_v_Proc: procedure(target:TGLenum; ind:TGLuint; data:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetBufferParameteri64v_Proc: procedure(target:TGLenum; pname:TGLenum; params:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenSamplers_Proc: procedure(count:TGLsizei; samplers:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteSamplers_Proc: procedure(count:TGLsizei; samplers:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsSampler_Proc: function(sampler:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindSampler_Proc: procedure(u:TGLuint; sampler:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSamplerParameteri_Proc: procedure(sampler:TGLuint; pname:TGLenum; param:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSamplerParameteriv_Proc: procedure(sampler:TGLuint; pname:TGLenum; param:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSamplerParameterf_Proc: procedure(sampler:TGLuint; pname:TGLenum; param:TGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSamplerParameterfv_Proc: procedure(sampler:TGLuint; pname:TGLenum; param:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetSamplerParameteriv_Proc: procedure(sampler:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetSamplerParameterfv_Proc: procedure(sampler:TGLuint; pname:TGLenum; params:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glVertexAttribDivisor_Proc: procedure(Ind, Divisor: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBindTransformFeedback_Proc: procedure(target:TGLenum; id:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeleteTransformFeedbacks_Proc: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenTransformFeedbacks_Proc: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glIsTransformFeedback_Proc: function(id:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glPauseTransformFeedback_Proc: procedure(); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glResumeTransformFeedback_Proc: procedure(); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetProgramBinary_Proc: procedure(prog:TGLuint; bufSize:TGLsizei; length:PGLsizei; binaryFormat:PGLenum; binary:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glProgramBinary_Proc: procedure(prog:TGLuint; binaryFormat:TGLenum; binary:pointer; length:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glProgramParameteri_Proc: procedure(prog:TGLuint; pname:TGLenum; value:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glInvalidateFramebuffer_Proc: procedure(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glInvalidateSubFramebuffer_Proc: procedure(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum; x:TGLint; y:TGLint;
            width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexStorage2D_Proc: procedure(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glTexStorage3D_Proc: procedure(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei;
            depth:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetInternalformativ_Proc: procedure(target:TGLenum; internalformat:TGLenum; pname:TGLenum; count:TGLsizei; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}

{------------------------------------------------------------------------*
* IMG extension tokens
*------------------------------------------------------------------------ }

const
  { GL_IMG_binary_shader  }
  GL_SGX_BINARY_IMG = $8C0A;
  { GL_IMG_texture_compression_pvrtc  }
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;
  GL_BGRA = $80E1;

  {------------------------------------------------------------------------*
  * IMG extension functions
  *------------------------------------------------------------------------ }
  { GL_IMG_binary_shader  }
  GL_IMG_binary_shader = 1;
  { GL_IMG_texture_compression_pvrtc  }
  GL_IMG_texture_compression_pvrtc = 1;
  {
  * This document is licensed under the SGI Free Software B License Version
  * 2.0. For details, see http://oss.sgi.com/projects/FreeB/ .
  }

  {------------------------------------------------------------------------*
  * OES extension tokens
  *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
  GL_ETC1_RGB8_OES = $8D64;
  { GL_OES_compressed_paletted_texture  }
  GL_PALETTE4_RGB8_OES = $8B90;
  GL_PALETTE4_RGBA8_OES = $8B91;
  GL_PALETTE4_R5_G6_B5_OES = $8B92;
  GL_PALETTE4_RGBA4_OES = $8B93;
  GL_PALETTE4_RGB5_A1_OES = $8B94;
  GL_PALETTE8_RGB8_OES = $8B95;
  GL_PALETTE8_RGBA8_OES = $8B96;
  GL_PALETTE8_R5_G6_B5_OES = $8B97;
  GL_PALETTE8_RGBA4_OES = $8B98;
  GL_PALETTE8_RGB5_A1_OES = $8B99;
  { GL_OES_depth24  }
  GL_DEPTH_COMPONENT24_OES = $81A6;
  { GL_OES_depth32  }
  GL_DEPTH_COMPONENT32_OES = $81A7;
  { GL_OES_depth_texture  }
  { No new tokens introduced by this extension.  }
  { GL_OES_EGL_image  }

type
  GLeglImageOES = pointer;

{ GL_OES_get_program_binary  }
const
  GL_PROGRAM_BINARY_LENGTH_OES = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS_OES = $87FE;
  GL_PROGRAM_BINARY_FORMATS_OES = $87FF;
  { GL_OES_mapbuffer  }
  GL_WRITE_ONLY_OES = $88B9;
  GL_BUFFER_ACCESS_OES = $88BB;
  GL_BUFFER_MAPPED_OES = $88BC;
  GL_BUFFER_MAP_POINTER_OES = $88BD;
  { GL_OES_packed_depth_stencil  }
  GL_DEPTH_STENCIL_OES = $84F9;
  GL_UNSIGNED_INT_24_8_OES = $84FA;
  GL_DEPTH24_STENCIL8_OES = $88F0;
  { GL_OES_rgb8_rgba8  }
  GL_RGB8_OES = $8051;
  GL_RGBA8_OES = $8058;
  { GL_OES_standard_derivatives  }
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES = $8B8B;
  { GL_OES_stencil1  }
  GL_STENCIL_INDEX1_OES = $8D46;
  { GL_OES_stencil4  }
  GL_STENCIL_INDEX4_OES = $8D47;
  { GL_OES_texture3D  }
  GL_TEXTURE_WRAP_R_OES = $8072;
  GL_TEXTURE_3D_OES = $806F;
  GL_TEXTURE_BINDING_3D_OES = $806A;
  GL_MAX_3D_TEXTURE_SIZE_OES = $8073;
  GL_SAMPLER_3D_OES = $8B5F;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES = $8CD4;
  { GL_OES_texture_half_float  }
  GL_HALF_FLOAT_OES = $8D61;
  { GL_OES_vertex_half_float  }
  { GL_HALF_FLOAT_OES defined in GL_OES_texture_half_float already.  }
  { GL_OES_vertex_type_10_10_10_2  }
  GL_UNSIGNED_INT_10_10_10_2_OES = $8DF6;
  GL_INT_10_10_10_2_OES = $8DF7;

  {------------------------------------------------------------------------*
  * AMD extension tokens
  *------------------------------------------------------------------------ }
  { GL_AMD_compressed_3DC_texture  }
  GL_3DC_X_AMD = $87F9;
  GL_3DC_XY_AMD = $87FA;
  { GL_AMD_compressed_ATC_texture  }
  GL_ATC_RGB_AMD = $8C92;
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = $8C93;
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;
  { GL_AMD_program_binary_Z400  }
  GL_Z400_BINARY_AMD = $8740;
  { GL_AMD_performance_monitor  }
  {$define GL_AMD_performance_monitor}
  GL_COUNTER_TYPE_AMD = $8BC0;
  GL_COUNTER_RANGE_AMD = $8BC1;
  GL_UNSIGNED_INT64_AMD = $8BC2;
  GL_PERCENTAGE_AMD = $8BC3;
  GL_PERFMON_RESULT_AVAILABLE_AMD = $8BC4;
  GL_PERFMON_RESULT_SIZE_AMD = $8BC5;
  GL_PERFMON_RESULT_AMD = $8BC6;

  {------------------------------------------------------------------------*
  * EXT extension tokens
  *------------------------------------------------------------------------ }
  { GL_EXT_texture_filter_anisotropic  }
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  { GL_EXT_texture_type_2_10_10_10_REV  }
  GL_UNSIGNED_INT_2_10_10_10_REV_EXT = $8368;

  {------------------------------------------------------------------------*
  * OES extension functions
  *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
  GL_OES_compressed_ETC1_RGB8_texture = 1;
  { GL_OES_compressed_paletted_texture  }
  GL_OES_compressed_paletted_texture = 1;
  { GL_OES_EGL_image  }

{ OpenGL ES 3.0 constants }
const
  GL_READ_BUFFER = $0C02;
  GL_UNPACK_ROW_LENGTH = $0CF2;
  GL_UNPACK_SKIP_ROWS = $0CF3;
  GL_UNPACK_SKIP_PIXELS = $0CF4;
  GL_PACK_ROW_LENGTH = $0D02;
  GL_PACK_SKIP_ROWS = $0D03;
  GL_PACK_SKIP_PIXELS = $0D04;
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;
  GL_RED = $1903;
  GL_RGB8 = $8051;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPARE_REF_TO_TEXTURE = $884E;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_MAX_VARYING_COMPONENTS = $8B4B;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_DRAW_FRAMEBUFFER_BINDING = $8CA6;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_COLOR_ATTACHMENT16 = $8CF0;
  GL_COLOR_ATTACHMENT17 = $8CF1;
  GL_COLOR_ATTACHMENT18 = $8CF2;
  GL_COLOR_ATTACHMENT19 = $8CF3;
  GL_COLOR_ATTACHMENT20 = $8CF4;
  GL_COLOR_ATTACHMENT21 = $8CF5;
  GL_COLOR_ATTACHMENT22 = $8CF6;
  GL_COLOR_ATTACHMENT23 = $8CF7;
  GL_COLOR_ATTACHMENT24 = $8CF8;
  GL_COLOR_ATTACHMENT25 = $8CF9;
  GL_COLOR_ATTACHMENT26 = $8CFA;
  GL_COLOR_ATTACHMENT27 = $8CFB;
  GL_COLOR_ATTACHMENT28 = $8CFC;
  GL_COLOR_ATTACHMENT29 = $8CFD;
  GL_COLOR_ATTACHMENT30 = $8CFE;
  GL_COLOR_ATTACHMENT31 = $8CFF;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
  GL_HALF_FLOAT = $140B;
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;
  GL_R8 = $8229;
  GL_RG8 = $822B;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;
  GL_VERTEX_ARRAY_BINDING = $85B5;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART_FIXED_INDEX = $8D69;
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;
  GL_COPY_READ_BUFFER_BINDING = $8F36;
  GL_COPY_WRITE_BUFFER_BINDING = $8F37;
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = $FFFFFFFF;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_TIMEOUT_IGNORED = $FFFFFFFFFFFFFFFF;
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
  GL_ANY_SAMPLES_PASSED = $8C2F;
  GL_ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
  GL_SAMPLER_BINDING = $8919;
  GL_RGB10_A2UI = $906F;
  GL_TEXTURE_SWIZZLE_R = $8E42;
  GL_TEXTURE_SWIZZLE_G = $8E43;
  GL_TEXTURE_SWIZZLE_B = $8E44;
  GL_TEXTURE_SWIZZLE_A = $8E45;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_INT_2_10_10_10_REV = $8D9F;
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
  GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
  GL_PROGRAM_BINARY_LENGTH = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS = $87FE;
  GL_PROGRAM_BINARY_FORMATS = $87FF;
  GL_COMPRESSED_R11_EAC = $9270;
  GL_COMPRESSED_SIGNED_R11_EAC = $9271;
  GL_COMPRESSED_RG11_EAC = $9272;
  GL_COMPRESSED_SIGNED_RG11_EAC = $9273;
  GL_COMPRESSED_RGB8_ETC2 = $9274;
  GL_COMPRESSED_SRGB8_ETC2 = $9275;
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9276;
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
  GL_COMPRESSED_RGBA8_ETC2_EAC = $9278;
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = $9279;
  GL_TEXTURE_IMMUTABLE_FORMAT = $912F;
  GL_MAX_ELEMENT_INDEX = $8D6B;
  GL_NUM_SAMPLE_COUNTS = $9380;
  GL_TEXTURE_IMMUTABLE_LEVELS = $82DF;

var
  glEGLImageTargetTexture2DOES_Proc : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEGLImageTargetRenderbufferStorageOES_Proc : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

const
  { GL_OES_depth24  }
  GL_OES_depth24 = 1;
  { GL_OES_depth32  }
  GL_OES_depth32 = 1;
  { GL_OES_depth_texture  }
  GL_OES_depth_texture = 1;
  { GL_OES_element_index_uint  }
  GL_OES_element_index_uint = 1;
  { GL_OES_fbo_render_mipmap  }
  GL_OES_fbo_render_mipmap = 1;
  { GL_OES_fragment_precision_high  }
  GL_OES_fragment_precision_high = 1;
  { GL_OES_get_program_binary  }

var
  glGetProgramBinaryOES_Proc : procedure(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  glProgramBinaryOES_Proc : procedure(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  (* Const before type ignored *)
  { GL_OES_mapbuffer  }

const
  GL_OES_mapbuffer = 1;

var
  glMapBufferOES_Proc : function(target:GLenum; access:GLenum):pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glUnmapBufferOES_Proc : function(target:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetBufferPointervOES_Proc : procedure(target:GLenum; pname:GLenum; params:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

type
  PFNGLMAPBUFFEROESPROC = pointer;

const
  { GL_OES_packed_depth_stencil  }
  GL_OES_packed_depth_stencil = 1;
  { GL_OES_rgb8_rgba8  }
  GL_OES_rgb8_rgba8 = 1;
  { GL_OES_standard_derivatives  }
  GL_OES_standard_derivatives = 1;
  { GL_OES_stencil1  }
  GL_OES_stencil1 = 1;
  { GL_OES_stencil4  }
  GL_OES_stencil4 = 1;
  { GL_OES_texture_3D  }
  GL_OES_texture_3D = 1;
  (* Const before type ignored *)

var
  glTexImage3DOES_Proc : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
    depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glTexSubImage3DOES_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
    width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum;
    pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glCopyTexSubImage3DOES_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
    x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glCompressedTexImage3DOES_Proc : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
    depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
  glCompressedTexSubImage3DOES_Proc : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
    width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei;
    data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glFramebufferTexture3DOES_Proc : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint;
    zoffset:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  { GL_OES_texture_float_linear  }

const
  GL_OES_texture_float_linear = 1;
  { GL_OES_texture_half_float_linear  }
  GL_OES_texture_half_float_linear = 1;
  { GL_OES_texture_float  }
  GL_OES_texture_float = 1;
  { GL_OES_texture_half_float  }
  GL_OES_texture_half_float = 1;
  { GL_OES_texture_npot  }
  GL_OES_texture_npot = 1;
  { GL_OES_vertex_half_float  }
  GL_OES_vertex_half_float = 1;
  { GL_OES_vertex_type_10_10_10_2  }
  GL_OES_vertex_type_10_10_10_2 = 1;

  { GL_AMD_compressed_3DC_texture  }
  GL_AMD_compressed_3DC_texture = 1;
  { GL_AMD_compressed_ATC_texture  }
  GL_AMD_compressed_ATC_texture = 1;
  { GL_AMD_program_binary_Z400  }
  GL_AMD_program_binary_Z400 = 1;
  { AMD_performance_monitor  }
  GL_AMD_performance_monitor = 1;

var
  glGetPerfMonitorGroupsAMD_Proc : procedure(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetPerfMonitorCountersAMD_Proc : procedure(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetPerfMonitorGroupStringAMD_Proc : procedure(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetPerfMonitorCounterStringAMD_Proc : procedure(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetPerfMonitorCounterInfoAMD_Proc : procedure(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGenPerfMonitorsAMD_Proc : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDeletePerfMonitorsAMD_Proc : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glSelectPerfMonitorCountersAMD_Proc : procedure(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glBeginPerfMonitorAMD_Proc : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glEndPerfMonitorAMD_Proc : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glGetPerfMonitorCounterDataAMD_Proc : procedure(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

{ GL_EXT_texture_filter_anisotropic  }

const
  GL_EXT_texture_filter_anisotropic = 1;
  { GL_EXT_texture_type_2_10_10_10_REV  }
  GL_EXT_texture_type_2_10_10_10_REV = 1;

{ GL_KHR_debug }

const
  GL_DEBUG_OUTPUT = $92E0;
  GL_CONTEXT_FLAG_DEBUG_BIT = $00000002;
  GL_STACK_OVERFLOW = $0503;
  GL_STACK_UNDERFLOW = $0504;
  GL_DEBUG_OUTPUT_SYNCHRONOUS = $8242;
  GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH = $8243;
  GL_DEBUG_CALLBACK_FUNCTION = $8244;
  GL_DEBUG_CALLBACK_USER_PARAM = $8245;
  GL_DEBUG_SOURCE_API = $8246;
  GL_DEBUG_SOURCE_WINDOW_SYSTEM = $8247;
  GL_DEBUG_SOURCE_SHADER_COMPILER = $8248;
  GL_DEBUG_SOURCE_THIRD_PARTY = $8249;
  GL_DEBUG_SOURCE_APPLICATION = $824A;
  GL_DEBUG_SOURCE_OTHER = $824B;
  GL_DEBUG_TYPE_ERROR = $824C;
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR = $824D;
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR = $824E;
  GL_DEBUG_TYPE_PORTABILITY = $824F;
  GL_DEBUG_TYPE_PERFORMANCE = $8250;
  GL_DEBUG_TYPE_OTHER = $8251;
  GL_DEBUG_TYPE_MARKER = $8268;
  GL_DEBUG_TYPE_PUSH_GROUP = $8269;
  GL_DEBUG_TYPE_POP_GROUP = $826A;
  GL_DEBUG_SEVERITY_NOTIFICATION = $826B;
  GL_MAX_DEBUG_GROUP_STACK_DEPTH = $826C;
  GL_DEBUG_GROUP_STACK_DEPTH = $826D;
  GL_BUFFER = $82E0;
  GL_SHADER = $82E1;
  GL_PROGRAM = $82E2;
  GL_QUERY = $82E3;
  GL_PROGRAM_PIPELINE = $82E4;
  GL_SAMPLER = $82E6;
  GL_DISPLAY_LIST = $82E7;
  GL_MAX_LABEL_LENGTH = $82E8;
  GL_MAX_DEBUG_MESSAGE_LENGTH = $9143;
  GL_MAX_DEBUG_LOGGED_MESSAGES = $9144;
  GL_DEBUG_LOGGED_MESSAGES = $9145;
  GL_DEBUG_SEVERITY_HIGH = $9146;
  GL_DEBUG_SEVERITY_MEDIUM = $9147;
  GL_DEBUG_SEVERITY_LOW = $9148;

type
  GLDEBUGPROC = procedure (source:GLenum; _type:GLenum; id:GLuint; severity:GLenum; length:GLsizei; message:PGLchar; userParam:Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}

var
  glDebugMessageCallback_Proc:procedure (callback:GLDEBUGPROC; userParam:Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
  glDebugMessageControl_Proc :procedure (source:GLenum; _type:GLenum; severity:GLenum; count:GLsizei; ids:PGLuint; enabled:GLboolean); {$ifdef windows}stdcall;{$else}cdecl;{$endif}


{ Wrapper procedures that log calls and check errors. }
function GLEnumName(const E: GLenum): String;
procedure glActiveTexture(texture:GLenum);
procedure glAttachShader(_program:GLuint; shader:GLuint);
procedure glBindAttribLocation(_program:GLuint; index:GLuint; name:PAnsiChar);
procedure glBindBuffer(target:GLenum; buffer:GLuint);
procedure glBindFramebuffer(target:GLenum; framebuffer:GLuint);
procedure glBindRenderbuffer(target:GLenum; renderbuffer:GLuint);
procedure glBindTexture(target:GLenum; texture:GLuint);
procedure glBlendColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);
procedure glBlendEquation(mode:GLenum);
procedure glBlendEquationSeparate(modeRGB:GLenum; modeAlpha:GLenum);
procedure glBlendFunc(sfactor:GLenum; dfactor:GLenum);
procedure glBlendFuncSeparate(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);
procedure glBufferData(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);
procedure glBufferSubData(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);
function glCheckFramebufferStatus(target:GLenum): GLenum;
procedure glClear(mask:GLbitfield);
procedure glClearColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);
procedure glClearDepthf(depth:GLclampf);
procedure glClearStencil(s:GLint);
procedure glColorMask(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);
procedure glCompileShader(shader:GLuint);
procedure glCompressedTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);
procedure glCompressedTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei; height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);
procedure glCopyTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint; width:GLsizei; height:GLsizei; border:GLint);
procedure glCopyTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint; y:GLint; width:GLsizei; height:GLsizei);
function glCreateProgram: GLuint;
function glCreateShader(_type:GLenum): GLuint;
procedure glCullFace(mode:GLenum);
procedure glDeleteBuffers(n:GLsizei; buffers:pGLuint);
procedure glDeleteFramebuffers(n:GLsizei; framebuffers:pGLuint);
procedure glDeleteProgram(_program:GLuint);
procedure glDeleteRenderbuffers(n:GLsizei; renderbuffers:pGLuint);
procedure glDeleteShader(shader:GLuint);
procedure glDeleteTextures(n:GLsizei; textures:pGLuint);
procedure glDepthFunc(func:GLenum);
procedure glDepthMask(flag:GLboolean);
procedure glDepthRangef(zNear:GLclampf; zFar:GLclampf);
procedure glDetachShader(_program:GLuint; shader:GLuint);
procedure glDisable(cap:GLenum);
procedure glDisableVertexAttribArray(index:GLuint);
procedure glDrawArrays(mode:GLenum; first:GLint; count:GLsizei);
procedure glDrawElements(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);
procedure glEnable(cap:GLenum);
procedure glEnableVertexAttribArray(index:GLuint);
procedure glFinish;
procedure glFlush;
procedure glFramebufferRenderbuffer(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);
procedure glFramebufferTexture2D(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);
procedure glFrontFace(mode:GLenum);
procedure glGenBuffers(n:GLsizei; buffers:pGLuint);
procedure glGenerateMipmap(target:GLenum);
procedure glGenFramebuffers(n:GLsizei; framebuffers:pGLuint);
procedure glGenRenderbuffers(n:GLsizei; renderbuffers:pGLuint);
procedure glGenTextures(n:GLsizei; textures:pGLuint);
procedure glGetActiveAttrib(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; _type:pGLenum; name:PAnsiChar);
procedure glGetActiveUniform(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; _type:pGLenum; name:PAnsiChar);
procedure glGetAttachedShaders(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);
function glGetAttribLocation(_program:GLuint; name:PAnsiChar): CInt32;
procedure glGetBooleanv(pname:GLenum; params:pGLboolean);
procedure glGetBufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);
function glGetError: GLenum;
procedure glGetFloatv(pname:GLenum; params:pGLfloat);
procedure glGetFramebufferAttachmentParameteriv(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);
procedure glGetIntegerv(pname:GLenum; params:pGLint);
procedure glGetProgramiv(_program:GLuint; pname:GLenum; params:pGLint);
procedure glGetProgramInfoLog(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);
procedure glGetRenderbufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);
procedure glGetShaderiv(shader:GLuint; pname:GLenum; params:pGLint);
procedure glGetShaderInfoLog(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);
procedure glGetShaderPrecisionFormat(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);
procedure glGetShaderSource(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:PAnsiChar);
function glGetString(name:GLenum): PGLubyte;
procedure glGetTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);
procedure glGetTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);
procedure glGetUniformfv(_program:GLuint; location:GLint; params:pGLfloat);
procedure glGetUniformiv(_program:GLuint; location:GLint; params:pGLint);
function glGetUniformLocation(_program:GLuint; name:PAnsiChar): CInt32;
procedure glGetVertexAttribfv(index:GLuint; pname:GLenum; params:pGLfloat);
procedure glGetVertexAttribiv(index:GLuint; pname:GLenum; params:pGLint);
procedure glGetVertexAttribPointerv(index:GLuint; pname:GLenum; pointer:Ppointer);
procedure glHint(target:GLenum; mode:GLenum);
function glIsBuffer(buffer:GLuint): GLboolean;
function glIsEnabled(cap:GLenum): GLboolean;
function glIsFramebuffer(framebuffer:GLuint): GLboolean;
function glIsProgram(_program:GLuint): GLboolean;
function glIsRenderbuffer(renderbuffer:GLuint): GLboolean;
function glIsShader(shader:GLuint): GLboolean;
function glIsTexture(texture:GLuint): GLboolean;
procedure glLineWidth(width:GLfloat);
procedure glLinkProgram(_program:GLuint);
procedure glPixelStorei(pname:GLenum; param:GLint);
procedure glPolygonOffset(factor:GLfloat; units:GLfloat);
procedure glReadPixels(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
procedure glReleaseShaderCompiler;
procedure glRenderbufferStorage(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);
procedure glSampleCoverage(value:GLclampf; invert:GLboolean);
procedure glScissor(x:GLint; y:GLint; width:GLsizei; height:GLsizei);
procedure glShaderBinary(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);
procedure glShaderSource(shader:GLuint; count:GLsizei; _string:PPGLchar; length:pGLint);
procedure glStencilFunc(func:GLenum; ref:GLint; mask:GLuint);
procedure glStencilFuncSeparate(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);
procedure glStencilMask(mask:GLuint);
procedure glStencilMaskSeparate(face:GLenum; mask:GLuint);
procedure glStencilOp(fail:GLenum; zfail:GLenum; zpass:GLenum);
procedure glStencilOpSeparate(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);
procedure glTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);
procedure glTexParameterf(target:GLenum; pname:GLenum; param:GLfloat);
procedure glTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);
procedure glTexParameteri(target:GLenum; pname:GLenum; param:GLint);
procedure glTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);
procedure glTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei; height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
procedure glUniform1f(location:GLint; x:GLfloat);
procedure glUniform1fv(location:GLint; count:GLsizei; v:pGLfloat);
procedure glUniform1i(location:GLint; x:GLint);
procedure glUniform1iv(location:GLint; count:GLsizei; v:pGLint);
procedure glUniform2f(location:GLint; x:GLfloat; y:GLfloat);
procedure glUniform2fv(location:GLint; count:GLsizei; v:pGLfloat);
procedure glUniform2i(location:GLint; x:GLint; y:GLint);
procedure glUniform2iv(location:GLint; count:GLsizei; v:pGLint);
procedure glUniform3f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);
procedure glUniform3fv(location:GLint; count:GLsizei; v:pGLfloat);
procedure glUniform3i(location:GLint; x:GLint; y:GLint; z:GLint);
procedure glUniform3iv(location:GLint; count:GLsizei; v:pGLint);
procedure glUniform4f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);
procedure glUniform4fv(location:GLint; count:GLsizei; v:pGLfloat);
procedure glUniform4i(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);
procedure glUniform4iv(location:GLint; count:GLsizei; v:pGLint);
procedure glUniformMatrix2fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
procedure glUniformMatrix3fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
procedure glUniformMatrix4fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
procedure glUseProgram(_program:GLuint);
procedure glValidateProgram(_program:GLuint);
procedure glVertexAttrib1f(indx:GLuint; x:GLfloat);
procedure glVertexAttrib1fv(indx:GLuint; values:pGLfloat);
procedure glVertexAttrib2f(indx:GLuint; x:GLfloat; y:GLfloat);
procedure glVertexAttrib2fv(indx:GLuint; values:pGLfloat);
procedure glVertexAttrib3f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);
procedure glVertexAttrib3fv(indx:GLuint; values:pGLfloat);
procedure glVertexAttrib4f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);
procedure glVertexAttrib4fv(indx:GLuint; values:pGLfloat);
procedure glVertexAttribPointer(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei; ptr:pointer);
procedure glViewport(x:GLint; y:GLint; width:GLsizei; height:GLsizei);
procedure glReadBuffer(Src: GLenum);
procedure glDrawRangeElements(Mode: GLenum; Start, Endd: GLuint; Count: GLsizei; Kind: GLenum; Indices: Pointer);
procedure glTexImage3D(target:TGLenum; level:TGLint; internalformat:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; border:TGLint; format:TGLenum; _type:TGLenum; pixels:pointer);
procedure glTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; _type:TGLenum; pixels:pointer);
procedure glCopyTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei);
procedure glCompressedTexImage3D(target:TGLenum; level:TGLint; internalformat:TGLenum; width:TGLsizei; height:TGLsizei; depth:TGLsizei; border:TGLint; imageSize:TGLsizei; data:pointer);
procedure glCompressedTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; imageSize:TGLsizei; data:pointer);
procedure glGenQueries(n:TGLsizei; ids:PGLuint);
procedure glDeleteQueries(n:TGLsizei; ids:PGLuint);
function glIsQuery(id:TGLuint): GLboolean;
procedure glBeginQuery(target:TGLenum; id:TGLuint);
procedure glEndQuery(target:TGLenum);
procedure glGetQueryiv(target:TGLenum; pname:TGLenum; params:PGLint);
procedure glGetQueryObjectuiv(id:TGLuint; pname:TGLenum; params:PGLuint);
function glUnmapBuffer(Target: GLuint): GLboolean;
procedure glGetBufferPointerv(target:TGLenum; pname:TGLenum; params:Ppointer);
procedure glDrawBuffers(n:TGLsizei; bufs:PGLenum);
procedure glUniformMatrix2x3fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glUniformMatrix3x2fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glUniformMatrix2x4fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glUniformMatrix4x2fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glUniformMatrix3x4fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glUniformMatrix4x3fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
procedure glBlitFramebuffer(srcX0:TGLint; srcY0:TGLint; srcX1:TGLint; srcY1:TGLint; dstX0:TGLint; dstY0:TGLint; dstX1:TGLint; dstY1:TGLint; mask:TGLbitfield; filter:TGLenum);
procedure glRenderbufferStorageMultisample(target:TGLenum; samples:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei);
procedure glFramebufferTextureLayer(target:TGLenum; attachment:TGLenum; texture:TGLuint; level:TGLint; layer:TGLint);
function glMapBufferRange(Target: GLuint; Offset: GLintptr; Len: GLsizeiptr; Access: GLbitfield): Pointer;
procedure glFlushMappedBufferRange(target:TGLenum; offset:TGLintptr; length:TGLsizeiptr);
procedure glBindVertexArray(A: GLuint);
procedure glDeleteVertexArrays(Count: GLuint; P: PGLuint);
procedure glGenVertexArrays(Count: GLuint; P: PGLuint);
function glIsVertexArray(arr:TGLuint): TGLboolean;
procedure glGetIntegeri_v(target:TGLenum; ind:TGLuint; data:PGLint);
procedure glBeginTransformFeedback(Mode: GLuint);
procedure glEndTransformFeedback;
procedure glBindBufferRange(target:TGLenum; ind:TGLuint; buffer:TGLuint; offset:TGLintptr; size:TGLsizeiptr);
procedure glBindBufferBase(Target, Ind, Buffer: GLuint);
procedure glTransformFeedbackVaryings(Prog: GLuint; Count: GLsizei; const Varyings: PPAnsiChar; BufferMode: GLuint);
procedure glGetTransformFeedbackVarying(prog:TGLuint; ind:TGLuint; bufSize:TGLsizei; length:PGLsizei; size:PGLsizei; _type:PGLenum; name:PGLchar);
procedure glVertexAttribIPointer(ind:TGLuint; size:TGLint; _type:TGLenum; stride:TGLsizei; p:pointer);
procedure glGetVertexAttribIiv(ind:TGLuint; pname:TGLenum; params:PGLint);
procedure glGetVertexAttribIuiv(ind:TGLuint; pname:TGLenum; params:PGLuint);
procedure glVertexAttribI4i(ind:TGLuint; x:TGLint; y:TGLint; z:TGLint; w:TGLint);
procedure glVertexAttribI4ui(ind:TGLuint; x:TGLuint; y:TGLuint; z:TGLuint; w:TGLuint);
procedure glVertexAttribI4iv(ind:TGLuint; v:PGLint);
procedure glVertexAttribI4uiv(ind:TGLuint; v:PGLuint);
procedure glGetUniformuiv(prog:TGLuint; location:TGLint; params:PGLuint);
function glGetFragDataLocation(prog:TGLuint; name:PGLchar): TGLint;
procedure glUniform1ui(location:TGLint; v0:TGLuint);
procedure glUniform2ui(location:TGLint; v0:TGLuint; v1:TGLuint);
procedure glUniform3ui(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint);
procedure glUniform4ui(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint; v3:TGLuint);
procedure glUniform1uiv(location:TGLint; count:TGLsizei; value:PGLuint);
procedure glUniform2uiv(location:TGLint; count:TGLsizei; value:PGLuint);
procedure glUniform3uiv(location:TGLint; count:TGLsizei; value:PGLuint);
procedure glUniform4uiv(location:TGLint; count:TGLsizei; value:PGLuint);
procedure glClearBufferiv(buffer:TGLenum; drawbuffer:TGLint; value:PGLint);
procedure glClearBufferuiv(buffer:TGLenum; drawbuffer:TGLint; value:PGLuint);
procedure glClearBufferfv(buffer:TGLenum; drawbuffer:TGLint; value:PGLfloat);
procedure glClearBufferfi(buffer:TGLenum; drawbuffer:TGLint; depth:TGLfloat; stencil:TGLint);
function glGetStringi(name:TGLenum; ind:TGLuint): PGLubyte;
procedure glCopyBufferSubData(readTarget:TGLenum; writeTarget:TGLenum; readOffset:TGLintptr; writeOffset:TGLintptr; size:TGLsizeiptr);
procedure glGetUniformIndices(prog:TGLuint; uniformCount:TGLsizei; uniformNames:PPGLchar; uniformIndices:PGLuint);
procedure glGetActiveUniformsiv(prog:TGLuint; uniformCount:TGLsizei; uniformIndices:PGLuint; pname:TGLenum; params:PGLint);
function glGetUniformBlockIndex(prog:TGLuint; uniformBlockName:PGLchar): TGLuint;
procedure glGetActiveUniformBlockiv(prog:TGLuint; uniformBlockIndex:TGLuint; pname:TGLenum; params:PGLint);
procedure glGetActiveUniformBlockName(prog:TGLuint; uniformBlockIndex:TGLuint; bufSize:TGLsizei; length:PGLsizei; uniformBlockName:PGLchar);
procedure glUniformBlockBinding(prog:TGLuint; uniformBlockIndex:TGLuint; uniformBlockBinding:TGLuint);
procedure glDrawArraysInstanced(Mode: GLuint; First, Count, InstanceCount: GLsizei);
procedure glDrawElementsInstanced(Mode: GLuint; Count, Kind: GLsizei; Indices: PGLuint; InstanceCount: GLsizei);
function glFenceSync(condition:TGLenum; flags:TGLbitfield): TGLsync;
function glIsSync(sync:TGLsync): TGLboolean;
procedure glDeleteSync(sync:TGLsync);
function glClientWaitSync(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64): TGLenum;
procedure glWaitSync(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64);
procedure glGetInteger64v(pname:TGLenum; data:PGLint64);
procedure glGetSynciv(sync:TGLsync; pname:TGLenum; count:TGLsizei; length:PGLsizei; values:PGLint);
procedure glGetInteger64i_v(target:TGLenum; ind:TGLuint; data:PGLint64);
procedure glGetBufferParameteri64v(target:TGLenum; pname:TGLenum; params:PGLint64);
procedure glGenSamplers(count:TGLsizei; samplers:PGLuint);
procedure glDeleteSamplers(count:TGLsizei; samplers:PGLuint);
function glIsSampler(sampler:TGLuint): TGLboolean;
procedure glBindSampler(u:TGLuint; sampler:TGLuint);
procedure glSamplerParameteri(sampler:TGLuint; pname:TGLenum; param:TGLint);
procedure glSamplerParameteriv(sampler:TGLuint; pname:TGLenum; param:PGLint);
procedure glSamplerParameterf(sampler:TGLuint; pname:TGLenum; param:TGLfloat);
procedure glSamplerParameterfv(sampler:TGLuint; pname:TGLenum; param:PGLfloat);
procedure glGetSamplerParameteriv(sampler:TGLuint; pname:TGLenum; params:PGLint);
procedure glGetSamplerParameterfv(sampler:TGLuint; pname:TGLenum; params:PGLfloat);
procedure glVertexAttribDivisor(Ind, Divisor: GLuint);
procedure glBindTransformFeedback(target:TGLenum; id:TGLuint);
procedure glDeleteTransformFeedbacks(n:TGLsizei; ids:PGLuint);
procedure glGenTransformFeedbacks(n:TGLsizei; ids:PGLuint);
function glIsTransformFeedback(id:TGLuint): TGLboolean;
procedure glPauseTransformFeedback;
procedure glResumeTransformFeedback;
procedure glGetProgramBinary(prog:TGLuint; bufSize:TGLsizei; length:PGLsizei; binaryFormat:PGLenum; binary:pointer);
procedure glProgramBinary(prog:TGLuint; binaryFormat:TGLenum; binary:pointer; length:TGLsizei);
procedure glProgramParameteri(prog:TGLuint; pname:TGLenum; value:TGLint);
procedure glInvalidateFramebuffer(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum);
procedure glInvalidateSubFramebuffer(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum; x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei);
procedure glTexStorage2D(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei);
procedure glTexStorage3D(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei; depth:TGLsizei);
procedure glGetInternalformativ(target:TGLenum; internalformat:TGLenum; pname:TGLenum; count:TGLsizei; params:PGLint);
procedure glEGLImageTargetTexture2DOES(target:GLenum; image:GLeglImageOES);
procedure glEGLImageTargetRenderbufferStorageOES(target:GLenum; image:GLeglImageOES);
procedure glGetProgramBinaryOES(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);
procedure glProgramBinaryOES(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);
function glMapBufferOES(target:GLenum; access:GLenum): pointer;
function glUnmapBufferOES(target:GLenum): GLboolean;
procedure glGetBufferPointervOES(target:GLenum; pname:GLenum; params:Ppointer);
procedure glTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);
procedure glTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
procedure glCopyTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; x:GLint; y:GLint; width:GLsizei; height:GLsizei);
procedure glCompressedTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);
procedure glCompressedTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);
procedure glFramebufferTexture3DOES(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint; zoffset:GLint);
procedure glGetPerfMonitorGroupsAMD(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);
procedure glGetPerfMonitorCountersAMD(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);
procedure glGetPerfMonitorGroupStringAMD(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:PAnsiChar);
procedure glGetPerfMonitorCounterStringAMD(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:PAnsiChar);
procedure glGetPerfMonitorCounterInfoAMD(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);
procedure glGenPerfMonitorsAMD(n:GLsizei; monitors:pGLuint);
procedure glDeletePerfMonitorsAMD(n:GLsizei; monitors:pGLuint);
procedure glSelectPerfMonitorCountersAMD(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);
procedure glBeginPerfMonitorAMD(monitor:GLuint);
procedure glEndPerfMonitorAMD(monitor:GLuint);
procedure glGetPerfMonitorCounterDataAMD(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);
procedure glDebugMessageCallback(callback:GLDEBUGPROC; userParam:Pointer);
procedure glDebugMessageControl(source:GLenum; _type:GLenum; severity:GLenum; count:GLsizei; ids:PGLuint; enabled:GLboolean);

procedure GLESInitialization;

implementation

uses CastleInternalEgl, CastleLog;

var
  GLESLib: TDynLib = nil;

procedure FreeGLES;
begin
  FreeAndNil(GLESLib);

  glActiveTexture_Proc:=nil;
  glAttachShader_Proc:=nil;
  glBindAttribLocation_Proc:=nil;
  glBindBuffer_Proc:=nil;
  glBindFramebuffer_Proc:=nil;
  glBindRenderbuffer_Proc:=nil;
  glBindTexture_Proc:=nil;
  glBlendColor_Proc:=nil;
  glBlendEquation_Proc:=nil;
  glBlendEquationSeparate_Proc:=nil;
  glBlendFunc_Proc:=nil;
  glBlendFuncSeparate_Proc:=nil;
  glBufferData_Proc:=nil;
  glBufferSubData_Proc:=nil;
  glCheckFramebufferStatus_Proc:=nil;
  glClear_Proc:=nil;
  glClearColor_Proc:=nil;
  glClearDepthf_Proc:=nil;
  glClearStencil_Proc:=nil;
  glColorMask_Proc:=nil;
  glCompileShader_Proc:=nil;
  glCompressedTexImage2D_Proc:=nil;
  glCompressedTexSubImage2D_Proc:=nil;
  glCopyTexImage2D_Proc:=nil;
  glCopyTexSubImage2D_Proc:=nil;
  glCreateProgram_Proc:=nil;
  glCreateShader_Proc:=nil;
  glCullFace_Proc:=nil;
  glDeleteBuffers_Proc:=nil;
  glDeleteFramebuffers_Proc:=nil;
  glDeleteProgram_Proc:=nil;
  glDeleteRenderbuffers_Proc:=nil;
  glDeleteShader_Proc:=nil;
  glDeleteTextures_Proc:=nil;
  glDepthFunc_Proc:=nil;
  glDepthMask_Proc:=nil;
  glDepthRangef_Proc:=nil;
  glDetachShader_Proc:=nil;
  glDisable_Proc:=nil;
  glDisableVertexAttribArray_Proc:=nil;
  glDrawArrays_Proc:=nil;
  glDrawElements_Proc:=nil;
  glEnable_Proc:=nil;
  glEnableVertexAttribArray_Proc:=nil;
  glFinish_Proc:=nil;
  glFlush_Proc:=nil;
  glFramebufferRenderbuffer_Proc:=nil;
  glFramebufferTexture2D_Proc:=nil;
  glFrontFace_Proc:=nil;
  glGenBuffers_Proc:=nil;
  glGenerateMipmap_Proc:=nil;
  glGenFramebuffers_Proc:=nil;
  glGenRenderbuffers_Proc:=nil;
  glGenTextures_Proc:=nil;
  glGetActiveAttrib_Proc:=nil;
  glGetActiveUniform_Proc:=nil;
  glGetAttachedShaders_Proc:=nil;
  glGetAttribLocation_Proc:=nil;
  glGetBooleanv_Proc:=nil;
  glGetBufferParameteriv_Proc:=nil;
  glGetError_Proc:=nil;
  glGetFloatv_Proc:=nil;
  glGetFramebufferAttachmentParameteriv_Proc:=nil;
  glGetIntegerv_Proc:=nil;
  glGetProgramiv_Proc:=nil;
  glGetProgramInfoLog_Proc:=nil;
  glGetRenderbufferParameteriv_Proc:=nil;
  glGetShaderiv_Proc:=nil;
  glGetShaderInfoLog_Proc:=nil;
  glGetShaderPrecisionFormat_Proc:=nil;
  glGetShaderSource_Proc:=nil;
  glGetString_Proc:=nil;
  glGetTexParameterfv_Proc:=nil;
  glGetTexParameteriv_Proc:=nil;
  glGetUniformfv_Proc:=nil;
  glGetUniformiv_Proc:=nil;
  glGetUniformLocation_Proc:=nil;
  glGetVertexAttribfv_Proc:=nil;
  glGetVertexAttribiv_Proc:=nil;
  glGetVertexAttribPointerv_Proc:=nil;
  glHint_Proc:=nil;
  glIsBuffer_Proc:=nil;
  glIsEnabled_Proc:=nil;
  glIsFramebuffer_Proc:=nil;
  glIsProgram_Proc:=nil;
  glIsRenderbuffer_Proc:=nil;
  glIsShader_Proc:=nil;
  glIsTexture_Proc:=nil;
  glLineWidth_Proc:=nil;
  glLinkProgram_Proc:=nil;
  glPixelStorei_Proc:=nil;
  glPolygonOffset_Proc:=nil;
  glReadPixels_Proc:=nil;
  glReleaseShaderCompiler_Proc:=nil;
  glRenderbufferStorage_Proc:=nil;
  glSampleCoverage_Proc:=nil;
  glScissor_Proc:=nil;
  glShaderBinary_Proc:=nil;
  glShaderSource_Proc:=nil;
  glStencilFunc_Proc:=nil;
  glStencilFuncSeparate_Proc:=nil;
  glStencilMask_Proc:=nil;
  glStencilMaskSeparate_Proc:=nil;
  glStencilOp_Proc:=nil;
  glStencilOpSeparate_Proc:=nil;
  glTexImage2D_Proc:=nil;
  glTexParameterf_Proc:=nil;
  glTexParameterfv_Proc:=nil;
  glTexParameteri_Proc:=nil;
  glTexParameteriv_Proc:=nil;
  glTexSubImage2D_Proc:=nil;
  glUniform1f_Proc:=nil;
  glUniform1fv_Proc:=nil;
  glUniform1i_Proc:=nil;
  glUniform1iv_Proc:=nil;
  glUniform2f_Proc:=nil;
  glUniform2fv_Proc:=nil;
  glUniform2i_Proc:=nil;
  glUniform2iv_Proc:=nil;
  glUniform3f_Proc:=nil;
  glUniform3fv_Proc:=nil;
  glUniform3i_Proc:=nil;
  glUniform3iv_Proc:=nil;
  glUniform4f_Proc:=nil;
  glUniform4fv_Proc:=nil;
  glUniform4i_Proc:=nil;
  glUniform4iv_Proc:=nil;
  glUniformMatrix2fv_Proc:=nil;
  glUniformMatrix3fv_Proc:=nil;
  glUniformMatrix4fv_Proc:=nil;
  glUseProgram_Proc:=nil;
  glValidateProgram_Proc:=nil;
  glVertexAttrib1f_Proc:=nil;
  glVertexAttrib1fv_Proc:=nil;
  glVertexAttrib2f_Proc:=nil;
  glVertexAttrib2fv_Proc:=nil;
  glVertexAttrib3f_Proc:=nil;
  glVertexAttrib3fv_Proc:=nil;
  glVertexAttrib4f_Proc:=nil;
  glVertexAttrib4fv_Proc:=nil;
  glVertexAttribPointer_Proc:=nil;
  glViewport_Proc:=nil;
  glEGLImageTargetTexture2DOES_Proc:=nil;
  glEGLImageTargetRenderbufferStorageOES_Proc:=nil;
  glGetProgramBinaryOES_Proc:=nil;
  glProgramBinaryOES_Proc:=nil;
  glMapBufferOES_Proc:=nil;
  glUnmapBufferOES_Proc:=nil;
  glGetBufferPointervOES_Proc:=nil;
  glTexImage3DOES_Proc:=nil;
  glTexSubImage3DOES_Proc:=nil;
  glCopyTexSubImage3DOES_Proc:=nil;
  glCompressedTexImage3DOES_Proc:=nil;
  glCompressedTexSubImage3DOES_Proc:=nil;
  glFramebufferTexture3DOES_Proc:=nil;
  glGetPerfMonitorGroupsAMD_Proc:=nil;
  glGetPerfMonitorCountersAMD_Proc:=nil;
  glGetPerfMonitorGroupStringAMD_Proc:=nil;
  glGetPerfMonitorCounterStringAMD_Proc:=nil;
  glGetPerfMonitorCounterInfoAMD_Proc:=nil;
  glGenPerfMonitorsAMD_Proc:=nil;
  glDeletePerfMonitorsAMD_Proc:=nil;
  glSelectPerfMonitorCountersAMD_Proc:=nil;
  glBeginPerfMonitorAMD_Proc:=nil;
  glEndPerfMonitorAMD_Proc:=nil;
  glGetPerfMonitorCounterDataAMD_Proc:=nil;
  // OpenGL ES 3.0 APIs
  glReadBuffer_Proc := nil;
  glDrawRangeElements_Proc := nil;
  glTexImage3D_Proc := nil;
  glTexSubImage3D_Proc := nil;
  glCopyTexSubImage3D_Proc := nil;
  glCompressedTexImage3D_Proc := nil;
  glCompressedTexSubImage3D_Proc := nil;
  glGenQueries_Proc := nil;
  glDeleteQueries_Proc := nil;
  glIsQuery_Proc := nil;
  glBeginQuery_Proc := nil;
  glEndQuery_Proc := nil;
  glGetQueryiv_Proc := nil;
  glGetQueryObjectuiv_Proc := nil;
  glUnmapBuffer_Proc := nil;
  glGetBufferPointerv_Proc := nil;
  glDrawBuffers_Proc := nil;
  glUniformMatrix2x3fv_Proc := nil;
  glUniformMatrix3x2fv_Proc := nil;
  glUniformMatrix2x4fv_Proc := nil;
  glUniformMatrix4x2fv_Proc := nil;
  glUniformMatrix3x4fv_Proc := nil;
  glUniformMatrix4x3fv_Proc := nil;
  glBlitFramebuffer_Proc := nil;
  glRenderbufferStorageMultisample_Proc := nil;
  glFramebufferTextureLayer_Proc := nil;
  glMapBufferRange_Proc := nil;
  glFlushMappedBufferRange_Proc := nil;
  glBindVertexArray_Proc := nil;
  glDeleteVertexArrays_Proc := nil;
  glGenVertexArrays_Proc := nil;
  glIsVertexArray_Proc := nil;
  glGetIntegeri_v_Proc := nil;
  glBeginTransformFeedback_Proc := nil;
  glEndTransformFeedback_Proc := nil;
  glBindBufferRange_Proc := nil;
  glBindBufferBase_Proc := nil;
  glTransformFeedbackVaryings_Proc := nil;
  glGetTransformFeedbackVarying_Proc := nil;
  glVertexAttribIPointer_Proc := nil;
  glGetVertexAttribIiv_Proc := nil;
  glGetVertexAttribIuiv_Proc := nil;
  glVertexAttribI4i_Proc := nil;
  glVertexAttribI4ui_Proc := nil;
  glVertexAttribI4iv_Proc := nil;
  glVertexAttribI4uiv_Proc := nil;
  glGetUniformuiv_Proc := nil;
  glGetFragDataLocation_Proc := nil;
  glUniform1ui_Proc := nil;
  glUniform2ui_Proc := nil;
  glUniform3ui_Proc := nil;
  glUniform4ui_Proc := nil;
  glUniform1uiv_Proc := nil;
  glUniform2uiv_Proc := nil;
  glUniform3uiv_Proc := nil;
  glUniform4uiv_Proc := nil;
  glClearBufferiv_Proc := nil;
  glClearBufferuiv_Proc := nil;
  glClearBufferfv_Proc := nil;
  glClearBufferfi_Proc := nil;
  glGetStringi_Proc := nil;
  glCopyBufferSubData_Proc := nil;
  glGetUniformIndices_Proc := nil;
  glGetActiveUniformsiv_Proc := nil;
  glGetUniformBlockIndex_Proc := nil;
  glGetActiveUniformBlockiv_Proc := nil;
  glGetActiveUniformBlockName_Proc := nil;
  glUniformBlockBinding_Proc := nil;
  glDrawArraysInstanced_Proc := nil;
  glDrawElementsInstanced_Proc := nil;
  glFenceSync_Proc := nil;
  glIsSync_Proc := nil;
  glDeleteSync_Proc := nil;
  glClientWaitSync_Proc := nil;
  glWaitSync_Proc := nil;
  glGetInteger64v_Proc := nil;
  glGetSynciv_Proc := nil;
  glGetInteger64i_v_Proc := nil;
  glGetBufferParameteri64v_Proc := nil;
  glGenSamplers_Proc := nil;
  glDeleteSamplers_Proc := nil;
  glIsSampler_Proc := nil;
  glBindSampler_Proc := nil;
  glSamplerParameteri_Proc := nil;
  glSamplerParameteriv_Proc := nil;
  glSamplerParameterf_Proc := nil;
  glSamplerParameterfv_Proc := nil;
  glGetSamplerParameteriv_Proc := nil;
  glGetSamplerParameterfv_Proc := nil;
  glVertexAttribDivisor_Proc := nil;
  glBindTransformFeedback_Proc := nil;
  glDeleteTransformFeedbacks_Proc := nil;
  glGenTransformFeedbacks_Proc := nil;
  glIsTransformFeedback_Proc := nil;
  glPauseTransformFeedback_Proc := nil;
  glResumeTransformFeedback_Proc := nil;
  glGetProgramBinary_Proc := nil;
  glProgramBinary_Proc := nil;
  glProgramParameteri_Proc := nil;
  glInvalidateFramebuffer_Proc := nil;
  glInvalidateSubFramebuffer_Proc := nil;
  glTexStorage2D_Proc := nil;
  glTexStorage3D_Proc := nil;
  glGetInternalformativ_Proc := nil;
end;

procedure LoadGLES(const Lib: string; const AltLibName: string = '');

  { Load function address, from library (GLESLib) or eglGetProcAddress.
    Call only when GLESLib <> nil. }
  function MyGetProcAddress(const ProcName: String): pointer;
  var
    ProcNameAnsi: AnsiString;
  begin
    Assert(GLESLib <> nil);
    Result := GLESLib.Symbol(PChar(ProcName));

    if Assigned(eglGetProcAddress) and (not Assigned(Result)) then
    begin
      ProcNameAnsi := ProcName;
      Result := eglGetProcAddress(PAnsiChar(ProcNameAnsi));
    end;
  end;

begin
  FreeGLES;
  {$ifdef OpenGLES}
  GLESLib := TDynLib.Load(Lib, false);
  if (GLESLib = nil) and (AltLibName <> '') then
    GLESLib := TDynLib.Load(AltLibName, false);
  if GLESLib = nil then
    raise Exception.Create(format('Could not load library: %s',[Lib]));
  {$else}
  Exit;
  {$endif}

  // we load also extensions and GL 3 that may not be available
  GLESLib.SymbolError := seReturnNil;

  { OpenGL ES 2.0 APIs }
  Pointer({$ifndef FPC}@{$endif} glActiveTexture_Proc) := MyGetProcAddress('glActiveTexture');
  Pointer({$ifndef FPC}@{$endif} glAttachShader_Proc) := MyGetProcAddress('glAttachShader');
  Pointer({$ifndef FPC}@{$endif} glBindAttribLocation_Proc) := MyGetProcAddress('glBindAttribLocation');
  Pointer({$ifndef FPC}@{$endif} glBindBuffer_Proc) := MyGetProcAddress('glBindBuffer');
  Pointer({$ifndef FPC}@{$endif} glBindFramebuffer_Proc) := MyGetProcAddress('glBindFramebuffer');
  Pointer({$ifndef FPC}@{$endif} glBindRenderbuffer_Proc) := MyGetProcAddress('glBindRenderbuffer');
  Pointer({$ifndef FPC}@{$endif} glBindTexture_Proc) := MyGetProcAddress('glBindTexture');
  Pointer({$ifndef FPC}@{$endif} glBlendColor_Proc) := MyGetProcAddress('glBlendColor');
  Pointer({$ifndef FPC}@{$endif} glBlendEquation_Proc) := MyGetProcAddress('glBlendEquation');
  Pointer({$ifndef FPC}@{$endif} glBlendEquationSeparate_Proc) := MyGetProcAddress('glBlendEquationSeparate');
  Pointer({$ifndef FPC}@{$endif} glBlendFunc_Proc) := MyGetProcAddress('glBlendFunc');
  Pointer({$ifndef FPC}@{$endif} glBlendFuncSeparate_Proc) := MyGetProcAddress('glBlendFuncSeparate');
  Pointer({$ifndef FPC}@{$endif} glBufferData_Proc) := MyGetProcAddress('glBufferData');
  Pointer({$ifndef FPC}@{$endif} glBufferSubData_Proc) := MyGetProcAddress('glBufferSubData');
  Pointer({$ifndef FPC}@{$endif} glCheckFramebufferStatus_Proc) := MyGetProcAddress('glCheckFramebufferStatus');
  Pointer({$ifndef FPC}@{$endif} glClear_Proc) := MyGetProcAddress('glClear');
  Pointer({$ifndef FPC}@{$endif} glClearColor_Proc) := MyGetProcAddress('glClearColor');
  Pointer({$ifndef FPC}@{$endif} glClearDepthf_Proc) := MyGetProcAddress('glClearDepthf');
  Pointer({$ifndef FPC}@{$endif} glClearStencil_Proc) := MyGetProcAddress('glClearStencil');
  Pointer({$ifndef FPC}@{$endif} glColorMask_Proc) := MyGetProcAddress('glColorMask');
  Pointer({$ifndef FPC}@{$endif} glCompileShader_Proc) := MyGetProcAddress('glCompileShader');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexImage2D_Proc) := MyGetProcAddress('glCompressedTexImage2D');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexSubImage2D_Proc) := MyGetProcAddress('glCompressedTexSubImage2D');
  Pointer({$ifndef FPC}@{$endif} glCopyTexImage2D_Proc) := MyGetProcAddress('glCopyTexImage2D');
  Pointer({$ifndef FPC}@{$endif} glCopyTexSubImage2D_Proc) := MyGetProcAddress('glCopyTexSubImage2D');
  Pointer({$ifndef FPC}@{$endif} glCreateProgram_Proc) := MyGetProcAddress('glCreateProgram');
  Pointer({$ifndef FPC}@{$endif} glCreateShader_Proc) := MyGetProcAddress('glCreateShader');
  Pointer({$ifndef FPC}@{$endif} glCullFace_Proc) := MyGetProcAddress('glCullFace');
  Pointer({$ifndef FPC}@{$endif} glDeleteBuffers_Proc) := MyGetProcAddress('glDeleteBuffers');
  Pointer({$ifndef FPC}@{$endif} glDeleteFramebuffers_Proc) := MyGetProcAddress('glDeleteFramebuffers');
  Pointer({$ifndef FPC}@{$endif} glDeleteProgram_Proc) := MyGetProcAddress('glDeleteProgram');
  Pointer({$ifndef FPC}@{$endif} glDeleteRenderbuffers_Proc) := MyGetProcAddress('glDeleteRenderbuffers');
  Pointer({$ifndef FPC}@{$endif} glDeleteShader_Proc) := MyGetProcAddress('glDeleteShader');
  Pointer({$ifndef FPC}@{$endif} glDeleteTextures_Proc) := MyGetProcAddress('glDeleteTextures');
  Pointer({$ifndef FPC}@{$endif} glDepthFunc_Proc) := MyGetProcAddress('glDepthFunc');
  Pointer({$ifndef FPC}@{$endif} glDepthMask_Proc) := MyGetProcAddress('glDepthMask');
  Pointer({$ifndef FPC}@{$endif} glDepthRangef_Proc) := MyGetProcAddress('glDepthRangef');
  Pointer({$ifndef FPC}@{$endif} glDetachShader_Proc) := MyGetProcAddress('glDetachShader');
  Pointer({$ifndef FPC}@{$endif} glDisable_Proc) := MyGetProcAddress('glDisable');
  Pointer({$ifndef FPC}@{$endif} glDisableVertexAttribArray_Proc) := MyGetProcAddress('glDisableVertexAttribArray');
  Pointer({$ifndef FPC}@{$endif} glDrawArrays_Proc) := MyGetProcAddress('glDrawArrays');
  Pointer({$ifndef FPC}@{$endif} glDrawElements_Proc) := MyGetProcAddress('glDrawElements');
  Pointer({$ifndef FPC}@{$endif} glEnable_Proc) := MyGetProcAddress('glEnable');
  Pointer({$ifndef FPC}@{$endif} glEnableVertexAttribArray_Proc) := MyGetProcAddress('glEnableVertexAttribArray');
  Pointer({$ifndef FPC}@{$endif} glFinish_Proc) := MyGetProcAddress('glFinish');
  Pointer({$ifndef FPC}@{$endif} glFlush_Proc) := MyGetProcAddress('glFlush');
  Pointer({$ifndef FPC}@{$endif} glFramebufferRenderbuffer_Proc) := MyGetProcAddress('glFramebufferRenderbuffer');
  Pointer({$ifndef FPC}@{$endif} glFramebufferTexture2D_Proc) := MyGetProcAddress('glFramebufferTexture2D');
  Pointer({$ifndef FPC}@{$endif} glFrontFace_Proc) := MyGetProcAddress('glFrontFace');
  Pointer({$ifndef FPC}@{$endif} glGenBuffers_Proc) := MyGetProcAddress('glGenBuffers');
  Pointer({$ifndef FPC}@{$endif} glGenerateMipmap_Proc) := MyGetProcAddress('glGenerateMipmap');
  Pointer({$ifndef FPC}@{$endif} glGenFramebuffers_Proc) := MyGetProcAddress('glGenFramebuffers');
  Pointer({$ifndef FPC}@{$endif} glGenRenderbuffers_Proc) := MyGetProcAddress('glGenRenderbuffers');
  Pointer({$ifndef FPC}@{$endif} glGenTextures_Proc) := MyGetProcAddress('glGenTextures');
  Pointer({$ifndef FPC}@{$endif} glGetActiveAttrib_Proc) := MyGetProcAddress('glGetActiveAttrib');
  Pointer({$ifndef FPC}@{$endif} glGetActiveUniform_Proc) := MyGetProcAddress('glGetActiveUniform');
  Pointer({$ifndef FPC}@{$endif} glGetAttachedShaders_Proc) := MyGetProcAddress('glGetAttachedShaders');
  Pointer({$ifndef FPC}@{$endif} glGetAttribLocation_Proc) := MyGetProcAddress('glGetAttribLocation');
  Pointer({$ifndef FPC}@{$endif} glGetBooleanv_Proc) := MyGetProcAddress('glGetBooleanv');
  Pointer({$ifndef FPC}@{$endif} glGetBufferParameteriv_Proc) := MyGetProcAddress('glGetBufferParameteriv');
  Pointer({$ifndef FPC}@{$endif} glGetError_Proc) := MyGetProcAddress('glGetError');
  Pointer({$ifndef FPC}@{$endif} glGetFloatv_Proc) := MyGetProcAddress('glGetFloatv');
  Pointer({$ifndef FPC}@{$endif} glGetFramebufferAttachmentParameteriv_Proc) := MyGetProcAddress('glGetFramebufferAttachmentParameteriv');
  Pointer({$ifndef FPC}@{$endif} glGetIntegerv_Proc) := MyGetProcAddress('glGetIntegerv');
  Pointer({$ifndef FPC}@{$endif} glGetProgramiv_Proc) := MyGetProcAddress('glGetProgramiv');
  Pointer({$ifndef FPC}@{$endif} glGetProgramInfoLog_Proc) := MyGetProcAddress('glGetProgramInfoLog');
  Pointer({$ifndef FPC}@{$endif} glGetRenderbufferParameteriv_Proc) := MyGetProcAddress('glGetRenderbufferParameteriv');
  Pointer({$ifndef FPC}@{$endif} glGetShaderiv_Proc) := MyGetProcAddress('glGetShaderiv');
  Pointer({$ifndef FPC}@{$endif} glGetShaderInfoLog_Proc) := MyGetProcAddress('glGetShaderInfoLog');
  Pointer({$ifndef FPC}@{$endif} glGetShaderPrecisionFormat_Proc) := MyGetProcAddress('glGetShaderPrecisionFormat');
  Pointer({$ifndef FPC}@{$endif} glGetShaderSource_Proc) := MyGetProcAddress('glGetShaderSource');
  Pointer({$ifndef FPC}@{$endif} glGetString_Proc) := MyGetProcAddress('glGetString');
  Pointer({$ifndef FPC}@{$endif} glGetTexParameterfv_Proc) := MyGetProcAddress('glGetTexParameterfv');
  Pointer({$ifndef FPC}@{$endif} glGetTexParameteriv_Proc) := MyGetProcAddress('glGetTexParameteriv');
  Pointer({$ifndef FPC}@{$endif} glGetUniformfv_Proc) := MyGetProcAddress('glGetUniformfv');
  Pointer({$ifndef FPC}@{$endif} glGetUniformiv_Proc) := MyGetProcAddress('glGetUniformiv');
  Pointer({$ifndef FPC}@{$endif} glGetUniformLocation_Proc) := MyGetProcAddress('glGetUniformLocation');
  Pointer({$ifndef FPC}@{$endif} glGetVertexAttribfv_Proc) := MyGetProcAddress('glGetVertexAttribfv');
  Pointer({$ifndef FPC}@{$endif} glGetVertexAttribiv_Proc) := MyGetProcAddress('glGetVertexAttribiv');
  Pointer({$ifndef FPC}@{$endif} glGetVertexAttribPointerv_Proc) := MyGetProcAddress('glGetVertexAttribPointerv');
  Pointer({$ifndef FPC}@{$endif} glHint_Proc) := MyGetProcAddress('glHint');
  Pointer({$ifndef FPC}@{$endif} glIsBuffer_Proc) := MyGetProcAddress('glIsBuffer');
  Pointer({$ifndef FPC}@{$endif} glIsEnabled_Proc) := MyGetProcAddress('glIsEnabled');
  Pointer({$ifndef FPC}@{$endif} glIsFramebuffer_Proc) := MyGetProcAddress('glIsFramebuffer');
  Pointer({$ifndef FPC}@{$endif} glIsProgram_Proc) := MyGetProcAddress('glIsProgram');
  Pointer({$ifndef FPC}@{$endif} glIsRenderbuffer_Proc) := MyGetProcAddress('glIsRenderbuffer');
  Pointer({$ifndef FPC}@{$endif} glIsShader_Proc) := MyGetProcAddress('glIsShader');
  Pointer({$ifndef FPC}@{$endif} glIsTexture_Proc) := MyGetProcAddress('glIsTexture');
  Pointer({$ifndef FPC}@{$endif} glLineWidth_Proc) := MyGetProcAddress('glLineWidth');
  Pointer({$ifndef FPC}@{$endif} glLinkProgram_Proc) := MyGetProcAddress('glLinkProgram');
  Pointer({$ifndef FPC}@{$endif} glPixelStorei_Proc) := MyGetProcAddress('glPixelStorei');
  Pointer({$ifndef FPC}@{$endif} glPolygonOffset_Proc) := MyGetProcAddress('glPolygonOffset');
  Pointer({$ifndef FPC}@{$endif} glReadPixels_Proc) := MyGetProcAddress('glReadPixels');
  Pointer({$ifndef FPC}@{$endif} glReleaseShaderCompiler_Proc) := MyGetProcAddress('glReleaseShaderCompiler');
  Pointer({$ifndef FPC}@{$endif} glRenderbufferStorage_Proc) := MyGetProcAddress('glRenderbufferStorage');
  Pointer({$ifndef FPC}@{$endif} glSampleCoverage_Proc) := MyGetProcAddress('glSampleCoverage');
  Pointer({$ifndef FPC}@{$endif} glScissor_Proc) := MyGetProcAddress('glScissor');
  Pointer({$ifndef FPC}@{$endif} glShaderBinary_Proc) := MyGetProcAddress('glShaderBinary');
  Pointer({$ifndef FPC}@{$endif} glShaderSource_Proc) := MyGetProcAddress('glShaderSource');
  Pointer({$ifndef FPC}@{$endif} glStencilFunc_Proc) := MyGetProcAddress('glStencilFunc');
  Pointer({$ifndef FPC}@{$endif} glStencilFuncSeparate_Proc) := MyGetProcAddress('glStencilFuncSeparate');
  Pointer({$ifndef FPC}@{$endif} glStencilMask_Proc) := MyGetProcAddress('glStencilMask');
  Pointer({$ifndef FPC}@{$endif} glStencilMaskSeparate_Proc) := MyGetProcAddress('glStencilMaskSeparate');
  Pointer({$ifndef FPC}@{$endif} glStencilOp_Proc) := MyGetProcAddress('glStencilOp');
  Pointer({$ifndef FPC}@{$endif} glStencilOpSeparate_Proc) := MyGetProcAddress('glStencilOpSeparate');
  Pointer({$ifndef FPC}@{$endif} glTexImage2D_Proc) := MyGetProcAddress('glTexImage2D');
  Pointer({$ifndef FPC}@{$endif} glTexParameterf_Proc) := MyGetProcAddress('glTexParameterf');
  Pointer({$ifndef FPC}@{$endif} glTexParameterfv_Proc) := MyGetProcAddress('glTexParameterfv');
  Pointer({$ifndef FPC}@{$endif} glTexParameteri_Proc) := MyGetProcAddress('glTexParameteri');
  Pointer({$ifndef FPC}@{$endif} glTexParameteriv_Proc) := MyGetProcAddress('glTexParameteriv');
  Pointer({$ifndef FPC}@{$endif} glTexSubImage2D_Proc) := MyGetProcAddress('glTexSubImage2D');
  Pointer({$ifndef FPC}@{$endif} glUniform1f_Proc) := MyGetProcAddress('glUniform1f');
  Pointer({$ifndef FPC}@{$endif} glUniform1fv_Proc) := MyGetProcAddress('glUniform1fv');
  Pointer({$ifndef FPC}@{$endif} glUniform1i_Proc) := MyGetProcAddress('glUniform1i');
  Pointer({$ifndef FPC}@{$endif} glUniform1iv_Proc) := MyGetProcAddress('glUniform1iv');
  Pointer({$ifndef FPC}@{$endif} glUniform2f_Proc) := MyGetProcAddress('glUniform2f');
  Pointer({$ifndef FPC}@{$endif} glUniform2fv_Proc) := MyGetProcAddress('glUniform2fv');
  Pointer({$ifndef FPC}@{$endif} glUniform2i_Proc) := MyGetProcAddress('glUniform2i');
  Pointer({$ifndef FPC}@{$endif} glUniform2iv_Proc) := MyGetProcAddress('glUniform2iv');
  Pointer({$ifndef FPC}@{$endif} glUniform3f_Proc) := MyGetProcAddress('glUniform3f');
  Pointer({$ifndef FPC}@{$endif} glUniform3fv_Proc) := MyGetProcAddress('glUniform3fv');
  Pointer({$ifndef FPC}@{$endif} glUniform3i_Proc) := MyGetProcAddress('glUniform3i');
  Pointer({$ifndef FPC}@{$endif} glUniform3iv_Proc) := MyGetProcAddress('glUniform3iv');
  Pointer({$ifndef FPC}@{$endif} glUniform4f_Proc) := MyGetProcAddress('glUniform4f');
  Pointer({$ifndef FPC}@{$endif} glUniform4fv_Proc) := MyGetProcAddress('glUniform4fv');
  Pointer({$ifndef FPC}@{$endif} glUniform4i_Proc) := MyGetProcAddress('glUniform4i');
  Pointer({$ifndef FPC}@{$endif} glUniform4iv_Proc) := MyGetProcAddress('glUniform4iv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix2fv_Proc) := MyGetProcAddress('glUniformMatrix2fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix3fv_Proc) := MyGetProcAddress('glUniformMatrix3fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix4fv_Proc) := MyGetProcAddress('glUniformMatrix4fv');
  Pointer({$ifndef FPC}@{$endif} glUseProgram_Proc) := MyGetProcAddress('glUseProgram');
  Pointer({$ifndef FPC}@{$endif} glValidateProgram_Proc) := MyGetProcAddress('glValidateProgram');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib1f_Proc) := MyGetProcAddress('glVertexAttrib1f');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib1fv_Proc) := MyGetProcAddress('glVertexAttrib1fv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib2f_Proc) := MyGetProcAddress('glVertexAttrib2f');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib2fv_Proc) := MyGetProcAddress('glVertexAttrib2fv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib3f_Proc) := MyGetProcAddress('glVertexAttrib3f');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib3fv_Proc) := MyGetProcAddress('glVertexAttrib3fv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib4f_Proc) := MyGetProcAddress('glVertexAttrib4f');
  Pointer({$ifndef FPC}@{$endif} glVertexAttrib4fv_Proc) := MyGetProcAddress('glVertexAttrib4fv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribPointer_Proc) := MyGetProcAddress('glVertexAttribPointer');
  Pointer({$ifndef FPC}@{$endif} glViewport_Proc) := MyGetProcAddress('glViewport');
  Pointer({$ifndef FPC}@{$endif} glEGLImageTargetTexture2DOES_Proc) := MyGetProcAddress('glEGLImageTargetTexture2DOES');
  Pointer({$ifndef FPC}@{$endif} glEGLImageTargetRenderbufferStorageOES_Proc) := MyGetProcAddress('glEGLImageTargetRenderbufferStorageOES');
  Pointer({$ifndef FPC}@{$endif} glGetProgramBinaryOES_Proc) := MyGetProcAddress('glGetProgramBinaryOES');
  Pointer({$ifndef FPC}@{$endif} glProgramBinaryOES_Proc) := MyGetProcAddress('glProgramBinaryOES');
  Pointer({$ifndef FPC}@{$endif} glMapBufferOES_Proc) := MyGetProcAddress('glMapBufferOES');
  Pointer({$ifndef FPC}@{$endif} glUnmapBufferOES_Proc) := MyGetProcAddress('glUnmapBufferOES');
  Pointer({$ifndef FPC}@{$endif} glGetBufferPointervOES_Proc) := MyGetProcAddress('glGetBufferPointervOES');
  Pointer({$ifndef FPC}@{$endif} glTexImage3DOES_Proc) := MyGetProcAddress('glTexImage3DOES');
  Pointer({$ifndef FPC}@{$endif} glTexSubImage3DOES_Proc) := MyGetProcAddress('glTexSubImage3DOES');
  Pointer({$ifndef FPC}@{$endif} glCopyTexSubImage3DOES_Proc) := MyGetProcAddress('glCopyTexSubImage3DOES');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexImage3DOES_Proc) := MyGetProcAddress('glCompressedTexImage3DOES');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexSubImage3DOES_Proc) := MyGetProcAddress('glCompressedTexSubImage3DOES');
  Pointer({$ifndef FPC}@{$endif} glFramebufferTexture3DOES_Proc) := MyGetProcAddress('glFramebufferTexture3DOES');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorGroupsAMD_Proc) := MyGetProcAddress('glGetPerfMonitorGroupsAMD');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorCountersAMD_Proc) := MyGetProcAddress('glGetPerfMonitorCountersAMD');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorGroupStringAMD_Proc) := MyGetProcAddress('glGetPerfMonitorGroupStringAMD');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorCounterStringAMD_Proc) := MyGetProcAddress('glGetPerfMonitorCounterStringAMD');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorCounterInfoAMD_Proc) := MyGetProcAddress('glGetPerfMonitorCounterInfoAMD');
  Pointer({$ifndef FPC}@{$endif} glGenPerfMonitorsAMD_Proc) := MyGetProcAddress('glGenPerfMonitorsAMD');
  Pointer({$ifndef FPC}@{$endif} glDeletePerfMonitorsAMD_Proc) := MyGetProcAddress('glDeletePerfMonitorsAMD');
  Pointer({$ifndef FPC}@{$endif} glSelectPerfMonitorCountersAMD_Proc) := MyGetProcAddress('glSelectPerfMonitorCountersAMD');
  Pointer({$ifndef FPC}@{$endif} glBeginPerfMonitorAMD_Proc) := MyGetProcAddress('glBeginPerfMonitorAMD');
  Pointer({$ifndef FPC}@{$endif} glEndPerfMonitorAMD_Proc) := MyGetProcAddress('glEndPerfMonitorAMD');
  Pointer({$ifndef FPC}@{$endif} glGetPerfMonitorCounterDataAMD_Proc) := MyGetProcAddress('glGetPerfMonitorCounterDataAMD');
  Pointer({$ifndef FPC}@{$endif} glDebugMessageCallback_Proc) := MyGetProcAddress('glDebugMessageCallback');
  Pointer({$ifndef FPC}@{$endif} glDebugMessageControl_Proc) := MyGetProcAddress('glDebugMessageControl');

  { OpenGL ES 3.0 APIs }
  Pointer({$ifndef FPC}@{$endif} glReadBuffer_Proc) := MyGetProcAddress('glReadBuffer');
  Pointer({$ifndef FPC}@{$endif} glDrawRangeElements_Proc) := MyGetProcAddress('glDrawRangeElements');
  Pointer({$ifndef FPC}@{$endif} glTexImage3D_Proc) := MyGetProcAddress('glTexImage3D');
  Pointer({$ifndef FPC}@{$endif} glTexSubImage3D_Proc) := MyGetProcAddress('glTexSubImage3D');
  Pointer({$ifndef FPC}@{$endif} glCopyTexSubImage3D_Proc) := MyGetProcAddress('glCopyTexSubImage3D');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexImage3D_Proc) := MyGetProcAddress('glCompressedTexImage3D');
  Pointer({$ifndef FPC}@{$endif} glCompressedTexSubImage3D_Proc) := MyGetProcAddress('glCompressedTexSubImage3D');
  Pointer({$ifndef FPC}@{$endif} glGenQueries_Proc) := MyGetProcAddress('glGenQueries');
  Pointer({$ifndef FPC}@{$endif} glDeleteQueries_Proc) := MyGetProcAddress('glDeleteQueries');
  Pointer({$ifndef FPC}@{$endif} glIsQuery_Proc) := MyGetProcAddress('glIsQuery');
  Pointer({$ifndef FPC}@{$endif} glBeginQuery_Proc) := MyGetProcAddress('glBeginQuery');
  Pointer({$ifndef FPC}@{$endif} glEndQuery_Proc) := MyGetProcAddress('glEndQuery');
  Pointer({$ifndef FPC}@{$endif} glGetQueryiv_Proc) := MyGetProcAddress('glGetQueryiv');
  Pointer({$ifndef FPC}@{$endif} glGetQueryObjectuiv_Proc) := MyGetProcAddress('glGetQueryObjectuiv');
  Pointer({$ifndef FPC}@{$endif} glUnmapBuffer_Proc) := MyGetProcAddress('glUnmapBuffer');
  Pointer({$ifndef FPC}@{$endif} glGetBufferPointerv_Proc) := MyGetProcAddress('glGetBufferPointerv');
  Pointer({$ifndef FPC}@{$endif} glDrawBuffers_Proc) := MyGetProcAddress('glDrawBuffers');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix2x3fv_Proc) := MyGetProcAddress('glUniformMatrix2x3fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix3x2fv_Proc) := MyGetProcAddress('glUniformMatrix3x2fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix2x4fv_Proc) := MyGetProcAddress('glUniformMatrix2x4fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix4x2fv_Proc) := MyGetProcAddress('glUniformMatrix4x2fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix3x4fv_Proc) := MyGetProcAddress('glUniformMatrix3x4fv');
  Pointer({$ifndef FPC}@{$endif} glUniformMatrix4x3fv_Proc) := MyGetProcAddress('glUniformMatrix4x3fv');
  Pointer({$ifndef FPC}@{$endif} glBlitFramebuffer_Proc) := MyGetProcAddress('glBlitFramebuffer');
  Pointer({$ifndef FPC}@{$endif} glRenderbufferStorageMultisample_Proc) := MyGetProcAddress('glRenderbufferStorageMultisample');
  Pointer({$ifndef FPC}@{$endif} glFramebufferTextureLayer_Proc) := MyGetProcAddress('glFramebufferTextureLayer');
  Pointer({$ifndef FPC}@{$endif} glMapBufferRange_Proc) := MyGetProcAddress('glMapBufferRange');
  Pointer({$ifndef FPC}@{$endif} glFlushMappedBufferRange_Proc) := MyGetProcAddress('glFlushMappedBufferRange');
  Pointer({$ifndef FPC}@{$endif} glBindVertexArray_Proc) := MyGetProcAddress('glBindVertexArray');
  Pointer({$ifndef FPC}@{$endif} glDeleteVertexArrays_Proc) := MyGetProcAddress('glDeleteVertexArrays');
  Pointer({$ifndef FPC}@{$endif} glGenVertexArrays_Proc) := MyGetProcAddress('glGenVertexArrays');
  Pointer({$ifndef FPC}@{$endif} glIsVertexArray_Proc) := MyGetProcAddress('glIsVertexArray');
  Pointer({$ifndef FPC}@{$endif} glGetIntegeri_v_Proc) := MyGetProcAddress('glGetIntegeri_v');
  Pointer({$ifndef FPC}@{$endif} glBeginTransformFeedback_Proc) := MyGetProcAddress('glBeginTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glEndTransformFeedback_Proc) := MyGetProcAddress('glEndTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glBindBufferRange_Proc) := MyGetProcAddress('glBindBufferRange');
  Pointer({$ifndef FPC}@{$endif} glBindBufferBase_Proc) := MyGetProcAddress('glBindBufferBase');
  Pointer({$ifndef FPC}@{$endif} glTransformFeedbackVaryings_Proc) := MyGetProcAddress('glTransformFeedbackVaryings');
  Pointer({$ifndef FPC}@{$endif} glGetTransformFeedbackVarying_Proc) := MyGetProcAddress('glGetTransformFeedbackVarying');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribIPointer_Proc) := MyGetProcAddress('glVertexAttribIPointer');
  Pointer({$ifndef FPC}@{$endif} glGetVertexAttribIiv_Proc) := MyGetProcAddress('glGetVertexAttribIiv');
  Pointer({$ifndef FPC}@{$endif} glGetVertexAttribIuiv_Proc) := MyGetProcAddress('glGetVertexAttribIuiv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribI4i_Proc) := MyGetProcAddress('glVertexAttribI4i');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribI4ui_Proc) := MyGetProcAddress('glVertexAttribI4ui');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribI4iv_Proc) := MyGetProcAddress('glVertexAttribI4iv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribI4uiv_Proc) := MyGetProcAddress('glVertexAttribI4uiv');
  Pointer({$ifndef FPC}@{$endif} glGetUniformuiv_Proc) := MyGetProcAddress('glGetUniformuiv');
  Pointer({$ifndef FPC}@{$endif} glGetFragDataLocation_Proc) := MyGetProcAddress('glGetFragDataLocation');
  Pointer({$ifndef FPC}@{$endif} glUniform1ui_Proc) := MyGetProcAddress('glUniform1ui');
  Pointer({$ifndef FPC}@{$endif} glUniform2ui_Proc) := MyGetProcAddress('glUniform2ui');
  Pointer({$ifndef FPC}@{$endif} glUniform3ui_Proc) := MyGetProcAddress('glUniform3ui');
  Pointer({$ifndef FPC}@{$endif} glUniform4ui_Proc) := MyGetProcAddress('glUniform4ui');
  Pointer({$ifndef FPC}@{$endif} glUniform1uiv_Proc) := MyGetProcAddress('glUniform1uiv');
  Pointer({$ifndef FPC}@{$endif} glUniform2uiv_Proc) := MyGetProcAddress('glUniform2uiv');
  Pointer({$ifndef FPC}@{$endif} glUniform3uiv_Proc) := MyGetProcAddress('glUniform3uiv');
  Pointer({$ifndef FPC}@{$endif} glUniform4uiv_Proc) := MyGetProcAddress('glUniform4uiv');
  Pointer({$ifndef FPC}@{$endif} glClearBufferiv_Proc) := MyGetProcAddress('glClearBufferiv');
  Pointer({$ifndef FPC}@{$endif} glClearBufferuiv_Proc) := MyGetProcAddress('glClearBufferuiv');
  Pointer({$ifndef FPC}@{$endif} glClearBufferfv_Proc) := MyGetProcAddress('glClearBufferfv');
  Pointer({$ifndef FPC}@{$endif} glClearBufferfi_Proc) := MyGetProcAddress('glClearBufferfi');
  Pointer({$ifndef FPC}@{$endif} glGetStringi_Proc) := MyGetProcAddress('glGetStringi');
  Pointer({$ifndef FPC}@{$endif} glCopyBufferSubData_Proc) := MyGetProcAddress('glCopyBufferSubData');
  Pointer({$ifndef FPC}@{$endif} glGetUniformIndices_Proc) := MyGetProcAddress('glGetUniformIndices');
  Pointer({$ifndef FPC}@{$endif} glGetActiveUniformsiv_Proc) := MyGetProcAddress('glGetActiveUniformsiv');
  Pointer({$ifndef FPC}@{$endif} glGetUniformBlockIndex_Proc) := MyGetProcAddress('glGetUniformBlockIndex');
  Pointer({$ifndef FPC}@{$endif} glGetActiveUniformBlockiv_Proc) := MyGetProcAddress('glGetActiveUniformBlockiv');
  Pointer({$ifndef FPC}@{$endif} glGetActiveUniformBlockName_Proc) := MyGetProcAddress('glGetActiveUniformBlockName');
  Pointer({$ifndef FPC}@{$endif} glUniformBlockBinding_Proc) := MyGetProcAddress('glUniformBlockBinding');
  Pointer({$ifndef FPC}@{$endif} glDrawArraysInstanced_Proc) := MyGetProcAddress('glDrawArraysInstanced');
  Pointer({$ifndef FPC}@{$endif} glDrawElementsInstanced_Proc) := MyGetProcAddress('glDrawElementsInstanced');
  Pointer({$ifndef FPC}@{$endif} glFenceSync_Proc) := MyGetProcAddress('glFenceSync');
  Pointer({$ifndef FPC}@{$endif} glIsSync_Proc) := MyGetProcAddress('glIsSync');
  Pointer({$ifndef FPC}@{$endif} glDeleteSync_Proc) := MyGetProcAddress('glDeleteSync');
  Pointer({$ifndef FPC}@{$endif} glClientWaitSync_Proc) := MyGetProcAddress('glClientWaitSync');
  Pointer({$ifndef FPC}@{$endif} glWaitSync_Proc) := MyGetProcAddress('glWaitSync');
  Pointer({$ifndef FPC}@{$endif} glGetInteger64v_Proc) := MyGetProcAddress('glGetInteger64v');
  Pointer({$ifndef FPC}@{$endif} glGetSynciv_Proc) := MyGetProcAddress('glGetSynciv');
  Pointer({$ifndef FPC}@{$endif} glGetInteger64i_v_Proc) := MyGetProcAddress('glGetInteger64i_v');
  Pointer({$ifndef FPC}@{$endif} glGetBufferParameteri64v_Proc) := MyGetProcAddress('glGetBufferParameteri64v');
  Pointer({$ifndef FPC}@{$endif} glGenSamplers_Proc) := MyGetProcAddress('glGenSamplers');
  Pointer({$ifndef FPC}@{$endif} glDeleteSamplers_Proc) := MyGetProcAddress('glDeleteSamplers');
  Pointer({$ifndef FPC}@{$endif} glIsSampler_Proc) := MyGetProcAddress('glIsSampler');
  Pointer({$ifndef FPC}@{$endif} glBindSampler_Proc) := MyGetProcAddress('glBindSampler');
  Pointer({$ifndef FPC}@{$endif} glSamplerParameteri_Proc) := MyGetProcAddress('glSamplerParameteri');
  Pointer({$ifndef FPC}@{$endif} glSamplerParameteriv_Proc) := MyGetProcAddress('glSamplerParameteriv');
  Pointer({$ifndef FPC}@{$endif} glSamplerParameterf_Proc) := MyGetProcAddress('glSamplerParameterf');
  Pointer({$ifndef FPC}@{$endif} glSamplerParameterfv_Proc) := MyGetProcAddress('glSamplerParameterfv');
  Pointer({$ifndef FPC}@{$endif} glGetSamplerParameteriv_Proc) := MyGetProcAddress('glGetSamplerParameteriv');
  Pointer({$ifndef FPC}@{$endif} glGetSamplerParameterfv_Proc) := MyGetProcAddress('glGetSamplerParameterfv');
  Pointer({$ifndef FPC}@{$endif} glVertexAttribDivisor_Proc) := MyGetProcAddress('glVertexAttribDivisor');
  Pointer({$ifndef FPC}@{$endif} glBindTransformFeedback_Proc) := MyGetProcAddress('glBindTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glDeleteTransformFeedbacks_Proc) := MyGetProcAddress('glDeleteTransformFeedbacks');
  Pointer({$ifndef FPC}@{$endif} glGenTransformFeedbacks_Proc) := MyGetProcAddress('glGenTransformFeedbacks');
  Pointer({$ifndef FPC}@{$endif} glIsTransformFeedback_Proc) := MyGetProcAddress('glIsTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glPauseTransformFeedback_Proc) := MyGetProcAddress('glPauseTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glResumeTransformFeedback_Proc) := MyGetProcAddress('glResumeTransformFeedback');
  Pointer({$ifndef FPC}@{$endif} glGetProgramBinary_Proc) := MyGetProcAddress('glGetProgramBinary');
  Pointer({$ifndef FPC}@{$endif} glProgramBinary_Proc) := MyGetProcAddress('glProgramBinary');
  Pointer({$ifndef FPC}@{$endif} glProgramParameteri_Proc) := MyGetProcAddress('glProgramParameteri');
  Pointer({$ifndef FPC}@{$endif} glInvalidateFramebuffer_Proc) := MyGetProcAddress('glInvalidateFramebuffer');
  Pointer({$ifndef FPC}@{$endif} glInvalidateSubFramebuffer_Proc) := MyGetProcAddress('glInvalidateSubFramebuffer');
  Pointer({$ifndef FPC}@{$endif} glTexStorage2D_Proc) := MyGetProcAddress('glTexStorage2D');
  Pointer({$ifndef FPC}@{$endif} glTexStorage3D_Proc) := MyGetProcAddress('glTexStorage3D');
  Pointer({$ifndef FPC}@{$endif} glGetInternalformativ_Proc) := MyGetProcAddress('glGetInternalformativ');
end;

procedure GLESInitialization;
begin
  LoadGLES(
    {$ifdef darwin} '/System/Library/Frameworks/OpenGLES.framework/OpenGLES'
    {$else}
      {$ifdef windows} 'libGLESv2.dll'
      { First try to access libGLESv2.so.2 (from libgles2-mesa package on Debian).
        The name libGLESv2.so is only available in -dev package. }
      {$else} 'libGLESv2.so.2', 'libGLESv2.so'
      {$endif}
    {$endif});
end;


{ Wrapper implementations }

{ Helper to convert common OpenGL ES enum values to strings for logging. }
function GLEnumName(const E: GLenum): String;
begin
  if E = GL_TEXTURE_2D then Result := 'GL_TEXTURE_2D'
  else if E = GL_TEXTURE_3D then Result := 'GL_TEXTURE_3D'
  else if E = GL_TEXTURE_2D_ARRAY then Result := 'GL_TEXTURE_2D_ARRAY'
  else if E = GL_TEXTURE_CUBE_MAP then Result := 'GL_TEXTURE_CUBE_MAP'
  else if E = GL_ARRAY_BUFFER then Result := 'GL_ARRAY_BUFFER'
  else if E = GL_ELEMENT_ARRAY_BUFFER then Result := 'GL_ELEMENT_ARRAY_BUFFER'
  else if E = GL_FRAMEBUFFER then Result := 'GL_FRAMEBUFFER'
  else if E = GL_RENDERBUFFER then Result := 'GL_RENDERBUFFER'
  else if E = GL_BLEND then Result := 'GL_BLEND'
  else if E = GL_DEPTH_TEST then Result := 'GL_DEPTH_TEST'
  else if E = GL_CULL_FACE then Result := 'GL_CULL_FACE'
  else if E = GL_STENCIL_TEST then Result := 'GL_STENCIL_TEST'
  else if E = GL_SCISSOR_TEST then Result := 'GL_SCISSOR_TEST'
  else if E = GL_VERTEX_SHADER then Result := 'GL_VERTEX_SHADER'
  else if E = GL_FRAGMENT_SHADER then Result := 'GL_FRAGMENT_SHADER'
  else if E = GL_LINE_STRIP then Result := 'GL_LINE_STRIP'
  else if E = GL_LINE_LOOP then Result := 'GL_LINE_LOOP'
  else if E = GL_TRIANGLES then Result := 'GL_TRIANGLES'
  else if E = GL_TRIANGLE_STRIP then Result := 'GL_TRIANGLE_STRIP'
  else if E = GL_TRIANGLE_FAN then Result := 'GL_TRIANGLE_FAN'
  else if E = GL_UNSIGNED_BYTE then Result := 'GL_UNSIGNED_BYTE'
  else if E = GL_UNSIGNED_SHORT then Result := 'GL_UNSIGNED_SHORT'
  else if E = GL_UNSIGNED_INT then Result := 'GL_UNSIGNED_INT'
  else if E = GL_FLOAT then Result := 'GL_FLOAT'
  else if E = GL_INT then Result := 'GL_INT'
  else if E = GL_HALF_FLOAT_OES then Result := 'GL_HALF_FLOAT_OES'
  else if E = GL_NEAREST then Result := 'GL_NEAREST'
  else if E = GL_LINEAR then Result := 'GL_LINEAR'
  else if E = GL_NEAREST_MIPMAP_LINEAR then Result := 'GL_NEAREST_MIPMAP_LINEAR'
  else if E = GL_LINEAR_MIPMAP_LINEAR then Result := 'GL_LINEAR_MIPMAP_LINEAR'
  else if E = GL_TEXTURE_MIN_FILTER then Result := 'GL_TEXTURE_MIN_FILTER'
  else if E = GL_TEXTURE_MAG_FILTER then Result := 'GL_TEXTURE_MAG_FILTER'
  else if E = GL_TEXTURE_WRAP_S then Result := 'GL_TEXTURE_WRAP_S'
  else if E = GL_TEXTURE_WRAP_T then Result := 'GL_TEXTURE_WRAP_T'
  else if E = GL_TEXTURE_WRAP_R_OES then Result := 'GL_TEXTURE_WRAP_R_OES'
  else if E = GL_REPEAT then Result := 'GL_REPEAT'
  else if E = GL_CLAMP_TO_EDGE then Result := 'GL_CLAMP_TO_EDGE'
  else if E = GL_MIRRORED_REPEAT then Result := 'GL_MIRRORED_REPEAT'
  else if E = GL_RGBA then Result := 'GL_RGBA'
  else if E = GL_RGB then Result := 'GL_RGB'
  else if E = GL_DEPTH_COMPONENT then Result := 'GL_DEPTH_COMPONENT'
  else if E = GL_DEPTH_STENCIL then Result := 'GL_DEPTH_STENCIL'
  else if E = GL_RED then Result := 'GL_RED'
  else if E = GL_FUNC_ADD then Result := 'GL_FUNC_ADD'
  else if E = GL_FUNC_SUBTRACT then Result := 'GL_FUNC_SUBTRACT'
  else if E = GL_FUNC_REVERSE_SUBTRACT then Result := 'GL_FUNC_REVERSE_SUBTRACT'
  else if E = GL_ZERO then Result := 'GL_ZERO'
  else if E = GL_ONE then Result := 'GL_ONE'
  else if E = GL_SRC_ALPHA then Result := 'GL_SRC_ALPHA'
  else if E = GL_ONE_MINUS_SRC_ALPHA then Result := 'GL_ONE_MINUS_SRC_ALPHA'
  else if E = GL_SRC_COLOR then Result := 'GL_SRC_COLOR'
  else if E = GL_ONE_MINUS_SRC_COLOR then Result := 'GL_ONE_MINUS_SRC_COLOR'
  else if E = GL_DST_ALPHA then Result := 'GL_DST_ALPHA'
  else if E = GL_ONE_MINUS_DST_ALPHA then Result := 'GL_ONE_MINUS_DST_ALPHA'
  else if E = GL_DST_COLOR then Result := 'GL_DST_COLOR'
  else if E = GL_ONE_MINUS_DST_COLOR then Result := 'GL_ONE_MINUS_DST_COLOR'
  else if E = GL_INVALID_ENUM then Result := 'GL_INVALID_ENUM'
  else if E = GL_INVALID_VALUE then Result := 'GL_INVALID_VALUE'
  else if E = GL_INVALID_OPERATION then Result := 'GL_INVALID_OPERATION'
  else if E = GL_OUT_OF_MEMORY then Result := 'GL_OUT_OF_MEMORY'
  else if E = GL_INVALID_FRAMEBUFFER_OPERATION then Result := 'GL_INVALID_FRAMEBUFFER_OPERATION'
  else if E = GL_DEPTH_BUFFER_BIT then Result := 'GL_DEPTH_BUFFER_BIT'
  else if E = GL_STENCIL_BUFFER_BIT then Result := 'GL_STENCIL_BUFFER_BIT'
  else if E = GL_COLOR_BUFFER_BIT then Result := 'GL_COLOR_BUFFER_BIT'
  else if E = GL_FRONT then Result := 'GL_FRONT'
  else if E = GL_BACK then Result := 'GL_BACK'
  else if E = GL_FRONT_AND_BACK then Result := 'GL_FRONT_AND_BACK'
  else if E = GL_STATIC_DRAW then Result := 'GL_STATIC_DRAW'
  else if E = GL_DYNAMIC_DRAW then Result := 'GL_DYNAMIC_DRAW'
  else if E = GL_STREAM_DRAW then Result := 'GL_STREAM_DRAW'
  else if E = GL_RGBA8 then Result := 'GL_RGBA8'
  else if E = GL_RGB8 then Result := 'GL_RGB8'
  else if E = GL_SRGB8_ALPHA8 then Result := 'GL_SRGB8_ALPHA8'
  else if E = GL_RGBA16F then Result := 'GL_RGBA16F'
  else if E = GL_RGB16F then Result := 'GL_RGB16F'
  else if E = GL_DEPTH_COMPONENT24 then Result := 'GL_DEPTH_COMPONENT24'
  else if E = GL_DEPTH24_STENCIL8 then Result := 'GL_DEPTH24_STENCIL8'
  else Result := IntToStr(Integer(E));
end;

{ Check GL errors after a call and log any error. }
procedure GLESCheckError(const ProcName: String);
var
  Err: GLenum;
begin
  if not Assigned(glGetError_Proc) then
    Exit;
  Err := glGetError_Proc();
  if Err <> GL_NO_ERROR then
    WritelnWarning('OpenGLES', ProcName + ' error: ' + GLEnumName(Err));
end;


procedure glActiveTexture(texture:GLenum);
begin
  WritelnLog('OpenGLES', 'glActiveTexture' + ' texture=' + GLEnumName(texture));
  glActiveTexture_Proc(texture);
  GLESCheckError('glActiveTexture');
end;

procedure glAttachShader(_program:GLuint; shader:GLuint);
begin
  WritelnLog('OpenGLES', 'glAttachShader' + ' _program=' + IntToStr(Integer(_program)) + ' shader=' + IntToStr(Integer(shader)));
  glAttachShader_Proc(_program, shader);
  GLESCheckError('glAttachShader');
end;

procedure glBindAttribLocation(_program:GLuint; index:GLuint; name:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glBindAttribLocation' + ' _program=' + IntToStr(Integer(_program)) + ' index=' + IntToStr(Integer(index)));
  glBindAttribLocation_Proc(_program, index, name);
  GLESCheckError('glBindAttribLocation');
end;

procedure glBindBuffer(target:GLenum; buffer:GLuint);
begin
  WritelnLog('OpenGLES', 'glBindBuffer' + ' target=' + GLEnumName(target) + ' buffer=' + IntToStr(Integer(buffer)));
  glBindBuffer_Proc(target, buffer);
  GLESCheckError('glBindBuffer');
end;

procedure glBindFramebuffer(target:GLenum; framebuffer:GLuint);
begin
  WritelnLog('OpenGLES', 'glBindFramebuffer' + ' target=' + GLEnumName(target) + ' framebuffer=' + IntToStr(Integer(framebuffer)));
  glBindFramebuffer_Proc(target, framebuffer);
  GLESCheckError('glBindFramebuffer');
end;

procedure glBindRenderbuffer(target:GLenum; renderbuffer:GLuint);
begin
  WritelnLog('OpenGLES', 'glBindRenderbuffer' + ' target=' + GLEnumName(target) + ' renderbuffer=' + IntToStr(Integer(renderbuffer)));
  glBindRenderbuffer_Proc(target, renderbuffer);
  GLESCheckError('glBindRenderbuffer');
end;

procedure glBindTexture(target:GLenum; texture:GLuint);
begin
  WritelnLog('OpenGLES', 'glBindTexture' + ' target=' + GLEnumName(target) + ' texture=' + IntToStr(Integer(texture)));
  glBindTexture_Proc(target, texture);
  GLESCheckError('glBindTexture');
end;

procedure glBlendColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);
begin
  WritelnLog('OpenGLES', 'glBlendColor');
  glBlendColor_Proc(red, green, blue, alpha);
  GLESCheckError('glBlendColor');
end;

procedure glBlendEquation(mode:GLenum);
begin
  WritelnLog('OpenGLES', 'glBlendEquation' + ' mode=' + GLEnumName(mode));
  glBlendEquation_Proc(mode);
  GLESCheckError('glBlendEquation');
end;

procedure glBlendEquationSeparate(modeRGB:GLenum; modeAlpha:GLenum);
begin
  WritelnLog('OpenGLES', 'glBlendEquationSeparate' + ' modeRGB=' + GLEnumName(modeRGB) + ' modeAlpha=' + GLEnumName(modeAlpha));
  glBlendEquationSeparate_Proc(modeRGB, modeAlpha);
  GLESCheckError('glBlendEquationSeparate');
end;

procedure glBlendFunc(sfactor:GLenum; dfactor:GLenum);
begin
  WritelnLog('OpenGLES', 'glBlendFunc' + ' sfactor=' + GLEnumName(sfactor) + ' dfactor=' + GLEnumName(dfactor));
  glBlendFunc_Proc(sfactor, dfactor);
  GLESCheckError('glBlendFunc');
end;

procedure glBlendFuncSeparate(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);
begin
  WritelnLog('OpenGLES', 'glBlendFuncSeparate' + ' srcRGB=' + GLEnumName(srcRGB) + ' dstRGB=' + GLEnumName(dstRGB) + ' srcAlpha=' + GLEnumName(srcAlpha) + ' dstAlpha=' + GLEnumName(dstAlpha));
  glBlendFuncSeparate_Proc(srcRGB, dstRGB, srcAlpha, dstAlpha);
  GLESCheckError('glBlendFuncSeparate');
end;

procedure glBufferData(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);
begin
  WritelnLog('OpenGLES', 'glBufferData' + ' target=' + GLEnumName(target) + ' size=' + IntToStr(size) + ' usage=' + GLEnumName(usage));
  glBufferData_Proc(target, size, data, usage);
  GLESCheckError('glBufferData');
end;

procedure glBufferSubData(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);
begin
  WritelnLog('OpenGLES', 'glBufferSubData' + ' target=' + GLEnumName(target) + ' offset=' + IntToStr(offset) + ' size=' + IntToStr(size));
  glBufferSubData_Proc(target, offset, size, data);
  GLESCheckError('glBufferSubData');
end;

function glCheckFramebufferStatus(target:GLenum): GLenum;
begin
  WritelnLog('OpenGLES', 'glCheckFramebufferStatus' + ' target=' + GLEnumName(target));
  Result := glCheckFramebufferStatus_Proc(target);
  GLESCheckError('glCheckFramebufferStatus');
end;

procedure glClear(mask:GLbitfield);
begin
  WritelnLog('OpenGLES', 'glClear' + ' mask=' + IntToStr(Integer(mask)));
  glClear_Proc(mask);
  GLESCheckError('glClear');
end;

procedure glClearColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);
begin
  WritelnLog('OpenGLES', 'glClearColor');
  glClearColor_Proc(red, green, blue, alpha);
  GLESCheckError('glClearColor');
end;

procedure glClearDepthf(depth:GLclampf);
begin
  WritelnLog('OpenGLES', 'glClearDepthf');
  glClearDepthf_Proc(depth);
  GLESCheckError('glClearDepthf');
end;

procedure glClearStencil(s:GLint);
begin
  WritelnLog('OpenGLES', 'glClearStencil' + ' s=' + IntToStr(s));
  glClearStencil_Proc(s);
  GLESCheckError('glClearStencil');
end;

procedure glColorMask(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);
begin
  WritelnLog('OpenGLES', 'glColorMask');
  glColorMask_Proc(red, green, blue, alpha);
  GLESCheckError('glColorMask');
end;

procedure glCompileShader(shader:GLuint);
begin
  WritelnLog('OpenGLES', 'glCompileShader' + ' shader=' + IntToStr(Integer(shader)));
  glCompileShader_Proc(shader);
  GLESCheckError('glCompileShader');
end;

procedure glCompressedTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glCompressedTexImage2D_Proc(target, level, internalformat, width, height, border, imageSize, data);
  GLESCheckError('glCompressedTexImage2D');
end;

procedure glCompressedTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei; height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexSubImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCompressedTexSubImage2D_Proc(target, level, xoffset, yoffset, width, height, format, imageSize, data);
  GLESCheckError('glCompressedTexSubImage2D');
end;

procedure glCopyTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint; width:GLsizei; height:GLsizei; border:GLint);
begin
  WritelnLog('OpenGLES', 'glCopyTexImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' x=' + IntToStr(x));
  glCopyTexImage2D_Proc(target, level, internalformat, x, y, width, height, border);
  GLESCheckError('glCopyTexImage2D');
end;

procedure glCopyTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint; y:GLint; width:GLsizei; height:GLsizei);
begin
  WritelnLog('OpenGLES', 'glCopyTexSubImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCopyTexSubImage2D_Proc(target, level, xoffset, yoffset, x, y, width, height);
  GLESCheckError('glCopyTexSubImage2D');
end;

function glCreateProgram: GLuint;
begin
  WritelnLog('OpenGLES', 'glCreateProgram');
  Result := glCreateProgram_Proc();
  GLESCheckError('glCreateProgram');
end;

function glCreateShader(_type:GLenum): GLuint;
begin
  WritelnLog('OpenGLES', 'glCreateShader' + ' _type=' + GLEnumName(_type));
  Result := glCreateShader_Proc(_type);
  GLESCheckError('glCreateShader');
end;

procedure glCullFace(mode:GLenum);
begin
  WritelnLog('OpenGLES', 'glCullFace' + ' mode=' + GLEnumName(mode));
  glCullFace_Proc(mode);
  GLESCheckError('glCullFace');
end;

procedure glDeleteBuffers(n:GLsizei; buffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteBuffers' + ' n=' + IntToStr(n));
  glDeleteBuffers_Proc(n, buffers);
  GLESCheckError('glDeleteBuffers');
end;

procedure glDeleteFramebuffers(n:GLsizei; framebuffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteFramebuffers' + ' n=' + IntToStr(n));
  glDeleteFramebuffers_Proc(n, framebuffers);
  GLESCheckError('glDeleteFramebuffers');
end;

procedure glDeleteProgram(_program:GLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteProgram' + ' _program=' + IntToStr(Integer(_program)));
  glDeleteProgram_Proc(_program);
  GLESCheckError('glDeleteProgram');
end;

procedure glDeleteRenderbuffers(n:GLsizei; renderbuffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteRenderbuffers' + ' n=' + IntToStr(n));
  glDeleteRenderbuffers_Proc(n, renderbuffers);
  GLESCheckError('glDeleteRenderbuffers');
end;

procedure glDeleteShader(shader:GLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteShader' + ' shader=' + IntToStr(Integer(shader)));
  glDeleteShader_Proc(shader);
  GLESCheckError('glDeleteShader');
end;

procedure glDeleteTextures(n:GLsizei; textures:pGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteTextures' + ' n=' + IntToStr(n));
  glDeleteTextures_Proc(n, textures);
  GLESCheckError('glDeleteTextures');
end;

procedure glDepthFunc(func:GLenum);
begin
  WritelnLog('OpenGLES', 'glDepthFunc' + ' func=' + GLEnumName(func));
  glDepthFunc_Proc(func);
  GLESCheckError('glDepthFunc');
end;

procedure glDepthMask(flag:GLboolean);
begin
  WritelnLog('OpenGLES', 'glDepthMask');
  glDepthMask_Proc(flag);
  GLESCheckError('glDepthMask');
end;

procedure glDepthRangef(zNear:GLclampf; zFar:GLclampf);
begin
  WritelnLog('OpenGLES', 'glDepthRangef');
  glDepthRangef_Proc(zNear, zFar);
  GLESCheckError('glDepthRangef');
end;

procedure glDetachShader(_program:GLuint; shader:GLuint);
begin
  WritelnLog('OpenGLES', 'glDetachShader' + ' _program=' + IntToStr(Integer(_program)) + ' shader=' + IntToStr(Integer(shader)));
  glDetachShader_Proc(_program, shader);
  GLESCheckError('glDetachShader');
end;

procedure glDisable(cap:GLenum);
begin
  WritelnLog('OpenGLES', 'glDisable' + ' cap=' + GLEnumName(cap));
  glDisable_Proc(cap);
  GLESCheckError('glDisable');
end;

procedure glDisableVertexAttribArray(index:GLuint);
begin
  WritelnLog('OpenGLES', 'glDisableVertexAttribArray' + ' index=' + IntToStr(Integer(index)));
  glDisableVertexAttribArray_Proc(index);
  GLESCheckError('glDisableVertexAttribArray');
end;

procedure glDrawArrays(mode:GLenum; first:GLint; count:GLsizei);
begin
  WritelnLog('OpenGLES', 'glDrawArrays' + ' mode=' + GLEnumName(mode) + ' first=' + IntToStr(first) + ' count=' + IntToStr(count));
  glDrawArrays_Proc(mode, first, count);
  GLESCheckError('glDrawArrays');
end;

procedure glDrawElements(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);
begin
  WritelnLog('OpenGLES', 'glDrawElements' + ' mode=' + GLEnumName(mode) + ' count=' + IntToStr(count) + ' _type=' + GLEnumName(_type));
  glDrawElements_Proc(mode, count, _type, indices);
  GLESCheckError('glDrawElements');
end;

procedure glEnable(cap:GLenum);
begin
  WritelnLog('OpenGLES', 'glEnable' + ' cap=' + GLEnumName(cap));
  glEnable_Proc(cap);
  GLESCheckError('glEnable');
end;

procedure glEnableVertexAttribArray(index:GLuint);
begin
  WritelnLog('OpenGLES', 'glEnableVertexAttribArray' + ' index=' + IntToStr(Integer(index)));
  glEnableVertexAttribArray_Proc(index);
  GLESCheckError('glEnableVertexAttribArray');
end;

procedure glFinish;
begin
  WritelnLog('OpenGLES', 'glFinish');
  glFinish_Proc;
  GLESCheckError('glFinish');
end;

procedure glFlush;
begin
  WritelnLog('OpenGLES', 'glFlush');
  glFlush_Proc;
  GLESCheckError('glFlush');
end;

procedure glFramebufferRenderbuffer(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);
begin
  WritelnLog('OpenGLES', 'glFramebufferRenderbuffer' + ' target=' + GLEnumName(target) + ' attachment=' + GLEnumName(attachment) + ' renderbuffertarget=' + GLEnumName(renderbuffertarget) + ' renderbuffer=' + IntToStr(Integer(renderbuffer)));
  glFramebufferRenderbuffer_Proc(target, attachment, renderbuffertarget, renderbuffer);
  GLESCheckError('glFramebufferRenderbuffer');
end;

procedure glFramebufferTexture2D(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);
begin
  WritelnLog('OpenGLES', 'glFramebufferTexture2D' + ' target=' + GLEnumName(target) + ' attachment=' + GLEnumName(attachment) + ' textarget=' + GLEnumName(textarget) + ' texture=' + IntToStr(Integer(texture)));
  glFramebufferTexture2D_Proc(target, attachment, textarget, texture, level);
  GLESCheckError('glFramebufferTexture2D');
end;

procedure glFrontFace(mode:GLenum);
begin
  WritelnLog('OpenGLES', 'glFrontFace' + ' mode=' + GLEnumName(mode));
  glFrontFace_Proc(mode);
  GLESCheckError('glFrontFace');
end;

procedure glGenBuffers(n:GLsizei; buffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGenBuffers' + ' n=' + IntToStr(n));
  glGenBuffers_Proc(n, buffers);
  GLESCheckError('glGenBuffers');
end;

procedure glGenerateMipmap(target:GLenum);
begin
  WritelnLog('OpenGLES', 'glGenerateMipmap' + ' target=' + GLEnumName(target));
  glGenerateMipmap_Proc(target);
  GLESCheckError('glGenerateMipmap');
end;

procedure glGenFramebuffers(n:GLsizei; framebuffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGenFramebuffers' + ' n=' + IntToStr(n));
  glGenFramebuffers_Proc(n, framebuffers);
  GLESCheckError('glGenFramebuffers');
end;

procedure glGenRenderbuffers(n:GLsizei; renderbuffers:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGenRenderbuffers' + ' n=' + IntToStr(n));
  glGenRenderbuffers_Proc(n, renderbuffers);
  GLESCheckError('glGenRenderbuffers');
end;

procedure glGenTextures(n:GLsizei; textures:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGenTextures' + ' n=' + IntToStr(n));
  glGenTextures_Proc(n, textures);
  GLESCheckError('glGenTextures');
end;

procedure glGetActiveAttrib(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; _type:pGLenum; name:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetActiveAttrib' + ' _program=' + IntToStr(Integer(_program)) + ' index=' + IntToStr(Integer(index)) + ' bufsize=' + IntToStr(bufsize));
  glGetActiveAttrib_Proc(_program, index, bufsize, length, size, _type, name);
  GLESCheckError('glGetActiveAttrib');
end;

procedure glGetActiveUniform(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; _type:pGLenum; name:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetActiveUniform' + ' _program=' + IntToStr(Integer(_program)) + ' index=' + IntToStr(Integer(index)) + ' bufsize=' + IntToStr(bufsize));
  glGetActiveUniform_Proc(_program, index, bufsize, length, size, _type, name);
  GLESCheckError('glGetActiveUniform');
end;

procedure glGetAttachedShaders(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGetAttachedShaders' + ' _program=' + IntToStr(Integer(_program)) + ' maxcount=' + IntToStr(maxcount));
  glGetAttachedShaders_Proc(_program, maxcount, count, shaders);
  GLESCheckError('glGetAttachedShaders');
end;

function glGetAttribLocation(_program:GLuint; name:PAnsiChar): CInt32;
begin
  WritelnLog('OpenGLES', 'glGetAttribLocation' + ' _program=' + IntToStr(Integer(_program)));
  Result := glGetAttribLocation_Proc(_program, name);
  GLESCheckError('glGetAttribLocation');
end;

procedure glGetBooleanv(pname:GLenum; params:pGLboolean);
begin
  WritelnLog('OpenGLES', 'glGetBooleanv' + ' pname=' + GLEnumName(pname));
  glGetBooleanv_Proc(pname, params);
  GLESCheckError('glGetBooleanv');
end;

procedure glGetBufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetBufferParameteriv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetBufferParameteriv_Proc(target, pname, params);
  GLESCheckError('glGetBufferParameteriv');
end;

function glGetError: GLenum;
begin
  WritelnLog('OpenGLES', 'glGetError');
  Result := glGetError_Proc();
end;

procedure glGetFloatv(pname:GLenum; params:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glGetFloatv' + ' pname=' + GLEnumName(pname));
  glGetFloatv_Proc(pname, params);
  GLESCheckError('glGetFloatv');
end;

procedure glGetFramebufferAttachmentParameteriv(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetFramebufferAttachmentParameteriv' + ' target=' + GLEnumName(target) + ' attachment=' + GLEnumName(attachment) + ' pname=' + GLEnumName(pname));
  glGetFramebufferAttachmentParameteriv_Proc(target, attachment, pname, params);
  GLESCheckError('glGetFramebufferAttachmentParameteriv');
end;

procedure glGetIntegerv(pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetIntegerv' + ' pname=' + GLEnumName(pname));
  glGetIntegerv_Proc(pname, params);
  GLESCheckError('glGetIntegerv');
end;

procedure glGetProgramiv(_program:GLuint; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetProgramiv' + ' _program=' + IntToStr(Integer(_program)) + ' pname=' + GLEnumName(pname));
  glGetProgramiv_Proc(_program, pname, params);
  GLESCheckError('glGetProgramiv');
end;

procedure glGetProgramInfoLog(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetProgramInfoLog' + ' _program=' + IntToStr(Integer(_program)) + ' bufsize=' + IntToStr(bufsize));
  glGetProgramInfoLog_Proc(_program, bufsize, length, infolog);
  GLESCheckError('glGetProgramInfoLog');
end;

procedure glGetRenderbufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetRenderbufferParameteriv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetRenderbufferParameteriv_Proc(target, pname, params);
  GLESCheckError('glGetRenderbufferParameteriv');
end;

procedure glGetShaderiv(shader:GLuint; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetShaderiv' + ' shader=' + IntToStr(Integer(shader)) + ' pname=' + GLEnumName(pname));
  glGetShaderiv_Proc(shader, pname, params);
  GLESCheckError('glGetShaderiv');
end;

procedure glGetShaderInfoLog(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetShaderInfoLog' + ' shader=' + IntToStr(Integer(shader)) + ' bufsize=' + IntToStr(bufsize));
  glGetShaderInfoLog_Proc(shader, bufsize, length, infolog);
  GLESCheckError('glGetShaderInfoLog');
end;

procedure glGetShaderPrecisionFormat(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetShaderPrecisionFormat' + ' shadertype=' + GLEnumName(shadertype) + ' precisiontype=' + GLEnumName(precisiontype));
  glGetShaderPrecisionFormat_Proc(shadertype, precisiontype, range, precision);
  GLESCheckError('glGetShaderPrecisionFormat');
end;

procedure glGetShaderSource(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetShaderSource' + ' shader=' + IntToStr(Integer(shader)) + ' bufsize=' + IntToStr(bufsize));
  glGetShaderSource_Proc(shader, bufsize, length, source);
  GLESCheckError('glGetShaderSource');
end;

function glGetString(name:GLenum): PGLubyte;
begin
  WritelnLog('OpenGLES', 'glGetString' + ' name=' + GLEnumName(name));
  Result := glGetString_Proc(name);
  GLESCheckError('glGetString');
end;

procedure glGetTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glGetTexParameterfv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetTexParameterfv_Proc(target, pname, params);
  GLESCheckError('glGetTexParameterfv');
end;

procedure glGetTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetTexParameteriv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetTexParameteriv_Proc(target, pname, params);
  GLESCheckError('glGetTexParameteriv');
end;

procedure glGetUniformfv(_program:GLuint; location:GLint; params:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glGetUniformfv' + ' _program=' + IntToStr(Integer(_program)) + ' location=' + IntToStr(location));
  glGetUniformfv_Proc(_program, location, params);
  GLESCheckError('glGetUniformfv');
end;

procedure glGetUniformiv(_program:GLuint; location:GLint; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetUniformiv' + ' _program=' + IntToStr(Integer(_program)) + ' location=' + IntToStr(location));
  glGetUniformiv_Proc(_program, location, params);
  GLESCheckError('glGetUniformiv');
end;

function glGetUniformLocation(_program:GLuint; name:PAnsiChar): CInt32;
begin
  WritelnLog('OpenGLES', 'glGetUniformLocation' + ' _program=' + IntToStr(Integer(_program)));
  Result := glGetUniformLocation_Proc(_program, name);
  GLESCheckError('glGetUniformLocation');
end;

procedure glGetVertexAttribfv(index:GLuint; pname:GLenum; params:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glGetVertexAttribfv' + ' index=' + IntToStr(Integer(index)) + ' pname=' + GLEnumName(pname));
  glGetVertexAttribfv_Proc(index, pname, params);
  GLESCheckError('glGetVertexAttribfv');
end;

procedure glGetVertexAttribiv(index:GLuint; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetVertexAttribiv' + ' index=' + IntToStr(Integer(index)) + ' pname=' + GLEnumName(pname));
  glGetVertexAttribiv_Proc(index, pname, params);
  GLESCheckError('glGetVertexAttribiv');
end;

procedure glGetVertexAttribPointerv(index:GLuint; pname:GLenum; pointer:Ppointer);
begin
  WritelnLog('OpenGLES', 'glGetVertexAttribPointerv' + ' index=' + IntToStr(Integer(index)) + ' pname=' + GLEnumName(pname));
  glGetVertexAttribPointerv_Proc(index, pname, pointer);
  GLESCheckError('glGetVertexAttribPointerv');
end;

procedure glHint(target:GLenum; mode:GLenum);
begin
  WritelnLog('OpenGLES', 'glHint' + ' target=' + GLEnumName(target) + ' mode=' + GLEnumName(mode));
  glHint_Proc(target, mode);
  GLESCheckError('glHint');
end;

function glIsBuffer(buffer:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsBuffer' + ' buffer=' + IntToStr(Integer(buffer)));
  Result := glIsBuffer_Proc(buffer);
  GLESCheckError('glIsBuffer');
end;

function glIsEnabled(cap:GLenum): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsEnabled' + ' cap=' + GLEnumName(cap));
  Result := glIsEnabled_Proc(cap);
  GLESCheckError('glIsEnabled');
end;

function glIsFramebuffer(framebuffer:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsFramebuffer' + ' framebuffer=' + IntToStr(Integer(framebuffer)));
  Result := glIsFramebuffer_Proc(framebuffer);
  GLESCheckError('glIsFramebuffer');
end;

function glIsProgram(_program:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsProgram' + ' _program=' + IntToStr(Integer(_program)));
  Result := glIsProgram_Proc(_program);
  GLESCheckError('glIsProgram');
end;

function glIsRenderbuffer(renderbuffer:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsRenderbuffer' + ' renderbuffer=' + IntToStr(Integer(renderbuffer)));
  Result := glIsRenderbuffer_Proc(renderbuffer);
  GLESCheckError('glIsRenderbuffer');
end;

function glIsShader(shader:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsShader' + ' shader=' + IntToStr(Integer(shader)));
  Result := glIsShader_Proc(shader);
  GLESCheckError('glIsShader');
end;

function glIsTexture(texture:GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsTexture' + ' texture=' + IntToStr(Integer(texture)));
  Result := glIsTexture_Proc(texture);
  GLESCheckError('glIsTexture');
end;

procedure glLineWidth(width:GLfloat);
begin
  WritelnLog('OpenGLES', 'glLineWidth');
  glLineWidth_Proc(width);
  GLESCheckError('glLineWidth');
end;

procedure glLinkProgram(_program:GLuint);
begin
  WritelnLog('OpenGLES', 'glLinkProgram' + ' _program=' + IntToStr(Integer(_program)));
  glLinkProgram_Proc(_program);
  GLESCheckError('glLinkProgram');
end;

procedure glPixelStorei(pname:GLenum; param:GLint);
begin
  WritelnLog('OpenGLES', 'glPixelStorei' + ' pname=' + GLEnumName(pname) + ' param=' + IntToStr(param));
  glPixelStorei_Proc(pname, param);
  GLESCheckError('glPixelStorei');
end;

procedure glPolygonOffset(factor:GLfloat; units:GLfloat);
begin
  WritelnLog('OpenGLES', 'glPolygonOffset');
  glPolygonOffset_Proc(factor, units);
  GLESCheckError('glPolygonOffset');
end;

procedure glReadPixels(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glReadPixels' + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' width=' + IntToStr(width) + ' height=' + IntToStr(height));
  glReadPixels_Proc(x, y, width, height, format, _type, pixels);
  GLESCheckError('glReadPixels');
end;

procedure glReleaseShaderCompiler;
begin
  WritelnLog('OpenGLES', 'glReleaseShaderCompiler');
  glReleaseShaderCompiler_Proc;
  GLESCheckError('glReleaseShaderCompiler');
end;

procedure glRenderbufferStorage(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);
begin
  WritelnLog('OpenGLES', 'glRenderbufferStorage' + ' target=' + GLEnumName(target) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width) + ' height=' + IntToStr(height));
  glRenderbufferStorage_Proc(target, internalformat, width, height);
  GLESCheckError('glRenderbufferStorage');
end;

procedure glSampleCoverage(value:GLclampf; invert:GLboolean);
begin
  WritelnLog('OpenGLES', 'glSampleCoverage');
  glSampleCoverage_Proc(value, invert);
  GLESCheckError('glSampleCoverage');
end;

procedure glScissor(x:GLint; y:GLint; width:GLsizei; height:GLsizei);
begin
  WritelnLog('OpenGLES', 'glScissor' + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' width=' + IntToStr(width) + ' height=' + IntToStr(height));
  glScissor_Proc(x, y, width, height);
  GLESCheckError('glScissor');
end;

procedure glShaderBinary(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);
begin
  WritelnLog('OpenGLES', 'glShaderBinary' + ' n=' + IntToStr(n) + ' binaryformat=' + GLEnumName(binaryformat));
  glShaderBinary_Proc(n, shaders, binaryformat, binary, length);
  GLESCheckError('glShaderBinary');
end;

procedure glShaderSource(shader:GLuint; count:GLsizei; _string:PPGLchar; length:pGLint);
begin
  WritelnLog('OpenGLES', 'glShaderSource' + ' shader=' + IntToStr(Integer(shader)) + ' count=' + IntToStr(count));
  glShaderSource_Proc(shader, count, _string, length);
  GLESCheckError('glShaderSource');
end;

procedure glStencilFunc(func:GLenum; ref:GLint; mask:GLuint);
begin
  WritelnLog('OpenGLES', 'glStencilFunc' + ' func=' + GLEnumName(func) + ' ref=' + IntToStr(ref) + ' mask=' + IntToStr(Integer(mask)));
  glStencilFunc_Proc(func, ref, mask);
  GLESCheckError('glStencilFunc');
end;

procedure glStencilFuncSeparate(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);
begin
  WritelnLog('OpenGLES', 'glStencilFuncSeparate' + ' face=' + GLEnumName(face) + ' func=' + GLEnumName(func) + ' ref=' + IntToStr(ref) + ' mask=' + IntToStr(Integer(mask)));
  glStencilFuncSeparate_Proc(face, func, ref, mask);
  GLESCheckError('glStencilFuncSeparate');
end;

procedure glStencilMask(mask:GLuint);
begin
  WritelnLog('OpenGLES', 'glStencilMask' + ' mask=' + IntToStr(Integer(mask)));
  glStencilMask_Proc(mask);
  GLESCheckError('glStencilMask');
end;

procedure glStencilMaskSeparate(face:GLenum; mask:GLuint);
begin
  WritelnLog('OpenGLES', 'glStencilMaskSeparate' + ' face=' + GLEnumName(face) + ' mask=' + IntToStr(Integer(mask)));
  glStencilMaskSeparate_Proc(face, mask);
  GLESCheckError('glStencilMaskSeparate');
end;

procedure glStencilOp(fail:GLenum; zfail:GLenum; zpass:GLenum);
begin
  WritelnLog('OpenGLES', 'glStencilOp' + ' fail=' + GLEnumName(fail) + ' zfail=' + GLEnumName(zfail) + ' zpass=' + GLEnumName(zpass));
  glStencilOp_Proc(fail, zfail, zpass);
  GLESCheckError('glStencilOp');
end;

procedure glStencilOpSeparate(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);
begin
  WritelnLog('OpenGLES', 'glStencilOpSeparate' + ' face=' + GLEnumName(face) + ' fail=' + GLEnumName(fail) + ' zfail=' + GLEnumName(zfail) + ' zpass=' + GLEnumName(zpass));
  glStencilOpSeparate_Proc(face, fail, zfail, zpass);
  GLESCheckError('glStencilOpSeparate');
end;

procedure glTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glTexImage2D_Proc(target, level, internalformat, width, height, border, format, _type, pixels);
  GLESCheckError('glTexImage2D');
end;

procedure glTexParameterf(target:GLenum; pname:GLenum; param:GLfloat);
begin
  WritelnLog('OpenGLES', 'glTexParameterf' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glTexParameterf_Proc(target, pname, param);
  GLESCheckError('glTexParameterf');
end;

procedure glTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glTexParameterfv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glTexParameterfv_Proc(target, pname, params);
  GLESCheckError('glTexParameterfv');
end;

procedure glTexParameteri(target:GLenum; pname:GLenum; param:GLint);
begin
  WritelnLog('OpenGLES', 'glTexParameteri' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname) + ' param=' + IntToStr(param));
  glTexParameteri_Proc(target, pname, param);
  GLESCheckError('glTexParameteri');
end;

procedure glTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);
begin
  WritelnLog('OpenGLES', 'glTexParameteriv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glTexParameteriv_Proc(target, pname, params);
  GLESCheckError('glTexParameteriv');
end;

procedure glTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei; height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexSubImage2D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glTexSubImage2D_Proc(target, level, xoffset, yoffset, width, height, format, _type, pixels);
  GLESCheckError('glTexSubImage2D');
end;

procedure glUniform1f(location:GLint; x:GLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform1f' + ' location=' + IntToStr(location));
  glUniform1f_Proc(location, x);
  GLESCheckError('glUniform1f');
end;

procedure glUniform1fv(location:GLint; count:GLsizei; v:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform1fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform1fv_Proc(location, count, v);
  GLESCheckError('glUniform1fv');
end;

procedure glUniform1i(location:GLint; x:GLint);
begin
  WritelnLog('OpenGLES', 'glUniform1i' + ' location=' + IntToStr(location) + ' x=' + IntToStr(x));
  glUniform1i_Proc(location, x);
  GLESCheckError('glUniform1i');
end;

procedure glUniform1iv(location:GLint; count:GLsizei; v:pGLint);
begin
  WritelnLog('OpenGLES', 'glUniform1iv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform1iv_Proc(location, count, v);
  GLESCheckError('glUniform1iv');
end;

procedure glUniform2f(location:GLint; x:GLfloat; y:GLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform2f' + ' location=' + IntToStr(location));
  glUniform2f_Proc(location, x, y);
  GLESCheckError('glUniform2f');
end;

procedure glUniform2fv(location:GLint; count:GLsizei; v:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform2fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform2fv_Proc(location, count, v);
  GLESCheckError('glUniform2fv');
end;

procedure glUniform2i(location:GLint; x:GLint; y:GLint);
begin
  WritelnLog('OpenGLES', 'glUniform2i' + ' location=' + IntToStr(location) + ' x=' + IntToStr(x) + ' y=' + IntToStr(y));
  glUniform2i_Proc(location, x, y);
  GLESCheckError('glUniform2i');
end;

procedure glUniform2iv(location:GLint; count:GLsizei; v:pGLint);
begin
  WritelnLog('OpenGLES', 'glUniform2iv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform2iv_Proc(location, count, v);
  GLESCheckError('glUniform2iv');
end;

procedure glUniform3f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform3f' + ' location=' + IntToStr(location));
  glUniform3f_Proc(location, x, y, z);
  GLESCheckError('glUniform3f');
end;

procedure glUniform3fv(location:GLint; count:GLsizei; v:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform3fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform3fv_Proc(location, count, v);
  GLESCheckError('glUniform3fv');
end;

procedure glUniform3i(location:GLint; x:GLint; y:GLint; z:GLint);
begin
  WritelnLog('OpenGLES', 'glUniform3i' + ' location=' + IntToStr(location) + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' z=' + IntToStr(z));
  glUniform3i_Proc(location, x, y, z);
  GLESCheckError('glUniform3i');
end;

procedure glUniform3iv(location:GLint; count:GLsizei; v:pGLint);
begin
  WritelnLog('OpenGLES', 'glUniform3iv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform3iv_Proc(location, count, v);
  GLESCheckError('glUniform3iv');
end;

procedure glUniform4f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform4f' + ' location=' + IntToStr(location));
  glUniform4f_Proc(location, x, y, z, w);
  GLESCheckError('glUniform4f');
end;

procedure glUniform4fv(location:GLint; count:GLsizei; v:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniform4fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform4fv_Proc(location, count, v);
  GLESCheckError('glUniform4fv');
end;

procedure glUniform4i(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);
begin
  WritelnLog('OpenGLES', 'glUniform4i' + ' location=' + IntToStr(location) + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' z=' + IntToStr(z));
  glUniform4i_Proc(location, x, y, z, w);
  GLESCheckError('glUniform4i');
end;

procedure glUniform4iv(location:GLint; count:GLsizei; v:pGLint);
begin
  WritelnLog('OpenGLES', 'glUniform4iv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform4iv_Proc(location, count, v);
  GLESCheckError('glUniform4iv');
end;

procedure glUniformMatrix2fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix2fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix2fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix2fv');
end;

procedure glUniformMatrix3fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix3fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix3fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix3fv');
end;

procedure glUniformMatrix4fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix4fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix4fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix4fv');
end;

procedure glUseProgram(_program:GLuint);
begin
  WritelnLog('OpenGLES', 'glUseProgram' + ' _program=' + IntToStr(Integer(_program)));
  glUseProgram_Proc(_program);
  GLESCheckError('glUseProgram');
end;

procedure glValidateProgram(_program:GLuint);
begin
  WritelnLog('OpenGLES', 'glValidateProgram' + ' _program=' + IntToStr(Integer(_program)));
  glValidateProgram_Proc(_program);
  GLESCheckError('glValidateProgram');
end;

procedure glVertexAttrib1f(indx:GLuint; x:GLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib1f' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib1f_Proc(indx, x);
  GLESCheckError('glVertexAttrib1f');
end;

procedure glVertexAttrib1fv(indx:GLuint; values:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib1fv' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib1fv_Proc(indx, values);
  GLESCheckError('glVertexAttrib1fv');
end;

procedure glVertexAttrib2f(indx:GLuint; x:GLfloat; y:GLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib2f' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib2f_Proc(indx, x, y);
  GLESCheckError('glVertexAttrib2f');
end;

procedure glVertexAttrib2fv(indx:GLuint; values:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib2fv' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib2fv_Proc(indx, values);
  GLESCheckError('glVertexAttrib2fv');
end;

procedure glVertexAttrib3f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib3f' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib3f_Proc(indx, x, y, z);
  GLESCheckError('glVertexAttrib3f');
end;

procedure glVertexAttrib3fv(indx:GLuint; values:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib3fv' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib3fv_Proc(indx, values);
  GLESCheckError('glVertexAttrib3fv');
end;

procedure glVertexAttrib4f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib4f' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib4f_Proc(indx, x, y, z, w);
  GLESCheckError('glVertexAttrib4f');
end;

procedure glVertexAttrib4fv(indx:GLuint; values:pGLfloat);
begin
  WritelnLog('OpenGLES', 'glVertexAttrib4fv' + ' indx=' + IntToStr(Integer(indx)));
  glVertexAttrib4fv_Proc(indx, values);
  GLESCheckError('glVertexAttrib4fv');
end;

procedure glVertexAttribPointer(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei; ptr:pointer);
begin
  WritelnLog('OpenGLES', 'glVertexAttribPointer' + ' indx=' + IntToStr(Integer(indx)) + ' size=' + IntToStr(size) + ' _type=' + GLEnumName(_type));
  glVertexAttribPointer_Proc(indx, size, _type, normalized, stride, ptr);
  GLESCheckError('glVertexAttribPointer');
end;

procedure glViewport(x:GLint; y:GLint; width:GLsizei; height:GLsizei);
begin
  WritelnLog('OpenGLES', 'glViewport' + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' width=' + IntToStr(width) + ' height=' + IntToStr(height));
  glViewport_Proc(x, y, width, height);
  GLESCheckError('glViewport');
end;

procedure glReadBuffer(Src: GLenum);
begin
  WritelnLog('OpenGLES', 'glReadBuffer' + ' Src=' + GLEnumName(Src));
  glReadBuffer_Proc(Src);
  GLESCheckError('glReadBuffer');
end;

procedure glDrawRangeElements(Mode: GLenum; Start, Endd: GLuint; Count: GLsizei; Kind: GLenum; Indices: Pointer);
begin
  WritelnLog('OpenGLES', 'glDrawRangeElements' + ' Mode=' + GLEnumName(Mode) + ' Start=' + IntToStr(Integer(Start)) + ' Endd=' + IntToStr(Integer(Endd)) + ' Count=' + IntToStr(Count));
  glDrawRangeElements_Proc(Mode, Start, Endd, Count, Kind, Indices);
  GLESCheckError('glDrawRangeElements');
end;

procedure glTexImage3D(target:TGLenum; level:TGLint; internalformat:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; border:TGLint; format:TGLenum; _type:TGLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexImage3D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + IntToStr(internalformat) + ' width=' + IntToStr(width));
  glTexImage3D_Proc(target, level, internalformat, width, height, depth, border, format, _type, pixels);
  GLESCheckError('glTexImage3D');
end;

procedure glTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; _type:TGLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexSubImage3D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glTexSubImage3D_Proc(target, level, xoffset, yoffset, zoffset, width, height, depth, format, _type, pixels);
  GLESCheckError('glTexSubImage3D');
end;

procedure glCopyTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glCopyTexSubImage3D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCopyTexSubImage3D_Proc(target, level, xoffset, yoffset, zoffset, x, y, width, height);
  GLESCheckError('glCopyTexSubImage3D');
end;

procedure glCompressedTexImage3D(target:TGLenum; level:TGLint; internalformat:TGLenum; width:TGLsizei; height:TGLsizei; depth:TGLsizei; border:TGLint; imageSize:TGLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexImage3D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glCompressedTexImage3D_Proc(target, level, internalformat, width, height, depth, border, imageSize, data);
  GLESCheckError('glCompressedTexImage3D');
end;

procedure glCompressedTexSubImage3D(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint; width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; imageSize:TGLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexSubImage3D' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCompressedTexSubImage3D_Proc(target, level, xoffset, yoffset, zoffset, width, height, depth, format, imageSize, data);
  GLESCheckError('glCompressedTexSubImage3D');
end;

procedure glGenQueries(n:TGLsizei; ids:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGenQueries' + ' n=' + IntToStr(n));
  glGenQueries_Proc(n, ids);
  GLESCheckError('glGenQueries');
end;

procedure glDeleteQueries(n:TGLsizei; ids:PGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteQueries' + ' n=' + IntToStr(n));
  glDeleteQueries_Proc(n, ids);
  GLESCheckError('glDeleteQueries');
end;

function glIsQuery(id:TGLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glIsQuery' + ' id=' + IntToStr(Integer(id)));
  Result := glIsQuery_Proc(id);
  GLESCheckError('glIsQuery');
end;

procedure glBeginQuery(target:TGLenum; id:TGLuint);
begin
  WritelnLog('OpenGLES', 'glBeginQuery' + ' target=' + GLEnumName(target) + ' id=' + IntToStr(Integer(id)));
  glBeginQuery_Proc(target, id);
  GLESCheckError('glBeginQuery');
end;

procedure glEndQuery(target:TGLenum);
begin
  WritelnLog('OpenGLES', 'glEndQuery' + ' target=' + GLEnumName(target));
  glEndQuery_Proc(target);
  GLESCheckError('glEndQuery');
end;

procedure glGetQueryiv(target:TGLenum; pname:TGLenum; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetQueryiv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetQueryiv_Proc(target, pname, params);
  GLESCheckError('glGetQueryiv');
end;

procedure glGetQueryObjectuiv(id:TGLuint; pname:TGLenum; params:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGetQueryObjectuiv' + ' id=' + IntToStr(Integer(id)) + ' pname=' + GLEnumName(pname));
  glGetQueryObjectuiv_Proc(id, pname, params);
  GLESCheckError('glGetQueryObjectuiv');
end;

function glUnmapBuffer(Target: GLuint): GLboolean;
begin
  WritelnLog('OpenGLES', 'glUnmapBuffer' + ' Target=' + IntToStr(Integer(Target)));
  Result := glUnmapBuffer_Proc(Target);
  GLESCheckError('glUnmapBuffer');
end;

procedure glGetBufferPointerv(target:TGLenum; pname:TGLenum; params:Ppointer);
begin
  WritelnLog('OpenGLES', 'glGetBufferPointerv' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetBufferPointerv_Proc(target, pname, params);
  GLESCheckError('glGetBufferPointerv');
end;

procedure glDrawBuffers(n:TGLsizei; bufs:PGLenum);
begin
  WritelnLog('OpenGLES', 'glDrawBuffers' + ' n=' + IntToStr(n));
  glDrawBuffers_Proc(n, bufs);
  GLESCheckError('glDrawBuffers');
end;

procedure glUniformMatrix2x3fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix2x3fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix2x3fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix2x3fv');
end;

procedure glUniformMatrix3x2fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix3x2fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix3x2fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix3x2fv');
end;

procedure glUniformMatrix2x4fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix2x4fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix2x4fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix2x4fv');
end;

procedure glUniformMatrix4x2fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix4x2fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix4x2fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix4x2fv');
end;

procedure glUniformMatrix3x4fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix3x4fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix3x4fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix3x4fv');
end;

procedure glUniformMatrix4x3fv(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glUniformMatrix4x3fv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniformMatrix4x3fv_Proc(location, count, transpose, value);
  GLESCheckError('glUniformMatrix4x3fv');
end;

procedure glBlitFramebuffer(srcX0:TGLint; srcY0:TGLint; srcX1:TGLint; srcY1:TGLint; dstX0:TGLint; dstY0:TGLint; dstX1:TGLint; dstY1:TGLint; mask:TGLbitfield; filter:TGLenum);
begin
  WritelnLog('OpenGLES', 'glBlitFramebuffer' + ' srcX0=' + IntToStr(srcX0) + ' srcY0=' + IntToStr(srcY0) + ' srcX1=' + IntToStr(srcX1) + ' srcY1=' + IntToStr(srcY1));
  glBlitFramebuffer_Proc(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
  GLESCheckError('glBlitFramebuffer');
end;

procedure glRenderbufferStorageMultisample(target:TGLenum; samples:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glRenderbufferStorageMultisample' + ' target=' + GLEnumName(target) + ' samples=' + IntToStr(samples) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glRenderbufferStorageMultisample_Proc(target, samples, internalformat, width, height);
  GLESCheckError('glRenderbufferStorageMultisample');
end;

procedure glFramebufferTextureLayer(target:TGLenum; attachment:TGLenum; texture:TGLuint; level:TGLint; layer:TGLint);
begin
  WritelnLog('OpenGLES', 'glFramebufferTextureLayer' + ' target=' + GLEnumName(target) + ' attachment=' + GLEnumName(attachment) + ' texture=' + IntToStr(Integer(texture)) + ' level=' + IntToStr(level));
  glFramebufferTextureLayer_Proc(target, attachment, texture, level, layer);
  GLESCheckError('glFramebufferTextureLayer');
end;

function glMapBufferRange(Target: GLuint; Offset: GLintptr; Len: GLsizeiptr; Access: GLbitfield): Pointer;
begin
  WritelnLog('OpenGLES', 'glMapBufferRange' + ' Target=' + IntToStr(Integer(Target)) + ' Offset=' + IntToStr(Offset) + ' Len=' + IntToStr(Len) + ' Access=' + IntToStr(Integer(Access)));
  Result := glMapBufferRange_Proc(Target, Offset, Len, Access);
  GLESCheckError('glMapBufferRange');
end;

procedure glFlushMappedBufferRange(target:TGLenum; offset:TGLintptr; length:TGLsizeiptr);
begin
  WritelnLog('OpenGLES', 'glFlushMappedBufferRange' + ' target=' + GLEnumName(target) + ' offset=' + IntToStr(offset) + ' length=' + IntToStr(length));
  glFlushMappedBufferRange_Proc(target, offset, length);
  GLESCheckError('glFlushMappedBufferRange');
end;

procedure glBindVertexArray(A: GLuint);
begin
  WritelnLog('OpenGLES', 'glBindVertexArray' + ' A=' + IntToStr(Integer(A)));
  glBindVertexArray_Proc(A);
  GLESCheckError('glBindVertexArray');
end;

procedure glDeleteVertexArrays(Count: GLuint; P: PGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteVertexArrays' + ' Count=' + IntToStr(Integer(Count)));
  glDeleteVertexArrays_Proc(Count, P);
  GLESCheckError('glDeleteVertexArrays');
end;

procedure glGenVertexArrays(Count: GLuint; P: PGLuint);
begin
  WritelnLog('OpenGLES', 'glGenVertexArrays' + ' Count=' + IntToStr(Integer(Count)));
  glGenVertexArrays_Proc(Count, P);
  GLESCheckError('glGenVertexArrays');
end;

function glIsVertexArray(arr:TGLuint): TGLboolean;
begin
  WritelnLog('OpenGLES', 'glIsVertexArray' + ' arr=' + IntToStr(Integer(arr)));
  Result := glIsVertexArray_Proc(arr);
  GLESCheckError('glIsVertexArray');
end;

procedure glGetIntegeri_v(target:TGLenum; ind:TGLuint; data:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetIntegeri_v' + ' target=' + GLEnumName(target) + ' ind=' + IntToStr(Integer(ind)));
  glGetIntegeri_v_Proc(target, ind, data);
  GLESCheckError('glGetIntegeri_v');
end;

procedure glBeginTransformFeedback(Mode: GLuint);
begin
  WritelnLog('OpenGLES', 'glBeginTransformFeedback' + ' Mode=' + IntToStr(Integer(Mode)));
  glBeginTransformFeedback_Proc(Mode);
  GLESCheckError('glBeginTransformFeedback');
end;

procedure glEndTransformFeedback;
begin
  WritelnLog('OpenGLES', 'glEndTransformFeedback');
  glEndTransformFeedback_Proc;
  GLESCheckError('glEndTransformFeedback');
end;

procedure glBindBufferRange(target:TGLenum; ind:TGLuint; buffer:TGLuint; offset:TGLintptr; size:TGLsizeiptr);
begin
  WritelnLog('OpenGLES', 'glBindBufferRange' + ' target=' + GLEnumName(target) + ' ind=' + IntToStr(Integer(ind)) + ' buffer=' + IntToStr(Integer(buffer)) + ' offset=' + IntToStr(offset));
  glBindBufferRange_Proc(target, ind, buffer, offset, size);
  GLESCheckError('glBindBufferRange');
end;

procedure glBindBufferBase(Target, Ind, Buffer: GLuint);
begin
  WritelnLog('OpenGLES', 'glBindBufferBase' + ' Target=' + IntToStr(Integer(Target)) + ' Ind=' + IntToStr(Integer(Ind)) + ' Buffer=' + IntToStr(Integer(Buffer)));
  glBindBufferBase_Proc(Target, Ind, Buffer);
  GLESCheckError('glBindBufferBase');
end;

procedure glTransformFeedbackVaryings(Prog: GLuint; Count: GLsizei; const Varyings: PPAnsiChar; BufferMode: GLuint);
begin
  WritelnLog('OpenGLES', 'glTransformFeedbackVaryings' + ' Prog=' + IntToStr(Integer(Prog)) + ' Count=' + IntToStr(Count) + ' BufferMode=' + IntToStr(Integer(BufferMode)));
  glTransformFeedbackVaryings_Proc(Prog, Count, Varyings, BufferMode);
  GLESCheckError('glTransformFeedbackVaryings');
end;

procedure glGetTransformFeedbackVarying(prog:TGLuint; ind:TGLuint; bufSize:TGLsizei; length:PGLsizei; size:PGLsizei; _type:PGLenum; name:PGLchar);
begin
  WritelnLog('OpenGLES', 'glGetTransformFeedbackVarying' + ' prog=' + IntToStr(Integer(prog)) + ' ind=' + IntToStr(Integer(ind)) + ' bufSize=' + IntToStr(bufSize));
  glGetTransformFeedbackVarying_Proc(prog, ind, bufSize, length, size, _type, name);
  GLESCheckError('glGetTransformFeedbackVarying');
end;

procedure glVertexAttribIPointer(ind:TGLuint; size:TGLint; _type:TGLenum; stride:TGLsizei; p:pointer);
begin
  WritelnLog('OpenGLES', 'glVertexAttribIPointer' + ' ind=' + IntToStr(Integer(ind)) + ' size=' + IntToStr(size) + ' _type=' + GLEnumName(_type) + ' stride=' + IntToStr(stride));
  glVertexAttribIPointer_Proc(ind, size, _type, stride, p);
  GLESCheckError('glVertexAttribIPointer');
end;

procedure glGetVertexAttribIiv(ind:TGLuint; pname:TGLenum; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetVertexAttribIiv' + ' ind=' + IntToStr(Integer(ind)) + ' pname=' + GLEnumName(pname));
  glGetVertexAttribIiv_Proc(ind, pname, params);
  GLESCheckError('glGetVertexAttribIiv');
end;

procedure glGetVertexAttribIuiv(ind:TGLuint; pname:TGLenum; params:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGetVertexAttribIuiv' + ' ind=' + IntToStr(Integer(ind)) + ' pname=' + GLEnumName(pname));
  glGetVertexAttribIuiv_Proc(ind, pname, params);
  GLESCheckError('glGetVertexAttribIuiv');
end;

procedure glVertexAttribI4i(ind:TGLuint; x:TGLint; y:TGLint; z:TGLint; w:TGLint);
begin
  WritelnLog('OpenGLES', 'glVertexAttribI4i' + ' ind=' + IntToStr(Integer(ind)) + ' x=' + IntToStr(x) + ' y=' + IntToStr(y) + ' z=' + IntToStr(z));
  glVertexAttribI4i_Proc(ind, x, y, z, w);
  GLESCheckError('glVertexAttribI4i');
end;

procedure glVertexAttribI4ui(ind:TGLuint; x:TGLuint; y:TGLuint; z:TGLuint; w:TGLuint);
begin
  WritelnLog('OpenGLES', 'glVertexAttribI4ui' + ' ind=' + IntToStr(Integer(ind)) + ' x=' + IntToStr(Integer(x)) + ' y=' + IntToStr(Integer(y)) + ' z=' + IntToStr(Integer(z)));
  glVertexAttribI4ui_Proc(ind, x, y, z, w);
  GLESCheckError('glVertexAttribI4ui');
end;

procedure glVertexAttribI4iv(ind:TGLuint; v:PGLint);
begin
  WritelnLog('OpenGLES', 'glVertexAttribI4iv' + ' ind=' + IntToStr(Integer(ind)));
  glVertexAttribI4iv_Proc(ind, v);
  GLESCheckError('glVertexAttribI4iv');
end;

procedure glVertexAttribI4uiv(ind:TGLuint; v:PGLuint);
begin
  WritelnLog('OpenGLES', 'glVertexAttribI4uiv' + ' ind=' + IntToStr(Integer(ind)));
  glVertexAttribI4uiv_Proc(ind, v);
  GLESCheckError('glVertexAttribI4uiv');
end;

procedure glGetUniformuiv(prog:TGLuint; location:TGLint; params:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGetUniformuiv' + ' prog=' + IntToStr(Integer(prog)) + ' location=' + IntToStr(location));
  glGetUniformuiv_Proc(prog, location, params);
  GLESCheckError('glGetUniformuiv');
end;

function glGetFragDataLocation(prog:TGLuint; name:PGLchar): TGLint;
begin
  WritelnLog('OpenGLES', 'glGetFragDataLocation' + ' prog=' + IntToStr(Integer(prog)));
  Result := glGetFragDataLocation_Proc(prog, name);
  GLESCheckError('glGetFragDataLocation');
end;

procedure glUniform1ui(location:TGLint; v0:TGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform1ui' + ' location=' + IntToStr(location) + ' v0=' + IntToStr(Integer(v0)));
  glUniform1ui_Proc(location, v0);
  GLESCheckError('glUniform1ui');
end;

procedure glUniform2ui(location:TGLint; v0:TGLuint; v1:TGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform2ui' + ' location=' + IntToStr(location) + ' v0=' + IntToStr(Integer(v0)) + ' v1=' + IntToStr(Integer(v1)));
  glUniform2ui_Proc(location, v0, v1);
  GLESCheckError('glUniform2ui');
end;

procedure glUniform3ui(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform3ui' + ' location=' + IntToStr(location) + ' v0=' + IntToStr(Integer(v0)) + ' v1=' + IntToStr(Integer(v1)) + ' v2=' + IntToStr(Integer(v2)));
  glUniform3ui_Proc(location, v0, v1, v2);
  GLESCheckError('glUniform3ui');
end;

procedure glUniform4ui(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint; v3:TGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform4ui' + ' location=' + IntToStr(location) + ' v0=' + IntToStr(Integer(v0)) + ' v1=' + IntToStr(Integer(v1)) + ' v2=' + IntToStr(Integer(v2)));
  glUniform4ui_Proc(location, v0, v1, v2, v3);
  GLESCheckError('glUniform4ui');
end;

procedure glUniform1uiv(location:TGLint; count:TGLsizei; value:PGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform1uiv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform1uiv_Proc(location, count, value);
  GLESCheckError('glUniform1uiv');
end;

procedure glUniform2uiv(location:TGLint; count:TGLsizei; value:PGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform2uiv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform2uiv_Proc(location, count, value);
  GLESCheckError('glUniform2uiv');
end;

procedure glUniform3uiv(location:TGLint; count:TGLsizei; value:PGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform3uiv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform3uiv_Proc(location, count, value);
  GLESCheckError('glUniform3uiv');
end;

procedure glUniform4uiv(location:TGLint; count:TGLsizei; value:PGLuint);
begin
  WritelnLog('OpenGLES', 'glUniform4uiv' + ' location=' + IntToStr(location) + ' count=' + IntToStr(count));
  glUniform4uiv_Proc(location, count, value);
  GLESCheckError('glUniform4uiv');
end;

procedure glClearBufferiv(buffer:TGLenum; drawbuffer:TGLint; value:PGLint);
begin
  WritelnLog('OpenGLES', 'glClearBufferiv' + ' buffer=' + GLEnumName(buffer) + ' drawbuffer=' + IntToStr(drawbuffer));
  glClearBufferiv_Proc(buffer, drawbuffer, value);
  GLESCheckError('glClearBufferiv');
end;

procedure glClearBufferuiv(buffer:TGLenum; drawbuffer:TGLint; value:PGLuint);
begin
  WritelnLog('OpenGLES', 'glClearBufferuiv' + ' buffer=' + GLEnumName(buffer) + ' drawbuffer=' + IntToStr(drawbuffer));
  glClearBufferuiv_Proc(buffer, drawbuffer, value);
  GLESCheckError('glClearBufferuiv');
end;

procedure glClearBufferfv(buffer:TGLenum; drawbuffer:TGLint; value:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glClearBufferfv' + ' buffer=' + GLEnumName(buffer) + ' drawbuffer=' + IntToStr(drawbuffer));
  glClearBufferfv_Proc(buffer, drawbuffer, value);
  GLESCheckError('glClearBufferfv');
end;

procedure glClearBufferfi(buffer:TGLenum; drawbuffer:TGLint; depth:TGLfloat; stencil:TGLint);
begin
  WritelnLog('OpenGLES', 'glClearBufferfi' + ' buffer=' + GLEnumName(buffer) + ' drawbuffer=' + IntToStr(drawbuffer) + ' stencil=' + IntToStr(stencil));
  glClearBufferfi_Proc(buffer, drawbuffer, depth, stencil);
  GLESCheckError('glClearBufferfi');
end;

function glGetStringi(name:TGLenum; ind:TGLuint): PGLubyte;
begin
  WritelnLog('OpenGLES', 'glGetStringi' + ' name=' + GLEnumName(name) + ' ind=' + IntToStr(Integer(ind)));
  Result := glGetStringi_Proc(name, ind);
  GLESCheckError('glGetStringi');
end;

procedure glCopyBufferSubData(readTarget:TGLenum; writeTarget:TGLenum; readOffset:TGLintptr; writeOffset:TGLintptr; size:TGLsizeiptr);
begin
  WritelnLog('OpenGLES', 'glCopyBufferSubData' + ' readTarget=' + GLEnumName(readTarget) + ' writeTarget=' + GLEnumName(writeTarget) + ' readOffset=' + IntToStr(readOffset) + ' writeOffset=' + IntToStr(writeOffset));
  glCopyBufferSubData_Proc(readTarget, writeTarget, readOffset, writeOffset, size);
  GLESCheckError('glCopyBufferSubData');
end;

procedure glGetUniformIndices(prog:TGLuint; uniformCount:TGLsizei; uniformNames:PPGLchar; uniformIndices:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGetUniformIndices' + ' prog=' + IntToStr(Integer(prog)) + ' uniformCount=' + IntToStr(uniformCount));
  glGetUniformIndices_Proc(prog, uniformCount, uniformNames, uniformIndices);
  GLESCheckError('glGetUniformIndices');
end;

procedure glGetActiveUniformsiv(prog:TGLuint; uniformCount:TGLsizei; uniformIndices:PGLuint; pname:TGLenum; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetActiveUniformsiv' + ' prog=' + IntToStr(Integer(prog)) + ' uniformCount=' + IntToStr(uniformCount) + ' pname=' + GLEnumName(pname));
  glGetActiveUniformsiv_Proc(prog, uniformCount, uniformIndices, pname, params);
  GLESCheckError('glGetActiveUniformsiv');
end;

function glGetUniformBlockIndex(prog:TGLuint; uniformBlockName:PGLchar): TGLuint;
begin
  WritelnLog('OpenGLES', 'glGetUniformBlockIndex' + ' prog=' + IntToStr(Integer(prog)));
  Result := glGetUniformBlockIndex_Proc(prog, uniformBlockName);
  GLESCheckError('glGetUniformBlockIndex');
end;

procedure glGetActiveUniformBlockiv(prog:TGLuint; uniformBlockIndex:TGLuint; pname:TGLenum; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetActiveUniformBlockiv' + ' prog=' + IntToStr(Integer(prog)) + ' uniformBlockIndex=' + IntToStr(Integer(uniformBlockIndex)) + ' pname=' + GLEnumName(pname));
  glGetActiveUniformBlockiv_Proc(prog, uniformBlockIndex, pname, params);
  GLESCheckError('glGetActiveUniformBlockiv');
end;

procedure glGetActiveUniformBlockName(prog:TGLuint; uniformBlockIndex:TGLuint; bufSize:TGLsizei; length:PGLsizei; uniformBlockName:PGLchar);
begin
  WritelnLog('OpenGLES', 'glGetActiveUniformBlockName' + ' prog=' + IntToStr(Integer(prog)) + ' uniformBlockIndex=' + IntToStr(Integer(uniformBlockIndex)) + ' bufSize=' + IntToStr(bufSize));
  glGetActiveUniformBlockName_Proc(prog, uniformBlockIndex, bufSize, length, uniformBlockName);
  GLESCheckError('glGetActiveUniformBlockName');
end;

procedure glUniformBlockBinding(prog:TGLuint; uniformBlockIndex:TGLuint; uniformBlockBinding:TGLuint);
begin
  WritelnLog('OpenGLES', 'glUniformBlockBinding' + ' prog=' + IntToStr(Integer(prog)) + ' uniformBlockIndex=' + IntToStr(Integer(uniformBlockIndex)) + ' uniformBlockBinding=' + IntToStr(Integer(uniformBlockBinding)));
  glUniformBlockBinding_Proc(prog, uniformBlockIndex, uniformBlockBinding);
  GLESCheckError('glUniformBlockBinding');
end;

procedure glDrawArraysInstanced(Mode: GLuint; First, Count, InstanceCount: GLsizei);
begin
  WritelnLog('OpenGLES', 'glDrawArraysInstanced' + ' Mode=' + IntToStr(Integer(Mode)) + ' First=' + IntToStr(First) + ' Count=' + IntToStr(Count) + ' InstanceCount=' + IntToStr(InstanceCount));
  glDrawArraysInstanced_Proc(Mode, First, Count, InstanceCount);
  GLESCheckError('glDrawArraysInstanced');
end;

procedure glDrawElementsInstanced(Mode: GLuint; Count, Kind: GLsizei; Indices: PGLuint; InstanceCount: GLsizei);
begin
  WritelnLog('OpenGLES', 'glDrawElementsInstanced' + ' Mode=' + IntToStr(Integer(Mode)) + ' Count=' + IntToStr(Count) + ' Kind=' + IntToStr(Kind));
  glDrawElementsInstanced_Proc(Mode, Count, Kind, Indices, InstanceCount);
  GLESCheckError('glDrawElementsInstanced');
end;

function glFenceSync(condition:TGLenum; flags:TGLbitfield): TGLsync;
begin
  WritelnLog('OpenGLES', 'glFenceSync' + ' condition=' + GLEnumName(condition) + ' flags=' + IntToStr(Integer(flags)));
  Result := glFenceSync_Proc(condition, flags);
  GLESCheckError('glFenceSync');
end;

function glIsSync(sync:TGLsync): TGLboolean;
begin
  WritelnLog('OpenGLES', 'glIsSync');
  Result := glIsSync_Proc(sync);
  GLESCheckError('glIsSync');
end;

procedure glDeleteSync(sync:TGLsync);
begin
  WritelnLog('OpenGLES', 'glDeleteSync');
  glDeleteSync_Proc(sync);
  GLESCheckError('glDeleteSync');
end;

function glClientWaitSync(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64): TGLenum;
begin
  WritelnLog('OpenGLES', 'glClientWaitSync' + ' flags=' + IntToStr(Integer(flags)) + ' timeout=' + IntToStr(Int64(timeout)));
  Result := glClientWaitSync_Proc(sync, flags, timeout);
  GLESCheckError('glClientWaitSync');
end;

procedure glWaitSync(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64);
begin
  WritelnLog('OpenGLES', 'glWaitSync' + ' flags=' + IntToStr(Integer(flags)) + ' timeout=' + IntToStr(Int64(timeout)));
  glWaitSync_Proc(sync, flags, timeout);
  GLESCheckError('glWaitSync');
end;

procedure glGetInteger64v(pname:TGLenum; data:PGLint64);
begin
  WritelnLog('OpenGLES', 'glGetInteger64v' + ' pname=' + GLEnumName(pname));
  glGetInteger64v_Proc(pname, data);
  GLESCheckError('glGetInteger64v');
end;

procedure glGetSynciv(sync:TGLsync; pname:TGLenum; count:TGLsizei; length:PGLsizei; values:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetSynciv' + ' pname=' + GLEnumName(pname) + ' count=' + IntToStr(count));
  glGetSynciv_Proc(sync, pname, count, length, values);
  GLESCheckError('glGetSynciv');
end;

procedure glGetInteger64i_v(target:TGLenum; ind:TGLuint; data:PGLint64);
begin
  WritelnLog('OpenGLES', 'glGetInteger64i_v' + ' target=' + GLEnumName(target) + ' ind=' + IntToStr(Integer(ind)));
  glGetInteger64i_v_Proc(target, ind, data);
  GLESCheckError('glGetInteger64i_v');
end;

procedure glGetBufferParameteri64v(target:TGLenum; pname:TGLenum; params:PGLint64);
begin
  WritelnLog('OpenGLES', 'glGetBufferParameteri64v' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetBufferParameteri64v_Proc(target, pname, params);
  GLESCheckError('glGetBufferParameteri64v');
end;

procedure glGenSamplers(count:TGLsizei; samplers:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGenSamplers' + ' count=' + IntToStr(count));
  glGenSamplers_Proc(count, samplers);
  GLESCheckError('glGenSamplers');
end;

procedure glDeleteSamplers(count:TGLsizei; samplers:PGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteSamplers' + ' count=' + IntToStr(count));
  glDeleteSamplers_Proc(count, samplers);
  GLESCheckError('glDeleteSamplers');
end;

function glIsSampler(sampler:TGLuint): TGLboolean;
begin
  WritelnLog('OpenGLES', 'glIsSampler' + ' sampler=' + IntToStr(Integer(sampler)));
  Result := glIsSampler_Proc(sampler);
  GLESCheckError('glIsSampler');
end;

procedure glBindSampler(u:TGLuint; sampler:TGLuint);
begin
  WritelnLog('OpenGLES', 'glBindSampler' + ' u=' + IntToStr(Integer(u)) + ' sampler=' + IntToStr(Integer(sampler)));
  glBindSampler_Proc(u, sampler);
  GLESCheckError('glBindSampler');
end;

procedure glSamplerParameteri(sampler:TGLuint; pname:TGLenum; param:TGLint);
begin
  WritelnLog('OpenGLES', 'glSamplerParameteri' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname) + ' param=' + IntToStr(param));
  glSamplerParameteri_Proc(sampler, pname, param);
  GLESCheckError('glSamplerParameteri');
end;

procedure glSamplerParameteriv(sampler:TGLuint; pname:TGLenum; param:PGLint);
begin
  WritelnLog('OpenGLES', 'glSamplerParameteriv' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname));
  glSamplerParameteriv_Proc(sampler, pname, param);
  GLESCheckError('glSamplerParameteriv');
end;

procedure glSamplerParameterf(sampler:TGLuint; pname:TGLenum; param:TGLfloat);
begin
  WritelnLog('OpenGLES', 'glSamplerParameterf' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname));
  glSamplerParameterf_Proc(sampler, pname, param);
  GLESCheckError('glSamplerParameterf');
end;

procedure glSamplerParameterfv(sampler:TGLuint; pname:TGLenum; param:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glSamplerParameterfv' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname));
  glSamplerParameterfv_Proc(sampler, pname, param);
  GLESCheckError('glSamplerParameterfv');
end;

procedure glGetSamplerParameteriv(sampler:TGLuint; pname:TGLenum; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetSamplerParameteriv' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname));
  glGetSamplerParameteriv_Proc(sampler, pname, params);
  GLESCheckError('glGetSamplerParameteriv');
end;

procedure glGetSamplerParameterfv(sampler:TGLuint; pname:TGLenum; params:PGLfloat);
begin
  WritelnLog('OpenGLES', 'glGetSamplerParameterfv' + ' sampler=' + IntToStr(Integer(sampler)) + ' pname=' + GLEnumName(pname));
  glGetSamplerParameterfv_Proc(sampler, pname, params);
  GLESCheckError('glGetSamplerParameterfv');
end;

procedure glVertexAttribDivisor(Ind, Divisor: GLuint);
begin
  WritelnLog('OpenGLES', 'glVertexAttribDivisor' + ' Ind=' + IntToStr(Integer(Ind)) + ' Divisor=' + IntToStr(Integer(Divisor)));
  glVertexAttribDivisor_Proc(Ind, Divisor);
  GLESCheckError('glVertexAttribDivisor');
end;

procedure glBindTransformFeedback(target:TGLenum; id:TGLuint);
begin
  WritelnLog('OpenGLES', 'glBindTransformFeedback' + ' target=' + GLEnumName(target) + ' id=' + IntToStr(Integer(id)));
  glBindTransformFeedback_Proc(target, id);
  GLESCheckError('glBindTransformFeedback');
end;

procedure glDeleteTransformFeedbacks(n:TGLsizei; ids:PGLuint);
begin
  WritelnLog('OpenGLES', 'glDeleteTransformFeedbacks' + ' n=' + IntToStr(n));
  glDeleteTransformFeedbacks_Proc(n, ids);
  GLESCheckError('glDeleteTransformFeedbacks');
end;

procedure glGenTransformFeedbacks(n:TGLsizei; ids:PGLuint);
begin
  WritelnLog('OpenGLES', 'glGenTransformFeedbacks' + ' n=' + IntToStr(n));
  glGenTransformFeedbacks_Proc(n, ids);
  GLESCheckError('glGenTransformFeedbacks');
end;

function glIsTransformFeedback(id:TGLuint): TGLboolean;
begin
  WritelnLog('OpenGLES', 'glIsTransformFeedback' + ' id=' + IntToStr(Integer(id)));
  Result := glIsTransformFeedback_Proc(id);
  GLESCheckError('glIsTransformFeedback');
end;

procedure glPauseTransformFeedback;
begin
  WritelnLog('OpenGLES', 'glPauseTransformFeedback');
  glPauseTransformFeedback_Proc;
  GLESCheckError('glPauseTransformFeedback');
end;

procedure glResumeTransformFeedback;
begin
  WritelnLog('OpenGLES', 'glResumeTransformFeedback');
  glResumeTransformFeedback_Proc;
  GLESCheckError('glResumeTransformFeedback');
end;

procedure glGetProgramBinary(prog:TGLuint; bufSize:TGLsizei; length:PGLsizei; binaryFormat:PGLenum; binary:pointer);
begin
  WritelnLog('OpenGLES', 'glGetProgramBinary' + ' prog=' + IntToStr(Integer(prog)) + ' bufSize=' + IntToStr(bufSize));
  glGetProgramBinary_Proc(prog, bufSize, length, binaryFormat, binary);
  GLESCheckError('glGetProgramBinary');
end;

procedure glProgramBinary(prog:TGLuint; binaryFormat:TGLenum; binary:pointer; length:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glProgramBinary' + ' prog=' + IntToStr(Integer(prog)) + ' binaryFormat=' + GLEnumName(binaryFormat) + ' length=' + IntToStr(length));
  glProgramBinary_Proc(prog, binaryFormat, binary, length);
  GLESCheckError('glProgramBinary');
end;

procedure glProgramParameteri(prog:TGLuint; pname:TGLenum; value:TGLint);
begin
  WritelnLog('OpenGLES', 'glProgramParameteri' + ' prog=' + IntToStr(Integer(prog)) + ' pname=' + GLEnumName(pname) + ' value=' + IntToStr(value));
  glProgramParameteri_Proc(prog, pname, value);
  GLESCheckError('glProgramParameteri');
end;

procedure glInvalidateFramebuffer(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum);
begin
  WritelnLog('OpenGLES', 'glInvalidateFramebuffer' + ' target=' + GLEnumName(target) + ' numAttachments=' + IntToStr(numAttachments));
  glInvalidateFramebuffer_Proc(target, numAttachments, attachments);
  GLESCheckError('glInvalidateFramebuffer');
end;

procedure glInvalidateSubFramebuffer(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum; x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glInvalidateSubFramebuffer' + ' target=' + GLEnumName(target) + ' numAttachments=' + IntToStr(numAttachments) + ' x=' + IntToStr(x));
  glInvalidateSubFramebuffer_Proc(target, numAttachments, attachments, x, y, width, height);
  GLESCheckError('glInvalidateSubFramebuffer');
end;

procedure glTexStorage2D(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glTexStorage2D' + ' target=' + GLEnumName(target) + ' levels=' + IntToStr(levels) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glTexStorage2D_Proc(target, levels, internalformat, width, height);
  GLESCheckError('glTexStorage2D');
end;

procedure glTexStorage3D(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei; depth:TGLsizei);
begin
  WritelnLog('OpenGLES', 'glTexStorage3D' + ' target=' + GLEnumName(target) + ' levels=' + IntToStr(levels) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glTexStorage3D_Proc(target, levels, internalformat, width, height, depth);
  GLESCheckError('glTexStorage3D');
end;

procedure glGetInternalformativ(target:TGLenum; internalformat:TGLenum; pname:TGLenum; count:TGLsizei; params:PGLint);
begin
  WritelnLog('OpenGLES', 'glGetInternalformativ' + ' target=' + GLEnumName(target) + ' internalformat=' + GLEnumName(internalformat) + ' pname=' + GLEnumName(pname) + ' count=' + IntToStr(count));
  glGetInternalformativ_Proc(target, internalformat, pname, count, params);
  GLESCheckError('glGetInternalformativ');
end;

procedure glEGLImageTargetTexture2DOES(target:GLenum; image:GLeglImageOES);
begin
  WritelnLog('OpenGLES', 'glEGLImageTargetTexture2DOES' + ' target=' + GLEnumName(target));
  glEGLImageTargetTexture2DOES_Proc(target, image);
  GLESCheckError('glEGLImageTargetTexture2DOES');
end;

procedure glEGLImageTargetRenderbufferStorageOES(target:GLenum; image:GLeglImageOES);
begin
  WritelnLog('OpenGLES', 'glEGLImageTargetRenderbufferStorageOES' + ' target=' + GLEnumName(target));
  glEGLImageTargetRenderbufferStorageOES_Proc(target, image);
  GLESCheckError('glEGLImageTargetRenderbufferStorageOES');
end;

procedure glGetProgramBinaryOES(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);
begin
  WritelnLog('OpenGLES', 'glGetProgramBinaryOES' + ' _program=' + IntToStr(Integer(_program)) + ' bufSize=' + IntToStr(bufSize));
  glGetProgramBinaryOES_Proc(_program, bufSize, length, binaryFormat, binary);
  GLESCheckError('glGetProgramBinaryOES');
end;

procedure glProgramBinaryOES(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);
begin
  WritelnLog('OpenGLES', 'glProgramBinaryOES' + ' _program=' + IntToStr(Integer(_program)) + ' binaryFormat=' + GLEnumName(binaryFormat) + ' length=' + IntToStr(length));
  glProgramBinaryOES_Proc(_program, binaryFormat, binary, length);
  GLESCheckError('glProgramBinaryOES');
end;

function glMapBufferOES(target:GLenum; access:GLenum): pointer;
begin
  WritelnLog('OpenGLES', 'glMapBufferOES' + ' target=' + GLEnumName(target) + ' access=' + GLEnumName(access));
  Result := glMapBufferOES_Proc(target, access);
  GLESCheckError('glMapBufferOES');
end;

function glUnmapBufferOES(target:GLenum): GLboolean;
begin
  WritelnLog('OpenGLES', 'glUnmapBufferOES' + ' target=' + GLEnumName(target));
  Result := glUnmapBufferOES_Proc(target);
  GLESCheckError('glUnmapBufferOES');
end;

procedure glGetBufferPointervOES(target:GLenum; pname:GLenum; params:Ppointer);
begin
  WritelnLog('OpenGLES', 'glGetBufferPointervOES' + ' target=' + GLEnumName(target) + ' pname=' + GLEnumName(pname));
  glGetBufferPointervOES_Proc(target, pname, params);
  GLESCheckError('glGetBufferPointervOES');
end;

procedure glTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexImage3DOES' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glTexImage3DOES_Proc(target, level, internalformat, width, height, depth, border, format, _type, pixels);
  GLESCheckError('glTexImage3DOES');
end;

procedure glTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);
begin
  WritelnLog('OpenGLES', 'glTexSubImage3DOES' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glTexSubImage3DOES_Proc(target, level, xoffset, yoffset, zoffset, width, height, depth, format, _type, pixels);
  GLESCheckError('glTexSubImage3DOES');
end;

procedure glCopyTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; x:GLint; y:GLint; width:GLsizei; height:GLsizei);
begin
  WritelnLog('OpenGLES', 'glCopyTexSubImage3DOES' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCopyTexSubImage3DOES_Proc(target, level, xoffset, yoffset, zoffset, x, y, width, height);
  GLESCheckError('glCopyTexSubImage3DOES');
end;

procedure glCompressedTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexImage3DOES' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' internalformat=' + GLEnumName(internalformat) + ' width=' + IntToStr(width));
  glCompressedTexImage3DOES_Proc(target, level, internalformat, width, height, depth, border, imageSize, data);
  GLESCheckError('glCompressedTexImage3DOES');
end;

procedure glCompressedTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint; width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);
begin
  WritelnLog('OpenGLES', 'glCompressedTexSubImage3DOES' + ' target=' + GLEnumName(target) + ' level=' + IntToStr(level) + ' xoffset=' + IntToStr(xoffset) + ' yoffset=' + IntToStr(yoffset));
  glCompressedTexSubImage3DOES_Proc(target, level, xoffset, yoffset, zoffset, width, height, depth, format, imageSize, data);
  GLESCheckError('glCompressedTexSubImage3DOES');
end;

procedure glFramebufferTexture3DOES(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint; zoffset:GLint);
begin
  WritelnLog('OpenGLES', 'glFramebufferTexture3DOES' + ' target=' + GLEnumName(target) + ' attachment=' + GLEnumName(attachment) + ' textarget=' + GLEnumName(textarget) + ' texture=' + IntToStr(Integer(texture)));
  glFramebufferTexture3DOES_Proc(target, attachment, textarget, texture, level, zoffset);
  GLESCheckError('glFramebufferTexture3DOES');
end;

procedure glGetPerfMonitorGroupsAMD(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorGroupsAMD' + ' groupsSize=' + IntToStr(groupsSize));
  glGetPerfMonitorGroupsAMD_Proc(numGroups, groupsSize, groups);
  GLESCheckError('glGetPerfMonitorGroupsAMD');
end;

procedure glGetPerfMonitorCountersAMD(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorCountersAMD' + ' group=' + IntToStr(Integer(group)) + ' counterSize=' + IntToStr(counterSize));
  glGetPerfMonitorCountersAMD_Proc(group, numCounters, maxActiveCounters, counterSize, counters);
  GLESCheckError('glGetPerfMonitorCountersAMD');
end;

procedure glGetPerfMonitorGroupStringAMD(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorGroupStringAMD' + ' group=' + IntToStr(Integer(group)) + ' bufSize=' + IntToStr(bufSize));
  glGetPerfMonitorGroupStringAMD_Proc(group, bufSize, length, groupString);
  GLESCheckError('glGetPerfMonitorGroupStringAMD');
end;

procedure glGetPerfMonitorCounterStringAMD(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:PAnsiChar);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorCounterStringAMD' + ' group=' + IntToStr(Integer(group)) + ' counter=' + IntToStr(Integer(counter)) + ' bufSize=' + IntToStr(bufSize));
  glGetPerfMonitorCounterStringAMD_Proc(group, counter, bufSize, length, counterString);
  GLESCheckError('glGetPerfMonitorCounterStringAMD');
end;

procedure glGetPerfMonitorCounterInfoAMD(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorCounterInfoAMD' + ' group=' + IntToStr(Integer(group)) + ' counter=' + IntToStr(Integer(counter)) + ' pname=' + GLEnumName(pname));
  glGetPerfMonitorCounterInfoAMD_Proc(group, counter, pname, data);
  GLESCheckError('glGetPerfMonitorCounterInfoAMD');
end;

procedure glGenPerfMonitorsAMD(n:GLsizei; monitors:pGLuint);
begin
  WritelnLog('OpenGLES', 'glGenPerfMonitorsAMD' + ' n=' + IntToStr(n));
  glGenPerfMonitorsAMD_Proc(n, monitors);
  GLESCheckError('glGenPerfMonitorsAMD');
end;

procedure glDeletePerfMonitorsAMD(n:GLsizei; monitors:pGLuint);
begin
  WritelnLog('OpenGLES', 'glDeletePerfMonitorsAMD' + ' n=' + IntToStr(n));
  glDeletePerfMonitorsAMD_Proc(n, monitors);
  GLESCheckError('glDeletePerfMonitorsAMD');
end;

procedure glSelectPerfMonitorCountersAMD(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);
begin
  WritelnLog('OpenGLES', 'glSelectPerfMonitorCountersAMD' + ' monitor=' + IntToStr(Integer(monitor)) + ' group=' + IntToStr(Integer(group)) + ' numCounters=' + IntToStr(numCounters));
  glSelectPerfMonitorCountersAMD_Proc(monitor, enable, group, numCounters, countersList);
  GLESCheckError('glSelectPerfMonitorCountersAMD');
end;

procedure glBeginPerfMonitorAMD(monitor:GLuint);
begin
  WritelnLog('OpenGLES', 'glBeginPerfMonitorAMD' + ' monitor=' + IntToStr(Integer(monitor)));
  glBeginPerfMonitorAMD_Proc(monitor);
  GLESCheckError('glBeginPerfMonitorAMD');
end;

procedure glEndPerfMonitorAMD(monitor:GLuint);
begin
  WritelnLog('OpenGLES', 'glEndPerfMonitorAMD' + ' monitor=' + IntToStr(Integer(monitor)));
  glEndPerfMonitorAMD_Proc(monitor);
  GLESCheckError('glEndPerfMonitorAMD');
end;

procedure glGetPerfMonitorCounterDataAMD(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);
begin
  WritelnLog('OpenGLES', 'glGetPerfMonitorCounterDataAMD' + ' monitor=' + IntToStr(Integer(monitor)) + ' pname=' + GLEnumName(pname) + ' dataSize=' + IntToStr(dataSize));
  glGetPerfMonitorCounterDataAMD_Proc(monitor, pname, dataSize, data, bytesWritten);
  GLESCheckError('glGetPerfMonitorCounterDataAMD');
end;

procedure glDebugMessageCallback(callback:GLDEBUGPROC; userParam:Pointer);
begin
  WritelnLog('OpenGLES', 'glDebugMessageCallback');
  glDebugMessageCallback_Proc(callback, userParam);
  GLESCheckError('glDebugMessageCallback');
end;

procedure glDebugMessageControl(source:GLenum; _type:GLenum; severity:GLenum; count:GLsizei; ids:PGLuint; enabled:GLboolean);
begin
  WritelnLog('OpenGLES', 'glDebugMessageControl' + ' source=' + GLEnumName(source) + ' _type=' + GLEnumName(_type) + ' severity=' + GLEnumName(severity) + ' count=' + IntToStr(count));
  glDebugMessageControl_Proc(source, _type, severity, count, ids, enabled);
  GLESCheckError('glDebugMessageControl');
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  GLESInitialization;
  {$endif}
finalization
  FreeGLES;
end.
