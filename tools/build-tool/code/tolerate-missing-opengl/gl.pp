(*
** License Applicability. Except to the extent portions of this file are
** made subject to an alternative license as permitted in the SGI Free
** Software License B, Version 1.1 (the "License"), the contents of this
** file are subject only to the provisions of the License. You may not use
** this file except in compliance with the License. You may obtain a copy
** of the License at Silicon Graphics, Inc., attn: Legal Services, 1600
** Amphitheatre Parkway, Mountain View, CA 94043-1351, or at:
** 
** http://oss.sgi.com/projects/FreeB
** 
** Note that, as provided in the License, the Software is distributed on an
** "AS IS" basis, with ALL EXPRESS AND IMPLIED WARRANTIES AND CONDITIONS
** DISCLAIMED, INCLUDING, WITHOUT LIMITATION, ANY IMPLIED WARRANTIES AND
** CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A
** PARTICULAR PURPOSE, AND NON-INFRINGEMENT.
** 
** Original Code. The Original Code is: OpenGL Sample Implementation,
** Version 1.2.1, released January 26, 2000, developed by Silicon Graphics,
** Inc. The Original Code is Copyright (c) 1991-2000 Silicon Graphics, Inc.
** Copyright in any portions created by third parties is as indicated
** elsewhere herein. All Rights Reserved.
** 
** Additional Notice Provisions: This software was created using the
** OpenGL(R) version 1.2.1 Sample Implementation published by SGI, but has
** not been independently verified as being compliant with the OpenGL(R)
** version 1.2.1 Specification.
**
** (this unit actually only contains the 1.1 parts of the specification,
**  the parts from the subsequence versions are in glext.pp)
*)

{******************************************************************************}
{ Converted to Delphi by Tom Nuydens (tom@delphi3d.net)                        }
{******************************************************************************}

{$MODE Delphi}
{$MACRO ON}
{$IFDEF Windows}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
  {$IFDEF MorphOS}
    {$INLINE ON}
    {$DEFINE GL_UNIT}
  {$ELSE}
   {$IFNDEF OS2}
    {$LINKLIB c}
   {$ENDIF OS2}
  {$ENDIF}
{$ENDIF}

unit GL;

interface

uses
  SysUtils,
  {$IFDEF Windows}
  Windows, dynlibs
  {$ELSE Windows}
  {$IFDEF MorphOS}
  TinyGL
  {$ELSE MorphOS}
  dynlibs
  {$ENDIF MorphOS}
  {$ENDIF Windows};

{$IFNDEF MORPHOS}
var
  LibGL: TLibHandle;
{$ENDIF MORPHOS}

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;
                              PPGLvoid    = ^PGLvoid;

  TGLenum     = GLenum;
  TGLboolean  = GLboolean;
  TGLbitfield = GLbitfield;
  TGLbyte     = GLbyte;
  TGLshort    = GLshort;
  TGLint      = GLint;
  TGLsizei    = GLsizei;
  TGLubyte    = GLubyte;
  TGLushort   = GLushort;
  TGLuint     = GLuint;
  TGLfloat    = GLfloat;
  TGLclampf   = GLclampf;
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;

{******************************************************************************}

const
  // Version
  GL_VERSION_1_1                    = 1;

  // AccumOp
  GL_ACCUM                          = $0100;
  GL_LOAD                           = $0101;
  GL_RETURN                         = $0102;
  GL_MULT                           = $0103;
  GL_ADD                            = $0104;

  // AlphaFunction
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // AttribMask
  GL_CURRENT_BIT                    = $00000001;
  GL_POINT_BIT                      = $00000002;
  GL_LINE_BIT                       = $00000004;
  GL_POLYGON_BIT                    = $00000008;
  GL_POLYGON_STIPPLE_BIT            = $00000010;
  GL_PIXEL_MODE_BIT                 = $00000020;
  GL_LIGHTING_BIT                   = $00000040;
  GL_FOG_BIT                        = $00000080;
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_ACCUM_BUFFER_BIT               = $00000200;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_VIEWPORT_BIT                   = $00000800;
  GL_TRANSFORM_BIT                  = $00001000;
  GL_ENABLE_BIT                     = $00002000;
  GL_COLOR_BUFFER_BIT               = $00004000;
  GL_HINT_BIT                       = $00008000;
  GL_EVAL_BIT                       = $00010000;
  GL_LIST_BIT                       = $00020000;
  GL_TEXTURE_BIT                    = $00040000;
  GL_SCISSOR_BIT                    = $00080000;
  GL_ALL_ATTRIB_BITS                = $000FFFFF;

  // BeginMode
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;
  GL_QUAD_STRIP                     = $0008;
  GL_POLYGON                        = $0009;

  // BlendingFactorDest
  GL_ZERO                           = 0;
  GL_ONE                            = 1;
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;

  // BlendingFactorSrc
  //      GL_ZERO
  //      GL_ONE
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;
  //      GL_SRC_ALPHA
  //      GL_ONE_MINUS_SRC_ALPHA
  //      GL_DST_ALPHA
  //      GL_ONE_MINUS_DST_ALPHA

  // Boolean
  GL_TRUE                           = 1;
  GL_FALSE                          = 0;

  // ClearBufferMask
  //      GL_COLOR_BUFFER_BIT
  //      GL_ACCUM_BUFFER_BIT
  //      GL_STENCIL_BUFFER_BIT
  //      GL_DEPTH_BUFFER_BIT

  // ClientArrayType
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY

  // ClipPlaneName
  GL_CLIP_PLANE0                    = $3000;
  GL_CLIP_PLANE1                    = $3001;
  GL_CLIP_PLANE2                    = $3002;
  GL_CLIP_PLANE3                    = $3003;
  GL_CLIP_PLANE4                    = $3004;
  GL_CLIP_PLANE5                    = $3005;

  // ColorMaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // ColorMaterialParameter
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR
  //      GL_EMISSION
  //      GL_AMBIENT_AND_DIFFUSE

  // ColorPointerType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // CullFaceMode
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_2_BYTES                        = $1407;
  GL_3_BYTES                        = $1408;
  GL_4_BYTES                        = $1409;
  GL_DOUBLE                         = $140A;

  // DepthFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // DrawBufferMode
  GL_NONE                           = 0;
  GL_FRONT_LEFT                     = $0400;
  GL_FRONT_RIGHT                    = $0401;
  GL_BACK_LEFT                      = $0402;
  GL_BACK_RIGHT                     = $0403;
  GL_FRONT                          = $0404;
  GL_BACK                           = $0405;
  GL_LEFT                           = $0406;
  GL_RIGHT                          = $0407;
  GL_FRONT_AND_BACK                 = $0408;
  GL_AUX0                           = $0409;
  GL_AUX1                           = $040A;
  GL_AUX2                           = $040B;
  GL_AUX3                           = $040C;

  // Enable
  //      GL_FOG
  //      GL_LIGHTING
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_LINE_STIPPLE
  //      GL_POLYGON_STIPPLE
  //      GL_CULL_FACE
  //      GL_ALPHA_TEST
  //      GL_BLEND
  //      GL_INDEX_LOGIC_OP
  //      GL_COLOR_LOGIC_OP
  //      GL_DITHER
  //      GL_STENCIL_TEST
  //      GL_DEPTH_TEST
  //      GL_CLIP_PLANE0
  //      GL_CLIP_PLANE1
  //      GL_CLIP_PLANE2
  //      GL_CLIP_PLANE3
  //      GL_CLIP_PLANE4
  //      GL_CLIP_PLANE5
  //      GL_LIGHT0
  //      GL_LIGHT1
  //      GL_LIGHT2
  //      GL_LIGHT3
  //      GL_LIGHT4
  //      GL_LIGHT5
  //      GL_LIGHT6
  //      GL_LIGHT7
  //      GL_TEXTURE_GEN_S
  //      GL_TEXTURE_GEN_T
  //      GL_TEXTURE_GEN_R
  //      GL_TEXTURE_GEN_Q
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_POINT_SMOOTH
  //      GL_LINE_SMOOTH
  //      GL_POLYGON_SMOOTH
  //      GL_SCISSOR_TEST
  //      GL_COLOR_MATERIAL
  //      GL_NORMALIZE
  //      GL_AUTO_NORMAL
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_POLYGON_OFFSET_POINT
  //      GL_POLYGON_OFFSET_LINE
  //      GL_POLYGON_OFFSET_FILL

  // ErrorCode
  GL_NO_ERROR                       = 0;
  GL_INVALID_ENUM                   = $0500;
  GL_INVALID_VALUE                  = $0501;
  GL_INVALID_OPERATION              = $0502;
  GL_STACK_OVERFLOW                 = $0503;
  GL_STACK_UNDERFLOW                = $0504;
  GL_OUT_OF_MEMORY                  = $0505;

  // FeedBackMode
  GL_2D                             = $0600;
  GL_3D                             = $0601;
  GL_3D_COLOR                       = $0602;
  GL_3D_COLOR_TEXTURE               = $0603;
  GL_4D_COLOR_TEXTURE               = $0604;

  // FeedBackToken
  GL_PASS_THROUGH_TOKEN             = $0700;
  GL_POINT_TOKEN                    = $0701;
  GL_LINE_TOKEN                     = $0702;
  GL_POLYGON_TOKEN                  = $0703;
  GL_BITMAP_TOKEN                   = $0704;
  GL_DRAW_PIXEL_TOKEN               = $0705;
  GL_COPY_PIXEL_TOKEN               = $0706;
  GL_LINE_RESET_TOKEN               = $0707;

  // FogMode
  //      GL_LINEAR
  GL_EXP                            = $0800;
  GL_EXP2                           = $0801;

  // FogParameter
  //      GL_FOG_COLOR
  //      GL_FOG_DENSITY
  //      GL_FOG_END
  //      GL_FOG_INDEX
  //      GL_FOG_MODE
  //      GL_FOG_START

  // FrontFaceDirection
  GL_CW                             = $0900;
  GL_CCW                            = $0901;

  // GetMapTarget
  GL_COEFF                          = $0A00;
  GL_ORDER                          = $0A01;
  GL_DOMAIN                         = $0A02;

  // GetPixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // GetPointerTarget
  //      GL_VERTEX_ARRAY_POINTER
  //      GL_NORMAL_ARRAY_POINTER
  //      GL_COLOR_ARRAY_POINTER
  //      GL_INDEX_ARRAY_POINTER
  //      GL_TEXTURE_COORD_ARRAY_POINTER
  //      GL_EDGE_FLAG_ARRAY_POINTER

  // GetTarget
  GL_CURRENT_COLOR                  = $0B00;
  GL_CURRENT_INDEX                  = $0B01;
  GL_CURRENT_NORMAL                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS         = $0B03;
  GL_CURRENT_RASTER_COLOR           = $0B04;
  GL_CURRENT_RASTER_INDEX           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS  = $0B06;
  GL_CURRENT_RASTER_POSITION        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID  = $0B08;
  GL_CURRENT_RASTER_DISTANCE        = $0B09;
  GL_POINT_SMOOTH                   = $0B10;
  GL_POINT_SIZE                     = $0B11;
  GL_POINT_SIZE_RANGE               = $0B12;
  GL_POINT_SIZE_GRANULARITY         = $0B13;
  GL_LINE_SMOOTH                    = $0B20;
  GL_LINE_WIDTH                     = $0B21;
  GL_LINE_WIDTH_RANGE               = $0B22;
  GL_LINE_WIDTH_GRANULARITY         = $0B23;
  GL_LINE_STIPPLE                   = $0B24;
  GL_LINE_STIPPLE_PATTERN           = $0B25;
  GL_LINE_STIPPLE_REPEAT            = $0B26;
  GL_LIST_MODE                      = $0B30;
  GL_MAX_LIST_NESTING               = $0B31;
  GL_LIST_BASE                      = $0B32;
  GL_LIST_INDEX                     = $0B33;
  GL_POLYGON_MODE                   = $0B40;
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_POLYGON_STIPPLE                = $0B42;
  GL_EDGE_FLAG                      = $0B43;
  GL_CULL_FACE                      = $0B44;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_LIGHTING                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE           = $0B52;
  GL_LIGHT_MODEL_AMBIENT            = $0B53;
  GL_SHADE_MODEL                    = $0B54;
  GL_COLOR_MATERIAL_FACE            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER       = $0B56;
  GL_COLOR_MATERIAL                 = $0B57;
  GL_FOG                            = $0B60;
  GL_FOG_INDEX                      = $0B61;
  GL_FOG_DENSITY                    = $0B62;
  GL_FOG_START                      = $0B63;
  GL_FOG_END                        = $0B64;
  GL_FOG_MODE                       = $0B65;
  GL_FOG_COLOR                      = $0B66;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_TEST                     = $0B71;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_ACCUM_CLEAR_VALUE              = $0B80;
  GL_STENCIL_TEST                   = $0B90;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_MATRIX_MODE                    = $0BA0;
  GL_NORMALIZE                      = $0BA1;
  GL_VIEWPORT                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH          = $0BA3;
  GL_PROJECTION_STACK_DEPTH         = $0BA4;
  GL_TEXTURE_STACK_DEPTH            = $0BA5;
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;
  GL_TEXTURE_MATRIX                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH      = $0BB1;
  GL_ALPHA_TEST                     = $0BC0;
  GL_ALPHA_TEST_FUNC                = $0BC1;
  GL_ALPHA_TEST_REF                 = $0BC2;
  GL_DITHER                         = $0BD0;
  GL_BLEND_DST                      = $0BE0;
  GL_BLEND_SRC                      = $0BE1;
  GL_BLEND                          = $0BE2;
  GL_LOGIC_OP_MODE                  = $0BF0;
  GL_INDEX_LOGIC_OP                 = $0BF1;
  GL_COLOR_LOGIC_OP                 = $0BF2;
  GL_AUX_BUFFERS                    = $0C00;
  GL_DRAW_BUFFER                    = $0C01;
  GL_READ_BUFFER                    = $0C02;
  GL_SCISSOR_BOX                    = $0C10;
  GL_SCISSOR_TEST                   = $0C11;
  GL_INDEX_CLEAR_VALUE              = $0C20;
  GL_INDEX_WRITEMASK                = $0C21;
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_INDEX_MODE                     = $0C30;
  GL_RGBA_MODE                      = $0C31;
  GL_DOUBLEBUFFER                   = $0C32;
  GL_STEREO                         = $0C33;
  GL_RENDER_MODE                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_POINT_SMOOTH_HINT              = $0C51;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
  GL_FOG_HINT                       = $0C54;
  GL_TEXTURE_GEN_S                  = $0C60;
  GL_TEXTURE_GEN_T                  = $0C61;
  GL_TEXTURE_GEN_R                  = $0C62;
  GL_TEXTURE_GEN_Q                  = $0C63;
  GL_PIXEL_MAP_I_TO_I               = $0C70;
  GL_PIXEL_MAP_S_TO_S               = $0C71;
  GL_PIXEL_MAP_I_TO_R               = $0C72;
  GL_PIXEL_MAP_I_TO_G               = $0C73;
  GL_PIXEL_MAP_I_TO_B               = $0C74;
  GL_PIXEL_MAP_I_TO_A               = $0C75;
  GL_PIXEL_MAP_R_TO_R               = $0C76;
  GL_PIXEL_MAP_G_TO_G               = $0C77;
  GL_PIXEL_MAP_B_TO_B               = $0C78;
  GL_PIXEL_MAP_A_TO_A               = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE          = $0CB9;
  GL_UNPACK_SWAP_BYTES              = $0CF0;
  GL_UNPACK_LSB_FIRST               = $0CF1;
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_UNPACK_SKIP_ROWS               = $0CF3;
  GL_UNPACK_SKIP_PIXELS             = $0CF4;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_SWAP_BYTES                = $0D00;
  GL_PACK_LSB_FIRST                 = $0D01;
  GL_PACK_ROW_LENGTH                = $0D02;
  GL_PACK_SKIP_ROWS                 = $0D03;
  GL_PACK_SKIP_PIXELS               = $0D04;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAP_COLOR                      = $0D10;
  GL_MAP_STENCIL                    = $0D11;
  GL_INDEX_SHIFT                    = $0D12;
  GL_INDEX_OFFSET                   = $0D13;
  GL_RED_SCALE                      = $0D14;
  GL_RED_BIAS                       = $0D15;
  GL_ZOOM_X                         = $0D16;
  GL_ZOOM_Y                         = $0D17;
  GL_GREEN_SCALE                    = $0D18;
  GL_GREEN_BIAS                     = $0D19;
  GL_BLUE_SCALE                     = $0D1A;
  GL_BLUE_BIAS                      = $0D1B;
  GL_ALPHA_SCALE                    = $0D1C;
  GL_ALPHA_BIAS                     = $0D1D;
  GL_DEPTH_SCALE                    = $0D1E;
  GL_DEPTH_BIAS                     = $0D1F;
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_INDEX_BITS                     = $0D51;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_ACCUM_RED_BITS                 = $0D58;
  GL_ACCUM_GREEN_BITS               = $0D59;
  GL_ACCUM_BLUE_BITS                = $0D5A;
  GL_ACCUM_ALPHA_BITS               = $0D5B;
  GL_NAME_STACK_DEPTH               = $0D70;
  GL_AUTO_NORMAL                    = $0D80;
  GL_MAP1_COLOR_4                   = $0D90;
  GL_MAP1_INDEX                     = $0D91;
  GL_MAP1_NORMAL                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1           = $0D93;
  GL_MAP1_TEXTURE_COORD_2           = $0D94;
  GL_MAP1_TEXTURE_COORD_3           = $0D95;
  GL_MAP1_TEXTURE_COORD_4           = $0D96;
  GL_MAP1_VERTEX_3                  = $0D97;
  GL_MAP1_VERTEX_4                  = $0D98;
  GL_MAP2_COLOR_4                   = $0DB0;
  GL_MAP2_INDEX                     = $0DB1;
  GL_MAP2_NORMAL                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4           = $0DB6;
  GL_MAP2_VERTEX_3                  = $0DB7;
  GL_MAP2_VERTEX_4                  = $0DB8;
  GL_MAP1_GRID_DOMAIN               = $0DD0;
  GL_MAP1_GRID_SEGMENTS             = $0DD1;
  GL_MAP2_GRID_DOMAIN               = $0DD2;
  GL_MAP2_GRID_SEGMENTS             = $0DD3;
  GL_TEXTURE_1D                     = $0DE0;
  GL_TEXTURE_2D                     = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE           = $0DF2;
  GL_SELECTION_BUFFER_POINTER       = $0DF3;
  GL_SELECTION_BUFFER_SIZE          = $0DF4;
  //      GL_TEXTURE_BINDING_1D
  //      GL_TEXTURE_BINDING_2D
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_VERTEX_ARRAY_SIZE
  //      GL_VERTEX_ARRAY_TYPE
  //      GL_VERTEX_ARRAY_STRIDE
  //      GL_NORMAL_ARRAY_TYPE
  //      GL_NORMAL_ARRAY_STRIDE
  //      GL_COLOR_ARRAY_SIZE
  //      GL_COLOR_ARRAY_TYPE
  //      GL_COLOR_ARRAY_STRIDE
  //      GL_INDEX_ARRAY_TYPE
  //      GL_INDEX_ARRAY_STRIDE
  //      GL_TEXTURE_COORD_ARRAY_SIZE
  //      GL_TEXTURE_COORD_ARRAY_TYPE
  //      GL_TEXTURE_COORD_ARRAY_STRIDE
  //      GL_EDGE_FLAG_ARRAY_STRIDE
  //      GL_POLYGON_OFFSET_FACTOR
  //      GL_POLYGON_OFFSET_UNITS

  // GetTextureParameter
  //      GL_TEXTURE_MAG_FILTER
  //      GL_TEXTURE_MIN_FILTER
  //      GL_TEXTURE_WRAP_S
  //      GL_TEXTURE_WRAP_T
  GL_TEXTURE_WIDTH                  = $1000;
  GL_TEXTURE_HEIGHT                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT        = $1003;
  GL_TEXTURE_BORDER_COLOR           = $1004;
  GL_TEXTURE_BORDER                 = $1005;
  //      GL_TEXTURE_RED_SIZE
  //      GL_TEXTURE_GREEN_SIZE
  //      GL_TEXTURE_BLUE_SIZE
  //      GL_TEXTURE_ALPHA_SIZE
  //      GL_TEXTURE_LUMINANCE_SIZE
  //      GL_TEXTURE_INTENSITY_SIZE
  //      GL_TEXTURE_PRIORITY
  //      GL_TEXTURE_RESIDENT

  // HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // HintTarget
  //      GL_PERSPECTIVE_CORRECTION_HINT
  //      GL_POINT_SMOOTH_HINT
  //      GL_LINE_SMOOTH_HINT
  //      GL_POLYGON_SMOOTH_HINT
  //      GL_FOG_HINT

  // IndexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // LightModelParameter
  //      GL_LIGHT_MODEL_AMBIENT
  //      GL_LIGHT_MODEL_LOCAL_VIEWER
  //      GL_LIGHT_MODEL_TWO_SIDE

  // LightName
  GL_LIGHT0                         = $4000;
  GL_LIGHT1                         = $4001;
  GL_LIGHT2                         = $4002;
  GL_LIGHT3                         = $4003;
  GL_LIGHT4                         = $4004;
  GL_LIGHT5                         = $4005;
  GL_LIGHT6                         = $4006;
  GL_LIGHT7                         = $4007;

  // LightParameter
  GL_AMBIENT                        = $1200;
  GL_DIFFUSE                        = $1201;
  GL_SPECULAR                       = $1202;
  GL_POSITION                       = $1203;
  GL_SPOT_DIRECTION                 = $1204;
  GL_SPOT_EXPONENT                  = $1205;
  GL_SPOT_CUTOFF                    = $1206;
  GL_CONSTANT_ATTENUATION           = $1207;
  GL_LINEAR_ATTENUATION             = $1208;
  GL_QUADRATIC_ATTENUATION          = $1209;

  // InterleavedArrays
  //      GL_V2F
  //      GL_V3F
  //      GL_C4UB_V2F
  //      GL_C4UB_V3F
  //      GL_C3F_V3F
  //      GL_N3F_V3F
  //      GL_C4F_N3F_V3F
  //      GL_T2F_V3F
  //      GL_T4F_V4F
  //      GL_T2F_C4UB_V3F
  //      GL_T2F_C3F_V3F
  //      GL_T2F_N3F_V3F
  //      GL_T2F_C4F_N3F_V3F
  //      GL_T4F_C4F_N3F_V4F

  // ListMode
  GL_COMPILE                        = $1300;
  GL_COMPILE_AND_EXECUTE            = $1301;

  // ListNameType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_2_BYTES
  //      GL_3_BYTES
  //      GL_4_BYTES

  // LogicOp
  GL_CLEAR                          = $1500;
  GL_AND                            = $1501;
  GL_AND_REVERSE                    = $1502;
  GL_COPY                           = $1503;
  GL_AND_INVERTED                   = $1504;
  GL_NOOP                           = $1505;
  GL_XOR                            = $1506;
  GL_OR                             = $1507;
  GL_NOR                            = $1508;
  GL_EQUIV                          = $1509;
  GL_INVERT                         = $150A;
  GL_OR_REVERSE                     = $150B;
  GL_COPY_INVERTED                  = $150C;
  GL_OR_INVERTED                    = $150D;
  GL_NAND                           = $150E;
  GL_SET                            = $150F;

  // MapTarget
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4

  // MaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // MaterialParameter
  GL_EMISSION                       = $1600;
  GL_SHININESS                      = $1601;
  GL_AMBIENT_AND_DIFFUSE            = $1602;
  GL_COLOR_INDEXES                  = $1603;
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR

  // MatrixMode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // MeshMode1
  //      GL_POINT
  //      GL_LINE

  // MeshMode2
  //      GL_POINT
  //      GL_LINE
  //      GL_FILL

  // NormalPointerType
  //      GL_BYTE
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // PixelCopyType
  GL_COLOR                          = $1800;
  GL_DEPTH                          = $1801;
  GL_STENCIL                        = $1802;

  // PixelFormat
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

  // PixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // PixelStore
  //      GL_UNPACK_SWAP_BYTES
  //      GL_UNPACK_LSB_FIRST
  //      GL_UNPACK_ROW_LENGTH
  //      GL_UNPACK_SKIP_ROWS
  //      GL_UNPACK_SKIP_PIXELS
  //      GL_UNPACK_ALIGNMENT
  //      GL_PACK_SWAP_BYTES
  //      GL_PACK_LSB_FIRST
  //      GL_PACK_ROW_LENGTH
  //      GL_PACK_SKIP_ROWS
  //      GL_PACK_SKIP_PIXELS
  //      GL_PACK_ALIGNMENT

  // PixelTransfer
  //      GL_MAP_COLOR
  //      GL_MAP_STENCIL
  //      GL_INDEX_SHIFT
  //      GL_INDEX_OFFSET
  //      GL_RED_SCALE
  //      GL_RED_BIAS
  //      GL_GREEN_SCALE
  //      GL_GREEN_BIAS
  //      GL_BLUE_SCALE
  //      GL_BLUE_BIAS
  //      GL_ALPHA_SCALE
  //      GL_ALPHA_BIAS
  //      GL_DEPTH_SCALE
  //      GL_DEPTH_BIAS

  // PixelType
  GL_BITMAP                         = $1A00;
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT

  // PolygonMode
  GL_POINT                          = $1B00;
  GL_LINE                           = $1B01;
  GL_FILL                           = $1B02;

  // ReadBufferMode
  //      GL_FRONT_LEFT
  //      GL_FRONT_RIGHT
  //      GL_BACK_LEFT
  //      GL_BACK_RIGHT
  //      GL_FRONT
  //      GL_BACK
  //      GL_LEFT
  //      GL_RIGHT
  //      GL_AUX0
  //      GL_AUX1
  //      GL_AUX2
  //      GL_AUX3

  // RenderingMode
  GL_RENDER                         = $1C00;
  GL_FEEDBACK                       = $1C01;
  GL_SELECT                         = $1C02;

  // ShadingModel
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // StencilFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // StencilOp
  //      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  //      GL_INVERT

  // StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // TextureCoordName
  GL_S                              = $2000;
  GL_T                              = $2001;
  GL_R                              = $2002;
  GL_Q                              = $2003;

  // TexCoordPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // TextureEnvMode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  //      GL_BLEND
  //      GL_REPLACE

  // TextureEnvParameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;

  // TextureEnvTarget
  GL_TEXTURE_ENV                    = $2300;

  // TextureGenMode
  GL_EYE_LINEAR                     = $2400;
  GL_OBJECT_LINEAR                  = $2401;
  GL_SPHERE_MAP                     = $2402;

  // TextureGenParameter
  GL_TEXTURE_GEN_MODE               = $2500;
  GL_OBJECT_PLANE                   = $2501;
  GL_EYE_PLANE                      = $2502;

  // TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

  // TextureMinFilter
  //      GL_NEAREST
  //      GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;

  // TextureParameterName
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;
  //      GL_TEXTURE_BORDER_COLOR
  //      GL_TEXTURE_PRIORITY

  // TextureTarget
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_PROXY_TEXTURE_1D
  //      GL_PROXY_TEXTURE_2D

  // TextureWrapMode
  GL_CLAMP                          = $2900;
  GL_REPEAT                         = $2901;

  // VertexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // ClientAttribMask
  GL_CLIENT_PIXEL_STORE_BIT         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS         = $FFFFFFFF;

  // polygon_offset
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
  GL_POLYGON_OFFSET_POINT           = $2A01;
  GL_POLYGON_OFFSET_LINE            = $2A02;
  GL_POLYGON_OFFSET_FILL            = $8037;

  // texture
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
  GL_TEXTURE_RED_SIZE               = $805C;
  GL_TEXTURE_GREEN_SIZE             = $805D;
  GL_TEXTURE_BLUE_SIZE              = $805E;
  GL_TEXTURE_ALPHA_SIZE             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE         = $8060;
  GL_TEXTURE_INTENSITY_SIZE         = $8061;
  GL_PROXY_TEXTURE_1D               = $8063;
  GL_PROXY_TEXTURE_2D               = $8064;

  // texture_object
  GL_TEXTURE_PRIORITY               = $8066;
  GL_TEXTURE_RESIDENT               = $8067;
  GL_TEXTURE_BINDING_1D             = $8068;
  GL_TEXTURE_BINDING_2D             = $8069;

  // vertex_array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;
  GL_EDGE_FLAG_ARRAY                = $8079;
  GL_VERTEX_ARRAY_SIZE              = $807A;
  GL_VERTEX_ARRAY_TYPE              = $807B;
  GL_VERTEX_ARRAY_STRIDE            = $807C;
  GL_NORMAL_ARRAY_TYPE              = $807E;
  GL_NORMAL_ARRAY_STRIDE            = $807F;
  GL_COLOR_ARRAY_SIZE               = $8081;
  GL_COLOR_ARRAY_TYPE               = $8082;
  GL_COLOR_ARRAY_STRIDE             = $8083;
  GL_INDEX_ARRAY_TYPE               = $8085;
  GL_INDEX_ARRAY_STRIDE             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE         = $808C;
  GL_VERTEX_ARRAY_POINTER           = $808E;
  GL_NORMAL_ARRAY_POINTER           = $808F;
  GL_COLOR_ARRAY_POINTER            = $8090;
  GL_INDEX_ARRAY_POINTER            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER        = $8093;
  GL_V2F                            = $2A20;
  GL_V3F                            = $2A21;
  GL_C4UB_V2F                       = $2A22;
  GL_C4UB_V3F                       = $2A23;
  GL_C3F_V3F                        = $2A24;
  GL_N3F_V3F                        = $2A25;
  GL_C4F_N3F_V3F                    = $2A26;
  GL_T2F_V3F                        = $2A27;
  GL_T4F_V4F                        = $2A28;
  GL_T2F_C4UB_V3F                   = $2A29;
  GL_T2F_C3F_V3F                    = $2A2A;
  GL_T2F_N3F_V3F                    = $2A2B;
  GL_T2F_C4F_N3F_V3F                = $2A2C;
  GL_T4F_C4F_N3F_V4F                = $2A2D;

  // For compatibility with OpenGL v1.0
  GL_LOGIC_OP                       = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS             = GL_TEXTURE_INTERNAL_FORMAT;

{******************************************************************************}

{$IFDEF MORPHOS}

{ MorphOS GL works differently due to different dynamic-library handling on Amiga-like }
{ systems, so its headers are included here. }
{$INCLUDE tinyglh.inc}

{$ELSE MORPHOS}
var
  glAccum: procedure(op: GLenum; value: GLfloat); extdecl;
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); extdecl;
  glAreTexturesResident: function (n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; extdecl;
  glArrayElement: procedure(i: GLint); extdecl;
  glBegin: procedure(mode: GLenum); extdecl;
  glBindTexture: procedure(target: GLenum; texture: GLuint); extdecl;
  glBitmap: procedure (width, height: GLsizei; xorig, yorig: GLfloat; xmove, ymove: GLfloat; const bitmap: PGLubyte); extdecl;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); extdecl;
  glCallList: procedure(list: GLuint); extdecl;
  glCallLists: procedure(n: GLsizei; atype: GLenum; const lists: Pointer); extdecl;
  glClear: procedure(mask: GLbitfield); extdecl;
  glClearAccum: procedure(red, green, blue, alpha: GLfloat); extdecl;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); extdecl;
  glClearDepth: procedure(depth: GLclampd); extdecl;
  glClearIndex: procedure(c: GLfloat); extdecl;
  glClearStencil: procedure(s: GLint); extdecl;
  glClipPlane: procedure(plane: GLenum; const equation: PGLdouble); extdecl;
  glColor3b: procedure(red, green, blue: GLbyte); extdecl;
  glColor3bv: procedure(const v: PGLbyte); extdecl;
  glColor3d: procedure(red, green, blue: GLdouble); extdecl;
  glColor3dv: procedure(const v: PGLdouble); extdecl;
  glColor3f: procedure(red, green, blue: GLfloat); extdecl;
  glColor3fv: procedure(const v: PGLfloat); extdecl;
  glColor3i: procedure(red, green, blue: GLint); extdecl;
  glColor3iv: procedure(const v: PGLint); extdecl;
  glColor3s: procedure(red, green, blue: GLshort); extdecl;
  glColor3sv: procedure(const v: PGLshort); extdecl;
  glColor3ub: procedure(red, green, blue: GLubyte); extdecl;
  glColor3ubv: procedure(const v: PGLubyte); extdecl;
  glColor3ui: procedure(red, green, blue: GLuint); extdecl;
  glColor3uiv: procedure(const v: PGLuint); extdecl;
  glColor3us: procedure(red, green, blue: GLushort); extdecl;
  glColor3usv: procedure(const v: PGLushort); extdecl;
  glColor4b: procedure(red, green, blue, alpha: GLbyte); extdecl;
  glColor4bv: procedure(const v: PGLbyte); extdecl;
  glColor4d: procedure(red, green, blue, alpha: GLdouble); extdecl;
  glColor4dv: procedure(const v: PGLdouble); extdecl;
  glColor4f: procedure(red, green, blue, alpha: GLfloat); extdecl;
  glColor4fv: procedure(const v: PGLfloat); extdecl;
  glColor4i: procedure(red, green, blue, alpha: GLint); extdecl;
  glColor4iv: procedure(const v: PGLint); extdecl;
  glColor4s: procedure(red, green, blue, alpha: GLshort); extdecl;
  glColor4sv: procedure(const v: PGLshort); extdecl;
  glColor4ub: procedure(red, green, blue, alpha: GLubyte); extdecl;
  glColor4ubv: procedure(const v: PGLubyte); extdecl;
  glColor4ui: procedure(red, green, blue, alpha: GLuint); extdecl;
  glColor4uiv: procedure(const v: PGLuint); extdecl;
  glColor4us: procedure(red, green, blue, alpha: GLushort); extdecl;
  glColor4usv: procedure(const v: PGLushort); extdecl;
  glColorMask: procedure(red, green, blue, alpha: GLboolean); extdecl;
  glColorMaterial: procedure(face, mode: GLenum); extdecl;
  glColorPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glCopyPixels: procedure(x, y: GLint; width, height: GLsizei; atype: GLenum); extdecl;
  glCopyTexImage1D: procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); extdecl;
  glCullFace: procedure(mode: GLenum); extdecl;
  glDeleteLists: procedure(list: GLuint; range: GLsizei); extdecl;
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); extdecl;
  glDepthFunc: procedure(func: GLenum); extdecl;
  glDepthMask: procedure(flag: GLboolean); extdecl;
  glDepthRange: procedure(zNear, zFar: GLclampd); extdecl;
  glDisable: procedure(cap: GLenum); extdecl;
  glDisableClientState: procedure(aarray: GLenum); extdecl;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawBuffer: procedure(mode: GLenum); extdecl;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); extdecl;
  glDrawPixels: procedure(width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glEdgeFlag: procedure(flag: GLboolean); extdecl;
  glEdgeFlagPointer: procedure(stride: GLsizei; const pointer: Pointer); extdecl;
  glEdgeFlagv: procedure(const flag: PGLboolean); extdecl;
  glEnable: procedure(cap: GLenum); extdecl;
  glEnableClientState: procedure(aarray: GLenum); extdecl;
  glEnd: procedure; extdecl;
  glEndList: procedure; extdecl;
  glEvalCoord1d: procedure(u: GLdouble); extdecl;
  glEvalCoord1dv: procedure(const u: PGLdouble); extdecl;
  glEvalCoord1f: procedure(u: GLfloat); extdecl;
  glEvalCoord1fv: procedure(const u: PGLfloat); extdecl;
  glEvalCoord2d: procedure(u, v: GLdouble); extdecl;
  glEvalCoord2dv: procedure(const u: PGLdouble); extdecl;
  glEvalCoord2f: procedure(u, v: GLfloat); extdecl;
  glEvalCoord2fv: procedure(const u: PGLfloat); extdecl;
  glEvalMesh1: procedure(mode: GLenum; i1, i2: GLint); extdecl;
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: GLint); extdecl;
  glEvalPoint1: procedure(i: GLint); extdecl;
  glEvalPoint2: procedure(i, j: GLint); extdecl;
  glFeedbackBuffer: procedure(size: GLsizei; atype: GLenum; buffer: PGLfloat); extdecl;
  glFinish: procedure; extdecl;
  glFlush: procedure; extdecl;
  glFogf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glFogfv: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glFogi: procedure(pname: GLenum; param: GLint); extdecl;
  glFogiv: procedure(pname: GLenum; const params: PGLint); extdecl;
  glFrontFace: procedure(mode: GLenum); extdecl;
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: GLdouble); extdecl;
  glGenLists: function(range: GLsizei): GLuint; extdecl;
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); extdecl;
  glGetBooleanv: procedure(pname: GLenum; params: PGLboolean); extdecl;
  glGetClipPlane: procedure(plane: GLenum; equation: PGLdouble); extdecl;
  glGetDoublev: procedure(pname: GLenum; params: PGLdouble); extdecl;
  glGetError: function: GLenum; extdecl;
  glGetFloatv: procedure(pname: GLenum; params: PGLfloat); extdecl;
  glGetIntegerv: procedure(pname: GLenum; params: PGLint); extdecl;
  glGetLightfv: procedure(light, pname: GLenum; params: PGLfloat); extdecl;
  glGetLightiv: procedure(light, pname: GLenum; params: PGLint); extdecl;
  glGetMapdv: procedure(target, query: GLenum; v: PGLdouble); extdecl;
  glGetMapfv: procedure(target, query: GLenum; v: PGLfloat); extdecl;
  glGetMapiv: procedure(target, query: GLenum; v: PGLint); extdecl;
  glGetMaterialfv: procedure(face, pname: GLenum; params: PGLfloat); extdecl;
  glGetMaterialiv: procedure(face, pname: GLenum; params: PGLint); extdecl;
  glGetPixelMapfv: procedure(map: GLenum; values: PGLfloat); extdecl;
  glGetPixelMapuiv: procedure(map: GLenum; values: PGLuint); extdecl;
  glGetPixelMapusv: procedure(map: GLenum; values: PGLushort); extdecl;
  glGetPointerv: procedure(pname: GLenum; params: Pointer); extdecl;
  glGetPolygonStipple: procedure(mask: PGLubyte); extdecl;
  glGetString: function(name: GLenum): PChar; extdecl;
  glGetTexEnvfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexEnviv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetTexGendv: procedure(coord, pname: GLenum; params: PGLdouble); extdecl;
  glGetTexGenfv: procedure(coord, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexGeniv: procedure(coord, pname: GLenum; params: PGLint); extdecl;
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); extdecl;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: Pointer); extdecl;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glGetTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glHint: procedure(target, mode: GLenum); extdecl;
  glIndexMask: procedure(mask: GLuint); extdecl;
  glIndexPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glIndexd: procedure(c: GLdouble); extdecl;
  glIndexdv: procedure(const c: PGLdouble); extdecl;
  glIndexf: procedure(c: GLfloat); extdecl;
  glIndexfv: procedure(const c: PGLfloat); extdecl;
  glIndexi: procedure(c: GLint); extdecl;
  glIndexiv: procedure(const c: PGLint); extdecl;
  glIndexs: procedure(c: GLshort); extdecl;
  glIndexsv: procedure(const c: PGLshort); extdecl;
  glIndexub: procedure(c: GLubyte); extdecl;
  glIndexubv: procedure(const c: PGLubyte); extdecl;
  glInitNames: procedure; extdecl;
  glInterleavedArrays: procedure(format: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glIsEnabled: function(cap: GLenum): GLboolean; extdecl;
  glIsList: function(list: GLuint): GLboolean; extdecl;
  glIsTexture: function(texture: GLuint): GLboolean; extdecl;
  glLightModelf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glLightModelfv: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glLightModeli: procedure(pname: GLenum; param: GLint); extdecl;
  glLightModeliv: procedure(pname: GLenum; const params: PGLint); extdecl;
  glLightf: procedure(light, pname: GLenum; param: GLfloat); extdecl;
  glLightfv: procedure(light, pname: GLenum; const params: PGLfloat); extdecl;
  glLighti: procedure(light, pname: GLenum; param: GLint); extdecl;
  glLightiv: procedure(light, pname: GLenum; const params: PGLint); extdecl;
  glLineStipple: procedure(factor: GLint; pattern: GLushort); extdecl;
  glLineWidth: procedure(width: GLfloat); extdecl;
  glListBase: procedure(base: GLuint); extdecl;
  glLoadIdentity: procedure; extdecl;
  glLoadMatrixd: procedure(const m: PGLdouble); extdecl;
  glLoadMatrixf: procedure(const m: PGLfloat); extdecl;
  glLoadName: procedure(name: GLuint); extdecl;
  glLogicOp: procedure(opcode: GLenum); extdecl;
  glMap1d: procedure(target: GLenum; u1, u2: GLdouble; stride, order: GLint; const points: PGLdouble); extdecl;
  glMap1f: procedure(target: GLenum; u1, u2: GLfloat; stride, order: GLint; const points: PGLfloat); extdecl;
  glMap2d: procedure(target: GLenum; u1, u2: GLdouble; ustride, uorder: GLint; v1, v2: GLdouble; vstride, vorder: GLint; const points: PGLdouble); extdecl;
  glMap2f: procedure(target: GLenum; u1, u2: GLfloat; ustride, uorder: GLint; v1, v2: GLfloat; vstride, vorder: GLint; const points: PGLfloat); extdecl;
  glMapGrid1d: procedure(un: GLint; u1, u2: GLdouble); extdecl;
  glMapGrid1f: procedure(un: GLint; u1, u2: GLfloat); extdecl;
  glMapGrid2d: procedure(un: GLint; u1, u2: GLdouble; vn: GLint; v1, v2: GLdouble); extdecl;
  glMapGrid2f: procedure(un: GLint; u1, u2: GLfloat; vn: GLint; v1, v2: GLfloat); extdecl;
  glMaterialf: procedure(face, pname: GLenum; param: GLfloat); extdecl;
  glMaterialfv: procedure(face, pname: GLenum; const params: PGLfloat); extdecl;
  glMateriali: procedure(face, pname: GLenum; param: GLint); extdecl;
  glMaterialiv: procedure(face, pname: GLenum; const params: PGLint); extdecl;
  glMatrixMode: procedure(mode: GLenum); extdecl;
  glMultMatrixd: procedure(const m: PGLdouble); extdecl;
  glMultMatrixf: procedure(const m: PGLfloat); extdecl;
  glNewList: procedure(list: GLuint; mode: GLenum); extdecl;
  glNormal3b: procedure(nx, ny, nz: GLbyte); extdecl;
  glNormal3bv: procedure(const v: PGLbyte); extdecl;
  glNormal3d: procedure(nx, ny, nz: GLdouble); extdecl;
  glNormal3dv: procedure(const v: PGLdouble); extdecl;
  glNormal3f: procedure(nx, ny, nz: GLfloat); extdecl;
  glNormal3fv: procedure(const v: PGLfloat); extdecl;
  glNormal3i: procedure(nx, ny, nz: GLint); extdecl;
  glNormal3iv: procedure(const v: PGLint); extdecl;
  glNormal3s: procedure(nx, ny, nz: GLshort); extdecl;
  glNormal3sv: procedure(const v: PGLshort); extdecl;
  glNormalPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: GLdouble); extdecl;
  glPassThrough: procedure(token: GLfloat); extdecl;
  glPixelMapfv: procedure(map: GLenum; mapsize: GLint; const values: PGLfloat); extdecl;
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLint; const values: PGLuint); extdecl;
  glPixelMapusv: procedure(map: GLenum; mapsize: GLint; const values: PGLushort); extdecl;
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelStorei: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelTransferi: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelZoom: procedure(xfactor, yfactor: GLfloat); extdecl;
  glPointSize: procedure(size: GLfloat); extdecl;
  glPolygonMode: procedure(face, mode: GLenum); extdecl;
  glPolygonOffset: procedure(factor, units: GLfloat); extdecl;
  glPolygonStipple: procedure(const mask: PGLubyte); extdecl;
  glPopAttrib: procedure; extdecl;
  glPopClientAttrib: procedure; extdecl;
  glPopMatrix: procedure; extdecl;
  glPopName: procedure; extdecl;
  glPrioritizeTextures: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); extdecl;
  glPushAttrib: procedure(mask: GLbitfield); extdecl;
  glPushClientAttrib: procedure(mask: GLbitfield); extdecl;
  glPushMatrix: procedure; extdecl;
  glPushName: procedure(name: GLuint); extdecl;
  glRasterPos2d: procedure(x, y: GLdouble); extdecl;
  glRasterPos2dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos2f: procedure(x, y: GLfloat); extdecl;
  glRasterPos2fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos2i: procedure(x, y: GLint); extdecl;
  glRasterPos2iv: procedure(const v: PGLint); extdecl;
  glRasterPos2s: procedure(x, y: GLshort); extdecl;
  glRasterPos2sv: procedure(const v: PGLshort); extdecl;
  glRasterPos3d: procedure(x, y, z: GLdouble); extdecl;
  glRasterPos3dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos3f: procedure(x, y, z: GLfloat); extdecl;
  glRasterPos3fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos3i: procedure(x, y, z: GLint); extdecl;
  glRasterPos3iv: procedure(const v: PGLint); extdecl;
  glRasterPos3s: procedure(x, y, z: GLshort); extdecl;
  glRasterPos3sv: procedure(const v: PGLshort); extdecl;
  glRasterPos4d: procedure(x, y, z, w: GLdouble); extdecl;
  glRasterPos4dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos4f: procedure(x, y, z, w: GLfloat); extdecl;
  glRasterPos4fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos4i: procedure(x, y, z, w: GLint); extdecl;
  glRasterPos4iv: procedure(const v: PGLint); extdecl;
  glRasterPos4s: procedure(x, y, z, w: GLshort); extdecl;
  glRasterPos4sv: procedure(const v: PGLshort); extdecl;
  glReadBuffer: procedure(mode: GLenum); extdecl;
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); extdecl;
  glRectd: procedure(x1, y1, x2, y2: GLdouble); extdecl;
  glRectdv: procedure(const v1: PGLdouble; const v2: PGLdouble); extdecl;
  glRectf: procedure(x1, y1, x2, y2: GLfloat); extdecl;
  glRectfv: procedure(const v1: PGLfloat; const v2: PGLfloat); extdecl;
  glRecti: procedure(x1, y1, x2, y2: GLint); extdecl;
  glRectiv: procedure(const v1: PGLint; const v2: PGLint); extdecl;
  glRects: procedure(x1, y1, x2, y2: GLshort); extdecl;
  glRectsv: procedure(const v1: PGLshort; const v2: PGLshort); extdecl;
  glRenderMode: function(mode: GLint): GLint; extdecl;
  glRotated: procedure(angle, x, y, z: GLdouble); extdecl;
  glRotatef: procedure(angle, x, y, z: GLfloat); extdecl;
  glScaled: procedure(x, y, z: GLdouble); extdecl;
  glScalef: procedure(x, y, z: GLfloat); extdecl;
  glScissor: procedure(x, y: GLint; width, height: GLsizei); extdecl;
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); extdecl;
  glShadeModel: procedure(mode: GLenum); extdecl;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); extdecl;
  glStencilMask: procedure(mask: GLuint); extdecl;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); extdecl;
  glTexCoord1d: procedure(s: GLdouble); extdecl;
  glTexCoord1dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord1f: procedure(s: GLfloat); extdecl;
  glTexCoord1fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord1i: procedure(s: GLint); extdecl;
  glTexCoord1iv: procedure(const v: PGLint); extdecl;
  glTexCoord1s: procedure(s: GLshort); extdecl;
  glTexCoord1sv: procedure(const v: PGLshort); extdecl;
  glTexCoord2d: procedure(s, t: GLdouble); extdecl;
  glTexCoord2dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord2f: procedure(s, t: GLfloat); extdecl;
  glTexCoord2fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord2i: procedure(s, t: GLint); extdecl;
  glTexCoord2iv: procedure(const v: PGLint); extdecl;
  glTexCoord2s: procedure(s, t: GLshort); extdecl;
  glTexCoord2sv: procedure(const v: PGLshort); extdecl;
  glTexCoord3d: procedure(s, t, r: GLdouble); extdecl;
  glTexCoord3dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord3f: procedure(s, t, r: GLfloat); extdecl;
  glTexCoord3fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord3i: procedure(s, t, r: GLint); extdecl;
  glTexCoord3iv: procedure(const v: PGLint); extdecl;
  glTexCoord3s: procedure(s, t, r: GLshort); extdecl;
  glTexCoord3sv: procedure(const v: PGLshort); extdecl;
  glTexCoord4d: procedure(s, t, r, q: GLdouble); extdecl;
  glTexCoord4dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord4f: procedure(s, t, r, q: GLfloat); extdecl;
  glTexCoord4fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord4i: procedure(s, t, r, q: GLint); extdecl;
  glTexCoord4iv: procedure(const v: PGLint); extdecl;
  glTexCoord4s: procedure(s, t, r, q: GLshort); extdecl;
  glTexCoord4sv: procedure(const v: PGLshort); extdecl;
  glTexCoordPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glTexEnvf: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexEnvfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexEnvi: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexEnviv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexGend: procedure(coord: GLenum; pname: GLenum; param: GLdouble); extdecl;
  glTexGendv: procedure(coord: GLenum; pname: GLenum; const params: PGLdouble); extdecl;
  glTexGenf: procedure(coord: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexGenfv: procedure(coord: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexGeni: procedure(coord: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexGeniv: procedure(coord: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexImage1D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexImage2D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTranslated: procedure(x, y, z: GLdouble); extdecl;
  glTranslatef: procedure(x, y, z: GLfloat); extdecl;
  glVertex2d: procedure(x, y: GLdouble); extdecl;
  glVertex2dv: procedure(const v: PGLdouble); extdecl;
  glVertex2f: procedure(x, y: GLfloat); extdecl;
  glVertex2fv: procedure(const v: PGLfloat); extdecl;
  glVertex2i: procedure(x, y: GLint); extdecl;
  glVertex2iv: procedure(const v: PGLint); extdecl;
  glVertex2s: procedure(x, y: GLshort); extdecl;
  glVertex2sv: procedure(const v: PGLshort); extdecl;
  glVertex3d: procedure(x, y, z: GLdouble); extdecl;
  glVertex3dv: procedure(const v: PGLdouble); extdecl;
  glVertex3f: procedure(x, y, z: GLfloat); extdecl;
  glVertex3fv: procedure(const v: PGLfloat); extdecl;
  glVertex3i: procedure(x, y, z: GLint); extdecl;
  glVertex3iv: procedure(const v: PGLint); extdecl;
  glVertex3s: procedure(x, y, z: GLshort); extdecl;
  glVertex3sv: procedure(const v: PGLshort); extdecl;
  glVertex4d: procedure(x, y, z, w: GLdouble); extdecl;
  glVertex4dv: procedure(const v: PGLdouble); extdecl;
  glVertex4f: procedure(x, y, z, w: GLfloat); extdecl;
  glVertex4fv: procedure(const v: PGLfloat); extdecl;
  glVertex4i: procedure(x, y, z, w: GLint); extdecl;
  glVertex4iv: procedure(const v: PGLint); extdecl;
  glVertex4s: procedure(x, y, z, w: GLshort); extdecl;
  glVertex4sv: procedure(const v: PGLshort); extdecl;
  glVertexPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glViewport: procedure(x, y: GLint; width, height: GLsizei); extdecl;
  {$IFDEF Windows}
  ChoosePixelFormat: function(DC: HDC; p2: PPixelFormatDescriptor): Integer; extdecl;
  {$ENDIF}
{$ENDIF MORPHOS}

type
  // EXT_vertex_array
  PFNGLARRAYELEMENTEXTPROC = procedure(i: GLint); extdecl;
  PFNGLDRAWARRAYSEXTPROC = procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  PFNGLVERTEXPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                        stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLNORMALPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                        const pointer: Pointer); extdecl;
  PFNGLCOLORPOINTEREXTPROC = procedure(size: GLint; atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); extdecl;
  PFNGLINDEXPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); extdecl;
  PFNGLTEXCOORDPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                          stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLEDGEFLAGPOINTEREXTPROC = procedure(stride, count: GLsizei;
                                          const pointer: PGLboolean); extdecl;
  PFNGLGETPOINTERVEXTPROC = procedure(pname: GLenum; params: Pointer); extdecl;
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: GLenum; count: GLsizei;
                                            const pi: Pointer); extdecl;

  // WIN_swap_hint
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: GLint; width, height: GLsizei); extdecl;

  // EXT_paletted_texture
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: GLenum; width: GLsizei;
                                     format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: GLenum; start, count: GLsizei;
                                        format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: GLenum; data: Pointer); extdecl;
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: GLenum; params: PGLint); extdecl;
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: GLenum; params: PGLfloat); extdecl;

procedure LoadOpenGL(const dll: String);
procedure FreeOpenGL;

implementation

{$if defined(cpui386) or defined(cpux86_64)}
uses
  math;
{$endif}

{$ifdef windows}
function WinChoosePixelFormat(DC: HDC; p2: PPixelFormatDescriptor): Integer; extdecl; external 'gdi32' name 'ChoosePixelFormat';
{$endif}

{$IFDEF MORPHOS}

{ MorphOS GL works differently due to different dynamic-library handling on Amiga-like }
{ systems, so its functions are included here. }
{$INCLUDE tinygl.inc}

{$ENDIF MORPHOS}

procedure FreeOpenGL;
begin
{$IFDEF MORPHOS}

  // MorphOS's GL will closed down by TinyGL unit, nothing is needed here.

{$ELSE MORPHOS}
  @glAccum := nil;
  @glAlphaFunc := nil;
  @glAreTexturesResident := nil;
  @glArrayElement := nil;
  @glBegin := nil;
  @glBindTexture := nil;
  @glBitmap := nil;
  @glBlendFunc := nil;
  @glCallList := nil;
  @glCallLists := nil;
  @glClear := nil;
  @glClearAccum := nil;
  @glClearColor := nil;
  @glClearDepth := nil;
  @glClearIndex := nil;
  @glClearStencil := nil;
  @glClipPlane := nil;
  @glColor3b := nil;
  @glColor3bv := nil;
  @glColor3d := nil;
  @glColor3dv := nil;
  @glColor3f := nil;
  @glColor3fv := nil;
  @glColor3i := nil;
  @glColor3iv := nil;
  @glColor3s := nil;
  @glColor3sv := nil;
  @glColor3ub := nil;
  @glColor3ubv := nil;
  @glColor3ui := nil;
  @glColor3uiv := nil;
  @glColor3us := nil;
  @glColor3usv := nil;
  @glColor4b := nil;
  @glColor4bv := nil;
  @glColor4d := nil;
  @glColor4dv := nil;
  @glColor4f := nil;
  @glColor4fv := nil;
  @glColor4i := nil;
  @glColor4iv := nil;
  @glColor4s := nil;
  @glColor4sv := nil;
  @glColor4ub := nil;
  @glColor4ubv := nil;
  @glColor4ui := nil;
  @glColor4uiv := nil;
  @glColor4us := nil;
  @glColor4usv := nil;
  @glColorMask := nil;
  @glColorMaterial := nil;
  @glColorPointer := nil;
  @glCopyPixels := nil;
  @glCopyTexImage1D := nil;
  @glCopyTexImage2D := nil;
  @glCopyTexSubImage1D := nil;
  @glCopyTexSubImage2D := nil;
  @glCullFace := nil;
  @glDeleteLists := nil;
  @glDeleteTextures := nil;
  @glDepthFunc := nil;
  @glDepthMask := nil;
  @glDepthRange := nil;
  @glDisable := nil;
  @glDisableClientState := nil;
  @glDrawArrays := nil;
  @glDrawBuffer := nil;
  @glDrawElements := nil;
  @glDrawPixels := nil;
  @glEdgeFlag := nil;
  @glEdgeFlagPointer := nil;
  @glEdgeFlagv := nil;
  @glEnable := nil;
  @glEnableClientState := nil;
  @glEnd := nil;
  @glEndList := nil;
  @glEvalCoord1d := nil;
  @glEvalCoord1dv := nil;
  @glEvalCoord1f := nil;
  @glEvalCoord1fv := nil;
  @glEvalCoord2d := nil;
  @glEvalCoord2dv := nil;
  @glEvalCoord2f := nil;
  @glEvalCoord2fv := nil;
  @glEvalMesh1 := nil;
  @glEvalMesh2 := nil;
  @glEvalPoint1 := nil;
  @glEvalPoint2 := nil;
  @glFeedbackBuffer := nil;
  @glFinish := nil;
  @glFlush := nil;
  @glFogf := nil;
  @glFogfv := nil;
  @glFogi := nil;
  @glFogiv := nil;
  @glFrontFace := nil;
  @glFrustum := nil;
  @glGenLists := nil;
  @glGenTextures := nil;
  @glGetBooleanv := nil;
  @glGetClipPlane := nil;
  @glGetDoublev := nil;
  @glGetError := nil;
  @glGetFloatv := nil;
  @glGetIntegerv := nil;
  @glGetLightfv := nil;
  @glGetLightiv := nil;
  @glGetMapdv := nil;
  @glGetMapfv := nil;
  @glGetMapiv := nil;
  @glGetMaterialfv := nil;
  @glGetMaterialiv := nil;
  @glGetPixelMapfv := nil;
  @glGetPixelMapuiv := nil;
  @glGetPixelMapusv := nil;
  @glGetPointerv := nil;
  @glGetPolygonStipple := nil;
  @glGetString := nil;
  @glGetTexEnvfv := nil;
  @glGetTexEnviv := nil;
  @glGetTexGendv := nil;
  @glGetTexGenfv := nil;
  @glGetTexGeniv := nil;
  @glGetTexImage := nil;
  @glGetTexLevelParameterfv := nil;
  @glGetTexLevelParameteriv := nil;
  @glGetTexParameterfv := nil;
  @glGetTexParameteriv := nil;
  @glHint := nil;
  @glIndexMask := nil;
  @glIndexPointer := nil;
  @glIndexd := nil;
  @glIndexdv := nil;
  @glIndexf := nil;
  @glIndexfv := nil;
  @glIndexi := nil;
  @glIndexiv := nil;
  @glIndexs := nil;
  @glIndexsv := nil;
  @glIndexub := nil;
  @glIndexubv := nil;
  @glInitNames := nil;
  @glInterleavedArrays := nil;
  @glIsEnabled := nil;
  @glIsList := nil;
  @glIsTexture := nil;
  @glLightModelf := nil;
  @glLightModelfv := nil;
  @glLightModeli := nil;
  @glLightModeliv := nil;
  @glLightf := nil;
  @glLightfv := nil;
  @glLighti := nil;
  @glLightiv := nil;
  @glLineStipple := nil;
  @glLineWidth := nil;
  @glListBase := nil;
  @glLoadIdentity := nil;
  @glLoadMatrixd := nil;
  @glLoadMatrixf := nil;
  @glLoadName := nil;
  @glLogicOp := nil;
  @glMap1d := nil;
  @glMap1f := nil;
  @glMap2d := nil;
  @glMap2f := nil;
  @glMapGrid1d := nil;
  @glMapGrid1f := nil;
  @glMapGrid2d := nil;
  @glMapGrid2f := nil;
  @glMaterialf := nil;
  @glMaterialfv := nil;
  @glMateriali := nil;
  @glMaterialiv := nil;
  @glMatrixMode := nil;
  @glMultMatrixd := nil;
  @glMultMatrixf := nil;
  @glNewList := nil;
  @glNormal3b := nil;
  @glNormal3bv := nil;
  @glNormal3d := nil;
  @glNormal3dv := nil;
  @glNormal3f := nil;
  @glNormal3fv := nil;
  @glNormal3i := nil;
  @glNormal3iv := nil;
  @glNormal3s := nil;
  @glNormal3sv := nil;
  @glNormalPointer := nil;
  @glOrtho := nil;
  @glPassThrough := nil;
  @glPixelMapfv := nil;
  @glPixelMapuiv := nil;
  @glPixelMapusv := nil;
  @glPixelStoref := nil;
  @glPixelStorei := nil;
  @glPixelTransferf := nil;
  @glPixelTransferi := nil;
  @glPixelZoom := nil;
  @glPointSize := nil;
  @glPolygonMode := nil;
  @glPolygonOffset := nil;
  @glPolygonStipple := nil;
  @glPopAttrib := nil;
  @glPopClientAttrib := nil;
  @glPopMatrix := nil;
  @glPopName := nil;
  @glPrioritizeTextures := nil;
  @glPushAttrib := nil;
  @glPushClientAttrib := nil;
  @glPushMatrix := nil;
  @glPushName := nil;
  @glRasterPos2d := nil;
  @glRasterPos2dv := nil;
  @glRasterPos2f := nil;
  @glRasterPos2fv := nil;
  @glRasterPos2i := nil;
  @glRasterPos2iv := nil;
  @glRasterPos2s := nil;
  @glRasterPos2sv := nil;
  @glRasterPos3d := nil;
  @glRasterPos3dv := nil;
  @glRasterPos3f := nil;
  @glRasterPos3fv := nil;
  @glRasterPos3i := nil;
  @glRasterPos3iv := nil;
  @glRasterPos3s := nil;
  @glRasterPos3sv := nil;
  @glRasterPos4d := nil;
  @glRasterPos4dv := nil;
  @glRasterPos4f := nil;
  @glRasterPos4fv := nil;
  @glRasterPos4i := nil;
  @glRasterPos4iv := nil;
  @glRasterPos4s := nil;
  @glRasterPos4sv := nil;
  @glReadBuffer := nil;
  @glReadPixels := nil;
  @glRectd := nil;
  @glRectdv := nil;
  @glRectf := nil;
  @glRectfv := nil;
  @glRecti := nil;
  @glRectiv := nil;
  @glRects := nil;
  @glRectsv := nil;
  @glRenderMode := nil;
  @glRotated := nil;
  @glRotatef := nil;
  @glScaled := nil;
  @glScalef := nil;
  @glScissor := nil;
  @glSelectBuffer := nil;
  @glShadeModel := nil;
  @glStencilFunc := nil;
  @glStencilMask := nil;
  @glStencilOp := nil;
  @glTexCoord1d := nil;
  @glTexCoord1dv := nil;
  @glTexCoord1f := nil;
  @glTexCoord1fv := nil;
  @glTexCoord1i := nil;
  @glTexCoord1iv := nil;
  @glTexCoord1s := nil;
  @glTexCoord1sv := nil;
  @glTexCoord2d := nil;
  @glTexCoord2dv := nil;
  @glTexCoord2f := nil;
  @glTexCoord2fv := nil;
  @glTexCoord2i := nil;
  @glTexCoord2iv := nil;
  @glTexCoord2s := nil;
  @glTexCoord2sv := nil;
  @glTexCoord3d := nil;
  @glTexCoord3dv := nil;
  @glTexCoord3f := nil;
  @glTexCoord3fv := nil;
  @glTexCoord3i := nil;
  @glTexCoord3iv := nil;
  @glTexCoord3s := nil;
  @glTexCoord3sv := nil;
  @glTexCoord4d := nil;
  @glTexCoord4dv := nil;
  @glTexCoord4f := nil;
  @glTexCoord4fv := nil;
  @glTexCoord4i := nil;
  @glTexCoord4iv := nil;
  @glTexCoord4s := nil;
  @glTexCoord4sv := nil;
  @glTexCoordPointer := nil;
  @glTexEnvf := nil;
  @glTexEnvfv := nil;
  @glTexEnvi := nil;
  @glTexEnviv := nil;
  @glTexGend := nil;
  @glTexGendv := nil;
  @glTexGenf := nil;
  @glTexGenfv := nil;
  @glTexGeni := nil;
  @glTexGeniv := nil;
  @glTexImage1D := nil;
  @glTexImage2D := nil;
  @glTexParameterf := nil;
  @glTexParameterfv := nil;
  @glTexParameteri := nil;
  @glTexParameteriv := nil;
  @glTexSubImage1D := nil;
  @glTexSubImage2D := nil;
  @glTranslated := nil;
  @glTranslatef := nil;
  @glVertex2d := nil;
  @glVertex2dv := nil;
  @glVertex2f := nil;
  @glVertex2fv := nil;
  @glVertex2i := nil;
  @glVertex2iv := nil;
  @glVertex2s := nil;
  @glVertex2sv := nil;
  @glVertex3d := nil;
  @glVertex3dv := nil;
  @glVertex3f := nil;
  @glVertex3fv := nil;
  @glVertex3i := nil;
  @glVertex3iv := nil;
  @glVertex3s := nil;
  @glVertex3sv := nil;
  @glVertex4d := nil;
  @glVertex4dv := nil;
  @glVertex4f := nil;
  @glVertex4fv := nil;
  @glVertex4i := nil;
  @glVertex4iv := nil;
  @glVertex4s := nil;
  @glVertex4sv := nil;
  @glVertexPointer := nil;
  @glViewport := nil;
  {$IFDEF Windows}
  @ChoosePixelFormat := nil;
  {$ENDIF}

  if (LibGL <> 0) then
    FreeLibrary(LibGL);
{$ENDIF MORPHOS}
end;

procedure LoadOpenGL(const dll: String);
{$IFDEF MORPHOS}
begin
  // MorphOS's GL has own initialization in TinyGL unit, nothing is needed here.
end;
{$ELSE MORPHOS}
var
  MethodName: string = '';

  function GetGLProcAddress(Lib: PtrInt; ProcName: PChar): Pointer;
  begin
    MethodName:=ProcName;
    Result:=GetProcAddress(Lib, ProcName);
  end;

begin

  FreeOpenGL;

  LibGL := LoadLibrary(PChar(dll));
  //Writeln('Using hacked GL unit to tolerate missing OpenGL');
  if LibGL = 0 then Exit;
  try
    @glAccum := GetGLProcAddress(LibGL, 'glAccum');
    @glAlphaFunc := GetGLProcAddress(LibGL, 'glAlphaFunc');
    @glAreTexturesResident := GetGLProcAddress(LibGL, 'glAreTexturesResident');
    @glArrayElement := GetGLProcAddress(LibGL, 'glArrayElement');
    @glBegin := GetGLProcAddress(LibGL, 'glBegin');
    @glBindTexture := GetGLProcAddress(LibGL, 'glBindTexture');
    @glBitmap := GetGLProcAddress(LibGL, 'glBitmap');
    @glBlendFunc := GetGLProcAddress(LibGL, 'glBlendFunc');
    @glCallList := GetGLProcAddress(LibGL, 'glCallList');
    @glCallLists := GetGLProcAddress(LibGL, 'glCallLists');
    @glClear := GetGLProcAddress(LibGL, 'glClear');
    @glClearAccum := GetGLProcAddress(LibGL, 'glClearAccum');
    @glClearColor := GetGLProcAddress(LibGL, 'glClearColor');
    @glClearDepth := GetGLProcAddress(LibGL, 'glClearDepth');
    @glClearIndex := GetGLProcAddress(LibGL, 'glClearIndex');
    @glClearStencil := GetGLProcAddress(LibGL, 'glClearStencil');
    @glClipPlane := GetGLProcAddress(LibGL, 'glClipPlane');
    @glColor3b := GetGLProcAddress(LibGL, 'glColor3b');
    @glColor3bv := GetGLProcAddress(LibGL, 'glColor3bv');
    @glColor3d := GetGLProcAddress(LibGL, 'glColor3d');
    @glColor3dv := GetGLProcAddress(LibGL, 'glColor3dv');
    @glColor3f := GetGLProcAddress(LibGL, 'glColor3f');
    @glColor3fv := GetGLProcAddress(LibGL, 'glColor3fv');
    @glColor3i := GetGLProcAddress(LibGL, 'glColor3i');
    @glColor3iv := GetGLProcAddress(LibGL, 'glColor3iv');
    @glColor3s := GetGLProcAddress(LibGL, 'glColor3s');
    @glColor3sv := GetGLProcAddress(LibGL, 'glColor3sv');
    @glColor3ub := GetGLProcAddress(LibGL, 'glColor3ub');
    @glColor3ubv := GetGLProcAddress(LibGL, 'glColor3ubv');
    @glColor3ui := GetGLProcAddress(LibGL, 'glColor3ui');
    @glColor3uiv := GetGLProcAddress(LibGL, 'glColor3uiv');
    @glColor3us := GetGLProcAddress(LibGL, 'glColor3us');
    @glColor3usv := GetGLProcAddress(LibGL, 'glColor3usv');
    @glColor4b := GetGLProcAddress(LibGL, 'glColor4b');
    @glColor4bv := GetGLProcAddress(LibGL, 'glColor4bv');
    @glColor4d := GetGLProcAddress(LibGL, 'glColor4d');
    @glColor4dv := GetGLProcAddress(LibGL, 'glColor4dv');
    @glColor4f := GetGLProcAddress(LibGL, 'glColor4f');
    @glColor4fv := GetGLProcAddress(LibGL, 'glColor4fv');
    @glColor4i := GetGLProcAddress(LibGL, 'glColor4i');
    @glColor4iv := GetGLProcAddress(LibGL, 'glColor4iv');
    @glColor4s := GetGLProcAddress(LibGL, 'glColor4s');
    @glColor4sv := GetGLProcAddress(LibGL, 'glColor4sv');
    @glColor4ub := GetGLProcAddress(LibGL, 'glColor4ub');
    @glColor4ubv := GetGLProcAddress(LibGL, 'glColor4ubv');
    @glColor4ui := GetGLProcAddress(LibGL, 'glColor4ui');
    @glColor4uiv := GetGLProcAddress(LibGL, 'glColor4uiv');
    @glColor4us := GetGLProcAddress(LibGL, 'glColor4us');
    @glColor4usv := GetGLProcAddress(LibGL, 'glColor4usv');
    @glColorMask := GetGLProcAddress(LibGL, 'glColorMask');
    @glColorMaterial := GetGLProcAddress(LibGL, 'glColorMaterial');
    @glColorPointer := GetGLProcAddress(LibGL, 'glColorPointer');
    @glCopyPixels := GetGLProcAddress(LibGL, 'glCopyPixels');
    @glCopyTexImage1D := GetGLProcAddress(LibGL, 'glCopyTexImage1D');
    @glCopyTexImage2D := GetGLProcAddress(LibGL, 'glCopyTexImage2D');
    @glCopyTexSubImage1D := GetGLProcAddress(LibGL, 'glCopyTexSubImage1D');
    @glCopyTexSubImage2D := GetGLProcAddress(LibGL, 'glCopyTexSubImage2D');
    @glCullFace := GetGLProcAddress(LibGL, 'glCullFace');
    @glDeleteLists := GetGLProcAddress(LibGL, 'glDeleteLists');
    @glDeleteTextures := GetGLProcAddress(LibGL, 'glDeleteTextures');
    @glDepthFunc := GetGLProcAddress(LibGL, 'glDepthFunc');
    @glDepthMask := GetGLProcAddress(LibGL, 'glDepthMask');
    @glDepthRange := GetGLProcAddress(LibGL, 'glDepthRange');
    @glDisable := GetGLProcAddress(LibGL, 'glDisable');
    @glDisableClientState := GetGLProcAddress(LibGL, 'glDisableClientState');
    @glDrawArrays := GetGLProcAddress(LibGL, 'glDrawArrays');
    @glDrawBuffer := GetGLProcAddress(LibGL, 'glDrawBuffer');
    @glDrawElements := GetGLProcAddress(LibGL, 'glDrawElements');
    @glDrawPixels := GetGLProcAddress(LibGL, 'glDrawPixels');
    @glEdgeFlag := GetGLProcAddress(LibGL, 'glEdgeFlag');
    @glEdgeFlagPointer := GetGLProcAddress(LibGL, 'glEdgeFlagPointer');
    @glEdgeFlagv := GetGLProcAddress(LibGL, 'glEdgeFlagv');
    @glEnable := GetGLProcAddress(LibGL, 'glEnable');
    @glEnableClientState := GetGLProcAddress(LibGL, 'glEnableClientState');
    @glEnd := GetGLProcAddress(LibGL, 'glEnd');
    @glEndList := GetGLProcAddress(LibGL, 'glEndList');
    @glEvalCoord1d := GetGLProcAddress(LibGL, 'glEvalCoord1d');
    @glEvalCoord1dv := GetGLProcAddress(LibGL, 'glEvalCoord1dv');
    @glEvalCoord1f := GetGLProcAddress(LibGL, 'glEvalCoord1f');
    @glEvalCoord1fv := GetGLProcAddress(LibGL, 'glEvalCoord1fv');
    @glEvalCoord2d := GetGLProcAddress(LibGL, 'glEvalCoord2d');
    @glEvalCoord2dv := GetGLProcAddress(LibGL, 'glEvalCoord2dv');
    @glEvalCoord2f := GetGLProcAddress(LibGL, 'glEvalCoord2f');
    @glEvalCoord2fv := GetGLProcAddress(LibGL, 'glEvalCoord2fv');
    @glEvalMesh1 := GetGLProcAddress(LibGL, 'glEvalMesh1');
    @glEvalMesh2 := GetGLProcAddress(LibGL, 'glEvalMesh2');
    @glEvalPoint1 := GetGLProcAddress(LibGL, 'glEvalPoint1');
    @glEvalPoint2 := GetGLProcAddress(LibGL, 'glEvalPoint2');
    @glFeedbackBuffer := GetGLProcAddress(LibGL, 'glFeedbackBuffer');
    @glFinish := GetGLProcAddress(LibGL, 'glFinish');
    @glFlush := GetGLProcAddress(LibGL, 'glFlush');
    @glFogf := GetGLProcAddress(LibGL, 'glFogf');
    @glFogfv := GetGLProcAddress(LibGL, 'glFogfv');
    @glFogi := GetGLProcAddress(LibGL, 'glFogi');
    @glFogiv := GetGLProcAddress(LibGL, 'glFogiv');
    @glFrontFace := GetGLProcAddress(LibGL, 'glFrontFace');
    @glFrustum := GetGLProcAddress(LibGL, 'glFrustum');
    @glGenLists := GetGLProcAddress(LibGL, 'glGenLists');
    @glGenTextures := GetGLProcAddress(LibGL, 'glGenTextures');
    @glGetBooleanv := GetGLProcAddress(LibGL, 'glGetBooleanv');
    @glGetClipPlane := GetGLProcAddress(LibGL, 'glGetClipPlane');
    @glGetDoublev := GetGLProcAddress(LibGL, 'glGetDoublev');
    @glGetError := GetGLProcAddress(LibGL, 'glGetError');
    @glGetFloatv := GetGLProcAddress(LibGL, 'glGetFloatv');
    @glGetIntegerv := GetGLProcAddress(LibGL, 'glGetIntegerv');
    @glGetLightfv := GetGLProcAddress(LibGL, 'glGetLightfv');
    @glGetLightiv := GetGLProcAddress(LibGL, 'glGetLightiv');
    @glGetMapdv := GetGLProcAddress(LibGL, 'glGetMapdv');
    @glGetMapfv := GetGLProcAddress(LibGL, 'glGetMapfv');
    @glGetMapiv := GetGLProcAddress(LibGL, 'glGetMapiv');
    @glGetMaterialfv := GetGLProcAddress(LibGL, 'glGetMaterialfv');
    @glGetMaterialiv := GetGLProcAddress(LibGL, 'glGetMaterialiv');
    @glGetPixelMapfv := GetGLProcAddress(LibGL, 'glGetPixelMapfv');
    @glGetPixelMapuiv := GetGLProcAddress(LibGL, 'glGetPixelMapuiv');
    @glGetPixelMapusv := GetGLProcAddress(LibGL, 'glGetPixelMapusv');
    @glGetPointerv := GetGLProcAddress(LibGL, 'glGetPointerv');
    @glGetPolygonStipple := GetGLProcAddress(LibGL, 'glGetPolygonStipple');
    @glGetString := GetGLProcAddress(LibGL, 'glGetString');
    @glGetTexEnvfv := GetGLProcAddress(LibGL, 'glGetTexEnvfv');
    @glGetTexEnviv := GetGLProcAddress(LibGL, 'glGetTexEnviv');
    @glGetTexGendv := GetGLProcAddress(LibGL, 'glGetTexGendv');
    @glGetTexGenfv := GetGLProcAddress(LibGL, 'glGetTexGenfv');
    @glGetTexGeniv := GetGLProcAddress(LibGL, 'glGetTexGeniv');
    @glGetTexImage := GetGLProcAddress(LibGL, 'glGetTexImage');
    @glGetTexLevelParameterfv := GetGLProcAddress(LibGL, 'glGetTexLevelParameterfv');
    @glGetTexLevelParameteriv := GetGLProcAddress(LibGL, 'glGetTexLevelParameteriv');
    @glGetTexParameterfv := GetGLProcAddress(LibGL, 'glGetTexParameterfv');
    @glGetTexParameteriv := GetGLProcAddress(LibGL, 'glGetTexParameteriv');
    @glHint := GetGLProcAddress(LibGL, 'glHint');
    @glIndexMask := GetGLProcAddress(LibGL, 'glIndexMask');
    @glIndexPointer := GetGLProcAddress(LibGL, 'glIndexPointer');
    @glIndexd := GetGLProcAddress(LibGL, 'glIndexd');
    @glIndexdv := GetGLProcAddress(LibGL, 'glIndexdv');
    @glIndexf := GetGLProcAddress(LibGL, 'glIndexf');
    @glIndexfv := GetGLProcAddress(LibGL, 'glIndexfv');
    @glIndexi := GetGLProcAddress(LibGL, 'glIndexi');
    @glIndexiv := GetGLProcAddress(LibGL, 'glIndexiv');
    @glIndexs := GetGLProcAddress(LibGL, 'glIndexs');
    @glIndexsv := GetGLProcAddress(LibGL, 'glIndexsv');
    @glIndexub := GetGLProcAddress(LibGL, 'glIndexub');
    @glIndexubv := GetGLProcAddress(LibGL, 'glIndexubv');
    @glInitNames := GetGLProcAddress(LibGL, 'glInitNames');
    @glInterleavedArrays := GetGLProcAddress(LibGL, 'glInterleavedArrays');
    @glIsEnabled := GetGLProcAddress(LibGL, 'glIsEnabled');
    @glIsList := GetGLProcAddress(LibGL, 'glIsList');
    @glIsTexture := GetGLProcAddress(LibGL, 'glIsTexture');
    @glLightModelf := GetGLProcAddress(LibGL, 'glLightModelf');
    @glLightModelfv := GetGLProcAddress(LibGL, 'glLightModelfv');
    @glLightModeli := GetGLProcAddress(LibGL, 'glLightModeli');
    @glLightModeliv := GetGLProcAddress(LibGL, 'glLightModeliv');
    @glLightf := GetGLProcAddress(LibGL, 'glLightf');
    @glLightfv := GetGLProcAddress(LibGL, 'glLightfv');
    @glLighti := GetGLProcAddress(LibGL, 'glLighti');
    @glLightiv := GetGLProcAddress(LibGL, 'glLightiv');
    @glLineStipple := GetGLProcAddress(LibGL, 'glLineStipple');
    @glLineWidth := GetGLProcAddress(LibGL, 'glLineWidth');
    @glListBase := GetGLProcAddress(LibGL, 'glListBase');
    @glLoadIdentity := GetGLProcAddress(LibGL, 'glLoadIdentity');
    @glLoadMatrixd := GetGLProcAddress(LibGL, 'glLoadMatrixd');
    @glLoadMatrixf := GetGLProcAddress(LibGL, 'glLoadMatrixf');
    @glLoadName := GetGLProcAddress(LibGL, 'glLoadName');
    @glLogicOp := GetGLProcAddress(LibGL, 'glLogicOp');
    @glMap1d := GetGLProcAddress(LibGL, 'glMap1d');
    @glMap1f := GetGLProcAddress(LibGL, 'glMap1f');
    @glMap2d := GetGLProcAddress(LibGL, 'glMap2d');
    @glMap2f := GetGLProcAddress(LibGL, 'glMap2f');
    @glMapGrid1d := GetGLProcAddress(LibGL, 'glMapGrid1d');
    @glMapGrid1f := GetGLProcAddress(LibGL, 'glMapGrid1f');
    @glMapGrid2d := GetGLProcAddress(LibGL, 'glMapGrid2d');
    @glMapGrid2f := GetGLProcAddress(LibGL, 'glMapGrid2f');
    @glMaterialf := GetGLProcAddress(LibGL, 'glMaterialf');
    @glMaterialfv := GetGLProcAddress(LibGL, 'glMaterialfv');
    @glMateriali := GetGLProcAddress(LibGL, 'glMateriali');
    @glMaterialiv := GetGLProcAddress(LibGL, 'glMaterialiv');
    @glMatrixMode := GetGLProcAddress(LibGL, 'glMatrixMode');
    @glMultMatrixd := GetGLProcAddress(LibGL, 'glMultMatrixd');
    @glMultMatrixf := GetGLProcAddress(LibGL, 'glMultMatrixf');
    @glNewList := GetGLProcAddress(LibGL, 'glNewList');
    @glNormal3b := GetGLProcAddress(LibGL, 'glNormal3b');
    @glNormal3bv := GetGLProcAddress(LibGL, 'glNormal3bv');
    @glNormal3d := GetGLProcAddress(LibGL, 'glNormal3d');
    @glNormal3dv := GetGLProcAddress(LibGL, 'glNormal3dv');
    @glNormal3f := GetGLProcAddress(LibGL, 'glNormal3f');
    @glNormal3fv := GetGLProcAddress(LibGL, 'glNormal3fv');
    @glNormal3i := GetGLProcAddress(LibGL, 'glNormal3i');
    @glNormal3iv := GetGLProcAddress(LibGL, 'glNormal3iv');
    @glNormal3s := GetGLProcAddress(LibGL, 'glNormal3s');
    @glNormal3sv := GetGLProcAddress(LibGL, 'glNormal3sv');
    @glNormalPointer := GetGLProcAddress(LibGL, 'glNormalPointer');
    @glOrtho := GetGLProcAddress(LibGL, 'glOrtho');
    @glPassThrough := GetGLProcAddress(LibGL, 'glPassThrough');
    @glPixelMapfv := GetGLProcAddress(LibGL, 'glPixelMapfv');
    @glPixelMapuiv := GetGLProcAddress(LibGL, 'glPixelMapuiv');
    @glPixelMapusv := GetGLProcAddress(LibGL, 'glPixelMapusv');
    @glPixelStoref := GetGLProcAddress(LibGL, 'glPixelStoref');
    @glPixelStorei := GetGLProcAddress(LibGL, 'glPixelStorei');
    @glPixelTransferf := GetGLProcAddress(LibGL, 'glPixelTransferf');
    @glPixelTransferi := GetGLProcAddress(LibGL, 'glPixelTransferi');
    @glPixelZoom := GetGLProcAddress(LibGL, 'glPixelZoom');
    @glPointSize := GetGLProcAddress(LibGL, 'glPointSize');
    @glPolygonMode := GetGLProcAddress(LibGL, 'glPolygonMode');
    @glPolygonOffset := GetGLProcAddress(LibGL, 'glPolygonOffset');
    @glPolygonStipple := GetGLProcAddress(LibGL, 'glPolygonStipple');
    @glPopAttrib := GetGLProcAddress(LibGL, 'glPopAttrib');
    @glPopClientAttrib := GetGLProcAddress(LibGL, 'glPopClientAttrib');
    @glPopMatrix := GetGLProcAddress(LibGL, 'glPopMatrix');
    @glPopName := GetGLProcAddress(LibGL, 'glPopName');
    @glPrioritizeTextures := GetGLProcAddress(LibGL, 'glPrioritizeTextures');
    @glPushAttrib := GetGLProcAddress(LibGL, 'glPushAttrib');
    @glPushClientAttrib := GetGLProcAddress(LibGL, 'glPushClientAttrib');
    @glPushMatrix := GetGLProcAddress(LibGL, 'glPushMatrix');
    @glPushName := GetGLProcAddress(LibGL, 'glPushName');
    @glRasterPos2d := GetGLProcAddress(LibGL, 'glRasterPos2d');
    @glRasterPos2dv := GetGLProcAddress(LibGL, 'glRasterPos2dv');
    @glRasterPos2f := GetGLProcAddress(LibGL, 'glRasterPos2f');
    @glRasterPos2fv := GetGLProcAddress(LibGL, 'glRasterPos2fv');
    @glRasterPos2i := GetGLProcAddress(LibGL, 'glRasterPos2i');
    @glRasterPos2iv := GetGLProcAddress(LibGL, 'glRasterPos2iv');
    @glRasterPos2s := GetGLProcAddress(LibGL, 'glRasterPos2s');
    @glRasterPos2sv := GetGLProcAddress(LibGL, 'glRasterPos2sv');
    @glRasterPos3d := GetGLProcAddress(LibGL, 'glRasterPos3d');
    @glRasterPos3dv := GetGLProcAddress(LibGL, 'glRasterPos3dv');
    @glRasterPos3f := GetGLProcAddress(LibGL, 'glRasterPos3f');
    @glRasterPos3fv := GetGLProcAddress(LibGL, 'glRasterPos3fv');
    @glRasterPos3i := GetGLProcAddress(LibGL, 'glRasterPos3i');
    @glRasterPos3iv := GetGLProcAddress(LibGL, 'glRasterPos3iv');
    @glRasterPos3s := GetGLProcAddress(LibGL, 'glRasterPos3s');
    @glRasterPos3sv := GetGLProcAddress(LibGL, 'glRasterPos3sv');
    @glRasterPos4d := GetGLProcAddress(LibGL, 'glRasterPos4d');
    @glRasterPos4dv := GetGLProcAddress(LibGL, 'glRasterPos4dv');
    @glRasterPos4f := GetGLProcAddress(LibGL, 'glRasterPos4f');
    @glRasterPos4fv := GetGLProcAddress(LibGL, 'glRasterPos4fv');
    @glRasterPos4i := GetGLProcAddress(LibGL, 'glRasterPos4i');
    @glRasterPos4iv := GetGLProcAddress(LibGL, 'glRasterPos4iv');
    @glRasterPos4s := GetGLProcAddress(LibGL, 'glRasterPos4s');
    @glRasterPos4sv := GetGLProcAddress(LibGL, 'glRasterPos4sv');
    @glReadBuffer := GetGLProcAddress(LibGL, 'glReadBuffer');
    @glReadPixels := GetGLProcAddress(LibGL, 'glReadPixels');
    @glRectd := GetGLProcAddress(LibGL, 'glRectd');
    @glRectdv := GetGLProcAddress(LibGL, 'glRectdv');
    @glRectf := GetGLProcAddress(LibGL, 'glRectf');
    @glRectfv := GetGLProcAddress(LibGL, 'glRectfv');
    @glRecti := GetGLProcAddress(LibGL, 'glRecti');
    @glRectiv := GetGLProcAddress(LibGL, 'glRectiv');
    @glRects := GetGLProcAddress(LibGL, 'glRects');
    @glRectsv := GetGLProcAddress(LibGL, 'glRectsv');
    @glRenderMode := GetGLProcAddress(LibGL, 'glRenderMode');
    @glRotated := GetGLProcAddress(LibGL, 'glRotated');
    @glRotatef := GetGLProcAddress(LibGL, 'glRotatef');
    @glScaled := GetGLProcAddress(LibGL, 'glScaled');
    @glScalef := GetGLProcAddress(LibGL, 'glScalef');
    @glScissor := GetGLProcAddress(LibGL, 'glScissor');
    @glSelectBuffer := GetGLProcAddress(LibGL, 'glSelectBuffer');
    @glShadeModel := GetGLProcAddress(LibGL, 'glShadeModel');
    @glStencilFunc := GetGLProcAddress(LibGL, 'glStencilFunc');
    @glStencilMask := GetGLProcAddress(LibGL, 'glStencilMask');
    @glStencilOp := GetGLProcAddress(LibGL, 'glStencilOp');
    @glTexCoord1d := GetGLProcAddress(LibGL, 'glTexCoord1d');
    @glTexCoord1dv := GetGLProcAddress(LibGL, 'glTexCoord1dv');
    @glTexCoord1f := GetGLProcAddress(LibGL, 'glTexCoord1f');
    @glTexCoord1fv := GetGLProcAddress(LibGL, 'glTexCoord1fv');
    @glTexCoord1i := GetGLProcAddress(LibGL, 'glTexCoord1i');
    @glTexCoord1iv := GetGLProcAddress(LibGL, 'glTexCoord1iv');
    @glTexCoord1s := GetGLProcAddress(LibGL, 'glTexCoord1s');
    @glTexCoord1sv := GetGLProcAddress(LibGL, 'glTexCoord1sv');
    @glTexCoord2d := GetGLProcAddress(LibGL, 'glTexCoord2d');
    @glTexCoord2dv := GetGLProcAddress(LibGL, 'glTexCoord2dv');
    @glTexCoord2f := GetGLProcAddress(LibGL, 'glTexCoord2f');
    @glTexCoord2fv := GetGLProcAddress(LibGL, 'glTexCoord2fv');
    @glTexCoord2i := GetGLProcAddress(LibGL, 'glTexCoord2i');
    @glTexCoord2iv := GetGLProcAddress(LibGL, 'glTexCoord2iv');
    @glTexCoord2s := GetGLProcAddress(LibGL, 'glTexCoord2s');
    @glTexCoord2sv := GetGLProcAddress(LibGL, 'glTexCoord2sv');
    @glTexCoord3d := GetGLProcAddress(LibGL, 'glTexCoord3d');
    @glTexCoord3dv := GetGLProcAddress(LibGL, 'glTexCoord3dv');
    @glTexCoord3f := GetGLProcAddress(LibGL, 'glTexCoord3f');
    @glTexCoord3fv := GetGLProcAddress(LibGL, 'glTexCoord3fv');
    @glTexCoord3i := GetGLProcAddress(LibGL, 'glTexCoord3i');
    @glTexCoord3iv := GetGLProcAddress(LibGL, 'glTexCoord3iv');
    @glTexCoord3s := GetGLProcAddress(LibGL, 'glTexCoord3s');
    @glTexCoord3sv := GetGLProcAddress(LibGL, 'glTexCoord3sv');
    @glTexCoord4d := GetGLProcAddress(LibGL, 'glTexCoord4d');
    @glTexCoord4dv := GetGLProcAddress(LibGL, 'glTexCoord4dv');
    @glTexCoord4f := GetGLProcAddress(LibGL, 'glTexCoord4f');
    @glTexCoord4fv := GetGLProcAddress(LibGL, 'glTexCoord4fv');
    @glTexCoord4i := GetGLProcAddress(LibGL, 'glTexCoord4i');
    @glTexCoord4iv := GetGLProcAddress(LibGL, 'glTexCoord4iv');
    @glTexCoord4s := GetGLProcAddress(LibGL, 'glTexCoord4s');
    @glTexCoord4sv := GetGLProcAddress(LibGL, 'glTexCoord4sv');
    @glTexCoordPointer := GetGLProcAddress(LibGL, 'glTexCoordPointer');
    @glTexEnvf := GetGLProcAddress(LibGL, 'glTexEnvf');
    @glTexEnvfv := GetGLProcAddress(LibGL, 'glTexEnvfv');
    @glTexEnvi := GetGLProcAddress(LibGL, 'glTexEnvi');
    @glTexEnviv := GetGLProcAddress(LibGL, 'glTexEnviv');
    @glTexGend := GetGLProcAddress(LibGL, 'glTexGend');
    @glTexGendv := GetGLProcAddress(LibGL, 'glTexGendv');
    @glTexGenf := GetGLProcAddress(LibGL, 'glTexGenf');
    @glTexGenfv := GetGLProcAddress(LibGL, 'glTexGenfv');
    @glTexGeni := GetGLProcAddress(LibGL, 'glTexGeni');
    @glTexGeniv := GetGLProcAddress(LibGL, 'glTexGeniv');
    @glTexImage1D := GetGLProcAddress(LibGL, 'glTexImage1D');
    @glTexImage2D := GetGLProcAddress(LibGL, 'glTexImage2D');
    @glTexParameterf := GetGLProcAddress(LibGL, 'glTexParameterf');
    @glTexParameterfv := GetGLProcAddress(LibGL, 'glTexParameterfv');
    @glTexParameteri := GetGLProcAddress(LibGL, 'glTexParameteri');
    @glTexParameteriv := GetGLProcAddress(LibGL, 'glTexParameteriv');
    @glTexSubImage1D := GetGLProcAddress(LibGL, 'glTexSubImage1D');
    @glTexSubImage2D := GetGLProcAddress(LibGL, 'glTexSubImage2D');
    @glTranslated := GetGLProcAddress(LibGL, 'glTranslated');
    @glTranslatef := GetGLProcAddress(LibGL, 'glTranslatef');
    @glVertex2d := GetGLProcAddress(LibGL, 'glVertex2d');
    @glVertex2dv := GetGLProcAddress(LibGL, 'glVertex2dv');
    @glVertex2f := GetGLProcAddress(LibGL, 'glVertex2f');
    @glVertex2fv := GetGLProcAddress(LibGL, 'glVertex2fv');
    @glVertex2i := GetGLProcAddress(LibGL, 'glVertex2i');
    @glVertex2iv := GetGLProcAddress(LibGL, 'glVertex2iv');
    @glVertex2s := GetGLProcAddress(LibGL, 'glVertex2s');
    @glVertex2sv := GetGLProcAddress(LibGL, 'glVertex2sv');
    @glVertex3d := GetGLProcAddress(LibGL, 'glVertex3d');
    @glVertex3dv := GetGLProcAddress(LibGL, 'glVertex3dv');
    @glVertex3f := GetGLProcAddress(LibGL, 'glVertex3f');
    @glVertex3fv := GetGLProcAddress(LibGL, 'glVertex3fv');
    @glVertex3i := GetGLProcAddress(LibGL, 'glVertex3i');
    @glVertex3iv := GetGLProcAddress(LibGL, 'glVertex3iv');
    @glVertex3s := GetGLProcAddress(LibGL, 'glVertex3s');
    @glVertex3sv := GetGLProcAddress(LibGL, 'glVertex3sv');
    @glVertex4d := GetGLProcAddress(LibGL, 'glVertex4d');
    @glVertex4dv := GetGLProcAddress(LibGL, 'glVertex4dv');
    @glVertex4f := GetGLProcAddress(LibGL, 'glVertex4f');
    @glVertex4fv := GetGLProcAddress(LibGL, 'glVertex4fv');
    @glVertex4i := GetGLProcAddress(LibGL, 'glVertex4i');
    @glVertex4iv := GetGLProcAddress(LibGL, 'glVertex4iv');
    @glVertex4s := GetGLProcAddress(LibGL, 'glVertex4s');
    @glVertex4sv := GetGLProcAddress(LibGL, 'glVertex4sv');
    @glVertexPointer := GetGLProcAddress(LibGL, 'glVertexPointer');
    @glViewport := GetGLProcAddress(LibGL, 'glViewport');
  except
    raise Exception.Create('Failed loading ' + MethodName +' from ' + dll);
  end;

  {$IFDEF Windows}
  try
    @ChoosePixelFormat := GetGLProcAddress(LibGL, 'ChoosePixelFormat');
    if not Assigned(ChoosePixelFormat) then
      @ChoosePixelFormat := @WinChoosePixelFormat;
  except
    raise Exception.Create('Unable to select pixel format');
  end;
  {$ENDIF}
end;
{$ENDIF MORPHOS}

initialization

  { according to bug 7570, this is necessary on all x86 platforms,
    maybe we've to fix the sse control word as well }
  { Yes, at least for darwin/x86_64 (JM) }
  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  {$endif}

  {$if defined(Windows)}
  LoadOpenGL('opengl32.dll');
  {$elseif defined(OS2)}
  LoadOpenGL('opengl.dll');
  {$elseif defined(darwin)}
  LoadOpenGL('/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib');
  {$elseif defined(MorphOS)}
  InitTinyGLLibrary;
  {$elseif defined(haiku) or defined(OpenBSD)}
  LoadOpenGL('libGL.so');
  {$else}
  LoadOpenGL('libGL.so.1');
  {$endif}

finalization

  FreeOpenGL;

end.
