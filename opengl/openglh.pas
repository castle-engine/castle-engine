{ This is Kambi's OpenGL interface unit.
  Based on Mike Lischke's OpenGL12 unit.
  I (Kambi) changed many things, small and large, for various reasons.

  I changed the name of this unit to @name instead of OpenGL12 to
  avoid conflicts (e.g. GLScene also uses unit @code(OpenGL12)
  based on Mike Lischke work).
  This unit is @italic(not) exactly compatible with original
  @code(OpenGL12) (and it was never meant to be).

  Some list of things that I changed:
  @unorderedList(
    @item(ladowanie bibliotek bylo strasznie nieporzadne i i
      tak nie bylo dobre pod Linuxem. Przepisalem wiec wszystko inaczej,
      konsekwentnie, uzywajac mojego TDynbLib.)

    @item(zmienilem tez semantyke wywolan InitOpenGL, CloseOpenGL)

    @item(typy sa w OpenGLTypes.inc i sa oparte na typach z VectorMath - mojego
      modulu niezaleznego od OpenGL'a ktory definiuje mnostwo pozytecznych
      operacji na wektorach i macierzach ktore w bezposredni sposob sa
      przydatne w OpenGL'u)

    @item(dodalem automatyczne wywolanie InitOpenGL w initialization.)

    @item(Zmodyfikowalem kilka niekonsekwentnych deklaracji ktore zamiast
      pobierac parametry PGLfloat pobieraly var :TVector*f)

    @item(wszystko co ma GLX otoczone $ifdef linux)

    @item(glColor4sv --- zwykla pomylka, poprawione z TGLshort na PGLshort)

    @item(gluTessVertex, 2nd parameter, changed from TVector3d
      (by value ? nonsense) to PVector3d,
      podobnie, gluUnproject, 2 macierze i viewport byly blednie przekazywane
      jako TMatrxi4d i TVactor4i, zmienione na P**. (Wersje T** dzialaly
      pod Delphi, i na tym koniec... FPC przekazywal je inaczej)
      I jeszcze w kilku miejscach tak bylo - parametry typu TVector*, TMatrix*
      byly przekazywane by value - najwyrazniej Delphi przekazywalo je tak
      ze robilo sobie gdzies ich kopie i przekazywalo wskaznik, ale FPC
      tak nie robil ! To nie wina FPC ani Delphi - ten co robil tego headera
      [...] sprawe opierajac sie na szczegolnym zachowaniu kompilatora
      Delphi.)

    @item(rozszerzenia glXa sa ladowane razem z rozszerzeniami (zamiast ladowac
       je razem z normalnymi funkcjami ktore maja gwarancje ze beda obecne))

    @item(...i pare innych drobiazdzkow, m.in. generalnych porzadkow w
      kodzie,  patrz komentarze "Kambi")

    @item(tyle-o-ile przystosowany do roznych UNIXow, nie tylko Linuxa,
      w zasadzie proste /s/Unix/Linux/.)
  )

  Poczatkowo ten modul byl dla win32, potem sciagnalem wersja ktora
  (tylko w teorii) powinna tez dzialac pod Linuxem (czyli kylixem).
  TA wersja modulu OpenGLh rzeczywiscie dziala zarowno pod Delphi,
  FPC+win32, Kylixem i FPC+linux.
  Pozniejsza notka - stracilem kompatybilnosc z Delphi bo zapisalem
  sobie duzo rzeczy wygodniej uzywajac dodatkow z FPC (przede wszystkim
  makr). A, w diabla z Delphi.

  Uwaga - pod Win32 rozne rendering contexts moga implementowac rozne
  zbiory extenstions, o ile maja rozny pixelFormat (jest gwarantowane
  ze jezeli dwa rendering contexts powstaly z tego samego pixelFormat
  to maja te same rozszerzenia o tych samych adresach; patrz html
  "All about OpenGL extensions" Kilgarda). Wiec jezeli uzywasz w programie
  kilku renering contexts o roznych pixelFormatach to NIE MOZESZ
  korzystac z ponizszych globalnych adresow rozszerzen - powinienem
  zaimplementowac adresy rozszerzen w TGLWindow jesli kiedys bede
  tego potrzebowal.

  TODO: this unit is scheduled for removal, I will probably
  switch to using FPC gl, glu, glext units soon.

  Right now you can recompile all my sources with -dUSE_GL_GLU_UNITS
  and get this behavior (thanks to little macro-magic in openglmac.inc).
  But note that you must use FPC 2.0.1 or 2.1.1 (or newer) to get this
  (or you can apply these fixes to older FPC source tree:
  [http://www.freepascal.org/bugs/showrec.php3?ID=3955]
  and [http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg02328.html]).
  When these fixes will be applied in stable FPC version,
  then I will "officially" switch to using gl, glu, glext units
  and this unit will be probably removed.
}

unit OpenGLh;

{******************************************************************************}
{                                                                              }
{       Borland Delphi Runtime Library                                         }
{       OpenGL interface unit                                                  }
{                                                                              }
{                                                                              }
{ This is an interface unit for the use of OpenGL from within Delphi and Kylix.}
{ It contains the translations of gl.h, glu.h, glx.h as well as some support   }
{ functions.                                                                   }
{                                                                              }
{                                                                              }
{ Portions created by Microsoft are                                            }
{ Copyright (C) 1995-2001 Microsoft Corporation.                               }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Silicon Graphics Incorporated are                        }
{ Copyright (C) 1995-2001 Silicon Graphics Incorporated                        }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by NVidia are                                               }
{ Copyright (C) 1995-2001 NVidia                                               }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Brian Paul                                               }
{ Copyright (C) 1995-2001 Brian Paul                                           }
{ All Rights Reserved.                                                         }
{                                                                              }
{                                                                              }
{ The original file is: gl.h                                                   }
{ The original file is: glut.h                                                 }
{ The original file is: glx.h                                                  }
{ The original file is: glx.h                                                  }
{                                                                              }
{ The original Pascal code is: OpenGL12.pas                                    }
{ The initial developer of the Pascal code is Mike Lischke                     }
{                                                                              }
{ Portions created by Mike Lischke are                                         }
{ Copyright (C) 2001 Mike Lischke.                                             }
{                                                                              }
{ Portions created by John O'Harrow are                                        }
{ Copyright (C) 2001 John O'Harrow.                                            }
{                                                                              }
{ Portions created by Eric Grange are                                          }
{ Copyright (C) 2001 Eric Grange.                                              }
{                                                                              }
{ Portions created by Olivier Chatelain                                        }
{ Copyright (C) 2001 Olivier Chatelain.                                        }
{                                                                              }
{ Portions created by Tom Nuydens                                              }
{ Copyright (C) 2001 Tom Nuydens.                                              }
{                                                                              }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2001 Matthias Thoma.                                           }
{                                                                              }
{ Portions created by Sven Bobrowski are                                       }
{ Copyright (C) 2001 Sven Bobrowski                                            }
{                                                                              }
{                                                                              }
{       Obtained through:                                                      }
{                                                                              }
{       Joint Endeavour of Delphi Innovators (Project JEDI)                    }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{******************************************************************************}

{---------------------------------------------------------------------------------------------------------------------- }
{ }
{  This is an interface unit for the use of OpenGL from within Delphi and contains }
{  the translations of gl.h, glu.h as well as some support functions. }
{  OpenGL12.pas contains bug fixes and enhancements of Delphi's and other translations }
{  as well as support for extensions. }
{ }

{---------------------------------------------------------------------------------------------------------------------- }
{ }
{ This translation is based on different sources: }
{ }
{ - first translation from Artemis Alliance Inc. }
{ - previous versions from Mike Lischke }
{ - Alexander Staubo }
{ - Borland OpenGL.pas (from Delphi 3) }
{ - Microsoft and SGI OpenGL header files }
{ - www.opengl.org, www.sgi.com/OpenGL }
{ - nVidia extension reference as of December 1999 }
{ - nVidia extension reference as of January 2001 }
{ - vertex_array_range sample by Tom Nuydens at Delphi3D }
{ - minor bug fixes and greatly extended by John O'Harrow (john@elmcrest.demon.co.uk) }
{ - context activation balancing by Eric Grange (egrange@infonie.fr) }
{ - additional nVidia extensions by Olivier Chatelain (Olivier.Chatelain@xitact.com) }
{ }
{  Contact: public@lischke-online.de }
{ }
{  Version: 1.2.7 }
{---------------------------------------------------------------------------------------------------------------------- }
{ }
{ 05-JUL-2001 ml: }
{   - own exception type for OpenGL }
{   - TGLboolean is now of type BYTEBOOL }
{ 05-MAY-2001 ml: }
{   - correct tracking of RC creation and release as well as multithreaded RC activation }
{   - compatibility routines for users of other OpenGL unit variants }
{   - improved rendering context creation }
{   - bug fixes }
{ 01-MAY-2001 ml: }
{   - added more nVidia extensions }
{---------------------------------------------------------------------------------------------------------------------- }

{ Pod Win32 OLDER_WGL powoduje ze nie wlaczamy symbolu wglSwapMultipleBuffers
  (ktorego nie ma np. w moim OpenGLu pod Windows 98 (NVidii,
  ale wgl chyba pochodzi z innej bibioteki a opengl32.dll tylko tu posredniczy ?)

  NO_GL_12 oznacza ze nie funkcji oznaczonych tutaj jako GL 1.2 :
  glDrawRangeElements glTexImage3D glBlendColor  glBlendEquation
  glColorSubTable glCopyColorSubTable glColorTable glCopyColorTable
  glColorTableParameteriv glColorTableParameterfv glGetColorTable
  glGetColorTableParameteriv glGetColorTableParameterfv glConvolutionFilter1D
  glConvolutionFilter2D glCopyConvolutionFilter1D glCopyConvolutionFilter2D
  glGetConvolutionFilter glSeparableFilter2D glGetSeparableFilter
  glConvolutionParameteri glConvolutionParameteriv glConvolutionParameterf
  glConvolutionParameterfv glGetConvolutionParameteriv glGetConvolutionParameterfv
  glHistogram glResetHistogram glGetHistogram glGetHistogramParameteriv
  glGetHistogramParameterfv glMinmax glResetMinmax glGetMinmax
  glGetMinmaxParameteriv glGetMinmaxParameterfv
}
{$ifdef WIN32}
  {$define OLDER_WGL}
  {$define NO_GL_12}
{$endif}

{$I kambiconf.inc}
{$I openglmac.inc}

interface

uses
  {$IFDEF WIN32}
    Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    Xlib, XlibUtils, {$ifdef FPC} X, XUtil, {$endif}
  {$ENDIF}
  KambiUtils,  VectorMath;

{$define read_interface}

type
  TGLenum     = LongWord; PGLenum     = ^TGLenum;
  TGLboolean  = byte;     PGLboolean  = ^TGLboolean;
  TGLbitfield = LongWord; PGLbitfield = ^TGLbitfield;
  TGLbyte     = ShortInt; PGLbyte     = ^TGLbyte;
  TGLshort    = SmallInt; PGLshort    = ^TGLshort;
  TGLint      = Integer;  PGLint      = ^TGLint;
  TGLsizei    = Integer;  PGLsizei    = ^TGLsizei;
  TGLubyte    = byte;     PGLubyte    = ^TGLubyte;
  TGLushort   = Word;     PGLushort   = ^TGLushort;
  TGLuint     = LongWord; PGLuint     = ^TGLuint;
  TGLfloat    = Single;   PGLfloat    = ^TGLfloat;
  TGLclampf   = Single;   PGLclampf   = ^TGLclampf;
  TGLdouble   = Double;   PGLdouble   = ^TGLdouble;
  TGLclampd   = Double;   PGLclampd   = ^TGLclampd;

{$I opengltypes.inc}

const
  OpenGLDLL =
    {$ifdef WIN32} 'opengl32.dll' {$endif}
    {$ifdef UNIX}
      {$ifdef DARWIN} 'libGL.dylib' { TODO--confirm this works under Darwin }
      {$else} 'libGL.so.1'
      {$endif}
    {$endif};
  GluDLL =
    {$ifdef WIN32} 'glu32.dll' {$endif}
    {$ifdef UNIX}
      {$ifdef DARWIN} 'libGLU.dylib' { TODO--confirm this works under Darwin }
      {$else} 'libGLU.so.1'
      {$endif}
    {$endif};

var
  GL_VERSION_1_0,
  GL_VERSION_1_1,
  GL_VERSION_1_2,
  GLU_VERSION_1_1,
  GLU_VERSION_1_2,
  GLU_VERSION_1_3: Boolean;

  { Extensions (gl) }
  GL_3DFX_multisample,
  GL_3DFX_tbuffer,
  GL_3DFX_texture_compression_FXT1,

  GL_APPLE_specular_vector,
  GL_APPLE_transform_hint,

  GL_ARB_imaging,
  GL_ARB_multisample,
  GL_ARB_multitexture,
  GL_ARB_texture_compression,
  GL_ARB_texture_cube_map,
  GL_ARB_transpose_matrix,
  GL_ARB_vertex_blend,

  GL_EXT_422_pixels,
  GL_EXT_abgr,
  GL_EXT_bgra,
  GL_EXT_blend_color,
  GL_EXT_blend_func_separate,
  GL_EXT_blend_logic_op,
  GL_EXT_blend_minmax,
  GL_EXT_blend_subtract,
  GL_EXT_clip_volume_hint,
  GL_EXT_cmyka,
  GL_EXT_color_subtable,
  GL_EXT_compiled_vertex_array,
  GL_EXT_convolution,
  GL_EXT_coordinate_frame,
  GL_EXT_copy_texture,
  GL_EXT_cull_vertex,
  GL_EXT_draw_range_elements,
  GL_EXT_fog_coord,
  GL_EXT_histogram,
  GL_EXT_index_array_formats,
  GL_EXT_index_func,
  GL_EXT_index_material,
  GL_EXT_index_texture,
  GL_EXT_light_max_exponent,
  GL_EXT_light_texture,
  GL_EXT_misc_attribute,
  GL_EXT_multi_draw_arrays,
  GL_EXT_multisample,
  GL_EXT_packed_pixels,
  GL_EXT_paletted_texture,
  GL_EXT_pixel_transform,
  GL_EXT_point_parameters,
  GL_EXT_polygon_offset,
  GL_EXT_rescale_normal,
  GL_EXT_scene_marker,
  GL_EXT_secondary_color,
  GL_EXT_separate_specular_color,
  GL_EXT_shared_texture_palette,
  GL_EXT_stencil_wrap,
  GL_EXT_subtexture,
  GL_EXT_texture_color_table,
  GL_EXT_texture_compression_s3tc,
  GL_EXT_texture_cube_map,
  GL_EXT_texture_edge_clamp,
  GL_EXT_texture_env_add,
  GL_EXT_texture_env_combine,
  GL_EXT_texture_filter_anisotropic,
  GL_EXT_texture_lod_bias,
  GL_EXT_texture_object,
  GL_EXT_texture_perturb_normal,
  GL_EXT_texture3D,
  GL_EXT_vertex_array,
  GL_EXT_vertex_weighting,

  GL_FfdMaskSGIX,
  GL_HP_convolution_border_modes,
  GL_HP_image_transform,
  GL_HP_occlusion_test,
  GL_HP_texture_lighting,

  GL_IBM_cull_vertex,
  GL_IBM_multimode_draw_arrays,
  GL_IBM_rasterpos_clip,
  GL_IBM_vertex_array_lists,

  GL_INGR_color_clamp,
  GL_INGR_interlace_read,

  GL_INTEL_parallel_arrays,

  GL_KTX_buffer_region,

  GL_MESA_resize_buffers,
  GL_MESA_window_pos,

  GL_NV_blend_square,
  GL_NV_fog_distance,
  GL_NV_light_max_exponent,
  GL_NV_register_combiners,
  GL_NV_texgen_emboss,
  GL_NV_texgen_reflection,
  GL_NV_texture_env_combine4,
  GL_NV_vertex_array_range,
  GL_NV_vertex_program,

  GL_PGI_misc_hints,
  GL_PGI_vertex_hints,

  GL_REND_screen_coordinates,

  GL_SGI_color_matrix,
  GL_SGI_color_table,
  GL_SGI_depth_pass_instrument,

  GL_SGIS_detail_texture,
  GL_SGIS_fog_function,
  GL_SGIS_generate_mipmap,
  GL_SGIS_multisample,
  GL_SGIS_multitexture,
  GL_SGIS_pixel_texture,
  GL_SGIS_point_line_texgen,
  GL_SGIS_point_parameters,
  GL_SGIS_sharpen_texture,
  GL_SGIS_texture_border_clamp,
  GL_SGIS_texture_color_mask,
  GL_SGIS_texture_edge_clamp,
  GL_SGIS_texture_filter4,
  GL_SGIS_texture_lod,
  GL_SGIS_texture_select,
  GL_SGIS_texture4D,

  GL_SGIX_async,
  GL_SGIX_async_histogram,
  GL_SGIX_async_pixel,
  GL_SGIX_blend_alpha_minmax,
  GL_SGIX_calligraphic_fragment,
  GL_SGIX_clipmap,
  GL_SGIX_convolution_accuracy,
  GL_SGIX_depth_texture,
  GL_SGIX_flush_raster,
  GL_SGIX_fog_offset,
  GL_SGIX_fog_scale,
  GL_SGIX_fragment_lighting,
  GL_SGIX_framezoom,
  GL_SGIX_igloo_interface,
  GL_SGIX_instruments,
  GL_SGIX_interlace,
  GL_SGIX_ir_instrument1,
  GL_SGIX_list_priority,
  GL_SGIX_pixel_texture,
  GL_SGIX_pixel_tiles,
  GL_SGIX_polynomial_ffd,
  GL_SGIX_reference_plane,
  GL_SGIX_resample,
  GL_SGIX_shadow,
  GL_SGIX_shadow_ambient,
  GL_SGIX_sprite,
  GL_SGIX_subsample,
  GL_SGIX_tag_sample_buffer,
  GL_SGIX_texture_add_env,
  GL_SGIX_texture_lod_bias,
  GL_SGIX_texture_multi_buffer,
  GL_SGIX_texture_scale_bias,
  GL_SGIX_vertex_preclip,
  GL_SGIX_ycrcb,
  GL_SGIX_ycrcba,

  GL_SUN_convolution_border_modes,
  GL_SUN_global_alpha,
  GL_SUN_triangle_list,
  GL_SUN_vertex,

  GL_SUNX_constant_data,

  GL_WIN_phong_shading,
  GL_WIN_specular_fog,
  GL_WIN_swap_hint,

  WGL_EXT_swap_control,

  { Extensions (glu) }
  GLU_EXT_Texture,
  GLU_EXT_object_space_tess,
  GLU_EXT_nurbs_tessellator: Boolean;

const
  { ********** GL generic constants ********** }

  { errors }
  GL_NO_ERROR                                       = 0;
  GL_INVALID_ENUM                                   = $0500;
  GL_INVALID_VALUE                                  = $0501;
  GL_INVALID_OPERATION                              = $0502;
  GL_STACK_OVERFLOW                                 = $0503;
  GL_STACK_UNDERFLOW                                = $0504;
  GL_OUT_OF_MEMORY                                  = $0505;

  { attribute bits }
  GL_CURRENT_BIT                                    = $00000001;
  GL_POINT_BIT                                      = $00000002;
  GL_LINE_BIT                                       = $00000004;
  GL_POLYGON_BIT                                    = $00000008;
  GL_POLYGON_STIPPLE_BIT                            = $00000010;
  GL_PIXEL_MODE_BIT                                 = $00000020;
  GL_LIGHTING_BIT                                   = $00000040;
  GL_FOG_BIT                                        = $00000080;
  GL_DEPTH_BUFFER_BIT                               = $00000100;
  GL_ACCUM_BUFFER_BIT                               = $00000200;
  GL_STENCIL_BUFFER_BIT                             = $00000400;
  GL_VIEWPORT_BIT                                   = $00000800;
  GL_TRANSFORM_BIT                                  = $00001000;
  GL_ENABLE_BIT                                     = $00002000;
  GL_COLOR_BUFFER_BIT                               = $00004000;
  GL_HINT_BIT                                       = $00008000;
  GL_EVAL_BIT                                       = $00010000;
  GL_LIST_BIT                                       = $00020000;
  GL_TEXTURE_BIT                                    = $00040000;
  GL_SCISSOR_BIT                                    = $00080000;
  GL_ALL_ATTRIB_BITS                                = $000FFFFF;

  { client attribute bits }
  GL_CLIENT_PIXEL_STORE_BIT                         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT                        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS                         = $FFFFFFFF;

  { boolean values }
  GL_FALSE                                          = 0;
  GL_TRUE                                           = 1;

  { primitives }
  GL_POINTS                                         = $0000;
  GL_LINES                                          = $0001;
  GL_LINE_LOOP                                      = $0002;
  GL_LINE_STRIP                                     = $0003;
  GL_TRIANGLES                                      = $0004;
  GL_TRIANGLE_STRIP                                 = $0005;
  GL_TRIANGLE_FAN                                   = $0006;
  GL_QUADS                                          = $0007;
  GL_QUAD_STRIP                                     = $0008;
  GL_POLYGON                                        = $0009;

  { blending }
  GL_ZERO                                           = 0;
  GL_ONE                                            = 1;
  GL_SRC_COLOR                                      = $0300;
  GL_ONE_MINUS_SRC_COLOR                            = $0301;
  GL_SRC_ALPHA                                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA                            = $0303;
  GL_DST_ALPHA                                      = $0304;
  GL_ONE_MINUS_DST_ALPHA                            = $0305;
  GL_DST_COLOR                                      = $0306;
  GL_ONE_MINUS_DST_COLOR                            = $0307;
  GL_SRC_ALPHA_SATURATE                             = $0308;
  GL_BLEND_DST                                      = $0BE0;
  GL_BLEND_SRC                                      = $0BE1;
  GL_BLEND                                          = $0BE2;

  { blending (GL 1.2 ARB imaging) }
  GL_BLEND_COLOR                                    = $8005;
  GL_CONSTANT_COLOR                                 = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR                       = $8002;
  GL_CONSTANT_ALPHA                                 = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA                       = $8004;
  GL_FUNC_ADD                                       = $8006;
  GL_MIN                                            = $8007;
  GL_MAX                                            = $8008;
  GL_FUNC_SUBTRACT                                  = $800A;
  GL_FUNC_REVERSE_SUBTRACT                          = $800B;

  { color table GL 1.2 ARB imaging }
  GL_COLOR_TABLE                                    = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE                   = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE                  = $80D2;
  GL_PROXY_COLOR_TABLE                              = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE             = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE            = $80D5;
  GL_COLOR_TABLE_SCALE                              = $80D6;
  GL_COLOR_TABLE_BIAS                               = $80D7;
  GL_COLOR_TABLE_FORMAT                             = $80D8;
  GL_COLOR_TABLE_WIDTH                              = $80D9;
  GL_COLOR_TABLE_RED_SIZE                           = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE                         = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE                          = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE                         = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE                     = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE                     = $80DF;

  { convolutions GL 1.2 ARB imaging }
  GL_CONVOLUTION_1D                                 = $8010;
  GL_CONVOLUTION_2D                                 = $8011;
  GL_SEPARABLE_2D                                   = $8012;
  GL_CONVOLUTION_BORDER_MODE                        = $8013;
  GL_CONVOLUTION_FILTER_SCALE                       = $8014;
  GL_CONVOLUTION_FILTER_BIAS                        = $8015;
  GL_REDUCE                                         = $8016;
  GL_CONVOLUTION_FORMAT                             = $8017;
  GL_CONVOLUTION_WIDTH                              = $8018;
  GL_CONVOLUTION_HEIGHT                             = $8019;
  GL_MAX_CONVOLUTION_WIDTH                          = $801A;
  GL_MAX_CONVOLUTION_HEIGHT                         = $801B;
  GL_POST_CONVOLUTION_RED_SCALE                     = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE                   = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE                    = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE                   = $801F;
  GL_POST_CONVOLUTION_RED_BIAS                      = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS                    = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS                     = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS                    = $8023;

  { histogram GL 1.2 ARB imaging }
  GL_HISTOGRAM                                      = $8024;
  GL_PROXY_HISTOGRAM                                = $8025;
  GL_HISTOGRAM_WIDTH                                = $8026;
  GL_HISTOGRAM_FORMAT                               = $8027;
  GL_HISTOGRAM_RED_SIZE                             = $8028;
  GL_HISTOGRAM_GREEN_SIZE                           = $8029;
  GL_HISTOGRAM_BLUE_SIZE                            = $802A;
  GL_HISTOGRAM_ALPHA_SIZE                           = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE                       = $802C;
  GL_HISTOGRAM_SINK                                 = $802D;
  GL_MINMAX                                         = $802E;
  GL_MINMAX_FORMAT                                  = $802F;
  GL_MINMAX_SINK                                    = $8030;

  { buffers }
  GL_NONE                                           = 0;
  GL_FRONT_LEFT                                     = $0400;
  GL_FRONT_RIGHT                                    = $0401;
  GL_BACK_LEFT                                      = $0402;
  GL_BACK_RIGHT                                     = $0403;
  GL_FRONT                                          = $0404;
  GL_BACK                                           = $0405;
  GL_LEFT                                           = $0406;
  GL_RIGHT                                          = $0407;
  GL_FRONT_AND_BACK                                 = $0408;
  GL_AUX0                                           = $0409;
  GL_AUX1                                           = $040A;
  GL_AUX2                                           = $040B;
  GL_AUX3                                           = $040C;
  GL_AUX_BUFFERS                                    = $0C00;
  GL_DRAW_BUFFER                                    = $0C01;
  GL_READ_BUFFER                                    = $0C02;
  GL_DOUBLEBUFFER                                   = $0C32;
  GL_STEREO                                         = $0C33;

  { depth buffer }
  GL_DEPTH_RANGE                                    = $0B70;
  GL_DEPTH_TEST                                     = $0B71;
  GL_DEPTH_WRITEMASK                                = $0B72;
  GL_DEPTH_CLEAR_VALUE                              = $0B73;
  GL_DEPTH_FUNC                                     = $0B74;
  GL_NEVER                                          = $0200;
  GL_LESS                                           = $0201;
  GL_EQUAL                                          = $0202;
  GL_LEQUAL                                         = $0203;
  GL_GREATER                                        = $0204;
  GL_NOTEQUAL                                       = $0205;
  GL_GEQUAL                                         = $0206;
  GL_ALWAYS                                         = $0207;

  { accumulation buffer }
  GL_ACCUM                                          = $0100;
  GL_LOAD                                           = $0101;
  GL_RETURN                                         = $0102;
  GL_MULT                                           = $0103;
  GL_ADD                                            = $0104;
  GL_ACCUM_CLEAR_VALUE                              = $0B80;

  { feedback buffer }
  GL_FEEDBACK_BUFFER_POINTER                        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE                           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE                           = $0DF2;

  { feedback types }
  GL_2D                                             = $0600;
  GL_3D                                             = $0601;
  GL_3D_COLOR                                       = $0602;
  GL_3D_COLOR_TEXTURE                               = $0603;
  GL_4D_COLOR_TEXTURE                               = $0604;

  { feedback tokens }
  GL_PASS_THROUGH_TOKEN                             = $0700;
  GL_POINT_TOKEN                                    = $0701;
  GL_LINE_TOKEN                                     = $0702;
  GL_POLYGON_TOKEN                                  = $0703;
  GL_BITMAP_TOKEN                                   = $0704;
  GL_DRAW_PIXEL_TOKEN                               = $0705;
  GL_COPY_PIXEL_TOKEN                               = $0706;
  GL_LINE_RESET_TOKEN                               = $0707;

  { fog }
  GL_EXP                                            = $0800;
  GL_EXP2                                           = $0801;
  GL_FOG                                            = $0B60;
  GL_FOG_INDEX                                      = $0B61;
  GL_FOG_DENSITY                                    = $0B62;
  GL_FOG_START                                      = $0B63;
  GL_FOG_END                                        = $0B64;
  GL_FOG_MODE                                       = $0B65;
  GL_FOG_COLOR                                      = $0B66;

  { pixel mode, transfer }
  GL_PIXEL_MAP_I_TO_I                               = $0C70;
  GL_PIXEL_MAP_S_TO_S                               = $0C71;
  GL_PIXEL_MAP_I_TO_R                               = $0C72;
  GL_PIXEL_MAP_I_TO_G                               = $0C73;
  GL_PIXEL_MAP_I_TO_B                               = $0C74;
  GL_PIXEL_MAP_I_TO_A                               = $0C75;
  GL_PIXEL_MAP_R_TO_R                               = $0C76;
  GL_PIXEL_MAP_G_TO_G                               = $0C77;
  GL_PIXEL_MAP_B_TO_B                               = $0C78;
  GL_PIXEL_MAP_A_TO_A                               = $0C79;

  { vertex arrays }
  GL_VERTEX_ARRAY_POINTER                           = $808E;
  GL_NORMAL_ARRAY_POINTER                           = $808F;
  GL_COLOR_ARRAY_POINTER                            = $8090;
  GL_INDEX_ARRAY_POINTER                            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER                    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER                        = $8093;

  { stenciling }
  GL_STENCIL_TEST                                   = $0B90;
  GL_STENCIL_CLEAR_VALUE                            = $0B91;
  GL_STENCIL_FUNC                                   = $0B92;
  GL_STENCIL_VALUE_MASK                             = $0B93;
  GL_STENCIL_FAIL                                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL                        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS                        = $0B96;
  GL_STENCIL_REF                                    = $0B97;
  GL_STENCIL_WRITEMASK                              = $0B98;
  GL_KEEP                                           = $1E00;
  GL_REPLACE                                        = $1E01;
  GL_INCR                                           = $1E02;
  GL_DECR                                           = $1E03;

  { color material }
  GL_COLOR_MATERIAL_FACE                            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER                       = $0B56;
  GL_COLOR_MATERIAL                                 = $0B57;

  { points }
  GL_POINT_SMOOTH                                   = $0B10;
  GL_POINT_SIZE                                     = $0B11;
  GL_POINT_SIZE_RANGE                               = $0B12;
  GL_POINT_SIZE_GRANULARITY                         = $0B13;

  { lines }
  GL_LINE_SMOOTH                                    = $0B20;
  GL_LINE_WIDTH                                     = $0B21;
  GL_LINE_WIDTH_RANGE                               = $0B22;
  GL_LINE_WIDTH_GRANULARITY                         = $0B23;
  GL_LINE_STIPPLE                                   = $0B24;
  GL_LINE_STIPPLE_PATTERN                           = $0B25;
  GL_LINE_STIPPLE_REPEAT                            = $0B26;

  { polygons }
  GL_POLYGON_MODE                                   = $0B40;
  GL_POLYGON_SMOOTH                                 = $0B41;
  GL_POLYGON_STIPPLE                                = $0B42;
  GL_EDGE_FLAG                                      = $0B43;
  GL_CULL_FACE                                      = $0B44;
  GL_CULL_FACE_MODE                                 = $0B45;
  GL_FRONT_FACE                                     = $0B46;
  GL_CW                                             = $0900;
  GL_CCW                                            = $0901;
  GL_POINT                                          = $1B00;
  GL_LINE                                           = $1B01;
  GL_FILL                                           = $1B02;

  { display lists }
  GL_LIST_MODE                                      = $0B30;
  GL_LIST_BASE                                      = $0B32;
  GL_LIST_INDEX                                     = $0B33;
  GL_COMPILE                                        = $1300;
  GL_COMPILE_AND_EXECUTE                            = $1301;

  { lighting }
  GL_LIGHTING                                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER                       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE                           = $0B52;
  GL_LIGHT_MODEL_AMBIENT                            = $0B53;
  GL_LIGHT_MODEL_COLOR_CONTROL                      = $81F8; { GL 1.2 }
  GL_SHADE_MODEL                                    = $0B54;
  GL_NORMALIZE                                      = $0BA1;
  GL_AMBIENT                                        = $1200;
  GL_DIFFUSE                                        = $1201;
  GL_SPECULAR                                       = $1202;
  GL_POSITION                                       = $1203;
  GL_SPOT_DIRECTION                                 = $1204;
  GL_SPOT_EXPONENT                                  = $1205;
  GL_SPOT_CUTOFF                                    = $1206;
  GL_CONSTANT_ATTENUATION                           = $1207;
  GL_LINEAR_ATTENUATION                             = $1208;
  GL_QUADRATIC_ATTENUATION                          = $1209;
  GL_EMISSION                                       = $1600;
  GL_SHININESS                                      = $1601;
  GL_AMBIENT_AND_DIFFUSE                            = $1602;
  GL_COLOR_INDEXES                                  = $1603;
  GL_FLAT                                           = $1D00;
  GL_SMOOTH                                         = $1D01;
  GL_LIGHT0                                         = $4000;
  GL_LIGHT1                                         = $4001;
  GL_LIGHT2                                         = $4002;
  GL_LIGHT3                                         = $4003;
  GL_LIGHT4                                         = $4004;
  GL_LIGHT5                                         = $4005;
  GL_LIGHT6                                         = $4006;
  GL_LIGHT7                                         = $4007;

  { matrix modes }
  GL_MATRIX_MODE                                    = $0BA0;
  GL_MODELVIEW                                      = $1700;
  GL_PROJECTION                                     = $1701;
  GL_TEXTURE                                        = $1702;

  { gets }
  GL_CURRENT_COLOR                                  = $0B00;
  GL_CURRENT_INDEX                                  = $0B01;
  GL_CURRENT_NORMAL                                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS                         = $0B03;
  GL_CURRENT_RASTER_COLOR                           = $0B04;
  GL_CURRENT_RASTER_INDEX                           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS                  = $0B06;
  GL_CURRENT_RASTER_POSITION                        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID                  = $0B08;
  GL_CURRENT_RASTER_DISTANCE                        = $0B09;
  GL_MAX_LIST_NESTING                               = $0B31;
  GL_VIEWPORT                                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH                          = $0BA3;
  GL_PROJECTION_STACK_DEPTH                         = $0BA4;
  GL_TEXTURE_STACK_DEPTH                            = $0BA5;
  GL_MODELVIEW_MATRIX                               = $0BA6;
  GL_PROJECTION_MATRIX                              = $0BA7;
  GL_TEXTURE_MATRIX                                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH                             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH                      = $0BB1;

  GL_SINGLE_COLOR                                   = $81F9; { GL 1.2 }
  GL_SEPARATE_SPECULAR_COLOR                        = $81FA; { GL 1.2 }

  { alpha testing }
  GL_ALPHA_TEST                                     = $0BC0;
  GL_ALPHA_TEST_FUNC                                = $0BC1;
  GL_ALPHA_TEST_REF                                 = $0BC2;

  GL_LOGIC_OP_MODE                                  = $0BF0;
  GL_INDEX_LOGIC_OP                                 = $0BF1;
  GL_LOGIC_OP                                       = $0BF1;
  GL_COLOR_LOGIC_OP                                 = $0BF2;
  GL_SCISSOR_BOX                                    = $0C10;
  GL_SCISSOR_TEST                                   = $0C11;
  GL_INDEX_CLEAR_VALUE                              = $0C20;
  GL_INDEX_WRITEMASK                                = $0C21;
  GL_COLOR_CLEAR_VALUE                              = $0C22;
  GL_COLOR_WRITEMASK                                = $0C23;
  GL_INDEX_MODE                                     = $0C30;
  GL_RGBA_MODE                                      = $0C31;
  GL_RENDER_MODE                                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT                    = $0C50;
  GL_POINT_SMOOTH_HINT                              = $0C51;
  GL_LINE_SMOOTH_HINT                               = $0C52;
  GL_POLYGON_SMOOTH_HINT                            = $0C53;
  GL_FOG_HINT                                       = $0C54;
  GL_TEXTURE_GEN_S                                  = $0C60;
  GL_TEXTURE_GEN_T                                  = $0C61;
  GL_TEXTURE_GEN_R                                  = $0C62;
  GL_TEXTURE_GEN_Q                                  = $0C63;
  GL_PIXEL_MAP_I_TO_I_SIZE                          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE                          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE                          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE                          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE                          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE                          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE                          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE                          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE                          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE                          = $0CB9;
  GL_UNPACK_SWAP_BYTES                              = $0CF0;
  GL_UNPACK_LSB_FIRST                               = $0CF1;
  GL_UNPACK_ROW_LENGTH                              = $0CF2;
  GL_UNPACK_SKIP_ROWS                               = $0CF3;
  GL_UNPACK_SKIP_PIXELS                             = $0CF4;
  GL_UNPACK_ALIGNMENT                               = $0CF5;
  GL_PACK_SWAP_BYTES                                = $0D00;
  GL_PACK_LSB_FIRST                                 = $0D01;
  GL_PACK_ROW_LENGTH                                = $0D02;
  GL_PACK_SKIP_ROWS                                 = $0D03;
  GL_PACK_SKIP_PIXELS                               = $0D04;
  GL_PACK_ALIGNMENT                                 = $0D05;
  GL_PACK_SKIP_IMAGES                               = $806B; { GL 1.2 }
  GL_PACK_IMAGE_HEIGHT                              = $806C; { GL 1.2 }
  GL_UNPACK_SKIP_IMAGES                             = $806D; { GL 1.2 }
  GL_UNPACK_IMAGE_HEIGHT                            = $806E; { GL 1.2 }
  GL_MAP_COLOR                                      = $0D10;
  GL_MAP_STENCIL                                    = $0D11;
  GL_INDEX_SHIFT                                    = $0D12;
  GL_INDEX_OFFSET                                   = $0D13;
  GL_RED_SCALE                                      = $0D14;
  GL_RED_BIAS                                       = $0D15;
  GL_ZOOM_X                                         = $0D16;
  GL_ZOOM_Y                                         = $0D17;
  GL_GREEN_SCALE                                    = $0D18;
  GL_GREEN_BIAS                                     = $0D19;
  GL_BLUE_SCALE                                     = $0D1A;
  GL_BLUE_BIAS                                      = $0D1B;
  GL_ALPHA_SCALE                                    = $0D1C;
  GL_ALPHA_BIAS                                     = $0D1D;
  GL_DEPTH_SCALE                                    = $0D1E;
  GL_DEPTH_BIAS                                     = $0D1F;
  GL_MAX_EVAL_ORDER                                 = $0D30;
  GL_MAX_LIGHTS                                     = $0D31;
  GL_MAX_CLIP_PLANES                                = $0D32;
  GL_MAX_TEXTURE_SIZE                               = $0D33;
  GL_MAX_3D_TEXTURE_SIZE                            = $8073; { GL 1.2 }
  GL_MAX_PIXEL_MAP_TABLE                            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH                         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH                      = $0D36;
  GL_MAX_NAME_STACK_DEPTH                           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH                     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH                        = $0D39;
  GL_MAX_VIEWPORT_DIMS                              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH                  = $0D3B;
  GL_MAX_ELEMENTS_VERTICES                          = $80E8; { GL 1.2 }
  GL_MAX_ELEMENTS_INDICES                           = $80E9; { GL 1.2   }
  GL_RESCALE_NORMAL                                 = $803A; { GL 1.2 }
  GL_SUBPIXEL_BITS                                  = $0D50;
  GL_INDEX_BITS                                     = $0D51;
  GL_RED_BITS                                       = $0D52;
  GL_GREEN_BITS                                     = $0D53;
  GL_BLUE_BITS                                      = $0D54;
  GL_ALPHA_BITS                                     = $0D55;
  GL_DEPTH_BITS                                     = $0D56;
  GL_STENCIL_BITS                                   = $0D57;
  GL_ACCUM_RED_BITS                                 = $0D58;
  GL_ACCUM_GREEN_BITS                               = $0D59;
  GL_ACCUM_BLUE_BITS                                = $0D5A;
  GL_ACCUM_ALPHA_BITS                               = $0D5B;
  GL_NAME_STACK_DEPTH                               = $0D70;
  GL_AUTO_NORMAL                                    = $0D80;
  GL_MAP1_COLOR_4                                   = $0D90;
  GL_MAP1_INDEX                                     = $0D91;
  GL_MAP1_NORMAL                                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1                           = $0D93;
  GL_MAP1_TEXTURE_COORD_2                           = $0D94;
  GL_MAP1_TEXTURE_COORD_3                           = $0D95;
  GL_MAP1_TEXTURE_COORD_4                           = $0D96;
  GL_MAP1_VERTEX_3                                  = $0D97;
  GL_MAP1_VERTEX_4                                  = $0D98;
  GL_MAP2_COLOR_4                                   = $0DB0;
  GL_MAP2_INDEX                                     = $0DB1;
  GL_MAP2_NORMAL                                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1                           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2                           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3                           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4                           = $0DB6;
  GL_MAP2_VERTEX_3                                  = $0DB7;
  GL_MAP2_VERTEX_4                                  = $0DB8;
  GL_MAP1_GRID_DOMAIN                               = $0DD0;
  GL_MAP1_GRID_SEGMENTS                             = $0DD1;
  GL_MAP2_GRID_DOMAIN                               = $0DD2;
  GL_MAP2_GRID_SEGMENTS                             = $0DD3;
  GL_TEXTURE_1D                                     = $0DE0;
  GL_TEXTURE_2D                                     = $0DE1;
  GL_TEXTURE_3D                                     = $806F; { GL 1.2 }
  GL_SELECTION_BUFFER_POINTER                       = $0DF3;
  GL_SELECTION_BUFFER_SIZE                          = $0DF4;
  GL_POLYGON_OFFSET_UNITS                           = $2A00;
  GL_POLYGON_OFFSET_POINT                           = $2A01;
  GL_POLYGON_OFFSET_LINE                            = $2A02;
  GL_POLYGON_OFFSET_FILL                            = $8037;
  GL_POLYGON_OFFSET_FACTOR                          = $8038;
  GL_TEXTURE_BINDING_1D                             = $8068;
  GL_TEXTURE_BINDING_2D                             = $8069;
  GL_VERTEX_ARRAY                                   = $8074;
  GL_NORMAL_ARRAY                                   = $8075;
  GL_COLOR_ARRAY                                    = $8076;
  GL_INDEX_ARRAY                                    = $8077;
  GL_TEXTURE_COORD_ARRAY                            = $8078;
  GL_EDGE_FLAG_ARRAY                                = $8079;
  GL_VERTEX_ARRAY_SIZE                              = $807A;
  GL_VERTEX_ARRAY_TYPE                              = $807B;
  GL_VERTEX_ARRAY_STRIDE                            = $807C;
  GL_NORMAL_ARRAY_TYPE                              = $807E;
  GL_NORMAL_ARRAY_STRIDE                            = $807F;
  GL_COLOR_ARRAY_SIZE                               = $8081;
  GL_COLOR_ARRAY_TYPE                               = $8082;
  GL_COLOR_ARRAY_STRIDE                             = $8083;
  GL_INDEX_ARRAY_TYPE                               = $8085;
  GL_INDEX_ARRAY_STRIDE                             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE                       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE                       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE                     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE                         = $808C;
  GL_COLOR_MATRIX                                   = $80B1; { GL 1.2 ARB imaging }
  GL_COLOR_MATRIX_STACK_DEPTH                       = $80B2; { GL 1.2 ARB imaging }
  GL_MAX_COLOR_MATRIX_STACK_DEPTH                   = $80B3; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_RED_SCALE                    = $80B4; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_GREEN_SCALE                  = $80B5; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_BLUE_SCALE                   = $80B6; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_ALPHA_SCALE                  = $80B7; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_RED_BIAS                     = $80B8; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_GREEN_BIAS                   = $80B9; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_BLUE_BIAS                    = $80BA; { GL 1.2 ARB imaging }
  GL_POST_COLOR_MATRIX_ALPHA_BIAS                   = $80BB; { GL 1.2 ARB imaging }

  { evaluators }
  GL_COEFF                                          = $0A00;
  GL_ORDER                                          = $0A01;
  GL_DOMAIN                                         = $0A02;

  { texture mapping }
  GL_TEXTURE_WIDTH                                  = $1000;
  GL_TEXTURE_HEIGHT                                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT                        = $1003;
  GL_TEXTURE_COMPONENTS                             = $1003;
  GL_TEXTURE_BORDER_COLOR                           = $1004;
  GL_TEXTURE_BORDER                                 = $1005;
  GL_TEXTURE_RED_SIZE                               = $805C;
  GL_TEXTURE_GREEN_SIZE                             = $805D;
  GL_TEXTURE_BLUE_SIZE                              = $805E;
  GL_TEXTURE_ALPHA_SIZE                             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE                         = $8060;
  GL_TEXTURE_INTENSITY_SIZE                         = $8061;
  GL_TEXTURE_PRIORITY                               = $8066;
  GL_TEXTURE_RESIDENT                               = $8067;
  GL_BGR                                            = $80E0; { v 1.2 }
  GL_BGRA                                           = $80E1; { v 1.2 }
  GL_S                                              = $2000;
  GL_T                                              = $2001;
  GL_R                                              = $2002;
  GL_Q                                              = $2003;
  GL_MODULATE                                       = $2100;
  GL_DECAL                                          = $2101;
  GL_TEXTURE_ENV_MODE                               = $2200;
  GL_TEXTURE_ENV_COLOR                              = $2201;
  GL_TEXTURE_ENV                                    = $2300;
  GL_EYE_LINEAR                                     = $2400;
  GL_OBJECT_LINEAR                                  = $2401;
  GL_SPHERE_MAP                                     = $2402;
  GL_TEXTURE_GEN_MODE                               = $2500;
  GL_OBJECT_PLANE                                   = $2501;
  GL_EYE_PLANE                                      = $2502;
  GL_NEAREST                                        = $2600;
  GL_LINEAR                                         = $2601;
  GL_NEAREST_MIPMAP_NEAREST                         = $2700;
  GL_LINEAR_MIPMAP_NEAREST                          = $2701;
  GL_NEAREST_MIPMAP_LINEAR                          = $2702;
  GL_LINEAR_MIPMAP_LINEAR                           = $2703;
  GL_TEXTURE_MAG_FILTER                             = $2800;
  GL_TEXTURE_MIN_FILTER                             = $2801;
  GL_TEXTURE_WRAP_R                                 = $8072; { GL 1.2 }
  GL_TEXTURE_WRAP_S                                 = $2802;
  GL_TEXTURE_WRAP_T                                 = $2803;
  GL_CLAMP_TO_EDGE                                  = $812F; { GL 1.2 }
  GL_TEXTURE_MIN_LOD                                = $813A; { GL 1.2 }
  GL_TEXTURE_MAX_LOD                                = $813B; { GL 1.2 }
  GL_TEXTURE_BASE_LEVEL                             = $813C; { GL 1.2 }
  GL_TEXTURE_MAX_LEVEL                              = $813D; { GL 1.2 }
  GL_TEXTURE_DEPTH                                  = $8071; { GL 1.2 }
  GL_PROXY_TEXTURE_1D                               = $8063;
  GL_PROXY_TEXTURE_2D                               = $8064;
  GL_PROXY_TEXTURE_3D                               = $8070; { GL 1.2 }
  GL_CLAMP                                          = $2900;
  GL_REPEAT                                         = $2901;

  { hints }
  GL_DONT_CARE                                      = $1100;
  GL_FASTEST                                        = $1101;
  GL_NICEST                                         = $1102;

  { data types }
  GL_BYTE                                           = $1400;
  GL_UNSIGNED_BYTE                                  = $1401;
  GL_SHORT                                          = $1402;
  GL_UNSIGNED_SHORT                                 = $1403;
  GL_INT                                            = $1404;
  GL_UNSIGNED_INT                                   = $1405;
  GL_FLOAT                                          = $1406;
  GL_2_BYTES                                        = $1407;
  GL_3_BYTES                                        = $1408;
  GL_4_BYTES                                        = $1409;
  GL_DOUBLE                                         = $140A;
  GL_DOUBLE_EXT                                     = $140A;

  { logic operations }
  GL_CLEAR                                          = $1500;
  GL_AND                                            = $1501;
  GL_AND_REVERSE                                    = $1502;
  GL_COPY                                           = $1503;
  GL_AND_INVERTED                                   = $1504;
  GL_NOOP                                           = $1505;
  GL_XOR                                            = $1506;
  GL_OR                                             = $1507;
  GL_NOR                                            = $1508;
  GL_EQUIV                                          = $1509;
  GL_INVERT                                         = $150A;
  GL_OR_REVERSE                                     = $150B;
  GL_COPY_INVERTED                                  = $150C;
  GL_OR_INVERTED                                    = $150D;
  GL_NAND                                           = $150E;
  GL_SET                                            = $150F;

  { PixelCopyType }
  GL_COLOR                                          = $1800;
  GL_DEPTH                                          = $1801;
  GL_STENCIL                                        = $1802;

  { pixel formats }
  GL_COLOR_INDEX                                    = $1900;
  GL_STENCIL_INDEX                                  = $1901;
  GL_DEPTH_COMPONENT                                = $1902;
  GL_RED                                            = $1903;
  GL_GREEN                                          = $1904;
  GL_BLUE                                           = $1905;
  GL_ALPHA                                          = $1906;
  GL_RGB                                            = $1907;
  GL_RGBA                                           = $1908;
  GL_LUMINANCE                                      = $1909;
  GL_LUMINANCE_ALPHA                                = $190A;

  { pixel type }
  GL_BITMAP                                         = $1A00;

  { rendering modes }
  GL_RENDER                                         = $1C00;
  GL_FEEDBACK                                       = $1C01;
  GL_SELECT                                         = $1C02;

  { implementation strings }
  GL_VENDOR                                         = $1F00;
  GL_RENDERER                                       = $1F01;
  GL_VERSION                                        = $1F02;
  GL_EXTENSIONS                                     = $1F03;

  { pixel formats }
  GL_R3_G3_B2                                       = $2A10;
  GL_ALPHA4                                         = $803B;
  GL_ALPHA8                                         = $803C;
  GL_ALPHA12                                        = $803D;
  GL_ALPHA16                                        = $803E;
  GL_LUMINANCE4                                     = $803F;
  GL_LUMINANCE8                                     = $8040;
  GL_LUMINANCE12                                    = $8041;
  GL_LUMINANCE16                                    = $8042;
  GL_LUMINANCE4_ALPHA4                              = $8043;
  GL_LUMINANCE6_ALPHA2                              = $8044;
  GL_LUMINANCE8_ALPHA8                              = $8045;
  GL_LUMINANCE12_ALPHA4                             = $8046;
  GL_LUMINANCE12_ALPHA12                            = $8047;
  GL_LUMINANCE16_ALPHA16                            = $8048;
  GL_INTENSITY                                      = $8049;
  GL_INTENSITY4                                     = $804A;
  GL_INTENSITY8                                     = $804B;
  GL_INTENSITY12                                    = $804C;
  GL_INTENSITY16                                    = $804D;
  GL_RGB4                                           = $804F;
  GL_RGB5                                           = $8050;
  GL_RGB8                                           = $8051;
  GL_RGB10                                          = $8052;
  GL_RGB12                                          = $8053;
  GL_RGB16                                          = $8054;
  GL_RGBA2                                          = $8055;
  GL_RGBA4                                          = $8056;
  GL_RGB5_A1                                        = $8057;
  GL_RGBA8                                          = $8058;
  GL_RGB10_A2                                       = $8059;
  GL_RGBA12                                         = $805A;
  GL_RGBA16                                         = $805B;
  UNSIGNED_BYTE_3_3_2                               = $8032; { GL 1.2 }
  UNSIGNED_BYTE_2_3_3_REV                           = $8362; { GL 1.2 }
  UNSIGNED_SHORT_5_6_5                              = $8363; { GL 1.2 }
  UNSIGNED_SHORT_5_6_5_REV                          = $8364; { GL 1.2 }
  UNSIGNED_SHORT_4_4_4_4                            = $8033; { GL 1.2 }
  UNSIGNED_SHORT_4_4_4_4_REV                        = $8365; { GL 1.2 }
  UNSIGNED_SHORT_5_5_5_1                            = $8034; { GL 1.2 }
  UNSIGNED_SHORT_1_5_5_5_REV                        = $8366; { GL 1.2 }
  UNSIGNED_INT_8_8_8_8                              = $8035; { GL 1.2 }
  UNSIGNED_INT_8_8_8_8_REV                          = $8367; { GL 1.2 }
  UNSIGNED_INT_10_10_10_2                           = $8036; { GL 1.2 }
  UNSIGNED_INT_2_10_10_10_REV                       = $8368; { GL 1.2 }

  { interleaved arrays formats }
  GL_V2F                                            = $2A20;
  GL_V3F                                            = $2A21;
  GL_C4UB_V2F                                       = $2A22;
  GL_C4UB_V3F                                       = $2A23;
  GL_C3F_V3F                                        = $2A24;
  GL_N3F_V3F                                        = $2A25;
  GL_C4F_N3F_V3F                                    = $2A26;
  GL_T2F_V3F                                        = $2A27;
  GL_T4F_V4F                                        = $2A28;
  GL_T2F_C4UB_V3F                                   = $2A29;
  GL_T2F_C3F_V3F                                    = $2A2A;
  GL_T2F_N3F_V3F                                    = $2A2B;
  GL_T2F_C4F_N3F_V3F                                = $2A2C;
  GL_T4F_C4F_N3F_V4F                                = $2A2D;

  { clip planes }
  GL_CLIP_PLANE0                                    = $3000;
  GL_CLIP_PLANE1                                    = $3001;
  GL_CLIP_PLANE2                                    = $3002;
  GL_CLIP_PLANE3                                    = $3003;
  GL_CLIP_PLANE4                                    = $3004;
  GL_CLIP_PLANE5                                    = $3005;

  { miscellaneous }
  GL_DITHER                                         = $0BD0;

  { ----- extensions enumerants ----- }
  { EXT_abgr }
  GL_ABGR_EXT                                       = $8000;

  { EXT_packed_pixels }
  GL_UNSIGNED_BYTE_3_3_2_EXT                        = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT                     = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT                     = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT                       = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT                    = $8036;

  { EXT_vertex_array }
  GL_VERTEX_ARRAY_EXT                               = $8074;
  GL_NORMAL_ARRAY_EXT                               = $8075;
  GL_COLOR_ARRAY_EXT                                = $8076;
  GL_INDEX_ARRAY_EXT                                = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT                        = $8078;
  GL_EDGE_FLAG_ARRAY_EXT                            = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT                          = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT                          = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT                        = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT                         = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT                          = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT                        = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT                         = $8080;
  GL_COLOR_ARRAY_SIZE_EXT                           = $8081;
  GL_COLOR_ARRAY_TYPE_EXT                           = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT                         = $8083;
  GL_COLOR_ARRAY_COUNT_EXT                          = $8084;
  GL_INDEX_ARRAY_TYPE_EXT                           = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT                         = $8086;
  GL_INDEX_ARRAY_COUNT_EXT                          = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT                   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT                   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT                 = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT                  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT                     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT                      = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT                       = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT                       = $808F;
  GL_COLOR_ARRAY_POINTER_EXT                        = $8090;
  GL_INDEX_ARRAY_POINTER_EXT                        = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT                = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT                    = $8093;

  { EXT_color_table }
  GL_TABLE_TOO_LARGE_EXT                            = $8031;
  GL_COLOR_TABLE_EXT                                = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_EXT               = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_EXT              = $80D2;
  GL_PROXY_COLOR_TABLE_EXT                          = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_EXT         = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_EXT        = $80D5;
  GL_COLOR_TABLE_SCALE_EXT                          = $80D6;
  GL_COLOR_TABLE_BIAS_EXT                           = $80D7;
  GL_COLOR_TABLE_FORMAT_EXT                         = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT                          = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT                       = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT                     = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT                      = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT                     = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT                 = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT                 = $80DF;

  { EXT_bgra }
  GL_BGR_EXT                                        = $80E0;
  GL_BGRA_EXT                                       = $80E1;

  { EXT_paletted_texture }
  GL_COLOR_INDEX1_EXT                               = $80E2;
  GL_COLOR_INDEX2_EXT                               = $80E3;
  GL_COLOR_INDEX4_EXT                               = $80E4;
  GL_COLOR_INDEX8_EXT                               = $80E5;
  GL_COLOR_INDEX12_EXT                              = $80E6;
  GL_COLOR_INDEX16_EXT                              = $80E7;

  { EXT_blend_color }
  GL_CONSTANT_COLOR_EXT                             = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT                   = $8002;
  GL_CONSTANT_ALPHA_EXT                             = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT                   = $8004;
  GL_BLEND_COLOR_EXT                                = $8005;

  { EXT_blend_minmax }
  GL_FUNC_ADD_EXT                                   = $8006;
  GL_MIN_EXT                                        = $8007;
  GL_MAX_EXT                                        = $8008;
  GL_BLEND_EQUATION_EXT                             = $8009;

  { EXT_blend_subtract }
  GL_FUNC_SUBTRACT_EXT                              = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT                      = $800B;

  { EXT_convolution }
  GL_CONVOLUTION_1D_EXT                             = $8010;
  GL_CONVOLUTION_2D_EXT                             = $8011;
  GL_SEPARABLE_2D_EXT                               = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT                    = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT                   = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT                    = $8015;
  GL_REDUCE_EXT                                     = $8016;
  GL_CONVOLUTION_FORMAT_EXT                         = $8017;
  GL_CONVOLUTION_WIDTH_EXT                          = $8018;
  GL_CONVOLUTION_HEIGHT_EXT                         = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT                      = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT                     = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT                 = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT               = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT                = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT               = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT                  = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT                = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT                 = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT                = $8023;

  { EXT_histogram }
  GL_HISTOGRAM_EXT                                  = $8024;
  GL_PROXY_HISTOGRAM_EXT                            = $8025;
  GL_HISTOGRAM_WIDTH_EXT                            = $8026;
  GL_HISTOGRAM_FORMAT_EXT                           = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT                         = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT                       = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT                        = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT                       = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT                   = $802C;
  GL_HISTOGRAM_SINK_EXT                             = $802D;
  GL_MINMAX_EXT                                     = $802E;
  GL_MINMAX_FORMAT_EXT                              = $802F;
  GL_MINMAX_SINK_EXT                                = $8030;

  { EXT_polygon_offset }
  GL_POLYGON_OFFSET_EXT                             = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT                      = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT                        = $8039;

  { EXT_texture }
  GL_ALPHA4_EXT                                     = $803B;
  GL_ALPHA8_EXT                                     = $803C;
  GL_ALPHA12_EXT                                    = $803D;
  GL_ALPHA16_EXT                                    = $803E;
  GL_LUMINANCE4_EXT                                 = $803F;
  GL_LUMINANCE8_EXT                                 = $8040;
  GL_LUMINANCE12_EXT                                = $8041;
  GL_LUMINANCE16_EXT                                = $8042;
  GL_LUMINANCE4_ALPHA4_EXT                          = $8043;
  GL_LUMINANCE6_ALPHA2_EXT                          = $8044;
  GL_LUMINANCE8_ALPHA8_EXT                          = $8045;
  GL_LUMINANCE12_ALPHA4_EXT                         = $8046;
  GL_LUMINANCE12_ALPHA12_EXT                        = $8047;
  GL_LUMINANCE16_ALPHA16_EXT                        = $8048;
  GL_INTENSITY_EXT                                  = $8049;
  GL_INTENSITY4_EXT                                 = $804A;
  GL_INTENSITY8_EXT                                 = $804B;
  GL_INTENSITY12_EXT                                = $804C;
  GL_INTENSITY16_EXT                                = $804D;
  GL_RGB2_EXT                                       = $804E;
  GL_RGB4_EXT                                       = $804F;
  GL_RGB5_EXT                                       = $8050;
  GL_RGB8_EXT                                       = $8051;
  GL_RGB10_EXT                                      = $8052;
  GL_RGB12_EXT                                      = $8053;
  GL_RGB16_EXT                                      = $8054;
  GL_RGBA2_EXT                                      = $8055;
  GL_RGBA4_EXT                                      = $8056;
  GL_RGB5_A1_EXT                                    = $8057;
  GL_RGBA8_EXT                                      = $8058;
  GL_RGB10_A2_EXT                                   = $8059;
  GL_RGBA12_EXT                                     = $805A;
  GL_RGBA16_EXT                                     = $805B;
  GL_TEXTURE_RED_SIZE_EXT                           = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT                         = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT                          = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT                         = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT                     = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT                     = $8061;
  GL_REPLACE_EXT                                    = $8062;
  GL_PROXY_TEXTURE_1D_EXT                           = $8063;
  GL_PROXY_TEXTURE_2D_EXT                           = $8064;
  GL_TEXTURE_TOO_LARGE_EXT                          = $8065;

  { EXT_texture_object }
  GL_TEXTURE_PRIORITY_EXT                           = $8066;
  GL_TEXTURE_RESIDENT_EXT                           = $8067;
  GL_TEXTURE_1D_BINDING_EXT                         = $8068;
  GL_TEXTURE_2D_BINDING_EXT                         = $8069;
  GL_TEXTURE_3D_BINDING_EXT                         = $806A;

  { EXT_texture3D }
  GL_PACK_SKIP_IMAGES_EXT                           = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT                          = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT                         = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT                        = $806E;
  GL_TEXTURE_3D_EXT                                 = $806F;
  GL_PROXY_TEXTURE_3D_EXT                           = $8070;
  GL_TEXTURE_DEPTH_EXT                              = $8071;
  GL_TEXTURE_WRAP_R_EXT                             = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT                        = $8073;

  { SGI_color_matrix }
  GL_COLOR_MATRIX_SGI                               = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI                   = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI               = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI                = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI              = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI               = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI              = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI                 = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI               = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI                = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI               = $80BB;

  { SGI_texture_color_table }
  GL_TEXTURE_COLOR_TABLE_SGI                        = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI                  = $80BD;
  GL_TEXTURE_COLOR_TABLE_BIAS_SGI                   = $80BE;
  GL_TEXTURE_COLOR_TABLE_SCALE_SGI                  = $80BF;

  { SGI_color_table }
  GL_COLOR_TABLE_SGI                                = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI               = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI              = $80D2;
  GL_PROXY_COLOR_TABLE_SGI                          = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI         = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI        = $80D5;
  GL_COLOR_TABLE_SCALE_SGI                          = $80D6;
  GL_COLOR_TABLE_BIAS_SGI                           = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI                         = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI                          = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI                       = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI                     = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI                      = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI                     = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI                 = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI                 = $80DF;

  { EXT_cmyka }
  GL_CMYK_EXT                                       = $800C;
  GL_CMYKA_EXT                                      = $800D;
  GL_PACK_CMYK_HINT_EXT                             = $800E;
  GL_UNPACK_CMYK_HINT_EXT                           = $800F;

  { EXT_rescale_normal }
  GL_RESCALE_NORMAL_EXT                             = $803A;

  { EXT_clip_volume_hint }
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT                      = $80F0;

  { EXT_cull_vertex }
  GL_CULL_VERTEX_EXT                                = $81AA;
  GL_CULL_VERTEX_EYE_POSITION_EXT                   = $81AB;
  GL_CULL_VERTEX_OBJECT_POSITION_EXT                = $81AC;

  { EXT_index_array_formats }
  GL_IUI_V2F_EXT                                    = $81AD;
  GL_IUI_V3F_EXT                                    = $81AE;
  GL_IUI_N3F_V2F_EXT                                = $81AF;
  GL_IUI_N3F_V3F_EXT                                = $81B0;
  GL_T2F_IUI_V2F_EXT                                = $81B1;
  GL_T2F_IUI_V3F_EXT                                = $81B2;
  GL_T2F_IUI_N3F_V2F_EXT                            = $81B3;
  GL_T2F_IUI_N3F_V3F_EXT                            = $81B4;

  { EXT_index_func }
  GL_INDEX_TEST_EXT                                 = $81B5;
  GL_INDEX_TEST_FUNC_EXT                            = $81B6;
  GL_INDEX_TEST_REF_EXT                             = $81B7;

  { EXT_index_material }
  GL_INDEX_MATERIAL_EXT                             = $81B8;
  GL_INDEX_MATERIAL_PARAMETER_EXT                   = $81B9;
  GL_INDEX_MATERIAL_FACE_EXT                        = $81BA;

  { EXT_misc_attribute }
  GL_MISC_BIT_EXT                                   = 0; { not yet defined }

  { EXT_scene_marker }
  GL_SCENE_REQUIRED_EXT                             = 0; { not yet defined }

  { EXT_shared_texture_palette }
  GL_SHARED_TEXTURE_PALETTE_EXT                     = $81FB;

  { EXT_nurbs_tessellator }
  GLU_NURBS_MODE_EXT                                = 100160;
  GLU_NURBS_TESSELLATOR_EXT                         = 100161;
  GLU_NURBS_RENDERER_EXT                            = 100162;
  GLU_NURBS_BEGIN_EXT                               = 100164;
  GLU_NURBS_VERTEX_EXT                              = 100165;
  GLU_NURBS_NORMAL_EXT                              = 100166;
  GLU_NURBS_COLOR_EXT                               = 100167;
  GLU_NURBS_TEX_COORD_EXT                           = 100168;
  GLU_NURBS_END_EXT                                 = 100169;
  GLU_NURBS_BEGIN_DATA_EXT                          = 100170;
  GLU_NURBS_VERTEX_DATA_EXT                         = 100171;
  GLU_NURBS_NORMAL_DATA_EXT                         = 100172;
  GLU_NURBS_COLOR_DATA_EXT                          = 100173;
  GLU_NURBS_TEX_COORD_DATA_EXT                      = 100174;
  GLU_NURBS_END_DATA_EXT                            = 100175;

  { EXT_object_space_tess }
  GLU_OBJECT_PARAMETRIC_ERROR_EXT                   = 100208;
  GLU_OBJECT_PATH_LENGTH_EXT                        = 100209;

  { EXT_point_parameters }
  GL_POINT_SIZE_MIN_EXT                             = $8126;
  GL_POINT_SIZE_MAX_EXT                             = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT                  = $8128;
  GL_DISTANCE_ATTENUATION_EXT                       = $8129;

  { EXT_compiled_vertex_array }
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT                   = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT                   = $81A9;

  { ARB_multitexture }
  GL_ACTIVE_TEXTURE_ARB                             = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB                      = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB                          = $84E2;
  GL_TEXTURE0_ARB                                   = $84C0;
  GL_TEXTURE1_ARB                                   = $84C1;
  GL_TEXTURE2_ARB                                   = $84C2;
  GL_TEXTURE3_ARB                                   = $84C3;
  GL_TEXTURE4_ARB                                   = $84C4;
  GL_TEXTURE5_ARB                                   = $84C5;
  GL_TEXTURE6_ARB                                   = $84C6;
  GL_TEXTURE7_ARB                                   = $84C7;
  GL_TEXTURE8_ARB                                   = $84C8;
  GL_TEXTURE9_ARB                                   = $84C9;
  GL_TEXTURE10_ARB                                  = $84CA;
  GL_TEXTURE11_ARB                                  = $84CB;
  GL_TEXTURE12_ARB                                  = $84CC;
  GL_TEXTURE13_ARB                                  = $84CD;
  GL_TEXTURE14_ARB                                  = $84CE;
  GL_TEXTURE15_ARB                                  = $84CF;
  GL_TEXTURE16_ARB                                  = $84D0;
  GL_TEXTURE17_ARB                                  = $84D1;
  GL_TEXTURE18_ARB                                  = $84D2;
  GL_TEXTURE19_ARB                                  = $84D3;
  GL_TEXTURE20_ARB                                  = $84D4;
  GL_TEXTURE21_ARB                                  = $84D5;
  GL_TEXTURE22_ARB                                  = $84D6;
  GL_TEXTURE23_ARB                                  = $84D7;
  GL_TEXTURE24_ARB                                  = $84D8;
  GL_TEXTURE25_ARB                                  = $84D9;
  GL_TEXTURE26_ARB                                  = $84DA;
  GL_TEXTURE27_ARB                                  = $84DB;
  GL_TEXTURE28_ARB                                  = $84DC;
  GL_TEXTURE29_ARB                                  = $84DD;
  GL_TEXTURE30_ARB                                  = $84DE;
  GL_TEXTURE31_ARB                                  = $84DF;

  { EXT_stencil_wrap }
  GL_INCR_WRAP_EXT                                  = $8507;
  GL_DECR_WRAP_EXT                                  = $8508;

  { NV_texgen_reflection }
  GL_NORMAL_MAP_NV                                  = $8511;
  GL_REFLECTION_MAP_NV                              = $8512;

  { EXT_texture_env_combine }
  GL_COMBINE_EXT                                    = $8570;
  GL_COMBINE_RGB_EXT                                = $8571;
  GL_COMBINE_ALPHA_EXT                              = $8572;
  GL_RGB_SCALE_EXT                                  = $8573;
  GL_ADD_SIGNED_EXT                                 = $8574;
  GL_INTERPOLATE_EXT                                = $8575;
  GL_CONSTANT_EXT                                   = $8576;
  GL_PRIMARY_COLOR_EXT                              = $8577;
  GL_PREVIOUS_EXT                                   = $8578;
  GL_SOURCE0_RGB_EXT                                = $8580;
  GL_SOURCE1_RGB_EXT                                = $8581;
  GL_SOURCE2_RGB_EXT                                = $8582;
  GL_SOURCE0_ALPHA_EXT                              = $8588;
  GL_SOURCE1_ALPHA_EXT                              = $8589;
  GL_SOURCE2_ALPHA_EXT                              = $858A;
  GL_OPERAND0_RGB_EXT                               = $8590;
  GL_OPERAND1_RGB_EXT                               = $8591;
  GL_OPERAND2_RGB_EXT                               = $8592;
  GL_OPERAND0_ALPHA_EXT                             = $8598;
  GL_OPERAND1_ALPHA_EXT                             = $8599;
  GL_OPERAND2_ALPHA_EXT                             = $859A;

  { NV_texture_env_combine4 }
  GL_COMBINE4_NV                                    = $8503;
  GL_SOURCE3_RGB_NV                                 = $8583;
  GL_SOURCE3_ALPHA_NV                               = $858B;
  GL_OPERAND3_RGB_NV                                = $8593;
  GL_OPERAND3_ALPHA_NV                              = $859B;

  GL_BLEND_EQUATION                                 = $8009;
  GL_TABLE_TOO_LARGE                                = $8031;
  GL_UNSIGNED_BYTE_3_3_2                            = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4                         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1                         = $8034;
  GL_UNSIGNED_INT_8_8_8_8                           = $8035;
  GL_UNSIGNED_INT_10_10_10_2                        = $8036;
  GL_UNSIGNED_BYTE_2_3_3_REV                        = $8362;
  GL_UNSIGNED_SHORT_5_6_5                           = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV                       = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV                     = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV                     = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV                       = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV                    = $8368;

  { GL_ARB_transpose_matrix }
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB                 = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB                = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB                   = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB                     = $84E6;

  { GL_ARB_multisample }
  GL_MULTISAMPLE_ARB                                = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB                   = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB                        = $809F;
  GL_SAMPLE_COVERAGE_ARB                            = $80A0;
  GL_SAMPLE_BUFFERS_ARB                             = $80A8;
  GL_SAMPLES_ARB                                    = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB                      = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB                     = $80AB;
  GL_MULTISAMPLE_BIT_ARB                            = $20000000;

  { GL_ARB_texture_cube_map }
  GL_NORMAL_MAP_ARB                                 = $8511;
  GL_REFLECTION_MAP_ARB                             = $8512;
  GL_TEXTURE_CUBE_MAP_ARB                           = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB                   = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB                = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB                = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB                = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB                = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB                = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB                = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB                     = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB                  = $851C;

  { GL_ARB_texture_compression }
  GL_COMPRESSED_ALPHA_ARB                           = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB                       = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB                 = $84EB;
  GL_COMPRESSED_INTENSITY_ARB                       = $84EC;
  GL_COMPRESSED_RGB_ARB                             = $84ED;
  GL_COMPRESSED_RGBA_ARB                            = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB                   = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB              = $86A0;
  GL_TEXTURE_COMPRESSED_ARB                         = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB             = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB                 = $86A3;

  { GL_ARB_vertex_blend }
  GL_MAX_VERTEX_UNITS_ARB                           = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB                        = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB                           = $86A6;
  GL_VERTEX_BLEND_ARB                               = $86A7;
  GL_CURRENT_WEIGHT_ARB                             = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB                          = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB                        = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB                          = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB                       = $86AC;
  GL_WEIGHT_ARRAY_ARB                               = $86AD;
  GL_MODELVIEW0_ARB                                 = $1700;
  GL_MODELVIEW1_ARB                                 = $850A;
  GL_MODELVIEW2_ARB                                 = $8722;
  GL_MODELVIEW3_ARB                                 = $8723;
  GL_MODELVIEW4_ARB                                 = $8724;
  GL_MODELVIEW5_ARB                                 = $8725;
  GL_MODELVIEW6_ARB                                 = $8726;
  GL_MODELVIEW7_ARB                                 = $8727;
  GL_MODELVIEW8_ARB                                 = $8728;
  GL_MODELVIEW9_ARB                                 = $8729;
  GL_MODELVIEW10_ARB                                = $872A;
  GL_MODELVIEW11_ARB                                = $872B;
  GL_MODELVIEW12_ARB                                = $872C;
  GL_MODELVIEW13_ARB                                = $872D;
  GL_MODELVIEW14_ARB                                = $872E;
  GL_MODELVIEW15_ARB                                = $872F;
  GL_MODELVIEW16_ARB                                = $8730;
  GL_MODELVIEW17_ARB                                = $8731;
  GL_MODELVIEW18_ARB                                = $8732;
  GL_MODELVIEW19_ARB                                = $8733;
  GL_MODELVIEW20_ARB                                = $8734;
  GL_MODELVIEW21_ARB                                = $8735;
  GL_MODELVIEW22_ARB                                = $8736;
  GL_MODELVIEW23_ARB                                = $8737;
  GL_MODELVIEW24_ARB                                = $8738;
  GL_MODELVIEW25_ARB                                = $8739;
  GL_MODELVIEW26_ARB                                = $873A;
  GL_MODELVIEW27_ARB                                = $873B;
  GL_MODELVIEW28_ARB                                = $873C;
  GL_MODELVIEW29_ARB                                = $873D;
  GL_MODELVIEW30_ARB                                = $873E;
  GL_MODELVIEW31_ARB                                = $873F;

  { GL_SGIS_texture_filter4 }
  GL_FILTER4_SGIS                                   = $8146;
  GL_TEXTURE_FILTER4_SIZE_SGIS                      = $8147;

  { GL_SGIS_pixel_texture }
  GL_PIXEL_TEXTURE_SGIS                             = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS                 = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS               = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS                         = $8356;

  { GL_SGIX_pixel_texture }
  GL_PIXEL_TEX_GEN_SGIX                             = $8139;
  GL_PIXEL_TEX_GEN_MODE_SGIX                        = $832B;

  { GL_SGIS_texture4D }
  GL_PACK_SKIP_VOLUMES_SGIS                         = $8130;
  GL_PACK_IMAGE_DEPTH_SGIS                          = $8131;
  GL_UNPACK_SKIP_VOLUMES_SGIS                       = $8132;
  GL_UNPACK_IMAGE_DEPTH_SGIS                        = $8133;
  GL_TEXTURE_4D_SGIS                                = $8134;
  GL_PROXY_TEXTURE_4D_SGIS                          = $8135;
  GL_TEXTURE_4DSIZE_SGIS                            = $8136;
  GL_TEXTURE_WRAP_Q_SGIS                            = $8137;
  GL_MAX_4D_TEXTURE_SIZE_SGIS                       = $8138;
  GL_TEXTURE_4D_BINDING_SGIS                        = $814F;

  { GL_SGIS_detail_texture }
  GL_DETAIL_TEXTURE_2D_SGIS                         = $8095;
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS                 = $8096;
  GL_LINEAR_DETAIL_SGIS                             = $8097;
  GL_LINEAR_DETAIL_ALPHA_SGIS                       = $8098;
  GL_LINEAR_DETAIL_COLOR_SGIS                       = $8099;
  GL_DETAIL_TEXTURE_LEVEL_SGIS                      = $809A;
  GL_DETAIL_TEXTURE_MODE_SGIS                       = $809B;
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS                = $809C;

  { GL_SGIS_sharpen_texture }
  GL_LINEAR_SHARPEN_SGIS                            = $80AD;
  GL_LINEAR_SHARPEN_ALPHA_SGIS                      = $80AE;
  GL_LINEAR_SHARPEN_COLOR_SGIS                      = $80AF;
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS               = $80B0;

  { GL_SGIS_texture_lod }
  GL_TEXTURE_MIN_LOD_SGIS                           = $813A;
  GL_TEXTURE_MAX_LOD_SGIS                           = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS                        = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS                         = $813D;

  { GL_SGIS_multisample }
  GL_MULTISAMPLE_SGIS                               = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS                      = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS                       = $809F;
  GL_SAMPLE_MASK_SGIS                               = $80A0;
  GL_1PASS_SGIS                                     = $80A1;
  GL_2PASS_0_SGIS                                   = $80A2;
  GL_2PASS_1_SGIS                                   = $80A3;
  GL_4PASS_0_SGIS                                   = $80A4;
  GL_4PASS_1_SGIS                                   = $80A5;
  GL_4PASS_2_SGIS                                   = $80A6;
  GL_4PASS_3_SGIS                                   = $80A7;
  GL_SAMPLE_BUFFERS_SGIS                            = $80A8;
  GL_SAMPLES_SGIS                                   = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS                         = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS                        = $80AB;
  GL_SAMPLE_PATTERN_SGIS                            = $80AC;

  { GL_SGIS_generate_mipmap }
  GL_GENERATE_MIPMAP_SGIS                           = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS                      = $8192;

  { GL_SGIX_clipmap }
  GL_LINEAR_CLIPMAP_LINEAR_SGIX                     = $8170;
  GL_TEXTURE_CLIPMAP_CENTER_SGIX                    = $8171;
  GL_TEXTURE_CLIPMAP_FRAME_SGIX                     = $8172;
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX                    = $8173;
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX             = $8174;
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX                = $8175;
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX                     = $8176;
  GL_MAX_CLIPMAP_DEPTH_SGIX                         = $8177;
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX                 = $8178;
  GL_NEAREST_CLIPMAP_NEAREST_SGIX                   = $844D;
  GL_NEAREST_CLIPMAP_LINEAR_SGIX                    = $844E;
  GL_LINEAR_CLIPMAP_NEAREST_SGIX                    = $844F;

  { GL_SGIX_shadow }
  GL_TEXTURE_COMPARE_SGIX                           = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX                  = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX                          = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX                          = $819D;

  { GL_SGIS_texture_edge_clamp }
  GL_CLAMP_TO_EDGE_SGIS                             = $812F;

  { GL_SGIS_texture_border_clamp }
  GL_CLAMP_TO_BORDER_SGIS                           = $812D;

  { GL_SGIX_interlace }
  GL_INTERLACE_SGIX                                 = $8094;

  { GL_SGIX_pixel_tiles }
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX                 = $813E;
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX                = $813F;
  GL_PIXEL_TILE_WIDTH_SGIX                          = $8140;
  GL_PIXEL_TILE_HEIGHT_SGIX                         = $8141;
  GL_PIXEL_TILE_GRID_WIDTH_SGIX                     = $8142;
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX                    = $8143;
  GL_PIXEL_TILE_GRID_DEPTH_SGIX                     = $8144;
  GL_PIXEL_TILE_CACHE_SIZE_SGIX                     = $8145;

  { GL_SGIS_texture_select }
  GL_DUAL_ALPHA4_SGIS                               = $8110;
  GL_DUAL_ALPHA8_SGIS                               = $8111;
  GL_DUAL_ALPHA12_SGIS                              = $8112;
  GL_DUAL_ALPHA16_SGIS                              = $8113;
  GL_DUAL_LUMINANCE4_SGIS                           = $8114;
  GL_DUAL_LUMINANCE8_SGIS                           = $8115;
  GL_DUAL_LUMINANCE12_SGIS                          = $8116;
  GL_DUAL_LUMINANCE16_SGIS                          = $8117;
  GL_DUAL_INTENSITY4_SGIS                           = $8118;
  GL_DUAL_INTENSITY8_SGIS                           = $8119;
  GL_DUAL_INTENSITY12_SGIS                          = $811A;
  GL_DUAL_INTENSITY16_SGIS                          = $811B;
  GL_DUAL_LUMINANCE_ALPHA4_SGIS                     = $811C;
  GL_DUAL_LUMINANCE_ALPHA8_SGIS                     = $811D;
  GL_QUAD_ALPHA4_SGIS                               = $811E;
  GL_QUAD_ALPHA8_SGIS                               = $811F;
  GL_QUAD_LUMINANCE4_SGIS                           = $8120;
  GL_QUAD_LUMINANCE8_SGIS                           = $8121;
  GL_QUAD_INTENSITY4_SGIS                           = $8122;
  GL_QUAD_INTENSITY8_SGIS                           = $8123;
  GL_DUAL_TEXTURE_SELECT_SGIS                       = $8124;
  GL_QUAD_TEXTURE_SELECT_SGIS                       = $8125;

  { GL_SGIX_sprite }
  GL_SPRITE_SGIX                                    = $8148;
  GL_SPRITE_MODE_SGIX                               = $8149;
  GL_SPRITE_AXIS_SGIX                               = $814A;
  GL_SPRITE_TRANSLATION_SGIX                        = $814B;
  GL_SPRITE_AXIAL_SGIX                              = $814C;
  GL_SPRITE_OBJECT_ALIGNED_SGIX                     = $814D;
  GL_SPRITE_EYE_ALIGNED_SGIX                        = $814E;

  { GL_SGIX_texture_multi_buffer }
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX                 = $812E;

  { GL_SGIS_point_parameters }
  GL_POINT_SIZE_MIN_SGIS                            = $8126;
  GL_POINT_SIZE_MAX_SGIS                            = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS                 = $8128;
  GL_DISTANCE_ATTENUATION_SGIS                      = $8129;

  { GL_SGIX_instruments }
  GL_INSTRUMENT_BUFFER_POINTER_SGIX                 = $8180;
  GL_INSTRUMENT_MEASUREMENTS_SGIX                   = $8181;

  { GL_SGIX_texture_scale_bias }
  GL_POST_TEXTURE_FILTER_BIAS_SGIX                  = $8179;
  GL_POST_TEXTURE_FILTER_SCALE_SGIX                 = $817A;
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX            = $817B;
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX           = $817C;

  { GL_SGIX_framezoom }
  GL_FRAMEZOOM_SGIX                                 = $818B;
  GL_FRAMEZOOM_FACTOR_SGIX                          = $818C;
  GL_MAX_FRAMEZOOM_FACTOR_SGIX                      = $818D;

  { GL_FfdMaskSGIX }
  GL_TEXTURE_DEFORMATION_BIT_SGIX                   = $00000001;
  GL_GEOMETRY_DEFORMATION_BIT_SGIX                  = $00000002;

  { GL_SGIX_polynomial_ffd }
  GL_GEOMETRY_DEFORMATION_SGIX                      = $8194;
  GL_TEXTURE_DEFORMATION_SGIX                       = $8195;
  GL_DEFORMATIONS_MASK_SGIX                         = $8196;
  GL_MAX_DEFORMATION_ORDER_SGIX                     = $8197;

  { GL_SGIX_reference_plane }
  GL_REFERENCE_PLANE_SGIX                           = $817D;
  GL_REFERENCE_PLANE_EQUATION_SGIX                  = $817E;

  { GL_SGIX_depth_texture }
  GL_DEPTH_COMPONENT16_SGIX                         = $81A5;
  GL_DEPTH_COMPONENT24_SGIX                         = $81A6;
  GL_DEPTH_COMPONENT32_SGIX                         = $81A7;

  { GL_SGIS_fog_function }
  GL_FOG_FUNC_SGIS                                  = $812A;
  GL_FOG_FUNC_POINTS_SGIS                           = $812B;
  GL_MAX_FOG_FUNC_POINTS_SGIS                       = $812C;

  { GL_SGIX_fog_offset }
  GL_FOG_OFFSET_SGIX                                = $8198;
  GL_FOG_OFFSET_VALUE_SGIX                          = $8199;

  { GL_HP_image_transform }
  GL_IMAGE_SCALE_X_HP                               = $8155;
  GL_IMAGE_SCALE_Y_HP                               = $8156;
  GL_IMAGE_TRANSLATE_X_HP                           = $8157;
  GL_IMAGE_TRANSLATE_Y_HP                           = $8158;
  GL_IMAGE_ROTATE_ANGLE_HP                          = $8159;
  GL_IMAGE_ROTATE_ORIGIN_X_HP                       = $815A;
  GL_IMAGE_ROTATE_ORIGIN_Y_HP                       = $815B;
  GL_IMAGE_MAG_FILTER_HP                            = $815C;
  GL_IMAGE_MIN_FILTER_HP                            = $815D;
  GL_IMAGE_CUBIC_WEIGHT_HP                          = $815E;
  GL_CUBIC_HP                                       = $815F;
  GL_AVERAGE_HP                                     = $8160;
  GL_IMAGE_TRANSFORM_2D_HP                          = $8161;
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP            = $8162;
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP      = $8163;

  { GL_HP_convolution_border_modes }
  GL_IGNORE_BORDER_HP                               = $8150;
  GL_CONSTANT_BORDER_HP                             = $8151;
  GL_REPLICATE_BORDER_HP                            = $8153;
  GL_CONVOLUTION_BORDER_COLOR_HP                    = $8154;

  { GL_SGIX_texture_add_env }
  GL_TEXTURE_ENV_BIAS_SGIX                          = $80BE;

  { GL_PGI_vertex_hints }
  GL_VERTEX_DATA_HINT_PGI                           = $1A22A;
  GL_VERTEX_CONSISTENT_HINT_PGI                     = $1A22B;
  GL_MATERIAL_SIDE_HINT_PGI                         = $1A22C;
  GL_MAX_VERTEX_HINT_PGI                            = $1A22D;
  GL_COLOR3_BIT_PGI                                 = $00010000;
  GL_COLOR4_BIT_PGI                                 = $00020000;
  GL_EDGEFLAG_BIT_PGI                               = $00040000;
  GL_INDEX_BIT_PGI                                  = $00080000;
  GL_MAT_AMBIENT_BIT_PGI                            = $00100000;
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI                = $00200000;
  GL_MAT_DIFFUSE_BIT_PGI                            = $00400000;
  GL_MAT_EMISSION_BIT_PGI                           = $00800000;
  GL_MAT_COLOR_INDEXES_BIT_PGI                      = $01000000;
  GL_MAT_SHININESS_BIT_PGI                          = $02000000;
  GL_MAT_SPECULAR_BIT_PGI                           = $04000000;
  GL_NORMAL_BIT_PGI                                 = $08000000;
  GL_TEXCOORD1_BIT_PGI                              = $10000000;
  GL_TEXCOORD2_BIT_PGI                              = $20000000;
  GL_TEXCOORD3_BIT_PGI                              = $40000000;
  GL_TEXCOORD4_BIT_PGI                              = $80000000;
  GL_VERTEX23_BIT_PGI                               = $00000004;
  GL_VERTEX4_BIT_PGI                                = $00000008;

  { GL_PGI_misc_hints }
  GL_PREFER_DOUBLEBUFFER_HINT_PGI                   = $1A1F8;
  GL_CONSERVE_MEMORY_HINT_PGI                       = $1A1FD;
  GL_RECLAIM_MEMORY_HINT_PGI                        = $1A1FE;
  GL_NATIVE_GRAPHICS_HANDLE_PGI                     = $1A202;
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI                 = $1A203;
  GL_NATIVE_GRAPHICS_END_HINT_PGI                   = $1A204;
  GL_ALWAYS_FAST_HINT_PGI                           = $1A20C;
  GL_ALWAYS_SOFT_HINT_PGI                           = $1A20D;
  GL_ALLOW_DRAW_OBJ_HINT_PGI                        = $1A20E;
  GL_ALLOW_DRAW_WIN_HINT_PGI                        = $1A20F;
  GL_ALLOW_DRAW_FRG_HINT_PGI                        = $1A210;
  GL_ALLOW_DRAW_MEM_HINT_PGI                        = $1A211;
  GL_STRICT_DEPTHFUNC_HINT_PGI                      = $1A216;
  GL_STRICT_LIGHTING_HINT_PGI                       = $1A217;
  GL_STRICT_SCISSOR_HINT_PGI                        = $1A218;
  GL_FULL_STIPPLE_HINT_PGI                          = $1A219;
  GL_CLIP_NEAR_HINT_PGI                             = $1A220;
  GL_CLIP_FAR_HINT_PGI                              = $1A221;
  GL_WIDE_LINE_HINT_PGI                             = $1A222;
  GL_BACK_NORMALS_HINT_PGI                          = $1A223;

  { GL_EXT_paletted_texture }
  GL_TEXTURE_INDEX_SIZE_EXT                         = $80ED;

  { GL_SGIX_list_priority }
  GL_LIST_PRIORITY_SGIX                             = $8182;

  { GL_SGIX_ir_instrument1 }
  GL_IR_INSTRUMENT1_SGIX                            = $817F;

  { GL_SGIX_calligraphic_fragment }
  GL_CALLIGRAPHIC_FRAGMENT_SGIX                     = $8183;

  { GL_SGIX_texture_lod_bias }
  GL_TEXTURE_LOD_BIAS_S_SGIX                        = $818E;
  GL_TEXTURE_LOD_BIAS_T_SGIX                        = $818F;
  GL_TEXTURE_LOD_BIAS_R_SGIX                        = $8190;

  { GL_SGIX_shadow_ambient }
  GL_SHADOW_AMBIENT_SGIX                            = $80BF;

  { GL_SGIX_ycrcb }
  GL_YCRCB_422_SGIX                                 = $81BB;
  GL_YCRCB_444_SGIX                                 = $81BC;

  { GL_SGIX_fragment_lighting }
  GL_FRAGMENT_LIGHTING_SGIX                         = $8400;
  GL_FRAGMENT_COLOR_MATERIAL_SGIX                   = $8401;
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX              = $8402;
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX         = $8403;
  GL_MAX_FRAGMENT_LIGHTS_SGIX                       = $8404;
  GL_MAX_ACTIVE_LIGHTS_SGIX                         = $8405;
  GL_CURRENT_RASTER_NORMAL_SGIX                     = $8406;
  GL_LIGHT_ENV_MODE_SGIX                            = $8407;
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX         = $8408;
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX             = $8409;
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX              = $840A;
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = $840B;
  GL_FRAGMENT_LIGHT0_SGIX                           = $840C;
  GL_FRAGMENT_LIGHT1_SGIX                           = $840D;
  GL_FRAGMENT_LIGHT2_SGIX                           = $840E;
  GL_FRAGMENT_LIGHT3_SGIX                           = $840F;
  GL_FRAGMENT_LIGHT4_SGIX                           = $8410;
  GL_FRAGMENT_LIGHT5_SGIX                           = $8411;
  GL_FRAGMENT_LIGHT6_SGIX                           = $8412;
  GL_FRAGMENT_LIGHT7_SGIX                           = $8413;

  { GL_IBM_rasterpos_clip }
  GL_RASTER_POSITION_UNCLIPPED_IBM                  = $19262;

  { GL_HP_texture_lighting }
  GL_TEXTURE_LIGHTING_MODE_HP                       = $8167;
  GL_TEXTURE_POST_SPECULAR_HP                       = $8168;
  GL_TEXTURE_PRE_SPECULAR_HP                        = $8169;

  { GL_EXT_draw_range_elements }
  GL_MAX_ELEMENTS_VERTICES_EXT                      = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT                       = $80E9;

  { GL_WIN_phong_shading }
  GL_PHONG_WIN                                      = $80EA;
  GL_PHONG_HINT_WIN                                 = $80EB;

  { GL_WIN_specular_fog }
  GL_FOG_SPECULAR_TEXTURE_WIN                       = $80EC;

  { GL_EXT_light_texture }
  GL_FRAGMENT_MATERIAL_EXT                          = $8349;
  GL_FRAGMENT_NORMAL_EXT                            = $834A;
  GL_FRAGMENT_COLOR_EXT                             = $834C;
  GL_ATTENUATION_EXT                                = $834D;
  GL_SHADOW_ATTENUATION_EXT                         = $834E;
  GL_TEXTURE_APPLICATION_MODE_EXT                   = $834F;
  GL_TEXTURE_LIGHT_EXT                              = $8350;
  GL_TEXTURE_MATERIAL_FACE_EXT                      = $8351;
  GL_TEXTURE_MATERIAL_PARAMETER_EXT                 = $8352;

  { GL_SGIX_blend_alpha_minmax }
  GL_ALPHA_MIN_SGIX                                 = $8320;
  GL_ALPHA_MAX_SGIX                                 = $8321;

  { GL_SGIX_async }
  GL_ASYNC_MARKER_SGIX                              = $8329;

  { GL_SGIX_async_pixel }
  GL_ASYNC_TEX_IMAGE_SGIX                           = $835C;
  GL_ASYNC_DRAW_PIXELS_SGIX                         = $835D;
  GL_ASYNC_READ_PIXELS_SGIX                         = $835E;
  GL_MAX_ASYNC_TEX_IMAGE_SGIX                       = $835F;
  GL_MAX_ASYNC_DRAW_PIXELS_SGIX                     = $8360;
  GL_MAX_ASYNC_READ_PIXELS_SGIX                     = $8361;

  { GL_SGIX_async_histogram }
  GL_ASYNC_HISTOGRAM_SGIX                           = $832C;
  GL_MAX_ASYNC_HISTOGRAM_SGIX                       = $832D;

  { GL_INTEL_parallel_arrays }
  GL_PARALLEL_ARRAYS_INTEL                          = $83F4;
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL           = $83F5;
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL           = $83F6;
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL            = $83F7;
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL    = $83F8;

  { GL_HP_occlusion_test }
  GL_OCCLUSION_TEST_HP                              = $8165;
  GL_OCCLUSION_TEST_RESULT_HP                       = $8166;

  { GL_EXT_pixel_transform }
  GL_PIXEL_TRANSFORM_2D_EXT                         = $8330;
  GL_PIXEL_MAG_FILTER_EXT                           = $8331;
  GL_PIXEL_MIN_FILTER_EXT                           = $8332;
  GL_PIXEL_CUBIC_WEIGHT_EXT                         = $8333;
  GL_CUBIC_EXT                                      = $8334;
  GL_AVERAGE_EXT                                    = $8335;
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT             = $8336;
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT         = $8337;
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT                  = $8338;

  { GL_EXT_separate_specular_color }
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT                  = $81F8;
  GL_SINGLE_COLOR_EXT                               = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT                    = $81FA;

  { GL_EXT_secondary_color }
  GL_COLOR_SUM_EXT                                  = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT                    = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT                 = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT                 = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT               = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT              = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT                      = $845E;

  { GL_EXT_texture_perturb_normal }
  GL_PERTURB_EXT                                    = $85AE;
  GL_TEXTURE_NORMAL_EXT                             = $85AF;

  { GL_EXT_fog_coord }
  GL_FOG_COORDINATE_SOURCE_EXT                      = $8450;
  GL_FOG_COORDINATE_EXT                             = $8451;
  GL_FRAGMENT_DEPTH_EXT                             = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT                     = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT                  = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT                = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT               = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT                       = $8457;

  { GL_REND_screen_coordinates }
  GL_SCREEN_COORDINATES_REND                        = $8490;
  GL_INVERTED_SCREEN_W_REND                         = $8491;

  { GL_EXT_coordinate_frame }
  GL_TANGENT_ARRAY_EXT                              = $8439;
  GL_BINORMAL_ARRAY_EXT                             = $843A;
  GL_CURRENT_TANGENT_EXT                            = $843B;
  GL_CURRENT_BINORMAL_EXT                           = $843C;
  GL_TANGENT_ARRAY_TYPE_EXT                         = $843E;
  GL_TANGENT_ARRAY_STRIDE_EXT                       = $843F;
  GL_BINORMAL_ARRAY_TYPE_EXT                        = $8440;
  GL_BINORMAL_ARRAY_STRIDE_EXT                      = $8441;
  GL_TANGENT_ARRAY_POINTER_EXT                      = $8442;
  GL_BINORMAL_ARRAY_POINTER_EXT                     = $8443;
  GL_MAP1_TANGENT_EXT                               = $8444;
  GL_MAP2_TANGENT_EXT                               = $8445;
  GL_MAP1_BINORMAL_EXT                              = $8446;
  GL_MAP2_BINORMAL_EXT                              = $8447;

  { GL_EXT_texture_env_combine }
  GL_SOURCE3_RGB_EXT                                = $8583;
  GL_SOURCE4_RGB_EXT                                = $8584;
  GL_SOURCE5_RGB_EXT                                = $8585;
  GL_SOURCE6_RGB_EXT                                = $8586;
  GL_SOURCE7_RGB_EXT                                = $8587;
  GL_SOURCE3_ALPHA_EXT                              = $858B;
  GL_SOURCE4_ALPHA_EXT                              = $858C;
  GL_SOURCE5_ALPHA_EXT                              = $858D;
  GL_SOURCE6_ALPHA_EXT                              = $858E;
  GL_SOURCE7_ALPHA_EXT                              = $858F;
  GL_OPERAND3_RGB_EXT                               = $8593;
  GL_OPERAND4_RGB_EXT                               = $8594;
  GL_OPERAND5_RGB_EXT                               = $8595;
  GL_OPERAND6_RGB_EXT                               = $8596;
  GL_OPERAND7_RGB_EXT                               = $8597;
  GL_OPERAND3_ALPHA_EXT                             = $859B;
  GL_OPERAND4_ALPHA_EXT                             = $859C;
  GL_OPERAND5_ALPHA_EXT                             = $859D;
  GL_OPERAND6_ALPHA_EXT                             = $859E;
  GL_OPERAND7_ALPHA_EXT                             = $859F;

  { GL_APPLE_specular_vector }
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE              = $85B0;

  { GL_APPLE_transform_hint }
  GL_TRANSFORM_HINT_APPLE                           = $85B1;

  { GL_SGIX_fog_scale }
  GL_FOG_SCALE_SGIX                                 = $81FC;
  GL_FOG_SCALE_VALUE_SGIX                           = $81FD;

  { GL_SUNX_constant_data }
  GL_UNPACK_CONSTANT_DATA_SUNX                      = $81D5;
  GL_TEXTURE_CONSTANT_DATA_SUNX                     = $81D6;

  { GL_SUN_global_alpha }
  GL_GLOBAL_ALPHA_SUN                               = $81D9;
  GL_GLOBAL_ALPHA_FACTOR_SUN                        = $81DA;

  { GL_SUN_triangle_list }
  GL_RESTART_SUN                                    = $01;
  GL_REPLACE_MIDDLE_SUN                             = $02;
  GL_REPLACE_OLDEST_SUN                             = $03;
  GL_TRIANGLE_LIST_SUN                              = $81D7;
  GL_REPLACEMENT_CODE_SUN                           = $81D8;
  GL_REPLACEMENT_CODE_ARRAY_SUN                     = $85C0;
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN                = $85C1;
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN              = $85C2;
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN             = $85C3;
  GL_R1UI_V3F_SUN                                   = $85C4;
  GL_R1UI_C4UB_V3F_SUN                              = $85C5;
  GL_R1UI_C3F_V3F_SUN                               = $85C6;
  GL_R1UI_N3F_V3F_SUN                               = $85C7;
  GL_R1UI_C4F_N3F_V3F_SUN                           = $85C8;
  GL_R1UI_T2F_V3F_SUN                               = $85C9;
  GL_R1UI_T2F_N3F_V3F_SUN                           = $85CA;
  GL_R1UI_T2F_C4F_N3F_V3F_SUN                       = $85CB;

  { GL_EXT_blend_func_separate }
  GL_BLEND_DST_RGB_EXT                              = $80C8;
  GL_BLEND_SRC_RGB_EXT                              = $80C9;
  GL_BLEND_DST_ALPHA_EXT                            = $80CA;
  GL_BLEND_SRC_ALPHA_EXT                            = $80CB;

  { GL_INGR_color_clamp }
  GL_RED_MIN_CLAMP_INGR                             = $8560;
  GL_GREEN_MIN_CLAMP_INGR                           = $8561;
  GL_BLUE_MIN_CLAMP_INGR                            = $8562;
  GL_ALPHA_MIN_CLAMP_INGR                           = $8563;
  GL_RED_MAX_CLAMP_INGR                             = $8564;
  GL_GREEN_MAX_CLAMP_INGR                           = $8565;
  GL_BLUE_MAX_CLAMP_INGR                            = $8566;
  GL_ALPHA_MAX_CLAMP_INGR                           = $8567;

  { GL_INGR_interlace_read }
  GL_INTERLACE_READ_INGR                            = $8568;

  { GL_EXT_422_pixels }
  GL_422_EXT                                        = $80CC;
  GL_422_REV_EXT                                    = $80CD;
  GL_422_AVERAGE_EXT                                = $80CE;
  GL_422_REV_AVERAGE_EXT                            = $80CF;

  { GL_EXT_texture_cube_map }
  GL_NORMAL_MAP_EXT                                 = $8511;
  GL_REFLECTION_MAP_EXT                             = $8512;
  GL_TEXTURE_CUBE_MAP_EXT                           = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT                   = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT                = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT                = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT                = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT                = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT                = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT                = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT                     = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT                  = $851C;

  { GL_SUN_convolution_border_modes }
  GL_WRAP_BORDER_SUN                                = $81D4;

  { GL_EXT_texture_lod_bias }
  GL_MAX_TEXTURE_LOD_BIAS_EXT                       = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT                     = $8500;
  GL_TEXTURE_LOD_BIAS_EXT                           = $8501;

  { GL_EXT_texture_filter_anisotropic }
  GL_TEXTURE_MAX_ANISOTROPY_EXT                     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                 = $84FF;

  { GL_EXT_vertex_weighting }
  GL_MODELVIEW0_STACK_DEPTH_EXT                     = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT                     = $8502;
  GL_MODELVIEW0_MATRIX_EXT                          = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW_MATRIX1_EXT                          = $8506;
  GL_VERTEX_WEIGHTING_EXT                           = $8509;
  GL_MODELVIEW0_EXT                                 = GL_MODELVIEW;
  GL_MODELVIEW1_EXT                                 = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT                      = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT                        = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT                   = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT                   = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT                 = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT                = $8510;

  { GL_NV_light_max_exponent }
  GL_MAX_SHININESS_NV                               = $8504;
  GL_MAX_SPOT_EXPONENT_NV                           = $8505;

  { GL_NV_vertex_array_range }
  GL_VERTEX_ARRAY_RANGE_NV                          = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV                   = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV                    = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV              = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV                  = $8521;

  { GL_NV_register_combiners }
  GL_REGISTER_COMBINERS_NV                          = $8522;
  GL_VARIABLE_A_NV                                  = $8523;
  GL_VARIABLE_B_NV                                  = $8524;
  GL_VARIABLE_C_NV                                  = $8525;
  GL_VARIABLE_D_NV                                  = $8526;
  GL_VARIABLE_E_NV                                  = $8527;
  GL_VARIABLE_F_NV                                  = $8528;
  GL_VARIABLE_G_NV                                  = $8529;
  GL_CONSTANT_COLOR0_NV                             = $852A;
  GL_CONSTANT_COLOR1_NV                             = $852B;
  GL_PRIMARY_COLOR_NV                               = $852C;
  GL_SECONDARY_COLOR_NV                             = $852D;
  GL_SPARE0_NV                                      = $852E;
  GL_SPARE1_NV                                      = $852F;
  GL_DISCARD_NV                                     = $8530;
  GL_E_TIMES_F_NV                                   = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV                 = $8532;
  GL_UNSIGNED_IDENTITY_NV                           = $8536;
  GL_UNSIGNED_INVERT_NV                             = $8537;
  GL_EXPAND_NORMAL_NV                               = $8538;
  GL_EXPAND_NEGATE_NV                               = $8539;
  GL_HALF_BIAS_NORMAL_NV                            = $853A;
  GL_HALF_BIAS_NEGATE_NV                            = $853B;
  GL_SIGNED_IDENTITY_NV                             = $853C;
  GL_SIGNED_NEGATE_NV                               = $853D;
  GL_SCALE_BY_TWO_NV                                = $853E;
  GL_SCALE_BY_FOUR_NV                               = $853F;
  GL_SCALE_BY_ONE_HALF_NV                           = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV                   = $8541;
  GL_COMBINER_INPUT_NV                              = $8542;
  GL_COMBINER_MAPPING_NV                            = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV                    = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV                     = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV                     = $8546;
  GL_COMBINER_MUX_SUM_NV                            = $8547;
  GL_COMBINER_SCALE_NV                              = $8548;
  GL_COMBINER_BIAS_NV                               = $8549;
  GL_COMBINER_AB_OUTPUT_NV                          = $854A;
  GL_COMBINER_CD_OUTPUT_NV                          = $854B;
  GL_COMBINER_SUM_OUTPUT_NV                         = $854C;
  GL_MAX_GENERAL_COMBINERS_NV                       = $854D;
  GL_NUM_GENERAL_COMBINERS_NV                       = $854E;
  GL_COLOR_SUM_CLAMP_NV                             = $854F;
  GL_COMBINER0_NV                                   = $8550;
  GL_COMBINER1_NV                                   = $8551;
  GL_COMBINER2_NV                                   = $8552;
  GL_COMBINER3_NV                                   = $8553;
  GL_COMBINER4_NV                                   = $8554;
  GL_COMBINER5_NV                                   = $8555;
  GL_COMBINER6_NV                                   = $8556;
  GL_COMBINER7_NV                                   = $8557;

  { GL_NV_fog_distance }
  GL_FOG_DISTANCE_MODE_NV                           = $855A;
  GL_EYE_RADIAL_NV                                  = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV                          = $855C;

  { GL_NV_texgen_emboss }
  GL_EMBOSS_LIGHT_NV                                = $855D;
  GL_EMBOSS_CONSTANT_NV                             = $855E;
  GL_EMBOSS_MAP_NV                                  = $855F;

  { GL_EXT_texture_compression_s3tc }
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT                   = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                  = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                  = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                  = $83F3;

  { GL_IBM_cull_vertex }
  GL_CULL_VERTEX_IBM                                = 103050;

  { GL_IBM_vertex_array_lists }
  GL_VERTEX_ARRAY_LIST_IBM                          = 103070;
  GL_NORMAL_ARRAY_LIST_IBM                          = 103071;
  GL_COLOR_ARRAY_LIST_IBM                           = 103072;
  GL_INDEX_ARRAY_LIST_IBM                           = 103073;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM                   = 103074;
  GL_EDGE_FLAG_ARRAY_LIST_IBM                       = 103075;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM                  = 103076;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM                 = 103077;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM                   = 103080;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM                   = 103081;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM                    = 103082;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM                    = 103083;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM            = 103084;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM                = 103085;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM           = 103086;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM          = 103087;

  { GL_SGIX_subsample }
  GL_PACK_SUBSAMPLE_RATE_SGIX                       = $85A0;
  GL_UNPACK_SUBSAMPLE_RATE_SGIX                     = $85A1;
  GL_PIXEL_SUBSAMPLE_4444_SGIX                      = $85A2;
  GL_PIXEL_SUBSAMPLE_2424_SGIX                      = $85A3;
  GL_PIXEL_SUBSAMPLE_4242_SGIX                      = $85A4;

  { GL_SGIX_ycrcba }
  GL_YCRCB_SGIX                                     = $8318;
  GL_YCRCBA_SGIX                                    = $8319;

  { GL_SGI_depth_pass_instrument }
  GL_DEPTH_PASS_INSTRUMENT_SGIX                     = $8310;
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX            = $8311;
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX                 = $8312;

  { GL_3DFX_texture_compression_FXT1 }
  GL_COMPRESSED_RGB_FXT1_3DFX                       = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX                      = $86B1;

  { GL_3DFX_multisample }
  GL_MULTISAMPLE_3DFX                               = $86B2;
  GL_SAMPLE_BUFFERS_3DFX                            = $86B3;
  GL_SAMPLES_3DFX                                   = $86B4;
  GL_MULTISAMPLE_BIT_3DFX                           = $20000000;

  { GL_EXT_multisample }
  GL_MULTISAMPLE_EXT                                = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT                       = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT                        = $809F;
  GL_SAMPLE_MASK_EXT                                = $80A0;
  GL_1PASS_EXT                                      = $80A1;
  GL_2PASS_0_EXT                                    = $80A2;
  GL_2PASS_1_EXT                                    = $80A3;
  GL_4PASS_0_EXT                                    = $80A4;
  GL_4PASS_1_EXT                                    = $80A5;
  GL_4PASS_2_EXT                                    = $80A6;
  GL_4PASS_3_EXT                                    = $80A7;
  GL_SAMPLE_BUFFERS_EXT                             = $80A8;
  GL_SAMPLES_EXT                                    = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT                          = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT                         = $80AB;
  GL_SAMPLE_PATTERN_EXT                             = $80AC;

  { GL_SGIX_vertex_preclip }
  GL_VERTEX_PRECLIP_SGIX                            = $83EE;
  GL_VERTEX_PRECLIP_HINT_SGIX                       = $83EF;

  { GL_SGIX_convolution_accuracy }
  GL_CONVOLUTION_HINT_SGIX                          = $8316;

  { GL_SGIX_resample }
  GL_PACK_RESAMPLE_SGIX                             = $842C;
  GL_UNPACK_RESAMPLE_SGIX                           = $842D;
  GL_RESAMPLE_REPLICATE_SGIX                        = $842E;
  GL_RESAMPLE_ZERO_FILL_SGIX                        = $842F;
  GL_RESAMPLE_DECIMATE_SGIX                         = $8430;

  { GL_SGIS_point_line_texgen }
  GL_EYE_DISTANCE_TO_POINT_SGIS                     = $81F0;
  GL_OBJECT_DISTANCE_TO_POINT_SGIS                  = $81F1;
  GL_EYE_DISTANCE_TO_LINE_SGIS                      = $81F2;
  GL_OBJECT_DISTANCE_TO_LINE_SGIS                   = $81F3;
  GL_EYE_POINT_SGIS                                 = $81F4;
  GL_OBJECT_POINT_SGIS                              = $81F5;
  GL_EYE_LINE_SGIS                                  = $81F6;
  GL_OBJECT_LINE_SGIS                               = $81F7;

  { GL_SGIS_texture_color_mask }
  GL_TEXTURE_COLOR_WRITEMASK_SGIS                   = $81EF;

  { GL_NV_vertex_program }
  GL_VERTEX_PROGRAM_NV                              = $8620;
  GL_VERTEX_STATE_PROGRAM_NV                        = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV                           = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV                         = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV                           = $8625;
  GL_CURRENT_ATTRIB_NV                              = $8626;
  GL_PROGRAM_LENGTH_NV                              = $8627;
  GL_PROGRAM_STRING_NV                              = $8628;
  GL_MODELVIEW_PROJECTION_NV                        = $8629;
  GL_IDENTITY_NV                                    = $862A;
  GL_INVERSE_NV                                     = $862B;
  GL_TRANSPOSE_NV                                   = $862C;
  GL_INVERSE_TRANSPOSE_NV                           = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV                = $862E;
  GL_MAX_TRACK_MATRICES_NV                          = $862F;
  GL_MATRIX0_NV                                     = $8630;
  GL_MATRIX1_NV                                     = $8631;
  GL_MATRIX2_NV                                     = $8632;
  GL_MATRIX3_NV                                     = $8633;
  GL_MATRIX4_NV                                     = $8634;
  GL_MATRIX5_NV                                     = $8635;
  GL_MATRIX6_NV                                     = $8636;
  GL_MATRIX7_NV                                     = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV                  = $8640;
  GL_CURRENT_MATRIX_NV                              = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV                   = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV                     = $8643;
  GL_PROGRAM_PARAMETER_NV                           = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV                        = $8645;
  GL_PROGRAM_TARGET_NV                              = $8646;
  GL_PROGRAM_RESIDENT_NV                            = $8647;
  GL_TRACK_MATRIX_NV                                = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV                      = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV                      = $864A;
  GL_PROGRAM_ERROR_POSITION_NV                      = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV                        = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV                        = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV                        = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV                        = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV                        = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV                        = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV                        = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV                        = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV                        = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV                        = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV                       = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV                       = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV                       = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV                       = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV                       = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV                       = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV                       = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV                       = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV                       = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV                       = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV                       = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV                       = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV                       = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV                       = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV                       = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV                       = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV                      = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV                      = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV                      = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV                      = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV                      = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV                      = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV                       = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV                       = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV                       = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV                       = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV                       = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV                       = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV                       = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV                       = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV                       = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV                       = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV                      = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV                      = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV                      = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV                      = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV                      = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV                      = $867F;


  { ********** GLU generic constants ********** }

  { Errors: (return value 0 = no error) }
  GLU_INVALID_ENUM                                  = 100900;
  GLU_INVALID_VALUE                                 = 100901;
  GLU_OUT_OF_MEMORY                                 = 100902;
  GLU_INCOMPATIBLE_GL_VERSION                       = 100903;

  { StringName }
  GLU_VERSION                                       = 100800;
  GLU_EXTENSIONS                                    = 100801;

  { Boolean }
  GLU_TRUE                                          = GL_TRUE;
  GLU_FALSE                                         = GL_FALSE;

  { Quadric constants }
  { QuadricNormal }
  GLU_SMOOTH                                        = 100000;
  GLU_FLAT                                          = 100001;
  GLU_NONE                                          = 100002;

  { QuadricDrawStyle }
  GLU_POINT                                         = 100010;
  GLU_LINE                                          = 100011;
  GLU_FILL                                          = 100012;
  GLU_SILHOUETTE                                    = 100013;

  { QuadricOrientation }
  GLU_OUTSIDE                                       = 100020;
  GLU_INSIDE                                        = 100021;

  { Tesselation constants }
  GLU_TESS_MAX_COORD                                = 1.0e150;

  { TessProperty }
  GLU_TESS_WINDING_RULE                             = 100140;
  GLU_TESS_BOUNDARY_ONLY                            = 100141;
  GLU_TESS_TOLERANCE                                = 100142;

  { TessWinding }
  GLU_TESS_WINDING_ODD                              = 100130;
  GLU_TESS_WINDING_NONZERO                          = 100131;
  GLU_TESS_WINDING_POSITIVE                         = 100132;
  GLU_TESS_WINDING_NEGATIVE                         = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO                      = 100134;

  { TessCallback }
  GLU_TESS_BEGIN                                    = 100100; { TGLUTessBeginProc }
  GLU_TESS_VERTEX                                   = 100101; { TGLUTessVertexProc }
  GLU_TESS_END                                      = 100102; { TGLUTessEndProc }
  GLU_TESS_ERROR                                    = 100103; { TGLUTessErrorProc }
  GLU_TESS_EDGE_FLAG                                = 100104; { TGLUTessEdgeFlagProc }
  GLU_TESS_COMBINE                                  = 100105; { TGLUTessCombineProc }
  GLU_TESS_BEGIN_DATA                               = 100106; { TGLUTessBeginDataProc }
  GLU_TESS_VERTEX_DATA                              = 100107; { TGLUTessVertexDataProc }
  GLU_TESS_END_DATA                                 = 100108; { TGLUTessEndDataProc }
  GLU_TESS_ERROR_DATA                               = 100109; { TGLUTessErrorDataProc }
  GLU_TESS_EDGE_FLAG_DATA                           = 100110; { TGLUTessEdgeFlagDataProc }
  GLU_TESS_COMBINE_DATA                             = 100111; { TGLUTessCombineDataProc }

  { TessError }
  GLU_TESS_ERROR1                                   = 100151;
  GLU_TESS_ERROR2                                   = 100152;
  GLU_TESS_ERROR3                                   = 100153;
  GLU_TESS_ERROR4                                   = 100154;
  GLU_TESS_ERROR5                                   = 100155;
  GLU_TESS_ERROR6                                   = 100156;
  GLU_TESS_ERROR7                                   = 100157;
  GLU_TESS_ERROR8                                   = 100158;

  GLU_TESS_MISSING_BEGIN_POLYGON                    = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR                    = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON                      = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR                      = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE                          = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK                    = GLU_TESS_ERROR6;

  { NURBS constants }

  { NurbsProperty }
  GLU_AUTO_LOAD_MATRIX                              = 100200;
  GLU_CULLING                                       = 100201;
  GLU_SAMPLING_TOLERANCE                            = 100203;
  GLU_DISPLAY_MODE                                  = 100204;
  GLU_PARAMETRIC_TOLERANCE                          = 100202;
  GLU_SAMPLING_METHOD                               = 100205;
  GLU_U_STEP                                        = 100206;
  GLU_V_STEP                                        = 100207;

  { NurbsSampling }
  GLU_PATH_LENGTH                                   = 100215;
  GLU_PARAMETRIC_ERROR                              = 100216;
  GLU_DOMAIN_DISTANCE                               = 100217;

  { NurbsTrim }
  GLU_MAP1_TRIM_2                                   = 100210;
  GLU_MAP1_TRIM_3                                   = 100211;

  { NurbsDisplay }
  GLU_OUTLINE_POLYGON                               = 100240;
  GLU_OUTLINE_PATCH                                 = 100241;

  { NurbsErrors }
  GLU_NURBS_ERROR1                                  = 100251;
  GLU_NURBS_ERROR2                                  = 100252;
  GLU_NURBS_ERROR3                                  = 100253;
  GLU_NURBS_ERROR4                                  = 100254;
  GLU_NURBS_ERROR5                                  = 100255;
  GLU_NURBS_ERROR6                                  = 100256;
  GLU_NURBS_ERROR7                                  = 100257;
  GLU_NURBS_ERROR8                                  = 100258;
  GLU_NURBS_ERROR9                                  = 100259;
  GLU_NURBS_ERROR10                                 = 100260;
  GLU_NURBS_ERROR11                                 = 100261;
  GLU_NURBS_ERROR12                                 = 100262;
  GLU_NURBS_ERROR13                                 = 100263;
  GLU_NURBS_ERROR14                                 = 100264;
  GLU_NURBS_ERROR15                                 = 100265;
  GLU_NURBS_ERROR16                                 = 100266;
  GLU_NURBS_ERROR17                                 = 100267;
  GLU_NURBS_ERROR18                                 = 100268;
  GLU_NURBS_ERROR19                                 = 100269;
  GLU_NURBS_ERROR20                                 = 100270;
  GLU_NURBS_ERROR21                                 = 100271;
  GLU_NURBS_ERROR22                                 = 100272;
  GLU_NURBS_ERROR23                                 = 100273;
  GLU_NURBS_ERROR24                                 = 100274;
  GLU_NURBS_ERROR25                                 = 100275;
  GLU_NURBS_ERROR26                                 = 100276;
  GLU_NURBS_ERROR27                                 = 100277;
  GLU_NURBS_ERROR28                                 = 100278;
  GLU_NURBS_ERROR29                                 = 100279;
  GLU_NURBS_ERROR30                                 = 100280;
  GLU_NURBS_ERROR31                                 = 100281;
  GLU_NURBS_ERROR32                                 = 100282;
  GLU_NURBS_ERROR33                                 = 100283;
  GLU_NURBS_ERROR34                                 = 100284;
  GLU_NURBS_ERROR35                                 = 100285;
  GLU_NURBS_ERROR36                                 = 100286;
  GLU_NURBS_ERROR37                                 = 100287;

  { Contours types -- obsolete! }
  GLU_CW                                            = 100120;
  GLU_CCW                                           = 100121;
  GLU_INTERIOR                                      = 100122;
  GLU_EXTERIOR                                      = 100123;
  GLU_UNKNOWN                                       = 100124;

  { Names without "TESS_" prefix }
  GLU_BEGIN                                         = GLU_TESS_BEGIN;
  GLU_VERTEX                                        = GLU_TESS_VERTEX;
  GLU_END                                           = GLU_TESS_END;
  GLU_ERROR                                         = GLU_TESS_ERROR;
  GLU_EDGE_FLAG                                     = GLU_TESS_EDGE_FLAG;

  {$ifdef UNIX}
  GLX_VERSION_1_1                                   = 1;
  GLX_VERSION_1_2                                   = 1;
  GLX_VERSION_1_3                                   = 1;
  GLX_EXTENSION_NAME                                = 'GLX';
  GLX_USE_GL                                        = 1;
  GLX_BUFFER_SIZE                                   = 2;
  GLX_LEVEL                                         = 3;
  GLX_RGBA                                          = 4;
  GLX_DOUBLEBUFFER                                  = 5;
  GLX_STEREO                                        = 6;
  GLX_AUX_BUFFERS                                   = 7;
  GLX_RED_SIZE                                      = 8;
  GLX_GREEN_SIZE                                    = 9;
  GLX_BLUE_SIZE                                     = 10;
  GLX_ALPHA_SIZE                                    = 11;
  GLX_DEPTH_SIZE                                    = 12;
  GLX_STENCIL_SIZE                                  = 13;
  GLX_ACCUM_RED_SIZE                                = 14;
  GLX_ACCUM_GREEN_SIZE                              = 15;
  GLX_ACCUM_BLUE_SIZE                               = 16;
  GLX_ACCUM_ALPHA_SIZE                              = 17;

  { Error codes returned by glXGetConfig: }
  GLX_BAD_SCREEN                                    = 1;
  GLX_BAD_ATTRIBUTE                                 = 2;
  GLX_NO_EXTENSION                                  = 3;
  GLX_BAD_VISUAL                                    = 4;
  GLX_BAD_CONTEXT                                   = 5;
  GLX_BAD_VALUE                                     = 6;
  GLX_BAD_ENUM                                      = 7;

  { GLX 1.1 and later: }
  GLX_VENDOR                                        = 1;
  GLX_VERSION                                       = 2;
  GLX_EXTENSIONS                                    = 3;

  { GLX 1.3 and later: }
  GLX_CONFIG_CAVEAT                                 = $20;
  GLX_DONT_CARE                                     = $FFFFFFFF;
  GLX_SLOW_CONFIG                                   = $8001;
  GLX_NON_CONFORMANT_CONFIG                         = $800D;
  GLX_X_VISUAL_TYPE                                 = $22;
  GLX_TRANSPARENT_TYPE                              = $23;
  GLX_TRANSPARENT_INDEX_VALUE                       = $24;
  GLX_TRANSPARENT_RED_VALUE                         = $25;
  GLX_TRANSPARENT_GREEN_VALUE                       = $26;
  GLX_TRANSPARENT_BLUE_VALUE                        = $27;
  GLX_TRANSPARENT_ALPHA_VALUE                       = $28;
  GLX_MAX_PBUFFER_WIDTH                             = $8016;
  GLX_MAX_PBUFFER_HEIGHT                            = $8017;
  GLX_MAX_PBUFFER_PIXELS                            = $8018;
  GLX_PRESERVED_CONTENTS                            = $801B;
  GLX_LARGEST_BUFFER                                = $801C;
  GLX_DRAWABLE_TYPE                                 = $8010;
  GLX_FBCONFIG_ID                                   = $8013;
  GLX_VISUAL_ID                                     = $800B;
  GLX_WINDOW_BIT                                    = $00000001;
  GLX_PIXMAP_BIT                                    = $00000002;
  GLX_PBUFFER_BIT                                   = $00000004;
  GLX_AUX_BUFFERS_BIT                               = $00000010;
  GLX_FRONT_LEFT_BUFFER_BIT                         = $00000001;
  GLX_FRONT_RIGHT_BUFFER_BIT                        = $00000002;
  GLX_BACK_LEFT_BUFFER_BIT                          = $00000004;
  GLX_BACK_RIGHT_BUFFER_BIT                         = $00000008;
  GLX_DEPTH_BUFFER_BIT                              = $00000020;
  GLX_STENCIL_BUFFER_BIT                            = $00000040;
  GLX_ACCUM_BUFFER_BIT                              = $00000080;
  GLX_RENDER_TYPE                                   = $8011;
  GLX_X_RENDERABLE                                  = $8012;
  GLX_NONE                                          = $8000;
  GLX_TRUE_COLOR                                    = $8002;
  GLX_DIRECT_COLOR                                  = $8003;
  GLX_PSEUDO_COLOR                                  = $8004;
  GLX_STATIC_COLOR                                  = $8005;
  GLX_GRAY_SCALE                                    = $8006;
  GLX_STATIC_GRAY                                   = $8007;
  GLX_TRANSPARENT_INDEX                             = $8009;
  GLX_COLOR_INDEX_TYPE                              = $8015;
  GLX_COLOR_INDEX_BIT                               = $00000002;
  GLX_SCREEN                                        = $800C;
  GLX_PBUFFER_CLOBBER_MASK                          = $08000000;
  GLX_DAMAGED                                       = $8020;
  GLX_SAVED                                         = $8021;
  GLX_WINDOW                                        = $8022;
  GLX_PBUFFER                                       = $8023;
  GLX_EXT_visual_info                               = 1;
  GLX_X_VISUAL_TYPE_EXT                             = $22;
  GLX_TRANSPARENT_TYPE_EXT                          = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT                   = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT                     = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT                   = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT                    = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT                   = $28;
  GLX_TRUE_COLOR_EXT                                = $8002;
  GLX_DIRECT_COLOR_EXT                              = $8003;
  GLX_PSEUDO_COLOR_EXT                              = $8004;
  GLX_STATIC_COLOR_EXT                              = $8005;
  GLX_GRAY_SCALE_EXT                                = $8006;
  GLX_STATIC_GRAY_EXT                               = $8007;
  GLX_NONE_EXT                                      = $8000;
  GLX_TRANSPARENT_RGB_EXT                           = $8008;
  GLX_TRANSPARENT_INDEX_EXT                         = $8009;
  GLX_VISUAL_CAVEAT_EXT                             = $20;
  GLX_SLOW_VISUAL_EXT                               = $8001;
  GLX_NON_CONFORMANT_VISUAL_EXT                     = $800D;
  GLX_SHARE_CONTEXT_EXT                             = $800A;
  GLX_VISUAL_ID_EXT                                 = $800B;
  GLX_SCREEN_EXT                                    = $800C;
  GLX_3DFX_WINDOW_MODE_MESA                         = $1;
  GLX_3DFX_FULLSCREEN_MODE_MESA                     = $2;
  {$endif}


type
  { GLU types }
  TGLUNurbs = record end;
  TGLUQuadric = record end;
  TGLUTesselator = record end;

  PGLUNurbs = ^TGLUNurbs;
  PGLUQuadric = ^TGLUQuadric;
  PGLUTesselator = ^TGLUTesselator;

  { backwards compatibility }
  TGLUNurbsObj = TGLUNurbs;
  TGLUQuadricObj = TGLUQuadric;
  TGLUTesselatorObj = TGLUTesselator;
  TGLUTriangulatorObj = TGLUTesselator;

  PGLUNurbsObj = PGLUNurbs;
  PGLUQuadricObj = PGLUQuadric;
  PGLUTesselatorObj = PGLUTesselator;
  PGLUTriangulatorObj = PGLUTesselator;

  { Callback function prototypes }
  { GLUQuadricCallback }
  TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); OPENGL_CALLBACK_CALL

  { GLUTessCallback }
  TGLUTessBeginProc = procedure(AType: TGLEnum); OPENGL_CALLBACK_CALL
  TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); OPENGL_CALLBACK_CALL
  TGLUTessVertexProc = procedure(VertexData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessEndProc = procedure; OPENGL_CALLBACK_CALL
  TGLUTessErrorProc = procedure(ErrNo: TGLEnum); OPENGL_CALLBACK_CALL
  TGLUTessCombineProc = procedure(Coords: PVector3d; VertexData: PVector4p; Weight: PVector4f; OutData: PPointer); OPENGL_CALLBACK_CALL
  TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessEndDataProc = procedure(UserData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); OPENGL_CALLBACK_CALL
  TGLUTessCombineDataProc = procedure(Coords: PVector3d; VertexData: PVector4p; Weight: PVector4f; OutData: PPointer; UserData: Pointer); OPENGL_CALLBACK_CALL

  { GLUNurbsCallback }
  TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); OPENGL_CALLBACK_CALL

var
  { GL functions and procedures }
  glAccum: procedure(op: TGLuint; value: TGLfloat); OPENGL_CALL
  glAlphaFunc: procedure(func: TGLEnum; ref: TGLclampf); OPENGL_CALL
  glAreTexturesResident: function(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; OPENGL_CALL
  glArrayElement: procedure(i: TGLint); OPENGL_CALL
  glBegin: procedure(mode: TGLEnum); OPENGL_CALL
  glBindTexture: procedure(target: TGLEnum; texture: TGLuint); OPENGL_CALL
  glBitmap: procedure(width, height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); OPENGL_CALL
  glBlendFunc: procedure(sfactor: TGLEnum; dfactor: TGLEnum); OPENGL_CALL
  glCallList: procedure(list: TGLuint); OPENGL_CALL
  glCallLists: procedure(n: TGLsizei; atype: TGLEnum; lists: Pointer); OPENGL_CALL
  glClear: procedure(mask: TGLbitfield); OPENGL_CALL
  glClearAccum: procedure(red, green, blue, alpha: TGLfloat); OPENGL_CALL
  glClearColor: procedure(red, green, blue, alpha: TGLclampf); OPENGL_CALL
  glClearDepth: procedure(depth: TGLclampd); OPENGL_CALL
  glClearIndex: procedure(c: TGLfloat); OPENGL_CALL
  glClearStencil: procedure(s: TGLint ); OPENGL_CALL
  glClipPlane: procedure(plane: TGLEnum; equation: PGLdouble); OPENGL_CALL
  glColor3b: procedure(red, green, blue: TGLbyte); OPENGL_CALL
  glColor3bv: procedure(v: PGLbyte); OPENGL_CALL
  glColor3d: procedure(red, green, blue: TGLdouble); OPENGL_CALL
  glColor3dv: procedure(v: PGLdouble); OPENGL_CALL
  glColor3f: procedure(red, green, blue: TGLfloat); OPENGL_CALL
  glColor3fv: procedure(v: PGLfloat); OPENGL_CALL
  glColor3i: procedure(red, green, blue: TGLint); OPENGL_CALL
  glColor3iv: procedure(v: PGLint); OPENGL_CALL
  glColor3s: procedure(red, green, blue: TGLshort); OPENGL_CALL
  glColor3sv: procedure(v: PGLshort); OPENGL_CALL
  glColor3ub: procedure(red, green, blue: TGLubyte); OPENGL_CALL
  glColor3ubv: procedure(v: PGLubyte); OPENGL_CALL
  glColor3ui: procedure(red, green, blue: TGLuint); OPENGL_CALL
  glColor3uiv: procedure(v: PGLuint); OPENGL_CALL
  glColor3us: procedure(red, green, blue: TGLushort); OPENGL_CALL
  glColor3usv: procedure(v: PGLushort); OPENGL_CALL
  glColor4b: procedure(red, green, blue, alpha: TGLbyte); OPENGL_CALL
  glColor4bv: procedure(v: PGLbyte); OPENGL_CALL
  glColor4d: procedure(red, green, blue, alpha: TGLdouble ); OPENGL_CALL
  glColor4dv: procedure(v: PGLdouble); OPENGL_CALL
  glColor4f: procedure(red, green, blue, alpha: TGLfloat); OPENGL_CALL
  glColor4fv: procedure(v: PGLfloat); OPENGL_CALL
  glColor4i: procedure(red, green, blue, alpha: TGLint); OPENGL_CALL
  glColor4iv: procedure(v: PGLint); OPENGL_CALL
  glColor4s: procedure(red, green, blue, alpha: TGLshort); OPENGL_CALL
  glColor4sv: procedure(v: PGLshort); OPENGL_CALL
  glColor4ub: procedure(red, green, blue, alpha: TGLubyte); OPENGL_CALL
  glColor4ubv: procedure(v: PGLubyte); OPENGL_CALL
  glColor4ui: procedure(red, green, blue, alpha: TGLuint); OPENGL_CALL
  glColor4uiv: procedure(v: PGLuint); OPENGL_CALL
  glColor4us: procedure(red, green, blue, alpha: TGLushort); OPENGL_CALL
  glColor4usv: procedure(v: PGLushort); OPENGL_CALL
  glColorMask: procedure(red, green, blue, alpha: TGLboolean); OPENGL_CALL
  glColorMaterial: procedure(face: TGLEnum; mode: TGLEnum); OPENGL_CALL
  glColorPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glCopyPixels: procedure(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); OPENGL_CALL
  glCopyTexImage1D: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); OPENGL_CALL
  glCopyTexImage2D: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); OPENGL_CALL
  glCopyTexSubImage1D: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); OPENGL_CALL
  glCopyTexSubImage2D: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); OPENGL_CALL
  glCullFace: procedure(mode: TGLEnum); OPENGL_CALL
  glDeleteLists: procedure(list: TGLuint; range: TGLsizei); OPENGL_CALL
  glDeleteTextures: procedure(n: TGLsizei; textures: PGLuint); OPENGL_CALL
  glDepthFunc: procedure(func: TGLEnum); OPENGL_CALL
  glDepthMask: procedure(flag: TGLboolean); OPENGL_CALL
  glDepthRange: procedure(zNear, zFar: TGLclampd); OPENGL_CALL
  glDisable: procedure(cap: TGLEnum); OPENGL_CALL
  glDisableClientState: procedure(aarray: TGLEnum); OPENGL_CALL
  glDrawArrays: procedure(mode: TGLEnum; first: TGLint; count: TGLsizei); OPENGL_CALL
  glDrawBuffer: procedure(mode: TGLEnum); OPENGL_CALL
  glDrawElements: procedure(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); OPENGL_CALL
  glDrawPixels: procedure(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glEdgeFlag: procedure(flag: TGLboolean); OPENGL_CALL
  glEdgeFlagPointer: procedure(stride: TGLsizei; data: pointer); OPENGL_CALL
  glEdgeFlagv: procedure(flag: PGLboolean); OPENGL_CALL
  glEnable: procedure(cap: TGLEnum); OPENGL_CALL
  glEnableClientState: procedure(aarray: TGLEnum); OPENGL_CALL
  glEnd: procedure; OPENGL_CALL
  glEndList: procedure; OPENGL_CALL
  glEvalCoord1d: procedure(u: TGLdouble); OPENGL_CALL
  glEvalCoord1dv: procedure(u: PGLdouble); OPENGL_CALL
  glEvalCoord1f: procedure(u: TGLfloat); OPENGL_CALL
  glEvalCoord1fv: procedure(u: PGLfloat); OPENGL_CALL
  glEvalCoord2d: procedure(u: TGLdouble; v: TGLdouble); OPENGL_CALL
  glEvalCoord2dv: procedure(u: PGLdouble); OPENGL_CALL
  glEvalCoord2f: procedure(u, v: TGLfloat); OPENGL_CALL
  glEvalCoord2fv: procedure(u: PGLfloat); OPENGL_CALL
  glEvalMesh1: procedure(mode: TGLEnum; i1, i2: TGLint); OPENGL_CALL
  glEvalMesh2: procedure(mode: TGLEnum; i1, i2, j1, j2: TGLint); OPENGL_CALL
  glEvalPoint1: procedure(i: TGLint); OPENGL_CALL
  glEvalPoint2: procedure(i, j: TGLint); OPENGL_CALL
  glFeedbackBuffer: procedure(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); OPENGL_CALL
  glFinish: procedure; OPENGL_CALL
  glFlush: procedure; OPENGL_CALL
  glFogf: procedure(pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glFogfv: procedure(pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glFogi: procedure(pname: TGLEnum; param: TGLint); OPENGL_CALL
  glFogiv: procedure(pname: TGLEnum; params: PGLint); OPENGL_CALL
  glFrontFace: procedure(mode: TGLEnum); OPENGL_CALL
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: TGLdouble); OPENGL_CALL
  glGenLists: function(range: TGLsizei): TGLuint; OPENGL_CALL
  glGenTextures: procedure(n: TGLsizei; textures: PGLuint); OPENGL_CALL
  glGetBooleanv: procedure(pname: TGLEnum; params: PGLboolean); OPENGL_CALL
  glGetClipPlane: procedure(plane: TGLEnum; equation: PGLdouble); OPENGL_CALL
  glGetDoublev: procedure(pname: TGLEnum; params: PGLdouble); OPENGL_CALL
  glGetError: function: TGLuint; OPENGL_CALL
  glGetFloatv: procedure(pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetIntegerv: procedure(pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetLightfv: procedure(light, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetLightiv: procedure(light, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetMapdv: procedure(target, query: TGLEnum; v: PGLdouble); OPENGL_CALL
  glGetMapfv: procedure(target, query: TGLEnum; v: PGLfloat); OPENGL_CALL
  glGetMapiv: procedure(target, query: TGLEnum; v: PGLint); OPENGL_CALL
  glGetMaterialfv: procedure(face, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetMaterialiv: procedure(face, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetPixelMapfv: procedure(map: TGLEnum; values: PGLfloat); OPENGL_CALL
  glGetPixelMapuiv: procedure(map: TGLEnum; values: PGLuint); OPENGL_CALL
  glGetPixelMapusv: procedure(map: TGLEnum; values: PGLushort); OPENGL_CALL
  glGetPointerv: procedure(pname: TGLEnum; var params); OPENGL_CALL
  glGetPolygonStipple: procedure(mask: PGLubyte); OPENGL_CALL
  glGetString: function(name: TGLEnum): PChar; OPENGL_CALL
  glGetTexEnvfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetTexEnviv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetTexGendv: procedure(coord, pname: TGLEnum; params: PGLdouble); OPENGL_CALL
  glGetTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetTexImage: procedure(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glGetTexLevelParameterfv: procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetTexLevelParameteriv: procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetTexParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetTexParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glHint: procedure(target, mode: TGLEnum); OPENGL_CALL
  glIndexMask: procedure(mask: TGLuint); OPENGL_CALL
  glIndexPointer: procedure(atype: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glIndexd: procedure(c: TGLdouble); OPENGL_CALL
  glIndexdv: procedure(c: PGLdouble); OPENGL_CALL
  glIndexf: procedure(c: TGLfloat); OPENGL_CALL
  glIndexfv: procedure(c: PGLfloat); OPENGL_CALL
  glIndexi: procedure(c: TGLint); OPENGL_CALL
  glIndexiv: procedure(c: PGLint); OPENGL_CALL
  glIndexs: procedure(c: TGLshort); OPENGL_CALL
  glIndexsv: procedure(c: PGLshort); OPENGL_CALL
  glIndexub: procedure(c: TGLubyte); OPENGL_CALL
  glIndexubv: procedure(c: PGLubyte); OPENGL_CALL
  glInitNames: procedure; OPENGL_CALL
  glInterleavedArrays: procedure(format: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glIsEnabled: function(cap: TGLEnum): TGLboolean; OPENGL_CALL
  glIsList: function(list: TGLuint): TGLboolean; OPENGL_CALL
  glIsTexture: function(texture: TGLuint): TGLboolean; OPENGL_CALL
  glLightModelf: procedure(pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glLightModelfv: procedure(pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glLightModeli: procedure(pname: TGLEnum; param: TGLint); OPENGL_CALL
  glLightModeliv: procedure(pname: TGLEnum; params: PGLint); OPENGL_CALL
  glLightf: procedure(light, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glLightfv: procedure(light, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glLighti: procedure(light, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glLightiv: procedure(light, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glLineStipple: procedure(factor: TGLint; pattern: TGLushort); OPENGL_CALL
  glLineWidth: procedure(width: TGLfloat); OPENGL_CALL
  glListBase: procedure(base: TGLuint); OPENGL_CALL
  glLoadIdentity: procedure; OPENGL_CALL
  glLoadMatrixd: procedure(m: PGLdouble); OPENGL_CALL
  glLoadMatrixf: procedure(m: PGLfloat); OPENGL_CALL
  glLoadName: procedure(name: TGLuint); OPENGL_CALL
  glLogicOp: procedure(opcode: TGLEnum); OPENGL_CALL
  glMap1d: procedure(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); OPENGL_CALL
  glMap1f: procedure(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat);   OPENGL_CALL
  glMap2d: procedure(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,    vorder: TGLint; points: PGLdouble); OPENGL_CALL
  glMap2f: procedure(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,    vorder: TGLint; points: PGLfloat); OPENGL_CALL
  glMapGrid1d: procedure(un: TGLint; u1, u2: TGLdouble); OPENGL_CALL
  glMapGrid1f: procedure(un: TGLint; u1, u2: TGLfloat); OPENGL_CALL
  glMapGrid2d: procedure(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); OPENGL_CALL
  glMapGrid2f: procedure(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); OPENGL_CALL
  glMaterialf: procedure(face, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glMaterialfv: procedure(face, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glMateriali: procedure(face, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glMaterialiv: procedure(face, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glMatrixMode: procedure(mode: TGLEnum); OPENGL_CALL
  glMultMatrixd: procedure(m: PGLdouble); OPENGL_CALL
  glMultMatrixf: procedure(m: PGLfloat); OPENGL_CALL
  glNewList: procedure(list: TGLuint; mode: TGLEnum); OPENGL_CALL
  glNormal3b: procedure(nx, ny, nz: TGLbyte); OPENGL_CALL
  glNormal3bv: procedure(v: PGLbyte); OPENGL_CALL
  glNormal3d: procedure(nx, ny, nz: TGLdouble); OPENGL_CALL
  glNormal3dv: procedure(v: PGLdouble); OPENGL_CALL
  glNormal3f: procedure(nx, ny, nz: TGLfloat); OPENGL_CALL
  glNormal3fv: procedure(v: PGLfloat); OPENGL_CALL
  glNormal3i: procedure(nx, ny, nz: TGLint); OPENGL_CALL
  glNormal3iv: procedure(v: PGLint); OPENGL_CALL
  glNormal3s: procedure(nx, ny, nz: TGLshort); OPENGL_CALL
  glNormal3sv: procedure(v: PGLshort); OPENGL_CALL
  glNormalPointer: procedure(atype: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: TGLdouble); OPENGL_CALL
  glPassThrough: procedure(token: TGLfloat); OPENGL_CALL
  glPixelMapfv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); OPENGL_CALL
  glPixelMapuiv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); OPENGL_CALL
  glPixelMapusv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); OPENGL_CALL
  glPixelStoref: procedure(pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glPixelStorei: procedure(pname: TGLEnum; param: TGLint); OPENGL_CALL
  glPixelTransferf: procedure(pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glPixelTransferi: procedure(pname: TGLEnum; param: TGLint); OPENGL_CALL
  glPixelZoom: procedure(xfactor, yfactor: TGLfloat); OPENGL_CALL
  glPointSize: procedure(size: TGLfloat); OPENGL_CALL
  glPolygonMode: procedure(face, mode: TGLEnum); OPENGL_CALL
  glPolygonOffset: procedure(factor, units: TGLfloat); OPENGL_CALL
  glPolygonStipple: procedure(mask: PPolygonStipple); OPENGL_CALL
  glPopAttrib: procedure; OPENGL_CALL
  glPopClientAttrib: procedure; OPENGL_CALL
  glPopMatrix: procedure; OPENGL_CALL
  glPopName: procedure; OPENGL_CALL
  glPrioritizeTextures: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); OPENGL_CALL
  glPushAttrib: procedure(mask: TGLbitfield); OPENGL_CALL
  glPushClientAttrib: procedure(mask: TGLbitfield); OPENGL_CALL
  glPushMatrix: procedure; OPENGL_CALL
  glPushName: procedure(name: TGLuint); OPENGL_CALL
  glRasterPos2d: procedure(x, y: TGLdouble); OPENGL_CALL
  glRasterPos2dv: procedure(v: PGLdouble); OPENGL_CALL
  glRasterPos2f: procedure(x, y: TGLfloat); OPENGL_CALL
  glRasterPos2fv: procedure(v: PGLfloat); OPENGL_CALL
  glRasterPos2i: procedure(x, y: TGLint); OPENGL_CALL
  glRasterPos2iv: procedure(v: PGLint); OPENGL_CALL
  glRasterPos2s: procedure(x, y: PGLshort); OPENGL_CALL
  glRasterPos2sv: procedure(v: PGLshort); OPENGL_CALL
  glRasterPos3d: procedure(x, y, z: TGLdouble); OPENGL_CALL
  glRasterPos3dv: procedure(v: PGLdouble); OPENGL_CALL
  glRasterPos3f: procedure(x, y, z: TGLfloat); OPENGL_CALL
  glRasterPos3fv: procedure(v: PGLfloat); OPENGL_CALL
  glRasterPos3i: procedure(x, y, z: TGLint); OPENGL_CALL
  glRasterPos3iv: procedure(v: PGLint); OPENGL_CALL
  glRasterPos3s: procedure(x, y, z: TGLshort); OPENGL_CALL
  glRasterPos3sv: procedure(v: PGLshort); OPENGL_CALL
  glRasterPos4d: procedure(x, y, z, w: TGLdouble); OPENGL_CALL
  glRasterPos4dv: procedure(v: PGLdouble); OPENGL_CALL
  glRasterPos4f: procedure(x, y, z, w: TGLfloat); OPENGL_CALL
  glRasterPos4fv: procedure(v: PGLfloat); OPENGL_CALL
  glRasterPos4i: procedure(x, y, z, w: TGLint); OPENGL_CALL
  glRasterPos4iv: procedure(v: PGLint); OPENGL_CALL
  glRasterPos4s: procedure(x, y, z, w: TGLshort); OPENGL_CALL
  glRasterPos4sv: procedure(v: PGLshort); OPENGL_CALL
  glReadBuffer: procedure(mode: TGLEnum); OPENGL_CALL
  glReadPixels: procedure(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glRectd: procedure(x1, y1, x2, y2: TGLdouble); OPENGL_CALL
  glRectdv: procedure(v1, v2: PGLdouble); OPENGL_CALL
  glRectf: procedure(x1, y1, x2, y2: TGLfloat); OPENGL_CALL
  glRectfv: procedure(v1, v2: PGLfloat); OPENGL_CALL
  glRecti: procedure(x1, y1, x2, y2: TGLint); OPENGL_CALL
  glRectiv: procedure(v1, v2: PGLint); OPENGL_CALL
  glRects: procedure(x1, y1, x2, y2: TGLshort); OPENGL_CALL
  glRectsv: procedure(v1, v2: PGLshort); OPENGL_CALL
  glRenderMode: function(mode: TGLEnum): TGLint; OPENGL_CALL
  glRotated: procedure(angle, x, y, z: TGLdouble); OPENGL_CALL
  glRotatef: procedure(angle, x, y, z: TGLfloat); OPENGL_CALL
  glScaled: procedure(x, y, z: TGLdouble); OPENGL_CALL
  glScalef: procedure(x, y, z: TGLfloat); OPENGL_CALL
  glScissor: procedure(x, y: TGLint; width, height: TGLsizei); OPENGL_CALL
  glSelectBuffer: procedure(size: TGLsizei; buffer: PGLuint); OPENGL_CALL
  glShadeModel: procedure(mode: TGLEnum); OPENGL_CALL
  glStencilFunc: procedure(func: TGLEnum; ref: TGLint; mask: TGLuint); OPENGL_CALL
  glStencilMask: procedure(mask: TGLuint); OPENGL_CALL
  glStencilOp: procedure(fail, zfail, zpass: TGLEnum); OPENGL_CALL
  glTexCoord1d: procedure(s: TGLdouble); OPENGL_CALL
  glTexCoord1dv: procedure(v: PGLdouble); OPENGL_CALL
  glTexCoord1f: procedure(s: TGLfloat); OPENGL_CALL
  glTexCoord1fv: procedure(v: PGLfloat); OPENGL_CALL
  glTexCoord1i: procedure(s: TGLint); OPENGL_CALL
  glTexCoord1iv: procedure(v: PGLint); OPENGL_CALL
  glTexCoord1s: procedure(s: TGLshort); OPENGL_CALL
  glTexCoord1sv: procedure(v: PGLshort); OPENGL_CALL
  glTexCoord2d: procedure(s, t: TGLdouble); OPENGL_CALL
  glTexCoord2dv: procedure(v: PGLdouble); OPENGL_CALL
  glTexCoord2f: procedure(s, t: TGLfloat); OPENGL_CALL
  glTexCoord2fv: procedure(v: PGLfloat); OPENGL_CALL
  glTexCoord2i: procedure(s, t: TGLint); OPENGL_CALL
  glTexCoord2iv: procedure(v: PGLint); OPENGL_CALL
  glTexCoord2s: procedure(s, t: TGLshort); OPENGL_CALL
  glTexCoord2sv: procedure(v: PGLshort); OPENGL_CALL
  glTexCoord3d: procedure(s, t, r: TGLdouble); OPENGL_CALL
  glTexCoord3dv: procedure(v: PGLdouble); OPENGL_CALL
  glTexCoord3f: procedure(s, t, r: TGLfloat); OPENGL_CALL
  glTexCoord3fv: procedure(v: PGLfloat); OPENGL_CALL
  glTexCoord3i: procedure(s, t, r: TGLint); OPENGL_CALL
  glTexCoord3iv: procedure(v: PGLint); OPENGL_CALL
  glTexCoord3s: procedure(s, t, r: TGLshort); OPENGL_CALL
  glTexCoord3sv: procedure(v: PGLshort); OPENGL_CALL
  glTexCoord4d: procedure(s, t, r, q: TGLdouble); OPENGL_CALL
  glTexCoord4dv: procedure(v: PGLdouble); OPENGL_CALL
  glTexCoord4f: procedure(s, t, r, q: TGLfloat); OPENGL_CALL
  glTexCoord4fv: procedure(v: PGLfloat); OPENGL_CALL
  glTexCoord4i: procedure(s, t, r, q: TGLint); OPENGL_CALL
  glTexCoord4iv: procedure(v: PGLint); OPENGL_CALL
  glTexCoord4s: procedure(s, t, r, q: TGLshort); OPENGL_CALL
  glTexCoord4sv: procedure(v: PGLshort); OPENGL_CALL
  glTexCoordPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glTexEnvf: procedure(target, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glTexEnvfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glTexEnvi: procedure(target, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glTexEnviv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glTexGend: procedure(coord, pname: TGLEnum; param: TGLdouble); OPENGL_CALL
  glTexGendv: procedure(coord, pname: TGLEnum; params: PGLdouble); OPENGL_CALL
  glTexGenf: procedure(coord, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glTexGeni: procedure(coord, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glTexImage1D: procedure(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,    atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glTexImage2D: procedure(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;     format, atype: TGLEnum; Pixels: Pointer); OPENGL_CALL
  glTexParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glTexParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glTexParameteri: procedure(target, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glTexParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glTexSubImage1D: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;     pixels: Pointer); OPENGL_CALL
  glTexSubImage2D: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,    atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glTranslated: procedure(x, y, z: TGLdouble); OPENGL_CALL
  glTranslatef: procedure(x, y, z: TGLfloat); OPENGL_CALL
  glVertex2d: procedure(x, y: TGLdouble); OPENGL_CALL
  glVertex2dv: procedure(v: PGLdouble); OPENGL_CALL
  glVertex2f: procedure(x, y: TGLfloat); OPENGL_CALL
  glVertex2fv: procedure(v: PGLfloat); OPENGL_CALL
  glVertex2i: procedure(x, y: TGLint); OPENGL_CALL
  glVertex2iv: procedure(v: PGLint); OPENGL_CALL
  glVertex2s: procedure(x, y: TGLshort); OPENGL_CALL
  glVertex2sv: procedure(v: PGLshort); OPENGL_CALL
  glVertex3d: procedure(x, y, z: TGLdouble); OPENGL_CALL
  glVertex3dv: procedure(v: PGLdouble); OPENGL_CALL
  glVertex3f: procedure(x, y, z: TGLfloat); OPENGL_CALL
  glVertex3fv: procedure(v: PGLfloat); OPENGL_CALL
  glVertex3i: procedure(x, y, z: TGLint); OPENGL_CALL
  glVertex3iv: procedure(v: PGLint); OPENGL_CALL
  glVertex3s: procedure(x, y, z: TGLshort); OPENGL_CALL
  glVertex3sv: procedure(v: PGLshort); OPENGL_CALL
  glVertex4d: procedure(x, y, z, w: TGLdouble); OPENGL_CALL
  glVertex4dv: procedure(v: PGLdouble); OPENGL_CALL
  glVertex4f: procedure(x, y, z, w: TGLfloat); OPENGL_CALL
  glVertex4fv: procedure(v: PGLfloat); OPENGL_CALL
  glVertex4i: procedure(x, y, z, w: TGLint); OPENGL_CALL
  glVertex4iv: procedure(v: PGLint); OPENGL_CALL
  glVertex4s: procedure(x, y, z, w: TGLshort); OPENGL_CALL
  glVertex4sv: procedure(v: PGLshort); OPENGL_CALL
  glVertexPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); OPENGL_CALL
  glViewport: procedure(x, y: TGLint; width, height: TGLsizei); OPENGL_CALL

  {$ifndef NO_GL_12}
  { GL 1.2 }
  glDrawRangeElements: procedure(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;     indices: Pointer); OPENGL_CALL
  glTexImage3D: procedure(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;     border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); OPENGL_CALL

  { GL 1.2 ARB imaging }
  glBlendColor: procedure(red, green, blue, alpha: TGLclampf); OPENGL_CALL
  glBlendEquation: procedure(mode: TGLEnum); OPENGL_CALL
  glColorSubTable: procedure(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); OPENGL_CALL
  glCopyColorSubTable: procedure(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); OPENGL_CALL
  glColorTable: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;     table: Pointer); OPENGL_CALL
  glCopyColorTable: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); OPENGL_CALL
  glColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetColorTable: procedure(target, format, Atype: TGLEnum; table: Pointer); OPENGL_CALL
  glGetColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glConvolutionFilter1D: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;     image: Pointer); OPENGL_CALL
  glConvolutionFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;     image: Pointer); OPENGL_CALL
  glCopyConvolutionFilter1D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); OPENGL_CALL
  glCopyConvolutionFilter2D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); OPENGL_CALL
  glGetConvolutionFilter: procedure(target, internalformat, Atype: TGLEnum; image: Pointer); OPENGL_CALL
  glSeparableFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,    column: Pointer); OPENGL_CALL
  glGetSeparableFilter: procedure(target, format, Atype: TGLEnum; row, column, span: Pointer); OPENGL_CALL
  glConvolutionParameteri: procedure(target, pname: TGLEnum; param: TGLint); OPENGL_CALL
  glConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glConvolutionParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); OPENGL_CALL
  glConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glGetConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glHistogram: procedure(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); OPENGL_CALL
  glResetHistogram: procedure(target: TGLEnum); OPENGL_CALL
  glGetHistogram: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); OPENGL_CALL
  glGetHistogramParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetHistogramParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  glMinmax: procedure(target, internalformat: TGLEnum; sink: TGLboolean); OPENGL_CALL
  glResetMinmax: procedure(target: TGLEnum); OPENGL_CALL
  glGetMinmax: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); OPENGL_CALL
  glGetMinmaxParameteriv: procedure(target, pname: TGLEnum; params: PGLint); OPENGL_CALL
  glGetMinmaxParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); OPENGL_CALL
  {$endif}

  { GL utility functions and procedures }
  gluErrorString: function(errCode: TGLEnum): PChar; OPENGL_CALL
  gluGetString: function(name: TGLEnum): PChar; OPENGL_CALL
  gluOrtho2D: procedure(left, right, bottom, top: TGLdouble); OPENGL_CALL
  gluPerspective: procedure(fovy, aspect, zNear, zFar: TGLdouble); OPENGL_CALL
  gluPickMatrix: procedure(x, y, width, height: TGLdouble; viewport: PVector4i); OPENGL_CALL
  gluLookAt: procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); OPENGL_CALL
  gluProject: function(objx, objy, objz: TGLdouble; modelMatrix: PMatrix4d; projMatrix: PMatrix4d; viewport: PVector4i;     winx, winy, winz: PGLdouble): TGLint; OPENGL_CALL
  gluUnProject: function(winx, winy, winz: TGLdouble; modelMatrix: PMatrix4d; projMatrix: PMatrix4d; viewport: PVector4i;     objx, objy, objz: PGLdouble): TGLint; OPENGL_CALL
  gluScaleImage: function(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,    heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; OPENGL_CALL
  gluBuild1DMipmaps: function(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;     data: Pointer): TGLint; OPENGL_CALL
  gluBuild2DMipmaps: function(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;     Data: Pointer): TGLint; OPENGL_CALL
  gluNewQuadric: function: PGLUquadric; OPENGL_CALL
  gluDeleteQuadric: procedure(state: PGLUquadric); OPENGL_CALL
  gluQuadricNormals: procedure(quadObject: PGLUquadric; normals: TGLEnum); OPENGL_CALL
  gluQuadricTexture: procedure(quadObject: PGLUquadric; textureCoords: TGLboolean); OPENGL_CALL
  gluQuadricOrientation: procedure(quadObject: PGLUquadric; orientation: TGLEnum); OPENGL_CALL
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadric; drawStyle: TGLEnum); OPENGL_CALL
  gluCylinder: procedure(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,    stacks: TGLint); OPENGL_CALL
  gluDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); OPENGL_CALL
  gluPartialDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;     startAngle, sweepAngle: TGLdouble); OPENGL_CALL
  gluSphere: procedure(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); OPENGL_CALL
  gluQuadricCallback: procedure(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); OPENGL_CALL
  gluNewTess: function: PGLUtesselator; OPENGL_CALL
  gluDeleteTess: procedure(tess: PGLUtesselator); OPENGL_CALL
  gluTessBeginPolygon: procedure(tess: PGLUtesselator; polygon_data: Pointer); OPENGL_CALL
  gluTessBeginContour: procedure(tess: PGLUtesselator); OPENGL_CALL
  gluTessVertex: procedure(tess: PGLUtesselator; coords: PVector3d; data: Pointer); OPENGL_CALL
  gluTessEndContour: procedure(tess: PGLUtesselator); OPENGL_CALL
  gluTessEndPolygon: procedure(tess: PGLUtesselator); OPENGL_CALL
  gluTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); OPENGL_CALL
  gluTessNormal: procedure(tess: PGLUtesselator; x, y, z: TGLdouble); OPENGL_CALL
  gluTessCallback: procedure(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); OPENGL_CALL
  gluGetTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); OPENGL_CALL
  gluNewNurbsRenderer: function: PGLUnurbs; OPENGL_CALL
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluBeginSurface: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluBeginCurve: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluEndCurve: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluEndSurface: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluBeginTrim: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluEndTrim: procedure(nobj: PGLUnurbs); OPENGL_CALL
  gluPwlCurve: procedure(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); OPENGL_CALL
  gluNurbsCurve: procedure(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); OPENGL_CALL
  gluNurbsSurface: procedure(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); OPENGL_CALL
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbs; modelMatrix, projMatrix: PMatrix4f; viewport: PVector4i); OPENGL_CALL
  gluNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); OPENGL_CALL
  gluGetNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); OPENGL_CALL
  gluNurbsCallback: procedure(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); OPENGL_CALL
  gluBeginPolygon: procedure(tess: PGLUtesselator); OPENGL_CALL
  gluNextContour: procedure(tess: PGLUtesselator; atype: TGLEnum); OPENGL_CALL
  gluEndPolygon: procedure(tess: PGLUtesselator); OPENGL_CALL

  { window support functions }
  {$IFDEF Win32}

type
  PWGLSwap = ^TWGLSwap;
  TWGLSwap = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;

var
  wglGetProcAddress: function(ProcName: PChar): Pointer; stdcall;
  wglCopyContext: function(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall;
  wglCreateContext: function(DC: HDC): HGLRC; stdcall;
  wglCreateLayerContext: function(p1: HDC; p2: Integer): HGLRC; stdcall;
  wglDeleteContext: function(p1: HGLRC): BOOL; stdcall;
  wglDescribeLayerPlane: function(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall;
  wglGetCurrentContext: function: HGLRC; stdcall;
  wglGetCurrentDC: function: HDC; stdcall;
  wglGetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  wglMakeCurrent: function(DC: HDC; p2: HGLRC): BOOL; stdcall;
  wglRealizeLayerPalette: function(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
  wglSetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  wglShareLists: function(p1, p2: HGLRC): BOOL; stdcall;
  wglSwapLayerBuffers: function(p1: HDC; p2: Cardinal): BOOL; stdcall;
  {$ifndef OLDER_WGL} wglSwapMultipleBuffers: function(p1: UINT; const p2: PWGLSwap): DWORD; stdcall; {$endif}
  wglUseFontBitmapsA: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlinesA: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  wglUseFontBitmapsW: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlinesW: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  wglUseFontBitmaps: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlines: function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  {$ENDIF}

  { ARB_multitexture }
  glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); OPENGL_CALL
  glMultiTexCoord1dVARB: procedure(target: TGLenum; v: PGLdouble); OPENGL_CALL
  glMultiTexCoord1fARBP: procedure(target: TGLenum; s: TGLfloat); OPENGL_CALL
  glMultiTexCoord1fVARB: procedure(target: TGLenum; v: TGLfloat); OPENGL_CALL
  glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); OPENGL_CALL
  glMultiTexCoord1iVARB: procedure(target: TGLenum; v: PGLInt); OPENGL_CALL
  glMultiTexCoord1sARBP: procedure(target: TGLenum; s: TGLshort); OPENGL_CALL
  glMultiTexCoord1sVARB: procedure(target: TGLenum; v: PGLshort); OPENGL_CALL
  glMultiTexCoord2dARB: procedure(target: TGLenum; s, t: TGLdouble); OPENGL_CALL
  glMultiTexCoord2dvARB: procedure(target: TGLenum; v: PGLdouble); OPENGL_CALL
  glMultiTexCoord2fARB: procedure(target: TGLenum; s, t: TGLfloat); OPENGL_CALL
  glMultiTexCoord2fvARB: procedure(target: TGLenum; v: PGLfloat); OPENGL_CALL
  glMultiTexCoord2iARB: procedure(target: TGLenum; s, t: TGLint); OPENGL_CALL
  glMultiTexCoord2ivARB: procedure(target: TGLenum; v: PGLint); OPENGL_CALL
  glMultiTexCoord2sARB: procedure(target: TGLenum; s, t: TGLshort); OPENGL_CALL
  glMultiTexCoord2svARB: procedure(target: TGLenum; v: PGLshort); OPENGL_CALL
  glMultiTexCoord3dARB: procedure(target: TGLenum; s, t, r: TGLdouble); OPENGL_CALL
  glMultiTexCoord3dvARB: procedure(target: TGLenum; v: PGLdouble); OPENGL_CALL
  glMultiTexCoord3fARB: procedure(target: TGLenum; s, t, r: TGLfloat); OPENGL_CALL
  glMultiTexCoord3fvARB: procedure(target: TGLenum; v: PGLfloat); OPENGL_CALL
  glMultiTexCoord3iARB: procedure(target: TGLenum; s, t, r: TGLint); OPENGL_CALL
  glMultiTexCoord3ivARB: procedure(target: TGLenum; v: PGLint); OPENGL_CALL
  glMultiTexCoord3sARB: procedure(target: TGLenum; s, t, r: TGLshort); OPENGL_CALL
  glMultiTexCoord3svARB: procedure(target: TGLenum; v: PGLshort); OPENGL_CALL
  glMultiTexCoord4dARB: procedure(target: TGLenum; s, t, r, q: TGLdouble); OPENGL_CALL
  glMultiTexCoord4dvARB: procedure(target: TGLenum; v: PGLdouble); OPENGL_CALL
  glMultiTexCoord4fARB: procedure(target: TGLenum; s, t, r, q: TGLfloat); OPENGL_CALL
  glMultiTexCoord4fvARB: procedure(target: TGLenum; v: PGLfloat); OPENGL_CALL
  glMultiTexCoord4iARB: procedure(target: TGLenum; s, t, r, q: TGLint); OPENGL_CALL
  glMultiTexCoord4ivARB: procedure(target: TGLenum; v: PGLint); OPENGL_CALL
  glMultiTexCoord4sARB: procedure(target: TGLenum; s, t, r, q: TGLshort); OPENGL_CALL
  glMultiTexCoord4svARB: procedure(target: TGLenum; v: PGLshort); OPENGL_CALL
  glActiveTextureARB: procedure(target: TGLenum); OPENGL_CALL
  glClientActiveTextureARB: procedure(target: TGLenum); OPENGL_CALL

  { GLU extensions }
  gluNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer); OPENGL_CALL
  gluNewNurbsTessellatorEXT: function: PGLUnurbs; OPENGL_CALL
  gluDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs); OPENGL_CALL

  { Extension functions }
  glAreTexturesResidentEXT: function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; OPENGL_CALL
  glArrayElementArrayEXT: procedure(mode: TGLEnum; count: TGLsizei; pi: Pointer); OPENGL_CALL
  glBeginSceneEXT: procedure; OPENGL_CALL
  glBindTextureEXT: procedure(target: TGLEnum; texture: TGLuint); OPENGL_CALL
  glColorTableEXT: procedure(target, internalFormat: TGLEnum; width: TGLsizei; format, atype: TGLEnum; data: Pointer); OPENGL_CALL
  glColorSubTableExt: procedure(target: TGLEnum; start, count: TGLsizei; format, atype: TGLEnum; data: Pointer); OPENGL_CALL
  glCopyTexImage1DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); OPENGL_CALL
  glCopyTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); OPENGL_CALL
  glCopyTexImage2DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); OPENGL_CALL
  glCopyTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); OPENGL_CALL
  glCopyTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); OPENGL_CALL
  glDeleteTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); OPENGL_CALL
  glEndSceneEXT: procedure; OPENGL_CALL
  glGenTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); OPENGL_CALL
  glGetColorTableEXT: procedure(target, format, atype: TGLEnum; data: Pointer); OPENGL_CALL
  glGetColorTablePameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); OPENGL_CALL
  glGetColorTablePameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); OPENGL_CALL
  glIndexFuncEXT: procedure(func: TGLEnum; ref: TGLfloat); OPENGL_CALL
  glIndexMaterialEXT: procedure(face: TGLEnum; mode: TGLEnum); OPENGL_CALL
  glIsTextureEXT: function(texture: TGLuint): TGLboolean; OPENGL_CALL
  glLockArraysEXT: procedure(first: TGLint; count: TGLsizei); OPENGL_CALL
  glPolygonOffsetEXT: procedure(factor, bias: TGLfloat); OPENGL_CALL
  glPrioritizeTexturesEXT: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); OPENGL_CALL
  glTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); OPENGL_CALL
  glUnlockArraysEXT: procedure; OPENGL_CALL

  { EXT_vertex_array }
  glArrayElementEXT: procedure(I: TGLint); OPENGL_CALL
  glColorPointerEXT: procedure(size: TGLInt; atype: TGLenum; stride, count: TGLsizei; data: Pointer); OPENGL_CALL
  glDrawArraysEXT: procedure(mode: TGLenum; first: TGLInt; count: TGLsizei); OPENGL_CALL
  glEdgeFlagPointerEXT: procedure(stride, count: TGLsizei; data: PGLboolean); OPENGL_CALL
  glGetPointervEXT: procedure(pname: TGLEnum; var params); OPENGL_CALL
  glIndexPointerEXT: procedure(AType: TGLEnum; stride, count: TGLsizei; P: Pointer); OPENGL_CALL
  glNormalPointerEXT: procedure(AType: TGLsizei; stride, count: TGLsizei; P: Pointer); OPENGL_CALL
  glTexCoordPointerEXT: procedure(size: TGLint; AType: TGLenum;  stride, count: TGLsizei; P: Pointer); OPENGL_CALL
  glVertexPointerEXT: procedure(size: TGLint; AType: TGLenum; stride, count: TGLsizei; P: Pointer); OPENGL_CALL

  { EXT_compiled_vertex_array }
  glLockArrayEXT: procedure(first: TGLint; count: TGLsizei); OPENGL_CALL
  glUnlockArrayEXT: procedure; OPENGL_CALL

  { EXT_cull_vertex }
  glCullParameterdvEXT: procedure(pname: TGLenum; params: PGLdouble); OPENGL_CALL
  glCullParameterfvEXT: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { WIN_swap_hint }
  glAddSwapHintRectWIN: procedure(x, y: TGLint; width, height: TGLsizei); OPENGL_CALL

  { EXT_point_parameter }
  glPointParameterfEXT: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glPointParameterfvEXT: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_ARB_transpose_matrix }
  glLoadTransposeMatrixfARB: procedure(m: PGLfloat); OPENGL_CALL
  glLoadTransposeMatrixdARB: procedure(m: PGLdouble); OPENGL_CALL
  glMultTransposeMatrixfARB: procedure(m: PGLfloat); OPENGL_CALL
  glMultTransposeMatrixdARB: procedure(m: PGLdouble); OPENGL_CALL

  { GL_ARB_multisample }
  glSampleCoverageARB: procedure(Value: TGLclampf; invert: TGLboolean); OPENGL_CALL
  glSamplePassARB: procedure(pass: TGLenum); OPENGL_CALL

  { GL_ARB_texture_compression }
  glCompressedTexImage3DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glCompressedTexImage2DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glCompressedTexImage1DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glCompressedTexSubImage3DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glCompressedTexSubImage2DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glCompressedTexSubImage1DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); OPENGL_CALL
  glGetCompressedTexImageARB: procedure(target: TGLenum; level: TGLint; img: pointer); OPENGL_CALL

  { GL_EXT_blend_color }
  glBlendColorEXT: procedure(red, green, blue: TGLclampf; alpha: TGLclampf); OPENGL_CALL

  { GL_EXT_texture3D }
  glTexImage3DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width, height, depth: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); OPENGL_CALL

  { GL_SGIS_texture_filter4 }
  glGetTexFilterFuncSGIS: procedure(target, Filter: TGLenum; weights: PGLfloat); OPENGL_CALL
  glTexFilterFuncSGIS: procedure(target, Filter: TGLenum; n: TGLsizei; weights: PGLfloat); OPENGL_CALL

  { GL_EXT_histogram }
  glGetHistogramEXT: procedure(target: TGLenum; reset: TGLboolean; Format, AType: TGLenum; values: Pointer); OPENGL_CALL
  glGetHistogramParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetHistogramParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetMinmaxEXT: procedure(target: TGLenum; reset: TGLboolean; Format, AType: TGLenum; values: Pointer); OPENGL_CALL
  glGetMinmaxParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetMinmaxParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glHistogramEXT: procedure(target: TGLenum; Width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); OPENGL_CALL
  glMinmaxEXT: procedure(target, internalformat: TGLenum; sink: TGLboolean); OPENGL_CALL
  glResetHistogramEXT: procedure(target: TGLenum); OPENGL_CALL
  glResetMinmaxEXT: procedure(target: TGLenum); OPENGL_CALL

  { GL_EXT_convolution }
  glConvolutionFilter1DEXT: procedure(target, internalformat: TGLenum; Width: TGLsizei; Format, AType: TGLenum; image: Pointer); OPENGL_CALL
  glConvolutionFilter2DEXT: procedure(target, internalformat: TGLenum; Width, Height: TGLsizei; Format, AType: TGLenum; image: Pointer); OPENGL_CALL
  glConvolutionParameterfEXT: procedure(target, pname: TGLenum; params: TGLfloat); OPENGL_CALL
  glConvolutionParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glConvolutionParameteriEXT: procedure(target, pname: TGLenum; params: TGLint); OPENGL_CALL
  glConvolutionParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glCopyConvolutionFilter1DEXT: procedure(target, internalformat: TGLenum; x, y: TGLint; Width: TGLsizei); OPENGL_CALL
  glCopyConvolutionFilter2DEXT: procedure(target, internalformat: TGLenum; x, y: TGLint; Width, Height: TGLsizei); OPENGL_CALL
  glGetConvolutionFilterEXT: procedure(target, Format, AType: TGLenum; image: pointer); OPENGL_CALL
  glGetConvolutionParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetConvolutionParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetSeparableFilterEXT: procedure(target, Format, AType: TGLenum; row, column, span: Pointer); OPENGL_CALL
  glSeparableFilter2DEXT: procedure(target, internalformat: TGLenum; Width, Height: TGLsizei; Format, AType: TGLenum; row, column: Pointer); OPENGL_CALL

  { GL_SGI_color_table }
  glColorTableSGI: procedure(target, internalformat: TGLenum; Width: TGLsizei; Format, AType: TGLenum; Table: pointer); OPENGL_CALL
  glColorTableParameterfvSGI: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glColorTableParameterivSGI: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glCopyColorTableSGI: procedure(target, internalformat: TGLenum; x, y: TGLint; Width: TGLsizei); OPENGL_CALL
  glGetColorTableSGI: procedure(target, Format, AType: TGLenum; Table: Pointer); OPENGL_CALL
  glGetColorTableParameterfvSGI: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetColorTableParameterivSGI: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL

  { GL_SGIX_pixel_texture }
  glPixelTexGenSGIX: procedure(mode: TGLenum); OPENGL_CALL

  { GL_SGIS_pixel_texture }
  glPixelTexGenParameteriSGIS: procedure(pname: TGLenum; param: TGLint); OPENGL_CALL
  glPixelTexGenParameterivSGIS: procedure(pname: TGLenum; params: PGLint); OPENGL_CALL
  glPixelTexGenParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetPixelTexGenParameterivSGIS: procedure(pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_SGIS_texture4D }
  glTexImage4DSGIS: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth, size4d: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); OPENGL_CALL
  glTexSubImage4DSGIS: procedure(target: TGLenum; level, xoffset, yoffset, zoffset, woffset: TGLint; Width, Height, depth, size4d: TGLsizei; Format, AType: TGLenum; pixels: Pointer); OPENGL_CALL

  { GL_SGIS_detail_texture }
  glDetailTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; points: PGLfloat); OPENGL_CALL
  glGetDetailTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); OPENGL_CALL

  { GL_SGIS_sharpen_texture }
  glSharpenTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; points: PGLfloat); OPENGL_CALL
  glGetSharpenTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); OPENGL_CALL

  { GL_SGIS_multisample }
  glSampleMaskSGIS: procedure(Value: TGLclampf; invert: TGLboolean); OPENGL_CALL
  glSamplePatternSGIS: procedure(pattern: TGLenum); OPENGL_CALL

  { GL_EXT_blend_minmax }
  glBlendEquationEXT: procedure(mode: TGLenum); OPENGL_CALL

  { GL_SGIX_sprite }
  glSpriteParameterfSGIX: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glSpriteParameterfvSGIX: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glSpriteParameteriSGIX: procedure(pname: TGLenum; param: TGLint); OPENGL_CALL
  glSpriteParameterivSGIX: procedure(pname: TGLenum; params: PGLint); OPENGL_CALL

  { GL_EXT_point_parameters }
  glPointParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glPointParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_SGIX_instruments }
  glGetInstrumentsSGIX: procedure; OPENGL_CALL
  glInstrumentsBufferSGIX: procedure(Size: TGLsizei; buffer: PGLint); OPENGL_CALL
  glPollInstrumentsSGIX: procedure(marker_p: PGLint); OPENGL_CALL
  glReadInstrumentsSGIX: procedure(marker: TGLint); OPENGL_CALL
  glStartInstrumentsSGIX: procedure; OPENGL_CALL
  glStopInstrumentsSGIX: procedure(marker: TGLint); OPENGL_CALL

  { GL_SGIX_framezoom }
  glFrameZoomSGIX: procedure(factor: TGLint); OPENGL_CALL

  { GL_SGIX_tag_sample_buffer }
  glTagSampleBufferSGIX: procedure; OPENGL_CALL

  { GL_SGIX_polynomial_ffd }
  glDeformationMap3dSGIX: procedure(target: TGLenum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride, vorder: TGLint; w1, w2: TGLdouble; wstride, worder: TGLint; points: PGLdouble); OPENGL_CALL
  glDeformationMap3fSGIX: procedure(target: TGLenum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride, vorder: TGLint; w1, w2: TGLfloat; wstride, worder: TGLint; points: PGLfloat); OPENGL_CALL
  glDeformSGIX: procedure(mask: TGLbitfield); OPENGL_CALL
  glLoadIdentityDeformationMapSGIX: procedure(mask: TGLbitfield); OPENGL_CALL

  { GL_SGIX_reference_plane }
  glReferencePlaneSGIX: procedure(equation: PGLdouble); OPENGL_CALL

  { GL_SGIX_flush_raster }
  glFlushRasterSGIX: procedure; OPENGL_CALL

  { GL_SGIS_fog_function }
  glFogFuncSGIS: procedure(n: TGLsizei; points: PGLfloat); OPENGL_CALL
  glGetFogFuncSGIS: procedure(points: PGLfloat); OPENGL_CALL

  { GL_HP_image_transform }
  glImageTransformParameteriHP: procedure(target, pname: TGLenum; param: TGLint); OPENGL_CALL
  glImageTransformParameterfHP: procedure(target, pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glImageTransformParameterivHP: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glImageTransformParameterfvHP: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetImageTransformParameterivHP: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetImageTransformParameterfvHP: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_EXT_color_subtable }
  glCopyColorSubTableEXT: procedure(target: TGLenum; start: TGLsizei; x, y: TGLint; Width: TGLsizei); OPENGL_CALL

  { GL_PGI_misc_hints }
  glHintPGI: procedure(target: TGLenum; mode: TGLint); OPENGL_CALL

  { GL_EXT_paletted_texture }
  glGetColorTableParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetColorTableParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_SGIX_list_priority }
  glGetListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLint); OPENGL_CALL
  glListParameterfSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glListParameteriSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLint); OPENGL_CALL
  glListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLint); OPENGL_CALL

  { GL_SGIX_fragment_lighting }
  glFragmentColorMaterialSGIX: procedure(face, mode: TGLenum); OPENGL_CALL
  glFragmentLightfSGIX: procedure(light, pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glFragmentLightfvSGIX: procedure(light, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glFragmentLightiSGIX: procedure(light, pname: TGLenum; param: TGLint); OPENGL_CALL
  glFragmentLightivSGIX: procedure(light, pname: TGLenum; params: PGLint); OPENGL_CALL
  glFragmentLightModelfSGIX: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glFragmentLightModelfvSGIX: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glFragmentLightModeliSGIX: procedure(pname: TGLenum; param: TGLint); OPENGL_CALL
  glFragmentLightModelivSGIX: procedure(pname: TGLenum; params: PGLint); OPENGL_CALL
  glFragmentMaterialfSGIX: procedure(face, pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glFragmentMaterialfvSGIX: procedure(face, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glFragmentMaterialiSGIX: procedure(face, pname: TGLenum; param: TGLint); OPENGL_CALL
  glFragmentMaterialivSGIX: procedure(face, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetFragmentLightfvSGIX: procedure(light, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetFragmentLightivSGIX: procedure(light, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetFragmentMaterialfvSGIX: procedure(face, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetFragmentMaterialivSGIX: procedure(face, pname: TGLenum; params: PGLint); OPENGL_CALL
  glLightEnviSGIX: procedure(pname: TGLenum; param: TGLint); OPENGL_CALL

  { GL_EXT_draw_range_elements }
  glDrawRangeElementsEXT: procedure(mode: TGLenum; start, Aend: TGLuint; Count: TGLsizei; Atype: TGLenum; indices: Pointer); OPENGL_CALL

  { GL_EXT_light_texture }
  glApplyTextureEXT: procedure(mode: TGLenum); OPENGL_CALL
  glTextureLightEXT: procedure(pname: TGLenum); OPENGL_CALL
  glTextureMaterialEXT: procedure(face, mode: TGLenum); OPENGL_CALL

  { GL_SGIX_async }
  glAsyncMarkerSGIX: procedure(marker: TGLuint); OPENGL_CALL
  glFinishAsyncSGIX: procedure(markerp: PGLuint); OPENGL_CALL
  glPollAsyncSGIX: procedure(markerp: PGLuint); OPENGL_CALL
  glGenAsyncMarkersSGIX: procedure(range: TGLsizei); OPENGL_CALL
  glDeleteAsyncMarkersSGIX: procedure(marker: TGLuint; range: TGLsizei); OPENGL_CALL
  glIsAsyncMarkerSGIX: procedure(marker: TGLuint); OPENGL_CALL

  { GL_INTEL_parallel_arrays }
  glVertexPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); OPENGL_CALL
  glNormalPointervINTEL: procedure(Atype: TGLenum; var P); OPENGL_CALL
  glColorPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); OPENGL_CALL
  glTexCoordPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); OPENGL_CALL

  { GL_EXT_pixel_transform }
  glPixelTransformParameteriEXT: procedure(target, pname: TGLenum; param: TGLint); OPENGL_CALL
  glPixelTransformParameterfEXT: procedure(target, pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glPixelTransformParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); OPENGL_CALL
  glPixelTransformParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); OPENGL_CALL

  { GL_EXT_secondary_color }
  glSecondaryColor3bEXT: procedure(red, green, blue: TGLbyte); OPENGL_CALL
  glSecondaryColor3bvEXT: procedure(v: PGLbyte); OPENGL_CALL
  glSecondaryColor3dEXT: procedure(red, green, blue: TGLdouble); OPENGL_CALL
  glSecondaryColor3dvEXT: procedure(v: PGLdouble); OPENGL_CALL
  glSecondaryColor3fEXT: procedure(red, green, blue: TGLfloat); OPENGL_CALL
  glSecondaryColor3fvEXT: procedure(v: PGLfloat); OPENGL_CALL
  glSecondaryColor3iEXT: procedure(red, green, blue: TGLint); OPENGL_CALL
  glSecondaryColor3ivEXT: procedure(v: PGLint); OPENGL_CALL

  glSecondaryColor3sEXT: procedure(red, green, blue: TGLshort); OPENGL_CALL
  glSecondaryColor3svEXT: procedure(v: PGLshort); OPENGL_CALL
  glSecondaryColor3ubEXT: procedure(red, green, blue: TGLubyte); OPENGL_CALL
  glSecondaryColor3ubvEXT: procedure(v: PGLubyte); OPENGL_CALL
  glSecondaryColor3uiEXT: procedure(red, green, blue: TGLuint); OPENGL_CALL
  glSecondaryColor3uivEXT: procedure(v: PGLuint); OPENGL_CALL
  glSecondaryColor3usEXT: procedure(red, green, blue: TGLushort); OPENGL_CALL
  glSecondaryColor3usvEXT: procedure(v: PGLushort); OPENGL_CALL
  glSecondaryColorPointerEXT: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); OPENGL_CALL

  { GL_EXT_texture_perturb_normal }
  glTextureNormalEXT: procedure(mode: TGLenum); OPENGL_CALL

  { GL_EXT_multi_draw_arrays }
  glMultiDrawArraysEXT: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); OPENGL_CALL
  glMultiDrawElementsEXT: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); OPENGL_CALL

  { GL_EXT_fog_coord }
  glFogCoordfEXT: procedure(coord: TGLfloat); OPENGL_CALL
  glFogCoordfvEXT: procedure(coord: PGLfloat); OPENGL_CALL
  glFogCoorddEXT: procedure(coord: TGLdouble); OPENGL_CALL
  glFogCoorddvEXT: procedure(coord: PGLdouble); OPENGL_CALL
  glFogCoordPointerEXT: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); OPENGL_CALL

  { GL_EXT_coordinate_frame }
  glTangent3bEXT: procedure(tx, ty, tz: TGLbyte); OPENGL_CALL
  glTangent3bvEXT: procedure(v: PGLbyte); OPENGL_CALL
  glTangent3dEXT: procedure(tx, ty, tz: TGLdouble); OPENGL_CALL
  glTangent3dvEXT: procedure(v: PGLdouble); OPENGL_CALL
  glTangent3fEXT: procedure(tx, ty, tz: TGLfloat); OPENGL_CALL
  glTangent3fvEXT: procedure(v: PGLfloat); OPENGL_CALL
  glTangent3iEXT: procedure(tx, ty, tz: TGLint); OPENGL_CALL
  glTangent3ivEXT: procedure(v: PGLint); OPENGL_CALL
  glTangent3sEXT: procedure(tx, ty, tz: TGLshort); OPENGL_CALL
  glTangent3svEXT: procedure(v: PGLshort); OPENGL_CALL

  glBinormal3bEXT: procedure(bx, by, bz: TGLbyte); OPENGL_CALL
  glBinormal3bvEXT: procedure(v: PGLbyte); OPENGL_CALL
  glBinormal3dEXT: procedure(bx, by, bz: TGLdouble); OPENGL_CALL
  glBinormal3dvEXT: procedure(v: PGLdouble); OPENGL_CALL
  glBinormal3fEXT: procedure(bx, by, bz: TGLfloat); OPENGL_CALL
  glBinormal3fvEXT: procedure(v: PGLfloat); OPENGL_CALL
  glBinormal3iEXT: procedure(bx, by, bz: TGLint); OPENGL_CALL
  glBinormal3ivEXT: procedure(v: PGLint); OPENGL_CALL
  glBinormal3sEXT: procedure(bx, by, bz: TGLshort); OPENGL_CALL
  glBinormal3svEXT: procedure(v: PGLshort); OPENGL_CALL
  glTangentPointerEXT: procedure(Atype: TGLenum; stride: TGLsizei; p: Pointer); OPENGL_CALL
  glBinormalPointerEXT: procedure(Atype: TGLenum; stride: TGLsizei; p: Pointer); OPENGL_CALL

  { GL_SUNX_constant_data }
  glFinishTextureSUNX: procedure; OPENGL_CALL

  { GL_SUN_global_alpha }
  glGlobalAlphaFactorbSUN: procedure(factor: TGLbyte); OPENGL_CALL
  glGlobalAlphaFactorsSUN: procedure(factor: TGLshort); OPENGL_CALL
  glGlobalAlphaFactoriSUN: procedure(factor: TGLint); OPENGL_CALL
  glGlobalAlphaFactorfSUN: procedure(factor: TGLfloat); OPENGL_CALL
  glGlobalAlphaFactordSUN: procedure(factor: TGLdouble); OPENGL_CALL
  glGlobalAlphaFactorubSUN: procedure(factor: TGLubyte); OPENGL_CALL
  glGlobalAlphaFactorusSUN: procedure(factor: TGLushort); OPENGL_CALL
  glGlobalAlphaFactoruiSUN: procedure(factor: TGLuint); OPENGL_CALL

  { GL_SUN_triangle_list }
  glReplacementCodeuiSUN: procedure(code: TGLuint); OPENGL_CALL
  glReplacementCodeusSUN: procedure(code: TGLushort); OPENGL_CALL
  glReplacementCodeubSUN: procedure(code: TGLubyte); OPENGL_CALL
  glReplacementCodeuivSUN: procedure(code: PGLuint); OPENGL_CALL
  glReplacementCodeusvSUN: procedure(code: PGLushort); OPENGL_CALL
  glReplacementCodeubvSUN: procedure(code: PGLubyte); OPENGL_CALL
  glReplacementCodePointerSUN: procedure(Atype: TGLenum; stride: TGLsizei; var p); OPENGL_CALL

  { GL_SUN_vertex }
  glColor4ubVertex2fSUN: procedure(r, g, b, a: TGLubyte; x, y: TGLfloat); OPENGL_CALL
  glColor4ubVertex2fvSUN: procedure(c: PGLubyte; v: PGLfloat); OPENGL_CALL
  glColor4ubVertex3fSUN: procedure(r, g, b, a: TGLubyte; x, y, z: TGLfloat); OPENGL_CALL
  glColor4ubVertex3fvSUN: procedure(c: PGLubyte; v: PGLfloat); OPENGL_CALL
  glColor3fVertex3fSUN: procedure(r, g, b, x, y, z: TGLfloat); OPENGL_CALL
  glColor3fVertex3fvSUN: procedure(c, v: PGLfloat); OPENGL_CALL
  glNormal3fVertex3fSUN: procedure(nx, ny, nz: TGLfloat; x, y, z: TGLfloat); OPENGL_CALL
  glNormal3fVertex3fvSUN: procedure(n, v: PGLfloat); OPENGL_CALL
  glColor4fNormal3fVertex3fSUN: procedure(r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glColor4fNormal3fVertex3fvSUN: procedure(c, n, v: PGLfloat); OPENGL_CALL
  glTexCoord2fVertex3fSUN: procedure(s, t, x, y, z: TGLfloat); OPENGL_CALL
  glTexCoord2fVertex3fvSUN: procedure(tc, v: PGLfloat); OPENGL_CALL
  glTexCoord4fVertex4fSUN: procedure(s, t, p, q, x, y, z, w: TGLfloat); OPENGL_CALL
  glTexCoord4fVertex4fvSUN: procedure(tc, v: PGLfloat); OPENGL_CALL
  glTexCoord2fColor4ubVertex3fSUN: procedure(s, t, r, g, b, a, x, y, z: TGLfloat); OPENGL_CALL
  glTexCoord2fColor4ubVertex3fvSUN: procedure(tc: PGLfloat; c: PGLubyte; v: PGLfloat); OPENGL_CALL
  glTexCoord2fColor3fVertex3fSUN: procedure(s, t, r, g, b, x, y, z: TGLfloat); OPENGL_CALL
  glTexCoord2fColor3fVertex3fvSUN: procedure(tc, c, v: PGLfloat); OPENGL_CALL
  glTexCoord2fNormal3fVertex3fSUN: procedure(s, t, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glTexCoord2fNormal3fVertex3fvSUN: procedure(tc, n, v: PGLfloat); OPENGL_CALL
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s, t, r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(tc, c, n, v: PGLfloat); OPENGL_CALL
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s, t, p, q, r, g, b, a, nx, ny, nz, x, y, z, w: TGLfloat); OPENGL_CALL
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(tc, c, n, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiVertex3fSUN: procedure(rc: TGLenum; x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiVertex3fvSUN: procedure(rc: PGLenum; v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: TGLenum; r, g, b, a: TGLubyte; x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(rc: PGLenum; c: PGLubyte; v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: TGLenum; r, g, b, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(rc: PGLenum; c, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: TGLenum; nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(rc: PGLenum; n, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: TGLenum; r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(rc: PGLenum; c, n, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: TGLenum; s, t, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(rc: PGLenum; tc, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: TGLenum; s, t, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(rc: PGLenum; tc, n, v: PGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: TGLenum; s, t, r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); OPENGL_CALL
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(rc: PGLenum; tc, c, n, v: PGLfloat); OPENGL_CALL

  { GL_EXT_blend_func_separate }
  glBlendFuncSeparateEXT: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); OPENGL_CALL

  { GL_EXT_vertex_weighting }
  glVertexWeightfEXT: procedure(weight: TGLfloat); OPENGL_CALL
  glVertexWeightfvEXT: procedure(weight: PGLfloat); OPENGL_CALL
  glVertexWeightPointerEXT: procedure(Size: TGLsizei; Atype: TGLenum; stride: TGLsizei; p: pointer); OPENGL_CALL

  { GL_NV_vertex_array_range }
  glFlushVertexArrayRangeNV: procedure; OPENGL_CALL
  glVertexArrayRangeNV: procedure(Size: TGLsizei; p: pointer); OPENGL_CALL
  wglAllocateMemoryNV: function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; OPENGL_CALL
  wglFreeMemoryNV: procedure(ptr: Pointer); OPENGL_CALL

  { GL_NV_register_combiners }
  glCombinerParameterfvNV: procedure(pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glCombinerParameterfNV: procedure(pname: TGLenum; param: TGLfloat); OPENGL_CALL
  glCombinerParameterivNV: procedure(pname: TGLenum; params: PGLint); OPENGL_CALL
  glCombinerParameteriNV: procedure(pname: TGLenum; param: TGLint); OPENGL_CALL
  glCombinerInputNV: procedure(stage, portion, variable, input, mapping, componentUsage: TGLenum); OPENGL_CALL
  glCombinerOutputNV: procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: TGLenum; abDotProduct, cdDotProduct, muxSum: TGLboolean); OPENGL_CALL
  glFinalCombinerInputNV: procedure(variable, input, mapping, componentUsage: TGLenum); OPENGL_CALL
  glGetCombinerInputParameterfvNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetCombinerInputParameterivNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetFinalCombinerInputParameterfvNV: procedure(variable, pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetFinalCombinerInputParameterivNV: procedure(variable, pname: TGLenum; params: PGLint); OPENGL_CALL

  { GL_MESA_resize_buffers }
  glResizeBuffersMESA: procedure; OPENGL_CALL

  { GL_MESA_window_pos }
  glWindowPos2dMESA: procedure(x, y: TGLdouble); OPENGL_CALL
  glWindowPos2dvMESA: procedure(v: PGLdouble); OPENGL_CALL
  glWindowPos2fMESA: procedure(x, y: TGLfloat); OPENGL_CALL
  glWindowPos2fvMESA: procedure(v: PGLfloat); OPENGL_CALL
  glWindowPos2iMESA: procedure(x, y: TGLint); OPENGL_CALL
  glWindowPos2ivMESA: procedure(v: PGLint); OPENGL_CALL
  glWindowPos2sMESA: procedure(x, y: TGLshort); OPENGL_CALL
  glWindowPos2svMESA: procedure(v: PGLshort); OPENGL_CALL
  glWindowPos3dMESA: procedure(x, y, z: TGLdouble); OPENGL_CALL
  glWindowPos3dvMESA: procedure(v: PGLdouble); OPENGL_CALL
  glWindowPos3fMESA: procedure(x, y, z: TGLfloat); OPENGL_CALL
  glWindowPos3fvMESA: procedure(v: PGLfloat); OPENGL_CALL
  glWindowPos3iMESA: procedure(x, y, z: TGLint); OPENGL_CALL
  glWindowPos3ivMESA: procedure(v: PGLint); OPENGL_CALL
  glWindowPos3sMESA: procedure(x, y, z: TGLshort); OPENGL_CALL
  glWindowPos3svMESA: procedure(v: PGLshort); OPENGL_CALL
  glWindowPos4dMESA: procedure(x, y, z, w: TGLdouble); OPENGL_CALL
  glWindowPos4dvMESA: procedure(v: PGLdouble); OPENGL_CALL
  glWindowPos4fMESA: procedure(x, y, z, w: TGLfloat); OPENGL_CALL
  glWindowPos4fvMESA: procedure(v: PGLfloat); OPENGL_CALL
  glWindowPos4iMESA: procedure(x, y, z, w: TGLint); OPENGL_CALL
  glWindowPos4ivMESA: procedure(v: PGLint); OPENGL_CALL
  glWindowPos4sMESA: procedure(x, y, z, w: TGLshort); OPENGL_CALL
  glWindowPos4svMESA: procedure(v: PGLshort); OPENGL_CALL

  { GL_IBM_multimode_draw_arrays }
  glMultiModeDrawArraysIBM: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei; modestride: TGLint); OPENGL_CALL
  glMultiModeDrawElementsIBM: procedure(mode: PGLenum; Count: PGLsizei; Atype: TGLenum; var indices; primcount: TGLsizei; modestride: TGLint); OPENGL_CALL

  { GL_IBM_vertex_array_lists }
  glColorPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glSecondaryColorPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glEdgeFlagPointerListIBM: procedure(stride: TGLint; var p: PGLboolean; ptrstride: TGLint); OPENGL_CALL
  glFogCoordPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glIndexPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glNormalPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glTexCoordPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL
  glVertexPointerListIBM:   procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); OPENGL_CALL

  { GL_3DFX_tbuffer }
  glTbufferMask3DFX: procedure(mask: TGLuint); OPENGL_CALL

  { GL_EXT_multisample }
  glSampleMaskEXT: procedure(Value: TGLclampf; invert: TGLboolean); OPENGL_CALL
  glSamplePatternEXT: procedure(pattern: TGLenum); OPENGL_CALL

  { GL_SGIS_texture_color_mask }
  glTextureColorMaskSGIS: procedure(red, green, blue, alpha: TGLboolean); OPENGL_CALL

  { GL_SGIX_igloo_interface }
  glIglooInterfaceSGIX: procedure(pname: TGLenum; params: pointer); OPENGL_CALL

  { GL_NV_vertex_program }
  glAreProgramsResidentNV: procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); OPENGL_CALL
  glBindProgramNV: procedure(target: TGLenum; id: TGLuint); OPENGL_CALL
  glDeleteProgramsNV: procedure(n: TGLSizei; programs: PGLuint); OPENGL_CALL
  glExecuteProgramNV: procedure(target: TGLenum; id: TGLuint; params: PGLfloat); OPENGL_CALL
  glGenProgramsNV: procedure(n: TGLSizei; programs: PGLuint); OPENGL_CALL
  glGetProgramParameterdvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); OPENGL_CALL
  glGetProgramParameterfvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetProgramivNV: procedure (id: TGLuint; pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetProgramStringNV: procedure (id: TGLuint; pname: TGLenum; programIdx: PGLubyte); OPENGL_CALL
  glGetTrackMatrixivNV: procedure (target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetVertexAttribdvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLdouble); OPENGL_CALL
  glGetVertexAttribfvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLfloat); OPENGL_CALL
  glGetVertexAttribivNV: procedure (index: TGLuint; pname: TGLenum; params: PGLint); OPENGL_CALL
  glGetVertexAttribPointervNV: procedure (index: TGLuint; pname: TGLenum; pointer: PPointer); OPENGL_CALL
  glIsProgramNV: function (id: TGLuint): TGLboolean; OPENGL_CALL
  glLoadProgramNV: procedure (target: TGLenum; id: TGLuint; len: TGLSizei; programIdx: PGLubyte); OPENGL_CALL
  glProgramParameter4dNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLdouble); OPENGL_CALL
  glProgramParameter4dvNV: procedure (target: TGLenum; index: TGLuint; v: PGLdouble ); OPENGL_CALL
  glProgramParameter4fNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLfloat); OPENGL_CALL
  glProgramParameter4fvNV: procedure (target: TGLenum; index: TGLuint; v: PGLfloat); OPENGL_CALL
  glProgramParameters4dvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLdouble); OPENGL_CALL
  glProgramParameters4fvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLfloat); OPENGL_CALL
  glRequestResidentProgramsNV: procedure (n: TGLSizei; programs: PGLuint); OPENGL_CALL
  glTrackMatrixNV: procedure (target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); OPENGL_CALL
  glVertexAttribPointerNV: procedure (index: TGLuint; fsize: TGLint; vertextype: TGLenum; stride: TGLSizei; pointer: Pointer); OPENGL_CALL
  glVertexAttrib1dNV: procedure (index: TGLuint; x: TGLdouble); OPENGL_CALL
  glVertexAttrib1dvNV: procedure (index: TGLuint; v: PGLdouble); OPENGL_CALL
  glVertexAttrib1fNV: procedure (index: TGLuint; x: TGLfloat); OPENGL_CALL
  glVertexAttrib1fvNV: procedure (index: TGLuint; v: PGLfloat); OPENGL_CALL
  glVertexAttrib1sNV: procedure (index: TGLuint; x: TGLshort); OPENGL_CALL
  glVertexAttrib1svNV: procedure (index: TGLuint; v: PGLshort); OPENGL_CALL
  glVertexAttrib2dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble); OPENGL_CALL
  glVertexAttrib2dvNV: procedure (index: TGLuint; v: PGLdouble); OPENGL_CALL
  glVertexAttrib2fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat); OPENGL_CALL
  glVertexAttrib2fvNV: procedure (index: TGLuint; v: PGLfloat); OPENGL_CALL
  glVertexAttrib2sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort); OPENGL_CALL
  glVertexAttrib2svNV: procedure (index: TGLuint; v: PGLshort); OPENGL_CALL
  glVertexAttrib3dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); OPENGL_CALL
  glVertexAttrib3dvNV: procedure (index: TGLuint; v: PGLdouble); OPENGL_CALL
  glVertexAttrib3fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); OPENGL_CALL
  glVertexAttrib3fvNV: procedure (index: TGLuint; v: PGLfloat); OPENGL_CALL
  glVertexAttrib3sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); OPENGL_CALL
  glVertexAttrib3svNV: procedure (index: TGLuint; v: PGLshort); OPENGL_CALL
  glVertexAttrib4dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); OPENGL_CALL
  glVertexAttrib4dvNV: procedure (index: TGLuint; v: PGLdouble); OPENGL_CALL
  glVertexAttrib4fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); OPENGL_CALL
  glVertexAttrib4fvNV: procedure(index: TGLuint; v: PGLfloat); OPENGL_CALL
  glVertexAttrib4sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); OPENGL_CALL
  glVertexAttrib4svNV: procedure (index: TGLuint; v: PGLshort); OPENGL_CALL
  glVertexAttrib4ubvNV: procedure (index: TGLuint; v: PGLubyte); OPENGL_CALL
  glVertexAttribs1dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); OPENGL_CALL
  glVertexAttribs1fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); OPENGL_CALL
  glVertexAttribs1svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); OPENGL_CALL
  glVertexAttribs2dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); OPENGL_CALL
  glVertexAttribs2fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); OPENGL_CALL
  glVertexAttribs2svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); OPENGL_CALL
  glVertexAttribs3dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); OPENGL_CALL
  glVertexAttribs3fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); OPENGL_CALL
  glVertexAttribs3svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); OPENGL_CALL
  glVertexAttribs4dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); OPENGL_CALL
  glVertexAttribs4fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); OPENGL_CALL
  glVertexAttribs4svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); OPENGL_CALL
  glVertexAttribs4ubvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLubyte); OPENGL_CALL

{$ifdef UNIX}
type
  GLXContext     = Pointer;
  GLXPixmap      = TXID;
  GLXDrawable    = TXID;

  { GLX 1.3 and later }
  GLXFBConfig    = Pointer;
  GLXFBConfigID  = TXID;
  GLXContextID   = TXID;
  GLXWindow      = TXID;
  GLXPbuffer     = TXID;

{ by Kambi : zamienione wszedzie TGLBoolean na XBool,
  poniewaz procedury glX uzywaja typu XBool (czyli typu Bool z Xlib, 4 bajty)
  a nie typu GLBoolean z opengl'a (1 bajtowego). }
var
  glXChooseVisual: function(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: XBool): GLXContext; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  {$define DYNAMIC_GLX_MAKECURRENT}
  {$ifdef DYNAMIC_GLX_MAKECURRENT}
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): XBool; cdecl;
  {$else}
  function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): XBool; cdecl; external OpenGLDLL;
  {$endif}
  { po co ta zabawa z glXMakeCurrent ? patrz old/OpenGLh_glxMakeCurrent.txt }

var
  glXCopyContext: procedure(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: TPixmap): GLXPixmap; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXQueryExtension: function(dpy: PDisplay; errorb: PGLInt; event: PGLInt): XBool; cdecl;
  glXQueryVersion: function(dpy: PDisplay; maj: PGLInt; min: PGLINT): XBool; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): XBool; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  glXWaitGL: procedure; cdecl;
  glXWaitX: procedure; cdecl;
  glXUseXFont: procedure(font: TFont; first: TGLInt; count: TGLInt; list: TGLint); cdecl;

  { GLX 1.1 and later }
  glXQueryExtensionsString: function(dpy: PDisplay; screen: TGLInt): PChar; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen: TGLInt; name: TGLInt): PChar; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: TGLInt): PChar; cdecl;

  { GLX 1.2 and later }
  glXGetCurrentDisplay: function: PDisplay; cdecl;

  { GLX 1.3 and later }
  glXChooseFBConfig: function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfig; cdecl;
  glXGetFBConfigAttrib: function(dpy: PDisplay; config: GLXFBConfig; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  glXGetFBConfigs: function(dpy: PDisplay; screen: TGLInt; nelements: PGLInt): GLXFBConfig; cdecl;
  glXGetVisualFromFBConfig: function(dpy: PDisplay; config: GLXFBConfig): PXVisualInfo; cdecl;
  glXCreateWindow: function(dpy: PDisplay; config: GLXFBConfig; win: TWindow; const attribList: PGLInt): GLXWindow; cdecl;
  glXDestroyWindow: procedure(dpy: PDisplay; window: GLXWindow); cdecl;
  glXCreatePixmap: function(dpy: PDisplay; config: GLXFBConfig; pixmap: TPixmap; attribList: PGLInt): GLXPixmap; cdecl;
  glXDestroyPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXCreatePbuffer: function(dpy: PDisplay; config: GLXFBConfig; attribList: PGLInt): GLXPBuffer; cdecl;
  glXDestroyPbuffer: procedure(dpy: PDisplay; pbuf: GLXPBuffer); cdecl;
  glXQueryDrawable: procedure(dpy: PDisplay; draw: GLXDrawable; attribute: TGLInt; value: PGLuint); cdecl;
  glXCreateNewContext: function(dpy: PDisplay; config: GLXFBConfig; renderType: TGLInt; shareList: GLXContext; direct: XBool): GLXContext; cdecl;
  glXMakeContextCurrent: function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): XBool; cdecl;
  glXGetCurrentReadDrawable: function: GLXDrawable; cdecl;
  glXQueryContext: function(dpy: PDisplay; ctx: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  glXSelectEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
  glXGetSelectedEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;

  { glX extensions }
  glXGetVideoSyncSGI: function(count: PGLuint): TGLInt; cdecl;
  glXWaitVideoSyncSGI: function(divisor: TGLInt; remainder: TGLInt; count: PGLuint): TGLInt; cdecl;
  glXFreeContextEXT: procedure(dpy: PDisplay; context: GLXContext); cdecl;
  glXGetContextIDEXT: function(const context: GLXContext): GLXContextID; cdecl;
  glXGetCurrentDisplayEXT: function: PDisplay; cdecl;
  glXImportContextEXT: function(dpy: PDisplay; contextID: GLXContextID): GLXContext; cdecl;
  glXQueryContextInfoEXT: function(dpy: PDisplay; context: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawable: GLXDrawable; x: TGLInt; y: TGLInt; width: TGLInt; height: TGLInt); cdecl;
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: TPixmap; cmap: TColormap): GLXPixmap; cdecl;
  glXReleaseBuffersMESA: function(dpy: PDisplay; d: GLXDrawable): XBool; cdecl;
  glXSet3DfxModeMESA: function(mode: TGLint): XBool; cdecl;
{$endif}

{---------------------------------------------------------------------------------------------------------------------- }

{ Note to users of GLWindow unit: you usually don't need to care about
  calling these functions, since @link(TGLWindow.Init) always calls
  ReadImplementationProperties and LoadProcExtensions. }

{ Requires rendering context. Inits boolean variables GL_VERSION_*, GLU_VERSION_*
  and all extensions booleans. }
procedure ReadImplementationProperties;

{ set to nil all extension functions or load all extension functions.
  Loading requires rendering context under Win32 (see also my note at
  the beginning of this module - various RCs may have different extensions
  at different addresses !). }
procedure ClearProcExtensions;
procedure LoadProcExtensions;

{---------------------------------------------------------------------------------------------------------------------- }

{$undef read_interface}

implementation

uses
  SysUtils, Classes;

{$define read_implementation}
{$I opengltypes.inc}

var
  GLLibrary: TDynLib;
  GLULibrary: TDynLib;

const
  SDefaultGLLibName = OpenglDLL;
  SDefaultGLULibName = GluDLL;

procedure CloseOpenGL; forward;
procedure InitOpenGL; forward; overload;
procedure InitOpenGL(const GLName, GLUName: String); forward; overload;
function IsOpenGLInitialized: Boolean; forward;

{
  ustaw na nil adresy wszystkich funkcji  (gl, glu, wgl, glX, ale nie rozszerzen):
    procedure ClearProcAddresses;
  zaladuj adresy tych funkcji z GLHandle i GLUHandle
    procedure LoadProcAddresses;
}

procedure ClearProcAddresses;
begin
 glAccum := nil;
 glAlphaFunc := nil;
 glAreTexturesResident := nil;
 glArrayElement := nil;
 glBegin := nil;
 glBindTexture := nil;
 glBitmap := nil;
 glBlendFunc := nil;
 glCallList := nil;
 glCallLists := nil;
 glClear := nil;
 glClearAccum := nil;
 glClearColor := nil;
 glClearDepth := nil;
 glClearIndex := nil;
 glClearStencil := nil;
 glClipPlane := nil;
 glColor3b := nil;
 glColor3bv := nil;
 glColor3d := nil;
 glColor3dv := nil;
 glColor3f := nil;
 glColor3fv := nil;
 glColor3i := nil;
 glColor3iv := nil;
 glColor3s := nil;
 glColor3sv := nil;
 glColor3ub := nil;
 glColor3ubv := nil;
 glColor3ui := nil;
 glColor3uiv := nil;
 glColor3us := nil;
 glColor3usv := nil;
 glColor4b := nil;
 glColor4bv := nil;
 glColor4d := nil;
 glColor4dv := nil;
 glColor4f := nil;
 glColor4fv := nil;
 glColor4i := nil;
 glColor4iv := nil;
 glColor4s := nil;
 glColor4sv := nil;
 glColor4ub := nil;
 glColor4ubv := nil;
 glColor4ui := nil;
 glColor4uiv := nil;
 glColor4us := nil;
 glColor4usv := nil;
 glColorMask := nil;
 glColorMaterial := nil;
 glColorPointer := nil;
 glCopyPixels := nil;
 glCopyTexImage1D := nil;
 glCopyTexImage2D := nil;
 glCopyTexSubImage1D := nil;
 glCopyTexSubImage2D := nil;
 glCullFace := nil;
 glDeleteLists := nil;
 glDeleteTextures := nil;
 glDepthFunc := nil;
 glDepthMask := nil;
 glDepthRange := nil;
 glDisable := nil;
 glDisableClientState := nil;
 glDrawArrays := nil;
 glDrawBuffer := nil;
 glDrawElements := nil;
 glDrawPixels := nil;
 glEdgeFlag := nil;
 glEdgeFlagPointer := nil;
 glEdgeFlagv := nil;
 glEnable := nil;
 glEnableClientState := nil;
 glEnd := nil;
 glEndList := nil;
 glEvalCoord1d := nil;
 glEvalCoord1dv := nil;
 glEvalCoord1f := nil;
 glEvalCoord1fv := nil;
 glEvalCoord2d := nil;
 glEvalCoord2dv := nil;
 glEvalCoord2f := nil;
 glEvalCoord2fv := nil;
 glEvalMesh1 := nil;
 glEvalMesh2 := nil;
 glEvalPoint1 := nil;
 glEvalPoint2 := nil;
 glFeedbackBuffer := nil;
 glFinish := nil;
 glFlush := nil;
 glFogf := nil;
 glFogfv := nil;
 glFogi := nil;
 glFogiv := nil;
 glFrontFace := nil;
 glFrustum := nil;
 glGenLists := nil;
 glGenTextures := nil;
 glGetBooleanv := nil;
 glGetClipPlane := nil;
 glGetDoublev := nil;
 glGetError := nil;
 glGetFloatv := nil;
 glGetIntegerv := nil;
 glGetLightfv := nil;
 glGetLightiv := nil;
 glGetMapdv := nil;
 glGetMapfv := nil;
 glGetMapiv := nil;
 glGetMaterialfv := nil;
 glGetMaterialiv := nil;
 glGetPixelMapfv := nil;
 glGetPixelMapuiv := nil;
 glGetPixelMapusv := nil;
 glGetPointerv := nil;
 glGetPolygonStipple := nil;
 glGetString := nil;
 glGetTexEnvfv := nil;
 glGetTexEnviv := nil;
 glGetTexGendv := nil;
 glGetTexGenfv := nil;
 glGetTexGeniv := nil;
 glGetTexImage := nil;
 glGetTexLevelParameterfv := nil;
 glGetTexLevelParameteriv := nil;
 glGetTexParameterfv := nil;
 glGetTexParameteriv := nil;
 glHint := nil;
 glIndexMask := nil;
 glIndexPointer := nil;
 glIndexd := nil;
 glIndexdv := nil;
 glIndexf := nil;
 glIndexfv := nil;
 glIndexi := nil;
 glIndexiv := nil;
 glIndexs := nil;
 glIndexsv := nil;
 glIndexub := nil;
 glIndexubv := nil;
 glInitNames := nil;
 glInterleavedArrays := nil;
 glIsEnabled := nil;
 glIsList := nil;
 glIsTexture := nil;
 glLightModelf := nil;
 glLightModelfv := nil;
 glLightModeli := nil;
 glLightModeliv := nil;
 glLightf := nil;
 glLightfv := nil;
 glLighti := nil;
 glLightiv := nil;
 glLineStipple := nil;
 glLineWidth := nil;
 glListBase := nil;
 glLoadIdentity := nil;
 glLoadMatrixd := nil;
 glLoadMatrixf := nil;
 glLoadName := nil;
 glLogicOp := nil;
 glMap1d := nil;
 glMap1f := nil;
 glMap2d := nil;
 glMap2f := nil;
 glMapGrid1d := nil;
 glMapGrid1f := nil;
 glMapGrid2d := nil;
 glMapGrid2f := nil;
 glMaterialf := nil;
 glMaterialfv := nil;
 glMateriali := nil;
 glMaterialiv := nil;
 glMatrixMode := nil;
 glMultMatrixd := nil;
 glMultMatrixf := nil;
 glNewList := nil;
 glNormal3b := nil;
 glNormal3bv := nil;
 glNormal3d := nil;
 glNormal3dv := nil;
 glNormal3f := nil;
 glNormal3fv := nil;
 glNormal3i := nil;
 glNormal3iv := nil;
 glNormal3s := nil;
 glNormal3sv := nil;
 glNormalPointer := nil;
 glOrtho := nil;
 glPassThrough := nil;
 glPixelMapfv := nil;
 glPixelMapuiv := nil;
 glPixelMapusv := nil;
 glPixelStoref := nil;
 glPixelStorei := nil;
 glPixelTransferf := nil;
 glPixelTransferi := nil;
 glPixelZoom := nil;
 glPointSize := nil;
 glPolygonMode := nil;
 glPolygonOffset := nil;
 glPolygonStipple := nil;
 glPopAttrib := nil;
 glPopClientAttrib := nil;
 glPopMatrix := nil;
 glPopName := nil;
 glPrioritizeTextures := nil;
 glPushAttrib := nil;
 glPushClientAttrib := nil;
 glPushMatrix := nil;
 glPushName := nil;
 glRasterPos2d := nil;
 glRasterPos2dv := nil;
 glRasterPos2f := nil;
 glRasterPos2fv := nil;
 glRasterPos2i := nil;
 glRasterPos2iv := nil;
 glRasterPos2s := nil;
 glRasterPos2sv := nil;
 glRasterPos3d := nil;
 glRasterPos3dv := nil;
 glRasterPos3f := nil;
 glRasterPos3fv := nil;
 glRasterPos3i := nil;
 glRasterPos3iv := nil;
 glRasterPos3s := nil;
 glRasterPos3sv := nil;
 glRasterPos4d := nil;
 glRasterPos4dv := nil;
 glRasterPos4f := nil;
 glRasterPos4fv := nil;
 glRasterPos4i := nil;
 glRasterPos4iv := nil;
 glRasterPos4s := nil;
 glRasterPos4sv := nil;
 glReadBuffer := nil;
 glReadPixels := nil;
 glRectd := nil;
 glRectdv := nil;
 glRectf := nil;
 glRectfv := nil;
 glRecti := nil;
 glRectiv := nil;
 glRects := nil;
 glRectsv := nil;
 glRenderMode := nil;
 glRotated := nil;
 glRotatef := nil;
 glScaled := nil;
 glScalef := nil;
 glScissor := nil;
 glSelectBuffer := nil;
 glShadeModel := nil;
 glStencilFunc := nil;
 glStencilMask := nil;
 glStencilOp := nil;
 glTexCoord1d := nil;
 glTexCoord1dv := nil;
 glTexCoord1f := nil;
 glTexCoord1fv := nil;
 glTexCoord1i := nil;
 glTexCoord1iv := nil;
 glTexCoord1s := nil;
 glTexCoord1sv := nil;
 glTexCoord2d := nil;
 glTexCoord2dv := nil;
 glTexCoord2f := nil;
 glTexCoord2fv := nil;
 glTexCoord2i := nil;
 glTexCoord2iv := nil;
 glTexCoord2s := nil;
 glTexCoord2sv := nil;
 glTexCoord3d := nil;
 glTexCoord3dv := nil;
 glTexCoord3f := nil;
 glTexCoord3fv := nil;
 glTexCoord3i := nil;
 glTexCoord3iv := nil;
 glTexCoord3s := nil;
 glTexCoord3sv := nil;
 glTexCoord4d := nil;
 glTexCoord4dv := nil;
 glTexCoord4f := nil;
 glTexCoord4fv := nil;
 glTexCoord4i := nil;
 glTexCoord4iv := nil;
 glTexCoord4s := nil;
 glTexCoord4sv := nil;
 glTexCoordPointer := nil;
 glTexEnvf := nil;
 glTexEnvfv := nil;
 glTexEnvi := nil;
 glTexEnviv := nil;
 glTexGend := nil;
 glTexGendv := nil;
 glTexGenf := nil;
 glTexGenfv := nil;
 glTexGeni := nil;
 glTexGeniv := nil;
 glTexImage1D := nil;
 glTexImage2D := nil;
 glTexParameterf := nil;
 glTexParameterfv := nil;
 glTexParameteri := nil;
 glTexParameteriv := nil;
 glTexSubImage1D := nil;
 glTexSubImage2D := nil;
 glTranslated := nil;
 glTranslatef := nil;
 glVertex2d := nil;
 glVertex2dv := nil;
 glVertex2f := nil;
 glVertex2fv := nil;
 glVertex2i := nil;
 glVertex2iv := nil;
 glVertex2s := nil;
 glVertex2sv := nil;
 glVertex3d := nil;
 glVertex3dv := nil;
 glVertex3f := nil;
 glVertex3fv := nil;
 glVertex3i := nil;
 glVertex3iv := nil;
 glVertex3s := nil;
 glVertex3sv := nil;
 glVertex4d := nil;
 glVertex4dv := nil;
 glVertex4f := nil;
 glVertex4fv := nil;
 glVertex4i := nil;
 glVertex4iv := nil;
 glVertex4s := nil;
 glVertex4sv := nil;
 glVertexPointer := nil;
 glViewport := nil;

 {$IFDEF Win32}
 wglGetProcAddress := nil;
 wglCopyContext := nil;
 wglCreateContext := nil;
 wglCreateLayerContext := nil;
 wglDeleteContext := nil;
 wglDescribeLayerPlane := nil;
 wglGetCurrentContext := nil;
 wglGetCurrentDC := nil;
 wglGetLayerPaletteEntries := nil;
 wglMakeCurrent := nil;
 wglRealizeLayerPalette := nil;
 wglSetLayerPaletteEntries := nil;
 wglShareLists := nil;
 wglSwapLayerBuffers := nil;
 {$ifndef OLDER_WGL} wglSwapMultipleBuffers := nil; {$endif}
 wglUseFontBitmapsA := nil;
 wglUseFontOutlinesA := nil;
 wglUseFontBitmapsW := nil;
 wglUseFontOutlinesW := nil;
 wglUseFontBitmaps := nil;
 wglUseFontOutlines := nil;
 {$ENDIF}

 {$ifndef NO_GL_12}
 { GL 1.2 }
 glDrawRangeElements := nil;
 glTexImage3D := nil;

 { GL 1.2 ARB imaging }
 glBlendColor := nil;
 glBlendEquation := nil;
 glColorSubTable := nil;
 glCopyColorSubTable := nil;
 glColorTable := nil;
 glCopyColorTable := nil;
 glColorTableParameteriv := nil;
 glColorTableParameterfv := nil;
 glGetColorTable := nil;
 glGetColorTableParameteriv := nil;
 glGetColorTableParameterfv := nil;
 glConvolutionFilter1D := nil;
 glConvolutionFilter2D := nil;
 glCopyConvolutionFilter1D := nil;
 glCopyConvolutionFilter2D := nil;
 glGetConvolutionFilter := nil;
 glSeparableFilter2D := nil;
 glGetSeparableFilter := nil;
 glConvolutionParameteri := nil;
 glConvolutionParameteriv := nil;
 glConvolutionParameterf := nil;
 glConvolutionParameterfv := nil;
 glGetConvolutionParameteriv := nil;
 glGetConvolutionParameterfv := nil;
 glHistogram := nil;
 glResetHistogram := nil;
 glGetHistogram := nil;
 glGetHistogramParameteriv := nil;
 glGetHistogramParameterfv := nil;
 glMinmax := nil;
 glResetMinmax := nil;
 glGetMinmax := nil;
 glGetMinmaxParameteriv := nil;
 glGetMinmaxParameterfv := nil;
 {$endif}

 { GLX }
 {$IFDEF UNIX}
 glXChooseVisual := nil;
 glXCreateContext := nil;
 glXDestroyContext := nil;
 {$ifdef DYNAMIC_GLX_MAKECURRENT}
 glXMakeCurrent := nil;
 {$endif}
 glXCopyContext := nil;
 glXSwapBuffers := nil;
 glXCreateGLXPixmap := nil;
 glXDestroyGLXPixmap := nil;
 glXQueryExtension := nil;
 glXQueryVersion := nil;
 glXIsDirect := nil;
 glXGetConfig := nil;
 glXGetCurrentContext := nil;
 glXGetCurrentDrawable := nil;
 glXWaitGL := nil;
 glXWaitX := nil;
 glXUseXFont := nil;

 { GLX 1.1 and later }
 glXQueryExtensionsString := nil;
 glXQueryServerString := nil;
 glXGetClientString := nil;

 { GLX 1.2 and later }
 glXGetCurrentDisplay := nil;

 { GLX 1.3 and later }
 glXChooseFBConfig := nil;
 glXGetFBConfigAttrib := nil;
 glXGetFBConfigs := nil;
 glXGetVisualFromFBConfig := nil;
 glXCreateWindow := nil;
 glXDestroyWindow := nil;
 glXCreatePixmap := nil;
 glXDestroyPixmap := nil;
 glXCreatePbuffer := nil;
 glXDestroyPbuffer := nil;
 glXQueryDrawable := nil;
 glXCreateNewContext := nil;
 glXMakeContextCurrent := nil;
 glXGetCurrentReadDrawable := nil;
 glXQueryContext := nil;
 glXSelectEvent := nil;
 glXGetSelectedEvent := nil;
 {$ENDIF}
end;

procedure LoadProcAddresses;
begin
 if GLLibrary <> nil then
 begin
  @glAccum := GLLibrary.Symbol('glAccum');
  @glAlphaFunc := GLLibrary.Symbol('glAlphaFunc');
  @glAreTexturesResident := GLLibrary.Symbol('glAreTexturesResident');
  @glArrayElement := GLLibrary.Symbol('glArrayElement');
  @glBegin := GLLibrary.Symbol('glBegin');
  @glBindTexture := GLLibrary.Symbol('glBindTexture');
  @glBitmap := GLLibrary.Symbol('glBitmap');
  @glBlendFunc := GLLibrary.Symbol('glBlendFunc');
  @glCallList := GLLibrary.Symbol('glCallList');
  @glCallLists := GLLibrary.Symbol('glCallLists');
  @glClear := GLLibrary.Symbol('glClear');
  @glClearAccum := GLLibrary.Symbol('glClearAccum');
  @glClearColor := GLLibrary.Symbol('glClearColor');
  @glClearDepth := GLLibrary.Symbol('glClearDepth');
  @glClearIndex := GLLibrary.Symbol('glClearIndex');
  @glClearStencil := GLLibrary.Symbol('glClearStencil');
  @glClipPlane := GLLibrary.Symbol('glClipPlane');
  @glColor3b := GLLibrary.Symbol('glColor3b');
  @glColor3bv := GLLibrary.Symbol('glColor3bv');
  @glColor3d := GLLibrary.Symbol('glColor3d');
  @glColor3dv := GLLibrary.Symbol('glColor3dv');
  @glColor3f := GLLibrary.Symbol('glColor3f');
  @glColor3fv := GLLibrary.Symbol('glColor3fv');
  @glColor3i := GLLibrary.Symbol('glColor3i');
  @glColor3iv := GLLibrary.Symbol('glColor3iv');
  @glColor3s := GLLibrary.Symbol('glColor3s');
  @glColor3sv := GLLibrary.Symbol('glColor3sv');
  @glColor3ub := GLLibrary.Symbol('glColor3ub');
  @glColor3ubv := GLLibrary.Symbol('glColor3ubv');
  @glColor3ui := GLLibrary.Symbol('glColor3ui');
  @glColor3uiv := GLLibrary.Symbol('glColor3uiv');
  @glColor3us := GLLibrary.Symbol('glColor3us');
  @glColor3usv := GLLibrary.Symbol('glColor3usv');
  @glColor4b := GLLibrary.Symbol('glColor4b');
  @glColor4bv := GLLibrary.Symbol('glColor4bv');
  @glColor4d := GLLibrary.Symbol('glColor4d');
  @glColor4dv := GLLibrary.Symbol('glColor4dv');
  @glColor4f := GLLibrary.Symbol('glColor4f');
  @glColor4fv := GLLibrary.Symbol('glColor4fv');
  @glColor4i := GLLibrary.Symbol('glColor4i');
  @glColor4iv := GLLibrary.Symbol('glColor4iv');
  @glColor4s := GLLibrary.Symbol('glColor4s');
  @glColor4sv := GLLibrary.Symbol('glColor4sv');
  @glColor4ub := GLLibrary.Symbol('glColor4ub');
  @glColor4ubv := GLLibrary.Symbol('glColor4ubv');
  @glColor4ui := GLLibrary.Symbol('glColor4ui');
  @glColor4uiv := GLLibrary.Symbol('glColor4uiv');
  @glColor4us := GLLibrary.Symbol('glColor4us');
  @glColor4usv := GLLibrary.Symbol('glColor4usv');
  @glColorMask := GLLibrary.Symbol('glColorMask');
  @glColorMaterial := GLLibrary.Symbol('glColorMaterial');
  @glColorPointer := GLLibrary.Symbol('glColorPointer');
  @glCopyPixels := GLLibrary.Symbol('glCopyPixels');
  @glCopyTexImage1D := GLLibrary.Symbol('glCopyTexImage1D');
  @glCopyTexImage2D := GLLibrary.Symbol('glCopyTexImage2D');
  @glCopyTexSubImage1D := GLLibrary.Symbol('glCopyTexSubImage1D');
  @glCopyTexSubImage2D := GLLibrary.Symbol('glCopyTexSubImage2D');
  @glCullFace := GLLibrary.Symbol('glCullFace');
  @glDeleteLists := GLLibrary.Symbol('glDeleteLists');
  @glDeleteTextures := GLLibrary.Symbol('glDeleteTextures');
  @glDepthFunc := GLLibrary.Symbol('glDepthFunc');
  @glDepthMask := GLLibrary.Symbol('glDepthMask');
  @glDepthRange := GLLibrary.Symbol('glDepthRange');
  @glDisable := GLLibrary.Symbol('glDisable');
  @glDisableClientState := GLLibrary.Symbol('glDisableClientState');
  @glDrawArrays := GLLibrary.Symbol('glDrawArrays');
  @glDrawBuffer := GLLibrary.Symbol('glDrawBuffer');
  @glDrawElements := GLLibrary.Symbol('glDrawElements');
  @glDrawPixels := GLLibrary.Symbol('glDrawPixels');
  @glEdgeFlag := GLLibrary.Symbol('glEdgeFlag');
  @glEdgeFlagPointer := GLLibrary.Symbol('glEdgeFlagPointer');
  @glEdgeFlagv := GLLibrary.Symbol('glEdgeFlagv');
  @glEnable := GLLibrary.Symbol('glEnable');
  @glEnableClientState := GLLibrary.Symbol('glEnableClientState');
  @glEnd := GLLibrary.Symbol('glEnd');
  @glEndList := GLLibrary.Symbol('glEndList');
  @glEvalCoord1d := GLLibrary.Symbol('glEvalCoord1d');
  @glEvalCoord1dv := GLLibrary.Symbol('glEvalCoord1dv');
  @glEvalCoord1f := GLLibrary.Symbol('glEvalCoord1f');
  @glEvalCoord1fv := GLLibrary.Symbol('glEvalCoord1fv');
  @glEvalCoord2d := GLLibrary.Symbol('glEvalCoord2d');
  @glEvalCoord2dv := GLLibrary.Symbol('glEvalCoord2dv');
  @glEvalCoord2f := GLLibrary.Symbol('glEvalCoord2f');
  @glEvalCoord2fv := GLLibrary.Symbol('glEvalCoord2fv');
  @glEvalMesh1 := GLLibrary.Symbol('glEvalMesh1');
  @glEvalMesh2 := GLLibrary.Symbol('glEvalMesh2');
  @glEvalPoint1 := GLLibrary.Symbol('glEvalPoint1');
  @glEvalPoint2 := GLLibrary.Symbol('glEvalPoint2');
  @glFeedbackBuffer := GLLibrary.Symbol('glFeedbackBuffer');
  @glFinish := GLLibrary.Symbol('glFinish');
  @glFlush := GLLibrary.Symbol('glFlush');
  @glFogf := GLLibrary.Symbol('glFogf');
  @glFogfv := GLLibrary.Symbol('glFogfv');
  @glFogi := GLLibrary.Symbol('glFogi');
  @glFogiv := GLLibrary.Symbol('glFogiv');
  @glFrontFace := GLLibrary.Symbol('glFrontFace');
  @glFrustum := GLLibrary.Symbol('glFrustum');
  @glGenLists := GLLibrary.Symbol('glGenLists');
  @glGenTextures := GLLibrary.Symbol('glGenTextures');
  @glGetBooleanv := GLLibrary.Symbol('glGetBooleanv');
  @glGetClipPlane := GLLibrary.Symbol('glGetClipPlane');
  @glGetDoublev := GLLibrary.Symbol('glGetDoublev');
  @glGetError := GLLibrary.Symbol('glGetError');
  @glGetFloatv := GLLibrary.Symbol('glGetFloatv');
  @glGetIntegerv := GLLibrary.Symbol('glGetIntegerv');
  @glGetLightfv := GLLibrary.Symbol('glGetLightfv');
  @glGetLightiv := GLLibrary.Symbol('glGetLightiv');
  @glGetMapdv := GLLibrary.Symbol('glGetMapdv');
  @glGetMapfv := GLLibrary.Symbol('glGetMapfv');
  @glGetMapiv := GLLibrary.Symbol('glGetMapiv');
  @glGetMaterialfv := GLLibrary.Symbol('glGetMaterialfv');
  @glGetMaterialiv := GLLibrary.Symbol('glGetMaterialiv');
  @glGetPixelMapfv := GLLibrary.Symbol('glGetPixelMapfv');
  @glGetPixelMapuiv := GLLibrary.Symbol('glGetPixelMapuiv');
  @glGetPixelMapusv := GLLibrary.Symbol('glGetPixelMapusv');
  @glGetPointerv := GLLibrary.Symbol('glGetPointerv');
  @glGetPolygonStipple := GLLibrary.Symbol('glGetPolygonStipple');
  @glGetString := GLLibrary.Symbol('glGetString');
  @glGetTexEnvfv := GLLibrary.Symbol('glGetTexEnvfv');
  @glGetTexEnviv := GLLibrary.Symbol('glGetTexEnviv');
  @glGetTexGendv := GLLibrary.Symbol('glGetTexGendv');
  @glGetTexGenfv := GLLibrary.Symbol('glGetTexGenfv');
  @glGetTexGeniv := GLLibrary.Symbol('glGetTexGeniv');
  @glGetTexImage := GLLibrary.Symbol('glGetTexImage');
  @glGetTexLevelParameterfv := GLLibrary.Symbol('glGetTexLevelParameterfv');
  @glGetTexLevelParameteriv := GLLibrary.Symbol('glGetTexLevelParameteriv');
  @glGetTexParameterfv := GLLibrary.Symbol('glGetTexParameterfv');
  @glGetTexParameteriv := GLLibrary.Symbol('glGetTexParameteriv');
  @glHint := GLLibrary.Symbol('glHint');
  @glIndexMask := GLLibrary.Symbol('glIndexMask');
  @glIndexPointer := GLLibrary.Symbol('glIndexPointer');
  @glIndexd := GLLibrary.Symbol('glIndexd');
  @glIndexdv := GLLibrary.Symbol('glIndexdv');
  @glIndexf := GLLibrary.Symbol('glIndexf');
  @glIndexfv := GLLibrary.Symbol('glIndexfv');
  @glIndexi := GLLibrary.Symbol('glIndexi');
  @glIndexiv := GLLibrary.Symbol('glIndexiv');
  @glIndexs := GLLibrary.Symbol('glIndexs');
  @glIndexsv := GLLibrary.Symbol('glIndexsv');
  @glIndexub := GLLibrary.Symbol('glIndexub');
  @glIndexubv := GLLibrary.Symbol('glIndexubv');
  @glInitNames := GLLibrary.Symbol('glInitNames');
  @glInterleavedArrays := GLLibrary.Symbol('glInterleavedArrays');
  @glIsEnabled := GLLibrary.Symbol('glIsEnabled');
  @glIsList := GLLibrary.Symbol('glIsList');
  @glIsTexture := GLLibrary.Symbol('glIsTexture');
  @glLightModelf := GLLibrary.Symbol('glLightModelf');
  @glLightModelfv := GLLibrary.Symbol('glLightModelfv');
  @glLightModeli := GLLibrary.Symbol('glLightModeli');
  @glLightModeliv := GLLibrary.Symbol('glLightModeliv');
  @glLightf := GLLibrary.Symbol('glLightf');
  @glLightfv := GLLibrary.Symbol('glLightfv');
  @glLighti := GLLibrary.Symbol('glLighti');
  @glLightiv := GLLibrary.Symbol('glLightiv');
  @glLineStipple := GLLibrary.Symbol('glLineStipple');
  @glLineWidth := GLLibrary.Symbol('glLineWidth');
  @glListBase := GLLibrary.Symbol('glListBase');
  @glLoadIdentity := GLLibrary.Symbol('glLoadIdentity');
  @glLoadMatrixd := GLLibrary.Symbol('glLoadMatrixd');
  @glLoadMatrixf := GLLibrary.Symbol('glLoadMatrixf');
  @glLoadName := GLLibrary.Symbol('glLoadName');
  @glLogicOp := GLLibrary.Symbol('glLogicOp');
  @glMap1d := GLLibrary.Symbol('glMap1d');
  @glMap1f := GLLibrary.Symbol('glMap1f');
  @glMap2d := GLLibrary.Symbol('glMap2d');
  @glMap2f := GLLibrary.Symbol('glMap2f');
  @glMapGrid1d := GLLibrary.Symbol('glMapGrid1d');
  @glMapGrid1f := GLLibrary.Symbol('glMapGrid1f');
  @glMapGrid2d := GLLibrary.Symbol('glMapGrid2d');
  @glMapGrid2f := GLLibrary.Symbol('glMapGrid2f');
  @glMaterialf := GLLibrary.Symbol('glMaterialf');
  @glMaterialfv := GLLibrary.Symbol('glMaterialfv');
  @glMateriali := GLLibrary.Symbol('glMateriali');
  @glMaterialiv := GLLibrary.Symbol('glMaterialiv');
  @glMatrixMode := GLLibrary.Symbol('glMatrixMode');
  @glMultMatrixd := GLLibrary.Symbol('glMultMatrixd');
  @glMultMatrixf := GLLibrary.Symbol('glMultMatrixf');
  @glNewList := GLLibrary.Symbol('glNewList');
  @glNormal3b := GLLibrary.Symbol('glNormal3b');
  @glNormal3bv := GLLibrary.Symbol('glNormal3bv');
  @glNormal3d := GLLibrary.Symbol('glNormal3d');
  @glNormal3dv := GLLibrary.Symbol('glNormal3dv');
  @glNormal3f := GLLibrary.Symbol('glNormal3f');
  @glNormal3fv := GLLibrary.Symbol('glNormal3fv');
  @glNormal3i := GLLibrary.Symbol('glNormal3i');
  @glNormal3iv := GLLibrary.Symbol('glNormal3iv');
  @glNormal3s := GLLibrary.Symbol('glNormal3s');
  @glNormal3sv := GLLibrary.Symbol('glNormal3sv');
  @glNormalPointer := GLLibrary.Symbol('glNormalPointer');
  @glOrtho := GLLibrary.Symbol('glOrtho');
  @glPassThrough := GLLibrary.Symbol('glPassThrough');
  @glPixelMapfv := GLLibrary.Symbol('glPixelMapfv');
  @glPixelMapuiv := GLLibrary.Symbol('glPixelMapuiv');
  @glPixelMapusv := GLLibrary.Symbol('glPixelMapusv');
  @glPixelStoref := GLLibrary.Symbol('glPixelStoref');
  @glPixelStorei := GLLibrary.Symbol('glPixelStorei');
  @glPixelTransferf := GLLibrary.Symbol('glPixelTransferf');
  @glPixelTransferi := GLLibrary.Symbol('glPixelTransferi');
  @glPixelZoom := GLLibrary.Symbol('glPixelZoom');
  @glPointSize := GLLibrary.Symbol('glPointSize');
  @glPolygonMode := GLLibrary.Symbol('glPolygonMode');
  @glPolygonOffset := GLLibrary.Symbol('glPolygonOffset');
  @glPolygonStipple := GLLibrary.Symbol('glPolygonStipple');
  @glPopAttrib := GLLibrary.Symbol('glPopAttrib');
  @glPopClientAttrib := GLLibrary.Symbol('glPopClientAttrib');
  @glPopMatrix := GLLibrary.Symbol('glPopMatrix');
  @glPopName := GLLibrary.Symbol('glPopName');
  @glPrioritizeTextures := GLLibrary.Symbol('glPrioritizeTextures');
  @glPushAttrib := GLLibrary.Symbol('glPushAttrib');
  @glPushClientAttrib := GLLibrary.Symbol('glPushClientAttrib');
  @glPushMatrix := GLLibrary.Symbol('glPushMatrix');
  @glPushName := GLLibrary.Symbol('glPushName');
  @glRasterPos2d := GLLibrary.Symbol('glRasterPos2d');
  @glRasterPos2dv := GLLibrary.Symbol('glRasterPos2dv');
  @glRasterPos2f := GLLibrary.Symbol('glRasterPos2f');
  @glRasterPos2fv := GLLibrary.Symbol('glRasterPos2fv');
  @glRasterPos2i := GLLibrary.Symbol('glRasterPos2i');
  @glRasterPos2iv := GLLibrary.Symbol('glRasterPos2iv');
  @glRasterPos2s := GLLibrary.Symbol('glRasterPos2s');
  @glRasterPos2sv := GLLibrary.Symbol('glRasterPos2sv');
  @glRasterPos3d := GLLibrary.Symbol('glRasterPos3d');
  @glRasterPos3dv := GLLibrary.Symbol('glRasterPos3dv');
  @glRasterPos3f := GLLibrary.Symbol('glRasterPos3f');
  @glRasterPos3fv := GLLibrary.Symbol('glRasterPos3fv');
  @glRasterPos3i := GLLibrary.Symbol('glRasterPos3i');
  @glRasterPos3iv := GLLibrary.Symbol('glRasterPos3iv');
  @glRasterPos3s := GLLibrary.Symbol('glRasterPos3s');
  @glRasterPos3sv := GLLibrary.Symbol('glRasterPos3sv');
  @glRasterPos4d := GLLibrary.Symbol('glRasterPos4d');
  @glRasterPos4dv := GLLibrary.Symbol('glRasterPos4dv');
  @glRasterPos4f := GLLibrary.Symbol('glRasterPos4f');
  @glRasterPos4fv := GLLibrary.Symbol('glRasterPos4fv');
  @glRasterPos4i := GLLibrary.Symbol('glRasterPos4i');
  @glRasterPos4iv := GLLibrary.Symbol('glRasterPos4iv');
  @glRasterPos4s := GLLibrary.Symbol('glRasterPos4s');
  @glRasterPos4sv := GLLibrary.Symbol('glRasterPos4sv');
  @glReadBuffer := GLLibrary.Symbol('glReadBuffer');
  @glReadPixels := GLLibrary.Symbol('glReadPixels');
  @glRectd := GLLibrary.Symbol('glRectd');
  @glRectdv := GLLibrary.Symbol('glRectdv');
  @glRectf := GLLibrary.Symbol('glRectf');
  @glRectfv := GLLibrary.Symbol('glRectfv');
  @glRecti := GLLibrary.Symbol('glRecti');
  @glRectiv := GLLibrary.Symbol('glRectiv');
  @glRects := GLLibrary.Symbol('glRects');
  @glRectsv := GLLibrary.Symbol('glRectsv');
  @glRenderMode := GLLibrary.Symbol('glRenderMode');
  @glRotated := GLLibrary.Symbol('glRotated');
  @glRotatef := GLLibrary.Symbol('glRotatef');
  @glScaled := GLLibrary.Symbol('glScaled');
  @glScalef := GLLibrary.Symbol('glScalef');
  @glScissor := GLLibrary.Symbol('glScissor');
  @glSelectBuffer := GLLibrary.Symbol('glSelectBuffer');
  @glShadeModel := GLLibrary.Symbol('glShadeModel');
  @glStencilFunc := GLLibrary.Symbol('glStencilFunc');
  @glStencilMask := GLLibrary.Symbol('glStencilMask');
  @glStencilOp := GLLibrary.Symbol('glStencilOp');
  @glTexCoord1d := GLLibrary.Symbol('glTexCoord1d');
  @glTexCoord1dv := GLLibrary.Symbol('glTexCoord1dv');
  @glTexCoord1f := GLLibrary.Symbol('glTexCoord1f');
  @glTexCoord1fv := GLLibrary.Symbol('glTexCoord1fv');
  @glTexCoord1i := GLLibrary.Symbol('glTexCoord1i');
  @glTexCoord1iv := GLLibrary.Symbol('glTexCoord1iv');
  @glTexCoord1s := GLLibrary.Symbol('glTexCoord1s');
  @glTexCoord1sv := GLLibrary.Symbol('glTexCoord1sv');
  @glTexCoord2d := GLLibrary.Symbol('glTexCoord2d');
  @glTexCoord2dv := GLLibrary.Symbol('glTexCoord2dv');
  @glTexCoord2f := GLLibrary.Symbol('glTexCoord2f');
  @glTexCoord2fv := GLLibrary.Symbol('glTexCoord2fv');
  @glTexCoord2i := GLLibrary.Symbol('glTexCoord2i');
  @glTexCoord2iv := GLLibrary.Symbol('glTexCoord2iv');
  @glTexCoord2s := GLLibrary.Symbol('glTexCoord2s');
  @glTexCoord2sv := GLLibrary.Symbol('glTexCoord2sv');
  @glTexCoord3d := GLLibrary.Symbol('glTexCoord3d');
  @glTexCoord3dv := GLLibrary.Symbol('glTexCoord3dv');
  @glTexCoord3f := GLLibrary.Symbol('glTexCoord3f');
  @glTexCoord3fv := GLLibrary.Symbol('glTexCoord3fv');
  @glTexCoord3i := GLLibrary.Symbol('glTexCoord3i');
  @glTexCoord3iv := GLLibrary.Symbol('glTexCoord3iv');
  @glTexCoord3s := GLLibrary.Symbol('glTexCoord3s');
  @glTexCoord3sv := GLLibrary.Symbol('glTexCoord3sv');
  @glTexCoord4d := GLLibrary.Symbol('glTexCoord4d');
  @glTexCoord4dv := GLLibrary.Symbol('glTexCoord4dv');
  @glTexCoord4f := GLLibrary.Symbol('glTexCoord4f');
  @glTexCoord4fv := GLLibrary.Symbol('glTexCoord4fv');
  @glTexCoord4i := GLLibrary.Symbol('glTexCoord4i');
  @glTexCoord4iv := GLLibrary.Symbol('glTexCoord4iv');
  @glTexCoord4s := GLLibrary.Symbol('glTexCoord4s');
  @glTexCoord4sv := GLLibrary.Symbol('glTexCoord4sv');
  @glTexCoordPointer := GLLibrary.Symbol('glTexCoordPointer');
  @glTexEnvf := GLLibrary.Symbol('glTexEnvf');
  @glTexEnvfv := GLLibrary.Symbol('glTexEnvfv');
  @glTexEnvi := GLLibrary.Symbol('glTexEnvi');
  @glTexEnviv := GLLibrary.Symbol('glTexEnviv');
  @glTexGend := GLLibrary.Symbol('glTexGend');
  @glTexGendv := GLLibrary.Symbol('glTexGendv');
  @glTexGenf := GLLibrary.Symbol('glTexGenf');
  @glTexGenfv := GLLibrary.Symbol('glTexGenfv');
  @glTexGeni := GLLibrary.Symbol('glTexGeni');
  @glTexGeniv := GLLibrary.Symbol('glTexGeniv');
  @glTexImage1D := GLLibrary.Symbol('glTexImage1D');
  @glTexImage2D := GLLibrary.Symbol('glTexImage2D');
  @glTexParameterf := GLLibrary.Symbol('glTexParameterf');
  @glTexParameterfv := GLLibrary.Symbol('glTexParameterfv');
  @glTexParameteri := GLLibrary.Symbol('glTexParameteri');
  @glTexParameteriv := GLLibrary.Symbol('glTexParameteriv');
  @glTexSubImage1D := GLLibrary.Symbol('glTexSubImage1D');
  @glTexSubImage2D := GLLibrary.Symbol('glTexSubImage2D');
  @glTranslated := GLLibrary.Symbol('glTranslated');
  @glTranslatef := GLLibrary.Symbol('glTranslatef');
  @glVertex2d := GLLibrary.Symbol('glVertex2d');
  @glVertex2dv := GLLibrary.Symbol('glVertex2dv');
  @glVertex2f := GLLibrary.Symbol('glVertex2f');
  @glVertex2fv := GLLibrary.Symbol('glVertex2fv');
  @glVertex2i := GLLibrary.Symbol('glVertex2i');
  @glVertex2iv := GLLibrary.Symbol('glVertex2iv');
  @glVertex2s := GLLibrary.Symbol('glVertex2s');
  @glVertex2sv := GLLibrary.Symbol('glVertex2sv');
  @glVertex3d := GLLibrary.Symbol('glVertex3d');
  @glVertex3dv := GLLibrary.Symbol('glVertex3dv');
  @glVertex3f := GLLibrary.Symbol('glVertex3f');
  @glVertex3fv := GLLibrary.Symbol('glVertex3fv');
  @glVertex3i := GLLibrary.Symbol('glVertex3i');
  @glVertex3iv := GLLibrary.Symbol('glVertex3iv');
  @glVertex3s := GLLibrary.Symbol('glVertex3s');
  @glVertex3sv := GLLibrary.Symbol('glVertex3sv');
  @glVertex4d := GLLibrary.Symbol('glVertex4d');
  @glVertex4dv := GLLibrary.Symbol('glVertex4dv');
  @glVertex4f := GLLibrary.Symbol('glVertex4f');
  @glVertex4fv := GLLibrary.Symbol('glVertex4fv');
  @glVertex4i := GLLibrary.Symbol('glVertex4i');
  @glVertex4iv := GLLibrary.Symbol('glVertex4iv');
  @glVertex4s := GLLibrary.Symbol('glVertex4s');
  @glVertex4sv := GLLibrary.Symbol('glVertex4sv');
  @glVertexPointer := GLLibrary.Symbol('glVertexPointer');
  @glViewport := GLLibrary.Symbol('glViewport');

  { window support routines }
  {$IFDEF Win32}
  @wglGetProcAddress := GLLibrary.Symbol('wglGetProcAddress');
  @wglCopyContext := GLLibrary.Symbol('wglCopyContext');
  @wglCreateContext := GLLibrary.Symbol('wglCreateContext');
  @wglCreateLayerContext := GLLibrary.Symbol('wglCreateLayerContext');
  @wglDeleteContext := GLLibrary.Symbol('wglDeleteContext');
  @wglDescribeLayerPlane := GLLibrary.Symbol('wglDescribeLayerPlane');
  @wglGetCurrentContext := GLLibrary.Symbol('wglGetCurrentContext');
  @wglGetCurrentDC := GLLibrary.Symbol('wglGetCurrentDC');
  @wglGetLayerPaletteEntries := GLLibrary.Symbol('wglGetLayerPaletteEntries');
  @wglMakeCurrent := GLLibrary.Symbol('wglMakeCurrent');
  @wglRealizeLayerPalette := GLLibrary.Symbol('wglRealizeLayerPalette');
  @wglSetLayerPaletteEntries := GLLibrary.Symbol('wglSetLayerPaletteEntries');
  @wglShareLists := GLLibrary.Symbol('wglShareLists');
  @wglSwapLayerBuffers := GLLibrary.Symbol('wglSwapLayerBuffers');
  {$ifndef OLDER_WGL} @wglSwapMultipleBuffers := GLLibrary.Symbol('wglSwapMultipleBuffers'); {$endif}
  @wglUseFontBitmapsA := GLLibrary.Symbol('wglUseFontBitmapsA');
  @wglUseFontOutlinesA := GLLibrary.Symbol('wglUseFontOutlinesA');
  @wglUseFontBitmapsW := GLLibrary.Symbol('wglUseFontBitmapsW');
  @wglUseFontOutlinesW := GLLibrary.Symbol('wglUseFontOutlinesW');
  @wglUseFontBitmaps := GLLibrary.Symbol('wglUseFontBitmapsA');
  @wglUseFontOutlines := GLLibrary.Symbol('wglUseFontOutlinesA');
  {$ENDIF}

  {$ifndef NO_GL_12}
  { GL 1.2 }
  @glDrawRangeElements := GLLibrary.Symbol('glDrawRangeElements');
  @glTexImage3D := GLLibrary.Symbol('glTexImage3D');

  { GL 1.2 ARB imaging }
  @glBlendColor := GLLibrary.Symbol('glBlendColor');
  @glBlendEquation := GLLibrary.Symbol('glBlendEquation');
  @glColorSubTable := GLLibrary.Symbol('glColorSubTable');
  @glCopyColorSubTable := GLLibrary.Symbol('glCopyColorSubTable');
  @glColorTable := GLLibrary.Symbol('glCopyColorSubTable');
  @glCopyColorTable := GLLibrary.Symbol('glCopyColorTable');
  @glColorTableParameteriv := GLLibrary.Symbol('glColorTableParameteriv');
  @glColorTableParameterfv := GLLibrary.Symbol('glColorTableParameterfv');
  @glGetColorTable := GLLibrary.Symbol('glGetColorTable');
  @glGetColorTableParameteriv := GLLibrary.Symbol('glGetColorTableParameteriv');
  @glGetColorTableParameterfv := GLLibrary.Symbol('glGetColorTableParameterfv');
  @glConvolutionFilter1D := GLLibrary.Symbol('glConvolutionFilter1D');
  @glConvolutionFilter2D := GLLibrary.Symbol('glConvolutionFilter2D');
  @glCopyConvolutionFilter1D := GLLibrary.Symbol('glCopyConvolutionFilter1D');
  @glCopyConvolutionFilter2D := GLLibrary.Symbol('glCopyConvolutionFilter2D');
  @glGetConvolutionFilter := GLLibrary.Symbol('glGetConvolutionFilter');
  @glSeparableFilter2D := GLLibrary.Symbol('glSeparableFilter2D');
  @glGetSeparableFilter := GLLibrary.Symbol('glGetSeparableFilter');
  @glConvolutionParameteri := GLLibrary.Symbol('glConvolutionParameteri');
  @glConvolutionParameteriv := GLLibrary.Symbol('glConvolutionParameteriv');
  @glConvolutionParameterf := GLLibrary.Symbol('glConvolutionParameterf');
  @glConvolutionParameterfv := GLLibrary.Symbol('glConvolutionParameterfv');
  @glGetConvolutionParameteriv := GLLibrary.Symbol('glGetConvolutionParameteriv');
  @glGetConvolutionParameterfv := GLLibrary.Symbol('glGetConvolutionParameterfv');
  @glHistogram := GLLibrary.Symbol('glHistogram');
  @glResetHistogram := GLLibrary.Symbol('glResetHistogram');
  @glGetHistogram := GLLibrary.Symbol('glGetHistogram');
  @glGetHistogramParameteriv := GLLibrary.Symbol('glGetHistogramParameteriv');
  @glGetHistogramParameterfv := GLLibrary.Symbol('glGetHistogramParameterfv');
  @glMinmax := GLLibrary.Symbol('glMinmax');
  @glResetMinmax := GLLibrary.Symbol('glResetMinmax');
  @glGetMinmax := GLLibrary.Symbol('glGetMinmax');
  @glGetMinmaxParameteriv := GLLibrary.Symbol('glGetMinmaxParameteriv');
  @glGetMinmaxParameterfv := GLLibrary.Symbol('glGetMinmaxParameterfv');
  {$endif}

  {$IFDEF UNIX}
  @glXChooseVisual := GLLibrary.Symbol('glXChooseVisual');
  @glXCreateContext := GLLibrary.Symbol('glXCreateContext');
  @glXDestroyContext := GLLibrary.Symbol('glXDestroyContext');
  {$ifdef DYNAMIC_GLX_MAKECURRENT}
  @glXMakeCurrent := GLLibrary.Symbol('glXMakeCurrent');
  {$endif}
  @glXCopyContext := GLLibrary.Symbol('glXCopyContext');
  @glXSwapBuffers := GLLibrary.Symbol('glXSwapBuffers');
  @glXCreateGLXPixmap := GLLibrary.Symbol('glXCreateGLXPixmap');
  @glXDestroyGLXPixmap := GLLibrary.Symbol('glXDestroyGLXPixmap');
  @glXQueryExtension := GLLibrary.Symbol('glXQueryExtension');
  @glXQueryVersion := GLLibrary.Symbol('glXQueryVersion');
  @glXIsDirect := GLLibrary.Symbol('glXIsDirect');
  @glXGetConfig := GLLibrary.Symbol('glXGetConfig');
  @glXGetCurrentContext := GLLibrary.Symbol('glXGetCurrentContext');
  @glXGetCurrentDrawable := GLLibrary.Symbol('glXGetCurrentDrawable');
  @glXWaitGL := GLLibrary.Symbol('glXWaitGL');
  @glXWaitX := GLLibrary.Symbol('glXWaitX');
  @glXUseXFont := GLLibrary.Symbol('glXUseXFont');
  @glXQueryExtensionsString := GLLibrary.Symbol('glXQueryExtensionsString');
  @glXQueryServerString := GLLibrary.Symbol('glXQueryServerString');
  @glXGetClientString := GLLibrary.Symbol('glXGetClientString');
  @glXGetCurrentDisplay := GLLibrary.Symbol('glXGetCurrentDisplay');
  @glXChooseFBConfig := GLLibrary.Symbol('glXChooseFBConfig');
  @glXGetFBConfigAttrib := GLLibrary.Symbol('glXGetFBConfigAttrib');
  @glXGetFBConfigs := GLLibrary.Symbol('glXGetFBConfigs');
  @glXGetVisualFromFBConfig := GLLibrary.Symbol('glXGetVisualFromFBConfig');
  @glXCreateWindow := GLLibrary.Symbol('glXCreateWindow');
  @glXDestroyWindow := GLLibrary.Symbol('glXDestroyWindow');
  @glXCreatePixmap := GLLibrary.Symbol('glXCreatePixmap');
  @glXDestroyPixmap := GLLibrary.Symbol('glXDestroyPixmap');
  @glXCreatePbuffer := GLLibrary.Symbol('glXCreatePbuffer');
  @glXDestroyPbuffer := GLLibrary.Symbol('glXDestroyPbuffer');
  @glXQueryDrawable := GLLibrary.Symbol('glXQueryDrawable');
  @glXCreateNewContext := GLLibrary.Symbol('glXCreateNewContext');
  @glXMakeContextCurrent := GLLibrary.Symbol('glXMakeContextCurrent');
  @glXGetCurrentReadDrawable := GLLibrary.Symbol('glXGetCurrentReadDrawable');
  @glXQueryContext := GLLibrary.Symbol('glXQueryContext');
  @glXSelectEvent := GLLibrary.Symbol('glXSelectEvent');
  @glXGetSelectedEvent := GLLibrary.Symbol('glXGetSelectedEvent');
  {$ENDIF}
 end;

 if GLULibrary <> nil then
 begin
  @gluBeginCurve := GLULibrary.Symbol('gluBeginCurve');
  @gluBeginPolygon := GLULibrary.Symbol('gluBeginPolygon');
  @gluBeginSurface := GLULibrary.Symbol('gluBeginSurface');
  @gluBeginTrim := GLULibrary.Symbol('gluBeginTrim');
  @gluBuild1DMipmaps := GLULibrary.Symbol('gluBuild1DMipmaps');
  @gluBuild2DMipmaps := GLULibrary.Symbol('gluBuild2DMipmaps');
  @gluCylinder := GLULibrary.Symbol('gluCylinder');
  @gluDeleteNurbsRenderer := GLULibrary.Symbol('gluDeleteNurbsRenderer');
  @gluDeleteQuadric := GLULibrary.Symbol('gluDeleteQuadric');
  @gluDeleteTess := GLULibrary.Symbol('gluDeleteTess');
  @gluDisk := GLULibrary.Symbol('gluDisk');
  @gluEndCurve := GLULibrary.Symbol('gluEndCurve');
  @gluEndPolygon := GLULibrary.Symbol('gluEndPolygon');
  @gluEndSurface := GLULibrary.Symbol('gluEndSurface');
  @gluEndTrim := GLULibrary.Symbol('gluEndTrim');
  @gluErrorString := GLULibrary.Symbol('gluErrorString');
  @gluGetNurbsProperty := GLULibrary.Symbol('gluGetNurbsProperty');
  @gluGetString := GLULibrary.Symbol('gluGetString');
  @gluGetTessProperty := GLULibrary.Symbol('gluGetTessProperty');
  @gluLoadSamplingMatrices := GLULibrary.Symbol('gluLoadSamplingMatrices');
  @gluLookAt := GLULibrary.Symbol('gluLookAt');
  @gluNewNurbsRenderer := GLULibrary.Symbol('gluNewNurbsRenderer');
  @gluNewQuadric := GLULibrary.Symbol('gluNewQuadric');
  @gluNewTess := GLULibrary.Symbol('gluNewTess');
  @gluNextContour := GLULibrary.Symbol('gluNextContour');
  @gluNurbsCallback := GLULibrary.Symbol('gluNurbsCallback');
  @gluNurbsCurve := GLULibrary.Symbol('gluNurbsCurve');
  @gluNurbsProperty := GLULibrary.Symbol('gluNurbsProperty');
  @gluNurbsSurface := GLULibrary.Symbol('gluNurbsSurface');
  @gluOrtho2D := GLULibrary.Symbol('gluOrtho2D');
  @gluPartialDisk := GLULibrary.Symbol('gluPartialDisk');
  @gluPerspective := GLULibrary.Symbol('gluPerspective');
  @gluPickMatrix := GLULibrary.Symbol('gluPickMatrix');
  @gluProject := GLULibrary.Symbol('gluProject');
  @gluPwlCurve := GLULibrary.Symbol('gluPwlCurve');
  @gluQuadricCallback := GLULibrary.Symbol('gluQuadricCallback');
  @gluQuadricDrawStyle := GLULibrary.Symbol('gluQuadricDrawStyle');
  @gluQuadricNormals := GLULibrary.Symbol('gluQuadricNormals');
  @gluQuadricOrientation := GLULibrary.Symbol('gluQuadricOrientation');
  @gluQuadricTexture := GLULibrary.Symbol('gluQuadricTexture');
  @gluScaleImage := GLULibrary.Symbol('gluScaleImage');
  @gluSphere := GLULibrary.Symbol('gluSphere');
  @gluTessBeginContour := GLULibrary.Symbol('gluTessBeginContour');
  @gluTessBeginPolygon := GLULibrary.Symbol('gluTessBeginPolygon');
  @gluTessCallback := GLULibrary.Symbol('gluTessCallback');
  @gluTessEndContour := GLULibrary.Symbol('gluTessEndContour');
  @gluTessEndPolygon := GLULibrary.Symbol('gluTessEndPolygon');
  @gluTessNormal := GLULibrary.Symbol('gluTessNormal');
  @gluTessProperty := GLULibrary.Symbol('gluTessProperty');
  @gluTessVertex := GLULibrary.Symbol('gluTessVertex');
  @gluUnProject := GLULibrary.Symbol('gluUnProject');
 end;
end;

{---------------------------------------------------------------------------------------------------------------------- }

procedure ClearProcExtensions;
begin
 glArrayElementEXT := nil;
 glDrawArraysEXT := nil;
 glVertexPointerEXT := nil;
 glNormalPointerEXT := nil;
 glColorPointerEXT := nil;
 glIndexPointerEXT := nil;
 glTexCoordPointerEXT := nil;
 glEdgeFlagPointerEXT := nil;
 glGetPointervEXT := nil;
 glArrayElementArrayEXT := nil;
 glAddSwapHintRectWIN := nil;
 glColorTableEXT := nil;
 glColorSubTableEXT := nil;
 glGetColorTableEXT := nil;
 glGetColorTablePameterivEXT := nil;
 glGetColorTablePameterfvEXT := nil;
 gluNurbsCallbackDataEXT := nil;
 gluNewNurbsTessellatorEXT := nil;
 gluDeleteNurbsTessellatorEXT := nil;
 glLockArraysEXT := nil;
 glUnlockArraysEXT := nil;
 glCopyTexImage1DEXT := nil;
 glCopyTexSubImage1DEXT := nil;
 glCopyTexImage2DEXT := nil;
 glCopyTexSubImage2DEXT := nil;
 glCopyTexSubImage3DEXT := nil;
 glCullParameterfvEXT := nil;
 glCullParameterdvEXT := nil;
 glIndexFuncEXT := nil;
 glIndexMaterialEXT := nil;
 glPolygonOffsetEXT := nil;
 glTexSubImage1DEXT := nil;
 glTexSubImage2DEXT := nil;
 glTexSubImage3DEXT := nil;
 glGenTexturesEXT := nil;
 glDeleteTexturesEXT := nil;
 glBindTextureEXT := nil;
 glPrioritizeTexturesEXT := nil;
 glAreTexturesResidentEXT := nil;
 glIsTextureEXT := nil;

 glMultiTexCoord1dARB := nil;
 glMultiTexCoord1dVARB := nil;
 glMultiTexCoord1fARBP := nil;
 glMultiTexCoord1fVARB := nil;
 glMultiTexCoord1iARB := nil;
 glMultiTexCoord1iVARB := nil;
 glMultiTexCoord1sARBP := nil;
 glMultiTexCoord1sVARB := nil;
 glMultiTexCoord2dARB := nil;
 glMultiTexCoord2dvARB := nil;
 glMultiTexCoord2fARB := nil;
 glMultiTexCoord2fvARB := nil;
 glMultiTexCoord2iARB := nil;
 glMultiTexCoord2ivARB := nil;
 glMultiTexCoord2sARB := nil;
 glMultiTexCoord2svARB := nil;
 glMultiTexCoord3dARB := nil;
 glMultiTexCoord3dvARB := nil;
 glMultiTexCoord3fARB := nil;
 glMultiTexCoord3fvARB := nil;
 glMultiTexCoord3iARB := nil;
 glMultiTexCoord3ivARB := nil;
 glMultiTexCoord3sARB := nil;
 glMultiTexCoord3svARB := nil;
 glMultiTexCoord4dARB := nil;
 glMultiTexCoord4dvARB := nil;
 glMultiTexCoord4fARB := nil;
 glMultiTexCoord4fvARB := nil;
 glMultiTexCoord4iARB := nil;
 glMultiTexCoord4ivARB := nil;
 glMultiTexCoord4sARB := nil;
 glMultiTexCoord4svARB := nil;
 glActiveTextureARB := nil;
 glClientActiveTextureARB := nil;

 { EXT_compiled_vertex_array }
 glLockArrayEXT := nil;
 glUnlockArrayEXT := nil;

 { EXT_cull_vertex }
 glCullParameterdvEXT := nil;
 glCullParameterfvEXT := nil;

 { WIN_swap_hint }
 glAddSwapHintRectWIN := nil;

 { EXT_point_parameter }
 glPointParameterfEXT := nil;
 glPointParameterfvEXT := nil;

 { GL_ARB_transpose_matrix }
 glLoadTransposeMatrixfARB := nil;
 glLoadTransposeMatrixdARB := nil;
 glMultTransposeMatrixfARB := nil;
 glMultTransposeMatrixdARB := nil;

 glSampleCoverageARB := nil;
 glSamplePassARB := nil;

 { GL_ARB_multisample }
 glCompressedTexImage3DARB := nil;
 glCompressedTexImage2DARB := nil;
 glCompressedTexImage1DARB := nil;
 glCompressedTexSubImage3DARB := nil;
 glCompressedTexSubImage2DARB := nil;
 glCompressedTexSubImage1DARB := nil;
 glGetCompressedTexImageARB := nil;

 { GL_EXT_blend_color }
 glBlendColorEXT := nil;

 { GL_EXT_texture3D }
 glTexImage3DEXT := nil;

 { GL_SGIS_texture_filter4 }
 glGetTexFilterFuncSGIS := nil;
 glTexFilterFuncSGIS := nil;

 { GL_EXT_histogram }
 glGetHistogramEXT := nil;
 glGetHistogramParameterfvEXT := nil;
 glGetHistogramParameterivEXT := nil;
 glGetMinmaxEXT := nil;
 glGetMinmaxParameterfvEXT := nil;
 glGetMinmaxParameterivEXT := nil;
 glHistogramEXT := nil;
 glMinmaxEXT := nil;
 glResetHistogramEXT := nil;
 glResetMinmaxEXT := nil;

 { GL_EXT_convolution }
 glConvolutionFilter1DEXT := nil;
 glConvolutionFilter2DEXT := nil;
 glConvolutionParameterfEXT := nil;
 glConvolutionParameterfvEXT := nil;
 glConvolutionParameteriEXT := nil;
 glConvolutionParameterivEXT := nil;
 glCopyConvolutionFilter1DEXT := nil;
 glCopyConvolutionFilter2DEXT := nil;
 glGetConvolutionFilterEXT := nil;
 glGetConvolutionParameterfvEXT := nil;
 glGetConvolutionParameterivEXT := nil;
 glGetSeparableFilterEXT := nil;
 glSeparableFilter2DEXT := nil;

 { GL_SGI_color_table }
 glColorTableSGI := nil;
 glColorTableParameterfvSGI := nil;
 glColorTableParameterivSGI := nil;
 glCopyColorTableSGI := nil;
 glGetColorTableSGI := nil;
 glGetColorTableParameterfvSGI := nil;
 glGetColorTableParameterivSGI := nil;

 { GL_SGIX_pixel_texture }
 glPixelTexGenSGIX := nil;

 { GL_SGIS_pixel_texture }
 glPixelTexGenParameteriSGIS := nil;
 glPixelTexGenParameterivSGIS := nil;
 glPixelTexGenParameterfSGIS := nil;
 glPixelTexGenParameterfvSGIS := nil;
 glGetPixelTexGenParameterivSGIS := nil;
 glGetPixelTexGenParameterfvSGIS := nil;

 { GL_SGIS_texture4D }
 glTexImage4DSGIS := nil;
 glTexSubImage4DSGIS := nil;

 { GL_SGIS_detail_texture }
 glDetailTexFuncSGIS := nil;
 glGetDetailTexFuncSGIS := nil;

 { GL_SGIS_sharpen_texture }
 glSharpenTexFuncSGIS := nil;
 glGetSharpenTexFuncSGIS := nil;

 { GL_SGIS_multisample }
 glSampleMaskSGIS := nil;
 glSamplePatternSGIS := nil;

 { GL_EXT_blend_minmax }
 glBlendEquationEXT := nil;

 { GL_SGIX_sprite }
 glSpriteParameterfSGIX := nil;
 glSpriteParameterfvSGIX := nil;
 glSpriteParameteriSGIX := nil;
 glSpriteParameterivSGIX := nil;

 { GL_EXT_point_parameters }
 glPointParameterfSGIS := nil;
 glPointParameterfvSGIS := nil;

 { GL_SGIX_instruments }
 glGetInstrumentsSGIX := nil;
 glInstrumentsBufferSGIX := nil;
 glPollInstrumentsSGIX := nil;
 glReadInstrumentsSGIX := nil;
 glStartInstrumentsSGIX := nil;
 glStopInstrumentsSGIX := nil;

 { GL_SGIX_framezoom }
 glFrameZoomSGIX := nil;

 { GL_SGIX_tag_sample_buffer }
 glTagSampleBufferSGIX := nil;

 { GL_SGIX_polynomial_ffd }
 glDeformationMap3dSGIX := nil;
 glDeformationMap3fSGIX := nil;
 glDeformSGIX := nil;
 glLoadIdentityDeformationMapSGIX := nil;

 { GL_SGIX_reference_plane }
 glReferencePlaneSGIX := nil;

 { GL_SGIX_flush_raster }
 glFlushRasterSGIX := nil;

 { GL_SGIS_fog_function }
 glFogFuncSGIS := nil;
 glGetFogFuncSGIS := nil;

 { GL_HP_image_transform }
 glImageTransformParameteriHP := nil;
 glImageTransformParameterfHP := nil;
 glImageTransformParameterivHP := nil;
 glImageTransformParameterfvHP := nil;
 glGetImageTransformParameterivHP := nil;
 glGetImageTransformParameterfvHP := nil;

 { GL_EXT_color_subtable }
 glCopyColorSubTableEXT := nil;

 { GL_PGI_misc_hints }
 glHintPGI := nil;

 { GL_EXT_paletted_texture }
 glGetColorTableParameterivEXT := nil;
 glGetColorTableParameterfvEXT := nil;

 { GL_SGIX_list_priority }
 glGetListParameterfvSGIX := nil;
 glGetListParameterivSGIX := nil;
 glListParameterfSGIX := nil;
 glListParameterfvSGIX := nil;
 glListParameteriSGIX := nil;
 glListParameterivSGIX := nil;

 { GL_SGIX_fragment_lighting }
 glFragmentColorMaterialSGIX := nil;
 glFragmentLightfSGIX := nil;
 glFragmentLightfvSGIX := nil;
 glFragmentLightiSGIX := nil;
 glFragmentLightivSGIX := nil;
 glFragmentLightModelfSGIX := nil;
 glFragmentLightModelfvSGIX := nil;
 glFragmentLightModeliSGIX := nil;
 glFragmentLightModelivSGIX := nil;
 glFragmentMaterialfSGIX := nil;
 glFragmentMaterialfvSGIX := nil;
 glFragmentMaterialiSGIX := nil;
 glFragmentMaterialivSGIX := nil;
 glGetFragmentLightfvSGIX := nil;
 glGetFragmentLightivSGIX := nil;
 glGetFragmentMaterialfvSGIX := nil;
 glGetFragmentMaterialivSGIX := nil;
 glLightEnviSGIX := nil;

 { GL_EXT_draw_range_elements }
 glDrawRangeElementsEXT := nil;

 { GL_EXT_light_texture }
 glApplyTextureEXT := nil;
 glTextureLightEXT := nil;
 glTextureMaterialEXT := nil;

 { GL_SGIX_async }
 glAsyncMarkerSGIX := nil;
 glFinishAsyncSGIX := nil;
 glPollAsyncSGIX := nil;
 glGenAsyncMarkersSGIX := nil;
 glDeleteAsyncMarkersSGIX := nil;
 glIsAsyncMarkerSGIX := nil;

 { GL_INTEL_parallel_arrays }
 glVertexPointervINTEL := nil;
 glNormalPointervINTEL := nil;
 glColorPointervINTEL := nil;
 glTexCoordPointervINTEL := nil;

 { GL_EXT_pixel_transform }
 glPixelTransformParameteriEXT := nil;
 glPixelTransformParameterfEXT := nil;
 glPixelTransformParameterivEXT := nil;
 glPixelTransformParameterfvEXT := nil;

 { GL_EXT_secondary_color }
 glSecondaryColor3bEXT := nil;
 glSecondaryColor3bvEXT := nil;
 glSecondaryColor3dEXT := nil;
 glSecondaryColor3dvEXT := nil;
 glSecondaryColor3fEXT := nil;
 glSecondaryColor3fvEXT := nil;
 glSecondaryColor3iEXT := nil;
 glSecondaryColor3ivEXT := nil;
 glSecondaryColor3sEXT := nil;
 glSecondaryColor3svEXT := nil;
 glSecondaryColor3ubEXT := nil;
 glSecondaryColor3ubvEXT := nil;
 glSecondaryColor3uiEXT := nil;
 glSecondaryColor3uivEXT := nil;
 glSecondaryColor3usEXT := nil;
 glSecondaryColor3usvEXT := nil;
 glSecondaryColorPointerEXT := nil;

 { GL_EXT_texture_perturb_normal }
 glTextureNormalEXT := nil;

 { GL_EXT_multi_draw_arrays }
 glMultiDrawArraysEXT := nil;
 glMultiDrawElementsEXT := nil;

 { GL_EXT_fog_coord }
 glFogCoordfEXT := nil;
 glFogCoordfvEXT := nil;
 glFogCoorddEXT := nil;
 glFogCoorddvEXT := nil;
 glFogCoordPointerEXT := nil;

 { GL_EXT_coordinate_frame }
 glTangent3bEXT := nil;
 glTangent3bvEXT := nil;
 glTangent3dEXT := nil;
 glTangent3dvEXT := nil;
 glTangent3fEXT := nil;
 glTangent3fvEXT := nil;
 glTangent3iEXT := nil;
 glTangent3ivEXT := nil;
 glTangent3sEXT := nil;
 glTangent3svEXT := nil;
 glBinormal3bEXT := nil;
 glBinormal3bvEXT := nil;
 glBinormal3dEXT := nil;
 glBinormal3dvEXT := nil;
 glBinormal3fEXT := nil;
 glBinormal3fvEXT := nil;
 glBinormal3iEXT := nil;
 glBinormal3ivEXT := nil;
 glBinormal3sEXT := nil;
 glBinormal3svEXT := nil;
 glTangentPointerEXT := nil;
 glBinormalPointerEXT := nil;

 { GL_SUNX_constant_data }
 glFinishTextureSUNX := nil;

 { GL_SUN_global_alpha }
 glGlobalAlphaFactorbSUN := nil;
 glGlobalAlphaFactorsSUN := nil;
 glGlobalAlphaFactoriSUN := nil;
 glGlobalAlphaFactorfSUN := nil;
 glGlobalAlphaFactordSUN := nil;
 glGlobalAlphaFactorubSUN := nil;
 glGlobalAlphaFactorusSUN := nil;
 glGlobalAlphaFactoruiSUN := nil;

 { GL_SUN_triangle_list }
 glReplacementCodeuiSUN := nil;
 glReplacementCodeusSUN := nil;
 glReplacementCodeubSUN := nil;
 glReplacementCodeuivSUN := nil;
 glReplacementCodeusvSUN := nil;
 glReplacementCodeubvSUN := nil;
 glReplacementCodePointerSUN := nil;

 { GL_SUN_vertex }
 glColor4ubVertex2fSUN := nil;
 glColor4ubVertex2fvSUN := nil;
 glColor4ubVertex3fSUN := nil;
 glColor4ubVertex3fvSUN := nil;
 glColor3fVertex3fSUN := nil;
 glColor3fVertex3fvSUN := nil;
 glNormal3fVertex3fSUN := nil;
 glNormal3fVertex3fvSUN := nil;
 glColor4fNormal3fVertex3fSUN := nil;
 glColor4fNormal3fVertex3fvSUN := nil;
 glTexCoord2fVertex3fSUN := nil;
 glTexCoord2fVertex3fvSUN := nil;
 glTexCoord4fVertex4fSUN := nil;
 glTexCoord4fVertex4fvSUN := nil;
 glTexCoord2fColor4ubVertex3fSUN := nil;
 glTexCoord2fColor4ubVertex3fvSUN := nil;
 glTexCoord2fColor3fVertex3fSUN := nil;
 glTexCoord2fColor3fVertex3fvSUN := nil;
 glTexCoord2fNormal3fVertex3fSUN := nil;
 glTexCoord2fNormal3fVertex3fvSUN := nil;
 glTexCoord2fColor4fNormal3fVertex3fSUN := nil;
 glTexCoord2fColor4fNormal3fVertex3fvSUN := nil;
 glTexCoord4fColor4fNormal3fVertex4fSUN := nil;
 glTexCoord4fColor4fNormal3fVertex4fvSUN := nil;
 glReplacementCodeuiVertex3fSUN := nil;
 glReplacementCodeuiVertex3fvSUN := nil;
 glReplacementCodeuiColor4ubVertex3fSUN := nil;
 glReplacementCodeuiColor4ubVertex3fvSUN := nil;
 glReplacementCodeuiColor3fVertex3fSUN := nil;
 glReplacementCodeuiColor3fVertex3fvSUN := nil;
 glReplacementCodeuiNormal3fVertex3fSUN := nil;
 glReplacementCodeuiNormal3fVertex3fvSUN := nil;
 glReplacementCodeuiColor4fNormal3fVertex3fSUN := nil;
 glReplacementCodeuiColor4fNormal3fVertex3fvSUN := nil;
 glReplacementCodeuiTexCoord2fVertex3fSUN := nil;
 glReplacementCodeuiTexCoord2fVertex3fvSUN := nil;
 glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := nil;
 glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := nil;
 glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := nil;
 glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := nil;

 { GL_EXT_blend_func_separate }
 glBlendFuncSeparateEXT := nil;

 { GL_EXT_vertex_weighting }
 glVertexWeightfEXT := nil;
 glVertexWeightfvEXT := nil;
 glVertexWeightPointerEXT := nil;

 { GL_NV_vertex_array_range }
 glFlushVertexArrayRangeNV := nil;
 glVertexArrayRangeNV := nil;
 wglAllocateMemoryNV := nil;
 wglFreeMemoryNV := nil;

 { GL_NV_register_combiners }
 glCombinerParameterfvNV := nil;
 glCombinerParameterfNV := nil;
 glCombinerParameterivNV := nil;
 glCombinerParameteriNV := nil;
 glCombinerInputNV := nil;
 glCombinerOutputNV := nil;
 glFinalCombinerInputNV := nil;
 glGetCombinerInputParameterfvNV := nil;
 glGetCombinerInputParameterivNV := nil;
 glGetCombinerOutputParameterfvNV := nil;
 glGetCombinerOutputParameterivNV := nil;
 glGetFinalCombinerInputParameterfvNV := nil;
 glGetFinalCombinerInputParameterivNV := nil;

 { GL_MESA_resize_buffers }
 glResizeBuffersMESA := nil;

 { GL_MESA_window_pos }
 glWindowPos2dMESA := nil;
 glWindowPos2dvMESA := nil;
 glWindowPos2fMESA := nil;
 glWindowPos2fvMESA := nil;
 glWindowPos2iMESA := nil;
 glWindowPos2ivMESA := nil;
 glWindowPos2sMESA := nil;
 glWindowPos2svMESA := nil;
 glWindowPos3dMESA := nil;
 glWindowPos3dvMESA := nil;
 glWindowPos3fMESA := nil;
 glWindowPos3fvMESA := nil;
 glWindowPos3iMESA := nil;
 glWindowPos3ivMESA := nil;
 glWindowPos3sMESA := nil;
 glWindowPos3svMESA := nil;
 glWindowPos4dMESA := nil;
 glWindowPos4dvMESA := nil;
 glWindowPos4fMESA := nil;
 glWindowPos4fvMESA := nil;
 glWindowPos4iMESA := nil;
 glWindowPos4ivMESA := nil;
 glWindowPos4sMESA := nil;
 glWindowPos4svMESA := nil;

 { GL_IBM_multimode_draw_arrays }
 glMultiModeDrawArraysIBM := nil;
 glMultiModeDrawElementsIBM := nil;

 { GL_IBM_vertex_array_lists }
 glColorPointerListIBM := nil;
 glSecondaryColorPointerListIBM := nil;
 glEdgeFlagPointerListIBM := nil;
 glFogCoordPointerListIBM := nil;
 glIndexPointerListIBM := nil;
 glNormalPointerListIBM := nil;
 glTexCoordPointerListIBM := nil;
 glVertexPointerListIBM := nil;

 { GL_3DFX_tbuffer }
 glTbufferMask3DFX := nil;

 { GL_EXT_multisample }
 glSampleMaskEXT := nil;
 glSamplePatternEXT := nil;

 { GL_SGIS_texture_color_mask }
 glTextureColorMaskSGIS := nil;

 { GL_SGIX_igloo_interface }
 glIglooInterfaceSGIX := nil;

 { GLU extensions }
 gluNurbsCallbackDataEXT := nil;
 gluNewNurbsTessellatorEXT := nil;
 gluDeleteNurbsTessellatorEXT := nil;

 { GL_NV_vertex_program }
 glAreProgramsResidentNV := nil;
 glBindProgramNV := nil;
 glDeleteProgramsNV := nil;
 glExecuteProgramNV := nil;
 glGenProgramsNV := nil;
 glGetProgramParameterdvNV := nil;
 glGetProgramParameterfvNV := nil;
 glGetProgramivNV := nil;
 glGetProgramStringNV := nil;
 glGetTrackMatrixivNV := nil;
 glGetVertexAttribdvNV := nil;
 glGetVertexAttribfvNV := nil;
 glGetVertexAttribivNV := nil;
 glGetVertexAttribPointervNV := nil;
 glIsProgramNV := nil;
 glLoadProgramNV := nil;
 glProgramParameter4dNV := nil;
 glProgramParameter4dvNV := nil;
 glProgramParameter4fNV := nil;
 glProgramParameter4fvNV := nil;
 glProgramParameters4dvNV := nil;
 glProgramParameters4fvNV := nil;
 glRequestResidentProgramsNV := nil;
 glTrackMatrixNV := nil;
 glVertexAttribPointerNV := nil;
 glVertexAttrib1dNV := nil;
 glVertexAttrib1dvNV := nil;
 glVertexAttrib1fNV := nil;
 glVertexAttrib1fvNV := nil;
 glVertexAttrib1sNV := nil;
 glVertexAttrib1svNV := nil;
 glVertexAttrib2dNV := nil;
 glVertexAttrib2dvNV := nil;
 glVertexAttrib2fNV := nil;
 glVertexAttrib2fvNV := nil;
 glVertexAttrib2sNV := nil;
 glVertexAttrib2svNV := nil;
 glVertexAttrib3dNV := nil;
 glVertexAttrib3dvNV := nil;
 glVertexAttrib3fNV := nil;
 glVertexAttrib3fvNV := nil;
 glVertexAttrib3sNV := nil;
 glVertexAttrib3svNV := nil;
 glVertexAttrib4dNV := nil;
 glVertexAttrib4dvNV := nil;
 glVertexAttrib4fNV := nil;
 glVertexAttrib4fvNV := nil;
 glVertexAttrib4sNV := nil;
 glVertexAttrib4svNV := nil;
 glVertexAttrib4ubvNV := nil;
 glVertexAttribs1dvNV := nil;
 glVertexAttribs1fvNV := nil;
 glVertexAttribs1svNV := nil;
 glVertexAttribs2dvNV := nil;
 glVertexAttribs2fvNV := nil;
 glVertexAttribs2svNV := nil;
 glVertexAttribs3dvNV := nil;
 glVertexAttribs3fvNV := nil;
 glVertexAttribs3svNV := nil;
 glVertexAttribs4dvNV := nil;
 glVertexAttribs4fvNV := nil;
 glVertexAttribs4svNV := nil;
 glVertexAttribs4ubvNV := nil;

 {$ifdef UNIX}
 glXGetVideoSyncSGI := nil;
 glXWaitVideoSyncSGI := nil;
 glXFreeContextEXT := nil;
 glXGetContextIDEXT := nil;
 glXGetCurrentDisplayEXT := nil;
 glXImportContextEXT := nil;
 glXQueryContextInfoEXT := nil;
 glXCopySubBufferMESA := nil;
 glXCreateGLXPixmapMESA := nil;
 glXReleaseBuffersMESA := nil;
 glXSet3DfxModeMESA := nil;
 {$endif}
end;

procedure LoadProcExtensions;

  function LoadExtensionFunction(functionName: PChar): pointer;
  begin
   {$ifdef WIN32} result := wglGetProcAddress(functionName);
   {$else}        result := GLLibrary.Symbol(functionName);
   {$endif}
  end;

var Old_GL_SymbolErrorBehaviour: TDynLibSymbolErrorBehaviour;
begin
 { save and unconditionally change GLLibrary.SymbolErrorBehaviour }
 Old_GL_SymbolErrorBehaviour := GLLibrary.SymbolErrorBehaviour;
 GLLibrary.SymbolErrorBehaviour := seReturnNil;
 try

  glArrayElementEXT := LoadExtensionFunction('glArrayElementEXT');
  glDrawArraysEXT := LoadExtensionFunction('glDrawArraysEXT');
  glVertexPointerEXT := LoadExtensionFunction('glVertexPointerEXT');
  glNormalPointerEXT := LoadExtensionFunction('glNormalPointerEXT');
  glColorPointerEXT := LoadExtensionFunction('glColorPointerEXT');
  glIndexPointerEXT := LoadExtensionFunction('glIndexPointerEXT');
  glTexCoordPointerEXT := LoadExtensionFunction('glTexCoordPointerEXT');
  glEdgeFlagPointerEXT := LoadExtensionFunction('glEdgeFlagPointerEXT');
  glGetPointervEXT := LoadExtensionFunction('glGetPointervEXT');
  glArrayElementArrayEXT := LoadExtensionFunction('glArrayElementArrayEXT');
  glAddSwapHintRectWIN := LoadExtensionFunction('glAddSwapHintRectWIN');
  glColorTableEXT := LoadExtensionFunction('glColorTableEXT');
  glColorSubTableEXT := LoadExtensionFunction('glColorSubTableEXT');
  glGetColorTableEXT := LoadExtensionFunction('glGetColorTableEXT');
  glGetColorTablePameterivEXT := LoadExtensionFunction('glGetColorTablePameterivEXT');
  glGetColorTablePameterfvEXT := LoadExtensionFunction('glGetColorTablePameterfvEXT');
  gluNurbsCallbackDataEXT := LoadExtensionFunction('gluNurbsCallbackDataEXT');
  gluNewNurbsTessellatorEXT := LoadExtensionFunction('gluNewNurbsTessellatorEXT');
  gluDeleteNurbsTessellatorEXT := LoadExtensionFunction('gluDeleteNurbsTessellatorEXT');
  glLockArraysEXT := LoadExtensionFunction('glLockArraysEXT');
  glUnlockArraysEXT := LoadExtensionFunction('glUnlockArraysEXT');
  glCopyTexImage1DEXT := LoadExtensionFunction('glCopyTexImage1DEXT');
  glCopyTexSubImage1DEXT := LoadExtensionFunction('glCopyTexSubImage1DEXT');
  glCopyTexImage2DEXT := LoadExtensionFunction('glCopyTexImage2DEXT');
  glCopyTexSubImage2DEXT := LoadExtensionFunction('glCopyTexSubImage2DEXT');
  glCopyTexSubImage3DEXT := LoadExtensionFunction('glCopyTexSubImage3DEXT');
  glCullParameterfvEXT := LoadExtensionFunction('glCullParameterfvEXT');
  glCullParameterdvEXT := LoadExtensionFunction('glCullParameterdvEXT');
  glIndexFuncEXT := LoadExtensionFunction('glIndexFuncEXT');
  glIndexMaterialEXT := LoadExtensionFunction('glIndexMaterialEXT');
  glPolygonOffsetEXT := LoadExtensionFunction('glPolygonOffsetEXT');
  glTexSubImage1DEXT := LoadExtensionFunction('glTexSubImage1DEXT');
  glTexSubImage2DEXT := LoadExtensionFunction('glTexSubImage2DEXT');
  glTexSubImage3DEXT := LoadExtensionFunction('glTexSubImage3DEXT');
  glGenTexturesEXT := LoadExtensionFunction('glGenTexturesEXT');
  glDeleteTexturesEXT := LoadExtensionFunction('glDeleteTexturesEXT');
  glBindTextureEXT := LoadExtensionFunction('glBindTextureEXT');
  glPrioritizeTexturesEXT := LoadExtensionFunction('glPrioritizeTexturesEXT');
  glAreTexturesResidentEXT := LoadExtensionFunction('glAreTexturesResidentEXT');
  glIsTextureEXT := LoadExtensionFunction('glIsTextureEXT');

  glMultiTexCoord1dARB := LoadExtensionFunction('glMultiTexCoord1dARB');
  glMultiTexCoord1dVARB := LoadExtensionFunction('glMultiTexCoord1dVARB');
  glMultiTexCoord1fARBP := LoadExtensionFunction('glMultiTexCoord1fARBP');
  glMultiTexCoord1fVARB := LoadExtensionFunction('glMultiTexCoord1fVARB');
  glMultiTexCoord1iARB := LoadExtensionFunction('glMultiTexCoord1iARB');
  glMultiTexCoord1iVARB := LoadExtensionFunction('glMultiTexCoord1iVARB');
  glMultiTexCoord1sARBP := LoadExtensionFunction('glMultiTexCoord1sARBP');
  glMultiTexCoord1sVARB := LoadExtensionFunction('glMultiTexCoord1sVARB');
  glMultiTexCoord2dARB := LoadExtensionFunction('glMultiTexCoord2dARB');
  glMultiTexCoord2dvARB := LoadExtensionFunction('glMultiTexCoord2dvARB');
  glMultiTexCoord2fARB := LoadExtensionFunction('glMultiTexCoord2fARB');
  glMultiTexCoord2fvARB := LoadExtensionFunction('glMultiTexCoord2fvARB');
  glMultiTexCoord2iARB := LoadExtensionFunction('glMultiTexCoord2iARB');
  glMultiTexCoord2ivARB := LoadExtensionFunction('glMultiTexCoord2ivARB');
  glMultiTexCoord2sARB := LoadExtensionFunction('glMultiTexCoord2sARB');
  glMultiTexCoord2svARB := LoadExtensionFunction('glMultiTexCoord2svARB');
  glMultiTexCoord3dARB := LoadExtensionFunction('glMultiTexCoord3dARB');
  glMultiTexCoord3dvARB := LoadExtensionFunction('glMultiTexCoord3dvARB');
  glMultiTexCoord3fARB := LoadExtensionFunction('glMultiTexCoord3fARB');
  glMultiTexCoord3fvARB := LoadExtensionFunction('glMultiTexCoord3fvARB');
  glMultiTexCoord3iARB := LoadExtensionFunction('glMultiTexCoord3iARB');
  glMultiTexCoord3ivARB := LoadExtensionFunction('glMultiTexCoord3ivARB');
  glMultiTexCoord3sARB := LoadExtensionFunction('glMultiTexCoord3sARB');
  glMultiTexCoord3svARB := LoadExtensionFunction('glMultiTexCoord3svARB');
  glMultiTexCoord4dARB := LoadExtensionFunction('glMultiTexCoord4dARB');
  glMultiTexCoord4dvARB := LoadExtensionFunction('glMultiTexCoord4dvARB');
  glMultiTexCoord4fARB := LoadExtensionFunction('glMultiTexCoord4fARB');
  glMultiTexCoord4fvARB := LoadExtensionFunction('glMultiTexCoord4fvARB');
  glMultiTexCoord4iARB := LoadExtensionFunction('glMultiTexCoord4iARB');
  glMultiTexCoord4ivARB := LoadExtensionFunction('glMultiTexCoord4ivARB');
  glMultiTexCoord4sARB := LoadExtensionFunction('glMultiTexCoord4sARB');
  glMultiTexCoord4svARB := LoadExtensionFunction('glMultiTexCoord4svARB');
  glActiveTextureARB := LoadExtensionFunction('glActiveTextureARB');
  glClientActiveTextureARB := LoadExtensionFunction('glClientActiveTextureARB');

   { EXT_compiled_vertex_array }
  glLockArrayEXT := LoadExtensionFunction('glLockArrayEXT');
  glUnlockArrayEXT := LoadExtensionFunction('glUnlockArrayEXT');

   { EXT_cull_vertex }
  glCullParameterdvEXT := LoadExtensionFunction('glCullParameterdvEXT');
  glCullParameterfvEXT := LoadExtensionFunction('glCullParameterfvEXT');

   { WIN_swap_hint }
  glAddSwapHintRectWIN := LoadExtensionFunction('glAddSwapHintRectWIN');

   { EXT_point_parameter }
  glPointParameterfEXT := LoadExtensionFunction('glPointParameterfEXT');
  glPointParameterfvEXT := LoadExtensionFunction('glPointParameterfvEXT');

   { GL_ARB_transpose_matrix }
  glLoadTransposeMatrixfARB := LoadExtensionFunction('glLoadTransposeMatrixfARB');
  glLoadTransposeMatrixdARB := LoadExtensionFunction('glLoadTransposeMatrixdARB');
  glMultTransposeMatrixfARB := LoadExtensionFunction('glMultTransposeMatrixfARB');
  glMultTransposeMatrixdARB := LoadExtensionFunction('glMultTransposeMatrixdARB');

  glSampleCoverageARB := LoadExtensionFunction('glSampleCoverageARB');
  glSamplePassARB := LoadExtensionFunction('glSamplePassARB');

   { GL_ARB_multisample }
  glCompressedTexImage3DARB := LoadExtensionFunction('glCompressedTexImage3DARB');
  glCompressedTexImage2DARB := LoadExtensionFunction('glCompressedTexImage2DARB');
  glCompressedTexImage1DARB := LoadExtensionFunction('glCompressedTexImage1DARB');
  glCompressedTexSubImage3DARB := LoadExtensionFunction('glCompressedTexSubImage3DARB');
  glCompressedTexSubImage2DARB := LoadExtensionFunction('glCompressedTexSubImage2DARB');
  glCompressedTexSubImage1DARB := LoadExtensionFunction('glCompressedTexSubImage1DARB');
  glGetCompressedTexImageARB := LoadExtensionFunction('glGetCompressedTexImageARB');

   { GL_EXT_blend_color }
  glBlendColorEXT := LoadExtensionFunction('glBlendColorEXT');

   { GL_EXT_texture3D }
  glTexImage3DEXT := LoadExtensionFunction('glTexImage3DEXT');

   { GL_SGIS_texture_filter4 }
  glGetTexFilterFuncSGIS := LoadExtensionFunction('glGetTexFilterFuncSGIS');
  glTexFilterFuncSGIS := LoadExtensionFunction('glTexFilterFuncSGIS');

   { GL_EXT_histogram }
  glGetHistogramEXT := LoadExtensionFunction('glGetHistogramEXT');
  glGetHistogramParameterfvEXT := LoadExtensionFunction('glGetHistogramParameterfvEXT');
  glGetHistogramParameterivEXT := LoadExtensionFunction('glGetHistogramParameterivEXT');
  glGetMinmaxEXT := LoadExtensionFunction('glGetMinmaxEXT');
  glGetMinmaxParameterfvEXT := LoadExtensionFunction('glGetMinmaxParameterfvEXT');
  glGetMinmaxParameterivEXT := LoadExtensionFunction('glGetMinmaxParameterivEXT');
  glHistogramEXT := LoadExtensionFunction('glHistogramEXT');
  glMinmaxEXT := LoadExtensionFunction('glMinmaxEXT');
  glResetHistogramEXT := LoadExtensionFunction('glResetHistogramEXT');
  glResetMinmaxEXT := LoadExtensionFunction('glResetMinmaxEXT');

   { GL_EXT_convolution }
  glConvolutionFilter1DEXT := LoadExtensionFunction('glConvolutionFilter1DEXT');
  glConvolutionFilter2DEXT := LoadExtensionFunction('glConvolutionFilter2DEXT');
  glConvolutionParameterfEXT := LoadExtensionFunction('glConvolutionParameterfEXT');
  glConvolutionParameterfvEXT := LoadExtensionFunction('glConvolutionParameterfvEXT');
  glConvolutionParameteriEXT := LoadExtensionFunction('glConvolutionParameteriEXT');
  glConvolutionParameterivEXT := LoadExtensionFunction('glConvolutionParameterivEXT');
  glCopyConvolutionFilter1DEXT := LoadExtensionFunction('glCopyConvolutionFilter1DEXT');
  glCopyConvolutionFilter2DEXT := LoadExtensionFunction('glCopyConvolutionFilter2DEXT');
  glGetConvolutionFilterEXT := LoadExtensionFunction('glGetConvolutionFilterEXT');
  glGetConvolutionParameterfvEXT := LoadExtensionFunction('glGetConvolutionParameterfvEXT');
  glGetConvolutionParameterivEXT := LoadExtensionFunction('glGetConvolutionParameterivEXT');
  glGetSeparableFilterEXT := LoadExtensionFunction('glGetSeparableFilterEXT');
  glSeparableFilter2DEXT := LoadExtensionFunction('glSeparableFilter2DEXT');

   { GL_SGI_color_table }
  glColorTableSGI := LoadExtensionFunction('glColorTableSGI');
  glColorTableParameterfvSGI := LoadExtensionFunction('glColorTableParameterfvSGI');
  glColorTableParameterivSGI := LoadExtensionFunction('glColorTableParameterivSGI');
  glCopyColorTableSGI := LoadExtensionFunction('glCopyColorTableSGI');
  glGetColorTableSGI := LoadExtensionFunction('glGetColorTableSGI');
  glGetColorTableParameterfvSGI := LoadExtensionFunction('glGetColorTableParameterfvSGI');
  glGetColorTableParameterivSGI := LoadExtensionFunction('glGetColorTableParameterivSGI');

   { GL_SGIX_pixel_texture }
  glPixelTexGenSGIX := LoadExtensionFunction('glPixelTexGenSGIX');

   { GL_SGIS_pixel_texture }
  glPixelTexGenParameteriSGIS := LoadExtensionFunction('glPixelTexGenParameteriSGIS');
  glPixelTexGenParameterivSGIS := LoadExtensionFunction('glPixelTexGenParameterivSGIS');
  glPixelTexGenParameterfSGIS := LoadExtensionFunction('glPixelTexGenParameterfSGIS');
  glPixelTexGenParameterfvSGIS := LoadExtensionFunction('glPixelTexGenParameterfvSGIS');
  glGetPixelTexGenParameterivSGIS := LoadExtensionFunction('glGetPixelTexGenParameterivSGIS');
  glGetPixelTexGenParameterfvSGIS := LoadExtensionFunction('glGetPixelTexGenParameterfvSGIS');

   { GL_SGIS_texture4D }
  glTexImage4DSGIS := LoadExtensionFunction('glTexImage4DSGIS');
  glTexSubImage4DSGIS := LoadExtensionFunction('glTexSubImage4DSGIS');

   { GL_SGIS_detail_texture }
  glDetailTexFuncSGIS := LoadExtensionFunction('glDetailTexFuncSGIS');
  glGetDetailTexFuncSGIS := LoadExtensionFunction('glGetDetailTexFuncSGIS');

   { GL_SGIS_sharpen_texture }
  glSharpenTexFuncSGIS := LoadExtensionFunction('glSharpenTexFuncSGIS');
  glGetSharpenTexFuncSGIS := LoadExtensionFunction('glGetSharpenTexFuncSGIS');

   { GL_SGIS_multisample }
  glSampleMaskSGIS := LoadExtensionFunction('glSampleMaskSGIS');
  glSamplePatternSGIS := LoadExtensionFunction('glSamplePatternSGIS');

   { GL_EXT_blend_minmax }
  glBlendEquationEXT := LoadExtensionFunction('glBlendEquationEXT');

   { GL_SGIX_sprite }
  glSpriteParameterfSGIX := LoadExtensionFunction('glSpriteParameterfSGIX');
  glSpriteParameterfvSGIX := LoadExtensionFunction('glSpriteParameterfvSGIX');
  glSpriteParameteriSGIX := LoadExtensionFunction('glSpriteParameteriSGIX');
  glSpriteParameterivSGIX := LoadExtensionFunction('glSpriteParameterivSGIX');

   { GL_EXT_point_parameters }
  glPointParameterfSGIS := LoadExtensionFunction('glPointParameterfSGIS');
  glPointParameterfvSGIS := LoadExtensionFunction('glPointParameterfvSGIS');

   { GL_SGIX_instruments }
  glGetInstrumentsSGIX := LoadExtensionFunction('glGetInstrumentsSGIX');
  glInstrumentsBufferSGIX := LoadExtensionFunction('glInstrumentsBufferSGIX');
  glPollInstrumentsSGIX := LoadExtensionFunction('glPollInstrumentsSGIX');
  glReadInstrumentsSGIX := LoadExtensionFunction('glReadInstrumentsSGIX');
  glStartInstrumentsSGIX := LoadExtensionFunction('glStartInstrumentsSGIX');
  glStopInstrumentsSGIX := LoadExtensionFunction('glStopInstrumentsSGIX');

   { GL_SGIX_framezoom }
  glFrameZoomSGIX := LoadExtensionFunction('glFrameZoomSGIX');

   { GL_SGIX_tag_sample_buffer }
  glTagSampleBufferSGIX := LoadExtensionFunction('glTagSampleBufferSGIX');

   { GL_SGIX_polynomial_ffd }
  glDeformationMap3dSGIX := LoadExtensionFunction('glDeformationMap3dSGIX');
  glDeformationMap3fSGIX := LoadExtensionFunction('glDeformationMap3fSGIX');
  glDeformSGIX := LoadExtensionFunction('glDeformSGIX');
  glLoadIdentityDeformationMapSGIX := LoadExtensionFunction('glLoadIdentityDeformationMapSGIX');

   { GL_SGIX_reference_plane }
  glReferencePlaneSGIX := LoadExtensionFunction('glReferencePlaneSGIX');

   { GL_SGIX_flush_raster }
  glFlushRasterSGIX := LoadExtensionFunction('glFlushRasterSGIX');

   { GL_SGIS_fog_function }
  glFogFuncSGIS := LoadExtensionFunction('glFogFuncSGIS');
  glGetFogFuncSGIS := LoadExtensionFunction('glGetFogFuncSGIS');

   { GL_HP_image_transform }
  glImageTransformParameteriHP := LoadExtensionFunction('glImageTransformParameteriHP');
  glImageTransformParameterfHP := LoadExtensionFunction('glImageTransformParameterfHP');
  glImageTransformParameterivHP := LoadExtensionFunction('glImageTransformParameterivHP');
  glImageTransformParameterfvHP := LoadExtensionFunction('glImageTransformParameterfvHP');
  glGetImageTransformParameterivHP := LoadExtensionFunction('glGetImageTransformParameterivHP');
  glGetImageTransformParameterfvHP := LoadExtensionFunction('glGetImageTransformParameterfvHP');

   { GL_EXT_color_subtable }
  glCopyColorSubTableEXT := LoadExtensionFunction('glCopyColorSubTableEXT');

   { GL_PGI_misc_hints }
  glHintPGI := LoadExtensionFunction('glHintPGI');

   { GL_EXT_paletted_texture }
  glGetColorTableParameterivEXT := LoadExtensionFunction('glGetColorTableParameterivEXT');
  glGetColorTableParameterfvEXT := LoadExtensionFunction('glGetColorTableParameterfvEXT');

   { GL_SGIX_list_priority }
  glGetListParameterfvSGIX := LoadExtensionFunction('glGetListParameterfvSGIX');
  glGetListParameterivSGIX := LoadExtensionFunction('glGetListParameterivSGIX');
  glListParameterfSGIX := LoadExtensionFunction('glListParameterfSGIX');
  glListParameterfvSGIX := LoadExtensionFunction('glListParameterfvSGIX');
  glListParameteriSGIX := LoadExtensionFunction('glListParameteriSGIX');
  glListParameterivSGIX := LoadExtensionFunction('glListParameterivSGIX');

   { GL_SGIX_fragment_lighting }
  glFragmentColorMaterialSGIX := LoadExtensionFunction('glFragmentColorMaterialSGIX');
  glFragmentLightfSGIX := LoadExtensionFunction('glFragmentLightfSGIX');
  glFragmentLightfvSGIX := LoadExtensionFunction('glFragmentLightfvSGIX');
  glFragmentLightiSGIX := LoadExtensionFunction('glFragmentLightiSGIX');
  glFragmentLightivSGIX := LoadExtensionFunction('glFragmentLightivSGIX');
  glFragmentLightModelfSGIX := LoadExtensionFunction('glFragmentLightModelfSGIX');
  glFragmentLightModelfvSGIX := LoadExtensionFunction('glFragmentLightModelfvSGIX');
  glFragmentLightModeliSGIX := LoadExtensionFunction('glFragmentLightModeliSGIX');
  glFragmentLightModelivSGIX := LoadExtensionFunction('glFragmentLightModelivSGIX');
  glFragmentMaterialfSGIX := LoadExtensionFunction('glFragmentMaterialfSGIX');
  glFragmentMaterialfvSGIX := LoadExtensionFunction('glFragmentMaterialfvSGIX');
  glFragmentMaterialiSGIX := LoadExtensionFunction('glFragmentMaterialiSGIX');
  glFragmentMaterialivSGIX := LoadExtensionFunction('glFragmentMaterialivSGIX');
  glGetFragmentLightfvSGIX := LoadExtensionFunction('glGetFragmentLightfvSGIX');
  glGetFragmentLightivSGIX := LoadExtensionFunction('glGetFragmentLightivSGIX');
  glGetFragmentMaterialfvSGIX := LoadExtensionFunction('glGetFragmentMaterialfvSGIX');
  glGetFragmentMaterialivSGIX := LoadExtensionFunction('glGetFragmentMaterialivSGIX');
  glLightEnviSGIX := LoadExtensionFunction('glLightEnviSGIX');

   { GL_EXT_draw_range_elements }
  glDrawRangeElementsEXT := LoadExtensionFunction('glDrawRangeElementsEXT');

   { GL_EXT_light_texture }
  glApplyTextureEXT := LoadExtensionFunction('glApplyTextureEXT');
  glTextureLightEXT := LoadExtensionFunction('glTextureLightEXT');
  glTextureMaterialEXT := LoadExtensionFunction('glTextureMaterialEXT');

   { GL_SGIX_async }
  glAsyncMarkerSGIX := LoadExtensionFunction('glAsyncMarkerSGIX');
  glFinishAsyncSGIX := LoadExtensionFunction('glFinishAsyncSGIX');
  glPollAsyncSGIX := LoadExtensionFunction('glPollAsyncSGIX');
  glGenAsyncMarkersSGIX := LoadExtensionFunction('glGenAsyncMarkersSGIX');
  glDeleteAsyncMarkersSGIX := LoadExtensionFunction('glDeleteAsyncMarkersSGIX');
  glIsAsyncMarkerSGIX := LoadExtensionFunction('glIsAsyncMarkerSGIX');

   { GL_INTEL_parallel_arrays }
  glVertexPointervINTEL := LoadExtensionFunction('glVertexPointervINTEL');
  glNormalPointervINTEL := LoadExtensionFunction('glNormalPointervINTEL');
  glColorPointervINTEL := LoadExtensionFunction('glColorPointervINTEL');
  glTexCoordPointervINTEL := LoadExtensionFunction('glTexCoordPointervINTEL');

   { GL_EXT_pixel_transform }
  glPixelTransformParameteriEXT := LoadExtensionFunction('glPixelTransformParameteriEXT');
  glPixelTransformParameterfEXT := LoadExtensionFunction('glPixelTransformParameterfEXT');
  glPixelTransformParameterivEXT := LoadExtensionFunction('glPixelTransformParameterivEXT');
  glPixelTransformParameterfvEXT := LoadExtensionFunction('glPixelTransformParameterfvEXT');

   { GL_EXT_secondary_color }
  glSecondaryColor3bEXT := LoadExtensionFunction('glSecondaryColor3bEXT');
  glSecondaryColor3bvEXT := LoadExtensionFunction('glSecondaryColor3bvEXT');
  glSecondaryColor3dEXT := LoadExtensionFunction('glSecondaryColor3dEXT');
  glSecondaryColor3dvEXT := LoadExtensionFunction('glSecondaryColor3dvEXT');
  glSecondaryColor3fEXT := LoadExtensionFunction('glSecondaryColor3fEXT');
  glSecondaryColor3fvEXT := LoadExtensionFunction('glSecondaryColor3fvEXT');
  glSecondaryColor3iEXT := LoadExtensionFunction('glSecondaryColor3iEXT');
  glSecondaryColor3ivEXT := LoadExtensionFunction('glSecondaryColor3ivEXT');
  glSecondaryColor3sEXT := LoadExtensionFunction('glSecondaryColor3sEXT');
  glSecondaryColor3svEXT := LoadExtensionFunction('glSecondaryColor3svEXT');
  glSecondaryColor3ubEXT := LoadExtensionFunction('glSecondaryColor3ubEXT');
  glSecondaryColor3ubvEXT := LoadExtensionFunction('glSecondaryColor3ubvEXT');
  glSecondaryColor3uiEXT := LoadExtensionFunction('glSecondaryColor3uiEXT');
  glSecondaryColor3uivEXT := LoadExtensionFunction('glSecondaryColor3uivEXT');
  glSecondaryColor3usEXT := LoadExtensionFunction('glSecondaryColor3usEXT');
  glSecondaryColor3usvEXT := LoadExtensionFunction('glSecondaryColor3usvEXT');
  glSecondaryColorPointerEXT := LoadExtensionFunction('glSecondaryColorPointerEXT');

   { GL_EXT_texture_perturb_normal }
  glTextureNormalEXT := LoadExtensionFunction('glTextureNormalEXT');

   { GL_EXT_multi_draw_arrays }
  glMultiDrawArraysEXT := LoadExtensionFunction('glMultiDrawArraysEXT');
  glMultiDrawElementsEXT := LoadExtensionFunction('glMultiDrawElementsEXT');

   { GL_EXT_fog_coord }
  glFogCoordfEXT := LoadExtensionFunction('glFogCoordfEXT');
  glFogCoordfvEXT := LoadExtensionFunction('glFogCoordfvEXT');
  glFogCoorddEXT := LoadExtensionFunction('glFogCoorddEXT');
  glFogCoorddvEXT := LoadExtensionFunction('glFogCoorddvEXT');
  glFogCoordPointerEXT := LoadExtensionFunction('glFogCoordPointerEXT');

   { GL_EXT_coordinate_frame }
  glTangent3bEXT := LoadExtensionFunction('glTangent3bEXT');
  glTangent3bvEXT := LoadExtensionFunction('glTangent3bvEXT');
  glTangent3dEXT := LoadExtensionFunction('glTangent3dEXT');
  glTangent3dvEXT := LoadExtensionFunction('glTangent3dvEXT');
  glTangent3fEXT := LoadExtensionFunction('glTangent3fEXT');
  glTangent3fvEXT := LoadExtensionFunction('glTangent3fvEXT');
  glTangent3iEXT := LoadExtensionFunction('glTangent3iEXT');
  glTangent3ivEXT := LoadExtensionFunction('glTangent3ivEXT');
  glTangent3sEXT := LoadExtensionFunction('glTangent3sEXT');
  glTangent3svEXT := LoadExtensionFunction('glTangent3svEXT');
  glBinormal3bEXT := LoadExtensionFunction('glBinormal3bEXT');
  glBinormal3bvEXT := LoadExtensionFunction('glBinormal3bvEXT');
  glBinormal3dEXT := LoadExtensionFunction('glBinormal3dEXT');
  glBinormal3dvEXT := LoadExtensionFunction('glBinormal3dvEXT');
  glBinormal3fEXT := LoadExtensionFunction('glBinormal3fEXT');
  glBinormal3fvEXT := LoadExtensionFunction('glBinormal3fvEXT');
  glBinormal3iEXT := LoadExtensionFunction('glBinormal3iEXT');
  glBinormal3ivEXT := LoadExtensionFunction('glBinormal3ivEXT');
  glBinormal3sEXT := LoadExtensionFunction('glBinormal3sEXT');
  glBinormal3svEXT := LoadExtensionFunction('glBinormal3svEXT');
  glTangentPointerEXT := LoadExtensionFunction('glTangentPointerEXT');
  glBinormalPointerEXT := LoadExtensionFunction('glBinormalPointerEXT');

   { GL_SUNX_constant_data }
  glFinishTextureSUNX := LoadExtensionFunction('glFinishTextureSUNX');

   { GL_SUN_global_alpha }
  glGlobalAlphaFactorbSUN := LoadExtensionFunction('glGlobalAlphaFactorbSUN');
  glGlobalAlphaFactorsSUN := LoadExtensionFunction('glGlobalAlphaFactorsSUN');
  glGlobalAlphaFactoriSUN := LoadExtensionFunction('glGlobalAlphaFactoriSUN');
  glGlobalAlphaFactorfSUN := LoadExtensionFunction('glGlobalAlphaFactorfSUN');
  glGlobalAlphaFactordSUN := LoadExtensionFunction('glGlobalAlphaFactordSUN');
  glGlobalAlphaFactorubSUN := LoadExtensionFunction('glGlobalAlphaFactorubSUN');
  glGlobalAlphaFactorusSUN := LoadExtensionFunction('glGlobalAlphaFactorusSUN');
  glGlobalAlphaFactoruiSUN := LoadExtensionFunction('glGlobalAlphaFactoruiSUN');

   { GL_SUN_triangle_list }
  glReplacementCodeuiSUN := LoadExtensionFunction('glReplacementCodeuiSUN');
  glReplacementCodeusSUN := LoadExtensionFunction('glReplacementCodeusSUN');
  glReplacementCodeubSUN := LoadExtensionFunction('glReplacementCodeubSUN');
  glReplacementCodeuivSUN := LoadExtensionFunction('glReplacementCodeuivSUN');
  glReplacementCodeusvSUN := LoadExtensionFunction('glReplacementCodeusvSUN');
  glReplacementCodeubvSUN := LoadExtensionFunction('glReplacementCodeubvSUN');
  glReplacementCodePointerSUN := LoadExtensionFunction('glReplacementCodePointerSUN');

   { GL_SUN_vertex }
  glColor4ubVertex2fSUN := LoadExtensionFunction('glColor4ubVertex2fSUN');
  glColor4ubVertex2fvSUN := LoadExtensionFunction('glColor4ubVertex2fvSUN');
  glColor4ubVertex3fSUN := LoadExtensionFunction('glColor4ubVertex3fSUN');
  glColor4ubVertex3fvSUN := LoadExtensionFunction('glColor4ubVertex3fvSUN');
  glColor3fVertex3fSUN := LoadExtensionFunction('glColor3fVertex3fSUN');
  glColor3fVertex3fvSUN := LoadExtensionFunction('glColor3fVertex3fvSUN');
  glNormal3fVertex3fSUN := LoadExtensionFunction('glNormal3fVertex3fSUN');
  glNormal3fVertex3fvSUN := LoadExtensionFunction('glNormal3fVertex3fvSUN');
  glColor4fNormal3fVertex3fSUN := LoadExtensionFunction('glColor4fNormal3fVertex3fSUN');
  glColor4fNormal3fVertex3fvSUN := LoadExtensionFunction('glColor4fNormal3fVertex3fvSUN');
  glTexCoord2fVertex3fSUN := LoadExtensionFunction('glTexCoord2fVertex3fSUN');
  glTexCoord2fVertex3fvSUN := LoadExtensionFunction('glTexCoord2fVertex3fvSUN');
  glTexCoord4fVertex4fSUN := LoadExtensionFunction('glTexCoord4fVertex4fSUN');
  glTexCoord4fVertex4fvSUN := LoadExtensionFunction('glTexCoord4fVertex4fvSUN');
  glTexCoord2fColor4ubVertex3fSUN := LoadExtensionFunction('glTexCoord2fColor4ubVertex3fSUN');
  glTexCoord2fColor4ubVertex3fvSUN := LoadExtensionFunction('glTexCoord2fColor4ubVertex3fvSUN');
  glTexCoord2fColor3fVertex3fSUN := LoadExtensionFunction('glTexCoord2fColor3fVertex3fSUN');
  glTexCoord2fColor3fVertex3fvSUN := LoadExtensionFunction('glTexCoord2fColor3fVertex3fvSUN');
  glTexCoord2fNormal3fVertex3fSUN := LoadExtensionFunction('glTexCoord2fNormal3fVertex3fSUN');
  glTexCoord2fNormal3fVertex3fvSUN := LoadExtensionFunction('glTexCoord2fNormal3fVertex3fvSUN');
  glTexCoord2fColor4fNormal3fVertex3fSUN := LoadExtensionFunction('glTexCoord2fColor4fNormal3fVertex3fSUN');
  glTexCoord2fColor4fNormal3fVertex3fvSUN := LoadExtensionFunction('glTexCoord2fColor4fNormal3fVertex3fvSUN');
  glTexCoord4fColor4fNormal3fVertex4fSUN := LoadExtensionFunction('glTexCoord4fColor4fNormal3fVertex4fSUN');
  glTexCoord4fColor4fNormal3fVertex4fvSUN := LoadExtensionFunction('glTexCoord4fColor4fNormal3fVertex4fvSUN');
  glReplacementCodeuiVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiVertex3fSUN');
  glReplacementCodeuiVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiVertex3fvSUN');
  glReplacementCodeuiColor4ubVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiColor4ubVertex3fSUN');
  glReplacementCodeuiColor4ubVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiColor4ubVertex3fvSUN');
  glReplacementCodeuiColor3fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiColor3fVertex3fSUN');
  glReplacementCodeuiColor3fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiColor3fVertex3fvSUN');
  glReplacementCodeuiNormal3fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiNormal3fVertex3fSUN');
  glReplacementCodeuiNormal3fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiNormal3fVertex3fvSUN');
  glReplacementCodeuiColor4fNormal3fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiColor4fNormal3fVertex3fSUN');
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiColor4fNormal3fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fVertex3fSUN');
  glReplacementCodeuiTexCoord2fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN');
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := LoadExtensionFunction('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN');

   { GL_EXT_blend_func_separate }
  glBlendFuncSeparateEXT := LoadExtensionFunction('glBlendFuncSeparateEXT');

   { GL_EXT_vertex_weighting }
  glVertexWeightfEXT := LoadExtensionFunction('glVertexWeightfEXT');
  glVertexWeightfvEXT := LoadExtensionFunction('glVertexWeightfvEXT');
  glVertexWeightPointerEXT := LoadExtensionFunction('glVertexWeightPointerEXT');

   { GL_NV_vertex_array_range }
  glFlushVertexArrayRangeNV := LoadExtensionFunction('glFlushVertexArrayRangeNV');
  glVertexArrayRangeNV := LoadExtensionFunction('glVertexArrayRangeNV');
  wglAllocateMemoryNV := LoadExtensionFunction('wglAllocateMemoryNV');
  wglFreeMemoryNV := LoadExtensionFunction('wglFreeMemoryNV');

   { GL_NV_register_combiners }
  glCombinerParameterfvNV := LoadExtensionFunction('glCombinerParameterfvNV');
  glCombinerParameterfNV := LoadExtensionFunction('glCombinerParameterfNV');
  glCombinerParameterivNV := LoadExtensionFunction('glCombinerParameterivNV');
  glCombinerParameteriNV := LoadExtensionFunction('glCombinerParameteriNV');
  glCombinerInputNV := LoadExtensionFunction('glCombinerInputNV');
  glCombinerOutputNV := LoadExtensionFunction('glCombinerOutputNV');
  glFinalCombinerInputNV := LoadExtensionFunction('glFinalCombinerInputNV');
  glGetCombinerInputParameterfvNV := LoadExtensionFunction('glGetCombinerInputParameterfvNV');
  glGetCombinerInputParameterivNV := LoadExtensionFunction('glGetCombinerInputParameterivNV');
  glGetCombinerOutputParameterfvNV := LoadExtensionFunction('glGetCombinerOutputParameterfvNV');
  glGetCombinerOutputParameterivNV := LoadExtensionFunction('glGetCombinerOutputParameterivNV');
  glGetFinalCombinerInputParameterfvNV := LoadExtensionFunction('glGetFinalCombinerInputParameterfvNV');
  glGetFinalCombinerInputParameterivNV := LoadExtensionFunction('glGetFinalCombinerInputParameterivNV');

   { GL_MESA_resize_buffers }
  glResizeBuffersMESA := LoadExtensionFunction('glResizeBuffersMESA');

   { GL_MESA_window_pos }
  glWindowPos2dMESA := LoadExtensionFunction('glWindowPos2dMESA');
  glWindowPos2dvMESA := LoadExtensionFunction('glWindowPos2dvMESA');
  glWindowPos2fMESA := LoadExtensionFunction('glWindowPos2fMESA');
  glWindowPos2fvMESA := LoadExtensionFunction('glWindowPos2fvMESA');
  glWindowPos2iMESA := LoadExtensionFunction('glWindowPos2iMESA');
  glWindowPos2ivMESA := LoadExtensionFunction('glWindowPos2ivMESA');
  glWindowPos2sMESA := LoadExtensionFunction('glWindowPos2sMESA');
  glWindowPos2svMESA := LoadExtensionFunction('glWindowPos2svMESA');
  glWindowPos3dMESA := LoadExtensionFunction('glWindowPos3dMESA');
  glWindowPos3dvMESA := LoadExtensionFunction('glWindowPos3dvMESA');
  glWindowPos3fMESA := LoadExtensionFunction('glWindowPos3fMESA');
  glWindowPos3fvMESA := LoadExtensionFunction('glWindowPos3fvMESA');
  glWindowPos3iMESA := LoadExtensionFunction('glWindowPos3iMESA');
  glWindowPos3ivMESA := LoadExtensionFunction('glWindowPos3ivMESA');
  glWindowPos3sMESA := LoadExtensionFunction('glWindowPos3sMESA');
  glWindowPos3svMESA := LoadExtensionFunction('glWindowPos3svMESA');
  glWindowPos4dMESA := LoadExtensionFunction('glWindowPos4dMESA');
  glWindowPos4dvMESA := LoadExtensionFunction('glWindowPos4dvMESA');
  glWindowPos4fMESA := LoadExtensionFunction('glWindowPos4fMESA');
  glWindowPos4fvMESA := LoadExtensionFunction('glWindowPos4fvMESA');
  glWindowPos4iMESA := LoadExtensionFunction('glWindowPos4iMESA');
  glWindowPos4ivMESA := LoadExtensionFunction('glWindowPos4ivMESA');
  glWindowPos4sMESA := LoadExtensionFunction('glWindowPos4sMESA');
  glWindowPos4svMESA := LoadExtensionFunction('glWindowPos4svMESA');

   { GL_IBM_multimode_draw_arrays }
  glMultiModeDrawArraysIBM := LoadExtensionFunction('glMultiModeDrawArraysIBM');
  glMultiModeDrawElementsIBM := LoadExtensionFunction('glMultiModeDrawElementsIBM');

   { GL_IBM_vertex_array_lists }
  glColorPointerListIBM := LoadExtensionFunction('glColorPointerListIBM');
  glSecondaryColorPointerListIBM := LoadExtensionFunction('glSecondaryColorPointerListIBM');
  glEdgeFlagPointerListIBM := LoadExtensionFunction('glEdgeFlagPointerListIBM');
  glFogCoordPointerListIBM := LoadExtensionFunction('glFogCoordPointerListIBM');
  glIndexPointerListIBM := LoadExtensionFunction('glIndexPointerListIBM');
  glNormalPointerListIBM := LoadExtensionFunction('glNormalPointerListIBM');
  glTexCoordPointerListIBM := LoadExtensionFunction('glTexCoordPointerListIBM');
  glVertexPointerListIBM := LoadExtensionFunction('glVertexPointerListIBM');

   { GL_3DFX_tbuffer }
  glTbufferMask3DFX := LoadExtensionFunction('glTbufferMask3DFX');

   { GL_EXT_multisample }
  glSampleMaskEXT := LoadExtensionFunction('glSampleMaskEXT');
  glSamplePatternEXT := LoadExtensionFunction('glSamplePatternEXT');

   { GL_SGIS_texture_color_mask }
  glTextureColorMaskSGIS := LoadExtensionFunction('glTextureColorMaskSGIS');

   { GL_SGIX_igloo_interface }
  glIglooInterfaceSGIX := LoadExtensionFunction('glIglooInterfaceSGIX');

   { GLU extensions }
  gluNurbsCallbackDataEXT := LoadExtensionFunction('gluNurbsCallbackDataEXT');
  gluNewNurbsTessellatorEXT := LoadExtensionFunction('gluNewNurbsTessellatorEXT');
  gluDeleteNurbsTessellatorEXT := LoadExtensionFunction('gluDeleteNurbsTessellatorEXT');

   { GL_NV_vertex_program }
  glAreProgramsResidentNV := LoadExtensionFunction('glAreProgramsResidentNV');
  glBindProgramNV := LoadExtensionFunction('glBindProgramNV');
  glDeleteProgramsNV := LoadExtensionFunction('glDeleteProgramsNV');
  glExecuteProgramNV := LoadExtensionFunction('glExecuteProgramNV');
  glGenProgramsNV := LoadExtensionFunction('glGenProgramsNV');
  glGetProgramParameterdvNV := LoadExtensionFunction('glGetProgramParameterdvNV');
  glGetProgramParameterfvNV := LoadExtensionFunction('glGetProgramParameterfvNV');
  glGetProgramivNV := LoadExtensionFunction('glGetProgramivNV');
  glGetProgramStringNV := LoadExtensionFunction('glGetProgramStringNV');
  glGetTrackMatrixivNV := LoadExtensionFunction('glGetTrackMatrixivNV');
  glGetVertexAttribdvNV := LoadExtensionFunction('glGetVertexAttribdvNV');
  glGetVertexAttribfvNV := LoadExtensionFunction('glGetVertexAttribfvNV');
  glGetVertexAttribivNV := LoadExtensionFunction('glGetVertexAttribivNV');
  glGetVertexAttribPointervNV := LoadExtensionFunction('glGetVertexAttribPointervNV');
  glIsProgramNV := LoadExtensionFunction('glIsProgramNV');
  glLoadProgramNV := LoadExtensionFunction('glLoadProgramNV');
  glProgramParameter4dNV := LoadExtensionFunction('glProgramParameter4dNV');
  glProgramParameter4dvNV := LoadExtensionFunction('glProgramParameter4dvNV');
  glProgramParameter4fNV := LoadExtensionFunction('glProgramParameter4fNV');
  glProgramParameter4fvNV := LoadExtensionFunction('glProgramParameter4fvNV');
  glProgramParameters4dvNV := LoadExtensionFunction('glProgramParameters4dvNV');
  glProgramParameters4fvNV := LoadExtensionFunction('glProgramParameters4fvNV');
  glRequestResidentProgramsNV := LoadExtensionFunction('glRequestResidentProgramsNV');
  glTrackMatrixNV := LoadExtensionFunction('glTrackMatrixNV');
  glVertexAttribPointerNV := LoadExtensionFunction('glVertexAttribPointerNV');
  glVertexAttrib1dNV := LoadExtensionFunction('glVertexAttrib1dNV');
  glVertexAttrib1dvNV := LoadExtensionFunction('glVertexAttrib1dvNV');
  glVertexAttrib1fNV := LoadExtensionFunction('glVertexAttrib1fNV');
  glVertexAttrib1fvNV := LoadExtensionFunction('glVertexAttrib1fvNV');
  glVertexAttrib1sNV := LoadExtensionFunction('glVertexAttrib1sNV');
  glVertexAttrib1svNV := LoadExtensionFunction('glVertexAttrib1svNV');
  glVertexAttrib2dNV := LoadExtensionFunction('glVertexAttrib2dNV');
  glVertexAttrib2dvNV := LoadExtensionFunction('glVertexAttrib2dvNV');
  glVertexAttrib2fNV := LoadExtensionFunction('glVertexAttrib2fNV');
  glVertexAttrib2fvNV := LoadExtensionFunction('glVertexAttrib2fvNV');
  glVertexAttrib2sNV := LoadExtensionFunction('glVertexAttrib2sNV');
  glVertexAttrib2svNV := LoadExtensionFunction('glVertexAttrib2svNV');
  glVertexAttrib3dNV := LoadExtensionFunction('glVertexAttrib3dNV');
  glVertexAttrib3dvNV := LoadExtensionFunction('glVertexAttrib3dvNV');
  glVertexAttrib3fNV := LoadExtensionFunction('glVertexAttrib3fNV');
  glVertexAttrib3fvNV := LoadExtensionFunction('glVertexAttrib3fvNV');
  glVertexAttrib3sNV := LoadExtensionFunction('glVertexAttrib3sNV');
  glVertexAttrib3svNV := LoadExtensionFunction('glVertexAttrib3svNV');
  glVertexAttrib4dNV := LoadExtensionFunction('glVertexAttrib4dNV');
  glVertexAttrib4dvNV := LoadExtensionFunction('glVertexAttrib4dvNV');
  glVertexAttrib4fNV := LoadExtensionFunction('glVertexAttrib4fNV');
  glVertexAttrib4fvNV := LoadExtensionFunction('glVertexAttrib4fvNV');
  glVertexAttrib4sNV := LoadExtensionFunction('glVertexAttrib4sNV');
  glVertexAttrib4svNV := LoadExtensionFunction('glVertexAttrib4svNV');
  glVertexAttrib4ubvNV := LoadExtensionFunction('glVertexAttrib4ubvNV');
  glVertexAttribs1dvNV := LoadExtensionFunction('glVertexAttribs1dvNV');
  glVertexAttribs1fvNV := LoadExtensionFunction('glVertexAttribs1fvNV');
  glVertexAttribs1svNV := LoadExtensionFunction('glVertexAttribs1svNV');
  glVertexAttribs2dvNV := LoadExtensionFunction('glVertexAttribs2dvNV');
  glVertexAttribs2fvNV := LoadExtensionFunction('glVertexAttribs2fvNV');
  glVertexAttribs2svNV := LoadExtensionFunction('glVertexAttribs2svNV');
  glVertexAttribs3dvNV := LoadExtensionFunction('glVertexAttribs3dvNV');
  glVertexAttribs3fvNV := LoadExtensionFunction('glVertexAttribs3fvNV');
  glVertexAttribs3svNV := LoadExtensionFunction('glVertexAttribs3svNV');
  glVertexAttribs4dvNV := LoadExtensionFunction('glVertexAttribs4dvNV');
  glVertexAttribs4fvNV := LoadExtensionFunction('glVertexAttribs4fvNV');
  glVertexAttribs4svNV := LoadExtensionFunction('glVertexAttribs4svNV');
  glVertexAttribs4ubvNV := LoadExtensionFunction('glVertexAttribs4ubvNV');

  {$ifdef UNIX}
  glXGetVideoSyncSGI := LoadExtensionFunction('glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := LoadExtensionFunction('glXWaitVideoSyncSGI');
  glXFreeContextEXT := LoadExtensionFunction('glXFreeContextEXT');
  glXGetContextIDEXT := LoadExtensionFunction('glXGetContextIDEXT');
  glXGetCurrentDisplayEXT := LoadExtensionFunction('glXGetCurrentDisplayEXT');
  glXImportContextEXT := LoadExtensionFunction('glXImportContextEXT');
  glXQueryContextInfoEXT := LoadExtensionFunction('glXQueryContextInfoEXT');
  glXCopySubBufferMESA := LoadExtensionFunction('glXCopySubBufferMESA');
  glXCreateGLXPixmapMESA := LoadExtensionFunction('glXCreateGLXPixmapMESA');
  glXReleaseBuffersMESA := LoadExtensionFunction('glXReleaseBuffersMESA');
  glXSet3DfxModeMESA := LoadExtensionFunction('glXSet3DfxModeMESA');
  {$endif}

 finally
  { restore GLLibrary.SymbolErrorBehaviour }
  GLLibrary.SymbolErrorBehaviour := Old_GL_SymbolErrorBehaviour;
 end;
end;

{---------------------------------------------------------------------------------------------------------------------- }

procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);

{ Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text" }
{ at least however "Major.Minor". }

var
  Separator: Integer;

begin
  try
    { There must be at least one dot to separate major and minor version number. }
    Separator := Pos('.', Buffer);
    { At least one number must be before and one after the dot. }
    if (Separator > 1) and (Separator < Length(Buffer)) and (Buffer[Separator - 1] in ['0'..'9']) and
      (Buffer[Separator + 1] in ['0'..'9']) then
    begin
      { OK, it's a valid version string. Now remove unnecessary parts. }
      Dec(Separator);
      { Find last non-numeric character before version number. }
      while (Separator > 0) and (Buffer[Separator] in ['0'..'9']) do
        Dec(Separator);
      { Delete leading characters which do not belong to the version string. }
      Delete(Buffer, 1, Separator);
      Separator := Pos('.', Buffer) + 1;
      { Find first non-numeric character after version number }
      while (Separator <= Length(Buffer)) and (Buffer[Separator] in ['0'..'9']) do
        Inc(Separator);
      { delete trailing characters not belonging to the version string }
      Delete(Buffer, Separator, 255);
      { Now translate the numbers. }
      Separator := Pos('.', Buffer); { This is necessary because the buffer length might have changed. }
      Max := StrToInt(Copy(Buffer, 1, Separator - 1));
      Min := StrToInt(Copy(Buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    Min := 0;
    Max := 0;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure ReadImplementationProperties;

var
  Buffer: string;
  MajorVersion,
  MinorVersion: Integer;

  {--------------- local function -------------------------------------------- }

   function CheckExtension(const Extension: string): Boolean;

   { Checks if the given Extension string is in Buffer. }

   var
     ExtPos: Integer;

   begin
     { First find the position of the extension string as substring in Buffer. }
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     { Now check that it isn't only a substring of another extension. }
     if Result then
       Result := ((ExtPos + Length(Extension) - 1) = Length(Buffer)) or
         not (Buffer[ExtPos + Length(Extension)] in ['_', 'A'..'Z', 'a'..'z']);
   end;

  {--------------- end local function ---------------------------------------- }

begin
  { determine version of implementation }
  { GL }
  Buffer := glGetString(GL_VERSION);
  TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion);
  GL_VERSION_1_0 := True;
  GL_VERSION_1_1 := False;
  GL_VERSION_1_2 := False;
  if MajorVersion > 0 then
  begin
    if MinorVersion > 0 then
    begin
      GL_VERSION_1_1 := True;
      if MinorVersion > 1 then
        GL_VERSION_1_2 := True;
    end;
  end;

  { GLU }
  GLU_VERSION_1_1 := False;
  GLU_VERSION_1_2 := False;
  GLU_VERSION_1_3 := False;
  { gluGetString is valid for version 1.1 or later }
  if Assigned(gluGetString) then
  begin
    Buffer := gluGetString(GLU_VERSION);
    TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion);
    GLU_VERSION_1_1 := True;
    if MinorVersion > 1 then
    begin
      GLU_VERSION_1_2 := True;
      if MinorVersion > 2 then
        GLU_VERSION_1_3 := True;
    end;
  end;

  { check supported extensions }
  { GL }
  Buffer := glGetString(GL_EXTENSIONS);

  GL_3DFX_multisample := CheckExtension('GL_3DFX_multisample');
  GL_3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
  GL_3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');

  GL_APPLE_specular_vector := CheckExtension('GL_APPLE_specular_vector');
  GL_APPLE_transform_hint := CheckExtension('GL_APPLE_transform_hint');

  GL_ARB_imaging := CheckExtension('GL_ARB_imaging');
  GL_ARB_multisample := CheckExtension('GL_ARB_multisample');
  GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture');
  GL_ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
  GL_ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
  GL_ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
  GL_ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');

  GL_EXT_422_pixels := CheckExtension('GL_EXT_422_pixels');
  GL_EXT_abgr := CheckExtension('GL_EXT_abgr');
  GL_EXT_bgra := CheckExtension('GL_EXT_bgra');
  GL_EXT_blend_color := CheckExtension('GL_EXT_blend_color');
  GL_EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
  GL_EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
  GL_EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
  GL_EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
  GL_EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
  GL_EXT_cmyka := CheckExtension('GL_EXT_cmyka');
  GL_EXT_color_subtable := CheckExtension('GL_EXT_color_subtable');
  GL_EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
  GL_EXT_convolution := CheckExtension('GL_EXT_convolution');
  GL_EXT_coordinate_frame := CheckExtension('GL_EXT_coordinate_frame');
  GL_EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
  GL_EXT_cull_vertex := CheckExtension('GL_EXT_cull_vertex');
  GL_EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
  GL_EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
  GL_EXT_histogram := CheckExtension('GL_EXT_histogram');
  GL_EXT_index_array_formats := CheckExtension('GL_EXT_index_array_formats');
  GL_EXT_index_func := CheckExtension('GL_EXT_index_func');
  GL_EXT_index_material := CheckExtension('GL_EXT_index_material');
  GL_EXT_index_texture := CheckExtension('GL_EXT_index_texture');
  GL_EXT_light_max_exponent := CheckExtension('GL_EXT_light_max_exponent');
  GL_EXT_light_texture := CheckExtension('GL_EXT_light_texture');
  GL_EXT_misc_attribute := CheckExtension('GL_EXT_misc_attribute');
  GL_EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
  GL_EXT_multisample := CheckExtension('GL_EXT_multisample');
  GL_EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
  GL_EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
  GL_EXT_pixel_transform := CheckExtension('GL_EXT_pixel_transform');
  GL_EXT_point_parameters := CheckExtension('GL_EXT_point_parameters');
  GL_EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
  GL_EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
  GL_EXT_scene_marker := CheckExtension('GL_EXT_scene_marker');
  GL_EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
  GL_EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
  GL_EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
  GL_EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
  GL_EXT_subtexture := CheckExtension('GL_EXT_subtexture');
  GL_EXT_texture_color_table := CheckExtension('GL_EXT_texture_color_table');
  GL_EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
  GL_EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
  GL_EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
  GL_EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
  GL_EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
  GL_EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic');
  GL_EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
  GL_EXT_texture_object := CheckExtension('GL_EXT_texture_object');
  GL_EXT_texture_perturb_normal := CheckExtension('GL_EXT_texture_perturb_normal');
  GL_EXT_texture3D := CheckExtension('GL_EXT_texture3D');
  GL_EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');
  GL_EXT_vertex_weighting := CheckExtension('GL_EXT_vertex_weighting');

  GL_FfdMaskSGIX := CheckExtension('GL_FfdMaskSGIX');
  GL_HP_convolution_border_modes := CheckExtension('GL_HP_convolution_border_modes');
  GL_HP_image_transform := CheckExtension('GL_HP_image_transform');
  GL_HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');
  GL_HP_texture_lighting := CheckExtension('GL_HP_texture_lighting');

  GL_IBM_cull_vertex := CheckExtension('GL_IBM_cull_vertex');
  GL_IBM_multimode_draw_arrays := CheckExtension('GL_IBM_multimode_draw_arrays');
  GL_IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');
  GL_IBM_vertex_array_lists := CheckExtension('GL_IBM_vertex_array_lists');

  GL_INGR_color_clamp := CheckExtension('GL_INGR_color_clamp');
  GL_INGR_interlace_read := CheckExtension('GL_INGR_interlace_read');

  GL_INTEL_parallel_arrays := CheckExtension('GL_INTEL_parallel_arrays');

  GL_KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');

  GL_MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');
  GL_MESA_window_pos := CheckExtension('GL_MESA_window_pos');

  GL_NV_blend_square := CheckExtension('GL_NV_blend_square');
  GL_NV_fog_distance := CheckExtension('GL_NV_fog_distance');
  GL_NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
  GL_NV_register_combiners := CheckExtension('GL_NV_register_combiners');
  GL_NV_texgen_emboss := CheckExtension('GL_NV_texgen_emboss');
  GL_NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
  GL_NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
  GL_NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
  GL_NV_vertex_program := CheckExtension('GL_NV_vertex_program');

  GL_PGI_misc_hints := CheckExtension('GL_PGI_misc_hints');
  GL_PGI_vertex_hints := CheckExtension('GL_PGI_vertex_hints');

  GL_REND_screen_coordinates := CheckExtension('GL_REND_screen_coordinates');

  GL_SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');
  GL_SGI_color_table := CheckExtension('GL_SGI_color_table');
  GL_SGI_depth_pass_instrument := CheckExtension('GL_SGI_depth_pass_instrument');

  GL_SGIS_detail_texture := CheckExtension('GL_SGIS_detail_texture');
  GL_SGIS_fog_function := CheckExtension('GL_SGIS_fog_function');
  GL_SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
  GL_SGIS_multisample := CheckExtension('GL_SGIS_multisample');
  GL_SGIS_multitexture := CheckExtension('GL_SGIS_multitexture');
  GL_SGIS_pixel_texture := CheckExtension('GL_SGIS_pixel_texture');
  GL_SGIS_point_line_texgen := CheckExtension('GL_SGIS_point_line_texgen');
  GL_SGIS_point_parameters := CheckExtension('GL_SGIS_point_parameters');
  GL_SGIS_sharpen_texture := CheckExtension('GL_SGIS_sharpen_texture');
  GL_SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
  GL_SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
  GL_SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
  GL_SGIS_texture_filter4 := CheckExtension('GL_SGIS_texture_filter4');
  GL_SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');
  GL_SGIS_texture_select := CheckExtension('GL_SGIS_texture_select');
  GL_SGIS_texture4D := CheckExtension('GL_SGIS_texture4D');

  GL_SGIX_async := CheckExtension('GL_SGIX_async');
  GL_SGIX_async_histogram := CheckExtension('GL_SGIX_async_histogram');
  GL_SGIX_async_pixel := CheckExtension('GL_SGIX_async_pixel');
  GL_SGIX_blend_alpha_minmax := CheckExtension('GL_SGIX_blend_alpha_minmax');
  GL_SGIX_calligraphic_fragment := CheckExtension('GL_SGIX_calligraphic_fragment');
  GL_SGIX_clipmap := CheckExtension('GL_SGIX_clipmap');
  GL_SGIX_convolution_accuracy := CheckExtension('GL_SGIX_convolution_accuracy');
  GL_SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
  GL_SGIX_flush_raster := CheckExtension('GL_SGIX_flush_raster');
  GL_SGIX_fog_offset := CheckExtension('GL_SGIX_fog_offset');
  GL_SGIX_fog_scale := CheckExtension('GL_SGIX_fog_scale');
  GL_SGIX_fragment_lighting := CheckExtension('GL_SGIX_fragment_lighting');
  GL_SGIX_framezoom := CheckExtension('GL_SGIX_framezoom');
  GL_SGIX_igloo_interface := CheckExtension('GL_SGIX_igloo_interface');
  GL_SGIX_instruments := CheckExtension('GL_SGIX_instruments');
  GL_SGIX_interlace := CheckExtension('GL_SGIX_interlace');
  GL_SGIX_ir_instrument1 := CheckExtension('GL_SGIX_ir_instrument1');
  GL_SGIX_list_priority := CheckExtension('GL_SGIX_list_priority');
  GL_SGIX_pixel_texture := CheckExtension('GL_SGIX_pixel_texture');
  GL_SGIX_pixel_tiles := CheckExtension('GL_SGIX_pixel_tiles');
  GL_SGIX_polynomial_ffd := CheckExtension('GL_SGIX_polynomial_ffd');
  GL_SGIX_reference_plane := CheckExtension('GL_SGIX_reference_plane');
  GL_SGIX_resample := CheckExtension('GL_SGIX_resample');
  GL_SGIX_shadow := CheckExtension('GL_SGIX_shadow');
  GL_SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');
  GL_SGIX_sprite := CheckExtension('GL_SGIX_sprite');
  GL_SGIX_subsample := CheckExtension('GL_SGIX_subsample');
  GL_SGIX_tag_sample_buffer := CheckExtension('GL_SGIX_tag_sample_buffer');
  GL_SGIX_texture_add_env := CheckExtension('GL_SGIX_texture_add_env');
  GL_SGIX_texture_lod_bias := CheckExtension('GL_SGIX_texture_lod_bias');
  GL_SGIX_texture_multi_buffer := CheckExtension('GL_SGIX_texture_multi_buffer');
  GL_SGIX_texture_scale_bias := CheckExtension('GL_SGIX_texture_scale_bias');
  GL_SGIX_vertex_preclip := CheckExtension('GL_SGIX_vertex_preclip');
  GL_SGIX_ycrcb := CheckExtension('GL_SGIX_ycrcb');
  GL_SGIX_ycrcba := CheckExtension('GL_SGIX_ycrcba');

  GL_SUN_convolution_border_modes := CheckExtension('GL_SUN_convolution_border_modes');
  GL_SUN_global_alpha := CheckExtension('GL_SUN_global_alpha');
  GL_SUN_triangle_list := CheckExtension('GL_SUN_triangle_list');
  GL_SUN_vertex := CheckExtension('GL_SUN_vertex');

  GL_SUNX_constant_data := CheckExtension('GL_SUNX_constant_data');

  GL_WIN_phong_shading := CheckExtension('GL_WIN_phong_shading');
  GL_WIN_specular_fog := CheckExtension('GL_WIN_specular_fog');
  GL_WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');

  WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');

  { GLU }
  Buffer := gluGetString(GLU_EXTENSIONS);
  GLU_EXT_TEXTURE := CheckExtension('GLU_EXT_TEXTURE');
  GLU_EXT_object_space_tess := CheckExtension('GLU_EXT_object_space_tess');
  GLU_EXT_nurbs_tessellator := CheckExtension('GLU_EXT_nurbs_tessellator');
end;

{---------------------------------------------------------------------------------------------------------------------- }

procedure CloseOpenGL;
begin
 FreeAndNil(GLULibrary);
 FreeAndNil(GLLibrary);
 ClearProcAddresses;
 ClearProcExtensions;
end;

{---------------------------------------------------------------------------------------------------------------------- }

procedure InitOpenGL;
begin
 if (GLLibrary = nil) or (GLULibrary = nil) then
  InitOpenGL(SDefaultGLLibName, SDefaultGLULibName);
end;

{---------------------------------------------------------------------------------------------------------------------- }

procedure InitOpenGL(const GLName, GLUName: string);
begin
 CloseOpenGL;

 {$ifdef UNIX} try {$endif}

  GLLibrary := TDynLib.Load(GLName);

 {$ifdef UNIX}
 except
  { Catch EDynLibError only to provide some useful error message for a
    known problem with nVidia graphics drivers combined with kernels 2.4.x }
  on E: EDynLibError do
  begin
   if Pos('cannot handle TLS data', E.Message) <> 0 then
    E.Message := E.Message +nl+
      nl+
      'Michalis note: if you use nVidia graphics card, then (quoting nVidia README) '+
      'you may workaround this problem by setting the environment variable '+
      'LD_ASSUME_KERNEL to something below "2.3.99" (for example: '+
      '`export LD_ASSUME_KERNEL=2.3.98`). Look at nVidia drivers README file, '+
      'nvidia-glx Debian package docs and search the web to get more information '+
      'about this problem.';
   raise;
  end;
 end;
 {$endif}

 GLULibrary := TDynLib.Load(GLUName);
 LoadProcAddresses;
end;

{ ----------------------------------------------------------------- }

function IsOpenGLInitialized: Boolean;
begin
 Result := GLLibrary <> nil;
end;

{ ------------------------------------------------------------- }

initialization
 { Kambi : In GLUT faq we can read explanation why Se8087CW below
   is needed : this is because Microsoft's OpenGL implementation
   can raise FPU exceptions doing floating point operations.
   Microsoft compilers, like Visual C++, by default ignore FPU
   exceptions so nothing needs to be done when you compile programs
   in Visual C++. But Borland's compilers (at least Delphi and
   C++Builder) and FPC don't mask FPU exceptions - by default, they convert
   them to normal C++/Pascal exceptions. So one should explicitly mask
   FPU exceptions before using any OpenGL routine under win32. }
 {$ifdef WIN32}
 Set8087CW( { $133F = } F8087_Projective_Infinity or
                        F8087_NearestOrEven_Round or
                        F8087_Extended_Precision or
                        F8087_Except );
 {$endif}

 { Kambi+, automatic initialization }
 InitOpenGL;
finalization
 CloseOpenGL;
 { We don't need to reset the FPU control word as the previous set call is
   process specific. }
end.
