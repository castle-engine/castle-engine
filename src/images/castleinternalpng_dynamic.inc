(*
  Libpng bindings. See [http://www.libpng.org/].
  Bases on FPC sources, modified by Kambi.

  Kambi modifications:

  @unorderedList(
    @item(
      If libpng is not installed on your system, there is no exception
      at initialization. Instead it merely sets CastlePngInitialized to false.
      This way programs that use this unit do not require libpng to be
      installed on target system. Libpng must be present only if program
      at runtime will really need it, e.g. Images.LoadPNG will raise an
      exception if CastlePngInitialized is @false.)

    @item(Use my CastleInternalZLib instead of Zlib module. Primarily for having
      CastleZLibInitialized, similar to CastlePngInitialized for zlib.)

    @item(We try to open libpng library from various names, to try hard
      to work with various libpng so/dll names user may have installed
      on his system.

      In particular, we eventually fallback to using libpng.so name
      (usually coming from libpng-dev packages on Linux distros).
      So it can work with any libpng version.
      This is possible by using explicit loading at unit
      initialization, instead of using "exported" declarations
      (that would tie us to particular so name, the one "ld" saw referenced
      by libpng.so symlink at compilation).

      Of course, this works as long as use only functions that are really
      compatible across all existing libpng versions.)

    @item(All functions are loaded using my TDynLib class in unit's initialization,
      instead of "external" declarations. This allows various above features,
      it also allows to easily find if some functions are missing in
      libpng.(so|dll).)

    @item(Added all missing constants (probably lost in FPC during h2pas).
      Added also PNG_LIBPNG_VER_* constants, although don't use them,
      see comments.)

    @item(
      Works with Windows libpng version with stdcalls
      (changed "cdecl" to "{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif}"))

    @item(
      Many deprecated functions removed by default.
      See LIBPNG_DEPRECATED define.
      Also, there's a define LIBPNG_1_4, to change some types to match 1.4
      headers, see
      [http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt].)

    @item(
      Compileable with Delphi (under Delphi use "{$ALIGN 4}" instead of
      "{$PACKRECORDS C}", "pointer" instead of "jmp_buf").
      Also DWord is LongWord, so it doesn't require Types unit under Delphi.
      Note: Delphi not really tested since a long time.)
  )
*)

interface

uses CastleInternalZLib;

{ Automatically converted by H2Pas 0.99.15 from png.h }
{ The following command line parameters were used:
    png.h
}

{$ifndef FPC}
  {$ALIGN 4}
{$else}
  {$PACKRECORDS C}
{$endif}

{ Version consts. Added by Kambi.

  Unfortunately, they are specific to so/dll version. Very bad --- I'd like
  to have application that can run with every version that is compatible
  instead of only some given version. So I use my functions
  SO_PNG_LIBPNG_VER_STRING, SO_PNG_VER_MAJOR etc.
  that return the real .so (or .dll) version number
  (using png_access_version_number). This version can be then passed to
  png_create_write_struct to ensure that libpng will not return with error :
  "png.h and png.c versions not compatible".

  Be aware that using an uncompatible libpng version will probably lead
  to nasty segfaults or such. }
const
  PNG_LIBPNG_VER_STRING = '1.2.13';
  { These should match the first 3 components of PNG_LIBPNG_VER_STRING: }
  PNG_LIBPNG_VER_MAJOR  = 1;
  PNG_LIBPNG_VER_MINOR  = 2;
  PNG_LIBPNG_VER_RELEASE= 13;

{ All consts below added by Kambi. }

const
{ Supported compression types for text in PNG files (tEXt, and zTXt).
 * The values of the PNG_TEXT_COMPRESSION_ defines should NOT be changed. }
  PNG_TEXT_COMPRESSION_NONE_WR = -3;
  PNG_TEXT_COMPRESSION_zTXt_WR = -2;
  PNG_TEXT_COMPRESSION_NONE = -1;
  PNG_TEXT_COMPRESSION_zTXt = 0;
  PNG_ITXT_COMPRESSION_NONE = 1;
  PNG_ITXT_COMPRESSION_zTXt = 2;
  PNG_TEXT_COMPRESSION_LAST = 3;  { Not a valid value }

{ Maximum positive integer used in PNG is (2^31)-1 }
  PNG_MAX_UINT = High(LongWord);

{ These describe the color_type field in png_info. }
{ color type masks }
  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR = 2;
  PNG_COLOR_MASK_ALPHA = 4;

{ color types.  Note that not all combinations are legal }
  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE);
  PNG_COLOR_TYPE_RGB = (PNG_COLOR_MASK_COLOR);
  PNG_COLOR_TYPE_RGB_ALPHA = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA);
  PNG_COLOR_TYPE_GRAY_ALPHA = (PNG_COLOR_MASK_ALPHA);
{ aliases }
  PNG_COLOR_TYPE_RGBA = PNG_COLOR_TYPE_RGB_ALPHA;
  PNG_COLOR_TYPE_GA = PNG_COLOR_TYPE_GRAY_ALPHA;

{ This is for compression type. PNG 1.0-1.2 only define the single type. }
  PNG_COMPRESSION_TYPE_BASE = 0 { Deflate method 8, 32K window };
  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;

{ This is for filter type. PNG 1.0-1.2 only define the single type. }
  PNG_FILTER_TYPE_BASE = 0 { Single row per-byte filtering };
  PNG_INTRAPIXEL_DIFFERENCING = 64 { Used only in MNG datastreams };
  PNG_FILTER_TYPE_DEFAULT = PNG_FILTER_TYPE_BASE;

{ These are for the interlacing type.  These values should NOT be changed. }
  PNG_INTERLACE_NONE = 0 { Non-interlaced image };
  PNG_INTERLACE_ADAM7 = 1 { Adam7 interlacing };
  PNG_INTERLACE_LAST = 2 { Not a valid value };

{ These are for the oFFs chunk.  These values should NOT be changed. }
  PNG_OFFSET_PIXEL = 0 { Offset in pixels };
  PNG_OFFSET_MICROMETER = 1 { Offset in micrometers (1/10^6 meter) };
  PNG_OFFSET_LAST = 2 { Not a valid value };

{ These are for the pCAL chunk.  These values should NOT be changed. }
  PNG_EQUATION_LINEAR = 0 { Linear transformation };
  PNG_EQUATION_BASE_E = 1 { Exponential base e transform };
  PNG_EQUATION_ARBITRARY = 2 { Arbitrary base exponential transform };
  PNG_EQUATION_HYPERBOLIC = 3 { Hyperbolic sine transformation };
  PNG_EQUATION_LAST = 4 { Not a valid value };

{ These are for the sCAL chunk.  These values should NOT be changed. }
  PNG_SCALE_UNKNOWN = 0 { unknown unit (image scale) };
  PNG_SCALE_METER = 1 { meters per pixel };
  PNG_SCALE_RADIAN = 2 { radians per pixel };
  PNG_SCALE_LAST = 3 { Not a valid value };

{ These are for the pHYs chunk.  These values should NOT be changed. }
  PNG_RESOLUTION_UNKNOWN = 0 { pixels/unknown unit (aspect ratio) };
  PNG_RESOLUTION_METER = 1 { pixels/meter };
  PNG_RESOLUTION_LAST = 2 { Not a valid value };

{ These are for the sRGB chunk.  These values should NOT be changed. }
  PNG_sRGB_INTENT_PERCEPTUAL =0;
  PNG_sRGB_INTENT_RELATIVE   =1;
  PNG_sRGB_INTENT_SATURATION =2;
  PNG_sRGB_INTENT_ABSOLUTE   =3;
  PNG_sRGB_INTENT_LAST = 4 { Not a valid value };

{ This is for text chunks }
  PNG_KEYWORD_MAX_LENGTH = 79;

{ Maximum number of entries in PLTE/sPLT/tRNS arrays }
  PNG_MAX_PALETTE_LENGTH = 256;

{ These determine if an ancillary chunk's data has been successfully read
 * from the PNG header, or if the application has filled in the corresponding
 * data in the info_struct to be written into the output file.  The values
 * of the PNG_INFO_<chunk> defines should NOT be changed.
 }
  PNG_INFO_gAMA = $0001;
  PNG_INFO_sBIT = $0002;
  PNG_INFO_cHRM = $0004;
  PNG_INFO_PLTE = $0008;
  PNG_INFO_tRNS = $0010;
  PNG_INFO_bKGD = $0020;
  PNG_INFO_hIST = $0040;
  PNG_INFO_pHYs = $0080;
  PNG_INFO_oFFs = $0100;
  PNG_INFO_tIME = $0200;
  PNG_INFO_pCAL = $0400;
  PNG_INFO_sRGB = $0800   { GR-P, 0.96a };
  PNG_INFO_iCCP = $1000   { ESR, 1.0.6 };
  PNG_INFO_sPLT = $2000   { ESR, 1.0.6 };
  PNG_INFO_sCAL = $4000   { ESR, 1.0.6 };
  PNG_INFO_IDAT = $8000   { ESR, 1.0.6 };

{ Transform masks for the high-level interface }
  PNG_TRANSFORM_IDENTITY = $0000    { read and write };
  PNG_TRANSFORM_STRIP_16 = $0001    { read only };
  PNG_TRANSFORM_STRIP_ALPHA = $0002    { read only };
  PNG_TRANSFORM_PACKING = $0004    { read and write };
  PNG_TRANSFORM_PACKSWAP = $0008    { read and write };
  PNG_TRANSFORM_EXPAND = $0010    { read only };
  PNG_TRANSFORM_INVERT_MONO = $0020    { read and write };
  PNG_TRANSFORM_SHIFT = $0040    { read and write };
  PNG_TRANSFORM_BGR = $0080    { read and write };
  PNG_TRANSFORM_SWAP_ALPHA = $0100    { read and write };
  PNG_TRANSFORM_SWAP_ENDIAN = $0200    { read and write };
  PNG_TRANSFORM_INVERT_ALPHA = $0400    { read and write };
  PNG_TRANSFORM_STRIP_FILLER = $0800    { WRITE only };

{ Flags for MNG supported features }
  PNG_FLAG_MNG_EMPTY_PLTE = $01;
  PNG_FLAG_MNG_FILTER_64 = $04;
  PNG_ALL_MNG_FEATURES = $05;

{ png_set_filler : Add a filler byte to 24-bit RGB images. }
{ The values of the PNG_FILLER_ defines should NOT be changed }
  PNG_FILLER_BEFORE =0;
  PNG_FILLER_AFTER =1;

{ png_set_background : Handle alpha and tRNS by replacing with a background color. }
  PNG_BACKGROUND_GAMMA_UNKNOWN =0;
  PNG_BACKGROUND_GAMMA_SCREEN  =1;
  PNG_BACKGROUND_GAMMA_FILE    =2;
  PNG_BACKGROUND_GAMMA_UNIQUE  =3;

{ Values for png_set_crc_action() to say how to handle CRC errors in
 * ancillary and critical chunks, and whether to use the data contained
 * therein.  Note that it is impossible to "discard" data in a critical
 * chunk.  For versions prior to 0.90, the action was always error/quit,
 * whereas in version 0.90 and later, the action for CRC errors in ancillary
 * chunks is warn/discard.  These values should NOT be changed.
 *
 *      value                       action:critical     action:ancillary
 }
  PNG_CRC_DEFAULT = 0  { error/quit          warn/discard data };
  PNG_CRC_ERROR_QUIT = 1  { error/quit          error/quit        };
  PNG_CRC_WARN_DISCARD = 2  { (INVALID)           warn/discard data };
  PNG_CRC_WARN_USE = 3  { warn/use data       warn/use data     };
  PNG_CRC_QUIET_USE = 4  { quiet/use data      quiet/use data    };
  PNG_CRC_NO_CHANGE = 5  { use current value   use current value };

{ Flags for png_set_filter() to say which filters to use.  The flags
 * are chosen so that they don't conflict with real filter types
 * below, in case they are supplied instead of the #defined constants.
 * These values should NOT be changed.
 }
  PNG_NO_FILTERS = $00;
  PNG_FILTER_NONE = $08;
  PNG_FILTER_SUB = $10;
  PNG_FILTER_UP = $20;
  PNG_FILTER_AVG = $40;
  PNG_FILTER_PAETH = $80;
  PNG_ALL_FILTERS = (PNG_FILTER_NONE or PNG_FILTER_SUB or PNG_FILTER_UP or
                         PNG_FILTER_AVG or PNG_FILTER_PAETH);

{ Filter values (not flags) - used in pngwrite.c, pngwutil.c for now.
 * These defines should NOT be changed.
 }
  PNG_FILTER_VALUE_NONE  =0;
  PNG_FILTER_VALUE_SUB   =1;
  PNG_FILTER_VALUE_UP    =2;
  PNG_FILTER_VALUE_AVG   =3;
  PNG_FILTER_VALUE_PAETH =4;
  PNG_FILTER_VALUE_LAST  =5;

{ Heuristic used for row filter selection.  These defines should NOT be
 * changed.
 }
  PNG_FILTER_HEURISTIC_DEFAULT = 0  { Currently "UNWEIGHTED" };
  PNG_FILTER_HEURISTIC_UNWEIGHTED = 1  { Used by libpng < 0.95 };
  PNG_FILTER_HEURISTIC_WEIGHTED = 2  { Experimental feature };
  PNG_FILTER_HEURISTIC_LAST = 3  { Not a valid value };

type
   { @noAutoLinkHere }
   size_t = longint;
   { @noAutoLinkHere }
   time_t = longint;
   { @noAutoLinkHere }
   int = longint;
   z_stream = TZStream;
   { @noAutoLinkHere }
   voidp = pointer;

   png_uint_32 = LongWord;
   png_int_32 = longint;
   png_uint_16 = word;
   png_int_16 = smallint;
   png_byte = byte;
   ppng_uint_32 = ^png_uint_32;
   ppng_int_32 = ^png_int_32;
   ppng_uint_16 = ^png_uint_16;
   ppng_int_16 = ^png_int_16;
   ppng_byte = ^png_byte;
   pppng_uint_32 = ^ppng_uint_32;
   pppng_int_32 = ^ppng_int_32;
   pppng_uint_16 = ^ppng_uint_16;
   pppng_int_16 = ^ppng_int_16;
   pppng_byte = ^ppng_byte;
   png_size_t = size_t;
   png_fixed_point = png_int_32;
   ppng_fixed_point = ^png_fixed_point;
   pppng_fixed_point = ^ppng_fixed_point;
   png_voidp = pointer;
   png_bytep = Ppng_byte;
   ppng_bytep = ^png_bytep;
   png_uint_32p = Ppng_uint_32;
   png_int_32p = Ppng_int_32;
   png_uint_16p = Ppng_uint_16;
   ppng_uint_16p = ^png_uint_16p;
   png_int_16p = Ppng_int_16;
(* Const before type ignored *)
   png_const_charp = Pchar;
   png_charp = Pchar;
   ppng_charp = ^png_charp;
   png_fixed_point_p = Ppng_fixed_point;
   TFile = Pointer;
   png_FILE_p = ^FILE;
   png_doublep = Pdouble;
   png_bytepp = PPpng_byte;
   png_uint_32pp = PPpng_uint_32;
   png_int_32pp = PPpng_int_32;
   png_uint_16pp = PPpng_uint_16;
   png_int_16pp = PPpng_int_16;
 (* Const before type ignored *)
   png_const_charpp = PPchar;
   png_charpp = PPchar;
   ppng_charpp = ^png_charpp;
   png_fixed_point_pp = PPpng_fixed_point;
   PPDouble = ^PDouble;
   png_doublepp = PPdouble;
   PPPChar = ^PPCHar;
   png_charppp = PPPchar;
   Pcharf = Pchar;
   PPcharf = ^Pcharf;
   png_zcharp = Pcharf;
   png_zcharpp = PPcharf;
   png_zstreamp = Pzstream;

{$ifdef LIBPNG_DEPRECATED}
{ These variables didn't work since a long time,
  and http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt
  confirms they are deprecated and removed in newer libpng 1.4. }
var
  png_libpng_ver : array[0..11] of char;   cvar; external;
  png_pass_start : array[0..6] of longint; cvar; external;
  png_pass_inc : array[0..6] of longint;   cvar; external;
  png_pass_ystart : array[0..6] of longint;cvar; external;
  png_pass_yinc : array[0..6] of longint;  cvar; external;
  png_pass_mask : array[0..6] of longint;  cvar; external;
  png_pass_dsp_mask : array[0..6] of longint; cvar; external;
{$endif LIBPNG_DEPRECATED}

Type
  png_color = record
       red : png_byte;
       green : png_byte;
       blue : png_byte;
    end;
  ppng_color = ^png_color;
  pppng_color = ^ppng_color;

  png_color_struct = png_color;
  png_colorp = Ppng_color;
  ppng_colorp = ^png_colorp;
  png_colorpp = PPpng_color;
  png_color_16 = record
       index : png_byte;
       red : png_uint_16;
       green : png_uint_16;
       blue : png_uint_16;
       gray : png_uint_16;
    end;
  ppng_color_16 = ^png_color_16 ;
  pppng_color_16 = ^ppng_color_16 ;
  png_color_16_struct = png_color_16;
  png_color_16p = Ppng_color_16;
  ppng_color_16p = ^png_color_16p;
  png_color_16pp = PPpng_color_16;
  png_color_8 = record
       red : png_byte;
       green : png_byte;
       blue : png_byte;
       gray : png_byte;
       alpha : png_byte;
    end;
  ppng_color_8 = ^png_color_8;
  pppng_color_8 = ^ppng_color_8;
  png_color_8_struct = png_color_8;
  png_color_8p = Ppng_color_8;
  ppng_color_8p = ^png_color_8p;
  png_color_8pp = PPpng_color_8;
  png_sPLT_entry = record
       red : png_uint_16;
       green : png_uint_16;
       blue : png_uint_16;
       alpha : png_uint_16;
       frequency : png_uint_16;
    end;
  ppng_sPLT_entry = ^png_sPLT_entry;
  pppng_sPLT_entry = ^ppng_sPLT_entry;
  png_sPLT_entry_struct = png_sPLT_entry;
  png_sPLT_entryp = Ppng_sPLT_entry;
  png_sPLT_entrypp = PPpng_sPLT_entry;
  png_sPLT_t = record
       name : png_charp;
       depth : png_byte;
       entries : png_sPLT_entryp;
       nentries : png_int_32;
    end;
  ppng_sPLT_t = ^png_sPLT_t;
  pppng_sPLT_t = ^ppng_sPLT_t;
  png_sPLT_struct = png_sPLT_t;
  png_sPLT_tp = Ppng_sPLT_t;
  png_sPLT_tpp = PPpng_sPLT_t;
  png_text = record
       compression : longint;
       key : png_charp;
       text : png_charp;
       text_length : png_size_t;
    end;
  ppng_text = ^png_text;
  pppng_text = ^ppng_text;

  png_text_struct = png_text;
  png_textp = Ppng_text;
  ppng_textp = ^png_textp;
  png_textpp = PPpng_text;
  png_time = record
       year : png_uint_16;
       month : png_byte;
       day : png_byte;
       hour : png_byte;
       minute : png_byte;
       second : png_byte;
    end;
  ppng_time = ^png_time;
  pppng_time = ^ppng_time;

  png_time_struct = png_time;
  png_timep = Ppng_time;
  PPNG_TIMEP = ^PNG_TIMEP;
  png_timepp = PPpng_time;
  png_unknown_chunk = record
       name : array[0..4] of png_byte;
       data : Ppng_byte;
       size : png_size_t;
       location : png_byte;
    end;
  ppng_unknown_chunk = ^png_unknown_chunk;
  pppng_unknown_chunk = ^ppng_unknown_chunk;

  png_unknown_chunk_t = png_unknown_chunk;
  png_unknown_chunkp = Ppng_unknown_chunk;
  png_unknown_chunkpp = PPpng_unknown_chunk;
  png_info = record
       width : png_uint_32;
       height : png_uint_32;
       valid : png_uint_32;
       rowbytes : png_uint_32;
       palette : png_colorp;
       num_palette : png_uint_16;
       num_trans : png_uint_16;
       bit_depth : png_byte;
       color_type : png_byte;
       compression_type : png_byte;
       filter_type : png_byte;
       interlace_type : png_byte;
       channels : png_byte;
       pixel_depth : png_byte;
       spare_byte : png_byte;
       signature : array[0..7] of png_byte;
       gamma : double;
       srgb_intent : png_byte;
       num_text : longint;
       max_text : longint;
       text : png_textp;
       mod_time : png_time;
       sig_bit : png_color_8;
       trans : png_bytep;
       trans_values : png_color_16;
       background : png_color_16;
       x_offset : png_int_32;
       y_offset : png_int_32;
       offset_unit_type : png_byte;
       x_pixels_per_unit : png_uint_32;
       y_pixels_per_unit : png_uint_32;
       phys_unit_type : png_byte;
       hist : png_uint_16p;
       x_white : double;
       y_white : double;
       x_red : double;
       y_red : double;
       x_green : double;
       y_green : double;
       x_blue : double;
       y_blue : double;
       pcal_purpose : png_charp;
       pcal_X0 : png_int_32;
       pcal_X1 : png_int_32;
       pcal_units : png_charp;
       pcal_params : png_charpp;
       pcal_type : png_byte;
       pcal_nparams : png_byte;
       free_me : png_uint_32;
       unknown_chunks : png_unknown_chunkp;
       unknown_chunks_num : png_size_t;
       iccp_name : png_charp;
       iccp_profile : png_charp;
       iccp_proflen : png_uint_32;
       iccp_compression : png_byte;
       splt_palettes : png_sPLT_tp;
       splt_palettes_num : png_uint_32;
       scal_unit : png_byte;
       scal_pixel_width : double;
       scal_pixel_height : double;
       scal_s_width : png_charp;
       scal_s_height : png_charp;
       row_pointers : png_bytepp;
       int_gamma : png_fixed_point;
       int_x_white : png_fixed_point;
       int_y_white : png_fixed_point;
       int_x_red : png_fixed_point;
       int_y_red : png_fixed_point;
       int_x_green : png_fixed_point;
       int_y_green : png_fixed_point;
       int_x_blue : png_fixed_point;
       int_y_blue : png_fixed_point;
    end;
  ppng_info = ^png_info;
  pppng_info = ^ppng_info;

  png_info_struct = png_info;
  png_infop = Ppng_info;
  png_infopp = PPpng_info;
  png_row_info = record
       width : png_uint_32;
       rowbytes : png_uint_32;
       color_type : png_byte;
       bit_depth : png_byte;
       channels : png_byte;
       pixel_depth : png_byte;
    end;
  ppng_row_info = ^png_row_info;
  pppng_row_info = ^ppng_row_info;

  png_row_info_struct = png_row_info;
  png_row_infop = Ppng_row_info;
  png_row_infopp = PPpng_row_info;
//  png_struct_def = png_struct;
  png_structp = ^png_struct;

png_error_ptr = Procedure(Arg1 : png_structp; Arg2 : png_const_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_rw_ptr = Procedure(Arg1 : png_structp; Arg2 : png_bytep; Arg3 : png_size_t);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_flush_ptr = procedure (Arg1 : png_structp) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_read_status_ptr = procedure (Arg1 : png_structp; Arg2 : png_uint_32; Arg3: int);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_write_status_ptr = Procedure (Arg1 : png_structp; Arg2: png_uint_32;Arg3 : int) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_progressive_info_ptr = Procedure (Arg1 : png_structp; Arg2 : png_infop) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_progressive_end_ptr = Procedure (Arg1 : png_structp; Arg2 : png_infop) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_progressive_row_ptr = Procedure (Arg1 : png_structp; Arg2 : png_bytep; Arg3 : png_uint_32; Arg4 : int) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_user_transform_ptr = Procedure (Arg1 : png_structp; Arg2 : png_row_infop; Arg3 : png_bytep) ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_user_chunk_ptr = Function (Arg1 : png_structp; Arg2 : png_unknown_chunkp): longint;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_unknown_chunk_ptr = Procedure (Arg1 : png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_malloc_ptr = Function (Arg1 : png_structp; Arg2 : png_size_t) : png_voidp ;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
png_free_ptr = Procedure (Arg1 : png_structp; Arg2 : png_voidp) ; {$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

   png_struct_def = record
        jmpbuf : jmp_buf;
        error_fn : png_error_ptr;
        warning_fn : png_error_ptr;
        error_ptr : png_voidp;
        write_data_fn : png_rw_ptr;
        read_data_fn : png_rw_ptr;
        io_ptr : png_voidp;
        read_user_transform_fn : png_user_transform_ptr;
        write_user_transform_fn : png_user_transform_ptr;
        user_transform_ptr : png_voidp;
        user_transform_depth : png_byte;
        user_transform_channels : png_byte;
        mode : png_uint_32;
        flags : png_uint_32;
        transformations : png_uint_32;
        zstream : z_stream;
        zbuf : png_bytep;
        zbuf_size : png_size_t;
        zlib_level : longint;
        zlib_method : longint;
        zlib_window_bits : longint;
        zlib_mem_level : longint;
        zlib_strategy : longint;
        width : png_uint_32;
        height : png_uint_32;
        num_rows : png_uint_32;
        usr_width : png_uint_32;
        rowbytes : png_uint_32;
        irowbytes : png_uint_32;
        iwidth : png_uint_32;
        row_number : png_uint_32;
        prev_row : png_bytep;
        row_buf : png_bytep;
        sub_row : png_bytep;
        up_row : png_bytep;
        avg_row : png_bytep;
        paeth_row : png_bytep;
        row_info : png_row_info;
        idat_size : png_uint_32;
        crc : png_uint_32;
        palette : png_colorp;
        num_palette : png_uint_16;
        num_trans : png_uint_16;
        chunk_name : array[0..4] of png_byte;
        compression : png_byte;
        filter : png_byte;
        interlaced : png_byte;
        pass : png_byte;
        do_filter : png_byte;
        color_type : png_byte;
        bit_depth : png_byte;
        usr_bit_depth : png_byte;
        pixel_depth : png_byte;
        channels : png_byte;
        usr_channels : png_byte;
        sig_bytes : png_byte;
        filler : png_uint_16;
        background_gamma_type : png_byte;
        background_gamma : double;
        background : png_color_16;
        background_1 : png_color_16;
        output_flush_fn : png_flush_ptr;
        flush_dist : png_uint_32;
        flush_rows : png_uint_32;
        gamma_shift : longint;
        gamma : double;
        screen_gamma : double;
        gamma_table : png_bytep;
        gamma_from_1 : png_bytep;
        gamma_to_1 : png_bytep;
        gamma_16_table : png_uint_16pp;
        gamma_16_from_1 : png_uint_16pp;
        gamma_16_to_1 : png_uint_16pp;
        sig_bit : png_color_8;
        shift : png_color_8;
        trans : png_bytep;
        trans_values : png_color_16;
        read_row_fn : png_read_status_ptr;
        write_row_fn : png_write_status_ptr;
        info_fn : png_progressive_info_ptr;
        row_fn : png_progressive_row_ptr;
        end_fn : png_progressive_end_ptr;
        save_buffer_ptr : png_bytep;
        save_buffer : png_bytep;
        current_buffer_ptr : png_bytep;
        current_buffer : png_bytep;
        push_length : png_uint_32;
        skip_length : png_uint_32;
        save_buffer_size : png_size_t;
        save_buffer_max : png_size_t;
        buffer_size : png_size_t;
        current_buffer_size : png_size_t;
        process_mode : longint;
        cur_palette : longint;
        current_text_size : png_size_t;
        current_text_left : png_size_t;
        current_text : png_charp;
        current_text_ptr : png_charp;
        palette_lookup : png_bytep;
        dither_index : png_bytep;
        hist : png_uint_16p;
        heuristic_method : png_byte;
        num_prev_filters : png_byte;
        prev_filters : png_bytep;
        filter_weights : png_uint_16p;
        inv_filter_weights : png_uint_16p;
        filter_costs : png_uint_16p;
        inv_filter_costs : png_uint_16p;
        time_buffer : png_charp;
        free_me : png_uint_32;
        user_chunk_ptr : png_voidp;
        read_user_chunk_fn : png_user_chunk_ptr;
        num_chunk_list : longint;
        chunk_list : png_bytep;
        rgb_to_gray_status : png_byte;
        rgb_to_gray_red_coeff : png_uint_16;
        rgb_to_gray_green_coeff : png_uint_16;
        rgb_to_gray_blue_coeff : png_uint_16;
        empty_plte_permitted : png_byte;
        int_gamma : png_fixed_point;
     end;
   ppng_struct_def = ^png_struct_def;
   pppng_struct_def = ^ppng_struct_def;
   png_struct = png_struct_def;
   ppng_struct = ^png_struct;
   pppng_struct = ^ppng_struct;

   version_1_0_8 = png_structp;
   png_structpp = PPpng_struct;

var
  png_access_version_number: function: png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sig_bytes: procedure(png_ptr: png_structp; num_bytes: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_sig_cmp: function(sig: png_bytep; start: png_size_t; num_to_check: png_size_t): longint;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_create_read_struct: function(user_png_ver: png_const_charp; error_ptr: png_voidp; error_fn: png_error_ptr; warn_fn: png_error_ptr): png_structp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_create_write_struct: function(user_png_ver: png_const_charp; error_ptr: png_voidp; error_fn: png_error_ptr; warn_fn: png_error_ptr): png_structp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_compression_buffer_size: function(png_ptr: png_structp): {$ifdef LIBPNG_1_4} png_size_t {$else} png_uint_32 {$endif};{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_buffer_size: procedure(png_ptr: png_structp; size: {$ifdef LIBPNG_1_4} png_size_t {$else} png_uint_32 {$endif});{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_reset_zstream: function(png_ptr: png_structp): longint;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_chunk: procedure(png_ptr: png_structp; chunk_name: png_bytep; data: png_bytep; length: png_size_t);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_chunk_start: procedure(png_ptr: png_structp; chunk_name: png_bytep; length: png_uint_32);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_chunk_data: procedure(png_ptr: png_structp; data: png_bytep; length: png_size_t);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_chunk_end: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_create_info_struct: function(png_ptr: png_structp): png_infop;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_info_before_PLTE: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_info: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_info: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_convert_to_rfc1123: function(png_ptr: png_structp; ptime: png_timep): png_charp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_convert_from_struct_tm: procedure(ptime: png_timep; ttime: Pointer);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_convert_from_time_t: procedure(ptime: png_timep; ttime: time_t);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_expand: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_palette_to_rgb: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_tRNS_to_alpha: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_bgr: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_gray_to_rgb: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_rgb_to_gray: procedure(png_ptr: png_structp; error_action: longint; red: double; green: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_rgb_to_gray_fixed: procedure(png_ptr: png_structp; error_action: longint; red: png_fixed_point; green: png_fixed_point);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_rgb_to_gray_status: function(png_ptr: png_structp): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_build_grayscale_palette: procedure(bit_depth: longint; palette: png_colorp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_strip_alpha: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_swap_alpha: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_invert_alpha: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_filler: procedure(png_ptr: png_structp; filler: png_uint_32; flags: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_swap: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_packing: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_packswap: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_shift: procedure(png_ptr: png_structp; true_bits: png_color_8p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_interlace_handling: function(png_ptr: png_structp): longint;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_invert_mono: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_background: procedure(png_ptr: png_structp; background_color: png_color_16p; background_gamma_code: longint; need_expand: longint; background_gamma: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_strip_16: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_gamma: procedure(png_ptr: png_structp; screen_gamma: double; default_file_gamma: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_flush: procedure(png_ptr: png_structp; nrows: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_flush: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_start_read_image: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_update_info: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_rows: procedure(png_ptr: png_structp; row: png_bytepp; display_row: png_bytepp; num_rows: png_uint_32);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_row: procedure(png_ptr: png_structp; row: png_bytep; display_row: png_bytep);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_image: procedure(png_ptr: png_structp; image: png_bytepp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_row: procedure(png_ptr: png_structp; row: png_bytep);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_rows: procedure(png_ptr: png_structp; row: png_bytepp; num_rows: png_uint_32);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_image: procedure(png_ptr: png_structp; image: png_bytepp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_end: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_end: procedure(png_ptr: png_structp; info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_destroy_info_struct: procedure(png_ptr: png_structp; info_ptr_ptr: png_infopp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_destroy_read_struct: procedure(png_ptr_ptr: png_structpp; info_ptr_ptr: png_infopp; end_info_ptr_ptr: png_infopp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_destroy_write_struct: procedure(png_ptr_ptr: png_structpp; info_ptr_ptr: png_infopp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

  png_set_crc_action: procedure(png_ptr: png_structp; crit_action: longint; ancil_action: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_filter: procedure(png_ptr: png_structp; method: longint; filters: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_filter_heuristics: procedure(png_ptr: png_structp; heuristic_method: longint; num_weights: longint; filter_weights: png_doublep; filter_costs: png_doublep);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_level: procedure(png_ptr: png_structp; level: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_mem_level: procedure(png_ptr: png_structp; mem_level: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_strategy: procedure(png_ptr: png_structp; strategy: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_window_bits: procedure(png_ptr: png_structp; window_bits: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_compression_method: procedure(png_ptr: png_structp; method: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_init_io: procedure(png_ptr: png_structp; fp: png_FILE_p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_error_fn: procedure(png_ptr: png_structp; error_ptr: png_voidp; error_fn: png_error_ptr; warning_fn: png_error_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_error_ptr: function(png_ptr: png_structp): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_write_fn: procedure(png_ptr: png_structp; io_ptr: png_voidp; write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_read_fn: procedure(png_ptr: png_structp; io_ptr: png_voidp; read_data_fn: png_rw_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_io_ptr: function(png_ptr: png_structp): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_read_status_fn: procedure(png_ptr: png_structp; read_row_fn: png_read_status_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_write_status_fn: procedure(png_ptr: png_structp; write_row_fn: png_write_status_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_read_user_transform_fn: procedure(png_ptr: png_structp; read_user_transform_fn: png_user_transform_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_write_user_transform_fn: procedure(png_ptr: png_structp; write_user_transform_fn: png_user_transform_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_user_transform_info: procedure(png_ptr: png_structp; user_transform_ptr: png_voidp; user_transform_depth: longint; user_transform_channels: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_user_transform_ptr: function(png_ptr: png_structp): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_read_user_chunk_fn: procedure(png_ptr: png_structp; user_chunk_ptr: png_voidp; read_user_chunk_fn: png_user_chunk_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_user_chunk_ptr: function(png_ptr: png_structp): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_progressive_read_fn: procedure(png_ptr: png_structp; progressive_ptr: png_voidp; info_fn: png_progressive_info_ptr; row_fn: png_progressive_row_ptr; end_fn: png_progressive_end_ptr);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_progressive_ptr: function(png_ptr: png_structp): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_process_data: procedure(png_ptr: png_structp; info_ptr: png_infop; buffer: png_bytep; buffer_size: png_size_t);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_progressive_combine_row: procedure(png_ptr: png_structp; old_row: png_bytep; new_row: png_bytep);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_malloc: function(png_ptr: png_structp; size: {$ifdef LIBPNG_1_4} png_alloc_size_t {$else} png_uint_32 {$endif}): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_free: procedure(png_ptr: png_structp; ptr: png_voidp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_free_data: procedure(png_ptr: png_structp; info_ptr: png_infop; free_me: png_uint_32; num: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_data_freer: procedure(png_ptr: png_structp; info_ptr: png_infop; freer: longint; mask: png_uint_32);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_error: procedure(png_ptr: png_structp; error: png_const_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_chunk_error: procedure(png_ptr: png_structp; error: png_const_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_warning: procedure(png_ptr: png_structp; message: png_const_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_chunk_warning: procedure(png_ptr: png_structp; message: png_const_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_valid: function(png_ptr: png_structp; info_ptr: png_infop; flag: png_uint_32): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_rowbytes: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_rows: function(png_ptr: png_structp; info_ptr: png_infop): png_bytepp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_rows: procedure(png_ptr: png_structp; info_ptr: png_infop; row_pointers: png_bytepp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_channels: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_image_width: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_image_height: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_bit_depth: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_color_type: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_filter_type: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_interlace_type: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_compression_type: function(png_ptr: png_structp; info_ptr: png_infop): png_byte;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_pixels_per_meter: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_x_pixels_per_meter: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_y_pixels_per_meter: function(png_ptr: png_structp; info_ptr: png_infop): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_pixel_aspect_ratio: function(png_ptr: png_structp; info_ptr: png_infop): double;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_x_offset_pixels: function(png_ptr: png_structp; info_ptr: png_infop): png_int_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_y_offset_pixels: function(png_ptr: png_structp; info_ptr: png_infop): png_int_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_x_offset_microns: function(png_ptr: png_structp; info_ptr: png_infop): png_int_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_y_offset_microns: function(png_ptr: png_structp; info_ptr: png_infop): png_int_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_signature: function(png_ptr: png_structp; info_ptr: png_infop): png_bytep;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_bKGD: function(png_ptr: png_structp; info_ptr: png_infop; background: Ppng_color_16p): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_bKGD: procedure(png_ptr: png_structp; info_ptr: png_infop; background: png_color_16p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_cHRM: function(png_ptr: png_structp; info_ptr: png_infop; white_x: Pdouble; white_y: Pdouble; red_x: Pdouble;
           red_y: Pdouble; green_x: Pdouble; green_y: Pdouble; blue_x: Pdouble; blue_y: Pdouble): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_cHRM_fixed: function(png_ptr: png_structp; info_ptr: png_infop; int_white_x: Ppng_fixed_point; int_white_y: Ppng_fixed_point; int_red_x: Ppng_fixed_point;
           int_red_y: Ppng_fixed_point; int_green_x: Ppng_fixed_point; int_green_y: Ppng_fixed_point; int_blue_x: Ppng_fixed_point; int_blue_y: Ppng_fixed_point): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_cHRM: procedure(png_ptr: png_structp; info_ptr: png_infop; white_x: double; white_y: double; red_x: double;
            red_y: double; green_x: double; green_y: double; blue_x: double; blue_y: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_cHRM_fixed: procedure(png_ptr: png_structp; info_ptr: png_infop; int_white_x: png_fixed_point; int_white_y: png_fixed_point; int_red_x: png_fixed_point;
            int_red_y: png_fixed_point; int_green_x: png_fixed_point; int_green_y: png_fixed_point; int_blue_x: png_fixed_point; int_blue_y: png_fixed_point);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_gAMA: function(png_ptr: png_structp; info_ptr: png_infop; file_gamma: Pdouble): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_gAMA_fixed: function(png_ptr: png_structp; info_ptr: png_infop; int_file_gamma: Ppng_fixed_point): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_gAMA: procedure(png_ptr: png_structp; info_ptr: png_infop; file_gamma: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_gAMA_fixed: procedure(png_ptr: png_structp; info_ptr: png_infop; int_file_gamma: png_fixed_point);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_hIST: function(png_ptr: png_structp; info_ptr: png_infop; hist: Ppng_uint_16p): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_hIST: procedure(png_ptr: png_structp; info_ptr: png_infop; hist: png_uint_16p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_IHDR: function(png_ptr: png_structp; info_ptr: png_infop; width: Ppng_uint_32; height: Ppng_uint_32; bit_depth: Plongint;
           color_type: Plongint; interlace_type: Plongint; compression_type: Plongint; filter_type: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_IHDR: procedure(png_ptr: png_structp; info_ptr: png_infop; width: png_uint_32; height: png_uint_32; bit_depth: longint;
            color_type: longint; interlace_type: longint; compression_type: longint; filter_type: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_oFFs: function(png_ptr: png_structp; info_ptr: png_infop; offset_x: Ppng_int_32; offset_y: Ppng_int_32; unit_type: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_oFFs: procedure(png_ptr: png_structp; info_ptr: png_infop; offset_x: png_int_32; offset_y: png_int_32; unit_type: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_pCAL: function(png_ptr: png_structp; info_ptr: png_infop; purpose: Ppng_charp; X0: Ppng_int_32; X1: Ppng_int_32;
           atype: Plongint; nparams: Plongint; units: Ppng_charp; params: Ppng_charpp): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_pCAL: procedure(png_ptr: png_structp; info_ptr: png_infop; purpose: png_charp; X0: png_int_32; X1: png_int_32;
            atype: longint; nparams: longint; units: png_charp; params: png_charpp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_pHYs: function(png_ptr: png_structp; info_ptr: png_infop; res_x: Ppng_uint_32; res_y: Ppng_uint_32; unit_type: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_pHYs: procedure(png_ptr: png_structp; info_ptr: png_infop; res_x: png_uint_32; res_y: png_uint_32; unit_type: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_PLTE: function(png_ptr: png_structp; info_ptr: png_infop; palette: Ppng_colorp; num_palette: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_PLTE: procedure(png_ptr: png_structp; info_ptr: png_infop; palette: png_colorp; num_palette: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_sBIT: function(png_ptr: png_structp; info_ptr: png_infop; sig_bit: Ppng_color_8p): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sBIT: procedure(png_ptr: png_structp; info_ptr: png_infop; sig_bit: png_color_8p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_sRGB: function(png_ptr: png_structp; info_ptr: png_infop; intent: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sRGB: procedure(png_ptr: png_structp; info_ptr: png_infop; intent: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sRGB_gAMA_and_cHRM: procedure(png_ptr: png_structp; info_ptr: png_infop; intent: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_iCCP: function(png_ptr: png_structp; info_ptr: png_infop; name: png_charpp; compression_type: Plongint; profile: png_charpp;
           proflen: Ppng_uint_32): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_iCCP: procedure(png_ptr: png_structp; info_ptr: png_infop; name: png_charp; compression_type: longint; profile: png_charp;
            proflen: png_uint_32);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_sPLT: function(png_ptr: png_structp; info_ptr: png_infop; entries: png_sPLT_tpp): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sPLT: procedure(png_ptr: png_structp; info_ptr: png_infop; entries: png_sPLT_tp; nentries: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_text: function(png_ptr: png_structp; info_ptr: png_infop; text_ptr: Ppng_textp; num_text: Plongint): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_text: procedure(png_ptr: png_structp; info_ptr: png_infop; text_ptr: png_textp; num_text: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_tIME: function(png_ptr: png_structp; info_ptr: png_infop; mod_time: Ppng_timep): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_tIME: procedure(png_ptr: png_structp; info_ptr: png_infop; mod_time: png_timep);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_tRNS: function(png_ptr: png_structp; info_ptr: png_infop; trans: Ppng_bytep; num_trans: Plongint; trans_values: Ppng_color_16p): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_tRNS: procedure(png_ptr: png_structp; info_ptr: png_infop; trans: png_bytep; num_trans: longint; trans_values: png_color_16p);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_sCAL: function(png_ptr: png_structp; info_ptr: png_infop; aunit: Plongint; width: Pdouble; height: Pdouble): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sCAL: procedure(png_ptr: png_structp; info_ptr: png_infop; aunit: longint; width: double; height: double);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_keep_unknown_chunks: procedure(png_ptr: png_structp; keep: longint; chunk_list: png_bytep; num_chunks: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_unknown_chunks: procedure(png_ptr: png_structp; info_ptr: png_infop; unknowns: png_unknown_chunkp; num_unknowns: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_unknown_chunk_location: procedure(png_ptr: png_structp; info_ptr: png_infop; chunk: longint; location: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_unknown_chunks: function(png_ptr: png_structp; info_ptr: png_infop; entries: png_unknown_chunkpp): png_uint_32;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_invalid: procedure(png_ptr: png_structp; info_ptr: png_infop; mask: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_png: procedure(png_ptr: png_structp; info_ptr: png_infop; transforms: longint; params: voidp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_png: procedure(png_ptr: png_structp; info_ptr: png_infop; transforms: longint; params: voidp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_header_ver: function(png_ptr: png_structp): png_charp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_get_header_version: function(png_ptr: png_structp): png_charp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

  { Since libpng-1.0.18 and 1.2.9, according to
    http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt.
    Will be set to @nil for older libpngs. }
  png_set_expand_gray_1_2_4_to_8: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

{$ifdef LIBPNG_DEPRECATED}
  { These are deprecated, and not available anymore in many libgpng,
    by experience. }
  png_write_destroy_info: procedure(info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_sCAL_s: procedure(png_ptr: png_structp; info_ptr: png_infop; aunit: longint; swidth: png_charp; sheight: png_charp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  { Not since libpng 1.4 }
  png_set_dither: procedure(png_ptr: png_structp; palette: png_colorp; num_palette: longint; maximum_colors: longint; histogram: png_uint_16p;
            full_dither: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

  { These are deprecated since libpng 0.95,
    and not available anymore in libpng 1.4
    (following http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt) }
  png_info_init: procedure(info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_read_destroy: procedure(png_ptr: png_structp; info_ptr: png_infop; end_info_ptr: png_infop);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_write_destroy: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

  { These are deprecated since libpng 1.0.9,
    and not available anymore in libpng 1.4
    (following http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt) }
  png_permit_empty_plte: procedure(png_ptr: png_structp; empty_plte_permitted: longint);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};

  { These are deprecated,
    and not available anymore in libpng 1.4
    (following http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt) }
  png_check_sig: function(sig: png_bytep; num: longint): longint;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_memcpy_check: function(png_ptr: png_structp; s1: png_voidp; s2: png_voidp; size: png_uint_32): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_memset_check: function(png_ptr: png_structp; s1: png_voidp; value: longint; size: png_uint_32): png_voidp;{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
  png_set_gray_1_2_4_to_8: procedure(png_ptr: png_structp);{$ifndef LIBPNG_CDECL} stdcall {$else} cdecl {$endif};
{$endif LIBPNG_DEPRECATED}

{ Returns true if libpng was available and all png_xxx functions
  in this unit are initialized to non-nil values, so you can just use libpng.

  It returns false if libpng library was not available (or maybe the required
  version was not available). Then all png_xxx functions in this unit are nil
  and you can't use them. }
function CastlePngInitialized: boolean;

procedure PngInitialization;

implementation

uses SysUtils, CastleDynLib
  {$ifdef DARWIN}, { for BundlePath } CastleFilesUtils {$endif};

var
  PngLibrary: TDynLib;

function CastlePngInitialized: boolean;
begin
  Result := PngLibrary <> nil;
end;

procedure PngInitialization;
begin
  FreeAndNil(PngLibrary);

  {$ifdef MSWINDOWS}
  { libpng common names for Windows }
  { libpng for win64 distributed by http://www.gtk.org/download/win64.php }
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng14-14.dll', false);
  { libpng distributed by gnuwin32.sourceforge.net }
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng13.dll', false);
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng12.dll', false);
  {$endif}

  {$ifdef UNIX}

  {$ifdef DARWIN}
  { libpng common names for macOS }
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng.dylib', false);
  if (PngLibrary = nil) and (BundlePath <> '') then
    PngLibrary := TDynLib.Load(BundlePath + 'Contents/MacOS/libpng14.14.dylib', false);

  {$else DARWIN}
  { libpng common names for Linux, FreeBSD and other Unixes (except macOS)  }
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng16.so.16', false);
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng14.so.14', false);
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng14.so', false);
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng12.so.0', false);
  { As a final fallback, use the libpng that comes from a package like libpng-dev. }
  if PngLibrary = nil then
    PngLibrary := TDynLib.Load('libpng.so', false);
  {$endif DARWIN}

  {$endif UNIX}

  if PngLibrary <> nil then
  begin
    Pointer(png_access_version_number) := PngLibrary.Symbol('png_access_version_number');
    Pointer(png_set_sig_bytes) := PngLibrary.Symbol('png_set_sig_bytes');
    Pointer(png_sig_cmp) := PngLibrary.Symbol('png_sig_cmp');
    Pointer(png_create_read_struct) := PngLibrary.Symbol('png_create_read_struct');
    Pointer(png_create_write_struct) := PngLibrary.Symbol('png_create_write_struct');
    Pointer(png_get_compression_buffer_size) := PngLibrary.Symbol('png_get_compression_buffer_size');
    Pointer(png_set_compression_buffer_size) := PngLibrary.Symbol('png_set_compression_buffer_size');
    Pointer(png_reset_zstream) := PngLibrary.Symbol('png_reset_zstream');
    Pointer(png_write_chunk) := PngLibrary.Symbol('png_write_chunk');
    Pointer(png_write_chunk_start) := PngLibrary.Symbol('png_write_chunk_start');
    Pointer(png_write_chunk_data) := PngLibrary.Symbol('png_write_chunk_data');
    Pointer(png_write_chunk_end) := PngLibrary.Symbol('png_write_chunk_end');
    Pointer(png_create_info_struct) := PngLibrary.Symbol('png_create_info_struct');
    Pointer(png_write_info_before_PLTE) := PngLibrary.Symbol('png_write_info_before_PLTE');
    Pointer(png_write_info) := PngLibrary.Symbol('png_write_info');
    Pointer(png_read_info) := PngLibrary.Symbol('png_read_info');
    Pointer(png_convert_to_rfc1123) := PngLibrary.Symbol('png_convert_to_rfc1123');
    Pointer(png_convert_from_struct_tm) := PngLibrary.Symbol('png_convert_from_struct_tm');
    Pointer(png_convert_from_time_t) := PngLibrary.Symbol('png_convert_from_time_t');
    Pointer(png_set_expand) := PngLibrary.Symbol('png_set_expand');
    Pointer(png_set_palette_to_rgb) := PngLibrary.Symbol('png_set_palette_to_rgb');
    Pointer(png_set_tRNS_to_alpha) := PngLibrary.Symbol('png_set_tRNS_to_alpha');
    Pointer(png_set_bgr) := PngLibrary.Symbol('png_set_bgr');
    Pointer(png_set_gray_to_rgb) := PngLibrary.Symbol('png_set_gray_to_rgb');
    Pointer(png_set_rgb_to_gray) := PngLibrary.Symbol('png_set_rgb_to_gray');
    Pointer(png_set_rgb_to_gray_fixed) := PngLibrary.Symbol('png_set_rgb_to_gray_fixed');
    Pointer(png_get_rgb_to_gray_status) := PngLibrary.Symbol('png_get_rgb_to_gray_status');
    Pointer(png_build_grayscale_palette) := PngLibrary.Symbol('png_build_grayscale_palette');
    Pointer(png_set_strip_alpha) := PngLibrary.Symbol('png_set_strip_alpha');
    Pointer(png_set_swap_alpha) := PngLibrary.Symbol('png_set_swap_alpha');
    Pointer(png_set_invert_alpha) := PngLibrary.Symbol('png_set_invert_alpha');
    Pointer(png_set_filler) := PngLibrary.Symbol('png_set_filler');
    Pointer(png_set_swap) := PngLibrary.Symbol('png_set_swap');
    Pointer(png_set_packing) := PngLibrary.Symbol('png_set_packing');
    Pointer(png_set_packswap) := PngLibrary.Symbol('png_set_packswap');
    Pointer(png_set_shift) := PngLibrary.Symbol('png_set_shift');
    Pointer(png_set_interlace_handling) := PngLibrary.Symbol('png_set_interlace_handling');
    Pointer(png_set_invert_mono) := PngLibrary.Symbol('png_set_invert_mono');
    Pointer(png_set_background) := PngLibrary.Symbol('png_set_background');
    Pointer(png_set_strip_16) := PngLibrary.Symbol('png_set_strip_16');
    Pointer(png_set_gamma) := PngLibrary.Symbol('png_set_gamma');
    Pointer(png_set_flush) := PngLibrary.Symbol('png_set_flush');
    Pointer(png_write_flush) := PngLibrary.Symbol('png_write_flush');
    Pointer(png_start_read_image) := PngLibrary.Symbol('png_start_read_image');
    Pointer(png_read_update_info) := PngLibrary.Symbol('png_read_update_info');
    Pointer(png_read_rows) := PngLibrary.Symbol('png_read_rows');
    Pointer(png_read_row) := PngLibrary.Symbol('png_read_row');
    Pointer(png_read_image) := PngLibrary.Symbol('png_read_image');
    Pointer(png_write_row) := PngLibrary.Symbol('png_write_row');
    Pointer(png_write_rows) := PngLibrary.Symbol('png_write_rows');
    Pointer(png_write_image) := PngLibrary.Symbol('png_write_image');
    Pointer(png_write_end) := PngLibrary.Symbol('png_write_end');
    Pointer(png_read_end) := PngLibrary.Symbol('png_read_end');
    Pointer(png_destroy_info_struct) := PngLibrary.Symbol('png_destroy_info_struct');
    Pointer(png_destroy_read_struct) := PngLibrary.Symbol('png_destroy_read_struct');
    Pointer(png_destroy_write_struct) := PngLibrary.Symbol('png_destroy_write_struct');
    Pointer(png_set_crc_action) := PngLibrary.Symbol('png_set_crc_action');
    Pointer(png_set_filter) := PngLibrary.Symbol('png_set_filter');
    Pointer(png_set_filter_heuristics) := PngLibrary.Symbol('png_set_filter_heuristics');
    Pointer(png_set_compression_level) := PngLibrary.Symbol('png_set_compression_level');
    Pointer(png_set_compression_mem_level) := PngLibrary.Symbol('png_set_compression_mem_level');
    Pointer(png_set_compression_strategy) := PngLibrary.Symbol('png_set_compression_strategy');
    Pointer(png_set_compression_window_bits) := PngLibrary.Symbol('png_set_compression_window_bits');
    Pointer(png_set_compression_method) := PngLibrary.Symbol('png_set_compression_method');
    Pointer(png_init_io) := PngLibrary.Symbol('png_init_io');
    Pointer(png_set_error_fn) := PngLibrary.Symbol('png_set_error_fn');
    Pointer(png_get_error_ptr) := PngLibrary.Symbol('png_get_error_ptr');
    Pointer(png_set_write_fn) := PngLibrary.Symbol('png_set_write_fn');
    Pointer(png_set_read_fn) := PngLibrary.Symbol('png_set_read_fn');
    Pointer(png_get_io_ptr) := PngLibrary.Symbol('png_get_io_ptr');
    Pointer(png_set_read_status_fn) := PngLibrary.Symbol('png_set_read_status_fn');
    Pointer(png_set_write_status_fn) := PngLibrary.Symbol('png_set_write_status_fn');
    Pointer(png_set_read_user_transform_fn) := PngLibrary.Symbol('png_set_read_user_transform_fn');
    Pointer(png_set_write_user_transform_fn) := PngLibrary.Symbol('png_set_write_user_transform_fn');
    Pointer(png_set_user_transform_info) := PngLibrary.Symbol('png_set_user_transform_info');
    Pointer(png_get_user_transform_ptr) := PngLibrary.Symbol('png_get_user_transform_ptr');
    Pointer(png_set_read_user_chunk_fn) := PngLibrary.Symbol('png_set_read_user_chunk_fn');
    Pointer(png_get_user_chunk_ptr) := PngLibrary.Symbol('png_get_user_chunk_ptr');
    Pointer(png_set_progressive_read_fn) := PngLibrary.Symbol('png_set_progressive_read_fn');
    Pointer(png_get_progressive_ptr) := PngLibrary.Symbol('png_get_progressive_ptr');
    Pointer(png_process_data) := PngLibrary.Symbol('png_process_data');
    Pointer(png_progressive_combine_row) := PngLibrary.Symbol('png_progressive_combine_row');
    Pointer(png_malloc) := PngLibrary.Symbol('png_malloc');
    Pointer(png_free) := PngLibrary.Symbol('png_free');
    Pointer(png_free_data) := PngLibrary.Symbol('png_free_data');
    Pointer(png_data_freer) := PngLibrary.Symbol('png_data_freer');
    Pointer(png_error) := PngLibrary.Symbol('png_error');
    Pointer(png_chunk_error) := PngLibrary.Symbol('png_chunk_error');
    Pointer(png_warning) := PngLibrary.Symbol('png_warning');
    Pointer(png_chunk_warning) := PngLibrary.Symbol('png_chunk_warning');
    Pointer(png_get_valid) := PngLibrary.Symbol('png_get_valid');
    Pointer(png_get_rowbytes) := PngLibrary.Symbol('png_get_rowbytes');
    Pointer(png_get_rows) := PngLibrary.Symbol('png_get_rows');
    Pointer(png_set_rows) := PngLibrary.Symbol('png_set_rows');
    Pointer(png_get_channels) := PngLibrary.Symbol('png_get_channels');
    Pointer(png_get_image_width) := PngLibrary.Symbol('png_get_image_width');
    Pointer(png_get_image_height) := PngLibrary.Symbol('png_get_image_height');
    Pointer(png_get_bit_depth) := PngLibrary.Symbol('png_get_bit_depth');
    Pointer(png_get_color_type) := PngLibrary.Symbol('png_get_color_type');
    Pointer(png_get_filter_type) := PngLibrary.Symbol('png_get_filter_type');
    Pointer(png_get_interlace_type) := PngLibrary.Symbol('png_get_interlace_type');
    Pointer(png_get_compression_type) := PngLibrary.Symbol('png_get_compression_type');
    Pointer(png_get_pixels_per_meter) := PngLibrary.Symbol('png_get_pixels_per_meter');
    Pointer(png_get_x_pixels_per_meter) := PngLibrary.Symbol('png_get_x_pixels_per_meter');
    Pointer(png_get_y_pixels_per_meter) := PngLibrary.Symbol('png_get_y_pixels_per_meter');
    Pointer(png_get_pixel_aspect_ratio) := PngLibrary.Symbol('png_get_pixel_aspect_ratio');
    Pointer(png_get_x_offset_pixels) := PngLibrary.Symbol('png_get_x_offset_pixels');
    Pointer(png_get_y_offset_pixels) := PngLibrary.Symbol('png_get_y_offset_pixels');
    Pointer(png_get_x_offset_microns) := PngLibrary.Symbol('png_get_x_offset_microns');
    Pointer(png_get_y_offset_microns) := PngLibrary.Symbol('png_get_y_offset_microns');
    Pointer(png_get_signature) := PngLibrary.Symbol('png_get_signature');
    Pointer(png_get_bKGD) := PngLibrary.Symbol('png_get_bKGD');
    Pointer(png_set_bKGD) := PngLibrary.Symbol('png_set_bKGD');
    Pointer(png_get_cHRM) := PngLibrary.Symbol('png_get_cHRM');
    Pointer(png_get_cHRM_fixed) := PngLibrary.Symbol('png_get_cHRM_fixed');
    Pointer(png_set_cHRM) := PngLibrary.Symbol('png_set_cHRM');
    Pointer(png_set_cHRM_fixed) := PngLibrary.Symbol('png_set_cHRM_fixed');
    Pointer(png_get_gAMA) := PngLibrary.Symbol('png_get_gAMA');
    Pointer(png_get_gAMA_fixed) := PngLibrary.Symbol('png_get_gAMA_fixed');
    Pointer(png_set_gAMA) := PngLibrary.Symbol('png_set_gAMA');
    Pointer(png_set_gAMA_fixed) := PngLibrary.Symbol('png_set_gAMA_fixed');
    Pointer(png_get_hIST) := PngLibrary.Symbol('png_get_hIST');
    Pointer(png_set_hIST) := PngLibrary.Symbol('png_set_hIST');
    Pointer(png_get_IHDR) := PngLibrary.Symbol('png_get_IHDR');
    Pointer(png_set_IHDR) := PngLibrary.Symbol('png_set_IHDR');
    Pointer(png_get_oFFs) := PngLibrary.Symbol('png_get_oFFs');
    Pointer(png_set_oFFs) := PngLibrary.Symbol('png_set_oFFs');
    Pointer(png_get_pCAL) := PngLibrary.Symbol('png_get_pCAL');
    Pointer(png_set_pCAL) := PngLibrary.Symbol('png_set_pCAL');
    Pointer(png_get_pHYs) := PngLibrary.Symbol('png_get_pHYs');
    Pointer(png_set_pHYs) := PngLibrary.Symbol('png_set_pHYs');
    Pointer(png_get_PLTE) := PngLibrary.Symbol('png_get_PLTE');
    Pointer(png_set_PLTE) := PngLibrary.Symbol('png_set_PLTE');
    Pointer(png_get_sBIT) := PngLibrary.Symbol('png_get_sBIT');
    Pointer(png_set_sBIT) := PngLibrary.Symbol('png_set_sBIT');
    Pointer(png_get_sRGB) := PngLibrary.Symbol('png_get_sRGB');
    Pointer(png_set_sRGB) := PngLibrary.Symbol('png_set_sRGB');
    Pointer(png_set_sRGB_gAMA_and_cHRM) := PngLibrary.Symbol('png_set_sRGB_gAMA_and_cHRM');
    Pointer(png_get_iCCP) := PngLibrary.Symbol('png_get_iCCP');
    Pointer(png_set_iCCP) := PngLibrary.Symbol('png_set_iCCP');
    Pointer(png_get_sPLT) := PngLibrary.Symbol('png_get_sPLT');
    Pointer(png_set_sPLT) := PngLibrary.Symbol('png_set_sPLT');
    Pointer(png_get_text) := PngLibrary.Symbol('png_get_text');
    Pointer(png_set_text) := PngLibrary.Symbol('png_set_text');
    Pointer(png_get_tIME) := PngLibrary.Symbol('png_get_tIME');
    Pointer(png_set_tIME) := PngLibrary.Symbol('png_set_tIME');
    Pointer(png_get_tRNS) := PngLibrary.Symbol('png_get_tRNS');
    Pointer(png_set_tRNS) := PngLibrary.Symbol('png_set_tRNS');
    Pointer(png_get_sCAL) := PngLibrary.Symbol('png_get_sCAL');
    Pointer(png_set_sCAL) := PngLibrary.Symbol('png_set_sCAL');
    Pointer(png_set_keep_unknown_chunks) := PngLibrary.Symbol('png_set_keep_unknown_chunks');
    Pointer(png_set_unknown_chunks) := PngLibrary.Symbol('png_set_unknown_chunks');
    Pointer(png_set_unknown_chunk_location) := PngLibrary.Symbol('png_set_unknown_chunk_location');
    Pointer(png_get_unknown_chunks) := PngLibrary.Symbol('png_get_unknown_chunks');
    Pointer(png_set_invalid) := PngLibrary.Symbol('png_set_invalid');
    Pointer(png_read_png) := PngLibrary.Symbol('png_read_png');
    Pointer(png_write_png) := PngLibrary.Symbol('png_write_png');
    Pointer(png_get_header_ver) := PngLibrary.Symbol('png_get_header_ver');
    Pointer(png_get_header_version) := PngLibrary.Symbol('png_get_header_version');

    { Allow png_set_expand_gray_1_2_4_to_8 to be nil when not found in library }
    PngLibrary.SymbolErrorBehaviour := seReturnNil;
    Pointer(png_set_expand_gray_1_2_4_to_8) := PngLibrary.Symbol('png_set_expand_gray_1_2_4_to_8');
    PngLibrary.SymbolErrorBehaviour := seRaise;

    {$ifdef LIBPNG_DEPRECATED}
    Pointer(png_check_sig) := PngLibrary.Symbol('png_check_sig');
    Pointer(png_info_init) := PngLibrary.Symbol('png_info_init');
    Pointer(png_set_gray_1_2_4_to_8) := PngLibrary.Symbol('png_set_gray_1_2_4_to_8');
    Pointer(png_permit_empty_plte) := PngLibrary.Symbol('png_permit_empty_plte');
    Pointer(png_memcpy_check) := PngLibrary.Symbol('png_memcpy_check');
    Pointer(png_memset_check) := PngLibrary.Symbol('png_memset_check');
    Pointer(png_read_destroy) := PngLibrary.Symbol('png_read_destroy');
    Pointer(png_write_destroy_info) := PngLibrary.Symbol('png_write_destroy_info');
    Pointer(png_write_destroy) := PngLibrary.Symbol('png_write_destroy');
    Pointer(png_set_sCAL_s) := PngLibrary.Symbol('png_set_sCAL_s');
    Pointer(png_set_dither) := PngLibrary.Symbol('png_set_dither');
    {$endif LIBPNG_DEPRECATED}
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  PngInitialization;
  {$endif}
finalization
  FreeAndNil(PngLibrary);
end.
