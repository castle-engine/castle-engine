{
  This file is based on the Free Pascal run time library,
  with various adjustments for Castle Game Engine.

  Copyright by the Free Pascal development team and Michalis Kamburelis.

  License:
  This file is adapted from the FPC RTL source code, as such
  the license and copyright information of FPC RTL applies here.
  That said, the license of FPC RTL happens to be *exactly*
  the same as used by the "Castle Game Engine": LGPL (version 2.1)
  with "static linking exception" (with exactly the same wording
  of the "static linking exception").
  See the file COPYING.txt, included in this distribution, for details about
  the copyright of "Castle Game Engine".
  See http://www.freepascal.org/faq.var#general-license about the copyright
  of FPC RTL.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ FreeType library routines.

  Based on FPC RTL, with some modifications:

  - Load dynamic library using TDynLib, and merely expose
    FreeTypeLibraryInitialized = false when library not found.
    TFontManager.Create will raise an exception if freetype library is not
    installed, and it can be handled gracefully.

  - Honor ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION symbol.

  - Optionally load staticaly, if CASTLE_FREETYPE_STATIC is defined.
    It is automatically defined on iOS now
    (matching Castle Game Engine "freetype" service on iOS,
    that links freetype statically into the main library).

  - Define some things we need (or needed in the past) in CGE:
    FT_New_Memory_Face, FT_Open_Face and friends. }
unit CastleInternalFreeTypeH;

{$i castleconf.inc}

{$ifdef CASTLE_IOS}
  {$define CASTLE_FREETYPE_STATIC}
{$endif}

interface

uses CTypes;

{$ifdef FPC}{$packrecords c}{$endif}

type
  FT_Encoding = array[0..3] of Ansichar;

const
  FT_FACE_FLAG_SCALABLE = 1 shl 0;
  FT_FACE_FLAG_FIXED_SIZES = 1 shl 1;
  FT_FACE_FLAG_FIXED_WIDTH = 1 shl 2;
  FT_FACE_FLAG_SFNT = 1 shl 3;
  FT_FACE_FLAG_HORIZONTAL = 1 shl 4;
  FT_FACE_FLAG_VERTICAL = 1 shl 5;
  FT_FACE_FLAG_KERNING = 1 shl 6;
  FT_FACE_FLAG_FAST_GLYPHS = 1 shl 7;
  FT_FACE_FLAG_MULTIPLE_MASTERS = 1 shl 8;
  FT_FACE_FLAG_GLYPH_NAMES = 1 shl 9;
  FT_FACE_FLAG_EXTERNAL_STREAM = 1 shl 10;

  FT_STYLE_FLAG_ITALIC = 1 shl 0;
  FT_STYLE_FLAG_BOLD = 1 shl 1;

  FT_LOAD_DEFAULT =          $0000;
  FT_LOAD_NO_SCALE =         $0001;
  FT_LOAD_NO_HINTING =       $0002;
  FT_LOAD_RENDER =           $0004;
  FT_LOAD_NO_BITMAP =        $0008;
  FT_LOAD_VERTICAL_LAYOUT =  $0010;
  FT_LOAD_FORCE_AUTOHINT =   $0020;
  FT_LOAD_CROP_BITMAP =      $0040;
  FT_LOAD_PEDANTIC =         $0080;
  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = $0200;
  FT_LOAD_NO_RECURSE =       $0400;
  FT_LOAD_IGNORE_TRANSFORM = $0800;
  FT_LOAD_MONOCHROME =       $1000;
  FT_LOAD_LINEAR_DESIGN =    $2000;

  ft_glyph_format_none      = $00000000;
  ft_glyph_format_composite = $636F6D70; //comp 099 111 109 112
  ft_glyph_format_bitmap    = $62697473; //bits 098 105 116 115
  ft_glyph_format_outline   = $6F75746C; //outl 111 117 116 108
  ft_glyph_format_plotter   = $706C6F74; //plot 112 108 111 116

  FT_ENCODING_MS_SYMBOL : FT_Encoding = 'symb';
  FT_ENCODING_UNICODE : FT_Encoding = 'unic';
  FT_ENCODING_MS_SJIS : FT_Encoding = 'sjis';
  FT_ENCODING_MS_GB2312 : FT_Encoding = 'gb  ';
  FT_ENCODING_MS_BIG5 : FT_Encoding = 'big5';
  FT_ENCODING_MS_WANSUNG : FT_Encoding = 'wans';
  FT_ENCODING_MS_JOHAB : FT_Encoding = 'joha';
  FT_ENCODING_ADOBE_STANDARD : FT_Encoding = 'ADOB';
  FT_ENCODING_ADOBE_EXPERT : FT_Encoding = 'ADBE';
  FT_ENCODING_ADOBE_CUSTOM : FT_Encoding = 'ADBC';
  FT_ENCODING_ADOBE_LATIN_1 : FT_Encoding = 'lat1';
  FT_ENCODING_OLD_LATIN_2 : FT_Encoding = 'lat2';
  FT_ENCODING_APPLE_ROMAN : FT_Encoding = 'armn';

  ft_glyph_bbox_unscaled  = 0; //* return unscaled font units           */
  ft_glyph_bbox_subpixels = 0; //* return unfitted 26.6 coordinates     */
  ft_glyph_bbox_gridfit   = 1; //* return grid-fitted 26.6 coordinates  */
  ft_glyph_bbox_truncate  = 2; //* return coordinates in integer pixels */
  ft_glyph_bbox_pixels    = 3; //* return grid-fitted pixel coordinates */

  FT_KERNING_DEFAULT  = 0;
  FT_KERNING_UNFITTED = 1;
  FT_KERNING_UNSCALED = 2;

  {**************************************************************************
   *
   * @enum:
   *   FT_OPEN_XXX
   *
   * @description:
   *   A list of bit field constants used within the `flags` field of the
   *   @FT_Open_Args structure.
   *
   * @values:
   *   FT_OPEN_MEMORY ::
   *     This is a memory-based stream.
   *
   *   FT_OPEN_STREAM ::
   *     Copy the stream from the `stream` field.
   *
   *   FT_OPEN_PATHNAME ::
   *     Create a new input stream from a C~path name.
   *
   *   FT_OPEN_DRIVER ::
   *     Use the `driver` field.
   *
   *   FT_OPEN_PARAMS ::
   *     Use the `num_params` and `params` fields.
   *
   * @note:
   *   The `FT_OPEN_MEMORY`, `FT_OPEN_STREAM`, and `FT_OPEN_PATHNAME` flags
   *   are mutually exclusive.
   *}
  FT_OPEN_MEMORY    = $1;
  FT_OPEN_STREAM    = $2;
  FT_OPEN_PATHNAME  = $4;
  FT_OPEN_DRIVER    = $8;
  FT_OPEN_PARAMS    = $10;

type
  FT_Bool = boolean;
  FT_FWord = smallint;
  FT_UFWord = word;
  FT_Char = AnsiChar;
  FT_Byte = byte;
  FT_String = Ansichar;
  PFT_String = PAnsiChar;
  FT_Short = smallint;
  FT_UShort = word;
  FT_Int = CInt32;
  FT_UInt = CUInt32;
  {$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  FT_Long = int64;
  FT_ULong = {$ifdef FPC} qword {$else} UInt64 {$endif};
  FT_Pos = int64;
  {$ELSE}
  FT_Long = CInt32;
  FT_ULong = CUInt32;
  FT_Pos = CInt32;
  {$ENDIF}
  FT_F2Dot14 = smallint;
  FT_F26Dot6 = CInt32;
  FT_Fixed = FT_Long;
  FT_Error = CInt32;
  FT_Pointer = pointer;
  //FT_Offset = size_t;
  //FT_PtrDist = size_t;

  {$ifndef FPC}{$MinEnumSize 4}{$endif}
  FT_Render_Mode = (FT_RENDER_MODE_NORMAL = 0, FT_RENDER_MODE_LIGHT,
      FT_RENDER_MODE_MONO, FT_RENDER_MODE_LCD, FT_RENDER_MODE_LCD_V,
      FT_RENDER_MODE_MAX);
  {$ifndef FPC}{$MinEnumSize 1}{$endif}

  FT_UnitVector_ = record
      x : FT_F2Dot14;
      y : FT_F2Dot14;
   end;
  FT_UnitVector = FT_UnitVector_;

  FT_Matrix = record
      xx : FT_Fixed;
      xy : FT_Fixed;
      yx : FT_Fixed;
      yy : FT_Fixed;
   end;
  PFT_Matrix = ^FT_Matrix;

  FT_Data = record
      pointer : ^FT_Byte;
      length : FT_Int;
   end;

  FT_Generic_Finalizer = procedure (AnObject:pointer);cdecl;

  FT_Generic = record
      data : pointer;
      finalizer : FT_Generic_Finalizer;
   end;

  FT_Glyph_Metrics = record
    width : FT_Pos;
    height : FT_Pos;
    horiBearingX : FT_Pos;
    horiBearingY : FT_Pos;
    horiAdvance : FT_Pos;
    vertBearingX : FT_Pos;
    vertBearingY : FT_Pos;
    vertAdvance : FT_Pos;
  end;

  FT_Bitmap_Size = record
    height : FT_Short;
    width : FT_Short;
  end;
  AFT_Bitmap_Size = array [0..1023] of FT_Bitmap_Size;
  PFT_Bitmap_Size = ^AFT_Bitmap_Size;

  FT_Vector = record
    x : FT_Pos;
    y : FT_Pos;
  end;
  PFT_Vector = ^FT_Vector;

  FT_BBox = record
    xMin, yMin : FT_Pos;
    xMax, yMax : FT_Pos;
  end;
  PFT_BBox = ^FT_BBox;

  FT_Bitmap = record
    rows : integer;
    width : integer;
    pitch : integer;
    buffer : pointer;
    num_grays : shortint;
    pixel_mode : Ansichar;
    palette_mode : Ansichar;
    palette : pointer;
  end;

  FT_Outline = record
    n_contours,
    n_points : smallint;
    points : PFT_Vector;
    tags : PAnsiChar;
    contours : ^smallint;
    flags : integer;
  end;
  PFT_Outline = ^FT_Outline;

  FT_Outline_MoveToFunc = function(const to_: PFT_Vector; user: Pointer): integer; cdecl;
  FT_Outline_LineToFunc = function(const to_: PFT_Vector; user: Pointer): integer; cdecl;
  FT_Outline_ConicToFunc = function(const control, to_: PFT_Vector; user: Pointer): integer; cdecl;
  FT_Outline_CubicToFunc = function(const control1, control2, to_: PFT_Vector; user: Pointer): integer; cdecl;

  FT_Outline_Funcs = record
    move_to: FT_Outline_MoveToFunc;
    line_to: FT_Outline_LineToFunc;
    conic_to: FT_Outline_ConicToFunc;
    cubic_to: FT_Outline_CubicToFunc;
    shift: integer;
    delta: FT_Pos;
  end;
  PFT_Outline_Funcs = ^FT_Outline_Funcs;

  FT_Size_Metrics = record
    x_ppem : FT_UShort;
    y_ppem : FT_UShort;
    x_scale : FT_Fixed;
    y_scale : FT_Fixed;
    ascender : FT_Pos;
    descender : FT_Pos;
    height : FT_Pos;
    max_advance : FT_Pos;
  end;


  PFT_Library = ^TFT_Library;
  //PPFT_Library = ^PFT_Library;
  PFT_Face = ^TFT_Face;
  //PPFT_Face = ^PFT_Face;
  PFT_Charmap = ^TFT_Charmap;
  PPFT_Charmap = ^PFT_Charmap;
  PFT_GlyphSlot = ^TFT_GlyphSlot;
  PFT_Subglyph = ^TFT_Subglyph;
  PFT_Size = ^TFT_Size;

  PFT_Glyph = ^TFT_Glyph;
  //PPFT_Glyph = ^PFT_Glyph;
  PFT_BitmapGlyph = ^TFT_BitmapGlyph;
  PFT_OutlineGlyph = ^TFT_OutlineGlyph;


  TFT_Library = record
  end;

  TFT_Charmap = record
    face : PFT_Face;
    encoding : FT_Encoding;
    platform_id, encoding_id : FT_UShort;
  end;

  TFT_Size = record
    face : PFT_Face;
    generic : FT_Generic;
    metrics : FT_Size_Metrics;
    //internal : FT_Size_Internal;
  end;

  TFT_Subglyph = record  // TODO
  end;

  TFT_GlyphSlot = record
    alibrary : PFT_Library;
    face : PFT_Face;
    next : PFT_GlyphSlot;
    flags : FT_UInt;
    generic : FT_Generic;
    metrics : FT_Glyph_Metrics;
    linearHoriAdvance : FT_Fixed;
    linearVertAdvance : FT_Fixed;
    advance : FT_Vector;
    format : CUInt32;
    bitmap : FT_Bitmap;
    bitmap_left : FT_Int;
    bitmap_top : FT_Int;
    outline : FT_Outline;
    num_subglyphs : FT_UInt;
    subglyphs : PFT_SubGlyph;
    control_data : pointer;
    control_len : CInt32;
    other : pointer;
  end;

  TFT_Face = record
    num_faces : FT_Long;
    face_index : FT_Long;
    face_flags : FT_Long;
    style_flags : FT_Long;
    num_glyphs : FT_Long;
    family_name : PAnsiChar;
    style_name : PAnsiChar;
    num_fixed_sizes : FT_Int;
    available_sizes : PFT_Bitmap_Size;     // is array
    num_charmaps : FT_Int;
    charmaps : PPFT_CharMap;               // is array
    generic : FT_Generic;
    bbox : FT_BBox;
    units_per_EM : FT_UShort;
    ascender : FT_Short;
    descender : FT_Short;
    height : FT_Short;
    max_advance_width : FT_Short;
    max_advance_height : FT_Short;
    underline_position : FT_Short;
    underline_thickness : FT_Short;
    glyph : PFT_GlyphSlot;
    size : PFT_Size;
    charmap : PFT_CharMap;
  end;

  TFT_Glyph = record
    FTlibrary : PFT_Library;
    clazz : pointer;
    aFormat : CUInt32;
    advance : FT_Vector;
  end;

  TFT_BitmapGlyph = record
    root : TFT_Glyph;
    left, top : FT_Int;
    bitmap : FT_Bitmap;
  end;

  TFT_OutlineGlyph = record
    root : TFT_Glyph;
    outline : FT_Outline;
  end;

  { Structure that describes a custom stream, used e.g. with FT_Open_Face .
    See https://freetype.org/freetype2/docs/reference/ft2-base_interface.html#ft_open_args }
  TFT_Open_Args = record
    flags: FT_UInt;
    memory_base: ^FT_Byte;
    memory_size: FT_Long;
    pathname: PFT_String;
    stream: Pointer; // FT_Stream; TODO: Not translated yet
    driver: Pointer; // FT_Module; TODO: Not translated yet
    num_params: FT_Int;
    params: Pointer; // FT_Parameter*; TODO: Not translated yet
  end;
  PFT_Open_Args = ^TFT_Open_Args;

{$ifdef CASTLE_FREETYPE_STATIC}

//Base Interface
function FT_Done_Face(face: PFT_Face): integer; cdecl; external;
function FT_Done_FreeType(alibrary: PFT_Library): integer; cdecl; external;
function FT_Get_Char_Index(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl; external;
function FT_Get_Kerning(face: PFT_Face; left_glyph, right_glyph, kern_mode: FT_UInt; out akerning: FT_Vector): integer; cdecl; external;
function FT_Init_FreeType(var alibrary: PFT_Library): integer; cdecl; external;
function FT_Load_Char(face: PFT_Face; charcode: FT_ULong; load_flags: CInt32): integer; cdecl; external;
function FT_Load_Glyph(face: PFT_Face; glyph_index: FT_UInt; load_flags: CInt32): integer; cdecl; external;
{ TODO:
  Is this right?
  - face_index should be FT_Long (makes different on 64-bit non-Windows)? }
function FT_New_Face(alibrary: PFT_Library; filepathname: PAnsiChar; face_index: integer; var aface: PFT_Face): integer; cdecl; external;
function FT_Open_Face(alibrary: PFT_Library; Args: PFT_Open_Args; face_index: FT_Long; var aface: PFT_Face): integer; cdecl; external;
function FT_New_Memory_Face(alibrary: PFT_Library; file_base: Pointer; file_size: FT_Long; face_index: FT_Long; var aface: PFT_Face): integer; cdecl; external;
function FT_Set_Char_Size(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl; external;
function FT_Set_Pixel_Sizes(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl; external;
procedure FT_Set_Transform(face: PFT_Face; matrix: PFT_Matrix; delta: PFT_Vector); cdecl; external;

//Outline Processing
function FT_Outline_Decompose(outline: PFT_Outline; const func_interface: PFT_Outline_Funcs; user: Pointer): integer; cdecl; external;

//FreeType Version
procedure FT_Library_Version(alibrary: PFT_Library; var amajor, aminor, apatch: integer); cdecl; external;

//Glyph Management
function FT_Get_Glyph(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl; external;
function FT_Glyph_Copy(Source: PFT_Glyph; out target: PFT_Glyph): integer; cdecl; external;
function FT_Glyph_To_Bitmap(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl; external;
function FT_Glyph_Transform(glyph: PFT_Glyph; matrix: PFT_Matrix; delta: PFT_Vector): integer; cdecl; external;
procedure FT_Done_Glyph(glyph: PFT_Glyph); cdecl; external;
procedure FT_Glyph_Get_CBox(glyph: PFT_Glyph; bbox_mode: FT_UInt; var acbox: FT_BBox); cdecl; external;

{$else}

var
//Base Interface
  FT_Done_Face: function(face: PFT_Face): integer; cdecl;
  FT_Done_FreeType: function(alibrary: PFT_Library): integer; cdecl;
  FT_Get_Char_Index: function(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl;
  FT_Get_Kerning: function(face: PFT_Face; left_glyph, right_glyph, kern_mode: FT_UInt; out akerning: FT_Vector): integer; cdecl;
  FT_Init_FreeType: function(var alibrary: PFT_Library): integer; cdecl;
  FT_Load_Char: function(face: PFT_Face; charcode: FT_ULong; load_flags: CInt32): integer; cdecl;
  FT_Load_Glyph: function(face: PFT_Face; glyph_index: FT_UInt; load_flags: CInt32): integer; cdecl;
  FT_New_Face: function(alibrary: PFT_Library; filepathname: PAnsiChar; face_index: integer; var aface: PFT_Face): integer; cdecl;
  FT_Open_Face: function(alibrary: PFT_Library; Args: PFT_Open_Args; face_index: FT_Long; var aface: PFT_Face): integer; cdecl;
  FT_New_Memory_Face: function(alibrary: PFT_Library; file_base: Pointer; file_size: FT_Long; face_index: FT_Long; var aface: PFT_Face): integer; cdecl;
  FT_Set_Char_Size: function(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl;
  FT_Set_Pixel_Sizes: function(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl;
  FT_Set_Transform: procedure(face: PFT_Face; matrix: PFT_Matrix; delta: PFT_Vector); cdecl;

//Outline Processing
  FT_Outline_Decompose: function(outline: PFT_Outline; const func_interface: PFT_Outline_Funcs; user: Pointer): integer; cdecl;

//FreeType Version
  FT_Library_Version: procedure(alibrary: PFT_Library; var amajor, aminor, apatch: integer); cdecl;

//Glyph Management
  FT_Get_Glyph: function(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl;
  FT_Glyph_Copy: function(Source: PFT_Glyph; out target: PFT_Glyph): integer; cdecl;
  FT_Glyph_To_Bitmap: function(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl;
  FT_Glyph_Transform: function(glyph: PFT_Glyph; matrix: PFT_Matrix; delta: PFT_Vector): integer; cdecl;
  FT_Done_Glyph: procedure(glyph: PFT_Glyph); cdecl;
  FT_Glyph_Get_CBox: procedure(glyph: PFT_Glyph; bbox_mode: FT_UInt; var acbox: FT_BBox); cdecl;
{$endif CASTLE_FREETYPE_STATIC}

//Base Interface - macros
function FT_IS_SCALABLE(face: PFT_Face): boolean;

procedure LoadFreeTypeLibrary;

{ Did we found FreeType library and loaded it's symbols.
  When this is @false, do not use any functions from this unit, they are @nil. }
function FreeTypeLibraryInitialized: boolean;

implementation

uses SysUtils, CastleDynLib;

{ FreeType macros ------------------------------------------------------------ }

function FT_IS_SCALABLE(face: PFT_Face): boolean;
begin
  Result := (face^.face_flags and FT_FACE_FLAG_SCALABLE) = 1;
end;

{ Loading FreeType library --------------------------------------------------- }

{$ifdef CASTLE_FREETYPE_STATIC}

procedure LoadFreeTypeLibrary;
begin
end;

function FreeTypeLibraryInitialized: boolean;
begin
  Result := true;
end;

end.
{$else CASTLE_FREETYPE_STATIC}

var
  FreeTypeLibrary: TDynLib;

function FreeTypeLibraryInitialized: boolean;
begin
  Result := FreeTypeLibrary <> nil;
end;

procedure LoadFreeTypeLibrary;
begin
  // Windows
  {$ifdef MSWINDOWS}
    {$define ft_found_platform}
    FreeTypeLibrary := TDynLib.Load('freetype.dll', false);
    if FreeTypeLibrary = nil then
      FreeTypeLibrary := TDynLib.Load('freetype-6.dll', false);
  {$endif}

  // macOS
  {$ifdef DARWIN}
    {$define ft_found_platform}
    FreeTypeLibrary := TDynLib.Load('libfreetype.dylib', false);
    if FreeTypeLibrary = nil then
      FreeTypeLibrary := TDynLib.Load('libfreetype.6.dylib', false);
    // try in typical location
    if FreeTypeLibrary = nil then
      FreeTypeLibrary := TDynLib.Load('/usr/X11/lib/libfreetype.dylib', false);
    if FreeTypeLibrary = nil then
      FreeTypeLibrary := TDynLib.Load('/usr/X11/lib/libfreetype.dylib.6', false);
  {$endif}

  // UNIX
  {$if defined(UNIX) and not defined(darwin)}
    {$define ft_found_platform}
    FreeTypeLibrary := TDynLib.Load('libfreetype.so', false);
    if FreeTypeLibrary = nil then
      FreeTypeLibrary := TDynLib.Load('libfreetype.so.6', false);
  {$endif}

  if FreeTypeLibrary <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} FT_Done_Face) := FreeTypeLibrary.Symbol('FT_Done_Face');
    Pointer({$ifndef FPC}@{$endif} FT_Done_FreeType) := FreeTypeLibrary.Symbol('FT_Done_FreeType');
    Pointer({$ifndef FPC}@{$endif} FT_Get_Char_Index) := FreeTypeLibrary.Symbol('FT_Get_Char_Index');
    Pointer({$ifndef FPC}@{$endif} FT_Get_Kerning) := FreeTypeLibrary.Symbol('FT_Get_Kerning');
    Pointer({$ifndef FPC}@{$endif} FT_Init_FreeType) := FreeTypeLibrary.Symbol('FT_Init_FreeType');
    Pointer({$ifndef FPC}@{$endif} FT_Load_Char) := FreeTypeLibrary.Symbol('FT_Load_Char');
    Pointer({$ifndef FPC}@{$endif} FT_Load_Glyph) := FreeTypeLibrary.Symbol('FT_Load_Glyph');
    Pointer({$ifndef FPC}@{$endif} FT_New_Face) := FreeTypeLibrary.Symbol('FT_New_Face');
    Pointer({$ifndef FPC}@{$endif} FT_Open_Face) := FreeTypeLibrary.Symbol('FT_Open_Face');
    Pointer({$ifndef FPC}@{$endif} FT_New_Memory_Face) := FreeTypeLibrary.Symbol('FT_New_Memory_Face');
    Pointer({$ifndef FPC}@{$endif} FT_Set_Char_Size) := FreeTypeLibrary.Symbol('FT_Set_Char_Size');
    Pointer({$ifndef FPC}@{$endif} FT_Set_Pixel_Sizes) := FreeTypeLibrary.Symbol('FT_Set_Pixel_Sizes');
    Pointer({$ifndef FPC}@{$endif} FT_Set_Transform) := FreeTypeLibrary.Symbol('FT_Set_Transform');
    Pointer({$ifndef FPC}@{$endif} FT_Outline_Decompose) := FreeTypeLibrary.Symbol('FT_Outline_Decompose');
    Pointer({$ifndef FPC}@{$endif} FT_Library_Version) := FreeTypeLibrary.Symbol('FT_Library_Version');
    Pointer({$ifndef FPC}@{$endif} FT_Get_Glyph) := FreeTypeLibrary.Symbol('FT_Get_Glyph');
    Pointer({$ifndef FPC}@{$endif} FT_Glyph_Copy) := FreeTypeLibrary.Symbol('FT_Glyph_Copy');
    Pointer({$ifndef FPC}@{$endif} FT_Glyph_To_Bitmap) := FreeTypeLibrary.Symbol('FT_Glyph_To_Bitmap');
    Pointer({$ifndef FPC}@{$endif} FT_Glyph_Transform) := FreeTypeLibrary.Symbol('FT_Glyph_Transform');
    Pointer({$ifndef FPC}@{$endif} FT_Done_Glyph) := FreeTypeLibrary.Symbol('FT_Done_Glyph');
    Pointer({$ifndef FPC}@{$endif} FT_Glyph_Get_CBox) := FreeTypeLibrary.Symbol('FT_Glyph_Get_CBox');
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  LoadFreeTypeLibrary;
  {$endif}
finalization
  FreeAndNil(FreeTypeLibrary);
end.

{$endif CASTLE_FREETYPE_STATIC}
