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
  See the file COPYING.md, included in this distribution, for details about
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

  - If not defined CASTLE_FREETYPE_DISABLE_DYNAMIC,
    we load dynamic library using TDynLib.

  - We fallback on Pascal-only FreeType implementation from
    CastleInternalFreeTypeHPascal if dynamic library loading fails.
    Or if CASTLE_FREETYPE_DISABLE_DYNAMIC was defined.

  - Honor ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION symbol.

  - Optionally load staticaly, if CASTLE_FREETYPE_STATIC is defined.
    It is automatically defined on iOS now
    (matching Castle Game Engine "freetype" service on iOS,
    that links freetype statically into the main library).

  - Declare only the subset of FreeType API that is actually used by CGE
    (by CastleInternalFreeType unit). }
unit CastleInternalFreeTypeH;

{$i castleconf.inc}

// FPC + iOS deployment always links with static FreeType library.
{$if defined(CASTLE_IOS) and defined(FPC)}
  {$define CASTLE_FREETYPE_STATIC}
{$endif}

interface

uses CTypes;

{$ifdef FPC}{$packrecords c}{$endif}

type
  FT_Encoding = array[0..3] of Ansichar;

const
  { Values for TFT_Face.face_flags.
    Note: Only the flags used by CGE are defined here. }
  FT_FACE_FLAG_SCALABLE = 1 shl 0;
  FT_FACE_FLAG_FIXED_SIZES = 1 shl 1;
  FT_FACE_FLAG_SFNT = 1 shl 3;
  FT_FACE_FLAG_HORIZONTAL = 1 shl 4;
  FT_FACE_FLAG_KERNING = 1 shl 6;

  FT_STYLE_FLAG_ITALIC = 1 shl 0;
  FT_STYLE_FLAG_BOLD = 1 shl 1;

  FT_LOAD_DEFAULT = $0000;

  ft_glyph_format_bitmap    = $62697473; //bits 098 105 116 115
  ft_glyph_format_outline   = $6F75746C; //outl 111 117 116 108

  { Special value returned by FT_Library_Version for the patch version
    of the Pascal implementation.
    Chosen arbitrarily, just a crazy enough number that no real FreeType
    version would use it. }
  FtPatchVersionPascalImplementation = 999888777;

type
  FT_Bool = boolean;
  FT_Short = CShort;
  FT_UShort = CUShort;
  FT_Int = CInt;
  FT_UInt = CUInt;
  { Note that C "long" is either 64-bit or 32-bit on 2 common platforms:
    - 64-bit on: defined(cpu64) and not(defined(win64) and defined(cpux86_64))
    - 32-bit on 32-bit processors
    - (see CTypes for exact definition accounting also for 16-bit).
    This is expected by FreeType definitions. }
  FT_Long = CLong;
  FT_ULong = CULong;
  FT_Pos = CLong;
  FT_F26Dot6 = CInt32;
  FT_Fixed = FT_Long;

  {$ifndef FPC}{$MinEnumSize 4}{$endif}
  FT_Render_Mode = (FT_RENDER_MODE_NORMAL = 0, FT_RENDER_MODE_LIGHT,
      FT_RENDER_MODE_MONO, FT_RENDER_MODE_LCD, FT_RENDER_MODE_LCD_V,
      FT_RENDER_MODE_MAX);
  {$ifndef FPC}{$MinEnumSize 1}{$endif}

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

  FT_Bitmap = record
    rows : integer;
    width : integer;
    pitch : integer;
    buffer : pointer;
    num_grays : CUShort;
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
  PFT_Face = ^TFT_Face;
  PFT_Charmap = ^TFT_Charmap;
  PPFT_Charmap = ^PFT_Charmap;
  PFT_GlyphSlot = ^TFT_GlyphSlot;
  PFT_Subglyph = ^TFT_Subglyph;
  PFT_Size = ^TFT_Size;

  PFT_Glyph = ^TFT_Glyph;
  PFT_BitmapGlyph = ^TFT_BitmapGlyph;


  TFT_Library = record
  end;

  TFT_Charmap = record
    face : PFT_Face;
    encoding : FT_Encoding;
    platform_id, encoding_id : FT_UShort;
  end;

  TFT_Size = record
    face : PFT_Face;
    &generic : FT_Generic;
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
    &generic : FT_Generic;
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
    &generic : FT_Generic;
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

{$if defined(CASTLE_FREETYPE_STATIC)}

function FT_Done_Face(face: PFT_Face): integer; cdecl; external;
function FT_Done_FreeType(alibrary: PFT_Library): integer; cdecl; external;
function FT_Get_Char_Index(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl; external;
function FT_Init_FreeType(var alibrary: PFT_Library): integer; cdecl; external;
function FT_Load_Glyph(face: PFT_Face; glyph_index: FT_UInt; load_flags: CInt32): integer; cdecl; external;
function FT_New_Memory_Face(alibrary: PFT_Library; file_base: Pointer; file_size: FT_Long; face_index: FT_Long; var aface: PFT_Face): integer; cdecl; external;
function FT_Set_Char_Size(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl; external;
function FT_Set_Pixel_Sizes(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl; external;
procedure FT_Library_Version(alibrary: PFT_Library; out amajor, aminor, apatch: integer); cdecl; external;
function FT_Get_Glyph(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl; external;
function FT_Glyph_To_Bitmap(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl; external;
procedure FT_Done_Glyph(glyph: PFT_Glyph); cdecl; external;

{$else}

var
  FT_Done_Face: function(face: PFT_Face): integer; cdecl;
  FT_Done_FreeType: function(alibrary: PFT_Library): integer; cdecl;
  FT_Get_Char_Index: function(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl;
  FT_Init_FreeType: function(var alibrary: PFT_Library): integer; cdecl;
  FT_Load_Glyph: function(face: PFT_Face; glyph_index: FT_UInt; load_flags: CInt32): integer; cdecl;
  FT_New_Memory_Face: function(alibrary: PFT_Library; file_base: Pointer; file_size: FT_Long; face_index: FT_Long; var aface: PFT_Face): integer; cdecl;
  FT_Set_Char_Size: function(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl;
  FT_Set_Pixel_Sizes: function(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl;
  FT_Library_Version: procedure(alibrary: PFT_Library; out amajor, aminor, apatch: integer); cdecl;
  FT_Get_Glyph: function(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl;
  FT_Glyph_To_Bitmap: function(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl;
  FT_Done_Glyph: procedure(glyph: PFT_Glyph); cdecl;

{$endif}

{ Load a dynamic FreeType library, if needed.

  This does nothing (nothing is needed) when CASTLE_FREETYPE_STATIC.

  Otherwise, it will attempt to load the dynamic FreeType library
  (unless CASTLE_FREETYPE_DISABLE_DYNAMIC).
  It will fallback on CastleInternalFreeTypeHPascal
  if the dynamic library cannot be loaded.

  Effectively, this is always successful, because we either use the dynamic library
  if available, or fallback on CastleInternalFreeTypeHPascal.
  So after this, right now, @link(FreeTypeLibraryInitialized) will always
  return @true. }
procedure LoadFreeTypeLibrary;

{ Did we found FreeType library and loaded it's symbols. }
function FreeTypeLibraryInitialized: boolean;

implementation

uses SysUtils,
  {$ifndef CASTLE_FREETYPE_STATIC}
    {$ifndef CASTLE_FREETYPE_DISABLE_DYNAMIC}
    CastleDynLib,
    {$endif}
    CastleInternalFreeTypeHPascal,
  {$endif}
  CastleUtils;

{ Loading FreeType library --------------------------------------------------- }

{$if defined(CASTLE_FREETYPE_STATIC)}

procedure LoadFreeTypeLibrary;
begin
end;

function FreeTypeLibraryInitialized: boolean;
begin
  Result := true;
end;

end.

{$else}

{ Load all entry points from CastleInternalFreeTypeHPascal implementations. }
procedure LoadFreeTypeLibraryFromPascal;
begin
  FT_Done_Face := {$ifdef FPC}@{$endif} Pascal_FT_Done_Face;
  FT_Done_FreeType := {$ifdef FPC}@{$endif} Pascal_FT_Done_FreeType;
  FT_Get_Char_Index := {$ifdef FPC}@{$endif} Pascal_FT_Get_Char_Index;
  FT_Init_FreeType := {$ifdef FPC}@{$endif} Pascal_FT_Init_FreeType;
  FT_Load_Glyph := {$ifdef FPC}@{$endif} Pascal_FT_Load_Glyph;
  FT_New_Memory_Face := {$ifdef FPC}@{$endif} Pascal_FT_New_Memory_Face;
  FT_Set_Char_Size := {$ifdef FPC}@{$endif} Pascal_FT_Set_Char_Size;
  FT_Set_Pixel_Sizes := {$ifdef FPC}@{$endif} Pascal_FT_Set_Pixel_Sizes;
  FT_Library_Version := {$ifdef FPC}@{$endif} Pascal_FT_Library_Version;
  FT_Get_Glyph := {$ifdef FPC}@{$endif} Pascal_FT_Get_Glyph;
  FT_Glyph_To_Bitmap := {$ifdef FPC}@{$endif} Pascal_FT_Glyph_To_Bitmap;
  FT_Done_Glyph := {$ifdef FPC}@{$endif} Pascal_FT_Done_Glyph;
end;

function FreeTypeLibraryInitialized: boolean;
begin
  { Our LoadFreeTypeLibrary always makes this true,
    since it either uses dynamic library or our
    CastleInternalFreeTypeHPascal implementation.

    But let's make sure to return false before LoadFreeTypeLibrary
    was called. }
  Result := Assigned(FT_Done_Face);
end;

{$ifdef CASTLE_FREETYPE_DISABLE_DYNAMIC}

procedure LoadFreeTypeLibrary;
begin
  LoadFreeTypeLibraryFromPascal;
end;

initialization
  LoadFreeTypeLibrary;
end.

{$else not CASTLE_FREETYPE_DISABLE_DYNAMIC}

var
  FreeTypeLibrary: TDynLib;

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
    Pointer({$ifndef FPC}@{$endif} FT_Init_FreeType) := FreeTypeLibrary.Symbol('FT_Init_FreeType');
    Pointer({$ifndef FPC}@{$endif} FT_Load_Glyph) := FreeTypeLibrary.Symbol('FT_Load_Glyph');
    Pointer({$ifndef FPC}@{$endif} FT_New_Memory_Face) := FreeTypeLibrary.Symbol('FT_New_Memory_Face');
    Pointer({$ifndef FPC}@{$endif} FT_Set_Char_Size) := FreeTypeLibrary.Symbol('FT_Set_Char_Size');
    Pointer({$ifndef FPC}@{$endif} FT_Set_Pixel_Sizes) := FreeTypeLibrary.Symbol('FT_Set_Pixel_Sizes');
    Pointer({$ifndef FPC}@{$endif} FT_Library_Version) := FreeTypeLibrary.Symbol('FT_Library_Version');
    Pointer({$ifndef FPC}@{$endif} FT_Get_Glyph) := FreeTypeLibrary.Symbol('FT_Get_Glyph');
    Pointer({$ifndef FPC}@{$endif} FT_Glyph_To_Bitmap) := FreeTypeLibrary.Symbol('FT_Glyph_To_Bitmap');
    Pointer({$ifndef FPC}@{$endif} FT_Done_Glyph) := FreeTypeLibrary.Symbol('FT_Done_Glyph');
  end else
    { Fallback: Load entry points from Pascal implementation }
    LoadFreeTypeLibraryFromPascal;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  LoadFreeTypeLibrary;
  {$endif}
finalization
  FreeAndNil(FreeTypeLibrary);
end.

{$endif not CASTLE_FREETYPE_DISABLE_DYNAMIC}

{$endif not CASTLE_FREETYPE_STATIC}
