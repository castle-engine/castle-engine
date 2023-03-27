{%MainUnit freetypehdyn}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    freetype header to be included from static and dynamically loaded header.      

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Const

{$packrecords c}

// Windows
{$ifdef windows}
  FreeTypeDLL = 'freetype.dll';
  {$define ft_found_platform}
{$endif}

// Mac OS X
{$ifdef darwin}
  freetypedll = 'libfreetype.dylib'; // Doesn't seam to matter much.
{$IFNDEF DYNAMIC}
{$linklib freetype}  // This one is the important part,
                     // but you also need to pass to fpc
                     // the following command:
                     // -k-L/usr/X11/lib
                     // or another place where it can find
                     // libfreetype.dylib
{$ENDIF}
{$define ft_found_platform}
{$endif}

// LINUX
{$if defined(UNIX) and not defined(darwin)}
  FreeTypeDLL = 'libfreetype.so';
  {$define ft_found_platform}
{$endif}
// Other platforms
{$ifndef ft_found_platform}
  FreeTypeDLL = 'freetype';
{$endif}

type
  FT_Encoding = array[0..3] of char;

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


type

  FT_Bool = boolean;
  FT_FWord = smallint;
  FT_UFWord = word;
  FT_Char = char;
  FT_Byte = byte;
  FT_String = char;
  FT_Short = smallint;
  FT_UShort = word;
  FT_Int = longint;
  FT_UInt = longword;
  {$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  FT_Long = int64;
  FT_ULong = qword;
  FT_Pos = int64;
  {$ELSE}
  FT_Long = longint;
  FT_ULong = longword;
  FT_Pos = longint;
  {$ENDIF}
  FT_F2Dot14 = smallint;
  FT_F26Dot6 = longint;
  FT_Fixed = FT_Long;
  FT_Error = longint;
  FT_Pointer = pointer;
  //FT_Offset = size_t;
  //FT_PtrDist = size_t;

  FT_Render_Mode = (FT_RENDER_MODE_NORMAL, FT_RENDER_MODE_LIGHT,
      FT_RENDER_MODE_MONO, FT_RENDER_MODE_LCD, FT_RENDER_MODE_LCD_V,
      FT_RENDER_MODE_MAX);

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
    pixel_mode : char;
    palette_mode : char;
    palette : pointer;
  end;

  FT_Outline = record
    n_contours,
    n_points : smallint;
    points : PFT_Vector;
    tags : pchar;
    contours : ^smallint;
    flags : integer;
  end;
  PFT_Outline = ^FT_Outline;

  FT_Outline_MoveToFunc = function(const to_: PFT_Vector; user: Pointer): integer;
  FT_Outline_LineToFunc = function(const to_: PFT_Vector; user: Pointer): integer;
  FT_Outline_ConicToFunc = function(const control, to_: PFT_Vector; user: Pointer): integer;
  FT_Outline_CubicToFunc = function(const control1, control2, to_: PFT_Vector; user: Pointer): integer;

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
    format : longword;
    bitmap : FT_Bitmap;
    bitmap_left : FT_Int;
    bitmap_top : FT_Int;
    outline : FT_Outline;
    num_subglyphs : FT_UInt;
    subglyphs : PFT_SubGlyph;
    control_data : pointer;
    control_len : longint;
    other : pointer;
  end;

  TFT_Face = record
    num_faces : FT_Long;
    face_index : FT_Long;
    face_flags : FT_Long;
    style_flags : FT_Long;
    num_glyphs : FT_Long;
    family_name : pchar;
    style_name : pchar;
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
    aFormat : longword;
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

// Macro
function FT_IS_SCALABLE(face: PFT_Face): boolean;

{$IFNDEF DYNAMIC}

//Base Interface
function FT_Done_Face(face: PFT_Face): integer; cdecl; external FreeTypeDLL Name 'FT_Done_Face';
function FT_Done_FreeType(alibrary: PFT_Library): integer; cdecl; external FreeTypeDLL Name 'FT_Done_FreeType';
function FT_Get_Char_Index(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl; external FreeTypeDLL Name 'FT_Get_Char_Index';
function FT_Get_Kerning(face: PFT_Face; left_glyph, right_glyph, kern_mode: FT_UInt; out akerning: FT_Vector): integer; cdecl; external FreeTypeDLL Name 'FT_Get_Kerning';
function FT_Init_FreeType(var alibrary: PFT_Library): integer; cdecl; external FreeTypeDLL Name 'FT_Init_FreeType';
function FT_Load_Char(face: PFT_Face; charcode: FT_ULong; load_flags: longint): integer; cdecl; external FreeTypeDLL Name 'FT_Load_Char';
function FT_Load_Glyph(face: PFT_Face; glyph_index: FT_UInt; load_flags: longint): integer; cdecl; external FreeTypeDLL Name 'FT_Load_Glyph';
function FT_New_Face(alibrary: PFT_Library; filepathname: PChar; face_index: integer; var aface: PFT_Face): integer; cdecl; external FreeTypeDLL Name 'FT_New_Face';
function FT_Set_Char_Size(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl; external FreeTypeDLL Name 'FT_Set_Char_Size';
function FT_Set_Pixel_Sizes(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl; external FreeTypeDLL Name 'FT_Set_Pixel_Sizes';
procedure FT_Set_Transform(face: PFT_Face; matrix: PFT_Matrix; delta: PFT_Vector); cdecl; external FreeTypeDLL Name 'FT_Set_Transform';

//Outline Processing
function FT_Outline_Decompose(outline: PFT_Outline; const func_interface: PFT_Outline_Funcs; user: Pointer): integer; cdecl; external FreeTypeDLL Name 'FT_Outline_Decompose';

//FreeType Version
procedure FT_Library_Version(alibrary: PFT_Library; var amajor, aminor, apatch: integer); cdecl; external FreeTypeDLL Name 'FT_Library_Version';

//Glyph Management
function FT_Get_Glyph(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl; external FreeTypeDLL Name 'FT_Get_Glyph';
function FT_Glyph_Copy(Source: PFT_Glyph; out target: PFT_Glyph): integer; cdecl; external FreeTypeDLL Name 'FT_Glyph_Copy';
function FT_Glyph_To_Bitmap(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl; external FreeTypeDLL Name 'FT_Glyph_To_Bitmap';
function FT_Glyph_Transform(glyph: PFT_Glyph; matrix: PFT_Matrix; delta: PFT_Vector): integer; cdecl; external FreeTypeDLL Name 'FT_Glyph_Transform';
procedure FT_Done_Glyph(glyph: PFT_Glyph); cdecl; external FreeTypeDLL Name 'FT_Done_Glyph';
procedure FT_Glyph_Get_CBox(glyph: PFT_Glyph; bbox_mode: FT_UInt; var acbox: FT_BBox); cdecl; external FreeTypeDLL Name 'FT_Glyph_Get_CBox';

{$ELSE}
    //Base Interface
Type
  TFT_Done_Face = function(face: PFT_Face): integer; cdecl;
  TFT_Done_FreeType = function(alibrary: PFT_Library): integer; cdecl;
  TFT_Get_Char_Index = function(face: PFT_Face; charcode: FT_ULong): FT_UInt; cdecl;
  TFT_Get_Kerning = function(face: PFT_Face; left_glyph, right_glyph, kern_mode: FT_UInt; out akerning: FT_Vector): integer; cdecl;
  TFT_Init_FreeType = function(var alibrary: PFT_Library): integer; cdecl;
  TFT_Load_Char = function(face: PFT_Face; charcode: FT_ULong; load_flags: longint): integer; cdecl;
  TFT_Load_Glyph = function(face: PFT_Face; glyph_index: FT_UInt; load_flags: longint): integer; cdecl;
  TFT_New_Face = function(alibrary: PFT_Library; filepathname: PChar; face_index: integer; var aface: PFT_Face): integer; cdecl;
  TFT_Set_Char_Size = function(face: PFT_Face; char_width, char_height: FT_F26dot6; horz_res, vert_res: FT_UInt): integer; cdecl;
  TFT_Set_Pixel_Sizes = function(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer; cdecl;
  TFT_Set_Transform = procedure(face: PFT_Face; matrix: PFT_Matrix; delta: PFT_Vector); cdecl;
    //Outline Processing
  TFT_Outline_Decompose = function(outline: PFT_Outline; const func_interface: PFT_Outline_Funcs; user: Pointer): integer; cdecl;

    //FreeType Version
  TFT_Library_Version = procedure(alibrary: PFT_Library; var amajor, aminor, apatch: integer); cdecl;
  //Glyph Management
  TFT_Get_Glyph = function(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer; cdecl;
  TFT_Glyph_Copy = function(Source: PFT_Glyph; out target: PFT_Glyph): integer; cdecl;
  TFT_Glyph_To_Bitmap = function(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode; origin: PFT_Vector; Destroy: FT_Bool): integer; cdecl;
  TFT_Glyph_Transform = function(glyph: PFT_Glyph; matrix: PFT_Matrix; delta: PFT_Vector): integer; cdecl;
  TFT_Done_Glyph = procedure(glyph: PFT_Glyph); cdecl;
  TFT_Glyph_Get_CBox = procedure(glyph: PFT_Glyph; bbox_mode: FT_UInt; var acbox: FT_BBox); cdecl;

Var
  FT_Done_Face : TFT_Done_Face;
  FT_Done_FreeType : TFT_Done_FreeType;
  FT_Get_Char_Index : TFT_Get_Char_Index;
  FT_Get_Kerning : TFT_Get_Kerning;
  FT_Init_FreeType : TFT_Init_FreeType;
  FT_Load_Char : TFT_Load_Char;
  FT_Load_Glyph : TFT_Load_Glyph;
  FT_New_Face : TFT_New_Face;
  FT_Set_Char_Size : TFT_Set_Char_Size;
  FT_Set_Pixel_Sizes : TFT_Set_Pixel_Sizes;
  FT_Set_Transform : TFT_Set_Transform;

  //Outline Processing
  FT_Outline_Decompose : TFT_Outline_Decompose;

  //FreeType Version
  FT_Library_Version : TFT_Library_Version;

  //Glyph Management
  FT_Get_Glyph : TFT_Get_Glyph;
  FT_Glyph_Copy : TFT_Glyph_Copy;
  FT_Glyph_To_Bitmap : TFT_Glyph_To_Bitmap;
  FT_Glyph_Transform : TFT_Glyph_Transform;
  FT_Done_Glyph : TFT_Done_Glyph;
  FT_Glyph_Get_CBox : TFT_Glyph_Get_CBox;

function InitializeFreetype(const LibraryName: UnicodeString = ''): Integer;
function  InitializeFreetype(const LibraryName: AnsiString):integer;
function TryInitializeFreetype(const LibraryName: Unicodestring = ''): Integer;
procedure ReleaseFreetype;
{$ENDIF}

implementation

function FT_IS_SCALABLE(face: PFT_Face): boolean;

begin
  Result := (face^.face_flags and FT_FACE_FLAG_SCALABLE) = 1;
end;

{$IFDEF DYNAMIC}
var
  FreetypeLibraryHandle: TLibHandle = NilHandle;
  FreetypeDefaultLibrary: String = FreeTypeDLL;
  FreetypeLoadedLibrary: UnicodeString = '';
  RefCount: Integer = 0;

resourcestring
  SErrLoadFailed     = 'Can not load Freetype library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'Freetype interface already initialized from library %s.';

procedure LoadAddresses(LibHandle: TLibHandle);

begin
  pointer(FT_Done_FreeType) := GetProcedureAddress(LibHandle,'FT_Done_FreeType');
  pointer(FT_Get_Char_Index) := GetProcedureAddress(LibHandle,'FT_Get_Char_Index');
  pointer(FT_Get_Kerning) := GetProcedureAddress(LibHandle,'FT_Get_Kerning');
  pointer(FT_Init_FreeType) := GetProcedureAddress(LibHandle,'FT_Init_FreeType');
  pointer(FT_Load_Char) := GetProcedureAddress(LibHandle,'FT_Load_Char');
  pointer(FT_Load_Glyph) := GetProcedureAddress(LibHandle,'FT_Load_Glyph');
  pointer(FT_New_Face) := GetProcedureAddress(LibHandle,'FT_New_Face');
  pointer(FT_Set_Char_Size) := GetProcedureAddress(LibHandle,'FT_Set_Char_Size');
  pointer(FT_Set_Pixel_Sizes) := GetProcedureAddress(LibHandle,'FT_Set_Pixel_Sizes');
  pointer(FT_Set_Transform) := GetProcedureAddress(LibHandle,'FT_Set_Transform');
  pointer(FT_Outline_Decompose) := GetProcedureAddress(LibHandle,'FT_Outline_Decompose');
  pointer(FT_Library_Version) := GetProcedureAddress(LibHandle,'FT_Library_Version');
  pointer(FT_Get_Glyph) := GetProcedureAddress(LibHandle,'FT_Get_Glyph');
  pointer(FT_Glyph_Copy) := GetProcedureAddress(LibHandle,'FT_Glyph_Copy');
  pointer(FT_Glyph_To_Bitmap) := GetProcedureAddress(LibHandle,'FT_Glyph_To_Bitmap');
  pointer(FT_Glyph_Transform) := GetProcedureAddress(LibHandle,'FT_Glyph_Transform');
  pointer(FT_Done_Glyph) := GetProcedureAddress(LibHandle,'FT_Done_Glyph');
  pointer(FT_Glyph_Get_CBox) := GetProcedureAddress(LibHandle,'FT_Glyph_Get_CBox');
end;

procedure NilAllAddresses;

begin
  pointer(FT_Done_FreeType) := Nil;
  pointer(FT_Get_Char_Index) := Nil;
  pointer(FT_Get_Kerning) := Nil;
  pointer(FT_Init_FreeType) := Nil;
  pointer(FT_Load_Char) := Nil;
  pointer(FT_Load_Glyph) := Nil;
  pointer(FT_New_Face) := Nil;
  pointer(FT_Set_Char_Size) := Nil;
  pointer(FT_Set_Pixel_Sizes) := Nil;
  pointer(FT_Set_Transform) := Nil;
  pointer(FT_Outline_Decompose) := Nil;
  pointer(FT_Library_Version) := Nil;
  pointer(FT_Get_Glyph) := Nil;
  pointer(FT_Glyph_Copy) := Nil;
  pointer(FT_Glyph_To_Bitmap) := Nil;
  pointer(FT_Glyph_Transform) := Nil;
  pointer(FT_Done_Glyph) := Nil;
  pointer(FT_Glyph_Get_CBox) := Nil;
end;


function TryInitializeFreetype(const LibraryName: UnicodeString): Integer;

Var
  N  : UnicodeString;
begin
  N:=LibraryName;
  if (N='') then
    N:=FreetypeDefaultLibrary;
  result:=InterlockedIncrement(RefCount);
  if result=1 then
    begin
    FreetypeLibraryHandle := LoadLibrary(N);
    if (FreetypeLibraryHandle = NilHandle) then
      begin
      RefCount:=0;
      Result:=0;
      end;
    FreetypeLoadedLibrary := N;
    LoadAddresses(FreetypeLibraryHandle);
    end;
end;

function  InitializeFreetype(const LibraryName: UnicodeString) :integer;

begin
  if (LibraryName<>'') and (FreetypeLoadedLibrary <> '') and (FreetypeLoadedLibrary <> LibraryName) then
    Raise EInoutError.CreateFmt(SErrAlreadyLoaded,[FreetypeLoadedLibrary]);
  Result:=TryInitializeFreetype(LibraryName);
  if result=0 then
    if LibraryName='' then
      Raise EInOutError.CreateFmt(SErrLoadFailed,[FreetypeDefaultLibrary])
    else
      Raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName]);
end;

function InitializeFreetype(const LibraryName: AnsiString):integer;
begin
  result:=InitializeFreetype(UnicodeString(LibraryName));
end;

procedure ReleaseFreetype;
begin
  if InterlockedDecrement(RefCount) <= 0 then
    begin
    if FreetypeLibraryHandle <> NilHandle then
      UnloadLibrary(FreetypeLibraryHandle);
    NilAllAddresses;
    FreetypeLibraryHandle := NilHandle;
    FreetypeLoadedLibrary := '';
    RefCount := 0;
    end;
end;
{$ENDIF}

