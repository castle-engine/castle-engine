{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Pure Pascal implementation of a subset of FreeType API,
  included in CastleInternalFreeTypeH when CASTLE_FREETYPE_PASCAL is defined.

  Limitations compared to the FreeType library:
  - no hinting (so glyphs are a bit softer at small sizes),
  - only TrueType / OpenType fonts (TTF, OTF, TTC),
  - and only the API subset used by CGE
    (in particular, FT_Outline_Decompose, FT_Set_Transform, FT_Glyph_Transform,
    FT_Glyph_Get_CBox, FT_Library_Version are not available).

  Uses CastleInternalOpenTypeFont to read fonts
  and CastleInternalGlyphRasterizer to render glyphs.

  The FreeType records (TFT_Face, TFT_Glyph etc.) are the first fields
  of our own records, that also contain our data.
  So PFT_Face and PFT_Glyph pointers returned by this implementation
  can be typecasted to our records.

  -----------------------------------------------------------------------------

  Disclosure: This file is largely Claude-generated.
  It was reviewed and slightly edited by Michalis, but this is still a
  (unusually, for Castle Game Engine) large automatically generated file.
}

const
  { FreeType error codes, see fterrdef.h in FreeType. }
  FtErrOk = 0;
  FtErrUnknownFileFormat = $02;
  FtErrInvalidArgument = $06;
  FtErrInvalidGlyphIndex = $10;
  FtErrInvalidGlyphFormat = $12;
  FtErrCannotRenderGlyph = $13;
  FtErrInvalidOutline = $14;
  FtErrInvalidHandle = $20;
  FtErrInvalidSizeHandle = $24;

  { FT_Pixel_Mode values. }
  FtPixelModeMono = 1;
  FtPixelModeGray = 2;

type
  PPascalFace = ^TPascalFace;
  TPascalFace = record
    { FreeType face. Must be the first field, PFT_Face points to it. }
    Face: TFT_Face;
    Slot: TFT_GlyphSlot;
    Size: TFT_Size;
    Font: TOpenTypeFont;
    FamilyName, StyleName: AnsiString;
    { Pixels per em, 0 if not set yet. }
    PixelSize: Integer;
    { Pixels per font unit. }
    Scale: Double;
    { Outline loaded by FT_Load_Glyph, in font units. }
    SlotOutline: TGlyphOutline;
  end;

  PPascalOutlineGlyph = ^TPascalOutlineGlyph;
  TPascalOutlineGlyph = record
    { FreeType glyph. Must be the first field, PFT_Glyph points to it. }
    Glyph: TFT_Glyph;
    { Outline, in font units. }
    Outline: TGlyphOutline;
    { Pixels per font unit. }
    Scale: Double;
  end;

  PPascalBitmapGlyph = ^TPascalBitmapGlyph;
  TPascalBitmapGlyph = record
    { FreeType bitmap glyph. Must be the first field, PFT_Glyph points to it. }
    Glyph: TFT_BitmapGlyph;
    { Bitmap data, Glyph.bitmap.buffer points to it. }
    Data: TBytes;
  end;

var
  { FT_Init_FreeType returns pointer to this. We don't need any library state. }
  PascalLibrary: Byte;

{ Round 26.6 value down / up / to nearest integer pixel, like FreeType
  FT_PIX_FLOOR, FT_PIX_CEIL, FT_PIX_ROUND. }
function PixFloor(const V: Int64): Int64;
begin
  Result := V and not Int64(63);
end;

function PixCeil(const V: Int64): Int64;
begin
  Result := PixFloor(V + 63);
end;

function PixRound(const V: Int64): Int64;
begin
  Result := PixFloor(V + 32);
end;

{ Library ------------------------------------------------------------------- }

procedure LoadFreeTypeLibrary;
begin
end;

function FreeTypeLibraryInitialized: boolean;
begin
  Result := true;
end;

function FT_Init_FreeType(var alibrary: PFT_Library): integer;
begin
  alibrary := PFT_Library(@PascalLibrary);
  Result := FtErrOk;
end;

function FT_Done_FreeType(alibrary: PFT_Library): integer;
begin
  Result := FtErrOk;
end;

{ Faces --------------------------------------------------------------------- }

function FT_New_Memory_Face(alibrary: PFT_Library; Stream: TCustomMemoryStream;
  face_index: FT_Long; var aface: PFT_Face): integer;
var
  Font: TOpenTypeFont;
  FaceIndex: Integer;
  PF: PPascalFace;
begin
  aface := nil;
  if Stream = nil then
    Exit(FtErrInvalidArgument);

  { Lower 16 bits of face_index are the face index in the collection
    (higher bits select variable font named instance in FreeType,
    not supported here). }
  if face_index < 0 then
    FaceIndex := 0
  else
    FaceIndex := face_index and $FFFF;

  { Font owns the Stream now (also when TOpenTypeFont.Create fails,
    then Stream is freed by TOpenTypeFont destructor). }
  try
    Font := TOpenTypeFont.Create(Stream, FaceIndex);
  except
    { Invalid font data or truncated font data.
      EMathError and EIntError (like EInvalidOp, ERangeError, EIntOverflow)
      may happen when calculating on crazy numbers from invalid font data. }
    on EOpenTypeFontError do
      Exit(FtErrUnknownFileFormat);
    on EReadError do
      Exit(FtErrUnknownFileFormat);
    on EMathError do
      Exit(FtErrUnknownFileFormat);
    on EIntError do
      Exit(FtErrUnknownFileFormat);
  end;

  New(PF);
  FillChar(PF^.Face, SizeOf(PF^.Face), 0);
  FillChar(PF^.Slot, SizeOf(PF^.Slot), 0);
  FillChar(PF^.Size, SizeOf(PF^.Size), 0);
  PF^.Font := Font;
  PF^.PixelSize := 0;
  PF^.Scale := 0;
  PF^.SlotOutline := TGlyphOutline.Create;
  PF^.FamilyName := Font.FamilyName;
  PF^.StyleName := Font.StyleName;

  PF^.Face.num_faces := Font.NumFaces;
  PF^.Face.face_index := FaceIndex;
  PF^.Face.face_flags :=
    FT_FACE_FLAG_SCALABLE or
    FT_FACE_FLAG_SFNT or
    FT_FACE_FLAG_HORIZONTAL;
  if Font.HasKerning then
    PF^.Face.face_flags := PF^.Face.face_flags or FT_FACE_FLAG_KERNING;
  PF^.Face.style_flags := 0;
  if Font.Italic then
    PF^.Face.style_flags := PF^.Face.style_flags or FT_STYLE_FLAG_ITALIC;
  if Font.Bold then
    PF^.Face.style_flags := PF^.Face.style_flags or FT_STYLE_FLAG_BOLD;
  PF^.Face.num_glyphs := Font.NumGlyphs;
  if PF^.FamilyName <> '' then
    PF^.Face.family_name := PAnsiChar(PF^.FamilyName);
  if PF^.StyleName <> '' then
    PF^.Face.style_name := PAnsiChar(PF^.StyleName);
  PF^.Face.num_fixed_sizes := 0;
  PF^.Face.available_sizes := nil;
  PF^.Face.num_charmaps := 0;
  PF^.Face.charmaps := nil;
  PF^.Face.bbox.xMin := Font.XMin;
  PF^.Face.bbox.yMin := Font.YMin;
  PF^.Face.bbox.xMax := Font.XMax;
  PF^.Face.bbox.yMax := Font.YMax;
  PF^.Face.units_per_EM := Font.UnitsPerEm;
  PF^.Face.ascender := Font.Ascender;
  PF^.Face.descender := Font.Descender;
  PF^.Face.height := Font.Ascender - Font.Descender + Font.LineGap;
  PF^.Face.max_advance_width := Font.AdvanceWidthMax;
  PF^.Face.max_advance_height := PF^.Face.height;
  PF^.Face.underline_position := Font.UnderlinePosition;
  PF^.Face.underline_thickness := Font.UnderlineThickness;
  PF^.Face.glyph := @PF^.Slot;
  PF^.Face.size := @PF^.Size;
  PF^.Face.charmap := nil;

  PF^.Slot.alibrary := alibrary;
  PF^.Slot.face := @PF^.Face;
  PF^.Size.face := @PF^.Face;

  aface := @PF^.Face;
  Result := FtErrOk;
end;

function FT_Done_Face(face: PFT_Face): integer;
var
  PF: PPascalFace;
begin
  if face = nil then
    Exit(FtErrInvalidHandle);
  PF := PPascalFace(face);
  FreeAndNil(PF^.Font);
  FreeAndNil(PF^.SlotOutline);
  Dispose(PF);
  Result := FtErrOk;
end;

{ Sizes --------------------------------------------------------------------- }

procedure SetPixelSize(const PF: PPascalFace; PixelSize: Integer);
var
  Font: TOpenTypeFont;
begin
  if PixelSize < 1 then
    PixelSize := 1;
  if PixelSize > High(FT_UShort) then
    PixelSize := High(FT_UShort);
  Font := PF^.Font;
  PF^.PixelSize := PixelSize;
  PF^.Scale := PixelSize / Font.UnitsPerEm;

  PF^.Size.metrics.x_ppem := PixelSize;
  PF^.Size.metrics.y_ppem := PixelSize;
  { scale from font units to 26.6 pixels, as 16.16 number }
  PF^.Size.metrics.x_scale := Round(PF^.Scale * 64 * 65536);
  PF^.Size.metrics.y_scale := PF^.Size.metrics.x_scale;
  { rounded like FreeType does for scalable fonts, in 26.6 }
  PF^.Size.metrics.ascender := PixCeil(Round(Font.Ascender * PF^.Scale * 64));
  PF^.Size.metrics.descender := PixFloor(Round(Font.Descender * PF^.Scale * 64));
  PF^.Size.metrics.height := PixRound(Round(
    (Font.Ascender - Font.Descender + Font.LineGap) * PF^.Scale * 64));
  PF^.Size.metrics.max_advance := PixRound(Round(Font.AdvanceWidthMax * PF^.Scale * 64));
end;

function FT_Set_Char_Size(face: PFT_Face; char_width, char_height: FT_F26dot6;
  horz_res, vert_res: FT_UInt): integer;
var
  Height: Int64;
  Resolution: Int64;
begin
  if face = nil then
    Exit(FtErrInvalidHandle);
  Height := char_height;
  if Height = 0 then
    Height := char_width;
  Resolution := vert_res;
  if Resolution = 0 then
    Resolution := horz_res;
  if Resolution = 0 then
    Resolution := 72;
  { Height in 26.6 points -> pixels. }
  SetPixelSize(PPascalFace(face), Integer(Round(Height * Resolution / 72 / 64)));
  Result := FtErrOk;
end;

function FT_Set_Pixel_Sizes(face: PFT_Face; pixel_width, pixel_height: FT_UInt): integer;
var
  Height: Int64;
begin
  if face = nil then
    Exit(FtErrInvalidHandle);
  Height := pixel_height;
  if Height = 0 then
    Height := pixel_width;
  if Height > High(FT_UShort) then
    Height := High(FT_UShort);
  SetPixelSize(PPascalFace(face), Integer(Height));
  Result := FtErrOk;
end;

{ Glyphs -------------------------------------------------------------------- }

function FT_Get_Char_Index(face: PFT_Face; charcode: FT_ULong): FT_UInt;
begin
  if (face = nil) or (charcode > $10FFFF) then
    Exit(0);
  try
    Result := PPascalFace(face)^.Font.GlyphIndex(Cardinal(charcode));
  except
    { invalid or truncated cmap data, treat as missing character }
    on EOpenTypeFontError do
      Result := 0;
    on EReadError do
      Result := 0;
    on EMathError do
      Result := 0;
    on EIntError do
      Result := 0;
  end;
end;

function FT_Load_Glyph(face: PFT_Face; glyph_index: FT_UInt; load_flags: CInt32): integer;
var
  PF: PPascalFace;
  Advance: Integer;
  Advance26: Int64;
begin
  if face = nil then
    Exit(FtErrInvalidHandle);
  PF := PPascalFace(face);
  if PF^.PixelSize = 0 then
    Exit(FtErrInvalidSizeHandle);
  if glyph_index >= FT_UInt(PF^.Font.NumGlyphs) then
    Exit(FtErrInvalidGlyphIndex);

  try
    PF^.Font.GetGlyphOutline(glyph_index, PF^.SlotOutline);
    Advance := PF^.Font.AdvanceWidth(glyph_index);
  except
    { invalid or truncated glyph data }
    on EOpenTypeFontError do
      Exit(FtErrInvalidOutline);
    on EReadError do
      Exit(FtErrInvalidOutline);
    on EMathError do
      Exit(FtErrInvalidOutline);
    on EIntError do
      Exit(FtErrInvalidOutline);
  end;

  { Round advance to integer pixels, like FreeType does when hinting. }
  Advance26 := PixRound(Round(Advance * PF^.Scale * 64));
  FillChar(PF^.Slot.metrics, SizeOf(PF^.Slot.metrics), 0);
  PF^.Slot.metrics.horiAdvance := Advance26;
  PF^.Slot.linearHoriAdvance := Round(Advance * PF^.Scale * 65536);
  PF^.Slot.linearVertAdvance := 0;
  PF^.Slot.advance.x := Advance26;
  PF^.Slot.advance.y := 0;
  PF^.Slot.format := ft_glyph_format_outline;
  FillChar(PF^.Slot.bitmap, SizeOf(PF^.Slot.bitmap), 0);
  PF^.Slot.bitmap_left := 0;
  PF^.Slot.bitmap_top := 0;
  Result := FtErrOk;
end;

function FT_Load_Char(face: PFT_Face; charcode: FT_ULong; load_flags: CInt32): integer;
begin
  Result := FT_Load_Glyph(face, FT_Get_Char_Index(face, charcode), load_flags);
end;

function FT_Get_Kerning(face: PFT_Face; left_glyph, right_glyph, kern_mode: FT_UInt;
  out akerning: FT_Vector): integer;
var
  PF: PPascalFace;
  K: Integer;
  V: Int64;
begin
  akerning.x := 0;
  akerning.y := 0;
  if face = nil then
    Exit(FtErrInvalidHandle);
  PF := PPascalFace(face);
  if (left_glyph > $FFFF) or (right_glyph > $FFFF) then
    Exit(FtErrOk);
  K := PF^.Font.Kerning(left_glyph, right_glyph);
  if kern_mode = FT_KERNING_UNSCALED then
    akerning.x := K
  else
  begin
    V := Round(K * PF^.Scale * 64);
    if kern_mode = FT_KERNING_DEFAULT then
      V := PixRound(V); // grid-fitted
    akerning.x := V;
  end;
  Result := FtErrOk;
end;

{ Glyph management ---------------------------------------------------------- }

function FT_Get_Glyph(slot: PFT_GlyphSlot; out aglyph: PFT_Glyph): integer;
var
  PF: PPascalFace;
  G: PPascalOutlineGlyph;
begin
  aglyph := nil;
  if (slot = nil) or (slot^.face = nil) then
    Exit(FtErrInvalidHandle);
  if slot^.format <> ft_glyph_format_outline then
    Exit(FtErrInvalidGlyphFormat);
  PF := PPascalFace(slot^.face);
  New(G);
  FillChar(G^.Glyph, SizeOf(G^.Glyph), 0);
  G^.Glyph.FTlibrary := slot^.alibrary;
  G^.Glyph.aFormat := ft_glyph_format_outline;
  { FT_Glyph advance is in 16.16 format, slot advance is in 26.6 }
  G^.Glyph.advance.x := slot^.advance.x * 1024;
  G^.Glyph.advance.y := slot^.advance.y * 1024;
  G^.Outline := TGlyphOutline.Create;
  G^.Outline.Assign(PF^.SlotOutline);
  G^.Scale := PF^.Scale;
  aglyph := PFT_Glyph(G);
  Result := FtErrOk;
end;

{ Set bitmap glyph fields that point to data. }
procedure UpdateBitmapBuffer(const G: PPascalBitmapGlyph);
begin
  if Length(G^.Data) <> 0 then
    G^.Glyph.bitmap.buffer := @G^.Data[0]
  else
    G^.Glyph.bitmap.buffer := nil;
end;

function FT_Glyph_Copy(Source: PFT_Glyph; out target: PFT_Glyph): integer;
var
  SourceOutline, TargetOutline: PPascalOutlineGlyph;
  SourceBitmap, TargetBitmap: PPascalBitmapGlyph;
begin
  target := nil;
  if Source = nil then
    Exit(FtErrInvalidHandle);
  if Source^.aFormat = ft_glyph_format_outline then
  begin
    SourceOutline := PPascalOutlineGlyph(Source);
    New(TargetOutline);
    TargetOutline^.Glyph := SourceOutline^.Glyph;
    TargetOutline^.Outline := TGlyphOutline.Create;
    TargetOutline^.Outline.Assign(SourceOutline^.Outline);
    TargetOutline^.Scale := SourceOutline^.Scale;
    target := PFT_Glyph(TargetOutline);
  end else
  if Source^.aFormat = ft_glyph_format_bitmap then
  begin
    SourceBitmap := PPascalBitmapGlyph(Source);
    New(TargetBitmap);
    TargetBitmap^.Glyph := SourceBitmap^.Glyph;
    TargetBitmap^.Data := Copy(SourceBitmap^.Data, 0, Length(SourceBitmap^.Data));
    UpdateBitmapBuffer(TargetBitmap);
    target := PFT_Glyph(TargetBitmap);
  end else
    Exit(FtErrInvalidGlyphFormat);
  Result := FtErrOk;
end;

procedure FT_Done_Glyph(glyph: PFT_Glyph);
var
  OutlineGlyph: PPascalOutlineGlyph;
  BitmapGlyph: PPascalBitmapGlyph;
begin
  if glyph = nil then
    Exit;
  if glyph^.aFormat = ft_glyph_format_outline then
  begin
    OutlineGlyph := PPascalOutlineGlyph(glyph);
    FreeAndNil(OutlineGlyph^.Outline);
    Dispose(OutlineGlyph);
  end else
  if glyph^.aFormat = ft_glyph_format_bitmap then
  begin
    BitmapGlyph := PPascalBitmapGlyph(glyph);
    Dispose(BitmapGlyph);
  end;
end;

function FT_Glyph_To_Bitmap(var the_glyph: PFT_Glyph; render_mode: FT_Render_Mode;
  origin: PFT_Vector; Destroy: FT_Bool): integer;
var
  OutlineGlyph: PPascalOutlineGlyph;
  BitmapGlyph: PPascalBitmapGlyph;
  OriginX, OriginY: Double;
  Rasterized: TRasterizedGlyph;
begin
  if the_glyph = nil then
    Exit(FtErrInvalidHandle);
  if the_glyph^.aFormat = ft_glyph_format_bitmap then
    Exit(FtErrOk); // already a bitmap
  if the_glyph^.aFormat <> ft_glyph_format_outline then
    Exit(FtErrInvalidGlyphFormat);
  OutlineGlyph := PPascalOutlineGlyph(the_glyph);

  OriginX := 0;
  OriginY := 0;
  if origin <> nil then
  begin
    OriginX := origin^.x / 64;
    OriginY := origin^.y / 64;
  end;

  try
    RasterizeGlyph(OutlineGlyph^.Outline, OutlineGlyph^.Scale, OriginX, OriginY,
      render_mode = FT_RENDER_MODE_MONO, Rasterized);
  except
    on EOpenTypeFontError do
      Exit(FtErrCannotRenderGlyph);
    on EMathError do
      Exit(FtErrCannotRenderGlyph);
    on EIntError do
      Exit(FtErrCannotRenderGlyph);
  end;

  New(BitmapGlyph);
  FillChar(BitmapGlyph^.Glyph, SizeOf(BitmapGlyph^.Glyph), 0);
  BitmapGlyph^.Glyph.root := OutlineGlyph^.Glyph;
  BitmapGlyph^.Glyph.root.aFormat := ft_glyph_format_bitmap;
  BitmapGlyph^.Glyph.left := Rasterized.Left;
  BitmapGlyph^.Glyph.top := Rasterized.Top;
  BitmapGlyph^.Glyph.bitmap.rows := Rasterized.Height;
  BitmapGlyph^.Glyph.bitmap.width := Rasterized.Width;
  BitmapGlyph^.Glyph.bitmap.pitch := Rasterized.Pitch;
  { Note: FT_Bitmap.num_grays should be 256 for gray bitmaps, but it's declared
    as shortint in this unit. CGE doesn't use it, so we leave it 0. }
  if Rasterized.Mono then
  begin
    BitmapGlyph^.Glyph.bitmap.num_grays := 2;
    BitmapGlyph^.Glyph.bitmap.pixel_mode := AnsiChar(FtPixelModeMono);
  end else
    BitmapGlyph^.Glyph.bitmap.pixel_mode := AnsiChar(FtPixelModeGray);
  BitmapGlyph^.Data := Rasterized.Data;
  UpdateBitmapBuffer(BitmapGlyph);

  if Destroy then
    FT_Done_Glyph(the_glyph);
  the_glyph := PFT_Glyph(BitmapGlyph);
  Result := FtErrOk;
end;
