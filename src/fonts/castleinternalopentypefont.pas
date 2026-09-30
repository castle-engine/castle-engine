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

{ Reading OpenType (TTF, OTF) font files in pure Pascal, without FreeType.

  Supports:

  @unorderedList(
    @item(TrueType outlines (glyf, loca tables), including composite glyphs.)
    @item(CFF outlines (Type 2 charstrings), including CID-keyed fonts.)
    @item(TrueType Collections (TTC), selecting the face by index.)
    @item(Character to glyph mapping (cmap formats 0, 4, 6, 12).)
    @item(Horizontal metrics, font names, style (bold / italic),
      kerning from the "kern" table (format 0).)
  )

  Not supported: hinting (bytecode instructions or CFF hints are ignored),
  bitmap-only fonts, color fonts (the regular outlines are used, if present),
  variable fonts variations (the default instance is used),
  GPOS kerning, CFF2 outlines.

  The glyph outlines are rasterized by the CastleInternalGlyphRasterizer unit.
  Both units together are used by CastleInternalFreeTypeH to implement
  a subset of FreeType API, on platforms where we don't want to depend
  on the FreeType library.

  -----------------------------------------------------------------------------

  Disclosure: This file is largely Claude-generated.
  It was reviewed and slightly edited by Michalis, but this is still a
  (unusually, for Castle Game Engine) large automatically generated file. }
unit CastleInternalOpenTypeFont;

{$I castleconf.inc}

interface

uses SysUtils, Classes;

type
  { Error reading the font file, e.g. the file is invalid or truncated. }
  EOpenTypeFontError = class(Exception);

  TOutlineCommandKind = (ocMove, ocLine, ocQuad, ocCubic);

  { Single command of TGlyphOutline. }
  TOutlineCommand = record
    Kind: TOutlineCommandKind;
    { Points, in font units (y goes up).
      For ocMove and ocLine, only X[0], Y[0] are used (the target point).
      For ocQuad, X[0], Y[0] is the control point, X[1], Y[1] the target point.
      For ocCubic, X[0], Y[0] and X[1], Y[1] are control points,
      X[2], Y[2] is the target point. }
    X, Y: array [0..2] of Double;
  end;

  { Glyph outline: a list of closed contours built from lines, quadratic
    and cubic Bezier curves.
    Each ocMove starts a new contour, contours are closed implicitly. }
  TGlyphOutline = class
  strict private
    procedure Add(const Kind: TOutlineCommandKind;
      const X0, Y0, X1, Y1, X2, Y2: Double);
  public
    Commands: array of TOutlineCommand;
    Count: Integer;
    procedure Clear;
    procedure MoveTo(const X, Y: Double);
    procedure LineTo(const X, Y: Double);
    procedure QuadTo(const CX, CY, X, Y: Double);
    procedure CubicTo(const C1X, C1Y, C2X, C2Y, X, Y: Double);
    procedure Assign(const Source: TGlyphOutline);
    { Transform all points: X' = A * X + C * Y, Y' = B * X + D * Y. }
    procedure Transform(const A, B, C, D: Double);
  end;

  { OpenType (TTF, OTF) font file. }
  TOpenTypeFont = class
  strict private
    type
      { CFF INDEX structure. Positions are absolute in FData. }
      TCffIndex = record
        Count: Integer;
        OffSize: Integer;
        OffsetsPos: Integer;
        { Position of the byte before the data. Offsets are relative to it. }
        DataBase: Integer;
        EndPos: Integer;
      end;

      { Values we read from CFF DICT (Top DICT, Font DICT, Private DICT). }
      TCffDict = record
        CharStringsOffset: Integer;
        PrivateSize, PrivateOffset: Integer;
        SubrsOffset: Integer;
        FDArrayOffset, FDSelectOffset: Integer;
        HasFontMatrix: Boolean;
        FontMatrix: array [0..5] of Double;
      end;

      { Subroutines of a CFF Private DICT. }
      TCffPrivate = record
        HasSubrs: Boolean;
        Subrs: TCffIndex;
        Bias: Integer;
      end;

      { State of the Type 2 charstring interpreter. }
      TType2State = record
        Stack: array [0..47] of Double;
        StackCount: Integer;
        NumStems: Integer;
        HaveWidth: Boolean;
        X, Y: Double;
        Open: Boolean;
        Ended: Boolean;
        Depth: Integer;
        Transient: array [0..31] of Double;
        Outline: TGlyphOutline;
        Subroutines: TCffPrivate;
      end;

      { TrueType glyph points (before converting to TGlyphOutline). }
      TTrueTypePoints = record
        X, Y: array of Double;
        OnCurve: array of Boolean;
        Count: Integer;
        ContourEnds: array of Integer;
        ContourCount: Integer;
      end;

    var
      { Font data. Owned by this object. }
      FData: TCustomMemoryStream;
      FSize: Integer;
      FNumFaces: Integer;
      FTableTags: array of UInt32;
      FTableOffsets: array of Integer;
      FUnitsPerEm: Integer;
      FXMin, FYMin, FXMax, FYMax: Integer;
      FIndexToLocFormat: Integer;
      FNumGlyphs: Integer;
      FAscender, FDescender, FLineGap: Integer;
      FAdvanceWidthMax: Integer;
      FNumHMetrics: Integer;
      FHmtx: Integer;
      FHasOS2: Boolean;
      FFsSelection: Integer;
      FUnderlinePosition, FUnderlineThickness: Integer;
      FCmapSubtable: Integer; //< -1 if none
      FCmapFormat: Integer;
      FCmapSymbol: Boolean;
      FKernPairsPos, FKernPairsCount: Integer; //< FKernPairsCount = 0 if none
      FFamilyName, FStyleName: AnsiString;
      FItalic, FBold: Boolean;
      { TrueType outlines }
      FGlyf, FLoca: Integer;
      { CFF outlines }
      FIsCff: Boolean;
      FCffStart: Integer;
      FCharStrings: TCffIndex;
      FGlobalSubrs: TCffIndex;
      FGlobalBias: Integer;
      FCffPrivate: TCffPrivate;
      FFDPrivates: array of TCffPrivate;
      FFDSelect: Integer; //< -1 if none (not CID-keyed)
      FFontMatrixUsed: Boolean;
      FFontMatrixA, FFontMatrixB, FFontMatrixC, FFontMatrixD: Double;

    procedure Error(const Message: String);
    { Raise error if the range is not inside the font data. }
    procedure CheckRange(const Pos, Len: Int64);

    { Move to given position in the font data. }
    procedure SeekTo(const Pos: Int64);
    { Move forward by given number of bytes. }
    procedure Skip(const Count: Int64);
    { Current position in the font data. }
    function Position: Integer;

    { Read big-endian values from the current position.
      They raise EReadError when reading outside of the font data. }
    function ReadUInt8: Byte;
    function ReadUInt16: UInt16;
    function ReadInt16: Int16;
    function ReadUInt32: UInt32;
    function ReadInt32: Int32;
    { Read unsigned big-endian number stored in Size (1..4) bytes,
      used by CFF offsets. }
    function ReadUIntOfSize(const Size: Integer): UInt32;
    { Read UInt32 offset, checked to be inside the font data. }
    function ReadOffset32: Integer;

    { Find table, return offset or -1 if not found (or raise error if Required). }
    function FindTable(const Tag: AnsiString; const Required: Boolean): Integer;

    procedure ReadTables(const FaceIndex: Integer);
    procedure ReadNames;
    function FindName(const NameId: Integer; out Name: AnsiString): Boolean;
    procedure SelectCmap;
    function CmapLookup(const Code: Cardinal): Cardinal;
    procedure ReadKern;

    { TrueType outlines }
    procedure GlyphLocation(const Glyph: Integer; out Pos, Len: Integer);
    procedure LoadTrueTypeGlyph(const Glyph: Integer; var Points: TTrueTypePoints;
      const Depth: Integer);
    procedure LoadTrueTypeSimple(const NumContours: Integer;
      var Points: TTrueTypePoints);
    procedure LoadTrueTypeComposite(var Points: TTrueTypePoints;
      const Depth: Integer);
    procedure TrueTypePointsToOutline(const Points: TTrueTypePoints;
      const Outline: TGlyphOutline);

    { CFF outlines }
    procedure ReadCff(const Start: Integer);
    function ReadCffIndex(const Pos: Integer): TCffIndex;
    procedure CffIndexItem(const Index: TCffIndex; const I: Integer;
      out Pos, Len: Integer);
    procedure ReadCffDict(const Pos, Len: Integer; out Dict: TCffDict);
    function ReadCffPrivate(const Dict: TCffDict): TCffPrivate;
    function CffFDForGlyph(const Glyph: Integer): Integer;
    procedure LoadCffGlyph(const Glyph: Integer; const Outline: TGlyphOutline);
    procedure RunType2(var State: TType2State; const Pos, Len: Integer);
    procedure Type2Escape(var State: TType2State; const Op: Integer);
  public
    { Load font from given stream.
      The Stream becomes owned by this object, it will be freed
      by our destructor (also when this constructor raises an exception).
      @raises(EOpenTypeFontError If the font data is invalid.)
      @raises(EReadError If the font data is truncated.) }
    constructor Create(const Stream: TCustomMemoryStream; const FaceIndex: Integer = 0);
    destructor Destroy; override;

    { Number of faces in the file (more than 1 only for TrueType Collections). }
    property NumFaces: Integer read FNumFaces;
    property NumGlyphs: Integer read FNumGlyphs;
    property UnitsPerEm: Integer read FUnitsPerEm;
    { Font bounding box, in font units. }
    property XMin: Integer read FXMin;
    property YMin: Integer read FYMin;
    property XMax: Integer read FXMax;
    property YMax: Integer read FYMax;
    { Metrics from the "hhea" table, in font units. }
    property Ascender: Integer read FAscender;
    property Descender: Integer read FDescender;
    property LineGap: Integer read FLineGap;
    property AdvanceWidthMax: Integer read FAdvanceWidthMax;
    { Underline metrics from the "post" table, in font units. }
    property UnderlinePosition: Integer read FUnderlinePosition;
    property UnderlineThickness: Integer read FUnderlineThickness;
    { Family and style names, determined like FreeType does.
      Non-ASCII characters are replaced with '?' (like FreeType does). }
    property FamilyName: AnsiString read FFamilyName;
    property StyleName: AnsiString read FStyleName;
    property Italic: Boolean read FItalic;
    property Bold: Boolean read FBold;
    { Whether the font has kerning information we can read. }
    function HasKerning: Boolean;

    { Glyph index for given Unicode character, 0 if not found.
      @raises(EOpenTypeFontError If the font data is invalid.)
      @raises(EReadError If the font data is truncated.) }
    function GlyphIndex(const Code: Cardinal): Integer;
    { Advance width of the glyph, in font units.
      @raises(EOpenTypeFontError If the font data is invalid.)
      @raises(EReadError If the font data is truncated.) }
    function AdvanceWidth(const Glyph: Integer): Integer;
    { Kerning between glyphs, in font units.
      @raises(EOpenTypeFontError If the font data is invalid.)
      @raises(EReadError If the font data is truncated.) }
    function Kerning(const LeftGlyph, RightGlyph: Integer): Integer;
    { Get glyph outline, in font units.
      Glyphs without outline (like space) result in empty Outline.
      @raises(EOpenTypeFontError If the font data is invalid.)
      @raises(EReadError If the font data is truncated.) }
    procedure GetGlyphOutline(const Glyph: Integer; const Outline: TGlyphOutline);
  end;

implementation

uses Math, CastleUtils, CastleStreamUtils;

{ Records read directly from the font data ----------------------------------- }

type
  { All values in records below are big-endian in the file,
    use FixEndian to convert the fields we use. }

  { Offset table (at the beginning of the font file, or the face in TTC). }
  TOffsetTable = packed record
    SfntVersion: UInt32;
    NumTables, SearchRange, EntrySelector, RangeShift: UInt16;
  end;

  { Table record in the offset table. }
  TTableRecord = packed record
    Tag, CheckSum, Offset, Length: UInt32;
  end;

  { "head" table. }
  THeadTable = packed record
    MajorVersion, MinorVersion: UInt16;
    FontRevision: Int32;
    CheckSumAdjustment, MagicNumber: UInt32;
    Flags, UnitsPerEm: UInt16;
    Created, Modified: Int64;
    XMin, YMin, XMax, YMax: Int16;
    MacStyle, LowestRecPPEM: UInt16;
    FontDirectionHint, IndexToLocFormat, GlyphDataFormat: Int16;
  end;

  { "hhea" table. }
  THheaTable = packed record
    MajorVersion, MinorVersion: UInt16;
    Ascender, Descender, LineGap: Int16;
    AdvanceWidthMax: UInt16;
    MinLeftSideBearing, MinRightSideBearing, XMaxExtent: Int16;
    CaretSlopeRise, CaretSlopeRun, CaretOffset: Int16;
    Reserved: array [0..3] of Int16;
    MetricDataFormat: Int16;
    NumberOfHMetrics: UInt16;
  end;

  { Name record in the "name" table. }
  TNameRecord = packed record
    PlatformId, EncodingId, LanguageId, NameId, Length, StringOffset: UInt16;
  end;

  { Encoding record in the "cmap" table. }
  TEncodingRecord = packed record
    PlatformId, EncodingId: UInt16;
    SubtableOffset: UInt32;
  end;

  { Group in "cmap" format 12 subtable. }
  TSequentialMapGroup = packed record
    StartCharCode, EndCharCode, StartGlyphId: UInt32;
  end;

  { Subtable header in the "kern" table. }
  TKernSubtableHeader = packed record
    Version, Length, Coverage: UInt16;
  end;

  { Pair in "kern" format 0 subtable. }
  TKernPair = packed record
    Left, Right: UInt16;
    Value: Int16;
  end;

  { Glyph header in the "glyf" table. }
  TGlyphHeader = packed record
    NumberOfContours, XMin, YMin, XMax, YMax: Int16;
  end;

  { CFF header. }
  TCffHeader = packed record
    Major, Minor, HeaderSize, OffSize: Byte;
  end;

{ Convert big-endian value (read from the font data) to native endianess. }
procedure FixEndian(var Value: UInt16); overload;
begin
  Value := BEtoN(Value);
end;

procedure FixEndian(var Value: Int16); overload;
begin
  Value := Int16(BEtoN(UInt16(Value)));
end;

procedure FixEndian(var Value: UInt32); overload;
begin
  Value := BEtoN(Value);
end;

const
  { TrueType simple glyph flags. }
  FlagOnCurve = $01;
  FlagXShort = $02;
  FlagYShort = $04;
  FlagRepeat = $08;
  FlagXSameOrPositive = $10;
  FlagYSameOrPositive = $20;

  { TrueType composite glyph flags. }
  FlagArg1And2AreWords = $0001;
  FlagArgsAreXYValues = $0002;
  FlagWeHaveAScale = $0008;
  FlagMoreComponents = $0020;
  FlagWeHaveAnXAndYScale = $0040;
  FlagWeHaveATwoByTwo = $0080;
  FlagScaledComponentOffset = $0800;
  FlagUnscaledComponentOffset = $1000;

  MaxCompositeDepth = 8;
  MaxSubrDepth = 10;

function TagValue(const Tag: AnsiString): UInt32;
begin
  Assert(Length(Tag) = 4);
  Result :=
    (UInt32(Ord(Tag[1])) shl 24) or
    (UInt32(Ord(Tag[2])) shl 16) or
    (UInt32(Ord(Tag[3])) shl 8) or
     UInt32(Ord(Tag[4]));
end;

{ Convert UInt32 to Integer, clamping to MaxInt. }
function ClampToInt(const V: UInt32): Integer;
begin
  if V > UInt32(MaxInt) then
    Result := MaxInt
  else
    Result := Integer(V);
end;

function SubrBias(const Count: Integer): Integer;
begin
  if Count < 1240 then
    Result := 107
  else
  if Count < 33900 then
    Result := 1131
  else
    Result := 32768;
end;

{ TGlyphOutline -------------------------------------------------------------- }

procedure TGlyphOutline.Clear;
begin
  Count := 0;
end;

procedure TGlyphOutline.Add(const Kind: TOutlineCommandKind;
  const X0, Y0, X1, Y1, X2, Y2: Double);
begin
  if Count >= Length(Commands) then
    SetLength(Commands, Max(16, Length(Commands) * 2));
  Commands[Count].Kind := Kind;
  Commands[Count].X[0] := X0;
  Commands[Count].Y[0] := Y0;
  Commands[Count].X[1] := X1;
  Commands[Count].Y[1] := Y1;
  Commands[Count].X[2] := X2;
  Commands[Count].Y[2] := Y2;
  Inc(Count);
end;

procedure TGlyphOutline.MoveTo(const X, Y: Double);
begin
  Add(ocMove, X, Y, 0, 0, 0, 0);
end;

procedure TGlyphOutline.LineTo(const X, Y: Double);
begin
  Add(ocLine, X, Y, 0, 0, 0, 0);
end;

procedure TGlyphOutline.QuadTo(const CX, CY, X, Y: Double);
begin
  Add(ocQuad, CX, CY, X, Y, 0, 0);
end;

procedure TGlyphOutline.CubicTo(const C1X, C1Y, C2X, C2Y, X, Y: Double);
begin
  Add(ocCubic, C1X, C1Y, C2X, C2Y, X, Y);
end;

procedure TGlyphOutline.Assign(const Source: TGlyphOutline);
begin
  Commands := Copy(Source.Commands, 0, Source.Count);
  Count := Source.Count;
end;

procedure TGlyphOutline.Transform(const A, B, C, D: Double);
var
  I, J: Integer;
  X, Y: Double;
begin
  for I := 0 to Count - 1 do
    for J := 0 to 2 do
    begin
      X := Commands[I].X[J];
      Y := Commands[I].Y[J];
      Commands[I].X[J] := A * X + C * Y;
      Commands[I].Y[J] := B * X + D * Y;
    end;
end;

{ TOpenTypeFont: reading data ------------------------------------------------ }

procedure TOpenTypeFont.Error(const Message: String);
begin
  raise EOpenTypeFontError.Create(Message);
end;

procedure TOpenTypeFont.CheckRange(const Pos, Len: Int64);
begin
  if (Pos < 0) or (Len < 0) or (Pos + Len > FSize) then
    Error(Format('Font data truncated or invalid (%d bytes at position %d, font size %d)',
      [Len, Pos, FSize]));
end;

procedure TOpenTypeFont.SeekTo(const Pos: Int64);
begin
  CheckRange(Pos, 0);
  FData.Position := Pos;
end;

procedure TOpenTypeFont.Skip(const Count: Int64);
begin
  SeekTo(FData.Position + Count);
end;

function TOpenTypeFont.Position: Integer;
begin
  Result := FData.Position;
end;

function TOpenTypeFont.ReadUInt8: Byte;
begin
  FData.ReadBuffer(Result, SizeOf(Result));
end;

function TOpenTypeFont.ReadUInt16: UInt16;
begin
  FData.ReadBE(Result);
end;

function TOpenTypeFont.ReadInt16: Int16;
begin
  FData.ReadBE(Result);
end;

function TOpenTypeFont.ReadUInt32: UInt32;
begin
  FData.ReadBE(Result);
end;

function TOpenTypeFont.ReadInt32: Int32;
begin
  FData.ReadBE(Result);
end;

function TOpenTypeFont.ReadUIntOfSize(const Size: Integer): UInt32;
var
  High: Byte;
begin
  case Size of
    1: Result := ReadUInt8;
    2: Result := ReadUInt16;
    3:
      begin
        { separate statements, to read in the correct order }
        High := ReadUInt8;
        Result := (UInt32(High) shl 16) or ReadUInt16;
      end;
    4: Result := ReadUInt32;
    else
      raise EOpenTypeFontError.CreateFmt('Invalid CFF offset size %d', [Size]);
  end;
end;

function TOpenTypeFont.ReadOffset32: Integer;
var
  V: UInt32;
begin
  V := ReadUInt32;
  if V > UInt32(FSize) then
    Error('Invalid offset in font data');
  Result := Integer(V);
end;

function TOpenTypeFont.FindTable(const Tag: AnsiString; const Required: Boolean): Integer;
var
  T: UInt32;
  I: Integer;
begin
  T := TagValue(Tag);
  for I := 0 to Length(FTableTags) - 1 do
    if FTableTags[I] = T then
      Exit(FTableOffsets[I]);
  if Required then
    Error('Font is missing a required table "' + String(Tag) + '"');
  Result := -1;
end;

{ TOpenTypeFont: loading ----------------------------------------------------- }

constructor TOpenTypeFont.Create(const Stream: TCustomMemoryStream; const FaceIndex: Integer);
begin
  inherited Create;
  FData := Stream;
  if (FData.Size < 12) or (FData.Size > MaxInt) then
    Error('Invalid font data size');
  FSize := Integer(FData.Size);
  ReadTables(FaceIndex);
end;

destructor TOpenTypeFont.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TOpenTypeFont.ReadTables(const FaceIndex: Integer);
var
  Base, I, Head, Maxp, Hhea, Os2, Post, Cff: Integer;
  OffsetTable: TOffsetTable;
  TableRecord: TTableRecord;
  HeadTable: THeadTable;
  HheaTable: THheaTable;
  MacStyle: UInt16;
begin
  { TrueType Collection header }
  Base := 0;
  SeekTo(0);
  if ReadUInt32 = TagValue('ttcf') then
  begin
    ReadUInt32; // version
    FNumFaces := ClampToInt(ReadUInt32);
    if (FaceIndex < 0) or (FaceIndex >= FNumFaces) then
      Error('Invalid face index in TrueType Collection');
    Skip(4 * Int64(FaceIndex));
    Base := ReadOffset32;
  end else
  begin
    if FaceIndex > 0 then
      Error('Invalid face index, font file contains only one face');
    FNumFaces := 1;
  end;

  { Offset table and table records }
  SeekTo(Base);
  FData.ReadBuffer(OffsetTable, SizeOf(OffsetTable));
  FixEndian(OffsetTable.SfntVersion);
  FixEndian(OffsetTable.NumTables);
  if (OffsetTable.SfntVersion <> $00010000) and
     (OffsetTable.SfntVersion <> TagValue('OTTO')) and
     (OffsetTable.SfntVersion <> TagValue('true')) then
    Error('Not a TrueType or OpenType font');
  SetLength(FTableTags, OffsetTable.NumTables);
  SetLength(FTableOffsets, OffsetTable.NumTables);
  for I := 0 to OffsetTable.NumTables - 1 do
  begin
    FData.ReadBuffer(TableRecord, SizeOf(TableRecord));
    FixEndian(TableRecord.Tag);
    FixEndian(TableRecord.Offset);
    FixEndian(TableRecord.Length);
    CheckRange(TableRecord.Offset, TableRecord.Length);
    FTableTags[I] := TableRecord.Tag;
    FTableOffsets[I] := Integer(TableRecord.Offset);
  end;

  { head }
  Head := FindTable('head', true);
  SeekTo(Head);
  FData.ReadBuffer(HeadTable, SizeOf(HeadTable));
  FixEndian(HeadTable.UnitsPerEm);
  FixEndian(HeadTable.XMin);
  FixEndian(HeadTable.YMin);
  FixEndian(HeadTable.XMax);
  FixEndian(HeadTable.YMax);
  FixEndian(HeadTable.MacStyle);
  FixEndian(HeadTable.IndexToLocFormat);
  FUnitsPerEm := HeadTable.UnitsPerEm;
  if FUnitsPerEm = 0 then
    Error('Invalid font unitsPerEm');
  FXMin := HeadTable.XMin;
  FYMin := HeadTable.YMin;
  FXMax := HeadTable.XMax;
  FYMax := HeadTable.YMax;
  MacStyle := HeadTable.MacStyle;
  FIndexToLocFormat := HeadTable.IndexToLocFormat;

  { maxp }
  Maxp := FindTable('maxp', true);
  SeekTo(Maxp);
  ReadUInt32; // version
  FNumGlyphs := ReadUInt16;

  { hhea }
  Hhea := FindTable('hhea', true);
  SeekTo(Hhea);
  FData.ReadBuffer(HheaTable, SizeOf(HheaTable));
  FixEndian(HheaTable.Ascender);
  FixEndian(HheaTable.Descender);
  FixEndian(HheaTable.LineGap);
  FixEndian(HheaTable.AdvanceWidthMax);
  FixEndian(HheaTable.NumberOfHMetrics);
  FAscender := HheaTable.Ascender;
  FDescender := HheaTable.Descender;
  FLineGap := HheaTable.LineGap;
  FAdvanceWidthMax := HheaTable.AdvanceWidthMax;
  FNumHMetrics := HheaTable.NumberOfHMetrics;
  if FNumHMetrics < 1 then
    Error('Invalid font numberOfHMetrics');

  { hmtx }
  FHmtx := FindTable('hmtx', true);
  CheckRange(FHmtx, 4 * FNumHMetrics);

  { OS/2 }
  Os2 := FindTable('OS/2', false);
  FHasOS2 := Os2 <> -1;
  if FHasOS2 then
  begin
    SeekTo(Os2 + 62);
    FFsSelection := ReadUInt16;
  end;

  { Style flags, like FreeType sfnt_load_face }
  if FHasOS2 then
  begin
    FItalic := (FFsSelection and (1 or 512)) <> 0; // italic or oblique
    FBold := (FFsSelection and 32) <> 0;
  end else
  begin
    FBold := (MacStyle and 1) <> 0;
    FItalic := (MacStyle and 2) <> 0;
  end;

  { post }
  Post := FindTable('post', false);
  if Post <> -1 then
  begin
    SeekTo(Post + 8);
    FUnderlinePosition := ReadInt16;
    FUnderlineThickness := ReadInt16;
  end;

  SelectCmap;
  ReadKern;
  ReadNames;

  { outlines }
  FFDSelect := -1;
  Cff := FindTable('CFF ', false);
  if Cff <> -1 then
  begin
    FIsCff := true;
    ReadCff(Cff);
  end else
  begin
    FGlyf := FindTable('glyf', true);
    FLoca := FindTable('loca', true);
  end;
end;

{ TOpenTypeFont: names ------------------------------------------------------- }

function TOpenTypeFont.FindName(const NameId: Integer; out Name: AnsiString): Boolean;

  { Convert name, replacing non-ASCII with '?', like FreeType does. }
  function ConvertName(const Pos, Len: Integer; const Utf16: Boolean): AnsiString;
  var
    I, C: Integer;
  begin
    SeekTo(Pos);
    Result := '';
    if Utf16 then
    begin
      for I := 0 to Len div 2 - 1 do
      begin
        C := ReadUInt16;
        if C = 0 then
          Break;
        if (C < 32) or (C > 127) then
          C := Ord('?');
        Result := Result + AnsiChar(C);
      end;
    end else
    begin
      for I := 0 to Len - 1 do
      begin
        C := ReadUInt8;
        if C = 0 then
          Break;
        if (C < 32) or (C > 127) then
          C := Ord('?');
        Result := Result + AnsiChar(C);
      end;
    end;
  end;

var
  NameTable, Count, Storage, I, Pos: Integer;
  Rec: TNameRecord;
  { Found records: Windows English, Windows (or Unicode) any, Apple Roman }
  WinPos, WinLen, AnyPos, AnyLen, ApplePos, AppleLen: Integer;
begin
  Result := false;
  Name := '';
  NameTable := FindTable('name', false);
  if NameTable = -1 then
    Exit;
  SeekTo(NameTable);
  ReadUInt16; // format
  Count := ReadUInt16;
  Storage := NameTable + ReadUInt16;
  WinPos := -1;
  WinLen := 0;
  AnyPos := -1;
  AnyLen := 0;
  ApplePos := -1;
  AppleLen := 0;
  for I := 0 to Count - 1 do
  begin
    FData.ReadBuffer(Rec, SizeOf(Rec));
    FixEndian(Rec.PlatformId);
    FixEndian(Rec.EncodingId);
    FixEndian(Rec.LanguageId);
    FixEndian(Rec.NameId);
    FixEndian(Rec.Length);
    FixEndian(Rec.StringOffset);
    if (Rec.NameId <> NameId) or (Rec.Length = 0) then
      Continue;
    Pos := Storage + Rec.StringOffset;
    { ignore invalid records, pointing outside of the font data }
    if Int64(Pos) + Rec.Length > FSize then
      Continue;
    if (Rec.PlatformId = 3) and
       ((Rec.EncodingId = 0) or (Rec.EncodingId = 1) or (Rec.EncodingId = 10)) then
    begin
      if ((Rec.LanguageId and $3FF) = $009) and (WinPos = -1) then
      begin
        WinPos := Pos;
        WinLen := Rec.Length;
      end;
      if AnyPos = -1 then
      begin
        AnyPos := Pos;
        AnyLen := Rec.Length;
      end;
    end else
    if Rec.PlatformId = 0 then
    begin
      if AnyPos = -1 then
      begin
        AnyPos := Pos;
        AnyLen := Rec.Length;
      end;
    end else
    if (Rec.PlatformId = 1) and (Rec.EncodingId = 0) and (Rec.LanguageId = 0) then
    begin
      if ApplePos = -1 then
      begin
        ApplePos := Pos;
        AppleLen := Rec.Length;
      end;
    end;
  end;

  if WinPos <> -1 then
    Name := ConvertName(WinPos, WinLen, true)
  else
  if AnyPos <> -1 then
    Name := ConvertName(AnyPos, AnyLen, true)
  else
  if ApplePos <> -1 then
    Name := ConvertName(ApplePos, AppleLen, false)
  else
    Exit;
  Result := true;
end;

procedure TOpenTypeFont.ReadNames;
const
  NameFamily = 1;
  NameSubfamily = 2;
  NameTypographicFamily = 16;
  NameTypographicSubfamily = 17;
  NameWwsFamily = 21;
  NameWwsSubfamily = 22;
begin
  { Choose names like FreeType sfnt_load_face.
    fsSelection bit 8 means "WWS": font names are already WWS-conformant. }
  if FHasOS2 and ((FFsSelection and 256) <> 0) then
  begin
    if not FindName(NameTypographicFamily, FFamilyName) then
      FindName(NameFamily, FFamilyName);
    if not FindName(NameTypographicSubfamily, FStyleName) then
      FindName(NameSubfamily, FStyleName);
  end else
  begin
    if not FindName(NameWwsFamily, FFamilyName) then
      if not FindName(NameTypographicFamily, FFamilyName) then
        FindName(NameFamily, FFamilyName);
    if not FindName(NameWwsSubfamily, FStyleName) then
      if not FindName(NameTypographicSubfamily, FStyleName) then
        FindName(NameSubfamily, FStyleName);
  end;
  if FStyleName = '' then
    FStyleName := 'Regular';
end;

{ TOpenTypeFont: cmap -------------------------------------------------------- }

procedure TOpenTypeFont.SelectCmap;
var
  Cmap, Count, I, Fmt, Rank, BestRank: Integer;
  Rec: TEncodingRecord;
  Sub: Int64;
begin
  FCmapSubtable := -1;
  Cmap := FindTable('cmap', false);
  if Cmap = -1 then
    Exit;
  SeekTo(Cmap);
  ReadUInt16; // version
  Count := ReadUInt16;
  BestRank := 0;
  for I := 0 to Count - 1 do
  begin
    { we read subtable format below, so seek to the record explicitly }
    SeekTo(Cmap + 4 + 8 * I);
    FData.ReadBuffer(Rec, SizeOf(Rec));
    FixEndian(Rec.PlatformId);
    FixEndian(Rec.EncodingId);
    FixEndian(Rec.SubtableOffset);
    Sub := Int64(Cmap) + Rec.SubtableOffset;
    if Sub + 4 > FSize then
      Continue;
    SeekTo(Sub);
    Fmt := ReadUInt16;
    if not ((Fmt = 0) or (Fmt = 4) or (Fmt = 6) or (Fmt = 12)) then
      Continue;

    { Prefer full Unicode (UCS-4), then Unicode BMP, then symbol, then Apple Roman. }
    Rank := 0;
    if ((Rec.PlatformId = 3) and (Rec.EncodingId = 10)) or
       ((Rec.PlatformId = 0) and ((Rec.EncodingId = 4) or (Rec.EncodingId = 6))) then
    begin
      if Fmt = 12 then
        Rank := 5
      else
        Rank := 4;
    end else
    if ((Rec.PlatformId = 3) and (Rec.EncodingId = 1)) or (Rec.PlatformId = 0) then
      Rank := 3
    else
    if (Rec.PlatformId = 3) and (Rec.EncodingId = 0) then
      Rank := 2
    else
    if (Rec.PlatformId = 1) and (Rec.EncodingId = 0) then
      Rank := 1;

    if Rank > BestRank then
    begin
      BestRank := Rank;
      FCmapSubtable := Integer(Sub);
      FCmapFormat := Fmt;
      FCmapSymbol := (Rec.PlatformId = 3) and (Rec.EncodingId = 0);
    end;
  end;
end;

function TOpenTypeFont.CmapLookup(const Code: Cardinal): Cardinal;
var
  Sub, First, Cnt, SegX2, Segs, Ends, Starts, Deltas, RangeOffsets: Integer;
  Lo, Hi, Mid, Seg, Start, Delta, RangeOffsetPos, RangeOffset, G: Integer;
  NGroups: UInt32;
  GLo, GHi, GMid: Int64;
  Group: TSequentialMapGroup;
begin
  Result := 0;
  Sub := FCmapSubtable;
  case FCmapFormat of
    0:
      begin
        if Code < 256 then
        begin
          SeekTo(Sub + 6 + Integer(Code));
          Result := ReadUInt8;
        end;
      end;
    6:
      begin
        SeekTo(Sub + 6);
        First := ReadUInt16;
        Cnt := ReadUInt16;
        if (Code >= Cardinal(First)) and (Code < Cardinal(First + Cnt)) then
        begin
          Skip(2 * (Integer(Code) - First));
          Result := ReadUInt16;
        end;
      end;
    4:
      begin
        if Code > $FFFF then
          Exit;
        SeekTo(Sub + 6);
        SegX2 := ReadUInt16;
        Segs := SegX2 div 2;
        Ends := Sub + 14;
        Starts := Ends + SegX2 + 2;
        Deltas := Starts + SegX2;
        RangeOffsets := Deltas + SegX2;
        { binary search for the first segment with end >= Code }
        Lo := 0;
        Hi := Segs - 1;
        Seg := -1;
        while Lo <= Hi do
        begin
          Mid := (Lo + Hi) div 2;
          SeekTo(Ends + 2 * Mid);
          if Cardinal(ReadUInt16) < Code then
            Lo := Mid + 1
          else
          begin
            Seg := Mid;
            Hi := Mid - 1;
          end;
        end;
        if Seg < 0 then
          Exit;
        SeekTo(Starts + 2 * Seg);
        Start := ReadUInt16;
        if Code < Cardinal(Start) then
          Exit;
        SeekTo(Deltas + 2 * Seg);
        Delta := ReadUInt16;
        RangeOffsetPos := RangeOffsets + 2 * Seg;
        SeekTo(RangeOffsetPos);
        RangeOffset := ReadUInt16;
        if RangeOffset = 0 then
          Result := (Integer(Code) + Delta) and $FFFF
        else
        begin
          SeekTo(RangeOffsetPos + RangeOffset + 2 * (Integer(Code) - Start));
          G := ReadUInt16;
          if G <> 0 then
            Result := (G + Delta) and $FFFF;
        end;
      end;
    12:
      begin
        SeekTo(Sub + 12);
        NGroups := ReadUInt32;
        GLo := 0;
        GHi := Int64(NGroups) - 1;
        while GLo <= GHi do
        begin
          GMid := (GLo + GHi) div 2;
          SeekTo(Int64(Sub) + 16 + SizeOf(TSequentialMapGroup) * GMid);
          FData.ReadBuffer(Group, SizeOf(Group));
          FixEndian(Group.StartCharCode);
          FixEndian(Group.EndCharCode);
          FixEndian(Group.StartGlyphId);
          if Code < Group.StartCharCode then
            GHi := GMid - 1
          else
          if Code > Group.EndCharCode then
            GLo := GMid + 1
          else
          begin
            { calculate in Int64 to avoid overflow on invalid data;
              GlyphIndex will reject too large result }
            Result := Cardinal(Min(
              Int64(Group.StartGlyphId) + (Code - Group.StartCharCode),
              Int64($FFFFFFFF)));
            Exit;
          end;
        end;
      end;
  end;
end;

function TOpenTypeFont.GlyphIndex(const Code: Cardinal): Integer;
var
  G: Cardinal;
begin
  if FCmapSubtable = -1 then
    Exit(0);
  G := CmapLookup(Code);
  { Symbol fonts map characters to the $F000..$F0FF range. }
  if (G = 0) and FCmapSymbol and (Code <= $FF) then
    G := CmapLookup($F000 + Code);
  if G >= Cardinal(FNumGlyphs) then
    G := 0;
  Result := G;
end;

{ TOpenTypeFont: metrics and kerning ----------------------------------------- }

function TOpenTypeFont.AdvanceWidth(const Glyph: Integer): Integer;
begin
  if (Glyph >= 0) and (Glyph < FNumHMetrics) then
    SeekTo(FHmtx + 4 * Glyph)
  else
    SeekTo(FHmtx + 4 * (FNumHMetrics - 1));
  Result := ReadUInt16;
end;

procedure TOpenTypeFont.ReadKern;
var
  Kern, Version, NumSubtables, Pos, I, NPairs: Integer;
  Header: TKernSubtableHeader;
begin
  FKernPairsCount := 0;
  Kern := FindTable('kern', false);
  if Kern = -1 then
    Exit;
  SeekTo(Kern);
  Version := ReadUInt16;
  if Version <> 0 then
    Exit; // Apple "kern" table version is not supported
  NumSubtables := ReadUInt16;
  for I := 0 to NumSubtables - 1 do
  begin
    Pos := Position;
    FData.ReadBuffer(Header, SizeOf(Header));
    FixEndian(Header.Length);
    FixEndian(Header.Coverage);
    { format 0, horizontal, not minimum, not cross-stream }
    if ((Header.Coverage shr 8) = 0) and ((Header.Coverage and $7) = $1) then
    begin
      NPairs := ReadUInt16;
      Skip(6); // searchRange, entrySelector, rangeShift
      CheckRange(Position, SizeOf(TKernPair) * NPairs);
      FKernPairsPos := Position;
      FKernPairsCount := NPairs;
      Exit;
    end;
    if Header.Length < SizeOf(Header) then
      Exit; // invalid subtable length, avoid infinite loop
    SeekTo(Pos + Header.Length);
  end;
end;

function TOpenTypeFont.HasKerning: Boolean;
begin
  Result := FKernPairsCount <> 0;
end;

function TOpenTypeFont.Kerning(const LeftGlyph, RightGlyph: Integer): Integer;
var
  Key, K: UInt32;
  Lo, Hi, Mid: Integer;
  Pair: TKernPair;
begin
  Result := 0;
  if (FKernPairsCount = 0) or
     (LeftGlyph < 0) or (LeftGlyph > $FFFF) or
     (RightGlyph < 0) or (RightGlyph > $FFFF) then
    Exit;
  Key := (UInt32(LeftGlyph) shl 16) or UInt32(RightGlyph);
  Lo := 0;
  Hi := FKernPairsCount - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    SeekTo(FKernPairsPos + SizeOf(TKernPair) * Mid);
    FData.ReadBuffer(Pair, SizeOf(Pair));
    FixEndian(Pair.Left);
    FixEndian(Pair.Right);
    FixEndian(Pair.Value);
    K := (UInt32(Pair.Left) shl 16) or Pair.Right;
    if K < Key then
      Lo := Mid + 1
    else
    if K > Key then
      Hi := Mid - 1
    else
      Exit(Pair.Value);
  end;
end;

{ TOpenTypeFont: glyph outlines ---------------------------------------------- }

procedure TOpenTypeFont.GetGlyphOutline(const Glyph: Integer; const Outline: TGlyphOutline);
var
  G: Integer;
  Points: TTrueTypePoints;
begin
  Outline.Clear;
  G := Glyph;
  if (G < 0) or (G >= FNumGlyphs) then
    G := 0;
  if FIsCff then
    LoadCffGlyph(G, Outline)
  else
  begin
    Points.Count := 0;
    Points.ContourCount := 0;
    LoadTrueTypeGlyph(G, Points, 0);
    TrueTypePointsToOutline(Points, Outline);
  end;
end;

{ TOpenTypeFont: TrueType outlines ------------------------------------------- }

procedure TOpenTypeFont.GlyphLocation(const Glyph: Integer; out Pos, Len: Integer);
var
  A, B: Int64;
begin
  if FIndexToLocFormat = 0 then
  begin
    SeekTo(FLoca + 2 * Int64(Glyph));
    A := Int64(ReadUInt16) * 2;
    B := Int64(ReadUInt16) * 2;
  end else
  begin
    SeekTo(FLoca + 4 * Int64(Glyph));
    A := ReadUInt32;
    B := ReadUInt32;
  end;
  if B < A then
    Error('Invalid font loca table');
  CheckRange(Int64(FGlyf) + A, B - A);
  Pos := Integer(FGlyf + A);
  Len := Integer(B - A);
end;

procedure TOpenTypeFont.LoadTrueTypeGlyph(const Glyph: Integer;
  var Points: TTrueTypePoints; const Depth: Integer);
var
  Pos, Len: Integer;
  Header: TGlyphHeader;
begin
  if Depth > MaxCompositeDepth then
    Error('Composite glyph nesting too deep');
  if (Glyph < 0) or (Glyph >= FNumGlyphs) then
    Exit;
  GlyphLocation(Glyph, Pos, Len);
  if Len = 0 then
    Exit; // empty glyph, like space
  SeekTo(Pos);
  FData.ReadBuffer(Header, SizeOf(Header));
  FixEndian(Header.NumberOfContours);
  { glyph data follows the header, at current position }
  if Header.NumberOfContours >= 0 then
    LoadTrueTypeSimple(Header.NumberOfContours, Points)
  else
    LoadTrueTypeComposite(Points, Depth);
end;

procedure TOpenTypeFont.LoadTrueTypeSimple(const NumContours: Integer;
  var Points: TTrueTypePoints);
var
  I, K, E, PrevEnd, NumPoints, BaseIndex, Rep, D: Integer;
  Flags: array of Byte;
  FlagsCount: Integer;
  F: Byte;
  X, Y: Integer;
begin
  BaseIndex := Points.Count;

  { contour ends }
  if Points.ContourCount + NumContours > Length(Points.ContourEnds) then
    SetLength(Points.ContourEnds, Points.ContourCount + NumContours + 16);
  PrevEnd := -1;
  for I := 0 to NumContours - 1 do
  begin
    E := ReadUInt16;
    if E < PrevEnd then
      Error('Invalid glyph contour ends');
    PrevEnd := E;
    Points.ContourEnds[Points.ContourCount + I] := BaseIndex + E;
  end;
  NumPoints := PrevEnd + 1;

  { instructions (ignored, we don't do hinting) }
  Skip(ReadUInt16);

  { flags }
  SetLength(Flags, NumPoints);
  FlagsCount := 0;
  while FlagsCount < NumPoints do
  begin
    F := ReadUInt8;
    Flags[FlagsCount] := F;
    Inc(FlagsCount);
    if (F and FlagRepeat) <> 0 then
    begin
      Rep := ReadUInt8;
      for K := 1 to Rep do
        if FlagsCount < NumPoints then
        begin
          Flags[FlagsCount] := F;
          Inc(FlagsCount);
        end;
    end;
  end;

  { make space for points }
  if Points.Count + NumPoints > Length(Points.X) then
  begin
    SetLength(Points.X, Points.Count + NumPoints + 64);
    SetLength(Points.Y, Points.Count + NumPoints + 64);
    SetLength(Points.OnCurve, Points.Count + NumPoints + 64);
  end;

  { x coordinates }
  X := 0;
  for I := 0 to NumPoints - 1 do
  begin
    F := Flags[I];
    if (F and FlagXShort) <> 0 then
    begin
      D := ReadUInt8;
      if (F and FlagXSameOrPositive) <> 0 then
        X := X + D
      else
        X := X - D;
    end else
    if (F and FlagXSameOrPositive) = 0 then
      X := X + ReadInt16;
    Points.X[BaseIndex + I] := X;
    Points.OnCurve[BaseIndex + I] := (F and FlagOnCurve) <> 0;
  end;

  { y coordinates }
  Y := 0;
  for I := 0 to NumPoints - 1 do
  begin
    F := Flags[I];
    if (F and FlagYShort) <> 0 then
    begin
      D := ReadUInt8;
      if (F and FlagYSameOrPositive) <> 0 then
        Y := Y + D
      else
        Y := Y - D;
    end else
    if (F and FlagYSameOrPositive) = 0 then
      Y := Y + ReadInt16;
    Points.Y[BaseIndex + I] := Y;
  end;

  Points.Count := Points.Count + NumPoints;
  Points.ContourCount := Points.ContourCount + NumContours;
end;

procedure TOpenTypeFont.LoadTrueTypeComposite(var Points: TTrueTypePoints;
  const Depth: Integer);
var
  Flags, Component, Arg1, Arg2, CompositeFirst, First, I, P1, P2, NextPos: Integer;
  A, B, C, D, DX, DY, X, Y: Double;
begin
  { index of the first point of this composite glyph in Points }
  CompositeFirst := Points.Count;
  repeat
    Flags := ReadUInt16;
    Component := ReadUInt16;
    if (Flags and FlagArg1And2AreWords) <> 0 then
    begin
      if (Flags and FlagArgsAreXYValues) <> 0 then
      begin
        Arg1 := ReadInt16;
        Arg2 := ReadInt16;
      end else
      begin
        Arg1 := ReadUInt16;
        Arg2 := ReadUInt16;
      end;
    end else
    begin
      Arg1 := ReadUInt8;
      Arg2 := ReadUInt8;
      if (Flags and FlagArgsAreXYValues) <> 0 then
      begin
        { signed bytes }
        if Arg1 >= 128 then
          Arg1 := Arg1 - 256;
        if Arg2 >= 128 then
          Arg2 := Arg2 - 256;
      end;
    end;

    A := 1;
    B := 0;
    C := 0;
    D := 1;
    if (Flags and FlagWeHaveAScale) <> 0 then
    begin
      A := ReadInt16 / 16384;
      D := A;
    end else
    if (Flags and FlagWeHaveAnXAndYScale) <> 0 then
    begin
      A := ReadInt16 / 16384;
      D := ReadInt16 / 16384;
    end else
    if (Flags and FlagWeHaveATwoByTwo) <> 0 then
    begin
      A := ReadInt16 / 16384;
      B := ReadInt16 / 16384;
      C := ReadInt16 / 16384;
      D := ReadInt16 / 16384;
    end;

    { loading the component moves the position, remember where to continue }
    NextPos := Position;
    First := Points.Count;
    LoadTrueTypeGlyph(Component, Points, Depth + 1);
    SeekTo(NextPos);

    { transform component points }
    for I := First to Points.Count - 1 do
    begin
      X := Points.X[I];
      Y := Points.Y[I];
      Points.X[I] := A * X + C * Y;
      Points.Y[I] := B * X + D * Y;
    end;

    if (Flags and FlagArgsAreXYValues) <> 0 then
    begin
      DX := Arg1;
      DY := Arg2;
      if ((Flags and FlagScaledComponentOffset) <> 0) and
         ((Flags and FlagUnscaledComponentOffset) = 0) then
      begin
        X := DX;
        Y := DY;
        DX := A * X + C * Y;
        DY := B * X + D * Y;
      end;
    end else
    begin
      { point matching: Arg1 is a point in this composite glyph
        (among already loaded components), Arg2 is a point in the new component }
      P1 := CompositeFirst + Arg1;
      P2 := First + Arg2;
      if (P1 >= First) or (P2 >= Points.Count) then
        Error('Invalid composite glyph point matching');
      DX := Points.X[P1] - Points.X[P2];
      DY := Points.Y[P1] - Points.Y[P2];
    end;

    for I := First to Points.Count - 1 do
    begin
      Points.X[I] := Points.X[I] + DX;
      Points.Y[I] := Points.Y[I] + DY;
    end;
  until (Flags and FlagMoreComponents) = 0;
end;

procedure TOpenTypeFont.TrueTypePointsToOutline(const Points: TTrueTypePoints;
  const Outline: TGlyphOutline);
var
  Contour, Start, EndIndex, N, K, I, FirstOn, Offset: Integer;
  BeginX, BeginY, CtrlX, CtrlY: Double;
  HasCtrl: Boolean;
begin
  Start := 0;
  for Contour := 0 to Points.ContourCount - 1 do
  begin
    EndIndex := Points.ContourEnds[Contour];
    N := EndIndex - Start + 1;
    if N <= 0 then
    begin
      Start := EndIndex + 1;
      Continue;
    end;

    { find the first on-curve point }
    FirstOn := -1;
    for K := 0 to N - 1 do
      if Points.OnCurve[Start + K] then
      begin
        FirstOn := K;
        Break;
      end;

    if FirstOn = -1 then
    begin
      { all points off-curve: start at the middle of the first two }
      BeginX := (Points.X[Start] + Points.X[Start + (1 mod N)]) / 2;
      BeginY := (Points.Y[Start] + Points.Y[Start + (1 mod N)]) / 2;
      Offset := 0;
    end else
    begin
      BeginX := Points.X[Start + FirstOn];
      BeginY := Points.Y[Start + FirstOn];
      Offset := FirstOn + 1;
    end;

    Outline.MoveTo(BeginX, BeginY);
    HasCtrl := false;
    CtrlX := 0;
    CtrlY := 0;
    for K := 0 to N - 1 do
    begin
      I := Start + (Offset + K) mod N;
      if Points.OnCurve[I] then
      begin
        if HasCtrl then
        begin
          Outline.QuadTo(CtrlX, CtrlY, Points.X[I], Points.Y[I]);
          HasCtrl := false;
        end else
          Outline.LineTo(Points.X[I], Points.Y[I]);
      end else
      begin
        if HasCtrl then
          Outline.QuadTo(CtrlX, CtrlY,
            (CtrlX + Points.X[I]) / 2,
            (CtrlY + Points.Y[I]) / 2);
        CtrlX := Points.X[I];
        CtrlY := Points.Y[I];
        HasCtrl := true;
      end;
    end;

    { close back to the beginning }
    if HasCtrl then
      Outline.QuadTo(CtrlX, CtrlY, BeginX, BeginY)
    else
      Outline.LineTo(BeginX, BeginY);

    Start := EndIndex + 1;
  end;
end;

{ TOpenTypeFont: CFF --------------------------------------------------------- }

function TOpenTypeFont.ReadCffIndex(const Pos: Integer): TCffIndex;
var
  I: Integer;
  Prev, Cur: UInt32;
begin
  SeekTo(Pos);
  Result.Count := ReadUInt16;
  if Result.Count = 0 then
  begin
    Result.OffSize := 0;
    Result.OffsetsPos := Pos + 2;
    Result.DataBase := Pos + 1;
    Result.EndPos := Pos + 2;
    Exit;
  end;
  Result.OffSize := ReadUInt8;
  if (Result.OffSize < 1) or (Result.OffSize > 4) then
    Error('Invalid CFF INDEX offSize');
  Result.OffsetsPos := Position;
  CheckRange(Result.OffsetsPos, Int64(Result.Count + 1) * Result.OffSize);
  Result.DataBase := Result.OffsetsPos + (Result.Count + 1) * Result.OffSize - 1;

  { validate offsets: they must be >= 1 and non-decreasing }
  Prev := 1;
  for I := 0 to Result.Count do
  begin
    Cur := ReadUIntOfSize(Result.OffSize);
    if Cur < Prev then
      Error('Invalid CFF INDEX offsets');
    Prev := Cur;
  end;
  CheckRange(Result.DataBase, Prev);
  Result.EndPos := Result.DataBase + Integer(Prev);
end;

procedure TOpenTypeFont.CffIndexItem(const Index: TCffIndex; const I: Integer;
  out Pos, Len: Integer);
var
  A, B: Integer;
begin
  if (I < 0) or (I >= Index.Count) then
    Error('CFF INDEX item out of range');
  SeekTo(Index.OffsetsPos + Int64(I) * Index.OffSize);
  { offsets were validated by ReadCffIndex, so they fit in Integer }
  A := Integer(ReadUIntOfSize(Index.OffSize));
  B := Integer(ReadUIntOfSize(Index.OffSize));
  Pos := Index.DataBase + A;
  Len := B - A;
end;

procedure TOpenTypeFont.ReadCffDict(const Pos, Len: Integer; out Dict: TCffDict);

  { Read real number (operand 30), after the initial 30 byte. }
  function ReadReal: Double;
  var
    Nibble, B, ExpSign, Exponent, DigitsAfterPoint: Integer;
    Mantissa, Exponent10: Double;
    Negative, AfterPoint, InExponent, Done, High: Boolean;
  begin
    Mantissa := 0;
    Negative := false;
    AfterPoint := false;
    InExponent := false;
    ExpSign := 1;
    Exponent := 0;
    DigitsAfterPoint := 0;
    Done := false;
    B := 0;
    High := true;
    while not Done do
    begin
      if High then
      begin
        B := ReadUInt8;
        Nibble := B shr 4;
      end else
        Nibble := B and 15;
      High := not High;
      case Nibble of
        0..9:
          if InExponent then
          begin
            if Exponent < 1000 then
              Exponent := Exponent * 10 + Nibble;
          end else
          begin
            Mantissa := Mantissa * 10 + Nibble;
            if AfterPoint then
              Inc(DigitsAfterPoint);
          end;
        10: AfterPoint := true;
        11: InExponent := true;
        12:
          begin
            InExponent := true;
            ExpSign := -1;
          end;
        14: Negative := true;
        15: Done := true;
        else ; // 13 is reserved
      end;
    end;
    Exponent10 := ExpSign * Exponent - DigitsAfterPoint;
    Result := Mantissa * Power(10.0, Exponent10);
    if Negative then
      Result := -Result;
  end;

var
  Operands: array [0..47] of Double;
  OperandsCount: Integer;

  function Operand(const I: Integer): Double;
  begin
    if I < OperandsCount then
      Result := Operands[I]
    else
      Result := 0;
  end;

  procedure AddOperand(const V: Double);
  begin
    if OperandsCount < Length(Operands) then
    begin
      Operands[OperandsCount] := V;
      Inc(OperandsCount);
    end;
  end;

var
  EndPos, B0, Op, I: Integer;
begin
  Dict.CharStringsOffset := -1;
  Dict.PrivateSize := 0;
  Dict.PrivateOffset := -1;
  Dict.SubrsOffset := -1;
  Dict.FDArrayOffset := -1;
  Dict.FDSelectOffset := -1;
  Dict.HasFontMatrix := false;
  for I := 0 to 5 do
    Dict.FontMatrix[I] := 0;

  OperandsCount := 0;
  SeekTo(Pos);
  EndPos := Pos + Len;
  while Position < EndPos do
  begin
    B0 := ReadUInt8;
    if B0 <= 21 then
    begin
      if B0 = 12 then
        Op := 1200 + ReadUInt8
      else
        Op := B0;
      case Op of
        17: Dict.CharStringsOffset := Round(Operand(0));
        18:
          begin
            Dict.PrivateSize := Round(Operand(0));
            Dict.PrivateOffset := Round(Operand(1));
          end;
        19: Dict.SubrsOffset := Round(Operand(0));
        1207:
          if OperandsCount >= 4 then
          begin
            Dict.HasFontMatrix := true;
            for I := 0 to 5 do
              Dict.FontMatrix[I] := Operand(I);
          end;
        1236: Dict.FDArrayOffset := Round(Operand(0));
        1237: Dict.FDSelectOffset := Round(Operand(0));
        else ;
      end;
      OperandsCount := 0;
    end else
    if B0 = 28 then
      AddOperand(ReadInt16)
    else
    if B0 = 29 then
      AddOperand(ReadInt32)
    else
    if B0 = 30 then
      AddOperand(ReadReal)
    else
    if (B0 >= 32) and (B0 <= 246) then
      AddOperand(B0 - 139)
    else
    if (B0 >= 247) and (B0 <= 250) then
      AddOperand((B0 - 247) * 256 + ReadUInt8 + 108)
    else
    if (B0 >= 251) and (B0 <= 254) then
      AddOperand(-(B0 - 251) * 256 - ReadUInt8 - 108)
    else
      Error('Invalid byte in CFF DICT');
  end;
end;

function TOpenTypeFont.ReadCffPrivate(const Dict: TCffDict): TCffPrivate;
var
  PrivateDict: TCffDict;
  PrivateStart: Integer;
begin
  Result.HasSubrs := false;
  Result.Bias := 0;
  if (Dict.PrivateOffset < 0) or (Dict.PrivateSize <= 0) then
    Exit;
  PrivateStart := FCffStart + Dict.PrivateOffset;
  CheckRange(PrivateStart, Dict.PrivateSize);
  ReadCffDict(PrivateStart, Dict.PrivateSize, PrivateDict);
  if PrivateDict.SubrsOffset >= 0 then
  begin
    Result.HasSubrs := true;
    Result.Subrs := ReadCffIndex(PrivateStart + PrivateDict.SubrsOffset);
    Result.Bias := SubrBias(Result.Subrs.Count);
  end;
end;

procedure TOpenTypeFont.ReadCff(const Start: Integer);
var
  TopPos, TopLen, I, FDPos, FDLen: Integer;
  Header: TCffHeader;
  NameIndex, TopIndex, StringIndex, FDArray: TCffIndex;
  Top, FontDict: TCffDict;
begin
  FCffStart := Start;
  SeekTo(Start);
  FData.ReadBuffer(Header, SizeOf(Header));
  NameIndex := ReadCffIndex(Start + Header.HeaderSize);
  TopIndex := ReadCffIndex(NameIndex.EndPos);
  StringIndex := ReadCffIndex(TopIndex.EndPos);
  FGlobalSubrs := ReadCffIndex(StringIndex.EndPos);
  FGlobalBias := SubrBias(FGlobalSubrs.Count);

  if TopIndex.Count < 1 then
    Error('CFF font has no Top DICT');
  CffIndexItem(TopIndex, 0, TopPos, TopLen);
  ReadCffDict(TopPos, TopLen, Top);
  if Top.CharStringsOffset < 0 then
    Error('CFF font has no CharStrings');
  FCharStrings := ReadCffIndex(Start + Top.CharStringsOffset);

  { FontMatrix converts charstring units to em.
    Multiply by unitsPerEm (from "head") to get font units.
    Usually this is identity (FontMatrix is 1 / unitsPerEm). }
  FFontMatrixUsed := false;
  if Top.HasFontMatrix then
  begin
    FFontMatrixA := Top.FontMatrix[0] * FUnitsPerEm;
    FFontMatrixB := Top.FontMatrix[1] * FUnitsPerEm;
    FFontMatrixC := Top.FontMatrix[2] * FUnitsPerEm;
    FFontMatrixD := Top.FontMatrix[3] * FUnitsPerEm;
    FFontMatrixUsed :=
      (Abs(FFontMatrixA - 1) > 1e-6) or
      (Abs(FFontMatrixB) > 1e-6) or
      (Abs(FFontMatrixC) > 1e-6) or
      (Abs(FFontMatrixD - 1) > 1e-6);
  end;

  if Top.FDArrayOffset >= 0 then
  begin
    { CID-keyed font }
    FDArray := ReadCffIndex(Start + Top.FDArrayOffset);
    SetLength(FFDPrivates, FDArray.Count);
    for I := 0 to FDArray.Count - 1 do
    begin
      CffIndexItem(FDArray, I, FDPos, FDLen);
      ReadCffDict(FDPos, FDLen, FontDict);
      FFDPrivates[I] := ReadCffPrivate(FontDict);
    end;
    if Top.FDSelectOffset >= 0 then
      FFDSelect := Start + Top.FDSelectOffset;
  end else
    FCffPrivate := ReadCffPrivate(Top);
end;

function TOpenTypeFont.CffFDForGlyph(const Glyph: Integer): Integer;
var
  Fmt, NRanges, I, First, NextFirst, FD: Integer;
begin
  Result := 0;
  if FFDSelect = -1 then
    Exit;
  SeekTo(FFDSelect);
  Fmt := ReadUInt8;
  case Fmt of
    0:
      begin
        Skip(Glyph);
        Result := ReadUInt8;
      end;
    3:
      begin
        NRanges := ReadUInt16;
        { ranges (first glyph, FD), followed by the sentinel glyph }
        First := ReadUInt16;
        for I := 0 to NRanges - 1 do
        begin
          FD := ReadUInt8;
          NextFirst := ReadUInt16;
          if (Glyph >= First) and (Glyph < NextFirst) then
            Exit(FD);
          First := NextFirst;
        end;
      end;
    else
      Error(Format('Unsupported CFF FDSelect format %d', [Fmt]));
  end;
end;

procedure TOpenTypeFont.LoadCffGlyph(const Glyph: Integer; const Outline: TGlyphOutline);
var
  State: TType2State;
  Pos, Len, FD: Integer;
begin
  if (Glyph < 0) or (Glyph >= FCharStrings.Count) then
    Exit;

  FillChar(State, SizeOf(State), 0);
  State.Outline := Outline;
  if Length(FFDPrivates) <> 0 then
  begin
    FD := CffFDForGlyph(Glyph);
    if (FD < 0) or (FD >= Length(FFDPrivates)) then
      Error('Invalid CFF FD index');
    State.Subroutines := FFDPrivates[FD];
  end else
    State.Subroutines := FCffPrivate;

  CffIndexItem(FCharStrings, Glyph, Pos, Len);
  RunType2(State, Pos, Len);

  if FFontMatrixUsed then
    Outline.Transform(FFontMatrixA, FFontMatrixB, FFontMatrixC, FFontMatrixD);
end;

procedure TOpenTypeFont.RunType2(var State: TType2State; const Pos, Len: Integer);

  procedure Push(const V: Double);
  begin
    if State.StackCount >= Length(State.Stack) then
      Error('CFF charstring stack overflow');
    State.Stack[State.StackCount] := V;
    Inc(State.StackCount);
  end;

  procedure Need(const N: Integer);
  begin
    if State.StackCount < N then
      Error('CFF charstring stack underflow');
  end;

  { If Condition, the first stack value is the glyph width: remove it.
    Only the first stack-clearing operator may have the width. }
  procedure TakeWidth(const Condition: Boolean);
  var
    I: Integer;
  begin
    if not State.HaveWidth then
    begin
      State.HaveWidth := true;
      if Condition then
      begin
        for I := 1 to State.StackCount - 1 do
          State.Stack[I - 1] := State.Stack[I];
        Dec(State.StackCount);
      end;
    end;
  end;

  procedure MoveTo(const DX, DY: Double);
  begin
    State.Open := false;
    State.X := State.X + DX;
    State.Y := State.Y + DY;
    State.Outline.MoveTo(State.X, State.Y);
    State.Open := true;
  end;

  procedure EnsureOpen;
  begin
    if not State.Open then
    begin
      { drawing without moveto: start contour at current point }
      State.Outline.MoveTo(State.X, State.Y);
      State.Open := true;
    end;
  end;

  procedure LineTo(const DX, DY: Double);
  begin
    EnsureOpen;
    State.X := State.X + DX;
    State.Y := State.Y + DY;
    State.Outline.LineTo(State.X, State.Y);
  end;

  procedure CurveTo(const DX1, DY1, DX2, DY2, DX3, DY3: Double);
  var
    X1, Y1, X2, Y2: Double;
  begin
    EnsureOpen;
    X1 := State.X + DX1;
    Y1 := State.Y + DY1;
    X2 := X1 + DX2;
    Y2 := Y1 + DY2;
    State.X := X2 + DX3;
    State.Y := Y2 + DY3;
    State.Outline.CubicTo(X1, Y1, X2, Y2, State.X, State.Y);
  end;

  { Call subroutine from given INDEX, continue reading after it. }
  procedure CallSubroutine(const Subrs: TCffIndex; const Bias: Integer);
  var
    ReturnPos, N, SubrPos, SubrLen: Integer;
  begin
    Need(1);
    Dec(State.StackCount);
    N := Round(State.Stack[State.StackCount]) + Bias;
    ReturnPos := Position;
    CffIndexItem(Subrs, N, SubrPos, SubrLen);
    RunType2(State, SubrPos, SubrLen);
    SeekTo(ReturnPos);
  end;

var
  EndPos, B0, I: Integer;
  Horizontal: Boolean;
  D1, DF: Double;
begin
  Inc(State.Depth);
  if State.Depth > MaxSubrDepth then
    Error('CFF subroutines nesting too deep');
  SeekTo(Pos);
  EndPos := Pos + Len;
  while (Position < EndPos) and not State.Ended do
  begin
    B0 := ReadUInt8;

    { operands }
    if B0 = 28 then
    begin
      Push(ReadInt16);
      Continue;
    end;
    if (B0 >= 32) and (B0 <= 246) then
    begin
      Push(B0 - 139);
      Continue;
    end;
    if (B0 >= 247) and (B0 <= 250) then
    begin
      Push((B0 - 247) * 256 + ReadUInt8 + 108);
      Continue;
    end;
    if (B0 >= 251) and (B0 <= 254) then
    begin
      Push(-(B0 - 251) * 256 - ReadUInt8 - 108);
      Continue;
    end;
    if B0 = 255 then
    begin
      Push(ReadInt32 / 65536);
      Continue;
    end;

    { operators }
    case B0 of
      1, 3, 18, 23: // hstem, vstem, hstemhm, vstemhm
        begin
          TakeWidth(Odd(State.StackCount));
          State.NumStems := State.NumStems + State.StackCount div 2;
          State.StackCount := 0;
        end;
      19, 20: // hintmask, cntrmask
        begin
          TakeWidth(Odd(State.StackCount));
          { stems (vstem) may be given implicitly before hintmask }
          State.NumStems := State.NumStems + State.StackCount div 2;
          State.StackCount := 0;
          { skip the mask (we don't do hinting) }
          Skip((State.NumStems + 7) div 8);
        end;
      21: // rmoveto
        begin
          TakeWidth(State.StackCount > 2);
          Need(2);
          MoveTo(State.Stack[0], State.Stack[1]);
          State.StackCount := 0;
        end;
      22: // hmoveto
        begin
          TakeWidth(State.StackCount > 1);
          Need(1);
          MoveTo(State.Stack[0], 0);
          State.StackCount := 0;
        end;
      4: // vmoveto
        begin
          TakeWidth(State.StackCount > 1);
          Need(1);
          MoveTo(0, State.Stack[0]);
          State.StackCount := 0;
        end;
      5: // rlineto
        begin
          I := 0;
          while I + 1 < State.StackCount do
          begin
            LineTo(State.Stack[I], State.Stack[I + 1]);
            I := I + 2;
          end;
          State.StackCount := 0;
        end;
      6, 7: // hlineto, vlineto
        begin
          Horizontal := B0 = 6;
          for I := 0 to State.StackCount - 1 do
          begin
            if Horizontal then
              LineTo(State.Stack[I], 0)
            else
              LineTo(0, State.Stack[I]);
            Horizontal := not Horizontal;
          end;
          State.StackCount := 0;
        end;
      8: // rrcurveto
        begin
          I := 0;
          while I + 5 < State.StackCount do
          begin
            CurveTo(State.Stack[I], State.Stack[I + 1], State.Stack[I + 2],
              State.Stack[I + 3], State.Stack[I + 4], State.Stack[I + 5]);
            I := I + 6;
          end;
          State.StackCount := 0;
        end;
      24: // rcurveline
        begin
          I := 0;
          while I + 5 < State.StackCount - 2 do
          begin
            CurveTo(State.Stack[I], State.Stack[I + 1], State.Stack[I + 2],
              State.Stack[I + 3], State.Stack[I + 4], State.Stack[I + 5]);
            I := I + 6;
          end;
          if I + 1 < State.StackCount then
            LineTo(State.Stack[I], State.Stack[I + 1]);
          State.StackCount := 0;
        end;
      25: // rlinecurve
        begin
          I := 0;
          while I + 1 < State.StackCount - 6 do
          begin
            LineTo(State.Stack[I], State.Stack[I + 1]);
            I := I + 2;
          end;
          if I + 5 < State.StackCount then
            CurveTo(State.Stack[I], State.Stack[I + 1], State.Stack[I + 2],
              State.Stack[I + 3], State.Stack[I + 4], State.Stack[I + 5]);
          State.StackCount := 0;
        end;
      26: // vvcurveto
        begin
          I := 0;
          D1 := 0;
          if Odd(State.StackCount) then
          begin
            D1 := State.Stack[0];
            I := 1;
          end;
          while I + 3 < State.StackCount do
          begin
            CurveTo(D1, State.Stack[I], State.Stack[I + 1], State.Stack[I + 2], 0, State.Stack[I + 3]);
            D1 := 0;
            I := I + 4;
          end;
          State.StackCount := 0;
        end;
      27: // hhcurveto
        begin
          I := 0;
          D1 := 0;
          if Odd(State.StackCount) then
          begin
            D1 := State.Stack[0];
            I := 1;
          end;
          while I + 3 < State.StackCount do
          begin
            CurveTo(State.Stack[I], D1, State.Stack[I + 1], State.Stack[I + 2], State.Stack[I + 3], 0);
            D1 := 0;
            I := I + 4;
          end;
          State.StackCount := 0;
        end;
      30, 31: // vhcurveto, hvcurveto
        begin
          Horizontal := B0 = 31;
          I := 0;
          while I + 3 < State.StackCount do
          begin
            if State.StackCount - I = 5 then
              DF := State.Stack[I + 4]
            else
              DF := 0;
            if Horizontal then
              CurveTo(State.Stack[I], 0, State.Stack[I + 1], State.Stack[I + 2], DF, State.Stack[I + 3])
            else
              CurveTo(0, State.Stack[I], State.Stack[I + 1], State.Stack[I + 2], State.Stack[I + 3], DF);
            Horizontal := not Horizontal;
            I := I + 4;
          end;
          State.StackCount := 0;
        end;
      10: // callsubr
        begin
          if not State.Subroutines.HasSubrs then
            Error('CFF charstring calls local subroutine, but font has no Subrs');
          CallSubroutine(State.Subroutines.Subrs, State.Subroutines.Bias);
        end;
      29: // callgsubr
        CallSubroutine(FGlobalSubrs, FGlobalBias);
      11: // return
        Break;
      14: // endchar
        begin
          TakeWidth((State.StackCount = 1) or (State.StackCount = 5));
          { Note: endchar with 4 arguments (accented character, like Type 1 seac)
            is not supported, it's ignored. }
          State.Open := false;
          State.Ended := true;
          State.StackCount := 0;
        end;
      12:
        Type2Escape(State, ReadUInt8);
      else
        { reserved operators }
        State.StackCount := 0;
    end;
  end;

  Dec(State.Depth);
end;

procedure TOpenTypeFont.Type2Escape(var State: TType2State; const Op: Integer);

  procedure Need(const N: Integer);
  begin
    if State.StackCount < N then
      Error('CFF charstring stack underflow');
  end;

  function Pop: Double;
  begin
    Need(1);
    Dec(State.StackCount);
    Result := State.Stack[State.StackCount];
  end;

  procedure Push(const V: Double);
  begin
    if State.StackCount >= Length(State.Stack) then
      Error('CFF charstring stack overflow');
    State.Stack[State.StackCount] := V;
    Inc(State.StackCount);
  end;

  procedure CurveTo(const DX1, DY1, DX2, DY2, DX3, DY3: Double);
  var
    X1, Y1, X2, Y2: Double;
  begin
    if not State.Open then
    begin
      State.Outline.MoveTo(State.X, State.Y);
      State.Open := true;
    end;
    X1 := State.X + DX1;
    Y1 := State.Y + DY1;
    X2 := X1 + DX2;
    Y2 := Y1 + DY2;
    State.X := X2 + DX3;
    State.Y := Y2 + DY3;
    State.Outline.CubicTo(X1, Y1, X2, Y2, State.X, State.Y);
  end;

  function BoolValue(const B: Boolean): Double;
  begin
    if B then
      Result := 1
    else
      Result := 0;
  end;

var
  S: array [0..10] of Double;
  I, J, N: Integer;
  A, B, DX, DY, DX6, DY6: Double;
  Rolled: array [0..47] of Double;
begin
  case Op of
    35: // flex
      begin
        Need(13);
        for I := 0 to 10 do
          S[I] := State.Stack[I];
        CurveTo(S[0], S[1], S[2], S[3], S[4], S[5]);
        CurveTo(S[6], S[7], S[8], S[9], S[10], State.Stack[11]);
        State.StackCount := 0;
      end;
    34: // hflex
      begin
        Need(7);
        for I := 0 to 6 do
          S[I] := State.Stack[I];
        // dx1 dx2 dy2 dx3 dx4 dx5 dx6
        CurveTo(S[0], 0, S[1], S[2], S[3], 0);
        CurveTo(S[4], 0, S[5], -S[2], S[6], 0);
        State.StackCount := 0;
      end;
    36: // hflex1
      begin
        Need(9);
        for I := 0 to 8 do
          S[I] := State.Stack[I];
        // dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6
        CurveTo(S[0], S[1], S[2], S[3], S[4], 0);
        CurveTo(S[5], 0, S[6], S[7], S[8], -(S[1] + S[3] + S[7]));
        State.StackCount := 0;
      end;
    37: // flex1
      begin
        Need(11);
        for I := 0 to 10 do
          S[I] := State.Stack[I];
        DX := S[0] + S[2] + S[4] + S[6] + S[8];
        DY := S[1] + S[3] + S[5] + S[7] + S[9];
        if Abs(DX) > Abs(DY) then
        begin
          DX6 := S[10];
          DY6 := -DY;
        end else
        begin
          DX6 := -DX;
          DY6 := S[10];
        end;
        CurveTo(S[0], S[1], S[2], S[3], S[4], S[5]);
        CurveTo(S[6], S[7], S[8], S[9], DX6, DY6);
        State.StackCount := 0;
      end;
    0: // dotsection (deprecated)
      State.StackCount := 0;
    3: // and
      begin
        B := Pop;
        A := Pop;
        Push(BoolValue((A <> 0) and (B <> 0)));
      end;
    4: // or
      begin
        B := Pop;
        A := Pop;
        Push(BoolValue((A <> 0) or (B <> 0)));
      end;
    5: // not
      Push(BoolValue(Pop = 0));
    9: // abs
      Push(Abs(Pop));
    10: // add
      begin
        B := Pop;
        A := Pop;
        Push(A + B);
      end;
    11: // sub
      begin
        B := Pop;
        A := Pop;
        Push(A - B);
      end;
    12: // div
      begin
        B := Pop;
        A := Pop;
        if B <> 0 then
          Push(A / B)
        else
          Push(0);
      end;
    14: // neg
      Push(-Pop);
    15: // eq
      begin
        B := Pop;
        A := Pop;
        Push(BoolValue(A = B));
      end;
    18: // drop
      Pop;
    20: // put
      begin
        I := Round(Pop);
        A := Pop;
        if (I >= 0) and (I < Length(State.Transient)) then
          State.Transient[I] := A;
      end;
    21: // get
      begin
        I := Round(Pop);
        if (I >= 0) and (I < Length(State.Transient)) then
          Push(State.Transient[I])
        else
          Push(0);
      end;
    22: // ifelse
      begin
        Need(4);
        B := Pop; // v2
        A := Pop; // v1
        DY := Pop; // s2
        DX := Pop; // s1
        if A <= B then
          Push(DX)
        else
          Push(DY);
      end;
    23: // random
      Push(0.5);
    24: // mul
      begin
        B := Pop;
        A := Pop;
        Push(A * B);
      end;
    26: // sqrt
      begin
        A := Pop;
        if A > 0 then
          Push(Sqrt(A))
        else
          Push(0);
      end;
    27: // dup
      begin
        Need(1);
        Push(State.Stack[State.StackCount - 1]);
      end;
    28: // exch
      begin
        Need(2);
        A := State.Stack[State.StackCount - 1];
        State.Stack[State.StackCount - 1] := State.Stack[State.StackCount - 2];
        State.Stack[State.StackCount - 2] := A;
      end;
    29: // index
      begin
        I := Round(Pop);
        if I < 0 then
          I := 0;
        Need(I + 1);
        Push(State.Stack[State.StackCount - 1 - I]);
      end;
    30: // roll
      begin
        J := Round(Pop);
        N := Round(Pop);
        if N > 0 then
        begin
          Need(N);
          J := J mod N;
          if J < 0 then
            J := J + N;
          for I := 0 to N - 1 do
            Rolled[(I + J) mod N] := State.Stack[State.StackCount - N + I];
          for I := 0 to N - 1 do
            State.Stack[State.StackCount - N + I] := Rolled[I];
        end;
      end;
    else
      State.StackCount := 0;
  end;
end;

end.
