{
  This unit originated from font management unit in FPC run time library.
  It was remade a lot for Castle Game Engine, and it's not really
  "font management" anymore, it's more a "thinnest possible OOP wrapper
  around FreeType API" now.

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

{ Management of FreeType fonts, serving as an OOP layer between FreeType
  library API (in CastleInternalFreeTypeH) and engine code loading font
  into image (in CastleTextureFontData).

  This is based on a similar FPC unit, but with lots of adjustments
  to what we need in our engine:

  - We don't need platform-specific searching for fonts,
    platform-specific resolutions,
    caching for fonts and sizes (if anything, we would cache TTextureFontData),
    rendering lines of text.

  - We only want to open font file from URL (using engine Download),
    and render 1 individual character at a time.
    (kerning information should be passed upward in API at some point).
}
unit CastleInternalFreeType;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleInternalFreeTypeH, CastleUnicode, CastleUtils;

type
  EFreeTypeException = class (Exception);

  { Raised when the freetype library is not available. }
  EFreeTypeLibraryNotFound = class (EFreeTypeException);

  { Single character rendered by FreeType. }
  TFontBitmap = class
    Height, Width, Pitch, X, Y, AdvanceX, AdvanceY: Integer;

    { Data from FreeType bitmap.
      Allocated with GetMem.
      Access this as an array like Data[xxx],
      relying on Delphi "pointermath on" in our castleconf.inc
      (default in FPC). }
    Data: PByte;

    { Does FreeMem on Data. }
    destructor Destroy; override;
  end;

  { Single font file loaded using FreeType. }
  TFreeTypeFont = class
  private
    Font : PFT_Face;
    FUrl: String;
    MemoryStream: TCustomMemoryStream;
  public
    constructor Create(const AUrl: String; const IndexInFontFile: Integer = 0);
    destructor Destroy; override;

    property Url: String read FUrl;

    { FreeType face, use to read font information like family_name. }
    property FreeTypeInfo: PFT_Face read Font;

    { Render single Unicode character into TFontBitmap.
      Caller must free the returned TFontBitmap instance. }
    function RenderChar(const UnicodeChar: TUnicodeChar;
      const Size: Integer; const AntiAliased: Boolean): TFontBitmap;
  end;

implementation

uses CastleDownload, CastleLog;

const
  sErrSetPixelSize = 'setting pixel size %d';
  sErrSetCharSize = 'setting char size %d';
  sErrLoadGlyph = 'loading glyph %d (FT_Load_Glyph)';
  sErrGetGlyph = 'getting glyph %d (FT_Get_Glyph)';
  sErrLoadFont = 'loading font %d from file %s';
  sErrInitializing = 'initializing FreeType';
  sErrDestroying = 'finalizing FreeType';
  sErrGlyphToBitmap = 'doing Glyph_To_Bitmap';

{ error handling ------------------------------------------------------------- }

{ Raise EFreeTypeException if ErrorCode <> 0,
  showing ErrorAtEvent as the context of the error,
  and ErrorCode as the FreeType error code. }
procedure FTCheck(const ErrorCode: Integer; const ErrorAtEvent: String);
const
  sErrFreeType = 'Error %d while %s';
begin
  if ErrorCode <> 0 then
    raise EFreeTypeException.CreateFmt(sErrFreeType, [ErrorCode, ErrorAtEvent]);
end;

{ PFT_Library initialization / finalization ---------------------------------- }

var
  FtLibrary: PFT_Library;

{ Initialize FreeType library, making sure library is loaded
  and calling FT_Init_FreeType.
  Can be called multiple times safely.
  @raises EFreeTypeLibraryNotFound If the library cannot be loaded. }
procedure InitFreeTypeLibrary;
var
  Major, Minor, Patch: Integer;
  VersionStr: String;
begin
  if FtLibrary <> nil then
    Exit; // already initialized

  if not FreeTypeLibraryInitialized then
    raise EFreeTypeLibraryNotFound.Create('Cannot load FreeType library, loading font files not possible');

  try
    FTCheck(FT_Init_FreeType(FtLibrary), sErrInitializing);
  except
    FtLibrary := nil; // make sure we don't leave it in an inconsistent state
    raise;
  end;

  FT_Library_Version(FtLibrary, Major, Minor, Patch);
  if Patch = FtPatchVersionPascalImplementation then
    VersionStr := Format('%d.%d, Pascal implementation', [Major, Minor])
  else
    VersionStr := Format('%d.%d.%d', [Major, Minor, Patch]);
  WritelnLog('FreeType (font reading) library initialized (version %s).', [VersionStr]);
end;

procedure DoneFreeTypeLibrary;
begin
  { Do nothing if InitFreeTypeLibrary was never called successfully.
    In particular, do not call FT_Done_FreeType when FreeType library
    was not found (FT_Done_FreeType would be nil then). }
  if FtLibrary = nil then
    Exit;

  FTCheck(FT_Done_FreeType(FtLibrary), sErrDestroying);

  FtLibrary := nil;
end;

{ TFontBitmap ---------------------------------------------------------------- }

destructor TFontBitmap.Destroy;
begin
  FreeMemNiling(Pointer(Data));
  inherited;
end;

{ TFreeTypeFont -------------------------------------------------------------- }

constructor TFreeTypeFont.Create(const AUrl: String; const IndexInFontFile: Integer);
begin
  inherited Create;
  FUrl := AUrl;

  InitFreeTypeLibrary;

  try
    { We never open the font by filename (FreeType's FT_New_Face,
      not even defined in our current minimal FreeType header in CastleInternalFreeTypeH).

      Instead we always use CGE Download() to open the URL
      and pass the memory chunk to FreeType using FT_New_Memory_Face.

      Reasons: opening by filename failed on Windows for filenames with
      non-ASCII characters (as it is unclear in what encoding FreeType
      expects the filename), and it required a temporary file to access
      non-file URLs. }
    MemoryStream := Download(Url, [soForceMemoryStream]) as TCustomMemoryStream;

    FTCheck(FT_New_Memory_Face(FtLibrary,
      MemoryStream.Memory, MemoryStream.Size, IndexInFontFile, font),
      Format(sErrLoadFont, [IndexInFontFile, AUrl]));
  except
    Font := nil;
    raise;
  end;
end;

destructor TFreeTypeFont.Destroy;
begin
  try
    { Free the face explicitly.

      With the FreeType library, this is not strictly necessary,
      as FT_Done_FreeType would also free all remaining faces,
      but that's not the case with our CastleInternalFreeTypeHPascal
      (and also we could waste memory by keeping unused face loaded).

      Also, MemoryStream must remain valid until FT_Done_Face. }
    if Font <> nil then
    begin
      FT_Done_Face(Font);
      Font := nil;
    end;
  finally
    { We free MemoryStream only in our destructor (not right after
      FT_New_Memory_Face in constructor).
      Otherwise weirdest errors can occur when reading the existing font 2nd time
      (autotests see weird Height),
      this is also said in docs
      https://freetype.org/freetype2/docs/reference/ft2-base_interface.html#ft_new_memory_face :
      "You must not deallocate the memory before calling FT_Done_Face."
    }
    FreeAndNil(MemoryStream);
    inherited Destroy;
  end;
end;

function TFreeTypeFont.RenderChar(const UnicodeChar: TUnicodeChar;
  const Size: Integer; const AntiAliased: Boolean): TFontBitmap;

  procedure SetPixelSize (aSize : integer);

    procedure CheckSize(aSize : integer);
    var r : integer;
    begin
      r := Font^.Num_fixed_sizes;
      repeat
        dec (r);
      until (r < 0) or
        ( (Font^.available_sizes^[r].height=asize) and
          (Font^.available_sizes^[r].width=asize) );
      if r < 0 then
        raise EFreeTypeException.CreateFmt ('Size %d not available for %s %s', [
          aSize,
          Font^.style_name,
          Font^.family_name
        ]);
    end;

  var
    s : Integer;
  const
    { Use 0, letting FreeType library use good default,
      http://www.freetype.org/freetype2/docs/tutorial/step1.html ,
      and in effect Size is in nice pixels by default. }
    Resolution = 0;
  begin
    if (Font^.face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then
    begin
      CheckSize(aSize);
      FTCheck(FT_Set_pixel_sizes (Font, aSize, aSize), Format(sErrSetPixelSize, [aSize]));
    end else
    begin
      s := aSize shl 6;
      FTCheck(FT_Set_char_size (Font, s, s, Resolution, Resolution), Format(sErrSetCharSize, [aSize]));
    end;
  end;

  { Load glyph for given character.
    Caller must free the result using FT_Done_Glyph. }
  function CreateGlyph(const C: TUnicodeChar): PFT_Glyph;
  var
    GlyphIndex: FT_UInt;
  begin
    GlyphIndex := FT_Get_Char_Index(Font, C);
    { FT_Load_Glyph sets "glyph slot" in Font^.glyph.
      See https://freetype.org/freetype2/docs/reference/ft2-glyph_retrieval.html#ft_load_glyph }
    FTCheck(FT_Load_Glyph(Font, GlyphIndex, FT_Load_Default), Format(sErrLoadGlyph, [C]));
    FTCheck(FT_Get_Glyph(Font^.glyph, Result), Format(sErrGetGlyph, [C]));
  end;

var
  Glyph: PFT_Glyph;
  BitmapGlyph: PFT_BitmapGlyph;
  Row: Integer;
  Buf: PByte;
  RenderMode: FT_Render_Mode;
begin
  SetPixelSize(Size);

  Glyph := CreateGlyph(UnicodeChar);
  try
    if AntiAliased then
      RenderMode := FT_RENDER_MODE_NORMAL
    else
      RenderMode := FT_RENDER_MODE_MONO;
    { On success, this frees the original Glyph (since destroy = true)
      and sets Glyph to the new bitmap glyph.
      On failure, Glyph remains unchanged (and we free it in "finally" clause). }
    FTCheck(FT_Glyph_To_Bitmap(Glyph, RenderMode, nil, true), sErrGlyphToBitmap);
    BitmapGlyph := PFT_BitmapGlyph(Glyph);

    Result := TFontBitmap.Create;
    try
      Result.AdvanceX := Glyph^.advance.x shr 6;
      Result.AdvanceY := Glyph^.advance.y shr 6;
      Result.Height := BitmapGlyph^.bitmap.rows;
      Result.Width := BitmapGlyph^.bitmap.width;
      Result.X := BitmapGlyph^.left;
      Result.Y := -BitmapGlyph^.top;
      Buf := BitmapGlyph^.bitmap.buffer;
      if BitmapGlyph^.bitmap.pitch < 0 then
      begin
        Result.Pitch := -BitmapGlyph^.bitmap.pitch;
        Result.Data := GetMem(Result.Pitch * Result.Height);
        for Row := Result.Height - 1 downto 0 do
          Move(Buf[Row * Result.Pitch],
            Result.Data[(Result.Height - Row - 1) * Result.Pitch], Result.Pitch);
      end else
      begin
        Result.Pitch := BitmapGlyph^.bitmap.pitch;
        Result.Data := GetMem(Result.Pitch * Result.Height);
        Move(Buf[0], Result.Data[0], Result.Pitch * Result.Height);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    FT_Done_Glyph(Glyph);
  end;
end;

initialization
finalization
  DoneFreeTypeLibrary;
end.
