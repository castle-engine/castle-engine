{
  This file is based on the Free Pascal run time library,
  remade a lot for Castle Game Engine.

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

{ Manager for FreeType fonts, serving as a middle-man between FreeType library API
  (in CastleInternalFreeTypeH) and CGE code loading font into image
  (in CastleTextureFontData).

  This is based on a similar FPC concept, but

  - We simplified this unit a lot to better suit our purpose.
    We don't need platform-specific searching for fonts,
    or platform-specific resolutions etc.
    We only want to open font files from URLs.

  - Loading fonts using from any URL supported by CGE,
    thanks to using Download and FT_New_Memory_Face underneath.

  - raise exception from TFontManager.Create when FreeType library not found

  - Use UTF-8 encoding for string and Cardinal for character type,
    following David Emerson patch from
    http://free-pascal-general.1045716.n5.nabble.com/freetype-unit-unicode-td4866273.html
    adjusted to use our CastleUnicode unit.
}
unit CastleInternalFreeType;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Types,
  CastleInternalFreeTypeH, CastleUnicode, CastleUtils;

// determine if file comparison need to be case sensitive or not
{$ifdef WIN32}
  {$undef CaseSense}
{$else}
  {$define CaseSense}
{$endif}

type
  FreeTypeException = class (exception);

  { Raised by TFontManager.Create when the freetype library is not available. }
  EFreeTypeLibraryNotFound = class (FreeTypeException);

  TBitmapType = (btBlackWhite, bt256Gray);
  TFontBitmap = record
    height, width, pitch,
    x,y, advanceX, advanceY : integer;
    { for some reason, default FPC TByteArray has limited length (0..32767).
      We sometimes need longer (if you try to generate some huge font size, like 300). }
    data : CastleUtils.PByteArray;
  end;
  PFontBitmap = ^TFontBitmap;

  TStringBitMaps = class
    private
      FList : TList;
      FBounds : TRect;
      FText : string;
      FMode : TBitmapType;
      function GetCount : integer;
      function GetBitmap (index:integer) : PFontBitmap;
      procedure CalculateGlobals;
    public
      constructor Create (ACount : integer);
      destructor destroy; override;
      procedure GetBoundRect (out aRect : TRect);
      property Text : string read FText;
      property Mode : TBitmapType read FMode;
      property Count : integer read GetCount;
      property Bitmaps[index:integer] : PFontBitmap read GetBitmap;
  end;

  TFontManager = class;

  PMgrGlyph = ^TMgrGlyph;
  TMgrGlyph = record
    Character : TUnicodeChar;
    GlyphIndex : FT_UInt;
    Glyph : PFT_Glyph;
  end;

  PMgrSize = ^TMgrSize;
  TMgrSize = record
    Size : integer;
    Glyphs : TList;
  end;

  TMgrFont = class
    private
      Mgr : TFontManager;
      Font : PFT_Face;
      FSizes : TList;
      Filename : string;
      LastSize : PMgrSize;
      MemoryStream: TCustomMemoryStream;
      procedure FreeGlyphs;
    public
      constructor Create (aMgr:TFontManager; afilename:string; anindex:integer);
      destructor destroy; override;
  end;

  TFontManager = class
    private
      FTLib : PFT_Library;
      FList : TList;
      FPaths : TStringList;
      CurFont : TMgrFont;
      CurSize : PMgrSize;
      CurRenderMode : FT_Render_Mode;
      UseKerning : boolean;
    protected
      function GetFontId (afilename:string; anindex:integer) : integer;
      function CreateFont (afilename:string; anindex:integer) : integer;
      //function SearchFont (afilename:string) : string;
      function GetFont (FontID:integer) : TMgrFont;
      procedure GetSize (aSize : integer);
      function CreateSize (aSize : integer) : PMgrSize;
      procedure SetPixelSize (aSize : integer);
      function GetGlyph (c : TUnicodeChar) : PMgrGlyph;
      function CreateGlyph (c : TUnicodeChar) : PMgrGlyph;
      procedure InitMakeString (FontID, Size:integer);
      function MakeString (FontId:integer; Text:string; Size:integer) : TStringBitmaps; overload;
    public
      { @raises EFreeTypeLibraryNotFound }
      constructor Create;
      destructor destroy; override;
      function RequestFont (afilename:string) : integer; overload;
      function RequestFont (afilename:string; anindex:integer) : integer; overload;
      function GetFreeTypeFont (aFontID:integer) : PFT_Face;
      function GetString (FontId:integer; Text:string; Size:integer) : TStringBitmaps; overload;
      // Black and white, following the direction of the font (left to right, top to bottom, ...)
      function GetStringGray (FontId:integer; Text:string; Size:integer) : TStringBitmaps; overload;
      // Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
  end;

const
  sErrErrorsInCleanup : string = '%d errors detected while freeing a Font Manager object';
  sErrFontFileNotFound : string = 'Font file "%s" not found';
  sErrFreeType : string = 'Error %d while %s';
  sInitializing : string = 'initializing font engine';
  sDestroying : string = 'destroying font engine';
  sErrErrorInCleanup : string = 'freeing Font Manager object';
  sErrSetPixelSize : string = 'setting pixel size %d';
  sErrSetCharSize : string = 'setting char size %d';
  sErrLoadingGlyph : string = 'loading glyph';
  sErrKerning : string = 'determining kerning distance';
  sErrMakingString1 : string = 'making string bitmaps step 1';
  sErrMakingString2 : string = 'making string bitmaps step 2';
  sErrMakingString3 : string = 'making string bitmaps step 3';
  sErrMakingString4 : string = 'making string bitmaps step 4';
  sErrLoadFont : string = 'loading font %d from file %s';
  sErrInitializing : string = 'initializing FreeType';
  sErrDestroying : string = 'finalizing FreeType';

var
  FontMgr : TFontManager;

{ @raises EFreeTypeLibraryNotFound }
procedure InitFontMgr;
procedure DoneFontMgr;

implementation

uses {$ifdef FPC}{$IFDEF win32}dos, {$ENDIF}{$endif}
  CastleDownload;

procedure FTError (Event:string; Err:integer);
begin
  raise FreeTypeException.CreateFmt (sErrFreeType, [Err,Event]);
end;

Function FTCheck (Res: Integer; Msg:string) : Integer;

begin
  Result:=Res;
  If (Result<>0) then
    FTError(Msg,Result);
end;

{procedure WriteFT_Face(CurFont: PFT_Face);
var
  i: Integer;
begin
  writeln(' num_faces=',CurFont^.num_faces);
  writeln(' face_index=',CurFont^.face_index);
  writeln(' face_flags=',CurFont^.face_flags);
  writeln(' style_flags=',CurFont^.style_flags);
  writeln(' num_glyphs=',CurFont^.num_glyphs);
  writeln(' family_name=',CurFont^.family_name<>nil);
  writeln(' style_name=',CurFont^.style_name<>nil);
  // if CurFont^.style_name<>nil then begin
  //   writeln('   ',CurFont^.style_name^);
  // end;
  writeln(' num_fixed_sizes=',CurFont^.num_fixed_sizes);
  writeln(' available_sizes=',CurFont^.available_sizes<>nil);
  for i:=1 to CurFont^.num_fixed_sizes do begin
    writeln('   ',i,' ',CurFont^.available_sizes^[i-1].width,'x',CurFont^.available_sizes^[i-1].height);
  end;
  writeln(' num_charmaps=',CurFont^.num_charmaps);
  writeln(' charmaps=',CurFont^.charmaps<>nil);
  writeln(' generic.data=',CurFont^.generic.data<>nil);
  writeln(' generic.finalizer=',CurFont^.generic.finalizer<>nil);
  writeln(' bbox.xMin=',CurFont^.bbox.xMin,
    ' bbox.xMax=',CurFont^.bbox.xMax,
    ' bbox.yMin=',CurFont^.bbox.yMin,
    ' bbox.yMax=',CurFont^.bbox.yMax,
    ' units_per_EM=',CurFont^.units_per_EM,
    ' ascender=',CurFont^.ascender,
    ' descender=',CurFont^.descender,
    ' height=',CurFont^.height,
    ' max_advance_width=',CurFont^.max_advance_width,
    ' max_advance_height=',CurFont^.max_advance_height,
    ' underline_position=',CurFont^.underline_position,
    ' underline_thickness=',CurFont^.underline_thickness,
    ' glyph=',CurFont^.glyph<>nil,
    ' size=',CurFont^.size<>nil,
    ' charmap=',CurFont^.charmap<>nil,
    '');
end;}

{ TMgrFont }

constructor TMgrFont.Create (aMgr:TFontManager; afilename:string; anindex:integer);
begin
  inherited create;
  Filename := afilename;
  Mgr := aMgr;
  FSizes := TList.create;
  LastSize := nil;

  Try
    { Old code:
    FTCheck(FT_New_Face (aMgr.FTLib, PAnsiChar(AnsiString(afilename)), anindex, font),format (sErrLoadFont,[anindex,afilename]));

      This mostly worked OK, except on Windows where it fails reading
      a filename with non-ASCII characters.
      Also it required a temporary file when accessing non-file URLs.
      So now we make it simpler: just use CGE Download() to open the URL
      (afilename can be any URL), and pass memory chunk to FreeType using
      FT_New_Memory_Face.
    }

    MemoryStream := Download(afilename, [soForceMemoryStream]) as TCustomMemoryStream;
    FTCheck(FT_New_Memory_Face(aMgr.FTLib, MemoryStream.Memory, MemoryStream.Size, anindex, font),
      format (sErrLoadFont,[anindex,afilename]));

    { We will free MemoryStream only in our destructor.
      Otherwise weirdest errors can occur when reading the existing font 2nd time
      (autotests see weird Height),
      this is also said in docs
      https://freetype.org/freetype2/docs/reference/ft2-base_interface.html#ft_new_memory_face :
      "You must not deallocate the memory before calling FT_Done_Face."
    }

    //WriteFT_Face(font);
  except
    Font:=Nil;
    Raise;
  end;
end;

destructor TMgrFont.destroy;
begin
  try
    FreeGlyphs;
  finally
    FreeAndNil(MemoryStream);
    FSizes.Free;
    inherited Destroy;
  end;
end;

procedure TMgrFont.FreeGlyphs;
var r,t : integer;
    S : PMgrSize;
    G : PMgrGlyph;
begin
  for r := FSizes.count-1 downto 0 do
    begin
    with PMgrSize(FSizes[r])^ do
      begin
      for t := Glyphs.count-1 downto 0 do
        begin
        with PMgrGlyph(Glyphs[t])^ do
          FT_Done_Glyph (Glyph);
        G := PMgrGlyph(Glyphs[t]);
        dispose (G);
        end;
      Glyphs.Free;
      end;
    S := PMgrSize(FSizes[r]);
    dispose (S);
    end;
end;


{ TFontManager }

constructor TFontManager.Create;
var r : integer;
begin
  inherited create;
  FList := Tlist.Create;
  FPaths := TStringList.Create;

  if not FreeTypeLibraryInitialized then
    raise EFreeTypeLibraryNotFound.Create('Cannot load FreeType library, loading font files not possible');

  r := FT_Init_FreeType(FTLib);
  if r <> 0  then
    begin
    FTLib := nil;
    FTError (sErrInitializing, r);
    end;
end;

destructor TFontManager.Destroy;
  procedure FreeFontObjects;
  var r : integer;
  begin
    for r := FList.Count-1 downto 0 do
      begin
      GetFont(r).Free;
      end;
  end;
  procedure FreeLibrary;
  var r : integer;
  begin
    r := FT_Done_FreeType (FTlib);
    if r <> 0 then
      FTError (sErrDestroying, r);
  end;
begin
  FreeFontObjects;
  FList.Free;
  FPaths.Free;
  try
    if assigned(FTLib) then
      FreeLibrary;
  finally
    inherited Destroy;
  end;
end;

function TFontManager.GetFontId (afilename:string; anindex:integer) : integer;
begin
  result := FList.count-1;
  while (result >= 0) and
        ( ({$ifdef CaseSense}CompareText{$else}CompareStr{$endif}
              (TMgrFont(FList[Result]).Filename, afilename) <> 0) or
          (anIndex <> TMgrFont(FList[Result]).font^.face_index)
        ) do
    dec (result);
end;

function TFontManager.CreateFont (afilename:string; anindex:integer) : integer;
var f : TMgrFont;
begin
//  writeln ('creating font ',afilename,' (',anindex,')');
  f := TMgrFont.Create (self, afilename, anindex);
  result := FList.Count;
  Flist.Add (f);
end;

function TFontManager.GetFont (FontID:integer) : TMgrFont;
begin
  if (FontID >= 0) and (FontID < FList.Count) then
  begin
    result := TMgrFont(FList[FontID]);

    if result <> CurFont then  // set last used size of the font as current size
    begin
      CurSize := result.LastSize;
    end;
  end
  else
    Result := nil;
end;

procedure TFontManager.GetSize (aSize : integer);
var r : integer;
begin
  if not ( assigned(CurSize) and
          (CurSize^.Size = aSize) ) then
    begin
    r := CurFont.FSizes.count;
    repeat
      dec (r)
    until (r < 0) or ( (PMgrSize(CurFont.FSizes[r])^.size = aSize) );
    if r < 0 then
      CurSize := CreateSize (aSize)
    else
      CurSize := PMgrSize(CurFont.FSizes[r]);
    CurFont.LastSize := CurSize;
    end;
end;

function TFontManager.CreateSize (aSize : integer) : PMgrSize;
begin
  new (result);
  result^.Size := aSize;
  result^.Glyphs := Tlist.Create;
  SetPixelSize (aSize);
  CurFont.FSizes.Add (result);
end;

procedure TFontManager.SetPixelSize (aSize : integer);

  procedure CheckSize;
  var r : integer;
  begin
    with Curfont.Font^ do
      begin
      r := Num_fixed_sizes;
      repeat
        dec (r);
      until (r < 0) or
         ( (available_sizes^[r].height=asize) and
           (available_sizes^[r].width=asize) );
      if r >= 0 then
        raise FreeTypeException.CreateFmt ('Size %d not available for %s %s',
                  [aSize, style_name, family_name]);
      end;
  end;

var s : longint;
    Err : integer;

const
  { Use 0, letting FreeType library use good default,
    http://www.freetype.org/freetype2/docs/tutorial/step1.html ,
    and in effect Size is in nice pixels by default. }
  Resolution = 0;
begin
  with Curfont, Font^ do
    if (face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then
      begin
      CheckSize;
      Err := FT_Set_pixel_sizes (Font, aSize, aSize);
      if Err <> 0 then
        FTError (format(sErrSetPixelSize,[aSize]), Err);
      end
    else
      begin
      s := aSize shl 6;
      Err := FT_Set_char_size (Font, s, s, Resolution, Resolution);
      if Err <> 0 then
        FTError (format(sErrSetCharSize,[aSize]), Err);
      end;
end;

function TFontManager.CreateGlyph (c : TUnicodeChar) : PMgrGlyph;
var e : integer;
begin
  new (result);
  FillByte(Result^,SizeOf(Result),0);
  result^.character := c;
  result^.GlyphIndex := FT_Get_Char_Index (CurFont.font, c);
  //WriteFT_Face(CurFont.Font);
  e := FT_Load_Glyph (CurFont.font, result^.GlyphIndex, FT_Load_Default);
  if e <> 0 then
    begin
    FTError (sErrLoadingGlyph, e);
    end;
  e := FT_Get_Glyph (Curfont.font^.glyph, result^.glyph);
  if e <> 0 then
    begin
    FTError (sErrLoadingGlyph, e);
    end;
  CurSize^.Glyphs.Add (result);
end;

function TFontManager.GetGlyph (c : TUnicodeChar) : PMgrGlyph;
var r : integer;
begin
  With CurSize^ do
    begin
    r := Glyphs.Count;
    repeat
      dec (r)
    until (r < 0) or (PMgrGlyph(Glyphs[r])^.character = c);
    if r < 0 then
      result := CreateGlyph (c)
    else
      result := PMgrGlyph(Glyphs[r]);
    end;
end;

procedure TFontManager.InitMakeString (FontID, Size:integer);
begin
  GetSize (size);
  UseKerning := ((Curfont.font^.face_flags and FT_FACE_FLAG_KERNING) <> 0);
end;

function TFontManager.MakeString(FontId: Integer; Text: String; Size: Integer): TStringBitmaps;
var g : PMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    e, prevIndex, prevx, r, rx : integer;
    {$ifdef FPC}
    cl : integer;
    pc : pchar;
    {$else}
    TextIndex: Integer;
    NextIndex: Integer;
    TextLength: Integer;
    {$endif}
    uc : TUnicodeChar;
    pos, kern : FT_Vector;
    buf : CastleUtils.PByteArray;
    reverse : boolean;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  result := TStringBitmaps.Create({$ifdef FPC}UTF8Length(Text){$else}GetUTF32Length(Text){$endif});
  if (CurRenderMode = FT_RENDER_MODE_MONO) then
    result.FMode := btBlackWhite
  else
    result.FMode := bt256Gray;
  prevIndex := 0;
  prevx := 0;
  pos.x := 0;
  pos.y := 0;
  r := -1;
  // get the unicode for the character. Also performed at the end of the while loop.
  {$ifdef FPC}
  pc := pchar(text);
  uc := UTF8CharacterToUnicode (pc, cl);
  while (cl>0) and (uc>0) do
  {$else}
  TextIndex := 1;
  TextLength := Length(Text);
  while (TextIndex <= TextLength) do
  {$endif}
  begin
    {$ifdef FPC}
      // increment pchar by character length
      inc (pc, cl);
    {$else}
      uc := GetUTF32Char(Text, TextIndex, NextIndex);
      TextIndex := NextIndex;
    {$endif}
    // retrieve loaded glyph
    g := GetGlyph (uc);
    inc (r);
    // check kerning
    if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
      begin
      prevx := pos.x;
      e := FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern);
      if e <> 0 then
        FTError (sErrKerning, e);
      pos.x := pos.x + kern.x;
      end;
    // render the glyph
    FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
    FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, @pos, true),sErrMakingString4);
    // Copy what is needed to record
    bm := PFT_BitmapGlyph(gl);
    with result.Bitmaps[r]^ do
      begin
      with gl^.advance do
        begin
        advanceX := x shr 6;
        advanceY := y shr 6;
        end;
      with bm^ do
        begin
        height := bitmap.rows;
        width := bitmap.width;
        x := (pos.x shr 6) + left;   // transformed bitmap has correct x,y
        y := (pos.y shr 6) - top;    // not transformed has only a relative correction
        buf := CastleUtils.PByteArray(bitmap.buffer);
        reverse := (bitmap.pitch < 0);
        if reverse then
          begin
          pitch := -bitmap.pitch;
          {$ifndef FPC}System.{$endif}getmem (data, pitch*height);
          for rx := height-1 downto 0 do
            move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
          end
        else
          begin
          pitch := bitmap.pitch;
          rx := pitch*height;
          {$ifndef FPC}System.{$endif}getmem (data, rx);
          move (buf^[0], data^[0], rx);
          end;
        end;
      end;
    // place position for next glyph
    // The previous code in this place used shr 10, which
    // produces wrongly spaced text and looks very ugly
    // for more information see: http://bugs.freepascal.org/view.php?id=17156
    pos.x := pos.x + (gl^.advance.x shr 11);
    // pos.y := pos.y + (gl^.advance.y shr 6); // for angled texts also
    if prevx > pos.x then
      pos.x := prevx;
    // finish rendered glyph
    FT_Done_Glyph (gl);
    // Get the next unicode
    {$ifdef FPC}
    uc := UTF8CharacterToUnicode (pc, cl);
    {$endif}
  end;  // while
  result.FText := Text;
  result.CalculateGlobals;
end;

function TFontManager.GetString (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.GetStringGray (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
// Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.RequestFont (afilename:string) : integer;
begin
  result := RequestFont (afilename,0);
end;

function TFontManager.RequestFont (afilename:string; anindex:integer) : integer;
begin
  if afilename = '' then
    result := -1
  else
    begin
    result := GetFontID (afilename,anindex);
    if result < 0 then
      result := CreateFont (afilename,anindex);
    end;
end;

function TFontManager.GetFreeTypeFont (aFontID:integer) : PFT_Face;
begin
  result := GetFont(aFontID).font;
end;

{ TStringBitmaps }

function TStringBitmaps.GetCount : integer;
begin
  result := FList.Count;
end;

function TStringBitmaps.GetBitmap (index:integer) : PFontBitmap;
begin
  result := PFontBitmap(FList[index]);
end;

constructor TStringBitmaps.Create (ACount : integer);
var r : integer;
    bm : PFontBitmap;
begin
  inherited create;
  FList := Tlist.Create;
  FList.Capacity := ACount;
  for r := 0 to ACount-1 do
    begin
    new (bm);
    FList.Add (bm);
    end;
end;

destructor TStringBitmaps.destroy;
var r : integer;
    bm : PFontBitmap;
begin
  for r := 0 to Flist.count-1 do
    begin
    bm := PFontBitmap(FList[r]);
    freemem (bm^.data);
    dispose (bm);
    end;
  FList.Free;
  inherited;
end;

procedure TStringBitmaps.CalculateGlobals;
var r : integer;
begin
  if count = 0 then
    Exit;
  // check first 2 bitmaps for left side
  // check last 2 bitmaps for right side
  with BitMaps[0]^ do
    begin
    FBounds.left := x;
    FBounds.top := y + height;
    FBounds.bottom := y;
    end;
  with Bitmaps[count-1]^ do
    FBounds.right := x + width;
  if count > 1 then
    begin
    with Bitmaps[1]^ do
      r := x;
    if r < FBounds.left then
      FBounds.left := r;
    with Bitmaps[count-2]^ do
      r := x + width;
    if r > FBounds.right then
      FBounds.right := r;
    end;
  // check top/bottom of other bitmaps
  for r := 1 to count-1 do
    with Bitmaps[r]^ do
      begin
      if FBounds.top < y + height then
        FBounds.top := y + height;
      if FBounds.bottom > y then
        FBounds.bottom := y;
      end;
end;

procedure TStringBitmaps.GetBoundRect (out aRect : TRect);
begin
  aRect := FBounds;
end;

procedure InitFontMgr;
begin
  if not assigned (FontMgr) then
    FontMgr := TFontManager.create;
end;

procedure DoneFontMgr;
begin
  if assigned (FontMgr) then
    FontMgr.Free;
end;

initialization
finalization
  DoneFontMgr;
end.
