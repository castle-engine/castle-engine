{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit freetype;

{$DEFINE DYNAMIC}

interface

uses sysutils, classes, {$IFDEF DYNAMIC}freetypehdyn{$ELSE}freetypeh{$ENDIF}, FPImgCmn;

{ TODO : take resolution in account to find the size }
{ TODO : speed optimization: search glyphs with a hash-function/tree/binary search/... }
{ TODO : memory optimization: TStringBitmaps keeps for each differnet character
         only 1 bitmap }
{ TODO : load other files depending on the extention }
{ possible TODO : different sizes/resolutions for x and y }
{ possible TODO : TFontmanager can fill a list of all the fonts he can find
              fontfiles and faces available in a fontfile }

// determine if file comparison need to be case sensitive or not
{$ifdef windows}
  {$undef CaseSense}
{$else}
  {$define CaseSense}
{$endif}

type

  FreeTypeException = class (exception);

  TBitmapType = (btBlackWhite, bt256Gray);
  TFontBitmap = record
    height, width, pitch,
    x,y, bearingX, bearingY, advanceX, advanceY : integer;
    data : PByteArray;
  end;
  PFontBitmap = ^TFontBitmap;


  TBaseStringBitMaps = class
    private
      FList : TList;
      FBounds : TRect;
      FMode : TBitmapType;
      function GetCount : integer;
      function GetBitmap (index:integer) : PFontBitmap;
      procedure CalculateGlobals;
    public
      constructor Create (ACount : integer);
      destructor destroy; override;
      procedure GetBoundRect (out aRect : TRect);
      property Mode : TBitmapType read FMode;
      property Count : integer read GetCount;
      property Bitmaps[index:integer] : PFontBitmap read GetBitmap;
  end;

  TStringBitMaps = class(TBaseStringBitMaps)
    private
      FText : STring;
    public
      property Text : string read FText;
  end;

  TUnicodeStringBitMaps = class(TBaseStringBitMaps)
  private
    FText : UnicodeString;
  public
    property Text : Unicodestring read FText;
  end;

  TFontManager = class;

  PMgrGlyph = ^TMgrGlyph;
  TMgrGlyph = record
    Character : unicodechar;
    GlyphIndex : FT_UInt;
    Glyph : PFT_Glyph;
  end;

  PMgrSize = ^TMgrSize;
  TMgrSize = record
    Resolution, Size : integer;
    Glyphs : TList;
  end;

  TMgrFont = class
    private
      Mgr : TFontManager;
      Font : PFT_Face;
      FSizes : TList;
      Filename : string;
      LastSize : PMgrSize;
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
      FExtention : string;
      FResolution : integer;
      CurFont : TMgrFont;
      CurSize : PMgrSize;
      CurRenderMode : FT_Render_Mode;
      UseKerning : boolean;
      function GetSearchPath : string;
      procedure SetSearchPath (AValue : string);
      procedure SetExtention (AValue : string);
      Procedure DoMakeString (Text : Array of cardinal; ABitmaps  : TBaseStringBitmaps);
      Procedure DoMakeString (Text : Array of cardinal; angle: real; ABitmaps  : TBaseStringBitmaps);
    protected
      function GetFontId (afilename:string; anindex:integer) : integer;
      function CreateFont (afilename:string; anindex:integer) : integer;
      function GetFont (FontID:integer) : TMgrFont;
      procedure GetSize (aSize, aResolution : integer);
      function CreateSize (aSize, aResolution : integer) : PMgrSize;
      procedure SetPixelSize (aSize, aResolution : integer);
      function GetGlyph (c : cardinal) : PMgrGlyph;
      function CreateGlyph (c : cardinal) : PMgrGlyph;
      procedure MakeTransformation (angle:real; out Transformation:FT_Matrix);
      procedure InitMakeString (FontID, Size:real);
      function MakeString (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;
      function MakeString (FontId:integer; Text:string; Size:real) : TStringBitmaps;
      function MakeString (FontId:integer; Text:Unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;
      function MakeString (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;
    public
      constructor Create;
      destructor destroy; override;
      function SearchFont(afilename: string; doraise: boolean=true): string;
      function RequestFont (afilename:string) : integer;
      function RequestFont (afilename:string; anindex:integer) : integer;
      function GetFreeTypeFont (aFontID:integer) : PFT_Face;
      function GetString (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;
      function GetString (FontId:integer; Text:Unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;
      // Black and white
      function GetStringGray (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;
      function GetStringGray (FontId:integer; Text:unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;
      // Anti Aliased gray scale
      function GetString (FontId:integer; Text:string; Size:real) : TStringBitmaps;
      function GetString (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;
      // Black and white, following the direction of the font (left to right, top to bottom, ...)
      function GetStringGray (FontId:integer; Text: String; Size:real) : TStringBitmaps;
      function GetStringGray (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;
      // Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
      property SearchPath : string read GetSearchPath write SetSearchPath;
      property DefaultExtention : string read FExtention write SetExtention;
      property Resolution : integer read Fresolution write FResolution;
  end;

const
  sErrErrorsInCleanup : string = '%d errors detected while freeing a Font Manager object';
  sErrFontFileNotFound : string = 'Font file "%s" not found';
  sErrFreeType : string = 'Error %d while %s';
  sInitializing : string = 'initializing font engine';
  sDestroying : string = 'destroying font engine';
  sErrErrorInCleanup : string = 'freeing Font Manager object';
  sErrSetPixelSize : string = 'setting pixel size %d (resolution %d)';
  sErrSetCharSize : string = 'setting char size %d (resolution %d)';
  sErrLoadingGlyph : string = 'loading glyph';
  sErrKerning : string = 'determining kerning distance';
  sErrMakingString1 : string = 'making string bitmaps step 1';
  sErrMakingString2 : string = 'making string bitmaps step 2';
  sErrMakingString3 : string = 'making string bitmaps step 3';
  sErrMakingString4 : string = 'making string bitmaps step 4';
  sErrLoadFont : string = 'loading font %d from file %s';
  sErrInitializing : string = 'initializing FreeType';
  sErrDestroying : string = 'finalizing FreeType';

  DefaultFontExtention : string = '.ttf';

  // Standard location for fonts in the Operating System
  {$ifdef Darwin}
  DefaultSearchPath : string = '/Library/Fonts/';
  {$else}
  DefaultSearchPath : string = '';
  {$endif}

  {$IFDEF MAC}
  DefaultResolution : integer = 72;
  {$ELSE}
  DefaultResolution : integer = 96;
  {$ENDIF}

implementation

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
  {if CurFont^.style_name<>nil then begin
    writeln('   ',CurFont^.style_name^);
  end;}
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
    FTCheck(FT_New_Face (aMgr.FTLib, pchar(afilename), anindex, font),format (sErrLoadFont,[anindex,afilename]));
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
{$IFDEF DYNAMIC}
  if Pointer(FT_Init_FreeType)=Nil then
    InitializeFreetype();
{$ENDIF}
  r := FT_Init_FreeType(FTLib);
  if r <> 0  then
    begin
    FTLib := nil;
    FTError (sErrInitializing, r);
    end;
  SearchPath := DefaultSearchPath;
  DefaultExtention := DefaultFontExtention;
  Resolution := DefaultResolution;
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

function TFontManager.GetSearchPath : string;
var r : integer;
begin
  if FPaths.count > 0 then
    begin
    result := FPaths[0];
    for r := 1 to FPaths.count-1 do
      result := result + ';' + FPaths[r];
    end
  else
    result := '';
end;

procedure TFontManager.SetSearchPath (AValue : string);
  procedure AddPath (apath : string);
  begin
    FPaths.Add (IncludeTrailingBackslash(Apath));
  end;
var p : integer;
begin
  while (AValue <> '') do
    begin
    p := pos (';', AValue);
    if p = 0 then
      begin
      AddPath (AValue);
      AValue := '';
      end
    else
      begin
      AddPath (copy(AValue,1,p-1));
      delete (AVAlue,1,p);
      end;
    end;
end;

procedure TFontManager.SetExtention (AValue : string);
begin
  if AValue <> '' then
    if AValue[1] <> '.' then
      FExtention := '.' + AValue
    else
      FExtention := AValue
  else
    AValue := '';
end;

function TFontManager.SearchFont (afilename:string; doraise : boolean = true) : string;
// returns full filename of font, taking SearchPath in account
var p,fn : string;
    r : integer;
begin
  Result:='';
  if (pos('.', afilename)=0) and (DefaultFontExtention<>'') then
    fn := afilename + DefaultFontExtention
  else
    fn := aFilename;
  if FileExists(fn) then
    result := ExpandFilename(fn)
  else
    begin
    p := ExtractFilepath(fn);
    if p = '' then
      begin  // no path given, look in SearchPaths
      r := FPaths.Count;
      repeat
        dec (r);
      until (r < 0) or FileExists(FPaths[r]+fn);
      if r >= 0 then
        Result := FPaths[r]+fn;
      end
    end;
  if (Result='') and doRaise then
    raise FreeTypeException.CreateFmt (sErrFontFileNotFound, [fn])
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

procedure TFontManager.GetSize (aSize, aResolution : integer);
var r : integer;
begin
  if not ( assigned(CurSize) and
          (CurSize^.Size = aSize) and (CurSize^.resolution = aResolution)) then
    begin
    r := CurFont.FSizes.count;
    repeat
      dec (r)
    until (r < 0) or ( (PMgrSize(CurFont.FSizes[r])^.size = aSize) and
                       (PMgrSize(CurFont.FSizes[r])^.resolution = FResolution) );
    if r < 0 then
      CurSize := CreateSize (aSize,aResolution)
    else
      CurSize := PMgrSize(CurFont.FSizes[r]);
    SetPixelSize(CurSize^.Size, CurSize^.resolution);
    CurFont.LastSize := CurSize;
    end;
end;

function TFontManager.CreateSize (aSize, aResolution : integer) : PMgrSize;
begin
  new (result);
  result^.Size := aSize;
  result^.Resolution := aResolution;
  result^.Glyphs := Tlist.Create;
  SetPixelSize (aSize,aResolution);
  CurFont.FSizes.Add (result);
end;

procedure TFontManager.SetPixelSize (aSize, aResolution : integer);

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

var Err : integer;

begin
  with Curfont, Font^ do
    if (face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then
      begin
      CheckSize;
      Err := FT_Set_pixel_sizes (Font, aSize, aSize);
      if Err <> 0 then
        FTError (format(sErrSetPixelSize,[aSize,aResolution]), Err);
      end
    else
      begin
      Err := FT_Set_char_size (Font, aSize, aSize, aResolution, aResolution);
      if Err <> 0 then
        FTError (format(sErrSetCharSize,[aSize,aResolution]), Err);
      end;
end;

procedure TFontManager.MakeTransformation (angle:real; out Transformation:FT_Matrix);
begin
  with Transformation do
    begin
    xx := round( cos(angle)*$10000);
    xy := round(-sin(angle)*$10000);
    yx := round( sin(angle)*$10000);
    yy := round( cos(angle)*$10000);
    end;
end;

function TFontManager.CreateGlyph (c : cardinal) : PMgrGlyph;
var e : integer;
begin
  new (result);
  FillByte(Result^,SizeOf(Result),0);
  result^.character := unicodechar(c);
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

function TFontManager.GetGlyph (c : cardinal) : PMgrGlyph;
var r : integer;
begin
  With CurSize^ do
    begin
    r := Glyphs.Count;
    repeat
      dec (r)
    until (r < 0) or (PMgrGlyph(Glyphs[r])^.character = unicodechar(c));
    if r < 0 then
      result := CreateGlyph (c)
    else
      result := PMgrGlyph(Glyphs[r]);
    end;
end;

procedure TFontManager.InitMakeString (FontID, Size:real);
begin
  GetSize (round(size*64),Resolution);
  UseKerning := ((Curfont.font^.face_flags and FT_FACE_FLAG_KERNING) <> 0);
end;

function TFontManager.MakeString (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;

Var
  T : Array of cardinal;
  C,I : Integer;
  U: UnicodeString;

begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  U := UnicodeString(Text);
  c := length(U);
  result := TStringBitmaps.Create(c);
  result.FText := Text;
  SetLength(T,c);
  For I:=1 to c do
    T[I-1]:=Ord(U[i]);
  DoMakeString(T,Angle,Result);
end;

function TFontManager.MakeString (FontId:integer; Text:Unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;

Var
  T : Array of cardinal;
  c,I : Integer;

begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TUnicodeStringBitmaps.Create(c);
  result.FText := Text;
  SetLength(T,C);
  For I:=1 to c do
    T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Angle,Result);
end;


procedure TFontManager.DoMakeString(Text: Array of cardinal; angle:real; ABitmaps : TBaseStringBitmaps);

var g : PMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    prevIndex, prevx, r, rx : integer;
    pre, adv, pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
    trans : FT_Matrix;
    FBM : PFontBitmap;

begin
  if  (Angle = 0) or   // no angle asked, or can't work with angles (not scalable)
      ((CurFont.Font^.face_flags and FT_FACE_FLAG_SCALABLE)=0) then
    DoMakeString (Text, ABitmaps)
  else
    begin
    if (CurRenderMode = FT_RENDER_MODE_MONO) then
      ABitmaps.FMode := btBlackWhite
    else
      ABitmaps.FMode := bt256Gray;
    MakeTransformation (angle, trans);
    prevIndex := 0;
    prevx := 0;
    pos.x := 0;
    pos.y := 0;
    pre.x := 0;
    pre.y := 0;
    for r := 0 to Length(Text)-1 do
      begin
      // retrieve loaded glyph
      g := GetGlyph (Text[r]);
      // check kerning
      if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
        begin
        prevx := pre.x;
        FTCheck(FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern),sErrKerning);
        pre.x := pre.x + kern.x;
        end;
      // render the glyph
      Gl:=Nil;
      FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
      //    placing the glyph
      FTCheck(FT_Glyph_Transform (gl, nil, @pre),sErrMakingString2);
      adv := gl^.advance;
      //    rotating the glyph
      FTCheck(FT_Glyph_Transform (gl, @trans, nil),sErrMakingString3);
      //    rendering the glyph
      FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, nil, true),sErrMakingString4);
      // Copy what is needed to record
      bm := PFT_BitmapGlyph(gl);
      FBM:=ABitmaps.Bitmaps[r];
      with FBM^ do
        begin
        with gl^.advance do
          begin
          // do not use shr 16 - rotated text can have negative advances
          advanceX := x div 65536;
          advanceY := y div 65536;
          end;
        with bm^ do
          begin
          height := bitmap.rows;
          width := bitmap.width;
          // transformed bitmap has correct x,y
          x := {(pos.x div 64)} + left;
          y := {(pos.y div 64)} - top;
          // bearings are not supported for rotated text (don't make sense)
          bearingX := 0;
          bearingY := 0;
          buf := PByteArray(bitmap.buffer);
          reverse := (bitmap.pitch < 0);
          if reverse then
            begin
            pitch := -bitmap.pitch;
            getmem (data, pitch*height);
            for rx := height-1 downto 0 do
              move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
            end
          else
            begin
            pitch := bitmap.pitch;
            rx := pitch*height;
            if RX=0 then
              Data:=Nil
            else
              begin
              getmem (data, rx);
              move (buf^[0], data^[0], rx);
              end;
            end;
          end;
        end;
      // place position for next glyph
      with gl^.advance do
        begin
        pos.x := pos.x + (x div 1024);
        pos.y := pos.y + (y div 1024);
        end;
      with adv do
        pre.x := pre.x + (x div 1024);
      if prevx > pre.x then
        pre.x := prevx;
      // finish rendered glyph
      FT_Done_Glyph (gl);
      end;
    ABitmaps.CalculateGlobals;
    end;
end;

function TFontManager.MakeString (FontId:integer; Text:string; Size:real) : TStringBitmaps;

Var
  T : Array of Cardinal;
  C,I : Integer;
  U : UnicodeString;
  
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  U := UnicodeString(Text);
  c := length(U);
  result := TStringBitmaps.Create(c);
  result.FText := Text;
  SetLength(T,c);
  For I:=1 to c do
    T[I-1]:=Ord(U[i]);
  DoMakeString(T,Result);
end;

function TFontManager.MakeString (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;

Var
  T : Array of Cardinal;
  C,I : Integer;
  
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TUnicodeStringBitmaps.Create(c);
  result.FText := Text;
  SetLength(T,C);
  For I:=1 to C do
    T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Result);
end;

Procedure TFontManager.DoMakeString (Text : Array of cardinal; ABitmaps  : TBaseStringBitmaps);

var g : PMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    e, prevIndex, prevx, r, rx : integer;
    pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
    bmpr : PFontBitmap;
begin
  if (CurRenderMode = FT_RENDER_MODE_MONO) then
    ABitmaps.FMode := btBlackWhite
  else
    ABitmaps.FMode := bt256Gray;
  prevIndex := 0;
  prevx := 0;
  pos.x := 0;
  pos.y := 0;
  for r := 0 to length(text)-1 do
    begin
    // retrieve loaded glyph
    g := GetGlyph (Text[r]);
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
    FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, PFT_Vector(0), true),sErrMakingString4);
    // Copy what is needed to record
    bm := PFT_BitmapGlyph(gl);
    bmpr := ABitmaps.Bitmaps[r];
    with bmpr^ do
      begin
      // glyph size including bearings all around
      with gl^.advance do
        begin
        advanceX := x shr 16;
        advanceY := y shr 16;
        end;
      with bm^ do
        begin
        // glyph pixel size
        height := bitmap.rows;
        width := bitmap.width;
        // origin of the glyph
        x := pos.x shr 6;
        y := pos.y shr 6;
        // bearing - where the pixels start relative to x/y origin
        bearingX := left;
        bearingY := top;
        buf := PByteArray(bitmap.buffer);
        reverse := (bitmap.pitch < 0);
        if reverse then
          begin
          pitch := -bitmap.pitch;
          getmem (data, pitch*height);
          for rx := height-1 downto 0 do
            move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
          end
        else
          begin
          pitch := bitmap.pitch;
          rx := pitch*height;
          getmem (data, rx);
          move (buf^[0], data^[0], rx);
          end;
        end;
      end;
    // place position for next glyph
    // pos is in 26.6 format, whereas advance is in 16.16 format - we need (shr (16-6)) for the conversion
    pos.x := pos.x + (gl^.advance.x shr 10);
    if prevx > pos.x then
      pos.x := prevx;
    // finish rendered glyph
    FT_Done_Glyph (gl);
    end;
  ABitmaps.CalculateGlobals;
end;

function TFontManager.GetString (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;
// Black and white
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size, angle);
end;

function TFontManager.GetStringGray (FontId:integer; Text:string; size:real; angle:real) : TStringBitmaps;
// Anti Aliased gray scale
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size, angle);
end;

{ Procedures without angle have own implementation to have better speed }

function TFontManager.GetString (FontId:integer; Text:string; Size:real) : TStringBitmaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.GetStringGray (FontId:integer; Text:string; Size:real) : TStringBitmaps;
// Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.GetString (FontId:integer; Text:Unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;
// Black and white
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size, angle);
end;

function TFontManager.GetStringGray (FontId:integer; Text:Unicodestring; size:real; angle:real) : TUnicodeStringBitmaps;
// Anti Aliased gray scale
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size, angle);
end;

{ Procedures without angle have own implementation to have better speed }

function TFontManager.GetString (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.GetStringGray (FontId:integer; Text:Unicodestring; Size:real) : TUnicodeStringBitmaps;
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
var s : string;
begin
  if afilename = '' then
    result := -1
  else
    begin
    s := SearchFont (afilename);
    result := GetFontID (s,anindex);
    if result < 0 then
      result := CreateFont (s,anindex);
    end;
end;

function TFontManager.GetFreeTypeFont (aFontID:integer) : PFT_Face;
begin
  result := GetFont(aFontID).font;
end;

{ TStringBitmaps }

function TBaseStringBitmaps.GetCount : integer;
begin
  result := FList.Count;
end;

function TBaseStringBitmaps.GetBitmap (index:integer) : PFontBitmap;
begin
  result := PFontBitmap(FList[index]);
end;

constructor TBaseStringBitmaps.Create (ACount : integer);
var r : integer;
    bm : PFontBitmap;
begin
  inherited create;
  FList := Tlist.Create;
  FList.Capacity := ACount;
  for r := 0 to ACount-1 do
    begin
    new (bm);
    FillChar(BM^,SizeOf(TFontBitmap),#0);
    FList.Add (bm);
    end;
end;

destructor TBaseStringBitmaps.destroy;
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

(*
Procedure DumpBitmap(BM : PFontBitmap);

begin
  Writeln('Bitmap h: ',BM^.height,', w: ',BM^.width,', x:',BM^.x,', y: ',bm^.y);
end;
*)

procedure TBAseStringBitmaps.CalculateGlobals;
var
  r : integer;
  Bmp : PFontBitmap;

begin
  if count = 0 then
    Exit;
  Bmp := Bitmaps[0];
  with Bmp^ do
    begin
    FBounds.left := x;
    FBounds.top := y + bearingY;
    FBounds.bottom := y + bearingY - height;
    end;
  Bmp := Bitmaps[Count-1];
  With Bmp^ do
    begin
    FBounds.right := x + advanceX;
    // typographically it is not correct to check the real width of the character
    //   because accents can exceed the advance (e.g. í - the dash goes beyond the character
    //   but i and í should have the same width)
    // on the other hand for some fonts the advance is always 1px short also for normal characters
    //   and also with this we support rotated text
    if FBounds.right < x + bearingX + width then
      FBounds.right := x + bearingX + width;
    end;
  // check top/bottom of other bitmaps
  for r := 1 to count-1 do
    begin
    Bmp := Bitmaps[r];
    with Bmp^ do
      begin
      if FBounds.top < y + bearingY then
        FBounds.top := y + bearingY;
      if FBounds.bottom > y + bearingY - height then
        FBounds.bottom := y + bearingY - height;
      end;
    end;
end;

procedure TBaseStringBitmaps.GetBoundRect (out aRect : TRect);
begin
  aRect := FBounds;
end;

{$ifdef WINDOWS}
procedure SetWindowsFontPath;
begin
  DefaultSearchPath := includetrailingbackslash(GetEnvironmentVariable('windir')) + 'fonts';
end;
{$endif}

initialization
  {$ifdef WINDOWS}
  SetWindowsFontPath;
  {$endif}
end.
