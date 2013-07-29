{
  Copyright 2001-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL bitmap fonts (TGLBitmapFont). }

unit CastleGLBitmapFonts;

interface

uses CastleBitmapFonts, GL, GLU, CastleGLUtils, Classes;

type
  { Abstract class for all OpenGL bitmap fonts. }
  TGLBitmapFontAbstract = class
  protected
    FRowHeight: Integer;
    FRowHeightBase: Integer;
  public
    { Draw text at the current OpenGL raster position, and move
      the raster position at the end. This way you can immediately
      call another PrintAndMove again, to add something at the end.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it moves raster position. }
    procedure PrintAndMove(const s: string); virtual; abstract;

    { Draw text at the current OpenGL raster position.
      In contrast to PrintAndMove, raster position is not changed.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    procedure Print(const s: string);

    function TextWidth(const s: string): integer; virtual; abstract;
    function TextHeight(const s: string): integer; virtual; abstract;

    { The height (above the baseline) of the text.
      This doesn't take into account height of the text below the baseline
      (for example letter "y" has the tail below the baseline in most fonts). }
    function TextHeightBase(const s: string): integer; virtual; abstract;

    { Height of a row of text in this font.
      This may be calculated as simply @code(TextHeight('Wy')) for most
      normal fonts. }
    property RowHeight: integer read FRowHeight;

    { Height (above the baseline) of a row of text in this font.
      Similar to TextHeightBase and TextHeight,
      note that RowHeightBase is generally smaller than RowHeight,
      because RowHeightBase doesn't care how low the letter may go below
      the baseline. }
    property RowHeightBase: Integer read FRowHeightBase;

    { How low the text may go below the baseline.
      By default this returns @code(TextHeight('y')-TextHeight('a')),
      which is suitable for normal fonts. }
    function Descend: integer; virtual;

    { Break lines (possibly break one long string into more strings)
      to fit the text with given MaxLineWidth.

      This takes into account current font information (works also
      for non-monospace fonts, of course), and converts your Unbroken
      text into Broken text, such that TextWidth of the longest Broken
      line fits within MaxLineWidth.

      Tries to break on white characters. If not possible (there's
      a long stream of non-white characters that really has to be broken),
      it will break in the middle of normal (non-white) characters.
      The only situation when we have to fail, and the resulting
      Broken text is wider than required MaxLineWidth, is when
      @italic(a single character in your font) is wider than MaxLineWidth.
      In such case, there's really no solution, and we'll just let such
      character stay.

      If you use the overloaded version where Unbroken is just a string,
      then note that already existing newlines (NL) inside Unbroken
      will be correctly preserved.

      If you use the overloaded version with separate Unbroken and
      Broken parameters, then the previous Broken contents are not modified.
      We only append to Broken new strings, coming from Unbroken text.
      The overloaded version that takes only Broken parameter
      (no Unbroken parameter) simply modifies it's Broken parameter
      (from the line FirstToBreak).

      @groupBegin }
    procedure BreakLines(const unbroken: string; broken: TStrings; maxLineWidth: integer); overload;
    procedure BreakLines(unbroken, broken: TStrings; maxLineWidth: integer); overload;
    procedure BreakLines(broken: TStrings; maxLineWidth: integer; FirstToBreak: integer); overload;
    { @groupEnd }

    { Largest width of the line of text in given list.

      @param(Tags Indicates that strings inside SList use HTML-like
        tags, the same as interpreted by PrintStrings.
        If your SList uses these tags (for example, you plan to call later
        PrintStrings with the same SList and Tags = @true) then make
        sure you pass Tags = @true to this method.
        Otherwise, MaxTextWidth will treat tags text (like @code(<font ...>))
        like a normal text, usually making the width incorrectly large.)
    }
    function MaxTextWidth(SList: TStringList; const Tags: boolean = false): integer;

    { Print all strings from the list.

      X0, Y0 is the bottom-left position of the whole text block
      (that is, it is the bottom-left position of the last string).
      Distance between each line is (RowHeight + BonusVerticalSpace) pixels.

      If PositionAsRaster is @true (this is default, for backwards compatibility)
      then the (X0, Y0) position is interpreted as OpenGL raster 2D position
      (so it is transformed by modelview matrix and such).
      For new code, we advice using PositionAsRaster = @false,
      as this will ease future transitions to OpenGL versions >= 3.

      Note that BonusVerticalSpace can be < 0 (as well as > 0),
      this may be sometimes useful if you really want to squeeze
      more text into some size. Still, make sure that
      (RowHeight + BonusVerticalSpace) is > 0.

      Previous OpenGL raster position value will be ignored
      and then modified by this method.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it moves raster position.

      @param(Tags Enable some HTML-like tags to mark font changes inside the text.
        For now, these can only be used to surround whole lines
        (so you have to place opening tag at the beginnig of line,
        and closing tag at the end of line).
        For now, the only tag handled is @code(<font color="#rrggbb">)
        that changes line color to specified RGB.
        Also, we handle @code(<font color="#rrggbbaa">) where the last
        component is alpha (opacity), and when it's < 1 then we render using blending.
        Close with @code(</font>).

        This functionality may be enhanced in the future (feature requests
        and patches welcome). Don't expect full HTML implementation inside,
        but some small set of useful tags may be doable and comfortable to use.
        Not necessarily replicating some (old version of) HTML standard.
      )

      @groupBegin }
    procedure PrintStrings(strs: TStrings;
      const Tags: boolean; BonusVerticalSpace: TGLint;
      const X0: Integer = 0;
      const Y0: Integer = 0;
      const PositionAsRaster: boolean = true); overload;
    procedure PrintStrings(const Strs: array of string;
      const Tags: boolean;
      BonusVerticalSpace: TGLint;
      const X0: Integer = 0;
      const Y0: Integer = 0;
      const PositionAsRaster: boolean = true); overload;
    { @groupEnd }

    { Print the string, broken such that it fits within MaxLineWidth.
      The string is broken into many lines using BreakLines,
      so the original newlines insides are correctly used,
      and the length of lines fits inside MaxLineWidth.

      The strings are printed on the screen, just like by PrintStrings
      (with Tags = always false for now, since our string breaking cannot
      omit tags).
      If PositionsFirst then the X0, Y0 determine
      the position of the first (top) line, otherwise they determine
      the position of the last (bottom) line.

      BonusVerticalSpace has the same meaning as for PrintStrings:
      it adds an additional space between lines (if positive) or forces
      the lines to be more tightly squeezed (if negative). Always make
      sure that (RowHeight + BonusVerticalSpace) > 0.

      Returns the number of lines printed, that is the number of lines
      after breaking the text into lines. This may be useful e.g. to calculate
      the height of the printed text.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    function PrintBrokenString(const s: string;
      MaxLineWidth, X0, Y0: Integer;
      PositionsFirst: boolean; BonusVerticalSpace: Integer;
      const PositionAsRaster: boolean = true): Integer;

    { Print all strings from the list, and draw a box with frames
      around it.

      The text is printed like by PrintStrings.

      For the deprecated overloaded versions without X0, Y0:
      the position of the text and box is determined by the current
      modelview matrix. The left-bottom box corner is at raster position 0, 0.
      The current raster position when this method is called doesn't matter.
      So your only way to move this box is to modify modelview matrix.

      For the new overloaded versions with X0, Y0:
      the X0, Y0 give explicitly the left-bottom box corner (in window
      coordinates, i.e. applied with SetWindowPos).

      BonusVerticalSpace has the same interpretation as for PrintStrings:
      additional space between lines (if positive) or forces
      the lines to be more tightly squeezed (if negative). Always make
      sure that (RowHeight + BonusVerticalSpace) > 0.

      The box background inside has color InsideCol. The box frame
      has color BorderCol. The text has color TextCol.

      BoxPixelMargin is the distance (in pixels) between text and the box
      frame.

      We assume that moving by 1 in modelview matrix is equal to moving 1 pixel.
      In other words, we assume you have normal 2D orthographic projection with
      the dimensions equal to pixel dimensions.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it modifies the raster position.

      @groupBegin }
    procedure PrintStringsBox(const Strs: array of string;
      const Tags: boolean; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer) overload; deprecated;

    procedure PrintStringsBox(Strs: TStringList;
      const Tags: boolean; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer) overload; deprecated;

    procedure PrintStringsBox(const Strs: array of string;
      const Tags: boolean; const X0, Y0: Integer; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer); overload;

    procedure PrintStringsBox(Strs: TStringList;
      const Tags: boolean; const X0, Y0: Integer; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer); overload;
    { @groupEnd }
  end;

  TGLBitmapFontClass = class of TGLBitmapFontAbstract;

  { OpenGL bitmap font. Uses a font description from CastleBitmapFonts unit
    (TBitmapFont type), and creates OpenGL resources to render text
    using this font.

    This way we can use fonts embedded in our source code
    (as TBitmapFont type), independent from the fonts available in external
    files, operating system and such. Or we can load TBitmapFont from file.

    See also TGLOutlineFont for a similar class for outline fonts. }
  TGLBitmapFont = class(TGLBitmapFontAbstract)
  private
    base: TGLuint;
    BitmapFont: TBitmapFont;
  public
    { Create OpenGL resources to render given bitmap font.

      We remember the pointer BitmapFont, without copying the contents.
      So do not free the BitmapFont contents, do not change it at all
      actually, during the lifetime of this object. }
    constructor Create(ABitmapFont: TBitmapFont);
    destructor Destroy; override;

    procedure PrintAndMove(const s: string); override;
    function TextWidth(const s: string): integer; override;
    function TextHeight(const s: string): integer; override;
    function TextHeightBase(const s: string): integer; override;
  end;

implementation

uses CastleUtils, CastleVectors, CastleStringUtils, CastleClassUtils;

{ HandleTags ----------------------------------------------------------------- }

function HandleTags(const S: string;
  out ColorChange: boolean; out Color: TVector4Single): string;

  function ExtractColor(const S: string; P: Integer;
    out Color: TVector4Single; out Length: Integer): boolean;
  const
    HexDigits = ['0'..'9', 'a'..'f', 'A'..'F'];
  begin
    Result := SCharIs(S, P    , HexDigits) and
              SCharIs(S, P + 1, HexDigits) and
              SCharIs(S, P + 2, HexDigits) and
              SCharIs(S, P + 3, HexDigits) and
              SCharIs(S, P + 4, HexDigits) and
              SCharIs(S, P + 5, HexDigits);
    Length := 6;
    if Result then
    begin
      Color[0] := StrHexToInt(Copy(S, P    , 2)) / 255;
      Color[1] := StrHexToInt(Copy(S, P + 2, 2)) / 255;
      Color[2] := StrHexToInt(Copy(S, P + 4, 2)) / 255;
      if SCharIs(S, P + 6, HexDigits) and
         SCharIs(S, P + 7, HexDigits) then
      begin
        Length += 2;
        Color[3] := StrHexToInt(Copy(S, P + 6, 2)) / 255;
      end else
        Color[3] := 1;
    end;
  end;

  { Is SubText present inside Text on position P.
    Secure for all lengths and values of position (that is, will answer
    false if P is <= 0 or P is too large and some part of SubText would
    be outside S). }
  function SubStringMatch(const SubText, Text: string; P: Integer): boolean;
  var
    I: Integer;
  begin
    Result := (P >= 1) and
              (P <= { signed } Integer(Length(Text)) - Length(SubText) + 1);
    if Result then
      for I := 1 to Length(SubText) do
      begin
        if SubText[I] <> Text[P] then Exit(false);
        Inc(P);
      end;
  end;

const
  SFontColorBegin1 = '<font color="#';
  SFontColorBegin2 = '">';
  SFontEnd = '</font>';
var
  ColorLength: Integer;
begin
  ColorChange :=
    { first check something most likely to fail, for speed }
    SCharIs(S, 1, '<') and
    SubStringMatch(SFontColorBegin1, S, 1) and
    ExtractColor(S, Length(SFontColorBegin1) + 1, Color, ColorLength) and
    SubStringMatch(SFontColorBegin2, S, Length(SFontColorBegin1) + ColorLength + 1) and
    SubStringMatch(SFontEnd, S, Length(S) - Length(SFontEnd) + 1);

  if ColorChange then
  begin
    Result := CopyPos(S,
      Length(SFontColorBegin1) + Length(SFontColorBegin2) + ColorLength + 1,
      Length(S) - Length(SFontEnd));
  end else
    Result := S;
end;

const BitmapTableCount = Ord(High(char)) - Ord(Low(char)) +1;

constructor TGLBitmapFont.Create(ABitmapFont: TBitmapFont);
var i: Cardinal;
    Znak: PBitmapChar;
    Saved_Unpack_Alignment: TGLint;
begin
 inherited Create;
 base := glGenListsCheck(BitmapTableCount, 'TGLBitmapFont.Create');
 BitmapFont := ABitmapFont;
 Saved_Unpack_Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);

 for i := 0 to 255 do
 begin
  Znak := BitmapFont.Data[Chr(i)];
  glPixelStorei(GL_UNPACK_ALIGNMENT, Znak^.Info.Alignment);
  glNewList(i+base, GL_COMPILE);
  glBitmap(Znak^.Info.Width, Znak^.Info.Height,
           Znak^.Info.XOrig, Znak^.Info.YOrig,
           Znak^.Info.XMove, Znak^.Info.YMove,
           @Znak^.Data);
  glEndList;
 end;
 glPixelStorei(GL_UNPACK_ALIGNMENT, Saved_Unpack_Alignment);

 FRowHeight := TextHeight('Wy') + 2;
 { RowHeight zwiekszylem o +2 zeby byl odstep miedzy liniami.
   TODO: this +2 is actually a bad idea, but can't remove now without careful testing. }
 { For RowHeightBase, I do not use +2. }
 FRowHeightBase := TextHeightBase('W');
end;

destructor TGLBitmapFont.Destroy;
begin
 glDeleteLists(base, BitmapTableCount);
 inherited;
end;

procedure TGLBitmapFont.PrintAndMove(const s: string);
begin
 glPushAttrib(GL_LIST_BIT);
   glListIBase(TGLint(base));
   glCallLists(length(s), GL_UNSIGNED_BYTE, PChar(s));
 glPopAttrib;
end;

function TGLBitmapFont.TextWidth(const s: string): integer;
var i: integer;
begin
 Result := 0;
 for i := 1 to length(s) do
  Result := Result + Round(BitmapFont.Data[s[i]]^.Info.XMove);
end;

function TGLBitmapFont.TextHeight(const s: string): integer;
var
  i: integer;
  minY, maxY, YOrig: integer;
begin
  minY := 0;
  maxY := 0;
  for i := 1 to length(s) do
  begin
    YOrig := Round(BitmapFont.Data[s[i]]^.Info.YOrig);
    MinTo1st(minY, -YOrig);
    { Yes, YOrig is *subtracted* here, see glBitmap meaning of yorig. }
    MaxTo1st(maxY, BitmapFont.Data[s[i]]^.Info.Height - YOrig);
  end;
  result := maxY - minY;
end;

function TGLBitmapFont.TextHeightBase(const s: string): integer;
var
  I: integer;
  YOrig: integer;
begin
  Result := 0;
  { This is just like TGLBitmapFont.TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }
  for i := 1 to length(s) do
  begin
    YOrig := Round(BitmapFont.Data[s[i]]^.Info.YOrig);
    MaxTo1st(Result, BitmapFont.Data[s[i]]^.Info.Height - YOrig);
  end;
end;

{ TGLBitmapFontAbstract ------------------------------------------------------}

procedure TGLBitmapFontAbstract.Print(const s: string);
var
  rasterPos4f: TVector4f;
begin
  { TODO: it is very ugly to do glGetFloatv here, may be slow for OpenGL
    if you call this often. We should rather calculate the shift
    do by PrintAndMove, and move back by glBitmap with x/yoffset params.

    This problem will become moot when new bitmap font rendering,
    using textures instead of rasters and textures, will be implemented. }

  glGetFloatv(GL_CURRENT_RASTER_POSITION, @rasterPos4f);
  PrintAndMove(s);
  glRasterPos4fv(@rasterPos4f);
end;

function TGLBitmapFontAbstract.Descend: integer;
begin
  result := TextHeight('y')-TextHeight('a');
end;

procedure TGLBitmapFontAbstract.BreakLines(const unbroken: string;
  broken: TStrings; maxLineWidth: integer);
var
  unbrokenlist: TStringList;
begin
  unbrokenlist := TStringList.Create;
  try
    Strings_SetText(unbrokenlist, unbroken);
    BreakLines(unbrokenlist, broken, maxLineWidth);
  finally unbrokenlist.Free end;
end;

procedure TGLBitmapFontAbstract.BreakLines(unbroken, broken: TStrings;
  maxLineWidth: integer);
var
  i, FirstToBreak: integer;
begin
  FirstToBreak := broken.count;
  for i := 0 to unbroken.count-1 do broken.Append(unbroken[i]);
  BreakLines(broken, maxLineWidth, FirstToBreak);
end;

procedure TGLBitmapFontAbstract.BreakLines(broken: TStrings;
  maxLineWidth: integer; FirstToBreak: integer);
var
  i, j: integer;
  linew: integer;
  p: integer;
  break1, break2: string;
begin
  { ponizej lamiemy stringi unbroken.
    Lamanie to nie jest takie proste bo my nie mamy czegos takiego jak
    MaxCol - ilosc znakow w linii, bo kazdy znak moze miec inna szerokosc -
    font nie musi byc monospaced ! Gdyby byl - no coz, to robota bylaby prosta :
     broken.text := WrapText(broken.text, maxLineWidth div font.TextWidth('w'));
     (no, zakladajac ze FirstToBreak = 0)
    i juz. A tak - musimy po kolei badac kazdy string szukajac w nim literki
    ktora sprawia ze nie miesci sie w maxLineWidth i wtedy obcinac.
  }

  i := FirstToBreak;
  { instead of "for" use "while" because broken.count will be changing }
  while i < broken.count do
  begin
    { zobacz czy nie trzeba zlamac linii nr i.
      Linii '' z pewnoscia nie trzeba lamac. }
    if broken[i] <> '' then
    begin
      { ponizsze dwie linijki implikuja ze do zlamanej linii ZAWSZE trafia
        pierwszy znak z linii niezlamanej, NAWET jesli ten pierwszy znak
        jest szerszy niz maxLineWidth. No bo jezeli ten znak jest szerszy
        od maxLineWidth to przeciez nie moglby trafic do ZADNEJ linii,
        prawda ? Jedyna alternatywa byloby rzucenie w takim wypadku
        wyjatku z komunikatem ze "maxLineWidth" jest za male zeby w pelni
        poprawnie polamac string. }
      linew := TextWidth(broken[i][1]);
      j := 2;
      while (j <= Length(broken[i])) and
            (linew + TextWidth(broken[i][j]) <= maxLineWidth) do
      begin
        linew := linew + TextWidth(broken[i][j]);
        Inc(j);
      end;
      if j <= Length(broken[i]) then
      begin
        { oho ! ta linie trzeba zlamac przed znakiem j, bo linia jest za dluga kiedy
          ma j znakow. Efekt breaka bedzie tez taki ze broken.count sie zwiekszy wiec
          w nastepnym obrocie petli bedziemy lamali dalsza czesc tej linii - i o to
          chodzi. }
        p := BackCharsPos(WhiteSpaces, Copy(broken[i], 1,j));
        if p > 0 then
        begin
          break1 := Copy(broken[i], 1,p-1);
          break2 := SEnding(broken[i], p+1) { break at pos p, delete p-th char }
        end else
        begin
          break1 := Copy(broken[i], 1,j-1);
          break2 := SEnding(broken[i], j);  { break at pos j-1 }
        end;
        broken[i] := break1;
        broken.Insert(i+1, break2);
      end;
    end;

    Inc(i);
  end;
end;

function TGLBitmapFontAbstract.MaxTextWidth(SList: TStringList;
  const Tags: boolean): Integer;
var
  I, LineW: integer;
  DummyColorChange: boolean;
  DummyColor: TVector4Single;
  S: string;
begin
  result := 0;
  for i := 0 to slist.Count-1 do
  begin
    S := SList[i];
    if Tags then
      S := HandleTags(S, DummyColorChange, DummyColor);
    LineW := TextWidth(S);
    if LineW > result then result := LineW;
  end;
end;

procedure TGLBitmapFontAbstract.PrintStrings(const Strs: array of string;
  const Tags: boolean; BonusVerticalSpace: TGLint;
  const X0, Y0: Integer;
  const PositionAsRaster: boolean);
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    AddStrArrayToStrings(Strs, SList);
    PrintStrings(SList, Tags, BonusVerticalSpace, X0, Y0, PositionAsRaster);
  finally SList.Free end;
end;

procedure TGLBitmapFontAbstract.PrintStrings(strs: TStrings;
  const Tags: boolean; BonusVerticalSpace: TGLint;
  const X0, Y0: Integer;
  const PositionAsRaster: boolean);
var
  Line: Integer;

  procedure SetPos;
  var
    Y: Integer;
  begin
    Y := (Strs.Count - 1 - Line) * (RowHeight + BonusVerticalSpace) + Y0;
    if PositionAsRaster then
      glRasterPos2i(X0, Y) else
      SetWindowPos(X0, Y);
  end;

var
  S: string;
  ColorChange: boolean;
  Color: TVector4Single;
begin
  for Line := 0 to Strs.Count - 1 do
  begin
    S := Strs[Line];
    if Tags then
    begin
      S := HandleTags(S, ColorChange, Color);
      if ColorChange then
      begin
        glPushAttrib(GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT);
          { glColor must be before setting raster to have proper effect.
            It will be correctly saved/restored by glPush/PopAttrib,
            as docs say that GL_CURRENT_BIT saves
            "RGBA color associated with current raster position". }
          glColorv(Color); // saved by GL_CURRENT_BIT
          if Color[3] < 1 then
          begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
            glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
          end;
          SetPos;
          PrintAndMove(S);
        glPopAttrib;
      end else
      begin
        SetPos;
        PrintAndMove(S);
      end;
    end else
    begin
      SetPos;
      PrintAndMove(S);
    end;
  end;
end;

function TGLBitmapFontAbstract.PrintBrokenString(const s: string;
  MaxLineWidth, X0, Y0: integer;
  PositionsFirst: boolean; BonusVerticalSpace: Integer;
  const PositionAsRaster: boolean): Integer;
var
  broken: TStringList;
begin
  broken := TStringList.Create;
  try
    BreakLines(s, broken, MaxLineWidth);
    if PositionsFirst then
      Y0 -= (broken.Count-1)*(RowHeight + BonusVerticalSpace);
    PrintStrings(broken, false, BonusVerticalSpace, X0, Y0, PositionAsRaster);
    result := broken.Count;
  finally broken.Free end;
end;

procedure TGLBitmapFontAbstract.PrintStringsBox(
  Strs: TStringList; const Tags: boolean; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
begin
  GLRectangleWithBorder(0, 0, MaxTextWidth(Strs, Tags) + 2 * BoxPixelMargin,
    (RowHeight + BonusVerticalSpace) * Strs.Count + 2 * BoxPixelMargin + Descend,
    InsideCol, BorderCol);
  glColorv(TextCol);
  PrintStrings(strs, Tags, BonusVerticalSpace, BoxPixelMargin,
    BoxPixelMargin + Descend);
end;

procedure TGLBitmapFontAbstract.PrintStringsBox(
  const Strs: array of string; const Tags: boolean; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
var
  slist: TStringList;
begin
  slist := TStringList.Create;
  try
    AddStrArrayToStrings(strs, slist);
    {$warnings off}
    { Do not warn that PrintStringsBox call below is deprecated.
      *This* method is deprecated too. }
    PrintStringsBox(slist, Tags, BonusVerticalSpace,
      InsideCol, BorderCol, TextCol, BoxPixelMargin);
    {$warnings on}
  finally slist.Free end;
end;

procedure TGLBitmapFontAbstract.PrintStringsBox(
  Strs: TStringList; const Tags: boolean;
  const X0, Y0: Integer; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
begin
  GLRectangleWithBorder(X0, Y0,
    X0 + MaxTextWidth(Strs, Tags) + 2 * BoxPixelMargin,
    Y0 + (RowHeight + BonusVerticalSpace) * Strs.Count + 2 * BoxPixelMargin + Descend,
    InsideCol, BorderCol);
  glColorv(TextCol);
  PrintStrings(strs, Tags, BonusVerticalSpace, X0 + BoxPixelMargin,
    Y0 + BoxPixelMargin + Descend, false);
end;

procedure TGLBitmapFontAbstract.PrintStringsBox(
  const Strs: array of string; const Tags: boolean;
  const X0, Y0: Integer; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
var
  slist: TStringList;
begin
  slist := TStringList.Create;
  try
    AddStrArrayToStrings(strs, slist);
    PrintStringsBox(slist, Tags, X0, Y0, BonusVerticalSpace,
      InsideCol, BorderCol, TextCol, BoxPixelMargin);
  finally slist.Free end;
end;

end.
