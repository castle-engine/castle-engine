{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base abstract classes for OpenGL fonts (TGLBitmapFont_Abstract,
  TGLOutlineFont_Abstract). }
unit OpenGLFonts;

interface

uses Classes, GL, GLU, SysUtils, CastleGLUtils;

type
  { Abstract class for all OpenGL bitmap fonts. }
  TGLBitmapFont_Abstract = class
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

    { Largest width of the line of text in given list. }
    function MaxTextWidth(slist: TStringList): integer;

    { Print all strings from the list.
      glRasterPos2i(RasterX0, RasterY0) is the position of the last string,
      each previous string will be RowHeight + BonusVerticalSpace higher.

      Note that BonusVerticalSpace can be < 0 (as well as > 0),
      this may be sometimes useful if you really want to squeeze
      more text into some size. Still, make sure that
      (RowHeight + BonusVerticalSpace) is > 0.

      Previous OpenGL raster position value will be ignored
      and then modified by this method.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it moves raster position.
      @groupBegin }
    procedure PrintStrings(strs: TStrings;
      BonusVerticalSpace: TGLint;
      const RasterX0: Integer = 0;
      const RasterY0: Integer = 0); overload;
    procedure PrintStrings(const strs: array of string;
      BonusVerticalSpace: TGLint;
      const RasterX0: Integer = 0;
      const RasterY0: Integer = 0); overload;
    { @groupEnd }

    { Print the string, broken such that it fits within MaxLineWidth.
      The string is broken into many lines using BreakLines,
      so the original newlines insides are correctly used,
      and the length of lines fits inside MaxLineWidth.

      The strings are printed on the string, just like by PrintStrings.
      If RasterPositionsFirst then the RasterX0, RasterY0 determine
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
      MaxLineWidth, RasterX0, RasterY0: Integer;
      RasterPositionsFirst: boolean; BonusVerticalSpace: Integer): Integer;

    { Print all strings from the list, and draw a box with frames
      around it.

      The text is printed like by PrintStrings.

      The position of the text and box is determined by the current
      modelview matrix. The left-bottom box corner is at raster position 0, 0.
      The current raster position when this method is called doesn't matter.
      So your only way to move this box is to modify modelview matrix.

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
    procedure PrintStringsBox(const strs: array of string; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer); overload;
    procedure PrintStringsBox(strs: TStringList; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f;
      BoxPixelMargin: integer); overload;
    { @groupEnd }
  end;

  TGLBitmapFontClass = class of TGLBitmapFont_Abstract;

  { Abstract class for all OpenGL outline fonts. }
  TGLOutlineFont_Abstract = class
  protected
    FRowHeight: single;
  public
    { Draw text at position determined by the current OpenGL modelview matrix,
      and change modelview matrix to contain a transformation of the text end.
      This way you can immediately
      call another PrintAndMove again, to add something at the end.

      May require 1 free slot on the attributes stack and on the modelview stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it changes modelview matrix. }
    procedure Print(const s: string); virtual; abstract;

    { Draw text at position determined by the current OpenGL modelview matrix.
      In contrast to PrintAndMove, modelview matrix value is not changed.

      May require 1 free slot on the attributes stack and on the modelview stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    procedure PrintAndMove(const s: string); virtual; abstract;

    function TextWidth(const s: string): single; virtual; abstract;
    function TextHeight(const s: string): single; virtual; abstract;

    { Height of a row of text in this font.
      This may be calculated as simply @code(TextHeight('Wy')) for most
      normal fonts. }
    property RowHeight: single read FRowHeight;

    { How low the text may go below the baseline.
      By default this returns @code(TextHeight('y')-TextHeight('a')),
      which is suitable for normal fonts. }
    function Descend: single; virtual;
  end;

implementation

uses CastleUtils, CastleClassUtils, CastleStringUtils;

{ TGLBitmapFont_Abstract ------------------------------------------------------}

procedure TGLBitmapFont_Abstract.Print(const s: string);
var
  rasterPos4f: TVector4f;
begin
  glGetFloatv(GL_CURRENT_RASTER_POSITION, @rasterPos4f);
  PrintAndMove(s);
  glRasterPos4fv(@rasterPos4f);
end;

function TGLBitmapFont_Abstract.Descend: integer;
begin
  result := TextHeight('y')-TextHeight('a');
end;

procedure TGLBitmapFont_Abstract.BreakLines(const unbroken: string;
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

procedure TGLBitmapFont_Abstract.BreakLines(unbroken, broken: TStrings;
  maxLineWidth: integer);
var
  i, FirstToBreak: integer;
begin
  FirstToBreak := broken.count;
  for i := 0 to unbroken.count-1 do broken.Append(unbroken[i]);
  BreakLines(broken, maxLineWidth, FirstToBreak);
end;

procedure TGLBitmapFont_Abstract.BreakLines(broken: TStrings;
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

function TGLBitmapFont_Abstract.MaxTextWidth(slist: TStringList): integer;
var
  i, linew: integer;
begin
  result := 0;
  for i := 0 to slist.Count-1 do
  begin
    linew := TextWidth(slist[i]);
    if linew > result then result := linew;
  end;
end;

procedure TGLBitmapFont_Abstract.PrintStrings(const strs: array of string;
  BonusVerticalSpace: TGLint;
  const RasterX0: Integer = 0; const RasterY0: Integer = 0);
var
  I, H: integer;
begin
  H := High(strs);
  for i := 0 to High(strs) do
  begin
    glRasterPos2i(RasterX0, (H-i) * (RowHeight + BonusVerticalSpace) + RasterY0);
    PrintAndMove(strs[i]);
  end;
end;

procedure TGLBitmapFont_Abstract.PrintStrings(strs: TStrings;
  BonusVerticalSpace: TGLint;
  const RasterX0: Integer = 0; const RasterY0: Integer = 0);
var
  I, H: integer;
begin
  H := strs.Count-1;
  for i := 0 to H do
  begin
    glRasterPos2i(RasterX0, (H-i) * (RowHeight + BonusVerticalSpace) + RasterY0);
    PrintAndMove(strs[i]);
  end;
end;

function TGLBitmapFont_Abstract.PrintBrokenString(const s: string;
  MaxLineWidth, RasterX0, RasterY0: integer;
  RasterPositionsFirst: boolean; BonusVerticalSpace: Integer): Integer;
var
  broken: TStringList;
begin
  broken := TStringList.Create;
  try
    BreakLines(s, broken, MaxLineWidth);
    if RasterPositionsFirst then
      RasterY0 -= (broken.Count-1)*(RowHeight + BonusVerticalSpace);
    PrintStrings(broken, BonusVerticalSpace, RasterX0, RasterY0);
    result := broken.Count;
  finally broken.Free end;
end;

procedure TGLBitmapFont_Abstract.PrintStringsBox(
  strs: TStringList; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
begin
  DrawGLBorderedRectangle(0, 0, MaxTextWidth(Strs) + 2 * BoxPixelMargin,
    (RowHeight + BonusVerticalSpace) * Strs.Count + 2 * BoxPixelMargin + Descend,
    InsideCol, BorderCol);
  glColorv(TextCol);
  PrintStrings(strs, BonusVerticalSpace, BoxPixelMargin, BoxPixelMargin + Descend);
end;

procedure TGLBitmapFont_Abstract.PrintStringsBox(
  const strs: array of string; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f;
  BoxPixelMargin: integer);
var
  slist: TStringList;
begin
  slist := TStringList.Create;
  try
    AddStrArrayToStrings(strs, slist);
    PrintStringsBox(slist, BonusVerticalSpace,
      InsideCol, BorderCol, TextCol, BoxPixelMargin);
  finally slist.Free end;
end;

{ TGLOutlineFont_Abstract ------------------------------------------------------}

function TGLOutlineFont_Abstract.descend: single;
begin
  result := TextHeight('y')-TextHeight('a');
end;

end.
