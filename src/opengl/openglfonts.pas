{
  Copyright 2001-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL fonts, basic abstract classes (TGLBitmapFont_Abstract,
  TGLOutlineFont_Abstract). }

unit OpenGLFonts;

interface

uses Classes, GL, GLU, SysUtils, KambiGLUtils;

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

    { Descend : jak gleboko moze spasc charakter ponizej wysokosci 0 ?
      Domyslnie brane jest TextHeight('y')-TextHeight('a'), mozesz pokryc ta
      metode w swojej klasie aby dostarczyc jakiejs dokladniejszej / inaczej
      liczonej informacji. }
    function Descend: integer; virtual;

    { procedury BreakLines lamia string (w ktorym moga byc juz zlamane miejsca
      nl, zostana one poprawnie zauwazone) lub TStrings w taki sposob
      aby zadna linia nie miala TextWidth wiekszego niz maxLineWidth.
      Zawartosc unbroken nie jest w zaden sposob modyfikowana.
      Dotychczasowa zawartosc broken zostaje zachowana, nowe stringi zostana
      dopisane na koniec broken. firstToBreak mowi od ktorej linii zaczac
      lamanie.

      Stara sie polamac PRAWIE za wszelka cene. To znaczy np. stara sie lamac
      na bialych znakach, ale jezeli w stringu jest za dlugi wyraz bez bialych
      znakow to tez go zlamie. Jedyna sytuacja w ktorej wynikowy MaxLineWidth()
      dla tak polamanej listy nie bedzie spelniac ograniczenia to gdy
      jeden ze znakow uzytych w tekscie jest SAM wiekszy od narzuconego
      maxLineWidth. Wtedy po prostu nie da sie zlamac stringa zadowalajaco -
      ponizsze procedury zezwola wtedy na to aby ten jeden znak byl sam
      w linii i naruszal ograniczenie maxLineWidth. }
    procedure BreakLines(const unbroken: string; broken: TStrings; maxLineWidth: integer); overload;
    procedure BreakLines(unbroken, broken: TStrings; maxLineWidth: integer); overload;
    procedure BreakLines(broken: TStrings; maxLineWidth: integer; firstToBreak: integer); overload;
    { MaxTextWidth - najwieksze TextWidth(slist[i]) gdzie i przebiega 0..slist.count-1 }
    function MaxTextWidth(slist: TStringList): integer;

    { Print strings from 0 to strs.count-1 using this font.
      glRasterPosi(RasterX0, RasterY0) is the position of the last string,
      each previous string will be RowHeight + BonusVerticalSpace higher,
      and so on (BonusVerticalSpace CAN be <0, only
      (RowHeight + BonusVerticalSpace) must be > 0).

      Wersje 2-arg uznaja RasterX0 = RasterY0 = 0.

      glRasterPos value will be ignored and then modified by this method.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    procedure PrintStrings(strs: TStrings; BonusVerticalSpace: TGLint); overload;
    procedure PrintStrings(const strs: array of string; BonusVerticalSpace: TGLint); overload;
    procedure PrintStrings(strs: TStrings;
      BonusVerticalSpace: TGLint; RasterX0, RasterY0: integer); overload;
    procedure PrintStrings(const strs: array of string;
      BonusVerticalSpace: TGLint; RasterX0, RasterY0: integer); overload;

    { Lamie string s (uzywajac BreakLines, wiec oryginalne #13 i #10 w stringu
      sa uwzglednione i string jest dzielony tak zeby zadna jego linia nie miala
      wiecej niz MaxLineWidth).

      Potem wypisuje string. Jezeli RasterPositionsFirst to RasterX0, RasterY0
      okresla pozycje pierwszego stringu na liscie, wpp. okresla pozycje
      ostatniego. Kolejne linie sa wypisywane kazda o
      RowHeight + BonusVerticalSpace nizej niz poprzednia (tak jak
      w PrintStrings, wymagane jest tylko zeby
      (RowHeight + BonusVerticalSpace) > 0).

      Funkcja zwraca liczbe linii jakie uzyskano po zlamaniu stringu s.
      W ten sposob mozesz np. obliczyc pozycje jaka miala na ekranie
      pierwsza / ostatnia ze zlamanych linii (to ktora z tych wartosci bylaby
      dla ciebie nieznana zalezy od tego ktora z tych wartosci podales,
      czyli od RasterPositionsFirst).

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    function PrintBrokenString(const s: string;
      MaxLineWidth, RasterX0, RasterY0: Integer;
      RasterPositionsFirst: boolean; BonusVerticalSpace: Integer): Integer;

    { rysuje strs kolorem StringCol i otacza je prostokatem ktory w srodku
        ma kolor InsideCol i stipple Stipple (lub nie ma stipple jesli
        Stipple = nil) a krawedz ma kolorem BorderCol.
        BoxPixelMargin to min odleglosc miedzy tekstem a krawedzia prostokata
        w pixelach. Robi to wszystko poprawnie
        zaczynajac od pozycji glRasterPos(0, 0), wzgledem aktualnej matrix
        (aktualne matrix to musi byc naturalnie MODELVIEW) i przy zalozeniu
        ze 1 jednostka OpenGL'a xowa to XPixelsRes piksli, a ykowa to
        YPixelsRes piksli.
      BonusVerticalSpace - jak w PrintStrings.
      RasterPos and curent color will be ignored and then modified by this proc,
        no other state is affected.
        Current matrix value is used and not modified.
      Requires one attrib stack and one matrix stack place.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix. }
    procedure PrintStringsBorderedRect(const strs: array of string; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f; Stipple: PPolygonStipple;
      BoxPixelMargin: integer; const XPixelsRes, YPixelsRes: TGLfloat); overload;
    procedure PrintStringsBorderedRect(strs: TStringList; BonusVerticalSpace: TGLint;
      const InsideCol, BorderCol, TextCol: TVector4f; Stipple: PPolygonStipple;
      BoxPixelMargin: integer; const XPixelsRes, YPixelsRes: TGLfloat); overload;
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
    property RowHeight: single read FRowHeight;
    function Descend: single; virtual;
  end;

implementation

uses KambiUtils, KambiClassUtils, KambiStringUtils;

{ TGLBitmapFont_Abstract ------------------------------------------------------}

procedure TGLBitmapFont_Abstract.Print(const s: string);
var rasterPos4f: TVector4f;
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
var unbrokenlist: TStringList;
begin
 unbrokenlist := TStringList.Create;
 try
  Strings_SetText(unbrokenlist, unbroken);
  BreakLines(unbrokenlist, broken, maxLineWidth);
 finally unbrokenlist.Free end;
end;

procedure TGLBitmapFont_Abstract.BreakLines(unbroken, broken: TStrings;
  maxLineWidth: integer);
var i, firstToBreak: integer;
begin
 firstToBreak := broken.count;
 for i := 0 to unbroken.count-1 do broken.Append(unbroken[i]);
 BreakLines(broken, maxLineWidth, firstToBreak);
end;

procedure TGLBitmapFont_Abstract.BreakLines(broken: TStrings;
  maxLineWidth: integer; firstToBreak: integer);
var i, j: integer;
    linew: integer;
    p: integer;
    break1, break2: string;
begin

 { ponizej lamiemy stringi unbroken.
   Lamanie to nie jest takie proste bo my nie mamy czegos takiego jak
   MaxCol - ilosc znakow w linii, bo kazdy znak moze miec inna szerokosc -
   font nie musi byc monospaced ! Gdyby byl - no coz, to robota bylaby prosta :
    broken.text := WrapText(broken.text, maxLineWidth div font.TextWidth('w'));
    (no, zakladajac ze firstToBreak = 0)
   i juz. A tak - musimy po kolei badac kazdy string szukajac w nim literki
   ktora sprawia ze nie miesci sie w maxLineWidth i wtedy obcinac.
 }

 i := firstToBreak;
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
var i, linew: integer;
begin
 result := 0;
 for i := 0 to slist.Count-1 do
 begin
  linew := TextWidth(slist[i]);
  if linew > result then result := linew;
 end;
end;

procedure TGLBitmapFont_Abstract.PrintStrings(const strs: array of string;
  BonusVerticalSpace: TGLint; RasterX0, RasterY0: integer);
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
  BonusVerticalSpace: TGLint; RasterX0, RasterY0: integer);
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

procedure TGLBitmapFont_Abstract.PrintStrings(const strs: array of string;
  BonusVerticalSpace: TGLint);
begin PrintStrings(strs, BonusVerticalSpace, 0, 0) end;

procedure TGLBitmapFont_Abstract.PrintStrings(strs: TStrings;
  BonusVerticalSpace: TGLint);
begin PrintStrings(strs, BonusVerticalSpace, 0, 0) end;

function TGLBitmapFont_Abstract.PrintBrokenString(const s: string;
  MaxLineWidth, RasterX0, RasterY0: integer;
  RasterPositionsFirst: boolean; BonusVerticalSpace: Integer): Integer;
var broken: TStringList;
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

procedure TGLBitmapFont_Abstract.PrintStringsBorderedRect(
  strs: TStringList; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f; Stipple: PPolygonStipple;
  BoxPixelMargin: integer; const XPixelsRes, YPixelsRes: TGLfloat);
var
  Y2: Integer;
begin
  { You can't calculate full Y2 / YPixelsRes in one expression,
    FPC 2.2.0 under x86_64 will calculate something random then.
    Submittted as
    [http://www.freepascal.org/mantis/view.php?id=9893] }
  Y2 := (RowHeight + BonusVerticalSpace) * Strs.Count +
    2 * BoxPixelMargin + Descend;
  DrawGLBorderedRectangle(0, 0,
    (MaxTextWidth(Strs) + 2 * BoxPixelMargin) / XPixelsRes,
    Y2 / YPixelsRes,
    InsideCol, BorderCol, Stipple);
  glColorv(TextCol);
  PrintStrings(strs, BonusVerticalSpace, BoxPixelMargin, BoxPixelMargin + Descend);
end;

procedure TGLBitmapFont_Abstract.PrintStringsBorderedRect(
  const strs: array of string; BonusVerticalSpace: TGLint;
  const InsideCol, BorderCol, TextCol: TVector4f; Stipple: PPolygonStipple;
  BoxPixelMargin: integer; const XPixelsRes, YPixelsRes: TGLfloat);
var slist: TStringList;
begin
 slist := TStringList.Create;
 try
  AddStrArrayToStrings(strs, slist);
  PrintStringsBorderedRect(slist, BonusVerticalSpace,
    InsideCol, BorderCol, TextCol, Stipple,
    BoxPixelMargin, XPixelsRes, YPixelsRes);
 finally slist.Free end;
end;

{ TGLOutlineFont_Abstract ------------------------------------------------------}

function TGLOutlineFont_Abstract.descend: single;
begin
 result := TextHeight('y')-TextHeight('a');
end;

end.
