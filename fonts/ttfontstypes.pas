{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi's fonts Pascal units".

  "Kambi's fonts Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's fonts Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's fonts Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Unit definiujacy typy do fontow truetype.)

  Definicje tych fontow moga zostac wygenerowane automatycznie przez moj
  program font2pascal.

  Krotko, na czym polegaja True-type fonty ?
  Font = ciag znakow.
  Znak = ciag polygonow.
  Polygon = Ciag Linii.
  Linia = Linia lamana / Krzywa beziera
          (ciag punktow, w kazdym razie, tylko roznie interpretowany)

  Zeby narysowac taki font trzeba rozwazyc wszystkie polygony.
  Punkty ktore znajduja sie w nieparzystej liczbie powinny byc wypelnione
  kolorem, pozostale punkty nie naleza do literki. W ten sposob np. literka
  "o" sklada sie z dwoch polygonow : zewnetrznego i wewnetrznego.
  Punkty na zewnatrz maja winding count = 0, punkty wewnarz wewnetrznego
  okregu maja winding count = 2 wiec tez nie naleza do literki. Tylko
  punkty pomiedzy dwoma koleczkami maja winding count = 1 i one
  wlasnie tworza literke.

  Z powyzszego wynika jak wyrenderowac literke pod OpenGL'em :
  uzyc tesselatora z GLU_TESS_WINDING_ODD i wrzucic mu wszystkie polygony.
  Krzywe Beziera mozna rozkladac na linie lamane z taka dokladnoscia
  jakiej chcesz (mozna nawet brac je za linie lamane - korzystajac
  z milych wlasnosci krzywych Beziera, wziecie krzywej lamanej z punktow
  kontrolnych krzywej tez jest jakims przyblizeniem krzywej).
  (patrz OpenGLTTFonts)
}

unit TTFontsTypes;

interface

type
  TPolygonKind = (pkNewPolygon, pkLines, pkBezier, pkPoint);

  TTTFCharItem = packed record
    case Kind:TPolygonKind of
     pkNewPolygon, pkLines, pkBezier : (Count:Cardinal);
     pkPoint : (x,y:Single);
  end;
  PTTFCharItem = ^TTTFCharItem;

  TTTFCharInfo = record
    MoveX, MoveY, Height:Single;
    { This tells you how many polygons are defined in Items,
      i.e. how many items with Kind=pkNewPolygon are there.
      Note: it CAN be equal to 0 (for characters such as space) }
    PolygonsCount:Cardinal;
    { This determines the real size of Items array of TTTFChar. }
    ItemsCount:Cardinal;
  end;

  { typ TTTFChar nie jest typem ktorego zmienne bedziemy tworzyc.
    Sluzy on tylko do zdefiniowania wskaznika PTTFChar.
    Faktyczna dlugosc tablicy Items moze byc odczytana z Info.ItemsCount
    (chociaz moglaby byc tez wywnioskowana uzywajac PolygonsCount i iterujac
    po tablicy Items; ale czesto posiadanie gotowego ItemsCount jest duzo
    wygodniejsze).

    It's packed because of the same reason as TBFNTChar. }
  TTTFChar = packed record
    Info:TTTFCharInfo;
    Items:packed array[0..MaxInt div SizeOf(TTTFCharItem) - 10] of TTTFCharItem;
  end;
  PTTFChar = ^TTTFChar;

  TTrueTypeFont = array[char] of PTTFChar;
  PTrueTypeFont = ^TTrueTypeFont;

(*
  Example:

  const
    CharX : record
      Info:TTTFCharInfo;
      Items:array[0..17] of TTTFCharItem;
    end =
    ( Info : (  MoveX:123; MoveY:456; Height:30;
                PolygonsCount:2;
                ItemsCount:18 );
      Items :
      ( (Kind:pkNewPolygon; Count:2),
        (Kind:pkLines; Count:3), (Kind:pkPoint; x:11; y:11), (Kind:pkPoint; x:22; y:22), (Kind:pkPoint; x:33; y:33),
        (Kind:prBezier; Count:3), (Kind:pkPoint; x:33; y:33), (Kind:pkPoint; x:44; y:44), (Kind:pkPoint; x:11; y:11),
        (Kind:pkNewPolygon; Count:2),
        (Kind:pkLines; Count:3), (Kind:pkPoint; x:111; y:111), (Kind:pkPoint; x:222; y:222), (Kind:pkPoint; x:333; y:333),
        (Kind:prBezier; Count:3), (Kind:pkPoint; x:333; y:333), (Kind:pkPoint; x:444; y:444), (Kind:pkPoint; x:111; y:111)
      )
    );

  Value of Kind field determine whether you should look at Count field
  or at X, Y fields.
  - Point is represented by TTTFCharItem with Kind = pkPoint and
    x,y set appropriately
  - Set of connected lines (polish: linia lamana) and Bezier curves are
    represented as
      (Kind:pkLines or pkBezier; Count: <n>),
      (Kind:pkPoint; x:...; y:...), { 1st point }
      ...
      (Kind:pkPoint; x:...; y:...), { <n>th point }
    (<n>+1 TTTFCharItem values)
    Each set of connected lines or Bezier curve must have <n> >= 2.
    I.e. if Kind in [pkLines, pkBezier], Count must be >= 2.
  - Polygon is
      (Kind:pkNewPolygon; Count: <m>),
      ... m "connected lines" and "bezier curves" follow
    <m> must be >= 1.
    I.e. if Kind = pkNewPolygon, Count must be >= 1.

  Note: as can be seen, Kind value of "pkPoint" is not actually needed:
  by iterating over Items sequentially you can always recognize whether
  you are standing on a point or not (using Count values of items
  with Kind=pkLines/pkBezier/pkNewPolygon, and you know that each
  char starts with Kind=pkNewPolygon and each polygon starts with pkLines
  or pkBezier). But it is often very useful to have Kinf=pkPoint.
  Because often you don't want to process Items array sequentially,
  paying close attention to all those Count fields.

  Jak widac w przykladzie, dla prostoty przetwarzania punkty sie powtarzaja.
  Pierwszy punkt calego polygonu na pewno jest ostatnim punktem calego polygonu.
  Pierwszy punkt kazdej linii na pewno jest ostatnim punktem poprzedniej linii.
*)

{ liczy Descend prosto, czyli jako (height y) - (height a).
  To bedzie dzialac dobrze dla normalnych fontow. Dla nienormalnych
  nalezaloby przegladnac informacje o wszystkich literkach i wybrac
  ta z najwiekszym Descend'em. }
function TTFontSimpleDescend(font:PTrueTypeFont):Single;

{ simple row height:= height('Mg') }
function TTFontSimpleRowHeight(font:PTrueTypeFont):Single;

function TTFontTextWidth(font:PTrueTypeFont; const s:string):Single;
function TTFontTextHeight(font:PTrueTypeFont; const s:string):Single;

implementation

function TTFontSimpleDescend(font:PTrueTypeFont):Single;
begin
 result:=font['y'].info.Height-font['a'].info.height;
end;

function TTFontSimpleRowHeight(font:PTrueTypeFont):Single;
begin
 result:=TTFontTextHeight(font,'Mg');
end;

function TTFontTextWidth(font:PTrueTypeFont; const s:string):Single;
var i:integer;
begin
 result:=0;
 for i:=1 to length(s) do result:=result+font[s[i]].info.moveX;
end;

function TTFontTextHeight(font:PTrueTypeFont; const s:string):Single;
var i:integer;
begin
 result:=0.0;
 for i:=1 to length(s) do
  if font[s[i]].info.Height>result then result:=font[s[i]].info.Height;
end;

end.
