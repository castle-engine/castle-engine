{
  Copyright 2002-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Types for TrueType fonts.)

  Concrete font types may be automatically loaded to such types,
  or even converted to Pascal units (so your fonts will be embedded
  directly into the program binary).

  Basic concepts:

  @unorderedList(
    @item(Font (TTrueTypeFont) is an array of characters.)

    @item(Character (TTTFChar) contains a basic character information
      (size and such) and a sequence of polygons to render this character.)

    @item(A polygon is a closed sequence of lines. It's defined inside
      TTTFCharItem array, with @code(Kind = pkNewPolygon) indicating
      start of a new polygon.)

    @item(A line is a sequence of points.
      These points should be rendered as either a sequence of straight
      line segments, or a Bezier (cubic) curve. Line is defined inside
      TTTFCharItem array, with @code(Kind = pkLines or pkBezier)
      indicating start of a new line.

      Note: unless you're going to see the font from a really close distance,
      a simple renderer may be OK with treating Bezier curves just
      like a sequence of straight line segments.)
  )

  Since characters may have holes inside, some polygons have to define
  the outline of the hole. Consider for example an "o" letter, that needs
  two polygons: one for the outer circle, one for the inner circle.
  For a given point on a 2D plane, it is part of the letter
  (e.g. should be drawn with font color) when it's inside an @italic(odd)
  number of polygons. If it's inside an @italic(even) number of polygons
  (0, or maybe 2, etc.) then it's not part of the letter (should be drawn
  with background color).

  The above definition makes it also natural to draw a font outline
  using OpenGL GLU tesselator. Simply use GLU_TESS_WINDING_ODD and
  pass all the polygons.
}

unit TTFontsTypes;

interface

type
  TPolygonKind = (pkNewPolygon, pkLines, pkBezier, pkPoint);

  TTTFCharItem = packed record
    case Kind: TPolygonKind of
      pkNewPolygon, pkLines, pkBezier : (Count: Cardinal);
      pkPoint : (x, y: Single);
  end;
  PTTFCharItem = ^TTTFCharItem;

  TTTFCharInfo = record
    MoveX, MoveY, Height: Single;

    { How many polygons are defined inside TTTFChar.Items.
      That is, how many items with Kind = pkNewPolygon are there.
      Note: it can be equal to 0 (for characters such as space). }
    PolygonsCount: Cardinal;

    { Number of Items inside a TTTFChar.Items. }
    ItemsCount: Cardinal;
  end;

  { Character information.
    It's packed because of the same reason as TBFNTChar. }
  TTTFChar = packed record
    Info: TTTFCharInfo;

    { Actual polygons, lines and points defining font outline.

      Although we define TTTFChar.Items as having
      a (practically) infinite number of items, we actually never declare
      variables of TTTFChar type, only of PTTFChar character.
      You have to always look at TTTFCharInfo.ItemsCount (Info.ItemsCount)
      to know actual number of items.

      You can also determine the end of items array
      by iterating over TTTFChar.Items, and knowing the Info.PolygonsCount.
      Although the ItemsCount gives this directly. }
    Items: packed array[0..MaxInt div SizeOf(TTTFCharItem) - 10] of TTTFCharItem;
  end;
  PTTFChar = ^TTTFChar;

  TTrueTypeFont = array[char] of PTTFChar;
  PTrueTypeFont = ^TTrueTypeFont;

(*
  Example:

  const
    CharX : record
      Info: TTTFCharInfo;
      Items: array[0..17] of TTTFCharItem;
    end =
    ( Info : (  MoveX:123; MoveY:456; Height:30;
                PolygonsCount:2;
                ItemsCount:18 );
      Items :
      ( (Kind: pkNewPolygon; Count:2),
        (Kind: pkLines; Count:3), (Kind: pkPoint; x:11; y:11), (Kind: pkPoint; x:22; y:22), (Kind: pkPoint; x:33; y:33),
        (Kind: prBezier; Count:3), (Kind: pkPoint; x:33; y:33), (Kind: pkPoint; x:44; y:44), (Kind: pkPoint; x:11; y:11),
        (Kind: pkNewPolygon; Count:2),
        (Kind: pkLines; Count:3), (Kind: pkPoint; x:111; y:111), (Kind: pkPoint; x:222; y:222), (Kind: pkPoint; x:333; y:333),
        (Kind: prBezier; Count:3), (Kind: pkPoint; x:333; y:333), (Kind: pkPoint; x:444; y:444), (Kind: pkPoint; x:111; y:111)
      )
    );

  Value of Kind field determine whether you should look at Count field
  or at X, Y fields.
  - Point is represented by TTTFCharItem with Kind = pkPoint and
    x, y set appropriately
  - Set of connected lines (polish: linia lamana) and Bezier curves are
    represented as
      (Kind: pkLines or pkBezier; Count: <n>),
      (Kind: pkPoint; x:...; y:...), { 1st point }
      ...
      (Kind: pkPoint; x:...; y:...), { <n>th point }
    (<n>+1 TTTFCharItem values)
    Each set of connected lines or Bezier curve must have <n> >= 2.
    I.e. if Kind in [pkLines, pkBezier], Count must be >= 2.
  - Polygon is
      (Kind: pkNewPolygon; Count: <m>),
      ... m "connected lines" and "bezier curves" follow
    <m> must be >= 1.
    I.e. if Kind = pkNewPolygon, Count must be >= 1.

  Note: as can be seen, Kind value of "pkPoint" is not actually needed:
  by iterating over Items sequentially you can always recognize whether
  you are standing on a point or not (using Count values of items
  with Kind = pkLines/pkBezier/pkNewPolygon, and you know that each
  char starts with Kind = pkNewPolygon and each polygon starts with pkLines
  or pkBezier). But it is often very useful to have Kinf = pkPoint.
  Because often you don't want to process Items array sequentially,
  paying close attention to all those Count fields.

  Jak widac w przykladzie, dla prostoty przetwarzania punkty sie powtarzaja.
  Pierwszy punkt calego polygonu na pewno jest ostatnim punktem calego polygonu.
  Pierwszy punkt kazdej linii na pewno jest ostatnim punktem poprzedniej linii.
*)

{ Calculate the height below the font baseline.
  This calculates the descend really simply ,as the height
  of letter "y" minus height of the letter "a". This will work Ok
  (and fast) for normal fonts.}
function TTFontSimpleDescend(font: PTrueTypeFont): Single;

{ Calculate row height. Simply, as the height of 'Mg' string. }
function TTFontSimpleRowHeight(font: PTrueTypeFont): Single;

function TTFontTextWidth(font: PTrueTypeFont; const s: string): Single;
function TTFontTextHeight(font: PTrueTypeFont; const s: string): Single;

implementation

function TTFontSimpleDescend(font: PTrueTypeFont): Single;
begin
 result := font^['y']^.info.Height - font^['a']^.info.height;
end;

function TTFontSimpleRowHeight(font: PTrueTypeFont): Single;
begin
 result := TTFontTextHeight(font, 'Mg');
end;

function TTFontTextWidth(font: PTrueTypeFont; const s: string): Single;
var i: integer;
begin
 result := 0;
 for i := 1 to length(s) do result := result + font^[s[i]]^.info.moveX;
end;

function TTFontTextHeight(font: PTrueTypeFont; const s: string): Single;
var i: integer;
begin
 result := 0.0;
 for i := 1 to length(s) do
   if font^[s[i]]^.info.Height > result then
     result := font^[s[i]]^.info.Height;
end;

end.
