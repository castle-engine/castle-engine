{
  Copyright 2001-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(Unit tworzacy fonty oparte na TGLBitmap_Abstract ktore uzywaja struktur
  zdefinowanych w BmpFontsTypes. Tym samym tworzymy sobie bitmap fonty na
  podstawie informacji zawartej w strukturach programu. Mamy wiec mozliwosc
  definiowania fontow w sposob niezalezny od systemu operacyjnego - majac
  zdefiniowane wlasne struktury nie potrzebujemy juz zadnych fontow
  systemu operacyjnego zeby stworzyc sobie fonty pod OpenGL'em.)

  Unit blizniaczy do OpenGLTTFonts ktory robi to samo dla outline fontow
  opartych na strukturach w TTFontsTypes .
}

unit OpenGLBmpFonts;

interface

uses BmpFontsTypes, OpenGLFonts, GL, GLU, KambiGLUtils;

type
  TGLBitmapFont = class(TGLBitmapFont_Abstract)
  private
    base: TGLuint;
    bmpfont: PBmpFont;
  public
    constructor Create(BitmapFont: PBmpFont);
     { uwaga : podobnie jak w TGLOutlineTTFont, konstruktor TGLBitmapFont
       pobiera jako parametr wskaznik na PBmpFont i zawartosc tego wskaznika
       NIE JEST kopiowana - zapamietywany jest tylko sam wskaznik. Dlatego,
       aby wszystko bylo ok, od momentu przekazania wskaznika na strukture
       TBmpFont powyzszemu konstruktorowi powinienes zawartosc TBmpFont i
       wszystkie strukturki (pochodne TBFNTZnak) traktowac jako read-only ! }
    destructor Destroy; override;

    procedure printAndMove(const s: string); override;
    function TextWidth(const s: string): integer; override;
    function TextHeight(const s: string): integer; override;
  end;

implementation

uses KambiUtils;

const BmpTableCount = Ord(High(char)) - Ord(Low(char)) +1;

constructor TGLBitmapFont.Create(BitmapFont: PBmpFont);
var i: Cardinal;
    Znak: PBFNTChar;
    Saved_Unpack_Alignment: TGLint;
begin
 inherited Create;
 base := glGenListsCheck(BmpTableCount, 'TGLBitmapFont.Create');
 Self.BmpFont := BitmapFont;
 Saved_Unpack_Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);

 for i := 0 to 255 do
 begin
  Znak := BmpFont^[Chr(i)];
  glPixelStorei(GL_UNPACK_ALIGNMENT, Znak^.Info.Alignment);
  glNewList(i+base, GL_COMPILE);
  glBitmap(Znak^.Info.Width, Znak^.Info.Height,
           Znak^.Info.XOrig, Znak^.Info.YOrig,
           Znak^.Info.XMove, Znak^.Info.YMove,
           @Znak^.Data);
  glEndList;
 end;
 glPixelStorei(GL_UNPACK_ALIGNMENT, Saved_Unpack_Alignment);

 fRowHeight := TextHeight('Wy') + 2;
 { RowHeight zwiekszylem o +2 zeby byl odstep miedzy liniami. }
end;

destructor TGLBitmapFont.Destroy;
begin
 glDeleteLists(base, BmpTableCount);
 inherited;
end;

procedure TGLBitmapFont.printAndMove(const s: string);
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
  Result := Result + Round(BmpFont^[s[i]]^.Info.XMove);
end;

function TGLBitmapFont.TextHeight(const s: string): integer;
var i: integer;
    minY, maxY: integer;
begin
 minY := 0;
 maxY := 0;
 for i := 1 to length(s) do
 begin
  minY := min(minY, -Round(BmpFont^[s[i]]^.Info.YOrig));
  maxY := max(maxY, BmpFont^[s[i]]^.Info.Height -
              Round(BmpFont^[s[i]]^.Info.YOrig));
 end;
 result := maxY-minY;
end;

end.
