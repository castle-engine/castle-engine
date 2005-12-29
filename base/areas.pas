{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Modul tworzacy takiego managera obszarow.
  Idea jest taka ze tworzysz sobie wiele obszarow o roznym ksztalcie
  i na roznej pozycji i potem dodajesz te obszary kiedy i jak chcesz
  do areas managera czyli obiektu AreasMan.)

  Sens tego taki ze potem mozesz przegladac i przeszukac
  te area w wygodny sposob. Ten modul zostal stworzony
  na potrzeby szklane_setup kiedy to chcialem napisac programik
  przy uzyciu GLWindow ktory oferowalby dosc przyjemny sposob
  komunikacji z uzytkownikiem - a wiec np. chcialem aby przesuwanie
  myszki nad napisami / przyciskami czynilo je aktywnymi. Do tego
  potrzebowalem wlasnie takiego moduliku ktory zajmowalby sie
  zarzadzaniem obszarow na ekranie zajmowanych przez te napisy
  i przyciski.

  Ten modul nie jest wiec zbyt ciekawy. Ale mozliwe ze kiedys
  przerobie go na managera kontrolek rysowanych pod OpenGLem -
  - a to juz bedzie calkiem ciekawe.

  @noAutoLinkHere
}

unit Areas;

interface

{$define read_interface}

uses KambiUtils, SysUtils;

type
  TArea = record
    left, bottom, width, height : real;
    userdata : integer;
  end;
  PArea = ^TArea;

  TDynArrayItem_1 = TArea;
  PDynArrayItem_1 = PArea;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynAreaArray = TDynArray_1;

  TAreasManager = class
  private
    FAreas: TDynAreaArray;
    function GetArea(nr: integer): PArea;
  public
    property Areas[nr: integer]:PArea read GetArea;
    function AreasCount: integer;

    { dodaje ar do listy zarzadzanych Areas. Zwraca indeks jaki ar dostalo
      w tablicy Areas. }
    function NewArea(const ar: TArea): integer;

    { FindArea zwraca indeks piewszej area (sprawdzajac od KONCA tablicy Areas)
      ktora zawiere punkt o wspolrzednych x, y. Sprawdzanie wykonywane jest od
      konca w ten sposob traktujac obszary bardziej na koncu jako te bardziej
      na wierzchu.
      Zwraca -1 jesli nie znajdzie. }
    function FindArea(x, y: real): integer;

    { Delete all Areas }
    procedure Clear;

    constructor Create;
    destructor Destroy; override;
  end;

{ @noAutoLinkHere }
function Area(left, bottom, width, height: real; userdata: integer): TArea;

function PointInArea(x, y: real; const ar: TArea): boolean;

var
  AreasMan: TAreasManager;

{$undef read_interface}

implementation

{$define read_implementation}
{$I DynArray_1.inc}

{ TAreasManager ------------------------------------------------------------ }

constructor TAreasManager.Create;
begin
 inherited;
 FAreas := TDynAreaArray.Create;
end;

destructor TAreasManager.Destroy;
begin
 FreeAndNil(FAreas);
 inherited;
end;

function TAreasManager.GetArea(nr: integer): PArea;
begin result := @FAreas.Items[nr] end;

function TAreasManager.AreasCount: integer;
begin result := FAreas.Length end;

function TAreasManager.NewArea(const ar: TArea): integer;
begin
 FAreas.IncLength;
 FAreas.Items[FAreas.High] := ar;
 result := FAreas.High;
end;

function TAreasManager.FindArea(x, y: real): integer;
begin
 for result := FAreas.High downto 0 do
  if PointInArea(x, y,FAreas.Items[result]) then exit;
 result := -1;
end;

procedure TAreasManager.Clear;
begin
 FAreas.SetLength(0);
end;

{ global funcs ----------------------------------------------  }

function Area(left, bottom, width, height: real; userdata: integer): TArea;
begin
 result.left := left;
 result.bottom := bottom;
 result.width := width;
 result.height := height;
 result.userdata := userdata;
end;

function PointInArea(x, y: real; const ar: TArea): boolean;
begin
 with ar do
  result:=(x >= left) and (x <= left+width) and
          (y >= bottom) and (y <= bottom+height);
end;

{ init / fini -------------------------------------------------- }

initialization
 AreasMan := TAreasManager.Create;
finalization
 FreeAndNil(AreasMan);
end.
