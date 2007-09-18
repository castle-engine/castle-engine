{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
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
}

unit Areas;

interface

{$define read_interface}

uses KambiUtils, SysUtils;

type
  TArea = record
    X0, Y0, Width, Height: Single;
    UserData: Pointer;
  end;
  PArea = ^TArea;

  TDynArrayItem_1 = TArea;
  PDynArrayItem_1 = PArea;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}

  TDynAreaArray = class(TDynArray_1)
  public

    { FindArea zwraca indeks piewszej area (sprawdzajac od KONCA tablicy Areas)
      ktora zawiere punkt o wspolrzednych x, y. Sprawdzanie wykonywane jest od
      konca w ten sposob traktujac obszary bardziej na koncu jako te bardziej
      na wierzchu.

      Zwraca -1 jesli nie znajdzie. }
    function FindArea(const X, Y: Single): integer;
  end;

function Area(const X0, Y0, Width, Height: Single;
  const UserData: Pointer = nil): TArea;

{ TODO: unused anywhere, so untested. }
function AreasSum(const Area1, Area2: TArea): TArea;

function PointInArea(const X, Y: Single; const Area: TArea): boolean;

var
  DefaultAreas: TDynAreaArray;

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

{ TDynAreaArray -------------------------------------------------------------- }

function TDynAreaArray.FindArea(const X, Y: Single): integer;
begin
  for Result := High downto 0 do
    if PointInArea(X, Y, Items[Result]) then
      Exit;
  Result := -1;
end;

{ global funcs --------------------------------------------------------------- }

function Area(const X0, Y0, Width, Height: Single;
  const UserData: Pointer): TArea;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.Width := Width;
  Result.Height := Height;
  Result.UserData := UserData;
end;

function AreasSum(const Area1, Area2: TArea): TArea;
begin
  Result.X0 := Min(Area1.X0, Area2.X0);
  Result.Y0 := Min(Area1.Y0, Area2.Y0);
  Result.Width := Max(
    Area1.X0 + Area1.Width - Result.X0,
    Area2.X0 + Area2.Width - Result.X0);
  Result.Height := Max(
    Area1.Y0 + Area1.Height - Result.Y0,
    Area2.Y0 + Area2.Height - Result.Y0);
end;

function PointInArea(const X, Y: Single; const Area: TArea): boolean;
begin
  Result := (X >= Area.X0) and (X <= Area.X0 + Area.Width) and
            (Y >= Area.Y0) and (Y <= Area.Y0 + Area.Height);
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  DefaultAreas := TDynAreaArray.Create;
finalization
  FreeAndNil(DefaultAreas);
end.
