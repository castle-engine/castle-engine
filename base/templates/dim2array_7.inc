{
  Copyright 2002-2004 Michalis Kamburelis.

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

{ TDim2XxxArray ------------------------------------------------------------ }

{ makra (FPC) lub typy (Delphi lub FPC) do zdefiniowania :
  TDim2Array_7 (zostanie zdefiniowany)
  TDim2ArrayItem_7 (ma byc zdefiniowany - to jest typ elementow tablicy)
  PDim2ArrayItem_7 (ma byc zdefiniowany jako ^TDim2ArrayItem_7)

  DIM2ARRAY_7_IS_INIT_FINI_TYPE : zdefiniowany lub nie, zdefiniowany oznacza
   ze typ TDim2ArrayItem_7 wymaga wywolania Initialize po zaalokowaniu dla
   niego pamieci przez GetMem i Finalize przed zwolnieniem jego pamieci
   przez FreeMem. Te wywolania Initialize / Finalize beda wykonane
   automatycznie przy przydzielaniu / zwalnianiu pamiecu w TDim2Array_7.

  DIM2ARRAY_7_IS_STRUCT : zdefiniowany - wlasciwosc Pointers[x,y] zwracajaca wskaznik
                 na element [x,y] jest domyslna. Wlasciwosci Values nie ma.
              niezdef - wlasciwosc Values[x,y] pozwalajaca normalnie
                 pobierac / ustawiac elementy jest domyslna.

  DIM2ARRAY_7_RANGECHECKS : zdefniuj ten symbol aby wszystkie odwolania
   do Pointers i Values byly sprawdzane na poprawnosc indeksow.
   Innymi slowy, zdefiniowanie tego symbolu to jak zrobienie $R+;
   jednak nie uzywam tutaj testu $ifopt R bo sprawdzanie zakresow
   tych tablic potrafi czesto NAPRAWDE zwolnic dzialanie programu
   wiec mozesz chciec miec R+ a mimo to DIM2ARRAY_7_RANGECHECKS
   wylaczone.

  TDim2Array_7.Create(x,y) zwraca obiekt zarzadzajacy 2 - wymiarowa tablica
   [0..x-1, 0..y-1]. Rozmiary tablicy nie moga byc zmieniane (nie ma czegos
   takiego jak SetLength,
   sorry - trzebaby to sensownie zdefiniowac dla tablic 2-dim i wtedy zrobic)
   ale rozmiar podajesz wywolujac konstruktor (wiec mozesz go najpierw wyliczyc
   z jakichs zmiennych itp.).
}

{$ifdef read_interface}

  TDim2Array_7 = class
  private
    P:PDim2ArrayItem_7;
    dim1,dim2:integer;
    function GetPointers(Adim1, Adim2:integer):PDim2ArrayItem_7;
    function GetHigh1:integer;
    function GetHigh2:integer;
    {$ifndef DIM2ARRAY_7_IS_STRUCT}
    function GetValues(Adim1, Adim2:integer):TDim2ArrayItem_7;
    procedure SetValues(Adim1, Adim2:integer; const Value:TDim2ArrayItem_7);
    {$endif}
  public
    { wskaznik na tablice packed array [0..dim1*dim2-1] of TDim2ArrayItem_7,
      wewnetrzna tablica reprezentujaca 2-wymiarowa tablice.
      Tablica jest ulozona w pamieci tak jak array[0..dim1-1, 0..dim2-1]
      a wiec pierwszy index powoduje zmiany wskaznika o
      dim2*SizeOf(TDim2ArrayItem_7) (gdy pierwszy indeks zmienia sie o 1)
      natomiast drugi indeks zmienia wskaznik tylko o SizeOf(TDim2ArrayItem_7)
      gdy sam zmienia sie o 1.
    }
    property ArrayPtr:PDim2ArrayItem_7 read P;

    property Length1:integer read dim1;
    property Length2:integer read dim2;
    property High1:integer read GetHigh1; { = Length1 - 1 }
    property High2:integer read GetHigh2; { = Length2 - 1 }

    constructor Create(Adim1, Adim2:integer);
    destructor Destroy; override;
    property Pointers[Adim1, Adim2:integer]:PDim2ArrayItem_7 read GetPointers; {$ifdef DIM2ARRAY_7_IS_STRUCT} default; {$endif}
    {$ifndef DIM2ARRAY_7_IS_STRUCT}
    property Values[Adim1, Adim2:integer]:TDim2ArrayItem_7 read GetValues write SetValues; default;
    {$endif}

    { FillMemByte wykonuje
      FillChar(ArrayPtr^, SizeOf(TDim2ArrayItem_7)*Length1*Length2, b).
      Innymi slowy, cala tablica zostanie wypelniona bajtami b.
      Uwazaj - czy otrzymane wartosci w tablicy beda sensowne zalezy tylko od ciebie ! }
    procedure FillMemByte(b:byte);
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TDim2Array_7.GetHigh1:integer; begin result:=Length1-1 end;
function TDim2Array_7.GetHigh2:integer; begin result:=Length2-1 end;

function TDim2Array_7.GetPointers(Adim1, Adim2:integer):PDim2ArrayItem_7;
begin
 {$ifdef DIM2ARRAY_7_RANGECHECKS}
 if (Adim1<0) or (Adim1>=dim1) or
    (Adim2<0) or (Adim2>=dim2) then
  ERangeError.CreateFmt(
   'TDim2Array_7 range error : requested item %d, %d, array dims : [%d, %d]',
   [Adim1, Adim2, dim1, dim2]);
 {$endif}
 result:=PDim2ArrayItem_7( PointerAdd(p,(Adim1*dim2+Adim2) *SizeOf(TDim2ArrayItem_7)) );
end;

{$ifndef DIM2ARRAY_7_IS_STRUCT}
function TDim2Array_7.GetValues(Adim1, Adim2:integer):TDim2ArrayItem_7;
begin
 result:=Pointers[Adim1, Adim2]^;
end;

procedure TDim2Array_7.SetValues(Adim1, Adim2:integer; const Value:TDim2ArrayItem_7);
begin
 Pointers[Adim1, Adim2]^:=Value;
end;
{$endif}

constructor TDim2Array_7.Create(Adim1, Adim2:integer);
{$ifdef DIM2ARRAY_7_IS_INIT_FINI_TYPE} var i:integer; {$endif}
begin
 inherited Create;
 dim1:=Adim1;
 dim2:=Adim2;
 p:=GetMem(dim1*dim2*SizeOf(TDim2ArrayItem_7));
 {$ifdef DIM2ARRAY_7_IS_INIT_FINI_TYPE}
 for i:=0 to dim1*dim2-1 do
  Initialize(PDim2ArrayItem_7( PointerAdd(p,i*SizeOf(TDim2ArrayItem_7)) )^);
 {$endif}
end;

destructor TDim2Array_7.Destroy;
 {$ifdef DIM2ARRAY_7_IS_INIT_FINI_TYPE}
var i:integer;
begin
 if p<>nil then
  for i:=0 to dim1*dim2-1 do
   Finalize(PDim2ArrayItem_7( PointerAdd(p,i*SizeOf(TDim2ArrayItem_7)) )^);
 {$else}
begin
 {$endif}
 if p<>nil then FreeMem(p);
 inherited;
end;

procedure TDim2Array_7.FillMemByte(b:byte);
begin
 FillChar(P^, SizeOf(TDim2ArrayItem_7)*Length1*Length2, b);
end;

{$endif read_implementation}