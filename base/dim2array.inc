{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ TDim2XxxArray ------------------------------------------------------------ }

{ makra (FPC) lub typy (Delphi lub FPC) do zdefiniowania :
  TDim2Array (zostanie zdefiniowany)
  TDim2ArrayItem (ma byc zdefiniowany - to jest typ elementow tablicy)
  PDim2ArrayItem (ma byc zdefiniowany jako ^TDim2ArrayItem)

  DIM2ARRAY_IS_INIT_FINI_TYPE : zdefiniowany lub nie, zdefiniowany oznacza
   ze typ TDim2ArrayItem wymaga wywolania Initialize po zaalokowaniu dla
   niego pamieci przez GetMem i Finalize przed zwolnieniem jego pamieci
   przez FreeMem. Te wywolania Initialize / Finalize beda wykonane
   automatycznie przy przydzielaniu / zwalnianiu pamiecu w TDim2Array.

  DIM2ARRAY_IS_STRUCT : zdefiniowany - wlasciwosc Pointers[x,y] zwracajaca wskaznik
                 na element [x,y] jest domyslna. Wlasciwosci Values nie ma.
              niezdef - wlasciwosc Values[x,y] pozwalajaca normalnie
                 pobierac / ustawiac elementy jest domyslna.

  DIM2ARRAY_RANGECHECKS : zdefniuj ten symbol aby wszystkie odwolania
   do Pointers i Values byly sprawdzane na poprawnosc indeksow.
   Innymi slowy, zdefiniowanie tego symbolu to jak zrobienie $R+;
   jednak nie uzywam tutaj testu $ifopt R bo sprawdzanie zakresow
   tych tablic potrafi czesto NAPRAWDE zwolnic dzialanie programu
   wiec mozesz chciec miec R+ a mimo to DIM2ARRAY_RANGECHECKS
   wylaczone.

  TDim2Array.Create(x,y) zwraca obiekt zarzadzajacy 2 - wymiarowa tablica
  [0..x-1, 0..y-1]. Rozmiary tablicy nie moga byc zmieniane (nie ma czegos
  takiego jak SetLength,
   
  TODO: trzebaby to sensownie zdefiniowac dla tablic 2-dim i wtedy zrobic)
  ale rozmiar podajesz wywolujac konstruktor (wiec mozesz go najpierw wyliczyc
  z jakichs zmiennych itp.).
}

{$ifdef read_interface}

  TDim2Array = class
  private
    P:PDim2ArrayItem;
    dim1,dim2:integer;
    function GetPointers(Adim1, Adim2:integer):PDim2ArrayItem;
    function GetHigh1:integer;
    function GetHigh2:integer;
    {$ifndef DIM2ARRAY_IS_STRUCT}
    function GetValues(Adim1, Adim2:integer):TDim2ArrayItem;
    procedure SetValues(Adim1, Adim2:integer; const Value:TDim2ArrayItem);
    {$endif}
  public
    { wskaznik na tablice packed array [0..dim1*dim2-1] of TDim2ArrayItem,
      wewnetrzna tablica reprezentujaca 2-wymiarowa tablice.
      Tablica jest ulozona w pamieci tak jak array[0..dim1-1, 0..dim2-1]
      a wiec pierwszy index powoduje zmiany wskaznika o
      dim2*SizeOf(TDim2ArrayItem) (gdy pierwszy indeks zmienia sie o 1)
      natomiast drugi indeks zmienia wskaznik tylko o SizeOf(TDim2ArrayItem)
      gdy sam zmienia sie o 1.
    }
    property ArrayPtr:PDim2ArrayItem read P;

    property Length1:integer read dim1;
    property Length2:integer read dim2;
    property High1:integer read GetHigh1; { = Length1 - 1 }
    property High2:integer read GetHigh2; { = Length2 - 1 }

    constructor Create(Adim1, Adim2:integer);
    destructor Destroy; override;
    property Pointers[Adim1, Adim2:integer]:PDim2ArrayItem read GetPointers; {$ifdef DIM2ARRAY_IS_STRUCT} default; {$endif}
    {$ifndef DIM2ARRAY_IS_STRUCT}
    property Values[Adim1, Adim2:integer]:TDim2ArrayItem read GetValues write SetValues; default;
    {$endif}

    { FillMemByte wykonuje
      FillChar(ArrayPtr^, SizeOf(TDim2ArrayItem)*Length1*Length2, b).
      Innymi slowy, cala tablica zostanie wypelniona bajtami b.
      Uwazaj - czy otrzymane wartosci w tablicy beda sensowne zalezy tylko od ciebie ! }
    procedure FillMemByte(b:byte);
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TDim2Array.GetHigh1:integer; begin result:=Length1-1 end;
function TDim2Array.GetHigh2:integer; begin result:=Length2-1 end;

function TDim2Array.GetPointers(Adim1, Adim2:integer):PDim2ArrayItem;
begin
 {$ifdef DIM2ARRAY_RANGECHECKS}
 if (Adim1<0) or (Adim1>=dim1) or
    (Adim2<0) or (Adim2>=dim2) then
  ERangeError.CreateFmt(
   'TDim2Array range error : requested item %d, %d, array dims : [%d, %d]',
   [Adim1, Adim2, dim1, dim2]);
 {$endif}
 result:=PDim2ArrayItem( PointerAdd(p,(Adim1*dim2+Adim2) *SizeOf(TDim2ArrayItem)) );
end;

{$ifndef DIM2ARRAY_IS_STRUCT}
function TDim2Array.GetValues(Adim1, Adim2:integer):TDim2ArrayItem;
begin
 result:=Pointers[Adim1, Adim2]^;
end;

procedure TDim2Array.SetValues(Adim1, Adim2:integer; const Value:TDim2ArrayItem);
begin
 Pointers[Adim1, Adim2]^:=Value;
end;
{$endif}

constructor TDim2Array.Create(Adim1, Adim2:integer);
{$ifdef DIM2ARRAY_IS_INIT_FINI_TYPE} var i:integer; {$endif}
begin
 inherited Create;
 dim1:=Adim1;
 dim2:=Adim2;
 p:=GetMem(dim1*dim2*SizeOf(TDim2ArrayItem));
 {$ifdef DIM2ARRAY_IS_INIT_FINI_TYPE}
 for i:=0 to dim1*dim2-1 do
  Initialize(PDim2ArrayItem( PointerAdd(p,i*SizeOf(TDim2ArrayItem)) )^);
 {$endif}
end;

destructor TDim2Array.Destroy;
 {$ifdef DIM2ARRAY_IS_INIT_FINI_TYPE}
var i:integer;
begin
 if p<>nil then
  for i:=0 to dim1*dim2-1 do
   Finalize(PDim2ArrayItem( PointerAdd(p,i*SizeOf(TDim2ArrayItem)) )^);
 {$else}
begin
 {$endif}
 if p<>nil then FreeMem(p);
 inherited;
end;

procedure TDim2Array.FillMemByte(b:byte);
begin
 FillChar(P^, SizeOf(TDim2ArrayItem)*Length1*Length2, b);
end;

{$endif read_implementation}