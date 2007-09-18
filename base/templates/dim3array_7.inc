{
  Copyright 2002-2004 Michalis Kamburelis.

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

{ TDim3XxxArray ------------------------------------------------------------ }

{ TDim3Array_7 - obiekt zarzadzajacy 3-wymiarowa tablica [0..high1,0..high2,0..high3].
  Wykonany absolutnie analogicznie do Dim2Array.

  makra (FPC) lub typy (Delphi lub FPC) do zdefiniowania :
  TDim3Array_7 (zostanie zdefiniowany)
  TDim3ArrayItem_7 (ma byc zdefiniowany - to jest typ elementow tablicy)
  PDim3ArrayItem_7 (ma byc zdefiniowany jako ^TDim3ArrayItem_7)

  symbole zdefiniowane lub nie :
   DIM3ARRAY_IS_INIT_FINE_TYPE ,
   DIM3ARRAY_7_IS_STRUCT ,
   DIM3ARRAY_7_RANGECHECKS - znaczenie jak w Dim2Array
}

{$ifdef read_interface}

type
  TDim3Array_7 = class
  private
    P:PDim3ArrayItem_7;
    dim1,dim2,dim3:integer;
    function GetPointers(Adim1, Adim2, Adim3:integer):PDim3ArrayItem_7;
    function GetHigh1:integer;
    function GetHigh2:integer;
    function GetHigh3:integer;
    {$ifndef DIM3ARRAY_7_IS_STRUCT}
    function GetValues(Adim1, Adim2, Adim3:integer):TDim3ArrayItem_7;
    procedure SetValues(Adim1, Adim2, Adim3:integer; const Value:TDim3ArrayItem_7);
    {$endif}
  public
    { wskaznik na tablice packed array [0..dim1*dim2*dim3-1] of TDim3ArrayItem_7,
      wewnetrzna tablica reprezentujaca 3-wymiarowa tablice.
      Tablica jest ulozona w pamieci tak jak array[0..dim1-1, 0..dim2-1, 0..dim3-1]
      a wiec
      - pierwszy index powoduje zmiany wskaznika o
        dim2*dim3*SizeOf(TDim3ArrayItem_7) (gdy pierwszy indeks zmienia sie o 1),
      - drugi indeks zmienia wskaznik tylko o dim3*SizeOf(TDim3ArrayItem_7)
        gdy sam zmienia sie o 1 i
      - wreszcie trzeci indeks zmienia adres o SizeOf(TDim3ArrayItem_7) gdy sam
        zmienia sie o jeden. Regula jest jasna.
    }
    property ArrayPtr:PDim3ArrayItem_7 read P;

    property Length1:integer read dim1;
    property Length2:integer read dim2;
    property Length3:integer read dim3;
    property High1:integer read GetHigh1; { = Length1 - 1 }
    property High2:integer read GetHigh2; { = Length2 - 1 }
    property High3:integer read GetHigh3; { = Length3 - 1 }

    constructor Create(Adim1, Adim2, Adim3:integer);
    destructor Destroy; override;
    property Pointers[Adim1, Adim2, Adim3:integer]:PDim3ArrayItem_7 read GetPointers; {$ifdef DIM3ARRAY_7_IS_STRUCT} default; {$endif}
    {$ifndef DIM3ARRAY_7_IS_STRUCT}
    property Values[Adim1, Adim2, Adim3:integer]:TDim3ArrayItem_7 read GetValues write SetValues; default;
    {$endif}

    { FillMemByte wykonuje
      FillChar(ArrayPtr^, SizeOf(TDim3ArrayItem_7)*Length1*Length2*Length3, b).
      Innymi slowy, cala tablica zostanie wypelniona bajtami b.
      Uwazaj - czy otrzymane wartosci w tablicy beda sensowne zalezy tylko od ciebie ! }
    procedure FillMemByte(b:byte);

    { SameDimensions sprawdza czy wymiary dwoch tablic sa takie same. }
    function SameDimensions(secondArray:TDim3Array_7):boolean;
    { SameContent sprawdza czy wymiary I zawartosci (porownuje przez CompareMem
      a wiec sprawdza dokladna binarna rownosc) dwoch tablic sa rowne. }
    function SameContent(secondArray:TDim3Array_7):boolean;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TDim3Array_7.GetHigh1:integer; begin result:=Length1-1 end;
function TDim3Array_7.GetHigh2:integer; begin result:=Length2-1 end;
function TDim3Array_7.GetHigh3:integer; begin result:=Length3-1 end;

function TDim3Array_7.GetPointers(Adim1, Adim2, Adim3:integer):PDim3ArrayItem_7;
begin
 {$ifdef DIM3ARRAY_7_RANGECHECKS}
 if (Adim1<0) or (Adim1>=dim1) or
    (Adim2<0) or (Adim2>=dim2) or
    (Adim3<0) or (Adim3<=dim3) then
  ERangeError.CreateFmt(
   'TDim3Array_7 range error : requested item %d, %d, %d, array dims : [%d, %d, %d]',
   [Adim1, Adim2, Adim3, dim1, dim2, dim3]);
 {$endif}
 result:=PDim3ArrayItem_7( PointerAdd(p,(Adim1*dim2*dim3+ Adim2*dim3 +Adim3) *SizeOf(TDim3ArrayItem_7)) );
end;

{$ifndef DIM3ARRAY_7_IS_STRUCT}
function TDim3Array_7.GetValues(Adim1, Adim2, Adim3:integer):TDim3ArrayItem_7;
begin
 result:=Pointers[Adim1, Adim2, Adim3]^;
end;

procedure TDim3Array_7.SetValues(Adim1, Adim2, Adim3:integer; const Value:TDim3ArrayItem_7);
begin
 Pointers[Adim1, Adim2, Adim3]^:=Value;
end;
{$endif}

constructor TDim3Array_7.Create(Adim1, Adim2, Adim3:integer);
{$ifdef DIM3ARRAY_7_IS_INIT_FINI_TYPE} var i:integer; {$endif}
begin
 inherited Create;
 dim1:=Adim1;
 dim2:=Adim2;
 dim3:=Adim3;
 p:=GetMem(dim1*dim2*dim3*SizeOf(TDim3ArrayItem_7));
 {$ifdef DIM3ARRAY_7_IS_INIT_FINI_TYPE}
 for i:=0 to dim1*dim2*dim3-1 do
  Initialize(PDim3ArrayItem_7( PointerAdd(p,i*SizeOf(TDim3ArrayItem_7)) )^);
 {$endif}
end;

destructor TDim3Array_7.Destroy;
 {$ifdef DIM3ARRAY_7_IS_INIT_FINI_TYPE}
var i:integer;
begin
 if p<>nil then
  for i:=0 to dim1*dim2*dim3-1 do
   Finalize(PDim3ArrayItem_7( PointerAdd(p,i*SizeOf(TDim3ArrayItem_7)) )^);
 {$else}
begin
 {$endif}
 if p<>nil then FreeMem(p);
 inherited;
end;

procedure TDim3Array_7.FillMemByte(b:byte);
begin
 FillChar(P^, SizeOf(TDim3ArrayItem_7)*Length1*Length2*Length3, b);
end;

function TDim3Array_7.SameDimensions(secondArray:TDim3Array_7):boolean;
begin
 result:=(Length1 = secondArray.Length1) and
         (Length2 = secondArray.Length2) and
         (Length3 = secondArray.Length3);
end;

function TDim3Array_7.SameContent(secondArray:TDim3Array_7):boolean;
begin
 result:=SameDimensions(secondArray) and
         CompareMem(ArrayPtr, secondArray.ArrayPtr,
           SizeOf(TDim3ArrayItem_7)*Length1*Length2*Length3);

end;

{$endif read_implementation}
