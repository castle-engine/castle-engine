{
  Copyright 2004-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestObjectsList;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestObjectsList = class(TTestCase)
    procedure TestObjectsList;
  end;

implementation

uses KambiUtils, KambiClassUtils;

{$define read_interface}
{$define read_implementation}

type
  TItem = class
    Str: string;
  end;

  TObjectsListItem = TItem;
  {$I objectslist.inc}
type
  TItemsList = class(TObjectsList)
    { For each item of the list, delete all it's duplicates. }
    procedure DeleteDuplicates;
  end;

procedure TItemsList.DeleteDuplicates;
var
  I, Index: integer;
begin
  I := 0;
  while I < Count do
  begin
    Index := I + 1;
    repeat
      Index := IndexOf(Items[I], Index);
      if Index = -1 then Break;
      Delete(Index);
    until false;

    Inc(I);
  end;
end;

procedure TTestObjectsList.TestObjectsList;
var ol, ol2: TItemsList;
begin
 ol := TItemsList.Create(false);
 try ol.Add(TItem.Create); ol[0].Str := 'foo'; ol[0].Free;
 finally ol.Free end;

 ol := TItemsList.Create(false);
 try ol.Add(TItem($454545)); ol.Delete(0);
 finally ol.Free end;

 ol := TItemsList.Create(false);
 try ol.Add(TItem.Create); ol.FreeContents
 finally ol.Free end;

 ol := TItemsList.Create(false);
 try ol.Add(nil);
 finally ol.FreeWithContents end;

 ol := TItemsList.Create(false);
 try ol.Add(TItem.Create);
 finally ol.FreeWithContents end;

 ol := TItemsList.Create(false);
 try
  ol.Add(TItem.Create); ol.Last.Str := 'first item';

  ol2 := TItemsList.Create(false);
  try
   ol2.Add(TItem.Create); ol2.Last.Str := 'one';
   ol2.Add(TItem.Create); ol2.Last.Str := 'two';
   { zwracam uwage ze dwa elementy na ol2 nie sa zwalniane - ich wskazniki
     sa kopiowane do ol }
   ol.AddList(ol2);
  finally ol2.Free end;

 finally ol.FreeWithContents end;

 ol := TItemsList.Create(false);
 try
  ol.Add(TItem.Create); ol.Last.Str := 'first item';

  ol2 := TItemsList.Create(false);
  try
   ol2.Add(TItem.Create); ol2.Last.Str := 'one';
   ol2.Add(TItem.Create); ol2.Last.Str := 'two';
   ol.AddList(ol2);
   ol.AddList(ol2);
  finally ol2.Free end;

  Assert(ol.Count = 5);
  Assert(ol[0].Str = 'first item');
  Assert(ol[1].Str = 'one');
  Assert(ol[2].Str = 'two');
  Assert(ol[3].Str = 'one');
  Assert(ol[4].Str = 'two');

  Assert(ol[1] = ol[3]); { (1 i 3) i (2 i 4) to te same obiekty. }
  Assert(ol[2] = ol[4]);

  { zeby FreeWithContents nie zrobilo dwa razy Free tego samego obiektu }
  ol.DeleteDuplicates;
 finally ol.FreeWithContents end;
end;

initialization
 RegisterTest(TTestObjectsList);
end.
