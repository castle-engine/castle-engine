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
  Classes, SysUtils, fpcunit, testutils, testregistry,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  TTestObjectsList = class(TTestCase)
    procedure TestObjectsList;
  end;

implementation

uses KambiUtils, KambiClassUtils;

type
  TItem = class
    Str: string;
  end;

  TItemsList = class(specialize TFPGObjectList<TItem>)
    { For each item of the list, delete all it's duplicates. }
    procedure DeleteDuplicates;
    procedure AddList(L: TItemsList);
  end;

procedure TItemsList.DeleteDuplicates;

  function IndexOf(Item: TItem; StartIndex: Integer): Integer;
  begin
    for result := StartIndex to Count - 1 do
      if Item = Items[result] then exit;
    result := -1;
  end;

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
      FPGObjectList_NilItem(Self, Index);
      Delete(Index);
    until false;

    Inc(I);
  end;
end;

procedure TItemsList.AddList(L: TItemsList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Pointer) * L.Count);
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

 ol := TItemsList.Create(true);
 try ol.Add(TItem.Create); ol.Clear;
 finally ol.Free end;

 ol := TItemsList.Create(true);
 try ol.Add(nil); ol.Clear;
 finally ol.Free end;

 ol := TItemsList.Create(true);
 try
  ol.Add(TItem.Create); ol.Last.Str := 'first item';

  ol2 := TItemsList.Create(false);
  try
   ol2.Add(TItem.Create); ol2.Last.Str := 'one';
   ol2.Add(TItem.Create); ol2.Last.Str := 'two';
   ol.AddList(ol2);
  finally ol2.Free end;
  ol.Clear;
 finally ol.Free end;

 ol := TItemsList.Create(true);
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

  { delete dups, preventing Clear from Freeing two times the same reference }
  ol.DeleteDuplicates;
  ol.Clear;
 finally ol.Free end;
end;

initialization
 RegisterTest(TTestObjectsList);
end.
