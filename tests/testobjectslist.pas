{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
  type TItemsList = TObjectsList;

procedure TTestObjectsList.TestObjectsList;
var ol, ol2: TItemsList;
begin
 ol := TItemsList.Create;
 try ol.Add(TItem.Create); ol[0].Str := 'foo'; ol[0].Free;
 finally ol.Free end;

 ol := TItemsList.Create;
 try ol.Add(TItem($454545)); ol.Delete(0);
 finally ol.Free end;

 ol := TItemsList.Create;
 try ol.Add(TItem.Create); ol.FreeContents
 finally ol.Free end;

 ol := TItemsList.Create;
 try ol.Add(nil);
 finally ol.FreeWithContents end;

 ol := TItemsList.Create;
 try ol.Add(TItem.Create);
 finally ol.FreeWithContents end;

 ol := TItemsList.Create;
 try
  ol.Add(TItem.Create); ol.Last.Str := 'first item';

  ol2 := TItemsList.Create;
  try
   ol2.Add(TItem.Create); ol2.Last.Str := 'one';
   ol2.Add(TItem.Create); ol2.Last.Str := 'two';
   { zwracam uwage ze dwa elementy na ol2 nie sa zwalniane - ich wskazniki
     sa kopiowane do ol }
   ol.AddList(ol2);
  finally ol2.Free end;

 finally ol.FreeWithContents end;

 ol := TItemsList.Create;
 try
  ol.Add(TItem.Create); ol.Last.Str := 'first item';

  ol2 := TItemsList.Create;
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
