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

unit TestKambiClassUtils;

interface

uses Classes, SysUtils, fpcunit, testutils, testregistry, KambiUtils,
  KambiClassUtils;

type
  TStreamFromStreamFunc = function(Stream: TStream): TPeekCharStream of object;

  TTestKambiClassUtils = class(TTestCase)
  private
    BufferSize: LongWord;
    procedure TestIndirectReadStream(
      StreamFromStreamFunc: TStreamFromStreamFunc);
    function StreamPeekCharFromStream(Stream: TStream): TPeekCharStream;
    function BufferedReadStreamFromStream(Stream: TStream): TPeekCharStream;
  published
    procedure TestStreamPeekChar;
    procedure TestBufferedReadStream;
    procedure TestObjectsListSort;
  end;

  TFoo = class
    constructor Create(AI: Integer; AnS: string);
  public
    I: Integer;
    S: string;
  end;

{$define read_interface}

  TObjectsListItem_1 = TFoo;
  {$I objectslist_1.inc}
  TFoosList = class(TObjectsList_1)
  private
    function IsFooSmaller(const A, B: TFoo): boolean;
  public
    procedure SortFoo;
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}

{ TFoo, TFoosList ------------------------------------------------------------ }

constructor TFoo.Create(AI: Integer; AnS: string);
begin
  I := AI;
  S := AnS;
end;

procedure TFoosList.SortFoo;
begin
  Sort(@IsFooSmaller);
end;

function TFoosList.IsFooSmaller(const A, B: TFoo): boolean;
begin
  Result := A.I < B.I;
end;

{ TTestKambiClassUtils ------------------------------------------------------- }

procedure TTestKambiClassUtils.TestIndirectReadStream(
  StreamFromStreamFunc: TStreamFromStreamFunc);
var
  SStream: TStringStream;
  ReaderStream: TPeekCharStream;
  Buf: array[0..2]of Byte;
begin
 SStream := TStringStream.Create(#1#2#3#4#5#6#7#8#9#10#11#12);
 try
  SStream.Position := 0;
  ReaderStream := StreamFromStreamFunc(SStream);
  try
   Assert(ReaderStream.Size = 12);
   Assert(ReaderStream.PeekChar = 1);
   Assert(ReaderStream.PeekChar = 1);
   Assert(ReaderStream.PeekChar = 1);
   Assert(ReaderStream.ReadChar = 1);

   Assert(ReaderStream.PeekChar = 2);
   Assert(ReaderStream.ReadChar = 2);

   Assert(ReaderStream.ReadChar = 3);

   Assert(ReaderStream.ReadChar = 4);

   Assert(ReaderStream.ReadUpto([#8, #9, #10]) = #5#6#7);

   Assert(ReaderStream.Position = 7);
   Assert(ReaderStream.ReadChar = 8);
   Assert(ReaderStream.Position = 8);

   ReaderStream.ReadBuffer(Buf, 3);
   Assert(Buf[0] =  9);
   Assert(Buf[1] = 10);
   Assert(Buf[2] = 11);

   Assert(ReaderStream.PeekChar = 12);
   Assert(ReaderStream.ReadChar = 12);

   Assert(ReaderStream.Read(Buf, 1) = 0);
   Assert(ReaderStream.PeekChar = -1);
   Assert(ReaderStream.Read(Buf, 1) = 0);
  finally ReaderStream.Free end;
 finally SStream.Free end;
end;

function TTestKambiClassUtils.StreamPeekCharFromStream(Stream: TStream):
  TPeekCharStream;
begin
 Result := TSimplePeekCharStream.Create(Stream, false);
end;

procedure TTestKambiClassUtils.TestStreamPeekChar;
begin
 TestIndirectReadStream(@StreamPeekCharFromStream);
end;

function TTestKambiClassUtils.BufferedReadStreamFromStream(Stream: TStream):
  TPeekCharStream;
begin
 Result := TBufferedReadStream.Create(Stream, false, BufferSize);
end;

procedure TTestKambiClassUtils.TestBufferedReadStream;
var i: Integer;
begin
 for i := 1 to 20 do
 begin
  BufferSize := i;
  TestIndirectReadStream(@BufferedReadStreamFromStream);
 end;
end;

procedure TTestKambiClassUtils.TestObjectsListSort;
var
  L: TFoosList;
begin
  L := TFoosList.Create;
  try
    L.Add(TFoo.Create(123, 'abc'));
    L.Add(TFoo.Create(-5, 'ZZZ'));
    L.Add(TFoo.Create(65, 'zuzanna'));
    L.SortFoo;
    Assert(L.Count = 3);
    Assert(L[0].I = -5);
    Assert(L[1].I = 65);
    Assert(L[2].I = 123);
  finally FreeWithContentsAndNil(L) end;
end;

initialization
 RegisterTest(TTestKambiClassUtils);
end.
