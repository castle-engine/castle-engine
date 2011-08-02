{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestKambiClassUtils;

interface

uses Classes, SysUtils, fpcunit, testutils, testregistry, KambiUtils,
  KambiClassUtils, FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

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

  TFoosList = class(specialize TFPGObjectList<TFoo>)
  public
    procedure SortFoo;
  end;

implementation

{ TFoo, TFoosList ------------------------------------------------------------ }

constructor TFoo.Create(AI: Integer; AnS: string);
begin
  I := AI;
  S := AnS;
end;

function IsFooSmaller(const A, B: TFoo): Integer;
begin
  Result := A.I - B.I;
end;

procedure TFoosList.SortFoo;
begin
  Sort(@IsFooSmaller);
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
  L := TFoosList.Create(false);
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
