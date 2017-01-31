{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleClassUtils;

interface

uses Classes, SysUtils, fpcunit, testutils, testregistry, CastleUtils,
  CastleClassUtils, FGL;

type
  TStreamFromStreamFunc = function(Stream: TStream): TPeekCharStream of object;

  TTestCastleClassUtils = class(TTestCase)
  private
    BufferSize: LongWord;
    function SimplePeekCharFromStream(Stream: TStream): TPeekCharStream;
    function BufferedReadStreamFromStream(Stream: TStream): TPeekCharStream;
    procedure TestIndirectReadStream(StreamFromStreamFunc: TStreamFromStreamFunc);
    procedure TestLineColumnStreamCore(StreamFromStreamFunc: TStreamFromStreamFunc);
  published
    procedure TestStreamPeekChar;
    procedure TestBufferedReadStream;
    procedure TestObjectsListSort;
    procedure TestNotifyEventArray;
    procedure TestLineColumn_SimplePeekCharStream;
    procedure TestLineColumn_BufferedReadStream;
  end;

  TFoo = class
    constructor Create(AI: Integer; AnS: string);
  public
    I: Integer;
    S: string;
  end;

  TFooList = class(specialize TFPGObjectList<TFoo>)
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

procedure TFooList.SortFoo;
begin
  Sort(@IsFooSmaller);
end;

{ TTestCastleClassUtils ------------------------------------------------------- }

procedure TTestCastleClassUtils.TestIndirectReadStream(
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
   AssertTrue(ReaderStream.Size = 12);
   AssertTrue(ReaderStream.PeekChar = 1);
   AssertTrue(ReaderStream.PeekChar = 1);
   AssertTrue(ReaderStream.PeekChar = 1);
   AssertTrue(ReaderStream.ReadChar = 1);

   AssertTrue(ReaderStream.PeekChar = 2);
   AssertTrue(ReaderStream.ReadChar = 2);

   AssertTrue(ReaderStream.ReadChar = 3);

   AssertTrue(ReaderStream.ReadChar = 4);

   AssertTrue(ReaderStream.ReadUpto([#8, #9, #10]) = #5#6#7);

   AssertTrue(ReaderStream.Position = 7);
   AssertTrue(ReaderStream.ReadChar = 8);
   AssertTrue(ReaderStream.Position = 8);

   ReaderStream.ReadBuffer(Buf, 3);
   AssertTrue(Buf[0] =  9);
   AssertTrue(Buf[1] = 10);
   AssertTrue(Buf[2] = 11);

   AssertTrue(ReaderStream.PeekChar = 12);
   AssertTrue(ReaderStream.ReadChar = 12);

   AssertTrue(ReaderStream.Read(Buf, 1) = 0);
   AssertTrue(ReaderStream.PeekChar = -1);
   AssertTrue(ReaderStream.Read(Buf, 1) = 0);
  finally ReaderStream.Free end;
 finally SStream.Free end;
end;

function TTestCastleClassUtils.SimplePeekCharFromStream(Stream: TStream):
  TPeekCharStream;
begin
 Result := TSimplePeekCharStream.Create(Stream, false);
end;

procedure TTestCastleClassUtils.TestStreamPeekChar;
begin
 TestIndirectReadStream(@SimplePeekCharFromStream);
end;

function TTestCastleClassUtils.BufferedReadStreamFromStream(Stream: TStream):
  TPeekCharStream;
begin
 Result := TBufferedReadStream.Create(Stream, false, BufferSize);
end;

procedure TTestCastleClassUtils.TestBufferedReadStream;
var i: Integer;
begin
 for i := 1 to 20 do
 begin
  BufferSize := i;
  TestIndirectReadStream(@BufferedReadStreamFromStream);
 end;
end;

procedure TTestCastleClassUtils.TestObjectsListSort;
var
  L: TFooList;
begin
  L := TFooList.Create(true);
  try
    L.Add(TFoo.Create(123, 'abc'));
    L.Add(TFoo.Create(-5, 'ZZZ'));
    L.Add(TFoo.Create(65, 'zuzanna'));
    L.SortFoo;
    AssertTrue(L.Count = 3);
    AssertTrue(L[0].I = -5);
    AssertTrue(L[1].I = 65);
    AssertTrue(L[2].I = 123);
  finally FreeAndNil(L) end;
end;

type
  TObj = class
    procedure Dummy(Sender: TObject);
  end;

procedure TObj.Dummy(Sender: TObject);
begin
end;

procedure TTestCastleClassUtils.TestNotifyEventArray;
{ There's a trap when implementing lists of methods: normal comparison operator
  is nonsense for methods, it compares only the code pointer.
  See http://bugs.freepascal.org/view.php?id=11868 ,
  http://bugs.freepascal.org/view.php?id=9228 .
  Make sure our TNotifyEventList doesn't have this problem. }
var
  A: TNotifyEventList;
  O1, O2, O3: TObj;
begin
  A := TNotifyEventList.Create;
  try
    O1 := TObj.Create;
    O2 := TObj.Create;
    O3 := TObj.Create;
    AssertTrue(A.IndexOf(@O1.Dummy) = -1);
    AssertTrue(A.IndexOf(@O2.Dummy) = -1);
    AssertTrue(A.IndexOf(@O3.Dummy) = -1);

    A.Add(@O1.Dummy);
    AssertTrue(A.IndexOf(@O1.Dummy) = 0);
    AssertTrue(A.IndexOf(@O2.Dummy) = -1);
    AssertTrue(A.IndexOf(@O3.Dummy) = -1);

    A.Add(@O2.Dummy);
    AssertTrue(A.IndexOf(@O1.Dummy) = 0);
    AssertTrue(A.IndexOf(@O2.Dummy) = 1);
    AssertTrue(A.IndexOf(@O3.Dummy) = -1);

    A.Remove(@O1.Dummy);
    AssertTrue(A.IndexOf(@O1.Dummy) = -1);
    AssertTrue(A.IndexOf(@O2.Dummy) = 0);
    AssertTrue(A.IndexOf(@O3.Dummy) = -1);

    FreeAndNil(O1);
    FreeAndNil(O2);
    FreeAndNil(O3);
  finally FreeAndNil(A) end;
end;

procedure TTestCastleClassUtils.TestLineColumnStreamCore(StreamFromStreamFunc: TStreamFromStreamFunc);
var
  S: TStringStream;
  PS: TPeekCharStream;
  B: array [0..1000] of char;
  I: Integer;
begin
  { ReadBuffer (using Read underneath) advances the Line and Column values correctly }
  S := TStringStream.Create('blabla' + #13#10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      PS.ReadBuffer(B, 10);
      AssertEquals(2, PS.Line);
      AssertEquals(3, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;

  { 2 * ReadBuffer(5,..) is the same thing as 1 * ReadBuffer(10,..) }
  S := TStringStream.Create('blabla' + #13#10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      PS.ReadBuffer(B, 5);
      PS.ReadBuffer(B, 5);
      AssertEquals(2, PS.Line);
      AssertEquals(3, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;

  { works with Unix line endings too, they are 1 char shorter }
  S := TStringStream.Create('blabla' + #10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      PS.ReadBuffer(B, 10);
      AssertEquals(2, PS.Line);
      AssertEquals(4, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;

  { 10 * ReadChar is the same thing as 1 * ReadBuffer(10,..) }
  S := TStringStream.Create('blabla' + #10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      for I := 1 to 10 do
        PS.ReadChar;
      AssertEquals(2, PS.Line);
      AssertEquals(4, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;

  { PeekChar doesn't affect it }
  S := TStringStream.Create('blabla' + #10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      for I := 1 to 10 do
        PS.ReadChar;
      PS.PeekChar; // does not change state compared to previous test
      AssertEquals(2, PS.Line);
      AssertEquals(4, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;

  { ReadUpto works too }
  S := TStringStream.Create('blabla' + #13#10 + 'foobar');
  try
    PS := StreamFromStreamFunc(S);
    try
      PS.ReadUpto(['o']);
      AssertEquals(2, PS.Line);
      AssertEquals(2, PS.Column);
    finally FreeAndNil(PS) end;
  finally FreeAndNil(S) end;
end;

procedure TTestCastleClassUtils.TestLineColumn_SimplePeekCharStream;
begin
  TestLineColumnStreamCore(@SimplePeekCharFromStream);
end;

procedure TTestCastleClassUtils.TestLineColumn_BufferedReadStream;
begin
  BufferSize := DefaultReadBufferSize; // assign before using BufferedReadStreamFromStream
  TestLineColumnStreamCore(@BufferedReadStreamFromStream);
  BufferSize := 1; // assign before using BufferedReadStreamFromStream
  TestLineColumnStreamCore(@BufferedReadStreamFromStream);
end;

initialization
  RegisterTest(TTestCastleClassUtils);
end.
