// -*- compile-command: "./test_single_testcase.sh TTestCastleClassUtils" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleClassUtils unit. }
unit TestCastleClassUtils;

{ Needed to define GENERICS_CONSTREF on some platforms/compilers. }
{$I ../../../src/common_includes/castleconf.inc}

interface

uses Classes, SysUtils, Generics.Collections,
  CastleTester, CastleUtils, CastleClassUtils;

type
  TStreamFromStreamFunc = function(Stream: TStream): TPeekCharStream of object;

  TTestCastleClassUtils = class(TCastleTestCase)
  private
    BufferSize: UInt32;
    function SimplePeekCharFromStream(Stream: TStream): TPeekCharStream;
    function BufferedReadStreamFromStream(Stream: TStream): TPeekCharStream;
    procedure TestIndirectReadStream(StreamFromStreamFunc: TStreamFromStreamFunc);
    procedure TestLineColumnStreamCore(StreamFromStreamFunc: TStreamFromStreamFunc);
    procedure DummyCallback;
    procedure DummyCallback2;
  published
    procedure TestStreamPeekChar;
    procedure TestBufferedReadStream;
    procedure TestObjectsListSort;
    procedure TestNotifyEventArray;
    procedure TestLineColumn_SimplePeekCharStream;
    procedure TestLineColumn_BufferedReadStream;
    procedure TestForIn;
    procedure TestSimpleNotifyEventListPack;
    procedure TestSimpleNotifyEventListUnassign;
  end;

  TFoo = class
    constructor Create(AI: Integer; AnS: string);
  public
    I: Integer;
    S: string;
  end;

  TFooList = class({$ifdef FPC}specialize{$endif} TObjectList<TFoo>)
  public
    procedure SortFoo;
  end;

implementation

uses Generics.Defaults,
  CastleStringUtils, CastleLog;

{ TFoo, TFoosList ------------------------------------------------------------ }

constructor TFoo.Create(AI: Integer; AnS: string);
begin
  I := AI;
  S := AnS;
end;

function IsFooSmaller({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TFoo): Integer;
begin
  Result := A.I - B.I;
end;

procedure TFooList.SortFoo;
type
  TFooComparer = {$ifdef FPC}specialize{$endif} TComparer<TFoo>;
begin
  Sort(TFooComparer.Construct({$ifdef FPC}@{$endif}IsFooSmaller));
end;

{ TTestCastleClassUtils ------------------------------------------------------- }

procedure TTestCastleClassUtils.TestIndirectReadStream(
  StreamFromStreamFunc: TStreamFromStreamFunc);
var
  SStream: TStringStream;
  ReaderStream: TPeekCharStream;
  Buf: array[0..2]of Byte;
begin
 SStream := TStringStream.Create(#1#2#3#4#5#6#7#8#9#10#11#12
   { no need to request UTF-8 encoding, it is default, also with Delphi. }
   {, TEncoding.UTF8});
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
 TestIndirectReadStream({$ifdef FPC}@{$endif}SimplePeekCharFromStream);
end;

function TTestCastleClassUtils.BufferedReadStreamFromStream(Stream: TStream):
  TPeekCharStream;
begin
 Result := TBufferedReadStream.Create(Stream, false, BufferSize);
end;

procedure TTestCastleClassUtils.TestBufferedReadStream;
var i: Integer;
begin
// TODO: Delphi
{$ifdef FPC}
 for i := 1 to 20 do
 begin
  BufferSize := i;
  TestIndirectReadStream({$ifdef FPC}@{$endif}BufferedReadStreamFromStream);
 end;
{$endif}
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
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O1.Dummy) = -1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O2.Dummy) = -1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O3.Dummy) = -1);

    A.Add({$ifdef FPC}@{$endif}O1.Dummy);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O1.Dummy) = 0);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O2.Dummy) = -1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O3.Dummy) = -1);

    A.Add({$ifdef FPC}@{$endif}O2.Dummy);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O1.Dummy) = 0);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O2.Dummy) = 1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O3.Dummy) = -1);

    A.Remove({$ifdef FPC}@{$endif}O1.Dummy);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O1.Dummy) = -1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O2.Dummy) = 0);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}O3.Dummy) = -1);

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
  TestLineColumnStreamCore({$ifdef FPC}@{$endif}SimplePeekCharFromStream);
end;

procedure TTestCastleClassUtils.TestLineColumn_BufferedReadStream;
begin
  BufferSize := DefaultReadBufferSize; // assign before using BufferedReadStreamFromStream
  TestLineColumnStreamCore({$ifdef FPC}@{$endif}BufferedReadStreamFromStream);
  BufferSize := 1; // assign before using BufferedReadStreamFromStream
  TestLineColumnStreamCore({$ifdef FPC}@{$endif}BufferedReadStreamFromStream);
end;

procedure TTestCastleClassUtils.TestForIn;
var
  C1, C2, C3, C: TComponent;
  T: TCastleComponent;
  I: Integer;
begin
  T := TCastleComponent.Create(nil);

  C1 := TComponent.Create(nil);
  C1.Name := 'B1';
  T.AddNonVisualComponent(C1);

  C2 := TComponent.Create(nil);
  C2.Name := 'B2';
  T.AddNonVisualComponent(C2);

  { This actually adds C1 again, as we don't have a problem with it
    on NonVisualComponents list, unlike AddBehavior. }
  T.AddNonVisualComponent(C1);

  C3 := TComponent.Create(nil);
  C3.Name := 'C3';
  T.AddNonVisualComponent(C3);

  AssertEquals(4, T.NonVisualComponentsCount);
  AssertTrue(T.NonVisualComponents[0] = C1);
  AssertTrue(T.NonVisualComponents[1] = C2);
  AssertTrue(T.NonVisualComponents[2] = C1);
  AssertTrue(T.NonVisualComponents[3] = C3);

  I := 0;
  for C in T.NonVisualComponentsEnumerate do
  begin
    AssertTrue(T.NonVisualComponents[I] = C);
    Inc(I);
  end;
  AssertEquals(4, I);

  FreeAndNil(T);
  FreeAndNil(C1);
  FreeAndNil(C2);
  FreeAndNil(C3);
end;

procedure TTestCastleClassUtils.DummyCallback;
begin
end;

procedure TTestCastleClassUtils.DummyCallback2;
begin
end;

{ While we could compare directly with SameMethods and typecasts to TMethod
  below, it is easier to make it compile with both FPC and Delphi
  by defining a dedicated function for this. }
function SameSimpleNotifyEvent(const M1, M2: TSimpleNotifyEvent): Boolean;
begin
  Result := SameMethods(
    TMethod(M1),
    TMethod(M2)
  );
end;

procedure TTestCastleClassUtils.TestSimpleNotifyEventListPack;
var
  L: TSimpleNotifyEventList;
  M: TSimpleNotifyEvent;
begin
  L := TSimpleNotifyEventList.Create;
  try
    AssertEquals(0, L.Count);

    L.Pack;
    AssertEquals(0, L.Count);

    L.Add(nil);
    AssertEquals(1, L.Count);

    L.Pack;
    AssertEquals(0, L.Count);

    L.Add({$ifdef FPC}@{$endif} DummyCallback);
    L.Add(nil);
    L.Add({$ifdef FPC}@{$endif} DummyCallback2);
    L.Add(nil);
    AssertEquals(4, L.Count);

    L.Pack;
    AssertEquals(2, L.Count);
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback, L[0]));
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback2, L[1]));
  finally FreeAndNil(L) end;
end;

procedure TTestCastleClassUtils.TestSimpleNotifyEventListUnassign;
var
  L: TSimpleNotifyEventList;
  M: TSimpleNotifyEvent;
begin
  L := TSimpleNotifyEventList.Create;
  try
    AssertEquals(0, L.Count);

    L.Unassign({$ifdef FPC}@{$endif} DummyCallback);
    AssertEquals(0, L.Count);

    L.Add({$ifdef FPC}@{$endif} DummyCallback);
    L.Add({$ifdef FPC}@{$endif} DummyCallback);
    L.Unassign({$ifdef FPC}@{$endif} DummyCallback);
    AssertEquals(2, L.Count);
    AssertTrue(SameSimpleNotifyEvent(nil, L[0]));
    AssertTrue(SameSimpleNotifyEvent(nil, L[1]));

    L.Pack;
    AssertEquals(0, L.Count);

    L.Add({$ifdef FPC}@{$endif} DummyCallback2);
    L.Add({$ifdef FPC}@{$endif} DummyCallback);
    L.Add(nil);
    L.Add({$ifdef FPC}@{$endif} DummyCallback);
    L.Add(nil);
    L.Add({$ifdef FPC}@{$endif} DummyCallback2);
    L.Unassign({$ifdef FPC}@{$endif} DummyCallback2);
    AssertEquals(6, L.Count);
    AssertTrue(SameSimpleNotifyEvent(nil, L[0]));
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback, L[1]));
    AssertTrue(SameSimpleNotifyEvent(nil, L[2]));
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback, L[3]));
    AssertTrue(SameSimpleNotifyEvent(nil, L[4]));
    AssertTrue(SameSimpleNotifyEvent(nil, L[5]));

    L.Pack;
    AssertEquals(2, L.Count);
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback, L[0]));
    AssertTrue(SameSimpleNotifyEvent({$ifdef FPC}@{$endif} DummyCallback, L[1]));
  finally FreeAndNil(L) end;
end;

initialization
  RegisterTest(TTestCastleClassUtils);
end.
