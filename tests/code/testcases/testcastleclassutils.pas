// -*- compile-command: "./test_single_testcase.sh TTestCastleClassUtils" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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
    procedure TestReadUptoUtf8Core(StreamFromStreamFunc: TStreamFromStreamFunc);
    procedure DummyCallback;
    procedure DummyCallback2;
  published
    procedure TestStreamPeekChar;
    procedure TestBufferedReadStream;
    procedure TestObjectsListSort;
    procedure TestNotifyEventArray;
    procedure TestLineColumn_SimplePeekCharStream;
    procedure TestLineColumn_BufferedReadStream;
    procedure TestReadUptoUtf8_SimplePeekCharStream;
    procedure TestReadUptoUtf8_BufferedReadStream;
    procedure TestWriteStrUtf8;
    procedure TestForIn;
    procedure TestSimpleNotifyEventListPack;
    procedure TestSimpleNotifyEventListUnassign;
    procedure TestComponentMap;
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
  CastleStringUtils, CastleLog, CastleUnicode;

{ Test data for the UTF-8 tests below.

  Note that we deliberately do @italic(not) use non-ASCII string literals here,
  as their meaning depends on the compiler and the encoding of this source file.
  Instead we spell out both

  - the exact UTF-8 bytes,
  - and the exact Unicode code points

  of the tested text, and build the tested strings from them at runtime. }

const
  { UTF-8 bytes of "zolc" with Polish diacritics, i.e. the Polish word
    "bile" written using 4 non-ASCII letters. }
  PolishUtf8Bytes: array [0..7] of Byte = (
    $C5, $BC, // U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
    $C3, $B3, // U+00F3 LATIN SMALL LETTER O WITH ACUTE
    $C5, $82, // U+0142 LATIN SMALL LETTER L WITH STROKE
    $C4, $87  // U+0107 LATIN SMALL LETTER C WITH ACUTE
  );

  { Unicode code points of the same 4 characters. }
  PolishCodePoints: array [0..3] of TUnicodeChar = ($17C, $F3, $142, $107);

{ The tested text as 8-bit string with UTF-8 encoding. }
function PolishUtf8: Utf8String;
begin
  SetLength(Result, Length(PolishUtf8Bytes));
  Move(PolishUtf8Bytes[0], Result[1], Length(PolishUtf8Bytes));
end;

{ The tested text as the default String
  (UTF-8 with FPC, UTF-16 with Delphi). }
function PolishString: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(PolishCodePoints) do
    Result := Result + UnicodeCharToString(PolishCodePoints[I]);
end;

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

procedure TTestCastleClassUtils.TestReadUptoUtf8Core(
  StreamFromStreamFunc: TStreamFromStreamFunc);

{ Test that TPeekCharStream.ReadUpto returns the raw 8-bit UTF-8 bytes
  from the stream, and that assigning them to a String decodes them correctly.

  This matters when AnsiString has some platform-specific encoding, which
  happens when CASTLE_DONT_CHANGE_STRING_ENCODING is defined. That's why
  ReadUpto returns Utf8String, not AnsiString.
  See ../../../doc/miscellaneous_notes/ansistring_encoding.md . }

var
  SourceStream: TMemoryStream;
  ReaderStream: TPeekCharStream;
  ReadUtf8: Utf8String;
  ReadStr: String;
  I: Integer;
  SeparatorByte: Byte;
begin
  SourceStream := TMemoryStream.Create;
  try
    { Stream contents: <Polish UTF-8 bytes> '|' <Polish UTF-8 bytes> }
    SourceStream.WriteBuffer(PolishUtf8Bytes[0], Length(PolishUtf8Bytes));
    SeparatorByte := Ord('|');
    SourceStream.WriteBuffer(SeparatorByte, 1);
    SourceStream.WriteBuffer(PolishUtf8Bytes[0], Length(PolishUtf8Bytes));
    SourceStream.Position := 0;

    ReaderStream := StreamFromStreamFunc(SourceStream);
    try
      ReadUtf8 := ReaderStream.ReadUpto(['|']);

      { The raw bytes are preserved, no encoding conversion happened. }
      AssertEquals(Length(PolishUtf8Bytes), Length(ReadUtf8));
      for I := 0 to High(PolishUtf8Bytes) do
        AssertEquals(Integer(PolishUtf8Bytes[I]), Ord(ReadUtf8[I + 1]));

      { And converting the result to String decodes UTF-8 correctly. }
      ReadStr := ReadUtf8;
      AssertEquals(PolishString, ReadStr);
      AssertEquals(Length(PolishCodePoints), StringLength(ReadStr));

      AssertEquals(Ord('|'), ReaderStream.ReadChar);

      { Once more, this time the reading ends because of the end of stream.
        Also test the implicit Utf8String -> String conversion at assignment. }
      ReadStr := ReaderStream.ReadUpto(['|']);
      AssertEquals(PolishString, ReadStr);
      AssertEquals(Length(PolishCodePoints), StringLength(ReadStr));

      AssertEquals(-1, ReaderStream.ReadChar);
    finally FreeAndNil(ReaderStream) end;
  finally FreeAndNil(SourceStream) end;
end;

procedure TTestCastleClassUtils.TestReadUptoUtf8_SimplePeekCharStream;
begin
  TestReadUptoUtf8Core({$ifdef FPC}@{$endif}SimplePeekCharFromStream);
end;

procedure TTestCastleClassUtils.TestReadUptoUtf8_BufferedReadStream;
begin
  BufferSize := DefaultReadBufferSize; // assign before using BufferedReadStreamFromStream
  TestReadUptoUtf8Core({$ifdef FPC}@{$endif}BufferedReadStreamFromStream);
  { Buffer smaller than the read text, to exercise also the code path
    that has to enlarge the result while reading. }
  BufferSize := 1; // assign before using BufferedReadStreamFromStream
  TestReadUptoUtf8Core({$ifdef FPC}@{$endif}BufferedReadStreamFromStream);
end;

procedure TTestCastleClassUtils.TestWriteStrUtf8;

{ Test that WriteStr, WritelnStr, MemoryStreamLoadFromString put UTF-8
  into the stream, and StreamToString reads UTF-8 back, regardless of
  what encoding AnsiString happens to have.
  See ../../../doc/miscellaneous_notes/ansistring_encoding.md . }

  { Check that Stream contents are exactly the UTF-8 bytes of our test text,
    optionally followed by the newline. }
  procedure CheckStreamContents(const Stream: TMemoryStream;
    const ExpectNewLine: Boolean);
  var
    Buf: array of Byte;
    I, ExpectedSize: Integer;
  begin
    ExpectedSize := Length(PolishUtf8Bytes);
    if ExpectNewLine then
      ExpectedSize := ExpectedSize + Length(NL);
    AssertEquals(ExpectedSize, Stream.Size);

    SetLength(Buf, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Buf[0], Length(Buf));
    for I := 0 to High(PolishUtf8Bytes) do
      AssertEquals(Integer(PolishUtf8Bytes[I]), Integer(Buf[I]));

    { Reading the stream contents back as a string decodes UTF-8 correctly. }
    AssertEquals(PolishString, Trim(StreamToString(Stream)));
    AssertEquals(Length(PolishCodePoints), StringLength(Trim(StreamToString(Stream))));
  end;

var
  Stream: TMemoryStream;
begin
  { WriteStr with default String (16-bit with Delphi) converts it to UTF-8. }
  Stream := TMemoryStream.Create;
  try
    WriteStr(Stream, PolishString);
    CheckStreamContents(Stream, false);
  finally FreeAndNil(Stream) end;

  { WriteStr with explicit Utf8String writes the bytes as-is. }
  Stream := TMemoryStream.Create;
  try
    WriteStr(Stream, PolishUtf8);
    CheckStreamContents(Stream, false);
  finally FreeAndNil(Stream) end;

  { WritelnStr adds the newline, otherwise it behaves the same. }
  Stream := TMemoryStream.Create;
  try
    WritelnStr(Stream, PolishString);
    CheckStreamContents(Stream, true);
  finally FreeAndNil(Stream) end;

  Stream := TMemoryStream.Create;
  try
    WritelnStr(Stream, PolishUtf8);
    CheckStreamContents(Stream, true);
  finally FreeAndNil(Stream) end;

  { MemoryStreamLoadFromString is the symmetric counterpart of StreamToString. }
  Stream := MemoryStreamLoadFromString(PolishString);
  try
    CheckStreamContents(Stream, false);
  finally FreeAndNil(Stream) end;

  Stream := MemoryStreamLoadFromString(PolishUtf8);
  try
    CheckStreamContents(Stream, false);
  finally FreeAndNil(Stream) end;
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

procedure TTestCastleClassUtils.TestComponentMap;
var
  ComponentMap: TComponentMap;
  C1, C2, C3: TComponent;
  Key, StoredKey: String;
begin
  C1 := nil;
  C2 := nil;
  C3 := nil;
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    C3 := TComponent.Create(nil);

    ComponentMap := TComponentMap.Create;
    try
      ComponentMap.Add('C1', C1);
      AssertEquals(1, ComponentMap.Count);
      ComponentMap.Add('C2', C2);
      AssertEquals(2, ComponentMap.Count);

      try
        ComponentMap.Add('C1', C3); // overwrite C1 with C3
        Fail('Should raise EListError because of duplicate key C1 (value C3 doesn''t matter)');
      except
        on E: Exception do
          AssertTrue(E is EListError);
      end;
      AssertEquals(2, ComponentMap.Count);

      try
        ComponentMap.Add('c1', C3); // overwrite C1 with C3
        Fail('Should raise EListError because of duplicate key C1 (value C3 doesn''t matter)');
      except
        on E: Exception do
          AssertTrue(E is EListError);
      end;
      AssertEquals(2, ComponentMap.Count);

      AssertTrue(ComponentMap.ContainsKey('C1'));
      AssertTrue(ComponentMap.ContainsKey('c1'));
      AssertFalse(ComponentMap.ContainsKey('C3'));
      AssertFalse(ComponentMap.ContainsKey('c3'));

      { Verify that the original case of the keys is preserved.
        We use a case-insensitive comparer, but we do not lowercase the
        stored keys, so the key remains 'MixedCaseKey', not 'mixedcasekey'. }
      ComponentMap.Add('MixedCaseKey', C3);
      AssertEquals(3, ComponentMap.Count);
      AssertTrue(ComponentMap.ContainsKey('mixedcasekey')); // case-insensitive match
      StoredKey := '';
      for Key in ComponentMap.Keys do
        if SameText(Key, 'MixedCaseKey') then
          StoredKey := Key;
      AssertEquals('MixedCaseKey', StoredKey);
      ComponentMap.Remove('MixedCaseKey');
      AssertEquals(2, ComponentMap.Count);

      ComponentMap['c1'] := C3; // overwrite C1 with C3
      AssertEquals(2, ComponentMap.Count);
      AssertTrue(ComponentMap['C1'] = C3);

      ComponentMap.Remove('C1');
      AssertEquals(1, ComponentMap.Count);
      AssertFalse(ComponentMap.ContainsKey('C1'));
      AssertFalse(ComponentMap.ContainsKey('c1'));
      AssertTrue(ComponentMap.ContainsKey('C2'));
    finally FreeAndNil(ComponentMap) end;
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
    FreeAndNil(C3);
  end;
end;

initialization
  RegisterTest(TTestCastleClassUtils);
end.
