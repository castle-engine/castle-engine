// -*- compile-command: "./test_single_testcase.sh TTestFpJson" -*-
{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test FpJson (and friends: JsonScanner, JsonReader, JsonParser).
  With FPC we use the FpJson from FPC.
  With Delphi we use our own copy, in src/compatibility/delphi-only/fcl-json/ . }
unit TestFpJson;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestFpJson = class(TCastleTestCase)
  published
    procedure TestUtf8Escapes;
  end;

implementation

uses FpJson, JsonParser, JsonScanner,
  CastleDownload, CastleUnicode, CastleTestUtils;

procedure TTestFpJson.TestUtf8Escapes;

{ Test that FpJson reads JSON as UTF-8, both when the file has literal UTF-8
  characters and when it uses \uXXXX escape sequences.

  The tested data file contains the same text 4 times:
  - as a string value, written literally (as UTF-8 bytes),
  - as a string value, written using \uXXXX escapes
    (so that part of the file is pure ASCII),
  - as an object member name, written literally,
  - as an object member name, written using \uXXXX escapes.
  All 4 must result in exactly the CastleTestUtils.SampleText,
  which has the expected characters hardcoded (as UTF-8 bytes)
  in the Pascal source.

  Note: The sample text ends with a character outside of BMP
  (U+1F600, written as a surrogate pair in JSON),
  as this is the most tricky case. }

var
  Stream: TStream;
  { To also verify the hardcoded expected value in CastleTestUtils. }
  SampleUtf8: Utf8String;
  JsonParser: TJsonParser;
  Json: TJsonData;
  JsonObject: TJsonObject;
  LiteralValue, EscapedValue, LiteralName, EscapedName: String;
begin
  { Sanity checks of the hardcoded expected value itself:
    it is 12 bytes of UTF-8, which decode to 5 Unicode characters. }
  SampleUtf8 := SampleTextUtf8;
  AssertEquals(Length(SampleTextUtf8Bytes), Length(SampleUtf8));
  AssertEquals(SampleTextLength, StringLength(SampleText));

  Stream := Download('castle-data:/json/utf8_escapes.json');
  try
    { Note: We pass joUTF8, just like our Spine and IFC loading do. }
    JsonParser := TJsonParser.Create(Stream, [joUTF8]);
    try
      Json := JsonParser.Parse;
      try
        AssertTrue(Json is TJsonObject);
        JsonObject := TJsonObject(Json);

        LiteralValue := JsonObject.Get('literalValue', '');
        EscapedValue := JsonObject.Get('escapedValue', '');

        { Member names of the 2 nested objects. }
        AssertEquals(1, JsonObject.Objects['literalName'].Count);
        AssertEquals(1, JsonObject.Objects['escapedName'].Count);
        LiteralName := JsonObject.Objects['literalName'].Names[0];
        EscapedName := JsonObject.Objects['escapedName'].Names[0];

        { All 4 forms must result in the expected characters. }
        AssertEquals(SampleText, LiteralValue);
        AssertEquals(SampleText, EscapedValue);
        AssertEquals(SampleText, LiteralName);
        AssertEquals(SampleText, EscapedName);
      finally FreeAndNil(Json) end;
    finally FreeAndNil(JsonParser) end;
  finally FreeAndNil(Stream) end;
end;

initialization
  RegisterTest(TTestFpJson);
end.
