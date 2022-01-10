// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleXMLUtils" -*-
{
  Copyright 2014-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleXMLUtils unit. }
unit TestCastleXMLUtils;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleXMLUtils = class(TCastleTestCase)
  published
    procedure TestReadResult;
    procedure TestAttributeReading;
  end;

implementation

uses DOM, CastleXMLUtils, CastleFilesUtils, CastleDownload;

procedure TTestCastleXMLUtils.TestReadResult;
var
  Doc: TXMLDocument;
begin
  { FPC's ReadXMLFile, and (consistenly) URLReadXML, have a non-standard
    way of initializing the 1st Doc parameter: it is always initialized,
    initially to nil,
    and may be non-nil even in case of exceptions (check: are you sure
    this can actually happen with ReadXMLFile implementation?).
    So always free it, do not depend on it being freed in case of trouble.

    The previous Doc value should be always overridden. }
  try
    Doc := TXMLDocument(Pointer(123));
    try
      URLReadXML(Doc, 'castle-data:/not-existing-test.xml');
      Fail('Should not reach here, non-existing-test.xml should not exist');
    finally AssertTrue(Doc = nil); end;
  except on EDownloadError do begin { this is Ok } end; end;
end;

procedure TTestCastleXMLUtils.TestAttributeReading;
var
  Doc: TXMLDocument;
  S: string;
  I: Integer;
begin
  try
    URLReadXML(Doc, 'castle-data:/test.xml');

    AssertTrue(Doc.DocumentElement.AttributeStringDef('some_string', 'blah') = 'some_string_value');
    //AssertTrue(Doc.DocumentElement.AttributeCardinalDef('some_int', 666) = 123);
    AssertTrue(Doc.DocumentElement.AttributeSingleDef('some_int', 666.0) = 123.0);
    AssertTrue(Doc.DocumentElement.AttributeFloatDef('some_int', 666.0) = 123.0);

    AssertTrue(Doc.DocumentElement.AttributeStringDef('some_string_not_existing', 'blah') = 'blah');
    //AssertTrue(Doc.DocumentElement.AttributeCardinalDef('some_int_not_existing', 666) = 666);
    AssertTrue(Doc.DocumentElement.AttributeSingleDef('some_int_not_existing', 666.0) = 666.0);
    AssertTrue(Doc.DocumentElement.AttributeFloatDef('some_int_not_existing', 666.0) = 666.0);

    AssertTrue(Doc.DocumentElement.AttributeString('some_string') = 'some_string_value');
    AssertTrue(Doc.DocumentElement.AttributeCardinal('some_int') = 123);
    AssertTrue(Doc.DocumentElement.AttributeSingle('some_int') = 123.0);
    AssertTrue(Doc.DocumentElement.AttributeFloat('some_int') = 123.0);

    try Doc.DocumentElement.AttributeString  ('some_string_not_existing'); AssertTrue(false); except on EDOMAttributeMissing do begin { good } end; end;
    try Doc.DocumentElement.AttributeCardinal('some_int_not_existing'   ); AssertTrue(false); except on EDOMAttributeMissing do begin { good } end; end;
    try Doc.DocumentElement.AttributeSingle  ('some_int_not_existing'   ); AssertTrue(false); except on EDOMAttributeMissing do begin { good } end; end;
    try Doc.DocumentElement.AttributeFloat   ('some_int_not_existing'   ); AssertTrue(false); except on EDOMAttributeMissing do begin { good } end; end;

    S := 'blah';
    AssertTrue(Doc.DocumentElement.AttributeString('some_string', S));
    AssertTrue(S = 'some_string_value');

    I := 456;
    AssertTrue(Doc.DocumentElement.AttributeInteger('some_int', I));
    AssertTrue(I = 123);

    S := 'good';
    AssertTrue(not Doc.DocumentElement.AttributeString('some_string_not_existing', S));
    AssertTrue(S = 'good');

    I := 456;
    AssertTrue(not Doc.DocumentElement.AttributeInteger('some_int_not_existing', I));
    AssertTrue(I = 456);
  finally FreeAndNil(Doc); end;
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleXMLUtils);
{$endif}
end.
