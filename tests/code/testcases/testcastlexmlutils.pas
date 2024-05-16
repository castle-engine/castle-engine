// -*- compile-command: "./test_single_testcase.sh TTestCastleXmlUtils" -*-
{
  Copyright 2014-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleXmlUtils unit. }
unit TestCastleXmlUtils;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleXmlUtils = class(TCastleTestCase)
  published
    procedure TestReadResult;
    procedure TestAttributeReading;
  end;

implementation

uses DOM, CastleXmlUtils, CastleFilesUtils, CastleDownload, CastleColors;

procedure TTestCastleXmlUtils.TestReadResult;
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

procedure TTestCastleXmlUtils.TestAttributeReading;
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

    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_hex').X = White.X);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_hex').Y = White.Y);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_hex').Z = White.Z);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_hex').W = White.W);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_vector').X = White.X);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_vector').Y = White.Y);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_vector').Z = White.Z);
    AssertTrue(Doc.DocumentElement.AttributeColor('some_color_vector').W = White.W);

    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_hex').X = WhiteRgb.X);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_hex').Y = WhiteRgb.Y);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_hex').Z = WhiteRgb.Z);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_vector').X = WhiteRgb.X);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_vector').Y = WhiteRgb.Y);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('some_colorrgb_vector').Z = WhiteRgb.Z);

    Doc.DocumentElement.AttributeSet('color_white_vector', White);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_vector').X = White.X);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_vector').Y = White.Y);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_vector').Z = White.Z);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_vector').W = White.W);
    Doc.DocumentElement.AttributeColorSet('color_white_hex', White);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_hex').X = White.X);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_hex').Y = White.Y);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_hex').Z = White.Z);
    AssertTrue(Doc.DocumentElement.AttributeColor('color_white_hex').W = White.W);

    Doc.DocumentElement.AttributeSet('color_whitergb_vector', WhiteRGB);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_vector').X = WhiteRGB.X);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_vector').Y = WhiteRGB.Y);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_vector').Z = WhiteRGB.Z);
    Doc.DocumentElement.AttributeColorSet('color_whitergb_hex', WhiteRGB);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_hex').X = WhiteRGB.X);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_hex').Y = WhiteRGB.Y);
    AssertTrue(Doc.DocumentElement.AttributeColorRGB('color_whitergb_hex').Z = WhiteRGB.Z);

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

initialization
  RegisterTest(TTestCastleXmlUtils);
end.
