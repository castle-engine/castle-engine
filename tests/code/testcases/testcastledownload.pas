// -*- compile-command: "./test_single_testcase.sh TTestDownload" -*-
{
  Copyright 2020-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleDownload unit. }
unit TestCastleDownload;

{ Needed for CASTLE_DEFORMAT_BUGGY }
{$I ../../../src/common_includes/castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestDownload = class(TCastleTestCase)
  published
    procedure TestLocalCharsCastleData;
    procedure TestLocalCharsContents;
    procedure TestLocalCharsCastleConfig;
    procedure TestCastleTextReader;
    procedure TestCastleTextReaderWriterUtf8;
    procedure TestCastleTextReaderWriterUtf8_Read;
    procedure TestCastleTextReaderWriterUtf8_Config;
    procedure TestRegisteredProtocolNotCaseSensitive;
  end;

implementation

uses CastleDownload, CastleClassUtils, CastleVectors, CastleStringUtils,
  CastleFonts, CastleFilesUtils, CastleUriUtils, CastleUtils, CastleUnicode;

procedure TTestDownload.TestLocalCharsCastleData;

  { Test reading file using URL (through CGE function). }
  procedure TestReading(const URL: String);
  var
    Stream: TStream;
    S: String;
  begin
    Stream := Download(URL);
    try
      S := StreamToString(Stream);
      AssertEquals('Testing.', Trim(S));
    finally FreeAndNil(Stream) end;
  end;

  { Test reading file, whose URL is written inside another file, in UTF-8 encoding. }
  procedure TestReadingThroughReference(const URL: String);
  var
    Stream: TStream;
    ReferredURL: String;
  begin
    Stream := Download(URL);
    try
      ReferredURL := Trim(StreamToString(Stream));
      TestReading(ReferredURL);
    finally FreeAndNil(Stream) end;
  end;

  { Test reading font (as it goes through FreeType library). }
  procedure TestReadingFont(const FontUrl: String);
  var
    MyNewFont: TCastleFont;
  begin
    MyNewFont := TCastleFont.Create(nil);
    try
      MyNewFont.Size := 20;
      MyNewFont.AntiAliased := true;
      MyNewFont.Url := FontUrl;
    finally FreeAndNil(MyNewFont) end;
  end;

begin
  TestReading('castle-data:/local_chars/ascii_name.txt');
  TestReading('castle-data:/local_chars/' + UrlEncode('name with Polish chars ćma źrebak żmija wąż królik.txt'));
  TestReading('castle-data:/local_chars/' + UrlEncode('name with Chinese chars 样例中文文本.txt'));
  TestReading('castle-data:/local_chars/' + UrlEncode('样例中文文本/name with Chinese chars 样例中文文本.txt'));
  TestReading('castle-data:/local_chars/' + UrlEncode('name with Russian chars образец русского текста.txt'));
  TestReading('castle-data:/local_chars/' + UrlEncode('образец русского текста/name with Russian chars образец русского текста.txt'));

  // Not really correct URLs, as space should be encoded as %20 etc., but we handle them too
  TestReading('castle-data:/local_chars/name with Polish chars ćma źrebak żmija wąż królik.txt');
  TestReading('castle-data:/local_chars/name with Chinese chars 样例中文文本.txt');
  TestReading('castle-data:/local_chars/样例中文文本/name with Chinese chars 样例中文文本.txt');
  TestReading('castle-data:/local_chars/name with Russian chars образец русского текста.txt');
  TestReading('castle-data:/local_chars/образец русского текста/name with Russian chars образец русского текста.txt');

  TestReadingThroughReference('castle-data:/' + UrlEncode('local_chars/reference to file with Chinese chars.txt'));
  TestReadingThroughReference('castle-data:/' + UrlEncode('local_chars/reference to file with Russian chars.txt'));
  TestReadingThroughReference('castle-data:/' + UrlEncode('local_chars/reference to file with Polish chars.txt'));

  // Not really correct URLs, as space should be encoded as %20 etc., but we handle them too
  TestReadingThroughReference('castle-data:/local_chars/reference to file with Chinese chars.txt');
  TestReadingThroughReference('castle-data:/local_chars/reference to file with Russian chars.txt');
  TestReadingThroughReference('castle-data:/local_chars/reference to file with Polish chars.txt');

  TestReadingFont('castle-data:/' + UrlEncode('local_chars/DejaVuSans name with Russian chars образец русского текста.ttf'));

  // Not really correct URLs, as space should be encoded as %20 etc., but we handle them too
  TestReadingFont('castle-data:/local_chars/DejaVuSans name with Russian chars образец русского текста.ttf');
end;

procedure TTestDownload.TestLocalCharsContents;

{ Test that reading and writing file contents with non-ASCII characters works.

  This is not about non-ASCII characters in file names (see
  TestLocalCharsCastleData and TestLocalCharsCastleConfig for this)
  but about non-ASCII characters inside the file.

  We assume UTF-8 in all text files, so the routines that read / write
  file contents as 8-bit strings (FileToString, StreamToString,
  MemoryStreamLoadFromString) use Utf8String, not AnsiString.
  So they work correctly also when AnsiString has some other, system-specific,
  encoding, which happens when CASTLE_ANSISTRING_UNCHANGED is defined. }

const
  ChineseUrl = 'castle-data:/local_chars/name with Chinese chars 样例中文文本.txt';
  ChineseText = 'Some text with Chinese chars 样例中文文本.';
var
  Stream: TStream;
  ReferenceUrl: String;
begin
  // test FileToString
  AssertEquals(ChineseUrl, Trim(FileToString(
    'castle-data:/local_chars/reference to file with Chinese chars.txt')));

  // test Download + StreamToString
  Stream := Download('castle-data:/local_chars/reference to file with Chinese chars.txt');
  try
    AssertEquals(ChineseUrl, Trim(StreamToString(Stream)));
  finally FreeAndNil(Stream) end;

  // test MemoryStreamLoadFromString, a symmetric counterpart of StreamToString
  Stream := MemoryStreamLoadFromString(ChineseText);
  try
    // 29 ASCII + 6 Chinese chars (3 bytes each in UTF-8) + 1 = 48 bytes
    AssertEquals(29 + 6 * 3 + 1, Stream.Size);
    AssertEquals(ChineseText, StreamToString(Stream));
  finally FreeAndNil(Stream) end;

  if not CanUseCastleConfig then
  begin
    AbortTest;
    Exit;
  end;

  // test StringToFile
  ReferenceUrl := 'castle-config:/' + UrlEncode('reference with Chinese chars.txt');
  StringToFile(ReferenceUrl, ChineseText);
  AssertEquals(ChineseText, FileToString(ReferenceUrl));
end;

procedure TTestDownload.TestLocalCharsCastleConfig;
begin
  if not CanUseCastleConfig then
  begin
    AbortTest;
    Exit;
  end;

  StringToFile('castle-config:/' + UrlEncode('config_ascii.txt'), 'Testing save.');
  StringToFile('castle-config:/' + UrlEncode('config with Chinese chars 样例中文文本.txt'), 'Testing save.');
  StringToFile('castle-config:/' + UrlEncode('config with Polish chars ćma źrebak żmija wąż królik.txt'), 'Testing save.');
  StringToFile('castle-config:/' + UrlEncode('config with Russian chars образец русского текста.txt'), 'Testing save.');

  // Not really correct URLs, as space should be encoded as %20 etc., but we handle them too
  StringToFile('castle-config:/2_config with Chinese chars 样例中文文本.txt', 'Testing save.');
  StringToFile('castle-config:/2_config with Polish chars ćma źrebak żmija wąż królik.txt', 'Testing save.');
  StringToFile('castle-config:/2_config with Russian chars образец русского текста.txt', 'Testing save.');
end;

procedure TTestDownload.TestCastleTextReader;

{ Testcase based on example from
  https://forum.castle-engine.io/t/setup-files-and-working-with-them/630/4
}

var
  T: TCastleTextReader;
  X, Y, Z: Single;
  V: TVector3;
begin
  // Workaround another issue with macOS/Aarch64 with FPC 3.2.3
  {$if defined(DARWIN) and defined(CPUAARCH64)}
  AbortTest;
  Exit;
  {$endif}

  { using ReadSingle }
  T := TCastleTextReader.Create('castle-data:/test_text_reader.txt');
  try
    X := T.ReadSingle;
    Y := T.ReadSingle;
    Z := T.ReadSingle;
    AssertSameValue(1, X);
    AssertSameValue(2, Y);
    AssertSameValue(3, Z);

    X := T.ReadSingle;
    Y := T.ReadSingle;
    Z := T.ReadSingle;
    AssertSameValue(4, X);
    AssertSameValue(5, Y);
    AssertSameValue(6, Z);

    X := T.ReadSingle;
    Y := T.ReadSingle;
    Z := T.ReadSingle;
    AssertSameValue(7, X);
    AssertSameValue(8, Y);
    AssertSameValue(9, Z);
  finally FreeAndNil(T) end;

  { alternative version using ReadVector3 }
  T := TCastleTextReader.Create('castle-data:/test_text_reader.txt');
  try
    V := T.ReadVector3;
    AssertVectorEquals(Vector3(1, 2, 3), V);
    V := T.ReadVector3;
    AssertVectorEquals(Vector3(4, 5, 6), V);
    V := T.ReadVector3;
    AssertVectorEquals(Vector3(7, 8, 9), V);
  finally FreeAndNil(T) end;

  { alternative version using Readln + Vector3FromStr }
  T := TCastleTextReader.Create('castle-data:/test_text_reader.txt');
  try
    V := Vector3FromStr(T.Readln);
    AssertVectorEquals(Vector3(1, 2, 3), V);
    V := Vector3FromStr(T.Readln);
    AssertVectorEquals(Vector3(4, 5, 6), V);
    V := Vector3FromStr(T.Readln);
    AssertVectorEquals(Vector3(7, 8, 9), V);
  finally FreeAndNil(T) end;

  {$ifdef CASTLE_DEFORMAT_BUGGY}
  AbortTest;
  Exit;
  {$else}
  { alternative version using Readln + DeFormat }
  T := TCastleTextReader.Create('castle-data:/test_text_reader.txt');
  try
    DeFormat(T.Readln, '%.single. %.single. %.single.', [@X, @Y, @Z]);
    AssertSameValue(1, X);
    AssertSameValue(2, Y);
    AssertSameValue(3, Z);

    DeFormat(T.Readln, '%.single. %.single. %.single.', [@X, @Y, @Z]);
    AssertSameValue(4, X);
    AssertSameValue(5, Y);
    AssertSameValue(6, Z);

    DeFormat(T.Readln, '%.single. %.single. %.single.', [@X, @Y, @Z]);
    AssertSameValue(7, X);
    AssertSameValue(8, Y);
    AssertSameValue(9, Z);
  finally FreeAndNil(T) end;
  {$endif}
end;

const
  { 29 ASCII characters, 6 Chinese characters (3 bytes each in UTF-8),
    1 dot -> 36 characters, 48 bytes in UTF-8. }
  ChineseText = 'Some text with Chinese chars 样例中文文本.';
  ChineseTextBytes = 29 + 6 * 3 + 1;
  ChineseTextChars = 29 + 6 + 1;

  AsciiLine = 'ASCII line';
  TextSeparator = ' and ';

procedure TTestDownload.TestCastleTextReaderWriterUtf8;

{ Test that TCastleTextWriter writes UTF-8, and TCastleTextReader reads UTF-8,
  regardless of what encoding AnsiString happens to have.

  This matters when CASTLE_ANSISTRING_UNCHANGED is defined, when
  AnsiString has platform-specific encoding. That's why TCastleTextReader
  buffers the read data in Utf8String, not AnsiString.
  See ../../../doc/miscellaneous_notes/ansistring_encoding.md .

  We also check the round-trip with other CGE routines that read / write
  text file contents (StreamToString, FileToString), to make sure they all
  agree that text files are UTF-8. }

var
  Stream: TMemoryStream;
  Writer: TCastleTextWriter;
  Reader: TCastleTextReader;
  { Note: Pass this variable, not the ChineseText constant, to "array of const"
    (i.e. to Format and Writeln with Args). FPC 3.2.2 encodes an untyped
    string constant put in "array of const" in a way that loses the non-ASCII
    characters. Passing a typed String is reliable with both FPC and Delphi. }
  ChineseTextVar: String;
begin
  ChineseTextVar := ChineseText;

  { Sanity check: the constants above indeed describe our test text. }
  AssertEquals(ChineseTextChars, StringLength(ChineseText));
  AssertEquals(ChineseTextBytes, Length(Utf8String(ChineseText)));

  { Write with TCastleTextWriter, read back with TCastleTextReader
    and StreamToString. }
  Stream := TMemoryStream.Create;
  try
    Writer := TCastleTextWriter.Create(Stream, false);
    try
      Writer.Writeln(ChineseText);
      Writer.Writeln(AsciiLine);
      Writer.Writeln('%s' + TextSeparator + '%s', [ChineseTextVar, ChineseTextVar]);
      Writer.Write(ChineseText);
    finally FreeAndNil(Writer) end;

    { The stream contains UTF-8, so its size is expressed in UTF-8 bytes. }
    AssertEquals(
      ChineseTextBytes + Length(NL) +
      Length(AsciiLine) + Length(NL) +
      ChineseTextBytes + Length(TextSeparator) + ChineseTextBytes + Length(NL) +
      ChineseTextBytes,
      Stream.Size);

    { StreamToString decodes the same UTF-8 back. }
    AssertEquals(
      ChineseText + NL +
      AsciiLine + NL +
      ChineseText + TextSeparator + ChineseText + NL +
      ChineseText,
      StreamToString(Stream));

    Stream.Position := 0;
    Reader := TCastleTextReader.Create(Stream, false);
    try
      AssertEquals(ChineseText, Reader.Readln);
      AssertEquals(AsciiLine, Reader.Readln);
      AssertEquals(ChineseText + TextSeparator + ChineseText, Reader.Readln);
      AssertFalse(Reader.Eof);
      AssertEquals(ChineseText, Reader.Readln);
      AssertTrue(Reader.Eof);
    finally FreeAndNil(Reader) end;
  finally FreeAndNil(Stream) end;
end;

procedure TTestDownload.TestCastleTextReaderWriterUtf8_Read;

{ Like TestCastleTextReaderWriterUtf8,
  but now test TCastleTextReader.Read (reads the next non-whitespace token).
  Internally it operates on byte indexes inside the UTF-8 buffer, so it is
  a good test that we don't mix byte and character indexes. }

const
  { 2 Chinese characters = 6 bytes, and 4 Chinese characters = 12 bytes. }
  Word1 = '样例';
  Word1Bytes = 2 * 3;
  Word2 = '中文文本';
  Word2Bytes = 4 * 3;
var
  Stream: TMemoryStream;
  Writer: TCastleTextWriter;
  Reader: TCastleTextReader;
begin
  Stream := TMemoryStream.Create;
  try
    Writer := TCastleTextWriter.Create(Stream, false);
    try
      Writer.Writeln('  ' + Word1 + '  ' + Word2 + ' ');
      Writer.Writeln(Word1);
    finally FreeAndNil(Writer) end;

    AssertEquals(
      2 + Word1Bytes + 2 + Word2Bytes + 1 + Length(NL) +
      Word1Bytes + Length(NL),
      Stream.Size);

    Stream.Position := 0;
    Reader := TCastleTextReader.Create(Stream, false);
    try
      AssertEquals(Word1, Reader.Read);
      AssertEquals(Word2, Reader.Read);
      AssertEquals(Word1, Reader.Read);
      { Returns empty string if (and only if) the stream ended. }
      AssertEquals('', Reader.Read);
    finally FreeAndNil(Reader) end;
  finally FreeAndNil(Stream) end;
end;

procedure TTestDownload.TestCastleTextReaderWriterUtf8_Config;
var
  Writer: TCastleTextWriter;
  Reader: TCastleTextReader;
  ConfigUrl: String;
begin
  if not CanUseCastleConfig then
  begin
    AbortTest;
    Exit;
  end;

  { Round-trip through an actual file: what TCastleTextWriter saved
    is readable by FileToString and TCastleTextReader. }
  ConfigUrl := 'castle-config:/' + UrlEncode('text writer with Chinese chars.txt');
  Writer := TCastleTextWriter.Create(ConfigUrl);
  try
    Writer.Writeln(ChineseText);
  finally FreeAndNil(Writer) end;

  AssertEquals(ChineseText, Trim(FileToString(ConfigUrl)));

  Reader := TCastleTextReader.Create(ConfigUrl);
  try
    AssertEquals(ChineseText, Reader.Readln);
  finally FreeAndNil(Reader) end;

  { And the reverse: what StringToFile saved is readable by TCastleTextReader. }
  StringToFile(ConfigUrl, ChineseText + NL);
  Reader := TCastleTextReader.Create(ConfigUrl);
  try
    AssertEquals(ChineseText, Reader.Readln);
  finally FreeAndNil(Reader) end;
end;

procedure TTestDownload.TestRegisteredProtocolNotCaseSensitive;
begin
  AssertFalse(RegisteredUrlProtocol('my-test-proto'));
  AssertFalse(RegisteredUrlProtocol('MY-test-PROTO'));
  AssertFalse(RegisteredUrlProtocol('my-Test-proto'));

  RegisterUrlProtocol('my-Test-proto', nil, nil);

  AssertTrue(RegisteredUrlProtocol('my-test-proto'));
  AssertTrue(RegisteredUrlProtocol('MY-test-PROTO'));
  AssertTrue(RegisteredUrlProtocol('my-Test-proto'));

  UnregisterUrlProtocol('My-Test-protO'); // different case when registered, no problem

  AssertFalse(RegisteredUrlProtocol('my-test-proto'));
  AssertFalse(RegisteredUrlProtocol('MY-test-PROTO'));
  AssertFalse(RegisteredUrlProtocol('my-Test-proto'));
end;

initialization
  RegisterTest(TTestDownload);
end.
