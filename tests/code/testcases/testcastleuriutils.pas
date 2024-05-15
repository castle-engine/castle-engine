// -*- compile-command: "./test_single_testcase.sh TTestUriUtils" -*-
{
  Copyright 2013-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleUriUtils unit. }
unit TestCastleUriUtils;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestUriUtils = class(TCastleTestCase)
    procedure TestUriProtocol;
    procedure TestAbsoluteUri;
    procedure TestUriToFilenameSafe;
    procedure TestPercentEncoding;
    procedure TestCombineUriEncoding;
    procedure TestUriDisplay;
    procedure TestRelativeURLWhitespace;
    procedure TestUriExists;
    procedure TestRelativeToCastleDataURL;
    procedure TestExtractUri;
    procedure TestDotfile;
    procedure TestDecodeBase64;
    procedure TestEncodeBase64;
    procedure TestMimeTypeHttpQuery;
  end;

implementation

uses Base64, UriParser,
  CastleUriUtils, CastleUtils, CastleClassUtils;

procedure TTestUriUtils.TestUriProtocol;
var
  Colon: Integer;
begin
  AssertTrue(UriProtocol('data:blah:foo') = 'data');
  AssertTrue(UriProtocolIs('data:blah:foo', 'data', Colon));
  AssertTrue(not UriProtocolIs('data:blah:foo', 'data1', Colon));
  AssertTrue(not UriProtocolIs('data:blah:foo', 'dat', Colon));
  AssertTrue(not UriProtocolIs('data', 'data', Colon));
  AssertTrue(not UriProtocolIs('', 'data', Colon));

  AssertTrue(UriProtocol('ecmascript:xyz') = 'ecmascript');
  AssertTrue(UriDeleteProtocol('ecmascript:xyz') = 'xyz');

  AssertTrue(UriProtocol('     ' + NL + '    ecmascript:xyz') = 'ecmascript');
  AssertTrue(UriDeleteProtocol('     ' + NL + '    ecmascript:xyz') = 'xyz');

  AssertTrue(UriProtocol('void main()' + NL + 'ecmascript:xyz') = '');
  AssertTrue(UriDeleteProtocol('void main()' + NL + 'ecmascript:xyz') = 'void main()' + NL + 'ecmascript:xyz');
end;

procedure TTestUriUtils.TestAbsoluteUri;
{$ifdef MSWINDOWS}
var
  Drive: string;
{$endif}
begin
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('file:///C:/foo.txt', AbsoluteUri('c:\foo.txt'));
  { Below ExpandFileName will change /foo.txt on Windows to add drive letter }
  Drive := ExtractFileDrive(GetCurrentDir);
  AssertFilenamesEqual('file:///' + Drive + '/foo.txt', AbsoluteUri('/foo.txt'));
  {$endif}

  {$ifdef UNIX}
  { Below ExpandFileName will add path on Unix, treating "c:"
    like a normal filename.

    Also, thanks to our AllowDirectorySeparators fix in CastleUtils,
    the backslash remains intact (with stupid default
    AllowDirectorySeparators backslash would get converted to slash
    by ExpandFileName). }
  AssertEquals(FilenameToUriSafe(InclPathDelim(GetCurrentDir) + 'c:\foo.txt'), AbsoluteUri('c:\foo.txt'));
  AssertEquals(InclPathDelim(GetCurrentDir) + 'c:\foo.txt', ExpandFileName('c:\foo.txt'));

  AssertEquals('file:///foo.txt', AbsoluteUri('/foo.txt'));
  {$endif}

  AssertFilenamesEqual(FilenameToUriSafe(InclPathDelim(GetCurrentDir) + 'foo.txt'), AbsoluteUri('foo.txt'));
  AssertEquals('http://foo', AbsoluteUri('http://foo'));
  AssertFilenamesEqual(FilenameToUriSafe(InclPathDelim(GetCurrentDir)), AbsoluteUri(''));
end;

procedure TTestUriUtils.TestUriToFilenameSafe;
var
  Temp: string;
begin
  { UriToFilename fails for Windows absolute filenames,
    but our UriToFilenameSafe works. }
  AssertTrue(not UriToFilename('c:\foo.txt', Temp));
  AssertEquals('c:\foo.txt', UriToFilenameSafe('c:\foo.txt'));
end;

procedure TTestUriUtils.TestPercentEncoding;
{$ifdef MSWINDOWS}
var
  FilenamePart: String;
  FilenamePartPercent: String;
  FilenamePartUnescaped: String;
  Filename: String;
  FilenameAsUri: String;
  FilenameFromUri: String;
{$endif}
begin
  { FilenameToUriSafe must percent-encode,
    UriToFilenameSafe must decode it back. }
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('file:///C:/foo%254d.txt', FilenameToUriSafe('c:\foo%4d.txt'));
  AssertFilenamesEqual('C:\fooM.txt', UriToFilenameSafe('file:///C:/foo%4d.txt'));
  AssertFilenamesEqual('C:\foo%.txt', UriToFilenameSafe('file:///C:/foo%25.txt'));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('file:///foo%254d.txt', FilenameToUriSafe('/foo%4d.txt'));
  AssertEquals('/fooM.txt', UriToFilenameSafe('file:///foo%4d.txt'));
  AssertEquals('/foo%.txt', UriToFilenameSafe('file:///foo%25.txt'));
  {$endif}

  { Always UriToFilenameSafe and FilenameToUriSafe should reverse each other. }
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('C:\foo%4d.txt', UriToFilenameSafe(FilenameToUriSafe('c:\foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///C:/foo%4d.txt', FilenameToUriSafe(UriToFilenameSafe('file:///C:/foo%4d.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside Uri. }
  AssertFilenamesEqual('file:///C:/fooM.txt', FilenameToUriSafe(UriToFilenameSafe('file:///C:/foo%4d.txt')));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('/foo%4d.txt', UriToFilenameSafe(FilenameToUriSafe('/foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///foo%25.txt', FilenameToUriSafe(UriToFilenameSafe('file:///foo%25.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside Uri. }
  AssertEquals('file:///fooM.txt', FilenameToUriSafe(UriToFilenameSafe('file:///foo%4d.txt')));
  {$endif}

  {$ifdef MSWINDOWS}
  Filename := 'C:\Users\cge\AppData\Local\test_local_filename_chars\config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenameAsUri := FilenameToUriSafe(Filename);
  AssertEquals('file:///C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt', FilenameAsUri);
  FilenameFromUri := UriToFilenameSafe(FilenameAsUri);
  AssertEquals(Filename, FilenameFromUri);

  FilenamePart := 'C:/Users/cge/AppData/Local/test_local_filename_chars/config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenamePartPercent := InternalUriEscape(FilenamePart);
  AssertEquals('C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt', FilenamePartPercent);
  FilenamePartUnescaped := InternalUriUnescape(FilenamePartPercent);
  AssertEquals(FilenamePart, FilenamePartUnescaped);
  {$endif}
end;

procedure TTestUriUtils.TestCombineUriEncoding;
begin
  { Simple test without any % inside }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineUri('http:///game/sounds.xml', 'player_sudden_pain.wav'));

  { Special % inside must not be destroyed by CombineUri }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineUri('http:///game/sounds%25.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game%25/player_sudden_pain.wav', CombineUri('http:///game%25/sounds.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game/player%25_sudden_pain.wav', CombineUri('http:///game/sounds.xml', 'player%25_sudden_pain.wav'));
  AssertEquals('http:///game%25/player%25_sudden_pain.wav', CombineUri('http:///game%25/sounds.xml', 'player%25_sudden_pain.wav'));

  { It is Ok to convert non-special %xx sequences }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineUri('http:///game/sounds%4d.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///gameM/player_sudden_pain.wav', CombineUri('http:///game%4d/sounds.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game/playerM_sudden_pain.wav', CombineUri('http:///game/sounds.xml', 'player%4d_sudden_pain.wav'));
  AssertEquals('http:///gameM/playerM_sudden_pain.wav', CombineUri('http:///game%4d/sounds.xml', 'player%4d_sudden_pain.wav'));
end;

procedure TTestUriUtils.TestUriDisplay;
const
  DataUriX3D =
    'data:model/x3d+xml,<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.0//EN" "http://www.web3d.org/specifications/x3d-3.0.dtd">' + LineEnding +
    '<X3D version="3.0" profile="Immersive" xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance" xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-3.0.xsd">' + LineEnding +
    '<head>' + LineEnding +
    '</head>' + LineEnding +
    '<Scene>' + LineEnding +
    '  <Transform translation="0 -8 0">' + LineEnding +
    '    <Shape>' + LineEnding +
    '      <Text string=''"X3D (XML) model inlined using data Uri"'' />' + LineEnding +
    '    </Shape>' + LineEnding +
    '  </Transform>' + LineEnding +
    '</Scene>' + LineEnding +
    '</X3D>' + LineEnding +
    '';
  ScriptUri =
    '' + LineEnding +
    'ecmascript:' + LineEnding +
    'blah blah' + LineEnding +
    '';
  ScriptUri2 =
    '' + LineEnding +
    'ecmascript:' + LineEnding +
    '';

begin
  AssertEquals('http:///blah/player_sudden_pain.wav#blabla', UriDisplay('http:///blah/player_sudden_pain.wav#blabla'));
  AssertEquals('file:///blah/player_sudden_pain.wav#blabla', UriDisplay('file:///blah/player_sudden_pain.wav#blabla'));
  AssertEquals('blah/player_sudden_pain.wav#blabla', UriDisplay('blah/player_sudden_pain.wav#blabla'));
  AssertEquals('data:model/x3d+xml,...', UriDisplay(DataUriX3D));
  AssertEquals('ecmascript:...', UriDisplay(ScriptUri));
  AssertEquals('ecmascript:', UriDisplay(ScriptUri2));

  AssertEquals('', UriDisplay('', false));
  AssertEquals('', UriDisplay('', true));
  AssertEquals('', UriCaption(''));
end;

procedure TTestUriUtils.TestRelativeURLWhitespace;
begin
  AssertEquals('   castlescript: blablah', CombineUri('http:///foo/bar', '   castlescript: blablah'));
  AssertEquals(NL + 'castlescript: blablah', CombineUri('http:///foo/bar', NL + 'castlescript: blablah'));

  AssertEquals('   ecmascript: blablah', CombineUri('http:///foo/bar', '   ecmascript: blablah'));
  AssertEquals(NL + 'ecmascript: blablah', CombineUri('http:///foo/bar', NL + 'ecmascript: blablah'));
  AssertEquals(NL + '  ecmascript: blablah', CombineUri('http:///foo/bar', NL + '  ecmascript: blablah'));
end;

procedure TTestUriUtils.TestUriExists;
begin
  AssertTrue(ueUnknown = UriExists('http:/whatever'));
  AssertTrue(ueUnknown = UriExists('unknown-protocol:/whatecer'));

  AssertTrue(ueNotExists = UriExists('castle-data:/not_existing'));
  AssertTrue(ueFile = UriExists('castle-data:/game/level.xml'));
  AssertTrue(ueDirectory = UriExists('castle-data:/game/'));
  AssertTrue(ueDirectory = UriExists('castle-data:/game'));
  AssertTrue(ueDirectory = UriExists('castle-data:/'));

  AssertTrue(ueNotExists = UriExists(ResolveCastleDataURL('castle-data:/not_existing')));
  AssertTrue(ueFile = UriExists(ResolveCastleDataURL('castle-data:/game/level.xml')));
  AssertTrue(ueDirectory = UriExists(ResolveCastleDataURL('castle-data:/game/')));
  AssertTrue(ueDirectory = UriExists(ResolveCastleDataURL('castle-data:/game')));
  AssertTrue(ueDirectory = UriExists(ResolveCastleDataURL('castle-data:/')));

  AssertTrue(ueFile = UriExists('data:model/vrml,#VRML V2.0 utf8' + NL +
    'Transform {' + NL +
    '  translation 0 -2 0' + NL +
    '  children Shape {' + NL +
    '    geometry Text { string "VRML 2.0 model inlined using data URI" }' + NL +
    '  }' + NL +
    '}'));
end;

procedure TTestUriUtils.TestRelativeToCastleDataURL;
var
  WasInsideData: Boolean;
begin
  AssertEquals('foo/bar.txt', RelativeToCastleDataURL('castle-data:/foo/bar.txt', WasInsideData));
  AssertTrue(WasInsideData);
  AssertEquals('bar.txt', RelativeToCastleDataURL('castle-data:/bar.txt', WasInsideData));
  AssertTrue(WasInsideData);
  AssertEquals('foo/bar.txt', RelativeToCastleDataURL(ResolveCastleDataURL('castle-data:/foo/bar.txt'), WasInsideData));
  AssertTrue(WasInsideData);
  AssertEquals('bar.txt', RelativeToCastleDataURL(ResolveCastleDataURL('castle-data:/bar.txt'), WasInsideData));
  AssertTrue(WasInsideData);
  AssertEquals('http://example.com/bar.txt', RelativeToCastleDataURL('http://example.com/bar.txt', WasInsideData));
  AssertFalse(WasInsideData);
  AssertEquals('/something-not-in-root/bar.txt', RelativeToCastleDataURL('/something-not-in-root/bar.txt', WasInsideData));
  AssertFalse(WasInsideData);
end;

procedure TTestUriUtils.TestExtractUri;
begin
  // Test with valid Spine URL, ending with anim-naming:strict-underscore
  AssertEquals('castle-data:/starling/', ExtractUriPath('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('character_zombie_atlas.starling-xml', ExtractUriName('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.starling-xml', UriDeleteAnchor('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.new-extension#fps:8,anim-naming:strict-underscore', ChangeUriExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore', '.new-extension'));
  AssertEquals('castle-data:/starling/character_zombie_atlas#fps:8,anim-naming:strict-underscore', DeleteUriExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));

  // Test also invalid Spine URL, ending with anim-naming:character_zombie_atlas.png
  AssertEquals('castle-data:/starling/', ExtractUriPath('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('character_zombie_atlas.starling-xml', ExtractUriName('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.starling-xml', UriDeleteAnchor('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.new-extension#fps:8,anim-naming:character_zombie_atlas.png', ChangeUriExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png', '.new-extension'));
  AssertEquals('castle-data:/starling/character_zombie_atlas#fps:8,anim-naming:character_zombie_atlas.png', DeleteUriExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));

  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/', ExtractUriPath('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('hawaii_exo.json', ExtractUriName('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json', UriDeleteAnchor('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.new-extension#skin:default', ChangeUriExt('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default', '.new-extension'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo#skin:default', DeleteUriExt('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));

  AssertEquals('armor.tga', ExtractUriName('armor.tga'));
  AssertEquals('armor.tga', ExtractUriName('/armor.tga'));
  AssertEquals('armor.tga', ExtractUriName('blabla/armor.tga'));
end;

procedure TTestUriUtils.TestDotfile;
begin
  { Test that dot on 1st place is not treated as extension start. }

  AssertEquals('.hidden', ExtractUriName('https://blah/.hidden'));
  AssertEquals('.hidden', ExtractUriName('castle-data:/.hidden'));
  AssertEquals('.hidden.x3d', ExtractUriName('https://blah/.hidden.x3d'));
  AssertEquals('.hidden.x3d', ExtractUriName('castle-data:/.hidden.x3d'));

  AssertEquals('https://blah/.hidden', DeleteUriExt('https://blah/.hidden'));
  AssertEquals('castle-data:/.hidden', DeleteUriExt('castle-data:/.hidden'));
  AssertEquals('https://blah/.hidden', DeleteUriExt('https://blah/.hidden.x3d'));
  AssertEquals('castle-data:/.hidden', DeleteUriExt('castle-data:/.hidden.x3d'));
  AssertEquals('https://blah/.hidden.x3d', DeleteUriExt('https://blah/.hidden.x3d.gz'));
  AssertEquals('castle-data:/.hidden.x3d', DeleteUriExt('castle-data:/.hidden.x3d.gz'));
end;

procedure TTestUriUtils.TestDecodeBase64;
var
  Source: TStream;
  Decode: TBase64DecodingStream;
  DecodeStr: String;
begin
  // sample from https://en.wikipedia.org/wiki/Base64

  { TODO: Why both versions with TStringStream fail on Delphi?
    TStringStream (esp 2nd variant) should encode in UTF-16, which should
    be what our TBase64DecodingStream expects (as shown by the fact that
    MemoryStreamLoadFromDefaultString is OK). }
  //Source := TStringStream.Create('TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu');
  //Source := TStringStream.Create('TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu', TEncoding.Unicode, false);

  Source := MemoryStreamLoadFromDefaultString('TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu');
  try
    Assert(Source.Size = SizeOf(Char) * Length('TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu'));
    Decode := TBase64DecodingStream.Create(Source, bdmMIME);
    try
      { The input from https://en.wikipedia.org/wiki/Base64 contains 8-bit
        (ASCII) string encoded, which is why ReadGrowingStreamToString (AnsiString)
        makes sense.
        See TBase64DecodingStream about encodings. }
      DecodeStr :=
        //ReadGrowingStreamToDefaultString
        ReadGrowingStreamToString
        (Decode);
      AssertEquals('Many hands make light work.', DecodeStr);
    finally FreeAndNil(Decode) end;
  finally FreeAndNil(Source) end;
end;

procedure TTestUriUtils.TestEncodeBase64;
var
  S: TStringStream;
  Encode: TBase64EncodingStream;
  EncodeStr: String;
begin
  // sample from https://en.wikipedia.org/wiki/Base64
  S := TStringStream.Create;
  try
    Encode := TBase64EncodingStream.Create(S);
    try
      { The sample from https://en.wikipedia.org/wiki/Base64 contains 8-bit
        (ASCII) string encoded, which is why feeding here using WriteStr
        (this always converts chars to 8-bit, i.e. AnsiString)
        makes sense.
        See TBase64EncodingStream about encodings. }
      WriteStr(Encode, 'Many hands make light work.');
    finally FreeAndNil(Encode) end;
    EncodeStr := S.DataString;
    AssertEquals('TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu', EncodeStr);
  finally FreeAndNil(S) end;
end;

procedure TTestUriUtils.TestMimeTypeHttpQuery;
begin
  { See https://github.com/castle-engine/castle-engine/issues/547
    and TUrlFile.MimeTypeStripHttpQuery }

  // no protocol
  AssertEquals('text/plain', UriMimeType('foo.pas'));
  AssertEquals('text/plain', UriMimeType('foo.pas?query-is-stripped-for-all-protocols'));

  // file protocol
  AssertEquals('text/plain', UriMimeType('file:///foo.pas'));
  AssertEquals('text/plain', UriMimeType('file:///foo.pas?query-is-stripped-for-all-protocols'));

  // http protocol
  AssertEquals('text/plain', UriMimeType('http://example.com/foo.pas'));
  AssertEquals('text/plain', UriMimeType('http://example.com/foo.pas?some-query'));
  AssertEquals('text/plain', UriMimeType('http://example.com/foo.pas?some-query=123'));

  // https protocol
  AssertEquals('text/plain', UriMimeType('https://example.com/foo.pas'));
  AssertEquals('text/plain', UriMimeType('https://example.com/foo.pas?some-query'));
  AssertEquals('text/plain', UriMimeType('https://example.com/foo.pas?some-query=123'));
  AssertEquals('text/plain', UriMimeType('https://example.com/foo.pas?some-query=123#anchor'));

  // glTF mime type

  // no protocol
  AssertEquals('model/gltf+json', UriMimeType('foo.gltf'));
  AssertEquals('model/gltf+json', UriMimeType('foo.gltf?query-is-stripped-for-all-protocols'));
  AssertEquals('model/gltf+json', UriMimeType('foo.gltf?query-is-stripped-for-all-protocols#anchor'));
  AssertEquals('model/gltf+json', UriMimeType('foo.gltf#anchor'));

  // file protocol
  AssertEquals('model/gltf+json', UriMimeType('file:///foo.gltf'));
  AssertEquals('model/gltf+json', UriMimeType('file:///foo.gltf?query-is-stripped-for-all-protocols'));

  // http protocol
  AssertEquals('model/gltf+json', UriMimeType('http://example.com/foo.gltf'));
  AssertEquals('model/gltf+json', UriMimeType('http://example.com/foo.gltf?some-query'));
  AssertEquals('model/gltf+json', UriMimeType('http://example.com/foo.gltf?some-query=123'));

  // https protocol
  AssertEquals('model/gltf+json', UriMimeType('https://example.com/foo.gltf'));
  AssertEquals('model/gltf+json', UriMimeType('https://example.com/foo.gltf?some-query'));
  AssertEquals('model/gltf+json', UriMimeType('https://example.com/foo.gltf?some-query=123'));
  AssertEquals('model/gltf+json', UriMimeType('https://example.com/foo.gltf?some-query=123#anchor'));

  // test exactly from https://github.com/castle-engine/castle-engine/issues/547
  AssertEquals('image/jpeg', UriMimeType('https://cards.scryfall.io/large/front/0/9/092c48bd-b648-4c9e-aa99-cac3c407911d.jpg?1692936576'));
end;

initialization
  RegisterTest(TTestUriUtils);
end.
