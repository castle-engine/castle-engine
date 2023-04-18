// -*- compile-command: "./test_single_testcase.sh TTestURIUtils" -*-
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

{ Test CastleURIUtils unit. }
unit TestCastleURIUtils;

interface

uses
  Classes, SysUtils{$ifndef CASTLE_TESTER}, FpcUnit, TestUtils, TestRegistry
  , CastleTestCase{$else}, CastleTester{$endif};

type
  TTestURIUtils = class(TCastleTestCase)
    procedure TestURIProtocol;
    procedure TestAbsoluteURI;
    procedure TestURIToFilenameSafe;
    procedure TestPercentEncoding;
    procedure TestCombineURIEncoding;
    procedure TestURIDisplay;
    procedure TestRelativeURLWhitespace;
    procedure TestURIExists;
    procedure TestRelativeToCastleDataURL;
    procedure TestExtractURI;
  end;

implementation

uses CastleURIUtils, URIParser, CastleUtils;

procedure TTestURIUtils.TestURIProtocol;
var
  Colon: Integer;
begin
  AssertTrue(URIProtocol('data:blah:foo') = 'data');
  AssertTrue(URIProtocolIs('data:blah:foo', 'data', Colon));
  AssertTrue(not URIProtocolIs('data:blah:foo', 'data1', Colon));
  AssertTrue(not URIProtocolIs('data:blah:foo', 'dat', Colon));
  AssertTrue(not URIProtocolIs('data', 'data', Colon));
  AssertTrue(not URIProtocolIs('', 'data', Colon));

  AssertTrue(URIProtocol('ecmascript:xyz') = 'ecmascript');
  AssertTrue(URIDeleteProtocol('ecmascript:xyz') = 'xyz');

  AssertTrue(URIProtocol('     ' + NL + '    ecmascript:xyz') = 'ecmascript');
  AssertTrue(URIDeleteProtocol('     ' + NL + '    ecmascript:xyz') = 'xyz');

  AssertTrue(URIProtocol('void main()' + NL + 'ecmascript:xyz') = '');
  AssertTrue(URIDeleteProtocol('void main()' + NL + 'ecmascript:xyz') = 'void main()' + NL + 'ecmascript:xyz');
end;

procedure TTestURIUtils.TestAbsoluteURI;
{$ifdef MSWINDOWS}
var
  Drive: string;
{$endif}
begin
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('file:///C:/foo.txt', AbsoluteURI('c:\foo.txt'));
  { Below ExpandFileName will change /foo.txt on Windows to add drive letter }
  Drive := ExtractFileDrive(GetCurrentDir);
  AssertFilenamesEqual('file:///' + Drive + '/foo.txt', AbsoluteURI('/foo.txt'));
  {$endif}

  {$ifdef UNIX}
  { Below ExpandFileName will add path on Unix, treating "c:"
    like a normal filename.

    Also, thanks to our AllowDirectorySeparators fix in CastleUtils,
    the backslash remains intact (with stupid default
    AllowDirectorySeparators backslash would get converted to slash
    by ExpandFileName). }
  AssertEquals(FilenameToURISafe(InclPathDelim(GetCurrentDir) + 'c:\foo.txt'), AbsoluteURI('c:\foo.txt'));
  AssertEquals(InclPathDelim(GetCurrentDir) + 'c:\foo.txt', ExpandFileName('c:\foo.txt'));

  AssertEquals('file:///foo.txt', AbsoluteURI('/foo.txt'));
  {$endif}

  AssertFilenamesEqual(FilenameToURISafe(InclPathDelim(GetCurrentDir) + 'foo.txt'), AbsoluteURI('foo.txt'));
  AssertEquals('http://foo', AbsoluteURI('http://foo'));
  AssertFilenamesEqual(FilenameToURISafe(InclPathDelim(GetCurrentDir)), AbsoluteURI(''));
end;

procedure TTestURIUtils.TestURIToFilenameSafe;
var
  Temp: string;
begin
  { URIToFilename fails for Windows absolute filenames,
    but our URIToFilenameSafe works. }
  AssertTrue(not URIToFilename('c:\foo.txt', Temp));
  AssertEquals('c:\foo.txt', URIToFilenameSafe('c:\foo.txt'));
end;

procedure TTestURIUtils.TestPercentEncoding;
{$ifdef MSWINDOWS}
var
  FilenamePart: String;
  FilenamePartPercent: String;
  FilenamePartUnescaped: String;
  Filename: String;
  FilenameAsUri: String;
  FilenameFromUri: String;
const
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];
{$endif}
begin
  { FilenameToURISafe must percent-encode,
    URIToFilenameSafe must decode it back. }
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('file:///C:/foo%254d.txt', FilenameToURISafe('c:\foo%4d.txt'));
  AssertFilenamesEqual('C:\fooM.txt', URIToFilenameSafe('file:///C:/foo%4d.txt'));
  AssertFilenamesEqual('C:\foo%.txt', URIToFilenameSafe('file:///C:/foo%25.txt'));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('file:///foo%254d.txt', FilenameToURISafe('/foo%4d.txt'));
  AssertEquals('/fooM.txt', URIToFilenameSafe('file:///foo%4d.txt'));
  AssertEquals('/foo%.txt', URIToFilenameSafe('file:///foo%25.txt'));
  {$endif}

  { Always URIToFilenameSafe and FilenameToURISafe should reverse each other. }
  {$ifdef MSWINDOWS}
  AssertFilenamesEqual('C:\foo%4d.txt', URIToFilenameSafe(FilenameToURISafe('c:\foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///C:/foo%4d.txt', FilenameToURISafe(URIToFilenameSafe('file:///C:/foo%4d.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertFilenamesEqual('file:///C:/fooM.txt', FilenameToURISafe(URIToFilenameSafe('file:///C:/foo%4d.txt')));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('/foo%4d.txt', URIToFilenameSafe(FilenameToURISafe('/foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///foo%25.txt', FilenameToURISafe(URIToFilenameSafe('file:///foo%25.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertEquals('file:///fooM.txt', FilenameToURISafe(URIToFilenameSafe('file:///foo%4d.txt')));
  {$endif}

  {$ifdef MSWINDOWS}
  Filename := 'C:\Users\cge\AppData\Local\test_local_filename_chars\config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenameAsUri := FilenameToURISafe(Filename);
  AssertEquals('file:///C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt', FilenameAsUri);
  FilenameFromUri := URIToFilenameSafe(FilenameAsUri);
  AssertEquals(Filename, FilenameFromUri);

  FilenamePart := 'C:/Users/cge/AppData/Local/test_local_filename_chars/config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenamePartPercent := InternalUriEscape(FilenamePart, ValidPathChars);
  AssertEquals('C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt', FilenamePartPercent);
  FilenamePartUnescaped := InternalUriUnescape(FilenamePartPercent);
  AssertEquals(FilenamePart, FilenamePartUnescaped);
  {$endif}
end;

procedure TTestURIUtils.TestCombineURIEncoding;
begin
  { Simple test without any % inside }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineURI('http:///game/sounds.xml', 'player_sudden_pain.wav'));

  { Special % inside must not be destroyed by CombineURI }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineURI('http:///game/sounds%25.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game%25/player_sudden_pain.wav', CombineURI('http:///game%25/sounds.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game/player%25_sudden_pain.wav', CombineURI('http:///game/sounds.xml', 'player%25_sudden_pain.wav'));
  AssertEquals('http:///game%25/player%25_sudden_pain.wav', CombineURI('http:///game%25/sounds.xml', 'player%25_sudden_pain.wav'));

  { It is Ok to convert non-special %xx sequences }
  AssertEquals('http:///game/player_sudden_pain.wav', CombineURI('http:///game/sounds%4d.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///gameM/player_sudden_pain.wav', CombineURI('http:///game%4d/sounds.xml', 'player_sudden_pain.wav'));
  AssertEquals('http:///game/playerM_sudden_pain.wav', CombineURI('http:///game/sounds.xml', 'player%4d_sudden_pain.wav'));
  AssertEquals('http:///gameM/playerM_sudden_pain.wav', CombineURI('http:///game%4d/sounds.xml', 'player%4d_sudden_pain.wav'));
end;

procedure TTestURIUtils.TestURIDisplay;
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
    '      <Text string=''"X3D (XML) model inlined using data URI"'' />' + LineEnding +
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
  AssertEquals('http:///blah/player_sudden_pain.wav#blabla', URIDisplay('http:///blah/player_sudden_pain.wav#blabla'));
  AssertEquals('file:///blah/player_sudden_pain.wav#blabla', URIDisplay('file:///blah/player_sudden_pain.wav#blabla'));
  AssertEquals('blah/player_sudden_pain.wav#blabla', URIDisplay('blah/player_sudden_pain.wav#blabla'));
  AssertEquals('data:model/x3d+xml,...', URIDisplay(DataUriX3D));
  AssertEquals('ecmascript:...', URIDisplay(ScriptUri));
  AssertEquals('ecmascript:', URIDisplay(ScriptUri2));

  AssertEquals('', URIDisplay('', false));
  AssertEquals('', URIDisplay('', true));
  AssertEquals('', URICaption(''));
end;

procedure TTestURIUtils.TestRelativeURLWhitespace;
begin
  AssertEquals('   castlescript: blablah', CombineURI('http:///foo/bar', '   castlescript: blablah'));
  AssertEquals(NL + 'castlescript: blablah', CombineURI('http:///foo/bar', NL + 'castlescript: blablah'));

  AssertEquals('   ecmascript: blablah', CombineURI('http:///foo/bar', '   ecmascript: blablah'));
  AssertEquals(NL + 'ecmascript: blablah', CombineURI('http:///foo/bar', NL + 'ecmascript: blablah'));
  AssertEquals(NL + '  ecmascript: blablah', CombineURI('http:///foo/bar', NL + '  ecmascript: blablah'));
end;

procedure TTestURIUtils.TestURIExists;
begin
  AssertTrue(ueUnknown = URIExists('http:/whatever'));
  AssertTrue(ueUnknown = URIExists('unknown-protocol:/whatecer'));

  AssertTrue(ueNotExists = URIExists('castle-data:/not_existing'));
  AssertTrue(ueFile = URIExists('castle-data:/game/level.xml'));
  AssertTrue(ueDirectory = URIExists('castle-data:/game/'));
  AssertTrue(ueDirectory = URIExists('castle-data:/game'));
  AssertTrue(ueDirectory = URIExists('castle-data:/'));

  AssertTrue(ueNotExists = URIExists(ResolveCastleDataURL('castle-data:/not_existing')));
  AssertTrue(ueFile = URIExists(ResolveCastleDataURL('castle-data:/game/level.xml')));
  AssertTrue(ueDirectory = URIExists(ResolveCastleDataURL('castle-data:/game/')));
  AssertTrue(ueDirectory = URIExists(ResolveCastleDataURL('castle-data:/game')));
  AssertTrue(ueDirectory = URIExists(ResolveCastleDataURL('castle-data:/')));

  AssertTrue(ueFile = URIExists('data:model/vrml,#VRML V2.0 utf8' + NL +
    'Transform {' + NL +
    '  translation 0 -2 0' + NL +
    '  children Shape {' + NL +
    '    geometry Text { string "VRML 2.0 model inlined using data URI" }' + NL +
    '  }' + NL +
    '}'));
end;

procedure TTestURIUtils.TestRelativeToCastleDataURL;
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

procedure TTestURIUtils.TestExtractURI;
begin
  // Test with valid Spine URL, ending with anim-naming:strict-underscore
  AssertEquals('castle-data:/starling/', ExtractURIPath('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('character_zombie_atlas.starling-xml', ExtractURIName('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.starling-xml', URIDeleteAnchor('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.new-extension#fps:8,anim-naming:strict-underscore', ChangeURIExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore', '.new-extension'));
  AssertEquals('castle-data:/starling/character_zombie_atlas#fps:8,anim-naming:strict-underscore', DeleteURIExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore'));

  // Test also invalid Spine URL, ending with anim-naming:character_zombie_atlas.png
  AssertEquals('castle-data:/starling/', ExtractURIPath('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('character_zombie_atlas.starling-xml', ExtractURIName('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.starling-xml', URIDeleteAnchor('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));
  AssertEquals('castle-data:/starling/character_zombie_atlas.new-extension#fps:8,anim-naming:character_zombie_atlas.png', ChangeURIExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png', '.new-extension'));
  AssertEquals('castle-data:/starling/character_zombie_atlas#fps:8,anim-naming:character_zombie_atlas.png', DeleteURIExt('castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:character_zombie_atlas.png'));

  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/', ExtractURIPath('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('hawaii_exo.json', ExtractURIName('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json', URIDeleteAnchor('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.new-extension#skin:default', ChangeURIExt('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default', '.new-extension'));
  AssertEquals('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo#skin:default', DeleteURIExt('castle-data:/walking/npcs/hotel_room/hero/hawaii_exo.json#skin:default'));

  AssertEquals('armor.tga', ExtractURIName('armor.tga'));
  AssertEquals('armor.tga', ExtractURIName('/armor.tga'));
  AssertEquals('armor.tga', ExtractURIName('blabla/armor.tga'));
end;

initialization
  RegisterTest(TTestURIUtils);
end.
