{
  Copyright 2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestURIUtils;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestURIUtils = class(TTestCase)
    procedure TestAbsoluteURI;
    procedure TestURIToFilenameSafe;
    procedure PercentEncoding;
    procedure CombineURIEncoding;
    procedure TestURIDisplay;
  end;

implementation

uses CastleURIUtils, URIParser, CastleUtils;

procedure TTestURIUtils.TestAbsoluteURI;
begin
  {$ifdef MSWINDOWS}
  AssertEquals('file:///C:/foo.txt', AbsoluteURI('c:\foo.txt'));
  { Below ExpandFileName will change /foo.txt on Windows to add drive letter }
  AssertEquals('file:///C:/foo.txt', AbsoluteURI('/foo.txt'));
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

  AssertEquals(FilenameToURISafe(InclPathDelim(GetCurrentDir) + 'foo.txt'), AbsoluteURI('foo.txt'));
  AssertEquals('http://foo', AbsoluteURI('http://foo'));
  AssertEquals(FilenameToURISafe(InclPathDelim(GetCurrentDir)), AbsoluteURI(''));
end;

procedure TTestURIUtils.TestURIToFilenameSafe;
var
  Temp: string;
begin
  { URIToFilename fails for Windows absolute filenames,
    but our URIToFilenameSafe works. }
  Assert(not URIToFilename('c:\foo.txt', Temp));
  AssertEquals('c:\foo.txt', URIToFilenameSafe('c:\foo.txt'));
end;

procedure TTestURIUtils.PercentEncoding;
begin
  { FilenameToURISafe must percent-encode,
    URIToFilenameSafe must decode it back. }
  {$ifdef MSWINDOWS}
  AssertEquals('file:///C:/foo%254d.txt', FilenameToURISafe('c:\foo%4d.txt'));
  AssertEquals('C:\fooM.txt', URIToFilenameSafe('file:///C:/foo%4d.txt'));
  AssertEquals('C:\foo%.txt', URIToFilenameSafe('file:///C:/foo%25.txt'));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('file:///foo%254d.txt', FilenameToURISafe('/foo%4d.txt'));
  AssertEquals('/fooM.txt', URIToFilenameSafe('file:///foo%4d.txt'));
  AssertEquals('/foo%.txt', URIToFilenameSafe('file:///foo%25.txt'));
  {$endif}

  { Always URIToFilenameSafe and FilenameToURISafe should reverse each other. }
  {$ifdef MSWINDOWS}
  AssertEquals('C:\foo%4d.txt', URIToFilenameSafe(FilenameToURISafe('c:\foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///C:/foo%4d.txt', FilenameToURISafe(URIToFilenameSafe('file:///C:/foo%4d.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertEquals('file:///C:/fooM.txt', FilenameToURISafe(URIToFilenameSafe('file:///C:/foo%4d.txt')));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('/foo%4d.txt', URIToFilenameSafe(FilenameToURISafe('/foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///foo%25.txt', FilenameToURISafe(URIToFilenameSafe('file:///foo%25.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertEquals('file:///fooM.txt', FilenameToURISafe(URIToFilenameSafe('file:///foo%4d.txt')));
  {$endif}
end;

procedure TTestURIUtils.CombineURIEncoding;
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

initialization
  RegisterTest(TTestURIUtils);
end.
