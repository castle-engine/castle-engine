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
  end;

implementation

uses CastleURIUtils, URIParser, CastleUtils;

procedure TTestURIUtils.TestAbsoluteURI;
begin
  {$ifdef MSWINDOWS}
  AssertEquals('file:///C:/foo.txt', AbsoluteURI('c:\foo.txt'));
  { ExpandFileName will change /foo.txt on Windows to add drive letter }
  AssertEquals('file:///C:/foo.txt', AbsoluteURI('/foo.txt'));
  {$endif}
  {$ifdef UNIX}
  { ExpandFileName will change below on Unix, treating "c:\" like a normal filename. }
  AssertEquals(FilenameToURI(InclPathDelim(GetCurrentDir) + 'c:\foo.txt', AbsoluteURI('c:\foo.txt'));
  AssertEquals('file:///foo.txt', AbsoluteURI('/foo.txt'));
  {$endif}
  AssertEquals(FilenameToURI(InclPathDelim(GetCurrentDir) + 'foo.txt'), AbsoluteURI('foo.txt'));
  AssertEquals('http://foo', AbsoluteURI('http://foo'));
  AssertEquals(FilenameToURI(InclPathDelim(GetCurrentDir)), AbsoluteURI(''));
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

initialization
  RegisterTest(TTestURIUtils);
end.
