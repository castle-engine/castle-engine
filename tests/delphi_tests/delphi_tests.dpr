{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ (Partially-)automatic tests of CGE with Delphi. }
program delphi_tests;

{$APPTYPE CONSOLE}
{$ASSERTIONS ON}

{ For EXTENDED_EQUALS_DOUBLE symbol. }
{$I castleconf.inc}

uses
  { standard units }
  SysUtils, Classes,
  { CGE units taken from FPC, only for Delphi compatibility }
  URIParser, DOM,
  { CGE units }
  CastleUtils, CastleVectors, CastleColors, CastleStringUtils,
  CastleLog, CastleClassUtils, CastleProjection, CastleTimeUtils,
  CastleRectangles, CastleFindFiles, CastleFilesUtils,
  CastleUriUtils, CastleXmlUtils, CastleImages, CastleInternalDataUri,
  CastleDownload, CastleInternalSoundFile, CastleSoundBase,
  { units specific to this project }
  CastleAssertions,
  TestFpJsonRTTI, TestFpJson, TestCastleConfig, TestCastleSerialization;

procedure TestLog;
begin
  WritelnLog('MyLog', 'Something to log: %d', [123]);
end;

procedure TestRects;
var
  R: TFloatRectangle;
begin
  R := FloatRectangle(2, 3, 10, 10);
  R := R.Grow(100, 100);
  Writeln(R.ToString);
  AssertRectsEqual(FloatRectangle(-98, -97, 210, 210), R);
end;

procedure FoundFile(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  Writeln('  Found file: ', FileInfo.Name, ', ', FileInfo.URL);
end;

procedure TestPaths;
begin
  // useful stuff from FPC RTL
  Writeln('ApplicationName: ', ApplicationName);
  Assert('delphi_tests' = ApplicationName);
  Writeln('AppConfigDir (user): ', GetAppConfigDir(false));
  Writeln('AppConfigDir (global): ', GetAppConfigDir(true));

  // CGE data
  Writeln('castle-data:/: ', ResolveCastleDataURL('castle-data:/'));

  (*
  // Old code, when we needed to set ApplicationDataOverride.
  // Now, the castle-data:/ works with Delphi out-of-the-box.

  {$ifdef MSWINDOWS}
  // Go 2 parent dirs up
  ExePath := ParentPath(ParentPath(ParentPath(ParamStr(0), false), false), false);
  {$else}
  ExePath := InclPathDelim(GetCurrentDir);
  {$endif}
  Writeln('Detected ExePath: ', ExePath);
  ApplicationDataOverride := FilenameToUriSafe(ExePath + 'data/');
  Writeln('castle-data:/ after ApplicationDataOverride: ', ResolveCastleDataURL('castle-data:/'));
  *)

  Writeln('castle-data:/image.png: ', ResolveCastleDataURL('castle-data:/image.png'));

  // CastleFilesUtils test
  Writeln('GetTempFileNameCheck: ', GetTempFileNameCheck);

  // FindFiles
  Writeln('Searching recursively for *.dpr*:');
  FindFiles('', '*.dpr*', false, FoundFile, nil, [ffRecursive]);
  Writeln('Searching recursively for *.dpr* in GetCurrentDir:');
  FindFiles(FilenameToUriSafe(GetCurrentDir), '*.dpr*', false, FoundFile, nil, [ffRecursive]);
end;

procedure TestURI;
var
  URI: TURI;
begin
  URI := ParseURI('https://example.com/mypath/myfile.html');
  Assert('https' = URI.Protocol);
  Assert('example.com' = URI.Host);
  Assert('/mypath/' = URI.Path);
  Assert('myfile.html' = URI.Document);
end;

procedure TestPercentEncoding;
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
  {$ifdef MSWINDOWS}
  AssertEquals('file:///c:/foo%254d.txt', FilenameToUriSafe('c:\foo%4d.txt'));
  AssertEquals('C:\fooM.txt', UriToFilenameSafe('file:///C:/foo%4d.txt'));
  AssertEquals('C:\foo%.txt', UriToFilenameSafe('file:///C:/foo%25.txt'));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('file:///foo%254d.txt', FilenameToUriSafe('/foo%4d.txt'));
  AssertEquals('/fooM.txt', UriToFilenameSafe('file:///foo%4d.txt'));
  AssertEquals('/foo%.txt', UriToFilenameSafe('file:///foo%25.txt'));
  {$endif}

  { Always UriToFilenameSafe and FilenameToUriSafe should reverse each other. }
  {$ifdef MSWINDOWS}
  AssertEquals('c:\foo%4d.txt', UriToFilenameSafe(FilenameToUriSafe('c:\foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///C:/foo%4d.txt', FilenameToUriSafe(UriToFilenameSafe('file:///C:/foo%4d.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertEquals('file:///C:/fooM.txt', FilenameToUriSafe(UriToFilenameSafe('file:///C:/foo%4d.txt')));
  {$endif}
  {$ifdef UNIX}
  AssertEquals('/foo%4d.txt', UriToFilenameSafe(FilenameToUriSafe('/foo%4d.txt')));
  { Actually this would be valid too:
    AssertEquals('file:///foo%25.txt', FilenameToUriSafe(UriToFilenameSafe('file:///foo%25.txt')));
    But it's Ok that %4d gets converted to M, as char "M" is safe inside URI. }
  AssertEquals('file:///fooM.txt', FilenameToUriSafe(UriToFilenameSafe('file:///foo%4d.txt')));
  {$endif}

  {$ifdef MSWINDOWS}
  Filename := 'C:\Users\cge\AppData\Local\test_local_filename_chars\config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenameAsUri := FilenameToUriSafe(Filename);
  Assert('file:///C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt' = FilenameAsUri);
  FilenameFromUri := UriToFilenameSafe(FilenameAsUri);
  Assert(Filename = FilenameFromUri);

  FilenamePart := 'C:/Users/cge/AppData/Local/test_local_filename_chars/config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenamePartPercent := InternalUriEscape(FilenamePart);
  Assert('C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt' = FilenamePartPercent);
  FilenamePartUnescaped := InternalUriUnescape(FilenamePartPercent);
  Assert(FilenamePart = FilenamePartUnescaped);
  {$endif}
end;


procedure TestTextRead;
begin
  Writeln('test txt file reading: ', FileToString('castle-data:/test-file.txt'));
  Assert(
    'one line' + NL +
    '2nd line' + NL = FileToString('castle-data:/test-file.txt'));
end;

procedure TestXmlRead;
var
  XML: TXMLDocument;
begin
  XML := URLReadXML('castle-data:/test.xml');
  try
    Writeln('test xml file reading: ',
      XML.DocumentElement.TagName, ' ',
      XML.DocumentElement.AttributeString('name'), ' ',
      XML.DocumentElement.Child('some_element') <> nil, ' ',
      XML.DocumentElement.Child('some_element').AttributeVector3('attribute').ToString);
    Assert('project' = XML.DocumentElement.TagName);
    Assert('castle_spine' = XML.DocumentElement.AttributeString('name'));
    Assert(XML.DocumentElement.Child('some_element') <> nil);
    Assert(TVector3.Equals(XML.DocumentElement.Child('some_element').AttributeVector3('attribute'),
      Vector3(1, 2, 3)));
  finally
    FreeAndNil(Xml);
  end;
end;

var
  TimeStart: TTimerResult;
begin
  TimeStart := Timer;
  InitializeLog;

  TestLog;
  TestRects;
  TestPaths;
  TestURI;
  TestPercentEncoding;
  TestTextRead;
  TestXmlRead;

  TestFpJson1;
  // simpledemo form fcl-json examples
  TestFpJson2;

  TestRTTI;
  TestRTTICastle;
  TestCastleConfig1;

  // timer test
  Writeln('This was done in ', TimerSeconds(Timer, TimeStart):1:2, ' seconds');

  //Readln;
end.
