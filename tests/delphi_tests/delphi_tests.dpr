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
  CastleURIUtils, CastleXMLUtils, CastleImages, CastleInternalDataUri,
  CastleDownload, CastleInternalSoundFile, CastleSoundBase,
  { units specific to this project }
  CastleAssertions,
  TestFpJsonRTTI, TestFpJson, TestCastleConfig, TestCastleSerialization;

procedure TestLog;
begin
  WritelnLog('MyLog', 'Something to log: %d', [123]);
end;

procedure TestVectors;
var
  M: TMatrix4;
  V: TVector3;
begin
  // vectors
  M := TranslationMatrix(10, 20, 30);
  V := Vector3(1, 2, 3);
  V := M.MultPoint(V);
  Writeln('Translated vector: ', V.ToString);
  AssertVectorEquals(Vector3(11, 22, 33), V);
end;

procedure TestColors;
var
  Col: TCastleColorRGB;
  ColHsv: TVector3;
begin
  Col := RedRGB;
  ColHsv := RgbToHsv(Col);
  ColHsv.Z := 0.5; // half brightness
  Col := HsvToRgb(ColHsv);
  Writeln('Red color with half brightness ', Col.ToString);
  AssertVectorEquals(Vector3(0.5, 0, 0), Col);
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
  Writeln('ApplicationData(''''): ', ApplicationData(''));

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
  ApplicationDataOverride := FilenameToURISafe(ExePath + 'data/');
  Writeln('ApplicationData('''') after ApplicationDataOverride: ', ApplicationData(''));
  *)

  Writeln('castle-data:/image.png: ', ResolveCastleDataURL('castle-data:/image.png'));

  // CastleFilesUtils test
  Writeln('GetTempFileNameCheck: ', GetTempFileNameCheck);

  // FindFiles
  Writeln('Searching recursively for *.dpr*:');
  FindFiles('', '*.dpr*', false, FoundFile, nil, [ffRecursive]);
  Writeln('Searching recursively for *.dpr* in GetCurrentDir:');
  FindFiles(FilenameToURISafe(GetCurrentDir), '*.dpr*', false, FoundFile, nil, [ffRecursive]);
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
  AssertEquals('file:///c:/foo%254d.txt', FilenameToURISafe('c:\foo%4d.txt'));
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
  AssertEquals('c:\foo%4d.txt', URIToFilenameSafe(FilenameToURISafe('c:\foo%4d.txt')));
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

  {$ifdef MSWINDOWS}
  Filename := 'C:\Users\cge\AppData\Local\test_local_filename_chars\config with Polish chars ćma źrebak żmija wąż królik.txt';
  FilenameAsUri := FilenameToURISafe(Filename);
  Assert('file:///C:/Users/cge/AppData/Local/test_local_filename_chars/config%20with%20Polish%20chars%20%C4%87ma%20%C5%BArebak%20%C5%BCmija%20w%C4%85%C5%BC%20kr%C3%B3lik.txt' = FilenameAsUri);
  FilenameFromUri := URIToFilenameSafe(FilenameAsUri);
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

procedure TestImage;
var
  Img: TCastleImage;
begin
  Img := LoadImage('castle-data:/test_texture.png');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;
end;

procedure TestTypeSizes;
begin
  AssertEquals(1, SizeOf(Byte));
  AssertEquals(1, SizeOf(ShortInt));

  AssertEquals(2, SizeOf(Word));
  AssertEquals(2, SizeOf(SmallInt));

  AssertEquals(4, SizeOf(Int32));
  AssertEquals(4, SizeOf(UInt32));

  AssertEquals(8, SizeOf(Int64));
  AssertEquals(8, SizeOf(UInt64));
  AssertEquals(8, SizeOf(QWord));

  { Both in FPC and Delphi, Integer/Cardinal remained 4-byte (even though
    in old days the Integer/Cardinal were documented as potentially
    platform-dependent size).
    See (Delphi): https://docwiki.embarcadero.com/RADStudio/Sydney/en/Simple_Types_(Delphi) }
  AssertEquals(4, SizeOf(Integer));
  AssertEquals(4, SizeOf(Cardinal));

  AssertEquals(4, SizeOf(Single));
  AssertEquals(8, SizeOf(Double));

  AssertEquals(
    {$if defined(EXTENDED_EQUALS_DOUBLE)} 8
    {$elseif defined(EXTENDED_EQUALS_LONG_DOUBLE)} 16
    {$else} 10
    {$endif}, SizeOf(Extended));
end;

{ Test reading data URI encoded with base64. }
procedure TestDataUri;
const
  ImageDataUri = {$I bricks_base64.inc};
  WavDataUri = {$I werewolf_howling_wav_base64.inc};
var
  S: TStream;
  Img: TCastleImage;
  SoundFile: TSoundFile;
begin
  S := Download(ImageDataUri);
  try
  finally FreeAndNil(S) end;

  Img := LoadImage(ImageDataUri);
  try
    AssertEquals(1024, Img.Width);
    AssertEquals(1024, Img.Height);
  finally FreeAndNil(Img) end;

  SoundFile := TSoundFile.Create(WavDataUri);
  try
    Writeln('Loaded: ', URICaption(SoundFile.URL));
    Writeln('  Format: ', DataFormatToStr(SoundFile.DataFormat));
    Writeln('  Frequency: ', SoundFile.Frequency);
    Writeln('  Duration: ', SoundFile.Duration:1:2);

    Assert(SoundFile.DataFormat = sfMono16);
    AssertSameValue(3.75, SoundFile.Duration, 0.01);
    AssertEquals(22050, SoundFile.Frequency);
    AssertEquals('data:audio/x-wav;base64,...', URICaption(SoundFile.URL));
  finally FreeAndNil(SoundFile) end;
end;

procedure TestFormatNameCounter;
var
  AllowOldPercentSyntax: Boolean;
  ReplacementsDone: Cardinal;
begin
  { assertions below should work for both AllowOldPercentSyntax values }
  for AllowOldPercentSyntax := false to true do
  begin
    AssertEquals('', FormatNameCounter('', 0, AllowOldPercentSyntax, ReplacementsDone));
    AssertEquals('a', FormatNameCounter('a', 0, AllowOldPercentSyntax, ReplacementsDone));
    AssertEquals('%again66', FormatNameCounter('%again@counter(1)', 66, AllowOldPercentSyntax, ReplacementsDone));
    AssertEquals('%%again66', FormatNameCounter('%%again@counter(1)', 66, AllowOldPercentSyntax, ReplacementsDone));
    AssertEquals('%%again0066', FormatNameCounter('%%again@counter(4)', 66, AllowOldPercentSyntax, ReplacementsDone));
  end;
end;

var
  TimeStart: TTimerResult;
begin
  TimeStart := Timer;
  InitializeLog;


  TestLog;
  TestVectors;
  TestColors;
  TestRects;
  TestPaths;
  TestURI;
  TestPercentEncoding;
  TestTextRead;
  TestXmlRead;
  TestImage;
  TestTypeSizes;
  TestDataUri;
  TestFormatNameCounter;

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
