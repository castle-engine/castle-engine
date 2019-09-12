program base_tests;

{$APPTYPE CONSOLE}
{$ASSERTIONS ON}

uses
  System.SysUtils,
  CastleAssertions,
  CastleUtils,
  CastleVectors,
  CastleColors,
  CastleStringUtils,
  CastleLog,
  CastleClassUtils,
  CastleProjection,
  CastleTimeUtils,
  CastleRectangles,
  URIParser,
  CastleFindFiles,
  CastleFilesUtils,
  CastleURIUtils,
  DOM,
  CastleXMLUtils;


procedure FoundFile(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  Writeln('  Found file: ', FileInfo.Name, ', ', FileInfo.URL);
end;

var
  M: TMatrix4;
  V: TVector3;
  Col: TCastleColorRGB;
  ColHsv: TVector3;
  R: TFloatRectangle;
  TimeStart: TTimerResult;
  URI: TURI;
  ExePath: String;
  XML: TXMLDocument;
begin
  TimeStart := Timer;

  // logging
  InitializeLog;
  WritelnLog('MyLog', 'Something to log: %d', [123]);

  // vectors
  M := TranslationMatrix(10, 20, 30);
  V := Vector3(1, 2, 3);
  V := M.MultPoint(V);
  Writeln('Translated vector: ', V.ToString);
  AssertVectorEquals(Vector3(11, 22, 33), V);

  // colors
  Col := RedRGB;
  ColHsv := RgbToHsv(Col);
  ColHsv.Z := 0.5; // half brightness
  Col := HsvToRgb(ColHsv);
  Writeln('Red color with half brightness ', Col.ToString);
  AssertVectorEquals(Vector3(0.5, 0, 0), Col);

  // useful stuff from FPC RTL
  Writeln('ApplicationName: ', ApplicationName);
  Writeln('AppConfigDir (user): ', GetAppConfigDir(false));
  Writeln('AppConfigDir (global): ', GetAppConfigDir(true));

  // rects
  R := FloatRectangle(2, 3, 10, 10);
  R := R.Grow(100, 100);
  Writeln(R.ToString);
  AssertRectsEqual(FloatRectangle(-98, -97, 210, 210), R);

  // URI
  URI := ParseURI('https://example.com/mypath/myfile.html');
  Assert('https' = URI.Protocol);
  Assert('example.com' = URI.Host);
  Assert('/mypath/' = URI.Path);
  Assert('myfile.html' = URI.Document);

  // CGE data
  Writeln('ApplicationData(''): ', ApplicationData(''));
  // Go 2 parent dirs up
  ExePath := ParentPath(ParentPath(ParentPath(ParamStr(0), false), false), false);
  ApplicationDataOverride := FilenameToURISafe(ExePath + 'data/');
  Writeln('ApplicationData('') after ApplicationDataOverride: ', ApplicationData(''));
  Writeln('castle-data:/image.png: ', ResolveCastleDataURL('castle-data:/image.png'));

  // CastleFilesUtils test
  Writeln('GetTempFileNameCheck: ', GetTempFileNameCheck);

  // FindFiles
  Writeln('Searching recursively for *.dpr*:');
  FindFiles('', '*.dpr*', false, FoundFile, nil, [ffRecursive]);
  Writeln('Searching recursively for *.dpr* in ExePath:');
  FindFiles(FilenameToURISafe(ExePath), '*.dpr*', false, FoundFile, nil, [ffRecursive]);

  // text file reading
  Writeln('test txt file reading: ', FileToString('castle-data:/test-file.txt'));
  Assert(
    'one line' + NL +
    '2nd line' + NL = FileToString('castle-data:/test-file.txt'));

  // XML file reading
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

  // timer
  Writeln('This was done in ', TimerSeconds(Timer, TimeStart):1:2, ' seconds');

  Readln;
end.

