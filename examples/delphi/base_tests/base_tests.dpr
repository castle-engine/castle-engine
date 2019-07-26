program base_tests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  CastleUtils,
  CastleVectors,
  CastleColors,
  CastleStringUtils,
  CastleLog,
  CastleClassUtils,
  CastleProjection,
  CastleTimeUtils,
  CastleRectangles;

var
  M: TMatrix4;
  V: TVector3;
  Col: TCastleColorRGB;
  ColHsv: TVector3;
  R: TFloatRectangle;
  TimeStart: TTimerResult;
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

  // colors
  Col := RedRGB;
  ColHsv := RgbToHsv(Col);
  ColHsv.Z := 0.5; // half brightness
  Col := HsvToRgb(ColHsv);
  Writeln('Red color with half brightness ', Col.ToString);

  // useful stuff from FPC RTL
  Writeln('ApplicationName: ', ApplicationName);
  Writeln('AppConfigDir (user): ', GetAppConfigDir(false));
  Writeln('AppConfigDir (global): ', GetAppConfigDir(true));

  // rects
  R := FloatRectangle(2, 3, 10, 10);
  R := R.Grow(100, 100);
  Writeln(R.ToString);

  // timer
  Writeln('This was done in ', TimerSeconds(Timer, TimeStart):1:2, ' seconds');

  Readln;
end.

