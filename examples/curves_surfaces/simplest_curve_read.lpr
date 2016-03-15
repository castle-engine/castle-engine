uses SysUtils, CastleVectors, CastleFilesUtils, CastleCurves;
var
  Curves: TCurveList;
  FirstCurve: TCurve;
begin
  Curves := TCurveList.Create(true { free objects });
  try
    Curves.LoadFromFile(ApplicationData('my_curves.xml'));
    if Curves.Count = 0 then
      raise Exception.Create('No curves defined in file');
    FirstCurve := Curves[0];
    // write some beginning curve points
    Writeln(VectorToNiceStr(FirstCurve.Point(0.0)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.1)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.2)));
  finally FreeAndNil(Curves) end;
end.
