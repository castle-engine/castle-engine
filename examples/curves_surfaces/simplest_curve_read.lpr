uses SysUtils, CastleVectors, CastleFilesUtils, CastleCurves;
var
  Curves: TCurveList;
  FirstCurve: TPiecewiseCubicBezier;
begin
  Curves := TCurveList.Create(true { free objects });
  try
    Curves.LoadFromFile(ApplicationData('my_curves.xml'));
    // assume that the file contains at least 1 curve,
    // and it's a TPiecewiseCubicBezier curve.
    FirstCurve := Curves[0] as TPiecewiseCubicBezier;
    // write some beginning curve points
    Writeln(VectorToNiceStr(FirstCurve.Point(0.0)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.1)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.2)));
  finally FreeAndNil(Curves) end;
end.
