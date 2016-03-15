uses SysUtils, CastleVectors, CastleFilesUtils, CastleCurves;
var
  Curves: TControlPointsCurveList;
  Curve0: TPiecewiseCubicBezier;
begin
  Curves := TControlPointsCurveList.Create(true { free objects });
  try
    Curves.LoadFromFile(ApplicationData('my_curves.xml'));
    // assume that the file contains at least 1 curve,
    // and it's a TPiecewiseCubicBezier curve.
    Curve0 := Curves[0] as TPiecewiseCubicBezier;
    // write some beginning curve points
    Writeln(VectorToNiceStr(Curve0.Point(0.0)));
    Writeln(VectorToNiceStr(Curve0.Point(0.1)));
    Writeln(VectorToNiceStr(Curve0.Point(0.2)));
  finally FreeAndNil(Curves) end;
end.
