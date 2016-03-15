uses SysUtils, CastleVectors, CastleFilesUtils, CastleCurves;
var
  FirstCurve: TCurve;
  Curves: TCurveList;
begin
  FirstCurve := TCurve.LoadFromFile(ApplicationData('my_curves.xml'));
  try
    // That's it, you loaded the 1st curve from XML file.
    // Write some initial curve points.
    Writeln(VectorToNiceStr(FirstCurve.Point(0.0)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.1)));
  finally FreeAndNil(FirstCurve) end;

  { in more complicated scenarios, my_curves.xml may keep many curves
    inside. Load them like this: }

  Curves := TCurveList.Create(true { free objects });
  try
    Curves.LoadFromFile(ApplicationData('my_curves.xml'));
    if Curves.Count = 0 then
      raise Exception.Create('No curves defined in file');
    FirstCurve := Curves[0];
    // That's it, you have the 1st curve from XML file.
    // Write some initial curve points.
    Writeln(VectorToNiceStr(FirstCurve.Point(0.0)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.1)));
    Writeln(VectorToNiceStr(FirstCurve.Point(0.2)));
  finally FreeAndNil(Curves) end;
end.
