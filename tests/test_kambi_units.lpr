program test_kambi_units;

{ $define TEXT_RUNNER}

{$mode objfpc}{$H+}

uses
  {$ifdef TEXT_RUNNER}
  ConsoleTestRunner,
  {$else}
  Interfaces, Forms, GuiTestRunner,
  {$endif}

  { Test units (their order determines default tests order) }
  TestKambiUtils,
  TestDynArrays,
  TestKambiClassUtils,
  TestVectorMath,
  TestImages,
  TestBoxes3d,
  TestFrustum,
  TestParsingParameters,
  TestFNMatch,
  TestVRMLFields,
  TestVRMLNodes,
  TestVRMLGLScene,
  TestVRMLScene,
  TestIntRects,
  TestSpaceFillingCurves,
  TestObjectsList,
  {$ifndef TEXT_RUNNER}
  kambi_units,
  {$endif}
  TestKambiStringUtils,
  TestKambiScript,
  TestKambiScriptVectors,
  TestCubeEnvMap,
  TestShadowFields;

{var
  T: TTestFrustum;}
begin
{ Sometimes it's comfortable to just run the rest directly, to get
  full backtrace from FPC. 

  T := TTestFrustum.Create;
  T.TestCompareWithUnoptimizedPlaneCollision;
  T.Free;}

  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
end.
