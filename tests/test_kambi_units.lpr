{ -*- compile-command: "./compile_console.sh" -*- }
program test_kambi_units;

{ $define TEXT_RUNNER}

{ This one requires you have checked out from SVN ../kambi_vrml_test_suite/
  directory, so disabled by default. }
{ $define TEST_OPENING_AND_RENDERING_3D}

{$mode objfpc}{$H+}

uses
  {$ifdef TEXT_RUNNER}
  ConsoleTestRunner,
  {$else}
  Interfaces, Forms, GuiTestRunner, kambi_base,
  {$endif}

  { Test units (their order determines default tests order) }
  TestKambiUtils,
  TestDynArrays,
  TestKambiClassUtils,
  TestVectorMath,
  TestImages,
  TestOldFPCBugs,
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
  TestKambiStringUtils,
  TestKambiScript,
  TestKambiScriptVectors,
  TestCubeMap,
  TestShadowFields,
  TestGLVersion,
  TestURLUtils,
  TestDDS
  {$ifdef TEXT_RUNNER}
  { These require GLWindow initializing it's own window,
    so they conflict with LCL windows. }
  ,TestGLWindow
  {$ifdef TEST_OPENING_AND_RENDERING_3D}
  ,TestOpeningAndRendering3D
  {$endif}
  {$endif};

{var
  T: TTestOpeningAndRendering3D;}
begin
{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC. }
{
  T := TTestOpeningAndRendering3D.Create;
  T.Test1;
  T.Free;
}
  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
end.
