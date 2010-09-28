{ -*- compile-command: "./compile_console.sh" -*- }
program test_kambi_units;

{ Define this if you use text runner for our tests.
  Usually this is automatically defined by calling compile_console.sh. }
{ $define TEXT_RUNNER}

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
  TestCameras,
  TestVRMLNodes,
  TestVRMLNodesOptimizedProxy,
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
  TestDDS {$ifdef TEXT_RUNNER},
  { These require GLWindow initializing it's own window,
    so they conflict with LCL windows. }
  TestGLWindow,
  TestOpeningAndRendering3D
  {$endif};

{var
  T: TTestVRMLNodesOptimizedProxy;}
begin
{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC.

  T := TTestVRMLNodesOptimizedProxy.Create;
  T.TestGeometryUsesOptimizedMethods;
  T.Free;
  Exit; }

  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
end.
