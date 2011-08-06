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

  KambiWarnings,

  { Test units (their order determines default tests order) }
  TestKambiUtils,
  TestBasicLists,
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
  { Not used anymore --- IntRects stuff is internal inside GLWinMessages,
    and honestly not important enough.
  TestIntRects, }
  TestSpaceFillingCurves,
  TestObjectsList,
  TestKambiStringUtils,
  TestKambiScript,
  TestKambiScriptVectors,
  TestCubeMap,
  TestShadowFields,
  TestGLVersion,
  TestURLUtils,
  TestDDS,
  TestTriangulator
  {$ifdef TEXT_RUNNER} {$ifndef NO_WINDOW_SYSTEM},
  { These require GLWindow initializing it's own window. So they
    1. conflict with LCL windows (so only when TEXT_RUNNER)
    2. are allowed only when window system (so not when NO_WINDOW_SYSTEM,
       e.g. do not do them when running inside non-X ssh session or cron) }
  TestGLWindow,
  TestOpeningAndRendering3D
  {$endif} {$endif};

{var
  T: TTestVRMLNodes;}
begin
  OnWarning := @OnWarningWrite;

{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC.

  T := TTestVRMLNodes.Create;
  T.TestConvertToX3D;
  T.Free;
  Exit; }

  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
end.
