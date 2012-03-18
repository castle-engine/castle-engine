{ -*- compile-command: "./compile_console.sh" -*- }
program test_castle_game_engine;

{ Define this if you use text runner for our tests.
  Usually this is automatically defined by calling compile_console.sh. }
{ $define TEXT_RUNNER}

{$mode objfpc}{$H+}

uses
  {$ifdef TEXT_RUNNER}
  ConsoleTestRunner,
  {$else}
  Interfaces, Forms, GuiTestRunner, castle_base,
  {$endif}

  CastleWarnings,

  { Test units (their order determines default tests order) }
  TestCastleUtils,
  TestGenericStructList,
  TestOSSpecific,
  TestBasicLists,
  TestCastleClassUtils,
  TestVectorMath,
  TestCastleColors,
  TestKeysMouse,
  TestFPImage,
  TestImages,
  TestOldFPCBugs,
  TestBoxes3d,
  TestFrustum,
  TestBase3D,
  TestParsingParameters,
  TestFNMatch,
  TestCameras,
  TestX3DNodes,
  TestX3DNodesOptimizedProxy,
  TestScene,
  TestSceneCore,
  { Not used anymore --- IntRects stuff is internal inside CastleMessages,
    and honestly not important enough.
  TestIntRects, }
  TestSpaceFillingCurves,
  TestObjectsList,
  TestCastleStringUtils,
  TestCastleScript,
  TestCastleScriptVectors,
  TestCubeMap,
  TestShadowFields,
  TestGLVersion,
  TestURLUtils,
  TestDDS,
  TestTriangulator
  {$ifdef TEXT_RUNNER} {$ifndef NO_WINDOW_SYSTEM},
  { These require CastleWindow initializing it's own window. So they
    1. conflict with LCL windows (so only when TEXT_RUNNER)
    2. are allowed only when window system (so not when NO_WINDOW_SYSTEM,
       e.g. do not do them when running inside non-X ssh session or cron) }
  TestCastleWindow,
  TestOpeningAndRendering3D,
  TestOpenGLFonts
  {$endif} {$endif}
  { Stuff requiring Lazarus LCL. }
  {$ifndef TEXT_RUNNER},
  TestCastleLCLUtils
  {$endif};

{var
  T: TTestX3DNodes;}
begin
  OnWarning := @OnWarningWrite;

{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC.

  T := TTestX3DNodes.Create;
  T.TestConvertToX3D;
  T.Free;
  Exit; }

  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
end.
