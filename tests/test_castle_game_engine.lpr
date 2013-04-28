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
  TestGenericLists,
  TestOSSpecific,
  TestBasicLists,
  TestCastleClassUtils,
  TestCastleVectors,
  TestCastleColors,
  TestKeysMouse,
  TestFPImage,
  TestImages,
  TestOldFPCBugs,
  TestCastleBoxes,
  TestCastleFrustum,
  TestCastle3D,
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
  TestCubeMaps,
  TestShadowFields,
  TestGLVersion,
  TestURLUtils,
  TestDDS,
  TestCastleTriangulate,
  TestGame,
  TestURIUtils
  {$ifdef TEXT_RUNNER} {$ifndef NO_WINDOW_SYSTEM},
  { These require CastleWindow initializing it's own window. So they
    1. conflict with LCL windows (so only when TEXT_RUNNER)
    2. are allowed only when graphical window system (like X on Unix)
       is available (so not when NO_WINDOW_SYSTEM,
       e.g. do not do them when running inside non-X ssh session or cron) }
  TestCastleWindow,
  TestOpeningAndRendering3D,
  TestCastleGLFonts
  {$endif} {$endif}
  { Stuff requiring Lazarus LCL. }
  {$ifndef TEXT_RUNNER},
  TestCastleLCLUtils
  {$endif};


{$ifdef TEXT_RUNNER}
var
  Application: TTestRunner;
{$endif}

{var
  T: TTestCastleTriangulate;}
begin
  OnWarning := @OnWarningWrite;

{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC.

  T := TTestCastleTriangulate.Create;
  T.TestTriangulateFace;
  T.Free;
  Exit;}

  {$ifdef TEXT_RUNNER}
  Application := TTestRunner.Create(nil);
  DefaultFormat := fPlain;
  DefaultRunAllTests := true;
  {$endif}
  Application.Initialize;
  {$ifndef TEXT_RUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$endif}
  Application.Run;
  {$ifdef TEXT_RUNNER}
  Application.Free;
  {$endif}
end.
