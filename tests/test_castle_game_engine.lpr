program test_castle_game_engine;

{ Define this if you use text runner for our tests.
  Usually this is automatically defined by calling compile_console.sh. }
{ $define TEXT_RUNNER}

{ Define this to disable any GUI tests (using CastleWindow).
  The CastleWindow
  - Conflicts with LCL windows (so it can be used only when TEXT_RUNNER,
    otherwise we use a runner that shows output in LCL window).
  - Can work only when graphical window system (like X on Unix)
    is available (e.g. not inside non-X ssh session, or cron).
}
{ $define NO_WINDOW_SYSTEM}

{$mode objfpc}{$H+}

uses
  {$ifdef TEXT_RUNNER}
  CastleConsoleTestRunner, ConsoleTestRunner,
  {$else}
  Interfaces, Forms, GuiTestRunner, castle_base,
  {$endif}

  CastleLog, CastleApplicationProperties,

  { Test units below. Their order determines default tests order. }

  { Testing (mainly) things inside FPC standard library, not CGE }
  TestCompiler,
  TestSysUtils,
  TestFGL,
  TestGenericsCollections,
  TestOldFPCBugs,
  TestFPImage,

  { Testing CGE units }
  TestCastleUtils,
  TestCastleRectangles,
  TestCastleGenericLists,
  TestCastleFilesUtils,
  TestCastleUtilsLists,
  TestCastleClassUtils,
  TestCastleVectors,
  TestCastleTriangles,
  TestCastleColors,
  TestCastleQuaternions,
  TestCastleKeysMouse,
  TestCastleImages,
  TestCastleImagesDraw,
  TestCastleBoxes,
  TestCastleFrustum,
  TestCastleTransform,
  TestCastleParameters,
  TestCastleCameras,
  TestX3DFields,
  TestX3DNodes,
  TestX3DNodesOptimizedProxy,
  TestCastleScene,
  TestCastleSceneCore,
  TestCastleSceneManager,
  TestCastleVideos,
  TestCastleSpaceFillingCurves,
  TestCastleStringUtils,
  TestCastleScript,
  TestCastleScriptVectors,
  TestCastleCubeMaps,
  TestCastleGLVersion,
  TestCastleCompositeImage,
  TestCastleTriangulate,
  TestCastleGame,
  TestCastleURIUtils,
  TestCastleXMLUtils,
  TestCastleCurves,
  TestCastleTimeUtils,
  TestCastleControls,
  TestCastleRandom,
  TestCastleSoundEngine

  {$ifdef TEXT_RUNNER} {$ifndef NO_WINDOW_SYSTEM},
  TestCastleWindow,
  TestCastleOpeningAndRendering3D,
  TestCastleFonts,
  TestCastleWindowOpen
  {$endif} {$endif}

  { Stuff requiring Lazarus LCL. }
  {$ifndef TEXT_RUNNER},
  TestCastleLCLUtils
  {$endif};

{$ifdef TEXT_RUNNER}
var
  Application: TCastleConsoleTestRunner;
{$endif}

{var
  T: TTestCastleTransform;}
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  // avoid warnings that opening files too early
  ApplicationProperties._FileAccessSafe := true;

{ Sometimes it's comfortable to just run the test directly, to get
  full backtrace from FPC.

  T := TTestCastleTransform.Create;
  T.TestPhysicsWorldOwnerEmptyBox;
  T.Free;
  Exit;}

  {$ifdef TEXT_RUNNER}
  Application := TCastleConsoleTestRunner.Create(nil);
  Application.Title := 'Castle Game Engine test runner (using fpcunit)';
  DefaultFormat := fPlain;
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
