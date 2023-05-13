// -*- compile-command: "./test_single_testcase.sh TTestOpeningAndRendering3D" -*-
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test TCastleScene opening and rendering various files. }
unit TestCastleOpeningAndRendering3D;

{ Test also all models found in

    ../../demo-models
    ../../castle/data
    ../../www/htdocs

  This will make the test take *much* longer time,
  TestOpeningAndRendering3D will then open + render a lot of models. }
{.$define CASTLE_TEST_DEMO_MODELS}

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry, CastleTestCase,
  {$else}CastleTester,{$endif} CastleFilesUtils, CastleFindFiles,
  CastleWindow, CastleSceneCore, CastleScene, CastleViewport;

type
  TTestOpeningAndRendering3D = class(TCastleTestCase)
  private
    { Available only during Test1 }
    Window: TCastleWindow;
    Viewport: TCastleAutoNavigationViewport;
    Scene: TCastleScene;
    RecreateSceneEachTime: boolean;

    { FileName empty means to load empty scene. }
    procedure TestScene(const FileName: string);
    procedure TestSceneFromEnum(const FileInfo: TFileInfo; var StopSearch: boolean);
    { If RecreateSceneEachTime, Scene will be destroyed and then created
      again before each load.

      Both values make sense for testing:

      false checks that pure "Load" properly deals (clears) with
      previously loaded content, so it mostly checks BeforeNodesFree
      and ChangedAll.

      true checks that destructions properly deals with a scene. }
    procedure TestOpenAndRender(const ARecreateSceneEachTime: boolean);
  published
    procedure Test1;
  end;

implementation

uses SysUtils, StrUtils,
  CastleUtils, CastleGLUtils, CastleGLVersion, CastleLog, CastleApplicationProperties,
  CastleTransform, CastleInternalGLUtils;

procedure TTestOpeningAndRendering3D.TestScene(const FileName: string);
begin
  if RecreateSceneEachTime then
  begin
    //Write('Recreating scene... ');

    FreeAndNil(Scene);
    AssertTrue(Viewport.Items.MainScene = nil);
    { the only remaining things should be TCastleCamera }
    AssertEquals(1, Viewport.Items.Count);
    AssertTrue(Viewport.Items[0] is TCastleCamera);

    Scene := TCastleScene.Create(Window);
    Scene.PreciseCollisions := true;
    Scene.ProcessEvents := true;

    Viewport := TCastleSceneManager.Create(Window);
    Viewport.Items.Add(Scene);
    Viewport.Items.MainScene := Scene;
  end;

  //Writeln('Testing "' + FileName + '"');

  if FileName = '' then
    Scene.Load(nil, true) else
    Scene.Load(FileName);

  Viewport.Navigation.Free;
  // camera should be nil now (thanks to free notification),
  // and no new camera should be automatically created yet.
  AssertTrue(Viewport.Navigation = nil);
  Viewport.RequiredNavigation;

  Viewport.Navigation := nil;
  AssertTrue(Viewport.Navigation = nil);

  { Force preparing and using OpenGL resources for the scene.
    This way we also check that next Load frees them Ok. }
  Window.Container.EventBeforeRender;
  Window.Container.EventRender;

  { Check OpenGL errors now, otherwise they could be detected by an unrelated
    CheckGLErrors call later. This way we know what 3D filename caused
    the error. }
  CheckGLErrors;
end;


procedure TTestOpeningAndRendering3D.TestSceneFromEnum(const FileInfo: TFileInfo; var StopSearch: boolean);
var
  ParentDirName: string;
begin
  { While our masks do not allow such files,
    but searching on Windows can find xxx.x3dv~ when only *.x3dv is requested.
    So explicitly avoid them (as they will fail to load in CGE, as unrecognized).
    TODO: should we just workaround it in FindFiles? The problem is inside FindFirst/Next. }
  if IsWild(FileInfo.Name, '*~', true) then Exit;

  { do not check files in "errors" subdir, these are known to cause trouble }
  ParentDirName := ExtractFileName(ExclPathDelim(ExtractFileDir(FileInfo.AbsoluteName)));
  if ParentDirName = 'errors' then Exit;

  TestScene(FileInfo.AbsoluteName);
end;

procedure TTestOpeningAndRendering3D.TestOpenAndRender(const ARecreateSceneEachTime: boolean);

  procedure TestScenesInDir(const Path: string);

    procedure DoMask(const Mask: string);
    begin
      FindFiles(Path, Mask, false, {$ifdef FPC}@{$endif}TestSceneFromEnum, [ffRecursive]);
    end;

  begin
    DoMask('*.wrl');
    DoMask('*.wrz');
    DoMask('*.wrl.gz');
    DoMask('*.x3d');
    DoMask('*.x3dz');
    DoMask('*.x3d.gz');
    DoMask('*.x3dv');
    DoMask('*.x3dvz');
    DoMask('*.x3dv.gz');
    DoMask('*.3ds');
    DoMask('*.dae');
  end;

begin
  RecreateSceneEachTime := ARecreateSceneEachTime;

  Window := TCastleWindow.Create(nil);
  try
    Scene := TCastleScene.Create(Window);
    Scene.PreciseCollisions := true;
    Scene.ProcessEvents := true;

    Viewport := TCastleAutoNavigationViewport.Create(Window);
    Viewport.Items.Add(Scene);
    Viewport.Items.MainScene := Scene;

    Window.Controls.InsertFront(Viewport);
    Window.Open;

    TestScene('');
    TestScenesInDir('data');

    {$ifdef CASTLE_TEST_DEMO_MODELS}
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'demo-models');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'castle' + PathDelim + 'data');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'www' + PathDelim + 'htdocs');
    {$endif CASTLE_TEST_DEMO_MODELS}

    ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
    try
      { e.g. tests that auto_normals_indexed_geometry.x3dv makes no warnings when generating
        arrays for rendering. }
      TestScene('castle-data:/auto_normals_indexed_geometry.x3dv');
      TestScene('castle-data:/auto_normals_indexed_geometry_full.obj');
    finally
      ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
    end;

    Window.Close;
  finally FreeAndNil(Window) end;
end;

procedure TTestOpeningAndRendering3D.Test1;
begin
  {$ifdef CASTLE_TESTER}
  if not CanCreateWindowForTest then
    Exit;
  {$endif}

  TestOpenAndRender(false);
  TestOpenAndRender(true);
end;

initialization
  RegisterTest(TTestOpeningAndRendering3D);
end.
