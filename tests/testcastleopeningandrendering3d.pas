{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleOpeningAndRendering3D;

{$I tests.inc}

interface

uses fpcunit, testutils, testregistry, CastleFilesUtils, CastleFindFiles,
  CastleWindow, CastleSceneCore, CastleScene, CastleSceneManager;

type
  TTestOpeningAndRendering3D = class(TTestCase)
  private
    { Available only during Test1 }
    Window: TCastleWindowCustom;
    SceneManager: TCastleSceneManager;
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

uses SysUtils, CastleUtils, CastleGLUtils, CastleGLVersion, CastleLog;

procedure TTestOpeningAndRendering3D.TestScene(const FileName: string);
begin
  if RecreateSceneEachTime then
  begin
    //Write('Recreating scene... ');

    FreeAndNil(Scene);
    AssertTrue(SceneManager.MainScene = nil);
    AssertTrue(SceneManager.Items.Count = 0);

    Scene := TCastleScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TCastleSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;
  end;

  //Writeln('Testing "' + FileName + '"');

  if FileName = '' then
    Scene.Load(nil, true) else
    Scene.Load(FileName);

  SceneManager.Camera.Free;
  SceneManager.Camera := SceneManager.CreateDefaultCamera;

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
  { do not check files in "errors" subdir, these are known to cause trouble }
  ParentDirName := ExtractFileName(ExclPathDelim(ExtractFileDir(FileInfo.AbsoluteName)));
  if ParentDirName = 'errors' then Exit;

  if GLVersion.Fglrx and
    ( (FileInfo.Name = 'ssao_stairs.x3dv') or
      (FileInfo.Name = 'twoboxes_ssao.x3dv') or
      (FileInfo.Name = 'ssao_barna29_0.x3dv') or
      (FileInfo.Name = 'ssao_stairs_with_test_plane.x3dv')
    ) then
  begin
    Writeln('Not testing "' + FileInfo.AbsoluteName + '": known to fail on fglrx (fucking ATI)');
    Exit;
  end;

  TestScene(FileInfo.AbsoluteName);
end;

procedure TTestOpeningAndRendering3D.TestOpenAndRender(const ARecreateSceneEachTime: boolean);

  procedure TestScenesInDir(const Path: string);

    procedure DoMask(const Mask: string);
    begin
      FindFiles(Path, Mask, false, @TestSceneFromEnum, [ffRecursive]);
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

  Window := TCastleWindowCustom.Create(nil);
  try
    Scene := TCastleScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TCastleSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    Window.Controls.InsertFront(SceneManager);
    Window.Open;

    TestScene('');
    TestScenesInDir('data');

    {$ifdef CASTLE_ENGINE_TRUNK_AVAILABLE}
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'demo_models');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'castle' + PathDelim + 'data');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'vrml_engine_doc');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'rift' + PathDelim + 'data');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'www' + PathDelim + 'htdocs');
    {$endif CASTLE_ENGINE_TRUNK_AVAILABLE}

    Window.Close;
  finally FreeAndNil(Window) end;
end;

procedure TTestOpeningAndRendering3D.Test1;
begin
  TestOpenAndRender(false);
  TestOpenAndRender(true);
end;

initialization
  RegisterTest(TTestOpeningAndRendering3D);
end.
