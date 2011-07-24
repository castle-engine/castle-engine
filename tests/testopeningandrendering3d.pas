{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestOpeningAndRendering3D;

{$I tests.inc}

interface

uses fpcunit, testutils, testregistry, KambiFilesUtils, EnumerateFiles,
  GLWindow, VRMLScene, VRMLGLScene, KambiSceneManager;

type
  TTestOpeningAndRendering3D = class(TTestCase)
  private
    { Available only during Test1 }
    Window: TGLUIWindow;
    SceneManager: TKamSceneManager;
    Scene: TVRMLGLScene;
    RecreateSceneEachTime: boolean;

    { FileName empty means to load empty scene. }
    procedure TestScene(const FileName: string);
    procedure TestSceneFromEnum(const FileInfo: TEnumeratedFileInfo);
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

uses SysUtils, KambiUtils, KambiGLUtils, GLVersionUnit, KambiWarnings;

procedure TTestOpeningAndRendering3D.TestScene(const FileName: string);
begin
  if RecreateSceneEachTime then
  begin
    Write('Recreating scene... ');

    FreeAndNil(Scene);
    Assert(SceneManager.MainScene = nil);
    Assert(SceneManager.Items.List.Count = 0);

    Scene := TVRMLGLScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TKamSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;
  end;

  Writeln('Testing "' + FileName + '"');

  if FileName = '' then
    Scene.Load(nil, true) else
    Scene.Load(FileName);

  SceneManager.Camera.Free;
  SceneManager.Camera := SceneManager.CreateDefaultCamera(SceneManager);

  { Force preparing and using OpenGL resources for the scene.
    This way we also check that next Load frees them Ok. }
  Window.EventBeforeDraw;
  Window.EventDraw;

  { Check OpenGL errors now, otherwise they could be detected by an unrelated
    CheckGLErrors call later. This way we know what 3D filename caused
    the error. }
  CheckGLErrors;
end;

procedure TTestOpeningAndRendering3D.TestSceneFromEnum(const FileInfo: TEnumeratedFileInfo);
var
  ParentDirName: string;
begin
  { do not check files in "errors" subdir, these are known to cause trouble }
  ParentDirName := ExtractFileName(ExclPathDelim(ExtractFileDir(FileInfo.FullFileName)));
  if ParentDirName = 'errors' then Exit;

  if GLVersion.IsFglrx and
    ( (FileInfo.SearchRec.Name = 'ssao_stairs.x3dv') or
      (FileInfo.SearchRec.Name = 'twoboxes_ssao.x3dv') or
      (FileInfo.SearchRec.Name = 'ssao_barna29_0.x3dv') or
      (FileInfo.SearchRec.Name = 'ssao_stairs_with_test_plane.x3dv')
    ) then
  begin
    Writeln('Not testing "' + FileInfo.FullFileName + '": known to fail on fglrx (fucking ATI)');
    Exit;
  end;

  TestScene(FileInfo.FullFileName);
end;

procedure TTestOpeningAndRendering3D.TestOpenAndRender(const ARecreateSceneEachTime: boolean);

  procedure TestScenesInDir(const Path: string);

    procedure DoMask(const Mask: string);
    begin
      EnumFilesObj(InclPathDelim(Path) + Mask, faReallyAnyFile,
        @TestSceneFromEnum, [eoRecursive]);
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
  OnWarning := @OnWarningIgnore;

  RecreateSceneEachTime := ARecreateSceneEachTime;

  Window := TGLUIWindow.Create(nil);
  try
    Scene := TVRMLGLScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TKamSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    Window.Controls.Add(SceneManager);
    Window.Open;

    TestScene('');
    TestScenesInDir('data');

    {$ifdef VRMLENGINE_TRUNK_AVAILABLE}
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'demo_models');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'castle' + PathDelim + 'data');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'vrml_engine_doc');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'rift' + PathDelim + 'data');
    TestScenesInDir('..' + PathDelim + '..' + PathDelim + 'www' + PathDelim + 'htdocs');
    {$endif VRMLENGINE_TRUNK_AVAILABLE}

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
