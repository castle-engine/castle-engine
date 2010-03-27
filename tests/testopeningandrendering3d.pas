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

interface

uses fpcunit, testutils, testregistry, EnumerateFiles,
  GLWindow, VRMLScene, VRMLGLScene, KambiSceneManager;

type
  TTestOpeningAndRendering3D = class(TTestCase)
  private
    { Available only during Test1 }
    Window: TGLUIWindow;
    SceneManager: TKamSceneManager;
    Scene: TVRMLGLScene;

    { FileName empty means to load empty scene. }
    procedure TestScene(const FileName: string);
    procedure TestSceneFromEnum(const FileInfo: TEnumeratedFileInfo);
  published
    procedure Test1;
  end;

implementation

uses SysUtils, VRMLErrors, KambiUtils, KambiGLUtils;

procedure TTestOpeningAndRendering3D.TestScene(const FileName: string);
begin
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

  { check GL errors now, otherwise that could be deferred to unrelated
    CheckGLErrors call later. This way we know what 3D filenam caused the error. }
  CheckGLErrors;
end;

procedure TTestOpeningAndRendering3D.TestSceneFromEnum(const FileInfo: TEnumeratedFileInfo);
var
  ParentDirName: string;
begin
  { do not check files in "errors" subdir, these are known to cause trouble }
  ParentDirName := ExtractFileName(ExclPathDelim(ExtractFileDir(FileInfo.FullFileName)));
  if ParentDirName = 'errors' then Exit;

  TestScene(FileInfo.FullFileName);
end;

procedure TTestOpeningAndRendering3D.Test1;
const
  VrmlTestSuite = '..' + PathDelim + '..' + PathDelim + 'kambi_vrml_test_suite' + PathDelim;

  procedure TestScenesInDir(const Path: string);

    procedure DoMask(const Mask: string);
    begin
      EnumFilesObj(Path + Mask, faReallyAnyFile, @TestSceneFromEnum, [eoRecursive]);
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
  VRMLWarning := @VRMLWarning_Ignore;

  Window := TGLUIWindow.Create(nil);
  try
    Scene := TVRMLGLScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TKamSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    Window.Controls.Add(SceneManager);
    Window.Init;

    TestScene('');

    TestScene(VrmlTestSuite + 'x3d' + PathDelim + 'follow_camera_by_proximity_sensor.x3dv');
    TestScene(VrmlTestSuite + 'x3d' + PathDelim + 'follow_camera_by_proximity_sensor.x3dv');

    TestScenesInDir(VrmlTestSuite);

    //TestScenesInDir(VrmlTestSuite + 'x3d/shadow_maps/');

    Window.Close;
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestOpeningAndRendering3D);
end.
