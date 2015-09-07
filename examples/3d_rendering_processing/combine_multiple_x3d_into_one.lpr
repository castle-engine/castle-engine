{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ An example how to load multiple 3D model files into a single X3D graph,
  by combining them into a single X3D tree rooted in TX3DRootNode.
  This is useful for example when auto-generating a complex 3D scene
  by code, from multiple small X3D files.

  If you want to render the complex scene fast, it's better to combine
  it, and use a single TCastleScene, than to use multiple TCastleScenes.
  Using a single TCastleScene means that e.g. frustum culling will be
  efficient.
  See http://castle-engine.sourceforge.net/tutorial_transformation_hierarchy.php .
}
program combine_multiple_x3d_into_one;

uses SysUtils, CastleWindow, CastleSceneCore, CastleScene, X3DLoad, X3DNodes,
  CastleFilesUtils, CastleVectors;

var
  MainRoot, Model1, Model2: TX3DRootNode;
  Transform1, Transform2: TTransformNode;
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  { Create an X3D graph like this:
    MainRoot (TX3DRootNode)
    * Transform1 (TTransformNode)
      * Model1 (TX3DRootNode, loaded from models/boxes.x3dv)
    * Transform2 (TTransformNode)
      * Model1 (TX3DRootNode, loaded from models/raptor_1.x3d)
  }

  Model1 := Load3D(ApplicationData('models/boxes.x3dv'));

  Transform1 := TTransformNode.Create('', '');
  Transform1.Translation := Vector3Single(-5, 0, 0);
  Transform1.FdChildren.Add(Model1);

  Model2 := Load3D(ApplicationData('models/raptor_1.x3d'));

  Transform2 := TTransformNode.Create('', '');
  Transform2.Translation := Vector3Single(5, 0, 0);
  Transform2.FdChildren.Add(Model2);

  MainRoot := TX3DRootNode.Create('', '');
  MainRoot.FdChildren.Add(Transform1);
  MainRoot.FdChildren.Add(Transform2);

  { now load and display MainRoot model }

  Scene := TCastleScene.Create(Application);
  Scene.Load(MainRoot, true { Scene owns this MainRoot node });
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.Open;
  Application.Run;
end.
