{
  Copyright 2015-2017 Michalis Kamburelis.

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
  MainRoot, ModelBoxes, ModelRaptor: TX3DRootNode;
  TransformBoxes: TTransformNode;
  TransformRaptor: array [0..2] of TTransformNode;
  Window: TCastleWindow;
  Scene: TCastleScene;
  I: Integer;
begin
  { Create an X3D graph like this:

    MainRoot (TX3DRootNode)
    |- TransformBoxes (TTransformNode)
       |- ModelBoxes (TX3DRootNode, loaded from data/boxes.x3dv)

    |- TransformRaptor[0] (TTransformNode)
       |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

    |- TransformRaptor[1] (TTransformNode)
       |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

    |- TransformRaptor[2] (TTransformNode)
       |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

    Note that the same TCastleScene instance "ModelRaptor" is added
    multiple times to the X3D nodes graph (just with different transformations).
    This is fully supported and valid.
  }

  MainRoot := TX3DRootNode.Create;

  { add ModelBoxes and TransformBoxes }

  ModelBoxes := Load3D(ApplicationData('boxes.x3dv'));

  TransformBoxes := TTransformNode.Create;
  TransformBoxes.Translation := Vector3Single(-5, 0, 0);
  TransformBoxes.FdChildren.Add(ModelBoxes);

  MainRoot.FdChildren.Add(TransformBoxes);

  { add ModelRaptor and TransformRaptor[0..2] }

  ModelRaptor := Load3D(ApplicationData('raptor_1.x3d'));

  for I := 0 to 2 do
  begin
    TransformRaptor[I] := TTransformNode.Create;
    TransformRaptor[I].Translation := Vector3Single(8, (I -1)  * 5, 0);
    TransformRaptor[I].FdChildren.Add(ModelRaptor);

    MainRoot.FdChildren.Add(TransformRaptor[I]);
  end;

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
