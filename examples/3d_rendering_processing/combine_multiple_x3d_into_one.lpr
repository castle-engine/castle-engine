{
  Copyright 2015-2018 Michalis Kamburelis.

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

  If you want to render the complex scene fast, it may be beneficial to combine
  it into a single TCastleScene, instead of using multiple TCastleScenes.
  See https://castle-engine.io/manual_transformation_hierarchy.php for detailed
  discussion.
}
program combine_multiple_x3d_into_one;

uses SysUtils, CastleWindow, CastleSceneCore, CastleScene, X3DLoad, X3DNodes,
  CastleFilesUtils, CastleVectors, CastleViewport;

var
  MainRoot, ModelBoxes, ModelRaptor: TX3DRootNode;
  TransformBoxes: TTransformNode;
  TransformRaptor: array [0..2] of TTransformNode;
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  I: Integer;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

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

  ModelBoxes := LoadNode('castle-data:/boxes.x3dv');

  TransformBoxes := TTransformNode.Create;
  TransformBoxes.Translation := Vector3(-5, 0, 0);
  TransformBoxes.AddChildren(ModelBoxes);

  MainRoot.AddChildren(TransformBoxes);

  { add ModelRaptor and TransformRaptor[0..2] }

  ModelRaptor := LoadNode('castle-data:/raptor_1.x3d');

  for I := 0 to 2 do
  begin
    TransformRaptor[I] := TTransformNode.Create;
    TransformRaptor[I].Translation := Vector3(8, (I -1)  * 5, 0);
    TransformRaptor[I].AddChildren(ModelRaptor);

    MainRoot.AddChildren(TransformRaptor[I]);
  end;

  { now load and display MainRoot model }

  Scene := TCastleScene.Create(Application);
  Scene.Load(MainRoot, true { Scene owns this MainRoot node });
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
