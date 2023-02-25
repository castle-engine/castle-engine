{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to add TextureProperties nodes, to request anisotropic filtering on specific textures. }

uses SysUtils, Classes,
  X3DNodes, X3DLoad, CastleLog, CastleVectors, CastleTransform, CastleScene,
  CastleWindow, CastleViewport, CastleTextureImages, CastleCameras;

type
  { The node processing callback (given to Node.EnumerateNodes) must be a method
    (that is, inside a class).
    In case of this simple application, we don't have a natural class where to put it,
    so we use this TNodeHandler.
    In a "real" application, you usually have a natural class, like TStateXxx class. }
  TNodeHandler = class
    class procedure HandleImageTextureNode(Node: TX3DNode);
  end;

class procedure TNodeHandler.HandleImageTextureNode(Node: TX3DNode);
var
  ImageTexture: TImageTextureNode;
  TextureProperties: TTexturePropertiesNode;
begin
  ImageTexture := Node as TImageTextureNode;
  // For the sake of demo, apply AnisotropicFiltering on grass texture, but not on brick texture
  if ImageTexture.FdUrl.Items.IndexOf('textures/grass_01.png') <> -1 then
  begin
    TextureProperties := TTexturePropertiesNode.Create;
    TextureProperties.AnisotropicDegree := 8;
    TextureProperties.MinificationFilter := minDefault;
    TextureProperties.MagnificationFilter := magDefault;
    TextureProperties.BoundaryModeS := BoolRepeatToBoundaryMode[ImageTexture.RepeatS];
    TextureProperties.BoundaryModeT := BoolRepeatToBoundaryMode[ImageTexture.RepeatT];
    ImageTexture.TextureProperties := TextureProperties;
  end;
end;

procedure SetAnisotropicFiltering(const Node: TX3DRootNode);
begin
  Node.EnumerateNodes(TImageTextureNode,
    // TODO: This will look more sane if we remake example to use UI state
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleImageTextureNode, false);
end;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  Node: TX3DRootNode;
begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Window.Controls.InsertFront(Viewport);

  Node := LoadNode('castle-data:/test_environment.gltf');
  SetAnisotropicFiltering(Node);

  Scene := TCastleScene.Create(nil);
  // Scene.Load('castle-data:/test_environment.gltf'); // load directly, without anisotropic filtering
  Scene.Load(Node, true);
  Scene.PreciseCollisions := true;
  { We set MainScene := Scene, this way automatic camera (initialized thanks
    to Viewport.AutoCamera=true) will follow camera set in Blender, in test_environment.gltf . }
  Viewport.Items.MainScene := Scene;
  Viewport.Items.Add(Scene);

  Application.Run;
end.
