{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define and display a 3D object with custom shaders in Object Pascal code.
  In CGE, you can build and edit X3D scene graph using Object Pascal,
  see https://castle-engine.io/manual_scene.php#section_building_and_editing .

  See also other X3D-build code, e.g. build_3d_object_by_code.lpr, build_3d_tunnel.lpr ,
  ExportToX3D in ../terrain/terrain.lpr (uses shaders too)...
}

uses SysUtils,
  CastleLog, CastleRenderOptions, CastleVectors, X3DNodes, CastleWindow,
  CastleSceneCore, CastleScene, CastleUtils, CastleViewport;

function BuildX3D: TX3DRootNode;
var
  Box: TBoxNode;
  BoxShape: TShapeNode;
  Appearance: TAppearanceNode;
  ComposedShader: TComposedShaderNode;
  VertexShader, FragmentShader: TShaderPartNode;
begin
  { create Box and BoxShape in one go }
  Box := TBoxNode.CreateWithShape(BoxShape);
  Box.Size := Vector3(1, 2, 3);

  { Note: if you're looking instead at a way to enhance the default shaders
    (not replace them), use the Effect and EffectPart
    nodes instead of ComposedShader and ShaderPart.
    See https://castle-engine.io/compositing_shaders.php . }

  VertexShader := TShaderPartNode.Create;
  VertexShader.ShaderType := stVertex;
  VertexShader.Contents :=
    'uniform mat4 castle_ModelViewMatrix;' + NL +
    'uniform mat4 castle_ProjectionMatrix;' + NL +
    'attribute vec4 castle_Vertex;' + NL +
    'void main(void)' + NL +
    '{' + NL +
    '  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);' + NL +
    '}';

  FragmentShader := TShaderPartNode.Create;
  FragmentShader.ShaderType := stFragment;
  FragmentShader.Contents :=
    'void main(void)' + NL +
    '{' + NL +
    '  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);' + NL +
    '}';

  ComposedShader := TComposedShaderNode.Create;
  ComposedShader.SetParts([VertexShader, FragmentShader]);

  Appearance := TAppearanceNode.Create;
  Appearance.SetShaders([ComposedShader]);

  // Appearance.Material is only used when OpenGL does not support shaders,
  // or if the shader failed to compile.
  // Appearance.Material := TMaterialNode.Create;

  BoxShape.Appearance := Appearance;

  Result := TX3DRootNode.Create;
  Result.AddChildren(BoxShape);
end;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  { the log will contain e.g. information if the GLSL failed to compile }
  InitializeLog;

  Window := TCastleWindowBase.Create(Application);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildX3D, true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Window.Open;
  Application.Run;
end.
