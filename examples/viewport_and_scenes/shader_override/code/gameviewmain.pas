{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleRenderOptions,
  X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
    LabelFps: TCastleLabel;
    Monkey2: TCastleScene;

    CustomShaderActive: Boolean;
    Appearance: TAppearanceNode;
    ComposedShader: TComposedShaderNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  VertexShader, FragmentShader: TShaderPartNode;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  Monkey2 := DesignedComponent('Monkey2') as TCastleScene;

  { Define nodes with GLSL shader code. }

  VertexShader := TShaderPartNode.Create;
  VertexShader.ShaderType := stVertex;
  VertexShader.Contents :=
    // Needed to calculate vertex position
    'uniform mat4 castle_ModelViewMatrix;' + NL +
    'uniform mat4 castle_ProjectionMatrix;' + NL +
    'attribute vec4 castle_Vertex;' + NL +

    // Needed to calculate intensity, to have some interesting shading
    'uniform mat3 castle_NormalMatrix;' + NL +
    'attribute vec3 castle_Normal;' + NL +

    // Pass intensity to fragment shader, to make simple toon shading
    'varying float intensity;' + NL +

    'void main(void)' + NL +
    '{' + NL +
    // Calculate intensity as simplest diffuse from a headlight
    '  intensity = dot(' + NL +
    '    normalize(vec3(0.0, 0.0, 1.0)),' + NL +
    '    normalize(castle_NormalMatrix * castle_Normal));' + NL +
    // Calculate final vertex position, determines where shape is rendered
    '  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);' + NL +
    '}';

  FragmentShader := TShaderPartNode.Create;
  FragmentShader.ShaderType := stFragment;
  FragmentShader.Contents :=
    'varying float intensity;' + NL +
    'void main(void)' + NL +
    '{' + NL +
    '  float intensity_toon = floor(intensity * 5.0) / 5.0;' + NL +
    '  gl_FragColor = vec4(1.0 * intensity_toon, 0.0, 0.0, 1.0);' + NL +
    '}';

  ComposedShader := TComposedShaderNode.Create;
  ComposedShader.SetParts([VertexShader, FragmentShader]);

  { By calling KeepExistingBegin we say we want to manually manage
    the lifetime of ComposedShader. Otherwise it would be freed
    as soon as it is no longer used, which means that pressing S twice
    (see TViewMain.Press) would crash, as ComposedShader would be freed.
    In TViewMain.Stop we will have to take care to free the node. }
  ComposedShader.KeepExistingBegin;

  { We know that Blender exported material wich such name to glTF,
    and we know that our glTF importer turned it into TAppearanceNode. }
  Appearance := Monkey2.Node('MonkeyMaterial') as TAppearanceNode;

  CustomShaderActive := true;
  Appearance.SetShaders([ComposedShader]);
end;

procedure TViewMain.Stop;
begin
  ComposedShader.KeepExistingEnd;
  FreeIfUnusedAndNil(ComposedShader);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyS) then
  begin
    CustomShaderActive := not CustomShaderActive;
    if CustomShaderActive then
      Appearance.SetShaders([ComposedShader])
    else
      // clearing Shaders list will restore default rendering
      Appearance.SetShaders([]);

    Exit(true); // key was handled
  end;
end;

end.
