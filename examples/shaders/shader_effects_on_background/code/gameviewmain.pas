{
  Copyright 2025-2025 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleScene, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse, X3DNodes, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonNoEffects, ButtonEffectColor, ButtonEffectNoise, ButtonEffectCubeMap,
      ButtonAllEffects: TCastleButton;
    MainBackground: TCastleBackground;
  private
    EffectColor, EffectNoise, EffectCubeMap: TEffectNode;
    EffectColorIntensity: TSFFloat;
    LifeTime: TFloatTime;
    procedure ClickNoEffects(Sender: TObject);
    procedure ClickEffectColor(Sender: TObject);
    procedure ClickEffectNoise(Sender: TObject);
    procedure ClickEffectsCubeMap(Sender: TObject);
    procedure ClickAllEffects(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Create EffectColor node.

    Exposes 'intensity' uniform shader variable
    using X3D field EffectColorIntensity.
    You can change such uniform shader variable at any time.
    You can even change it very often (e.g. every frame),
    changing uniform values is cheap. }
  procedure CreateEffectColor;
  var
    EffectPart: TEffectPartNode;
  begin
    EffectColor := TEffectNode.Create;
    EffectColor.Language := slGLSL;
    { Don't use reference counting to free this X3D node when nothing refers to it.
      We will manage it manually. }
    EffectColor.WaitForRelease;

    { Add custom field (maps to GLSL uniform "color"), initially white }
    EffectColorIntensity := TSFFloat.Create(EffectColor, true, 'intensity', 0.0);
    EffectColor.AddCustomField(EffectColorIntensity);

    { Add TEffectPartNode which actually contains shader code.
      You can load shader code from file,
      you could also set it explicitly from string using EffectPart.Contents := '...'; }
    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/skybox_color_effect.fs']);
    EffectColor.SetParts([EffectPart]);
  end;

  { Create EffectNoise node. }
  procedure CreateEffectNoise;
  var
    EffectParts: array of TEffectPartNode;
  begin
    EffectNoise := TEffectNode.Create;
    EffectNoise.Language := slGLSL;
    EffectNoise.WaitForRelease;

    { Use 2 TEffectPartNode instance, because the GLSL noise functions
      live in a separate file (to show that we can have multiple shader parts,
      and also because they've been copied from
      https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83 ). }
    SetLength(EffectParts, 2);

    EffectParts[0] := TEffectPartNode.Create;
    EffectParts[0].ShaderType := stFragment;
    EffectParts[0].SetUrl(['castle-data:/shaders/skybox_noise.fs']);

    EffectParts[1] := TEffectPartNode.Create;
    EffectParts[1].ShaderType := stFragment;
    EffectParts[1].SetUrl(['castle-data:/shaders/noise.glsl']);

    EffectNoise.SetParts(EffectParts);
  end;

  { Create EffectCubeMap node. }
  procedure CreateEffectCubeMap;
  var
    EffectPart: TEffectPartNode;
    EffectTextureField: TSFNode;
    TestCubeMap: TImageCubeMapTextureNode;
  begin
    EffectCubeMap := TEffectNode.Create;
    EffectCubeMap.Language := slGLSL;
    EffectCubeMap.SetShaderLibraries(['castle-shader:/EyeWorldSpace.glsl']);
    EffectCubeMap.WaitForRelease;

    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/skybox_cubemap.fs']);

    { Note: You could have used TComposedCubeMapTextureNode as well,
      to define cubemap as a composition of 6 regular (2D) images. }
    TestCubeMap := TImageCubeMapTextureNode.Create;
    TestCubeMap.SetUrl(['castle-data:/test_cubemap.dds']);

    { Add custom field (maps to GLSL uniform "test_cube_map") }
    EffectTextureField := TSFNode.Create(EffectCubeMap, true, 'test_cube_map',
      [TImageCubeMapTextureNode], TestCubeMap);
    EffectCubeMap.AddCustomField(EffectTextureField);

    EffectCubeMap.SetParts([EffectPart]);
  end;

begin
  inherited;
  // assign OnClick handler to buttons
  ButtonNoEffects.OnClick := {$ifdef FPC}@{$endif} ClickNoEffects;
  ButtonEffectColor.OnClick := {$ifdef FPC}@{$endif} ClickEffectColor;
  ButtonEffectNoise.OnClick := {$ifdef FPC}@{$endif} ClickEffectNoise;
  ButtonEffectCubeMap.OnClick := {$ifdef FPC}@{$endif} ClickEffectsCubeMap;
  ButtonAllEffects.OnClick := {$ifdef FPC}@{$endif} ClickAllEffects;

  CreateEffectColor;
  CreateEffectNoise;
  CreateEffectCubeMap;
end;

procedure TViewMain.Stop;
begin
  NodeRelease(EffectColor);
  NodeRelease(EffectNoise);
  NodeRelease(EffectCubeMap);
  EffectColorIntensity := nil; // was (or will be) implicitly freed by EffectColor freeing
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  // Animate the effect color 'intensity' uniform, just to show that we can
  LifeTime := LifeTime + SecondsPassed;
  EffectColorIntensity.Send(Frac(LifeTime * 2.0));
end;

procedure TViewMain.ClickNoEffects(Sender: TObject);
begin
  MainBackground.SetEffects([]);
end;

procedure TViewMain.ClickEffectColor(Sender: TObject);
begin
  MainBackground.SetEffects([EffectColor]);
  LifeTime := 0; // reset the lifetime, so that the color effect starts from the beginning
end;

procedure TViewMain.ClickEffectNoise(Sender: TObject);
begin
  MainBackground.SetEffects([EffectNoise]);
end;

procedure TViewMain.ClickEffectsCubeMap(Sender: TObject);
begin
  MainBackground.SetEffects([EffectCubeMap]);
end;

procedure TViewMain.ClickAllEffects(Sender: TObject);
begin
  { Note: Order matters, it determines the order of calling
    the effects calculations. }
  MainBackground.SetEffects([EffectCubeMap, EffectColor, EffectNoise]);
  LifeTime := 0; // reset the lifetime, so that the color effect starts from the beginning
end;

end.
