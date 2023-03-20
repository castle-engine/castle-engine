{
  Copyright 2010-2023 Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonOpen: TCastleButton;
    MainScene, SceneParticles, SceneRaptor: TCastleScene;
    ViewportNormal, ViewportTransparent,
      ViewportScreenEffect, ViewportExamine: TCastleViewport;
  private
    procedure ClickOpen(Sender: TObject);
    procedure CameraNavigationReinitialize;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleScreenEffects, X3DNodes, CastleUtils, CastleRenderOptions, CastleWindow,
  X3DLoad;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  procedure AddScreenEffect(const ScreenEffectControl: TCastleScreenEffects);
  var
    ScreenEffect: TScreenEffectNode;
    ComposedShader: TComposedShaderNode;
    FragmentShader: TShaderPartNode;
  begin
    FragmentShader := TShaderPartNode.Create;
    FragmentShader.ShaderType := stFragment;
    FragmentShader.Contents :=
      'void main (void)' +NL+
      '{' +NL+
        '  vec4 left   = screen_get_color(ivec2(screen_x() - 1, screen_y()));' + NL +
        '  vec4 right  = screen_get_color(ivec2(screen_x() + 1, screen_y()));' + NL +
        '  vec4 top    = screen_get_color(ivec2(screen_x(), screen_y() - 1));' + NL +
        '  vec4 bottom = screen_get_color(ivec2(screen_x(), screen_y() + 1));' + NL +
        '  gl_FragColor = (1.0 + abs(left - right) + abs(top - bottom)) / 2.0;' + NL +
      '}';

    ComposedShader := TComposedShaderNode.Create;
    ComposedShader.SetParts([FragmentShader]);

    ScreenEffect := TScreenEffectNode.Create;
    ScreenEffect.SetShaders([ComposedShader]);

    ScreenEffectControl.AddScreenEffect(ScreenEffect);
  end;

begin
  inherited;

  ViewportTransparent.Items := ViewportNormal.Items;
  ViewportScreenEffect.Items := ViewportNormal.Items;
  ViewportExamine.Items := ViewportNormal.Items;

  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} ClickOpen;

  AddScreenEffect(ViewportScreenEffect);

  CameraNavigationReinitialize;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.CameraNavigationReinitialize;
begin
  ViewportNormal.AssignDefaultCamera;
  ViewportTransparent.AssignDefaultCamera;
  ViewportScreenEffect.AssignDefaultCamera;
  ViewportExamine.AssignDefaultCamera;
end;

procedure TViewMain.ClickOpen(Sender: TObject);
var
  NewUrl: string;
begin
  NewUrl := MainScene.Url;
  if Application.MainWindow.FileDialog('Open Scene', NewUrl, true, LoadScene_FileFilters) then
  begin
    { Free other scenes than MainScene.
      They are present initially just to demonstrate that multiple scenes work too. }
    FreeAndNil(SceneParticles);
    FreeAndNil(SceneRaptor);

    MainScene.Load(NewUrl);
    CameraNavigationReinitialize;
  end;
end;

end.
