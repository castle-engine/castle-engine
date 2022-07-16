{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonOpen: TCastleButton;
    MainScene, SceneParticles, SceneRaptor: TCastleScene;
    ViewportNormal, ViewportTransparent,
      ViewportScreenEffect, ViewportExamine: TCastleViewport;

    procedure ClickOpen(Sender: TObject);
    procedure CameraNavigationReinitialize;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleScreenEffects, X3DNodes, CastleUtils, CastleRenderOptions, CastleWindow,
  X3DLoad;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

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

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ButtonOpen := DesignedComponent('ButtonOpen') as TCastleButton;
  MainScene := DesignedComponent('MainScene') as TCastleScene;
  SceneParticles := DesignedComponent('SceneParticles') as TCastleScene;
  SceneRaptor := DesignedComponent('SceneRaptor') as TCastleScene;
  ViewportNormal := DesignedComponent('ViewportNormal') as TCastleViewport;
  ViewportTransparent := DesignedComponent('ViewportTransparent') as TCastleViewport;
  ViewportScreenEffect := DesignedComponent('ViewportScreenEffect') as TCastleViewport;
  ViewportExamine := DesignedComponent('ViewportExamine') as TCastleViewport;

  ViewportTransparent.Items := ViewportNormal.Items;
  ViewportScreenEffect.Items := ViewportNormal.Items;
  ViewportExamine.Items := ViewportNormal.Items;

  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} ClickOpen;

  AddScreenEffect(ViewportScreenEffect);

  CameraNavigationReinitialize;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.CameraNavigationReinitialize;
begin
  ViewportNormal.AssignDefaultCamera;
  ViewportTransparent.AssignDefaultCamera;
  ViewportScreenEffect.AssignDefaultCamera;
  ViewportExamine.AssignDefaultCamera;
end;

procedure TStateMain.ClickOpen(Sender: TObject);
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
