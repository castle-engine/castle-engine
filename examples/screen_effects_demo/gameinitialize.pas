{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

{$modeswitch advancedrecords}

interface

implementation

uses SysUtils,
  CastleWindowTouch, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleScreenEffects,
  CastleSceneManager, Castle2DSceneManager, X3DNodes, X3DLoad, CastleUtils,
  CastleRendererBaseTypes, CastleVectors;

var
  Window: TCastleWindowBase;
  LabelFPS: TCastleLabel;

  { 3 controls below have methods AddScreenEffect / RemoveScreenEffect
    that we will use.

    Note that we *could* insert each TCastleSceneManager and TCastle2DSceneManager
    as child of a new TCastleScreenEffects instance,
    and thus have 3 TCastleScreenEffects instances like this:
    - ScreenEffectsOnButtonImage,
    - ScreenEffectsOnSceneManager
    - ScreenEffectsOn2DSceneManager
    However, we demonstrate below a simpler approach:
    TCastleSceneManager and TCastle2DSceneManager already descend
    from TCastleScreenEffects. So we can call AddScreenEffect method
    directly on SceneManager and SceneManager2D, without the need to create
    something like ScreenEffectsOnSceneManager. }
  ScreenEffectsOnButtonImage: TCastleScreenEffects;
  SceneManager2D: TCastle2DSceneManager;
  SceneManager3D: TCastleSceneManager;

{ TEffect -------------------------------------------------------------------- }

type
  { Container for all the information related to a particular screen effect. }
  TEffect = record
    { Button that controls whether this effect is enabled.
      Button.Pressed is always synchronized with Enabled value of the effect
      in all controls. }
    Button: TCastleButton;

    { 3 instances of TScreenEffectNode, for 3 controls
      (for ScreenEffectsOnButtonImage,
      for SceneManager2D,
      for SceneManager3D). }
    EffectNodes: array [0..2] of TScreenEffectNode;

    { 3 instances of X3D node graph that contains TScreenEffectNode,
      for 3 controls.
      RootNodes[I] is a graph that always contains EffectNodes[I].
      It may be equal to EffectNodes[I], but may have something more,
      e.g. a "support" TimeSensor node that provides value to the shader. }
    RootNodes: array [0..2] of TAbstractChildNode;

    { Change Button.Pressed, synchronize Enabled state of all effect nodes. }
    procedure ToggleButton;
  end;

procedure TEffect.ToggleButton;
var
  E: TScreenEffectNode;
begin
  Button.Pressed := not Button.Pressed;
  for E in EffectNodes do
    E.Enabled := Button.Pressed;
end;

var
  FilmGrain, Pixelate, EdgeDetect: TEffect;

{ handle button clicks ------------------------------------------------------- }

type
  TEventsHandler = class
    class procedure ToggleFilmGrain(Sender: TObject);
    class procedure TogglePixelate(Sender: TObject);
    class procedure ToggleEdgeDetect(Sender: TObject);
  end;

class procedure TEventsHandler.ToggleFilmGrain(Sender: TObject);
begin
  FilmGrain.ToggleButton;
end;

class procedure TEventsHandler.TogglePixelate(Sender: TObject);
begin
  Pixelate.ToggleButton;
end;

class procedure TEventsHandler.ToggleEdgeDetect(Sender: TObject);
begin
  EdgeDetect.ToggleButton;
end;

{ routines ------------------------------------------------------------------- }

procedure WindowUpdate(Container: TUIContainer);
begin
  LabelFPS.Caption := 'FPS: ' + Container.Fps.ToString;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  { Just to show that it is possible, build TScreenEffectNode performing
    "edge detect" effect completely using Pascal, without any external
    X3D or GLSL files. }
  function CreateEdgeDetectNode: TScreenEffectNode;
  var
    ComposedShader: TComposedShaderNode;
    FragmentShader: TShaderPartNode;
  begin
    FragmentShader := TShaderPartNode.Create;
    FragmentShader.ShaderType := stFragment;
    FragmentShader.Contents :=
      'void main (void)' + NL +
      '{' + NL +
      '  vec4 left   = screen_get_color(ivec2(screen_x() - 1, screen_y()));' + NL +
      '  vec4 right  = screen_get_color(ivec2(screen_x() + 1, screen_y()));' + NL +
      '  vec4 top    = screen_get_color(ivec2(screen_x(), screen_y() - 1));' + NL +
      '  vec4 bottom = screen_get_color(ivec2(screen_x(), screen_y() + 1));' + NL +
      '  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;' + NL +
      '}';

    ComposedShader := TComposedShaderNode.Create;
    ComposedShader.SetParts([FragmentShader]);

    Result := TScreenEffectNode.Create;
    Result.SetShaders([ComposedShader]);
  end;

  procedure InitialiazeScreenEffects;
  var
    I: Integer;
  begin
    { The implementation of this procedure got complicated due to the amount
      of things I wanted to demonstrate. But the basic API to add screen effects
      is very simple: you want to load the X3D graph containing one or more
      TScreenEffectNode, and call AddScreenEffect with it.
      For example, this is how to load film grain effect in SceneManager3D:

      SceneManager3D.AddScreenEffect(
        LoadNode('castle-data:/screen_effects/film_grain.x3dv'));

      Alternatively, you may want to save the LoadNode result,
      and find there an instance of TScreenEffectNode, to be able to
      toggle it's Enabled property:

      var
        RootNode: TX3DRootNode;
        ScreenEffectNode: TScreenEffectNode;
      begin
        RootNode := LoadNode('castle-data:/screen_effects/film_grain.x3dv');
        ScreenEffectNode := RootNode.FindNodeByName(TScreenEffectNode,
          'FilmGrainScreenEffect', true) as TScreenEffectNode;
        SceneManager3D.AddScreenEffect(RootNode);
      end;

      Note that you should call "AddScreenEffect(RootNode)",
      not "AddScreenEffect(ScreenEffectNode)".
      That is because RootNode may contain some additional "support"
      nodes, e.g. TimeSensor nodes that feed the time to your shader.

      What we do below is somewhat more complicated code using this API,
      as we want to repeat above idea 9 times, and store results for later.
    }

    for I := 0 to 2 do
    begin
      FilmGrain.RootNodes[I] := LoadNode('castle-data:/screen_effects/film_grain.x3dv');
      FilmGrain.EffectNodes[I] := FilmGrain.RootNodes[I].FindNodeByName(TScreenEffectNode,
        'FilmGrainScreenEffect', true) as TScreenEffectNode;

      Pixelate.RootNodes[I] := LoadNode('castle-data:/screen_effects/pixelate.x3dv');
      Pixelate.EffectNodes[I] := Pixelate.RootNodes[I].FindNodeByName(TScreenEffectNode,
        'PixelateScreenEffect', true) as TScreenEffectNode;

      { In case of "edge detect" node,
        the EdgeDetect.EffectNodes[I] equals EdgeDetect.RootNodes[I].
        The X3D graph to define this effect is only a simple TScreenEffectNode,
        without any additions. }
      EdgeDetect.EffectNodes[I] := CreateEdgeDetectNode;
      EdgeDetect.RootNodes[I] := EdgeDetect.EffectNodes[I];

      { Synchronize Enabled state with buttons pressed }
      FilmGrain.EffectNodes[I].Enabled := FilmGrain.Button.Pressed;
      Pixelate.EffectNodes[I].Enabled := Pixelate.Button.Pressed;
      EdgeDetect.EffectNodes[I].Enabled := EdgeDetect.Button.Pressed;
    end;

    ScreenEffectsOnButtonImage.AddScreenEffect(FilmGrain.RootNodes[0]);
    ScreenEffectsOnButtonImage.AddScreenEffect(Pixelate.RootNodes[0]);
    ScreenEffectsOnButtonImage.AddScreenEffect(EdgeDetect.RootNodes[0]);

    SceneManager3D.AddScreenEffect(FilmGrain.RootNodes[1]);
    SceneManager3D.AddScreenEffect(Pixelate.RootNodes[1]);
    SceneManager3D.AddScreenEffect(EdgeDetect.RootNodes[1]);

    SceneManager2D.AddScreenEffect(FilmGrain.RootNodes[2]);
    SceneManager2D.AddScreenEffect(Pixelate.RootNodes[2]);
    SceneManager2D.AddScreenEffect(EdgeDetect.RootNodes[2]);
  end;

var
  Background: TCastleRectangleControl;
  ExampleImage: TCastleImageControl;
  ExampleButton: TCastleButton;
  Example3DScene: TCastleScene;
  Example2DScene: TCastle2DScene;
  Label1, Label2, Label3, LabelEffects: TCastleLabel;
  BottomControls: TCastleHorizontalGroup;
begin
  { Scalable UI (adjusts to any window size in a smart way). }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Insert almost-black background, otherwise (as Window is only TCastleWindowBase)
    the background would be undefined. }
  Background := TCastleRectangleControl.Create(Application);
  Background.Color := Vector4(0.1, 0.1, 0.1, 1);
  Background.FullSize := true;
  Window.Controls.InsertBack(Background);

  { Show a label with frames per second information }
  LabelFPS := TCastleLabel.Create(Application);
  LabelFPS.Anchor(vpTop, -10);
  LabelFPS.Anchor(hpRight, -10);
  LabelFPS.Color := Yellow;
  Window.Controls.InsertFront(LabelFPS);

  ScreenEffectsOnButtonImage := TCastleScreenEffects.Create(Application);
  ScreenEffectsOnButtonImage.Anchor(hpMiddle, - 300 - 50);
  ScreenEffectsOnButtonImage.Anchor(vpMiddle);
  ScreenEffectsOnButtonImage.Width := 300;
  ScreenEffectsOnButtonImage.Height := 300;
  Window.Controls.InsertFront(ScreenEffectsOnButtonImage);

  ExampleImage := TCastleImageControl.Create(Application);
  ExampleImage.URL := 'castle-data:/example_image.png';
  ExampleImage.Anchor(hpMiddle);
  ExampleImage.Anchor(vpTop);
  ExampleImage.Width := 300;
  ExampleImage.Height := 150;
  ExampleImage.Stretch := true;
  ScreenEffectsOnButtonImage.InsertFront(ExampleImage);

  ExampleButton := TCastleButton.Create(Application);
  ExampleButton.Caption := 'Example Button';
  ExampleButton.AutoSize := false;
  ExampleButton.Anchor(hpMiddle);
  ExampleButton.Anchor(vpBottom);
  ExampleButton.Width := 300;
  ExampleButton.Height := 150;
  ScreenEffectsOnButtonImage.InsertFront(ExampleButton);

  SceneManager2D := TCastle2DSceneManager.Create(Application);
  SceneManager2D.Anchor(hpMiddle);
  SceneManager2D.Anchor(vpMiddle);
  SceneManager2D.FullSize := false;
  SceneManager2D.Width := 300;
  SceneManager2D.Height := 300;
  SceneManager2D.BackgroundColor := Gray;
  SceneManager2D.ProjectionAutoSize := false;
  SceneManager2D.ProjectionWidth := 2000;
  SceneManager2D.ProjectionOriginCenter := true;
  Window.Controls.InsertFront(SceneManager2D);

  Example2DScene := TCastle2DScene.Create(Application);
  Example2DScene.Load('castle-data:/example_2d_scene/dragon/dragon.json');
  Example2DScene.Spatial := [ssRendering, ssDynamicCollisions];
  Example2DScene.ProcessEvents := true;
  Example2DScene.PlayAnimation('flying', true);
  SceneManager2D.Items.Add(Example2DScene);
  SceneManager2D.MainScene := Example2DScene;

  SceneManager3D := TCastleSceneManager.Create(Application);
  SceneManager3D.Anchor(hpMiddle, 300 + 50);
  SceneManager3D.Anchor(vpMiddle);
  SceneManager3D.FullSize := false;
  SceneManager3D.Width := 300;
  SceneManager3D.Height := 300;
  SceneManager3D.BackgroundColor := Gray;
  Window.Controls.InsertFront(SceneManager3D);

  Example3DScene := TCastleScene.Create(Application);
  Example3DScene.Load('castle-data:/example_3d_scene.x3dv');
  Example3DScene.Spatial := [ssRendering, ssDynamicCollisions];
  Example3DScene.ProcessEvents := true;
  Example3DScene.PlayAnimation('AnimateRotation', true);
  SceneManager3D.Items.Add(Example3DScene);
  SceneManager3D.MainScene := Example3DScene;

  Label1 := TCastleLabel.Create(Application);
  Label1.Caption := 'Example button and image';
  Label1.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label1.Anchor(hpMiddle, -300 - 50);
  Label1.Color := White;
  Window.Controls.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Application);
  Label2.Caption := '2D Scene Manager';
  Label2.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label2.Anchor(hpMiddle);
  Label2.Color := White;
  Window.Controls.InsertFront(Label2);

  Label3 := TCastleLabel.Create(Application);
  Label3.Caption := '3D Scene Manager';
  Label3.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label3.Anchor(hpMiddle, 300 + 50);
  Label3.Color := White;
  Window.Controls.InsertFront(Label3);

  BottomControls := TCastleHorizontalGroup.Create(Application);
  BottomControls.Anchor(hpMiddle);
  BottomControls.Anchor(vpBottom, 10);
  BottomControls.Frame := true;
  BottomControls.Padding := 10;
  BottomControls.Spacing := 10;
  Window.Controls.InsertFront(BottomControls);

  LabelEffects := TCastleLabel.Create(Application);
  LabelEffects.Caption := 'Toggle Screen Effects:';
  LabelEffects.Color := White;
  BottomControls.InsertFront(LabelEffects);

  FilmGrain.Button := TCastleButton.Create(Application);
  FilmGrain.Button.Caption := 'Film Grain';
  FilmGrain.Button.Toggle := true;
  FilmGrain.Button.OnClick := @TEventsHandler(nil).ToggleFilmGrain;
  BottomControls.InsertFront(FilmGrain.Button);

  Pixelate.Button := TCastleButton.Create(Application);
  Pixelate.Button.Caption := 'Pixelate';
  Pixelate.Button.Toggle := true;
  Pixelate.Button.OnClick := @TEventsHandler(nil).TogglePixelate;
  BottomControls.InsertFront(Pixelate.Button);

  EdgeDetect.Button := TCastleButton.Create(Application);
  EdgeDetect.Button.Caption := 'Edge Detect';
  EdgeDetect.Button.Toggle := true;
  EdgeDetect.Button.OnClick := @TEventsHandler(nil).ToggleEdgeDetect;
  BottomControls.InsertFront(EdgeDetect.Button);

  InitialiazeScreenEffects;
end;

initialization
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
end.
