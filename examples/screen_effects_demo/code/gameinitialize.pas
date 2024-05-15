{
  Copyright 2018-2023 Michalis Kamburelis.

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

{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleScreenEffects,
  CastleViewport, X3DNodes, X3DLoad, CastleUtils, CastleCameras,
  CastleRenderOptions, CastleVectors;

var
  Window: TCastleWindow;
  LabelFPS: TCastleLabel;

  { 3 controls below have methods AddScreenEffect / RemoveScreenEffect
    that we will use.

    Note that we *could* insert each TCastleViewport
    as child of a new TCastleScreenEffects instance,
    and thus have 3 TCastleScreenEffects instances like this:

    - ScreenEffectsOnButtonImage,
    - ScreenEffectsOnViewport3D
    - ScreenEffectsOnViewport2D

    However, for demo, we demonstrate below another approach:
    Since TCastleViewport already descends from TCastleScreenEffects,
    we can call AddScreenEffect method directly on TCastleViewport.
    So we don't need something like ScreenEffectsOnViewport3D, ScreenEffectsOnViewport2D. }
  ScreenEffectsOnButtonImage: TCastleScreenEffects;
  Viewport2D: TCastleViewport;
  Viewport3D: TCastleViewport;

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
      for Viewport2D,
      for Viewport3D). }
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

procedure WindowUpdate(Container: TCastleContainer);
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
      '  float x = screenf_x();' + NL +
      '  float y = screenf_y();' + NL +
      '  #define SCAN_DISTANCE 1.0' + NL +
      '  vec4 left   = screenf_get_color(vec2(x - SCAN_DISTANCE, y));' + NL +
      '  vec4 right  = screenf_get_color(vec2(x + SCAN_DISTANCE, y));' + NL +
      '  vec4 top    = screenf_get_color(vec2(x, y - SCAN_DISTANCE));' + NL +
      '  vec4 bottom = screenf_get_color(vec2(x, y + SCAN_DISTANCE));' + NL +
      '  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;' + NL +
      '}';

    ComposedShader := TComposedShaderNode.Create;
    ComposedShader.SetParts([FragmentShader]);

    Result := TScreenEffectNode.Create;
    Result.SetShaders([ComposedShader]);
  end;

  procedure InitializeScreenEffects;
  var
    I: Integer;
  begin
    { The implementation of this procedure got complicated due to the amount
      of things I wanted to demonstrate. But the basic API to add screen effects
      is very simple: you want to load the X3D graph containing one or more
      TScreenEffectNode, and call AddScreenEffect with it.
      For example, this is how to load film grain effect in Viewport3D:

      Viewport3D.AddScreenEffect(
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
        Viewport3D.AddScreenEffect(RootNode);
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
      FilmGrain.EffectNodes[I] := FilmGrain.RootNodes[I].FindNode(TScreenEffectNode,
        'FilmGrainScreenEffect') as TScreenEffectNode;

      Pixelate.RootNodes[I] := LoadNode('castle-data:/screen_effects/pixelate.x3dv');
      Pixelate.EffectNodes[I] := Pixelate.RootNodes[I].FindNode(TScreenEffectNode,
        'PixelateScreenEffect') as TScreenEffectNode;

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

    Viewport3D.AddScreenEffect(FilmGrain.RootNodes[1]);
    Viewport3D.AddScreenEffect(Pixelate.RootNodes[1]);
    Viewport3D.AddScreenEffect(EdgeDetect.RootNodes[1]);

    Viewport2D.AddScreenEffect(FilmGrain.RootNodes[2]);
    Viewport2D.AddScreenEffect(Pixelate.RootNodes[2]);
    Viewport2D.AddScreenEffect(EdgeDetect.RootNodes[2]);
  end;

var
  Background: TCastleRectangleControl;
  ExampleImage: TCastleImageControl;
  ExampleButton: TCastleButton;
  Example3DScene: TCastleScene;
  Example2DScene: TCastleScene;
  Label1, Label2, Label3, LabelEffects: TCastleLabel;
  BottomControls: TCastleHorizontalGroup;
begin
  { Scalable UI (adjusts to any window size in a smart way). }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Insert almost-black background, otherwise (as Window is only TCastleWindow)
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

  Viewport2D := TCastleViewport.Create(Application);
  Viewport2D.Setup2D;
  Viewport2D.Anchor(hpMiddle);
  Viewport2D.Anchor(vpMiddle);
  Viewport2D.FullSize := false;
  Viewport2D.Width := 300;
  Viewport2D.Height := 300;
  Viewport2D.BackgroundColor := Gray;
  Viewport2D.Camera.Orthographic.Width := 2000;
  Viewport2D.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Window.Controls.InsertFront(Viewport2D);

  Example2DScene := TCastleScene.Create(Application);
  Example2DScene.Load('castle-data:/example_2d_scene/dragon/dragon.json');
  Example2DScene.PreciseCollisions := true;
  Example2DScene.ProcessEvents := true;
  Example2DScene.PlayAnimation('flying', true);
  Viewport2D.Items.Add(Example2DScene);
  Viewport2D.AssignDefaultCamera;

  Viewport3D := TCastleViewport.Create(Application);
  Viewport3D.InsertBack(TCastleExamineNavigation.Create(Application));
  Viewport3D.Anchor(hpMiddle, 300 + 50);
  Viewport3D.Anchor(vpMiddle);
  Viewport3D.FullSize := false;
  Viewport3D.Width := 300;
  Viewport3D.Height := 300;
  Viewport3D.BackgroundColor := Gray;
  Window.Controls.InsertFront(Viewport3D);

  Example3DScene := TCastleScene.Create(Application);
  Example3DScene.Load('castle-data:/example_3d_scene.x3dv');
  Example3DScene.PreciseCollisions := true;
  Example3DScene.ProcessEvents := true;
  Example3DScene.PlayAnimation('AnimateRotation', true);
  Viewport3D.Items.Add(Example3DScene);
  Viewport3D.Camera.Add(TCastleDirectionalLight.Create(Application)); // headlight
  Viewport3D.AssignDefaultCamera;

  Label1 := TCastleLabel.Create(Application);
  Label1.Caption := 'Example button and image';
  Label1.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label1.Anchor(hpMiddle, -300 - 50);
  Label1.Color := White;
  Window.Controls.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Application);
  Label2.Caption := '2D Viewport';
  Label2.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label2.Anchor(hpMiddle);
  Label2.Color := White;
  Window.Controls.InsertFront(Label2);

  Label3 := TCastleLabel.Create(Application);
  Label3.Caption := '3D Viewport';
  Label3.Anchor(vpBottom, vpMiddle, 150 + 20);
  Label3.Anchor(hpMiddle, 300 + 50);
  Label3.Color := White;
  Window.Controls.InsertFront(Label3);

  BottomControls := TCastleHorizontalGroup.Create(Application);
  BottomControls.Anchor(hpMiddle);
  BottomControls.Anchor(vpBottom, 10);
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
  FilmGrain.Button.OnClick := {$ifdef FPC}@{$endif} TEventsHandler.ToggleFilmGrain;
  BottomControls.InsertFront(FilmGrain.Button);

  Pixelate.Button := TCastleButton.Create(Application);
  Pixelate.Button.Caption := 'Pixelate';
  Pixelate.Button.Toggle := true;
  Pixelate.Button.OnClick := {$ifdef FPC}@{$endif} TEventsHandler.TogglePixelate;
  BottomControls.InsertFront(Pixelate.Button);

  EdgeDetect.Button := TCastleButton.Create(Application);
  EdgeDetect.Button.Caption := 'Edge Detect';
  EdgeDetect.Button.Toggle := true;
  EdgeDetect.Button.OnClick := {$ifdef FPC}@{$endif} TEventsHandler.ToggleEdgeDetect;
  BottomControls.InsertFront(EdgeDetect.Button);

  InitializeScreenEffects;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
