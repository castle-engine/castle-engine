{
  Copyright 2023-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleScene, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleGLImages, CastleViewport,
  CastleTransform, CastleGLUtils;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelGlInformation: TCastleLabel;
    ScenePhong: TCastleScene;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    UnlitMeshParent: TCastleTransform;
  private
    DrawableImage: TDrawableImage;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure RenderOverChildren; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleLoadGltf, CastleRectangles, CastleImages,
  CastleBoxes, CastleColors, CastleRenderContext, CastleUtils, X3DLoad,
  GameMyMesh;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  MyMesh: TMyMesh;
begin
  inherited;

  LabelGlInformation.Caption :=
    'OpenGL capabilities requested: ' + CapabilitiesStr[TGLFeatures.RequestCapabilities] + NL +
    '(see log for the details about context)';

  { Load ScenePhong.Url with temporary GltfForcePhongMaterials set to true.
    We want to test Phong shading works in ForceFixedFunction too. }
  GltfForcePhongMaterials := true;
  ScenePhong.Url := 'castle-data:/sample_3d.gltf';
  GltfForcePhongMaterials := false;

  DrawableImage := TDrawableImage.Create('castle-data:/texture_alpha.png');

  MyMesh := TMyMesh.Create(FreeAtStop);
  UnlitMeshParent.Add(MyMesh);

  MainViewport.AddScreenEffect(LoadNode('castle-data:/screen_effect_frame.x3dv'));
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(DrawableImage);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  WalkNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

procedure TViewMain.RenderOverChildren;
begin
  inherited;
  { Do some direct drawing, to test this API works with ForceFixedFunction too. }
  DrawRectangle(FloatRectangle(10, 10, 50, 50), Green);
  DrawableImage.Draw(100, 10);

  { Render DrawableImage again, forcing alpha testing }
  DrawableImage.Alpha := acTest;
  DrawableImage.Draw(400, 10);
  DrawableImage.Alpha := acAuto;

  FallbackFont.Print(700, 10, Red, 'Another sample text');
end;

end.
