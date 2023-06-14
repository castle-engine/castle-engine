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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleTransform, CastleScene, CastleTimeUtils, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
  private
    type
      TRotateBehavior = class(TCastleBehavior)
      public
        LifeTime: TFloatTime;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;
    var
      StarsCount: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleVectors, CastleRenderOptions, X3DNodes, CastleUtils;

{ TViewMain.TRotateBehavior ------------------------------------------------- }

procedure TViewMain.TRotateBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  Parent.Rotation := Vector4(0, 0, 1, LifeTime);
end;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  StarTemplate: TCastleScene;

  procedure InstantiateStar(const Center: TVector3; const Radius: Single;
    const RecursionLevel: Cardinal; const ParentTransform: TCastleTransform);
  const
    CircleCount = 8;
    RadiusDecrease = 0.75; // each recursion step decreases radius
    MaxRecursionLevel = 4;
  var
    I: Integer;
    S, C: Float;
    Material: TUnlitMaterialNode;
    RotateBeh: TRotateBehavior;
    Scene: TCastleScene;
  begin
    Scene := StarTemplate.Clone(FreeAtStop);
    Scene.Translation := Center;
    Inc(StarsCount);

    Material := (Scene.Node(TAppearanceNode, 'MatMain') as TAppearanceNode).
      Material as TUnlitMaterialNode;
    Material.EmissiveColor := Vector3(
      RecursionLevel / MaxRecursionLevel,
      RecursionLevel / MaxRecursionLevel,
      1.0);

    RotateBeh := TRotateBehavior.Create(FreeAtStop);
    Scene.AddBehavior(RotateBeh);
    ParentTransform.Add(Scene);

    if RecursionLevel + 1 < MaxRecursionLevel then
      for I := 0 to CircleCount - 1 do
      begin
        SinCos(2 * Pi * I / CircleCount, S, C);
        InstantiateStar(Center + Vector3(S * Radius, C * Radius, 0),
          Radius * RadiusDecrease, RecursionLevel +  1, Scene);
      end;
  end;

begin
  inherited;

  StarTemplate := TCastleScene.Create(FreeAtStop);
  StarTemplate.Load('castle-data:/star.gltf');

  StarsCount := 0;
  InstantiateStar(Vector3(0, 0, 0), 10, 0, MainViewport.Items);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    'Star scenes: ' + IntToStr(StarsCount);
end;

end.
