{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Show Spherical harmonics function basis.
  Single SH function is shown on a sphere, yellow indicates positive values,
  blue negative values.

  Navigate with mouse or keyboard (like view3dscene in Examine mode).
}

program show_sh;

uses Classes, CastleFrustum, Castle3D,
  CastleVectors, CastleBoxes, CastleWindow, CastleUIControls,
  CastleClassUtils, CastleUtils, SysUtils, CastleFilesUtils, CastleControls,
  CastleGLUtils, CastleCameras, Math, CastleSphereSampling, CastleSphericalHarmonics,
  CastleSceneManager, CastleScene, X3DNodes, CastleShapes,
  CastleStringUtils, CastleKeysMouse, CastleColors;

var
  Window: TCastleWindow;

  LM: Cardinal = 0;

  MinSHValue, MaxSHValue: Float;

procedure Render(Container: TUIContainer);
var
  L: Cardinal;
  M: Integer;
begin
  LMDecode(LM, L, M);
  UIFont.Print(10, 10, Yellow,
    Format('Spherical harmonic number %d. (L, M) = (%d, %d). Results in range (%f, %f)',
    [LM, L, M, MinSHValue, MaxSHValue]));
end;

type
  TMyScene = class(TCastleScene)
  private
    procedure VertexColor(var Color: TVector3Single;
      Shape: TShape; const VertexPosition: TVector3Single;
      VertexIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

procedure TMyScene.VertexColor(var Color: TVector3Single;
  Shape: TShape; const VertexPosition: TVector3Single;
  VertexIndex: Integer);
var
  SH: Float;
begin
  SH := SHBasis(LM, XYZToPhiTheta(VertexPosition));
  if SH > MaxSHValue then MaxSHValue := SH;
  if SH < MinSHValue then MinSHValue := SH;

  if SH >= 0 then
    Color := Vector3Single(SH, SH, 0) else
    Color := Vector3Single(0, 0, -SH);
end;

constructor TMyScene.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
  Shape: TShapeNode;
  SphereNode: TSphereNode;
begin
  inherited;

  Attributes.OnVertexColor := @VertexColor;

  SphereNode := TSphereNode.Create;

  Shape := TShapeNode.Create;
  Shape.FdGeometry.Value := SphereNode;

  Root := TX3DRootNode.Create;
  Root.FdChildren.Add(Shape);

  Load(Root, true);
end;

procedure TMyScene.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    { before every rendering clear Min/MaxSHValue, so that VertexColor can set them }
    MinSHValue :=  MaxFloat;
    MaxSHValue := -MaxFloat;
  end;
  inherited;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);
begin
  case Item.IntData of
    10: LM := ChangeIntCycle(LM, -1, MaxSHBasis - 1);
    20: LM := ChangeIntCycle(LM, +1, MaxSHBasis - 1);
    else Exit;
  end;
  Window.Invalidate;
end;

var
  M: TMenu;
begin
  Window := TCastleWindow.Create(Application);

  DefaultTriangulationSlices := 60;
  DefaultTriangulationStacks := 60;

  Window.MainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItem.Create('_Previous basis', 10, 'p'));
    M.Append(TMenuItem.Create('_Next basis', 20, 'n'));
    Window.MainMenu.Append(M);

  Window.SceneManager.MainScene := TMyScene.Create(Application);
  Window.SceneManager.Items.Add(Window.SceneManager.MainScene);

  Window.OnMenuClick := @MenuClick;
  Window.OnRender := @Render;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
