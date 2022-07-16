{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

program visualize_spherical_harmonics;

uses SysUtils, Classes, Math,
  CastleFrustum, CastleVectors, CastleBoxes, CastleWindow, CastleUIControls,
  CastleClassUtils, CastleUtils, CastleFilesUtils, CastleControls,
  CastleGLUtils, CastleCameras, CastleInternalSphereSampling, CastleInternalSphericalHarmonics,
  CastleViewport, CastleScene, X3DNodes, CastleShapes,
  CastleStringUtils, CastleKeysMouse, CastleColors, CastleTransform,
  SceneUtilities;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;

  LM: Cardinal = 0;

  MinSHValue, MaxSHValue: Float;

procedure Render(Container: TCastleContainer);
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
    function VertexColor(
      const Shape: TShape;
      const VertexPosition: TVector3;
      const VertexIndex: Integer): TCastleColorRGB;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LocalRender(const Params: TRenderParams); override;
  end;

function TMyScene.VertexColor(
  const Shape: TShape;
  const VertexPosition: TVector3;
  const VertexIndex: Integer): TCastleColorRGB;
var
  SH: Float;
begin
  SH := SHBasis(LM, XYZToPhiTheta(VertexPosition));
  if SH > MaxSHValue then MaxSHValue := SH;
  if SH < MinSHValue then MinSHValue := SH;

  if SH >= 0 then
    Result := Vector3(SH, SH, 0)
  else
    Result := Vector3(0, 0, -SH);
end;

constructor TMyScene.Create(AOwner: TComponent);
begin
  inherited;

  { Load a scene with sphere.

    Why not using TSphereNode?
    Because our SetSceneColor doesn't support changing colors on TSphereNode. }
  Load('castle-data:/sphere.gltf');
end;

procedure TMyScene.LocalRender(const Params: TRenderParams);
begin
  if (not Params.Transparent) and
     (true in Params.ShadowVolumesReceivers) then
  begin
    { before every rendering clear Min/MaxSHValue, so that VertexColor can set them }
    MinSHValue :=  MaxFloat;
    MaxSHValue := -MaxFloat;
    SetSceneColors(Self, @VertexColor);
  end;

  inherited;
end;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);
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

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Window.Controls.InsertFront(Viewport);

  DefaultTriangulationSlices := 60;
  DefaultTriangulationStacks := 60;

  Window.MainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItem.Create('_Previous basis', 10, 'p'));
    M.Append(TMenuItem.Create('_Next basis', 20, 'n'));
    Window.MainMenu.Append(M);

  Viewport.Items.MainScene := TMyScene.Create(Application);
  Viewport.Items.Add(Viewport.Items.MainScene);

  Window.OnMenuClick := @MenuClick;
  Window.OnRender := @Render;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.OpenAndRun;
end.
