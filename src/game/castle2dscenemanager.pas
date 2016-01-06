{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager (T2DSceneManager) and scene (T2DScene) best suited for 2D worlds. }
unit Castle2DSceneManager;

interface

uses Classes,
  CastleScene, CastleSceneManager, CastleUIControls, CastleCameras, CastleRays;

type
  { Scene manager best suited for 2D worlds.

    Features:
    @unorderedList(
      @item(By default creates camera looks down in -Z,
        good when your world spans in XY plane.
        And TUniversalCamera.NavigationType is ntNone, which means that no
        automatic way to move camera in the world is possible,
        you want to program your own movement for 2D.)

      @item(Sets 2D projection. By default (see CalculateProjection)
        our visible X range is [0..window width in pixels],
        visible Y is [0..window height in pixels].

        Such projection is set regardless of the viewpoints defined in MainScene
        (this is unlike ancestor TCastleSceneManager,
        that sets projection using a flexible
        algorithm that takes into account VRML/X3D viewpoints in MainScene).
      )

      @item(Sets RenderStyle = rs2D by default, which makes it possible
        to place the scene manager rendering in the middle of other 2D controls
        (for example, over some 2D background and before some 2D buttons.))

      @item(Sets Transparent = @true by default, which means that
        background underneath will be visible. Useful for 2D games
        where you usually have an image or another background underneath,
        like TCastleImage or TCastleSimpleBackground.)
    ) }
  T2DSceneManager = class(TCastleSceneManager)
  private
    FProjectionAutoSize: boolean;
    FProjectionHeight, FProjectionWidth: Single;
    FCurrentProjectionWidth, FCurrentProjectionHeight: Single;
    FProjectionSpan: Single;
  protected
    function CalculateProjection: TProjection; override;
  public
    const
      DefaultProjectionSpan = 1000.0;
    property RenderStyle default rs2D;

    { When ProjectionAutoSize is @true, the size of the world visible
      in our viewport depends on scene manager size.
      ProjectionHeight and ProjectionWidth are ignored then.

      When ProjectionAutoSize is @false, ProjectionHeight and ProjectionWidth
      are used to determine the world visible in our viewport.
      If one of them is zero, the other is automatically adjusted to
      follow aspect ratio of viewport size.
      If both of them are zero, projection is automatically calculated just as if
      ProjectionAutoSize was @true.
      @bold(By default, height is 1.0 and width is automatically adjusted
      to follow aspect ratio.)

      In all cases, CurrentProjectionWidth and CurrentProjectionHeight
      can be checked to see actual projection dimensions. }
    property ProjectionAutoSize: boolean
      read FProjectionAutoSize write FProjectionAutoSize default true;
    property ProjectionHeight: Single
      read FProjectionHeight write FProjectionHeight default 1;
    property ProjectionWidth: Single
      read FProjectionWidth write FProjectionWidth default 0;
    property CurrentProjectionWidth: Single read FCurrentProjectionWidth;
    property CurrentProjectionHeight: Single read FCurrentProjectionHeight;
    property ProjectionSpan: Single
      read FProjectionSpan write FProjectionSpan default DefaultProjectionSpan;

    constructor Create(AOwner: TComponent); override;
    function CreateDefaultCamera(AOwner: TComponent): TCamera; override;
  published
    property Transparent default true;
  end;

  { Scene best suited for 2D models. Sets BlendingSort := bs2D,
    good when your transparent objects have proper order along the Z axis
    (useful e.g. for Spine animations). }
  T2DScene = class(TCastleScene)
  public
    constructor Create(AOwner: TComponent); override;

    { Create a scene with the same contents (X3D scene graph) as this one.
      Note that this @bold(does not copy other scene attributes),
      like @link(ProcessEvents) or @link(Spatial) or rendering attributes
      in @link(Attributes). }
    function Clone(const AOwner: TComponent): T2DScene;
  end;

implementation

uses CastleVectors, CastleGLUtils, X3DNodes;

{ T2DSceneManager -------------------------------------------------------- }

constructor T2DSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  RenderStyle := rs2D;
  Transparent := true;
  ProjectionAutoSize := true;
  FProjectionSpan := DefaultProjectionSpan;
  FProjectionHeight := 1;
  FProjectionWidth := 0;
end;

function T2DSceneManager.CreateDefaultCamera(AOwner: TComponent): TCamera;
var
  UCamera: TUniversalCamera;
begin
  UCamera := TUniversalCamera.Create(AOwner);
  UCamera.NavigationType := ntNone;
  UCamera.SetInitialView(
    { pos } Vector3Single(0, 0, 0),
    { dir } Vector3Single(0, 0, -1),
    { up } Vector3Single(0, 1, 0), false);
  UCamera.GoToInitial;
  UCamera.Radius := 0.01; { will not be used for anything, but set to something sensible just in case }
  Result := UCamera;
end;

function T2DSceneManager.CalculateProjection: TProjection;
begin
  Result.ProjectionType := ptOrthographic;
  Result.OrthoDimensions[0] := 0;
  Result.OrthoDimensions[1] := 0;

  if ProjectionAutoSize or
     ((ProjectionWidth = 0) and (ProjectionHeight = 0)) then
  begin
    FCurrentProjectionWidth := Rect.Width;
    FCurrentProjectionHeight := Rect.Height;
  end else
  if ProjectionWidth = 0 then
  begin
    FCurrentProjectionWidth := ProjectionHeight * Rect.Width / Rect.Height;
    FCurrentProjectionHeight := ProjectionHeight;
  end else
  if ProjectionHeight = 0 then
  begin
    FCurrentProjectionWidth := ProjectionWidth;
    FCurrentProjectionHeight := ProjectionWidth * Rect.Height / Rect.Width;
  end else
  begin
    FCurrentProjectionWidth := ProjectionWidth;
    FCurrentProjectionHeight := ProjectionHeight;
  end;

  Result.OrthoDimensions[2] := FCurrentProjectionWidth;
  Result.OrthoDimensions[3] := FCurrentProjectionHeight;
  Result.ProjectionNear := -ProjectionSpan;
  Result.ProjectionFar := ProjectionSpan;
  Result.ProjectionFarFinite := Result.ProjectionFar;
end;

{ T2DScene --------------------------------------------------------------- }

constructor T2DScene.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.BlendingSort := bs2D;
end;

function T2DScene.Clone(const AOwner: TComponent): T2DScene;
begin
  Result := T2DScene.Create(AOwner);
  if RootNode <> nil then
    Result.Load(RootNode.DeepCopy as TX3DRootNode, true);
end;

end.
