{
  Copyright 2014-2017 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses Classes,
  CastleScene, CastleSceneManager, CastleUIControls, CastleCameras, CastleRays;

type
  { Scene manager best suited for 2D worlds.

    Features:

    @unorderedList(
      @item(The default @bold(camera position, direction, up) is suitable
        for 2D worlds that span horizontally in X,
        span vertically in Y,
        and are more-or-less flat around the Z = 0 plane.

        More precisely, the camera is positioned at the point
        @code((0, 0, DefaultCameraZ)),
        and looks along the -Z direction, with "up" vector in +Y.
      )

      @item(The @bold(camera does not give the user any automatic way
        to move in the world). Because you typically want to
        code from scratch all your own movement for 2D.

        More precisely, the TUniversalCamera.NavigationType is ntNone
        by default.
      )

      @item(Sets @bold(2D projection). By default
        our visible X range is @code([0..scene manager width in pixels]),
        visible Y is @code([0..scene manager height in pixels]).
        See the @link(ProjectionOriginCenter) for other options.

        Such projection is set regardless of the X3D viewpoint nodes
        present in the MainScene.
        This is in contrast to the ancestor TCastleSceneManager,
        that sets projection using a flexible
        algorithm that takes into account X3D viewpoint nodes,
        @link(TViewpointNode), in @link(TCastleSceneManager.MainScene).
      )

      @item(Sets Transparent = @true by default, which means that
        @bold(background underneath the scene manager is visible).
        Useful for 2D games where you often have an image or
        another background underneath,
        like TCastleImage or TCastleSimpleBackground.
      )
    ) }
  T2DSceneManager = class(TCastleSceneManager)
  private
    FProjectionAutoSize: boolean;
    FProjectionHeight, FProjectionWidth: Single;
    FCurrentProjectionWidth, FCurrentProjectionHeight: Single;
    FProjectionSpan: Single;
    FProjectionOriginCenter: boolean;
  protected
    function CalculateProjection: TProjection; override;
  public
    const
      DefaultProjectionSpan = 1000.0;
      DefaultCameraZ = DefaultProjectionSpan / 2;

    { When ProjectionAutoSize is @true, the size of the world visible
      in our viewport depends on scene manager size.
      ProjectionHeight and ProjectionWidth are ignored then.

      When ProjectionAutoSize is @false, ProjectionHeight and ProjectionWidth
      are used to determine the world visible in our viewport.
      If one of them is zero, the other is automatically adjusted to
      follow aspect ratio of viewport size.
      If both of them are zero, projection is automatically calculated just as
      if ProjectionAutoSize was @true.

      In all cases, CurrentProjectionWidth and CurrentProjectionHeight
      can be checked to see actual projection dimensions. }
    property ProjectionAutoSize: boolean
      read FProjectionAutoSize write FProjectionAutoSize default true;
    property ProjectionHeight: Single
      read FProjectionHeight write FProjectionHeight default 0;
    property ProjectionWidth: Single
      read FProjectionWidth write FProjectionWidth default 0;
    property CurrentProjectionWidth: Single read FCurrentProjectionWidth;
    property CurrentProjectionHeight: Single read FCurrentProjectionHeight;
    property ProjectionSpan: Single
      read FProjectionSpan write FProjectionSpan default DefaultProjectionSpan;

    { Where is the (0,0) world point with respect to the viewport.

      If @false, the (0,0) is in the left-bottom corner, which matches
      the typical 2D drawing coordinates used throughout our engine.
      In other words, if the camera is at position (0,0,whatever),
      then the (0,0) position in 2D is in the left-bottom corner of the scene manager
      rectangle.

      If @true, the (0,0) is in the middle of the viewport.
      In other words, if the camera is at position (0,0,whatever),
      then the (0,0) position is in the center of the scene manager
      rectangle.

      Both values of @name make sense,
      it depends on the game type and how you prefer to think in 2D coordinates.
      And how do you want the result to behave when aspect ratio changes:

      @unorderedList(
        @item(With ProjectionOriginCenter = @true, things will stay "glued"
          to the center.)
        @item(With ProjectionOriginCenter = @false, things will stay "glued"
          to the left-bottom corner.)
      )
    }
    property ProjectionOriginCenter: boolean
      read FProjectionOriginCenter write FProjectionOriginCenter default false;

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

uses CastleBoxes, CastleVectors, CastleGLUtils, X3DNodes;

{ T2DSceneManager -------------------------------------------------------- }

constructor T2DSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  Transparent := true;
  ProjectionAutoSize := true;
  FProjectionSpan := DefaultProjectionSpan;
  FProjectionHeight := 0;
  FProjectionWidth := 0;
end;

function T2DSceneManager.CreateDefaultCamera(AOwner: TComponent): TCamera;
var
  UCamera: TUniversalCamera;
begin
  UCamera := TUniversalCamera.Create(AOwner);
  UCamera.NavigationType := ntNone;
  UCamera.SetInitialView(
    { pos } Vector3Single(0, 0, DefaultCameraZ),
    { dir } Vector3Single(0, 0, -1),
    { up } Vector3Single(0, 1, 0), false);
  UCamera.GoToInitial;
  UCamera.Radius := 0.01; { will not be used for anything, but set to something sensible just in case }
  Result := UCamera;
end;

function T2DSceneManager.CalculateProjection: TProjection;
var
  ControlWidth, ControlHeight: Integer;
begin
  Result.ProjectionType := ptOrthographic;
  Result.OrthoDimensions[0] := 0;
  Result.OrthoDimensions[1] := 0;

  ControlWidth := CalculatedWidth;
  ControlHeight := CalculatedHeight;

  if ProjectionAutoSize or
     ((ProjectionWidth = 0) and (ProjectionHeight = 0)) then
  begin
    FCurrentProjectionWidth := ControlWidth;
    FCurrentProjectionHeight := ControlHeight;
  end else
  if ProjectionWidth = 0 then
  begin
    FCurrentProjectionWidth := ProjectionHeight * ControlWidth / ControlHeight;
    FCurrentProjectionHeight := ProjectionHeight;
  end else
  if ProjectionHeight = 0 then
  begin
    FCurrentProjectionWidth := ProjectionWidth;
    FCurrentProjectionHeight := ProjectionWidth * ControlHeight / ControlWidth;
  end else
  begin
    FCurrentProjectionWidth := ProjectionWidth;
    FCurrentProjectionHeight := ProjectionHeight;
  end;

  Result.OrthoDimensions[2] := FCurrentProjectionWidth;
  Result.OrthoDimensions[3] := FCurrentProjectionHeight;
  if FProjectionOriginCenter then
  begin
    Result.OrthoDimensions[2] := Result.OrthoDimensions[2] / 2;
    Result.OrthoDimensions[3] := Result.OrthoDimensions[3] / 2;
    Result.OrthoDimensions[0] := -Result.OrthoDimensions[2]; // already divided by /2
    Result.OrthoDimensions[1] := -Result.OrthoDimensions[3]; // already divided by /2
  end;
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
