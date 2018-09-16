{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager (TCastle2DSceneManager) and scene (TCastle2DScene) best suited for 2D worlds. }
unit Castle2DSceneManager;

{$I castleconf.inc}

interface

uses Classes,
  CastleScene, CastleSceneManager, CastleUIControls, CastleCameras, CastleProjection;

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
        code yourself all your camera movement for 2D games.

        More precisely, the NavigationType is ntNone by default.
      )

      @item(Sets @bold(2D projection). By default
        our visible X range is @code([0..scene manager width in pixels]),
        visible Y range is @code([0..scene manager height in pixels]).

        You can set the @link(ProjectionAutoSize) to @false,
        and then set @link(ProjectionWidth) or @link(ProjectionHeight)
        to explicitly control the size of the game world @italic(inside the scene manager),
        regardless of the size of the scene manager control.
        This is a trivial way to scale your 2D game contents.

        You can set the @link(ProjectionOriginCenter) to control where the
        origin is (how does the Camera.Position affect the view).

        Such projection is set regardless of the X3D viewpoint nodes
        present in the MainScene.
        This is in contrast to the ancestor TCastleSceneManager,
        that sets projection using a flexible
        algorithm that takes into account X3D viewpoint nodes,
        @link(TViewpointNode), in @link(TCastleSceneManager.MainScene).
      )
    ) }
  TCastle2DSceneManager = class(TCastleSceneManager)
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

    { Determines the minimum and maximum depth visible, relative to the camera Z.

      Higher values allow to see more.
      The objects are visible in Z range
      @code([Camera.Position.Z - ProjectionSpan ..
      Camera.Position.Z + ProjectionSpan]).

      Lower values improve depth buffer precision. }
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
    procedure AssignDefaultCamera; override;
  end;

  T2DSceneManager = class(TCastle2DSceneManager)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Transparent default true;
  end deprecated 'use TCastle2DSceneManager instead (and note that it has different default Transparent value)';

  { Scene best suited for 2D models. Sets BlendingSort := bs2D,
    good when your transparent objects have proper order along the Z axis
    (useful e.g. for Spine animations). }
  TCastle2DScene = class(TCastleScene)
  public
    constructor Create(AOwner: TComponent); override;

    { Create a scene with the same contents (X3D scene graph) as this one.
      Note that this @bold(does not copy other scene attributes),
      like @link(ProcessEvents) or @link(Spatial) or rendering attributes
      in @link(Attributes). }
    function Clone(const AOwner: TComponent): TCastle2DScene;
  end;

  T2DScene = TCastle2DScene deprecated 'use TCastle2DScene';

implementation

uses CastleBoxes, CastleVectors, CastleGLUtils, X3DNodes, CastleComponentSerialize;

{ TCastle2DSceneManager -------------------------------------------------------- }

constructor TCastle2DSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  ProjectionAutoSize := true;
  FProjectionSpan := DefaultProjectionSpan;
  FProjectionHeight := 0;
  FProjectionWidth := 0;

  { Make camera already existing, so WalkCamera returns it,
    instead of using AssignDefaultCamera and then switching to ntWalk. }
  AssignDefaultCamera;
end;

procedure TCastle2DSceneManager.AssignDefaultCamera;
begin
  { Set Camera explicitly, otherwise SetNavigationType below could call
    ExamineCamera / WalkCamera that call AssignDefaultCamera when Camera = nil,
    and we would have infinite AssignDefaultCamera calls loop. }
  Camera := InternalWalkCamera;

  NavigationType := ntNone;
  Camera.SetInitialView(
    { pos } Vector3(0, 0, DefaultCameraZ),
    { dir } Vector3(0, 0, -1),
    { up } Vector3(0, 1, 0), false);
  Camera.GoToInitial;
  Camera.Radius := 0.01; { will not be used for anything, but set to something sensible just in case }
end;

function TCastle2DSceneManager.CalculateProjection: TProjection;
var
  ControlWidth, ControlHeight: Single;
begin
  Result.ProjectionType := ptOrthographic;
  Result.Dimensions.Left := 0;
  Result.Dimensions.Bottom := 0;

  ControlWidth := EffectiveWidth;
  ControlHeight := EffectiveHeight;

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

  Result.Dimensions.Width  := FCurrentProjectionWidth;
  Result.Dimensions.Height := FCurrentProjectionHeight;
  if FProjectionOriginCenter then
  begin
    Result.Dimensions.Left   := -Result.Dimensions.Width / 2;
    Result.Dimensions.Bottom := -Result.Dimensions.Height / 2;
  end;
  Result.ProjectionNear := -ProjectionSpan;
  Result.ProjectionFar := ProjectionSpan;
  Result.ProjectionFarFinite := Result.ProjectionFar;
end;

{ T2DSceneManager ------------------------------------------------------------ }

constructor T2DSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  Transparent := true;
end;

{ TCastle2DScene --------------------------------------------------------------- }

constructor TCastle2DScene.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.BlendingSort := bs2D;
end;

function TCastle2DScene.Clone(const AOwner: TComponent): TCastle2DScene;
begin
  Result := TCastle2DScene.Create(AOwner);
  if RootNode <> nil then
    Result.Load(RootNode.DeepCopy as TX3DRootNode, true);
end;

initialization
  RegisterSerializableComponent(TCastle2DSceneManager, '2D Scene Manager');
  RegisterSerializableComponent(TCastle2DScene, '2D Scene');
end.
