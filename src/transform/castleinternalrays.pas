{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Calculating 3D rays that correspond to the given points on 2D screen.
  This is used by ray-tracing (casting a ray for each image pixel)
  or when picking objects (what 3D object/point is indicated by
  the current mouse position). }
unit CastleInternalRays;

{$I castleconf.inc}

interface

uses CastleVectors, CastleProjection, CastleRectangles;

type
  { Calculate primary rays for given camera settings and screen size. }
  TRaysWindow = class
  private
    FCamPosition, FCamDirection, FCamUp: TVector3;
    CamSide: TVector3;
  public
    { Camera vectors. Initialized in the constructor.
      Must be given already normalized.
      Note that CamUp may be changed in constructor, to be always perfectly
      orthogonal to CamDirection.

      @groupBegin }
    property CamPosition: TVector3 read FCamPosition;
    property CamDirection: TVector3 read FCamDirection;
    property CamUp: TVector3 read FCamUp;
    { @groupEnd }

    { Constructor. Calculates some values based on camera settings,
      this way many calls to PrimaryRay for the same camera settings
      are fast (useful for ray-tracers). }
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3);

    { Create appropriate TRaysWindow instance.
      Constructs non-abstract descendant (TPerspectiveRaysWindow or
      TOrthographicRaysWindow, depending on Projection.ProjectionType). }
    class function CreateDescendant(
      const ACamPosition, ACamDirection, ACamUp: TVector3;
      const Projection: TProjection): TRaysWindow;

    { Calculate position and direction of the primary ray cast from CamPosition,
      going through the pixel X, Y.

      X, Y coordinates start from (0, 0) if bottom left (like in typical 2D
      OpenGL).  When they are integers and in the range of
      X = 0..ScreenWidth-1 (left..right), Y = 0..ScreenHeight-1 (bottom..top)
      it's guaranteed that resulting ray will go exactly through the middle
      of the appropriate pixel (on imaginary "rzutnia" = image positioned
      paraller to view direction). But you can provide non-integer X, Y,
      useful for multisampling (taking many samples within the pixel,
      like (X, Y) = (PixX + Random - 0.5, PixY + Random - 0.5)).

      Resulting RayDirection is guaranteed to be normalized
      (this is in practice not costly to us, and it often helps --- when ray
      direction is normalized, various distances from ray collisions are "real"). }
    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Single;
      out RayOrigin, RayDirection: TVector3); virtual; abstract;
  end;

  TPerspectiveRaysWindow = class(TRaysWindow)
  private
    WindowWidth, WindowHeight: Single;
    PerspectiveAngles: TVector2;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3;
      const APerspectiveAngles: TVector2);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Single;
      out RayOrigin, RayDirection: TVector3); override;
  end;

  TFrustumRaysWindow = class(TRaysWindow)
  private
    Dimensions: TFloatRectangle;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3;
      const ADimensions: TFloatRectangle);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Single;
      out RayOrigin, RayDirection: TVector3); override;
  end;

  TOrthographicRaysWindow = class(TRaysWindow)
  private
    Dimensions: TFloatRectangle;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3;
      const ADimensions: TFloatRectangle);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Single;
      out RayOrigin, RayDirection: TVector3); override;
  end;

{ Calculate position and direction of the primary ray cast from CamPosition,
  going through the pixel X, Y.
  Takes into account camera 3D settings and screen sizes.
  RayDirection will always be normalized, just like from TRaysWindow.PrimaryRay.

  If you want to call this many times for the same camera settings,
  it may be more optimal to create TRaysWindow instance first
  and call it's TRaysWindow.PrimaryRay method. }
procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Single;
  const CamPosition, CamDirection, CamUp: TVector3;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);

implementation

uses Math, CastleUtils;

{ TRaysWindow ------------------------------------------------------------ }

constructor TRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3);
begin
  inherited Create;

  FCamPosition := ACamPosition;
  FCamDirection := ACamDirection;
  FCamUp := ACamUp;

  { fix CamUp }
  MakeVectorsOrthoOnTheirPlane(FCamUp, FCamDirection);

  { CamSide will be normalized, if CamDirection and CamUp are normalized too. }
  CamSide := TVector3.CrossProduct(CamDirection, CamUp);

  Assert(SameValue(CamDirection.LengthSqr, 1.0, 0.01));
  Assert(SameValue(CamUp.LengthSqr, 1.0, 0.01));
  Assert(SameValue(CamSide.LengthSqr, 1.0, 0.01));
end;

class function TRaysWindow.CreateDescendant(
  const ACamPosition, ACamDirection, ACamUp: TVector3;
  const Projection: TProjection): TRaysWindow;
begin
  case Projection.ProjectionType of
    ptPerspective:
      Result := TPerspectiveRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.PerspectiveAnglesRad);
    ptOrthographic:
      Result := TOrthographicRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.Dimensions);
    ptFrustum:
      Result := TFrustumRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.Dimensions);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TRaysWindow.CreateDescendant:ProjectionType?');
    {$endif}
  end;
end;

{ TPerspectiveRaysWindow ----------------------------------------------------- }

constructor TPerspectiveRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3;
  const APerspectiveAngles: TVector2);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);

  PerspectiveAngles := APerspectiveAngles;

  { calculate window parameters, ignoring camera settings.
    We assume distance to projection plane is 1 (this simplifies some calculations,
    and we can choose this distance arbitrarily --- it doesn't matter for user
    of this class).
    We know that WindowWidth / 2 = Tan(ViewAngleX / 2).
    From this, equations below follow. }
  WindowWidth  := Tan(PerspectiveAngles.X / 2) * 2;
  WindowHeight := Tan(PerspectiveAngles.Y / 2) * 2;
end;

procedure TPerspectiveRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Single;
  out RayOrigin, RayDirection: TVector3);
begin
  RayOrigin := CamPosition;

  { Direction of ray, ignoring current camera settings
    (assume camera position = zero, direction = -Z, up = +Y).
    Integer X, Y values should result in a ray that goes
    right through the middle of the pixel area. }
  RayDirection.X := MapRange(x+0.5, 0, ScreenWidth , -WindowWidth /2, WindowWidth /2);
  RayDirection.Y := MapRange(y+0.5, 0, ScreenHeight, -WindowHeight/2, WindowHeight/2);
  RayDirection.Z := -1;

  { Transform ray to take camera settings into acount. }
  RayDirection := TransformToCoords(RayDirection, CamSide, CamUp, -CamDirection);

  RayDirection := RayDirection.Normalize;
end;

{ TFrustumRaysWindow ---------------------------------------------------- }

constructor TFrustumRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3;
  const ADimensions: TFloatRectangle);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);
  { Workaround FPC bug on Darwin for AArch64 (not on other platforms):
    castlerays.pas(262,3) Fatal: Internal error 2014121702
    Occurs with 3.0.4 and with 3.3.1 (r44333 from 2020/03/22). }
  {$if defined(DARWIN) and defined(CPUAARCH64)}
  Dimensions := FloatRectangle(
    ADimensions.Left,
    ADimensions.Bottom,
    ADimensions.Width,
    ADimensions.Height
  );
  {$else}
  Dimensions := ADimensions;
  {$endif}
end;

procedure TFrustumRaysWindow.PrimaryRay(const X, Y: Single;
  const ScreenWidth, ScreenHeight: Single;
  out RayOrigin, RayDirection: TVector3);
begin
  { this is quite similar to TPerspectiveRaysWindow }

  RayOrigin := CamPosition;

  { Direction of ray, ignoring current camera settings
    (assume camera position = zero, direction = -Z, up = +Y).
    Integer X, Y values should result in a ray that goes
    right through the middle of the pixel area. }
  RayDirection.X := MapRange(X + 0.5, 0, ScreenWidth , Dimensions.Left, Dimensions.Right);
  RayDirection.Y := MapRange(Y + 0.5, 0, ScreenHeight, Dimensions.Bottom, Dimensions.Top);
  RayDirection.Z := -1;

  { Transform ray to take camera settings into acount. }
  RayDirection := TransformToCoords(RayDirection, CamSide, CamUp, -CamDirection);

  RayDirection := RayDirection.Normalize;
end;

{ TOrthographicRaysWindow ---------------------------------------------------- }

constructor TOrthographicRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3;
  const ADimensions: TFloatRectangle);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);
  { Workaround FPC bug on Darwin for AArch64 (not on other platforms):
    castlerays.pas(262,3) Fatal: Internal error 2014121702
    Occurs with 3.0.4 and with 3.3.1 (r44333 from 2020/03/22). }
  {$if defined(DARWIN) and defined(CPUAARCH64)}
  Dimensions := FloatRectangle(
    ADimensions.Left,
    ADimensions.Bottom,
    ADimensions.Width,
    ADimensions.Height
  );
  {$else}
  Dimensions := ADimensions;
  {$endif}
end;

procedure TOrthographicRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Single;
  out RayOrigin, RayDirection: TVector3);
begin
  RayOrigin := CamPosition;
  RayOrigin := RayOrigin + CamSide * MapRange(X + 0.5, 0, ScreenWidth,
    Dimensions.Left, Dimensions.Right);
  RayOrigin := RayOrigin + CamUp * MapRange(Y + 0.5, 0, ScreenHeight,
    Dimensions.Bottom, Dimensions.Top);

  { CamDirection must already be normalized, so RayDirection is normalized too }
  RayDirection := CamDirection;
end;

{ global functions ----------------------------------------------------------- }

procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Single;
  const CamPosition, CamDirection, CamUp: TVector3;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
var
  RaysWindow: TRaysWindow;
begin
  RaysWindow := TRaysWindow.CreateDescendant(CamPosition, CamDirection, CamUp,
    Projection);
  try
    RaysWindow.PrimaryRay(x, y, ScreenWidth, ScreenHeight, RayOrigin, RayDirection);
  finally RaysWindow.Free end;
end;

end.
