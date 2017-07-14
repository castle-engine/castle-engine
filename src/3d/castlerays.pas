{
  Copyright 2003-2017 Michalis Kamburelis.

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
unit CastleRays;

{$I castleconf.inc}

interface

uses CastleVectors, CastleProjection, CastleRectangles;

type
  { Calculate primary rays for given camera settings and screen size. }
  TRaysWindow = class
  private
    FCamPosition, FCamDirection, FCamUp: TVector3Single;
    CamSide: TVector3Single;
  public
    { Camera vectors. Initialized in the constructor.
      Must be given already normalized.
      Note that CamUp may be changed in constructor, to be always perfectly
      orthogonal to CamDirection.

      @groupBegin }
    property CamPosition: TVector3Single read FCamPosition;
    property CamDirection: TVector3Single read FCamDirection;
    property CamUp: TVector3Single read FCamUp;
    { @groupEnd }

    { Constructor. Calculates some values based on camera settings,
      this way many calls to PrimaryRay for the same camera settings
      are fast (useful for ray-tracers). }
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single);

    { Create appropriate TRaysWindow instance.
      Constructs non-abstract descendant (TPerspectiveRaysWindow or
      TOrthographicRaysWindow, depending on Projection.ProjectionType). }
    class function CreateDescendant(
      const ACamPosition, ACamDirection, ACamUp: TVector3Single;
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
      const ScreenWidth, ScreenHeight: Integer;
      out RayOrigin, RayDirection: TVector3Single); virtual; abstract;
  end;

  TPerspectiveRaysWindow = class(TRaysWindow)
  private
    WindowWidth, WindowHeight: Single;
    PerspectiveAngles: TVector2Single;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const APerspectiveAngles: TVector2Single);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out RayOrigin, RayDirection: TVector3Single); override;
  end;

  TFrustumRaysWindow = class(TRaysWindow)
  private
    Dimensions: TFloatRectangle;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const ADimensions: TFloatRectangle);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out RayOrigin, RayDirection: TVector3Single); override;
  end;

  TOrthographicRaysWindow = class(TRaysWindow)
  private
    Dimensions: TFloatRectangle;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const ADimensions: TFloatRectangle);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out RayOrigin, RayDirection: TVector3Single); override;
  end;

{ Calculate position and direction of the primary ray cast from CamPosition,
  going through the pixel X, Y.
  Takes into account camera 3D settings and screen sizes.
  RayDirection will always be normalized, just like from TRaysWindow.PrimaryRay.

  If you want to call this many times for the same camera settings,
  it may be more optimal to create TRaysWindow instance first
  and call it's TRaysWindow.PrimaryRay method. }
procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3Single);

implementation

uses Math, CastleUtils;

{ TRaysWindow ------------------------------------------------------------ }

constructor TRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single);
begin
  inherited Create;

  FCamPosition := ACamPosition;
  FCamDirection := ACamDirection;
  FCamUp := ACamUp;

  { fix CamUp }
  MakeVectorsOrthoOnTheirPlane(FCamUp, FCamDirection);

  { CamSide will be normalized, if CamDirection and CamUp are normalized too. }
  CamSide := CamDirection >< CamUp;

  Assert(FloatsEqual(VectorLenSqr(CamDirection), 1.0, 0.01));
  Assert(FloatsEqual(VectorLenSqr(CamUp), 1.0, 0.01));
  Assert(FloatsEqual(VectorLenSqr(CamSide), 1.0, 0.01));
end;

class function TRaysWindow.CreateDescendant(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const Projection: TProjection): TRaysWindow;
begin
  case Projection.ProjectionType of
    ptPerspective:
      Result := TPerspectiveRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.PerspectiveAngles);
    ptOrthographic:
      Result := TOrthographicRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.Dimensions);
    ptFrustum:
      Result := TFrustumRaysWindow.Create(
        ACamPosition, ACamDirection, ACamUp, Projection.Dimensions);
    else raise EInternalError.Create('TRaysWindow.CreateDescendant:ProjectionType?');
  end;
end;

{ TPerspectiveRaysWindow ----------------------------------------------------- }

constructor TPerspectiveRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const APerspectiveAngles: TVector2Single);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);

  PerspectiveAngles := APerspectiveAngles;

  { calculate window parameters, ignoring camera settings.
    We assume distance to projection plane is 1 (this simplifies some calculations,
    and we can choose this distance arbitrarily --- it doesn't matter for user
    of this class).
    We know that WindowWidth / 2 = Tan(ViewAngleX / 2).
    From this, equations below follow. }
  WindowWidth  := Tan(DegToRad(PerspectiveAngles[0]) / 2) * 2;
  WindowHeight := Tan(DegToRad(PerspectiveAngles[1]) / 2) * 2;
end;

procedure TPerspectiveRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Integer;
  out RayOrigin, RayDirection: TVector3Single);
begin
  RayOrigin := CamPosition;

  { Direction of ray, ignoring current camera settings
    (assume camera position = zero, direction = -Z, up = +Y).
    Integer X, Y values should result in a ray that goes
    right through the middle of the pixel area. }
  RayDirection[0] := MapRange(x+0.5, 0, ScreenWidth , -WindowWidth /2, WindowWidth /2);
  RayDirection[1] := MapRange(y+0.5, 0, ScreenHeight, -WindowHeight/2, WindowHeight/2);
  RayDirection[2] := -1;

  { Transform ray to take camera settings into acount. }
  RayDirection := TransformToCoords(RayDirection, CamSide, CamUp, -CamDirection);

  NormalizeVar(RayDirection);
end;

{ TFrustumRaysWindow ---------------------------------------------------- }

constructor TFrustumRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const ADimensions: TFloatRectangle);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);
  Dimensions := ADimensions;
end;

procedure TFrustumRaysWindow.PrimaryRay(const X, Y: Single;
  const ScreenWidth, ScreenHeight: Integer;
  out RayOrigin, RayDirection: TVector3Single);
begin
  { this is quite similar to TPerspectiveRaysWindow }

  RayOrigin := CamPosition;

  { Direction of ray, ignoring current camera settings
    (assume camera position = zero, direction = -Z, up = +Y).
    Integer X, Y values should result in a ray that goes
    right through the middle of the pixel area. }
  RayDirection[0] := MapRange(X + 0.5, 0, ScreenWidth , Dimensions.Left, Dimensions.Right);
  RayDirection[1] := MapRange(Y + 0.5, 0, ScreenHeight, Dimensions.Bottom, Dimensions.Top);
  RayDirection[2] := -1;

  { Transform ray to take camera settings into acount. }
  RayDirection := TransformToCoords(RayDirection, CamSide, CamUp, -CamDirection);

  NormalizeVar(RayDirection);
end;

{ TOrthographicRaysWindow ---------------------------------------------------- }

constructor TOrthographicRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const ADimensions: TFloatRectangle);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);
  Dimensions := ADimensions;
end;

procedure TOrthographicRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Integer;
  out RayOrigin, RayDirection: TVector3Single);
begin
  RayOrigin := CamPosition;
  RayOrigin += CamSide * MapRange(X + 0.5, 0, ScreenWidth,
    Dimensions.Left, Dimensions.Right);
  RayOrigin += CamUp * MapRange(Y + 0.5, 0, ScreenHeight,
    Dimensions.Bottom, Dimensions.Top);

  { CamDirection must already be normalized, so RayDirection is normalized too }
  RayDirection := CamDirection;
end;

{ global functions ----------------------------------------------------------- }

procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3Single);
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
