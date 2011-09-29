{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Calculating rays that correspond to the given points on 2D screen.
  This is used in situations such as by ray-tracing (that casts a ray
  for each image pixel) and when "picking" objects
  (e.g. user clicks with a mouse on window,
  and we want to know what object in 3D space he picked). }
unit RaysWindow;

interface

uses VectorMath;

{ Calculate second viewing angle for perspective projection.
  Given one viewing angle of the camera (FirstViewAngleDeg) and
  aspect ratio of your window sizes (SecondToFirstRatio),
  calculate second viewing angle of the camera.

  The intention is that when projecting camera view (with given view angles)
  on a screen with given aspect ratio), the image will not be distorted
  (squeezed horizontally or vertically).

  For the "Deg" version both angles (given and returned) are in degress,
  for the "Rad" version both angles and in radians.

  @groupBegin }
function AdjustViewAngleDegToAspectRatio(const FirstViewAngleDeg,
  SecondToFirstRatio: Single): Single;
function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad,
  SecondToFirstRatio: Single): Single;
{ @groupEnd }

type
  { Calculate primary rays for given camera settings
    and screen size.

    This has just two methods: constructor and PrimaryRay that calculates
    the position and direction of the primary ray. For typical ray-tracer usage,
    you call constructor only once, and then call PrimaryRay for each pixel.
    This way constructor can do some slightly more time-consuming work,
    and in PrimaryRay we can reuse some calculation results. }
  TRaysWindow = class
  private
    FCamPosition, FCamDirection, FCamUp: TVector3Single;
  public
    { Camera vectors. Initialized in the constructor.
      Note that CamUp may be changed in constructor, to be always perfectly
      orthogonal to CamDriection.

      @groupBegin }
    property CamPosition: TVector3Single read FCamPosition;
    property CamDirection: TVector3Single read FCamDirection;
    property CamUp: TVector3Single read FCamUp;
    { @groupEnd }

    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single);

    { Create appropriate non-abstract descendant
      (TPerspectiveRaysWindow or TOrthographicRaysWindow). }
    class function CreateDescendant(
      const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const APerspectiveView: boolean;
      const APerspectiveViewAngles: TVector2Single;
      const AOrthoViewDimensions: TVector4Single): TRaysWindow;

    { Calculate position and direction of the primary ray cast from CamPosition,
      going through the pixel X, Y.

      X, Y coordinates start from (0, 0) if bottom left (like in typical 2D
      OpenGL).  When they are integers and in the range of
      X = 0..ScreenWidth-1 (left..right), Y = 0..ScreenHeight-1 (bottom..top)
      it's guaranteed that resulting ray will go exactly through the middle
      of the appropriate pixel (on imaginary "rzutnia" = image positioned
      paraller to view direction). But you can provide non-integer X, Y,
      useful for multisampling (taking many samples within the pixel,
      like (X, Y) = (PixX + Random - 0.5, PixY + Random - 0.5)). }
    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out Ray0, RayDirection: TVector3Single); virtual; abstract;
  end;

  TPerspectiveRaysWindow = class(TRaysWindow)
  private
    WindowZ, WindowWidth, WindowHeight: Single;
    PerspectiveViewAngles: TVector2Single;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const APerspectiveViewAngles: TVector2Single);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out Ray0, RayDirection: TVector3Single); override;
  end;

  TOrthographicRaysWindow = class(TRaysWindow)
  private
    OrthoViewDimensions: TVector4Single;
    CamSide: TVector3Single;
  public
    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      const AOrthoViewDimensions: TVector4Single);

    procedure PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer;
      out Ray0, RayDirection: TVector3Single); override;
  end;

{ Calculate position and direction of the primary ray cast from CamPosition,
  going through the pixel X, Y.
  Takes into account camera 3D settings and screen sizes.

  See TCastleSceneManager.PerspectiveView for specification
  of projection properties PerspectiveView etc.

  This simply creates and uses TRaysWindow instance, which is not optimal
  if you will want to ask for PrimaryRay many times with the same camera
  and window settings. Better use TRaysWindow class directly then.
  For things like picking interactively objects with mouse this is usually
  fast enough (camera will change anyway on each move). }
procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  out Ray0, RayDirection: TVector3Single);

implementation

uses Math, CastleUtils;

{ AdjustViewAngle*ToAspectRatio ---------------------------------------- }

function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad, SecondToFirstRatio: Single): Single;
begin
 { naiwny blad jaki kiedys popelnilem chcac to policzyc (chociaz wtedy jeszcze
   nie bylo to opakowane w ta funkcje) to bylo zastosowanie wzorku
     result := FirstViewAngle * SecondToFirstRatio;
   ktory wychodzil z zupelnie blednego zalozenia ze stosunek miar katow x/y
   jest taki jak stosunek wymiarow rzutni x/y. Elementarny blad !

   Majac dane dwa katy , a i b, i wymiary rzutni x, y mamy
     tg(a/2) = (x/2)/d
     tg(b/2) = (y/2)/d
   gdzie d to odleglosc od rzutni (ktora sie zaraz skroci i nie bedzie potrzebna).
   Mamy z tego ze
     tg(a/2) / tg(b/2) = x/y
   a wiec stosunek TANGENSOW katow jest taki jak stosunek wymiarow rzutni
   (zreszta widac to w PrimaryRay_Core_Prepare gdzie obliczamy rzutnie z katow)
   (TANGENSOW, a nie samych KATOW !!).

   Odpowiednio przeksztalcajac powyzsza zaleznosc otrzymujemy poprawny
   wzor pozwalajacy nam wyliczyc b majac dane a i stosunek y/x -
   ten wzor jest zapisany ponizej w kodzie. }
 result := ArcTan( Tan(FirstViewAngleRad/2) * SecondToFirstRatio) * 2;
end;

function AdjustViewAngleDegToAspectRatio(const FirstViewAngleDeg, SecondToFirstRatio: Single): Single;
begin
 result := RadToDeg( AdjustViewAngleRadToAspectRatio( DegToRad(FirstViewAngleDeg),
   SecondToFirstRatio));
end;

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
end;

class function TRaysWindow.CreateDescendant(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const APerspectiveView: boolean;
  const APerspectiveViewAngles: TVector2Single;
  const AOrthoViewDimensions: TVector4Single): TRaysWindow;
begin
  if APerspectiveView then
    Result := TPerspectiveRaysWindow.Create(
      ACamPosition, ACamDirection, ACamUp, APerspectiveViewAngles) else
    Result := TOrthographicRaysWindow.Create(
      ACamPosition, ACamDirection, ACamUp, AOrthoViewDimensions);
end;

{ TPerspectiveRaysWindow ----------------------------------------------------- }

constructor TPerspectiveRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const APerspectiveViewAngles: TVector2Single);
const
  WindowDistance = 1;  { dowolna stala > 0, moze kiedys na cos sie przyda }
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);

  PerspectiveViewAngles := APerspectiveViewAngles;

  { oblicz rzutnie pomijajac Cam* i przyjmujac ze kamera jest
    w punkcie (0, 0, 0) skierowana w (0, 0, -1) i ma up w (0, 1, 0) }
  WindowZ := -WindowDistance;
  { wiemy ze szerokosc rzutni / 2 / Abs(WindowZ) = tg (ViewAngleX / 2).
    Wiec szerokosc rzutni = tg(ViewAngleX / 2) * Abs(WindowZ) * 2;
    podobnie wysokosc rzutni = tg(ViewAngleY / 2) * Abs(WindowZ) * 2; }
  WindowWidth  := Tan(DegToRad(PerspectiveViewAngles[0]) / 2) * Abs(WindowZ) * 2;
  WindowHeight := Tan(DegToRad(PerspectiveViewAngles[1]) / 2) * Abs(WindowZ) * 2;
end;

procedure TPerspectiveRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Integer;
  out Ray0, RayDirection: TVector3Single);
begin
 Ray0 := CamPosition;

 { wyznacz kierunek promienia pierwotnego.
   X z zakresu 0..ScreenWidth-1 ma dawac promienie dokladnie przez srodek
   pixela na rzutni, analogicznie Y. }
 RayDirection := Vector3Single(
   MapRange(x+0.5, 0, ScreenWidth , -WindowWidth /2, WindowWidth /2),
   MapRange(y+0.5, 0, ScreenHeight, -WindowHeight/2, WindowHeight/2),
   WindowZ);

 { uwzglednij teraz Cam* transformujac odpowiednio promien.
   Zwracam uwage ze nie uwzgledniamy tu przesuniecia CamPosition
   (wtedy traktowalibysmy RayVector jako punkt na polprostej promienia,
   a nie jako kierunek. Wiec musielibysmy wtedy od RayVector z powrotem
   odjac CamPosition, zupelnie bez sensu skoro mozemy je juz teraz pominac). }
 RayDirection := MatrixMultPoint(
   TransformToCoordsNoScaleMatrix(ZeroVector3Single,
     CamDirection >< CamUp,
     CamUp,
     -CamDirection), RayDirection);
end;

{ TOrthographicRaysWindow ---------------------------------------------------- }

constructor TOrthographicRaysWindow.Create(
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  const AOrthoViewDimensions: TVector4Single);
begin
  inherited Create(ACamPosition, ACamDirection, ACamUp);

  { we want to have CamUp always normalized }
  NormalizeTo1st(FCamUp);

  { we want to have CamSide, also always normalized }
  CamSide := CamDirection >< CamUp;
  NormalizeTo1st(CamSide);

  OrthoViewDimensions := AOrthoViewDimensions;
end;

procedure TOrthographicRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Integer;
  out Ray0, RayDirection: TVector3Single);
begin
  Ray0 := CamPosition;
  Ray0 += VectorScale(CamSide, MapRange(X + 0.5, 0, ScreenWidth,
    OrthoViewDimensions[0], OrthoViewDimensions[2]));
  Ray0 += VectorScale(CamUp, MapRange(Y + 0.5, 0, ScreenHeight,
    OrthoViewDimensions[1], OrthoViewDimensions[3]));
  RayDirection := CamDirection;
end;

{ global functions ----------------------------------------------------------- }

procedure PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  out Ray0, RayDirection: TVector3Single);
var
  RaysWindow: TRaysWindow;
begin
  RaysWindow := TRaysWindow.CreateDescendant(CamPosition, CamDirection, CamUp,
    PerspectiveView, PerspectiveViewAngles, OrthoViewDimensions);
  try
    RaysWindow.PrimaryRay(x, y, ScreenWidth, ScreenHeight, Ray0, RayDirection);
  finally RaysWindow.Free end;
end;

end.