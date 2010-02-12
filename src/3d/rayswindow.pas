{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Calculating rays that correspond to the given points on 2D screen.
  This is used in situations such as by ray-tracing (that casts a ray
  for each image pixel) and when "picking" objects
  (e.g. user clicks with a mouse on window,
  and we want to know what object in 3D space he picked). }
unit RaysWindow;

interface

uses VectorMath;

{ Given one viewing angle of the camera (FirstViewAngleDeg) and
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

{ Calculate primary rays for given camera settings
  and screen size.

  This has just two methods: constructor and PrimaryRay that calculates
  the direction of primary ray. For typical ray-tracer usage,
  you call constructor only once, and then call PrimaryRay for each pixel.
  So constructor does some slightly more time-consuming work
  (initializes Window* from angles, correct CamUp to be perfectly
  orthogonal to CamDrection). In PrimaryRay we just use them. }
type
  TRaysWindow = class
  private
    FWindowZ, FWindowWidth, FWindowHeight: Single;
    FCamPosition, FCamDirection, FCamUp: TVector3Single;
    FViewAngleDegX, FViewAngleDegY: Single;
  public
    property WindowZ: Single read FWindowZ;
    property WindowWidth: Single read FWindowWidth;
    property WindowHeight: Single read FWindowHeight;

    { Camera and view angles, initialized in constructor.

      Note that CamUp may be changed in constructor, to be always perfectly
      orthogonal to CamDriection.

      @groupBegin }
    property CamPosition: TVector3Single read FCamPosition;
    property CamDirection: TVector3Single read FCamDirection;
    property CamUp: TVector3Single read FCamUp;

    property ViewAngleDegX: Single read FViewAngleDegX;
    property ViewAngleDegY: Single read FViewAngleDegY;
    { @groupEnd }

    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3Single;
      AViewAngleDegX, AViewAngleDegY: Single);

    { Calculate direction of the primary ray cast from CamPosition,
      going through the pixel X, Y.

      X, Y coordinates start from (0, 0) if bottom left (like in typical 2D
      OpenGL).  When they are integers and in the range of
      X = 0..ScreenWidth-1 (left..right), Y = 0..ScreenHeight-1 (bottom..top)
      it's guaranteed that resulting ray will go exactly through the middle
      of the appropriate pixel (on imaginary "rzutnia" = image positioned
      paraller to view direction). But you can provide non-integer X, Y,
      useful for multisampling (taking many samples within the pixel,
      like (X, Y) = (PixX + Random - 0.5, PixY + Random - 0.5)). }
    function PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer): TVector3Single;
  end;

{ Calculate direction of the primary ray cast from CamPosition,
  going through the pixel X, Y.
  Takes into account camera 3D settings and screen sizes.

  This simply creates and uses TRaysWindow instance, which is not optimal
  if you will want to ask for PrimaryRay many times with the same camera
  and window settings. Better use TRaysWindow class directly then.
  For things like picking interactively objects with mouse this is usually
  fast enough (camera will change anyway on each move). }
function PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single): TVector3Single;

implementation

uses Math, KambiUtils;

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
  const ACamPosition, ACamDirection, ACamUp: TVector3Single;
  AViewAngleDegX, AViewAngleDegY: Single);
const
  WindowDistance = 1;  { dowolna stala > 0, moze kiedys na cos sie przyda }
begin
 inherited Create;

 FCamPosition := ACamPosition;
 FCamDirection := ACamDirection;
 FCamUp := ACamUp;
 FViewAngleDegX := AViewAngleDegX;
 FViewAngleDegY := AViewAngleDegY;

 { popraw CamUp }
 MakeVectorsOrthoOnTheirPlane(FCamUp, FCamDirection);

 { oblicz rzutnie pomijajac Cam* i przyjmujac ze kamera jest
   w punkcie (0, 0, 0) skierowana w (0, 0, -1) i ma up w (0, 1, 0) }
 FWindowZ := -WindowDistance;
 { wiemy ze szerokosc rzutni / 2 / Abs(WindowZ) = tg (ViewAngleX / 2).
   Wiec szerokosc rzutni = tg(ViewAngleX / 2) * Abs(WindowZ) * 2;
   podobnie wysokosc rzutni = tg(ViewAngleY / 2) * Abs(WindowZ) * 2; }
 FWindowWidth := Tan(DegToRad(ViewAngleDegX) / 2) * Abs(WindowZ) * 2;
 FWindowHeight := Tan(DegToRad(ViewAngleDegY) / 2) * Abs(WindowZ) * 2;
end;

function TRaysWindow.PrimaryRay(const x, y: Single;
  const ScreenWidth, ScreenHeight: Integer): TVector3Single;
begin
 { wyznacz kierunek promienia pierwotnego.
   X z zakresu 0..ScreenWidth-1 ma dawac promienie dokladnie przez srodek
   pixela na rzutni, analogicznie Y. }
 result := Vector3Single(
   MapRange(x+0.5, 0, ScreenWidth , -WindowWidth /2, WindowWidth /2),
   MapRange(y+0.5, 0, ScreenHeight, -WindowHeight/2, WindowHeight/2),
   WindowZ);

 { uwzglednij teraz Cam* transformujac odpowiednio promien.
   Zwracam uwage ze nie uwzgledniamy tu przesuniecia CamPosition
   (wtedy traktowalibysmy RayVector jako punkt na polprostej promienia,
   a nie jako kierunek. Wiec musielibysmy wtedy od RayVector z powrotem
   odjac CamPosition, zupelnie bez sensu skoro mozemy je juz teraz pominac). }
 Result := MatrixMultPoint(
   TransformToCoordsNoScaleMatrix(ZeroVector3Single,
     CamDirection >< CamUp,
     CamUp,
     -CamDirection), result);
end;

function PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single): TVector3Single;
var RaysWindow: TRaysWindow;
begin
 RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp,
   ViewAngleDegX, ViewAngleDegY);
 try
  result := RaysWindow.PrimaryRay(x, y, ScreenWidth, ScreenHeight);
 finally RaysWindow.Free end;
end;

end.