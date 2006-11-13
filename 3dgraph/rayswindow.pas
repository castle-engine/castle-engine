{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Calculating rays that correspond to the given points on 2D screen.
  This is used in situations such as by ray-tracing (that casts a ray
  for each image pixel) and when "picking" objects
  (e.g. user clicks with a mouse on window,
  and we want to know what object in 3D space he picked). }
unit RaysWindow;

interface

uses VectorMath, Matrix;

{ Given one viewing angle of camera (FirstViewAngleDeg) and
  aspect ratio of your window sizes (SecondToFirstRatio),
  calculate second viewing angle of the camera.

  The intention is that when projecting camera view (with given view angles)
  on a screen with given aspect ratio, the image will not be deformed
  (squeezed horizontally or vertically).

  For the "Deg" version both angles (given and returned) are in degress,
  for the "Rad" version both angles and in radians. }
function AdjustViewAngleDegToAspectRatio(const FirstViewAngleDeg,
  SecondToFirstRatio: Single): Single;
function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad,
  SecondToFirstRatio: Single): Single;

{ This is used to calculate primary rays for given camera settings
  and screen size.

  Mozesz zauwazyc ze caly ten obiekt to tylko dwie metody : konstruktor
  i funkcja generujaca promien pierowtny. To rozdzielenie jest bardzo
  wazne - w konstruktorze inicjujemy atrybuty Window* i poprawiamy
  CamUp (zeby na pewno byl prostop. do CamDrection) i obie te rzeczy
  w sumie marnuja troszeczke czasu. W PrimaryRay() juz wykorzystujemy
  te obliczone wlasciwosci. W rezultacie raytracery moga zyskac troche
  czasu inicjujac rzutnie tylko raz, na poczatku, a potem dla kazdego
  pixela korzystac z tej samej rzutni i pytac sie jej tylko o kolejne
  PrimaryRay. }
type
  TRaysWindow = class
  private
    FWindowZ, FWindowWidth, FWindowHeight: Single;
    FCamPosition, FCamDirection, FCamUp: TVector3_Single;
    FViewAngleDegX, FViewAngleDegY: Single;
  public
    property WindowZ: Single read FWindowZ;
    property WindowWidth: Single read FWindowWidth;
    property WindowHeight: Single read FWindowHeight;

    { CamPosition/Direction i ViewAngle* beda identyczne z podanymi
      w konstruktorze, natomiast CamUp bedzie poprawione tak zeby na pewno
      bylo prostopadle do CamDirection (mozesz z tego skorzystac i po
      utworzeniu rzutni wziac juz poprawiony CamUp). }
    property CamPosition: TVector3_Single read FCamPosition;
    property CamDirection: TVector3_Single read FCamDirection;
    property CamUp: TVector3_Single read FCamUp;

    property ViewAngleDegX: Single read FViewAngleDegX;
    property ViewAngleDegY: Single read FViewAngleDegY;

    constructor Create(const ACamPosition, ACamDirection, ACamUp: TVector3_Single;
      AViewAngleDegX, AViewAngleDegY: Single);

    { x, y sa liczone od lewej do dolu. Jezeli X, Y sa calkowite i w zakresach
      X = 0..ScreenWidth-1 (lewa..prawa), Y = 0..ScreenHeight-1 (dol..gora)
      to wynikiem bedzie promien przechodzacy dokladnie przez srodek odpowiedniego
      pixela na rzutni, ale mozna podawac dowolne X, Y. Np. jezeli chcemy wziac
      kilka losowych probek z pixela PixX, PixY to mozemy kilkakrotnie
      poprosic o promien dla (X, Y) = (PixX + Random - 0.5, PixY + Random - 0.5).

      Zwraca RayVector promienia, Ray0 masz przeciez w CamPosition. }
    function PrimaryRay(const x, y: Single;
      const ScreenWidth, ScreenHeight: Integer): TVector3_Single;
  end;

{ Oblicz promien pierwotny na podstawie ulozenia kamery i ViewAngleXY.
  Ta funkcja tworzy rzutnie, pyta ja o jeden PrimaryRay i zwraca go.
  Jak juz napisalem przy TRaysWindow to jest bardzo nieefektywne zachowanie
  wiec czesto bedziesz chcial uzyc typu TRaysWindow a nie tej funkcji ale
  czasem rzeczywiscie nie zalezy ci na czasie - np. do wykonania mechanizmu
  picking (jaki obiekt jest wyswietlany na pixlu ekranu x, y ?) -
  takie picking bedzie poprawne o ile oryginalny rysunek ktory widzi user
  byl wykonany z dokladnie takimi samymi ustawieniami perspektywy -
  no i do picking wystarczy nam pozniej wysledzenie tego jednego promienia
  wiec rzeczywiscie nie ma sensu pieprzyc sie z klasa TRaysWindow. }
function PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3_Single;
  const ViewAngleDegX, ViewAngleDegY: Single): TVector3_Single;

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
  const ACamPosition, ACamDirection, ACamUp: TVector3_Single;
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
 MakeVectorsOrthoOnTheirPlane(FCamUp.Data, FCamDirection);

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
  const ScreenWidth, ScreenHeight: Integer): TVector3_Single;
begin
 { wyznacz kierunek promienia pierwotnego.
   X z zakresu 0..ScreenWidth-1 ma dawac promienie dokladnie przez srodek
   pixela na rzutni, analogicznie Y. }
 result.Init(
   MapRange(x+0.5, 0, ScreenWidth , -WindowWidth /2, WindowWidth /2),
   MapRange(y+0.5, 0, ScreenHeight, -WindowHeight/2, WindowHeight/2),
   WindowZ);

 { uwzglednij teraz Cam* transformujac odpowiednio promien.
   Zwracam uwage ze nie uwzgledniamy tu przesuniecia CamPosition
   (wtedy traktowalibysmy RayVector jako punkt na polprostej promienia,
   a nie jako kierunek. Wiec musielibysmy wtedy od RayVector z powrotem
   odjac CamPosition, zupelnie bez sensu skoro mozemy je juz teraz pominac). }
 Result := MultMatrixPoint(
   TransformToCoordsNoScaleMatrix(ZeroVector3Single,
     CamDirection >< CamUp,
     CamUp,
     -CamDirection), result);
end;

function PrimaryRay(const x, y: Single; const ScreenWidth, ScreenHeight: Integer;
  const CamPosition, CamDirection, CamUp: TVector3_Single;
  const ViewAngleDegX, ViewAngleDegY: Single): TVector3_Single;
var RaysWindow: TRaysWindow;
begin
 RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp,
   ViewAngleDegX, ViewAngleDegY);
 try
  result := RaysWindow.PrimaryRay(x, y, ScreenWidth, ScreenHeight);
 finally RaysWindow.Free end;
end;

end.