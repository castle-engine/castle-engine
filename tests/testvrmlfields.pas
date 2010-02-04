{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit TestVRMLFields;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLFields = class(TTestCase)
    procedure TestVRMLFields;
  end;

implementation

uses KambiUtils, VectorMath, VRMLFields, VRMLCameraUtils, Math;

procedure TTestVRMLFields.TestVRMLFields;

  (* ------------------------
     HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
     -----------------------

     As a hack, I just pasted here implementation of CamDirUp2Orient.
     I don't want to make CamDirUp2Orient public in VRMLCameraUtils. *)
  function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
    forward; overload;
  procedure CamDirUp2Orient(CamDir, CamUp: TVector3Single;
    out OrientAxis: TVector3Single; out OrientRadAngle: Single);
    forward; overload;

  procedure CamDirUp2Orient(CamDir, CamUp: TVector3Single;
    out OrientAxis: TVector3Single; out OrientRadAngle: Single);
  { Poczatkowo byl tutaj kod based on Stephen Chenney's ANSI C code orient.c.
    Byl w nim bledzik (patrz testUnits.Test_VRMLFields - TestOrints[4])
    i nawet teraz nie wiem jaki bo ostatecznie zrozumialem sama idee tamtego kodu
    i zapisalem tutaj rzeczy po swojemu, i ku mojej radosci nie mam tego bledu.

    Tutejsze funkcje lokalne operujace na kwaternionach zamierzam
    odseparowac kiedys, jak tylko bede chcial gdzies jeszcze uzyc kwaternionow.

    Niniejszym ustalam sobie ze jesli gdzies potraktuje kwaternion jako wektor
    4 x skalar to bede mial na mysli ze pierwsze trzy skladowe okreslaja wektor
    a ostatnia skladowa - kat, albo (ogolniej) ze pierwsze trzy skladowe
    to wspolczynniki przy i, j, k a ostatnia skladowa to czesc rzeczywista.
    Widzialem rozne konwencje tego, ale bede sie trzymal powyzszego bo
    - tak jest podawane SFRotation VRMLa (ktore nie jest kwaternionem ale
      jest podobne)
    - tak bylo podawane na PGK gdzie pierwszy raz zobaczylem kwaternion

    Pomysl na ta funkcje: mamy CamDir i CamUp. Zeby je zamienic na
    orientation VRMLa, czyli axis i angle obrotu standardowego dir
    (0, 0, -1) i standardowego up (0, 1, 0), wyobrazamy sobie jaka transformacje
    musielibysmy zrobic standardowym dir/up zeby zamienic je na nasze CamDir/Up.
    1) najpierw bierzemy wektor prostop. do standardowego dir i CamDir.
       Obracamy sie wokol niego zeby standardowe dir nalozylo sie na CamDir.
    2) Teraz obracamy sie wokol CamDir tak zeby standardowe up (ktore juz
       zostalo obrocone przez transformacje pierwsza) nalozylo sie na CamUp.
       Mozemy to zrobic bo CamDir i CamUp sa prostopadle i maja dlugosc 1,
       podobnie jak standardowe dir i up.
    Zlozenie tych dwoch transformacji to jest nasza szukana transformacja.

    Jedyny problem jaki pozostaje to czym jest transformacja ? Jezeli mowimy
    o macierzy to jest prosto, macierze dwoch obrotow umiemy skonstruowac
    i wymnozyc ale na koncu dostajemy macierz. A chcemy miec axis+angle.
    A wiec quaternion.
    Moznaby to zrobic inaczej (np. wyciagnac z matrix quaternion lub
    wyciagajac z matrix katy eulera i konwertujac je na quaternion)
    ale najwygodniej jest skorzystac tutaj z mozliwosci mnozenia kwaternionow:
    przemnoz quaterniony obrotu q*p a orztymasz quaternion ktory za pomoca
    jednego obrotu wyraza zlozenie dwoch obrotow, p i q (najpierw p, potem q).
    To jest wlasnie idea z kodu Stephen Chenney's "orient.c".
  }

    type
      TQuaternion = record vect_part: TVector3Single; real_part: Single end;

    function AxisAngleCos_To_Quaternion(const Axis: TVector3Single;
      const angle_cos: Single): TQuaternion;
    { zamien Axis i angle_cos na kwaternion odpowiedniego obrotu.
      Jezeli Axis jest znormalizowane to kwaternion tez wyjdzie znormalizowany. }
    var sin_half_angle, cos_half_angle, AngleRad: Float;
    begin
     {* The quaternion requires half angles. *}
     AngleRad := ArcCos(Clamped(angle_cos, -1.0, 1.0));
     SinCos(AngleRad/2, sin_half_angle, cos_half_angle);

     result.vect_part := VectorScale(axis, sin_half_angle);
     result.real_part := cos_half_angle;
    end;

    function QQMul(const q1, q2: TQuaternion): TQuaternion;
    { mnozenie dwoch kwaternionow (dowolnych) }
    begin
     result.real_part := q1.real_part * q2.real_part - VectorDotProduct(q1.vect_part, q2.vect_part);
     result.vect_part := VectorProduct(q1.vect_part, q2.vect_part);
     VectorAddTo1st(result.vect_part, VectorScale(q1.vect_part, q2.real_part));
     VectorAddTo1st(result.vect_part, VectorScale(q2.vect_part, q1.real_part));
    end;

    procedure Quaternion_To_AxisAngle(const q: TQuaternion;
      out axis: TVector3Single;
      out angle: Single);
    { q musi byc znormalizowanym kwaternionem obrotu,
      tzn. <sin(alfa/2)*normalized(wektor), cos(alfa/2)> }
    var half_angle, sin_half_angle: Single;
    begin
     half_angle := ArcCos(q.real_part);
     sin_half_angle := Sin(half_angle);
     angle := half_angle * 2;
     if Zero(sin_half_angle) then
     begin
      { Jezeli Zero(sin_half_angle) to znaczy ze q.vect_part = ZeroVector.
        Wiec skoro q jest znormalizowany to Sqr(q.real_part) = 1 a wiec
        q.real_part = -1 lub 1. A wiec Cos(Angle/2) = -1 lub 1 a wiec
        Angle/2 = Pi * k dla k calkowitych. A wiec Angle = 2k * Pi a wiec
        Angle jest rownowazne zero. (Angle = 2Pi czy -2Pi to przeciez to samo
        co Angle = 0).

        Jesli Angle = 2k * Pi to wszystko ok bo to znaczy ze Angle jest
        rownowazny 0 a wiec Axis ktore chcemy wyliczyc jest bez znaczenia.
        Ustawiamy wtedy Axis na cokolwiek (ale NIE na wektor zerowy
        (jak to bylo w oryginalnym kodzie orient.c), ustawianie wektora zerowego
        jest bez sensu, nigdy nie mozna jako Axis dawac wektora zerowego.).

        Wpp. (jezeli Zero(sin_half_angle) ale nie Zero(Angle)) to wykrylismy
        blad, to znaczy ze quaternion wcale nie byl ladnym znorm. quaternionem
        obrotu. }
      if Zero(Angle) then
       Axis := Vector3Single(0, 0, 1) else
       raise EVectorMathInvalidOp.Create('Invalid quaternion in Quaternion_To_AxisAngle');
     end else
      Axis := VectorScale(q.vect_part, 1/sin_half_angle);
    end;

    function Quaternion_Rotate(q: TQuaternion; const Point: TVector3Single): TVector3Single;
    { q to znormalizowany kwaternion obrotu. Wynik: punkt Point odwrocony o
      kwaternion, zgodnie ze wzorkiem q * P * q^(-1) gdzie P = <Point, 0> }
    var PointQuat, ResultQuat: TQuaternion;
    begin
     PointQuat.real_part := 0.0;
     PointQuat.vect_part := Point;

     ResultQuat := QQMul(q, PointQuat);
     { q := q^(-1). Poniewaz wiemy ze q jest znormalizowany to mozemy obliczyc
       q^(-1) prosto jako wartosc sprzezona do q. }
     VectorNegateTo1st(q.vect_part);
     ResultQuat := QQMul(ResultQuat, q);

     result := ResultQuat.vect_part;
    end;

  var Rot1Axis, Rot2Axis, StdCamUpAfterRot1: TVector3Single;
      Rot1Quat, Rot2Quat, OrientQuat: TQuaternion;
      Rot1CosAngle, Rot2CosAngle: Single;
  begin
   NormalizeTo1st(CamDir);
   NormalizeTo1st(CamUp);

   { calculate Rot1Quat }
   Rot1Axis := Normalized( VectorProduct(DefaultVRMLCameraDirection, CamDir) );
   Rot1CosAngle := VectorDotProduct(DefaultVRMLCameraDirection, CamDir);
   Rot1Quat := AxisAngleCos_To_Quaternion(Rot1Axis, Rot1CosAngle);

   { calculate Rot2Quat }
   StdCamUpAfterRot1 := Quaternion_Rotate(Rot1Quat, DefaultVRMLCameraUp);
   { wiemy ze Rot2Axis to CamDir lub -CamDir. Wyznaczamy je jednak w tak
     prosty sposob bo nie przychodzi mi teraz do glowy inny sposob jak rozpoznac
     czy powinnismy tu wziac CamDir czy -CamDir (chodzi o to zeby pozniej obrot
     o Rot2CosAngle byl w dobra strone) }
   Rot2Axis := Normalized( VectorProduct(StdCamUpAfterRot1, CamUp) );
   Rot2CosAngle := VectorDotProduct(StdCamUpAfterRot1, CamUp);
   Rot2Quat := AxisAngleCos_To_Quaternion(Rot2Axis, Rot2CosAngle);

   { calculate OrientQuat = zlozenie Rot1 i Rot2 (tak, kolejnosc mnozenia QQMul musi
     byc odwrotna) }
   OrientQuat := QQMul(Rot2Quat, Rot1Quat);

   { Extract the axis and angle from the quaternion. }
   Quaternion_To_AxisAngle(OrientQuat, OrientAxis, OrientRadAngle);
  end;

  function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
  var OrientAxis: TVector3Single;
      OrientAngle: Single;
  begin
   CamDirUp2Orient(CamDir, CamUp, OrientAxis, OrientAngle);
   result := Vector4Single(OrientAxis, OrientAngle);
  end;

  (* ------------------------
     End of the HACK End of the HACK End of the HACK End of the HACK
     ----------------------- *)

  procedure CamOrientToDirUp(const Orient: TVector4Single; var Dir, Up: TVector3Single);
  var OrientAxis: TVector3Single absolute Orient;
  begin
   Dir := RotatePointAroundAxisRad(Orient[3], Vector3Single(0, 0, -1), OrientAxis);
   Up := RotatePointAroundAxisRad(Orient[3], Vector3Single(0, 1, 0 ), OrientAxis);
  end;

const
  TestOrients: array[0..4]of TVector4Single =
  (
    { orientacje z lbview }
    (-1, 0, 0, 1.5708),
    (-0.590284, 0.769274, 0.244504, 0.987861),
    (0, 0, 1, 0),
    (0, 1, 0, 1.5708),

    (0.9133538007736206, -0.23269428312778472, -0.33412325382232666, 1.807408332824707)
  );

  EqEpsilon = 0.0001;

var Dir, Up: TVector3Single;
    i: Integer;
    NewOrient: TVector4Single;
    oohFailed: boolean;
begin
 { Uzywam zmiennej oohFailed bo chce wykonac zawsze test dla wszystkich
   wymienionych orientacji, nawet jesli niektore po drodze zawioda.
   I tak bede chcial zobaczyc ktorym sie udalo.
   Dopiero po wykonaniu wszystkich testow rzucam wyjatek jezeli jakis
   test zakonczyl sie porazka. }
 oohFailed := false;

 for i := 0 to High(TestOrients) do
 begin
  CamOrientToDirUp(TestOrients[i], Dir, Up);
  NewOrient := CamDirUp2Orient(Dir, Up);
  if not ( (Zero(NewOrient[3]) and Zero(TestOrients[i, 3])) or
	   VectorsEqual(NewOrient, TestOrients[i], EqEpsilon) ) then
  begin
   Writeln(Format(
     'failed z TestOrients[%d] = %s' +nl+
     'Dir = %s' +nl+
     'Up = %s' +nl+
     'NewOrient = %s',
     [ i,
       VectorToNiceStr(TestOrients[i]),
       VectorToNiceStr(Dir),
       VectorToNiceStr(Up),
       VectorToNiceStr(NewOrient) ]));
   oohFailed := true;
  end;
 end;

 Assert(not oohFailed);
end;

initialization
 RegisterTest(TTestVRMLFields);
end.
