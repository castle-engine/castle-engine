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

{ @abstract(Samplowanie sfery i polsfery.)

  Wiekszosc rzeczy napisana bezposrednio na podstawie GlobalIllumComp IV.B,
  nie bede tego juz tutaj zaznaczal przy kazdej funkcji.

  Funkcje bez XYZ zwracaja wektor 2 floatow = dwa katy Phi, Theta
  (w tej kolejnosci), gdzie Phi = [0; 2*Pi] (traktowane jako odchylenie
  od jakiegos ustalonego poludnika) a Theta = [0, Pi/2] dla polsfery i
  [0, Pi] dla sfery (traktowane jako odchylenie od wybranej zasadniczej osi).
  (de facto traktowanie wartosci Phi, Theta jako jakichs katow wyznaczajacych
  cos na sferze jest czysto umowne, to po prostu dwie liczby z
  jakiegos zakresu a my samplujemy je z jakas gestoscia.)

  Funkcje XYZ zwracaja punkt x, y, z gdzie 0, 0, 0 to srodek (pol)sfery
  (0, 0, 1) to kierunek osi polsfery (tzn. dla niego Theta = 0),
  (1, 0, 0) to kierunek dla ktorego Phi = 0 i Theta = Pi/2,
  (0, 1, 0) to kierunek dla ktorego Phi = Pi/2 i Theta = Pi/2.
  (zgodnie z konwencja pokazana w GlobalIllumComp punkt (21)).

  Funkcje z Density <> Const zwracaja wartosc PdfValue dla wylosowanego punktu,
  tzn. dla probkowania z p(Theta) te funkcje zwracaja PfdValue = p(result[1]).
  (ale te funkcje robia to bez zadnej straty czasu, bo i tak licza
  PdfValue jako czesc obliczania result (natomiast gdybys chcial na podstawie
  otrzymanego result obliczyc wartosc p(result) to musialbys czesto stracic
  czas np. na liczenie Cosinusa).
  PdfValue przyda sie do importance sampling.
}

unit SphereSampling;

interface

uses VectorMath, KambiUtils;

{ zamien PhiTheta na XYZ zgodnie z podanymi konwencjami w komentarzu na
  poczatku modulu (tzn. (0, 0, 1) to punkt gdzie Theta = 0 itd. }
function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereRadius: Single)
  :TVector3Single; overload;

{ zamien PhiTheta na XYZ przyjmujac za os [pol]sfery (tam gdzie Theta = 0)
  wektor SphereTheta0. Gdzie bedzie punkt (Phi = 0, Theta =Pi/2) lub jakikolwiek
  punkt o Theta <> 0 nie jest ustalone, tzn. nie jest zdefiniowane jak beda
  rozlozone wspolrzedne Phi. (zazwyczaj nie jest to zbyt istotne bo probkowanie
  jest rozlozone rownomiernie wzdluz wartosci Phi).

  Zwracam uwage ze parametr SphereRadius juz nie jest tu potrzebny, dlugosc
  SphereTheta0 wyznacza SphereRadius. }
function PhiThetaToXYZ(const PhiTheta: TVector2Single;
  const SphereTheta0: TVector3Single): TVector3Single; overload;

{ DensityConst czyli p(Theta) = 1/2*Pi }
function RandomUnitHemispherePointDensityConst: TVector2Single;
function RandomUnitHemispherePointDensityConstXYZ: TVector3Single;

{ DensityCosTheta czyli p(Theta) = cos(Theta)/Pi }
function RandomUnitHemispherePointDensityCosTheta(
  out PdfValue: Single): TVector2Single;
function RandomUnitHemispherePointDensityCosThetaXYZ(
  out PdfValue: Single): TVector3Single;

{ DensityCosThetaExp czyli p(Theta) = (n+1) * (cos(Theta))^n / 2*Pi }
function RandomUnitHemispherePointDensityCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2Single;
function RandomUnitHemispherePointDensityCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3Single;

implementation

{ Notki o implementacji: pamietajmy ze typem podstawowywm dla modulu Math
  jest Float. Starajmy sie zminimalizowac konwersje Single <-> Float. }

uses Math;

function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereRadius: Single): TVector3Single;
var SinPhi, CosPhi, SinTheta, CosTheta: Float;
begin
 SinCos(PhiTheta[0], SinPhi, CosPhi);
 SinCos(PhiTheta[1], SinTheta, CosTheta);

 result[0] := SphereRadius * CosPhi * SinTheta;
 result[1] := SphereRadius * SinPhi * SinTheta;
 result[2] := SphereRadius * CosTheta;
end;

function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereTheta0: TVector3Single): TVector3Single;
var NewX, NewY: TVector3Single;
    SphereRadius, NewXLen, NewYLen: Single;
begin
 result := PhiThetaToXYZ(PhiTheta, 1);

 { chce zeby NewX bylo dowolnym wektorem prostopadlym do SphereTheta0.
   (i zeby nie bylo wektorem zerowym). }
 if IsZero(SphereTheta0[0]) and IsZero(SphereTheta0[1]) then
 begin
  { to na pewno SphereTheta0[2] <> 0 wiec ponizszy NewX na pewno bedzie niezerowy : }
  NewX[0] := 0;
  NewX[1] := -SphereTheta0[2];
  NewX[2] := SphereTheta0[1];
 end else
 begin
  NewX[0] := -SphereTheta0[1];
  NewX[1] := SphereTheta0[0];
  NewX[2] := 0;
 end;
 { teraz licz NewY = wektor prostopadly do NewX i NewY }
 NewY := VectorProduct(SphereTheta0, NewX);
 { ustaw prawidlowe dlugosci NewX i NewY. Robimy mala zabawe zeby NewYLen mozna
   bylo policzyc uzywajac mnozenia, zamiast VectorLen (ktore wymaga
   pierwiastkowania). Korzystamy przy tym ze NewY to wynik odpowiedniego
   VectorProduct i ze kat miedzy NewX a SphereTheta0 = 90 stopni. }
 SphereRadius := VectorLen(SphereTheta0);
 NewXLen := VectorLen(NewX);
 NewYLen := NewXLen * SphereRadius;

 VectorScaleTo1st(NewX, SphereRadius/NewXLen);
 VectorScaleTo1st(NewY, SphereRadius/NewYLen);

 { TODO: zrob MultMatrixPointTo1st, bedzie szybciej }
 result := MultMatrixPoint(TransformToCoordsMatrix(ZeroVector3Single,
   NewX,
   NewY,
   SphereTheta0), result);
end;

function RandomUnitHemispherePointDensityConst: TVector2Single;
begin
 result[0] := 2*Pi*Random;
 result[1] := ArcCos(Random);
end;

function RandomUnitHemispherePointDensityConstXYZ: TVector3Single;
var r1, r2, sqroot: Single;
    cosinus, sinus: Float;
begin
 r1 := Random;
 r2 := Random;
 SinCos(2*Pi*r1, sinus, cosinus);
 sqroot := Sqrt(1-Sqr(r2));

 result[0] := cosinus * sqroot;
 result[1] := sinus * sqroot;
 result[2] := r2;
end;

function RandomUnitHemispherePointDensityCosTheta(
  out PdfValue: Single): TVector2Single;
var SqrtR2: Float;
begin
 SqrtR2 := Sqrt(Random);

 result[0] := 2*Pi*Random;
 result[1] := ArcCos(SqrtR2);
 PdfValue := SqrtR2 / Pi;
end;

function RandomUnitHemispherePointDensityCosThetaXYZ(
  out PdfValue: Single): TVector3Single;
var SqRoot, r1, r2: Single;
    SinR1, CosR1: Float;
begin
 r1 := Random;
 r2 := Random;
 SinCos(2*Pi*r1, SinR1, CosR1);
 SqRoot := Sqrt(1-r2);

 result[0] := CosR1 * SqRoot;
 result[1] := SinR1 * SqRoot;
 result[2] := Sqrt(r2);
 PdfValue := result[2];
end;

function RandomUnitHemispherePointDensityCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2Single;
var r2: Float;
begin
 r2 := Random;

 result[0] := 2*Pi*Random;
 result[1] := ArcCos(Power(r2, 1/(n+1)));
 PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

function RandomUnitHemispherePointDensityCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3Single;
var r1, r2, r2Power, r2Root: Single;
    SinR1, CosR1: Float;
begin
 r1 := Random;
 r2 := Random;
 SinCos(2*Pi*r1, SinR1, CosR1);
 r2Power := Power(r2, 1/(n+1));
 r2Root := Sqrt(1-Sqr(r2Power));

 result[0] := CosR1 * r2Root;
 result[1] := SinR1 * r2Root;
 result[2] := r2Power;
 PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

end.
