{
  Copyright 2003-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Rendering VRML models using ray-tracing.) }

unit VRMLRayTracer;

{$I kambiconf.inc}

{ TODO:
  - for classic raytracer do shadow cache
  - for classic raytracer use various space filling curves
  - now that FPC inline is stable and cross-unit, use it to inline
    various things from VectorMath. Check speed.
}

{ Define PATHTR_USES_SHADOW_CACHE to make path tracing use shadow cache.
  Speed gain is small (sometimes it's even a little worse with
  shadow cache (see /win/3dmodels/rayhunter-demos/raporty/shadow-cache)),
  but in general it's a good idea. For appropriate scenes, speed gain
  is more than 110%. }
{$define PATHTR_USES_SHADOW_CACHE}

interface

uses VectorMath, Images, RaysWindow,
  VRMLTriangle, VRMLNodes, SpaceFillingCurves, Matrix;

type
  TPixelsMadeNotifierFunc = procedure(PixelsMadeCount: Cardinal; Data: Pointer);

  { ten typ nie jest nigdzie uzywany w tym module (chociaz najprawdopodobniej
    kiedys bedzie) ale moze byc przydatny dla kazdego kodu chcacego
    wywolac jakiegos raytracera. }
  TRayTracerKind = (rtkClassic, rtkPathTracer);

  TRayTracer = class
  public
    { This describes the actual scene that will be used.
      Must be set before calling @link(Execute). }
    Octree: TVRMLBaseTrianglesOctree;

    { Image where the ray-tracer result will be stored.
      Must be set before calling @link(Execute).

      We will not resize given here Image. Instead we will use it's
      current size --- so you just have to set Image size as appropriate
      before calling this method.

      We will write on image using TImage.SetColorRGB
      method, so this method must be implemented in Image class you use
      (it's implemented in all 3 classes TRGBImage, TRGBAlphaImage, TRGBEImage
      in Images unit, so usually you just don't worry about that).

      Dla kazdego pixela w ktorym promien trafia na obiekt
      ze sceny zapisujemy w obrazku wyliczony kolor RGB sceny w tym miejscu.
      Nie dotykamy kanalu Alpha obrazka TRGBAlphaImage.
      (kiedys mialem tu mechanizm obslugi
      tego kanalu ale zbytnio mi zawadzal a i tak nie byl uzywany, dlatego
      go wylaczylem). Akceptujemy naturalnie obrazki TRGBEImage i bedziemy w nich
      zapisywali precyzyjne kolory --- de facto format TRGBEImage zaimplementowalem
      wlasnie po to zeby raytracery w tym module mogly zapisywac obrazki
      precyzyjnie, z kolorami jako 3xFloat.

      Ponadto, gdy uzywasz dowolnego formatu innego niz TRGBEImage (patrzac
      przyszlosciowo : dowolnego formatu z precyzja i zakresem float) tracisz nie
      tylko precyzje : kolory 3xSingle sa konwertowane na 3xByte a
      konwersja Byte->Single jest robiona
      tak jak VectorMath.Vector3Byte : zakres Single 0-1 jest skalowany na
      zakres bajtu 0-255, jezeli wartosc Single jest wieksza niz 1 (co jest
      przeciez zupelnie mozliwe) to jest obcinana do 1. Wiec zapisujac do
      ikRGB/ikAlpha moze sie okazac ze przy bardzo jasnej scenie wyjdzie ci
      caly bialy obrazek - podczas gdy przy ikRGBE obrazek mialby po prostu kolory
      wieksze niz (1.0, 1.0, 1.0) co moznaby zawsze zniwelowac skalowaniem
      albo korekcja gamma. Ta zaleta formatu RGBE jest pokrotce opisana
      takze na poczatku dokumentacji do rayhuntera na
      [http://vrmlengine.sourceforge.net/rayhunter.php].

      (Ta wada formatow RGB/Alpha @italic(nie bedzie naprawiona w tej funkcji)
      bo niby jak ? Jedynym sensownym rozwiazaniem jest tutaj zapisac obrazek
      raytracerem w formacie RGBE a potem, po wygenerowaniu calosci,
      ustalic maksymalna wartosc komponentu i zrobic odpowiednie skalowanie
      lub korekcje gamma; ale do tego nie musze wcale naprawiac tej funkcji ---
      wiec mozna co najwyzej uznac to za blad rayhuntera i view3dscene
      ze takiego naprawiania nie robia po uzyciu RaytraceTo1st.
      Ale: view3dscene zrzuca sprawe na "view3dscene nigdy nie mial realizowac
      raytracingu do celu innego niz tylko testowanie, tzn. view3dscene
      nie sluzy do generowania ostatecznych dopieszczonych obrazkow - od tego
      jest rayhunter". A rayhunter argumentuje ze "zeby to zrobic to w srodku
      rayhuntera musialbym robic zapis do RGBE i dopiero na koncu konwertowac
      do RGB : w ten sposob nie tylko komplikuje sobie kod ale takze
      robienie @--write-partial-row staje sie klopotliwe. A wiec, na podobnej
      zasadzie jak funkcja @link(Execute) zwala problem na kod zewnetrzny,
      tak rayhunter zwala problem na program zewnetrzny. Innymi slowy,
      zawsze generuj obrazki do RGBE jesli nie chcesz miec tego problemu.
      Zreszta, Radiance tez tak robi : zawsze zapisuje obrazki
      do swojego RGBE i potem w czasie post-processingu obrazka umozliwia
      robienie korekcji kolorow)" )

      Wszystkie pixle obrazka zostana zapisane przez raytracer. Kiedys
      mialem tu mechanizm umozliwiajacy podlozenie pod rendering tla,
      ale skasowalem to bo bylo malo uzyteczne a bardzo nieeleganckie
      w zapisie (wymagalo ode mnie rozrozniania czy promien pierw. trafil
      w scene czy nie; teraz po prostu promien ktory nie trafia w scene
      przynosi kolor SceneBGColor). }
    Image: TImage;

    { Parametry CamPosition, CamDirection, CamUp naturalnie ustawiaja kamere
      w scenie. Tak jak zwykle, jezeli CamUp i CamDirection nie sa prostopadle
      to poprawiany jest CamUp aby byc prostopadly (a NIE camDirection). }
    CamPosition, CamDirection, CamUp: TVector3_Single;

    { ViewAngleDegX i ViewAngleDegY to rozpietosc obiektywu w stopniach. }
    ViewAngleDegX, ViewAngleDegY: Single;

    SceneBGColor: TVector3_Single;

    { PixelsMadeNotifier, jezeli <> nil, to bedzie wywolywane po zapisaniu
      kazdego pixla w Image. W ten sposob bedzie mozna wyswietlac obrazek
      juz w trakcie generowania, jezeli tylko bedzie taka potrzeba.
      PixelsMadeNotifier dostanie jako parametry liczbe pixli jaka zostala juz
      zrobiona i PixelsMadeNotifierData (ktory tradycyjnie mozesz dowolnie
      wykorzystac do wygodnej komunikacji z callbackiem).

      Na pytanie KTORE konkretnie pixele zostaly zrobione, tzn. gdzie na obrazku
      jest to PixelsDoneCount pixli, inaczej mowiac : w jakiej kolejnosci
      robimy pixle :
      dla ClassicRayTracera: jak TSwapScanCurve
      dla PathTracera: zalezy od SFCurveClass;

      Wszystko przez to ze SFCurveClass okazaly sie praktycznie bezuzyteczne dla
      path tracera, niczego tam nie przyspieszaja. Ale byc moze okaza sie bardziej
      laskawe dla Classic RayTracera. Na razie nie zaimplementowalem jeszcze
      w ClassicRayTracerze shadow-cache a wiec takze te SFCurveClass sa bez sensu,
      ale kiedys planuje to zrobic.

      Chwilowo powinienes zawsze uzywac w path tracerze BestRaytrSFC aby miec
      TSwapScanCurve ktore renderuje prosto wierszami.
      Wiekszych komplikacji nie ma sensu na razie
      gdziekolwiek wprowadzac skoro uzywanie innych SFC niz renderujace
      wierszami od dolu do gory jest bezcelowe (dla path tracera) lub
      niezaimplementowane (dla classic ray tracera).

      Remember that pixels not done yet have the same content as they
      had when you @link(Execute) method started. In other words,
      if you set PixelsMadeNotifier <> nil, then often it's
      desirable to initialize Image content (e.g. to all SceneBGColor)
      before calling @link(Execute). Otherwise at the time of @link(Execute)
      call, the pixels not done yet will have undefined colors. }
    PixelsMadeNotifier: TPixelsMadeNotifierFunc;
    PixelsMadeNotifierData: Pointer;

    { FirstPixel pozwala renderowac obrazki od srodka - normalnie FirstPixel = 0
      powoduje ze zaczynamy renderowac od pierwszego pixela i wyrenderujemy
      caly obrazek. FirstPixel > 0 oznacza ze zaczynamy gdzies dalej, uznajac
      pierwsze FirstPixel pixeli na Image za juz zrobione (i nie zapisujemy
      im zadnej wartosci). To jest przydatne do wznawianie dzialania ray tracera
      poprzednio przerwanego ktory zapisal czesciowo wygenerowany rysunek.
      FirstPixel musi byc w zakresie 0..Image.Width*Image.Height przy czym
      wartosc najwieksza (Image.Width*Image.Height) instruuje ray tracera zeby
      nic nie robil (caly obrazek jest juz wygenerowany).

      Zwracam uwage ze pierwszy parametr PixelMadeNotifier uwzglednia fakt tak
      jakbysmy zrobili juz FirstPixel pixeli, tzn. kolejne wywolania
      PixelMadeNotifier beda kolejno dostawaly parametry FirstPixel+1,
      FirstPixel+2 ... itd. az do Image.Width * Image.Height. }
    FirstPixel: Cardinal;

    { Do ray-tracing: write a ray-traced image into the @link(Image). }
    procedure Execute; virtual; abstract;
  end;

  { Classic Whitted-style ray-tracer.
    See [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/ch04s02.html]
    for documentation.

    Make sure that VRML2ActiveLights are properly initialized if you
    plan to render VRML 2.0 nodes. TVRMLScene and descendants do
    this for you usually. }
  TClassicRayTracer = class(TRayTracer)
  public
    procedure Execute; override;

    { InitialDepth to ograniczenie raytracera na globokosc drzewa promieni - tak jak
      zad.2 z rgk, tzn. InitialDepth = 0 oznacza tylko promienie prierw,
      1 = promienie 1-krotnie odbite + bezposrednie cienie itd. }
    InitialDepth: Cardinal;

    { Podaj FogNode <> nil aby miec odpowiednia mgle, zgodnie ze specyfik. VRMLa 97.
      FogDistanceScaling to skalowanie FogNode z transformacji sceny
      VRMLa w miejscu gdzie byl FogNode --- tak samo jak pole
      TVRMLScene o tej samej naziwe. }
    FogNode: TNodeFog;
    FogDistanceScaling: Single;
  end;

  { Path tracer. See
    [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/ch04s03.html]
    for documentation. }
  TPathTracer = class(TRayTracer)
  public
    constructor Create;
    procedure Execute; override;

    { MinDepth i RRoulContinue razem ustalaja warunki konczenia sciezki.
      Wyglada to tak ze do glebokosci rekursji MinDepth wchodzimy na pewno,
      natomiast glebiej wchodzimy pod warunkiem powodzenia w rosyjskiej
      ruletce, tzn. jezeli Depth <= 0 to musi byc Random < RRoulContinue i wtedy
      wywolujemy Trace(...,Depth-1) i wynik skalujemy przez 1/RRoulContinue.

      Jezeli nie chcesz uzywac w ogole rosyjskiej ruletki to daj RRoulContinue = 0.0
      (tak, porownywanie Random < RRoulContinue jak zwykle uzywa _ostrej_
      nierownosci, co jest konsekwente bo Random zwraca wyniki z przedzialu [0;1) ).
      Przestrzegam jednak przed rezygnowaniem z rosyjskiej ruletki - rezygnacja
      ta powoduje bias na obrazku; obrazek wynikowy jest ciemniejszy ni¿ powinien byæ.
      Przestrzegam te¿ przed dawaniem _bardzo_ malych wartosci dla RRoulContinue -
      w wyniku tego bedziemy niekiedy skalowali wynik przez 1/RRoulContinue
      (= bardzo duzo jezeli RRoulContinue = bardzo malo) a niekiedy wynik bedzie = 0,
      wiec rosyjska ruletka spowoduje tu olbrzymia wariancje (a wiec noise).
      RRoulContinue powinno byc w zakresie 0..1, jesli nie jest - bedzie clamped.

      Jezeli nie chcesz uzywac MinDepth (to znaczy chcesz zeby o kazdym wywolaniu
      rekurencyjnym decydowala rosyjska ruletka) to przekaz MinDepth = 0.
      (wartosci MinDepth < 0 w tej chwili sa w zasadzie dopuszczalne i
      traktowane przez implementacje tak samo jak MinDepth = 0 ale byc moze kiedys
      sie to zmieni).

      Uzywanie rosyjskiej ruletki gwarantuje nam ze nie mamy biasu (bez wzgledu
      na MinDepth). Uzywanie MinDepth zmniejsza wariancje (chociaz naturalnie
      wydluza czas dzialania bo przeciez sciezki beda mialy zawsze jakas minimalna
      dlugosc, no, za wyjatkiem tych nielicznych sciezek ktore urwa sie w sposob
      "naturalny", tzn. wyleca na zewnatrz sceny itp.). Razem, uzywanie MinDepth
      w polaczeniu z rosyjska ruletka daje wynik unbiased i czesciowo niweluje noise
      powodowany przez rosyjska ruletke. }
    MinDepth: Integer;
    RRoulContinue: Single;

    { Primary- i NonPrimary- SamplesCount okreslaja ile sciezek wygenerowac dla
      kazdego pixela (obie musza byc >0, co jest sprawdzane uzywajac Check(),
      nawet w wersji RELEASE). Primary mowi ile promieni pierwotnych jest
      puszczanych dla kazdego pixela obrazka; dla kazdego promienia pierwotnego
      ktory trafi w scene generowanych jest nastepnie NonPrimary sciezek
      (tzn. teraz juz prawdziwych sciezek, ktore nie rozgaleziaja sie dalej).
      Razem mamy wiec Primary*NonPrimary sciezek dla kazdego pixla.
      Jezeli dasz NonPrimary = 1 i bedziesz operowal tylko Primary to w rezultacie
      otrzymasz naiwna wersje path tracera, ktora np. dla 1000 sciezek
      bedzie musiala przesledzic 1000 (tyle samo) promieni. O ile wziecie
      1000 sciezek ma sens jesli chcesz miec ladny obrazek z path tracera
      o tyle liczenie az 1000 promieni pierwotnych ma tutaj male zastosowanie.
      Wiekszosc z tych promieni pierwotnych bedzie trafiala w jeden i ten sam
      punkt sceny i dopiero dalsze kroki sciezki beda determinowaly faktyczne
      roznice w kolorach przynoszonych przez konkretne sciezki.
      Innymi slowy, promienie pierwotne tworza scisla wiazke i chcemy to
      wykorzystac. W powyzszym przykladzie zmiana na Primary = 1 i NonPrimary = 1000
      da ciagle 1000 sciezek na scene (i rendering rzeczywiscie bedzie
      mial taka sama jakosc, taki sam poziom szumu) a czas renderowania
      bedzie mniejszy (o ile dokladnie bedzie krotszy - to zalezy od dlugosci
      sredniej sciezki, jesli sciezki sa b. dlugie to niewiele tutaj pomozemy
      bo w koncu optymalizujemy tylko 1 krok kazdej sciezki; ale zazwyczaj
      sciezki nie sa zbyt dlugie). Jedyna wada to ze zmniejszenie Primary do 1
      spowodowalo aliasing. Mozna temu zaradzic zwiekszajac (ale tylko
      nieznacznie !) Primary, np. Primary = 10 i NonPrimary = 100 powinno dac
      nam dobry antialiasing, zachowujac 1000 sciezek dla pixela i jednoczesnie
      lepszy czas renderowania. Faktem ktory ratuje nam tutaj tylek jest to
      ze dla antialiasingu nie potrzebujemy az tak wielu probek na pixel. }
    PrimarySamplesCount, NonPrimarySamplesCount: Cardinal;

    { DirectIllumSamplesCount okresla ile sampli ma byc wyslanych zeby
      w kazdym punkcie sciezki obliczyc bezposrednio direct illumination
      (tzn. kazdy taki sample jest skierowany w strone losowego punktu
      losowego zrodla swiatla). Podaj tutaj 0 aby miec "naiwny path tracing" :
      liczy direct i indirect illumination probujac wygenerowac sciezki ktore
      trafilyby w zrodlo swiatla. Potrzebuje naprawde olbrzymiej ilosci
      [Non]PrimarySamplesCount zeby dac jakiekolwiek rezultaty.
      Podaj tutaj 1 lub wiecej aby miec typowego path tracera.
      Dla wartosci rownej 1 rezultaty sa juz dobre. }
    DirectIllumSamplesCount: Cardinal;

    { For now it's not useful to change the value of this field.
      Using any other than default (TSwapScanCurve) curves doesn't
      give any speed benefit.

      TSwapScanCurve przynajmniej pozwala latwo oddzielic na obrazku
      czesc zrobiona i niezrobiona (i dzieki temu np. view3dscene moze
      szybciej rysowac obrazek w miare generowania). }
    SFCurveClass: TSpaceFillingCurveClass;
  end;

implementation

uses SysUtils, Math, KambiUtils, Boxes3d, IllumModels, SphereSampling;

{$I vectormathinlines.inc}

{ TODO: Note that we use in the implementation of this unit
  TVector*_* objects from FPC Matrix unit, while in the interface
  this unit (and many others) use TVector* arrays from VectorMath unit.

  VectorMath unit defines handy assignment operators that allow
  FPC to automagically convert between TVector*_* objects
  and TVector* arrays, so this is mostly unnoticeable in the syntax now.
  But be aware of this.
}

{ RayVector calculations ----------------------------------------------------- }

{ Calculate the transmitted ray created by hitting a ray
  - with direction NormRayVector (normalized ray direction is expected here)
  - from material with angle of refraction EtaFrom
  - transmitted into the material with angle of refraction EtaTo
  - hit occurs on the plane with normal vector (i.e. normalized) PlaneNormal }
function TryTransmittedRayVector(
  out TransmittedRayVector: TVector3_Single;
  const NormRayVector: TVector3_Single;
  const PlaneNormal: TVector4_Single;
  const EtaFrom, EtaTo: Single): boolean;
{ Written based on Foley, page 627 }
var
  EtaTransmission, RayIDotNormal, ToBeSqrRooted: Single;
  RayI: TVector3_Single;
  { This is the Normal pointing in the direction from where the RayVector came
    (i.e. in the opposite of RayVector,
    i.e. -RayVector (note the "-") and Normal must point to the same side
    of plane with PlaneNormal) }
  Normal: TVector3_Single;
begin
  Normal := PlaneDirNotInDirection(PlaneNormal, NormRayVector);
  RayI := -NormRayVector;

  RayIDotNormal := RayI ** Normal;

  { EtaTransmission is the ratio between angles of refraction of materials
    that change when the transmitted ray enters to the other side
    of the plane. }
  EtaTransmission := EtaFrom / EtaTo;

  ToBeSqrRooted := 1 - Sqr(EtaTransmission) * (1 - Sqr(RayIDotNormal));

  Result := ToBeSqrRooted >= 0;
  if Result then
    TransmittedRayVector :=
      (Normal * (EtaTransmission * RayIDotNormal - Sqrt(ToBeSqrRooted)))
      - (RayI * EtaTransmission);
end;

{ Calculate the perfect reflected vector.
  Arguments NormRayVector and PlaneNormal like for TryTransmittedRayVector. }
function ReflectedRayVector(
  const NormRayVector: TVector3_Single;
  const PlaneNormal: TVector4_Single): TVector3_Single;
var
  Normal, NormNegatedRayVector: TVector3_Single;
begin
  { Calculate Normal like in TryTransmittedRayVector. }
  Normal := PlaneDirNotInDirection(PlaneNormal, NormRayVector);
  NormNegatedRayVector := -NormRayVector;

  { We calculate ray as mirror ray to NormNegatedRayVector.
    Calculation is just like in Foley (page 601, section (14.16)). }
  Result := (Normal * 2 * (Normal ** NormNegatedRayVector))
    - NormNegatedRayVector;
end;

{ TClassicRayTracer ---------------------------------------------------------- }

procedure TClassicRayTracer.Execute;
var
  FogType: Integer;

  { Traces the ray with given Depth.
    Returns @false if the ray didn't hit anything, otherwise
    returns @true and sets Color. }
  function Trace(const Ray0, RayVector: TVector3_Single; const Depth: Cardinal;
    const TriangleToIgnore: PVRMLTriangle; IgnoreMarginAtStart: boolean):
    TVector3_Single;
  var
    Intersection: TVector3_Single;
    IntersectNode: PVRMLTriangle;
    MaterialMirror, MaterialTransparency: Single;

    procedure ModifyColorByTransmittedRay;
    var
      TransmittedColor, TransmittedRayVec: TVector3_Single;
      EtaFrom, EtaTo: Single;
    const
      EtaConst = 1.3;
    begin
      { Make transmitted ray if Transparency > 0 and there
        is no total internal reflection
        [http://en.wikipedia.org/wiki/Total_internal_reflection]. }
      if MaterialTransparency > 0 then
      begin
        { TODO: we should get an information from our model here
          (from Octree IntersectNode item). But we don't have it for now.
          So for now we just always assume that the first transmission
          has Eta = EtaConst and the next one has 1/EtaConst
          and the next one has again EtaConst etc. }
        if Odd(InitialDepth - Depth) then
          begin EtaFrom := 1; EtaTo := EtaConst end else
          begin EtaFrom := EtaConst; EtaTo := 1 end;

        if TryTransmittedRayVector(
          TransmittedRayVec, Vector_Get_Normalized(RayVector),
          IntersectNode^.World.Plane, EtaFrom, EtaTo) then
        begin
          TransmittedColor := Trace(Intersection, TransmittedRayVec,
            Depth - 1, IntersectNode, true);
          Result :=  Result * (1 - MaterialTransparency) +
            TransmittedColor * MaterialTransparency;
        end;
      end;
    end;

    procedure ModifyColorByReflectedRay;
    var
      ReflRayVector, ReflColor: TVector3_Single;
    begin
      if MaterialMirror > 0 then
      begin
        ReflRayVector := ReflectedRayVector(Vector_Get_Normalized(RayVector),
          IntersectNode^.World.Plane);
        ReflColor := Trace(Intersection, ReflRayVector, Depth - 1,
          IntersectNode, true);
        Result := Result * (1 - MaterialMirror) + ReflColor * MaterialMirror;
      end;
    end;

    function LightNotBlocked(const Light: TActiveLight): boolean;
    begin
      { Does the light get to the current surface ?

        Note that we treat partially transparent objects here
        as not casting a shadow.
        This is better that not doing anything (partially transparent objects
        making normal "blocking" shadows looks bad), but it's not really correct,
        since in reality partially transparent objects just bend
        (or rather translate, if you consider a thin partially transparent object
        like a glass that doesn't intersect any other objects)
        the ray so it can get to the light.

        We also take into account that the light may be on the opposide
        side of the plane than from where RayVector came.
        In such case the light shines on IntersectNode, but from the opposite
        side, so we will not add it here. }
      Result := Octree.ActiveLightNotBlocked(Light,
        Intersection, IntersectNode^.World.Normal,
        -RayVector, IntersectNode, true);
    end;

  var
    i: integer;
    M1: TNodeMaterial_1;
    M2: TNodeMaterial_2;
    ActiveLights: TDynActiveLightArray;
  begin
    IntersectNode := Octree.RayCollision(Intersection.Data,
      Ray0, RayVector, true,
      TriangleToIgnore, IgnoreMarginAtStart, nil);
    if IntersectNode = nil then Exit(SceneBGColor);

    { calculate material properties, taking into account VRML 1.0 and 2.0
      material. }
    if IntersectNode^.State.ParentShape <> nil then
    begin
      { VRML 2.0 }
      M2 := IntersectNode^.State.ParentShape.Material;
      if M2 <> nil then
      begin
        MaterialMirror := M2.FdMirror.Value;
        MaterialTransparency := M2.FdTransparency.Value;
      end else
      begin
        MaterialMirror := DefaultMaterialMirror;
        MaterialTransparency := DefaultMaterialTransparency;
      end;
    end else
    begin
      { VRML 1.0 }
      M1 := IntersectNode^.State.LastNodes.Material;
      MaterialMirror := M1.Mirror(IntersectNode^.MatNum);
      MaterialTransparency := M1.Transparency(IntersectNode^.MatNum);
    end;

    Result := VRML97Emission(IntersectNode^, InitialDepth <> 0);
    with IntersectNode^ do
    begin
      if Depth > 0 then
      begin
        ActiveLights := State.CurrentActiveLights;
        for i := 0 to ActiveLights.Count - 1 do
          if LightNotBlocked(ActiveLights.Items[i]) then
            Result += VRML97LightContribution(
              ActiveLights.Items[i], Intersection, IntersectNode^, CamPosition);

        { Calculate recursively reflected and transmitted rays.
          Note that the order of calls (first reflected or first transmitted ?)
          is important --- as they may scale our Result (not only add
          to it). }
        ModifyColorByReflectedRay;
        ModifyColorByTransmittedRay;
      end;
    end;

    { That's right, VRMLFog calculation should be done on every
      Depth of ray-tracing, not only once (i.e. for primary rays).
      Reasoning: imagine that you look through a glass window
      (and you're really close to this window, so that fog doesn't
      affect the window glass noticeably) and through this window
      you see something distant, like a forest. The forest is distant
      so fog setting should affect it. Obviously even though the window
      glass wasn't affected much by the fog, you will see a forest
      covered by the fog. So each recursive call of Trace should bring
      a color affected by the fog. }
    if FogType <> -1 then
      Result := VRML97Fog(Result,
        PointsDistance(CamPosition, Intersection),
        FogNode, FogDistanceScaling, FogType);
  end;

var
  RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  begin
    Image.SetColorRGB(x, y,
      Trace(CamPosition, RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height),
        InitialDepth, nil, false));
  end;

var
  PixCoord: TVector2Cardinal;
  SFCurve: TSpaceFillingCurve;
begin
  FogType := VRML97FogType(FogNode);

  RaysWindow := nil;
  SFCurve := nil;
  try
    RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp,
      ViewAngleDegX, ViewAngleDegY);

    { Using any other kind of space filling curve doesn't have any
      noticeable impact right now for classic ray tracer, since
      classic ray tracer doesn't use shadow cache or any other similar technique
      that benefits from processing similar cases in one block.
      In the future this make come to use.
      Right now, using SFCurve just makes implementing FirstPixel
      and PixelsMadeNotifier easier. }
    SFCurve := TSwapScanCurve.Create(Image.Width, Image.Height);
    SFCurve.SkipPixels(FirstPixel);

    { generate image pixels }
    if Assigned(PixelsMadeNotifier) then
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
        PixelsMadeNotifier(SFCurve.PixelsDone, PixelsMadeNotifierData);
      end;
    end else
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
      end;
    end;

  finally
    RaysWindow.Free;
    SFCurve.Free;
  end;
end;

{ TPathTracer -------------------------------------------------------------- }

constructor TPathTracer.Create;
begin
  inherited;
  SFCurveClass := TSwapScanCurve;
end;

{ Some notes about path tracer implementation :

  - Meaning of TraceOnlyIndirect parameter for Trace():

    When (DirectIllumSamplesCount <> 0) then rendering equation at each point
    is splitted into
      L_out = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    where
      L_indirect = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    and L_indirect must hit something that is *not* a light source.
    Which means that L_emission is always here = (0, 0, 0) so we have
      L_indirect = IntegralOverHemisphere( L_direct + L_indirect ),

    Which means that when we do recursive calls to Trace to calculate
    L_indirect we *do not want to hit light source* since then
    L_direct would be calculated twice.

    In other words,
    - for primary rays we pass TraceOnIndirect = false
      since we calculate L_out and we want L_emission (which makes sense anyway,
      since when you look directly at the light source you obviously see it's
      light).

    - for non-primary rays, i.e. in Trace recursive call from Trace itself,
      we pass TraceOnIndirect = true. (Well, unless DirectIllumSamplesCount = 0,
      in which case we just always make normal rendering equation
      and we want to always calculate L_emission;
      So TraceOnlyIndirect is actually DirectIllumSamplesCount <> 0).

    Tests confirm that this is right. Without this (if we would remove
    the check
    "if TraceOnlyIndirect and IsLightSource(IntersectNode) then ...")
    we get a lot more noise on our image.

    Trace call with TraceOnlyIndirect = true that hits into a light source
    returns just black color.

  - Note that MinDepth and Depth (Trace parameters) are *signed* integers.
    Depth can be negative, for each recursive call we pass Depth = Depth - 1.
    When Depth <= 0 then roussian roulette will be used.
    MinDepth is also signed becasue of that, since it's a starting Depth value.
}

procedure TPathTracer.Execute;
var
  { In LightItems we have pointers to Octree.Triangles[] pointing
    to the items with emission color > 0. In other words, light sources. }
  LightItems: TDynPointerArray;

  {$ifdef PATHTR_USES_SHADOW_CACHE}
  { For each light in LightItems[I], ShadowCache[I] gives the pointer
    (somewhere into Octree.Triangles[]) of the last object that blocked this
    light source (or nil if no such object was yet found during
    this path tracer execution).

    This index is updated and used in IsLightShadowed.
    The idea of "shadow cache" comes from RGK, crystalized in "Graphic Gems II". }
  ShadowCache: TDynPointerArray;
  {$endif}

  { TODO: comments below are in Polish. }

const
  { LightEmissionArea definiuje co znacza wlasciwosci .Emission zrodel swiatla.
    Kolor emission swiatla bedzie oczywiscie skalowany przez kat brylowy jaki
    ma zrodlo swiatla dla oswietlanego punktu. Skalowanie to bedzie robione tak
    ze jezeli swiatlo bedzie zajmowalo kat brylowy LightEmissionArea
    (w steradianach) to kolor swiatla dodany do DirectIllumination bedzie wynosil
    doklanie Emission swiatla. Jesli kat brylowy bedzie a razy wiekszy (mniejszy)
    niz LightEmissionArea to kolor jaki bedzie mial byc dodany do Direct Illumination
    bedzie wynosil a*LightEmissionArea.

    Oczywiscie ten kolor bedzie dalej skalowany, przez BRDFa i inne rzeczy,
    wiec to nie oznacza ze dokladnie taki kolor a*Emission zostanie zwrocony
    przez DirectIllumination. Ale taka bedzie "podstawa" tego koloru.

    Mowiac nieco formalnie, LightEmissionArea okresla w jakich jednostkach
    mamy zapisane Emission swiatla w modelu.

    Wartosc ponizej dobralem eksperymentalnie chcac zeby cornell-box renderowane
    z path wygladalo tak samo jak rendering na stronie www.cornell-box'a.
    Dopasowywalem ten parametr tak dlugo az rysunki path (z rosyjska ruletka,
    bo ona nie wprowadza biasu) mialy "na oko" podobna jasnosc co tamtejszy
    box.jpg. }
  LightEmissionArea = 1/30;

  function IsLightSource(const Item: PVRMLTriangle): boolean;
  begin
    Result := VectorLenSqr(Item^.State.LastNodes.Material.
      EmissiveColor3Single(Item^.MatNum))
      > Sqr(SingleEqualityEpsilon);
  end;

  function IsLightShadowed(const Item: PVRMLTriangle;
    const ItemPoint: TVector3_Single;
    const LightSourceIndiceIndex: Integer;
    LightSourcePoint: TVector3_Single): boolean;
  { ta funkcja liczy shadow ray (a w zasadzie segment). Zwraca true jezeli
    pomiedzy punktem ItemPoint a LightSourcePoint jest jakis element
    o transparency = 1. Wpp. zwraca false.
    LightSourceIndiceIndex to indeks to tablicy LightItems[].
    Item to pointer to given item (somewhere in Octree.Triangles[]). }
  { TODO: transparent objects should scale light color instead of just
    letting it pass }
  var
    OctreeIgnorer: TVRMLOctreeIgnoreTransparentAndOneItem;
    Shadower: PVRMLTriangle;
  {$ifdef PATHTR_USES_SHADOW_CACHE}
    CachedShadower: PVRMLTriangle;
  {$endif}
  begin
    {$ifdef PATHTR_USES_SHADOW_CACHE}
    { sprobuj wziac wynik z ShadowCache }
    CachedShadower := ShadowCache.Items[LightSourceIndiceIndex];
    if (CachedShadower <> nil) and
       (CachedShadower <> Item) then
    begin
      {TODO:Inc(Octree.DirectCollisionTestsCounter);}
      if IsTriangleSegmentCollision(CachedShadower^.World.Triangle,
        CachedShadower^.World.Plane, ItemPoint, LightSourcePoint) then
        Exit(true);

      { powyzej zapominamy o marginesie epsilonowym wokol ItemPoint i
        LightSourcePoint (jezeli tam jest przeciecie to powinno byc uznawane
        za niewazne). Zreszta ponizej zapominamy o marginesie wokol
        LightSourcePoint. W moim kodzie te marginesy epsilonowe nie sa tak
        wazne (tylko dla nieprawidlowych modeli, dla prawidlowych modeli
        wystarcza ItemIndexToIgnore i OctreeIgnorer) wiec olewam tutaj te
        niedorobki. }
    end;
    {$endif}

    { oblicz przeciecie uzywajac Octree }
    OctreeIgnorer := TVRMLOctreeIgnoreTransparentAndOneItem.Create(
      LightItems.Items[LightSourceIndiceIndex]);
    try
      Shadower := Octree.SegmentCollision(ItemPoint, LightSourcePoint, false,
        Item, true, @OctreeIgnorer.IgnoreItem);
      Result := Shadower <> nil;
      {$ifdef PATHTR_USES_SHADOW_CACHE}
      ShadowCache.Items[LightSourceIndiceIndex] := Shadower;
      {$endif}
    finally OctreeIgnorer.Free end;
  end;

  function Trace(const Ray0, RayVector: TVector3_Single;
    const Depth: Integer; const TriangleToIgnore: PVRMLTriangle;
    const IgnoreMarginAtStart: boolean; const TraceOnlyIndirect: boolean)
    : TVector3_Single;
  { sledzi promien z zadana glebokoscia. Zwraca Black (0, 0, 0) jesli
    promien w nic nie trafia, wpp. zwraca wyliczony kolor. }
  var
    Intersection: TVector3_Single;
    IntersectNode: PVRMLTriangle;
    MaterialNode: TNodeMaterial_1; { = IntersectNode.State.LastNodes.Material }
    IntersectNormalInRay0Dir: TVector3_Single;

    function TraceNonEmissivePart: TVector3_Single;

      function TryCalculateTransmittedSpecularRayVector(
        var TracedDir: TVector3_Single;
        var PdfValue: Single): boolean;
      var
        TransmittedRayVector: TVector3_Single;
        EtaFrom, EtaTo: Single;
      const
        EtaConst = 1.3; { TODO: tu tez uzywam EtaConst, jak w Classic }
      begin
        if Odd(MinDepth-Depth) then
          begin EtaFrom := 1; EtaTo := EtaConst end else
          begin EtaFrom := EtaConst; EtaTo := 1 end;

        Result := TryTransmittedRayVector(TransmittedRayVector,
          Vector_Get_Normalized(RayVector),
          IntersectNode^.World.Plane, EtaFrom, EtaTo);
        if Result then
          TracedDir := PhiThetaToXYZ(
            RandomUnitHemispherePointDensityCosThetaExp(
              Round(MaterialNode.TransSpecularExp(IntersectNode^.MatNum)),
              PdfValue),
            TransmittedRayVector);
      end;

      function DirectIllumination: TVector3_Single;
      { ta funkcja liczy DirectIllumination dla naszego Intersection.
        Implementacja : uzywamy sformulowania (101) z GlobalIllumCompendium :

        for i = 0..DirectIllumSamplesCount-1 do
          uniformly losuj LightItemIndex sposrod 0..LightIndices.Count-1.
          uniformly (wzgledem pola powierzchni trojkata wylosowanego swiatla)
            losuj punkt na swietle jako SampleLightPoint.
          if (SampleLightPoint widoczny z Intersection) then
            result += PolePowierzchni(LightItem) * LightEmission * BRDF *
              GeometryFunction
        Na koncu result *= LightIndices.Count / DirectIllumSamplesCount.

        Mozna powiedziec ze instrukcja
          result += PolePowierzchni(LightItem) * LightEmission * BRDF *
            GeometryFunction
        jest mniej wiecej rownowazne
          result += LightEmission * BRDF * cos(LightDirNorm, IntersectNormalInRay0Dir)
            * solid-angle-swiatla
        (taka jest rola PolePowierzchni(LightItem) i czesci GeometryFunction -
        one po prostu licza solid angle; no, de facto pewne bardzo dobre przyblizenie
        solid angle)

        W rezultacie result = sredni kolor ze swiatla razy srednia powierzchnia
          swiatla * ilosc swiatel = wlasnie direct illumination, uwzgledniajace
          ze rozne swiatla maja rozna powierzchnie i swieca z rozna intensywnoscia.
      }
      { TODO: better approach : (102), czyli losuj punkt na zrodle swiatla
              ze wzgledu na jego solid angle.
        TODO: jeszcze lepiej : (103), czyli losuj swiatlo w taki sposob ze
              swiatla o wiekszej powierzchni (a wlasciwie, o wiekszym kacie
              brylowym) i/lub o wiekszej intensywnosci beda wybierane czesciej. }
      var
        LightSource: PVRMLTriangle;
        LightSourceIndiceIndex: Integer; { indeks do LightIndices[] }
        SampleLightPoint: TVector3_Single;
        DirectColor, LightDirNorm, NegatedLightDirNorm: TVector3_Single;
        i: integer;
      begin
        Result.Init_Zero;

        { trzeba ustrzec sie tu przed LightsItems.Count = 0 (zeby moc pozniej
          spokojnie robic Random(LightsItems.Count) i przed
          DirectIllumSamplesCount = 0 (zeby moc pozniej spokojnie podzielic przez
          DirectIllumSamplesCount). }
        if (LightItems.Count = 0) or (DirectIllumSamplesCount = 0) then Exit;

        for i := 0 to DirectIllumSamplesCount - 1 do
        begin
          { calculate LightSourceIndiceIndex, LightSourceIndex, LightSource }
          LightSourceIndiceIndex := Random(LightItems.Count);
          LightSource := LightItems.Items[LightSourceIndiceIndex];
          if LightSource = IntersectNode then Continue;

          { calculate SampleLightPoint.
            Lepiej pozniej sprawdz ze SampleLightPoint jest
            rozny od Intersection (poniewaz SampleLightPoint jest losowy to na
            nieprawidlowo skonstruowanym modelu wszystko moze sie zdarzyc...)  }
          SampleLightPoint := SampleTrianglePoint(LightSource^.World.Triangle);
          if VectorsEqual(SampleLightPoint, Intersection) then Continue;

          { calculate LigtDirNorm (nieznormalizowane).
            Jezeli LigtDirNorm wychodzi z innej strony
            IntersectionNode.TriangleNormPlane niz IntersectNormalInRay0Dir
            to znaczy ze swiatlo jest po przeciwnej stronie plane - wiec
            swiatlo nie oswietla naszego pixela. }
          LightDirNorm := SampleLightPoint - Intersection;
          if not VectorsSamePlaneDirections(LightDirNorm, IntersectNormalInRay0Dir,
            IntersectNode^.World.Plane) then Continue;

          { sprawdz IsLightShadowed, czyli zrob shadow ray }
          if IsLightShadowed(IntersectNode, Intersection,
            LightSourceIndiceIndex, SampleLightPoint) then Continue;

          { calculate DirectColor = kolor emission swiatla }
          DirectColor := LightSource^.State.LastNodes.Material.
            EmissiveColor3Single(LightSource^.MatNum);

          { wymnoz przez naszego "niby-BRDFa" czyli po prostu przez kolor Diffuse
            materialu }
          DirectColor *= MaterialNode.DiffuseColor3Single(IntersectNode^.MatNum);

          { calculate LightDirNorm (znormalizowane), NegatedLightDirNorm }
          Vector_Normalize(LightDirNorm);
          NegatedLightDirNorm := -LightDirNorm;

          { Wymnoz DirectColor
            1) przez GeometryFunction czyli
                 cos(LightDirNorm, IntersectNormalInRay0Dir)
                   * cos(-LightDirNorm, LightSource.World.Normal) /
                   PointsDistanceSqr(SampleLightPoint, Intersection).
               Cosinusy naturalnie licz uzywajac dot product.
            2) przez TriangleArea

            Mozna zauwazyc ze czlon
              TriangleArea *
              cos(-LightDirNorm, LightSource.World.Normal) /
                PointsDistanceSqr(SampleLightPoint, Intersection)
            liczy po prostu solid angle swiatla with respect to Intersection
            (no, mowiac scisle pewne bardzo dobre przyblizenie tego solid angle).

            Moze byc tutaj pouczajace zobaczyc jak to dziala gdy usuniemy mnozenie
              przez cos(-LightDirNorm, LightSource.World.Normal)
              (swiatlo bedzie wtedy jasniej swiecilo jakby "w bok"),
            pouczajace moze tez byc usuniecie dzielenia przez
              PointsDistanceSqr(SampleLightPoint, Intersection) i jednoczesnie
              mnozenia przez TriangleArea (te dwie rzeczy "wspolpracuja ze soba",
              tzn. wazny jest tu wlasnie ich iloraz, dlatego usuwanie tylko
              jednej z tych wartosci nie ma sensu).

            Elegancko byloby tutaj pomnozyc jeszcze przez
              1/LightEmissionArea. Ale poniewaz LightEmissionArea = const wiec
              przenioslem mnozenie przez LightEmissionArea na sam koniec tej
              funkcji.}
          DirectColor *=
            (LightDirNorm ** IntersectNormalInRay0Dir) *
            (NegatedLightDirNorm **
              PlaneDirInDirection(LightSource^.World.Plane,
                NegatedLightDirNorm)) *
            LightSource^.World.Area /
            PointsDistanceSqr(SampleLightPoint, Intersection);

          Result += DirectColor;
        end;

        { dopiero tu przemnoz przez 1/LightEmissionArea.
          Podziel tez przez ilosc probek i pomnoz przez ilosc swiatel -
          - w rezultacie spraw zeby wynik przyblizal sume wkladu direct illumination
          wszystkich swiatel. }
        Result *= LightItems.Count /
          (LightEmissionArea * DirectIllumSamplesCount);
      end;

    type
      { kolory Transmittive/Reflective Diffuse/Specular }
      TColorKind = (ckRS, ckRD, ckTS, ckTD);
    var
      Colors: array[TColorKind]of TVector3_Single;
      Weights: array[TColorKind]of Single;
      WeightsSum: Single;
      RandomCK: Single;
      PdfValue: Single;
      TracedCol, TracedDir: TVector3_Single;
      ck: TColorKind;
    begin
      Result.Init_Zero;
      { caly result jaki tu wyliczymy dostaniemy dzieki wygranej w rosyjskiej
        ruletce jezeli Depth <= 0. (Trzeba o tym pamietac i pozniej podzielic
        przez RROulContinue.) }

      if (Depth > 0) or (Random < RRoulContinue) then
      begin
        { krok sciezki to importance sampling zgodnie z Modified Phong BRDF,
          patrz GlobalIllumComp (66), diffuse samplujemy z gestoscia cos(),
          specular z gestoscia cos()^N_EXP.

          W rezultacie po otrzymaniu wyniku Trace diffuse nie dziele juz wyniku
          przez cosinus() (a powinienem, bo to jest importance sampling) ani
          nie mnoze go przez cosinus() (a powinienem, bo w calce BRDF'a jest
          ten cosinus - diffuse oznacza zbieranie ze wszystkich kierunkow swiatla
          rownomiernie ale pod mniejszym katem na powierzchnie pada mniej promieni,
          dlatego w diffuse mamy cosinus). Wszystko dlatego ze te cosinusy sie
          skracaja.

          Podobnie dla specular - mam nadzieje ! TODO: Specular jeszcze
          nie jest zbyt dobrze przetestowane...

          W rezultacie kompletnie ignoruje PdfValue (otrzymywane w wyniku
          RandomUnitHemispeherePoint) i BRDF'a - po prostu akurat taki rozklad PDF'ow
          odpowiada DOKLADNIE temu jak wpada swiatlo. }

        { calculate Colors[] }
        Colors[ckRS] := MaterialNode.ReflSpecular (IntersectNode^.MatNum);
        Colors[ckRD] := MaterialNode.ReflDiffuse  (IntersectNode^.MatNum);
        Colors[ckTS] := MaterialNode.TransSpecular(IntersectNode^.MatNum);
        Colors[ckTD] := MaterialNode.TransDiffuse (IntersectNode^.MatNum);

        { calculate Weights[] and WeightSum }
        WeightsSum := 0;
        for ck := Low(ck) to High(ck) do
        begin
          Weights[ck] := Colors[ck].Data[0] +
                         Colors[ck].Data[1] +
                         Colors[ck].Data[2];
          WeightsSum += Weights[ck];
        end;

        { wylosuj jedno z ck : wylosuj zmienna RandomCK z przedzialu 0..WeightsSum
          a potem zbadaj do ktorego z przedzialow Weights[] wpada. Calculate ck. }
        RandomCK := Random * WeightsSum;
        ck := Low(ck);
        while ck < High(ck) do
        begin
          if RandomCK < Weights[ck] then break;
          RandomCK -= Weights[ck];
          Inc(ck);
        end;

        { notka : nie, ponizej nie mozna zamienic na test WeightsSum >
          SingleEqualityEpsilon. Nawet gdy to zachodzi ciagle moze sie okazac
          ze WeightsSum jest wprawdzie duzo wieksze od zera ale samo
          Weights[ck] jest mikroskopijnie male (i po prostu mielismy duzo
          szczescia w losowaniu; path tracer robi tyle sciezek, tyle pixeli
          itd. ze nietrudno tutaj "przez przypadek" wylosowac mikroskopijnie
          mala wartosc). }
        if Weights[ck] > SingleEqualityEpsilon then
        begin
          { calculate IntersectNormalInRay0Dir - Normal at intersection in direction Ray0 }
          IntersectNormalInRay0Dir := PlaneDirNotInDirection(
            IntersectNode^.World.Plane, RayVector);

          { calculate TracedDir i PdfValue samplujac odpowiednio polsfere
           (na podstawie ck). W przypadku TS moze wystapic calk. odbicie wewn.
           i wtedy konczymy sciezke. }
          case ck of
            ckTD: TracedDir := PhiThetaToXYZ(
                    RandomUnitHemispherePointDensityCosTheta(PdfValue),
                    -IntersectNormalInRay0Dir);
            ckTS: if not TryCalculateTransmittedSpecularRayVector(
                    TracedDir, PdfValue) then Exit;
            ckRD: TracedDir := PhiThetaToXYZ(
                    RandomUnitHemispherePointDensityCosTheta(PdfValue),
                    IntersectNormalInRay0Dir);
            ckRS: TracedDir := PhiThetaToXYZ(
                    RandomUnitHemispherePointDensityCosThetaExp(
                      Round(MaterialNode.ReflSpecularExp(IntersectNode^.MatNum)),
                      PdfValue),
                    ReflectedRayVector(Vector_Get_Normalized(RayVector),
                      IntersectNode^.World.Plane));
          end;

          { wywolaj rekurencyjnie Trace(), a wiec idz sciezka dalej }
          TracedCol := Trace(Intersection, TracedDir, Depth - 1,
            IntersectNode, true, DirectIllumSamplesCount <> 0);

          { przetworz TracedCol : wymnoz przez Colors[ck], podziel przez szanse
            jego wyboru sposrod czterech Colors[], czyli przez
            Weights[ck]/WeightsSum (bo to w koncu jest importance sampling)
            (czyli pomnoz przez WeightsSum/Weights[ck], wiemy ze mianownik jest
            > SingleEqualityEpsilon, sprawdzilismy to juz wczesniej). }
          TracedCol *= Colors[ck];
          TracedCol *= WeightsSum / Weights[ck];

          Result += TracedCol;
        end;

        { dodaj DirectIllumination }
        Result += DirectIllumination;

        { Jezeli weszlismy tu dzieki rosyjskiej ruletce (a wiec jezeli Depth <= 0)
          to skaluj Result zeby zapisany tu estymator byl unbiased. }
        if Depth <= 0 then Result *= 1/RRoulContinue;
      end;
    end;

  var
    i: Integer;
    NonEmissiveColor: TVector3_Single;
  begin
    IntersectNode := Octree.RayCollision(Intersection.Data, Ray0, RayVector, true,
      TriangleToIgnore, IgnoreMarginAtStart, nil);
    if IntersectNode = nil then Exit(SceneBGColor);

    if TraceOnlyIndirect and IsLightSource(IntersectNode) then
    begin
      Result.Init_Zero;
      Exit;
    end;

    MaterialNode := IntersectNode^.State.LastNodes.Material;
    { de facto jezeli TraceOnlyIndirect to ponizsza linijka na pewno dodaje
      do result (0, 0, 0). Ale nie widze w tej chwili jak z tego wyciagnac
      jakas specjalna optymalizacje. }
    Result := MaterialNode.EmissiveColor3Single(IntersectNode^.MatNum);

    { jezeli MinDepth = Depth to znaczy ze nasz Trace zwraca kolor dla primary ray.
      Wiec rozgaleziamy sie tutaj na NonPrimarySamplesCount, czyli dzialamy
        jakbysmy byly stochastycznym ray tracerem ktory rozgalezia sie
        na wiele promieni w punkcie rekursji.
      Wpp. idziemy sciezka czyli dzialamy jakbysmy byly path tracerem czyli
        nie rozgaleziamy sie na wiele promieni. }
    if MinDepth = Depth then
    begin
      NonEmissiveColor.Init_Zero;
      for i := 0 to NonPrimarySamplesCount-1 do
        NonEmissiveColor += TraceNonEmissivePart;
      NonEmissiveColor *= 1 / NonPrimarySamplesCount;
      Result += NonEmissiveColor;
    end else
      Result += TraceNonEmissivePart;
  end;

var
  RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  var
    PixColor, PrimaryRayVector: TVector3_Single;
    SampleNum: Integer;
  begin
    { generuj pixel x, y. calculate PixColor }
    if PrimarySamplesCount = 1 then
    begin
      { gdy PrimarySamplesCount = 1 to wysylamy jeden promien pierwotny
        i ten promien NIE jest losowany na rzutni w zakresie pixela
        x, y ale przechodzi dokladnie przez srodek pixela x, y. }
      PrimaryRayVector := RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height);
      PixColor := Trace(CamPosition, PrimaryRayVector, MinDepth,
        nil, false, false);
    end else
    begin
      PixColor.Init_Zero;
      for SampleNum := 0 to PrimarySamplesCount - 1 do
      begin
        PrimaryRayVector := RaysWindow.PrimaryRay(
          x + Random - 0.5, y + Random - 0.5,
          Image.Width, Image.Height);
        PixColor += Trace(CamPosition, PrimaryRayVector, MinDepth,
          nil, false, false);
      end;
      PixColor *= 1 / PrimarySamplesCount;
    end;

    { zapisz PixColor do Image }
    Image.SetColorRGB(x, y, PixColor);
  end;

var
  PixCoord: TVector2Cardinal;
//  i: integer;
  SFCurve: TSpaceFillingCurve;
begin
  { check parameters (path tracing i tak trwa bardzo dlugo wiec mozemy sobie
    pozwolic zeby na poczatku tej procedury wykonac kilka testow, nawet gdy
    kompilujemy sie w wersji RELEASE) }
  Check(PrimarySamplesCount > 0, 'PrimarySamplesCount for PathTracer must be greater than 0');
  Check(NonPrimarySamplesCount > 0, 'NonPrimarySamplesCount for PathTracer must be greater than 0');
  Clamp(RRoulContinue, Single(0.0), Single(1.0));

  { zainicjuj na nil'e, zeby moc napisac proste try..finally }
  LightItems := nil;
  {$ifdef PATHTR_USES_SHADOW_CACHE} ShadowCache := nil; {$endif}
  RaysWindow := nil;
  SFCurve := nil;
  try
    { calculate LightIndices }
    LightItems := TDynPointerArray.Create;
    {TODO:
    LightItems.AllowedCapacityOverflow := Octree.Triangles.Count div 4;
    for I := 0 to Octree.Triangles.Count - 1 do
      if IsLightSource(Octree.Triangles.Pointers[I]) then
        LightItems.AppendItem(Octree.Triangles.Pointers[I]);
    LightItems.AllowedCapacityOverflow := 4;}

    {$ifdef PATHTR_USES_SHADOW_CACHE}
    { calculate ShadowCache }
    ShadowCache := TDynPointerArray.Create;
    ShadowCache.SetLength(LightItems.Length);
    ShadowCache.SetAll(nil);
    {$endif}

    { calculate RaysWindow }
    RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp,
      ViewAngleDegX, ViewAngleDegY);

    { calculate SFCurve }
    SFCurve := SFCurveClass.Create(Image.Width, Image.Height);
    SFCurve.SkipPixels(FirstPixel);

    { generuj pixle obrazka }
    if Assigned(PixelsMadeNotifier) then
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
        PixelsMadeNotifier(SFCurve.PixelsDone, PixelsMadeNotifierData);
      end;
    end else
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
      end;
    end;

  finally
    SFCurve.Free;
    RaysWindow.Free;
    {$ifdef PATHTR_USES_SHADOW_CACHE} ShadowCache.Free; {$endif}
    LightItems.Free;
  end;
end;

end.
