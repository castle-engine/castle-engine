{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
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
  VRMLTriangleOctree, VRMLNodes, SpaceFillingCurves;

type
  TPixelsMadeNotifierFunc = procedure(PixelsMadeCount: Cardinal; Data: Pointer);

  { ten typ nie jest nigdzie uzywany w tym module (chociaz najprawdopodobniej
    kiedys bedzie) ale moze byc przydatny dla kazdego kodu chcacego
    wywolac jakiegos raytracera. }
  TRayTracerKind = (rtkClassic, rtkPathTracer);

{ RayTrace : do zadanego stworzonego juz obrazka Image zapisz do niego obraz
  utworzony ze sceny zapisanej w drzewie Octree.

  Istniejace wymiary obrazka beda zachowane, bedziemy tutaj zmieniac
  tylko jego RawPixels. We will write on image using TImage.SetColorRGB
  method, so this method must be implemented in Image class you use
  (it's implemented in all 3 classes TRGBImage, TAlphaImage, TRGBEImage
  in Images unit, so usually you just don't worry about that).

  Dla kazdego pixela w ktorym promien trafia na obiekt
    ze sceny zapisujemy w obrazku wyliczony kolor RGB sceny w tym miejscu.
    Nie dotykamy kanalu Alpha obrazka ikAlpha. (kiedys mialem tu mechanizm obslugi
    tego kanalu ale zbytnio mi zawadzal a i tak nie byl uzywany, dlatego
    go wylaczylem). Akceptujemy naturalnie obrazki ikRGBE i bedziemy w nich
    zapisywali precyzyjne kolory - de facto format ikRGBE zaimplementowalem
    wlasnie po to zeby raytracery w tym module mogly zapisywac obrazki
    precyzyjnie, z kolorami jako 3xFloat.

    Ponadto, gdy uzywasz dowolnego formatu innego niz ikRGBE (patrzac
    przyszlosciowo : dowolnego formatu z precyzja i zakresem float) tracisz nie
    tylko precyzje : przy zapisywaniu obrazka do ikRGB/ikAlpha kolory
    3xSingle sa konwertowane na 3xByte a konwersja Byte->Single jest robiona
    tak jak VectorMath.Vector3Byte : zakres Single 0-1 jest skalowany na
    zakres bajtu 0-255, jezeli wartosc Single jest wieksza niz 1 (co jest
    przeciez zupelnie mozliwe) to jest obcinana do 1. Wiec zapisujac do
    ikRGB/ikAlpha moze sie okazac ze przy bardzo jasnej scenie wyjdzie ci
    caly bialy obrazek - podczas gdy przy ikRGBE obrazek mialby po prostu kolory
    wieksze niz (1.0, 1.0, 1.0) co moznaby zawsze zniwelowac skalowaniem
    albo korekcja gamma. Ta zaleta formatu RGBE jest pokrotce opisana
    takze na poczatku dokumentacji do rayhuntera na camelot/rayhunter.php.

    (Ta wada formatow RGB/Alpha _nie bedzie naprawiona w tej funkcji_
    bo niby jak ? Jedynym sensownym rozwiazaniem jest tutaj zapisac obrazek
    raytracerem w formacie RGBE a potem, po wygenerowaniu calosci,
    ustalic maksymalna wartosc komponentu i zrobic odpowiednie skalowanie
    lub korekcje gamma; ale do tego nie musze wcale naprawiac tej funkcji -
    - wiec mozna co najwyzej uznac to za blad rayhuntera i view3dscene
    ze takiego naprawiania nie robia po uzyciu RaytraceTo1st.
    Ale: view3dscene zrzuca sprawe na "view3dscene nigdy nie mial realizowac
    raytracingu do celu innego niz tylko testowanie, tzn. view3dscene
    nie sluzy do generowania ostatecznych dopieszczonych obrazkow - od tego
    jest rayhunter". A rayhunter argumentuje ze "zeby to zrobic to w srodku
    rayhuntera musialbym robic zapis do RGBE i dopiero na koncu konwertowac
    do RGB : w ten sposob nie tylko komplikuje sobie kod ale takze
    robienie --write-partial-row staje sie klopotliwe. A wiec, na podobnej
    zasadzie jak funkcja RaytraceTo1st zwala problem na kod zewnetrzny,
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
    przynosi kolor SceneBGColor).
    Jezeli RowMadeNotifier <> nil to mozesz obserwowac czesciowo
    wygenerowany obrazek wiêc nale¿y tutaj powiedzieæ jak obrazek jest
    zapisywany : zapisujemy mianowicie wierszami, id±c od do³u.
    Górna czê¶æ obrazka (jeszcze nie wygenerowana) ma zawsze tak± zawarto¶æ
    jak± mia³a w momencie przekazywania jej jako parametr do tej funkcji,
    dlatego je¿eli zamierzasz jako¶ wy¶wietlaæ czê¶ciowo wygenerowany
    obrazek to powiniene¶ zadbaæ o jakie¶ zainicjowanie jego pocz±tkowej
    zawarto¶ci.
  Parametry CamPosition, CamDirection, CamUp naturalnie ustawiaja kamere
    w scenie. Tak jak zwykle, jezeli CamUp i CamDirection nie sa prostopadle
    to poprawiany jest CamUp aby byc prostopadly (a NIE camDirection).
  ViewAngleDegX i ViewAngleDegY to rozpietosc obiektywu w stopniach.
  PixelsMadeNotifier, jezeli <> nil, to bedzie wywolywane po zapisaniu
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

  InitialDepth to ograniczenie raytracera na globokosc drzewa promieni - tak jak
    zad.2 z rgk, tzn. InitialDepth = 0 oznacza tylko promienie prierw,
    1 = promienie 1-krotnie odbite + bezposrednie cienie itd.
  FirstPixel pozwala renderowac obrazki od srodka - normalnie FirstPixel = 0
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
    FirstPixel+2 ... itd. az do Image.Width * Image.Height.

  Podaj FogNode <> nil aby miec odpowiednia mgle, zgodnie ze specyfik. VRMLa 97.
  FogDistanceScaling to skalowanie FogNode z transformacji sceny
  VRMLa w miejscu gdzie byl FogNode --- tak samo jak pole
  TVRMLFlatScene o tej samej naziwe.

  Uzywamy lokalnego modelu oswietlenia zdefiniowanego w funkcjach
  VRML97* w IllumModels, patrz tam po komentarze. Po pewne szczegoly
  patrz tez rayhunter.php.  }
procedure ClassicRayTracerTo1st(const Image: TImage;
  Octree: TVRMLTriangleOctree;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  const PixelsMadeNotifier: TPixelsMadeNotifierFunc; const PixelsMadeNotifierData: Pointer;
  const InitialDepth: Cardinal;
  FogNode: TNodeFog; const FogDistanceScaling: Single;
  const FirstPixel: Cardinal {$ifdef DEFPARS}=0{$endif}); overload;

{ Znaczenie Image, Octree, Cam*, ViewAngle*, RowMade*, SceneBGColor - patrz
    ClassicRayTraceTo1st.
  MinDepth i RRoulContinue razem ustalaja warunki konczenia sciezki.
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
    powodowany przez rosyjska ruletke.
  Primary- i NonPrimary- SamplesCount okreslaja ile sciezek wygenerowac dla
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
    ze dla antialiasingu nie potrzebujemy az tak wielu probek na pixel.
  DirectIllumSamplesCount okresla ile sampli ma byc wyslanych zeby
    w kazdym punkcie sciezki obliczyc bezposrednio direct illumination
    (tzn. kazdy taki sample jest skierowany w strone losowego punktu
    losowego zrodla swiatla). Podaj tutaj 0 aby miec "naiwny path tracing" :
    liczy direct i indirect illumination probujac wygenerowac sciezki ktore
    trafilyby w zrodlo swiatla. Potrzebuje naprawde olbrzymiej ilosci
    [Non]PrimarySamplesCount zeby dac jakiekolwiek rezultaty.
    Podaj tutaj 1 lub wiecej aby miec typowego path tracera.
    Dla wartosci rownej 1 rezultaty sa juz dobre. }
procedure PathTracerTo1st(const Image: TImage;
  Octree: TVRMLTriangleOctree;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  const PixelsMadeNotifier: TPixelsMadeNotifierFunc; const PixelsMadeNotifierData: Pointer;
  const MinDepth: Integer; RRoulContinue: Single;
  const PrimarySamplesCount, NonPrimarySamplesCount, DirectIllumSamplesCount: Cardinal;
  SFCurveClass: TSpaceFillingCurveClass;
  const FirstPixel: Cardinal {$ifdef DEFPARS}=0{$endif}); overload;

const
  { uzywanie innych TSpaceFillingCurveClass nie przyspiesza NIC a NIC
    wiec na razie niech rayhunter i view3dscene uzywaja zawsze
    TSwapScanCurve, on przynajmniej pozwala latwo oddzielic na obrazku
    czesc zrobiona i niezrobiona (i dzieki temu np. view3dscene moze
    szybciej rysowac obrazek w miare generowania) }
  BestRaytrSFC: TSpaceFillingCurveClass = TSwapScanCurve;

implementation

uses SysUtils, Math, KambiUtils, Boxes3d, IllumModels, SphereSampling;

{$I vectormathinlines.inc}

{ RayVector calculations ---------------------------------------- }

{ oblicz promien ugiety powstaly przez uderzenie promienia z kierunku
  NormRayVector (znormalizowany kierunek) (i z materii o wspolczynniku
  zalamania EtaFrom) w powierzchnie o wektorze normalnym (znormalizowanym)
  PlaneNormal. Materia po drugiej stronie powierzchni ma wspolczynnik
  zalamania = EtaTo. }
function TryTransmittedRayVector(var TransmittedRayVector: TVector3Single;
  const NormRayVector: TVector3Single; const PlaneNormal: TVector4Single;
  const EtaFrom, EtaTo: Single)
  :boolean;
{ napisane na podstawie Foleya, str. 627 }
var EtaZalamania, RayIDotNormal, ToBeSqrRooted: Single;
    RayI,
    Normal: TVector3Single; { Normal w strone z ktorej przyszedl RayVector }
begin
 { oblicz Normal w ta strone z ktorej przybyl RayVector (czyli -RayVector i
   Normal musza wskazywac w ta sama strone powierzchni) }
 Normal := PlaneDirNotInDirection(PlaneNormal, NormRayVector);
 RayI := VectorNegate(NormRayVector);

 RayIDotNormal := VectorDotProduct(RayI, Normal);

 { EtaZalamania to stosunek wspolczynnikow zalamania materialow
   ktore wlasnie zmieniamy wchodzac promieniem zalamanym do srodka. }
 EtaZalamania := EtaFrom / EtaTo;

 ToBeSqrRooted := 1 - Sqr(EtaZalamania) * (1 - Sqr(RayIDotNormal));

 result := ToBeSqrRooted >= 0;
 if result then
  TransmittedRayVector := VectorSubtract(
    VectorScale(Normal, EtaZalamania * RayIDotNormal - Sqrt(ToBeSqrRooted)),
    VectorScale(RayI, EtaZalamania) );
end;

{ licz idealnie lustrzany ReflectedRayVector. }
function ReflectedRayVector(const NormRayVector: TVector3Single;
  const PlaneNormal: TVector4Single): TVector3Single;
var Normal, NormNegatedRayVector: TVector3Single;
begin
 { oblicz Normal w ta strone z ktorej przybyl RayVector (czyli -RayVector i
   Normal musza wskazywac w ta sama strone powierzchni) }
 Normal := PlaneDirNotInDirection(PlaneNormal, NormRayVector);
 NormNegatedRayVector := VectorNegate(NormRayVector);

 { liczymy promien jako promien lustrzany do NormNegatedRayVector
   wzgledem Normal. Licz jako 2 * Normal * Dot(Normal, NormNegatedRayVector)
   - NormNegatedRayVector, zupelnie analogicznie do obliczania (14.16)
   w Foley'u (str. 601) }
 result := VectorSubtract(
   VectorScale(Normal, 2*VectorDotProduct(Normal, NormNegatedRayVector)),
   NormNegatedRayVector);
end;

{ ClassicRayTracer -------------------------------------------------------------- }

procedure ClassicRayTracerTo1st(const Image: TImage;
  Octree: TVRMLTriangleOctree;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  const PixelsMadeNotifier: TPixelsMadeNotifierFunc; const PixelsMadeNotifierData: Pointer;
  const InitialDepth: Cardinal;
  FogNode: TNodeFog; const FogDistanceScaling: Single;
  const FirstPixel: Cardinal);

var FogType: Integer;

  function Trace(const Ray0, RayVector: TVector3Single; const Depth: Cardinal;
    const OctreeItemToIgnore: integer; IgnoreMarginAtStart: boolean): TVector3Single;
  { sledzi promien z zadana glebokoscia. Zwraca false jesli promien w nic
    nie trafia, wpp. zwraca true i ustawia Color na wyliczony Color. }
  var Intersection: TVector3Single;
      IntersectNodeIndex: integer;
      IntersectNode: POctreeItem;
      MaterialNode: TNodeMaterial; { = IntersectNode.State.LastNodes.Material }

    procedure ModifyColorByTransmittedRay;
    var TransmittedColor, TransmittedRayVec: TVector3Single;
        EtaFrom, EtaTo: Single;
        MatTransparency: Single;
    const ETA_CONST = 1.3;
    begin
     { rob promien zalamany jesli Transparency > 0 i nie ma calkowitego odbicia wewn.}
     MatTransparency := MaterialNode.Transparency(IntersectNode.MatNum);
     if MatTransparency > 0 then
     begin
      { Powinnismy to brac z informacji zapisanych w modelu - ale nie mamy
        tego. Wiec tymczasowo udaje tutaj ze zawsze przy zalamywaniu
        mamy Eta = ETA_CONST, potem 1/ETA_CONST (w nastepnym rekurencyjnym
        zalamaniu), potem znowu ETA_CONST itd. }
      if Odd(InitialDepth-Depth) then
       begin EtaFrom := 1; EtaTo := ETA_CONST end else
       begin EtaFrom := ETA_CONST; EtaTo := 1 end;

      if TryTransmittedRayVector(TransmittedRayVec, Normalized(RayVector),
        IntersectNode.TriangleNormPlane, EtaFrom, EtaTo) then
      begin
       VectorScaleTo1st(result, 1-MatTransparency);
       TransmittedColor := Trace(Intersection, TransmittedRayVec, Depth-1, IntersectNodeIndex, true);
       VectorScaleTo1st(TransmittedColor, MatTransparency);
       VectorAddTo1st(result, TransmittedColor);
      end;
     end;
    end;

    procedure ModifyColorByReflectedRay;
    var ReflRayVector, ReflColor: TVector3Single;
        MatMirror: Single;
    begin
     { rob promien odbity }
     MatMirror := MaterialNode.Mirror(IntersectNode.MatNum);
     if MatMirror > 0 then
     begin
      ReflRayVector := ReflectedRayVector(Normalized(RayVector), IntersectNode.TriangleNormPlane);
      VectorScaleTo1st(result, 1-MatMirror);
      ReflColor := Trace(Intersection, ReflRayVector, Depth-1, IntersectNodeIndex, true);
      VectorScaleTo1st(ReflColor, MatMirror);
      VectorAddTo1st(result, ReflColor);
     end;
    end;

    function LightNotBlocked(const Light: TActiveLight): boolean;
    begin
     { czy swiatlo dociera do powierzchni ? Zwracam uwage ze
         uwzgledniamy fakt ze obiekty polprzezroczyste nie rzucaja cienia.
         (chociaz tak naprawde powinnismy robic nawet wiecej - przeciez
         obiekty polprzezroczyste rzucaja cien, po prostu nie calkowity -
         one tylko skaluja swiatlo przychodzace).
       Uwzgledniamy tez fakt ze swiatlo musi byc skierowane w ta strone
         plaszczyzny IntersectNode co -RayVector - gdyby nie to znaczy
         ze swiatlo oswietla IntersectNode ale Z DRUGIEJ STRONY.
         A tego przeciez nie chcemy liczyc. }
     result := ActiveLightNotBlocked(Octree, Light,
       Intersection, PVector3Single(@IntersectNode.TriangleNormPlane)^,
       VectorNegate(RayVector), IntersectNodeIndex, true);
    end;

  var i: integer;
  begin
   IntersectNodeIndex := Octree.RayCollision(Intersection, Ray0, RayVector, true,
     OctreeItemToIgnore, IgnoreMarginAtStart, nil);
   if IntersectNodeIndex = NoItemIndex then Exit(SceneBGColor);

   IntersectNode := Octree.OctreeItems.Pointers[IntersectNodeIndex];
   MaterialNode := IntersectNode.State.LastNodes.Material;
   result := VRML97Emission(IntersectNode^, InitialDepth <> 0);
   with IntersectNode^ do
   begin
    if Depth > 0 then
    begin
     for i := 0 to State.ActiveLights.Count-1 do
      if LightNotBlocked(State.ActiveLights.Items[i]) then
       VectorAddTo1st(result,
         VRML97LightContribution(State.ActiveLights.Items[i],
           Intersection, IntersectNode^, CamPosition));

     { licz rekurencyjnie kolory z promieni odbitych i zalamanych
       (kolejnosc wywolywania (najpierw Reflected czy najpierw Transmitted)
       MA znaczenie - te procedury niekoniecznie dodaja cos do Color,
       one moga tez np. skalowac Color) }
     ModifyColorByReflectedRay;
     ModifyColorByTransmittedRay;
    end;
   end;

   { tak jest, VRMLFog musi byc robione na kazdym poziomie rekurencji, nie tylko
     dla promieni pierwotnych. Bo wyobrazmy sobie np. sytuacje gdy patrzymy
     przez szybe (ktora jest bardzo blisko i mgla jej nie zakrywa) na cos
     co jest daleko i jest zamglone. Widac ze rekurencyjne wywolania Trace
     tez powinny przynosic kolory z uwzgledniona mgla, nie ? }
   if FogType <> -1 then
    result := VRML97Fog(result, PointsDistance(CamPosition, Intersection),
      FogNode, FogDistanceScaling, FogType);
  end;

var RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  begin
   Image.SetColorRGB(x, y,
     Trace(CamPosition, RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height),
       InitialDepth, NoItemIndex, false));
  end;

var PixCoord: TVector2Cardinal;
    SFCurve: TSpaceFillingCurve;
begin
 FogType := VRML97FogType(FogNode);

 RaysWindow := nil;
 SFCurve := nil;
 try
  RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp, ViewAngleDegX, ViewAngleDegY);

  { uzywanie jakichkolwiek space filling curves nie ma teraz znaczenia dla
    classic ray tracera, bo classic ray tracer nie uzywa shadow cache ani
    zadnych innych tego typu technik. Ale w przyszlosci bedzie, to raz;
    dwa to ze w ten sposob moge wygodniej realizowac parametr FirstPixel
    i PixelsMadeNotifier. }
  SFCurve := TSwapScanCurve.Create(Image.Width, Image.Height);
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
  RaysWindow.Free;
  SFCurve.Free;
 end;
end;

{ PathTracer -------------------------------------------------------------- }

{ Elementy implementacji path tracera :
 - Co znaczy parametr TraceOnlyIndirect dla Trace() ?
    O ile tylko (DirectIllumSamplesCount <> 0) to rownanie renderingu rozbijamy
    w kazdym punkcie na
      L_out = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    przy czym
      L_indirect = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    i L_indirect musi trafiac w cos co NIE jest zrodlem swiatla,
    z czego wynika ze L_emission zawsze bedzie tu = (0, 0, 0) wiec mamy
      L_indirect = IntegralOverHemisphere( L_direct + L_indirect ),

   Wynika z tego ze robiac rekurencyjne wywolanie Trace aby policzyc
   L_indirect NIE CHCEMY trafic w zrodlo swiatla bo wtedy policzylibysmy
   L_direct dwukrotnie.

   Innymi slowy wysylajac promienie pierwotne dajemy TraceOnIndirect = false
   bo liczymy L_out (co jest zreszta bardzo rozsadne nawet jesli nie bedziemy
   patrzec na wzor; gdybysmy tak nie robili to ponizsze zabezpieczenie
   spowodowaloby omylkowo ze zrodla swiatla sa zawsze niewidoczne,
   tzn. zawsze maja czarny kolor, a przeciec zrodla swiatla zawsze maja
   wlasnie inny (choc troche jasniejszy) niz czarny kolor).

   Podczas gdy wysylajac promienie rekurencyjne, tzn. wywolujac Trace() z
   Trace(), chcemy dawac TraceOnIndirect = true, no, chyba ze
   DirectIllumSamplesCount = 0, wiec w sumie dajemy
   TraceOnIndirect = DirectIllumSamplesCount <> 0.

   Testy wykazuja ze rzeczywiscie na obrazie jest duzo mniej "noise" gdy
   na poczatku Trace() uwzgledniamy ten parametr i nie pozwalamy sobie
   policzyc omylkowo L_direct a wiec trafic przez przypadek w zrodlo
   swiatla. (Trace zwraca wtedy ZeroVectyor3Single i konczy sciezke
   (tzn. Trace ktore trafilo w zrodlo swiatla nie pojdzie juz dalej)).

 - Zwracam uwage ze MinDepth i Depth (parametr Trace()) sa Integerami.
   MinDepth dlatego ze jest poczatkowa wartoscia Depth.
   Depth dlatego ze moze byc ujemne,
   dla kazdego wywolania rekurencyjnego Trace przekazujemy Depth = Depth-1.
   Gdy Depth <= 0 to o tym czy wykonamy kolejne wywolanie rekurencyjne decyduje
   rosyjska ruletka.
}

procedure PathTracerTo1st(const Image: TImage;
  Octree: TVRMLTriangleOctree;
  const CamPosition, CamDirection, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  const PixelsMadeNotifier: TPixelsMadeNotifierFunc; const PixelsMadeNotifierData: Pointer;
  const MinDepth: Integer; RRoulContinue: Single;
  const PrimarySamplesCount, NonPrimarySamplesCount, DirectIllumSamplesCount: Cardinal;
  SFCurveClass: TSpaceFillingCurveClass;
  const FirstPixel: Cardinal);

var
  { w LightIndices mamy indeksy do Octree.OctreeItems[] wskazujace na
    elementy ktore maja emission kolor > 0, czyli na elementy ktore sa
    swiatlami }
  LightsIndices: TDynIntegerArray;
  {$ifdef PATHTR_USES_SHADOW_CACHE}
  { dla kazdego swiatla w LightsIndices[i], ShadowCache[i] okresla
    indeks (do Octree.OctreeItems[]) ktory wskazuje na ostatni obiekt ktory
    blokowal to zrodlo swiatla (albo na NoItemIndex jesli jeszcze nie znalezlismy
    takiego obiektu w czasie dzialania PathTracera).
    Ten indeks jest uaktualniany i jednoczesnie wykorzystywany w IsLightShadowed
    aby przyspieszyc dzialanie. Idea "shadow cache" - z RGK,
    wykrystalizowana dzieki "Graphic Gems II". }
  ShadowCache: TDynIntegerArray;
  {$endif}

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

  function IsLightSourceIndex(ItemIndex: Integer): boolean;
  begin
   result := VectorLenSqr(Octree.OctreeItems[ItemIndex].State.LastNodes.Material.
     EmissiveColor3Single(Octree.OctreeItems[ItemIndex].MatNum))
      > Sqr(SingleEqualityEpsilon);
  end;

  function IsLightShadowed(const ItemIndex: Integer; const ItemPoint: TVector3Single;
    const LightSourceIndiceIndex: Integer; LightSourcePoint: TVector3Single): boolean;
  { ta funkcja liczy shadow ray (a w zasadzie segment). Zwraca true jezeli
    pomiedzy punktem ItemPoint a LightSourcePoint jest jakis element
    o transparency = 1. Wpp. zwraca false.
    LightSourceIndiceIndex to indeks to tablicy LightsIndices[].
    ItemIndex to indeks do, jak zwykle, Octree.OctreeItems[]. }
  { TODO: transparent objects should scale light color instead of just
    letting it pass }
  var OctreeIgnorer: TOctreeIgnore_Transparent_And_OneItem;
      ShadowerIndex: Integer;
  {$ifdef PATHTR_USES_SHADOW_CACHE}
      CachedShadowerIndex: Integer;
      CachedShadower: POctreeItem;
  {$endif}
  begin
   {$ifdef PATHTR_USES_SHADOW_CACHE}
   { sprobuj wziac wynik z ShadowCache }
   CachedShadowerIndex := ShadowCache.Items[LightSourceIndiceIndex];
   if (CachedShadowerIndex <> NoItemIndex) and
      (CachedShadowerIndex <> ItemIndex) then
   begin
    CachedShadower := Octree.OctreeItems.Pointers[CachedShadowerIndex];
    Inc(Octree.DirectCollisionTestsCounter);
    if IsTriangleSegmentCollision(CachedShadower.Triangle,
      CachedShadower.TriangleNormPlane, ItemPoint, LightSourcePoint) then
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
   OctreeIgnorer := TOctreeIgnore_Transparent_And_OneItem.Create(
     LightsIndices.Items[LightSourceIndiceIndex]);
   try
    ShadowerIndex := Octree.SegmentCollision(ItemPoint, LightSourcePoint, false,
      ItemIndex, true, OctreeIgnorer.IgnoreItem);
    result := ShadowerIndex <> NoItemIndex;
    {$ifdef PATHTR_USES_SHADOW_CACHE}
    ShadowCache.Items[LightSourceIndiceIndex] := ShadowerIndex;
    {$endif}
   finally OctreeIgnorer.Free end;
  end;

  function Trace(const Ray0, RayVector: TVector3Single;
    const Depth: Integer; const OctreeItemToIgnore: integer;
    const IgnoreMarginAtStart: boolean; const TraceOnlyIndirect: boolean)
    :TVector3Single;
  { sledzi promien z zadana glebokoscia. Zwraca Black (0, 0, 0) jesli
    promien w nic nie trafia, wpp. zwraca wyliczony kolor. }
  var Intersection: TVector3Single;
      IntersectNodeIndex: integer;
      IntersectNode: POctreeItem;
      MaterialNode: TNodeMaterial; { = IntersectNode.State.LastNodes.Material }
      IntersectNormalInRay0Dir: TVector3Single;

    function TraceNonEmissivePart: TVector3Single;

      function TryEvaluateTransmittedSpecularRayVector(var TracedDir: TVector3Single;
        var PdfValue: Single): boolean;
      var TransmittedRayVector: TVector3Single;
          EtaFrom, EtaTo: Single;
      const ETA_CONST = 1.3; { TODO: tu tez uzywam ETA_CONST, jak w Classic }
      begin
       if Odd(MinDepth-Depth) then
        begin EtaFrom := 1; EtaTo := ETA_CONST end else
        begin EtaFrom := ETA_CONST; EtaTo := 1 end;

       result := TryTransmittedRayVector(TransmittedRayVector, Normalized(RayVector),
         IntersectNode.TriangleNormPlane, EtaFrom, EtaTo);
       if result then
        TracedDir := PhiThetaToXYZ(
          RandomUnitHemispherePointDensityCosThetaExp(
            Round(MaterialNode.TransSpecularExp(IntersectNode.MatNum)),
            PdfValue),
          TransmittedRayVector);
      end;

      function DirectIllumination: TVector3Single;
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
      var LightSource: POctreeItem;
          LightSourceIndiceIndex: Integer; { indeks do LightIndices[] }
          LightSourceIndex: Integer; { indeks do Octree.OctreeItems[] }
          SampleLightPoint: TVector3Single;
          DirectColor, LightDirNorm, NegatedLightDirNorm: TVector3Single;
          i: integer;
      begin
       result := ZeroVector3Single;

       { trzeba ustrzec sie tu przed LightsIndices.Count = 0 (zeby moc pozniej
         spokojnie robic Random(LightsIndices.Count) i przed
         DirectIllumSamplesCount = 0 (zeby moc pozniej spokojnie podzielic przez
         DirectIllumSamplesCount). }
       if (LightsIndices.Count = 0) or (DirectIllumSamplesCount = 0) then Exit;

       for i := 0 to DirectIllumSamplesCount-1 do
       begin
        { evaluate LightSourceIndiceIndex, LightSourceIndex, LightSource }
        LightSourceIndiceIndex := Random(LightsIndices.Count);
        LightSourceIndex := LightsIndices.Items[LightSourceIndiceIndex];
        if LightSourceIndex = IntersectNodeIndex then Continue;
        LightSource := Octree.OctreeItems.Pointers[LightSourceIndex];

        { evaluate SampleLightPoint. Lepiej pozniej sprawdz ze SampleLightPoint jest
          rozny od Intersection (poniewaz SampleLightPoint jest losowy to na
          nieprawidlowo skonstruowanym modelu wszystko moze sie zdarzyc...)  }
        SampleLightPoint := SampleTrianglePoint(LightSource.Triangle);
        if VectorsEqual(SampleLightPoint, Intersection) then Continue;

        { evaluate LigtDirNorm (nieznormalizowane).
          Jezeli LigtDirNorm wychodzi z innej strony
          IntersectionNode.TriangleNormPlane niz IntersectNormalInRay0Dir
          to znaczy ze swiatlo jest po przeciwnej stronie plane - wiec
          swiatlo nie oswietla naszego pixela. }
        LightDirNorm := VectorSubtract(SampleLightPoint, Intersection);
        if not VectorsSamePlaneDirections(LightDirNorm, IntersectNormalInRay0Dir,
          IntersectNode.TriangleNormPlane) then Continue;

        { sprawdz IsLightShadowed, czyli zrob shadow ray }
        if IsLightShadowed(IntersectNodeIndex, Intersection,
          LightSourceIndiceIndex, SampleLightPoint) then Continue;

        { evaluate DirectColor = kolor emission swiatla }
        DirectColor := LightSource.State.LastNodes.Material.EmissiveColor3Single(LightSource.MatNum);

        { wymnoz przez naszego "niby-BRDFa" czyli po prostu przez kolor Diffuse
          materialu }
        VectorMultEachPosTo1st(DirectColor,
          MaterialNode.DiffuseColor3Single(IntersectNode.MatNum));

        { evaluate LightDirNorm (znormalizowane), NegatedLightDirNorm }
        NormalizeTo1st(LightDirNorm);
        NegatedLightDirNorm := VectorNegate(LightDirNorm);

        { Wymnoz DirectColor
          1) przez GeometryFunction czyli
               cos(LightDirNorm, IntersectNormalInRay0Dir)
                 * cos(-LightDirNorm, LightSource.TriangleNormal) /
                 PointsDistanceSqr(SampleLightPoint, Intersection).
             Cosinusy naturalnie licz uzywajac VectorDotProduct.
          2) przez TriangleArea

          Mozna zauwazyc ze czlon
            TriangleArea *
            cos(-LightDirNorm, LightSource.TriangleNormal) /
              PointsDistanceSqr(SampleLightPoint, Intersection)
          liczy po prostu solid angle swiatla with respect to Intersection
          (no, mowiac scisle pewne bardzo dobre przyblizenie tego solid angle).

          Moze byc tutaj pouczajace zobaczyc jak to dziala gdy usuniemy mnozenie
            przez cos(-LightDirNorm, LightSource.TriangleNormal)
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
        VectorScaleTo1st(DirectColor,
          VectorDotProduct(LightDirNorm, IntersectNormalInRay0Dir) *
          VectorDotProduct(NegatedLightDirNorm,
            PlaneDirInDirection(LightSource.TriangleNormPlane, NegatedLightDirNorm)) *
          LightSource.TriangleArea /
          PointsDistanceSqr(SampleLightPoint, Intersection)
        );

        { result += DirectColor }
        VectorAddTo1st(result, DirectColor);
       end;

       { dopiero tu przemnoz przez 1/LightEmissionArea.
         Podziel tez przez ilosc probek i pomnoz przez ilosc swiatel -
         - w rezultacie spraw zeby wynik przyblizal sume wkladu direct illumination
         wszystkich swiatel. }
       VectorScaleTo1st(result, LightsIndices.Count/
         (LightEmissionArea*DirectIllumSamplesCount) );
      end;

    type
      { kolory Transmittive/Reflective Diffuse/Specular }
      TColorKind = (ckRS, ckRD, ckTS, ckTD);
    var Colors: array[TColorKind]of TVector3Single;
        Weights: array[TColorKind]of Single;
        WeightsSum: Single;
        RandomCK: Single;
        PdfValue: Single;
        TracedCol, TracedDir: TVector3Single;
        ck: TColorKind;
    begin
     result := ZeroVector3Single;
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

      { evaluate Colors[] }
      Colors[ckRS] := MaterialNode.ReflSpecular (IntersectNode.MatNum);
      Colors[ckRD] := MaterialNode.ReflDiffuse  (IntersectNode.MatNum);
      Colors[ckTS] := MaterialNode.TransSpecular(IntersectNode.MatNum);
      Colors[ckTD] := MaterialNode.TransDiffuse (IntersectNode.MatNum);

      { evaluate Weights[] and WeightSum }
      WeightsSum := 0;
      for ck := Low(ck) to High(ck) do
      begin
       Weights[ck] := Colors[ck, 0] + Colors[ck, 1] + Colors[ck, 2];
       WeightsSum += Weights[ck];
      end;

      { wylosuj jedno z ck : wylosuj zmienna RandomCK z przedzialu 0..WeightsSum
       a potem zbadaj do ktorego z przedzialow Weights[] wpada. Evaluate ck. }
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
       { evaluate IntersectNormalInRay0Dir - Normal at intersection in direction Ray0 }
       IntersectNormalInRay0Dir := PlaneDirNotInDirection(
        IntersectNode.TriangleNormPlane, RayVector);

       { evaluate TracedDir i PdfValue samplujac odpowiednio polsfere
        (na podstawie ck). W przypadku TS moze wystapic calk. odbicie wewn.
        i wtedy konczymy sciezke. }
       case ck of
        ckTD: TracedDir := PhiThetaToXYZ(
               RandomUnitHemispherePointDensityCosTheta(PdfValue),
               VectorNegate(IntersectNormalInRay0Dir));
        ckTS: if not TryEvaluateTransmittedSpecularRayVector(TracedDir, PdfValue) then Exit;
        ckRD: TracedDir := PhiThetaToXYZ(
               RandomUnitHemispherePointDensityCosTheta(PdfValue),
               IntersectNormalInRay0Dir);
        ckRS: TracedDir := PhiThetaToXYZ(
                RandomUnitHemispherePointDensityCosThetaExp(
                  Round(MaterialNode.ReflSpecularExp(IntersectNode.MatNum)),
                  PdfValue),
                ReflectedRayVector(Normalized(RayVector),
                  IntersectNode.TriangleNormPlane));
       end;

       { wywolaj rekurencyjnie Trace(), a wiec idz sciezka dalej }
       TracedCol := Trace(Intersection, TracedDir, Depth-1, IntersectNodeIndex, true,
        DirectIllumSamplesCount <> 0);

       { przetworz TracedCol : wymnoz przez Colors[ck], podziel przez szanse
         jego wyboru sposrod czterech Colors[], czyli przez
         Weights[ck]/WeightsSum (bo to w koncu jest importance sampling)
         (czyli pomnoz przez WeightsSum/Weights[ck], wiemy ze mianownik jest
         > SingleEqualityEpsilon, sprawdzilismy to juz wczesniej). }
       VectorMultEachPosTo1st(TracedCol, Colors[ck]);
       VectorScaleTo1st(TracedCol, WeightsSum/Weights[ck]);

       VectorAddTo1st(Result, TracedCol);
      end;

      { dodaj DirectIllumination }
      VectorAddTo1st(Result, DirectIllumination);

      { Jezeli weszlismy tu dzieki rosyjskiej ruletce (a wiec jezeli Depth <= 0)
        to skaluj Result zeby zapisany tu estymator byl unbiased. }
      if Depth <= 0 then VectorScaleTo1st(Result, 1/RRoulContinue);
     end;
    end;

  var i: Integer;
      NonEmissiveColor: TVector3Single;
  begin
   IntersectNodeIndex := Octree.RayCollision(Intersection, Ray0, RayVector, true,
     OctreeItemToIgnore, IgnoreMarginAtStart, nil);
   if IntersectNodeIndex = NoItemIndex then Exit(SceneBGColor);

   if TraceOnlyIndirect and IsLightSourceIndex(IntersectNodeIndex) then
    Exit(ZeroVector3Single);

   IntersectNode := Octree.OctreeItems.Pointers[IntersectNodeIndex];
   MaterialNode := IntersectNode.State.LastNodes.Material;
   { de facto jezeli TraceOnlyIndirect to ponizsza linijka na pewno dodaje
     do result ZeroVector3Single. Ale nie widze w tej chwili jak z tego wyciagnac
     jakas specjalna optymalizacje. }
   result := MaterialNode.EmissiveColor3Single(IntersectNode.MatNum);

   { jezeli MinDepth = Depth to znaczy ze nasz Trace zwraca kolor dla primary ray.
     Wiec rozgaleziamy sie tutaj na NonPrimarySamplesCount, czyli dzialamy
       jakbysmy byly stochastycznym ray tracerem ktory rozgalezia sie
       na wiele promieni w punkcie rekursji.
     Wpp. idziemy sciezka czyli dzialamy jakbysmy byly path tracerem czyli
       nie rozgaleziamy sie na wiele promieni. }
   if MinDepth = Depth then
   begin
    NonEmissiveColor := ZeroVector3Single;
    for i := 0 to NonPrimarySamplesCount-1 do
     VectorAddTo1st(NonEmissiveColor, TraceNonEmissivePart);
    VectorScaleTo1st(NonEmissiveColor, 1/NonPrimarySamplesCount);
    VectorAddTo1st(result, NonEmissiveColor);
   end else
    VectorAddTo1st(result, TraceNonEmissivePart);
  end;

var RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  var PixColor, PrimaryRayVector: TVector3Single;
      SampleNum: Integer;
  begin
   { generuj pixel x, y. evaluate PixColor }
   if PrimarySamplesCount = 1 then
   begin
    { gdy PrimarySamplesCount = 1 to wysylamy jeden promien pierwotny
      i ten promien NIE jest losowany na rzutni w zakresie pixela
      x, y ale przechodzi dokladnie przez srodek pixela x, y. }
    PrimaryRayVector := RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height);
    PixColor := Trace(CamPosition, PrimaryRayVector, MinDepth,
      NoItemIndex, false, false);
   end else
   begin
    PixColor := ZeroVector3Single;
    for SampleNum := 0 to PrimarySamplesCount-1 do
    begin
     PrimaryRayVector := RaysWindow.PrimaryRay(x + Random - 0.5, y + Random - 0.5,
       Image.Width, Image.Height);
     VectorAddTo1st(PixColor, Trace(CamPosition, PrimaryRayVector, MinDepth,
       NoItemIndex, false, false) );
    end;
    VectorScaleTo1st(PixColor, 1/PrimarySamplesCount);
   end;

   { zapisz PixColor do Image }
   Image.SetColorRGB(x, y, PixColor);
  end;

var PixCoord: TVector2Cardinal;
    i: integer;
    SFCurve: TSpaceFillingCurve;
begin
 { check parameters (path tracing i tak trwa bardzo dlugo wiec mozemy sobie
   pozwolic zeby na poczatku tej procedury wykonac kilka testow, nawet gdy
   kompilujemy sie w wersji RELEASE) }
 Check(PrimarySamplesCount > 0, 'PrimarySamplesCount for PathTracer must be greater than 0');
 Check(NonPrimarySamplesCount > 0, 'NonPrimarySamplesCount for PathTracer must be greater than 0');
 Clamp(RRoulContinue, Single(0.0), Single(1.0));

 { zainicjuj na nil'e, zeby moc napisac proste try..finally }
 LightsIndices := nil;
 {$ifdef PATHTR_USES_SHADOW_CACHE} ShadowCache := nil; {$endif}
 RaysWindow := nil;
 SFCurve := nil;
 try
  { evaluate LightIndices }
  LightsIndices := TDynIntegerArray.Create;
  LightsIndices.AllowedCapacityOverflow := Octree.OctreeItems.Count div 4;
  for i := 0 to Octree.OctreeItems.Count-1 do
   if IsLightSourceIndex(i) then LightsIndices.AppendItem(i);
  LightsIndices.AllowedCapacityOverflow := 4;

  {$ifdef PATHTR_USES_SHADOW_CACHE}
  { evaluate ShadowCache }
  ShadowCache := TDynIntegerArray.Create;
  ShadowCache.SetLength(LightsIndices.Length);
  ShadowCache.SetAll(NoItemIndex);
  {$endif}

  { evaluate RaysWindow }
  RaysWindow := TRaysWindow.Create(CamPosition, CamDirection, CamUp, ViewAngleDegX, ViewAngleDegY);

  { evaluate SFCurve }
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
  LightsIndices.Free;
 end;
end;

end.
