{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Rendering Background --- sky/ground around the camera.)

  Niebo to
  @unorderedList(
    @itemSpacing Compact
    @item szescian pokryty teksturami
    @item kawalek sfery (ground) z interpolowanymi kolorami
    @item wszystko to zawarte w calej sferze (sky) z interpolowanymi kolorami.
  )

  Idea jest taka zeby gracz zawsze byl polozony dokladnie w srodku takiego
  Background (tzn. koncepcyjnie mozna powiedziec ze ten szescian i sfery
  sa nieskonczone) (chociaz gracz moze sie oczywiscie obracac aby ogladac
  cale to Background).

  Historia: Ten modul zaczal istniec jeszcze zanim zabralem sie za VRMLa jako
  czesc "szklanych lasow", potem zostal przystosowany do uzytku ogolnego
  i przeniesiony do units.openGL/ gdy robilem "malfunction" a nastepnie
  (sierpien 2003) przystosowalem go aby nadawal sie do renderowania takiego
  Background zgodnie z semantyka odpowiedniego wezla VRMLa 97.

  Ten modul znajduje bezposrednie zastosowanie przy realizowaniu odpowiedniego
  wezla VRMLa 97 (ktory ja obsluguje takze w VRMLu 1.0). Tym niemniej ten
  modul jest w units.openGL/ a wiec NIE zalezy od zadnych modulow zwiazanych
  z VRMLem. W ten sposob mozna tego modulu uzywac takze w programach uzywajacych
  OpenGLa ktore nie laduja swojego nieba z VRMLa (i byc moze w ogole nie maja
  nic wspolnego z VRMLem).

  Uzywane w tym module struktury TBackgroundImages musza miec wszystkie obrazki
  (ktore nie sa ImageNone) o Kind wsrod ImageGLFormats.

  Malenki kawalek matematyki: Dana sfera o promieniu SphereRadius, dany
  szescian o boku dlugosci CubeSize. Srodek sfery = srodek szescianu,
  jak wyrazic CubeSize w zaleznosci od SphereRadius i w druga strone
  aby szescian byl dokladnie wpisany w sfere ?

@preformatted(
  Przekatna szescianu (ale nie boku szescianu) = 2*promien sfery,
    czyli bok szescianu * Sqrt2 = przekatna boku i
    Sqrt(Sqr(przekatna boku) + Sqr(bok)) = przekatna szescianu
    czyli
      Sqrt(Sqr(CubeSize * Sqrt2) + Sqr(CubeSize)) = 2 * SkySphereRadius
      Sqr(CubeSize * Sqrt2) + Sqr(CubeSize) = Sqr(2 * SkySphereRadius)
      Sqr(CubeSize) * Sqr(Sqrt2) + Sqr(CubeSize) = Sqr(2 * SkySphereRadius)
      3*Sqr(CubeSize) = Sqr(2 * SkySphereRadius)
      CubeSize = Sqrt( Sqr(2 * SkySphereRadius)/3 )
               = 2 * SkySphereRadius / Sqrt(3)
    funkcja w druga strone : SkySphereRadius = Sqrt(3) * CubeSize / 2

  Dlatego wlasnie w module ponizej sa stale
    SphereRadiusToCubeSize = 2/Sqrt(3)
    CubeSizeToSpeherRadius = Sqrt(3)/2
)
}

unit BackgroundGL;

{$I kambiconf.inc}

interface

uses VectorMath, SysUtils, GL, GLU, GLExt, KambiGLUtils, KambiUtils, Images,
  BackgroundBase;

const
  { }
  SphereRadiusToCubeSize = 2/Sqrt(3);
  CubeSizeToSphereRadius = Sqrt(3)/2;

{ TBackgroundGL ------------------------------------------------------------ }

type
  TBackgroundGL = class
  private
    szescianNieba_list: TGLuint;
    nieboTex: packed array[TBackgroundSide]of TGLuint;
  public
    { rysuje wokolo punktu na ktorym stoimy niebo. Niebo jest zorientowane
      zgodnie ze standardem VRMLa - patrz specyfikacja VRMLa 97 po znaczenie
      wszystkich zmiennych podawanych jako parametr do konstruktora
      (poza SkySphereRadius).

      Potrzebuje jednego wolnego miejsca na stosie attribow. Samo ustawia
      sobie enabled odpowiednich rzeczy. Aktualny kolor nie ma znaczenia
      (nie bedzie uzywany); aktualna macierz pomnozona przez Transform
      moze zawierac tylko obroty - zadnego przesuniecia ani skalowania.
      W ten sposob renderowanie wokol punktu 0, 0, 0 powoduje renderowanie
      wokolo widoku usera (a wiec np. zapewne chcesz robic Render przed
      swoim gluLookAt).

      Zakladamy ze user stoi w srodku tej sfery, wiec mozna wykonac ew. backface
      culling scian na zewnatrz sceny (to czy rzeczywiscie bedzie to robine
      zalezy od implementacji). Bedziemy rysowac z wylaczonym DEPTH_TEST wiec
      zakryjemy wszystko co jest narysowane dotychczas na ekranie - Sky.Render
      to powinna byc pierwsza instrukcja piszaca do color-buffera w OnDraw
      (nawet glClear(GL_COLOR_BUFFER) nie ma sensu, de facto jest marnowaniem
      czasu).

      Niebo to sfera o promieniu SkySphereRadius. Sfera ground i szescian
      6 tekstur nieba beda zrobione tak zeby byc jak najblizej sfery sky -
      sfera ground bedzie miala ten sam promien co sfera sky (rysujemy bez
      DEPTH_TEST wiec nie bedzie walki w z-buforze pomiedzy tymi elementami)
      a szescian bedzie wyznaczony tak zeby rogami dotykac tej sfery
      (tzn. CubeSize = SkySphereRadius * SphereRadiusToCubeSize).

      Nie umieszczaj Render na display liscie - juz tutaj w srodku
      realizujemy sobie renderowanie przez display liste. }
    procedure Render;

    { Calculate (or just confirm that Proposed value is still OK)
      the sky sphere radius that fits nicely in your projection near/far.

      Zarowno szescian jak i sfera musza byc pomiedzy near i far perpsektywy,
      tzn. musi byc near < CubeSize/2, far > SkySphereRadius.
      Albo, patrzac na to z drugiej strony, warunek jaki musi spelniac
      SkyCubeSize to

        near * 2 < SkyCubeSize < far * SphereRadiusToCubeSize

      Tu uwaga - ta nierownosc formalnie potwierdza fakt ze mozna
      dobrac near i far tak bliskie siebie ze zadne SkyCubeSize nie
      bedzie mozliwe (tzn. near < far nie gwarantuje ze istnieje
      SkyCubeSize spelniajace ta nierownosc bo

        2 > SphereRadiusToCubeSize).

      NearFarToSkySphereRadius wyliczy near i far na podstawie wartosci
      jakie ustawiles projection (jako
      srednia ( near * 2,  far * SphereRadiusToCubeSize )).
      (background nie jest brane pod uwage w depth-tescie, zalezy nam tylko
      zeby bylo pomiedzy near a far i zeby nie bylo clipped).

      Jeszcze jedno : mialem pomysl aby Render samo ustawialo jakies proste
      Projection Matrix ktore byloby dobre dla niego.
      Wtedy nie musielibysmy podawac SkyCubeSize. Ale : po pierwsze,
      trzeba byloby z kolei podawac aspect i fovy wiec wyszloby na
      to samo. Po drugie, wymagaloby to uzycia jednego miejsca na
      stosie matryc projection, a ten jest bardzo plytki. Po trzecie,
      tak jak jest jest szybciej - nie musimy ustawiac zadnej macierzy.

      This method first check is Proposed a good result value (it satisfies
      the conditions, with some safety margin). If yes, then we return
      exactly the Proposed value. Otherwise, we calculate new value
      as an average in our range.
      This way, if you already had sky sphere radius calculated
      (and prepared some OpenGL resources for it),
      and projection near/far changes very slightly
      (e.g. because bounding box slightly changed), then you don't have
      to recreate background --- if the old sky sphere radius is still OK,
      then the old background resources are still OK.

      Just pass Proposed = 0 (or anything else that is always outside
      the range) if you don't need this feature. }
    class function NearFarToSkySphereRadius(const zNear, zFar: Single;
      const Proposed: Single = 0): Single;

    { Laduje nebo. Parametry maja znaczenie jak w specyfikacji VRMLa i musza
      spelniac odpowiednie, wyszczegolnione tam zalozenia (np. SkyColorCount > 0,
      GroundColorCount > GroundAngleCount itp.). Obrazki musza byc rodzaju
      ImageGLFormats. Mozesz tez podac ImageNone jako dowolne sposrod Images.
      Zarowno konstruktor jak i destruktor wymagaja aktywnego gl context. }
    constructor Create(const Transform: TMatrix4Single;
      GroundAngle: PArray_Single; GroundAngleCount: Integer;
      GroundColor: PArray_Vector3Single; GroundColorCount: Integer;
      const Imgs: TBackgroundImages;
      SkyAngle: PArray_Single; SkyAngleCount: Integer;
      SkyColor: PArray_Vector3Single; SkyColorCount: Integer;
      SkySphereRadius: Single);
    destructor Destroy; override;
  end;

{ TSkyCube (obsolete) ------------------------------------------------------- }

type
  { bedzie rysowal niebo wedlug naszych standardow -
    bottom jest w -Z, top w +Z. Ma takze konstruktor ktory bedzie automatycznie
    sobie ladowal nieba przy pomocy BackgroundImagesLoadFromOldNamePattern. }
  TSkyCube = class(TBackgroundGL)
  public
    constructor Create(const SkyNamePattern: string; zNear, zFar: Single); overload;
    constructor Create(const Imgs: TBackgroundImages; zNear, zFar: Single); overload;
  end;

implementation

uses DataErrors, GLImages;

{ TBackgroundGL ------------------------------------------------------------ }

procedure TBackgroundGL.Render;
begin
 glCallList(szescianNieba_list);
end;

class function TBackgroundGL.NearFarToSkySphereRadius(const zNear, zFar: Single;
  const Proposed: Single): Single;
var
  Min, Max, SafeMin, SafeMax: Single;
begin
  Min := zNear * 2;
  Max := zFar * SphereRadiusToCubeSize;

  { The new sphere radius should be in [Min...Max].
    For maximum safety (from floating point troubles), be require
    that it's within slightly smaller "safe" range. }

  SafeMin := Lerp(0.1, Min, Max);
  SafeMax := Lerp(0.9, Min, Max);

  if (Proposed >= SafeMin) and
     (Proposed <= SafeMax) then
    Result := Proposed else
    Result := (Min + Max) / 2;
end;

constructor TBackgroundGL.Create(const Transform: TMatrix4Single;
  GroundAngle: PArray_Single; GroundAngleCount: Integer;
  GroundColor: PArray_Vector3Single; GroundColorCount: Integer;
  const Imgs: TBackgroundImages;
  SkyAngle: PArray_Single; SkyAngleCount: Integer;
  SkyColor: PArray_Vector3Single; SkyColorCount: Integer;
  SkySphereRadius: Single);

var CubeSize, CubeSize2: Single;

  procedure RenderTextureSide(bs: TBackgroundSide);
  const
    { wspolrzedne tekstury beda zawsze nakladane na te coords w kolejnosci
      (0, 0), (1, 0), (1, 1), (0, 1). }
    Coords: array[TBackgroundSide, 0..3]of TVector3Integer =
    ( ((1, 0, 1), (0, 0, 1), (0, 1, 1), (1, 1, 1)), {back}
      ((0, 0, 1), (1, 0, 1), (1, 0, 0), (0, 0, 0)), {bottom}
      ((0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)), {front}
      ((0, 0, 1), (0, 0, 0), (0, 1, 0), (0, 1, 1)), {left}
      ((1, 0, 0), (1, 0, 1), (1, 1, 1), (1, 1, 0)), {right}
      ((0, 1, 0), (1, 1, 0), (1, 1, 1), (0, 1, 1))  {top}
    );
    TexCoords: array[0..3]of TVector2i = ((0, 0), (1, 0), (1, 1), (0, 1));
  var i: Integer;
  begin
   if nieboTex[bs] = 0 then Exit;

   { If nieboTex[bs] <> 0 to for sure Imgs[bs] <> nil,
     so I can safely do here checks "Imgs[bs] is ..." }

   if Imgs[bs].HasAlpha then
   begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
   end;

   glBindTexture(GL_TEXTURE_2D, nieboTex[bs]);
   glBegin(GL_QUADS);
     for i := 0 to 3 do
     begin
      glTexCoordv(TexCoords[i]);
      glVertex3f( (Coords[bs, i, 0]*2-1)*CubeSize2,
                  (Coords[bs, i, 1]*2-1)*CubeSize2,
                  (Coords[bs, i, 2]*2-1)*CubeSize2);
     end;
   glEnd;

   if Imgs[bs].HasAlpha then
    glDisable(GL_BLEND);
  end;

  procedure StackCircleCalc(const Angle: Single; var Y, Radius: Single);
  { dla zadanego Angle, w konwencji 0 = zenith, Pi = nadir, oblicz
    na jakiej wysokosci powinna znalezc sie obrecz kola dla tego Angle
    i Radius tego kola. Wez pod uwage SkySphereRadius. }
  begin
   Radius := sin(Angle)*SkySphereRadius;
   Y := cos(Angle)*SkySphereRadius;
  end;

  { Render*Stack: render one stack of sky/ground sphere.
    Angles are given in the sky connvention : 0 is zenith, Pi is nadir.
    Colors are nterpolated from upper to lower angle from upper to lower color.
    RenderUpper/LowerStack do not need upper/lower angle: it is implicitly
    understood to be the zenith/nadir.

    TODO: ustalic we wszystkich Render*Stack ze sciany do wewnatrz sa
    zawsze CCW (albo na odwrot) i uzyc backface culling ? Czy cos na
    tym zyskamy ?
  }

  const
    { slices of rings rendered in Render*Stack }
    Slices = 24;

  procedure RenderStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  var UpperY, UpperRadius, LowerY, LowerRadius: Single;

    procedure Bar(const SliceAngle: Single);
    var SinSliceAngle, CosSliceAngle: Single;
    begin
     SinSliceAngle := Sin(SliceAngle);
     CosSliceAngle := Cos(SliceAngle);
     glColorv(LowerColor);
     glVertex3f(SinSliceAngle*LowerRadius, LowerY, CosSliceAngle*LowerRadius);
     glColorv(UpperColor);
     glVertex3f(SinSliceAngle*UpperRadius, UpperY, CosSliceAngle*UpperRadius);
    end;

  var i: Integer;
  begin
   StackCircleCalc(UpperAngle, UpperY, UpperRadius);
   StackCircleCalc(LowerAngle, LowerY, LowerRadius);
   glBegin(GL_QUAD_STRIP);
     Bar(0); for i := 1 to Slices-1 do Bar(i* 2*Pi/Slices); Bar(0);
   glEnd;
  end;

  procedure RenderUpperStack(
    const UpperColor: TVector3Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  { latwo ale bardzo nieoptymalnie moznaby to zapisac jako
    RenderStack(UpperColor, 0, LowerColor, LowerAngle); }
  var LowerY, LowerRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
     glVertex3f(Sin(SliceAngle)*LowerRadius, LowerY, Cos(SliceAngle)*LowerRadius);
    end;

  var i: Integer;
  begin
   StackCircleCalc(LowerAngle, LowerY, LowerRadius);
   glBegin(GL_TRIANGLE_FAN);
     glColorv(UpperColor);
     glVertex3f(0, SkySphereRadius, 0);
     glColorv(LowerColor);
     Pt(0); for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices); Pt(0);
   glEnd;
  end;

  procedure RenderLowerStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single);
  { latwo ale bardzo nieoptymalnie moznaby to zapisac jako
    RenderStack(UpperColor, UpperAngle, LowerColor, Pi); }
  var UpperY, UpperRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
     glVertex3f(Sin(SliceAngle)*UpperRadius, UpperY, Cos(SliceAngle)*UpperRadius);
    end;

  var i: Integer;
  begin
   StackCircleCalc(UpperAngle, UpperY, UpperRadius);
   glBegin(GL_TRIANGLE_FAN);
     glColorv(LowerColor);
     glVertex3f(0, -SkySphereRadius, 0);
     glColorv(UpperColor);
     Pt(0); for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices); Pt(0);
   glEnd;
  end;

var bs: TBackgroundSide;
    TexturedSides: TBackgroundSides;
    i: Integer;
    GroundHighestAngle: Single;
    SomeTexturesWithAlpha: boolean;
begin
 inherited Create;

 { caly konstruktor sprowadza sie do skonstruowania display listy
   szescianNieba_list, no i do zaladowania tekstur nieboTex zeby pozniej
   ta display lista mogla ich uzyc. }

 { calculate nieboTex and SomeTexturesWithAlpha }
 SomeTexturesWithAlpha := false;
 TexturedSides := [];
 for bs := Low(bs) to High(bs) do
 begin
  nieboTex[bs] := 0;
  if (Imgs[bs] <> nil) and (not Imgs[bs].IsNull) then
  begin
   try
     nieboTex[bs] := LoadGLTexture(Imgs[bs], GL_LINEAR, GL_LINEAR,
       { poniewaz rozciagamy teksture przy pomocy GL_LINEAR a nie chce nam
         sie robic teksturze borderow - musimy uzyc GL_CLAMP_TO_EDGE
         aby uzyskac dobry efekt na krancach }
       Texture2DClampToEdge);
   except
     { Although texture image is already loaded in Imgs[bs],
       still texture loading may fail, e.g. with ECannotLoadS3TCTexture
       when OpenGL doesn't have proper extensions. Secure against this by
       making nice DataWarning. }
     on E: ETextureLoadError do
     begin
       DataWarning('Texture load error: ' + E.Message);
       Continue;
     end;
   end;

   Include(TexturedSides, bs);
   if Imgs[bs].HasAlpha then SomeTexturesWithAlpha := true;
  end;
 end;

 CubeSize := SkySphereRadius * SphereRadiusToCubeSize;
 CubeSize2 := CubeSize / 2;

 szescianNieba_list := glGenListsCheck(1, 'TBackgroundGL.Create');

 glNewList(szescianNieba_list, GL_COMPILE);
 glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_COLOR_BUFFER_BIT);
 glPushMatrix;
 try
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LIGHTING);
   glDisable(GL_FOG);
   glDisable(GL_BLEND);

   if GL_ARB_multitexture then
     glActiveTextureARB(GL_TEXTURE0_ARB);
   glDisable(GL_TEXTURE_2D);
   if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
   if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);

   glMultMatrix(Transform);

   { wykonujemy najbardziej elementarna optymalizacje : jesli mamy 6 tekstur
     i zadna nie ma kanalu alpha (a w praktyce jest to chyba najczestsza sytuacja)
     to nie ma sensu sie w ogole przejmowac sky i ground, tekstury je zaslonia. }
   if (TexturedSides <> BGAllSides) or SomeTexturesWithAlpha then
   begin
    { calculate GroundHighestAngle, will be usable to optimize rendering sky.
      GroundHighestAngle is measured in sky convention (0 = zenith, Pi = nadir).
      If there is no sky I simply set GroundHighestAngle to sthg > Pi. }
    if GroundAngleCount <> 0 then
     GroundHighestAngle := Pi-GroundAngle^[GroundAngleCount-1] else
     GroundHighestAngle := Pi + 1;

    { render sky }
    Assert(SkyColorCount >= 1, 'Sky must have at least one color');
    Assert(SkyAngleCount+1 = SkyColorCount, 'Sky must have exactly one more Color than Angles');

    if SkyColorCount = 1 then
    begin
     { alpha ponizszego koloru nie ma znaczenia dla nas. Uzywamy 0 bo jest
       standardem (standardowo glClearColor ma alpha = wlasnie 0). }
     glClearColorv(SkyColor^[0], 0);
     glClear(GL_COLOR_BUFFER_BIT);
    end else
    begin
     { wiec SkyColorCount >= 2. W zasadzie rendering przebiega na zasadzie
         RenderUpperStack
         RenderStack iles razy
         RenderLowerStack
       Probujemy jednak przerwac robote w trakcie ktoregos RenderStack
       lub RenderLowerStack zeby nie tracic czasu na malowanie obszaru
       ktory i tak zamalujemy przez ground. Uzywamy do tego GroundHighestAngle.
     }
     RenderUpperStack(SkyColor^[0], SkyColor^[1], SkyAngle^[0]);
     for i := 1 to SkyAngleCount-1 do
     begin
      if SkyAngle^[i-1] > GroundHighestAngle then Break;
      RenderStack(SkyColor^[i]  , SkyAngle^[i-1],
                  SkyColor^[i+1], SkyAngle^[i]);
     end;
     { TODO: jesli ostatni stack ma SkyAngle bliskie Pi to powinnismy renderowac
       juz ostatni stack przy uzyciu RenderLowerStack. }
     if SkyAngle^[SkyAngleCount-1] <= GroundHighestAngle then
      RenderLowerStack(
        SkyColor^[SkyColorCount-1], SkyAngle^[SkyAngleCount-1],
        SkyColor^[SkyColorCount-1]);
    end;

    { render ground }
    if GroundAngleCount <> 0 then
    begin
     { jesli GroundAngleCount = 0 to nie ma ground wiec nie wymagamy wtedy
       zeby GroundColorCount = 1 (a wiec jest to wyjatek od zasady
       GroundAngleCount + 1 = GroundColorCount) }
     Assert(GroundAngleCount+1 = GroundColorCount, 'Ground must have exactly one more Color than Angles');

     RenderLowerStack(GroundColor^[1], Pi-GroundAngle^[0],
                      GroundColor^[0]);
     for i := 1 to GroundAngleCount-1 do
      RenderStack(GroundColor^[i+1], Pi-GroundAngle^[i],
                  GroundColor^[i]  , Pi-GroundAngle^[i-1]);
    end;
   end;

   { render cube with six textured faces }

   glEnable(GL_TEXTURE_2D);
   { Wybieramy GL_REPLACE bo scianki szescianu beda zawsze cale teksturowane
     i chcemy olac zupelnie kolor/material jaki bedzie na tych sciankach.
     Chcemy wziac to z tekstury (dlatego standardowe GL_MODULATE nie jest dobre).
     Ponadto, kanal alpha tez chcemy wziac z tekstury, tzn. szescian
     nieba ma byc przeswitujacy gdy tekstura bedzie przeswitujaca
     (dlatego GL_DECAL nie jest odpowiedni). }
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

   for bs := Low(bs) to High(bs) do RenderTextureSide(bs);
 finally
  glPopMatrix;
  glPopAttrib;
  glEndList;
 end;
end;

destructor TBackgroundGL.Destroy;
begin
 glDeleteLists(szescianNieba_list, 1);
 glDeleteTextures(6, @nieboTex);
 inherited;
end;

{ TSkyCube -------------------------------------------------------------- }

constructor TSkyCube.Create(const SkyNamePattern: string; zNear, zFar: Single);
var SkyImgs: TBackgroundImages;
begin
 SkyImgs := BackgroundImagesLoadFromOldNamePattern(SkyNamePattern);
 try
  Create(SkyImgs, zNear, zFar);
 finally BackgroundImagesFreeAll(SkyImgs, nil) end;
end;

constructor TSkyCube.Create(const Imgs: TBackgroundImages; zNear, zFar: Single);
begin
 inherited Create(RotationMatrixRad(Pi/2, Vector3Single(1, 0, 0)),
   nil, 0, nil, 0, Imgs, nil, 0, @Black3Single, 1,
   NearFarToSkySphereRadius(zNear, zFar));
end;

end.
