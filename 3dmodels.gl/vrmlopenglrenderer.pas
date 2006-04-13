{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Tutaj implementujemy klase @link(TVRMLOpenGLRenderer)
  ktora jest odpowiedzialna za renderowanie sceny VRML'a.)

  Scena VRML'a jest tutaj po prostu ciagiem par
  (Node: TNodeGeneralShape; State: TVRMLGraphTraverseState).

  Chcac wyrenderowac ciag takich par wywolaj

  - najpierw Prepare na wszystkich stanach ktore zamierzasz tutaj
    renderowac. Kolejnosc podawania stanow dla Prepare nie ma znaczenia,
    mozesz tez podawac stany ktorych w ogole nie bedziesz pozniej renderowal.
    Jezeli jakis stan bedzie renderowany dwa razy mozesz go podac tylko raz.
    Wazne jest zeby KAZDY STAN jaki bedziesz pozniej renderowal zostal
    podany dla Prepare. I teraz najwazniejsze : kazdy stan jaki bedziesz
    pozniej podawal do Render to musi byc dokladnie ten sam stan co tutaj,
    wiec musi miec te same wskazniki do node'ow Last*/Active* i
    ZAWARTOSC POL TYCH NODE'OW TEZ MUSI BYC JUZ CIAGLE TAKA SAMA.
    W szczegolnosci, bedziemy sobie tutaj zapamietywali wiazania niektorych
    obiektow z zasobami OpenGL'a ktore bedziemy im przydzielac w Prepare.
    Wiec musi byc pewne zeby dany wskaznik na obiekt wskazywal juz zawsze na
    TEN SAM OBIEKT.

    Prepare wymaga aktywnego kontekstu OpenGL'a. Prepare
    nie modyfikuje aktualnego stanu OpenGL'a poza tym ze moze zajac
    jakies display listy i indeksy tekstur OpenGL'a.
    Prepare nie moze byc wywolane jako czesc display-listy.

    Po wywolaniu Prepare (i pomiedzy kolejnymi wywolaniami Prepare)
    mozesz sobie spokojnie robic z OpenGL'em co ci sie zywnie podoba.
    Pamietaj tylko aby nie dotykac zajetych juz display-list i indeksow tekstury.

  - Jezeli zamierzasz zmienic zawartosc pol jakiegos node'a ktory trafil
    gdzies tutaj jako Last*/Active* w jakims State (albo jezeli np. zamierzasz
    w ogole zwolnic ten obiekt) i jezeli zamierzasz jeszcze uzywac
    tego renderera to musisz wywolac UnPrepare na tym nodzie.
    Wszystkie stany ktore przygotowales przez Prepare ktore
    jako jeden ze swoich Last*/Active* mialy ten node juz oczywiscie nie sa
    przygotowane. Pamietaj ze nawet jezeli zwalniasz ten node (a wiec
    nie zamierzasz nic z nim juz renderowac) to musisz mu tu zrobic
    Unprepare (bo inaczej jakis inny obiekt moglby byc kiedys
    skonstruowany tak ze wskazywalby na to samo miejsce w pamieci
    i co wtedy ? Nasz renderer moglby uznac ze ma do czynienia z tym samym
    obiektem !).

    Przy czym wiedz ze obiekt ktory podajesz do Unprepare MOZE byc
    (w momencie podawanie jego wskaznika) juz zwolniony. Wiec Unprepare
    w ogole nie zaglada do zawartosci obiektu. Wywolanie Unprepare
    na obiekcie ktory nigdy nie byl podawany jako Last*/Active* w jakims State
    tez nie jest zabronione (po prostu niczego nie spowoduje).

  - w jakis czas potem wywolaj RenderBegin. To zainicjuje stan OpenGL'a
    na taki jakiego bedziemy pozniej potrzebowac. Od momentu wywolania
    RenderBegin stan OpenGL'a jest we wladzy tego obiektu : nie wolno
    ci go modyfikowac w zaden sposob niz poprzez ten obiekt.
  - potem wywoluj Render(Node, State) aby renderowac pary Node+State.
    Jak juz powiedzialem, kazda podawana teraz para musiala wystapic
    wczesniej w wywolaniu Prepare.
  - potem wywolaj RenderEnd. Po wywolaniu RenderEnd bedziesz mial
    taki sam stan OpenGL'a jak przed wywolaniem RenderBegin, modulo
    fakt ze zawartosci buforow kolorow i depth buffera zostana
    zmodyfikowane (chociaz zawartosci buforow nie naleza do stanu OpenGL'a,
    wedlug definicji stanu OpenGL'a z jego specyfikacji).

    Dowolny kawalek powyzszych Renderow moze byc zapamietany na display-liscie.
    Typowe zastosowania to zapamietywanie na osobnych listach RenderBegin,
    Render kazdej pary i RenderEnd lub zapamietywanie na jednej display-liscie
    Render wszystkiego (Begin + pary + End). Jest gwarantowane ze rzeczy
    robione w RenderBegin i RenderEnd nie zaleza od Render node'ow jakie byly
    wykonywane i ze w ogole Render kazdej pary Node+State jest niezalezne -
    zapamietane na diplay-liscie, te listy beda nastepnie mogly byc
    wywolywane w innej kolejnosci itp.

  - mozesz powtarzac ten scenariusz na dowolne sposoby dowolnie wiele razy.
    Mozesz robic sobie Unprepare'y, Prepare'y, ciagi
    RenderBegin + Render'y + RenderEnd. Pamietaj tylko ze caly czas musi byc
    obecny ten sam kontekst OpenGL'a i pamietaj to co mowilem o node'ach
    ktore sa podawane jako Last*/Active* w odpowiednich State'ach : one musza caly
    czas zajmowac swoj wskaznik w pamieci (zeby zaden inny obiekt nie wskazywal
    nigdy na to miejsce pamieci) i zawsze musza miec ta sama zawartosc.

    W koncu zechcesz zwolnic ten obiekt : badz swiadomy ze to spowoduje
    zwolnienie tez kilku zasobow OpenGL'a bo wywola Unprepare.
    Wiec musi byc aktywny ten sam kontekst OpenGL'a.
    Podobnie jak z samym Unprepare : wszystkie node'y ktore gdzie pojawily
    sie jako Last*/Active* MOGA byc w tym czasie juz zwolnione z pamieci.

  Jeszcze slowko o kontekstach OpenGL'a : Create nie wymaga zadnego
    kontekstu aktywnego. Potem wszystko co wywolujesz musi byc w tym samym
    kontekscie. Az do wywolania UnprepareAll, po ktorym
    mozesz zaczac uzywac tego renderera na zupelnie innym kontekscie
    (UnprepareAll po prostu niszczy wszelkie zwiazki tego renderera
    z aktualnym kontekstem OpenGL'a). Uwaga - wywolanie Unpreprare
    na wszystkich node'ach ktore podales kiedys jako Last*/Active* to NIE jest
    to samo co UnprepareAll - wywolanie wszystkich mozliwych Unprepare
    to NIE to samo co UnpreprareAll; UnprepareAll moze robic cos wiecej
    (wiec wywoluj zawsze UnprepareAll aby uciac wszelki swoj zwiazek z
    aktualnym kontekstem OpenGL'a). UnprepareAll mozesz
    wykonywac dowolnie wiele razy, tzn. UnprepareAll nie zrobi nic
    (i tym samym nie bedzie wymagalo glcontextu) jesli juz ostatnio
    je wywolales i od tego czasu nie zrobiles zadnego Prepare.
    Destruktor wykonuje UnprepareAll automatycznie a wiec albo wywolasz
    destruktor podczas gdy ten glcontext jest jeszcze aktywny albo
    przynajmniej zrobisz UnprepareAll jako ostatnie wywolanie dla tego
    gl-kontekstu.

  Na koniec slowo o atrybutach renderowania (polach/wlasciwosciach Attrib_),
    ktore steruja tym jak bedzie wygladac renderowanie : mozesz je zmieniac
    tylko w momencie gdy renderer
    nie jest przywiazany do zadnego kontekstu OpenGL'a (co jest zwiazane z tym
    ze przywiazanie do danego kontekstu OpenGL'a oznacza takze ze czesc pracy
    z dostosowaniem sie do takich a nie innych atrybutow renderowania
    zostala juz zrobiona) czyli zaraz po wywolaniu konstruktora lub
    UnprepareAll (przed wywolaniem jakiegokolwiek Prepare czy Render*).

  Pewne notki w kwestii stanu OpenGL'a i renderowania sceny VRML'a :
    Jest jasne ze w implementacji RenderBegin musimy sobie zainicjowac
    pewne elementy stanu OpenGL'a, jak np. ustawic macierz na MODELVIEW
    itp. Oczywiscie potem w RenderEnd odtworzymy takie atrybuty
    jakie byly, ale nie o to chodzi. Chodzi o to ze sa pewne atrybuty
    ktorych NIE CHCEMY inicjowac na stan poczatkowy bo chcemy ci pozwolic
    na ich zainicjowanie samemu. Przede wszystkim nalezy tu wymienic
    aktualny stan macierzy MODELVIEW : gdybysmy chcieli dokladnie odtworzyc
    w OpenGL'u model VRML'a powinnismy na poczatku zrobic glLoadIdentity.
    Ale oczywiscie nie robimy tego, to by byla zupelnie bezsensowna strata
    funkcjonalnosci : poprzez pozwolenie ci na odpowiednie ustawienie
    macierzy MODELVIEW przed wywolaniem RenderBegin mozesz
    przesuwac, obracac i skalowac caly model VRML'a.
    Oto spisik rzeczy ktore mozesz roznie ustawic przed wywolaniem
    RenderBegin i za pomoca ktorych mozesz wplywac na sposob renderowania
    sceny VRML'a przez VRMLOpenGLRenderera :
     - aktualna macierz MODELVIEW i PROJECTION
     - glLightModel (GL_LIGHT_MODEL_AMBIENT)
       (notka: specyfikacja VRMLa 1.0 podaje domyslne LIGHT_MODEL_AMBIENT
        0.2, 0.2, 0.2 co jest akurat zgodne z domyslnym stanem OpenGL'a.
        Specyfikacja lighting VRMLa 97 (z ktora probujemy byc zgodni,
        nawet w VRMLu 1.0) podaje z kolei rownania swiatla takie ze wynika z nich
        ze LIGHT_MODEL_AMBIENT powinno byc zerowe.)
     - glPolygonMode
       (oczywiscie, zeby wszystko bylo normalne powinienes
       uzywac GL_FRONT_AND_BACK, GL_FILL. Ale przelaczanie sie na model
       wireframe moze byc bardzo pouczajace zeby zobaczyc jak model
       jest skostruowany i jak jest wyswietlany)
     - stan enabled GL_LIGHTING (sa specjalne sytuacje gdy material w VRMLu
       okresla juz precalculated color - wtedy LIGHTING moze byc chwilowo
       wylaczone i kolory obiektow beda brane z EmissionColor;
       ale normalnie, LIGHTING OpenGL'a bedzie pozostawione w takim stanie
       w jakim bylo w momencie wejscia w RenderBegin i OpenGL dostanie
       informacje ktore pozwola mu wyrenderowac wszystko ladnie bez wzgledu
       na to czy ma czy nie ma wlaczone LIGHTING)
     - glTexEnv
       (oczywiscie, powinienes uzywac defaultowego GL_MODULATE ktore
       gwarantuje ze polaczenie wlasciwosci materialu, swiatla i tekstury
       bedzie odpowiednio wyrenderowane; ale jesli masz model do specjalnych
       zastosowan, albo nie zamierzasz uzywac swiatla itp. - inne tryby
       tez moga byc uzyteczne)
     - glDepthMask, stan enabled GL_BLEND i glBlendFunc
       (te stany nie sa tu kontrolowane, choc zapewne kod zewnetrzny
       (tzn. kod uzywajacy tego renderera) moze  zechciec ustawic
       te wartosci na cos konkretnego. Manipulujac tymi wartosciami kod
       zewnetrzny moze zaimplementowac obiekty polprzezroczyste uzywajac
       blending OpenGLa, przykladowo tak robi VRMLFlatSceneGL)

  Notki o Triagles/VerticesCount : poniewaz OpenGL oferuje tylko cieniowanie
    Gourauda (no i plaskie cieniowanie, w zaleznosci od Attrib_SmoothShading),
    idea OverTriangulate = true w VRMLNodes.TNodeGeneralShape.[Local]Triangulate
    staje sie uzyteczna. Jest gwarantowane ze ten renderer bedzie uzywal
    dokladnie takich trojkatow jakie generuje metoda Triangulate z
    [Local]Triangulate, a wiec ze powinienes uzywac
    Triangles/VerticesCount(State, true) aby powiedziec userowi ile trojkatow
    dajemy OpenGLowi do wyrenderowania.
    TODO - niniejsza implementacja robi to, ale tak naprawde wcale nie uzywa
      metody Triangulate. A wiec de facto zapewniamy sobie zgodnosc z
      Triangles/VerticesCount(State, true) ale w brudny sposob - po prostu
      znajac implementacje Triangulate, implementujemy w tym rendererze taka
      sama triangulacje. Dlaczego nie uzywamy tu metody Triangulate ?
      Bo spowodowalaby mnostwo straty czasu bo powodowalaby ze niektore
      wartosci bylyby liczone dla tych samych vertexow wiele razy
      bo czesto podawalibysmy te same vertexy OpenGLowi nie mowiac mu ze one
      sa te same ! (bo Triangulate nie mowiloby nam ze one sa te same).
      Zeby to zaimplementowac musielibysmy znacznie skomplikowac obsluge metody
      Triangulate - musialaby zwracac vertexy + indeksy do vertexow,
      zamiast po prostu kazdy trojkat = 3 vertexy jak teraz.

  About GL extensions:

    This unit uses GL_EXT_compiled_vertex_array extension if it's
    available. Although, honestly, I didn't observe any speed
    improvements from using this extension (even when using TVRMLFlatSceneGL
    with RendererOptimization = roNone), but, anyway, this can't hurt us.

    Anyway, if you want to use this extension, you should init
    extensions in OpenGL unit by calling procedures
      ReadImplementationProperties;
      LoadProcExtensions;
    In the future this unit may use some more OpenGL extensions,
    so to have best performance be sure that you called these
    2 functions before using OpenGL renderer in this unit.
    Note to users of GLWindow unit: remember that @link(TGLWindow.Init)
    already takes care about it for you.
}

unit VRMLOpenGLRenderer;

{$I openglmac.inc}

{ TODO
  - use Backface culling in Render of Cone(all parts), Cube,
    Cylinder(all parts), Sphere when the viewer is outside
  - test Attrib_PointSize
}

{ define USE_VRML_NODES_TRIANGULATION only for testing purposes.
  We will use then very simple rendering method using
  TNodeGeneralShape.Triangulate method. This will be SLOW and many features
  WILL NOT be available - the ONLY reason why you (I) would need this is to
  test TNodeGeneralShape.Triangulate method. }
{ $define USE_VRML_NODES_TRIANGULATION}

{$ifdef USE_VRML_NODES_TRIANGULATION}
  {$ifdef RELEASE}
    {$fatal Undefine USE_VRML_NODES_TRIANGULATION
      for VRMLOpenGLRenderer - you don't want to use this in RELEASE version.}
  {$endif}
{$endif}

interface

uses
  Classes, SysUtils, KambiUtils, VectorMath, OpenGLh,
  VRMLFields, VRMLNodes, VRMLLexer, Boxes3d, OpenGLTTFonts, Images,
  OpenGLFonts, KambiGLUtils, VRMLLightSetGL;

{$define read_interface}

type
  TBeforeGLVertexProc = procedure (Node: TNodeGeneralShape;
    const Vert: TVector3Single) of object;

  TTextureGLBinding = record
    TextureNode: TNodeTexture2;
    TextureGL: TGLuint;
  end;
  PTextureGLBinding = ^TTextureGLBinding;

  TDynArrayItem_1 = TTextureGLBinding;
  PDynArrayItem_1 = PTextureGLBinding;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynTextureGLBindingArray = class(TDynArray_1)
    { szuka rekordu z danym TextureNode.
      Zwraca jego indeks lub -1 jesli nie znajdzie. }
    function TextureNodeIndex(TexNode: TNodeTexture2): integer;
  end;

  TVRMLOpenGLRenderer = class
  private
    { ---------------------------------------------------------
      GLContext-specific things, so freed (or reset in some other way to default
        uninitialized values) in UnprepareAll. }

    { FLastGLFreeLight = -1 if not calculated.
      GLContext - specific, so it is reset to -1 in UnprepareAll.
      Use always LastGLFreeLight, not FLastGLFreeLight or Attrib_LastGLFreeLight
      to get LastGLFreeLight. LastGLFreeLight function will not ever return -1
      and will minimize amount of calls to glGetInteger() }
    FLastGLFreeLight: integer;
    function LastGLFreeLight: integer;
    { TTF_Fonts = list of already created fonts. GLContext-specific so
      freed in UnprepareAll. }
    TTF_Fonts: array[TVRMLFontFamily, boolean, boolean]of TGLOutlineFont;
    TextureGLBindings: TDynTextureGLBindingArray;

    { ------------------------------------------------------------
      Rzeczy z ktorych mozna korzystac tylko w czasie Render. }

    { kopie aktualnego State i Node na czas Render }
    Render_State: TVRMLGraphTraverseState;
    Render_Node: TNodeGeneralShape;

    { te dwie zmienne sa wewnetrzne dla funkcji MeterialsBegin/End, BindMaterial }
    Render_Material_ForcedLightDisable: boolean;
    Render_Material_BoundMatNum: integer;
    procedure Render_MaterialsBegin;
    procedure Render_MaterialsEnd;
    procedure Render_BindMaterial(MatNum: integer);

    { czy Render node'ow musi generowac tex coords ? }
    Render_TexCoordsNeeded: boolean;

    procedure DoBeforeGLVertex(const Vert: TVector3Single);
    procedure DoGLVertex(const Vert: TVector3Single);
    procedure DoGLArrayElement(const Verts: PArray_Vector3Single; ith: TGLint);

    { ----------------------------------------------------------------- }

    { zwraca GLU_SMOOTH lub GLU_FLAT, zgodnie z Attrib_SmoothShading. }
    function Attrib_SmoothNormalsGLU: TGLenum;

    { zwraca Color zmodulowany uzywajac Attrib_ColorModulator }
    function ColorModulated(const Color: TVector3Single): TVector3Single; overload;
    function ColorModulated(const Color: TVector3Byte): TVector3Byte; overload;

    {$ifdef USE_VRML_NODES_TRIANGULATION}
    procedure DrawTriangle(const Tri: TTriangle3Single;
      State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape; MatNum: integer);
    {$endif}
  public

    { ---------------------------------------------------------------------
      atrybuty wyswietlania Attrib }

    {tuz przed narysowaniem KAZDEGO vertexa bedzie wywolywana ta procedura.
       (to znaczy TUZ przed glVertex, juz po ustaleniu koloru (glColor),
       glTexCoord, glNormal, glEdgeFlag, no w ogole - wszystkiego).
       Najpierw bedzie wywolana ta procedura z parametrami a)node podklasy
       TNodeGeneralShape ktory renderuje ten vertex i b)wspolrzedne vertexa
       przemnozone przez RenderState.CurrMatrix. Innymi slowy bedzie to
       wspolrzedna vertexa wzgledem lokalnego ukladu wspolrzednych VRML'a
       (podczas gdy rzeczywista komenda glVertex prawdopodobnie przekaze
       OpenGL'owi wspolrzedne prosto z pliku VRML (a wiec byc moze wzgledem
       ukladu wspolrzednych aktualnego node'a, a nie calego VRML'a)).
     Zrobilem ta proc zeby zrobic glForCoord, ale byc moze znajdzie sie
       kiedys dla niej jeszcze jakies zastosowanie. }
    Attrib_OnBeforeGLVertex: TBeforeGLVertexProc; { = nil }

    { Ponizsze ustawienie kontroluje czy na poczatku renderowania sceny wywolac
        glShadeModel(GL_SMOOTH) czy GL_FLAT. Ponadto w czasie renderowania
        sceny beda generowane odpowiednie normale (troszeczke inne normale
        trzeba generowac gdy chcemy miec powierzchnie flat, a inne gdy smooth;
        NIE WYSTACZY generowac zawsze normali smooth i tylko przestawiac
        glShadeModel na GL_FLAT aby miec poprawne cieniowanie flat.
        Chociaz zazwyczaj daje to tez dobre efekty; w szczeglnosci,
        pamietaj ze nie bedziemy zmieniac normali ktore juz bedziemy mieli
        podane w pliku VRMLa; wiec i tak nie bedzie pelnej poprawnosci
        bo jezeli te normale byly zapisane smooth a my chcemy flat
        to uzyjemy tych normali smooth z shadeModel ustawionym na FLAT).
      Oczywiscie powinienes uzywac GL_SMOOTH bo rezultaty moga nie byc
        tak dobre jak moglyby byc jesli uzyjesz GL_FLAT; no, ale beda
        przewidywalne; wiec jesli np. wiesz ze dla danego modelu
        GL_FLAT wyprodukuje zadowalajacy wynik to mozesz tego uzyc)}
    Attrib_SmoothShading: boolean; { = true }

    { Za pomoca ponizszych ColorModulatorow mozna osiagnac mile efekty renderowania
      sceny w spacjalnych kolorach - np. grayscale, tylko red channel itp.
      Zawsze lepiej jest je robic metodami OpenGL'a, jak np. BlackOuty,
      ale nie wszystko mozna zrobic metodami OpenGL'a - np. sceny grayscale
      nie stworzymy robiac jakies proste sztuczki 2d w OpenGL'u.
      Tutaj mozna podac funkcje ktora w dowolny sposob modyfikuje kolor
      (ale pamietajmy ze ona modyfikuje kolor obiektu/swiatla/itp., a nie
      wynikowy kolor pixela na 2d; wiec np. powierzchnie nieoswietlone
      zawsze beda w ten sposob ciemne chocby modulator odwracal kolory itp.).

      Obie funkcje ColorModulator* musza byc nil lub musza wykonywac analogiczna
      konwersje ! (bo nie jest zdefiniowane kiedy nasz engine uzyje wersji
      Byte a kiedy Single)

      Ponadto musza wyniki tych funkcji musza byc zdeterminowane na podstawie
      argumentow - tzn. te funkcje nie moga zwracac czegos losowego, nie moga
      zwracac czegos na podstawie aktualnego czasu itp. To dlatego ze
      nie jest zdefiniowane jak dlugo wyniki tych funkcji moga byc w roznych
      miejscach kodu cache'owane. }
    Attrib_ColorModulatorSingle: TColorModulatorSingleFunc; { = nil }
    Attrib_ColorModulatorByte: TColorModulatorByteFunc; { = nil }

    { UseLights mowi zeby oswietlac shape'y swiatlami w node'ach VRMLa.
      Wykorzysta do tego swiatla OpenGL'a od FirstGLFreeLight .. LastGLFreeLight.
      Gdy LastGLFreeLight = -1 to zostanie uzyte glGet(GL_MAX_LIGHT)-1
      (czyli wszystkie swiatla powyzej First beda potraktowane jako wolne. }
    Attrib_UseLights: boolean; { = false }
    Attrib_FirstGLFreeLight: integer; { = 0 }
    Attrib_LastGLFreeLight: integer; { = -1 }

    { Jezeli not RenderAttrib_EnableTextures to przez caly czas renderowania
        sceny jest glDisable(GL_TEXTURE2D). Wiekszosc node'ow powstrzymuje sie
        wtedy takze od generowania wspolrzednych tekstury wiec mozemy miec
        maly zysk szybkosci.
      Jezeli jest true to robimy normalnie : gdy jakas tekstura jest aktywna
        to jej uzywamy, ustawiajac sobie glEnable(GL_TEXTURE2D) gdy trzeba. }
    Attrib_EnableTextures: boolean; { = true }

    { ponizsze parametry kontroluja min i mag filter dla tekstur }
    Attrib_TextureMinFilter, Attrib_TextureMagFilter: TGLint; { = GL_LINEAR, GL_LINEAR }

    { scena bedzie wyswietlana z glPointSize(Attrib_PointSize),
      co ma wplyw tylko na renderowanie PointSet. Zrobilem to atrybutem
      renderera (zamiast po prostu pozwolic temu stanowi OpenGL'a "przeciec"
      z zewnatrz) bo domyslny rozmiar mial byc = 3 a nie 1 (jak w OpenGL'u) }
    Attrib_PointSize: integer; { = 3 }

    { true oznacza ze stan zmiennych OpenGLa GL_FOG_BIT (w szczegolnosci
        stan enabled/disabled GL_FOG) jest kontrolowany przez tego renderera
        (pomiedzy RenderBegin a RenderEnd stan tych zmiennych zalezy tylko od
        przekazanych do RenderBegin informacji o mgle, nie zalezy od dotychczasowego
        (przed wywolaniem RenderBegin) stanu OpenGLa).
      false oznacza ze informacje o mgle przekazywane do RenderBegin sa
        ignorowane i w tej klasie zadne wywolania, wlacznie z RenderBegin i
        RenderEnd, nie dotykaja zmiennych z grupy atrybutow GL_FOG_BIT.
      Innymi slowy, przy true ustawienia mgly z jakimi renderowany jest model
        sa calkowicie zdeterminowane przez podane do RenderBegin informacje
        o mgle. Przy false sa calkowicie zdeterminowane przez ustawienia
        mgly w OpenGLu w momencie wywolywania RenderBegin. }
    Attrib_UseFog: boolean; { = true }

    { ----------------------------------------------------------------------------- }

    constructor Create;
    destructor Destroy; override;

    { przygotuj stan State aby moc pozniej renderowac shape'y ze stanem State.
      Od tego momentu do wywolania Unprepare[All] node'y przekazane tu jako
      Last*/Active* sa "zamrozone" : nie mozna zrobic im Free, nie mozna
      zmienic ich zawartosci. }
    procedure Prepare(State: TVRMLGraphTraverseState);
    { zniszcz powiazania renderera z danym node'em i z zasobami OpenGL'a jakie
      byly utworzone w ramach "przygotowan do wyswietlenia tego node'a" jako
      Last*/Active* node w jakims State. }
    procedure Unprepare(Node: TVRMLNode);
    { zniszcz istniejace powiazania tego renderera z jakimikolwiek node'ami
      jakie dostawal gdzies w Prepare i z aktualnym kontekstem OpenGL'a.
      Nie zrobi nic jezeli takich powiazan nie bylo (np. jesli wywolales
      UnprepareAll drugi raz zaraz po pierwszym) }
    procedure UnprepareAll;

    procedure RenderBegin(FogNode: TNodeFog;
      const FogDistanceScaling: Single);
    procedure RenderEnd;
    procedure Render(Node: TNodeGeneralShape; State: TVRMLGraphTraverseState);
  end;

  EVRMLOpenGLRenderError = class(EVRMLError);

{$undef read_interface}

implementation

uses NormalsCalculator, Math, Triangulator;

{$define read_implementation}
{$I dynarray_1.inc}

{$I openglmac.inc}

{ TDynTextureGLBindingArray ---------------------------------------------------- }

function TDynTextureGLBindingArray.TextureNodeIndex(TexNode: TNodeTexture2): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].TextureNode = TexNode then exit;
 result := -1;
end;

{ TVRMLOpenGLRenderer ---------------------------------------------------------- }

constructor TVRMLOpenGLRenderer.Create;
begin
 inherited Create;

 FLastGLFreeLight := -1;

 Attrib_SmoothShading := true;
 Attrib_UseLights := false;
 Attrib_FirstGLFreeLight := 0;
 Attrib_LastGLFreeLight := -1;
 Attrib_EnableTextures := true;
 Attrib_TextureMinFilter := GL_LINEAR;
 Attrib_TextureMagFilter := GL_LINEAR;
 Attrib_PointSize := 3;
 Attrib_UseFog := true;

 TextureGLBindings := TDynTextureGLBindingArray.Create;
end;

destructor TVRMLOpenGLRenderer.Destroy;
begin
 UnprepareAll;
 TextureGLBindings.Free;
 inherited;
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

procedure TVRMLOpenGLRenderer.Prepare(State: TVRMLGraphTraverseState);
const
  TexWrapEnumToGL: array[TEXWRAP_REPEAT..TEXWRAP_CLAMP]of TGLenum =
  (GL_REPEAT, GL_CLAMP);
var fsfam: TVRMLFontFamily;
    fsbold, fsitalic: boolean;
    TexGLBinding: TTextureGLBinding;
begin
 {przygotuj font dla LastFontStyle}
 fsfam := State.LastNodes.FontStyle.FdFamily.Value;
 fsbold := State.LastNodes.FontStyle.FdStyle.Flags[FSSTYLE_BOLD];
 fsitalic := State.LastNodes.FontStyle.FdStyle.Flags[FSSTYLE_ITALIC];
 if TTF_Fonts[fsfam, fsbold, fsitalic] = nil then
  TTF_Fonts[fsfam, fsbold, fsitalic] := TGLOutlineFont.Create(State.LastNodes.FontStyle.TTF_Font);

 {przygotuj teksture dla LastTexture2}
 if (TextureGLBindings.TextureNodeIndex(State.LastNodes.Texture2) = -1) then
 begin
  if State.LastNodes.Texture2.IsTextureImage then
  begin
   TexGLBinding.TextureNode := State.LastNodes.Texture2;

   TexGLBinding.TextureGL := LoadGLTextureModulated(
     State.LastNodes.Texture2.TextureImage,
     Attrib_TextureMinFilter, Attrib_TextureMagFilter,
     TexWrapEnumToGL[State.LastNodes.Texture2.FdWrapS.Value],
     TexWrapEnumToGL[State.LastNodes.Texture2.FdWrapT.Value],
     Attrib_ColorModulatorByte);

   TextureGLBindings.AppendItem(TexGLBinding);
  end;
 end;
end;

procedure TVRMLOpenGLRenderer.Unprepare(Node: TVRMLNode);
var i: integer;
begin
 {zwracam uwage ze nie niszczymy tu fontow dla (LastNode is TNodeFontStyle)
  bo fonty sa gromadzone w tablicy TTF_Fonts i wszystkie Font Styles
  o takich samych wlasciwosciach Family i Style korzystaja zawsze z tego
  samego juz utworzonego fontu.}

 {niszczymy teksture}
 if Node is TNodeTexture2 then
 begin
  i := TextureGLBindings.TextureNodeIndex(TNodeTexture2(Node));
  if i >= 0 then
  begin
   glDeleteTextures(1, @(TextureGLBindings.Items[i].TextureGL));
   TextureGLBindings.Delete(i, 1);
  end;
 end;
end;

procedure TVRMLOpenGLRenderer.UnprepareAll;
var ff: TVRMLFontFamily;
    b1, b2: boolean;
    i: integer;
begin
 FLastGLFreeLight := -1;

 {niszcz fonty}
 for ff := Low(ff) to High(ff) do
  for b1 := Low(boolean) to High(boolean) do
   for b2 := Low(boolean) to High(boolean) do
    FreeAndNil(TTF_Fonts[ff, b1, b2]);

 {niszcz wszystkie tekstury}
 for i := 0 to TextureGLBindings.Count-1 do
  glDeleteTextures(1, @(TextureGLBindings.Items[i].TextureGL));
 TextureGLBindings.SetLength(0);
end;

function TVRMLOpenGLRenderer.LastGLFreeLight: integer;
begin
 if FLastGLFreeLight = -1 then
 begin
  {jezeli jeszcze nie pobrane FLastGLFreeLight to pobierz je teraz:}
  if Attrib_LastGLFreeLight = -1 then
   FLastGLFreeLight := glGetInteger(GL_MAX_LIGHTS)-1 else
   FLastGLFreeLight := Attrib_LastGLFreeLight;
 end;
 result := FLastGLFreeLight;
end;

{ Render ---------------------------------------------------------------------- }

{$I vrmlopenglrenderer_render_glvertex.inc}
{$I vrmlopenglrenderer_render_materials.inc}
{$I vrmlopenglrenderer_indexednodesrenderer.inc}

procedure TVRMLOpenGLRenderer.RenderBegin(FogNode: TNodeFog;
  const FogDistanceScaling: Single);

  procedure SetupFog;
  var
    FogType: Integer;
    FogVisibilityRangeScaled: Single;
  const FogDensityFactor = 3.0;
  begin
   if not Attrib_UseFog then Exit;

   if (FogNode = nil) or (FogNode.FdVisibilityRange.Value = 0.0) then
    begin glDisable(GL_FOG); Exit end;

   { evaluate FogType, check if it's >= 0 }
   FogType := ArrayPosStr(FogNode.FdFogType.Value, ['LINEAR', 'EXPONENTIAL']);
   if FogType = -1 then
   begin
    VRMLNonFatalError('Unknown fog type '''+FogNode.FdFogType.Value+'''');
    glDisable(GL_FOG);
    Exit;
   end;

   FogVisibilityRangeScaled :=
     FogNode.FdVisibilityRange.Value * FogDistanceScaling;

   glEnable(GL_FOG);
   glFogv(GL_FOG_COLOR, Vector4Single(ColorModulated(FogNode.FdColor.Value), 1.0));
   case FogType of
    0: begin
        glFogi(GL_FOG_MODE, GL_LINEAR);
        glFogf(GL_FOG_START, 0);
        glFogf(GL_FOG_END, FogVisibilityRangeScaled);
       end;
    1: begin
        glFogi(GL_FOG_MODE, GL_EXP);
        { patrz VRMLNotes.txt po komentarz dlaczego w ten sposob implementuje
          mgle exponential VRMLa w OpenGLu }
        glFogf(GL_FOG_DENSITY, FogDensityFactor / FogVisibilityRangeScaled);
       end;
   end;
  end;

var i: integer;
begin
 {push attribs and matrices (by pushing attribs FIRST we save also current
  matrix mode)}
 glPushAttrib(GL_ALL_ATTRIB_BITS);
 glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
 glMatrixMode(GL_TEXTURE); glPushMatrix;
 glMatrixMode(GL_MODELVIEW);

 {init our OpenGL state}
 glMatrixMode(GL_MODELVIEW);
 glDisable(GL_COLOR_MATERIAL);
 glDisable(GL_TEXTURE_GEN_S);
 glDisable(GL_TEXTURE_GEN_T);
 glDisable(GL_TEXTURE_GEN_Q);
 glEnable(GL_NORMALIZE);
 glPointSize(Attrib_PointSize);
 glEnable(GL_DEPTH_TEST);
 glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

 {podczas renderowania Indexed_Faces_Or_Triangles mozemy na chwile wlaczac
  GL_CULL_FACE, mozemy tez zmieniac na chwile glFrontFace,
  mozemy tez zmieniac glCullFace.}
 glFrontFace(GL_CCW);
 glCullFace(GL_BACK);
 glDisable(GL_CULL_FACE);

 glDisable(GL_ALPHA_TEST);
 {AlphaFunc uzywane tylko w Texture2 i tam taka wartosc jest dobra}
 glAlphaFunc(GL_GEQUAL, 0.5);

 if Attrib_SmoothShading then
  glShadeModel(GL_SMOOTH) else
  glShadeModel(GL_FLAT);

 if Attrib_UseLights then
  for i := Attrib_FirstGLFreeLight to LastGLFreeLight do glDisable(GL_LIGHT0+i);

 SetupFog;
end;

procedure TVRMLOpenGLRenderer.RenderEnd;
begin
 {pop matrices and attribs (by popping matrix LAST we restore also saved
  matrix mode)}
 glMatrixMode(GL_TEXTURE); glPopMatrix;
 glPopClientAttrib;
 glPopAttrib;
end;

{$ifdef USE_VRML_NODES_TRIANGULATION}
procedure TVRMLOpenGLRenderer.DrawTriangle(const Tri: TTriangle3Single;
  State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape; MatNum: integer);
begin
 with State.LastNodes.Material do
 begin
  glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4f(ColorModulated(AmbientColor3Single(MatNum)), Opacity(MatNum)));
  glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4f(ColorModulated(DiffuseColor3Single(MatNum)), Opacity(MatNum)));
  glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4f(ColorModulated(SpecularColor3Single(MatNum)), Opacity(MatNum)));
  glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4f(ColorModulated(EmissiveColor3Single(MatNum)), Opacity(MatNum)));
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, ShininessExp(MatNum));
  glColorv(ColorModulated(DiffuseColor3Single(MatNum)));
 end;

 glNormalv(TriangleNormal(Tri));

 glBegin(GL_TRIANGLES);
  glVertexv(Tri[0]);
  glVertexv(Tri[1]);
  glVertexv(Tri[2]);
 glEnd;
end;

procedure TVRMLOpenGLRenderer.Render(Node: TNodeGeneralShape; State: TVRMLGraphTraverseState);
begin
 { alternatywny prosty rendering przez Triangulate, only to test
   Triangulate. We should use here OverTriagulate = true (but it's not
   impl yet because I don't need it anywhere (well, I would use it here
   but this is just some testing code)) }
 { Self. below required due to FPC 1.0.10 bug }
 Node.Triangulate(State, false, Self.DrawTriangle);
end;

{$else}

procedure TVRMLOpenGLRenderer.Render(Node: TNodeGeneralShape;
  State: TVRMLGraphTraverseState);
var
  { jaki font jest aktualny (na podstawie State.LastNodes.FontStyle) }
  CurrentFont: TGLOutlineFont;

  {$I VRMLOpenGLRenderer_Render_SpecificNodes.inc}

var IndexedRenderer: TGeneralIndexedRenderer;
    TextureGLBindingsIndex: Integer;
begin
 {LogWrite('Rendering ' +Node.NodeTypeName+ ' named ' +Node.NodeName);}

 {zrob nasze kopie}
 Render_State := State;
 Render_Node := Node;

 glMatrixMode(GL_TEXTURE); glLoadMatrix(State.CurrTextureMatrix);
 glMatrixMode(GL_MODELVIEW);

 { uwzglednij atrybut Attrib_UseLights : jezeli jest = true to zdefiniuj
   OpenGLowi wszystkie State.ActiveLights. Robimy to PRZED zaladowaniem
   transformacji State.CurrMatrix (bo swiatla maja wlasne CurrMatrix i
   nie podlegaja transformacji aktualnego State'a w ktorym sa) }
 if Attrib_UseLights then
   glLightsFromVRML(State.ActiveLights, Attrib_FirstGLFreeLight, LastGLFreeLight,
     Attrib_ColorModulatorSingle);

 glPushMatrix;
   glMultMatrix(State.CurrMatrix);

   {ponizej zarzadzamy wlasciwosciami alphaTest(+enabled), i texture2d enabled}

   {notka : nalezy tu zauwazyc ze robimy alphaTest ale jezeli
    GL_TEXTURE_ENV_MODE = GL_MODULATE to alpha ktore bedzie testowane
    tak naprawde nie bedzie alpha textury - to bedzie alpha textury
    zmieszane z alpha koloru zmieszane z alpha swiatla. Nawet gdybysmy
    dali texture env mode = GL_REPLACE to ciagle alpha swiatla ma wplyw
    na testowane alpha fragmentu (chociaz to juz nie jest takie zle,
    bo w VRML'u nie ma czegos takiego jak "alpha" czy "transparency"
    koloru swiatla co jest dosc rozsadnym uproszczeniem) ALE przeciez
    my chcemy miec GL_MODULATE bo powinnismy modulowac kolor tekstury
    kolorem materialu ! Nie widze tu ladnego sposobu jak mialbym
    pogodzic transparency materialu z kanalem alpha tekstury, przeciez
    chcac zadowolic specyfikacje VRML'a musze honorowac obie te rzeczy.
    Wiec niniejszym decyduje sie po prostu mieszac alpha textury z
    transparency materialu tak jak to robi OpenGL w GL_MODULATE.

    (Dementi - chyba alpha swiatla w OpenGLu nie ma wplywu
     na wyliczone alpha vertexu. Do czego jest alpha swiatla ?)

    To wszystko bedzie mialo wieksze znaczenie gdy zaimplementuje
    pelne partial-transparency na teksturach (nie tylko 0-1 jak jest teraz)
    i ktos zechce kombinowac finezyjne transparency
    materialu z finezyjnym (nie-0-1-kowym) kanalem alpha textury.
   }
   if (State.LastNodes.Texture2.IsTextureImage) and Attrib_EnableTextures then
   begin
    SetGLEnabled(GL_ALPHA_TEST, State.LastNodes.Texture2.TextureImage is TAlphaImage);
    glEnable(GL_TEXTURE_2D);
    TextureGLBindingsIndex := TextureGLBindings.TextureNodeIndex(
      State.LastNodes.Texture2);
    Assert(TextureGLBindingsIndex <> -1,
      'You''re calling TVRMLOpenGLRenderer.Render with a State ' +
      'that was not passed to TVRMLOpenGLRenderer.Prepare. ' +
      'You must call TVRMLOpenGLRenderer.Prepare first');
    glBindTexture(GL_TEXTURE_2D,
      TextureGLBindings.Items[TextureGLBindingsIndex].TextureGL);
    Render_TexCoordsNeeded := true;
   end else
   begin
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_ALPHA_TEST);
    Render_TexCoordsNeeded := false;
   end;

   with State.LastNodes.FontStyle do
    CurrentFont := TTF_Fonts[FdFamily.Value, FdStyle.Flags[FSSTYLE_BOLD],
      FdStyle.Flags[FSSTYLE_ITALIC]];

   Render_MaterialsBegin;
   try
    case ArrayPosPointer(Node.ClassType, [
          TNodeAsciiText,
          TNodeCone,
          TNodeCube,
          TNodeCylinder,
          TNodePointSet,
          TNodeSphere,
          TNodeIndexedFaceSet,
          TNodeIndexedTriangleMesh,
          TNodeIndexedLineSet ]) of
     0: RenderAsciiText(TNodeAsciiText(Node));
     1: RenderCone     (TNodeCone     (Node));
     2: RenderCube     (TNodeCube     (Node));
     3: RenderCylinder (TNodeCylinder (Node));
     4: RenderPointSet (TNodePointSet (Node));
     5: RenderSphere   (TNodeSphere   (Node));
     6, 7, 8:
       begin
        IndexedRenderer := CreateIndexedRenderer(Self);
        try
         IndexedRenderer.Render;
        finally IndexedRenderer.Free end;
       end;
     else
       raise EVRMLOpenGLRenderError.Create(
         'Rendering of node kind '+Node.NodeTypeName+' not implemented');
    end;
   finally Render_MaterialsEnd end;
 glPopMatrix;
end;
{$endif}

{ inne metody --------------------------------------------------------------- }

function TVRMLOpenGLRenderer.Attrib_SmoothNormalsGLU: TGLenum;
begin
 if Attrib_SmoothShading then result := GLU_SMOOTH else result := GLU_FLAT;
end;

function TVRMLOpenGLRenderer.ColorModulated(const Color: TVector3Single): TVector3Single;
begin
 if Assigned(Attrib_ColorModulatorSingle) then
  result := Attrib_ColorModulatorSingle(Color) else
  result := Color;
end;

function TVRMLOpenGLRenderer.ColorModulated(const Color: TVector3Byte): TVector3Byte;
begin
 if Assigned(Attrib_ColorModulatorByte) then
  result := Attrib_ColorModulatorByte(Color) else
  result := Color;
end;

end.
