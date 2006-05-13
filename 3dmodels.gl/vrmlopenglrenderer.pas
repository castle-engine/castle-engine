{
  Copyright 2002-2006 Michalis Kamburelis.

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

  - potem wywoluj RenderShapeState(Node, State)
    aby renderowac pary Node+State.
    Jak juz powiedzialem, kazda podawana teraz para musiala wystapic
    wczesniej w wywolaniu Prepare.

    Alternatively you can call RenderShapeStateBegin,
    RenderShapeStateNoTransform, RenderShapeStateEnd (always in this sequence,
    always RenderShapeStateEnd in the "finally" clause, so that
    after RenderShapeStateBegin there is always RenderShapeStateEnd
    called). This is equivalent to RenderShapeState
    (but sometimes it's better as it allows you to place
    RenderShapeStateNoTransform on a separate display list, that can be more
    shared (because it doesn't take some transformations into account)).

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
    Gourauda (no i plaskie cieniowanie, w zaleznosci od Attributes.SmoothShading),
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
  - test Attributes.PointSize
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
  OpenGLFonts, KambiGLUtils, VRMLLightSetGL, TTFontsTypes;

{$define read_interface}

type
  TBeforeGLVertexProc = procedure (Node: TNodeGeneralShape;
    const Vert: TVector3Single) of object;

  { These are various properties that control rendering done
    with @link(TVRMLOpenGLRenderer).

    They are collected here,
    in a class separate from @link(TVRMLOpenGLRenderer),
    because various things (like TVRMLFlatSceneGL and TVRMLGLAnimation)
    wrap @link(TVRMLOpenGLRenderer) instances and hide it,
    but still they want to allow user to change these attributes. }
  TVRMLRenderingAttributes = class(TPersistent)
  private
    FOnBeforeGLVertex: TBeforeGLVertexProc;
    FSmoothShading: boolean;
    FColorModulatorSingle: TColorModulatorSingleFunc;
    FColorModulatorByte: TColorModulatorByteFunc;
    FUseLights: boolean;
    FFirstGLFreeLight: integer;
    FLastGLFreeLight: integer;
    FEnableTextures: boolean;
    FTextureMinFilter: TGLint;
    FTextureMagFilter: TGLint;
    FPointSize: integer;
    FUseFog: boolean;
  protected
    { In this class these methods just set value on given property.
      In descendants you can do something more here, like automatic
      calling UnprepareAll of related TVRMLOpenGLRenderer
      (this is not done here, as this would be dangerous ---
      caller must be aware that TVRMLOpenGLRenderer was unprepared,
      and must prepare it again, otherwise rendering will fail).
      @groupBegin }
    procedure SetOnBeforeGLVertex(const Value: TBeforeGLVertexProc); virtual;
    procedure SetSmoothShading(const Value: boolean); virtual;
    procedure SetColorModulatorSingle(const Value: TColorModulatorSingleFunc); virtual;
    procedure SetColorModulatorByte(const Value: TColorModulatorByteFunc); virtual;
    procedure SetUseLights(const Value: boolean); virtual;
    procedure SetFirstGLFreeLight(const Value: integer); virtual;
    procedure SetLastGLFreeLight(const Value: integer); virtual;
    procedure SetEnableTextures(const Value: boolean); virtual;
    procedure SetTextureMinFilter(const Value: TGLint); virtual;
    procedure SetTextureMagFilter(const Value: TGLint); virtual;
    procedure SetPointSize(const Value: integer); virtual;
    procedure SetUseFog(const Value: boolean); virtual;
    { @groupEnd }
  public
    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TPersistent): boolean; virtual;

    { tuz przed narysowaniem KAZDEGO vertexa bedzie wywolywana ta procedura.
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
      kiedys dla niej jeszcze jakies zastosowanie.

      nil by default. }
    property OnBeforeGLVertex: TBeforeGLVertexProc
      read FOnBeforeGLVertex write SetOnBeforeGLVertex;

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
    property SmoothShading: boolean
      read FSmoothShading write SetSmoothShading default true;

    { zwraca GLU_SMOOTH lub GLU_FLAT, zgodnie z SmoothShading. }
    function SmoothNormalsGLU: TGLenum;

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
      miejscach kodu cache'owane.

      Both are nil by default.
      @groupBegin }
    property ColorModulatorSingle: TColorModulatorSingleFunc
      read FColorModulatorSingle write SetColorModulatorSingle;
    property ColorModulatorByte: TColorModulatorByteFunc
      read FColorModulatorByte write SetColorModulatorByte;
    { @groupEnd }

    { zwraca Color zmodulowany uzywajac ColorModulatorXxx }
    function ColorModulated(const Color: TVector3Single): TVector3Single; overload;
    function ColorModulated(const Color: TVector3Byte): TVector3Byte; overload;

    { UseLights mowi zeby oswietlac shape'y swiatlami w node'ach VRMLa.
      Wykorzysta do tego swiatla OpenGL'a od FirstGLFreeLight .. LastGLFreeLight.
      Gdy LastGLFreeLight = -1 to zostanie uzyte glGet(GL_MAX_LIGHT)-1
      (czyli wszystkie swiatla powyzej First beda potraktowane jako wolne. }
    property UseLights: boolean
      read FUseLights write SetUseLights default false;
    property FirstGLFreeLight: integer
      read FFirstGLFreeLight write SetFirstGLFreeLight default 0;
    property LastGLFreeLight: integer
      read FLastGLFreeLight write SetLastGLFreeLight default -1;

    { Jezeli not EnableTextures to przez caly czas renderowania
      sceny jest glDisable(GL_TEXTURE2D). Wiekszosc node'ow powstrzymuje sie
      wtedy takze od generowania wspolrzednych tekstury wiec mozemy miec
      maly zysk szybkosci.

      Jezeli jest true to robimy normalnie : gdy jakas tekstura jest aktywna
      to jej uzywamy, ustawiajac sobie glEnable(GL_TEXTURE2D) gdy trzeba. }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { ponizsze parametry kontroluja min i mag filter dla tekstur.
      @groupBegin }
    property TextureMinFilter: TGLint
      read FTextureMinFilter write SetTextureMinFilter default GL_LINEAR;
    property TextureMagFilter: TGLint
      read FTextureMagFilter write SetTextureMagFilter default GL_LINEAR;
    { @groupEnd }

    { scena bedzie wyswietlana z glPointSize(PointSize),
      co ma wplyw tylko na renderowanie PointSet. Zrobilem to atrybutem
      renderera (zamiast po prostu pozwolic temu stanowi OpenGL'a "przeciec"
      z zewnatrz) bo domyslny rozmiar mial byc = 3 a nie 1 (jak w OpenGL'u) }
    property PointSize: integer
      read FPointSize write SetPointSize default 3;

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
    property UseFog: boolean
      read FUseFog write SetUseFog default true;
  end;

  TVRMLRenderingAttributesClass = class of TVRMLRenderingAttributes;

  TGLOutlineFontCache = record
    References: Cardinal;
    Instance: TGLOutlineFont;
  end;

  TTextureCache = record
    FileName: string;
    Node: TNodeTexture2;
    MinFilter: TGLint;
    MagFilter: TGLint;
    WrapS: TGLenum;
    WrapT: TGLenum;
    ColorModulator: TColorModulatorByteFunc;
    References: Cardinal;
    GLName: TGLuint;
  end;
  PTextureCache = ^TTextureCache;

  TDynArrayItem_2 = TTextureCache;
  PDynArrayItem_2 = PTextureCache;
  {$define DYNARRAY_2_IS_STRUCT}
  {$define DYNARRAY_2_IS_INIT_FINI_TYPE}
  {$I dynarray_2.inc}
  TDynTextureCacheArray = class(TDynArray_2)
  end;

  { Note that Attributes and State are owned by this record
    (TVRMLOpenGLRendererContextCache will make sure about creating/destroying
    them), but ShapeNode and FogNode are a references somewhere to the scene
    (they will be supplied to TVRMLOpenGLRendererContextCache instance)
    and we don't own them. }
  TShapeStateCache = record
    Attributes: TVRMLRenderingAttributes;
    ShapeNode: TNodeGeneralShape;
    State: TVRMLGraphTraverseState;
    FogNode: TNodeFog;
    FogDistanceScaling: Single;

    GLList: TGLuint;
    References: Cardinal;
  end;
  PShapeStateCache = ^TShapeStateCache;

  TDynArrayItem_3 = TShapeStateCache;
  PDynArrayItem_3 = PShapeStateCache;
  {$define DYNARRAY_3_IS_STRUCT}
  {$I dynarray_3.inc}
  TDynShapeStateCacheArray = class(TDynArray_3)
  end;

  TRenderBeginEndCache = record
    Attributes: TVRMLRenderingAttributes;
    FogNode: TNodeFog;
    FogDistanceScaling: Single;

    GLList: TGLuint;
    References: Cardinal;
  end;
  PRenderBeginEndCache = ^TRenderBeginEndCache;

  TDynArrayItem_4 = TRenderBeginEndCache;
  PDynArrayItem_4 = PRenderBeginEndCache;
  {$define DYNARRAY_4_IS_STRUCT}
  {$I dynarray_4.inc}
  TDynRenderBeginEndCacheArray = class(TDynArray_4)
  end;

  { This is a cache that may be used by many TVRMLOpenGLRenderer
    instances to share some common resources related to this OpenGL
    context.

    For examples, texture names and OpenGL display lists
    for fonts. Such things can usually be shared by all
    TVRMLOpenGLRenderer instances used within the same OpenGL context.
    And this may save a lot of memory if you use many TVRMLOpenGLRenderer
    instances in your program.

    Instance of this class is tied to particular OpenGL context if and only if
    there are some TVRMLOpenGLRenderer instances using this cache and
    tied to that OpenGL context. }
  TVRMLOpenGLRendererContextCache = class
  private
    Fonts: array[TVRMLFontFamily, boolean, boolean] of TGLOutlineFontCache;
    TexturesCaches: TDynTextureCacheArray;
    FUseTextureFileNames: boolean;
    ShapeStateCaches: TDynShapeStateCacheArray;
    RenderBeginCaches: TDynRenderBeginEndCacheArray;
    RenderEndCaches: TDynRenderBeginEndCacheArray;

    procedure Fonts_IncReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
      TTF_Font: PTrueTypeFont);

    procedure Fonts_DecReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);

    { Note that either TextureFileName or TextureNode will be ignored,
      depending on UseTextureFileNames. }
    function Texture_IncReference(
      const TextureImage: TImage;
      const TextureFileName: string;
      const TextureNode: TNodeTexture2;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureWrapS, TextureWrapT: TGLenum;
      const TextureColorModulator: TColorModulatorByteFunc): TGLuint;

    procedure Texture_DecReference(
      const TextureGLName: TGLuint);

    function FogParametersEqual(
      FogNode1: TNodeFog; const FogDistanceScaling1: Single;
      FogNode2: TNodeFog; const FogDistanceScaling2: Single): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { This deteremines how texture comparison with the cache is performed,
      i.e. when Texture_IncReference assumes that we have given texture
      in the cache (and it can be reused) and when it decides that
      we have not (so it must be added to the cache).

      If @true then if the textures come from the same filename
      ("filename" means here combined WWWBasePath + filename field
      of Texture2 VRML node) then the textures are assumed to contain
      the same image. This seems reasonable and is sensible in 99% of most
      common cases.

      However this strategy is actually too relaxed
      (i.e. it can falsely claim that two textures are equal and cache
      may be used, while in fact the cache shouldn't be reused in this case).
      For starters, consider that the actual image could be loaded from
      the filename at various times, and the actual image file could
      change between. Moreover, consider the fact that there is also
      an "image" field of Texture2 node that allows you to write image
      content directly in VRML file. In this case "filename" field
      of Texture2 node may be left empty. The embedded "image" will also
      be used if "filename" field is not empty but we can't load image
      from that file (e.g. file does not exist or invalid format).
      UseTextureFileNames = @true strategy will completely fail in these
      cases, because it compares only "filename".
      See e.g. file like
      /win/3dmodels/vrml/kambi_vrml_examples/inlined_textures.wrl
      that uses various embedded textures.

      That's why another, safer strategy is the default:
      When UseTextureFileNames = @false, texture images are assumed to be
      equal only if they come from the same TNodeTexture2 instance.
      In practice this will usually happen only if these come from
      the actual same VRML file and are the same actual VRML Texture2 node.
      So if you have two shape nodes that simply use the same Texture2 node,
      then OK --- they will reuse the same texture. Obviously one Texture2 node
      will not be loaded more than once to OpenGL texture memory.
      So this is OK.

      But in more elaborate cases, UseTextureFileNames = @true can
      get significant memory savings. Since it doesn't require that
      texture image comes from the same Texture2 node, so moreover
      it doesn't require that it actually comes from the same VRML file.
      All TVRMLFLatSceneGL and TVRMLGLAnimation instances can share
      a single OpenGL texture name. For example, "The Castle" version
      0.6.3 used almost 100 MB memory less (memory use dropped
      from 475 MB to 380 MB) thanks to using UseTextureFileNames := @true.

      You can change this only before you make any texture reference
      to this cache (i.e. before any call to Texture_IncReference,
      that happens within Prepare calls of TVRMLOpenGLRenderer). }
    property UseTextureFileNames: boolean
      read FUseTextureFileNames write FUseTextureFileNames default false;

    { These will be used by TVRMLFlatSceneGL.

      Note that we have two versions of ShapeState_IncReference,
      because if the list will already exist in the cache then we don't want to
      waste time on creating and immediately freeing unnecessary list.
      you should call ShapeState_IncReference_Existing, and if @false
      then you should build display list and call
      ShapeState_IncReference_New. }

    function ShapeState_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AShapeNode: TNodeGeneralShape;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure ShapeState_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AShapeNode: TNodeGeneralShape;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure ShapeState_DecReference(
      const GLList: TGLuint);

    function RenderBegin_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure RenderBegin_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure RenderBegin_DecReference(
      const GLList: TGLuint);

    function RenderEnd_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure RenderEnd_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure RenderEnd_DecReference(
      const GLList: TGLuint);
  end;

  TTextureReference = record
    TextureNode: TNodeTexture2;
    TextureGL: TGLuint;
  end;
  PTextureReference = ^TTextureReference;

  TDynArrayItem_1 = TTextureReference;
  PDynArrayItem_1 = PTextureReference;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynTextureReferenceArray = class(TDynArray_1)
  public
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
      Use always LastGLFreeLight, not FLastGLFreeLight or Attributes.LastGLFreeLight
      to get LastGLFreeLight. LastGLFreeLight function will not ever return -1
      and will minimize amount of calls to glGetInteger() }
    FLastGLFreeLight: integer;
    function LastGLFreeLight: integer;
    TextureReferences: TDynTextureReferenceArray;

    { To which fonts we made a reference in the cache ? }
    FontsReferences: array[TVRMLFontFamily, boolean, boolean] of boolean;

    { ------------------------------------------------------------
      Rzeczy z ktorych mozna korzystac tylko w czasie Render. }

    { kopie aktualnego State i Node na czas Render }
    Render_State: TVRMLGraphTraverseState;
    Render_Node: TNodeGeneralShape;

    { te dwie zmienne sa wewnetrzne dla funkcji MeterialsBegin/End, BindMaterial }
    Render_Material_ForcedLightDisable: boolean;
    Render_Material_BoundMatNum: integer;
    Render_Material_LastFogImmune: boolean;
    procedure Render_MaterialsBegin;
    procedure Render_MaterialsEnd;
    procedure Render_BindMaterial(MatNum: integer);

    { czy Render node'ow musi generowac tex coords ? }
    Render_TexCoordsNeeded: boolean;

    procedure DoBeforeGLVertex(const Vert: TVector3Single);
    procedure DoGLVertex(const Vert: TVector3Single);
    procedure DoGLArrayElement(const Verts: PArray_Vector3Single; ith: TGLint);

    { ----------------------------------------------------------------- }

    {$ifdef USE_VRML_NODES_TRIANGULATION}
    procedure DrawTriangle(const Tri: TTriangle3Single;
      State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape; MatNum: integer);
    {$endif}

    { Inited in RenderBegin, according to our FogNode.
      If not UseFog then it's always false. }
    FogVolumetric: boolean;
    FogEnabled: boolean;
    FogVolumetricDirection: TVector3Single;
    FogVolumetricVisibilityStart: Single;

    FAttributes: TVRMLRenderingAttributes;

    FCache: TVRMLOpenGLRendererContextCache;
    OwnsCache: boolean;
  public
    { Constructor.

      Passing nil as Cache will cause the private cache instance
      to be created and used for this TVRMLOpenGLRenderer.
      I.e. no cache will be shared between different TVRMLOpenGLRenderer
      instances. Otherwise you can pass here your Cache. Of course
      it has to remain created for the whole lifetime while
      this TVRMLOpenGLRenderer is created. }
    constructor Create(AttributesClass: TVRMLRenderingAttributesClass;
      ACache: TVRMLOpenGLRendererContextCache);

    destructor Destroy; override;

    { slowo o atrybutach renderowania
      ktore steruja tym jak bedzie wygladac renderowanie : mozesz je zmieniac
      tylko w momencie gdy renderer
      nie jest przywiazany do zadnego kontekstu OpenGL'a (co jest zwiazane z tym
      ze przywiazanie do danego kontekstu OpenGL'a oznacza takze ze czesc pracy
      z dostosowaniem sie do takich a nie innych atrybutow renderowania
      zostala juz zrobiona) czyli zaraz po wywolaniu konstruktora lub
      UnprepareAll (przed wywolaniem jakiegokolwiek Prepare czy Render*). }
    property Attributes: TVRMLRenderingAttributes read FAttributes;

    property Cache: TVRMLOpenGLRendererContextCache read FCache;

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

    procedure RenderShapeStateBegin(Node: TNodeGeneralShape;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeStateNoTransform(Node: TNodeGeneralShape;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeStateEnd(Node: TNodeGeneralShape;
      State: TVRMLGraphTraverseState);

    procedure RenderShapeState(Node: TNodeGeneralShape;
      State: TVRMLGraphTraverseState);
  end;

  EVRMLOpenGLRenderError = class(EVRMLError);

{$undef read_interface}

implementation

uses NormalsCalculator, Math, Triangulator;

{$define read_implementation}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}

{$I openglmac.inc}

{ TVRMLOpenGLRendererContextCache -------------------------------------------- }

{$define DEBUG_VRML_RENDERER_CACHE}

constructor TVRMLOpenGLRendererContextCache.Create;
begin
  inherited;
  TexturesCaches := TDynTextureCacheArray.Create;
  ShapeStateCaches := TDynShapeStateCacheArray.Create;
  RenderBeginCaches := TDynRenderBeginEndCacheArray.Create;
  RenderEndCaches := TDynRenderBeginEndCacheArray.Create;
end;

destructor TVRMLOpenGLRendererContextCache.Destroy;
var
  fsfam: TVRMLFontFamily;
  fsbold , fsitalic: boolean;
begin
  for fsfam := Low(fsfam) to High(fsfam) do
    for fsbold := Low(boolean) to High(boolean) do
      for fsitalic := Low(boolean) to High(boolean) do
      begin
        Assert(
          (Fonts[fsfam, fsbold, fsitalic].Instance = nil) =
          (Fonts[fsfam, fsbold, fsitalic].References = 0));
        Assert(Fonts[fsfam, fsbold, fsitalic].Instance = nil,
          'Some references to fonts still exist' +
          ' when freeing TVRMLOpenGLRendererContextCache');
      end;

  if TexturesCaches <> nil then
  begin
    Assert(TexturesCaches.Count = 0, 'Some references to textures still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(TexturesCaches);
  end;

  if ShapeStateCaches <> nil then
  begin
    Assert(ShapeStateCaches.Count = 0, 'Some references to ShapeStates still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(ShapeStateCaches);
  end;

  if RenderBeginCaches <> nil then
  begin
    Assert(RenderBeginCaches.Count = 0, 'Some references to RenderBegins still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(RenderBeginCaches);
  end;

  if RenderEndCaches <> nil then
  begin
    Assert(RenderEndCaches.Count = 0, 'Some references to RenderEnds still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(RenderEndCaches);
  end;

  inherited;
end;

procedure TVRMLOpenGLRendererContextCache.Fonts_IncReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
  TTF_Font: PTrueTypeFont);
begin
  Inc(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].Instance = nil then
    Fonts[fsfam, fsbold, fsitalic].Instance := TGLOutlineFont.Create(TTF_Font);
  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Font : ', Fonts[fsfam, fsbold, fsitalic].References);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.Fonts_DecReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);
begin
  Dec(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].References = 0 then
    FreeAndNil(Fonts[fsfam, fsbold, fsitalic].Instance);
  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('-- : Font : ', Fonts[fsfam, fsbold, fsitalic].References);
  {$endif}
end;

function TVRMLOpenGLRendererContextCache.Texture_IncReference(
  const TextureImage: TImage;
  const TextureFileName: string;
  const TextureNode: TNodeTexture2;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureWrapS, TextureWrapT: TGLenum;
  const TextureColorModulator: TColorModulatorByteFunc): TGLuint;
var
  I: Integer;
  TextureCached: PTextureCache;
  ImagesEqual: boolean;
begin
  for I := 0 to TexturesCaches.High do
  begin
    TextureCached := TexturesCaches.Pointers[I];
    if UseTextureFileNames then
      ImagesEqual := TextureCached^.FileName = TextureFileName else
      ImagesEqual := TextureCached^.Node = TextureNode;
    if ImagesEqual and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.WrapS = TextureWrapS) and
       (TextureCached^.WrapT = TextureWrapT) and
       (@TextureCached^.ColorModulator = @TextureColorModulator) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFileName, ' : ', TextureCached^.References);
      {$endif}
      Exit(TextureCached^.GLName);
    end;
  end;

  TexturesCaches.IncLength;
  TextureCached := TexturesCaches.Pointers[TexturesCaches.High];
  TextureCached^.FileName := TextureFileName;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.WrapS := TextureWrapS;
  TextureCached^.WrapT := TextureWrapT;
  TextureCached^.ColorModulator := TextureColorModulator;
  TextureCached^.References := 1;
  TextureCached^.GLName := LoadGLTextureModulated(
    TextureImage, TextureMinFilter, TextureMagFilter,
    TextureWrapS, TextureWrapT, TextureColorModulator);

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ', TextureFileName, ' : ', 1);
  {$endif}
  Exit(TextureCached^.GLName);
end;

procedure TVRMLOpenGLRendererContextCache.Texture_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TexturesCaches.High do
    if TexturesCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TexturesCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ', TexturesCaches.Items[I].FileName, ' : ',
        TexturesCaches.Items[I].References);
      {$endif}
      if TexturesCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TexturesCaches.Items[I].GLName));
        TexturesCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.Texture_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLOpenGLRendererContextCache.FogParametersEqual(
  FogNode1: TNodeFog; const FogDistanceScaling1: Single;
  FogNode2: TNodeFog; const FogDistanceScaling2: Single): boolean;
begin
  Result := (FogNode1 = FogNode2);
  { If both fog nodes are nil, don't compare FogDistanceScaling,
    as they are meaningless. }
  if Result and (FogNode1 <> nil) then
    Result := FogDistanceScaling1 = FogDistanceScaling2;
end;

function TVRMLOpenGLRendererContextCache.ShapeState_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AShapeNode: TNodeGeneralShape;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  SSCache: PShapeStateCache;
begin
  for I := 0 to ShapeStateCaches.High do
  begin
    SSCache := ShapeStateCaches.Pointers[I];
    if (SSCache^.Attributes.Equals(AAttributes)) and
       (SSCache^.ShapeNode = AShapeNode) and
       (SSCache^.State.Equals(AState)) and
       FogParametersEqual(
         SSCache^.FogNode, SSCache^.FogDistanceScaling,
                 AFogNode,         AFogDistanceScaling) then
    begin
      Inc(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ShapeState ', SSCache^.GLList, ' : ', SSCache^.References);
      {$endif}
      AGLList := SSCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.ShapeState_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AShapeNode: TNodeGeneralShape;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  SSCache: PShapeStateCache;
begin
  ShapeStateCaches.IncLength;
  SSCache := ShapeStateCaches.Pointers[ShapeStateCaches.High];
  SSCache^.Attributes := AAttributes;
  SSCache^.ShapeNode := AShapeNode;
  SSCache^.State := AState;
  SSCache^.FogNode := AFogNode;
  SSCache^.FogDistanceScaling := AFogDistanceScaling;
  SSCache^.GLList := AGLList;
  SSCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ShapeState ', SSCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.ShapeState_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  SSCache: PShapeStateCache;
begin
  for I := 0 to ShapeStateCaches.High do
  begin
    SSCache := ShapeStateCaches.Pointers[I];
    if SSCache^.GLList = GLList then
    begin
      Dec(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ShapeState ', SSCache^.GLList, ' : ', SSCache^.References);
      {$endif}
      if SSCache^.References = 0 then
      begin
        FreeAndNil(SSCache^.Attributes);
        FreeAndNil(SSCache^.State);
        glFreeDisplayList(SSCache^.GLList);
        ShapeStateCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.ShapeState_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

function TVRMLOpenGLRendererContextCache.RenderBegin_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderBeginCaches.High do
  begin
    RenderCache := RenderBeginCaches.Pointers[I];
    if (RenderCache^.Attributes.Equals(AAttributes)) and
      FogParametersEqual(
        RenderCache^.FogNode, RenderCache^.FogDistanceScaling,
                    AFogNode,             AFogDistanceScaling) then
    begin
      Inc(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : RenderBegin ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      AGLList := RenderCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.RenderBegin_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  RenderCache: PRenderBeginEndCache;
begin
  RenderBeginCaches.IncLength;
  RenderCache := RenderBeginCaches.Pointers[RenderBeginCaches.High];
  RenderCache^.Attributes := AAttributes;
  RenderCache^.FogNode := AFogNode;
  RenderCache^.FogDistanceScaling := AFogDistanceScaling;
  RenderCache^.GLList := AGLList;
  RenderCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : RenderBegin ', RenderCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.RenderBegin_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderBeginCaches.High do
  begin
    RenderCache := RenderBeginCaches.Pointers[I];
    if RenderCache^.GLList = GLList then
    begin
      Dec(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : RenderBegin ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      if RenderCache^.References = 0 then
      begin
        FreeAndNil(RenderCache^.Attributes);
        glFreeDisplayList(RenderCache^.GLList);
        RenderBeginCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.RenderBegin_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

function TVRMLOpenGLRendererContextCache.RenderEnd_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderEndCaches.High do
  begin
    RenderCache := RenderEndCaches.Pointers[I];
    if (RenderCache^.Attributes.Equals(AAttributes)) and
      FogParametersEqual(
        RenderCache^.FogNode, RenderCache^.FogDistanceScaling,
                    AFogNode,             AFogDistanceScaling) then
    begin
      Inc(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : RenderEnd ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      AGLList := RenderCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.RenderEnd_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  RenderCache: PRenderBeginEndCache;
begin
  RenderEndCaches.IncLength;
  RenderCache := RenderEndCaches.Pointers[RenderEndCaches.High];
  RenderCache^.Attributes := AAttributes;
  RenderCache^.FogNode := AFogNode;
  RenderCache^.FogDistanceScaling := AFogDistanceScaling;
  RenderCache^.GLList := AGLList;
  RenderCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : RenderEnd ', RenderCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.RenderEnd_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderEndCaches.High do
  begin
    RenderCache := RenderEndCaches.Pointers[I];
    if RenderCache^.GLList = GLList then
    begin
      Dec(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : RenderEnd ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      if RenderCache^.References = 0 then
      begin
        FreeAndNil(RenderCache^.Attributes);
        glFreeDisplayList(RenderCache^.GLList);
        RenderEndCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.RenderEnd_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

{ TVRMLRenderingAttributes --------------------------------------------------- }

procedure TVRMLRenderingAttributes.Assign(Source: TPersistent);
begin
  if Source is TVRMLRenderingAttributes then
  begin
    OnBeforeGLVertex := TVRMLRenderingAttributes(Source).OnBeforeGLVertex;
    SmoothShading := TVRMLRenderingAttributes(Source).SmoothShading;
    ColorModulatorSingle := TVRMLRenderingAttributes(Source).ColorModulatorSingle;
    ColorModulatorByte := TVRMLRenderingAttributes(Source).ColorModulatorByte;
    UseLights := TVRMLRenderingAttributes(Source).UseLights;
    FirstGLFreeLight := TVRMLRenderingAttributes(Source).FirstGLFreeLight;
    LastGLFreeLight := TVRMLRenderingAttributes(Source).LastGLFreeLight;
    EnableTextures := TVRMLRenderingAttributes(Source).EnableTextures;
    TextureMinFilter := TVRMLRenderingAttributes(Source).TextureMinFilter;
    TextureMagFilter := TVRMLRenderingAttributes(Source).TextureMagFilter;
    PointSize := TVRMLRenderingAttributes(Source).PointSize;
    UseFog := TVRMLRenderingAttributes(Source).UseFog;
  end else
    inherited;
end;

function TVRMLRenderingAttributes.Equals(SecondValue: TPersistent): boolean;
begin
  Result := (SecondValue is TVRMLRenderingAttributes) and
    (@TVRMLRenderingAttributes(SecondValue).OnBeforeGLVertex = @OnBeforeGLVertex) and
    (TVRMLRenderingAttributes(SecondValue).SmoothShading = SmoothShading) and
    (@TVRMLRenderingAttributes(SecondValue).ColorModulatorSingle = @ColorModulatorSingle) and
    (@TVRMLRenderingAttributes(SecondValue).ColorModulatorByte = @ColorModulatorByte) and
    (TVRMLRenderingAttributes(SecondValue).UseLights = UseLights) and
    (TVRMLRenderingAttributes(SecondValue).FirstGLFreeLight = FirstGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).LastGLFreeLight = LastGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).EnableTextures = EnableTextures) and
    (TVRMLRenderingAttributes(SecondValue).TextureMinFilter = TextureMinFilter) and
    (TVRMLRenderingAttributes(SecondValue).TextureMagFilter = TextureMagFilter) and
    (TVRMLRenderingAttributes(SecondValue).PointSize = PointSize) and
    (TVRMLRenderingAttributes(SecondValue).UseFog = UseFog);
end;

constructor TVRMLRenderingAttributes.Create;
begin
  inherited;

  FSmoothShading := true;
  FUseLights := false;
  FFirstGLFreeLight := 0;
  FLastGLFreeLight := -1;
  FEnableTextures := true;
  FTextureMinFilter := GL_LINEAR;
  FTextureMagFilter := GL_LINEAR;
  FPointSize := 3;
  FUseFog := true;
end;

procedure TVRMLRenderingAttributes.SetOnBeforeGLVertex(
  const Value: TBeforeGLVertexProc);
begin
  FOnBeforeGLVertex := Value;
end;

procedure TVRMLRenderingAttributes.SetSmoothShading(const Value: boolean);
begin
  FSmoothShading := Value;
end;

procedure TVRMLRenderingAttributes.SetColorModulatorSingle(
  const Value: TColorModulatorSingleFunc);
begin
  FColorModulatorSingle := Value;
end;

procedure TVRMLRenderingAttributes.SetColorModulatorByte(
  const Value: TColorModulatorByteFunc);
begin
  FColorModulatorByte := Value;
end;

procedure TVRMLRenderingAttributes.SetUseLights(const Value: boolean);
begin
  FUseLights := Value;
end;

procedure TVRMLRenderingAttributes.SetFirstGLFreeLight(const Value: integer);
begin
  FFirstGLFreeLight := Value;
end;

procedure TVRMLRenderingAttributes.SetLastGLFreeLight(const Value: integer);
begin
  FLastGLFreeLight := Value;
end;

procedure TVRMLRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  FEnableTextures := Value;
end;

procedure TVRMLRenderingAttributes.SetTextureMinFilter(const Value: TGLint);
begin
  FTextureMinFilter := Value;
end;

procedure TVRMLRenderingAttributes.SetTextureMagFilter(const Value: TGLint);
begin
  FTextureMagFilter := Value;
end;

procedure TVRMLRenderingAttributes.SetPointSize(const Value: integer);
begin
  FPointSize := Value;
end;

procedure TVRMLRenderingAttributes.SetUseFog(const Value: boolean);
begin
  FUseFog := Value;
end;

function TVRMLRenderingAttributes.SmoothNormalsGLU: TGLenum;
begin
  if SmoothShading then
    Result := GLU_SMOOTH else
    Result := GLU_FLAT;
end;

function TVRMLRenderingAttributes.ColorModulated(
  const Color: TVector3Single): TVector3Single;
begin
  if Assigned(ColorModulatorSingle) then
    Result := ColorModulatorSingle(Color) else
    Result := Color;
end;

function TVRMLRenderingAttributes.ColorModulated(
  const Color: TVector3Byte): TVector3Byte;
begin
  if Assigned(ColorModulatorByte) then
    Result := ColorModulatorByte(Color) else
    Result := Color;
end;

{ TDynTextureReferenceArray ---------------------------------------------------- }

function TDynTextureReferenceArray.TextureNodeIndex(TexNode: TNodeTexture2): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].TextureNode = TexNode then exit;
 result := -1;
end;

{ TVRMLOpenGLRenderer ---------------------------------------------------------- }

constructor TVRMLOpenGLRenderer.Create(
  AttributesClass: TVRMLRenderingAttributesClass;
  ACache: TVRMLOpenGLRendererContextCache);
begin
  inherited Create;

  { This is something different than FAttributes.FLastGLFreeLight.
    See LastGLFreeLight function. }
  FLastGLFreeLight := -1;

  FAttributes := AttributesClass.Create;
  TextureReferences := TDynTextureReferenceArray.Create;

  OwnsCache := ACache = nil;
  if OwnsCache then
    FCache := TVRMLOpenGLRendererContextCache.Create else
    FCache := ACache;
end;

destructor TVRMLOpenGLRenderer.Destroy;
begin
  UnprepareAll;
  TextureReferences.Free;
  FreeAndNil(FAttributes);

  if OwnsCache then
    FreeAndNil(FCache);

  inherited;
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

procedure TVRMLOpenGLRenderer.Prepare(State: TVRMLGraphTraverseState);
const
  TexWrapEnumToGL: array[TEXWRAP_REPEAT..TEXWRAP_CLAMP]of TGLenum =
  (GL_REPEAT, GL_CLAMP);
var
  fsfam: TVRMLFontFamily;
  fsbold, fsitalic: boolean;
  TextureReference: TTextureReference;
  TextureFileName: string;
  TextureNode: TNodeTexture2;
begin
 {przygotuj font dla LastFontStyle}
 fsfam := State.LastNodes.FontStyle.FdFamily.Value;
 fsbold := State.LastNodes.FontStyle.FdStyle.Flags[FSSTYLE_BOLD];
 fsitalic := State.LastNodes.FontStyle.FdStyle.Flags[FSSTYLE_ITALIC];

 if not FontsReferences[fsfam, fsbold, fsitalic] then
 begin
   Cache.Fonts_IncReference(fsfam, fsbold, fsitalic,
     State.LastNodes.FontStyle.TTF_Font);
   FontsReferences[fsfam, fsbold, fsitalic] := true;
 end;

 {przygotuj teksture dla LastTexture2}
 TextureNode := State.LastNodes.Texture2;
 if (TextureReferences.TextureNodeIndex(TextureNode) = -1) then
 begin
  if TextureNode.IsTextureImage then
  begin
   TextureReference.TextureNode := TextureNode;

   TextureFileName := TextureNode.FdFileName.Value;
   if TextureFileName <> '' then
     TextureFileName := TextureNode.PathFromWWWBasePath(TextureFileName);

   TextureReference.TextureGL := Cache.Texture_IncReference(
     TextureNode.TextureImage,
     TextureFileName,
     TextureNode,
     Attributes.TextureMinFilter,
     Attributes.TextureMagFilter,
     TexWrapEnumToGL[TextureNode.FdWrapS.Value],
     TexWrapEnumToGL[TextureNode.FdWrapT.Value],
     Attributes.ColorModulatorByte);

   TextureReferences.AppendItem(TextureReference);
  end;
 end;
end;

procedure TVRMLOpenGLRenderer.Unprepare(Node: TVRMLNode);
var i: integer;
begin
 {zwracam uwage ze nie niszczymy tu fontow dla (LastNode is TNodeFontStyle)
  bo fonty sa gromadzone w tablicy Cache.Fonts i wszystkie Font Styles
  o takich samych wlasciwosciach Family i Style korzystaja zawsze z tego
  samego juz utworzonego fontu.}

 {niszczymy teksture}
 if Node is TNodeTexture2 then
 begin
  i := TextureReferences.TextureNodeIndex(TNodeTexture2(Node));
  if i >= 0 then
  begin
   Cache.Texture_DecReference(TextureReferences.Items[i].TextureGL);
   TextureReferences.Delete(i, 1);
  end;
 end;
end;

procedure TVRMLOpenGLRenderer.UnprepareAll;
var
  fsfam: TVRMLFontFamily;
  fsbold , fsitalic: boolean;
  i: integer;
begin
  FLastGLFreeLight := -1;

  {niszcz fonty}
  for fsfam := Low(fsfam) to High(fsfam) do
    for fsbold := Low(boolean) to High(boolean) do
      for fsitalic := Low(boolean) to High(boolean) do
        if FontsReferences[fsfam, fsbold, fsitalic] then
        begin
          FontsReferences[fsfam, fsbold, fsitalic] := false;
          Cache.Fonts_DecReference(fsfam, fsbold, fsitalic);
        end;

  {niszcz wszystkie tekstury}
  for i := 0 to TextureReferences.Count-1 do
    Cache.Texture_DecReference(TextureReferences.Items[i].TextureGL);
  TextureReferences.SetLength(0);
end;

function TVRMLOpenGLRenderer.LastGLFreeLight: integer;
begin
 if FLastGLFreeLight = -1 then
 begin
  {jezeli jeszcze nie pobrane FLastGLFreeLight to pobierz je teraz:}
  if Attributes.LastGLFreeLight = -1 then
   FLastGLFreeLight := glGetInteger(GL_MAX_LIGHTS)-1 else
   FLastGLFreeLight := Attributes.LastGLFreeLight;
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
   FogVolumetric := false;
   FogEnabled := false;
   if not Attributes.UseFog then Exit;

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

   FogEnabled := true;

   FogVisibilityRangeScaled :=
     FogNode.FdVisibilityRange.Value * FogDistanceScaling;

   FogVolumetric := FogNode.FdVolumetric.Value and GL_EXT_fog_coord;

   if FogNode.FdVolumetric.Value and (not GL_EXT_fog_coord) then
   begin
     { Then we will use normal fog, and try to keep (as much as possible..)
       the similar fog look. I found that enlarging FogVisibilityRangeScaled
       below and just letting other things to work as usual gives
       acceptable results. }
     FogVisibilityRangeScaled := FogVisibilityRangeScaled * 5;
   end;

   if FogVolumetric then
   begin
     FogVolumetricVisibilityStart :=
       FogNode.FdVolumetricVisibilityStart.Value * FogDistanceScaling;
     FogVolumetricDirection :=
       FogNode.FdVolumetricDirection.Value;
     glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
   end else
   begin
     { If not FogVolumetric but still GL_EXT_fog_coord, we make sure
       that we're *not* using FogCoord below. }
     if GL_EXT_fog_coord then
       glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
   end;

   glEnable(GL_FOG);
   glFogv(GL_FOG_COLOR,
     Vector4Single(Attributes.ColorModulated(FogNode.FdColor.Value), 1.0));
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
 glPointSize(Attributes.PointSize);
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

 if Attributes.SmoothShading then
  glShadeModel(GL_SMOOTH) else
  glShadeModel(GL_FLAT);

 if Attributes.UseLights then
   for i := Attributes.FirstGLFreeLight to LastGLFreeLight do
     glDisable(GL_LIGHT0+i);

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

procedure TVRMLOpenGLRenderer.RenderShapeStateBegin(
  Node: TNodeGeneralShape;
  State: TVRMLGraphTraverseState);
begin
  {LogWrite('Rendering ' +Node.NodeTypeName+ ' named ' +Node.NodeName);}

  {zrob nasze kopie}
  Render_State := State;
  Render_Node := Node;

  glMatrixMode(GL_TEXTURE); glLoadMatrix(State.CurrTextureMatrix);
  glMatrixMode(GL_MODELVIEW);

  { uwzglednij atrybut Attributes.UseLights : jezeli jest = true to zdefiniuj
    OpenGLowi wszystkie State.ActiveLights. Robimy to PRZED zaladowaniem
    transformacji State.CurrMatrix (bo swiatla maja wlasne CurrMatrix i
    nie podlegaja transformacji aktualnego State'a w ktorym sa) }
  if Attributes.UseLights then
    glLightsFromVRML(State.ActiveLights,
      Attributes.FirstGLFreeLight, LastGLFreeLight,
      Attributes.ColorModulatorSingle);

  glPushMatrix;
    glMultMatrix(State.CurrMatrix);
end;

procedure TVRMLOpenGLRenderer.RenderShapeStateNoTransform(
  Node: TNodeGeneralShape;
  State: TVRMLGraphTraverseState);
var
  { jaki font jest aktualny (na podstawie State.LastNodes.FontStyle) }
  CurrentFont: TGLOutlineFont;

  {$I VRMLOpenGLRenderer_Render_SpecificNodes.inc}

var
  IndexedRenderer: TGeneralIndexedRenderer;
  TextureReferencesIndex: Integer;
begin
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
  if (State.LastNodes.Texture2.IsTextureImage) and
    Attributes.EnableTextures then
  begin
   SetGLEnabled(GL_ALPHA_TEST,
     State.LastNodes.Texture2.TextureImage is TAlphaImage);
   glEnable(GL_TEXTURE_2D);
   TextureReferencesIndex := TextureReferences.TextureNodeIndex(
     State.LastNodes.Texture2);
   Assert(TextureReferencesIndex <> -1,
     'You''re calling TVRMLOpenGLRenderer.Render with a State ' +
     'that was not passed to TVRMLOpenGLRenderer.Prepare. ' +
     'You must call TVRMLOpenGLRenderer.Prepare first');
   glBindTexture(GL_TEXTURE_2D,
     TextureReferences.Items[TextureReferencesIndex].TextureGL);
   Render_TexCoordsNeeded := true;
  end else
  begin
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_ALPHA_TEST);
   Render_TexCoordsNeeded := false;
  end;

  with State.LastNodes.FontStyle do
    CurrentFont := Cache.Fonts[FdFamily.Value, FdStyle.Flags[FSSTYLE_BOLD],
      FdStyle.Flags[FSSTYLE_ITALIC]].Instance;

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
end;

procedure TVRMLOpenGLRenderer.RenderShapeStateEnd(
  Node: TNodeGeneralShape;
  State: TVRMLGraphTraverseState);
begin
  glPopMatrix;
end;

procedure TVRMLOpenGLRenderer.RenderShapeState(
  Node: TNodeGeneralShape;
  State: TVRMLGraphTraverseState);
begin
  RenderShapeStateBegin(Node, State);
  try
    RenderShapeStateNoTransform(Node, State);
  finally
    RenderShapeStateEnd(Node, State);
  end;
end;
{$endif}

end.
