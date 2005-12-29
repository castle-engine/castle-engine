{
  Copyright 2002-2005 Michalis Kamburelis.

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

(*
  @abstract(Ten modul definiuje wezly VRML'a. Jako ze caly plik VRML'a
  to po prostu wezel VRML'a wiec ten modul definiuje wszystko
  co trzeba zeby reprezentowac w pamieci cala strukture sceny
  VRML'a.)

  Sa tu metody do odczytu sceny ze strumienia
  pomagajac sobie lekserem VRML'a (w VRMLLexer) i parserem
  pol VRML'a (w VRMLFields). Takze metody do zapisu wezlow
  do strumienia.

  I przede wszystkim metody do przeszukiwania grafu VRML'a na rozne sposoby -
  patrz metody Traverse, EnumNodes i FindNode. Szczegolnie metoda Traverse
  jest wazna : pozwala ona zamienic graf VRMl'a na liste par
  (Node typu TGeneralShapeNode + State : TVRMLGraphTraverseState).
  Renderowanie takiej listy sprowadza sie teraz do wyrenderowania
  kazdej takiej pary, czyli kazdego shape'u z odpowiednimi ustawieniami
  State. To jest cala robota jaka musi wykonac renderer taki jak np.
  OpenGLRenderer w VRMLOpenGLRenderer (alternatywnie, renderer moze uzywac
  tez Triangulate).

  Oraz kazdy node ma zdefiniowane swoje pola i wiele node'ow
  maja zaimplementowane pewne ogolne metody do operowania
  na nich (ladowanie zawartosci w node'ach WWWInline i Texture2,
  funkcja MatrixTransformation w podklasach GeneralTransformation itp.)
  Node'y z grupy GeneralShape maja kilka istotnych metod jak
  [Local]BoudingBox, Vertices/TrianglesCount, Triangulate.

  Ten modul naturalnie nie zalezy od OpenGL'a. Tak miedzy nami to kiedys
  zalezal od OpenGL'a i w ogole bylo tu zaimplementowane mnostwo
  innych rzeczy, m.in. jadro renderera OpenGL'owego i bylo tu naprawde tloczno.
  Ale teraz juz jest czysto. I jestesmy kompletnie niezalezni od renderera,
  co jest wazne bo renderer przez OpenGL'a nie jest juz jedynym mozliwym
  rendererem - mamy juz VRMLRayTracer.

  Kompatybilnosc z roznymi wersjami VRML'a :
  - Elementarne jest aby KAZDY poprawny plik VRML'a 1.0 byl poprawnie
    odczytany. Kazdy prawidlowy a nawet pare nieprawidlowych plikow
    w VRMl'u 1.0 zostanie tu dobrze odczytanych.

  - Zgodnosc z Inventorem 1.0 : no coz, oblugujemy Inventora 1.0 tak jakby
    byl VRML'em 1.0. Obslugujemy tez pare specyficznych rzeczy z
    Inventora 1.0 ktorych nie ma w VRML'u a ktore czesto sie pojawiaja
    w plikach .iv :
    ) pole "hints" node'a ShapeHints
    ) node IndexedTriangleMesh

    Mechanizm TNodeUnknown pozwala omijac parserowi nawet kompletnie
    nieznane node'y pozbawione pol "fields" i "isA" (z VRML'a 1.0
    extensibility features). Dlatego jestesmy w stanie odczytac i
    wyswietlic satysfakcjonujaca czesc wiekszosci plikow Inventora
    jakie mialem w /usr/share/inventor/demos/. Super !

  - W tej chwili o VRML'a 97 troszczy sie tylko lekser,
    a i to jeszcze nieskonczone, wiec zaden plik w VRML'u 97 nie bedzie
    odczytany i zrozumiany. sorry.

    Chociaz w wielu miejscach uzywam specyfikacji VRMLa 97
    - zeby ustalic rzeczy zdefiniowane w niejasny sposob w specyfikacji
      VRML 1.0,
    - zeby pododawac do VRMLa 1.0 male drobiazgi z VRMLa 97, jak
      attenuation swiatel.
    - VRMLRayTracer uzywa modelu oswietlenia zdefiniowanego w specyfikacji
      VRMLa 97

  Notka do mechanizmu DEF/USE : gdy uzywamy DEF nie mozemy odwolac
  sie do nazwy node'a ktory aktualnie parsujemy (osiagamy to
  po prostu dodajac nazwe node do NodeNameBinding dopiero PO
  sparsowaniu node'a). W ten sposob zapewniamy sobie ze graf VRML'a
  nie moze zawierac cykli i jestesmy szczesliwi.

  Notka do mechanizmu wczytywania grafu VRMLa : jak widac nie robimy
  dekonstrukcji w czasie odczytywania sceny - co znaczy tyle ze
  po odczytaniu calego strumienia jestesmy w stanie zapisac z
  powrotem do innego strumienia cala scene VRMLa, nie tracac zadnej
  informacji. O ile oczywiscie na renderowanie i w ogole wiekszosc operacji
  mozna patrzec jako na jakis rodzaj dekonstrukcji to my zawsze zostajemy
  w posiadaniu calej informacji o scenie.
  Sa dwa wyjatki :
  1) node WWWInline ktory w BeforeTraverse laduje swoja scene jako swoje dziecko
  2) node'y moga (w niezdefiniowanym momencie) poprawiac wartosci
     swoich pol jesli te sa w oczywisty sposob nieprawidlowe i
     bezsensowne. Staram sie przy tym uznawac mozliwie duzo wartosci
     za sensowne (np. specyfikacja VRMLa nie dopuszcza Cylindra ktory
     nie ma zadnej czesci wlaczonej, ale ja to dopuszczam) wiec jezeli program
     i scena sa dobrze napisane to taka sytuacja nie powinna nigdy zajsc.
  3) w przypadku scen o wielu root node'ach (ktore sa de facto niepoprawne
     trzymajac sie sciscle specyfikacji VRMLa 1.0, chociaz sa poprawne w
     VRMLu 97 i ja je dopuszczam takze w VRMLu 1.0, patrz nizej) jezeli
     root node'ow w pliku byloby wiele to jako root node tworzymy sobie
     node Group i w nim umieszczamy wszystkie root nodes.
     (jest to zaimplementowane w ParseVRMLFile. Jest to chyba najbardziej
     sensowny sposob w jaki mozna to zrobic - w programie bedziemy chcieli
     przeciez reprezentowac model VRMLa jako jeden obiekt; wiec nalezaloby
     uzyc TVRMLNodesList, ale wtedy musielibysmy powtorzyc implementacje
     wielu rzeczy w TVRMLNode takze dla takiej listy; wiec tutaj pomysl:
     przeciez klasa TNodeGroup jest wlasnie taka prosta lista node'ow.)
  Takie unikanie dekonstrukcji pozwoli nam
  1) na unikniecie zbytniego przywiazania naszego kodu VRMLa do konkretnych
     zastosowan. Poniewaz mamy cala informacje o scenie mozemy zrobic
     wszystko co mozemy zrobic ze scena VRMLa - co nie byloby mozliwe gdybysmy
     w czasie dekonstrukcji (np. wykonujac juz w czasie odczytu wszystkie
     transformacje na macierzy i transformujac punkty) tracili jakas czesc
     informacji.
  2) no i mozemy w ten sposob latwo wykorzystac nasz kod VRMLa do pisania
     konwerterow innych formatow na VRMLa. Uzywajac modulu Object3dAsVRML
     i tutejszego SaveToVRMLFile mamy sliczny konwerter 3ds, obj, geo -> VRML.

  Specyfikacja VRMLa 1.0 z dodanymi "moimi rozszerzeniami VMRLa"
  [http://www.camelot.homedns.org/~michalis/kambi-vrml.php] stanowia
  uzasadnienie dla wielu rzeczy ktore robimy w tym module.

  Node'y o nazwach *General* to nie sa koncowe klasy node'ow,
  to tylko klasy posrednie jak GeneralShape, GeneralLight,
  GeneralCamera, GeneralTraformation itp. Te klasy pozwalaja
  nam zaimplementowac jakas funkcjonalnosc dla kilku podobnych
  klas jednoczesnie, te klasy buduja tez ladne drzewko
  zaleznosci obiektow dzieki czemu np. w EnumNodes mozemy
  jako parametr podac TNodeGeneralLights aby znalezc wszystkie
  swiatla na scenie.

  24 sierpnia 2003: znaczne osiagniecie : wyeliminowalem typ TVRMLNodeKind
    ktory wyliczal mi wszystkie istniajace klasy node'ow. Dzieki temu
    typowi moglem latwo robic powiazania w stylu "dana nazwa wezla VRMLa
    odpowiada jakiej podklasie TVRMLNode ?" : taki typ (razem z kilkoma
    stalymi) przechowywal mi informacje o wszystkich istniejacych
    koncowych podklasach TVRMLNode. Wada tego jednak byl fakt ze wszystkie
    wezly VRMLa musialy byc zdefiniowane w tym jednym module. Nie mozna
    bylo np. w malfunction zaimplementowac specjalnych node'ow w stylu
    "MalfunctionLevel { fog TRUE }" do uzytku tylko przez malfunction.
    To znaczy nie bylo praktycznie sensu definiowac potomkow typu
    TVRMLNode w innych modulach niz VRMLNodes bo nawet jesli mogles zdefiniowac
    podklasy TVRMLNode w innych modulach to i tak nie mogles ich uzyc
    (bo zeby one byly widziane przez mechanizm odwzorowywania
    "nazwa VRMLa -> podklasa TVRMLNode" musialy byc dodane do odpowiednich
    stalych w TYM module...). Usunalem ta niedogodnosc eliminujac zupelnie
    typ TVRMLNodeKind. Teraz nie ma zadnej stalej tablicy przechowujacej
    jakies informacje na temat "wszystkich dostepnych node'ow" - ekwiwalentem
    tego jest obiekt NodesManager w ktorym mozna w czasie wykonania programu
    rejestrowac dowolne stworzone podklasy TVRMLNode (prawdopodobnie najsensowniej
    jest robic to w sekcji initialization modulu; uzywaj tego podobnie jak
    Picture.RegisterPictureClass z Graphics z VCLa Borlanda.).
  Ciagle pozostaje niestety ograniczenie ze wszystkie wezly ktore maja byc
    uwzgledniane w tablicy TVRMLGraphTraverseState.LastNodes musza byc
    zadeklarowane w tym module. To powoduje ze ciagle nie mozemy przerzucic
    implementacji wszystkich specyficznych node'ow do jakiegos osobnego modulu.
    Ktoregos dnia, gdy ograniczenie to zacznie mi przeszkadzac, zapewne
    to zaimplementuje (tablica TraverseStateLastNodesClasses bedzie musiala
    byc wtedy zrobiona jako dynamiczna lista klas TVRMLNodeClass; problemem
    jest tutaj ze TTraverseStateLastNodes bedzie wtedy takze musialo
    byc struktura dynamiczna i proste zapytania LastNodes.Coordinate3 bedzie
    musialo byc zastapione na cos mniej wygodnego i badziej czasochlonnego
    podczas wykonywania : LastNodes.NodesFind[TNodeCoordinate3].)
  Aby wykazac ze to rzeczywiscie dziala (i to dziala calkiem fajnie) zapisalem
    w malfunction w LevelUnit cztery node'y specyficzne dla malfunction :
    MalfunctionLevelInfo, Malfunction(NotMoving|CircleMoving|Hunting)Enemy.
    Teraz definicje leveli malfunction nie potrzebuja ZADNYCH specjalnie
    parsowanych wezlow Info. Zalety: parser+lekser VRMLa od razu robia
    mi mnostwo checkow i podaja moim node'ow gotowy wynik. Kod parsowania
    zawartosci wezlow Info byl bardzo prosty, mimo to teraz nie musimy
    go juz w ogole pisac - wszystko zrzucamy na leksera+parsera VRMLa.
    Ponadto nasze node'y dzialaja od razu w ramach wezlow VRMLa, a wiec
    np. nie trzeba sobie wszedzie pisac co sie stanie jesli nie podasz tego
    pola. W VRMLu jesli nie podasz pola to zostanie uzyta jego wartosc domyslna,
    wystarczy powiedziec ile ona wynosi. Trzeba przyznac ze do tak prostych
    zadan jak levele malfunction definiowanie wlasnych wezlow VRMLa zapewnia
    duza elegancje w kodzie ale nie daje az tak wiele realnych zyskow.

  Examples of defining your own VRML node types (without modifying
  sources of this unit, or any other unit) are in these programs:
  - bezier_curves (VRMLBezierCurve)
  - malfunction (LevelUnit)

  Co do zapisywania VRMLa :
  - kazde pole zapisuje 1 lub wiecej calych linii
  - kazdy node zapisuje najpierw linie [DEF NodeName] NodeKindName {
    potem swoje pola
    potem swoje subnode'y
    potem linie }
    zmieniajac Indent o IndentIncrement zdefiniowane w VRMLFields
  - linie sa konczone przez nl
  - mechanizm DEF/USE pol jest zapisywany dobrze, tzn. jezeli
    jakies pole jest obecne wiecej niz raz w drzewie VRML'a to jest
    zapisywane do pliku tylko raz, kazdy nastepny zapis to tylko
    zapisanie 'USE <NodeName>'
  - w wiekszosci przypadkow pola o wartosciach domyslnych nie sa zapisywane.
    W zasadzie zapisywanie pol o wartosciach domyslnych nie byloby bledem,
    choc mogloby denerwowac userow. Ale my naprawde tego potrzebujemy ze
    wzgledu na male rozszerzenia VRMLa 1.0 jakie tu zaimplementowalem
    (patrz wyzej). Jesli chcemy zapisywac poprawne pliki VRMLa to nie mozemy
    dopuszczac zeby np. eksport pliku 3DS na VRMLa dodawal jakies pole
    "mirror" do kazdego Materialu. Wiec nie bedziemy zapisywac "mirror" kiedy
    "mirror" = [0.0] i w ten sposob rozwiazujemy problem. Jednoczesnie,
    jesli ktos rzeczywiscie stworzyl plik podajac wlasciwosc "mirror"
    rozna od domyslnej to uwzglednimy wszedzie to pole i zapiszemy je w
    razie potrzeby z powrotem do pliku VRMLa.
*)

unit VRMLNodes;


{
  TODO: make sure docs look good in pasdoc
  TODO: translate docs to English
}

{
  Known problems:
  - MFString field with strings not enclosed in double quotes will
    not be parsed corectly. Moreover, parsing SFStrings not enclosed
    in double quotes is implemented rather as a "quick & dirty hack"
    than as a nice solution. Really, it's a weird "feature" of
    VRML 1.0 (eliminated in VRML 97) to allow strings not enclosed
    in double quotes.
    And I know about only ONE program that utilizes it (Blender)
    and this program uses it only in SFString field (Texture2.filename).
    So I doubt I will ever fix this -- I would consider it a waste of time,
    since not enclosing strings in double quotes is something totally
    useless.

  Sorry (TODO list):
  - Of course VRML 97
}

interface

uses VectorMath, Classes, SysUtils, VRMLLexer, KambiUtils, KambiClassUtils,
  VRMLFields, Boxes3d, Images, TTFontsTypes, BackgroundBase;

{$define read_interface}

const
  { }
  CountTraverseStateLastNodes = 10;
  HighTraverseStateLastNodes = CountTraverseStateLastNodes - 1;

{ -----------------------------------------------------------------------------
  dluuuga deklaracja "type" w ktorej wiele rzeczy jest zdefiniowanych
  wzajemnie (rekurencyjnie). }

type
  {forward declarations}
  TVRMLNodesList = class;
  TNodeGeneralLightsList = class;
  TVRMLNode = class;
  TNodeCoordinate3 = class;
  TNodeShapeHints = class;
  TNodeFontStyle = class;
  TNodeMaterial = class;
  TNodeMaterialBinding = class;
  TNodeNormal = class;
  TNodeNormalBinding = class;
  TNodeTexture2 = class;
  TNodeTextureCoordinate2 = class;
  TNodeGeneralShape = class;
  TNodeGeneralLight = class;
  TNodeKambiTriangulation = class;

  TVRMLNodeClass = class of TVRMLNode;

  TVRMLNodeProc = procedure (node: TVRMLNode) of object;

  { Stala TraverseStateLastNodesClasses okresla jakie node'y beda zapamietywane
    w TVRMLGraphTraverseState w LastNodes. TTraverseStateLastNodes to wlasnie
    typ dla LastNodes, ktory dzieki wariantom pozwala zarowno iterowac
    po swojej zawartosci (przez Nodes[]) jak i odwolywac sie do tej zawartosci
    przez odpowiednie nazwy ktore juz maja odpowiednie typy
    (np. zamiast "Rekord.TNodeCoordinate3(Nodes[0])" wystarczy
    "Rekord.Coordinate3")  }
  TTraverseStateLastNodes = record
    case Integer of
      0: ( Nodes: array[0..HighTraverseStateLastNodes]of TVRMLNode; );
      1: ( Coordinate3 :TNodeCoordinate3;
           ShapeHints :TNodeShapeHints;
           FontStyle :TNodeFontStyle;
           Material :TNodeMaterial;
           MaterialBinding :TNodeMaterialBinding;
           Normal :TNodeNormal;
           NormalBinding :TNodeNormalBinding;
           Texture2 :TNodeTexture2;
           TextureCoordinate2 :TNodeTextureCoordinate2;
           KambiTriangulation: TNodeKambiTriangulation;
           { additions here must be synchronized with additions to
             TraverseStateLastNodesClasses }
         );
  end;

  { rekord do zapamietywania w TVRMLGraphTraverseState swiatla.
    Kazde swiatlo to jeden node z grupy general light i jego tranformacja -
    rzeczy takie jak pozycja i kierunek, swiatla sa modyfikowane przez ta
    transformacje.

    TransfLocation to juz przeliczone Transformation*Location dla swiatel
    TNodeGenarlLightWLocation. TransfNormDirection to juz
    znormalizowane i transformowane Direction dla swiatel Directional i Spot
    (jest transformowane tak zeby przesuniecia nie mialy znaczenia,
    licza sie tylko obroty i skalowania).

    sorry - TransfLocation/Direction nie jest zbyt eleganckim rozwiazaniem

    NIGDY nie konstruuj tego rekordu recznie - on moze byc modyfikowany tylko
    w TDynActiveLightArray.AddLight  }
  TActiveLight = record
    LightNode: TNodeGeneralLight;
    Transform: TMatrix4Single;
    TransfLocation: TVector3Single;
    TransfNormDirection: TVector3Single;
  end;
  PActiveLight = ^TActiveLight;

  TDynArrayItem_1 = TActiveLight;
  PDynArrayItem_1 = PActiveLight;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I DynArray_1.inc}
  TDynActiveLightArray = class(TDynArray_1)
    { -1 jesli nie ma }
    function IndexOfLightNode(LightNode: TNodeGeneralLight): integer;
    procedure AddLight(ALightNode: TNodeGeneralLight; const ATransform: TMatrix4Single);
  end;
  TArray_ActiveLight = TInfiniteArray_1;
  PArray_ActiveLight = PInfiniteArray_1;

  { ponizsza klasa TVRMLGraphTraversalState definiuje "stan" w czasie
    przechodzenia sceny VRML'a.

    jedyne pole ktore pamieta aktualna transformacje to CurrMatrix,
    wiec wiadomo jak zaimplementowac np. TransformSeparator.

    Node'y ktore trafiaja na liste LastNodes (bo sa wsrod
    TraverseStateLastNodesClasses) nie moga wplywac w zaden inny sposob na stan
    tej klasy podczas wykonywania Traverse w Before/Middle/After Traverse.
    Innymi slowy, gwarantuje sie ze zmiana wartosci na jakims polu jkaiegos
    node'a z grupy LastNodesKinds nie wplynie w zaden sposob na sposob
    w jaki powinien byc renderowany shape ktorego State nie zawiera
    danego LastNode'a. Korzystam z tego w TFLatScene.ChangedFields.

    Podobnie swiatla trafiaja na ActiveLights i w zaden inny sposob nie moga
    zmieniac State trawersowania. }
  TVRMLGraphTraverseState = class
  private
    FLastNodes: TTraverseStateLastNodes;
    procedure CommonCreate;
  public
    { nie, ParentsCount elementow Last* NIE odzwierciedla faktu ze sa one
      podlegle TVRMLRenderState. W ogole ten obiekt nie zajmuje sie
      zarzadzaniem tymi polami - on tylko przechowuje sobie ich wartosci.
      W szczegolnosci wiec gwarantowane jest ze obiekty nigdy nie beda nil
      ale ta gwarancja musi byc takze zapewniona przez kod ktory tworzy
      ten obiekt przez Create. }
    property LastNodes: TTraverseStateLastNodes read FLastNodes;

    ActiveLights: TDynActiveLightArray;

    CurrMatrix: TMatrix4Single;
    CurrTextureMatrix: TMatrix4Single;

    constructor CreateCopy(Source: TVRMLGraphTraverseState);
    constructor Create(const ADefaultLastNodes: TTraverseStateLastNodes);
    destructor Destroy; override;
  end;

  TTraversingFunc = procedure (node: TVRMLNode; State: TVRMLGraphTraverseState) of object;

  TNewTriangleProc = procedure (const Tri: TTriangle3Single;
    State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
    MatNum: integer) of object;

  TVRMLNode = class
  private
    fNodeName: string;
    FWWWBasePath: string;
  private
    FChildren, FParents: TVRMLNodesList;
    function GetChildrenItem(i: integer): TVRMLNode;
    function GetParentsItem(i: integer): TVRMLNode;
  protected
    fAllowedChildren: boolean;
    fParsingAllowedChildren: boolean;

    { za kazdym razem gdy chcemy przechodzic graf VRMLa Traverse
      iteruja po dzieciach o indeksach od FirstChild do LastChild,
      gdzie zmienne FirstChild i LastChild zostaly otrzymane wywolujac
      ChildrenToEnter.

      Domyslnie zwracane jest 0, ChildrenCount-1 a wiec bedzie wchodzil
      we wszystkie children. Pokryj ta metode jesli z danego node'a
      nie ma wchodzic w children - np. dla Switcha (ktory wybiera jedno/
      wszystkie/zadne dziecko na podstawie swojego pola) lub LODa
      (ktory wybiera jedno sposrod swoich dzieci na podstawie swojego
      pola i pozcji viewera).

      Iteracja bedzie robiona instrukcja
        "for i := max(FirstChild, 0) to min(LastChild, ChildrenCount-1)"
      (lub czyms rownowaznym, naturalnie) wiec jezeli FirstChild > LastChild
      lub FirstChild > ChildrenCount-1 lub LastChild < 0 to nie
      bedziemy wchodzic w zadne children. }
    procedure ChildrenToEnter(var FirstChild, LastChild: integer); virtual;

    { w tej klasie te metody nie nie robia, w podklasach mozna za ich
      pomoca zmodyfikowac nieco zachowanie state'a podczas przechodzenia
      grafu. BeforeTraverse MOZE podmienic State na inny, tylko musi
      go pozniej przywrocic w AfterTraverse. (to jest uzywane w Separatorze).
      PAMIETAJ wywolywac inherited - w Before i Middle Traverse inherited
      powinno byc wywolywane na poczatku, w AfterTraverse - na koncu.  }
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); virtual;
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); virtual;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); virtual;

    { method below can be used by a specific node kind to parse fields
      that are NOT on Fields list. Notka : oczywiscie, normalnie
      WSZYSTKIE pola powinny byc na liscie Fields i musza tam byc abysmy
      mogli je normalnie odczytywac ! Po co wiec to udziwnienie ?
      Chodzi o to ze probujemy obslugiwac kilka roznych odmian VRML'a :
      Inventora, VRML'a 1.0 i VRML'a 97. Czasem pewne pola
      danego node'a sa w innej odmianie wyrazone innym polem. Chcac zachowac
      rozsadna prostote wykonania nie mozemy w danym nodzie umieszcac
      dwoch pol ktore wyrazaja to samo ale w inny sposob. Wiec za pomoca
      ponizszej metody pozwalamy node'owi obsluzyc "inne pola" tak aby
      mogl je zinterpretowac i zapisac w swoich polach. Przyklad:
      TNodeShapeHints.

      W klasie TVRMLNode ta metoda nic nie robi, zwraca tylko false.
      Jezeli w jakiejs klasie je przedefiniujesz, powinienes albo
      odczytywac cale pole na ktorym stoi lexer i zwracac true albo
      nie zmieniac pozycji lexera i zwracac false.

      Jest gwarantowane ze w momencie wywolania tej proc. Lexer.Token = vtName. }
    function TryParseSpecialField(Lexer: TVRMLLexer): boolean; virtual;

    {methods to use in Fd* fields to allow comfortable access to node's specific fields}
    function GetField(i: integer): TVRMLField;
    function GetFieldAsSFBitMask(i: integer): TSFBitMask;
    function GetFieldAsSFBool(i: integer): TSFBool;
    function GetFieldAsSFColor(i: integer): TSFColor;
    function GetFieldAsSFEnum(i: integer): TSFEnum;
    function GetFieldAsSFFloat(i: integer): TSFFloat;
    function GetFieldAsSFImage(i: integer): TSFImage;
    function GetFieldAsSFLong(i: integer): TSFLong;
    function GetFieldAsSFMatrix(i: integer): TSFMatrix;
    function GetFieldAsSFRotation(i: integer): TSFRotation;
    function GetFieldAsSFString(i: integer): TSFString;
    function GetFieldAsSFVec2f(i: integer): TSFVec2f;
    function GetFieldAsSFVec3f(i: integer): TSFVec3f;
    function GetFieldAsMFColor(i: integer): TMFColor;
    function GetFieldAsMFLong(i: integer): TMFLong;
    function GetFieldAsMFVec2f(i: integer): TMFVec2f;
    function GetFieldAsMFVec3f(i: integer): TMFVec3f;
    function GetFieldAsMFFloat(i: integer): TMFFloat;
    function GetFieldAsMFString(i: integer): TMFString;
  public
    { kazdy typ node'a ma ustalone Fields razem z ich defaultowymi wartosciami
      w konstruktorze. Potem, czytajac obiekt ze strumienia lub operujac na
      nim kodem mozesz zmieniac tylko wartosci na poszczegolnych polach.
      Node sam zwalnia swoje pola w destruktorze.

      Uwaga - w przypadku klasy TNodeUnknown (i tylko tam) to pole jest
      inicjowane kazdorazowo po parsowaniu (a wiec moze ulegac zmianie
      w czasie zycia obiektu, juz po wywolaniu konstruktora).

      @noAutoLinkHere }
    Fields: TVRMLFieldsList;

    { Kazdy VRML nodes moze miec dowolnie wiele Children.
      Kiedy jakis node jest na liscie Children
      jednego node'a to ma swojego rodzica na swojej liscie Parents.
      Wiec w ten sposob mozemy podrozowac po grafie w obie strony.
      (pamietaj ze graf VRML'a nie ma cykli gdy na niego patrzec jak na graf
      skierowany (a takim wlasnie jest) ale kazdy node moze miec wiele rodzicow
      wiec jezeli potraktujemy go jako graf nieskierowany to mozemy otrzymac
      cykle; wszystko przez to ze node moze miec wiele Parents bo moze
      uzywac mechanizmu USE).

      Kiedy jakis node jest na liscie Children innego node'a to gdy ten inny
      node bedzie go kasowal ze swojej listy Children (a w destruktorze
      kazdy node kasuje wszystkich ze swojej listy Children) to wywola
      jego destruktora. Innymi slowy, gdy jakis node jest czyims dzieckiem
      to jest reference-counted i automatycznie zwalniany.

      Wazna konwencja : jak widac, rodzic automatycznie martwi sie o swoje
      dzieci. Natomiast dziecko w swoim Free nie martwi sie o uaktualnienie
      swoich rodzicow.

      Zwracam tez uwage ze RemoveChild wymaga indeksu. Okreslanie dziecka
      jako children: TVRMLNode jest nie-1-znaczne bo przeciez jeden node
      moze miec kilka razy to samo dziecko (i w rezultacie, nawiasem mowiac,
      kazde dziecko moze miec wiele razy tego samego Parenta). A nie chcemy
      przeciez pomieszac sobie kolejnosci w Children (ona determinuje
      przeciez kolejnosc przegladania grafu, a wiec Renderowania itp.)
      (Natomiast mozemy sobie pozwolic i nieraz pozwalamy na ew. pomieszanie
      kolejnosci w Parents; inaczej musielibysmy z kazdym Parents pamietac
      swoj index na jego liscie). Tak wiec na listach Children i Parents
      moga byc duplikaty i zdecydowanie nie powinnismy nigdzie niefrasobliwie
      "czyscic" tych list przez DeleteDuplicates;

      @noAutoLinkHere }
    property Children[i: integer]:TVRMLNode read GetChildrenItem;

    function ChildrenCount: integer;

    { AddChild z Index (musi byc w zakresie 0..ChildrenCount)
      przesuwa elementy o numerach Index i wiekszych w prawo i
      wstawia child na wskazane Index.    }
    procedure AddChild(Index: Integer; child: TVRMLNode); overload;

    { AddChild bez Indexu - dodaje na koniec listy Children. }
    procedure AddChild(child: TVRMLNode); overload;

    procedure RemoveChild(i: integer);
    procedure RemoveAllChildren;

    property Parents[i: integer]:TVRMLNode read GetParentsItem;
    function ParentsCount: integer;

    {bardzo speszial metoda Free: o ile tylko Self <> nil, usuwa nasz node
     ze WSZYSTKICH list Parents[].Children i robi Destroy.
     Tym samym robi nam Free robiac to czego normalne Free nie robi :
     martwiac sie o Parents. Jezeli chcesz usunac node ze srodka hierarchii
     VRMLa - to jest dobra metoda zeby to zrobic. }
    procedure FreeRemovingFromAllParents;

    { AllowedChildren okresla jakie dzieci moga byc dziecmi tego node'a.
      Warunek ten bedzie sprawdzany w AddChild wiec nigdy nie uda ci sie dodac
      node'a ktory nie jest tutaj dozwolony.

      ParsingAllowedChildren okresla jakie dzieci moga byc odczytane
      ze strumienia jako dzieci tego node'a. Chwilowo ma to zastosowanie
      tylko dla WWWInline ktory w strumieniu nie moze miec zapisanych
      zadnych dzieci ale laduje swoj inline jako swoje Child.
      Wiec musi miec ParsingAllowedChildren=[] i AllowedChildren = All.

      TODO: jak bedzie mi to potrzebne to zaimplementuje te pola jako
      tablice TDynVRMLNodeClassArray z dodatkowym polem Any. Taka tablica
      bedzie pasowala do wszystkiego gdy Any = true, wpp. tylko do wymienionych
      na niej elementow. Wartosc *AllowedChildren = true tutaj odpowiadac
      bedzie Any = true tam, *AllowedChildren = false oznacza Any = false i Items=[].
      Wartosc Any jest potrzebna zeby na zapas powiedziec : wszystkie node'y
      sa tu dozwolone, nawet takie o jakich jeszcze nie wie NodesManager -
      - a jest to przeciez czesta sytuacja.

      Naturalnie ParsingAllowedChildren musi sie zawierac w AllowedChildren
      bo inaczej parsowanie moze wygenerowac wyjatek (wywolujac
      AddChild z niedozwolonym argumentem). W tej chwili oznacza to tylko
      ze nie moze byc ParsingAllowedChildren = true i AllowedChildren = false.

      Uwaga - w przypadku klasy TNodeUnknown (i tylko tam) wartosci tych
      pol sa inicjowane po kazdorazowym parsowaniu (a wiec ulegaja zmianie
      juz po wykonaniu konstruktora obiektu). }
    property AllowedChildren: boolean read fAllowedChildren; { = false }
    property ParsingAllowedChildren: boolean read fParsingAllowedChildren; { = false }

    { NodeName ='' oznacza ze obiekt nie mial zdefiniowanej nazwy.
      Nie pomyl NodeName z Name : w VRML'u 2.0 TVRMLNode to po prostu pole
      typu TVRMLSingleField w zwiazku z tym ma swoja nazwe pole (Name)
      i ma swoja nazwe jako node (NodeName). Chwilowo tego nie zaimplementowalem
      i TVRMLNode nie dziedziczy od TVRMLField ale kiedys to niewatpliwie
      nastapi. }
    property NodeName: string read fNodeName;

    { WWWBasePath is the base URL path for all URL's
      in node's fields. This is currently used by Texture2
      and WWWInline nodes, could be used by WWWAnchor (but currently
      WWWAnchor ignores it's URL anchor).

      This way URL's in node's fields may contain relative names.
      If WWWBasePath doesn't begin with <proto>:// it is understood
      to be a file:// base path.

      TODO: chwilowo, poniewaz tylko odwolania do lokalnych plikow
      sa zaimplementowane, cale to bajanie o URL'ach to tylko mowa
      "jak kiedys bedzie". Chwilowo WWWBasePath musi byc lokalna sciezka
      (i to absolutna, bezwzgledna sciezka).

      WWWBasePath jest ustalane w Create, ew. pozniej jest zmieniane w trakcie
      Parse() na podstawie Lexer.WWWBasePath.
      W ten sposob np. moglibysmy, gdybysmy tylko chcieli, rozwiazywac WWWInline
      lub Texture2 zaraz po sparsowaniu ich. }
    property WWWBasePath: string read FWWWBasePath;

    { Parse jest zaimplementowane ogolnie dla wszystkich TVRMLNode'ow
      za wyjatkiem node'a TNodeUnknown ktory redefiniuje ta metode.
      Parse ustala wartosci Fields, liste Children, WWWBasePath.
      W przypadku TNodeUnknown ma dozwolone takze zeby inicjowal
      *AllowedChildren i ilosc i typy Fields.
      Czasami jakies inne node'y robia override tej metody zeby (po wywolaniu
      w niej inherited) zrobic jakies dodatkowe rzeczy ktore powinno sie
      zrobic po sparsowaniu.

      @noAutoLinkHere }
    procedure Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList); virtual;

    { Konstruktor. Inicjuje wszystko (jak to konstruktor), w szczegolnosci :
      @unorderedList(
        @item(inicjuje NodeName, WWWBasePath na podstawie podanych tu parametrow)
        @item(
          inicjuje tablice Fields ustawiajac polom ich defaultowe wartosci
          (dla implementatorow podklas TVRMLNode:
          w klasie TVRMLNode inicjujemy Fields na tablice o 0 elementach;
          w konstruktorze podklasy musisz wywolac Fields.Add(...)
          aby dodac sobie odpowiednie pola))
        @item(
          [Parsing]AllowedChildren
          (dla implementatorow podklas TVRMLNode:
          w klasie TVRMLNode inicjujemy je na zbiory puste (tzn. chwilowo po prostu
          na false) po prostu dlatego ze wydaje sie to byc najczestsza wartoscia.
          W konstruktorze podklas mozesz swobodnie zmienic wartosci tych pol.))
      )

      @noAutoLinkHere }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); virtual;

    { CreateParse : wygodne polaczenie Create+Parse }
    constructor CreateParse(const ANodeName: string; Lexer: TVRMLLexer; NodeNameBinding: TStringList);

    { @noAutoLinkHere }
    destructor Destroy; override;

    { NodeTypeName zwraca nazwe klasy w VRML'u. Zawsze jest <>''.
      To ma byc normalna nazwa node'a, taka ktora odczytujemy
      i zapisujemy bezposrednio z/do pliku VRMLa (wobec tego jest ona tez
      case-sensitive, jak caly VRML).
      Nie zmienia sie przez caly czas zycia obiektu, tzn. raz zainicjowana w
      konstruktorze juz taka pozostaje (nawet dla obiektu TNodeUnknown;
      dla obiektu TNodeUnknown po prostu NodeTypeName nie jest 1-znacznie
      wyznaczone przez sama klase, tzn. obiekty klasy TNodeUnknown moga
      miec rozne NodeTypeName, w przeciwienstwie do "normalnych" klas ktore
      zawsze maja takie samo NodeTypeName; ale to nie czyni przypadku
      TNodeUnknown czyms wyjatkowym dla tej funkcji).

      W tej klasie NodeTypeName zwraca ClassNodeTypeName. Uwagi do
      implementacji podklas TVRMLNode dotyczace tej funkcji - patrz
      ClassNodeTypeName. }
    function NodeTypeName: string; virtual;

    { ClassNodeTypeName zwraca nazwe klasy VRMLa zwiazanej z tym node'm lub
      '' w przypadku klas ktore nie maja zwiazanej ze soba 1-znacznej nazwy
      typu wezla VRMLa ktory reprezentuja (a poniewaz kazda klasa wezla VRMLa
      musi miec NodeTypeName <> '' wiec oznacza to ze te wyjatkowe klasy ustalaja
      sobie NodeTypeName w jakis inny, specjalny sposob - jedyny dostepny
      w tej chwili przyklad tego to TNodeUnknown (chociaz nie wykluczam sobie
      w tym momencie czy nie pojawi sie kiedys jeszcze jakis inny tego typu node))

      Jezeli masz do dyspozycji instancje obiektu to nie powinienes uzywac
      tej funkcji. Jedyna jej zaleta ponad NodeTypeName jest ze jest funkcja
      klasy. Jezeli masz do dyspozycji tylko klase obiektu to uzywajac
      tej funkcji musisz sie zdecydowac co zrobic jesli dostaniesz w odpowiedzi
      '' (albo jakos sie zabezpieczyc zeby nigdy w danym kontekscie takich
      klas nie miec...)

      Uwagi do implementacji podklas TVRMLNode:
      W tej klasie ClassNodeTypeName zwraca ''. Wszystkie niesbtrakcyjne
      klasy wezlow VRMLa musza pokrywac albo ClassNodeTypeName (i w ten
      sposob staja sie normalnymi klasami ktore maja zawszetaka sama wartosc
      NodeTypeName dla swojej klasy) albo NodeClassTypeName (i w ten sposob
      staja sie klasami specjalnymi, jak TNodeUnknown, ktore nie maja
      stalej wartosci NodeTypeName). }
    class function ClassNodeTypeName: string; virtual;

    { przejdz po tym nodzie i wszystkich subnode'ach i dla wszystkich
      node'ow z klasy NodeClass wywolaj TraversingFunc z odpowiednim
      aktualnym State.
      Schemat dzialania Traverse :

@preformatted(
  BeforeTraverse;
  if Self is NodeClass then TraversingFunc (Self, State)
  MiddleTraverse
  dla wszystkich Children sposrod ChildrenToEnter wywolaj ich Traverse(State)
  AfterTraverse,
  dodaj Self do stanu State do LastNode (o ile Self wsrod
    TraverseStateLastNodesClasses)
)

      Jezeli zostalo wykonane BeforeTraverse, na pewno zostanie wykonane tez
      AfterTraverse (wywolanie AfterTraverse jest w finally..end).

      Kolejnosc w jakiej przechodzi graf jest naturalnie istotna.
      W czasie wykonywania Traverse mozesz modyfikowac tylko node'y dzieci
      (bezposrednie i niebezposrednie) node'a na ktorym wlasnie stoisz. }
    procedure Traverse(State: TVRMLGraphTraverseState; NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc); virtual;

    { enumerate all our children of some class. Recursively.
      Zwroci do proc() takze sam obiekt na ktorym EnumNodes zostalo
      wywolane, jezeli tylko ten obiekt jest klasy nodeClass.

      Wersja z argumentem SeekNodeName wymaga ponadto aby node mial NodeName=
      SeekNodeName (gdy SeekNodeName = '' to znajduje nienazwane node'y,
      wiec wartosc '' nie jest tu traktowana specjalnie).

      Jezeli enumOnlyInActiveNodes to bedzie wchodzil tylko w dzieci
      zwrocone przez ChildrenToEnter (czyli bedzie sie zachowywal jak
      takie proste Traverse ktore nie sledzi swojego State).
      Wpp. bedzie wchodzil we wszystkie dzieci.

      Zaczyna przegladac dzieci dopiero jak przegladnie Self. Jezeli np.
      w proc. zmodyfikowales (np. dodales) wlasne Children to EnumNodes
      will enumerate these new children. To ma znaczenie np. w
      TVRMLScene.LoadAllInlined gdzie w proc robimy LoadInlined. Poniewaz
      EnumNodes przeglada dzieci po wywolaniu proc., wiadomo ze
      przegladnie tez nowo zaladowane dziecko.

      BTW modyfikowanie dzieci node'a ktory wlasnie dostales do proc()
      to jedyna dozwolona modyfikacja na hierarchii VRMLa ktora mozesz
      wykonywac w czasie EnumNodes. }
    procedure EnumNodes(nodeClass: TVRMLNodeClass; proc: TVRMLNodeProc;
      enumOnlyInActiveNodes: boolean); overload;
    procedure EnumNodes(nodeClass: TVRMLNodeClass; const SeekNodeName: string;
      proc: TVRMLNodeProc; enumOnlyInActiveNodes: boolean); overload;

    { TryFindNode(ByName) : szuka wsrod siebie i swoich subnode'ow
      node'a o zadanej nazwie/klasie. Jezeli nie znajdzie zwraca nil.

      FindNodeByName : j.w. ale jezeli nie znajdzie - wyjatek.

      Jezeli enumOnlyInActiveNodes to bedzie wchodzil tylko w dzieci
      zwrocone przez ChildrenToEnter. Wpp. bedzie wchodzil we wszystkie
      dzieci. }
    function TryFindNodeByName(const FindName: string; enumOnlyInActiveNodes: boolean): TVRMLNode;
    function FindNodeByName(const FindName: string; enumOnlyInActiveNodes: boolean): TVRMLNode;
    function TryFindNode(FindClass: TVRMLNodeClass; enumOnlyInActiveNodes: boolean): TVRMLNode;
    function FindNode(FindClass: TVRMLNodeClass; enumOnlyInActiveNodes: boolean): TVRMLNode;

    { Znajdz pierwszy Node (zadanej klasy NodeClass) razem ze State
      (lub tylko z Transform).
      Dziala jak Traverse ktore zatrzymuje sie po pierwszej udanej probie.
      Pamietaj ze State nie jest pamietane nigdzie indziej i musisz je zwolnic.
      W przypadku TryFindNodeTransform nie musisz o tym pamietac,
      no i TryFindNodeTransform dziala nieco szybciej.
      Zwraca false i nie zmienia Node ani State ani Transform jesli nie znajdzie. }
    function TryFindNodeState(InitialState: TVRMLGraphTraverseState;
      NodeClass: TVRMLNodeClass;
      var Node: TVRMLNode; var State: TVRMLGraphTraverseState): boolean;
    function TryFindNodeTransform(InitialState: TVRMLGraphTraverseState;
      NodeClass: TVRMLNodeClass;
      var Node: TVRMLNode; var Transform: TMatrix4Single): boolean;

    { Szuka wsrod Self, wsrod node'ow Parents, wsrod ich node'ow Parents itd.
      Innymi slowy, dzialaja tak samo jak odpowiedniki bez "Parent" ale
      ida w gore (tzn. w/g wlasciwosci Parents, a nie Children).
      Zwracam uwage ze zawsze przeszukuja caly graf w gore, nie ograniczajac
      sie tylko do aktywnej czesci (bo nie mamy tu mechanizmow aby isc
      w gore grafu wzdluz aktywnych czesci, zreszta nie da sie ich zdefiniowac -
      - trzebaby podawac tutaj jakies RootNode). }
    function TryFindParentNodeByName(const FindName: string): TVRMLNode;
    function FindParentNodeByName(const FindName: string): TVRMLNode;
    { Przeszukuje podobnie jak powyzsze FindParentNodeByName. Zwraca true
      jesli znalazl tam gdzies node Node. }
    function HasParent(Node: TVRMLNode): boolean;

    { sprawdza czy istnieje w grafie VRMl'a zaczepionym w danym punkcie
      node Node. Znaczenie seekOnlyInActiveNodes jak zwykle. }
    function IsNodePresent(Node: TVRMLNode; seekOnlyInActiveNodes: boolean): boolean;

    { policz ile jest node'ow danej klasy. Uzywajac np. TNodeGeneralLight mozesz
      sprawdzic czy na scenie zostalo zdefiniowane jakiekolwiek swiato.
      Parametr countOnlyActiveNodes ma znaczenie jak zwykle. }
    function CountNodes(NodeClass: TVRMLNodeClass; countOnlyActiveNodes: boolean): integer;

    { zapisz node do strumienia; ta metoda jest tu zaimplementowana zupelnie
      ogolnie i dziala dla kazdej podklasy TVRMLNode. Jak widac,
      zapisujac graf VRML'a takze trzymamy sobie aktualne NodeNameBinding.
      W ten sposob wiemy ze jezeli jakis node juz jest na tej liscie
      to wystarczy zrobic mu USE. Jednoczesnie NodeNameBinding to,
      podobnie jak przy parsowaniu, lista bez duplikatow, wiec jezeli nawet
      w scenie beda dwa node'y o tej samej nazwie to my zapiszemy scene
      poprawnie (uzyjemy USE tylko tam gdzie bedziemy mogli, jesli nie bedziemy
      mogli - zapiszemy node normalnie).

      Notka - jesli Self is TNodeWWWInline to nie zapisujemy swoich Children. }
    procedure SaveToStream(Stream: TStream; const Indent: string; NodeNameBinding: TStringList);

    { szuka tej klasy node'a (rzeczywistej koncowej klasy, z ClassType) w
      TraverseStateLastNodesClasses. Zwraca indeks lub -1 jesli nie znalazl. }
    class function TraverseStateLastNodesIndex: Integer;
  end;

  TObjectsListItem_3 = TVRMLNode;
  {$I ObjectsList_3.inc}
  TVRMLNodesList = class(TObjectsList_3);

{ specific VRML nodes. ----------------------------------------------------
  All specific VRML nodes define
   - Fd* fields that allow fast, comfortable and type-secure way
     for program to retrieve and set their fields
   - override constructor (look at TVRMLNode.Create comments for things
     that MUST be defined in this derived constructor)
   - some override Before/Middle/AfterTraverse, TryParseSpecialField
}

  { Shape is the only node that produces some visible results
    during rendering. Basically, whole VRML 1.0 language is just
    a method of describing those shapes and almost all other nodes
    are defined only to set up additional state for shapes.

    This @italic(almost) stands for "cameras, WWWAnchor, Info and Fog nodes"
    (VRML97 has some more of these, like, soon to be implemented here,
    WorldInfo and Background), these nodes specify some things that can't
    be embedded in simple Render command fro OpenGL.
    These things describe
    @unorderedList(
      @item(user interaction with the world (cameras, WWWAnchor))
      @item(
        some information that has no meaning to us and all we can do about it
        (besides ignoring it) is to show it to the user (Info, WorldInfo))
      @item(
        some information about how to render the world that cannot be just
        expressed as "modifying the way all subsequent shapes are drawn"
        (Fog, Background))
    )

    This class may have some special functionality and it builds
    comfortable object inheritance hierarchy.
    For example, now we can use EnumNodes(TNodeGeneralShape).

    A few things that make Shape node special :
    @unorderedList(
      @item only shape nodes may have [Local]BoundingBox
      @item(
        only shape nodes define something visible "in usual way" during rendering
        (there may be some nodes that are also visible : those are only
        present in VRML 97, e.g. very usueful Sky node (that I'm planning
        to implement rather soon). However, this nodes must be rendered in
        a special way --- they are not affected in any usual way by the current
        transformation matrix))
      @item(
        only shape nodes can add triangles to the scene, so the Triangulate
        method can be defined only for shape nodes.)
      @item(
        shape nodes never have children (that's why I don't need to define
        in interface whether [Local]BoundingBox or Triangles/VerticesCount
        calculate child nodes too - because they will never have any child nodes))
      @item(
        shape nodes doesn't affect anything in graph traverse state.)
    ) }
  TNodeGeneralShape = class(TVRMLNode)
    { BoundingBox oblicza BoundingBox shape node'a VRMLa ktory podczas
      trawersowania grafu VRML'a ma stan State.

      LocalBoundingBox liczy BoundingBox jakby CurrMatrix = IdentityMatrix,
      czyli liczy bounding box wzgledem lokalnego ukladu node'a.

      W tej klasie LocalBoundingBox jest liczone jako BoundingBox ktore
      dostaje specjalnie spreparowane State z CurrMatrix = zawsze Identity.
      Jest to poprawna metoda realizacji LocalBoundingBox'a natomiast
      nieco nieoptymalna czasowo : bedzie wykonywanych wiele mnozen przez
      macierz o ktorej wiadomo ze jest Identity. Wiec w podklasach mozesz
      pokrywac ta metode aby liczyc LocalBoundingBox'a w szybszy sposob.

      Zwracam uwage ze odwrotny pomysl --- realizacja BoundingBox'a przez
      LocalBoundingBox'a (transformujac wyliczony LocalBoundingBox przez
      State.CurrMatrix) nie jest juz tak dobrym pomyslem --- mozemy w rezultacie
      otrzymac o wiele za duze BoundingBox'y.

      Tym niemniej miejscami zamierzam tak liczyc BoundingBox'a --- np. dla sfery.
      Wiec w tej klasie BoundingBox jest zaimplementowany wlasnie jako
      LocalBoundingBox transformowany o State.CurrMatrix.

      W kazdej podklasie Shape powinienes pokryc przynajmniej jedna z tych metod
      --- jak to napisalem powyzej, jezeli nie pokryjesz BoundingBox'a to byc
      moze otrzymany BoundingBox bedzie nieco za duze (co jest w sumie
      dopuszczalne, ale nie do przesady), jezeli nie pokryjesz LocalBoundingBox
      --- to LocalBoundingBox nie bedzie liczony tak szybko jak moglby byc.
      Najlepiej wiec byloby gdybys pokrywal obie te metody. }
    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; virtual;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; virtual;

    { kazda podklasa GeneralShape musi pokrywac i implementowac te metody.

      Te metody zwracaja ilosc trojkatow jaka definiuje [Local]Triangulate
      (z takimi samymi parametrami State i OverTriangulate)
      dla tego node'a i ilosc roznych vertexow jakie sa uzywane w tych
      trojkatach (chociaz nie wykonuje w tym celu zadnych porownan miedzy
      zdefiniowanymi punktami w node'ach i definiujac nieporzadnie node'y
      (np. podajac dwa razy ten sam punkt w Coordinate3) mozesz latwo to
      oszukac). (acha, dla PointSet naturalnie nie ma zadnych trojkatow
      ale VerticesCount to ciagle liczba vertexow, mimo ze nie sa uzywane
      w zadnych trojkatach).

      State chwilowo nie jest nigdzie uzywany w TrianglesCount,
      ale jestem gotowy gdyby w przyszlosci jakis node tego potrzebowal
      (bo w sumie nie byloby w tym nic wyjatkowego, tzn. nie byloby to
      nic co w modelu VRMLu jaki tu zaimplementowalem musialbym gdziekolwiek
      traktowac jako jakis wyjatek)

      Uwaga --- gdy przychodzi do TrianglesCount moze sie okazac ze Triangulate
      zwrocilo inna ilosc trojkatow gdy niektore face byly non-convex (bo w tym
      przypadku TriangulateFace ma prawo pousuwac trojkaty zdegenerowane
      do punktu). Generalnie nie polegaj na TrianglesCount jako na dokladnej
      wartosci --- raczej jako na przyblizeniu ktore zazwyczaj bedzie bardzo
      bardzo dokladne. }
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; virtual; abstract;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; virtual; abstract;

    { triangulate node = call NewTriangleProc for each triangle this node
      defines. NewTriangleProc will be called with (Tri, State, Node) where
      Tri will be new triangle,  and State will always be State podany tutaj
      parametrem and Node will always be Self.

      LocalTriangulate robi to samo ale nie uwzglednia State.CurrMatrix.
      W podklasach trzeba zdefiniowac tylko LocalTriangulate.

      Jezeli OverTriangulate = false to [Local]Triangulate generuje tylko tyle
      trojkatow zeby doskonale odzwierciedlac ksztalt node'a. W przypadku
      gdy nie jest to mozliwe (np. przy kulach, stozkach itp.)
      [Local]Triangulate uzywa State.KambiTriangulation
      aby zdecydowac sie jak dobre chcemy miec przyblizenie oryginalu.

      Jezeli OverTriangulate = true to [Local]Triangulate moze wygenerowac
      wiecej trojkatow --- w przypadku duzych powierzchni moze zdecydowac sie
      zeby je rozbic na wiecej malych trojkatow niz jest to rzeczywiscie
      potrzebne aby wyrazic te powierzchnie jako zbior trojkatow.
      Np. node Cube (szescianik) mozna wyrazic doskonale dokladnie jako
      12 trojkatow i to wlasnie zrobimy gdy OverTriangulate = false.
      ALE gdy OverTriangulate = true my rozbijemy kazda sciane szescianu
      na WIECEJ trojkatow, zgodnie z parametrem State.
      KambiTriangulation .RectDivisions.
      Do czego to sie moze przydac ? Gdy uzywasz cieniowania Gourauda (albo
      jeszcze gorzej, cieniowania plaskiego) jedynym remedium zeby renderowac
      rozblyski na srodku duzych powierzchni jest wlasnie rozbijac te powierzchnie
      na duzo trojkatow. }
    procedure Triangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      NewTriangleProc: TNewTriangleProc);
    procedure LocalTriangulate(State: TVRMLGraphTraverseState;
      OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); virtual; abstract;
  end;

  TNodeAsciiText = class(TNodeGeneralShape)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdString: TMFString index 0 read GetFieldAsMFString;
    property FdSpacing: TSFFloat index 1 read GetFieldAsSFFloat;
    { Use consts JUSTIFICATION_XXX (declared below in this unit) }
    property FdJustification: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdWidth: TMFFloat index 3 read GetFieldAsMFFloat;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCone = class(TNodeGeneralShape)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdParts: TSFBitMask index 0 read GetFieldAsSFBitMask;
    property FdBottomRadius: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 2 read GetFieldAsSFFloat;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCube = class(TNodeGeneralShape)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdWidth: TSFFloat index 0 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdDepth: TSFFloat index 2 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCylinder = class(TNodeGeneralShape)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdParts: TSFBitMask index 0 read GetFieldAsSFBitMask;
    property FdRadius: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 2 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  {wspolny rodzic dla IndexedFaceSet, IndexedTriangleMesh, IndexedLineSet}
  TNodeGeneralIndexed = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdCoordIndex: TMFLong index 0 read GetFieldAsMFLong;
    property FdMaterialIndex: TMFLong index 1 read GetFieldAsMFLong;
    property FdNormalIndex: TMFLong index 2 read GetFieldAsMFLong;
    property FdTextureCoordIndex: TMFLong index 3 read GetFieldAsMFLong;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  { wspolny rodzic dla IndexedFaceSet i IndexedTriangleMesh }
  TNodeIndexed_Faces_Or_Triangles = class(TNodeGeneralIndexed)
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeIndexedFaceSet = class(TNodeIndexed_Faces_Or_Triangles)
    class function ClassNodeTypeName: string; override;
  end;

  { IndexedTriangleMesh - from Inventor 1.0. }
  TNodeIndexedTriangleMesh = class(TNodeIndexed_Faces_Or_Triangles)
    class function ClassNodeTypeName: string; override;
  end;

  TNodeIndexedLineSet = class(TNodeGeneralIndexed)
    class function ClassNodeTypeName: string; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodePointSet = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdStartIndex: TSFLong index 0 read GetFieldAsSFLong;
    property FdNumPoints: TSFLong index 1 read GetFieldAsSFLong;

    {oblicz startIndex i numPoints na podstawie odpowiednich pol.
     Zwrocone numPoints jest na pewno > 0, przedzial startIndex..numPoints-1
     na pewno zawiera sie w przedziale 0..LastCoordinate3.FdPoint.Count-1.
     Ta proc. NIE poprawia wartosci na polach Fd* - bo byc moze w skryptach
     bedzie wygodniej zakladac ze nawet nieprawidlowe wartosci sa trwale. }
    procedure CalculateRange(LastCoordinate3: TNodeCoordinate3;
      var startIndex, numPoints: integer);

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeSphere = class(TNodeGeneralShape)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRadius: TSFFloat index 0 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCoordinate3 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdPoint: TMFVec3f index 0 read GetFieldAsMFVec3f;
  end;

  TVRMLFontFamily = 0..2; {uzywaj stalych FSFAMLILY}
  TNodeFontStyle = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdSize: TSFFloat index 0 read GetFieldAsSFFloat;
    property FdFamily: TSFEnum index 1 read GetFieldAsSFEnum;
    property FdStyle: TSFBitMask index 2 read GetFieldAsSFBitMask;
    function TTF_Font: PTrueTypeFont;
  end;

  TNodeInfo = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdString: TSFString index 0 read GetFieldAsSFString;
  end;

  TNodeLOD = class(TVRMLNode)
    {sorry - tu proc SetChildrenToEnterFromDistanceToViewer}
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    procedure ChildrenToEnter(var FirstChild, LastChild: integer); override;
    property FdRange: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdCenter: TSFVec3f index 1 read GetFieldAsSFVec3f;
  end;

  TNodeMaterial = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdAmbientColor: TMFColor index 0 read GetFieldAsMFColor;
    property FdDiffuseColor: TMFColor index 1 read GetFieldAsMFColor;
    property FdSpecularColor: TMFColor index 2 read GetFieldAsMFColor;
    property FdEmissiveColor: TMFColor index 3 read GetFieldAsMFColor;
    property FdShininess: TMFFloat index 4 read GetFieldAsMFFloat;
    property FdTransparency: TMFFloat index 5 read GetFieldAsMFFloat;

    { pola dodane przeze mnie na potrzeby raytracerow na RGK }
    property FdMirror: TMFFloat index 6 read GetFieldAsMFFloat;
    property FdReflSpecular: TMFColor index 7 read GetFieldAsMFColor;
    property FdReflDiffuse: TMFColor index 8 read GetFieldAsMFColor;
    property FdTransSpecular: TMFColor index 9 read GetFieldAsMFColor;
    property FdTransDiffuse: TMFColor index 10 read GetFieldAsMFColor;
    property FdReflSpecularExp: TMFFloat index 11 read GetFieldAsMFFloat;
    property FdTransSpecularExp: TMFFloat index 12 read GetFieldAsMFFloat;

    { ponizej sa funkcje do latwego wyciagania materialu z node'a material.
      De facto powinienes ich uzywac zawsze gdy zalezy ci na wyciagnieciu
      wartosci materialu. Te funkcje uwzgledniaja fakt ze pola moga byc
      zapisane zle (niezgodnie ze specyfik. VRMLa albo zgodnie ale w jakis
      szczegolny przypadek wymagajacy opakowania) - np. ze moze nie byc
      podanej zadnej wartosci dla ambient a my chcemy kolor numer zero
      (w tym przypadku zwracany jest defaultowy kolor ambient materialu VRMLa)
      albo ze moze nie byc wystarczajacej ilosci kolorow (wtedy bierzemy
      ostatni kolor).

      W przypadku czterech wartosci Trans/ReflSpecular/Diffuse
      te funkcje realizuja "wyliczanie" na podstawie innych pol materialu,
      tak jak opisalem w
      [http://camelot.homedns.org/~michalis/kambi_vrml_extensions.php].

      Funkcje zwracajace TVector4Single zwracaja [TVector3Single, Opacity].

      Transparency i Opacity sa zwracane w zakresie 0..1, przy czym
      Opacity = 1 - Transparency.

      ShininessExp to @italic(nieznormalizowany) wykladnik odbicia zwierciadlanego
      (dla modelu osw. Phonga) (mimo ze w VRMLu powinnismy dostac
      znormalizowany, tzn. w zakresie 0..1 reprezentujacym 0..128;
      ale nie widze szczerze mowiac sensu takiej normalizacji, wartosc
      128 nie jest (chyba?) zadna szczegolna wartoscia, to tylko taka
      ustalona granica powyzej ktorej i tak wartosci juz w zasadzie nie maja
      sensu (cos^128 to juz jest BARDZO waska gorka).
      W sumie dla usera zakres 0..1 moze byc po prostu wygodniejszy do myslenia
      (chociaz moze byc tez mylacy, bo roznice miedzy 1 a 0.5 w takiej konwencji
      trudno jakos okreslic jako "dwa razy wieksza') ale dla wszystkiego co
      chcielibysmy robic programowo zdecydowanie bedziemy potrzebowali
      nieznormalizowanej wartosci. }
    function AmbientColor3Single(MatNum: integer): TVector3Single;
    function AmbientColor4Single(MatNum: integer): TVector4Single;
    function DiffuseColor3Single(MatNum: integer): TVector3Single;
    function DiffuseColor4Single(MatNum: integer): TVector4Single;
    function SpecularColor3Single(MatNum: integer): TVector3Single;
    function SpecularColor4Single(MatNum: integer): TVector4Single;
    function EmissiveColor3Single(MatNum: integer): TVector3Single;
    function EmissiveColor4Single(MatNum: integer): TVector4Single;
    function Transparency(MatNum: integer): Single;
    function Opacity(MatNum: integer): Single;
    function ShininessExp(MatNum: integer): Single;

    function Mirror(MatNum: integer): Single;
    function ReflSpecular (MatNum: integer): TVector3Single;
    function ReflDiffuse  (MatNum: integer): TVector3Single;
    function TransSpecular(MatNum: integer): TVector3Single;
    function TransDiffuse (MatNum: integer): TVector3Single;
    function ReflSpecularExp (MatNum: integer): Single;
    function TransSpecularExp(MatNum: integer): Single;

    { true oznacza ze ten material to specjalny przypadek o ktorym mowa w
      specyfikacji VRMLa : pola ambient, diffuse i specular maja dlugosc = 0
      a wiec nalezy uznac kolor Emissive za JUZ WYLICZONY ostateczny kolor.
      Powinienes wowczas odczytac tylko wartosc EmissiveColor i
      Transparency/Opacity (ujete w jednej prostej wartosci EmissiveColor4Single)
      i nie patrzec na inne wlasciwosci materialu. }
    function OnlyEmissiveMaterial: boolean;

    { true jesli wszystkie elementy pola FdTransparency sa ostro wieksze
      od zera  (od SingleEqualityEpsilon, tak naprawde).
      Pamietaj ze pole FdTransparency o Length = 0 jest zawsze traktowane
      jakby podano wartosc domyslna, tzn. [0], a wiec wtedy ta funkcja zwraca
      false (jest to sprzeczne z matematyczna (i intuicyjna) definicja
      kwalifikatora "all" ktora mowi ze "kazde zdanie kwalifikowane
      'dla kazdego' jest prawdziwe dla zbioru pustego") }
    function IsAllMaterialsTransparent: boolean;
  end;

  TNodeMaterialBinding = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdValue: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  TNodeNormal = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdVector: TMFVec3f index 0 read GetFieldAsMFVec3f;
  end;

  TNodeNormalBinding = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdValue: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  TNodeTexture2 = class(TVRMLNode)
  private
    { This is always <> nil.
      We use only IsNull to indicate whether we have or have not a texture here. }
    FTextureImage: TImage;
    FIsTextureLoaded: boolean;
  public
    { Pierwsze uzycie TextureImage spowoduje ze tekstura zostanie automatycznie
      zaladowana na podstawie pol obiektu: z pliku (pole filename)
      lub inlined (pole image). Pierwszenstwo ma tekstura z pliku,
      jesli filename = '' (lub wystapi jakis blad przy ladowaniu z filename
      ale VRMLNonFatalError to zignoruje) to zostanie uzyta tekstura inline.
      Jezeli nie ma tekstury inline i nie ma prawidlowego filename to tekstura
      zostanie zaladowana jako ImageNone. To ostatnie stwierdzenie ma
      znaczenie: mowi ono ze IsTextureImage znaczy co innego niz IsTextureLoaded.

      IsTextureLoaded mowi czy nastepne uzycie TextureImage spowoduje
      ReloadTexture, a wiec potencjalnie siegniecie do pliku
      (a takze blad VRMLNonFatalError jesli plik nie istnieje/nie mozna
      go odczytac itp.).

      IsTextureImage <=> not TextureImage.IsNull, a wiec samo
      uzycie IsTextureImage powoduje automatycznie zaladowanie tekstury
      (i ustawienie IsTextureLoaded na true).

      Dzieki temu mechanizmowi w standardowej sytuacji, tzn. gdy nigdy
      nie uzyjesz w programie ReloadTexture, blad w rodzaju
      "texture file foo.png does not exist" bedzie zgloszony do VRMLNonFatalError
      tylko raz - za kazdym nastepnym razem IsTextureImage = false ale
      IsTextureLoaded = true wiec odwolania do TextureImage beda po prostu
      zwracac ImageNone.

      BTW, taki node z IsTextureLoaded = true i IsTextureImage = false
      tez ma swoje znaczenie: oznacza "wylacz aktywna teksture".

      TextureImage class is always in (TRGBImage, TAlphaImage)
      po prostu dlatego ze takie sa formaty akceptowane w KambiGLUtils.
      sorry - to nie jest eleganckie, przeciez nie chcemy zeby OpenGL
      wplywal na ten modul nawet w taki subtelny sposob.   }
    function TextureImage: TImage;
    function IsTextureImage: boolean; { = not TextureImage.IsNull }
    property IsTextureLoaded: boolean read FIsTextureLoaded;
    procedure ReloadTexture;

    { krotki opis tego jak zdefiniowana jest tekstura. none jesli nie
      zdefiniowana, jakie jest filename, jakie jest inline. NIE okresla
      jak i jaka tekstura jest zaladowana. }
    function TextureDescription: string;

    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    destructor Destroy; override;

    property FdFilename: TSFString index 0 read GetFieldAsSFString;
    property FdImage: TSFImage index 1 read GetFieldAsSFImage;
    property FdWrapS: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdWrapT: TSFEnum index 3 read GetFieldAsSFEnum;

    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    property FdModel: TSFEnum index 4 read GetFieldAsSFEnum;
    property FdBlendColor: TSFVec3f index 5 read GetFieldAsSFVec3f;
  end;

  TNodeTexture2Transform = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec2f index 0 read GetFieldAsSFVec2f;
    property FdRotation: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdScaleFactor: TSFVec2f index 2 read GetFieldAsSFVec2f;
    property FdCenter: TSFVec2f index 3 read GetFieldAsSFVec2f;
    function TextureMatrixTransformation: TMatrix4Single;
  end;

  TNodeTextureCoordinate2 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdPoint: TMFVec2f index 0 read GetFieldAsMFVec2f;
  end;

  TNodeShapeHints = class(TVRMLNode)
  private
    function TryParseSpecialField(Lexer: TVRMLLexer): boolean; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdVertexOrdering: TSFenum index 0 read GetFieldAsSFEnum;
    property FdShapeType: TSFEnum index 1 read GetFieldAsSFEnum;
    property FdFaceType: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdCreaseAngle: TSFFloat index 3 read GetFieldAsSFFloat;
  end;

  { TNodeGeneralTransformation - wspolna klasa dla wszystkich node'ow ktorych
    jedynym celem jest zmodyfikowac aktualna macierz modelview.
    Wystarczy ze w kazdej z podklas napiszesz funkcje MatrixTransform: TMatrix4f
    no i oczywiscie zainicjujesz pola danego node'a. }
  TNodeGeneralTransformation = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    function MatrixTransformation: TMatrix4Single; virtual; abstract;
  end;

  TNodeMatrixTransform = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdMatrix: TSFMatrix index 0 read GetFieldAsSFMatrix;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  TNodeRotation = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRotation: TSFRotation index 0 read GetFieldAsSFRotation;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  { This node is actually from Inventor. It's not in VRML 1.0 spec.
    But some invalid VRML 1.0 models use it,
    e.g. [http://www-vrl.umich.edu/sel_prj/EECS498/]. }
  TNodeRotationXYZ = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdAxis: TSFEnum index 0 read GetFieldAsSFEnum;
    property FdAngle: TSFFloat index 1 read GetFieldAsSFFloat;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  TNodeScale = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdScaleFactor: TSFVec3f index 0 read GetFieldAsSFVec3f;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  TNodeTransform = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property FdRotation: TSFRotation index 1 read GetFieldAsSFRotation;
    property FdScaleFactor: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdScaleOrientation: TSFRotation index 3 read GetFieldAsSFRotation;
    property FdCenter: TSFVec3f index 4 read GetFieldAsSFVec3f;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  TNodeTranslation = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec3f index 0 read GetFieldAsSFVec3f;
    function MatrixTransformation: TMatrix4Single; override;
  end;

  TVRMLCameraKind = (ckOrthographic, ckPerspective);
  TNodeGeneralCameraClass = class of TNodeGeneralCamera;

  { GeneralCamera - wspolna klasa dla wszystkich kamer VRML'a. }
  TNodeGeneralCamera = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdPosition: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property FdOrientation: TSFRotation index 1 read GetFieldAsSFRotation;
    property FdFocalDistance: TSFFloat index 2 read GetFieldAsSFFloat;
    property FdHeightAngle: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdDirection: TMFVec3f index 4 read GetFieldAsMFVec3f;
    property FdUp: TMFVec3f index 5 read GetFieldAsMFVec3f;
    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    property FdNearDistance: TSFFloat index 6 read GetFieldAsSFFloat;
    property FdFarDistance: TSFFloat index 7 read GetFieldAsSFFloat;

    class function CameraKind: TVRMLCameraKind; virtual; abstract;

    { Oblicz CamPos, Dir, Up na podstawie aktualnych ustawien kamery - zgodnie
      ze specyfikacja VRMLa,

@preformatted(
  CamPos = FdPosition,
  CamDir = (0, 0, -1) rotated by FdOrientation,
  CamUp = (0, 1, 0) rotated by FdOrientation,
  and CamPos, Dir, Up are transformed by given CamTransform.
)
      (you should give here the actual VRML transformation at the point in file
      where camera is defined).

      Dodajemy do tego dodatki Kambiego :
      jesli FdDirection.Length > 0 to CamDirection nie jest liczone z
      FdOrientation ale jest brane wprost z FdDirection.Items[0].
      Podobnie dla FdUp.

      Zwraca zawsze znormalizowany CamDir i CamUp bo:
      @orderedList(
        @item(
          zeby zmusic cie do stosowania konsekwentnej zasady wyrazonej na
          poczatku VRMLNodes i nie pisania kodu ktory w jakis sposob bylby
          uzalezniony od podawanych tu CamDir. To dlatego ze
          pola FdOrientation / FdDirection / FdUp tego wezla NIE sluza do
          podawania czegokolwiek poza kierunkami, a wiec ich dlugosc jest
          niewazna. No i stosujac FdOrientation, a wiec uzywajac standardowego
          VRMLa 1.0, nie mozna nawet podac dlugosci FdDirection/FdUp innej
          niz 1.)
        @item(
          Normalizujemy tutaj bo w implementacji tej funkcji czesto wiemy
          ze nie trzeba normalizowac, np. gdy zwracamy standardowe dir/up
          kamery obrocone o orientation to nie potrzebujemy robic zadnej
          normalizacji bo wiemy ze wynik ma dlugosc 1. W ten sytuacji byloby
          nieoptymalne gdybys musial po wywolaniu tej procedury wywolac
          NormalizeTo1st(CamDir), bo przeciez czesto w ponizszej procedurze
          wiadomo ze nie trzea normalizowac (a wiec wywolanie NormalizeTo1st
          i wywolywany w jego srodku Sqrt sa zbedne).)
      )

      TODO: FocalDistance powinien tez byc tu zwracany (po przeliczeniu
      przez CamTransform) }
    procedure CalcCamera(const CamTransform: TMatrix4Single;
      var CamPos, CamDir, CamUp: TVector3Single);
  end;

  TNodeOrthographicCamera = class(TNodeGeneralCamera)
    class function ClassNodeTypeName: string; override;
    class function CameraKind: TVRMLCameraKind; override;
  end;

  TNodePerspectiveCamera = class(TNodeGeneralCamera)
    class function ClassNodeTypeName: string; override;
    class function CameraKind: TVRMLCameraKind; override;
  end;

  TNodeGeneralLight = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdOn: TSFBool index 0 read GetFieldAsSFBool;
    property FdIntensity: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdColor: TSFColor index 2 read GetFieldAsSFColor;
  end;

  TObjectsListItem_1 = TNodeGeneralLight;
  {$I objectslist_1.inc}
  TNodeGeneralLightsList = class(TObjectsList_1);

  TNodeDirectionalLight = class(TNodeGeneralLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdDirection: TSFVec3f index 3 read GetFieldAsSFVec3f;
  end;

  TNodeGeneralPositionalLight = class(TNodeGeneralLight)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdLocation: TSFVec3f index 3 read GetFieldAsSFVec3f;
    property FdAttenuation: TSFVec3f index 4 read GetFieldAsSFVec3f;

    { Attenuation obliczaja attenuation (tzn. wzorek 1/max( attenuation[0] + ...)
      zgodnie ze specyfik. VRMLa 97. Poniewaz obliczenie parametru DistanceToLight
      moze czasem wiazac sie ze spora strata czasu mozesz sprawdzic najpierw
      wartosc DistanceNeededForAttenuation --- jezeli jest false, to wartosc
      parametru DistanceToLight dla Attenutaion() nie ma znaczenia (mozesz
      podac cokolwiek).

      PAMIETAJ --- DistanceToLight powinien byc w lights coordinate system.
      TODO: raytracer nie realizuje teraz tego "PAMIETAJ" powyzej
      TODO: nie wiem czy OpenGL realizuje to "PAMIETAJ" powyzej
      w swoich swiatlach, check w OpenGL spec }
    function DistanceNeededForAttenuation: boolean;
    function Attenuation(const DistanceToLight: Single): Single; overload;
    function Attenuation(const DistanceToLight: Double): Double; overload;
  end;

  TNodePointLight = class(TNodeGeneralPositionalLight)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
  end;

  TNodeSpotLight = class(TNodeGeneralPositionalLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdDirection: TSFVec3f index 5 read GetFieldAsSFVec3f;
    property FdDropOffRate: TSFFloat index 6 read GetFieldAsSFFloat;
    property FdCutOffAngle: TSFFloat index 7 read GetFieldAsSFFloat;

    { nieznormalizowany wykladnik dla spot'a (na podstawie dropOffAngle) }
    function SpotExp: Single;
  end;

  TNodeGroup = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
  end;

  { A general class that can ce used as a separator, something that
    pushes and pops all attribs and matrices.
    It is used in implementation of Separator, WWWAnchor and WWWInline.}
  TNodeGeneralSeparator = class(TVRMLNode)
  private
    OriginalState: TVRMLGraphTraverseState;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeSeparator = class(TNodeGeneralSeparator)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRenderCulling: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  TNodeSwitch = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdWhichChild: TSFLong index 0 read GetFieldAsSFLong;
    procedure ChildrenToEnter(var FirstChild, LastChild: integer); override;
  end;

  TNodeTransformSeparator = class(TVRMLNode)
  private
    OriginalMatrix: TMatrix4Single;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
  end;

  { TODO: node anchor dziala jak Separator, wartosci jego pol nie maja
    nigdzie zadnego znaczenia. Trzebaby to zaimplementowac, co wymaga
    1) TURLDataStream, patrz komentarz przy TNodeWWWInline
    2) mechaznimu picking - to juz mamy w view3dscene }
  TNodeWWWAnchor = class(TNodeGeneralSeparator)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdName: TSFString index 0 read GetFieldAsSFString;
    property FdDescription: TSFString index 0 read GetFieldAsSFString;
    property FdMap: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  { gdy chcemy operowac na scenie juz po jej zaladowaniu, bezposrednio
    lub poprzez metode w rodzaju TVRMLScene.EnumNodes, jest istotne
    gdzie w hierarchii sceny znajduja sie Inlined nodes. Odpowiadam :
    sa one SubNode'ami WWWInline. Mozesz testowac ChildrenCount <> 0
    aby sprawdzic czy Inlined zostaly juz zaladowane. Mozesz
    zazadac ich natychmiastowego zaladowania uzywajac LoadInlined. }
  TNodeWWWInline = class(TNodeGeneralSeparator)
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    { Call LoadInlined to load Inlined NOW. If Inlined is already loaded,
      than : if CanReload = true Inlined will be freed and loaded again,
      else (if CanReload = false) nothing will happen.

      LoadInlined(false) will be called automatically in BeforeTraverse.

      Acha --- sorry : naturalnie tylko FdName jako nazwa pliku na lokalnym
      systemie plikow jest obslugiwana chwilowo. W ogole, generalnie
      to fajnie byloby gdyby TVRMLScene.Create przyjmowalo URL a nie
      filename albo jeszcze lepiej gdyby miec strumien TURLDataStream
      ktory moze podawac dane identyfikowane przez URL...
      Moze nie w najblizszym czasie, ale zamierzam cos takiego kiedys
      zaimplementowac - szkielet (dla http) juz zrobilem w iswb. }
    procedure LoadInlined(CanReload: boolean);

    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdName: TSFString index 0 read GetFieldAsSFString;
    property FdBboxSize: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property FdBboxCenter: TSFVec3f index 2 read GetFieldAsSFVec3f;
  end;

  TNodeFog = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdColor: TSFColor index 0 read GetFieldAsSFColor;
    property FdFogType: TSFString index 1 read GetFieldAsSFString;
    property FdVisibilityRange: TSFFloat index 2 read GetFieldAsSFFloat;
  end;

  TNodeBackground = class(TVRMLNode)
  private
    FBgImagesLoaded: boolean; { = false }

    { if not FBgImagesLoaded it should be always equal to BackgroundImagesNone
      (this is not important for interface of this class but we will use
      this in our implementation) }
    FBgImages: TBackgroundImages;

    FAllowedBgImagesClasses: TDynArrayImageClasses;
    function GetBgImages: TBackgroundImages;
    procedure UnloadImages;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    property FdGroundAngle: TMFFloat index 0 read GetFieldAsMFFloat; { [0, Pi/2] }
    property FdGroundColor: TMFColor index 1 read GetFieldAsMFColor; { [0, 1] }
    property FdBackUrl: TMFString index 2 read GetFieldAsMFString;
    property FdBottomUrl: TMFString index 3 read GetFieldAsMFString;
    property FdFrontUrl: TMFString index 4 read GetFieldAsMFString;
    property FdLeftUrl: TMFString index 5 read GetFieldAsMFString;
    property FdRightUrl: TMFString index 6 read GetFieldAsMFString;
    property FdTopUrl: TMFString index 7 read GetFieldAsMFString;
    property FdSkyAngle: TMFFloat index 8 read GetFieldAsMFFloat; {  [0, Pi] }
    property FdSkyColor: TMFColor index 9 read GetFieldAsMFColor; {  [0, 1] }

    procedure Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList); override;

    { Pierwsze uzycie BgImages (albo pierwsze uzycie BgImages po Parse)
      automatycznie zaladuje obrazki z URLi
      BackUrl/BottomUrl itd. Obrazki dla ktorych zaden osiagalny URL nie byl
      podany zostana ustawione na ImageNone.
      Obrazki beda mialy Kind wsrod AllowedBgImagesKinds.

      Obrazki beda zawsze odpowiadaly URLom w polach Fd*Url - dlatego po
      zrobieniu Parse na tym obiekcie nastepne wywolanie BgImages bedzie
      ladowalo je od nowa. }
    property BgImages: TBackgroundImages read GetBgImages;

    { Czy obrazki juz sa zaladowane ? W zasadzie ta funkcja nigdy nie powinna
      ci byc potrzebna skoro cale ladowanie / zwalnianie obrazkow jest
      robione automatycznie. Ale moze ci przydac : ona mowi czy najblizsze
      wywolanie BgImages wywola ReloadBgImages czy nie. }
    property BgImagesLoaded: boolean read FBgImagesLoaded;

    { Wymusza przeladowanie obrazkow BgImages, nawet jesli BgImages zostalo
      juz raz wywolane (a wiec obrazki juz raz zostaly zaladowane).
      Uzywaj np. jezeli podejrzewasz ze zawartosc obrazkow zmienila sie
      na dysku. }
    procedure ReloadBgImages;

    { Wszystkie obrazki na BgImages (ktore nie sa nil) maja zawsze
      klase wsrod AllowedBgImagesClasses.

      Aby zapewnic ze to stwierdzenie zawsze zachodzi jezeli BgImages zostalo
      juz chociaz raz wywolane to zmiana AllowedBgImagesClasses spowoduje
      uniewaznienie BgImages (tzn. nastepne wywolanie BgImages bedzie
      musialo przeladowac obrazki od nowa) }
    property AllowedBgImagesClasses: TDynArrayImageClasses
      read FAllowedBgImagesClasses; { = [], so all image classes are allowed }
    procedure SetAllowedBgImagesClasses(const Value: array of TImageClass);
  end;

  TNodeKambiTriangulation = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdQuadricSlices: TSFLong index 0 read GetFieldAsSFLong;
    property FdQuadricStacks: TSFLong index 1 read GetFieldAsSFLong;
    property FdRectDivisions: TSFLong index 2 read GetFieldAsSFLong;

    { zwracaja wartosc z odpowiedniego pola FdXxx lub,
      jesli ta wartosc jest -1, zwracaja Detail_Xxx.

      (Jesli ta wartosc jest nieprawidlowa to wywoluja VRMLNonFatalError
      a potem "po cichu" zmieniaja ta wartosc na wartosc wzieta z Detail_Xxx.
      A wiec poprawiaja blednego VRMLa.) }
    function QuadricStacks: Cardinal;
    function QuadricSlices: Cardinal;
    function RectDivisions: Cardinal;
  end;

{ very very special node --------------------------------------------------- }

  (* @abstract(TNodeUnknown represents a node with an unrecognized type.)

    If we approach some node with not recognized name we create TNodeUnknown.
    TNodeUnknown has very special Parse method.
    We want to use "fields" and "isA" VRML 1.0 extensibility features here.
    (sorry - these extensibility features are not implemented yet;
     for now all unrecognized nodes are of kind 1))
    We have three cases :

    @orderedList(
      @item(
        Unknown node that doesn't have the "fields" field. It CAN be parsed
        by simply looking for matching "}". Such node will use default
        Render, Init/CloseGL and BoundingBox method implementations -
        wiec w praktyce taki node nie bedzie robil zadnego renderingu i mial
        BoundingBox = EmptyBox3d.
        Uwaga - *AllowedChildren bedzie = false.)

      @item(
        node ktory ma pole "fields" ale nie ma pola "isA" albo nie jest
        tam podany zaden znany typ node'a. Taki node bedzie mial
        wypelnione podczas parsowania pola Fields i bedzie parsowany
        normalnie ale ciagle nie bedzie mial zadnego wplywu na renderowanie
        sceny VRML'a, podobnie jak w przypadku 1. (chociaz jego dzieci BEDA
        mialy - taki node bedzie zasadniczo dzialal jak node Group)
        Uwaga - *AllowedChildren bedzie = true.)

      @item(
        wreszcie node ktory ma pola "fields" i "isA" i znalezlismy
        wsrod isA jakis znany nam typ node'a. Czyli wiemy ze mamy
        jakies rozszerzenie znanego nam node'a. Taki node bedzie mial
        dynamicznie utworzone pola Fields i bedzie normalnie parsowany
        (tak jak typ 2) a ponadto taki node bedzie mial taki wplyw
        na model VRML'a jakby byl typem node'a ktory jest pierwszym
        znanym nam typem na liscie "isA".
        Uwaga - *AllowedChildren bedzie = *AllowedChildren znanego typu node'a.)
    )

    Ten node nigdy nie powinien byc tworzony tak jak normalny node
    --- wrecz normalne wirtualne Create(const AName: string) spowoduje
    wyjatek ! Tworz obiekty tego typu tylko uzywajac CreateUnknownParse,
    ew. mozesz uzyc CreateUnknown (jezeli tylko znajdziesz jakis sens
    dla uzycia CreateUnknown bez Parse...).

    Spostrzezenie : ten mechanizm jest calkiem dobry - node'y kazdego
    typu, nawet 1, moga byc nazywane i mozna sie pozniej do nich
    odwolywac przez USE. Jezeli node jest typu 2 i 3 to nawet
    ich SubNode'y beda wlaczone w ten standardowy mechanizm !
    Po Parse node'u unknown typu 1) robimy VRMLNonFatalError
    (bo dokladnie to zaszlo --- to jest nieprawidlowy node, ale umiemy sobie
    poradzic).
  *)
  TNodeUnknown = class(TVRMLNode)
  private
    fNodeTypeName: string;
  public
    function NodeTypeName: string; override;
    procedure Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList); override;

    { base Create will throw exception. Always use CreateUnknown*

      @noAutoLinkHere }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    constructor CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
    constructor CreateUnknownParse(const ANodeName, ANodeTypeName :string;
      Lexer: TVRMLLexer; NodeNameBinding: TStringList);
  end;

{ TraverseStateLastNodesClasses ---------------------------------------------- }

const
  { opis patrz TTraverseStateLastNodes }
  TraverseStateLastNodesClasses :
    array[0..HighTraverseStateLastNodes] of TVRMLNodeClass =
    ( TNodeCoordinate3, TNodeShapeHints, TNodeFontStyle,
      TNodeMaterial, TNodeMaterialBinding, TNodeNormal, TNodeNormalBinding,
      TNodeTexture2, TNodeTextureCoordinate2,
      TNodeKambiTriangulation
      { additions here must be synchronized with additions to
        TTraverseStateLastNodes }
    );

{ TNodesManager ------------------------------------------------------------ }

type
  ENodesManagerError = class(EVRMLError);
  ENodeClassRegisterError = class(ENodesManagerError);
  TNodesManager = class
  private
    { Strings[] to ClassNodeTypeName. Objects[] to odpowiednie klasy. }
    Registered: TStringList;
  public
    { mozesz rejestrowac tylko klasy o ClassNodeTypeName <> '' (w tej procedurze
      to sprawdzimy i ew. rzucimy wyjatek ENodeClassRegisterError).
      Nie mozesz zarejestrowac dwa razy tej samej klasy. W tym momencie
      spowoduje to ENodeClassRegisterError ale w przyszlosci zamierzam
      dodac flage do tej procedury ktora bedzie kontrolowala co wtedy zrobic
      (np. zignorowac blad i dodac, zignorowac i nie dodawac itd.). }
    procedure RegisterNodeClass(NodeClass: TVRMLNodeClass);
    procedure RegisterNodeClasses(const NodeClasses: array of TVRMLNodeClass);

    { This unregisters class NodeClass, i.e. it removes it from
      our map table.

      @raises(ENodesManagerError if NodeClass.ClassNodeTypeName = ''
        (so it cannot be even registered), or if
        ((NodeClass was not registered) and ErrorIfNotRegistered)) }
    procedure UnRegisterNodeClass(NodeClass: TVRMLNodeClass;
      ErrorIfNotRegistered: boolean = true);

    { NodesManager zostal stworzony wlasnie po to aby zaimplementowac
      funkcje TypeNameToClass: odwzorowuje ona nazwe typu VRMLa
      na klase VRMLa ktora ma takie samo ClassNodeTypeName. Aby takie cos
      przeprowadzic potrzebny byl gdzies ekwiwalent globalnej tablicy
      przechowujacej wszystkie stworzone klasy wezlow VRMLa - takim
      odpowiednikiem jest wlasnie ta klasa do ktorej trzeba rejestrowac wezly.
      Bedzie szukac wsrod zarejestrowanych klas klasy o zadanym ClassNodeTypeName.
      Jesli nie znajdzie zwroci nil. }
    function NodeTypeNameToClass(const ANodeTypeName: string): TVRMLNodeClass;

    constructor Create;
    destructor Destroy; override;
  end;

var
  { tworzony i niszczony w init/fini tego modulu }
  NodesManager: TNodesManager;

{ global procedures ---------------------------------------------------------- }

(*
  parse node : [ DEF <nodename> ] <nodetype> { node-content } or USE <nodename>
  NodeNameBinding jest lista bez duplikatow okreslajaca wszystkie dotychczasowe
    nazwy node'ow razem z ich instancjami. Jezeli kilka instancji mialo takie
    samo NodeName to na liscie znajduje sie ostatni z nich (ostatni w sensie
    pozycji w pliku, czy raczej w strumieniu tokenow Lexera). Tym samym
    jest chyba jasne do czego uzywamy NodeNameBinding : do realizacji
    konstrukcji "USE <nodename>". Procedura ParseNode nie moze modyfikowac
    tej listy, to zadania ma wykonywac TVRMLNode.Parse.
*)
function ParseNode(Lexer: TVRMLLexer; NodeNameBinding: TStringList; const AllowedNodes: boolean): TVRMLNode;

{ parse VRML file : parse whole VRML file, returning it's root node.

  Note that you must pass here TPeekCharStream class, not just any
  generic TStream class. But it's not a problem, really, because
  you can wrap any class inside TPeekCharStream descendant. E.g. do
  @longcode(#
    ParseVRMLFile(TBufferedReadStream.Create(MyStream, false), WWWBasePath)
  #)

  Note that this function can't handle compressed data (VRML files are
  sometimes compressed with gzip). You should already pass here a stream
  with uncompressed text data. }
function ParseVRMLFile(Stream: TPeekCharStream;
  const WWWBasePath: string): TVRMLNode; overload;

{ FileName to nazwa istniejacego pliku (wzgledna lub bezwzgledna).
  Jezeli AllowStdIn to jesli filename = '-' to odczytamy model z StdInStream,
  w tym przypadku WWWBasePath bedzie ustawione na GetCurrentDir.

  This function can handle files compressed with gzip
  (it just internally filters file contents with TGZFileStream,
  uncompressing it on the fly). }
function ParseVRMLFile(const FileName: string;
  AllowStdIn: boolean): TVRMLNode; overload;

{ gdy podczas robienia czegos na VRMLu wystapi blad ale taki po ktorym mozemy
  kontynuowac dzialanie (np. nie mozna odczytac tekstury ze wskazanego pliku)
  to zostanie wywolane VRMLNonFatalError z odpowiednim stringiem.
  Mozesz zrobic z tym co chcesz - zignorowac, wyrzucic exception,
  wypisac przez WarningWrite - slowem, cokolwiek. Jak widac, domyslnie
  jest exception klasy EVRMLError.
}
procedure VRMLNonFatalError_WarningWrite(const s: string);
procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
procedure VRMLNonFatalError_Ignore(const s: string);
type
  TVRMLNonFatalErrorProc = procedure(const s: string);
var
  VRMLNonFatalError: TVRMLNonFatalErrorProc = VRMLNonFatalError_RaiseEVRMLError;

{ SaveToVRMLFile writes whole VRML file (with signature '#VRML V1.0 ascii'
  and '# '+PrecedingComment, if PrecedingComment <> '') with RootNode =
  given Node }
procedure SaveToVRMLFile(Node: TVRMLNode; Stream: TStream; const PrecedingComment: string); overload;
procedure SaveToVRMLFile(Node: TVRMLNode; const Filename, PrecedingComment: string); overload;

const
  VRMLCameraKindToStr: array[TVRMLCameraKind]of string =
  ('Orthographic', 'Perspective');
  VRMLCameraKindToVRMLNodeName: array[TVRMLCameraKind]of string =
  ('OrthographicCamera', 'PerspectiveCamera');

const
  { consts for TNodeAsciiText.FdJustification.Value }
  JUSTIFICATION_LEFT = 0;
  JUSTIFICATION_CENTER = 1;
  JUSTIFICATION_RIGHT = 2;

  { consts for TNode(Material|Normal)Binding.FdValue.Value }
  BIND_DEFAULT = 0;
  BIND_OVERALL = 1;
  BIND_PER_PART = 2;
  BIND_PER_PART_INDEXED = 3;
  BIND_PER_FACE = 4;
  BIND_PER_FACE_INDEXED = 5;
  BIND_PER_VERTEX = 6;
  BIND_PER_VERTEX_INDEXED = 7;

  { consts for TNodeShapeHints.FdVertexOrdering.Value }
  VERTORDER_UNKNOWN = 0;
  VERTORDER_CLOCKWISE = 1;
  VERTORDER_COUNTERCLOCKWISE = 2;

  { consts for TNodeShapeHints.FdShapeType.Value }
  SHTYPE_UNKNOWN = 0;
  SHTYPE_SOLID = 1;

  { consts for TNodeShapeHints.FdFaceType.Value }
  FACETYPE_UNKNOWN = 0;
  FACETYPE_CONVEX = 1;

  { consts for TNodeFontStyle.FdFamily.Value }
  FSFAMILY_SERIF = 0;
  FSFAMILY_SANS = 1;
  FSFAMILY_TYPEWRITER = 2;

  { consts for TNodeFontStyle.FdStyleFlags[] }
  FSSTYLE_BOLD = 0;
  FSSTYLE_ITALIC = 1;

  { consts for TNodeCone.FdParts.Flags[] }
  CONE_PARTS_SIDES = 0;
  CONE_PARTS_BOTTOM = 1;

  { consts for TNodeCylinder.FdParts.Flags[] }
  CYLINDER_PARTS_SIDES = 0;
  CYLINDER_PARTS_TOP = 1;
  CYLINDER_PARTS_BOTTOM = 2;

  { consts for TNodeTexture2.FdWrapS/WrapT.Value }
  TEXWRAP_REPEAT = 0;
  TEXWRAP_CLAMP = 1;

{ sorry - these Detail parameters below should depend on object's distance
  from viewer. But there is a problem : we need those parameters defined
  when implementing Vertices/TrianglesCount and Triangulate. }
var
  { cylinder, cone, sphere and disk slices/stacks (slices for all objects
      must be equal to perfectly "match" when objects are connected
      (e.g. sphere connected with cylinder). Stacks and RectDivisions
      nie sa do tego zmuszone ale i tak nie ma zadnego sensownego powodu
      zeby z gory mowic ze dana bryla potrzebuje mniej stacks a inna wiecej).
    For the meaning of Detail_Quadric* consts look at definition of glu
      quadric functions (it is not guaranteed that our code will use this
      functions but we will always honour this Detail parameters in the same way).
    For the meaning of Detail_RectDivisions (used only in Cube for now) look
      at KambiGLUtils.DrawGLPlane.

    For now, you can change these variables only _before_ using _anything_
    from this module.

    This variables _must_ always honour Min values listed below. }
  Detail_QuadricSlices: Cardinal = 30;
  Detail_QuadricStacks: Cardinal = 20;
  Detail_RectDivisions: Cardinal = 2;

const
  { uzywaj w programie zawsze tych stalych zamiast zakladac ze maja one
    konkretne wartosci, ale mozesz oczywiscie przyjac zalozenie ze na pewno
    sa one Cardinalami (sa >=0) }
  MinQuadricSlices: Cardinal = 3; { mimo ze OpenGL akceptuje minimum 2, ale dla 2 wynik jest bez sensu }
  MinQuadricStacks: Cardinal = 2; { mimo ze OpenGL akceptuje minimum 1, ale dla 1 wynik jest bez sensu }
  MinRectDivisions: Cardinal = 0;

{$undef read_interface}

implementation

uses
  { fonts for AsciiText }
  { Bitstream Vera Sans }
  TTF_BitstreamVeraSans_Unit,
  TTF_BitstreamVeraSans_Bold_Unit,
  TTF_BitstreamVeraSans_Italic_Unit,
  TTF_BitstreamVeraSans_Bold_Italic_Unit,
  { Bitstream Vera Sans Mono }
  TTF_BitstreamVeraSansMono_Unit,
  TTF_BitstreamVeraSansMono_Bold_Unit,
  TTF_BitstreamVeraSansMono_Italic_Unit,
  TTF_BitstreamVeraSansMono_Bold_Italic_Unit,
  { Bitstream Vera Serif }
  TTF_BitstreamVeraSerif_Unit,
  TTF_BitstreamVeraSerif_Bold_Unit,
  TTF_BitstreamVeraSerif_Italic_Unit,
  TTF_BitstreamVeraSerif_Bold_Italic_Unit,

  Math, Triangulator, Object3dAsVRML, KambiZStream;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_3.inc}
{$I dynarray_1.inc}

{ consts in this module ---------------------------------------------------- }

{ Combines BasePath with RelPath. BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath. }
{ sorry - zaimplementowac ta funkcje porzadnie i przerzucic do KambiUtils.
  Na razie to jest takie "zazwyczaj dziala". }
function CombinePaths(const BasePath, RelPath: string): string; forward;

{ TDynActiveLightArray --------------------------------------------------------- }

function TDynActiveLightArray.IndexOfLightNode(LightNode: TNodeGeneralLight): integer;
begin
 for result := 0 to High do
  if Items[result].LightNode = LightNode then exit;
 result := -1;
end;

procedure TDynActiveLightArray.AddLight(ALightNode: TNodeGeneralLight; const ATransform: TMatrix4Single);
begin
 IncLength;
 with Items[High] do
 begin
  LightNode := ALightNode;
  Transform := ATransform;
  if LightNode is TNodeGeneralPositionalLight then
   TransfLocation := MultMatrixPoint(Transform,
     TNodeGeneralPositionalLight(LightNode).FdLocation.Value);

  if LightNode is TNodeSpotLight then
   TransfNormDirection := Normalized( MultMatrixPointNoTranslation(Transform,
     TNodeSpotLight(LightNode).FdDirection.Value) ) else
  if LightNode is TNodeDirectionalLight then
   TransfNormDirection := Normalized( MultMatrixPointNoTranslation(Transform,
     TNodeDirectionalLight(LightNode).FdDirection.Value) );
 end;
end;

{ TVRMLGraphTraverseState -------------------------------------------------------- }

procedure TVRMLGraphTraverseState.CommonCreate;
begin
 inherited Create;
 ActiveLights := TDynActiveLightArray.Create;
end;

constructor TVRMLGraphTraverseState.CreateCopy(Source: TVRMLGraphTraverseState);
begin
 CommonCreate;

 CurrMatrix := Source.CurrMatrix;
 CurrTextureMatrix := Source.CurrTextureMatrix;
 FLastNodes := Source.FLastNodes;
 ActiveLights.AppendDynArray(Source.ActiveLights);
end;

constructor TVRMLGraphTraverseState.Create(const ADefaultLastNodes: TTraverseStateLastNodes);
begin
 CommonCreate;

 CurrMatrix := IdentityMatrix4Single;
 CurrTextureMatrix := IdentityMatrix4Single;
 FLastNodes := ADefaultLastNodes;
end;

destructor TVRMLGraphTraverseState.Destroy;
begin
 ActiveLights.Free;
 inherited;
end;

{ TVRMLNode ------------------------------------------------------------------- }

constructor TVRMLNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited Create;
 FAllowedChildren := false;
 FParsingAllowedChildren := false;

 FNodeName := ANodeName;
 FWWWBasePath := AWWWBasePath;

 FChildren := TVRMLNodesList.Create;
 FParents := TVRMLNodesList.Create;
 Fields := TVRMLFieldsList.Create;
end;

destructor TVRMLNode.Destroy;
begin
 if FChildren <> nil then RemoveAllChildren;

 Fields.FreeWithContents;
 FChildren.Free;
 FParents.Free;
 inherited;
end;

procedure TVRMLNode.AddChild(Index: Integer; child: TVRMLNode);
begin
 Check( {is child allowed in AllowedChildren ?} AllowedChildren,
   'Node '+NodeTypeName+' is not allowed to have child node of type '+
   Child.NodeTypeName);
 child.FParents.Add(Self);
 FChildren.Insert(Index, child);
end;

procedure TVRMLNode.AddChild(child: TVRMLNode);
begin
 AddChild(FChildren.Count, child);
end;

procedure TVRMLNode.RemoveChild(i: integer);
var c: TVRMLNode;
begin
 c := FChildren[i];
 FChildren.Delete(i);
 c.FParents.Delete(Self);
 if c.FParents.Count = 0 then c.Free;
end;

procedure TVRMLNode.RemoveAllChildren;
begin
 while FChildren.Count > 0 do RemoveChild(0);
end;

function TVRMLNode.GetChildrenItem(i: integer): TVRMLNode; begin result := FChildren[i] end;
function TVRMLNode.GetParentsItem(i: integer): TVRMLNode; begin result := FParents[i] end;

function TVRMLNode.ChildrenCount: integer; begin result := FChildren.Count end;
function TVRMLNode.ParentsCount: integer; begin result := FParents.Count end;

procedure TVRMLNode.FreeRemovingFromAllParents;
var i, j: integer;
begin
 if Self = nil then exit;

 for i := 0 to FParents.Count-1 do
 begin
  j := FParents[i].FChildren.IndexOf(Self);
  FParents[i].FChildren.Delete(j);
  {nie musimy sie tu martwic usuwaniem naszego Parenta z listy FParents ktora
   wlasnie przegladamy bo przeciez i tak zaraz zrobimy sobie Destroy; }
 end;
 Self.Destroy;
end;

procedure TVRMLNode.ChildrenToEnter(var FirstChild, LastChild: integer);
begin
 FirstChild := 0;
 LastChild := ChildrenCount-1;
end;

procedure TVRMLNode.BeforeTraverse(var State: TVRMLGraphTraverseState); begin end;
procedure TVRMLNode.MiddleTraverse(State: TVRMLGraphTraverseState); begin end;
procedure TVRMLNode.AfterTraverse(var State: TVRMLGraphTraverseState); begin end;

{$define ITERATE_CHILDREN_DECLARE:=
var FirstChild, LastChild, i: integer; }

{$define ITERATE_CHILDEN_INIT:=
ChildrenToEnter(FirstChild, LastChild)}

{$define ITERATE_CHILDREN_LOOP:=
for i := max(FirstChild, 0) to min(LastChild, ChildrenCount-1) do}

procedure TVRMLNode.Traverse(State: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass; TraversingFunc: TTraversingFunc);
ITERATE_CHILDREN_DECLARE
var LastNodesIndex: Integer;
begin
 BeforeTraverse(State);
 try
  if Self is NodeClass then TraversingFunc(Self, State);
  MiddleTraverse(State);

  ITERATE_CHILDEN_INIT;
  ITERATE_CHILDREN_LOOP Children[i].Traverse(State, NodeClass, TraversingFunc);
 finally AfterTraverse(State) end;

 LastNodesIndex := TraverseStateLastNodesIndex;
 if LastNodesIndex <> -1 then State.FLastNodes.Nodes[LastNodesIndex] := Self;
end;

function TVRMLNode.NodeTypeName: string;
begin
 result := ClassNodeTypeName;
end;

class function TVRMLNode.ClassNodeTypeName: string;
begin
 result := '';
end;

function TVRMLNode.GetField(i: integer): TVRMLField; begin result := Fields[i] end;
function TVRMLNode.GetFieldAsSFBitMask(i: integer): TSFBitMask; begin result := TSFBitMask(Fields[i]) end;
function TVRMLNode.GetFieldAsSFBool(i: integer): TSFBool; begin result := TSFBool(Fields[i]) end;
function TVRMLNode.GetFieldAsSFColor(i: integer): TSFColor; begin result := TSFColor(Fields[i]) end;
function TVRMLNode.GetFieldAsSFEnum(i: integer): TSFEnum; begin result := TSFEnum(Fields[i]) end;
function TVRMLNode.GetFieldAsSFFloat(i: integer): TSFFloat; begin result := TSFFloat(Fields[i]) end;
function TVRMLNode.GetFieldAsSFImage(i: integer): TSFImage; begin result := TSFImage(Fields[i]) end;
function TVRMLNode.GetFieldAsSFLong(i: integer): TSFLong; begin result := TSFLong(Fields[i]) end;
function TVRMLNode.GetFieldAsSFMatrix(i: integer): TSFMatrix; begin result := TSFMatrix(Fields[i]) end;
function TVRMLNode.GetFieldAsSFRotation(i: integer): TSFRotation; begin result := TSFRotation(Fields[i]) end;
function TVRMLNode.GetFieldAsSFString(i: integer): TSFString; begin result := TSFString(Fields[i]) end;
function TVRMLNode.GetFieldAsSFVec2f(i: integer): TSFVec2f; begin result := TSFVec2f(Fields[i]) end;
function TVRMLNode.GetFieldAsSFVec3f(i: integer): TSFVec3f; begin result := TSFVec3f(Fields[i]) end;
function TVRMLNode.GetFieldAsMFColor(i: integer): TMFColor; begin result := TMFColor(Fields[i]) end;
function TVRMLNode.GetFieldAsMFLong(i: integer): TMFLong; begin result := TMFLong(Fields[i]) end;
function TVRMLNode.GetFieldAsMFVec2f(i: integer): TMFVec2f; begin result := TMFVec2f(Fields[i]) end;
function TVRMLNode.GetFieldAsMFVec3f(i: integer): TMFVec3f; begin result := TMFVec3f(Fields[i]) end;
function TVRMLNode.GetFieldAsMFFloat(i: integer): TMFFloat; begin result := TMFFloat(Fields[i]) end;
function TVRMLNode.GetFieldAsMFString(i: integer): TMFString; begin result := TMFString(Fields[i]) end;

constructor TVRMLNode.CreateParse(const ANodeName: string; Lexer: TVRMLLexer; NodeNameBinding: TStringList);
begin
 Create(ANodeName, '');
 Parse(Lexer, NodeNameBinding);
end;

procedure TVRMLNode.Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList);
var ni: integer;
    ThisIsField: boolean;
begin
 RemoveAllChildren;

 {parse node}
 Lexer.CheckTokenIs(vtOpenWasBracket);
 Lexer.NextToken;
 while Lexer.Token <> vtCloseWasBracket do
 begin
  ThisIsField := false;

  {gdybym wiedzial ze wszystkie node'y sa standardowe (zgodne ze specyfikacja
   VRML'a) to moglbym tu sprawdzac czy Lexer.TokenName[0] in 'a'..'z'.
   Jezeli nie - wiedzialbym juz ze to na pewno nie jest nazwa pola bo
   wszystkie nazwy pol zaczynaja sie z malej litery. Ale nie mozemy tak
   zrobic poniewaz zarowno w VRML'u 1.0 jak 2.0 (97) mozna definiowac
   wlasne niestandardowe node'y i one moga miec pola ktore zaczynaja sie
   od duzych liter. }
  if Lexer.Token = vtName then
  begin
   ni := Fields.NameIndex(Lexer.TokenName);
   if ni >= 0 then
   begin
    ThisIsField := true;

    { Below: usually, it should be just "Lexer.NextToken;"
      But I have to add here some dirty hack to allow SFString fields
      to contain strings not enclosed in double quotes.
      So I have to call here NextTokenForceVTString before SFString field. }
    if Fields[ni] is TSFString then
     Lexer.NextTokenForceVTString else
     Lexer.NextToken;

    Fields[ni].Parse(Lexer);
   end else
   if TryParseSpecialField(Lexer) then
    ThisIsField := true;
  end;

  if not ThisIsField then
   AddChild(ParseNode(Lexer, NodeNameBinding, ParsingAllowedChildren));
 end;
 Lexer.NextToken;

 FWWWBasePath := Lexer.WWWBasePath;
end;

function TVRMLNode.TryParseSpecialField(Lexer: TVRMLLexer): boolean;
begin result := false end;

procedure TVRMLNode.EnumNodes(nodeClass: TVRMLNodeClass; proc: TVRMLNodeProc;
  enumOnlyInActiveNodes: boolean);
ITERATE_CHILDREN_DECLARE
begin
 if Self is nodeClass then proc(Self);

 if enumOnlyInActiveNodes then
  ITERATE_CHILDEN_INIT else
  begin FirstChild := 0; LastChild := ChildrenCount-1 end;

 ITERATE_CHILDREN_LOOP Children[i].EnumNodes(nodeClass, proc, enumOnlyInActiveNodes);
end;

procedure TVRMLNode.EnumNodes(nodeClass: TVRMLNodeClass; const SeekNodeName: string;
  proc: TVRMLNodeProc; enumOnlyInActiveNodes: boolean);
ITERATE_CHILDREN_DECLARE
begin
 if (Self is nodeClass) and (NodeName = SeekNodeName) then proc(Self);

 if enumOnlyInActiveNodes then
  ITERATE_CHILDEN_INIT else
  begin FirstChild := 0; LastChild := ChildrenCount-1 end;

 ITERATE_CHILDREN_LOOP Children[i].EnumNodes(nodeClass, SeekNodeName, proc, enumOnlyInActiveNodes);
end;

function TVRMLNode.TryFindNode(FindClass: TVRMLNodeClass; enumOnlyInActiveNodes: boolean): TVRMLNode;
ITERATE_CHILDREN_DECLARE
begin
 if Self is FindClass then
  result := Self else
 begin
  result := nil;

  if enumOnlyInActiveNodes then
   ITERATE_CHILDEN_INIT else
   begin FirstChild := 0; LastChild := ChildrenCount-1 end;

  ITERATE_CHILDREN_LOOP
  begin
   result := Children[i].TryFindNode(FindClass, enumOnlyInActiveNodes);
   if result <> nil then exit;
  end;
 end;
end;

function TVRMLNode.FindNode(FindClass: TVRMLNodeClass; enumOnlyInActiveNodes: boolean): TVRMLNode;
begin
 result := TryFindNode(FindClass, enumOnlyInActiveNodes);
 Check(result <> nil, 'Node class '+FindClass.ClassName+' not found (by TVRMLNode.FindNode)');
end;

{ TVRMLNode.TryFindNodeState/Transform ----------------------------------------- }

  type
    PVRMLNode = ^TVRMLNode;
    PVRMLGraphTraverseState = ^TVRMLGraphTraverseState;
    BreakTryFindNodeState = class(TCodeBreaker);
    TTryFindNodeStateObj = class
      PNode: PVRMLNode;
      PState: PVRMLGraphTraverseState;
      procedure TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
    end;

    procedure TTryFindNodeStateObj.TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
    begin
     PNode^:=ANode;
     PState^:=TVRMLGraphTraverseState.CreateCopy(AState);
     raise BreakTryFindNodeState.Create;
    end;

function TVRMLNode.TryFindNodeState(InitialState: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass;
  var Node: TVRMLNode; var State: TVRMLGraphTraverseState): boolean;
var Obj: TTryFindNodeStateObj;
begin
 Obj := TTryFindNodeStateObj.Create;
 try
  try
   Obj.PNode := @Node;
   Obj.PState := @State;
   Traverse(InitialState, NodeClass, Obj.TraverseFunc);
   result := false;
  except
   on BreakTryFindNodeState do result := true;
  end;
 finally Obj.Free end;
end;

  type
    TTryFindNodeTransformObj = class
      PNode: PVRMLNode;
      PTransform: PMatrix4Single;
      procedure TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
    end;

    procedure TTryFindNodeTransformObj.TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
    begin
     PNode^:=ANode;
     { to dlatego TryFindNodeTransform jest szybsze od TryFindNodeState :
       w TryFindNodeState trzeba tutaj kopiowac cale state,
       w TryFindNodeTransform wystarczy skopiowac transformacje. }
     PTransform^:=AState.CurrMatrix;
     raise BreakTryFindNodeState.Create;
    end;

function TVRMLNode.TryFindNodeTransform(InitialState: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass;
  var Node: TVRMLNode; var Transform: TMatrix4Single): boolean;
var Obj: TTryFindNodeTransformObj;
begin
 Obj := TTryFindNodeTransformObj.Create;
 try
  try
   Obj.PNode := @Node;
   Obj.PTransform := @Transform;
   Traverse(InitialState, NodeClass, Obj.TraverseFunc);
   result := false;
  except
   on BreakTryFindNodeState do result := true;
  end;
 finally Obj.Free end;
end;

{ TVRMLNode.[...]Find[...]NodeByName ------------------------------------------ }

function TVRMLNode.TryFindNodeByName(const FindName: string;
  enumOnlyInActiveNodes: boolean): TVRMLNode;
ITERATE_CHILDREN_DECLARE
begin
 if NodeName = FindName then
  result := Self else
 begin
  result := nil;

  if enumOnlyInActiveNodes then
   ITERATE_CHILDEN_INIT else
   begin FirstChild := 0; LastChild := ChildrenCount-1 end;

  ITERATE_CHILDREN_LOOP
  begin
   result := Children[i].TryFindNodeByName(FindName, enumOnlyInActiveNodes);
   if result <> nil then exit;
  end;
 end;
end;

function TVRMLNode.FindNodeByName(const FindName: string;
  enumOnlyInActiveNodes: boolean): TVRMLNode;
begin
 result := TryFindNodeByName(FindName, enumOnlyInActiveNodes);
 Check(result <> nil, 'Node name '+FindName+' not found (by TVRMLNode.FindNodeByName)');
end;

function TVRMLNode.TryFindParentNodeByName(const FindName: string): TVRMLNode;
var i: integer;
begin
 if NodeName = FindName then
  result := Self else
 begin
  result := nil;
  for i := 0 to ParentsCount-1 do
  begin
   result := Parents[i].TryFindParentNodeByName(FindName);
   if result <> nil then exit;
  end;
 end;
end;

function TVRMLNode.FindParentNodeByName(const FindName: string): TVRMLNode;
begin
 result := TryFindParentNodeByName(FindName);
 Check(result <> nil, 'Node name '+FindName+' not found in parents');
end;

function TVRMLNode.HasParent(Node: TVRMLNode): boolean;
var i: integer;
begin
 if Self = Node then
  result := true else
 begin
  for i := 0 to ParentsCount-1 do
   if Parents[i].HasParent(Node) then Exit(true);
  result := False;
 end;
end;

function TVRMLNode.IsNodePresent(Node: TVRMLNode; seekOnlyInActiveNodes: boolean): boolean;
ITERATE_CHILDREN_DECLARE
begin
 if Self = Node then exit(true);

 if seekOnlyInActiveNodes then
  ITERATE_CHILDEN_INIT else
  begin FirstChild := 0; LastChild := ChildrenCount-1 end;

 ITERATE_CHILDREN_LOOP
  if Children[i].IsNodePresent(Node, seekOnlyInActiveNodes) then exit(true);

 result := false;
end;

type
  TNodeCounter = class
    procedure CountNode(node: TVRMLNode);
    Counter: integer;
  end;
  procedure TNodeCounter.CountNode(node: TVRMLNode);
  begin Inc(Counter) end;

function TVRMLNode.CountNodes(NodeClass: TVRMLNodeClass; countOnlyActiveNodes: boolean): integer;
var C: TNodeCounter;
begin
 C := TNodeCounter.Create;
 try
  EnumNodes(NodeClass, C.CountNode, countOnlyActiveNodes);
  result := C.Counter;
 finally C.Free end;
end;

procedure TVRMLNode.SaveToStream(Stream: TStream; const Indent: string; NodeNameBinding: TStringList);
var i: integer;
    NewIndent: string;
begin
 if NodeNameBinding.IndexOfObject(Self) >= 0 then
  WriteStr(Stream, Indent +'USE ' +NodeName +nl) else
 begin
  {zapisz nas do strumienia}
  WriteStr(Stream, Indent);
  if NodeName <> '' then WriteStr(Stream, 'DEF ' +NodeName +' ');
  WriteStr(Stream, NodeTypeName +' {' +nl);

  NewIndent := Indent +IndentIncrement;

  for i := 0 to Fields.Count-1 do
   Fields[i].SaveToStream(Stream, NewIndent);
  if not (Self is TNodeWWWInline) then
   for i := 0 to ChildrenCount-1 do
    Children[i].SaveToStream(Stream, NewIndent, NodeNameBinding);

  WriteStr(Stream, Indent +'}' +nl);

  {teraz uaktualnij NodeNameBinding}
  if NodeName <> '' then
  begin
   i := NodeNameBinding.IndexOf(NodeName);
   if i >= 0 then
    NodeNameBinding.Objects[i] := Self else
    NodeNameBinding.Addobject(NodeName, Self);
  end;
 end;
end;

class function TVRMLNode.TraverseStateLastNodesIndex: Integer;
{ zwraca indeks do tablicy TraverseStateLastNodesClasses taki ze element
  na tej pozycji = NodeClass. Zwraca -1 jesli nie znalazl. }
begin
 { jestesmy w metodzie klasy, wiec Self to klasa (dlatego ponizej mozemy
   porownywac TraverseStateLastNodesClasses[result] z Self) }
 for result := 0 to HighTraverseStateLastNodes do
  if TraverseStateLastNodesClasses[result] = Self then Exit;
 result := -1;
end;

{ specific VRML nodes ---------------------------------------------------- }

{$I VRMLNodes_BoundingBoxes.inc}
{$I VRMLNodes_VerticesAndTrianglesCounting.inc}
{$I VRMLNodes_Triangulating.inc}

constructor TNodeAsciiText.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..0]of string = ('');
      A2: array[0..2]of string = ('LEFT', 'CENTER', 'RIGHT');
      A3: array[0..0]of Single = (0);
begin
 inherited;
 Fields.Add(TMFString.Create('string', A1));
 Fields.Add(TSFFloat.Create('spacing', 1));
 Fields.Add(TSFEnum.Create('justification', A2, JUSTIFICATION_LEFT));
 Fields.Add(TMFFloat.Create('width', A3));
end;

class function TNodeAsciiText.ClassNodeTypeName: string;
begin
 result := 'AsciiText';
end;

constructor TNodeCone.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..1]of string = ('SIDES', 'BOTTOM');
begin
 inherited;
 Fields.Add(TSFBitMask.Create('parts', A1, 'NONE', 'ALL', [true, true]));
 Fields.Add(TSFFloat.Create('bottomRadius', 1, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
end;

class function TNodeCone.ClassNodeTypeName: string;
begin
 result := 'Cone';
end;

constructor TNodeCube.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFFloat.Create('width', 2, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
 Fields.Add(TSFFloat.Create('depth', 2, true));
end;

class function TNodeCube.ClassNodeTypeName: string;
begin
 result := 'Cube';
end;

constructor TNodeCylinder.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..2]of string = ('SIDES', 'TOP', 'BOTTOM');
begin
 inherited;
 Fields.Add(TSFBitMask.Create('parts', A1, 'NONE', 'ALL', [true, true, true]));
 Fields.Add(TSFFloat.Create('radius', 1, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
end;

class function TNodeCylinder.ClassNodeTypeName: string;
begin
 result := 'Cylinder';
end;

constructor TNodeGeneralIndexed.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..0]of Longint = (0);
      A2: array[0..0]of Longint = (-1);
begin
 inherited;
 Fields.Add(TMFLong.CreateMFLong('coordIndex', A1, true));
 Fields.Add(TMFLong.Create('materialIndex', A2));
 Fields.Add(TMFLong.Create('normalIndex', A2));
 Fields.Add(TMFLong.CreateMFLong('textureCoordIndex', A2, true));
end;

class function TNodeIndexedFaceSet.ClassNodeTypeName: string;
begin
 result := 'IndexedFaceSet';
end;

class function TNodeIndexedTriangleMesh.ClassNodeTypeName: string;
begin
 result := 'IndexedTriangleMesh';
end;

class function TNodeIndexedLineSet.ClassNodeTypeName: string;
begin
 result := 'IndexedLineSet';
end;

constructor TNodePointSet.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('startIndex', 0));
 Fields.Add(TSFLong.Create('numPoints', -1));
end;

class function TNodePointSet.ClassNodeTypeName: string;
begin
 result := 'PointSet';
end;

procedure TNodePointSet.CalculateRange(LastCoordinate3: TNodeCoordinate3;
  var startIndex, numPoints: integer);
begin
 startIndex := FdStartIndex.Value;
 numPoints := FdNumPoints.Value;
 if startIndex >= LastCoordinate3.FdPoint.Count then
 begin
  startIndex := 0;
  numPoints := 0;
 end else
 begin
  if startIndex < 0 then
  begin
   if numPoints >= 0 then numPoints := numPoints+startIndex;
   startIndex := 0;
  end;

  {startIndex juz jest na pewno dobry, teraz ew. popraw numPoints}
  if numPoints >= 0 then
  begin
   if startIndex+numPoints > LastCoordinate3.FdPoint.Count then
    numPoints := LastCoordinate3.FdPoint.Count-startIndex;
  end else
   numPoints := LastCoordinate3.FdPoint.Count-startIndex;
 end;
end;

constructor TNodeSphere.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFFloat.Create('radius', 1, true));
end;

class function TNodeSphere.ClassNodeTypeName: string;
begin
 result := 'Sphere';
end;

constructor TNodeCoordinate3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFVec3f.Create('point', [Vector3Single(0, 0, 0)]));
end;

class function TNodeCoordinate3.ClassNodeTypeName: string;
begin
 result := 'Coordinate3';
end;

constructor TNodeFontStyle.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..2]of string = ('SERIF', 'SANS', 'TYPEWRITER');
      A2: array[0..1]of string = ('BOLD', 'ITALIC');
begin
 inherited;
 Fields.Add(TSFFloat.Create('size', 10, true));
 Fields.Add(TSFEnum.Create('family', A1, FSFAMILY_SERIF));
 Fields.Add(TSFBitMask.Create('style', A2, 'NONE', '', [false, false]));
end;

class function TNodeFontStyle.ClassNodeTypeName: string;
begin
 result := 'FontStyle';
end;

function TNodeFontStyle.TTF_Font: PTrueTypeFont;
const
  Results: array[TVRMLFontFamily, boolean, boolean]of PTrueTypeFont =
  (              {  {[],                          [italic],                            [bold],                      [italic, bold] }
    {serif}      ( ((@TTF_BitstreamVeraSerif),   (@TTF_BitstreamVeraSerif_Italic)),    ((@TTF_BitstreamVeraSerif_Bold),    (@TTF_BitstreamVeraSerif_Bold_Italic)) ),
    {sans}       ( ((@TTF_BitstreamVeraSans),    (@TTF_BitstreamVeraSans_Italic)),     ((@TTF_BitstreamVeraSans_Bold),     (@TTF_BitstreamVeraSans_Bold_Italic)) ),
    {typewriter} ( ((@TTF_BitstreamVeraSansMono),(@TTF_BitstreamVeraSansMono_Italic)), ((@TTF_BitstreamVeraSansMono_Bold), (@TTF_BitstreamVeraSansMono_Bold_Italic)) )
  );
begin
 result := Results[FdFamily.Value, FdStyle.Flags[FSSTYLE_BOLD],
   FdStyle.Flags[FSSTYLE_ITALIC]];
end;

constructor TNodeInfo.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFString.Create('string', '<Undefined info>'));
end;

class function TNodeInfo.ClassNodeTypeName: string;
begin
 result := 'Info';
end;

constructor TNodeLOD.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFFloat.Create('range',[]));
 Fields.Add(TSFVec3f.Create('center', Vector3Single(0, 0, 0)));
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

class function TNodeLOD.ClassNodeTypeName: string;
begin
 result := 'LOD';
end;

procedure TNodeLOD.ChildrenToEnter(var FirstChild, LastChild: integer);
begin
 {sorry - powinnismy tu uzywac odleglosci od viewera ? Problem.
   dla renderowania jest problem z wrzucaniem tego na display liste.
   dla boundingBoxa
     Wybrac ostatnie SubNode bo bedzie je nalatwiej obliczac ?
     Pierwsze, bo jest dokladne ? To ktore renderujemy ?
     W ostatnim przypadku, ladujemy z tym samym klopotem co RenderNKSpecific :
     zapamietywanie takiego BoundingBoxa nie jest poprawne.
 }
 if ChildrenCount = 0 then
  raise EVRMLError.Create('LOD node must have at least one child');
 FirstChild := 0;
 LastChild := 0;
end;

const
  DEF_MAT_AMBIENT : TVector3Single = (0.2, 0.2, 0.2);
  DEF_MAT_DIFFUSE : TVector3Single = (0.8, 0.8, 0.8);
  DEF_MAT_SPECULAR : TVector3Single = (0, 0, 0);
  DEF_MAT_EMISSIVE : TVector3Single = (0, 0, 0);
  DEF_MAT_SHININESS : Single = 0.2;
  DEF_MAT_TRANSPARENCY : Single = 0;
  DEF_MAT_MIRROR : Single = 0;
  DEF_MAT_REFL_SPECULAR_EXP  : Single = 1000000;
  DEF_MAT_TRANS_SPECULAR_EXP : Single = 1000000;

constructor TNodeMaterial.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFColor.Create('ambientColor', [DEF_MAT_AMBIENT]));
 Fields.Add(TMFColor.Create('diffuseColor', [DEF_MAT_DIFFUSE]));
 Fields.Add(TMFColor.Create('specularColor', [DEF_MAT_SPECULAR]));
 Fields.Add(TMFColor.Create('emissiveColor', [DEF_MAT_EMISSIVE]));
 Fields.Add(TMFFloat.Create('shininess', [DEF_MAT_SHININESS]));
 Fields.Add(TMFFloat.Create('transparency', [DEF_MAT_TRANSPARENCY]));

 Fields.Add(TMFFloat.Create('mirror', [DEF_MAT_MIRROR]));
 Fields.Add(TMFColor.Create('reflSpecular', []));
 Fields.Add(TMFColor.Create('reflDiffuse', []));
 Fields.Add(TMFColor.Create('transSpecular', []));
 Fields.Add(TMFColor.Create('transDiffuse', []));
 Fields.Add(TMFFloat.Create('reflSpecularExp', [DEF_MAT_REFL_SPECULAR_EXP]));
 Fields.Add(TMFFloat.Create('transSpecularExp', [DEF_MAT_TRANS_SPECULAR_EXP]));
end;

class function TNodeMaterial.ClassNodeTypeName: string;
begin
 result := 'Material';
end;

{functions below return MatNum Material property. If there doesn't exist
   enouch properties defined, thay return the last defined. This is useful :
   for example you can give one ambient, specular, emissive color and
   define multiple diffuseColors and then you can use multiple materials
   without defining multiple values for each field.
 VRML specification doesn't state clearly what to do when thare are not enouch
   material properties - there were some idea of "cycling" mechanism but was
   later deprecated. So I use the mechanism below - returning to the last defined
   property number. }

{$define MATERIAL_FUNCTION_3_SINGLE:=
function TNodeMaterial.MATERIAL_FUNCTION_NAME_3(MatNum: integer): TVector3Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_DEFAULT else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[
    min(MatNum, MATERIAL_FUNCTION_FIELD.Count-1)];
end;

function TNodeMaterial.MATERIAL_FUNCTION_NAME_4(MatNum: integer): TVector4Single;
var result3: TVector3Single absolute result;
begin
 result3 := MATERIAL_FUNCTION_NAME_3(MatNum);
 result[3] := Opacity(MatNum);
end;
}

  {$define MATERIAL_FUNCTION_FIELD := FdAmbientColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_AMBIENT}
  {$define MATERIAL_FUNCTION_NAME_3 := AmbientColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := AmbientColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdDiffuseColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_DIFFUSE}
  {$define MATERIAL_FUNCTION_NAME_3 := DiffuseColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := DiffuseColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdSpecularColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_SPECULAR}
  {$define MATERIAL_FUNCTION_NAME_3 := SpecularColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := SpecularColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdEmissiveColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_EMISSIVE}
  {$define MATERIAL_FUNCTION_NAME_3 := EmissiveColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := EmissiveColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

{$undef MATERIAL_FUNCTION_3_SINGLE}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_DEFAULT}
{$undef MATERIAL_FUNCTION_NAME_3}
{$undef MATERIAL_FUNCTION_NAME_4}

{$define MATERIAL_FUNCTION_SINGLE:=
function TNodeMaterial.MATERIAL_FUNCTION_NAME(MatNum: integer): Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_DEFAULT else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[
    min(MatNum, MATERIAL_FUNCTION_FIELD.Count-1)];
end;}

  {$define MATERIAL_FUNCTION_NAME := Transparency}
  {$define MATERIAL_FUNCTION_FIELD := FdTransparency}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_TRANSPARENCY}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := Mirror}
  {$define MATERIAL_FUNCTION_FIELD := FdMirror}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_MIRROR}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := ReflSpecularExp}
  {$define MATERIAL_FUNCTION_FIELD := FdReflSpecularExp}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_REFL_SPECULAR_EXP}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := TransSpecularExp}
  {$define MATERIAL_FUNCTION_FIELD := FdTransSpecularExp}
  {$define MATERIAL_FUNCTION_DEFAULT := DEF_MAT_TRANS_SPECULAR_EXP}
  MATERIAL_FUNCTION_SINGLE

{$undef MATERIAL_FUNCTION_NAME}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_DEFAULT}
{$undef MATERIAL_FUNCTION_SINGLE}

function TNodeMaterial.Opacity(MatNum: integer): Single;
begin
 result := 1-Transparency(MatNum);
end;

function TNodeMaterial.ShininessExp(MatNum: integer): Single;
begin
 if FdShininess.Count = 0 then
  result := DEF_MAT_SHININESS else
  result := FdShininess.Items.Items[min(MatNum, FdShininess.Count-1)];

 {zgodnie ze specyfikacja VRML'a zakres 0..1 shininess VRML'a mapuje sie
  jednostajnie na caly zakres 0..128 OpenGL'a. Super. Robimy clamp
  na wszelki zas, na wypadek gdybysmy na skutek jakichs bledow nieco wyszli
  za zakres albo gdyby ktos zapisale zla wartosc w pliku (np. ujemna).

  A jezeli odczytana wartosc jest > 2 to zakladamy ze jakis
  kretyn zapisal nieznormalizowane wartosci Shininess w pliku
  (tacy sie zdarzaja, patrz helix.wrl).
  Pisze kretyn a mysle duzo gorzej bo jestem naprawde wkurzony - coraz
  wiecej musze w kodzie wprowadzac poprawek zeby odczytywac niepoprawnie
  zapisane VRMLe. A to rotacja wokol wektora (0, 0, 0) (program "Pioneer")
  a to nieznormalizowany Shininess... }
 if result > 2 then
  result := Clamped(result,         0.0, 128.0) else
  result := Clamped(result * 128.0, 0.0, 128.0);
end;

function TNodeMaterial.OnlyEmissiveMaterial: boolean;
begin
 result:=(FdAmbientColor.Count = 0) and
         (FdDiffuseColor.Count = 0) and
         (FdSpecularColor.Count = 0);
end;

{ cztery funkcje ktore w razie braku wartosci zapisanych w polu (FdXxx.Count = 0)
  wyliczaja sobie kolor z innych wlasciwosci materialu. }
{$define MATERIAL_FUNCTION_CALC:=
function TNodeMaterial.MATERIAL_FUNCTION_NAME(MatNum: integer): TVector3Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_CALCULATE else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[min(MatNum,
    MATERIAL_FUNCTION_FIELD.Count-1)]
end;}

  {$define MATERIAL_FUNCTION_NAME := ReflSpecular}
  {$define MATERIAL_FUNCTION_FIELD := FdReflSpecular}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    Vector3Single(Mirror(MatNum), Mirror(MatNum), Mirror(MatNum))}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := ReflDiffuse}
  {$define MATERIAL_FUNCTION_FIELD := FdReflDiffuse}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    DiffuseColor3Single(MatNum)}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := TransSpecular}
  {$define MATERIAL_FUNCTION_FIELD := FdTransSpecular}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    Vector3Single(Transparency(MatNum), Transparency(MatNum), Transparency(MatNum))}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := TransDiffuse}
  {$define MATERIAL_FUNCTION_FIELD := FdTransDiffuse}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    VectorScale(DiffuseColor3Single(MatNum), Transparency(MatNum) )}
  MATERIAL_FUNCTION_CALC

{$undef MATERIAL_FUNCTION_CALC}
{$undef MATERIAL_FUNCTION_NAME}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_CALCULATE}

function TNodeMaterial.IsAllMaterialsTransparent: boolean;
var i: Integer;
begin
 if FdTransparency.Items.Length = 0 then
  result := DEF_MAT_TRANSPARENCY > SingleEqualityEpsilon else
 begin
  for i := 0 to FdTransparency.Items.Length-1 do
   if FdTransparency.Items.Items[i] <= SingleEqualityEpsilon then Exit(false);
  result := true;
 end;
end;

constructor TNodeMaterialBinding.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..7]of string = ('DEFAULT', 'OVERALL',
  'PER_PART', 'PER_PART_INDEXED',
  'PER_FACE', 'PER_FACE_INDEXED',
  'PER_VERTEX', 'PER_VERTEX_INDEXED');
begin
 inherited;
 Fields.Add(TSFEnum.Create('value', A1, 1));
end;

class function TNodeMaterialBinding.ClassNodeTypeName: string;
begin
 result := 'MaterialBinding';
end;

constructor TNodeNormal.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFVec3f.Create('vector', []));
end;

class function TNodeNormal.ClassNodeTypeName: string;
begin
 result := 'Normal';
end;

constructor TNodeNormalBinding.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..7]of string = ('DEFAULT', 'OVERALL',
  'PER_PART', 'PER_PART_INDEXED',
  'PER_FACE', 'PER_FACE_INDEXED',
  'PER_VERTEX', 'PER_VERTEX_INDEXED');
begin
 inherited;
 Fields.Add(TSFEnum.Create('value', A1, 0));
end;

class function TNodeNormalBinding.ClassNodeTypeName: string;
begin
 result := 'NormalBinding';
end;

constructor TNodeTexture2.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..1]of string = ('REPEAT', 'CLAMP');
begin
 inherited;
 Fields.Add(TSFString.Create('filename', ''));
 Fields.Add(TSFImage.Create('image', nil));
 Fields.Add(TSFEnum.Create('wrapS', A1, TEXWRAP_REPEAT));
 Fields.Add(TSFEnum.Create('wrapT', A1, TEXWRAP_REPEAT));
 Fields.Add(TSFEnum.Create('model', ['DECAL'], 0));
 Fields.Add(TSFVec3f.Create('blendColor', Vector3Single(0, 0, 0)));

 FTextureImage := TRGBImage.Create;
 FIsTextureLoaded := false;
end;

class function TNodeTexture2.ClassNodeTypeName: string;
begin
 result := 'Texture2';
end;

destructor TNodeTexture2.Destroy;
begin
 FreeAndNil(FTextureImage);
 inherited;
end;

function TNodeTexture2.TextureImage: TImage;
begin
 if not IsTextureLoaded then ReloadTexture;
 Assert(IsTextureLoaded);
 result := FTextureImage;
end;

function TNodeTexture2.IsTextureImage: boolean;
begin
 result := not TextureImage.IsNull;
end;

procedure TNodeTexture2.ReloadTexture;

  procedure ReplaceTextureImage(NewFTextureImage: TImage);
  begin
   FreeAndNil(FTextureImage);
   FTextureImage := NewFTextureImage;
  end;

begin
 { Just like in implementation of TSFImage.Parse:

   Note that we should never let FTextureImage to be nil too long,
   because even if this method exits with exception, FTextureImage should
   always remain non-nil.
   That's why I'm doing below FTextureImage.Null instead of
   FreeAndNil(FTextureImage) and I'm using ReplaceTextureImage to set
   new FTextureImage.
   This way if e.g. TRGBImage.Create with out of mem exception,
   FTextureImage will still remain non-nil.

   This is all because I just changed Images unit interface to class-like
   and I want to do minimal changes to VRMLNodes unit to not break
   anything. TODO -- this will be solved better in the future, by simply
   allowing TextureImage to be nil at any time.
 }

 FTextureImage.Null;

 { sprobuj zaladowac teksture z pliku FdFilename }
 if FdFilename.Value <> '' then
 try
  ReplaceTextureImage( LoadImage( CombinePaths(WWWBasePath, FdFilename.Value),
    [TRGBImage, TAlphaImage], []) );
 except
  on E: Exception do
   { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
     (chociaz nie musi) }
   VRMLNonFatalError('Exception '+E.ClassName+' occured when trying to load '+
     'texture from filename '+FdFilename.Value+' : '+E.Message);
 end;

 { FTextureImage.IsNull oznacza ze nie bylo filename albo tekstury z
   filename nie dalo sie zaladowac. Wiec jezeli jest to uzywamy inlined
   tekstury (w polu FdImage) }
 if (FTextureImage.IsNull) and (not FdImage.Value.IsNull) then
  ReplaceTextureImage(FdImage.Value.MakeCopy);

 FIsTextureLoaded := true;
end;

function TNodeTexture2.TextureDescription: string;

  function InlinedDescr: string;
  begin
   result := Format('inlined (width = %d; height = %d; with alpha = %s)',
     [ FdImage.Value.Width, FdImage.Value.Height,
       BoolToStr[FdImage.Value is TAlphaImage] ]);
  end;

begin
 if FdFilename.Value <> '' then
 begin
  result := 'file "' +CombinePaths(WWWBasePath, FdFilename.Value) +'"';
  if not FdImage.Value.IsNull then result += ' (and '+InlinedDescr+')';
 end else
 if not FdImage.Value.IsNull then
  result := InlinedDescr else
  result := 'none';
end;

constructor TNodeTexture2Transform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec2f.Create('translation', Vector2Single(0, 0)));
 Fields.Add(TSFFloat.Create('rotation', 0));
 Fields.Add(TSFVec2f.Create('scaleFactor', Vector2Single(1, 1)));
 Fields.Add(TSFVec2f.Create('center', Vector2Single(0, 0)));
end;

class function TNodeTexture2Transform.ClassNodeTypeName: string;
begin
 result := 'Texture2Transform';
end;

function TNodeTexture2Transform.TextureMatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix( Vector3Single(
   VectorAdd(FdTranslation.Value, FdCenter.Value) ));
 result := MultMatrices(result, RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
 result := MultMatrices(result, ScalingMatrix(
   Vector3Single( FdScaleFactor.Value[0], FdScaleFactor.Value[1], 1 )));
 result := MultMatrices(result, TranslationMatrix(
   Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

procedure TNodeTexture2Transform.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
 inherited;
 State.CurrTextureMatrix := MultMatrices(State.CurrTextureMatrix,
   TextureMatrixTransformation);
end;

constructor TNodeTextureCoordinate2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFVec2f.Create('point', [Vector2Single(0, 0)]));
end;

class function TNodeTextureCoordinate2.ClassNodeTypeName: string;
begin
 result := 'TextureCoordinate2';
end;

constructor TNodeShapeHints.Create(const ANodeName: string;
  const AWWWBasePath: string);
const
  A1: array[0..2]of string = ('UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE');
  A2: array[0..1]of string = ('UNKNOWN_SHAPE_TYPE', 'SOLID');
  A3: array[0..1]of string = ('UNKNOWN_FACE_TYPE', 'CONVEX');
begin
 inherited;
 Fields.Add(TSFEnum.Create('vertexOrdering', A1, VERTORDER_UNKNOWN));
 Fields.Add(TSFEnum.Create('shapeType', A2, SHTYPE_UNKNOWN));
 Fields.Add(TSFEnum.Create('faceType', A3, FACETYPE_CONVEX));
 Fields.Add(TSFFloat.Create('creaseAngle', 0.5));
end;

class function TNodeShapeHints.ClassNodeTypeName: string;
begin
 result := 'ShapeHints';
end;

function TNodeShapeHints.TryParseSpecialField(Lexer: TVRMLLexer): boolean;
const A1: array[0..2]of string=('SOLID', 'ORDERED', 'CONVEX');
var Hints: TSFBitMask;
begin
 if (Lexer.VRMLVerMajor = 0) and (Lexer.TokenName = 'hints') then
 begin
  Hints := TSFBitMask.Create('hints', A1, 'NONE', '',  [false, true, true]);
  try
   Lexer.NextToken;
   Hints.Parse(Lexer);
   if Hints.Flags[0] then
    FdShapeType.Value := SHTYPE_SOLID else
    FdShapeType.Value := SHTYPE_UNKNOWN;
   if Hints.Flags[1] then
    FdVertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE else
    FdVertexOrdering.Value := VERTORDER_UNKNOWN;
   if Hints.Flags[2] then
    FdFaceType.Value := FACETYPE_CONVEX else
    FdFaceType.Value := FACETYPE_UNKNOWN;
  finally Hints.Free end;
  result := true;
 end else
  result := false;
end;

procedure TNodeGeneralTransformation.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
 inherited;
 State.CurrMatrix := MultMatrices(State.CurrMatrix, MatrixTransformation);
end;

constructor TNodeMatrixTransform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFMatrix.Create('matrix', IdentityMatrix4Single));
end;

class function TNodeMatrixTransform.ClassNodeTypeName: string;
begin
 result := 'MatrixTransform';
end;

function TNodeMatrixTransform.MatrixTransformation: TMatrix4Single;
begin
 result := FdMatrix.Matrix;
end;

constructor TNodeRotation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0));
end;

class function TNodeRotation.ClassNodeTypeName: string;
begin
 result := 'Rotation';
end;

function TNodeRotation.MatrixTransformation: TMatrix4Single;
begin
 {glRotate OpenGL'a ma obroty skierowane w ta sama strone co
    w/g specyfikacji VRML'a, wiec wszystko OK.
  Musimy sie tu zabezpieczyc przed glupim wektorem FdRotation -
    program o nazwie "Pioneer" potrafi takie cos zapisywac do
    pliku VRMla. }
 if IsZeroVector(FdRotation.Axis) then
  result := IdentityMatrix4Single else
  result := RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis);
end;

constructor TNodeRotationXYZ.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFEnum.Create('axis' , ['X', 'Y', 'Z'], 0));
 Fields.Add(TSFFloat.Create('angle', 0));
end;

class function TNodeRotationXYZ.ClassNodeTypeName: string;
begin
 Result := 'RotationXYZ';
end;

function TNodeRotationXYZ.MatrixTransformation: TMatrix4Single;
const
  AxisVectors: array[0..2]of TVector3Single =
  ( (1, 0, 0), (0, 1, 0), (0, 0, 1) );
begin
 Result := RotationMatrixRad(FdAngle.Value, AxisVectors[FdAxis.Value]);
end;

constructor TNodeScale.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('scaleFactor', Vector3Single(1, 1, 1)));
end;

class function TNodeScale.ClassNodeTypeName: string;
begin
 result := 'Scale';
end;

function TNodeScale.MatrixTransformation: TMatrix4Single;
begin
 result := ScalingMatrix(FdScaleFactor.Value);
end;

constructor TNodeTransform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('translation', Vector3Single(0, 0, 0)));
 Fields.Add(TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0));
 Fields.Add(TSFVec3f.Create('scaleFactor', Vector3Single(1, 1, 1)));
 Fields.Add(TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0));
 Fields.Add(TSFVec3f.Create('center', Vector3Single(0, 0, 0)));
end;

class function TNodeTransform.ClassNodeTypeName: string;
begin
 result := 'Transform';
end;

function TNodeTransform.MatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix(FdTranslation.Value);
 result := MultMatrices(result, TranslationMatrix(FdCenter.Value));
 if not IsZeroVector(FdRotation.Axis) then
  result := MultMatrices(result,
    RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis));
 result := MultMatrices(result,
   RotationMatrixRad(FdScaleOrientation.RotationRad, FdScaleOrientation.Axis));
 result := MultMatrices(result, ScalingMatrix(FdScaleFactor.Value));
 result := MultMatrices(result,
   RotationMatrixRad(-FdScaleOrientation.RotationRad, FdScaleOrientation.Axis));
 result := MultMatrices(result, TranslationMatrix(VectorNegate(FdCenter.Value)));
end;

constructor TNodeTranslation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('translation', Vector3Single(0, 0, 0)));
end;

class function TNodeTranslation.ClassNodeTypeName: string;
begin
 result := 'Translation';
end;

function TNodeTranslation.MatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix(FdTranslation.Value);
end;

constructor TNodeGeneralCamera.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('position', Vector3Single(0, 0, 1)));
 Fields.Add(TSFRotation.Create('orientation', Vector3Single(0, 0, 1), 0));
 Fields.Add(TSFFloat.Create('focalDistance', 5, true));
 Fields.Add(TSFFloat.Create('heightAngle', 0.785398, true));
 Fields.Add(TMFVec3f.Create('direction', []));
 Fields.Add(TMFVec3f.Create('up', []));
 Fields.Add(TSFFloat.Create('nearDistance', 0));
 Fields.Add(TSFFloat.Create('farDistance', 0));
end;

procedure TNodeGeneralCamera.CalcCamera(const CamTransform: TMatrix4Single;
  var CamPos, CamDir, CamUp: TVector3Single);
begin
 CamPos := FdPosition.Value;
 if FdDirection.Items.Length > 0 then
  CamDir := FdDirection.Items.Items[0] else
  CamDir := FdOrientation.RotatedPoint( StdVRMLCamDir );
 if FdUp.Items.Length > 0 then
  CamUp := FdUp.Items.Items[0] else
  CamUp := FdOrientation.RotatedPoint( StdVRMLCamUp );

 { niestety, macierz ponizej moze cos skalowac wiec nawet jesli powyzej
   uzylismy FdOrientation.RotatedPoint( StdVRMLCamDir/Up ) i wiemy ze CamDir/Up
   jest znormalizowane - to i tak musimy je tutaj znormalizowac.
   sorry- byloby dobrze uzyc tutaj czegos jak MultMatrixPointNoTranslationNoScale }
 CamPos := MultMatrixPoint(CamTransform, CamPos);
 CamDir := Normalized( MultMatrixPointNoTranslation(CamTransform, CamDir) );
 CamUp := Normalized( MultMatrixPointNoTranslation(CamTransform, CamUp) );

 Assert(FloatsEqual(VectorLenSqr(CamDir), 1.0, 0.0001));
 Assert(FloatsEqual(VectorLenSqr(CamUp), 1.0, 0.0001));
end;

class function TNodeOrthographicCamera.ClassNodeTypeName: string;
begin
 result := 'OrthographicCamera';
end;

class function TNodeOrthographicCamera.CameraKind: TVRMLCameraKind;
begin
 result := ckOrthographic;
end;

class function TNodePerspectiveCamera.ClassNodeTypeName: string;
begin
 result := 'PerspectiveCamera';
end;

class function TNodePerspectiveCamera.CameraKind: TVRMLCameraKind;
begin
 result := ckPerspective;
end;

constructor TNodeGeneralLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFBool.Create('on', true));
 Fields.Add(TSFFloat.Create('intensity', 1));
 Fields.Add(TSFColor.Create('color', Vector3Single(1, 1, 1)));
end;

procedure TNodeGeneralLight.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
 inherited;
 State.ActiveLights.AddLight(Self, State.CurrMatrix);
end;

constructor TNodeGeneralPositionalLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('location', Vector3Single(0, 0, 1)));
 Fields.Add(TSFVec3f.Create('attenuation', Vector3Single(1, 0, 0)));
end;

function TNodeGeneralPositionalLight.DistanceNeededForAttenuation: boolean;
begin
 result:=(FdAttenuation.Value[1] > 0) or (FdAttenuation.Value[2] > 0);
end;

{$define ATTENUATION_IMPLEMENTATION:=
begin
 (* moglibysmy tu nie badac czy DistanceNeededForAttenuation i zawsze
    robic wersje pelna (bo przeciez
      FdAttenuation.Value[1] * DistanceToLight +
      FdAttenuation.Value[2] * Sqr(DistanceToLight)
    i tak bedzie = 0 gdy FdAttenuation.Value[1] = FdAttenuation.Value[2] = 0.
    Ale wydaje mi sie ze tak jest szybciej - testowanie kosztuje nas
    troszke czasu ale mozemy sobie w ten sposob ocalic 2 x mnozenie i dodawanie. *)

 (* we check whether attenuation = (0, 0, 0). VRML 97 spec says that specifying
    (0, 0, 0) should be equal to specifying (1, 0, 0). (well, we avoid
    division by zero possibility this way so it's quite sensible, even
    if it wastes some time) *)
 if (FdAttenuation.Value[0] = 0) and
    (FdAttenuation.Value[1] = 0) and
    (FdAttenuation.Value[2] = 0) then result := 1;

 if DistanceNeededForAttenuation then
  result := 1/ KambiUtils.max(FdAttenuation.Value[0] +
                   FdAttenuation.Value[1] * DistanceToLight +
                   FdAttenuation.Value[2] * Sqr(DistanceToLight), Single(1.0)) else
  result := 1/ KambiUtils.max(FdAttenuation.Value[0], Single(1.0));
end;}

function TNodeGeneralPositionalLight.Attenuation(const DistanceToLight: Single): Single;
ATTENUATION_IMPLEMENTATION

function TNodeGeneralPositionalLight.Attenuation(const DistanceToLight: Double): Double;
ATTENUATION_IMPLEMENTATION

constructor TNodeDirectionalLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, -1)));
end;

class function TNodeDirectionalLight.ClassNodeTypeName: string;
begin
 result := 'DirectionalLight';
end;

constructor TNodePointLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 { no new fields - this is just TNodeGeneralPositionalLight}
end;

class function TNodePointLight.ClassNodeTypeName: string;
begin
 result := 'PointLight';
end;

constructor TNodeSpotLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, -1)));
 Fields.Add(TSFFloat.Create('dropOffRate', 0));
 Fields.Add(TSFFloat.Create('cutOffAngle', 0.785398));
end;

class function TNodeSpotLight.ClassNodeTypeName: string;
begin
 result := 'SpotLight';
end;

function TNodeSpotLight.SpotExp: Single;
begin
 result := FdDropOffRate.Value*128.0;
end;

constructor TNodeGroup.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

class function TNodeGroup.ClassNodeTypeName: string;
begin
 result := 'Group';
end;

constructor TNodeGeneralSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

procedure TNodeGeneralSeparator.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
 inherited;
 OriginalState := State;
 State := TVRMLGraphTraverseState.CreateCopy(OriginalState);
end;

procedure TNodeGeneralSeparator.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
 State.Free;
 State := OriginalState;
 inherited;
end;

constructor TNodeSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFEnum.Create('renderCulling', ['ON', 'OFF', 'AUTO'], 2));
end;

class function TNodeSeparator.ClassNodeTypeName: string;
begin
 result := 'Separator';
end;

constructor TNodeSwitch.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('whichChild', -1));
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

class function TNodeSwitch.ClassNodeTypeName: string;
begin
 result := 'Switch';
end;

procedure TNodeSwitch.ChildrenToEnter(var FirstChild, LastChild: integer);
begin
 if FdWhichChild.Value = -3 then
 begin
  FirstChild := 0;
  LastChild := ChildrenCount-1;
 end else
 begin
  { jezeli whichChild jest nieprawidlowe to w rezultacie nie wejdziemy w
    zadne Child. Wpp. wejdziemy w jedno wyznaczone child. I o to chodzi.
    (note : value -1 is no special value; any value that doesn't specify
     valid child number and is not -3 instructs Switch to not enter
     into any child. This is conformant with VRML 97 specification) }
  FirstChild := FdWhichChild.Value;
  LastChild := FdWhichChild.Value;
 end;

 { note : value -3 is already deprecated in VRML 1.0;
   but I support it, at least for now }
end;

constructor TNodeTransformSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

class function TNodeTransformSeparator.ClassNodeTypeName: string;
begin
 result := 'TransformSeparator';
end;

procedure TNodeTransformSeparator.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
 inherited;
 {nie robimy kopii calego State'a bo w TVRMLRenderState moga byc
  jeszcze inne informacje ktore powinny "przeciec" na zewnatrz
  TransformSeparator'a.}
 OriginalMatrix := State.CurrMatrix;
end;

procedure TNodeTransformSeparator.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
 State.CurrMatrix := OriginalMatrix;
 inherited;
end;

constructor TNodeWWWAnchor.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..1]of string = ('NONE','POINT');
begin
 inherited;
 Fields.Add(TSFString.Create('name', ''));
 Fields.Add(TSFString.Create('description', ''));
 Fields.Add(TSFEnum.Create('map', A1, 0));
end;

class function TNodeWWWAnchor.ClassNodeTypeName: string;
begin
 result := 'WWWAnchor';
end;

constructor TNodeWWWInline.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFString.Create('name', ''));
 Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(0, 0, 0)));
 Fields.Add(TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0)));
 { we must set fParsingAllowedChildren because we inherit value
   AllVRMLNodeKinds from TNodeSeparator }
 fParsingAllowedChildren := false;
 fAllowedChildren := true;
end;

class function TNodeWWWInline.ClassNodeTypeName: string;
begin
 result := 'WWWInline';
end;

procedure TNodeWWWInline.LoadInlined(CanReload: boolean);
begin
 if ChildrenCount > 0 then
 begin
  if CanReload then RemoveAllChildren else exit;
 end;
 AddChild(LoadAsVRML(CombinePaths(WWWBasePath, FdName.Value), false));
end;

procedure TNodeWWWInline.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
 inherited;
 LoadInlined(false);
end;

constructor TNodeFog.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFColor.Create('color', Vector3Single(1, 1, 1)));
 Fields.Add(TSFString.Create('fogType', 'LINEAR'));
 Fields.Add(TSFFloat.Create('visibilityRange', 0));
end;

class function TNodeFog.ClassNodeTypeName: string;
begin
 result := 'Fog';
end;

constructor TNodeBackground.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFFloat.Create('groundAngle', [])); {  [0, Pi/2] }
 Fields.Add(TMFColor.Create('groundColor', [])); {  [0, 1] }
 Fields.Add(TMFString.Create('backUrl', []));
 Fields.Add(TMFString.Create('bottomUrl', []));
 Fields.Add(TMFString.Create('frontUrl', []));
 Fields.Add(TMFString.Create('leftUrl', []));
 Fields.Add(TMFString.Create('rightUrl', []));
 Fields.Add(TMFString.Create('topUrl', []));
 Fields.Add(TMFFloat.Create('skyAngle', [])); {  [0, Pi] }
 Fields.Add(TMFColor.Create('skyColor', [Vector3Single(0, 0, 0)])); {  [0, 1] }

 ImageClassesAssign(FAllowedBgImagesClasses, []);
 FBgImagesLoaded := false;
 FBgImages := BackgroundImagesNone;
end;

class function TNodeBackground.ClassNodeTypeName: string;
begin
 result := 'Background';
end;

procedure TNodeBackground.UnloadImages;
begin
 FBgImagesLoaded := false;
 BackgroundImagesFreeAll(FbgImages);
end;

procedure TNodeBackground.Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList);
begin
 inherited;
 UnloadImages;
end;

function TNodeBackground.GetBgImages: TBackgroundImages;
begin
 if not FBgImagesLoaded then ReloadBgImages;
 result := FBgImages;
end;

procedure TNodeBackground.ReloadBgImages;

  procedure LoadImg(bs: TBackgroundSide; Urls: TMFString);
  var i: Integer;
  begin
   FBgImages[bs] := nil;
   for i := 0 to Urls.Count-1 do
   begin
    try
     FBgImages[bs] := LoadImage( CombinePaths(WWWBasePath, Urls.Items.Items[i]),
       AllowedBgImagesClasses, [], 0, 0);
     Break;
    except on E: Exception do {silence exception}; end;
   end;
  end;

begin
 if FBgImagesLoaded then UnloadImages;

 LoadImg(bsBack, FdBackUrl);
 LoadImg(bsBottom, FdBottomUrl);
 LoadImg(bsFront, FdFrontUrl);
 LoadImg(bsLeft, FdLeftUrl);
 LoadImg(bsRight, FdRightUrl);
 LoadImg(bsTop, FdTopUrl);

 FBgImagesLoaded := true;
end;

procedure TNodeBackground.SetAllowedBgImagesClasses(const Value: array of TImageClass);
begin
 if not ImageClassesEqual(Value, AllowedBgImagesClasses) then
 begin
  ImageClassesAssign(FAllowedBgImagesClasses, Value);
  UnloadImages;
 end;
end;

destructor TNodeBackground.Destroy;
begin
 UnloadImages;
 inherited;
end;

const
  TriangulationUseDef = -1;

constructor TNodeKambiTriangulation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('quadricSlices', TriangulationUseDef));
 Fields.Add(TSFLong.Create('quadricStacks', TriangulationUseDef));
 Fields.Add(TSFLong.Create('rectDivisions', TriangulationUseDef));
end;

class function TNodeKambiTriangulation.ClassNodeTypeName: string;
begin
 result := 'KambiTriangulation';
end;

{$define TRIANGULATION_DETAIL_FUNC:=
function TNodeKambiTriangulation.TRIANGULATION_DETAIL_FUNC_NAME: Cardinal;
begin
 if TRIANGULATION_DETAIL_FIELD.Value = TriangulationUseDef then
  result := TRIANGULATION_DETAIL_GLOBAL_VALUE else
 begin
  if Int64(TRIANGULATION_DETAIL_FIELD.Value) < Int64(TRIANGULATION_DETAIL_MIN) then
  begin
   VRMLNonFatalError(Format('Node "KambiTriangulation" '+
     'field "%s" value is %d but must be >= %d (or = -1)',
     [TRIANGULATION_DETAIL_FIELD_STRING,
      TRIANGULATION_DETAIL_FIELD.Value,
      TRIANGULATION_DETAIL_MIN]));
   TRIANGULATION_DETAIL_FIELD.Value := TRIANGULATION_DETAIL_GLOBAL_VALUE;
  end;

  result := TRIANGULATION_DETAIL_FIELD.Value;
 end;
end;}

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricSlices}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricSlices}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricSlices'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricStacks}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricStacks}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricStacks'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := RectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD := FdRectDivisions}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_RectDivisions}
  {$define TRIANGULATION_DETAIL_MIN := MinRectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'rectDivisions'}
  TRIANGULATION_DETAIL_FUNC

{$undef TRIANGULATION_DETAIL_FUNC_NAME}
{$undef TRIANGULATION_DETAIL_FIELD}
{$undef TRIANGULATION_DETAIL_GLOBAL_VALUE}
{$undef TRIANGULATION_DETAIL_MIN}
{$undef TRIANGULATION_DETAIL_FIELD_STRING}
{$undef TRIANGULATION_DETAIL_FUNC}

{ TNodeUnknown ---------------------------------------------------------------- }

function TNodeUnknown.NodeTypeName: string;
begin
 result := fNodeTypeName;
end;

procedure TNodeUnknown.Parse(Lexer: TVRMLLexer; NodeNameBinding: TStringList);
{sorry - tutaj zrobic parsowanie node'ow unknown typu 2) i 3),
 VRMlNonFatalError tez nie trzeba zawsze rzucac. }
var level: integer;
begin
 { w przypadku TNodeUnknown musimy fAllowedChildren i fParseAllowedChildren
   inicjowac na podstawie parsowania. }
 fAllowedChildren := false;
 fParsingAllowedChildren := false;

 Lexer.CheckTokenIs(vtOpenWasBracket);
 level := 1;
 while (level > 0) and (Lexer.Token <> vtEnd) do
 begin
  Lexer.NextToken;
  if Lexer.Token = vtOpenWasBracket then Inc(level) else
   if Lexer.Token = vtCloseWasBracket then Dec(level);
 end;
 {sprawdz czy nie wyladowalismy na EndOfFile}
 Lexer.CheckTokenIs(vtCloseWasBracket);
 Lexer.NextToken;

 FWWWBasePath := Lexer.WWWBasePath;

 VRMLNonFatalError('Unknown VRML node of type '''+NodeTypeName+
   ''' (named '''+NodeName+''')');
end;

constructor TNodeUnknown.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 { ponizej : "bezpiecznik" zeby nigdy nie tworzyc tego node'a normalnie,
   zeby zawsze fNodeTypeName bylo ustalone. }
 raise Exception.Create('You cannot create Unknown node using default constructor');
end;

constructor TNodeUnknown.CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
begin
 inherited Create(ANodeName, AWWWBasePath);
 fNodeTypeName := ANodeTypeName;
end;

constructor TNodeUnknown.CreateUnknownParse(const ANodeName, ANodeTypeName :string;
  Lexer: TVRMLLexer; NodeNameBinding: TStringList);
begin
 CreateUnknown(ANodeName, '', ANodeTypeName);
 Parse(Lexer, NodeNameBinding);
end;


{ TNodesManager ------------------------------------------------------------ }

procedure TNodesManager.RegisterNodeClass(NodeClass: TVRMLNodeClass);
begin
 if NodeClass.ClassNodeTypeName = '' then
  raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
   'empty ClassNodeTypeName so it cannot be registered in TNodesManager');
 if Registered.IndexOf(NodeClass.ClassNodeTypeName) <> -1 then
  raise ENodesManagerError.Create('Class type name '+NodeClass.ClassNodeTypeName+
    ' was already registered in TNodesManager');
 Registered.AddObject(NodeClass.ClassNodeTypeName, Pointer(NodeClass));
end;

procedure TNodesManager.RegisterNodeClasses(const NodeClasses: array of TVRMLNodeClass);
var i: Integer;
begin
 for i := 0 to High(NodeClasses) do RegisterNodeClass(NodeClasses[i]);
end;

procedure TNodesManager.UnRegisterNodeClass(NodeClass: TVRMLNodeClass;
  ErrorIfNotRegistered: boolean);
var i: Integer;
begin
 if NodeClass.ClassNodeTypeName = '' then
  raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
   'empty ClassNodeTypeName so it cannot be unregistered (or even registered) '+
   'in TNodesManager');

 i := Registered.IndexOf(NodeClass.ClassNodeTypeName);
 if i <> - 1 then
  Registered.Delete(i) else
 if ErrorIfNotRegistered then
  ENodesManagerError.Create('Node class "' + NodeClass.ClassNodeTypeName +
    '" was not registered, so you cannot unregister it');
end;

function TNodesManager.NodeTypeNameToClass(const ANodeTypeName: string): TVRMLNodeClass;
var i: Integer;
begin
 result := nil;
 i := Registered.IndexOf(ANodeTypeName);
 if i = -1 then
  result := nil else
  result := TVRMLNodeClass(Registered.Objects[i]);
end;

constructor TNodesManager.Create;
begin
 inherited;
 Registered := TStringListCaseSens.Create;
end;

destructor TNodesManager.Destroy;
begin
 Registered.Free;
 inherited;
end;

{ global procedures ---------------------------------------------------------- }

function ParseNode(Lexer: TVRMLLexer; NodeNameBinding: TStringList; const AllowedNodes: boolean): TVRMLNode;

  procedure ParseNamedNode(const nodename: string);

    function LexerTokenToNode: TVRMLNodeClass;
    {uwaga - moze zwrocic TNodeUnknown (ktory trzeba tworzyc specjalnym
     konstruktorem)}
    begin
     Lexer.CheckTokenIs(vtName, 'node type');
     result := NodesManager.NodeTypeNameToClass(Lexer.TokenName);
     if result <> nil then
     begin
      if not ({result is allowed in AllowedNodes ?} AllowedNodes) then
       raise EVRMLParserError.Create(Lexer, 'Node type '+result.ClassNodeTypeName+' not allowed here');
     end else
     begin
      if not ({TNodeUnknown is allowed in AllowedNodes ?} AllowedNodes) then
       raise EVRMLParserError.Create(Lexer, 'Unknown node type ('+Lexer.TokenName+') not allowed here');
      result := TNodeUnknown;
     end;
    end;

  var nodeclass: TVRMLNodeClass;
      NodeTypeName: string;
      i: integer;
  begin
   nodeClass := LexerTokenToNode;
   NodeTypeName := Lexer.TokenName;
   Lexer.NextToken;

   if nodeClass <> TNodeUnknown then
    result := nodeclass.CreateParse(nodename, Lexer, NodeNameBinding) else
    result := TNodeUnknown.CreateUnknownParse(nodename, NodeTypeName, Lexer, NodeNameBinding);

   {add NodeName to NodeNameBinding. Note : adding result to
    NodeNameBinding AFTER parsing result we make infinite recursion loops
    impossible.}
   i := NodeNameBinding.IndexOf(NodeName);
   if i >= 0 then
    NodeNameBinding.Objects[i] := result else
    NodeNameBinding.AddObject(NodeName, result);
  end;

var nodename: string;
    i: integer;
begin
 (* node means :
    DEF <name> <nodetype> { node-content } or
    USE <name> or
    <nodetype> { node-content }
 *)

 case Lexer.Token of
  vtKeyword:
    case Lexer.TokenKeyword of
     vkDEF:
       begin
        Lexer.NextTokenForceVTName;
        nodename := Lexer.TokenName;
        Lexer.NextToken;
        ParseNamedNode(nodename);
       end;
     vkUSE:
       begin
        Lexer.NextTokenForceVTName;
        nodename := Lexer.TokenName;

        {get appropriate node}
        i := NodeNameBinding.IndexOf(nodename);
        if i = -1 then
         raise EVRMLParserError.Create(Lexer, 'Incorrect USE clause : node name '+nodename+' undefined');
        result := TVRMLNode(NodeNameBinding.Objects[i]);

        Lexer.NextToken;
       end;
     else raise EVRMLParserError.Create(Lexer,
            'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
    end;
  vtName: ParseNamedNode('');
  else raise EVRMLParserError.Create(Lexer,
         'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
 end;
end;

function ParseVRMLFile(Stream: TPeekCharStream;
  const WWWBasePath: string): TVRMLNode;
var Lexer: TVRMLLexer;
    NodeNameBinding: TStringList;
    childNode: TVRMLNode;
begin
 Lexer := nil;
 NodeNameBinding := nil;
 try
  Lexer := TVRMLLexer.Create(Stream, WWWBasePath);
  NodeNameBinding := TStringListCaseSens.Create;

  result := ParseNode(Lexer, NodeNameBinding, true);
  try

   { ponizej : tak wyglada implementacja tej procedury ktora wymagalaby zeby
     caly plik byl jednym nodem VRMLa (tak jak tego wymaga specyfikacja VRMLa 97).

       Lexer.CheckTokenIs(vtEnd, 'end of file (remember : VRML 1.0 files can contain only one "global" node)');

     Nie uzywam jej, zamiast tego pozwalam plikow zawsze miec wiele node'ow
     i jezeli maja ich wiecej niz 1 to wrzucam je do stworzonego sztucznie
     jednego node'a Group (wiec odczytany i zapisany z powrotem w ten sposob
     plik zawsze ma jeden node w pliku). Zaimplementowalem to bo jest wiele
     tacich niepoprawnych plikow VRMLa 1.0 w internecie ktore maja wiecej
     niz jeden node zdefiniowany. Poza tym jest to krok w strone VRMLa 97
     (ktorego specyfikacja jawnie na to pozwala). Poza tym jest to proste i
     do zaimplementowania i jest calkiem logicznym rozszerzeniem VRMLa 1.0.
   }

   if Lexer.Token <> vtEnd then
   begin
    childNode := result;
    result := TNodeGroup.Create('', WWWBasePath);
    result.AddChild(childNode);

    repeat
     result.AddChild(ParseNode(Lexer, NodeNameBinding, true));
    until Lexer.Token = vtEnd;
   end;

  except result.Free; raise end;
 finally
  Lexer.Free;
  NodeNameBinding.Free
 end;
end;

function ParseVRMLFile(const FileName: string; AllowStdIn: boolean): TVRMLNode;
{
  First version.
  This will be nice if I will modify one day TDecompressionStream
  to make it able to handle gzip compressed streams
  (not zlib-compressed streams, as it does for now).

  function DoIt(BaseStream: TStream; FreeBaseStream: boolean;
    const WWWBasePath: string): TVRMLNode;
  var
    Stream: TPeekCharStream;
  begin
   try
    Stream := TBufferedReadStream.Create(
      TDecompressionStream.Create(BaseStream, 15+16), true);
    try
     Result := ParseVRMLFile(Stream, WWWBasePath);
    finally Stream.Free end;
   finally
    if FreeBaseStream then BaseStream.Free;
   end;
  end;

begin
 if AllowStdIn and (FileName = '-') then
  Result := DoIt(StdInStream, false, GetCurrentDir) else
  Result := DoIt(TFileStream.Create(FileName, fmOpenRead), true,
    ExtractFilePath(ExpandFilename(filename))); }

  function DoIt(BaseStream: TStream; FreeBaseStream: boolean;
    const WWWBasePath: string): TVRMLNode;
  var
    Stream: TPeekCharStream;
  begin
   Stream := TBufferedReadStream.Create(BaseStream, FreeBaseStream);
   try
    Result := ParseVRMLFile(Stream, WWWBasePath);
   finally Stream.Free end;
  end;

begin
 if AllowStdIn and (FileName = '-') then
  Result := DoIt(StdInStream, false, GetCurrentDir) else
 begin
  if SameText(ExtractFileExt(FileName), '.gz') then
   Result := DoIt(TGZFileStream.Create(FileName, gzOpenRead), true,
     ExtractFilePath(ExpandFilename(FileName))) else
   Result := DoIt(TFileStream.Create(FileName, fmOpenRead), true,
     ExtractFilePath(ExpandFilename(FileName)));
 end;
end;

procedure VRMLNonFatalError_WarningWrite(const s: string);
begin WarningWrite(ProgramName+ ': WARNING: '+ s) end;

procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
begin raise EVRMLError.Create(s); end;

procedure VRMLNonFatalError_Ignore(const s: string);
begin end;

procedure SaveToVRMLFile(Node: TVRMLNode; Stream: TStream; const PrecedingComment: string);
var NodeNameBinding: TStringList;
begin
 NodeNameBinding := TStringListCaseSens.Create;
 try
  WriteStr(Stream, VRML10SignatureLine +nl +nl);
  if PrecedingComment <> '' then WriteStr(Stream, '# '+PrecedingComment +nl +nl);
  Node.SaveToStream(Stream, '', NodeNameBinding);
 finally NodeNameBinding.Free end;
end;

procedure SaveToVRMLFile(Node: TVRMLNode; const Filename, PrecedingComment: string);
var Stream: TFileStream;
begin
 Stream := TFileStream.Create(Filename, fmCreate);
 try
  SaveToVRMLFile(Node, Stream, PrecedingComment);
 finally Stream.Free end;
end;

{ miscellaneous  ------------------------------------------------------------ }

function CombinePaths(const BasePath, RelPath: string): string;
begin
 if IsPathAbsolute(RelPath) then
  result := RelPath else
 {$ifdef WIN32}
 if IsPathAbsoluteOnDrive(RelPath) then
  result := BasePath[1] +DriveDelim +RelPath else
 {$endif}
  {sorry - ponizej jest miejsce ktore jest zrobione nieporzadnie}
  result := InclPathDelim(BasePath)+RelPath;
end;

{ unit init/fini ------------------------------------------------------------ }

initialization
 NodesManager := TNodesManager.Create;
 NodesManager.RegisterNodeClasses([
   TNodeAsciiText, TNodeCone, TNodeCube, TNodeCylinder,
   TNodeIndexedFaceSet, TNodeIndexedTriangleMesh, TNodeIndexedLineSet,
   TNodePointSet, TNodeSphere,
   TNodeCoordinate3, TNodeFontStyle, TNodeInfo, TNodeLOD, TNodeMaterial,
   TNodeMaterialBinding, TNodeNormal, TNodeNormalBinding, TNodeTexture2,
   TNodeTexture2Transform,
   TNodeTextureCoordinate2, TNodeShapeHints,
   TNodeMatrixTransform, TNodeRotation, TNodeRotationXYZ,
   TNodeScale, TNodeTransform,
   TNodeTranslation,
   TNodeOrthographicCamera, TNodePerspectiveCamera,
   TNodeDirectionalLight, TNodePointLight, TNodeSpotLight,
   TNodeGroup, TNodeSeparator, TNodeSwitch, TNodeTransformSeparator,
   TNodeWWWAnchor,
   TNodeWWWInline, TNodeFog, TNodeBackground, TNodeKambiTriangulation]);
finalization
 FreeAndNil(NodesManager);
end.
