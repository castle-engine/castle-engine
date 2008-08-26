{
  Copyright 2002-2008 Michalis Kamburelis.

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

(*
  @abstract(Define all VRML / X3D nodes, along with other bulding blocks
  of VRML / X3D (prototypes, routes etc.).)

  This is the most important unit for VRML processing,
  as VRML file is basically just a nodes' graph.
  In fact, we represent VRML file as just one root node
  (if necessary, we will wrap actual file nodes within
  one "artificial" node, see TVRMLRootNode_1 or TVRMLRootNode_2).

  The chapter "Reading, writing, processing VRML scene graph"
  in the documentation on
  [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.scene_graph.html]
  is almost completely devoted to documenting the design of this single unit.

  Nodes can be loaded/saved from stream in "classic" encoding
  (the only encoding available before X3D), using internally
  the lexer (in VRMLLexer unit) and parser of VRML fields
  (in VRMLFields unit).

  For VRML nodes' graph processing, this unit offers a lot TVRMLNode
  methods. See e.g. TVRMLNode.Traverse, TVRMLNode.EnumerateNodes and
  TVRMLNode.FindNode. Traverse is especially important, since it
  walks through VRML graph just as VRML specification says
  (e.g. visiting only one child from a Switch node's children),
  gathering some state (useful especially for VRML 1.0, but also
  used for various things in later VRML versions).
  When you want to e.g. render VRML graph, you can just traverse
  the graph and render each pair of State with a geometry node
  (some TVRMLGeometryNode instance).
  (Alternatively, simple renderer can also use TVRMLGeometryNode.Triangulate).

  Many node classes have also specific routines useful for manipulating
  them (for example loading the contents of various Inline and Texture nodes,
  extracting transformation from nodes that can change it etc.).
  TVRMLGeometryNode in particular has some useful routines
  for calculating bounding box, vertex/triangles counts and
  to triangulate the node.

  This unit doesn't depend on OpenGL, or any other particular rendering
  method. So it's suitable also for VRMLRayTracer, and every other possible
  renderer that will ever get implemented.

  VRML node classes names, and inheritance:

  @unorderedList(
    @item(Pascal classes named like TNodeXxx are @bold(official) node classes,
      that is they implement VRML / X3D specification node Xxx.
      This also applies to some extension nodes (for example,
      http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php).
      The important fact is that these node names are somehow available to
      the user, required by some specification etc.

      X3D abstract node classes also fall into this category (they are
      specified in X3D specification; although nothing actually forces us
      to implement these abstract X3D classes, and VRML author cannot directly use
      them, I decided it's a proper way to define these classes.).

      They all descend from TVRMLNode. For example, TNodeIndexedFaceSet.

      There are also Pascal interfaces, like INodeXxx that represent
      VRML node Xxx interface, more on this below. They all descend from IVRMLNode.

      The second group are @bold(unofficial) node classes. These are internal
      to our engine, help our implementation, and often may help
      a programmer to process some VRML files --- but they are not visible
      to final VRML user/author. They are named TVRMLXxxNode,
      like TVRMLGeometryNode, TVRMLTextureNode, TVRMLLightNode, TVRMLUnknownNode.

      So you can immediately tell which nodes come from
      VRML / X3D specification and which nodes are only added by our engine
      to simplify implementation.

      Before X3D, adding my own TVRMLXxxNode classes was the only way to
      add some interesting inheritance among VRML nodes.
      This allows to reuse implementation, and also simplifies some processing:
      for example, you can search the scene for any TVRMLLightNode, this
      way looking for all PointLight, SpotLight etc. nodes.)

    @item(
      Optional suffix _1 or _2 or _3 after the node class name indicates that
      this node class is used only for given major VRML version.
      _1 means VRML 1.0, _2 means VRML 2.0 (aka 97), _3 means VRML 3.0 (aka X3D).
      Also _2 sometimes mean both VRML 2.0 and 3.0.

      This suffix is used when there are nodes with the same name in VRML
      specifications, but they have to be handled by different Pascal classes
      since they are incompatible.

      For example, we have TNodeIndexedFaceSet_1 for VRML 1.0 and
      TNodeIndexedFaceSet_2 for VRML >= 2.0 (97, X3D).)

    @item(
      Since X3D introduced abstract node classes, and defines node
      inheritance, we follow this by actually defining in our engine
      all the X3D abstract classes and using them. Inheritance of our
      Pascal classes reflects inheritance of nodes in X3D specification.

      Many X3D nodes are declared as both Pascal interface and class.
      That's because we preserve X3D inheritance graph, and when node
      descends from more than one VRML class --- the rest of VRML classes
      has to be expressed as Pascal interfaces. For now, these interfaces
      have little use beside simple "is" checking, but they may be more
      useful in the future.)
  )

  As for VRML versions handling:

  @unorderedList(
    @item(
      We handle VRML 1.0, VRML 2.0 (aka 97) and X3D (aka VRML 3.x).

      Every correct VRML / X3D file in classic encoding should be parsed
      by this unit. (For X3D XML encoding, see X3DXMLToVRML unit).
      See [http://vrmlengine.sourceforge.net/vrml_implementation_status.php]
      for much more detailed information about supported features.)

    @item(
      Also many Inventor 1.0 files are correctly parsed.
      We handle Inventor 1.0 mostly like VRML 1.0, also some small
      things and nodes specific for Inventor 1.0 are implemented here, see
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_iv_in_vrml].)

    @item(
      Note that structures in this unit are @italic(not) focused
      on either VRML 1.0 or VRML >= 2.0. On the contrary: we try to handle
      the @italic(sum of all VRML and X3D). When reading VRML 1.0,
      many VRML 2.0 constructs (that not conflict with anything in VRML 1.0)
      are allowed, and the other way around too.

      Internally, we do not convert VRML 1.0-specific constructs
      to VRML 2.0 constructs (or the other way around).
      See [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.vrml_1_2_sum.html]
      for more in-depth explanation of how, and why, we handle both
      old-style (Inventor, VRML 1.0) and new-style (VRML 2.0, X3D) VRML
      syntax.)
  )

  Note that when reading VRML files, we generally do not change the
  VRML graph. So we're able to save exactly the same VRML graph
  back to another file. See also
  [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/ch02s04.html#id2671890].
  This makes an excellent opportunity for writing various VRML
  processing tools, that can simply read the file, change whatever
  they want, and write the file back --- knowing that the "untouched"
  parts of VRML graph are preserved nicely.

  Takie unikanie dekonstrukcji pozwoli nam
  @orderedList(
    @item(
      na unikniecie zbytniego przywiazania naszego kodu VRMLa do konkretnych
      zastosowan. Poniewaz mamy cala informacje o scenie mozemy zrobic
      wszystko co mozemy zrobic ze scena VRMLa - co nie byloby mozliwe gdybysmy
      w czasie dekonstrukcji (np. wykonujac juz w czasie odczytu wszystkie
      transformacje na macierzy i transformujac punkty) tracili jakas czesc
      informacji.)

    @item(
      no i mozemy w ten sposob latwo wykorzystac nasz kod VRMLa do pisania
      konwerterow innych formatow na VRMLa. Uzywajac modulu Object3dAsVRML
      i tutejszego SaveToVRMLFile mamy sliczny konwerter 3ds, obj, geo -> VRML.)
  )

  Specyfikacja VRMLa 1.0 z dodanymi "moimi rozszerzeniami VMRLa"
  [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php] stanowia
  uzasadnienie dla wielu rzeczy ktore robimy w tym module.

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

  Implementation files organization notes:

  @unorderedList(
    @item(vrml1nodes.inc contains only VRML-1.0 specific nodes)
    @item(vrml97nodes.inc contains only VRML 97 specific nodes)
    @item(various x3d_xxx.inc contain X3D nodes belonging to specific xxx component.
      So most of the nodes (also VRML 97 nodes, since they are usually the same
      for VRML 97 and X3D) are inside x3d_xxx.inc.
      That's a nice thing, since X3D components provide a natural way to group
      the vast number of nodes into files.)
  )
*)

unit VRMLNodes;

interface

uses VectorMath, Classes, SysUtils, VRMLLexer, KambiUtils, KambiClassUtils,
  VRMLFields, Boxes3d, Images, TTFontsTypes, BackgroundBase, VRMLErrors,
  ImagesCache, VideosCache, KambiInterfaces, Videos, KambiTimeUtils;

{$define read_interface}

const
  { }
  CountTraverseStateLastNodes = 10;
  HighTraverseStateLastNodes = CountTraverseStateLastNodes - 1;

const
  DefaultMaterial_1AmbientColor: TVector3Single = (0.2, 0.2, 0.2);
  DefaultMaterial_2AmbientIntensity = 0.2;
  DefaultMaterialDiffuseColor: TVector3Single = (0.8, 0.8, 0.8);
  DefaultMaterialSpecularColor: TVector3Single = (0, 0, 0);
  DefaultMaterialEmissiveColor: TVector3Single = (0, 0, 0);
  DefaultMaterialShininess = 0.2;
  DefaultMaterialTransparency = 0.0;
  DefaultMaterialMirror = 0.0;
  DefaultMaterialReflSpecularExp = 1000000;
  DefaultMaterialTransSpecularExp = 1000000;

{ -----------------------------------------------------------------------------
  dluuuga deklaracja "type" w ktorej wiele rzeczy jest zdefiniowanych
  wzajemnie (rekurencyjnie). }

type
  { forward declarations } { }
  TVRMLNodesList = class;
  TVRMLLightNodesList = class;
  TVRMLNode = class;
  TNodeCoordinate3 = class;
  TNodeShapeHints = class;
  TNodeFontStyle_1 = class;
  TNodeMaterial_1 = class;
  TNodeMaterialBinding = class;
  TNodeNormal = class;
  TNodeNormalBinding = class;
  TNodeTexture2 = class;
  TNodeTextureCoordinate2 = class;
  TVRMLGeometryNode = class;
  TVRMLLightNode = class;
  TNodeKambiTriangulation = class;
  TNodeX3DShapeNode = class;
  TVRMLTextureNode = class;
  TNodeBlendMode = class;

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
           FontStyle :TNodeFontStyle_1;
           Material :TNodeMaterial_1;
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

  { This is used in TVRMLGraphTraverseState to keep track of used light.
    When the light is used, not only it's node is important (in LightNode),
    but also current transformation (this transformation affects things
    like actual position and direction). This record keeps it all.

    Moreover, it provides precalculated properties like TransfLocation
    and TransfNormDirection. Their computation too often would be somewhat
    expensive, and thanks to this class we can calculate them just once
    when they are found by traversing VRML file.

    @bold(This record may be initialized only by
    TVRMLLightNode.CreateActiveLight). }
  TActiveLight = record
    LightNode: TVRMLLightNode;

    Transform: TMatrix4Single;
    AverageScaleTransform: Single;

    { TransfLocation to juz przeliczone Transformation*Location dla swiatel
      TVRMLPositionalLightNode. }
    TransfLocation: TVector3Single;

    { TransfNormDirection to juz
      znormalizowane i transformowane Direction dla swiatel Directional i Spot
      (jest transformowane tak zeby przesuniecia nie mialy znaczenia,
      licza sie tylko obroty i skalowania). }
    TransfNormDirection: TVector3Single;

    { For VRML 2.0 positional lights, this is precalculated node's radius
      scaled by Transform. }
    TransfRadius: Single;
  end;
  PActiveLight = ^TActiveLight;

  TDynArrayItem_1 = TActiveLight;
  PDynArrayItem_1 = PActiveLight;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynActiveLightArray = class(TDynArray_1)
  public
    { -1 jesli nie ma }
    function IndexOfLightNode(LightNode: TVRMLLightNode): integer;
    function Equals(SecondValue: TDynActiveLightArray): boolean;
  end;
  TArray_ActiveLight = TInfiniteArray_1;
  PArray_ActiveLight = PInfiniteArray_1;

  { This describes current "state" (current transformation and such)
    when traversing VRML graph.

    For VRML >= 2.0 this could be smaller, as VRML >= 2.0 doesn't need
    to keep track of all these LastNodes. But we want to handle VRML 1.0
    100% correctly still, so there we are.

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
    OwnsLastNodes: boolean;
    procedure CommonCreate;
  public
    { nie, ParentsCount elementow Last* NIE odzwierciedla faktu ze sa one
      podlegle TVRMLRenderState. W ogole ten obiekt nie zajmuje sie
      zarzadzaniem tymi polami - on tylko przechowuje sobie ich wartosci.
      W szczegolnosci wiec gwarantowane jest ze obiekty nigdy nie beda nil
      ale ta gwarancja musi byc takze zapewniona przez kod ktory tworzy
      ten obiekt przez Create. }
    property LastNodes: TTraverseStateLastNodes read FLastNodes;

    { Lights active in this state, two separate versions for each VRML flavor
      needed here.

      VRML2ActiveLights should be for any VRML >= 2 (including X3D),
      VRML1ActiveLights should be for any VRML <= 1 (including Inventor).

      VRML 2.0 may have more lights active
      (since DirectionalLight affects all siblings, and other lights affect
      potentially all). But VRML 2.0 may also have some lights less,
      since some lights are limited by radius and may be determined to
      not light here.

      Also, note that VRML2ActiveLights cannot be fully calculated in Traverse
      pass (contrary to everything else in TVRMLGraphTraverseState).
      DirectionalLights are calculated here, but positional lights have
      to be calculated later (VRML 2 spec says that they affect whole scene,
      based on their radius, and regardless of their position in VRML graph;
      so this is not possible to fill during Traverse call).
      See how UpdateVRML2ActiveLights in TVRMLFlatScene does this. }
    VRML1ActiveLights, VRML2ActiveLights: TDynActiveLightArray;

    { This returns VRML1ActiveLights or VRML2ActiveLights, based on VRML
      flavor used to render with this state.

      More precisely, it checks "VRML flavor" by looking at ParentShape:
      when ParentShape is @nil, we're in VRML 1 mode, otherwise in VRML 2 mode. }
    function CurrentActiveLights: TDynActiveLightArray;

    { Current transformation. }
    Transform: TMatrix4Single;

    { Inverted @link(Transform) matrix. This matrix is crucial for some
      special effects (for example, it's needed for calculating in tangent space
      for bump mapping).

      This is not calculated using any complex matrix inversion algorithms.
      Instead, this is calculated by updating this along the way, just like
      we calculate normal @link(Transform). So it's simple, quick, and
      guaranteed to be correct @italic(assuming that user didn't use
      VRML 1.0 MatrixTransform along the way). That's why MatrixTransform node
      was removed from VRML 2.0, it breaks such things.

      Also, any scale with zero component along the way will make this
      partially invalid (we'll substitute identity in place of inverted scaling
      matrix). This is unavoidable, there's no reverse matrix for scaling
      with zero factor, since one resulting point may correpond to infinitely many
      source points (i.e., it's natural that such scaling function cannot be
      reversed). }
    InvertedTransform: TMatrix4Single;

    { This is a uniform scale caused by matrix Transform.

      I.e., if we could extract the scaling from Transform, then it would
      be AverageScaleTransform. Assuming that the scale is uniform.
      If we used any non-uniform scale along the way, this is the average scale.

      This is calculated by updating it along the way, just like Transform
      is updated. This way it's calculated easily --- contrary to the
      non-trivial operation of extracting a scale from a 4x4 matrix.
      It's also 100% correct, @italic(assuming that user didn't use
      VRML 1.0 MatrixTransform along the way). }
    AverageScaleTransform: Single;

    { Current texture transformation. Usable only for VRML 1.0, in VRML 2.0
      texture transformations don't accumulate like modelview transformations. }
    TextureTransform: TMatrix4Single;

    ParentShape: TNodeX3DShapeNode;

    constructor CreateCopy(Source: TVRMLGraphTraverseState);
    constructor Create(const ADefaultLastNodes: TTraverseStateLastNodes); overload;

    { Standard create, with standard initial LastNodes state.

      This is equivalent to creating last nodes like
      @longCode(#  TraverseState_CreateNodes(StateDefaultNodes) #)
      then creating this object with
      @longCode(#  Create(StateDefaultNodes) #)
      When this object will be freed, such implicitly created StateDefaultNodes
      will be also freed (using TraverseState_FreeAndNilNodes).

      Note: while this constructor seems very comfortable, in some cases
      it's not useful, exactly because it frees at the end used StateDefaultNodes.
      Consider e.g. TVRMLFlatScene, that has to traverse all nodes and
      store the traversing result in a flat list: this means that it must
      save various TVRMLGraphTraverseState instances, that may have
      references to nodes from StateDefaultNodes. So it must have independent
      StateDefaultNodes field that "lives" for the whole lifetime
      of TVRMLFlatScene and is passed to each TVRMLGraphTraverseState.Create call.

      If you don't understand the note above then don't worry,
      you're probably fine with using this parameter-less constructor :) }
    constructor Create; overload;

    destructor Destroy; override;

    { Compare with other TVRMLGraphTraverseState instance.
      The idea is that two states are equal if, when applied to the same
      VRML node, they "mean" the same thing, i.e. produce the same shape,
      behavior etc.

      So note that @name doesn't compare OwnsLastNodes values,
      as they don't really define the "content" of the state,
      they don't change the behavior of VRML node affected by this state. }
    function Equals(SecondValue: TVRMLGraphTraverseState): boolean;

    { This is like @link(Equals) but it ignores some fields that are
      ignored when rendering using
      TVRMLOpenGLRenderer.RenderShapeStateNoTransform.
      For example, it ignores Transform, AverageScaleTransform, InvertedTransform. }
    function EqualsNoTransform(SecondValue: TVRMLGraphTraverseState): boolean;

    { Returns proper texture node that should be used
      for nodes within this State, regardless whether this in
      VRML 1.0 or 2.0.

      Details:
      If ParentShape <> nil, this returns texture node taken from
      ParentShape.Texture (note that it may be nil, if Apperance
      of Appearance.Texture node is NULL in VRML).
      Otherwise it returns texture from LastNodes.Texture2. }
    function Texture: TVRMLTextureNode;

    { Returns BlendMode for this state, or @nil if not present.

      Details: if ParentShape <> nil (VRML >= 2.0), and it's Appearance <> nil, and it's
      a KambiAppearance and it has BlendMode <> nil... then returns it.
      Otherwise @nil. }
    function BlendMode: TNodeBlendMode;
  end;

  PTraversingInfo = ^TTraversingInfo;
  TTraversingInfo = record
    Node: TVRMLNode;
    ParentInfo: PTraversingInfo;
  end;

  { Used as a callback by TVRMLNode.Traverse. }
  TTraversingFunc = procedure (Node: TVRMLNode;
    State: TVRMLGraphTraverseState;
    ParentInfo: PTraversingInfo) of object;

  { Used as a callback by TVRMLNode.TraverseBlenderObjects. }
  TBlenderTraversingFunc = procedure (
    BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
    BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
    GeometryNode: TVRMLGeometryNode;
    State: TVRMLGraphTraverseState) of object;

  TEnumerateChildrenFunction =
    procedure (Node, Child: TVRMLNode) of object;

  TEnumerateReplaceNodesFunction =
    procedure (ParentNode: TVRMLNode; var Node: TVRMLNode) of object;

  TNewTriangleProc = procedure (const Tri: TTriangle3Single;
    State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer) of object;

  TSFNode = class;
  TMFNode = class;
  TVRMLPrototypeNode = class;
  TVRMLPrototypeBasesList = class;
  TVRMLRoutesList = class;

  TVRMLAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);
  TVRMLAccessTypes = set of TVRMLAccessType;

  TVRMLInterfaceDeclarationsList = class;

  TNodeDestructionNotification = procedure (Node: TVRMLNode) of object;

  TDynArrayItem_2 = TNodeDestructionNotification;
  PDynArrayItem_2 = ^TNodeDestructionNotification;
  {$define DYNARRAY_2_IS_FUNCTION}
  {$define DYNARRAY_2_IS_FUNCTION_METHOD}
  {$define DYNARRAY_2_USE_EQUALITY}
  {$I dynarray_2.inc}
  TDynNodeDestructionNotificationArray = class(TDynArray_2)
  public
    { This calls all functions (all Items) passing them Node parameter. }
    procedure ExecuteAll(Node: TVRMLNode);
  end;

  { Basic VRML node interface class, all other interfaces for VRML nodes descend
    from this. }
  IVRMLNode = interface
  ['{5BD33327-430F-47EC-9241-AD899F072BF8}']
  end;

  { VRML node.

    Descendant implementors note: Each descendant should
    override constructor to create and add his fields and events.
    Like @code(Fields.Add(TSFFloat.Create('width', 2, true))).
    Also, you should define FdXxx properties that allow fast,
    comfortable and type-secure way to retrieve and set these fields. }
  TVRMLNode = class(TVRMLFileItem, IVRMLNode)
  private
    fNodeName: string;
    FWWWBasePath: string;
    FChildren, FParentNodes: TVRMLNodesList;
    function GetChildrenItem(i: integer): TVRMLNode;
    function GetParentNodesItem(i: integer): TVRMLNode;
    procedure SetChildrenItem(I: Integer; Value: TVRMLNode);
    FParentFields: TVRMLFieldsList;
    function GetParentFieldsItem(Index: Integer): TVRMLField;
    function GetParentFieldsNodeItem(Index: Integer): TVRMLNode;
    procedure RemoveParentField(Field: TVRMLField);
    procedure AddParentField(Field: TVRMLField);
    function GetSmartChildren(Index: Integer): TVRMLNode;
    procedure AndSuggestedVRMLVersion(
      var Result: boolean;
      var VerMajor, VerMinor, SuggestionPriority: Integer;
      const NewResult: boolean;
      const NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer);
    procedure TryFindNode_Found(Node: TVRMLNode);
    FPrototypes: TVRMLPrototypeBasesList;
    FRoutes: TVRMLRoutesList;
    FFields: TVRMLFieldsList;
    FEvents: TVRMLEventsList;
    FPrototypeInstance: boolean;
    FPrototypeInstanceSourceNode: TVRMLPrototypeNode;
    FPrototypeInstanceHelpers: TVRMLNode;
    FDefaultContainerField: string;
    FExplicitContainerField: string;
    FHasInterfaceDeclarations: TVRMLAccessTypes;
    procedure SetHasInterfaceDeclarations(const Value: TVRMLAccessTypes);
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;
    FCDataAllowed: boolean;
    FCDataExists: boolean;
    FCData: string;
    FDestructionNotifications: TDynNodeDestructionNotificationArray;
  protected
    fAllowedChildren: boolean;
    fParsingAllowedChildren: boolean;

    { This enumerates all active child nodes of given node.

      "Active nodes" are the ones affecting current VRML graph look
      or collisions, e.g. from Switch
      node only one child will be enumerated.
      See @link(Traverse) for more precise definition.

      "Direct" means that this enumerates only direct
      descendants, i.e. this is not recursive.
      See methods like Traverse or EnumerateNodes if you
      want recursive behavior.

      This can enumerate both @link(Children) nodes in VRML 1.0
      style and nodes within TSFNode and TMFNode fields.

      Default implementation in this class returns all Children
      nodes of VRML 1.0. If you need to remove some children
      for VRML 1.0 (e.g. for Switch or LOD nodes)
      or add some children for VRML 2.0 you
      have to override this. You're not required to call
      inherited when overriding this. }
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); virtual;

    { This simply enumerates all direct descendant nodes of
      this node. I.e. all children in VRML 1.0 style and
      all nodes in SFNode and MFNode fields. }
    procedure DirectEnumerateAll(
      Func: TEnumerateChildrenFunction);

    { This enumerates direct descendant nodes of this node.
      This is equivalent to DirectEnumerateActive or
      DirectEnumerateAll, depending on value of OnlyActive param. }
    procedure DirectEnumerate(
      Func: TEnumerateChildrenFunction;
      OnlyActive: boolean);

    { You can override these methods to determine what happens when
      given node is traversed during Traverse call.
      The main use of this is to modify TVRMLGraphTraverseState.

      Remember to always call inherited when overriding.
      In BeforeTraverse and MiddleTraverse you should call inherited
      at the beginning, in AfterTraverse inherited should be called at the end.

      Besides changing State fields, you can even replace the curent State
      instance in BeforeTraverse. But in this case, you @italic(must
      change it back to original value in AfterTraverse).

      @groupBegin }
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); virtual;
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); virtual;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); virtual;
    { @groupEnd }

    { Parse VRML node body element. Usually, this is a field.
      May also be VRML 1.0 style child node.
      May also be VRML 2.0 Script node interface declaration, etc.
      --- see VRML 2.0 grammar spec.

      This should be overriden to parse special features within particular
      nodes. While generally VRML is very clean and there's no need to
      override this, there's one use for this currently:

      @orderedList(
        @item(Since we handle a couple of VRML flavors (at least
          Inventor, VRML 1.0 and VRML 97), sometimes the same node has
          different fields to express the same things in various VRML flavors.
          So it may be useful to parse a field and copy it's value into
          other fields.

          Example: TNodeShapeHints in Inventor parses "hints" field,
          and copies it's value to other fields as appropriate.
          "hints" field is not exposed in TNodeShapeHints interface,
          so everything is clean in the interface, and under the hood
          TNodeShapeHints can "magically" handle "hints" field for Inventor.)
      )


      When overriding, always check inherited result first, and exit if
      inherited handled successfully.
      Otherwise either read your stuff and return @true
      (Lexer should advance to the position of next "nodeBodyElement").
      Or return @false without changing Lexer position. }
    function ParseNodeBodyElement(Lexer: TVRMLLexer;
      const APositionInParent: Integer): boolean; virtual;

    (* This will be called by SaveToStream within { }.
       Usually you want to save here what you read in your overridden
       ParseNodeBodyElement. *)
    procedure SaveContentsToStream(SaveProperties: TVRMLSaveToStreamProperties);
      virtual;
  public
    { Node fields.

      For normal nodes, all Fields are created and added
      to Fields list in constructor. Fields default values are set,
      and of course current field values are set to these defaults.
      Later, we only modify these fields current values (e.g. when parsing).

      However, there are special node classes that set their Fields differently.
      TVRMLPrototypeNode has their fields set according to it's VRML 2.0 prototype.
      TVRMLUnknownNode may have it's fields set by VRML 1.0 "fields" feature
      (so it's Fields are initialized by parsing it).

      All fields on this list are owned by this object. }
    property Fields: TVRMLFieldsList read FFields;

    { Explicit events (that is, not exposed by some field) of this node.
      For exposed events, see each field's property ExposedEvents. }
    property Events: TVRMLEventsList read FEvents;

    { Search by name for given field or event (exposed by some field or not).

      If SearchInterfaceDeclarations, then will also search
      InterfaceDeclarations, if available (if HasInterfaceDeclarations <> []).
      For most cases, you want this as @true, since you want to treat
      InterfaceDeclarations mostly as regular fields/events.

      @nil if not found. }
    function FieldOrEvent(const Name: string;
      SearchInterfaceDeclarations: boolean): TVRMLFieldOrEvent;

    { Search by name for given event (exposed by some field or not).

      If SearchInterfaceDeclarations, then will also search
      InterfaceDeclarations, if available (if HasInterfaceDeclarations <> []).
      For most cases, you want this as @true, since you want to treat
      InterfaceDeclarations mostly as regular fields/events.

      @nil if not found. }
    function AnyEvent(const Name: string;
      SearchInterfaceDeclarations: boolean): TVRMLEvent;

    { Children property lists children VRML nodes, in the sense of VRML 1.0.
      In VRML 2.0, nodes never have any Children nodes expressed on this
      list (however, their children nodes may be expressed as items
      of TMFNode / TSFNode fields).

      Kazdy VRML nodes moze miec dowolnie wiele Children.
      Kiedy jakis node jest na liscie Children
      jednego node'a to ma swojego rodzica na swojej liscie ParentNodes.
      Wiec w ten sposob mozemy podrozowac po grafie w obie strony.
      (pamietaj ze graf VRML'a nie ma cykli gdy na niego patrzec jak na graf
      skierowany (a takim wlasnie jest) ale kazdy node moze miec wiele rodzicow
      wiec jezeli potraktujemy go jako graf nieskierowany to mozemy otrzymac
      cykle; wszystko przez to ze node moze miec wiele ParentNodes bo moze
      uzywac mechanizmu USE).

      Kiedy jakis node jest na liscie Children innego node'a to gdy ten inny
      node bedzie go kasowal ze swojej listy Children (a w destruktorze
      kazdy node kasuje wszystkich ze swojej listy Children) to wywola
      jego destruktora. Innymi slowy, gdy jakis node jest czyims dzieckiem
      to jest reference-counted i automatycznie zwalniany.
      Actually, nodes can be children of both nodes (VRML 1.0 style,
      then Children and ParentNodes is used) or fields (TMFNode or TSFNode,
      in VRML 2.0 style; then ParentFields is used). So the node is freed
      only when it's not referenced by any node and not referenced by any
      field.

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
      kolejnosci w ParentNodes; inaczej musielibysmy z kazdym ParentNodes pamietac
      swoj index na jego liscie). Tak wiec na listach Children i ParentNodes
      moga byc duplikaty i zdecydowanie nie powinnismy nigdzie niefrasobliwie
      "czyscic" tych list przez DeleteDuplicates;

      You can also replace one children with another by writing
      to this property, like @code(Children[I] := NewChildren;).
      This works like a shortcut for
      @code(RemoveChild(I); AddChild(I, NewChildren);).
      But 1. it's more efficient; 2. it's safer --- if
      Children[I] is already equal to NewChildren, then
      first @code(RemoveChild(I);) would free this children and following
      AddChild would be totally wrong. }
    property Children[i: integer]: TVRMLNode
      read GetChildrenItem write SetChildrenItem;

    function ChildrenCount: integer;

    { AddChild z Index (musi byc w zakresie 0..ChildrenCount)
      przesuwa elementy o numerach Index i wiekszych w prawo i
      wstawia child na wskazane Index.    }
    procedure AddChild(Index: Integer; child: TVRMLNode); overload;

    { AddChild bez Indexu - dodaje na koniec listy Children. }
    procedure AddChild(child: TVRMLNode); overload;

    procedure RemoveChild(i: integer);
    procedure RemoveAllChildren;

    { Remove and return children indexed I, and @italic(never free it).

      Compare this with RemoveChild, that removes children I and
      frees it if it's reference count gets 0.

      ExtractChild removes children I, appropriately adjusting
      all parent/children links, but even if reference count of the node
      will get zero, ExtractChild will not free it.
      ExtractChild always returns extracted child. }
    function ExtractChild(I: Integer): TVRMLNode;

    { All nodes where this node is referenced as a child.
      This counts "parents" in the VRML 1.0 / Inventor sense.

      @groupBegin }
    property ParentNodes[i: integer]:TVRMLNode read GetParentNodesItem;
    function ParentNodesCount: integer;
    { @groupEnd }

    { This lists all SFNode and MFNode fields where this node is referenced.
      This is somewhat analogous for ParentNodes, but for VRML 2.0.

      ParentFieldsNode is just for your comfort, it returns always
      appropriate field's ParentNode property value
      (i.e. @code((ParentField[Index] as TSFNode).ParentNode)
      or @code((ParentField[Index] as TMFNode).ParentNode)).

      @groupBegin }
    property ParentFields[Index: Integer]: TVRMLField read GetParentFieldsItem;
    property ParentFieldsNode[Index: Integer]: TVRMLNode
      read GetParentFieldsNodeItem;
    function ParentFieldsCount: Integer;
    { @groupEnd }

    { Free this object (if it's not @nil) @italic(also removing
      it from @bold(all) parent nodes and fields).

      By design, normal destructor (Destroy called by Free)
      doesn't care about removing references to this object from
      it's parents. That's because it's the parents that usually
      initialize freeing of their children, and they free child
      when it's reference count is 0. So this freeing method
      is special in this regard.

      Use this if you really want to remove all node occurences from the middle
      of VRML hierarchy. }
    procedure FreeRemovingFromAllParents;

    { Free if this node is not referenced by any VRML 1.0 parent node
      or VRML 2.0 SFNode or MFNode fields. This is a safe way of removing
      a node that may, but doesn't have to, be part of VRML graph.
      The idea is that if node is a part of the graph,
      we will do nothing (assuming you have
      a reference to the entine graph somewhere), otherwise node is
      considered unused and freed.

      For safety, I advice to usually set reference to @nil after calling
      FreeIfUnused, like

@longCode(#
  Child.FreeIfUnused;
  Child := nil;
#)
       }
    procedure FreeIfUnused;

    { AllowedChildren okresla jakie dzieci moga byc dziecmi tego node'a.
      Warunek ten bedzie sprawdzany w AddChild wiec nigdy nie uda ci sie dodac
      node'a ktory nie jest tutaj dozwolony.

      ParsingAllowedChildren okresla jakie dzieci moga byc odczytane
      ze strumienia jako dzieci tego node'a. Chwilowo ma to zastosowanie
      tylko dla wezlow *Inline ktore w strumieniu nie moze miec zapisanych
      zadnych dzieci ale laduja swoje inline jako swoje Child.
      Wiec musza miec ParsingAllowedChildren=[] i AllowedChildren = All.

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

      Note that in some special cases AllowedChildren and
      ParsingAllowedChildren values may be changed during object lifetime.
      Currently, this may concern TVRMLUnknownNode. }
    property AllowedChildren: boolean read fAllowedChildren default false;
    property ParsingAllowedChildren: boolean
      read fParsingAllowedChildren default false;

    { Name of this node, as defined by VRML "DEF" construct.

      NodeName = '' oznacza ze obiekt nie mial zdefiniowanej nazwy.

      It's named NodeName, to not confuse this with TVRMLField.Name.
      (Even though TVRMLField and TVRMLNode classes have nothing in common.
      TSFNode descends from TVRMLField and @italic(contains) TVRMLNode
      instance in it's Value field. Once I wanted to just make
      TSFNode = TVRMLNode and TVRMLNode descendant of TVRMLField,
      but this wasn't a good idea: TSFNode may be NULL, but still
      it has a field name, so it should be nicely represented as
      TSFNode instance with Value = nil.)

      Note that this is writeable property, so you can change NodeName
      at runtime. Beware that some operations depend that node names
      don't change during their work: loading and saving nodes
      from stream (since these operations keep current collection names to
      read / write VRML DEF / USE statements), searching for nodes by name. }
    property NodeName: string read fNodeName write FNodeName;

    { WWWBasePath is the base URL path for all URLs
      in node's fields. This is used by all nodes that get some
      url (like Texture2 and WWWInline in VRML 1.0, ImageTexture
      and Inline in VRML 2.0 etc.).

      This way URL's in node's fields may contain relative names.
      If WWWBasePath doesn't begin with <proto>:// it is understood
      to be a file:// base path.

      TODO: chwilowo, poniewaz tylko odwolania do lokalnych plikow
      sa zaimplementowane, cale to bajanie o URL'ach to tylko mowa
      "jak kiedys bedzie". Chwilowo WWWBasePath musi byc lokalna sciezka
      (i to absolutna, bezwzgledna sciezka).

      WWWBasePath is set in constructor, and eventually adjusted by
      various parsing/converting routines (TVRMLNode.Parse, but also
      potentially other things from Object3dAsVRML).
      This way we could, if only we would like to, resolve nodes
      like Inline or ImageTexture immediately after parsing them. }
    property WWWBasePath: string read FWWWBasePath write FWWWBasePath;

    { This returns absolute path, assuming that RelativePath is relative
      path from WWWBasePath or that RelativePath is already absolute. }
    function PathFromWWWBasePath(const RelativePath: string): string;

    { Parse node. This should set values of your fields, VRML 1.0 Children
      list, WWWBasePath.

      In special cases like TVRMLUnknownNode this may
      actually initialize whole Fields list (by VRML 1.0 "fields" extensibility
      feature). }
    procedure Parse(Lexer: TVRMLLexer); virtual;

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
      ) }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); virtual;

    { CreateParse simply does Create and then calls Parse. }
    constructor CreateParse(const ANodeName: string; Lexer: TVRMLLexer);

    destructor Destroy; override;

    { NodeTypeName zwraca nazwe klasy w VRML'u. Zawsze jest <>''.
      To ma byc normalna nazwa node'a, taka ktora odczytujemy
      i zapisujemy bezposrednio z/do pliku VRMLa (wobec tego jest ona tez
      case-sensitive, jak caly VRML).

      Nie zmienia sie przez caly czas zycia obiektu, tzn. raz zainicjowana w
      konstruktorze juz taka pozostaje. Even for special nodes, like
      TVRMLUnknownNode and TVRMLPrototypeNode (where this is determined
      at runtime, since these special nodes are used to instantiate special
      nodes that are not built-in) --- but still even for these special
      nodes, NodeTypeName is constant for the life of this object.

      W tej klasie NodeTypeName zwraca ClassNodeTypeName. Uwagi do
      implementacji podklas TVRMLNode dotyczace tej funkcji - patrz
      ClassNodeTypeName. }
    function NodeTypeName: string; virtual;

    { ClassNodeTypeName zwraca nazwe klasy VRMLa zwiazanej z tym node'm lub
      '' w przypadku klas ktore nie maja zwiazanej ze soba 1-znacznej nazwy
      typu wezla VRMLa ktory reprezentuja (a poniewaz kazda klasa wezla VRMLa
      musi miec NodeTypeName <> '' wiec oznacza to ze te wyjatkowe klasy ustalaja
      sobie NodeTypeName w jakis inny, specjalny sposob - jedyny dostepny
      w tej chwili przyklad tego to TVRMLUnknownNode (chociaz nie wykluczam sobie
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
      staja sie klasami specjalnymi, jak TVRMLUnknownNode, ktore nie maja
      stalej wartosci NodeTypeName). }
    class function ClassNodeTypeName: string; virtual;

    { Traverse enumerates all nodes of VRML graph that are active,

      An "active" part of the VRML graph are the nodes that actually
      change what the VRML file represents, in terms of geometry,
      collision detection etc. For example, the Switch node has only
      one child usually active. Nodes that merely influence
      the active graph by some events and routes do not have to be
      active (these nodes may change what the VRML file actually represents,
      but only by changing other nodes).

      For all nodes of NodeClass TraversingFunc
      will be called.

      Traverse not only enumerates these
      nodes, it also collects all state (transformation, etc ---
      see TVRMLGraphTraverseState) that affects how given node should
      be presented.

      Also, TraversingInfo is passed to each TraversingFunc call.
      This allows you to investigate, during TraversingFunc call, the parents
      hierarchy (you can't use ParentNodes / ParentFields of the current node,
      since a node may have many parents). Traverse calls are
      naturally recursive, and so the stack of TraversingInfo
      structures is naturally build and destroyed by recursive calls.
      For the root node (the one where you called Traverse without
      specifying initial TraversingInfo), ParentInfo is simply @nil.

      The scheme of how Traverse works:

@preformatted(
  BeforeTraverse;
  if Self is NodeClass then TraversingFunc (Self, State)
  MiddleTraverse
  for all children returned by DirectEnumerateActive
    call their Traverse(State)
  AfterTraverse,
  dodaj Self do stanu State do LastNode (o ile Self wsrod
    TraverseStateLastNodesClasses)
)

      Jezeli zostalo wykonane BeforeTraverse, na pewno zostanie wykonane tez
      AfterTraverse (wywolanie AfterTraverse jest w finally..end).

      Kolejnosc w jakiej przechodzi graf jest naturalnie istotna.
      W czasie wykonywania Traverse mozesz modyfikowac tylko node'y dzieci
      (bezposrednie i niebezposrednie) node'a na ktorym wlasnie stoisz. }
    procedure Traverse(State: TVRMLGraphTraverseState;
      NodeClass: TVRMLNodeClass;
      TraversingFunc: TTraversingFunc;
      ParentInfo: PTraversingInfo = nil);

    { This is like @link(Traverse), but it automatically handles
      creating and destroying of TVRMLGraphTraverseState and it's LastNodes.

      This is comfortable --- but see comments at
      TVRMLGraphTraverseState.Create: if you want to save for later
      State instances obtained during traversing,
      than you shouldn't use this. }
    procedure TraverseFromDefaultState(
      NodeClass: TVRMLNodeClass; TraversingFunc: TTraversingFunc);

    { Enumerate all our children of some class. Recursively.
      Zwroci do proc() takze sam obiekt na ktorym EnumerateNodes zostalo
      wywolane, jezeli tylko ten obiekt jest klasy nodeClass.

      This enumerates both VRML 1.0 @link(Children) as well as
      nodes in TSFNode and TMFNode fields.
      If OnlyActive then it will enumerate only active parts
      of the graph ("active" as defined by @link(Traverse)),
      so it will work as a simpler version of Traverse
      (simpler, because it doesn't track any state).
      If not OnlyActive then it will simply enumerate all nodes.

      Wersja z argumentem SeekNodeName wymaga ponadto aby node mial NodeName=
      SeekNodeName (gdy SeekNodeName = '' to znajduje nienazwane node'y,
      wiec wartosc '' nie jest tu traktowana specjalnie).

      Zaczyna przegladac dzieci dopiero jak przegladnie Self. Jezeli np.
      w proc. zmodyfikowales (np. dodales) wlasne Children to EnumerateNodes
      will enumerate these new children. To ma znaczenie np. w
      TVRMLScene.LoadAllInlined gdzie w proc robimy LoadInlined. Poniewaz
      EnumerateNodes przeglada dzieci po wywolaniu proc., wiadomo ze
      przegladnie tez nowo zaladowane dziecko.

      BTW modyfikowanie dzieci node'a ktory wlasnie dostales do proc()
      to jedyna dozwolona modyfikacja na hierarchii VRMLa ktora mozesz
      wykonywac w czasie EnumerateNodes.

      @groupBegin }
    procedure EnumerateNodes(
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TVRMLNodeClass;
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TVRMLNodeClass;
      const SeekNodeName: string;
      proc: TVRMLNodeProc; OnlyActive: boolean); overload;
    { @groupEnd }

    { TryFindNodeByName and TryFindNode seek for a node with
      given class (and node name, in case of TryFindNodeByName).
      If OnlyActive then they seek among only active nodes
      ("active" as defined by @link(Traverse)), otherwise all nodes.

      These functions are quite like EnumerateNodes, except
      they stop at the first occurence and return it.

      TryFindNodeByName and TryFindNode return @nil if such node
      is not found. FindNodeByName and FindNode raise exception
      in this case.

      @groupBegin }
    function TryFindNodeByName(FindClass: TVRMLNodeClass;
      const FindName: string;
      OnlyActive: boolean): TVRMLNode;
    function FindNodeByName(FindClass: TVRMLNodeClass;
      const FindName: string;
      OnlyActive: boolean): TVRMLNode;
    function TryFindNode(FindClass: TVRMLNodeClass;
      OnlyActive: boolean): TVRMLNode;
    function FindNode(FindClass: TVRMLNodeClass;
      OnlyActive: boolean): TVRMLNode;
    { @groupEnd }

    { Znajdz pierwszy Node (zadanej klasy NodeClass) razem ze State
      (lub tylko z Transform).
      Dziala jak Traverse ktore zatrzymuje sie po pierwszej udanej probie.
      Pamietaj ze State nie jest pamietane nigdzie indziej i musisz je zwolnic.
      W przypadku TryFindNodeTransform nie musisz o tym pamietac,
      no i TryFindNodeTransform dziala nieco szybciej.

      Zwraca false and sets Node, State and Transform to undefined
      (because they are "out" params) if not found.

      @groupBegin }
    function TryFindNodeState(
      InitialState: TVRMLGraphTraverseState;
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out State: TVRMLGraphTraverseState): boolean;
    function TryFindNodeTransform(
      InitialState: TVRMLGraphTraverseState;
      NodeClass: TVRMLNodeClass;
      out Node: TVRMLNode;
      out Transform: TMatrix4Single;
      out AverageScaleTransform: Single): boolean;
    { @groupEnd }

    { This seeks Self and parent nodes (from ParentNodes and ParentFields,
      recursively), for given node name.

      In other words, this is similar to TryNodeByName or NodeByName,
      but it goes "upward" in graph hierarchy. Note that this
      never restricts itself only to "active" graph part
      ("active" as defined by @link(Traverse))
      because you really can't detect what is the "active"
      part of the graph when going upward.

      @groupBegin }
    function TryFindParentByName(const FindName: string): TVRMLNode;
    function FindParentByName(const FindName: string): TVRMLNode;
    { @groupEnd }

    { Przeszukuje podobnie jak powyzsze FindParentByName. Zwraca true
      jesli znalazl tam gdzies node Node. }
    function HasParent(Node: TVRMLNode): boolean;

    { Searches immediate parents of this node for a node with given FindName.
      Returns @nil if not found. }
    function TryFindDirectParentByName(const FindName: string): TVRMLNode;

    { sprawdza czy istnieje w grafie VRML'a zaczepionym w danym punkcie
      node Node.

      If OnlyActive, then only active parts are searched
      ("active" as defined by @link(Traverse)). }
    function IsNodePresent(Node: TVRMLNode; OnlyActive: boolean): boolean;

    { policz ile jest node'ow danej klasy.
      Uzywajac np. TVRMLLightNode mozesz
      sprawdzic czy na scenie zostalo zdefiniowane jakiekolwiek swiato.

      If CountOnlyActiveNodes, then only active parts are searched
      ("active" as defined by @link(Traverse)).

      This traverses both VRML 1.0 children nodes and VRML 2.0 nodes
      inside SFNode and MFNode fields. }
    function NodesCount(NodeClass: TVRMLNodeClass;
      CountOnlyActiveNodes: boolean): integer;

    (*Save node to stream. This saves everything, including node name,
      node type, then node contents within { }.

      We use SaveProperties.NodeNameBinding, pretty much like when parsing.
      If a node name is already bound with this node, then we know
      we have to write only USE ... statement. Otherwise we write
      full node contents, with eventual DEF ... statement.

      Note that if ChildrenSaveToStream returns @false
      we don't write our Children. Currently this is used by various inline
      nodes (WWWInline, Inline, etc.).
    *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

    { szuka tej klasy node'a (rzeczywistej koncowej klasy, z ClassType) w
      TraverseStateLastNodesClasses. Zwraca indeks lub -1 jesli nie znalazl. }
    class function TraverseStateLastNodesIndex: Integer;

    { Some of the nodes are meant to be handled only for specific
      VRML versions. This functions says whether this node is supposed
      to be present in given VRML version. VerMajor and VerMinor
      arguments are expected in the same form as TVRMLLexer.VRMLVerMajor,
      TVRMLLexer.VRMLVerMinor.

      For example some nodes can only work in VRML < 2.0,
      some others only in VRML >= 2.0. There are even some pairs
      of nodes: for example TNodeCone_1 works with VRML < 2.0,
      TNodeCone_2 works with VRML >= 2.0.

      NodesManager will use this.

      Default implementation of this function returns always @true.
      Generally, I don't try to set this too aggresively ---
      in other words, for all cases when it's sensible, I allow
      nodes to be used in every VRML version, even when official
      specification doesn't. This means that when reading VRML 1.0
      files actually a large part of VRML 2.0 is allowed too,
      and also while reading VRML 2.0 many constructs from VRML 1.0
      (officially no longer present in VRML 2.0) are allowed too.
      I'm trying to support what I call a "sum of VRML 1.0 and 2.0".

      In practice I only use this function when both VRML 1.0 and 2.0
      specify the same node name but

      @unorderedList(
        @item(With different fields.

          For example Cone and Cylinder have slightly different fields,
          due to the fact that VRML 2.0 resigned from using TSFBitMask fields.)

        @item(With different behavior.

          For example definitions of Sphere for VRML 1.0
          and 2.0 are practically equal. However, the behavior from where
          to take texture and material info is different --- in VRML 1.0
          we take last Texture2, Material etc. nodes, while in VRML 2.0
          we look in parent Shape's "appearance" field. So once again
          two different Sphere classes are needed.)
      ) }
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      virtual;

    { MFNode field of this node that should be treated as general
      "children" field of this node. This is used in some places,
      like SmartAddChild.

      Should return nil if this node doesn't have such
      field (that's the default implementation in this class).
      This should always return the same value for given class instance
      (in other words, don't implement this to sometimes return one field,
      sometimes the other, sometimes nil, etc.). }
    function ChildrenField: TMFNode; virtual;

    { These operate on children nodes, in either VRML 2.0 style
      (if ChildrenField is non-nil, then these get/set ChildrenField.Items)
      or in VRML 1.0 style (if ChildrenField is nil, then these get/set
      our Children).

      This is useful to operate on grouping nodes both in VRML 1.0 and VRML 2.0
      style using the same code. }
    procedure SmartAddChild(Node: TVRMLNode);
    property SmartChildren[Index: Integer]: TVRMLNode read GetSmartChildren;
    function SmartChildrenCount: integer;
    function SmartExtractChild(Index: Integer): TVRMLNode;

    { SuggestedVRMLVersion determines what VRML header to use
      when saving the node to file. Returns @true and sets out arguments
      if some version is preferred, otherwise returns @false.

      SuggestionPriority should be used to indicate the "strongness"
      of this suggestion. The idea is that if there are two nodes
      that have different VRML version suggestions, then the one
      with greater SuggestionPriority "wins".

      Currently used priorities:

      @unorderedList(
        @item(1000 for nodes that are only in VRML <= 1.0,
          or only in VRML >= 2.0. This applies to majority of nodes.)

        @item(100 for nodes that are only for VRML >= 2.0 according to specifications,
          but in our engine they were often used also in VRML 1.0 files.
          That is possible thanks to our implementation, see
          [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_mix_vrml_1_2].
          This concerns Fog, Background and a few other nodes.

          By using weaker priority for such nodes, VRML 1.0 files with
          "bits" of VRML 2.0 will be retained as VRML 1.0. (They are readable
          only by our engine anyway.) On the other hand, only Background
          node will be correctly saved as VRML 2.0 file.)

        @item(2000 for nodes that are only in X3D, that is VRML >= 3.0.

          X3D nodes suggest version 3.2 (not 3.0, because some features (shaders)
          are only in ammendment 1 I think, so 3.1 is safer and for simplicity
          (since I looked mostly at 3.2) 3.2 is even better).

          They use priority 2000, so they are slightly stronger than
          VRML 97 nodes. This way a model with mixed VRML 97 and X3D nodes
          will be judged as X3D. And that's Ok, because almost everything
          (incompatible exceptions are some NURBS and geo changes)
          valid in VRML 97 is also valid in X3D.)

        @item(10 * 1000 is used by TVRMLRootNode_1 and TVRMLRootNode_2
          ForceVersion mechanism. This way we can always force VRML version
          using this node, which is useful to write files with the same
          VRML version as was read from disk.)
      )

      Default implementation in this class enumerates all
      SFNode and MFNoden fields and Children nodes
      and determines their suggested VRML version. }
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; virtual;

    { Returns should SaveToStream save our Children.
      In this class default implementation returns @true,
      this is what you will want in 99% of cases.
      It's useful to set this to false if you use
      Children internally, e.g. *Inline nodes. }
    class function ChildrenSaveToStream: boolean; virtual;

    { Enumerates all children nodes (recursively),
      allowing you to decide for each node to replace or remove it.

      So this is something like EnumerateNodes,
      except that it allows you to remove the nodes. It always
      enumerates all nodes, not only active (e.g. it enumerates all
      Switch node children, not only the chosen one).

      Note that (unlike regular EnumerateNodes) @bold(this doesn't
      report Self to Func !). Which is natural, since this may remove
      nodes by normal RemoveChild calls, so it needs to know ParentNode
      of the removed node.

      For each node Func will be called, with ParentNode and Node set.
      If you change the Node to something else, then the old node will
      be removed and new Node inserted in the same place.
      If new Node is @nil, then only the old node will be removed.

      Nodes are traversed in depth-first search. Node is first reported
      to Func, and then (if it's not replaced) we descend into this Node.

      @returns The number of removed nodes. }
    function EnumerateReplaceChildren(
      Func: TEnumerateReplaceNodesFunction): Cardinal;

    { Removes all children (and their children, recursively) with
      node names matchig Wildcard. You can use * and ? special chars
      in the Wildcard.
      @returns The number of removed nodes. }
    function RemoveChildrenWithMatchingName(
      const Wildcard: string; IgnoreCase: Boolean): Cardinal;

    property Prototypes: TVRMLPrototypeBasesList read FPrototypes;
    property Routes: TVRMLRoutesList read FRoutes;

    { Add Self (NodeName must be initialized) to nodes namespace.
      Doesn't do anything if NodeName = ''. }
    procedure Bind(NodeNameBinding: TStringList);

    { PrototypeInstance = @true indicates that this node was created
      from a non-external prototype instantiation.

      Then PrototypeInstanceSourceNode is non-nil and indicates
      parsed prototype node (and PrototypeInstanceSourceNode.Prototype
      gives you even a link to the actual prototype specification).

      PrototypeInstanceHelpers may be @nil if empty, or may contain
      a list of other nodes duplicated along with the main prototype node.
      From VRML spec:

      @preformatted(
        Any other nodes and accompanying scene graphs
        are not part of the transformation hierarchy, but may be referenced
        by ROUTE statements or Script nodes in the prototype definition.)

      TODO: memory leaks are known to be possible in some difficult cases
      with PrototypeInstanceHelpers. See e.g.
      ../../../kambi_vrml_test_suite/vrml_2/warnings/errors/proto_leak.wrl and
      ../../../kambi_vrml_test_suite/vrml_2/warnings/errors/proto_leak_2.wrl
      for simple testcases. Reason: PrototypeInstanceHelpers may contain,
      by DEF statements, links to Self.
      This causes circular dependency (Self is child of some node on
      PrototypeInstanceHelpers, but PrototypeInstanceHelpers will
      be freed only if Self is freed) causing some memory to be left
      always allocated.

      @groupBegin }
    property PrototypeInstance: boolean read FPrototypeInstance;
    property PrototypeInstanceSourceNode: TVRMLPrototypeNode
      read FPrototypeInstanceSourceNode;
    property PrototypeInstanceHelpers: TVRMLNodesList;
    { @groupEnd }

    { Should we use this node when URN is required by EXTERNPROTO ?

      Implementors note: in this class, this returns @false.
      You can use constants like URNVRML97Nodes and URNKambiNodes to help
      implementing this. }
    class function URNMatching(const URN: string): boolean; virtual;

    { Traverses all Blender objects/meshes instances in this model,
      assuming that this VRML node was created by Blender VRML 1.0 or 2.0
      exporter.

      For each Blender object (which means, for each Blender mesh instantiation),
      this calls TraversingFunc. Since each Blender object is unique in file,
      you can be sure that each BlenderObjectNode will be enumerated only
      once by TraversingFunc, @italic(as long as this file was really
      made by Blender exporter). As for BlenderObjectName, Blender VRML 1.0
      exporter doesn't write object names (only meshes), so it's always ''
      for VRML 1.0.

      Mesh may occur many times in the file, and both Blender exporters
      correctly use VRML DEF/USE mechanism, so the same BlenderMeshNode
      and BlenderMeshName may be enumerated many times by TraversingFunc.

      Implementation of this follows the logic of Blender VRML 1.0 and 2.0
      standard exporters, there's no other way to implement this.
      If you wrote in Python your own Blender exporter for VRML,
      this method may obviously not work. But it's guaranteed that this
      method will not crash or anything on any VRML model. The worst thing
      that can happen on all VRML models is simply that TraversingFunc will
      enumerate something that doesn't correspond to any Blender object...

      Overloaded version without State just creates default state internally.

      @groupBegin }
    procedure TraverseBlenderObjects(
      State: TVRMLGraphTraverseState;
      TraversingFunc: TBlenderTraversingFunc); overload;

    procedure TraverseBlenderObjects(
      TraversingFunc: TBlenderTraversingFunc); overload;
    { @groupEnd }

    { Default value of "containerField" attribute for this node in X3D XML
      encoding. }
    property DefaultContainerField: string
      read FDefaultContainerField write FDefaultContainerField;

    { Value of "containerField" attribute specified explicitly
      for this node in X3D XML encoding. This is practically usable
      (read/write) only by X3D XML reader. }
    property ExplicitContainerField: string
      read FExplicitContainerField write FExplicitContainerField;

    { For some special VRML / X3D nodes (like Script, ComposedShader)
      that allow the definition of additional fields/events within.
      If HasInterfaceDeclarations is not [], then InterfaceDeclarations
      will be non-nil and parser (classic VRML parser in this unit,
      X3D XML reader too) will read this from VRML files.

      In X3D specification this is marked like

@preformatted(
  # And any number of:
  fieldType [in]     fieldName
  fieldType [in,out] fieldName    initialValue
  fieldType [out]    fieldName
  fieldType []       fieldName    initialValue
)

      @groupBegin }
    property HasInterfaceDeclarations: TVRMLAccessTypes
      read FHasInterfaceDeclarations
      write SetHasInterfaceDeclarations default [];

    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;
    { @groupEnd }

    { Does this node allow CDATA section when encoded in XML.
      See X3D XML encoding specification about
      "Encapsulating Script node code", instantreality also uses
      CDATA to encode shader source code within XML file and this
      seems sensible (following the intention of the spec?).

      This is only used to produce eventual warnings when CDATA is
      encountered. Whether or not CDataAllowed is @true, we will parse
      CDATA anyway into @link(CData) value.

      This should be set in descendants constructor. }
    property CDataAllowed: boolean read FCDataAllowed write FCDataAllowed;

    { CDATA section when this node is encoded in XML.
      See X3D XML encoding specification.
      When CDataExists = @false, CData is always empty.

      @groupBegin }
    property CDataExists: boolean read FCDataExists write FCDataExists;
    property CData: string read FCData write FCData;
    { @groupEnd }

    { Functions registered here will be called when this TVRMLNode descendant
      will be destroyed. }
    property DestructionNotifications: TDynNodeDestructionNotificationArray
      read FDestructionNotifications;
  end;

  TObjectsListItem_3 = TVRMLNode;
  {$I objectslist_3.inc}
  TVRMLNodesList = class(TObjectsList_3)
    function FindNodeName(const Name: string): Integer;
  end;

  TVRMLNodeClassesList = class(TList)
  private
    function GetItems(Index: Integer): TVRMLNodeClass;
    procedure SetItems(Index: Integer; Value: TVRMLNodeClass);
  public
    property Items[Index: Integer]: TVRMLNodeClass
      read GetItems write SetItems; default;
    procedure AssignArray(
      const AItemsArray: array of TVRMLNodeClass);
    function IndexOf(NodeClass: TVRMLNodeClass): Integer; overload;
    { This is equivalent to IndexOf(NodeClass.ClassType),
      taking care of necessary typecasts. }
    function IndexOf(Node: TVRMLNode): Integer; overload;

    { This looks for a node class that is ancestor of given Node,
      in other words that satisfies @code(Node is Items[Result]).
      So, comparing to IndexOf, this may also find something when
      IndexOf doesn't, since this doesn't require an exact
      match --- only "is" match.

      Returns -1 if not found. }
    function IndexOfAnyAncestor(Node: TVRMLNode): Integer;

    procedure Add(Value: TVRMLNodeClass);

    { Add all node classes registered in NodesManager that implement given
      interface Interf. }
    procedure AddRegisteredImplementing(Interf: TGUID);
  end;

  { SFNode VRML field.
    It's defined in this unit, not in VRMLFields, since it uses
    TVRMLNode definition. NULL value of the field is indicated by
    Value field = nil.

    Note that we store AllowedChildren list, which is a list of
    classes allowed as a Value (also nil is always allowed).
    But this is used only to produce warnings for a user.
    You should never assert that Value actually is one the requested
    classes. We want to keep here even not allowed items,
    because we want operation "read from VRML file + write to VRML file"
    to be as non-destructible as possible. So if user wrote
    invalid class hierarchy, we will output this invalid class hierarchy. }
  TSFNode = class(TVRMLSingleField)
  private
    FValue: TVRMLNode;
    FParentNode: TVRMLNode;
    FAllowedChildrenAll: boolean;
    FAllowedChildren: TVRMLNodeClassesList;
    procedure SetValue(AValue: TVRMLNode);

    FDefaultValue: TVRMLNode;
    FDefaultValueExists: boolean;
    procedure SetDefaultValue(ADefaultValue: TVRMLNode);
    procedure SetDefaultValueExists(AValue: boolean);
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor CreateUndefined(AParentNode: TVRMLFileItem;
      const AName: string); override;
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      const AnAllowedChildren: array of TVRMLNodeClass;
      AValue: TVRMLNode = nil); overload;
    { Constructor that takes AnAllowedChildren as TVRMNodeClassesList.
      Note that we copy the contents of AnAllowedChildren, not the
      reference. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AnAllowedChildren: TVRMLNodeClassesList;
      AValue: TVRMLNode = nil); overload;
    { Constructor that initializes AllowedChildren to all
      classes implementing AllowedChildrenInterface. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AllowedChildrenInterface: TGUID;
      AValue: TVRMLNode = nil); overload;
    destructor Destroy; override;

    { This says that all children are allowed, regardless of
      AllowedChildren value.

      CreateUndefined creates always object with
      AllowedChildrenAll = @true (otherwise AllowedChildren list is empty
      and nothing would be allowed; but for e.g. fields in instantiated
      prototypes, we must initially just allow all, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing).

      Other constructors set AllowedChildren
      to something meaningful and set this to @false. You can change this
      to @true then if you consciously want to turn this check off. }
    property AllowedChildrenAll: boolean
      read FAllowedChildrenAll write FAllowedChildrenAll;

    { DefaultValue of SFNode field.

      While X3D specification says for all SFNode fields that their
      default value is NULL, this is not necessarily true for PROTO
      SFNode fiels. So we have to take into account that any DefaultValue
      is possible.

      Note that this doesn't have to be @nil, but will be irrelevant
      if not DefaultValueExists. (Once I had an idea to automatically
      set DefaultValue to @nil when DefaultValueExists is set to @false,
      but this was uncomfortable (like "what to do when DefaultValue
      is assigned non-nil when DefaultValueExists is false?").)

      Freeing of this is automatically managed, just like the normal
      @link(Value) property. This means that you can simply set
      DefaultValue to @nil or some existing node, and eventual memory
      deallocation of previous DefaultValue node (if unused) will happen
      automatically. }
    property DefaultValue: TVRMLNode
      read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write SetDefaultValueExists default false;

    property Value: TVRMLNode read FValue write SetValue;
    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;
    procedure AssignDefaultValueFromValue; override;

    { VRML node containing this field. May be @nil if unknown, in special
      cases.

      Note that this property is exactly the same as
      TVRMLFieldOrEvent.ParentNode,
      contains always the same value. But this is declared as TVRMLNode,
      so it's more comfortable. }
    property ParentNode: TVRMLNode read FParentNode;

    class function VRMLTypeName: string; override;

    { Checks is Child allowed as a value of thia SFNode,
      and makes VRMLNonFatalError if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      VRMLNonFatalError message will suggest that this Child is used as value
      of this node. In other words, you should only pass as Child
      a node that you want to assign as Value to this field,
      otherwise VRMLNonFatalError message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TVRMLNode);

    function ChildAllowed(Child: TVRMLNode): boolean;
    function CurrentChildAllowed: boolean;

    { Calls Func for our @link(Value), assuming it's set (non-nil) and valid
      (allowed by ChildAllowed).

      The main use for this is to simplify implementation of
      @link(TVRMLNode.DirectEnumerateActive) overrides in TVRMLNode descendants. }
    procedure EnumerateValid(Func: TEnumerateChildrenFunction);
  end;

  { MFNode VRML field.

    Just like SFNode, it's defined in this unit, as it uses TVRMLNode.
    Note that items of MFNode @italic(cannot) be nil (i.e. VRML doesn't
    allow to use NULL inside MFNode), contrary to SFNode.

    Note that TMFNode implementation doesn't use TVRMLSimpleMultField.
    Reasons ? 1. We don't want to use TDynArray descendant.
    We want to use TVRMLNodesList. 2. We don't want to do parsing
    using SFNode, because MFNode doesn't allow NULL items.

    Just like for TSFNode:
    Note that we store AllowedChildren list, which is a list of
    classes allowed as Items.
    But this is used only to produce warnings for a user.
    You should never assert that every item actually is one the requested
    classes.  }
  TMFNode = class(TVRMLMultField)
  private
    FItems: TVRMLNodesList;
    FDefaultItems: TVRMLNodesList;
    FDefaultValueExists: boolean;
    FParentNode: TVRMLNode;
    FAllowedChildren: TVRMLNodeClassesList;
    FAllowedChildrenAll: boolean;
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor CreateUndefined(AParentNode: TVRMLFileItem;
      const AName: string); override;
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      const AnAllowedChildren: array of TVRMLNodeClass); overload;
    { Constructor that takes AnAllowedChildren as TVRMNodeClassesList.
      Note that we copy the contents of AnAllowedChildren, not the
      reference. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AnAllowedChildren: TVRMLNodeClassesList); overload;
    { Constructor that initializes AllowedChildren to all
      classes implementing AllowedChildrenInterface. }
    constructor Create(AParentNode: TVRMLNode; const AName: string;
      AllowedChildrenInterface: TGUID); overload;
    destructor Destroy; override;

    { This says that all children are allowed, regardless of
      AllowedChildren value.

      CreateUndefined creates always object with
      AllowedChildrenAll = @true (otherwise AllowedChildren list is empty
      and nothing would be allowed; but for e.g. fields in instantiated
      prototypes, we must initially just allow all, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing).

      Other constructors set AllowedChildren
      to something meaningful and set this to @false. You can change this
      to @true then if you consciously want to turn this check off. }
    property AllowedChildrenAll: boolean
      read FAllowedChildrenAll write FAllowedChildrenAll;

    { Lists items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like AddItem (they take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields). }
    property Items: TVRMLNodesList read FItems;

    procedure AddItem(Node: TVRMLNode); overload;
    procedure AddItem(Position: Integer; Node: TVRMLNode); overload;

    procedure RemoveItem(Index: Integer);
    { Remove child with given Index, and return it, @italic(never freeing it).
      This is analogous to TVRMLNode.ExtractChild, see there for more
      explanation. }
    function ExtractItem(Index: Integer): TVRMLNode;
    procedure ClearItems;
    procedure AssignItems(SourceItems: TVRMLNodesList);
    procedure ReplaceItem(Index: Integer; Node: TVRMLNode);

    { Just a shortcut for Items.Count }
    function Count: integer; override;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Double): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;
    procedure AssignDefaultValueFromValue; override;

    property ParentNode: TVRMLNode read FParentNode;

    class function VRMLTypeName: string; override;

    { Checks is Child allowed on the list of nodes of this MFNode,
      and makes VRMLNonFatalError if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      VRMLNonFatalError message will suggest that this Child is used as value
      of this node. In other words, you should only pass as Child
      a node that you want to add (e.g. by AddItem) to this field,
      otherwise VRMLNonFatalError message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TVRMLNode);

    function ChildAllowed(Child: TVRMLNode): boolean;

    { Lists default items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like AssignDefaultItems (they take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields). }
    property DefaultItems: TVRMLNodesList read FDefaultItems;

    { Operate on DefaultItems, just like analogous AssignItems and
      ClearItems.
      @groupBegin }
    procedure AssignDefaultItems(SourceItems: TVRMLNodesList);
    procedure ClearDefaultItems;
    { @groupEnd }

    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists default false;

    { Calls Func for all current children that are valid
      (allowed by ChildAllowed).

      The main use for this is to simplify implementation of
      @link(TVRMLNode.DirectEnumerateActive) overrides in TVRMLNode descendants. }
    procedure EnumerateValid(Func: TEnumerateChildrenFunction);
  end;

{ Specific VRML nodes from specifications, part 1 -------------------------- }

{$I x3d_core.inc}

{ TVRMLGeometryNode ---------------------------------------------------- }

  TCoordRangeHandler = procedure (const RangeNumber: Cardinal;
    BeginIndex, EndIndex: Integer) of object;

  TIndexedPolygonHandler = procedure (const Indexes: array of Cardinal)
    of object;

  ENotCoordinateBasedNode = class(EVRMLError);

  { Geometry nodes are the only nodes that produces some visible results
    during rendering. Much of the VRML language is just
    a method of describing properties how geometry nodes are displayed
    (materials, transformations, lighting).

    A few things that make geometry node special :
    @unorderedList(
      @item(Only geometry nodes may have [Local]BoundingBox.)
      @item(
        Only geometry nodes define something visible "in usual way"
        during rendering (Some other nodes in VRML / X3D are visible but in an
        unusual way, like Background and Fog. These nodes must be rendered in
        a special way --- they are not affected in any usual way by the current
        transformation matrix etc.))

      @item(
        Only geometry nodes can add triangles to the scene, so the Triangulate
        method can be defined only for geometry nodes.)

      @item(
        Geometry nodes are never "grouping nodes", in particular there's
        never a geometry node that is (direct or indirect) child of another
        geometry node. So there's no need to be concerned whether geometry nodes'
        children are included in things like [Local]BoundingBox or
        Triangles/VerticesCount.)

      @item(
        Geometry nodes don't affect anything in graph traverse state.
        (This is important mostly for VRML 1.0, since in newer VRML / X3D
        "graph traverse state" is not that important.))
    )

    For X3D, this descends from TNodeX3DNode, and TNodeX3DGeometryNode
    descends from us. This way in X3D TNodeX3DGeometryNode descends
    from this, and also X3D hierarchy is preserved (X3DGeometryNode
    must descend from X3DNode). }
  TVRMLGeometryNode = class(TNodeX3DNode)
  public
    { Constructor.

      Only sets DefaultContainerField to 'geometry', since this is valid
      for all X3D nodes descending from TVRMLGeometryNode. }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Calculate bounding box of this geometry node.
      They require State of this node during VRML traverse state --- this
      is mainly for VRML 1.0 nodes, that depend on such state.

      LocalBoundingBox gives a bounding box ignoring current transformation
      (or, equivalently, assuming like Transform = IdentityMatrix).
      Normal BoundingBox gives a bounding box taking current transformation
      into account.

      @italic(Notes for descendants implementors:)

      The default implementations of these methods in TVRMLGeometryNode
      try to be smart and cover all common bases, so that you have to do
      as little work as possible to implement working descendant.

      @orderedList(
        @item(
          For nodes based on coordinates (when @link(Coord) returns @true),
          LocalBoundingBox and BoundingBox
          already have optimal and correct implementation in this class.
          Using Coord and CoordIndex, no other information is needed.)

        @item(
          For other nodes, we check @link(Proxy) result. If it returns
          non-nil, we will use it to calculate bounding boxes,
          local and not local.

          So for nodes with @link(Proxy) overridden, you don't have
          to implement bounding box calculation, instead a @link(Proxy)
          will be used. This will work Ok if @link(Proxy) node will
          have bounding box calculation implemented (this includes
          even the case when the proxy itself will delegate the work to
          another @link(Proxy)).

          You can always override these methods, if you don't want
          to use proxy (for example, maybe there exists much faster
          method to calculate bounding box, or maybe tighter
          bounding box may be calculated directly).)

        @item(
          For other nodes (not coordinate-based and without a proxy):

          The default implementation of LocalBoundingBox just calls
          BoundingBox with a specially modified State, such that
          Transform is identity.

          The default implementation of BoundingBox, in turn, just calls
          LocalBoundingBox and transforms this bounding box.

          So the default implementations call each other, and will loop
          infinitely... But if you override any one of them
          (local or not local), the other one will magically work.

          Note that the default implementation of LocalBoundingBox
          may be non-optimal as far as time is concerned,
          as we'll do useless multiplications by identity matrix.
          And the default implementation of BoundingBox may generate
          non-optimal bounding box, more direct approach (transforming
          each vertex) may give much tightier bounding box.

          So you only have to override one method --- although if you
          want the best implementation, fastest and with the best tight
          bounding boxes, you may need to override both of them for some nodes.)
      )

      @groupBegin }
    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; virtual;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; virtual;
    { @groupEnd }

    { Calculate vertex and triangles count of this node.

      They require State of this node during VRML traverse state --- this
      is mainly for VRML 1.0 nodes, that depend on such state.
      OverTriangulate has the same meaning as for Triangulate.

      Vertices count calculates number of different vertexes in this node.
      That is, it doesn't eliminate doubles in cases like Coordinate node
      with multiple points the same. But if some face is known to use
      twice the same vertex index, then this counts like a single vertex.
      The idea is that this indicates rendering speed.

      For triangles count, the returned value may be different then
      actual if some faces were non-convex. Things like TriangulateFace
      may remove degenerate triangles, so actual number of triangles may
      be slightly less. So don't depend on TrianglesCount as a precise
      measure --- but it's a good fast measure of complexity of given
      node, how fast it will be rendered, used with collision detection etc.

      @italic(Notes for descendants implementors:)

      For coordinate-based nodes (when @link(Coord) returns @true),
      VerticesCount is already implemented in this class.
      Using Coord method, no other information is needed.

      For other nodes, the default implementation of
      both VerticesCount and TrianglesCount in this TVRMLGeometryNode
      class will use @link(Proxy) to do the work.
      You should override these methods if Proxy is not available,
      or some faster approach is possible.

      @groupBegin }
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; virtual;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; virtual;
    { @groupEnd }

    { Triangulate node, calling NewTriangleProc for each triangle this node
      defines. NewTriangleProc will be called with (Triangle, State, Node) where
      Triangle will be the new triangle, State will always be the
      State given here and Node will always be Self.

      The difference between LocalTriangulate and Triangulate is just like
      between LocalBoundingBox and BoundingBox: "local" version ignored
      transformation (stored in State.Transform), while Triangulate not.

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
      na duzo trojkatow.

      @italic(Notes for descendants implementors:)

      Default implementation of LocalTriangulate in this class uses
      @link(Proxy), if it's non-nil. You have to override this if
      @link(Proxy) is not available, or if you want to plug some more optimal
      method.

      Triangulate is always implemented by simply calling LocalTriangulate
      and transforming each triangle.

      @groupBegin }
    procedure Triangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      NewTriangleProc: TNewTriangleProc);
    procedure LocalTriangulate(State: TVRMLGraphTraverseState;
      OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); virtual;
    { @groupEnd }

    { Return node's list of coordinates. Returns @false if node is
      not based on coordinates. Returns @true and sets ACoord
      if the node is based on coordinates. Even when returns @true,
      it can set ACoord = @nil, which means that node is based on
      coordinates but they are empty right now (so for example
      bounding box may be considered empty).

      In base TVRMLGeometryNode class this always returns @false.

      Override this for descendants that have some kind of "coord" field,
      then this should return @true and set ACoord to coord.point field,
      assuming that coord is set and specifies Coordinate node.
      Otherwise should return @true and set ACoord = @nil.

      For VRML 1.0, coord may be taken from State, that's why we have to
      pass current traverse state here. }
    function Coord(State: TVRMLGraphTraverseState;
      out ACoord: TMFVec3f): boolean; virtual;

    { Return node's list of coordinates, raising exception if node
      is not based on coordinates.

      This is just like the @link(Coord) method,
      except it simply returns the coordinates, not the boolean result.
      If virtual @link(Coord) returns @false (indicating the node
      is not coordinate-based) this raises ENotCoordinateBasedNode.

      @raises(ENotCoordinateBasedNode If node is not coordinate-based,
        that is @link(Coord) returns false.)
    }
    function Coordinates(State: TVRMLGraphTraverseState): TMFVec3f;

    { Node's list of coordinate indexes.

      In base TVRMLGeometryNode class this always returns @nil.

      Override this for descendants that have some kind of "coordIndex"
      or "index" field used to index @link(Coord) array. }
    function CoordIndex: TMFLong; virtual;

    { Returns an information how to split @link(Coord) array into ranges.

      When CoordIndex = @nil, then if the node's @link(Coord) array
      can be divided into some "ranges", we will use this information.
      This is used (and should be overridden) for X3D non-indexed nodes,
      like fanCount or stripCount or vertexCount.

      What precisely is a "range of coordinates" is not specified
      here. It may be a line stip, or one triangle strip, etc. ---
      depending on the descendant.

      Returns @true is this is available. In this case, RangeCount must
      be set to something <> nil, and the rest of returned variables
      are mainly to generate proper warnings by MakeCoordRanges. }
    function CoordRangesCounts(out RangeCount: TDynLongIntArray;
      out SRanges, SRangeName: string;
      out RangeMinimumCount: Cardinal): boolean; virtual;

    { Splits @link(Coord) array into ranges.

      If CoordIndex is assigned, then a "range of coordinates" is
      just a range of non-negative indexes within CoordIndex.
      Otherwise (when CoordIndex = @nil), CoordRangesCounts must
      return @true and we will use RangeCount to split coordinates.

      Call this only for nodes with coordinates, that is only when
      @link(Coord) returns @true. }
    procedure MakeCoordRanges(
      State: TVRMLGraphTraverseState;
      CoordRangeHandler: TCoordRangeHandler);

    { Splits coordinate-based node into polygons.
      The idea is that this can be usable for both NormalsCalculator
      (used by the renderer) and implementation of LocalTriangulate
      (and so Triangulate) methods.

      Indexes in PolygonHandler point to CoordIndex, if assigned,
      or directly to Coord. The ordering of generated polygons is correct,
      so what pointed CCW in the node field, will still point CCW
      according to generated PolygonHandler indexes.

      In this class this does nothing. Some, but not all, coordinate-based
      nodes (the ones when @link(Coord) returns @true) override this.
      So currently, whether this is implemented is coordinated with
      NormalsCalculator and LocalTriangulate internal needs. }
    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); virtual;

    { Converts this node to another node class that may be better supported.

      Typically, converts some complex geometry node (like
      Extrusion or Teapot) into more common node like IndexedFaceSet
      or IndexedTriangleSet. Then many methods may try to use
      this converted node by default, so it will act like a "proxy"
      instead of actual node. In all cases, it should always
      be possible to write an optimized method specially for given node
      (i.e., not using Proxy), but the Proxy way may act as
      a working and easy fallback.

      In the base TVRMLGeometryNode class, returns @nil indicating
      that no conversion it known. }
    function Proxy: TVRMLGeometryNode; virtual;
  end;

{ IVRMLInlineNode --------------------------------------------------------- }

  { Basic interface that should be implemented by all Inline VRML nodes. }
  IVRMLInlineNode = interface(IVRMLNode)
  ['{70E19208-A2EF-4883-BD04-E7EFF7932F3A}']
    { Call LoadInlined to load inlined VRML content @bold(now).
      If Inlined is already loaded,
      than: if CanReload = @true Inlined will be freed and loaded again,
      else (if CanReload = @false) nothing will happen.

      LoadInlined(false) will be called automatically in BeforeTraverse. }
    procedure LoadInlined(CanReload: boolean);
  end;

{ Specific VRML nodes from specifications, part 2 -------------------------- }

{$define GeometryNotImplemented :=
  function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
  function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
}

{$I x3d_time.inc}
{$I x3d_grouping.inc}
{$I x3d_networking.inc}
{$I x3d_rendering.inc}
{$I x3d_shape.inc}
{$I x3d_geometry3d.inc}
{$I x3d_geometry2d.inc}
{$I x3d_text.inc}
{$I x3d_sound.inc}
{$I x3d_lighting.inc}
{$I x3d_texturing.inc}
{$I x3d_interpolation.inc}
{$I x3d_pointingdevicesensor.inc}
{$I x3d_keydevicesensor.inc}
{$I x3d_environmentalsensor.inc}
{$I x3d_navigation.inc}
{$I x3d_environmentaleffects.inc}
{$I x3d_geospatial.inc}
{$I x3d_h-anim.inc}
{$I x3d_nurbs.inc}
{$I x3d_dis.inc}
{$I x3d_scripting.inc}
{$I x3d_eventutilities.inc}
{$I x3d_shaders.inc}
{$I x3d_cadgeometry.inc}
{$I x3d_texturing3d.inc}
{$I x3d_cubemaptexturing.inc}
{$I x3d_layering.inc}
{$I x3d_layout.inc}
{$I x3d_rigidbodyphysics.inc}
{$I x3d_picking.inc}
{$I x3d_followers.inc}
{$I x3d_particlesystems.inc}

{$I vrml1nodes.inc}
{$I vrmlinventornodes.inc}
{$I vrml97nodes.inc}
{$I vrmlkambinodes.inc}
{$I vrml_avalon_nodes.inc}

{ TVRMLUnknownNode --------------------------------------------------- }

  (* @abstract(TVRMLUnknownNode represents a node with an unrecognized type.)

    If we approach some node with not recognized name we create TVRMLUnknownNode.
    TVRMLUnknownNode has very special Parse method.
    We want to use "fields" and "isA" VRML 1.0 extensibility features here.
    (TODO - these extensibility features are not implemented yet;
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
    lub CreateUnknown. This way we can safely assume that NodeTypeName
    is always correctly set.

    Spostrzezenie : ten mechanizm jest calkiem dobry - node'y kazdego
    typu, nawet 1, moga byc nazywane i mozna sie pozniej do nich
    odwolywac przez USE. Jezeli node jest typu 2 i 3 to nawet
    ich SubNode'y beda wlaczone w ten standardowy mechanizm !
    Po Parse node'u unknown typu 1) robimy VRMLNonFatalError
    (bo dokladnie to zaszlo --- to jest nieprawidlowy node, ale umiemy sobie
    poradzic).
  *)
  TVRMLUnknownNode = class(TVRMLNode)
  private
    fNodeTypeName: string;
  public
    function NodeTypeName: string; override;
    procedure Parse(Lexer: TVRMLLexer); override;

    { base Create will throw exception. Always use CreateUnknown* }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    constructor CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
    constructor CreateUnknownParse(const ANodeName, ANodeTypeName :string;
      Lexer: TVRMLLexer);
  end;

{ TVRMLInterfaceDeclaration -------------------------------------------------- }

  { Interface declaration, used in VRML (exposed) prototypes and scripts.
    See VRML 2.0 spec.

    Each interface specification is a field or an event, stored
    in FieldOrEvent. FieldOrEvent is @nil before parsing.

    Field value is not initialized if you passed FieldValue = @false
    to @link(Parse) (although IsClause and IsClauseName will
    always be initialized). FieldValue = @true is used for prototype
    (not external) declarations and scripts.
    In the future maybe some property like
    FieldValueInitialized will be exposed here, if needed at some point.

    Interface declaration doesn't have much properties, since all
    the information is contained within FieldOrEvent
    instance, like Name, field class type, out or in (in case of event),
    exposed or not (in case of field), IsClause and IsClauseName. }
  TVRMLInterfaceDeclaration = class(TVRMLFileItem)
  private
    FFieldOrEvent: TVRMLFieldOrEvent;

    { kept in synch with FFieldOrEvent by SetFieldOrEvent }
    FField: TVRMLField;
    FEvent: TVRMLEvent;

    procedure SetFieldOrEvent(const Value: TVRMLFieldOrEvent);

    FParentNode: TVRMLNode;
  public
    constructor Create(AParentNode: TVRMLNode);
    destructor Destroy; override;

    { Containing node, if any, for this VRML interface declaration.
      This must also be set to FieldOrEvent.ParentNode created for this
      interface declaration. }
    property ParentNode: TVRMLNode read FParentNode;

    { Field or event of this interface declaration.
      Is non-nil after parsing.

      You can assign to this property, to constructs interface
      declarations (and so also prototypes) in your own code
      (e.g. this is used X3D XML reader). Just be careful, and remember
      that this object owns FieldOrEvent (that is, will free it
      at destruction). }
    property FieldOrEvent: TVRMLFieldOrEvent
      read FFieldOrEvent write SetFieldOrEvent;

    { Return FieldOrEvent casted as appropriate class.
      @nil if such cast is not possible, for example when
      FieldOrEvent is an event and you try to use Field method.
      @groupBegin }
    property Field: TVRMLField read FField;
    property Event: TVRMLEvent read FEvent;
    { @groupEnd }

    procedure Parse(Lexer: TVRMLLexer;
      FieldValue, IsClauseAllowed: boolean); virtual;

    { Save this interface declaration to stream.
      This assumes that it starts at the beginning of the line,
      and at the end always writes NL, so at the end it's also
      at the beginning of some line.

      @param(FieldValue If @true then we will always save
        Field value or IS clause to stream, along with this interface
        decl (if this interface declaration has the Field set).
        Otherwise, field's value will not be saved, only IS clause
        if present.)
    }
    procedure IDeclSaveToStream(SaveProperties: TVRMLSaveToStreamProperties;
      FieldValue: boolean);

    { Save this interface declaration to stream.

      Saves with field value, just by calling
      IDeclSaveToStream(SaveProperties, true).

      @seealso IDeclSaveToStream }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
      override;

    { Returns access type, corresponding to current @link(Event)
      and @link(Field) values.

      Result is undefined if both Event
      and Field are @nil (which may happen when it's not initialized
      (e.g. parsed) yet) or when both are non-nil (which should never
      happen). }
    function AccessType: TVRMLAccessType;
  end;

  TObjectsListItem_2 = TVRMLInterfaceDeclaration;
  {$I objectslist_2.inc}
  TVRMLInterfaceDeclarationsList = class(TObjectsList_2)
  public
    { Find field or event with given Name.
      @nil if not found. }
    function TryFindName(const Name: string): TVRMLFieldOrEvent;

    { Find field with given Name.
      @nil if not found. }
    function TryFindFieldName(const Name: string): TVRMLField;

    { Find event with given Name.
      @nil if not found. }
    function TryFindEventName(const Name: string): TVRMLEvent;
  end;

{ TVRMLPrototype ------------------------------------------------------------- }

  TVRMLPrototypeBase = class;

  EVRMLPrototypeInstantiateError = class(Exception);

  { VRML node related to a given prototype.

    This node will have fields
    initialized according to associated Prototype.InterfaceDeclarations.
    This way you can simply parse this node (just like any other node)
    to parse prototype instance.

    The prototype may be instantiated. After parsing you can
    do it by @link(Instantiate) method. In case of non-external prototype,
    this should always be possible (for a valid VRML files, that is),
    in case of external prototype this may requite loading the external
    prototype file.

    This node cannot be created by standard Create method,
    always use CreatePrototypeNode. }
  TVRMLPrototypeNode = class(TVRMLNode)
  private
    FPrototype: TVRMLPrototypeBase;

    { This searches Node for fields with "IS" clauses, and expands them
      using our current fields values (i.e. changes IsClause to @false
      and fills appropriate value). It also descends into all children.

      Aborts if current node is from another prototype (PrototypeInstance is @true).
      We can do it, since "IS" always refers to innermost prototype.
      This is very useful, because it means that prototype can always be
      instantiated right after it's defined. Actually, this is the only sensible
      solution: if "IS" would refer to some non-innermost proto, then the proto
      couldn't be instantiated within it's direct parent --- and proto
      must *always be instantiated within it's direct parent*, not higher,
      so says the spec. }
    procedure InstantiateReplaceIsClauses(Node, Child: TVRMLNode);

    { Basically, do Destination.AssignValue(Source).
      If Source.IsClause, then copies Source.IsClauseName to
      Destination.IsClauseName.
      In case of EVRMLFieldAssign, make VRMLNonFatalError with clear message. }
    procedure FieldsAssignValue(Destination, Source: TVRMLField;
      UsingIsClause: boolean);
  public
    { This constructor will raise exception for TVRMLPrototypeNode.
      Always use CreatePrototypeNode for this node class. }
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    constructor CreatePrototypeNode(const ANodeName, AWWWBasePath :string;
      APrototype: TVRMLPrototypeBase);

    function NodeTypeName: string; override;

    property Prototype: TVRMLPrototypeBase read FPrototype;

    { Instantiate the prototype, that is create new VRML node
      (of "normal" classs, not TVRMLPrototypeNode) using prototype description.

      For non-external prototype, in essense it just takes Prototype.Node
      and returns it's copy. For external prototype it first loads external file,
      and then uses non-external prototype there. Eventually,
      for external prototype we may also use build-in node (if URN will
      indicate so).

      Actually, the process is a little more involved (see below for
      details), but the idea is that returned node can be simply inserted
      into VRML hierarchy and works just like a normal node.
      The important feature is that returned instance class is the same
      that was specified as a first prototype node. For example, if the
      prototype should expand to Material node, then this returns
      TNodeMaterial. Just like Material node would be normally specified,
      not created by some prototype.

      Note that this TVRMLPrototypeNode becomes "owned" by returned
      node instance, in PrototypeInstanceSourceNode.
      (that's needed for returned node's SaveToStream to work correctly).

      Details:
      @unorderedList(
        @item(
          Prototype.Node may be just a wrapper, i.e. TVRMLRootNode_1
          or TVRMLRootNode_2.

          In this case the first children of Prototype.Node is used
          to create instance. The rest of the wrapper (with this first children
          removed, to not cause cycles) is also duplicated and set
          as new node's PrototypeInstanceHelpers.)

        @item(
          Returned Node (with all it's helpers in PrototypeInstanceHelpers)
          has "IS" clauses everywhere filled, according to our field values.)

        @item(NodeName of returned node is copied from our NodeName.)

        @item(
          For SaveToStream to work, returned Node has PrototypeInstance = @true,
          and PrototypeInstanceSourceNode set to Self. This allows SaveToStream
          to correctly save using PrototypeInstanceSourceNode, instead
          of writing actual node contents.))

      @raises(EVRMLPrototypeInstantiateError if for some reason
        the prototype cannot be instantiated.
        You can catch this and replace with VRMLNonFatalError, if possible.)
    }
    function Instantiate: TVRMLNode;
  end;

  TVRMLPrototypeBase = class(TVRMLFileItem)
  private
    FName: string;
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;

    { Parses InterfaceDeclarations. Also inits WWWBasePath from
      Lexer.WWWBasePath, by the way. }
    procedure ParseInterfaceDeclarations(ExternalProto: boolean;
      Lexer: TVRMLLexer);

    { Saves Name, and interface declarations enclosed
      within [ ]. In descendant, you should first write the keyword PROTO
      or EXTERNPROTO, then call this, then write the rest of your prototype. }
    procedure SaveInterfaceDeclarationsToStream(
      SaveProperties: TVRMLSaveToStreamProperties;
      ExternalProto: boolean);

    FWWWBasePath: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;

    { Parse prototype, and add it to Lexer.ProtoNameBinding by @link(Bind). }
    procedure Parse(Lexer: TVRMLLexer); virtual; abstract;

    { Add Self (at least Name must be initialized) to prototypes namespace. }
    procedure Bind(ProtoNameBinding: TStringList);

    { The base URL path used to resolve urls inside.
      For now, used by EXTERNPROTO urls.
      See TVRMLNode.WWWBasePath for more comments. }
    property WWWBasePath: string read FWWWBasePath write FWWWBasePath;
  end;

  TObjectsListItem_4 = TVRMLPrototypeBase;
  {$I objectslist_4.inc}
  TVRMLPrototypeBasesList = class(TObjectsList_4);

  TVRMLPrototype = class(TVRMLPrototypeBase)
  private
    FNode: TVRMLNode;
  public
    destructor Destroy; override;

    procedure Parse(Lexer: TVRMLLexer); override;
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

    { These are actual prototype contents: all nodes, prototypes, routes
      defined within this prototype.

      We wrap this all inside a single
      VRML node, just like we do when reading whole VRML file:
      if the whole thing is exactly one VRML node, then this is this node.
      Otherwise, we wrap inside artificial TVRMLRootNode_1 or
      TVRMLRootNode_2.

      You have permission to write to this property, to be able to
      write code that constructs prototypes (like needed by X3D XML reader).
      Just be careful. This node is owned by prototype instance (will be
      freed at destructor). }
    property Node: TVRMLNode read FNode write FNode;
  end;

  TVRMLExternalPrototype = class(TVRMLPrototypeBase)
  private
    FURLList: TMFString;

    { FReferencedPrototype has links to other parts of the VRML graph.
      Not only FReferencedPrototype.Node, but also
      FReferencedPrototype.InterfaceDeclaration may have links to it:
      if referenced node has SFNode or MFNode fields and their default
      values have "USE ..." clauses.
      So it's best to keep whole ReferencedPrototypeNode (whole VRML file
      that contained this prototype) loaded. }
    ReferencedPrototypeNode: TVRMLNode;

    FReferencedPrototype: TVRMLPrototype;

    FReferencedClass: TVRMLNodeClass;
  public
    constructor Create;
    destructor Destroy; override;
    property URLList: TMFString read FURLList;

    procedure Parse(Lexer: TVRMLLexer); override;
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

    property ReferencedPrototype: TVRMLPrototype read FReferencedPrototype;
    property ReferencedClass: TVRMLNodeClass read FReferencedClass;

    { Loads URL, until the first success. Sets either ReferencedClass to non-nil
      (if it's built-in node) or ReferencedPrototype (if prototype expansion
      found in external file). }
    procedure LoadReferenced;
    procedure UnloadReferenced;
  end;

{ TVRMLRoute ----------------------------------------------------------------- }

  TVRMLRoute = class(TVRMLFileItem)
  private
    FSourceNode: TVRMLNode;
    FSourceExposedField: TVRMLField;
    FSourceEvent: TVRMLEvent;

    FDestinationNode: TVRMLNode;
    FDestinationExposedField: TVRMLField;
    FDestinationEvent: TVRMLEvent;

    procedure DestructionNotification(Node: TVRMLNode);

    procedure UnsetEnding(
      var Node: TVRMLNode; var ExposedField: TVRMLField; var Event: TVRMLEvent;
      const DestEnding: boolean;
      RemoveFromDestructionNotification: boolean = true);

    procedure SetEnding(const NodeName, FieldOrEventName: string;
      NodeNameBinding: TStringList;
      var Node: TVRMLNode; var ExposedField: TVRMLField; var Event: TVRMLEvent;
      const DestEnding: boolean);

    LastEventTime: TKamTime;

    procedure EventReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TKamTime);
  public
    constructor Create;
    destructor Destroy; override;

    { Source event properties. Either all three are @nil, or:

      @unorderedList(
        @item(SourceEvent is assigned, meaning is self-explanatory.)

        @item(SourceNode must also be assigned and
          this must be the node enclosing SourceEvent. That is, the node that
          has SourceEvent as one of explicit (on TVRMLNode.Events list) or
          implicit (exposed by some field) event.)

        @item(If this event is exposed by some field, then SourceExposedField
          is also assigned. Otherwise SourceExposedField is @nil.)
      )

      @groupBegin }
    property SourceNode: TVRMLNode read FSourceNode;
    property SourceExposedField: TVRMLField read FSourceExposedField;
    property SourceEvent: TVRMLEvent read FSourceEvent;
    { @groupEnd }

    { Destination event properties.
      Analogous to SourceEvent, SourceExposedField, SourceNode.

      @groupBegin }
    property DestinationNode: TVRMLNode read FDestinationNode;
    property DestinationExposedField: TVRMLField read FDestinationExposedField;
    property DestinationEvent: TVRMLEvent read FDestinationEvent;
    { @groupEnd }

    procedure SetSource(
      const SourceNodeName, SourceFieldOrEventName: string;
      NodeNameBinding: TStringList);

    procedure SetDestination(
      const DestinationNodeName, DestinationFieldOrEventName: string;
      NodeNameBinding: TStringList);

    { Parses the route statement.
      Implementation should be able to safely assume that current token
      is ROUTE. }
    procedure Parse(Lexer: TVRMLLexer);

    { Save a ROUTE to VRML file.

      Will generate VRMLNonFatalError when route cannot be saved.
      This can happen when SourceNode or SourceEvent
      or DestinationNode or DestinationEvent are @nil.
      Also, if SourceNode and DestinationNode are without a name,
      or the name is not currently bound in SaveProperties.NodeNameBinding.
    }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

    { Clear the memory when the last event passed through this route.
      Route must remember such thing, to avoid loops in routes.
      This is following VRML 2.0 / X3D specifications, that explicitly
      say that only one event per ROUTE per timestamp is allowed.

      Use ResetLastEventTime when you really want to reset this memory.
      In practice, this should be used only by TVRMLFlatScene.ResetWorldTime
      implementation. }
    procedure ResetLastEventTime;
  end;

  TObjectsListItem_5 = TVRMLRoute;
  {$I objectslist_5.inc}
  TVRMLRoutesList = class(TObjectsList_5);

{ TraverseStateLastNodesClasses ---------------------------------------------- }

const
  { opis patrz TTraverseStateLastNodes }
  TraverseStateLastNodesClasses :
    array[0..HighTraverseStateLastNodes] of TVRMLNodeClass =
    ( TNodeCoordinate3, TNodeShapeHints, TNodeFontStyle_1,
      TNodeMaterial_1, TNodeMaterialBinding, TNodeNormal, TNodeNormalBinding,
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
    { Strings[] is ClassNodeTypeName. Objects[] is the actual class
      (typecast to TVRMLNodeClass is safe). }
    FRegistered: TStringList;
    function GetRegistered(Index: Integer): TVRMLNodeClass;
  public
    constructor Create;
    destructor Destroy; override;

    { Mozesz rejestrowac tylko klasy o ClassNodeTypeName <> '' (w tej procedurze
      to sprawdzimy i ew. rzucimy wyjatek ENodeClassRegisterError).

      Nie mozesz zarejestrowac dwa razy tej samej klasy,
      spowoduje to ENodeClassRegisterError.

      Natomiast mozesz zarejestrowac wiele razy rozne klasy o tym samym
      ClassNodeTypeName. For example TNodeCone_1 and TNodeCone_2.
      They will be chosen in NodeTypeNameToClass using their ForVRMLVersion. }
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
      na klase VRMLa ktora ma takie samo ClassNodeTypeName.

      Aby takie cos
      przeprowadzic potrzebny byl gdzies ekwiwalent globalnej tablicy
      przechowujacej wszystkie stworzone klasy wezlow VRMLa --- takim
      odpowiednikiem jest wlasnie ta klasa do ktorej trzeba rejestrowac wezly.
      Bedzie szukac wsrod zarejestrowanych klas klasy o zadanym
      ClassNodeTypeName, wybierajac tylko klase ktorej
      @code(ForVRMLVersion(VerMajor, VerMinor)) will return @true.

      Jesli nie znajdzie zwroci nil. }
    function NodeTypeNameToClass(const ANodeTypeName: string;
      const VerMajor, VerMinor: Integer): TVRMLNodeClass;

    { Return class that matches given URL. This is useful for EXTERNROTOs.
      Returns @nil if not found. }
    function URNToClass(const URN: string): TVRMLNodeClass;

    { Enumerate all registered classes, from Registered[0] to
      Registered[RegisteredCount - 1].

      @groupBegin }
    property Registered [Index: Integer]: TVRMLNodeClass read GetRegistered;
    function RegisteredCount: Cardinal;
    { @groupEnd }
  end;

var
  { tworzony i niszczony w init/fini tego modulu }
  NodesManager: TNodesManager;

{ global procedures ---------------------------------------------------------- }

(*
  Parse VRML node. This parses
  @preformatted([ DEF <nodename> ] <nodetype> { node-content })
  or
  @preformatted(USE <nodename>)

  If we will find USE clause but node name will be unknown, the normal
  behavior (when NilIfUnresolvedUSE = @false, default) is to raise
  EVRMLParserError (just like in case of many other errors).
  However, this is a particular parsing error, because we can probably
  pretty safely continue parsing, ignoring this error.
  So if you pass NilIfUnresolvedUSE = @true, this function will do
  VRMLNonFatalError and simply return @nil.

  @raises(EVRMLParserError On various parsing errors.)
*)
function ParseNode(Lexer: TVRMLLexer;
  NilIfUnresolvedUSE: boolean = false): TVRMLNode;

{ Parse whole VRML file, return it's root node.

  Note that you must pass here TPeekCharStream class, not just any
  generic TStream class. But it's not a problem, really, because
  you can wrap any class inside TPeekCharStream descendant. E.g. do
  @longcode(#
    ParseVRMLFile(TBufferedReadStream.Create(MyStream, false), WWWBasePath)
  #)

  Note that this function can't handle compressed data (VRML files are
  sometimes compressed with gzip). You should already pass here a stream
  with uncompressed text data.

  @param(ProtoNameBinding If <> @nil, will be filled with global
    prototype namespace at the end of parsing the file. Usually not useful.)

  @raises(EVRMLGzipCompressed If the Stream starts with gzip file header.) }
function ParseVRMLFile(Stream: TPeekCharStream;
  const WWWBasePath: string;
  ProtoNameBinding: TStringList = nil): TVRMLNode; overload;

function ParseVRMLFileFromString(const VRMLContents: string;
  const WWWBasePath: string): TVRMLNode; overload;

{ FileName to nazwa istniejacego pliku (wzgledna lub bezwzgledna).
  Jezeli AllowStdIn to jesli filename = '-' to odczytamy model z StdInStream,
  w tym przypadku WWWBasePath bedzie ustawione na GetCurrentDir.

  This function can handle files compressed with gzip
  (it just internally filters file contents with TGZFileStream,
  uncompressing it on the fly).

  @param(ProtoNameBinding If <> @nil, will be filled with global
    prototype namespace at the end of parsing the file. Usually not useful.) }
function ParseVRMLFile(const FileName: string;
  AllowStdIn: boolean;
  ProtoNameBinding: TStringList = nil): TVRMLNode; overload;

{ SaveToVRMLFile writes whole VRML file with given root Node.
  This includes writing VRML header '#VRML ...'.

  @param(PrecedingComment
    If PrecedingComment <> '' then we will write a comment
    '# '+ PrecedingComment at the beginning. This is for you
    to optionally put name of the generator program or source filename
    or time/date of generation etc.)

  @param(WriteExpandedPrototype
    See TVRMLSaveToStreamProperties.WriteExpandedPrototype. In pretty much
    all normal cases (i.e. when you save your VRML node hierarchy into a file
    for user) you can leave this as @false.)
  }
procedure SaveToVRMLFile(Node: TVRMLNode;
  Stream: TStream; const PrecedingComment: string;
  WriteExpandedPrototype: boolean = false); overload;
procedure SaveToVRMLFile(Node: TVRMLNode;
  const Filename, PrecedingComment: string;
  WriteExpandedPrototype: boolean = false); overload;

const
  { File filters for TGLWindow.FileDialog if you want to open a file and then
    pass it to ParseVRMLFile. Note that only ParseVRMLFile that takes a FileName
    parameter (as a string) can deal with gzip compressed files. }
  ParseVRMLFile_FileFilters =
  'All files|*|' +
  '*VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz';

  { File filters for TGLWindow.FileDialog if you want to save a file using
    SaveToVRMLFile. }
  SaveToVRMLFile_FileFilters =
  'All files|*|' +
  '*VRML (not compressed) (*.wrl)|*.wrl';

{ Create and assign all State.Nodes. }
procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);

{ Free and nil all State.Nodes. }
procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);

{ Create a new node with exactly the same VRML node hierarchy.
  Everything in the hierarchy is a new node instance, although the same
  node classess are present, with the same names, field values etc. }
function VRMLNodeDeepCopy(SourceNode: TVRMLNode): TVRMLNode;

{ Free all VRML nodes with no parents on the list, then free and @nil the list
  itself. }
procedure VRMLNodesList_FreeWithNonParentedContentsAndNil(var List: TVRMLNodesList);

const
  VRMLCameraKindToStr: array[TVRMLCameraKind]of string =
  ('Orthographic', 'Perspective');

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

  DefaultHeightMapScale = 0.01;
  DefaultVRML1CreaseAngle = 0.5;

  DefaultViewpointFieldOfView = Pi / 4;

{ TODO: these Detail parameters below should depend on object's distance
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

    For now, you can change these variables only @italic(before using anything)
    from this module.

    These variables @italic(must) always honour Min values listed below. }
  Detail_QuadricSlices: Cardinal = 30;
  Detail_QuadricStacks: Cardinal = 20;
  Detail_RectDivisions: Cardinal = 2;

const
  { uzywaj w programie zawsze tych stalych zamiast zakladac ze maja one
    konkretne wartosci, ale mozesz oczywiscie przyjac zalozenie ze na pewno
    sa one Cardinalami (sa >=0) }
  MinQuadricSlices: Cardinal = 3; { mimo ze OpenGL akceptuje minimum 2, ale dla 2 wynik jest bez sensu }
  MinQuadricStacks: Cardinal = 1;
  MinRectDivisions: Cardinal = 0;

const
  { URNs used to indicate standard VRML / X3D nodes.

    Funny thing, I actually didn't found anywhere a definite official
    statement that they are using such-and-such URNs.

    X3D specification refers to RFC
    [http://www.ietf.org/rfc/rfc3541.txt?number=3541] which, basically,
    just says "we like URNs and we'll use them" and nothing more.
    Same thing for VRML 97 spec
    [http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/extensions.html].
    There is no precise answer e.g. what URN should be used to
    Indicate some standard VRML 97 / X3D node.

    I constructed URNs below looking at examples in the RFC,
    annotated by a funny line "The following examples are not
    guaranteed to be real. They are presented for pedagogical reasons only."

    @groupBegin }
  URNVRML97Nodes = 'urn:web3d:vrml97:node:';
  URNX3DNodes = 'urn:web3d:x3d:node:';
  { @groupEnd }

  { URN used to indicate VRML / X3D nodes that are Kambi VRML game engine
    extensions. }
  URNKambiNodes = 'urn:vrmlengine.sourceforge.net:node:';

const
  AllAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput];
  RestrictedAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly];

type
  { List to keep node names while parsing VRML file.
    This assumes that all strings are node names and their Objects[]
    are TVRMLNode instances with given names.

    The only advantage of using this is that it registers and unregisters
    itself for AnyNodeDestructionNotifications, so if any node may be
    destroyed during parsing, it will also be removed from this list. }
  TNodeNameBinding = class(TStringListCaseSens)
  private
    procedure DestructionNotification(Node: TVRMLNode);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  { Functions registered here will be called when any TVRMLNode descendant
    will be destroyed. }
  AnyNodeDestructionNotifications: TDynNodeDestructionNotificationArray;

{$undef read_interface}

implementation

uses
  { Fonts for Text, FontStyle, AsciiText nodes }
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

  Math, Triangulator, Object3dAsVRML, KambiZStream, VRMLCameraUtils,
  KambiStringUtils, KambiFilesUtils, RaysWindow, StrUtils, KambiURLUtils,
  VRMLGeometry, KambiLog;

{$define read_implementation}

{$define GeometryNotImplemented :=
  function TGeometryNotImplemented.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
  begin
    Result := EmptyBox3d;
  end;

  function TGeometryNotImplemented.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
  begin
    Result := 0;
  end;

  function TGeometryNotImplemented.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
  begin
    Result := 0;
  end;

  procedure TGeometryNotImplemented.LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
  begin
  end;
}

{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}
{$I objectslist_4.inc}
{$I objectslist_5.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}

{$I vrmlnodes_boundingboxes.inc}
{$I vrmlnodes_verticesandtrianglescounting.inc}
{$I vrmlnodes_triangulating.inc}
{$I vrmlnodes_coordpolygons.inc}

{$I x3d_core.inc}
{$I x3d_time.inc}
{$I x3d_grouping.inc}
{$I x3d_networking.inc}
{$I x3d_rendering.inc}
{$I x3d_shape.inc}
{$I x3d_geometry3d.inc}
{$I x3d_geometry2d.inc}
{$I x3d_text.inc}
{$I x3d_sound.inc}
{$I x3d_lighting.inc}
{$I x3d_texturing.inc}
{$I x3d_interpolation.inc}
{$I x3d_pointingdevicesensor.inc}
{$I x3d_keydevicesensor.inc}
{$I x3d_environmentalsensor.inc}
{$I x3d_navigation.inc}
{$I x3d_environmentaleffects.inc}
{$I x3d_geospatial.inc}
{$I x3d_h-anim.inc}
{$I x3d_nurbs.inc}
{$I x3d_dis.inc}
{$I x3d_scripting.inc}
{$I x3d_eventutilities.inc}
{$I x3d_shaders.inc}
{$I x3d_cadgeometry.inc}
{$I x3d_texturing3d.inc}
{$I x3d_cubemaptexturing.inc}
{$I x3d_layering.inc}
{$I x3d_layout.inc}
{$I x3d_rigidbodyphysics.inc}
{$I x3d_picking.inc}
{$I x3d_followers.inc}
{$I x3d_particlesystems.inc}

{$I vrml1nodes.inc}
{$I vrmlinventornodes.inc}
{$I vrml97nodes.inc}
{$I vrmlkambinodes.inc}
{$I vrml_avalon_nodes.inc}

resourcestring
  SExpectedInterfaceDeclaration =
    'Expected interface declaration (for VRML 2.0: eventIn, eventOut, field or ' +
    'exposedField keyword; for X3D: inputOnly, outputOnly, initializeOnly or ' +
    'inputOutput keyword) but found %s';
  SExpectedFieldType =
    'Expected field type name (like SFVec2f etc.) but found %s';

function InterfaceDeclarationKeywords(
  const AccessTypes: TVRMLAccessTypes): TVRMLKeywords;
begin
  Result := [];
  if atInputOnly in AccessTypes then
    Result += [vkEventIn, vkInputOnly];
  if atOutputOnly in AccessTypes then
    Result += [vkEventOut, vkOutputOnly];
  if atInitializeOnly in AccessTypes then
    Result += [vkField, vkInitializeOnly];
  if atInputOutput in AccessTypes then
    Result += [vkExposedField, vkInputOutput];
end;

{ TVRMLNodesList ------------------------------------------------------------- }

function TVRMLNodesList.FindNodeName(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].NodeName = Name then
      Exit;
  Result := -1;
end;

procedure VRMLNodesList_FreeWithNonParentedContentsAndNil(var List: TVRMLNodesList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      if List.Items[I].ParentNodesCount + List.Items[I].ParentFieldsCount = 0 then
        List.Items[I].Free;
    FreeAndNil(List);
  end;
end;

{ TDynActiveLightArray --------------------------------------------------------- }

function TDynActiveLightArray.IndexOfLightNode(LightNode: TVRMLLightNode): integer;
begin
 for result := 0 to High do
  if Items[result].LightNode = LightNode then exit;
 result := -1;
end;

function TDynActiveLightArray.Equals(SecondValue: TDynActiveLightArray): boolean;

  function ActiveLightEquals(const L1, L2: TActiveLight): boolean;
  begin
    Result := (L1.LightNode = L2.LightNode) and
      MatricesPerfectlyEqual(L1.Transform, L2.Transform);

    { No need to compare TransfLocation or TransfNormDirection,
      as they are just precalculated based on LightNode and Transform. }
  end;

var
  I: Integer;
begin
  Result := SecondValue.Count = Count;
  if Result then
    for I := 0 to High do
      if not ActiveLightEquals(Items[I], SecondValue.Items[I]) then
        Exit(false);
end;

{ TVRMLGraphTraverseState ---------------------------------------------------- }

procedure TVRMLGraphTraverseState.CommonCreate;
begin
  inherited Create;
  VRML1ActiveLights := TDynActiveLightArray.Create;
  VRML2ActiveLights := TDynActiveLightArray.Create;
end;

constructor TVRMLGraphTraverseState.CreateCopy(Source: TVRMLGraphTraverseState);
begin
  CommonCreate;

  Transform := Source.Transform;
  AverageScaleTransform := Source.AverageScaleTransform;
  InvertedTransform := Source.InvertedTransform;

  TextureTransform := Source.TextureTransform;
  FLastNodes := Source.FLastNodes;
  OwnsLastNodes := false;
  ParentShape := Source.ParentShape;

  VRML1ActiveLights.AppendDynArray(Source.VRML1ActiveLights);
  VRML2ActiveLights.AppendDynArray(Source.VRML2ActiveLights);
end;

constructor TVRMLGraphTraverseState.Create(const ADefaultLastNodes: TTraverseStateLastNodes);
begin
  CommonCreate;

  Transform := IdentityMatrix4Single;
  AverageScaleTransform := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  FLastNodes := ADefaultLastNodes;
  OwnsLastNodes := false;
end;

constructor TVRMLGraphTraverseState.Create;
begin
  CommonCreate;

  Transform := IdentityMatrix4Single;
  AverageScaleTransform := 1.0;
  InvertedTransform := IdentityMatrix4Single;

  TextureTransform := IdentityMatrix4Single;
  TraverseState_CreateNodes(FLastNodes);
  OwnsLastNodes := true;
end;

destructor TVRMLGraphTraverseState.Destroy;
begin
  if OwnsLastNodes then
    TraverseState_FreeAndNilNodes(FLastNodes);

  FreeAndNil(VRML1ActiveLights);
  FreeAndNil(VRML2ActiveLights);

  inherited;
end;

function TVRMLGraphTraverseState.Equals(SecondValue: TVRMLGraphTraverseState):
  boolean;
var
  I: Integer;
begin
  Result :=
    VRML1ActiveLights.Equals(SecondValue.VRML1ActiveLights) and
    VRML2ActiveLights.Equals(SecondValue.VRML2ActiveLights) and
    { no need to compare InvertedTransform, it should be equal when normal
      Transform is equal. }
    MatricesPerfectlyEqual(Transform, SecondValue.Transform) and
    (AverageScaleTransform = SecondValue.AverageScaleTransform) and
    MatricesPerfectlyEqual(TextureTransform, SecondValue.TextureTransform) and
    (ParentShape = SecondValue.ParentShape);

  if Result then
  begin
    for I := 0 to HighTraverseStateLastNodes do
      if SecondValue.LastNodes.Nodes[I] <> LastNodes.Nodes[I] then
        Exit(false);
  end;
end;

function TVRMLGraphTraverseState.EqualsNoTransform(
  SecondValue: TVRMLGraphTraverseState): boolean;
var
  I: Integer;
begin
  { ActiveLights, Transform, AverageScaleTransform, InvertedTransform,
    TextureTransform are ignored by
    TVRMLOpenGLRenderer.RenderShapeStateNoTransform }

  Result := (ParentShape = SecondValue.ParentShape);

  if Result then
  begin
    for I := 0 to HighTraverseStateLastNodes do
      if SecondValue.LastNodes.Nodes[I] <> LastNodes.Nodes[I] then
        Exit(false);
  end;
end;

function TVRMLGraphTraverseState.Texture: TVRMLTextureNode;
begin
  if ParentShape = nil then
    Result := LastNodes.Texture2 else
    Result := ParentShape.Texture;
end;

function TVRMLGraphTraverseState.BlendMode: TNodeBlendMode;
var
  Node: TVRMLNode;
begin
  Result := nil;
  if ParentShape <> nil then
  begin
    Node := ParentShape.FdAppearance.Value;
    if (Node <> nil) and (Node is TNodeKambiAppearance) then
    begin
      Node := TNodeKambiAppearance(Node).FdBlendMode.Value;
      if (Node <> nil) and (Node is TNodeBlendMode) then
        Result := TNodeBlendMode(Node);
    end;
  end;
end;

function TVRMLGraphTraverseState.CurrentActiveLights: TDynActiveLightArray;
begin
  if ParentShape = nil then
    Result := VRML1ActiveLights else
    Result := VRML2ActiveLights;
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
  FParentNodes := TVRMLNodesList.Create;
  FParentFields := TVRMLFieldsList.Create;
  FFields := TVRMLFieldsList.Create;

  FEvents := TVRMLEventsList.Create;

  FPrototypes := TVRMLPrototypeBasesList.Create;
  FRoutes := TVRMLRoutesList.Create;

  FHasInterfaceDeclarations := [];
  FInterfaceDeclarations := nil;

  FDestructionNotifications := TDynNodeDestructionNotificationArray.Create;
end;

destructor TVRMLNode.Destroy;
begin
  AnyNodeDestructionNotifications.ExecuteAll(Self);

  if DestructionNotifications <> nil then
  begin
    DestructionNotifications.ExecuteAll(Self);
    FreeAndNil(FDestructionNotifications);
  end;

  FreeWithContentsAndNil(FInterfaceDeclarations);

  if FChildren <> nil then RemoveAllChildren;

  if PrototypeInstance then
  begin
    FreeAndNil(FPrototypeInstanceSourceNode);
    FreeAndNil(FPrototypeInstanceHelpers);
    FPrototypeInstance := false;
  end;

  FreeWithContentsAndNil(FPrototypes);
  FreeWithContentsAndNil(FRoutes);

  FreeWithContentsAndNil(FEvents);

  FreeWithContentsAndNil(FFields);
  FreeAndNil(FChildren);
  FreeAndNil(FParentNodes);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TVRMLNode.FreeIfUnused;
begin
  if (FParentNodes.Count = 0) and
     (FParentFields.Count = 0) then
  begin
    { This is written as "Self.Destroy" to actually do the desctruction,
      freeing memory etc. If I would just call it "Destroy", it would
      perform what destructor does but leaving object instance unfreed. }
    Self.Destroy;
  end;
end;

procedure TVRMLNode.AddChild(Index: Integer; child: TVRMLNode);
begin
 Check( {is child allowed in AllowedChildren ?} AllowedChildren,
   'Node '+NodeTypeName+' is not allowed to have child node of type '+
   Child.NodeTypeName);
 child.FParentNodes.Add(Self);
 FChildren.Insert(Index, child);
end;

procedure TVRMLNode.AddChild(child: TVRMLNode);
begin
 AddChild(FChildren.Count, child);
end;

procedure TVRMLNode.RemoveChild(i: integer);
var
  OldChild: TVRMLNode;
begin
  OldChild := FChildren[i];
  FChildren.Delete(i);
  OldChild.FParentNodes.Delete(Self);
  OldChild.FreeIfUnused;
end;

function TVRMLNode.ExtractChild(I: Integer): TVRMLNode;
begin
  Result := FChildren[i];
  FChildren.Delete(i);
  Result.FParentNodes.Delete(Self);

  { RemoveChild now does
      OldChild.FreeIfUnused;
    but ExtractChild doesn't do it. }
end;

procedure TVRMLNode.SetChildrenItem(I: Integer; Value: TVRMLNode);
var
  OldChild: TVRMLNode;
begin
  { Inefficient implementation: RemoveChild(I); AddChild(I, Value); }

  if Value <> FChildren[I] then
  begin
    Check( {is child allowed in AllowedChildren ?} AllowedChildren,
      'Node '+NodeTypeName+' is not allowed to have child node of type '+
      Value.NodeTypeName);

    OldChild := FChildren[i];
    FChildren[I] := Value;

    OldChild.FParentNodes.Delete(Self);
    OldChild.FreeIfUnused;

    Value.FParentNodes.Add(Self);
  end;
end;

procedure TVRMLNode.RemoveAllChildren;
begin
 while FChildren.Count > 0 do RemoveChild(0);
end;

function TVRMLNode.GetChildrenItem(i: integer): TVRMLNode; begin result := FChildren[i] end;
function TVRMLNode.GetParentNodesItem(i: integer): TVRMLNode; begin result := FParentNodes[i] end;

function TVRMLNode.ChildrenCount: integer; begin result := FChildren.Count end;
function TVRMLNode.ParentNodesCount: integer; begin result := FParentNodes.Count end;

procedure TVRMLNode.FreeRemovingFromAllParents;
var
  i, j: integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  if Self = nil then exit;

  for i := 0 to FParentNodes.Count - 1 do
  begin
    j := FParentNodes[i].FChildren.IndexOf(Self);
    FParentNodes[i].FChildren.Delete(j);
    { nie musimy sie tu martwic usuwaniem naszego Parenta z listy
      FParentNodes ktora
     wlasnie przegladamy bo przeciez i tak zaraz zrobimy sobie Destroy; }
  end;

  for I := 0 to FParentFields.Count - 1 do
  begin
    if FParentFields[I] is TSFNode then
    begin
      SF := TSFNode(FParentFields[I]);
      { We remove accessing private SF.FValue,
        not SF.Value property setter,
        to avoid checking our reference count (and possibly calling
        our destructor) by this setter. }
      SF.FValue := nil;
    end else
    if FParentFields[I] is TMFNode then
    begin
      MF := TMFNode(FParentFields[I]);
      { Again we remove using internal methods, that shouldn't be used
        by normal usage from outside: we call directly FItems methods
        (instead of calling MFNode.RemoveItem method that would call our
        RemoveParentField that possibly calls our destructor). }
      J := MF.FItems.IndexOf(Self);
      Assert(J <> -1, 'Node must be present on Items list of parent MFNode');
      MF.FItems.Delete(J);
    end else
      raise EInternalError.Create('TVRMLNode.ParentFields not SF or MF Node class');
  end;

  Self.Destroy;
end;

function TVRMLNode.GetParentFieldsItem(Index: Integer): TVRMLField;
begin
  Result := FParentFields[Index];
end;

function TVRMLNode.GetParentFieldsNodeItem(Index: Integer): TVRMLNode;
var
  F: TVRMLField;
begin
  F := ParentFields[Index];
  if F is TSFNode then
    Result := TSFNode(F).ParentNode else
    Result := (F as TMFNode).ParentNode;
end;

function TVRMLNode.ParentFieldsCount: Integer;
begin
  Result := FParentFields.Count;
end;

procedure TVRMLNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to ChildrenCount - 1 do
    Func(Self, Children[I]);
end;

procedure TVRMLNode.DirectEnumerateAll(
  Func: TEnumerateChildrenFunction);
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
begin
  for I := 0 to ChildrenCount - 1 do
    Func(Self, Children[I]);

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
        Func(Self, SF.Value);
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
      for J := 0 to MF.Items.Count - 1 do
        Func(Self, MF.Items[J]);
    end;
  end;
end;

procedure TVRMLNode.DirectEnumerate(
  Func: TEnumerateChildrenFunction;
  OnlyActive: boolean);
begin
  if OnlyActive then
    DirectEnumerateActive(Func) else
    DirectEnumerateAll(Func);
end;

procedure TVRMLNode.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
end;

procedure TVRMLNode.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
end;

procedure TVRMLNode.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
end;

type
  TTraverseEnumerator = class
    State: TVRMLGraphTraverseState;
    NodeClass: TVRMLNodeClass;
    TraversingFunc: TTraversingFunc;
    ParentInfo: PTraversingInfo;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TTraverseEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    Child.Traverse(State, NodeClass, TraversingFunc, ParentInfo);
  end;

procedure TVRMLNode.Traverse(State: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass; TraversingFunc: TTraversingFunc;
  ParentInfo: PTraversingInfo);
var
  LastNodesIndex: Integer;
  Enumerator: TTraverseEnumerator;
  CurrentInfo: TTraversingInfo;
begin
  BeforeTraverse(State);
  try
    if Self is NodeClass then TraversingFunc(Self, State, ParentInfo);
    MiddleTraverse(State);

    { CurrentInfo will be passed to children as their ParentInfo. }
    CurrentInfo.Node := Self;
    CurrentInfo.ParentInfo := ParentInfo;

    Enumerator := TTraverseEnumerator.Create;
    try
      Enumerator.State := State;
      Enumerator.NodeClass := NodeClass;
      Enumerator.TraversingFunc := TraversingFunc;
      Enumerator.ParentInfo := @CurrentInfo;
      DirectEnumerateActive(
        {$ifdef FPC_OBJFPC} @ {$endif} Enumerator.EnumerateChildrenFunction);
    finally FreeAndNil(Enumerator) end;
  finally AfterTraverse(State) end;

  LastNodesIndex := TraverseStateLastNodesIndex;
  if LastNodesIndex <> -1 then State.FLastNodes.Nodes[LastNodesIndex] := Self;
end;

procedure TVRMLNode.TraverseFromDefaultState(
  NodeClass: TVRMLNodeClass; TraversingFunc: TTraversingFunc);
var
  InitialState: TVRMLGraphTraverseState;
begin
  InitialState := TVRMLGraphTraverseState.Create;
  try
    Traverse(InitialState, NodeClass, TraversingFunc);
  finally InitialState.Free end;
end;

function TVRMLNode.NodeTypeName: string;
begin
 result := ClassNodeTypeName;
end;

class function TVRMLNode.ClassNodeTypeName: string;
begin
 result := '';
end;

constructor TVRMLNode.CreateParse(const ANodeName: string; Lexer: TVRMLLexer);
begin
  Create(ANodeName, '');
  Parse(Lexer);
end;

function TVRMLNode.PathFromWWWBasePath(const RelativePath: string): string;
begin
  { This is a workaround for Blender errorneous VRML 1.0 export.
    Blender exports relative paths by prefixing them by "//"
    (that's a general convention used internally by Blender, AFAIK).
    Here I simply remove this "//". }
  if IsPrefix('//', RelativePath) then
    Result := CombinePaths(WWWBasePath, SEnding(RelativePath, 3)) else
    Result := CombinePaths(WWWBasePath, RelativePath);
end;

procedure TVRMLNode.Parse(Lexer: TVRMLLexer);
var
  Handled: boolean;
  Position: Integer;
  ChildNode: TVRMLNode;
begin
  RemoveAllChildren;

  { In classic VRML encoding, CDATA never exists. }
  CDataExists := false;
  CData := '';

  Position := 0;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  while Lexer.Token <> vtCloseCurlyBracket do
  begin
    Handled := ParseNodeBodyElement(Lexer, Position);

    { VRML 1.0 children nodes are handled as a last resort here
      (that's also why they can't be inside our ParseNodeBodyElement).
      That's because ParseNode just raises exception in case of unknown
      node, so I have to catch first everything else (like "hints" field
      of TNodeShapeHints). }
    if not Handled then
    begin
      if ParsingAllowedChildren then
      begin
        ChildNode := ParseNode(Lexer);
        ChildNode.PositionInParent := Position;
        AddChild(ChildNode);
      end else
      begin
        raise EVRMLParserError.Create(Lexer,
          Format('Invalid VRML node content (unknown or not allowed' +
            ' field, prototype or VRML 1.0-style children) inside "%s": got %s',
            [NodeTypeName, Lexer.DescribeToken]));
      end;
    end;

    Inc(Position);
  end;
  Lexer.NextToken;

  FWWWBasePath := Lexer.WWWBasePath;
end;

function TVRMLNode.ParseNodeBodyElement(Lexer: TVRMLLexer;
  const APositionInParent: Integer): boolean;

  procedure ParseExtensibilityFields;

    procedure ReadOneField;
    var
      FieldTypeName: string;
      //FieldName: string;
      FieldType: TVRMLFieldClass;
    begin
      Lexer.CheckTokenIs(vtName, 'Field type name');
      FieldTypeName := Lexer.TokenName;
      FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
      if FieldType = nil then
        raise EVRMLParserError.Create(
          Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

      Lexer.NextToken;

      Lexer.CheckTokenIs(vtName, 'Field name');
      //FieldName := Lexer.TokenName;

      Lexer.NextToken;

      { TODO: we should actually do something with obtained here
        FieldName, FieldType }
    end;

  begin
    { We parse VRML 1.0 "fields" extensibility feature in a way similar to
      MF fields syntax, this was the intention (although not clarified precisely)
      of VRML 1.0 spec. }
    if Lexer.Token = vtOpenSqBracket then
    begin
      Lexer.NextToken;

      while Lexer.Token <> vtCloseSqBracket do
      begin
        ReadOneField;

        if Lexer.Token = vtCloseSqBracket then break;

        { In VRML >= 2.0 the comma is simply a whitespace and will be ignored
          by the lexer. }
        if Lexer.VRMLVerMajor < 2 then
        begin
          Lexer.CheckTokenIs(vtComma);
          Lexer.NextToken;
        end;
      end;

      { consume final "]" }
      Lexer.NextToken;
    end else
    begin
      { one single field - not enclosed in [] brackets }
      ReadOneField;
    end;
  end;

  procedure ParseExtensibilityIsA;
  var
    IsAField: TMFString;
  begin
    IsAField := TMFString.Create(Self, '', []);
    try
      IsAField.Parse(Lexer, false);

      { TODO: we should actually do something with obtained here
        isA value }
    finally FreeAndNil(IsAField) end;
  end;

var
  I: integer;
  Route: TVRMLRoute;
  Proto: TVRMLPrototypeBase;
  Event: TVRMLEvent;
  IDecl: TVRMLInterfaceDeclaration;
begin
  Result := false;

  { If I would know that all fields used are standard, I could
    check first for if Lexer.TokenName[0] in ['a'..'z'], since all
    standard field names start lowercase. But of course I can't,
    all VRML versions allow to define your own nodes and fields. }
  if Lexer.Token = vtName then
  begin
    I := Fields.IndexOf(Lexer.TokenName);
    if I >= 0 then
    begin
      Result := true;

      { Advance to the next token. Usually, it should be just "Lexer.NextToken;"
        But I have to add here some dirty hack to allow SFString fields
        to contain strings not enclosed in double quotes in VRML 1.0.
        So I call here NextTokenForceVTString before SFString field.

        For VRML >= 2.0, this nonsense feature was fortunately removed,
        and that's good because in VRML >= 2.0 you must be able to use
        keyword "IS" here, so calling NextTokenForceVTString would be bad. }
      if (Fields[I] is TSFString) and (Lexer.VRMLVerMajor <= 1) then
        Lexer.NextTokenForceVTString else
        Lexer.NextToken;

      Fields[I].Parse(Lexer, true);
      Fields[I].PositionInParent := APositionInParent;
    end else
    begin
      Event := AnyEvent(Lexer.TokenName,
        { "IS" clauses for events inside additional InterfaceDeclarations
          will be specified at that InterfaceDeclaration's time.
          So TVRMLInterfaceDeclaration.Parse worries about it, not we,
          so we don't search here inside InterfaceDeclarations. }
        false);
      if Event <> nil then
      begin
        Result := true;
        Lexer.NextToken;
        Event.Parse(Lexer);
        Event.PositionInParent := APositionInParent;
      end else
      if Lexer.TokenName = 'fields' then
      begin
        Result := true;
        Lexer.NextToken;
        ParseExtensibilityFields;
      end else
      if Lexer.TokenName = 'isA' then
      begin
        Result := true;
        Lexer.NextToken;
        ParseExtensibilityIsA;
      end;
    end;
  end else
  if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(HasInterfaceDeclarations)) then
  begin
    Result := true;

    { since we're here, HasInterfaceDeclarations is <> [] }
    Assert(InterfaceDeclarations <> nil);

    IDecl := TVRMLInterfaceDeclaration.Create(Self);
    InterfaceDeclarations.Add(IDecl);
    IDecl.Parse(Lexer, true, true);
    IDecl.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkPROTO) then
  begin
    Result := true;

    Proto := TVRMLPrototype.Create;
    Prototypes.Add(Proto);
    Proto.Parse(Lexer);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkEXTERNPROTO) then
  begin
    Result := true;

    Proto := TVRMLExternalPrototype.Create;
    Prototypes.Add(Proto);
    Proto.Parse(Lexer);
    Proto.PositionInParent := APositionInParent;
  end else
  if Lexer.TokenIsKeyword(vkROUTE) then
  begin
    Result := true;

    Route := TVRMLRoute.Create;
    Routes.Add(Route);
    Route.Parse(Lexer);
    Route.PositionInParent := APositionInParent;
  end;
end;

type
  TEnumerateNodes0Enumerator = class
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes0Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    Child.EnumerateNodes(Proc, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(
  Proc: TVRMLNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes0Enumerator;
begin
  Proc(Self);

  Enumerator := TEnumerateNodes0Enumerator.Create;
  try
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    DirectEnumerate({$ifdef FPC_OBJFPC} @ {$endif}
      Enumerator.EnumerateChildrenFunction, OnlyActive);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes1Enumerator = class
    NodeClass: TVRMLNodeClass;
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes1Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    Child.EnumerateNodes(NodeClass, Proc, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(nodeClass: TVRMLNodeClass;
  Proc: TVRMLNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes1Enumerator;
begin
  if Self is NodeClass then Proc(Self);

  Enumerator := TEnumerateNodes1Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    DirectEnumerate({$ifdef FPC_OBJFPC} @ {$endif}
      Enumerator.EnumerateChildrenFunction, OnlyActive);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes2Enumerator = class
    NodeClass: TVRMLNodeClass;
    SeekNodeName: string;
    Proc: TVRMLNodeProc;
    OnlyActive: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TEnumerateNodes2Enumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    Child.EnumerateNodes(NodeClass, SeekNodeName, Proc, OnlyActive);
  end;

procedure TVRMLNode.EnumerateNodes(NodeClass: TVRMLNodeClass;
  const SeekNodeName: string;
  Proc: TVRMLNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes2Enumerator;
begin
  if (Self is nodeClass) and (NodeName = SeekNodeName) then proc(Self);

  Enumerator := TEnumerateNodes2Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.SeekNodeName := SeekNodeName;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    DirectEnumerate({$ifdef FPC_OBJFPC} @ {$endif}
      Enumerator.EnumerateChildrenFunction, OnlyActive);
  finally FreeAndNil(Enumerator) end;
end;

type
  BreakTryFindNode = class(TCodeBreaker)
  public
    FoundNode: TVRMLNode;
    constructor Create(AFoundNode: TVRMLNode);
  end;

  constructor BreakTryFindNode.Create(AFoundNode: TVRMLNode);
  begin
    inherited Create;
    FoundNode := AFoundNode;
  end;

procedure TVRMLNode.TryFindNode_Found(Node: TVRMLNode);
begin
  raise BreakTryFindNode.Create(Node);
end;

function TVRMLNode.TryFindNode(FindClass: TVRMLNodeClass;
  OnlyActive: boolean): TVRMLNode;
begin
  try
    EnumerateNodes(FindClass, {$ifdef FPC_OBJFPC} @ {$endif}
      TryFindNode_Found, OnlyActive);
    Result := nil;
  except
    on B: BreakTryFindNode do Result := B.FoundNode;
  end;
end;

function TVRMLNode.FindNode(FindClass: TVRMLNodeClass; OnlyActive: boolean): TVRMLNode;
begin
  result := TryFindNode(FindClass, OnlyActive);
  Check(result <> nil,
    'Node class '+FindClass.ClassName+' not found (by TVRMLNode.FindNode)');
end;

function TVRMLNode.TryFindNodeByName(
  FindClass: TVRMLNodeClass; const FindName: string;
  OnlyActive: boolean): TVRMLNode;
begin
  try
    EnumerateNodes(FindClass, FindName, {$ifdef FPC_OBJFPC} @ {$endif}
      TryFindNode_Found, OnlyActive);
    Result := nil;
  except
    on B: BreakTryFindNode do Result := B.FoundNode;
  end;
end;

function TVRMLNode.FindNodeByName(
  FindClass: TVRMLNodeClass; const FindName: string;
  OnlyActive: boolean): TVRMLNode;
begin
  result := TryFindNodeByName(FindClass, FindName, OnlyActive);
  Check(result <> nil,
    'Node name '+FindName+' not found (by TVRMLNode.FindNodeByName)');
end;

{ TVRMLNode.TryFindNodeState/Transform ----------------------------------------- }

  type
    PVRMLNode = ^TVRMLNode;
    PVRMLGraphTraverseState = ^TVRMLGraphTraverseState;
    BreakTryFindNodeState = class(TCodeBreaker);
    TTryFindNodeStateObj = class
      PNode: PVRMLNode;
      PState: PVRMLGraphTraverseState;
      procedure TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState;
        ParentInfo: PTraversingInfo);
    end;

  procedure TTryFindNodeStateObj.TraverseFunc(ANode: TVRMLNode;
    AState: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
  begin
    PNode^ := ANode;
    PState^ := TVRMLGraphTraverseState.CreateCopy(AState);
    raise BreakTryFindNodeState.Create;
  end;

function TVRMLNode.TryFindNodeState(InitialState: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass;
  out Node: TVRMLNode; out State: TVRMLGraphTraverseState): boolean;
var Obj: TTryFindNodeStateObj;
begin
 Obj := TTryFindNodeStateObj.Create;
 try
  try
   Obj.PNode := @Node;
   Obj.PState := @State;
   Traverse(InitialState, NodeClass,
     {$ifdef FPC_OBJFPC} @ {$endif} Obj.TraverseFunc);
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
      PAverageScaleTransform: PSingle;
      procedure TraverseFunc(ANode: TVRMLNode; AState: TVRMLGraphTraverseState;
        ParentInfo: PTraversingInfo);
    end;

  procedure TTryFindNodeTransformObj.TraverseFunc(ANode: TVRMLNode;
    AState: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
  begin
    PNode^ := ANode;
    { to dlatego TryFindNodeTransform jest szybsze od TryFindNodeState :
      w TryFindNodeState trzeba tutaj kopiowac cale state,
      w TryFindNodeTransform wystarczy skopiowac transformacje. }
    PTransform^ := AState.Transform;
    PAverageScaleTransform^ := AState.AverageScaleTransform;
    raise BreakTryFindNodeState.Create;
  end;

function TVRMLNode.TryFindNodeTransform(InitialState: TVRMLGraphTraverseState;
  NodeClass: TVRMLNodeClass;
  out Node: TVRMLNode;
  out Transform: TMatrix4Single;
  out AverageScaleTransform: Single): boolean;
var
  Obj: TTryFindNodeTransformObj;
begin
  Obj := TTryFindNodeTransformObj.Create;
  try
    try
      Obj.PNode := @Node;
      Obj.PTransform := @Transform;
      Obj.PAverageScaleTransform := @AverageScaleTransform;
      Traverse(InitialState, NodeClass,
        {$ifdef FPC_OBJFPC} @ {$endif} Obj.TraverseFunc);
      Result := false;
    except
      on BreakTryFindNodeState do Result := true;
    end;
  finally Obj.Free end;
end;

function TVRMLNode.TryFindParentByName(const FindName: string): TVRMLNode;
var
  I: integer;
begin
  if NodeName = FindName then
    result := Self else
  begin
    result := nil;

    for I := 0 to ParentNodesCount - 1 do
    begin
      result := ParentNodes[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;

    for I := 0 to ParentFieldsCount - 1 do
    begin
      result := ParentFieldsNode[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;
  end;
end;

function TVRMLNode.FindParentByName(const FindName: string): TVRMLNode;
begin
  result := TryFindParentByName(FindName);
  Check(result <> nil, 'Node name '+FindName+' not found in parents');
end;

function TVRMLNode.TryFindDirectParentByName(const FindName: string): TVRMLNode;
var
  I: Integer;
begin
  for I := 0 to ParentNodesCount - 1 do
  begin
    Result := ParentNodes[I];
    if Result.NodeName = FindName then Exit;
  end;

  for I := 0 to ParentFieldsCount - 1 do
  begin
    Result := ParentFieldsNode[I];
    if Result.NodeName = FindName then Exit;
  end;

  Result := nil;
end;

function TVRMLNode.HasParent(Node: TVRMLNode): boolean;
var
  I: integer;
begin
  if Self = Node then
    result := true else
  begin
    for i := 0 to ParentNodesCount - 1 do
      if ParentNodes[i].HasParent(Node) then Exit(true);

    for i := 0 to ParentFieldsCount - 1 do
      if ParentFieldsNode[i].HasParent(Node) then Exit(true);

    result := False;
  end;
end;

type
  TIsNodePresentSeeker = class
    SeekNode: TVRMLNode;
    procedure Seek(Node: TVRMLNode);
  end;

  BreakIsNodePresent = class(TCodeBreaker);

  procedure TIsNodePresentSeeker.Seek(Node: TVRMLNode);
  begin
    if Node = SeekNode then
      raise BreakIsNodePresent.Create;
  end;

function TVRMLNode.IsNodePresent(Node: TVRMLNode;
  OnlyActive: boolean): boolean;
var
  Seeker: TIsNodePresentSeeker;
begin
  Seeker := TIsNodePresentSeeker.Create;
  try
    Seeker.SeekNode := Node;
    try
      EnumerateNodes({$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek, OnlyActive);
      Result := false;
    except
      on BreakIsNodePresent do Result := true;
    end;
  finally FreeAndNil(Seeker) end;
end;

type
  TNodeCounter = class
    procedure CountNode(node: TVRMLNode);
    Counter: integer;
  end;

  procedure TNodeCounter.CountNode(node: TVRMLNode);
  begin Inc(Counter) end;

function TVRMLNode.NodesCount(NodeClass: TVRMLNodeClass;
  CountOnlyActiveNodes: boolean): integer;
var
  C: TNodeCounter;
begin
  C := TNodeCounter.Create;
  try
    EnumerateNodes(NodeClass,
      {$ifdef FPC_OBJFPC} @ {$endif} C.CountNode, CountOnlyActiveNodes);
    result := C.Counter;
  finally C.Free end;
end;

procedure TVRMLNode.SaveContentsToStream(
  SaveProperties: TVRMLSaveToStreamProperties);
var
  I: integer;
  FileItems: TVRMLFileItemsList;
begin
  FileItems := TVRMLFileItemsList.Create;
  try
    if HasInterfaceDeclarations <> [] then
    begin
      for I := 0 to InterfaceDeclarations.Count - 1 do
        FileItems.Add(InterfaceDeclarations[I]);
    end;

    for I := 0 to Prototypes.Count - 1 do
      FileItems.Add(Prototypes[I]);

    for I := 0 to Fields.Count - 1 do
      FileItems.Add(Fields[I]);

    if ChildrenSaveToStream then
      for I := 0 to ChildrenCount - 1 do
        FileItems.Add(Children[I]);

    for I := 0 to Events.Count - 1 do
      if Events[I].IsClause then
        FileItems.Add(Events[I]);

    for I := 0 to Routes.Count - 1 do
      FileItems.Add(Routes[I]);

    FileItems.SaveToStream(SaveProperties);
  finally FreeAndNil(FileItems) end;
end;

procedure TVRMLNode.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
begin
  if PrototypeInstance and (not SaveProperties.WriteExpandedPrototype) then
  begin
    { If this is an expanded prototype, than delegate writing to the
      PrototypeInstanceSourceNode. }
    PrototypeInstanceSourceNode.SaveToStream(SaveProperties);

    { What to do about
        Bind(SaveProperties.NodeNameBinding)
      called from PrototypeInstanceSourceNode.SaveToStream ?
      This means that PrototypeInstanceSourceNode (TVRMLPrototypeNode)
      is bound to given name.
      But when reading, we bound Self node (the actual expanded proto)
      to the same name.
      Routes when saving check this (to make sure correct names are bound).
      So we bind again Self, instead of PrototypeInstanceSourceNode,
      to this name. }

    Bind(SaveProperties.NodeNameBinding);
  end else
  if SaveProperties.NodeNameBinding.IndexOfObject(Self) >= 0 then
  begin
    SaveProperties.WritelnIndent('USE ' + NodeName);
  end else
  begin
    { write us to stream }
    SaveProperties.WriteIndent('');
    if NodeName <> '' then SaveProperties.Write('DEF ' +NodeName +' ');
    SaveProperties.Writeln(NodeTypeName +' {');

    SaveProperties.IncIndent;
    SaveContentsToStream(SaveProperties);
    SaveProperties.DecIndent;

    SaveProperties.WritelnIndent('}');

    { update NodeNameBinding.

      TODO: same problem here as when reading VRML file.
      We call Bind(NodeNameBinding) after writing node contents, because
      we assume there are no cycles... but in case of Script nodes,
      cycles are unfortunately possible. }
    Bind(SaveProperties.NodeNameBinding);
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

class function TVRMLNode.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := true;
end;

procedure TVRMLNode.RemoveParentField(Field: TVRMLField);
begin
  Check(FParentFields.Delete(Field), 'RemoveParentField: parent not found');
  FreeIfUnused;
end;

procedure TVRMLNode.AddParentField(Field: TVRMLField);
begin
  FParentFields.Add(Field);
end;

function TVRMLNode.ChildrenField: TMFNode;
begin
  Result := nil;
end;

procedure TVRMLNode.SmartAddChild(Node: TVRMLNode);
begin
  if ChildrenField = nil then
    AddChild(Node) else
    ChildrenField.AddItem(Node);
end;

function TVRMLNode.GetSmartChildren(Index: Integer): TVRMLNode;
begin
  if ChildrenField = nil then
    Result := Children[Index] else
    Result := ChildrenField.Items[Index];
end;

function TVRMLNode.SmartChildrenCount: integer;
begin
  if ChildrenField = nil then
    Result := ChildrenCount else
    Result := ChildrenField.Items.Count;
end;

function TVRMLNode.SmartExtractChild(Index: Integer): TVRMLNode;
begin
  if ChildrenField = nil then
    Result := ExtractChild(Index) else
    Result := ChildrenField.ExtractItem(Index);
end;

function TVRMLNode.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
  NewResult: boolean;
  NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer;
  ChildIndex: Integer;
begin
  Result := false;

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
      begin
        NewResult := SF.Value.SuggestedVRMLVersion(
          NewVerMajor, NewVerMinor, NewSuggestionPriority);
        AndSuggestedVRMLVersion(
          Result, VerMajor, VerMinor, SuggestionPriority,
          NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
      end;
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
      for J := 0 to MF.Items.Count - 1 do
      begin
        NewResult := MF.Items[J].SuggestedVRMLVersion(
          NewVerMajor, NewVerMinor, NewSuggestionPriority);
        AndSuggestedVRMLVersion(
          Result, VerMajor, VerMinor, SuggestionPriority,
          NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
      end;
    end;
  end;

  for ChildIndex := 0 to ChildrenCount - 1 do
  begin
    NewResult :=  Children[ChildIndex].SuggestedVRMLVersion(
      NewVerMajor, NewVerMinor, NewSuggestionPriority);
    AndSuggestedVRMLVersion(
      Result, VerMajor, VerMinor, SuggestionPriority,
      NewResult, NewVerMajor, NewVerMinor, NewSuggestionPriority);
  end;
end;

procedure TVRMLNode.AndSuggestedVRMLVersion(
  var Result: boolean;
  var VerMajor, VerMinor, SuggestionPriority: Integer;
  const NewResult: boolean;
  const NewVerMajor, NewVerMinor, NewSuggestionPriority: Integer);
begin
  if NewResult then
  begin
    if (not Result) or (NewSuggestionPriority >= SuggestionPriority) then
    begin
      VerMajor := NewVerMajor;
      VerMinor := NewVerMinor;
      SuggestionPriority := NewSuggestionPriority;
      Result := true;
    end;
  end;
end;

class function TVRMLNode.ChildrenSaveToStream: boolean;
begin
  Result := true;
end;

function TVRMLNode.EnumerateReplaceChildren(
  Func: TEnumerateReplaceNodesFunction): Cardinal;
var
  I, J: Integer;
  SF: TSFNode;
  MF: TMFNode;
  NewNode: TVRMLNode;
begin
  { I don't use EnumerateNodes since I have to enumerate them myself,
    since they may be removed during enumeration.
    The code below mimics TVRMLNode.DirectEnumerateAll implementation,
    but it takes into account that nodes may be removed. }

  Result := 0;

  I := 0;
  while I < ChildrenCount do
  begin
    NewNode := Children[I];
    Func(Self, NewNode);
    if NewNode <> Children[I] then
    begin
      RemoveChild(I);
      Inc(Result);
      if NewNode <> nil then
      begin
        AddChild(I, NewNode);
        Inc(I);
      end;
    end else
    begin
      Result += Children[I].EnumerateReplaceChildren(Func);
      Inc(I);
    end;
  end;

  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TSFNode then
    begin
      SF := TSFNode(Fields[I]);
      if SF.Value <> nil then
      begin
        NewNode := SF.Value;
        Func(Self, NewNode);
        if NewNode <> SF.Value then
        begin
          SF.Value := NewNode;
          Inc(Result);
        end else
        begin
          Result += SF.Value.EnumerateReplaceChildren(Func);
        end;
      end;
    end else
    if Fields[I] is TMFNode then
    begin
      MF := TMFNode(Fields[I]);
      J := 0;
      while J < MF.Items.Count do
      begin
        NewNode := MF.Items[J];
        Func(Self, NewNode);
        if NewNode <> MF.Items[J] then
        begin
          MF.RemoveItem(J);
          Inc(Result);
          if NewNode <> nil then
          begin
            MF.AddItem(J, NewNode);
            Inc(J);
          end;
        end else
        begin
          Result += MF.Items[J].EnumerateReplaceChildren(Func);
          Inc(J);
        end;
      end;
    end;
  end;
end;

  type
    TRemoveChildrenWithMatchingNameHelper = class
      Wildcard: string;
      IgnoreCase: boolean;
      procedure DoIt(ParentNode: TVRMLNode; var Node: TVRMLNode);
    end;

  procedure TRemoveChildrenWithMatchingNameHelper.DoIt(
    ParentNode: TVRMLNode; var Node: TVRMLNode);
  begin
    if IsWild(Node.NodeName, Wildcard, IgnoreCase) then
      Node := nil;
  end;

function TVRMLNode.RemoveChildrenWithMatchingName(
  const Wildcard: string; IgnoreCase: Boolean): Cardinal;
var
  Helper: TRemoveChildrenWithMatchingNameHelper;
begin
  Helper := TRemoveChildrenWithMatchingNameHelper.Create;
  try
    Helper.Wildcard := Wildcard;
    Helper.IgnoreCase := IgnoreCase;
    Result := EnumerateReplaceChildren(@Helper.DoIt);
  finally FreeAndNil(Helper) end;
end;

procedure TVRMLNode.Bind(NodeNameBinding: TStringList);
var
  I: Integer;
begin
  if NodeName <> '' then
  begin
    I := NodeNameBinding.IndexOf(NodeName);
    if I >= 0 then
      NodeNameBinding.Objects[I] := Self else
      NodeNameBinding.AddObject(NodeName, Self);
  end;
end;

class function TVRMLNode.URNMatching(const URN: string): boolean;
begin
  Result := false;
end;

type
  TBlenderObjectsTraverser = class
    TraversingFunc: TBlenderTraversingFunc;
    procedure Traverse(Node: TVRMLNode; State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
  end;

procedure TBlenderObjectsTraverser.Traverse(Node: TVRMLNode;
  State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
var
  GeometryNode: TVRMLGeometryNode absolute Node;
  BlenderObjectNode: TVRMLNode;
  BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode;
  BlenderMeshName: string;
begin
  if GeometryNode is TVRMLGeometryNode_1 then
  begin
    { Shape node generated by Blender VRML 1.0 exporter should have
      one parent, and this is his mesh. This mesh may have may
      parents, and these are his objects. }
    if ParentInfo <> nil then
    begin
      BlenderMeshNode := ParentInfo^.Node;
      BlenderMeshName := BlenderMeshNode.NodeName;

      ParentInfo := ParentInfo^.ParentInfo;

      if ParentInfo <> nil then
      begin
        BlenderObjectNode := ParentInfo^.Node;
        { Unfortunately, this will always be ''. Blender VRML 1.0 exporter
          doesn't write this. }
        BlenderObjectName := BlenderObjectNode.NodeName;
        TraversingFunc(BlenderObjectNode, BlenderObjectName,
          BlenderMeshNode, BlenderMeshName, GeometryNode, State);
      end;
    end;
  end else
  if (State.ParentShape <> nil) and (ParentInfo <> nil) then
  begin
    { For VRML 2.0 exporter, the situation is actually quite similar, but
      we have to remove ME_ and OB_ prefixes from node names.
      Oh, and VRML 2.0 exporter actually does write object names.

      Initially we do ParentInfo := ParentInfo^.Parent,
      since we want to start from parent Shape node.
      That's how VRML 2.0 Blender exporter writes. }

    ParentInfo := ParentInfo^.ParentInfo;

    if ParentInfo <> nil then
    begin
      BlenderMeshNode := ParentInfo^.Node;
      BlenderMeshName := PrefixRemove('ME_', BlenderMeshNode.NodeName, false);

      ParentInfo := ParentInfo^.ParentInfo;

      if ParentInfo <> nil then
      begin
        BlenderObjectNode := ParentInfo^.Node;
        BlenderObjectName := PrefixRemove('OB_', BlenderObjectNode.NodeName, false);
        TraversingFunc(BlenderObjectNode, BlenderObjectName,
          BlenderMeshNode, BlenderMeshName, GeometryNode, State);
      end;
    end;
  end;
end;

procedure TVRMLNode.TraverseBlenderObjects(
  State: TVRMLGraphTraverseState;
  TraversingFunc: TBlenderTraversingFunc);
var
  Traverser: TBlenderObjectsTraverser;
begin
  Traverser := TBlenderObjectsTraverser.Create;
  try
    Traverser.TraversingFunc := TraversingFunc;
    Traverse(State, TVRMLGeometryNode, @Traverser.Traverse);
  finally FreeAndNil(Traverser) end;
end;

procedure TVRMLNode.TraverseBlenderObjects(
  TraversingFunc: TBlenderTraversingFunc);
var
  InitialState: TVRMLGraphTraverseState;
begin
  InitialState := TVRMLGraphTraverseState.Create;
  try
    TraverseBlenderObjects(InitialState, TraversingFunc);
  finally InitialState.Free end;
end;

function TVRMLNode.FieldOrEvent(const Name: string;
  SearchInterfaceDeclarations: boolean): TVRMLFieldOrEvent;
var
  I: Integer;
  ResultEvent: TVRMLEvent;
begin
  I := Fields.IndexOf(Name);
  if I <> -1 then
    Exit(Fields[I]);

  { I use helper ResultEvent below, instead of passing
    "TVRMLEvent(Result)" as last param: don't know why,
    but with FPC 2.2.0 this cast may fail (even though it shouldn't
    be checked at all?), testcase:
      view3dscene www.web3d.org/x3d/content/examples/Basic/CAD/CADGeometryPrototypes.x3d
  }

  I := Fields.IndexOfExposedEvent(Name, ResultEvent);
  if I <> -1 then
    Exit(ResultEvent);

  I := Events.IndexOf(Name);
  if I <> -1 then
    Exit(Events[I]);

  if SearchInterfaceDeclarations and (InterfaceDeclarations <> nil) then
  begin
    Result := InterfaceDeclarations.TryFindName(Name);
    if Result <> nil then
      Exit;
  end;

  Result := nil; { not found }
end;

function TVRMLNode.AnyEvent(const Name: string;
  SearchInterfaceDeclarations: boolean): TVRMLEvent;
var
  I: Integer;
begin
  I := Fields.IndexOfExposedEvent(Name, Result);
  if I <> -1 then
    Exit; { Result is already set }

  I := Events.IndexOf(Name);
  if I <> -1 then
    Exit(Events[I]);

  if SearchInterfaceDeclarations and (InterfaceDeclarations <> nil) then
  begin
    Result := InterfaceDeclarations.TryFindEventName(Name);
    if Result <> nil then
      Exit;
  end;

  Result := nil; { not found }
end;

procedure TVRMLNode.SetHasInterfaceDeclarations(const Value: TVRMLAccessTypes);
begin
  if Value <> HasInterfaceDeclarations then
  begin
    FHasInterfaceDeclarations := Value;
    if HasInterfaceDeclarations <> [] then
    begin
      { make sure InterfaceDeclarations is non-nil }
      if FInterfaceDeclarations = nil then
        FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
    end else
    begin
      { make sure InterfaceDeclarations is nil }
      FreeWithContentsAndNil(FInterfaceDeclarations);
    end;
  end;
end;

{ TVRMLNodeClassesList ------------------------------------------------------- }

function TVRMLNodeClassesList.GetItems(Index: Integer): TVRMLNodeClass;
begin
  Result := TVRMLNodeClass(inherited Items[Index]);
end;

procedure TVRMLNodeClassesList.SetItems(Index: Integer; Value: TVRMLNodeClass);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TVRMLNodeClassesList.AssignArray(
  const AItemsArray: array of TVRMLNodeClass);
var
  I: Integer;
begin
  Count := High(AItemsArray) + 1;
  for I := 0 to High(AItemsArray) do
    Items[I] := AItemsArray[I];
end;

function TVRMLNodeClassesList.IndexOf(NodeClass: TVRMLNodeClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = NodeClass then
      Exit;
  Result := -1;
end;

function TVRMLNodeClassesList.IndexOf(Node: TVRMLNode): Integer;
begin
  Result := IndexOf(TVRMLNodeClass(Node.ClassType));
end;

function TVRMLNodeClassesList.IndexOfAnyAncestor(Node: TVRMLNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Node is TVRMLNodeClass(Items[Result]) then
      Exit;
  Result := -1;
end;

procedure TVRMLNodeClassesList.Add(Value: TVRMLNodeClass);
begin
  inherited Add(Pointer(Value));
end;

procedure TVRMLNodeClassesList.AddRegisteredImplementing(Interf: TGUID);
var
  I: Integer;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
    if Supports(NodesManager.Registered[I], Interf) then
      Add(NodesManager.Registered[I]);
end;

{ TSFNode --------------------------------------------------------------------- }

constructor TSFNode.CreateUndefined(AParentNode: TVRMLFileItem;
  const AName: string);
begin
  inherited;
  Value := nil;

  FAllowedChildren := TVRMLNodeClassesList.Create;
  FAllowedChildrenAll := true;

  FDefaultValue := nil;
  FDefaultValueExists := false;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  const AnAllowedChildren: array of TVRMLNodeClass;
  AValue: TVRMLNode);
begin
  inherited Create(AParentNode, AName);

  { FParentNode is just a copy of inherited (TVRMLFieldOrEvent) FParentNode,
    but casted to TVRMLNode }
  FParentNode := AParentNode;

  FAllowedChildren.AssignArray(AnAllowedChildren);
  FAllowedChildrenAll := false;

  Value := AValue;
  AssignDefaultValueFromValue;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AnAllowedChildren: TVRMLNodeClassesList;
  AValue: TVRMLNode);
begin
  Create(AParentNode, AName, [], AValue);

  FAllowedChildren.Assign(AnAllowedChildren);
  FAllowedChildrenAll := false;
end;

constructor TSFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AllowedChildrenInterface: TGUID;
  AValue: TVRMLNode);
var
  AllowedChildren: TVRMLNodeClassesList;
begin
  { TODO: this unnecessarily creates long list of AllowedChildren,
    and depends on current NodesManager registered nodes.
    Some day I may improve this, to just store AllowedChildrenInterface
    inside, and check at runtime "Supports".
    Same thing for MFNode analogous constructor. }

  AllowedChildren := TVRMLNodeClassesList.Create;
  AllowedChildren.AddRegisteredImplementing(AllowedChildrenInterface);
  try
    Create(AParentNode, AName, AllowedChildren, AValue);
  finally FreeAndNil(AllowedChildren) end;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  { To delete Self from DefaultValue.FParentFields, and eventually free DefaultValue. }
  DefaultValue := nil;
  FreeAndNil(FAllowedChildren);
  inherited;
end;

function TSFNode.ChildAllowed(Child: TVRMLNode): boolean;
begin
  Result := (Child = nil) or
    AllowedChildrenAll or
    (FAllowedChildren.IndexOfAnyAncestor(Child) <> -1);
end;

function TSFNode.CurrentChildAllowed: boolean;
begin
  Result := ChildAllowed(Value);
end;

procedure TSFNode.WarningIfChildNotAllowed(Child: TVRMLNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.NodeTypeName, Name]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.NodeTypeName]);
    VRMLNonFatalError(S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TSFNode.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  if (Lexer.Token = vtKeyword) and (Lexer.TokenKeyword = vkNULL) then
  begin
    Value := nil;
    Lexer.NextToken;
  end else
  begin
    { This is one case when we can use NilIfUnresolvedUSE = @true }
    Value := ParseNode(Lexer, true);
    if Value <> nil then
      WarningIfChildNotAllowed(Value);
  end;
end;

procedure TSFNode.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  if Value = nil then
    SaveProperties.Write('NULL') else
  begin
    { TVRMLNode.SaveToStream normally starts from new line with an indent...
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    SaveProperties.DiscardNextIndent;
    Value.SaveToStream(SaveProperties);
  end;
end;

function TSFNode.EqualsDefaultValue: boolean;
begin
  Result := (not IsClause) and DefaultValueExists and (Value = DefaultValue);
end;

function TSFNode.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Double): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFNode) and
   (TSFNode(SecondValue).Value = Value);
end;

procedure TSFNode.Assign(Source: TPersistent);
begin
  if Source is TSFNode then
  begin
    { Assign using Value property, so that FParentFields will get
      correctly updated. }
    Value              := TSFNode(Source).Value;
    DefaultValue       := TSFNode(Source).DefaultValue;
    DefaultValueExists := TSFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TSFNode.AssignValue(Source: TVRMLField);
begin
  if Source is TSFNode then
  begin
    inherited;
    Value := TSFNode(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFNode.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSFNode.SetValue(AValue: TVRMLNode);
begin
  if FValue <> AValue then
  begin
    if FValue <> nil then
      FValue.RemoveParentField(Self);

    FValue := AValue;

    if AValue <> nil then
      FValue.AddParentField(Self);
  end;
end;

procedure TSFNode.SetDefaultValue(ADefaultValue: TVRMLNode);
begin
  if FDefaultValue <> ADefaultValue then
  begin
    if FDefaultValue <> nil then
      FDefaultValue.RemoveParentField(Self);

    FDefaultValue := ADefaultValue;

    if ADefaultValue <> nil then
      FDefaultValue.AddParentField(Self);
  end;
end;

procedure TSFNode.SetDefaultValueExists(AValue: boolean);
begin
  FDefaultValueExists := AValue;
end;

class function TSFNode.VRMLTypeName: string;
begin
  Result := 'SFNode';
end;

procedure TSFNode.EnumerateValid(Func: TEnumerateChildrenFunction);
begin
  if (Value <> nil) and CurrentChildAllowed then
    Func(ParentNode, Value);
end;

{ TMFNode -------------------------------------------------------------------- }

constructor TMFNode.CreateUndefined(AParentNode: TVRMLFileItem;
  const AName: string);
begin
  inherited;
  FItems := TVRMLNodesList.Create;

  FAllowedChildren := TVRMLNodeClassesList.Create;
  FAllowedChildrenAll := true;

  FDefaultItems := TVRMLNodesList.Create;
  FDefaultValueExists := false;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  const AnAllowedChildren: array of TVRMLNodeClass);
begin
  inherited Create(AParentNode, AName);
  FParentNode := AParentNode;

  FAllowedChildren.AssignArray(AnAllowedChildren);
  FAllowedChildrenAll := false;

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AnAllowedChildren: TVRMLNodeClassesList);
begin
  Create(AParentNode, AName, []);

  FAllowedChildren.Assign(AnAllowedChildren);
  FAllowedChildrenAll := false;
end;

constructor TMFNode.Create(AParentNode: TVRMLNode; const AName: string;
  AllowedChildrenInterface: TGUID);
var
  AllowedChildren: TVRMLNodeClassesList;
begin
  AllowedChildren := TVRMLNodeClassesList.Create;
  AllowedChildren.AddRegisteredImplementing(AllowedChildrenInterface);
  try
    Create(AParentNode, AName, AllowedChildren);
  finally FreeAndNil(AllowedChildren) end;
end;

destructor TMFNode.Destroy;
begin
  ClearItems;
  ClearDefaultItems;
  FreeAndNil(FItems);
  FreeAndNil(FDefaultItems);
  FreeAndNil(FAllowedChildren);
  inherited;
end;

procedure TMFNode.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
var
  I: Integer;
begin
  { We code Count = 0 and Count = 1 cases separately just to get a more
    compact look in these common situations. }
  if Count = 0 then
    SaveProperties.Write('[]') else
  if Count = 1 then
  begin
    { TVRMLNode.SaveToStream normally starts from new line with an indent...
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    SaveProperties.DiscardNextIndent;
    Items[0].SaveToStream(SaveProperties);
  end else
  begin
    SaveProperties.Writeln('[');
    SaveProperties.IncIndent;
    for I := 0 to Count - 1 do
      Items[I].SaveToStream(SaveProperties);
    SaveProperties.DecIndent;
    SaveProperties.WriteIndent(']');
  end;
end;

function TMFNode.Count: integer;
begin
  Result := Items.Count;
end;

procedure TMFNode.AddItem(Node: TVRMLNode);
begin
  Items.Add(Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.AddItem(Position: Integer; Node: TVRMLNode);
begin
  Items.Insert(Position, Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.RemoveItem(Index: Integer);
begin
  Items[Index].RemoveParentField(Self);
  Items.Delete(Index);
end;

function TMFNode.ExtractItem(Index: Integer): TVRMLNode;
begin
  Result := Items[Index];

  { Instead of calling Result.RemoveParentField(Self), which would possibly
    free Result, we manually call FParentFields.Delete. }
  Result.FParentFields.Delete(Self);

  Items.Delete(Index);
end;

procedure TMFNode.ReplaceItem(Index: Integer; Node: TVRMLNode);
begin
  if FItems[Index] <> Node then
  begin
    FItems[Index].RemoveParentField(Self);
    FItems[Index] := Node;
    FItems[Index].AddParentField(Self);
  end;
end;

procedure TMFNode.ClearItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].RemoveParentField(Self);
  FItems.Count := 0;
end;

procedure TMFNode.ClearDefaultItems;
var
  I: Integer;
begin
  for I := 0 to FDefaultItems.Count - 1 do
    FDefaultItems[I].RemoveParentField(Self);
  FDefaultItems.Count := 0;
end;

procedure TMFNode.AssignItems(SourceItems: TVRMLNodesList);
var
  I: Integer;
begin
  ClearItems;

  Items.Assign(SourceItems);

  for I := 0 to Count - 1 do
    Items[I].AddParentField(Self);
end;

procedure TMFNode.AssignDefaultItems(SourceItems: TVRMLNodesList);
var
  I: Integer;
begin
  ClearDefaultItems;

  DefaultItems.Assign(SourceItems);

  for I := 0 to DefaultItems.Count - 1 do
    DefaultItems[I].AddParentField(Self);
end;

function TMFNode.ChildAllowed(Child: TVRMLNode): boolean;
begin
  Result := (Child = nil) or
    AllowedChildrenAll or
    (FAllowedChildren.IndexOfAnyAncestor(Child) <> -1);
end;

procedure TMFNode.WarningIfChildNotAllowed(Child: TVRMLNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.NodeTypeName, Name]);
    if ParentNode <> nil then
      S += Format(' of the node "%s"', [ParentNode.NodeTypeName]);
    VRMLNonFatalError(S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TMFNode.Parse(Lexer: TVRMLLexer;
  IsClauseAllowed: boolean);

  procedure ParseOneItem;
  var
    Node: TVRMLNode;
  begin
    Node := ParseNode(Lexer);
    AddItem(Node);
    WarningIfChildNotAllowed(Node);
  end;

begin
  inherited;

  ClearItems;

  if IsClause then Exit;

  { Note that we ignore commas here, because MFNode is in VRML 2.0 only. }
  if Lexer.Token = vtOpenSqBracket then
  begin
    Lexer.NextToken;

    while Lexer.Token <> vtCloseSqBracket do
      ParseOneItem;

    Lexer.NextToken;
  end else
  begin
    { one single item - not enclosed in [] brackets }
    ParseOneItem;
  end;
end;

function TMFNode.EqualsDefaultValue: boolean;
begin
  Result := (not IsClause) and DefaultValueExists and
    DefaultItems.Equals(Items);
end;

function TMFNode.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Double): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TMFNode) and
    (TMFNode(SecondValue).Items.Equals(Items));
end;

procedure TMFNode.Assign(Source: TPersistent);
begin
  if Source is TMFNode then
  begin
    AssignItems(TMFNode(Source).Items);
    AssignDefaultItems(TMFNode(Source).DefaultItems);
    DefaultValueExists := TMFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TMFNode.AssignValue(Source: TVRMLField);
begin
  if Source is TMFNode then
  begin
    inherited;
    AssignItems(TMFNode(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TMFNode.AssignDefaultValueFromValue;
begin
  inherited;
  AssignDefaultItems(Items);
  DefaultValueExists := true;
end;

class function TMFNode.VRMLTypeName: string;
begin
  Result := 'MFNode';
end;

procedure TMFNode.EnumerateValid(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if ChildAllowed(Items[I]) then
      Func(ParentNode, Items[I]);
end;

{ TVRMLGeometryNode ---------------------------------------------------------- }

constructor TVRMLGeometryNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'geometry';
end;

function TVRMLGeometryNode.Coord(State: TVRMLGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := false;
end;

function TVRMLGeometryNode.Coordinates(State: TVRMLGraphTraverseState): TMFVec3f;
begin
  if not Coord(State, Result) then
    raise ENotCoordinateBasedNode.CreateFmt('Node %s is not a coordinate-based node',
      [NodeTypeName]);
end;

function TVRMLGeometryNode.CoordIndex: TMFLong;
begin
  Result := nil;
end;

function TVRMLGeometryNode.CoordRangesCounts(
  out RangeCount: TDynLongIntArray;
  out SRanges, SRangeName: string;
  out RangeMinimumCount: Cardinal): boolean;
begin
  Result := false;
end;

procedure TVRMLGeometryNode.MakeCoordRanges(
  State: TVRMLGraphTraverseState;
  CoordRangeHandler: TCoordRangeHandler);
var
  BeginIndex, EndIndex: Integer;
  RangeNumber: Cardinal;
  RangeCount: TDynLongIntArray;
  SRanges, SRangeName: string;
  RangeMinimumCount: Cardinal;
  C: TMFVec3f;
begin
  C := Coordinates(State);

  if C = nil then
    Exit;

  if CoordIndex <> nil then
  begin
    BeginIndex := 0;
    RangeNumber := 0;
    while BeginIndex < CoordIndex.Count do
    begin
      EndIndex := BeginIndex;
      while (EndIndex < CoordIndex.Count) and
            (CoordIndex.Items.Items[EndIndex] >= 0) do
        Inc(EndIndex);
      CoordRangeHandler(RangeNumber, BeginIndex, EndIndex);
      Inc(RangeNumber);
      BeginIndex := EndIndex + 1;
    end;
  end else
  begin
    if not CoordRangesCounts(RangeCount, SRanges, SRangeName,
      RangeMinimumCount) then
      raise EInternalError.CreateFmt('%s.MakeCoordRanges: either CoordIndex or CoordRangesCounts must be defined to split coordinates', [ClassName]);
    EndIndex := 0;
    if RangeCount.Count > 0 then
      for RangeNumber := 0 to RangeCount.Count - 1 do
      begin
        BeginIndex := EndIndex;
        EndIndex := BeginIndex + RangeCount.Items[RangeNumber];
        { Note that EndIndex *may* be equal to C.Count,
          as EndIndex is not taken into account by CoordRangeHandler. }
        if EndIndex > C.Count then
        begin
          VRMLNonFatalError(Format('Too much %s (not enough coordinates) in %s',
            [SRanges, NodeTypeName]));
          Break;
        end;
        if Cardinal(EndIndex - BeginIndex) >= RangeMinimumCount then
          CoordRangeHandler(RangeNumber, BeginIndex, EndIndex) else
          VRMLNonFatalError(Format('%s is less than %d in %s',
            [SRangeName, RangeMinimumCount, NodeTypeName]));
      end;
  end;
end;

procedure TVRMLGeometryNode.CoordPolygons(
  State: TVRMLGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
begin
  { Nothing to do in this class. }
end;

function TVRMLGeometryNode.Proxy: TVRMLGeometryNode;
begin
  Result := nil;
end;

{ TVRMLUnknownNode ---------------------------------------------------------------- }

function TVRMLUnknownNode.NodeTypeName: string;
begin
 result := fNodeTypeName;
end;

procedure ParseIgnoreToMatchingCurlyBracket(Lexer: TVRMLLexer); forward;

procedure TVRMLUnknownNode.Parse(Lexer: TVRMLLexer);
{ TODO: tutaj zrobic parsowanie node'ow unknown typu 2) i 3),
  VRMlNonFatalError tez nie trzeba zawsze rzucac. }
begin
  { w przypadku TVRMLUnknownNode musimy fAllowedChildren i fParseAllowedChildren
    inicjowac na podstawie parsowania. }
  fAllowedChildren := false;
  fParsingAllowedChildren := false;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  ParseIgnoreToMatchingCurlyBracket(Lexer);

  FWWWBasePath := Lexer.WWWBasePath;

  VRMLNonFatalError('Unknown VRML node of type '''+NodeTypeName+
    ''' (named '''+NodeName+''')');
end;

constructor TVRMLUnknownNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 { ponizej : "bezpiecznik" zeby nigdy nie tworzyc tego node'a normalnie,
   zeby zawsze fNodeTypeName bylo ustalone. }
 raise Exception.Create('You cannot create Unknown node using default constructor');
end;

constructor TVRMLUnknownNode.CreateUnknown(const ANodeName, AWWWBasePath, ANodeTypeName :string);
begin
 inherited Create(ANodeName, AWWWBasePath);
 fNodeTypeName := ANodeTypeName;
end;

constructor TVRMLUnknownNode.CreateUnknownParse(const ANodeName, ANodeTypeName :string;
  Lexer: TVRMLLexer);
begin
 CreateUnknown(ANodeName, '', ANodeTypeName);
 Parse(Lexer);
end;

{ TVRMLInterfaceDeclaration -------------------------------------------------- }

constructor TVRMLInterfaceDeclaration.Create(AParentNode: TVRMLNode);
begin
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TVRMLInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FFieldOrEvent);
  FField := nil;
  FEvent := nil;

  inherited;
end;

procedure TVRMLInterfaceDeclaration.SetFieldOrEvent(
  const Value: TVRMLFieldOrEvent);
begin
  FFieldOrEvent := Value;

  { set FField and FEvent, for fast access to them }
  if FFieldOrEvent = nil then
  begin
    FField := nil;
    FEvent := nil;
  end else
  if FFieldOrEvent is TVRMLField then
  begin
    FField := TVRMLField(FFieldOrEvent);
    FEvent := nil;
  end else
  begin
    Assert(FFieldOrEvent is TVRMLEvent);
    FField := nil;
    FEvent := TVRMLEvent(FFieldOrEvent);
  end;
end;

procedure TVRMLInterfaceDeclaration.Parse(Lexer: TVRMLLexer;
  FieldValue, IsClauseAllowed: boolean);
var
  FieldTypeName: string;
  Kind: TVRMLAccessType;
  FieldType: TVRMLFieldClass;
  Name: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  if Lexer.Token = vtKeyword then
  begin
    case Lexer.TokenKeyword of
      vkEventIn, vkInputOnly: Kind := atInputOnly;
      vkEventOut, vkOutputOnly: Kind := atOutputOnly;
      vkField, vkInitializeOnly: Kind := atInitializeOnly;
      vkExposedField, vkInputOutput: Kind := atInputOutput;
      else raise EVRMLParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
    end;
  end else
    raise EVRMLParserError.Create(
      Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'field type (for interface declaration)');
  FieldTypeName := Lexer.TokenName;
  FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
  if FieldType = nil then
    raise EVRMLParserError.Create(
      Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'name (for interface declaration)');
  Name := Lexer.TokenName;

  { we know everything now to create Event/Field instance }
  case Kind of
    atInputOnly, atOutputOnly:
      FieldOrEvent := TVRMLEvent.Create(ParentNode, Name, FieldType, Kind = atInputOnly);
    atInitializeOnly, atInputOutput:
      begin
        FieldOrEvent := FieldType.CreateUndefined(ParentNode, Name);
        Field.Exposed := Kind = atInputOutput;
      end;
    else raise EInternalError.Create('Kind ? in TVRMLInterfaceDeclaration.Parse');
  end;

  Lexer.NextToken;

  if Event <> nil then
  begin
    if IsClauseAllowed then
      Event.Parse(Lexer);
  end else
  begin
    if FieldValue then
      Field.Parse(Lexer, IsClauseAllowed) else
    if IsClauseAllowed then
      Field.ParseIsClause(Lexer);
  end;
end;

procedure TVRMLInterfaceDeclaration.IDeclSaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties;
  FieldValue: boolean);

  function ATName(const AcessType: TVRMLAccessType): string;
  const
    Names: array
      [ boolean { is it X3D ? },
        TVRMLAccessType] of string =
      ( ('eventIn', 'eventOut', 'field', 'exposedField'),
        ('inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput') );
  begin
    Result := Names[SaveProperties.VerMajor >= 3, AcessType];
  end;

begin
  if Event <> nil then
  begin
    if Event.InEvent then
      SaveProperties.WriteIndent(ATName(atInputOnly) + ' ') else
      SaveProperties.WriteIndent(ATName(atOutputOnly) + ' ');
    SaveProperties.Write(Event.FieldClass.VRMLTypeName + ' ');
    SaveProperties.DiscardNextIndent;
    Event.SaveToStream(SaveProperties);
  end else
  begin
    if Field.Exposed then
      SaveProperties.WriteIndent(ATName(atInputOutput) + ' ') else
      SaveProperties.WriteIndent(ATName(atInitializeOnly) + ' ');
    SaveProperties.Write(Field.VRMLTypeName + ' ');
    { Do not write field only if Field.IsClause is @false and
      FieldValue = @false, so the field has a value but we don't want to
      output it. }
    if Field.IsClause or FieldValue then
    begin
      { Field.SaveToStream normally starts from new line with an indent...
        In this case, we want it to start on the same line, so indent must
        be discarded. }
      SaveProperties.DiscardNextIndent;
      Field.FieldSaveToStream(SaveProperties, true);
      { In this case, SaveProperties.Writeln will be done by Field.SaveToStream.
        (we pass SaveWhenDefault anyway, so we can be sure that
        this newline will be done). }
    end else
    begin
      SaveProperties.Writeln(Field.Name);
    end;
  end;
end;

procedure TVRMLInterfaceDeclaration.SaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties);
begin
  IDeclSaveToStream(SaveProperties, true);
end;

function TVRMLInterfaceDeclaration.AccessType: TVRMLAccessType;
begin
  if Event <> nil then
  begin
    if Event.InEvent then
      Result := atInputOnly else
      Result := atOutputOnly;
  end else
  if Field <> nil then
  begin
    if Field.Exposed then
      Result := atInputOutput else
      Result := atInitializeOnly;
  end else
    { Result is undefined in this case, but we don't want to signal any error }
    Result := atInitializeOnly
end;

{ TVRMLInterfaceDeclarationsList --------------------------------------------- }

function TVRMLInterfaceDeclarationsList.TryFindName(
  const Name: string): TVRMLFieldOrEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FieldOrEvent;
    if Result.Name = Name then
      Exit;
  end;
  Result := nil;
end;

function TVRMLInterfaceDeclarationsList.TryFindFieldName(const Name: string): TVRMLField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TVRMLField then
    begin
      Result := Items[I].Field;
      if Result.Name = Name then
        Exit;
    end;
  Result := nil;
end;

function TVRMLInterfaceDeclarationsList.TryFindEventName(const Name: string): TVRMLEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TVRMLEvent then
    begin
      Result := Items[I].Event;
      if Result.Name = Name then
        Exit;
    end;
  Result := nil;
end;

{ TVRMLPrototypeNode --------------------------------------------------------- }

constructor TVRMLPrototypeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  raise EInternalError.Create('TVRMLPrototypeNode node must be created' +
    ' using CreatePrototypeNode, never default constructor');
end;

constructor TVRMLPrototypeNode.CreatePrototypeNode(
  const ANodeName, AWWWBasePath :string;
  APrototype: TVRMLPrototypeBase);
var
  I: TVRMLInterfaceDeclaration;
  Index: Integer;
  F: TVRMLField;
  E: TVRMLEvent;
begin
  inherited Create(ANodeName, AWWWBasePath);
  FPrototype := APrototype;
  for Index := 0 to Prototype.InterfaceDeclarations.Count - 1 do
  begin
    I := Prototype.InterfaceDeclarations.Items[Index];
    if I.Field <> nil then
    begin
      Check(not I.Field.IsClause,
        'Prototype interface field cannot have "IS" clause');

      { F := copy of I.Field }
      F := TVRMLFieldClass(I.Field.ClassType).CreateUndefined(Self, I.Field.Name);
      F.Assign(I.Field);

      { CreateUndefined creates field without any default value,
        so it will always get saved later to file.

        But this is not nice: for non-node fields, it merely makes
        resulting file longer. For node fields (SFNode and MFNode)
        this means that node value will be written to file. But this
        is bad, since this means that node contents will have to duplicated,
        if node is not named or it's name is unbound now (e.g. overridden
        by other node name) (otherwise "USE Xxx" could be used, which
        is acceptable).

        See ../../kambi_vrml_test_suite/x3d/proto_sfnode_default.x3dv
        and tricky_def_use.x3dv for
        examples (open and save it back e.g. in view3dscene).

        So to make it work right, we have to set DefaultValue for our
        fields, in particular for TSFNode and TMFNode fields.
        So that EqualsDefaultValue will work Ok when saving to file. }
      F.AssignDefaultValueFromValue;

      Fields.Add(F);
    end else
    if I.Event <> nil then
    begin
      { E := copy of I.Event }
      E := TVRMLEvent.Create(Self,
        I.Event.Name, I.Event.FieldClass, I.Event.InEvent);
      Events.Add(E);
    end else
      raise EInternalError.Create('interface declaration but no Field or Event');
  end;
end;

function TVRMLPrototypeNode.NodeTypeName: string;
begin
  Result := Prototype.Name;
end;

procedure TVRMLPrototypeNode.FieldsAssignValue(Destination, Source: TVRMLField;
  UsingIsClause: boolean);
begin
  if Source.IsClause then
  begin
    Destination.IsClauseName := Source.IsClauseName;
  end else
  try
    Destination.AssignValue(Source);
  except
    on E: EVRMLFieldAssign do
    begin
      if (E is EVRMLFieldAssignInvalidClass) and UsingIsClause then
        VRMLNonFatalError(Format('Within prototype "%s", ' +
          'field of type %s (named "%s") references ' +
          '(by "IS" clause) field of different type %s (named "%s")',
          [Prototype.Name,
           Destination.VRMLTypeName,
           Destination.Name,
           Source.VRMLTypeName,
           Source.Name])) else
        VRMLNonFatalError(Format('Error when expanding prototype "%s": ',
          [Prototype.Name]) + E.Message);
    end;
  end;
end;

procedure TVRMLPrototypeNode.InstantiateReplaceIsClauses(
  Node, Child: TVRMLNode);

  { @true if field is exposed, and it's IsClauseName references
    one of our events (either explicit ones, in Events, or implicit,
    in our own exposedFields). }
  function ExposedFieldReferencesEvent(Field: TVRMLField): boolean;
  begin
    Result := Field.Exposed and (AnyEvent(Field.IsClauseName,
      { It doesn't matter whether this parameter is false/true.
        That's because TVRMLPrototypeNode has always HasInterfaceDeclarations
        = [], in other words when instantiating a prototype you cannot
        add additional interface decls (like for Script or ComposedShader nodes).
        For the future, I set this to true, in case it will
        be possible some day. }
      true) <> nil);
  end;

var
  InstanceField, OurField: TVRMLField;
  I, OurFieldIndex: Integer;
begin
  if Child.PrototypeInstance then Exit;

  for I := 0 to Child.Fields.Count - 1 do
  begin
    InstanceField := Child.Fields[I];
    if InstanceField.IsClause then
    begin
      OurFieldIndex := Fields.IndexOf(InstanceField.IsClauseName);
      if OurFieldIndex <> -1 then
      begin
        OurField := Fields[OurFieldIndex];

        { Note that if OurField.IsClause, then InstanceField will have
          OurField.IsClauseName assigned.

          This means that the prototype (current Prototype) is expanded
          within the definition of another prototype.
          So InstanceField.IsClauseName referers to OurField
          (from current Prototype), but OurField.IsClauseName refers to yet
          another, enclosing, prototype (that will be expanded later --- for
          now we're within enclosing prototype definition).

          So solution is simple: InstanceField is expanded to also
          refer to this enclosing prototype. }

        FieldsAssignValue(InstanceField, OurField, true);
      end else
      if not ExposedFieldReferencesEvent(InstanceField) then
      begin
        VRMLNonFatalError(Format('Within prototype "%s", field "%s" references ' +
          '(by "IS" clause) non-existing field "%s"',
          [Prototype.Name, InstanceField.Name, InstanceField.IsClauseName]));
      end;
    end;
  end;

  Child.DirectEnumerateAll(@InstantiateReplaceIsClauses);

  { TODO: some kind of events "IS" copying also should take place here.
    For now I don't use events, so I don't do it.

    Also, above ExposedFieldReferencesEvent is used only to avoid
    warnings... while in fact, it should have some effect on the field value
    and associations with event.

    file:///home/michalis/doc/web3d.org/vrml_97/ISO-IEC-14772-VRML97/part1/concepts.html#4.8.3
  }
end;

function TVRMLPrototypeNode.Instantiate: TVRMLNode;

  procedure InstantiateNonExternalPrototype(Proto: TVRMLPrototype);
  var
    NodeCopy: TVRMLNode;
  begin
    { Even when Proto.Node is a wrapper (TVRMLRootNode_*),
      we want to copy the whole Proto.Node, instead of copying separately
      Proto.Node.SmartChildren[0], Proto.Node.SmartChildren[1] etc.
      This way, DEF / USE links within are preserved as they should.

      Moreover, we can keep Routes and prototypes correctly.
      (although nested prototypes are not important at this point,
      since they are already expanded; but still, preserving them
      "feels right"). }
    NodeCopy := VRMLNodeDeepCopy(Proto.Node);

    if (Proto.Node is TVRMLRootNode_1) or
       (Proto.Node is TVRMLRootNode_2) then
    begin
      if NodeCopy.SmartChildrenCount = 0 then
      begin
        { If exception occurs before NodeCopy is connected to Result,
          NodeCopy should be simply freed. }
        FreeAndNil(NodeCopy);
        raise EVRMLPrototypeInstantiateError.CreateFmt(
          'Prototype "%s" has no nodes, cannot instantiate',
          [Proto.Name]);
      end;

      { ExtractChild/Item methods were really invented specially for this case.

        We have to remove Result from NodeCopy, to avoid cycles
        (that can cause mem leaks) because Result.FPrototypeInstanceHelpers
        has to keep pointer to NodeCopy.

        At the same time, Result must not be freed here because of ref count = 0... }
      Result := NodeCopy.SmartExtractChild(0);

      { Result.FPrototypeInstanceHelpers is used to keep the rest of
        NodeCopy.SmartChildren[1...] that should accompany this node. }
      Result.FPrototypeInstanceHelpers := NodeCopy;
    end else
      Result := NodeCopy;

    Result.NodeName := NodeName;

    try
      InstantiateReplaceIsClauses(nil, Result);
    except
      { Result.FPrototypeInstance is @false here, so make sure to free
        FPrototypeInstanceHelpers specifically. }
      FreeAndNil(Result.FPrototypeInstanceHelpers);
      FreeAndNil(Result);
      raise;
    end;

    { Note: set PrototypeInstance to @true *after* InstantiateReplaceIsClauses,
      otherwise InstantiateReplaceIsClauses would not enter this node. }
    Result.FPrototypeInstance := true;
    Result.FPrototypeInstanceSourceNode := Self;
  end;

  procedure InstantiateExternalPrototype(Proto: TVRMLExternalPrototype);
  var
    NodeExternalPrototype: TVRMLPrototypeNode;
    F: TVRMLField;
    FieldIndex, I: Integer;
  begin
    if Proto.ReferencedPrototype = nil then
      raise EVRMLPrototypeInstantiateError.CreateFmt(
        'External prototype "%s" cannot be loaded, so cannot instantiate nodes using it',
        [Proto.Name]);

    NodeExternalPrototype := TVRMLPrototypeNode.CreatePrototypeNode(NodeName,
      WWWBasePath, Proto.ReferencedPrototype);
    try
      for FieldIndex := 0 to Fields.Count - 1 do
      begin
        F := Fields[FieldIndex];
        I := NodeExternalPrototype.Fields.IndexOf(F.Name);
        if I <> -1 then
        begin
          { In case of type mismatch, FieldsAssignValue will make nice
            VRMLNonFatalError.

            Note that if F.IsClause, then F.IsClauseName will be copied.
            That's OK. This means that external prototype is expanded within
            other (more outside, non-external) prototype. So F.IsClauseName
            refers to this outside prototype. So F.IsClauseName should
            be copied to NodeExternalPrototype.Fields[I].IsClauseName,
            and this in turn will be eventually copied to fields inside
            the prototype expansion. }
          FieldsAssignValue(NodeExternalPrototype.Fields[I], F, false);
          NodeExternalPrototype.Fields[I].Exposed := F.Exposed;
        end else
          VRMLNonFatalError(Format(
            'Error when expanding external prototype "%s": ' +
            'prototype implementation doesn''t have field "%s"',
            [Proto.Name, F.Name]));
      end;

      { TODO: I should probably copy events somehow too }

      Result := NodeExternalPrototype.Instantiate;
    except
      { In case of exceptions, NodeExternalPrototype is to be freed.
        (In case of no exception, NodeExternalPrototype will also be
        freed, by FreeAndNil(Result.FPrototypeInstanceSourceNode)). }
      FreeAndNil(NodeExternalPrototype);
      raise;
    end;

    { We use NodeExternalPrototype to instantiate, but then we want
      Result.FPrototypeInstanceSourceNode to be Self. }
    FreeAndNil(Result.FPrototypeInstanceSourceNode);
    Result.FPrototypeInstanceSourceNode := Self;
  end;

begin
  if Prototype is TVRMLPrototype then
    InstantiateNonExternalPrototype(Prototype as TVRMLPrototype) else
  if Prototype is TVRMLExternalPrototype then
    InstantiateExternalPrototype(Prototype as TVRMLExternalPrototype) else
    raise EVRMLPrototypeInstantiateError.CreateFmt(
      'Cannot instantiate prototype "%s": '+
      'unknown prototype class %s', [Prototype.Name, Prototype.ClassName]);
end;

{ TVRMLPrototypeBase --------------------------------------------------------- }

constructor TVRMLPrototypeBase.Create;
begin
  inherited;
  FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
end;

destructor TVRMLPrototypeBase.Destroy;
begin
  FreeWithContentsAndNil(FInterfaceDeclarations);
  inherited;
end;

procedure TVRMLPrototypeBase.ParseInterfaceDeclarations(ExternalProto: boolean;
  Lexer: TVRMLLexer);
var
  I: TVRMLInterfaceDeclaration;
begin
  while Lexer.Token <> vtCloseSqBracket do
  begin
    I := TVRMLInterfaceDeclaration.Create(nil);
    InterfaceDeclarations.Add(I);

    if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(AllAccessTypes)) then
    begin
      I.Parse(Lexer, not ExternalProto, false);
    end else
      raise EVRMLParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
  end;

  { eat "]" token }
  Lexer.NextToken;

  FWWWBasePath := Lexer.WWWBasePath;
end;

procedure TVRMLPrototypeBase.Bind(ProtoNameBinding: TStringList);
var
  I: Integer;
begin
  I := ProtoNameBinding.IndexOf(Name);
  if I <> - 1 then
    ProtoNameBinding.Objects[I] := Self else
    ProtoNameBinding.AddObject(Name, Self);
end;

procedure TVRMLPrototypeBase.SaveInterfaceDeclarationsToStream(
  SaveProperties: TVRMLSaveToStreamProperties;
  ExternalProto: boolean);
var
  I: Integer;
begin
  SaveProperties.Writeln(Name + ' [');
  SaveProperties.IncIndent;
  for I := 0 to InterfaceDeclarations.Count - 1 do
  begin
    InterfaceDeclarations.Items[I].IDeclSaveToStream(
      SaveProperties, not ExternalProto);
  end;
  SaveProperties.DecIndent;
  SaveProperties.WritelnIndent(']');
end;

{ TVRMLPrototype ------------------------------------------------------------- }

destructor TVRMLPrototype.Destroy;
begin
  FreeAndNil(FNode);
  inherited;
end;

function ParseVRMLStatements(
  Lexer: TVRMLLexer;
  const EndToken: TVRMLToken;
  ParseX3DHeader: boolean): TVRMLNode; forward;

procedure TVRMLPrototype.Parse(Lexer: TVRMLLexer);
var
  OldNodeNameBinding: TStringList;
  OldProtoNameBinding: TStringList;
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(false, Lexer);

  Lexer.CheckTokenIs(vtOpenCurlyBracket);

  Lexer.NextToken;
  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside. So we create
    new NodeNameBinding for parsing prototype. }
  OldNodeNameBinding := Lexer.NodeNameBinding;
  Lexer.NodeNameBinding := TStringListCaseSens.Create;
  try
    { Also prototype name scope is local within the prototype,
      however it starts from current prototype name scope (not empty,
      like in case of NodeNameBinding). So prototypes defined outside
      are available inside, but nested prototypes inside are not
      available outside. }
    OldProtoNameBinding := Lexer.ProtoNameBinding;
    Lexer.ProtoNameBinding := TStringListCaseSens.Create;
    try
      Lexer.ProtoNameBinding.Assign(OldProtoNameBinding);
      FNode := ParseVRMLStatements(Lexer, vtCloseCurlyBracket, false);
    finally
      FreeAndNil(Lexer.ProtoNameBinding);
      Lexer.ProtoNameBinding := OldProtoNameBinding;
    end;
  finally
    FreeAndNil(Lexer.NodeNameBinding);
    Lexer.NodeNameBinding := OldNodeNameBinding;
  end;

  { consume last vtCloseCurlyBracket, ParseVRMLStatements doesn't do it }
  Lexer.NextToken;

  Bind(Lexer.ProtoNameBinding);
end;

procedure TVRMLPrototype.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
var
  OldNodeNameBinding: TStringList;
begin
  SaveProperties.WriteIndent('PROTO ');

  SaveInterfaceDeclarationsToStream(SaveProperties, false);

  { Inside prototype has it's own DEF/USE scope. }
  OldNodeNameBinding := SaveProperties.NodeNameBinding;
  SaveProperties.NodeNameBinding := TStringListCaseSens.Create;
  try
    SaveProperties.WritelnIndent('{');
    { Node may be TVRMLRootNode_* here, that's OK,
      TVRMLRootNode_*.SaveToStream will magically handle this right. }
    SaveProperties.IncIndent;
    Node.SaveToStream(SaveProperties);
    SaveProperties.DecIndent;
    SaveProperties.WritelnIndent('}');
  finally
    FreeAndNil(SaveProperties.NodeNameBinding);
    SaveProperties.NodeNameBinding := OldNodeNameBinding;
  end;
end;

{ TVRMLExternalPrototype ----------------------------------------------------- }

constructor TVRMLExternalPrototype.Create;
begin
  inherited;
  FURLList := TMFString.Create(nil, '', []);
end;

destructor TVRMLExternalPrototype.Destroy;
begin
  UnloadReferenced;
  FreeAndNil(FURLList);
  inherited;
end;

procedure TVRMLExternalPrototype.Parse(Lexer: TVRMLLexer);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(true, Lexer);

  URLList.Parse(Lexer, false);

  Bind(Lexer.ProtoNameBinding);

  LoadReferenced;
end;

procedure TVRMLExternalPrototype.SaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.WriteIndent('EXTERNPROTO ');

  SaveInterfaceDeclarationsToStream(SaveProperties, true);

  { SaveProperties.NodeNameBinding will be ignored by URLList
    (TMFString.SaveToStream), don't worry about it. }

  URLList.SaveToStream(SaveProperties);
end;

procedure TVRMLExternalPrototype.LoadReferenced;

  procedure LoadInterfaceDeclarationsValues;
  var
    IIndex: Integer;
    I: TVRMLInterfaceDeclaration;
    ReferencedField: TVRMLField;
  begin
    { We should load default values for our fields now,
      since InterfaceDeclaration of external prototype doesn't
      specify default values. }
    for IIndex := 0 to InterfaceDeclarations.Count - 1 do
    begin
      I := InterfaceDeclarations[IIndex];
      if I.Field <> nil then
      begin
        ReferencedField := ReferencedPrototype.InterfaceDeclarations.
          TryFindFieldName(I.Field.Name);
        if ReferencedField <> nil then
        begin
          try
            I.Field.AssignValue(ReferencedField);
          except
            on E: EVRMLFieldAssign do
            begin
              VRMLNonFatalError(Format(
                'Error when linking external prototype "%s" with prototype "%s": ',
                [Name, ReferencedPrototype.Name]) + E.Message);
            end;
          end;
        end else
          VRMLNonFatalError(Format('Prototype "%s" referenced by external ' +
            'prototype "%s" doesn''t have field "%s"',
            [ReferencedPrototype.Name, Name, I.Field.Name]));
      end;
    end;
  end;

var
  ProtoNameBinding: TStringList;

  function LoadFromExternalVRML(const RelativeURL: string): boolean;
  var
    URL: string;

    procedure ProtoNonFatalError(const S: string);
    begin
      VRMLNonFatalError(Format('Cannot load external prototype from URL "%s": ',
        [URL]) + S);
    end;

    { Find PROTO (but not EXTERNPROTO) with matching Name.
      Name is ignored if ''.
      @nil if not found. }
    function TryFindProtoNonExternal(const Name: string): TVRMLPrototype;
    var
      I: Integer;
    begin
      for I := 0 to ProtoNameBinding.Count - 1 do
        if ProtoNameBinding.Objects[I] is TVRMLPrototype then
        begin
          Result := TVRMLPrototype(ProtoNameBinding.Objects[I]);
          if (Name = '') or (Result.Name = Name) then
            Exit;
        end;
      Result := nil;
    end;

  var
    Anchor: string;
  begin
    Result := false;

    { TODO: in case of prototype libraries, it's wasteful to load URL each time
      for each external prototype. We should have some cache of prototypes. }

    URL := CombinePaths(WWWBasePath, RelativeURL);
    URLExtractAnchor(URL, Anchor);
    try
      ReferencedPrototypeNode := ParseVRMLFile(URL, false, ProtoNameBinding);
    except
      on E: Exception do
      begin
        ProtoNonFatalError(E.Message);
        Exit;
      end;
    end;

    FReferencedPrototype := TryFindProtoNonExternal(Anchor);
    if FReferencedPrototype = nil then
    begin
      FreeAndNil(ReferencedPrototypeNode);
      if Anchor = '' then
        ProtoNonFatalError('No PROTO found') else
        ProtoNonFatalError(Format('No PROTO named "%s" found', [Anchor]));
      Exit;
    end;

    Result := true;

    LoadInterfaceDeclarationsValues;
  end;

  function LoadFromURN(const URN: string): boolean;
  begin
    FReferencedClass := NodesManager.URNToClass(URN);
    Result := ReferencedClass <> nil;
    if not Result then
      VRMLNonFatalError(Format('Unknown node URN "%s"', [URN]));
  end;

var
  I: Integer;
  S: string;
  Loaded: boolean;
begin
  UnloadReferenced;

  ProtoNameBinding := TStringList.Create;
  try
    for I := 0 to URLList.Count - 1 do
    begin
      S := URLList.Items.Items[I];
      if IsPrefix('urn:', S) then
        Loaded := LoadFromURN(S) else
        Loaded := LoadFromExternalVRML(S);
      if Loaded then
        Break;
    end;
  finally FreeAndNil(ProtoNameBinding); end;
end;

procedure TVRMLExternalPrototype.UnloadReferenced;
begin
  { FReferencedPrototype will be freed as part of ReferencedPrototypeNode }
  FReferencedPrototype := nil;

  FreeAndNil(ReferencedPrototypeNode);

  FReferencedClass := nil;
end;

{ TNodesManager ------------------------------------------------------------ }

constructor TNodesManager.Create;
begin
  inherited;
  FRegistered := TStringListCaseSens.Create;
end;

destructor TNodesManager.Destroy;
begin
  FRegistered.Free;
  inherited;
end;

procedure TNodesManager.RegisterNodeClass(NodeClass: TVRMLNodeClass);
begin
  if NodeClass.ClassNodeTypeName = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassNodeTypeName so it cannot be registered in TNodesManager');

  if FRegistered.IndexOfObject(TObject(Pointer(NodeClass))) <> -1 then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+
      ' was already registered in TNodesManager');

  FRegistered.AddObject(NodeClass.ClassNodeTypeName, TObject(Pointer(NodeClass)));
end;

procedure TNodesManager.RegisterNodeClasses(
  const NodeClasses: array of TVRMLNodeClass);
var
  I: Integer;
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

  i := FRegistered.IndexOfObject(TObject(Pointer(NodeClass)));
  if i <> - 1 then
    FRegistered.Delete(i) else
  if ErrorIfNotRegistered then
    ENodesManagerError.Create('Node class "' + NodeClass.ClassName +
      '" was not registered, so you cannot unregister it');
end;

function TNodesManager.NodeTypeNameToClass(const ANodeTypeName: string;
  const VerMajor, VerMinor: Integer): TVRMLNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TVRMLNodeClass(FRegistered.Objects[I]);
    if (FRegistered[I] = ANodeTypeName) and
       Result.ForVRMLVersion(VerMajor, VerMinor) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.URNToClass(const URN: string): TVRMLNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TVRMLNodeClass(FRegistered.Objects[I]);
    if Result.URNMatching(URN) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.GetRegistered(Index: Integer): TVRMLNodeClass;
begin
  Result := TVRMLNodeClass(FRegistered.Objects[Index]);
end;

function TNodesManager.RegisteredCount: Cardinal;
begin
  Result := FRegistered.Count;
end;

{ TVRMLRoute ----------------------------------------------------------------- }

constructor TVRMLRoute.Create;
begin
  inherited;
  ResetLastEventTime;
end;

destructor TVRMLRoute.Destroy;
begin
  { We have to unset, to call
    DestructionNotifications.DeleteFirstEqual(...) on our nodes before
    we get destroyed. Otherwise nodes would have invalid references
    on DestructionNotifications list. }

  UnsetEnding(FSourceNode     , FSourceExposedField     , FSourceEvent     , false);
  UnsetEnding(FDestinationNode, FDestinationExposedField, FDestinationEvent, true);
  inherited;
end;

procedure TVRMLRoute.Parse(Lexer: TVRMLLexer);
var
  SourceNodeName, SourceEventName: string;
  DestinationNodeName, DestinationEventName: string;
begin
  { We don't use NextTokenForceVTName here, as then the dot "."
    is treated like part the VRML name. So this assumes that VRML node names
    are really correct VRML names. }

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  SourceNodeName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  SourceEventName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIsKeyword(vkTO);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  DestinationNodeName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  DestinationEventName := Lexer.TokenName;

  Lexer.NextToken;

  SetSource     (SourceNodeName     , SourceEventName     , Lexer.NodeNameBinding);
  SetDestination(DestinationNodeName, DestinationEventName, Lexer.NodeNameBinding);
end;

procedure TVRMLRoute.UnsetEnding(
  var Node: TVRMLNode; var ExposedField: TVRMLField; var Event: TVRMLEvent;
  const DestEnding: boolean;
  RemoveFromDestructionNotification: boolean);
begin
  if (Event <> nil) and (not DestEnding) then
    Event.OnReceive.DeleteFirstEqual(@EventReceive);

  if Node <> nil then
  begin
    if RemoveFromDestructionNotification then
      Node.DestructionNotifications.DeleteFirstEqual(@DestructionNotification);
    Node := nil;
  end;
  ExposedField := nil;
  Event := nil;
end;

procedure TVRMLRoute.EventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
begin
  Assert(Event = SourceEvent);

  { Follow ROUTE only when LastEventTime is older than current Time.
    This avoids loops in ROUTEs, following VRML / X3D specs. }
  if Time > LastEventTime then
  begin
    LastEventTime := Time;
    if DestinationEvent <> nil then
      DestinationEvent.Send(Value, Time);
  end else
  if Log then
    WritelnLog('VRMLRoute', Format(
      'Route from %s.%s ignored another event at <= timestamp (%f, while last event was on %f). Potential routes loop avoided',
      [ SourceNode.NodeName, SourceEvent.Name, Time, LastEventTime ]));
end;

procedure TVRMLRoute.ResetLastEventTime;
begin
  LastEventTime := -MaxDouble;
end;

type
  ERouteSetEndingError = class(EVRMLError);

procedure TVRMLRoute.SetEnding(const NodeName, FieldOrEventName: string;
  NodeNameBinding: TStringList;
  var Node: TVRMLNode; var ExposedField: TVRMLField; var Event: TVRMLEvent;
  const DestEnding: boolean);
const
  DestEndingNames: array [boolean] of string =
  ('source', 'destination');
var
  Index: Integer;
  FieldOrEvent: TVRMLFieldOrEvent;
begin
  UnsetEnding(Node, ExposedField, Event, DestEnding);

  try

    Index := NodeNameBinding.IndexOf(NodeName);
    if Index = -1 then
      raise ERouteSetEndingError.CreateFmt('Route %s node name "%s" not found',
        [ DestEndingNames[DestEnding], NodeName ]);

    Node := NodeNameBinding.Objects[Index] as TVRMLNode;
    Node.DestructionNotifications.AppendItem(@DestructionNotification);

    FieldOrEvent := Node.FieldOrEvent(FieldOrEventName, true);
    if FieldOrEvent = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s field/event name "%s" (for node "%s", type "%s") not found',
        [ DestEndingNames[DestEnding], FieldOrEventName, NodeName, Node.NodeTypeName ]);

    if FieldOrEvent is TVRMLField then
    begin
      ExposedField := TVRMLField(FieldOrEvent);
      if not ExposedField.Exposed then
        raise ERouteSetEndingError.CreateFmt('Route %s specifies field "%s" (for node "%s"), but this is not an exposed field (cannot generate/receive events)',
          [ DestEndingNames[DestEnding], FieldOrEventName, NodeName ]);
      Event := TVRMLField(FieldOrEvent).ExposedEvents[DestEnding];
    end else
    begin
      Assert(FieldOrEvent is TVRMLEvent);
      Event := TVRMLEvent(FieldOrEvent);
    end;

    if Event.InEvent <> DestEnding then
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: source of the route (%s, type %s) can only be input event, and destination (%s, type %s) can only be output event',
        [ SourceEvent     .Name, SourceEvent     .FieldClass.VRMLTypeName,
          DestinationEvent.Name, DestinationEvent.FieldClass.VRMLTypeName ]);

    if (SourceEvent <> nil) and
       (DestinationEvent <> nil) and
       (SourceEvent.FieldClass <> DestinationEvent.FieldClass) and
       { destination field can be XFAny (for Logger.write) as an exception. }
       (not (DestinationEvent.FieldClass = TVRMLField)) then
      raise ERouteSetEndingError.CreateFmt('Route has different event types for source (%s, type %s) and destination (%s, type %s)',
        [ SourceEvent     .Name, SourceEvent     .FieldClass.VRMLTypeName,
          DestinationEvent.Name, DestinationEvent.FieldClass.VRMLTypeName ]);

    if (Event <> nil) and (not DestEnding) then
      Event.OnReceive.AppendItem(@EventReceive);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, ExposedField, Event, DestEnding);
      VRMLNonFatalError(E.Message);
    end;
  end;
end;

procedure TVRMLRoute.SetSource(
  const SourceNodeName, SourceFieldOrEventName: string;
  NodeNameBinding: TStringList);
begin
  SetEnding(SourceNodeName, SourceFieldOrEventName,
    NodeNameBinding,
    FSourceNode, FSourceExposedField, FSourceEvent,
    false);
end;

procedure TVRMLRoute.SetDestination(
  const DestinationNodeName, DestinationFieldOrEventName: string;
  NodeNameBinding: TStringList);
begin
  SetEnding(DestinationNodeName, DestinationFieldOrEventName,
    NodeNameBinding,
    FDestinationNode, FDestinationExposedField, FDestinationEvent,
    true);
end;

type
  EVRMLRouteSaveError = class(EVRMLError);

procedure TVRMLRoute.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
var
  Output: string;

  procedure WriteEnding(Node: TVRMLNode; ExposedField: TVRMLField;
    Event: TVRMLEvent; const S: string);
  var
    Index: Integer;
  begin
    { Check Node }
    if Node = nil then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node not assigned (look for warnings when reading this VRML file)', [S]);
    if Node.NodeName = '' then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node not named', [S]);

    Index := SaveProperties.NodeNameBinding.IndexOf(Node.NodeName);
    if Index = -1 then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound',
        [S, Node.NodeName]);
    if SaveProperties.NodeNameBinding.Objects[Index] <> Node then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound (another node bound to the same name)',
        [S, Node.NodeName]);

    Output += Node.NodeName + '.';

    { Check Event }
    if Event = nil then
      raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s event not assigned', [S]);

    { Check ExposedField }
    if ExposedField = nil then
    begin
      if Event.Name = '' then
        raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s event not named', [S]);
      Output += Event.Name;
    end else
    begin
      if ExposedField.Name = '' then
        raise EVRMLRouteSaveError.CreateFmt('Cannot save VRML route: %s exposed field not named', [S]);
      Output += ExposedField.Name;
    end;
  end;

begin
  try
    Output := 'ROUTE ';
    WriteEnding(SourceNode     , SourceExposedField     , SourceEvent     , 'source'     );
    Output += ' TO ';
    WriteEnding(DestinationNode, DestinationExposedField, DestinationEvent, 'destination');
    SaveProperties.WritelnIndent(Output);
  except
    on E: EVRMLRouteSaveError do
    begin
      VRMLNonFatalError(E.Message);
    end;
  end;
end;

procedure TVRMLRoute.DestructionNotification(Node: TVRMLNode);
begin
  { UnsetEnding is called with RemoveFromDestructionNotification = false.
    Reason:
    1. Removing from DestructionNotification is not needed,
       since the Node is destroyed anyway, along with it's
       DestructionNotification list.
    2. Moreover, right now we probably iterate over DestructionNotification,
       so removing items from it is very bad idea (indexes shift,
       pointers change if reallocation occurs). }

  if Node = FSourceNode then
    UnsetEnding(FSourceNode     , FSourceExposedField     , FSourceEvent     , false, false);

  if Node = FDestinationNode then
    UnsetEnding(FDestinationNode, FDestinationExposedField, FDestinationEvent, true , false);
end;

{ global procedures ---------------------------------------------------------- }

{ Internal for ParseIgnoreToMatchingSqBracket and
  ParseIgnoreToMatchingCurlyBracket }
procedure ParseIgnoreToMatchingBracket(
  Lexer: TVRMLLexer;
  LevelSqBracket, LevelCurlyBracket: integer);
begin
  while (LevelSqBracket > 0) or
        (LevelCurlyBracket > 0) do
  begin
    case Lexer.Token of
      vtOpenCurlyBracket: Inc(LevelCurlyBracket);
      vtCloseCurlyBracket: Dec(LevelCurlyBracket);
      vtOpenSqBracket: Inc(LevelSqBracket);
      vtCloseSqBracket: Dec(LevelSqBracket);
      vtEnd: raise EVRMLParserError.Create(Lexer, 'Unexpected end of stream');
    end;
    Lexer.NextToken;
  end;
end;

(* Assume that we just read "[", and are looking at next character.
   Read everything up to (and including) matching "]".
   This is a hack to omit (not really parse) interface sections
   of prototypes. *)
procedure ParseIgnoreToMatchingSqBracket(Lexer: TVRMLLexer);
begin
  ParseIgnoreToMatchingBracket(Lexer, 1, 0);
end;

(* Just like ParseIgnoreToMatchingSqBracket, but here for "{" and "}" brackets.
   This is a hack to omit (not really parse) unknown VRML nodes. *)
procedure ParseIgnoreToMatchingCurlyBracket(Lexer: TVRMLLexer);
begin
  ParseIgnoreToMatchingBracket(Lexer, 0, 1);
end;

function ParseNode(Lexer: TVRMLLexer;
  NilIfUnresolvedUSE: boolean = false): TVRMLNode;

  procedure ParseNamedNode(const nodename: string);
  var
    NodeClass: TVRMLNodeClass;
    NodeTypeName: string;
    ProtoIndex: Integer;
    Proto: TVRMLPrototypeBase;
  begin
    Lexer.CheckTokenIs(vtName, 'node type');
    NodeTypeName := Lexer.TokenName;
    Lexer.NextToken;

    { VRML / X3D specifications say it is an error for a PROTO to have
      the same name as built-in node. I didn't found any definite
      statements about EXTERNPROTO names, although I vaguely remember
      some notes that EXTERNPROTO name should take precedence over
      built-in names. And this seems most sensible. Especially for
      nodes that were not available in older VRML / X3D editions and
      were declared (possibly slightly sensible) using EXTERNPROTOs,
      it's a good thing to use EXTERNPROTOs: they were probably most
      current with respect to given VRML file. Examples are CAD
      test files from
      http://www.web3d.org/x3d/content/examples/Basic/CAD/

      So we first search for NodeTypeName among protos.
      Only when this failed, we look at built-in nodes in NodesManager.
    }

    ProtoIndex := Lexer.ProtoNameBinding.IndexOf(NodeTypeName);
    if ProtoIndex <> -1 then
    begin
      Proto := Lexer.ProtoNameBinding.Objects[ProtoIndex] as TVRMLPrototypeBase;
      if (Proto is TVRMLExternalPrototype) and
         (TVRMLExternalPrototype(Proto).ReferencedClass <> nil) then
        Result := TVRMLExternalPrototype(Proto).ReferencedClass.Create(NodeName, '') else
        Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, '', Proto);
    end else
    begin
      NodeClass := NodesManager.NodeTypeNameToClass(NodeTypeName,
        Lexer.VRMLVerMajor, Lexer.VRMLVerMinor);
      if NodeClass <> nil then
      begin
        Result := NodeClass.Create(nodename, '');
      end else
      begin
        Result := TVRMLUnknownNode.CreateUnknown(nodename, '', NodeTypeName);
      end;
    end;

    Result.Parse(Lexer);

    if Result is TVRMLPrototypeNode then
    try
      Result := TVRMLPrototypeNode(Result).Instantiate;
    except
      on E: EVRMLPrototypeInstantiateError do
        { Just write E.Message and silence the exception.
          Result will simply remain as TVRMLPrototypeNode instance in this case. }
        VRMLNonFatalError(E.Message);
    end;

    { Cycles in VRML graph are bad, I think we all agree about this.
      They can cause traversing routines to loop infinitely
      (sure, this is avoidable easily by keeping a list of already visited nodes,
      but the time cost is bad...).
      Most importantly, they can cause memory leaks, since reference
      counting doesn't work when we have loops.

      My initial brilliant idea is that I can 100% avoid having cycles
      by simply doing Result.Bind *after* the node is parsed.
      This is trivially simple and works 100% to avoid cycles.
      Moreover, this is needed in the light of possible
        "Result := TVRMLPrototypeNode(Result).Instantiate"
      which means that before parsing, our Result is possibly not something
      that we wanted to insert to NodeNameBinding.

      So there I was, swimming in joy with ultra-simple and 100% correct
      solution. Until I found a problem with it...

      In VRML 2.0, there is a Script node. It has interface declarations.
      It's interface declaration may have SFNode type. And then it can
      do USE call. And the trouble: VRML allows this very special USE
      clause to create cycle. E.g. see
      ~/sources/openvrml/openvrml/models/jsTouch.wrl.

      TODO: Which means that this is too easy solution, and ultimately
      Result.Bind should be moved up, or I should invent some other more clever
      solution for this (I think that this is needed only for Script interface
      declarations, so it can be some pretty specific hack just to
      fix this...).

      For now, NilIfUnresolvedUSE successfully changes there special
      cycles in Script into mere VRMLNonFatalError, so they don't stop
      us from reading the rest of VRML file. And since we don't handle
      any scripting, this is not a problem in practice. But some day it'll
      have to be fixed... }

    Result.Bind(Lexer.NodeNameBinding);
  end;

var
  NodeName, S: string;
  i: integer;
begin
  Result := nil;
  try
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
           i := Lexer.NodeNameBinding.IndexOf(nodename);
           if i = -1 then
           begin
             S := Format('Incorrect USE clause: node name "%s" undefined',
               [NodeName]);
             if NilIfUnresolvedUSE then
             begin
               Result := nil;
               VRMLNonFatalError(S);
             end else
               raise EVRMLParserError.Create(Lexer, S);
           end else
             Result := TVRMLNode(Lexer.NodeNameBinding.Objects[i]);

           Lexer.NextToken;
          end;
        else raise EVRMLParserError.Create(Lexer,
               'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
       end;
     vtName: ParseNamedNode('');
     else raise EVRMLParserError.Create(Lexer,
            'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
    end;
  except FreeAndNil(Result); raise end;
end;

{ This parses a sequence of VRML statements: any number of nodes,
  (external) protypes, routes. This is good to use to parse whole VRML file,
  or a (non-external) prototype content.

  Returns a single VRML node. If there was exactly one statement
  and it was a node statement, returns this node. Otherwise,
  returns everything read wrapped in artifical TVRMLRootNode_1
  or TVRMLRootNode_2 instance. }
function ParseVRMLStatements(
  Lexer: TVRMLLexer;
  const EndToken: TVRMLToken;
  ParseX3DHeader: boolean): TVRMLNode;
var
  PositionInParent: Integer;

  { Create root group node, appropriate for current VRML version in Lexer. }
  function CreateRootNode: TVRMLNode;
  begin
    if TVRMLRootNode_1.ForVRMLVersion(
        Lexer.VRMLVerMajor, Lexer.VRMLVerMinor) then
    begin
      Result := TVRMLRootNode_1.Create('', Lexer.WWWBasePath);
      TVRMLRootNode_1(Result).ForceVersion := true;
      TVRMLRootNode_1(Result).ForceVersionMajor := Lexer.VRMLVerMajor;
      TVRMLRootNode_1(Result).ForceVersionMinor := Lexer.VRMLVerMinor;
    end else
    begin
      Result := TVRMLRootNode_2.Create('', Lexer.WWWBasePath);
      TVRMLRootNode_2(Result).ForceVersion := true;
      TVRMLRootNode_2(Result).ForceVersionMajor := Lexer.VRMLVerMajor;
      TVRMLRootNode_2(Result).ForceVersionMinor := Lexer.VRMLVerMinor;
    end;
  end;

  procedure ParseProfile;
  begin
    if Lexer.TokenIsKeyword(vkPROFILE) then
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D profile name');
      (Result as TVRMLRootNode_2).X3DProfile := Lexer.TokenName;

      Lexer.NextToken;
    end else
      { We allow PROFILE to be omitted, which is not really allowed by
        X3D spec. }
      VRMLNonFatalError('X3D PROFILE statement missing');
  end;

  procedure ParseComponents;
  var
    Name: string;
    Level: Integer;
  begin
    while Lexer.TokenIsKeyword(vkCOMPONENT) do
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D component name');
      Name := Lexer.TokenName;

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtColon);

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtInteger, 'X3D component level');
      Level := Lexer.TokenInteger;
      (Result as TVRMLRootNode_2).X3DComponentNames.AppendItem(Name);
      (Result as TVRMLRootNode_2).X3DComponentLevels.AppendItem(Level);

      Lexer.NextToken;
    end;
  end;

  procedure ParseMetas;
  var
    Key, Value: string;
  begin
    while Lexer.TokenIsKeyword(vkMETA) do
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtString, 'X3D meta key');
      Key := Lexer.TokenString;

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtString, 'X3D meta value');
      Value := Lexer.TokenString;
      (Result as TVRMLRootNode_2).X3DMetaKeys.AppendItem(Key);
      (Result as TVRMLRootNode_2).X3DMetaValues.AppendItem(Value);

      Lexer.NextToken;
    end;
  end;

  procedure ParseVRMLStatement;

    procedure ParseRouteInternal;
    var
      Route: TVRMLRoute;
    begin
      Route := TVRMLRoute.Create;
      Result.Routes.Add(Route);
      Route.Parse(Lexer);
      Route.PositionInParent := PositionInParent;
    end;

    { You can safely assume that current token is PROTO or EXTERNPROTO. }
    procedure ParseProtoStatement;
    var
      Proto: TVRMLPrototypeBase;
    begin
      if Lexer.TokenKeyword = vkPROTO then
        Proto := TVRMLPrototype.Create else
        Proto := TVRMLExternalPrototype.Create;
      Result.Prototypes.Add(Proto);
      Proto.Parse(Lexer);
      Proto.PositionInParent := PositionInParent;
    end;

    procedure ParseNodeInternal;
    var
      NewNode: TVRMLNode;
    begin
      NewNode := ParseNode(Lexer);
      NewNode.PositionInParent := PositionInParent;
      Result.SmartAddChild(NewNode);
    end;

  begin
    if (Lexer.Token = vtKeyword) and
       (Lexer.TokenKeyword = vkROUTE) then
      ParseRouteInternal else
    if (Lexer.Token = vtKeyword) and
       (Lexer.TokenKeyword in [vkPROTO, vkEXTERNPROTO]) then
      ParseProtoStatement else
      ParseNodeInternal;
  end;

begin
  { We used to have more conservative mechanism here when it comes to using
    CreateRootNode: we used to start with Result = nil.
    First ParseNodeInternal simply set the Result to it's contents.
    ParseProtoStatement, ParseRouteInternal and second and subsequent
    ParseNodeInternal calls called CreateRootNode, and added previous
    content as new root node child.

    This was an unnecessarily complicated mechanism. The history is that
    initially, for correct VRML 1.0, the whole concept of RootGroup was
    not needed. It became needed since VRML 2.0, to
    - store multiple root nodes of VRML 2.0 file
    - and many VRML 1.0 files also used (incorrectly) the "multiple root nodes"
      feature
    - also, VRML 2.0 routes and prototypes declared at global scope had
      to be stored in a root group
    - Finally, ForceVersion mechanism is available for RootGroup to force
      writing of specific VRML version.
    - And for X3D, TVRMLRootNode_2 has to be enhanced
      to keep other top-level X3D data: profile, components, meta statements.

    So now, we simply always create root node by ParseVRMLStatements
    (although the interface should not guarantee this, in case I'll change
    my mind later). }

  Result := CreateRootNode;
  try
    { Parse X3D "configuration information": profile, component and meta
      statements here. }
    if ParseX3DHeader and (Lexer.VRMLVerMajor >= 3) then
    begin
      ParseProfile;
      ParseComponents;
      ParseMetas;
    end;

    PositionInParent := 0;

    while Lexer.Token <> EndToken do
    begin
      ParseVRMLStatement;
      Inc(PositionInParent);
    end;
  except FreeAndNil(Result); raise end;
end;

function ParseVRMLFile(Stream: TPeekCharStream;
  const WWWBasePath: string;
  ProtoNameBinding: TStringList): TVRMLNode;
var
  Lexer: TVRMLLexer;
begin
  Lexer := TVRMLLexer.Create(Stream, false, WWWBasePath);
  try
    Result := ParseVRMLStatements(Lexer, vtEnd, true);
    if ProtoNameBinding <> nil then
      ProtoNameBinding.Assign(Lexer.ProtoNameBinding);
  finally
    Lexer.Free;
  end;
end;

function ParseVRMLFile(const FileName: string; AllowStdIn: boolean;
  ProtoNameBinding: TStringList): TVRMLNode;

  function DoIt(BaseStream: TStream; FreeBaseStream: boolean;
    const WWWBasePath: string): TVRMLNode;
  var
    Stream: TPeekCharStream;
  begin
    Stream := TBufferedReadStream.Create(BaseStream, FreeBaseStream);
    try
      Result := ParseVRMLFile(Stream, WWWBasePath, ProtoNameBinding);
    finally Stream.Free end;
  end;

var
  WWWBasePath: string;
begin
  if AllowStdIn and (FileName = '-') then
    Result := DoIt(StdInStream, false, GetCurrentDir) else
  begin
    WWWBasePath := ExtractFilePath(ExpandFilename(FileName));

    if SameText(ExtractFileExt(FileName), '.gz') or
       SameText(ExtractFileExt(FileName), '.wrz') then
      Result := DoIt(TGZFileStream.Create(FileName, gzOpenRead), true,
        WWWBasePath) else
    begin
      {$ifdef VER2_0_4}
        {$ifdef WIN32}
        { This is a temporary workaround for FPC 2.0.4 bug (already fixed
          in 2.1.4 and current trunk (2007-09-05).
          When opening non-existent file on Windows (on Unixes it's Ok)
          by TFileStream.Create, no exception is raised, instead read procedures
          behave like it was an empty file. This is a minor problem, because
          error message then sound like nonsense for the user: E.g.
            view3dscene not_existing_file.wrl
          says "VRML lexical error at position 0 : Unexpected end of file on the 1st line",
          while it should simply say that file doesn't exist.

          Of course this workaround isn't perfect (file may disappear between
          FileExists check and actual open, file permissions may not allow to read
          etc., that's why usually it's best to left such checks for OS routines called
          by TFileStream constructor) but it should work in usual cases. }
        if not FileExists(FileName) then
          raise EFOpenError.CreateFmt('Unable to open file "%s"', [FileName]);
        {$endif}
      {$endif}

      try
        Result := DoIt(TFileStream.Create(FileName, fmOpenRead), true, WWWBasePath);
      except
        { It's somewhat hacky solution to reopen the file, creating new stream.
          But in practice, this works OK, and it allows us to read files
          compressed by gzip but without indicating this by file extension.
          The other idea would be to pipe the TFileStream through something
          like TDecompressionStream, but

          1. I simply prefer to not filter through yet another stream
             class (we already wrap TFileStream in TBufferedReadStream)
             when I don't have to.

          2. TDecompressionStream doesn't handle gzip compressed data for now
             (it handles zlib-compressed streams, which is not the same thing). }
        on EVRMLGzipCompressed do
          Result := DoIt(TGZFileStream.Create(FileName, gzOpenRead), true, WWWBasePath);
      end;
    end;
  end;
end;

function ParseVRMLFileFromString(const VRMLContents: string;
  const WWWBasePath: string): TVRMLNode; overload;
var
  Stream: TPeekCharStream;
begin
  Stream := TSimplePeekCharStream.Create(
    TStringStream.Create(VRMLContents), true);
  try
    Result := ParseVRMLFile(Stream, WWWBasePath);
  finally FreeAndNil(Stream) end;
end;

procedure SaveToVRMLFile(Node: TVRMLNode; Stream: TStream;
  const PrecedingComment: string;
  WriteExpandedPrototype: boolean);
var
  SaveProperties: TVRMLSaveToStreamProperties;

  procedure SaveProfile;
  begin
    if (Node is TVRMLRootNode_2) and
       (TVRMLRootNode_2(Node).X3DProfile <> '') then
      SaveProperties.Writeln('PROFILE ' + TVRMLRootNode_2(Node).X3DProfile + NL) else
      { Just output some profile, as every X3D file should have this.
        We actually don't detect for now in any way which profile
        would be best. }
      SaveProperties.Writeln('PROFILE Interchange' + NL);
  end;

  procedure SaveComponents;
  var
    I: Integer;
  begin
    if Node is TVRMLRootNode_2 then
      for I := 0 to TVRMLRootNode_2(Node).X3DComponentNames.Count - 1 do
      begin
        SaveProperties.Writeln(Format('COMPONENT %s:%d' + NL,
          [ TVRMLRootNode_2(Node).X3DComponentNames[I],
            TVRMLRootNode_2(Node).X3DComponentLevels[I] ]));
      end;
  end;

  procedure SaveMetas;
  var
    I: Integer;
  begin
    if Node is TVRMLRootNode_2 then
      for I := 0 to TVRMLRootNode_2(Node).X3DMetaKeys.Count - 1 do
      begin
        SaveProperties.Writeln(Format('META %s %s' + NL,
          [ StringToVRMLStringToken(TVRMLRootNode_2(Node).X3DMetaKeys[I]),
            StringToVRMLStringToken(TVRMLRootNode_2(Node).X3DMetaValues[I])]));
      end;
  end;

const
  VRML10Header = '#VRML V1.0 ascii';
  VRML20Header = '#VRML V2.0 utf8';
  X3DHeader = '#X3D V%d.%d utf8';
var
  VerMajor, VerMinor, SuggestionPriority: Integer;
  VRMLHeader: string;
begin
  SaveProperties := TVRMLSaveToStreamProperties.Create(Stream);
  try
    SaveProperties.WriteExpandedPrototype := WriteExpandedPrototype;

    if not Node.SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority) then
    begin
      { If nothing is suggested, we use X3D 3.2. Reason:
        - For now, SuggestedVRMLVersion doesn't check IsClause.
          But if IsClause is present anywhere, then this must use VRML >= 2.0
          features ("IS" is keyword only in VRML >= 2.0,
          it will not be understood in VRML 1.0).
        - Besides, we should promote newer VRML standard. }

      VerMajor := 3;
      VerMinor := 2;
    end;

    if VerMajor <= 1 then
      VRMLHeader := VRML10Header else
    if VerMajor = 2 then
      VRMLHeader := VRML20Header else
    if VerMajor >= 3 then
      VRMLHeader := Format(X3DHeader, [VerMajor, VerMinor]);

    SaveProperties.VerMajor := VerMajor;
    SaveProperties.VerMinor := VerMinor;

    SaveProperties.Writeln(VRMLHeader + NL { yes, one more NL, to look good });
    if PrecedingComment <> '' then
      SaveProperties.Writeln('# '+PrecedingComment + NL);

    if VerMajor >= 3 then
    begin
      SaveProfile;
      SaveComponents;
      SaveMetas;
    end;

    { Node may be TVRMLRootNode_* here, that's OK,
      TVRMLRootNode_*.SaveToStream will magically handle this right. }
    Node.SaveToStream(SaveProperties);
  finally FreeAndNil(SaveProperties) end;
end;

procedure SaveToVRMLFile(Node: TVRMLNode;
  const Filename, PrecedingComment: string;
  WriteExpandedPrototype: boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToVRMLFile(Node, Stream, PrecedingComment, WriteExpandedPrototype);
  finally Stream.Free end;
end;

procedure TraverseState_CreateNodes(var StateNodes: TTraverseStateLastNodes);
var
  I: Integer;
begin
  for I := 0 to HighTraverseStateLastNodes do
    StateNodes.Nodes[I] := TraverseStateLastNodesClasses[i].Create('', '');
end;

procedure TraverseState_FreeAndNilNodes(var StateNodes: TTraverseStateLastNodes);
var
  I: Integer;
begin
  for I := 0 to HighTraverseStateLastNodes do
    FreeAndNil(StateNodes.Nodes[i]);
end;

function VRMLNodeDeepCopy(SourceNode: TVRMLNode): TVRMLNode;

  procedure Rewrap(WrapperClass: TVRMLNodeClass);
  var
    NewResult: TVRMLNode;
  begin
    NewResult := WrapperClass.Create('', '');
    NewResult.SmartAddChild(Result);
    Result := NewResult;
  end;

var
  S: TMemoryStream;
  SPeek: TPeekCharStream;
  NewResult: TVRMLNode;
begin
  S := TMemoryStream.Create;
  try
    { TODO: this implementation works 100% correctly, but,
      in case you didn't notice, it's a terrible hack actually,
      it can be made dramatically faster
      and less resource hungry. It saves SourceNode to stream, then loads
      new node hierarchy from this stream...

      We should instead copy VRML hierarchy by doing nice traverse of the graph.
      See TVRMLGLAnimation.Load implementation, when merging VRML nodes
      this also iterates everything that should be copied.
    }

    { It's crucial that we use WriteExpandedPrototype = @true here. Otherwise:

      1. We would cause unnecessary expanding of prototypes.
         SourceNode may have prototypes already
         expanded, but when saving it to stream, SaveToStream would
         discard the expanded version and save the prototype call instead.
         Which means that ParseVRMLFile will have to expand it again...

         This may hurt performance badly in case of nested large prototypes,
         as prototype Instantiate itself uses VRMLNodeDeepCopy.

      2. Moreover, this can cause actual errors, see e.g.
         kambi_vrml_test_suite/vrml_2/proto_nested.wrl test.
         Reason: inside a prototype, we may use a prototype defined at higher
         level. Which means that this prototype definition will not be
         recorded by SaveToVRMLFile below... but it we will write unexpanded
         prototype, this definition will be needed by ParseVRMLFile. }
    SaveToVRMLFile(SourceNode, S, '', true);

    S.Position := 0;
    SPeek := TBufferedReadStream.Create(S, false);
    try
      Result := ParseVRMLFile(SPeek, SourceNode.WWWBasePath);

      { SaveToVRMLFile always strips TVRMLRootNode_* wrapper.
        ParseVRMLFile adds TVRMLRootNode_* wrapper,
        currently always.

        We want to guarantee that Result has a wrapper if and only if
        SourceNode has a wrapper. So we optionally strip the wrapper
        added by ParseVRMLFile. }
      if (not Supports(SourceNode, IVRMLRootNode)) and
         Supports(Result, IVRMLRootNode) then
      begin
        Assert(Result.SmartChildrenCount = 1);
        NewResult := Result.SmartExtractChild(0);
        FreeAndNil(Result);
        Result := NewResult;
      end;

      Assert(
        Supports(SourceNode, IVRMLRootNode) =
        Supports(Result, IVRMLRootNode) );

      Assert(SourceNode.ClassType = Result.ClassType);
    finally FreeAndNil(SPeek) end;
  finally FreeAndNil(S) end;
end;

{ TDynNodeDestructionNotifications ------------------------------------------- }

procedure TDynNodeDestructionNotificationArray.ExecuteAll(Node: TVRMLNode);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
    Items[I](Node);
end;

{ TNodeNameBinding ----------------------------------------------------------- }

constructor TNodeNameBinding.Create;
begin
  inherited;
  AnyNodeDestructionNotifications.AppendItem(@DestructionNotification);
end;

destructor TNodeNameBinding.Destroy;
begin
  AnyNodeDestructionNotifications.DeleteFirstEqual(@DestructionNotification);
  inherited;
end;

procedure TNodeNameBinding.DestructionNotification(Node: TVRMLNode);
var
  I: Integer;
begin
  I := IndexOfObject(Node);
  if I >= 0 then
    Delete(I);
end;

{ unit init/fini ------------------------------------------------------------ }

initialization
  AnyNodeDestructionNotifications := TDynNodeDestructionNotificationArray.Create;

  VRMLFieldsManager.RegisterClasses([TSFNode, TMFNode]);

  NodesManager := TNodesManager.Create;

  RegistedInventorNodes;
  RegisterVRML1Nodes;
  RegisterVRML97Nodes;
  RegisterKambiNodes;
  RegisterAvalonNodes;

  { X3D components registration : }

  RegisterCoreNodes;
  RegisterTimeNodes;
  RegisterNetworkingNodes;
  RegisterGroupingNodes;
  RegisterRenderingNodes;
  RegisterShapeNodes;
  RegisterGeometry3DNodes;
  RegisterGeometry2DNodes;
  RegisterTextNodes;
  RegisterSoundNodes;
  RegisterLightingNodes;
  RegisterTexturingNodes;
  RegisterInterpolationNodes;
  RegisterPointingDeviceSensorNodes;
  RegisterKeyDeviceSensorNodes;
  RegisterEnvironmentalSensorNodes;
  RegisterNavigationNodes;
  RegisterEnvironmentalEffectsNodes;
  RegisterGeospatialNodes;
  RegisterHAnimNodes;
  RegisterNURBSNodes;
  RegisterDISNodes;
  RegisterScriptingNodes;
  RegisterEventUtilitiesNodes;
  RegisterShadersNodes;
  RegisterCADGeometryNodes;
  RegisterTexturing3DNodes;
  RegisterCubeMapTexturingNodes;
  RegisterLayeringNodes;
  RegisterLayoutNodes;
  RegisterRigidBodyPhysicsNodes;
  RegisterPickingNodes;
  RegisterFollowersNodes;
  RegisterParticleSystemsNodes;
finalization
  FreeAndNil(NodesManager);
  FreeAndNil(AnyNodeDestructionNotifications);
end.
