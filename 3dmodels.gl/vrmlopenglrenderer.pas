{
  Copyright 2002-2009 Michalis Kamburelis.

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

{ @abstract(The @link(TVRMLOpenGLRenderer) class, responsible for
  rendering VRML elements.)

  The overview of this class can also be found in my master's thesis
  [http://vrmlengine.sourceforge.net/vrml_engine_doc.php]
  in chapter "OpenGL rendering", section "Basic OpenGL rendering".
  You should read that description first --- the text below only
  adds some details.

  For this class, a VRML scene is just a sequence of pairs
  (Node: TVRMLGeometryNode; State: TVRMLGraphTraverseState).
  To render a sequence of such pairs you should:

  @orderedList(
    @item(
      Call @link(TVRMLOpenGLRenderer.Prepare) for all states
      that you want to later render. The order of calling Prepare
      methods doesn't matter, also you are free to prepare states that you
      will not actually use later. Of course a state, once prepared,
      may be used in rendering as many times as you want.

      It's important that you will prepare @italic(every state that
      you want to render later). And when rendring the state
      must have exactly the same (fields, properties) values as when
      it was prepared. This includes that it must have the same
      pointers to nodes Last*/Active* and their contents
      also must be the same. In particular, Prepare
      may save some associations between objects and OpenGL resources,
      so it's important that the same pointer must always point to the
      same object (until it's unprepared).

      (Below is in Polish, sorry. See my master's thesis for
      English summary of this documentation.)

      Prepare wymaga aktywnego kontekstu OpenGL'a. Prepare
      nie modyfikuje aktualnego stanu OpenGL'a poza tym ze moze zajac
      jakies display listy i indeksy tekstur OpenGL'a.
      Prepare nie moze byc wywolane jako czesc display-listy.

      Po wywolaniu Prepare (i pomiedzy kolejnymi wywolaniami Prepare)
      mozesz sobie spokojnie robic z OpenGL'em co ci sie zywnie podoba.
      Pamietaj tylko aby nie dotykac zajetych juz display-list i indeksow tekstury.
    )

    @item(
      Jezeli zamierzasz zmienic zawartosc pol jakiegos node'a ktory trafil
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
    )

    @item(
      w jakis czas potem wywolaj RenderBegin. To zainicjuje stan OpenGL'a
      na taki jakiego bedziemy pozniej potrzebowac. Od momentu wywolania
      RenderBegin stan OpenGL'a jest we wladzy tego obiektu : nie wolno
      ci go modyfikowac w zaden sposob niz poprzez ten obiekt.
    )

    @item(
      potem wywoluj
        RenderShapeLights(LightsRenderer, State)
        RenderShape(Geometry, State)
      aby renderowac pary Geometry+State.
      Jak juz powiedzialem, kazda podawana teraz para musiala wystapic
      wczesniej w wywolaniu Prepare.

      Alternatively instead of RenderShape you can call
        RenderShapeBegin,
        RenderShapeNoTransform,
        RenderShapeEnd (always in this sequence,
      always RenderShapeEnd in the "finally" clause, so that
      after RenderShapeBegin there is always RenderShapeEnd
      called). This is equivalent to RenderShape
      (but sometimes it's better as it allows you to place
      RenderShapeNoTransform on a separate display list, that can be more
      shared (because it doesn't take some transformations into account)).

      Make sure that VRML2ActiveLights are properly initialized if you
      plan to render VRML 2.0 nodes. TVRMLScene and descendants do
      this for you usually.
    )

    @item(
      potem wywolaj RenderEnd. Po wywolaniu RenderEnd bedziesz mial
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
    )

    @item(
      mozesz powtarzac ten scenariusz na dowolne sposoby dowolnie wiele razy.
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
    )
  )

  @bold(Jeszcze slowko o kontekstach OpenGL'a :)

  Create nie wymaga zadnego
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

  @bold(Pewne notki w kwestii stanu OpenGL'a i renderowania sceny VRML'a :)

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

  @unorderedList(
    @item(aktualna macierz MODELVIEW i PROJECTION i TEXTURE)

    @item(glLightModel (GL_LIGHT_MODEL_AMBIENT)

      (notka: specyfikacja VRMLa 1.0 podaje domyslne LIGHT_MODEL_AMBIENT
      0.2, 0.2, 0.2 co jest akurat zgodne z domyslnym stanem OpenGL'a.
      Specyfikacja lighting VRMLa 97 (z ktora probujemy byc zgodni,
      nawet w VRMLu 1.0) podaje z kolei rownania swiatla takie ze wynika z nich
      ze LIGHT_MODEL_AMBIENT powinno byc zerowe.))

    @item(glPolygonMode

      (oczywiscie, zeby wszystko bylo normalne powinienes
      uzywac GL_FRONT_AND_BACK, GL_FILL. Ale przelaczanie sie na model
      wireframe moze byc bardzo pouczajace zeby zobaczyc jak model
      jest skostruowany i jak jest wyswietlany))

    @item(
      stan enabled GL_LIGHTING (sa specjalne sytuacje gdy material w VRMLu
      okresla juz precalculated color - wtedy LIGHTING moze byc chwilowo
      wylaczone i kolory obiektow beda brane z EmissionColor;
      ale normalnie, LIGHTING OpenGL'a bedzie pozostawione w takim stanie
      w jakim bylo w momencie wejscia w RenderBegin i OpenGL dostanie
      informacje ktore pozwola mu wyrenderowac wszystko ladnie bez wzgledu
      na to czy ma czy nie ma wlaczone LIGHTING))

    @item(
      glDepthMask, stan enabled GL_BLEND i glBlendFunc
      (te stany nie sa tu kontrolowane, choc zapewne kod zewnetrzny
      (tzn. kod uzywajacy tego renderera) moze  zechciec ustawic
      te wartosci na cos konkretnego. Manipulujac tymi wartosciami kod
      zewnetrzny moze zaimplementowac obiekty polprzezroczyste uzywajac
      blending OpenGLa, przykladowo tak robi VRMLGLScene))

    @item(GL_FOG_HINT is also not manipulated by this unit.
      Just like for any other OpenGL program, you may want to set this
      to GL_NICEST (if you have to render models that may look bad
      when fog is interpolated without perspective correction).)

    @item(glFrontFace is assumed to be CCW (OpenGL default) but not manipulated
      by this unit anywhere.

      So our normals passed to OpenGL always point from CCW side.
      Even if you supplied in VRML file normals pointing from CW
      (indicated e.g. by IndexedFaceSet.ccw = FALSE field in VRML 97),
      we will internally invert them and pass inverted ones to OpenGL.
      And when culling faces, we switch using @code(glCullFace(
      GL_BACK / GL_FRONT)), not by switching front face.

      Why so much work was done to always work with front face = CCW assumption ?
      Because this is very handy when you render mirrors by using
      @code(Scale(1, 1, -1)) trick. See
      [http://www.opengl.org/resources/code/samples/mjktips/Reflect.html]
      and example program
      @code(kambi_vrml_game_engine/3dmodels.gl/examples/plane_mirror_and_shadow.pasprogram).
      With such strange scale, CCW and CW invert places. Sides that were
      CCW normally are now CW. This means that you want to call @code(glFrontFace(GL_CW))
      temporarily when rendering scene in the mirror. This way scene in the mirror
      will have correct normals and backface culling.

      Since we don't touch @code(glFrontFace) anywhere, this is possible to you.
      And you can reuse display lists etc. for the scene in the mirror.
    )
  )

  @bold(Notki o Triagles/VerticesCount :)

  Poniewaz OpenGL oferuje tylko cieniowanie
  Gourauda (no i plaskie cieniowanie, w zaleznosci od Attributes.SmoothShading),
  idea OverTriangulate = true w VRMLNodes.TVRMLGeometryNode.[Local]Triangulate
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

  @bold(About OpenGL extensions :)

  You should always call LoadAllExtensions before using this unit.
  This unit may use various OpenGL extensions and check OpenGL version.
  If you initialize OpenGL context using our GLWindow unit or
  TKamOpenGLControl then this will be done for you automatically during
  GL context initialization.
}

unit VRMLOpenGLRenderer;

{$I openglmac.inc}

{ When you define USE_VRML_NODES_TRIANGULATION, an alternative
  rendering method will be used. Each node will be triangulated
  using TVRMLGeometryNode.LocalTriangulate and then this triangle
  will be passed to OpenGL.

  This is a proof-of-concept implementation that shows that
  using TVRMLGeometryNode.LocalTriangulate we can render all
  nodes in the same manner --- no need to write separate rendering
  routines for various TVRMLGeometryNode descendants.
  All you have to do is to implement triangulating.

  This is mainly for testing purposes, it allows you to test
  LocalTriangulate and allows you to render nodes that don't have
  specialized rendering procedure done yet. It has a couple of
  practical disadvantages:
  1) It's slower, and always will be, than dedicated rendering
     procedures for each node.
  2) Things that are not expressed as triangles
     (IndexedLineSet, PointSet) will not be rendered at all.
  3) It lacks some features, because the triangulating routines
     do not return enough information. For example, textures
     are not applied (texture coords are not generated),
     flat shading is always used (because each triangle has
     always the same normal vector).
     This disadvantage could be removed (by extending information
     that triangulate method returns for each node),
     but it will always be non-optimal anyway --- see point 1) above.
}
{ $define USE_VRML_NODES_TRIANGULATION}

{$ifdef USE_VRML_NODES_TRIANGULATION}
  {$ifdef RELEASE}
    {$fatal Undefine USE_VRML_NODES_TRIANGULATION
      for VRMLOpenGLRenderer ---
      you don't want to use this in RELEASE version. }
  {$endif}
{$endif}

interface

uses
  Classes, SysUtils, KambiUtils, VectorMath, GL, GLU, GLExt,
  VRMLFields, VRMLNodes, VRMLLexer, Boxes3d, OpenGLTTFonts, Images,
  OpenGLFonts, KambiGLUtils, VRMLLightSetGL, TTFontsTypes,
  VRMLErrors, VideosCache, GLShaders, GLImages, Videos, VRMLTime, VRMLShape,
  GLCubeMap;

{$define read_interface}

const
  DefaultBumpMappingLightAmbientColor: TVector4Single = (0, 0, 0, 1);
  DefaultBumpMappingLightDiffuseColor: TVector4Single = (1, 1, 1, 1);

type
  TBeforeGLVertexProc = procedure (Node: TVRMLGeometryNode;
    const Vert: TVector3Single) of object;

  TRadianceTransferFunction = function (Node: TVRMLGeometryNode;
    RadianceTransfer: PVector3Single;
    const RadianceTransferCount: Cardinal): TVector3Single of object;

  { Callback used by TVRMLRendererAttributes.OnVertexColorFunction.
    Passed here VertexPosition is in local coordinates (that is,
    local of this object, multiply by State.Transform to get scene coords).
    VertexIndex is the direct index to Node.Coordinates. }
  TVertexColorFunction = procedure (var Color: TVector3Single;
    Shape: TVRMLShape; const VertexPosition: TVector3Single;
    VertexIndex: Integer) of object;

  { Various bump mapping methods. Generally sorted from worst one
    (bmNone, which does no bump mapping) to the best.
    Which one is chosen is determined at runtime, based on OpenGL capabilities,
    and on TVRMLRenderingAttributes.BumpMappingMaximum. }
  TBumpMappingMethod = (
    { No bump mapping done. }
    bmNone,

    { Most primitive "dot" bump mapping using multitexturing.
      Requires 2 texture units.

      Light position in tangent space is calculated each time we render the
      scene. This is important if you change
      TVRMLOpenGLRenderer.BumpMappingLightPosition: with bmMultiTex*,
      after changing BumpMappingLightPosition you have to render the scene
      using TVRMLOpenGLRenderer again. Which means that display lists
      built by TVRMLGLScene cannot be reused.

      If you use TVRMLGLScene this means that either:

      @unorderedList(
        @item(You use optimizations with display lists, like roSceneAsAWhole.
          Then changing BumpMappingLightPosition is very costly operation
          (display lists must be rebuild), so you should not do this
          e.g. every frame.)

        @item(Or you can use optimization roNone. Then changing
          BumpMappingLightPosition is not costly. But, roNone generally means
          "no optimization" and the whole rendering of your model may suffer...
          In other words, this solution is suitable only if your model is
          relatively simple, such that rendering it with roNone is Ok.)) }
    bmMultiTexDotNotNormalized,

    { Dot product bump mapping using multitexturing
      (with normalization using cube map).
      Requires 3 texture units.

      This is very similar to previous method (bmMultiTexDotNotNormalized), but
      normalizes normals at each pixel by special cube map. This makes better
      effect, but means that 1 more texture unit is required.

      All other comments about bmMultiTexDotNotNormalized apply also here.
      In particular, if you use TVRMLGLScene then either
      @orderedList(
        @itemSpacing compact
        @item don't modify BumpMappingLightPosition too often or
        @item use roNone optimization.
      ) }
    bmMultiTexDotNormalized,

    { Normal (calculate light by "dot" per pixel) bump mapping using GLSL
      shader.

      This requires OpenGL that supports GLSL. It's highly superior over
      previous (multitexturing without GLSL) methods, since it's more flexible
      and also changing it's parameters doesn't require to regenerate display
      lists.

      @orderedList(
        @item(Light position in tangent space is calculated by shader program.
          In short, this means that you can use good renderer optimization
          (like roSceneAsAWhole) even if you change BumpMappingLightPosition
          every frame. )

        @item(Previous methods generally break features such as ambient (as they
          modulate per-pixel lighting like
          (lighting_per_pixel * material * original_texture),
          while it should be (ambient + lighting_per_pixel * material) *
          original_texture). Actually, other methods mostly ignore
          material properties.

          This method honours material ambient and diffuse properties
          like it should. )

        @item(This honours properties like BumpMappingLightAmbientColor,
          BumpMappingLightDiffuseColor,
          so you can control the light more like normal OpenGL light.)
      ) }
    bmGLSLNormal,

    { This is like bmGLSLNormal, but additionally (if the heightMap of the surface
      is available) this will do parallax mapping.
      Steep parallax mapping with self-shadowing,
      if supported by hardware, otherwise classic parallax mapping
      (with offset limiting, i.e. with E.z component removed).

      Parallax mapping, in short, means that the texture coordinate is perturbed,
      based on texture heightMap topology and camera direction, to create
      illusion of 3D shape instead of flat texture.
      This makes e.g. the bricks on the texture really
      visible as "standing out", in 3D, from the wall. And self-shadowing
      means that these bricks even cast appropriate shadows on each other. }
    bmGLSLParallax);

const
  DefaultBumpMappingMaximum = bmNone;

type
  { These are various properties that control rendering done
    with @link(TVRMLOpenGLRenderer).

    They are collected here,
    in a class separate from @link(TVRMLOpenGLRenderer),
    because various things (like TVRMLGLScene and TVRMLGLAnimation)
    wrap @link(TVRMLOpenGLRenderer) instances and hide it,
    but still they want to allow user to change these attributes. }
  TVRMLRenderingAttributes = class(TPersistent)
  private
    FOnBeforeGLVertex: TBeforeGLVertexProc;
    FOnRadianceTransfer: TRadianceTransferFunction;
    FOnVertexColor: TVertexColorFunction;
    FSmoothShading: boolean;
    FColorModulatorSingle: TColorModulatorSingleFunc;
    FColorModulatorByte: TColorModulatorByteFunc;
    FUseLights: boolean;
    FFirstGLFreeLight: Cardinal;
    FLastGLFreeLight: integer;
    FControlMaterials: boolean;
    FControlTextures: boolean;
    FEnableTextures: boolean;
    FFirstGLFreeTexture: Cardinal;
    FLastGLFreeTexture: integer;
    FTextureMinFilter: TGLint;
    FTextureMagFilter: TGLint;
    FPointSize: TGLFloat;
    FUseFog: boolean;
    FBumpMappingMaximum: TBumpMappingMethod;
    FGLSLShaders: boolean;
    FPureGeometry: boolean;
  protected
    { In this class these methods just set value on given property.
      In descendants you can do something more here, like automatic
      calling UnprepareAll of related TVRMLOpenGLRenderer
      (this is not done here, as this would be dangerous ---
      caller must be aware that TVRMLOpenGLRenderer was unprepared,
      and must prepare it again, otherwise rendering will fail).
      @groupBegin }
    procedure SetOnBeforeGLVertex(const Value: TBeforeGLVertexProc); virtual;
    procedure SetOnRadianceTransfer(const Value: TRadianceTransferFunction); virtual;
    procedure SetOnVertexColor(const Value: TVertexColorFunction); virtual;
    procedure SetSmoothShading(const Value: boolean); virtual;
    procedure SetColorModulatorSingle(const Value: TColorModulatorSingleFunc); virtual;
    procedure SetColorModulatorByte(const Value: TColorModulatorByteFunc); virtual;
    procedure SetUseLights(const Value: boolean); virtual;
    procedure SetFirstGLFreeLight(const Value: Cardinal); virtual;
    procedure SetLastGLFreeLight(const Value: integer); virtual;
    procedure SetControlMaterials(const Value: boolean); virtual;
    procedure SetControlTextures(const Value: boolean); virtual;
    procedure SetEnableTextures(const Value: boolean); virtual;
    procedure SetFirstGLFreeTexture(const Value: Cardinal); virtual;
    procedure SetLastGLFreeTexture(const Value: integer); virtual;
    procedure SetTextureMinFilter(const Value: TGLint); virtual;
    procedure SetTextureMagFilter(const Value: TGLint); virtual;
    procedure SetPointSize(const Value: TGLFloat); virtual;
    procedure SetUseFog(const Value: boolean); virtual;
    procedure SetBumpMappingMaximum(const Value: TBumpMappingMethod); virtual;
    procedure SetGLSLShaders(const Value: boolean); virtual;
    procedure SetPureGeometry(const Value: boolean); virtual;
    { @groupEnd }
  public
    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TPersistent): boolean; virtual;

    { tuz przed narysowaniem KAZDEGO vertexa bedzie wywolywana ta procedura.
      (to znaczy TUZ przed glVertex, juz po ustaleniu koloru (glColor),
      glTexCoord, glNormal, glEdgeFlag, no w ogole - wszystkiego).
      Najpierw bedzie wywolana ta procedura z parametrami a)node podklasy
      TVRMLGeometryNode ktory renderuje ten vertex i b)wspolrzedne vertexa
      przemnozone przez RenderState.Transform. Innymi slowy bedzie to
      wspolrzedna vertexa wzgledem lokalnego ukladu wspolrzednych VRML'a
      (podczas gdy rzeczywista komenda glVertex prawdopodobnie przekaze
      OpenGL'owi wspolrzedne prosto z pliku VRML (a wiec byc moze wzgledem
      ukladu wspolrzednych aktualnego node'a, a nie calego VRML'a)).

      Zrobilem ta proc zeby zrobic glForCoord, ale byc moze znajdzie sie
      kiedys dla niej jeszcze jakies zastosowanie.

      nil by default. }
    property OnBeforeGLVertex: TBeforeGLVertexProc
      read FOnBeforeGLVertex write SetOnBeforeGLVertex;

    { Calculate vertex color from radiance transfer.
      If this is assigned, and geometry object has radianceTransfer
      field (see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_radiance_transfer])
      then this is used to calculate the color of each vertex.

      Note that this is evaluated when object is rendered.
      If your object has dynamic lighting, you want to use roNone optimization,
      otherwise colors returned by this are saved on display list and
      never change. }
    property OnRadianceTransfer: TRadianceTransferFunction
      read FOnRadianceTransfer write SetOnRadianceTransfer;

    { Calculate vertex color for given vertex by a callback.
      If this is assigned, then this is used to calculate
      the color of each vertex.

      Note that this is evaluated when object is rendered.
      If this changes dynamically (for example, it calculates some dynamic
      lighting), you want to use roNone optimization,
      otherwise colors returned by this are saved on display list and
      never change. }
    property OnVertexColor: TVertexColorFunction
      read FOnVertexColor write SetOnVertexColor;

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
    property FirstGLFreeLight: Cardinal
      read FFirstGLFreeLight write SetFirstGLFreeLight default 0;
    property LastGLFreeLight: integer
      read FLastGLFreeLight write SetLastGLFreeLight default -1;

    { If this is @true, then our engine takes care of applying appropriate
      materials and colors on your model.

      For special purposes, you can set this to @false.
      Then you are expected to set glColor/glMaterial yourself. }
    property ControlMaterials: boolean
      read FControlMaterials write SetControlMaterials default true;

    { If this is @true, then our engine takes care of everything related
      to texturing your model. Textures will be automatically activated
      (for multitexturing), enabled/disabled, bound, and texture coordinates
      will be used/generated, according to your VRML model data
      (and EnableTextures attribute).

      For special purposes, you can set this to @false.
      Then our engine assumes no control over the
      enabled/disabled state of OpenGL texturing and the currently bound texture.
      Texture coordinates will still be generated, if applicable.
      This is useful when your model specifies texture coordinates but
      still you want to control from your program which (if any) texture is
      currently bound and enabled. }
    property ControlTextures: boolean
      read FControlTextures write SetControlTextures default true;

    { If ControlTextures is @true, then this property determines
      whether we should actually take model textures into account.
      In other words:

      @unorderedList(
        @item(When ControlTextures = EnableTextures = @true (default),
          then our engine takes care of everything related to texturing
          for you: enabling and using textures for textured parts of the model,
          disabling textures for non-textured parts.)

        @item(When ControlTextures = @true but EnableTextures = @false,
          you force the engine to ignore textures in your model.
          The whole scene will be rendered with glDisable(GL_TEXTURE_*),
          texture coordinates will not be generated etc.
          This is for special purposes.)

        @item(When ControlTextures = @false, value of EnableTextures
          doesn't matter. See ControlTextures for description.)
      ) }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { These specify which OpenGL texture units are free to use.

      Note that for now we assume that at least one texture unit is free.
      If OpenGL multitexturing is not available, we will just use the default
      texture unit.

      LastGLFreeTexture = -1 means that up to the end, all texture units
      are available. In other words, -1 is equivalent to
      glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) - 1.

      @groupBegin }
    property FirstGLFreeTexture: Cardinal
      read FFirstGLFreeTexture write SetFirstGLFreeTexture default 0;
    property LastGLFreeTexture: Integer
      read FLastGLFreeTexture write SetLastGLFreeTexture default -1;
    { @groupEnd }

    { Default minification and magnification filters for textures.
      These can be overridden on a per-texture basis in VRML / X3D files
      by X3D TextureProperties node (see X3D specification).

      @groupBegin }
    property TextureMinFilter: TGLint
      read FTextureMinFilter write SetTextureMinFilter default GL_LINEAR_MIPMAP_LINEAR;
    property TextureMagFilter: TGLint
      read FTextureMagFilter write SetTextureMagFilter default GL_LINEAR;
    { @groupEnd }

    { scena bedzie wyswietlana z glPointSize(PointSize),
      co ma wplyw tylko na renderowanie PointSet. Zrobilem to atrybutem
      renderera (zamiast po prostu pozwolic temu stanowi OpenGL'a "przeciec"
      z zewnatrz) bo domyslny rozmiar mial byc = 3 a nie 1 (jak w OpenGL'u) }
    property PointSize: TGLFloat
      read FPointSize write SetPointSize default 3.0;

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

    { Enable bump mapping. This sets maximum allowed bump mapping method
      (actual method used may be lower, depending on OpenGL capabilities
      and information provided in VRML model, like normalMap and heightMap).
      Set to bmNone (default) to disable using bump mapping.
      Set to High(TBumpMappingMethod) to enable best implemented bump mapping.

      To actually use this, it requires also
      some OpenGL capabilities (some extensions present, and enough texture
      units available). And naturally it comes to use only if
      VRML model will specify normalMap field for some shapes nodes.
      For parallax mapping, heightMap is also needed.

      You have to update Renderer.BumpMappingLightPosition if enable bump
      mappping (that is, you set BumpMappingMaximum to something <> bmNone),
      to actually specify how bumps should appear.
      See TVRMLOpenGLRenderer.BumpMappingLightPosition, or
      TVRMLGLScene.BumpMappingLightPosition for more comfortable version.
      See also other TVRMLOpenGLRenderer.BumpMappingLightXxx properties,
      like TVRMLOpenGLRenderer.BumpMappingLightDiffuseColor.

      There are some TODOs related to this:
      @unorderedList(
        @item(bmMultiTex* methods don't take texture transform into account correctly.)
        @item(
          We are able to calculate s/t tangent vectors (and so do bump mapping)
          only on IndexedFaceSet (with explicit texture coords,
          or implicit (autogenerated)) for now.
          Other primitives should be done one day.)
        @item(For each texture, there must always be the same
          normalMap and heightMap
          (since we store it in the same TTextureImageReference).)
      )
      See other TODOs in this file.
    }
    property BumpMappingMaximum: TBumpMappingMethod
      read FBumpMappingMaximum write SetBumpMappingMaximum
      default bmNone;

    { @abstract(Use shaders defined in VRML file in GLSL language ?) }
    property GLSLShaders: boolean read FGLSLShaders write SetGLSLShaders
      default true;

    { Use this to render pure geometry, without any colors, materials, etc.
      If this is @true, only pure geometry will be rendered.
      This means that the rendering of the model will be equivalent to
      calling only @code(glBegin(...)), then @code(glVertex(...)),
      then @code(glEnd). Actually, some additional calls may be done
      (to push/pop/multiply current modelview matrix, and to realize
      drawing of vertexes by vertex arrays).
      But the point is that no OpenGL state, besides the absolute minimum to render
      polygons at their correct places, will be touched.
      Oh, and backface culling will be correctly enabled (glCullFace mode,
      GL_CULL_FACE flag) as appropriate.

      For example, Renderer will not set any color (no glColor calls),
      will not set any material
      (no glMaterial calls), will not set any texture coordinates and
      will not bind any texture, will not enable any depth testing, fog and
      everything else.

      This is only useful for some special OpenGL tricks. For example if you
      want, for whatever reason, to set glColor and/or glMaterial
      and/or texturing by yourself, for the whole model.
      A practical example of use is to render plane-projected shadows,
      see kambi_vrml_game_engine/3dmodels.gl/examples/plane_projected_shadow_demo.pasprogram.
      In this program, we must be able to render any VRML model with pure
      black color, possibly (when using stenciling) withuout even depth
      testing. }
    property PureGeometry: boolean
      read FPureGeometry write SetPureGeometry default false;
  end;

  TVRMLRenderingAttributesClass = class of TVRMLRenderingAttributes;

  TGLOutlineFontCache = record
    References: Cardinal;
    Instance: TGLOutlineFont;
  end;

  TTextureImageCache = record
    FullUrl: string;

    { This is only the first TVRMLTextureNode node, that initiated this
      TTextureImageCache item. Note that many TVRMLTextureNode nodes
      may correspond to a single TTextureImageCache (since TTextureImageCache
      only tries to share GLName between them). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of texture nodes related to this video texture! }
    InitialNode: TVRMLTextureNode;

    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    ColorModulator: TColorModulatorByteFunc;
    References: Cardinal;
    GLName: TGLuint;

    { This is the saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureImageCache = ^TTextureImageCache;

  TDynArrayItem_2 = TTextureImageCache;
  PDynArrayItem_2 = PTextureImageCache;
  {$define DYNARRAY_2_IS_STRUCT}
  {$define DYNARRAY_2_IS_INIT_FINI_TYPE}
  {$I dynarray_2.inc}
  TDynTextureImageCacheArray = class(TDynArray_2)
  end;

  TTextureVideoCache = record
    FullUrl: string;

    { This is only the first TNodeMovieTexture node, that initiated this
      TTextureVideoCache item. Note that many TNodeMovieTexture nodes
      may correspond to a single TTextureVideoCache (since TTextureVideoCache
      only tries to share TGLVideo between them, they don't have to share
      other fields like current time etc.). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of MovieTexture nodes related to this video texture! }
    InitialNode: TNodeMovieTexture;

    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    ColorModulator: TColorModulatorByteFunc;
    References: Cardinal;
    GLVideo: TGLVideo;

    { This is the saved result of TVideo.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureVideoCache = ^TTextureVideoCache;

  TDynArrayItem_7 = TTextureVideoCache;
  PDynArrayItem_7 = PTextureVideoCache;
  {$define DYNARRAY_7_IS_STRUCT}
  {$define DYNARRAY_7_IS_INIT_FINI_TYPE}
  {$I dynarray_7.inc}
  TDynTextureVideoCacheArray = class(TDynArray_7)
  end;

  TTextureCubeMapCache = record
    InitialNode: TNodeX3DEnvironmentTextureNode;
    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    References: Cardinal;
    GLName: TGLuint;

    { This is the saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureCubeMapCache = ^TTextureCubeMapCache;

  TDynArrayItem_9 = TTextureCubeMapCache;
  PDynArrayItem_9 = PTextureCubeMapCache;
  {$define DYNARRAY_9_IS_STRUCT}
  {$define DYNARRAY_9_IS_INIT_FINI_TYPE}
  {$I dynarray_9.inc}
  TDynTextureCubeMapCacheArray = class(TDynArray_9)
  end;

  TTexture3DCache = record
    InitialNode: TNodeX3DTexture3DNode;
    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap3D;
    References: Cardinal;
    GLName: TGLuint;

    { This is the saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTexture3DCache = ^TTexture3DCache;

  TDynArrayItem_11 = TTexture3DCache;
  PDynArrayItem_11 = PTexture3DCache;
  {$define DYNARRAY_11_IS_STRUCT}
  {$define DYNARRAY_11_IS_INIT_FINI_TYPE}
  {$I dynarray_11.inc}
  TDynTexture3DCacheArray = TDynArray_11;

  TTextureDepthCache = record
    InitialNode: TNodeGeneratedShadowMap;
    References: Cardinal;
    GLName: TGLuint;
  end;
  PTextureDepthCache = ^TTextureDepthCache;

  TDynArrayItem_13 = TTextureDepthCache;
  PDynArrayItem_13 = PTextureDepthCache;
  {$define DYNARRAY_13_IS_STRUCT}
  {$define DYNARRAY_13_IS_INIT_FINI_TYPE}
  {$I dynarray_13.inc}
  TDynTextureDepthCacheArray = TDynArray_13;

  { Note that Attributes and State are owned by this record
    (TVRMLOpenGLRendererContextCache will make sure about creating/destroying
    them), but GeometryNode and FogNode are a references somewhere to the scene
    (they will be supplied to TVRMLOpenGLRendererContextCache instance)
    and we don't own them. }
  TShapeCache = record
    Attributes: TVRMLRenderingAttributes;
    GeometryNode: TVRMLGeometryNode;
    State: TVRMLGraphTraverseState;
    FogNode: TNodeFog;
    FogDistanceScaling: Single;

    GLList: TGLuint;
    References: Cardinal;
  end;
  PShapeCache = ^TShapeCache;

  TDynArrayItem_3 = TShapeCache;
  PDynArrayItem_3 = PShapeCache;
  {$define DYNARRAY_3_IS_STRUCT}
  {$I dynarray_3.inc}
  TDynShapeCacheArray = class(TDynArray_3)
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

  TGLSLProgramCache = record
    ProgramNode: TNodeComposedShader;
    { GLSLProgram is always non-nil here. }
    GLSLProgram: TGLSLProgram;
    References: Cardinal;
  end;
  PGLSLProgramCache = ^TGLSLProgramCache;

  TDynArrayItem_5 = TGLSLProgramCache;
  PDynArrayItem_5 = PGLSLProgramCache;
  {$define DYNARRAY_5_IS_STRUCT}
  {$I dynarray_5.inc}
  TDynGLSLProgramCacheArray = class(TDynArray_5)
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
  TVRMLOpenGLRendererContextCache = class(TImagesVideosCache)
  private
    Fonts: array[TVRMLFontFamily, boolean, boolean] of TGLOutlineFontCache;
    TextureImageCaches: TDynTextureImageCacheArray;
    TextureVideoCaches: TDynTextureVideoCacheArray;
    TextureCubeMapCaches: TDynTextureCubeMapCacheArray;
    Texture3DCaches: TDynTexture3DCacheArray;
    TextureDepthCaches: TDynTextureDepthCacheArray;
    ShapeCaches: TDynShapeCacheArray;
    ShapeNoTransformCaches: TDynShapeCacheArray;
    RenderBeginCaches: TDynRenderBeginEndCacheArray;
    RenderEndCaches: TDynRenderBeginEndCacheArray;
    GLSLProgramCaches: TDynGLSLProgramCacheArray;

    function TextureImage_IncReference(
      const TextureImage: TEncodedImage;
      const TextureFullUrl: string;
      const TextureNode: TVRMLTextureNode;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const TextureColorModulator: TColorModulatorByteFunc;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const TextureNode: TNodeMovieTexture;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const TextureColorModulator: TColorModulatorByteFunc;
      out AlphaChannelType: TAlphaChannelType): TGLVideo;

    procedure TextureVideo_DecReference(
      const TextureVideo: TGLVideo);

    function TextureCubeMap_IncReference(
      Node: TNodeX3DEnvironmentTextureNode;
      const MinFilter, MagFilter: TGLint;
      const Anisotropy: TGLfloat;
      PositiveX, NegativeX,
      PositiveY, NegativeY,
      PositiveZ, NegativeZ: TImage;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure TextureCubeMap_DecReference(
      const TextureGLName: TGLuint);

    function TextureDepth_IncReference(
      Node: TNodeGeneratedShadowMap;
      const Size: Cardinal): TGLuint;

    procedure TextureDepth_DecReference(
      const TextureGLName: TGLuint);

    function Texture3D_IncReference(
      Node: TNodeX3DTexture3DNode;
      const MinFilter, MagFilter: TGLint;
      const Anisotropy: TGLfloat;
      const TextureWrap: TTextureWrap3D;
      Image: TImage;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure Texture3D_DecReference(
      const TextureGLName: TGLuint);

    function FogParametersEqual(
      FogNode1: TNodeFog; const FogDistanceScaling1: Single;
      FogNode2: TNodeFog; const FogDistanceScaling2: Single): boolean;

    { Set GLSLProgram uniform variable from VRML field value.
      Uniform name is contained in UniformName. UniformValue indicates
      uniform type and new value (UniformValue.Name is not used). }
    procedure SetUniformFromField(
      GLSLProgram: TGLSLProgram; UniformName: string;
      UniformValue: TVRMLField);

    procedure EventReceiveGLSLUniform(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);

    function GLSLProgram_IncReference(
      ProgramNode: TNodeComposedShader): TGLSLProgram;
    procedure GLSLProgram_DecReference(var GLSLProgram: TGLSLProgram);
  public
    constructor Create;
    destructor Destroy; override;

    function Fonts_IncReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
      TTF_Font: PTrueTypeFont): TGLOutlineFont;

    procedure Fonts_DecReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);

    { These will be used by TVRMLGLScene.

      Note that we have two versions of Shape_IncReference,
      because if the list will already exist in the cache then we don't want to
      waste time on creating and immediately freeing unnecessary list.
      you should call Shape_IncReference_Existing, and if @false
      then you should build display list and call
      Shape_IncReference_New. }

    function Shape_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure Shape_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure Shape_DecReference(
      const GLList: TGLuint);

    function ShapeNoTransform_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure ShapeNoTransform_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure ShapeNoTransform_DecReference(
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

  TTextureImageReference = record
    Node: TVRMLTextureNode;
    GLName: TGLuint;
    NormalMap, HeightMap: TGLuint;
    HeightMapScale: Single;
    { This is the saved result of TImage.AlphaChannelType. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureImageReference = ^TTextureImageReference;

  TDynArrayItem_1 = TTextureImageReference;
  PDynArrayItem_1 = PTextureImageReference;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynTextureImageReferenceArray = class(TDynArray_1)
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TVRMLTextureNode): integer;
  end;

  TTextureVideoReference = record
    Node: TNodeMovieTexture;
    GLVideo: TGLVideo;
    { For now, I don't support movie textures with bump mapping.
    NormalMap, HeightMap: TGLuint;
    HeightMapScale: Single; }
    { This is the saved result of TVideo.AlphaChannelType. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureVideoReference = ^TTextureVideoReference;

  TDynArrayItem_8 = TTextureVideoReference;
  PDynArrayItem_8 = PTextureVideoReference;
  {$define DYNARRAY_8_IS_STRUCT}
  {$I dynarray_8.inc}
  TDynTextureVideoReferenceArray = class(TDynArray_8)
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TVRMLTextureNode): integer;
  end;

  TTextureCubeMapReference = record
    Node: TNodeX3DEnvironmentTextureNode;
    GLName: TGLuint;
    { When Node is TNodeGeneratedTextureCubeMap,
      this is the right size of the texture,
      that satisfies all OpenGL cube map sizes requirements
      (IsCubeMapTextureSized).
      Unused for other Node classes. }
    GeneratedSize: Cardinal;
    { When Node is TNodeGeneratedTextureCubeMap,
      this says if MinFilter needs mipmaps. }
    GeneratedNeedsMipmaps: boolean;

    { This is the saved result of TImage.AlphaChannelType. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureCubeMapReference = ^TTextureCubeMapReference;

  TDynArrayItem_10 = TTextureCubeMapReference;
  PDynArrayItem_10 = PTextureCubeMapReference;
  {$define DYNARRAY_10_IS_STRUCT}
  {$I dynarray_10.inc}
  TDynTextureCubeMapReferenceArray = class(TDynArray_10)
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TNodeX3DEnvironmentTextureNode): Integer;
  end;

  TTexture3DReference = record
    Node: TNodeX3DTexture3DNode;
    GLName: TGLuint;

    { This is the saved result of TImage.AlphaChannelType. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTexture3DReference = ^TTexture3DReference;

  TDynArrayItem_12 = TTexture3DReference;
  PDynArrayItem_12 = PTexture3DReference;
  {$define DYNARRAY_12_IS_STRUCT}
  {$I dynarray_12.inc}
  TDynTexture3DReferenceArray = class(TDynArray_12)
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TNodeX3DTexture3DNode): Integer;
  end;

  TTextureDepthReference = record
    Node: TNodeGeneratedShadowMap;
    GLName: TGLuint;
    { When Node is TNodeGeneratedShadowMap,
      this is the right size of the texture,
      that satisfies all OpenGL sizes requirements.
      Unused for other Node classes. }
    GeneratedSize: Cardinal;
  end;
  PTextureDepthReference = ^TTextureDepthReference;

  TDynArrayItem_14 = TTextureDepthReference;
  PDynArrayItem_14 = PTextureDepthReference;
  {$define DYNARRAY_14_IS_STRUCT}
  {$I dynarray_14.inc}
  TDynTextureDepthReferenceArray = class(TDynArray_14)
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TNodeGeneratedShadowMap): Integer;
  end;

  TGLSLProgramReference = record
    ProgramNode: TNodeComposedShader;

    { GLSLProgram prepared.

      @nil means that this GLSL program failed to initialize.
      So do not try to initialize it again, and no need to unprepare
      it from Cache (as Cache doesn't have this program). }
    GLSLProgram: TGLSLProgram;
  end;
  PGLSLProgramReference = ^TGLSLProgramReference;

  TDynArrayItem_6 = TGLSLProgramReference;
  PDynArrayItem_6 = PGLSLProgramReference;
  {$define DYNARRAY_6_IS_STRUCT}
  {$I dynarray_6.inc}
  TDynGLSLProgramReferenceArray = class(TDynArray_6)
  public
    { szuka rekordu z danym ProgramNode.
      Zwraca jego indeks lub -1 jesli nie znajdzie. }
    function ProgramNodeIndex(ProgramNode: TNodeComposedShader): Integer;
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

    { Use always LastGLFreeTexture, this will never return -1.
      Will return Attributes.LastGLFreeTexture, or
      glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) -1 if -1.

      To minimize number of glGetInteger calls, the result of this is cached
      in FLastGLFreeTexture. }
    FLastGLFreeTexture: Integer;
    function LastGLFreeTexture: Cardinal;

    { Number of available texture units.
      Just a shortcut for LastGLFreeTexture - FirstGLFreeTexture + 1,
      always >= 0. }
    function FreeGLTexturesCount: Cardinal;

    BumpMappingMethodCached: TBumpMappingMethod;
    BumpMappingMethodIsCached: boolean;

    { Will be created by some prepare, if BumpMappingMethod is bmGLSLAll
      and it's detected that bump mapping will be actually used.

      Boolean index indicates whether it's the version with parallax
      mapping. }
    BmGLSLProgram: array[boolean] of TGLSLProgram;

    { Only if BumpMappingMethod in bmGLSLAll and BmGLSLProgram[true] is prepared
      (GLSL program for bump mapping with parallax mapping)
      this will indicate whether we prepared version with or without
      "steep" parallax mapping.

      Note that I didn't add another TBumpMappingMethod value, like
      bmGLSLSteepParallax, because availability of steep parallax mapping
      is checked only when it's program is actually compiled.
      Failure to compile causes us to fallback to normal parallax, without
      steep improvements. }
    BmSteepParallaxMapping: boolean;

    BmGLSLAttribObjectSpaceToTangent: array[boolean] of TGLSLAttribute;

    TextureImageReferences: TDynTextureImageReferenceArray;
    TextureVideoReferences: TDynTextureVideoReferenceArray;
    TextureCubeMapReferences: TDynTextureCubeMapReferenceArray;
    Texture3DReferences: TDynTexture3DReferenceArray;
    TextureDepthReferences: TDynTextureDepthReferenceArray;
    GLSLProgramReferences: TDynGLSLProgramReferenceArray;

    { To which fonts we made a reference in the cache ? }
    FontsReferences: array[TVRMLFontFamily, boolean, boolean] of boolean;

    TexNormalizationCube: TGLuint;

    { ------------------------------------------------------------
      Rzeczy z ktorych mozna korzystac tylko w czasie Render. }

    { Our mesh renderer. Actually of TVRMLMeshRenderer class, but I really
      don't want to expose TVRMLMeshRenderer class in the interface. }
    ExposedMeshRenderer: TObject;

    { kopie aktualnego Shape, Shape.State i Shape.Geometry na czas Render }
    CurrentShape: TVRMLShape;
    CurrentState: TVRMLGraphTraverseState;
    CurrentGeometry: TVRMLGeometryNode;

    { te zmienne sa wewnetrzne dla funkcji MeterialsBegin/End, BindMaterial }
    Render_Material_ForcedLightDisable: boolean;
    Render_Material_BoundMatNum: integer;
    Render_Material_LastFogImmune: boolean;
    Material_BoundOpacity: Single;
    MaterialFromColorEnabled: boolean;

    procedure Render_MaterialsBegin;
    procedure Render_MaterialsEnd;
    procedure Render_BindMaterial_1(MatNum: integer);
    procedure Render_BindMaterial_2;
    procedure Render_Material(
      const Lit: boolean;
      const AmbientColor, DiffuseColor, SpecularColor,
        EmissiveColor: TVector3Single;
      const UnLitColor: TVector3Single;
      const ShininessExp, Opacity: Single;
      const FogImmune: boolean);
    procedure SetColor(const Color: TVector3Single);

    { Judge whether the node can be lit. }
    function NodeLit(Node: TVRMLGeometryNode): boolean;

    { For how many texture units does Render must generate tex coords?

      This is the number of texture units used.
      Always <= 1 if OpenGL doesn't support multitexturing
      (UseMultiTexturing = @false).
      It's also already clamped by the number of available texture units
      (determined from First/LastGLFreeTexture).

      Always = 1 if bump mapping is used (our multitexturing setup is
      special then, we will actually use more texture units for special
      purposes). }
    TexCoordsNeeded: Cardinal;

    { @true if OpenGL allows and we should use multi-texturing
      for VRML/X3D MultiTexture support.

      This is orthogonal to bump mapping method, and in fact will be ignored
      where bump mapping is used (bump mapping methods check the multitexturing
      support explicitly, for themselves). }
    UseMultiTexturing: boolean;

    { Set by RenderShapeBegin, used by RenderShapeEnd. Tells for which
      texture units we pushed and modified the texture matrix.
      Always <= 1 if not UseMultiTexturing. }
    TextureTransformUnitsUsed: Cardinal;

    { Additional texture units used,
      in addition to 0..TextureTransformUnitsUsed - 1.
      Cleared by RenderShapeBegin, added by PushTextureUnit,
      used by RenderShapeEnd. }
    TextureTransformUnitsUsedMore: TDynLongIntArray;

    { This calls glPushMatrix, assuming that current matrix mode is GL_TEXTURE
      and current tex unit is TexUnit (always make sure this is true when
      calling it!).

      It also records this fact, so that RenderShapeEnd will be able to
      make pop texture matrix later.

      In fact this optimizes push/pops on texture matrix stack, such that
      VRML TextureTransform nodes and such together with PushTextureUnit
      will only use only matrix stack place, even if texture will be
      "pushed" multiple times (both by PushTextureUnit and normal
      VRML TextureTransform realized in RenderShapeBegin.) }
    procedure PushTextureUnit(const TexUnit: Cardinal);

    { ----------------------------------------------------------------- }

    {$ifdef USE_VRML_NODES_TRIANGULATION}
    procedure DrawTriangle(const Tri: TTriangle3Single;
      State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
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

    { If ARB_multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0_ARB
      + FirstGLFreeTexture.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (ARB_multitexturing, GL_TEXTURE0_ARB,
      FirstGLFreeTexture values) is taken care of inside here. }
    procedure ActiveTexture(TextureUnit: Cardinal);

    FBumpMappingLightPosition: TVector3Single;
    procedure SetBumpMappingLightPosition(const Value: TVector3Single);

    FBumpMappingLightAmbientColor: TVector4Single;
    procedure SetBumpMappingLightAmbientColor(const Value: TVector4Single);

    FBumpMappingLightDiffuseColor: TVector4Single;
    procedure SetBumpMappingLightDiffuseColor(const Value: TVector4Single);
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

    procedure RenderShapeLights(
      LightsRenderer: TVRMLGLLightsCachingRenderer;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeBegin(Shape: TVRMLShape);
    procedure RenderShapeNoTransform(Shape: TVRMLShape);
    procedure RenderShapeEnd(Shape: TVRMLShape);

    procedure RenderShape(Shape: TVRMLShape);

    { This checks Attributes (mainly Attributes.BumpMappingMaximum) and OpenGL
      context capabilities to check which bump mapping method (if any)
      should be used.

      More precisely: this checks Attributes.BumpMappingMaximum,
      Attributes.ControlTextures, Attributes.EnableTextures.
      Then checks are appropriate OpenGL capabilities
      present (GL_ARB_multitexture and friends, GLSL for better methods).
      Then checks are enough texture units available (using First/LastGLFreeTexture).

      This method is mainly for debugging purposes, as this class handles everything
      related to bump mapping inside. This function may be usable for you
      only to display to user this property. Note that calling this
      ties us to current OpenGL context (just like any PrepareXxx or RenderXxx
      call). }
    function BumpMappingMethod: TBumpMappingMethod;

    { How we would support bump mapping in current OpenGL context, with given
      Attributes values.

      The contract is that if you @italic(create TVRMLOpenGLRenderer in current
      OpenGL context) and @italic(set it's Attributes just like parameters to
      this method) then @italic(created TVRMLOpenGLRenderer will
      have BumpMappingMethod the same as what this function tells).

      This is helpful if you don't have TVRMLOpenGLRenderer and it's
      attributes instances created yet, but you want to know right now
      what bump mapping will be available. }
    class function GLContextBumpMappingMethod(
      const FirstGLFreeTexture: Cardinal;
      ALastGLFreeTexture: Integer;
      const AttributesBumpMappingMaximum: TBumpMappingMethod;
      const AttributesControlTextures, AttributesEnableTextures, AttributesPureGeometry: boolean):
      TBumpMappingMethod;

    { Sets light position used for bump mapping.
      This is meaningful if BumpMappingMethod <> bmNone.

      If BumpMappingMethod is in bmMultiTexAll, then this simply sets internal variable.
      You have to actually render model (that is, call RenderBegin +
      RenderShape...) to use new BumpMappingLightPosition.
      If you stored rendering results in display lists, you have bad luck
      --- you have to rebuild them. Reason: to recalculate
      light direction in tangent space.  Which practically means that if you
      want to change BumpMappingLightPosition often, you have to
      use roNone as Optimization for TVRMLGLScene.

      If BumpMappingMethod is in bmGLSLAll, things are better.
      If the bump mapping shader is already prepared, then setting this property
      simply sets the uniform value of this shader. And light direction
      in tangent space is calculated by the shader. So you can simply reuse
      your display lists. (If the bump mapping shader is not prepared yet,
      then value set here will be used at preparation... so things work without
      problems in any case.) }
    property BumpMappingLightPosition: TVector3Single
      read FBumpMappingLightPosition write SetBumpMappingLightPosition;

    { Ambient color of the light calculated when doing bump mapping.

      When doing bump mapping, we don't use VRML lights. Instead some
      properties of the light are controlled by BumpMappingLightPosition
      (or TVRMLGLScene.BumpMappingLightPosition) and attributes like
      this one.

      Note that whether this is used depends on BumpMappingMethod used
      (and this depends on OpenGL capabilities). Some bump mapping methods
      may not be able to use this. For now, this is used only by bmGLSLAll
      methods.

      Default value is DefaultBumpMappingLightAmbientColor.

      4th component of the color has the same meaning and use as 4th color
      component for OpenGL lights. Usually, just set this to 1.0. }
    property BumpMappingLightAmbientColor: TVector4Single
      read FBumpMappingLightAmbientColor
      write SetBumpMappingLightAmbientColor;

    { Diffuse color of the light calculated when doing bump mapping.
      See also comments at BumpMappingLightAmbientColor.

      Default value is DefaultBumpMappingLightDiffuseColor. }
    property BumpMappingLightDiffuseColor: TVector4Single
      read FBumpMappingLightDiffuseColor
      write SetBumpMappingLightDiffuseColor;

    { Get calculated TImage.AlphaChannelType for texture in our cache.

      Returns @false if texture is not in the cache. If you want to make
      sure the texture is in the cache make sure that
      @unorderedList(
        @item(Texture node was previously prepared
          (passed to @link(Prepare) method along with some state,
          as State.Texture),)
        @item(Attributes.PureGeometry = @false,)
        @item(and node must have some texture data
          (for TVRMLTextureNode, check TextureNode.IsTextureImage or
          TextureNode.IsTextureVideo))
      ) }
    function PreparedTextureAlphaChannelType(
      TextureNode: TNodeX3DTextureNode;
      out AlphaChannelType: TAlphaChannelType): boolean;

    { Last available OpenGL light number.

      This is never -1, in contrast to Attributes.LastGLFreeLight.
      In other words, if Attributes.LastGLFreeLight, then this method
      will actually make appropriate glGetInteger call to get number of
      available lights in this OpenGL context. }
    function LastGLFreeLight: integer;

    { Update generated texture for this shape.

      NeedsRestoreViewport will be set to @true if viewport was
      (possibly) changed by this procedure (otherwise, NeedsRestoreViewport
      will not be modified). }
    procedure UpdateGeneratedTextures(Shape: TVRMLShape;
      TextureNode: TVRMLNode;
      const Render: TRenderTargetFunction;
      const ProjectionNear, ProjectionFar: Single;
      const MapsOverlap: boolean;
      const MapScreenX, MapScreenY: Integer;
      var NeedsRestoreViewport: boolean);
  end;

  EVRMLOpenGLRenderError = class(EVRMLError);

const
  BumpMappingMethodNames: array [TBumpMappingMethod] of string =
  ( 'None',
    'Dot (multitex, not normalized)',
    'Dot (multitex, normalized by cube map)',
    'Dot (by GLSL)',
    'Dot with steep parallax (by GLSL)' );

  bmMultiTexAll = [bmMultiTexDotNotNormalized, bmMultiTexDotNormalized];
  bmGLSLAll = [bmGLSLNormal, bmGLSLParallax];

var
  { Current camera matrix. Transforms from world space (normal 3D space)
    to camera space (camera space is the space where you're always
    standing on zero point, looking in -Z, and so on).
    This is currently needed only for handling correctly
    TextureCoordinateGenerator.mode = "WORLDSPACEREFLECTIONVECTOR".

    TODO: this is not supposed to be global var }
  CameraMatrix: TMatrix4Single;

{$undef read_interface}

implementation

uses Math, Triangulator, NormalizationCubeMap,
  KambiStringUtils, GLVersionUnit, KambiLog, KambiClassUtils,
  VRMLGeometry, VRMLScene, DDS, Frustum;

{$define read_implementation}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}
{$I dynarray_5.inc}
{$I dynarray_6.inc}
{$I dynarray_7.inc}
{$I dynarray_8.inc}
{$I dynarray_9.inc}
{$I dynarray_10.inc}
{$I dynarray_11.inc}
{$I dynarray_12.inc}
{$I dynarray_13.inc}
{$I dynarray_14.inc}

{$I openglmac.inc}

{$I vrmlmeshrenderer.inc}
{$I vrmlmeshrenderer_x3d_rendering.inc}
{$I vrmlmeshrenderer_x3d_geometry3d.inc}
{$I vrmlmeshrenderer_x3d_geometry2d.inc}
{$I vrmlmeshrenderer_simple_nodes.inc}

{ TVRMLOpenGLRendererContextCache -------------------------------------------- }

{ $define DEBUG_VRML_RENDERER_CACHE}

constructor TVRMLOpenGLRendererContextCache.Create;
begin
  inherited;
  TextureImageCaches := TDynTextureImageCacheArray.Create;
  TextureVideoCaches := TDynTextureVideoCacheArray.Create;
  TextureCubeMapCaches := TDynTextureCubeMapCacheArray.Create;
  Texture3DCaches := TDynTexture3DCacheArray.Create;
  TextureDepthCaches := TDynTextureDepthCacheArray.Create;
  ShapeCaches := TDynShapeCacheArray.Create;
  ShapeNoTransformCaches := TDynShapeCacheArray.Create;
  RenderBeginCaches := TDynRenderBeginEndCacheArray.Create;
  RenderEndCaches := TDynRenderBeginEndCacheArray.Create;
  GLSLProgramCaches := TDynGLSLProgramCacheArray.Create;
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

  if TextureImageCaches <> nil then
  begin
    Assert(TextureImageCaches.Count = 0, 'Some references to texture images still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(TextureImageCaches);
  end;

  if TextureVideoCaches <> nil then
  begin
    Assert(TextureVideoCaches.Count = 0, 'Some references to texture videos still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(TextureVideoCaches);
  end;

  if TextureCubeMapCaches <> nil then
  begin
    Assert(TextureCubeMapCaches.Count = 0, 'Some references to texture cubemaps still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(TextureCubeMapCaches);
  end;

  if Texture3DCaches <> nil then
  begin
    Assert(Texture3DCaches.Count = 0, 'Some references to texture 3D still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(Texture3DCaches);
  end;

  if TextureDepthCaches <> nil then
  begin
    Assert(TextureDepthCaches.Count = 0, 'Some references to depth texture still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(TextureDepthCaches);
  end;

  if ShapeCaches <> nil then
  begin
    Assert(ShapeCaches.Count = 0, 'Some references to Shapes still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(ShapeCaches);
  end;

  if ShapeNoTransformCaches <> nil then
  begin
    Assert(ShapeNoTransformCaches.Count = 0,
      'Some references to ShapesNoTransform still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(ShapeNoTransformCaches);
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

  if GLSLProgramCaches <> nil then
  begin
    Assert(GLSLProgramCaches.Count = 0, 'Some references to GLSLProgram' +
      '  still exist when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(GLSLProgramCaches);
  end;

  inherited;
end;

function TVRMLOpenGLRendererContextCache.Fonts_IncReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
  TTF_Font: PTrueTypeFont): TGLOutlineFont;
begin
  Inc(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].Instance = nil then
    Fonts[fsfam, fsbold, fsitalic].Instance := TGLOutlineFont.Create(TTF_Font);
  Result := Fonts[fsfam, fsbold, fsitalic].Instance;
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

const
  { Parameters for AlphaChannelType to detect textures alpha channel }
  AlphaTolerance = 5;
  AlphaWrongPixelsTolerance = 0.01;

function TVRMLOpenGLRendererContextCache.TextureImage_IncReference(
  const TextureImage: TEncodedImage;
  const TextureFullUrl: string;
  const TextureNode: TVRMLTextureNode;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const TextureColorModulator: TColorModulatorByteFunc;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTextureImageCache;
begin
  for I := 0 to TextureImageCaches.High do
  begin
    TextureCached := TextureImageCaches.Pointers[I];

    { Once I had an idea to make here comparison with
      TextureImage = TextureCached^.Image. Since we have ImagesCache,
      so images from the same URL would have the same reference, so this
      would work perfectly, and make comparison with TextureURL obsolete, right ?

      But there's a problem with this: Image reference may be freed while
      the corresponding texture is still cached. In fact, it's normal in
      "The Castle", if you use FreeResources([frTexturesInNodes]) feature.
      Which means that Image reference may become invalid, and, worse,
      another Image may be potentially assigned the same reference.

      What would be needed is to automatically set cached Image reference
      to nil (and implement to not use Image reference if it's nil) if
      Image instance is freed. Something like FreeNotification.

      But still, the same FreeResources([frTexturesInNodes]) would prevent
      the texture from sharing, if we would free the texture prematurely
      and later load the same texture, with to different TImage instance.

      For now, I don't use this idea, and rely on TextureFullUrl. }

    if ( ( (TextureFullUrl <> '') and
           (TextureCached^.FullUrl = TextureFullUrl) ) or
         (TextureCached^.InitialNode = TextureNode) ) and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.Anisotropy = TextureAnisotropy) and
       (TextureCached^.Wrap = TextureWrap) and
       (TextureCached^.ColorModulator = TextureColorModulator) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  { Initialize Result first, before calling TextureImageCaches.AppendItem.
    That's because in case LoadGLTextureModulated raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureImage_DecReference later). }
  if Assigned(TextureColorModulator) then
  begin
    if TextureImage is TImage then
    begin
      Result := LoadGLTextureModulated(
        TImage(TextureImage), TextureMinFilter, TextureMagFilter,
        TextureWrap, TextureColorModulator);
    end else
    begin
      VRMLWarning(vwIgnorable, 'Cannot modulate S3TC compressed texture by ColorModulator, loading unmodulated');
      Result := LoadGLTexture(
        TextureImage, TextureMinFilter, TextureMagFilter, TextureWrap);
    end;
  end else
  begin
    Result := LoadGLTexture(
      TextureImage, TextureMinFilter, TextureMagFilter, TextureWrap);
  end;

  TexParameterMaxAnisotropy(GL_TEXTURE_2D, TextureAnisotropy);

  TextureCached := TextureImageCaches.AppendItem;
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.InitialNode := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.Anisotropy := TextureAnisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.ColorModulator := TextureColorModulator;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := TextureImage.AlphaChannelTypeOverride(
    TextureNode.DetectAlphaChannel,
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha texture ' + TextureFullUrl +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ', TextureFullUrl, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.TextureImage_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureImageCaches.High do
    if TextureImageCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureImageCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ', TextureImageCaches.Items[I].FullUrl, ' : ',
                       TextureImageCaches.Items[I].References);
      {$endif}
      if TextureImageCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureImageCaches.Items[I].GLName));
        TextureImageCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.TextureImage_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLOpenGLRendererContextCache.TextureVideo_IncReference(
  const TextureVideo: TVideo;
  const TextureFullUrl: string;
  const TextureNode: TNodeMovieTexture;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const TextureColorModulator: TColorModulatorByteFunc;
  out AlphaChannelType: TAlphaChannelType): TGLVideo;
var
  I: Integer;
  TextureCached: PTextureVideoCache;
begin
  for I := 0 to TextureVideoCaches.High do
  begin
    TextureCached := TextureVideoCaches.Pointers[I];

    if ( ( (TextureFullUrl <> '') and
           (TextureCached^.FullUrl = TextureFullUrl) ) or
         (TextureCached^.InitialNode = TextureNode) ) and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.Anisotropy = TextureAnisotropy) and
       (TextureCached^.Wrap = TextureWrap) and
       (TextureCached^.ColorModulator = TextureColorModulator) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLVideo);
    end;
  end;

  { Initialize Result first, before calling TextureVideoCaches.AppendItem.
    That's because in case TGLVideo.Create raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureVideo_DecReference later). }
  Result := TGLVideo.Create(
    TextureVideo, TextureMinFilter, TextureMagFilter, TextureAnisotropy,
    TextureWrap, TextureColorModulator);

  TextureCached := TextureVideoCaches.AppendItem;
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.InitialNode := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.Anisotropy := TextureAnisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.ColorModulator := TextureColorModulator;
  TextureCached^.References := 1;
  TextureCached^.GLVideo := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := TextureVideo.AlphaChannelTypeOverride(
    TextureNode.DetectAlphaChannel,
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha texture ' + TextureFullUrl +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ', TextureFullUrl, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.TextureVideo_DecReference(
  const TextureVideo: TGLVideo);
var
  I: Integer;
begin
  for I := 0 to TextureVideoCaches.High do
    if TextureVideoCaches.Items[I].GLVideo = TextureVideo then
    begin
      Dec(TextureVideoCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ', TextureVideoCaches.Items[I].FullUrl, ' : ',
                       TextureVideoCaches.Items[I].References);
      {$endif}
      if TextureVideoCaches.Items[I].References = 0 then
      begin
        FreeAndNil(TextureVideoCaches.Items[I].GLVideo);
        TextureVideoCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.TextureVideo_DecReference: no reference ' +
    'found to texture %s', [PointerToStr(TextureVideo)]);
end;

function TVRMLOpenGLRendererContextCache.TextureCubeMap_IncReference(
  Node: TNodeX3DEnvironmentTextureNode;
  const MinFilter, MagFilter: TGLint;
  const Anisotropy: TGLfloat;
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TImage;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTextureCubeMapCache;
begin
  for I := 0 to TextureCubeMapCaches.High do
  begin
    TextureCached := TextureCubeMapCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.MinFilter = MinFilter) and
       (TextureCached^.MagFilter = MagFilter) and
       (TextureCached^.Anisotropy = Anisotropy) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : cube map ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, Result);

  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MIN_FILTER, MinFilter);

  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexImages2DForCubeMap(
    PositiveX, NegativeX,
    PositiveY, NegativeY,
    PositiveZ, NegativeZ,
    TextureMinFilterNeedsMipmaps(MinFilter));

  TexParameterMaxAnisotropy(GL_TEXTURE_CUBE_MAP_ARB, Anisotropy);

  TextureCached := TextureCubeMapCaches.AppendItem;
  TextureCached^.InitialNode := Node;
  TextureCached^.MinFilter := MinFilter;
  TextureCached^.MagFilter := MagFilter;
  TextureCached^.Anisotropy := Anisotropy;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache.
    Use PositiveX image --- it doesn't matter, they all should have
    the same AlphaChannelType. }
  TextureCached^.AlphaChannelType := PositiveX.AlphaChannelType(
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha cube map texture ' + PointerToStr(Node) +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : cube map ', PointerToStr(Node), ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.TextureCubeMap_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureCubeMapCaches.High do
    if TextureCubeMapCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureCubeMapCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : cube map ', PointerToStr(TextureCubeMapCaches.Items[I].InitialNode), ' : ', TextureCubeMapCaches.Items[I].References);
      {$endif}
      if TextureCubeMapCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureCubeMapCaches.Items[I].GLName));
        TextureCubeMapCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.TextureCubeMap_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLOpenGLRendererContextCache.Texture3D_IncReference(
  Node: TNodeX3DTexture3DNode;
  const MinFilter, MagFilter: TGLint;
  const Anisotropy: TGLfloat;
  const TextureWrap: TTextureWrap3D;
  Image: TImage;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTexture3DCache;
begin
  for I := 0 to Texture3DCaches.High do
  begin
    TextureCached := Texture3DCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.MinFilter = MinFilter) and
       (TextureCached^.MagFilter = MagFilter) and
       (TextureCached^.Anisotropy = Anisotropy) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : 3d texture ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_3D_EXT, Result);

  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, MinFilter);

  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, TextureWrap[0]);
  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, TextureWrap[1]);
  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R, TextureWrap[2]);

  glTextureImage3d(Image, TextureMinFilterNeedsMipmaps(MinFilter));

  TexParameterMaxAnisotropy(GL_TEXTURE_3D_EXT, Anisotropy);

  TextureCached := Texture3DCaches.AppendItem;
  TextureCached^.InitialNode := Node;
  TextureCached^.MinFilter := MinFilter;
  TextureCached^.MagFilter := MagFilter;
  TextureCached^.Anisotropy := Anisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := Image.AlphaChannelType(
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha 3D texture ' + PointerToStr(Node) +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : 3d texture ', PointerToStr(Node), ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.Texture3D_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to Texture3DCaches.High do
    if Texture3DCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(Texture3DCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : 3d texture ', PointerToStr(Texture3DCaches.Items[I].InitialNode), ' : ', Texture3DCaches.Items[I].References);
      {$endif}
      if Texture3DCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(Texture3DCaches.Items[I].GLName));
        Texture3DCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.Texture3D_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLOpenGLRendererContextCache.TextureDepth_IncReference(
  Node: TNodeGeneratedShadowMap;
  const Size: Cardinal): TGLuint;
var
  I: Integer;
  TextureCached: PTextureDepthCache;
begin
  for I := 0 to TextureDepthCaches.High do
  begin
    TextureCached := TextureDepthCaches.Pointers[I];

    if TextureCached^.InitialNode = Node then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Depth texture ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  { In this case, clamp to border is Ok? TODO: test here }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  { Do not init any texture image. Just initialize texture sizes
    and both internal and external formats to GL_DEPTH_COMPONENT_ARB
    (will match depth buffer precision). }
  glTexImage2d(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT,
    Size, Size, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nil);

  if GL_ARB_shadow then
  begin
    if Node.FdCompareMode.Value = 'COMPARE_R_LEQUAL' then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_LEQUAL);
    end else
    if Node.FdCompareMode.Value = 'COMPARE_R_GEQUAL' then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_GEQUAL);
    end else
    if Node.FdCompareMode.Value = 'NONE' then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE) else
      VRMLWarning(vwSerious, Format('Invalid value for GeneratedShadowMode.compareMode: "%s"', [Node.FdCompareMode.Value]));

    glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
  end else
    VRMLWarning(vwIgnorable, 'OpenGL doesn''t support ARB_shadow, we cannot set depth comparison for depth texture');

  TextureCached := TextureDepthCaches.AppendItem;
  TextureCached^.InitialNode := Node;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Depth texture ', PointerToStr(Node), ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.TextureDepth_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthCaches.High do
    if TextureDepthCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthCaches.Items[I].References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Depth texture ', PointerToStr(TextureDepthCaches.Items[I].InitialNode), ' : ', TextureDepthCaches.Items[I].References);
      {$endif}
      if TextureDepthCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureDepthCaches.Items[I].GLName));
        TextureDepthCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.TextureDepth_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

procedure TVRMLOpenGLRendererContextCache.SetUniformFromField(
  GLSLProgram: TGLSLProgram; UniformName: string;
  UniformValue: TVRMLField);
var
  TempF: TDynSingleArray;
  TempVec2f: TDynVector2SingleArray;
  TempVec3f: TDynVector3SingleArray;
  TempVec4f: TDynVector4SingleArray;
  TempMat3f: TDynMatrix3SingleArray;
  TempMat4f: TDynMatrix4SingleArray;
begin
  { program must be active to set uniform values. }
  GLSLProgram.Enable;

  CheckGLErrors('Cleaning GL errors before setting GLSL uniform:');

  try
    if UniformValue is TSFBool then
      GLSLProgram.SetUniform(UniformName, TSFBool(UniformValue).Value) else
    if UniformValue is TSFLong then
      { Handling of SFLong also takes care of SFInt32. }
      GLSLProgram.SetUniform(UniformName, TSFLong(UniformValue).Value) else
    if UniformValue is TSFVec2f then
      GLSLProgram.SetUniform(UniformName, TSFVec2f(UniformValue).Value) else
    { Check TSFColor first, otherwise TSFVec3f would also catch and handle
      TSFColor. And we don't want this: for GLSL, color is passed
      as vec4 (so says the spec, I guess that the reason is that for GLSL most
      input/output colors are vec4). }
    if UniformValue is TSFColor then
      GLSLProgram.SetUniform(UniformName, Vector4Single(TSFColor(UniformValue).Value, 1.0)) else
    if UniformValue is TSFVec3f then
      GLSLProgram.SetUniform(UniformName, TSFVec3f(UniformValue).Value) else
    if UniformValue is TSFVec4f then
      GLSLProgram.SetUniform(UniformName, TSFVec4f(UniformValue).Value) else
    if UniformValue is TSFRotation then
      GLSLProgram.SetUniform(UniformName, TSFRotation(UniformValue).Value) else
    if UniformValue is TSFMatrix3f then
      GLSLProgram.SetUniform(UniformName, TSFMatrix3f(UniformValue).Value) else
    if UniformValue is TSFMatrix4f then
      GLSLProgram.SetUniform(UniformName, TSFMatrix4f(UniformValue).Value) else
    if UniformValue is TSFFloat then
      GLSLProgram.SetUniform(UniformName, TSFFloat(UniformValue).Value) else
    if UniformValue is TSFDouble then
      { SFDouble also takes care of SFTime }
      GLSLProgram.SetUniform(UniformName, TSFDouble(UniformValue).Value) else

    { Double-precision vector and matrix types.

      Note that X3D spec specifies only mapping for SF/MFVec3d, 4d
      (not specifying any mapping for SF/MFVec2d, and all matrix types).
      And it specifies that they map to types float3, float4 ---
      which are not valid types in GLSL?

      So I simply ignore non-sensible specification, and take
      the reasonable approach: support all double-precision vectors and matrices,
      just like single-precision. }
    if UniformValue is TSFVec2d then
      GLSLProgram.SetUniform(UniformName, Vector2Single(TSFVec2d(UniformValue).Value)) else
    if UniformValue is TSFVec3d then
      GLSLProgram.SetUniform(UniformName, Vector3Single(TSFVec3d(UniformValue).Value)) else
    if UniformValue is TSFVec4d then
      GLSLProgram.SetUniform(UniformName, Vector4Single(TSFVec4d(UniformValue).Value)) else
    if UniformValue is TSFMatrix3d then
      GLSLProgram.SetUniform(UniformName, Matrix3Single(TSFMatrix3d(UniformValue).Value)) else
    if UniformValue is TSFMatrix4d then
      GLSLProgram.SetUniform(UniformName, Matrix4Single(TSFMatrix4d(UniformValue).Value)) else

    { Now repeat this for array types }
    if UniformValue is TMFBool then
      GLSLProgram.SetUniform(UniformName, TMFBool(UniformValue).Items) else
    if UniformValue is TMFLong then
      GLSLProgram.SetUniform(UniformName, TMFLong(UniformValue).Items) else
    if UniformValue is TMFVec2f then
      GLSLProgram.SetUniform(UniformName, TMFVec2f(UniformValue).Items) else
    if UniformValue is TMFColor then
    begin
      TempVec4f := TMFColor(UniformValue).Items.ToVector4Single(1.0);
      try
        GLSLProgram.SetUniform(UniformName, TempVec4f);
      finally FreeAndNil(TempVec4f) end;
    end else
    if UniformValue is TMFVec3f then
      GLSLProgram.SetUniform(UniformName, TMFVec3f(UniformValue).Items) else
    if UniformValue is TMFVec4f then
      GLSLProgram.SetUniform(UniformName, TMFVec4f(UniformValue).Items) else
    if UniformValue is TMFRotation then
      GLSLProgram.SetUniform(UniformName, TMFRotation(UniformValue).Items) else
    if UniformValue is TMFMatrix3f then
      GLSLProgram.SetUniform(UniformName, TMFMatrix3f(UniformValue).Items) else
    if UniformValue is TMFMatrix4f then
      GLSLProgram.SetUniform(UniformName, TMFMatrix4f(UniformValue).Items) else
    if UniformValue is TMFFloat then
      GLSLProgram.SetUniform(UniformName, TMFFloat(UniformValue).Items) else
    if UniformValue is TMFDouble then
    begin
      TempF := TMFDouble(UniformValue).Items.ToSingle;
      try
        GLSLProgram.SetUniform(UniformName, TempF);
      finally FreeAndNil(TempF) end;
    end else
    if UniformValue is TMFVec2d then
    begin
      TempVec2f := TMFVec2d(UniformValue).Items.ToVector2Single;
      try
        GLSLProgram.SetUniform(UniformName, TempVec2f);
      finally FreeAndNil(TempVec2f) end;
    end else
    if UniformValue is TMFVec3d then
    begin
      TempVec3f := TMFVec3d(UniformValue).Items.ToVector3Single;
      try
        GLSLProgram.SetUniform(UniformName, TempVec3f);
      finally FreeAndNil(TempVec3f) end;
    end else
    if UniformValue is TMFVec4d then
    begin
      TempVec4f := TMFVec4d(UniformValue).Items.ToVector4Single;
      try
        GLSLProgram.SetUniform(UniformName, TempVec4f);
      finally FreeAndNil(TempVec4f) end;
    end else
    if UniformValue is TMFMatrix3d then
    begin
      TempMat3f := TMFMatrix3d(UniformValue).Items.ToMatrix3Single;
      try
        GLSLProgram.SetUniform(UniformName, TempMat3f);
      finally FreeAndNil(TempMat3f) end;
    end else
    if UniformValue is TMFMatrix4d then
    begin
      TempMat4f := TMFMatrix4d(UniformValue).Items.ToMatrix4Single;
      try
        GLSLProgram.SetUniform(UniformName, TempMat4f);
      finally FreeAndNil(TempMat4f) end;
    end else

      VRMLWarning(vwSerious, 'Setting uniform GLSL variable from X3D field type "' + UniformValue.VRMLTypeName + '" not supported');

    { Invalid glUniform call, that specifies wrong uniform variable type,
      may cause OpenGL error "invalid operation". We want to catch it,
      and convert into appropriate nice VRML warning.
      We cleaned GL error before doing SetUniform, so if there's an error
      now --- we know it's because of SetUniform.

      CheckGLError below will raise EOpenGLError, will be catched
      and converter to VRMLWarning below. }
    CheckGLErrors;

    { TODO: other field types, full list is in X3D spec in
      "OpenGL shading language (GLSL) binding".
      Remaining:
        SF/MFNode,
        SF/MFImage }

  except
    { X3D spec "OpenGL shading language (GLSL) binding" says
      "If the name is not available as a uniform variable in the
      provided shader source, the values of the node shall be ignored"
      (although it says when talking about "Vertex attributes",
      seems they mixed attributes and uniforms meaning in spec?).

      So we catch EGLSLUniformNotFound and report it through
      VRMLWarning(vwIgnorable, ...). }
    on E: EGLSLUniformNotFound do
    begin
      VRMLWarning(vwIgnorable, 'ComposedShader specifies uniform variable ' +
        'name not found (or not used) in the shader source: ' +
        E.Message);
    end;

    on E: EOpenGLError do
    begin
      VRMLWarning(vwSerious,
        Format('When setting GLSL uniform variable "%s": ', [UniformName])
        + E.Message);
    end;
  end;

  { TODO: this should restore previously bound program }
  GLSLProgram.Disable;
end;

procedure TVRMLOpenGLRendererContextCache.EventReceiveGLSLUniform(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  I: Integer;
  UniformName: string;
  GLSLProgramCache: PGLSLProgramCache;
  EventsProcessor: TObject;
begin
  if Event.ParentExposedField = nil then
    UniformName := Event.Name else
    UniformName := Event.ParentExposedField.Name;

  { We need to find GLSLProgram instance, to know which GLSL program
    actually has this uniform variable. We can do it: Event.ParentNode
    should point to appropriate ComposedShader node, so we can find
    corresponding GLSLProgramCaches item, and this will contain
    needed GLSLProgram. }

  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];

    if GLSLProgramCache^.ProgramNode = Event.ParentNode then
    begin
      SetUniformFromField(GLSLProgramCache^.GLSLProgram, UniformName, Value);

      { Although ExposedEvents implementation already sends notification
        about changes to ParentEventsProcessor, we can also get here
        by eventIn invocation (which doesn't trigger
        ParentEventsProcessor.ChangedFields, since it doesn't change a field...).
        So we should explicitly do DoPostRedisplay here, to make sure
        it gets called when uniform changed. }

      EventsProcessor := GLSLProgramCache^.ProgramNode.ParentEventsProcessor;
      if EventsProcessor <> nil then
        (EventsProcessor as TVRMLScene).DoPostRedisplay;

      Exit;
    end;
  end;

  VRMLWarning(vwSerious, Format(
    'INTERNAL ERROR, we can continue but please report a bug: uniform variable "%s" should be set from event now, but it turns out that GLSL program for this event''s ComposedShader node is not in the cache',
    [Event.Name]));
end;

function TVRMLOpenGLRendererContextCache.GLSLProgram_IncReference(
  ProgramNode: TNodeComposedShader): TGLSLProgram;

  procedure LoadGLSLProgram(GLSLProgram: TGLSLProgram;
    ProgramNode: TNodeComposedShader);
  var
    I: Integer;
    Part: TNodeShaderPart;
    Source: String;
    HasAnyShader: boolean;
    IDecls: TVRMLInterfaceDeclarationsList;
    UniformField: TVRMLField;
    UniformEvent: TVRMLEvent;
  begin
    HasAnyShader := false;

    { Iterate over ProgramNode.FdParts, looking for vertex shaders
      and fragment shaders. Note that more than one vertex/fragment shader
      is OK (as long as each has only one main() entry, OpenGL will check
      this when linking program). }

    for I := 0 to ProgramNode.FdParts.Count - 1 do
      if ProgramNode.FdParts.Items[I] is TNodeShaderPart then
      begin
        Part := TNodeShaderPart(ProgramNode.FdParts.Items[I]);

        if Part.FdType.Value = 'VERTEX' then
        begin
          Source := Part.LoadContents;
          if Part.UsedFullUrl <> '' then
          begin
            GLSLProgram.AttachVertexShader(Source);
            HasAnyShader := true;
          end;
        end else

        if Part.FdType.Value = 'FRAGMENT' then
        begin
          Source := Part.LoadContents;
          if Part.UsedFullUrl <> '' then
          begin
            GLSLProgram.AttachFragmentShader(Source);
            HasAnyShader := true;
          end;
        end else

          VRMLWarning(vwSerious, Format('Unknown type for ShaderPart: "%s"',
            [Part.FdType.Value]));
      end;

    if not HasAnyShader then
      raise EGLSLError.Create('No vertex and no fragment shader for GLSL program');

    GLSLProgram.Link(true);

    IDecls := ProgramNode.InterfaceDeclarations;

    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;
      UniformEvent := IDecls.Items[I].Event;

      { Set initial value for this GLSL uniform variable,
        from VRML field or exposedField }

      if UniformField <> nil then
      begin
        { Ok, we have a field with a value (interface declarations with
          fields inside ComposedShader always have a value).
          So set GLSL uniform variable from this field. }
        SetUniformFromField(GLSLProgram, UniformField.Name, UniformField);
      end;

      { Allow future changing of this GLSL uniform variable,
        from VRML eventIn or exposedField }
      if (UniformField <> nil) and UniformField.Exposed then
        UniformField.ExposedEvents[false].OnReceive.AppendItem(@EventReceiveGLSLUniform) else
      if (UniformEvent <> nil) and UniformEvent.InEvent then
        UniformEvent.OnReceive.AppendItem(@EventReceiveGLSLUniform);
    end;
  end;

var
  I: Integer;
  GLSLProgramCache: PGLSLProgramCache;
begin
  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];

    if GLSLProgramCache^.ProgramNode = ProgramNode then
    begin
      Inc(GLSLProgramCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : GLSL program ' + ProgramNode.DescribeUsedUrls + ' : ',
        GLSLProgramCache^.References);
      {$endif}
      Exit(GLSLProgramCache^.GLSLProgram);
    end;
  end;

  { Initialize Result first, before calling GLSLProgramCaches.AppendItem.
    That's because in case of loading problems,
    we don't want to add program to cache (because caller would have
    no way to call GLSLProgram_DecReference later). }

  Result := TGLSLProgram.Create;
  try
    LoadGLSLProgram(Result, ProgramNode);
    ProgramNode.EventIsValidSend(true);
  except
    { In case of problems with initializing GLSL program, free the program
      and reraise exception. Caller of GLSLProgram_IncReference will
      decide what to do with it (TVRMLOpenGLRenderer will make VRMLWarning
      and record that this shader program failed to initialize by recording
      GLSLProgram = nil). }
    FreeAndNil(Result);
    ProgramNode.EventIsValidSend(false);
    raise;
  end;

  GLSLProgramCache := GLSLProgramCaches.AppendItem;
  GLSLProgramCache^.ProgramNode := ProgramNode;
  GLSLProgramCache^.References := 1;
  GLSLProgramCache^.GLSLProgram := Result;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : GLSL program ' + ProgramNode.DescribeUsedUrls + ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.GLSLProgram_DecReference(
  var GLSLProgram: TGLSLProgram);
var
  I, J: Integer;
  GLSLProgramCache: PGLSLProgramCache;
  IDecls: TVRMLInterfaceDeclarationsList;
  UniformField: TVRMLField;
  UniformEvent: TVRMLEvent;
begin
  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];
    if GLSLProgramCache^.GLSLProgram = GLSLProgram then
    begin
      Dec(GLSLProgramCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : GLSLProgram ' + GLSLProgramCache^.ProgramNode.DescribeUsedUrls + ' : ',
        GLSLProgramCache^.References);
      {$endif}
      if GLSLProgramCache^.References = 0 then
      begin
        { Remove EventReceiveGLSLUniform callback,
          reverting the work done in GLSLProgram_IncReference. }
        IDecls := GLSLProgramCache^.ProgramNode.InterfaceDeclarations;
        for J := 0 to IDecls.Count - 1 do
        begin
          UniformField := IDecls.Items[J].Field;
          UniformEvent := IDecls.Items[J].Event;

          if (UniformField <> nil) and UniformField.Exposed then
            UniformField.ExposedEvents[false].OnReceive.DeleteFirstEqual(@EventReceiveGLSLUniform) else
          if (UniformEvent <> nil) and UniformEvent.InEvent then
            UniformEvent.OnReceive.DeleteFirstEqual(@EventReceiveGLSLUniform);
        end;

        FreeAndNil(GLSLProgramCache^.GLSLProgram);
        GLSLProgramCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TVRMLOpenGLRendererContextCache.GLSLProgram_DecReference: no reference ' +
    'found to GLSL program');
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

function TVRMLOpenGLRendererContextCache.Shape_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  SSCache: PShapeCache;
begin
  for I := 0 to ShapeCaches.High do
  begin
    SSCache := ShapeCaches.Pointers[I];
    if (SSCache^.Attributes.Equals(AAttributes)) and
       (SSCache^.GeometryNode = AGeometryNode) and
       (SSCache^.State.Equals(AState)) and
       FogParametersEqual(
         SSCache^.FogNode, SSCache^.FogDistanceScaling,
                 AFogNode,         AFogDistanceScaling) then
    begin
      Inc(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Shape ', SSCache^.GLList, ' : ', SSCache^.References);
      {$endif}
      AGLList := SSCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.Shape_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  SSCache: PShapeCache;
begin
  SSCache := ShapeCaches.AppendItem;
  SSCache^.Attributes := AAttributes;
  SSCache^.GeometryNode := AGeometryNode;
  SSCache^.State := AState;
  SSCache^.FogNode := AFogNode;
  SSCache^.FogDistanceScaling := AFogDistanceScaling;
  SSCache^.GLList := AGLList;
  SSCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Shape ', SSCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.Shape_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  SSCache: PShapeCache;
begin
  for I := 0 to ShapeCaches.High do
  begin
    SSCache := ShapeCaches.Pointers[I];
    if SSCache^.GLList = GLList then
    begin
      Dec(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Shape ', SSCache^.GLList, ' : ', SSCache^.References);
      {$endif}
      if SSCache^.References = 0 then
      begin
        FreeAndNil(SSCache^.Attributes);
        FreeAndNil(SSCache^.State);
        glFreeDisplayList(SSCache^.GLList);
        ShapeCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.Shape_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

function TVRMLOpenGLRendererContextCache.ShapeNoTransform_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  SSCache: PShapeCache;
begin
  for I := 0 to ShapeNoTransformCaches.High do
  begin
    SSCache := ShapeNoTransformCaches.Pointers[I];
    if (SSCache^.Attributes.Equals(AAttributes)) and
       (SSCache^.GeometryNode = AGeometryNode) and
       (SSCache^.State.EqualsNoTransform(AState)) and
       FogParametersEqual(
         SSCache^.FogNode, SSCache^.FogDistanceScaling,
                 AFogNode,         AFogDistanceScaling) then
    begin
      Inc(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Shape NoTransform ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      AGLList := SSCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.ShapeNoTransform_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  SSCache: PShapeCache;
begin
  SSCache := ShapeNoTransformCaches.AppendItem;
  SSCache^.Attributes := AAttributes;
  SSCache^.GeometryNode := AGeometryNode;
  SSCache^.State := AState;
  SSCache^.FogNode := AFogNode;
  SSCache^.FogDistanceScaling := AFogDistanceScaling;
  SSCache^.GLList := AGLList;
  SSCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Shape NoTransform ', SSCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.ShapeNoTransform_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  SSCache: PShapeCache;
begin
  for I := 0 to ShapeNoTransformCaches.High do
  begin
    SSCache := ShapeNoTransformCaches.Pointers[I];
    if SSCache^.GLList = GLList then
    begin
      Dec(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Shape NoTransform ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      if SSCache^.References = 0 then
      begin
        FreeAndNil(SSCache^.Attributes);
        FreeAndNil(SSCache^.State);
        glFreeDisplayList(SSCache^.GLList);
        ShapeNoTransformCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.ShapeNoTransform_DecReference: ' +
    'no reference ' +
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
  RenderCache := RenderBeginCaches.AppendItem;
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
  RenderCache := RenderEndCaches.AppendItem;
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
    OnRadianceTransfer := TVRMLRenderingAttributes(Source).OnRadianceTransfer;
    OnVertexColor := TVRMLRenderingAttributes(Source).OnVertexColor;
    SmoothShading := TVRMLRenderingAttributes(Source).SmoothShading;
    ColorModulatorSingle := TVRMLRenderingAttributes(Source).ColorModulatorSingle;
    ColorModulatorByte := TVRMLRenderingAttributes(Source).ColorModulatorByte;
    UseLights := TVRMLRenderingAttributes(Source).UseLights;
    FirstGLFreeLight := TVRMLRenderingAttributes(Source).FirstGLFreeLight;
    LastGLFreeLight := TVRMLRenderingAttributes(Source).LastGLFreeLight;
    ControlMaterials := TVRMLRenderingAttributes(Source).ControlMaterials;
    ControlTextures := TVRMLRenderingAttributes(Source).ControlTextures;
    EnableTextures := TVRMLRenderingAttributes(Source).EnableTextures;
    FirstGLFreeTexture := TVRMLRenderingAttributes(Source).FirstGLFreeTexture;
    LastGLFreeTexture := TVRMLRenderingAttributes(Source).LastGLFreeTexture;
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
    (TVRMLRenderingAttributes(SecondValue).OnBeforeGLVertex = OnBeforeGLVertex) and
    (TVRMLRenderingAttributes(SecondValue).OnRadianceTransfer = OnRadianceTransfer) and
    (TVRMLRenderingAttributes(SecondValue).OnVertexColor = OnVertexColor) and
    (TVRMLRenderingAttributes(SecondValue).SmoothShading = SmoothShading) and
    (TVRMLRenderingAttributes(SecondValue).ColorModulatorSingle = ColorModulatorSingle) and
    (TVRMLRenderingAttributes(SecondValue).ColorModulatorByte = ColorModulatorByte) and
    (TVRMLRenderingAttributes(SecondValue).UseLights = UseLights) and
    (TVRMLRenderingAttributes(SecondValue).FirstGLFreeLight = FirstGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).LastGLFreeLight = LastGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).ControlMaterials = ControlMaterials) and
    (TVRMLRenderingAttributes(SecondValue).ControlTextures = ControlTextures) and
    (TVRMLRenderingAttributes(SecondValue).EnableTextures = EnableTextures) and
    (TVRMLRenderingAttributes(SecondValue).FirstGLFreeTexture = FirstGLFreeTexture) and
    (TVRMLRenderingAttributes(SecondValue).LastGLFreeTexture = LastGLFreeTexture) and
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
  FFirstGLFreeTexture := 0;
  FLastGLFreeTexture := -1;
  FControlMaterials := true;
  FControlTextures := true;
  FEnableTextures := true;
  FTextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;
  FTextureMagFilter := GL_LINEAR;
  FPointSize := 3.0;
  FUseFog := true;
  FBumpMappingMaximum := bmNone;
  FGLSLShaders := true;
end;

procedure TVRMLRenderingAttributes.SetOnBeforeGLVertex(
  const Value: TBeforeGLVertexProc);
begin
  FOnBeforeGLVertex := Value;
end;

procedure TVRMLRenderingAttributes.SetOnRadianceTransfer(
  const Value: TRadianceTransferFunction);
begin
  FOnRadianceTransfer := Value;
end;

procedure TVRMLRenderingAttributes.SetOnVertexColor(
  const Value: TVertexColorFunction);
begin
  FOnVertexColor := Value;
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

procedure TVRMLRenderingAttributes.SetFirstGLFreeLight(const Value: Cardinal);
begin
  FFirstGLFreeLight := Value;
end;

procedure TVRMLRenderingAttributes.SetLastGLFreeLight(const Value: integer);
begin
  FLastGLFreeLight := Value;
end;

procedure TVRMLRenderingAttributes.SetControlMaterials(const Value: boolean);
begin
  FControlMaterials := Value;
end;

procedure TVRMLRenderingAttributes.SetControlTextures(const Value: boolean);
begin
  FControlTextures := Value;
end;

procedure TVRMLRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  FEnableTextures := Value;
end;

procedure TVRMLRenderingAttributes.SetFirstGLFreeTexture(const Value: Cardinal);
begin
  FFirstGLFreeTexture := Value;
end;

procedure TVRMLRenderingAttributes.SetLastGLFreeTexture(const Value: integer);
begin
  FLastGLFreeTexture := Value;
end;

procedure TVRMLRenderingAttributes.SetTextureMinFilter(const Value: TGLint);
begin
  FTextureMinFilter := Value;
end;

procedure TVRMLRenderingAttributes.SetTextureMagFilter(const Value: TGLint);
begin
  FTextureMagFilter := Value;
end;

procedure TVRMLRenderingAttributes.SetPointSize(const Value: TGLFloat);
begin
  FPointSize := Value;
end;

procedure TVRMLRenderingAttributes.SetUseFog(const Value: boolean);
begin
  FUseFog := Value;
end;

procedure TVRMLRenderingAttributes.SetBumpMappingMaximum(
  const Value: TBumpMappingMethod);
begin
  FBumpMappingMaximum := Value;
end;

procedure TVRMLRenderingAttributes.SetGLSLShaders(const Value: boolean);
begin
  FGLSLShaders := Value;
end;

procedure TVRMLRenderingAttributes.SetPureGeometry(const Value: boolean);
begin
  FPureGeometry := Value;
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

{ TDynTextureImageReferenceArray --------------------------------------------- }

function TDynTextureImageReferenceArray.TextureNodeIndex(
  ANode: TVRMLTextureNode): integer;
begin
  for result := 0 to Count - 1 do
    if Items[result].Node = ANode then exit;
  result := -1;
end;

{ TDynTextureVideoReferenceArray --------------------------------------------- }

function TDynTextureVideoReferenceArray.TextureNodeIndex(
  ANode: TVRMLTextureNode): integer;
begin
  for result := 0 to Count - 1 do
    if Items[result].Node = ANode then exit;
  result := -1;
end;

{ TDynTextureCubeMapReferenceArray --------------------------------------------- }

function TDynTextureCubeMapReferenceArray.TextureNodeIndex(
  ANode: TNodeX3DEnvironmentTextureNode): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[result].Node = ANode then exit;
  result := -1;
end;

{ TDynTexture3DReferenceArray --------------------------------------------- }

function TDynTexture3DReferenceArray.TextureNodeIndex(
  ANode: TNodeX3DTexture3DNode): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[result].Node = ANode then exit;
  result := -1;
end;

{ TDynTextureDepthReferenceArray --------------------------------------------- }

function TDynTextureDepthReferenceArray.TextureNodeIndex(
  ANode: TNodeGeneratedShadowMap): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[result].Node = ANode then exit;
  result := -1;
end;

{ TDynGLSLProgramReferenceArray ---------------------------------------------- }

function TDynGLSLProgramReferenceArray.ProgramNodeIndex(
  ProgramNode: TNodeComposedShader): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[result].ProgramNode = ProgramNode then Exit;
  Result := -1;
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

  FLastGLFreeTexture := -1;

  FBumpMappingLightAmbientColor := DefaultBumpMappingLightAmbientColor;
  FBumpMappingLightDiffuseColor := DefaultBumpMappingLightDiffuseColor;

  { asumme that "steep" version of parallax mapping is possible }
  BmSteepParallaxMapping := true;

  FAttributes := AttributesClass.Create;
  TextureImageReferences := TDynTextureImageReferenceArray.Create;
  TextureVideoReferences := TDynTextureVideoReferenceArray.Create;
  TextureCubeMapReferences := TDynTextureCubeMapReferenceArray.Create;
  Texture3DReferences := TDynTexture3DReferenceArray.Create;
  TextureDepthReferences := TDynTextureDepthReferenceArray.Create;
  GLSLProgramReferences := TDynGLSLProgramReferenceArray.Create;
  TextureTransformUnitsUsedMore := TDynLongIntArray.Create;

  OwnsCache := ACache = nil;
  if OwnsCache then
    FCache := TVRMLOpenGLRendererContextCache.Create else
    FCache := ACache;
end;

destructor TVRMLOpenGLRenderer.Destroy;
begin
  UnprepareAll;

  FreeAndNil(TextureTransformUnitsUsedMore);
  FreeAndNil(TextureImageReferences);
  FreeAndNil(TextureVideoReferences);
  FreeAndNil(TextureCubeMapReferences);
  FreeAndNil(Texture3DReferences);
  FreeAndNil(TextureDepthReferences);
  FreeAndNil(GLSLProgramReferences);
  FreeAndNil(FAttributes);

  if OwnsCache then
    FreeAndNil(FCache);

  inherited;
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

procedure TVRMLOpenGLRenderer.Prepare(State: TVRMLGraphTraverseState);

  procedure PrepareFont(
    fsfam: TVRMLFontFamily;
    fsbold, fsitalic: boolean;
    TTF_Font: PTrueTypeFont);
  begin
    if not FontsReferences[fsfam, fsbold, fsitalic] then
    begin
      Cache.Fonts_IncReference(fsfam, fsbold, fsitalic, TTF_Font);
      FontsReferences[fsfam, fsbold, fsitalic] := true;
    end;
  end;

  procedure PrepareGLSLProgram;
  var
    I: Integer;
    ProgramNode: TNodeComposedShader;
    GLSLProgram: TGLSLProgram;
    GLSLProgramReference: PGLSLProgramReference;
    ExistingReferenceIndex: Integer;
  begin
    { prepare GLSLProgram }
    if (not Attributes.PureGeometry) and
       Attributes.GLSLShaders and
       (State.ParentShape <> nil) and
       (State.ParentShape.Appearance <> nil) then
    begin
      for I := 0 to State.ParentShape.Appearance.FdShaders.Items.Count - 1 do
      begin
        ProgramNode := State.ParentShape.Appearance.GLSLShader(I);
        if ProgramNode <> nil then
        begin
          ExistingReferenceIndex := GLSLProgramReferences.ProgramNodeIndex(ProgramNode);

          if ExistingReferenceIndex <> -1 then
          begin
            { This ProgramNode was already prepared.
              So just take it's GLSLProgram (to decide lower whether we can Break or not
              now). }
            GLSLProgram := GLSLProgramReferences.Items[ExistingReferenceIndex].GLSLProgram;
          end else
          begin
            try
              GLSLProgram := Cache.GLSLProgram_IncReference(ProgramNode);
              ProgramNode.EventIsSelectedSend(true);
            except
              { EGLSLError catches errors from Cache.GLSLProgram_IncReference,
                including GLShaders errors like
                EGLSLShaderCompileError or EGLSLProgramLinkError }
              on E: EGLSLError do
              begin
                VRMLWarning(vwSerious, 'Error when initializing GLSL shader : ' + E.Message);
                GLSLProgram := nil;
                ProgramNode.EventIsSelectedSend(false);
              end;
            end;

            { Whether GLSLProgram is nil or not (GLSLProgram_IncReference
              succeded or not), we add record to GLSLProgramReferences }
            GLSLProgramReference := GLSLProgramReferences.AppendItem;
            GLSLProgramReference^.ProgramNode := ProgramNode;
            GLSLProgramReference^.GLSLProgram := GLSLProgram;
          end;

          { Only if successfull, break. }
          if GLSLProgram <> nil then
            Break;
        end else
        begin
          { GLSLShader(I) is nil, so this is not appropriate node class
            or "language" field was bad.
            So at least send him "isSelected" = false, if it's X3DShaderNode. }
          if State.ParentShape.Appearance.FdShaders.Items[I] is TNodeX3DShaderNode then
            (State.ParentShape.Appearance.FdShaders.Items[I] as TNodeX3DShaderNode).EventIsSelectedSend(false);
        end;
      end;
    end;
  end;

  { Called when BumpMappingMethod <> bmNone and it's detected that bump mapping
    may be actually used. This is supposed to initialize anything related to
    BumpMapping. }
  procedure PrepareBumpMapping(Parallax: boolean);
  var
    ProgramDefines: string;
  begin
    case BumpMappingMethod of
      bmMultiTexDotNormalized:
        if TexNormalizationCube = 0 then
        begin
          TexNormalizationCube := MakeNormalizationCubeMap;
        end;

      bmGLSLNormal,
      bmGLSLParallax:
        if BmGLSLProgram[Parallax] = nil then
        begin
          BmGLSLProgram[Parallax] := TGLSLProgram.Create;

          { If BumpMappingMethod is in bmGLSLAll, then we checked in
            BumpMappingMethod that support is <> gsNone }
          Assert(BmGLSLProgram[Parallax].Support <> gsNone);

          try
            if Parallax then
            begin
              { ATI Mobility Radeon X1600 (on Mac Book Pro, computer "chantal")
                says that
                  #version must occur before any other statement in the program
                At the same time, NVidia requires this #version... To be
                absolutely clean, I just place #version here, at the very
                beginning of shader source. }
              ProgramDefines := '#version 110' + LineEnding;

              if BmSteepParallaxMapping then
                ProgramDefines += '#define STEEP' + LineEnding else
                ProgramDefines += '';

              BmGLSLProgram[Parallax].AttachVertexShader(
                ProgramDefines + {$I glsl_parallax_bump_mapping.vs.inc});
              BmGLSLProgram[Parallax].AttachFragmentShader(
                ProgramDefines + {$I glsl_parallax_bump_mapping.fs.inc});
            end else
            begin
              BmGLSLProgram[Parallax].AttachVertexShader({$I glsl_bump_mapping.vs.inc});
              BmGLSLProgram[Parallax].AttachFragmentShader({$I glsl_bump_mapping.fs.inc});
            end;

            BmGLSLProgram[Parallax].Link(true);

            if Log then
              WritelnLog('Bump mapping',
                Format('Compiled and linked GLSL program for ' +
                  'bump mapping. Parallax: %s (if true: steep parallax ' +
                  'with self-shadowing: %s).',
                  [BoolToStr[Parallax], BoolToStr[BmSteepParallaxMapping]]));
          except
            on E: EGLSLError do
            begin
              if Parallax and BmSteepParallaxMapping then
              begin
                { If we failed with compiling/linking steep parallax mapping,
                  retry without BmSteepParallaxMapping.
                  This happens e.g. on NVidia GeForce FX 5200
                  ("kocury home" computer). }
                if Log then
                  WritelnLog('Bump mapping', 'Steep parallax mapping program ' +
                    'not compiled or linked, falling back to classic parallax.');

                FreeAndNil(BmGLSLProgram[Parallax]);
                BmSteepParallaxMapping := false;
                PrepareBumpMapping(Parallax);
                Exit;
              end else
                raise;
            end;
          end;

          { tests: Writeln(BmGLSLProgram[Parallax].DebugInfo); }

          BmGLSLAttribObjectSpaceToTangent[Parallax] :=
            BmGLSLProgram[Parallax].CreateAttribute('object_space_to_tangent');

          BmGLSLProgram[Parallax].Enable;
          BmGLSLProgram[Parallax].SetUniform('light_position_world_space',
            BumpMappingLightPosition);
          BmGLSLProgram[Parallax].SetUniform('light_ambient_color',
            BumpMappingLightAmbientColor);
          BmGLSLProgram[Parallax].SetUniform('light_diffuse_color',
            BumpMappingLightDiffuseColor);

          { set uniform samplers, so that fragment shader has access
            to all bound textures }
          BmGLSLProgram[Parallax].SetUniform('tex_normal_map', 0);
          BmGLSLProgram[Parallax].SetUniform('tex_original', 1);
          if Parallax then
            BmGLSLProgram[Parallax].SetUniform('tex_height_map', 2);

          { TODO: this should restore previously bound program }
          BmGLSLProgram[Parallax].Disable;
        end;
    end;
  end;

  { Calculate things from TextureProperties node.
    If TextureProperties = @nil, they are taken from defaults
    (possibly in Attributes). }
  procedure HandleTexturePropertiesCore(
    TextureProperties: TNodeTextureProperties;
    out MinFilter, MagFilter: TGLint;
    out Anisotropy: TGLfloat);

    function StrToMinFilter(S: string): TGLint;
    begin
      S := UpperCase(S);

      if S = 'AVG_PIXEL' then Result := GL_LINEAR else
      if S = 'AVG_PIXEL_AVG_MIPMAP' then Result := GL_LINEAR_MIPMAP_LINEAR else
      if S = 'AVG_PIXEL_NEAREST_MIPMAP' then Result := GL_LINEAR_MIPMAP_NEAREST else
      if S = 'NEAREST_PIXEL_AVG_MIPMAP' then Result := GL_NEAREST_MIPMAP_LINEAR else
      if S = 'NEAREST_PIXEL_NEAREST_MIPMAP' then Result := GL_NEAREST_MIPMAP_NEAREST else
      if S = 'NEAREST_PIXEL' then Result := GL_NEAREST else

      if S = 'DEFAULT' then Result := Attributes.TextureMinFilter else

      if S = 'FASTEST' then Result := GL_NEAREST else
      if S = 'NICEST' then Result := GL_LINEAR_MIPMAP_LINEAR else

      if S = 'NEAREST' then
      begin
        VRMLWarning(vwSerious, Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
        Result := GL_NEAREST;
      end else

      begin
        Result := Attributes.TextureMinFilter;
        VRMLWarning(vwSerious, Format('Unknown texture minification filter "%s"', [S]));
      end;
    end;

    function StrToMagFilter(S: string): TGLint;
    begin
      S := UpperCase(S);

      if S = 'AVG_PIXEL' then Result := GL_LINEAR else
      if S = 'NEAREST_PIXEL' then Result := GL_NEAREST else

      if S = 'DEFAULT' then Result := Attributes.TextureMagFilter else

      if S = 'FASTEST' then Result := GL_NEAREST else
      if S = 'NICEST' then Result := GL_LINEAR else

      if S = 'NEAREST' then
      begin
        VRMLWarning(vwSerious, Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
        Result := GL_NEAREST;
      end else

      begin
        Result := Attributes.TextureMagFilter;
        VRMLWarning(vwSerious, Format('Unknown texture minification filter "%s"', [S]));
      end;
    end;

  begin { HandleTextureProperties }
    if TextureProperties <> nil then
    begin
      MinFilter := StrToMinFilter(TextureProperties.FdMinificationFilter.Value);
      MagFilter := StrToMagFilter(TextureProperties.FdMagnificationFilter.Value);
      Anisotropy := TextureProperties.FdAnisotropicDegree.Value;
    end else
    begin
      MinFilter := Attributes.TextureMinFilter;
      MagFilter := Attributes.TextureMagFilter;
      Anisotropy := 1;
    end;
  end { HandleTextureProperties };

  { Calculate things from TextureProperties node.
    If TextureProperties = @nil or not of TNodeTextureProperties class,
    they are taken from defaults (possibly in Attributes).

    This is useful when interpreting VRML/X3D files,
    as you have no guarantee user didn't place there some disallowed node
    in "textureProperties" field. }
  procedure HandleTextureProperties(
    TextureProperties: TVRMLNode;
    out MinFilter, MagFilter: TGLint;
    out Anisotropy: TGLfloat);
  begin
    if (TextureProperties = nil) or
       not (TextureProperties is TNodeTextureProperties) then
      HandleTexturePropertiesCore(nil, MinFilter, MagFilter, Anisotropy) else
      HandleTexturePropertiesCore(TNodeTextureProperties(TextureProperties),
        MinFilter, MagFilter, Anisotropy);
  end;

  const
    TextureRepeatToGL: array[boolean]of TGLenum = (
      { GL_CLAMP is useless if VRML doesn't allow to control texture border color,
        and CLAMP_TO_EDGE is the more natural clamping method anyway...
        Hm, but X3D specification seems to indicate that normal clamp is OpenGL's CLAMP,
        and CLAMP_TO_EDGE is available by TextureProperties.boundaryMode*.
        But until this will get implemented, it's much safer (and more sensible?)
        to use GL_CLAMP_TO_EDGE here. }
      GL_CLAMP_TO_EDGE,
      GL_REPEAT);

  { Do the necessary preparations for a non-multi texture node.
    TextureNode must be non-nil. }
  procedure PrepareSingle2DTexture(TextureNode: TVRMLTextureNode);
  var
    TextureImageReference: TTextureImageReference;
    TextureVideoReference: TTextureVideoReference;
    HeightMapGrayscale: TGrayscaleImage;
    OriginalTexture: TEncodedImage;
    MinFilter, MagFilter: TGLint;
    Anisotropy: TGLfloat;
    TextureWrap: TTextureWrap2D;
  begin
    if (TextureImageReferences.TextureNodeIndex(TextureNode) = -1) and
       (TextureVideoReferences.TextureNodeIndex(TextureNode) = -1) then
    begin
      TextureNode.ImagesVideosCache := Cache;

      HandleTexturePropertiesCore(TextureNode.TextureProperties,
        MinFilter, MagFilter, Anisotropy);

      TextureWrap[0] := TextureRepeatToGL[TextureNode.RepeatS];
      TextureWrap[1] := TextureRepeatToGL[TextureNode.RepeatT];

      if TextureNode.IsTextureImage then
      begin
        TextureImageReference.Node := TextureNode;
        TextureImageReference.GLName := Cache.TextureImage_IncReference(
          TextureNode.TextureImage,
          TextureNode.TextureUsedFullUrl,
          TextureNode,
          MinFilter,
          MagFilter,
          Anisotropy,
          TextureWrap,
          Attributes.ColorModulatorByte,
          { This way, our AlphaChannelType is calculated (or taken from cache)
            by TextureImage_IncReference }
          TextureImageReference.AlphaChannelType);

        { TODO: for now, bump mapping is used only if the node has normal texture
          too. It should be possible to use bump mapping even if the node is colored
          only by material (in this case we should remember to still generate
          texture coords etc.).

          TODO: Also, now for each texture, there must always be the same bump map
          (since we store it in the same TextureImageReference }

        TextureImageReference.NormalMap := 0;
        if (BumpMappingMethod <> bmNone) and
           (State.ParentShape <> nil) and
           (State.ParentShape.NormalMap <> nil) then
        begin
          State.ParentShape.NormalMap.ImagesVideosCache := Cache;
          if State.ParentShape.NormalMap.IsTextureImage then
          begin
            { TODO: normal map textures should be shared by Cache }
            TextureImageReference.NormalMap := LoadGLTexture(
              State.ParentShape.NormalMap.TextureImage,
              GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, TextureWrap);
          end;
        end;

        TextureImageReference.HeightMap := 0;
        if (BumpMappingMethod <> bmNone) and
           (State.ParentShape <> nil) and
           (State.ParentShape.HeightMap <> nil) then
        begin
          State.ParentShape.HeightMap.ImagesVideosCache := Cache;
          if State.ParentShape.HeightMap.IsTextureImage then
          begin
            { TODO: height map textures should be shared by Cache }

            OriginalTexture := State.ParentShape.HeightMap.TextureImage;

            { Calculate HeightMapGrayscale }
            { TODO: this is not nice to convert here, we should load
              straight to TGrayscalaImage }
            if OriginalTexture is TRGBImage then
              HeightMapGrayscale := TRGBImage(OriginalTexture).ToGrayscale else
            if OriginalTexture is TGrayscaleImage then
              HeightMapGrayscale := TGrayscaleImage(OriginalTexture) else
              HeightMapGrayscale := nil;

            if HeightMapGrayscale <> nil then
            try
              TextureImageReference.HeightMap :=
                LoadGLTexture(HeightMapGrayscale,
                  GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, TextureWrap);
              TextureImageReference.HeightMapScale :=
                State.ParentShape.HeightMapScale;
            finally
              if HeightMapGrayscale <> OriginalTexture then
                FreeAndNil(HeightMapGrayscale);
            end;
          end;
        end;

        TextureImageReferences.AppendItem(TextureImageReference);

        { Note: do PrepareBumpMapping *after* TextureImageReferences.AppendItem.
          This way in case of errors (some GLSL errors on current OpenGL ?)
          we exit with nice state, able to free this texture reference later. }

        if TextureImageReference.NormalMap <> 0 then
          PrepareBumpMapping(
            (TextureImageReference.HeightMap <> 0) and
            (BumpMappingMethod >= bmGLSLParallax));
      end else
      if TextureNode.IsTextureVideo then
      begin
        TextureVideoReference.Node :=
          { Only TNodeMovieTexture can have IsTextureVideo }
          TextureNode as TNodeMovieTexture;
        TextureVideoReference.GLVideo := Cache.TextureVideo_IncReference(
          TextureNode.TextureVideo,
          TextureNode.TextureUsedFullUrl,
          TextureVideoReference.Node,
          MinFilter,
          MagFilter,
          Anisotropy,
          TextureWrap,
          Attributes.ColorModulatorByte,
          { This way, our AlphaChannelType is calculated (or taken from cache)
            by TextureVideo_IncReference }
          TextureVideoReference.AlphaChannelType);

        TextureVideoReferences.AppendItem(TextureVideoReference);
      end;
    end;
  end;

  { Do the necessary preparations for a cube env map texture node.
    CubeTexture must be non-nil. }
  procedure PrepareSingleComposedCubeTexture(CubeTexture: TNodeComposedCubeMapTexture);

    { Checks is given side has non-nil valid node class,
      and then if image there can be loaded. }
    function SideLoaded(SideField: TSFNode): boolean;
    var
      SideTex: TVRMLTextureNode;
    begin
      Result :=
        (SideField.Value <> nil) and
        (SideField.Value is TVRMLTextureNode);
      if Result then
      begin
        SideTex := TVRMLTextureNode(SideField.Value);
        SideTex.ImagesVideosCache := Cache;
        Result := SideTex.IsTextureImage;

        if Result and not (SideTex.TextureImage is TImage) then
        begin
          VRMLWarning(vwIgnorable, 'ComposedCubeMapTexture cannot contain S3TC images, as we have to rotate images within, and we cannot do this (fast) with compressed textures');
          Result := false;
        end;
      end;
    end;

  var
    MinFilter, MagFilter: TGLint;
    Anisotropy: TGLfloat;
    TextureCubeMapReference: TTextureCubeMapReference;
    BackRot, FrontRot, LeftRot, RightRot: TImage;
  begin
    if TextureCubeMapReferences.TextureNodeIndex(CubeTexture) <> -1 then
      { Already loaded, nothing to do }
      Exit;

    if not GL_ARB_texture_cube_map then
    begin
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
      Exit;
    end;

    if not (SideLoaded(CubeTexture.FdBack) and
       SideLoaded(CubeTexture.FdBottom) and
       SideLoaded(CubeTexture.FdFront) and
       SideLoaded(CubeTexture.FdLeft) and
       SideLoaded(CubeTexture.FdRight) and
       SideLoaded(CubeTexture.FdTop)) then
    begin
      VRMLWarning(vwSerious, 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
      Exit;
    end;

    HandleTextureProperties(CubeTexture.FdTextureProperties.Value,
      MinFilter, MagFilter, Anisotropy);

    try
      { To match expected orientation for OpenGL, we have to rotate images.
        (source images are oriented as for VRML Background.)
        We safely cast them to TImage below, SideLoaded above checked
        that they are indeed of TImage class. }
      BackRot  := (TVRMLTextureNode(CubeTexture.FdBack .Value).TextureImage as TImage).MakeRotated(2);
      FrontRot := (TVRMLTextureNode(CubeTexture.FdFront.Value).TextureImage as TImage).MakeRotated(2);
      LeftRot  := (TVRMLTextureNode(CubeTexture.FdLeft .Value).TextureImage as TImage).MakeRotated(2);
      RightRot := (TVRMLTextureNode(CubeTexture.FdRight.Value).TextureImage as TImage).MakeRotated(2);

      TextureCubeMapReference.Node := CubeTexture;
      TextureCubeMapReference.GLName := Cache.TextureCubeMap_IncReference(
        CubeTexture,
        MinFilter, MagFilter, Anisotropy,
        { positive x } RightRot,
        { negative x } LeftRot,
        { positive y } TVRMLTextureNode(CubeTexture.FdTop   .Value).TextureImage as TImage,
        { negative y } TVRMLTextureNode(CubeTexture.FdBottom.Value).TextureImage as TImage,
        { positive z } BackRot,
        { negative z } FrontRot,
        TextureCubeMapReference.AlphaChannelType);
      TextureCubeMapReferences.AppendItem(TextureCubeMapReference);

    finally
      FreeAndNil(BackRot);
      FreeAndNil(FrontRot);
      FreeAndNil(LeftRot);
      FreeAndNil(RightRot);
    end;
  end;

  { Do the necessary preparations for a cube env map texture node.
    CubeTexture must be non-nil. }
  procedure PrepareSingleImageCubeTexture(CubeTexture: TNodeImageCubeMapTexture);
  var
    MinFilter, MagFilter: TGLint;
    Anisotropy: TGLfloat;
    TextureCubeMapReference: TTextureCubeMapReference;
    DDS: TDDSImage;
  begin
    if TextureCubeMapReferences.TextureNodeIndex(CubeTexture) <> -1 then
      { Already loaded, nothing to do }
      Exit;

    if not GL_ARB_texture_cube_map then
    begin
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
      Exit;
    end;

    DDS := CubeTexture.LoadImage;
    { If CubeTexture doesn't contain anything useful, just exit.
      CubeTexture.LoadImage already did necessary VRMLWarnings. }
    if DDS = nil then Exit;

    try

      HandleTextureProperties(CubeTexture.FdTextureProperties.Value,
        MinFilter, MagFilter, Anisotropy);

      { TODO: this is a quick and dirty method:
        - We call LoadImage each time, while load calls should
          be minimized (to avoid loading image many times, but also
          to avoid making repeated warnings in case image fails).
          Should be cached, like for 2D texture nodes.
        - We do not use cube map mipmaps stored inside DDS file.
        - We crash ("as") on S3TC compressed cube maps.
      }

      TextureCubeMapReference.Node := CubeTexture;
      TextureCubeMapReference.GLName := Cache.TextureCubeMap_IncReference(
        CubeTexture,
        MinFilter, MagFilter, Anisotropy,
        DDS.CubeMapImage(dcsPositiveX) as TImage,
        DDS.CubeMapImage(dcsNegativeX) as TImage,
        { Swap meaning of positive/negative Y faces from DDS,
          see TDDSCubeMapSide for explanation. }
        DDS.CubeMapImage(dcsNegativeY) as TImage,
        DDS.CubeMapImage(dcsPositiveY) as TImage,
        DDS.CubeMapImage(dcsPositiveZ) as TImage,
        DDS.CubeMapImage(dcsNegativeZ) as TImage,
        TextureCubeMapReference.AlphaChannelType);
      TextureCubeMapReferences.AppendItem(TextureCubeMapReference);

    finally FreeAndNil(DDS); end;
  end;

  procedure PrepareSingleGeneratedCubeTexture(CubeTexture: TNodeGeneratedCubeMapTexture);
  var
    MinFilter, MagFilter: TGLint;
    Anisotropy: TGLfloat;
    TextureCubeMapReference: TTextureCubeMapReference;
    InitialImage: TImage;
    Size: Cardinal;
    NeedsMipmaps: boolean;
  begin
    if TextureCubeMapReferences.TextureNodeIndex(CubeTexture) <> -1 then
      { Already loaded, nothing to do }
      Exit;

    if not GL_ARB_texture_cube_map then
    begin
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
      Exit;
    end;

    HandleTextureProperties(CubeTexture.FdTextureProperties.Value,
      MinFilter, MagFilter, Anisotropy);

    { calculate MinFilter, MagFilter, Anisotropy, NeedsMipmaps }
    NeedsMipmaps := TextureMinFilterNeedsMipmaps(MinFilter);
    if NeedsMipmaps and not HasGenerateMipmap then
    begin
      VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
        limits, so it may be impossible to predict by VRML author,
        so it's "ignorable" warning. },
        'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for GeneratedCubeMapTexture');
      MinFilter := GL_LINEAR;
      NeedsMipmaps := false;
    end;
    TextureCubeMapReference.GeneratedNeedsMipmaps := NeedsMipmaps;

    { calculate Size }
    Size := Max(CubeTexture.FdSize.Value, 0);
    if not IsCubeMapTextureSized(Size) then
    begin
      Size := ResizeToCubeMapTextureSize(Size);
      VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
        limits, so it may be impossible to predict by VRML author,
        so it's "ignorable" warning. },
        Format('Cube map texture size %d is incorrect (cube map texture size must be a power of two, > 0 and <= GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = %d), corrected to %d',
          [ CubeTexture.FdSize.Value,
            GLMaxCubeMapTextureSizeARB,
            Size]));
    end;
    TextureCubeMapReference.GeneratedSize := Size;

    InitialImage := TRGBImage.Create(Size, Size);
    try
      { Fill with deliberately stupid (but constant) color,
        to recognize easily GeneratedCubeMapTexture which don't have textures
        updated. }
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

      TextureCubeMapReference.Node := CubeTexture;
      TextureCubeMapReference.GLName := Cache.TextureCubeMap_IncReference(
        CubeTexture,
        MinFilter, MagFilter, Anisotropy,
        InitialImage, InitialImage,
        InitialImage, InitialImage,
        InitialImage, InitialImage,
        TextureCubeMapReference.AlphaChannelType);
      TextureCubeMapReferences.AppendItem(TextureCubeMapReference);
    finally FreeAndNil(InitialImage) end;
  end;

  { Do the necessary preparations for a 3d texture node.
    Texture must be non-nil. }
  procedure PrepareSingleTexture3D(Texture: TNodeX3DTexture3DNode);
  var
    MinFilter, MagFilter: TGLint;
    Anisotropy: TGLfloat;
    TextureReference: TTexture3DReference;
    DDS: TDDSImage;
    Image: TEncodedImage;
    TextureWrap: TTextureWrap3D;
  begin
    if Texture3DReferences.TextureNodeIndex(Texture) <> -1 then
      { Already loaded, nothing to do }
      Exit;

    if not GL_EXT_texture3D then
    begin
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support EXT_texture3D, cannot use Texture3D nodes');
      Exit;
    end;

    Image := Texture.LoadImage(DDS);
    { If Texture doesn't contain anything useful, just exit.
      Texture.LoadImage already did necessary VRMLWarnings. }
    if Image = nil then Exit;

    try

      HandleTextureProperties(Texture.FdTextureProperties.Value,
        MinFilter, MagFilter, Anisotropy);

      { calculate TextureWrap }
      TextureWrap[0] := TextureRepeatToGL[Texture.FdRepeatS.Value];
      TextureWrap[1] := TextureRepeatToGL[Texture.FdRepeatT.Value];
      TextureWrap[2] := TextureRepeatToGL[Texture.FdRepeatR.Value];

      { TODO: this is a quick and dirty method:
        - We call LoadImage each time, while load calls should
          be minimized (to avoid loading image many times, but also
          to avoid making repeated warnings in case image fails).
          Should be cached, like for 2D texture nodes.
        - We do not use texture 3d mipmaps stored inside DDS file.
        - We crash ("as") on S3TC compressed images.
          (although DDS doesn't allow compressed 3d textures, so this
          is not so important now.)
      }

      TextureReference.Node := Texture;
      TextureReference.GLName := Cache.Texture3D_IncReference(
        Texture, MinFilter, MagFilter, Anisotropy,
        TextureWrap, Image as TImage,
        TextureReference.AlphaChannelType);
      Texture3DReferences.AppendItem(TextureReference);

    finally
      FreeAndNil(Image);
      FreeAndNil(DDS);
    end;
  end;

  { Do the necessary preparations for a 3d texture node.
    Texture must be non-nil. }
  procedure PrepareSingleTextureDepth(Texture: TNodeGeneratedShadowMap);
  var
    TextureReference: TTextureDepthReference;
  begin
    if TextureDepthReferences.TextureNodeIndex(Texture) <> -1 then
      { Already loaded, nothing to do }
      Exit;

    if not GL_ARB_depth_texture then
    begin
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_depth_texture, cannot use GeneratedShadowMap nodes');
      Exit;
    end;

    { TODO: fix Texture.FdSize.Value if needed }
    TextureReference.GeneratedSize := Texture.FdSize.Value;

    TextureReference.Node := Texture;
    TextureReference.GLName := Cache.TextureDepth_IncReference(
      Texture, TextureReference.GeneratedSize);

    TextureDepthReferences.AppendItem(TextureReference);
  end;

  { Do the necessary preparations for a multi-texture node.
    MultiTexture must be non-nil. }
  procedure PrepareMultiTexture(MultiTexture: TNodeMultiTexture);
  var
    ChildTex: TVRMLNode;
    I: Integer;
  begin
    for I := 0 to MultiTexture.FdTexture.Items.Count - 1 do
    begin
      ChildTex := MultiTexture.FdTexture.Items.Items[I];
      if ChildTex <> nil then
      begin
        if ChildTex is TNodeMultiTexture then
          VRMLWarning(vwSerious, 'Child of MultiTexture node cannot be another MultiTexture node') else
        if ChildTex is TNodeComposedCubeMapTexture then
          PrepareSingleComposedCubeTexture(TNodeComposedCubeMapTexture(ChildTex)) else
        if ChildTex is TNodeImageCubeMapTexture then
          PrepareSingleImageCubeTexture(TNodeImageCubeMapTexture(ChildTex)) else
        if ChildTex is TNodeGeneratedCubeMapTexture then
          PrepareSingleGeneratedCubeTexture(TNodeGeneratedCubeMapTexture(ChildTex)) else
        if ChildTex is TVRMLTextureNode then
          PrepareSingle2DTexture(TVRMLTextureNode(ChildTex)) else
        if ChildTex is TNodeX3DTexture3DNode then
          PrepareSingleTexture3D(TNodeX3DTexture3DNode(ChildTex)) else
        if ChildTex is TNodeGeneratedShadowMap then
          PrepareSingleTextureDepth(TNodeGeneratedShadowMap(ChildTex));
      end;
    end;
  end;

  { Prepare texture.

    Accepts multi texture or not-multi texture nodes, accepts (and ignores)
    also @nil as TextureNode. }
  procedure PrepareTexture(TextureNode: TNodeX3DTextureNode);
  begin
    { Conditions below describing when the texture is added to the cache
      (along with "...TextureNodeIndex(TextureNode) = -1" inside
      PrepareTextureNonMulti)
      are reflected in PreparedTextureAlphaChannelType interface. }

    if (not Attributes.PureGeometry) and
       (TextureNode <> nil) then
    begin
      if TextureNode is TNodeMultiTexture then
        PrepareMultiTexture(TNodeMultiTexture(TextureNode)) else
      if TextureNode is TNodeComposedCubeMapTexture then
        PrepareSingleComposedCubeTexture(TNodeComposedCubeMapTexture(TextureNode)) else
      if TextureNode is TNodeImageCubeMapTexture then
        PrepareSingleImageCubeTexture(TNodeImageCubeMapTexture(TextureNode)) else
      if TextureNode is TNodeGeneratedCubeMapTexture then
        PrepareSingleGeneratedCubeTexture(TNodeGeneratedCubeMapTexture(TextureNode)) else
      if TextureNode is TVRMLTextureNode then
        PrepareSingle2DTexture(TVRMLTextureNode(TextureNode)) else
      if TextureNode is TNodeX3DTexture3DNode then
        PrepareSingleTexture3D(TNodeX3DTexture3DNode(TextureNode)) else
      if TextureNode is TNodeGeneratedShadowMap then
        PrepareSingleTextureDepth(TNodeGeneratedShadowMap(TextureNode));
    end;
  end;

var
  FontStyle: TNodeFontStyle_2;
begin
  { przygotuj font }
  if State.ParentShape = nil then
    PrepareFont(
      State.LastNodes.FontStyle.Family,
      State.LastNodes.FontStyle.Bold,
      State.LastNodes.FontStyle.Italic,
      State.LastNodes.FontStyle.TTF_Font) else
  if (State.ParentShape.FdGeometry.Value <> nil) and
     (State.ParentShape.FdGeometry.Value is TNodeText) then
  begin
    { We know that TNodeText(State.ParentShape.FdGeometry.Value)
      will be the shape node rendered along with this State.
      That's how it works in VRML 2.0: State actually contains
      reference to Shape that contains reference to geometry node,
      which means that actually State contains rendered node too. }
    FontStyle := TNodeText(State.ParentShape.FdGeometry.Value).FontStyle;
    if FontStyle = nil then
      PrepareFont(
        TNodeFontStyle_2.DefaultFamily,
        TNodeFontStyle_2.DefaultBold,
        TNodeFontStyle_2.DefaultItalic,
        TNodeFontStyle_2.DefaultTTF_Font) else
      PrepareFont(
        FontStyle.Family,
        FontStyle.Bold,
        FontStyle.Italic,
        FontStyle.TTF_Font);
  end else
  if (State.ParentShape.FdGeometry.Value <> nil) and
     (State.ParentShape.FdGeometry.Value is TNodeText3D) then
  begin
    { We know that TNodeText3D(State.ParentShape.FdGeometry.Value)
      will be the shape node rendered along with this State.
      That's how it works in VRML 2.0: State actually contains
      reference to Shape that contains reference to geometry node,
      which means that actually State contains rendered node too. }
    FontStyle := TNodeText3D(State.ParentShape.FdGeometry.Value).FontStyle;
    if FontStyle = nil then
      PrepareFont(
        TNodeFontStyle_2.DefaultFamily,
        TNodeFontStyle_2.DefaultBold,
        TNodeFontStyle_2.DefaultItalic,
        TNodeFontStyle_2.DefaultTTF_Font) else
      PrepareFont(
        FontStyle.Family,
        FontStyle.Bold,
        FontStyle.Italic,
        FontStyle.TTF_Font);
  end;

  PrepareTexture(State.Texture);

  PrepareGLSLProgram;
end;

procedure TVRMLOpenGLRenderer.Unprepare(Node: TVRMLNode);

  procedure UnprepareSingle2DTexture(Tex: TVRMLTextureNode);
  var
    i: integer;
  begin
    i := TextureImageReferences.TextureNodeIndex(Tex);
    if i >= 0 then
    begin
      Cache.TextureImage_DecReference(TextureImageReferences.Items[i].GLName);
      TextureImageReferences.Delete(i, 1);
    end;

    i := TextureVideoReferences.TextureNodeIndex(Tex);
    if i >= 0 then
    begin
      Cache.TextureVideo_DecReference(TextureVideoReferences.Items[i].GLVideo);
      TextureVideoReferences.Delete(i, 1);
    end;
  end;

  procedure UnprepareSingleCubeMapTexture(Tex: TNodeX3DEnvironmentTextureNode);
  var
    i: integer;
  begin
    i := TextureCubeMapReferences.TextureNodeIndex(Tex);
    if i >= 0 then
    begin
      Cache.TextureCubeMap_DecReference(TextureCubeMapReferences.Items[i].GLName);
      TextureCubeMapReferences.Delete(i, 1);
    end;
  end;

  procedure UnprepareSingleTexture3D(Tex: TNodeX3DTexture3DNode);
  var
    i: integer;
  begin
    i := Texture3DReferences.TextureNodeIndex(Tex);
    if i >= 0 then
    begin
      Cache.Texture3D_DecReference(Texture3DReferences.Items[i].GLName);
      Texture3DReferences.Delete(i, 1);
    end;
  end;

  procedure UnprepareSingleTextureDepth(Tex: TNodeGeneratedShadowMap);
  var
    i: integer;
  begin
    i := TextureDepthReferences.TextureNodeIndex(Tex);
    if i >= 0 then
    begin
      Cache.TextureDepth_DecReference(TextureDepthReferences.Items[i].GLName);
      TextureDepthReferences.Delete(i, 1);
    end;
  end;

  procedure UnprepareMultiTexture(Tex: TNodeMultiTexture);
  var
    TexItem: TVRMLNode;
    I: Integer;
  begin
    for I := 0 to Tex.FdTexture.Count - 1 do
    begin
      TexItem := Tex.FdTexture.Items[I];
      if TexItem = nil then Continue;

      if TexItem is TVRMLTextureNode then
        UnprepareSingle2DTexture(TVRMLTextureNode(TexItem)) else
      if TexItem is TNodeX3DEnvironmentTextureNode then
        UnprepareSingleCubeMapTexture(TNodeX3DEnvironmentTextureNode(TexItem)) else
      if TexItem is TNodeX3DTexture3DNode then
        UnprepareSingleTexture3D(TNodeX3DTexture3DNode(TexItem)) else
      if TexItem is TNodeGeneratedShadowMap then
        UnprepareSingleTextureDepth(TNodeGeneratedShadowMap(TexItem));
    end;
  end;

var
  I: Integer;
begin
  {zwracam uwage ze nie niszczymy tu fontow dla (LastNode is TNodeFontStyle)
   bo fonty sa gromadzone w tablicy Cache.Fonts i wszystkie Font Styles
   o takich samych wlasciwosciach Family i Style korzystaja zawsze z tego
   samego juz utworzonego fontu.}

  { unprepare single texture }
  if Node is TVRMLTextureNode then
    UnprepareSingle2DTexture(TVRMLTextureNode(Node)) else

  { unprepare multi texture }
  if Node is TNodeMultiTexture then
    UnprepareMultiTexture(TNodeMultiTexture(Node)) else

  { unprepare cube map texture }
  if Node is TNodeX3DEnvironmentTextureNode then
    UnprepareSingleCubeMapTexture(TNodeX3DEnvironmentTextureNode(Node));

  if Node is TNodeX3DTexture3DNode then
    UnprepareSingleTexture3D(TNodeX3DTexture3DNode(Node));

  if Node is TNodeGeneratedShadowMap then
    UnprepareSingleTextureDepth(TNodeGeneratedShadowMap(Node));

  { unprepare GLSLProgram }
  { This is not used for now anywhere actually ? Nowhere I unprepare
    TNodeComposedShader nodes ? }
  if Node is TNodeComposedShader then
  begin
    I := GLSLProgramReferences.ProgramNodeIndex(TNodeComposedShader(Node));
    if I >= 0 then
    begin
      if GLSLProgramReferences.Items[I].GLSLProgram <> nil then
        Cache.GLSLProgram_DecReference(GLSLProgramReferences.Items[I].GLSLProgram);
      GLSLProgramReferences.Delete(I, 1);
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
  FLastGLFreeTexture := -1;
  BumpMappingMethodIsCached := false;

  { asumme that "steep" version of parallax mapping is possible }
  BmSteepParallaxMapping := true;

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
  for i := 0 to TextureImageReferences.Count-1 do
    Cache.TextureImage_DecReference(TextureImageReferences.Items[i].GLName);
  TextureImageReferences.SetLength(0);

  for i := 0 to TextureVideoReferences.Count-1 do
    Cache.TextureVideo_DecReference(TextureVideoReferences.Items[i].GLVideo);
  TextureVideoReferences.SetLength(0);

  for i := 0 to TextureCubeMapReferences.Count-1 do
    Cache.TextureCubeMap_DecReference(TextureCubeMapReferences.Items[i].GLName);
  TextureCubeMapReferences.SetLength(0);

  for i := 0 to Texture3DReferences.Count-1 do
    Cache.Texture3D_DecReference(Texture3DReferences.Items[i].GLName);
  Texture3DReferences.SetLength(0);

  for i := 0 to TextureDepthReferences.Count-1 do
    Cache.TextureDepth_DecReference(TextureDepthReferences.Items[i].GLName);
  TextureDepthReferences.SetLength(0);

  { unprepare all GLSLPrograms }
  for i := 0 to GLSLProgramReferences.Count - 1 do
    if GLSLProgramReferences.Items[i].GLSLProgram <> nil then
      Cache.GLSLProgram_DecReference(GLSLProgramReferences.Items[i].GLSLProgram);
  GLSLProgramReferences.SetLength(0);

  { unprepare TexNormalizationCube }
  glFreeTexture(TexNormalizationCube);

  { unprepare BmGLSLProgram }
  FreeAndNil(BmGLSLProgram[false]);
  FreeAndNil(BmGLSLProgram[true]);
  FreeAndNil(BmGLSLAttribObjectSpaceToTangent[false]);
  FreeAndNil(BmGLSLAttribObjectSpaceToTangent[true]);
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

function TVRMLOpenGLRenderer.LastGLFreeTexture: Cardinal;
begin
  if FLastGLFreeTexture = -1 then
  begin
    if Attributes.LastGLFreeTexture = -1 then
    begin
      { actually get this from OpenGL }
      if GL_ARB_multitexture then
        FLastGLFreeTexture := GLMaxTextureUnitsARB - 1 else
        FLastGLFreeTexture := 0;
    end else
      FLastGLFreeTexture := Attributes.LastGLFreeTexture;
  end;
  Result := FLastGLFreeTexture;
end;

function TVRMLOpenGLRenderer.FreeGLTexturesCount: Cardinal;
begin
  if LastGLFreeTexture >= Attributes.FirstGLFreeTexture then
    Result := LastGLFreeTexture - Attributes.FirstGLFreeTexture + 1 else
    Result := 0;
end;

class function TVRMLOpenGLRenderer.GLContextBumpMappingMethod(
  const FirstGLFreeTexture: Cardinal;
  ALastGLFreeTexture: Integer;
  const AttributesBumpMappingMaximum: TBumpMappingMethod;
  const AttributesControlTextures, AttributesEnableTextures, AttributesPureGeometry: boolean):
  TBumpMappingMethod;
var
  TextureUnitsAvailable: Cardinal;
begin
  if ALastGLFreeTexture = -1 then
  begin
    { When ALastGLFreeTexture = -1, we get this from OpenGL, thus somewhat
      duplicating functionality that we already implemented in
      TVRMLOpenGLRenderer.LastGLFreeTexture method. However, this is useful:

      - When calling GLContextBumpMappingMethod internally, by BumpMappingMethod,
        TVRMLOpenGLRenderer.LastGLFreeTexture will be passed, so it's never -1.

      - When calling GLContextBumpMappingMethod from other places, in 99%
        of the cases it's very comfortable being able to pass -1 for this. }

    if GL_ARB_multitexture then
      ALastGLFreeTexture := GLMaxTextureUnitsARB - 1 else
      ALastGLFreeTexture := 0;
  end;

  TextureUnitsAvailable := ALastGLFreeTexture - FirstGLFreeTexture + 1;

  if (AttributesBumpMappingMaximum > bmNone) and
     AttributesControlTextures and
     AttributesEnableTextures and
     (not AttributesPureGeometry) and

    { EXT_texture_env_combine (standard since 1.3) required }
    (GL_EXT_texture_env_combine or GL_version_1_3) and

    { Actually, other extensions also don't have to exist, they are built in
      newer OpenGL version. But this requires getting their procedures under different
      names (without extension suffix). For EXT_texture_env_combine, this is simpler
      since it only defines new constants and these are the same, whether it's extension
      or built-in GL 1.3. }

    { ARB_multitexture required (TODO: standard since 1.3, see above comments) }
    GL_ARB_multitexture and

    { GL >= 1.3 required for GL_SUBTRACT.

      As you see, actually this whole check could be substituted by GL >= 1.3,
      as this allows GL_SUBTRACT and provides all extensions required here. }
    GL_version_1_3 and

    { ARB_texture_env_dot3 required (TODO: standard since 1.3, see above comments) }
    GL_ARB_texture_env_dot3 and

    { At least 2 texture units (for MultiTexDotNotNormalized,
      or for GLSL that does normalization in shader, without
      the need for additional texture) }
    (TextureUnitsAvailable >= 2) then
  begin
    if TGLSLProgram.ClassSupport <> gsNone then
    begin
      { parallax mapping requires one more texture, since height map must be
        passed too. }
      if TextureUnitsAvailable >= 3 then
        Result := bmGLSLParallax else
        Result := bmGLSLNormal;
    end else
    if { ARB_texture_cube_map required for bmMultiTexDotNormalized
         (TODO: standard since 1.3, see above comments) }
      GL_ARB_texture_cube_map and

      { one more texture unit for bmMultiTexDotNormalized }
      (TextureUnitsAvailable >= 3) then

      Result := bmMultiTexDotNormalized else
      Result := bmMultiTexDotNotNormalized;
  end else
    Result := bmNone;

  if Result > AttributesBumpMappingMaximum then
    Result := AttributesBumpMappingMaximum;
end;

function TVRMLOpenGLRenderer.BumpMappingMethod: TBumpMappingMethod;
begin
  if not BumpMappingMethodIsCached then
  begin
    BumpMappingMethodCached := GLContextBumpMappingMethod(
      Attributes.FirstGLFreeTexture,
      LastGLFreeTexture,
      Attributes.BumpMappingMaximum,
      Attributes.ControlTextures,
      Attributes.EnableTextures,
      Attributes.PureGeometry);

    if Log then
      WritelnLog('Bump mapping', 'Bump mapping method detected: "' +
        BumpMappingMethodNames[BumpMappingMethodCached] + '"');

    BumpMappingMethodIsCached := true;
  end;

  Result := BumpMappingMethodCached;
end;

function TVRMLOpenGLRenderer.PreparedTextureAlphaChannelType(
  TextureNode: TNodeX3DTextureNode;
  out AlphaChannelType: TAlphaChannelType): boolean;

  procedure DoIt2D(TextureNode: TVRMLTextureNode);
  var
    Index: Integer;
  begin
    Index := TextureImageReferences.TextureNodeIndex(TextureNode);
    Result := Index <> -1;

    if Result then
      AlphaChannelType := TextureImageReferences.Items[Index].AlphaChannelType else
    begin
      Index := TextureVideoReferences.TextureNodeIndex(TextureNode);
      Result := Index <> -1;

      if Result then
        AlphaChannelType := TextureVideoReferences.Items[Index].AlphaChannelType;
    end;
  end;

  procedure DoItCubeMap(TextureNode: TNodeX3DEnvironmentTextureNode);
  var
    Index: Integer;
  begin
    Index := TextureCubeMapReferences.TextureNodeIndex(TextureNode);
    Result := Index <> -1;
    if Result then
      AlphaChannelType := TextureCubeMapReferences.Items[Index].AlphaChannelType;
  end;

  procedure DoIt3D(TextureNode: TNodeX3DTexture3DNode);
  var
    Index: Integer;
  begin
    Index := Texture3DReferences.TextureNodeIndex(TextureNode);
    Result := Index <> -1;
    if Result then
      AlphaChannelType := Texture3DReferences.Items[Index].AlphaChannelType;
  end;

  procedure DoItDepth(TextureNode: TNodeGeneratedShadowMap);
  var
    Index: Integer;
  begin
    Index := TextureDepthReferences.TextureNodeIndex(TextureNode);
    Result := Index <> -1;
    if Result then
      { Our depth textures never have alpha channel. }
      AlphaChannelType := atNone;
  end;

begin
  if TextureNode is TVRMLTextureNode then
    DoIt2D(TVRMLTextureNode(TextureNode)) else
  if TextureNode is TNodeX3DEnvironmentTextureNode then
    DoItCubeMap(TNodeX3DEnvironmentTextureNode(TextureNode)) else
  if TextureNode is TNodeX3DTexture3DNode then
    DoIt3D(TNodeX3DTexture3DNode(TextureNode)) else
  if TextureNode is TNodeGeneratedShadowMap then
    DoItDepth(TNodeGeneratedShadowMap(TextureNode)) else
    Result := false;
end;

{ Render ---------------------------------------------------------------------- }

{$define MeshRenderer := TVRMLMeshRenderer(ExposedMeshRenderer) }

{$I vrmlopenglrenderer_render_materials.inc}

procedure TVRMLOpenGLRenderer.ActiveTexture(TextureUnit: Cardinal);
begin
  if GL_ARB_multitexture then
    glActiveTextureARB(GL_TEXTURE0_ARB +
      Attributes.FirstGLFreeTexture + TextureUnit);
end;

procedure TVRMLOpenGLRenderer.RenderBegin(FogNode: TNodeFog;
  const FogDistanceScaling: Single);

  procedure SetupFog(FogNode: TNodeFog);
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
     VRMLWarning(vwSerious, 'Unknown fog type "' + FogNode.FdFogType.Value + '"');
     SetupFog(FogNode.Alternative);
     Exit;
   end;

   if FogNode.FdVolumetric.Value and (not GL_EXT_fog_coord) then
   begin
     { Earlier I tried in such cases to just do a normal fog
       that looks "similar". But it turns out to be impossible
       to automatically decide what non-volumetric fog setting (if any) will
       look similar to requested volumetric fog.
       So right now I just resort to "alternative" field. }
     SetupFog(FogNode.Alternative);
     Exit;
   end;

   FogEnabled := true;

   FogVisibilityRangeScaled :=
     FogNode.FdVisibilityRange.Value * FogDistanceScaling;

   FogVolumetric := FogNode.FdVolumetric.Value and GL_EXT_fog_coord;

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
  { calculate UseMultiTexturing: check extensions required for multitexturing.

    We simply require all extensions, like
    EXT_texture_env_combine and ARB_multitexture and
    ARB_texture_env_dot3. If any of them is missing, I'll not use
    multitexturing. This is acceptable, as all modern OpenGL versions
    will have them all. }

  UseMultiTexturing :=
    { EXT_texture_env_combine (standard since 1.3) required }
    (GL_EXT_texture_env_combine or GL_version_1_3) and

    { Actually, other extensions also don't have to exist, they are built in
      newer OpenGL version. But this requires getting their procedures under different
      names (without extension suffix). For EXT_texture_env_combine, this is simpler
      since it only defines new constants and these are the same, whether it's extension
      or built-in GL 1.3. }

    { ARB_multitexture required (TODO: standard since 1.3, see above comments) }
    GL_ARB_multitexture and

    { GL >= 1.3 required for GL_SUBTRACT.

      As you see, actually this whole check could be substituted by GL >= 1.3,
      as this allows GL_SUBTRACT and provides all extensions required here. }
    GL_version_1_3 and

    { ARB_texture_env_dot3 required (TODO: standard since 1.3, see above comments) }
    GL_ARB_texture_env_dot3;

 { Push attribs and matrices (by pushing attribs FIRST we save also current
   matrix mode).

   Note for BuggyPointSetAttrib: yes, this may cause bugs,
   as glPointSize call "leaks" out. But there's nothing we can do about it,
   we cannot use GL_POINT_BIT as it crashes Mesa (and produces "invalid
   enumerant" error in case of debug compile). }
 if not GLVersion.BuggyPointSetAttrib then
   glPushAttrib(GL_ALL_ATTRIB_BITS) else
   glPushAttrib(GL_ALL_ATTRIB_BITS and (not GL_POINT_BIT));
 glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

 { TODO: push/pop is not fully correctly done for multitexturing now:
   - We should push/pop all texture units matrices.
     Right now, we actually push only 0th texture unit matrix.
   - Make sure that currently active texture unit is saved ?
     I'm not sure, does some glPushAttrib param saves this ?

   Push/pop texture state saves environment state of all texture units, so at least
   we got glTexEnv covered. (this says OpenGL manpage for glPushAttrib,
   and it applies to both multitexturing by ARB extension and by standard GL).
 }
 ActiveTexture(0);

 {init our OpenGL state}
 glMatrixMode(GL_MODELVIEW);

 if not Attributes.PureGeometry then
 begin
   glDisable(GL_COLOR_MATERIAL);
   if Attributes.ControlTextures then
   begin
     glDisable(GL_TEXTURE_GEN_S);
     glDisable(GL_TEXTURE_GEN_T);
     glDisable(GL_TEXTURE_GEN_Q);
   end;
   glEnable(GL_NORMALIZE);
   glPointSize(Attributes.PointSize);
   glEnable(GL_DEPTH_TEST);
   glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

   { While rendering Indexed_Faces_Or_Triangles we may temporarily
     enable/disable GL_CULL_FACE and change glCullFace. We want to make
     sure from what state we start, so we set if here.
     Note that we *do not* set glFrontFace --- see comments at the beginning
     of this unit to know why. }
   glCullFace(GL_BACK);
   glDisable(GL_CULL_FACE);

   glDisable(GL_ALPHA_TEST);
   {AlphaFunc uzywane tylko dla textures i tam taka wartosc jest dobra}
   glAlphaFunc(GL_GEQUAL, 0.5);

   if Attributes.SmoothShading then
    glShadeModel(GL_SMOOTH) else
    glShadeModel(GL_FLAT);

   if Attributes.UseLights then
     for i := Attributes.FirstGLFreeLight to LastGLFreeLight do
       glDisable(GL_LIGHT0+i);

   SetupFog(FogNode);
 end;
end;

procedure TVRMLOpenGLRenderer.RenderEnd;
begin
  ActiveTexture(0);

  {pop matrices and attribs (popping attrib restores also saved matrix mode)}
  glPopClientAttrib;
  glPopAttrib;
end;

{$ifdef USE_VRML_NODES_TRIANGULATION}
procedure TVRMLOpenGLRenderer.DrawTriangle(const Tri: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Render_BindMaterial_1(MatNum);

  glNormalv(TriangleNormal(Tri));

  glBegin(GL_TRIANGLES);
    glVertexv(Tri[0]);
    glVertexv(Tri[1]);
    glVertexv(Tri[2]);
  glEnd;
end;
{$endif USE_VRML_NODES_TRIANGULATION}

procedure TVRMLOpenGLRenderer.RenderShapeLights(
  LightsRenderer: TVRMLGLLightsCachingRenderer;
  State: TVRMLGraphTraverseState);
begin
  glMatrixMode(GL_MODELVIEW);

  { Render lights in given State, if Attributes.UseLights.

    This is done in RenderShapeLights, before RenderShapeBegin,
    in particular before loading State.Transform --- this is good,
    as the lights positions/directions are in world coordinates. }

  if Attributes.UseLights then
    LightsRenderer.Render(State.CurrentActiveLights);

    { Without LightsRenderer, we would do it like this:
    glLightsFromVRML(State.CurrentActiveLights,
      Attributes.FirstGLFreeLight, LastGLFreeLight,
      Attributes.ColorModulatorSingle);}
end;

procedure TVRMLOpenGLRenderer.RenderShapeBegin(Shape: TVRMLShape);

  { Pass non-nil TextureTransform that is not a MultiTextureTransform.
    Then this will simply do glMultMatrix (or equivalent) applying
    transformations encoded in this TextureTransform node. }
  procedure TextureMultMatrix(TextureTransform: TNodeX3DTextureTransformNode);
  begin
    if TextureTransform is TNodeTextureTransform then
    begin
      { Optimized version of
          glMultMatrix(TextureTransform.TransformMatrix);
        specially for TNodeTextureTransform. Possibly using OpenGL
        translate etc. commands instead of loading directly 4x4 matrix will
        result in some performance/precision gain (but, not confirmed in
        practice). }
      with TNodeTextureTransform(TextureTransform) do
      begin
        glTranslatef(
          FdTranslation.Value[0] + FdCenter.Value[0],
          FdTranslation.Value[1] + FdCenter.Value[1], 0);
        glRotatef(RadToDeg(FdRotation.Value), 0, 0, 1);
        glScalef(FdScale.Value[0], FdScale.Value[1], 1);
        glTranslatef(-FdCenter.Value[0], -FdCenter.Value[1], 0);
      end;
    end else
      glMultMatrix(TextureTransform.TransformMatrix);
  end;

var
  TextureTransform: TNodeX3DTextureTransformNode;
  Child: TVRMLNode;
  Transforms: TMFNode;
  I: Integer;
  State: TVRMLGraphTraverseState;
begin
  State := Shape.State;

  TextureTransformUnitsUsed := 0;
  TextureTransformUnitsUsedMore.Count := 0;

  if (State.ParentShape = nil { VRML 1.0, always some texture transform }) or
     (State.ParentShape.TextureTransform <> nil { VRML 2.0 with tex transform }) then
  begin
    glMatrixMode(GL_TEXTURE);

    { We work assuming that texture matrix before RenderShapeBegin was identity.
      Texture transform encoded in VRML/X3D will be multiplied by this.

      This allows the programmer to eventually transform all textures
      by placing non-identity texture matrix (just like a programmer
      can transform whole rendered model by changing modelview matrix).
      So this is a good thing.

      Additional advantage is that we do not have to explicitly "clear"
      the texture matrix if it's an identity transformation in VRML/X3D.
      We just let it stay like it was.

      This also nicely cooperates with X3D MultiTextureTransform desired
      behavior: "If there are too few entries in the textureTransform field,
      identity matrices shall be used for all remaining undefined channels.".
      Which means that looking at MultiTextureTransform node, we know exactly
      on which texture units we have to apply transform: we can leave
      the remaining texture units as they were, regardless of whether
      MultiTexture is used at all and regardless of how many texture units
      are actually used by MultiTexture. }

    { TODO: for bump mapping, TextureTransform should be done on more than one texture unit. }

    if State.ParentShape = nil then
    begin
      { No multitexturing in VRML 1.0, just always transform first tex unit. }
      TextureTransformUnitsUsed := 1;
      ActiveTexture(0);
      glPushMatrix;
      glMultMatrix(State.TextureTransform);
    end else
    begin
      TextureTransform := State.ParentShape.TextureTransform;
      if TextureTransform <> nil then
      begin
        if TextureTransform is TNodeMultiTextureTransform then
        begin
          Transforms := TNodeMultiTextureTransform(TextureTransform).FdTextureTransform;

          { Multitexturing, so use as many texture units as there are children in
            MultiTextureTransform.textureTransform.

            Cap by available texture units (First/LastGLFreeTexture,
            and check UseMultiTexturing in case OpenGL cannot support
            multitex at all). }
          TextureTransformUnitsUsed := Min(Transforms.Count, FreeGLTexturesCount);
          if not UseMultiTexturing then
            MinTo1st(TextureTransformUnitsUsed, 1);

          for I := 0 to TextureTransformUnitsUsed - 1 do
          begin
            ActiveTexture(I);
            glPushMatrix;
            Child := Transforms.Items[I];
            if (Child <> nil) and
               (Child is TNodeX3DTextureTransformNode) then
            begin
              if Child is TNodeMultiTextureTransform then
                VRMLWarning(vwSerious, 'MultiTextureTransform.textureTransform list cannot contain another MultiTextureTransform instance') else
                TextureMultMatrix(TNodeX3DTextureTransformNode(Child));
            end;
          end;
        end else
        { Check below is done because X3D specification explicitly
          says that MultiTexture is affected *only* by MultiTextureTransform,
          that is normal TextureTransform and such is ignored (treated
          like identity transform, *not* applied to 1st texture unit).

          By the way, we don't do any texture transform if Texture = nil,
          since then no texture is used anyway. }
        if (State.Texture <> nil) and
           (not (State.Texture is TNodeMultiTexture)) then
        begin
          TextureTransformUnitsUsed := 1;
          ActiveTexture(0);
          glPushMatrix;
          TextureMultMatrix(TextureTransform);
        end;
      end;
    end;
  end;

  glMatrixMode(GL_MODELVIEW);

  glPushMatrix;
    glMultMatrix(State.Transform);
end;

procedure TVRMLOpenGLRenderer.RenderShapeNoTransform(Shape: TVRMLShape);

  function NodeTextured(Node: TVRMLGeometryNode): boolean;
  begin
    Result := not (
      (Node is TNodePointSet_2) or
      (Node is TNodeIndexedLineSet_2));
  end;

  { If CurrentGeometry should be rendered using one of TVRMLMeshRenderer
    classes, then create appropriate MeshRenderer. Takes care
    of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render.
    Otherwise, MeshRenderer is set to @nil. }
  procedure InitMeshRenderer;
  begin
    if CurrentGeometry is TNodeIndexedTriangleMesh_1 then
      ExposedMeshRenderer := TTriangleStripSetRenderer.Create(Self) else
    if CurrentGeometry is TNodeIndexedFaceSet_1 then
      ExposedMeshRenderer := TIndexedFaceSet_1Renderer.Create(Self) else
    if CurrentGeometry is TNodeIndexedFaceSet_2 then
      ExposedMeshRenderer := TIndexedFaceSet_2Renderer.Create(Self) else
    if CurrentGeometry is TNodeIndexedLineSet_1 then
      ExposedMeshRenderer := TIndexedLineSet_1Renderer.Create(Self) else
    if (CurrentGeometry is TNodeIndexedLineSet_2) or
       (CurrentGeometry is TNodeLineSet) then
      ExposedMeshRenderer := TLineSet_2Renderer.Create(Self) else
    if CurrentGeometry is TNodePointSet_1 then
      ExposedMeshRenderer := TPointSet_1Renderer.Create(Self) else
    if CurrentGeometry is TNodePointSet_2 then
      ExposedMeshRenderer := TPointSet_2Renderer.Create(Self) else
    if CurrentGeometry is TNodeElevationGrid then
      ExposedMeshRenderer := TElevationGridRenderer.Create(Self) else
    if (CurrentGeometry is TNodeTriangleSet) or
       (CurrentGeometry is TNodeIndexedTriangleSet) then
      ExposedMeshRenderer := TTriangleSetRenderer.Create(Self) else
    if (CurrentGeometry is TNodeTriangleFanSet) or
       (CurrentGeometry is TNodeIndexedTriangleFanSet) then
      ExposedMeshRenderer := TTriangleFanSetRenderer.Create(Self) else
    if (CurrentGeometry is TNodeTriangleStripSet) or
       (CurrentGeometry is TNodeIndexedTriangleStripSet) then
      ExposedMeshRenderer := TTriangleStripSetRenderer.Create(Self) else
    if (CurrentGeometry is TNodeQuadSet) or
       (CurrentGeometry is TNodeIndexedQuadSet) then
      ExposedMeshRenderer := TQuadSetRenderer.Create(Self) else
    if CurrentGeometry is TNodeAsciiText_1 then
      ExposedMeshRenderer := TAsciiTextRenderer.Create(Self) else
    if CurrentGeometry is TNodeText then
      ExposedMeshRenderer := TTextRenderer.Create(Self) else
    if CurrentGeometry is TNodeText3D then
      ExposedMeshRenderer := TText3DRenderer.Create(Self) else
    if CurrentGeometry is TNodeCone_1 then
      ExposedMeshRenderer := TCone_1Renderer.Create(Self) else
    if CurrentGeometry is TNodeCone_2 then
      ExposedMeshRenderer := TCone_2Renderer.Create(Self) else
    if CurrentGeometry is TNodeCube_1 then
      ExposedMeshRenderer := TCube_1Renderer.Create(Self) else
    if CurrentGeometry is TNodeBox then
      ExposedMeshRenderer := TBoxRenderer.Create(Self) else
    if CurrentGeometry is TNodeCylinder_1 then
      ExposedMeshRenderer := TCylinder_1Renderer.Create(Self) else
    if CurrentGeometry is TNodeCylinder_2 then
      ExposedMeshRenderer := TCylinder_2Renderer.Create(Self) else
    if CurrentGeometry is TNodeSphere_1 then
      ExposedMeshRenderer := TSphere_1Renderer.Create(Self) else
    if CurrentGeometry is TNodeSphere_2 then
      ExposedMeshRenderer := TSphere_2Renderer.Create(Self) else
    if CurrentGeometry is TNodeRectangle2D then
      ExposedMeshRenderer := TRectangle2DRenderer.Create(Self) else
    if CurrentGeometry is TNodeCircle2D then
      ExposedMeshRenderer := TCircle2DRenderer.Create(Self) else
      ExposedMeshRenderer := nil;
  end;

  procedure InitTextures;

    type
      TTextureEnableDisable = (etOff, et2D, etCubeMap, et3D);

    { Enable/disable texturing (2D and cube map) on current texture unit. }
    procedure TextureEnableDisable(const Enable: TTextureEnableDisable);
    begin
      case Enable of
        etOff:
          begin
            glDisable(GL_TEXTURE_2D);
            if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
            if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
          end;
        et2D:
          begin
            glEnable(GL_TEXTURE_2D);
            if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
            if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
          end;
        etCubeMap:
          begin
            glDisable(GL_TEXTURE_2D);
            if GL_ARB_texture_cube_map then glEnable(GL_TEXTURE_CUBE_MAP_ARB);
            if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
          end;
        et3D:
          begin
            glDisable(GL_TEXTURE_2D);
            if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
            if GL_EXT_texture3D        then glEnable(GL_TEXTURE_3D_EXT);
          end;
        else raise EInternalError.Create('TextureEnableDisable?');
      end;
    end;

  var
    AlphaTest: boolean;

    procedure DisableTexture(const TextureUnit: Cardinal);
    begin
      ActiveTexture(TextureUnit);
      TextureEnableDisable(etOff);
    end;

    { Enable non-multi texture.
      TextureNode must be non-nil.

      Returns success: false means that texture node was not prepared
      for OpenGL, which means (we assume that you called Prepare before
      Render) that texture failed to load. You should then disable
      the texture unit, and you don't have to generate tex coords for it.

      When returns @true (success) you can be sure that the specified
      TextureUnit is currently bound (if OpenGL multitexturing
      extensions are available at all). This is useful, if you want
      to later adjust texture unit parameters, like
      glTexEnvi(GL_TEXTURE_ENV, ...).

      It's also already enabled by glEnable(GL_TEXTURE_2D).

      AlphaTest may be modified, but only to true, by this procedure. }
    function EnableSingle2DTexture(const TextureUnit: Cardinal;
      TextureNode: TVRMLTextureNode;
      UseForBumpMappingAllowed: boolean): boolean;

      procedure EnableClassicTexturing(GLTexture: TGLuint);
      begin
        ActiveTexture(TextureUnit);
        TextureEnableDisable(et2D);

        glBindTexture(GL_TEXTURE_2D, GLTexture);
      end;

    var
      IndexedFaceRenderer: TIndexedFaceSetRenderer;
      TextureReferencesIndex: Integer;
      TexImageReference: PTextureImageReference;
      TexVideoReference: PTextureVideoReference;
      VideoTime: TKamTime;
    begin
      Result := false;

      { Note: don't call IsTextureImage, IsTextureVideo here --- this
        would causes reloading images/videos, nullifying
        TVRMLScene.FreeResources([frTextureDataInNodes]) purpose.

        Actually, it would be safe to call this for non-MovieTexture nodes,
        as they should be prepared to display lists before doing
        FreeResources. But for MovieTexture nodes it's forbidden,
        as it's called at every frame render. }

      TextureReferencesIndex := TextureImageReferences.TextureNodeIndex(
        TextureNode);
      if TextureReferencesIndex <> -1 then
      begin
        TexImageReference := TextureImageReferences.Pointers[
          TextureReferencesIndex];

        AlphaTest := AlphaTest or
          (TexImageReference^.AlphaChannelType = atSimpleYesNo);

        if UseForBumpMappingAllowed and
           (MeshRenderer <> nil) and
           MeshRenderer.BumpMappingAllowed and
           (BumpMappingMethod <> bmNone) then
        begin
          if TexImageReference^.NormalMap <> 0 then
          begin
            MeshRenderer.BumpMappingMethod := BumpMappingMethod;
            Assert(MeshRenderer is TIndexedFaceSetRenderer,
              'We assumed that only TIndexedFaceSetRenderer may actually have BumpMappingMethod <> bmNone');
            IndexedFaceRenderer := TIndexedFaceSetRenderer(MeshRenderer);
            IndexedFaceRenderer.TexNormalizationCube := TexNormalizationCube;
            IndexedFaceRenderer.TexOriginal := TexImageReference^.GLName;
            IndexedFaceRenderer.TexOriginalAlpha := AlphaTest;
            IndexedFaceRenderer.TexNormalMap := TexImageReference^.NormalMap;
            IndexedFaceRenderer.TexHeightMap := TexImageReference^.HeightMap;
            IndexedFaceRenderer.TexHeightMapScale := TexImageReference^.HeightMapScale;
            { use parallax only if the model actually has heightMap }
            if (IndexedFaceRenderer.TexHeightMap = 0) and
               (MeshRenderer.BumpMappingMethod >= bmGLSLParallax) then
              MeshRenderer.BumpMappingMethod := bmGLSLNormal;

            { Bind specified TextureUnit. }
            ActiveTexture(TextureUnit);
          end else
            EnableClassicTexturing(TexImageReference^.GLName);
        end else
          EnableClassicTexturing(TexImageReference^.GLName);

        Result := true;
      end else
      begin
        TextureReferencesIndex := TextureVideoReferences.TextureNodeIndex(
          TextureNode);
        if TextureReferencesIndex <> -1 then
        begin
          TexVideoReference := TextureVideoReferences.Pointers[TextureReferencesIndex];

          AlphaTest := AlphaTest or
            (TexVideoReference^.AlphaChannelType = atSimpleYesNo);

          VideoTime := TexVideoReference^.Node.TimeDependentNodeHandler.ElapsedTime *
                       TexVideoReference^.Node.FdSpeed.Value;
          if TexVideoReference^.Node.FdSpeed.Value < 0 then
            VideoTime := TexVideoReference^.Node.Duration + VideoTime;

          EnableClassicTexturing(
            TexVideoReference^.GLVideo.GLTextureFromTime(VideoTime));

          Result := true;
        end;
      end;
    end;

    function EnableSingleCubeMapTexture(const TextureUnit: Cardinal;
      CubeTexture: TNodeX3DEnvironmentTextureNode): boolean;
    var
      TexRefIndex: Integer;
      TexRef: PTextureCubeMapReference;
    begin
      TexRefIndex := TextureCubeMapReferences.TextureNodeIndex(CubeTexture);
      Result := TexRefIndex <> -1;
      if Result then
      begin
        TexRef := TextureCubeMapReferences.Pointers[TexRefIndex];

        AlphaTest := AlphaTest or (TexRef^.AlphaChannelType = atSimpleYesNo);

        ActiveTexture(TextureUnit);
        glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, TexRef^.GLName);
        TextureEnableDisable(etCubeMap);
      end;
    end;

    function EnableSingleTexture3D(const TextureUnit: Cardinal;
      Texture: TNodeX3DTexture3DNode): boolean;
    var
      TexRefIndex: Integer;
      TexRef: PTexture3DReference;
    begin
      TexRefIndex := Texture3DReferences.TextureNodeIndex(Texture);
      Result := TexRefIndex <> -1;
      if Result then
      begin
        TexRef := Texture3DReferences.Pointers[TexRefIndex];

        AlphaTest := AlphaTest or (TexRef^.AlphaChannelType = atSimpleYesNo);

        ActiveTexture(TextureUnit);
        glBindTexture(GL_TEXTURE_3D_EXT, TexRef^.GLName);
        TextureEnableDisable(et3D);
      end;
    end;

    function EnableSingleTextureDepth(const TextureUnit: Cardinal;
      Texture: TNodeGeneratedShadowMap): boolean;
    var
      TexRefIndex: Integer;
      TexRef: PTextureDepthReference;
    begin
      TexRefIndex := TextureDepthReferences.TextureNodeIndex(Texture);
      Result := TexRefIndex <> -1;
      if Result then
      begin
        TexRef := TextureDepthReferences.Pointers[TexRefIndex];

        { Depth textures never have an alpha channel:
        AlphaTest := AlphaTest or (TexRef^.AlphaChannelType = atSimpleYesNo); }

        ActiveTexture(TextureUnit);
        glBindTexture(GL_TEXTURE_2D, TexRef^.GLName);
        TextureEnableDisable(et2D);
      end;
    end;

    { Do the necessary preparations for a multi-texture node.
      MultiTexture must be non-nil.

      Sets enabled/disabled texture state for all texture units < TexCount. }
    procedure EnableMultiTexture(const TexCount: Cardinal;
      MultiTexture: TNodeMultiTexture);

      { Return OpenGL values for
        GL_COMBINE_RGB_EXT, GL_COMBINE_ALPHA_EXT.

        Also, specify which argument (0, 1, 2 for
        GL_SOURCE0, GL_SOURCE1, GL_SOURCE2, or -1 if none) should represent
        current texture unit (this is "Arg1" in X3D spec wording),
        and which is the source (default is previous texture unit,
        although may change by "source" field; this is "Arg2" in X3D spec).

        You can assign here explicitly sources/operands for other
        arguments, if needed (e.g. "BLEND*" modes fill here 3rd source/operand
        directly).

        Also, specify RGBScale, AlphaScale (remember OpenGL allows
        only 1.0, 2.0 and 4.0 scales). By default are 1. }
      procedure ModeFromString(const S: string;
        out CombineRGB, CombineAlpha: TGLint;
        out Arg1, Arg2: Integer;
        var RGBScale, AlphaScale: TGLfloat;
        var AlreadyHandled: boolean;
        var NeedsConstantColor: boolean);
      var
        LS: string;
      begin
        LS := LowerCase(S);
        if (LS = 'modulate') or (LS = '') then
        begin
          { LS = '' means that mode list was too short.
            X3D spec says explicitly that default mode is "MODULATE"
            in this case. (Accidentaly, this also will accept
            explict "" string as "MODULATE" --- not a worry, we don't
            have to produce error messages for all possible invalid VRMLs...). }
          CombineRGB := GL_MODULATE;
          CombineAlpha := GL_MODULATE;
          Arg1 := 0;
          Arg2 := 1;
        end else
        if LS = 'modulate2x' then
        begin
          CombineRGB := GL_MODULATE;
          CombineAlpha := GL_MODULATE;
          Arg1 := 0;
          Arg2 := 1;
          RGBScale := 2;
          AlphaScale := 2;
        end else
        if LS = 'modulate4x' then
        begin
          CombineRGB := GL_MODULATE;
          CombineAlpha := GL_MODULATE;
          Arg1 := 0;
          Arg2 := 1;
          RGBScale := 4;
          AlphaScale := 4;
        end else
        if (LS = 'replace') or (LS = 'selectarg1') then
        begin
          { SELECTARG1 is exactly the same as REPLACE.

            Note: don't get confused by X3D spec saying in table 18.3 that
            "REPLACE" takes the Arg2, that's an error, it takes
            from Arg1 to be consistent with other spec words.
            I wrote some remarks about this on
            http://vrmlengine.sourceforge.net/vrml_implementation_status.php }

          CombineRGB := GL_REPLACE;
          CombineAlpha := GL_REPLACE;
          Arg1 := 0;
          Arg2 := -1;
        end else
        if LS = 'selectarg2' then
        begin
          CombineRGB := GL_REPLACE;
          CombineAlpha := GL_REPLACE;
          Arg1 := -1;
          Arg2 := 0;
        end else
        if LS = 'off' then
        begin
          { For OFF, turn off the texture unit? This is the correct
            interpretation, right? }
          TextureEnableDisable(etOff);
          AlreadyHandled := true;
        end else
        if LS = 'add' then
        begin
          CombineRGB := GL_ADD;
          CombineAlpha := GL_ADD;
          Arg1 := 0;
          Arg2 := 1;
        end else
        if LS = 'addsigned' then
        begin
          CombineRGB := GL_ADD_SIGNED_EXT;
          CombineAlpha := GL_ADD_SIGNED_EXT;
          Arg1 := 0;
          Arg2 := 1;
        end else
        if LS = 'addsigned2x' then
        begin
          CombineRGB := GL_ADD_SIGNED_EXT;
          CombineAlpha := GL_ADD_SIGNED_EXT;
          Arg1 := 0;
          Arg2 := 1;
          RGBScale := 2;
          AlphaScale := 2;
        end else
        if LS = 'subtract' then
        begin
          CombineRGB := GL_SUBTRACT;
          CombineAlpha := GL_SUBTRACT;
          Arg1 := 0;
          Arg2 := 1;
        end else
        if LS = 'dotproduct3' then
        begin
          { We use DOT3_RGBA_ARB, not DOT3_RGB_ARB.
            See [http://www.opengl.org/registry/specs/ARB/texture_env_dot3.txt].

            This means that the dot (done on only RGB channels) will
            be replicated to all four channels (RGBA). This is exactly what
            the X3D specification requires, so we're happy.
            Yes, this means that COMBINE_ALPHA_ARB will be ignored. }

          CombineRGB := GL_DOT3_RGBA_ARB;
          CombineAlpha := GL_REPLACE; { <- whatever, will be ignored }
          Arg1 := 0;
          Arg2 := 1;
        end else
        if LS = 'blenddiffusealpha' then
        begin
          CombineRGB := GL_INTERPOLATE_EXT;
          CombineAlpha := GL_INTERPOLATE_EXT;
          Arg1 := 0;
          Arg2 := 1;

          { Whole source2 (both RGB and alpha) is filled by alpha of material
            (primary color). }
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_PRIMARY_COLOR_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
        end else
        if LS = 'blendtexturealpha' then
        begin
          CombineRGB := GL_INTERPOLATE_EXT;
          CombineAlpha := GL_INTERPOLATE_EXT;
          Arg1 := 0;
          Arg2 := 1;

          { Whole source2 (both RGB and alpha) is filled by alpha of current
            tex unit. }
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_TEXTURE);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_TEXTURE);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
        end else
        if LS = 'blendfactoralpha' then
        begin
          CombineRGB := GL_INTERPOLATE_EXT;
          CombineAlpha := GL_INTERPOLATE_EXT;
          Arg1 := 0;
          Arg2 := 1;

          { Whole source2 (both RGB and alpha) is filled by const alpha. }
          NeedsConstantColor := true;
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_CONSTANT_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_CONSTANT_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
        end else
        if LS = 'blendcurrentalpha' then
        begin
          CombineRGB := GL_INTERPOLATE_EXT;
          CombineAlpha := GL_INTERPOLATE_EXT;
          Arg1 := 0;
          Arg2 := 1;

          { Whole source2 (both RGB and alpha) is filled by alpha from prev tex. }
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_PREVIOUS_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_PREVIOUS_EXT);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
        end else
        begin
          CombineRGB := GL_MODULATE;
          CombineAlpha := GL_MODULATE;
          Arg1 := 0;
          Arg2 := 1;
          VRMLWarning(vwSerious, Format('Not supported multi-texturing mode "%s"', [S]))
        end;
      end;

      procedure SourceFromString(const S: string; out Source: TGLint;
        var NeedsConstantColor: boolean);
      var
        LS: string;
      begin
        LS := LowerCase(S);
        if LS = '' then
          Source := GL_PREVIOUS_EXT else
        if (LS = 'diffuse') or (LS = 'specular') then
          Source := GL_PRIMARY_COLOR_EXT else
        if LS = 'factor' then
        begin
          NeedsConstantColor := true;
          Source := GL_CONSTANT_EXT;
        end else
        begin
          Source := GL_PREVIOUS_EXT;
          VRMLWarning(vwSerious, Format('Not supported multi-texturing source "%s"', [S]))
        end;
      end;

    var
      ChildTex: TVRMLNode;
      I: Integer;
      Success: boolean;
      CombineRGB, CombineAlpha: TGLint;
      Arg1, Arg2: Integer;
      RGBScale, AlphaScale: TGLfloat;
      S: string;
      AlreadyHandled: boolean;
      NeedsConstantColor: boolean;
      Source: TGLint;
    begin
      Assert(Integer(TexCount) <= MultiTexture.FdTexture.Items.Count);
      for I := 0 to Integer(TexCount) - 1 do
      begin
        ChildTex := MultiTexture.FdTexture.Items.Items[I];
        Success := false;

        if ChildTex <> nil then
        begin
          if ChildTex is TNodeMultiTexture then
            VRMLWarning(vwSerious, 'Child of MultiTexture node cannot be another MultiTexture node') else
          if ChildTex is TVRMLTextureNode then
            Success := EnableSingle2DTexture(I, TVRMLTextureNode(ChildTex), false) else
          if ChildTex is TNodeX3DEnvironmentTextureNode then
            Success := EnableSingleCubeMapTexture(I, TNodeX3DEnvironmentTextureNode(ChildTex)) else
          if ChildTex is TNodeX3DTexture3DNode then
            Success := EnableSingleTexture3D(I, TNodeX3DTexture3DNode(ChildTex)) else
          if ChildTex is TNodeGeneratedShadowMap then
            Success := EnableSingleTextureDepth(I, TNodeGeneratedShadowMap(ChildTex));

          if Success then
          begin
            { Set all the multitexture mode-related stuff.
              Below we handle MultiTexture.mode, source, color, alpha,
              function fields. }

            AlreadyHandled := false;
            RGBScale := 1;
            AlphaScale := 1;
            NeedsConstantColor := false;

            if I < MultiTexture.FdMode.Count then
              S := MultiTexture.FdMode.Items[I] else
              S := '';

            ModeFromString(S, CombineRGB, CombineAlpha, Arg1, Arg2,
              RGBScale, AlphaScale, AlreadyHandled, NeedsConstantColor);

            if not AlreadyHandled then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

              glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, CombineRGB);
              glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, CombineAlpha);

              glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE_EXT, RGBScale);
              glTexEnvf(GL_TEXTURE_ENV, GL_ALPHA_SCALE, AlphaScale);

              if Arg1 <> -1 then
              begin
                { First argument is always current texture unit.
                  (This is indicated by X3D spec wording
                  "The source field determines the colour source for the second argument.") }
                glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT + Arg1, GL_TEXTURE);
                glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT + Arg1, GL_SRC_COLOR);

                glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT + Arg1, GL_TEXTURE);
                glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT + Arg1, GL_SRC_ALPHA);
              end;

              if Arg2 <> -1 then
              begin
                if I < MultiTexture.FdSource.Count then
                  S := MultiTexture.FdSource.Items[I] else
                  S := '';

                SourceFromString(S, Source, NeedsConstantColor);

                glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT + Arg2, Source);
                glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT + Arg2, GL_SRC_COLOR);

                glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT + Arg2, Source);
                glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT + Arg2, GL_SRC_ALPHA);
              end;

              if NeedsConstantColor then
              begin
                { Assign constant color now, when we know it should be used. }
                glTexEnvv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, Vector4Single(
                  MultiTexture.FdColor.Value,
                  MultiTexture.FdAlpha.Value));
              end;
            end;
          end;
        end;

        if not Success then
          DisableTexture(I);
      end;
    end;

  var
    I: Integer;
    TextureNode: TNodeX3DTextureNode;
  begin
    if Attributes.PureGeometry then
    begin
      TexCoordsNeeded := 0;
      Exit;
    end;

    if not Attributes.ControlTextures then
    begin
      { Require texture coordinates for 1st texture, but don't do anything else
        (like setting active texture, enabling/disabling it, don't even look
        at VRML texture node.) }
      TexCoordsNeeded := 1;
      Exit;
    end;

    AlphaTest := false;

    TextureNode := CurrentState.Texture;
    {$ifdef USE_VRML_NODES_TRIANGULATION}
    { We don't generate texture coords, so disable textures. }
    TextureNode := nil;
    {$endif}

    TexCoordsNeeded := 0;

    if (TextureNode <> nil) and
       Attributes.EnableTextures and
       NodeTextured(CurrentGeometry) then
    begin
      if TextureNode is TVRMLTextureNode then
      begin
        if EnableSingle2DTexture(0, TVRMLTextureNode(TextureNode), true) then
        begin
          { TODO: this should be fixed, for non-multi tex decal
            is standard? }
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
          TexCoordsNeeded := 1;
        end else
          TexCoordsNeeded := 0;
      end else
      if TextureNode is TNodeX3DEnvironmentTextureNode then
      begin
        if EnableSingleCubeMapTexture(0, TNodeX3DEnvironmentTextureNode(TextureNode)) then
        begin
          { TODO: this should be fixed, for non-multi tex decal
            is standard? }
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
          TexCoordsNeeded := 1;
        end else
          TexCoordsNeeded := 0;
      end else
      if TextureNode is TNodeX3DTexture3DNode then
      begin
        if EnableSingleTexture3D(0, TNodeX3DTexture3DNode(TextureNode)) then
        begin
          { TODO: this should be fixed, for non-multi tex decal
            is standard? }
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
          TexCoordsNeeded := 1;
        end else
          TexCoordsNeeded := 0;
      end else
      if TextureNode is TNodeGeneratedShadowMap then
      begin
        if EnableSingleTextureDepth(0, TNodeGeneratedShadowMap(TextureNode)) then
        begin
          { TODO: this should be fixed, for non-multi tex decal
            is standard? }
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
          TexCoordsNeeded := 1;
        end else
          TexCoordsNeeded := 0;
      end else
      if TextureNode is TNodeMultiTexture then
      begin
        { We set TexCoordsNeeded assuming that all EnableNonMultiTexture
          will succeed. In other words, we're potentially loosing a small
          optimization here: if some textures in multitexture failed
          to load, we could avoid generating texture coords for them.
          This would requre changing TexCoordsNeeded into bool array,
          and generally is not considered worthy implementing for now. }

        TexCoordsNeeded := Min(
          FreeGLTexturesCount,
          TNodeMultiTexture(TextureNode).FdTexture.Count);

        if not UseMultiTexturing then
          MinTo1st(TexCoordsNeeded, 1);

        EnableMultiTexture(TexCoordsNeeded, TNodeMultiTexture(TextureNode));
      end;
    end;

    { Disable unused textures }
    if UseMultiTexturing then
    begin
      for I := Attributes.FirstGLFreeTexture + TexCoordsNeeded to LastGLFreeTexture do
      begin
        glActiveTextureARB(GL_TEXTURE0_ARB + I);
        TextureEnableDisable(etOff);
      end;
    end else
    begin
      if TexCoordsNeeded = 0 then
        TextureEnableDisable(etOff);
    end;

    { Set alpha_test enabled state.
      Current approach: if there is any texture used,
      with simple alpha channel, then use alpha_test.

      This is not necessarily perfect for multitexturing,
      but there's really no way to set it automatically correct for
      multitexturing, as various operations may effectively flatten
      alpha anyway.

      So we only care to make it correct for a single texture case.
    }

    { Notka: nalezy tu zauwazyc ze robimy alphaTest ale jezeli
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

      To wszystko ma wieksze znaczenie gdy
      ktos zechce kombinowac finezyjne transparency
      materialu z finezyjnym (nie-0-1-kowym) kanalem alpha textury.
    }

    SetGLEnabled(GL_ALPHA_TEST, AlphaTest);

    { Make active texture 0. This is helpful for rendering code of
      some primitives that do not support multitexturing now
      (for example, primitives inside vrmlmeshrenderer_simple_nodes.inc),
      this way they will at least define correct texture coordinates
      for texture unit 0. }

    if (TexCoordsNeeded > 0) and UseMultiTexturing then
      ActiveTexture(0);
  end;

  var
    UsedGLSLProgram: TGLSLProgram;

  { Find if some shader is available and prepared for this state.
    If yes, then sets UsedGLSLProgram to non-nil and enables this shader. }
  procedure RenderShadersBegin;
  var
    I, ProgramReference: Integer;
    ProgramNode: TNodeComposedShader;
  begin
    UsedGLSLProgram := nil;

    if (not Attributes.PureGeometry) and
       (CurrentState.ParentShape <> nil) and
       (CurrentState.ParentShape.Appearance <> nil) then
    begin
      for I := 0 to CurrentState.ParentShape.Appearance.FdShaders.Items.Count - 1 do
      begin
        ProgramNode := CurrentState.ParentShape.Appearance.GLSLShader(I);
        if ProgramNode <> nil then
        begin
          ProgramReference := GLSLProgramReferences.ProgramNodeIndex(ProgramNode);
          if (ProgramReference <> -1) and
             (GLSLProgramReferences.Items[ProgramReference].GLSLProgram <> nil) then
          begin
            UsedGLSLProgram := GLSLProgramReferences.Items[ProgramReference].GLSLProgram;
            UsedGLSLProgram.Enable;
            Break;
          end;
        end;
      end;
    end;
  end;

  procedure RenderShadersEnd;
  begin
    if UsedGLSLProgram <> nil then
      UsedGLSLProgram.Disable;
  end;

begin
  { make a copy to our class fields }
  CurrentShape := Shape;
  CurrentState := Shape.State;

  { Node class, like TNodeTeapot must be mentioned here explicitly,
    to use it's Proxy, for now. This is to allow nodes with a Proxy
    still be rendered using direct specialized method, if available.
    This may be improved in the future, if the Proxy mechanism will
    get used by more nodes. }
  if (Shape.Geometry is TNodeTeapot) or
     (Shape.Geometry is TNodeExtrusion) then
    CurrentGeometry := Shape.Geometry.Proxy else
    CurrentGeometry := Shape.Geometry;
  try
    RenderShadersBegin;
    try
      InitMeshRenderer;
      try
        InitTextures;

        Render_MaterialsBegin;
        try
          {$ifdef USE_VRML_NODES_TRIANGULATION}

          { alternatywny prosty rendering przez LocalTriangulate, only to test
            LocalTriangulate. We should use here OverTriagulate = true (but it's not
            impl yet because I don't need it anywhere (well, I would use it here
            but this is just some testing code)) }
          CurrentGeometry.LocalTriangulate(State, false, @DrawTriangle);

          {$else}

          if MeshRenderer <> nil then
            MeshRenderer.Render else
            VRMLWarning(vwSerious,
              { We display for user Shape.Geometry.NodeTypeName,
                although actually it's CurrentGeometry.NodeTypeName
                that cannot be rendered. Internally, this is different
                only when we know that original Geometry cannot be directly
                renderer, so it's all Ok. }
              'Rendering of node kind "' + Shape.Geometry.NodeTypeName + '" not implemented');

          {$endif USE_VRML_NODES_TRIANGULATION}

        finally Render_MaterialsEnd end;
      finally FreeAndNil(ExposedMeshRenderer); end;
    finally RenderShadersEnd; end;
  finally
    if CurrentGeometry <> Shape.Geometry then
      FreeAndNil(CurrentGeometry);
    { Just for safety, force them @nil now }
    CurrentGeometry := nil;
    CurrentState := nil;
  end;
end;

procedure TVRMLOpenGLRenderer.RenderShapeEnd(Shape: TVRMLShape);
var
  I: Integer;
begin
  if (TextureTransformUnitsUsed <> 0) or
     (TextureTransformUnitsUsedMore.Count <> 0) then
  begin
    glMatrixMode(GL_TEXTURE);

    for I := 0 to TextureTransformUnitsUsed - 1 do
    begin
      { This code is Ok also when not UseMultiTexturing: then
        TextureTransformUnitsUsed for sure is <= 1 and ActiveTexture
        will be simply ignored. }
      ActiveTexture(I);
      glPopMatrix;
    end;

    for I := 0 to TextureTransformUnitsUsedMore.High do
    begin
      ActiveTexture(TextureTransformUnitsUsedMore.Items[I]);
      glPopMatrix;
    end;
  end;

  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  { at the end, we're in modelview mode }
end;

procedure TVRMLOpenGLRenderer.RenderShape(Shape: TVRMLShape);
begin
  RenderShapeBegin(Shape);
  try
    RenderShapeNoTransform(Shape);
  finally
    RenderShapeEnd(Shape);
  end;
end;

procedure TVRMLOpenGLRenderer.PushTextureUnit(const TexUnit: Cardinal);
begin
  { Only continue if texture unit is not already pushed
    (otherwise glPushMatrix would not be paired by exactly one glPopMatrix
    later). }

  if (TexUnit >= TextureTransformUnitsUsed) and
     (TextureTransformUnitsUsedMore.IndexOf(TexUnit) = -1) then
  begin
    glPushMatrix;

    { Simple implementation would just add always TexUnit
      to TextureTransformUnitsUsedMore. But there are optimizations possible,
      knowing that TextureTransformUnitsUsed already takes care of many units,
      and TextureTransformUnitsUsed can only be increased (by this
      very method...) between RenderShapeBegin/End.

      If texture unit is = TextureTransformUnitsUsed, this can be taken care
      of easily, just increase TextureTransformUnitsUsed. (This is an often
      case, as it happens when no texture transform was explicitly defined
      in VRML file, and only one texture unit using WORLDSPACEREFLECTIONVECTOR
      is defined; this is the most common case when using cube env mapping
      with WORLDSPACEREFLECTIONVECTOR.)

      Otherwise, we know (from previous checks) that
      TexUnit > TextureTransformUnitsUsed and it's not mentioned in
      TextureTransformUnitsUsedMore. So add it there. }

    if TexUnit = TextureTransformUnitsUsed then
      Inc(TextureTransformUnitsUsed) else
      TextureTransformUnitsUsedMore.AppendItem(TexUnit);
  end;
end;

procedure TVRMLOpenGLRenderer.SetBumpMappingLightPosition(const Value: TVector3Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_position_world_space', BumpMappingLightPosition);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightPosition := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
end;

procedure TVRMLOpenGLRenderer.SetBumpMappingLightAmbientColor(const Value: TVector4Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_ambient_color', BumpMappingLightAmbientColor);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightAmbientColor := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
end;

procedure TVRMLOpenGLRenderer.SetBumpMappingLightDiffuseColor(const Value: TVector4Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_diffuse_color', BumpMappingLightDiffuseColor);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightDiffuseColor := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
end;

procedure TVRMLOpenGLRenderer.UpdateGeneratedTextures(Shape: TVRMLShape;
  TextureNode: TVRMLNode;
  const Render: TRenderTargetFunction;
  const ProjectionNear, ProjectionFar: Single;
  const MapsOverlap: boolean;
  const MapScreenX, MapScreenY: Integer;
  var NeedsRestoreViewport: boolean);

var
  { Only for CheckUpdateField and PostUpdateField }
  UpdateIndex: Integer;
  SavedUpdateField: TSFString;

  { Look at the "update" field's value, decide whether we need updating.
    Will take care of making warning on incorrect "update". }
  function CheckUpdate(UpdateField: TSFString): boolean;
  begin
    SavedUpdateField := UpdateField; { for PostUpdateField }

    UpdateIndex := ArrayPosStr(LowerCase(UpdateField.Value),
      { Names below must be lowercase }
      ['none', 'next_frame_only', 'always']);

    { Only if update = 'NEXT_FRAME_ONLY' or 'ALWAYS' remake the texture. }
    Result := UpdateIndex > 0;

    if UpdateIndex = -1 then
      VRMLWarning(vwSerious, Format('%s.update invalid field value "%s", will be treated like "NONE"',
        [TextureNode.NodeTypeName, UpdateField.Value]));
  end;

  { Call this after CheckUpdateField returned @true and you updated
    the texture.
    Will take care of sending "NONE" after "NEXT_FRAME_ONLY". }
  procedure PostUpdate;
  begin
    { If update = 'NEXT_FRAME_ONLY', change it to 'NONE' now }
    if UpdateIndex = 1 then
    begin
      if TextureNode.ParentEventsProcessor <> nil then
        SavedUpdateField.EventIn.Send('NONE',
          (TextureNode.ParentEventsProcessor as TVRMLScene).WorldTime) else
        SavedUpdateField.Value := 'NONE';
    end;
  end;

  procedure UpdateGeneratedCubeMap(TexNode: TNodeGeneratedCubeMapTexture);
  var
    TexRefIndex: Integer;
    TexRef: PTextureCubeMapReference;
  begin
    { Shape.BoundingBox must be non-empty, otherwise we don't know from what
      3D point to capture encironment. }
    if IsEmptyBox3d(Shape.BoundingBox) then Exit;

    if CheckUpdate(TexNode.FdUpdate) then
    begin
      TexRefIndex := TextureCubeMapReferences.TextureNodeIndex(TexNode);
      if TexRefIndex <> -1 then
      begin
        TexRef := TextureCubeMapReferences.Pointers[TexRefIndex];

        GLCaptureCubeMapTexture(TexRef^.GLName, TexRef^.GeneratedSize,
          Box3dMiddle(Shape.BoundingBox),
          Render, ProjectionNear, ProjectionFar, MapsOverlap,
          MapScreenX, MapScreenY);

        if TexRef^.GeneratedNeedsMipmaps then
        begin
          { GLCaptureCubeMapTexture already bound the texture for OpenGL. }
          GenerateMipmap(GL_TEXTURE_CUBE_MAP);
        end;

        NeedsRestoreViewport := true;

        PostUpdate;

        if Log then
          WritelnLog('CubeMap', 'GeneratedCubeMapTexture texture regenerated');
      end;
    end;
  end;

  procedure UpdateGeneratedShadowMap(TexNode: TNodeGeneratedShadowMap);
  var
    Light: TNodeX3DLightNode;
    TexRefIndex: Integer;
    TexRef: PTextureDepthReference;
    ProjectionMatrix, CameraMatrix, CameraRotationOnlyMatrix: TMatrix4Single;
    Frustum: TFrustum;
    Size: Cardinal;
  begin
    if CheckUpdate(TexNode.FdUpdate) then
    begin
      if (TexNode.FdLight.Value <> nil) and
         (TexNode.FdLight.Value is TNodeX3DLightNode) then
      begin
        Light := TNodeX3DLightNode(TexNode.FdLight.Value);

        TexRefIndex := TextureDepthReferences.TextureNodeIndex(TexNode);
        if TexRefIndex <> -1 then
        begin
          TexRef := TextureDepthReferences.Pointers[TexRefIndex];

          { Render view for shadow map }
          ProjectionMatrix := Light.MapProjectionMatrix;
          CameraMatrix := Light.MapModelviewMatrix;
          CameraRotationOnlyMatrix := IdentityMatrix4Single;
          Size := TexRef^.GeneratedSize;

          glViewport(0, 0, Size, Size);

          glMatrixMode(GL_PROJECTION);
          glPushMatrix;
            glLoadMatrix(ProjectionMatrix);
            glMatrixMode(GL_MODELVIEW);

            glPushAttrib(GL_POLYGON_BIT);
              { enable polygon offset for everything (whole scene) }
              glEnable(GL_POLYGON_OFFSET_FILL); { saved by GL_POLYGON_BIT }
              glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
              glEnable(GL_POLYGON_OFFSET_POINT); { saved by GL_POLYGON_BIT }
              glPolygonOffset(TexNode.FdScale.Value, TexNode.FdBias.Value); { saved by GL_POLYGON_BIT }

              Frustum.Init(ProjectionMatrix, CameraMatrix);
              Render(rtShadowMap, CameraMatrix, CameraRotationOnlyMatrix, Frustum);
            glPopAttrib;

            glMatrixMode(GL_PROJECTION);
          glPopMatrix;
          glMatrixMode(GL_MODELVIEW);

          { Actually update OpenGL texture TexRef^.GLName }
          glBindTexture(GL_TEXTURE_2D, TexRef^.GLName);
          glReadBuffer(GL_BACK);
          glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, Size, Size);

          NeedsRestoreViewport := true;

          PostUpdate;

          if Log then
            WritelnLog('GeneratedShadowMap', 'GeneratedShadowMap texture regenerated');
        end;
      end else
        VRMLWarning(vwSerious, 'GeneratedShadowMap needs updating, but light = NULL or incorrect');
    end;
  end;

begin
  if TextureNode is TNodeGeneratedCubeMapTexture then
    UpdateGeneratedCubeMap(TNodeGeneratedCubeMapTexture(TextureNode)) else
  if TextureNode is TNodeGeneratedShadowMap then
    UpdateGeneratedShadowMap(TNodeGeneratedShadowMap(TextureNode));
end;

end.
