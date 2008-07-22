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
      potem wywoluj RenderShapeState(Node, State)
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

      Make sure that VRML2ActiveLights are properly initialized if you
      plan to render VRML 2.0 nodes. TVRMLFlatScene and descendants do
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
      blending OpenGLa, przykladowo tak robi VRMLFlatSceneGL))

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

{ TODO
  - use Backface culling in Render of Cone(all parts), Cube,
    Cylinder(all parts), Sphere when the viewer is outside
}

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
  VRMLErrors, VideosCache, GLShaders, GLImages, Videos,
  KambiTimeUtils;

{$define read_interface}

const
  DefaultBumpMappingLightAmbientColor: TVector4Single = (0, 0, 0, 1);
  DefaultBumpMappingLightDiffuseColor: TVector4Single = (1, 1, 1, 1);

type
  TBeforeGLVertexProc = procedure (Node: TVRMLGeometryNode;
    const Vert: TVector3Single) of object;

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
      built by TVRMLFlatSceneGL cannot be reused.

      If you use TVRMLFlatSceneGL this means that either:

      @unorderedList(
        @item(You use optimizations with display lists, like roSceneAsAWhole.
          Then changing BumpMappingLightPosition is very costly operation
          (display ilsts must be rebuild), so you should not do this
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
      In particular, if you use TVRMLFlatSceneGL then either
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
          The whole scene will be rendered with glDisable(GL_TEXTURE2D),
          texture coordinates will not be generated etc.
          This is for special purposes.)

        @item(When ControlTextures = @false, value of EnableTextures
          doesn't matter. See ControlTextures for description.)
      ) }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { These specify which OpenGL texture units are free to use.

      Note that for now we assume that at least one tetxure unit is free.
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

    { ponizsze parametry kontroluja min i mag filter dla tekstur.
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
      TVRMLFlatSceneGL.BumpMappingLightPosition for more comfortable version.
      See also other TVRMLOpenGLRenderer.BumpMappingLightXxx properties,
      like TVRMLOpenGLRenderer.BumpMappingLightDiffuseColor.

      There are some TODOs related to this:
      @unorderedList(
        @item(bmMultiTex* methods don't take texture transform into account correctly.)
        @item(
          We are able to calculate s/t tangent vectors (and so do bump mapping)
          only on IndexedFaceSet with explicit texture coords for now.
          IndexedFaceSet with autogenerated texture coords and other primitives
          should be done one day.)
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
    Node: TVRMLTextureNode;
    MinFilter: TGLint;
    MagFilter: TGLint;
    WrapS: TGLenum;
    WrapT: TGLenum;
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
    WrapS: TGLenum;
    WrapT: TGLenum;
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

  { Note that Attributes and State are owned by this record
    (TVRMLOpenGLRendererContextCache will make sure about creating/destroying
    them), but GeometryNode and FogNode are a references somewhere to the scene
    (they will be supplied to TVRMLOpenGLRendererContextCache instance)
    and we don't own them. }
  TShapeStateCache = record
    Attributes: TVRMLRenderingAttributes;
    GeometryNode: TVRMLGeometryNode;
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
    ShapeStateCaches: TDynShapeStateCacheArray;
    ShapeStateNoTransformCaches: TDynShapeStateCacheArray;
    RenderBeginCaches: TDynRenderBeginEndCacheArray;
    RenderEndCaches: TDynRenderBeginEndCacheArray;
    GLSLProgramCaches: TDynGLSLProgramCacheArray;

    function TextureImage_IncReference(
      const TextureImage: TImage;
      const TextureFullUrl: string;
      const TextureNode: TVRMLTextureNode;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureWrapS, TextureWrapT: TGLenum;
      const TextureColorModulator: TColorModulatorByteFunc;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const TextureNode: TNodeMovieTexture;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureWrapS, TextureWrapT: TGLenum;
      const TextureColorModulator: TColorModulatorByteFunc;
      out AlphaChannelType: TAlphaChannelType): TGLVideo;

    procedure TextureVideo_DecReference(
      const TextureVideo: TGLVideo);

    function FogParametersEqual(
      FogNode1: TNodeFog; const FogDistanceScaling1: Single;
      FogNode2: TNodeFog; const FogDistanceScaling2: Single): boolean;

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

    { These will be used by TVRMLFlatSceneGL.

      Note that we have two versions of ShapeState_IncReference,
      because if the list will already exist in the cache then we don't want to
      waste time on creating and immediately freeing unnecessary list.
      you should call ShapeState_IncReference_Existing, and if @false
      then you should build display list and call
      ShapeState_IncReference_New. }

    function ShapeState_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure ShapeState_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure ShapeState_DecReference(
      const GLList: TGLuint);

    function ShapeStateNoTransform_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      out AGLList: TGLuint): boolean;

    procedure ShapeStateNoTransform_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      const AFogDistanceScaling: Single;
      AGLList: TGLuint);

    procedure ShapeStateNoTransform_DecReference(
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
    function LastGLFreeLight: integer;

    { Use always LastGLFreeTexture, this will never return -1.
      Will return Attributes.LastGLFreeTexture, or
      glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) -1 if -1.

      To minimize number of glGetInteger calls, the result of this is cached
      in FLastGLFreeTexture. }
    FLastGLFreeTexture: integer;
    function LastGLFreeTexture: integer;

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
    GLSLProgramReferences: TDynGLSLProgramReferenceArray;

    { To which fonts we made a reference in the cache ? }
    FontsReferences: array[TVRMLFontFamily, boolean, boolean] of boolean;

    TexNormalizationCube: TGLuint;

    { ------------------------------------------------------------
      Rzeczy z ktorych mozna korzystac tylko w czasie Render. }

    { Our mesh renderer. Actually of TVRMLMeshRenderer class, but I really
      don't want to expose TVRMLMeshRenderer class in the interface. }
    ExposedMeshRenderer: TObject;

    { kopie aktualnego State i Node na czas Render }
    Render_State: TVRMLGraphTraverseState;
    Render_Node: TVRMLGeometryNode;

    { te dwie zmienne sa wewnetrzne dla funkcji MeterialsBegin/End, BindMaterial }
    Render_Material_ForcedLightDisable: boolean;
    Render_Material_BoundMatNum: integer;
    Render_Material_LastFogImmune: boolean;
    Material_BoundOpacity: Single;
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
    procedure SetColor_2(const Color: TVector3Single);

    { Judge whether the node can be lit. }
    function NodeLit(Node: TVRMLGeometryNode): boolean;

    { czy Render node'ow musi generowac tex coords ? }
    Render_TexCoordsNeeded: boolean;

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

      So the only thing that you have to care about is to specify TextureUnit <=
      LastGLFreeTexture - FirstGLFreeTexture.
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

    procedure RenderShapeStateBegin(Node: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeStateNoTransform(Node: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeStateEnd(Node: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);

    procedure RenderShapeState(Node: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);

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
      RenderShapeState...) to use new BumpMappingLightPosition.
      If you stored rendering results in display lists, you have bad luck
      --- you have to rebuild them. Reason: to recalculate
      light direction in tangent space.  Which practically means that if you
      want to change BumpMappingLightPosition often, you have to
      use roNone as Optimization for TVRMLFlatSceneGL.

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
      (or TVRMLFlatSceneGL.BumpMappingLightPosition) and attributes like
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
          (check TextureNode.IsTextureImage or TextureNode.IsTextureVideo))
      ) }
    function PreparedTextureAlphaChannelType(
      TextureNode: TVRMLTextureNode;
      out AlphaChannelType: TAlphaChannelType): boolean;
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

{ This is a world time, that is passed to time-dependent nodes.
  See X3D specification "Time" component about time-dependent nodes.
  In short, this "drives" the time passed to TimeSensor, MovieTexture
  and AudioClip. See SetWorldTime for changing this.

  Also, as an extension in our engine,
  it is passed to GLSL shaders that define
  a float/double/time uniform with a special name @code(kambi_time).
  In other words, GLSL shader nodes for now behave a little like
  time-dependent nodes. That's bacause TimeSensor, routes and events are
  not implemented yet... so we just "feed" GLSL shaders directly.
  In the future special uniform name will be removed from
  the GLSL implementation, and the same effect will be available by routing
  VRML TimeSensor to one of ComposedShader fields. This will allow
  VRML authors to achieve the same effect by standard VRML methods.

  For now, since routes and sensors are not handled, this is a quick
  and dirty and practically working way to pass your animation time
  to GLSL shaders, allowing an infinite number of interesting
  time-dependent shaders.

  Note that for now this is a global property that controls
  time for all time-dependent nodes used in all your VRML models.
  This will most probably change to something more flexible in the future
  (although greater care will have to be taken then when sharing the
  nodes, videos in MovieTexture and GLSL shader among many VRML models).

  Default value is 0.0 (zero). }
function WorldTime: TKamTime;

{ This changes world time, see WorldTime.
  It is crucial that you call this continously to have some VRML
  time-dependent features working, like MovieTexture and time for GLSL shaders.
  See WorldTime for details what is affected by this.

  This causes time to be passed to appropriate time-dependent nodes,
  events will be called etc. (TODO: actually, events are not implemented yet,
  but when they are --- this will work.)

  SetWorldTime and IncreaseWorldTime do exactly the same, the difference is
  only that for IncreaseWorldTime you specify increase in the time
  (that is, NewTime = WorldTime + TimeIncrease). Use whichever version
  is more handy.

  Following X3D specification, time should only grow. So NewValue should
  be > WorldTime (or TimeIncrease > 0).
  Otherwise we ignore this call (with SomethingChanged = always @false).
  For resetting the time (when you don't necessarily want to grow WorldTime)
  see ResetWorldTime.

  SomethingChanged is set if some time-dependent node was active between
  old WorldTime and new WorldTime. The idea is that you can use
  SomethingChanged to decide whether the scene needs to be renderer once again.
  When SomethingChanged = @false, you know that nothing actually happened
  and there's no need to render again.
  If @true, then you probably want to do something like post redisplay
  (see TGLWindow.PostRedisplay).

  @groupBegin }
procedure SetWorldTime(const NewValue: TKamTime;
  out SomethingChanged: boolean);
procedure IncreaseWorldTime(const TimeIncrease: TKamTime;
  out SomethingChanged: boolean);
{ @groupEnd }

{ Set WorldTime to arbitrary value.

  You should only use this when you loaded new VRML model.

  Unlike SetWorldTime and IncreaseWorldTime, this doesn't require that
  WorldTime grows. It still does some time-dependent events work,
  although some time-dependent nodes may be just unconditionally reset
  by this to starting value (as they keep local time, and so require
  TimeIncrease notion, without it we can only reset them).

  @groupBegin }
procedure ResetWorldTime(const NewValue: TKamTime;
  out SomethingChanged: boolean); overload;
procedure ResetWorldTime(const NewValue: TKamTime); overload;
{ @groupEnd }

{$undef read_interface}

implementation

uses NormalsCalculator, Math, Triangulator, NormalizationCubeMap,
  KambiStringUtils, GLVersionUnit, KambiLog, KambiClassUtils,
  VRMLGeometry;

{$define read_implementation}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}
{$I dynarray_5.inc}
{$I dynarray_6.inc}
{$I dynarray_7.inc}
{$I dynarray_8.inc}

{$I openglmac.inc}

{$I vrmlmeshrenderer.inc}
{$I vrmlmeshrenderer_x3d_rendering.inc}
{$I vrmlmeshrenderer_x3d_geometry3d.inc}
{$I vrmlmeshrenderer_simple_nodes.inc}

{ WorldTime and related -------------------------------------------------- }

type
  TObjectsListItem_1 = TObject;
  {$define read_interface}
  {$I objectslist_1.inc}
  {$undef read_interface}
type
  TObjectsList = TObjectsList_1;

  { List of existing nodes that depend on time.

    In the future, this is supposed to contain only TVRMLNode descendants
    that implement INodeX3DTimeDependentNode interface,
    and @bold(all) such nodes. So it will descend from TVRMLNodesList.

    For now, since events/routes are not supported, this contains
    only TNodeMovieTexture nodes (they implement INodeX3DTimeDependentNode)
    and TGLSLProgram instances that have uniform with SUniformTimeName.

    Don't place duplicates here --- for time-dependent nodes they could
    be then incremented many times during single SetWorldTime, so their
    local time would grow too fast. }
  TTimeDependentNodesList = class(TObjectsList)
  end;

const
  { If a GLSL shader in X3D will have uniform with this name
    (and of type SFFloat or SFDouble or SFTime)
    then we will use this to pass WorldTime to the shader.

    TODO: this is a hack, in the future this will be removed and
    the feature will be available using standard VRML
    (routing TimeSensor to a ComposedShader field).
    For now (since events, routing and sensors are not handled)
    this at least provides this most important variable to the shader. }
  SUniformTimeName = 'kambi_time';

var
  TimeDependentNodes: TTimeDependentNodesList;
  FWorldTime: TKamTime = 0.0;

function WorldTime: TKamTime;
begin
  Result := FWorldTime;
end;

{ Internal procedure that handles WorldTime changes.

  TimeIncrease = 0.0 here means "TimeIncrease is unknown",
  this can happen only when were called by ResetWorldTime.
  In other circumstances, TimeIncrease is > 0.

  References: see X3D specification "Time" component,
  8.2 ("concepts") for logic behind all those start/stop/pause/resumeTime,
  cycleInterval, loop properties. Why currently this is applied
  only to MovieTexture, the implementation here should be perfectly fine
  for any X3DTimeDependentNode (and will be used for TimeSensor and others
  when implemented). }
procedure InternalSetWorldTime(const NewValue, TimeIncrease: TKamTime;
  out SomethingChanged: boolean);
var
  MovieTexture: TNodeMovieTexture;

{ $define LOG_TIME_DEPENDENT_NODES}

  { Increase MovieTexture.ElapsedTime, taking care of
    MovieTexture.CycleInterval and looping.
    StopOnNonLoopedEnd says what to do if ElapsedTime passed
    CycleInterval and not looping. }
  procedure IncreaseElapsedTime(const Increase: TKamTime;
    StopOnNonLoopedEnd: boolean);
  begin
    MovieTexture.ElapsedTime := MovieTexture.ElapsedTime + Increase;
    if MovieTexture.ElapsedTime > MovieTexture.CycleInterval then
    begin
      if MovieTexture.CycleInterval <> 0 then
      begin
        if MovieTexture.FdLoop.Value then
        begin
          MovieTexture.ElapsedTime :=
            FloatModulo(MovieTexture.ElapsedTime, MovieTexture.CycleInterval);
        end else
        if StopOnNonLoopedEnd then
          MovieTexture.IsActive := false;
      end else
      begin
        { for cycleInterval = 0 this always remains 0 }
        MovieTexture.ElapsedTime := 0;

        if (not MovieTexture.FdLoop.Value) and StopOnNonLoopedEnd then
          MovieTexture.IsActive := false;
      end;
    end;
  end;

var
  I: Integer;
  Prog: TGLSLProgram;
begin
  SomethingChanged := false;

  for I := 0 to TimeDependentNodes.Count - 1 do
  begin
    if TimeDependentNodes.Items[I] is TGLSLProgram then
    begin
      Prog := TimeDependentNodes.Items[I] as TGLSLProgram;
      Prog.Enable;
      Prog.SetUniform(SUniformTimeName, NewValue);
      Prog.Disable;
      SomethingChanged := true;
    end else
    if TimeDependentNodes.Items[I] is TNodeMovieTexture then
    begin
      MovieTexture := TimeDependentNodes.Items[I] as TNodeMovieTexture;

      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog('TimeDependentNodes', Format('%s: before: active %s, paused %s, loop %s',
          [ MovieTexture.NodeTypeName,
            BoolToStr[MovieTexture.IsActive],
            BoolToStr[MovieTexture.IsPaused],
            BoolToStr[MovieTexture.FdLoop.Value]
            ]));
      {$endif}

      { For ResetWorldTime, set time-dependent node properties to default
        (like after TNodeMovieTexture creation) at the beginning. }
      if TimeIncrease = 0 then
      begin
        MovieTexture.IsActive := false;
        MovieTexture.IsPaused := false;
        MovieTexture.ElapsedTime := 0;
      end;

      if not MovieTexture.IsActive then
      begin
        if (NewValue >= MovieTexture.FdStartTime.Value) and
           ( (NewValue < MovieTexture.FdStopTime.Value) or
             { stopTime is ignored if it's <= startTime }
             (MovieTexture.FdStopTime.Value <= MovieTexture.FdStartTime.Value) ) and
           { avoid starting the movie if it should be stopped according
             to loop and cycleInterval }
           not ( (NewValue - MovieTexture.FdStartTime.Value >
                 MovieTexture.CycleInterval) and
                 (not MovieTexture.FdLoop.Value) ) then
        begin
          MovieTexture.IsActive := true;
          MovieTexture.IsPaused := false;
          MovieTexture.ElapsedTime := 0;
          { Do not advance by TimeIncrease (time from last WorldTime),
            advance only by the time passed since startTime. }
          IncreaseElapsedTime(NewValue - MovieTexture.FdStartTime.Value, true);
          SomethingChanged := true;
        end;
      end else
      if MovieTexture.IsPaused then
      begin
        if (NewValue >= MovieTexture.FdResumeTime.Value) and
           (MovieTexture.FdResumeTime.Value > MovieTexture.FdPauseTime.Value) then
        begin
          MovieTexture.IsPaused := false;
          { Advance only by the time passed since resumeTime. }
          IncreaseElapsedTime(NewValue - MovieTexture.FdResumeTime.Value, true);
          SomethingChanged := true;
        end;
      end else
      begin
        SomethingChanged := true;

        if (NewValue >= MovieTexture.FdStopTime.Value) and
           { stopTime is ignored if it's <= startTime }
           (MovieTexture.FdStopTime.Value > MovieTexture.FdStartTime.Value) then
        begin
          MovieTexture.IsActive := false;
          { advance only to the stopTime }
          if TimeIncrease <> 0 then
            IncreaseElapsedTime(TimeIncrease -
              (NewValue - MovieTexture.FdStopTime.Value), false);
        end else
        if (NewValue >= MovieTexture.FdPauseTime.Value) and
           (MovieTexture.FdPauseTime.Value > MovieTexture.FdResumeTime.Value) then
        begin
          MovieTexture.IsPaused := true;
          { advance only to the pauseTime }
          if TimeIncrease <> 0 then
            IncreaseElapsedTime(TimeIncrease -
              (NewValue - MovieTexture.FdPauseTime.Value), false);
        end else
        begin
          { active and not paused movie }
          if TimeIncrease = 0 then
            MovieTexture.ElapsedTime := 0 else
            IncreaseElapsedTime(TimeIncrease, true);
        end;
      end;

      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog('TimeDependentNodes', Format('%s: after: active %s, paused %s',
          [ MovieTexture.NodeTypeName,
            BoolToStr[MovieTexture.IsActive],
            BoolToStr[MovieTexture.IsPaused]]));
      {$endif}
    end else
      raise EInternalError.Create('for now, only TNodeMovieTexture and TGLSLProgram should be present in TimeDependentNodes');
  end;

  FWorldTime := NewValue;
end;

procedure SetWorldTime(const NewValue: TKamTime;
  out SomethingChanged: boolean);
var
  TimeIncrease: TKamTime;
begin
  TimeIncrease := NewValue - FWorldTime;
  if TimeIncrease > 0 then
    InternalSetWorldTime(NewValue, TimeIncrease, SomethingChanged);
end;

procedure IncreaseWorldTime(const TimeIncrease: TKamTime;
  out SomethingChanged: boolean);
begin
  if TimeIncrease > 0 then
    InternalSetWorldTime(FWorldTime + TimeIncrease, TimeIncrease, SomethingChanged);
end;

procedure ResetWorldTime(const NewValue: TKamTime;
  out SomethingChanged: boolean);
begin
  InternalSetWorldTime(NewValue, 0, SomethingChanged);
end;

procedure ResetWorldTime(const NewValue: TKamTime);
var
  SomethingChanged: boolean;
begin
  ResetWorldTime(NewValue, SomethingChanged);
  { just ignore SomethingChanged }
end;

{ TVRMLOpenGLRendererContextCache -------------------------------------------- }

{ $define DEBUG_VRML_RENDERER_CACHE}

constructor TVRMLOpenGLRendererContextCache.Create;
begin
  inherited;
  TextureImageCaches := TDynTextureImageCacheArray.Create;
  TextureVideoCaches := TDynTextureVideoCacheArray.Create;
  ShapeStateCaches := TDynShapeStateCacheArray.Create;
  ShapeStateNoTransformCaches := TDynShapeStateCacheArray.Create;
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

  if ShapeStateCaches <> nil then
  begin
    Assert(ShapeStateCaches.Count = 0, 'Some references to ShapeStates still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(ShapeStateCaches);
  end;

  if ShapeStateNoTransformCaches <> nil then
  begin
    Assert(ShapeStateNoTransformCaches.Count = 0,
      'Some references to ShapeStatesNoTransform still exist' +
      ' when freeing TVRMLOpenGLRendererContextCache');
    FreeAndNil(ShapeStateNoTransformCaches);
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

function TVRMLOpenGLRendererContextCache.TextureImage_IncReference(
  const TextureImage: TImage;
  const TextureFullUrl: string;
  const TextureNode: TVRMLTextureNode;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureWrapS, TextureWrapT: TGLenum;
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
         (TextureCached^.Node = TextureNode) ) and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.WrapS = TextureWrapS) and
       (TextureCached^.WrapT = TextureWrapT) and
       ({$ifndef FPC_OBJFPC} @ {$endif} TextureCached^.ColorModulator =
        {$ifndef FPC_OBJFPC} @ {$endif} TextureColorModulator) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  { Initialize Result first, before calling TextureImageCaches.IncLength.
    That's because in case LoadGLTextureModulated raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureImage_DecReference later). }
  Result := LoadGLTextureModulated(
    TextureImage, TextureMinFilter, TextureMagFilter,
    TextureWrapS, TextureWrapT, TextureColorModulator);

  TextureImageCaches.IncLength;
  TextureCached := TextureImageCaches.Pointers[TextureImageCaches.High];
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.Node := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.WrapS := TextureWrapS;
  TextureCached^.WrapT := TextureWrapT;
  TextureCached^.ColorModulator := TextureColorModulator;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := TextureImage.AlphaChannelType(5, 0.1);
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
  const TextureWrapS, TextureWrapT: TGLenum;
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
       (TextureCached^.WrapS = TextureWrapS) and
       (TextureCached^.WrapT = TextureWrapT) and
       ({$ifndef FPC_OBJFPC} @ {$endif} TextureCached^.ColorModulator =
        {$ifndef FPC_OBJFPC} @ {$endif} TextureColorModulator) then
    begin
      Inc(TextureCached^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLVideo);
    end;
  end;

  { Initialize Result first, before calling TextureVideoCaches.IncLength.
    That's because in case TGLVideo.Create raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureVideo_DecReference later). }
  Result := TGLVideo.Create(
    TextureVideo, TextureMinFilter, TextureMagFilter,
    TextureWrapS, TextureWrapT, TextureColorModulator);

  TextureVideoCaches.IncLength;
  TextureCached := TextureVideoCaches.Pointers[TextureVideoCaches.High];
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.InitialNode := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.WrapS := TextureWrapS;
  TextureCached^.WrapT := TextureWrapT;
  TextureCached^.ColorModulator := TextureColorModulator;
  TextureCached^.References := 1;
  TextureCached^.GLVideo := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := TextureVideo.AlphaChannelType(5, 0.1);
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

function TVRMLOpenGLRendererContextCache.GLSLProgram_IncReference(
  ProgramNode: TNodeComposedShader): TGLSLProgram;

  procedure LoadGLSLProgram(GLSLProgram: TGLSLProgram;
    ProgramNode: TNodeComposedShader);
  var
    I: Integer;
    Part: TNodeShaderPart;
    Source: String;
    HasAnyShader: boolean;
    UniformField: TVRMLField;
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

          VRMLNonFatalError(Format('Unknown type for ShaderPart: "%s"',
            [Part.FdType.Value]));
      end;

    if not HasAnyShader then
      raise EGLSLError.Create('No vertex and no fragment shader for GLSL program');

    GLSLProgram.Link(true);

    if ProgramNode.InterfaceDeclarations.Count > 0 then
    begin
      { program must be active to set uniform values. }
      GLSLProgram.Enable;

      for I := 0 to ProgramNode.InterfaceDeclarations.Count - 1 do
      begin
        UniformField := ProgramNode.InterfaceDeclarations.Items[I].Field;
        if UniformField <> nil then
        begin
          { Ok, we have a field with a value (interface declarations with
            fields inside ComposedShader always have a value, we force it
            when parsing in TNodeComposedShader.ParseNodeBodyElement).
            So set GLSL uniform variable from this field. }

          try
            if UniformField is TSFLong then
              { Handling of SFLong also takes care of SFInt32. }
              GLSLProgram.SetUniform(UniformField.Name, TSFLong(UniformField).Value) else
            if UniformField is TSFVec2f then
              GLSLProgram.SetUniform(UniformField.Name, TSFVec2f(UniformField).Value) else
            if UniformField is TSFVec3f then
              { Handling of SFVec3f also takes care of SFColor.
                Although X3D spec says SFColor should go to vec4, not vec3,
                but this is an error? SFColor has just 3 components... }
              GLSLProgram.SetUniform(UniformField.Name, TSFVec3f(UniformField).Value) else
            if UniformField is TSFVec4f then
              GLSLProgram.SetUniform(UniformField.Name, TSFVec4f(UniformField).Value) else
            if UniformField is TSFRotation then
              GLSLProgram.SetUniform(UniformField.Name, TSFRotation(UniformField).Value) else
            if UniformField is TSFMatrix3f then
              GLSLProgram.SetUniform(UniformField.Name, TSFMatrix3f(UniformField).Value) else
            if UniformField is TSFMatrix4f then
              GLSLProgram.SetUniform(UniformField.Name, TSFMatrix4f(UniformField).Value) else
            if UniformField is TSFFloat then
            begin
              if UniformField.Name = SUniformTimeName then
              begin
                GLSLProgram.SetUniform(UniformField.Name, WorldTime);
                TimeDependentNodes.Add(GLSLProgram);
              end else
                GLSLProgram.SetUniform(UniformField.Name, TSFFloat(UniformField).Value);
            end else
            if UniformField is TSFDouble then
            begin
              { Handling SFDouble also takes care of it's descendant SFTime }
              if UniformField.Name = SUniformTimeName then
              begin
                GLSLProgram.SetUniform(UniformField.Name, WorldTime);
                TimeDependentNodes.Add(GLSLProgram);
              end else
                GLSLProgram.SetUniform(UniformField.Name, TSFDouble(UniformField).Value);
            end else
              VRMLNonFatalError('Setting uniform GLSL variable from X3D field type "' + UniformField.VRMLTypeName + '" not supported');

            { TODO: other field types, full list is in X3D spec in
              "OpenGL shading language (GLSL) binding" }

          except
            { X3D spec "OpenGL shading language (GLSL) binding" says
              "If the name is not available as a uniform variable in the
              provided shader source, the values of the node shall be ignored"
              (although it says when talking about "Vertex attributes",
              seems they mixed attributes and uniforms meaning in spec?).

              So we catch EGLSLUniformNotFound and don't even report it
              by VRMLNonFatalError. (Still, we report to debug WritelnLog.) }
            on E: EGLSLUniformNotFound do
            begin
              if Log then
                WritelnLog('GLSL', 'ComposedShader specifies uniform variable ' +
                  'name not found (or not used) in the shader source: ' +
                  E.Message);
            end;
          end;
        end;
      end;

      { TODO: this should restore previously bound program }
      GLSLProgram.Disable;
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

  { Initialize Result first, before calling GLSLProgramCaches.IncLength.
    That's because in case of loading problems,
    we don't want to add program to cache (because caller would have
    no way to call GLSLProgram_DecReference later). }

  Result := TGLSLProgram.Create;
  try
    LoadGLSLProgram(Result, ProgramNode);
  except
    { In case of problems with initializing GLSL program, free the program
      and reraise exception. Caller of GLSLProgram_IncReference will
      decide what to do with it (TVRMLOpenGLRenderer will make VRMLNonFatalError
      and record that this shader program failed to initialize by recording
      GLSLProgram = nil). }
    FreeAndNil(Result);
    raise;
  end;

  GLSLProgramCaches.IncLength;
  GLSLProgramCache := GLSLProgramCaches.Pointers[GLSLProgramCaches.High];
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
  I: Integer;
  GLSLProgramCache: PGLSLProgramCache;
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
        TimeDependentNodes.Delete(GLSLProgramCache^.GLSLProgram);
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

function TVRMLOpenGLRendererContextCache.ShapeState_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
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
       (SSCache^.GeometryNode = AGeometryNode) and
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
  AGeometryNode: TVRMLGeometryNode;
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
  SSCache^.GeometryNode := AGeometryNode;
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

function TVRMLOpenGLRendererContextCache.ShapeStateNoTransform_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  SSCache: PShapeStateCache;
begin
  for I := 0 to ShapeStateNoTransformCaches.High do
  begin
    SSCache := ShapeStateNoTransformCaches.Pointers[I];
    if (SSCache^.Attributes.Equals(AAttributes)) and
       (SSCache^.GeometryNode = AGeometryNode) and
       (SSCache^.State.EqualsNoTransform(AState)) and
       FogParametersEqual(
         SSCache^.FogNode, SSCache^.FogDistanceScaling,
                 AFogNode,         AFogDistanceScaling) then
    begin
      Inc(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ShapeState NoTransform ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      AGLList := SSCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLOpenGLRendererContextCache.ShapeStateNoTransform_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  const AFogDistanceScaling: Single;
  AGLList: TGLuint);
var
  SSCache: PShapeStateCache;
begin
  ShapeStateNoTransformCaches.IncLength;
  SSCache := ShapeStateNoTransformCaches.Pointers[
    ShapeStateNoTransformCaches.High];
  SSCache^.Attributes := AAttributes;
  SSCache^.GeometryNode := AGeometryNode;
  SSCache^.State := AState;
  SSCache^.FogNode := AFogNode;
  SSCache^.FogDistanceScaling := AFogDistanceScaling;
  SSCache^.GLList := AGLList;
  SSCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ShapeState NoTransform ', SSCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLOpenGLRendererContextCache.ShapeStateNoTransform_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  SSCache: PShapeStateCache;
begin
  for I := 0 to ShapeStateNoTransformCaches.High do
  begin
    SSCache := ShapeStateNoTransformCaches.Pointers[I];
    if SSCache^.GLList = GLList then
    begin
      Dec(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ShapeState NoTransform ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      if SSCache^.References = 0 then
      begin
        FreeAndNil(SSCache^.Attributes);
        FreeAndNil(SSCache^.State);
        glFreeDisplayList(SSCache^.GLList);
        ShapeStateNoTransformCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLOpenGLRendererContextCache.ShapeStateNoTransform_DecReference: ' +
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
    ({$ifndef FPC_OBJFPC} @ {$endif} TVRMLRenderingAttributes(SecondValue).OnBeforeGLVertex =
     {$ifndef FPC_OBJFPC} @ {$endif} OnBeforeGLVertex) and
    (TVRMLRenderingAttributes(SecondValue).SmoothShading = SmoothShading) and
    ({$ifndef FPC_OBJFPC} @ {$endif} TVRMLRenderingAttributes(SecondValue).ColorModulatorSingle =
     {$ifndef FPC_OBJFPC} @ {$endif} ColorModulatorSingle) and
    ({$ifndef FPC_OBJFPC} @ {$endif} TVRMLRenderingAttributes(SecondValue).ColorModulatorByte =
     {$ifndef FPC_OBJFPC} @ {$endif} ColorModulatorByte) and
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
  GLSLProgramReferences := TDynGLSLProgramReferenceArray.Create;

  OwnsCache := ACache = nil;
  if OwnsCache then
    FCache := TVRMLOpenGLRendererContextCache.Create else
    FCache := ACache;
end;

destructor TVRMLOpenGLRenderer.Destroy;
begin
  UnprepareAll;
  FreeAndNil(TextureImageReferences);
  FreeAndNil(TextureVideoReferences);
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
            except
              { EGLSLError catches errors from Cache.GLSLProgram_IncReference,
                including GLShaders errors like
                EGLSLShaderCompileError or EGLSLProgramLinkError }
              on E: EGLSLError do
              begin
                VRMLNonFatalError('Error when initializing GLSL shader : ' + E.Message);
                GLSLProgram := nil;
              end;
            end;

            { Whether GLSLProgram is nil or not (GLSLProgram_IncReference
              succeded or not), we add record to GLSLProgramReferences }
            GLSLProgramReferences.IncLength;
            GLSLProgramReference := GLSLProgramReferences.Pointers[GLSLProgramReferences.High];
            GLSLProgramReference^.ProgramNode := ProgramNode;
            GLSLProgramReference^.GLSLProgram := GLSLProgram;
          end;

          { Only if successfull, break. }
          if GLSLProgram <> nil then
            Break;
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
var
  TextureImageReference: TTextureImageReference;
  TextureVideoReference: TTextureVideoReference;
  TextureNode: TVRMLTextureNode;
  FontStyle: TNodeFontStyle_2;
  HeightMapGrayscale: TGrayscaleImage;
  OriginalTexture: TImage;
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

 { przygotuj teksture }
 { Conditions below describing when the texture is added to the cache
   are reflected in PreparedTextureAlphaChannelType interface. }

 TextureNode := State.Texture;
 if (not Attributes.PureGeometry) and
    (TextureNode <> nil) and
    (TextureImageReferences.TextureNodeIndex(TextureNode) = -1) and
    (TextureVideoReferences.TextureNodeIndex(TextureNode) = -1) then
 begin
  TextureNode.ImagesVideosCache := Cache;

  if TextureNode.IsTextureImage then
  begin
   TextureImageReference.Node := TextureNode;
   TextureImageReference.GLName := Cache.TextureImage_IncReference(
     TextureNode.TextureImage,
     TextureNode.TextureUsedFullUrl,
     TextureNode,
     Attributes.TextureMinFilter,
     Attributes.TextureMagFilter,
     TextureRepeatToGL[TextureNode.RepeatS],
     TextureRepeatToGL[TextureNode.RepeatT],
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
         GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR,
         TextureRepeatToGL[TextureNode.RepeatS],
         TextureRepeatToGL[TextureNode.RepeatT]);
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
             GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR,
             TextureRepeatToGL[TextureNode.RepeatS],
             TextureRepeatToGL[TextureNode.RepeatT]);
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
     Attributes.TextureMinFilter,
     Attributes.TextureMagFilter,
     TextureRepeatToGL[TextureNode.RepeatS],
     TextureRepeatToGL[TextureNode.RepeatT],
     Attributes.ColorModulatorByte,
     { This way, our AlphaChannelType is calculated (or taken from cache)
       by TextureVideo_IncReference }
     TextureVideoReference.AlphaChannelType);

   TextureVideoReferences.AppendItem(TextureVideoReference);
   TimeDependentNodes.Add(TextureNode);
  end;
 end;

  PrepareGLSLProgram;
end;

procedure TVRMLOpenGLRenderer.Unprepare(Node: TVRMLNode);
var i: integer;
begin
 {zwracam uwage ze nie niszczymy tu fontow dla (LastNode is TNodeFontStyle)
  bo fonty sa gromadzone w tablicy Cache.Fonts i wszystkie Font Styles
  o takich samych wlasciwosciach Family i Style korzystaja zawsze z tego
  samego juz utworzonego fontu.}

 {niszczymy teksture}
 if Node is TVRMLTextureNode then
 begin
   i := TextureImageReferences.TextureNodeIndex(TVRMLTextureNode(Node));
   if i >= 0 then
   begin
     Cache.TextureImage_DecReference(TextureImageReferences.Items[i].GLName);
     TextureImageReferences.Delete(i, 1);
   end;

   i := TextureVideoReferences.TextureNodeIndex(TVRMLTextureNode(Node));
   if i >= 0 then
   begin
     Cache.TextureVideo_DecReference(TextureVideoReferences.Items[i].GLVideo);
     TextureVideoReferences.Delete(i, 1);
     TimeDependentNodes.Delete(Node);
   end;
 end;

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
  begin
    Cache.TextureVideo_DecReference(TextureVideoReferences.Items[i].GLVideo);
    TimeDependentNodes.Delete(TextureVideoReferences.Items[I].Node);
  end;
  TextureVideoReferences.SetLength(0);

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

function TVRMLOpenGLRenderer.LastGLFreeTexture: Integer;
begin
  if FLastGLFreeTexture = -1 then
  begin
    if Attributes.LastGLFreeTexture = -1 then
    begin
      { actually get this from OpenGL }
      if GL_ARB_multitexture then
        FLastGLFreeTexture := glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) - 1 else
        FLastGLFreeTexture := 0;
    end else
      FLastGLFreeTexture := Attributes.LastGLFreeTexture;
  end;
  Result := FLastGLFreeTexture;
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
      ALastGLFreeTexture := glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) - 1 else
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
  TextureNode: TVRMLTextureNode;
  out AlphaChannelType: TAlphaChannelType): boolean;
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
     VRMLNonFatalError('Unknown fog type "' + FogNode.FdFogType.Value + '"');
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
 {push attribs and matrices (by pushing attribs FIRST we save also current
  matrix mode)}
 glPushAttrib(GL_ALL_ATTRIB_BITS);
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

procedure TVRMLOpenGLRenderer.RenderShapeStateBegin(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState);
var
  TextureTransform: TNodeTextureTransform;
begin
  if (State.ParentShape = nil { VRML 1.0, always some texture transform }) or
     (State.ParentShape.TextureTransform <> nil { VRML 2.0 with tex transform }) then
  begin
    glMatrixMode(GL_TEXTURE);

    { TODO: for bump mapping, TextureTransform should be done on more than one texture unit. }
    ActiveTexture(0);
    glPushMatrix;

    if State.ParentShape = nil then
      glMultMatrix(State.TextureTransform) else
    begin
      TextureTransform := State.ParentShape.TextureTransform;
      if TextureTransform <> nil then
      begin
        { Alternative version of the code below:
            glMultMatrix(TextureTransform.Matrix);
          See TNodeTextureTransform.Matrix comments.
          Below we do the same thing, but we just implement this
          directly by calling OpenGL commands. }
        with TextureTransform do
        begin
          glTranslatef(
            FdTranslation.Value[0] + FdCenter.Value[0],
            FdTranslation.Value[1] + FdCenter.Value[1], 0);
          glRotatef(RadToDeg(FdRotation.Value), 0, 0, 1);
          glScalef(FdScale.Value[0], FdScale.Value[1], 1);
          glTranslatef(-FdCenter.Value[0], -FdCenter.Value[1], 0);
        end;
      end;
    end;
  end;

  glMatrixMode(GL_MODELVIEW);

  { uwzglednij atrybut Attributes.UseLights : jezeli jest = true to zdefiniuj
    OpenGLowi wszystkie State.ActiveLights. Robimy to PRZED zaladowaniem
    transformacji State.Transform (bo swiatla maja wlasne Transform i
    nie podlegaja transformacji aktualnego State'a w ktorym sa) }
  if Attributes.UseLights then
    glLightsFromVRML(State.CurrentActiveLights,
      Attributes.FirstGLFreeLight, LastGLFreeLight,
      Attributes.ColorModulatorSingle);

  glPushMatrix;
    glMultMatrix(State.Transform);
end;

procedure TVRMLOpenGLRenderer.RenderShapeStateNoTransform(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState);

  function NodeTextured(Node: TVRMLGeometryNode): boolean;
  begin
    Result := not (
      (Node is TNodePointSet_2) or
      (Node is TNodeIndexedLineSet_2));
  end;

  { If current Node should be rendered using one of TVRMLMeshRenderer
    classes, then create appropriate MeshRenderer. Takes care
    of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render.
    Otherwise, MeshRenderer is set to @nil. }
  procedure InitMeshRenderer;
  begin
    if Node is TNodeIndexedTriangleMesh_1 then
      ExposedMeshRenderer := TIndexedTriangleMesh_1Renderer.Create(Self, TNodeIndexedTriangleMesh_1(Node)) else
    if Node is TNodeIndexedFaceSet_1 then
      ExposedMeshRenderer := TIndexedFaceSet_1Renderer.Create(Self, TNodeIndexedFaceSet_1(Node)) else
    if Node is TNodeIndexedFaceSet_2 then
      ExposedMeshRenderer := TIndexedFaceSet_2Renderer.Create(Self, TNodeIndexedFaceSet_2(Node)) else
    if Node is TNodeIndexedLineSet_1 then
      ExposedMeshRenderer := TIndexedLineSet_1Renderer.Create(Self) else
    if (Node is TNodeIndexedLineSet_2) or
       (Node is TNodeLineSet) then
      ExposedMeshRenderer := TLineSet_2Renderer.Create(Self) else
    if Node is TNodePointSet_1 then
      ExposedMeshRenderer := TPointSet_1Renderer.Create(Self) else
    if Node is TNodePointSet_2 then
      ExposedMeshRenderer := TPointSet_2Renderer.Create(Self) else
    if Node is TNodeElevationGrid then
      ExposedMeshRenderer := TElevationGridRenderer.Create(Self) else
    if Node is TNodeExtrusion then
      ExposedMeshRenderer := TExtrusionRenderer.Create(Self) else
    if (Node is TNodeTriangleSet) or
       (Node is TNodeIndexedTriangleSet) then
      ExposedMeshRenderer := TTriangleSetRenderer.Create(Self) else
    if (Node is TNodeTriangleFanSet) or
       (Node is TNodeIndexedTriangleFanSet) then
      ExposedMeshRenderer := TTriangleFanSetRenderer.Create(Self) else
    if (Node is TNodeTriangleStripSet) or
       (Node is TNodeIndexedTriangleStripSet) then
      ExposedMeshRenderer := TTriangleStripSetRenderer.Create(Self) else
    if (Node is TNodeQuadSet) or
       (Node is TNodeIndexedQuadSet) then
      ExposedMeshRenderer := TQuadSetRenderer.Create(Self) else
    if Node is TNodeAsciiText_1 then
      ExposedMeshRenderer := TAsciiTextRenderer.Create(Self) else
    if Node is TNodeText then
      ExposedMeshRenderer := TTextRenderer.Create(Self) else
    if Node is TNodeText3D then
      ExposedMeshRenderer := TText3DRenderer.Create(Self) else
    if Node is TNodeCone_1 then
      ExposedMeshRenderer := TCone_1Renderer.Create(Self) else
    if Node is TNodeCone_2 then
      ExposedMeshRenderer := TCone_2Renderer.Create(Self) else
    if Node is TNodeCube_1 then
      ExposedMeshRenderer := TCube_1Renderer.Create(Self) else
    if Node is TNodeBox then
      ExposedMeshRenderer := TBoxRenderer.Create(Self) else
    if Node is TNodeCylinder_1 then
      ExposedMeshRenderer := TCylinder_1Renderer.Create(Self) else
    if Node is TNodeCylinder_2 then
      ExposedMeshRenderer := TCylinder_2Renderer.Create(Self) else
    if Node is TNodeSphere_1 then
      ExposedMeshRenderer := TSphere_1Renderer.Create(Self) else
    if Node is TNodeSphere_2 then
      ExposedMeshRenderer := TSphere_2Renderer.Create(Self) else
      ExposedMeshRenderer := nil;
  end;

  procedure InitTextures;

    procedure EnableClassicTexturing(GLTexture: TGLuint);
    begin
      ActiveTexture(0);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      glBindTexture(GL_TEXTURE_2D, GLTexture);
    end;

  var
    AlphaTest: boolean;
    IndexedFaceRenderer: TIndexedFaceSetRenderer;
    TextureReferencesIndex: Integer;
    TextureNode: TVRMLTextureNode;
    Success: boolean;
    TexImageReference: PTextureImageReference;
    TexVideoReference: PTextureVideoReference;
    VideoTime: TKamTime;
  begin
    if Attributes.PureGeometry then
    begin
      Render_TexCoordsNeeded := false;
      Exit;
    end;

    if not Attributes.ControlTextures then
    begin
      { require texture coordinates, but don't do anything else
        (like setting active texture, enabling/disabling it, don't even look
        at VRML texture node.) }
      Render_TexCoordsNeeded := true;
      Exit;
    end;

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

     To wszystko ma wieksze znaczenie gdy
     ktos zechce kombinowac finezyjne transparency
     materialu z finezyjnym (nie-0-1-kowym) kanalem alpha textury.
    }
    TextureNode := State.Texture;
    {$ifdef USE_VRML_NODES_TRIANGULATION}
    { We don't generate texture coords, so disable textures. }
    TextureNode := nil;
    {$endif}

    Success := false;

    if (TextureNode <> nil) and
       Attributes.EnableTextures and
       NodeTextured(Node) then
    begin
      { Note: don't call IsTextureImage, IsTextureVideo here --- this
        would causes reloading images/videos, nullifying
        TVRMLFlatScene.FreeResouces([frTextureDataInNodes]) purpose.

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

        AlphaTest := TexImageReference^.AlphaChannelType = atSimpleYesNo;

        if (MeshRenderer <> nil) and
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
          end else
            EnableClassicTexturing(TexImageReference^.GLName);
        end else
          EnableClassicTexturing(TexImageReference^.GLName);

        Render_TexCoordsNeeded := true;
        Success := true;
      end else
      begin
        TextureReferencesIndex := TextureVideoReferences.TextureNodeIndex(
          TextureNode);
        if TextureReferencesIndex <> -1 then
        begin
          TexVideoReference := TextureVideoReferences.Pointers[TextureReferencesIndex];

          AlphaTest := TexVideoReference^.AlphaChannelType = atSimpleYesNo;

          VideoTime := TexVideoReference^.Node.ElapsedTime *
                       TexVideoReference^.Node.FdSpeed.Value;
          if TexVideoReference^.Node.FdSpeed.Value < 0 then
            VideoTime := TexVideoReference^.Node.Duration + VideoTime;

          EnableClassicTexturing(
            TexVideoReference^.GLVideo.GLTextureFromTime(VideoTime));

          Render_TexCoordsNeeded := true;
          Success := true;
        end;
      end;
    end;

    if not Success then
    begin
      ActiveTexture(0);
      glDisable(GL_TEXTURE_2D);
      AlphaTest := false;
      Render_TexCoordsNeeded := false;
    end;

    SetGLEnabled(GL_ALPHA_TEST, AlphaTest);
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
       (State.ParentShape <> nil) and
       (State.ParentShape.Appearance <> nil) then
    begin
      for I := 0 to State.ParentShape.Appearance.FdShaders.Items.Count - 1 do
      begin
        ProgramNode := State.ParentShape.Appearance.GLSLShader(I);
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
  {zrob nasze kopie}
  Render_State := State;
  Render_Node := Node;

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
        Node.LocalTriangulate(State, false, @DrawTriangle);

        {$else}

        if MeshRenderer <> nil then
          MeshRenderer.Render else
          VRMLNonFatalError(
            'Rendering of node kind "' + Node.NodeTypeName + '" not implemented');

        {$endif USE_VRML_NODES_TRIANGULATION}

      finally Render_MaterialsEnd end;
    finally FreeAndNil(ExposedMeshRenderer); end;
  finally RenderShadersEnd; end;
end;

procedure TVRMLOpenGLRenderer.RenderShapeStateEnd(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState);
begin
  if (State.ParentShape = nil { VRML 1.0, always some texture transform }) or
     (State.ParentShape.TextureTransform <> nil { VRML 2.0 with tex transform }) then
  begin
    ActiveTexture(0);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
  end;

  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  { at the end, we're in modelview mode }
end;

procedure TVRMLOpenGLRenderer.RenderShapeState(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState);
begin
  RenderShapeStateBegin(Node, State);
  try
    RenderShapeStateNoTransform(Node, State);
  finally
    RenderShapeStateEnd(Node, State);
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

initialization
  TimeDependentNodes := TTimeDependentNodesList.Create;
finalization
  FreeAndNil(TimeDependentNodes);
end.
