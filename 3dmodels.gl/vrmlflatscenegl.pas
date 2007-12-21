{
  Copyright 2003-2007 Michalis Kamburelis.

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

{ @abstract(@link(TVRMLFlatSceneGL) class.) }

unit VRMLFlatSceneGL;

{$I openglmac.inc}

{ TODO --- demo that roSeparateShapeStates is great (better than roNone
  and roSceneAsAWhole) when you change only small part of an object
  at runtime and you're calling ChangedShapeState because change is very local.
}

{ RenderFrustum tests each ShapeState for collision with given Frustum
  before rendering this ShapeState. It can use ShapeState.BoundingBox
  (if RENDER_FRUSTUM_USES_BOUNDING_BOX is defined)
  or ShapeState.BoundingSphere (if RENDER_FRUSTUM_USES_BOUNDING_SPHERE
  is defined) or both, i.e. first test versus ShapeState.BoundingSphere
  and then, if it succeeds, test also versus ShapeState.BoundingBox
  (if RENDER_FRUSTUM_USES_BOTH).

  ShapeState.BoundingBox is (in a current implementation) always
  a better approximation of shape geometry than ShapeState.BoundingSphere.
  So advantage of using ShapeState.BoundingBox is that more ShapeStates
  may be eliminated. Advantage of using ShapeState.BoundingSphere
  is that checking for collision Frustum<->Sphere is much faster,
  so you don't waste so much time on testing for collisions between
  frustum and ShapeState.

  My tests show that in practice performance is the best (but differences
  in speed are not large) when RENDER_FRUSTUM_USES_BOUNDING_SPHERE is used.
  You can experiment with this if you like.

  Exactly one of defines RENDER_FRUSTUM_USES_xxx must be defined. }
{$define RENDER_FRUSTUM_USES_BOUNDING_SPHERE}
{ $define RENDER_FRUSTUM_USES_BOUNDING_BOX}
{ $define RENDER_FRUSTUM_USES_BOTH}

{ With this you can fine-tune performance of RenderFrustumOctree.
  Exactly one of symbols RENDER_FRUSTUM_OCTREE_xxx below must be defined.
  See implementation of @link(TVRMLFlatSceneGL.RenderFrustumOctree)
  to see what each symbol means.

  My tests show that RENDER_FRUSTUM_OCTREE_NO_BONUS_CHECKS
  yield better performance.
}
{$define RENDER_FRUSTUM_OCTREE_NO_BONUS_CHECKS}
{ $define RENDER_FRUSTUM_OCTREE_BONUS_SPHERE_CHECK}

interface

uses
  SysUtils, Classes, VectorMath, Boxes3d, VRMLNodes, KambiClassUtils, KambiUtils,
  VRMLFlatScene, VRMLOpenGLRenderer, OpenGLh, BackgroundGL, KambiGLUtils,
  VRMLShapeStateOctree, VRMLGLHeadLight, VRMLRendererOptimization,
  ShadowVolumesHelper;

{$define read_interface}

const
  { }
  DefaultBlendingSourceFactor = GL_SRC_ALPHA;

  { Default value of Attributes.BlendingDestinationFactor.

    Why isn't it GL_ONE_MINUS_SRC_ALPHA ?
    See [http://vrmlengine.sourceforge.net/vrml_engine_doc.php],
    chapter "OpenGL rendering", section about "mat transparency
    using blending".

    Polish: Wada GL_ONE jest fakt ze wynikowy obraz bedzie bardzo jasny
    tam gdzie widoczne sa obiekty blended. (bo GL_ONE zawsze tylko
    zwieksza kolor obrazu).
    Natomiast wada GL_ONE_MINUS_SRC_ALPHA jest fakt ze z wynikowego
    obrazu moze za szybko zniknac kolor obiektu nie-blended ktory
    byl za obiektami blended (bo GL_ONE_MINUS_SRC_ALPHA bedzie
    go za kazdym razem zmniejszala). }
  DefaultBlendingDestinationFactor = GL_ONE {_MINUS_SRC_ALPHA};

type
  TGLRendererOptimization = VRMLRendererOptimization.TGLRendererOptimization;
  PGLRendererOptimization = VRMLRendererOptimization.PGLRendererOptimization;

const
  roNone = VRMLRendererOptimization.roNone;
  roSceneAsAWhole = VRMLRendererOptimization.roSceneAsAWhole;
  roSeparateShapeStates = VRMLRendererOptimization.roSeparateShapeStates;
  roSeparateShapeStatesNoTransform = VRMLRendererOptimization.roSeparateShapeStatesNoTransform;

type
  { Internal for TVRMLFlatSceneGL }
  TRenderShapeState = procedure(ShapeStateNum: Integer) of object;
  TObjectProcedure = procedure of object;

  TTestShapeStateVisibility = function(ShapeStateNum: Integer): boolean
    of object;

  TVRMLFlatSceneGLsList = class;

  TVRMLSceneRenderingAttributes = class(TVRMLRenderingAttributes)
  private
    { Scenes that use Renderer with this TVRMLSceneRenderingAttributes instance. }
    FScenes: TVRMLFlatSceneGLsList;

    FBlending: boolean;
    FBlendingSourceFactor: TGLenum;
    FBlendingDestinationFactor: TGLenum;
  protected
    procedure SetOnBeforeGLVertex(const Value: TBeforeGLVertexProc); override;
    procedure SetSmoothShading(const Value: boolean); override;
    procedure SetColorModulatorSingle(const Value: TColorModulatorSingleFunc); override;
    procedure SetColorModulatorByte(const Value: TColorModulatorByteFunc); override;
    procedure SetUseLights(const Value: boolean); override;
    procedure SetFirstGLFreeLight(const Value: Cardinal); override;
    procedure SetLastGLFreeLight(const Value: integer); override;
    procedure SetEnableTextures(const Value: boolean); override;
    procedure SetFirstGLFreeTexture(const Value: Cardinal); override;
    procedure SetLastGLFreeTexture(const Value: integer); override;
    procedure SetTextureMinFilter(const Value: TGLint); override;
    procedure SetTextureMagFilter(const Value: TGLint); override;
    procedure SetPointSize(const Value: TGLFloat); override;
    procedure SetUseFog(const Value: boolean); override;

    procedure SetBlending(const Value: boolean); virtual;
    procedure SetBlendingSourceFactor(const Value: TGLenum); virtual;
    procedure SetBlendingDestinationFactor(const Value: TGLenum); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Equals(SecondValue: TPersistent): boolean; override;

    { jezeli true to elementy sceny z IsAllMaterialsTransparent beda
      rysowane uzywajac blending OpenGLa. Wpp. wszystko bedzie rysowane
      jako nieprzezroczyste. }
    property Blending: boolean
      read FBlending write SetBlending default true;

    { Blending function parameters, used when @link(Blending).
      See OpenGL documentation of glBlendFunc for possible values here.
      @groupBegin }
    property BlendingSourceFactor: TGLenum
      read FBlendingSourceFactor write SetBlendingSourceFactor
      default DefaultBlendingSourceFactor;
    property BlendingDestinationFactor: TGLenum
      read FBlendingDestinationFactor write SetBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;
    { @groupEnd }
  end;

  TTransparentGroup = (tgTransparent, tgOpaque, tgAll);
  TTransparentGroups = set of TTransparentGroup;

  { Various things that TVRMLFlatSceneGL.PrepareRender may prepare. }
  TPrepareRenderOption = (prBackground, prBoundingBox,
    prTrianglesListNotOverTriangulate,
    prTrianglesListOverTriangulate,
    prManifoldAndBorderEdges);
  TPrepareRenderOptions = set of TPrepareRenderOption;

  { This is a descendant of TVRMLFlatScene that makes it easy to render
    VRML scene into OpenGL. The point is that this class is the final,
    comfortable utility to deal with VRML files when you want to be able
    to render them using OpenGL.

    This class uses internal @link(TVRMLOpenGLRenderer) instance,
    thus hiding some "cumbersomness" (is it English?) of the interface of
    @link(TVRMLOpenGLRenderer) class. Also this class provides some
    functionality (like transparency using OpenGL blending)
    and some optimizations (like using OpenGL's display lists)
    that couldn't be achieved inside @link(TVRMLOpenGLRenderer) class
    (because they require looking at rendered VRML model as a whole,
    not only as a separate ShapeNode+State parts).
    See @link(Render) method for more details.

    Also this class can provide comfortable management for
    @link(TBackgroundGL) instance associated with this VRML model,
    that may be used to render VRML's background.
    See @link(Background) function.

    Connection with particular OpenGL context: from the 1st call
    of [Prepare]Render or Background methods to the next call of
    CloseGL method or the destructor. Everything between
    must be called within the *same* OpenGL context active.
    In particular: remember that if you called Render method
    at least once, you *must* destroy this object or at least call
    it's CloseGL method *before* releasing OpenGL context (that was
    active during Render). }
  TVRMLFlatSceneGL = class(TVRMLFLatScene)
  private
    FOptimization: TGLRendererOptimization;
    Renderer: TVRMLOpenGLRenderer;

    { This simply calls Renderer.Render(ShapeStates[ShapeStateNum].ShapeNode,
      ShapeStates[ShapeStateNum].State); }
    procedure RenderShapeStateSimple(ShapeStateNum: Integer);

    { This simply calls Renderer.Prepare(ShapeStates[ShapeStateNum].ShapeNode)
      and then RenderShapeStateSimple(ShapeStateNum).
      So this cannot be put inside display list. }
    procedure PrepareAndRenderShapeStateSimple(
      ShapeStateNum: Integer);

    procedure RenderBeginSimple;
    procedure RenderEndSimple;

    { wywoluje Renderer.RenderBegin, pozniej na wszystkich elementach
      ShapeStates[] wywoluje RenderShapeStateProc z ich indexem
      (tylko o ile TestShapeStateVisibility is not assigned or
      returns true for given node, and if TransparentGroup includes
      given shapestate), pozniej Renderer.RenderEnd.
      Dodatkowo moze robic jeszcze jakies rzeczy z OpenGLem, np. to tutaj
      realizujemy blending.

      De facto nie wywoluje Renderer.RenderBegin and Renderer.RenderEnd
      tylko RenderBeginProc i RenderEndProc. Te procedury *musza* wywolac
      odpowiednie Renderer.RenderBegin/End. Co najwyzej niekiedy
      moga to opakowac w display liste. Patrz
      RenderBeginSimple and RenderEndSimple.

      Jesli chcesz przekaz
      RenderBeginProc, RenderEndProc = nil, wtedy musisz sam sie upewnic
      ze je wywolasz naokolo RenderShapeStatesNoDisplayList
      (this is needed because roSceneAsAWhole needs to honour
      RenderBeginEndToDisplayList).

      This procedure never creates or uses any display list.
      You can freely put it's contents inside display list
      (assuming that RenderShapeStateProc, RenderBeginProc and RenderEndProc
      are something that can be part of display list).

      This sets FLastRender_RenderedShapeStatesCount and
      FLastRender_AllShapeStatesCount. }
    procedure RenderShapeStatesNoDisplayList(
      TestShapeStateVisibility: TTestShapeStateVisibility;
      RenderShapeStateProc: TRenderShapeState;
      RenderBeginProc, RenderEndProc: TObjectProcedure;
      TransparentGroup: TTransparentGroup);

    { niszcz powiazania z kontekstem OpenGLa obiektu Renderer i ew. wszelkich
      wygenerowanych na podstawie niego rzeczy (w tym momencie oznacza
      to SAAW_DisplayList i SSSX_DisplayLists).
      Nie niszcz powiazan Background.
      Ta procedura jest uzyteczna aby ja wywolac np. gdy zmieniamy
      Attrib (bo one (poza ColorModulatorami) wymagaja tylko odlaczenia
      kontekstu OpenGLa od Renderera, Background nie musi byc invalidated) }
    procedure CloseGLRenderer;

    FLastRender_RenderedShapeStatesCount: Cardinal;
    FLastRender_AllShapeStatesCount: Cardinal;

    FUsingProvidedRenderer: boolean;

    procedure CommonCreate(
      ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AOptimization: TGLRendererOptimization;
      AUsingProvidedRenderer: boolean;
      ARenderer: TVRMLOpenGLRenderer);

    { When using any optimization except roNone you can put
      Renderer.RenderBegin and Renderer.RenderEnd calls inside
      display lists too.

      However, stupid Mesa 6.4.2 bug prevents using EXT_fog_coord calls
        glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
        glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
      inside a display list (they cause OpenGL error GL_INVALID_ENUM).

      So before putting Renderer.RenderBegin or Renderer.RenderEnd calls
      inside display list always check this function. This checks
      whether we have Mesa. }
    function RenderBeginEndToDisplayList: boolean;

    { Private things only for RenderFrustum ---------------------- }

    RenderFrustum_Frustum: PFrustum;
    function RenderFrustum_TestShapeState(ShapeStateNum: Integer): boolean;

    { Private things only for RenderFrustumOctree ---------------------- }

    { This is private for RenderFrustumOctree, but it is created in
      constructor of this class, destroyed in destructor and
      resized in ChangedAll, for the sake of speed
      (since it would be costly to create such array each time
      you call RenderFrustumOctree). }
    RenderFrustumOctree_Visible: TDynBooleanArray;
    RenderFrustumOctree_Frustum: PFrustum;
    procedure RenderFrustumOctree_EnumerateOctreeItem(
      ShapeStateNum: Integer; CollidesForSure: boolean);
    function RenderFrustumOctree_TestShapeState(ShapeStateNum: Integer): boolean;

    { ------------------------------------------------------------
      Private things used only when Optimization = roSceneAsAWhole.
      Prefixed with SAAW, for clarity. }

    { This is always 0 when Optimization <> roSceneAsAWhole.
      When Optimization = roSceneAsAWhole, 0 means "not initialized" . }
    SAAW_DisplayList: array [TTransparentGroup] of TGLuint;

    { Przygotuje wszystko. Wywoluj tylko gdy
      Optimization = roSceneAsAWhole i
      SAAW_DisplayList[TransparentGroup] = 0.

      This calls RenderShapeStatesNoDisplayList so this sets
      FLastRender_RenderedShapeStatesCount and
      FLastRender_AllShapeStatesCount. }
    procedure SAAW_Prepare(TransparentGroup: TTransparentGroup);

    procedure SAAW_Render(TransparentGroup: TTransparentGroup);

    { ------------------------------------------------------------
      Private things used only when Optimization is
      roSeparateShapeStates or roSeparateShapeStatesNoTransform.
      Prefixed with SSSX, for clarity. }

    { <> nil if and only if Optimization is not
      roSeparateShapeStates or roSeparateShapeStatesNoTransform.
      Every item is 0 if it is not initialized. }
    SSSX_DisplayLists: TDynGLuintArray;

    SSSX_RenderBeginDisplayList: TGLuint;
    SSSX_RenderEndDisplayList: TGLuint;

    { These create appropriate SSSX_Render*DisplayList display list. }
    procedure SSSX_PrepareBegin;
    procedure SSSX_PrepareEnd;

    { These call appropriate SSSX_Render*DisplayList display list.
      If display list is not ready, they create it. }
    procedure SSSX_RenderBegin;
    procedure SSSX_RenderEnd;

    { ------------------------------------------------------------
      Private things used only when Optimization is
      roSeparateShapeStates. Prefixed with SSS, for clarity. }

    { Use this only when Optimization = roSeparateShapeStates.
      It can be passed as RenderShapeStateProc.

      This renders SSSX_DisplayLists.Items[ShapeStateNum]
      display list (creating it if necessary). }
    procedure SSS_RenderShapeState(ShapeStateNum: Integer);

    { Call this only when Optimization = roSeparateShapeStates and
      SSSX_DisplayLists.Items[ShapeStateNum] = 0.

      Prepares shapestate (by Renderer.Prepare(ShapeStates[ShapeStateNum].State).

      Then creates display list SSSX_DisplayLists.Items[ShapeStateNum]
      and initializes it with contents of RenderShapeStateSimple(ShapeStateNum).
      Mode GL_COMPILE is passed to glNewList, so it only creates
      given display list.

      This is somehow equivalent to SAAW_Prepare,
      but it operates only on a single ShapeState.

      Note that SSS_RenderShapeState simply calls
      SSS_PrepareShapeState if display list has to be created.
      Then it renders the list. }
    procedure SSS_PrepareShapeState(ShapeStateNum: Integer);

    { ------------------------------------------------------------
      Private things used only when Optimization is
      roSeparateShapeStatesNoTransform. Prefixed with SSSNT, for clarity. }

    procedure SSSNT_RenderShapeState(ShapeStateNum: Integer);
    procedure SSSNT_PrepareShapeState(ShapeStateNum: Integer);

    { shadow things ---------------------------------------------------------- }

    procedure RenderSilhouetteShadowVolume(
      const LightPos: TVector4Single;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      const LightCap, DarkCap: boolean);

    procedure RenderAllShadowVolume(
      const LightPos: TVector4Single;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      LightCap, DarkCap: boolean);
  public
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AOptimization: TGLRendererOptimization;
      ACache: TVRMLOpenGLRendererContextCache = nil);

    { This is a very special constructor, that forces this class to use
      provided AProvidedRenderer.

      Note that this renderer must be created with AttributesClass
      = TVRMLSceneRenderingAttributes.

      @italic(Don't use this unless you really know what you're doing!)
      In all normal circumstances you should use normal @link(Create)
      constructor, that will internally create and use internal renderer object.
      If you use this constructor you will have to understand how internally
      this class synchronizes itself with underlying Renderer object.

      Once again, if you're not sure, then simply don't use this
      constructor. It's for internal use --- namely it's internally used
      by TVRMLGLAnimation, this way all scenes of the animation share
      the same renderer which means that they also share the same
      information about textures and images loaded into OpenGL.
      And this is crucial for TVRMLGLAnimation, otherwise animation with
      100 scenes would load the same texture to OpenGL 100 times. }
    constructor CreateProvidedRenderer(
      ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AOptimization: TGLRendererOptimization;
      AProvidedRenderer: TVRMLOpenGLRenderer);

    destructor Destroy; override;

    { Destroy any associations of this object with current OpenGL context.
      For example, releaseany allocated texture or display list names.

      Generally speaking, destroys everything that is allocated by
      PrepareRender([...], []) call. It's harmless to call this
      method when there are already no associations with current OpenGL context.
      This is called automatically from the destructor. }
    procedure CloseGL;

    { This prepares some internal things in this class, making sure that
      appropriate methods execute as fast as possible.
      In most cases, it's not strictly required to call this method
      --- most things will be prepared "as needed" anyway.
      But this means that some calls may sometimes take a long time,
      e.g. the first Render call will take a long time because it may
      have to prepare display lists that will be reused in next Render calls.
      This may cause a strange behavior of the program: rendering of the
      first frame takes unusually long time (which confuses user, and
      also makes things like TGLWindow.FpsCompSpeed strange for a short
      time). So calling this procedure may be desirable.
      You may want to show to user that "now we're preparing
      the VRML scene --- please wait".

      This method ties this object to current OpenGL context.
      But it doesn't change any OpenGL state or buffers contents
      (at most, it allocates some texture and display list names).

      @param(TransparentGroups specifies for what TransparentGroup value
        it should prepare rendering resources (usually you only use
        [tgAll] or only one of [tgTransparent, tgOpaque] ---
        so it would be a waste of resources and time to prepare for every
        possible TransparentGroup value).)

      @param(Options says what additional features (besides rendering)
        should be prepared to execute fast. See TPrepareRenderOption,
        the names should be self-explanatory (they refer to appropriate
        methods of this class).) }
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions);

    { Render : probably the most important function in this class,
      often it is the reason why this class is used.
      Render renders this VRML scene.

      It uses internal @link(TVRMLOpenGLRenderer) instance.
      Although this internal object is not accessible to your code,
      you can get some detailed info about how rendering into OpenGL
      works by looking at comments in @link(VRMLOpenGLRenderer) unit.

      Each call to Render renders the scene,
      roughly executing the same OpenGL commands as would be done by calling
      following methods of @link(TVRMLOpenGLRenderer) instance:

      @unorderedList(
        @item RenderBegin
        @item(
@longcode(#
  for S := each item of ShapeStates list,
    if (TestShapeStateVisibility is not assigned) or
      (TestShapeStateVisibility returns true for given ShapeState) then
    call Render(S.ShapeNode, S.State)
#))
        @item RenderEnd
      )

      If Optimization = roSceneAsAWhole, TestShapeStateVisibility
      is ignored (because then rendering call almost always does not
      have such detailed control over which shapestates are actually
      rendered). So generally you should think of TestShapeStateVisibility
      as a way to optimize rendering, by quickly eliminating whole shapestates
      that you know are not visible (e.g. you know that their BoundingBox
      is outside current camera frustum).

      Don't try to put Render inside OpenGL's display-list,
      the point is that Render can internally create such display-list
      and manage it itself. So you don't have to worry about such things.
      W ten sposob
      kod z zewnatrz uzywajacy tej klasy moze zapomniec o zawilosciach
      interfejsu VRMLOpenGLRenderera (i skupic sie na zawilosciach interfejsu
      tej klasy, TVRMLFlatSceneGLa).

      Some additional notes (specific to TVRMLFlatSceneGL.Render,
      not to the VRMLOpenGLRenderer):
      @unorderedList(
        @item(
          glDepthMask, glEnable/Disable(GL_BLEND), glBlendFunc states are
          controlled in this function. This means that the state of this variables
          before calling this function does not have any influence on the effect
          produced by this function - and this means that this function
          does something like glPushAttrib + initialize those variables to some
          predetermined values + render everything + glPopAttrib.

          We use these OpenGL variables to implement transparency using OpenGL's
          blending. Some more notes about blending: what we do here is a standard
          OpenGL technique : first we render all opaque objects
          (if TransparentGroup is tgAll or tgOpaque) and then
          we make depth-buffer read-only and then we render all partially
          trasparent objects (if TransparentGroup is tgAll or tgTransparent).

          Note that while rendering just everything with tgAll is simple,
          but it has some important disadvantages if your OnDraw does
          not consist of only one call to Render. E.g. instead of simple
@longCode(
  Scene.Render(nil, tgAll);
)
          you have
@longCode(
  Scene1.Render(nil, tgAll);
  Scene2.Render(nil, tgAll);
)
          The code above it not good if both scenes contain some
          opaque and some transparent objects.
          You should always render all opaque objects before
          all transparent objects. E.g. Scene2 can't have any opaque objects
          if Scene1 has some of them.

          So that's when TransparentGroups come to use: you can write
@longCode(
  Scene1.Render(nil, tgOpaque);
  Scene2.Render(nil, tgOpaque);

  Scene1.Render(nil, tgTransparent);
  Scene2.Render(nil, tgTransparent);
)
          Note that when Attributes.Blending is @false then everything
          is always opaque, so tgOpaque renders everything and tgTransparent
          renders nothing.
        ))
    }
    procedure Render(TestShapeStateVisibility: TTestShapeStateVisibility;
      TransparentGroup: TTransparentGroup);

    { This calls Render passing TestShapeStateVisibility
      that tries to quickly eliminate ShapeStates that are entirely
      not within Frustum.
      In other words, this does so-called "frustum culling". }
    procedure RenderFrustum(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup);

    { This is like @link(RenderFrustum) but it tries to enumerate
      visible ShapeStates using given Octree (instead of just testing
      each ShapeState separately).

      This way it may work much faster when you have many ShapeStates.

      Note that if Optimization = roSceneAsAWhole this
      doesn't use Octree, but simply calls Render(nil).
      That's because when Optimization = roSceneAsAWhole
      Render always renders the whole scene,
      ignores TestShapeStateVisibility function,
      so it's useless (and would waste some time)
      to analyze the scene with Octree. }
    procedure RenderFrustumOctree(const Frustum: TFrustum;
      Octree: TVRMLShapeStateOctree;
      TransparentGroup: TTransparentGroup); overload;

    { This simply calls RenderFrustumOctree(Frustum, DefaultShapeStareOctree).
      Be sure that you assigned DefaultShapeStareOctree property before
      calling this. }
    procedure RenderFrustumOctree(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); overload;

    { LastRender_ properties provide you read-only statistics
      about what happened during last render. For now you
      can see how many ShapeStates were rendered (i.e. send to OpenGL
      pipeline) versus all ShapeStates that were available
      (this is simply copied from ShapeStates.Count).

      This way you can see how effective was frustum culling
      (for @link(RenderFrustum) or @link(RenderFrustumOctree))
      or how effective was your function TestShapeStateVisibility
      (if you used directly @link(Render)). "Effective" in the meaning
      "effective at eliminating invisible ShapeStates from rendering
      pipeline".

      These are initially equal to zeros.
      Then they are updated each time you called
      @link(RenderFrustumOctree) or @link(RenderFrustum) or
      @link(Render). }
    property LastRender_RenderedShapeStatesCount: Cardinal
      read FLastRender_RenderedShapeStatesCount;

    property LastRender_AllShapeStatesCount: Cardinal
      read FLastRender_AllShapeStatesCount;

    { metoda optymalizacji wyswietlania w OpenGLu danego obiektu.
      Po utworzeniu tego obiektu ta metoda juz bedzie zawsze ta sama.
      Jest to dosc wewnetrzna sprawa ale decyzja o tym jaka optymalizacja
      bedzie najlepsza musi byc podjeta z zewnatrz tej klasy; wszystko
      zalezy od tego jak chcesz takiej klasy uzywac - czy bedziesz czesto
      zmienial graf VRMLa czy moze graf VRMLa w RootNode bedzie juz staly przez
      caly czas zycia tego obiektu ? Czy bedziesz ogladal zawsze scene
      z daleka obejmujac na ekranie wszystkie elementy sceny czy tez
      bedziesz raczej wchodzil w srodek sceny, a wiec we Frustum widzenia
      zazwyczaj bedzie sie zawierala tylko czesc sceny ? }
    property Optimization: TGLRendererOptimization read FOptimization;

    procedure ChangedAll; override;
    procedure ChangedShapeStateFields(ShapeStateNum: integer); override;

    { Render shadow volume (sides and caps) of this scene, for shadow volume
      algorithm.

      There are two underlying algorithms here, and their speed
      difference is very noticeable:

      @orderedList(
        @item(Rendering with AllowSilhouetteOptimization.
          This is the usual, fast method of rendering shadow volumes.

          This renders shadow quads of silhouette edge. Edges from ManifoldEdges
          list are used to find silhouette edge. Additionally edges from
          BorderEdges always produce shadow quads, i.e. we treat them
          like they would always be silhouette edges.

          The very idea of this optimization is that most edges are in
          ManifoldEdges and so only real silhouette edges produce shadow quads.
          In other words, BorderEdges list should not contain too many items.
          When BorderEdges contains all edges (ManifoldEdges is empty), then
          this method degenerates to a naive rendering without silhouette
          optimization. So you should try to make your models as much as
          possible resembling nice 2-manifolds. Ideally, if your mesh
          is a number of perfectly closed manifolds, and vertex ordering
          is consistent, then BorderEdges is empty, and this works perfect.

          Usually, most models are mostly 2-manifold (only the real border
          edges are, well, in BorderEdges), and this works great.)

        @item(Without silhouette optimization.
          If you pass AllowSilhouetteOptimization = @false,
          you explicitly want to use the naive approach that just renders
          3 shadow quads for each triangle. This is much slower,
          and is not adviced... read below for exceptions.

          The only good reason to use this is that silhouette optimization
          for models that are not perfect 2-manifold (i.e., have some
          BorderEdges) may show some artifacts. See
          "VRML engine documentation" on
          [http://vrmlengine.sourceforge.net/vrml_engine_doc.php],
          chapter "Shadows", for description and pictures of these artifacts.
          They are quite unavoidable in any shadow volumes implementation,
          just like normal ghost shadows.

          While you can avoid these artifacts by turning off
          AllowSilhouetteOptimization, still it's usually
          much better to fix your 3D model to be correct 2-manifold.))

      All shadow quads are generated from scene triangles transformed
      by Transform. This must be able to correctly detect front and
      back facing triangles with respect to LightPos, so "LightPos" and
      "scene transformed by Transform" must be in the same coordinate system.
      (That's why explicit Transform parameter is needed, you can't get away
      with simply doing glPush/PopMatrix and glMustMatrix around RenderShadowVolume
      call.) If TransformIsIdentity then Transform value is ignored and
      everything works like Transform = identity matrix (and is a little
      faster in this special case).

      This uses TrianglesList(false) and ManifoldEdges and BorderEdges
      (so you may prefer to prepare it before, e.g. by calling PrepareRender with
      prShadowVolume included).

      LightPos is the light position. LightPos[3] must be 1
      (to indicate positional light) or 0 (a directional light).

      LightCap and DarkCap say whether you want to cap your shadow volume.
      LightCap is the cap at the caster position, DarkCap is the cap in infinity.
      This is needed by z-fail method, you should set them both to @true.
      To be more optimal, you can request LightCap only if z-fail @italic(and
      the caster is inside camera frustum). For directional lights, DarkCap is
      ignored, since the volume is always closed by a single point in infinity.

      For ShadowVolumesHelper version, LightPos, LightCap and DarkCap
      are already available in ShadowVolumesHelper properties (set by
      ShadowVolumesHelper.InitFrustumAndLight and ShadowVolumesHelper.InitScene
      calls).

      Faces (both shadow quads and caps) are rendered such that
      CCW <=> you're looking at it from outside
      (i.e. it's considered front face of this shadow volume).

      All the commands passed to OpenGL by this methods are:
      glBegin, sequence of glVertex, then glEnd. }
    procedure RenderShadowVolume(
      const LightPos: TVector4Single;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      const LightCap: boolean;
      const DarkCap: boolean;
      const AllowSilhouetteOptimization: boolean = true);

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      const AllowSilhouetteOptimization: boolean = true);

    { A shortcut for ShadowVolumesHelper.InitScene and then RenderShadowVolume.
      It will calculate current bounding box using Transform, TransformIsIdentity
      and BoundingBox method. }
    procedure InitAndRenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      const AllowSilhouetteOptimization: boolean = true);
  private
    FBackgroundSkySphereRadius: Single;
    { Cached Background value }
    FBackground: TBackgroundGL;
    { Is FBackground valid ? We can't use "nil" FBackground value to flag this
      (bacause nil is valid value for Background function).
      If not FBackgroundValid then FBackground must always be nil.
      Never set FBackgroundValid to false directly - use FBackgroundInvalidate,
      this will automatically call FreeAndNil(FBackground) before setting
      FBackgroundValid to false. }
    FBackgroundValid: boolean;
    procedure FBackgroundInvalidate;
    procedure SetBackgroundSkySphereRadius(const Value: Single);
  public
    property BackgroundSkySphereRadius: Single
      read FBackgroundSkySphereRadius write SetBackgroundSkySphereRadius; { = 1 }

    procedure PrepareBackground;

    { Returns TBackgroundGL instance for this scene. Background's properties
      are based on the attributes of first "Background" VRML node in the
      RootNode scene (and on his place in scene transformations).
      They are also based on current value of BackgroundSkySphereRadius.
      And on the values of Attributes.ColorModulatorSingle/Byte.
      If there is no "Background" node in VRML scene this function returns nil.

      Note: this Background object is managed (automatically created/freed
      etc.) by this TVRMLFlatSceneGL object but it is NOT used anywhere
      in this class, e.g. Render does not call Background.Render. If you want to
      use this Background somehow, you have to do this yourself.

      The results of this function are internally cached. Cache is invalidated
      on such situations as change in RootNode scene, changes to
      BackgroundSkySphereRadius, CloseGL, Attributes.ColorModulatorSingle/Byte.

      PrepareBackground (and PrepareRender(true, ...)) automatically validate this
      cache.

      Remember that this cache is connected with the current OpenGL context.
      So you HAVE to call CloseGL to disconnent this object from
      current OpenGL context after you used this function. }
    function Background: TBackgroundGL;

    { Atrybuty renderowania. Wszystkie atrybuty renderowania mozesz
      zmieniac w dowolnym momencie. Nawet atrybuty zdefiniowane
      i wykonywane przez TVRMLOpenGLRenderera; mimo ze TVRMLOpenGLRenderer
      mial bardziej restrykcyjna polityke kiedy mozna bylo modyfikowac
      Attributes.

      Chociaz powinienes zdawac sobie sprawe z faktu ze zmiana wartosci
      atrybutu moze spowodowac ze nastepne
      wywolanie Render bedzie musialo sobie cos przeliczac od nowa
      (wywolanie PrepareRender moze miec sens w takiej sytuacji).

      Note for ColorModulatorSingle/Byte properties:
      In addition to effects described at TVRMLOpenGLRenderer,
      they also affect what the TVRMLFlatSceneGL.Background function returns. }
    function Attributes: TVRMLSceneRenderingAttributes;

    { Creates a headlight, using (if present) KambiHeadLight node defined
      in this VRML file. You're responsible for freeing this node.

      Note that this is @italic(not) concerned whether you
      actually should use this headlight (this information usually comes from
      NavigationInfo.headlight value). }
    function CreateHeadLight: TVRMLGLHeadLight;

    { @abstract(Which bump mapping method will be used ?)

      This is decided and controlled internally, based on current OpenGL
      capabilities (the only things you can control
      are Attributes.EnableTextures and Attributes.BumpMapping properties).
      So the only use of this function is when you want to report this
      to user, or for debug purposes etc.

      Note that calling this ties us to current OpenGL context.

      @seealso TVRMLOpenGLRenderer.BumpMappingMethod }
    function BumpMappingMethod: TBumpMappingMethod;
  end;

  TObjectsListItem_1 = TVRMLFlatSceneGL;
  {$I objectslist_1.inc}
  TVRMLFlatSceneGLsList = class(TObjectsList_1)
  private
    { Just call FBackgroundInvalidate or CloseGLRenderer on all items.
      These methods are private, because corresponding methods in
      TVRMLFlatSceneGL are also private and we don't want to expose
      them here. }
    procedure FBackgroundInvalidate;
    procedure CloseGLRenderer;
  public
    { Just call CloseGL on all items. }
    procedure CloseGL;
  end;

const
  { Options to pass to TVRMLFlatSceneGL.PrepareRender to make
    sure that next call to TVRMLFlatSceneGL.RenderShadowVolume
    is as fast as possible.

    For now this actually could be equal to prManifoldEdges
    (prTrianglesListNotOverTriangulate has to be prepared while preparing
    ManifoldEdges edges anyway). But for the future shadow volumes
    optimizations, it's best to use this constant. }
  prShadowVolume = [prTrianglesListNotOverTriangulate, prManifoldAndBorderEdges];

type
  TDynArrayItem_1 = TTriangle4Single;
  PDynArrayItem_1 = PTriangle4Single;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Triangle4Single = TInfiniteArray_1;
  PArray_Triangle4Single = PInfiniteArray_1;
  TDynTriangle4SingleArray = TDynArray_1;

{$undef read_interface}

implementation

uses VRMLErrors, GLVersionUnit, GLImages;

{$define read_implementation}
{$I objectslist_1.inc}
{$I dynarray_1.inc}

{ ------------------------------------------------------------ }

{ Notes about GL_COMPILE_AND_EXECUTE mode for glNewList:

  Hell and damnation. In some places of my code I used
  glNewList with an assumption that it's always better
  to create and execute display list using glNewList(GL_COMPILE_AND_EXECUTE)
  than creating display list using glNewList(GL_COMPILE) and then execute it
  using glCallList.

  I mean, at worst, OpenGL implementation may
  implement glNewList(GL_COMPILE_AND_EXECUTE) as
  something like simple glNewList(GL_COMPILE) + glCallList.
  ( (*) Actually this is not so easily possible since you may call
  between glNewList and glEndList some commands that aren't
  placed inside display list, but must take immediate effect
  and must affect interpretation of subsequent commands
  passed to display list (like e.g. packing of texture images
  in memory))
  But it's also possible
  that smart OpenGL implementation will be actually able to compile
  and execute the list at the same time, so the call
  glNewList(GL_COMPILE_AND_EXECUTE) would be faster.

  All that time one assumption was obvious to me:
  display lists created by glNewList(GL_COMPILE) are optimized
  the same way as display lists created by
  glNewList(GL_COMPILE_AND_EXECUTE). I mean, OpenGL implementation
  does not sacrifice quality of display list to make single call to
  glNewList(GL_COMPILE_AND_EXECUTE) execute faster.

  Unfortunately I found by experiment that this is not the case
  on my NVidia GeForce 2.
  I wasn't able to find any official confirmations on www that things
  may work like that, only some comments on some game-programming
  forums and statement that confirms that this is the case with
  HP implementation of OpenGL (or at least some version of it)
  [http://www.talisman.org/opengl-1.1/ImpGuide/05_WriteProg.html#GLCOMPILEandEXECUTEMode].
  Also OpenGL FAQ [http://www.opengl.org/resources/faq/technical/displaylist.htm]
  says "stay away from GL_COMPILE_AND_EXECUTE mode".
  So I guess it's official. Later I removed from the code possibility
  to use GL_COMPILE_AND_EXECUTE at all, since it was useless
  and i wanted to make the code a little more manaegable.

  At first I wanted to implement KamGLNewList and KamGLEndList:

    KamGLNewList and KamGLEndList work like glNewList and glEndList
    but they keep one additional assumption: display lists created
    with glNewList(GL_COMPILE_AND_EXECUTE) have the same quality
    as those created by glNewList(GL_COMPILE).

    On some OpenGL implementations (some versions, by some vendors...)
    KamGLNewList and KamGLEndList may actually just call glNewList and glEndList.
    On others glNewList(List, GL_COMPILE_AND_EXECUTE) + ... + glEndList()
    may be actually realized as
    glNewList(GL_COMPILE) + ... + glEndList + glCallList(List).

  But this is not so easy to do cleanly, because of
  - problem marked with "(*)" mentioned above makes it impossible
    to implement real drop-in for replacement glNew/EndList.
  - have to add additional param to KamGLEndList.
}

{ TVRMLFlatSceneGL ------------------------------------------------------------ }

procedure TVRMLFlatSceneGL.CommonCreate(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization;
  AUsingProvidedRenderer: boolean;
  ARenderer: TVRMLOpenGLRenderer);
begin
  { inherited Create calls ChangedAll that is overriden in this class
    and uses SSSX_DisplayLists,
    RenderFrustumOctree_Visible, Optimization.
    That's why I have to init them *before* "inherited Create" }

  FOptimization := AOptimization;

  case Optimization of
    roSeparateShapeStates, roSeparateShapeStatesNoTransform:
      SSSX_DisplayLists := TDynGLuintArray.Create;
  end;

  RenderFrustumOctree_Visible := TDynBooleanArray.Create;

  inherited Create(ARootNode, AOwnsRootNode);

  FBackgroundSkySphereRadius := 1.0;
  FBackgroundValid := false;
  FBackground := nil;

  FUsingProvidedRenderer := AUsingProvidedRenderer;

  Renderer := ARenderer;
  Assert(Renderer.Attributes is TVRMLSceneRenderingAttributes);

  { Note that this calls Renderer.Attributes, so use this after
    initializing Renderer. }
  Attributes.FScenes.Add(Self);
end;

constructor TVRMLFlatSceneGL.Create(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization;
  ACache: TVRMLOpenGLRendererContextCache);
begin
  CommonCreate(ARootNode, AOwnsRootNode, AOptimization, false,
    TVRMLOpenGLRenderer.Create(TVRMLSceneRenderingAttributes, ACache));
end;

constructor TVRMLFlatSceneGL.CreateProvidedRenderer(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization;
  AProvidedRenderer: TVRMLOpenGLRenderer);
begin
  CommonCreate(ARootNode, AOwnsRootNode, AOptimization, true, AProvidedRenderer);
end;

destructor TVRMLFlatSceneGL.Destroy;
begin
  CloseGL;

  { Note that this calls Renderer.Attributes, so use this before
    deinitializing Renderer. }
  if Renderer <> nil then
    Attributes.FScenes.Delete(Self);

  if not FUsingProvidedRenderer then
  begin
    { We must release all connections between RootNode and Renderer first.
      Reason: when freeing RootNode, image references (from texture nodes)
      are decremented. So cache used when loading these images must be
      available.

      If we used provided renderer, then this is not
      our problem: if OwnsRootNode then RootNode will be freed soon
      by "inherited", if not OwnsRootNode then it's the using programmer
      responsibility to free both RootNode and ProvidedRenderer
      in exactly this order.

      If we used our own renderer (actually, this is needed only if we used
      own own cache, so caller didn't provide a renderer and didn't provide
      a cache (ACache = nil for constructor), but we don't store this information
      for now) : we must make sure that freeing RootNode is safe.

      If OwnsRootNode then we know that inherited will free RootNode
      and so the simpler solution, to just FreeAndNil(Renderer) after
      inherited, would be possible. But it's not possible, since
      OwnsRootNode may be false and then programmer may want to free
      RootNode at undefined later time.

      So we have to guarantee, *now*, that freeing RootNode is safe ---
      no dangling references to Renderer.Cache. }
    FreeResources([frTextureImageInNodes, frBackgroundImageInNodes]);

    FreeAndNil(Renderer);
  end;

  FreeAndNil(SSSX_DisplayLists);
  FreeAndNil(RenderFrustumOctree_Visible);

  inherited;
end;

procedure TVRMLFlatSceneGL.CloseGLRenderer;
{ uwazaj - ta funkcja jest wywolywana z ChangedAll, w rezultacie moze
  byc wywolana zanim jeszcze nasz konstruktor w tej klasie zakonczy dzialanie.
  Ponadto jest tez wywolywana w destruktorze a wiec jezeli wyjdziemy z
  konstruktora wyjatkiem - to tez trafimy tutaj z obiektem ktory nie jest
  w pelni skonstruowany.
  W tym momencie sprowadza sie to do tego ze trzeba sprawdzac czy
  Renderer <> nil. }
var
  ShapeStateNum: Integer;
  TG: TTransparentGroup;
begin
  case Optimization of
    roSceneAsAWhole:
      for TG := Low(TG) to High(TG) do
        glFreeDisplayList(SAAW_DisplayList[TG]);
    roSeparateShapeStates, roSeparateShapeStatesNoTransform:
      begin
        { Because CloseGLRenderer may be called after scene has changed
          and after "inherited ChangedAll" changed ShapeStates.Count to
          reflect this change but before our ChangedAll changed
          SSSX_DisplayLists.Length (after all, CloseGLRenderer must be
          called before changing SSSX_DisplayLists.Length, since CloseGLRenderer
          must finalize what was left) ... so, I can't assume here that
          ShapeStates.Count = SSSX_DisplayLists.Count (like I do in many
          other places in this unit). So below I must iterate to
          "SSSX_DisplayLists.Count - 1", *not* to "ShapeStates.Count - 1". }
        if Renderer <> nil then
        begin
          for ShapeStateNum := 0 to SSSX_DisplayLists.Count - 1 do
            if SSSX_DisplayLists.Items[ShapeStateNum] <> 0 then
            begin

              if Optimization = roSeparateShapeStates then
                Renderer.Cache.ShapeState_DecReference(
                  SSSX_DisplayLists.Items[ShapeStateNum]) else
                Renderer.Cache.ShapeStateNoTransform_DecReference(
                  SSSX_DisplayLists.Items[ShapeStateNum]);

              SSSX_DisplayLists.Items[ShapeStateNum] := 0;
            end;

          if SSSX_RenderBeginDisplayList <> 0 then
          begin
            Renderer.Cache.RenderBegin_DecReference(SSSX_RenderBeginDisplayList);
            SSSX_RenderBeginDisplayList := 0;
          end;

          if SSSX_RenderEndDisplayList <> 0 then
          begin
            Renderer.Cache.RenderEnd_DecReference(SSSX_RenderEndDisplayList);
            SSSX_RenderEndDisplayList := 0;
          end;
        end;
      end;
  end;

  { TODO: if FUsingProvidedRenderer then we should do something more detailed
    then just Renderer.UnprepareAll. It's not needed for TVRMLGLAnimation
    right now, so it's not implemented. }
  if Renderer <> nil then Renderer.UnprepareAll;
end;

procedure TVRMLFlatSceneGL.CloseGL;
begin
  CloseGLRenderer;
  FBackgroundInvalidate;
end;

procedure TVRMLFlatSceneGL.PrepareAndRenderShapeStateSimple(
  ShapeStateNum: Integer);
begin
  Renderer.Prepare(ShapeStates[ShapeStateNum].State);
  RenderShapeStateSimple(ShapeStateNum);
end;

procedure TVRMLFlatSceneGL.RenderShapeStateSimple(ShapeStateNum: Integer);
begin
  Renderer.RenderShapeState(ShapeStates[ShapeStateNum].ShapeNode,
    ShapeStates[ShapeStateNum].State);
end;

procedure TVRMLFlatSceneGL.RenderBeginSimple;
begin
 Renderer.RenderBegin(FogNode, FogDistanceScaling);
end;

procedure TVRMLFlatSceneGL.RenderEndSimple;
begin
 Renderer.RenderEnd;
end;

function TVRMLFlatSceneGL.RenderBeginEndToDisplayList: boolean;
begin
  Result := not GLVersion.IsMesa;

  { TODO: this should check for Mesa version, and only activate when
    Mesa version <= something. I have to check Mesa CVS version
    and eventually report this as Mesa bug, if not fixed yet.
    Right now checked with:
    - 6.4.2: confirmed that the problem occurs and is solved by
      RenderBeginEndToDisplayList set to false
    - 6.5.1, 6.5.2: like above
  }
end;

procedure TVRMLFlatSceneGL.RenderShapeStatesNoDisplayList(
  TestShapeStateVisibility: TTestShapeStateVisibility;
  RenderShapeStateProc: TRenderShapeState;
  RenderBeginProc, RenderEndProc: TObjectProcedure;
  TransparentGroup: TTransparentGroup);

const
  AllOrOpaque = [tgAll, tgOpaque];
  AllOrTransparent = [tgAll, tgTransparent];

  procedure TestRenderShapeStateProc(ShapeStateNum: Integer);
  begin
    if (not Assigned(TestShapeStateVisibility)) or
       TestShapeStateVisibility(ShapeStateNum) then
    begin
      Inc(FLastRender_RenderedShapeStatesCount);
      RenderShapeStateProc(ShapeStateNum);
    end;
  end;

var
  ShapeStateNum: integer;
  TransparentObjectsExists: boolean;
begin
  FLastRender_RenderedShapeStatesCount := 0;
  FLastRender_AllShapeStatesCount := ShapeStates.Count;

  if Assigned(RenderBeginProc) then
    RenderBeginProc;
  try
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    try
      { TODO - niestety, jezeli jeden shape uzywa wielu materialow z ktorych
        niektore sa opaque a niektore nie to wynik renderowania nie bedzie
        za dobry. Idealnym rozwiazaniem byloby rozbijac tu renderowanie
        na trojkaty - ale ponizej, dla szybkosci, renderujemy cale
        shape'y. W zwiazku z tym zdecydowalem ze jezeli shape uzywa jakiegos
        materialu i nie wszystkie elementy tego materialu sa transparent
        to renderujemy shape jako opaque - wpp. renderujemy shape jako
        transparent. W rezultacie jezeli chcesz zeby shape byl renderowany
        z uwzglednieniem partial transparency to musisz zrobic tak zeby
        shape uzywal materialu ktorego pole Transparency ma wszystkie
        elementy > 0.

        TODO - powinnismy rysowac obiekty partially transparent od najdalszego
        do najblizszego zeby zapewnic dobry wyglad w miejscach gdzie kilka
        partially transparent obiektow zachodzi na siebie.

        Note for AllMaterialsTransparent: it's important here that we use
        AllMaterialsTransparent from TVRMLShapeState, because this is cached
        in TVRMLShapeState. If we would directly call
        State.LastNodes.Material.IsAllMaterialsTransparent then
        our trick with freeing RootNode to conserve memory
        (see TVRMLFlatScene.RootNode docs) would make calling Render
        impossible with optimization roSeparateShapeStates.
      }

      glDepthMask(GL_TRUE);
      glDisable(GL_BLEND);
      if Attributes.Blending then
      begin
        { uzywamy zmiennej TransparentObjectsExists aby ew. (jesli na scenie
          nie ma zadnych obiektow ktore chcemy renderowac z blending)
          zaoszczedzic czas i nie robic zmian stanu OpenGLa glDepthMask(GL_FALSE);
          itd. i nie iterowac ponownie po liscie ShapeStates }
        TransparentObjectsExists := false;

        { draw fully opaque objects }
        for ShapeStateNum := 0 to ShapeStates.Count - 1 do
          if not ShapeStates[ShapeStateNum].AllMaterialsTransparent then
          begin
            if TransparentGroup in AllOrOpaque then
              TestRenderShapeStateProc(ShapeStateNum);
          end else
            TransparentObjectsExists := true;

        { draw partially transparent objects }
        if TransparentObjectsExists and
           (TransparentGroup in AllOrTransparent) then
        begin
          glDepthMask(GL_FALSE);
          glEnable(GL_BLEND);
          glBlendFunc(Attributes.BlendingSourceFactor,
                      Attributes.BlendingDestinationFactor);
          for ShapeStateNum := 0 to ShapeStates.Count - 1 do
            if ShapeStates[ShapeStateNum].AllMaterialsTransparent then
              TestRenderShapeStateProc(ShapeStateNum);
        end;
      end else
      if TransparentGroup in AllOrOpaque then
      begin
        for ShapeStateNum := 0 to ShapeStates.Count - 1 do
          TestRenderShapeStateProc(ShapeStateNum);
      end;

    finally glPopAttrib end;
  finally
    if Assigned(RenderEndProc) then
      RenderEndProc;
  end;
end;

procedure TVRMLFlatSceneGL.SSSX_PrepareBegin;
var
  AttributesCopy: TVRMLSceneRenderingAttributes;
begin
  if not RenderBeginEndToDisplayList then
  begin
    { Although SSSX_PrepareBegin shouldn't call any actual OpenGL commands
      outside of display list, (not RenderBeginEndToDisplayList) forces
      us to call RenderBeginSimple here. See comments inside analogous
      SAAW_Prepare situation. }
    RenderBeginSimple;
    Exit;
  end;

  if not Renderer.Cache.RenderBegin_IncReference_Existing(
    Attributes,
    FogNode, FogDistanceScaling,
    SSSX_RenderBeginDisplayList) then
  begin
    SSSX_RenderBeginDisplayList := glGenListsCheck(1,
      'TVRMLFlatSceneGL.SSSX_PrepareBegin');
    glNewList(SSSX_RenderBeginDisplayList, GL_COMPILE);
    try
      RenderBeginSimple;
    finally glEndList end;

    AttributesCopy := TVRMLSceneRenderingAttributes.Create;
    AttributesCopy.Assign(Attributes);
    Renderer.Cache.RenderBegin_IncReference_New(
      AttributesCopy,
      FogNode, FogDistanceScaling,
      SSSX_RenderBeginDisplayList);
  end;
end;

procedure TVRMLFlatSceneGL.SSSX_PrepareEnd;
var
  AttributesCopy: TVRMLSceneRenderingAttributes;
begin
  if not RenderBeginEndToDisplayList then
  begin
    RenderEndSimple;
    Exit;
  end;

  if not Renderer.Cache.RenderEnd_IncReference_Existing(
    Attributes,
    FogNode, FogDistanceScaling,
    SSSX_RenderEndDisplayList) then
  begin
    SSSX_RenderEndDisplayList := glGenListsCheck(1,
      'TVRMLFlatSceneGL.SSSX_PrepareEnd');
    glNewList(SSSX_RenderEndDisplayList, GL_COMPILE);
    try
      RenderEndSimple;
    finally glEndList end;

    AttributesCopy := TVRMLSceneRenderingAttributes.Create;
    AttributesCopy.Assign(Attributes);
    Renderer.Cache.RenderEnd_IncReference_New(
      AttributesCopy,
      FogNode, FogDistanceScaling,
      SSSX_RenderEndDisplayList);
  end;
end;

procedure TVRMLFlatSceneGL.SSSX_RenderBegin;
begin
  if SSSX_RenderBeginDisplayList = 0 then
    SSSX_PrepareBegin;

  if RenderBeginEndToDisplayList then
    glCallList(SSSX_RenderBeginDisplayList) else
    RenderBeginSimple;
end;

procedure TVRMLFlatSceneGL.SSSX_RenderEnd;
begin
  if SSSX_RenderEndDisplayList = 0 then
    SSSX_PrepareEnd;

  if RenderBeginEndToDisplayList then
    glCallList(SSSX_RenderEndDisplayList) else
    RenderEndSimple;
end;

procedure TVRMLFlatSceneGL.SSS_PrepareShapeState(
  ShapeStateNum: Integer);
var
  AttributesCopy: TVRMLSceneRenderingAttributes;
  StateCopy: TVRMLGraphTraverseState;
begin
  Renderer.Prepare(ShapeStates[ShapeStateNum].State);

  if not Renderer.Cache.ShapeState_IncReference_Existing(
    Attributes,
    ShapeStates[ShapeStateNum].ShapeNode,
    ShapeStates[ShapeStateNum].State,
    FogNode, FogDistanceScaling,
    SSSX_DisplayLists.Items[ShapeStateNum]) then
  begin
    SSSX_DisplayLists.Items[ShapeStateNum] := glGenListsCheck(1,
      'TVRMLFlatSceneGL.SSS_PrepareShapeState');
    glNewList(SSSX_DisplayLists.Items[ShapeStateNum], GL_COMPILE);
    try
      RenderShapeStateSimple(ShapeStateNum);
      glEndList;
    except
      glEndList;
      { In case of trouble, make sure that
        SSSX_DisplayLists.Items[ShapeStateNum]
        resources are released and it's set to 0.
        Otherwise we would try to do ShapeState_DecReference later,
        but ShapeState_IncReference_New was not called yet
        and ShapeState_DecReference would fail. }
      glFreeDisplayList(SSSX_DisplayLists.Items[ShapeStateNum]);
      raise;
    end;

    AttributesCopy := TVRMLSceneRenderingAttributes.Create;
    AttributesCopy.Assign(Attributes);
    StateCopy := TVRMLGraphTraverseState.CreateCopy(
      ShapeStates[ShapeStateNum].State);
    Renderer.Cache.ShapeState_IncReference_New(
      AttributesCopy,
      ShapeStates[ShapeStateNum].ShapeNode,
      StateCopy,
      FogNode, FogDistanceScaling,
      SSSX_DisplayLists.Items[ShapeStateNum]);
  end;
end;

procedure TVRMLFlatSceneGL.SSS_RenderShapeState(
  ShapeStateNum: Integer);
begin
  if SSSX_DisplayLists.Items[ShapeStateNum] = 0 then
    SSS_PrepareShapeState(ShapeStateNum);
  glCallList(SSSX_DisplayLists.Items[ShapeStateNum]);
end;

procedure TVRMLFlatSceneGL.SSSNT_PrepareShapeState(
  ShapeStateNum: Integer);
var
  AttributesCopy: TVRMLSceneRenderingAttributes;
  StateCopy: TVRMLGraphTraverseState;
begin
  Renderer.Prepare(ShapeStates[ShapeStateNum].State);

  if not Renderer.Cache.ShapeStateNoTransform_IncReference_Existing(
    Attributes,
    ShapeStates[ShapeStateNum].ShapeNode,
    ShapeStates[ShapeStateNum].State,
    FogNode, FogDistanceScaling,
    SSSX_DisplayLists.Items[ShapeStateNum]) then
  begin
    SSSX_DisplayLists.Items[ShapeStateNum] := glGenListsCheck(1,
      'TVRMLFlatSceneGL.SSSNT_PrepareShapeState');
    glNewList(SSSX_DisplayLists.Items[ShapeStateNum], GL_COMPILE);
    try
      Renderer.RenderShapeStateNoTransform(
        ShapeStates[ShapeStateNum].ShapeNode,
        ShapeStates[ShapeStateNum].State);
      glEndList;
    except
      glEndList;
      { In case of trouble, make sure that
        SSSX_DisplayLists.Items[ShapeStateNum]
        resources are released and it's set to 0.
        Otherwise we would try to do ShapeState_DecReference later,
        but ShapeState_IncReference_New was not called yet
        and ShapeState_DecReference would fail. }
      glFreeDisplayList(SSSX_DisplayLists.Items[ShapeStateNum]);
      raise;
    end;

    AttributesCopy := TVRMLSceneRenderingAttributes.Create;
    AttributesCopy.Assign(Attributes);
    StateCopy := TVRMLGraphTraverseState.CreateCopy(
      ShapeStates[ShapeStateNum].State);
    Renderer.Cache.ShapeStateNoTransform_IncReference_New(
      AttributesCopy,
      ShapeStates[ShapeStateNum].ShapeNode,
      StateCopy,
      FogNode, FogDistanceScaling,
      SSSX_DisplayLists.Items[ShapeStateNum]);
  end;
end;

procedure TVRMLFlatSceneGL.SSSNT_RenderShapeState(
  ShapeStateNum: Integer);
begin
  if SSSX_DisplayLists.Items[ShapeStateNum] = 0 then
    SSSNT_PrepareShapeState(ShapeStateNum);

  Renderer.RenderShapeStateBegin(
    ShapeStates[ShapeStateNum].ShapeNode,
    ShapeStates[ShapeStateNum].State);
  try
    glCallList(SSSX_DisplayLists.Items[ShapeStateNum]);
  finally
    Renderer.RenderShapeStateEnd(
      ShapeStates[ShapeStateNum].ShapeNode,
      ShapeStates[ShapeStateNum].State);
  end;
end;

procedure TVRMLFlatSceneGL.SAAW_Prepare(TransparentGroup: TTransparentGroup);
var i: Integer;
begin
  { First prepare all (because I can't later call Renderer.Prepare
    while being inside display-list) }
  for i := 0 to ShapeStates.Count - 1 do
    Renderer.Prepare(ShapeStates[i].State);

  SAAW_DisplayList[TransparentGroup] := glGenListsCheck(1,
    'TVRMLFlatSceneGL.SAAW_Prepare');
  if RenderBeginEndToDisplayList then
  begin
    glNewList(SAAW_DisplayList[TransparentGroup], GL_COMPILE);
    try
      RenderShapeStatesNoDisplayList(nil,
        {$ifdef FPC_OBJFPC} @ {$endif} RenderShapeStateSimple,
        {$ifdef FPC_OBJFPC} @ {$endif} RenderBeginSimple,
        {$ifdef FPC_OBJFPC} @ {$endif} RenderEndSimple,
        TransparentGroup);
    finally glEndList end;
  end else
  begin
    { Although this is SAAW_Prepare, and we shouldn't call here
      any OpenGL command outside of display list, we have to call
      RenderBegin/EndSimple outside of display list:
      - (not RenderBeginEndToDisplayList) doesn't allow us to call
        this inside display list,
      - and TVRMLOpenGLRenderer requires
        that RenderBegin/End must be called around particular shape+state
        rendering (e.g. because RenderBegin sets up private variables for
        volumetric fog).
      Fortunately RenderBegin + RenderEnd do a full push/pop attributes
      and matrices, so this shouldn't be a problem.
    }

    RenderBeginSimple;
    try
      glNewList(SAAW_DisplayList[TransparentGroup], GL_COMPILE);
      try
        RenderShapeStatesNoDisplayList(nil,
          {$ifdef FPC_OBJFPC} @ {$endif} RenderShapeStateSimple, nil, nil,
          TransparentGroup);
      finally glEndList end;
    finally RenderEndSimple end;
  end;
end;

procedure TVRMLFlatSceneGL.SAAW_Render(TransparentGroup: TTransparentGroup);
begin
  if SAAW_DisplayList[TransparentGroup] = 0 then
    SAAW_Prepare(TransparentGroup) else
  begin
    { In this case I must directly set here LastRender_Xxx variables.
      TODO: this is wrong when TransparentGroup <> tgAll, then something
      < ShapeStates.Count should be used. }
    FLastRender_AllShapeStatesCount := ShapeStates.Count;
    FLastRender_RenderedShapeStatesCount := FLastRender_AllShapeStatesCount;
  end;

  if RenderBeginEndToDisplayList then
    glCallList(SAAW_DisplayList[TransparentGroup]) else
  begin
    RenderBeginSimple;
    try
      glCallList(SAAW_DisplayList[TransparentGroup]);
    finally RenderEndSimple end;
  end;
end;

procedure TVRMLFlatSceneGL.PrepareRender(
  TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions);
var
  ShapeStateNum: Integer;
  TG: TTransparentGroup;
begin
  case Optimization of
    roSceneAsAWhole:
      for TG := Low(TG) to High(TG) do
        if (TG in TransparentGroups) and (SAAW_DisplayList[TG] = 0) then
          SAAW_Prepare(TG);
    roSeparateShapeStates, roSeparateShapeStatesNoTransform:
      begin
        { build display lists (if needed) for begin/end and all shape states }
        if SSSX_RenderBeginDisplayList = 0 then
          SSSX_PrepareBegin;
        try
          for ShapeStateNum := 0 to ShapeStates.Count - 1 do
          begin
            if SSSX_DisplayLists.Items[ShapeStateNum] = 0 then
            begin
              if Optimization = roSeparateShapeStates then
                SSS_PrepareShapeState(ShapeStateNum) else
                SSSNT_PrepareShapeState(ShapeStateNum);
            end;

            { Calculate AllMeterialTransparent and make it cached in
              ShapeStatesList[ShapeStateNum] instance. This is needed
              for our trick with freeing RootNode, see
              TVRMLFlatSceneGL.RenderShapeStatesNoDisplayList implementation
              comments. }
            ShapeStates[ShapeStateNum].AllMaterialsTransparent;
          end;
        finally
          if SSSX_RenderEndDisplayList = 0 then
            SSSX_PrepareEnd;
        end;
      end;
  end;

  if prBackground in Options then
    PrepareBackground;

  if prBoundingBox in Options then
    BoundingBox { ignore the result };

  if prTrianglesListNotOverTriangulate in Options then
    TrianglesList(false);

  if prTrianglesListOverTriangulate in Options then
    TrianglesList(true);

  if prManifoldAndBorderEdges in Options then
    ManifoldEdges;
end;

procedure TVRMLFlatSceneGL.Render(
  TestShapeStateVisibility: TTestShapeStateVisibility;
  TransparentGroup: TTransparentGroup);
begin
  case Optimization of
    roNone:
      begin
        RenderShapeStatesNoDisplayList(TestShapeStateVisibility,
          {$ifdef FPC_OBJFPC} @ {$endif} PrepareAndRenderShapeStateSimple,
          {$ifdef FPC_OBJFPC} @ {$endif} RenderBeginSimple,
          {$ifdef FPC_OBJFPC} @ {$endif} RenderEndSimple,
          TransparentGroup);
      end;
    roSceneAsAWhole:
      SAAW_Render(TransparentGroup);
    roSeparateShapeStates:
      begin
        { build display lists (if needed) and render all shape states }
        RenderShapeStatesNoDisplayList(TestShapeStateVisibility,
          {$ifdef FPC_OBJFPC} @ {$endif} SSS_RenderShapeState,
          {$ifdef FPC_OBJFPC} @ {$endif} SSSX_RenderBegin,
          {$ifdef FPC_OBJFPC} @ {$endif} SSSX_RenderEnd,
          TransparentGroup);
      end;
    roSeparateShapeStatesNoTransform:
      begin
        { build display lists (if needed) and render all shape states }
        RenderShapeStatesNoDisplayList(TestShapeStateVisibility,
          {$ifdef FPC_OBJFPC} @ {$endif} SSSNT_RenderShapeState,
          {$ifdef FPC_OBJFPC} @ {$endif} SSSX_RenderBegin,
          {$ifdef FPC_OBJFPC} @ {$endif} SSSX_RenderEnd,
          TransparentGroup);
      end;
  end;
end;

procedure TVRMLFlatSceneGL.ChangedAll;
begin
  inherited;

  { zmienily sie wskazniki na jakies obiekty,
    wiec musimy zrobic pelne UnprepareAll,
    mimo ze nie zalezy nam na utracie polaczenia z danym kontekstem OpenGL'a.
    Podobnie SAAW_DisplayList lub SSSX_DisplayLists sa juz nieaktualne
    wiec ich tez musimy sie pozbyc. Wiec trzeba wywolac po prostu CloseGL. }
  CloseGL;

  case Optimization of
    roSeparateShapeStates, roSeparateShapeStatesNoTransform:
      begin
        SSSX_DisplayLists.Count := ShapeStates.Count;

        { Yeah, in previous CloseGL call we also resetted all
          SSSX_DisplayLists items to 0
          (as a side-effect of calling glFreeDisplayList),
          but previous statement "SSSX_DisplayLists.Count := ..."
          possibly enlarged SSSX_DisplayLists.Count,
          so we must now make sure that all new items are inited to 0. }

        SSSX_DisplayLists.SetAll(0);
      end;
  end;

  RenderFrustumOctree_Visible.Count := ShapeStates.Count;
end;

procedure TVRMLFlatSceneGL.ChangedShapeStateFields(ShapeStateNum: integer);
var
  TG: TTransparentGroup;
begin
  inherited;

  { nie musimy tu robic nigdy Renderer.Unprepare*, bo przeciez obiekty node'ow
    sie nie zmienily, tylko ich pola. Zwracam uwage ze w ten sposob gdy
    Optimization = roNone to w tej procedurze nie musimy NIC robic - a wiec
    jest to jakis zysk gdy uzywamy roNone. }

  case Optimization of
    roSceneAsAWhole:
      for TG := Low(TG) to High(TG) do
        glFreeDisplayList(SAAW_DisplayList[TG]);
    roSeparateShapeStates, roSeparateShapeStatesNoTransform:
      { TODO -- test this }
      if SSSX_DisplayLists.Items[ShapeStateNum] <> 0 then
      begin
        if Optimization = roSeparateShapeStates then
          Renderer.Cache.ShapeState_DecReference(
            SSSX_DisplayLists.Items[ShapeStateNum]) else
          Renderer.Cache.ShapeStateNoTransform_DecReference(
            SSSX_DisplayLists.Items[ShapeStateNum]);
        SSSX_DisplayLists.Items[ShapeStateNum] := 0;
      end;
  end;
end;

{ shadow quads --------------------------------------------------------------- }

{ This returns vertex Original extruded into infinity, as seen from light
  at position LightPos.

  This is designed to work only with LightPos[3] = 1. In the future, when
  need arises, this may be improved to work with any LightPos[3] <> 0.

  For LightPos[3] = 0, i.e. directional light,
  don't use this, and there's no need to do it,
  since then the extruded point is just LightPos (for any vertex).
  RenderXxxShadowVolume want to treat it specially anyway (to optimize
  drawing, since then quads degenerate to triangles). }
function ExtrudeVertex(
  const Original: TVector3Single;
  const LightPos: TVector4Single): TVector4Single;
var
  LightPos3: TVector3Single absolute LightPos;
begin
  { Below is the moment when we require that
    if LightPos[3] <> 0 then LightPos[3] = 1 (not any other non-zero value).
    Otherwise we would have to divide here LightPos3 by LightPos[3].
    Maybe in the future this requirement will be removed and we'll work
    for any LightPos in homogenous coordinates, for now it's not really
    needed. }
  Result[0] := Original[0] -  LightPos3[0];
  Result[1] := Original[1] -  LightPos3[1];
  Result[2] := Original[2] -  LightPos3[2];
  Result[3] := 0;
end;

procedure TVRMLFlatSceneGL.RenderAllShadowVolume(
  const LightPos: TVector4Single;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  LightCap, DarkCap: boolean);

{ Zaklada ze wsrod podanych trojkatow wszystkie sa valid (tzn. nie ma
  zdegenerowanych trojkatow). To jest wazne zeby zagwarantowac to
  (TrianglesList gwarantuje to)
  bo inaczej zdegenerowane trojkaty moga sprawic ze wynik renderowania
  bedzie nieprawidlowy (pojawia sie na ekranie osobliwe "paski cienia"
  powstale w wyniku zdegenerowanych trojkatow dla ktorych wszystkie 3 sciany
  zostaly uznane za "front facing"). }

var
  TrianglesForLightCap: TDynTriangle3SingleArray;
  TrianglesForDarkCap: TDynTriangle4SingleArray;

  procedure RenderShadowQuad(
    const P0, P1: TVector3Single;
    const PExtruded0, PExtruded1: TVector4Single); overload;
  begin
    //glNormalv(TriangleNormal(P0, P1, PExtruded1));
    glVertexv(P0);
    glVertexv(P1);
    glVertexv(PExtruded1);
    glVertexv(PExtruded0);
  end;

  procedure RenderShadowQuad(
    const P0, P1: TVector3Single;
    const PExtruded: TVector4Single); overload;
  begin
    glVertexv(P0);
    glVertexv(P1);
    glVertexv(PExtruded);
  end;

  procedure HandleTriangle(const T: TTriangle3Single);
  var
    TExtruded: TTriangle4Single;
    Plane: TVector4Single;
    PlaneSide: Single;
  begin
    { We want to have consistent CCW orientation of shadow quads faces,
      so that face is oriented CCW <=> you're looking at it from outside
      (i.e. it's considered front face of this shadow quad).
      This is needed, since user of this method may want to do culling
      to eliminate back or front faces.

      If TriangleDir(T) indicates direction that goes from CCW triangle side.
      If TriangleDir(T) points in the same direction as LightPos then
      1st quad should be T1, T0, TExtruded0, TExtruded1.
      If TriangleDir(T) points in the opposite direction as LightPos then
      1st quad should be T0, T1, TExtruded1, TExtruded0.
      And so on.

      Note that this works for any LightPos[3].
      - For LightPos[3] = 1 this is  normal check.
      - For other LightPos[3] > 0 this is equivalent to normal check.
      - For LightPos[3] = 0, this calculates dot between light direction
        and plane direction. Plane direction points outwards, so PlaneSide > 0
        indicates that light is from the outside. So it matches results for
        LightPos[3] = 1.
      - For LightPos[3] < 0, is seems that the test has to be reversed !
        I.e. add "if LightPos[3] < 0 then PlaneSide := -PlaneSide;".
        This will be done when we'll have to do accept any homogeneous
        coords for LightPos, right now it's not needed.
    }
    Plane := TrianglePlane(T);
    PlaneSide := Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3];

    { Don't render quads on caps if LightPos lies on the Plane
      (which means that PlaneSide = 0) }
    if PlaneSide = 0 then
      Exit;

    if LightPos[3] <> 0 then
    begin
      TExtruded[0] := ExtrudeVertex(T[0], LightPos);
      TExtruded[1] := ExtrudeVertex(T[1], LightPos);
      TExtruded[2] := ExtrudeVertex(T[2], LightPos);

      if PlaneSide > 0 then
      begin
        RenderShadowQuad(T[1], T[0], TExtruded[1], TExtruded[0]);
        RenderShadowQuad(T[0], T[2], TExtruded[0], TExtruded[2]);
        RenderShadowQuad(T[2], T[1], TExtruded[2], TExtruded[1]);
      end else
      begin
        RenderShadowQuad(T[0], T[1], TExtruded[0], TExtruded[1]);
        RenderShadowQuad(T[1], T[2], TExtruded[1], TExtruded[2]);
        RenderShadowQuad(T[2], T[0], TExtruded[2], TExtruded[0]);
      end;

      if DarkCap then
      begin
        { reverse TExtruded dir, we want to render caps CCW outside always.

          Note that the test for reversing here is "PlaneSide > 0", while
          test for reversing LightCaps is "PlaneSide < 0": that's as it should
          be, as DarkCap triangle should always be in reversed direction
          than corresponding LightCap triangle (since they both should be
          CCW outside). }
        if PlaneSide > 0 then
          SwapValues(TExtruded[0], TExtruded[2]);
        TrianglesForDarkCap.AppendItem(TExtruded);
      end;
    end else
    begin
      { For directional lights, this gets a little simpler, since
        all extruded points are the same and equal just LightPos. }
      if PlaneSide > 0 then
      begin
        RenderShadowQuad(T[1], T[0], LightPos);
        RenderShadowQuad(T[0], T[2], LightPos);
        RenderShadowQuad(T[2], T[1], LightPos);
      end else
      begin
        RenderShadowQuad(T[0], T[1], LightPos);
        RenderShadowQuad(T[1], T[2], LightPos);
        RenderShadowQuad(T[2], T[0], LightPos);
      end;
    end;

    if LightCap then
    begin
      { reverse T dir, we want to render caps CCW outside always }
      if PlaneSide < 0 then
        TrianglesForLightCap.AppendItem(Triangle3Single(T[2], T[1], T[0])) else
        TrianglesForLightCap.AppendItem(T);
    end;
  end;

  procedure RenderTriangle3Single(const T: TTriangle3Single);
  begin
    glVertexv(T[0]);
    glVertexv(T[1]);
    glVertexv(T[2]);
  end;

  procedure RenderTriangle4Single(const T: TTriangle4Single);
  begin
    glVertexv(T[0]);
    glVertexv(T[1]);
    glVertexv(T[2]);
  end;

var
  I: Integer;
  Triangles: TDynTriangle3SingleArray;
  TransformedTri: TTriangle3Single;
  TPtr: PTriangle3Single;
  T4Ptr: PTriangle4Single;
begin
  TrianglesForLightCap := nil;
  TrianglesForDarkCap := nil;

  Triangles := TrianglesList(false);

  { If light is directional, no need to render dark cap }
  DarkCap := DarkCap and (LightPos[3] <> 0);

  { It's a not nice that we have to create a structure in memory
    to hold TrianglesForLight/DarkCap. But that's because they have to be rendered
    after rendering normal shadow quads (because shadow quads may be
    quads or triangles, caps are only triangles, and are rendered in
    glDepthFunc(GL_NEVER) mode. }

  if LightCap then
  begin
    TrianglesForLightCap := TDynTriangle3SingleArray.Create;
    TrianglesForLightCap.AllowedCapacityOverflow := Triangles.Count;
  end;

  if DarkCap then
  begin
    TrianglesForDarkCap := TDynTriangle4SingleArray.Create;
    TrianglesForDarkCap.AllowedCapacityOverflow := Triangles.Count;
  end;

  try

    if LightPos[3] <> 0 then
      glBegin(GL_QUADS) else
      glBegin(GL_TRIANGLES);

    TPtr := Triangles.Pointers[0];

    if TransformIsIdentity then
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        HandleTriangle(TPtr^);
        Inc(TPtr);
      end;
    end else
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        { calculate TransformedTri := Triangles[I] transformed by Transform }
        TransformedTri[0] := MultMatrixPoint(Transform, TPtr^[0]);
        TransformedTri[1] := MultMatrixPoint(Transform, TPtr^[1]);
        TransformedTri[2] := MultMatrixPoint(Transform, TPtr^[2]);

        HandleTriangle(TransformedTri);
        Inc(TPtr);
      end;
    end;

    glEnd;

    if LightCap or DarkCap then
    begin
      { See RenderSilhouetteShadowVolume for explanation why caps
        should be rendered with glDepthFunc(GL_NEVER). }
      glPushAttrib(GL_DEPTH_BUFFER_BIT); { to save glDepthFunc call below }
      glDepthFunc(GL_NEVER);
      glBegin(GL_TRIANGLES);

      if LightCap then
      begin
        TPtr := TrianglesForLightCap.Pointers[0];
        for I := 0 to TrianglesForLightCap.Count - 1 do
        begin
          RenderTriangle3Single(TPtr^);
          Inc(TPtr);
        end;
      end;

      if DarkCap then
      begin
        T4Ptr := TrianglesForDarkCap.Pointers[0];
        for I := 0 to TrianglesForDarkCap.Count - 1 do
        begin
          RenderTriangle4Single(T4Ptr^);
          Inc(T4Ptr);
        end;
      end;

      glEnd;
      glPopAttrib;
    end;
  finally
    FreeAndNil(TrianglesForLightCap);
    FreeAndNil(TrianglesForDarkCap);
  end;
end;

procedure TVRMLFlatSceneGL.RenderSilhouetteShadowVolume(
  const LightPos: TVector4Single;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  const LightCap, DarkCap: boolean);

{ Speed:

  At the beginning we used here the simple algorithm from
  [http://www.gamedev.net/reference/articles/article1873.asp]
  (look into SVN revision < 1980 in Kambi private repo).
  For each triangle with dot > 0, add it to the Edges list
  --- unless it's already there, in which case remove it.
  This way, at the end Edges contain all edges that have on one
  side triangle with dot > 0 and on the other side triangle with dot <= 0.
  In other words, all sihouette edges.
  (This is all assuming that model is composed from manifold parts,
  which means that each edge has exactly 2 neighbor triangles).

  But this algorithms proved to be unacceptably slow for typical cases.
  While it generated much less shadow quads than naive
  RenderAllShadowVolume, the time spent in detecting the silhouette edges
  made the total time even worse than RenderAllShadowVolume.
  Obviously, that's because we started from the list of triangles,
  without any explicit information about the edges.
  The time of this algorithm was n*m, if n is the number of triangles
  and m the number of edges, and on closed manifold n*3/2 = n so
  it's just n^2. Terrible, if you take complicated shadow caster.

  To make this faster, we have to know the connections inside the model:
  that's what ManifoldEdges list is all about. It allowed us to
  implement this in time proportional to number of edges, which is

  TODO: have some indexed line list to only pass through
  interesting edges. In other words, once you find one silhouette edge,
  just travel from this edge to other edges.
  Advantages:
  - speed increase because we travel only on the interesting edges
  - speed increase because we can render quad_strip instead of quads list
    in case of directional lights, we can even use triangle fan for shadow quads
    in case of DarkCap (and not directional light), we can render DarkCap
    as triangle fan also (see "fast, robust and practical sv" paper", this
    works)
  Disadvantages:
  - this would require that we would have to use only really manifold shapes.
    E.g. right now it's ok to have one manifold scene created by two
    IndexedFaceSet nodes, that have two Coordinate3 nodes
    (assuming that appropriate vertexes on Coordinate3 are really exactly
    the same).
  - what about shapes that have more than one silhouette edge ?
    Yes, this happens, since shapes are not necessarily convex.
}

var
  Triangles: TDynTriangle3SingleArray;

  procedure RenderShadowQuad(EdgePtr: PManifoldEdge;
    const P0Index, P1Index: Cardinal); overload;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Triangles.Pointers[EdgePtr^.Triangles[0]];
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    if TransformIsIdentity then
    begin
      V0 := EdgeV0^;
      V1 := EdgeV1^;
    end else
    begin
      V0 := MultMatrixPoint(Transform, EdgeV0^);
      V1 := MultMatrixPoint(Transform, EdgeV1^);
    end;

    glVertexv(V0);
    glVertexv(V1);

    if LightPos[3] <> 0 then
    begin
      glVertexv(ExtrudeVertex(V1, LightPos));
      glVertexv(ExtrudeVertex(V0, LightPos));
    end else
      glVertexv(LightPos);
  end;

  procedure RenderShadowQuad(EdgePtr: PBorderEdge;
    const P0Index, P1Index: Cardinal); overload;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Triangles.Pointers[EdgePtr^.TriangleIndex];
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    if TransformIsIdentity then
    begin
      V0 := EdgeV0^;
      V1 := EdgeV1^;
    end else
    begin
      V0 := MultMatrixPoint(Transform, EdgeV0^);
      V1 := MultMatrixPoint(Transform, EdgeV1^);
    end;

    glVertexv(V0);
    glVertexv(V1);
    if LightPos[3] <> 0 then
    begin
      glVertexv(ExtrudeVertex(V1, LightPos));
      glVertexv(ExtrudeVertex(V0, LightPos));
    end else
      glVertexv(LightPos);
  end;

  { We initialize TrianglesPlaneSide and render caps in one step,
    this way we have to iterate over Triangles only once, and in case
    of PlaneSide_NotIdentity and rendering caps --- we have to transform
    each triangle only once. }
  procedure InitializeTrianglesPlaneSideAndRenderCaps(
    TrianglesPlaneSide: TDynBooleanArray;
    LightCap, DarkCap: boolean);

    procedure RenderCaps(const T: TTriangle3Single);
    begin
      if LightCap then
      begin
        glVertexv(T[0]);
        glVertexv(T[1]);
        glVertexv(T[2]);
      end;

      if DarkCap then
      begin
        glVertexv(ExtrudeVertex(T[2], LightPos));
        glVertexv(ExtrudeVertex(T[1], LightPos));
        glVertexv(ExtrudeVertex(T[0], LightPos));
      end;
    end;

    function PlaneSide_Identity(const T: TTriangle3Single): boolean;
    var
      Plane: TVector4Single;
    begin
      Plane := TrianglePlane(T);
      Result := (Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3]) > 0;
      if Result then RenderCaps(T);
    end;

    function PlaneSide_NotIdentity(const T: TTriangle3Single): boolean;
    var
      Plane: TVector4Single;
      TriangleTransformed: TTriangle3Single;
    begin
      TriangleTransformed[0] := MultMatrixPoint(Transform, T[0]);
      TriangleTransformed[1] := MultMatrixPoint(Transform, T[1]);
      TriangleTransformed[2] := MultMatrixPoint(Transform, T[2]);
      Plane := TrianglePlane(TriangleTransformed);
      Result := (Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3]) > 0;
      if Result then RenderCaps(TriangleTransformed);
    end;

  var
    TrianglePtr: PTriangle3Single;
    I: Integer;
  begin
    TrianglesPlaneSide.Count := Triangles.Count;
    TrianglePtr := Triangles.Pointers[0];

    { If light is directional, no need to render dark cap }
    DarkCap := DarkCap and (LightPos[3] <> 0);

    if LightCap or DarkCap then
    begin
      { It's crucial to set glDepthFunc(GL_NEVER) for LightCap.
        This way we get proper self-shadowing. Otherwise, LightCap would
        collide in z buffer with the object itself.

        Setting glDepthFunc(GL_NEVER) for DarkCap also is harmless and OK.
        And it allows us to render both LightCap and DarkCap in one
        GL_TRIANGLES pass, in one iteration over Triangles list, which is
        good for speed.

        Some papers propose other solution:
          glEnable(GL_POLYGON_OFFSET_FILL);
          glPolygonOffset(1, 1);
        but this is no good for use, because it cannot be applied
        to DarkCap (otherwise DarkCap in infinity (as done by ExtrudeVertex)
        would go outside of depth range (even for infinite projection,
        as glPolygonOffset works already after the vertex is transformed
        by projection), as this would break DarkCap rendering).
      }

      glPushAttrib(GL_DEPTH_BUFFER_BIT); { to save glDepthFunc call below }
      glDepthFunc(GL_NEVER);
      glBegin(GL_TRIANGLES);
    end;

    if TransformIsIdentity then
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.Items[I] := PlaneSide_Identity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
    end else
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.Items[I] := PlaneSide_NotIdentity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
    end;

    if LightCap or DarkCap then
    begin
      glEnd;
      glPopAttrib;
    end;
  end;

var
  I: Integer;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TDynBooleanArray;
  ManifoldEdgesNow: TDynManifoldEdgeArray;
  ManifoldEdgePtr: PManifoldEdge;
  BorderEdgesNow: TDynBorderEdgeArray;
  BorderEdgePtr: PBorderEdge;
begin
  Triangles := TrianglesList(false);

  TrianglesPlaneSide := TDynBooleanArray.Create;
  try
    InitializeTrianglesPlaneSideAndRenderCaps(TrianglesPlaneSide,
      LightCap, DarkCap);

    if LightPos[3] <> 0 then
      glBegin(GL_QUADS) else
      glBegin(GL_TRIANGLES);

      { for each 2-manifold edge, possibly render it's shadow quad }
      ManifoldEdgesNow := ManifoldEdges;
      ManifoldEdgePtr := ManifoldEdgesNow.Pointers[0];
      for I := 0 to ManifoldEdgesNow.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.Items[ManifoldEdgePtr^.Triangles[0]];
        PlaneSide1 := TrianglesPlaneSide.Items[ManifoldEdgePtr^.Triangles[1]];

        { Only if PlaneSide0 <> PlaneSide1 it's a silhouette edge,
          so only then render it's shadow quad.

          We want to have consistent CCW orientation of shadow quads faces,
          so that face is oriented CCW <=> you're looking at it from outside
          (i.e. it's considered front face of this shadow quad).
          This is needed, since user of this method may want to do culling
          to eliminate back or front faces.

          TriangleDir(T) indicates direction that goes from CCW triangle side
          (that's guaranteed by the way TriangleDir calculates plane dir).
          So PlaneSideX is @true if LightPos is on CCW side of appropriate
          triangle. So if PlaneSide0 the shadow quad is extended
          in reversed Triangles[0] order, i.e. like 1, 0, Extruded0, Extruded1.
          Otherwise, in normal Triangles[0], i.e. 0, 1, Extruded1, Extruded0.

          Just draw it, the triangle corners numbered with 0,1,2 in CCW and
          imagine that you want the shadow quad to be also CCW on the outside,
          it will make sense then :) }
        if PlaneSide0 and not PlaneSide1 then
          RenderShadowQuad(ManifoldEdgePtr, 1, 0) else
        if PlaneSide1 and not PlaneSide0 then
          RenderShadowQuad(ManifoldEdgePtr, 0, 1);

        Inc(ManifoldEdgePtr);
      end;

      { for each border edge, always render it's shadow quad }
      BorderEdgesNow := BorderEdges;
      BorderEdgePtr := BorderEdgesNow.Pointers[0];
      for I := 0 to BorderEdgesNow.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.Items[BorderEdgePtr^.TriangleIndex];

        { We want to have consistent CCW orientation of shadow quads faces,
          so that face is oriented CCW <=> you're looking at it from outside
          (i.e. it's considered front face of this shadow quad).
          This is needed, since user of this method may want to do culling
          to eliminate back or front faces.

          TriangleDir(T) indicates direction that goes from CCW triangle side
          (that's guaranteed by the way TriangleDir calculates plane dir).
          So PlaneSide0 is true if LightPos is on CCW side of appropriate
          triangle. So if PlaneSide0, the shadow quad is extended
          in the direction of TriangleIndex, like 1, 0, Extruded0, Extruded1. }
        if PlaneSide0 then
          RenderShadowQuad(BorderEdgePtr, 1, 0) else
          RenderShadowQuad(BorderEdgePtr, 0, 1);

        Inc(BorderEdgePtr);
      end;

    glEnd;

  finally FreeAndNil(TrianglesPlaneSide) end;
end;

procedure TVRMLFlatSceneGL.RenderShadowVolume(
  const LightPos: TVector4Single;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  const LightCap, DarkCap: boolean;
  const AllowSilhouetteOptimization: boolean);
begin
  if (ManifoldEdges <> nil) and AllowSilhouetteOptimization then
    RenderSilhouetteShadowVolume(
      LightPos, TransformIsIdentity, Transform, LightCap, DarkCap) else
    RenderAllShadowVolume(
      LightPos, TransformIsIdentity, Transform, LightCap, DarkCap);
end;

procedure TVRMLFlatSceneGL.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  const AllowSilhouetteOptimization: boolean);
begin
  if ShadowVolumesHelper.SceneShadowPossiblyVisible then
  begin
    RenderShadowVolume(ShadowVolumesHelper.LightPosition,
      TransformIsIdentity, Transform,
      ShadowVolumesHelper.ZFailAndLightCap,
      ShadowVolumesHelper.ZFail,
      AllowSilhouetteOptimization);
  end;
end;

procedure TVRMLFlatSceneGL.InitAndRenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  const AllowSilhouetteOptimization: boolean);
var
  Box: TBox3d;
begin
  { calculate Box }
  Box := BoundingBox;
  if not TransformIsIdentity then
    Box := BoundingBoxTransform(Box, Transform);

  ShadowVolumesHelper.InitScene(Box);

  RenderShadowVolume(ShadowVolumesHelper, TransformIsIdentity, Transform,
    AllowSilhouetteOptimization);
end;

{ RenderFrustum and helpers ---------------------------------------- }

function TVRMLFlatSceneGL.RenderFrustum_TestShapeState(
  ShapeStateNum: Integer): boolean;

{$ifdef RENDER_FRUSTUM_USES_BOUNDING_SPHERE}
begin
 Result := ShapeStates[ShapeStateNum].
   FrustumBoundingSphereCollisionPossibleSimple(RenderFrustum_Frustum^);
{$endif}

{$ifdef RENDER_FRUSTUM_USES_BOUNDING_BOX}
begin
 Result := FrustumBox3dCollisionPossibleSimple(RenderFrustum_Frustum^,
   ShapeStates[ShapeStateNum].BoundingBox);
{$endif}

{$ifdef RENDER_FRUSTUM_USES_BOTH}
begin
 Result :=
   ShapeStates[ShapeStateNum].FrustumBoundingSphereCollisionPossibleSimple(
     RenderFrustum_Frustum^) and
   FrustumBox3dCollisionPossibleSimple(RenderFrustum_Frustum^,
     ShapeStates[ShapeStateNum].BoundingBox);
{$endif}

end;

procedure TVRMLFlatSceneGL.RenderFrustum(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
  RenderFrustum_Frustum := @Frustum;
  Render({$ifdef FPC_OBJFPC} @ {$endif} RenderFrustum_TestShapeState,
    TransparentGroup);
end;

{ RenderFrustumOctree ---------------------------------------- }

function TVRMLFlatSceneGL.RenderFrustumOctree_TestShapeState(
  ShapeStateNum: Integer): boolean;
begin
  Result := RenderFrustumOctree_Visible.Items[ShapeStateNum];
end;

procedure TVRMLFlatSceneGL.RenderFrustumOctree_EnumerateOctreeItem(
  ShapeStateNum: Integer; CollidesForSure: boolean);

{$ifdef RENDER_FRUSTUM_OCTREE_NO_BONUS_CHECKS}
begin
  { This implementation is fast, but may not eliminate as many
    ShapeStates from rendering pipeline as it's possible
    (so overall speed may be worse) : }

  RenderFrustumOctree_Visible.Items[ShapeStateNum] := true;
{$endif}

{$ifdef RENDER_FRUSTUM_OCTREE_BONUS_SPHERE_CHECK}
begin
  { Another implementation: if CollidesForSure = false
    then checks shapeshate's bounding sphere versus frustum before
    setting
      RenderFrustumOctree_Visible.Items[ShapeStateNum] := true
    This means that it wastes some time on doing
    FrustumSphereCollisionPossibleSimple but it may be able
    to eliminate more shapestate's from rendering pipeline,
    so overall speed may be better. }

  if (not RenderFrustumOctree_Visible.Items[ShapeStateNum]) and
     ( CollidesForSure or
       ShapeStates[ShapeStateNum].FrustumBoundingSphereCollisionPossibleSimple
         (RenderFrustumOctree_Frustum^) ) then
    RenderFrustumOctree_Visible.Items[ShapeStateNum] := true;
{$endif}

{ Other implementations are also possible :

  3rd one: check FrustumSphereCollisionPossibleSimple
  and (if it succeeds) then additionally check
  FrustumBox3dCollisionPossibleSimple.

  4th one: check only Frustumbox3dCollisionPossibleSimple.
  (but this will probably be worse then 3rd one). }

end;

procedure TVRMLFlatSceneGL.RenderFrustumOctree(const Frustum: TFrustum;
  Octree: TVRMLShapeStateOctree;
  TransparentGroup: TTransparentGroup);
begin
  if Optimization <> roSceneAsAWhole then
  begin
    RenderFrustumOctree_Frustum := @Frustum;

    RenderFrustumOctree_Visible.SetAll(false);
    Octree.EnumerateCollidingOctreeItems(Frustum,
      {$ifdef FPC_OBJFPC} @ {$endif} RenderFrustumOctree_EnumerateOctreeItem);
    Render({$ifdef FPC_OBJFPC} @ {$endif} RenderFrustumOctree_TestShapeState,
      TransparentGroup);
  end else
    Render(nil, TransparentGroup);
end;

procedure TVRMLFlatSceneGL.RenderFrustumOctree(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
  Assert(DefaultShapeStateOctree <> nil);
  RenderFrustumOctree(Frustum, DefaultShapeStateOctree,
    TransparentGroup);
end;

{ Background-related things ---------------------------------------- }

procedure TVRMLFlatSceneGL.FBackgroundInvalidate;
begin
 FreeAndNil(FBackground);
 FBackgroundValid := false;
end;

procedure TVRMLFlatSceneGL.SetBackgroundSkySphereRadius(const Value: Single);
begin
 FBackgroundInvalidate;
 FBackgroundSkySphereRadius := Value;
end;

procedure TVRMLFlatSceneGL.PrepareBackground;
{ After PrepareBackground assertion FBackgroundValid is valid }
var
  InitialState: TVRMLGraphTraverseState;
  BgTransform: TMatrix4Single;
  BgAverageScaleTransform: Single;
  BgNode: TNodeBackground;
  SkyAngleCount: Integer;
  SkyColorCount: Integer;
  GroundAngleCount: Integer;
  GroundColorCount: Integer;
begin
  if FBackgroundValid then Exit;

  InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
  try
    if (RootNode <> nil) and
      RootNode.TryFindNodeTransform(InitialState, TNodeBackground,
        TVRMLNode(BgNode), BgTransform, BgAverageScaleTransform) then
    begin
      SkyAngleCount := BgNode.FdSkyAngle.Count;
      SkyColorCount := BgNode.FdSkyColor.Count;

      if SkyColorCount <= 0 then
      begin
        VRMLNonFatalError('Background node incorrect: ' +
          'Sky must have at least one color');
        FBackground := nil;
      end else
      begin
        if SkyAngleCount + 1 <> SkyColorCount then
        begin
          VRMLNonFatalError('Background node incorrect: ' +
            'Sky must have exactly one more Color than Angles');
          { We know now that SkyColorCount >= 1 and
            SkyAngleCount >= 0 (since SkyAngleCount is a count of an array).
            So we correct one of them to be smaller. }
          if SkyAngleCount + 1 > SkyColorCount then
            SkyAngleCount := SkyColorCount - 1 else
            SkyColorCount := SkyAngleCount + 1;
        end;

        GroundAngleCount := BgNode.FdGroundAngle.Count;
        GroundColorCount := BgNode.FdGroundColor.Count;

        if (GroundAngleCount <> 0) and
           (GroundAngleCount + 1 <> GroundColorCount) then
        begin
          VRMLNonFatalError('Background node incorrect: ' +
            'Ground must have exactly one more Color than Angles');
          { We know now that GroundColorCount >= 1 and
            GroundAngleCount >= 0 (since GroundAngleCount is a count of an array).
            So we correct one of them to be smaller. }
          if GroundAngleCount + 1 > GroundColorCount then
            GroundAngleCount := GroundColorCount - 1 else
            GroundColorCount := GroundAngleCount + 1;
        end;

        { TODO: We should extract here only rotation from BgTransform matrix.
          Below is a very hacky way of at least cancelling the translation.
          This will work OK for any rigid body matrix, i.e. composed only from
          rotation and translation. }
        BgTransform[3, 0] := 0;
        BgTransform[3, 1] := 0;
        BgTransform[3, 2] := 0;

        { The call to BgNode.BgImages is important here, as it may actually
          load the images from file. So first we want to set AllowedBgImagesClasses
          and ImagesCache as appropriate. }
        BgNode.SetAllowedBgImagesClasses(GLImageClasses);
        BgNode.ImagesCache := Renderer.Cache;

        FBackground := TBackgroundGL.Create(BgTransform,
          @(BgNode.FdGroundAngle.Items.Items[0]), GroundAngleCount,
          @(BgNode.FdGroundColor.Items.Items[0]), GroundColorCount,
          BgNode.BgImages,
          @(BgNode.FdSkyAngle.Items.Items[0]), SkyAngleCount,
          @(BgNode.FdSkyColor.Items.Items[0]), SkyColorCount,
          BackgroundSkySphereRadius,
          Attributes.ColorModulatorSingle,
          Attributes.ColorModulatorByte);
      end;
    end else
      FBackground := nil;

    FBackgroundValid := true;
  finally InitialState.Free end;
end;

function TVRMLFlatSceneGL.Background: TBackgroundGL;
begin
 PrepareBackground;
 result := FBackground;
end;

function TVRMLFlatSceneGL.Attributes: TVRMLSceneRenderingAttributes;
begin
  Result := Renderer.Attributes as TVRMLSceneRenderingAttributes;
end;

function TVRMLFlatSceneGL.CreateHeadLight: TVRMLGLHeadLight;
var
  HeadLightNode: TNodeKambiHeadLight;
begin
  HeadLightNode := nil;
  if RootNode <> nil then
    HeadLightNode := RootNode.TryFindNode(TNodeKambiHeadLight, true) as
      TNodeKambiHeadLight;
  Result := TVRMLGLHeadLight.Create(HeadLightNode);
end;

function TVRMLFlatSceneGL.BumpMappingMethod: TBumpMappingMethod;
begin
  Result := Renderer.BumpMappingMethod;
end;

{ TVRMLSceneRenderingAttributes ---------------------------------------------- }

constructor TVRMLSceneRenderingAttributes.Create;
begin
  inherited;

  FBlending := true;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;

  FScenes := TVRMLFlatSceneGLsList.Create;
end;

destructor TVRMLSceneRenderingAttributes.Destroy;
begin
  FreeAndNil(FScenes);
  inherited;
end;

procedure TVRMLSceneRenderingAttributes.Assign(Source: TPersistent);
var
  S: TVRMLSceneRenderingAttributes;
begin
  if Source is TVRMLSceneRenderingAttributes then
  begin
    S := TVRMLSceneRenderingAttributes(Source);
    Blending := S.Blending;
    BlendingSourceFactor := S.BlendingSourceFactor;
    BlendingDestinationFactor := S.BlendingDestinationFactor;
    inherited;
  end else
    inherited;
end;

function TVRMLSceneRenderingAttributes.Equals(SecondValue: TPersistent): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TVRMLSceneRenderingAttributes) and
    (TVRMLSceneRenderingAttributes(SecondValue).Blending = Blending) and
    (TVRMLSceneRenderingAttributes(SecondValue).BlendingSourceFactor = BlendingSourceFactor) and
    (TVRMLSceneRenderingAttributes(SecondValue).BlendingDestinationFactor = BlendingDestinationFactor);
end;

{ Interfejs Renderera mowi ze zeby zmienic atrybut renderer musi byc wolny
  od aktualnego kontekstu OpenGLa, wiec musimy przed zmiana atrybutu
  wywolac przynajmniej Renderer.UnprepareAll.

  Prawda jest taka ze my tez musimy byc wolni - nie mozemy miec zadnych
  przeliczonych display-list, nic takiego, bo wlasnie zmiana Attributes renderera
  moze te rzeczy zdezaktualizowac - z innymi attrib renderer bedzie dawal
  co innego.

  Wiec kazda zmiana atrybutu musi byc poprzedzona ScenesCloseGLRenderer. }

procedure TVRMLSceneRenderingAttributes.SetBlending(const Value: boolean);
begin
  if Blending <> Value then
  begin
    FScenes.CloseGLRenderer;
    FBlending := Value;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetBlendingSourceFactor(
  const Value: TGLenum);
begin
  if BlendingSourceFactor <> Value then
  begin
    FScenes.CloseGLRenderer;
    FBlendingSourceFactor := Value;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetBlendingDestinationFactor(
  const Value: TGLenum);
begin
  if BlendingDestinationFactor <> Value then
  begin
    FScenes.CloseGLRenderer;
    FBlendingDestinationFactor := Value;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetOnBeforeGLVertex(
  const Value: TBeforeGLVertexProc);
begin
  if {$ifndef FPC_OBJFPC} @ {$endif} OnBeforeGLVertex <>
     {$ifndef FPC_OBJFPC} @ {$endif} Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetSmoothShading(const Value: boolean);
begin
  if SmoothShading <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetColorModulatorSingle(
  const Value: TColorModulatorSingleFunc);
begin
  if {$ifndef FPC_OBJFPC} @ {$endif} ColorModulatorSingle <>
     {$ifndef FPC_OBJFPC} @ {$endif} Value then
  begin
    FScenes.FBackgroundInvalidate;
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetColorModulatorByte(
  const Value: TColorModulatorByteFunc);
begin
  if {$ifndef FPC_OBJFPC} @ {$endif} ColorModulatorByte <>
     {$ifndef FPC_OBJFPC} @ {$endif} Value then
  begin
    FScenes.FBackgroundInvalidate;
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetUseLights(const Value: boolean);
begin
  if UseLights <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetFirstGLFreeLight(const Value: Cardinal);
begin
  if FirstGLFreeLight <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetLastGLFreeLight(const Value: integer);
begin
  if LastGLFreeLight <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  if EnableTextures <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetFirstGLFreeTexture(const Value: Cardinal);
begin
  if FirstGLFreeTexture <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetLastGLFreeTexture(const Value: integer);
begin
  if LastGLFreeTexture <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetTextureMinFilter(const Value: TGLint);
begin
  if TextureMinFilter <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetTextureMagFilter(const Value: TGLint);
begin
  if TextureMagFilter <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetPointSize(const Value: TGLFloat);
begin
  if PointSize <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

procedure TVRMLSceneRenderingAttributes.SetUseFog(const Value: boolean);
begin
  if Usefog <> Value then
  begin
    FScenes.CloseGLRenderer;
    inherited;
  end;
end;

{ TVRMLFlatSceneGLsList ------------------------------------------------------ }

procedure TVRMLFlatSceneGLsList.CloseGL;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   if Items[I] <> nil then
     Items[I].CloseGL;
end;

procedure TVRMLFlatSceneGLsList.FBackgroundInvalidate;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   if Items[I] <> nil then
     Items[I].FBackgroundInvalidate;
end;

procedure TVRMLFlatSceneGLsList.CloseGLRenderer;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   if Items[I] <> nil then
     Items[I].CloseGLRenderer;
end;

end.