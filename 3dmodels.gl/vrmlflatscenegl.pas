{
  Copyright 2003-2006 Michalis Kamburelis.

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
  VRMLShapeStateOctree;

{$define read_interface}

type
  { This is used by @link(TVRMLFlatSceneGL.Optimization) to describe
    what kind of optimization should be done. }
  TGLRendererOptimization = (
    { No optimization. No OpenGL display lists are constructed.
      So calling PrepareRender and ChangedAll is very fast.
      On the other hand, rendering is significantly slower,
      as display lists often help a lot.

      Use this if you plan to change the scene at runtime a lot
      (or when you're going to render TVRMLFlatSceneGL object
      very few times, say, only 1-2 frames, in some special situations)
      Then building display lists would be only a waste of time,
      since they would have to be rebuild very often. }
    roNone,

    { Treat the scene as a one big static object.
      One OpenGL display list is used, that renders the whole object.
      This is great optimization if the scene is static (otherwise
      rebuilding display lists too often would cost too much time)
      and you're sure that user will usually see the whole scene
      (or just a large part of it).

      If the scene is static but user usually only looks at some small
      part of it, then building octree for the scene and using
      roSeparateShapeStates and @link(TVRMLFlatSceneGL.RenderFrustumOctree)
      may be better. }
    roSceneAsAWhole,

    { Build separate OpenGL display list for each @link(TVRMLShapeState)
      on list @link(TVRMLFlatScene.ShapeStates). Use this if

      @orderedList(
        @item(you will change from time to time only some small parts of
          the scene (since this will allow to rebuild, on changing, only
          some small display lists, as opposed to roSceneAsAWhole,
          that has to rebuild large display list even if the change
          is very local).)

        @item(and/or you know that usually user will not see the whole scene,
         only a small part of it.
         See TestShapeStateVisibility parameter of @link(TVRMLFlatSceneGL.Render)
         and @link(TVRMLFlatSceneGL.RenderFrustum) and
         @link(TVRMLFlatSceneGL.RenderFrustumOctree).)
       )
    }
    roSeparateShapeStates
  );
  PGLRendererOptimization = ^TGLRendererOptimization;

  { Internal for TVRMLFlatSceneGL }
  TRenderShapeState = procedure(ShapeStateNum: Integer) of object;
  TObjectProcedure = procedure of object;

  TTestShapeStateVisibility = function(ShapeStateNum: Integer): boolean
    of object;

  TQuad3Single = array[0..3] of TVector3Single;
  PQuad3Single = ^TQuad3Single;

  TDynArrayItem_1 = TQuad3Single;
  PDynArrayItem_1 = PQuad3Single;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Quad3Single = TInfiniteArray_1;
  PArray_Quad3Single = PInfiniteArray_1;
  TDynQuad3SingleArray = TDynArray_1;

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
      returns true for given node), pozniej Renderer.RenderEnd.
      Dodatkowo moze robic jeszcze jakies rzeczy z OpenGLem, np. to tutaj
      realizujemy blending.

      De facto nie wywoluje Renderer.RenderBegin and Renderer.RenderEnd
      tylko RenderBeginProc i RenderEndProc. Te procedury *musza* wywolac
      odpowiednie Renderer.RenderBegin/End. Co najwyzej niekiedy
      moga to opakowac w display liste. Patrz
      RenderBeginSimple and RenderEndSimple.

      This procedure never creates or uses any display list.
      You can freely put it's contents inside display list
      (assuming that RenderShapeStateProc, RenderBeginProc and RenderEndProc
      are something that can be part of display list).

      This sets FLastRender_RenderedShapeStatesCount and
      FLastRender_AllShapeStatesCount. }
    procedure RenderShapeStatesNoDispList(
      TestShapeStateVisibility: TTestShapeStateVisibility;
      RenderShapeStateProc: TRenderShapeState;
      RenderBeginProc, RenderEndProc: TObjectProcedure);

    { niszcz powiazania z kontekstem OpenGLa obiektu Renderer i ew. wszelkich
      wygenerowanych na podstawie niego rzeczy (w tym momencie oznacza
      to SAAW_DisplayList i SSS_DisplayLists).
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
      AProvidedRenderer: TVRMLOpenGLRenderer);

    DefaultSavedShadowQuads: TDynQuad3SingleArray;

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
    SAAW_DisplayList: TGLuint; { = 0 jesli nie zainicjowana }

    { Mode = GL_COMPILE to przygotuje wszystko. Mode = GL_COMPILE_AND_EXECUTE
      to przygotuje wszystko i od razu wyrenderuje. Wywoluj tylko gdy
      Optimization = roSceneAsAWhole i SAAW_DisplayList = 0.

      This calls RenderShapeStatesNoDispList so this sets
      FLastRender_RenderedShapeStatesCount and
      FLastRender_AllShapeStatesCount. }
    procedure SAAW_PrepareRenderAndMaybeRender(Mode: TGLenum);

    { ------------------------------------------------------------
      Private things used only when Optimization = roSeparateShapeStates.
      Prefixed with SSS, for clarity.  }

    { <> nil if and only if Optimization = roSeparateShapeStates.
      Every item is 0 if it is not initialized. }
    SSS_DisplayLists: TDynGLuintArray;

    SSS_RenderBeginDisplayList: TGLuint;
    SSS_RenderEndDisplayList: TGLuint;

    { These create appropriate SSS_Render*DisplayList display list.
      If mode = GL_COMPILE_AND_EXECUTE they also execute it. }
    procedure SSS_PrepareAndMaybeRenderBegin(Mode: TGLenum);
    procedure SSS_PrepareAndMaybeRenderEnd(Mode: TGLenum);

    { These call appropriate SSS_Render*DisplayList display list.
      If display list is not ready, they create it. }
    procedure SSS_RenderBegin;
    procedure SSS_RenderEnd;

    { Use this only when Optimization = roSeparateShapeStates.
      It can be passed as RenderShapeStateProc.

      This renders SSS_DisplayLists.Items[ShapeStateNum]
      display list (creating it if necessary). }
    procedure SSS_PrepareAndRenderShapeState(ShapeStateNum: Integer);

    { Call this only when Optimization = roSeparateShapeStates and
      SSS_DisplayLists.Items[ShapeStateNum] = 0.

      Prepares shapestate (by Renderer.Prepare(ShapeStates[ShapeStateNum].State).

      Then creates display list SSS_DisplayLists.Items[ShapeStateNum]
      and initializes it with contents of RenderShapeStateSimple(ShapeStateNum).
      Mode is passed to glNewList: if it's GL_COMPILE_AND_EXECUTE,
      it not only creates display list but also renders it now,
      if it's GL_COMPILE it only creates given display list.

      This is somehow equivalent to SAAW_PrepareRenderAndMaybeRender,
      but it operates only on a single ShapeState.

      Short comparison of SSS_PrepareAndRenderShapeState versus
      SSS_PrepareRenderAndMaybeRender:
      @orderedList(
        @item(
          SSS_RenderShapeState always renders shapestate,
          also creating it's display list if necessary)
        @item(
          SSS_PrepareRenderAndMaybeRender always creates shapestate's display list,
          also rendering it if Mode = GL_COMPILE_AND_EXECUTE)
      ) }
    procedure SSS_PrepareRenderAndMaybeRender(
      Mode: TGLenum; ShapeStateNum: Integer);
  public
    { @noAutoLinkHere }
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AOptimization: TGLRendererOptimization);

    { This is a very special constructor, that forces this class to use
      provided AProvidedRenderer.

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

    { @noAutoLinkHere }
    destructor Destroy; override;

    { Niszcz wszelkie powiazania tego obiektu z aktualnym kontekstem OpenGL'a.
      Generalnie, wszystko to co tworzy PrepareRender(true).
      Jezeli juz aktualnie nie ma takich powiazan (bo np. wywolales ta metode
      dwa razy pod rzad) to nic nie zrobi. Wywolywane tez automatycznie
      z destruktora. }
    procedure CloseGL;

    { procedura PrepareRender zwiazuje nas z aktualnym kontekstem OpenGL'a
      ale nie zmienia stanu OpenGL'a ani zawartosci jakiegos z buforow
      OpenGLa. Moze co najwyzej zajmowac jakies display-listy i tekstury OpenGLa.
      wywolywanie PrepareRender nigdy nie jest wymagane; problem polega jednak
      na tym ze samo wywolanie Render za pierwszym razem moze samo zainicjowac
      sobie mase rzeczy. To jest dobrze ze ta inicjalizacja nastepuje automatycznie
      ale czasami moze to byc bolesne ze gdy piszesz kod np. zdarzenia
      TGLWindow.OnDraw to za pierwszym razem to wywolanie moze zajac bardzo
      duzo czasu bo za pierwszym razem Render bedzie sobie chcialo bardzo
      duzo zainicjowac. Moze to byc niepozadane, przyklad patrz "Malfunction".
      Cel PrepareRender jest taki : niech najblizsze wywolanie Render zajmie
      mniej wiecej tyle samo czasu co wszystkie nastepne.

      If DoPrepareBackground then it will call PrepareBackground too.
      Then this function will make next call to Background proceed fast.

      If DoPrepareBoundingBox then it will also make sure that call to
      BoundingBox is fast. }
    procedure PrepareRender(DoPrepareBackground, DoPrepareBoundingBox,
      DoPrepareTrianglesListNotOverTriangulate,
      DoPrepareTrianglesListOverTriangulate: boolean);

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
          OpenGL technique : first we render all opaque objects and then
          we make depth-buffer read-only and then we render all partially
          trasparent objects. This technique has some important disadvantages
          if your OnDraw does not consist of only one call to Render, e.g.
          instead simple
@preformatted(
  Scene.Render;
)
          you have
@preformatted(
  Scene1.Render;
  Scene2.Render;
  + some other rendings, using TVRMLFlatSceneGL or not
)
          Basically, you should always render all opaque objects before
          all transparent objects. E.g. Scene2 can't have any opaque objects
          if Scene1 has some of them.
        ))

      @noAutoLinkHere
    }
    procedure Render(TestShapeStateVisibility: TTestShapeStateVisibility);

    { This calls Render passing TestShapeStateVisibility
      that tries to quickly eliminate ShapeStates that are entirely
      not within Frustum.
      In other words, this does so-called "frustum culling". }
    procedure RenderFrustum(const Frustum: TFrustum);

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
      Octree: TVRMLShapeStateOctree); overload;

    { This simply calls RenderFrustumOctree(Frustum, DefaultShapeStareOctree).
      Be sure that you assigned DefaultShapeStareOctree property before
      calling this. }
    procedure RenderFrustumOctree(const Frustum: TFrustum); overload;

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

    { This renders shadow volume of this scene.

      All shadow quads are generated from scene triangles
      transformed by TrianglesTransform. RenderFrontShadowQuads
      renders only quads front facing CameraPos,
      RenderBackShadowQuads renders the rest of the quads.

      Uses TrianglesList(false) (so you may prefer to prepare it
      before, e.g. by calling PrepareRender with
      DoPrepareTrianglesListNonOverTriangulate = true).

      All the commands passed to OpenGL by this methods are:
      glBegin, sequence of glVertex, then glEnd.

      When rendering front shadow quads, we actually calculate
      also back shadow quads. To RenderFrontShadowQuads you
      just pass instance of TDynQuad3SingleArray (don't care
      about it's contents, all will be initialized by RenderFrontShadowQuads).
      To RenderBackShadowQuads you must pass unmodified SavedShadowQuads
      as received by previous RenderFrontShadowQuads call.
      You can also use versions that don't take SavedShadowQuads argument
      at all --- they will just internally use TDynQuad3Single instance
      inside this object (so they are a little less flexible,
      and in some cases may unnecessarily waste memory).

      @groupBegin }
    procedure RenderFrontShadowQuads(
      const LightPos, CameraPos: TVector3Single;
      const TrianglesTransform: TMatrix4Single;
      SavedShadowQuads: TDynQuad3SingleArray); overload;

    procedure RenderBackShadowQuads(
      SavedShadowQuads: TDynQuad3SingleArray); overload;

    procedure RenderFrontShadowQuads(
      const LightPos, CameraPos: TVector3Single;
      const TrianglesTransform: TMatrix4Single); overload;

    procedure RenderBackShadowQuads; overload;
    { @groupEnd }
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
      And on the values of Attrib_ColorModulatorSingle/Byte.
      If there is no "Background" node in VRML scene this function returns nil.

      Note: this Background object is managed (automatically created/freed
      etc.) by this TVRMLFlatSceneGL object but it is NOT used anywhere
      in this class, e.g. Render does not call Background.Render. If you want to
      use this Background somehow, you have to do this yourself.

      The results of this function are internally cached. Cache is invalidated
      on such situations as change in RootNode scene, changes to
      BackgroundSkySphereRadius, CloseGL, Attrib_ColorModulatorSingle/Byte.

      PrepareBackground (and PrepareRender(true, ...)) automatically validate this
      cache.

      Remember that this cache is connected with the current OpenGL context.
      So you HAVE to call CloseGL to disconnent this object from
      current OpenGL context after you used this function. }
    function Background: TBackgroundGL;
  private
    FAttrib_Blending: boolean;
    procedure SetAttrib_Blending(value: boolean);
  public

    { -------------------------------------------------------------------
      Atrybuty renderowania. Wszystkie atrybuty renderowania mozesz
      zmieniac w dowolnym momencie (nawet atrybuty przekazywane do
      VRMLOpenGLRenderera; mimo ze VRMLOpenGLRenderer
      mial bardziej restrykcyjna polityke kiedy mozna bylo modyfikowac
      te atrybuty).

      Chociaz powinienes zdawac sobie sprawe z faktu ze zmiana wartosci
      atrybutu moze spowodowac ze nastepne
      wywolanie Render bedzie musialo sobie cos przeliczac od nowa
      (wywolanie PrepareRender moze miec sens w takiej sytuacji). }

    { Atrybuty renderowania realizowane w tym module }

    { jezeli true to elementy sceny z IsAllMaterialsTransparent beda
      rysowane uzywajac blending OpenGLa. Wpp. wszystko bedzie rysowane
      jako nieprzezroczyste. }
    property Attrib_Blending: boolean read FAttrib_Blending write
      SetAttrib_Blending; { = true }

    { Atrybuty renderowania przekazywane do VRMLOpenGLRenderera.
      Po opis znaczenia tych atrybutow patrz VRMLOpenGLRenderer;
      tam tez zobaczysz domyslne wartosci tych atrybutow. }
  private
    function GetAttrib_OnBeforeGLVertex: TBeforeGLVertexProc;
    function GetAttrib_SmoothShading: boolean;
    function GetAttrib_ColorModulatorSingle: TColorModulatorSingleFunc;
    function GetAttrib_ColorModulatorByte: TColorModulatorByteFunc;
    function GetAttrib_UseLights: boolean;
    function GetAttrib_FirstGLFreeLight: integer;
    function GetAttrib_LastGLFreeLight: integer;
    function GetAttrib_EnableTextures: boolean;
    function GetAttrib_TextureMinFilter: TGLint;
    function GetAttrib_TextureMagFilter: TGLint;
    function GetAttrib_PointSize: integer;
    function GetAttrib_UseFog: boolean;

    procedure SetAttrib_OnBeforeGLVertex(const Value: TBeforeGLVertexProc);
    procedure SetAttrib_SmoothShading(const Value: boolean);
    procedure SetAttrib_ColorModulatorSingle(const Value: TColorModulatorSingleFunc);
    procedure SetAttrib_ColorModulatorByte(const Value: TColorModulatorByteFunc);
    procedure SetAttrib_UseLights(const Value: boolean);
    procedure SetAttrib_FirstGLFreeLight(const Value: integer);
    procedure SetAttrib_LastGLFreeLight(const Value: integer);
    procedure SetAttrib_EnableTextures(const Value: boolean);
    procedure SetAttrib_TextureMinFilter(const Value: TGLint);
    procedure SetAttrib_TextureMagFilter(const Value: TGLint);
    procedure SetAttrib_PointSize(const Value: integer);
    procedure SetAttrib_UseFog(const Value: boolean);
  public
    property Attrib_OnBeforeGLVertex: TBeforeGLVertexProc read GetAttrib_OnBeforeGLVertex write SetAttrib_OnBeforeGLVertex;
    property Attrib_SmoothShading: boolean read GetAttrib_SmoothShading write SetAttrib_SmoothShading;

    { In addition to effects described at TVRMLOpenGLRenderer,
      Attrib_ColorModulatorSingle/Byte also affect what the
      TVRMLFlatSceneGL.Background function returns. }
    property Attrib_ColorModulatorSingle: TColorModulatorSingleFunc read GetAttrib_ColorModulatorSingle write SetAttrib_ColorModulatorSingle;
    property Attrib_ColorModulatorByte: TColorModulatorByteFunc read GetAttrib_ColorModulatorByte write SetAttrib_ColorModulatorByte;

    property Attrib_UseLights: boolean read GetAttrib_UseLights write SetAttrib_UseLights;
    property Attrib_FirstGLFreeLight: integer read GetAttrib_FirstGLFreeLight write SetAttrib_FirstGLFreeLight;
    property Attrib_LastGLFreeLight: integer read GetAttrib_LastGLFreeLight write SetAttrib_LastGLFreeLight;
    property Attrib_EnableTextures: boolean read GetAttrib_EnableTextures write SetAttrib_EnableTextures;
    property Attrib_TextureMinFilter: TGLint read GetAttrib_TextureMinFilter write SetAttrib_TextureMinFilter;
    property Attrib_TextureMagFilter: TGLint read GetAttrib_TextureMagFilter write SetAttrib_TextureMagFilter;
    property Attrib_PointSize: integer read GetAttrib_PointSize write SetAttrib_PointSize;
    property Attrib_UseFog: boolean read GetAttrib_UseFog write SetAttrib_UseFog;
  end;

{ Parses and removes from Parameters[1]..Parameters.High
  parameter @--renderer-optimization, and sets RendererOptimization
  to the value specified by user.
  See view3dscene documentation
  [http://camelot.homedns.org/~michalis/view3dscene.php] for description. }
procedure RendererOptimizationOptionsParse(
  var RendererOptimization: TGLRendererOptimization);

{ Describe what parameters are parsed by RendererOptimizationOptionsParse.
  This is nice to use e.g. in help text (e.g. the one printed in response
  to "@--help" command-line parameter). }
function RendererOptimizationOptionsHelp: string;

type
  TObjectsListItem_1 = TVRMLFlatSceneGL;
  {$I objectslist_1.inc}
  TVRMLFlatSceneGLsList = class(TObjectsList_1)
    procedure CloseGLAll;
  end;

{$undef read_interface}

implementation

uses ParseParametersUnit;

{$define read_implementation}
{$I objectslist_1.inc}
{$I dynarray_1.inc}

procedure TVRMLFlatSceneGLsList.CloseGLAll;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   if Items[I] <> nil then
     Items[I].CloseGL;
end;

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
  [http://www.talisman.org/opengl-1.1/ImpGuide/05_WriteProg.html#GLCOMPILEandEXECUTEMode]

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

{ TVRMLFlatSceneGL ---------------------------------------- }

procedure TVRMLFlatSceneGL.CommonCreate(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization;
  AUsingProvidedRenderer: boolean;
  AProvidedRenderer: TVRMLOpenGLRenderer);
begin
  { inherited Create calls ChangedAll that is overriden in this class
    and uses SSS_DisplayLists, RenderFrustumOctree_Visible, Optimization.
    That's why I have to init them *before* "inherited Create" }

  FOptimization := AOptimization;

  if Optimization = roSeparateShapeStates then
   SSS_DisplayLists := TDynGLuintArray.Create;

  RenderFrustumOctree_Visible := TDynBooleanArray.Create;

  inherited Create(ARootNode, AOwnsRootNode);

  FAttrib_Blending := true;

  FBackgroundSkySphereRadius := 1.0;
  FBackgroundValid := false;
  FBackground := nil;

  FUsingProvidedRenderer := AUsingProvidedRenderer;

  DefaultSavedShadowQuads := TDynQuad3SingleArray.Create;

  if FUsingProvidedRenderer then
    Renderer := AProvidedRenderer else
    Renderer := TVRMLOpenGLRenderer.Create;
end;

constructor TVRMLFlatSceneGL.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization);
begin
  CommonCreate(ARootNode, AOwnsRootNode, AOptimization, false, nil);
end;

constructor TVRMLFlatSceneGL.CreateProvidedRenderer(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AOptimization: TGLRendererOptimization; AProvidedRenderer: TVRMLOpenGLRenderer);
begin
  CommonCreate(ARootNode, AOwnsRootNode, AOptimization, true, AProvidedRenderer);
end;

destructor TVRMLFlatSceneGL.Destroy;
begin
 CloseGL;

 if not FUsingProvidedRenderer then
   FreeAndNil(Renderer);

 FreeAndNil(SSS_DisplayLists);
 FreeAndNil(RenderFrustumOctree_Visible);

 FreeAndNil(DefaultSavedShadowQuads);

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
var ShapeStateNum: Integer;
begin
 case Optimization of
  roSceneAsAWhole:
    glFreeDisplayList(SAAW_DisplayList);
  roSeparateShapeStates:
    begin
     { Because CloseGLRenderer may be called after scene has changed
       and after "inherited ChangedAll" changed ShapeStates.Count to
       reflect this change but before our ChangedAll changed
       SSS_DisplayLists.Length (after all, CloseGLRenderer must be
       called before changing SSS_DisplayLists.Length, since CloseGLRenderer
       must finalize what was left) ... so, I can't assume here that
       ShapeStates.Count = SSS_DisplayLists.Count (like I do in many
       other places in this unit). So below I must iterate to
       "SSS_DisplayLists.Count - 1", *not* to "ShapeStates.Count - 1". }
     for ShapeStateNum := 0 to SSS_DisplayLists.Count - 1 do
      glFreeDisplayList(SSS_DisplayLists.Items[ShapeStateNum]);

     glFreeDisplayList(SSS_RenderBeginDisplayList);
     glFreeDisplayList(SSS_RenderEndDisplayList);
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
 Renderer.Render(ShapeStates[ShapeStateNum].ShapeNode,
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

procedure TVRMLFlatSceneGL.RenderShapeStatesNoDispList(
  TestShapeStateVisibility: TTestShapeStateVisibility;
  RenderShapeStateProc: TRenderShapeState;
  RenderBeginProc, RenderEndProc: TObjectProcedure);

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
     partially transparent obiektow zachodzi na siebie. }

   glDepthMask(GL_TRUE);
   glDisable(GL_BLEND);
   if Attrib_Blending then
   begin
    { uzywamy zmiennej TransparentObjectsExists aby ew. (jesli na scenie
      nie ma zadnych obiektow ktore chcemy renderowac z blending)
      zaoszczedzic czas i nie robic zmian stanu OpenGLa glDepthMask(GL_FALSE);
      itd. i nie iterowac ponownie po liscie ShapeStates }
    TransparentObjectsExists := false;

    { draw fully opaque objects }
    for ShapeStateNum := 0 to ShapeStates.Count - 1 do
     if not ShapeStates[ShapeStateNum].
       State.LastNodes.Material.IsAllMaterialsTransparent then
      TestRenderShapeStateProc(ShapeStateNum) else
      TransparentObjectsExists := true;

    { draw partially transparent objects }
    if TransparentObjectsExists then
    begin
     glDepthMask(GL_FALSE);
     glEnable(GL_BLEND);
     { Wada GL_ONE jest fakt ze wynikowy obraz bedzie bardzo jasny
       tam gdzie widoczne sa obiekty blended. (bo GL_ONE zawsze tylko
       zwieksza kolor obrazu).
       Natomiast wada GL_ONE_MINUS_SRC_ALPHA jest fakt ze z wynikowego
       obrazu moze za szybko zniknac kolor obiektu nie-blended ktory
       byl za obiektami blended (bo GL_ONE_MINUS_SRC_ALPHA bedzie
       go za kazdym razem zmniejszala). }
     glBlendFunc(GL_SRC_ALPHA, GL_ONE {_MINUS_SRC_ALPHA});
     for ShapeStateNum := 0 to ShapeStates.Count - 1 do
      if ShapeStates[ShapeStateNum].
        State.LastNodes.Material.IsAllMaterialsTransparent then
       TestRenderShapeStateProc(ShapeStateNum);
    end;
   end else
   begin
    for ShapeStateNum := 0 to ShapeStates.Count - 1 do
     TestRenderShapeStateProc(ShapeStateNum);
   end;

  finally glPopAttrib end;
 finally RenderEndProc end;
end;

procedure TVRMLFlatSceneGL.SSS_PrepareAndMaybeRenderBegin(Mode: TGLenum);
begin
 SSS_RenderBeginDisplayList := glGenLists(1);
 glNewList(SSS_RenderBeginDisplayList, Mode);
 try
  RenderBeginSimple;
 finally glEndList end;
end;

procedure TVRMLFlatSceneGL.SSS_PrepareAndMaybeRenderEnd(Mode: TGLenum);
begin
 SSS_RenderEndDisplayList := glGenLists(1);
 glNewList(SSS_RenderEndDisplayList, Mode);
 try
  RenderEndSimple;
 finally glEndList end;
end;

procedure TVRMLFlatSceneGL.SSS_RenderBegin;
begin
 if SSS_RenderBeginDisplayList = 0 then
  SSS_PrepareAndMaybeRenderBegin({GL_COMPILE_AND_EXECUTE}GL_COMPILE);
 glCallList(SSS_RenderBeginDisplayList);
end;

procedure TVRMLFlatSceneGL.SSS_RenderEnd;
begin
 if SSS_RenderEndDisplayList = 0 then
  SSS_PrepareAndMaybeRenderEnd({GL_COMPILE_AND_EXECUTE}GL_COMPILE);
 glCallList(SSS_RenderEndDisplayList);
end;

procedure TVRMLFlatSceneGL.SSS_PrepareRenderAndMaybeRender(
  Mode: TGLenum; ShapeStateNum: Integer);
begin
 Renderer.Prepare(ShapeStates[ShapeStateNum].State);

 SSS_DisplayLists.Items[ShapeStateNum] := glGenLists(1);
 glNewList(SSS_DisplayLists.Items[ShapeStateNum], Mode);
 try
  RenderShapeStateSimple(ShapeStateNum);
 finally glEndList end;
end;

procedure TVRMLFlatSceneGL.SSS_PrepareAndRenderShapeState(
  ShapeStateNum: Integer);
begin
 if SSS_DisplayLists.Items[ShapeStateNum] = 0 then
  SSS_PrepareRenderAndMaybeRender({GL_COMPILE_AND_EXECUTE}GL_COMPILE,
    ShapeStateNum);
 glCallList(SSS_DisplayLists.Items[ShapeStateNum]);
end;

procedure TVRMLFlatSceneGL.SAAW_PrepareRenderAndMaybeRender(Mode: TGLenum);
var i: Integer;
begin
 { First prepare all (because I can't later call Renderer.Prepare
   while being inside display-list) }
 for i := 0 to ShapeStates.Count - 1 do
  Renderer.Prepare(ShapeStates[i].State);

 SAAW_DisplayList := glGenLists(1);
 glNewList(SAAW_DisplayList, mode);
 try
  RenderShapeStatesNoDispList(nil, RenderShapeStateSimple,
    RenderBeginSimple, RenderEndSimple);
 finally glEndList end;
end;

procedure TVRMLFlatSceneGL.PrepareRender(
  DoPrepareBackground, DoPrepareBoundingBox,
  DoPrepareTrianglesListNotOverTriangulate,
  DoPrepareTrianglesListOverTriangulate: boolean);
var ShapeStateNum: Integer;
begin
 case Optimization of
  roSceneAsAWhole:
    if SAAW_DisplayList = 0 then
     SAAW_PrepareRenderAndMaybeRender(GL_COMPILE);
  roSeparateShapeStates:
    begin
     { build display lists (if needed) for all shape states }
     for ShapeStateNum := 0 to ShapeStates.Count - 1 do
      if SSS_DisplayLists.Items[ShapeStateNum] = 0 then
       SSS_PrepareRenderAndMaybeRender(GL_COMPILE, ShapeStateNum);

     if SSS_RenderBeginDisplayList = 0 then
      SSS_PrepareAndMaybeRenderBegin(GL_COMPILE);

     if SSS_RenderEndDisplayList = 0 then
      SSS_PrepareAndMaybeRenderEnd(GL_COMPILE);
    end;
 end;

 if DoPrepareBackground then PrepareBackground;

 if DoPrepareBoundingBox then BoundingBox; { ignore the result }

 if DoPrepareTrianglesListNotOverTriangulate then
   TrianglesList(false);

 if DoPrepareTrianglesListOverTriangulate then
   TrianglesList(true);
end;

procedure TVRMLFlatSceneGL.Render(
  TestShapeStateVisibility: TTestShapeStateVisibility);
begin
 case Optimization of
  roNone:
    begin
     RenderShapeStatesNoDispList(TestShapeStateVisibility,
       PrepareAndRenderShapeStateSimple, RenderBeginSimple, RenderEndSimple);
    end;
  roSceneAsAWhole:
    begin
     if SAAW_DisplayList = 0 then
      SAAW_PrepareRenderAndMaybeRender({GL_COMPILE_AND_EXECUTE}GL_COMPILE) else
     begin
      { In this case I must directly set here LastRender_Xxx variables. }
      FLastRender_AllShapeStatesCount := ShapeStates.Count;
      FLastRender_RenderedShapeStatesCount := FLastRender_AllShapeStatesCount;
     end;
     glCallList(SAAW_DisplayList);
    end;
  roSeparateShapeStates:
    begin
     { build display lists (if needed) and render all shape states }
     RenderShapeStatesNoDispList(TestShapeStateVisibility,
       SSS_PrepareAndRenderShapeState, SSS_RenderBegin, SSS_RenderEnd);
    end;
 end;
end;

procedure TVRMLFlatSceneGL.ChangedAll;
begin
 inherited;

 { zmienily sie wskazniki na jakies obiekty,
   wiec musimy zrobic pelne UnprepareAll,
   mimo ze nie zalezy nam na utracie polaczenia z danym kontekstem OpenGL'a.
   Podobnie SAAW_DisplayList lub SSS_DisplayLists sa juz nieaktualne
   wiec ich tez musimy sie pozbyc. Wiec trzeba wywolac po prostu CloseGL. }
 CloseGL;

 if Optimization = roSeparateShapeStates then
 begin
  SSS_DisplayLists.Count := ShapeStates.Count;

  { Yeah, in previous CloseGL call we also resetted all
    SSS_DisplayLists items to 0
    (as a side-effect of calling glFreeDisplayList),
    but previous statement "SSS_DisplayLists.Count := ..."
    possibly enlarged SSS_DisplayLists.Count,
    so we must now make sure that all new items are inited to 0. }

  SSS_DisplayLists.SetAll(0);
 end;

 RenderFrustumOctree_Visible.Count := ShapeStates.Count;
end;

procedure TVRMLFlatSceneGL.ChangedShapeStateFields(ShapeStateNum: integer);
begin
 inherited;

 { nie musimy tu robic nigdy Renderer.Unprepare*, bo przeciez obiekty node'ow
   sie nie zmienily, tylko ich pola. Zwracam uwage ze w ten sposob gdy
   Optimization = roNone to w tej procedurze nie musimy NIC robic - a wiec
   jest to jakis zysk gdy uzywamy roNone. }

 case Optimization of
  roSceneAsAWhole:
    glFreeDisplayList(SAAW_DisplayList);
  roSeparateShapeStates:
    { TODO -- test this }
    glFreeDisplayList(SSS_DisplayLists.Items[ShapeStateNum]);
 end;
end;

{ shadow quads --------------------------------------------------------------- }

procedure TVRMLFlatSceneGL.RenderFrontShadowQuads(
  const LightPos, CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single;
  SavedShadowQuads: TDynQuad3SingleArray);

{ Zaklada ze wsrod podanych trojkatow wszystkie sa valid (tzn. nie ma
  zdegenerowanych trojkatow). To jest wazne zeby zagwarantowac to
  (TrianglesList gwarantuje to)
  bo inaczej zdegenerowane trojkaty moga sprawic ze wynik renderowania
  bedzie nieprawidlowy (pojawia sie na ekranie osobliwe "paski cienia"
  powstale w wyniku zdegenerowanych trojkatow dla ktorych wszystkie 3 sciany
  zostaly uznane za "front facing"). }

  { Let SQ = shadow quad constructed by extending P0 and P1 by lines
    from LightPos. POther is given here as a reference of the "inside"
    part of triangle: let P = plane formed by P0, P1 and LightPos,
    if CameraPos is on the same side of plane P as POther then
    SQ is back-facing, else SQ is front-facing.
    Let SQFront:="is SQ front facing".
    If SQFront = Front then this procedure renders SQ, else is does not. }
  procedure MaybeRenderShadowQuad(const P0, P1, POther,
    PExtruded0, PExtruded1: TVector3Single);
  var
    SQFront: boolean;
    P: TVector4Single;
    QuadPtr: PQuad3Single;
  begin
    P := TrianglePlane(P0, P1, LightPos);
    SQFront := not PointsSamePlaneSides(POther, CameraPos, P);

    if SQFront then
    begin
      glVertexv(P0);
      glVertexv(P1);
      glVertexv(PExtruded1);
      glVertexv(PExtruded0);
    end else
    begin
      SavedShadowQuads.IncLength;
      QuadPtr := SavedShadowQuads.Pointers[SavedShadowQuads.High];
      QuadPtr[0] := P0;
      QuadPtr[1] := P1;
      QuadPtr[2] := PExtruded1;
      QuadPtr[3] := PExtruded0;
    end;
  end;

const
  { TODO: wartosc 1000 jest tu dobrana "ot tak".

    Bo w teorii shadow quad ma nieskonczona powierzchnie.
    Rozwiazac ten problem - mozna podawac max rozmiar modelu sceny parametrem
    ale przeciez wtedy powstanie problem ze bedzie trzeba dodac
    jakies normalizacje do kodu RenderShadowQuads a wiec strata szybkosci
    na bzdure.

    Mozna kombinowac z robieniem sztuczek zeby renderowac nieskonczony
    shadow volume (bo vertex jest de facto 4D, nie 3D, dla OpenGLa). }
  MakeInfinite = 1000;

var
  I: Integer;
  Triangles: TDynTriangle3SingleArray;
  T0, T1, T2, TExtruded0, TExtruded1, TExtruded2: TVector3Single;
begin
  Triangles := TrianglesList(false);

  SavedShadowQuads.Count := 0;
  SavedShadowQuads.AllowedCapacityOverflow := Triangles.Count * 3;

  glBegin(GL_QUADS);
    for I := 0 to Triangles.Count - 1 do
    begin
      { evaluate T := Triangles[I] transformed by TrianglesTransform }
      T0 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][0]);
      T1 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][1]);
      T2 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][2]);

      TExtruded0 := VectorAdd(VectorScale(VectorSubtract(T0, LightPos), MakeInfinite), T0);
      TExtruded1 := VectorAdd(VectorScale(VectorSubtract(T1, LightPos), MakeInfinite), T1);
      TExtruded2 := VectorAdd(VectorScale(VectorSubtract(T2, LightPos), MakeInfinite), T2);

      MaybeRenderShadowQuad(T0, T1, T2, TExtruded0, TExtruded1);
      MaybeRenderShadowQuad(T1, T2, T0, TExtruded1, TExtruded2);
      MaybeRenderShadowQuad(T2, T0, T1, TExtruded2, TExtruded0);
    end;
  glEnd;
end;

procedure TVRMLFlatSceneGL.RenderBackShadowQuads(
  SavedShadowQuads: TDynQuad3SingleArray);
var
  I: Integer;
begin
  { That's brutally simple, just render all the quads. }
  glBegin(GL_QUADS);
    for I := 0 to SavedShadowQuads.High do
    begin
      glVertexv(SavedShadowQuads.Items[I][0]);
      glVertexv(SavedShadowQuads.Items[I][1]);
      glVertexv(SavedShadowQuads.Items[I][2]);
      glVertexv(SavedShadowQuads.Items[I][3]);
    end;
  glEnd;
end;

procedure TVRMLFlatSceneGL.RenderFrontShadowQuads(
  const LightPos, CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single);
begin
  RenderFrontShadowQuads(LightPos, CameraPos, TrianglesTransform,
    DefaultSavedShadowQuads);
end;

procedure TVRMLFlatSceneGL.RenderBackShadowQuads;
begin
  RenderBackShadowQuads(DefaultSavedShadowQuads);
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

procedure TVRMLFlatSceneGL.RenderFrustum(const Frustum: TFrustum);
begin
 RenderFrustum_Frustum := @Frustum;
 Render(RenderFrustum_TestShapeState);
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
  Octree: TVRMLShapeStateOctree);
begin
 if Optimization <> roSceneAsAWhole then
 begin
  RenderFrustumOctree_Frustum := @Frustum;

  RenderFrustumOctree_Visible.SetAll(false);
  Octree.EnumerateCollidingOctreeItems(Frustum,
    RenderFrustumOctree_EnumerateOctreeItem);
  Render(RenderFrustumOctree_TestShapeState);
 end else
  Render(nil);
end;

procedure TVRMLFlatSceneGL.RenderFrustumOctree(const Frustum: TFrustum);
begin
 RenderFrustumOctree(Frustum, DefaultShapeStateOctree);
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
var InitialState: TVRMLGraphTraverseState;
    BgTransform: TMatrix4Single;
    BgNode: TNodeBackground;
begin
 if FBackgroundValid then Exit;

 InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
 try
  if (RootNode <> nil) and
    RootNode.TryFindNodeTransform(InitialState, TNodeBackground,
      TVRMLNode(BgNode), BgTransform) then
  begin
   { TODO - extract only rotation from BgTransform matrix ! }
   BgNode.SetAllowedBgImagesClasses(GLImageClasses);
   FBackground := TBackgroundGL.Create(BgTransform,
     @(BgNode.FdGroundAngle.Items.Items[0]), BgNode.FdGroundAngle.Count,
     @(BgNode.FdGroundColor.Items.Items[0]), BgNode.FdGroundColor.Count,
     BgNode.BgImages,
     @(BgNode.FdSkyAngle.Items.Items[0]), BgNode.FdSkyAngle.Count,
     @(BgNode.FdSkyColor.Items.Items[0]), BgNode.FdSkyColor.Count,
     BackgroundSkySphereRadius,
     Attrib_ColorModulatorSingle,
     Attrib_ColorModulatorByte);
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

{ atrybuty realizowane w tym module ---------------------------------------- }

{$define PROPERTY_SET_WITH_CLOSE_GL_RENDERER:=
begin
 if PROPERTY_PRIVATE_NAME <> Value then
 begin
  CloseGLRenderer;
  PROPERTY_PRIVATE_NAME := Value;
 end;
end;}

{$define PROPERTY_FUNC_SET_WITH_CLOSE_GL_RENDERER:=
begin
 if @PROPERTY_FUNC_PRIVATE_NAME <> @Value then
 begin
  CloseGLRenderer;
  PROPERTY_FUNC_PRIVATE_NAME := Value;
 end;
end;}

procedure TVRMLFlatSceneGL.SetAttrib_Blending(value: boolean);
{$define PROPERTY_PRIVATE_NAME := FAttrib_Blending}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

{ --------------------------------------------------------------------------------
  atrybuty przekazywane do VRMLOpenGLRenderera - procedury Get/Set do nich;
  Interfejs Renderera mowi ze zeby zmienic atrybut renderer musi byc wolny
  od aktualnego kontekstu OpenGLa, wiec musimy przed zmiana atrybutu
  wywolac przynajmniej Renderer.UnprepareAll.

  Prawda jest taka ze my tez musimy byc wolni - nie mozemy miec zadnych
  przeliczonych display-list, nic takiego, bo wlasnie zmiana Attrib_ renderera
  moze te rzeczy zdezaktualizowac - z innymi attrib renderer bedzie dawal
  co innego.

  Wiec kazda zmiana atrybutu musi byc poprzedzona CloseGLRenderer, wiec
  uzywamy makr PROPERTY_SET_WITH_CLOSE_GL_RENDERER. }

procedure TVRMLFlatSceneGL.SetAttrib_OnBeforeGLVertex(const Value: TBeforeGLVertexProc);
{$define PROPERTY_FUNC_PRIVATE_NAME := Renderer.Attrib_OnBeforeGLVertex}
PROPERTY_FUNC_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_SmoothShading(const Value: boolean);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_SmoothShading}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_ColorModulatorSingle(const Value: TColorModulatorSingleFunc);
{$define PROPERTY_FUNC_PRIVATE_NAME := Renderer.Attrib_ColorModulatorSingle}
begin
 FBackgroundInvalidate;
 PROPERTY_FUNC_SET_WITH_CLOSE_GL_RENDERER
end;

procedure TVRMLFlatSceneGL.SetAttrib_ColorModulatorByte(const Value: TColorModulatorByteFunc);
{$define PROPERTY_FUNC_PRIVATE_NAME := Renderer.Attrib_ColorModulatorByte}
begin
 FBackgroundInvalidate;
 PROPERTY_FUNC_SET_WITH_CLOSE_GL_RENDERER
end;

procedure TVRMLFlatSceneGL.SetAttrib_UseLights(const Value: boolean);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_UseLights}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_FirstGLFreeLight(const Value: integer);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_FirstGLFreeLight}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_LastGLFreeLight(const Value: integer);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_LastGLFreeLight}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_EnableTextures(const Value: boolean);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_EnableTextures}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_TextureMinFilter(const Value: TGLint);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_TextureMinFilter}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_TextureMagFilter(const Value: TGLint);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_TextureMagFilter}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_PointSize(const Value: integer);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_PointSize}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

procedure TVRMLFlatSceneGL.SetAttrib_UseFog(const Value: boolean);
{$define PROPERTY_PRIVATE_NAME := Renderer.Attrib_UseFog}
PROPERTY_SET_WITH_CLOSE_GL_RENDERER

function TVRMLFlatSceneGL.GetAttrib_OnBeforeGLVertex: TBeforeGLVertexProc; begin result := Renderer.Attrib_OnBeforeGLVertex end;
function TVRMLFlatSceneGL.GetAttrib_SmoothShading: boolean; begin result := Renderer.Attrib_SmoothShading end;
function TVRMLFlatSceneGL.GetAttrib_ColorModulatorSingle: TColorModulatorSingleFunc; begin result := Renderer.Attrib_ColorModulatorSingle end;
function TVRMLFlatSceneGL.GetAttrib_ColorModulatorByte: TColorModulatorByteFunc; begin result := Renderer.Attrib_ColorModulatorByte end;
function TVRMLFlatSceneGL.GetAttrib_UseLights: boolean; begin result := Renderer.Attrib_UseLights end;
function TVRMLFlatSceneGL.GetAttrib_FirstGLFreeLight: integer; begin result := Renderer.Attrib_FirstGLFreeLight end;
function TVRMLFlatSceneGL.GetAttrib_LastGLFreeLight: integer; begin result := Renderer.Attrib_LastGLFreeLight end;
function TVRMLFlatSceneGL.GetAttrib_EnableTextures: boolean; begin result := Renderer.Attrib_EnableTextures end;
function TVRMLFlatSceneGL.GetAttrib_TextureMinFilter: TGLint; begin result := Renderer.Attrib_TextureMinFilter end;
function TVRMLFlatSceneGL.GetAttrib_TextureMagFilter: TGLint; begin result := Renderer.Attrib_TextureMagFilter end;
function TVRMLFlatSceneGL.GetAttrib_PointSize: integer; begin result := Renderer.Attrib_PointSize end;
function TVRMLFlatSceneGL.GetAttrib_UseFog: boolean; begin result := Renderer.Attrib_UseFog end;

{ non-object routines -------------------------------------------------- }

const
  RendererOptimizationNames: array[TGLRendererOptimization] of string =
  ( 'none', 'scene-as-a-whole', 'separate-shape-states' );

  {$define ARRAY_POS_FUNCTION_NAME := RendererOptimizationFromName}
  {$define ARRAY_POS_ARRAY_NAME := RendererOptimizationNames}
  {$define ARRAY_POS_INDEX_TYPE := TGLRendererOptimization}
  {$I macarraypos.inc}
  IMPLEMENT_ARRAY_POS_CASE_CHECKED

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var RendererOptimizationPtr: PGLRendererOptimization absolute Data;
begin
 case OptionNum of
  0: RendererOptimizationPtr^ := RendererOptimizationFromName(Argument, true);
  else raise EInternalError.Create('VRMLFlatSceneGL.OptionProc: OptionNum');
 end;
end;

procedure RendererOptimizationOptionsParse(
  var RendererOptimization: TGLRendererOptimization);
const
  Options: array[0..0] of TOption =
  ( (Short: #0; Long: 'renderer-optimization'; Argument: oaRequired)
  );
begin
 ParseParameters(Options, OptionProc, @RendererOptimization, true);
end;

function RendererOptimizationOptionsHelp: string;
var ro: TGLRendererOptimization;
begin
 Result :=
   '  --renderer-optimization ' +
   RendererOptimizationNames[Low(TGLRendererOptimization)];
 for ro := Succ(Low(ro)) to High(ro) do
  Result += '|' + RendererOptimizationNames[ro];
 Result += nl+
   '                        Set rendering optimization kind.';
end;

end.