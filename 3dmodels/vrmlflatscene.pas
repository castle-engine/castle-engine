{
  Copyright 2003-2007 Michalis Kamburelis.

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

{ @abstract(@link(TVRMLFlatScene) class.) }

unit VRMLFlatScene;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3d, VRMLNodes, KambiClassUtils, KambiUtils,
  VRMLShapeState, VRMLTriangleOctree, ProgressUnit, VRMLShapeStateOctree;

{$define read_interface}

type
  TDynArrayItem_1 = TTriangle3Single;
  PDynArrayItem_1 = PTriangle3Single;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Triangle3Single = TInfiniteArray_1;
  PArray_Triangle3Single = PInfiniteArray_1;
  TDynTriangle3SingleArray = TDynArray_1;

  { Internal helper type for TVRMLFlatScene.
    @exclude }
  TVRMLFlatSceneValidity = (fvBBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvFog,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvManifoldEdges);

  { @exclude }
  TVRMLFlatSceneValidities = set of TVRMLFlatSceneValidity;

  TViewpointFunction = procedure (Node: TNodeGeneralViewpoint;
    const Transform: TMatrix4Single) of object;

  { This represents scene edge. It's used by @link(TVRMLFlatScene.ManifoldEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TManifoldEdge = record
    { Index to get vertexes of this edge.
      The actual edge's vertexes are not recorded here (this would prevent
      using TVRMLFlatScene.ShareManifoldEdges with various scenes from
      the same animation). You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the first triangle
      (i.e. Triangles[0]). }
    VertexIndex: Cardinal;
    { Indexes to TVRMLFlatScene.Triangles(false) array }
    Triangles: array [0..1] of Cardinal;
    TrianglesCount: Cardinal;
  end;
  PManifoldEdge = ^TManifoldEdge;

  TDynArrayItem_2 = TManifoldEdge;
  PDynArrayItem_2 = PManifoldEdge;
  {$define DYNARRAY_2_IS_STRUCT}
  {$I dynarray_2.inc}

  TDynManifoldEdgeArray = class(TDynArray_2)
  private
    { Private for TVRMLFlatScene.ManifoldEdges.
      Adds given edge to the list. If the edge already exists and
      has 2 triangles, returns @false since the scene is not a manifold. }
    function AddEdgeCheckManifold(
      const TriangleIndex: Cardinal;
      const V0: TVector3Single;
      const V1: TVector3Single;
      const VertexIndex: Cardinal;
      Triangles: TDynTriangle3SingleArray): boolean;
  end;

  { This class represents a VRML scene (that is, graph of VRML nodes
    rooted in RootNode) deconstructed to a list of @link(TVRMLShapeState)
    objects. The basic idea is to "have" at the same time hierarchical
    view of the scene (in @link(RootNode)) and a flattened view of the same scene
    (in @link(ShapeStates) list).

    Unfortunately this means that things get a little more complicated
    if you want to dynamically change the scene.
    Although whole @link(TVRMLNode) class works very nicely and you
    can freely change any part of it, add, delete and move nodes around
    and change their properties, this is no longer so easy with
    @link(TVRMLFlatScene). Basically you can just call @link(ChangedAll)
    after changing some things inside @link(RootNode), but you should
    also take a look at other @code(Changed*) methods defined here.

    In exchange, this class provides many functionality and most
    things work very quickly if the scene is more-or-less static.
    E.g. methods [Local]BoundingBox, Vertices/TrianglesCount cache
    their results so after the first call to @link(TrianglesCount)
    next calls to the same method will return instantly (assuming
    that scene did not changed much). And the @link(ShapeStates) list
    is the key to various processing of the scene, most important
    it's the key to write a flexible OpenGL renderer of the VRML scene.
  }
  TVRMLFlatScene = class
  private
    FOwnsRootNode: boolean;
    FShapeStates: TVRMLShapeStatesList;
    FRootNode: TVRMLNode;
    procedure AddFromTraverse(Node: TVRMLNode; State: TVRMLGraphTraverseState);

    FFogNode: TNodeFog;
    FFogDistanceScaling: Single;
    { calculate FFogNode and FFogDistanceScaling, include fvFog
      in Validities }
    procedure ValidateFog;

    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLFlatSceneValidities;
    function CalculateBoundingBox: TBox3d;
    function CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
    function CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;

    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);

    FDefaultShapeStateOctree: TVRMLShapeStateOctree;
    FDefaultTriangleOctree: TVRMLTriangleOctree;
    FOwnsDefaultTriangleOctree: boolean;
    FOwnsDefaultShapeStateOctree: boolean;

    { If appropriate fvXxx is not in Validities, then
      - free if needed appropriate FTrianglesList[] item
      - calculate appropriate FTrianglesList[] item
      - add appropriate fvXxx to Validities. }
    procedure ValidateTrianglesList(OverTriangulate: boolean);
    FTrianglesList:
      array[boolean { OverTriangulate ?}] of TDynTriangle3SingleArray;

    function GetViewpointCore(
      const OnlyPerspective: boolean;
      out CamKind: TVRMLCameraKind;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single):
      TNodeGeneralViewpoint;

    FManifoldEdges: TDynManifoldEdgeArray;
    FOwnsManifoldEdges: boolean;
  public
    { @noAutoLinkHere }
    destructor Destroy; override;

    { @noAutoLinkHere }
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);

    { ShapeStates contents are read-only from outside.

      Note that the only place where ShapeStates length is changed
      in this class is ChangedAll procedure.
      So e.g. if you want to do something after each change of
      ShapeStates length, you can simply override ChangedAll
      and do your work after calling "inherited". }
    property ShapeStates: TVRMLShapeStatesList read FShapeStates;

    { specyfikacja co robia BoundingBox, VerticesCount i TrianglesCount -
      patrz VRMLNodes.TNodeGenaralShape, tylko tutaj sumujemy je
      dla wszystkich ShapeStates }
    function BoundingBox: TBox3d;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;

    { ta klasa zapamietuje sobie pewne przeliczone rzeczy w swoich polach,
      w zwiazku z czym musi byc powiadamiana gdy dokonasz jakichs zmian w
      strukturze RootNode.

      Wywolaj ChangedAll gdy zmieniles po prostu cokolwiek - zupelnie cala
      scene VRML'a zapisana w RootNode (dodales/usunales/przesunales jakies
      node'y), zupelnie dowolne pola dowolnych node'ow.

      Wywolaj ChangedShapeStateFields(i) gdy zmieniles tylko zawartosci pol
      node'a ShapeList[i].ShapeNode, node'ow ShapeList[i].State.Last* i
      node'ow ShapeList[i].State.Active*. (i jestes PEWIEN ze node ktorego
      pole zmieniles nie wystepuje w innych ShapeState'ach).

      Wywolaj ChangedFields gdy zmieniles pola danego Node'a. Mozesz podac
      Node ktory jest lub ktorego nie ma w aktualnym grafie VRML'a zaczepionym
      w RootNode, moze byc w czesci aktywnej lub nieaktywnej (takiej do ktorej
      nie dociera Traverse) tego grafu, moze byc Node'm dowolnej klasy,
      moze byc takze jednym z Node'ow StateDefaultNodes ktory wziales z
      jakiegos State.LastNodes[] - to wszystko tutaj uwzglednimy i wywolanie
      ChangedFields rozlozy sie na wywolania innych Changed.
      To jest prawdopodobnie najwygodniejsze sposrod Changed*.

      Notka dla implementatorow podklas : jak widac ChangedAll i
      ChangedShapeStateFields  sa wirtualne i moga byc pokryte. Oczywiscie
      musisz wywolac inherited w podklasie. ChangedAll jest wywolywane
      pod koniec konstruktora w tej klasie, wiec wystarczy ze cala inicjalizacje
      umiescisz w ChangedAll. }
    procedure ChangedAll; virtual;
    procedure ChangedShapeStateFields(ShapeStateNum: Integer); virtual;
    procedure ChangedFields(Node: TVRMLNode);

    { Returns short information about the scene.
      This consists of a few lines, separated by KambiUtils.NL.
      Last line also ends with KambiUtils.NL. }
    function Info(InfoTriVertCounts, InfoBoundingBox: boolean): string;

    { Write contents of all VRML "Info" nodes.
      Also write how many Info nodes there are in the scene. }
    procedure WritelnInfoNodes;

    { This class can be viewed as a wrapper around specified RootNode.

      As such it is allowed to change contents of RootNode
      (however, this object requires notification of such changes
      via ChangedXxx methods, see their docs).
      Moreover it is allowed to change value of RootNode
      and even to set RootNode to nil for some time.

      This is useful for programs like view3dscene, that want to
      have one TVRMLFlatScene for all the lifetime and only
      replace RootNode value from time to time.
      This is useful because it allows to change viewed model
      (by changing RootNode) while preserving values of things
      like Attributes properties in subclass @link(TVRMLFlatSceneGL).

      That's why it is possible to change RootNode and it is even
      possible to set it to nil. And when When RootNode = nil everything
      should work -- you can query such scene (with RootNode = nil)
      for Vecrtices/TrianglesCount (answer will be 0),
      for BoundingBox (answer will be EmptyBox3d),
      you can render such scene (nothing will be rendered) etc.
      Scene RootNode = nil will act quite like a Scene with
      e.g. no TNodeGeneralShape nodes.

      Always call ChangedAll when you changed RootNode.

      Note that there is also a trick to conserve memory use.
      After you've done PrepareRender some things are precalculated here,
      and RootNode is actually not used. So you can free RootNode
      (and set it to nil here) @italic(without calling ChangedAll)
      and some things will just continue to work, unaware of the fact
      that the underlying RootNode structure is lost.
      Note that this is still considered a "trick", and you will
      have to be extra-careful then about what methods/properties
      from this class. Generally, use only things that you prepared
      with PrepareRender. So e.g. calling Render or using BoundingBox.
      If all your needs are that simple, then you can use this trick
      to save some memory. This is actually useful when using TVRMLGLAnimation,
      as it creates a lot of intermediate node structures and TVRMLFlatScene
      instances. }
    property RootNode: TVRMLNode read FRootNode write FRootNode;

    { jezeli OwnsRootNode to zwolni go w destruktorze }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode;

    { Creates triangle octree and inits it with our BoundingBox
      and adds all triangles from our ShapeStates.
      (generated using  ShapeNode.Triangulate(State, false, ...)
      (note : OverTriangulate = false because it's not
      necessary for collision detection))

      If ProgressTitle <> '' then it uses Progress while building octree.

      Remember that such octree has a reference to Shape nodes
      inside RootNode vrml tree and to State objects inside
      our ShapeStates list.
      So you must not use this octree after freeing this object.
      Also you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of property
      @link(DefaultTriangleOctree). But of course you can use it
      (and you often will) in code like
        Scene.DefaultTriangleOctree := Scene.CreateTriangleOctree(...)
    }
    function CreateTriangleOctree(const ProgressTitle: string):
      TVRMLTriangleOctree; overload;
    function CreateTriangleOctree(AMaxDepth, AMaxLeafItemsCount: integer;
      const ProgressTitle: string): TVRMLTriangleOctree; overload;

    { Creates shapestate octree and inits it with our BoundingBox
      and adds all our ShapeStates.

      If ProgressTitle <> '' then it uses Progress while building octree.

      Remember that such octree has a reference to our ShapeStates list.
      So you must not use this octree after freeing this object.
      Also you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of property
      @link(DefaultShapeStateOctree). But of course you can use it
      (and you often will) in code like
        Scene.DefaultShapeStateOctree := Scene.CreateShapeStateOctree(...)
    }
    function CreateShapeStateOctree(const ProgressTitle: string):
      TVRMLShapeStateOctree; overload;
    function CreateShapeStateOctree(AMaxDepth, AMaxLeafItemsCount: integer;
      const ProgressTitle: string): TVRMLShapeStateOctree; overload;

    { Notes for both DefaultTriangleOctree and
      DefaultShapeStateOctree:

      Everything in my units is done in the spirit
      that you can create as many octrees as you want for a given scene
      (both octrees based on triangles and based on shapestates).
      Also, in some special cases an octree may be constructed in
      some special way (not only using @link(CreateShapeStateOctree)
      or @link(CreateTriangleOctree)) so that it doesn't contain
      the whole scene from some TVRMLFlatScene object, or it contains
      the scene from many TVRMLFlatScene objects, or something else.

      What I want to say is that it's generally wrong to think of
      an octree as something that maps 1-1 to some TVRMLFlatScene object.
      Octrees, as implemented here, are a lot more flexible.

      That said, it's very often the case that you actually want to
      create exactly one octree of each kind (one TVRMLTriangleOctree
      and one TVRMLShapeStateOctree) for each scene.
      Properties below make it easier for you.
      Basically you can use them however you like.
      They can simply serve for you as some variables inside
      TVRMLFlatSceneGL that you can use however you like,
      so that you don't have to declare two additional variables
      like SceneTriangleOctree and SceneShapeStateOctree
      each time you define variable Scene: TVRMLFlatScene.

      Also, some methods in this class that take an octree from you
      as a parameter may have some overloaded versions that
      implicitly use octree objects stored in properties below,
      e.g. see @link(TVRMLFlatSceneGL.RenderFrustumOctree).

      This class modifies these properties in *only* one case:
      if OwnsDefaultTriangleOctree is true (default value)
      then at destruction this class calls FreeAndNil(DefaultTriangleOctree).
      Similar for DefaultShapeStateOctree.

      In any case, these properties are not managed by this object.
      E.g. these octrees are not automaticaly rebuild when you
      call ChangedAll.
    }
    property DefaultTriangleOctree: TVRMLTriangleOctree
      read FDefaultTriangleOctree write FDefaultTriangleOctree;

    property DefaultShapeStateOctree: TVRMLShapeStateOctree
      read FDefaultShapeStateOctree write FDefaultShapeStateOctree;

    property OwnsDefaultTriangleOctree: boolean
      read FOwnsDefaultTriangleOctree write FOwnsDefaultTriangleOctree
      default true;

    property OwnsDefaultShapeStateOctree: boolean
      read FOwnsDefaultShapeStateOctree write FOwnsDefaultShapeStateOctree
      default true;

    { GetViewpoint and GetPerspectiveViewpoint return the properties
      of first defined Viewpoint in VRML file, or some default viewpoint
      properties if no viewpoint is defined in file. They seek for
      nodes Viewpoint (for VRML 2.0), PerspectiveCamera and OrthographicCamera
      (for VRML 1.0) in scene graph. GetPerspectiveViewpoint omits
      OrthographicCamera.

      Jezeli VRML posiada node kamery
      zdefiniowany w aktywnej czesci swojego grafu to oblicza swoje zmienne
      "out" na podstawie tego node'a, wpp. zwraca domyslne ulozenia kamery
      w VRMLu, zgodnie ze specyfik. VRMLa (tzn. CamPos = (0, 0, 1),
      CamDir = (0, 0, -1), CamUp = GravityUp = (0, 1, 0), CamType = ctPerspective
      (ze domyslna kamera jest ctPerspective to juz sam sobie dopowiedzialem)).

      If camera properties were found in some node,
      it returns this node. Otherwise it returns nil.
      This way you can optionally extract some additional info from
      used viewpoint node, or do something special if default values
      were used. Often you will just ignore result of this function
      --- after all, the most important feature of this function
      is that you don't @italic(have) to care about details of
      dealing with camera node.

      Zwraca zawsze znormalizowane CamDir i CamUp i GravityUp ---
      powody takie same jak dla TNodeGeneralViewpoint.GetCameraVectors.

      @groupBegin }
    function GetViewpoint(
      out CamKind: TVRMLCameraKind;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single):
      TNodeGeneralViewpoint;

    function GetPerspectiveViewpoint(
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single):
      TNodeGeneralViewpoint;
    { @groupEnd }

    { This enumerates all viewpoint nodes (Viewpoint (for VRML 2.0),
      PerspectiveCamera and OrthographicCamera (for VRML 1.0))
      in scene graph.
      For each such node, it calls ViewpointFunction.

      Essentially, this is a trivial wrapper over RootNode.Traverse
      that returns only TNodeGeneralViewpoint. }
    procedure EnumerateViewpoints(ViewpointFunction: TViewpointFunction);

    { FogNode zwraca aktualny node Fog w tym modelu VRMLa, lub nil jesli
      nie ma aktywnego node'u fog.

      FogDistanceScaling zwraca skalowanie tego node'a, wziete
      z transformacji w miejscu gdzie sie znajduje node Fog w hierarchii VRMLa.
      You should always multiply FogNode.FdVisibilityRange.Value
      by FogDistanceScaling when applying (rendering etc.) this fog node.
      Value of FogDistanceScaling is undefined if FogNode = nil.

      Wyniki tych funkcji sa "cachowane" wiec nie ma strachu - mozna uzywac
      tych funkcji czesto, dopoki nie bedziesz w miedzyczasie zmienial modelu
      VRMLa to te funkcje beda prosto zwracaly juz przeliczone wyniki,
      bez zadnej straty czasu. }
    function FogNode: TNodeFog;
    function FogDistanceScaling: Single;

    { This returns an array of all triangles of this scene.
      I.e. it triangulates the scene, adding all non-degenerated
      (see IsValidTriangles) triangles to the list.

      It's your responsibility what to do with resulting
      object, when to free it etc. }
    function CreateTrianglesList(OverTriangulate: boolean):
      TDynTriangle3SingleArray;

    { Just like CreateTrianglesList, but results of this function are cached,
      and returned object is read-only for you. Don't modify it, don't free it. }
    function TrianglesList(OverTriangulate: boolean):
      TDynTriangle3SingleArray;

    { List of edges of this scene, assuming that the scene is a closed manifold.
      More precisely, the scene must be composed from any number of closed
      manifolds. Which means that each edge must have exactly two neighboring
      faces. And the triangles must be consistently ordered,
      all CCW on the outside.
      Returns @nil if the scene does not satisfy these requirements.

      Result of this function is cached, and it's also owned by this object.
      So don't modify it, don't free it.

      This uses TrianglesList(false). }
    function ManifoldEdges: TDynManifoldEdgeArray;

    { Set @link(ManifoldEdges) value. The Value set here will be returned
      by following ManifoldEdges calls. The Value passed here will @italic(not
      be owned) by this object --- you gave this, you're responsible for
      freeing it.

      This is handy if you know that this scene has the same
      ManifoldEdges contents as some other scene. In particular,
      this is extremely handy in cases of animations in TVRMLGLAnimation,
      where all scenes actually need only a single instance of TDynManifoldEdgeArray,
      this greatly speeds up TVRMLGLAnimation loading and reduces memory use.

      Note that passing here as Value the same reference
      that is already returned by ManifoldEdges is always guaranteed to be
      a harmless operation. If ManifoldEdges was owned by this object,
      it will remain owned in this case (while in normal sharing situation,
      Value set here is assumed to be owned by something else). }
    procedure ShareManifoldEdges(Value: TDynManifoldEdgeArray);
  end;

{ TODO - I tu dochodzimy do mechanizmu automatycznego wywolywania odpowiedniego
  Changed. Byloby to bardzo wygodne, prawda ? Obecnie wszystkie node'y
  maja juz liste swoich Parents; na pewno bedziemy musieli to zrobic
  takze dla wszystkich TVRMLField. I jeszcze trzeba zrobic mechanizm zeby
  node mogl przeslac informacje wyzej, do TVRMLShapeState lub
  TVRMLFlatScene. Sporo tu do zrobienia - jak tylko mi sie to
  skrystalizuje zrobie to.
}

var
  { StateDefaultNodes, used as a starting state nodes for TVRMLFlatScene
    and descendants when traversing VRML tree.

    It's needed that various TVRMLFlatScene instances use the same
    StateDefaultNodes, because in some cases we assume that nodes
    are equal only when their references are equal
    (e.g. TVRMLGraphTraverseState.Equals does this, and this is used
    by ShapeState caching in TVRMLOpenGLRendererContextCache).

    This is read-only from outside, initialized and finalized in
    this unit. }
  StateDefaultNodes: TTraverseStateLastNodes;

{$undef read_interface}

implementation

uses VRMLFields, VRMLCameraUtils;

{$define read_implementation}
{$I macprecalcvaluereturn.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}

{ TVRMLFlatScene ----------------------------------------------------------- }

constructor TVRMLFlatScene.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
 inherited Create;
 FRootNode := ARootNode;
 FOwnsRootNode := AOwnsRootNode;

 FOwnsDefaultTriangleOctree := true;
 FOwnsDefaultShapeStateOctree := true;

 FShapeStates := TVRMLShapeStatesList.Create;

 ChangedAll;
end;

destructor TVRMLFlatScene.Destroy;
begin
 FreeAndNil(FTrianglesList[false]);
 FreeAndNil(FTrianglesList[true]);

 if FOwnsManifoldEdges then
   FreeAndNil(FManifoldEdges);

 ShapeStates.FreeWithContents;

 if OwnsDefaultTriangleOctree then FreeAndNil(FDefaultTriangleOctree);
 if OwnsDefaultShapeStateOctree then FreeAndNil(FDefaultShapeStateOctree);
 if OwnsRootNode then FreeAndNil(FRootNode);
 inherited;
end;

function TVRMLFlatScene.CalculateBoundingBox: TBox3d;
var i: integer;
begin
 Result := EmptyBox3d;
 for i := 0 to ShapeStates.Count-1 do
  Box3dSumTo1st(Result, ShapeStates[i].BoundingBox);
end;

function TVRMLFlatScene.CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
var i: integer;
begin
 Result := 0;
 for i := 0 to ShapeStates.Count-1 do
  Result += ShapeStates[i].VerticesCount(OverTriangulate);
end;

function TVRMLFlatScene.CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
var i: integer;
begin
 Result := 0;
 for i := 0 to ShapeStates.Count-1 do
  Result += ShapeStates[i].TrianglesCount(OverTriangulate);
end;

function TVRMLFlatScene.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := fvBBox}
{$define PRECALC_VALUE := FBoundingBox}
{$define PRECALC_VALUE_CALCULATE := CalculateBoundingBox}
PRECALC_VALUE_RETURN

function TVRMLFlatScene.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := CalculateVerticesCount(OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := fvVerticesCountOver}
  {$define PRECALC_VALUE := FVerticesCountOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := fvVerticesCountNotOver}
  {$define PRECALC_VALUE := FVerticesCountNotOver}
  PRECALC_VALUE_RETURN
 end;
end;

function TVRMLFlatScene.TrianglesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := CalculateTrianglesCount(OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := fvTrianglesCountOver}
  {$define PRECALC_VALUE := FTrianglesCountOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := fvTrianglesCountNotOver}
  {$define PRECALC_VALUE := FTrianglesCountNotOver}
  PRECALC_VALUE_RETURN
 end;
end;

procedure TVRMLFlatScene.AddFromTraverse(Node: TVRMLNode; State: TVRMLGraphTraverseState);
begin
 ShapeStates.Add(
   TVRMLShapeState.Create(Node as TNodeGeneralShape,
   TVRMLGraphTraverseState.CreateCopy(State)));
end;

procedure TVRMLFlatScene.ChangedAll;
var InitialState: TVRMLGraphTraverseState;
begin
 ShapeStates.FreeContents;
 Validities := [];

 if RootNode <> nil then
 begin
  InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
  try
   RootNode.Traverse(InitialState, TNodeGeneralShape,
     {$ifdef FPC_OBJFPC} @ {$endif} AddFromTraverse);
  finally InitialState.Free end;
 end;
end;

procedure TVRMLFlatScene.ChangedShapeStateFields(ShapeStateNum: integer);
begin
 Validities := [];
 ShapeStates[ShapeStateNum].Changed;
end;

procedure TVRMLFlatScene.ChangedFields(Node: TVRMLNode);
var NodeLastNodesIndex, i: integer;
begin
 NodeLastNodesIndex := Node.TraverseStateLastNodesIndex;

 {ignore this Changed if node is not in our VRML graph (or is in the inactive
    part) and is not one of StateDefaultNodes nodes.
  Note : zakladamy tutaj ze IsNodePresent(,true) zwraca stan Node'a zarowno
    przed modyfkacja pola jak i po - innymi slowy, zakladamy tu ze zmiana
    pola node'a nie mogla zmienic jego wlasnego stanu active/inactive. }
 if (RootNode = nil) or
    ( (not RootNode.IsNodePresent(Node, true)) and
      ((NodeLastNodesIndex = -1) or
        (StateDefaultNodes.Nodes[NodeLastNodesIndex] <> Node))
    ) then
  Exit;

 if NodeLastNodesIndex <> -1 then
 begin
  { node jest jednym z node'ow Last*. Wiec wplynal tylko na ShapeStates
    gdzie wystepuje jako Last. }
  for i := 0 to ShapeStates.Count-1 do
   if ShapeStates[i].State.LastNodes.Nodes[NodeLastNodesIndex] = Node then
    ChangedShapeStateFields(i);
 end else
 if (Node is TNodeGeneralLight) then
 begin
  { node jest jednym z node'ow Active*. Wiec wplynal tylko na ShapeStates
    gdzie wystepuje jako Active. }
  for i := 0 to ShapeStates.Count-1 do
   if ShapeStates[i].State.ActiveLights.
     IndexOfLightNode(TNodeGeneralLight(Node)) >= 0 then
    ChangedShapeStateFields(i);
 end else
 if (Node is TNodeGeneralShape) then
 begin
  { node jest Shape'm. Wiec wplynal tylko na ShapeStates gdzie wystepuje jako
    ShapeNode. }
  for i := 0 to ShapeStates.Count-1 do
   if ShapeStates[i].ShapeNode = Node then
    ChangedShapeStateFields(i);
 end else
  { node jest czyms innym; wiec musimy zalozyc ze zmiana jego pol wplynela
    jakos na State nastepujacych po nim node'ow (a moze nawet wplynela na to
    co znajduje sie w aktywnej czesci grafu VRMLa pod niniejszym node'm -
    tak sie moglo stac gdy zmienilismy pole Switch.whichChild. ) }
  ChangedAll;
end;

resourcestring
  SSceneInfoTriVertCounts_Same = 'Scene contains %d triangles and %d ' +
    'vertices (with and without over-triangulating).';
  SSceneInfoTriVertCounts_1 =
    'When we don''t use over-triangulating (e.g. when we do collision '+
    'detection or ray tracing) scene has %d triangles and %d vertices.';
  SSceneInfoTriVertCounts_2 =
    'When we use over-triangulating (e.g. when we do OpenGL rendering) '+
    'scene has %d triangles and %d vertices.';

function TVRMLFlatScene.Info(
  InfoTriVertCounts, InfoBoundingBox: boolean): string;
var
  BBox: TBox3d;
begin
  Result := '';

  if InfoTriVertCounts then
  begin
    if (VerticesCount(false) = VerticesCount(true)) and
       (TrianglesCount(false) = TrianglesCount(true)) then
      Result += Format(SSceneInfoTriVertCounts_Same,
        [TrianglesCount(false), VerticesCount(false)]) + NL else
    begin
      Result +=
        Format(SSceneInfoTriVertCounts_1,
          [TrianglesCount(false), VerticesCount(false)]) + NL +
        Format(SSceneInfoTriVertCounts_2,
          [TrianglesCount(true), VerticesCount(true)]) + NL;
    end;
  end;

  if InfoBoundingBox then
  begin
    BBox := BoundingBox;
    Result += 'Bounding box : ' + Box3dToNiceStr(BBox);
    if not IsEmptyBox3d(BBox) then
    begin
      Result += ', average size : ' + FloatToNiceStr(Box3dAvgSize(BBox));
    end;
    Result += NL;
  end;
end;

type
  TInfoNodeWriter = class
    Count: Cardinal;
    procedure WriteNode(node: TVRMLNode);
  end;
  procedure TInfoNodeWriter.WriteNode(node: TVRMLNode);
  begin
   Inc(Count);
   Writeln('Info node : "',(Node as TNodeInfo).FdString.Value, '"')
  end;

procedure TVRMLFlatScene.WritelnInfoNodes;
var W: TInfoNodeWriter;
begin
 if RootNode = nil then Exit;

 W := TInfoNodeWriter.Create;
 try
  RootNode.EnumerateNodes(TNodeInfo,
    {$ifdef FPC_OBJFPC} @ {$endif} W.WriteNode, false);
  Writeln(W.Count, ' Info nodes in the scene.');
 finally W.Free end;
end;

{ using triangle octree -------------------------------------------------- }

procedure TVRMLFlatScene.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, ShapeNode, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

function TVRMLFlatScene.CreateTriangleOctree(const ProgressTitle: string):
  TVRMLTriangleOctree;
begin
 result := CreateTriangleOctree(
   DefTriangleOctreeMaxDepth,
   DefTriangleOctreeMaxLeafItemsCount,
   ProgressTitle);
end;

function TVRMLFlatScene.CreateTriangleOctree(
  AMaxDepth, AMaxLeafItemsCount: integer;
  const ProgressTitle: string): TVRMLTriangleOctree;

  procedure FillOctree(AddTriProc: TNewTriangleProc);
  var i: integer;
  begin
   for i := 0 to ShapeStates.Count-1 do
    ShapeStates[i].ShapeNode.Triangulate(ShapeStates[i].State, false, AddTriProc);
  end;

begin
 result := TVRMLTriangleOctree.Create(AMaxDepth, AMaxLeafItemsCount, BoundingBox);
 try
  result.OctreeItems.AllowedCapacityOverflow := TrianglesCount(false);
  try
   if ProgressTitle <> '' then
   begin
    Progress.Init(TrianglesCount(false), ProgressTitle);
    try
     TriangleOctreeToAdd := result;
     FillOctree({$ifdef FPC_OBJFPC} @ {$endif} AddTriangleToOctreeProgress);
    finally Progress.Fini end;
   end else
    FillOctree({$ifdef FPC_OBJFPC} @ {$endif} Result.AddItemTriangle);
  finally
   result.OctreeItems.AllowedCapacityOverflow := 4;
  end;
 except result.Free; raise end;
end;

{ using shapestate octree ---------------------------------------- }

function TVRMLFlatScene.CreateShapeStateOctree(const ProgressTitle: string):
  TVRMLShapeStateOctree;
begin
 Result := CreateShapeStateOctree(
   DefShapeStateOctreeMaxDepth,
   DefShapeStateOctreeMaxLeafItemsCount,
   ProgressTitle);
end;

function TVRMLFlatScene.CreateShapeStateOctree(
  AMaxDepth, AMaxLeafItemsCount: integer;
  const ProgressTitle: string): TVRMLShapeStateOctree;
var i: Integer;
begin
 Result := TVRMLShapeStateOctree.Create(AMaxDepth, AMaxLeafItemsCount,
   BoundingBox, ShapeStates);
 try

  if ProgressTitle <> '' then
  begin
   Progress.Init(ShapeStates.Count, ProgressTitle);
   try
    for i := 0 to ShapeStates.Count - 1 do
    begin
     Result.TreeRoot.AddItem(i);
     Progress.Step;
    end;
   finally Progress.Fini end;
  end else
  begin
   for i := 0 to ShapeStates.Count - 1 do
    Result.TreeRoot.AddItem(i);
  end;

 except Result.Free; raise end;
end;

{ viewpoints ----------------------------------------------------------------- }

type
  TViewpointsSeeker = class
    ViewpointFunction: TViewpointFunction;
    procedure Seek(ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
  end;

  procedure TViewpointsSeeker.Seek(
    ANode: TVRMLNode; AState: TVRMLGraphTraverseState);
  begin
    ViewpointFunction(
      TNodeGeneralViewpoint(ANode),
      AState.CurrMatrix);
  end;

procedure TVRMLFlatScene.EnumerateViewpoints(
  ViewpointFunction: TViewpointFunction);
var
  InitialState: TVRMLGraphTraverseState;
  Seeker: TViewpointsSeeker;
begin
  if RootNode <> nil then
  begin
    InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
    try
      Seeker := TViewpointsSeeker.Create;
      try
        Seeker.ViewpointFunction := ViewpointFunction;
        RootNode.Traverse(InitialState, TNodeGeneralViewpoint,
          {$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek);
      finally FreeAndNil(Seeker) end;
    finally FreeAndNil(InitialState) end;
  end;
end;

type
  BreakFirstViewpointFound = class(TCodeBreaker);

  TFirstViewpointSeeker = class
    OnlyPerspective: boolean;
    FoundNode: TNodeGeneralViewpoint;
    FoundTransform: PMatrix4Single;
    procedure Seek(Node: TNodeGeneralViewpoint;
      const Transform: TMatrix4Single);
  end;

  procedure TFirstViewpointSeeker.Seek(
    Node: TNodeGeneralViewpoint;
    const Transform: TMatrix4Single);
  begin
    if (not OnlyPerspective) or (Node.CameraKind = ckPerspective) then
    begin
      FoundTransform^ := Transform;
      FoundNode := Node;
      raise BreakFirstViewpointFound.Create;
    end;
  end;

function TVRMLFlatScene.GetViewpointCore(
  const OnlyPerspective: boolean;
  out CamKind: TVRMLCameraKind;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single): TNodeGeneralViewpoint;
var
  CamTransform: TMatrix4Single;
  Seeker: TFirstViewpointSeeker;
begin
  Result := nil;
  Seeker := TFirstViewpointSeeker.Create;
  try
    Seeker.OnlyPerspective := OnlyPerspective;
    Seeker.FoundTransform := @CamTransform;
    try
      EnumerateViewpoints({$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek);
    except
      on BreakFirstViewpointFound do
      begin
        Result := Seeker.FoundNode;
      end;
    end;
  finally FreeAndNil(Seeker) end;

  if Result <> nil then
  begin
    Result.GetCameraVectors(CamTransform, CamPos, CamDir, CamUp, GravityUp);
    CamKind := Result.CameraKind;
  end else
  begin
    { use default camera settings }
    CamPos := StdVRMLCamPos[1];
    CamDir := StdVRMLCamDir;
    CamUp := StdVRMLCamUp;
    GravityUp := StdVRMLGravityUp;
    CamKind := ckPerspective;
  end;
end;

function TVRMLFlatScene.GetViewpoint(
  out CamKind: TVRMLCameraKind;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single): TNodeGeneralViewpoint;
begin
  Result := GetViewpointCore(false, CamKind, CamPos, CamDir, CamUp, GravityUp);
end;

function TVRMLFlatScene.GetPerspectiveViewpoint(
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single): TNodeGeneralViewpoint;
var
  CamKind: TVRMLCameraKind;
begin
  Result := GetViewpointCore(true, CamKind, CamPos, CamDir, CamUp, GravityUp);
  Assert(CamKind = ckPerspective);
end;

{ fog ---------------------------------------------------------------------- }

procedure TVRMLFlatScene.ValidateFog;
var FogTransform: TMatrix4Single;
    InitialState: TVRMLGraphTraverseState;
begin
 InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
 try
  if (RootNode <> nil) and
    RootNode.TryFindNodeTransform(InitialState, TNodeFog, TVRMLNode(FFogNode),
      FogTransform) then
  begin
   { TODO: nie mamy jak tutaj obliczyc FFogDistanceScaling
     uwzgledniajac skalowanie w transformacji FogTransform bo przeciez
     skalowanie moze byc rozne w rozne strony. Dopiero teraz zorientowalem
     sie o co chodzi : zamiast FFogDistanceScaling powinnismy
     sobie tutaj jakos wyliczac transformacje odwrotna do FogTransform.
     Potem kazdy element ktory bedziemy rysowac najpierw zrzutujemy
     do coordinate space node'u mgly, podobnie jak pozycje kamery,
     obliczymy odleglosci tych zrzutowanych punktow i to te odleglosci
     bedziemy porownywac z FogNode.VisibilityRange. To jest poprawna metoda.
     I w ten sposob np. mozemy zrobic mgle bardziej gesta w jednym kierunku
     a mniej w drugim. Fajne.

     Zupelnie nie wiem jak to zrobic w OpenGLu - jego GL_FOG_END (chyba)
     nie przechodzi takiej transformacji. Wiec w OpenGLu nie zrobie
     przy pomocy glFog takiej mgly (a przeciez samemu robic mgle nie bedzie
     mi sie chcialo, nie mowiac juz o tym ze zalezy mi na szybkosci a strace
     ja jesli bede implementowal rzeczy ktore juz sa w OpenGLu).

     Tymczasem uzywam wiec prostego rozwiazania : wyliczam
     FFogDistanceScaling na podstawie sredniej skali zawartej
     w transformacji FogTransform (trzy wspolczynniki skalowania mam w
     FogTransform[0, 0], FogTransform[1, 1], FogTransform[2, 2], biore ich
     srednia arytmetyczna i to jest "srednia skala").

     A wiec FFogDistanceScaling jest niepoprawne
     ale nie likwiduje tego parametru. Gdy sie zdecyduje, zastapie go czyms
     w rodzaju FogInvertedTransform: TMatrix4Single.
   }
   FFogDistanceScaling :=
     ((FogTransform[0, 0] + FogTransform[1, 1] + FogTransform[2, 2])/3);
  end else
   FFogNode := nil;

  Include(Validities, fvFog);
 finally InitialState.Free end;
end;

function TVRMLFlatScene.FogNode: TNodeFog;
begin
 if not (fvFog in Validities) then ValidateFog;
 result := FFogNode;
end;

function TVRMLFlatScene.FogDistanceScaling: Single;
begin
 if not (fvFog in Validities) then ValidateFog;
 result := FFogDistanceScaling;
end;

{ triangles list ------------------------------------------------------------- }

procedure TVRMLFlatScene.ValidateTrianglesList(OverTriangulate: boolean);
var
  ValidityValue: TVRMLFlatSceneValidity;
begin
  if OverTriangulate then
    ValidityValue := fvTrianglesListOverTriangulate else
    ValidityValue := fvTrianglesListNotOverTriangulate;
  if not (ValidityValue in Validities) then
  begin
    FreeAndNil(FTrianglesList[OverTriangulate]);
    FTrianglesList[OverTriangulate] := CreateTrianglesList(OverTriangulate);
    Include(Validities, ValidityValue);
  end;
end;

type
  TTriangleAdder = class
    TriangleList: TDynTriangle3SingleArray;
    procedure AddTriangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      ShapeNode: TNodeGeneralShape;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangleAdder.AddTriangle(const Triangle: TTriangle3Single;
    State: TVRMLGraphTraverseState;
    ShapeNode: TNodeGeneralShape;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  begin
    if IsValidTriangle(Triangle) then
      TriangleList.AppendItem(Triangle);
  end;

function TVRMLFlatScene.CreateTrianglesList(OverTriangulate: boolean):
  TDynTriangle3SingleArray;
var
  I: Integer;
  TriangleAdder: TTriangleAdder;
begin
  Result := TDynTriangle3SingleArray.Create;
  try
    Result.AllowedCapacityOverflow := TrianglesCount(false);
    try
      TriangleAdder := TTriangleAdder.Create;
      try
        TriangleAdder.TriangleList := Result;
        for I := 0 to ShapeStates.Count - 1 do
          ShapeStates[I].ShapeNode.Triangulate(
            ShapeStates[I].State, OverTriangulate,
            {$ifdef FPC_OBJFPC} @ {$endif} TriangleAdder.AddTriangle);
      finally FreeAndNil(TriangleAdder) end;
    finally Result.AllowedCapacityOverflow := 4 end;
  except Result.Free; raise end;
end;

function TVRMLFlatScene.TrianglesList(OverTriangulate: boolean):
  TDynTriangle3SingleArray;
begin
  ValidateTrianglesList(OverTriangulate);
  Result := FTrianglesList[OverTriangulate];
end;

function TDynManifoldEdgeArray.AddEdgeCheckManifold(
  const TriangleIndex: Cardinal;
  const V0: TVector3Single;
  const V1: TVector3Single;
  const VertexIndex: Cardinal;
  Triangles: TDynTriangle3SingleArray): boolean;
var
  I: Integer;
  EdgePtr: PManifoldEdge;
  TrianglePtr: PTriangle3Single;
  EdgeV0, EdgeV1: PVector3Single;
begin
  if Count <> 0 then
  begin
    EdgePtr := Pointers[0];
    for I := 0 to Count - 1 do
    begin
      TrianglePtr := Triangles.Pointers[EdgePtr^.Triangles[0]];
      EdgeV0 := @TrianglePtr^[EdgePtr^.VertexIndex];
      EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + 1) mod 3];

      { Triangles must be consistently ordered on a manifold,
        so the second time an edge is present, we know it must
        be in different order. So we compare V0 with EdgeV1
        (and V1 with EdgeV0), no need to compare V1 with EdgeV1. }
      if VectorsPerfectlyEqual(V0, EdgeV1^) and
         VectorsPerfectlyEqual(V1, EdgeV0^) then
      begin
        { Add triangle to existing edge }
        if EdgePtr^.TrianglesCount = 2 then
          Result := false else
        begin
          EdgePtr^.Triangles[EdgePtr^.TrianglesCount] := TriangleIndex;
          Inc(EdgePtr^.TrianglesCount);
          Result := true;
        end;
        Exit;
      end;
      Inc(EdgePtr);
    end;
  end;

  { New adge }
  IncLength;
  EdgePtr := Pointers[High];
  EdgePtr^.VertexIndex := VertexIndex;
  EdgePtr^.Triangles[0] := TriangleIndex;
  EdgePtr^.TrianglesCount := 1;
end;

function TVRMLFlatScene.ManifoldEdges: TDynManifoldEdgeArray;

  { TODO: FManifoldEdges and triangles lists should be freed
    (and removed from Validities) on any ChangedXxx call. }

  { Sets FManifoldEdges. Assumes that FManifoldEdges is @nil on enter. }
  procedure CalculateManifoldEdges;
  var
    I: Integer;
    Triangles: TDynTriangle3SingleArray;
    TrianglePtr: PTriangle3Single;
  begin
    Assert(FManifoldEdges = nil);

    { It's important here that TrianglesList guarentees that only valid
      triangles are included. Otherwise degenerate triangles could make
      shadow volumes rendering result bad. }
    Triangles := TrianglesList(false);

    FManifoldEdges := TDynManifoldEdgeArray.Create;

    { There is a precise relation between number of edges and number of faces
      on a closed manifold: E = T * 3 / 2. }
    FManifoldEdges.AllowedCapacityOverflow := Triangles.Count * 3 div 2;

    TrianglePtr := Triangles.Pointers[0];
    for I := 0 to Triangles.Count - 1 do
    begin
      { TrianglePtr points to Triangles[I] now }
      if (not FManifoldEdges.AddEdgeCheckManifold(I, TrianglePtr^[0], TrianglePtr^[1], 0, Triangles)) or
         (not FManifoldEdges.AddEdgeCheckManifold(I, TrianglePtr^[1], TrianglePtr^[2], 1, Triangles)) or
         (not FManifoldEdges.AddEdgeCheckManifold(I, TrianglePtr^[2], TrianglePtr^[0], 2, Triangles)) then
      begin
        { scene not a manifold: more than 2 faces for one edge }
        FreeAndNil(FManifoldEdges);
        Exit;
      end;
      Inc(TrianglePtr);
    end;

    for I := 0 to FManifoldEdges.Count - 1 do
      if FManifoldEdges.Items[I].TrianglesCount <> 2 then
      begin
        { scene not a manifold: less than 2 faces for one edge
          (the case with more than 2 is already eliminated above) }
        FreeAndNil(FManifoldEdges);
        Exit;
      end;
  end;

begin
  if not (fvManifoldEdges in Validities) then
  begin
    FOwnsManifoldEdges := true;
    CalculateManifoldEdges;
    Include(Validities, fvManifoldEdges);
  end;

  Result := FManifoldEdges;
end;

procedure TVRMLFlatScene.ShareManifoldEdges(Value: TDynManifoldEdgeArray);
begin
  if (fvManifoldEdges in Validities) and (Value = FManifoldEdges) then
    { No need to do anything in this case.

      If Value = FManifoldEdges = nil, then we may leave FOwnsManifoldEdges = true
      while it could be = false (if we let this procedure continue),
      but this doesn't matter (since FOwnsManifoldEdges doesn't matter when
      FManifoldEdges = nil).

      If Value <> nil and old FOwnsManifoldEdges is false then this
      doesn't change anything.

      Finally, the important case: If Value <> nil and old
      FOwnsManifoldEdges is true. Then it would be very very bad
      to continue this method, as we would free FManifoldEdges pointer
      and right away set FManifoldEdges to the same pointer (that would
      be invalid now). }
    Exit;

  if FOwnsManifoldEdges then
    FreeAndNil(FManifoldEdges);

  FManifoldEdges := Value;
  FOwnsManifoldEdges := false;
  Include(Validities, fvManifoldEdges);
end;

initialization
  TraverseState_CreateNodes(StateDefaultNodes);
finalization
  TraverseState_FreeAndNilNodes(StateDefaultNodes);
end.
