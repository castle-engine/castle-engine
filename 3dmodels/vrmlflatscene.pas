{
  Copyright 2003-2005 Michalis Kamburelis.

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

{ TVRMLFlatScene ----------------------------------------------------------- }

type
  { Internal helper type for TVRMLFlatScene }
  TVRMLFlatSceneValidities = set of (fvBBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvFog);

  { This class represents a VRML scene (i.e. graph of VRML nodes
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
  protected
    StateDefaultNodes: TTraverseStateLastNodes;
  private
    FOwnsRootNode: boolean;
    FShapeStates: TVRMLShapeStatesList;
    FRootNode: TVRMLNode;
    procedure AddFromTraverse(Node: TVRMLNode; State: TVRMLGraphTraverseState);

    FFogNode: TNodeFog;
    FFogTransformedVisibilityRange: Single;
    { calculate FFogNode and FFogTransformedVisibilityRange, include fvFog
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
      MatNum: integer);

    FDefaultShapeStateOctree: TVRMLShapeStateOctree;
    FDefaultTriangleOctree: TVRMLTriangleOctree;
    FOwnsDefaultTriangleOctree: boolean;
    FOwnsDefaultShapeStateOctree: boolean;
  public
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

    { write some short (a few lines, at most) basic information about the scene }
    procedure WritelnSceneInfo(WritelnTriVertCounts,
      WritelnBoundingBox: boolean);
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
      like Attrib_Xxx properties in subclass @link(TVRMLFlatSceneGL).

      That's why it is possible to change RootNode and it is even
      possible to set it to nil. And when When RootNode = nil everything
      should work -- you can query such scene (with RootNode = nil)
      for Vecrtices/TrianglesCount (answer will be 0),
      for BoundingBox (answer will be EmptyBox3d),
      you can render such scene (nothing will be rendered) etc.
      Scene RootNode = nil will act quite like a Scene with
      e.g. no TNodeGeneralShape nodes.

      Always call ChangedAll when you changed RootNode.
    }
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

    { Zwraca kamere zdefiniowana w tym VRMLu: jezeli VRML posiada node kamery
      zdefiniowany w aktywnej czesci swojego grafu to oblicza swoje zmienne
      "var" na podstawie tego node'a, wpp. zwraca domyslne ulozenia kamery
      w VRMLu, zgodnie ze specyfik. VRMLa (tzn. CamPos = (0, 0, 1),
      CamDir = (0, 0, -1), CamUp = (0, 1, 0), CamType = ctPerspective
      (ze domyslna kamera jest ctPerspective to juz sam sobie dopowiedzialem)).

      Podajac CamClass mozesz swobodnie ograniczyc klase kamer jakich ma szukac
      - bedzie szukal tylko kamer ktore "is CamClass". Jesli nie chcesz
      ograniczac zakresu szukanych kamer podaj TNodeGeneralCamera aby objac
      wszystkie mozliwe kamery.

      Zwraca "true" jezeli uzyl kamery zdefiniowanej w pliku VRMLa (false
      oznacza ze uzyl domyslnych ustawien kamery). Czesto bedziesz chcial
      zignorowac wynik tej funkcji, w koncu zasadnicza funkcja tej funkcji
      jest zaproponowanie jakiegos ustawienia poczatkowego kamery.

      Zwraca zawsze znormalizowane CamDir i CamUp - powody takie same jak
      dla TNodeGeneralCamera.CalcCamera. }
    function GetCamera(const CamClass: TNodeGeneralCameraClass;
      var CamKind: TVRMLCameraKind;
      var CamPos, CamDir, CamUp: TVector3Single): boolean;
    { j.w. ale ignoruje kamery OrthographicCamera w VRMLu }
    function GetPerspectiveCamera(var CamPos, CamDir, CamUp: TVector3Single): boolean;

    { FogNode zwraca aktualny node Fog w tym modelu VRMLa, lub nil jesli
      nie ma aktywnego node'u fog. FogTransformedVisibilityRange
      zwraca pole visibilityRange tego node'a przetransformowane przez
      transformacje w miejscu gdzie sie znajduje node Fog w hierarchii VRMLa
      (zawsze powinienes uzywac tego FogTransformedVisibilityRange zamiast
      FogNode.FdVisibilityRange.Value). FogTransformedVisibilityRange
      jest niezdefiniowane jesli FogNode = nil.

      Wyniki tych funkcji sa "cachowane" wiec nie ma strachu - mozna uzywac
      tych funkcji czesto, dopoki nie bedziesz w miedzyczasie zmienial modelu
      VRMLa to te funkcje beda prosto zwracaly juz przeliczone wyniki,
      bez zadnej straty czasu. }
    function FogNode: TNodeFog;
    function FogTransformedVisibilityRange: Single;

    { @noAutoLinkHere }
    destructor Destroy; override;

    { @noAutoLinkHere }
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
  end;

{ TODO - I tu dochodzimy do mechanizmu automatycznego wywolywania odpowiedniego
  Changed. Byloby to bardzo wygodne, prawda ? Obecnie wszystkie node'y
  maja juz liste swoich Parents; na pewno bedziemy musieli to zrobic
  takze dla wszystkich TVRMLField. I jeszcze trzeba zrobic mechanizm zeby
  node mogl przeslac informacje wyzej, do TVRMLShapeState lub
  TVRMLFlatScene. Sporo tu do zrobienia - jak tylko mi sie to
  skrystalizuje zrobie to.
}

{$undef read_interface}

implementation

uses VRMLFields, VRMLCameraUtils;

{$define read_implementation}
{$I macprecalcvaluereturn.inc}

{ TVRMLFlatScene ----------------------------------------------------------- }

constructor TVRMLFlatScene.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
var i: Integer;
begin
 inherited Create;
 FRootNode := ARootNode;
 FOwnsRootNode := AOwnsRootNode;

 FOwnsDefaultTriangleOctree := true;
 FOwnsDefaultShapeStateOctree := true;

 FShapeStates := TVRMLShapeStatesList.Create;
 for i := 0 to HighTraverseStateLastNodes do
  StateDefaultNodes.Nodes[i] := TraverseStateLastNodesClasses[i].Create('', '');

 ChangedAll;
end;

destructor TVRMLFlatScene.Destroy;
var i: Integer;
begin
 for i := 0 to HighTraverseStateLastNodes do StateDefaultNodes.Nodes[i].Free;
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
   { "Self." below required because of fpc 1.0.10 func_ofobject_bug }
   RootNode.Traverse(InitialState, TNodeGeneralShape, Self.AddFromTraverse);
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

procedure TVRMLFlatScene.WritelnSceneInfo(WritelnTriVertCounts,
  WritelnBoundingBox: boolean);
var BBox: TBox3d;
begin
 if WritelnTriVertCounts then
 begin
  if (VerticesCount(false) = VerticesCount(true)) and
     (TrianglesCount(false) = TrianglesCount(true)) then
   Writeln('Scene contains ',TrianglesCount(false), ' triangles and '
     ,VerticesCount(false), ' vertices (with and without over-triangulating).') else
  begin
   Writeln('When we don''t use over-triangulating (e.g. when we do collision '+
     'detection or ray tracing) scene has ',TrianglesCount(false),
     ' triangles and ',VerticesCount(false), ' vertices.');
   Writeln('When we use over-triangulating (e.g. when we do OpenGL rendering) '+
     'scene has ',TrianglesCount(true),
     ' triangles and ',VerticesCount(true), ' vertices.');
  end;
 end;
 if WritelnBoundingBox then
 begin
  BBox := BoundingBox;
  Write('Bounding box : ', Box3dToNiceStr(BBox));
  if not IsEmptyBox3d(BBox) then
   Write(', average size : ',FloatToNiceStr(Box3dAvgSize(BBox)) );
  Writeln;
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
  RootNode.EnumNodes(TNodeInfo, W.WriteNode, false);
  Writeln(W.Count, ' Info nodes in the scene.');
 finally W.Free end;
end;

{ using triangle octree -------------------------------------------------- }

procedure TVRMLFlatScene.AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape; MatNum: integer);
begin
 Progress.Step;
 TriangleOctreeToAdd.AddItemTriangle(Triangle, State, ShapeNode, MatNum);
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
     { "Self." below required because of fpc 1.0.10 func_ofobject_bug }
     FillOctree(Self.AddTriangleToOctreeProgress);
    finally Progress.Fini end;
   end else
    FillOctree(result.AddItemTriangle);
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

{ camera ----------------------------------------------------------------------- }

function TVRMLFlatScene.GetCamera(const CamClass: TNodeGeneralCameraClass;
  var CamKind: TVRMLCameraKind; var CamPos, CamDir, CamUp: TVector3Single): boolean;
var InitialState: TVRMLGraphTraverseState;
    CamNode: TNodeGeneralCamera;
    CamTransform: TMatrix4Single;
begin
 InitialState := TVRMLGraphTraverseState.Create(StateDefaultNodes);
 try
  Result := (RootNode <> nil) and
    RootNode.TryFindNodeTransform(InitialState, CamClass, TVRMLNode(CamNode),
      CamTransform);
  if Result then
  begin
   CamNode.CalcCamera(CamTransform, CamPos, CamDir, CamUp);
   CamKind := CamNode.CameraKind;
  end else
  begin
   { use default camera settings }
   CamPos := StdVRMLCamPos;
   CamDir := StdVRMLCamDir;
   CamUp := StdVRMLCamUp;
   CamKind := ckPerspective;
  end;
 finally InitialState.Free end;
end;

function TVRMLFlatScene.GetPerspectiveCamera(
  var CamPos, CamDir, CamUp: TVector3Single): boolean;
var CamKind: TVRMLCameraKind;
begin
 result := GetCamera(TNodePerspectiveCamera, CamKind, CamPos, CamDir, CamUp);
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
   {sorry - nie mamy jak tutaj obliczyc FFogTransformedVisibilityRange
    uwzgledniajac skalowanie w transformacji FogTransform bo przeciez
    skalowanie moze byc rozne w rozne strony. Dopiero teraz zorientowalem
    sie o co chodzi : zamiast FFogTransformedVisibilityRange powinnismy
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
    FFogTransformedVisibilityRange na podstawie sredniej skali zawartej
    w transformacji FogTransform (trzy wspolczynniki skalowania mam w
    FogTransform[0, 0], FogTransform[1, 1], FogTransform[2, 2], biore ich
    srednia arytmetyczna i to jest "srednia skala").

    A wiec FFogTransformedVisibilityRange jest niepoprawne
    ale nie likwiduje tego parametru. Gdy sie zdecyduje, zastapie go czyms
    w rodzaju FogInvertedTransform: TMatrix4Single.
   }
   FFogTransformedVisibilityRange :=
     ((FogTransform[0, 0] + FogTransform[1, 1] + FogTransform[2, 2])/3) *
     FFogNode.FdVisibilityRange.Value;
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

function TVRMLFlatScene.FogTransformedVisibilityRange: Single;
begin
 if not (fvFog in Validities) then ValidateFog;
 result := FFogTransformedVisibilityRange;
end;

end.
