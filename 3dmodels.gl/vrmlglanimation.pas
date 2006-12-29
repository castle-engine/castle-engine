{ TVRMLGLAnimation class. }
unit VRMLGLAnimation;

interface

uses SysUtils, VRMLNodes, VRMLOpenGLRenderer, VRMLFlatSceneGL,
  KambiUtils, DOM;

type
  EModelsStructureDifferent = class(Exception)
    constructor CreateFmt(const S: string; const Args: array of const);
  end;

  { This is an animation of VRML model done by interpolating between
    any number of model states.

    When constructing object of this class,
    you must provide one or more VRML models that have exactly the same
    structure, but possibly different values for various fields.
    Each scene has an associated position in Time (Time is just a float number).
    For constructor Scenes must be specified in increasing Time order.
    For example, first object may be a small sphere with blue color, the other
    object may be a larger sphere with white color, and the simplest
    times are 0.0 for the 1st scene and 1.0 for the 2nd scene.

    This class creates a list of @link(Scenes) such that
    @unorderedList(
      @itemSpacing Compact
      @item the first scene on the list is exactly the 1st object
      @item the last scene on the list is exactly the last object
      @item(intermediate scenes are accordingly interpolated between
        the two surrounding "predefined" by you scenes)
    )

    In effect, the @link(Scenes) is a list of scenes that,
    when rendered, will display an animation of the 1st object smoothly
    changing (morphing) into the 2nd object, then to the 3rd and so on,
    until the last object. In our example,
    the blue sphere will grow larger and larger and will fade into the white
    color. Of course, any kind of models is allowed --- e.g. it can
    be a walking man at various stages, so in effect you get an animation
    of walking man.

    A special case when you pass only one scene to this class is allowed
    (it may be handy in some situations). This will obviously produce
    just a still result, i.e. resulting TVRMLGLAnimation will be just
    a wrapper around single TVRMLFlatSceneGL instance.

    All given objects must be "structurally equal"
    --- the same nodes hierarchy, the same names of nodes,
    the same values of all fields (with the exception of fields
    that are interpolated: SFColor, SFFloat, SFMatrix,
    SFRotation, SFVec2f, SFVec3f and equivalent MFXxx fields).
    For multi-fields (MFXxx) that can be interpolated: note that values
    of items may differ, but still the counts of items must be equal.

    Note that this is not the perfect way to design animations ---
    a much better way would be to write the animation details in VRML
    file, use a modeller that allows you to design animations and store them
    in such way in VRML file, and read animations from this VRML file.
    But this means that many things must be done that currently are not
    ready --- VRML 2.0 (and X3D) support must be done (they allow to write
    data about how model should be animated), and Blender exporter to VRML
    must store Blender animation data in VRML.

    Until the things mentioned above are done, this class allows you
    to create animations by simply making two or more VRML scenes
    with various state of the same model. In many cases this should be
    acceptable solution. }
  TVRMLGLAnimation = class
  private
    FScenes: TVRMLFlatSceneGLsList;
    function GetScenes(I: Integer): TVRMLFlatSceneGL;
    Renderer: TVRMLOpenGLRenderer;
    FTimeBegin, FTimeEnd: Single;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FOwnsFirstRootNode: boolean;

    procedure Close;
  public
    { Constructor.

      @param(RootNodes
        Models describing the "predefined" frames
        of animation.

        For all nodes except the first: They are @italic(always)
        owned by this class --- that's needed,
        because actually we may do some operations on these models when
        building animation (including even freeing some RootNodes,
        if we will find that they are equivalent to some other RootNodes).
        They all must point to different objects.)

      @param(Times Array specifying the point of time for each "predefined"
        frame. Length of this array must equal to length of RootNodes array.)

      @param(ScenesPerTime
        This says how many scenes will be used for period of time equal to 1.0.
        This will determine Scenes.Count.
        RootNodes[0] always takes Scenes[0] and RootNodes[High(RootNodes)]
        always takes Scenes[Scenes.High].

        Note that if we will find that some nodes along the way are
        exactly equal, we may drop scenes count between --- because if they are
        both equal, we can simply render the same scene for some period
        of time.)

      @param(AOptimization
        This is passed to TVRMLFlatSceneGL constructor, see there for docs.

        Note that this class should generally use roSeparateShapeStatesNoTransform
        or roSeparateShapeStates for Optimization, to conserve memory
        in some common cases. See docs at TGLRendererOptimization type.)

      @param(EqualityEpsilon
        This will be used for comparing fields, to decide if two fields
        (and, consequently, nodes) are equal. It will be simply
        passed to TVRMLField.Equals.

        You can pass here 0 to use exact comparison, but it's
        adviced to use here something > 0. Otherwise we could waste
        display list memory (and loading time) for many frames of the
        same node that are in fact equal.)

      @raises(EModelsStructureDifferent
        When models in RootNode1 and RootNode2 are not structurally equal
        (see TVRMLGLAnimation comments for the precise meaning of
        "structurally equal" models).)

      @noAutoLinkHere }
    constructor Create(
      RootNodes: TVRMLNodesList;
      AOwnsFirstRootNode: boolean;
      ATimes: TDynSingleArray;
      ScenesPerTime: Cardinal;
      AOptimization: TGLRendererOptimization;
      const EqualityEpsilon: Single;
      ACache: TVRMLOpenGLRendererContextCache = nil);

    { This creates TVRMLGLAnimation instance by loading it's parameters
      (models to use, times to use etc.) from given file.
      File format is described in ../../doc/kanim_format.txt file.

      Note that after such animation is created, you cannot change
      some of it's rendering parameters like ScenesPerTime and AOptimization
      --- they are already set as specified in the file.
      If you need more control from your program's code,
      you should use something more flexible (and less comfortable to use)
      like LoadFromFile class procedure.

      Note that you can change TimeLoop and TimeBackwards --- since these
      properties are writeable after the instance is created. }
    constructor CreateFromFile(
      const FileName: string;
      ACache: TVRMLOpenGLRendererContextCache = nil);

    { @noAutoLinkHere }
    destructor Destroy; override;

    property OwnsFirstRootNode: boolean
      read FOwnsFirstRootNode;

    { You can read anything from Scenes below. But you cannot set some
      things: don't set their scenes Attributes properties.
      Use only our @link(Attributes).

      @noAutoLinkHere }
    property Scenes[I: Integer]: TVRMLFlatSceneGL read GetScenes;
    function ScenesCount: Integer;

    { Just a shortcut for Scenes[0]. }
    function FirstScene: TVRMLFlatSceneGL;

    { Just a shortcut for Scenes[ScenesCount - 1]. }
    function LastScene: TVRMLFlatSceneGL;

    { Prepare all scenes for rendering. This just calls
      PrepareRender(...) for all Scenes.

      If ProgressStep then it will additionally call Progress.Step after
      preparing each scene (it will call it ScenesCount times).

      If FreeRootNodes then it will free (and set to nil) RootNode
      of each scene after preparing. This is somewhat dangerous
      (because you have to be careful then about what methods
      from scenes you use), but it allows you to save some memory.
      Note that if OwnsFirstRootNode then the initial RootNodes[0]
      will *not* be freed. }
    procedure PrepareRender(DoPrepareBackground, DoPrepareBoundingBox,
      DoPrepareTrianglesListNotOverTriangulate,
      DoPrepareTrianglesListOverTriangulate: boolean;
      ProgressStep: boolean;
      FreeRootNodes: boolean);

    { Close anything associated with current OpenGL context in this class.
      This calls CloseGL on every Scenes[], and additionally may close
      some other internal things here. }
    procedure CloseGL;

    { Just a shortcut for TimeEnd - TimeBegin. }
    function TimeDuration: Single;

    { This is TimeDuration * 2 if TimeBackwards, otherwise it's just
      TimeDuration. In other words, this is the time of the one "full"
      (forward + backward) animation. }
    function TimeDurationWithBack: Single;

    { First and last time that you passed to constructor.
      In other words, Times[0] and Times[High(Times)].
      @groupBegin }
    property TimeBegin: Single read FTimeBegin;
    property TimeEnd: Single read FTimeEnd;
    { @groupEnd }

    { This will return appropriate scene from @link(Scenes) based on given
      Time. If Time is between given TimeBegin and TimeEnd,
      then this will be appropriate scene in the middle.

      For Time outside the range TimeBegin .. TimeEnd
      behavior depends on TimeLoop and TimeBackwards properties:

      @unorderedList(
        @item(When not TimeLoop and not TimeBackwards then:

          If Time is < TimeBegin, always the first scene will
          be returned. If Time is > TimeEnd, always the last scene will
          be returned.

          So there will no real animation outside
          TimeBegin .. TimeEnd timeline.)

        @item(When not TimeLoop and TimeBackwards then:

          If Time is < TimeBegin, always the first scene will
          be returned. If Time is between TimeEnd and
          TimeEnd + TimeDuration, then the animation
          will be played backwards. When Time is > TimeEnd + TimeDuration,
          again always the first scene will be returned.

          So between TimeEnd and TimeEnd + TimeDuration
          animation will be played backwards, and
          there will no real animation outside
          TimeBegin .. TimeEnd + TimeDuration timeline.)

        @item(When TimeLoop and not TimeBackwards then:

          Outside TimeBegin .. TimeEnd, animation will cycle.
          This means that e.g. between TimeEnd and TimeEnd + TimeDuration
          animation will be played just like between TimeBegin and TimeEnd.)

        @item(When TimeLoop and TimeBackwardsm then:

          Outside TimeBegin .. TimeEnd, animation will cycle.
          Cycle between TimeEnd and TimeEnd + TimeDuration will
          go backwards. Cycle between TimeEnd + TimeDuration
          and TimeEnd + TimeDuration * 2 will again go forward.
          And so on.)
      )
    }
    function SceneFromTime(const Time: Single): TVRMLFlatSceneGL;

    { See SceneFromTime for description what this property does. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop default true;

    { See SceneFromTime for description what this property does. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards default false;

    { Attributes controlling rendering.
      See TVRMLSceneRenderingAttributes and TVRMLRenderingAttributes
      for documentation of properties.

      You can change properties of this
      object at any time, but beware that some changes may force
      time-consuming regeneration of some things (like OpenGL display lists)
      in the nearest Render of the scenes.
      So explicitly calling PrepareRender may be useful after
      changing these Attributes.

      @noAutoLinkHere }
    function Attributes: TVRMLSceneRenderingAttributes;

    { Load TVRMLGLAnimation data from a given FileName
      to a set of variables.

      See ../../doc/kanim_format.txt for specification of the file format.

      This is a @italic(class procedure) --- it doesn't load the animation
      data to the given TVRMLGLAnimation instance. Instead it loads
      the data to your variables (passed as "out" params). In case
      of RootNodes and Times, you should pass here references to
      @italic(already created and currently empty) lists.

      ModelFileNames returned will always be absolute paths.
      We will expand them as necessary (actually, we will also expand
      passed here FileName to be absolute).

      If you seek for most comfortable way to load TVRMLGLAnimation from a file,
      you should use the correct TVRMLGLAnimation constructor like
      CreateFromFile. This procedure is more flexible --- it allows
      you to e.g. modify parameters before creating TVRMLGLAnimation
      instance, and it's usefull to implement a class like
      TVRMLGLAnimationInfo that also wants to read animation data,
      but doesn't have an TVRMLGLAnimation instance available. }
    class procedure LoadFromFile(const FileName: string;
      ModelFileNames: TDynStringArray;
      Times: TDynSingleArray;
      out ScenesPerTime: Cardinal;
      out AOptimization: TGLRendererOptimization;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);

    { Load TVRMLGLAnimation data from a given XML element
      to a set of variables.

      This is just like LoadFromFile, but it works using
      an Element. This way you can use it to load <animation> element
      that is a part of some larger XML file.

      It requires BasePath --- this is the path from which relative
      filenames inside Element will be resolved. (this path doesn't
      need to be an absolute path, we will expand it to make it absolute
      if necessary). }
    class procedure LoadFromDOMElement(Element: TDOMElement;
      const BasePath: string;
      ModelFileNames: TDynStringArray;
      Times: TDynSingleArray;
      out ScenesPerTime: Cardinal;
      out AOptimization: TGLRendererOptimization;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);
  end;

implementation

uses Math, KambiClassUtils, VectorMath, VRMLFields,
  ProgressUnit, XMLRead, KambiXMLUtils, KambiFilesUtils, Object3dAsVRML;

{ EModelsStructureDifferent --------------------------------------------------- }

constructor EModelsStructureDifferent.CreateFmt(const S: string;
  const Args: array of const);
begin
  inherited CreateFmt('Models are structurally different: ' + S, Args);
end;

{ TVRMLGLAnimation ------------------------------------------------------------ }

constructor TVRMLGLAnimation.Create(
  RootNodes: TVRMLNodesList;
  AOwnsFirstRootNode: boolean;
  ATimes: TDynSingleArray;
  ScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
  const EqualityEpsilon: Single;
  ACache: TVRMLOpenGLRendererContextCache = nil);

  { This will check that Model1 and Model2 are exactly equal,
    or that at least interpolating (see VRMLModelLerp) is possible.

    If models are structurally different (which means that even
    interpolating between Model1 and Model2 is not possible),
    it will raise EModelsStructureDifferent. }
  procedure CheckVRMLModelsStructurallyEqual(Model1, Model2: TVRMLNode);
  var
    I: Integer;
  begin
    { Yes, Model1 and Model2 must have *exactly* the same classes. }
    if Model1.ClassType <> Model2.ClassType then
      raise EModelsStructureDifferent.CreateFmt(
        'Different nodes classes: "%s" and "%s"',
        [Model1.ClassName <> Model2.ClassName]);

    (* TODO: I would prefer to use here interface code, like
      if Supports(Model1, INodeGeneralInline) and
       Supports(Model2, INodeGeneralInline) then
       begin
         { Make sure that *Inline content is loaded now. }
         INodeGeneralInline(Model1).LoadInlined(false);
         INodeGeneralInline(Model2).LoadInlined(false);
       end; *)

    if Model1 is TNodeWWWInline then
    begin
      TNodeWWWInline(Model1).LoadInlined(false);
      TNodeWWWInline(Model2).LoadInlined(false);
    end else
    if Model1 is TNodeInline then
    begin
      TNodeInline(Model1).LoadInlined(false);
      TNodeInline(Model2).LoadInlined(false);
    end else
    if Model1 is TNodeInlineLoadControl then
    begin
      TNodeInlineLoadControl(Model1).LoadInlined(false);
      TNodeInlineLoadControl(Model2).LoadInlined(false);
    end;

    if Model1.NodeName <> Model2.NodeName then
      raise EModelsStructureDifferent.CreateFmt(
        'Different names of nodes: "%s" and "%s"',
        [Model1.NodeName, Model2.NodeName]);

    if Model1.WWWBasePath <> Model2.WWWBasePath then
      raise EModelsStructureDifferent.CreateFmt(
        'Different WWWBasePath of nodes: "%s" and "%s"',
        [Model1.WWWBasePath, Model2.WWWBasePath]);

    if Model1.ChildrenCount <> Model2.ChildrenCount then
      raise EModelsStructureDifferent.CreateFmt(
        'Different number of children in nodes: "%d" and "%d"',
        [Model1.ChildrenCount, Model2.ChildrenCount]);

    for I := 0 to Model1.ChildrenCount - 1 do
      CheckVRMLModelsStructurallyEqual(Model1.Children[I], Model2.Children[I]);

    { Yes, the situation below can happen. *Usually* when we know
      that Model1 and Model2 are equal classes then we know that
      they have the same number of fields of the same type.
      However, for TNodeUnknown, it's not that easy. Two different instances
      of TNodeUnknown class may have completely different fields,
      so we must safeguard against this. }
    if Model1.Fields.Count <> Model2.Fields.Count then
      raise EModelsStructureDifferent.CreateFmt(
        'Different number of fields in nodes: "%d" and "%d"',
        [Model1.Fields.Count, Model2.Fields.Count]);

    for I := 0 to Model1.Fields.Count - 1 do
    begin
      if Model1.Fields[I].ClassType <> Model2.Fields[I].ClassType then
        raise EModelsStructureDifferent.CreateFmt(
          'Different type of field number %d in nodes: "%s" and "%s"',
          [I, Model1.Fields[I].ClassName, Model2.Fields[I].ClassName]);

      {$define CheckMFStructuralEquality :=
        begin
          try
            (Model1.Fields[I] as TVRMLMultField).CheckCountEqual
              (Model2.Fields[I] as TVRMLMultField);
          except
            (* Translate EVRMLMultFieldDifferentCount exception
               (may be raised by TVRMLMultField.CheckCountEqual above)
               to EModelsStructureDifferent. *)
            on E: EVRMLMultFieldDifferentCount do
              raise EModelsStructureDifferent.CreateFmt('%s', [E.Message]);
          end;
        end}

      if Model1.Fields[I] is TSFColor    then begin { no need to check anything } end else
      if Model1.Fields[I] is TSFFloat    then begin { no need to check anything } end else
      if Model1.Fields[I] is TSFMatrix   then begin { no need to check anything } end else
      if Model1.Fields[I] is TSFRotation then begin { no need to check anything } end else
      if Model1.Fields[I] is TSFVec2f    then begin { no need to check anything } end else
      if Model1.Fields[I] is TSFVec3f    then begin { no need to check anything } end else
      if Model1.Fields[I] is TMFColor    then CheckMFStructuralEquality else
      if Model1.Fields[I] is TMFVec2f    then CheckMFStructuralEquality else
      if Model1.Fields[I] is TMFVec3f    then CheckMFStructuralEquality else
      if Model1.Fields[I] is TMFFloat    then CheckMFStructuralEquality else

      {$undef CheckMFStructuralEquality}

      begin
        { Check fields for equality.

          Some special fields like TNodeWWWInline.FdName do not
          have to be equal, as they don't have any role for the
          "real" meaning of the model. I mean, if TNodeWWWInline
          children (loaded from pointed file) have the same structure,
          then we're happy. And it's handy to allow this --- see e.g.
          examples/models/gus_1_final.wrl and
          examples/models/gus_2_final.wrl trick. }

        if not (
           ( (Model1 is TNodeWWWInline)         and (Model1.Fields[I].Name = 'name') ) or
           ( (Model2 is TNodeInline)            and (Model1.Fields[I].Name = 'url') ) or
           ( (Model1 is TNodeInlineLoadControl) and (Model1.Fields[I].Name = 'url') ) or
           Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon)
           ) then
          raise EModelsStructureDifferent.CreateFmt(
            'Fields "%s" (class "%s") are not equal',
            [Model1.Fields[I].Name, Model1.Fields[I].ClassName]);
      end;
    end;
  end;

  { This will merge equal children of Model1 and Model2,
    and check that Model1 and Model2 are exactly equal.

    It assumes that models are structurally equal, i.e. that you
    already did run CheckVRMLModelsStructurallyEqual over them.

    It works recursively: first it checks for every children
    are they equal. For each pair that is equal, it replaces
    given children in Model2 with appropriate children of Model1.
    At the end, if every children pair was equal and additionally
    if all fields are equal, then it returns true.

    Such copying of references is useful, because then we simply copy given
    node's reference instead of duplicating this object.
    This way Model1 and Model2 and all models interpolated along the way
    can share the same object reference. This is very good, because:

    1. If nodes are equal then creating new object each
       time would mean that I create a lot of objects with exactly the
       same contents. So memory is wasted, without any good reason.

    2. For nodes like Texture2, this is good because then the image
       is loaded from the file only once. This means that memory is saved,
       once again. This also means that in case when texture file doesn't
       exist, user gets only 1 warning/error message (instead of getting
       warning/error message for each duplicated TNodeTexture2 instance).

    3. Also for nodes like Texture2, this means that if we use the same
       VRMLOpenGLRenderer to render every model of the animation,
       then VRMLOpenGLRenderer will recognize this and given texture
       will be loaded only once for OpenGL. So loading time and
       memory are saved *once again*  (otherwise OpenGL would allocate
       internal copy of texture for each duplicated node, once again
       wasting a lot of memory).

    4. And later the ShapeState cache of TVRMLOpenGLRenderer can speed
       up loading time and conserve memory use, if it sees the same
       reference to given ShapeNode twice. }
  function VRMLModelsMerge(Model1, Model2: TVRMLNode): boolean;
  var
    I: Integer;
  begin
    Result := true;

    { Note that this loop will iterate over every Children,
      even if somewhere along the way we will already set Result to false.
      Even if we already know that Result is false, we stil want to
      merge Model1 and Model2 children as much as we can. }
    for I := 0 to Model1.ChildrenCount - 1 do
    begin
      if VRMLModelsMerge(Model1.Children[I], Model2.Children[I]) then
      begin
        { Tests: Writeln('merged child ', I, ' of class ',
          Model1.Children[I].NodeTypeName); }
        Model1.Children[I] := Model2.Children[I];
      end else
        Result := false;
    end;

    if not Result then Exit;

    for I := 0 to Model1.Fields.Count - 1 do
    begin
      {$define CheckEqualitySetResult :=
        begin
          if not Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon) then
            Exit(false);
        end}

      if Model1.Fields[I] is TSFColor    then CheckEqualitySetResult else
      if Model1.Fields[I] is TSFFloat    then CheckEqualitySetResult else
      if Model1.Fields[I] is TSFMatrix   then CheckEqualitySetResult else
      if Model1.Fields[I] is TSFRotation then CheckEqualitySetResult else
      if Model1.Fields[I] is TSFVec2f    then CheckEqualitySetResult else
      if Model1.Fields[I] is TSFVec3f    then CheckEqualitySetResult else
      if Model1.Fields[I] is TMFColor    then CheckEqualitySetResult else
      if Model1.Fields[I] is TMFVec2f    then CheckEqualitySetResult else
      if Model1.Fields[I] is TMFVec3f    then CheckEqualitySetResult else
      if Model1.Fields[I] is TMFFloat    then CheckEqualitySetResult;

      { Other fields were already checked by CheckVRMLModelsStructurallyEqual }

      {$undef CheckEqualitySetResult}
    end;
  end;

  { Linear interpolation between Model1 and Model2.
    A = 0 means Model1, A = 1 means Model2, A between 0 and 1 is lerp
    between Model1 and Model2.

    If Model1 and Model2 are the same object (the same references),
    then this will return just Model1. This way it keeps memory optimization
    described by VRMLModelsMerge. }
  function VRMLModelLerp(const A: Single; Model1, Model2: TVRMLNode): TVRMLNode;
  var
    I: Integer;
  begin
    if Model1 = Model2 then
      Exit(Model1);

    Result := TVRMLNodeClass(Model1.ClassType).Create(Model1.NodeName,
      Model1.WWWBasePath);
    try
      { TODO: the code below doesn't deal efficiently with the situation when single
        TVRMLNode is used as a child many times in one of the nodes.
        (through VRML "USE" keyword). Code below will then unnecessarily
        create copies of such things (wasting construction time and memory),
        instead of reusing the same object reference. }
      for I := 0 to Model1.ChildrenCount - 1 do
        Result.AddChild(VRMLModelLerp(A, Model1.Children[I], Model2.Children[I]));

      { TODO: for TNodeUnknown, we should fill here Result.Fields. }

      for I := 0 to Model1.Fields.Count - 1 do
      begin
        if Model1.Fields[I] is TSFColor    then (Result.Fields[I] as TSFColor   ).AssignLerp(A, TSFColor   (Model1.Fields[I]), TSFColor   (Model2.Fields[I])) else
        if Model1.Fields[I] is TSFFloat    then (Result.Fields[I] as TSFFloat   ).AssignLerp(A, TSFFloat   (Model1.Fields[I]), TSFFloat   (Model2.Fields[I])) else
        if Model1.Fields[I] is TSFMatrix   then (Result.Fields[I] as TSFMatrix  ).AssignLerp(A, TSFMatrix  (Model1.Fields[I]), TSFMatrix  (Model2.Fields[I])) else
        if Model1.Fields[I] is TSFRotation then (Result.Fields[I] as TSFRotation).AssignLerp(A, TSFRotation(Model1.Fields[I]), TSFRotation(Model2.Fields[I])) else
        if Model1.Fields[I] is TSFVec2f    then (Result.Fields[I] as TSFVec2f   ).AssignLerp(A, TSFVec2f   (Model1.Fields[I]), TSFVec2f   (Model2.Fields[I])) else
        if Model1.Fields[I] is TSFVec3f    then (Result.Fields[I] as TSFVec3f   ).AssignLerp(A, TSFVec3f   (Model1.Fields[I]), TSFVec3f   (Model2.Fields[I])) else
        if Model1.Fields[I] is TMFColor    then (Result.Fields[I] as TMFColor   ).AssignLerp(A, TMFColor   (Model1.Fields[I]), TMFColor   (Model2.Fields[I])) else
        if Model1.Fields[I] is TMFVec2f    then (Result.Fields[I] as TMFVec2f   ).AssignLerp(A, TMFVec2f   (Model1.Fields[I]), TMFVec2f   (Model2.Fields[I])) else
        if Model1.Fields[I] is TMFVec3f    then (Result.Fields[I] as TMFVec3f   ).AssignLerp(A, TMFVec3f   (Model1.Fields[I]), TMFVec3f   (Model2.Fields[I])) else
        if Model1.Fields[I] is TMFFloat    then (Result.Fields[I] as TMFFloat   ).AssignLerp(A, TMFFloat   (Model1.Fields[I]), TMFFloat   (Model2.Fields[I])) else
        begin
          { These fields cannot be interpolated.
            So just copy to Result.Fields[I]. }
          Result.Fields[I].Assign(Model1.Fields[I]);
        end;
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;

  function CreateOneScene(Node: TVRMLNode;
    OwnsRootNode: boolean): TVRMLFlatSceneGL;
  begin
    Result := TVRMLFlatSceneGL.CreateProvidedRenderer(
      Node, OwnsRootNode, AOptimization, Renderer);
  end;

var
  I: Integer;
  RootNodesEqual: boolean;
  LastSceneIndex: Integer;
  LastSceneRootNode: TVRMLNode;
  SceneIndex: Integer;
begin
  inherited Create;

  FOwnsFirstRootNode := AOwnsFirstRootNode;

  Assert(RootNodes.Count = ATimes.Count);

  for I := 1 to RootNodes.High do
    CheckVRMLModelsStructurallyEqual(RootNodes[0], RootNodes[I]);

  FScenes := TVRMLFlatSceneGLsList.Create;

  Renderer := TVRMLOpenGLRenderer.Create(TVRMLSceneRenderingAttributes, ACache);

  FTimeBegin := ATimes[0];
  FTimeEnd := ATimes[ATimes.High];

  FTimeLoop := true;
  FTimeBackwards := false;

  { calculate FScenes contents now }

  { RootNodes[0] goes to FScenes[0], that's easy }
  FScenes.Count := 1;
  FScenes[0] := CreateOneScene(RootNodes[0], OwnsFirstRootNode);
  LastSceneIndex := 0;
  LastSceneRootNode := RootNodes[0];

  for I := 1 to RootNodes.High do
  begin
    { Now add RootNodes[I] }

    FScenes.Count := FScenes.Count +
      Max(1, Round((ATimes[I] - ATimes[I-1]) * ScenesPerTime));

    { Try to merge it with LastSceneRootNode.
      Then initialize FScenes[LastSceneIndex + 1 to FScenes.High]. }
    RootNodesEqual := VRMLModelsMerge(RootNodes[I], LastSceneRootNode);
    if RootNodesEqual then
    begin
      { In this case I don't waste memory, and I'm simply reusing
        LastSceneRootNode. Actually, I'm just copying FScenes[LastSceneIndex].
        This way I have a series of the same instances of TVRMLFlatSceneGL
        along the way. When freeing FScenes, we will be smart and
        avoid deallocating the same pointer twice. }
      RootNodes.FreeAndNil(I);
      for SceneIndex := LastSceneIndex + 1 to FScenes.High do
        FScenes[SceneIndex] := FScenes[LastSceneIndex];
    end else
    begin
      for SceneIndex := LastSceneIndex + 1 to FScenes.High - 1 do
        FScenes[SceneIndex] := CreateOneScene(VRMLModelLerp(
          MapRange(SceneIndex, LastSceneIndex, FScenes.High, 0.0, 1.0),
          LastSceneRootNode, RootNodes[I]), true);
      FScenes.Last := CreateOneScene(RootNodes[I], true);
      LastSceneRootNode := RootNodes[I];
    end;

    LastSceneIndex := FScenes.High;
  end;
end;

constructor TVRMLGLAnimation.CreateFromFile(
  const FileName: string;
  ACache: TVRMLOpenGLRendererContextCache = nil);
var
  { Vars from LoadFromFile }
  ModelFileNames: TDynStringArray;
  Times: TDynSingleArray;
  ScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
  EqualityEpsilon: Single;
  ATimeLoop, ATimeBackwards: boolean;

  RootNodes: TVRMLNodesList;
  I, J: Integer;
begin
  ModelFileNames := nil;
  Times := nil;

  try
    ModelFileNames := TDynStringArray.Create;
    Times := TDynSingleArray.Create;

    LoadFromFile(FileName, ModelFileNames, Times,
      ScenesPerTime, AOptimization, EqualityEpsilon, ATimeLoop, ATimeBackwards);

    Assert(ModelFileNames.Length = Times.Length);
    Assert(ModelFileNames.Length >= 1);

    RootNodes := TVRMLNodesList.Create;
    RootNodes.Count := ModelFileNames.Count;

    for I := 0 to ModelFileNames.High do
    try
      RootNodes[I] := LoadAsVRML(ModelFileNames[I]);
    except
      for J := 0 to I - 1 do
        RootNodes.FreeAndNil(J);
      raise;
    end;

    Create(RootNodes, true, Times, ScenesPerTime, AOptimization,
      EqualityEpsilon, ACache);
    TimeLoop := ATimeLoop;
    TimeBackwards := ATimeBackwards;

  finally
    FreeAndNil(ModelFileNames);
    FreeAndNil(Times);
    FreeAndNil(RootNodes);
  end;
end;

destructor TVRMLGLAnimation.Destroy;
begin
  Close;
  inherited;
end;

procedure TVRMLGLAnimation.Close;
var
  I: Integer;
begin
  { This is called from destructor, so this must always check whether
    things are <> nil before trying to free them. }

  CloseGL;

  if FScenes <> nil then
  begin
    { Now we must note that we may have a sequences of the same scenes
      on FScenes. So we must deallocate smartly, to avoid deallocating
      the same pointer more than once. }
    for I := 0 to FScenes.High - 1 do
    begin
      if FScenes[I] = FScenes[I+1] then
        FScenes[I] := nil { set to nil, just for safety } else
        FScenes.FreeAndNil(I);
    end;
    FScenes.FreeAndNil(FScenes.High);

    FreeAndNil(FScenes);
  end;

  FreeAndNil(Renderer);
end;

function TVRMLGLAnimation.GetScenes(I: Integer): TVRMLFlatSceneGL;
begin
  Result := FScenes[I];
end;

function TVRMLGLAnimation.ScenesCount: Integer;
begin
  Result := FScenes.Count;
end;

function TVRMLGLAnimation.FirstScene: TVRMLFlatSceneGL;
begin
  Result := FScenes.First;
end;

function TVRMLGLAnimation.LastScene: TVRMLFlatSceneGL;
begin
  Result := FScenes.Last;
end;

procedure TVRMLGLAnimation.PrepareRender(
  DoPrepareBackground, DoPrepareBoundingBox,
  DoPrepareTrianglesListNotOverTriangulate,
  DoPrepareTrianglesListOverTriangulate: boolean;
  ProgressStep, FreeRootNodes: boolean);
var
  I: Integer;
begin
  for I := 0 to FScenes.High do
  begin
    FScenes[I].PrepareRender(DoPrepareBackground, DoPrepareBoundingBox,
      DoPrepareTrianglesListNotOverTriangulate,
      DoPrepareTrianglesListOverTriangulate);

    { We check FScenes[I].OwnsRootNode here, because if OwnsFirstRootNode
      was false then FScenes[I].OwnsRootNode will be false. }

    if FreeRootNodes and FScenes[I].OwnsRootNode then
    begin
      FScenes[I].RootNode.Free;
      FScenes[I].RootNode := nil;
    end;

    if ProgressStep then
      Progress.Step;
  end;
end;

procedure TVRMLGLAnimation.CloseGL;
{ Note that this is called from destructor, so we must be extra careful
  here and check is everything <> nil before freeing it. }
begin
  if FScenes <> nil then
    FScenes.CloseGL;

  if Renderer <> nil then
    Renderer.UnprepareAll;
end;

function TVRMLGLAnimation.TimeDuration: Single;
begin
  Result := TimeEnd - TimeBegin;
end;

function TVRMLGLAnimation.TimeDurationWithBack: Single;
begin
  Result := TimeDuration;
  if TimeBackwards then
    Result *= 2;
end;

function TVRMLGLAnimation.SceneFromTime(const Time: Single): TVRMLFlatSceneGL;
var
  SceneNumber: Integer;
  DivResult: SmallInt;
  ModResult: Word;
begin
  if FScenes.Count = 1 then
  begin
    { In this case TimeBegin = TimeEnd, so it's better to not perform
      any MapRange(Time, TimeBegin, TimeEnd, ...) calculation here
      and just treat this as a special case. }
    SceneNumber := 0;
  end else
  begin
    SceneNumber := Round(MapRange(Time, TimeBegin, TimeEnd, 0, FScenes.High));

    DivUnsignedMod(SceneNumber, FScenes.Count, DivResult, ModResult);

    if TimeLoop then
    begin
      if TimeBackwards and Odd(DivResult) then
        SceneNumber := FScenes.High - ModResult else
        SceneNumber := ModResult;
    end else
    begin
      if TimeBackwards then
      begin
        if (DivResult < 0) or (DivResult > 1) then
          SceneNumber := 0 else
        if DivResult = 1 then
          SceneNumber := FScenes.High - ModResult;
          { else DivResult = 0, so SceneNumber is already correct }
      end else
      begin
        if DivResult < 0 then
          SceneNumber := 0 else
        if DivResult > 0 then
          SceneNumber := FScenes.High;
      end;
    end;
  end;

  Result := FScenes[SceneNumber];
end;

function TVRMLGLAnimation.Attributes: TVRMLSceneRenderingAttributes;
begin
  Result := TVRMLSceneRenderingAttributes(Renderer.Attributes);
end;

class procedure TVRMLGLAnimation.LoadFromFile(const FileName: string;
  ModelFileNames: TDynStringArray;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out AOptimization: TGLRendererOptimization;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  Document: TXMLDocument;
begin
  ReadXMLFile(Document, FileName);
  try
    LoadFromDOMElement(Document.DocumentElement,
      ExtractFilePath(FileName),
      ModelFileNames, Times, ScenesPerTime, AOptimization,
      EqualityEpsilon, ATimeLoop, ATimeBackwards);
  finally FreeAndNil(Document); end;
end;

const
  DefaultKAnimScenesPerTime = 30;
  DefaultKAnimOptimization = roSeparateShapeStatesNoTransform;
  DefaultKAnimEqualityEpsilon = 0.001;
  DefaultKAnimLoop = false;
  DefaultKAnimBackwards = false;

class procedure TVRMLGLAnimation.LoadFromDOMElement(
  Element: TDOMElement;
  const BasePath: string;
  ModelFileNames: TDynStringArray;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out AOptimization: TGLRendererOptimization;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  AbsoluteBasePath: string;
  FrameElement: TDOMElement;
  Children: TDOMNodeList;
  I: Integer;
  FrameTime: Single;
  FrameFileName: string;
  Attr: TDOMAttr;
begin
  Assert(Times.Length = 0);
  Assert(ModelFileNames.Length = 0);

  AbsoluteBasePath := ExpandFileName(BasePath);

  Check(Element.TagName = 'animation',
    'Root node of an animation XML file must be <animation>');

  { Assign default values for optional attributes }
  ScenesPerTime := DefaultKAnimScenesPerTime;
  AOptimization := DefaultKAnimOptimization;
  EqualityEpsilon := DefaultKAnimEqualityEpsilon;
  ATimeLoop := DefaultKAnimLoop;
  ATimeBackwards := DefaultKAnimBackwards;

  for I := 0 to Integer(Element.Attributes.Length) - 1 do
  begin
    Attr := Element.Attributes.Item[I] as TDOMAttr;
    if Attr.Name = 'scenes_per_time' then
      ScenesPerTime := StrToInt(Attr.Value) else
    if Attr.Name = 'optimization' then
      AOptimization := RendererOptimizationFromName(Attr.Value, true) else
    if Attr.Name = 'equality_epsilon' then
      EqualityEpsilon := StrToFloat(Attr.Value) else
    if Attr.Name = 'loop' then
      ATimeLoop := StrToBool(Attr.Value) else
    if Attr.Name = 'backwards' then
      ATimeBackwards := StrToBool(Attr.Value) else
      raise Exception.CreateFmt('Unknown attribute of <animation> element: "%s"',
        [Attr.Name]);
  end;

  Children := Element.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
      if Children.Item[I].NodeType = ELEMENT_NODE then
      begin
        FrameElement := Children.Item[I] as TDOMElement;
        Check(FrameElement.TagName = 'frame',
          'Each child of <animation> element must be a <frame> element');

        if not DOMGetSingleAttribute(FrameElement, 'time', FrameTime) then
          raise Exception.Create('<frame> element must have a "time" attribute');

        if not DOMGetAttribute(FrameElement, 'file_name', FrameFileName) then
          raise Exception.Create('<frame> element must have a "file_name" attribute');

        { Make FrameFileName absolute, treating it as relative vs
          AbsoluteBasePath }
        FrameFileName := CombinePaths(AbsoluteBasePath, FrameFileName);

        if (Times.Count > 0) and (FrameTime <= Times.Items[Times.High]) then
          raise Exception.Create(
            'Frames within <animation> element must be specified in ' +
            'increasing time order');

        ModelFileNames.AppendItem(FrameFileName);
        Times.AppendItem(FrameTime);
      end;

    if ModelFileNames.Count = 0 then
      raise Exception.Create(
        'At least one <frame> is required within <animation> element');
  finally Children.Release end;
end;

end.