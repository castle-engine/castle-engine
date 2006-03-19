{ TVRMLGLAnimation class. }
unit VRMLGLAnimation;

interface

uses SysUtils, VRMLNodes, VRMLFlatSceneGL;

type
  EModelsStructureDifferent = class(Exception)
    constructor CreateFmt(const S: string; const Args: array of const);
  end;

  { This is an animation of VRML model by interpolating between two model
    states.

    When constructing object of this class,
    you must provide two VRML models that have exactly the same structure,
    but possibly different values for various fields. For example,
    first object may be a small sphere with blue color, the other
    object may be a larger sphere with white color.
    Then this class creates a list of @link(Scenes) such that
    @unorderedList(
      @itemSpacing Compact
      @item the first scene on the list is exactly the 1st object
      @item the last scene on the list is exactly the 2nd object
      @item(intermediate scenes are accordingly interpolated between
        the first and last scene)
    )

    In effect, the @link(Scenes) list is a list of scenes that,
    when rendered, will display an animation of the 1st object smoothly
    changing (morphing) into the 2nd object. In our example,
    the blue sphere will grow larger and larger and will fade into the white
    color. Of course, any kind of models is allowed --- e.g. it can
    be a walking man at various stages, so in effect you get an animation
    of walking man.

    Of course two given objects must be "structurally equal"
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
    to somehow create animations by simply making two or more VRML scenes
    with various state of the same model. In many cases this should be
    acceptable solution.

    TODO: textures should be shared by all constructed scenes,
    not loaded and kept separately. Right now for larger ScenesCount
    it's impossible to use a scene with some textures, because textures
    get loaded many times and eat a *lot* of memory. Both in my space
    and in OpenGL lib space. VRMLOpenGLRenderer is flexible in this regard,
    VRMLFlatSceneGL must be also made flexible and this class must use it. }
  TVRMLGLAnimation = class
  private
    FScenes: TVRMLFlatSceneGLsList;
    function GetScenes(I: Integer): TVRMLFlatSceneGL;
  public
    { Constructor.
      @param(RootNode1 Model describing first frame of animation.
        It's owned by this class --- that's needed, because actually
        we may do some operations on this model when building animation
        (including even freeing RootNode1, if we will find that it's
        actually equal to RootNode2))
      @param(RootNode2 Model describing last frame of animation.
        Analogous to RootNode1 --- it's owned by this class.)
      @param(ScenesCount
        This will set Scenes.Count, it must be >= 2
        (RootNode1 always takes Scenes[0] and RootNode2 always takes
        Scenes[Scenes.High]).
        Note that if we will find that RootNode1 and RootNode2 are
        exactly equal, we may change ScenesCount to 1.)
      @param(AOptimization
        This is passed to TVRMLFlatSceneGL constructor, see there for docs.)
      @raises(EModelsStructureDifferent
        When models in RootNode1 and RootNode2 are not structurally equal
        (see TVRMLGLAnimation comments for the precise meaning of
        "structurally equal" models).)

      @noAutoLinkHere }
    constructor Create(
      RootNode1: TVRMLNode;
      RootNode2: TVRMLNode;
      ScenesCount: Cardinal;
      AOptimization: TGLRendererOptimization);

    { @noAutoLinkHere }
    destructor Destroy; override;

    { @noAutoLinkHere }
    property Scenes[I: Integer]: TVRMLFlatSceneGL read GetScenes;
    function ScenesCount: Integer;

    { Call CloseGL on every Scenes[] }
    procedure ScenesCloseGLAll;
  end;

implementation

uses KambiClassUtils, VectorMath, VRMLFields;

{ EModelsStructureDifferent --------------------------------------------------- }

constructor EModelsStructureDifferent.CreateFmt(const S: string;
  const Args: array of const);
begin
  inherited CreateFmt('Models are structurally different: ' + S, Args);
end;

{ TVRMLGLAnimation ------------------------------------------------------------ }

constructor TVRMLGLAnimation.Create(
  RootNode1: TVRMLNode;
  RootNode2: TVRMLNode;
  ScenesCount: Cardinal;
  AOptimization: TGLRendererOptimization);

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

    if Model1 is TNodeWWWInline then
    begin
      { Make sure that WWWInline is loaded now. }
      (Model1 as TNodeWWWInline).LoadInlined(false);
      (Model2 as TNodeWWWInline).LoadInlined(false);
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
           ( (Model1 is TNodeWWWInline) and (Model1.Fields[I].Name = 'name') ) or
           Model1.Fields[I].Equals(Model2.Fields[I])
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
       wasting a lot of memory). }
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
          if not Model1.Fields[I].Equals(Model2.Fields[I]) then
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

var
  I: Integer;
  RootNodesEqual: boolean;
begin
  inherited Create;

  Assert(ScenesCount >= 2);

  CheckVRMLModelsStructurallyEqual(RootNode1, RootNode2);

  { do VRMLModelsMerge,
    calculate RootNodesEqual,
    eventually change ScenesCount to 1 }
  RootNodesEqual := VRMLModelsMerge(RootNode1, RootNode2);
  if RootNodesEqual then
  begin
    ScenesCount := 1;
    FreeAndNil(RootNode2);
  end;

  FScenes := TVRMLFlatSceneGLsList.Create;
  FScenes.Count := ScenesCount;

  if RootNodesEqual then
  begin
    FScenes[0] := TVRMLFlatSceneGL.Create(RootNode1, true, AOptimization);
  end else
  begin
    FScenes.First :=
      TVRMLFlatSceneGL.Create(RootNode1, true, AOptimization);
    FScenes.Last :=
      TVRMLFlatSceneGL.Create(RootNode2, true, AOptimization);
    for I := 1 to FScenes.Count - 2 do
      FScenes[I] := TVRMLFlatSceneGL.Create(
        VRMLModelLerp(I / FScenes.High, RootNode1, RootNode2),
        true, AOptimization);
  end;
end;

destructor TVRMLGLAnimation.Destroy;
begin
  FreeWithContentsAndNil(FScenes);
  inherited;
end;

function TVRMLGLAnimation.GetScenes(I: Integer): TVRMLFlatSceneGL;
begin
  Result := FScenes[I];
end;

function TVRMLGLAnimation.ScenesCount: Integer;
begin
  Result := FScenes.Count;
end;

procedure TVRMLGLAnimation.ScenesCloseGLAll;
begin
  FScenes.CloseGLAll;
end;

end.