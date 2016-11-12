{
  Copyright 2006-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Powerful automatic interpolation for VRML/X3D node hierarchies,
  used by CastlePrecalculatedAnimation now. }
unit CastleInternalNodeInterpolator;

{$I castleconf.inc}

interface

uses Classes,
  CastleUtils, X3DNodes;

type
  TGetKeyNodeWithTime = procedure (const Index: Cardinal;
    out KeyNode: TX3DRootNode; out Time: Single) of object;

  TNodeInterpolator = class
  strict private
    { Helpers for Load implementation. }
    LoadToX3D_KeyNodes: TX3DNodeList; static;
    LoadToX3D_KeyTimes: TSingleList; static;
    class procedure LoadToX3D_GetKeyNodeWithTime(const Index: Cardinal;
      out KeyNode: TX3DRootNode; out Time: Single);
  public
    const
      DefaultScenesPerTime = 30;
      DefaultEqualityEpsilon = 0.001;
      DefaultAnimationName = 'animation';

    { Load animation data from castle-anim-frames file (in a given URL) to a set of variables.
      See [http://castle-engine.sourceforge.net/castle_animation_frames.php]
      for specification of the file format.

      In case of KeyNodes and KeyTimes,
      you should pass here references to
      @italic(already created and currently empty) lists.

      If you seek for most comfortable way to load TCastlePrecalculatedAnimation from a file,
      you probably want to use TCastlePrecalculatedAnimation.LoadFromFile.
      This procedure is more flexible --- it allows
      you to e.g. modify parameters before creating TCastlePrecalculatedAnimation
      instance, and it's usefull to implement a class like
      TCastlePrecalculatedAnimationInfo that also wants to read animation data,
      but doesn't have an TCastlePrecalculatedAnimation instance available. }
    class procedure LoadAnimFramesToKeyNodes(const URL: string;
      const KeyNodes: TX3DNodeList;
      const KeyTimes: TSingleList;
      out ScenesPerTime: Cardinal;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);

    { From key nodes, create a series of baked nodes (with final
      animation pose already calculated) representing an animation.

      KeyNodesCount must always be at least one,
      you cannot create animation from an empty list.

      The returned list contains only TX3DRootNode instances.
      It's never empty. Must be freed by the caller. }
    class function BakeToSequence(const GetKeyNodeWithTime: TGetKeyNodeWithTime;
      const KeyNodesCount: Cardinal;
      const ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single;
      out TimeBegin, TimeEnd: Single): TX3DNodeList;

    { Convert a node list to animate (like the one from MD3 loader,
      or from TNodeInterpolator.BakeToSequence,
      which may come from castle-anim-frames file or from generated animation)
      into a simple X3D graph that animates it (using TimeSensor,
      IntegerSequencer and Switch node, wrapping given here Nodes).
      This allows to read key nodes from any format,
      like a castle-anim-frames or MD3, and convert them to a simple X3D animation. }
    class function LoadSequenceToX3D(
      const Nodes: TX3DNodeList; const Duration: Single;
      const Loop, Backwards: boolean): TX3DRootNode;

    { Load a series of key X3D nodes into a X3D animation.
      This is a combined BakeToSequence (that interpolates and creates
      intermediate nodes from a list of key nodes) and LoadSequenceToX3D
      (that converts a sequence of nodes into an animation, using TimeSensor,
      Switch etc.) }
    class function LoadToX3D(
      const KeyNodes: TX3DNodeList;
      const KeyTimes: TSingleList;
      const ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single;
      const Loop, Backwards: boolean): TX3DRootNode;
  end;

implementation

uses SysUtils, XMLRead, DOM,
  CastleLog, X3DFields, CastleXMLUtils, CastleFilesUtils,
  CastleDownload, CastleURIUtils, X3DLoad, CastleClassUtils;

{ EModelsStructureDifferent -------------------------------------------------- }

type
  EModelsStructureDifferent = class(Exception)
    constructor CreateFmt(const S: string; const Args: array of const);
  end;

constructor EModelsStructureDifferent.CreateFmt(const S: string;
  const Args: array of const);
begin
  inherited CreateFmt('Models are structurally different: ' + S, Args);
end;

{ utilities for TNodeInterpolator.BakeToSequence ----------------------------- }

{ Check that Model1 and Model2 are exactly equal,
  or that at least interpolating (see NodesLerp) is possible.

  If models are structurally different (which means that even
  interpolating between Model1 and Model2 is not possible),
  it will raise EModelsStructureDifferent. }
procedure CheckNodesStructurallyEqual(Model1, Model2: TX3DNode;
  const EqualityEpsilon: Single);

  procedure CheckSFNodesStructurallyEqual(Field1, Field2: TSFNode);
  begin
    if (Field1.Value <> nil) and (Field2.Value <> nil) then
    begin
      CheckNodesStructurallyEqual(Field1.Value, Field2.Value, EqualityEpsilon);
    end else
    if not ((Field1.Value = nil) and (Field2.Value = nil)) then
      raise EModelsStructureDifferent.CreateFmt('Field "%s" of type SFNode ' +
        'is once NULL and once not-NULL', [Field1.Name]);
  end;

  procedure CheckMFNodesStructurallyEqual(Field1, Field2: TMFNode);
  var
    I: Integer;
  begin
    if Field1.Items.Count <> Field2.Items.Count then
      raise EModelsStructureDifferent.CreateFmt(
        'Different number of children in MFNode fields: "%d" and "%d"',
        [Model1.VRML1ChildrenCount, Model2.VRML1ChildrenCount]);

    for I := 0 to Field1.Items.Count - 1 do
      CheckNodesStructurallyEqual(Field1[I], Field2[I], EqualityEpsilon);
  end;

var
  I: Integer;
begin
  { Yes, Model1 and Model2 must have *exactly* the same classes. }
  if Model1.ClassType <> Model2.ClassType then
    raise EModelsStructureDifferent.CreateFmt(
      'Different nodes classes: "%s" and "%s"',
      [Model1.ClassName, Model2.ClassName]);

  { Make sure that *Inline content is loaded now. }
  if Model1 is TInlineNode then
  begin
    TInlineNode(Model1).LoadInlined(false);
    TInlineNode(Model2).LoadInlined(false);
  end;

  if Model1.NodeName <> Model2.NodeName then
    raise EModelsStructureDifferent.CreateFmt(
      'Different names of nodes: "%s" and "%s"',
      [Model1.NodeName, Model2.NodeName]);

  { We are interested whether Model1.BaseUrl and Model2.BaseUrl will
    give different results when using them to resolve relative URLs.
    Simply comparing them is not good --- they may contain filenames
    at the end. Stripping these filenames with ExtractURIPath
    is dirty. So we just test CombineURI with a test name. }
  if Model1.PathFromBaseUrl('test') <> Model2.PathFromBaseUrl('test') then
    raise EModelsStructureDifferent.CreateFmt(
      'BaseUrl of nodes different (will resolve relative URLs to different things): "%s" and "%s"',
      [Model1.BaseUrl, Model2.BaseUrl]);

  if Model1.VRML1ChildrenCount <> Model2.VRML1ChildrenCount then
    raise EModelsStructureDifferent.CreateFmt(
      'Different number of children in nodes: "%d" and "%d"',
      [Model1.VRML1ChildrenCount, Model2.VRML1ChildrenCount]);

  for I := 0 to Model1.VRML1ChildrenCount - 1 do
    CheckNodesStructurallyEqual(Model1.VRML1Children[I], Model2.VRML1Children[I], EqualityEpsilon);

  { Yes, the situation below can happen. *Usually* when we know
    that Model1 and Model2 are equal classes then we know that
    they have the same number of fields of the same type.
    However, for TX3DUnknownNode, it's not that easy. Two different instances
    of TX3DUnknownNode class may have completely different fields,
    so we must safeguard against this. }
  if Model1.FieldsCount <> Model2.FieldsCount then
    raise EModelsStructureDifferent.CreateFmt(
      'Different number of fields in nodes: "%d" and "%d"',
      [Model1.FieldsCount, Model2.FieldsCount]);

  for I := 0 to Model1.FieldsCount - 1 do
  begin
    if Model1.Fields[I].ClassType <> Model2.Fields[I].ClassType then
      raise EModelsStructureDifferent.CreateFmt(
        'Different type of field number %d in nodes: "%s" and "%s"',
        [I, Model1.Fields[I].ClassName, Model2.Fields[I].ClassName]);

    if Model1.Fields[I] is TSFNode then
      CheckSFNodesStructurallyEqual(
        TSFNode(Model1.Fields[I]), TSFNode(Model2.Fields[I])) else
    if Model1.Fields[I] is TMFNode then
      CheckMFNodesStructurallyEqual(
        TMFNode(Model1.Fields[I]), TMFNode(Model2.Fields[I])) else
    if Model1.Fields[I].CanAssignLerp then
    begin
      if Model1.Fields[I] is TX3DMultField then
      begin
        try
          (Model1.Fields[I] as TX3DMultField).CheckCountEqual
            (Model2.Fields[I] as TX3DMultField);
        except
          (* Translate EX3DMultFieldDifferentCount exception
             (may be raised by TX3DMultField.CheckCountEqual above)
             to EModelsStructureDifferent. *)
          on E: EX3DMultFieldDifferentCount do
            raise EModelsStructureDifferent.CreateFmt('%s', [E.Message]);
        end;
      end;
      { Else we have single-value field that can lerp.
        No need to check anything in this case,
        it's ready to go (that is, to lerp). }
    end else
    begin
      { Check fields for equality.

        Some special fields like TInlineNode.FdUrl do not
        have to be equal, as they don't have any role for the
        "real" meaning of the model. I mean, if TInlineNode.Inlined
        contents (loaded from pointed file) have the same structure,
        then we're happy. And it's handy to allow this --- see e.g.
        examples/models/gus_1_final.wrl and
        examples/models/gus_2_final.wrl trick. }

      if not (
         ( (Model1 is TInlineNode)            and (Model1.Fields[I].Name = 'url') ) or
         Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon)
         ) then
        raise EModelsStructureDifferent.CreateFmt(
          'Fields "%s" (class "%s") are not equal',
          [Model1.Fields[I].Name, Model1.Fields[I].ClassName]);
    end;
  end;
end;

{ Merge equal children of Model1 and Model2,
  and check that Model1 and Model2 are exactly equal.

  It assumes that models are structurally equal, i.e. that you
  already did run CheckNodesStructurallyEqual over them.

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

  2. For nodes like ImageTexture, this is good because then the image
     is loaded from the file only once. This means that memory is saved,
     once again. This also means that in case when texture file doesn't
     exist, user gets only 1 warning/error message (instead of getting
     warning/error message for each duplicated TImageTextureNode instance).

  3. Also for nodes like ImageTexture, this means that if we use the same
     GLRenderer to render every model of the animation,
     then GLRenderer will recognize this and given texture
     will be loaded only once for OpenGL. So loading time and
     memory are saved *once again*  (otherwise OpenGL would allocate
     internal copy of texture for each duplicated node, once again
     wasting a lot of memory).

     Although 2. and 3. are actually somewhat void right now,
     as we have a more general cache that caches texture resources
     even across different nodes right now (the only need is to have
     equal URLs).

  4. And later the Shape cache of TGLRenderer can speed
     up loading time and conserve memory use, if it sees the same
     reference to given GeometryNode twice. }
function NodesMerge(Model1, Model2: TX3DNode;
  const EqualityEpsilon: Single): boolean;

  function SFNodesMerge(Field1, Field2: TSFNode): boolean;
  begin
    Result := true;

    { Equality was already checked by CheckNodesStructurallyEqual,
      so now if one SFNode value is not nil, we know that the other
      one is not nil too. }
    if Field1.Value <> nil then
    begin
      if NodesMerge(Field1.Value, Field2.Value, EqualityEpsilon) then
        Field1.Value := Field2.Value else
        Result := false;
    end;
  end;

  function MFNodesMerge(Field1, Field2: TMFNode): boolean;
  var
    I: Integer;
  begin
    Result := true;

    { Note that we already know that Counts are equals,
      checked already by CheckNodesStructurallyEqual. }
    Assert(Field1.Items.Count = Field2.Items.Count);
    for I := 0 to Field1.Items.Count - 1 do
    begin
      if NodesMerge(Field1[I], Field2[I], EqualityEpsilon) then
      begin
        { Think of this as
            Field1[I] := Field2[I]
          but I can't call this directly, I must use Field1.Replace
          to not mess reference counts. }
        Field1.Replace(I, Field2[I]);
      end else
        Result := false;
    end;
  end;

var
  I: Integer;
begin
  Result := true;

  { Note that this loop will iterate over every Children,
    even if somewhere along the way we will already set Result to false.
    Even if we already know that Result is false, we stil want to
    merge Model1 and Model2 children as much as we can. }
  for I := 0 to Model1.VRML1ChildrenCount - 1 do
  begin
    if NodesMerge(Model1.VRML1Children[I], Model2.VRML1Children[I], EqualityEpsilon) then
    begin
      { Tests: Writeln('merged child ', I, ' of class ',
        Model1.VRML1Children[I].NodeTypeName); }
      Model1.VRML1Children[I] := Model2.VRML1Children[I];
    end else
      Result := false;
  end;

  if not Result then Exit;

  for I := 0 to Model1.FieldsCount - 1 do
  begin
    if Model1.Fields[I] is TSFNode then
    begin
      if not SFNodesMerge(TSFNode(Model1.Fields[I]),
                          TSFNode(Model2.Fields[I])) then
        Result := false;
    end else
    if Model1.Fields[I] is TMFNode then
    begin
      if not MFNodesMerge(TMFNode(Model1.Fields[I]),
                          TMFNode(Model2.Fields[I])) then
        Result := false;
    end else
    if Model1.Fields[I].CanAssignLerp then
    begin
      if not Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon) then
        Result := false;
    end;

    { Other fields were already checked by CheckNodesStructurallyEqual }
  end;
end;

{ Linear interpolation between Model1 and Model2.
  A = 0 means Model1, A = 1 means Model2, A between 0 and 1 is lerp
  between Model1 and Model2.

  If Model1 and Model2 are the same object (the same references),
  then this will return just Model1. This way it keeps memory optimization
  described by NodesMerge. This is also true if both Model1 and Model2
  are nil: then you can safely call this and it will return also nil. }
function NodesLerp(const A: Single; Model1, Model2: TX3DNode): TX3DNode;

  procedure SFNodeLerp(Target, Field1, Field2: TSFNode);
  begin
    Target.Value := NodesLerp(A, Field1.Value, Field2.Value);
  end;

  procedure MFNodeLerp(Target, Field1, Field2: TMFNode);
  var
    I: Integer;
  begin
    for I := 0 to Field1.Items.Count - 1 do
      Target.Add(NodesLerp(A, Field1[I], Field2[I]));
  end;

var
  I: Integer;
begin
  if Model1 = Model2 then
    Exit(Model1);

  Result := TX3DNodeClass(Model1.ClassType).Create(Model1.NodeName,
    Model1.BaseUrl);
  try
    { We already loaded all inlines (in CheckNodesStructurallyEqual).
      We have to mark it now, by setting Loaded := true field as necessary
      inside inline nodes --- otherwise, they could be loaded again
      (adding content to already existing nodes, making content loaded
      more than once). }
    if Result is TInlineNode then
    begin
      TInlineNode(Result).LoadedInlineDirectly;
    end;

    if Result is TX3DRootNode then
    begin
      { copy TX3DRootNode special fields, like TX3DRootNode.DeepCopyCore.
        This is necessary for WrapRootNode working Ok lower in this file. }
      TX3DRootNode(Result).HasForceVersion := (Model1 as TX3DRootNode).HasForceVersion;
      TX3DRootNode(Result).ForceVersion := (Model1 as TX3DRootNode).ForceVersion;
      TX3DRootNode(Result).Scale := (Model1 as TX3DRootNode).Scale;
      TX3DRootNode(Result).Profile := (Model1 as TX3DRootNode).Profile;
      TX3DRootNode(Result).Components.Assign((Model1 as TX3DRootNode).Components);
      TX3DRootNode(Result).Meta.Assign((Model1 as TX3DRootNode).Meta);
    end;

    { TODO: the code below doesn't deal efficiently with the situation when single
      TX3DNode is used as a child many times in one of the nodes.
      (through "USE" keyword). Code below will then unnecessarily
      create copies of such things (wasting construction time and memory),
      instead of reusing the same object reference. }
    for I := 0 to Model1.VRML1ChildrenCount - 1 do
      Result.VRML1ChildAdd(NodesLerp(A, Model1.VRML1Children[I], Model2.VRML1Children[I]));

    { TODO: for TX3DUnknownNode, we should fill here Result.Fields.
      Also for TX3DPrototypeNode. }

    for I := 0 to Model1.FieldsCount - 1 do
    begin
      if Model1.Fields[I] is TSFNode then
      begin
        SFNodeLerp(
          (Result.Fields[I] as TSFNode),
          (Model1.Fields[I] as TSFNode),
          (Model2.Fields[I] as TSFNode));
      end else
      if Model1.Fields[I] is TMFNode then
      begin
        MFNodeLerp(
          (Result.Fields[I] as TMFNode),
          (Model1.Fields[I] as TMFNode),
          (Model2.Fields[I] as TMFNode));
      end else
      if Model1.Fields[I].CanAssignLerp then
      begin
        Result.Fields[I].AssignLerp(A, Model1.Fields[I], Model2.Fields[I]);
      end else
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

{ TNodeInterpolator ---------------------------------------------------------- }

class function TNodeInterpolator.BakeToSequence(
  const GetKeyNodeWithTime: TGetKeyNodeWithTime;
  const KeyNodesCount: Cardinal;
  const ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  out TimeBegin, TimeEnd: Single): TX3DNodeList;
var
  I: Integer;
  StructurallyEqual, KeyNodesEqual: boolean;
  LastResultIndex: Integer;
  LastKeyNode, NewKeyNode: TX3DRootNode;
  LastTime, NewTime: Single;
  ResultIndex: Integer;
begin
  Result := TX3DNodeList.Create(false);
  try
    Assert(KeyNodesCount > 0);

    { KeyNodes[0] goes to Result[0], that's easy }
    GetKeyNodeWithTime(0, NewKeyNode, NewTime);

    Result.Count := 1;
    Result[0] := NewKeyNode;
    LastResultIndex := 0;
    LastTime := NewTime;
    LastKeyNode := NewKeyNode;

    { calculate TimeBegin at this point }
    TimeBegin := NewTime;

    for I := 1 to KeyNodesCount - 1 do
    begin
      { Now add KeyNodes[I] }
      GetKeyNodeWithTime(I, NewKeyNode, NewTime);

      StructurallyEqual := false;

      try
        CheckNodesStructurallyEqual(LastKeyNode, NewKeyNode, EqualityEpsilon);
        StructurallyEqual := true;
      except
        on E: EModelsStructureDifferent do
        begin
          if Log then
            WritelnLog('PrecalculatedAnimation', Format(
              'Nodes %d and %d structurally different, so animation will not be smoothed between them: ',
              [I - 1, I]) + E.Message);
        end;
      end;

      Result.Count := Result.Count +
        Max(1, Round((NewTime - LastTime) * ScenesPerTime));

      if StructurallyEqual then
      begin
        { Try to merge it with LastKeyNode.
          Then initialize Result[LastResultIndex + 1 to Result.Count - 1]. }
        KeyNodesEqual := NodesMerge(NewKeyNode, LastKeyNode, EqualityEpsilon);
        if KeyNodesEqual then
        begin
          { In this case don't waste memory, simply reuse
            LastKeyNode. }
          FreeAndNil(NewKeyNode);
          for ResultIndex := LastResultIndex + 1 to Result.Count - 1 do
            Result[ResultIndex] := Result[LastResultIndex];
        end else
        begin
          for ResultIndex := LastResultIndex + 1 to Result.Count - 2 do
            Result[ResultIndex] := NodesLerp(
              MapRange(ResultIndex, LastResultIndex, Result.Count - 1, 0.0, 1.0),
              LastKeyNode, NewKeyNode);
          Result[Result.Count - 1] := NewKeyNode;
          LastKeyNode := NewKeyNode;
        end;
      end else
      begin
        { We cannot interpolate between last and new node.
          So just duplicate last node until Result.Count - 2,
          and at Result.Last insert new node. }
        for ResultIndex := LastResultIndex + 1 to Result.Count - 2 do
          Result[ResultIndex] := Result[LastResultIndex];
        Result[Result.Count - 1] := NewKeyNode;
        LastKeyNode := NewKeyNode;
      end;

      LastTime := NewTime;
      LastResultIndex := Result.Count - 1;
    end;

    { calculate TimeEnd at this point }
    TimeEnd := NewTime;

  except FreeAndNil(Result); raise end;
end;

class procedure TNodeInterpolator.LoadAnimFramesToKeyNodes(const URL: string;
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);

  { Load castle-anim-frames animation data from a given XML element to a set of variables.

    This is just like LoadAnimFramesToKeyNodes, but it works using
    an Element. This way you can use it to load <animation> element
    that is a part of some larger XML file.

    @param(BaseUrl The URL from which relative
      URLs inside Element will be resolved. It doesn't
      have to be absolute, we will expand it to make it absolute
      if necessary.) }
  procedure LoadFromDOMElement(
    Element: TDOMElement;
    const BaseUrl: string);
  const
    DefaultLoop = false;
    DefaultBackwards = false;
  var
    AbsoluteBaseUrl: string;
    FrameElement: TDOMElement;
    Children: TXMLElementIterator;
    I: Integer;
    FrameTime: Single;
    FrameURL: string;
    NewNode: TX3DRootNode;
    Attr: TDOMAttr;
  begin
    Assert(KeyTimes.Count = 0);
    Assert(KeyNodes.Count = 0);

    AbsoluteBaseUrl := AbsoluteURI(BaseUrl);

    Check(Element.TagName = 'animation',
      'Root node of an animation XML file must be <animation>');

    { Assign default values for optional attributes }
    ScenesPerTime := DefaultScenesPerTime;
    EqualityEpsilon := DefaultEqualityEpsilon;
    ATimeLoop := DefaultLoop;
    ATimeBackwards := DefaultBackwards;

    for I := 0 to Integer(Element.Attributes.Length) - 1 do
    begin
      Attr := Element.Attributes.Item[I] as TDOMAttr;
      if Attr.Name = 'scenes_per_time' then
        ScenesPerTime := StrToInt(Attr.Value) else
      if Attr.Name = 'optimization' then
        { ignore } else
      if Attr.Name = 'equality_epsilon' then
        EqualityEpsilon := StrToFloat(Attr.Value) else
      if Attr.Name = 'loop' then
        ATimeLoop := StrToBool(Attr.Value) else
      if Attr.Name = 'backwards' then
        ATimeBackwards := StrToBool(Attr.Value) else
        raise Exception.CreateFmt('Unknown attribute of <animation> element: "%s"',
          [Attr.Name]);
    end;

    try
      Children := Element.ChildrenIterator;
      try
        while Children.GetNext do
        begin
          FrameElement := Children.Current;
          Check(FrameElement.TagName = 'frame',
            'Each child of <animation> element must be a <frame> element');

          if not FrameElement.AttributeSingle('time', FrameTime) then
            raise Exception.Create('<frame> element must have a "time" attribute');
          if (KeyTimes.Count > 0) and (FrameTime <= KeyTimes.Last) then
            raise Exception.Create(
              'Frames within <animation> element must be specified in ' +
              'increasing time order');
          KeyTimes.Add(FrameTime);

          if FrameElement.AttributeString('url', FrameURL) or
             FrameElement.AttributeString('file_name', FrameURL) then
          begin
            { Make FrameURL absolute, treating it as relative vs
              AbsoluteBaseUrl }
            FrameURL := CombineURI(AbsoluteBaseUrl, FrameURL);
            NewNode := Load3D(FrameURL);
          end else
          begin
            NewNode := LoadX3DXml(FrameElement.ChildElement('X3D'), AbsoluteBaseUrl);
          end;
          KeyNodes.Add(NewNode);
        end;
      finally FreeAndNil(Children) end;
    except
      { in case of trouble, clear the partial KeyNodes contents }
      for I := 0 to KeyNodes.Count - 1 do
        FPGObjectList_FreeAndNilItem(KeyNodes, I);
      KeyNodes.Clear;
      raise;
    end;

    if KeyNodes.Count = 0 then
      raise Exception.Create(
        'At least one <frame> is required within <animation> element');
  end;

var
  Document: TXMLDocument;
  Stream: TStream;
begin
  Stream := Download(URL);
  try
    ReadXMLFile(Document, Stream);
  finally FreeAndNil(Stream) end;

  try
    LoadFromDOMElement(Document.DocumentElement, URL);
  finally FreeAndNil(Document); end;
end;

class function TNodeInterpolator.LoadSequenceToX3D(
  const Nodes: TX3DNodeList; const Duration: Single;
  const Loop, Backwards: boolean): TX3DRootNode;
var
  BaseUrl: string;

  { For VRML 1.0, wrap the contents in SeparateGroup. Prevents leaking
    transformations between switch node children (testcase:
    castle-game/data/creatures/alien/walk.kanim). Possibly it could be done
    differently at higher level (because this is a switch, so it should
    block leaking anyway, but probably Switch for X3D doesn't work for VRML 1.0
    so well...). But it's obsolete VRML 1.0, so the hack is acceptable:) }
  function WrapRootNode(const RootNode: TX3DRootNode): TX3DNode;
  begin
    if RootNode.HasForceVersion and (RootNode.ForceVersion.Major <= 1) then
    begin
      Result := TSeparatorNode_1.Create('', BaseUrl);
      Result.VRML1ChildAdd(RootNode);
    end else
      Result := RootNode;
  end;

  function WrapInCollisionNode(const Node: TX3DNode): TX3DNode;
  var
    CollisionNode: TCollisionNode;
  begin
    CollisionNode := TCollisionNode.Create;
    CollisionNode.FdChildren.Add(Node);
    CollisionNode.Enabled := false;
    Result := CollisionNode;
  end;

var
  TimeSensor: TTimeSensorNode;
  IntSequencer: TIntegerSequencerNode;
  Switch: TSwitchNode;
  I: Integer;
  Route: TX3DRoute;
  ChildNode: TX3DNode;
begin
  BaseUrl := Nodes[0].BaseUrl;

  Result := TX3DRootNode.Create('', BaseUrl);

  TimeSensor := TTimeSensorNode.Create(DefaultAnimationName, BaseUrl);
  TimeSensor.CycleInterval := Duration;
  TimeSensor.Loop := Loop;
  Result.FdChildren.Add(TimeSensor);

  IntSequencer := TIntegerSequencerNode.Create(
    DefaultAnimationName + '_IntegerSequencer', BaseUrl);
  if Backwards then
  begin
    IntSequencer.FdKey.Count := Nodes.Count * 2;
    IntSequencer.FdKeyValue.Count := Nodes.Count * 2;
    for I := 0 to Nodes.Count - 1 do
    begin
      IntSequencer.FdKey.Items[I] := I / IntSequencer.FdKey.Count;
      IntSequencer.FdKeyValue.Items[I] := I;
    end;
    for I := 0 to Nodes.Count - 1 do
    begin
      IntSequencer.FdKey.Items[Nodes.Count + I] := (Nodes.Count + I) / IntSequencer.FdKey.Count;
      IntSequencer.FdKeyValue.Items[Nodes.Count + I] := Nodes.Count - 1 - I;
    end;
  end else
  begin
    IntSequencer.FdKey.Count := Nodes.Count;
    IntSequencer.FdKeyValue.Count := Nodes.Count;
    for I := 0 to Nodes.Count - 1 do
    begin
      IntSequencer.FdKey.Items[I] := I / Nodes.Count;
      IntSequencer.FdKeyValue.Items[I] := I;
    end;
  end;
  Result.FdChildren.Add(IntSequencer);

  Switch := TSwitchNode.Create(DefaultAnimationName + '_Switch', BaseUrl);
  for I := 0 to Nodes.Count - 1 do
  begin
    ChildNode := WrapRootNode(Nodes[I] as TX3DRootNode);
    { TODO: Initializing collisions for a long series of nodes is really
      time-consuming. Better to avoid it. We have to implement actual conversion
      from a series of nodes -> interpolators to have proper collisions
      with castle-anim-frames contents. }
    if I <> 0 then
      ChildNode := WrapInCollisionNode(ChildNode);
    Switch.FdChildren.Add(ChildNode);
  end;
  { we set whichChoice to 0 to have sensible,
    non-empty bounding box before you run the animation }
  Switch.WhichChoice := 0;
  Result.FdChildren.Add(Switch);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(TimeSensor.EventFraction_changed);
  Route.SetDestinationDirectly(IntSequencer.EventSet_fraction);
  Result.AddRoute(Route);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(IntSequencer.EventValue_changed);
  Route.SetDestinationDirectly(Switch.FdWhichChoice);
  Result.AddRoute(Route);

  Result.ManuallyExportNode(TimeSensor);
end;

class procedure TNodeInterpolator.LoadToX3D_GetKeyNodeWithTime(const Index: Cardinal;
  out KeyNode: TX3DRootNode; out Time: Single);
begin
  KeyNode := LoadToX3D_KeyNodes[Index] as TX3DRootNode;
  Time := LoadToX3D_KeyTimes[Index];
end;

class function TNodeInterpolator.LoadToX3D(
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  const ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  const Loop, Backwards: boolean): TX3DRootNode;
var
  TimeBegin, TimeEnd: Single;
  Nodes: TX3DNodeList;
begin
  LoadToX3D_KeyNodes := KeyNodes;
  LoadToX3D_KeyTimes := KeyTimes;
  Nodes := BakeToSequence(@LoadToX3D_GetKeyNodeWithTime, KeyNodes.Count, ScenesPerTime, EqualityEpsilon, TimeBegin, TimeEnd);
  try
    Result := LoadSequenceToX3D(Nodes, TimeEnd - TimeBegin, Loop, Backwards);
  finally FreeAndNil(Nodes) end;
end;

end.
