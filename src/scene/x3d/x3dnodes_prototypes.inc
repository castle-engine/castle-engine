{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Raised when there's a problem instantiating X3D prototype,
    see @link(TX3DPrototypeNode.Instantiate).

    User code usually doesn't see this exception, unless you call
    @link(TX3DPrototypeNode.Instantiate) explicitly (but you usually don't).
    In normal circumstances, the engine calls @link(TX3DPrototypeNode.Instantiate)
    when necessary, and converts eventual EX3DPrototypeInstantiateError
    exceptions into warnings (reported by
    @link(TCastleApplicationProperties.OnWarning ApplicationProperties.OnWarning)). }
  EX3DPrototypeInstantiateError = class(EX3DError);

  { Node with information about X3D prototype.

    This node will have fields
    initialized according to associated Prototype.InterfaceDeclarations.
    This way you can simply parse this node (just like any other node)
    to parse prototype instance.

    The prototype may be instantiated. After parsing you can
    do it by @link(Instantiate) method. In case of non-external prototype,
    this should always be possible (for a valid X3D files, that is),
    in case of external prototype this may requite loading the external
    prototype file.

    This node cannot be created by standard Create method,
    always use CreatePrototypeNode. }
  TX3DPrototypeNode = class(TX3DNode)
  strict private
    FPrototype: TX3DPrototypeBase;

    function PrepareInstantiateIsClause(Node, Child: TX3DNode): Pointer;

    (*This searches Node for fields/events with "IS" clauses, and handles them:
      for fields, this means copying field value from Self to Child
      (and setting ValueFromIsClause).
      for events, this means adding internal routes to route event
      from/to Self to/from Child.

      Handled IS clauses are removed (to not be seen by next calls
      of InstantiateIsClauses on the same node, or on it's copy,
      see below).

      It also descends recursively into all children nodes.
      Note that it descends into all
      nodes, even the ones that came from another prototype
      (PrototypeInstance is @true). It doesn't try to omit them,
      as they may have "IS" clauses that refer to our fields.
      Consider part of key_sensor.x3dv test:

      @preformatted(
        PROTO SimpleText [
          inputOutput MFString onestring ""
        ] { Shape { geometry Text { string IS onestring } } }

        PROTO PressedText [
          inputOutput MFString againstring ""
        ] { SimpleText { onestring IS againstring } }

        PressedText { againstring "zero" }
      )

      After expanding SimpleText within PressedText, we have
      @code(Shape { geometry Text { string IS againstring } }),
      that is we resolved "IS onestring" to yet another IS clause:
      "IS againstring". Which means that when expanding PressedText,
      we have to process everything again, to eventually fill "againstring"
      value. *)
    function InstantiateIsClauses(Node, Child: TX3DNode): Pointer;

    { Handle "IS" clause on Destination field/event, by copying it's value
      from Source.

      For fields basically does Destination.AssignValue(Source).
      In case of EX3DFieldAssign, make WritelnWarning with clear message.

      For events establishes internal route.

      Assumes that all Source "IS" clauses are not expanded yet.
      In fact, Source field/event always comes from this node,
      that is TX3DPrototypeNode. So we just expand prototype within
      TX3DPrototypeNode. If this TX3DPrototypeNode is itself within
      another prototype, there's no way Source.IsClauseNames could be expanded
      already.

      Above paragraph means that when Source.IsClauseNames exist,
      we know what to do. If this is about fields, then we have to copy
      field's value (in case in Source field value was specified explicitly)
      and also assign Source.IsClauseNames as new Destination.IsClauseNames
      (eventually, higher IS clauses will override field's value).
      For events, we also copy IsClauseNames, in addition to creating
      internal route (is internal route still necessary? for exposedFields?
      seems so, I'm not sure...). }
    procedure FieldOrEventHandleIsClause(
      Destination, Source: TX3DFieldOrEvent;
      NewIsClauseNames: TCastleStringList);
  protected
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
  public
    { This constructor will raise exception for TX3DPrototypeNode.
      Always use CreatePrototypeNode for this node class. }
    constructor Create(const AName, ABaseUrl: string); override;
    constructor CreatePrototypeNode(const AName, ABaseUrl: string;
      APrototype: TX3DPrototypeBase);
    function X3DType: string; override;

    property Prototype: TX3DPrototypeBase read FPrototype;

    { Instantiate the prototype, that is create new X3D node
      (of "normal" classs, not TX3DPrototypeNode) using prototype description.

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
      TMaterialNode. Just like Material node would be normally specified,
      not created by some prototype.

      Note that this TX3DPrototypeNode becomes "owned" by returned
      node instance, in PrototypeInstanceSourceNode.
      (that's needed for returned node's SaveToStream to work correctly).

      Details:
      @unorderedList(
        @item(
          Prototype.Node may be just a wrapper, i.e. TX3DRootNode.

          In this case the first children of Prototype.Node is used
          to create instance. The rest of the wrapper (with this first children
          removed, to not cause cycles) is also duplicated and set
          as new node's PrototypeInstanceHelpers.)

        @item(
          Returned Node (with all it's helpers in PrototypeInstanceHelpers)
          has "IS" clauses everywhere filled, according to our field values.)

        @item(Name of returned node is copied from our Name.)

        @item(
          For SaveToStream to work, returned Node has PrototypeInstance = @true,
          and PrototypeInstanceSourceNode set to Self. This allows SaveToStream
          to correctly save using PrototypeInstanceSourceNode, instead
          of writing actual node contents.))

      @raises(EX3DPrototypeInstantiateError If for some reason
        the prototype cannot be instantiated.
        Outside code should catch this and replace with
        ApplicationProperties.OnWarning, if possible.)
    }
    function Instantiate: TX3DNode;
  end;

  { Common class to represent X3D external prototypes (referencing another file)
    or non-external prototypes (describing how to expand the prototype into other nodes). }
  TX3DPrototypeBase = class(TX3DFileItem)
  private
    FX3DName: string;
    FInterfaceDeclarations: TX3DInterfaceDeclarationList;

    FBaseUrl: string;

    { Parses InterfaceDeclarations. Also inits BaseUrl from
      Names.BaseUrl, by the way. }
    procedure ParseInterfaceDeclarations(ExternalProto: boolean;
      Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse interface declarations in XML encoding.
      Handle sequence of <field> elements.

      Note: unlike classic ParseInterfaceDeclarations,
      this doesn't set BaseUrl, do it yourself (because often
      you do not have 'ProtoInterface', so you would have to do it yourself
      anyway). }
    procedure ParseInterfaceDeclarationsXML(ExternalProto: boolean;
      Element: TDOMElement; Reader: TX3DReaderNames);

    { Saves interface declarations of the prototype.
      For classic encoding, they are already enclosed in [ ]. }
    procedure SaveInterfaceDeclarationsToStream(
      Writer: TX3DWriter; ExternalProto: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property X3DName: string read FX3DName write FX3DName;
    {$ifdef FPC}
    property Name: string read FX3DName write FX3DName; deprecated 'use X3DName';
    {$endif}
    property InterfaceDeclarations: TX3DInterfaceDeclarationList
      read FInterfaceDeclarations;

    { Parse prototype, and add it to Names.Prototypes.
      Adds to @code(Names) by @code(Names.Prototypes.Bind(Self)).
      @groupBegin }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); virtual; abstract;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); virtual; abstract;
    { @groupEnd }

    { The base URL path used to resolve urls inside.
      For now, used by EXTERNPROTO urls.
      See TX3DNode.BaseUrl for more comments. }
    property BaseUrl: string read FBaseUrl write FBaseUrl;
  end;

  TX3DPrototypeBaseList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DPrototypeBase>);

  { X3D non-external prototype (describing how to expand the prototype into other nodes). }
  TX3DPrototype = class(TX3DPrototypeBase)
  strict private
    FNode: TX3DRootNode;
  public
    destructor Destroy; override;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); override;
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Prototype contents: all nodes, prototypes, routes defined inside. }
    property Node: TX3DRootNode read FNode;
  end;

  { X3D external prototype (referencing another file). }
  TX3DExternalPrototype = class(TX3DPrototypeBase)
  strict private
    FURLList: TMFString;

    { FReferencedPrototype has links to other parts of the VRML graph.
      Not only FReferencedPrototype.Node, but also
      FReferencedPrototype.InterfaceDeclaration may have links to it:
      if referenced node has SFNode or MFNode fields and their default
      values have "USE ..." clauses.
      So it's best to keep whole ReferencedPrototypeNode (whole VRML file
      that contained this prototype) loaded. }
    ReferencedPrototypeNode: TX3DRootNode;

    FReferencedPrototype: TX3DPrototype;

    FReferencedClass: TX3DNodeClass;
  public
    constructor Create;
    destructor Destroy; override;
    property URLList: TMFString read FURLList;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames); override;
    procedure SaveToStream(Writer: TX3DWriter); override;

    property ReferencedPrototype: TX3DPrototype read FReferencedPrototype;
    property ReferencedClass: TX3DNodeClass read FReferencedClass;

    { Loads URL, until the first success. Sets either ReferencedClass to non-nil
      (if it's built-in node) or ReferencedPrototype (if prototype expansion
      found in external file). }
    procedure LoadReferenced(const Version: TX3DVersion);
    procedure UnloadReferenced;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DPrototypeNode --------------------------------------------------------- }

constructor TX3DPrototypeNode.Create(const AName, ABaseUrl: string);
begin
  raise EInternalError.Create('TX3DPrototypeNode node must be created' +
    ' using CreatePrototypeNode, never default constructor');
end;

constructor TX3DPrototypeNode.CreatePrototypeNode(
  const AName, ABaseUrl: string;
  APrototype: TX3DPrototypeBase);
var
  I: TX3DInterfaceDeclaration;
  Index: Integer;
  ProtoInitial: TX3DPrototypeBase;
begin
  inherited Create(AName, ABaseUrl);
  FPrototype := APrototype;

  ProtoInitial := Prototype;

  if (ProtoInitial is TX3DExternalPrototype) and
     (TX3DExternalPrototype(ProtoInitial).ReferencedPrototype <> nil) then
    { It's important to use ReferencedPrototype, not just current
      Prototype, in this case. That's because field's default values
      should be set by constructor (before parsing the specified
      fields), and TX3DExternalPrototype doesn't have default values
      available.

      If ReferencedPrototype = nil (e.g. because couldn't
      be loaded) then Instantiate will not be able to instantiate
      it anyway (and will produce appropriate WritelnWarning). }
    ProtoInitial := TX3DExternalPrototype(ProtoInitial).ReferencedPrototype;

  for Index := 0 to ProtoInitial.InterfaceDeclarations.Count - 1 do
  begin
    I := ProtoInitial.InterfaceDeclarations.Items[Index];
    I.CopyAndAddFieldOrEvent(Self);
  end;
end;

function TX3DPrototypeNode.DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DPrototypeNode.CreatePrototypeNode(X3DName, BaseUrl,
    { TODO: for now, we don't copy proto, instead simply passing the same
      proto reference. }
    Prototype);
end;

function TX3DPrototypeNode.X3DType: string;
begin
  Result := Prototype.X3DName;
end;

procedure TX3DPrototypeNode.FieldOrEventHandleIsClause(
  Destination, Source: TX3DFieldOrEvent;
  NewIsClauseNames: TCastleStringList);
var
  DestinationField, SourceField: TX3DField;
  DestinationEvent, SourceEvent: TX3DEvent;
  Route: TX3DRoute;
  I: Integer;
const
  InEventName: array [boolean] of string = ( 'output', 'input' );
begin
  { When Source.IsClauseNamesCount <> 0, then we're expanded
    within the definition of another prototype.
    So Destination.IsClauseNames referers to Source
    (from current Prototype), but Source.IsClauseNames refers to yet
    another, enclosing, prototype (that will be expanded later --- for
    now we're within enclosing prototype definition).

    See comments in the interface for more. }

  { Intuitively: NewIsClauseNames.AddStrings(Source.IsClauseNames); }
  for I := 0 to Source.IsClauseNamesCount - 1 do
    NewIsClauseNames.Add(Source.IsClauseNames[I]);

  if Source is TX3DField then
  begin
    Assert(Destination is TX3DField);
    SourceField := Source as TX3DField;
    DestinationField := Destination as TX3DField;

    try
      DestinationField.AssignValue(SourceField);
      DestinationField.ValueFromIsClause := true;
    except
      on E: EX3DFieldAssignInvalidClass do
      begin
        WritelnWarning('VRML/X3D', Format('Within prototype "%s", ' +
          'field of type %s (named "%s") references ' +
          '(by "IS" clause) field of different type %s (named "%s")',
          [Prototype.X3DName,
           DestinationField.X3DType,
           Destination.X3DName,
           SourceField.X3DType,
           Source.X3DName]));
      end;
      on E: EX3DFieldAssign do
      begin
        WritelnWarning('VRML/X3D', Format('Error when expanding prototype "%s": ',
          [Prototype.X3DName]) + E.Message);
      end;
    end;
  end else
  if Source is TX3DEvent then
  begin
    Assert(Destination is TX3DEvent);
    SourceEvent := Source as TX3DEvent;
    DestinationEvent := Destination as TX3DEvent;

    if SourceEvent.InEvent <> DestinationEvent.InEvent then
    begin
      WritelnWarning('VRML/X3D', Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.X3DName,
          InEventName[DestinationEvent.InEvent],
          InEventName[SourceEvent.InEvent] ]));
      Exit;
    end;

    if SourceEvent.FieldClass <> DestinationEvent.FieldClass then
    begin
      WritelnWarning('VRML/X3D', Format('When expanding prototype "%s": "%s" event references (by "IS" clause) "%s" event',
        [ Prototype.X3DName,
          DestinationEvent.FieldClass.X3DType,
          SourceEvent.FieldClass.X3DType ]));
      Exit;
    end;

    Route := TX3DRoute.Create;
    Route.Internal := true;

    try
      if SourceEvent.InEvent then
      begin
        Route.SetSourceDirectly(SourceEvent);
        Route.SetDestinationDirectly(DestinationEvent);
      end else
      begin
        Route.SetSourceDirectly(DestinationEvent);
        Route.SetDestinationDirectly(SourceEvent);
      end;

      AddRoute(Route);
    except
      FreeAndNil(Route);
      raise;
    end;
  end;
end;

function TX3DPrototypeNode.InstantiateIsClauses(Node, Child: TX3DNode): Pointer;

  { In terminology of VRML/X3D specs,
    InstanceField/Event is the one in "prototype definition"
    (that is, inside prototype content) and
    OutField/Event is the one in "prototype declaration".

    This is where we implement table "Rules for mapping PROTOTYPE
    declarations to node instances" in specifications about
    "PROTO definition semantics". }

  procedure ExpandEvent(InstanceEvent: TX3DEvent); forward;

  procedure ExpandField(InstanceField: TX3DField);
  var
    OurField: TX3DField;
    OurEvent: TX3DEvent;
    OurFieldIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TCastleStringList;
  begin
    if InstanceField.IsClauseNamesCount <> 0 then
    begin
      NewIsClauseNames := TCastleStringList.Create;
      try
        for I := 0 to InstanceField.IsClauseNamesCount - 1 do
        begin
          IsClauseName := InstanceField.IsClauseNames[I];
          OurFieldIndex := IndexOfField(IsClauseName);
          if OurFieldIndex <> -1 then
          begin
            OurField := Fields[OurFieldIndex];
            FieldOrEventHandleIsClause(InstanceField, OurField, NewIsClauseNames);

            if InstanceField.Exposed and OurField.Exposed then
            begin
              { We have to "break" exposed fields inside TX3DPrototypeNode,
                that is they will not automatically get changes and forwarding
                in event to out event. Reason:

                - This causes cycles in routes (remember that
                  FieldOrEventHandleIsClause creates internal routes
                  for "IS" between events.) When something sets event,
                  by sending to InstanceField.EventIn, then this will
                  be forwarded to InstanceField.EventOut. But also
                  will be forwarded to OurField.EventIn (this must be done,
                  in case other "IS" clauses interact with these exposed
                  events). Without breaking ExposedEventsLinked,
                  OurField.EventIn would forward this once again to
                  InstanceField.EventOut, causing all routes from
                  InstanceField.EventOut to the outside be detected as loops,
                  as another event travels through them.

                - Lastly, this is not needed. Copying field's value
                  is useless, as field's value in TX3DPrototypeNode
                  is useless (field value must get to actual field in expanded
                  hierarchy).
              }
              OurField.ExposedEventsLinked := false;

              { Note that I pass here NewIsClauseNames, that is
                is our in/out event will have specialized IS clauses,
                they will be assigned to whole exposed field.
                This is Ok, as event may only point to another event
                higher in proto nesting. And exposed field may also refer
                to another event (inputOnly or outputOnly) higher in
                proto nesting. }

              FieldOrEventHandleIsClause(InstanceField.EventIn , OurField.EventIn , NewIsClauseNames);
              FieldOrEventHandleIsClause(InstanceField.EventOut, OurField.EventOut, NewIsClauseNames);
            end;
          end else
          if InstanceField.Exposed then
          begin
            { exposed field may also reference by IS clause the simple
              "only input" or "only output" event. }
            { TODO: untested case }
            OurEvent := AnyEvent(IsClauseName);
            if OurEvent <> nil then
            begin
              if OurEvent.InEvent then
                FieldOrEventHandleIsClause(InstanceField.EventIn , OurEvent, NewIsClauseNames) else
                FieldOrEventHandleIsClause(InstanceField.EventOut, OurEvent, NewIsClauseNames);
            end else
              WritelnWarning('VRML/X3D', Format('Within prototype "%s", exposed field "%s" references (by "IS" clause) non-existing field/event name "%s"',
                [Prototype.X3DName, InstanceField.X3DName, IsClauseName]));
          end else
            WritelnWarning('VRML/X3D', Format('Within prototype "%s", field "%s" references (by "IS" clause) non-existing field "%s"',
              [Prototype.X3DName, InstanceField.X3DName, IsClauseName]));
        end;

        InstanceField.IsClauseNamesAssign(NewIsClauseNames);
      finally FreeAndNil(NewIsClauseNames) end;
    end;

    if InstanceField.Exposed then
    begin
      (*Completely independent from the fact whether InstanceField.IsClause,
        it's exposed events may also have their own IS clauses, for example

          DirectionalLight {
            on IS light_on_initial_value
            set_on IS light_on_setter
            on_changed IS light_on_getter
          }
      *)

      ExpandEvent(InstanceField.EventIn);
      ExpandEvent(InstanceField.EventOut);
    end;
  end;

  procedure ExpandEvent(InstanceEvent: TX3DEvent);
  var
    OurEvent: TX3DEvent;
    OurEventIndex: Integer;
    I: Integer;
    IsClauseName: string;
    NewIsClauseNames: TCastleStringList;
  begin
    if InstanceEvent.IsClauseNamesCount <> 0 then
    begin
      NewIsClauseNames := TCastleStringList.Create;
      try
        for I := 0 to InstanceEvent.IsClauseNamesCount - 1 do
        begin
          IsClauseName := InstanceEvent.IsClauseNames[I];

          { Event from prototype definition can only correspond to the
            same event type of prototype declaration. It cannot reference
            implicit event (within exposed field) of prototype declaration.
            Which is good, since otherwise it would be difficult to implement
            (Self (TX3DPrototypeNode) is used to keep events, but it doesn't
            keep actual field values (there are kept within actual expanded nodes).

            This also means that searching below by Events.IndexOf is Ok,
            no need to use AnyEvent to search. }

          OurEventIndex := IndexOfEvent(IsClauseName);
          if OurEventIndex <> -1 then
          begin
            OurEvent := Events[OurEventIndex];
            FieldOrEventHandleIsClause(InstanceEvent, OurEvent, NewIsClauseNames);
          end else
            WritelnWarning('VRML/X3D', Format('Within prototype "%s", event "%s" references (by "IS" clause) non-existing event "%s"',
              [Prototype.X3DName, InstanceEvent.X3DName, IsClauseName]));
        end;

        InstanceEvent.IsClauseNamesAssign(NewIsClauseNames);
      finally FreeAndNil(NewIsClauseNames) end;
    end;
  end;

var
  I: Integer;
begin
  Result := nil;

  { The NeedsInstantiateIsClause flag is needed for InstantiateIsClauses.
     By checking this flag (and setting back to @false after handling):

     1. We ensure that every node is expanded only once (otherwise,
        in case of funny DEF/USE usage, we could process the same node twice
        by InstantiateIsClauses).

     2. We avoid instantiating "IS" clauses in nodes outside the NodeCopy.
        Such nodes with "IS" clauses may be added by InstantiateIsClauses,
        when our own PROTO has fields with SFNode / MFNode type,
        and they contain "IS" clauses that should be handled by other
        (outer) proto. See e.g. demo_models/vrml_2/proto_nested_expand.wrl
        testcase.

     (The 2nd point could also be fixed by simply moving recursive call to
     Child.DirectEnumerateAll(@InstantiateIsClauses) to the beginning,
     not the end, of InstantiateIsClauses. But this would leave 1st point
     unfixed.) }

  if not Child.NeedsInstantiateIsClause then Exit;
  Child.NeedsInstantiateIsClause := false;

  if RebaseRelativeUrlsInPrototypes then
    Child.BaseUrl := BaseUrl;

  for I := 0 to Child.FieldsCount - 1 do
    ExpandField(Child.Fields[I]);

  for I := 0 to Child.EventsCount - 1 do
    ExpandEvent(Child.Events[I]);

  Child.DirectEnumerateAll({$ifdef FPC}@{$endif} Self.InstantiateIsClauses);
end;

function TX3DPrototypeNode.PrepareInstantiateIsClause(
  Node, Child: TX3DNode): Pointer;
begin
  Result := nil;
  if not (Child.NeedsInstantiateIsClause or (Child is TInlineNode)) then
  begin
    Child.NeedsInstantiateIsClause := true;
    Child.DirectEnumerateAll({$ifdef FPC}@{$endif} Self.PrepareInstantiateIsClause);
  end;
end;

function TX3DPrototypeNode.Instantiate: TX3DNode;

  procedure InstantiateNonExternalPrototype(Proto: TX3DPrototype);
  var
    NodeCopy, NewPrototypeInstanceHelpers: TX3DRootNode;
  begin
    { We want to copy the whole Proto.Node, instead of copying separately
      Proto.Node.FdChildren[0], Proto.Node.FdChildren[1] etc.
      This way, DEF / USE links, routes links (internal, for nested
      protos "IS" clauses, and non-internal) are preserved as they should. }

    NodeCopy := Proto.Node.DeepCopy as TX3DRootNode;

    Assert(NodeCopy.PrototypeInstance =
      (NodeCopy.PrototypeInstanceSourceNode <> nil));
    Assert(NodeCopy.PrototypeInstance or
      (NodeCopy.PrototypeInstanceHelpers = nil));

    try
      { First, set NeedsInstantiateIsClause := true everywhere
        inside NodeCopy. (We can assume that NeedsInstantiateIsClause
        was @false everywhere before this.) }
      PrepareInstantiateIsClause(nil, NodeCopy);

      InstantiateIsClauses(nil, NodeCopy);
    except
      FreeAndNil(NodeCopy);
      raise;
    end;

    if NodeCopy.FdChildren.Count = 0 then
    begin
      { If exception occurs before NodeCopy is connected to Result,
        NodeCopy should be simply freed. }
      FreeAndNil(NodeCopy);
      raise EX3DPrototypeInstantiateError.CreateFmt(
        'Prototype "%s" has no nodes, cannot instantiate',
        [Proto.X3DName]);
    end;

    { ExtractChild/Item methods were really invented specially for this case.

      We have to remove Result from NodeCopy, to avoid cycles
      (that can cause mem leaks) because Result.FPrototypeInstanceHelpers
      has to keep pointer to NodeCopy.

      At the same time, Result must not be freed here because of ref count = 0... }
    Result := NodeCopy.FdChildren.Extract(0);

    Assert(Result.PrototypeInstance =
      (Result.PrototypeInstanceSourceNode <> nil));
    Assert(Result.PrototypeInstance or
      (Result.PrototypeInstanceHelpers = nil));

    { NewPrototypeInstanceHelpers is used to keep the rest of
      NodeCopy.FdChildren[1...] that should accompany this node. }
    NewPrototypeInstanceHelpers := NodeCopy;

    Result.X3DName := X3DName;

    (* Result and NodeCopy may come from another prototype.
       For example,

         PROTO SimpleText [
           inputOutput MFString string ""
         ] { Shape { geometry Text { string IS string } } }

         PROTO PressedText [
           inputOutput MFString string ""
         ] { SimpleText { string IS string } }

         PressedText { string "zero" }

       (this is a simplified part of ../../../demo_models/x3d/key_sensor.x3dv
       file). In such case, when PressedText, you may get Shape that already
       was expanded from SimpleText. So NodeCopy will have PrototypeInstance
       = true. Or NodeCopy may be TX3DRootNode wrapper, then it's
       first item (that is assigned to Result) will
       have PrototypeInstance = true.

       We have assertions above to check that their PrototypeInstance*
       properties are in valid state.

       Note that we take proper actions to not leak memory (so do
       not blindly overwrite Result.PrototypeInstanceSourceNode and ...Helpers).
       Also, existing Result.PrototypeInstanceSourceNode and ...Helpers
       must be retained, as they may contain routes for proper functioning
       of this nested prototype.

       What can we do? We have to keep them somewhere, but move out
       of the way. Current approach is to overuse
       TX3DPrototypeNode.PrototypeInstanceSourceNode and ...Helpers
       fiels for this. (They should be empty right now, as one
       TX3DPrototypeNode is expanded only once.)
       We store there information about nested prototype.
    *)
    if Result.PrototypeInstance then
    begin
      Assert(not PrototypeInstance);
      Assert(PrototypeInstanceSourceNode = nil);
      Assert(PrototypeInstanceHelpers = nil);

      FPrototypeInstance := Result.PrototypeInstance;
      FPrototypeInstanceSourceNode := Result.PrototypeInstanceSourceNode;
      FPrototypeInstanceHelpers := Result.PrototypeInstanceHelpers;
    end;

    { Note: set PrototypeInstance to @true *after* InstantiateIsClauses,
      otherwise InstantiateIsClauses would not enter this node.
      TODO: bad comment above? }
    Result.FPrototypeInstance := true;
    Result.FPrototypeInstanceSourceNode := Self;
    Result.FPrototypeInstanceHelpers := NewPrototypeInstanceHelpers;
  end;

  procedure InstantiateExternalPrototype(Proto: TX3DExternalPrototype);
  begin
    if Proto.ReferencedPrototype = nil then
      raise EX3DPrototypeInstantiateError.CreateFmt(
        'External prototype "%s" cannot be loaded, so cannot instantiate nodes using it',
        [Proto.X3DName]);

    { Note that we do not check whether ReferencedPrototype actually
      has the same fields/events as declared for externproto.
      Although when expanding IS clauses, missing declarations
      or incorrect types or field/event will be caught, so the necessary
      things will be checked when expanding. }

    InstantiateNonExternalPrototype(Proto.ReferencedPrototype);
  end;

begin
  if Prototype is TX3DPrototype then
    InstantiateNonExternalPrototype(Prototype as TX3DPrototype) else
  if Prototype is TX3DExternalPrototype then
    InstantiateExternalPrototype(Prototype as TX3DExternalPrototype) else
    raise EX3DPrototypeInstantiateError.CreateFmt(
      'Cannot instantiate prototype "%s": '+
      'unknown prototype class %s', [Prototype.X3DName, Prototype.ClassName]);
end;

{ TX3DPrototypeBase --------------------------------------------------------- }

constructor TX3DPrototypeBase.Create;
begin
  inherited;
  FInterfaceDeclarations := TX3DInterfaceDeclarationList.Create(true);
end;

destructor TX3DPrototypeBase.Destroy;
begin
  FreeAndNil(FInterfaceDeclarations);
  inherited;
end;

procedure TX3DPrototypeBase.ParseInterfaceDeclarations(ExternalProto: boolean;
  Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  I: TX3DInterfaceDeclaration;
begin
  while Lexer.Token <> vtCloseSqBracket do
  begin
    I := TX3DInterfaceDeclaration.Create(nil);
    InterfaceDeclarations.Add(I);

    if Lexer.TokenIsKeyword(InterfaceDeclarationKeywords(AllAccessTypes)) then
    begin
      I.Parse(Lexer, Reader, not ExternalProto, false);
    end else
      raise EX3DParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
  end;

  { eat "]" token }
  Lexer.NextToken;

  FBaseUrl := Reader.BaseUrl;
end;

procedure TX3DPrototypeBase.ParseInterfaceDeclarationsXML(ExternalProto: boolean;
  Element: TDOMElement; Reader: TX3DReaderNames);
var
  I: TX3DInterfaceDeclaration;
  Iter: TXMLElementIterator;
begin
  Iter := Element.ChildrenIterator;
  try
    while Iter.GetNext do
    begin
      if Iter.Current.TagName = 'field' then
      begin
        I := TX3DInterfaceDeclaration.Create(nil);
        InterfaceDeclarations.Add(I);
        I.ParseXML(Iter.Current, Reader, not ExternalProto);
      end else
        WritelnWarning('VRML/X3D', 'X3D XML: only <field> elements expected in prototype interface');
    end;
  finally FreeAndNil(Iter) end;
end;

procedure TX3DPrototypeBase.SaveInterfaceDeclarationsToStream(
  Writer: TX3DWriter; ExternalProto: boolean);
var
  I: Integer;
begin
  if Writer.Encoding = xeClassic then Writer.Writeln('[');
  Writer.IncIndent;
  for I := 0 to InterfaceDeclarations.Count - 1 do
    InterfaceDeclarations.Items[I].IDeclSaveToStream(Writer, not ExternalProto);
  Writer.DecIndent;
  if Writer.Encoding = xeClassic then Writer.WritelnIndent(']');
end;

{ TX3DPrototype ------------------------------------------------------------- }

destructor TX3DPrototype.Destroy;
begin
  FreeAndNil(FNode);
  inherited;
end;

procedure TX3DPrototype.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  OldReader: TX3DReaderNames;
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FX3DName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(false, Lexer, Reader);

  Lexer.CheckTokenIs(vtOpenCurlyBracket);

  Lexer.NextToken;
  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Reader.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldReader := Reader;
  Reader := TX3DReaderNames.CreateCopy(true, OldReader);
  try
    Reader.Prototypes.Assign(OldReader.Prototypes);
    FNode := ParseStatements(Lexer, Reader, vtCloseCurlyBracket, false);
  finally
    FreeAndNil(Reader);
    Reader := OldReader;
  end;

  { consume last vtCloseCurlyBracket, ParseStatements doesn't do it }
  Lexer.NextToken;

  Reader.Prototypes.Bind(Self);
end;

procedure TX3DPrototype.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  OldReader: TX3DReaderNames;
  NewName: string;
  E: TDOMElement;
begin
  BaseUrl := Reader.BaseUrl;

  if Element.AttributeString('name', NewName) then
    X3DName := NewName
  else
    raise EX3DXmlError.Create('Missing "name" for <ProtoDeclare> element');

  E := Element.ChildElement('ProtoInterface', false);
  if E <> nil then
    ParseInterfaceDeclarationsXML(false, E, Reader);

  E := Element.ChildElement('ProtoBody', false);
  if E = nil then
    raise EX3DXmlError.CreateFmt('Missing <ProtoBody> inside <ProtoDeclare> element of prototype "%s"', [X3DName]);

  FreeAndNil(FNode);

  { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
    scope, completely independent from the outside.

    Also prototype name scope is local within the prototype,
    however it starts from current prototype name scope (not empty,
    like in case of Reader.Nodes). So prototypes defined outside
    are available inside, but nested prototypes inside are not
    available outside. }
  OldReader := Reader;
  Reader := TX3DReaderNames.CreateCopy(true, OldReader);
  try
    Reader.Prototypes.Assign(OldReader.Prototypes);
    FNode := ParseStatements(E, false, nil, Reader);
  finally
    FreeAndNil(Reader);
    Reader := OldReader;
  end;

  Reader.Prototypes.Bind(Self);
end;

procedure TX3DPrototype.SaveToStream(Writer: TX3DWriter);
var
  OldNodeNames: TX3DNodeNames;
  WriterNames: TX3DWriterNames;
begin
  // TODO: We should save/load prototype names using EncodeX3DName/DecodeX3DName too
  case Writer.Encoding of
    xeClassic: Writer.WriteIndent('PROTO ' + X3DName + ' ');
    xeXML    : Writer.WritelnIndent('<ProtoDeclare name=' + StringToX3DXml(X3DName) + '>');
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DPrototype.SaveToStream Encoding?');
    {$endif}
  end;

  if Writer.Encoding = xeXML then
  begin
    Writer.IncIndent;
    Writer.WritelnIndent('<ProtoInterface>');
    Writer.IncIndent;
  end;

  SaveInterfaceDeclarationsToStream(Writer, false);

  if Writer.Encoding = xeXML then
  begin
    Writer.DecIndent;
    Writer.WritelnIndent('</ProtoInterface>');
  end;

  WriterNames := Writer as TX3DWriterNames;

  { Inside prototype has it's own DEF/USE scope. }
  OldNodeNames := WriterNames.NodeNames;
  WriterNames.NodeNames := TX3DNodeNames.Create(false);
  try
    case Writer.Encoding of
      xeClassic: Writer.WritelnIndent('{');
      xeXML    : Writer.WritelnIndent('<ProtoBody>');
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TX3DPrototype.SaveToStream 2 Encoding?');
      {$endif}
    end;
    { Node may be TX3DRootNode here, that's OK,
      TX3DRootNode.SaveToStream will magically handle this right. }
    Writer.IncIndent;
    Node.SaveToStream(Writer);
    Writer.DecIndent;
    case Writer.Encoding of
      xeClassic: Writer.WritelnIndent('}');
      xeXML    : Writer.WritelnIndent('</ProtoBody>');
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TX3DPrototype.SaveToStream 3 Encoding?');
      {$endif}
    end;
  finally
    FreeAndNil(WriterNames.NodeNames);
    WriterNames.NodeNames := OldNodeNames;
  end;

  if Writer.Encoding = xeXML then
  begin
    Writer.DecIndent;
    Writer.WritelnIndent('</ProtoDeclare>');
  end;
end;

{ TX3DExternalPrototype ----------------------------------------------------- }

constructor TX3DExternalPrototype.Create;
begin
  inherited;
  FURLList := TMFString.Create(nil, false, '', []);
end;

destructor TX3DExternalPrototype.Destroy;
begin
  UnloadReferenced;
  FreeAndNil(FURLList);
  inherited;
end;

procedure TX3DExternalPrototype.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName);
  FX3DName := Lexer.TokenName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtOpenSqBracket);

  Lexer.NextToken;
  ParseInterfaceDeclarations(true, Lexer, Reader);

  URLList.Parse(Lexer, Reader, false);

  Reader.Prototypes.Bind(Self);

  LoadReferenced(Reader.Version);
end;

procedure TX3DExternalPrototype.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  NewName, URLListValue: string;
begin
  BaseUrl := Reader.BaseUrl;

  if Element.AttributeString('name', NewName) then
    X3DName := NewName
  else
    raise EX3DXmlError.Create('Missing "name" for <ExternProtoDeclare> element');

  ParseInterfaceDeclarationsXML(true, Element, Reader);

  if Element.AttributeString('url', URLListValue) then
    URLList.ParseXMLAttribute(URLListValue, Reader) else
    raise EX3DXmlError.Create('Missing "url" for <ExternProtoDeclare> element');

  Reader.Prototypes.Bind(Self);

  LoadReferenced(Reader.Version);
end;

procedure TX3DExternalPrototype.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXTERNPROTO ' + X3DName + ' ');

        SaveInterfaceDeclarationsToStream(Writer, true);

        { Writer.NodeNames will be ignored by URLList
          (TMFString.SaveToStream), don't worry about it. }
        URLList.SaveToStream(Writer);
      end;
    xeXML:
      begin
        Writer.WriteIndent('<ExternProtoDeclare name=' + StringToX3DXml(X3DName) + ' url=');
        URLList.FieldSaveToStream(Writer, true, true);
        Writer.Writeln('>');

        Writer.IncIndent;
        SaveInterfaceDeclarationsToStream(Writer, true);
        Writer.DecIndent;

        Writer.WritelnIndent('</ExternProtoDeclare>');
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DExternalPrototype.SaveToStream Encoding?');
    {$endif}
  end;
end;

procedure TX3DExternalPrototype.LoadReferenced(const Version: TX3DVersion);

  procedure LoadInterfaceDeclarationsValues;
  var
    IIndex: Integer;
    I: TX3DInterfaceDeclaration;
    ReferencedField: TX3DField;
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
          TryFindFieldName(I.Field.X3DName);
        if ReferencedField <> nil then
        begin
          try
            I.Field.AssignValue(ReferencedField);
          except
            on E: EX3DFieldAssign do
            begin
              WritelnWarning('VRML/X3D', Format(
                'Error when linking external prototype "%s" with prototype "%s": ',
                [X3DName, ReferencedPrototype.X3DName]) + E.Message);
            end;
          end;
        end else
          WritelnWarning('VRML/X3D', Format('Prototype "%s" referenced by external ' +
            'prototype "%s" doesn''t have field "%s"',
            [ReferencedPrototype.X3DName, X3DName, I.Field.X3DName]));
      end;
    end;
  end;

  function LoadFromExternalVRML(const RelativeURL: string): boolean;
  var
    URL: string;
    PrototypeNames: TX3DPrototypeNames;

    procedure ProtoWarning(const S: string);
    begin
      WritelnWarning('VRML/X3D', Format('Cannot load external prototype from URL "%s": ',
        [URL]) + S);
    end;

    { Find PROTO (but not EXTERNPROTO) with matching Name.
      Name is ignored if ''.
      @nil if not found. }
    function TryFindProtoNonExternal(const Name: string): TX3DPrototype;
    var
      I: Integer;
    begin
      if PrototypeNames <> nil then
        for I := 0 to PrototypeNames.Count - 1 do
          if PrototypeNames.Objects[I] is TX3DPrototype then
          begin
            Result := TX3DPrototype(PrototypeNames.Objects[I]);
            if (Name = '') or (Result.X3DName = Name) then
              Exit;
          end;
      Result := nil;
    end;

  var
    Anchor: string;
  begin
    Result := false;

    URL := CombineURI(BaseUrl, RelativeURL);
    URIExtractAnchor(URL, Anchor);
    try
      ReferencedPrototypeNode := X3DCache.LoadNode(URL);
      PrototypeNames := ReferencedPrototypeNode.PrototypeNames;
    except
      on E: Exception do
      begin
        ProtoWarning(E.Message);
        Exit;
      end;
    end;

    FReferencedPrototype := TryFindProtoNonExternal(Anchor);
    if FReferencedPrototype = nil then
    begin
      X3DCache.FreeNode(ReferencedPrototypeNode);
      if Anchor = '' then
        ProtoWarning('No PROTO found') else
        ProtoWarning(Format('No PROTO named "%s" found', [Anchor]));
      Exit;
    end;

    Result := true;

    LoadInterfaceDeclarationsValues;
  end;

  function LoadFromURN(const URN: string): boolean;
  begin
    FReferencedClass := NodesManager.URNToClass(URN, Version);
    Result := ReferencedClass <> nil;
    if not Result then
      WritelnWarning('VRML/X3D', Format('Unknown node URN "%s"', [URN]));
  end;

var
  I: Integer;
  S: string;
  ProtoLoaded: boolean;
begin
  UnloadReferenced;

  for I := 0 to URLList.Count - 1 do
  begin
    S := URLList.Items[I];
    if IsPrefix('urn:', S) then
      ProtoLoaded := LoadFromURN(S)
    else
      ProtoLoaded := LoadFromExternalVRML(S);
    if ProtoLoaded then
      Break;
  end;
end;

procedure TX3DExternalPrototype.UnloadReferenced;
begin
  { FReferencedPrototype will be freed as part of ReferencedPrototypeNode }
  FReferencedPrototype := nil;

  X3DCache.FreeNode(ReferencedPrototypeNode);

  FReferencedClass := nil;
end;

{$endif read_implementation}
