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

  TAllowedChildren = (acAll, acClasses, acFunctionality);

  TNodeChangeEvent = procedure (const Sender: TSFNode; const NewNode: TX3DNode) of object;

  { X3D field holding a reference to a single node.
    It's defined in this unit, not in X3DFields, since it uses
    TX3DNode definition. NULL value of the field is indicated by
    Value field = nil.

    Note that we store AllowedChildren list, which is a list of
    classes allowed as a Value (also nil is always allowed).
    But this is used only to produce warnings for a user.
    You should never assert that Value actually is one the requested
    classes. We want to keep here even not allowed items,
    because we want operation "read from VRML file + write to VRML file"
    to be as non-destructible as possible. So if user wrote
    invalid class hierarchy, we will output this invalid class hierarchy. }
  TSFNode = class(TX3DSingleField)
  strict private
    FParentNode: TX3DNode;
    FOnBeforeValueChange: TNodeChangeEvent;
    AllowedChildren: TAllowedChildren;
    AllowedChildrenClasses: TX3DNodeClassesList;
    AllowedChildrenFunctionality: TNodeFunctionalityClass;
    FDefaultValue: TX3DNode;
    FDefaultValueExists: boolean;
    FWeakLink: boolean;
    FAutomaticWeakLink: Boolean;
    procedure SetValue(const AValue: TX3DNode);
    procedure SetDefaultValue(const ADefaultValue: TX3DNode);
    procedure SetDefaultValueExists(const AValue: boolean);
    procedure SetWeakLink(const AValue: boolean);
    procedure WarningIfUnusedWeakLink(const Reader: TX3DReaderNames);
    procedure DestructionNotification(const Node: TX3DNode);
  private
    FValue: TX3DNode;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
  public
    { Construct a field allowing any children class.
      Suitable only for special cases. For example, in instantiated prototypes,
      we must initially just allow all children, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing. }
    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); override;
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AAllowedChildrenClasses: array of TX3DNodeClass;
      const AValue: TX3DNode = nil); overload;
    { Constructor that takes a list of allowed children classes.
      Note that we copy the contents of AAllowedChildrenClasses,
      not the reference. }
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AAllowedChildrenClasses: TX3DNodeClassesList;
      const AValue: TX3DNode = nil); overload;
    { Constructor that allows as children any implementor of given functionality. }
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AnAllowedChildrenFunctionality: TNodeFunctionalityClass;
      const AValue: TX3DNode = nil); overload;
    destructor Destroy; override;

    { Default value of SFNode field.

      While X3D specification says for all SFNode fields that their
      default value is NULL, this is not necessarily true for PROTO
      SFNode fiels. So we have to take into account that any DefaultValue
      is possible.

      Note that this doesn't have to be @nil, but will be irrelevant
      if not DefaultValueExists. (Once I had an idea to automatically
      set DefaultValue to @nil when DefaultValueExists is set to @false,
      but this was uncomfortable (like "what to do when DefaultValue
      is assigned non-nil when DefaultValueExists is false?").)

      Freeing of this is automatically managed, just like the normal
      @link(Value) property. This means that you can simply set
      DefaultValue to @nil or some existing node, and eventual memory
      deallocation of previous DefaultValue node (if unused) will happen
      automatically. }
    property DefaultValue: TX3DNode
      read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write SetDefaultValueExists default false;

    property Value: TX3DNode read FValue write SetValue;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    procedure ParseXMLElement(Element: TDOMElement; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure UnassignDefaultValue; override;

    { @exclude }
    procedure InternalAssignDeepCopy(const Source: TX3DFieldOrEvent; const CopyState: TObject); override;

    { VRML node containing this field. May be @nil if unknown, in special
      cases.

      Note that this property is exactly the same as
      TX3DFieldOrEvent.ParentNode,
      contains always the same value. But this is declared as TX3DNode,
      so it's more comfortable. }
    property ParentNode: TX3DNode read FParentNode;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Checks is the Child allowed as a value of this SFNode,
      and makes WritelnWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      WritelnWarning message will suggest that this Child is used as value
      of this node. In other words, you should only pass as Child
      a node that you want to assign as Value to this field,
      otherwise WritelnWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TX3DNode);

    function ChildAllowed(Child: TX3DNode): boolean;
    function CurrentChildAllowed: boolean;

    { Calls Func for our @link(Value), assuming it's set (non-nil).
      The main use for this is to simplify implementation of
      TX3DNode.DirectEnumerateActive overrides in TX3DNode descendants. }
    function Enumerate(Func: TEnumerateChildrenFunction): Pointer;

    procedure Send(const AValue: TX3DNode); overload;

    { Use weak links to deal with cycles in the X3D graph.

      Marking a field as a @italic(weak link) can only be done
      when the field value is empty, right when the field is created,
      in @link(TX3DNode.CreateNode) descendant.

      Being a @italic(weak link) means two things:

      @orderedList(
        @item(The nodes inside a weak link are not enumerated
          when traversing the X3D graph in @italic(any) way.
          This includes @link(TX3DNode.EnumerateNodes),
          @link(TX3DNode.Traverse) and all others.
          Nodes implementing @link(TX3DNode.DirectEnumerateActive)
          should also omit these fields.)

        @item(A weak link does not create a reference count
          preventing the node from being freed (or freeing
          it automatically when ref count drops to zero).
          Instead, weak links merely observe the nodes, and automatically
          set their value to @nil when the node gets freed.)
      )

      If effect, this avoids loops when enumerating (and avoids
      recursive loops in reference counts, which would cause memory leaks),
      but use this only when you know that the node
      must occur somewhere else in the X3D graph anyway (or it's OK to
      ignore it).
      For example, this is useful for
      @link(TGeneratedShadowMapNode.Light), as we know that the light
      must occur somewhere else in the graph anyway to be useful.
    }
    property WeakLink: boolean
      read FWeakLink write SetWeakLink default false;

    (*Automatically use WeakLink if field would contain a node cycle.
      Using WeakLink=true allows to handle node cycles, like common in VRML/X3D

      @preformatted(
      DEF Xxx Transform {
        children [
          ...
          Script {
            ...
            inputOutput SFNode someField USE Xxx
          }
        ]
      }
      )

      When AutomaticWeakLink, the given field contents may, but don't have to,
      be a node cycle -- we only use WeakLink when necessary.

      Use this only for fields that are not enumerated by DirectEnumerateActive,
      Traverse etc., otherwise you create a cycle in nodes that we generally cannot handle. *)
    property AutomaticWeakLink: Boolean
      read FAutomaticWeakLink write FAutomaticWeakLink default false;

    { Called when @link(Value) changes.
      Called before the change is actually done,
      right after we know that new value is different than old value. }
    property OnBeforeValueChange: TNodeChangeEvent
      read FOnBeforeValueChange write FOnBeforeValueChange;
  end;

  TSFNodeEventHelper = class helper for TSFNodeEvent
    procedure Send(const Value: TX3DNode; const Time: TX3DTime;
      const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TX3DNode); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSFNode --------------------------------------------------------------------- }

constructor TSFNode.CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  inherited;
  Value := nil;

  AllowedChildren := acAll;
  { AllowedChildrenClasses may remain nil in this case }

  FDefaultValue := nil;
  FDefaultValueExists := false;
end;

constructor TSFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AAllowedChildrenClasses: array of TX3DNodeClass;
  const AValue: TX3DNode);
begin
  inherited Create(AParentNode, AExposed, AName);

  { FParentNode is just a copy of inherited (TX3DFieldOrEvent) FParentNode,
    but casted to TX3DNode }
  FParentNode := AParentNode;

  AllowedChildren := acClasses;
  if AllowedChildrenClasses = nil then
    AllowedChildrenClasses := TX3DNodeClassesList.Create;
  AllowedChildrenClasses.AssignArray(AAllowedChildrenClasses);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

constructor TSFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AAllowedChildrenClasses: TX3DNodeClassesList;
  const AValue: TX3DNode);
begin
  Create(AParentNode, AExposed, AName, [], AValue);

  Assert(AllowedChildren = acClasses);
  Assert(AllowedChildrenClasses <> nil);
  AllowedChildrenClasses.Assign(AAllowedChildrenClasses);
end;

constructor TSFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AnAllowedChildrenFunctionality: TNodeFunctionalityClass;
  const AValue: TX3DNode);
begin
  inherited Create(AParentNode, AExposed, AName);

  { FParentNode is just a copy of inherited (TX3DFieldOrEvent) FParentNode,
    but casted to TX3DNode }
  FParentNode := AParentNode;

  AllowedChildren := acFunctionality;
  AllowedChildrenFunctionality := AnAllowedChildrenFunctionality;

  Value := AValue;
  AssignDefaultValueFromValue;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  { To delete Self from DefaultValue.FParentFields, and eventually free DefaultValue. }
  DefaultValue := nil;
  FreeAndNil(AllowedChildrenClasses);
  inherited;
end;

function TSFNode.ChildAllowed(Child: TX3DNode): boolean;
begin
  case AllowedChildren of
    acAll          : Result := true;
    acClasses      : Result := (Child = nil) or (AllowedChildrenClasses.IndexOfAnyAncestor(Child) <> -1);
    acFunctionality: Result := (Child = nil) or (Child.Functionality(AllowedChildrenFunctionality) <> nil);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('AllowedChildren?');
    {$endif}
  end;
end;

function TSFNode.CurrentChildAllowed: boolean;
begin
  Result := ChildAllowed(Value);
end;

procedure TSFNode.WarningIfChildNotAllowed(Child: TX3DNode);

  procedure ChildNotAllowed;
  var
    S: string;
  begin
    S := Format('Node "%s" is not allowed in the field "%s"',
      [Child.X3DType, X3DName]);
    if ParentNode <> nil then
      S := S + Format(' of the node "%s"', [ParentNode.X3DType]);
    WritelnWarning('X3D', S);
  end;

begin
  if not ChildAllowed(Child) then
    ChildNotAllowed;
end;

procedure TSFNode.WarningIfUnusedWeakLink(const Reader: TX3DReaderNames);
begin
  if WeakLink and
     (Value <> nil) and
     (Value.VRML1ParentsCount = 0) and
     (Value.FParentFields.Count = 0) and
     (Value.KeepExisting = 0) and
     (not Reader.Nodes.NodeDuringReading(Value)) then
  begin
    FValue.FreeIfUnused; // we know it will be freed now
    FValue := nil;
    { do a warning after freeing FValue, to avoid memory leaks in case OnWarning makes exception }
    WritelnWarning('X3D', Format('A node inside the field "%s" must be already used elsewhere (use USE clause, do not declare a new node here)',
      [NiceName]));
  end;
end;

procedure TSFNode.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  UsingNodeCycle: Boolean;
  NewValue: TX3DNode;
begin
  if (Lexer.Token = vtKeyword) and (Lexer.TokenKeyword = vkNULL) then
  begin
    Value := nil;
    Lexer.NextToken;
  end else
  begin
    UsingNodeCycle := false;
    { This is one case when we can use NilIfUnresolvedUSE = @true }
    NewValue := ParseNode(Lexer, Reader as TX3DReaderNames, true, AutomaticWeakLink, UsingNodeCycle);
    if UsingNodeCycle then
      WeakLink := true;

    { Copying NewValue -> Value only after WeakLink was adjusted by AutomaticWeakLink,
      as Value cannot change once WeakLink changed. }
    Value := NewValue;

    if Value <> nil then
    begin
      WarningIfChildNotAllowed(Value);
      WarningIfUnusedWeakLink(Reader as TX3DReaderNames);
    end;
  end;
end;

procedure TSFNode.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
const
  SNull = 'NULL';
var
  UsedNodeFinished: boolean;
  NewValue: TX3DNode;
begin
  { For SFNode and MFNode, X3D XML encoding has special handling:
    field value just indicates the node name, or NULL.
    (other values for SFNode / MFNode cannot be expressed inside
    the attribute). }

  NewValue := (Reader as TX3DReaderNames).Bound(AttributeValue, UsedNodeFinished, true);
  { When AutomaticWeakLink, then UsedNodeFinished=false is not a problem.
    Just flip WeakLink to true, and we can handle a loop in node definition. }
  if (NewValue <> nil) and (not UsedNodeFinished) and AutomaticWeakLink then
  begin
    WeakLink := true;
    UsedNodeFinished := true;
  end;
  if (NewValue <> nil) and (not UsedNodeFinished) then
  begin
    WritelnWarning('X3D', Format('Cycles in X3D graph: SFNode value inside node "%s" refers to the same name', [AttributeValue]));
    Value := nil;
    Exit;
  end;

  Value := NewValue;
  if Value = nil then
  begin
    if AttributeValue <> SNull then
      WritelnWarning('X3D', Format('Invalid node name for SFNode field: "%s"', [AttributeValue]));
  end else
  begin
    WarningIfChildNotAllowed(Value);
    WarningIfUnusedWeakLink(Reader as TX3DReaderNames);
  end;
end;

procedure TSFNode.ParseXMLElement(Element: TDOMElement; Reader: TX3DReader);
var
  Child: TX3DNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
  UsingNodeCycle: Boolean;
begin
  I := Element.ChildrenIterator;
  try
    if I.GetNext then
    begin
      UsingNodeCycle := false;
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Reader as TX3DReaderNames,
        true, AutomaticWeakLink, UsingNodeCycle);
      if UsingNodeCycle then
        WeakLink := true;

      if Child <> nil then
      begin
        Value := Child;
        WarningIfChildNotAllowed(Child);
      end;

      if I.GetNext then
        WritelnWarning('X3D', Format('Field "%s" is SFNode, but it contains more than one XML element (2nd element is "%s")',
          [X3DName, I.Current.TagName]));
    end;
  finally FreeAndNil(I) end;
end;

procedure TSFNode.SaveToStreamValue(Writer: TX3DWriter);
begin
  if Value = nil then
    { For XML encoding, note that the NULL value can only be saved
      as an XML attribute (not child element).
      Also, there's no way to specify containerField for NULL value
      --- and that's Ok, since NULL is the default value of all SFNode fields,
      so it's never actually written in normal cases. }
    Writer.Write('NULL') else
  begin
    { TX3DNode.SaveToStream normally starts from new line with an indent.
      In this case, we want it to start on the same line, so indent must
      be discarded. }
    if Writer.Encoding = xeClassic then
      Writer.DiscardNextIndent;

    Value.NodeSaveToStream(Writer, NameForVersion(Writer.Version));
  end;
end;

function TSFNode.SaveToXmlValue: TSaveToXmlMethod;
begin
  { NULL can only be encoded as an attribute in XML encoding }
  if Value = nil then
    Result := sxAttribute else
    Result := sxChildElement;
end;

function TSFNode.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (Value = DefaultValue);
end;

function TSFNode.Equals(SecondValue: TX3DField): boolean;
begin
 Result := (inherited Equals(SecondValue)) and
   (SecondValue is TSFNode) and
   (TSFNode(SecondValue).Value = Value);
end;

procedure TSFNode.Assign(Source: TPersistent);
begin
  if Source is TSFNode then
  begin
    { Assign using Value property, so that FParentFields will get
      correctly updated. }
    Value              := TSFNode(Source).Value;
    DefaultValue       := TSFNode(Source).DefaultValue;
    DefaultValueExists := TSFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TX3DField(Source));
    { When updating this, remember to also update TSFNode.InternalAssignDeepCopy }
  end else
    inherited;
end;

procedure TSFNode.InternalAssignDeepCopy(const Source: TX3DFieldOrEvent; const CopyState: TObject);
var
  GoodCopyState: TX3DNodeDeepCopyState;

  function DeepCopyNode(const Node: TX3DNode): TX3DNode;
  begin
    if Node = nil then
      Result := nil
    else
      Result := GoodCopyState.DeepCopy(Node);
  end;

begin
  Assert(Source is TSFNode);
  GoodCopyState := CopyState as TX3DNodeDeepCopyState;

  { Similar to TSFNode.Assign }
  Value              := DeepCopyNode(TSFNode(Source).Value);
  DefaultValue       := DeepCopyNode(TSFNode(Source).DefaultValue);
  DefaultValueExists := TSFNode(Source).DefaultValueExists;
  VRMLFieldAssignCommon(TX3DField(Source));
end;

procedure TSFNode.AssignValue(Source: TX3DField);
begin
  if Source is TSFNode then
  begin
    inherited;
    Value := TSFNode(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFNode.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSFNode.UnassignDefaultValue;
begin
  DefaultValueExists := false;
end;

procedure TSFNode.SetValue(const AValue: TX3DNode);
begin
  if FValue <> AValue then
  begin
    if Assigned(OnBeforeValueChange) then
      OnBeforeValueChange(Self, AValue);

    if FValue <> nil then
    begin
      if WeakLink then
        FValue.RemoveDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification)
      else
        FValue.RemoveParentField(Self);
    end;

    FValue := AValue;

    if AValue <> nil then
    begin
      if WeakLink then
        FValue.AddDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification)
      else
        FValue.AddParentField(Self);
    end;
  end;
end;

procedure TSFNode.SetDefaultValue(const ADefaultValue: TX3DNode);
begin
  if FDefaultValue <> ADefaultValue then
  begin
    if FDefaultValue <> nil then
    begin
      if WeakLink then
        FDefaultValue.RemoveDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification)
      else
        FDefaultValue.RemoveParentField(Self);
    end;

    FDefaultValue := ADefaultValue;

    if ADefaultValue <> nil then
    begin
      if WeakLink then
        FDefaultValue.AddDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification)
      else
        FDefaultValue.AddParentField(Self);
    end;
  end;
end;

procedure TSFNode.DestructionNotification(const Node: TX3DNode);
begin
  if WeakLink then
  begin
    if FValue = Node then
      FValue := nil;
    if FDefaultValue = Node then
      FDefaultValue := nil;
  end;
end;

procedure TSFNode.SetDefaultValueExists(const AValue: boolean);
begin
  FDefaultValueExists := AValue;
end;

class function TSFNode.X3DType: string;
begin
  Result := 'SFNode';
end;

function TSFNode.Enumerate(Func: TEnumerateChildrenFunction): Pointer;
begin
  { checking CurrentChildAllowed is not really necessary here,
    and costs time, because it may do a slow Supports() call }
  //if (Value <> nil) and CurrentChildAllowed and not WeakLink then

  if (Value <> nil) and not WeakLink then
    Result := Func(ParentNode, Value)
  else
    Result := nil;
end;

class function TSFNode.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFNodeEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFNode.Send(const AValue: TX3DNode);
var
  FieldValue: TSFNode;
begin
  { We construct using CreateUndefined constructor,to have AllowedChildren = acAll }
  { AExposed = false below, because not needed otherwise. }
  FieldValue := TSFNode.CreateUndefined(ParentNode, false, X3DName);
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

procedure TSFNode.SetWeakLink(const AValue: boolean);
begin
  if FWeakLink <> AValue then
  begin
    if Value <> nil then
      raise EInternalError.Create('TSFNode.WeakLink cannot change when some node is already assigned');
    FWeakLink := AValue;
  end;
end;

{ TSFNodeEventHelper --------------------------------------------------------- }

procedure TSFNodeEventHelper.Send(const Value: TX3DNode; const Time: TX3DTime;
  const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFNode).Value := Value;
  try
    Self.Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFNodeEventHelper.Send(const Value: TX3DNode);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{$endif read_implementation}
