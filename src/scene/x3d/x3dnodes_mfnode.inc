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

  { VRML/X3D field holding a list of nodes.

    Just like SFNode, it's defined in this unit, as it uses TX3DNode.
    Note that items of MFNode @italic(cannot) be nil (i.e. VRML/X3D doesn't
    allow to use NULL inside MFNode), contrary to SFNode.

    Note that TMFNode implementation doesn't use TX3DSimpleMultField.
    One reason is that we don't want to parse MFNode items
    by SFNode parser, because MFNode doesn't allow NULL items.
    (In the past, another argument was that we want to use TX3DNodeList
    and it wasn't compatible with TX3DSimpleMultField.
    But now TX3DNodeList descends from TFPSList, so it isn't a problem.)

    Just like for TSFNode:
    Note that we store AllowedChildren list, which is a list of
    classes allowed as Items.
    But this is used only to produce warnings for a user.
    You should never assert that every item actually is one the requested
    classes.  }
  TMFNode = class(TX3DMultField)
  strict private
    type
      TEnumerator = record
      strict private
        FList: TX3DNodeList;
        FPosition: Integer;
        function GetCurrent: TX3DNode; inline;
      public
        constructor Create(const AList: TX3DNodeList);
        function MoveNext: Boolean; inline;
        property Current: TX3DNode read GetCurrent;
      end;
    var
      FDefaultItems: TX3DNodeList;
      FDefaultValueExists: boolean;
      FParentNode: TX3DNode;
      AllowedChildren: TAllowedChildren;
      AllowedChildrenClasses: TX3DNodeClassesList;
      AllowedChildrenFunctionality: TNodeFunctionalityClass;
    function GetItems(const Index: Integer): TX3DNode;
    procedure AddSelfParentField(const AItems: TX3DNodeList);
    procedure AssignItemsDeepCopy(const SourceItems: TX3DNodeList;
      const CopyState: TX3DNodeDeepCopyState);
    procedure AssignDefaultItemsDeepCopy(const SourceDefaultItems: TX3DNodeList;
      const CopyState: TX3DNodeDeepCopyState);
  private
    FItems: TX3DNodeList;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
    { Get or set the number of items.

      When increasing this, remember that new items of TMFNode
      will be @nil. You @bold(must immediately initialize them to
      something else then @nil) by the @link(Replace) method.
      Other TMFNode methods, and outside code working with MFNodes,
      usually assumes that all MFNode children are non-nil.
      (As VRML/X3D spec don't really allow NULL items inside MFNode fields.)
      @groupBegin }
    function GetCount: TListSize; override;
    procedure SetCount(const Value: TListSize); override;
    { @groupEnd }
  public
    { Construct a field allowing any children class.
      Suitable only for special cases. For example, in instantiated prototypes,
      we must initially just allow all children, otherwise valid prototypes
      with SFNode/MFNode would cause warnings when parsing. }
    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); override;
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AAllowedChildrenClasses: array of TX3DNodeClass); overload;
    { Constructor that takes a list of allowed children classes.
      Note that we copy the contents of AAllowedChildrenClasses,
      not the reference. }
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AAllowedChildrenClasses: TX3DNodeClassesList); overload;
    { Constructor that allows as children any implementor of given functionality. }
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AName: string;
      const AnAllowedChildrenFunctionality: TNodeFunctionalityClass); overload;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator;

    procedure Replace(const Index: Integer; Node: TX3DNode);

    { Items of this field. }
    property Items[const Index: Integer]: TX3DNode read GetItems write Replace; default;
    {$ifdef FPC}
    property ItemsArray[Index: Integer]: TX3DNode read GetItems; deprecated 'use Items (default array property)';
    {$endif}

    { Lists of items in this field.
      Do not modify this list explicitly. Use only methods in this class
      like @link(Add). They take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      can break reference-counting of nodes by ParentFields.
      @exclude }
    property InternalItems: TX3DNodeList read FItems;

    procedure Add(Node: TX3DNode); overload;
    procedure Add(Position: Integer; Node: TX3DNode); overload;

    procedure Delete(Index: Integer);
    { Search list for given node, and, if found, remove it.
      Returns the index of removed item, or -1 if not found. }
    function Remove(const Node: TX3DNode): Integer;
    { Remove child with given Index, and return it, @italic(never freeing it).
      This is analogous to TX3DNode.ExtractChild, see there for more
      explanation. }
    function Extract(Index: Integer): TX3DNode;
    procedure Clear;
    procedure AssignItems(const SourceItems: TX3DNodeList); overload;
    procedure AssignItems(const SourceItems: array of TX3DNode); overload;

    { @exclude }
    procedure InternalAssignDeepCopy(const Source: TX3DFieldOrEvent; const CopyState: TObject); override;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    procedure ParseXMLElement(Element: TDOMElement; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure UnassignDefaultValue; override;

    property ParentNode: TX3DNode read FParentNode;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    { Checks is Child allowed on the list of nodes of this MFNode,
      and makes WritelnWarning if not.

      Check is allowed is done looking at AllowedChildrenAll
      and AllowedChildren properties.

      Child must not be @nil.

      WritelnWarning message will suggest that this Child is added to
      this node. In other words, you should only pass as Child
      a node that you want to add (e.g. by @link(Add)) to this field,
      otherwise WritelnWarning message will be a little unsensible. }
    procedure WarningIfChildNotAllowed(Child: TX3DNode);

    function ChildAllowed(Child: TX3DNode): boolean;

    { Lists default items of this field.

      Do not modify this list explicitly. Use only methods in this class
      like AssignDefaultItems (they take care of calling appropriate
      AddParentField / RemoveParentField, otherwise you
      could break reference-counting of nodes by ParentFields). }
    property DefaultItems: TX3DNodeList read FDefaultItems;

    { Operate on DefaultItems, just like analogous AssignItems and @link(Clear).
      @groupBegin }
    procedure AssignDefaultItems(const SourceDefaultItems: TX3DNodeList);
    procedure ClearDefault;
    { @groupEnd }

    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists default false;

    { Calls Func for all current children.
      Stops if Func returns something non-nil.
      The main use for this is to simplify implementation of
      TX3DNode.DirectEnumerateActive overrides in TX3DNode descendants. }
    function Enumerate(Func: TEnumerateChildrenFunction): Pointer;

    { Set the field's value in a correct way (by sending X3D event, or at least notifying
      the parent scene). }
    procedure Send(const AValue: array of TX3DNode); overload;

    // Not needed now
    //procedure Send(const AValue: TX3DNodeList); overload;

    { Find node by name, @nil if not found.

      For empty node name, always returns @nil. This follows the definition
      of @link(TX3DNode.X3DName):
      empty means that node has no name, so it should not be found
      by searches by name, ever.

      @seealso(IndexOfName Returns the node index. This is sometimes more
        flexible (you can always quickly get actual node knowing the index,
        by Items[Index], but not the other way around). It is also sometimes
        less comfortable (often you're not interested in node index).) }
    function FindName(const AName: string): TX3DNode;

    { Find index of a node with given name, -1 if not found. }
    function IndexOfName(const AName: string): Integer;
    { Find index of a node , -1 if not found. }
    function IndexOf(const ANode: TX3DNode): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMFNode -------------------------------------------------------------------- }

constructor TMFNode.CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  inherited;
  FItems := TX3DNodeList.Create(false);

  AllowedChildren := acAll;
  { AllowedChildrenClasses may remain nil in this case }

  FDefaultItems := TX3DNodeList.Create(false);
  FDefaultValueExists := false;
end;

constructor TMFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AAllowedChildrenClasses: array of TX3DNodeClass);
begin
  inherited Create(AParentNode, AExposed, AName);
  FParentNode := AParentNode;

  AllowedChildren := acClasses;
  if AllowedChildrenClasses = nil then
    AllowedChildrenClasses := TX3DNodeClassesList.Create;
  AllowedChildrenClasses.AssignArray(AAllowedChildrenClasses);

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

constructor TMFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AAllowedChildrenClasses: TX3DNodeClassesList);
begin
  Create(AParentNode, AExposed, AName, []);

  Assert(AllowedChildren = acClasses);
  Assert(AllowedChildrenClasses <> nil);
  AllowedChildrenClasses.Assign(AAllowedChildrenClasses);
end;

constructor TMFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AName: string;
  const AnAllowedChildrenFunctionality: TNodeFunctionalityClass);
begin
  inherited Create(AParentNode, AExposed, AName);
  FParentNode := AParentNode;

  AllowedChildren := acFunctionality;
  AllowedChildrenFunctionality := AnAllowedChildrenFunctionality;

  { In the future, this constructor may also allow setting DefaultItems
    from parameters. For now, this is not needed anywhere.
    We assume DefaultItems = [] if you used this constructor. }
  DefaultValueExists := true;
end;

destructor TMFNode.Destroy;
begin
  Clear;
  ClearDefault;
  FreeAndNil(FItems);
  FreeAndNil(FDefaultItems);
  FreeAndNil(AllowedChildrenClasses);
  inherited;
end;

procedure TMFNode.SaveToStreamValue(Writer: TX3DWriter);
var
  I: Integer;
  N: string;
begin
  N := NameForVersion(Writer.Version);
  case Writer.Encoding of
    xeClassic:
      { We code Count = 0 and Count = 1 cases separately just to get a more
        compact look in these common situations. }
      if Count = 0 then
        Writer.Write('[]') else
      if Count = 1 then
      begin
        { TX3DNode.SaveToStream normally starts from new line with an indent...
          In this case, we want it to start on the same line, so indent must
          be discarded. }
        Writer.DiscardNextIndent;
        FItems[0].NodeSaveToStream(Writer, N);
      end else
      begin
        Writer.Writeln('[');
        Writer.IncIndent;
        for I := 0 to Count - 1 do
        begin
          FItems[I].NodeSaveToStream(Writer, N);
          Writer.Writeln;
        end;
        Writer.DecIndent;
        Writer.WriteIndent(']');
      end;
    xeXML:
      for I := 0 to Count - 1 do
        FItems[I].NodeSaveToStream(Writer, N);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TMFNode.SaveToStreamValue Encoding?');
    {$endif}
  end;
end;

function TMFNode.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxChildElement;
end;

function TMFNode.GetCount: TListSize;
begin
  Result := FItems.Count;
end;

procedure TMFNode.SetCount(const Value: TListSize);
var
  I: Integer;
begin
  if Value < FItems.Count then
  begin
    for I := Value to FItems.Count - 1 do
      FItems[I].RemoveParentField(Self);
    FItems.Count := Value;
  end else
  if Value > FItems.Count then
  begin
    { TObjectList makes sure that increasing count sets new items to nil }
    FItems.Count := Value;
  end;
end;

procedure TMFNode.Add(Node: TX3DNode);
begin
  if Node = nil then
    raise EX3DError.Create('Cannot add nil node to an MFNode field ' + NiceName);
  FItems.Add(Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.Add(Position: Integer; Node: TX3DNode);
begin
  if Node = nil then
    raise EX3DError.Create('Cannot add nil node to an MFNode field ' + NiceName);
  FItems.Insert(Position, Node);
  Node.AddParentField(Self);
end;

procedure TMFNode.Delete(Index: Integer);
begin
  FItems[Index].RemoveParentField(Self);
  FItems.Delete(Index);
end;

function TMFNode.Remove(const Node: TX3DNode): Integer;
begin
  Result := FItems.IndexOf(Node);
  if Result <> -1 then
    Delete(Result);
end;

function TMFNode.Extract(Index: Integer): TX3DNode;
begin
  Result := FItems[Index];

  { Instead of calling Result.RemoveParentField(Self), which would possibly
    free Result, we manually call FParentFields.Delete. }
  Result.FParentFields.Remove(Self);

  FItems.Delete(Index);
end;

procedure TMFNode.Replace(const Index: Integer; Node: TX3DNode);
begin
  if FItems[Index] <> Node then
  begin
    { Replace is the only method that must work even when items are nil }
    if FItems[Index] <> nil then
      FItems[Index].RemoveParentField(Self);
    FItems[Index] := Node;
    FItems[Index].AddParentField(Self);
  end;
end;

procedure TMFNode.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].RemoveParentField(Self);
  FItems.Count := 0;
end;

procedure TMFNode.ClearDefault;
var
  I: Integer;
begin
  for I := 0 to FDefaultItems.Count - 1 do
    FDefaultItems[I].RemoveParentField(Self);
  FDefaultItems.Count := 0;
end;

procedure TMFNode.AddSelfParentField(const AItems: TX3DNodeList);
var
  I: Integer;
begin
  for I := 0 to AItems.Count - 1 do
    AItems[I].AddParentField(Self);
end;

procedure TMFNode.AssignItems(const SourceItems: array of TX3DNode);
begin
  Clear;
  FItems.Assign(SourceItems);
  AddSelfParentField(FItems);
end;

procedure TMFNode.AssignItems(const SourceItems: TX3DNodeList);
begin
  Clear;
  FItems.Assign(SourceItems);
  AddSelfParentField(FItems);
end;

procedure TMFNode.AssignDefaultItems(const SourceDefaultItems: TX3DNodeList);
begin
  ClearDefault;
  DefaultItems.Assign(SourceDefaultItems);
  AddSelfParentField(DefaultItems);
end;

procedure TMFNode.AssignItemsDeepCopy(const SourceItems: TX3DNodeList;
  const CopyState: TX3DNodeDeepCopyState);
var
  I: Integer;
begin
  Clear;
  FItems.Assign(SourceItems);
  for I := 0 to FItems.Count - 1 do
    FItems[I] := CopyState.DeepCopy(FItems[I]);
  AddSelfParentField(FItems);
end;

procedure TMFNode.AssignDefaultItemsDeepCopy(const SourceDefaultItems: TX3DNodeList;
  const CopyState: TX3DNodeDeepCopyState);
var
  I: Integer;
begin
  ClearDefault;
  DefaultItems.Assign(SourceDefaultItems);
  for I := 0 to DefaultItems.Count - 1 do
    DefaultItems[I] := CopyState.DeepCopy(DefaultItems[I]);
  AddSelfParentField(DefaultItems);
end;

function TMFNode.ChildAllowed(Child: TX3DNode): boolean;
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

procedure TMFNode.WarningIfChildNotAllowed(Child: TX3DNode);

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

procedure TMFNode.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure ParseOneItem;
  var
    Node: TX3DNode;
  begin
    Node := ParseNode(Lexer, Reader as TX3DReaderNames, false);
    Add(Node);
    WarningIfChildNotAllowed(Node);
  end;

begin
  Clear;

  { Note that we ignore commas here, because MFNode is in VRML 2.0 only. }
  if Lexer.Token = vtOpenSqBracket then
  begin
    Lexer.NextToken;

    while Lexer.Token <> vtCloseSqBracket do
      ParseOneItem;

    Lexer.NextToken;
  end else
  begin
    { one single item - not enclosed in [] brackets }
    ParseOneItem;
  end;
end;

procedure TMFNode.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Node: TX3DNode;
  UsedNodeFinished: boolean;
begin
  Node := (Reader as TX3DReaderNames).Bound(AttributeValue, UsedNodeFinished, true);
  if Node = nil then
  begin
    { NULL not allowed for MFNode, unlike the SFNode }
    WritelnWarning('X3D', Format('Invalid node name for MFNode field: "%s"', [AttributeValue]));
  end else
  if not UsedNodeFinished then
  begin
    WritelnWarning('X3D', Format('Cycles in X3D graph: MFNode value inside node "%s" refers to the same name', [AttributeValue]));
  end else
  begin
    Add(Node);
    WarningIfChildNotAllowed(Node);
  end;
end;

procedure TMFNode.ParseXMLElement(Element: TDOMElement; Reader: TX3DReader);
var
  Child: TX3DNode;
  I: TXMLElementIterator;
  ContainerFieldDummy: string;
begin
  { Clear is necessary because MFNode default value may be non-empty,
    when it's defined by a prototype.
    See http://web3d.org/pipermail/x3d-public_web3d.org/2016-May/004771.html }
  Clear;

  I := Element.ChildrenIterator;
  try
    while I.GetNext do
    begin
      Child := ParseXMLNode(I.Current,
        ContainerFieldDummy { ignore containerField }, Reader as TX3DReaderNames, true);
      if Child <> nil then
      begin
        Add(Child);
        WarningIfChildNotAllowed(Child);
      end;
    end;
  finally FreeAndNil(I) end;
end;

function TMFNode.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and DefaultItems.Equals(FItems);
end;

function TMFNode.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TMFNode) and
    (TMFNode(SecondValue).FItems.Equals(FItems));
end;

procedure TMFNode.Assign(Source: TPersistent);
begin
  if Source is TMFNode then
  begin
    AssignItems(TMFNode(Source).FItems);
    AssignDefaultItems(TMFNode(Source).DefaultItems);
    DefaultValueExists := TMFNode(Source).DefaultValueExists;
    VRMLFieldAssignCommon(TX3DField(Source));
    { When updating this, remember to also update TMFNode.InternalAssignDeepCopy }
  end else
    inherited;
end;

procedure TMFNode.InternalAssignDeepCopy(const Source: TX3DFieldOrEvent; const CopyState: TObject);
var
  GoodCopyState: TX3DNodeDeepCopyState;
begin
  Assert(Source is TMFNode);
  GoodCopyState := CopyState as TX3DNodeDeepCopyState;

  { Similar to TMFNode.Assign }
  AssignItemsDeepCopy       (TMFNode(Source).FItems      , GoodCopyState);
  AssignDefaultItemsDeepCopy(TMFNode(Source).DefaultItems, GoodCopyState);
  DefaultValueExists := TMFNode(Source).DefaultValueExists;
  VRMLFieldAssignCommon(TX3DField(Source));
end;

procedure TMFNode.AssignValue(Source: TX3DField);
begin
  if Source is TMFNode then
  begin
    inherited;
    AssignItems(TMFNode(Source).FItems);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TMFNode.AssignDefaultValueFromValue;
begin
  inherited;
  AssignDefaultItems(FItems);
  DefaultValueExists := true;
end;

procedure TMFNode.UnassignDefaultValue;
begin
  DefaultValueExists := false;
end;

class function TMFNode.X3DType: string;
begin
  Result := 'MFNode';
end;

function TMFNode.Enumerate(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  { Checking ChildAllowed is not really necessary here,
    and costs time, because it may do a slow Supports / Functionality call }
  //if ChildAllowed(Items[I]) then
  begin
    Result := Func(ParentNode, FItems[I]);
    if Result <> nil then Exit;
  end;
end;

function TMFNode.GetItems(const Index: Integer): TX3DNode;
begin
  Result := FItems[Index];
end;

class function TMFNode.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFNodeEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TMFNode.Send(const AValue: array of TX3DNode);
var
  FieldValue: TMFNode;
begin
  { implementation just like TX3DSimpleMultField.Send }
  if SendsByEvent then
  begin
    { AExposed = false below, because not needed otherwise. }
    FieldValue := TMFNode.CreateUndefined(ParentNode, false, X3DName);
    try
      FieldValue.AssignItems(AValue);
      Send(FieldValue);
    finally FreeAndNil(FieldValue) end;
  end else
  begin
    { When SendsByEvent = false, we can optimize this method.
      No need to create an intermediate FieldValue instance
      (and assign AValue to FieldValue, just to later assign FieldValue to Items). }
    ValueFromIsClause := false;
    AssignItems(AValue);
    Changed;
  end;
end;

function TMFNode.FindName(const AName: string): TX3DNode;
begin
  Result := FItems.FindName(AName);
end;

function TMFNode.IndexOfName(const AName: string): Integer;
begin
  Result := FItems.IndexOfName(AName);
end;

function TMFNode.IndexOf(const ANode: TX3DNode): Integer;
begin
  Result := FItems.IndexOf(ANode);
end;


// Not needed now
// procedure TMFNode.Send(const AValue: TX3DNodeList);
// var
//   FieldValue: TMFNode;
// begin
//   { This is a copy-paste of implementation of Send(const AValue: array of TItem) }
//   if SendsByEvent then
//   begin
//     FieldValue := TMFNode.CreateUndefined(ParentNode, false, X3DName);
//     try
//       FieldValue.Items.AddRange(AValue);
//       Send(FieldValue);
//     finally FreeAndNil(FieldValue) end;
//   end else
//   begin
//     ValueFromIsClause := false;
//     AssignItems(AValue);
//     Changed;
//   end;
// end;

function TMFNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FItems);
end;

{ TMFNode.TEnumerator ------------------------------------------------- }

function TMFNode.TEnumerator.GetCurrent: TX3DNode;
begin
  Result := FList[FPosition];
end;

constructor TMFNode.TEnumerator.Create(const AList: TX3DNodeList);
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TMFNode.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{$endif read_implementation}
