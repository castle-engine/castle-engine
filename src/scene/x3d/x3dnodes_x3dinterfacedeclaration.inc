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

  { Interface declaration, used in VRML/X3D (exposed) prototypes and
    for nodes with dynamic fields (Script, ComposedShader).
    See VRML 2.0 and X3D specs.

    Each interface specification is a field or an event, stored
    in FieldOrEvent. FieldOrEvent is @nil before parsing.

    Field value is not initialized if you passed FieldValue = @false
    to @link(Parse) (although IsClauseNames will
    always be initialized). FieldValue = @true is used for prototype
    (not external) declarations and nodes with interface declarations
    (Script, ComposedShader etc.).
    In the future maybe some property like
    FieldValueInitialized will be exposed here, if needed at some point.

    Interface declaration doesn't have much properties, since all
    the information is contained within FieldOrEvent
    instance, like Name, field class type, out or in (in case of event),
    exposed or not (in case of field), IsClauseNames. }
  TX3DInterfaceDeclaration = class(TX3DFileItem)
  strict private
    FParentNode: TX3DNode;
    FFieldOrEvent: TX3DFieldOrEvent;

    { kept in synch with FFieldOrEvent by SetFieldOrEvent }
    FField: TX3DField;
    FEvent: TX3DEvent;

    procedure SetFieldOrEvent(const Value: TX3DFieldOrEvent);
  private
    { Create a copy of current FieldOrEvent.
      Sets NewParentNode as Result.ParentNode.
      Note the new copy will not have ParentIntefaceDeclaration set
      (as the idea is that you own created copy, not this TX3DInterfaceDeclaration
      instance).

      If CopyState is non-nil, then any nodes referenced inside SFNode / MFNode
      fields and events are copied using DeepCopy (not by simply copying references). }
    function CopyFieldOrEvent(const NewParentNode: TX3DNode;
      const CopyState: TX3DNodeDeepCopyState): TX3DFieldOrEvent;

    { Create a copy of current FieldOrEvent, and add it to Node.Fields
      or Node.Events. }
    procedure CopyAndAddFieldOrEvent(const Node: TX3DNode);
  public
    constructor Create(AParentNode: TX3DNode);
    destructor Destroy; override;

    { Containing node, if any, for this VRML interface declaration.
      This must also be set to FieldOrEvent.ParentNode created for this
      interface declaration. }
    property ParentNode: TX3DNode read FParentNode;

    { Field or event of this interface declaration.
      Is non-nil after parsing.

      You can assign to this property, to constructs interface
      declarations (and so also prototypes) in your own code
      (e.g. this is used X3D XML reader). Just be careful, and remember
      that this object owns FieldOrEvent (that is, will free it
      at destruction). }
    property FieldOrEvent: TX3DFieldOrEvent
      read FFieldOrEvent write SetFieldOrEvent;

    { Copies only reference to FieldOrEvent, adding it to Node.Fields
      or Node.Events. }
    procedure AddFieldOrEvent(Node: TX3DNode);

    { Return FieldOrEvent casted as appropriate class.
      @nil if such cast is not possible, for example when
      FieldOrEvent is an event and you try to use Field method.
      @groupBegin }
    property Field: TX3DField read FField;
    property Event: TX3DEvent read FEvent;
    { @groupEnd }

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames;
      FieldValue, IsClauseAllowed: boolean);

    { Parse interface declaration encoded in XML.

      Note that classic VRML parser has here IsClauseAllowed: boolean
      parameter, this was set to @true when parsing InterfaceDeclarations
      of special nodes (Script, ComposedShader etc.), since they could
      have IS clause (at least, as far as I understood the spec).
      But for X3D XML encoding, it's not available, since (AFAI understand
      the X3D XML encoding spec) the <IS> element inside node body may
      point from nodeField to any interface field of this node, including
      InterfaceDeclarations. So ParseISStatement handles this. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames;
      FieldValue: boolean);

    { Save this interface declaration to stream.
      This assumes that it starts at the beginning of the line,
      and at the end always writes NL, so at the end it's also
      at the beginning of some line.

      For XML encoding, IS clauses are not saved here.
      They must be saved by containing node.

      @param(FieldValue If @true then we will always save
        Field value or (if classic encoding) IS clauses to stream,
        along with this interface
        declaration (if this interface declaration has the Field set).
        Otherwise, field's value will not be saved, only IS clauses
        if present.)
    }
    procedure IDeclSaveToStream(Writer: TX3DWriter; FieldValue: boolean);

    { Save this interface declaration to stream.
      @seealso IDeclSaveToStream }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Returns access type, corresponding to current @link(Event)
      and @link(Field) values.

      Result is undefined if both Event
      and Field are @nil (which may happen when it's not initialized
      (e.g. parsed) yet) or when both are non-nil (which should never
      happen). }
    function AccessType: TX3DAccessType;

    function DeepCopy(const NewParentNode: TX3DNode;
      const CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
  end;

  TX3DInterfaceDeclarationList = class({$ifdef FPC}specialize{$endif}
    TObjectList<TX3DInterfaceDeclaration>)
  public
    { Find field or event with given Name.
      @nil if not found. }
    function TryFindName(const Name: string): TX3DFieldOrEvent;

    { Find field with given Name.
      @nil if not found. }
    function TryFindFieldName(const Name: string): TX3DField;

    { Find event with given Name.
      @nil if not found. }
    function TryFindEventName(const Name: string): TX3DEvent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DInterfaceDeclaration -------------------------------------------------- }

constructor TX3DInterfaceDeclaration.Create(AParentNode: TX3DNode);
begin
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TX3DInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FFieldOrEvent);
  FField := nil;
  FEvent := nil;

  inherited;
end;

procedure TX3DInterfaceDeclaration.SetFieldOrEvent(
  const Value: TX3DFieldOrEvent);
begin
  FFieldOrEvent := Value;

  { set FField and FEvent, for fast access to them }
  if FFieldOrEvent = nil then
  begin
    FField := nil;
    FEvent := nil;
  end else
  if FFieldOrEvent is TX3DField then
  begin
    FField := TX3DField(FFieldOrEvent);
    FEvent := nil;
  end else
  begin
    Assert(FFieldOrEvent is TX3DEvent);
    FField := nil;
    FEvent := TX3DEvent(FFieldOrEvent);
  end;
end;

procedure TX3DInterfaceDeclaration.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames;
  FieldValue, IsClauseAllowed: boolean);
var
  X3DType: string;
  Access: TX3DAccessType;
  FieldType: TX3DFieldClass;
  ParsedName: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  if Lexer.Token = vtKeyword then
  begin
    case Lexer.TokenKeyword of
      vkEventIn, vkInputOnly: Access := atInputOnly;
      vkEventOut, vkOutputOnly: Access := atOutputOnly;
      vkField, vkInitializeOnly: Access := atInitializeOnly;
      vkExposedField, vkInputOutput: Access := atInputOutput;
      else raise EX3DParserError.Create(
        Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));
    end;
  end else
    raise EX3DParserError.Create(
      Lexer, Format(SExpectedInterfaceDeclaration, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'field type (for interface declaration)');
  X3DType := Lexer.TokenName;
  FieldType := X3DFieldsManager.X3DTypeToClass(X3DType);
  if FieldType = nil then
    raise EX3DParserError.Create(
      Lexer, Format(SExpectedFieldType, [Lexer.DescribeToken]));

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'name (for interface declaration)');
  ParsedName := Lexer.TokenName;

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := FieldType.CreateEvent(ParentNode, ParsedName, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      FieldOrEvent := FieldType.CreateUndefined(ParentNode, Access = atInputOutput, ParsedName);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('Access ? in TX3DInterfaceDeclaration.Parse');
    {$endif}
  end;

  Lexer.NextToken;

  if Event <> nil then
  begin
    if IsClauseAllowed then
      Event.Parse(Lexer);
  end else
  begin
    if FieldValue then
    begin
      if Field is TSFNode then
        TSFNode(Field).AutomaticWeakLink := true;
      Field.Parse(Lexer, Reader, IsClauseAllowed)
    end else
    if IsClauseAllowed then
      Field.ParseIsClause(Lexer);
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

procedure TX3DInterfaceDeclaration.ParseXML(
  Element: TDOMElement; Reader: TX3DReaderNames; FieldValue: boolean);
var
  Access: TX3DAccessType;
  AccessIndex: Integer;
  AccessName: string;
  FieldX3DType: string;
  FieldType: TX3DFieldClass;
  ParsedName, FieldActualValue: string;
begin
  { clear instance before parsing }
  FieldOrEvent.Free;
  FieldOrEvent := nil;

  { calculate Access }
  if Element.AttributeString('accessType', AccessName) then
  begin
    AccessIndex := ArrayPosStr(AccessName,
      ['inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput']);
    if AccessIndex <> -1 then
      Access := TX3DAccessType(AccessIndex) else
      raise EX3DXmlError.CreateFmt('Access type "%s" unknown', [AccessName]);
  end else
    raise EX3DXmlError.Create('Missing access type in X3D interface declaration');

  { calculate FieldType }
  if Element.AttributeString('type', FieldX3DType) then
  begin
    FieldType := X3DFieldsManager.X3DTypeToClass(FieldX3DType);
    if FieldType = nil then
      raise EX3DXmlError.CreateFmt('Field type "%s" unknown', [FieldX3DType]);
  end else
    raise EX3DXmlError.Create('Missing field type in X3D interface declaration');

  if not Element.AttributeString('name', ParsedName) then
    raise EX3DXmlError.Create('Missing name in X3D interface declaration');

  { we know everything now to create Event/Field instance }
  case Access of
    atInputOnly, atOutputOnly:
      FieldOrEvent := FieldType.CreateEvent(ParentNode, ParsedName, Access = atInputOnly);
    atInitializeOnly, atInputOutput:
      FieldOrEvent := FieldType.CreateUndefined(ParentNode, Access = atInputOutput, ParsedName);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('AccessType ?');
    {$endif}
  end;

  if Event <> nil then
  begin
    { Classic VRML parser has here
        if IsClauseAllowed then Event.Parse(Lexer);
      but for X3D XML encoding this is not needed, see comments above. }
  end else
  begin
    if FieldValue then
    begin
      if Field is TSFNode then
        TSFNode(Field).AutomaticWeakLink := true;

      if Element.AttributeString('value', FieldActualValue) then
        Field.ParseXMLAttribute(FieldActualValue, Reader)
      else
        Field.ParseXMLElement(Element, Reader);
    end;

    { Classic VRML parser has here
        else if IsClauseAllowed then Field.ParseIsClause(Lexer);
      but for X3D XML encoding this is not needed, see comments above. }
  end;

  FieldOrEvent.ParentInterfaceDeclaration := Self;
end;

function TX3DInterfaceDeclaration.CopyFieldOrEvent(
  const NewParentNode: TX3DNode;
  const CopyState: TX3DNodeDeepCopyState): TX3DFieldOrEvent;
var
  F: TX3DField absolute Result;
  E: TX3DEvent absolute Result;
begin
  if Field <> nil then
  begin
    { F := copy of Field }
    F := TX3DFieldClass(Field.ClassType).CreateUndefined(NewParentNode,
      Field.Exposed, Field.X3DName);

    if CopyState <> nil then
      F.InternalAssignDeepCopy(Field, CopyState)
    else
      F.Assign(Field);

    { CreateUndefined creates field without any default value,
      so it will always get saved later to file.

      But this is not nice: for non-node fields, it merely makes
      resulting file longer. For node fields (SFNode and MFNode)
      this means that node value will be written to file. But this
      is bad, since this means that node contents will have to duplicated,
      if node is not named or it's name is unbound now (e.g. overridden
      by other node name) (otherwise "USE Xxx" could be used, which
      is acceptable).

      See
      ../../../../demo-models/prototypes/proto_sfnode_default.x3dv
      ../../../../demo-models/x3d/tricky_def_use.x3dv
      for  examples (open and save them back e.g. in view3dscene).

      So to make it work right, we have to set DefaultValue for our
      fields, in particular for TSFNode and TMFNode fields.
      So that EqualsDefaultValue will work Ok when saving to file. }
    F.AssignDefaultValueFromValue;
  end else
  if Event <> nil then
  begin
    { E := copy of Event }
    E := Event.FieldClass.CreateEvent(NewParentNode, Event.X3DName, Event.InEvent);
    { Although above constructor already copied most event properties,
      some were omitted (like IsClauseNames --- important for Script with
      eventIn/out events with IS clauses inside prototypes).
      Assign call below takes care of them. }
    if CopyState <> nil then
      E.InternalAssignDeepCopy(Event, CopyState) // note: for events actually now InternalAssignDeepCopy = Assign
    else
      E.Assign(Event);
  end else
    raise EInternalError.Create('interface declaration but no Field or Event');

  Result.ParentInterfaceDeclaration := nil;
end;

procedure TX3DInterfaceDeclaration.AddFieldOrEvent(
  Node: TX3DNode);
begin
  if Field <> nil then
    Node.AddField(Field) else
  begin
    Assert(Event <> nil);
    Node.AddEvent(Event);
  end;
end;

procedure TX3DInterfaceDeclaration.CopyAndAddFieldOrEvent(
  const Node: TX3DNode);
var
  Copy: TX3DFieldOrEvent;
begin
  Copy := CopyFieldOrEvent(Node, nil);
  if Copy is TX3DField then
    Node.AddField(TX3DField(Copy)) else
  begin
    Assert(Copy is TX3DEvent);
    Node.AddEvent(TX3DEvent(Copy));
  end;
end;

procedure TX3DInterfaceDeclaration.IDeclSaveToStream(
  Writer: TX3DWriter; FieldValue: boolean);

  function ATName(const AccessType: TX3DAccessType): string;
  const
    Names: array
      [ boolean { is it X3D or XML encoding ? },
        TX3DAccessType] of string =
      ( ('eventIn', 'eventOut', 'field', 'exposedField'),
        ('inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput') );
  begin
    Result := Names[(Writer.Encoding = xeXML) or (Writer.Version.Major >= 3),
      AccessType];
  end;

var
  N: string;
begin
  N := FieldOrEvent.NameForVersion(Writer.Version);

  case Writer.Encoding of
    xeClassic:
      if Event <> nil then
      begin
        if Event.InEvent then
          Writer.WriteIndent(ATName(atInputOnly) + ' ') else
          Writer.WriteIndent(ATName(atOutputOnly) + ' ');
        Writer.Write(Event.FieldClass.X3DType + ' ');
        if Event.IsClauseNamesCount <> 0 then
        begin
          Writer.DiscardNextIndent;
          { Note that there may be many IS clauses. This will still work Ok:
            first IS clause will "belong" to this interface declaration,
            the rest will look like normal IS clauses. }
          Event.SaveToStreamClassicIsClauses(Writer);
        end else
          Writer.Writeln(N);
      end else
      begin
        if Field.Exposed then
          Writer.WriteIndent(ATName(atInputOutput) + ' ') else
          Writer.WriteIndent(ATName(atInitializeOnly) + ' ');
        Writer.Write(Field.X3DType + ' ');

        { When saving from interface declaration, you can only
          1. write sole field name
          2. write field name + value (if FieldValue = @true)
          3. write field name + IS clause (only one allowed) }

        if ( FieldValue and
             (not Field.ValueFromIsClause) and
             (Field.IsClauseNamesCount = 0) ) then
        begin
          { Field.SaveToStream normally starts from new line with an indent...
            In this case, we want it to start on the same line, so indent must
            be discarded. }
          Writer.DiscardNextIndent;
          Field.FieldSaveToStream(Writer, true);
          { In this case, Writer.Writeln will be done by Field.SaveToStream.
            (we pass SaveWhenDefault anyway, so we can be sure that
            this newline will be done). }
        end else

        if Field.IsClauseNamesCount <> 0 then
        begin
          Writer.DiscardNextIndent;
          { Note that there may be many IS clauses. This will still work Ok:
            first IS clause will "belong" to this interface declaration,
            the rest will look like normal IS clauses. }
          Field.SaveToStreamClassicIsClauses(Writer);
        end else

        begin
          Writer.Writeln(N);
        end;
      end;
    xeXML:
      { We don't save IS clauses here for XML encoding. They must be saved
        inside containing TX3DNode. }
      if Event <> nil then
      begin
        Writer.WritelnIndent(Format('<field accessType=%s type=%s name=%s />',
          [ Iff(Event.InEvent,
              StringToX3DXml(ATName(atInputOnly)),
              StringToX3DXml(ATName(atOutputOnly))),
            StringToX3DXml(Event.FieldClass.X3DType),
            StringToX3DXml(N) ]));
      end else
      begin
        Writer.WriteIndent(Format('<field accessType=%s type=%s name=%s',
          [ Iff(Field.Exposed,
              StringToX3DXml(ATName(atInputOutput)),
              StringToX3DXml(ATName(atInitializeOnly))),
            StringToX3DXml(Field.X3DType),
            StringToX3DXml(N) ]));

        if ( FieldValue and
             (not Field.ValueFromIsClause) and
             (Field.IsClauseNamesCount = 0) ) then
        begin
          if Field.SaveToXml in [sxAttribute, sxAttributeCustomQuotes] then
          begin
            Writer.Write(' value=');
            Field.FieldSaveToStream(Writer, true, true);
            Writer.Writeln(' />');
          end else
          begin
            Writer.Writeln('>');
            { Parameter XmlAvoidSavingNameBeforeValue doesn't matter here }
            Field.FieldSaveToStream(Writer, true);
            Writer.WritelnIndent('</field>');
          end;
        end else
          { no field value, no IS clauses.
            This can happen for field/event declaration inside <ExternProto>.
            Just close <field> element. }
          Writer.Writeln(' />');
      end;

    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DInterfaceDeclaration.IDeclSaveToStream Encoding?');
    {$endif}
  end;
end;

procedure TX3DInterfaceDeclaration.SaveToStream(Writer: TX3DWriter);
begin
  IDeclSaveToStream(Writer, true);
end;

function TX3DInterfaceDeclaration.AccessType: TX3DAccessType;
begin
  if Event <> nil then
  begin
    if Event.InEvent then
      Result := atInputOnly else
      Result := atOutputOnly;
  end else
  if Field <> nil then
  begin
    if Field.Exposed then
      Result := atInputOutput else
      Result := atInitializeOnly;
  end else
    { Result is undefined in this case, but we don't want to signal any error }
    Result := atInitializeOnly
end;

function TX3DInterfaceDeclaration.DeepCopy(
  const NewParentNode: TX3DNode;
  const CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
begin
  Result := TX3DInterfaceDeclaration.Create(NewParentNode);
  Result.FieldOrEvent := CopyFieldOrEvent(NewParentNode, CopyState);
  Result.FieldOrEvent.ParentInterfaceDeclaration := Result;
end;

{ TX3DInterfaceDeclarationList --------------------------------------------- }

function TX3DInterfaceDeclarationList.TryFindName(
  const Name: string): TX3DFieldOrEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FieldOrEvent;
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindFieldName(const Name: string): TX3DField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DField then
    begin
      Result := Items[I].Field;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindEventName(const Name: string): TX3DEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DEvent then
    begin
      Result := Items[I].Event;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

{$endif read_implementation}
