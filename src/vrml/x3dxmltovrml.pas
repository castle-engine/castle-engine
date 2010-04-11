{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Read X3D encoded in XML. }
unit X3DXmlToVRML;

{$I kambiconf.inc}

interface

uses VRMLNodes, Classes;

{ Read X3D encoded in XML, and convert it to VRML/X3D nodes graph.

  @param(PrototypeNames If <> @nil, will be filled with global
  prototype namespace at the end of parsing the file.
  Useful mostly for EXTERNPROTO implementation.) }
function LoadX3DXmlAsVRML(const FileName: string;
  Gzipped: boolean;
  PrototypeNames: TVRMLPrototypeNames = nil): TVRMLNode;

implementation

uses SysUtils, DOM, KambiXMLRead, KambiUtils, KambiXMLUtils,
  VRMLLexer, VRMLErrors, VRMLFields, KambiZStream,
  KambiClassUtils, KambiStringUtils;

type
  EX3DXmlError = class(EVRMLError);
  EX3DXmlNotAllowedError = class(EX3DXmlError);
  EX3DXmlUnknownNodeNotAllowed = class(EX3DXmlError);

{ TODO: X3D IMPORT/EXPORT is not handled yet, and it's not even parsed
  by this unit.
  See 4.3.12 IMPORT/EXPORT statement syntax. }

function LoadX3DXmlAsVRML(const FileName: string;
  Gzipped: boolean; PrototypeNames: TVRMLPrototypeNames): TVRMLNode;
var
  WWWBasePath: string;

  { X3D version numbers. }
  VRMLVerMajor: Integer;
  VRMLVerMinor: Integer;

  { TODO: each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
  Names: TVRMLNames;

const
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';
  SNull = 'NULL';

  function ParseNode(Element: TDOMElement;
    out ContainerField: string;
    NilIfUnresolvedUSE: boolean): TVRMLNode; forward;
  function ParseVRMLStatements(Element: TDOMElement;
    ParseX3DHeader: boolean;
    X3DHeaderElement: TDOMElement): TVRMLNode; forward;
  procedure ParsePrototype(Proto: TVRMLPrototype; Element: TDOMElement); forward;
  procedure ParseExternalPrototype(Proto: TVRMLExternalPrototype;
    Element: TDOMElement); forward;
  procedure ParseInterfaceDeclaration(
    I: TVRMLInterfaceDeclaration; Element: TDOMElement;
    FieldValue: boolean); forward;

  { Checks is Element a correct <connect> element, extracting
    nodeField and protoField value. Returns @true if all Ok, otherwise
    returns @false. }
  function ParseConnectElement(Element: TDOMElement;
    out NodeField, ProtoField: string): boolean;
  begin
    Result := false;

    if Element.TagName <> 'connect' then
    begin
      VRMLWarning(vwSerious, 'Only <connect> elements are allowed inside <IS> element');
      Exit;
    end;

    if not DOMGetAttribute(Element, 'nodeField', NodeField) then
    begin
      VRMLWarning(vwSerious, 'Missing "nodeField" inside <connect> element');
      Exit;
    end;

    if not DOMGetAttribute(Element, 'protoField', ProtoField) then
    begin
      VRMLWarning(vwSerious, 'Missing "protoField" inside <connect> element');
      Exit;
    end;

    Result := true;
  end;

  { Look only inside Element children, to read SFNode / MFNode field value. }
  procedure ParseFieldValueFromElement(Field: TVRMLField; Element: TDOMElement);
  var
    Child: TVRMLNode;
    SF: TSFNode;
    MF: TMFNode;
    I: TXMLElementIterator;
    ContainerFieldDummy: string;
  begin
    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        Child := ParseNode(I.Current,
          ContainerFieldDummy { ignore containerField }, true);
        if Child <> nil then
        begin
          if Field is TSFNode then
          begin
            SF := Field as TSFNode;
            SF.Value := Child;
            SF.WarningIfChildNotAllowed(Child);
          end else
          if Field is TMFNode then
          begin
            MF := Field as TMFNode;
            MF.AddItem(Child);
            MF.WarningIfChildNotAllowed(Child);
          end else
          begin
            Child.FreeIfUnused;
            Child := nil;
            VRMLWarning(vwSerious, 'X3D field "' + Field.Name + '" is not SFNode or MFNode, but a node value (XML element) is specified');
          end;
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  procedure ParseISStatement(Node: TVRMLNode; ISElement: TDOMElement;
    var PositionInParent: Integer);
  var
    I: TXMLElementIterator;
    NodeField, ProtoField: string;
    NodeFieldOrEvent: TVRMLFieldOrEvent;
  begin
    I := TXMLElementIterator.Create(ISElement);
    try
      while I.GetNext do
        if ParseConnectElement(I.Current, NodeField, ProtoField) then
        begin
          NodeFieldOrEvent := Node.FieldOrEvent(NodeField);
          if NodeFieldOrEvent <> nil then
          begin
            NodeFieldOrEvent.IsClauseNames.Add(ProtoField);
            NodeFieldOrEvent.PositionInParent := PositionInParent;
            Inc(PositionInParent);
          end else
            VRMLWarning(vwSerious, Format('<connect> element "nodeField" doesn''t indicate any known field/event name: "%s"', [NodeField]));
        end;
    finally FreeAndNil(I) end;
  end;

  { Parse node body, i.e. mainly node's fields.
    This is roughly equivalent to TVRMLNode.Parse in classic VRML encoding
    parser. }
  procedure ParseNodeBody(Node: TVRMLNode;
    Element: TDOMElement);
  var
    PositionInParent: Integer;

    procedure ParseXMLAttributes;
    var
      Attr: TDOMAttr;
      AttrNode: TDOMNode;
      AttrIndex, Index: Integer;
    begin
      { enumerate over all attributes }
      for AttrIndex := 0 to Element.Attributes.Length - 1 do
      begin
        AttrNode := Element.Attributes[AttrIndex];
        Assert(AttrNode.NodeType = ATTRIBUTE_NODE);
        Attr := AttrNode as TDOMAttr;

        { containerField and DEF attributes are handled in ParseNode,
          we can safely ignore them now. }
        if (Attr.Name = SAttrContainerField) or
           (Attr.Name = SAttrDEF) then
          Continue;

        Index := Node.Fields.IndexOf(Attr.Name);
        if Index >= 0 then
        begin
          Node.Fields[Index].ParseXMLAttribute(Attr.Value, Names);
          Node.Fields[Index].PositionInParent := PositionInParent;
          Inc(PositionInParent);
        end else
          VRMLWarning(vwSerious, 'Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.Name + '" in node "' + Node.NodeTypeName + '"');
      end;
    end;

    procedure ParseXMLChildrenNodes;
    var
      FieldIndex: Integer;
      Child: TVRMLNode;
      ContainerField: string;
      SF: TSFNode;
      MF: TMFNode;
      Route: TVRMLRoute;
      I: TXMLElementIterator;
      Proto: TVRMLPrototype;
      ExternProto: TVRMLExternalPrototype;
      IDecl: TVRMLInterfaceDeclaration;
      Import: TVRMLImport;
      ExportItem: TVRMLExport;
    begin
      I := TXMLElementIterator.Create(Element);
      try
        while I.GetNext do
        begin
          if I.Current.TagName = 'ROUTE' then
          begin
            Route := TVRMLRoute.Create;
            Route.PositionInParent := PositionInParent;
            Node.Routes.Add(Route);
            Route.ParseXML(I.Current, Names);
          end else
          if I.Current.TagName = 'IS' then
          begin
            ParseISStatement(Node, I.Current, PositionInParent);
          end else
          if I.Current.TagName = 'IMPORT' then
          begin
            Import := TVRMLImport.Create;
            Import.PositionInParent := PositionInParent;
            Node.ImportsList.Add(Import);
            Import.ParseXML(I.Current, Names);
          end else
          if I.Current.TagName = 'EXPORT' then
          begin
            ExportItem := TVRMLExport.Create;
            ExportItem.PositionInParent := PositionInParent;
            Node.ExportsList.Add(ExportItem);
            ExportItem.ParseXML(I.Current, Names);
          end else
          if I.Current.TagName = 'ProtoDeclare' then
          begin
            Proto := TVRMLPrototype.Create;
            Proto.PositionInParent := PositionInParent;
            Node.Prototypes.Add(Proto);
            ParsePrototype(Proto, I.Current);
          end else
          if I.Current.TagName = 'ExternProtoDeclare' then
          begin
            ExternProto := TVRMLExternalPrototype.Create;
            ExternProto.PositionInParent := PositionInParent;
            Node.Prototypes.Add(ExternProto);
            ParseExternalPrototype(ExternProto, I.Current);
          end else
          if I.Current.TagName = 'field' then
          begin
            IDecl := TVRMLInterfaceDeclaration.Create(Node);
            try
              ParseInterfaceDeclaration(IDecl, I.Current, true);
              IDecl.PositionInParent := PositionInParent;
              if IDecl.AccessType in Node.HasInterfaceDeclarations then
              begin
                Node.InterfaceDeclarations.Add(IDecl);
                Node.PostAddInterfaceDeclaration(IDecl);
              end else
              begin
                FreeAndNil(IDecl);
                VRMLWarning(vwSerious, 'X3D XML: specified <field> inside node, but this node doesn''t allow interface declaration with such accessType');
              end;
            except
              FreeAndNil(IDecl);
              raise;
            end;
          end else
          begin
            Child := ParseNode(I.Current, ContainerField, true);
            if Child <> nil then
            begin
              Child.PositionInParent := PositionInParent;
              FieldIndex := Node.Fields.IndexOf(ContainerField);

              if (FieldIndex = -1) and
                 (ContainerField <> Child.DefaultContainerField) and
                 (Child.DefaultContainerField <> '') then
              begin
                { Retry with DefaultContainerField value, since it exists
                  and is different than current ContainerField. }
                FieldIndex := Node.Fields.IndexOf(Child.DefaultContainerField);
                if FieldIndex >= 0 then
                  VRMLWarning(vwSerious, 'X3D XML: containerField indicated unknown field name ("' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + Node.NodeTypeName + '"), using the default containerField value "' + Child.DefaultContainerField + '" succeded');
              end;

              if FieldIndex >= 0 then
              begin
                if Node.Fields[FieldIndex] is TSFNode then
                begin
                  SF := Node.Fields[FieldIndex] as TSFNode;
                  { Although field doesn't have a set position in XML X3D
                    encoding, when saving later in classic encoding we
                    need some order of fields. This is yet another problem
                    with non-unique names, something defined in XML X3D
                    may be not possible to save in other encoding:

                    <Group>
                      <Shape> ... <Appearance DEF="XXX" ....> </Shape>
                      <ROUTE ... using "XXX" name ...>
                      <Shape> ... <Appearance DEF="XXX" ....> </Shape>
                      <ROUTE ... using "XXX" name ...>
                    </Group>

                    This is uneasy to save in classic encoding, since
                    you cannot insert ROUTE in the middle of "children"
                    field of Group node in classic encoding.
                  }
                  Node.Fields[FieldIndex].PositionInParent := PositionInParent;
                  SF.Value := Child;
                  SF.WarningIfChildNotAllowed(Child);
                end else
                if Node.Fields[FieldIndex] is TMFNode then
                begin
                  MF := Node.Fields[FieldIndex] as TMFNode;
                  Node.Fields[FieldIndex].PositionInParent := PositionInParent;
                  MF.AddItem(Child);
                  MF.WarningIfChildNotAllowed(Child);
                end else
                begin
                  Child.FreeIfUnused;
                  Child := nil;
                  VRMLWarning(vwSerious, 'X3D field "' + ContainerField + '" is not SFNode or MFNode, but a node value (XML element) is specified');
                end;
              end else
              begin
                try
                  VRMLWarning(vwSerious, 'Unknown X3D field name (indicated by containerField value) "' + ContainerField + '" by node "' + Child.NodeTypeName + '" inside node "' + Node.NodeTypeName + '"');
                finally
                  Child.FreeIfUnused;
                  Child := nil;
                end;
              end;
            end;
          end;
          Inc(PositionInParent);
        end;
      finally FreeAndNil(I) end;
    end;

    procedure ParseXMLCdata;
    var
      I: TXMLCDataIterator;
    begin
      Node.CDataExists := false;
      Node.CData := '';

      I := TXMLCDataIterator.Create(Element);
      try
        if I.GetNext then
        begin
          Node.CDataExists := true;
          if not Node.CDataAllowed then
            VRMLWarning(vwSerious, Format('VRML / X3D node %s doesn''t allow CDATA section, but it''s specified',
              [Node.NodeTypeName]));
          { append all CData sections to Node.CData }
          repeat
            Node.CData := Node.CData + I.Current;
          until not I.GetNext;
        end;
      finally FreeAndNil(I) end;
    end;

  begin
    PositionInParent := 0;
    { The order below is important: first parse XML attributes,
      then elements, since VRML DEF mechanism says that DEF order
      is significant. }
    ParseXMLAttributes;
    ParseXMLChildrenNodes;
    ParseXMLCdata;
  end;

  (*
    Parse VRML node. This parses normal node (with optional DEF),
    or node with USE attribute.

    It's somewhat similar to classic VRML ParseNode.
    (Admittedly, it was even implemented by copying and modifying
    classic ParseNode :) ).

    If we will find USE clause but node name will be unknown, the normal
    behavior (when NilIfUnresolvedUSE = @false, default) is to raise
    EX3DXmlNotAllowedError (just like in case of many other errors).
    However, this is a particular parsing error, because we can probably
    pretty safely continue parsing, ignoring this error.
    So if you pass NilIfUnresolvedUSE = @true, this function will do
    VRMLWarning and simply return @nil.

    @raises(EX3DXmlNotAllowedError On various not-allowed errors.)

    @raises(EX3DXmlUnknownNodeNotAllowed On a special parsing error:
      we got unknown node name, and AllowedNodes was @false.

      We have a special error class for this, because in some cases
      it means that actually the unknown node name could be also
      unknown field / proto etc. name, so error message for the user should
      be better.)
  *)

  function ParseNode(Element: TDOMElement;
    out ContainerField: string;
    NilIfUnresolvedUSE: boolean): TVRMLNode;

    procedure ParseNamedNode(const NodeName: string);
    var
      NodeClass: TVRMLNodeClass;
      NodeTypeName: string;
      ProtoName: string;
      Proto: TVRMLPrototypeBase;
      ProtoIter: TXMLElementIterator;
      FieldActualValue, FieldName: string;
      FieldIndex: Integer;
      ExplicitContainerField: string;
      PositionInParent: Integer;
    begin
      NodeTypeName := Element.TagName;

      if NodeTypeName = 'ProtoInstance' then
      begin
        if not DOMGetAttribute(Element, 'name', ProtoName) then
          raise EX3DXmlError.Create('<ProtoInstance> doesn''t specify "name" of the prototype');

        Proto := Names.Prototypes.Bound(ProtoName);
        if Proto = nil then
          raise EX3DXmlError.CreateFmt('<ProtoInstance> specifies unknown prototype name "%s"', [ProtoName]);

        if (Proto is TVRMLExternalPrototype) and
           (TVRMLExternalPrototype(Proto).ReferencedClass <> nil) then
          Result := TVRMLExternalPrototype(Proto).ReferencedClass.Create(NodeName, WWWBasePath) else
          Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, WWWBasePath, Proto);

        { parse field values from <fieldValue> elements }
        ProtoIter := TXMLElementIterator.Create(Element);
        try
          PositionInParent := 0;

          while ProtoIter.GetNext do
          begin
            if ProtoIter.Current.TagName = 'fieldValue' then
            begin
              if not DOMGetAttribute(ProtoIter.Current, 'name', FieldName) then
              begin
                VRMLWarning(vwSerious, 'X3D XML: missing "name" attribute for <fieldValue> element');
                Continue;
              end;

              FieldIndex := Result.Fields.IndexOf(FieldName);
              if FieldIndex = -1 then
              begin
                VRMLWarning(vwSerious, Format('X3D XML: <fieldValue> element references unknown field name "%s"', [FieldName]));
                Continue;
              end;

              if DOMGetAttribute(ProtoIter.Current, 'value', FieldActualValue) then
                Result.Fields[FieldIndex].ParseXMLAttribute(FieldActualValue, Names) else
                ParseFieldValueFromElement(Result.Fields[FieldIndex], ProtoIter.Current);

              Result.Fields[FieldIndex].PositionInParent := PositionInParent;
            end else
            if ProtoIter.Current.TagName = 'IS' then
            begin
              ParseISStatement(Result, ProtoIter.Current, PositionInParent);
            end else
            begin
              VRMLWarning(vwSerious, Format('X3D XML: only <fieldValue> or <IS> elements expected in prototype instantiation, but "%s" found', [ProtoIter.Current.TagName]));
            end;

            Inc(PositionInParent);
          end;
        finally FreeAndNil(ProtoIter) end;

        { If it was normal (non-external) prototype, then instantiate
          it now (this sort-of expands prototype "macro" in place). }
        if Result is TVRMLPrototypeNode then
        try
          Result := TVRMLPrototypeNode(Result).Instantiate;
        except
          on E: EVRMLPrototypeInstantiateError do
            { Just write E.Message and silence the exception.
              Result will simply remain as TVRMLPrototypeNode instance in this case. }
            VRMLWarning(vwSerious, E.Message);
        end;
      end else
      begin
        NodeClass := NodesManager.NodeTypeNameToClass(NodeTypeName,
          VRMLVerMajor, VRMLVerMinor);
        if NodeClass <> nil then
        begin
          Result := NodeClass.Create(NodeName, WWWBasePath);
          ParseNodeBody(Result, Element);
        end else
        begin
          Result := TVRMLUnknownNode.CreateUnknown(NodeName, WWWBasePath, NodeTypeName);

          { In classic VRML parser, we had special TVRMLUnknownNode.Parse
            that performed the "magic" trick of
            ParseIgnoreToMatchingCurlyBracket. This is not needed for
            X3D XML, we can simply omit the node by not looking
            at it's attributes. All we need to do is to make
            VRMLWarning warning. }

          VRMLWarning(vwSerious, 'Unknown X3D node type "' + NodeTypeName + '"');
        end;
      end;

      { TODO: this has a problem, see classic VRML ParseNode
        comment starting with "Cycles in VRML graph are bad..." }

      Names.Nodes.Bind(Result);

      if DOMGetAttribute(Element, SAttrContainerField, ExplicitContainerField) then
        Result.ExplicitContainerField := ExplicitContainerField;
    end;

  var
    NodeName, S: string;
  begin
    Result := nil;
    try
      if DOMGetAttribute(Element, 'USE', NodeName) then
      begin
        { get appropriate node }
        Result := Names.Nodes.Bound(NodeName);
        if Result = nil then
        begin
          S := Format('Incorrect USE element: node name "%s" undefined',
            [NodeName]);
          if NilIfUnresolvedUSE then
            VRMLWarning(vwSerious, S) else
            raise EX3DXmlNotAllowedError.Create(S);
        end;
      end else
      begin
        if DOMGetAttribute(Element, SAttrDEF, NodeName) then
          ParseNamedNode(NodeName) else
          ParseNamedNode('');
      end;

      { calculate ContainerField.

        Note that we do not diffentiate here between the case of <USE>
        element and real node element --- because that's the intention
        of X3D specification, in both situations element may have
        containerField attribute.

        Also note that we take into account both
        DefaultContainerField and ExplicitContainerField.
        ExplicitContainerField is needed --- imagine a node with DEF
        has explicit "containerField" attribute, then this takes precedence
        over implicit DefaultContainerField, and has to be stored
        in Result instance.

        It can be overriden at each USE of this node. }
      if Result <> nil then
      begin
        if Result.ExplicitContainerField <> '' then
          ContainerField := Result.ExplicitContainerField else
          ContainerField := Result.DefaultContainerField;
      end;
      DOMGetAttribute(Element, SAttrContainerField, ContainerField);

    except FreeAndNil(Result); raise end;
  end;

  { This is equivalent to TVRMLInterfaceDeclaration.Parse
    in classic VRML parser.

    Note that in classic VRML parser we had here IsClauseAllowed: boolean
    parameter, this was set to @true when parsing InterfaceDeclarations
    of special nodes (Script, ComposedShader etc.), since they could
    have IS clause (at least, as far as I understood the spec).
    But for X3D XML encoding, it's not available, since (AFAI understand
    the X3D XML encoding spec) the <IS> element inside node body may
    point from nodeField to any interface field of this node, including
    InterfaceDeclarations. So ParseISStatement handles this. }
  procedure ParseInterfaceDeclaration(
    I: TVRMLInterfaceDeclaration; Element: TDOMElement;
    FieldValue: boolean);
  var
    AccessType: TVRMLAccessType;
    AccessTypeIndex: Integer;
    AccessTypeName: string;
    FieldTypeName: string;
    FieldType: TVRMLFieldClass;
    Name, FieldActualValue: string;
  begin
    { clear instance before parsing }
    I.FieldOrEvent.Free;
    I.FieldOrEvent := nil;

    { calculate AccessType }
    if DOMGetAttribute(Element, 'accessType', AccessTypeName) then
    begin
      AccessTypeIndex := ArrayPosStr(AccessTypeName,
        ['inputOnly', 'outputOnly', 'initializeOnly', 'inputOutput']);
      if AccessTypeIndex <> -1 then
        AccessType := TVRMLAccessType(AccessTypeIndex) else
        raise EX3DXmlError.CreateFmt('Access type "%s" unknown', [AccessTypeName]);
    end else
      raise EX3DXmlError.Create('Missing access type in X3D interface declaration');

    { calculate FieldType }
    if DOMGetAttribute(Element, 'type', FieldTypeName) then
    begin
      FieldType := VRMLFieldsManager.FieldTypeNameToClass(FieldTypeName);
      if FieldType = nil then
        raise EX3DXmlError.CreateFmt('Field type "%s" unknown', [FieldTypeName]);
    end else
      raise EX3DXmlError.Create('Missing field type in X3D interface declaration');

    if not DOMGetAttribute(Element, 'name', Name) then
      raise EX3DXmlError.Create('Missing name in X3D interface declaration');

    { we know everything now to create Event/Field instance }
    case AccessType of
      atInputOnly, atOutputOnly:
        I.FieldOrEvent := TVRMLEvent.Create(I.ParentNode, Name, FieldType, AccessType = atInputOnly);
      atInitializeOnly, atInputOutput:
        begin
          I.FieldOrEvent := FieldType.CreateUndefined(I.ParentNode, Name);
          I.Field.Exposed := AccessType = atInputOutput;
        end;
      else raise EInternalError.Create('AccessType ?');
    end;

    if I.Event <> nil then
    begin
      { Classic VRML parser has here
          if IsClauseAllowed then I.Event.Parse(Lexer);
        but for X3D XML encoding this is not needed, see comments above. }
    end else
    begin
      if FieldValue then
      begin
        if DOMGetAttribute(Element, 'value', FieldActualValue) then
          I.Field.ParseXMLAttribute(FieldActualValue, Names) else
          ParseFieldValueFromElement(I.Field, Element);
      end;

      { Classic VRML parser has here
          else if IsClauseAllowed then I.Field.ParseIsClause(Lexer);
        but for X3D XML encoding this is not needed, see comments above. }
    end;

    I.FieldOrEvent.ParentInterfaceDeclaration := I;
  end;

  { Handle sequence of <field> elements.
    This is equivalent to TVRMLPrototypeBase.ParseInterfaceDeclarations
    in classic VRML parser. }
  procedure ParseInterfaceDeclarations(
    Proto: TVRMLPrototypeBase;
    Element: TDOMElement; ExternalProto: boolean);
  var
    I: TVRMLInterfaceDeclaration;
    Iter: TXMLElementIterator;
  begin
    Iter := TXMLElementIterator.Create(Element);
    try
      while Iter.GetNext do
      begin
        if Iter.Current.TagName = 'field' then
        begin
          I := TVRMLInterfaceDeclaration.Create(nil);
          Proto.InterfaceDeclarations.Add(I);
          ParseInterfaceDeclaration(I, Iter.Current, not ExternalProto);
        end else
          VRMLWarning(vwSerious, 'X3D XML: only <field> elements expected in prototype interface');
      end;
    finally FreeAndNil(Iter) end;
  end;

  { Equivalent to TVRMLPrototype.Parse }
  procedure ParsePrototype(Proto: TVRMLPrototype; Element: TDOMElement);
  var
    OldNames: TVRMLNames;
    Name: string;
    E: TDOMElement;
  begin
    Proto.WWWBasePath := WWWBasePath;

    if DOMGetAttribute(Element, 'name', Name) then
      Proto.Name := Name else
      raise EX3DXmlError.Create('Missing "name" for <ProtoDeclare> element');

    E := DOMGetChildElement(Element, 'ProtoInterface', false);
    if E <> nil then
      ParseInterfaceDeclarations(Proto, E, false);

    E := DOMGetChildElement(Element, 'ProtoBody', false);
    if E = nil then
      raise EX3DXmlError.Create('Missing <ProtoBody> inside <ProtoDeclare> element');

    Proto.Node.Free; Proto.Node := nil;

    { VRML 2.0 spec explicitly says that inside prototype has it's own DEF/USE
      scope, completely independent from the outside.

      Also prototype name scope is local within the prototype,
      however it starts from current prototype name scope (not empty,
      like in case of Names.Nodes). So prototypes defined outside
      are available inside, but nested prototypes inside are not
      available outside. }
    OldNames := Names;
    Names := TVRMLNames.Create(true);
    try
      Names.Prototypes.Assign(OldNames.Prototypes);
      Proto.Node := ParseVRMLStatements(E, false, nil);
    finally
      FreeAndNil(Names);
      Names := OldNames;
    end;

    Names.Prototypes.Bind(Proto);
  end;

  { Equivalent to TVRMLExternalPrototype.Parse }
  procedure ParseExternalPrototype(Proto: TVRMLExternalPrototype;
    Element: TDOMElement);
  var
    Name, URLListValue: string;
  begin
    Proto.WWWBasePath := WWWBasePath;

    if DOMGetAttribute(Element, 'name', Name) then
      Proto.Name := Name else
      raise EX3DXmlError.Create('Missing "name" for <ExternProtoDeclare> element');

    ParseInterfaceDeclarations(Proto, Element, true);

    if DOMGetAttribute(Element, 'url', URLListValue) then
      Proto.URLList.ParseXMLAttribute(URLListValue, Names) else
      raise EX3DXmlError.Create('Missing "url" for <ExternProtoDeclare> element');

    Names.Prototypes.Bind(Proto);

    Proto.LoadReferenced;
  end;

  { This parses a sequence of X3D statements: any number of nodes,
    (external) protypes, routes. This is good to use to parse whole VRML file,
    or a (non-external) prototype content.

    It's somewhat similar to classic VRML ParseVRMLStatements.
    (Admittedly, it was even implemented by copying and modifying
    classic ParseVRMLStatements :) ).

    Returns a single VRML node. If there was exactly one statement
    and it was a node statement, returns this node. Otherwise,
    returns everything read wrapped in artifical TVRMLRootNode_2 instance. }
  function ParseVRMLStatements(Element: TDOMElement;
    ParseX3DHeader: boolean;
    X3DHeaderElement: TDOMElement): TVRMLNode;
  var
    PositionInParent: Integer;

    { Create root group node. }
    function CreateRootNode: TVRMLNode;
    begin
      Result := TVRMLRootNode_2.Create('', WWWBasePath);
      TVRMLRootNode_2(Result).ForceVersion := true;
      TVRMLRootNode_2(Result).ForceVersionMajor := VRMLVerMajor;
      TVRMLRootNode_2(Result).ForceVersionMinor := VRMLVerMinor;
    end;

    procedure ParseProfile;
    var
      Profile: string;
    begin
      { parse "profile" attribute }
      if DOMGetAttribute(X3DHeaderElement, 'profile', Profile) then
      begin
        (Result as TVRMLRootNode_2).X3DProfile := Profile;
      end else
        { We allow PROFILE to be omitted.
          Actually, we do not use profile for anything right now. }
        VRMLWarning(vwSerious, 'X3D "profile" attribute missing');
    end;

    procedure ParseComponentsAndMetas;
    var
      Head: TDOMElement;
      I: TXMLElementIterator;
      MetaName, MetaContent: string;
      ComponentName: string;
      ComponentLevel: Integer;
    begin
      Head := DOMGetChildElement(X3DHeaderElement, 'head', false);
      if Head = nil then Exit;

      I := TXMLElementIterator.Create(Head);
      try
        while I.GetNext do
        begin
          if I.Current.TagName = 'meta' then
          begin
            MetaName := '';
            MetaContent := '';
            DOMGetAttribute(I.Current, 'name', MetaName);
            DOMGetAttribute(I.Current, 'content', MetaContent);
            (Result as TVRMLRootNode_2).X3DMetaKeys.Add(MetaName);
            (Result as TVRMLRootNode_2).X3DMetaValues.Add(MetaContent);
          end else
          if I.Current.TagName = 'component' then
          begin
            if DOMGetAttribute(I.Current, 'name', ComponentName) then
            begin
              if not DOMGetIntegerAttribute(I.Current, 'level', ComponentLevel) then
                ComponentLevel := 1;
              (Result as TVRMLRootNode_2).X3DComponentNames.Add(ComponentName);
              (Result as TVRMLRootNode_2).X3DComponentLevels.Add(ComponentLevel);
            end else
              VRMLWarning(vwSerious, Format('X3D XML: <component> element without required "name" attribute',
                [I.Current.TagName]));
          end else
            VRMLWarning(vwSerious, Format('X3D XML: unrecognized element "%s" in <head>',
              [I.Current.TagName]));
        end;
      finally FreeAndNil(I) end;
    end;

    procedure ParseVRMLStatement(Element: TDOMElement);

      { You can safely assume that Element.TagName
        indicates proto or externproto. }
      procedure ParseProtoStatement;
      var
        Proto: TVRMLPrototypeBase;
      begin
        if Element.TagName = 'ProtoDeclare' then
          Proto := TVRMLPrototype.Create else
          Proto := TVRMLExternalPrototype.Create;

        Proto.PositionInParent := PositionInParent;

        Result.Prototypes.Add(Proto);

        if Proto is TVRMLPrototype then
          ParsePrototype(Proto as TVRMLPrototype, Element) else
          ParseExternalPrototype(Proto as TVRMLExternalPrototype, Element);
      end;

      procedure ParseRouteStatement;
      var
        Route: TVRMLRoute;
      begin
        Route := TVRMLRoute.Create;
        Route.PositionInParent := PositionInParent;
        Result.Routes.Add(Route);
        Route.ParseXML(Element, Names);
      end;

      procedure ParseImportStatement;
      var
        Import: TVRMLImport;
      begin
        Import := TVRMLImport.Create;
        Import.PositionInParent := PositionInParent;
        Result.ImportsList.Add(Import);
        Import.ParseXML(Element, Names);
      end;

      procedure ParseExportStatement;
      var
        ExportItem: TVRMLExport;
      begin
        ExportItem := TVRMLExport.Create;
        ExportItem.PositionInParent := PositionInParent;
        Result.ExportsList.Add(ExportItem);
        ExportItem.ParseXML(Element, Names);
      end;

      procedure ParseNodeStatement;
      var
        NewNode: TVRMLNode;
        ContainerFieldDummy: string;
      begin
        NewNode := ParseNode(Element, ContainerFieldDummy, false);
        NewNode.PositionInParent := PositionInParent;
        Result.SmartAddChild(NewNode);
      end;

    begin
      if (Element.TagName = 'ProtoDeclare') or
         (Element.TagName = 'ExternProtoDeclare') then
        ParseProtoStatement else
      if Element.TagName = 'ROUTE' then
        ParseRouteStatement else
      if Element.TagName = 'IMPORT' then
        ParseImportStatement else
      if Element.TagName = 'EXPORT' then
        ParseExportStatement else
        ParseNodeStatement;
    end;

  var
    I: TXMLElementIterator;
  begin
    Result := CreateRootNode;
    try
      if ParseX3DHeader then
      begin
        ParseProfile;
        ParseComponentsAndMetas;
      end;

      I := TXMLElementIterator.Create(Element);
      try
        PositionInParent := 0;

        while I.GetNext do
        begin
          ParseVRMLStatement(I.Current);
          Inc(PositionInParent);
        end;
      finally FreeAndNil(I) end;
    except FreeAndNil(Result); raise end;
  end;

var
  Doc: TXMLDocument;
  SceneElement: TDOMElement;

  { Eventually used to decompress gzip file. }
  Stream: TStream;

  Version: string;
begin
  WWWBasePath := ExtractFilePath(ExpandFileName(FileName));

  Stream := nil;

  { X3D XML requires AutoRemove = true below }
  Names := TVRMLNames.Create(true);
  try

    if Gzipped then
    begin
      Stream := TGZFileStream.Create(FileName, gzOpenRead);
      ReadXMLFile(Doc, Stream, 'file:/' + WWWBasePath);
    end else
      ReadXMLFile(Doc, FileName);
    try
      Check(Doc.DocumentElement.TagName = 'X3D',
        'Root element of X3D file must be <X3D>');

      { parse "version" attribute }
      if DOMGetAttribute(Doc.DocumentElement, 'version', Version) then
      begin
        DeFormat(Version, '%d.%d', [@VRMLVerMajor, @VRMLVerMinor]);
        if VRMLVerMajor < 3 then
        begin
          VRMLWarning(vwSerious, Format('X3D version number too low (%d.%d)', [VRMLVerMajor, VRMLVerMinor]));
          VRMLVerMajor := 3;
          VRMLVerMinor := 2;
        end;
      end else
      begin
        { Max X3D version number supported }
        VRMLVerMajor := 3;
        VRMLVerMinor := 2;
        VRMLWarning(vwSerious, Format('Missing X3D version number, assuming %d.%d', [VRMLVerMajor, VRMLVerMinor]));
      end;

      SceneElement := DOMGetChildElement(Doc.DocumentElement, 'Scene', true);
      Result := ParseVRMLStatements(SceneElement, true, Doc.DocumentElement);
    finally FreeAndNil(Doc) end;

    if PrototypeNames <> nil then
      PrototypeNames.Assign(Names.Prototypes);
  finally
    FreeAndNil(Names);
    FreeAndNil(Stream);
  end;
end;

end.
