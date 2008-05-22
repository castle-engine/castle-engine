{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Read X3D encoded in XML, and convert it to VRML/X3D nodes graph. }
unit X3DXmlToVRML;

interface

uses VRMLNodes;

function LoadX3DXmlAsVRML(const FileName: string;
  Gzipped: boolean): TVRMLNode;

implementation

uses SysUtils, DOM, XMLRead, KambiUtils, KambiXMLUtils, Classes,
  VRMLLexer, VRMLErrors, VRMLFields, KambiZStream,
  KambiClassUtils;

type
  EX3DXmlError = class(EVRMLError);
  EX3DXmlNotAllowedError = class(EX3DXmlError);
  EX3DXmlUnknownNodeNotAllowed = class(EX3DXmlError);

{ TODO: this is a hasty implementation.
  I should read X3D spec about XML encoding once again, this time carefully
  adding handling all remaining bits to this reader.
  Like X3D "profile" attribute, the whole <head> element, etc.
}

function LoadX3DXmlAsVRML(const FileName: string;
  Gzipped: boolean): TVRMLNode;
var
  WWWBasePath: string;

  { This is used the same way as Lexer.NodeNameBinding when
    reading VRML files (that is, in classic VRML encoding).

    TODO: this means that each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
  NodeNameBinding: TStringList;

  ProtoNamebinding: TStringList;

const
  { X3D version numbers. }
  VRMLVerMajor = 3;
  VRMLVerMinor = 1;
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';
  SNull = 'NULL';

  function ParseNode(Element: TDOMElement;
    out ContainerField: string;
    NilIfUnresolvedUSE: boolean): TVRMLNode; forward;
  function ParseVRMLStatements(Element: TDOMElement): TVRMLNode; forward;
  procedure ParsePrototype(Proto: TVRMLPrototype; Element: TDOMElement); forward;
  procedure ParseExternalPrototype(Proto: TVRMLExternalPrototype;
    Element: TDOMElement); forward;
  procedure ParseInterfaceDeclaration(
    I: TVRMLInterfaceDeclaration; Element: TDOMElement;
    FieldValue, IsClauseAllowed: boolean); forward;

  { Parse ROUTE. Conceptually equivalent to TVRMLRoute.Parse in classic VRML
    encoding. }
  procedure ParseRoute(Route: TVRMLRoute;
    Element: TDOMElement);

    function RequiredAttrib(const AttrName: string): string;
    begin
      if not DOMGetAttribute(Element, AttrName, Result) then
      begin
        VRMLNonFatalError('Missing ROUTE ' + AttrName + ' attribute');
        Result := '';
      end;
    end;

  begin
    Route.SourceNodeName := RequiredAttrib('fromNode');
    Route.SourceFieldName := RequiredAttrib('fromField');
    Route.DestinationNodeName := RequiredAttrib('toNode');
    Route.DestinationFieldName := RequiredAttrib('toField');
  end;

  procedure ParseFieldValueFromAttribute(Field: TVRMLField;
    const Value: string);
  var
    Lexer: TVRMLLexer;
    SF: TSFNode;
    MF: TMFNode;
    Node: TVRMLNode;
    NodeIndex: Integer;
  begin
    if Field is TSFString then
    begin
      { SFString has quite special interpretation, it's just attrib
        name. It would not be usefull trying to use TVRMLLexer here,
        it's easier just to handle this as a special case. }
      TSFString(Field).Value := Value;
    end else
    if Field is TSFNode then
    begin
      { For SFNode and MFNode, X3D XML encoding has special handling:
        field value just indicates the node name, or NULL.
        (other values for SFNode / MFNode cannot be expressed inside
        the attribute). }

      SF := Field as TSFNode;

      { get appropriate node }
      NodeIndex := NodeNameBinding.IndexOf(Value);
      if NodeIndex = -1 then
      begin
        if Value = SNull then
          SF.Value := nil else
          VRMLNonFatalError(Format('Invalid node name for SFNode field: "%s"', [Value]));
      end else
      begin
        Node := TVRMLNode(NodeNameBinding.Objects[NodeIndex]);
        SF.Value := Node;
        SF.WarningIfChildNotAllowed(Node);
      end;
    end else
    if Field is TMFNode then
    begin
      MF := Field as TMFNode;

      { get appropriate node }
      NodeIndex := NodeNameBinding.IndexOf(Value);
      if NodeIndex = -1 then
      begin
        { NULL not allowed for MFNode, like for SFNode }
        VRMLNonFatalError(Format('Invalid node name for MFNode field: "%s"', [Value]));
      end else
      begin
        Node := TVRMLNode(NodeNameBinding.Objects[NodeIndex]);
        MF.AddItem(Node);
        MF.WarningIfChildNotAllowed(Node);
      end;
    end else
    begin
      Lexer := TVRMLLexer.CreateForPartialStream(Value, WWWBasePath,
        VRMLVerMajor, VRMLVerMinor);
      try
        try
          Field.ParseX3DXmlAttr(Lexer);
        except
          on E: EVRMLParserError do
          begin
            if Field is TMFString then
            begin
              { This is very common error, even in models from
                http://www.web3d.org/x3d/content/examples/Basic/
                Although specification clearly says that MFString
                components should always be enclosed within double
                quotes. We just do what Xj3D seems to do, that is
                we handle this as a single string (producing a warning). }
              VRMLNonFatalError('Error when parsing MFString field "' + Field.Name + '" value, probably missing double quotes (treating as a single string): ' + E.Message);
              TMFString(Field).Items.Count := 0;
              TMFString(Field).Items.AppendItem(Value);
            end else
              VRMLNonFatalError('Error when parsing field "' + Field.Name + '" value: ' + E.Message);
          end;
        end;
      finally FreeAndNil(Lexer) end;
    end;
  end;

  { Checks is Element a correct <connect> element, extracting
    nodeField and protoField value. Returns @true if all Ok, otherwise
    returns @false. }
  function ParseConnectElement(Element: TDOMElement;
    out NodeField, ProtoField: string): boolean;
  begin
    Result := false;

    if Element.TagName <> 'connect' then
    begin
      VRMLNonFatalError('Only <connect> elements are allowed inside <IS> element');
      Exit;
    end;

    if not DOMGetAttribute(Element, 'nodeField', NodeField) then
    begin
      VRMLNonFatalError('Missing "nodeField" inside <connect> element');
      Exit;
    end;

    if not DOMGetAttribute(Element, 'protoField', ProtoField) then
    begin
      VRMLNonFatalError('Missing "protoField" inside <connect> element');
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
            FreeAndNil(Child);
            VRMLNonFatalError('X3D field "' + Field.Name + '" is not SFNode or MFNode, but a node value (XML element) is specified');
          end;
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  { Parse node body, i.e. mainly node's fields.
    This is roughly equivalent to TVRMLNode.Parse in classic VRML encoding
    parser. }
  procedure ParseNodeBody(Node: TVRMLNode;
    Element: TDOMElement);

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
          ParseFieldValueFromAttribute(Node.Fields[Index], Attr.Value);
        end else
          VRMLNonFatalError('Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.Name + '" in node "' + Node.NodeTypeName + '"');
      end;
    end;

    procedure ParseXMLChildrenNodes;

      procedure ParseISStatement(ISElement: TDOMElement);
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
                NodeFieldOrEvent.IsClause := true;
                NodeFieldOrEvent.IsClauseName := ProtoField;
              end else
                VRMLNonFatalError(Format('<connect> element "nodeField" doesn''t indicate any known field/event name: "%s"', [NodeField]));
            end;
        finally FreeAndNil(I) end;
      end;

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
    begin
      I := TXMLElementIterator.Create(Element);
      try
        while I.GetNext do
        begin
          if I.Current.TagName = 'ROUTE' then
          begin
            Route := TVRMLRoute.Create;
            Node.Routes.Add(Route);
            ParseRoute(Route, I.Current);
          end else
          if I.Current.TagName = 'IS' then
          begin
            ParseISStatement(I.Current);
          end else
          if I.Current.TagName = 'ProtoDeclare' then
          begin
            Proto := TVRMLPrototype.Create;
            Node.Prototypes.Add(Proto);
            ParsePrototype(Proto, I.Current);
          end else
          if I.Current.TagName = 'ExternProtoDeclare' then
          begin
            ExternProto := TVRMLExternalPrototype.Create;
            Node.Prototypes.Add(ExternProto);
            ParseExternalPrototype(ExternProto, I.Current);
          end else
          if I.Current.TagName = 'field' then
          begin
            IDecl := TVRMLInterfaceDeclaration.Create;
            try
              ParseInterfaceDeclaration(IDecl, I.Current, true, true);
              if IDecl.AccessType in Node.HasInterfaceDeclarations then
                Node.InterfaceDeclarations.Add(IDecl) else
              begin
                FreeAndNil(IDecl);
                VRMLNonFatalError('X3D XML: specified <field> inside node, but this node doesn''t allow interface declaration with such accessType');
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
              FieldIndex := Node.Fields.IndexOf(ContainerField);
              if FieldIndex >= 0 then
              begin
                if Node.Fields[FieldIndex] is TSFNode then
                begin
                  SF := Node.Fields[FieldIndex] as TSFNode;
                  SF.Value := Child;
                  SF.WarningIfChildNotAllowed(Child);
                end else
                if Node.Fields[FieldIndex] is TMFNode then
                begin
                  MF := Node.Fields[FieldIndex] as TMFNode;
                  MF.AddItem(Child);
                  MF.WarningIfChildNotAllowed(Child);
                end else
                begin
                  FreeAndNil(Child);
                  VRMLNonFatalError('X3D field "' + ContainerField + '" is not SFNode or MFNode, but a node value (XML element) is specified');
                end;
              end else
              begin
                FreeAndNil(Child);
                VRMLNonFatalError('Unknown X3D field name (indicated by containerField value) "' + ContainerField + '" in node "' + Node.NodeTypeName + '"');
              end;
            end;
          end;
        end;
      finally FreeAndNil(I) end;
    end;

  begin
    ParseXMLAttributes;
    ParseXMLChildrenNodes;
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
    VRMLNonFatalError and simply return @nil.

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
      ProtoIndex: Integer;
      Proto: TVRMLPrototypeBase;
      ProtoIter: TXMLElementIterator;
      FieldActualValue, FieldName: string;
      FieldIndex: Integer;
    begin
      NodeTypeName := Element.TagName;

      if NodeTypeName = 'ProtoInstance' then
      begin
        if not DOMGetAttribute(Element, 'name', ProtoName) then
          raise EX3DXmlError.Create('<ProtoInstance> doesn''t specify "name" of the prototype');

        ProtoIndex := ProtoNameBinding.IndexOf(ProtoName);
        if ProtoIndex = -1 then
          raise EX3DXmlError.CreateFmt('<ProtoInstance> specifies unknown prototype name "%s"', [ProtoName]);

        Proto := ProtoNameBinding.Objects[ProtoIndex] as TVRMLPrototypeBase;
        if (Proto is TVRMLExternalPrototype) and
           (TVRMLExternalPrototype(Proto).ReferencedClass <> nil) then
          Result := TVRMLExternalPrototype(Proto).ReferencedClass.Create(NodeName, WWWBasePath) else
          Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, WWWBasePath, Proto);

        { parse field values from <fieldValue> elements }
        ProtoIter := TXMLElementIterator.Create(Element);
        try
          while ProtoIter.GetNext do
          begin
            if ProtoIter.Current.TagName <> 'fieldValue' then
            begin
              VRMLNonFatalError('X3D XML: only <fieldValue> elements expected in prototype instantiation');
              Continue;
            end;

            if not DOMGetAttribute(ProtoIter.Current, 'name', FieldName) then
            begin
              VRMLNonFatalError('X3D XML: missing "name" attribute for <fieldValue> element');
              Continue;
            end;

            FieldIndex := Result.Fields.IndexOf(FieldName);
            if FieldIndex = -1 then
            begin
              VRMLNonFatalError(Format('X3D XML: <fieldValue> element references unknown field name "%s"', [FieldName]));
              Continue;
            end;

            if DOMGetAttribute(ProtoIter.Current, 'value', FieldActualValue) then
              ParseFieldValueFromAttribute(Result.Fields[FieldIndex], FieldActualValue) else
              ParseFieldValueFromElement(Result.Fields[FieldIndex], ProtoIter.Current);
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
            VRMLNonFatalError(E.Message);
        end;
      end else
      begin
        NodeClass := NodesManager.NodeTypeNameToClass(NodeTypeName,
          VRMLVerMajor, VRMLVerMinor);
        if NodeClass <> nil then
        begin
          Result := NodeClass.Create(NodeName, WWWBasePath);
        end else
        begin
          Result := TNodeUnknown.CreateUnknown(NodeName, WWWBasePath, NodeTypeName);
        end;

        ParseNodeBody(Result, Element);
      end;

      { TODO: this has a problem, see classic VRML ParseNode
        comment starting with "Cycles in VRML graph are bad..." }

      Result.Bind(NodeNameBinding);
    end;

  var
    NodeName, S: string;
    i: integer;
  begin
    Result := nil;
    try
      if DOMGetAttribute(Element, 'USE', NodeName) then
      begin
        { get appropriate node }
        I := NodeNameBinding.IndexOf(NodeName);
        if I = -1 then
        begin
          S := Format('Incorrect USE element: node name "%s" undefined',
            [NodeName]);
          if NilIfUnresolvedUSE then
          begin
            Result := nil;
            VRMLNonFatalError(S);
          end else
            raise EX3DXmlNotAllowedError.Create(S);
        end else
          Result := TVRMLNode(NodeNameBinding.Objects[i]);
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

        TODO: hm, if USEd element also had containerField, then this should
        be taken into account? So we also need TVRMLNode.ContainerField,
        set by ParseNamedNode but not by <USE> case? }
      if Result <> nil then
        ContainerField := Result.DefaultContainerField;
      DOMGetAttribute(Element, SAttrContainerField, ContainerField);

    except FreeAndNil(Result); raise end;
  end;

  { Equivalent to TVRMLEvent.Parse }
  (*
  procedure ParseEvent(Event: TVRMLEvent; Element: TDOMElement);
  begin
    ElementIs := DOMGetChildElement(Element, 'IS',
  end;
  TODO
  *)

  { This is equivalent to TVRMLInterfaceDeclaration.Parse
    in classic VRML parser. }
  procedure ParseInterfaceDeclaration(
    I: TVRMLInterfaceDeclaration; Element: TDOMElement;
    FieldValue, IsClauseAllowed: boolean);
  var
    AccessType: TVRMLAccessType;
    AccessTypeIndex: Integer;
    AccessTypeName: string;
    FieldTypeName: string;
    FieldType: TVRMLFieldClass;
    Name, FieldActualValue: string;
  begin
    { clear instance before parsing }
    I.Event.Free; I.Event := nil;
    I.Field.Free; I.Field := nil;

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
        I.Event := TVRMLEvent.Create(Name, FieldType, AccessType = atInputOnly);
      atInitializeOnly, atInputOutput:
        begin
          I.Field := FieldType.CreateUndefined(Name);
          I.Field.Exposed := AccessType = atInputOutput;
        end;
      else raise EInternalError.Create('AccessType ?');
    end;

    if I.Event <> nil then
    begin
      {TODO if IsClauseAllowed then
        ParseEvent(I.Event, Element};
    end else
    begin
      if FieldValue then
      begin
        if DOMGetAttribute(Element, 'value', FieldActualValue) then
          ParseFieldValueFromAttribute(I.Field, FieldActualValue) else
          ParseFieldValueFromElement(I.Field, Element);
      end else
      {TODO if IsClauseAllowed then
        Field.ParseIsClause(Lexer)};
    end;
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
          I := TVRMLInterfaceDeclaration.Create;
          Proto.InterfaceDeclarations.Add(I);
          ParseInterfaceDeclaration(I, Iter.Current, not ExternalProto, false);
        end else
          VRMLNonFatalError('X3D XML: only <field> elements expected in prototype interface');
      end;
    finally FreeAndNil(Iter) end;
  end;

  { Equivalent to TVRMLPrototype.Parse }
  procedure ParsePrototype(Proto: TVRMLPrototype; Element: TDOMElement);
  var
    OldNodeNameBinding: TStringList;
    OldProtoNameBinding: TStringList;
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
      scope, completely independent from the outside. So we create
      new NodeNameBinding for parsing prototype. }
    OldNodeNameBinding := NodeNameBinding;
    NodeNameBinding := TStringListCaseSens.Create;
    try
      { Also prototype name scope is local within the prototype,
        however it starts from current prototype name scope (not empty,
        like in case of NodeNameBinding). So prototypes defined outside
        are available inside, but nested prototypes inside are not
        available outside. }
      OldProtoNameBinding := ProtoNameBinding;
      ProtoNameBinding := TStringListCaseSens.Create;
      try
        ProtoNameBinding.Assign(OldProtoNameBinding);
        Proto.Node := ParseVRMLStatements(E);
      finally
        FreeAndNil(ProtoNameBinding);
        ProtoNameBinding := OldProtoNameBinding;
      end;
    finally
      FreeAndNil(NodeNameBinding);
      NodeNameBinding := OldNodeNameBinding;
    end;

    Proto.Bind(ProtoNameBinding);
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
      ParseFieldValueFromAttribute(Proto.URLList, URLListValue) else
      raise EX3DXmlError.Create('Missing "url" for <ExternProtoDeclare> element');

    Proto.Bind(ProtoNameBinding);

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
    returns everything read wrapped in artifical TNodeGroupHidden_2 instance. }
  function ParseVRMLStatements(Element: TDOMElement): TVRMLNode;

    { Create hidden group node. }
    function CreateHiddenGroup: TVRMLNode;
    begin
      Result := TNodeGroupHidden_2.Create('', WWWBasePath);
    end;

    { Change Result to a hidden group node, if it's not already.
      If previous Result was <> @nil,
      then it will be inserted as first child of this new hidden group. }
    procedure MakeResultHiddenGroup;
    var
      ChildNode: TVRMLNode;
    begin
      if not ( (Result <> nil) and
               ( (Result is TNodeGroupHidden_1) or
                 (Result is TNodeGroupHidden_2) ) ) then
      begin
        ChildNode := Result;
        Result := CreateHiddenGroup;
        if ChildNode <> nil then
          Result.SmartAddChild(ChildNode);
      end;
    end;

    procedure ParseVRMLStatement(Element: TDOMElement);

      procedure ParseRouteInternal;
      var
        Route: TVRMLRoute;
      begin
        Route := TVRMLRoute.Create;

        MakeResultHiddenGroup;
        Result.Routes.Add(Route);

        ParseRoute(Route, Element);
      end;

      { You can safely assume that Element.TagName
        indicates proto or externproto. }
      procedure ParseProtoStatement;
      var
        Proto: TVRMLPrototypeBase;
      begin
        if Element.TagName = 'ProtoDeclare' then
          Proto := TVRMLPrototype.Create else
          Proto := TVRMLExternalPrototype.Create;

        MakeResultHiddenGroup;
        Result.Prototypes.Add(Proto);

        if Proto is TVRMLPrototype then
          ParsePrototype(Proto as TVRMLPrototype, Element) else
          ParseExternalPrototype(Proto as TVRMLExternalPrototype, Element);
      end;

      procedure ParseNodeInternal;
      var
        NewNode: TVRMLNode;
        ContainerFieldDummy: string;
      begin
        NewNode := ParseNode(Element, ContainerFieldDummy, false);

        if Result = nil then
        begin
          { This will happen on 1st ParseNode call. }
          Result := NewNode;
        end else
        begin
          { Result <> nil, so make sure it's a hidden group
            (we don't want to insert NewNode into normal node).
            This will happen on 2nd ParseNode call.
            Result is now assigned, but we want to add 2nd node: so we wrap
            current Result (and NewNode too) together in a hidden Group node. }
          MakeResultHiddenGroup;
          Result.SmartAddChild(NewNode);
        end;
      end;

    begin
      if Element.TagName = 'ROUTE' then
        ParseRouteInternal else
      if (Element.TagName = 'ProtoDeclare') or
         (Element.TagName = 'ExternProtoDeclare') then
        ParseProtoStatement else
        ParseNodeInternal;
    end;

  var
    I: TXMLElementIterator;
  begin
    Result := nil;
    try
      I := TXMLElementIterator.Create(Element);
      try
        while I.GetNext do
        begin
          ParseVRMLStatement(I.Current);
        end;
      finally FreeAndNil(I) end;

      if Result = nil then
        Result := CreateHiddenGroup;
    except FreeAndNil(Result); raise end;
  end;

var
  Doc: TXMLDocument;
  SceneElement: TDOMElement;

  { Eventually used to decompress gzip file. }
  Stream: TStream;
begin
  WWWBasePath := ExtractFilePath(ExpandFileName(FileName));

  Stream := nil;
  NodeNameBinding := nil;
  ProtoNameBinding := nil;
  try
    NodeNameBinding := TStringList.Create;
    ProtoNameBinding := TStringList.Create;

    if Gzipped then
    begin
      Stream := TGZFileStream.Create(FileName, gzOpenRead);
      ReadXMLFile(Doc, Stream, 'file:/' + WWWBasePath);
    end else
      ReadXMLFile(Doc, FileName);
    try
      Check(Doc.DocumentElement.TagName = 'X3D',
        'Root node of X3D file must be <X3D>');

      SceneElement := DOMGetChildElement(Doc.DocumentElement, 'Scene', true);
      Result := ParseVRMLStatements(SceneElement);
    finally FreeAndNil(Doc) end;
  finally
    FreeAndNil(NodeNameBinding);
    FreeAndNil(ProtoNameBinding);
    FreeAndNil(Stream);
  end;
end;

end.
