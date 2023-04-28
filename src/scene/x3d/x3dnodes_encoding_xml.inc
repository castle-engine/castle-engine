{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global routines for parsing XML X3D encoding. }

{$ifdef read_interface}

const
  LoadX3DXml_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz'
  deprecated 'use LoadNode and LoadScene_FileFilters';

{ Read X3D encoded in XML, and convert it to VRML/X3D nodes graph.

  Overloaded version that takes Stream as a parameter expects that
  reading the stream returns the uncompressed content (no longer gzip
  compressed). This version also takes URL as a parameter,
  but it is not used to load contents (these are inside Stream),
  it it only used to resolve relative URLs inside content and for error messages.

  @exclude

  @groupBegin }
function LoadX3DXml(const URL: string; const Gzipped: boolean): TX3DRootNode; overload; deprecated 'use LoadNode';

{ @exclude }
function LoadX3DXmlInternal(const Stream: TStream; const BaseUrl: string): TX3DRootNode; overload;

{ @exclude }
function LoadX3DXmlInternal(const X3DElement: TDOMElement; const BaseUrl: string): TX3DRootNode; overload;

{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

{ Like "Element.AttributeString(AttributeName, Value)",
  but also decoded the read string using DecodeX3DName. }
function XMLAttributeX3DName(const Element: TDOMElement;
  const AttributeName: String; var Value: string): Boolean;
begin
  Result := Element.AttributeString(AttributeName, Value);
  if Result then
    Value := DecodeX3DName(Value);
end;

type
  EX3DXmlError = class(EX3DError);
  EX3DXmlNotAllowedError = class(EX3DXmlError);

const
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';

function ParseXMLNode(const Element: TDOMElement;
  out ContainerField: string; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: boolean): TX3DNode; overload; forward;
function ParseXMLNode(const Element: TDOMElement;
  out ContainerField: string; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: Boolean;
  const AllowNodeCycle: Boolean; var UsingNodeCycle: Boolean): TX3DNode; overload; forward;
function ParseStatements(Element: TDOMElement;
  FileTopLevel: boolean;
  X3DHeaderElement: TDOMElement; Reader: TX3DReaderNames): TX3DRootNode; overload; forward;

{ Checks is Element a correct <connect> element, extracting
  nodeField and protoField value. Returns @true if all Ok, otherwise
  returns @false. }
function ParseConnectElement(Element: TDOMElement;
  out NodeField, ProtoField: string): boolean;
begin
  Result := false;

  if Element.TagName <> 'connect' then
  begin
    WritelnWarning('X3D', 'Only <connect> elements are allowed inside <IS> element');
    Exit;
  end;

  if not Element.AttributeString('nodeField', NodeField) then
  begin
    WritelnWarning('X3D', 'Missing "nodeField" inside <connect> element');
    Exit;
  end;

  if not Element.AttributeString('protoField', ProtoField) then
  begin
    WritelnWarning('X3D', 'Missing "protoField" inside <connect> element');
    Exit;
  end;

  Result := true;
end;

procedure ParseISStatement(Node: TX3DNode; ISElement: TDOMElement;
  var PositionInParent: Integer);
var
  I: TXMLElementIterator;
  NodeField, ProtoField: string;
  NodeFieldOrEvent: TX3DFieldOrEvent;
begin
  I := ISElement.ChildrenIterator;
  try
    while I.GetNext do
      if ParseConnectElement(I.Current, NodeField, ProtoField) then
      begin
        NodeFieldOrEvent := Node.FieldOrEvent(NodeField);
        if NodeFieldOrEvent <> nil then
        begin
          NodeFieldOrEvent.IsClauseNamesAdd(ProtoField);
          NodeFieldOrEvent.PositionInParent := PositionInParent;
          Inc(PositionInParent);
        end else
          WritelnWarning('X3D', Format('<connect> element "nodeField" doesn''t indicate any known field/event name: "%s"', [NodeField]));
      end;
  finally FreeAndNil(I) end;
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
  WritelnWarning and simply return @nil.

  @raises(EX3DXmlNotAllowedError On various not-allowed errors.)

  @raises(EX3DXmlUnknownNodeNotAllowed On a special parsing error:
    we got unknown node name, and AllowedNodes was @false.

    We have a special error class for this, because in some cases
    it means that actually the unknown node name could be also
    unknown field / proto etc. name, so error message for the user should
    be better.)
*)
function ParseXMLNode(const Element: TDOMElement;
  out ContainerField: string; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: Boolean;
  const AllowNodeCycle: Boolean; var UsingNodeCycle: Boolean): TX3DNode;

  procedure ParseNamedNode(const NodeName: string);
  var
    NodeClass: TX3DNodeClass;
    X3DType: string;
    ProtoName: string;
    Proto: TX3DPrototypeBase;
    ProtoIter: TXMLElementIterator;
    FieldActualValue, FieldName: string;
    FieldIndex: Integer;
    PositionInParent: Integer;
  begin
    X3DType := Element.TagName8;

    if X3DType = 'ProtoInstance' then
    begin
      if not Element.AttributeString('name', ProtoName) then
        raise EX3DXmlError.Create('<ProtoInstance> doesn''t specify "name" of the prototype');

      Proto := Reader.Prototypes.Bound(ProtoName);
      if Proto = nil then
        raise EX3DXmlError.CreateFmt('<ProtoInstance> specifies unknown prototype name "%s"', [ProtoName]);

      if (Proto is TX3DExternalPrototype) and
         (TX3DExternalPrototype(Proto).ReferencedClass <> nil) then
        Result := TX3DExternalPrototype(Proto).ReferencedClass.Create(NodeName, Reader.BaseUrl) else
        Result := TX3DPrototypeNode.CreatePrototypeNode(NodeName, Reader.BaseUrl, Proto);

      Reader.Nodes.Bind(Result, false);

      { parse field values from <fieldValue> elements }
      ProtoIter := Element.ChildrenIterator;
      try
        PositionInParent := 0;

        while ProtoIter.GetNext do
        begin
          if ProtoIter.Current.TagName = 'fieldValue' then
          begin
            if not ProtoIter.Current.AttributeString('name', FieldName) then
            begin
              WritelnWarning('X3D', 'X3D XML: missing "name" attribute for <fieldValue> element');
              Continue;
            end;

            FieldIndex := Result.IndexOfField(FieldName);
            if FieldIndex = -1 then
            begin
              WritelnWarning('X3D', Format('X3D XML: <fieldValue> element references unknown field name "%s"', [FieldName]));
              Continue;
            end;

            if ProtoIter.Current.AttributeString('value', FieldActualValue) then
              Result.Fields[FieldIndex].ParseXMLAttribute(FieldActualValue, Reader) else
              Result.Fields[FieldIndex].ParseXMLElement(ProtoIter.Current, Reader);

            Result.Fields[FieldIndex].PositionInParent := PositionInParent;
          end else
          if ProtoIter.Current.TagName = 'IS' then
          begin
            ParseISStatement(Result, ProtoIter.Current, PositionInParent);
          end else
          begin
            WritelnWarning('X3D', Format('X3D XML: only <fieldValue> or <IS> elements expected in prototype instantiation, but "%s" found', [ProtoIter.Current.TagName]));
          end;

          Inc(PositionInParent);
        end;
      finally FreeAndNil(ProtoIter) end;

      { If it was normal (non-external) prototype, then instantiate
        it now (this sort-of expands prototype "macro" in place). }
      if Result is TX3DPrototypeNode then
      try
        Result := TX3DPrototypeNode(Result).Instantiate;
      except
        on E: EX3DPrototypeInstantiateError do
          { Just write E.Message and silence the exception.
            Result will simply remain as TX3DPrototypeNode instance in this case. }
          WritelnWarning('X3D', E.Message);
      end;
    end else
    begin
      NodeClass := NodesManager.X3DTypeToClass(X3DType, Reader.Version);
      if NodeClass <> nil then
      begin
        Result := NodeClass.Create(NodeName, Reader.BaseUrl);
        Reader.Nodes.Bind(Result, false);
        Result.ParseXML(Element, Reader);
      end else
      begin
        Result := TX3DUnknownNode.CreateUnknown(NodeName, Reader.BaseUrl, X3DType);

        { In classic VRML parser, we had special TX3DUnknownNode.Parse
          that performed the "magic" trick of
          ParseIgnoreToMatchingCurlyBracket. This is not needed for
          X3D XML, we can simply omit the node by not looking
          at it's attributes. All we need to do is to make
          WritelnWarning warning. }

        WritelnWarning('X3D', 'Unknown X3D node type "' + X3DType + '"');
      end;
    end;

    Reader.Nodes.Bind(Result, true);
  end;

var
  NodeName, S: string;
  UsedNodeFinished: boolean;
begin
  Result := nil;
  try
    if XMLAttributeX3DName(Element, 'USE', NodeName) then
    begin
      { get appropriate node }
      Result := Reader.Bound(NodeName, UsedNodeFinished, true);
      { When AllowNodeCycle, then UsedNodeFinished=false is not a problem.
        Just flip UsingNodeCycle to true, caller can handle a loop in node definition. }
      if (Result <> nil) and (not UsedNodeFinished) and AllowNodeCycle then
      begin
        UsingNodeCycle := true;
        UsedNodeFinished := true;
      end;
      if (Result = nil) or (not UsedNodeFinished) then
      begin
        if Result = nil then
          S := Format('Incorrect USE clause: node name "%s" undefined', [NodeName]) else
        begin
          S := Format('Cycles in VRML/X3D graph: USE clause inside node "%s" refers to the same node', [NodeName]);
          Result := nil; { return nil in case of cycles }
        end;
        if NilIfUnresolvedUSE then
          WritelnWarning('X3D', S)
        else
          raise EX3DXmlNotAllowedError.Create(S);
      end;
    end else
    begin
      if XMLAttributeX3DName(Element, SAttrDEF, NodeName) then
      begin
        ParseNamedNode(NodeName)
      end else
        ParseNamedNode('');
    end;

    { calculate ContainerField.

      Note that we do not diffentiate here between the case of <USE>
      element and real node element --- because that's the intention
      of X3D specification, in both situations element may have
      containerField attribute.

      We either use DefaultContainerField, or explicit containerField value.
      Note that containerField doesn't have to be preserved
      (see demo_models/x3d/container_field_def_use.x3d).
      Each USE occurrence must specify suitable containerField or use class default. }
    if Result <> nil then
      ContainerField := Result.DefaultContainerField else
      ContainerField := ''; { will not be used anyway }
    Element.AttributeString(SAttrContainerField, ContainerField);

  except FreeAndNil(Result); raise end;
end;

function ParseXMLNode(const Element: TDOMElement;
  out ContainerField: string; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: boolean): TX3DNode;
var
  IgnoredUsingNodeCycle: Boolean;
begin
  IgnoredUsingNodeCycle := false;
  Result := ParseXMLNode(Element, ContainerField, Reader, NilIfUnresolvedUSE, false, IgnoredUsingNodeCycle);
end;

{ This parses a sequence of X3D statements: any number of nodes,
  (external) protypes, routes.
  This is good to use to parse whole VRML file (when FileTopLevel = true),
  or a (non-external) prototype content (when FileTopLevel = false).

  It's somewhat similar to classic ParseStatements.
  (Admittedly, it was even implemented by copying and modifying
  classic ParseStatements :) ). }
function ParseStatements(Element: TDOMElement;
  FileTopLevel: boolean;
  X3DHeaderElement: TDOMElement; Reader: TX3DReaderNames): TX3DRootNode; overload;
var
  PositionInParent: Integer;

  { Create root group node. }
  function CreateRootNode: TX3DRootNode;
  begin
    Result := TX3DRootNode.Create('', Reader.BaseUrl);
    Result.HasForceVersion := true;
    Result.ForceVersion := Reader.Version;
  end;

  procedure ParseProfile;
  var
    Profile: string;
  begin
    { parse "profile" attribute }
    if X3DHeaderElement.AttributeString('profile', Profile) then
    begin
      Result.Profile := Profile;
    end else
      { We allow PROFILE to be omitted.
        Actually, we do not use profile for anything right now. }
      WritelnWarning('X3D', 'X3D "profile" attribute missing');
  end;

  procedure ParseHead;

    procedure ParseMeta(Element: TDOMElement);
    var
      MetaName, MetaContent: string;
    begin
      MetaName := '';
      MetaContent := '';
      Element.AttributeString('name', MetaName);
      Element.AttributeString('content', MetaContent);
      Result.Meta[MetaName] := MetaContent;
    end;

    procedure ParseComponent(Element: TDOMElement);
    var
      ComponentName: string;
      ComponentLevel: Integer;
    begin
      if Element.AttributeString('name', ComponentName) then
      begin
        if not Element.AttributeInteger('level', ComponentLevel) then
          ComponentLevel := 1;
        Result.Components[ComponentName] := ComponentLevel;
      end else
        WritelnWarning('X3D', Format('X3D XML: <component> element without required "name" attribute',
          [Element.TagName]));
    end;

    procedure ParseUnit(Element: TDOMElement);
    var
      Category, Name: string;
      ConversionFactor: Float;
    begin
      if not Element.AttributeString('category', Category) then
      begin
        WritelnWarning('X3D', 'Missing <unit> category');
        Exit;
      end;

      if not Element.AttributeString('name', Name) then
      begin
        WritelnWarning('X3D', 'Missing <unit> category');
        Exit;
      end;

      if not Element.AttributeFloat('conversionFactor', ConversionFactor) then
      begin
        WritelnWarning('X3D', 'Missing <unit> category');
        Exit;
      end;

      Reader.UnitConversion(Category, Name, ConversionFactor);
    end;

  var
    Head: TDOMElement;
    I: TXMLElementIterator;
  begin
    Head := X3DHeaderElement.ChildElement('head', false);
    if Head = nil then Exit;

    I := Head.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName = 'meta' then
          ParseMeta(I.Current) else
        if I.Current.TagName = 'component' then
          ParseComponent(I.Current) else
        if I.Current.TagName = 'unit' then
          ParseUnit(I.Current) else
          WritelnWarning('X3D', Format('X3D XML: unrecognized element "%s" in <head>',
            [I.Current.TagName]));
      end;
    finally FreeAndNil(I) end;
    Result.Scale := Reader.LengthConversionFactor;
  end;

  procedure ParseStatement(Element: TDOMElement);

    { You can safely assume that Element.TagName
      indicates proto or externproto. }
    procedure ParseProtoStatement;
    var
      Proto: TX3DPrototypeBase;
    begin
      if Element.TagName = 'ProtoDeclare' then
        Proto := TX3DPrototype.Create else
        Proto := TX3DExternalPrototype.Create;

      Proto.PositionInParent := PositionInParent;

      Result.AddPrototype(Proto);

      Proto.ParseXML(Element, Reader);
    end;

    procedure ParseRouteStatement;
    var
      Route: TX3DRoute;
    begin
      Route := TX3DRoute.Create;
      Route.PositionInParent := PositionInParent;
      Result.AddRoute(Route);
      Route.ParseXML(Element, Reader);
    end;

    procedure ParseImportStatement;
    var
      Import: TX3DImport;
    begin
      Import := TX3DImport.Create;
      Import.PositionInParent := PositionInParent;
      Result.AddImport(Import);
      Import.ParseXML(Element, Reader);
    end;

    procedure ParseExportStatement;
    var
      ExportItem: TX3DExport;
    begin
      ExportItem := TX3DExport.Create;
      ExportItem.PositionInParent := PositionInParent;
      Result.AddExport(ExportItem);
      ExportItem.ParseXML(Element, Reader);
    end;

    procedure ParseNodeStatement;
    var
      NewNode: TX3DNode;
      ContainerFieldDummy: string;
    begin
      NewNode := ParseXMLNode(Element, ContainerFieldDummy, Reader, false);
      NewNode.PositionInParent := PositionInParent;
      Result.FdChildren.Add(NewNode);
      { for prototypes, do not check NewNode class, as anything is valid. }
      if FileTopLevel then
        Result.TopLevelCheckChild(NewNode);
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
    if FileTopLevel then
    begin
      ParseProfile;
      ParseHead;
    end;

    I := Element.ChildrenIterator;
    try
      PositionInParent := 0;

      while I.GetNext do
      begin
        ParseStatement(I.Current);
        Inc(PositionInParent);
      end;
    finally FreeAndNil(I) end;
  except FreeAndNil(Result); raise end;
end;

function LoadX3DXml(const URL: string; const Gzipped: boolean): TX3DRootNode;
var
  Stream: TStream;
  StreamOptions: TStreamOptions;
begin
  StreamOptions := [];
  if Gzipped then
    Include(StreamOptions, soGzip);
  Stream := Download(URL, StreamOptions);
  try
    Result := LoadX3DXmlInternal(Stream, AbsoluteURI(URL));
  finally
    FreeAndNil(Stream);
  end;
end;

function LoadX3DXmlInternal(const Stream: TStream; const BaseUrl: string): TX3DRootNode;
var
  Doc: TXMLDocument;
begin
  Doc := nil;
  try
    { The ReadXMLFile receives BaseUrl only to produce nice error messages
      in case of errors. }
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(Doc, Stream, AbsoluteURI(BaseUrl));
    Result := LoadX3DXmlInternal(Doc.DocumentElement, BaseUrl);
  finally FreeAndNil(Doc); end;
end;

function LoadX3DXmlInternal(const X3DElement: TDOMElement; const BaseUrl: string): TX3DRootNode; overload;
var
  SceneElement: TDOMElement;
  VersionStr: string;
  Version: TX3DVersion;
  { TODO: each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
  Reader: TX3DReaderNames;
begin
  Check(X3DElement.TagName = 'X3D', 'Root element of X3D file must be <X3D>');

  { parse "version" attribute }
  if X3DElement.AttributeString('version', VersionStr) then
  begin
    DeFormat(VersionStr, '%d.%d', [@Version.Major, @Version.Minor]);
    if Version.Major < 3 then
    begin
      WritelnWarning('X3D', Format('X3D version number too low (%d.%d)', [Version.Major, Version.Minor]));
      Version := X3DVersion; { some sensible version number }
    end;
  end else
  begin
    Version := X3DVersion; { some sensible version number }
    WritelnWarning('X3D', Format('Missing X3D version number, assuming %d.%d', [Version.Major, Version.Minor]));
  end;

  SceneElement := X3DElement.ChildElement('Scene', true);

  { X3D XML requires AutoRemove = true below }
  Reader := TX3DReaderNames.Create(true, BaseUrl, Version);
  try
    Result := ParseStatements(SceneElement, true, X3DElement, Reader);
    Reader.ExtractNames(Result.FPrototypeNames, Result.FExportedNames, Result.FImportedNames);
  finally FreeAndNil(Reader) end;
end;

{$endif read_implementation}
