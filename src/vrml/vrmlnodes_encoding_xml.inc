{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global routines for parsing XML X3D encoding. }

{$ifdef read_interface}

const
  LoadX3DXml_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz';

{ Read X3D encoded in XML, and convert it to VRML/X3D nodes graph.

  Overloaded version that takes Stream as a parameter expects that
  stream (as read) is no longer compressed.
  @groupBegin }
function LoadX3DXml(const FileName: string; Gzipped: boolean): TVRMLRootNode;
function LoadX3DXml(Stream: TStream; const WWWBasePath: string;
  const BaseName: string = ''): TVRMLRootNode;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

type
  EX3DXmlError = class(EVRMLError);
  EX3DXmlNotAllowedError = class(EX3DXmlError);
  EX3DXmlUnknownNodeNotAllowed = class(EX3DXmlError);

const
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';

function ParseXMLNode(Element: TDOMElement;
  out ContainerField: string; Names: TVRMLNames;
  NilIfUnresolvedUSE: boolean): TVRMLNode; forward;
function ParseVRMLStatements(Element: TDOMElement;
  ParseX3DHeader: boolean;
  X3DHeaderElement: TDOMElement; Names: TVRMLNames): TVRMLRootNode; forward;

{ Checks is Element a correct <connect> element, extracting
  nodeField and protoField value. Returns @true if all Ok, otherwise
  returns @false. }
function ParseConnectElement(Element: TDOMElement;
  out NodeField, ProtoField: string): boolean;
begin
  Result := false;

  if Element.TagName <> 'connect' then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Only <connect> elements are allowed inside <IS> element');
    Exit;
  end;

  if not DOMGetAttribute(Element, 'nodeField', NodeField) then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Missing "nodeField" inside <connect> element');
    Exit;
  end;

  if not DOMGetAttribute(Element, 'protoField', ProtoField) then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Missing "protoField" inside <connect> element');
    Exit;
  end;

  Result := true;
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
          OnWarning(wtMajor, 'VRML/X3D', Format('<connect> element "nodeField" doesn''t indicate any known field/event name: "%s"', [NodeField]));
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
  OnWarning and simply return @nil.

  @raises(EX3DXmlNotAllowedError On various not-allowed errors.)

  @raises(EX3DXmlUnknownNodeNotAllowed On a special parsing error:
    we got unknown node name, and AllowedNodes was @false.

    We have a special error class for this, because in some cases
    it means that actually the unknown node name could be also
    unknown field / proto etc. name, so error message for the user should
    be better.)
*)
function ParseXMLNode(Element: TDOMElement;
  out ContainerField: string; Names: TVRMLNames;
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
        Result := TVRMLExternalPrototype(Proto).ReferencedClass.Create(NodeName, Names.WWWBasePath) else
        Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, Names.WWWBasePath, Proto);

      Names.Nodes.Bind(Result, false);

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
              OnWarning(wtMajor, 'VRML/X3D', 'X3D XML: missing "name" attribute for <fieldValue> element');
              Continue;
            end;

            FieldIndex := Result.Fields.IndexOf(FieldName);
            if FieldIndex = -1 then
            begin
              OnWarning(wtMajor, 'VRML/X3D', Format('X3D XML: <fieldValue> element references unknown field name "%s"', [FieldName]));
              Continue;
            end;

            if DOMGetAttribute(ProtoIter.Current, 'value', FieldActualValue) then
              Result.Fields[FieldIndex].ParseXMLAttribute(FieldActualValue, Names) else
              Result.Fields[FieldIndex].ParseXMLElement(ProtoIter.Current, Names);

            Result.Fields[FieldIndex].PositionInParent := PositionInParent;
          end else
          if ProtoIter.Current.TagName = 'IS' then
          begin
            ParseISStatement(Result, ProtoIter.Current, PositionInParent);
          end else
          begin
            OnWarning(wtMajor, 'VRML/X3D', Format('X3D XML: only <fieldValue> or <IS> elements expected in prototype instantiation, but "%s" found', [ProtoIter.Current.TagName]));
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
          OnWarning(wtMajor, 'VRML/X3D', E.Message);
      end;
    end else
    begin
      NodeClass := NodesManager.NodeTypeNameToClass(NodeTypeName, Names.Version);
      if NodeClass <> nil then
      begin
        Result := NodeClass.Create(NodeName, Names.WWWBasePath);
        Names.Nodes.Bind(Result, false);
        Result.ParseXML(Element, Names);
      end else
      begin
        Result := TVRMLUnknownNode.CreateUnknown(NodeName, Names.WWWBasePath, NodeTypeName);

        { In classic VRML parser, we had special TVRMLUnknownNode.Parse
          that performed the "magic" trick of
          ParseIgnoreToMatchingCurlyBracket. This is not needed for
          X3D XML, we can simply omit the node by not looking
          at it's attributes. All we need to do is to make
          OnWarning warning. }

        OnWarning(wtMajor, 'VRML/X3D', 'Unknown X3D node type "' + NodeTypeName + '"');
      end;
    end;

    Names.Nodes.Bind(Result, true);
  end;

var
  NodeName, S: string;
  UsedNodeFinished: boolean;
begin
  Result := nil;
  try
    if DOMGetAttribute(Element, 'USE', NodeName) then
    begin
      { get appropriate node }
      Result := Names.Nodes.Bound(NodeName, UsedNodeFinished);
      if (Result = nil) or (not UsedNodeFinished) then
      begin
        if Result = nil then
          S := Format('Incorrect USE clause: node name "%s" undefined', [NodeName]) else
        begin
          S := Format('Cycles in VRML/X3D graph: USE clause inside node "%s" refers to the same node', [NodeName]);
          Result := nil; { return nil in case of cycles }
        end;
        if NilIfUnresolvedUSE then
          OnWarning(wtMajor, 'VRML/X3D', S) else
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

      We either use DefaultContainerField, or explicit containerField value.
      Note that containerField doesn't have to be preserved
      (see demo_models/x3d/container_field_def_use.x3d).
      Each USE occurence must specify suitable containerField or use class default. }
    if Result <> nil then
      ContainerField := Result.DefaultContainerField else
      ContainerField := ''; { will not be used anyway }
    DOMGetAttribute(Element, SAttrContainerField, ContainerField);

  except FreeAndNil(Result); raise end;
end;

{ This parses a sequence of X3D statements: any number of nodes,
  (external) protypes, routes. This is good to use to parse whole VRML file,
  or a (non-external) prototype content.

  It's somewhat similar to classic VRML ParseVRMLStatements.
  (Admittedly, it was even implemented by copying and modifying
  classic ParseVRMLStatements :) ). }
function ParseVRMLStatements(Element: TDOMElement;
  ParseX3DHeader: boolean;
  X3DHeaderElement: TDOMElement; Names: TVRMLNames): TVRMLRootNode;
var
  PositionInParent: Integer;

  { Create root group node. }
  function CreateRootNode: TVRMLRootNode;
  begin
    Result := TVRMLRootNode.Create('', Names.WWWBasePath);
    Result.HasForceVersion := true;
    Result.ForceVersion := Names.Version;
  end;

  procedure ParseProfile;
  var
    Profile: string;
  begin
    { parse "profile" attribute }
    if DOMGetAttribute(X3DHeaderElement, 'profile', Profile) then
    begin
      Result.Profile := Profile;
    end else
      { We allow PROFILE to be omitted.
        Actually, we do not use profile for anything right now. }
      OnWarning(wtMajor, 'VRML/X3D', 'X3D "profile" attribute missing');
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
          Result.Meta[MetaName] := MetaContent;
        end else
        if I.Current.TagName = 'component' then
        begin
          if DOMGetAttribute(I.Current, 'name', ComponentName) then
          begin
            if not DOMGetIntegerAttribute(I.Current, 'level', ComponentLevel) then
              ComponentLevel := 1;
            Result.Components[ComponentName] := ComponentLevel;
          end else
            OnWarning(wtMajor, 'VRML/X3D', Format('X3D XML: <component> element without required "name" attribute',
              [I.Current.TagName]));
        end else
          OnWarning(wtMajor, 'VRML/X3D', Format('X3D XML: unrecognized element "%s" in <head>',
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

      Proto.ParseXML(Element, Names);
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
      NewNode := ParseXMLNode(Element, ContainerFieldDummy, Names, false);
      NewNode.PositionInParent := PositionInParent;
      Result.FdChildren.Add(NewNode);
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

function LoadX3DXml(const FileName: string; Gzipped: boolean): TVRMLRootNode;
var
  Stream: TStream;
begin
  if Gzipped then
    Stream := TGZFileStream.Create(FileName, gzOpenRead) else
    { Create the same stream class as ReadXMLFile does
      when called with FileName:string }
    Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    Result := LoadX3DXml(Stream, ExtractFilePath(ExpandFileName(FileName)),
      ExtractFileName(FileName));
  finally
    FreeAndNil(Stream);
  end;
end;

function LoadX3DXml(Stream: TStream; const WWWBasePath: string;
  const BaseName: string): TVRMLRootNode;
var
  Doc: TXMLDocument;
  SceneElement: TDOMElement;
  VersionStr: string;
  Version: TVRMLVersion;
  { TODO: each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
  Names: TVRMLNames;
  FileName: string;
begin
  Doc := nil;
  try
    { calculate FileName. Used for ReadXMLFile, used there e.g. for error messages }
    if BaseName <> '' then
      FileName := CombinePaths(WWWBasePath, BaseName) else
      FileName := WWWBasePath;

    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }

    ReadXMLFile(Doc, Stream, FilenameToURI(FileName));

    Check(Doc.DocumentElement.TagName = 'X3D',
      'Root element of X3D file must be <X3D>');

    { parse "version" attribute }
    if DOMGetAttribute(Doc.DocumentElement, 'version', VersionStr) then
    begin
      DeFormat(VersionStr, '%d.%d', [@Version.Major, @Version.Minor]);
      if Version.Major < 3 then
      begin
        OnWarning(wtMajor, 'VRML/X3D', Format('X3D version number too low (%d.%d)', [Version.Major, Version.Minor]));
        Version := X3DVersion; { some sensible version number }
      end;
    end else
    begin
      Version := X3DVersion; { some sensible version number }
      OnWarning(wtMajor, 'VRML/X3D', Format('Missing X3D version number, assuming %d.%d', [Version.Major, Version.Minor]));
    end;

    SceneElement := DOMGetChildElement(Doc.DocumentElement, 'Scene', true);

    { X3D XML requires AutoRemove = true below }
    Names := TVRMLNames.Create(true, WWWBasePath, Version);
    try
      Result := ParseVRMLStatements(SceneElement, true, Doc.DocumentElement, Names);
      Names.ExtractNames(Result.FPrototypeNames, Result.FExportedNames);
    finally FreeAndNil(Names) end;
  finally
    FreeAndNil(Doc);
  end;
end;

{$endif read_implementation}
