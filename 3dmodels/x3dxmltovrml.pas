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

function LoadX3DXmlAsVRML(const FileName: string): TVRMLNode;

implementation

uses SysUtils, DOM, XMLRead, KambiUtils, KambiXMLUtils, Classes,
  VRMLLexer, VRMLErrors, VRMLFields;

type
  EX3DXmlNotAllowedError = class(EVRMLError);
  EX3DXmlUnknownNodeNotAllowed = class(EVRMLError);

{ TODO: this is a hasty implementation.
  I should read X3D spec about XML encoding once again, this time carefully
  adding handling all remaining bits to this reader.
  Like X3D "profile" attribute, the whole <head> element, etc.
}

function LoadX3DXmlAsVRML(const FileName: string): TVRMLNode;
var
  WWWBasePath: string;

const
  { X3D version numbers. }
  VRMLVerMajor = 3;
  VRMLVerMinor = 0;
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';

  function ParseNode(Element: TDOMElement;
    out ContainerField: string;
    NodeNameBinding: TStringList;
    NilIfUnresolvedUSE: boolean): TVRMLNode; forward;

  { Parse node body, i.e. mainly node's fields.
    This is roughly equivalent to TVRMLNode.Parse in classic VRML encoding
    parser. }
  procedure ParseNodeBody(Node: TVRMLNode;
    Element: TDOMElement;
    NodeNameBinding: TStringList);

    procedure ParseXMLAttributes;
    var
      Attr: TDOMAttr;
      AttrNode: TDOMNode;
      AttrIndex, Index: Integer;
      Lexer: TVRMLLexer;
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

        Index := Node.Fields.NameIndex(Attr.Name);
        if Index >= 0 then
        begin
          { Note that we allow here something more than X3D XML encoding:
            you can even specify SFNode, MFNode fields using
            classical VRML syntax, by specifying SFNode, MFNode fields
            as element attributes. }

          { SFString has quite special interpretation, it's just attrib
            name. It would not be usefull trying to use TVRMLLexer here,
            it's easier just to handle this as a special case. }
          if Node.Fields[Index] is TSFString then
            TSFString(Node.Fields[Index]).Value := Attr.Value else
          begin
            Lexer := TVRMLLexer.CreateForPartialStream(Attr.Value, WWWBasePath,
              VRMLVerMajor, VRMLVerMinor);
            try
              try
                Node.Fields[Index].ParseX3DXmlAttr(Lexer);
              except
                on E: EVRMLParserError do
                begin
                  if Node.Fields[Index] is TMFString then
                  begin
                    { This is very common error, even in models from
                      http://www.web3d.org/x3d/content/examples/Basic/
                      Although specification clearly says that MFString
                      components should always be enclosed within double
                      quotes. We just do what Xj3D seems to do, that is
                      we handle this as a single string (producing a warning). }
                    VRMLNonFatalError('Error when parsing MFString field "' + Attr.Name + '" value, probably missing double quotes (treating as a single string): ' + E.Message);
                    TMFString(Node.Fields[Index]).Items.Count := 0;
                    TMFString(Node.Fields[Index]).Items.AppendItem(Attr.Value);
                  end else
                    VRMLNonFatalError('Error when parsing field "' + Attr.Name + '" value: ' + E.Message);
                end;
              end;
            finally FreeAndNil(Lexer) end;
          end;
        end else
          VRMLNonFatalError('Unknown X3D field name (unhandled X3D XML attribute) "' + Attr.Name + '" in node "' + Node.NodeTypeName + '"');
      end;
    end;

    procedure ParseXMLChildrenNodes;
    var
      ChildIndex, FieldIndex: Integer;
      ChildrenList: TDOMNodeList;
      ChildNode: TDOMNode;
      Child: TVRMLNode;
      ContainerField: string;
      SF: TSFNode;
      MF: TMFNode;
    begin
      ChildrenList := Element.ChildNodes;
      try
        for ChildIndex := 0 to ChildrenList.Count - 1 do
        begin
          ChildNode := ChildrenList.Item[ChildIndex];
          if ChildNode.NodeType = ELEMENT_NODE then
          begin
            Child := ParseNode(ChildNode as TDOMElement, ContainerField,
              NodeNameBinding, true);
            if Child <> nil then
            begin
              FieldIndex := Node.Fields.NameIndex(ContainerField);
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
      finally ChildrenList.Release; end;
    end;

  begin
    ParseXMLAttributes;
    ParseXMLChildrenNodes;

    { TODO from classic encoding:

      I := Events.IndexOf(Lexer.TokenName);
      if I >= 0 then
      begin
        Result := true;
        Lexer.NextToken;
        Events[I].Parse(Lexer);
      end else

      if Lexer.TokenIsKeyword(vkPROTO) then
      begin
        Result := true;

        Proto := TVRMLPrototype.Create;
        Prototypes.Add(Proto);
        Proto.Parse(Lexer);
      end else
      if Lexer.TokenIsKeyword(vkEXTERNPROTO) then
      begin
        Result := true;

        Proto := TVRMLExternalPrototype.Create;
        Prototypes.Add(Proto);
        Proto.Parse(Lexer);
      end else
      if Lexer.TokenIsKeyword(vkROUTE) then
      begin
        Result := true;

        Route := TVRMLRoute.Create;
        Routes.Add(Route);
        Route.Parse(Lexer);
      end; }

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
    NodeNameBinding: TStringList;
    NilIfUnresolvedUSE: boolean): TVRMLNode;

    procedure ParseNamedNode(const NodeName: string);
    var
      NodeClass: TVRMLNodeClass;
      NodeTypeName: string;
      //ProtoIndex: Integer;
      //Proto: TVRMLPrototypeBase;
    begin
      NodeTypeName := Element.TagName;

      NodeClass := NodesManager.NodeTypeNameToClass(NodeTypeName,
        VRMLVerMajor, VRMLVerMinor);
      if NodeClass <> nil then
      begin
        Result := NodeClass.Create(NodeName, WWWBasePath);
      end else
      begin
        {}{TODO:
        ProtoIndex := Lexer.ProtoNameBinding.IndexOf(NodeTypeName);
        if ProtoIndex <> -1 then
        begin
          Proto := Lexer.ProtoNameBinding.Objects[ProtoIndex] as TVRMLPrototypeBase;
          if (Proto is TVRMLExternalPrototype) and
             (TVRMLExternalPrototype(Proto).ReferencedClass <> nil) then
            Result := TVRMLExternalPrototype(Proto).ReferencedClass.Create(NodeName, WWWBasePath) else
            Result := TVRMLPrototypeNode.CreatePrototypeNode(NodeName, WWWBasePath, Proto);
        end else}
        begin
          Result := TNodeUnknown.CreateUnknown(NodeName, WWWBasePath, NodeTypeName);
        end;
      end;

      ParseNodeBody(Result, Element, NodeNameBinding);

      {}(*TODO:

      if Result is TVRMLPrototypeNode then
      try
        Result := TVRMLPrototypeNode(Result).Instantiate;
      except
        on E: EX3DXmlPrototypeInstantiateError do
          { Just write E.Message and silence the exception.
            Result will simply remain as TVRMLPrototypeNode instance in this case. }
          VRMLNonFatalError(E.Message);
      end;*)

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

  { This parses a sequence of X3D statements: any number of nodes,
    (external) protypes, routes. This is good to use to parse whole VRML file,
    or a (non-external) prototype content.

    It's somewhat similar to classic VRML ParseVRMLStatements.
    (Admittedly, it was even implemented by copying and modifying
    classic ParseVRMLStatements :) ).

    Returns a single VRML node. If there was exactly one statement
    and it was a node statement, returns this node. Otherwise,
    returns everything read wrapped in artifical TNodeGroupHidden_2 instance. }
  function ParseVRMLStatements(Element: TDOMElement;
    NodeNameBinding: TStringList): TVRMLNode;

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
      {var
        Route: TVRMLRoute;}
      begin
        { TODO:
        Route := TVRMLRoute.Create;

        MakeResultHiddenGroup;
        Result.Routes.Add(Route);

        Route.Parse(Lexer);
        }
      end;

      { You can safely assume that Element.TagName
        indicates proto or externproto. }
      procedure ParseProtoStatement;
      {var
        Proto: TVRMLPrototypeBase;}
      begin
        { TODO:
        if Lexer.TokenKeyword = vkPROTO then
          Proto := TVRMLPrototype.Create else
          Proto := TVRMLExternalPrototype.Create;

        MakeResultHiddenGroup;
        Result.Prototypes.Add(Proto);

        Proto.Parse(Lexer);
        }
      end;

      procedure ParseNodeInternal;
      var
        NewNode: TVRMLNode;
        ContainerFieldDummy: string;
      begin
        NewNode := ParseNode(Element, ContainerFieldDummy,
          NodeNameBinding, false);

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
    I: Integer;
    DocChildren: TDOMNodeList;
    ChildNode: TDOMNode;
  begin
    Result := nil;
    try
      DocChildren := Element.ChildNodes;
      try
        for I := 0 to DocChildren.Count - 1 do
        begin
          ChildNode := DocChildren.Item[I];
          if ChildNode.NodeType = ELEMENT_NODE then
            ParseVRMLStatement(ChildNode as TDOMElement);
        end;
      finally DocChildren.Release; end;

      if Result = nil then
        Result := CreateHiddenGroup;
    except FreeAndNil(Result); raise end;
  end;

var
  Doc: TXMLDocument;
  SceneElement: TDOMElement;

  { This is used the same way as Lexer.NodeNameBinding when
    reading VRML files (that is, in classic VRML encoding).

    TODO: this means that each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
  NodeNameBinding: TStringList;
begin
  WWWBasePath := ExtractFilePath(FileName);

  NodeNameBinding := TStringList.Create;
  try
    ReadXMLFile(Doc, FileName);
    try
      Check(Doc.DocumentElement.TagName = 'X3D',
        'Root node of X3D file must be <X3D>');

      SceneElement := DOMGetChildElement(Doc.DocumentElement, 'Scene', true);
      Result := ParseVRMLStatements(SceneElement, NodeNameBinding);
    finally FreeAndNil(Doc) end;
  finally FreeAndNil(NodeNameBinding) end;
end;

end.
