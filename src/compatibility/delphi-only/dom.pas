{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Delphi XML classes.
  This unit is compatible with a subset of FPC DOM unit.

  Note that reading XML in Delphi depends on Windows-only MSXML,
  you can download it from
  https://www.microsoft.com/en-us/download/details.aspx?id=3988 .
}
unit DOM;

{$I castleconf.inc}

interface

uses XMLDoc, XMLIntf, Contnrs, Classes;

type
  DOMString = string;

  TDOMNodeList = class;
  TXMLDocument = class;

  { Node type.
    This type is not present in FPC DOM unit, instead ELEMENT_NODE
    and so on are numerical constants. }
  TXMLNodeType = (
    INTERNAL_RESERVED,
    ELEMENT_NODE,
    ATTRIBUTE_NODE,
    TEXT_NODE,
    CDATA_SECTION_NODE,
    ENTITY_REFERENCE_NODE,
    ENTITY_NODE,
    PROCESSING_INSTRUCTION_NODE,
    COMMENT_NODE,
    DOCUMENT_NODE,
    DOCUMENT_TYPE_NODE,
    DOCUMENT_FRAGMENT_NODE,
    NOTATION_NODE
  );

  { Node is a general concept in XML, it can be an element, attribute etc.

    We use TComponent to represent both TDOMNode and TXMLDocument,
    and all nodes are always owned by the document.
    Create nodes only by TXMLDocument methods like CreateElement, CreateComment. }
  TDOMNode = class(TComponent)
  private
    FOwnerDocument: TXMLDocument;
    InternalNode: IXMLNode;
  strict private
    FChildNodes: TDOMNodeList;
    function GetNodeName: String;
    function GetNodeValue: String;
    procedure SetNodeValue(const Value: String);
  public
    constructor Create(const AOwnerDocument: TXMLDocument; const AInternalNode: IXMLNode); reintroduce;
    destructor Destroy; override;
    property NodeName: String read GetNodeName;
    property NodeValue: String read GetNodeValue write SetNodeValue;
    property OwnerDocument: TXMLDocument read FOwnerDocument;
    function ChildNodes: TDOMNodeList;
    function NodeType: TXMLNodeType;
  end;

  TDOMNodeList = class
  strict private
    FOwnerDocument: TXMLDocument;
    InternalList: IXMLNodeList;
    Nodes: TObjectList;
    function GetCount: LongWord;
    function GetItem(const I: LongWord): TDOMNode;
  public
    constructor Create(const ANode: TDOMNode);
    destructor Destroy; override;
    property Item[const I: LongWord]: TDOMNode read GetItem; default;
    property Count: LongWord read GetCount;
    property Length: LongWord read GetCount;
  end;

  TDOMElement = class(TDOMNode)
    { Read from Element attribute value and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. Value is a "var", not "out" param,
      because in the latter case it's guaranteed that the old Value
      will not be cleared.

      Note that the returned Value may be empty, even when this returns @true,
      if the value is explicitly set to empty in XML (by @code(xxx="") in XML). }
    function AttributeString(const Name: String; var Value: String): Boolean; overload;

    procedure SetAttribute(const Name, Value: String);
    function TagName: String;
    procedure AppendChild(const Child: TDOMNode);
  end;

  TDOMCharacterData = class(TDOMNode)
    function Data: String;
  end;

  TDOMText = class(TDOMCharacterData)
  end;

  TDOMComment = class(TDOMCharacterData)
  end;

  TDOMCDATASection = class(TDOMText)
  end;

  TXMLDocument = class(TComponent)
  strict private
    FDocumentElement: TDOMElement;
    function GetDocumentElement: TDOMElement;
  public
    InternalDocument: IXMLDocument;
    constructor Create; reintroduce;
    destructor Destroy; override;
    property DocumentElement: TDOMElement read GetDocumentElement;
    function CreateElement(const Name: String): TDOMElement;
    function CreateComment(const CommentContents: String): TDOMComment;
    procedure AppendChild(const Child: TDOMNode);
  end;

implementation

uses SysUtils, ComObj;

{ TDOMNodeList --------------------------------------------------------------- }

constructor TDOMNodeList.Create(const ANode: TDOMNode);
begin
  inherited Create;
  FOwnerDocument := ANode.OwnerDocument;
  InternalList := ANode.InternalNode.ChildNodes;
  Nodes := TObjectList.Create(false);
end;

destructor TDOMNodeList.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TDOMNodeList.GetCount: LongWord;
begin
  Result := InternalList.Count;
end;

function TDOMNodeList.GetItem(const I: LongWord): TDOMNode;
var
  NewNode: TDOMNode;
begin
  // create on-demand TDOMNode wrapping InternalList[I]
  if Nodes.Count < Integer(I + 1) then
    Nodes.Count := I + 1;
  if Nodes[I] = nil then
  begin
    case InternalList[I].NodeType of
      ntElement: NewNode := TDOMElement.Create(FOwnerDocument, InternalList[I]);
      ntText   : NewNode := TDOMText.Create(FOwnerDocument, InternalList[I]);
      ntComment: NewNode := TDOMComment.Create(FOwnerDocument, InternalList[I]);
      ntCData  : NewNode := TDOMCDATASection.Create(FOwnerDocument, InternalList[I]);
      else       NewNode := TDOMNode.Create(FOwnerDocument, InternalList[I]);
    end;
    Nodes[I] := NewNode;
  end;
  Result := Nodes[I] as TDOMNode;
end;

{ TDOMNode ------------------------------------------------------------------- }

function TDOMNode.ChildNodes: TDOMNodeList;
begin
  if FChildNodes = nil then
    FChildNodes := TDOMNodeList.Create(Self);
  Result := FChildNodes;
end;

constructor TDOMNode.Create(const AOwnerDocument: TXMLDocument; const AInternalNode: IXMLNode);
begin
  inherited Create(OwnerDocument);
  FOwnerDocument := AOwnerDocument;
  InternalNode := AInternalNode;
end;

destructor TDOMNode.Destroy;
begin
  FreeAndNil(FChildNodes);
  inherited;
end;

function TDOMNode.GetNodeName: String;
begin
  Result := InternalNode.NodeName;
end;

function TDOMNode.GetNodeValue: String;
begin
  Result := InternalNode.NodeValue;
end;

function TDOMNode.NodeType: TXMLNodeType;
const
  NodeTypeMap: array [XMLIntf.TNodeType] of TXMLNodeType = (
    INTERNAL_RESERVED,
    ELEMENT_NODE,
    ATTRIBUTE_NODE,
    TEXT_NODE,
    CDATA_SECTION_NODE,
    ENTITY_REFERENCE_NODE,
    ENTITY_NODE,
    PROCESSING_INSTRUCTION_NODE,
    COMMENT_NODE,
    DOCUMENT_NODE,
    DOCUMENT_TYPE_NODE,
    DOCUMENT_FRAGMENT_NODE,
    NOTATION_NODE
  );
begin
  Result := NodeTypeMap[InternalNode.NodeType];
end;

procedure TDOMNode.SetNodeValue(const Value: String);
begin
  InternalNode.NodeValue := Value;
end;

{ TDOMElement ---------------------------------------------------------------- }

procedure TDOMElement.AppendChild(const Child: TDOMNode);
begin
  InternalNode.ChildNodes.Add(Child.InternalNode);
end;

function TDOMElement.AttributeString(const Name: String; var Value: String): Boolean;
var
  ANodes: IXMLNodeList;
  Index: Integer;
begin
  ANodes := InternalNode.AttributeNodes;
  Index := ANodes.IndexOf(Name);
  Result := Index <> -1;
  if Result then
    Value := ANodes[Index].NodeValue;
end;

procedure TDOMElement.SetAttribute(const Name, Value: String);
begin
  InternalNode.Attributes[Name] := Value;
end;

function TDOMElement.TagName: String;
begin
  Result := NodeName;
end;

{ TDOMCharacterData --------------------------------------------------------- }

function TDOMCharacterData.Data: String;
begin
  Result := NodeValue;
end;

{ TXMLDocument --------------------------------------------------------------- }

procedure TXMLDocument.AppendChild(const Child: TDOMNode);
begin
  DocumentElement.AppendChild(Child);
end;

constructor TXMLDocument.Create;
begin
  inherited Create(nil);

  { This needs to be called before using COM interfaces with MSXML. }
  CoInitializeEx(nil, 0);
end;

function TXMLDocument.CreateComment(const CommentContents: String): TDOMComment;
begin
  Result := TDOMComment.Create(Self, InternalDocument.CreateNode(CommentContents, ntComment));
end;

function TXMLDocument.CreateElement(const Name: String): TDOMElement;
begin
  Result := TDOMElement.Create(Self, InternalDocument.CreateElement(Name, ''));
end;

destructor TXMLDocument.Destroy;
begin
  inherited;
end;

function TXMLDocument.GetDocumentElement: TDOMElement;
begin
  if FDocumentElement = nil then
    FDocumentElement := TDOMElement.Create(Self, InternalDocument.DocumentElement);
  Result := FDocumentElement;
end;

end.

