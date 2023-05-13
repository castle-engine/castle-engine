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

uses XMLDoc, XMLIntf, Contnrs, Classes, SysUtils;

const


  // DOM Level 1 exception codes:

  INDEX_SIZE_ERR              = 1;  // index or size is negative, or greater than the allowed value
  DOMSTRING_SIZE_ERR          = 2;  // Specified range of text does not fit into a DOMString
  HIERARCHY_REQUEST_ERR       = 3;  // node is inserted somewhere it does not belong
  WRONG_DOCUMENT_ERR          = 4;  // node is used in a different document than the one that created it (that does not support it)
  INVALID_CHARACTER_ERR       = 5;  // invalid or illegal character is specified, such as in a name
  NO_DATA_ALLOWED_ERR         = 6;  // data is specified for a node which does not support data
  NO_MODIFICATION_ALLOWED_ERR = 7;  // an attempt is made to modify an object where modifications are not allowed
  NOT_FOUND_ERR               = 8;  // an attempt is made to reference a node in a context where it does not exist
  NOT_SUPPORTED_ERR           = 9;  // implementation does not support the type of object requested
  INUSE_ATTRIBUTE_ERR         = 10;  // an attempt is made to add an attribute that is already in use elsewhere

  // DOM Level 2 exception codes:

  INVALID_STATE_ERR           = 11;  // an attempt is made to use an object that is not, or is no longer, usable
  SYNTAX_ERR                  = 12;  // invalid or illegal string specified
  INVALID_MODIFICATION_ERR    = 13;  // an attempt is made to modify the type of the underlying object
  NAMESPACE_ERR               = 14;  // an attempt is made to create or change an object in a way which is incorrect with regard to namespaces
  INVALID_ACCESS_ERR          = 15;  // parameter or operation is not supported by the underlying object


type
  DOMString = string;
  TDOMNode = class;
  TDOMAttr = class;
  TDOMNodeList = class;
  TDOMDocument = class;

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


  EDOMError = class(Exception)
  public
    Code: Integer;
    constructor Create(ACode: Integer; const ASituation: String);
  end;

  EDOMHierarchyRequest = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotFound = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  TDOMNamedNodeMap = class
  strict private
    FOwnerDocument: TDOMDocument;
    FOwnerNode: TDOMNode;
    Nodes: TObjectList;
    InternalList: IXMLNodeList;

    function GetItem(const I: LongWord): TDOMNode;
    function GetLength: LongWord;
  private
    function InternalRemove(const Name: String): TDOMNode;
  public
    constructor Create(OwnerNode: TDOMNode);
    destructor Destroy;override;
    function GetNamedItem(Name: String): TDOMNode;

    property Item[const Index: LongWord]: TDOMNode read GetItem; default;
    property Length: LongWord read GetLength;
    procedure InvalidateMap;
  end;

  { Node is a general concept in XML, it can be an element, attribute etc.

    We use TComponent to represent both TDOMNode and TXMLDocument,
    and all nodes are always owned by the document.
    Create nodes only by TXMLDocument methods like CreateElement, CreateComment. }
  TDOMNode = class(TComponent)
  private
    FOwnerDocument: TDOMDocument;
    InternalNode: IXMLNode;
    FParentNode: TDOMNode;
    function GetNodeName: String;

  strict private
    FChildNodes: TDOMNodeList;
    function GetNodeValue: String;
    procedure SetNodeValue(const Value: String);
    function  GetFirstChild: TDOMNode; virtual;
  protected
    procedure InvalidateParent;
    function GetAttributes: TDOMNamedNodeMap; virtual;
    function GetParentNode: TDOMNode; virtual;
    function GetTextContent: String; virtual;
    procedure SetTextContent(const Value: String);virtual;
  public
    constructor Create(const AOwnerDocument: TDOMDocument; const AInternalNode: IXMLNode); reintroduce;
    destructor Destroy; override;
    property NodeName: String read GetNodeName;
    property NodeValue: String read GetNodeValue write SetNodeValue;
    property OwnerDocument: TDOMDocument read FOwnerDocument;
    function ChildNodes: TDOMNodeList;
    function NodeType: TXMLNodeType;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function DetachChild(Child: TDOMNode): TDOMNode;
    function RemoveChild(Child: TDOMNode): TDOMNode;
    procedure AppendChild(const Child: TDOMNode); virtual;
    function FindNode(const NodeName: String): TDOMNode;

    property Attributes: TDOMNamedNodeMap read GetAttributes;
    property ParentNode: TDOMNode read GetParentNode;
    property FirstChild: TDOMNode read GetFirstChild;

    property TextContent: String read GetTextContent write SetTextContent;
  end;

  TDOMNodeList = class
  strict private
    FOwnerDocument: TDOMDocument;
    InternalList: IXMLNodeList;
    Nodes: TObjectList;
    function GetCount: LongWord;
    function GetItem(const I: LongWord): TDOMNode;
  private
    procedure RemoveNode(const Child: TDOMNode);
  public
    constructor Create(const ANode: TDOMNode);
    destructor Destroy; override;
    property Item[const I: LongWord]: TDOMNode read GetItem; default;
    property Count: LongWord read GetCount;
    property Length: LongWord read GetCount;
  end;

  TDOMElement = class(TDOMNode)
  strict private
    FAttributes: TDOMNamedNodeMap;
  protected
    function GetAttributes: TDOMNamedNodeMap; override;

  public
    { Read from Element attribute value and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. Value is a "var", not "out" param,
      because in the latter case it's guaranteed that the old Value
      will not be cleared.

      Note that the returned Value may be empty, even when this returns @true,
      if the value is explicitly set to empty in XML (by @code(xxx="") in XML). }
    function AttributeString(const Name: String; var Value: String): Boolean; overload;

    function  GetAttribute(const name: String): String;
    procedure SetAttribute(const Name, Value: String);
    function  GetAttributeNode(const Name: String): TDOMAttr;
    procedure RemoveAttribute(const Name: String);
    function HasAttribute(const Name: String): Boolean;

    function TagName: String;

    procedure AppendChild(const Child: TDOMNode); override;

    property AttribStrings[const Name: String]: String
      read GetAttribute write SetAttribute; default;
  end;

  TDOMCharacterData = class(TDOMNode)
    function Data: String;
  end;

  TDOMAttr = class (TDOMNode)
  public
    property Name: String read GetNodeName;
  end;

  TDOMText = class(TDOMCharacterData)
  end;

  TDOMComment = class(TDOMCharacterData)
  end;

  TDOMCDATASection = class(TDOMText)
  end;

  TDOMDocument = class(TComponent)
  strict private
    FDocumentElement: TDOMElement;
    function GetDocumentElement: TDOMElement;
  public
    InternalDocument: IXMLDocument;
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);

    property DocumentElement: TDOMElement read GetDocumentElement;
    function CreateElement(const Name: String): TDOMElement;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
    function CreateComment(const CommentContents: String): TDOMComment;
    procedure AppendChild(const Child: TDOMNode);
  end;

  TXMLDocument = class(TDOMDocument)

  end;

implementation

{$ifdef MSWINDOWS}
uses ComObj, Xml.Win.msxmldom;
{$endif}

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

procedure TDOMNodeList.RemoveNode(const Child: TDOMNode);
begin
  Nodes.Remove(Child);
end;

{ TDOMNode ------------------------------------------------------------------- }

procedure TDOMNode.AppendChild(const Child: TDOMNode);
begin
  raise EDOMHierarchyRequest.Create('Node.AppendChild');
end;

function TDOMNode.ChildNodes: TDOMNodeList;
begin
  if FChildNodes = nil then
    FChildNodes := TDOMNodeList.Create(Self);
  Result := FChildNodes;
end;

constructor TDOMNode.Create(const AOwnerDocument: TDOMDocument; const AInternalNode: IXMLNode);
begin
  inherited Create(AOwnerDocument);
  FOwnerDocument := AOwnerDocument;
  FParentNode := nil;
  InternalNode := AInternalNode;
end;

destructor TDOMNode.Destroy;
begin
  FreeAndNil(FChildNodes);
  inherited;
end;

function TDOMNode.DetachChild(Child: TDOMNode): TDOMNode;
begin
  if Child.ParentNode <> Self then
    raise EDOMNotFound.Create('Node.RemoveChild');

  Child.InvalidateParent;

  InternalNode.ChildNodes.Remove(Child.InternalNode);
  ChildNodes.RemoveNode(Child);
  Result := Child;
end;

function TDOMNode.FindNode(const NodeName: String): TDOMNode;
var
  I: Integer;
begin
  for I := 0 to Integer(ChildNodes.Count) - 1 do
  begin
    if ChildNodes[I].NodeName = NodeName then
      Exit(ChildNodes[I]);
  end;
  Result := nil;
end;

function TDOMNode.GetAttributes: TDOMNamedNodeMap;
begin
  Result := nil;
end;

function TDOMNode.GetFirstChild: TDOMNode;
begin
  if FChildNodes = nil then
    Exit(nil);

  if FChildNodes.Count = 0 then
    Exit(nil);

  Result := FChildNodes.Item[0];
end;

function TDOMNode.GetNodeName: String;
begin
  Result := InternalNode.NodeName;
end;

function TDOMNode.GetNodeValue: String;
begin
  Result := InternalNode.NodeValue;
end;

function TDOMNode.GetParentNode: TDOMNode;
var
  InternalParentNode: IXMLNode;
  I: Integer;
  NodeToTest: TDOMNode;
begin
  InternalParentNode := InternalNode.ParentNode;

  if InternalParentNode = nil then
    Exit(nil);

  if FParentNode <> nil then
    Exit(FParentNode);

  for I := 0 to FOwnerDocument.ComponentCount -1 do
  begin
    if not (FOwnerDocument.Components[I] is TDOMNode) then
      continue;

    NodeToTest := TDOMNode(FOwnerDocument.Components[I]);

    if NodeToTest.InternalNode = InternalParentNode then
    begin
      FParentNode := NodeToTest;
      break;
    end;
  end;

  Result := FParentNode;
end;

function TDOMNode.GetTextContent: String;
begin
  Result := NodeValue;
end;

function TDOMNode.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
end;

procedure TDOMNode.InvalidateParent;
begin
  FParentNode := nil;
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

function TDOMNode.RemoveChild(Child: TDOMNode): TDOMNode;
begin
  Result := DetachChild(Child);
end;

procedure TDOMNode.SetNodeValue(const Value: String);
begin
  InternalNode.NodeValue := Value;
end;

procedure TDOMNode.SetTextContent(const Value: String);
begin
  SetNodeValue(Value);
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

function TDOMElement.GetAttribute(const name: String): String;
begin
  Result := InternalNode.Attributes[Name];
end;

function TDOMElement.GetAttributeNode(const Name: String): TDOMAttr;
begin
  Result := GetAttributes.GetNamedItem(Name) as TDOMAttr
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  if FAttributes = nil then
    FAttributes := TDOMNamedNodeMap.Create(Self);

  Result := FAttributes;
end;

function TDOMElement.HasAttribute(const Name: String): Boolean;
begin
  Result := InternalNode.HasAttribute(Name);
end;

procedure TDOMElement.RemoveAttribute(const Name: String);
var
  AttributeNode: IXMLNode;
begin
  AttributeNode := InternalNode.AttributeNodes.FindNode(Name);
  if Assigned(AttributeNode) then
  begin
    if Assigned(FAttributes) then
    begin
      FAttributes.InternalRemove(Name);
    end;

    InternalNode.AttributeNodes.Remove(AttributeNode);
  end;
end;

procedure TDOMElement.SetAttribute(const Name, Value: String);
begin
  InternalNode.Attributes[Name] := Value;
  if FAttributes <> nil then
    FAttributes.InvalidateMap;
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

{ TDOMDocument --------------------------------------------------------------- }

procedure TDOMDocument.AppendChild(const Child: TDOMNode);
begin
  { We support only one root element now. To change that we should
    use InternalDocument.DomDocument API and IDOMNode interface not
    IXMLNode. }

  if InternalDocument.DocumentElement = nil then
  begin
    if Child is TDOMElement then
    begin
      InternalDocument.DocumentElement := Child.InternalNode;
      FDocumentElement := TDOMElement(Child);
    end else
    if Child <> nil then
      raise EDOMHierarchyRequest.CreateFmt('TDOMDocument.AppendChild accepts only TDOMElement, but given %s', [
        Child.ClassName
      ]);
  end else
    raise Exception.Create('Delphi limitation: There can be only one root element!');
end;

constructor TDOMDocument.Create;
begin
  inherited Create(nil);

  {$ifdef MSWINDOWS}
  { This needs to be called before using COM interfaces with MSXML. }
  CoInitializeEx(nil, 0);
  {$endif}

  InternalDocument := XMLDoc.TXMLDocument.Create(Self);
  InternalDocument.Active := true;
  FDocumentElement := nil;
end;

function TDOMDocument.CreateComment(const CommentContents: String): TDOMComment;
begin

  Result := TDOMComment.Create(Self, InternalDocument.CreateNode(CommentContents, ntComment));
end;

function TDOMDocument.CreateElement(const Name: String): TDOMElement;
begin
  Result := TDOMElement.Create(Self, InternalDocument.CreateElement(Name, ''));
end;

destructor TDOMDocument.Destroy;
begin
  inherited;
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
begin
  Result := FDocumentElement;
end;

procedure TDOMDocument.LoadFromStream(Stream: TStream);
begin
  FreeAndNil(FDocumentElement);

  InternalDocument.LoadFromStream(Stream);

  if InternalDocument.DocumentElement <> nil then
  begin
    FDocumentElement := TDOMElement.Create(Self, InternalDocument.DocumentElement);
  end;
end;

function TDOMDocument.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  if (OldChild = FDocumentElement) and (NewChild is TDOMElement) then
  begin
    FDocumentElement := TDOMElement(NewChild);
    InternalDocument.DocumentElement := NewChild.InternalNode;

    Result := OldChild;
  end;

  Result := nil;
  // TODO: Exception? nil?
end;

{ TDOMNamedNodeMap ----------------------------------------------------------- }

constructor TDOMNamedNodeMap.Create(OwnerNode: TDOMNode);
begin
  inherited Create;
  FOwnerNode := OwnerNode;
  FOwnerDocument := FOwnerNode.FOwnerDocument;
  InternalList := FOwnerNode.InternalNode.AttributeNodes;

  Nodes := TObjectList.Create(false);
end;

destructor TDOMNamedNodeMap.Destroy;
begin

  inherited;
end;

function TDOMNamedNodeMap.GetItem(const I: LongWord): TDOMNode;
var
  NewNode: TDOMNode;
begin
  // create on-demand TDOMNode wrapping InternalList[I]
  if Nodes.Count < Integer(I + 1) then
    Nodes.Count := I + 1;
  if Nodes[I] = nil then
  begin
    case InternalList[I].NodeType of
      ntElement  : NewNode := TDOMElement.Create(FOwnerDocument, InternalList[I]);
      ntText     : NewNode := TDOMText.Create(FOwnerDocument, InternalList[I]);
      ntComment  : NewNode := TDOMComment.Create(FOwnerDocument, InternalList[I]);
      ntCData    : NewNode := TDOMCDATASection.Create(FOwnerDocument, InternalList[I]);
      ntAttribute: NewNode := TDOMAttr.Create(FOwnerDocument, InternalList[I]);
      else         NewNode := TDOMNode.Create(FOwnerDocument, InternalList[I]);
    end;
    Nodes[I] := NewNode;
    NewNode.FParentNode := FOwnerNode;
  end;
  Result := Nodes[I] as TDOMNode;
end;

function TDOMNamedNodeMap.GetLength: LongWord;
begin
  Result := InternalList.Count;
end;

function TDOMNamedNodeMap.GetNamedItem(Name: String): TDOMNode;
var
  I: Integer;
begin
  I := InternalList.IndexOf(Name);
  if I = -1 then
    Exit(nil);
  Result := GetItem(I);
end;

function TDOMNamedNodeMap.InternalRemove(const Name: String): TDOMNode;
var
  I: Integer;
begin
  I := InternalList.IndexOf(Name);
  if I = -1 then
    Exit(nil);

  Result := Nodes[I] as TDOMNode;
  Nodes.Delete(I);
end;

procedure TDOMNamedNodeMap.InvalidateMap;
begin
  // after adding a new attribute InternalNode.AttributeNodes returns
  // new IXMLNodeList with NEW nodes so we need change InternalList pointer
  // this solution is not perfect, previously retrieved attributes will
  // consume memory until the document is released

  InternalList := FOwnerNode.InternalNode.AttributeNodes;
  Nodes.Clear;
end;

{ EDOMError ------------------------------------------------------------------ }

constructor EDOMError.Create(ACode: Integer; const ASituation: String);
begin
  Code := ACode;
  inherited Create(Self.ClassName + ' in ' + ASituation);
end;

{ EDOMHierarchyRequest ------------------------------------------------------- }

constructor EDOMHierarchyRequest.Create(const ASituation: String);
begin
  inherited Create(HIERARCHY_REQUEST_ERR, ASituation);
end;

{ EDOMNotFound --------------------------------------------------------------- }

constructor EDOMNotFound.Create(const ASituation: String);
begin
  inherited Create(NOT_FOUND_ERR, ASituation);
end;

initialization
  { Reading X3D XML fails without this.
    See https://bobsotherblog.wordpress.com/2013/09/19/fixing-dtd-is-prohibited-error-in-delphi/
    https://docwiki.embarcadero.com/Libraries/Sydney/en/Xml.Win.msxmldom.MSXML6_ProhibitDTD }
  Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
end.

