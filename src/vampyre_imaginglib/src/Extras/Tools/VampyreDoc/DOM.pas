{
    This file is part of the Free Component Library

    Implementation of DOM interfaces
    Copyright (c) 1999-2003 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This unit provides classes which implement the interfaces defined in the
  DOM (Document Object Model) specification.
  The current state is:
  DOM Level 1  -  Almost completely implemented
  DOM Level 2  -  Partially implemented


  Specification used for this implementation:

  "Document Object Model (DOM) Level 2 Specification Version 1.0
   W3C Candidate Recommendation 07 March, 2000"
  http://www.w3.org/TR/2000/CR-DOM-Level-2-20000307
}


unit DOM;

interface

uses SysUtils, Classes;

type

// -------------------------------------------------------
//   DOMString
// -------------------------------------------------------

{$IFDEF ver1_0}
  DOMString = String;
{$ELSE}
  DOMString = WideString;
{$ENDIF}


// -------------------------------------------------------
//   DOMException
// -------------------------------------------------------

const

  // DOM Level 1 exception codes:

  INDEX_SIZE_ERR              = 1;      // index or size is negative, or greater than the allowed value
  DOMSTRING_SIZE_ERR          = 2;      // Specified range of text does not fit into a DOMString
  HIERARCHY_REQUEST_ERR       = 3;      // node is inserted somewhere it does not belong
  WRONG_DOCUMENT_ERR          = 4;      // node is used in a different document than the one that created it (that does not support it)
  INVALID_CHARACTER_ERR       = 5;      // invalid or illegal character is specified, such as in a name
  NO_DATA_ALLOWED_ERR         = 6;      // data is specified for a node which does not support data
  NO_MODIFICATION_ALLOWED_ERR = 7;      // an attempt is made to modify an object where modifications are not allowed
  NOT_FOUND_ERR               = 8;      // an attempt is made to reference a node in a context where it does not exist
  NOT_SUPPORTED_ERR           = 9;      // implementation does not support the type of object requested
  INUSE_ATTRIBUTE_ERR         = 10;     // an attempt is made to add an attribute that is already in use elsewhere

  // DOM Level 2 exception codes:

  INVALID_STATE_ERR           = 11;     // an attempt is made to use an object that is not, or is no longer, usable
  SYNTAX_ERR                  = 12;     // invalid or illegal string specified
  INVALID_MODIFICATION_ERR    = 13;     // an attempt is made to modify the type of the underlying object
  NAMESPACE_ERR               = 14;     // an attempt is made to create or change an object in a way which is incorrect with regard to namespaces
  INVALID_ACCESS_ERR          = 15;     // parameter or operation is not supported by the underlying object


type

  EDOMError = class(Exception)
  protected
    constructor Create(ACode: Integer; const ASituation: String);
  public
    Code: Integer;
  end;

  EDOMIndexSize = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMHierarchyRequest = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMWrongDocument = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotFound = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotSupported = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInUseAttribute = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidState = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMSyntax = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidModification = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNamespace = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidAccess = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

const

  ELEMENT_NODE = 1;
  ATTRIBUTE_NODE = 2;
  TEXT_NODE = 3;
  CDATA_SECTION_NODE = 4;
  ENTITY_REFERENCE_NODE = 5;
  ENTITY_NODE = 6;
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE = 8;
  DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE = 12;


type

  TDOMImplementation = class;
  TDOMDocumentFragment = class;
  TDOMDocument = class;
  TDOMNode = class;
  TDOMNodeList = class;
  TDOMNamedNodeMap = class;
  TDOMCharacterData = class;
  TDOMAttr = class;
  TDOMElement = class;
  TDOMText = class;
  TDOMComment = class;
  TDOMCDATASection = class;
  TDOMDocumentType = class;
  TDOMNotation = class;
  TDOMEntity = class;
  TDOMEntityReference = class;
  TDOMProcessingInstruction = class;

  TRefClass = class
  protected
    RefCounter: LongInt;
  public
    constructor Create;
    function AddRef: LongInt; virtual;
    function Release: LongInt; virtual;
  end;

  TDOMNode = class
  protected
    FNodeName, FNodeValue: DOMString;
    FNodeType: Integer;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  GetNodeValue: DOMString; virtual;
    procedure SetNodeValue(AValue: DOMString); virtual;
    function  GetFirstChild: TDOMNode; virtual;
    function  GetLastChild: TDOMNode; virtual;
    function  GetAttributes: TDOMNamedNodeMap; virtual;

    constructor Create(AOwner: TDOMDocument);
  public
    // Free NodeList with TDOMNodeList.Release!
    function GetChildNodes: TDOMNodeList; virtual;

    property NodeName: DOMString read FNodeName;
    property NodeValue: DOMString read GetNodeValue write SetNodeValue;
    property NodeType: Integer read FNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read GetFirstChild;
    property LastChild: TDOMNode read GetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read GetAttributes;
    property OwnerDocument: TDOMDocument read FOwnerDocument;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; virtual;
    function AppendChild(NewChild: TDOMNode): TDOMNode; virtual;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode; overload;

    // Extensions to DOM interface:
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; virtual;
    function FindNode(const ANodeName: DOMString): TDOMNode;
  end;


  { The following class is an implementation specific extension, it is just an
    extended implementation of TDOMNode, the generic DOM::Node interface
    implementation. (Its main purpose is to save memory in a big node tree) }

  TDOMNode_WithChildren = class(TDOMNode)
  protected
    FFirstChild, FLastChild: TDOMNode;
    function GetFirstChild: TDOMNode; override;
    function GetLastChild: TDOMNode; override;
    procedure CloneChildren(ACopy: TDOMNode; ACloneOwner: TDOMDocument);
  public
    destructor Destroy; override;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; override;
    function AppendChild(NewChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
  end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TDOMNodeList = class(TRefClass)
  protected
    node: TDOMNode;
    filter: DOMString;
    UseFilter: Boolean;
    constructor Create(ANode: TDOMNode; AFilter: DOMString);
    function GetCount: LongInt;
    function GetItem(index: LongWord): TDOMNode;
  public
    property Item[index: LongWord]: TDOMNode read GetItem;
    property Count: LongInt read GetCount;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  TDOMNamedNodeMap = class(TList)
  protected
    OwnerDocument: TDOMDocument;
    function GetItem(index: LongWord): TDOMNode;
    procedure SetItem(index: LongWord; AItem: TDOMNode);
    function GetLength: LongInt;

    constructor Create(AOwner: TDOMDocument);
  public
    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    property Item[index: LongWord]: TDOMNode read GetItem write SetItem; default;
    property Length: LongInt read GetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  protected
    function  GetLength: LongInt;
  public
    property Data: DOMString read FNodeValue;
    property Length: LongInt read GetLength;
    function SubstringData(offset, count: LongWord): DOMString;
    procedure AppendData(const arg: DOMString);
    procedure InsertData(offset: LongWord; const arg: DOMString);
    procedure DeleteData(offset, count: LongWord);
    procedure ReplaceData(offset, count: LongWord; const arg: DOMString);
  end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

  TDOMImplementation = class
  public
    function HasFeature(const feature, version: DOMString): Boolean;

    // Introduced in DOM Level 2:

    function CreateDocumentType(const QualifiedName, PublicID,
      SystemID: DOMString): TDOMDocumentType;
    function CreateDocument(const NamespaceURI, QualifiedName: DOMString;
      doctype: TDOMDocumentType): TDOMDocument;
  end;


// -------------------------------------------------------
//   DocumentFragment
// -------------------------------------------------------

  TDOMDocumentFragment = class(TDOMNode_WithChildren)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FDocType: TDOMDocumentType;
    FImplementation: TDOMImplementation;
    function GetDocumentElement: TDOMElement;
  public
    property DocType: TDOMDocumentType read FDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read GetDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateComment(const data: DOMString): TDOMComment;
    function CreateCDATASection(const data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateAttribute(const name: DOMString): TDOMAttr; virtual;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference;
      virtual;
    // Free NodeList with TDOMNodeList.Release!
    function GetElementsByTagName(const tagname: DOMString): TDOMNodeList;

    // Extensions to DOM interface:
    constructor Create; virtual;
    function CreateEntity(const data: DOMString): TDOMEntity;
  end;

  TXMLDocument = class(TDOMDocument)
  public
    // These fields are extensions to the DOM interface:
    XMLVersion, Encoding, StylesheetType, StylesheetHRef: DOMString;

    function CreateCDATASection(const data: DOMString): TDOMCDATASection; override;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; override;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference; override;
  end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TDOMAttr = class(TDOMNode_WithChildren)
  protected
    FSpecified: Boolean;
    AttrOwner: TDOMNamedNodeMap;
    function  GetNodeValue: DOMString; override;
    procedure SetNodeValue(AValue: DOMString); override;

    constructor Create(AOwner: TDOMDocument);
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
    property Name: DOMString read FNodeName;
    property Specified: Boolean read FSpecified;
    property Value: DOMString read GetNodeValue write SetNodeValue;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  TDOMElement = class(TDOMNode_WithChildren)
  protected
    FAttributes: TDOMNamedNodeMap;
    function GetAttributes: TDOMNamedNodeMap; override;

    constructor Create(AOwner: TDOMDocument); virtual;
  public
    destructor Destroy; override;
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
    property  TagName: DOMString read FNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    procedure SetAttributeNode(NewAttr: TDOMAttr);
    function  RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    // Free NodeList with TDOMNodeList.Release!
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;
    procedure Normalize;

    property AttribStrings[const Name: DOMString]: DOMString
      read GetAttribute write SetAttribute; default;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
    function SplitText(offset: LongWord): TDOMText;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FEntities, FNotations: TDOMNamedNodeMap;

    constructor Create(AOwner: TDOMDocument);
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
    property Name: DOMString read FNodeName;
    property Entities: TDOMNamedNodeMap read FEntities;
    property Notations: TDOMNamedNodeMap read FEntities;
  end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

  TDOMNotation = class(TDOMNode)
  protected
    FPublicID, FSystemID: DOMString;

    constructor Create(AOwner: TDOMDocument);
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
      overload; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
  end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

  TDOMEntity = class(TDOMNode_WithChildren)
  protected
    FPublicID, FSystemID, FNotationName: DOMString;

    constructor Create(AOwner: TDOMDocument);
  public
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property NotationName: DOMString read FNotationName;
  end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

  TDOMEntityReference = class(TDOMNode_WithChildren)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    property Target: DOMString read FNodeName;
    property Data: DOMString read FNodeValue;
  end;




// =======================================================
// =======================================================

implementation


constructor TRefClass.Create;
begin
  inherited Create;
  RefCounter := 1;
end;

function TRefClass.AddRef: LongInt;
begin
  Inc(RefCounter);
  Result := RefCounter;
end;

function TRefClass.Release: LongInt;
begin
  Dec(RefCounter);
  Result := RefCounter;
  if RefCounter <= 0 then Free;
end;


// -------------------------------------------------------
//   DOM Exception
// -------------------------------------------------------

constructor EDOMError.Create(ACode: Integer; const ASituation: String);
begin
  Code := ACode;
  inherited Create(Self.ClassName + ' in ' + ASituation);
end;

constructor EDOMIndexSize.Create(const ASituation: String);    // 1
begin
  inherited Create(INDEX_SIZE_ERR, ASituation);
end;

constructor EDOMHierarchyRequest.Create(const ASituation: String);    // 3
begin
  inherited Create(HIERARCHY_REQUEST_ERR, ASituation);
end;

constructor EDOMWrongDocument.Create(const ASituation: String);    // 4
begin
  inherited Create(WRONG_DOCUMENT_ERR, ASituation);
end;

constructor EDOMNotFound.Create(const ASituation: String);    // 8
begin
  inherited Create(NOT_FOUND_ERR, ASituation);
end;

constructor EDOMNotSupported.Create(const ASituation: String);    // 9
begin
  inherited Create(NOT_SUPPORTED_ERR, ASituation);
end;

constructor EDOMInUseAttribute.Create(const ASituation: String);    // 10
begin
  inherited Create(INUSE_ATTRIBUTE_ERR, ASituation);
end;

constructor EDOMInvalidState.Create(const ASituation: String);    // 11
begin
  inherited Create(INVALID_STATE_ERR, ASituation);
end;

constructor EDOMSyntax.Create(const ASituation: String);    // 12
begin
  inherited Create(SYNTAX_ERR, ASituation);
end;

constructor EDOMInvalidModification.Create(const ASituation: String);    // 13
begin
  inherited Create(INVALID_MODIFICATION_ERR, ASituation);
end;

constructor EDOMNamespace.Create(const ASituation: String);    // 14
begin
  inherited Create(NAMESPACE_ERR, ASituation);
end;

constructor EDOMInvalidAccess.Create(const ASituation: String);    // 15
begin
  inherited Create(INVALID_ACCESS_ERR, ASituation);
end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

constructor TDOMNode.Create(AOwner: TDOMDocument);
begin
  FOwnerDocument := AOwner;
  inherited Create;
end;

function TDOMNode.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMNode.SetNodeValue(AValue: DOMString);
begin
  FNodeValue := AValue;
end;

function TDOMNode.GetChildNodes: TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, '*');
end;

function TDOMNode.GetFirstChild: TDOMNode; begin Result := nil end;
function TDOMNode.GetLastChild: TDOMNode; begin Result := nil end;
function TDOMNode.GetAttributes: TDOMNamedNodeMap; begin Result := nil end;

function TDOMNode.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.RemoveChild');
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.AppendChild');
end;

function TDOMNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

function TDOMNode.CloneNode(deep: Boolean): TDOMNode;
begin
  Result:=CloneNode(deep, FOwnerDocument);
end;

function TDOMNode.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  raise EDOMNotSupported.Create('CloneNode not implemented for ' + ClassName);
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
var
  child: TDOMNode;
begin
  child := FirstChild;
  while Assigned(child) do
  begin
    if child.NodeName = ANodeName then
    begin
      Result := child;
      exit;
    end;
    child := child.NextSibling;
  end;
  Result := nil;
end;


function TDOMNode_WithChildren.GetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.GetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

destructor TDOMNode_WithChildren.Destroy;
var
  child, next: TDOMNode;
begin
  child := FirstChild;
  while Assigned(child) do
  begin
    next := child.NextSibling;
    child.Free;
    child := next;
  end;
  inherited Destroy;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
var
  i: Integer;
begin
  Result := NewChild;

  if not Assigned(RefChild) then
  begin
    AppendChild(NewChild);
    exit;
  end;

  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.InsertBefore');

  if RefChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('NodeWC.InsertBefore for DocumentFragment');

  NewChild.FNextSibling := RefChild;
  if RefChild = FFirstChild then
    FFirstChild := NewChild
  else
  begin
    RefChild.FPreviousSibling.FNextSibling := NewChild;
    NewChild.FPreviousSibling := RefChild.FPreviousSibling;
  end;

  RefChild.FPreviousSibling := NewChild;
  NewChild.FParentNode := Self;
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
  InsertBefore(NewChild, OldChild);
  if Assigned(OldChild) then
    RemoveChild(OldChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.RemoveChild(OldChild: TDOMNode):
  TDOMNode;
begin
  if OldChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.RemoveChild');

  if OldChild = FFirstChild then
    FFirstChild := FFirstChild.NextSibling
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := FLastChild.FPreviousSibling
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;

  OldChild.Free;
end;

function TDOMNode_WithChildren.AppendChild(NewChild: TDOMNode): TDOMNode;
var
  Parent: TDOMNode;
begin
  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.AppendChild');

  Parent := Self;
  while Assigned(Parent) do
  begin
    if Parent = NewChild then
      raise EDOMHierarchyRequest.Create('NodeWC.AppendChild (cycle in tree)');
    Parent := Parent.ParentNode;
  end;

  if NewChild.FParentNode = Self then
    RemoveChild(NewChild);

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('NodeWC.AppendChild for DocumentFragments')
  else begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end else
      FFirstChild := NewChild;
    FLastChild := NewChild;
    NewChild.FParentNode := Self;
  end;
  Result := NewChild;
end;

function TDOMNode_WithChildren.HasChildNodes: Boolean;
begin
  Result := Assigned(FFirstChild);
end;

procedure TDOMNode_WithChildren.CloneChildren(ACopy: TDOMNode; ACloneOwner: TDOMDocument);
var
  node: TDOMNode;
begin
  node := FirstChild;
  while Assigned(node) do
  begin
    ACopy.AppendChild(node.CloneNode(True, ACloneOwner));
    node := node.NextSibling;
  end;
end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

constructor TDOMNodeList.Create(ANode: TDOMNode; AFilter: DOMString);
begin
  inherited Create;
  node := ANode;
  filter := AFilter;
  UseFilter := filter <> '*';
end;

function TDOMNodeList.GetCount: LongInt;
var
  child: TDOMNode;
begin
  Result := 0;
  child := node.FirstChild;
  while Assigned(child) do
  begin
    if (not UseFilter) or (child.NodeName = filter) then
      Inc(Result);
    child := child.NextSibling;
  end;
end;

function TDOMNodeList.GetItem(index: LongWord): TDOMNode;
var
  child: TDOMNode;
begin
  Result := nil;
  if index < 0 then
    exit;
  child := node.FirstChild;
  while Assigned(child) do
  begin
    if index = 0 then
    begin
      Result := child;
      break;
    end;
    if (not UseFilter) or (child.NodeName = filter) then
      Dec(index);
    child := child.NextSibling;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMDocument);
begin
  inherited Create;
  OwnerDocument := AOwner;
end;

function TDOMNamedNodeMap.GetItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(Items[index]);
end;

procedure TDOMNamedNodeMap.SetItem(index: LongWord; AItem: TDOMNode);
begin
  Items[index] := AItem;
end;

function TDOMNamedNodeMap.GetLength: LongInt;
begin
  Result := Count;
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Item[i];
    if Result.NodeName = name then
      exit;
  end;
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Integer;
begin
  if arg.FOwnerDocument <> OwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItem');

  if arg.NodeType = ATTRIBUTE_NODE then
  begin
    if Assigned(TDOMAttr(arg).AttrOwner) then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItem');
    TDOMAttr(arg).AttrOwner := Self;
  end;

  for i := 0 to Count - 1 do
    if Item[i].NodeName = arg.NodeName then
    begin
      Result := Item[i];
      Item[i] := arg;
      exit;
    end;
  Add(arg);
  Result := nil;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Item[i].NodeName = name then
    begin
      Result := Item[i];
      Result.FParentNode := nil;
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

function TDOMCharacterData.GetLength: LongInt;
begin
  Result := system.Length(FNodeValue);
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if (offset < 0) or (offset > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  if (offset < 0) or (offset > Length) then
    raise EDOMIndexSize.Create('CharacterData.InsertData');

  FNodeValue := Copy(FNodeValue, 1, offset) + arg +
    Copy(FNodeValue, offset + 1, Length);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  if (offset < 0) or (offset > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');

  FNodeValue := Copy(FNodeValue, 1, offset) +
    Copy(FNodeValue, offset + count + 1, Length);
end;

procedure TDOMCharacterData.ReplaceData(offset, count: LongWord; const arg: DOMString);
begin
  DeleteData(offset, count);
  InsertData(offset, arg);
end;


// -------------------------------------------------------
//   DocumentFragmet
// -------------------------------------------------------

constructor TDOMDocumentFragment.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_FRAGMENT_NODE;
  FNodeName := '#document-fragment';
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
begin
  Result := False;
end;

function TDOMImplementation.CreateDocumentType(const QualifiedName, PublicID,
  SystemID: DOMString): TDOMDocumentType;
begin
  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocumentType');
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName: DOMString; doctype: TDOMDocumentType): TDOMDocument;
begin
  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocument');
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  FNodeType := DOCUMENT_NODE;
  FNodeName := '#document';
  inherited Create(nil);
  FOwnerDocument := Self;
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) do
  begin
    if node.FNodeType = ELEMENT_NODE then
    begin
      Result := TDOMElement(node);
      exit;
    end;
    node := node.NextSibling;
  end;
  Result := nil;
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  Result.FNodeName := tagName;
end;

function TDOMDocument.CreateDocumentFragment: TDOMDocumentFragment;
begin
  Result := TDOMDocumentFragment.Create(Self);
end;

function TDOMDocument.CreateTextNode(const data: DOMString): TDOMText;
begin
  Result := TDOMText.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateComment(const data: DOMString): TDOMComment;
begin
  Result := TDOMComment.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
end;

function TDOMDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
end;

function TDOMDocument.CreateAttribute(const name: DOMString): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  Result.FNodeName := name;
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
end;

function TDOMDocument.CreateEntity(const data: DOMString): TDOMEntity;
begin
  Result := TDOMEntity.Create(Self);
  Result.FNodeName := data;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, tagname);
end;


function TXMLDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  Result := TDOMCDATASection.Create(Self);
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  Result := TDOMProcessingInstruction.Create(Self);
  Result.FNodeName := target;
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  Result := TDOMEntityReference.Create(Self);
  Result.FNodeName := name;
end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

constructor TDOMAttr.Create(AOwner: TDOMDocument);
begin
  FNodeType := ATTRIBUTE_NODE;
  inherited Create(AOwner);
end;

function TDOMAttr.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMAttr.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  TDOMAttr(Result).FSpecified := FSpecified;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMAttr.GetNodeValue: DOMString;
var
  child: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FFirstChild) then
  begin
    child := FFirstChild;
    while Assigned(child) do
    begin
      if child.NodeType = ENTITY_REFERENCE_NODE then
        Result := Result + '&' + child.NodeName + ';'
      else
        Result := Result + child.NodeValue;
      child := child.NextSibling;
    end;
  end;
end;

procedure TDOMAttr.SetNodeValue(AValue: DOMString);
var
  tn: TDOMText;
begin
  FSpecified := True;
  tn := TDOMText.Create(FOwnerDocument);
  tn.FNodeValue := AValue;
  if Assigned(FFirstChild) then
    ReplaceChild(tn, FFirstChild)
  else
    AppendChild(tn);
end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

constructor TDOMElement.Create(AOwner: TDOMDocument);
begin
  FNodeType := ELEMENT_NODE;
  inherited Create(AOwner);
  FAttributes := TDOMNamedNodeMap.Create(AOwner);
end;

destructor TDOMElement.Destroy;
var
  i: Integer;
begin
  {As the attributes are _not_ childs of the element node, we have to free
   them manually here:}
  for i := 0 to FAttributes.Count - 1 do
    FAttributes[i].Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
var
  i: Integer;
begin
  Result := TDOMElement.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  for i := 0 to FAttributes.Count - 1 do
    TDOMElement(Result).FAttributes.Add(FAttributes[i].CloneNode(True, ACloneOwner));
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      Result := FAttributes[i].NodeValue;
      exit;
    end;
  SetLength(Result, 0);
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  i: Integer;
  attr: TDOMAttr;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      FAttributes[i].NodeValue := value;
      exit;
    end;
  attr := TDOMAttr.Create(FOwnerDocument);
  attr.FNodeName := name;
  attr.NodeValue := value;
  FAttributes.Add(attr);
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      FAttributes[i].Free;
      FAttributes.Delete(i);
      exit;
    end;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      Result := TDOMAttr(FAttributes[i]);
      exit;
    end;
  Result := nil;
end;

procedure TDOMElement.SetAttributeNode(NewAttr: TDOMAttr);
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = NewAttr.NodeName then
    begin
      FAttributes[i].Free;
      FAttributes[i] := NewAttr;
      exit;
    end;
end;

function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
var
  i: Integer;
  node: TDOMNode;
begin
  for i := 0 to FAttributes.Count - 1 do
  begin
    node := FAttributes[i];
    if node = OldAttr then
    begin
      FAttributes.Delete(i);
      Result := TDOMAttr(node);
      exit;
    end;
  end;
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, name);
end;

procedure TDOMElement.Normalize;
begin
  // !!!: Not implemented
end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

constructor TDOMText.Create(AOwner: TDOMDocument);
begin
  FNodeType := TEXT_NODE;
  FNodeName := '#text';
  inherited Create(AOwner);
end;

function TDOMText.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMText.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  Result := TDOMText.Create(FOwnerDocument);
  Result.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  FNodeValue := Copy(FNodeValue, 1, offset);
  FParentNode.InsertBefore(Result, FNextSibling);
end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

constructor TDOMComment.Create(AOwner: TDOMDocument);
begin
  FNodeType := COMMENT_NODE;
  FNodeName := '#comment';
  inherited Create(AOwner);
end;

function TDOMComment.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMComment.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

constructor TDOMCDATASection.Create(AOwner: TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType := CDATA_SECTION_NODE;
  FNodeName := '#cdata-section';
end;

function TDOMCDATASection.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMCDATASection.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

constructor TDOMDocumentType.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_TYPE_NODE;
  inherited Create(AOwner);
end;

function TDOMDocumentType.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMDocumentType.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

constructor TDOMNotation.Create(AOwner: TDOMDocument);
begin
  FNodeType := NOTATION_NODE;
  inherited Create(AOwner);
end;

function TDOMNotation.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMNotation.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

constructor TDOMEntity.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

constructor TDOMEntityReference.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_REFERENCE_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

constructor TDOMProcessingInstruction.Create(AOwner: TDOMDocument);
begin
  FNodeType := PROCESSING_INSTRUCTION_NODE;
  inherited Create(AOwner);
end;


end.


{
  Revision 1.13  2003/11/15 10:31:50  michael
  + Fixed CloneNode overloaded call (from Andreas Hausladen)

  Revision 1.12  2003/01/15 21:59:55  sg
  * the units DOM, XMLRead and XMLWrite now compile with Delphi without
    modifications as well

  Revision 1.11  2002/12/11 21:06:07  sg
  * Small cleanups
  * Replaced htmldoc unit with dom_html unit
  * Added SAX parser framework and SAX HTML parser

  Revision 1.10  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

  Revision 1.9  2002/03/01 10:02:38  sg
  * Fixed read access method for TDOMAttr.Value

}
