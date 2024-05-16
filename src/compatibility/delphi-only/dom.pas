{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Classes to access XML from Delphi.

  The API of this unit is deliberately compatible with (a subset of) FPC DOM
  unit, so you can just do "uses DOM" in both FPC and Delphi code
  and operate on XML with the same API.
  Underneath we use Delphi XML / DOM API based on interfaces
  ( https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_the_Document_Object_Model ). }
unit DOM;

{$I castleconf.inc}

{ Delphi XML can be supported by various vendors.
  See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_the_Document_Object_Model .

  The default:
  - on Windows is MSXML
    (in some older versions you had to download it from
    https://www.microsoft.com/en-us/download/details.aspx?id=3988 ).
  - On other platforms, despite the Delphi docs (which say that XML will just
    not work without selecting other vendor) it seems some other vendor is
    automatically picked.
    TODO: which one? Omni?

  Define this symbol to explicitly select OmniXML vendor on all platform.
  OmniXML is:
  - cross-platform
  - open-source
    https://code.google.com/archive/p/omnixml/
    https://github.com/mremec/omnixml/
    (though it is at this point just included in Delphi, so you don't need to care)
  - doesn't cause a crash when CastleConfig is finalized from C++ Builder
    (testcase: examples/delphi/cpp_builder/window/)
}
{$define CASTLE_XML_OMNI}

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

    function GetItem(const I: Cardinal): TDOMNode;
    function GetLength: Cardinal;
  private
    function InternalRemove(const Name: String): TDOMNode;
  public
    constructor Create(OwnerNode: TDOMNode);
    destructor Destroy;override;
    function GetNamedItem(Name: String): TDOMNode;

    property Item[const Index: Cardinal]: TDOMNode read GetItem; default;
    property Length: Cardinal read GetLength;
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
  strict private
    FChildNodes: TDOMNodeList;
    function GetNodeValue: String;
    procedure SetNodeValue(const Value: String);
    function  GetFirstChild: TDOMNode; virtual;
  protected
    function GetNodeName: String;
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
    function GetCount: Cardinal;
    function GetItem(const I: Cardinal): TDOMNode;
  private
    procedure RemoveNode(const Child: TDOMNode);
  public
    constructor Create(const ANode: TDOMNode);
    destructor Destroy; override;
    property Item[const I: Cardinal]: TDOMNode read GetItem; default;
    property Count: Cardinal read GetCount;
    property Length: Cardinal read GetCount;
  end;

  TDOMElement = class(TDOMNode)
  strict private
    FAttributes: TDOMNamedNodeMap;
  protected
    function GetAttributes: TDOMNamedNodeMap; override;

  public
    destructor Destroy; override;

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

{$ifdef CASTLE_XML_OMNI}
uses Xml.omnixmldom, Xml.xmldom;
{$else}
  // otherwise we use default MSXML on Windows, which requires some adjustments
  {$ifdef MSWINDOWS}
  uses ComObj, Xml.Win.msxmldom;
  {$endif}
{$endif}

{ TDOMNodeList --------------------------------------------------------------- }

constructor TDOMNodeList.Create(const ANode: TDOMNode);
begin
  inherited Create;
  FOwnerDocument := ANode.OwnerDocument;
  InternalList := ANode.InternalNode.ChildNodes;

  { We pass OwnsChildren=false parameter to TObjectList.Create,
    because we use TComponent ownership mechanism to free TXMLNode instances. }
  Nodes := TObjectList.Create(false);
end;

destructor TDOMNodeList.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TDOMNodeList.GetCount: Cardinal;
begin
  Result := InternalList.Count;
end;

function TDOMNodeList.GetItem(const I: Cardinal): TDOMNode;
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
  // Temporary workaroud for Delphi 12. This will change to access using Integer anyway in delphi-linux branch.
  Result := Nodes[Integer(I)] as TDOMNode;
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

destructor TDOMElement.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

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
var
  InternalDocumentNonRef: XMLDoc.TXMLDocument;
begin
  inherited Create(nil);

  { MSXML special tweak:
    This needs to be called before using COM interfaces with MSXML.
    It seems other XML vendors like OmniXML do not require it. }
  {$ifndef CASTLE_XML_OMNI}
  {$ifdef MSWINDOWS}
  { This needs to be called before using COM interfaces with MSXML. }
  CoInitializeEx(nil, 0);
  {$endif}
  {$endif}

  InternalDocumentNonRef := XMLDoc.TXMLDocument.Create(Self);
  {$ifdef CASTLE_XML_OMNI}
  InternalDocumentNonRef.DOMVendor := GetDOMVendor(sOmniXmlVendor);
  {$endif}
  InternalDocumentNonRef.Active := true;
  InternalDocument := InternalDocumentNonRef; // assign to interface, it is ref-counted from now on

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
  { Note that we do nothing with InternalDocument,
    it's a COM interface and should be ref-counted.
    No need for "InternalDocument := nil;". }
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

  { We pass OwnsChildren=false parameter to TObjectList.Create,
    because we use TComponent ownership mechanism to free TXMLNode instances. }
  Nodes := TObjectList.Create(false);
end;

destructor TDOMNamedNodeMap.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TDOMNamedNodeMap.GetItem(const I: Cardinal): TDOMNode;
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
  // Temporary workaroud for Delphi 12. This will change to access using Integer anyway in delphi-linux branch.
  Result := Nodes[Integer(I)] as TDOMNode;
end;

function TDOMNamedNodeMap.GetLength: Cardinal;
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
  {$ifdef CASTLE_XML_OMNI}
  (*In the past we changed DefaultDOMVendor, but this was more invasive,
    as it affects XML implementation used by Delphi IDE (since this unit
    is also in a design-time package).

    It caused occasional issues when trying to save / open a project in Delphi IDE,
    about "document encoding",
    with Delphi IDE stacktrace like this:

      [6FBD3F97]{xmlrtl290.bpl} Xml.XMLDoc.TXMLDocument.LoadData (Line 2557, "Xml.XMLDoc.pas" + 11) + $26
      [7168A1C4]{rtl290.bpl  } System.@CheckAutoResult (Line 40283, "System.pas" + 4) + $6
      [6FBD3F97]{xmlrtl290.bpl} Xml.XMLDoc.TXMLDocument.LoadData (Line 2557, "Xml.XMLDoc.pas" + 11) + $26
      [6FBD3E53]{xmlrtl290.bpl} Xml.XMLDoc.TXMLDocument.SetActive (Line 2523, "Xml.XMLDoc.pas" + 13) + $7
      [67E70559]{profiledeployide290.bpl} DeploymentImpl.TDeploymentStorageManager.ReadDefaultDeployment (Line 1802, "DeploymentImpl.pas" + 47) + $7
      [67E6C40D]{profiledeployide290.bpl} DeploymentImpl.TDeploymentModuleHandler.EnsureDefaultDeployment (Line 651, "DeploymentImpl.pas" + 13) + $1A
      [67E6D5C5]{profiledeployide290.bpl} DeploymentImpl.TDeploymentModuleHandler.GetFiles (Line 909, "DeploymentImpl.pas" + 1) + $2
      [67E6D420]{profiledeployide290.bpl} DeploymentImpl.TDeploymentModuleHandler.GetSortedFiles (Line 865, "DeploymentImpl.pas" + 1) + $2
      [67E71758]{profiledeployide290.bpl} DeploymentImpl.TDeploymentStorageManager.Write (Line 1962, "DeploymentImpl.pas" + 9) + $8
      [67E72B37]{profiledeployide290.bpl} DeploymentImpl.TDeploymentStorageManager.ProjectSaving (Line 2153, "DeploymentImpl.pas" + 3) + $11
      [6F162850]{coreide290.bpl} ProjectFileUtils.CallProjectFileStorage (Line 562, "ProjectFileUtils.pas" + 14) + $B
      [6F163D55]{coreide290.bpl} ProjectFileUtils.NotifyProjectStorageSaving (Line 844, "ProjectFileUtils.pas" + 4) + $67
      [6F0C67F5]{coreide290.bpl} ProjectModule.TBaseProject.NotifyProjectStorageSaving (Line 2028, "ProjectModule.pas" + 3) + $23
      [6F0C6B5A]{coreide290.bpl} ProjectModule.TCustomProject.Save (Line 2108, "ProjectModule.pas" + 20) + $3
      [6F029C25]{coreide290.bpl} ProjectGroup.SaveProjects (Line 2022, "ProjectGroup.pas" + 68) + $1E
      [6F029EB4]{coreide290.bpl} ProjectGroup.TProjectGroup.Save (Line 2058, "ProjectGroup.pas" + 2) + $1
      [6F02DBF9]{coreide290.bpl} ProjectGroup.TProjectGroupWrapper.Save (Line 3357, "ProjectGroup.pas" + 2) + $7
      [00C28263]{bds.exe     } AppMain.TAppBuilder.CanCloseProjectGroup + $4F
      ....

    The testcase was not 100% confirmed, but it seems that changing package settings
    from AllProjects group, then trying to open a different project (like fps_game),
    saying "Yes" to save the AllProjects changes, was causing errors.

    Now we do a safer approach: never change global DefaultDOMVendor,
    instead we'll set TXMLDocument.DOMVendor after TXMLDocument creation.
  *)
  //DefaultDOMVendor := sOmniXmlVendor;

  {$else}
  {$ifdef MSWINDOWS}
  { Reading X3D XML fails without this.
    See https://bobsotherblog.wordpress.com/2013/09/19/fixing-dtd-is-prohibited-error-in-delphi/
    https://docwiki.embarcadero.com/Libraries/Sydney/en/Xml.Win.msxmldom.MSXML6_ProhibitDTD }
  Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  {$endif}
  {$endif}
end.

