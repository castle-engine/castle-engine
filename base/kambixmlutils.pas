{ Various XML and DOM utilities. }
unit KambiXMLUtils;

interface

uses SysUtils, DOM;

{ Retrieves from Element attribute Value and returns @true,
  or (of there is no such attribute) returns @false
  and does not modify Value. Value is a "var", not "out" param,
  because in the latter case it's guaranteed that the old Value
  will not be cleared. }
function DOMGetAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: string): boolean;

{ Like DOMGetAttribute, but reads Cardinal value. }
function DOMGetCardinalAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Cardinal): boolean;

{ Like DOMGetAttribute, but reads Integer value. }
function DOMGetIntegerAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Integer): boolean;

{ Like DOMGetAttribute, but reads Single value. }
function DOMGetSingleAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Single): boolean;

{ Like DOMGetAttribute, but reads Boolean value.
  A boolean value is interpreted just like FPC's TXMLConfig
  objects: true is designated by word @code(true), false by word
  @code(false), case is ignored.

  If attribute exists but it's value
  is not @code(true) or @code(false), then returns @false and doesn't
  modify Value paramater. So behaves just like the attribute didn't exist. }
function DOMGetBooleanAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: boolean): boolean;

{ This returns the @italic(one and only) child element of this Element.
  If given Element has none or more than one child elements,
  returns @nil. This is handy for parsing XML in cases when you
  know that given element must contain exactly one other element
  in correct XML file. }
function DOMGetOneChildElement(const Element: TDOMElement): TDOMElement;

type
  EDOMChildElementError = class(Exception);

{ Searches children elements inside Element for element with given
  ChildName.

  For example

@preformatted(
  <level>
    <creatures>
      ...
    </creatures>
    <items>
      ...
    </items>
  </level>
)

  If you pass as Element the <level> node, and 'items' as
  ChildNode, then the TDOMElement representing <items>
  will be returned. If given ChildName will not exist
  @italic(or it will exist more than once (yes, that's checked)),
  then will return @nil or raise EDOMChildElementError
  (depending on RaiseOnError).

  @raises(EDOMChildElementError
    If child not found or found more than once and RaiseOnError)  }
function DOMGetChildElement(const Element: TDOMElement;
  const ChildName: string; RaiseOnError: boolean): TDOMElement;

{ This returns the text data contained in this element.

  This is suitable if an element is supposed to contain only some text.
  It raises an error if an element contains any other element as child.

  It concatenates all text data nodes that are direct children
  of this element. So if there are no text data nodes, it returns
  empty string without raising any error.

  AFAIK it's uncommon but
  possible to have here more than one text node. Normally, more than one
  text nodes occur because they are separated by other child elements,
  but we already eliminated this possibility (i.e. we raise error
  in this case). Still, if you operated on DOM tree, e.g. deleted
  some elements, or inserted some text nodes, then I think it's possible
  that you will have more than one text node within this element.
  So this procedure should still work OK in this case. }
function DOMGetTextData(const Element: TDOMElement): string;

implementation

uses KambiUtils;

function DOMGetAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: string): boolean;
var
  AttrNode: TDOMNode;
begin
  AttrNode := Element.Attributes.GetNamedItem(AttrName);
  Result := AttrNode <> nil;
  if Result then
  begin
    Check(AttrNode.NodeType = ATTRIBUTE_NODE,
      'All element attributes must have ATTRIBUTE_NODE');
    Value := (AttrNode as TDOMAttr).Value;
  end;
end;

function DOMGetCardinalAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Cardinal): boolean;
var
  ValueStr: string;
begin
  Result := DOMGetAttribute(Element, AttrName, ValueStr);
  if Result then
    Value := StrToInt(ValueStr);
end;

function DOMGetIntegerAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Integer): boolean;
var
  ValueStr: string;
begin
  Result := DOMGetAttribute(Element, AttrName, ValueStr);
  if Result then
    Value := StrToInt(ValueStr);
end;

function DOMGetSingleAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Single): boolean;
var
  ValueStr: string;
begin
  Result := DOMGetAttribute(Element, AttrName, ValueStr);
  if Result then
    Value := StrToFloat(ValueStr);
end;

function DOMGetBooleanAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: boolean): boolean;
var
  ValueStr: string;
begin
  Result := DOMGetAttribute(Element, AttrName, ValueStr);
  if Result then
  begin
    if AnsiCompareText(ValueStr, 'TRUE') = 0 then
      Value := true else
    if AnsiCompareText(ValueStr, 'FALSE') = 0 then
      Value := false else
      Result := false;
  end;
end;

function DOMGetOneChildElement(const Element: TDOMElement): TDOMElement;
var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := nil;
  Children := Element.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
    begin
      Node := Children.Item[I];
      if Node.NodeType = ELEMENT_NODE then
      begin
        if Result = nil then
          Result := Node as TDOMElement else
        begin
          { More than one element in Children. }
          Result := nil;
          Exit;
        end;
      end;
    end;
  finally Children.Release end;
end;

function DOMGetChildElement(const Element: TDOMElement;
  const ChildName: string; RaiseOnError: boolean): TDOMElement;
var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := nil;
  Children := Element.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
    begin
      Node := Children.Item[I];
      if (Node.NodeType = ELEMENT_NODE) and
         ((Node as TDOMElement).TagName = ChildName) then
      begin
        if Result = nil then
          Result := TDOMElement(Node) else
        begin
          if RaiseOnError then
            raise EDOMChildElementError.CreateFmt(
              'Child "%s" occurs more than once', [ChildName]) else
            Exit(nil);
        end;
      end;
    end;
  finally Children.Release end;

  if (Result = nil) and RaiseOnError then
    raise EDOMChildElementError.CreateFmt(
      'Child "%s" not found', [ChildName])
end;

function DOMGetTextData(const Element: TDOMElement): string;
var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := '';
  Children := Element.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
    begin
      Node := Children.Item[I];
      case Node.NodeType of
        TEXT_NODE: Result += (Node as TDOMText).Data;
        ELEMENT_NODE: raise Exception.CreateFmt(
          'Child elements not allowed within element <%s>', [Element.TagName]);
      end;
    end;
  finally Children.Release end;
end;

end.