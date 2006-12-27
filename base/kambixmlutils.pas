{ Various XML and DOM utilities. }
unit KambiXMLUtils;

interface

uses DOM;

{ Retrieves from Element attribute Value and returns @true,
  or (of there is no such attribute) returns @false
  and does not modify Value. Value is a "var", not "out" param,
  because in the latter case it's guaranteed that the old Value
  will not be cleared. }
function DOMGetAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: string): boolean;

{ Like DOMGetCardinalAttribute, but reads Cardinal value. }
function DOMGetCardinalAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Cardinal): boolean;

{ Like DOMGetCardinalAttribute, but reads Single value. }
function DOMGetSingleAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Single): boolean;

{ Like DOMGetCardinalAttribute, but reads Boolean value.
  A boolean value is interpreted just like FPC's TXMLConfig
  objects: true is designated by word @code(true), false by word
  @code(false), case is ignored.

  If attribute exists but it's value
  is not @code(true) or @code(false), then returns @false and doesn't
  modify Value paramater. So behaves just like the attribute didn't exist. }
function DOMGetBooleanAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: boolean): boolean;

implementation

uses SysUtils, KambiUtils;

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

end.