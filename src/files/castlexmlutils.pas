{
  Copyright 2012-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various XML and DOM utilities. }
unit CastleXMLUtils;

interface

uses SysUtils, DOM, CastleDownload,
  CastleUtils, CastleColors, CastleVectors;

type
  EDOMAttributeMissing = class(Exception);
  EDOMChildElementError = class(Exception);

  TXMLElementIterator = class;

  TDOMNodeHelper = class helper for TDOMNode
  private
    function GetNodeValue8: string;
    procedure SetNodeValue8(const S: string);
  public
    { Node name (attribute, element or such name).

      In FPC this is expressed as an 8-bit string (in UTF-8 encoding),
      contrary to the NodeName from FPC DOM unit that is a WideString (DOMString). }
    function NodeName8: string;

    { Node value (like an attribute value).

      In FPC this is expressed as an 8-bit string (in UTF-8 encoding),
      contrary to the NodeValue from FPC DOM unit that is a WideString (DOMString). }
    property NodeValue8: string read GetNodeValue8 write SetNodeValue8;
  end;

  TDOMCharacterDataHelper = class helper for TDOMCharacterData
    { String data.

      In FPC this is expressed as an 8-bit string (in UTF-8 encoding),
      contrary to the Data from FPC DOM unit that is a WideString (DOMString). }
    function Data8: string;
  end;

  TDOMElementHelper = class helper for TDOMElement
  public

    { ------------------------------------------------------------------------
      Get an optional attribute to a "var" parameter, returns if found. }

    {$ifdef FPC}
    { Read from Element attribute value and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. Value is a "var", not "out" param,
      because in the latter case it's guaranteed that the old Value
      will not be cleared.

      Note that the returned Value may be empty, even when this returns @true,
      if the value is explicitly set to empty in XML (by @code(xxx="") in XML). }
    function AttributeString(const AttrName: string; var Value: string): boolean; overload;
    {$endif}

    { Read from Element attribute value as URL and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      Returned URL is always absolute. The value in file may be a relative URL,
      it is resolved with respect to BaseUrl, that must be absolute. }
    function AttributeURL(const AttrName: string; const BaseUrl: string; var URL: string): boolean; overload;

    { Read from Element attribute value as Cardinal and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeCardinal(const AttrName: string; var Value: Cardinal): boolean; overload;

    { Read from Element attribute value as Integer and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeInteger(const AttrName: string; var Value: Integer): boolean; overload;

    { Read from Element attribute value as Int64 and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeInt64(const AttrName: string; var Value: Int64): boolean; overload;

    { Read from Element attribute value as QWord and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeQWord(const AttrName: string; var Value: QWord): boolean; overload;

    { Read from Element attribute value as Single and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeSingle(const AttrName: string; var Value: Single): boolean; overload;

    { Read from Element attribute value as Float and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      Note: for powerful reading of float expressions,
      consider using @code(AttributeFloatExpression) instead of @code(AttributeFloat).
      It can read expressions like @code("3.0 * 2.0") or @code("sin(2.0)").
      Use CastleScriptXML unit to introduce
      necessary class helper for this, see @link(TDOMElementScriptHelper.AttributeFloatExpression). }
    function AttributeFloat(const AttrName: string; var Value: Float): boolean; overload;

    { Read from Element attribute value as Boolean and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      A boolean value is interpreted just like FPC's TXMLConfig
      objects: true is designated by word @code(true), false by word
      @code(false), case is ignored.
      If attribute exists but it's value
      is not @code(true) or @code(false), then returns @false and doesn't
      modify Value paramater. So behaves just like the attribute didn't exist. }
    function AttributeBoolean(const AttrName: string; var Value: boolean): boolean; overload;

    { Read from Element attribute value as color and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeColor(const AttrName: string; var Value: TCastleColor): boolean; overload;

    { Read from Element attribute value as RGB color and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value. }
    function AttributeColorRGB(const AttrName: string; var Value: TCastleColorRGB): boolean; overload;

    { Read from Element attribute as a 2D vector (2 floats), and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      @raises EConvertError If the attribute exists in XML, but has invalid format. }
    function AttributeVector2(const AttrName: string; var Value: TVector2): boolean; overload;

    { Read from Element attribute as a 3D vector (3 floats), and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      @raises EConvertError If the attribute exists in XML, but has invalid format. }
    function AttributeVector3(const AttrName: string; var Value: TVector3): boolean; overload;

    { Read from Element attribute as a 4D vector(4 floats, e.g. rotation), and returns @true,
      or (if there is no such attribute) returns @false
      and does not modify Value.

      @raises EConvertError If the attribute exists in XML, but has invalid format. }
    function AttributeVector4(const AttrName: string; var Value: TVector4): boolean; overload;

    { ------------------------------------------------------------------------
      Get a required attribute, returns value (exception if not found). }

    { Retrieves from Element given attribute as a string,
      raises EDOMAttributeMissing if missing.

      Note that the attribute is required, but it's value may still be empty
      if it's explicitly set to empty in XML (by @code(xxx="") in XML).
      This is different than TCastleConfig.GetStringNonEmpty method,
      that serves a similar purpose for TCastleConfig, but it requires
      @italic(non-empty value) exists. Here, we only require that the value
      exists, but it may still be empty.

      @raises EDOMAttributeMissing }
    function AttributeString(const AttrName: string): string; overload;

    { Retrieves from Element given attribute as an absolute URL,
      raises EDOMAttributeMissing if missing.
      Returned URL is always absolute. The value in file may be a relative URL,
      it is resolved with respect to BaseUrl, that must be absolute.
      @raises EDOMAttributeMissing }
    function AttributeURL(const AttrName: string; const BaseUrl: string): string; overload;

    { Retrieves from Element given attribute as a Cardinal,
      raises EDOMAttributeMissing if missing.
      @raises EDOMAttributeMissing }
    function AttributeCardinal(const AttrName: string): Cardinal; overload;

    { Retrieves from Element given attribute as an Integer,
      raises EDOMAttributeMissing if missing.
      @raises EDOMAttributeMissing }
    function AttributeInteger(const AttrName: string): Integer; overload;

    { Retrieves from Element given attribute as an Int64,
      raises EDOMAttributeMissing if missing.
      @raises EDOMAttributeMissing }
    function AttributeInt64(const AttrName: string): Int64; overload;

    { Retrieves from Element given attribute as an QWord,
      raises EDOMAttributeMissing if missing.
      @raises EDOMAttributeMissing }
    function AttributeQWord(const AttrName: string): QWord; overload;

    { Retrieves from Element given attribute as a Single,
      raises EDOMAttributeMissing if missing.
      @raises EDOMAttributeMissing }
    function AttributeSingle(const AttrName: string): Single; overload;

    { Retrieves from Element given attribute as a Float,
      raises EDOMAttributeMissing if missing.

      Note: for powerful reading of float expressions,
      consider using @code(AttributeFloatExpression) instead of @code(AttributeFloat).
      It can read expressions like @code("3.0 * 2.0") or @code("sin(2.0)").
      Use CastleScriptXML unit to introduce
      necessary class helper for this, see @link(TDOMElementScriptHelper.AttributeFloatExpression).

      @raises EDOMAttributeMissing }
    function AttributeFloat(const AttrName: string): Float; overload;

    { Retrieves from Element given attribute as a boolean,
      raises EDOMAttributeMissing if missing or has invalid value.
      A boolean value is interpreted just like FPC's TXMLConfig
      objects: true is designated by word @code(true), false by word
      @code(false), case is ignored.

      If attribute exists but it's value
      is not @code(true) or @code(false), then raises EDOMAttributeMissing.
      So behaves just like the attribute didn't exist.

      @raises EDOMAttributeMissing }
    function AttributeBoolean(const AttrName: string): boolean; overload;

    { Retrieves from Element given attribute as a color,
      raises EDOMAttributeMissing if missing or has invalid format.
      @raises EDOMAttributeMissing }
    function AttributeColor(const AttrName: string): TCastleColor; overload;

    { Retrieves from Element given attribute as an RGB color,
      raises EDOMAttributeMissing if missing or has invalid format.
      @raises EDOMAttributeMissing }
    function AttributeColorRGB(const AttrName: string): TCastleColorRGB; overload;

    { Retrieves from Element given attribute as a 2D vector (2 floats),
      raises EDOMAttributeMissing if missing or has invalid format.
      @raises EDOMAttributeMissing }
    function AttributeVector2(const AttrName: string): TVector2; overload;

    { Retrieves from Element given attribute as a 3D vector (3 floats),
      raises EDOMAttributeMissing if missing or has invalid format.
      @raises EDOMAttributeMissing }
    function AttributeVector3(const AttrName: string): TVector3; overload;

    { Retrieves from Element given attribute as a 4D vector (4 floats, e.g. rotation),
      raises EDOMAttributeMissing if missing or has invalid format.
      @raises EDOMAttributeMissing }
    function AttributeVector4(const AttrName: string): TVector4; overload;

    { ------------------------------------------------------------------------
      Get an optional attribute, returns attribute or a default value. }

    { Retrieves from Element given attribute as a string, or a default value
      if the attribute was not explicitly given. }
    function AttributeStringDef(const AttrName: string; const DefaultValue: string): string;

    { Retrieves from Element given attribute as a Cardinal, or a default value. }
    function AttributeCardinalDef(const AttrName: string; const DefaultValue: Cardinal): Cardinal;

    { Retrieves from Element given attribute as an Integer, or a default value. }
    function AttributeIntegerDef(const AttrName: string; const DefaultValue: Integer): Integer;

    { Retrieves from Element given attribute as an Int64, or a default value. }
    function AttributeInt64Def(const AttrName: string; const DefaultValue: Int64): Int64;

    { Retrieves from Element given attribute as an QWord, or a default value. }
    function AttributeQWordDef(const AttrName: string; const DefaultValue: QWord): QWord;

    { Retrieves from Element given attribute as a Single, or a default value. }
    function AttributeSingleDef(const AttrName: string; const DefaultValue: Single): Single;

    { Retrieves from Element given attribute as a Float, or a default value.

      Note: for powerful reading of float expressions,
      consider using @code(AttributeFloatExpressionDef) instead of @code(AttributeFloatDef).
      It can read expressions like @code("3.0 * 2.0") or @code("sin(2.0)").
      Use CastleScriptXML unit to introduce
      necessary class helper for this, see @link(TDOMElementScriptHelper.AttributeFloatExpressionDef). }
    function AttributeFloatDef(const AttrName: string; const DefaultValue: Float): Float;

    { Retrieves from Element given attribute as a boolean,
      returns a default value if missing or has invalid value. }
    function AttributeBooleanDef(const AttrName: string; const DefaultValue: boolean): boolean;

    { Retrieves from Element given attribute as a color, or a default value. }
    function AttributeColorDef(const AttrName: string; const DefaultValue: TCastleColor): TCastleColor;

    { Retrieves from Element given attribute as an RGB color, or a default value. }
    function AttributeColorRGBDef(const AttrName: string; const DefaultValue: TCastleColorRGB): TCastleColorRGB;

    { Retrieves from Element given attribute as a 2D vector (2 floats), or a default value.
      @raises EConvertError If the value exists in XML, but has invalid format. }
    function AttributeVector2Def(const AttrName: string; const DefaultValue: TVector2): TVector2;

    { Retrieves from Element given attribute as a 3D vector (3 floats), or a default value.
      @raises EConvertError If the value exists in XML, but has invalid format. }
    function AttributeVector3Def(const AttrName: string; const DefaultValue: TVector3): TVector3;

    { Retrieves from Element given attribute as a 4D vector (4 floats), or a default value.
      @raises EConvertError If the value exists in XML, but has invalid format. }
    function AttributeVector4Def(const AttrName: string; const DefaultValue: TVector4): TVector4;

    { Attribute setting ------------------------------------------------------ }

    { Set the attribute as string. Equivalent to standard SetAttribute in DOM unit,
      but provided here for consistency with other AttributeSet overloads. }
    procedure AttributeSet(const AttrName: string; const Value: string); overload;

    { Set the attribute as boolean,
      such that it's readable back by @link(AttributeBoolean) and @link(AttributeBooleanDef). }
    procedure AttributeSet(const AttrName: string; const Value: boolean); overload;

    { Set the attribute as Integer,
      such that it's readable back by @link(AttributeInteger) and @link(AttributeIntegerDef). }
    procedure AttributeSet(const AttrName: string; const Value: Integer); overload;

    { Set the attribute as Int64,
      such that it's readable back by @link(AttributeInt64) and @link(AttributeInt64Def). }
    procedure AttributeSet(const AttrName: string; const Value: Int64); overload;

    { Set the attribute as QWord,
      such that it's readable back by @link(AttributeQWord) and @link(AttributeQWordDef). }
    procedure AttributeSet(const AttrName: string; const Value: QWord); overload;

    { Set the attribute as Cardinal,
      such that it's readable back by @link(AttributeCardinal) and @link(AttributeCardinalDef). }
    procedure AttributeSet(const AttrName: string; const Value: Cardinal); overload;

    { Set the attribute as Single,
      such that it's readable back by @link(AttributeSingle) and @link(AttributeSingleDef). }
    procedure AttributeSet(const AttrName: string; const Value: Single); overload;

    { Set the attribute as TVector2,
      such that it's readable back by @link(AttributeVector2) and @link(AttributeVector2Def). }
    procedure AttributeSet(const AttrName: string; const Value: TVector2); overload;

    { Set the attribute as TVector3,
      such that it's readable back by @link(AttributeVector3) and @link(AttributeVector3Def). }
    procedure AttributeSet(const AttrName: string; const Value: TVector3); overload;

    { Set the attribute as TVector4,
      such that it's readable back by @link(AttributeVector4) and @link(AttributeVector4Def). }
    procedure AttributeSet(const AttrName: string; const Value: TVector4); overload;

    { Other methods ---------------------------------------------------------- }

    { Get child element with given ChildName.

      For example use @code(LevelElement.Child('items'))
      to get the <items> element within <level> element, as in example below.

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

      There must be @bold(one and only one child element with this name).
      In case there's zero, or more than one such element,
      we will raise EDOMChildElementError (if Required is @true, default)
      or return @nil (if Required is @false).

      @raises(EDOMChildElementError
        If child not found (or found more than once), and Required = @true.)  }
    function Child(const ChildName: string; const Required: boolean = true): TDOMElement;

    { Same as @link(Child). }
    function ChildElement(const ChildName: string; const Required: boolean = true): TDOMElement;

    { Create a new child element under this element, and return it. }
    function CreateChild(const ChildName: string): TDOMElement;

    { Iterator over all children elements. Use like this:

      @longCode(#
      var
        I: TXMLElementIterator;
      begin
        I := Element.ChildrenIterator;
        try
          while I.GetNext do
          begin
            // ... here goes your code to process I.Current ...
          end;
        finally FreeAndNil(I) end;
      end;
      #) }
    function ChildrenIterator: TXMLElementIterator; overload;

    { Iterator over all children elements named ChildName. Use like this:

      @longCode(#
      var
        I: TXMLElementIterator;
      begin
        I := Element.ChildrenIterator('item');
        try
          while I.GetNext do
          begin
            // ... here goes your code to process I.Current ...
          end;
        finally FreeAndNil(I) end;
      end;
      #) }
    function ChildrenIterator(const ChildName: string): TXMLElementIterator; overload;

    { The text data contained in this element.

      This is suitable if an element is supposed to contain only some text.
      Like @code(<some_value>This is a text inside.</some_value>).
      It raises an error if an element contains anything else as child.

      If there are no text data nodes, e.g. if the element is empty,
      it returns empty string without raising any error. }
    function TextData: string;

    { Tag name (element name).
      Expressed as an 8-bit string (in UTF-8 encoding), contrary to the TagName
      from FPC DOM unit that is a WideString (DOMString). }
    function TagName8: string;
  end;

  { Iterate over all children elements of given XML element.

    Without this, typical iteration looks like

    @longCode(#
    var
      Index: Integer;
      ChildrenList: TDOMNodeList;
      ChildNode: TDOMNode;
      ChildElement: TDOMElement;
    begin
      ChildrenList := Element.ChildNodes;

      for Index := 0 to ChildrenList.Count - 1 do
      begin
        ChildNode := ChildrenList.Item[Index];
        if ChildNode.NodeType = ELEMENT_NODE then
        begin
          ChildElement := ChildNode as TDOMElement;
          ... here goes your code to process ChildElement ...
        end;
      end;
    end;
    #)

    ... which is an easy code, but it becomes tiresome
    to write this over and over again, especially
    for units that heavily process XML (like X3D XML or Collada readers).
    So this class allows you to write instead

    @longCode(#
    var
      I: TXMLElementIterator;
    begin
      I := Element.ChildrenIterator;
      try
        while I.GetNext do
        begin
          ... here goes your code to process I.Current ...
        end;
      finally FreeAndNil(I) end;
    end;
    #) }
  TXMLElementIterator = class
  private
    ChildNodes: TDOMNodeList;
    ChildIndex: Integer;
    FCurrent: TDOMElement;
  public
    constructor Create(ParentElement: TDOMElement);
    function GetNext: boolean; virtual;
    property Current: TDOMElement read FCurrent;
    procedure Rewind;
    function Count: Integer;
  end;

  { Iterate over children elements of given XML element, that have matching TagName. }
  TXMLElementFilteringIterator = class(TXMLElementIterator)
  private
    FTagName: DOMString;
  public
    constructor Create(ParentElement: TDOMElement; const TagName: string);
    function GetNext: boolean; override;
  end;

  { Iterate over all CDATA nodes of given XML element.

    Simple usage:

    @longCode(#
    var
      I: TXMLCDataIterator;
    begin
      I := TXMLCDataIterator.Create(Element);
      try
        while I.GetNext do
        begin
          ... here goes your code to process I.Current ...
        end;
      finally FreeAndNil(I) end;
    end;
    #) }
  TXMLCDataIterator = class
  private
    ChildNodes: TDOMNodeList;
    ChildIndex: Integer;
    FCurrent: string;
  public
    constructor Create(ParentElement: TDOMElement);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: string read FCurrent;
  end;

{ Retrieves from Element attribute Value and returns @true,
  or (if there is no such attribute) returns @false
  and does not modify Value. Value is a "var", not "out" param,
  because in the latter case it's guaranteed that the old Value
  will not be cleared.

  @deprecated Deprecated, use Element.AttributeString instead. }
function DOMGetAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: string): boolean;
  deprecated 'use helper method AttributeString on TDOMElement';

{ Like DOMGetAttribute, but reads Cardinal value.

  @deprecated Deprecated, use Element.AttributeCardinal instead. }
function DOMGetCardinalAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Cardinal): boolean;
  deprecated 'use helper method AttributeCardinal on TDOMElement';

{ Like DOMGetAttribute, but reads Integer value.

  @deprecated Deprecated, use Element.AttributeInteger instead. }
function DOMGetIntegerAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Integer): boolean;
  deprecated 'use helper method AttributeInteger on TDOMElement';

{ Like DOMGetAttribute, but reads Single value.

  @deprecated Deprecated, use Element.AttributeSingle instead. }
function DOMGetSingleAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Single): boolean;
  deprecated 'use helper method AttributeSingle on TDOMElement';

{ Like DOMGetAttribute, but reads Float value.

  @deprecated Deprecated, use Element.AttributeFloat instead. }
function DOMGetFloatAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Float): boolean;
  deprecated 'use helper method AttributeFloat on TDOMElement';

{ Like DOMGetAttribute, but reads Boolean value.
  A boolean value is interpreted just like FPC's TXMLConfig
  objects: true is designated by word @code(true), false by word
  @code(false), case is ignored.

  If attribute exists but it's value
  is not @code(true) or @code(false), then returns @false and doesn't
  modify Value paramater. So behaves just like the attribute didn't exist.

  @deprecated Deprecated, use Element.AttributeBoolean instead. }
function DOMGetBooleanAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: boolean): boolean;
  deprecated 'use helper method AttributeBoolean on TDOMElement';

{ Returns the @italic(one and only) child element of this Element.
  If given Element has none or more than one child elements,
  returns @nil. This is handy for parsing XML in cases when you
  know that given element must contain exactly one other element
  in correct XML file. }
function DOMGetOneChildElement(const Element: TDOMElement): TDOMElement;
  deprecated 'This method did not prove to be of much use, and it only clutters the API. Don''t use, or show us a convincing usecase when this is sensible.';

function DOMGetChildElement(const Element: TDOMElement;
  const ChildName: string; RaiseOnError: boolean): TDOMElement;
  deprecated 'use TDOMElement helper called ChildElement';

function DOMGetTextData(const Element: TDOMElement): string;
  deprecated 'use TDOMElement helper called TextData';

{ Gets a child of Element named ChildName, and gets text data within
  this child.

  This is just a shortcut for @code(DOMGetTextData(DOMGetChildElement(Element,
  ChildName, true))).

  @raises(EDOMChildElementError
    If child not found or found more than once and RaiseOnError) }
function DOMGetTextChild(const Element: TDOMElement;
  const ChildName: string): string;
  deprecated 'This method did not prove to be of much use, and it only clutters the API. Don''t use, or show us a convincing usecase when this is sensible.';

{ If needed, free result of TDOMElement.ChildNodes.

  This abstracts FPC DOM unit differences:
  @unorderedList(
    @item(
      For FPC <= 2.2.x, it was needed to call ChildNodes.Release when you're
      done with them.)

    @item(
      In newer FPC, you do not have to free nodes at all (since FPC rev 13143),
      and their Release method doesn't exist (since rev 13113).)
  ) }
procedure FreeChildNodes(const ChildNodes: TDOMNodeList);
  deprecated 'this is useless since a long time (FPC >= 2.4.x), you can remove this as the engine does not support older FPC versions anyway';

{ Replacements for standard ReadXMLFile and WriteXMLFile that operate on URLs.
  Optionally they can encrypt / decrypt content using BlowFish.
  @groupBegin }
procedure URLReadXML(out Doc: TXMLDocument; const URL: String); overload;
function URLReadXML(const URL: String): TXMLDocument; overload;
procedure URLWriteXML(Doc: TXMLDocument; const URL: String); overload;

{$ifdef FPC}
procedure URLReadXML(out Doc: TXMLDocument; const URL: String; const BlowFishKeyPhrase: string); overload;
function URLReadXML(const URL: String; const BlowFishKeyPhrase: string): TXMLDocument; overload;
procedure URLWriteXML(Doc: TXMLDocument; const URL: String; const BlowFishKeyPhrase: string); overload;
{$endif}
{ @groupEnd }

implementation

uses Classes, XMLRead, XMLWrite, {$ifdef FPC} BlowFish, {$endif}
  CastleURIUtils, CastleClassUtils;

{ TDOMNodeHelper ------------------------------------------------------------- }

function TDOMNodeHelper.NodeName8: string;
begin
  Result := UTF8Encode(NodeName);
end;

function TDOMNodeHelper.GetNodeValue8: string;
begin
  Result := UTF8Encode(NodeValue);
end;

procedure TDOMNodeHelper.SetNodeValue8(const S: string);
begin
  NodeValue := UTF8Decode(S);
end;

{ TDOMCharacterDataHelper ---------------------------------------------------- }

function TDOMCharacterDataHelper.Data8: string;
begin
  Result := UTF8Encode(Data);
end;

{ ----------------------------------------------------------------------------
  TDOMElementHelper:
  Get an optional attribute to a "var" parameter, returns if found. }

{$ifdef FPC}
function TDOMElementHelper.AttributeString(const AttrName: string; var Value: string): boolean;
var
  AttrNode: TDOMNode;
begin
  AttrNode := Attributes.GetNamedItem(UTF8decode(AttrName));
  Result := AttrNode <> nil;
  if Result then
  begin
    Check(AttrNode.NodeType = ATTRIBUTE_NODE,
      'All element attributes must have ATTRIBUTE_NODE');
    Value := UTF8Encode((AttrNode as TDOMAttr).Value);
  end;
end;
{$endif}

function TDOMElementHelper.AttributeURL(
  const AttrName: string; const BaseUrl: string; var URL: string): boolean;
begin
  Result := AttributeString(AttrName, URL);
  if Result then
    URL := CombineURI(BaseUrl, URL);
end;

function TDOMElementHelper.AttributeCardinal(
  const AttrName: string; var Value: Cardinal): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToInt64(ValueStr);
end;

function TDOMElementHelper.AttributeInteger(
  const AttrName: string; var Value: Integer): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToInt(ValueStr);
end;

function TDOMElementHelper.AttributeInt64(
  const AttrName: string; var Value: Int64): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToInt64(ValueStr);
end;

function TDOMElementHelper.AttributeQWord(
  const AttrName: string; var Value: QWord): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToQWord(ValueStr);
end;

function TDOMElementHelper.AttributeSingle(
  const AttrName: string; var Value: Single): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToFloat(ValueStr);
end;

function TDOMElementHelper.AttributeFloat(
  const AttrName: string; var Value: Float): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := StrToFloat(ValueStr);
end;

function TDOMElementHelper.AttributeBoolean(
  const AttrName: string; var Value: boolean): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
  begin
    if AnsiCompareText(ValueStr, 'TRUE') = 0 then
      Value := true else
    if AnsiCompareText(ValueStr, 'FALSE') = 0 then
      Value := false else
      Result := false;
  end;
end;

function TDOMElementHelper.AttributeColor(
  const AttrName: string; var Value: TCastleColor): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := HexToColor(ValueStr);
end;

function TDOMElementHelper.AttributeColorRGB(
  const AttrName: string; var Value: TCastleColorRGB): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := HexToColorRGB(ValueStr);
end;

function TDOMElementHelper.AttributeVector2(
  const AttrName: string; var Value: TVector2): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := Vector2FromStr(ValueStr);
end;

function TDOMElementHelper.AttributeVector3(
  const AttrName: string; var Value: TVector3): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := Vector3FromStr(ValueStr);
end;

function TDOMElementHelper.AttributeVector4(
  const AttrName: string; var Value: TVector4): boolean;
var
  ValueStr: string;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
    Value := Vector4FromStr(ValueStr);
end;

{ ------------------------------------------------------------------------
  TDOMElementHelper:
  Get a required attribute, returns value (exception if not found). }

function TDOMElementHelper.AttributeString(const AttrName: string): string;
begin
  if not AttributeString(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (string) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeURL(const AttrName: string; const BaseUrl: string): string;
begin
  if not AttributeURL(AttrName, BaseUrl, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (URL) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeCardinal(const AttrName: string): Cardinal;
begin
  if not AttributeCardinal(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (unsigned integer) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeInteger(const AttrName: string): Integer;
begin
  if not AttributeInteger(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (integer) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeInt64(const AttrName: string): Int64;
begin
  if not AttributeInt64(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (64-bit integer) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeQWord(const AttrName: string): QWord;
begin
  if not AttributeQWord(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (unsigned 64-bit integer) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeSingle(const AttrName: string): Single;
begin
  if not AttributeSingle(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (float) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeFloat(const AttrName: string): Float;
begin
  if not AttributeFloat(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (float) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeBoolean(const AttrName: string): boolean;
begin
  if not AttributeBoolean(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (boolean) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeColor(const AttrName: string): TCastleColor;
begin
  if not AttributeColor(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (color) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeColorRGB(const AttrName: string): TCastleColorRGB;
begin
  if not AttributeColorRGB(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (RGB color) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeVector2(const AttrName: string): TVector2;
begin
  if not AttributeVector2(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (vector2) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeVector3(const AttrName: string): TVector3;
begin
  if not AttributeVector3(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (vector3) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementHelper.AttributeVector4(const AttrName: string): TVector4;
begin
  if not AttributeVector4(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing (or has an invalid value) required (vector4) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

{ ------------------------------------------------------------------------
  TDOMElementHelper:
  Get an optional attribute, returns attribute or a default value. }

function TDOMElementHelper.AttributeStringDef(const AttrName: string; const DefaultValue: string): string;
begin
  if not AttributeString(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeIntegerDef(const AttrName: string; const DefaultValue: Integer): Integer;
begin
  if not AttributeInteger(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeInt64Def(const AttrName: string; const DefaultValue: Int64): Int64;
begin
  if not AttributeInt64(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeQWordDef(const AttrName: string; const DefaultValue: QWord): QWord;
begin
  if not AttributeQWord(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeCardinalDef(const AttrName: string; const DefaultValue: Cardinal): Cardinal;
begin
  if not AttributeCardinal(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeSingleDef(const AttrName: string; const DefaultValue: Single): Single;
begin
  if not AttributeSingle(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeFloatDef(const AttrName: string; const DefaultValue: Float): Float;
begin
  if not AttributeFloat(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeBooleanDef(const AttrName: string; const DefaultValue: boolean): boolean;
begin
  if not AttributeBoolean(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeColorDef(const AttrName: string; const DefaultValue: TCastleColor): TCastleColor;
begin
  if not AttributeColor(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeColorRGBDef(const AttrName: string; const DefaultValue: TCastleColorRGB): TCastleColorRGB;
begin
  if not AttributeColorRGB(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeVector2Def(const AttrName: string; const DefaultValue: TVector2): TVector2;
begin
  if not AttributeVector2(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeVector3Def(const AttrName: string; const DefaultValue: TVector3): TVector3;
begin
  if not AttributeVector3(AttrName, Result) then
    Result := DefaultValue;
end;

function TDOMElementHelper.AttributeVector4Def(const AttrName: string; const DefaultValue: TVector4): TVector4;
begin
  if not AttributeVector4(AttrName, Result) then
    Result := DefaultValue;
end;

{ TDOMElementHelper: Attribute setting ------------------------------------------------------ }

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: string);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(Value));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: boolean);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(SysUtils.BoolToStr(Value, true)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: Integer);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(IntToStr(Value)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: Int64);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(IntToStr(Value)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: QWord);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(IntToStr(Value)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: Cardinal);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(IntToStr(Value)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: Single);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(FloatToStr(Value)));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: TVector2);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(Value.ToRawString));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: TVector3);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(Value.ToRawString));
end;

procedure TDOMElementHelper.AttributeSet(const AttrName: string; const Value: TVector4);
begin
  SetAttribute(UTF8Decode(AttrName), UTF8Decode(Value.ToRawString));
end;

{ ------------------------------------------------------------------------
  TDOMElementHelper: Other methods. }

function TDOMElementHelper.ChildElement(const ChildName: string;
  const Required: boolean): TDOMElement;
begin
  Result := Child(ChildName, Required);
end;

function TDOMElementHelper.Child(const ChildName: string;
  const Required: boolean): TDOMElement;
var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := nil;
  Children := ChildNodes;

  for I := 0 to Integer(Children.Count) - 1 do
  begin
    Node := Children.Item[I];
    if (Node.NodeType = ELEMENT_NODE) and
       ((Node as TDOMElement).TagName = UTF8Decode(ChildName)) then
    begin
      if Result = nil then
        Result := TDOMElement(Node) else
      begin
        if Required then
          raise EDOMChildElementError.CreateFmt(
            'Child "%s" occurs more than once', [ChildName]) else
          Exit(nil);
      end;
    end;
  end;

  if (Result = nil) and Required then
    raise EDOMChildElementError.CreateFmt('Child "%s" not found', [ChildName])
end;

function TDOMElementHelper.TextData: string;

{
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
  So this procedure should still work OK in this case.
}

var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := '';
  Children := ChildNodes;
  for I := 0 to Integer(Children.Count) - 1 do
  begin
    Node := Children.Item[I];
    case Node.NodeType of
      TEXT_NODE: Result := Result + (Node as TDOMText).NodeValue8;
      ELEMENT_NODE: raise Exception.CreateFmt(
        'Child elements not allowed within element <%s>, but found %s',
          [TagName, (Node as TDOMElement).TagName]);
    end;
  end;
end;

function TDOMElementHelper.CreateChild(const ChildName: string): TDOMElement;
begin
  Result := OwnerDocument.CreateElement(UTF8Decode(ChildName));
  AppendChild(Result);
end;

function TDOMElementHelper.ChildrenIterator: TXMLElementIterator;
begin
  Result := TXMLElementIterator.Create(Self);
end;

function TDOMElementHelper.ChildrenIterator(const ChildName: string): TXMLElementIterator;
begin
  Result := TXMLElementFilteringIterator.Create(Self, ChildName);
end;

function TDOMElementHelper.TagName8: string;
begin
  Result := UTF8Encode(TagName);
end;

{ TXMLElementIterator -------------------------------------------------------- }

constructor TXMLElementIterator.Create(ParentElement: TDOMElement);
begin
  inherited Create;
  ChildNodes := ParentElement.ChildNodes;
  ChildIndex := -1;
end;

procedure TXMLElementIterator.Rewind;
begin
  ChildIndex := -1;
end;

function TXMLElementIterator.Count: Integer;
begin
  Result := ChildNodes.Count;
end;

function TXMLElementIterator.GetNext: boolean;
var
  ChildNode: TDOMNode;
begin
  repeat
    Inc(ChildIndex);

    if ChildIndex >= Integer(ChildNodes.Count) then
    begin
      Result := false;
      Break;
    end else
    begin
      ChildNode := ChildNodes[ChildIndex];
      if ChildNode.NodeType = ELEMENT_NODE then
      begin
        Result := true;
        FCurrent := ChildNode as TDOMElement;
        Break;
      end;
    end;
  until false;
end;

{ TXMLElementFilteringIterator ----------------------------------------------- }

constructor TXMLElementFilteringIterator.Create(ParentElement: TDOMElement; const TagName: string);
begin
  inherited Create(ParentElement);
  FTagName := UTF8Decode(TagName);
end;

function TXMLElementFilteringIterator.GetNext: boolean;
begin
  repeat
    Result := inherited GetNext;
  until (not Result) or (Current.TagName = FTagName);
end;

{ TXMLCDataIterator -------------------------------------------------------- }

constructor TXMLCDataIterator.Create(ParentElement: TDOMElement);
begin
  inherited Create;
  ChildNodes := ParentElement.ChildNodes;
  ChildIndex := -1;
end;

destructor TXMLCDataIterator.Destroy;
begin
  inherited;
end;

function TXMLCDataIterator.GetNext: boolean;
var
  ChildNode: TDOMNode;
begin
  repeat
    Inc(ChildIndex);

    if ChildIndex >= Integer(ChildNodes.Count) then
    begin
      Result := false;
      Break;
    end else
    begin
      ChildNode := ChildNodes[ChildIndex];
      if ChildNode.NodeType = CDATA_SECTION_NODE then
      begin
        Result := true;
        FCurrent := UTF8Encode((ChildNode as TDOMCDataSection).Data);
        Break;
      end;
    end;
  until false;
end;

{ globals -------------------------------------------------------------------- }

function DOMGetAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: string): boolean;
begin
  Result := Element.AttributeString(AttrName, Value);
end;

function DOMGetCardinalAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Cardinal): boolean;
begin
  Result := Element.AttributeCardinal(AttrName, Value);
end;

function DOMGetIntegerAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Integer): boolean;
begin
  Result := Element.AttributeInteger(AttrName, Value);
end;

function DOMGetSingleAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Single): boolean;
begin
  Result := Element.AttributeSingle(AttrName, Value);
end;

function DOMGetFloatAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: Float): boolean;
begin
  Result := Element.AttributeFloat(AttrName, Value);
end;

function DOMGetBooleanAttribute(const Element: TDOMElement;
  const AttrName: string; var Value: boolean): boolean;
begin
  Result := Element.AttributeBoolean(AttrName, Value);
end;

function DOMGetChildElement(const Element: TDOMElement;
  const ChildName: string; RaiseOnError: boolean): TDOMElement;
begin
  Result := Element.ChildElement(ChildName, RaiseOnError);
end;

function DOMGetTextData(const Element: TDOMElement): string;
begin
  Result := Element.TextData;
end;

function DOMGetOneChildElement(const Element: TDOMElement): TDOMElement;
var
  Children: TDOMNodeList;
  Node: TDOMNode;
  I: Integer;
begin
  Result := nil;
  Children := Element.ChildNodes;
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
end;

function DOMGetTextChild(const Element: TDOMElement;
  const ChildName: string): string;
begin
  Result := Element.ChildElement(ChildName).TextData;
end;

procedure FreeChildNodes(const ChildNodes: TDOMNodeList);
begin
  {$ifdef VER2_0} ChildNodes.Release; {$endif}
  {$ifdef VER2_1} ChildNodes.Release; {$endif}
  {$ifdef VER2_2} ChildNodes.Release; {$endif}
end;

{$ifdef FPC}

procedure URLReadXML(out Doc: TXMLDocument; const URL: String; const BlowFishKeyPhrase: string);
var
  Stream: TStream;
  DecryptStream: TBlowFishDecryptStream;
  DecryptedCorrectStream: TStringStream;
  L: Integer;
  DecryptedContent: string;
begin
  Doc := nil; // clean "out" param at start, just like ReadXMLFile
  Stream := Download(URL, []);
  try
    DecryptStream := TBlowFishDecryptStream.Create(BlowFishKeyPhrase, Stream);
    try
      { TBlowFishDecryptStream (or maybe encryption?) adds zeros at the end.
        Cut them off. }
      DecryptedContent := ReadGrowingStreamToString(DecryptStream);
      L := Length(DecryptedContent);
      while (L > 0) and (DecryptedContent[L] = #0) do
        Dec(L);
      SetLength(DecryptedContent, L);
      DecryptedCorrectStream := TStringStream.Create(DecryptedContent);
      try
        try
          ReadXMLFile(Doc, DecryptedCorrectStream);
        except
          // on EXMLReadError, improve exception message and reraise
          on E: EXMLReadError do
          begin
            E.Message := E.Message + ' (in file "' + URIDisplay(URL) + '", encrypted)';
            raise;
          end;
        end;
      finally FreeAndNil(DecryptedCorrectStream) end;
    finally FreeAndNil(DecryptStream) end;
  finally FreeAndNil(Stream) end;
end;

{$endif}

procedure URLReadXML(out Doc: TXMLDocument; const URL: String);
var
  Stream: TStream;
  StreamOptions: TStreamOptions;
  Gzipped: boolean;
begin
  Doc := nil; // clean "out" param at start, just like ReadXMLFile

  //guess gzipped based on file extension
  StreamOptions := [];
  URIMimeType(URL, Gzipped);
  if Gzipped then
    Include(StreamOptions, soGzip);

  Stream := Download(URL, StreamOptions);
  try
    try
      ReadXMLFile(Doc, Stream);
    except
      // on EXMLReadError, improve exception message and reraise
      on E: EXMLReadError do
      begin
        E.Message := E.Message + ' (in file "' + URIDisplay(URL) + '")';
        raise;
      end;
    end;
  finally FreeAndNil(Stream) end;
end;

function URLReadXML(const URL: String): TXMLDocument;
begin
  try
    // URLReadXML and ReadXMLFile nil the parameter when there's no need to free it
    URLReadXML(Result, URL);     //this one will automatically take care of gzipping
  except FreeAndNil(Result); raise; end;
end;

{$ifdef FPC}

function URLReadXML(const URL: String; const BlowFishKeyPhrase: string): TXMLDocument;
begin
  try
    // URLReadXML and ReadXMLFile nil the parameter when there's no need to free it
    URLReadXML(Result, URL, BlowFishKeyPhrase);
  except FreeAndNil(Result); raise; end;
end;

procedure URLWriteXML(Doc: TXMLDocument; const URL: String; const BlowFishKeyPhrase: string);
var
  Stream: TStream;
  EncryptStream: TBlowFishEncryptStream;
begin
  Stream := URLSaveStream(URL);
  try
    EncryptStream := TBlowFishEncryptStream.Create(BlowFishKeyPhrase, Stream);
    try
      WriteXMLFile(Doc, EncryptStream);
    finally FreeAndNil(EncryptStream) end;
  finally FreeAndNil(Stream) end;
end;

{$endif}

procedure URLWriteXML(Doc: TXMLDocument; const URL: String);
var
  Stream: TStream;
  StreamOptions: TSaveStreamOptions;
  gzipped: boolean;
begin
  //guess gzipped based on file extension
  StreamOptions := [];
  URIMimeType(URL, Gzipped);
  if Gzipped then
    Include(StreamOptions, ssoGzip);

  Stream := URLSaveStream(URL, StreamOptions);
  try
    WriteXMLFile(Doc, Stream);
  finally FreeAndNil(Stream) end;
end;

end.
