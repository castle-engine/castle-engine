{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit KambiXMLCfg;

interface

uses KambiUtils, XMLCfg, DOM;

type
  { This is descendant of TXMLConfig that adds
    GetFloat, SetFloat, SetDeleteFloat for the Float type.

    Note: at the beginning I named them GetValue, SetValue etc.
    and made them overloaded. But this is *very* bad idea.
    Why ? Because integers are casted to floats without any problems,
    and this may cause choosing wrong overloaded version.
    Consider that default value for some float parameter is integer
    (e.g. because it was declared as an integer, I forgot to
    write "0.0" instead of "0" etc.). Then
      MyValue := GetValue(Name, IntegerValue);
    will choose GetValue that interprets given value as an integer.
    Although MyValue is variable of type float, you can assign integer
    to a float without any problem, so again no compile-time error.
    But this is obviously wrong --- if float value was recorded
    in the file, it will be read incorrectly. }
  TKamXMLConfig = class(TXMLConfig)
  public
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float;

    procedure SetFloat(const APath: string;
      const AValue: Float);

    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);

    { For a given path, return correspond DOM element of XML tree.
      This is useful if you want to mix XMLConfig style operations
      on the file and then use some real DOM functions to more directly
      operate/read on XML document.

      Note that for paths that you pass to various SetValue versions,
      the last path component is the attribute name. You do not pass
      this here. Path passed here should end with the name of final
      element.

      Path passed here may but doesn't have to be terminated by a final slash.
      In fact, for now the path is just splitted using slash character
      as a separator, so a path like @code(/some////path/) is equivalent
      to a path like (some/path). But don't depend on this behavior.

      Returns nil if there is no such element.

      Remember that XMLConfig idea of XML document is limited.
      That's intentional (XMLConfig is supposed to offer only a simple limited
      XML access), and this means that some XML trees may confuse XMLConfig.
      For example, if there are two elements with the same TagName as a children
      of the same element: XMLConfig will (probably ?) just always ignore
      the second one. Which means that if you use this method to change
      some XML content, you should be careful when accessing this content
      from regular XMLConfig Get/SetValue methods. }
    function PathElement(const APath: string): TDOMElement;
  end;

implementation

uses SysUtils, KambiStringUtils;

{ TKamXMLConfig -------------------------------------------------------------- }

function TKamXMLConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

procedure TKamXMLConfig.SetFloat(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStr(AValue));
end;

procedure TKamXMLConfig.SetDeleteFloat(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStr(AValue), FloatToStr(ADefaultValue));
end;

function TKamXMLConfig.PathElement(const APath: string): TDOMElement;

  { Find a children element, nil if not found. }
  function FindElementChildren(Element: TDOMElement;
    const ElementName: string): TDOMElement;
  var
    Node: TDOMNode;
  begin
    Node := Element.FindNode(ElementName);
    if (Node <> nil) and (Node.NodeType = ELEMENT_NODE) then
      Result := Node as TDOMElement else
      Result := nil;
  end;

var
  SeekPos: Integer;
  PathComponent: string;
begin
  Result := Doc.DocumentElement;
  SeekPos := 1;
  while Result <> nil do
  begin
    PathComponent := NextToken(APath, SeekPos, ['/']);
    if PathComponent = '' then break;
    Result := FindElementChildren(Result, PathComponent);
  end;
end;

end.