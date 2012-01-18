{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Storing configuration files in XML (TCastleConfig). }
unit CastleXMLConfig;

{ In new FPC versions, XMLConf unit is advised and XMLCfg is deprecated.
  See e.g. [http://www.mail-archive.com/lazarus@lists.lazarus.freepascal.org/msg09489.html].
  But XMLConf requires adding units to your uses clause that are otherwise
  not needed:

    This binary has no unicodestrings support compiled in.
    Recompile the application with a unicodestrings-manager in the program uses clause.

  So we keep using XMLCfg for now. Undefine USE_OLD_XMLCFG if you wish
  to use XMLConf. }
{$define USE_OLD_XMLCFG}

interface

uses CastleUtils, {$ifdef USE_OLD_XMLCFG} XMLCfg {$else} XMLConf {$endif}, DOM,
  VectorMath, KeysMouse;

type
  { Store configuration in XML format.

    This is a descendant of TXMLConfig that adds various small extensions:
    float types (GetFloat, SetFloat, SetDeleteFloat),
    vector types, key (TKey) types,
    PathElement utility. }
  TCastleConfig = class(TXMLConfig)
  public
    { Internal notes: At the beginning I made the float methods
      to overload existing names (GetValue, SetValue etc.).

      But this turned out to be a *very* bad idea: integers are
      casted to floats automatically, and this means that it's too
      easy to use integer getter to read a value that may be float.
      Consider that default value for some float parameter is of integer type
      (e.g. because it was declared as an integer, because you forget to
      write "0.0" instead of "0" etc.). Then

        MyValue := GetValue('float_param', 0);

      will choose GetValue that interprets given value as an integer.
      If you perviously stored a float value there
      (like by SetValue('float_param', 3.14)) then the GetValue above
      will compile but fail miserably at runtime }

    { }
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float;
    procedure SetFloat(const APath: string;
      const AValue: Float);
    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);

    function GetValue(const APath: string;
      const ADefaultValue: TVector3Single): TVector3Single; overload;
    procedure SetValue(const APath: string;
      const AValue: TVector3Single); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector3Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TVector4Single): TVector4Single; overload;
    procedure SetValue(const APath: string;
      const AValue: TVector4Single); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector4Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TKey): TKey; overload;
    procedure SetValue(const APath: string;
      const AValue: TKey); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TKey); overload;

    { For a given path, return corresponding DOM element of XML tree.
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

procedure Register;

implementation

uses SysUtils, CastleStringUtils, Classes;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleConfig]);
end;

{ TCastleConfig -------------------------------------------------------------- }

function TCastleConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

procedure TCastleConfig.SetFloat(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStr(AValue));
end;

procedure TCastleConfig.SetDeleteFloat(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStr(AValue), FloatToStr(ADefaultValue));
end;

const
  VectorComponentPaths: array [0..3] of string =
  ('/x', '/y', '/z', '/w');

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector3Single): TVector3Single;
var
  I: Integer;
begin
  for I := 0 to 2 do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to 2 do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to 2 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector4Single): TVector4Single;
var
  I: Integer;
begin
  for I := 0 to 3 do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to 3 do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to 3 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TKey): TKey;
begin
  Result := StrToKey(GetValue(APath, KeyToStr(ADefaultValue)), ADefaultValue);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TKey);
begin
  SetValue(APath, KeyToStr(AValue));
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TKey);
begin
  SetDeleteValue(APath, KeyToStr(AValue), KeyToStr(ADefaultValue));
end;

function TCastleConfig.PathElement(const APath: string): TDOMElement;

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
