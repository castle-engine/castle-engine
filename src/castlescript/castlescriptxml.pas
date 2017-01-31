{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers to read CastleScript expressions from XML config files
  (TCastleConfigScriptHelper, TDOMElementScriptHelper). }
unit CastleScriptXML;

{$I castleconf.inc}

interface

uses Math, DOM,
  CastleKeysMouse, CastleXMLConfig, CastleXMLUtils;

type
  { Class helper to read CastleScript expressions from XML config files.
    Use this unit, and then you can call powerful @link(GetFloatExpression)
    instead of @link(TCastleConfig.GetFloat). }
  TCastleConfigScriptHelper = class helper(TCastleConfigKeysMouseHelper) for TCastleConfig

    { Read a float expression composed in CastleScript,
      like @code("123.0") or @code("3.0 * 2.0") or @code("sin(2.0)").
      @groupBegin }
    function GetFloatExpression(const APath: string;
      const ADefaultValue: Float): Float;
    function GetFloatExpression(const APath: string;
      const ADefaultValue: string): Float;
    function GetFloatExpression(const APath: string): Float;
    { @groupEnd }

    { Read an integer expression composed in CastleScript,
      like @code("123") or @code("3 * 2 + 5").
      @groupBegin }
    function GetIntExpression(const APath: string;
      const ADefaultValue: Int64): Int64;
    function GetIntExpression(const APath: string;
      const ADefaultValue: string): Int64;
    function GetIntExpression(const APath: string): Int64;
    { @groupEnd }
  end;

  { Class helper to read CastleScript expressions from DOM (XML files).
    Use this unit, and then you can call powerful methods
    @link(AttributeFloatExpression), @link(AttributeFloatExpressionDef)
    instead of the
    @link(TDOMElementHelper.AttributeFloat), @link(TDOMElementHelper.AttributeFloatDef). }
  TDOMElementScriptHelper = class helper(TDOMElementHelper) for TDOMElement

    { Read from Element attribute value as a Float expression, and returns @true.
      The expression can be anything in CastleScript
      http://castle-engine.sourceforge.net/castle_script.php ,
      for example something crazy like @code(2.0 * sin(5.0) + Pi).
      You can use this method instead of @link(AttributeFloat) to easily allow
      mathematical exressions in XML attributes.

      If there is no such attribute returns @false and does not modify Value. }
    function AttributeFloatExpression(const AttrName: string; var Value: Float): boolean;

    { Retrieves from Element given attribute as a Float expression,
      raises EDOMAttributeMissing if missing.

      The expression can be anything in CastleScript
      http://castle-engine.sourceforge.net/castle_script.php ,
      for example something crazy like @code(2.0 * sin(5.0) + Pi).
      You can use this method instead of @link(AttributeFloat) to easily allow
      mathematical exressions in XML attributes.

      @raises EDOMAttributeMissing }
    function AttributeFloatExpression(const AttrName: string): Float;

    { Retrieves from Element given attribute as a Float expression, or a default value.

      The expression can be anything in CastleScript
      http://castle-engine.sourceforge.net/castle_script.php ,
      for example something crazy like @code(2.0 * sin(5.0) + Pi).
      You can use this method instead of @link(AttributeFloatDef) to easily allow
      mathematical exressions in XML attributes. }
    function AttributeFloatExpressionDef(const AttrName: string; const DefaultValue: Float): Float;
  end;

implementation

uses SysUtils,
  CastleScript, CastleScriptParser;

{ TCastleConfigScriptHelper -------------------------------------------------- }

function TCastleConfigScriptHelper.GetFloatExpression(const APath: string;
  const ADefaultValue: Float): Float;
begin
  Result := GetFloatExpression(APath, FloatToStr(ADefaultValue));
end;

function TCastleConfigScriptHelper.GetFloatExpression(const APath: string;
  const ADefaultValue: string): Float;
var
  ResultString: string;
  E: TCasScriptExpression;
begin
  ResultString := GetValue(APath, ADefaultValue);
  E := ParseFloatExpression(ResultString, []);
  try
    Result := E.AsFloat;
  finally FreeAndNil(E) end;
end;

function TCastleConfigScriptHelper.GetFloatExpression(const APath: string): Float;
var
  E: TCasScriptExpression;
begin
  E := ParseFloatExpression(GetStringNonEmpty(APath), []);
  try
    Result := E.AsFloat;
  finally FreeAndNil(E) end;
end;

function TCastleConfigScriptHelper.GetIntExpression(const APath: string;
  const ADefaultValue: Int64): Int64;
begin
  Result := GetIntExpression(APath, IntToStr(ADefaultValue));
end;

function TCastleConfigScriptHelper.GetIntExpression(const APath: string;
  const ADefaultValue: string): Int64;
var
  ResultString: string;
  E: TCasScriptExpression;
begin
  ResultString := GetValue(APath, ADefaultValue);
  E := ParseIntExpression(ResultString, []);
  try
    Result := E.AsInt;
  finally FreeAndNil(E) end;
end;

function TCastleConfigScriptHelper.GetIntExpression(const APath: string): Int64;
var
  E: TCasScriptExpression;
begin
  E := ParseIntExpression(GetStringNonEmpty(APath), []);
  try
    Result := E.AsInt;
  finally FreeAndNil(E) end;
end;

{ TDOMElementScriptHelper ---------------------------------------------------- }

function TDOMElementScriptHelper.AttributeFloatExpression(
  const AttrName: string; var Value: Float): boolean;
var
  ValueStr: string;
  E: TCasScriptExpression;
begin
  Result := AttributeString(AttrName, ValueStr);
  if Result then
  begin
    E := ParseFloatExpression(ValueStr, []);
    try
      Value := E.AsFloat;
    finally FreeAndNil(E) end;
  end;
end;

function TDOMElementScriptHelper.AttributeFloatExpression(const AttrName: string): Float;
begin
  if not AttributeFloatExpression(AttrName, Result) then
    raise EDOMAttributeMissing.CreateFmt('Missing required (float) attribute "%s" on element "%s"', [AttrName, TagName]);
end;

function TDOMElementScriptHelper.AttributeFloatExpressionDef(const AttrName: string; const DefaultValue: Float): Float;
begin
  if not AttributeFloatExpression(AttrName, Result) then
    Result := DefaultValue;
end;

end.
