{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Class helper to read CastleScript expressions from XML config files
  (TCastleConfigScriptHelper). }
unit CastleScriptConfig;

interface

uses Math,
  CastleKeysMouse, CastleXMLConfig;

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
  end;

implementation

uses SysUtils,
  CastleScript, CastleScriptParser;

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

end.
