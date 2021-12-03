{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for CastleInternalInspector. }
unit CastleInternalInspectorUtils;

{$I castleconf.inc}

interface

uses TypInfo;

function PropertyGet(const PropObject: TObject; const PropInfo: PPropInfo; out Name, Value: String): Boolean;

implementation

uses SysUtils,
  CastleUtils, CastleVectors;

function PropertyGet(const PropObject: TObject; const PropInfo: PPropInfo; out Name, Value: String): Boolean;

  function ObjectToString(const O: TObject): String;
  begin
    if O = nil then
      Result := 'nil'
    else
    if O is TCastleVector2Persistent then
      Result := TCastleVector2Persistent(O).Value.ToString
    else
    if O is TCastleVector3Persistent then
      Result := TCastleVector3Persistent(O).Value.ToString
    else
    if O is TCastleVector4Persistent then
      Result := TCastleVector4Persistent(O).Value.ToString
    else
      Result := O.ClassName;
  end;

var
  PropType: PTypeInfo;
begin
  Name := PropInfo^.Name;
  PropType := PropInfo^.PropType;

  Result := true;
  case PropType^.Kind of
    tkInteger:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
    tkEnumeration:
      Value := GetEnumName(PropType, GetOrdProp(PropObject, PropInfo));
    tkFloat:
      Value := FloatToStrDot(GetFloatProp(PropObject, PropInfo));
    //tkSet: TODO
    tkChar:
      Value := Char(GetOrdProp(PropObject, PropInfo));
    tkSString,
    tkLString,
    tkAString:
      Value := GetStrProp(PropObject, PropInfo);
    tkWString:
      Value := UTF8Encode(GetWideStrProp(PropObject, PropInfo));
    tkVariant:
      Value := GetVariantProp(PropObject, PropInfo);
    tkClass:
      Value := ObjectToString(GetObjectProp(PropObject, PropInfo));
    tkWChar:
      Value := UTF8Encode(WideChar(GetOrdProp(PropObject, PropInfo)));
    tkBool:
      Value := BoolToStr(GetOrdProp(PropObject, PropInfo) <> 0, true);
    tkInt64:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
    tkQWord:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
    //tkObject:
    // tkArray,
    // tkRecord,
    // tkInterface,
    // tkDynArray,
    // tkInterfaceRaw,
    // tkProcVar,
    // tkMethod:
    tkUString :
      Value := UTF8Encode(GetWideStrProp(PropObject, PropInfo));
    tkUChar:
      Value := UTF8Encode(UnicodeChar(GetOrdProp(PropObject, PropInfo)));
    else
      Result := false;
  end;
end;

end.
