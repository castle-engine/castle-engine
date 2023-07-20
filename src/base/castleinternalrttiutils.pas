{
  Copyright 2021-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ RTTI utilities. }
unit CastleInternalRttiUtils;

{$I castleconf.inc}

interface

uses TypInfo;

{ Get the property name and value as Strings (e.g. to display in Object Inspector). }
function PropertyGet(const PropObject: TObject; const PropInfo: PPropInfo;
  out Name, Value: String): Boolean;

{ Does the property have a default value now.
  This is 100% reliable to use to avoid serializing things that have default value.
  Their state is equal to the state when object is created.

  Note that this is not a complete check to determine
  should the property be serialized or not.
  You should also check @code(IsStoredProp(PropObject, PropInfo))
  in case the property has a "stored" method.
  IsStoredProp works in all cases (whether "stored" indicates a constant,
  field or some method) and it is @true if there was no "stored" value.
  So the full check is

  @longCode(#
    PropertyHasDefaultValue(PropObject, PropInfo) and
    IsStoredProp(PropObject, PropInfo)
  #)

  @param(TreatSubComponentAsDefault
    Determines how to treat subcomponents,
    i.e. instances with csSubComponent placed in properties
    (with the exception of subcomponents with
    TCastleComponent.ValueIsStreamed overridden, like TCastleVectorXxPersistent).

    The idea of subcomponent is that these subcomponents are automatically
    created and owned by parent. So their existence is by definition
    "default state". But their contents may have
    been modified compared to a default state.

    By default (when TreatSubComponentAsDefault is @false)
    we return @false for such subcomponents.
    In case of TCastleComponent, this means we return "not PropObject.ValueIsStreamed".
    This is safer and better for serialization system:
    serialization should store the contents of such subcomponent.

    When TreatSubComponentAsDefault is @true,
    we return @true for them. Even though their contents may be non-default.
    This is useful for object inspector display,
    that may display subcomponents expanded,
    so particular subcomponent's non-default properties would be emphasized anyway.

    Note: LCL object inspector avoids the need for this by
    implementing TClassPropertyEditor.ValueIsStreamed that scans
    all property editors in the property of subcomponent.

    Note that TreatSubComponentAsDefault doesn't matter
    when PropObject is TCastleComponent with
    TCastleComponent.ValueIsStreamed overridden.
    In this case we always return "not PropObject.ValueIsStreamed",
    regardless of TreatSubComponentAsDefault value.
    This means TreatSubComponentAsDefault doesn't affect display
    of components like TCastleVectorXxPersistent.
  ) }
function PropertyHasDefaultValue(const PropObject: TObject;
  const PropInfo: PPropInfo; const TreatSubComponentAsDefault: Boolean = false): Boolean;

implementation

uses SysUtils, RtlConsts, Classes,
  CastleUtils, CastleStringUtils, CastleClassUtils;

{$ifndef FPC}
resourcestring
  SErrNoVariantSupport = 'No variant support for properties. Please use the variants unit in your project and recompile';
{$endif}

var
  InternalComponent: TCastleComponent;

{ C is TCastleComponent and it overriddes TCastleComponent.ValueIsStreamed.

  Note: In general checking for such "is this method overridden" is dirty,
  as not expected by developers. But in this case it's used only for internal
  functionality in this unit, and only for display of properties
  in CGE inspector.

  For now this is OK. }
function CustomizedValueIsStreamed(const C: TObject): Boolean;
type
  TValueIsStreamed = function: Boolean of object;
var
  CurMethod, NotOverriddedMethod: TValueIsStreamed;
begin
  Result := false;
  if C is TCastleComponent then
  begin
    CurMethod := {$ifdef FPC}@{$endif} TCastleComponent(C).ValueIsStreamed;
    NotOverriddedMethod := {$ifdef FPC}@{$endif} InternalComponent.ValueIsStreamed;
    Result := TMethod(CurMethod).Code <> TMethod(NotOverriddedMethod).Code;
  end;
end;

function PropertyGet(const PropObject: TObject; const PropInfo: PPropInfo; out Name, Value: String): Boolean;

  function ObjectToString(const O: TObject): String;
  begin
    if O = nil then
      Result := 'nil'
    else
    if CustomizedValueIsStreamed(O) then
    begin
      { We rely here on conventions about objects that implement ValueIsStreamed:
        - Their Name may end with "Persistent" suffix which we want to hide,
          to show in inspector e.g. 'Translation' instead of 'TranslationPersistent'.
        - They implement ToString to show something useful. }
      Result := O.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TComponent then
      Result := TComponent(O).Name + ': ' + O.ClassName
    else
      Result := O.ClassName;
  end;

var
  PropType: PTypeInfo;
begin
  Name := PropInfo^.Name;
  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};

  Result := true;
  case PropType^.Kind of
    tkInteger:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
    tkEnumeration:
      Value := GetEnumName(PropType, GetOrdProp(PropObject, PropInfo));
{$ifndef FPUNONE}
    tkFloat:
      Value := FloatToStrDot(GetFloatProp(PropObject, PropInfo));
{$endif}
    //tkSet: TODO
    tkChar:
      Value := Char(GetOrdProp(PropObject, PropInfo));
{$ifdef FPC}
    tkSString, tkLString, tkAString:
{$else}
    tkString, tkLString:
{$endif}
      Value := GetStrProp(PropObject, PropInfo);
    tkWString:
      Value := UTF8Encode(GetWideStrProp(PropObject, PropInfo));
    tkVariant:
      Value := GetVariantProp(PropObject, PropInfo);
    tkClass:
      Value := ObjectToString(GetObjectProp(PropObject, PropInfo));
    tkWChar:
      Value := UTF8Encode(WideChar(GetOrdProp(PropObject, PropInfo)));
{$ifdef FPC}
    tkBool:
      Value := BoolToStr(GetOrdProp(PropObject, PropInfo) <> 0, true);
{$endif}
    tkInt64:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
{$ifdef FPC}
    tkQWord:
      Value := IntToStr(GetOrdProp(PropObject, PropInfo));
{$endif}
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
{$ifdef FPC}
    tkUChar:
      Value := UTF8Encode(UnicodeChar(GetOrdProp(PropObject, PropInfo)));
{$endif}
    else
      Result := false;
  end;
end;

function PropertyHasDefaultValue(const PropObject: TObject;
  const PropInfo: PPropInfo; const TreatSubComponentAsDefault: Boolean): Boolean;

{ Implemented looking closely at what standard FPC writer does,
  3.0.4/fpcsrc/rtl/objpas/classes/writer.inc ,
  and simplified a lot for CGE purposes. }

var
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
{$ifndef FPUNONE}
  FloatValue, DefFloatValue: Extended;
{$endif}
  MethodValue, DefMethodValue: TMethod;
  VarValue, DefVarValue: tvardata;
  BoolValue, DefBoolValue, DefValueUse: Boolean;
  C: TObject;
begin
  Result := false; // for unknown types, assume false

  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};
  DefValue := PropInfo^.Default;
  { $80000000 means that there's no default value (in case of Single or String,
    you need to specify it by "nodefault") }
  DefValueUse := DefValue <> LongInt($80000000);
  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(PropObject, PropInfo);
        Result := (Value = DefValue) and DefValueUse;
      end;
{$ifndef FPUNONE}
    tkFloat:
      begin
        FloatValue := GetFloatProp(PropObject, PropInfo);
        DefFloatValue := PSingle(@PropInfo^.Default)^;
        Result := (FloatValue = DefFloatValue) and DefValueUse;
      end;
{$endif}
    tkMethod:
      begin
        MethodValue := GetMethodProp(PropObject, PropInfo);
        DefMethodValue.Data := nil;
        DefMethodValue.Code := nil;
        Result := SameMethods(MethodValue, DefMethodValue);
      end;
{$ifdef FPC}
    tkSString, tkLString, tkAString:
      begin
        Result := (GetStrProp(PropObject, PropInfo) = '') and DefValueUse;
      end;
{$else}
    tkString, tkLString:
      begin
        Result := (GetAnsiStrProp(PropObject, PropInfo) = '') and DefValueUse;
      end;
{$endif}
    tkWString:
      begin
        Result := (GetWideStrProp(PropObject, PropInfo) = '') and DefValueUse;
      end;
    tkUString:
      begin
        Result := (GetUnicodeStrProp(PropObject, PropInfo) = '') and DefValueUse;
      end;
    tkVariant:
      begin
        { Ensure that a Variant manager is installed }
        if not Assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);
        VarValue := tvardata(GetVariantProp(PropObject, PropInfo));
        FillChar(DefVarValue,sizeof(DefVarValue),0);
        {$ifdef FPC}
        Result := CompareByte(VarValue,DefVarValue,sizeof(VarValue)) = 0;
        {$else}
        Result := CompareMem(@VarValue, @DefVarValue, sizeof(VarValue));
        {$endif}
      end;
    tkClass:
      begin
        C := GetObjectProp(PropObject, PropInfo);
        Result :=
          (C = nil) or
          (
            { When TreatSubComponentAsDefault:
              Treat subcomponents
              (except the ones overriding TCastleComponent.ValueIsStreamed)
              as having "default value", without analyzing the contents. }
            TreatSubComponentAsDefault and
            (C is TComponent) and
            (csSubComponent in TComponent(C).ComponentStyle) and
            (not CustomizedValueIsStreamed(C))
          ) or
          (
            { Avoid saving common subcomponents with all values left as default.
              This makes .castle-user-interface files smaller, which makes also their
              diffs easier to read (useful when commiting them, reviewing etc.) }
            (C is TCastleComponent) and
            (not TCastleComponent(C).ValueIsStreamed)
          );
      end;
    tkInt64{$ifdef FPC}, tkQWord{$endif}:
      begin
        Result := GetInt64Prop(PropObject, PropInfo) = 0;
      end;
{$ifdef FPC}
    tkBool:
      begin
        BoolValue := GetOrdProp(PropObject, PropInfo)<>0;
        DefBoolValue := DefValue <> 0;
        Result := (BoolValue = DefBoolValue) and DefValueUse;
      end;
{$endif}
    else ;
  end;
end;

initialization
  InternalComponent := TCastleComponent.Create(nil);
finalization
  FreeAndNil(InternalComponent);
end.
