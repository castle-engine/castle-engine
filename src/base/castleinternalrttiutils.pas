{
  Copyright 2021-2024 Michalis Kamburelis.

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

type
  { Property type used by PropertyType function that also decides
    which other routines from this unit are guaranteed to work.
    For example, if a PropertyType is ptBoolean,
    then PropertyGetBoolean and PropertySetBoolean can be used. }
  TPropertyType = (
    { Any integer type that fits witihin 64-bits.
      This includes Integer, Int64 and QWord (only these can be published in Pascal). }
    ptInteger,

    { Single, 32-bit floating point number.
      Other floating point types cannot be published in Pascal
      (if they ever will, we will extend this property type to account for them,
      and make PropertyGetFloat and PropertySetFloat return a more general type
      like Extended or Double or Math.Float). }
    ptFloat,

    { Any String type.
      This includes ShortString, AnsiString, WideString, UnicodeString,
      UTF8String.
      We convert them all to just String, following our conventions
      what does it mean: https://castle-engine.io/coding_conventions#strings_unicode . }
    ptString,

    { Boolean or any similar type that in principle holds only 2 values,
      false and true. Like LongBool. }
    ptBoolean,

    { Class instance, anything descending from TObject. }
    ptInstance,

    { Any other property type not handled by our code. }
    ptOther
  );

{ Get property type as TPropertyType.

  This is an abstraction over the FPC / Delphi possible property types,
  intended to be

  @unorderedList(
    @item(cross-compiler (e.g. it hides how FPC / Delphi encode Boolean properties),)

    @item(
      limited to CGE purposes (e.g. we don't need to distinguish between
      various string types, we want to use just String and follow our conventions
      https://castle-engine.io/coding_conventions#strings_unicode ),
    )

    @item(and, because of above, simpler.)
  )
}
function PropertyType(const PropInfo: PPropInfo): TPropertyType;

{ Get a property of type Boolean.
  Use only when PropertyType is ptBoolean, undefined what happens otherwise.
  @groupBegin }
function PropertyGetBoolean(const PropObject: TObject; const PropInfo: PPropInfo): Boolean;
procedure PropertySetBoolean(const PropObject: TObject; const PropInfo: PPropInfo; const NewValue: Boolean);
{ @groupEnd }

{ Get or set a property of type Integer, Int64 or QWord.
  Use only when PropertyType is ptInteger, undefined what happens otherwise.
  Do not use with QWord values outside of Int64 range -- undefined what happens
  (for now, we just cast QWord to Int64).
  @groupBegin }
function PropertyGetInteger(const PropObject: TObject; const PropInfo: PPropInfo): Int64;
procedure PropertySetInteger(const PropObject: TObject; const PropInfo: PPropInfo; const Value: Int64);
{ @groupEnd }

{ Get or set a property of type Single.
  Use only when PropertyType is ptFloat, undefined what happens otherwise.
  @groupBegin }
function PropertyGetFloat(const PropObject: TObject; const PropInfo: PPropInfo): Single;
procedure PropertySetFloat(const PropObject: TObject; const PropInfo: PPropInfo; const Value: Single);
{ @groupEnd }

{ Get or set a property of type String.
  Use only when PropertyType is ptString, undefined what happens otherwise.
  @groupBegin }
function PropertyGetString(const PropObject: TObject; const PropInfo: PPropInfo): String;
procedure PropertySetString(const PropObject: TObject; const PropInfo: PPropInfo; const Value: String);
{ @groupEnd }

{ Get or set a property of type TObject (object instance).
  Use only when PropertyType is ptInstance, undefined what happens otherwise.
  @groupBegin }
function PropertyGetInstance(const PropObject: TObject; const PropInfo: PPropInfo): TObject;
procedure PropertySetInstance(const PropObject: TObject; const PropInfo: PPropInfo; const Value: TObject);
{ @groupEnd }

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
  Result := true;

  case PropertyType(PropInfo) of
    ptInteger:
      Value := IntToStr(PropertyGetInteger(PropObject, PropInfo));
    ptFloat:
      Value := FloatToStrDot(PropertyGetFloat(PropObject, PropInfo));
    ptString:
      Value := PropertyGetString(PropObject, PropInfo);
    ptBoolean:
      Value := BoolToStr(PropertyGetBoolean(PropObject, PropInfo), true);
    ptInstance:
      Value := ObjectToString(PropertyGetInstance(PropObject, PropInfo));
    ptOther:
      begin
        { Handle additional property types that are not handled byPropertyType
          for now, but we want to return something useful from PropertyGet
          for them. }
        PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};
        case PropType^.Kind of
          tkEnumeration:
            Value := GetEnumName(PropType, GetOrdProp(PropObject, PropInfo));
          tkChar:
            Value := Char(GetOrdProp(PropObject, PropInfo));
          tkVariant:
            Value := GetVariantProp(PropObject, PropInfo);
          tkWChar:
            Value := UTF8Encode(WideChar(GetOrdProp(PropObject, PropInfo)));
          {$ifdef FPC}
          tkUChar:
            Value := UTF8Encode(UnicodeChar(GetOrdProp(PropObject, PropInfo)));
          {$endif}
          // TODO: These are unhandled now:
          // tkSet:
          // tkObject:
          // tkArray,
          // tkRecord,
          // tkInterface,
          // tkDynArray,
          // tkInterfaceRaw,
          // tkProcVar,
          // tkMethod:
          else
            Result := false;
        end;
      end;
  end;
end;

function PropertyHasDefaultValue(const PropObject: TObject;
  const PropInfo: PPropInfo; const TreatSubComponentAsDefault: Boolean): Boolean;

{ Implemented looking closely at what standard FPC writer does,
  3.0.4/fpcsrc/rtl/objpas/classes/writer.inc ,
  and simplified a lot for CGE purposes. }

var
  PropType: PTypeInfo;
  Value, DefValue: Int32;
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

  { $80000000 means that there's no default value (in case of Single or String
    or Int64, you need to specify it by "nodefault") }
  DefValueUse := DefValue <> Int32($80000000);
  if (not DefValueUse) and
     { PropInfo^.Default doesn't matter for our tkClass logic.
       And it seems it may be $80000000 always (with FPC 3.3.1 at least). }
     (PropType^.Kind <> tkClass) then
    Exit;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(PropObject, PropInfo);
        Result := Value = DefValue;
      end;
{$ifndef FPUNONE}
    tkFloat:
      begin
        FloatValue := GetFloatProp(PropObject, PropInfo);
        DefFloatValue := PSingle(@PropInfo^.Default)^;
        Result := FloatValue = DefFloatValue;
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
        Result := GetStrProp(PropObject, PropInfo) = '';
      end;
{$else}
    tkString, tkLString:
      begin
        Result := GetAnsiStrProp(PropObject, PropInfo) = '';
      end;
{$endif}
    tkWString:
      begin
        Result := GetWideStrProp(PropObject, PropInfo) = '';
      end;
    tkUString:
      begin
        Result := GetUnicodeStrProp(PropObject, PropInfo) = '';
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
        Result := BoolValue = DefBoolValue;
      end;
{$endif}
    else ;
  end;
end;

function PropertyType(const PropInfo: PPropInfo): TPropertyType;
var
  PropType: PTypeInfo;
begin
  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};

  case PropType^.Kind of
    tkInteger, tkInt64 {$ifdef FPC}, tkQWord{$endif}:
      Result := ptInteger;
{$ifndef FPUNONE}
    tkFloat:
      Result := ptFloat;
{$endif}
{$ifdef FPC}
    tkSString, tkLString, tkAString:
      Result := ptString;
{$else}
    tkString, tkLString:
      Result := ptString;
{$endif}
    tkWString, tkUString:
      Result := ptString;
    tkClass:
      Result := ptInstance;
{$ifdef FPC}
    tkBool:
      Result := ptBoolean;
{$endif}
    else
      Result := ptOther;
  end;

  {$ifndef FPC}
  { Delphi makes it a bit harder to detect ptBoolean,
    using tkEnumeration for Boolean. See
    - http://blong.com/Conferences/BorConUK98/DelphiRTTI/CB140.htm ,
    - https://stackoverflow.com/questions/10188459/how-to-loop-all-properties-in-a-class ,
    - https://en.delphipraxis.net/topic/11756-safegetenumname-a-safer-implementation-of-typinfogetenumname/ ,
    - https://blog.dummzeuch.de/2024/06/21/safegetenumname-a-safer-implementation-of-typinfo-getenumname/
  }
  if (PropType^.Kind = tkEnumeration) and
     (GetTypeData(PropType)^.BaseType^ = TypeInfo(Boolean)) then
    Result := ptBoolean;
  {$endif}
end;

function PropertyGetBoolean(const PropObject: TObject; const PropInfo: PPropInfo): Boolean;
begin
  Assert(PropertyType(PropInfo) = ptBoolean);
  Result := GetOrdProp(PropObject, PropInfo) <> 0;
end;

procedure PropertySetBoolean(const PropObject: TObject; const PropInfo: PPropInfo; const NewValue: Boolean);
begin
  Assert(PropertyType(PropInfo) = ptBoolean);
  SetOrdProp(PropObject, PropInfo, Iff(NewValue, 1, 0));
end;

function PropertyGetInteger(const PropObject: TObject; const PropInfo: PPropInfo): Int64;
var
  PropType: PTypeInfo;
begin
  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};
  case PropType^.Kind of
    tkInteger:
      Result := GetOrdProp(PropObject, PropInfo);
    tkInt64 {$ifdef FPC}, tkQWord{$endif}:
      Result := GetInt64Prop(PropObject, PropInfo);
    else
      raise EInternalError.CreateFmt('PropertyGetInteger called for non-integer property "%s"', [
        PropInfo^.Name
      ]);
  end;
end;

procedure PropertySetInteger(const PropObject: TObject; const PropInfo: PPropInfo; const Value: Int64);
var
  PropType: PTypeInfo;
begin
  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};
  case PropType^.Kind of
    tkInteger:
      SetOrdProp(PropObject, PropInfo, Value);
    tkInt64 {$ifdef FPC}, tkQWord{$endif}:
      SetInt64Prop(PropObject, PropInfo, Value);
    else
      raise EInternalError.CreateFmt('PropertySetInteger called for non-integer property "%s"', [
        PropInfo^.Name
      ]);
  end;
end;

function PropertyGetFloat(const PropObject: TObject; const PropInfo: PPropInfo): Single;
begin
  Assert(PropertyType(PropInfo) = ptFloat);
  Result := GetFloatProp(PropObject, PropInfo);
end;

procedure PropertySetFloat(const PropObject: TObject; const PropInfo: PPropInfo; const Value: Single);
begin
  Assert(PropertyType(PropInfo) = ptFloat);
  SetFloatProp(PropObject, PropInfo, Value);
end;

function PropertyGetString(const PropObject: TObject; const PropInfo: PPropInfo): String;
var
  PropType: PTypeInfo;
begin
  Assert(PropertyType(PropInfo) = ptString);
  PropType := PropInfo^.PropType{$ifndef FPC}^{$endif};
  case PropType^.Kind of
{$ifdef FPC}
    tkSString, tkLString, tkAString:
      Result := GetStrProp(PropObject, PropInfo);
{$else}
    tkString, tkLString:
      Result := GetAnsiStrProp(PropObject, PropInfo);
{$endif}
    tkWString:
      Result := UTF8Encode(GetWideStrProp(PropObject, PropInfo));
    tkUString:
      Result := UTF8Encode(GetUnicodeStrProp(PropObject, PropInfo));
    else
      raise EInternalError.CreateFmt('PropertyGetString called for non-string property "%s"', [
        PropInfo^.Name
      ]);
  end;
end;

procedure PropertySetString(const PropObject: TObject; const PropInfo: PPropInfo; const Value: String);
begin
  Assert(PropertyType(PropInfo) = ptString);
  case PropInfo^.PropType{$ifndef FPC}^{$endif}^.Kind of
{$ifdef FPC}
    tkSString, tkLString, tkAString:
      SetStrProp(PropObject, PropInfo, Value);
{$else}
    tkString, tkLString:
      SetAnsiStrProp(PropObject, PropInfo, Value);
{$endif}
    tkWString:
      SetWideStrProp(PropObject, PropInfo, UTF8Decode(Value));
    tkUString:
      SetUnicodeStrProp(PropObject, PropInfo, UTF8Decode(Value));
    else
      raise EInternalError.CreateFmt('PropertySetString called for non-string property "%s"', [
        PropInfo^.Name
      ]);
  end;
end;

function PropertyGetInstance(const PropObject: TObject; const PropInfo: PPropInfo): TObject;
begin
  Assert(PropertyType(PropInfo) = ptInstance);
  Result := GetObjectProp(PropObject, PropInfo);
end;

procedure PropertySetInstance(const PropObject: TObject; const PropInfo: PPropInfo; const Value: TObject);
begin
  Assert(PropertyType(PropInfo) = ptInstance);
  SetObjectProp(PropObject, PropInfo, Value);
end;

initialization
  InternalComponent := TCastleComponent.Create(nil);
finalization
  FreeAndNil(InternalComponent);
end.
