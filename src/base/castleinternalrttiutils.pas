{
  Copyright 2021-2022 Michalis Kamburelis.

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
  TThis is 100% reliable to use to avoid serializing things that have default value.
  Their state is equal to the state when object is created.

  @param(TreatSubComponentAsDefault
    Determines how to treat subcomponents,
    i.e. instances with csSubComponent placed in properties,
    (with the exception of some special subcomponents with HasDefaultValue
    methods, like TCastleVectorXxPersistent).

    The idea of subcomponent is that these subcomponents are automatically
    created and owned by parent. So their existence is by definition
    "default state". But their contents may have
    been modified compared to a default state.

    By default (when TreatSubComponentAsDefault is @false) we return @false for such subcomponents.
    This is safer and better for serialization system:
    serialization should store the contents of such subcomponent.

    When TreatSubComponentAsDefault is @true,
    we return @true for them. Even though their contents may be non-default.
    This is useful for object inspector that may display subcomponents expanded,
    so particular subcomponent's non-default properties would be emphasized anyway.

    Note that TreatSubComponentAsDefault doesn't matter for behavior on
    special subcomponents with HasDefaultValue
    methods, like TCastleVectorXxPersistent.
  ) }
function PropertyHasDefaultValue(const PropObject: TObject;
  const PropInfo: PPropInfo; const TreatSubComponentAsDefault: Boolean = false): Boolean;

implementation

uses SysUtils, RtlConsts, Classes,
  CastleUtils, CastleVectors, CastleStringUtils, CastleColors, CastleRectangles;

{$ifndef FPC}
resourcestring
  SErrNoVariantSupport = 'No variant support for properties. Please use the variants unit in your project and recompile';
{$endif}

function PropertyGet(const PropObject: TObject; const PropInfo: PPropInfo; out Name, Value: String): Boolean;

  function ObjectToString(const O: TObject): String;
  begin
    if O = nil then
      Result := 'nil'
    else
    if O is TCastleVector2Persistent then
    begin
      Result := TCastleVector2Persistent(O).Value.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TCastleVector3Persistent then
    begin
      Result := TCastleVector3Persistent(O).Value.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TCastleVector4Persistent then
    begin
      Result := TCastleVector4Persistent(O).Value.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TCastleColorPersistent then
    begin
      Result := TCastleColorPersistent(O).Value.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TCastleColorRGBPersistent then
    begin
      Result := TCastleColorRGBPersistent(O).Value.ToString;
      Name := SuffixRemove('persistent', Name, true);
    end else
    if O is TFloatRectanglePersistent then
    begin
      Result := TFloatRectanglePersistent(O).Value.ToString;
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
              Treat subcomponents (except the ones implementing HasDefaultValue)
              as having "default value", without analyzing the contents. }
            TreatSubComponentAsDefault and
            (C is TComponent) and
            (csSubComponent in TComponent(C).ComponentStyle) and
            (C.ClassType <> TCastleVector2Persistent) and
            (C.ClassType <> TCastleVector3Persistent) and
            (C.ClassType <> TCastleVector4Persistent) and
            (C.ClassType <> TCastleColorRGBPersistent) and
            (C.ClassType <> TCastleColorPersistent) and
            (C.ClassType <> TBorder)
          ) or
          (
            { Avoid saving common subcomponents with all values left as default.
              This makes .castle-user-interface files smaller, which makes also their
              diffs easier to read (useful when commiting them, reviewing etc.)

              The TCastleVector*Persistent and TBorder do not descend from TComponent
              so we cannot actually check are they subcomponents:

                (C is TComponent) and
                (csSubComponent in TComponent(C).ComponentStyle) and

              TODO: no longer true for TCastleVector*Persistent.
            }
            ((C.ClassType = TCastleVector2Persistent) and TCastleVector2Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleVector3Persistent) and TCastleVector3Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleVector4Persistent) and TCastleVector4Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleVector4RotationPersistent) and TCastleVector4RotationPersistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleColorRGBPersistent) and TCastleColorRGBPersistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleColorPersistent) and TCastleColorPersistent(C).HasDefaultValue) or
            ((C.ClassType = TBorder) and TBorder(C).HasDefaultValue)
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

end.
