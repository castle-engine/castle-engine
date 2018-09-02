{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading and writing of hierachy of CGE components
  (TCastleUserInterface, TCastleTransform) to and from files.
  This is used by CGE editor to read/write components,
  and it can be used at runtime by games to instantiate components designed
  in CGE editor. }
unit CastleComponentSerialize;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FpJson, FpJsonRtti, Generics.Collections,
  CastleUIControls, CastleTransform;

type
  EInvalidComponentFile = class(Exception);

  { Internal for InternalAddChild methods. @exclude }
  TCastleComponentReader = class
  private
    FJsonReader: TJSONDeStreamer;
    FOwner: TComponent;
    procedure DeStreamerAfterReadObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
  public
    property JsonReader: TJSONDeStreamer read FJsonReader;
    { Will own all deserialized components. }
    property Owner: TComponent read FOwner;
  end;

{ Save / load TCastleTransform (or descendant) to a .castle-transform file. }
procedure TransformSave(const T: TCastleTransform; const Url: String);
function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;

{ Save / load TCastleUserInterface (or descendant) to a .castle-user-interface file. }
procedure UserInterfaceSave(const C: TCastleUserInterface; const Url: String);
function UserInterfaceLoad(const Url: String; const Owner: TComponent): TCastleUserInterface;

{ Save / load TComponent (or descendant)
  to a .castle-user-interface or .castle-transform file.

  Usually it is more comfortable to use stronger typed
  @link(UserInterfaceSave), @link(UserInterfaceLoad),
  @link(TransformSave), @link(TransformLoad). }
procedure ComponentSave(const C: TComponent; const Url: String);
function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;

{ Register a component that can be serialized and edited using CGE editor.
  @param(Caption Nice caption to show user in the editor.) }
procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const Caption: String);

type
  TRegisteredComponent = class
    ComponentClass: TComponentClass;
    Caption: String;
  end;
  TRegisteredComponents = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TRegisteredComponent>;

{ Read-only list of currently registered
  (using @link(RegisterSerializableComponent)) components. }
function RegisteredComponents: TRegisteredComponents;

implementation

uses JsonParser, TypInfo, RtlConsts,
  CastleFilesUtils, CastleUtils;

{ component registration ----------------------------------------------------- }

var
  FRegisteredComponents: TRegisteredComponents;

function RegisteredComponents: TRegisteredComponents;
begin
  if FRegisteredComponents = nil then
    FRegisteredComponents := TRegisteredComponents.Create(true);
  Result := FRegisteredComponents;
end;

procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const Caption: String);
var
  R: TRegisteredComponent;
begin
  R := TRegisteredComponent.Create;
  R.ComponentClass := ComponentClass;
  R.Caption := Caption;
  RegisteredComponents.Add(R);
end;

function FindComponentClass(const AClassName: string): TComponentClass;
var
  R: TRegisteredComponent;
begin
  for R in RegisteredComponents do
    if R.ComponentClass.ClassName = AClassName then
      Exit(R.ComponentClass);
  Result := nil;
end;

{ loading from JSON ---------------------------------------------------------- }

{ Read and create suitable component class from JSON. }
function CreateComponentFromJson(const JsonObject: TJSONObject;
  const Owner: TComponent): TComponent;
var
  ResultClassName: String;
  ResultClass: TComponentClass;
begin
  ResultClassName := JsonObject.Strings['_ClassName'];
  // do not confuse TJSONDeStreamer with extra ClassName property
  JsonObject.Delete('_ClassName');

  ResultClass := FindComponentClass(ResultClassName);
  if ResultClass = nil then
    raise EInvalidComponentFile.CreateFmt('Component JSON file contains unrecognized class "%s"',
      [ResultClassName]);
  Result := ResultClass.Create(Owner);
end;

procedure TCastleComponentReader.DeStreamerAfterReadObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);
var
  JsonChildren: TJSONArray;
  JsonChild: TJSONObject;
  I: Integer;
  Child: TComponent;
begin
  if AObject is TComponent then
  begin
    JsonChildren := Json.Arrays['_Children'];
    if JsonChildren <> nil then
    begin
      for I := 0 to JsonChildren.Count - 1 do
      begin
        JsonChild := JsonChildren.Objects[I];
        if JsonChild = nil then
          raise EInvalidComponentFile.Create('_Children must be an array of JSON objects');
        Child := CreateComponentFromJson(JsonChild, Owner);
        FJsonReader.JSONToObject(JsonChild, Child);
        if AObject is TCastleUserInterface then
          // matches TCastleUserInterface.GetChildren implementation
          TCastleUserInterface(AObject).InsertFront(Child as TCastleUserInterface)
        else
        if AObject is TCastleTransform then
          // matches TCastleTransform.GetChildren implementation
          TCastleTransform(AObject).Add(Child as TCastleTransform)
        else
          raise EInvalidComponentFile.CreateFmt('_Children contains unexpected class, it cannot be added to parent: %s',
            [Child.ClassName]);
      end;
    end;
    Json.Delete('_Children');
  end;
end;

{ Load any TComponent.

  It mostly works automatically with any TComponent.
  But it has some special connections to TCastleUserInterface and TCastleTransform.
  It expects that they implement InternalAddChild (an analogue to GetChildren
  method), otherwise deserializing custom children (defined by GetChildren)
  is not possible. }
function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;
var
  Reader: TCastleComponentReader;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
begin
  Reader := TCastleComponentReader.Create;
  try
    Reader.FJsonReader := TJSONDeStreamer.Create(nil);
    Reader.FJsonReader.AfterReadObject := @Reader.DeStreamerAfterReadObject;
    Reader.FOwner := Owner;

    JsonData := GetJson(FileToString(Url), true);
    try
      if not (JsonData is TJSONObject) then
        raise EInvalidComponentFile.Create('Component JSON file should contain an object');
      JsonObject := JsonData as TJSONObject;

      { create Result with appropriate class }
      Result := CreateComponentFromJson(JsonObject, Owner);

      { read Result contents from JSON }
      Reader.FJsonReader.JSONToObject(JsonObject, Result);
    finally FreeAndNil(JsonData) end;
  finally
    FreeAndNil(Reader.FJsonReader);
    FreeAndNil(Reader);
  end;
end;

{ saving to JSON ------------------------------------------------------------- }

type
  TCastleComponentWriter = class
  strict private
    { Does the property have default value.
      Written looking closely at what standard FPC writer does,
      3.0.4/fpcsrc/rtl/objpas/classes/writer.inc ,
      and simplified a lot for our purposes. }
    class function HasDefaultValue(const Instance: TPersistent; const PropInfo: PPropInfo): Boolean;
  public
    class procedure AfterStreamObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
    class procedure StreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
  end;

class procedure TCastleComponentWriter.AfterStreamObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);
begin
  { set _ClassName string, our reader depends on it }
  Json.Strings['_ClassName'] := AObject.ClassName;
end;

class procedure TCastleComponentWriter.StreamProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; var Res: TJSONData);
begin
  // always save it
  if Info^.Name = 'Name' then
    Exit;

  // do not stream null values, as reader makes errors on them
  if Res is TJSONNull then
    FreeAndNil(Res);

  // do not stream properties with stored=false or default values
  if not IsStoredProp(AObject as TPersistent, Info) then
    FreeAndNil(Res);

  // do not store properties with default values
  if HasDefaultValue(AObject as TPersistent, Info) then
    FreeAndNil(Res);
end;

class function TCastleComponentWriter.HasDefaultValue(
  const Instance: TPersistent; const PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
{$ifndef FPUNONE}
  FloatValue, DefFloatValue: Extended;
{$endif}
  MethodValue, DefMethodValue: TMethod;
  VarValue, DefVarValue : tvardata;
  BoolValue, DefBoolValue: boolean;
begin
  Result := false; // for unknown types, assume false

  PropType := PropInfo^.PropType;
  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(Instance, PropInfo);
        DefValue := PropInfo^.Default;
        Result := (Value = DefValue) and (DefValue <> longint($80000000));
      end;
{$ifndef FPUNONE}
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        DefValue := PropInfo^.Default;
        DefFloatValue := PSingle(@PropInfo^.Default)^;
        Result := (FloatValue = DefFloatValue) and (DefValue <> longint($80000000));
      end;
{$endif}
    tkMethod:
      begin
        MethodValue := GetMethodProp(Instance, PropInfo);
        DefMethodValue.Data := nil;
        DefMethodValue.Code := nil;
        Result := CompareMethods(MethodValue, DefMethodValue);
      end;
    tkSString, tkLString, tkAString:
      begin
        Result := GetStrProp(Instance, PropInfo) = '';
      end;
    tkWString:
      begin
        Result := GetWideStrProp(Instance, PropInfo) = '';
      end;
    tkUString:
      begin
        Result := GetUnicodeStrProp(Instance, PropInfo) = '';
      end;
    tkVariant:
      begin
        { Ensure that a Variant manager is installed }
        if not assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);
        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        FillChar(DefVarValue,sizeof(DefVarValue),0);
        Result := CompareByte(VarValue,DefVarValue,sizeof(VarValue)) = 0;
      end;
    tkClass:
      begin
        Result := GetObjectProp(Instance, PropInfo) = nil;
      end;
    tkInt64, tkQWord:
      begin
        Result := GetInt64Prop(Instance, PropInfo) = 0;
      end;
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        DefBoolValue := PropInfo^.Default<>0;
        DefValue := PropInfo^.Default;
        Result := (BoolValue = DefBoolValue) and (DefValue <> longint($80000000));
      end;
  end;
end;

procedure ComponentSave(const C: TComponent; const Url: String);
var
  JsonWriter: TJSONStreamer;
  Json: TJSONObject;
begin
  JsonWriter := TJSONStreamer.Create(nil);
  try
    JsonWriter.Options := [jsoStreamChildren];
    JsonWriter.AfterStreamObject := @TCastleComponentWriter(nil).AfterStreamObject;
    JsonWriter.OnStreamProperty := @TCastleComponentWriter(nil).StreamProperty;
    JsonWriter.ChildProperty := '_Children';
    Json := JsonWriter.ObjectToJSON(C);
    try
      StringToFile(Url, Json.FormatJSON);
    finally FreeAndNil(Json) end;
  finally FreeAndNil(JsonWriter) end;
end;

{ simple utilities ----------------------------------------------------------- }

procedure TransformSave(const T: TCastleTransform; const Url: String);
begin
  ComponentSave(T, Url);
end;

function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;
begin
  Result := ComponentLoad(Url, Owner) as TCastleTransform;
end;

procedure UserInterfaceSave(const C: TCastleUserInterface; const Url: String);
begin
  ComponentSave(C, Url);
end;

function UserInterfaceLoad(const Url: String; const Owner: TComponent): TCastleUserInterface;
begin
  Result := ComponentLoad(Url, Owner) as TCastleUserInterface;
end;

finalization
  FreeAndNil(FRegisteredComponents);
end.
