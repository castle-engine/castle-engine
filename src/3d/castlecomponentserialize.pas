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

uses SysUtils, Classes, FpJson, FpJsonRtti, Generics.Collections, TypInfo,
  CastleUIControls, CastleTransform;

type
  EInvalidComponentFile = class(Exception);

  { Internal for InternalAddChild methods. @exclude }
  TCastleComponentReader = class
  private
    type
      TMyJSONDeStreamer = class(TJSONDeStreamer)
      private
        Reader: TCastleComponentReader;
      end;
    { Events called by FJsonReader }
    procedure GetObject(AObject: TObject; Info: PPropInfo;
      AData: TJSONObject; DataName: TJSONStringType; var AValue: TObject);
  strict private
    type
      TResolveObjectProperty = class
        Instance: TObject;
        InstanceProperty: PPropInfo;
        PropertyValue: String;
      end;
      TResolveObjectPropertyList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TResolveObjectProperty>;
    var
      FJsonReader: TMyJSONDeStreamer;
      ResolveObjectProperties: TResolveObjectPropertyList;

    { Events called by FJsonReader }
    procedure BeforeReadObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
    procedure AfterReadObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
    procedure RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
  private
    FOwner: TComponent;
    { Call immediately after using JsonReader to deserialize JSON.
      Some object references may be unresolved, if the object is defined
      in JSON after it was referred to by name.
      In this case this method will finalize this resolution. }
    procedure FinishResolvingObjectProperties;
  public
    constructor Create;
    destructor Destroy; override;

    function JsonReader: TJSONDeStreamer;
    { Will own all deserialized components. }
    property Owner: TComponent read FOwner;
  end;

{ Save / load TCastleTransform (or descendant) to a .castle-transform file.

  An example:
  @includeCode(../../examples/short_api_samples/transform_save_load/transform_save_load.lpr) }
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

{ Save / load TComponent (or descendant) to a string.
  The string contents have the same format
  as a .castle-user-interface or .castle-transform file. }
function ComponentToString(const C: TComponent): String;
function StringToComponent(const Contents: String; const Owner: TComponent): TComponent;

type
  { Describes a component registered using @link(RegisterSerializableComponent),
    enumerated using @link(RegisteredComponents) list. }
  TRegisteredComponent = class
  public
    { Class of the component. Never leave this @nil. }
    ComponentClass: TComponentClass;
    { Nice caption to show user in the editor. }
    Caption: String;
    { Called by the editor always after creating this component. }
    OnCreate: TNotifyEvent;
    { Should correspond to whether class is declared as "deprecated" in Pascal
      (we cannot get it using RTTI for now). }
    IsDeprecated: Boolean;
  end;
  TRegisteredComponents = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TRegisteredComponent>;

{ Register a component that can be serialized and edited using CGE editor.

  In case of the overloaded version that gets TRegisteredComponent instance,
  the TRegisteredComponent instance becomes internally owned in this unit
  (do not free it yourself). }
procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const Caption: String);
procedure RegisterSerializableComponent(const C: TRegisteredComponent);

{ Read-only list of currently registered
  (using @link(RegisterSerializableComponent)) components. }
function RegisteredComponents: TRegisteredComponents;

type
  EComponentNotFound = class(Exception);

  TComponentHelper = class helper for TComponent
    { Find a child component that is owned by this component.
      This is just like standard TComponent.FindComponent
      (see https://www.freepascal.org/docs-html/rtl/classes/tcomponent.findcomponent.html )
      but it makes an exception if component could not be found.
      @raises EComponentNotFound If child component with given name could not be found.
    }
    function FindRequiredComponent(const AName: String): TComponent;
  end;

  { Load the serialized component once, instantiate it many times. }
  TSerializedComponent = class
  strict private
    FUrl, FTranslationGroupName: String;
    JsonObject: TJSONObject;
  public
    constructor Create(const AUrl: String);
    constructor CreateFromString(const Contents: String);
    destructor Destroy; override;

    { Instantiate component.
      Using this is equivalent to using global
      @link(CastleComponentSerialize.TransformLoad),
      but it is much faster if you want to instantiate the same file many times. }
    function TransformLoad(const Owner: TComponent): TCastleTransform;

    { Instantiate component.
      Using this is equivalent to using global
      @link(CastleComponentSerialize.UserInterfaceLoad),
      but it is much faster if you want to instantiate the same file many times. }
    function UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;

    { Instantiate component.
      Using this is equivalent to using global
      @link(CastleComponentSerialize.ComponentLoad),
      but it is much faster if you want to instantiate the same file many times. }
    function ComponentLoad(const Owner: TComponent): TComponent;
  end;

  { Internal, used by TranslateAllDesigns. @exclude }
  TInternalTranslateDesignCallback = procedure (const C: TComponent; const GroupName: String);

var
  { Internal, used by TranslateAllDesigns.
    @exclude }
  OnInternalTranslateDesign: TInternalTranslateDesignCallback;

var
  { Set by custom editor template (used when project defines editor_units).
    @exclude }
  InternalHasCustomComponents: Boolean;

implementation

uses JsonParser, RtlConsts,
  CastleFilesUtils, CastleUtils, CastleLog, CastleStringUtils, CastleClassUtils,
  CastleURIUtils, CastleVectors, CastleColors;

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

procedure RegisterSerializableComponent(const C: TRegisteredComponent);
begin
  RegisteredComponents.Add(C);
end;

function FindComponentClass(const AClassName: string): TComponentClass;
var
  R: TRegisteredComponent;
begin
  for R in RegisteredComponents do
    if R.ComponentClass.ClassName = AClassName then
      Exit(R.ComponentClass);
  WritelnWarning('Cannot find component class "' + AClassName + '", (de)serialization of it will fail');
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
  if Owner = nil then
    raise Exception.Create('You must provide non-nil Owner when deserializing a component. Without an Owner, it is not possible to free the component hierarchy easily, and you will most likely have memory leaks.');

  if JsonObject.Find('$ClassName') <> nil then
    ResultClassName := JsonObject.Strings['$ClassName']
  else
    ResultClassName := JsonObject.Strings['_ClassName']; // handle older format

  { Initially we did here
      JsonObject.Delete('_ClassName');
    to not confuse TJSONDeStreamer with extra _ClassName property.
    But later: it is better to leave JSON structure unmodified
    (allows to read it multiple times, if needed). }

  ResultClass := FindComponentClass(ResultClassName);
  if ResultClass = nil then
    raise EInvalidComponentFile.CreateFmt('Component JSON file references an unrecognized class "%s".' + NL + NL +
      'Add the unit that registers "%s" to the "uses" clause of any unit in the application. E.g. add "CastleTiledMap" to some uses clause, if you use "TCastleTiledMapControl" in the design.',
      [ResultClassName, ResultClassName]);
  Result := ResultClass.Create(Owner);
end;

procedure TCastleComponentReader.BeforeReadObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);
var
  C: TCastleComponent;
begin
  if AObject is TCastleComponent then
  begin
    C := TCastleComponent(AObject);
    C.InternalLoading; // add csLoading flag to ComponentState
  end;
end;

procedure TCastleComponentReader.AfterReadObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);

  { Because of our TCastleComponentReader.RestoreProperty changing
    Name to be unique, we may have desynchronized Name with InternalText.
    Synchronize it again. }
  procedure SynchronizeNameWithInternalText(const C: TCastleComponent);
  begin
    if (C.InternalOriginalName <> '') and
       (C.InternalOriginalName = C.InternalText) then
      C.InternalText := C.Name;
  end;

  { Call C.InternalAddChild for all children.
    This reverses saving children returned by C.GetChildren. }
  procedure ReadChildren(const C: TCastleComponent);
  var
    JsonChildren: TJSONArray;
    JsonChild: TJSONObject;
    I: Integer;
    Child: TComponent;
  begin
    if Json.Find('$Children') <> nil then
      JsonChildren := Json.Arrays['$Children']
    else
    if Json.Find('_Children') <> nil then
      JsonChildren := Json.Arrays['_Children'] // handle older format
    else
      JsonChildren := nil;

    if JsonChildren <> nil then
    begin
      for I := 0 to JsonChildren.Count - 1 do
      begin
        JsonChild := JsonChildren.Objects[I];
        if JsonChild = nil then
          raise EInvalidComponentFile.Create('$Children must be an array of JSON objects');
        Child := CreateComponentFromJson(JsonChild, Owner);
        FJsonReader.JSONToObject(JsonChild, Child);
        C.InternalAddChild(Child);
      end;
    end;

    { Initially we did here
        Json.Delete('_Children');
      to not confuse TJSONDeStreamer with extra property.
      But later: it is better to leave JSON structure unmodified
      (allows to read it multiple times, if needed). }
  end;

var
  C: TCastleComponent;
begin
  if AObject is TCastleComponent then
  begin
    C := TCastleComponent(AObject);
    SynchronizeNameWithInternalText(C);
    ReadChildren(C);
    C.InternalLoaded; // remove csLoading flag from ComponentState
  end;
end;

procedure TCastleComponentReader.RestoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);

  function RenameUniquely(const Owner: TComponent; const NewName: String): String;
  var
    NameWithoutNumber: String;
    Number: Int64;
    P: Integer;
  begin
    // calculate P (position of last digit)
    P := Length(NewName) + 1;
    while (P > 1) and (NewName[P - 1] in ['0'..'9']) do
      Dec(P);

    // calculate NameWithoutNumber, Number
    NameWithoutNumber := Copy(NewName, 1, P - 1);
    try
      if P <= Length(NewName) then
        Number := StrToInt(SEnding(NewName, P))
      else
        Number := 0;
    except
      on EConvertError do
      begin
        // StrToInt can fail e.g. if you supply 'Blah99999999999999999999999999'.
        NameWithoutNumber := NewName;
        Number := 0;
      end;
    end;

    repeat
      Inc(Number);
      Result := NameWithoutNumber + IntToStr(Number);
    until Owner.FindComponent(Result) = nil;
  end;

var
  TI: PTypeInfo;
  NewName: TJSONStringType;
begin
  TI := Info^.PropType;
  if (TI^.Kind in [tkSString, tkLString, tkAString]) and
     (Info^.Name = 'Name') then
  begin
    { We handle setting Name ourselves, this way we can change the Name
      to avoid conflicts. This is the only way to make Copy+Paste in CGE editor,
      it is also a useful feature in general (makes it easier to instantiate
      designs, when you don't have to worry that owner may have this name
      already reserved). }
    NewName := AValue.AsString;
    if Owner.FindComponent(NewName) <> nil then
    begin
      if AObject is TCastleComponent then
        TCastleComponent(AObject).InternalOriginalName := NewName;
      NewName := RenameUniquely(Owner, NewName);
    end;
    SetStrProp(AObject, Info, NewName);
    Handled := true;
  end;
end;

procedure TCastleComponentReader.GetObject(AObject: TObject; Info: PPropInfo;
  AData: TJSONObject; DataName: TJSONStringType; var AValue: TObject);
var
  R: TResolveObjectProperty;
begin
  { OnGetObject may also be called with other parameters,
    looking at FpJsonRtti code. Ignore them. }
  if (DataName = '') or (Info = nil) then
    Exit;

  AValue := Owner.FindComponent(DataName);

  { In this case TJSONDeStreamer.GetObject will create a new instance.
    Allow it (we have no choise), but also rememeber to finalize this property later. }
  if AValue = nil then
  begin
    R := TResolveObjectProperty.Create;
    R.Instance := AObject;
    R.InstanceProperty := Info;
    R.PropertyValue := DataName;
    ResolveObjectProperties.Add(R);
    // This is too verbose (and alarming) for normal user. Everything is OK when you see this log.
    // WritelnLog('Delaying resolving of component name "%s" (we will create a new empty instance, and resolve it at the end of loading)', [
    //   DataName
    // ]);
  end;
end;

procedure TCastleComponentReader.FinishResolvingObjectProperties;
var
  R: TResolveObjectProperty;
  PropertyValueAsObject, OldPropertyValue: TObject;
begin
  for R in ResolveObjectProperties do
  begin
    PropertyValueAsObject := Owner.FindComponent(R.PropertyValue);
    if PropertyValueAsObject = nil then
    begin
      WritelnWarning('Cannot resolve component name "%s", it will be a new empty instance', [
        R.PropertyValue
      ]);
      Continue;
    end;

    // free previous property value, in the safest way possible
    OldPropertyValue := GetObjectProp(R.Instance, R.InstanceProperty);
    SetObjectProp(R.Instance, R.InstanceProperty, nil);
    FreeAndNil(OldPropertyValue);

    // set new property value
    SetObjectProp(R.Instance, R.InstanceProperty, PropertyValueAsObject);
  end;
  ResolveObjectProperties.Clear;
end;

{ This is a global routine because TJSONDeStreamer.OnGetObject requires global.
  We just call Reader.GetObject method. }
procedure ReaderGetObject(Sender: TObject; AObject: TObject; Info: PPropInfo;
  AData: TJSONObject; DataName: TJSONStringType; var AValue: TObject);
var
  SenderDeStreamer: TCastleComponentReader.TMyJSONDeStreamer;
begin
  SenderDeStreamer := Sender as TCastleComponentReader.TMyJSONDeStreamer;
  SenderDeStreamer.Reader.GetObject(AObject, Info, AData, DataName, AValue);
end;

constructor TCastleComponentReader.Create;
begin
  inherited;

  FJsonReader := TMyJSONDeStreamer.Create(nil);
  FJsonReader.Reader := Self;
  FJsonReader.BeforeReadObject := @BeforeReadObject;
  FJsonReader.AfterReadObject := @AfterReadObject;
  FJsonReader.OnRestoreProperty := @RestoreProperty;
  FJsonReader.OnGetObject := @ReaderGetObject;

  ResolveObjectProperties := TResolveObjectPropertyList.Create(true);
end;

destructor TCastleComponentReader.Destroy;
begin
  FreeAndNil(ResolveObjectProperties);
  FreeAndNil(FJsonReader);
  inherited;
end;

function TCastleComponentReader.JsonReader: TJSONDeStreamer;
begin
  Result := FJsonReader;
end;

{ TSerializedComponent ------------------------------------------------------- }

constructor TSerializedComponent.Create(const AUrl: String);
begin
  FUrl := AUrl;
  FTranslationGroupName := DeleteURIExt(ExtractURIName(FUrl));
  CreateFromString(FileToString(AUrl));
end;

constructor TSerializedComponent.CreateFromString(const Contents: String);
var
  JsonData: TJSONData;
begin
  inherited Create;

  JsonData := GetJson(Contents, true);
  if not (JsonData is TJSONObject) then
    raise EInvalidComponentFile.Create('Component JSON file should contain an object');
  JsonObject := JsonData as TJSONObject;
end;

destructor TSerializedComponent.Destroy;
begin
  FreeAndNil(JsonObject);
  inherited;
end;

function TSerializedComponent.ComponentLoad(const Owner: TComponent): TComponent;

{ Load any TComponent.

  It mostly works automatically with any TComponent.
  But it has some special connections to TCastleUserInterface and TCastleTransform.
  It expects that they implement InternalAddChild (an analogue to GetChildren
  method), otherwise deserializing custom children (defined by GetChildren)
  is not possible. }

var
  Reader: TCastleComponentReader;
begin
  Reader := TCastleComponentReader.Create;
  try
    Reader.FOwner := Owner;

    { create Result with appropriate class }
    Result := CreateComponentFromJson(JsonObject, Owner);

    { read Result contents from JSON }
    Reader.JsonReader.JSONToObject(JsonObject, Result);

    Reader.FinishResolvingObjectProperties;

    if Assigned(OnInternalTranslateDesign) and (FTranslationGroupName <> '') then
      OnInternalTranslateDesign(Result, FTranslationGroupName);
  finally
    FreeAndNil(Reader);
  end;
end;

function TSerializedComponent.TransformLoad(const Owner: TComponent): TCastleTransform;
begin
  Result := ComponentLoad(Owner) as TCastleTransform;
end;

function TSerializedComponent.UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;
begin
  Result := ComponentLoad(Owner) as TCastleUserInterface;
end;

function StringToComponent(const Contents: String; const Owner: TComponent): TComponent;
var
  SerializedComponent: TSerializedComponent;
begin
  SerializedComponent := TSerializedComponent.CreateFromString(Contents);
  try
    Result := SerializedComponent.ComponentLoad(Owner);
  finally FreeAndNil(SerializedComponent) end;
end;

function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;

{ We could use StringToComponent now, but then TSerializedComponent
  would not know Url, so TSerializedComponent.FTranslationGroupName
  would be empty and TranslateAllDesigns would not work.

begin
  Result := StringToComponent(FileToString(Url), Owner);
end;}

var
  SerializedComponent: TSerializedComponent;
begin
  SerializedComponent := TSerializedComponent.Create(Url);
  try
    Result := SerializedComponent.ComponentLoad(Owner);
  finally FreeAndNil(SerializedComponent) end;
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
  Json.Strings['$ClassName'] := AObject.ClassName;
end;

class procedure TCastleComponentWriter.StreamProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; var Res: TJSONData);
begin
  // always save it
  if Info^.Name = 'Name' then
    Exit;

  // do not stream null values, as reader makes errors on them
  if Res is TJSONNull then
  begin
    FreeAndNil(Res);
    Exit;
  end;

  // do not stream properties with stored=false
  if not IsStoredProp(AObject as TPersistent, Info) then
  begin
    //WritelnLog('Not serializing ' + AObject.ClassName + '.' + Info^.Name + ' because stored function answers false');
    FreeAndNil(Res);
    Exit;
  end;

  // do not store properties with default values
  if HasDefaultValue(AObject as TPersistent, Info) then
  begin
    //WritelnLog('Not serializing ' + AObject.ClassName + '.' + Info^.Name + ' because it has default value');
    FreeAndNil(Res);
    Exit;
  end;
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
  VarValue, DefVarValue: tvardata;
  BoolValue, DefBoolValue, DefValueUse: Boolean;
  C: TObject;
begin
  Result := false; // for unknown types, assume false

  PropType := PropInfo^.PropType;
  DefValue := PropInfo^.Default;
  { $80000000 means that there's no default value (in case of Single or String,
    you need to specify it by "nodefault") }
  DefValueUse := DefValue <> LongInt($80000000);
  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(Instance, PropInfo);
        Result := (Value = DefValue) and DefValueUse;
      end;
{$ifndef FPUNONE}
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        DefFloatValue := PSingle(@PropInfo^.Default)^;
        Result := (FloatValue = DefFloatValue) and DefValueUse;
      end;
{$endif}
    tkMethod:
      begin
        MethodValue := GetMethodProp(Instance, PropInfo);
        DefMethodValue.Data := nil;
        DefMethodValue.Code := nil;
        Result := SameMethods(MethodValue, DefMethodValue);
      end;
    tkSString, tkLString, tkAString:
      begin
        Result := (GetStrProp(Instance, PropInfo) = '') and DefValueUse;
      end;
    tkWString:
      begin
        Result := (GetWideStrProp(Instance, PropInfo) = '') and DefValueUse;
      end;
    tkUString:
      begin
        Result := (GetUnicodeStrProp(Instance, PropInfo) = '') and DefValueUse;
      end;
    tkVariant:
      begin
        { Ensure that a Variant manager is installed }
        if not Assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);
        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        FillChar(DefVarValue,sizeof(DefVarValue),0);
        Result := CompareByte(VarValue,DefVarValue,sizeof(VarValue)) = 0;
      end;
    tkClass:
      begin
        C := GetObjectProp(Instance, PropInfo);
        Result :=
          (C = nil) or
          (
            { Avoid saving common subcomponents with all values left as default.
              This makes .castle-user-interface files smaller, which makes also their
              diffs easier to read (useful when commiting them, reviewing etc.)

              The TCastleVector*Persistent and TBorder do not descend from TComponent
              so we cannot actually check are they subcomponents:

                (C is TComponent) and
                (csSubComponent * TComponent(C).ComponentState) and
            }
            ((C.ClassType = TCastleVector2Persistent) and TCastleVector2Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleVector3Persistent) and TCastleVector3Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleVector4Persistent) and TCastleVector4Persistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleColorRGBPersistent) and TCastleColorRGBPersistent(C).HasDefaultValue) or
            ((C.ClassType = TCastleColorPersistent) and TCastleColorPersistent(C).HasDefaultValue) or
            ((C.ClassType = TBorder) and TBorder(C).HasDefaultValue)
          );
      end;
    tkInt64, tkQWord:
      begin
        Result := GetInt64Prop(Instance, PropInfo) = 0;
      end;
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        DefBoolValue := DefValue <> 0;
        Result := (BoolValue = DefBoolValue) and DefValueUse;
      end;
    else ;
  end;
end;

function ComponentToString(const C: TComponent): String;
var
  JsonWriter: TJSONStreamer;
  Json: TJSONObject;
begin
  JsonWriter := TJSONStreamer.Create(nil);
  try
    JsonWriter.Options := [
      jsoStreamChildren,
      { Otherwise TStrings (like TCastleLabel.Text) is written
        as a single String, and newlines are written as "\n" or "\r\n"
        depending on OS used to write the file.
        This causes needless differences in version control later. }
      jsoTStringsAsArray,
      { Makes TDateTime more readable }
      jsoDateTimeAsString,
      jsoCheckEmptyDateTime
    ];
    JsonWriter.AfterStreamObject := @TCastleComponentWriter(nil).AfterStreamObject;
    JsonWriter.OnStreamProperty := @TCastleComponentWriter(nil).StreamProperty;
    JsonWriter.ChildProperty := '$Children';
    Json := JsonWriter.ObjectToJSON(C);
    try
      Result := Json.FormatJSON;
    finally FreeAndNil(Json) end;
  finally FreeAndNil(JsonWriter) end;
end;

procedure ComponentSave(const C: TComponent; const Url: String);
begin
  StringToFile(Url, ComponentToString(C));
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

{ TComponentHelper ----------------------------------------------------------- }

function TComponentHelper.FindRequiredComponent(const AName: String): TComponent;
begin
  Result := FindComponent(AName);
  if Result = nil then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s"', [AName]);
end;

finalization
  FreeAndNil(FRegisteredComponents);
end.
