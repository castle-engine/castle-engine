{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading and writing a hierachy of CGE components to/from files.
  This is used by CGE editor to read/write components,
  and it can be used at runtime by games to instantiate components designed
  in the CGE editor.

  Most code should use @link(UserInterfaceLoad),
  @link(UserInterfaceSave), @link(TransformLoad), @link(TransformSave)
  which are defined in other units, that rely on this unit for base
  @link(ComponentLoad), @link(ComponentSave) implementation. }
unit CastleComponentSerialize;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FpJson, FpJsonRtti, Generics.Collections, TypInfo;

{$ifndef FPC}
Resourcestring
  SErrNoVariantSupport          = 'No variant support for properties. Please use the variants unit in your project and recompile';
{$endif}

type
  EInvalidComponentFile = class(Exception);

{ Save / load TComponent (or any descendant)
  to a .castle-component, .castle-user-interface or .castle-transform file.

  If you have a TCastleUserInterface or TCastleTransform then it is advised
  to use instead stronger typed
  @link(UserInterfaceSave), @link(UserInterfaceLoad),
  @link(TransformSave), @link(TransformLoad). }
procedure ComponentSave(const C: TComponent; const Url: String);
function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;

{ Save / load TComponent (or descendant) to a string.
  The string contents have the same format
  as a .castle-component, .castle-user-interface or .castle-transform file. }
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
  TRegisteredComponents = {$ifdef FPC}specialize{$endif} TObjectList<TRegisteredComponent>;

{ Register a component that can be serialized and edited using CGE editor.

  In case of the overloaded version that gets TRegisteredComponent instance,
  the TRegisteredComponent instance becomes internally owned in this unit
  (do not free it yourself). }
procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const Caption: String); overload;
procedure RegisterSerializableComponent(const C: TRegisteredComponent); overload;

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
    JsonObject: TJsonObject;
  public
    constructor Create(const AUrl: String);
    constructor CreateFromString(const Contents: String);
    destructor Destroy; override;

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
  { Non-empty only in custom editor builds.
    Indicates project name corresponding to this editor.
    Set by custom editor template (used when project defines editor_units).
    @exclude }
  InternalCustomComponentsForProject: String;

implementation

uses JsonParser, RtlConsts, StrUtils,
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

type
  TCastleJsonReader = class
  private
    type
      TMyJsonDeStreamer = class(TJsonDeStreamer)
      private
        Reader: TCastleJsonReader;
      end;
    { Events called by FJsonDeStreamer }
    procedure GetObject(AObject: TObject; Info: PPropInfo;
      AData: TJsonObject; DataName: TJsonStringType; var AValue: TObject);
  strict private
    type
      TResolveObjectProperty = class
        Instance: TObject;
        InstanceProperty: PPropInfo;
        PropertyValue: String;
      end;
      TResolveObjectPropertyList = {$ifdef FPC}specialize{$endif} TObjectList<TResolveObjectProperty>;

      { Handle reading custom things during TCastleComponent.CustomSerialization. }
      TSerializationProcessReader = class(TSerializationProcess)
      public
        Reader: TCastleJsonReader;
        CurrentlyReading: TJsonObject;
        procedure ReadWrite(const Key: String;
          const ListEnumerate: TSerializationProcess.TListEnumerateEvent; const ListAdd: TSerializationProcess.TListAddEvent;
          const ListClear: TSerializationProcess.TListClearEvent); override;
      end;
      TSerializationProcessReaderList = {$ifdef FPC}specialize{$endif} TObjectList<TSerializationProcessReader>;

    var
      FDeStreamer: TMyJsonDeStreamer;
      ResolveObjectProperties: TResolveObjectPropertyList;
      SerializationProcessPool: TSerializationProcessReaderList;
      SerializationProcessPoolUsed: Integer;

    { Events called by DeStreamer }
    procedure BeforeReadObject(Sender: TObject; AObject: TObject; Json: TJsonObject);
    procedure AfterReadObject(Sender: TObject; AObject: TObject; Json: TJsonObject);
    procedure RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJsonData; var Handled: Boolean);
  private
    FOwner: TComponent;
    { Call immediately after using DeStreamer to deserialize JSON.
      Some object references may be unresolved, if the object is defined
      in JSON after it was referred to by name.
      In this case this method will finalize this resolution. }
    procedure FinishResolvingObjectProperties;
  public
    constructor Create;
    destructor Destroy; override;

    function DeStreamer: TJsonDeStreamer;
    { Will own all deserialized components. }
    property Owner: TComponent read FOwner;
  end;

{ Read and create suitable component class from JSON. }
function CreateComponentFromJson(const JsonObject: TJsonObject;
  const Owner: TComponent): TComponent;
var
  ResultClassName: String;
  ResultClass: TComponentClass;
begin
  if Owner = nil then
    raise Exception.Create('You must provide non-nil Owner when deserializing a component. Without an Owner, it is not possible to free the component hierarchy easily, and you will most likely have memory leaks.');

  if JsonObject.Find('$$ClassName') <> nil then
    ResultClassName := JsonObject.Strings['$$ClassName']
  else
  if JsonObject.Find('$ClassName') <> nil then
    ResultClassName := JsonObject.Strings['$ClassName'] // handle older format
  else
    ResultClassName := JsonObject.Strings['_ClassName']; // handle older format

  { Initially we did here
      JsonObject.Delete('_ClassName');
    to not confuse TJsonDeStreamer with extra _ClassName property.
    But later: it is better to leave JSON structure unmodified
    (allows to read it multiple times, if needed). }

  ResultClass := FindComponentClass(ResultClassName);
  if ResultClass = nil then
    raise EInvalidComponentFile.CreateFmt('Component JSON file references an unrecognized class "%s".' + NL + NL +
      Iff(CastleDesignMode,
      'As you see this in the editor: the most likely cause is that this project uses custom components, and you did not make a custom editor build. Use the menu item "Project -> Restart Editor (With Custom Components)" to build and run correct editor.',
      'Add the unit that registers "%s" to the "uses" clause of any unit in the application. E.g. add "CastleTiledMap" to some uses clause, if you use "TCastleTiledMapControl" in the design.'),
      [ResultClassName, ResultClassName]);
  Result := ResultClass.Create(Owner);
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWrite(const Key: String;
  const ListEnumerate: TSerializationProcess.TListEnumerateEvent; const ListAdd: TSerializationProcess.TListAddEvent;
  const ListClear: TSerializationProcess.TListClearEvent);
var
  JsonChildren: TJsonArray;
  JsonChild: TJsonObject;
  I: Integer;
  Child: TComponent;
begin
  ListClear;
  JsonChildren := CurrentlyReading.Find('$' + Key, jtArray) as TJsonArray;

  { backward compatibily for reading old children in JSON }
  if (Key = 'Children') and (JsonChildren = nil) then
    JsonChildren := CurrentlyReading.Find('_Children', jtArray) as TJsonArray;

  if JsonChildren <> nil then
  begin
    for I := 0 to JsonChildren.Count - 1 do
    begin
      JsonChild := JsonChildren.Objects[I];
      if JsonChild = nil then
        raise EInvalidComponentFile.Create('$' + Key + ' must be an array of JSON objects');
      Child := CreateComponentFromJson(JsonChild, Reader.Owner);
      Reader.DeStreamer.JsonToObject(JsonChild, Child);
      ListAdd(Child);
    end;
  end;
end;

procedure TCastleJsonReader.BeforeReadObject(
  Sender: TObject; AObject: TObject; Json: TJsonObject);
var
  C: TCastleComponent;
begin
  if AObject is TCastleComponent then
  begin
    C := TCastleComponent(AObject);
    C.InternalLoading; // add csLoading flag to ComponentState
  end;
end;

procedure TCastleJsonReader.AfterReadObject(
  Sender: TObject; AObject: TObject; Json: TJsonObject);

  { Because of our TCastleJsonReader.RestoreProperty changing
    Name to be unique, we may have desynchronized Name with InternalText.
    Synchronize it again. }
  procedure SynchronizeNameWithInternalText(const C: TCastleComponent);
  begin
    if (C.InternalOriginalName <> '') and
       (C.InternalOriginalName = C.InternalText) then
      C.InternalText := C.Name;
  end;

  { Call C.CustomSerialization(SerializationProcess) }
  procedure CustomSerializationWithSerializationProcess(const C: TCastleComponent;
    const SerializationProcess: TSerializationProcessReader);
  begin
    SerializationProcess.CurrentlyReading := Json;
    SerializationProcess.Reader := Self;
    C.CustomSerialization(SerializationProcess);
  end;

  { Call C.CustomSerialization }
  procedure CustomSerialization(const C: TCastleComponent);
  var
    SerializationProcess: TSerializationProcessReader;
  begin
    if SerializationProcessPoolUsed < SerializationProcessPool.Count then
      // faster: use ready SerializationProcess from pool
      SerializationProcess := SerializationProcessPool[SerializationProcessPoolUsed]
    else
    begin
      // slower: create new SerializationProcess,
      // and add it to pool for future use from the same reader
      SerializationProcess := TSerializationProcessReader.Create;
      SerializationProcessPool.Add(SerializationProcess);
    end;
    Assert(SerializationProcessPoolUsed < SerializationProcessPool.Count);

    Inc(SerializationProcessPoolUsed);
    try
      CustomSerializationWithSerializationProcess(C, SerializationProcess);
    finally Dec(SerializationProcessPoolUsed) end;
  end;

var
  C: TCastleComponent;
begin
  if AObject is TCastleComponent then
  begin
    C := TCastleComponent(AObject);
    SynchronizeNameWithInternalText(C);
    CustomSerialization(C);
    C.InternalLoaded; // remove csLoading flag from ComponentState
  end;
end;

procedure TCastleJsonReader.RestoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJsonData; var Handled: Boolean);

  function RenameUniquely(const Owner: TComponent; const NewName: String): String;
  var
    NameWithoutNumber: String;
    Number: Int64;
    P: Integer;
  begin
    // calculate P (position of last digit)
    P := Length(NewName) + 1;
    while (P > 1) and CharInSet(NewName[P - 1], ['0'..'9']) do
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
  NewName: TJsonStringType;
begin
  TI := Info^.PropType{$ifndef FPC}^{$endif};
  if (TI^.Kind in [{$ifdef FPC} tkSString, tkLString, tkAString {$else} tkUString {$endif}]) and
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

procedure TCastleJsonReader.GetObject(AObject: TObject; Info: PPropInfo;
  AData: TJsonObject; DataName: TJsonStringType; var AValue: TObject);
var
  R: TResolveObjectProperty;
begin
  { OnGetObject may also be called with other parameters,
    looking at FpJsonRtti code. Ignore them. }
  if (DataName = '') or (Info = nil) then
    Exit;

  AValue := Owner.FindComponent(DataName);

  { In this case TJsonDeStreamer.GetObject will create a new instance.
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

procedure TCastleJsonReader.FinishResolvingObjectProperties;
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

{ This is a global routine because TJsonDeStreamer.OnGetObject requires global.
  We just call Reader.GetObject method. }
procedure ReaderGetObject(Sender: TObject; AObject: TObject; Info: PPropInfo;
  AData: TJsonObject; DataName: TJsonStringType; var AValue: TObject);
var
  SenderDeStreamer: TCastleJsonReader.TMyJsonDeStreamer;
begin
  SenderDeStreamer := Sender as TCastleJsonReader.TMyJsonDeStreamer;
  SenderDeStreamer.Reader.GetObject(AObject, Info, AData, DataName, AValue);
end;

constructor TCastleJsonReader.Create;
begin
  inherited;

  FDeStreamer := TMyJsonDeStreamer.Create(nil);
  FDeStreamer.Reader := Self;
  FDeStreamer.BeforeReadObject := {$ifdef FPC}@{$endif}BeforeReadObject;
  FDeStreamer.AfterReadObject := {$ifdef FPC}@{$endif}AfterReadObject;
  FDeStreamer.OnRestoreProperty := {$ifdef FPC}@{$endif}RestoreProperty;
  FDeStreamer.OnGetObject := {$ifdef FPC}@{$endif}ReaderGetObject;

  ResolveObjectProperties := TResolveObjectPropertyList.Create(true);

  SerializationProcessPool := TSerializationProcessReaderList.Create(true);
end;

destructor TCastleJsonReader.Destroy;
begin
  FreeAndNil(ResolveObjectProperties);
  FreeAndNil(FDeStreamer);
  FreeAndNil(SerializationProcessPool);
  inherited;
end;

function TCastleJsonReader.DeStreamer: TJsonDeStreamer;
begin
  Result := FDeStreamer;
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
  JsonData: TJsonData;
begin
  inherited Create;

  JsonData := GetJson(Contents, true);
  if not (JsonData is TJsonObject) then
    raise EInvalidComponentFile.Create('Component JSON file should contain an object');
  JsonObject := JsonData as TJsonObject;
end;

destructor TSerializedComponent.Destroy;
begin
  FreeAndNil(JsonObject);
  inherited;
end;

function TSerializedComponent.ComponentLoad(const Owner: TComponent): TComponent;
var
  Reader: TCastleJsonReader;
begin
  Reader := TCastleJsonReader.Create;
  try
    Reader.FOwner := Owner;

    { create Result with appropriate class }
    Result := CreateComponentFromJson(JsonObject, Owner);

    { read Result contents from JSON }
    Reader.DeStreamer.JsonToObject(JsonObject, Result);

    Reader.FinishResolvingObjectProperties;

    if Assigned(OnInternalTranslateDesign) and (FTranslationGroupName <> '') then
      OnInternalTranslateDesign(Result, FTranslationGroupName);
  finally
    FreeAndNil(Reader);
  end;
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
  TCastleJsonWriter = class
  strict private
    type
      { Handle writing custom things during TCastleComponent.CustomSerialization. }
      TSerializationProcessWriter = class(TSerializationProcess)
      strict private
        CurrentlyWritingArray: TJsonArray;
        Key: String;
        procedure WriteItem(C: TComponent);
      public
        Writer: TCastleJsonWriter;
        CurrentlyWriting: TJsonObject;
        procedure ReadWrite(const AKey: String;
          const ListEnumerate: TSerializationProcess.TListEnumerateEvent;
          const ListAdd: TSerializationProcess.TListAddEvent;
          const ListClear: TSerializationProcess.TListClearEvent); override;
      end;
      TSerializationProcessWriterList = {$ifdef FPC}specialize{$endif} TObjectList<TSerializationProcessWriter>;

    var
      FStreamer: TJsonStreamer;
      { Using just one TSerializationProcessWriter instance is not enough,
        as C.CustomSerialization calls may happen recursively. }
      SerializationProcessPool: TSerializationProcessWriterList;
      SerializationProcessPoolUsed: Integer;

    { Does the property have default value. }
    class function HasDefaultValue(const Instance: TPersistent; const PropInfo: PPropInfo): Boolean; static;

    procedure BeforeStreamObject(Sender: TObject; AObject: TObject; Json: TJsonObject);
    procedure AfterStreamObject(Sender: TObject; AObject: TObject; Json: TJsonObject);
    procedure StreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJsonData);
  public
    constructor Create;
    destructor Destroy; override;
    property Streamer: TJsonStreamer read FStreamer;
  end;

procedure TCastleJsonWriter.TSerializationProcessWriter.WriteItem(C: TComponent);
begin
  // create JSON array only when the list is non-empty, this way JSON is simpler.
  if CurrentlyWritingArray = nil then
  begin
    CurrentlyWritingArray := TJsonArray.Create;
    CurrentlyWriting.Add('$' + Key, CurrentlyWritingArray);
  end;

  CurrentlyWritingArray.Add(Writer.Streamer.ObjectToJson(C));
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWrite(const AKey: String;
  const ListEnumerate: TSerializationProcess.TListEnumerateEvent;
  const ListAdd: TSerializationProcess.TListAddEvent;
  const ListClear: TSerializationProcess.TListClearEvent);
begin
  CurrentlyWritingArray := nil; // will be created on-demand
  Key := AKey;
  ListEnumerate({$ifdef FPC}@{$endif}WriteItem);
end;

constructor TCastleJsonWriter.Create;
begin
  inherited Create;

  FStreamer := TJsonStreamer.Create(nil);
  Streamer.Options := [
    // We no longer use it. Our CustomSerialization fills this use-case in more flexible manner.
    // jsoStreamChildren,
    { Otherwise TStrings (like TCastleLabel.Text) is written
      as a single String, and newlines are written as "\n" or "\r\n"
      depending on OS used to write the file.
      This causes needless differences in version control later. }
    jsoTStringsAsArray,
    { Makes TDateTime more readable }
    jsoDateTimeAsString,
    jsoCheckEmptyDateTime
  ];
  Streamer.BeforeStreamObject := {$ifdef FPC}@{$endif}BeforeStreamObject;
  Streamer.AfterStreamObject := {$ifdef FPC}@{$endif}AfterStreamObject;
  Streamer.OnStreamProperty := {$ifdef FPC}@{$endif}StreamProperty;

  SerializationProcessPool := TSerializationProcessWriterList.Create(true);
end;

destructor TCastleJsonWriter.Destroy;
begin
  FreeAndNil(FStreamer);
  FreeAndNil(SerializationProcessPool);
  inherited;
end;

procedure TCastleJsonWriter.BeforeStreamObject(
  Sender: TObject; AObject: TObject; Json: TJsonObject);
begin
  { set $$ClassName string, our reader depends on it.
    Uses 2 $, to differentiate from stuff written by TSerializationProcess.ReadWrite.
    We do this in BeforeStreamObject (not AfterStreamObject) only because this way
    resulting JSON is easier to read by humans ($$ClassName is at the beginning). }
  Json.Strings['$$ClassName'] := AObject.ClassName;
end;

procedure TCastleJsonWriter.AfterStreamObject(
  Sender: TObject; AObject: TObject; Json: TJsonObject);

  { Call C.CustomSerialization(SerializationProcess) }
  procedure CustomSerializationWithSerializationProcess(const C: TCastleComponent;
    const SerializationProcess: TSerializationProcessWriter);
  begin
    SerializationProcess.CurrentlyWriting := Json;
    SerializationProcess.Writer := Self;
    C.CustomSerialization(SerializationProcess);
  end;

  { Call C.CustomSerialization }
  procedure CustomSerialization(const C: TCastleComponent);
  var
     SerializationProcess: TSerializationProcessWriter;
  begin
    if SerializationProcessPoolUsed < SerializationProcessPool.Count then
      // faster: use ready SerializationProcess from pool
      SerializationProcess := SerializationProcessPool[SerializationProcessPoolUsed]
    else
    begin
      // slower: create new SerializationProcess,
      // and add it to pool for future use from the same reader
      SerializationProcess := TSerializationProcessWriter.Create;
      SerializationProcessPool.Add(SerializationProcess);
    end;
    Assert(SerializationProcessPoolUsed < SerializationProcessPool.Count);

    Inc(SerializationProcessPoolUsed);
    try
      CustomSerializationWithSerializationProcess(C, SerializationProcess);
    finally Dec(SerializationProcessPoolUsed) end;
  end;

var
  C: TCastleComponent;
begin
  if AObject is TCastleComponent then
  begin
    C := TCastleComponent(AObject);
    CustomSerialization(C);
  end;
end;

procedure TCastleJsonWriter.StreamProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; var Res: TJsonData);
begin
  // always save it
  if Info^.Name = 'Name' then
    Exit;

  // do not stream null values, as reader makes errors on them
  if Res is TJsonNull then
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

class function TCastleJsonWriter.HasDefaultValue(
  const Instance: TPersistent; const PropInfo: PPropInfo): Boolean; {$ifdef FPC}static;{$endif}

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
{$ifdef FPC}
    tkSString, tkLString, tkAString:
      begin
        Result := (GetStrProp(Instance, PropInfo) = '') and DefValueUse;
      end;
{$else}
    tkString, tkLString:
      begin
        Result := (GetAnsiStrProp(Instance, PropInfo) = '') and DefValueUse;
      end;
{$endif}
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
        {$ifdef FPC}
        Result := CompareByte(VarValue,DefVarValue,sizeof(VarValue)) = 0;
        {$else}
        Result := CompareMem(@VarValue, @DefVarValue, sizeof(VarValue));
        {$endif}
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
    tkInt64{$ifdef FPC}, tkQWord{$endif}:
      begin
        Result := GetInt64Prop(Instance, PropInfo) = 0;
      end;
{$ifdef FPC}
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        DefBoolValue := DefValue <> 0;
        Result := (BoolValue = DefBoolValue) and DefValueUse;
      end;
{$endif}
    else ;
  end;
end;

function ComponentToString(const C: TComponent): String;
var
  Json: TJsonObject;
  Writer: TCastleJsonWriter;
begin
  Writer := TCastleJsonWriter.Create;
  try
    Json := Writer.Streamer.ObjectToJson(C);
    try
      Result := Json.FormatJson;
    finally FreeAndNil(Json) end;
  finally FreeAndNil(Writer) end;
end;

procedure ComponentSave(const C: TComponent; const Url: String);
begin
  StringToFile(Url, ComponentToString(C));
end;

{ TComponentHelper ----------------------------------------------------------- }

function TComponentHelper.FindRequiredComponent(const AName: String): TComponent;
begin
  Result := FindComponent(AName);
  if Result = nil then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s"', [AName]);
end;

initialization
  // not useful: RegisterSerializableComponent(TComponent, 'Component (Basic)');
  RegisterSerializableComponent(TCastleComponent, 'Component (Group)');
finalization
  FreeAndNil(FRegisteredComponents);
end.
