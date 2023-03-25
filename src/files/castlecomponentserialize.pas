{
  Copyright 2018-2023 Michalis Kamburelis.

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
  { Additional loading configuration for InternalStringToComponent.
    @exclude }
  TInternalComponentLoadInfo = class
    { Allows viewports to preserve design-time camera/navigation across CGE editor undo. }
    PreserveDataAcrossUndo: TComponent;
    { If ChangeClassName is non-empty, at loading we change class of given component to this. }
    ChangeClassName: String;
    ChangeClassClass: TComponentClass;
  end;

{ Like StringToComponent but takes additional TInternalComponentLoadInfo
  that adds some possibilities. LoadInfo may be @nil (making this equivalent
  to just StringToComponent).
  @exclude }
function InternalStringToComponent(const Contents: String;
  const Owner: TComponent;
  const LoadInfo: TInternalComponentLoadInfo): TComponent;

type
  { Describes a component registered using @link(RegisterSerializableComponent),
    enumerated using @link(RegisteredComponents) list. }
  TRegisteredComponent = class
  public
    { Class of the component. Never leave this @nil. }
    ComponentClass: TComponentClass;
    { Nice caption to show user in the editor. }
    Caption: array of String;
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
  const Caption: array of String); overload;
procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const CaptionOnePart: String); overload;
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
  private
    function InternalComponentLoad(const Owner: TComponent;
      const LoadInfo: TInternalComponentLoadInfo): TComponent;
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

  { Are we inside ComponentLoad.
    Prefer to instead look at component property TCastleComponent.IsLoading,
    this variable is really only a hack for exceptional situations.
    @exclude }
  InternalLoadingComponent: Cardinal;

{ Copy the properties of Source to Destination using the serialization to JSON.
  This has a nice advantage that you don't need to implement field-by-field
  assignment manually (e.g. overriding TPersistent.Assign).

  It has however problems:

  - It works correctly only when Destination is in completely default state
    (right after constructor).
    Or, at least, the properties of Source that are in default state
    are also already in default state in Destination.
    That's because these properties are not serialized, and thus will not be modified
    in Destination.

  - It's inefficient, doing serializing + deserializing.
    While this doesn't actually serialize JSON to string,
    it only uses in-memory JSON  classes,
    still there's a lot of unnecessary creation of temporary structures and copying
    compared to traditional approach of overriding TPersistent.Assign.

  For these 2 reasons, this remains an internal "hack",
  and not something we advise you to use.
  @exclude
}
procedure InternalAssignUsingSerialization(const Destination, Source: TComponent);

implementation

uses JsonParser, RtlConsts, StrUtils,
  CastleFilesUtils, CastleUtils, CastleLog, CastleStringUtils, CastleClassUtils,
  CastleURIUtils, CastleVectors, CastleColors, CastleInternalRttiUtils;

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
  const Caption: array of String);
var
  R: TRegisteredComponent;
  I: Integer;
begin
  R := TRegisteredComponent.Create;
  R.ComponentClass := ComponentClass;
  SetLength(R.Caption, High(Caption) + 1);
  for I := 0 to High(Caption) do
    R.Caption[I] := Caption[I];
  RegisteredComponents.Add(R);
end;

procedure RegisterSerializableComponent(const ComponentClass: TComponentClass;
  const CaptionOnePart: String);
var
  R: TRegisteredComponent;
begin
  R := TRegisteredComponent.Create;
  R.ComponentClass := ComponentClass;
  R.Caption := [CaptionOnePart];
  RegisteredComponents.Add(R);
end;

procedure RegisterSerializableComponent(const C: TRegisteredComponent);

  function InsertSpacesBeforeUpperLetters(const S: String): String;
  var
    StrBuild: TStringBuilder;
    I: Integer;
  begin
    StrBuild := TStringBuilder.Create;
    try
      for I := 1 to Length(S) do
      begin
        if (I > 1) and (S[I] in ['A'..'Z']) then
          StrBuild.Append(' ');
        StrBuild.Append(S[I]);
      end;
      Result := StrBuild.ToString;
    finally FreeAndNil(StrBuild) end;
  end;

var
  GuessedCaption: String;
begin
  if C.ComponentClass = nil then
    raise Exception.Create('RegisterSerializableComponent: ComponentClass not assigned');

  if (Length(C.Caption) = 0) or (C.Caption[0] = '') then
  begin
    GuessedCaption := InsertSpacesBeforeUpperLetters(
      PrefixRemove('Castle', PrefixRemove('T', C.ComponentClass.ClassName, true), true));
    C.Caption := [GuessedCaption];
    WritelnWarning('RegisterSerializableComponent: component Caption at registration cannot be empty, setting a placeholder "%s"', [
      GuessedCaption
    ]);
  end;

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
        procedure ReadWriteInteger(const Key: String; var Value: Integer; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteBoolean(const Key: String; var Value: Boolean; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteString(const Key: String; var Value: String; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteSingle(const Key: String; var Value: Single; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteSubComponent(const Key: String; const Value: TComponent;
          const IsStored: Boolean); override;
        procedure ReadWriteList(const Key: String;
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
    LoadInfo: TInternalComponentLoadInfo;
    FOwner: TComponent;
    (*Resolve hanging references, when JSON referred to some component name
      before this component was actually defined.
      Like when Viewport references a camera in Viewport.Camera,
      but the camera is only defined later while reading Viewport.Items,
      like

        "Camera": "Camera1",
        "Items" {
          ...
          {
            Name: "Camera1",
          }
        }
    *)
    procedure FinishResolvingComponentProperties;

    { Like FinishResolvingComponentProperties, but only resolves references to C,
      so it is much faster, and doesn't warn about other references remaining
      unsolved. }
    procedure ResolveComponentReferences(const C: TComponent);
  public
    constructor Create;
    destructor Destroy; override;

    function DeStreamer: TJsonDeStreamer;
    { Will own all deserialized components. }
    property Owner: TComponent read FOwner;
  end;

{ Read and create suitable component class from JSON. }
function CreateComponentFromJson(const JsonObject: TJsonObject;
  const Owner: TComponent;
  const LoadInfo: TInternalComponentLoadInfo): TComponent;
var
  ResultClassName: String;
  ResultClass: TComponentClass;
begin
  if Owner = nil then
    raise Exception.Create('You must provide non-nil Owner when deserializing a component. Without an Owner, it is not possible to free the component hierarchy easily, and you will most likely have memory leaks.');

  if (LoadInfo <> nil) and
     (LoadInfo.ChangeClassName <> '') and // early exit in the usual case
     (LoadInfo.ChangeClassName = JsonObject.Strings['Name']) then
  begin
    ResultClass := LoadInfo.ChangeClassClass;
  end else
  begin
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
  end;

  if ResultClass = nil then
    raise EInvalidComponentFile.CreateFmt('Component JSON file references an unrecognized class "%s".' + NL + NL +
      Iff(CastleDesignMode,
      'As you see this in the editor: the most likely cause is that this project uses custom components, and you did not make a custom editor build. Use the menu item "Project -> Restart Editor (With Custom Components)" to build and run correct editor.',
      'Add the unit that registers "%s" to the "uses" clause of any unit in the application. E.g. add "CastleTiledMap" to some uses clause, if you use "TCastleTiledMap" in the design.'),
      [ResultClassName, ResultClassName]);
  Result := ResultClass.Create(Owner);
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteInteger(
  const Key: String; var Value: Integer; const IsStored: Boolean);
var
  JsonData: TJsonData;
begin
  JsonData := CurrentlyReading.Find(Key) as TJsonData;
  if JsonData <> nil then
    Value := JsonData.AsInteger;
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteBoolean(
  const Key: String; var Value: Boolean; const IsStored: Boolean);
var
  JsonData: TJsonData;
begin
  JsonData := CurrentlyReading.Find(Key) as TJsonData;
  if JsonData <> nil then
    Value := JsonData.AsBoolean;
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteString(
  const Key: String; var Value: String; const IsStored: Boolean);
var
  JsonData: TJsonData;
begin
  JsonData := CurrentlyReading.Find(Key) as TJsonData;
  if JsonData <> nil then
    Value := JsonData.AsString;
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteSingle(
  const Key: String; var Value: Single; const IsStored: Boolean);
var
  JsonData: TJsonData;
begin
  JsonData := CurrentlyReading.Find(Key) as TJsonData;
  if JsonData <> nil then
    Value := JsonData.AsFloat;
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteSubComponent(
  const Key: String; const Value: TComponent; const IsStored: Boolean);
var
  JsonData: TJsonData;
begin
  Assert(Value <> nil);
  JsonData := CurrentlyReading.Find(Key) as TJsonData;
  if (JsonData <> nil) and (JsonData.JsonType = jtObject) then
    Reader.FDeStreamer.JsonToObject(JsonData as TJsonObject, Value);
end;

procedure TCastleJsonReader.TSerializationProcessReader.ReadWriteList(const Key: String;
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
      Child := CreateComponentFromJson(JsonChild, Reader.Owner, Reader.LoadInfo);
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
    C.InternalLoading; // set C.IsLoading := true, (FPC only) add csLoading flag to ComponentState
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
    if LoadInfo <> nil then
      SerializationProcess.InternalPreserveDataAcrossUndo := LoadInfo.PreserveDataAcrossUndo
    else
      SerializationProcess.InternalPreserveDataAcrossUndo := nil;
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
    C.InternalLoaded; // set IsLoading = false, (FPC only) remove csLoading flag from ComponentState, calls Loaded virtual method
  end;

  if AObject is TComponent then
    { Resolve references to this object, as soon as this object is read
      (without waiting for final FinishResolvingComponentProperties).

      This is necessary for TCastleViewport.Loaded to function correctly,
      it assumes that Camera inside has the same owner as Viewport,
      which means that it is not a "dummy" instance created by FpRttiJson
      when property was nil. }
    ResolveComponentReferences(TComponent(AObject));
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
      to avoid conflicts. This way we can safely add new components into
      existing hierarchy (with some names already reserved in Owner).
      This way:

      - We can make Copy+Paste in CGE editor, by just pasting the components
        into existing owner.

      - In general, code can safely instantiate designs,
        without worrying that owner may have some names already reserved
        (which would otherwise cause exception when trying to add new children
        with the same name).

      - Even invalid old designs can be read: in old designs, you could have
        multiple camera components named 'Camera'. This was not a problem,
        as camera was a subcomponent owned by each viewport.
        In new designs, these cameras are all owned by a single DesignOwner,
        Thanks to this mechanism, these cameras will be renamed as necessary,
        to not conflict. (While TCastleViewport.Loaded also does renaming,
        but only in CastleDesignMode; if you load old design at runtime,
        then the mechanism below kicks-in.)
    }
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
  ValueClass: TClass;
begin
  if (DataName = '') and (Info <> nil) then
  begin
    (*This happens when JSON wants to set object property that is currently nil,
      but it has some content in JSON.
      In theory it should not happen -- such properties should be a non-nil
      subcomponent. But in practice it is possible for backward-compatibility.
      This is possible in old designs:

        "Camera": { Name: "Camera1", ... }

      The default implementation in TJSONDeStreamer.GetObject
      does *almost* what we want... except the owner is then wrong
      (it creates new object, with owner from containing object).
    *)

    ValueClass := GetTypeData(Info^.PropType {$ifndef FPC}^{$endif})^.ClassType;
    if ValueClass.InheritsFrom(TComponent) then
      AValue := TComponentClass(ValueClass).Create(Owner);
  end;

  if (DataName <> '') and (Info <> nil) then
  begin
    { This happens when JSON wants to set object property using a name
      of some existing instance. Like
        "Camera": "Camera1"
    }
    AValue := Owner.FindComponent(DataName);

    if AValue = nil then
    begin
      { In this case TJsonDeStreamer.GetObject will create a new instance,
        that is useless (we will want to throw it away later),
        but we cannot avoid this creation in TJsonDeStreamer.GetObject.

        But at least rememeber to finalize this property later. }

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
end;

procedure TCastleJsonReader.ResolveComponentReferences(const C: TComponent);
var
  R: TResolveObjectProperty;
  OldPropertyValue: TObject;
  I: Integer;
begin
  if C.Name = '' then Exit; // nothing to resolve for unnamed component

  for I := ResolveObjectProperties.Count - 1 downto 0 do
  begin
    R := ResolveObjectProperties[I];
    if R.PropertyValue = C.Name then
    begin
      // free previous property value, in the safest way possible
      OldPropertyValue := GetObjectProp(R.Instance, R.InstanceProperty);
      SetObjectProp(R.Instance, R.InstanceProperty, nil);
      FreeAndNil(OldPropertyValue);

      // set new property value
      SetObjectProp(R.Instance, R.InstanceProperty, C);

      ResolveObjectProperties.Delete(I);
    end;
  end;
end;

procedure TCastleJsonReader.FinishResolvingComponentProperties;
var
  R: TResolveObjectProperty;
  PropertyValueAsObject, OldPropertyValue: TObject;
begin
  for R in ResolveObjectProperties do
  begin
    PropertyValueAsObject := Owner.FindComponent(R.PropertyValue);
    if PropertyValueAsObject = nil then
    begin
      { In case we cannot resolve the component name, it is better to set
        the property to nil than to leave the empty (unnamed) placeholder
        instance created by FpJson.
        That is because having an unnamed component would cause further troubles:
        - If you save it again, we will save a component with name='' and a reference
          to it using empty name.
        - Opening it, we could not resolve the empty name (because empty name never
          matches in FindComponent) and we would create a new empty component...

        This is actually possible:
        - Save with "Save Selected" a subset of hierarchy that doesn't include
          some referenced component.
          For example, make TCastleViewport.Camera reference a TCastleCamera instance
          defined *outside of this TCastleViewport* (on another viewport) called
          'CameraOutside'.
        - Saving it with "Save Selected" for now just makes a design with broken link.
          The Camera="CameraOutside" reference in viewport cannot be resolved.
          If we convert it to nil on load, that's just a warning and we can continue OK.
        - If we leave the TCastleCamera placeholder instance, next save of this will save
          to JSON TCastleCamera with Name='', and Camera="" reference in viewport.
        - On load, we will fail to resolve Camera="".
          Saving and loading will create more and more unnamed TCastleCamera instances...
      }
      WritelnWarning('Cannot resolve component name "%s", it will be set to nil', [
        R.PropertyValue
      ]);
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

  { Do this early, so that destructor can just rely that it was for sure done,
    even if there's exception from constructor. }
  Inc(InternalLoadingComponent);

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
  Dec(InternalLoadingComponent);
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
begin
  Result := InternalComponentLoad(Owner, nil);
end;

function TSerializedComponent.InternalComponentLoad(const Owner: TComponent;
  const LoadInfo: TInternalComponentLoadInfo): TComponent;
var
  Reader: TCastleJsonReader;
begin
  Reader := TCastleJsonReader.Create;
  try
    Reader.FOwner := Owner;
    Reader.LoadInfo := LoadInfo;

    { create Result with appropriate class }
    Result := CreateComponentFromJson(JsonObject, Owner, LoadInfo);

    { read Result contents from JSON }
    Reader.DeStreamer.JsonToObject(JsonObject, Result);

    Reader.FinishResolvingComponentProperties;

    if Assigned(OnInternalTranslateDesign) and (FTranslationGroupName <> '') then
      OnInternalTranslateDesign(Result, FTranslationGroupName);
  finally
    FreeAndNil(Reader);
  end;
end;

function StringToComponent(const Contents: String; const Owner: TComponent): TComponent;
begin
  Result := InternalStringToComponent(Contents, Owner, nil);
end;

function InternalStringToComponent(const Contents: String;
  const Owner: TComponent;
  const LoadInfo: TInternalComponentLoadInfo): TComponent;
var
  SerializedComponent: TSerializedComponent;
begin
  SerializedComponent := TSerializedComponent.CreateFromString(Contents);
  try
    Result := SerializedComponent.InternalComponentLoad(Owner, LoadInfo);
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
        procedure ReadWriteInteger(const AKey: String; var Value: Integer; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteBoolean(const AKey: String; var Value: Boolean; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteString(const AKey: String; var Value: String; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteSingle(const AKey: String; var Value: Single; const IsStored: Boolean);
          overload; override;
        procedure ReadWriteSubComponent(const AKey: String; const Value: TComponent;
          const IsStored: Boolean); override;
        procedure ReadWriteList(const AKey: String;
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

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteInteger(
  const AKey: String; var Value: Integer; const IsStored: Boolean);
begin
  if IsStored then
  begin
    CurrentlyWriting.Add(AKey, TJsonIntegerNumber.Create(Value));
  end;
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteBoolean(
  const AKey: String; var Value: Boolean; const IsStored: Boolean);
begin
  if IsStored then
  begin
    CurrentlyWriting.Add(AKey, TJsonBoolean.Create(Value));
  end;
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteString(
  const AKey: String; var Value: String; const IsStored: Boolean);
begin
  if IsStored then
  begin
    CurrentlyWriting.Add(AKey, TJsonString.Create(Value));
  end;
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteSingle(
  const AKey: String; var Value: Single; const IsStored: Boolean);
begin
  if IsStored then
  begin
    CurrentlyWriting.Add(AKey, TJsonFloatNumber.Create(Value));
  end;
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteSubComponent(
  const AKey: String; const Value: TComponent; const IsStored: Boolean);
begin
  if IsStored then
  begin
    CurrentlyWriting.Add(AKey, Writer.Streamer.ObjectToJson(Value));
  end;
end;

procedure TCastleJsonWriter.TSerializationProcessWriter.ReadWriteList(const AKey: String;
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

  { Serialize to JSON a set of values from 0 to Highest. }
  function SerializeSet(
    { Note that GetOrdProp result type for each compiler is different:
      - Int64 https://www.freepascal.org/docs-html/rtl/typinfo/getordprop.html
      - NativeInt https://docwiki.embarcadero.com/Libraries/Sydney/en/System.TypInfo.GetOrdProp
      It seems we can reliably handle at most 32 bits.
      Actually TCastleTiledMap.TLayerIndex limits itself to 31 bits for now,
      to avoid worrying about whether negative values are passed through the API OK.
    }
    const ValueOfSet: UInt32;
    const Highest: Integer): TJsonArray;
  type
    TIntegerSet = set of 0..31;
  var
    I: Integer;
  begin
    Result := TJSONArray.Create;
    for I := 0 to Highest do
      if I in TIntegerSet(ValueOfSet) then
        TJSONArray(Result).Add(I);
  end;

begin
  if Info^.Name = 'Name' then
  begin
    if (AObject is TComponent) and
       (csSubComponent in TComponent(AObject).ComponentStyle) then
    begin
      { Do not stream names of subcomponents, like
        - TCastlePerspective (their names are internal, not really supposed to be edited by user -- no point)
        - TCastleVector3Persistent (their names are internal, not really supposed to be edited by user -- no point, and always empty)

        This is consistent with TDesignFrame.InspectorFilter that hides such names.
        Although here we cannot detect them by "Owner is different than DesignOwner",
        because when serializing we don't know the DesignOwner. }
      //WritelnLog('Not serializing ' + AObject.ClassName + '.' + Info^.Name + ' because it is a subcomponent name');
      FreeAndNil(Res);
    end;

    { Otherwise (for not subcomponents) serialize 'Name' always,
      ignore the rest of the checks (they would reject serializing Name). }
    Exit;
  end;

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
  if PropertyHasDefaultValue(AObject, Info) then
  begin
    //WritelnLog('Not serializing ' + AObject.ClassName + '.' + Info^.Name + ' because it has default value');
    FreeAndNil(Res);
    Exit;
  end;

  { Custom support for sets of integers (T3DCoords, TLayers) serialization.

    By default FpJsonRtti has a bug in this case:
    It tries to do "GetEnumName" on integers 0..max, and serializes weird thing

      "LockRotation" : [
        "\u0000",
        ""
      ]

    .. that it cannot deserialize back.
    The code below does serialization as if "jsoSetEnumeratedAsInteger in Options"
    but only for this type. We don't want to change serialization of sets of enums.
  }
  if (Info^.PropType^.Kind = tkSet) and (Info^.PropType^.Name = 'T3DCoords')  then
  begin
    FreeAndNil(Res);
    Res := SerializeSet(GetOrdProp(AObject, Info), 2 { manually synchronized with T3DCoord });
  end;
  if (Info^.PropType^.Kind = tkSet) and (Info^.PropType^.Name = 'TLayers')  then
  begin
    FreeAndNil(Res);
    Res := SerializeSet(GetOrdProp(AObject, Info), 30 { manually synchronized with TCastleTiledMap.TLayerIndex });
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

procedure InternalAssignUsingSerialization(const Destination, Source: TComponent);
var
  Json: TJsonObject;
  Reader: TCastleJsonReader;
  Writer: TCastleJsonWriter;
begin
  { We check proper inheritance here,
    this way we don't need to later care about whether Json has proper
      Json.Strings['$$ClassName']
    recorded. }
  if not Destination.InheritsFrom(Source.ClassType) then
    raise Exception.CreateFmt('Cannot assign instance of %s (source) to %s (destination). Destination class should be equal or inherit from Source class.', [
      Source.ClassName,
      Destination.ClassName
    ]);

  Writer := TCastleJsonWriter.Create;
  try
    Json := Writer.Streamer.ObjectToJson(Source);
    try
      Reader := TCastleJsonReader.Create;
      try
        Reader.FOwner := Destination;

        { read Result contents from JSON }
        Reader.DeStreamer.JsonToObject(Json, Destination);
        Reader.FinishResolvingComponentProperties;
      finally FreeAndNil(Reader) end;
    finally FreeAndNil(Json) end;
  finally FreeAndNil(Writer) end;
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
