{%MainUnit castleinternalloadsaveifc.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ IFC JSON reading and writing. }

{$ifdef read_interface}

{ Load IFC from JSON.

  All the IFC class instances in the resulting graph
  are owned by the returned TIfcFile instance, so you can easily
  free them by just freing the resulting TIfcFile.

  If you create your own instances of IFC classes, you can follow your
  own idea of a memory management. But if you want to keep it simple,
  and you want to free everything by just freeing the resulting
  TIfcFile instance, then just create all IFC instances with the owner
  being the returned TIfcFile instance. }
function IfcJsonLoad(const Json: TJsonData): TIfcFile;

{ Save IFC to JSON. }
function IfcJsonSave(const IfcFile: TIfcFile): TJsonObject;

{$endif read_interface}

{$ifdef read_implementation}

{ Simple hack to detect does given object is a TObjectList<xxx> specialization.
  They don't share any common ancestor in Generics.Collections in FPC,
  so there's no obvious "is" check to do this.
  This hack seems acceptable in this case -- as this is only internal
  and has to account only for classes inside
  our own castleinternalloadsaveifc_ifc_standard_types.inc,
  so we can rely on our own naming conventions. }
function ClassNameOfList(const PotentialListClassName: String): Boolean;
begin
  Result :=
    IsSuffix('List', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<', '>', PotentialListClassName, false);
end;

{ For a list in castleinternalloadsaveifc_ifc_standard_types.inc,
  return the class of the list children. }
function ClassOfListChildren(const List: TObject): TIfcPersistentClass;
var
  ChildClassName: String;
begin
  if IsPrefixSuffix('TObjectList<', '>', List.ClassName, false) then
    ChildClassName := PrefixSuffixRemove('TObjectList<', '>', List.ClassName, false)
  else
  if IsPrefixSuffix('T', 'List', List.ClassName, false) then
    ChildClassName := PrefixSuffixRemove('T', 'List', List.ClassName, false)
  else
    raise EInvalidIfc.CreateFmt('Cannot determine list children class from list class: %s', [
      List.ClassName
    ]);

  Result := IfcClasses.FindIfcType(ChildClassName);
  if Result = nil then
    raise EInvalidIfc.CreateFmt('Cannot find IFC class for list children class: %s (from list class %s)', [
      ChildClassName,
      List.ClassName
    ]);
end;

(*Convert JSON value from IFC to string.
  Accounts both for values specified directly, e.g.

    "name": "Default Building",
    "description": "Description of Default Building",

  and wrapped in JSON objects using IfcLabel or IfcText, e.g.

    "name" : {
      "type" : "IfcLabel",
      "value" : "Default Building"
    },
    "description" : {
      "type" : "IfcText",
      "value" : "Description of Default Building"
    },

  Both forms are found in practice, see
  https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json
  and a version of it imported + exported by Bonsai BIM.
*)
function JsonToString(const JsonValue: TJsonData; const DebugJsonKey: String): String;
var
  JsonObj: TJsonObject;
  JsonObjType: String;
begin
  if JsonValue is TJsonObject then
  begin
    JsonObj := JsonValue as TJsonObject;
    JsonObjType := JsonObj.Get('type', '');
    if (JsonObjType = 'IfcLabel') or
        (JsonObjType = 'IfcText') then
      Result := JsonObj.Get('value', '')
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcLabel" or "IfcText", ignoring', [
        DebugJsonKey
      ]);
      Result := '';
    end;
  end else
    Result := JsonValue.AsString;
end;

(*Convert JSON value from IFC to Int64.
  Accounts both for values specified directly, e.g.

    "creationDate": 1323724715,

  and wrapped in JSON objects e.g.

    "creationDate" : {
      "type" : "IfcTimeStamp",
      "value" : 1323724715
    }
*)
function JsonToInteger(const JsonValue: TJsonData; const DebugJsonKey: String): Int64;
var
  JsonObj: TJsonObject;
  JsonObjType: String;
begin
  if JsonValue is TJsonObject then
  begin
    JsonObj := JsonValue as TJsonObject;
    JsonObjType := JsonObj.Get('type', '');
    if JsonObjType = 'IfcTimeStamp' then
      Result := JsonObj.Get('value', Int64(0))
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcTimeStamp", ignoring', [
        DebugJsonKey
      ]);
      Result := 0;
    end;
  end else
    Result := JsonValue.AsInt64;
end;

(*Convert JSON value from IFC to string.
  Account for both wrapped and not values:

    "elevation": 0.0,

  or

    "elevation" : {
      "type" : "IfcLengthMeasure",
      "value" : 0.0
    }

  Both forms are found in practice, see
  https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json
  and a version of it imported + exported by Bonsai BIM.
*)
function JsonToFloat(const JsonValue: TJsonData; const DebugJsonKey: String): Single;
var
  JsonObj: TJsonObject;
  JsonObjType: String;
begin
  if JsonValue is TJsonObject then
  begin
    JsonObj := JsonValue as TJsonObject;
    JsonObjType := JsonObj.Get('type', '');
    if (JsonObjType = 'IfcPositiveLengthMeasure') or
        (JsonObjType = 'IfcLengthMeasure') or
        (JsonObjType = 'IfcReal') then
      Result := JsonObj.Get('value', 0.0)
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcReal" or "IfcLengthMeasure" or "IfcPositiveLengthMeasure", ignoring', [
        DebugJsonKey
      ]);
      Result := 0.0;
    end;
  end else
    Result := JsonValue.AsFloat;
end;

function IfcJsonLoad(const Json: TJsonData): TIfcFile;
var
  { All TIfcPersistent instances are created with this owner.
    This is just the returned TIfcFile instance, which is a TComponent. }
  IfcOwner: TComponent;
  { Map of currently known GUIDs -> TIfcPersistent instances.
    This is used to resolve "ref" in IFC throughout loading. }
  Refs: TCastleStringList;

  procedure DeserializeJson(const Json: TJsonObject; const Ifc: TIfcPersistent);
  var
    DebugJsonKey: String;

    { Get IFC object referenced by IfcRef.
      Returns either
      - a "ready" object (if this reference is already read),
      - or a stub (object with Ref property set, nothing more, to be resolved later),
      - or @nil (in case of error -- this now happens if reference is ready
        but wrong class).

      @param (RequiredClass Used to check if resolved reference
        is of the expected class, it also determines the stub type.
        Cannot be @nil.)
    }
    function GetIfcReference(const IfcRef: String; const RequiredClass: TIfcPersistentClass): TIfcPersistent;
    var
      IfcRefIndex: Integer;
    begin
      { We cannot allow RequiredClass to be nil.
        It would mean that we cannot create a good "stub" (when reference
        needs to be resolved later) that satisfies property required class. }
      Assert(RequiredClass <> nil);

      IfcRefIndex := Refs.IndexOf(IfcRef);
      if IfcRefIndex = -1 then
      begin
        // save the reference to resolve later
        Result := RequiredClass.Create(IfcOwner);
        Result.UnresolvedRef := IfcRef;
        Exit;
      end;

      Result := Refs.Objects[IfcRefIndex] as TIfcPersistent;
      if not (Result is RequiredClass) then
      begin
        WritelnWarning('IFC', 'IFC reference "%s" to class %s is not of the expected class, ignoring', [
          IfcRef,
          RequiredClass.ClassName
        ]);
        Exit(nil);
      end;

      // WritelnLog('IFC', 'Success: IFC reference "%s" to class %s resolved', [
      //   IfcRef,
      //   IfcChildType
      // ]);
    end;

    { From JsonChildObj create and read Pascal class (or return ready instance from Refs).
      Returns @nil if not possible, which happens if "type" in IFC is not implemented
      on our side.

      @param(SuggestedClass Optional (may be @nil) class that should be created
        for reading this IFC instance, if it does not have "type" specified.

        This workarounds invalid (as far as I know) IFC files,
        since they should always have "type" specified,
        but e.g. https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json
        has 2 occurences when "type" is not specified.)
    }
    function CreateIfcInstance(const JsonChildObj: TJsonObject;
      const SuggestedClass: TIfcPersistentClass = nil): TIfcPersistent;
    var
      IfcRef: String;
      IfcChildType: String;
      FindIfcClass: TIfcPersistentClass;
    begin
      IfcChildType:= JsonChildObj.Get('type', '');
      if IfcChildType = '' then
      begin
        FindIfcClass := SuggestedClass;
        if FindIfcClass = nil then
        begin
          WritelnWarning('IFC', 'IFC class (inside IFC field "%s") does not have "type" specified and none could be guessed, ignoring', [
            DebugJsonKey
          ]);
          Exit(nil);
        end;
      end else
      begin
        FindIfcClass := IfcClasses.FindIfcType(IfcChildType);
        if FindIfcClass = nil then
        begin
          WritelnWarning('IFC', 'IFC type "%s" (inside IFC field "%s") not implemented as a Pascal class, ignoring', [
            IfcChildType,
            DebugJsonKey
          ]);
          Exit(nil);
        end;
      end;

      IfcRef := JsonChildObj.Get('ref', '');
      if IfcRef <> '' then
      begin
        Assert(FindIfcClass <> nil);
        Result := GetIfcReference(IfcRef, FindIfcClass);
        Exit;
      end;

      Result := FindIfcClass.Create(IfcOwner);
      DeserializeJson(JsonChildObj, Result);
    end;

    { From JsonChildObj create and read Pascal class,
      and place it in property PropInfo of Ifc. }
    procedure ReadIfcChildInstance(const PropInfo: PPropInfo;
      const JsonChildObj: TJsonObject);
    var
      IfcChild: TIfcPersistent;
      SuggestedClass: TIfcPersistentClass;
    begin
      SuggestedClass := TIfcPersistentClass(PropertyGetInstanceClass(Ifc, PropInfo));
      IfcChild := CreateIfcInstance(JsonChildObj, SuggestedClass);
      PropertySetInstance(Ifc, PropInfo, IfcChild);
    end;

    { From elements of JsonChildArray read TVector3 and place it in
      property PropInfo of Ifc, which must be a TCastleVector3Persistent. }
    procedure ReadIfcChildVector3(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      VectorPersistent: TCastleVector3Persistent;
      Vector: TVector3;
    begin
      VectorPersistent := PropertyGetInstance(Ifc, PropInfo) as TCastleVector3Persistent;
      if VectorPersistent = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TCastleVector3Persistent, but it is nil', [
          PropInfo^.Name
        ]);

      // read JSON array into Vector:TVector3
      Vector := TVector3.Zero;
      for I := 0 to Vector.Count - 1 do
        if I < JsonChildArray.Count then
          Vector.Data[I] := JsonToFloat(JsonChildArray.Items[I], DebugJsonKey);

      VectorPersistent.Value := Vector;
    end;

    { From elements of JsonChildArray read TVector4 and place it in
      property PropInfo of Ifc, which must be a TCastleVector4Persistent. }
    procedure ReadIfcChildVector4(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      VectorPersistent: TCastleVector4Persistent;
      Vector: TVector4;
    begin
      VectorPersistent := PropertyGetInstance(Ifc, PropInfo) as TCastleVector4Persistent;
      if VectorPersistent = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TCastleVector4Persistent, but it is nil', [
          PropInfo^.Name
        ]);

      // read JSON array into Vector:TVector4
      Vector := TVector4.Zero;
      for I := 0 to Vector.Count - 1 do
        if I < JsonChildArray.Count then
          Vector.Data[I] := JsonToFloat(JsonChildArray.Items[I], DebugJsonKey);

      VectorPersistent.Value := Vector;
    end;

    {$ifdef FPC}
      {$push}
      // Disable object check for our TIfcPersistentList cast hack
      {$objectchecks off}
    {$endif}

    { Get the list property from PropInfo as TIfcPersistentList. }
    function GetListProperty(const PropInfo: PPropInfo): TIfcPersistentList;
    begin
      { Cast to TIfcPersistentList because it is a cheat,
        actually everything is from generic class TObjectList<something specific>
        with "something specific" being a descendant of TIfcPersistent.
        There doesn't seem to be any other way to do this using old RTTI
        (from TypInfo vs new Rtti unit), we need to access this list using
        RTTI to add each member. }
      Result := TIfcPersistentList(PropertyGetInstance(Ifc, PropInfo));
      if Result = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s.%s" is supposed to be a list, but it is nil', [
          Ifc.ClassName,
          PropInfo^.Name
        ]);

      { To be at least a bit safer, we check whether class name looks like a list.
        This is satisfied by all lists in castleinternalloadsaveifc_ifc_types.inc. }
      if not ClassNameOfList(Result.ClassName) then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a list, but the class name does not end with "List": %s', [
          PropInfo^.Name,
          Result.ClassName
        ]);
    end;

    { For all elements on JsonChildArray create and read Pascal class,
      and add it to property PropInfo of Ifc,
      which should be a list descendant. }
    procedure ReadIfcChildArray(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      List: TIfcPersistentList;
      I: Integer;
      IfcChild: TIfcPersistent;
      JsonChildObj: TJsonObject;
      RequiredClass: TIfcPersistentClass;
    begin
      List := GetListProperty(PropInfo);

      for I := 0 to JsonChildArray.Count - 1 do
      begin
        if JsonChildArray.Items[I] is TJsonObject then
        begin
          JsonChildObj := JsonChildArray.Items[I] as TJsonObject;
          IfcChild := CreateIfcInstance(JsonChildObj);
        end else
        if JsonChildArray.Items[I] is TJsonString then
        begin
          RequiredClass := ClassOfListChildren(List);
          IfcChild := GetIfcReference(JsonChildArray.Items[I].AsString, RequiredClass);
        end else
        begin
          WritelnWarning('IFC', 'IFC array item %d (inside IFC field "%s") not a JSON object or string, ignoring', [
            I,
            DebugJsonKey
          ]);
          Continue;
        end;
        if IfcChild <> nil then // if nil, then CreateIfcInstance already made warning
          List.Add(IfcChild);
      end;
    end;

    { Read one JSON object, add it as the only item to the list property PropInfo of Ifc.

      This is sometimes used by IFC JSON, see
      https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json#L70
      where "isDecomposedBy" (a list of IfcRelAggregates) is just a single
      instance of IfcRelAggregates. }
    procedure ReadIfcChildArrayOneItem(const PropInfo: PPropInfo;
      const JsonChildObj: TJsonObject);
    var
      List: TIfcPersistentList;
      IfcChild: TIfcPersistent;
    begin
      List := GetListProperty(PropInfo);
      IfcChild := CreateIfcInstance(JsonChildObj);
      if IfcChild <> nil then // if nil, then CreateIfcInstance already made warning
        List.Add(IfcChild);
    end;

    {$ifdef FPC}
      {$pop}
    {$endif}

    procedure ReadInstanceProperty(const PropInfo: PPropInfo; JsonInstanceValue: TJsonData);
    var
      CurrentInstance: TObject;
      RequiredClass: TIfcPersistentClass;
    begin
      CurrentInstance := PropertyGetInstance(Ifc, PropInfo);

      { Sometimes JSON gives a 1-item array when actually spec has "SET [0:1]",
        so it's not a list in Pascal.
        Example: TIfcProject.HasContext in
        https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json
        Tolerate it.
      }
      if (JsonInstanceValue is TJsonArray) and
         (not (CurrentInstance is TCastleVector3Persistent)) and
         (not (CurrentInstance is TCastleVector4Persistent)) and
         (not ClassNameOfList(PropertyGetInstanceClass(Ifc, PropInfo).ClassName)) then
      begin
        case TJsonArray(JsonInstanceValue).Count of
          0:begin
              PropertySetInstance(Ifc, PropInfo, nil);
              Exit;
            end;
          1:begin
              { Allow further code to read 1st array item
                (maybe TJsonObject, maybe TJsonString for ref). }
              JsonInstanceValue := TJsonArray(JsonInstanceValue).Items[0];
            end;
          else
          begin
            WritelnWarning('IFC', 'IFC field "%s" is a JSON array with more than 1 item, ignoring', [
              DebugJsonKey
            ]);
            Exit;
          end;
        end;
      end;

      if JsonInstanceValue is TJsonObject then
      begin
        if (CurrentInstance <> nil) and
            ClassNameOfList(CurrentInstance.ClassName) then
          ReadIfcChildArrayOneItem(PropInfo, JsonInstanceValue as TJsonObject)
        else
          ReadIfcChildInstance(PropInfo, JsonInstanceValue as TJsonObject)
      end else
      if JsonInstanceValue is TJsonArray then
      begin
        if CurrentInstance is TCastleVector3Persistent then
          ReadIfcChildVector3(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TCastleVector4Persistent then
          ReadIfcChildVector4(PropInfo, JsonInstanceValue as TJsonArray)
        else
          ReadIfcChildArray(PropInfo, JsonInstanceValue as TJsonArray)
      end else
      if JsonInstanceValue is TJsonString then
      begin
        // when a string is given, it's a reference to another object
        RequiredClass := TIfcPersistentClass(PropertyGetInstanceClass(Ifc, PropInfo));
        PropertySetInstance(Ifc, PropInfo, GetIfcReference(JsonInstanceValue.AsString, RequiredClass));
      end else
      begin
        WritelnWarning('IFC', 'IFC type "%s" expected to be a JSON object or array (because it is a Pascal class), ignoring', [
          DebugJsonKey
        ]);
      end;
    end;

  var
    I: Integer;
    JsonKey: String;
    PropInfo: PPropInfo;
  begin
    for I := 0 to Json.Count - 1 do
    begin
      JsonKey := Json.Names[I];
      if JsonKey = 'type' then // special field, handled by code that calls DeserializeJson
        Continue;
      DebugJsonKey := PrefixRemove('T', Ifc.ClassName, false) + '.' + JsonKey;

      PropInfo := GetPropInfo(Ifc.ClassInfo, JsonKey);
      if PropInfo = nil then
      begin
        WritelnWarning('IFC', 'IFC field "%s" not found in Pascal class %s, ignoring', [
          DebugJsonKey,
          Ifc.ClassName
        ]);
        Continue;
      end;

      try
        case PropertyType(PropInfo) of
          ptString:
            PropertySetString(Ifc, PropInfo, JsonToString(Json.Items[I], DebugJsonKey));
          ptEnumeration:
            PropertySetEnumStr(Ifc, PropInfo, Json.Items[I].AsString);
          ptInteger:
            PropertySetInteger(Ifc, PropInfo, JsonToInteger(Json.Items[I], DebugJsonKey));
          ptBoolean:
            PropertySetBoolean(Ifc, PropInfo, Json.Items[I].AsBoolean);
          ptFloat:
            PropertySetFloat(Ifc, PropInfo, JsonToFloat(Json.Items[I], DebugJsonKey));
          ptInstance:
            ReadInstanceProperty(PropInfo, Json.Items[I]);
          else
            begin
              WritelnWarning('IFC', 'IFC field "%s" found in Pascal class %s but with unhandled type, ignoring', [
                DebugJsonKey,
                Ifc.ClassName
              ]);
            end;
        end;
      except
        on E: EJson do
        begin
          { Enhance JSON error message, e.g. when unexpected JSON type occurs
            and the AsString above cannot convert.
            Also change type, to prevent recursively adding " (field "field")"
            to the message. }
          raise EInvalidIfc.Create(E.Message + Format(' (field "%s")', [DebugJsonKey]));
        end;
      end;
    end;

    if Ifc.GlobalId <> '' then
      Refs.AddObject(Ifc.GlobalId, Ifc);
  end;

  { Recursively resolve all remaining unresolved references in the IFC graph.

    This means we find all TIfcPersistent instances with Ref non-empty,
    and either resolve them (replacing with proper objects)
    or make a warning and set Ref to '' if not possible.

    By the end of this, Ref should be empty in all TIfcPersistent instances. }
  procedure ResolveReferences(const Ifc: TIfcPersistent);
  begin
    // TODO
  end;

var
  JsonObj: TJsonObject;
  TopLevelType: String;
  Project: TIfcProject;
begin
  InitializeIfcClasses; // make sure IfcClasses ready

  Result := TIfcFile.Create(nil);
  IfcOwner := Result;

  if not (Json is TJsonObject) then
    raise EInvalidIfc.Create('Top-level JSON object expected');
  JsonObj := Json as TJsonObject;

  Refs := TCastleStringList.Create;
  try
    { Some JSON files use "IfcProject" at top-level
      (see https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json ),
      some use "IFC.JSON" at top-level and then "IfcProject" is inside "data".
      Account for both cases. }
    TopLevelType := JsonObj.Get('type', '');
    if TopLevelType = 'IfcProject' then
    begin
      Project := TIfcProject.Create(IfcOwner);
      Result.Data.Add(Project);
      DeserializeJson(JsonObj, Project);
    end else
    begin
      if TopLevelType <> 'IFC.JSON' then
        WritelnWarning('IFC', 'Top-level JSON object should have "type" = "IfcProject" or "IFC.JSON", but is "%s"', [
          TopLevelType
        ]);
      DeserializeJson(JsonObj, Result);
    end;

    ResolveReferences(Result);
  finally FreeAndNil(Refs) end;
end;

function IfcJsonSave(const IfcFile: TIfcFile): TJsonObject;

  function SerializeJson(const Ifc: TIfcPersistent): TJsonObject; forward;

  { Serialize TVector3 to JSON array. }
  function SerializeJsonVector3(const Vector: TCastleVector3Persistent): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Value.Count - 1 do
      Result.Add(Vector.Value[I]);
  end;

  { Serialize TVector4 to JSON array. }
  function SerializeJsonVector4(const Vector: TCastleVector4Persistent): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Value.Count - 1 do
      Result.Add(Vector.Value[I]);
  end;

  {$ifdef FPC}
    {$push}
    // Disable object check for our TIfcPersistentList cast hack
    {$objectchecks off}
  {$endif}

  { For all elements on JsonChildArray create and read Pascal class,
    and add it to property PropInfo of Ifc,
    which should be a list descendant. }
  function SerializeJsonList(const ListRaw: TObject): TJsonArray;
  var
    I: Integer;
    List: TIfcPersistentList;
  begin
    List := TIfcPersistentList(ListRaw);
    Result := TJsonArray.Create;
    for I := 0 to List.Count - 1 do
      Result.Add(SerializeJson(List[I]));
  end;

  { Is given ListRaw a list, and it is not empty. }
  function NonEmptyList(const ListRaw: TObject): Boolean;
  var
    List: TIfcPersistentList;
  begin
    List := TIfcPersistentList(ListRaw);
    Result :=
      (List <> nil) and
      ClassNameOfList(List.ClassName) and
      (List.Count <> 0);
  end;

  {$ifdef FPC}
    {$pop}
  {$endif}

  function SerializeJson(const Ifc: TIfcPersistent): TJsonObject;
  var
    PropInfos: TPropInfoList;
    PropInfo: PPropInfo;
    PropName, DebugPropName, IfcClassName, JsonKey: String;
    I: Integer;
    IfcObjProperty: TObject;
    PropType: TPropertyType;
    HasDefaultValue: Boolean;
  begin
    Result := TJsonObject.Create;

    // save "type" in IFC
    IfcClassName := PrefixRemove('T', Ifc.ClassName, false);
    if Ifc is TIfcFile then
      Result.Add('type', 'IFC.JSON')
    else
      Result.Add('type', IfcClassName);

    PropInfos := TPropInfoList.Create(Ifc, tkProperties);
    try
      for I := 0 to PropInfos.Count - 1 do
      begin
        PropInfo := PropInfos.Items[I];
        PropName := PropInfo^.Name;

        // calculate key in JSON object to use for this property
        JsonKey := PropName;
        Assert(JsonKey <> '', 'Property name is empty');
        Assert(JsonKey <> 'type', 'Property name "type" is reserved for IFC');
        JsonKey[1] := LoCase(JsonKey[1]); // IFC wants camelCase not PascalCase

        PropType := PropertyType(PropInfo);

        HasDefaultValue := PropertyHasDefaultValue(Ifc, PropInfo) and
          (not (PropType in [ptInstance, ptOther]));
        { For simple types (strings, numbers, booleans) we don't save
          the default value, as it's not necessary (means shorter output,
          also means we don't save TComponent.Tag = 0 to IFC).
          For ptInstance, not saving default (nil) is already accounted for
          in "case" below.
          For ptOther, let "case" below make warning. }
        if HasDefaultValue then
          Continue;

        DebugPropName := IfcClassName + '.' + PropName;
        case PropType of
          ptString:
            Result.Add(JsonKey, PropertyGetString(Ifc, PropInfo));
          ptEnumeration:
            Result.Add(JsonKey, UpperCase(PropertyGetEnumStr(Ifc, PropInfo)));
          ptInteger:
            Result.Add(JsonKey, PropertyGetInteger(Ifc, PropInfo));
          ptBoolean:
            Result.Add(JsonKey, PropertyGetBoolean(Ifc, PropInfo));
          ptFloat:
            Result.Add(JsonKey, PropertyGetFloat(Ifc, PropInfo));
          ptInstance:
            begin
              IfcObjProperty := PropertyGetInstance(Ifc, PropInfo);
              if IfcObjProperty <> nil then
              begin
                // TODO: avoid infinite recursion because of cycles, use ref=xxx
                if IfcObjProperty is TCastleVector3Persistent then
                  Result.Add(JsonKey, SerializeJsonVector3(TCastleVector3Persistent(IfcObjProperty)))
                else
                if IfcObjProperty is TCastleVector4Persistent then
                  Result.Add(JsonKey, SerializeJsonVector4(TCastleVector4Persistent(IfcObjProperty)))
                else
                if IfcObjProperty is TIfcPersistent then
                  Result.Add(JsonKey, SerializeJson(TIfcPersistent(IfcObjProperty)))
                else
                if ClassNameOfList(IfcObjProperty.ClassName) then
                begin
                  // don't save empty lists, just for shorter output
                  if NonEmptyList(IfcObjProperty) then
                    Result.Add(JsonKey, SerializeJsonList(IfcObjProperty));
                end else
                  WritelnWarning('IFC', 'IFC field "%s" has unsupported class "%s", ignoring', [
                    DebugPropName,
                    IfcObjProperty.ClassName
                  ]);
              end;
            end;
          else
            begin
              WritelnWarning('IFC', 'IFC field "%s" found in Pascal class with unhandled type, ignoring', [
                DebugPropName
              ]);
            end;
        end;
      end;
    finally FreeAndNil(PropInfos) end;
  end;

begin
  Result := SerializeJson(IfcFile);
end;

{$endif read_implementation}
