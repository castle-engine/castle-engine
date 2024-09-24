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

{ Load IFC from JSON. }
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
    IsPrefix('TObjectList<', PotentialListClassName, false);
end;

function IfcJsonLoad(const Json: TJsonData): TIfcFile;

  procedure DeserializeJson(const Json: TJsonObject; const Ifc: TIfcPersistent);
  var
    DebugJsonKey: String;

    { From JsonChildObj create and read Pascal class.
      Returns @nil if not possible, which happens if "type" in IFC is not implemented
      on our side. }
    function CreateIfcInstance(const JsonChildObj: TJsonObject): TIfcPersistent;
    var
      IfcChildType: String;
      FindIfcClass: TIfcPersistentClass;
    begin
      IfcChildType:= JsonChildObj.Get('type', '');
      FindIfcClass := IfcClasses.FindIfcType(IfcChildType);
      if FindIfcClass = nil then
      begin
        WritelnWarning('IFC', 'IFC type "%s" (inside IFC field "%s") not implemented as a Pascal class, ignoring', [
          IfcChildType,
          DebugJsonKey
        ]);
        Exit(nil);
      end;

      Result := FindIfcClass.Create;
      DeserializeJson(JsonChildObj, Result);
    end;

    { From JsonChildObj create and read Pascal class,
      and place it in property PropInfo of Ifc. }
    procedure ReadIfcChildInstance(const PropInfo: PPropInfo;
      const JsonChildObj: TJsonObject);
    var
      IfcChild: TIfcPersistent;
    begin
      IfcChild := CreateIfcInstance(JsonChildObj);
      PropertySetInstance(Ifc, PropInfo, IfcChild);
    end;

    {$ifdef FPC}
      {$push}
      // Disable object check for our TIfcPersistentList cast hack
      {$objectchecks off}
    {$endif}

    { For all elements on JsonChildArray create and read Pascal class,
      and add it to property PropInfo of Ifc,
      which should be a list descendant. }
    procedure ReadIfcChildArray(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      IfcChild: TIfcPersistent;
      List: TIfcPersistentList;
      JsonChildObj: TJsonObject;
    begin
      { Cast to TIfcPersistentList because it is a cheat,
        actually everything is from generic class TObjectList<something specific>
        with "something specific" being a descendant of TIfcPersistent.
        There doesn't seem to be any other way to do this using old RTTI
        (from TypInfo vs new Rtti unit), we need to access this list using
        RTTI to add each member. }
      List := TIfcPersistentList(PropertyGetInstance(Ifc, PropInfo));
      if List = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a list, but it is nil', [
          PropInfo^.Name
        ]);

      { To be at least a bit safer, we check whether class name looks like a list.
        This is satisfied by all lists in castleinternalloadsaveifc_ifc_types.inc. }
      if not ClassNameOfList(List.ClassName) then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a list, but the class name does not end with "List": %s', [
          PropInfo^.Name,
          List.ClassName
        ]);

      for I := 0 to JsonChildArray.Count - 1 do
      begin
        if not (JsonChildArray.Items[I] is TJsonObject) then
        begin
          WritelnWarning('IFC', 'IFC array item %d (inside IFC field "%s") not a JSON object, ignoring', [
            I,
            DebugJsonKey
          ]);
          Continue;
        end;
        JsonChildObj := JsonChildArray.Items[I] as TJsonObject;
        IfcChild := CreateIfcInstance(JsonChildObj);
        if IfcChild <> nil then // if nil, then CreateIfcInstance already made warning
          List.Add(IfcChild);
      end;
    end;

    {$ifdef FPC}
      {$pop}
    {$endif}

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
    function JsonToString(const JsonValue: TJsonData): String;
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
    function JsonToFloat(const JsonValue: TJsonData): Single;
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
            PropertySetString(Ifc, PropInfo, JsonToString(Json.Items[I]));
          ptInteger:
            PropertySetInteger(Ifc, PropInfo, Json.Items[I].AsInt64);
          ptBoolean:
            PropertySetBoolean(Ifc, PropInfo, Json.Items[I].AsBoolean);
          ptFloat:
            PropertySetFloat(Ifc, PropInfo, JsonToFloat(Json.Items[I]));
          ptInstance:
            begin
              if Json.Items[I] is TJsonObject then
                ReadIfcChildInstance(PropInfo, Json.Items[I] as TJsonObject)
              else
              if Json.Items[I] is TJsonArray then
                ReadIfcChildArray(PropInfo, Json.Items[I] as TJsonArray)
              else
              begin
                WritelnWarning('IFC', 'IFC type "%s" expected to be a JSON object or array (because it is a Pascal class), ignoring', [
                  DebugJsonKey
                ]);
                Continue
              end;
            end;
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

      // TODO special code to hadnle TVector3 (TIfcDirection.DirectionRatios, TIfcCartesianPoint.Coordinate)
      // TODO: figure out ownership; should we just make them all TComponent, and owned by common owner?
      // TODO: resolve refs
    end;
  end;

var
  JsonObj: TJsonObject;
  TopLevelType: String;
  Project: TIfcProject;
begin
  InitializeIfcClasses; // make sure IfcClasses ready

  Result := TIfcFile.Create;
  if not (Json is TJsonObject) then
    raise EInvalidIfc.Create('Top-level JSON object expected');
  JsonObj := Json as TJsonObject;

  { Some JSON files use "IfcProject" at top-level
    (see https://github.com/buildingSMART/Sample-Test-Files/blob/master/IFC%204.0/BuildingSMARTSpec/wall-with-opening-and-window.json ),
    some use "IFC.JSON" at top-level and then "IfcProject" is inside "data".
    Account for both cases. }

  TopLevelType := JsonObj.Get('type', '');
  if TopLevelType = 'IfcProject' then
  begin
    Project := TIfcProject.Create;
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
end;

function IfcJsonSave(const IfcFile: TIfcFile): TJsonObject;

  function SerializeJson(const Ifc: TIfcPersistent): TJsonObject; forward;

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

        DebugPropName := IfcClassName + '.' + PropName;
        case PropertyType(PropInfo) of
          ptString:
            Result.Add(JsonKey, PropertyGetString(Ifc, PropInfo));
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
                if IfcObjProperty is TIfcPersistent then
                  Result.Add(JsonKey, SerializeJson(TIfcPersistent(IfcObjProperty)))
                else
                if ClassNameOfList(IfcObjProperty.ClassName) then
                  Result.Add(JsonKey, SerializeJsonList(IfcObjProperty))
                else
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
