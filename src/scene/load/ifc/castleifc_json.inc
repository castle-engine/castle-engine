{%MainUnit castleifc.pas}
{
  Copyright 2024-2025 Michalis Kamburelis.

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
function IfcJsonLoad(const Json: TJsonData): TIfcFile; overload;
function IfcJsonLoad(const Url: String): TIfcFile; overload;

{ Save IFC to JSON. }
function IfcJsonSave(const IfcFile: TIfcFile): TJsonObject; overload;
procedure IfcJsonSave(const IfcFile: TIfcFile; const Url: String); overload;

{$endif read_interface}

{$ifdef read_implementation}

{ Simple hack to detect does given object is a TObjectList<xxx> specialization
  and is a list of IFC classes.

  Reason: TObjectList<xxx> don't share any common ancestor
  in Generics.Collections in FPC,
  so there's no obvious "is" check to do this.
  This hack seems acceptable in this case -- as this is only internal
  and has to account only for classes inside
  our own castleifc_ifc_standard_types.inc,
  so we can rely on our own naming conventions. }
function ClassNameOfList(const PotentialListClassName: String): Boolean;
begin
  Result :=
    IsPrefixSuffix('TIfc', 'List', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<TIfc', '>', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<CastleIfc.TIfc', '>', PotentialListClassName, false);
end;

{ For a list of IFC classes in castleifc_ifc_standard_types.inc,
  return the class of the list children. }
function ClassOfListChildren(const List: TObject): TIfcPersistentClass;
var
  ChildClassName: String;
begin
  if IsPrefixSuffix('TIfc', 'List', List.ClassName, false) then
    ChildClassName := PrefixSuffixRemove('TIfc', 'List', List.ClassName, false)
  else
  if IsPrefixSuffix('TObjectList<TIfc', '>', List.ClassName, false) then
    ChildClassName := PrefixSuffixRemove('TObjectList<TIfc', '>', List.ClassName, false)
  else
  if IsPrefixSuffix('TObjectList<CastleIfc.TIfc', '>', List.ClassName, false) then
    ChildClassName := PrefixSuffixRemove('TObjectList<CastleIfc.TIfc', '>', List.ClassName, false)
  else
    raise EInvalidIfc.CreateFmt('Cannot determine list children class from list class: %s', [
      List.ClassName
    ]);

  // readd "Ifc" prefix, we want it for FindIfcType
  ChildClassName := 'Ifc' + ChildClassName;

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
  https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json
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
       (JsonObjType = 'IfcText') or
       (JsonObjType = 'IfcIdentifier') or
       // hack to read IfcValue from TIfcPropertySingleValue.NominalValue into String
       (DebugJsonKey = 'IfcPropertySingleValue.nominalValue') then
      Result := JsonObj.Get('value', '')
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcLabel" or "IfcText" or "IfcIdentifier", ignoring', [
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
    if (JsonObjType = 'IfcTimeStamp') or
       (JsonObjType = 'IfcDimensionCount') or
       // Testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/beam-varying-profiles.ifcjson
       (JsonObjType = 'IfcCardinalPointReference') or
       // Testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/beam-varying-profiles.ifcjson
       (JsonObjType = 'IfcInteger') or
       // Testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/beam-curved-i-shape-tessellated.ifcjson
       (JsonObjType = 'IfcPositiveInteger') then
      Result := JsonObj.Get('value', Int64(0))
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcTimeStamp" or "IfcDimensionCount", ignoring', [
        DebugJsonKey
      ]);
      Result := 0;
    end;
  end else
    Result := JsonValue.AsInt64;
end;

(*Convert JSON value from IFC to Boolean.
  Accounts both for values specified directly, e.g.

    "isVentilated" : false

  and wrapped in JSON objects e.g.

    "isVentilated" : {
      "type" : "IfcLogical",
      "value" : false
    },

  Testcase: wall-standard-case.ifcjson from IFC spec examples.
*)
function JsonToBoolean(const JsonValue: TJsonData; const DebugJsonKey: String): Boolean;
var
  JsonObj: TJsonObject;
  JsonObjType: String;
begin
  if JsonValue is TJsonObject then
  begin
    JsonObj := JsonValue as TJsonObject;
    JsonObjType := JsonObj.Get('type', '');
    if (JsonObjType = 'IfcLogical') or
       (JsonObjType = 'IfcBoolean') then
      Result := JsonObj.Get('value', false)
    else
    begin
      WritelnWarning('IFC', 'IFC field "%s" is a JSON object, but not of type "IfcLogical", ignoring', [
        DebugJsonKey
      ]);
      Result := false;
    end;
  end else
    Result := JsonValue.AsBoolean;
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
  https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json
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
       (JsonObjType = 'IfcNonNegativeLengthMeasure') or
       (JsonObjType = 'IfcLengthMeasure') or
       (JsonObjType = 'IfcReal') or
       (JsonObjType = 'IfcAreaMeasure') or
       // Testcase: Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/column-straight-rectangle-tessellation.ifcjson
       (JsonObjType = 'IfcParameterValue') then
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

(*Read JSON into TVector2, TVector3 or TVector4.
  Json can be an array, like [1, 2, 3].
  Or an object like

  {
    "type" : "IfcCartesianPoint",
    "coordinates" : [ {
      "type" : "IfcLengthMeasure",
      "value" : -0.0796551778912544
    }, {
      "type" : "IfcLengthMeasure",
      "value" : 0.0758620649576187
    }, {
      "type" : "IfcLengthMeasure",
      "value" : -0.28099998831749
    } ]
  }

  See testcase
  Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/slab-openings.ifcjson,
  to test 2D version, for IfcCartesianPointList2D.
*)
procedure JsonToVectorGeneric(const Json: TJsonData; const DebugJsonKey: String;
  const Vector: PSingle; const VectorCount: Integer);
var
  I: Integer;
  JsonArray: TJsonArray;
  JsonObj: TJsonObject;
begin
  for I := 0 to VectorCount - 1 do
    Vector[I] := 0.0;

  if Json is TJsonArray then
  begin
    JsonArray := TJsonArray(Json);
    for I := 0 to Min(VectorCount, JsonArray.Count) - 1 do
      Vector[I] := JsonToFloat(JsonArray.Items[I], DebugJsonKey);
  end else
  if Json is TJsonObject then
  begin
    JsonObj := TJsonObject(Json);
    if (JsonObj.Get('type', '') = 'IfcCartesianPoint') and
       (JsonObj.Find('coordinates') is TJsonArray) then // also checks JsonObj.Find('coordinates') is not nil
    begin
      // recursive call to read from "coordinates" field
      JsonToVectorGeneric(JsonObj.Find('coordinates'), DebugJsonKey, Vector, VectorCount);
    end else
    // Testcase: normals in Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/column-straight-rectangle-tessellation.ifcjson
    if (JsonObj.Get('type', '') = '') and
       (JsonObj.Find('parameterValues') is TJsonArray) then // also checks JsonObj.Find('coordinates') is not nil
    begin
      // recursive call to read from "coordinates" field
      JsonToVectorGeneric(JsonObj.Find('parameterValues'), DebugJsonKey, Vector, VectorCount);
    end else
    begin
      WritelnWarning('IFC', 'Cannot read vector: IFC field "%s" is a JSON object, but not of type "IfcCartesianPoint" or with "parameterValues"', [
        DebugJsonKey
      ]);
    end;
  end else
  begin
    WritelnWarning('IFC', 'Cannot read vector: IFC field "%s" is not a JSON array or object', [
      DebugJsonKey
    ]);
  end;
end;

function JsonToVector2(const Json: TJsonData; const DebugJsonKey: String): TVector2;
begin
  JsonToVectorGeneric(Json, DebugJsonKey, @Result.Data, 2);
end;

function JsonToVector3(const Json: TJsonData; const DebugJsonKey: String): TVector3;
begin
  JsonToVectorGeneric(Json, DebugJsonKey, @Result.Data, 3);
end;

function JsonToVector4(const Json: TJsonData; const DebugJsonKey: String): TVector4;
begin
  JsonToVectorGeneric(Json, DebugJsonKey, @Result.Data, 4);
end;

(*Read JSON into TVector2Integer, TVector3Integer or TVector4Integer.
  Json can be an array, like [1, 2, 3].
  Or an object like

  Or an object like

  {
    "positiveIntegers" : [ {
      "type" : "IfcPositiveInteger",
      "value" : 57
    }, {
      "type" : "IfcPositiveInteger",
      "value" : 6
    }, {
      "type" : "IfcPositiveInteger",
      "value" : 59
    } ]
  }

  Testcase:
  Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/beam-curved-i-shape-tessellated.ifcjson
*)
procedure JsonToVectorIntegerGeneric(const Json: TJsonData; const DebugJsonKey: String;
  const Vector: PInteger; const VectorCount: Integer);
var
  I: Integer;
  JsonArray: TJsonArray;
  JsonObj: TJsonObject;
  JsonObjType: String;
  JsonObjValue: TJsonData;
begin
  for I := 0 to VectorCount - 1 do
    Vector[I] := 0;

  if Json is TJsonArray then
  begin
    JsonArray := TJsonArray(Json);
    for I := 0 to Min(VectorCount, JsonArray.Count) - 1 do
      Vector[I] := JsonToInteger(JsonArray.Items[I], DebugJsonKey);
  end else
  if Json is TJsonObject then
  begin
    JsonObj := TJsonObject(Json);
    JsonObjType := JsonObj.Get('type', '');
    JsonObjValue := JsonObj.Find('positiveIntegers');
    if (JsonObjType = '') and
       (JsonObjValue is TJsonArray) then // also checks JsonObjValue is not nil
    begin
      // recursive call to read from "coordinates" field
      JsonToVectorIntegerGeneric(JsonObjValue, DebugJsonKey, Vector, VectorCount);
    end else
    begin
      WritelnWarning('IFC', 'Cannot read vector: IFC field "%s" is a JSON object, but not of type "IfcCartesianPoint"', [
        DebugJsonKey
      ]);
    end;
  end else
  begin
    WritelnWarning('IFC', 'Cannot read vector: IFC field "%s" is not a JSON array or object', [
      DebugJsonKey
    ]);
  end;
end;

{ Read JSON into TVector3Integer. }
function JsonToVector3Integer(const Json: TJsonData; const DebugJsonKey: String): TVector3Integer;
begin
  JsonToVectorIntegerGeneric(Json, DebugJsonKey, @Result.Data, 3);
end;

{ Return random GUID, formatted as common in IFC JSON encoding. }
function RandomGuidIfc: String;
var
  MyGuid: TGUID;
begin
  CreateGUID(MyGuid);
  Result := PrefixRemove('{', SuffixRemove('}', GUIDToString(MyGuid), false), false);
  Result := LowerCase(Result);
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
        but e.g. https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json
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
      VectorPersistent: TCastleVector3Persistent;
    begin
      VectorPersistent := PropertyGetInstance(Ifc, PropInfo) as TCastleVector3Persistent;
      if VectorPersistent = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TCastleVector3Persistent, but it is nil', [
          PropInfo^.Name
        ]);
      VectorPersistent.Value := JsonToVector3(JsonChildArray, DebugJsonKey);
    end;

    { From elements of JsonChildArray read TVector4 and place it in
      property PropInfo of Ifc, which must be a TCastleVector4Persistent. }
    procedure ReadIfcChildVector4(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      VectorPersistent: TCastleVector4Persistent;
    begin
      VectorPersistent := PropertyGetInstance(Ifc, PropInfo) as TCastleVector4Persistent;
      if VectorPersistent = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TCastleVector4Persistent, but it is nil', [
          PropInfo^.Name
        ]);
      VectorPersistent.Value := JsonToVector4(JsonChildArray, DebugJsonKey);
    end;

    { From elements of JsonChildArray read TCastleStringList contents
      into property PropInfo of Ifc, which must be a TCastleStringList. }
    procedure ReadIfcChildStringList(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      StringList: TCastleStringList;
    begin
      StringList := PropertyGetInstance(Ifc, PropInfo) as TCastleStringList;
      if StringList = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TCastleStringList, but it is nil', [
          PropInfo^.Name
        ]);
      StringList.Clear;
      for I := 0 to JsonChildArray.Count - 1 do
        StringList.Add(JsonToString(JsonChildArray.Items[I], DebugJsonKey));
    end;

    { From elements of JsonChildArray read TIntegerList contents
      into property PropInfo of Ifc, which must be a TIntegerList. }
    procedure ReadIfcChildIntegerList(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      IntegerList: TIntegerList;
    begin
      IntegerList := PropertyGetInstance(Ifc, PropInfo) as TIntegerList;
      if IntegerList = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TIntegerList, but it is nil', [
          PropInfo^.Name
        ]);
      IntegerList.Clear;
      for I := 0 to JsonChildArray.Count - 1 do
        IntegerList.Add(JsonToInteger(JsonChildArray.Items[I], DebugJsonKey));
    end;

    { From elements of JsonChildArray read TVector2List contents
      into property PropInfo of Ifc, which must be a TVector2List. }
    procedure ReadIfcChildVector2List(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      Vector2List: TVector2List;
    begin
      Vector2List := PropertyGetInstance(Ifc, PropInfo) as TVector2List;
      if Vector2List = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TVector2List, but it is nil', [
          PropInfo^.Name
        ]);
      Vector2List.Clear;
      for I := 0 to JsonChildArray.Count - 1 do
        Vector2List.Add(JsonToVector2(JsonChildArray.Items[I], DebugJsonKey));
    end;

    { From elements of JsonChildArray read TVector3List contents
      into property PropInfo of Ifc, which must be a TVector3List. }
    procedure ReadIfcChildVector3List(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      VecList: TVector3List;
    begin
      VecList := PropertyGetInstance(Ifc, PropInfo) as TVector3List;
      if VecList = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TVector3List, but it is nil', [
          PropInfo^.Name
        ]);
      VecList.Clear;
      for I := 0 to JsonChildArray.Count - 1 do
        VecList.Add(JsonToVector3(JsonChildArray.Items[I], DebugJsonKey));
    end;

    { From elements of JsonChildArray read TVector3IntegerList contents
      into property PropInfo of Ifc, which must be a TVector3IntegerList. }
    procedure ReadIfcChildVector3IntegerList(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      I: Integer;
      VecList: TVector3IntegerList;
    begin
      VecList := PropertyGetInstance(Ifc, PropInfo) as TVector3IntegerList;
      if VecList = nil then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a TVector3IntegerList, but it is nil', [
          PropInfo^.Name
        ]);
      VecList.Clear;
      for I := 0 to JsonChildArray.Count - 1 do
        VecList.Add(JsonToVector3Integer(JsonChildArray.Items[I], DebugJsonKey));
    end;

    (*From elements of JsonChildArray read TIfcCompoundPlaneAngleMeasure instance
      into property PropInfo of Ifc. The property must accept instances of
      TIfcCompoundPlaneAngleMeasure.
      This is necessary as CompoundPlaneAngleMeasure may be recorded in IFC as
      object:

        "refLatitude" : {
          "type" : "IfcCompoundPlaneAngleMeasure",
          "degrees" : 24,
          "minutes" : 28,
          "seconds" : 0,
          "microSeconds" : 0
        },
        "refLongitude" : {
          "type" : "IfcCompoundPlaneAngleMeasure",
          "degrees" : 54,
          "minutes" : 25,
          "seconds" : 0,
          "microSeconds" : 0
        },

      or as integers array:

        "refLatitude": [
            24,
            28,
            0
        ],
        "refLongitude": [
            54,
            25,
            0
        ],

      This function takes care of handling the 2nd form. *)
    procedure ReadIfcChildArrayCompoundPlaneAngleMeasure(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray);
    var
      AngleMeasure: TIfcCompoundPlaneAngleMeasure;
      OldPropValue: TObject;
    begin
      AngleMeasure := TIfcCompoundPlaneAngleMeasure.Create(IfcOwner);
      if JsonChildArray.Count >= 1 then
        AngleMeasure.Degrees := JsonToInteger(JsonChildArray.Items[0], DebugJsonKey);
      if JsonChildArray.Count >= 2 then
        AngleMeasure.Minutes := JsonToInteger(JsonChildArray.Items[1], DebugJsonKey);
      if JsonChildArray.Count >= 3 then
        AngleMeasure.Seconds := JsonToInteger(JsonChildArray.Items[2], DebugJsonKey);
      if JsonChildArray.Count >= 4 then
        AngleMeasure.MicroSeconds := JsonToInteger(JsonChildArray.Items[3], DebugJsonKey);
      OldPropValue := PropertyGetInstance(Ifc, PropInfo);
      FreeAndNil(OldPropValue);
      PropertySetInstance(Ifc, PropInfo, AngleMeasure);
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
        This is satisfied by all lists in castleifc_ifc_types.inc. }
      if not ClassNameOfList(Result.ClassName) then
        raise EInvalidIfc.CreateFmt('Pascal property "%s" is supposed to be a list, but the class name does not end with "List": %s', [
          PropInfo^.Name,
          Result.ClassName
        ]);
    end;

    { For all elements on JsonChildArray create and read Pascal class,
      and add it to List. }
    procedure ReadIfcChildArray(const List: TIfcPersistentList;
      const JsonChildArray: TJsonArray); overload;
    var
      I: Integer;
      IfcChild: TIfcPersistent;
      JsonChildObj: TJsonObject;
      RequiredClass: TIfcPersistentClass;
    begin
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
        begin
          RequiredClass := ClassOfListChildren(List);
          if not IfcChild.InheritsFrom(RequiredClass) then
            WritelnWarning('IFC array item of class %s (index %d, inside IFC field "%s") is not of the expected class (%s), ignoring', [
              IfcChild.ClassName,
              I,
              DebugJsonKey,
              RequiredClass.ClassName
            ]) else
          List.Add(IfcChild);
        end;
      end;
    end;

    { For all elements on JsonChildArray create and read Pascal class,
      and add it to property PropInfo of Ifc,
      which should be a list descendant. }
    procedure ReadIfcChildArray(const PropInfo: PPropInfo;
      const JsonChildArray: TJsonArray); overload;
    begin
      ReadIfcChildArray(GetListProperty(PropInfo), JsonChildArray);
    end;

    { Read one JSON object, add it as the only item to the list property PropInfo of Ifc.

      This is sometimes used by IFC JSON, see
      https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json#L70
      where "isDecomposedBy" (a list of IfcRelAggregates) is just a single
      instance of IfcRelAggregates. }
    procedure ReadIfcChildArrayOneItem(const PropInfo: PPropInfo;
      const JsonChildObj: TJsonObject);
    var
      List: TIfcPersistentList;
      IfcChild: TIfcPersistent;
      RequiredClass: TIfcPersistentClass;
    begin
      List := GetListProperty(PropInfo);
      IfcChild := CreateIfcInstance(JsonChildObj);
      if IfcChild <> nil then // if nil, then CreateIfcInstance already made warning
      begin
        RequiredClass := ClassOfListChildren(List);
        if not IfcChild.InheritsFrom(RequiredClass) then
            WritelnWarning('IFC array item of class %s (inside IFC field "%s") is not of the expected class (%s), ignoring', [
              IfcChild.ClassName,
              DebugJsonKey,
              RequiredClass.ClassName
            ]) else
          List.Add(IfcChild);
      end;
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
      RequiredClass := TIfcPersistentClass(PropertyGetInstanceClass(Ifc, PropInfo));

      { Sometimes JSON gives a 1-item array when actually spec has "SET [0:1]",
        so it's not a list in Pascal.
        Example: TIfcProject.HasContext in
        https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json
        Tolerate it.
      }
      if (JsonInstanceValue is TJsonArray) and
         (not (CurrentInstance is TCastleVector3Persistent)) and
         (not (CurrentInstance is TCastleVector4Persistent)) and
         (not (CurrentInstance is TCastleStringList)) and
         (not (CurrentInstance is TIntegerList)) and
         (not (CurrentInstance is TVector2List)) and
         (not (CurrentInstance is TVector3List)) and
         (not (CurrentInstance is TVector3IntegerList)) and
         (not RequiredClass.InheritsFrom(TIfcCompoundPlaneAngleMeasure)) and
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
        if RequiredClass.InheritsFrom(TIfcCompoundPlaneAngleMeasure) then
          ReadIfcChildArrayCompoundPlaneAngleMeasure(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TCastleStringList then
          ReadIfcChildStringList(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TIntegerList then
          ReadIfcChildIntegerList(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TVector2List then
          ReadIfcChildVector2List(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TVector3List then
          ReadIfcChildVector3List(PropInfo, JsonInstanceValue as TJsonArray)
        else
        if CurrentInstance is TVector3IntegerList then
          ReadIfcChildVector3IntegerList(PropInfo, JsonInstanceValue as TJsonArray)
        else
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
        PropertySetInstance(Ifc, PropInfo, GetIfcReference(JsonInstanceValue.AsString, RequiredClass));
      end else
      begin
        WritelnWarning('IFC', 'IFC type "%s" expected to be a JSON object or array (because it is a Pascal class), ignoring', [
          DebugJsonKey
        ]);
      end;
    end;

    { Read "data" list to TIfcFile.TemporaryData. }
    procedure ReadFileData(const IfcFile: TIfcFile; const JsonArray: TJsonArray);
    begin
      if IfcFile.TemporaryData <> nil then
        raise EInvalidIfc.Create('Duplicate "data" field in IFC file');
      IfcFile.TemporaryData := TIfcPersistentList.Create(false);
      ReadIfcChildArray(IfcFile.TemporaryData, JsonArray);
      IfcFile.Project := TIfcFile.ProjectFromDataList(IfcFile.TemporaryData);
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
      DebugJsonKey := Ifc.IfcClassName + '.' + JsonKey;

      { Special handling of "data" list within TIfcFile.
        TIfcFile.Data is not a published property (because it would make sense
        only temporarily during loading), so we load it with dedicated code here. }
      if (JsonKey = 'data') and
         (Json.Items[I] is TJsonArray) and
         (Ifc is TIfcFile) then
      begin
        ReadFileData(TIfcFile(Ifc), Json.Items[I] as TJsonArray);
        Continue;
      end;

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
            PropertySetBoolean(Ifc, PropInfo, JsonToBoolean(Json.Items[I], DebugJsonKey));
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

  {$ifdef FPC}
    {$push}
    // Disable object check for our TIfcPersistentList cast hack
    {$objectchecks off}
  {$endif}

  { Recursively resolve all remaining unresolved references in the IFC graph.

    This means we find all TIfcPersistent instances with UnresolvedRef non-empty,
    and either resolve them (replacing with proper objects)
    or make a warning and set UnresolvedRef to '' if not possible.

    By the end of this, UnresolvedRef should be empty in all TIfcPersistent instances. }
  procedure ResolveReferences(const IfcFile: TIfcFile);
  var
    Visited: TIfcPersistentList;

    { Resolve reference indicated by IfcToResolve,
      which must have UnresolvedRef non-empty.
      Sets NewIfc to the resolved reference (if success)
      or makes a warning and sets to IfcToResolve (if failure). }
    procedure ResolveRef(const IfcToResolve: TIfcPersistent; out NewIfc: TIfcPersistent);
    var
      IfcRefIndex: Integer;
    begin
      Assert(IfcToResolve.UnresolvedRef <> '');
      IfcRefIndex := Refs.IndexOf(IfcToResolve.UnresolvedRef);
      if IfcRefIndex = -1 then
      begin
        WritelnWarning('IFC', 'IFC reference "%s" (class "%s") not found, leaving unresolved', [
          IfcToResolve.UnresolvedRef,
          IfcToResolve.ClassName
        ]);
        IfcToResolve.UnresolvedRef := '';
        NewIfc := IfcToResolve;
      end else
      begin
        NewIfc := Refs.Objects[IfcRefIndex] as TIfcPersistent;
        Assert(NewIfc <> nil);
      end;
    end;

    procedure ResolveReferencesCore(const Ifc: TIfcPersistent); forward;

    procedure ResolveReferencesList(const List: TIfcPersistentList);
    var
      J: Integer;
      IfcObjPropertyPersistent, NewIfcObjProperty: TIfcPersistent;
    begin
      for J := 0 to List.Count - 1 do
      begin
        IfcObjPropertyPersistent := List[J];
        if IfcObjPropertyPersistent.UnresolvedRef <> '' then
        begin
          ResolveRef(IfcObjPropertyPersistent, NewIfcObjProperty);
          List[J] := NewIfcObjProperty;
        end else
          ResolveReferencesCore(IfcObjPropertyPersistent);
      end;
    end;

    { Recursive subroutine of ResolveReferences. }
    procedure ResolveReferencesCore(const Ifc: TIfcPersistent);
    var
      PropInfos: TPropInfoList;
      PropInfo: PPropInfo;
      PropType: TPropertyType;
      IfcObjProperty: TObject;
      IfcObjPropertyPersistent, NewIfcObjProperty: TIfcPersistent;
      I: Integer;
    begin
      if Visited.Contains(Ifc) then
        Exit; // already visited, exit early to avoid cycles
      Visited.Add(Ifc);

      PropInfos := TPropInfoList.Create(Ifc, tkProperties);
      try
        for I := 0 to PropInfos.Count - 1 do
        begin
          PropInfo := PropInfos.Items[I];
          PropType := PropertyType(PropInfo);
          if PropType = ptInstance then
          begin
            IfcObjProperty := PropertyGetInstance(Ifc, PropInfo);
            if (IfcObjProperty <> nil) and
              ClassNameOfList(IfcObjProperty.ClassName) then
            begin
              { List of instances. Resolve references in each of them. }
              ResolveReferencesList(TIfcPersistentList(IfcObjProperty));
            end else
            if IfcObjProperty is TIfcPersistent then
            begin
              { Single instance. Resolve it, or recursively resolve references in it. }
              IfcObjPropertyPersistent := TIfcPersistent(IfcObjProperty);
              if IfcObjPropertyPersistent.UnresolvedRef <> '' then
              begin
                ResolveRef(IfcObjPropertyPersistent, NewIfcObjProperty);
                PropertySetInstance(Ifc, PropInfo, NewIfcObjProperty);
              end else
                ResolveReferencesCore(IfcObjPropertyPersistent);
            end;
          end;
        end;
      finally FreeAndNil(PropInfos) end;
    end;

  begin
    Visited := TIfcPersistentList.Create(false);
    try
      ResolveReferencesCore(IfcFile);

      { Special handling for unpublished TIfcFile.TemporaryData,
        where we also need to resolve references,
        to have everything in TIfcFile.Project (that refers to one item
        on this list) resolved. }
      if IfcFile.TemporaryData <> nil then
        ResolveReferencesList(IfcFile.TemporaryData);
    finally FreeAndNil(Visited) end;
  end;

  {$ifdef FPC}
    {$pop}
  {$endif}

var
  JsonObj: TJsonObject;
  TopLevelType: String;
  Project: TIfcProject;
begin
  Result := TIfcFile.Create(nil);
  IfcOwner := Result;

  if not (Json is TJsonObject) then
    raise EInvalidIfc.Create('Top-level JSON object expected');
  JsonObj := Json as TJsonObject;

  Refs := TCastleStringList.Create;
  try
    { Some JSON files use "IfcProject" at top-level
      (see https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json ),
      some use "IFC.JSON" at top-level and then "IfcProject" is inside "data".
      Account for both cases. }
    TopLevelType := JsonObj.Get('type', '');
    if TopLevelType = 'IfcProject' then
    begin
      Project := TIfcProject.Create(IfcOwner);
      Result.Project := Project;
      DeserializeJson(JsonObj, Project);
    end else
    begin
      { BonsaiBIM, and CGE own saving, set IFC.JSON.
        Converters on
          https://github.com/buildingsmart-community/ifcJSON ,
          https://github.com/michaliskambi/ifcJSON
        set ifcJSON.
        Accept both }
      if (TopLevelType <> 'IFC.JSON') and
         (TopLevelType <> 'ifcJSON') then
        WritelnWarning('IFC', 'Top-level JSON object should have "type" = "IfcProject", "IFC.JSON", "ifcJSON", but is "%s"', [
          TopLevelType
        ]);
      DeserializeJson(JsonObj, Result);
    end;

    ResolveReferences(Result);

    FreeAndNil(Result.TemporaryData);
  finally FreeAndNil(Refs) end;
end;

function IfcJsonSave(const IfcFile: TIfcFile): TJsonObject;
var
  { List of currently saved TIfcPersistent instances.
    This is used to decide when to write out only reference
    (when given instance is already saved) and when full instance. }
  Refs: TIfcPersistentList;

  function SerializeJson(const Ifc: TIfcPersistent;
    const SerializingFileData: Boolean): TJsonObject; forward;

  { Serialize TVector2 to JSON array. }
  function SerializeJsonVector2(const Vector: TVector2): TJsonArray; overload;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Count - 1 do
      Result.Add(Vector.Data[I]);
  end;

  { Serialize TVector3 to JSON array. }
  function SerializeJsonVector3(const Vector: TVector3): TJsonArray; overload;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Count - 1 do
      Result.Add(Vector.Data[I]);
  end;

  function SerializeJsonVector3(const Vector: TCastleVector3Persistent): TJsonArray; overload;
  begin
    Result := SerializeJsonVector3(Vector.Value);
  end;

  { Serialize TVector3Integer to JSON array. }
  function SerializeJsonVector3Integer(const Vector: TVector3Integer): TJsonArray; overload;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Count - 1 do
      Result.Add(Vector.Data[I]);
  end;

  { Serialize TVector4 to JSON array. }
  function SerializeJsonVector4(const Vector: TVector4): TJsonArray; overload;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector.Count - 1 do
      Result.Add(Vector.Data[I]);
  end;

  function SerializeJsonVector4(const Vector: TCastleVector4Persistent): TJsonArray; overload;
  begin
    Result := SerializeJsonVector4(Vector.Value);
  end;

  { Serialize TCastleStringList to JSON array. }
  function SerializeJsonStringList(const StringList: TCastleStringList): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to StringList.Count - 1 do
      Result.Add(StringList[I]);
  end;

  { Serialize TIntegerList to JSON array. }
  function SerializeJsonIntegerList(const IntegerList: TIntegerList): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to IntegerList.Count - 1 do
      Result.Add(IntegerList[I]);
  end;

  { Serialize TVector2List to JSON array. }
  function SerializeJsonVector2List(const Vector2List: TVector2List): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to Vector2List.Count - 1 do
      Result.Add(SerializeJsonVector2(Vector2List[I]));
  end;

  { Serialize TVector3List to JSON array. }
  function SerializeJsonVector3List(const VecList: TVector3List): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to VecList.Count - 1 do
      Result.Add(SerializeJsonVector3(VecList[I]));
  end;

  { Serialize TVector3IntegerList to JSON array. }
  function SerializeJsonVector3IntegerList(const VecList: TVector3IntegerList): TJsonArray;
  var
    I: Integer;
  begin
    Result := TJsonArray.Create;
    for I := 0 to VecList.Count - 1 do
      Result.Add(SerializeJsonVector3Integer(VecList[I]));
  end;

  {$ifdef FPC}
    {$push}
    // Disable object check for our TIfcPersistentList cast hack
    {$objectchecks off}
  {$endif}

  { For all elements on JsonChildArray create and read Pascal class,
    and add it to property PropInfo of Ifc,
    which should be a list descendant. }
  function SerializeJsonList(const ListRaw: TObject;
    const SerializingFileData: Boolean): TJsonArray;
  var
    I: Integer;
    List: TIfcPersistentList;
  begin
    List := TIfcPersistentList(ListRaw);
    Result := TJsonArray.Create;
    for I := 0 to List.Count - 1 do
      Result.Add(SerializeJson(List[I], SerializingFileData));
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

var
  { Result of TIfcFile.CreateDataList. }
  FileData: TIfcPersistentList;

  {$ifdef FPC}
    {$pop}
  {$endif}

  { Serialize TIfcPersistent instance to JSON object.
    @param(SerializingFileData When we iterate over data list
      returned by TIfcFile.CreateDataList) }
  function SerializeJson(const Ifc: TIfcPersistent;
    const SerializingFileData: Boolean): TJsonObject;
  var
    PropInfos: TPropInfoList;
    PropInfo: PPropInfo;
    PropName, DebugPropName, IfcClassName, JsonKey: String;
    I: Integer;
    IfcObjProperty: TObject;
    PropType: TPropertyType;
    RefsContains, HasDefaultValue: Boolean;
  begin
    Result := TJsonObject.Create;

    // save "type" in IFC
    IfcClassName := Ifc.IfcClassName;
    if Ifc is TIfcFile then
      Result.Add('type', 'IFC.JSON')
    else
      Result.Add('type', IfcClassName);

    { Serialize TIfcPersistent instance to JSON object using Ifc.GlobalId. }
    RefsContains := Refs.Contains(Ifc);
    if RefsContains and (Ifc.GlobalId <> '') then
    begin
      Result.Add('ref', Ifc.GlobalId);
      Exit;
    end;

    { All IFC objects must be listed (not as references)
      on the "data" list. This make JSON read correctly by conversion scripts
      in https://github.com/buildingsmart-community/ifcJSON .
      This implies that we only write a reference to an object,
      if it should be instead present on the "data" list. }
    if (FileData.IndexOf(Ifc) <> -1) and
       (not SerializingFileData) then
    begin
      Result.Add('ref', Ifc.GlobalId);
      Exit;
    end;

    // update Refs
    if RefsContains then
    begin
      {$ifdef CASTLE_DEBUG_IFC_MISSING_GUID}
      WritelnWarning
      {$else}
      raise EInvalidIfc.CreateFmt
      {$endif}
      ('IFC instance of "%s" (GUID "%s") was already saved to IFC file.' + NL +
        '  If the GUID is empty then maybe the %s class, or one of things that contain it in IFC file, needs WantsGlobalId = true.' + NL +
        '  If GUID is not empty, then our logic to avoid writing full class more than once (and avoid infinite recursion when writing) is faulty.' + NL +
        '  Report a bug with testcase either way.', [
        Ifc.ClassName,
        Ifc.GlobalId,
        Ifc.ClassName
      ]);
      Result.Add('warning', Format('IFC instance of "%s" (GUID "%s") was already saved to IFC file', [
        Ifc.ClassName,
        Ifc.GlobalId
      ]));
      Exit;
    end;

    { Create GlobalId only when WantsGlobalId.
      IFC spec allows it only on TIfcRoot, though IFC JSON spec encourages
      "broader use" and in practice it is used, and really necessary,
      on other classes too. }
    if (Ifc.GlobalId = '') and Ifc.WantsGlobalId then
      Ifc.GlobalId := RandomGuidIfc;
    { Update Refs for any object, not only TIfcRoot.
      So Ifc.GlobalId may be '' here. }
    Refs.Add(Ifc);

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

        { Do no serialize properties with "stored false",
          like TIfcProperty.Description. }
        if not IsStoredProp(Ifc, PropInfo) then
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
                if IfcObjProperty is TCastleStringList then
                  Result.Add(JsonKey, SerializeJsonStringList(TCastleStringList(IfcObjProperty)))
                else
                if IfcObjProperty is TIntegerList then
                  Result.Add(JsonKey, SerializeJsonIntegerList(TIntegerList(IfcObjProperty)))
                else
                if IfcObjProperty is TVector2List then
                  Result.Add(JsonKey, SerializeJsonVector2List(TVector2List(IfcObjProperty)))
                else
                if IfcObjProperty is TVector3List then
                  Result.Add(JsonKey, SerializeJsonVector3List(TVector3List(IfcObjProperty)))
                else
                if IfcObjProperty is TVector3IntegerList then
                  Result.Add(JsonKey, SerializeJsonVector3IntegerList(TVector3IntegerList(IfcObjProperty)))
                else
                if IfcObjProperty is TCastleVector3Persistent then
                  Result.Add(JsonKey, SerializeJsonVector3(TCastleVector3Persistent(IfcObjProperty)))
                else
                if IfcObjProperty is TCastleVector4Persistent then
                  Result.Add(JsonKey, SerializeJsonVector4(TCastleVector4Persistent(IfcObjProperty)))
                else
                if IfcObjProperty is TIfcPersistent then
                  Result.Add(JsonKey, SerializeJson(TIfcPersistent(IfcObjProperty), false))
                else
                if ClassNameOfList(IfcObjProperty.ClassName) then
                begin
                  // don't save empty lists, just for shorter output
                  if NonEmptyList(IfcObjProperty) then
                    Result.Add(JsonKey, SerializeJsonList(IfcObjProperty, false));
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
  FileData := IfcFile.CreateDataList;
  try
    Refs := TIfcPersistentList.Create(false);
    try
      Result := SerializeJson(IfcFile, false);

      { Serialize IfcFile.CreateDataList result.
        Note that we could also define "Data: TIfcPersistentList" at TIfcFile,
        to have simpler load / save code, and in fact we had this in the past.
        But then it was bad TIfcFile API: the Data should not be ever
        manipulated except during loading / saving, normal IFC operations
        should always work on TIfcFile.Project. }
      Result.Add('data', SerializeJsonList(FileData, true));
    finally FreeAndNil(Refs) end;
  finally FreeAndNil(FileData) end;
end;

{ convenience overloads that load/save from/to URL --------------------------- }

function IfcJsonLoad(const Url: String): TIfcFile;
var
  Stream: TStream;
  JsonParser: TJsonParser;
  Json: TJsonData;
begin
  Stream := Download(Url);
  try
    JsonParser := TJsonParser.Create(Stream, [joComments]);
    try
      Json := JsonParser.Parse;
      try
        Result := IfcJsonLoad(Json);
      finally FreeAndNil(Json) end;
    finally FreeAndNil(JsonParser) end;
  finally FreeAndNil(Stream) end;
end;

procedure IfcJsonSave(const IfcFile: TIfcFile; const Url: String);
var
  JsonObj: TJsonObject;
  JsonString: String;
  Stream: TStream;
begin
  Stream := UrlSaveStream(Url);
  try
    JsonObj := IfcJsonSave(IfcFile);
    try
      JsonString := JsonObj.FormatJSON;
      WriteStr(Stream, JsonString);
    finally FreeAndNil(JsonObj) end;
  finally FreeAndNil(Stream) end;
end;

{$endif read_implementation}
