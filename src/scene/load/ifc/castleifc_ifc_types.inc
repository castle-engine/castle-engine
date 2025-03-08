{%MainUnit castleifc.pas}
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

{ Types and classes related to the IFC, but not from IFC standard. }

{$ifdef read_interface_types}

  // forward declarations
  TIfcProject = class;
  TCastleIfcMapping = class; // < Resolved in castleifc_load_to_x3d.inc

  {$M+}
  { Abstract class that can be serialized to / deserialized from an IFC file.
    This is not part of the official IFC standard, it is added for CGE purposes.

    It is a TComponent, because all IFC classes are owned by a common owner.
    This way we avoid complicated memory management ("who is responsible
    for freeing who", or reference counting as we did for X3D nodes),
    and we can easily free all IFC classes by freeing the owner. }
  TIfcPersistent = class abstract(TComponent)
  private
    { Useful during parsing, indicates this is an unresolved reference
      and should be replaced by the actual object later, once we know all
      references.
      Once parsing finished, i.e. IfcLoadJson returned,
      no instance should have this non-empty. }
    UnresolvedRef: String;

    FGlobalId: String; // actually TGUID;
  protected
    { If @true, instances of this class will have @link(GlobalId)
      auto-assigned at saving, and they will be saved as part of the "data"
      list in IFC JSON files.

      Strictly following the spec, this should be only defined for TIfcRoot,
      because only IfcRoot has GlobalId. In reality, we need GlobalId at
      TIfcPersistent to load some files,
      and we sometimes need WantsGlobalId = @true to save some files. }
    function WantsGlobalId: Boolean; virtual;

    { Name for the IFC standard, used in IFC files.
      By default, this is just ClassName with prefix 'T' removed. }
    class function IfcClassName: String; virtual;
  published
    { Globally unique identifier for the object instance.

      Note: Not declared as TGUID, because TGUID cannot be published,
      but also we don't really need it: for our purposes, this identifier
      is just an arbitrary string.

      Note: IFC 4.3 specification puts this only at IfcRoot, not at every IFC class.
      But testcase https://github.com/buildingsmart-community/Community-Sample-Test-Files/blob/main/IFC%204.0.2.1%20(IFC%204)/ISO%20Spec%20archive/wall-with-opening-and-window.json
      (from IFC 4.0) shows this can be present at any IFC class,
      in particular TIfcGeometricRepresentationContext.

      In fact, TIfcGeometricRepresentationContext @italic(needs) to have GlobalId
      to be successfully saved (avoid recursively trying to serialize it)
      in some cases. We indicate such classes with @link(WantsGlobalId).

      IFC JSON documentation says this, which seems to (unfortunately it is not
      unambiguous enough...) allow this at any IFC class:
      @italic("Whereas the globalId attribute is only available to those elements
      that are descendent of the IfcRoot entity in the EXPRESS schema,
      we encourage a broader use of this globalId,
      to enable referencing between objects in a JSON file when useful."). }
    property GlobalId: String read FGlobalId write FGlobalId;
  end;
  {$M-}

  { List of TIfcPersistent instances. }
  TIfcPersistentList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPersistent>;

  TIfcPersistentClass = class of TIfcPersistent;

  { List of TIfcPersistent descendant classes. }
  TIfcPersistentClassList = class({$ifdef FPC}specialize{$endif} TList<TIfcPersistentClass>)
  public
    { Find class corresponding to the given IFC type.
      Compares with TIfcPersistent.IfcClassName.
      Returns @nil if not found. }
    function FindIfcType(const IfcType: String): TIfcPersistentClass;
  end;

  { IFC file representation.

    This class is not defined in the IFC standard, but it is consistent
    with how the IFC file is actually represented, in both STEP and JSON encodings.
    The file starts with some metadata (in STEP header, on JSON top-level object)
    and then follows the "data" which is TIfcProject instance. }
  TIfcFile = class(TIfcPersistent)
  strict private
    FProject: TIfcProject;
    FEncodingType: String;
    FVersion: String;
    FSchemaIdentifier: String;
    FOriginatingSystem: String;
    FPreprocessorVersion: String;
    FTimeStamp: String;
  private
    { Used during loading. }
    TemporaryData: TIfcPersistentList;

    { Create a list of all TIfcRoot instances inside @link(Project),
      and make sure they all have GlobalId set.
      This list should be saved into the IFC files (JSON, STEP) as the "data"
      list.

      Note that in JSON encoding, this list is in theory not necessary.
      We could just save @link(Project) as the only item that includes
      everything else.
      But https://github.com/buildingsmart-community/ifcJSON
      converters cannot handle such files, they require everything on
      "data", and the "data" objects cannot be references. }
    function CreateDataList: TIfcPersistentList;

    { Return one and only TIfcProject instance from Data.
      @raises(EInvalidIfc If there is no TIfcProject instance in Data
        or there's more than one TIfcProject instance in Data.) }
    class function ProjectFromDataList(const Data: TIfcPersistentList): TIfcProject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BuildNode(const Mapping: TCastleIfcMapping): TX3DRootNode;

    { Project, always exactly one TIfcProject instance inside IFC file.
      May be temporarily @nil when building a new IFC file,
      but all valid IFC files should have this assigned. }
    property Project: TIfcProject read FProject write FProject;
  published
    property EncodingType: String read FEncodingType write FEncodingType;
    property Version: String read FVersion write FVersion;
    property SchemaIdentifier: String read FSchemaIdentifier write FSchemaIdentifier;
    property OriginatingSystem: String read FOriginatingSystem write FOriginatingSystem;
    property PreprocessorVersion: String read FPreprocessorVersion write FPreprocessorVersion;
    // TODO: If ever necessary, convert this to TDateTime
    property TimeStamp: String read FTimeStamp write FTimeStamp;
  end;

  { Invalid IFC file exception.
    Not all IFC extensions descend from this, e.g. we may also generate EJson
    exception or stream-reading exception. }
  EInvalidIfc = class(Exception);

  { Like TCastleVector3Persistent, but it just contains TVector3,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleVector3PersistentSimple = class(TCastleVector3Persistent)
  strict private
    SimpleValue: TVector3;
    function GetValueSimple: TVector3;
    procedure SetValueSimple(const AValue: TVector3);
  public
    constructor Create(const AOwner: TComponent = nil);
  end;

  { Like TCastleVector4Persistent, but it just contains TVector4,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleVector4PersistentSimple = class(TCastleVector4Persistent)
  strict private
    SimpleValue: TVector4;
    function GetValueSimple: TVector4;
    procedure SetValueSimple(const AValue: TVector4);
  public
    constructor Create(const AOwner: TComponent = nil);
  end;

{$endif read_interface_type}

{$ifdef read_implementation}

function ClassNameOfList(const PotentialListClassName: String): Boolean; forward;
function RandomGuidIfc: String; forward;

{ TIfcPersistent ------------------------------------------------------------- }

function TIfcPersistent.WantsGlobalId: Boolean;
begin
  Result := false;
end;

class function TIfcPersistent.IfcClassName: String;
begin
  // Result := PrefixRemove('T', Ifc.ClassName, false)
  // Slightly optimized version:
  if SCharIs(ClassName, 1, ['T', 't']) then
    Result := SEnding(ClassName, 2)
  else
    Result := ClassName;
  Result := SuffixRemove('_Boxed', Result, true);
  Result := SuffixRemove('_Spec', Result, true);
end;

{ TIfcPersistentList --------------------------------------------------------- }

function TIfcPersistentClassList.FindIfcType(const IfcType: String): TIfcPersistentClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    { All IFC files we've seen treat IFC class names as case-sensitive,
      so we can compare directly, case-sensitive. }
    if Items[I].IfcClassName = IfcType then
    begin
      Result := Items[I];
      Exit;
    end;
  Result := nil;
end;

{ TIfcFile ------------------------------------------------------------------- }

constructor TIfcFile.Create(AOwner: TComponent);
begin
  inherited;

  // reasonable defaults
  EncodingType := 'IFC.JSON'; // we can only save IFC JSON now
  Version := '0.0.1';
  SchemaIdentifier := 'IFC4';
  OriginatingSystem := 'Castle Game Engine ' + CastleEngineVersion;
end;

destructor TIfcFile.Destroy;
begin
  inherited;
end;

class function TIfcFile.ProjectFromDataList(const Data: TIfcPersistentList): TIfcProject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Data.Count - 1 do
  begin
    if Data[I] is TIfcProject then
    begin
      if Result <> nil then
        raise EInvalidIfc.Create('IFC file contains more than one TIfcProject');
      Result := TIfcProject(Data[I]);
    end;
  end;
  if Result = nil then
    raise EInvalidIfc.Create('IFC file does not contain any TIfcProject');
end;

{$ifdef FPC}
  {$push}
  // Disable object check for our TIfcPersistentList cast hack
  {$objectchecks off}
{$endif}

function TIfcFile.CreateDataList: TIfcPersistentList;
var
  Visited: TIfcPersistentList;

  { Recursive subroutine of CreateDataList. }
  procedure CreateDataListCore(const Ifc: TIfcPersistent);
  var
    PropInfos: TPropInfoList;
    PropInfo: PPropInfo;
    IfcObjProperty: TObject;
    IfcObjPropertyPersistent: TIfcPersistent;
    List: TIfcPersistentList;
    I, J: Integer;
  begin
    if Visited.Contains(Ifc) then
      Exit; // already visited, exit early to avoid cycles
    Visited.Add(Ifc);

    // Add Ifc to Result, if it's not there yet
    if Ifc.WantsGlobalId then
    begin
      { All objects on the "data" list need GlobalId, as we will use it
        to reference them from other places during JSON saving. }
      if Ifc.GlobalId = '' then
        Ifc.GlobalId := RandomGuidIfc;
      Result.Add(Ifc);
    end;

    // recursively call UpdateDataCore for all referenced TIfcPersistent instances
    PropInfos := TPropInfoList.Create(Ifc, tkProperties);
    try
      for I := 0 to PropInfos.Count - 1 do
      begin
        PropInfo := PropInfos.Items[I];
        if PropertyType(PropInfo) = ptInstance then
        begin
          IfcObjProperty := PropertyGetInstance(Ifc, PropInfo);
          if (IfcObjProperty <> nil) and
            ClassNameOfList(IfcObjProperty.ClassName) then
          begin
            { List of instances. Handle each of them, recursively. }
            List := TIfcPersistentList(IfcObjProperty);
            for J := 0 to List.Count - 1 do
              CreateDataListCore(List[J]);
          end else
          if IfcObjProperty is TIfcPersistent then
          begin
            { Single instance. Handle it, recursively. }
            IfcObjPropertyPersistent := TIfcPersistent(IfcObjProperty);
            CreateDataListCore(IfcObjPropertyPersistent);
          end;
        end;
      end;
    finally FreeAndNil(PropInfos) end;
  end;

begin
  Result := TIfcPersistentList.Create(false);

  Visited := TIfcPersistentList.Create(false);
  try
    CreateDataListCore(Project);
  finally FreeAndNil(Visited) end;
end;

{$ifdef FPC}
  {$pop}
{$endif}

{ TCastleVector3PersistentSimple --------------------------------------------- }

constructor TCastleVector3PersistentSimple.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  InternalGetValue := {$ifdef FPC}@{$endif} GetValueSimple;
  InternalSetValue := {$ifdef FPC}@{$endif} SetValueSimple;
end;

function TCastleVector3PersistentSimple.GetValueSimple: TVector3;
begin
  Result := SimpleValue;
end;

procedure TCastleVector3PersistentSimple.SetValueSimple(const AValue: TVector3);
begin
  SimpleValue := AValue;
end;

{ TCastleVector4PersistentSimple --------------------------------------------- }

constructor TCastleVector4PersistentSimple.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  InternalGetValue := {$ifdef FPC}@{$endif} GetValueSimple;
  InternalSetValue := {$ifdef FPC}@{$endif} SetValueSimple;
end;

function TCastleVector4PersistentSimple.GetValueSimple: TVector4;
begin
  Result := SimpleValue;
end;

procedure TCastleVector4PersistentSimple.SetValueSimple(const AValue: TVector4);
begin
  SimpleValue := AValue;
end;

{$endif read_implementation}
