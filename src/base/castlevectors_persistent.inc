{%MainUnit castlevectors.pas}
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

{ CastleVectors types wrapped as a TCastleComponent instance. }

{$ifdef read_interface}

type
  TGetVector2Event = function: TVector2 of object;
  TSetVector2Event = procedure (const Value: TVector2) of object;
  TGetVector3Event = function: TVector3 of object;
  TSetVector3Event = procedure (const Value: TVector3) of object;
  TGetVector4Event = function: TVector4 of object;
  TSetVector4Event = procedure (const Value: TVector4) of object;

  { TVector2 record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer) and to serialize it.

    Normal user code does not need to deal with this class or it's instances.
    Instead directly operate on TVector2 values,
    which is faster and more natural.
    TVector2 are simple records, so they are naturally copied on assignment,
    and various operators like additions work in a natural way too.
  }
  TCastleVector2Persistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TVector2;
    HasScheduledSetValue: Boolean;
    function GetValue: TVector2;
    function GetX: Single;
    function GetY: Single;
    procedure SetValue(const AValue: TVector2);
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    function XIsStored: Boolean;
    function YIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PVector2;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetVector2Event;
    InternalSetValue: TSetVector2Event;
    { Default value, just like Pascal "default" property attribute,
      is only used to decide which vector components to write to file.

      It is your responsibility to make sure that this is really the default
      (right after creation) state of the object,
      and the deserialization will just @italic(not modify) components not specified
      (it will not reset them to InternalDefaultValue value). }
    InternalDefaultValue: TVector2;
    constructor Create(const AOwner: TComponent = nil); reintroduce;
    property Value: TVector2 read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property X: Single read GetX write SetX stored XIsStored nodefault;
    property Y: Single read GetY write SetY stored YIsStored nodefault;
  end;

  { TVector3 record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer) and to serialize it.

    Normal user code does not need to deal with this class or it's instances.
    Instead directly operate on TVector3 values,
    which is faster and more natural.
    TVector3 are simple records, so they are naturally copied on assignment,
    and various operators like additions work in a natural way too.
  }
  TCastleVector3Persistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TVector3;
    HasScheduledSetValue: Boolean;
    function GetValue: TVector3;
    function GetX: Single;
    function GetY: Single;
    function GetZ: Single;
    procedure SetValue(const AValue: TVector3);
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    procedure SetZ(const AValue: Single);
    function XIsStored: Boolean;
    function YIsStored: Boolean;
    function ZIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PVector3;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    InternalDefaultValue: TVector3;
    constructor Create(const AOwner: TComponent = nil); reintroduce;
    property Value: TVector3 read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property X: Single read GetX write SetX stored XIsStored nodefault;
    property Y: Single read GetY write SetY stored YIsStored nodefault;
    property Z: Single read GetZ write SetZ stored ZIsStored nodefault;
  end;

  { TVector4 record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer) and to serialize it.

    Normal user code does not need to deal with this class or it's instances.
    Instead directly operate on TVector4 values,
    which is faster and more natural.
    TVector4 are simple records, so they are naturally copied on assignment,
    and various operators like additions work in a natural way too.
  }
  TCastleVector4Persistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TVector4;
    HasScheduledSetValue: Boolean;
    function GetValue: TVector4;
    function GetX: Single;
    function GetY: Single;
    function GetZ: Single;
    function GetW: Single;
    procedure SetValue(const AValue: TVector4);
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    procedure SetZ(const AValue: Single);
    procedure SetW(const AValue: Single);
    function XIsStored: Boolean;
    function YIsStored: Boolean;
    function ZIsStored: Boolean;
    function WIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PVector4;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    InternalDefaultValue: TVector4;
    constructor Create(const AOwner: TComponent = nil); reintroduce;
    property Value: TVector4 read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property X: Single read GetX write SetX stored XIsStored nodefault;
    property Y: Single read GetY write SetY stored YIsStored nodefault;
    property Z: Single read GetZ write SetZ stored ZIsStored nodefault;
    property W: Single read GetW write SetW stored WIsStored nodefault;
  end;

  { TCastleVector4Persistent descendant for castle editor
    to display angle component nicer
    (as 'Angle (W)' and 'Deg(90)' instead of as 'W' and in radians). }
  TCastleVector4RotationPersistent = class(TCastleVector4Persistent)

  end;

  { Helper methods you can use from TCastleComponent.CustomSerialization
    to manage reading/writing of vectors. }
  TSerializationProcessVectorsHelper = class helper for TSerializationProcess
    { Serialize and deserialize given vector Value.

      When deserializing, we always try to read it from file.
      If it is not present, the Value is not modified.
      If it is present in the file, but not all components,
      then the not present componens are not modified (they are @italic(not)
      reset to DefaultValue! The DefaultValue is only used at writing).

      When serializing, we write it to file only if IsStored.
      Generally IsStored=false should indicate "the Value is the same as when the object
      is created, thus there's no point in serializing it".
      Moreover, each vector component (Value.X, Value.Y...)
      is written only if it is different than corresponding DefaultValue
      vector component (DefaultValue.X, DefaultValue.Y...).

      @groupBegin }
    procedure ReadWriteVector(const Key: String; var Value: TVector2;
      const DefaultValue: TVector2; const IsStored: Boolean); overload;
    procedure ReadWriteVector(const Key: String; var Value: TVector3;
      const DefaultValue: TVector3; const IsStored: Boolean); overload;
    procedure ReadWriteVector(const Key: String; var Value: TVector4;
      const DefaultValue: TVector4; const IsStored: Boolean); overload;
    { @groupEnd }
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleVector2Persistent ---------------------------------------------------- }

constructor TCastleVector2Persistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { Usually the class should not call on itself SetSubComponent,
    as being subcomponent is determined by the caller/owner.
    But in this case
    1. we know that this class should *only* be useful to help with serialization,
       as subcomponent
    2. we don't want to break compatibility in how this class is used,
       and users may have used these classes in own projects following our
       earlier template without SetSubComponent call. }
  SetSubComponent(true);
end;

function TCastleVector2Persistent.GetValue: TVector2;
begin
  Result := InternalGetValue();
end;

procedure TCastleVector2Persistent.SetValue(const AValue: TVector2);
begin
  InternalSetValue(AValue);
end;

function TCastleVector2Persistent.GetX: Single;
begin
  Result := Value.X;
end;

function TCastleVector2Persistent.GetY: Single;
begin
  Result := Value.Y;
end;

procedure TCastleVector2Persistent.SetX(const AValue: Single);
var
  V: TVector2;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.X := AValue;
  end else
  begin
    V := Value;
    V.X := AValue;
    Value := V;
  end;
end;

procedure TCastleVector2Persistent.SetY(const AValue: Single);
var
  V: TVector2;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Y := AValue;
  end else
  begin
    V := Value;
    V.Y := AValue;
    Value := V;
  end;
end;

function TCastleVector2Persistent.XIsStored: Boolean;
begin
  Result := Value.X <> InternalDefaultValue.X;
end;

function TCastleVector2Persistent.YIsStored: Boolean;
begin
  Result := Value.Y <> InternalDefaultValue.Y;
end;

function TCastleVector2Persistent.ValueIsStreamed: Boolean;
begin
  Result := not TVector2.PerfectlyEquals(Value, InternalDefaultValue);
end;

function TCastleVector2Persistent.InitScheduledSetValue: PVector2;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TCastleVector2Persistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TCastleVector2Persistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'X') or
     (PropertyName = 'Y') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleVector2Persistent.ToString: String;
begin
  Result := Value.ToString;
end;

{ TCastleVector3Persistent ---------------------------------------------------- }

constructor TCastleVector3Persistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { See TCastleVector2Persistent.Create comments about SetSubComponent call. }
  SetSubComponent(true);
end;

function TCastleVector3Persistent.GetValue: TVector3;
begin
  Result := InternalGetValue();
end;

procedure TCastleVector3Persistent.SetValue(const AValue: TVector3);
begin
  InternalSetValue(AValue);
end;

function TCastleVector3Persistent.GetX: Single;
begin
  Result := Value.X;
end;

function TCastleVector3Persistent.GetY: Single;
begin
  Result := Value.Y;
end;

function TCastleVector3Persistent.GetZ: Single;
begin
  Result := Value.Z;
end;

procedure TCastleVector3Persistent.SetX(const AValue: Single);
var
  V: TVector3;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.X := AValue;
  end else
  begin
    V := Value;
    V.X := AValue;
    Value := V;
  end;
end;

procedure TCastleVector3Persistent.SetY(const AValue: Single);
var
  V: TVector3;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Y := AValue;
  end else
  begin
    V := Value;
    V.Y := AValue;
    Value := V;
  end;
end;

procedure TCastleVector3Persistent.SetZ(const AValue: Single);
var
  V: TVector3;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Z := AValue;
  end else
  begin
    V := Value;
    V.Z := AValue;
    Value := V;
  end;
end;

function TCastleVector3Persistent.XIsStored: Boolean;
begin
  Result := Value.X <> InternalDefaultValue.X;
end;

function TCastleVector3Persistent.YIsStored: Boolean;
begin
  Result := Value.Y <> InternalDefaultValue.Y;
end;

function TCastleVector3Persistent.ZIsStored: Boolean;
begin
  Result := Value.Z <> InternalDefaultValue.Z;
end;

function TCastleVector3Persistent.ValueIsStreamed: Boolean;
begin
  Result := not TVector3.PerfectlyEquals(Value, InternalDefaultValue);
end;

function TCastleVector3Persistent.InitScheduledSetValue: PVector3;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TCastleVector3Persistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TCastleVector3Persistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'X') or
     (PropertyName = 'Y') or
     (PropertyName = 'Z') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleVector3Persistent.ToString: String;
begin
  Result := Value.ToString;
end;

{ TCastleVector4Persistent ---------------------------------------------------- }

constructor TCastleVector4Persistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { See TCastleVector2Persistent.Create comments about SetSubComponent call. }
  SetSubComponent(true);
end;

function TCastleVector4Persistent.GetValue: TVector4;
begin
  Result := InternalGetValue();
end;

procedure TCastleVector4Persistent.SetValue(const AValue: TVector4);
begin
  InternalSetValue(AValue);
end;

function TCastleVector4Persistent.GetX: Single;
begin
  Result := Value.X;
end;

function TCastleVector4Persistent.GetY: Single;
begin
  Result := Value.Y;
end;

function TCastleVector4Persistent.GetZ: Single;
begin
  Result := Value.Z;
end;

function TCastleVector4Persistent.GetW: Single;
begin
  Result := Value.W;
end;

procedure TCastleVector4Persistent.SetX(const AValue: Single);
var
  V: TVector4;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.X := AValue;
  end else
  begin
    V := Value;
    V.X := AValue;
    Value := V;
  end;
end;

procedure TCastleVector4Persistent.SetY(const AValue: Single);
var
  V: TVector4;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Y := AValue;
  end else
  begin
    V := Value;
    V.Y := AValue;
    Value := V;
  end;
end;

procedure TCastleVector4Persistent.SetZ(const AValue: Single);
var
  V: TVector4;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Z := AValue;
  end else
  begin
    V := Value;
    V.Z := AValue;
    Value := V;
  end;
end;

procedure TCastleVector4Persistent.SetW(const AValue: Single);
var
  V: TVector4;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.W := AValue;
  end else
  begin
    V := Value;
    V.W := AValue;
    Value := V;
  end;
end;

function TCastleVector4Persistent.XIsStored: Boolean;
begin
  Result := Value.X <> InternalDefaultValue.X;
end;

function TCastleVector4Persistent.YIsStored: Boolean;
begin
  Result := Value.Y <> InternalDefaultValue.Y;
end;

function TCastleVector4Persistent.ZIsStored: Boolean;
begin
  Result := Value.Z <> InternalDefaultValue.Z;
end;

function TCastleVector4Persistent.WIsStored: Boolean;
begin
  Result := Value.W <> InternalDefaultValue.W;
end;

function TCastleVector4Persistent.ValueIsStreamed: Boolean;
begin
  Result := not TVector4.PerfectlyEquals(Value, InternalDefaultValue);
end;

function TCastleVector4Persistent.InitScheduledSetValue: PVector4;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TCastleVector4Persistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TCastleVector4Persistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'X') or
     (PropertyName = 'Y') or
     (PropertyName = 'Z') or
     (PropertyName = 'W') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleVector4Persistent.ToString: String;
begin
  Result := Value.ToString;
end;

{ TCastleVector2PersistentSimple -------------------------------------------- }

type
  { Like TCastleVector2Persistent, but it just contains TVector2,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleVector2PersistentSimple = class(TCastleVector2Persistent)
  strict private
    function GetValueSimple: TVector2;
    procedure SetValueSimple(const AValue: TVector2);
  public
    SimpleValue: TVector2;
    constructor Create(const AOwner: TComponent = nil);
  end;

constructor TCastleVector2PersistentSimple.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  InternalGetValue := {$ifdef FPC}@{$endif} GetValueSimple;
  InternalSetValue := {$ifdef FPC}@{$endif} SetValueSimple;
end;

function TCastleVector2PersistentSimple.GetValueSimple: TVector2;
begin
  Result := SimpleValue;
end;

procedure TCastleVector2PersistentSimple.SetValueSimple(const AValue: TVector2);
begin
  SimpleValue := AValue;
end;

{ TCastleVector3PersistentSimple -------------------------------------------- }

type
  { Like TCastleVector3Persistent, but it just contains TVector3,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleVector3PersistentSimple = class(TCastleVector3Persistent)
  strict private
    function GetValueSimple: TVector3;
    procedure SetValueSimple(const AValue: TVector3);
  public
    SimpleValue: TVector3;
    constructor Create(const AOwner: TComponent = nil);
  end;

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

{ TCastleVector4PersistentSimple -------------------------------------------- }

type
  { Like TCastleVector4Persistent, but it just contains TVector4,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleVector4PersistentSimple = class(TCastleVector4Persistent)
  strict private
    function GetValueSimple: TVector4;
    procedure SetValueSimple(const AValue: TVector4);
  public
    SimpleValue: TVector4;
    constructor Create(const AOwner: TComponent = nil);
  end;

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

{ TSerializationProcessVectorsHelper ----------------------------------------- }

procedure TSerializationProcessVectorsHelper.ReadWriteVector(
  const Key: String; var Value: TVector2; const DefaultValue: TVector2; const IsStored: Boolean);
var
  Persistent: TCastleVector2PersistentSimple;
begin
  Persistent := TCastleVector2PersistentSimple.Create;
  try
    Persistent.SimpleValue := Value;
    Persistent.InternalDefaultValue := DefaultValue;
    ReadWriteSubComponent(Key, Persistent, IsStored);
    Value := Persistent.SimpleValue; // read back, in case ReadWriteSubComponent read it from file
  finally FreeAndNil(Persistent) end;
end;

procedure TSerializationProcessVectorsHelper.ReadWriteVector(
  const Key: String; var Value: TVector3; const DefaultValue: TVector3; const IsStored: Boolean);
var
  Persistent: TCastleVector3PersistentSimple;
begin
  Persistent := TCastleVector3PersistentSimple.Create;
  try
    Persistent.SimpleValue := Value;
    Persistent.InternalDefaultValue := DefaultValue;
    ReadWriteSubComponent(Key, Persistent, IsStored);
    Value := Persistent.SimpleValue; // read back, in case ReadWriteSubComponent read it from file
  finally FreeAndNil(Persistent) end;
end;

procedure TSerializationProcessVectorsHelper.ReadWriteVector(
  const Key: String; var Value: TVector4; const DefaultValue: TVector4; const IsStored: Boolean);
var
  Persistent: TCastleVector4PersistentSimple;
begin
  Persistent := TCastleVector4PersistentSimple.Create;
  try
    Persistent.SimpleValue := Value;
    Persistent.InternalDefaultValue := DefaultValue;
    ReadWriteSubComponent(Key, Persistent, IsStored);
    Value := Persistent.SimpleValue; // read back, in case ReadWriteSubComponent read it from file
  finally FreeAndNil(Persistent) end;
end;

{$endif read_implementation}
