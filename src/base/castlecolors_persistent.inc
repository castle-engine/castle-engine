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

{ CastleColors types wrapped as a TPersistent instance. }

{$ifdef read_interface}

type
  { TCastleColorRGB record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer) and to serialize it.

    Normal user code does not need to deal with this class or it's instances.
    Instead directly operate on TCastleColorRGB values,
    which is faster and more natural.
    TCastleColorRGB are simple records, so they are naturally copied on assignment,
    and various operators like additions work in a natural way too.
  }
  TCastleColorRGBPersistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TCastleColorRGB;
    HasScheduledSetValue: Boolean;
    function GetValue: TCastleColorRGB;
    function GetRed: Single;
    function GetGreen: Single;
    function GetBlue: Single;
    procedure SetValue(const AValue: TCastleColorRGB);
    procedure SetRed(const AValue: Single);
    procedure SetGreen(const AValue: Single);
    procedure SetBlue(const AValue: Single);
    function RedIsStored: Boolean;
    function GreenIsStored: Boolean;
    function BlueIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PCastleColorRGB;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    InternalDefaultValue: TCastleColorRGB;
    constructor Create(const AOwner: TComponent = nil); reintroduce;
    property Value: TCastleColorRGB read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property Red: Single read GetRed write SetRed stored RedIsStored nodefault;
    property Green: Single read GetGreen write SetGreen stored GreenIsStored nodefault;
    property Blue: Single read GetBlue write SetBlue stored BlueIsStored nodefault;
  end;

  { TCastleColor record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer) and to serialize it.

    Normal user code does not need to deal with this class or it's instances.
    Instead directly operate on TCastleColor values,
    which is faster and more natural.
    TCastleColor are simple records, so they are naturally copied on assignment,
    and various operators like additions work in a natural way too.
  }
  TCastleColorPersistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TCastleColor;
    HasScheduledSetValue: Boolean;
    function GetValue: TCastleColor;
    function GetRed: Single;
    function GetGreen: Single;
    function GetBlue: Single;
    function GetAlpha: Single;
    procedure SetValue(const AValue: TCastleColor);
    procedure SetRed(const AValue: Single);
    procedure SetGreen(const AValue: Single);
    procedure SetBlue(const AValue: Single);
    procedure SetAlpha(const AValue: Single);
    function RedIsStored: Boolean;
    function GreenIsStored: Boolean;
    function BlueIsStored: Boolean;
    function AlphaIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PCastleColor;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    InternalDefaultValue: TCastleColor;
    constructor Create(const AOwner: TComponent = nil); reintroduce;
    property Value: TCastleColor read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property Red: Single read GetRed write SetRed stored RedIsStored nodefault;
    property Green: Single read GetGreen write SetGreen stored GreenIsStored nodefault;
    property Blue: Single read GetBlue write SetBlue stored BlueIsStored nodefault;
    property Alpha: Single read GetAlpha write SetAlpha stored AlphaIsStored nodefault;
  end;

  { Helper methods you can use from TCastleComponent.CustomSerialization
    to manage reading/writing of colors. }
  TSerializationProcessColorsHelper = class helper(TSerializationProcessVectorsHelper) for TSerializationProcess
    { Serialize and deserialize given color Value.

      When deserializing, we always try to read it from file.
      If it is not present, the Value is not modified.
      If it is present in the file, but not all components,
      then the not present componens are not modified (they are @italic(not)
      reset to DefaultValue! The DefaultValue is only used at writing).

      When serializing, we write it to file only if IsStored.
      Generally IsStored=false should indicate "the Value is the same as when the object
      is created, thus there's no point in serializing it".
      Moreover, each color component (Value.X, Value.Y...)
      is written only if it is different than corresponding DefaultValue
      color component (DefaultValue.X, DefaultValue.Y...).

      @groupBegin }
    procedure ReadWriteColor(const Key: String; var Value: TCastleColor;
      const DefaultValue: TCastleColor; const IsStored: Boolean); overload;
    procedure ReadWriteColor(const Key: String; var Value: TCastleColorRGB;
      const DefaultValue: TCastleColorRGB; const IsStored: Boolean); overload;
    { @groupEnd }
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleColorRGBPersistent ---------------------------------------------------- }

constructor TCastleColorRGBPersistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { See TCastleVector2Persistent.Create comments about SetSubComponent call. }
  SetSubComponent(true);
end;

function TCastleColorRGBPersistent.GetValue: TCastleColorRGB;
begin
  Result := InternalGetValue();
end;

procedure TCastleColorRGBPersistent.SetValue(const AValue: TCastleColorRGB);
begin
  InternalSetValue(AValue);
end;

function TCastleColorRGBPersistent.GetRed: Single;
begin
  Result := Value.X;
end;

function TCastleColorRGBPersistent.GetGreen: Single;
begin
  Result := Value.Y;
end;

function TCastleColorRGBPersistent.GetBlue: Single;
begin
  Result := Value.Z;
end;

procedure TCastleColorRGBPersistent.SetRed(const AValue: Single);
var
  V: TCastleColorRGB;
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

procedure TCastleColorRGBPersistent.SetGreen(const AValue: Single);
var
  V: TCastleColorRGB;
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

procedure TCastleColorRGBPersistent.SetBlue(const AValue: Single);
var
  V: TCastleColorRGB;
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

function TCastleColorRGBPersistent.RedIsStored: Boolean;
begin
  Result := Value.X <> InternalDefaultValue.X;
end;

function TCastleColorRGBPersistent.GreenIsStored: Boolean;
begin
  Result := Value.Y <> InternalDefaultValue.Y;
end;

function TCastleColorRGBPersistent.BlueIsStored: Boolean;
begin
  Result := Value.Z <> InternalDefaultValue.Z;
end;

function TCastleColorRGBPersistent.ValueIsStreamed: Boolean;
begin
  Result := not TVector3.PerfectlyEquals(Value, InternalDefaultValue);
end;

function TCastleColorRGBPersistent.InitScheduledSetValue: PCastleColorRGB;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TCastleColorRGBPersistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TCastleColorRGBPersistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Red') or
     (PropertyName = 'Green') or
     (PropertyName = 'Blue') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleColorRGBPersistent.ToString: String;
begin
  Result := Value.ToString;
end;

{ TCastleColorPersistent ---------------------------------------------------- }

constructor TCastleColorPersistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { See TCastleVector2Persistent.Create comments about SetSubComponent call. }
  SetSubComponent(true);
end;

function TCastleColorPersistent.GetValue: TCastleColor;
begin
  Result := InternalGetValue();
end;

procedure TCastleColorPersistent.SetValue(const AValue: TCastleColor);
begin
  InternalSetValue(AValue);
end;

function TCastleColorPersistent.GetRed: Single;
begin
  Result := Value.X;
end;

function TCastleColorPersistent.GetGreen: Single;
begin
  Result := Value.Y;
end;

function TCastleColorPersistent.GetBlue: Single;
begin
  Result := Value.Z;
end;

function TCastleColorPersistent.GetAlpha: Single;
begin
  Result := Value.W;
end;

procedure TCastleColorPersistent.SetRed(const AValue: Single);
var
  V: TCastleColor;
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

procedure TCastleColorPersistent.SetGreen(const AValue: Single);
var
  V: TCastleColor;
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

procedure TCastleColorPersistent.SetBlue(const AValue: Single);
var
  V: TCastleColor;
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

procedure TCastleColorPersistent.SetAlpha(const AValue: Single);
var
  V: TCastleColor;
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

function TCastleColorPersistent.RedIsStored: Boolean;
begin
  Result := Value.X <> InternalDefaultValue.X;
end;

function TCastleColorPersistent.GreenIsStored: Boolean;
begin
  Result := Value.Y <> InternalDefaultValue.Y;
end;

function TCastleColorPersistent.BlueIsStored: Boolean;
begin
  Result := Value.Z <> InternalDefaultValue.Z;
end;

function TCastleColorPersistent.AlphaIsStored: Boolean;
begin
  Result := Value.W <> InternalDefaultValue.W;
end;

function TCastleColorPersistent.ValueIsStreamed: Boolean;
begin
  Result := not TVector4.PerfectlyEquals(Value, InternalDefaultValue);
end;

function TCastleColorPersistent.InitScheduledSetValue: PCastleColor;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TCastleColorPersistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TCastleColorPersistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Red') or
     (PropertyName = 'Green') or
     (PropertyName = 'Blue') or
     (PropertyName = 'Alpha') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleColorPersistent.ToString: String;
begin
  Result := Value.ToString;
end;

{ TCastleColorPersistentSimple -------------------------------------------- }

type
  { Like TCastleColorPersistent, but it just contains TCastleColor,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleColorPersistentSimple = class(TCastleColorPersistent)
  strict private
    function GetValueSimple: TCastleColor;
    procedure SetValueSimple(const AValue: TCastleColor);
  public
    SimpleValue: TCastleColor;
    constructor Create(const AOwner: TComponent = nil);
  end;

constructor TCastleColorPersistentSimple.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  InternalGetValue := {$ifdef FPC}@{$endif} GetValueSimple;
  InternalSetValue := {$ifdef FPC}@{$endif} SetValueSimple;
end;

function TCastleColorPersistentSimple.GetValueSimple: TCastleColor;
begin
  Result := SimpleValue;
end;

procedure TCastleColorPersistentSimple.SetValueSimple(const AValue: TCastleColor);
begin
  SimpleValue := AValue;
end;

{ TCastleColorRGBPersistentSimple -------------------------------------------- }

type
  { Like TCastleColorRGBPersistent, but it just contains TCastleColorRGB,
    you don't need to provide InternalGetValue, InternalSetValue callbacks. }
  TCastleColorRGBPersistentSimple = class(TCastleColorRGBPersistent)
  strict private
    function GetValueSimple: TCastleColorRGB;
    procedure SetValueSimple(const AValue: TCastleColorRGB);
  public
    SimpleValue: TCastleColorRGB;
    constructor Create(const AOwner: TComponent = nil);
  end;

constructor TCastleColorRGBPersistentSimple.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  InternalGetValue := {$ifdef FPC}@{$endif} GetValueSimple;
  InternalSetValue := {$ifdef FPC}@{$endif} SetValueSimple;
end;

function TCastleColorRGBPersistentSimple.GetValueSimple: TCastleColorRGB;
begin
  Result := SimpleValue;
end;

procedure TCastleColorRGBPersistentSimple.SetValueSimple(const AValue: TCastleColorRGB);
begin
  SimpleValue := AValue;
end;

{ TSerializationProcessColorsHelper ----------------------------------------- }

procedure TSerializationProcessColorsHelper.ReadWriteColor(
  const Key: String; var Value: TCastleColor; const DefaultValue: TCastleColor; const IsStored: Boolean);
var
  Persistent: TCastleColorPersistentSimple;
begin
  Persistent := TCastleColorPersistentSimple.Create;
  try
    Persistent.SimpleValue := Value;
    Persistent.InternalDefaultValue := DefaultValue;
    ReadWriteSubComponent(Key, Persistent, IsStored);
    Value := Persistent.SimpleValue; // read back, in case ReadWriteSubComponent read it from file
  finally FreeAndNil(Persistent) end;
end;

procedure TSerializationProcessColorsHelper.ReadWriteColor(
  const Key: String; var Value: TCastleColorRGB; const DefaultValue: TCastleColorRGB; const IsStored: Boolean);
var
  Persistent: TCastleColorRGBPersistentSimple;
begin
  Persistent := TCastleColorRGBPersistentSimple.Create;
  try
    Persistent.SimpleValue := Value;
    Persistent.InternalDefaultValue := DefaultValue;
    ReadWriteSubComponent(Key, Persistent, IsStored);
    Value := Persistent.SimpleValue; // read back, in case ReadWriteSubComponent read it from file
  finally FreeAndNil(Persistent) end;
end;

{$endif read_implementation}
