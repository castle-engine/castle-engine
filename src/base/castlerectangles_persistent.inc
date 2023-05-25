{%MainUnit castlerectangles.pas}
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

{ TFloatRectanglePersistent type wrapped as a TCastleComponent instance. }

{$ifdef read_interface}

type
  TGetFloatRectangleEvent = function: TFloatRectangle of object;
  TSetFloatRectangleEvent = procedure (const Value: TFloatRectangle) of object;

  TFloatRectanglePersistent = class(TCastleComponent)
  strict private
    { During IsLoading, Set* only modifies ScheduledSetValue,
      and then in Loaded we send whole vector by one InternalSetValue call.
      This decreases the number of calls to the vector setters during deserialization.
      Also it means that we send the vector as a whole, which is good if the setter
      is doing some processing (e.g. normalizing the value). }
    ScheduledSetValue: TFloatRectangle;
    HasScheduledSetValue: Boolean;
    function GetValue: TFloatRectangle;
    function GetLeft: Single;
    function GetBottom: Single;
    function GetWidth: Single;
    function GetHeight: Single;
    procedure SetValue(const AValue: TFloatRectangle);
    procedure SetLeft(const AValue: Single);
    procedure SetBottom(const AValue: Single);
    procedure SetWidth(const AValue: Single);
    procedure SetHeight(const AValue: Single);
    function LeftIsStored: Boolean;
    function BottomIsStored: Boolean;
    function WidthIsStored: Boolean;
    function HeightIsStored: Boolean;
    { Make sure HasScheduledSetValue is true, ScheduledSetValue is valid,
      and return @ScheduledSetValue. }
    function InitScheduledSetValue: PFloatRectangle;
  protected
    procedure Loaded; override;
  public
    InternalGetValue: TGetFloatRectangleEvent;
    InternalSetValue: TSetFloatRectangleEvent;
    InternalDefaultValue: TFloatRectangle;
    property Value: TFloatRectangle read GetValue write SetValue;
    function ValueIsStreamed: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function ToString: String; override;
  published
    property Left: Single read GetLeft write SetLeft stored LeftIsStored nodefault;
    property Bottom: Single read GetBottom write SetBottom stored BottomIsStored nodefault;
    property Width: Single read GetWidth write SetWidth stored WidthIsStored nodefault;
    property Height: Single read GetHeight write SetHeight stored HeightIsStored nodefault;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TFloatRectanglePersistent ---------------------------------------------------- }

(*
Note that analogous TCastleVector2Persistent.Create constructor
1. is reintroduced, to have AOwner=nil by default
2. calls SetSubComponent(true) on itself.
Both 1 and 2 are somewhat dirty, and done only for backward compatibility.
For new TFloatRectanglePersistent we don't do this.

constructor TFloatRectanglePersistent.Create(const AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  { See TCastleVector2Persistent.Create comments about SetSubComponent call. }
  SetSubComponent(true);
end;
*)

function TFloatRectanglePersistent.GetValue: TFloatRectangle;
begin
  Result := InternalGetValue();
end;

procedure TFloatRectanglePersistent.SetValue(const AValue: TFloatRectangle);
begin
  InternalSetValue(AValue);
end;

function TFloatRectanglePersistent.GetLeft: Single;
begin
  Result := Value.Left;
end;

function TFloatRectanglePersistent.GetBottom: Single;
begin
  Result := Value.Bottom;
end;

function TFloatRectanglePersistent.GetWidth: Single;
begin
  Result := Value.Width;
end;

function TFloatRectanglePersistent.GetHeight: Single;
begin
  Result := Value.Height;
end;

procedure TFloatRectanglePersistent.SetLeft(const AValue: Single);
var
  V: TFloatRectangle;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Left := AValue;
  end else
  begin
    V := Value;
    V.Left := AValue;
    Value := V;
  end;
end;

procedure TFloatRectanglePersistent.SetBottom(const AValue: Single);
var
  V: TFloatRectangle;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Bottom := AValue;
  end else
  begin
    V := Value;
    V.Bottom := AValue;
    Value := V;
  end;
end;

procedure TFloatRectanglePersistent.SetWidth(const AValue: Single);
var
  V: TFloatRectangle;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Width := AValue;
  end else
  begin
    V := Value;
    V.Width := AValue;
    Value := V;
  end;
end;

procedure TFloatRectanglePersistent.SetHeight(const AValue: Single);
var
  V: TFloatRectangle;
begin
  if IsLoading then
  begin
    InitScheduledSetValue^.Height := AValue;
  end else
  begin
    V := Value;
    V.Height := AValue;
    Value := V;
  end;
end;

function TFloatRectanglePersistent.LeftIsStored: Boolean;
begin
  Result := Value.Left <> InternalDefaultValue.Left;
end;

function TFloatRectanglePersistent.BottomIsStored: Boolean;
begin
  Result := Value.Bottom <> InternalDefaultValue.Bottom;
end;

function TFloatRectanglePersistent.WidthIsStored: Boolean;
begin
  Result := Value.Width <> InternalDefaultValue.Width;
end;

function TFloatRectanglePersistent.HeightIsStored: Boolean;
begin
  Result := Value.Height <> InternalDefaultValue.Height;
end;

function TFloatRectanglePersistent.InitScheduledSetValue: PFloatRectangle;
begin
  if not HasScheduledSetValue then
  begin
    HasScheduledSetValue := true;
    ScheduledSetValue := Value;
  end;
  Result := @ScheduledSetValue;
end;

procedure TFloatRectanglePersistent.Loaded;
begin
  inherited;
  if HasScheduledSetValue then
  begin
    HasScheduledSetValue := false;
    Value := ScheduledSetValue;
  end;
end;

function TFloatRectanglePersistent.ValueIsStreamed: Boolean;
begin
  Result := not Value.PerfectlyEquals(InternalDefaultValue);
end;

function TFloatRectanglePersistent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Left') or
     (PropertyName = 'Bottom') or
     (PropertyName = 'Width') or
     (PropertyName = 'Height') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TFloatRectanglePersistent.ToString: String;
begin
  Result := Value.ToString;
end;

{$endif read_implementation}
