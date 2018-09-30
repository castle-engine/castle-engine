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
  TCastleColorRGBPersistent = class(TPersistent)
  strict private
    function GetValue: TCastleColorRGB;
    function GetR: Single;
    function GetG: Single;
    function GetB: Single;
    procedure SetValue(const AValue: TCastleColorRGB);
    procedure SetR(const AValue: Single);
    procedure SetG(const AValue: Single);
    procedure SetB(const AValue: Single);
    function RIsStored: Boolean;
    function GIsStored: Boolean;
    function BIsStored: Boolean;
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    InternalDefaultValue: TCastleColorRGB;
    property Value: TCastleColorRGB read GetValue write SetValue;
  published
    { Red color component. }
    property R: Single read GetR write SetR stored RIsStored nodefault;
    { Green color component. }
    property G: Single read GetG write SetG stored GIsStored nodefault;
    { Blue color component. }
    property B: Single read GetB write SetB stored BIsStored nodefault;
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
  TCastleColorPersistent = class(TPersistent)
  strict private
    function GetValue: TCastleColor;
    function GetR: Single;
    function GetG: Single;
    function GetB: Single;
    function GetA: Single;
    procedure SetValue(const AValue: TCastleColor);
    procedure SetR(const AValue: Single);
    procedure SetG(const AValue: Single);
    procedure SetB(const AValue: Single);
    procedure SetA(const AValue: Single);
    function RIsStored: Boolean;
    function GIsStored: Boolean;
    function BIsStored: Boolean;
    function AIsStored: Boolean;
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    InternalDefaultValue: TCastleColor;
    property Value: TCastleColor read GetValue write SetValue;
  published
    { Red color component. }
    property R: Single read GetR write SetR stored RIsStored nodefault;
    { Green color component. }
    property G: Single read GetG write SetG stored GIsStored nodefault;
    { Blue color component. }
    property B: Single read GetB write SetB stored BIsStored nodefault;
    { Alpha (opacity) color component. }
    property A: Single read GetA write SetA stored AIsStored nodefault;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleColorRGBPersistent ---------------------------------------------------- }

function TCastleColorRGBPersistent.GetValue: TCastleColorRGB;
begin
  Result := InternalGetValue();
end;

procedure TCastleColorRGBPersistent.SetValue(const AValue: TCastleColorRGB);
begin
  InternalSetValue(AValue);
end;

function TCastleColorRGBPersistent.GetR: Single;
begin
  Result := Value.Data[0];
end;

function TCastleColorRGBPersistent.GetG: Single;
begin
  Result := Value.Data[1];
end;

function TCastleColorRGBPersistent.GetB: Single;
begin
  Result := Value.Data[2];
end;

procedure TCastleColorRGBPersistent.SetR(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleColorRGBPersistent.SetG(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleColorRGBPersistent.SetB(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

function TCastleColorRGBPersistent.RIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleColorRGBPersistent.GIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleColorRGBPersistent.BIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

{ TCastleColorPersistent ---------------------------------------------------- }

function TCastleColorPersistent.GetValue: TCastleColor;
begin
  Result := InternalGetValue();
end;

procedure TCastleColorPersistent.SetValue(const AValue: TCastleColor);
begin
  InternalSetValue(AValue);
end;

function TCastleColorPersistent.GetR: Single;
begin
  Result := Value.Data[0];
end;

function TCastleColorPersistent.GetG: Single;
begin
  Result := Value.Data[1];
end;

function TCastleColorPersistent.GetB: Single;
begin
  Result := Value.Data[2];
end;

function TCastleColorPersistent.GetA: Single;
begin
  Result := Value.Data[3];
end;

procedure TCastleColorPersistent.SetR(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetG(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetB(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetA(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[3] := AValue;
  Value := V;
end;

function TCastleColorPersistent.RIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleColorPersistent.GIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleColorPersistent.BIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

function TCastleColorPersistent.AIsStored: Boolean;
begin
  Result := Value.Data[3] <> InternalDefaultValue.Data[3];
end;

{$endif read_implementation}
