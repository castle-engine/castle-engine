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

{ CastleVectors types wrapped as a TPersistent instance. }

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
  TCastleVector2Persistent = class(TPersistent)
  strict private
    function GetValue: TVector2;
    function GetX: Single;
    function GetY: Single;
    procedure SetValue(const AValue: TVector2);
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    function XIsStored: Boolean;
    function YIsStored: Boolean;
  public
    InternalGetValue: TGetVector2Event;
    InternalSetValue: TSetVector2Event;
    InternalDefaultValue: TVector2;
    property Value: TVector2 read GetValue write SetValue;
    function HasDefaultValue: Boolean;
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
  TCastleVector3Persistent = class(TPersistent)
  strict private
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
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    InternalDefaultValue: TVector3;
    property Value: TVector3 read GetValue write SetValue;
    function HasDefaultValue: Boolean;
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
  TCastleVector4Persistent = class(TPersistent)
  strict private
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
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    InternalDefaultValue: TVector4;
    property Value: TVector4 read GetValue write SetValue;
    function HasDefaultValue: Boolean;
  published
    property X: Single read GetX write SetX stored XIsStored nodefault;
    property Y: Single read GetY write SetY stored YIsStored nodefault;
    property Z: Single read GetZ write SetZ stored ZIsStored nodefault;
    property W: Single read GetW write SetW stored WIsStored nodefault;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleVector2Persistent ---------------------------------------------------- }

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
  Result := Value.Data[0];
end;

function TCastleVector2Persistent.GetY: Single;
begin
  Result := Value.Data[1];
end;

procedure TCastleVector2Persistent.SetX(const AValue: Single);
var
  V: TVector2;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleVector2Persistent.SetY(const AValue: Single);
var
  V: TVector2;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

function TCastleVector2Persistent.XIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleVector2Persistent.YIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleVector2Persistent.HasDefaultValue: Boolean;
begin
  Result := TVector2.PerfectlyEquals(Value, InternalDefaultValue);
end;

{ TCastleVector3Persistent ---------------------------------------------------- }

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
  Result := Value.Data[0];
end;

function TCastleVector3Persistent.GetY: Single;
begin
  Result := Value.Data[1];
end;

function TCastleVector3Persistent.GetZ: Single;
begin
  Result := Value.Data[2];
end;

procedure TCastleVector3Persistent.SetX(const AValue: Single);
var
  V: TVector3;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleVector3Persistent.SetY(const AValue: Single);
var
  V: TVector3;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleVector3Persistent.SetZ(const AValue: Single);
var
  V: TVector3;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

function TCastleVector3Persistent.XIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleVector3Persistent.YIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleVector3Persistent.ZIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

function TCastleVector3Persistent.HasDefaultValue: Boolean;
begin
  Result := TVector3.PerfectlyEquals(Value, InternalDefaultValue);
end;

{ TCastleVector4Persistent ---------------------------------------------------- }

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
  Result := Value.Data[0];
end;

function TCastleVector4Persistent.GetY: Single;
begin
  Result := Value.Data[1];
end;

function TCastleVector4Persistent.GetZ: Single;
begin
  Result := Value.Data[2];
end;

function TCastleVector4Persistent.GetW: Single;
begin
  Result := Value.Data[3];
end;

procedure TCastleVector4Persistent.SetX(const AValue: Single);
var
  V: TVector4;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleVector4Persistent.SetY(const AValue: Single);
var
  V: TVector4;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleVector4Persistent.SetZ(const AValue: Single);
var
  V: TVector4;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

procedure TCastleVector4Persistent.SetW(const AValue: Single);
var
  V: TVector4;
begin
  V := Value;
  V.Data[3] := AValue;
  Value := V;
end;

function TCastleVector4Persistent.XIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleVector4Persistent.YIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleVector4Persistent.ZIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

function TCastleVector4Persistent.WIsStored: Boolean;
begin
  Result := Value.Data[3] <> InternalDefaultValue.Data[3];
end;

function TCastleVector4Persistent.HasDefaultValue: Boolean;
begin
  Result := TVector4.PerfectlyEquals(Value, InternalDefaultValue);
end;

{$endif read_implementation}
