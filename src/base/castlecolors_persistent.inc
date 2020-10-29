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
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    InternalDefaultValue: TCastleColorRGB;
    property Value: TCastleColorRGB read GetValue write SetValue;
    function HasDefaultValue: Boolean;
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
  TCastleColorPersistent = class(TPersistent)
  strict private
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
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    InternalDefaultValue: TCastleColor;
    property Value: TCastleColor read GetValue write SetValue;
    function HasDefaultValue: Boolean;
  published
    property Red: Single read GetRed write SetRed stored RedIsStored nodefault;
    property Green: Single read GetGreen write SetGreen stored GreenIsStored nodefault;
    property Blue: Single read GetBlue write SetBlue stored BlueIsStored nodefault;
    property Alpha: Single read GetAlpha write SetAlpha stored AlphaIsStored nodefault;
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

function TCastleColorRGBPersistent.GetRed: Single;
begin
  Result := Value.Data[0];
end;

function TCastleColorRGBPersistent.GetGreen: Single;
begin
  Result := Value.Data[1];
end;

function TCastleColorRGBPersistent.GetBlue: Single;
begin
  Result := Value.Data[2];
end;

procedure TCastleColorRGBPersistent.SetRed(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleColorRGBPersistent.SetGreen(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleColorRGBPersistent.SetBlue(const AValue: Single);
var
  V: TCastleColorRGB;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

function TCastleColorRGBPersistent.RedIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleColorRGBPersistent.GreenIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleColorRGBPersistent.BlueIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

function TCastleColorRGBPersistent.HasDefaultValue: Boolean;
begin
  Result := TVector3.PerfectlyEquals(Value, InternalDefaultValue);
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

function TCastleColorPersistent.GetRed: Single;
begin
  Result := Value.Data[0];
end;

function TCastleColorPersistent.GetGreen: Single;
begin
  Result := Value.Data[1];
end;

function TCastleColorPersistent.GetBlue: Single;
begin
  Result := Value.Data[2];
end;

function TCastleColorPersistent.GetAlpha: Single;
begin
  Result := Value.Data[3];
end;

procedure TCastleColorPersistent.SetRed(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[0] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetGreen(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[1] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetBlue(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[2] := AValue;
  Value := V;
end;

procedure TCastleColorPersistent.SetAlpha(const AValue: Single);
var
  V: TCastleColor;
begin
  V := Value;
  V.Data[3] := AValue;
  Value := V;
end;

function TCastleColorPersistent.RedIsStored: Boolean;
begin
  Result := Value.Data[0] <> InternalDefaultValue.Data[0];
end;

function TCastleColorPersistent.GreenIsStored: Boolean;
begin
  Result := Value.Data[1] <> InternalDefaultValue.Data[1];
end;

function TCastleColorPersistent.BlueIsStored: Boolean;
begin
  Result := Value.Data[2] <> InternalDefaultValue.Data[2];
end;

function TCastleColorPersistent.AlphaIsStored: Boolean;
begin
  Result := Value.Data[3] <> InternalDefaultValue.Data[3];
end;

function TCastleColorPersistent.HasDefaultValue: Boolean;
begin
  Result := TVector4.PerfectlyEquals(Value, InternalDefaultValue);
end;

{$endif read_implementation}
