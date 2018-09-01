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
    and Castle Game Engine visual designer).

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
  public
    InternalGetValue: TGetVector3Event;
    InternalSetValue: TSetVector3Event;
    property Value: TCastleColorRGB read GetValue write SetValue;
  published
    property R: Single read GetR write SetR;
    property G: Single read GetG write SetG;
    property B: Single read GetB write SetB;
  end;

  { TCastleColor record represented as a TPersistent descendant,
    to be able to visually edit it (in Lazarus and Delphi visual designer,
    and Castle Game Engine visual designer).

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
    //procedure ReadValue(Reader: TReader);
    //procedure WriteValue(Writer: TWriter);
  protected
    // TODO: doesn't work for LFM for now.
    //{ Makes the color properly saved to LFM / DFM.
    //  Not used (and not necessary) for JSON serialization in CastleComponentSerialize. }
    //procedure DefineProperties(Filer: TFiler); override;
  public
    InternalGetValue: TGetVector4Event;
    InternalSetValue: TSetVector4Event;
    property Value: TCastleColor read GetValue write SetValue;
  published
    property R: Single read GetR write SetR;
    property G: Single read GetG write SetG;
    property B: Single read GetB write SetB;
    property A: Single read GetA write SetA;
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

//procedure TCastleColorPersistent.ReadValue(Reader: TReader);
//var
//  V: TCastleColor;
//begin
//  V.Data[0] := Reader.ReadSingle;
//  V.Data[1] := Reader.ReadSingle;
//  V.Data[2] := Reader.ReadSingle;
//  V.Data[3] := Reader.ReadSingle;
//  Value := V;
//end;
//
//procedure TCastleColorPersistent.WriteValue(Writer: TWriter);
//var
//  V: TCastleColor;
//begin
//  V := Value;
//  Writer.WriteSingle(V.Data[0]);
//  Writer.WriteSingle(V.Data[1]);
//  Writer.WriteSingle(V.Data[2]);
//  Writer.WriteSingle(V.Data[3]);
//end;
//
//procedure TCastleColorPersistent.DefineProperties(Filer: TFiler);
//begin
//  inherited;
//  Filer.DefineProperty('Value',
//    {$ifdef CASTLE_OBJFPC}@{$endif} ReadValue,
//    {$ifdef CASTLE_OBJFPC}@{$endif} WriteValue,
//    // TODO: no default, always write
//    true);
//end;

{$endif read_implementation}
