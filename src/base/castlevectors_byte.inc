{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(* Vectors based on the Byte type. *)

{$ifdef read_interface}

type
  { Vector of 2 Byte values.
    @seealso TVector3Byte }
  TVector2Byte = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
    function GetItemsInt(const Index: Integer): Byte; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Byte); inline;
  public
    var
      Data: array [TIndex] of Byte;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Byte): TVector2Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Byte): TVector2Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Byte): TVector2Byte; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Byte read GetItems write SetItems; default;
    property X: Byte index 0 read GetItemsInt write SetItemsInt;
    property Y: Byte index 1 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector2Byte): Boolean; static; inline;

    class function Zero: TVector2Byte; static; inline;
  end;

  { Vector of 3 Byte values. }
  TVector3Byte = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
    function GetItemsInt(const Index: Integer): Byte; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Byte); inline;
  public
    var
      Data: array [TIndex] of Byte;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Byte): TVector3Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Byte): TVector3Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Byte): TVector3Byte; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Byte read GetItems write SetItems; default;
    property X: Byte index 0 read GetItemsInt write SetItemsInt;
    property Y: Byte index 1 read GetItemsInt write SetItemsInt;
    property Z: Byte index 2 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector3Byte): Boolean; static; inline;

    class function Zero: TVector3Byte; static; inline;
  end;

  { Vector of 4 Byte values. }
  TVector4Byte = record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
    function GetItemsInt(const Index: Integer): Byte; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Byte); inline;
  public
    var
      Data: array [TIndex] of Byte;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Byte): TVector4Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Byte): TVector4Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Byte): TVector4Byte; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Byte read GetItems write SetItems; default;
    property X: Byte index 0 read GetItemsInt write SetItemsInt;
    property Y: Byte index 1 read GetItemsInt write SetItemsInt;
    property Z: Byte index 2 read GetItemsInt write SetItemsInt;
    property W: Byte index 3 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector4Byte): Boolean; static; inline;

    class function Zero: TVector4Byte; static;
  end;

  PVector2Byte = ^TVector2Byte;
  PVector3Byte = ^TVector3Byte;
  PVector4Byte = ^TVector4Byte;

  TVector2ByteArray = packed array [0..MaxInt div SizeOf(TVector2Byte)-1] of TVector2Byte;
  PVector2ByteArray = ^TVector2ByteArray;
  TVector3ByteArray = packed array [0..MaxInt div SizeOf(TVector3Byte)-1] of TVector3Byte;
  PVector3ByteArray = ^TVector3ByteArray;
  TVector4ByteArray = packed array [0..MaxInt div SizeOf(TVector4Byte)-1] of TVector4Byte;
  PVector4ByteArray = ^TVector4ByteArray;

function Vector2Byte(const X, Y: Byte): TVector2Byte; overload; inline;
function Vector3Byte(const X, Y, Z: Byte): TVector3Byte; overload; inline;
function Vector4Byte(const X, Y, Z, W: Byte): TVector4Byte; overload; inline;

{ Convert float vectors into byte vectors.
  Each float component is converted such that float 0.0 (or less)
  results in byte 0, float 1.0 (or more) results in byte 255.
  Values between 0.0 and 1.0 are appropriately (linearly) converted
  into the byte range.
  @groupBegin }
function Vector2Byte(const V: TVector2): TVector2Byte; overload;
function Vector3Byte(const V: TVector3): TVector3Byte; overload;
function Vector4Byte(const V: TVector4): TVector4Byte; overload;
{ @groupEnd }

function Vector2(const V: TVector2Byte): TVector2; overload;
function Vector3(const V: TVector3Byte): TVector3; overload;
function Vector4(const V: TVector4Byte): TVector4; overload;

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte; overload; inline;
function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte; overload; inline;
function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte; overload; inline;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2Byte ------------------------------------------------------------ }

class operator TVector2Byte. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Byte): TVector2Byte;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
end;

class operator TVector2Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Byte): TVector2Byte;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
end;

class operator TVector2Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Byte): TVector2Byte;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
end;

function TVector2Byte.ToString: String;
begin
  Result := Format('%d %d', [Data[0], Data[1]]);
end;

function TVector2Byte.GetItems(const Index: TIndex): Byte;
begin
  Result := Data[Index];
end;

procedure TVector2Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector2Byte.GetItemsInt(const Index: Integer): Byte;
begin
  Result := Data[Index];
end;

procedure TVector2Byte.SetItemsInt(const Index: Integer; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector2Byte.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector2Byte.Equals(const V1, V2: TVector2Byte): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]);
end;

class function TVector2Byte.Zero: TVector2Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector3Byte ------------------------------------------------------------ }

class operator TVector3Byte. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
end;

class operator TVector3Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
end;

class operator TVector3Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
end;

function TVector3Byte.ToString: String;
begin
  Result := Format('%d %d %d', [Data[0], Data[1], Data[2]]);
end;

function TVector3Byte.GetItems(const Index: TIndex): Byte;
begin
  Result := Data[Index];
end;

procedure TVector3Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector3Byte.GetItemsInt(const Index: Integer): Byte;
begin
  Result := Data[Index];
end;

procedure TVector3Byte.SetItemsInt(const Index: Integer; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector3Byte.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector3Byte.Equals(const V1, V2: TVector3Byte): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]) and
            (V1.Data[2] = V2.Data[2]);
end;

class function TVector3Byte.Zero: TVector3Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector4Byte ------------------------------------------------------------ }

class operator TVector4Byte. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Byte): TVector4Byte;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
  Result.Data[3] := A.Data[3] + B.Data[3];
end;

class operator TVector4Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Byte): TVector4Byte;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
  Result.Data[3] := A.Data[3] - B.Data[3];
end;

class operator TVector4Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Byte): TVector4Byte;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
  Result.Data[3] := - V.Data[3];
end;

function TVector4Byte.ToString: String;
begin
  Result := Format('%d %d %d %d', [Data[0], Data[1], Data[2], Data[3]]);
end;

function TVector4Byte.GetItems(const Index: TIndex): Byte;
begin
  Result := Data[Index];
end;

procedure TVector4Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector4Byte.GetItemsInt(const Index: Integer): Byte;
begin
  Result := Data[Index];
end;

procedure TVector4Byte.SetItemsInt(const Index: Integer; const Value: Byte);
begin
  Data[Index] := Value;
end;

function TVector4Byte.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector4Byte.Equals(const V1, V2: TVector4Byte): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]) and
            (V1.Data[2] = V2.Data[2]) and
            (V1.Data[3] = V2.Data[3]);
end;

class function TVector4Byte.Zero: TVector4Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2Byte(const X, Y: Byte): TVector2Byte;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

function Vector3Byte(const X, Y, Z: Byte): TVector3Byte;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

function Vector4Byte(const X, Y, Z, W: Byte): TVector4Byte;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
  Result.Data[3] := W;
end;

function Vector2Byte(const V: TVector2): TVector2Byte;
begin
  Result.Data[0] := Clamped(Round(V.Data[0] * 255), Low(Byte), High(Byte));
  Result.Data[1] := Clamped(Round(V.Data[1] * 255), Low(Byte), High(Byte));
end;

function Vector3Byte(const V: TVector3): TVector3Byte;
begin
  Result.Data[0] := Clamped(Round(V.Data[0] * 255), Low(Byte), High(Byte));
  Result.Data[1] := Clamped(Round(V.Data[1] * 255), Low(Byte), High(Byte));
  Result.Data[2] := Clamped(Round(V.Data[2] * 255), Low(Byte), High(Byte));
end;

function Vector4Byte(const V: TVector4): TVector4Byte;
begin
  Result.Data[0] := Clamped(Round(V.Data[0] * 255), Low(Byte), High(Byte));
  Result.Data[1] := Clamped(Round(V.Data[1] * 255), Low(Byte), High(Byte));
  Result.Data[2] := Clamped(Round(V.Data[2] * 255), Low(Byte), High(Byte));
  Result.Data[3] := Clamped(Round(V.Data[3] * 255), Low(Byte), High(Byte));
end;

function Vector2(const V: TVector2Byte): TVector2;
begin
  Result.Data[0] := V.Data[0] / 255;
  Result.Data[1] := V.Data[1] / 255;
end;

function Vector3(const V: TVector3Byte): TVector3;
begin
  Result.Data[0] := V.Data[0] / 255;
  Result.Data[1] := V.Data[1] / 255;
  Result.Data[2] := V.Data[2] / 255;
end;

function Vector4(const V: TVector4Byte): TVector4;
begin
  Result.Data[0] := V.Data[0] / 255;
  Result.Data[1] := V.Data[1] / 255;
  Result.Data[2] := V.Data[2] / 255;
  Result.Data[3] := V.Data[3] / 255;
end;

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Byte));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Byte));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Byte));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Byte));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Byte));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), 0, High(Byte));
  Result.Data[3] := Clamped(Round(V1.Data[3] + A * (V2.Data[3] - V1.Data[3])), 0, High(Byte));
end;

{$endif read_implementation}
