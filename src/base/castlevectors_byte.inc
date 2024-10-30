{%MainUnit castlevectors.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

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
  TVector2Byte = packed record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Byte read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Byte read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Byte): TVector2Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Byte): TVector2Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Byte): TVector2Byte; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector2Byte): boolean; static; inline;

    class function Zero: TVector2Byte; static; inline;

    case Integer of
      0: (X, Y: Byte);
      1: (Data: array [TIndex] of Byte);
  end;

  { Vector of 3 Byte values. }
  TVector3Byte = packed record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Byte read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Byte read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Byte): TVector3Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Byte): TVector3Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Byte): TVector3Byte; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector3Byte): boolean; static; inline;

    class function Zero: TVector3Byte; static; inline;

    case Integer of
      0: (X, Y, Z: Byte);
      1: (Data: array [TIndex] of Byte);
      2: (XY: TVector2Byte);
  end;

  { Vector of 4 Byte values. }
  TVector4Byte = packed record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): Byte; inline;
    procedure SetItems(const Index: TIndex; const Value: Byte); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Byte read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Byte read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z, W';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Byte): TVector4Byte; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Byte): TVector4Byte; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Byte): TVector4Byte; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector4Byte): boolean; static; inline;

    class function Zero: TVector4Byte; static;

    case Integer of
      0: (X, Y, Z, W: Byte);
      1: (Data: array [TIndex] of Byte);
      2: (XY: TVector2Byte; ZW: TVector2Byte);
      3: (XYZ: TVector3Byte);
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
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVector2Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Byte): TVector2Byte;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVector2Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Byte): TVector2Byte;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

function TVector2Byte.ToString: string;
begin
  Result := Format('%d %d', [X, Y]);
end;

function TVector2Byte.GetItems(const Index: TIndex): Byte;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    {$ifndef FPC} else raise EInternalError.Create(202202117); {$endif}
  end;
end;

procedure TVector2Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    {$ifndef FPC} else raise EInternalError.Create(202202118); {$endif}
  end;
end;

function TVector2Byte.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0);
end;

class function TVector2Byte.Equals(const V1, V2: TVector2Byte): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y);
end;

class function TVector2Byte.Zero: TVector2Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector3Byte ------------------------------------------------------------ }

class operator TVector3Byte. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Byte): TVector3Byte;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector3Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Byte): TVector3Byte;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TVector3Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Byte): TVector3Byte;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
end;

function TVector3Byte.ToString: string;
begin
  Result := Format('%d %d %d', [X, Y, Z]);
end;

function TVector3Byte.GetItems(const Index: TIndex): Byte;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    {$ifndef FPC} else raise EInternalError.Create(202202119); {$endif}
  end;
end;

procedure TVector3Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021110); {$endif}
  end;
end;

function TVector3Byte.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0);
end;

class function TVector3Byte.Equals(const V1, V2: TVector3Byte): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z);
end;

class function TVector3Byte.Zero: TVector3Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector4Byte ------------------------------------------------------------ }

class operator TVector4Byte. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Byte): TVector4Byte;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TVector4Byte. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Byte): TVector4Byte;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TVector4Byte. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Byte): TVector4Byte;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
  Result.W := - V.W;
end;

function TVector4Byte.ToString: string;
begin
  Result := Format('%d %d %d %d', [X, Y, Z, W]);
end;

function TVector4Byte.GetItems(const Index: TIndex): Byte;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    3: Result := W;
    {$ifndef FPC} else raise EInternalError.Create(2022021111); {$endif}
  end;
end;

procedure TVector4Byte.SetItems(const Index: TIndex; const Value: Byte);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    3: W := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021112); {$endif}
  end;
end;

function TVector4Byte.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0) and
    (W = 0);
end;

class function TVector4Byte.Equals(const V1, V2: TVector4Byte): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z) and
            (V1.W = V2.W);
end;

class function TVector4Byte.Zero: TVector4Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2Byte(const X, Y: Byte): TVector2Byte;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3Byte(const X, Y, Z: Byte): TVector3Byte;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector4Byte(const X, Y, Z, W: Byte): TVector4Byte;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector2Byte(const V: TVector2): TVector2Byte;
begin
  Result.X := Clamped(Round(V.X * 255), Low(Byte), High(Byte));
  Result.Y := Clamped(Round(V.Y * 255), Low(Byte), High(Byte));
end;

function Vector3Byte(const V: TVector3): TVector3Byte;
begin
  Result.X := Clamped(Round(V.X * 255), Low(Byte), High(Byte));
  Result.Y := Clamped(Round(V.Y * 255), Low(Byte), High(Byte));
  Result.Z := Clamped(Round(V.Z * 255), Low(Byte), High(Byte));
end;

function Vector4Byte(const V: TVector4): TVector4Byte;
begin
  Result.X := Clamped(Round(V.X * 255), Low(Byte), High(Byte));
  Result.Y := Clamped(Round(V.Y * 255), Low(Byte), High(Byte));
  Result.Z := Clamped(Round(V.Z * 255), Low(Byte), High(Byte));
  Result.W := Clamped(Round(V.W * 255), Low(Byte), High(Byte));
end;

function Vector2(const V: TVector2Byte): TVector2;
begin
  Result.X := V.X / 255;
  Result.Y := V.Y / 255;
end;

function Vector3(const V: TVector3Byte): TVector3;
begin
  Result.X := V.X / 255;
  Result.Y := V.Y / 255;
  Result.Z := V.Z / 255;
end;

function Vector4(const V: TVector4Byte): TVector4;
begin
  Result.X := V.X / 255;
  Result.Y := V.Y / 255;
  Result.Z := V.Z / 255;
  Result.W := V.W / 255;
end;

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Byte));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Byte));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Byte));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Byte));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Byte));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), 0, High(Byte));
  Result.W := Clamped(Round(V1.W + A * (V2.W - V1.W)), 0, High(Byte));
end;

{$endif read_implementation}
