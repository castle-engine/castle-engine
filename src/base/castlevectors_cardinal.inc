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

{ Vector types using Cardinal components. }

{$ifdef read_interface}

type
  { Vector of 2 Cardinal values.
    @seealso TVector3Cardinal }
  TVector2Cardinal = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): Cardinal; inline;
    procedure SetItems(const Index: TIndex; const Value: Cardinal); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Cardinal read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Cardinal read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Cardinal): TVector2Cardinal; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector2Cardinal): boolean; static; inline;

    class function Zero: TVector2Cardinal; static; inline;

    case Integer of
      0: (X, Y: Cardinal);
      1: (Data: array [TIndex] of Cardinal);
  end;

  { Vector of 3 Cardinal values. }
  TVector3Cardinal = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): Cardinal; inline;
    procedure SetItems(const Index: TIndex; const Value: Cardinal); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Cardinal read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Cardinal read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Cardinal): TVector3Cardinal; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector3Cardinal): boolean; static; inline;

    class function Zero: TVector3Cardinal; static; inline;

    case Integer of
      0: (X, Y, Z: Cardinal);
      1: (Data: array [TIndex] of Cardinal);
      2: (XY: TVector2Cardinal);
  end;

  { Vector of 4 Cardinal values. }
  TVector4Cardinal = record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): Cardinal; inline;
    procedure SetItems(const Index: TIndex; const Value: Cardinal); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Cardinal read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Cardinal read GetItems write SetItems;
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use instead X, Y, Z, W fields';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Cardinal): TVector4Cardinal; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector4Cardinal): boolean; static; inline;

    class function Zero: TVector4Cardinal; static; inline;

    case Integer of
      0: (X, Y, Z, W: Cardinal);
      1: (Data: array [TIndex] of Cardinal);
      2: (XY: TVector2Cardinal; ZW: Cardinal);
      3: (XYZ: TVector3Cardinal);
  end;

  PVector2Cardinal = ^TVector2Cardinal;
  PVector3Cardinal = ^TVector3Cardinal;
  PVector4Cardinal = ^TVector4Cardinal;

  TVector2CardinalArray = packed array [0..MaxInt div SizeOf(TVector2Cardinal)-1] of TVector2Cardinal;
  PVector2CardinalArray = ^TVector2CardinalArray;
  TVector3CardinalArray = packed array [0..MaxInt div SizeOf(TVector3Cardinal)-1] of TVector3Cardinal;
  PVector3CardinalArray = ^TVector3CardinalArray;
  TVector4CardinalArray = packed array [0..MaxInt div SizeOf(TVector4Cardinal)-1] of TVector4Cardinal;
  PVector4CardinalArray = ^TVector4CardinalArray;

function Vector2Cardinal(const X, Y: Cardinal): TVector2Cardinal; inline;
function Vector3Cardinal(const X, Y, Z: Cardinal): TVector3Cardinal; inline;
function Vector4Cardinal(const X, Y, Z, W: Cardinal): TVector4Cardinal; inline;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2Cardinal ------------------------------------------------------------ }

class operator TVector2Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVector2Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVector2Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Cardinal): TVector2Cardinal;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

function TVector2Cardinal.ToString: string;
begin
  Result := Format('%d %d', [X, Y]);
end;

function TVector2Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    {$ifndef FPC} else raise EInternalError.Create(2022021119); {$endif}
  end;
end;

procedure TVector2Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021120); {$endif}
  end;
end;

function TVector2Cardinal.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0);
end;

class function TVector2Cardinal.Equals(const V1, V2: TVector2Cardinal): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y);
end;

class function TVector2Cardinal.Zero: TVector2Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector3Cardinal ------------------------------------------------------------ }

class operator TVector3Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector3Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TVector3Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Cardinal): TVector3Cardinal;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
end;

function TVector3Cardinal.ToString: string;
begin
  Result := Format('%d %d %d', [X, Y, Z]);
end;

function TVector3Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    {$ifndef FPC} else raise EInternalError.Create(2022021121); {$endif}
  end;
end;

procedure TVector3Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021122); {$endif}
  end;
end;

function TVector3Cardinal.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0);
end;

class function TVector3Cardinal.Equals(const V1, V2: TVector3Cardinal): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z);
end;

class function TVector3Cardinal.Zero: TVector3Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector4Cardinal ------------------------------------------------------------ }

class operator TVector4Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TVector4Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TVector4Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Cardinal): TVector4Cardinal;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
  Result.W := - V.W;
end;

function TVector4Cardinal.ToString: string;
begin
  Result := Format('%d %d %d %d', [X, Y, Z, W]);
end;

function TVector4Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    3: Result := W;
    {$ifndef FPC} else raise EInternalError.Create(2022021123); {$endif}
  end;
end;

procedure TVector4Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    3: W := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021124); {$endif}
  end;
end;

function TVector4Cardinal.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0) and
    (W = 0);
end;

class function TVector4Cardinal.Equals(const V1, V2: TVector4Cardinal): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z) and
            (V1.W = V2.W);
end;

class function TVector4Cardinal.Zero: TVector4Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2Cardinal(const X, Y: Cardinal): TVector2Cardinal;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3Cardinal(const X, Y, Z: Cardinal): TVector3Cardinal;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector4Cardinal(const X, Y, Z, W: Cardinal): TVector4Cardinal;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

(*
Not used now, see TVector*Integer comments for Lerp.

function Lerp(const A: Single; const V1, V2: TVector2Cardinal): TVector2Cardinal; overload;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Cardinal));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Cardinal));
end;

function Lerp(const A: Single; const V1, V2: TVector3Cardinal): TVector3Cardinal; overload;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Cardinal));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Cardinal));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), 0, High(Cardinal));
end;

function Lerp(const A: Single; const V1, V2: TVector4Cardinal): TVector4Cardinal; overload;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), 0, High(Cardinal));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), 0, High(Cardinal));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), 0, High(Cardinal));
  Result.W := Clamped(Round(V1.W + A * (V2.W - V1.W)), 0, High(Cardinal));
end;
*)

{$endif read_implementation}
