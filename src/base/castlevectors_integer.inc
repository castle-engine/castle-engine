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

{ Vector types using Integer components. }

{$ifdef read_interface}

type
  { Vector of 2 Integer values.
    @seealso TVector3Integer }
  TVector2Integer = packed record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Integer read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y fields';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y fields';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Integer): TVector2Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Integer): TVector2Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Integer): TVector2Integer; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector2Integer): boolean; static; inline;

    class function Zero: TVector2Integer; static; inline;

    case Integer of
      0: (X, Y: Integer);
      1: (Data: array [TIndex] of Integer);
  end;

  { Vector of 3 Integer values. }
  TVector3Integer = packed record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Integer read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Integer): TVector3Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Integer): TVector3Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Integer): TVector3Integer; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector3Integer): boolean; static; inline;

    class function Zero: TVector3Integer; static; inline;

    case Integer of
      0: (X, Y, Z: Integer);
      1: (Data: array [TIndex] of Integer);
      2: (XY: TVector2Integer);
  end;

  { Vector of 4 Integer values. }
  TVector4Integer = packed record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: Integer read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z, W';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: Integer read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use instead X, Y, Z, W';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Integer): TVector4Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Integer): TVector4Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Integer): TVector4Integer; inline;

    function ToString: string;

    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector4Integer): boolean; static; inline;

    class function Zero: TVector4Integer; static; inline;

    case Integer of
      0: (X, Y, Z, W: Integer);
      1: (Data: array [TIndex] of Integer);
      2: (XY: TVector2Integer; ZW: TVector2Integer);
      3: (XYZ: TVector3Integer);
  end;

  PVector2Integer = ^TVector2Integer;
  PVector3Integer = ^TVector3Integer;
  PVector4Integer = ^TVector4Integer;

  TVector2IntegerArray = packed array [0..MaxInt div SizeOf(TVector2Integer)-1] of TVector2Integer;
  PVector2IntegerArray = ^TVector2IntegerArray;
  TVector3IntegerArray = packed array [0..MaxInt div SizeOf(TVector3Integer)-1] of TVector3Integer;
  PVector3IntegerArray = ^TVector3IntegerArray;
  TVector4IntegerArray = packed array [0..MaxInt div SizeOf(TVector4Integer)-1] of TVector4Integer;
  PVector4IntegerArray = ^TVector4IntegerArray;

function Vector2Integer(const X, Y: Integer): TVector2Integer; inline;
function Vector3Integer(const X, Y, Z: Integer): TVector3Integer; inline;
function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer; inline;

function Vector2(const V: TVector2Integer): TVector2; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

(*
Not defined for now, it's not obvious whether this is the most intuitive Lerp.

function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Integer; overload;
function Lerp(const A: Single; const V1, V2: TVector3Integer): TVector3Integer; overload;
function Lerp(const A: Single; const V1, V2: TVector4Integer): TVector4Integer; overload;

The other approach is to convert to floating-point type while doing Lerp, like this:

function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2; overload;
function Lerp(const A: Single; const V1, V2: TVector3Integer): TVector3; overload;
function Lerp(const A: Single; const V1, V2: TVector4Integer): TVector4; overload;
*)

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2Integer ------------------------------------------------------------ }

class operator TVector2Integer. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Integer): TVector2Integer;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVector2Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Integer): TVector2Integer;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVector2Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Integer): TVector2Integer;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

function TVector2Integer.ToString: string;
begin
  Result := Format('%d %d', [X, Y]);
end;

function TVector2Integer.GetItems(const Index: TIndex): Integer;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    {$ifndef FPC} else raise EInternalError.Create(2022021113); {$endif}
  end;
end;

procedure TVector2Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021114); {$endif}
  end;
end;

function TVector2Integer.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0);
end;

class function TVector2Integer.Equals(const V1, V2: TVector2Integer): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y);
end;

class function TVector2Integer.Zero: TVector2Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector3Integer ------------------------------------------------------------ }

class operator TVector3Integer. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Integer): TVector3Integer;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector3Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Integer): TVector3Integer;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TVector3Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Integer): TVector3Integer;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
end;

function TVector3Integer.ToString: string;
begin
  Result := Format('%d %d %d', [X, Y, Z]);
end;

function TVector3Integer.GetItems(const Index: TIndex): Integer;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    {$ifndef FPC} else raise EInternalError.Create(2022021115); {$endif}
  end;
end;

procedure TVector3Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021116); {$endif}
  end;
end;

function TVector3Integer.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0);
end;

class function TVector3Integer.Equals(const V1, V2: TVector3Integer): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z);
end;

class function TVector3Integer.Zero: TVector3Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector4Integer ------------------------------------------------------------ }

class operator TVector4Integer. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Integer): TVector4Integer;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TVector4Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Integer): TVector4Integer;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TVector4Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Integer): TVector4Integer;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
  Result.W := - V.W;
end;

function TVector4Integer.ToString: string;
begin
  Result := Format('%d %d %d %d', [X, Y, Z, W]);
end;

function TVector4Integer.GetItems(const Index: TIndex): Integer;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    3: Result := W;
    {$ifndef FPC} else raise EInternalError.Create(2022021117); {$endif}
  end;
end;

procedure TVector4Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    3: W := Value;
    {$ifndef FPC} else raise EInternalError.Create(2022021118); {$endif}
  end;
end;

function TVector4Integer.IsZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0) and
    (W = 0);
end;

class function TVector4Integer.Equals(const V1, V2: TVector4Integer): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z) and
            (V1.W = V2.W);
end;

class function TVector4Integer.Zero: TVector4Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2Integer(const X, Y: Integer): TVector2Integer;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3Integer(const X, Y, Z: Integer): TVector3Integer;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector2(const V: TVector2Integer): TVector2;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

(*
function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Integer;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), Low(Integer), High(Integer));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), Low(Integer), High(Integer));
end;

function Lerp(const A: Single; const V1, V2: TVector3Integer): TVector3Integer;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), Low(Integer), High(Integer));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), Low(Integer), High(Integer));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), Low(Integer), High(Integer));
end;

function Lerp(const A: Single; const V1, V2: TVector4Integer): TVector4Integer;
begin
  Result.X := Clamped(Round(V1.X + A * (V2.X - V1.X)), Low(Integer), High(Integer));
  Result.Y := Clamped(Round(V1.Y + A * (V2.Y - V1.Y)), Low(Integer), High(Integer));
  Result.Z := Clamped(Round(V1.Z + A * (V2.Z - V1.Z)), Low(Integer), High(Integer));
  Result.W := Clamped(Round(V1.W + A * (V2.W - V1.W)), Low(Integer), High(Integer));
end;
*)

{$endif read_implementation}
