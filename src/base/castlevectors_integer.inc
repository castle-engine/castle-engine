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

{ Vector types using Integer components. }

{$ifdef read_interface}

type
  { Vector of 2 Integer values.
    @seealso TVector3Integer }
  TVector2Integer = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
    function GetItemsInt(const Index: Integer): Integer; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Integer); inline;
  public
    var
      Data: array [TIndex] of Integer;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Integer): TVector2Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Integer): TVector2Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Integer): TVector2Integer; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Integer read GetItems write SetItems; default;
    property X: Integer index 0 read GetItemsInt write SetItemsInt;
    property Y: Integer index 1 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector2Integer): Boolean; static; inline;

    class function Zero: TVector2Integer; static; inline;
  end;

  { Vector of 3 Integer values. }
  TVector3Integer = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
    function GetItemsInt(const Index: Integer): Integer; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Integer); inline;
  public
    var
      Data: array [TIndex] of Integer;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Integer): TVector3Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Integer): TVector3Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Integer): TVector3Integer; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Integer read GetItems write SetItems; default;
    property X: Integer index 0 read GetItemsInt write SetItemsInt;
    property Y: Integer index 1 read GetItemsInt write SetItemsInt;
    property Z: Integer index 2 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector3Integer): Boolean; static; inline;

    class function Zero: TVector3Integer; static; inline;
  end;

  { Vector of 4 Integer values. }
  TVector4Integer = record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): Integer; inline;
    procedure SetItems(const Index: TIndex; const Value: Integer); inline;
    function GetItemsInt(const Index: Integer): Integer; inline;
    procedure SetItemsInt(const Index: Integer; const Value: Integer); inline;
  public
    var
      Data: array [TIndex] of Integer;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Integer): TVector4Integer; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Integer): TVector4Integer; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Integer): TVector4Integer; inline;

    function ToString: String;
    property Items [const Index: TIndex]: Integer read GetItems write SetItems; default;
    property X: Integer index 0 read GetItemsInt write SetItemsInt;
    property Y: Integer index 1 read GetItemsInt write SetItemsInt;
    property Z: Integer index 2 read GetItemsInt write SetItemsInt;
    property W: Integer index 3 read GetItemsInt write SetItemsInt;

    function IsZero: Boolean; inline;
    class function Equals(const V1, V2: TVector4Integer): Boolean; static; inline;

    class function Zero: TVector4Integer; static; inline;
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

function Vector2(const V: TVector2Integer): TVector2; overload; inline;

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
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
end;

class operator TVector2Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Integer): TVector2Integer;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
end;

class operator TVector2Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Integer): TVector2Integer;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
end;

function TVector2Integer.ToString: String;
begin
  Result := Format('%d %d', [Data[0], Data[1]]);
end;

function TVector2Integer.GetItems(const Index: TIndex): Integer;
begin
  Result := Data[Index];
end;

procedure TVector2Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector2Integer.GetItemsInt(const Index: Integer): Integer;
begin
  Result := Data[Index];
end;

procedure TVector2Integer.SetItemsInt(const Index: Integer; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector2Integer.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector2Integer.Equals(const V1, V2: TVector2Integer): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]);
end;

class function TVector2Integer.Zero: TVector2Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector3Integer ------------------------------------------------------------ }

class operator TVector3Integer. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Integer): TVector3Integer;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
end;

class operator TVector3Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Integer): TVector3Integer;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
end;

class operator TVector3Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Integer): TVector3Integer;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
end;

function TVector3Integer.ToString: String;
begin
  Result := Format('%d %d %d', [Data[0], Data[1], Data[2]]);
end;

function TVector3Integer.GetItems(const Index: TIndex): Integer;
begin
  Result := Data[Index];
end;

procedure TVector3Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector3Integer.GetItemsInt(const Index: Integer): Integer;
begin
  Result := Data[Index];
end;

procedure TVector3Integer.SetItemsInt(const Index: Integer; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector3Integer.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector3Integer.Equals(const V1, V2: TVector3Integer): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]) and
            (V1.Data[2] = V2.Data[2]);
end;

class function TVector3Integer.Zero: TVector3Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVector4Integer ------------------------------------------------------------ }

class operator TVector4Integer. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Integer): TVector4Integer;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
  Result.Data[3] := A.Data[3] + B.Data[3];
end;

class operator TVector4Integer. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Integer): TVector4Integer;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
  Result.Data[3] := A.Data[3] - B.Data[3];
end;

class operator TVector4Integer. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Integer): TVector4Integer;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
  Result.Data[3] := - V.Data[3];
end;

function TVector4Integer.ToString: String;
begin
  Result := Format('%d %d %d %d', [Data[0], Data[1], Data[2], Data[3]]);
end;

function TVector4Integer.GetItems(const Index: TIndex): Integer;
begin
  Result := Data[Index];
end;

procedure TVector4Integer.SetItems(const Index: TIndex; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector4Integer.GetItemsInt(const Index: Integer): Integer;
begin
  Result := Data[Index];
end;

procedure TVector4Integer.SetItemsInt(const Index: Integer; const Value: Integer);
begin
  Data[Index] := Value;
end;

function TVector4Integer.IsZero: Boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector4Integer.Equals(const V1, V2: TVector4Integer): Boolean;
begin
  Result := (V1.Data[0] = V2.Data[0]) and
            (V1.Data[1] = V2.Data[1]) and
            (V1.Data[2] = V2.Data[2]) and
            (V1.Data[3] = V2.Data[3]);
end;

class function TVector4Integer.Zero: TVector4Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2Integer(const X, Y: Integer): TVector2Integer;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

function Vector3Integer(const X, Y, Z: Integer): TVector3Integer;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
  Result.Data[3] := W;
end;

function Vector2(const V: TVector2Integer): TVector2;
begin
  Result.Data[0] := V.Data[0];
  Result.Data[1] := V.Data[1];
end;

(*
function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Integer;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), Low(Integer), High(Integer));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), Low(Integer), High(Integer));
end;

function Lerp(const A: Single; const V1, V2: TVector3Integer): TVector3Integer;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), Low(Integer), High(Integer));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), Low(Integer), High(Integer));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), Low(Integer), High(Integer));
end;

function Lerp(const A: Single; const V1, V2: TVector4Integer): TVector4Integer;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), Low(Integer), High(Integer));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), Low(Integer), High(Integer));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), Low(Integer), High(Integer));
  Result.Data[3] := Clamped(Round(V1.Data[3] + A * (V2.Data[3] - V1.Data[3])), Low(Integer), High(Integer));
end;
*)

{$endif read_implementation}
