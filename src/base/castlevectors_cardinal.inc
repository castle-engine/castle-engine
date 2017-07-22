{
  Copyright 2017-2017 Michalis Kamburelis.

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
    var
      Data: array [TIndex] of Cardinal;
    {$ifndef BUGGY_ZERO_CONSTANT}
    const
      Zero: TVector2Cardinal = (Data: (0, 0));
    {$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Cardinal): TVector2Cardinal; inline;

    function ToString: string;
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems; default;
    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector2Cardinal): boolean; static; inline;

    {$ifdef BUGGY_ZERO_CONSTANT}
    class function Zero: TVector2Cardinal; static; inline;
    {$endif}
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
    var
      Data: array [TIndex] of Cardinal;
    {$ifndef BUGGY_ZERO_CONSTANT}
    const
      Zero: TVector3Cardinal = (Data: (0, 0, 0));
    {$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Cardinal): TVector3Cardinal; inline;

    function ToString: string;
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems; default;
    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector3Cardinal): boolean; static; inline;

    {$ifdef BUGGY_ZERO_CONSTANT}
    class function Zero: TVector3Cardinal; static; inline;
    {$endif}
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
    var
      Data: array [TIndex] of Cardinal;
    {$ifndef BUGGY_ZERO_CONSTANT}
    const
      Zero: TVector4Cardinal = (Data: (0, 0, 0, 0));
    {$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Cardinal): TVector4Cardinal; inline;

    function ToString: string;
    property Items [const Index: TIndex]: Cardinal read GetItems write SetItems; default;
    function IsZero: boolean; inline;
    class function Equals(const V1, V2: TVector4Cardinal): boolean; static; inline;

    {$ifdef BUGGY_ZERO_CONSTANT}
    class function Zero: TVector4Cardinal; static; inline;
    {$endif}
  end;

  PVector2Cardinal = ^TVector2Cardinal;
  PVector3Cardinal = ^TVector3Cardinal;
  PVector4Cardinal = ^TVector4Cardinal;

  TArray_Vector2Cardinal = packed array [0..MaxInt div SizeOf(TVector2Cardinal)-1] of TVector2Cardinal;
  PArray_Vector2Cardinal = ^TArray_Vector2Cardinal;
  TArray_Vector3Cardinal = packed array [0..MaxInt div SizeOf(TVector3Cardinal)-1] of TVector3Cardinal;
  PArray_Vector3Cardinal = ^TArray_Vector3Cardinal;
  TArray_Vector4Cardinal = packed array [0..MaxInt div SizeOf(TVector4Cardinal)-1] of TVector4Cardinal;
  PArray_Vector4Cardinal = ^TArray_Vector4Cardinal;

function Vector2Cardinal(const X, Y: Cardinal): TVector2Cardinal; inline;
function Vector3Cardinal(const X, Y, Z: Cardinal): TVector3Cardinal; inline;
function Vector4Cardinal(const X, Y, Z, W: Cardinal): TVector4Cardinal; inline;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2Cardinal ------------------------------------------------------------ }

class operator TVector2Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
end;

class operator TVector2Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2Cardinal): TVector2Cardinal;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
end;

class operator TVector2Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2Cardinal): TVector2Cardinal;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
end;

function TVector2Cardinal.ToString: string;
begin
  Result := Format('%d %d', [Data[0], Data[1]]);
end;

function TVector2Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  Result := Data[Index];
end;

procedure TVector2Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  Data[Index] := Value;
end;

function TVector2Cardinal.IsZero: boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector2Cardinal.Equals(const V1, V2: TVector2Cardinal): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]);
end;

{$ifdef BUGGY_ZERO_CONSTANT}
class function TVector2Cardinal.Zero: TVector2Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;
{$endif}

{ TVector3Cardinal ------------------------------------------------------------ }

class operator TVector3Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
end;

class operator TVector3Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector3Cardinal): TVector3Cardinal;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
end;

class operator TVector3Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector3Cardinal): TVector3Cardinal;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
end;

function TVector3Cardinal.ToString: string;
begin
  Result := Format('%d %d %d', [Data[0], Data[1], Data[2]]);
end;

function TVector3Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  Result := Data[Index];
end;

procedure TVector3Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  Data[Index] := Value;
end;

function TVector3Cardinal.IsZero: boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector3Cardinal.Equals(const V1, V2: TVector3Cardinal): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]);
end;

{$ifdef BUGGY_ZERO_CONSTANT}
class function TVector3Cardinal.Zero: TVector3Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;
{$endif}

{ TVector4Cardinal ------------------------------------------------------------ }

class operator TVector4Cardinal. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
  Result.Data[3] := A.Data[3] + B.Data[3];
end;

class operator TVector4Cardinal. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector4Cardinal): TVector4Cardinal;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
  Result.Data[3] := A.Data[3] - B.Data[3];
end;

class operator TVector4Cardinal. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector4Cardinal): TVector4Cardinal;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
  Result.Data[2] := - V.Data[2];
  Result.Data[3] := - V.Data[3];
end;

function TVector4Cardinal.ToString: string;
begin
  Result := Format('%d %d %d %d', [Data[0], Data[1], Data[2], Data[3]]);
end;

function TVector4Cardinal.GetItems(const Index: TIndex): Cardinal;
begin
  Result := Data[Index];
end;

procedure TVector4Cardinal.SetItems(const Index: TIndex; const Value: Cardinal);
begin
  Data[Index] := Value;
end;

function TVector4Cardinal.IsZero: boolean;
begin
  Result := IsMemCharFilled(Data, SizeOf(Data), #0);
end;

class function TVector4Cardinal.Equals(const V1, V2: TVector4Cardinal): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]) and
            (V1[3] = V2[3]);
end;

{$ifdef BUGGY_ZERO_CONSTANT}
class function TVector4Cardinal.Zero: TVector4Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);
end;
{$endif}

{ global routines ------------------------------------------------------------ }

function Vector2Cardinal(const X, Y: Cardinal): TVector2Cardinal;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

function Vector3Cardinal(const X, Y, Z: Cardinal): TVector3Cardinal;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

function Vector4Cardinal(const X, Y, Z, W: Cardinal): TVector4Cardinal;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
  Result.Data[3] := W;
end;

function Lerp(const A: Single; const V1, V2: TVector2Cardinal): TVector2Cardinal;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Cardinal));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Cardinal));
end;

function Lerp(const A: Single; const V1, V2: TVector3Cardinal): TVector3Cardinal;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Cardinal));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Cardinal));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), 0, High(Cardinal));
end;

function Lerp(const A: Single; const V1, V2: TVector4Cardinal): TVector4Cardinal;
begin
  Result.Data[0] := Clamped(Round(V1.Data[0] + A * (V2.Data[0] - V1.Data[0])), 0, High(Cardinal));
  Result.Data[1] := Clamped(Round(V1.Data[1] + A * (V2.Data[1] - V1.Data[1])), 0, High(Cardinal));
  Result.Data[2] := Clamped(Round(V1.Data[2] + A * (V2.Data[2] - V1.Data[2])), 0, High(Cardinal));
  Result.Data[3] := Clamped(Round(V1.Data[3] + A * (V2.Data[3] - V1.Data[3])), 0, High(Cardinal));
end;

{$endif read_implementation}
