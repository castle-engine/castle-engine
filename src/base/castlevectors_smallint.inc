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

{ Vector types using SmallInt components. }

{$ifdef read_interface}

type
  { Vector of 2 SmallInt values. }
  TVector2SmallInt = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): SmallInt; inline;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123) if you want to set by index,
      or @code(MyVector.X := 123). }
    property AsArray [const Index: TIndex]: SmallInt read GetItems; default;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2SmallInt): TVector2SmallInt; inline;

    function ToString: string;

    class function Zero: TVector2SmallInt; static; inline;

    case Integer of
      0: (X, Y: SmallInt);
      1: (Data: array [TIndex] of SmallInt);
  end;

  PVector2SmallInt = ^TVector2SmallInt;

function Vector2SmallInt(const X, Y: SmallInt): TVector2SmallInt; inline;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2SmallInt ------------------------------------------------------------ }

class operator TVector2SmallInt. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
end;

class operator TVector2SmallInt. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
end;

class operator TVector2SmallInt. {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2SmallInt): TVector2SmallInt;
begin
  Result.Data[0] := - V.Data[0];
  Result.Data[1] := - V.Data[1];
end;

function TVector2SmallInt.ToString: string;
begin
  Result := Format('%d %d', [Data[0], Data[1]]);
end;

function TVector2SmallInt.GetItems(const Index: TIndex): SmallInt;
begin
  Result := Data[Index];
end;

class function TVector2SmallInt.Zero: TVector2SmallInt;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ global routines ------------------------------------------------------------ }

function Vector2SmallInt(const X, Y: SmallInt): TVector2SmallInt;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

{$endif read_implementation}
