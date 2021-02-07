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
    procedure SetItems(const Index: TIndex; const Value: SmallInt); inline;
    function GetItemsInt(const Index: Integer): SmallInt; inline;
    procedure SetItemsInt(const Index: Integer; const Value: SmallInt); inline;
  public
    var
      Data: array [TIndex] of SmallInt;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt; inline;
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TVector2SmallInt): TVector2SmallInt; inline;
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TVector2SmallInt): TVector2SmallInt; inline;

    function ToString: String;

    property Items [const Index: TIndex]: SmallInt read GetItems write SetItems; default;
    property X: SmallInt index 0 read GetItemsInt write SetItemsInt;
    property Y: SmallInt index 1 read GetItemsInt write SetItemsInt;

    class function Zero: TVector2SmallInt; static; inline;
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

function TVector2SmallInt.ToString: String;
begin
  Result := Format('%d %d', [Data[0], Data[1]]);
end;

function TVector2SmallInt.GetItems(const Index: TIndex): SmallInt;
begin
  Result := Data[Index];
end;

procedure TVector2SmallInt.SetItems(const Index: TIndex; const Value: SmallInt);
begin
  Data[Index] := Value;
end;

function TVector2SmallInt.GetItemsInt(const Index: Integer): SmallInt;
begin
  Result := Data[Index];
end;

procedure TVector2SmallInt.SetItemsInt(const Index: Integer; const Value: SmallInt);
begin
  Data[Index] := Value;
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
