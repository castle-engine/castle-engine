{%MainUnit castlevectors.pas}
{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleVectors lists of vectors and matrices using Double-precision types. }

{$ifdef read_interface}

type
  { List of @link(TVector2Double). }
  TVector2DoubleList = class({$ifdef FPC}specialize{$endif} TStructList<TVector2Double>)
  public
    function ToVector2: TVector2List;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TVector2DoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TVector3Double). }
  TVector3DoubleList = class({$ifdef FPC}specialize{$endif} TStructList<TVector3Double>)
  public
    function ToVector3: TVector3List;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TVector3DoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TVector4Double). }
  TVector4DoubleList = class({$ifdef FPC}specialize{$endif} TStructList<TVector4Double>)
  public
    function ToVector4: TVector4List;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TVector4DoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TMatrix3Double). }
  TMatrix3DoubleList = class({$ifdef FPC}specialize{$endif} TStructList<TMatrix3Double>)
  public
    function ToMatrix3: TMatrix3List;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TMatrix3DoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TMatrix4Double). }
  TMatrix4DoubleList = class({$ifdef FPC}specialize{$endif} TStructList<TMatrix4Double>)
  public
    function ToMatrix4: TMatrix4List;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TMatrix4DoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2DoubleList ----------------------------------------------------- }

function TVector2DoubleList.ToVector2: TVector2List;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector2List.Create;
  Result.Count := Count;
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 2 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector2DoubleList.AssignLerp(const Fraction: Double; const V1, V2: TVector2DoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[I], V2.L[I]);
end;

function TVector2DoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector2DoubleList;
  if Result then
  begin
    Result := Count = TVector2DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector2Double.Equals(TVector2DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector2DoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector2DoubleList;
  if Result then
  begin
    Result := Count = TVector2DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector2Double.PerfectlyEquals(TVector2DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TVector3DoubleList ----------------------------------------------------- }

function TVector3DoubleList.ToVector3: TVector3List;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector3List.Create;
  Result.Count := Count;
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector3DoubleList.AssignLerp(const Fraction: Double; const V1, V2: TVector3DoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[I], V2.L[I]);
end;

function TVector3DoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector3DoubleList;
  if Result then
  begin
    Result := Count = TVector3DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector3Double.Equals(TVector3DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector3DoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector3DoubleList;
  if Result then
  begin
    Result := Count = TVector3DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector3Double.PerfectlyEquals(TVector3DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TVector4DoubleList ----------------------------------------------------- }

function TVector4DoubleList.ToVector4: TVector4List;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector4List.Create;
  Result.Count := Count;
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector4DoubleList.AssignLerp(const Fraction: Double; const V1, V2: TVector4DoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[I], V2.L[I]);
end;

function TVector4DoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector4DoubleList;
  if Result then
  begin
    Result := Count = TVector4DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector4Double.Equals(TVector4DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector4DoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector4DoubleList;
  if Result then
  begin
    Result := Count = TVector4DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector4Double.PerfectlyEquals(TVector4DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TMatrix3DoubleList ----------------------------------------------------- }

function TMatrix3DoubleList.ToMatrix3: TMatrix3List;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix3List.Create;
  Result.Count := Count;
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 3 * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix3DoubleList.AssignLerp(const Fraction: Double; const V1, V2: TMatrix3DoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[I], V2.L[I]);
end;

function TMatrix3DoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix3DoubleList;
  if Result then
  begin
    Result := Count = TMatrix3DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix3Double.Equals(TMatrix3DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TMatrix3DoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix3DoubleList;
  if Result then
  begin
    Result := Count = TMatrix3DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix3Double.PerfectlyEquals(TMatrix3DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TMatrix4DoubleList ----------------------------------------------------- }

function TMatrix4DoubleList.ToMatrix4: TMatrix4List;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix4List.Create;
  Result.Count := Count;
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 4 * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix4DoubleList.AssignLerp(const Fraction: Double; const V1, V2: TMatrix4DoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[I], V2.L[I]);
end;

function TMatrix4DoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix4DoubleList;
  if Result then
  begin
    Result := Count = TMatrix4DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix4Double.Equals(TMatrix4DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TMatrix4DoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix4DoubleList;
  if Result then
  begin
    Result := Count = TMatrix4DoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix4Double.PerfectlyEquals(TMatrix4DoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{$endif read_implementation}
