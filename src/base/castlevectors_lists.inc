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

{ CastleVectors lists of vectors and matrices. }

{$ifdef read_interface}

type
  TVector4List = class;

  { List of @link(TVector2).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TVector2List = class({$ifdef FPC}specialize{$endif} TStructList<TVector2>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2): boolean;

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector2>;
      const Index1, Index2, ACount: TListSize); override;

    procedure AddRange(const Source: TVector2List); overload;
    procedure AddSubRange(const Source: TVector2List; const Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector2List; const Index, AddCount: Integer); deprecated 'use AddSubRange';

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TVector3).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TVector3List = class({$ifdef FPC}specialize{$endif} TStructList<TVector3>)
  public
    procedure AssignNegated(const Source: TVector3List); deprecated 'use Assign and Negate separately';

    { Negate all items. }
    procedure Negate;

    { Normalize all items. Zero vectors are left as zero. }
    procedure Normalize;

    { Multiply each item, component-wise, with V. }
    procedure MultiplyComponents(const V: TVector3);

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector3>;
      const Index1, Index2, ACount: TListSize); override;

    { Assign linear interpolation between two other vector arrays,
      and normalize resulting vectors.
      @seealso AssignLerp }
    procedure AssignLerpNormalize(const Fraction: Single;
      const V1, V2: TVector3List;
      const Index1, Index2, ACount: TListSize);

    { Assign linear interpolation between two other vector arrays,
      treating vectors as RGB colors and interpolating in HSV space.
      @seealso AssignLerp }
    procedure AssignLerpRgbInHsv(const Fraction: Single;
      const V1, V2: TVector3List;
      const Index1, Index2, ACount: TListSize);

    procedure AddRange(const Source: TVector3List); overload;
    procedure AddSubRange(const Source: TVector3List; const Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector3List; const Index, AddCount: Integer); deprecated 'use AddSubRange';

    procedure AddRangeTransformed(const Source: TVector3List; const Transform: TMatrix4);

    { Convert to TVector4List, with 4th vector component in
      new array set to constant W. }
    function ToVector4(const W: Single): TVector4List;

    { When two vertexes on the list are closer than MergeDistance,
      set them truly (exactly) equal.
      Returns how many vertex positions were changed. }
    function MergeCloseVertexes(MergeDistance: Single): Cardinal;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TVector4).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TVector4List = class({$ifdef FPC}specialize{$endif} TStructList<TVector4>)
  public
    procedure AddRange(const Source: TVector4List);
    procedure AddSubRange(const Source: TVector4List; const Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector4List; const Index, AddCount: Integer); deprecated 'use AddSubRange';

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector4>;
      const Index1, Index2, ACount: TListSize); override;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TVector4Integer).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TVector4IntegerList = class({$ifdef FPC}specialize{$endif} TStructList<TVector4Integer>)
  public
    procedure AddRange(const Source: TVector4IntegerList);
    procedure AddSubRange(const Source: TVector4IntegerList; const Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector4IntegerList; const Index, AddCount: Integer); deprecated 'use AddSubRange';

    { Does the SecondValue have equal length and content. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TMatrix3).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TMatrix3List = class({$ifdef FPC}specialize{$endif} TStructList<TMatrix3>)
  public
    procedure AddRange(const Source: TMatrix3List);
    procedure AddSubRange(const Source: TMatrix3List; const Index, AddCount: Integer);

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TMatrix3>;
      const Index1, Index2, ACount: TListSize); override;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TMatrix4).
    Note that the notification mechanism (OnNotify) is broken here by routines like AddRange
    and AddSubRange (for maximum speed). }
  TMatrix4List = class({$ifdef FPC}specialize{$endif} TStructList<TMatrix4>)
  public
    procedure AddRange(const Source: TMatrix4List);
    procedure AddSubRange(const Source: TMatrix4List; const Index, AddCount: Integer);

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TMatrix4>;
      const Index1, Index2, ACount: TListSize); override;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of @link(TVector2SmallInt). }
  TVector2SmallIntList = {$ifdef FPC}specialize{$endif} TStructList<TVector2SmallInt>;

  { List of @link(TVector2Cardinal). }
  TVector3CardinalList = {$ifdef FPC}specialize{$endif} TStructList<TVector3Cardinal>;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector2List ----------------------------------------------------- }

function TVector2List.MinMax(out Min, Max: TVector2): boolean;
var
  I: Integer;
begin
  Result := Count > 0;
  if Result then
  begin
    Min := L[0];
    Max := L[0];
    for I := 1 to Count - 1 do
    begin
      if L[I].X < Min.X then Min.X := L[I].X else
      if L[I].X > Max.X then Max.X := L[I].X;

      if L[I].Y < Min.Y then Min.Y := L[I].Y else
      if L[I].Y > Max.Y then Max.Y := L[I].Y;
    end;
  end;
end;

procedure TVector2List.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector2>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

procedure TVector2List.AddRange(const Source: TVector2List);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TVector2List.AddSubRange(const Source: TVector2List; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector2) * AddCount);
end;

procedure TVector2List.AddListRange(const Source: TVector2List; const Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

function TVector2List.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector2List;
  if Result then
  begin
    Result := Count = TVector2List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector2.Equals(TVector2List(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TVector3List ----------------------------------------------------- }

procedure TVector3List.AssignNegated(const Source: TVector3List);
begin
  Assign(Source);
  Negate;
end;

procedure TVector3List.Negate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I] := -L[I];
end;

procedure TVector3List.Normalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I] := L[I].Normalize;
end;

procedure TVector3List.MultiplyComponents(const V: TVector3);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I] := L[I] * V;
end;

procedure TVector3List.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector3>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

procedure TVector3List.AssignLerpNormalize(const Fraction: Single;
  const V1, V2: TVector3List; const Index1, Index2, ACount: TListSize);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
  begin
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]).Normalize;
  end;
end;

procedure TVector3List.AssignLerpRgbInHsv(const Fraction: Single;
  const V1, V2: TVector3List; const Index1, Index2, ACount: TListSize);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := LerpRgbInHsv(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

function TVector3List.ToVector4(const W: Single): TVector4List;
var
  I: Integer;
begin
  Result := TVector4List.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.L[I] := Vector4(L[I], W);
end;

function TVector3List.MergeCloseVertexes(MergeDistance: Single): Cardinal;
var
  V1, V2: PtrT;
  I, J: Integer;
begin
  MergeDistance := Sqr(MergeDistance);
  Result := 0;

  V1 := L;
  for I := 0 to Count - 1 do
  begin
    { Find vertexes closer to L[I], and merge them.

      Note that this is not optimal: we could avoid processing
      here L[I] that were detected previously (and possibly merged)
      as being equal to some previous items. But in practice this seems
      not needed, as there are not many merged vertices in typical situation,
      so time saving would be minimal (and small temporary memory cost
      introduced). }

    V2 := Ptr(I + 1);
    for J := I + 1 to Count - 1 do
    begin
      if PointsDistanceSqr(V1^, V2^) < MergeDistance then
        { We do the TVector3.PerfectlyEquals comparison only to get nice Result.
          But this *is* an important value for the user, so it's worth it. }
        if not TVector3.PerfectlyEquals(V1^, V2^) then
        begin
          V2^ := V1^;
          Inc(Result);
        end;
      Inc(V2);
    end;

    Inc(V1);
  end;
end;

procedure TVector3List.AddRange(const Source: TVector3List);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TVector3List.AddSubRange(const Source: TVector3List; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector3) * AddCount);
end;

procedure TVector3List.AddListRange(const Source: TVector3List; const Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

function TVector3List.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector3List;
  if Result then
  begin
    Result := Count = TVector3List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector3.Equals(TVector3List(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

procedure TVector3List.AddRangeTransformed(const Source: TVector3List; const Transform: TMatrix4);
var
  I, OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  for I := 0 to Source.Count - 1 do
    L[OldCount + I] := Transform.MultPoint(Source.L[I]);
end;

{ TVector4List ----------------------------------------------------- }

procedure TVector4List.AddRange(const Source: TVector4List);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TVector4List.AddSubRange(const Source: TVector4List; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4) * AddCount);
end;

procedure TVector4List.AddListRange(const Source: TVector4List; const Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

procedure TVector4List.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TVector4>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

function TVector4List.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector4List;
  if Result then
  begin
    Result := Count = TVector4List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector4.Equals(TVector4List(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TVector4IntegerList ----------------------------------------------------- }

procedure TVector4IntegerList.AddRange(const Source: TVector4IntegerList);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TVector4IntegerList.AddSubRange(const Source: TVector4IntegerList; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4Integer) * AddCount);
end;

procedure TVector4IntegerList.AddListRange(const Source: TVector4IntegerList; const Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

function TVector4IntegerList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector4IntegerList;
  if Result then
  begin
    Result := Count = TVector4IntegerList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector4Integer.Equals(TVector4IntegerList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TMatrix3List --------------------------------------------------------------- }

procedure TMatrix3List.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TMatrix3>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

function TMatrix3List.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix3List;
  if Result then
  begin
    Result := Count = TMatrix3List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix3.Equals(TMatrix3List(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

procedure TMatrix3List.AddRange(const Source: TMatrix3List);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TMatrix3List.AddSubRange(const Source: TMatrix3List; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TMatrix3) * AddCount);
end;

{ TMatrix4List --------------------------------------------------------------- }

procedure TMatrix4List.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<TMatrix4>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

function TMatrix4List.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix4List;
  if Result then
  begin
    Result := Count = TMatrix4List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix4.Equals(TMatrix4List(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

procedure TMatrix4List.AddRange(const Source: TMatrix4List);
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TMatrix4List.AddSubRange(const Source: TMatrix4List; const Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if AddCount <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TMatrix4) * AddCount);
end;

{$endif read_implementation}
