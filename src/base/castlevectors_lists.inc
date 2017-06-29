{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Interface and implementation of CastleVectors for lists. }

{$ifdef read_interface}

type
  TVector4SingleList = class;

  TVector3SingleList = class(specialize TGenericStructList<TVector3Single>)
  public
    procedure AssignNegated(Source: TVector3SingleList);
    { Negate all items. }
    procedure Negate;
    { Normalize all items. Zero vectors are left as zero. }
    procedure Normalize;
    { Multiply each item, component-wise, with V. }
    procedure MultiplyComponents(const V: TVector3Single);

    { Assign linear interpolation between two other vector arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      and normalize resulting vectors.
      @seealso AssignLerp }
    procedure AssignLerpNormalize(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      treating vectors as RGB colors and interpolating in HSV space.
      @seealso AssignLerp }
    procedure AssignLerpRgbInHsv(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    procedure AddList(Source: TVector3SingleList);
    procedure AddListRange(Source: TVector3SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector3Single);
    procedure AssignArray(const A: array of TVector3Single);

    { Convert to TVector4SingleList, with 4th vector component in
      new array set to constant W. }
    function ToVector4Single(const W: Single): TVector4SingleList;

    { When two vertexes on the list are closer than MergeDistance,
      set them truly (exactly) equal.
      Returns how many vertex positions were changed. }
    function MergeCloseVertexes(MergeDistance: Single): Cardinal;
  end;

  TVector2SingleList = class(specialize TGenericStructList<TVector2Single>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2Single): boolean;

    { Assign linear interpolation between two other vector arrays.
      @seealso TVector3SingleList.AssignLerp }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector2SingleList; Index1, Index2, ACount: Integer);

    procedure AddList(Source: TVector2SingleList);
    procedure AddListRange(Source: TVector2SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector2Single);
    procedure AssignArray(const A: array of TVector2Single);
  end;

  TVector4SingleList = class(specialize TGenericStructList<TVector4Single>)
  public
    procedure AddList(Source: TVector4SingleList);
    procedure AddListRange(Source: TVector4SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector4Single);
    procedure AssignArray(const A: array of TVector4Single);
  end;

  TVector3CardinalList = specialize TGenericStructList<TVector3Cardinal>;

  TVector2DoubleList = class(specialize TGenericStructList<TVector2Double>)
  public
    function ToVector2Single: TVector2SingleList;
    procedure AddList(Source: TVector2DoubleList);
    procedure AddArray(const A: array of TVector2Double);
  end;

  TVector3DoubleList = class(specialize TGenericStructList<TVector3Double>)
  public
    function ToVector3Single: TVector3SingleList;
    procedure AddList(Source: TVector3DoubleList);
    procedure AddArray(const A: array of TVector3Double);
  end;

  TVector4DoubleList = class(specialize TGenericStructList<TVector4Double>)
  public
    function ToVector4Single: TVector4SingleList;
    procedure AddList(Source: TVector4DoubleList);
    procedure AddArray(const A: array of TVector4Double);
  end;

  TMatrix3SingleList = class(specialize TGenericStructList<TMatrix3Single>)
  public
    procedure AddList(Source: TMatrix3SingleList);
    procedure AddArray(const A: array of TMatrix3Single);
  end;

  TMatrix3DoubleList = class(specialize TGenericStructList<TMatrix3Double>)
  public
    function ToMatrix3Single: TMatrix3SingleList;
    procedure AddList(Source: TMatrix3DoubleList);
    procedure AddArray(const A: array of TMatrix3Double);
  end;

  TMatrix4SingleList = class(specialize TGenericStructList<TMatrix4Single>)
  public
    procedure AddList(Source: TMatrix4SingleList);
    procedure AddArray(const A: array of TMatrix4Single);
  end;

  TMatrix4DoubleList = class(specialize TGenericStructList<TMatrix4Double>)
  public
    function ToMatrix4Single: TMatrix4SingleList;
    procedure AddList(Source: TMatrix4DoubleList);
    procedure AddArray(const A: array of TMatrix4Double);
  end;

  TVector2IntegerList = class(specialize TGenericStructList<TVector2Integer>)
    procedure Reverse;
    procedure AddList(Source: TVector2IntegerList);
  end;

  TVector2SmallIntList = specialize TGenericStructList<TVector2SmallInt>;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector3SingleList ----------------------------------------------------- }

procedure TVector3SingleList.AssignNegated(Source: TVector3SingleList);
begin
  Assign(Source);
  Negate;
end;

procedure TVector3SingleList.Negate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VectorNegateVar(L[I]);
end;

procedure TVector3SingleList.Normalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    NormalizeVar(L[I]);
end;

procedure TVector3SingleList.MultiplyComponents(const V: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VectorMultiplyComponentsVar(L[I], V);
end;

procedure TVector3SingleList.AssignLerp(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

procedure TVector3SingleList.AssignLerpNormalize(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
  begin
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
    NormalizeVar(L[I]);
  end;
end;

procedure TVector3SingleList.AssignLerpRgbInHsv(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := LerpRgbInHsv(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

function TVector3SingleList.ToVector4Single(const W: Single): TVector4SingleList;
var
  I: Integer;
begin
  Result := TVector4SingleList.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.L[I] := Vector4Single(L[I], W);
end;

function TVector3SingleList.MergeCloseVertexes(MergeDistance: Single): Cardinal;
var
  V1, V2: PVector3Single;
  I, J: Integer;
begin
  MergeDistance := Sqr(MergeDistance);
  Result := 0;

  V1 := PVector3Single(List);
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
        { We do the VectorsPerfectlyEqual comparison only to get nice Result.
          But this *is* an important value for the user, so it's worth it. }
        if not VectorsPerfectlyEqual(V1^, V2^) then
        begin
          V2^ := V1^;
          Inc(Result);
        end;
      Inc(V2);
    end;

    Inc(V1);
  end;
end;

procedure TVector3SingleList.AddList(Source: TVector3SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector3Single) * Source.Count);
end;

procedure TVector3SingleList.AddListRange(Source: TVector3SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector3Single) * AddCount);
end;

procedure TVector3SingleList.AddArray(const A: array of TVector3Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector3Single) * (High(A) + 1));
end;

procedure TVector3SingleList.AssignArray(const A: array of TVector3Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector2SingleList ----------------------------------------------------- }

function TVector2SingleList.MinMax(out Min, Max: TVector2Single): boolean;
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
      if L[I][0] < Min[0] then Min[0] := L[I][0] else
      if L[I][0] > Max[0] then Max[0] := L[I][0];

      if L[I][1] < Min[1] then Min[1] := L[I][1] else
      if L[I][1] > Max[1] then Max[1] := L[I][1];
    end;
  end;
end;

procedure TVector2SingleList.AssignLerp(const Fraction: Single;
  V1, V2: TVector2SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

procedure TVector2SingleList.AddList(Source: TVector2SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Single) * Source.Count);
end;

procedure TVector2SingleList.AddListRange(Source: TVector2SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector2Single) * AddCount);
end;

procedure TVector2SingleList.AddArray(const A: array of TVector2Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector2Single) * (High(A) + 1));
end;

procedure TVector2SingleList.AssignArray(const A: array of TVector2Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector4SingleList ----------------------------------------------------- }

procedure TVector4SingleList.AddList(Source: TVector4SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector4Single) * Source.Count);
end;

procedure TVector4SingleList.AddListRange(Source: TVector4SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4Single) * AddCount);
end;

procedure TVector4SingleList.AddArray(const A: array of TVector4Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector4Single) * (High(A) + 1));
end;

procedure TVector4SingleList.AssignArray(const A: array of TVector4Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector2DoubleList ----------------------------------------------------- }

function TVector2DoubleList.ToVector2Single: TVector2SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector2SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 2 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector2DoubleList.AddList(Source: TVector2DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Double) * Source.Count);
end;

procedure TVector2DoubleList.AddArray(const A: array of TVector2Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector2Double) * (High(A) + 1));
end;

{ TVector3DoubleList ----------------------------------------------------- }

function TVector3DoubleList.ToVector3Single: TVector3SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector3SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector3DoubleList.AddList(Source: TVector3DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector3Double) * Source.Count);
end;

procedure TVector3DoubleList.AddArray(const A: array of TVector3Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector3Double) * (High(A) + 1));
end;

{ TVector4DoubleList ----------------------------------------------------- }

function TVector4DoubleList.ToVector4Single: TVector4SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector4SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector4DoubleList.AddList(Source: TVector4DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector4Double) * Source.Count);
end;

procedure TVector4DoubleList.AddArray(const A: array of TVector4Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector4Double) * (High(A) + 1));
end;

{ TVector2IntegerList ---------------------------------------------------- }

procedure TVector2IntegerList.Reverse;
var
  I: Integer;
begin
  { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
    which means that loop would try invalid Exchange(0, -1). }
  if Count = 0 then Exit;
  for I := 0 to (Count - 1) div 2 do
    Exchange(I, Count - 1 - I);
end;

procedure TVector2IntegerList.AddList(Source: TVector2IntegerList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Integer) * Source.Count);
end;

{ TMatrix3SingleList ----------------------------------------------------- }

procedure TMatrix3SingleList.AddList(Source: TMatrix3SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix3Single) * Source.Count);
end;

procedure TMatrix3SingleList.AddArray(const A: array of TMatrix3Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix3Single) * (High(A) + 1));
end;

{ TMatrix4SingleList ----------------------------------------------------- }

procedure TMatrix4SingleList.AddList(Source: TMatrix4SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix4Single) * Source.Count);
end;

procedure TMatrix4SingleList.AddArray(const A: array of TMatrix4Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix4Single) * (High(A) + 1));
end;

{ TMatrix3DoubleList ----------------------------------------------------- }

function TMatrix3DoubleList.ToMatrix3Single: TMatrix3SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix3SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 3 * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix3DoubleList.AddList(Source: TMatrix3DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix3Double) * Source.Count);
end;

procedure TMatrix3DoubleList.AddArray(const A: array of TMatrix3Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix3Double) * (High(A) + 1));
end;

{ TMatrix4DoubleList ----------------------------------------------------- }

function TMatrix4DoubleList.ToMatrix4Single: TMatrix4SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix4SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 4 * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix4DoubleList.AddList(Source: TMatrix4DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix4Double) * Source.Count);
end;

procedure TMatrix4DoubleList.AddArray(const A: array of TMatrix4Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix4Double) * (High(A) + 1));
end;

{$endif read_implementation}
