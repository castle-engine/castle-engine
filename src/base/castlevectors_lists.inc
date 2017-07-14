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

  TVector3SingleList = class(specialize TStructList<TVector3Single>)
  public
    procedure AssignNegated(const Source: TVector3SingleList);

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

    procedure AddList(const Source: TVector3SingleList); deprecated 'use AddRange';
    procedure AddListRange(const Source: TVector3SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector3Single); deprecated 'use AddRange';
    procedure AssignArray(const A: array of TVector3Single);

    { Convert to TVector4SingleList, with 4th vector component in
      new array set to constant W. }
    function ToVector4Single(const W: Single): TVector4SingleList;

    { When two vertexes on the list are closer than MergeDistance,
      set them truly (exactly) equal.
      Returns how many vertex positions were changed. }
    function MergeCloseVertexes(MergeDistance: Single): Cardinal;
  end;

  TVector2SingleList = class(specialize TStructList<TVector2Single>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2Single): boolean;

    { Assign linear interpolation between two other vector arrays.
      @seealso TVector3SingleList.AssignLerp }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector2SingleList; Index1, Index2, ACount: Integer);

    procedure AddList(const Source: TVector2SingleList); deprecated 'use AddRange';
    procedure AddListRange(const Source: TVector2SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector2Single); deprecated 'use AddRange';
    procedure AssignArray(const A: array of TVector2Single);
  end;

  TVector4SingleList = class(specialize TStructList<TVector4Single>)
  public
    procedure AddList(const Source: TVector4SingleList); deprecated 'use AddRange';
    procedure AddListRange(const Source: TVector4SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector4Single); deprecated 'use AddRange';
    procedure AssignArray(const A: array of TVector4Single);
  end;

  TVector3CardinalList = specialize TStructList<TVector3Cardinal>;

  TVector2DoubleList = class(specialize TStructList<TVector2Double>)
  public
    function ToVector2Single: TVector2SingleList;
    procedure AddList(const Source: TVector2DoubleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TVector2Double); deprecated 'use AddRange';
  end;

  TVector3DoubleList = class(specialize TStructList<TVector3Double>)
  public
    function ToVector3Single: TVector3SingleList;
    procedure AddList(const Source: TVector3DoubleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TVector3Double); deprecated 'use AddRange';
  end;

  TVector4DoubleList = class(specialize TStructList<TVector4Double>)
  public
    function ToVector4Single: TVector4SingleList;
    procedure AddList(const Source: TVector4DoubleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TVector4Double); deprecated 'use AddRange';
  end;

  TMatrix3SingleList = class(specialize TStructList<TMatrix3Single>)
  public
    procedure AddList(const Source: TMatrix3SingleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TMatrix3Single); deprecated 'use AddRange';
  end;

  TMatrix3DoubleList = class(specialize TStructList<TMatrix3Double>)
  public
    function ToMatrix3Single: TMatrix3SingleList;
    procedure AddList(const Source: TMatrix3DoubleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TMatrix3Double); deprecated 'use AddRange';
  end;

  TMatrix4SingleList = class(specialize TStructList<TMatrix4Single>)
  public
    procedure AddList(const Source: TMatrix4SingleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TMatrix4Single); deprecated 'use AddRange';
  end;

  TMatrix4DoubleList = class(specialize TStructList<TMatrix4Double>)
  public
    function ToMatrix4Single: TMatrix4SingleList;
    procedure AddList(const Source: TMatrix4DoubleList); deprecated 'use AddRange';
    procedure AddArray(const A: array of TMatrix4Double); deprecated 'use AddRange';
  end;

  TVector2IntegerList = class(specialize TStructList<TVector2Integer>)
  public
    procedure Reverse;
    procedure AddList(const Source: TVector2IntegerList); deprecated 'use AddRange';
  end;

  TVector2SmallIntList = specialize TStructList<TVector2SmallInt>;

{$endif read_interface}

{$ifdef read_implementation}

{ TVector3SingleList ----------------------------------------------------- }

procedure TVector3SingleList.AssignNegated(const Source: TVector3SingleList);
begin
  Assign(Source);
  Negate;
end;

procedure TVector3SingleList.Negate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I] := -L[I];
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

procedure TVector3SingleList.AddList(const Source: TVector3SingleList);
begin
  AddRange(Source);
end;

procedure TVector3SingleList.AddListRange(const Source: TVector3SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector3Single) * AddCount);
end;

procedure TVector3SingleList.AddArray(const A: array of TVector3Single);
begin
  AddRange(A);
end;

procedure TVector3SingleList.AssignArray(const A: array of TVector3Single);
begin
  Clear;
  AddRange(A);
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

procedure TVector2SingleList.AddList(const Source: TVector2SingleList);
begin
  AddRange(Source);
end;

procedure TVector2SingleList.AddListRange(const Source: TVector2SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector2Single) * AddCount);
end;

procedure TVector2SingleList.AddArray(const A: array of TVector2Single);
begin
  AddRange(A);
end;

procedure TVector2SingleList.AssignArray(const A: array of TVector2Single);
begin
  Clear;
  AddRange(A);
end;

{ TVector4SingleList ----------------------------------------------------- }

procedure TVector4SingleList.AddList(const Source: TVector4SingleList);
begin
  AddRange(Source);
end;

procedure TVector4SingleList.AddListRange(const Source: TVector4SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4Single) * AddCount);
end;

procedure TVector4SingleList.AddArray(const A: array of TVector4Single);
begin
  AddRange(A);
end;

procedure TVector4SingleList.AssignArray(const A: array of TVector4Single);
begin
  Clear;
  AddRange(A);
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
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 2 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector2DoubleList.AddList(const Source: TVector2DoubleList);
begin
  AddRange(Source);
end;

procedure TVector2DoubleList.AddArray(const A: array of TVector2Double);
begin
  AddRange(A);
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
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector3DoubleList.AddList(const Source: TVector3DoubleList);
begin
  AddRange(Source);
end;

procedure TVector3DoubleList.AddArray(const A: array of TVector3Double);
begin
  AddRange(A);
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
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector4DoubleList.AddList(const Source: TVector4DoubleList);
begin
  AddRange(Source);
end;

procedure TVector4DoubleList.AddArray(const A: array of TVector4Double);
begin
  AddRange(A);
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

procedure TVector2IntegerList.AddList(const Source: TVector2IntegerList);
begin
  AddRange(Source);
end;

{ TMatrix3SingleList ----------------------------------------------------- }

procedure TMatrix3SingleList.AddList(const Source: TMatrix3SingleList);
begin
  AddRange(Source);
end;

procedure TMatrix3SingleList.AddArray(const A: array of TMatrix3Single);
begin
  AddRange(A);
end;

{ TMatrix4SingleList ----------------------------------------------------- }

procedure TMatrix4SingleList.AddList(const Source: TMatrix4SingleList);
begin
  AddRange(Source);
end;

procedure TMatrix4SingleList.AddArray(const A: array of TMatrix4Single);
begin
  AddRange(A);
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
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 3 * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix3DoubleList.AddList(const Source: TMatrix3DoubleList);
begin
  AddRange(Source);
end;

procedure TMatrix3DoubleList.AddArray(const A: array of TMatrix3Double);
begin
  AddRange(A);
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
  Source := PDouble(L);
  Dest := PSingle(Result.L);
  for I := 0 to Count * 4 * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix4DoubleList.AddList(const Source: TMatrix4DoubleList);
begin
  AddRange(Source);
end;

procedure TMatrix4DoubleList.AddArray(const A: array of TMatrix4Double);
begin
  AddRange(A);
end;

{$endif read_implementation}
