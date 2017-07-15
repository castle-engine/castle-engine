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

{ Interface and implementation of CastleVectors for lists using record-based
  vector and matrix types (TVector3 and friends). }

{$ifdef read_interface}

type
  TVector4List = class;

  TVector2List = class(specialize TStructList<TVector2>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2): boolean;

    { Assign linear interpolation between two other vector arrays.
      @seealso TVector3List.AssignLerp }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector2List; Index1, Index2, ACount: Integer);

    procedure AddSubRange(const Source: TVector2List; Index, AddCount: Integer);
    procedure Assign(const A: array of TVector2);
  end;

  TVector3List = class(specialize TStructList<TVector3>)
  public
    // procedure AssignNegated(const Source: TVector3List);

    { Negate all items. }
    procedure Negate;

    { Normalize all items. Zero vectors are left as zero. }
    procedure Normalize;

    { Multiply each item, component-wise, with V. }
    // procedure MultiplyComponents(const V: TVector3);

    { Assign linear interpolation between two other vector arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector3List; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      and normalize resulting vectors.
      @seealso AssignLerp }
    // procedure AssignLerpNormalize(const Fraction: Single;
    //   V1, V2: TVector3List; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      treating vectors as RGB colors and interpolating in HSV space.
      @seealso AssignLerp }
    procedure AssignLerpRgbInHsv(const Fraction: Single;
      V1, V2: TVector3List; Index1, Index2, ACount: Integer);

    procedure AddSubRange(const Source: TVector3List; Index, AddCount: Integer);
    procedure Assign(const A: array of TVector3);

    { Convert to TVector4List, with 4th vector component in
      new array set to constant W. }
    // function ToVector4(const W: Single): TVector4List;

    { When two vertexes on the list are closer than MergeDistance,
      set them truly (exactly) equal.
      Returns how many vertex positions were changed. }
    function MergeCloseVertexes(MergeDistance: Single): Cardinal;
  end;

  TVector4List = class(specialize TStructList<TVector4>)
  public
    procedure AddSubRange(const Source: TVector4List; Index, AddCount: Integer);
    procedure Assign(const A: array of TVector4);
  end;

  TVector2dList = class(specialize TStructList<TVector2d>)
  public
    function ToVector2: TVector2List;
  end;

  TVector3dList = class(specialize TStructList<TVector3d>)
  public
    function ToVector3: TVector3List;
  end;

  TVector4dList = class(specialize TStructList<TVector4d>)
  public
    function ToVector4: TVector4List;
  end;

  TMatrix3List = class(specialize TStructList<TMatrix3>)
  public
  end;

  TMatrix3dList = class(specialize TStructList<TMatrix3d>)
  public
    function ToMatrix3: TMatrix3List;
  end;

  TMatrix4List = class(specialize TStructList<TMatrix4>)
  public
  end;

  TMatrix4dList = class(specialize TStructList<TMatrix4d>)
  public
    function ToMatrix4: TMatrix4List;
  end;

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
      if L[I].Data[0] < Min.Data[0] then Min.Data[0] := L[I].Data[0] else
      if L[I].Data[0] > Max.Data[0] then Max.Data[0] := L[I].Data[0];

      if L[I].Data[1] < Min.Data[1] then Min.Data[1] := L[I].Data[1] else
      if L[I].Data[1] > Max.Data[1] then Max.Data[1] := L[I].Data[1];
    end;
  end;
end;

procedure TVector2List.AssignLerp(const Fraction: Single;
  V1, V2: TVector2List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

procedure TVector2List.AddSubRange(const Source: TVector2List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector2) * AddCount);
end;

procedure TVector2List.Assign(const A: array of TVector2);
begin
  Clear;
  AddRange(A);
end;

{ TVector3List ----------------------------------------------------- }

// procedure TVector3List.AssignNegated(const Source: TVector3List);
// begin
//   Assign(Source);
//   Negate;
// end;

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

// procedure TVector3List.MultiplyComponents(const V: TVector3);
// var
//   I: Integer;
// begin
//   for I := 0 to Count - 1 do
//     L[I] := L[I].MultiplyComponents(V);
// end;

procedure TVector3List.AssignLerp(const Fraction: Single;
  V1, V2: TVector3List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

// procedure TVector3List.AssignLerpNormalize(const Fraction: Single;
//   V1, V2: TVector3List; Index1, Index2, ACount: Integer);
// var
//   I: Integer;
// begin
//   Count := ACount;
//   for I := 0 to Count - 1 do
//   begin
//     L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
//     L[I] := L[I].Normalize;
//   end;
// end;

procedure TVector3List.AssignLerpRgbInHsv(const Fraction: Single;
  V1, V2: TVector3List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := LerpRgbInHsv(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

// function TVector3List.ToVector4(const W: Single): TVector4List;
// var
//   I: Integer;
// begin
//   Result := TVector4List.Create;
//   Result.Count := Count;
//   for I := 0 to Count - 1 do
//     Result.L[I] := Vector4(L[I], W);
// end;

function TVector3List.MergeCloseVertexes(MergeDistance: Single): Cardinal;
var
  V1, V2: PT;
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

procedure TVector3List.AddSubRange(const Source: TVector3List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector3) * AddCount);
end;

procedure TVector3List.Assign(const A: array of TVector3);
begin
  Clear;
  AddRange(A);
end;

{ TVector4List ----------------------------------------------------- }

procedure TVector4List.AddSubRange(const Source: TVector4List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4) * AddCount);
end;

procedure TVector4List.Assign(const A: array of TVector4);
begin
  Clear;
  AddRange(A);
end;

{ TVector2dList ----------------------------------------------------- }

function TVector2dList.ToVector2: TVector2List;
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

{ TVector3dList ----------------------------------------------------- }

function TVector3dList.ToVector3: TVector3List;
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

{ TVector4dList ----------------------------------------------------- }

function TVector4dList.ToVector4: TVector4List;
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

{ TMatrix3List ----------------------------------------------------- }

{ TMatrix4List ----------------------------------------------------- }

{ TMatrix3dList ----------------------------------------------------- }

function TMatrix3dList.ToMatrix3: TMatrix3List;
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

{ TMatrix4dList ----------------------------------------------------- }

function TMatrix4dList.ToMatrix4: TMatrix4List;
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

{$endif read_implementation}
