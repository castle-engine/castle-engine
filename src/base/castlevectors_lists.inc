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

{ CastleVectors lists of vectors and matrices. }

{$ifdef read_interface}

type
  TVector4List = class;

  { List of @link(TVector2). }
  TVector2List = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVector2>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2): boolean;

    { Assign linear interpolation between two other vector arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector2List; Index1, Index2, ACount: Integer); overload;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TVector2List); overload;

    procedure AddSubRange(const Source: TVector2List; Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector2List; Index, AddCount: Integer); deprecated 'use AddSubRange';

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TVector3). }
  TVector3List = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVector3>)
  public
    procedure AssignNegated(const Source: TVector3List); deprecated 'use Assign and Negate separately';

    { Negate all items. }
    procedure Negate;

    { Normalize all items. Zero vectors are left as zero. }
    procedure Normalize;

    { Multiply each item, component-wise, with V. }
    procedure MultiplyComponents(const V: TVector3);

    { Assign linear interpolation between two other vector arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector3List; Index1, Index2, ACount: Integer); overload;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TVector3List); overload;

    { Assign linear interpolation between two other vector arrays,
      and normalize resulting vectors.
      @seealso AssignLerp }
    procedure AssignLerpNormalize(const Fraction: Single;
      V1, V2: TVector3List; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      treating vectors as RGB colors and interpolating in HSV space.
      @seealso AssignLerp }
    procedure AssignLerpRgbInHsv(const Fraction: Single;
      V1, V2: TVector3List; Index1, Index2, ACount: Integer);

    procedure AddSubRange(const Source: TVector3List; Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector3List; Index, AddCount: Integer); deprecated 'use AddSubRange';

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

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TVector4). }
  TVector4List = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVector4>)
  public
    procedure AddSubRange(const Source: TVector4List; Index, AddCount: Integer);
    procedure AddListRange(const Source: TVector4List; Index, AddCount: Integer); deprecated 'use AddSubRange';

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TVector4List);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TMatrix3). }
  TMatrix3List = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TMatrix3>)
  public
    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TMatrix3List);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TMatrix4). }
  TMatrix4List = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TMatrix4>)
  public
    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TMatrix4List);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  { List of @link(TVector2SmallInt). }
  TVector2SmallIntList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVector2SmallInt>;

  { List of @link(TVector2Cardinal). }
  TVector3CardinalList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVector3Cardinal>;

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
    Min := List^[0];
    Max := List^[0];
    for I := 1 to Count - 1 do
    begin
      if List^[I].Data[0] < Min.Data[0] then Min.Data[0] := List^[I].Data[0] else
      if List^[I].Data[0] > Max.Data[0] then Max.Data[0] := List^[I].Data[0];

      if List^[I].Data[1] < Min.Data[1] then Min.Data[1] := List^[I].Data[1] else
      if List^[I].Data[1] > Max.Data[1] then Max.Data[1] := List^[I].Data[1];
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
    List^[I] := Lerp(Fraction, V1.List^[Index1 + I], V2.List^[Index2 + I]);
end;

procedure TVector2List.AssignLerp(const Fraction: Single; const V1, V2: TVector2List);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
end;

procedure TVector2List.AddSubRange(const Source: TVector2List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(TVector2) * AddCount);
end;

procedure TVector2List.AddListRange(const Source: TVector2List; Index, AddCount: Integer);
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
        if not TVector2.Equals(TVector2List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector2List.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector2List;
  if Result then
  begin
    Result := Count = TVector2List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector2.PerfectlyEquals(TVector2List(SecondValue).List^[I], List^[I]) then
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
    List^[I] := -List^[I];
end;

procedure TVector3List.Normalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List^[I].NormalizeMe;
end;

procedure TVector3List.MultiplyComponents(const V: TVector3);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List^[I] := List^[I] * V;
end;

procedure TVector3List.AssignLerp(const Fraction: Single;
  V1, V2: TVector3List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[Index1 + I], V2.List^[Index2 + I]);
end;

procedure TVector3List.AssignLerp(const Fraction: Single; const V1, V2: TVector3List);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
end;

procedure TVector3List.AssignLerpNormalize(const Fraction: Single;
  V1, V2: TVector3List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
  begin
    List^[I] := Lerp(Fraction, V1.List^[Index1 + I], V2.List^[Index2 + I]);
    List^[I].NormalizeMe;
  end;
end;

procedure TVector3List.AssignLerpRgbInHsv(const Fraction: Single;
  V1, V2: TVector3List; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    List^[I] := LerpRgbInHsv(Fraction, V1.List^[Index1 + I], V2.List^[Index2 + I]);
end;

function TVector3List.ToVector4(const W: Single): TVector4List;
var
  I: Integer;
begin
  Result := TVector4List.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.List^[I] := Vector4(List^[I], W);
end;

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
    { Find vertexes closer to List^[I], and merge them.

      Note that this is not optimal: we could avoid processing
      here List^[I] that were detected previously (and possibly merged)
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

procedure TVector3List.AddSubRange(const Source: TVector3List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(TVector3) * AddCount);
end;

procedure TVector3List.AddListRange(const Source: TVector3List; Index, AddCount: Integer);
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
        if not TVector3.Equals(TVector3List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector3List.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector3List;
  if Result then
  begin
    Result := Count = TVector3List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector3.PerfectlyEquals(TVector3List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TVector4List ----------------------------------------------------- }

procedure TVector4List.AddSubRange(const Source: TVector4List; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(TVector4) * AddCount);
end;

procedure TVector4List.AddListRange(const Source: TVector4List; Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

procedure TVector4List.AssignLerp(const Fraction: Single; const V1, V2: TVector4List);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
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
        if not TVector4.Equals(TVector4List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TVector4List.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TVector4List;
  if Result then
  begin
    Result := Count = TVector4List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TVector4.PerfectlyEquals(TVector4List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TMatrix3List --------------------------------------------------------------- }

procedure TMatrix3List.AssignLerp(const Fraction: Single; const V1, V2: TMatrix3List);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
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
        if not TMatrix3.Equals(TMatrix3List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TMatrix3List.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix3List;
  if Result then
  begin
    Result := Count = TMatrix3List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix3.PerfectlyEquals(TMatrix3List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TMatrix4List --------------------------------------------------------------- }

procedure TMatrix4List.AssignLerp(const Fraction: Single; const V1, V2: TMatrix4List);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
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
        if not TMatrix4.Equals(TMatrix4List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TMatrix4List.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TMatrix4List;
  if Result then
  begin
    Result := Count = TMatrix4List(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not TMatrix4.PerfectlyEquals(TMatrix4List(SecondValue).List^[I], List^[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{$endif read_implementation}
