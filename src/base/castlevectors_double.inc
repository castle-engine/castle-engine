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

{ Interface and implementation of CastleVectors
  for vectors and matrices with Double precision. }

{$ifdef read_interface}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
function TryMatrixInverse(const M: TMatrix2Double; out MInverse: TMatrix2Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Double; out MInverse: TMatrix3Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Double; out MInverse: TMatrix4Double): boolean; overload;
{$endif CASTLE_HAS_DOUBLE_PRECISION}

{ Convert string to vector. Each component is simply parsed by StrToFloat,
  and components must be separated by whitespace (see @link(WhiteSpaces) constant).
  @raises(EConvertError In case of problems during conversion (invalid float
    or unexpected string end or expected but missed string end).) }
function Vector3DoubleFromStr(const S: string): TVector3Double;

{$endif read_interface}

{$ifdef read_implementation}

function TryMatrixInverse(const M: TMatrix2Double; out MInverse: TMatrix2Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix3Double; out MInverse: TMatrix3Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix4Double; out MInverse: TMatrix4Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function Vector3DoubleFromStr(const s: string): TVector3Double;
{$I castlevectors_vector3fromstr.inc}

{$endif read_implementation}
