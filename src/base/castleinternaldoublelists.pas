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

{ Hack to workaround FPC 3.0.0 bug: we have to define Double-precision lists
  in a separate unit. }
unit CastleInternalDoubleLists;

{$I castleconf.inc}

interface

{$ifdef BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

uses CastleUtils, CastleVectors;

type
  TVector2DoubleList = class(specialize TStructList<TVector2Double>)
  public
    function ToVector2: TVector2List;
  end;

  TVector3DoubleList = class(specialize TStructList<TVector3Double>)
  public
    function ToVector3: TVector3List;
  end;

  TVector4DoubleList = class(specialize TStructList<TVector4Double>)
  public
    function ToVector4: TVector4List;
  end;

  TMatrix3DoubleList = class(specialize TStructList<TMatrix3Double>)
  public
    function ToMatrix3: TMatrix3List;
  end;

  TMatrix4DoubleList = class(specialize TStructList<TMatrix4Double>)
  public
    function ToMatrix4: TMatrix4List;
  end;

{$endif BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

implementation

{$ifdef BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

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

{$endif BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

end.
