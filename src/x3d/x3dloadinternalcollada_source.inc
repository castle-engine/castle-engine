{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Collada <source>. This is a named container keeping a series of floats
    in Collada. It may contain only data for some specific purpose (like positions
    or normals or tex coords) or be interleaved with other data. }
  TColladaSource = class
    Name: string;
    Floats: TFloatList;
    Params: TCastleStringList;
    { Collada <accessor> description of vectors inside Floats.
      Note that Count here refers to count of whole vectors (so it's usually
      < than Floats.Count). }
    Count, Stride, Offset: Integer;

    constructor Create;
    destructor Destroy; override;

    { Check components and counts before AssignToVectorXYZ.
      It @false, a warning is done, and some parameters are adjusted.
      You can then abort using this vector
      (if it can be reasonably ignored, e.g. for normals),
      or try AssignToVectorXYZ anyway
      (if it's necessary, e.g. when this is for vertex positions). }
    function CheckXYZ(out XIndex, YIndex, ZIndex: Integer): boolean;
    function CheckST(out SIndex, TIndex: Integer): boolean;

    { Extract from source an array of TVector3Single.
      Params will be checked to contain at least three vector components
      'X', 'Y', 'Z'. (These should be specified in <param name="..." type="float"/>).

      Order of X, Y, Z params (and thus order of components in our Floats)
      may be different, we will reorder them to XYZ anyway.
      This way Collada data may have XYZ or ST in a different order,
      and this method will reorder this suitable for X3D.

      Returns if components were fully correct (no warnings). }
    function AssignToVectorXYZ(const Value: TVector3SingleList): boolean;

    { Extract from source an array of TVector2Single, for texture coordinates.
      Like AssignToVectorXYZ, but for 2D vectors, with component names 'S' and 'T'
      (or 'U' and 'V'). }
    function AssignToVectorST(const Value: TVector2SingleList): boolean;
  end;

  TColladaSourceList = class(specialize TFPGObjectList<TColladaSource>)
  public
    { Find a TColladaSource with given Name, @nil if not found. }
    function Find(const Name: string): TColladaSource;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TColladaSource.Create;
begin
  inherited;
  Params := TCastleStringList.Create;
  Floats := TFloatList.Create;
end;

destructor TColladaSource.Destroy;
begin
  FreeAndNil(Params);
  FreeAndNil(Floats);
  inherited;
end;

function TColladaSource.CheckXYZ(out XIndex, YIndex, ZIndex: Integer): boolean;
var
  MinCount: Integer;
begin
  XIndex := Params.IndexOf('X');
  if XIndex = -1 then
  begin
    WritelnWarning('Collada', 'Missing "X" parameter (for 3D vector) in this <source>');
    Exit(false);
  end;

  YIndex := Params.IndexOf('Y');
  if YIndex = -1 then
  begin
    WritelnWarning('Collada', 'Missing "Y" parameter (for 3D vector) in this <source>');
    Exit(false);
  end;

  ZIndex := Params.IndexOf('Z');
  if ZIndex = -1 then
  begin
    WritelnWarning('Collada', 'Missing "Z" parameter (for 3D vector) in this <source>');
    Exit(false);
  end;

  MinCount := Offset + Stride * (Count - 1) + CastleUtils.Max(XIndex, YIndex, ZIndex) + 1;
  Result := Floats.Count >= MinCount;
  if not Result then
  begin
    WritelnWarning('Collada', Format('<accessor> count requires at least %d float values in <float_array>, but only %d are avilable',
      [MinCount, Floats.Count]));
    { force Count smaller, also force other params to common values }
    Stride := Max(Stride, 1);
    XIndex := 0;
    YIndex := 1;
    ZIndex := 2;
    Count := (Floats.Count - Offset) div Stride;
  end;
end;

function TColladaSource.AssignToVectorXYZ(const Value: TVector3SingleList): boolean;
var
  XIndex, YIndex, ZIndex, I: Integer;
begin
  Result := CheckXYZ(XIndex, YIndex, ZIndex);
  Value.Count := Count;
  for I := 0 to Count - 1 do
  begin
    Value.L[I][0] := Floats.L[Offset + Stride * I + XIndex];
    Value.L[I][1] := Floats.L[Offset + Stride * I + YIndex];
    Value.L[I][2] := Floats.L[Offset + Stride * I + ZIndex];
  end;
end;

function TColladaSource.CheckST(out SIndex, TIndex: Integer): boolean;
var
  MinCount: Integer;
begin
  SIndex := Params.IndexOf('S');
  if SIndex = -1 then
  begin
    SIndex := Params.IndexOf('U');
    if SIndex = -1 then
    begin
      WritelnWarning('Collada', 'Missing "S" or "U" parameter (1st component of 2D tex coord) in this <source>');
      Exit(false);
    end;
  end;

  TIndex := Params.IndexOf('T');
  if TIndex = -1 then
  begin
    TIndex := Params.IndexOf('V');
    if TIndex = -1 then
    begin
      WritelnWarning('Collada', 'Missing "T" or "V" parameter (2nd component of 2D tex coord) in this <source>');
      Exit(false);
    end;
  end;

  MinCount := Offset + Stride * (Count - 1) + Max(SIndex, TIndex) + 1;
  Result := Floats.Count >= MinCount;
  if not Result then
  begin
    WritelnWarning('Collada', Format('<accessor> count requires at least %d float values in <float_array>, but only %d are avilable',
      [MinCount, Floats.Count]));
    { force Count smaller, also force other params to common values }
    Stride := Max(Stride, 1);
    SIndex := 0;
    TIndex := 1;
    Count := (Floats.Count - Offset) div Stride;
  end;
end;

function TColladaSource.AssignToVectorST(const Value: TVector2SingleList): boolean;
var
  SIndex, TIndex, I: Integer;
begin
  Result := CheckST(SIndex, TIndex);
  Value.Count := Count;
  for I := 0 to Count - 1 do
  begin
    Value.L[I][0] := Floats.L[Offset + Stride * I + SIndex];
    Value.L[I][1] := Floats.L[Offset + Stride * I + TIndex];
  end;
end;

function TColladaSourceList.Find(const Name: string): TColladaSource;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  Result := nil;
end;

{$endif read_implementation}
