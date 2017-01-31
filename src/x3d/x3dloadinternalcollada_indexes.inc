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
  { Indexes of a single vertex. Used only together with TColladaIndexes,
    that knows which ones are relevant (Coord is always relevant,
    but TexCoord is relevant only if TexCoordIndexOffset <> -1,
    and Normal is relevant only if NormalIndexOffset <> -1). }
  TIndex = record Coord, TexCoord, Normal: Integer end;

  { Handling Collada <p> (indexes inside primitives) to fill X3D geometry indexes.
    Accepted geometry types are TIndexedFaceSetNode and TIndexedLineSetNode,
    as only such may be created for Collada primitives. }
  TColladaIndexes = class
  private
    Ints: TIntegersParser;
    FGeometry: TAbstractGeometryNode;
    FInputsCount, FCoordIndexOffset, FTexCoordIndexOffset, FNormalIndexOffset: Integer;
  public
    constructor Create(const Geometry: TAbstractGeometryNode;
      const InputsCount, CoordIndexOffset, TexCoordIndexOffset, NormalIndexOffset: Integer);
    destructor Destroy; override;
    procedure BeginElement(const PElement: TDOMElement);
    function ReadVertex(out Index: TIndex): boolean;
    function ReadAddVertex(out Index: TIndex): boolean;
    function ReadAddVertex: boolean;
    procedure AddVertex(const Index: TIndex);
    procedure PolygonEnd;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TColladaIndexes.Create(
  const Geometry: TAbstractGeometryNode;
  const InputsCount, CoordIndexOffset, TexCoordIndexOffset, NormalIndexOffset: Integer);
begin
  inherited Create;
  FGeometry := Geometry;
  FInputsCount := InputsCount;
  FCoordIndexOffset := CoordIndexOffset;
  FTexCoordIndexOffset := TexCoordIndexOffset;
  FNormalIndexOffset := NormalIndexOffset;
end;

procedure TColladaIndexes.BeginElement(const PElement: TDOMElement);
begin
  FreeAndNil(Ints);
  Ints := TIntegersParser.Create(PElement);
end;

destructor TColladaIndexes.Destroy;
begin
  FreeAndNil(Ints);
  inherited;
end;

const
  EmptyIndex: TIndex = (Coord: -1; TexCoord: -1; Normal: -1);

function TColladaIndexes.ReadVertex(out Index: TIndex): boolean;
var
  I: Integer;
begin
  Index := EmptyIndex;

  for I := 0 to FInputsCount - 1 do
  begin
    if not Ints.GetNext then
    begin
      if I <> 0 then
        WritelnWarning('Collada', 'Indexes in <p> suddenly end, in the middle of a vertex');
      Exit(false);
    end;

    if I = FCoordIndexOffset then
      Index.Coord := Ints.Current else
    if I = FTexCoordIndexOffset then
      Index.TexCoord := Ints.Current else
    if I = FNormalIndexOffset then
      Index.Normal := Ints.Current;
  end;

  Result := true;
end;

function TColladaIndexes.ReadAddVertex(out Index: TIndex): boolean;
begin
  Result := ReadVertex(Index);
  if Result then
    AddVertex(Index);
end;

function TColladaIndexes.ReadAddVertex: boolean;
var
  IgnoreIndex: TIndex;
begin
  Result := ReadAddVertex(IgnoreIndex);
end;

procedure TColladaIndexes.AddVertex(const Index: TIndex);
begin
  if FGeometry is TIndexedFaceSetNode then
  begin
    TIndexedFaceSetNode(FGeometry).FdCoordIndex.Items.Add(Index.Coord);
    if FTexCoordIndexOffset <> -1 then
      TIndexedFaceSetNode(FGeometry).FdTexCoordIndex.Items.Add(Index.TexCoord);
    if FNormalIndexOffset <> -1 then
      TIndexedFaceSetNode(FGeometry).FdNormalIndex.Items.Add(Index.Normal);
  end else
  begin
    Assert(FGeometry is TIndexedLineSetNode);
    TIndexedLineSetNode(FGeometry).FdCoordIndex.Items.Add(Index.Coord);
  end;
end;

procedure TColladaIndexes.PolygonEnd;
begin
  AddVertex(EmptyIndex);
end;

{$endif read_implementation}
